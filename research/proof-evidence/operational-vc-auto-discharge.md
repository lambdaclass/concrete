# Operational VC Auto-Discharge

Status: **forcing probe RUN (2026-06-22) — verdict GO.** Was: research /
roadmap candidate. The "Forcing Probe" section below now records the measured
result, not a plan. Gate: `scripts/tests/check_operational_vc_auto_discharge.sh`;
fixtures: `scripts/tests/fixtures/operational_vc_autodischarge/{closes,boundary}.lean`.

Priority: P0

Concrete already has a useful proof ladder for arithmetic and bitvector facts:
linear obligations route to `omega`, bitvector obligations route to
`bv_decide`, and reports classify the resulting evidence. The missing tier is
the operational body of ordinary code. Today, obligations about what code
actually computes often require a hand-written Lean bridge theorem even when
the body is straight-line, bounded, and decidable.

This note records the research target: generate operational verification
conditions from Core / ProofCore for a narrow fragment, discharge decidable
parts with the existing kernel decision procedures, and reserve hand Lean for
the cases that genuinely need it.

## Current Gap

The current engine split is visible in `Concrete/ObligationCore.lean`:

- `linear` obligations use `omega`.
- `bitvector` obligations use `bv_decide`.
- `operational` and `refinement` obligations use Lean proof links.

That means the arithmetic skeleton of a loop can be automatic while the
operational preservation step still asks for a human theorem. In
`examples/loop_invariant/snapshot/contracts.txt`, the counter and variant facts
are kernel-decided, but the operational preservation is either
`proved_by_lean` or still planned as a Lean proof. The same pattern appears in
flagship bit-twiddling code: the source `#[ensures]` facts are linked to
hand-authored Lean theorems even when the underlying identity is a finite
bitvector formula.

The cost is not just aesthetic. Concrete's evidence story depends on ordinary
systems engineers being able to attach useful proof without becoming Lean
experts. If every substantive `#[ensures]` needs a bespoke bridge theorem, the
external-validation bar is much harder to clear.

## Goal

Build an automation tier between local syntactic discharge and hand Lean:

1. Symbolically execute a small Core / ProofCore fragment.
2. Generate an operational VC for the postcondition, loop step, or refinement
   obligation.
3. Split decidable leaves into the existing `omega` / `bv_decide` paths.
4. Report unsupported shapes honestly as `needs_lean` or `not_supported`.

The result must never silently upgrade evidence. A fact is
`proved_by_kernel_decision` only when the kernel-backed decision procedure closes
the generated VC. A fact is `proved_by_lean` only when a linked Lean theorem
checks. Everything else stays visible as missing, unsupported, runtime-checked,
or assumption-backed according to the existing evidence model.

## V1 Fragment

The first useful fragment should be deliberately small:

- straight-line integer and fixed-width bitvector expressions;
- assignments to local variables;
- simple struct field projection / construction where layout is not the proof
  subject;
- enum construction and match only for finite tag/value cases that lower
  cleanly into ProofCore;
- fixed arrays and bounded buffers where all indices have explicit bounds;
- bounded loops with explicit `#[invariant]` and `#[decreases]`, where the body
  is symbolically evaluable over local state;
- pure functions already available as ProofCore definitions or previously
  proved summaries.

Target examples:

- `result == ch_spec(x, y, z)` for a bitvector choice function;
- loop invariant preservation for `i := i + 1`, `acc := acc + value`, and
  bounded array-copy shapes;
- postconditions for small parsers/codecs where the state update is a finite
  sequence of cursor and buffer operations.

## Out Of Scope For V1

V1 should not attempt to solve the full verification problem:

- unbounded loops or recursion;
- heap-heavy aliasing, raw pointers, `Unsafe`, trusted code, or FFI;
- I/O, capabilities, nondeterminism, time, process state, or device effects;
- maps, sets, and collection theories unless a specific finite theory and gate
  are designed;
- floating-point reasoning beyond the explicit arithmetic profile;
- deep inductive functional specs;
- arbitrary theorem-prover search hidden behind a green status.

Unsupported cases must stay visible and must still be eligible for explicit
Lean proofs.

## Forcing Probe — RESULT (2026-06-22)

The probe was run, not just planned. Six verbatim copies of real, currently
hand-proved VC statements were attacked with ONE *fixed, mechanical* tactic
allowed only what an auto-discharger would have — unfold the evaluator (a fixed
reusable lemma set), unfold the extracted body def and the named spec it
refines, the reusable Int↔Nat↔BitVec round-trip collapse lemmas, a mechanical
conjunction split / guard `by_cases`, then route the leaf to `bv_decide` /
`omega` / `rfl` / `simp`. **No** per-obligation `rw [show … by omega]`, bespoke
helper lemma, or hand-picked rewrite was permitted.

| VC (real obligation)                  | Class                    | Mechanical tactic         | Result |
|---------------------------------------|--------------------------|---------------------------|:------:|
| `ch` refines spec                     | pure bitwise word fn     | eval-unfold → `bv_decide` | closes |
| `maj` refines spec                    | pure bitwise word fn     | eval-unfold → `bv_decide` | closes |
| `count_up` invariant preservation     | straight-line loop body  | eval-unfold → `omega`     | closes |
| `validate_version` postcond (+split)  | branching parser         | `by_cases` → `simp`/`omega` | closes |
| `rotr` refines spec                   | shift-amount arithmetic  | eval-unfold → `bv_decide` | **fails** |
| `validate_version` postcond (no split)| branching parser         | eval-unfold → `omega`     | **fails** |

**4 of 6 close with a fixed tactic — verdict GO.** The dominant cost is *not*
"can the SMT engines decide these" (they can); it is the **cast-normalization
fragment** between the evaluator's `Int` world and the spec's `Nat`/`BitVec`
world. The two failures are the boundary, and each is a bounded fragment
feature, not a research wall:

1. **`rotr` / sigmas / packing / indexing:** the goal stalls at
   `X >>> (↑n).toNat ||| X <<< (32 - ↑n).toNat` vs `X >>> n ||| X <<< (32 - n)`.
   The evaluator binds shift amounts as `Int`; the spec shifts by `Nat`; the
   leftover `(↑n).toNat` / `(32 - ↑n).toNat` casts block `bv_decide` (not a pure
   BitVec goal) and `rfl` (not def-eq). The hand proof bridges with one
   mechanical line: `rw [show ((n:Int)).toNat = n by omega, …]`. So V1 must
   **emit the `toNat`/`ofInt` round-trip + linear-bound side-goals and discharge
   them with `omega` before handing the BitVec leaf to `bv_decide`** — step 3 in
   the build order below is therefore the load-bearing one.
2. **branching postconditions:** fail only because the fixed tactic did not
   split the guard; the `+split` row proves a mechanical `by_cases` closes it.
   So this is "the discharger must split guards," a fragment feature.

This result is locked by `scripts/tests/check_operational_vc_auto_discharge.sh`:
the passing four live in `closes.lean` (must type-check); the two boundary cases
live in `boundary.lean` (must fail — the open-gap tripwire). When V1's
cast-normalization fragment lands, the boundary cases start closing, the gate
flips, and the flip is the explicit signal to promote them and update this note.

**Deferred edge — measured but out of the 80/20:** the full compression `round`
needs a reusable `v ^^^ 0xFFFFFFFF = ~~~v` not-mask lemma (bakeable) *and* ~4M
heartbeats, so it is a real prover-COST question — V1 should bound or refuse it,
not silently attempt it; the message schedule is genuinely inductive and stays
`needs_lean`. The VC-*generation* half already exists (`genPreservationVC` and
the operational-step printer in `Report.lean`), so generation is not a new gap.

## Build Order

1. Add the forcing probe and examples without changing evidence claims.
2. Define the supported Core / ProofCore fragment and the VC shape.
3. Implement straight-line symbolic execution for locals and pure expressions.
4. Add the bitvector route through `bv_decide`.
5. Add the linear/integer route through `omega`.
6. Add loop-step VC generation for explicit invariants and variants.
7. Add report classifications: `auto_closed`, `needs_lean`,
   `not_supported`, `blocked_by_effect`, and `blocked_by_trust`.
8. Gate positive and negative cases with
   `scripts/tests/check_operational_vc_auto_discharge.sh`.

The gate must include at least one negative where automation cannot close the
obligation and must prove the tool does not report a false green.

## Trust Model

This feature should reuse the existing evidence taxonomy:

- kernel decision procedures produce `proved_by_kernel_decision`;
- external solvers, if ever added, remain `solver_trusted`;
- linked Lean theorems remain `proved_by_lean`;
- generated VCs and artifacts must be replayable.

The automation is valuable only if it lowers proof-authoring cost without
weakening the audit story.

## Relationship To SPARK-Class Assurance

SPARK-class assurance needs loop invariants, frame/dependency facts, and
reviewable evidence bundles. Operational VC auto-discharge is the automation
layer that makes those annotations affordable for common systems code. Without
it, Concrete can express strong claims but still asks users to hand-write Lean
for many ordinary operational postconditions. With it, Concrete can make the
common decidable cases cheap while keeping harder proofs explicit.
