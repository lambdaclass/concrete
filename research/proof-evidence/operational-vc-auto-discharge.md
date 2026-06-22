# Operational VC Auto-Discharge

Status: research / roadmap candidate

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

## Forcing Probe

Before building the feature, run a forcing probe over existing obligations that
currently rely on hand Lean or are missing:

1. `examples/loop_invariant`: operational preservation for the counting loop
   and the `count_to_eight` style postcondition.
2. HMAC / SHA bitvector identities such as `ch_refines` or
   `ch_selects_high`.
3. One bounded parser, codec, or fixed-buffer obligation from a workload once it
   exists in the examples tree.

For each case, emit the candidate operational VC and measure:

- whether `omega` or `bv_decide` can close it without a human bridge theorem;
- how much source/Core context is needed;
- which unsupported constructs block automation;
- whether the report can explain the fallback cleanly.

If the probe cannot close any real obligation, keep the feature deferred and
use the probe results to improve proof stubs instead. If it closes even a small
repeatable class, build V1 with gates.

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
