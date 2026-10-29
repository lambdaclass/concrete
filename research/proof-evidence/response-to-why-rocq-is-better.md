# A response to "Why Rocq is better than Lean"

**Status:** position note / draft response (2026-07-29).
Re: Joomy Korkut, *"Why Rocq is better than Lean"*
(<https://joomy.korkutblech.com/posts/2026-07-28-why-rocq-is-better.html>).

This is a response from the perspective of Concrete — a no-GC systems language
*implemented in* Lean, that both compiles to LLVM/QBE and proves selected properties of
user programs in Lean. Korkut's argument is scoped to **program verification**
(executable code + proofs), which is exactly Concrete's domain, so it deserves a direct
answer rather than a defense of Lean in general.

**Short version:** we agree with most of the diagnosis and disagree with the conclusion
*for this kind of project*. Several of the article's Lean criticisms are real, but a
system designed around a finitist, artifact-first fragment either avoids them by
construction or has already paid the small tax to work around them. The one point that
genuinely stands — certification lineage — is orthogonal to the type theory.

---

## Where we agree

- **`partial def` opacity is real, and it bites.** Lean's `partial def` runs but is
  kernel-opaque (no equation lemmas, no `simp`/`rfl` reduction), so you cannot unfold it
  in proofs. We hit this exactly where the article predicts: extraction functions using
  `List.mapM` over child lists could not be seen as structurally decreasing. This is a
  fair criticism of Lean.
- **Coinduction is Lean's weakest area.** Native executable codata is immature; the
  library story (QPFTypes) fails on parameterless/mutual/indexed cases. For interaction
  trees and long-running/reactive programs, Rocq is materially better today.
- **Certification lineage is a genuine Rocq advantage.** ANSSI's Common Criteria criteria
  for Rocq and CompCert's aircraft qualification are real institutional facts a Lean-based
  TCB does not inherit. For safety-critical certification, this matters and we do not
  hand-wave it.

## Where the criticisms miss a finitist, artifact-first design

The article's three type-theory complaints — coinduction, nested inductives, extraction —
mostly do not bite a system built the way Concrete is:

- **Coinduction.** Concrete's provable fragment excludes recursion and unbounded loops;
  loops are modeled with **fuel** (finite, terminating). There is no infinite/coinductive
  object to reason about, so Lean's coinduction gap is simply out of scope for the current
  proof story. This is a design choice, not luck: a statically-enumerable, no-hidden-work
  language *wants* a finitist proof fragment. (Caveat: the gap reappears the day we model
  genuinely unbounded/reactive behavior — that is the boundary to watch, and where we
  would most feel the pull toward Rocq.)

- **Nested inductive datatypes.** The article's precise trigger — an inductive *relation*
  pairing name-equality *and* recursive validation inside a `Forall2` — never arises,
  because Concrete defines typing and evaluation as **total functions, not `Prop`-valued
  inductive relations**. Functional recursion through `List` is not subject to the
  kernel's nesting/positivity rules. The problem class is designed out, not worked around.

- **Extraction.** This one we simply do not depend on. Concrete does not use Lean
  extraction; it emits its own LLVM/QBE and proves the lowering sound. The article's
  critique of Lean's single opaque runtime is a critique of a pipeline we replaced. If
  anything, Concrete is *building* the verified-pipeline story the article praises Rocq
  for — with its own trusted surface made explicit rather than inherited.

The residual Lean frictions we did meet (`partial def` opacity; `DecidableEq` auto-derive
failing through container recursion) we resolved with bounded, one-time engineering:
rewrite `mapM` into explicit structural recursion so the mutual block is non-`partial` and
reduces under `simp`; convert `native_decide` proofs to `simp`-based unfolds. The cost was
real but small, and paid once.

## Why we do not conclude "switch to Rocq"

Two reasons, one specific to Concrete and one general.

1. **The differentiator is orthogonal to the host prover.** Concrete's thesis is an
   *evidence ledger* — keeping `proved` / `enforced` / `tested` / `runtime_checked` /
   `assumed` / `trusted` as distinct, composing, audit-visible classes — plus a no-GC
   systems substrate. Neither depends on whether the kernel is Lean's or Rocq's. Switching
   hosts would re-litigate the type theory without advancing the actual thesis.

2. **The honest answer to a multi-kernel argument is multi-kernel, not mono-kernel.** If
   independent kernel checking is valuable — and we think it is — the move is to make
   obligations **prover-neutral** and let *several* checkers attest, not to pick a single
   "better" one. A Lean-hosted compiler can emit the same obligation to Lean *and* Rocq
   (and Isabelle, and certificate-checked SMT), yielding a strictly stronger
   `proved_by_two_kernels` class. That turns the article's Lean-vs-Rocq framing into a
   feature: the trust value is in the *independence* of the checkers, and Lean+Rocq are
   both CIC-flavored — so if we add a second kernel purely for independence, an
   HOL system (Isabelle) buys more than Rocq, while Rocq wins specifically on
   certification. The right axis is "which independent checkers, and why," not "which one
   language."

## The one point we take as a to-do

Certification lineage. It is the article's strongest point against a Lean-hosted project
and it is not answerable by type-theory arguments. Our planned response is the
prover-neutral obligation layer above: emit obligations to Rocq as a *second* checker
specifically so the certifiable artifact can carry the lineage regulators recognize —
without rewriting the compiler or abandoning the evidence model. See
[`why3-architecture-and-positioning.md`](why3-architecture-and-positioning.md) and
[`lean-vs-rocq-tradeoffs.md`](lean-vs-rocq-tradeoffs.md).

### Prototype status (2026-07-30, branch `spike/multi-prover-evidence`)

The multi-kernel argument is no longer just a claim — there is a working spike. A
prover-neutral driver (`Report.ProverLowering`: a binop column plus a goal template)
lowers the *same* structured obligation to multiple independent kernels, dispatched
behind opt-in flags:

```
concrete <file>.con --report multi-kernel --rocq --isabelle
```

On a linear no-overflow obligation (`a + b` with both operands bounded), all three
kernels independently discharge it and the evidence graduates:

```
[demo.add_bounded#ovf0]  a + b  (range [-2147483648, 2147483647])
    lean:omega = true   rocq:lia = true   isabelle:presburger = true
    => proved_by_multi_kernel (3: lean, rocq:lia, isabelle:presburger)
```

Notes that make this honest rather than decorative:

- **Foundational independence, demonstrated.** Two kernels are CIC (Lean `omega`, Rocq
  `lia`) and one is HOL (Isabelle `presburger`). Agreement across CIC *and* HOL is the
  strong form of the independence argument — a shared CIC soundness bug cannot explain
  a HOL kernel also accepting the goal.
- **The badge has teeth.** A weakly-bounded `a * b` (only `0 <= a, 0 <= b`) is closed by
  *no* kernel and honestly stays `unproven` — the class is earned, not stamped.
- **Absent kernels never attest.** Without `--rocq`/`--isabelle` (or when the tool is off
  PATH) the kernel column reads "not run" and nothing graduates; an unavailable checker
  cannot manufacture evidence.
- **New classes are distinct.** `proved_by_rocq` / `proved_by_two_kernels` /
  `proved_by_multi_kernel` are added to the one `statusVocabulary` and do not launder
  past a `trusted` boundary.

Follow-on work landed on the branch (all with skip-if-absent tests in
`scripts/tests/check_multi_kernel.sh`):

- **All runtime-safety families, not just overflow.** A prover-neutral
  `MultiKernelObl` routes overflow / array-bounds / division-by-zero obligations
  through one lowering; each graduates across Lean/Rocq/Isabelle.
- **Real ledger + release gate.** `foldMultiKernelResults` folds independent-kernel
  agreement into the one `ObligationCore` ledger (so `proved_by_two_kernels` /
  `proved_by_multi_kernel` are produced by a real path, not only the report), and
  `--report multi-kernel --require-two-kernels` is a CI gate that exits non-zero when
  any obligation falls below two kernels.
- **Bridge differential-check** (`--report bridge-check`). An independent concrete
  evaluator fuzzes hypothesis-satisfying inputs; a proved obligation refuted by a
  concrete input would expose a Core→VC bridge/discharge unsoundness (exit 1). It has
  teeth: on an unproved obligation (`a*b` with only sign bounds) it finds the real
  overflow. This directly targets the *bridge*-vs-*checker* gap above — still
  single-sourced bridge, but now continuously cross-checked against execution.
- **Solver certificate-check** (`--report solver-cert`). A nonlinear VC an SMT solver
  reports `unsat` (`solver_trusted`, solver in the TCB) that Rocq's `nia` also closes
  graduates to `solver_checked` — a kernel corroborates the solver, dropping it from
  the trusted base. Honest boundary: kernel corroboration, not literal LRAT/Alethe
  proof-term replay (that needs a certified SAT/SMT checker — future work).

Scope caveat: today the shared fragment is linear integer arithmetic (the favorable
slice) over `#[overflow_checked]` obligations. It proves the *mechanism* — same
obligation, N independent kernels, graded evidence — not the hard deferred features
(heap, coinduction) where the provers would diverge in capability. Certification lineage
itself still requires a qualified Rocq artifact; the spike proves the plumbing that would
carry it. Toolchain is provided via the flake devShell (`coq` + `coqPackages.stdlib`,
`isabelle`).

### Checker diversity vs. bridge diversity (what the badge does *not* attest)

The pipeline has two translation steps, and the spike only diversifies the second:

```
Concrete Core IR
   │  Core -> obligation   (overflowObligations / toLeanProp)   <- THE BRIDGE
   ▼                                                               single-sourced, TRUSTED
obligation Expr   ∀ vars, hyps -> (lo <= e <= hi)               <- THE SPEC (one object)
   ├─ to Lean ─ omega (CIC)
   ├─ to Rocq ─ lia   (CIC)     <- three independent CHECKERS
   └─ to Isabelle ─ presburger (HOL)                             genuinely diverse
```

- **What it buys: checker diversity.** Independent kernels + decision procedures, spanning
  CIC *and* HOL, each confirm the *same* obligation `Expr`. This hardens the "is the
  formula true?" step — a soundness bug in one kernel/decision procedure is caught.
- **What it does NOT buy: bridge diversity.** The `Core -> obligation` lowering that
  connects the *program's semantics* to that formula is produced once by Concrete and is
  identical for all N kernels. A bug there — wrong integer width, a dropped `#[requires]`
  conjunct, wrong overflow semantics for `+` — yields the same poisoned obligation to
  every kernel; all N prove it and agree, and the badge fires with false confidence.
- **Not even spec diversity, strictly.** The obligation `Expr` is a single object; the
  three per-prover renderers re-*spell* it but do not re-*derive* it. Their agreement is on
  the boolean "closed?", not "closed the same formula?", so a transcription bug that maps
  to a different-but-provable goal is invisible. The renderers are a weak sanity check, not
  independent specs.

So the honest reading of the badge is **"N independent kernels confirm this VC is valid,"
never "N kernels confirm the program is correct."** The HOL-vs-CIC independence is real but
sits *entirely downstream of the bridge*; it does nothing for the trust-critical question
of whether the VC is the *right* VC. The report prints this boundary inline.

Genuine bridge diversity is a much larger effort (roughly the size of the existing
`ProofCore` embedding, ~2–4k lines, vs. this spike's ~300): re-encode Core's `eval`
semantics in each prover and *re-derive* the obligation there, then either differentially
test the re-encoded semantics against the reference interpreter (pragmatic, evidence-grade)
or prove agreement (research-grade). And if both encodings are *emitted* from one Concrete
generator, that generator is a shared bug surface — bridge diversity requires genuinely
independent derivations, not one generator with two backends. See the Core->Rocq
extraction / "Realization" row in
[`why3-architecture-and-positioning.md`](why3-architecture-and-positioning.md).

## Summary

The article is right that Lean is weaker than Rocq on coinduction, `partial def`
transparency, and certification history. It is right that this matters for program
verification. But for a system that deliberately lives in a finitist, functions-over-
relations, own-verified-codegen fragment, most of that weakness is out of scope or
cheaply handled — and the part that genuinely stands (certification) is better answered by
*adding* Rocq as one independent checker than by *switching* to it. We read the piece less
as "use Rocq" and more as a precise map of the edge of Lean's comfortable fragment — which
is exactly the boundary an evidence-first language should be honest about.
