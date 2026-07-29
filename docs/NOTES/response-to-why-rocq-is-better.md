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

## Summary

The article is right that Lean is weaker than Rocq on coinduction, `partial def`
transparency, and certification history. It is right that this matters for program
verification. But for a system that deliberately lives in a finitist, functions-over-
relations, own-verified-codegen fragment, most of that weakness is out of scope or
cheaply handled — and the part that genuinely stands (certification) is better answered by
*adding* Rocq as one independent checker than by *switching* to it. We read the piece less
as "use Rocq" and more as a precise map of the edge of Lean's comfortable fragment — which
is exactly the boundary an evidence-first language should be honest about.
