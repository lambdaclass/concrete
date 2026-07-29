# Lean vs. Rocq trade-offs (as they apply to Concrete)

**Status:** informal notes, not a decision record. Prompted by Joomy Korkut,
"Why Rocq is better than Lean" (2026-07-28,
<https://joomy.korkutblech.com/posts/2026-07-28-why-rocq-is-better.html>).
Captured 2026-07-29.

The article argues Rocq beats Lean **specifically for program verification**
(writing executable code and proving it correct), while conceding Lean's lead
for formalizing mathematics. Concrete lives squarely in that "verify then run"
domain, so the critique is on-target. This note records which of the author's
points bite Concrete, which Concrete's design already neutralizes, and where the
cost lands as Concrete grows past `ProvableV1`.

## The article's points

1. **Coinductive types / cofixpoints.** Rocq has native, executable `CoFixpoint`
   (infinite streams, interaction trees, reactive/non-terminating programs). Lean's
   `coinductive` only generates bisimulation predicates, not executable codata;
   the QPFTypes library fails on parameterless / mutual / indexed codata. Lean's
   `partial def` *runs* but is **kernel-opaque** — no equation lemmas, `rfl`/`simp`
   cannot reduce it — so it can't be unfolded in proofs.
2. **Nested inductive types.** Lean's kernel rejects some nested inductives Rocq
   accepts — e.g. a recursive relation pairing name-equality **and** recursive
   validation inside a `Forall2` (error: "nested inductive datatype parameters
   cannot contain local variables"). Workaround: split the relation or hand-write
   custom induction principles.
3. **Program extraction.** Rocq offers a menu of extraction backends
   (OCaml/Haskell/Scheme, plus *verified* pipelines: CertiCoq, CompCert), giving an
   end-to-end correctness story. Lean compiles through one opaque runtime with no
   verified pipeline and deliberately unreadable output.
4. **Ecosystem maturity.** Rocq has decades of exercised infrastructure: Iris
   (concurrent separation logic), VST, CompCert, Fiat Crypto, verified parsers.
   Lean equivalents exist but are young.
5. **Certification precedent.** ANSSI published criteria for using Rocq in Common
   Criteria evaluations; CompCert qualified for aircraft. Lean inherits none of
   this track record.
6. **"AI prefers Lean" rebuttal.** The author argues the model-tooling advantage is
   real but transient — models adapt to well-documented languages — so it's a weak
   basis for a foundational choice.

## How each point lands on Concrete

### 1. Coinduction — mostly NOT a threat, by design
`ProvableV1` deliberately excludes recursion, unbounded loops, heap, and FFI;
loops are modeled with **fuel** (bounded, terminating). Concrete never needs
coinductive semantics for the current proof story. Its finitist,
statically-enumerable ethos is exactly the move that dodges Lean's weakest area.

Caveat — this is the forward-looking risk: the deferred features
(unbounded-loop trace semantics, long-running/reactive programs behind the
reserved `Thread`/`Device` capabilities) are *precisely* where coinduction /
interaction-tree modeling is the natural tool, and precisely where Lean is
weakest and Rocq strongest.

### 2. Nested inductive datatypes — the exact wall does NOT bite us
Concrete's core IR is nested-through-`List` (mutual `CExpr` / `CMatchArm` /
`CStmt` in `Concrete/Elab/Core.lean`), but the `List` parameters carry no local
variables, so Lean's kernel accepts it. The author's precise trigger — an
inductive *relation* pairing equality + recursive validation in a nested
position — never occurs, because Concrete defines typing/evaluation as
**functions**, not `Prop`-valued inductive relations (see
`Concrete/Semantics/TypeJudgment.lean`, `eval`/`evalBinOp`). Functional recursion
through `List` isn't subject to the kernel's nesting/positivity rules, so the
whole class of problem is designed out.

### 3. Extraction — NOT a threat; Concrete builds the thing the article wants
Concrete does not use Lean extraction. It has its own LLVM backend (QBE planned)
and proves the lowering sound in Lean (`Concrete/Proof/ProofSoundness.lean`). It
is itself a verified compilation pipeline with an explicit trusted surface. The
only residual: the Concrete **compiler binary** runs on Lean's unverified
runtime — but that's a trusted component, differentially tested against the
reference interpreter, which is a reasonable trust boundary.

### 4. Ecosystem — mostly N/A by philosophy
`docs/DESIGN_POLICY.md`'s admission principle favors a small, compiler-owned
proof surface over importing large frameworks; Concrete rolls its own minimal
`ProofKit`/`ProofCore`. The only place Rocq's ecosystem lead would matter is the
deferred heap/allocation and concurrency proofs, where Iris-grade separation
logic is decades ahead and Concrete would be reinventing a lot.

### 5. Certification precedent — strategically relevant, unchanged
Concrete targets auditable, safety-critical low-level code (constant-time crypto,
ELF parsing, evidence ledgers). Its Lean-based TCB inherits none of the
ANSSI/CompCert regulatory track record Rocq carries. Doesn't affect the
engineering, but worth a `DECISIONS.md` note if certification ever becomes a goal.

### 6. "AI prefers Lean" — meta
Undercuts one common "why we picked Lean" justification, but not actionable.

## What we actually hit in Lean (grounded in the code)

The *exact* nested-inductive rejection (point 2) never bit us. Two **adjacent**
frictions did — and notably, the first is the mechanism behind the author's
*coinduction* complaint (point 1), not his nested-inductive one:

1. **`partial def` opacity.** `cExprToPExprImpl` used `List.mapM` over child
   lists; Lean's structural-recursion checker can't see `mapM` as decreasing, so
   the function had to be `partial def` — kernel-opaque, no equation lemmas, no
   `simp`/`rfl` reduction (our own note, `Concrete/Proof/ProofCore.lean:~1098`).
   This is verbatim the author's `partial def` criticism. **We already escaped it**
   by hand-rewriting every `mapM` into explicit structural-recursion helpers
   (`cExprListToPExpr`, `cFieldsToPExpr`, `cMatchArmsToP`, …) so the mutual block is
   non-`partial` and theorems reduce by `simp` (see
   `Concrete/Proof/ProofSoundness.lean:~461`).
2. **`DecidableEq` auto-deriving fails through container recursion**
   (`Concrete/Proof/Proof.lean:~189`): Lean won't derive `DecidableEq` for
   `List (String × PVal)` recursion; those proofs were converted from
   `native_decide` to `simp`-based unfolds. A hand-written mutual instance is noted
   as "the standard pattern" if ever needed.

## Takeaways

- **Concrete's design is, in effect, a set of pre-emptive answers to the article's
  Lean critiques.** Finitist fragment → dodges the coinduction gap. Own verified
  LLVM backend → dodges the "no verified extraction" gap. Functions-not-relations
  and explicit-recursion-not-`mapM` → dodges `partial def` opacity and
  nested-inductive friction. The one inherent kernel weakness it can't design away
  is a shape it simply doesn't lean on.
- **The real signal is about the deferred roadmap, not today's fragment.**
  Unbounded-loop trace semantics, reactive/long-running programs, recursive proof
  functions, and heap/allocation proofs are exactly the territory where the article
  says Lean is weak and Rocq strong. Treat each as a spot to budget extra
  Lean-fighting effort — and as a smell to watch (`partial def`, inductive
  relations over nested trees) at the boundary of the provable fragment.
- **This is not an argument to switch to Rocq.** Concrete's differentiator (the
  evidence-class ledger, the "nothing is dark" model) is orthogonal to the host
  prover, and Concrete has already absorbed the specific Lean frictions the article
  warns about. The article's value here is as a **checklist for the edge of the
  provable fragment**, not a migration case.
