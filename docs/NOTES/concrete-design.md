# Concrete — Design Session Notes

**Date:** 2026-07-29
**Scope:** A working session covering (1) cloning `unbalancedparentheses/concrete2`,
(2) analyzing the project against Joomy Korkut's "Why Rocq is better than Lean"
article, (3) estimating a Rocq port, (4) a dual/multi-prover architecture, (5) a
prover-neutral obligation IR, and (6) which additional proof backends are worth adding.

This file records the questions asked and the answers given, in order.

---

## Q1. Clone `unbalancedparentheses/concrete2`

Cloned to `/home/unbalancedparen/projects/concrete2`. It's a Lean project
(latest commit at clone time: `8bfa8245`, "feat: R-0004 steps 2-3 — typed identity
boundary and canonical finite tables").

---

## Q2. Analyze the project and read the article — anything interesting for Concrete?

Article: <https://joomy.korkutblech.com/posts/2026-07-28-why-rocq-is-better.html>

### The article's thesis
Rocq (Coq) beats Lean **specifically for program verification** (writing executable
code and proving it correct), while conceding Lean's lead for formalizing mathematics.
Six technical arguments:

1. **Coinductive types / cofixpoints.** Rocq has native, executable `CoFixpoint`
   (infinite streams, interaction trees, reactive/non-terminating programs). Lean's
   `coinductive` (added 4.25) only generates bisimulation predicates, not executable
   codata; the QPFTypes library fails on parameterless / mutual / indexed codata.
   Lean's `partial def` *runs* but is **kernel-opaque** — no equation lemmas, `rfl`/`simp`
   cannot reduce it — so it can't be unfolded in proofs.
2. **Nested inductive types.** Lean's kernel rejects some nested inductives Rocq
   accepts — e.g. a recursive relation pairing name-equality **and** recursive
   validation inside a `Forall2` (error: "nested inductive datatype parameters cannot
   contain local variables"). Workaround: split the relation or hand-write custom
   induction principles.
3. **Program extraction.** Rocq offers a menu of extraction backends
   (OCaml/Haskell/Scheme, plus *verified* pipelines: CertiCoq, CompCert), giving an
   end-to-end correctness story. Lean compiles through one opaque runtime with no
   verified pipeline and deliberately unreadable output.
4. **Ecosystem maturity.** Rocq has decades of exercised infrastructure: Iris
   (concurrent separation logic), VST, CompCert, Fiat Crypto, verified parsers.
   Lean equivalents (Iris-Lean, mvcgen, Velvet) exist but are young.
5. **Certification precedent.** ANSSI published criteria for using Rocq in Common
   Criteria evaluations; CompCert qualified for aircraft (ATR 42/72). Lean inherits
   none of this track record.
6. **"AI prefers Lean" rebuttal.** The tooling advantage is real but transient —
   models adapt to well-documented languages — so it's a weak basis for a foundational
   choice.

### What Concrete is (project analysis)
**Concrete** is a **Lean-hosted, verification-oriented systems programming language**:
a compiler written in Lean that (a) compiles Concrete → LLVM via its own backend
(QBE planned), and (b) proves Concrete programs correct in Lean via a `ProofCore` /
evidence-class machinery.

- **Thesis:** systems control **plus** evidence accounting. Keeps proofs, tests,
  solver results, runtime checks, and assumptions as *distinct, inspectable classes*
  rather than one "verified" badge.
- **Language model:** linear ownership (non-`Copy` values used exactly once),
  capabilities in signatures (`with(Console)`, `with(File)`, …), explicit cleanup
  (`defer x.drop()`), whole-program monomorphization, no closures/trait-objects/macros.
- **Nine evidence classes:** `proved_by_lean`, `proved_by_kernel_decision (omega)`,
  `proved_by_kernel_decision (bv_decide)`, `solver_trusted`, `tested_by_oracle`,
  `runtime_checked`, `enforced`, `assumed`, `trusted`.
- **ProvableV1 fragment (first admitted proof subset):** authority-free, non-trusted,
  **no recursion, no unbounded loops, no heap, no FFI**; loops modeled with **fuel**.
  Types: `Int`, `Bool`, fixed integers, structs, enums, fixed arrays.
- **Size:** ~35k LOC compiler, ~11.3k LOC proofs, zero Mathlib/Batteries dependency.
- **Roadmap:** immutable `R-NNNN` task IDs; currently Phase 7+.

### How each article point lands on Concrete
- **Point 1 (coinduction) — mostly NOT a threat, by design.** ProvableV1 excludes
  recursion/unbounded loops; loops use fuel (finite, terminating). Concrete never needs
  coinductive semantics today. Its finitist, statically-enumerable ethos is exactly the
  move that dodges Lean's weakest area. *Caveat:* deferred features (unbounded-loop trace
  semantics, reactive/long-running programs behind the reserved `Thread`/`Device`
  capabilities) are precisely where coinduction/ITrees are the natural tool and where
  Rocq is materially stronger.
- **Point 2 (nested inductives) — the exact wall does NOT bite.** Concrete's core IR is
  nested-through-`List` (`Concrete/Elab/Core.lean`, mutual `CExpr`/`CMatchArm`/`CStmt`),
  but the `List` params carry no local variables, so Lean's kernel accepts it. The
  precise trigger (an inductive *relation* pairing equality + recursive validation in a
  nested position) never occurs because Concrete defines typing/evaluation as
  **functions**, not `Prop`-valued inductive relations
  (`Concrete/Semantics/TypeJudgment.lean`). Functional recursion through `List` isn't
  subject to the kernel's nesting/positivity rules, so the whole problem class is
  designed out.
- **Point 3 (extraction) — NOT a threat; Concrete builds the thing the article wants.**
  Concrete doesn't use Lean extraction; it has its own LLVM backend and proves the
  lowering sound (`Concrete/Proof/ProofSoundness.lean`). It is itself a verified
  compilation pipeline with an explicit trusted surface. Residual: the compiler *binary*
  runs on Lean's unverified runtime — a trusted component, differentially tested against
  the reference interpreter (a reasonable trust boundary).
- **Point 4 (ecosystem) — mostly N/A by philosophy.** `docs/DESIGN_POLICY.md`'s
  admission principle favors a small, compiler-owned proof surface over importing large
  frameworks. Only relevant to the deferred heap/concurrency proofs, where Rocq's Iris is
  decades ahead.
- **Point 5 (certification) — strategically relevant, unchanged.** Concrete targets
  auditable, safety-critical low-level code; its Lean TCB inherits none of the
  ANSSI/CompCert regulatory track record.
- **Point 6 ("AI prefers Lean") — meta.** Undercuts one common "why Lean" justification;
  not actionable.

### What Concrete actually hit in Lean (grounded in code)
The *exact* nested-inductive rejection never bit. Two **adjacent** frictions did — and
the first is the mechanism behind the article's *coinduction* complaint:

1. **`partial def` opacity.** `cExprToPExprImpl` used `List.mapM` over child lists;
   Lean's structural-recursion checker can't see `mapM` as decreasing → forced
   `partial def` → kernel-opaque, no equation lemmas, no `simp`/`rfl` reduction
   (`Concrete/Proof/ProofCore.lean:~1098`). This is verbatim the article's `partial def`
   criticism. **Concrete already escaped it** by hand-rewriting every `mapM` into explicit
   structural-recursion helpers (`cExprListToPExpr`, `cFieldsToPExpr`, `cMatchArmsToP`, …)
   so the mutual block is non-`partial` and theorems reduce by `simp`
   (`Concrete/Proof/ProofSoundness.lean:~461`).
2. **`DecidableEq` auto-deriving fails through container recursion**
   (`Concrete/Proof/Proof.lean:~189`): Lean won't derive `DecidableEq` for
   `List (String × PVal)` recursion; those proofs were converted from `native_decide` to
   `simp`-based unfolds. A hand-written mutual instance is noted as "the standard pattern."

### Takeaways
- Concrete's design is, in effect, a set of pre-emptive answers to the article's Lean
  critiques: finitist fragment → dodges coinduction; own verified LLVM backend → dodges
  the extraction gap; functions-not-relations and explicit-recursion-not-`mapM` → dodge
  `partial def` opacity and nested-inductive friction.
- The real forward-looking signal is about the **deferred roadmap** (unbounded-loop trace
  semantics, reactive programs, recursive proof functions, heap/allocation proofs) —
  exactly the territory where the article says Lean is weak and Rocq strong.
- **Not** an argument to switch to Rocq: Concrete's differentiator (the evidence-class
  ledger) is orthogonal to the host prover, and it has already absorbed the specific Lean
  frictions the article warns about. The article's value is as a **checklist for the edge
  of the provable fragment.**

*(This analysis was also written to `docs/NOTES/lean-vs-rocq-tradeoffs.md` in the repo.)*

---

## Q3. How difficult would it be to port the compiler to Rocq?

The crux: **Lean is both a programming language and a proof assistant; Rocq is really
only the latter** — Gallina is a *total* language, not general-purpose. So "port the
compiler to Rocq" is four jobs with very different costs:

1. **Pure total passes** (AST→Core→SSA transforms) — portable to Gallina, but every
   recursion needs an explicit termination argument (well-founded or fuel).
2. **The effectful shell** (file IO, invoking clang, mutable maps/hashing) — *not
   expressible in Gallina*; becomes OCaml glue around extracted code (ironically the
   extraction pipeline the article praises).
3. **Metaprogramming** (macros, `deriving`, elaborators) — a paradigm rewrite.
4. **The proofs** — a full tactic-language rewrite.

### Quantitative sweep of the codebase
| Category | LOC | Notes |
|---|---|---|
| Compiler impl (`Concrete/` excl. Proof) | ~35,202 | IO-free core; passes are `Except`-based |
| Proof code (`Concrete/Proof/` + `proofs/`) | ~11,265 | tactic-mappable |
| Std lib (`std/*.con`) | ~13,263 | user-facing Concrete lib |
| Tests | ~32,821 | mostly `.con` programs |

- **`partial def`: 426 total.** Concentrated in executable plumbing — Parser (56), Mono
  (41), ReportVC (33), Report (27), Lower (26), Interp (23) — and only 32 in the proof
  kernel (`ProofCore`). Each needs a termination measure/fuel in Rocq. This is the single
  biggest port cost, and it lives almost entirely in code you would *not* reimplement.
- **IO/effects:** ~500 LOC of IO glue in `Main.lean` (git, clang, `llvm-as`,
  `/proc/self/statm`); `IO.Process.spawn` ×8; parser uses `StateT`/`ExceptT`. No
  `IO.Ref`. The compiler *core* is IO-free and Gallina-natural.
- **Metaprogramming:** effectively **zero** — **no `Lean.Elab`/`Lean.Meta` imports**;
  `DESIGN_POLICY.md` bans macros/`#[derive]`. The reported `elab`/`syntax` hits are almost
  certainly the compiler's own *elaboration pass* (the `Elab.lean` module), not Lean
  metaprogramming. (Correction to the raw sweep, which over-counted this bucket.)
- **Dependencies:** zero Mathlib, zero Batteries; only a few `Std.Data.HashMap/HashSet`
  and `Std.Tactic.BVDecide` imports. Very low external burden.
- **Proof tactics map ~1:1:** `omega`→`omega`/`lia`, `simp`→`simp`/`simpl`,
  `bv_decide`→`bv`, `decide`→`decide`/`vm_compute`. Only `native_decide` (5 uses) lacks a
  direct analogue (→ Rocq `native_compute`).

### Verdict
Two corrections to a naive estimate: (a) metaprogramming cost is ~zero, not weeks;
(b) a "4–5 month" figure is a *one-time* number for a *frozen* target — but Concrete is
a moving target (Phase 7+, live roadmap), so the real cost of a full port is the
**recurring divergence** of maintaining two compilers in lockstep, which is unbounded.
A full executable port is feasible but expensive and never "done." **The part worth
dual-hosting is the ~6.8k-LOC soundness kernel**, which has the favorable profile
(tactic-mappable, dependency-free, few partials), not the 35k-LOC plumbing.

---

## Q4. Could Concrete have the compiler in *both* Rocq and Lean (given LLVM + QBE backends)?

**Backends fan *out*; a host prover is the *spine*.** LLVM and QBE are alternative
*outputs* of one compiler — cheap, additive, shared frontend. A second host prover is not
a leaf you add; it's the ground the whole thing stands on. "Also in Rocq" is a *second
parallel spine*, multiplicative cost, not a third backend.

The real question is *which part* you want in two provers. Three graduated options:

- **Option A — One host (status quo).** Lean only; Rocq as a doc reference. Zero cost.
- **Option B — Dual-check the *proof kernel only* (recommended).** Keep the executable
  compiler in Lean. Additionally formalize only the **semantic model + soundness theorem**
  in Rocq. Make the obligation format **prover-neutral**; emit the *same* per-program
  obligations to Lean *and* Rocq, so any obligation can be re-checked by either kernel →
  a new, strictly-stronger evidence class `proved_by_two_kernels`. Bounded cost
  (~5–7 person-weeks against the kernel), delivers the Rocq certification lineage for the
  artifact that matters, and is exactly the "distinct classes of evidence" ethos.
- **Option C — Two full executable compilers (N-version).** Strongest integrity claim and
  the only standalone-certifiable Rocq artifact, but **more than doubles** ongoing cost
  (426 termination obligations + IO extraction + lockstep maintenance on a moving target).
  Reserve for the day a certifier specifically demands a native Rocq compiler.

**Why the numbers favor B:** the `partial def` tax and IO glue — the dominant port costs —
live in the plumbing you would *not* reimplement under B. The soundness kernel
(`ProofSoundness` 1.2k + `ProofCore` 2.7k + `Proof` 2.9k ≈ 6.8k LOC) is proof-heavy,
tactic-mappable, dependency-free, and its 32 partials are the *same* functions already
rewritten from `mapM` into explicit recursion — already in the shape Rocq wants.

**Recommendation:** two backends yes; for provers, don't port the compiler — port the
soundness kernel and make the obligation format prover-neutral. The single enabling task
is decoupling `ProofCore` from being "Lean-facing" into a serialized, host-neutral
obligation IR with per-host emitters (`--emit-lean`, `--emit-rocq`).

---

## Q5. Sketch the prover-neutral obligation IR

**Key realization:** Concrete already emits one obligation to two targets. In
`Concrete/Report/ReportVC.lean`, a VC (`Expr`) is lowered to a **Lean prop string**
(`toLeanProp`/`toLeanPropD`/`toLeanBV`) *and* to **SMT-LIB** (`exprToSmt`), both consulting
a shared operator table (`obBinOpLean`/`obBinOpSmt`), kept in sync by
`scripts/check_obligation_lowering.sh`. Prover-neutrality is a generalization of this
existing seam. The design is **two tiers**, matching the two existing obligation paths.

### Tier 1 — Neutral VC obligations (shallow, cheap, ~days)
Keep the VC as the structured `Expr`; make emission per-prover over the shared table.
Add:
```
obBinOpRocq : BinOp → Option (String × Bool)   -- ≤→"<=", ∧→"/\", etc.
toRocqProp  : Expr → Option String              -- mirror of toLeanProp
toRocqPropD : Expr → Option String              -- Rocq Z.quot/Z.rem TRUNCATE toward zero,
                                                 --   matching Concrete sdiv/srem — a *better*
                                                 --   match than Lean's floor division!
```
`bv_decide` VCs (`toLeanBV`) → Rocq `bv`/`bitblast`. This tier is one new column, one
pretty-printer, one row in the drift-check.

### Tier 2 — Neutral semantic obligations (deep, bounded, ~weeks)
Three parts:

**(a) Neutral term encoding.** `PExpr`/`PVal`/`PMatchPat` are already prover-agnostic
(and deliberately avoid nested-mutual-inductive shapes — `PMatchPat` is non-recursive so
`PExpr` can hold `List (PMatchPat × PExpr)` without a mutual block). Serialize to versioned
S-expr/JSON (`ObligationCore.toJson`/`ledgerJson` and `FnTable.schemaVersion` already
exist):
```
(binop (op shr 32 false) (var "x") (lit (int 3)))
(call  "parse_byte" ((var "data") (var "off")))     ; resolves via globals namespace
(apply "f" ((var "x")))                              ; resolves via callables namespace  (bug 061 — KEEP DISTINCT)
(match (var "r") ((pat-enum "Result" "Ok" ("v")) (var "v"))
                 ((pat-var "_")                    (lit (int 0))))
(fndef (id <CallableId>) (params "data" "off") (body <PExpr>))   ; identity = CallableId, NOT displayName
```
Two load-bearing invariants: **`.call` vs `.applyVar` stay distinct** (the two-namespace
`globals`/`callables` resolution that closed bug 061); **`displayName` is excluded from the
digest** (it's "EXPLICITLY NOT IDENTITY").

**(b) Neutral obligation record** — generalize `Obligation` + `SpecAttachment`:
```
NeutralObligation {
  function      : { qualName, fingerprint }        -- = FunctionIdentity
  spec          : { name, version }                -- = SpecIdentity
  subjectDigest : Hash                              -- R-0004 ProofSubjectDigest, computed over the
                                                    --   NEUTRAL term (not the Lean AST) ← critical
  body          : <neutral PExpr>
  goal          : <neutral PExpr / VC>
  hypotheses    : [<neutral PExpr>]
  dependencies  : [qualName]
  trustedDeps   : [qualName]                        -- audited escape hatches travel with the claim
  attestations  : [ HostAttestation ]               -- NEW: filled per prover
}
HostAttestation {
  host    : "lean" | "rocq" | ...
  proofRef: String                                  -- theoremName in that host (was SpecAttachment.proofName)
  status  : proved | stale | blocked | ineligible | trusted | ...   -- = ObligationStatus, per host
  method  : proved_by_lean | proved_by_kernel_decision(omega|bv) | solver_checked | ...
  checkedAgainstDigest : Hash                        -- must equal subjectDigest, else stale
}
```
Single most important point: **the staleness digest is computed over the neutral term**,
so both kernels agree on freshness independent of any host AST.

**(c) The semantics contract (the honest hard part).** `eval` (in `Proof.lean`) is the
meaning of the term language. For "proved in both kernels" to mean anything, Lean's `eval`
and Rocq's `eval` must define the *same* semantics — **and no single kernel can bridge two
kernels** (no checker sees both). The bridge is necessarily a *spec + conformance testing*,
not a proof. So the neutral IR ships a **conformance vector suite**:
`(neutral program, neutral input) → expected PVal`. Each host's `eval` must reproduce every
vector. Agreement of the two evaluators is itself evidence — the class Concrete already has:
`tested_by_oracle`.

New evidence class:
```
proved_by_two_kernels(ob) :=
     lean.status == proved  ∧  rocq.status == proved
  ∧  lean.checkedAgainstDigest == rocq.checkedAgainstDigest == ob.subjectDigest
  ∧  both evals conform on the vector suite covering ob.body     -- the tested_by_oracle bridge
  ∧  ob.trustedDeps identical in both attestations
```
Strictly stronger than `proved_by_lean`, and honest about *why* (it folds in a
`tested_by_oracle` component for the cross-semantics step rather than pretending it's pure
kernel evidence).

### Where Lean and Rocq physically split
Shared: obligation extractor (`CExpr → neutral PExpr`, one copy), neutral encoder,
conformance-vector generator. Per-host (the only divergent code): operator-table column,
term pretty-printer/deserializer, `eval` port, theorem-statement emitter, tactic mapping,
replay driver. None of the 35k-LOC plumbing, none of the 426 partials, none of the IO glue
crosses the line.

### Phasing & risks
1. Tier 1 now (Rocq column + `toRocqProp` + drift-check row).
2. Neutral encoding + digest-over-neutral-form (the R-0004 decoupling; valuable even if
   Rocq never ships).
3. Tier 2 Rocq emitter (port `eval`, emit stubs, conformance suite, `proved_by_two_kernels`).

**Risks (not hidden):** (a) the `eval` port is where a subtle semantic divergence could
silently weaken the two-kernel claim → the conformance suite must be *adversarial*;
(b) `PExpr` keeps growing (`while_step`, `arraySet`, casts were all added under workload
pressure), so the neutral encoding + Rocq `eval` become a second thing to keep in sync on
every semantic extension — gate it with the same drift-check discipline as
`check_obligation_lowering.sh`.

**Prior art:** this is a slice of **Why3** (one VC IR dispatched to Z3/cvc5/Coq/Isabelle) —
read its per-backend "soundness driver" design before building the dispatch layer.

---

## Q6. Would you add any other prover beyond Rocq? (Lean + SMT + Rocq → prove; LLVM + QBE → generate)

Among proof backends, value is **independence × TCB-minimization × certificate-checkability**,
not count — and it's bounded by the fragment (quantifier-free integer + bitvector + datatype
props + small functional-refinement proofs). Two flavors of the same logic buy little.

### The two axes
- **Codegen (LLVM/QBE):** fan-out; alternative outputs; cheap/additive (Cranelift/C/wasm
  later).
- **Provers:** fan-in; independent checkers; each is a permanent sync/maintenance tax on the
  neutral IR.

You already have **Lean** (host) *and* **SMT** (`exprToSmt`, `solver_trusted`). So the
question is the *marginal* checker.

### Biggest win isn't a new prover — make the SMT you have *checkable*
Upgrade `solver_trusted` → **`solver_checked`**:
- **Portfolio:** Z3 + cvc5 + **Bitwuzla** (BV specialist — relevant to SHA-256/HMAC).
- **Consume proof certificates and replay them in an existing kernel:** DRAT/LRAT at the
  bit level (you already do this for `bv_decide`), Alethe (cvc5)/LFSC at the SMT level.
  Turns "trust Z3" into "a tiny verified checker re-derived the proof." Highest
  trust-per-dollar; extends an existing mechanism; no new prover to babysit.

### The "additional full kernel" choice — pick by *why*
Lean and Rocq are **both dependent type theory (CIC-flavored)**, so Rocq is the *least
independent* choice on the logic axis. Its justification is different:

| Candidate | Uniquely buys | Cost/fit |
|---|---|---|
| **Rocq** | Certification lineage (CompCert, ANSSI Common Criteria). | Medium; logically similar to Lean (weak independence). |
| **Isabelle/HOL** | Max logical independence (classical HOL + LCF micro-kernel); best automation for this fragment (**sledgehammer**); **seL4** systems pedigree. | Different logic → real work, but sledgehammer may auto-discharge much of a QF fragment. |
| **Metamath Zero (MM0)** | Ultimate TCB minimization — tiny self-verified checker built (Mario Carneiro) for compiler/systems proofs. | Research-grade, small ecosystem, high effort; philosophically the closest fit to Concrete's "minimize trusted surface" identity. |

- Driver = "regulators must accept it" → **Rocq**.
- Driver = "two-kernel claim should span different logics + get automation" → **Isabelle**.
- Driver = "our identity is minimal trusted surface" → **MM0** (as a bottom-layer
  certificate checker, not an authoring environment).

**Skip:** Agda (too close to Lean, weak automation), HOL Light/HOL4 (dominated by Isabelle
for practicality or MM0 for minimality). F*/Dafny are rival languages, not backends.

### Evidence model scales, classes stay distinct
```
kernel-checked : proved_by_lean | proved_by_rocq | proved_by_isabelle | proved_by_mm0
solver-checked : solver_checked(z3|cvc5|bitwuzla, certificate replayed in <kernel>)   -- was solver_trusted
solver-trusted : solver_trusted(...)                                                  -- no certificate — weakest
```
`proved_by_N_kernels` = derived, strictly stronger; the more independent the kernels, the
stronger (Lean+Isabelle > Lean+Rocq on that metric).

### Recommendation (in order)
1. **Now:** certificate-checked SMT portfolio (`solver_trusted` → `solver_checked`;
   Z3+cvc5+Bitwuzla; LRAT/Alethe replayed in Lean). Biggest gain, extends existing work.
2. **First additional kernel — pick one by goal:** Rocq if certification is the point,
   **Isabelle** if independence + automation + seL4 pedigree is the point (lean this way
   unless regulatory acceptance is near-term).
3. **Aspirational / identity:** MM0 as a minimal bottom-layer checker.

Don't collect provers for their own sake. The strong, honest picture is:
**Lean + certificate-checked portfolio SMT + one deliberately-different kernel**, generating
LLVM/QBE.

**Prior art to steal from (not add):** Why3 (multi-prover VC dispatch); SAW/Cryptol for
crypto-equivalence workloads.

---

## Q7. Explain the Why3 model and what Concrete can use or copy

**What Why3 is:** a platform for deductive program verification whose whole job is the
thing Concrete is building — generate proof obligations and dispatch each to *many*
provers through a shared neutral layer. It's the engine under **Frama-C** (C), **SPARK**
(Ada, used in avionics/rail), etc. A production existence proof that "one VC layer → SMT
solvers *and* ITPs, over a real systems language" works.

### The five parts, mapped to Concrete
| Why3 concept | What it is | Concrete equivalent |
|---|---|---|
| **Task** | Self-contained obligation: context (decls/axioms/hypotheses) + one goal. | `NeutralObligation` (body + goal + hypotheses + deps). |
| **Transformations** | A pipeline of composable *logic-to-logic* rewrites applied before emit (eliminate algebraic types, eliminate inductive preds, inline, `split_goal`, compute, encode polymorphism), **per-backend selected**. | Your lowering (`toLeanProp`/`exprToSmt`) — currently hard-coded & monolithic. **The big thing to copy.** |
| **Driver** | *Declarative data file* per prover: how to print each symbol/operator, which transformations to run, which theories are built-in, the command line, timeout, and a **result-parsing regexp**. | `obBinOpLean`/`obBinOpSmt` table + pretty-printer + replay command — but as **data, not code**, with printing separated from transforming. |
| **Session** | Persistent record of which goal was proved by which prover under which transforms, plus a **goal shape/checksum** to re-associate proofs after edits (staleness). | Obligation ledger + `subjectDigest`/`fingerprint`/`ObligationStatus.stale` (R-0004). |
| **Realization** | For ITP backends, *prove in Coq/Isabelle that Why3's built-in axioms hold* in that system's model — anchoring trusted theories per ITP. | The **semantics contract** / `eval`-agreement problem. |

### Copy directly
1. **Explicit, composable transformation pipeline, driver-selected.** Model VC→target
   lowering as a list of small named transforms; each backend's driver picks which run.
   `split_goal` (break one obligation into independent sub-goals — portfolio spread +
   per-sub-goal progress), `inline_defs`/`compute` (normalize), `eliminate_algebraic`/
   `eliminate_match` (lower `PExpr` datatypes/`match_` for SMT while ITPs keep them
   intact). This is what lets ONE neutral obligation serve both first-order SMT and an ITP.
2. **Drivers as declarative data.** Push the `obBinOp` table all the way: per-prover config
   = symbol printing + selected transforms + replay command + **result regexp** + built-in
   theories (so native `Int`/`BitVec` isn't re-axiomatized). Adding a prover = write a
   driver file, not a lowering module. This is what makes the 5-prover vision sustainable.
3. **Session shape+checksum staleness.** Store a structural *shape* hash separately from an
   exact checksum, so an edit re-attaches a proof to a moved-but-unchanged goal instead of
   marking everything stale. Sharpens your `stale` vs `unbound` vs `depsNotCurrent`.

### Adapt (and improve): realization > conformance vectors
Why3's *realization* is a stronger bridge than the conformance vectors proposed in Q5:
instead of only testing evaluator agreement on samples, **prove inside each ITP that the
neutral semantics' axioms hold** in that ITP's model. Each kernel independently discharges
"the shared spec is sound *here*." (You still can't bridge two kernels to each other — no
checker sees both — but realization replaces "tested on vectors" with "proved sound against
the shared axiomatization, per kernel.") Adopt realization as primary; keep conformance
vectors as the cheap always-on cross-check.

### Do NOT copy
- **WhyML itself.** Why3 ships a whole ML-like source language + WP calculus; Concrete
  already has its own source + obligation extraction. Want only the task/driver/session
  layer.
- **Why3's trust posture.** Its transformations and drivers are **part of the TCB and
  largely unverified** — a printer bug can make a false goal look proved. That contradicts
  the evidence-ledger ethos. Copy the architecture but **diverge on trust**: prefer
  **certificate replay** (LRAT/Alethe checked in a kernel — the `solver_checked` idea) over
  trusting the driver, and keep the transform set small / proven sound where possible.
  This is where Concrete can be *better than* Why3.

### Action items
1. Refactor VC lowering into **named, composable task transformations** + per-backend
   selection (generalizes `check_obligation_lowering.sh`).
2. Turn the `obBinOp` table into a **declarative driver** (printing + transforms + replay +
   result regexp + built-in theories).
3. Adopt **session shape+checksum** staleness.
4. Use **realization** as the ITP semantics bridge; conformance vectors as cross-check.
5. Deliberately **improve on Why3's trust model** with certificate-checking — the
   differentiator.

**Prior art also worth a look:** SAW/Cryptol (crypto-equivalence via SMT).

---

## Artifacts produced this session
- Cloned repo: `/home/unbalancedparen/projects/concrete2`
- In-repo note: `docs/NOTES/lean-vs-rocq-tradeoffs.md` (the Q2 analysis; written earlier this
  session when explicitly requested)
- This file: `/home/unbalancedparen/concrete-design.md` (full session Q&A, incl. the Q5 + Q6
  + Q7 prover-neutral-obligation-IR design)

*Note: the Q5/Q6/Q7 design was NOT written into the repo as a separate proposal file — per
instruction to leave the repo untouched. The full design lives in this home-directory file.*
