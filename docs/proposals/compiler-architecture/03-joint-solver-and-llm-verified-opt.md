**PROPOSED RESEARCH & DESIGN NOTE (2026-11-27); decides no language rule. Maps to provisional task [R-0732](../../../ROADMAP.md#r-0732) (extending `R-0299`, `R-0583`, and `R-0727`).**

# 03 — Kernel-Verified Refinement Validators, Untrusted Joint Backend Solvers, and Verified LLM Optimization

## Problem Statement

Two of the hardest problems in compiler engineering are actually **the exact same architectural problem**:

1. **The Backend Phase-Ordering Wall:** Instruction selection, instruction scheduling, register allocation, and spilling are jointly NP-hard and interfere destructively when run as separate sequential passes. LLVM hides this behind millions of lines of brittle heuristics. Verified compilers (CompCert, CakeML) prove modest sequential passes directly, which makes matching LLVM's native speed enormously expensive.
2. **The LLM-Era Optimization Opportunity:** [docs/PHILOSOPHY.md](../../PHILOSOPHY.md#what-concrete-is-for) explicitly expects most Concrete code to be written by language models and checked by humans and machines. An LLM (or an offline solver like CP-SAT / Unison / MAP-Elites under `R-0727`) can synthesize remarkably fast domain-specific implementations (bit-twiddling, cache blocking, SIMD schedules, algebraic simplifications)—**if and only if** the compiler can soundly reject any subtly buggy candidate without expanding the trusted computing base.

Both problems collapse cleanly once the compiler separates **untrusted search** from **kernel-checked validation**.

---

## 1. Core Principle: Zero-TCB Untrusted Search + Tiny Verified Validator

Concrete's pipeline contract ([Concrete/Contracts.lean](../../../Concrete/Contracts.lean)) already distinguishes:
- `EvidenceKind.directProof` — for stable structural passes (lexing, elaboration, lowering, BIR→LCB adaptation).
- `EvidenceKind.provedValidator` — for search-heavy, heuristic, or externally proposed transformations.

For any search-heavy step—whether it is an e-graph, a CP-SAT joint instruction-selector/register-allocator, a MAP-Elites search driver (`R-0727`), or an LLM proposing a faster body—**the proposer does not need a single line of Lean proof**. Only the validators need soundness proofs, and **each cheating vector gets its own explicit check** (never one collapsed "validated" label):

1. **Per-function / open-interface functional refinement** (not closed `main`-only comparison, which an adversary can cheat by corrupting an uncalled exported or helper function!).
2. **Exact trap-site correspondence + elimination accounting** (not bare `target ⊆ source` subset, which an adversary can cheat by swapping two existing trap locations `site₁ ↔ site₂`!).
3. **Explicit resource / stack non-regression** (plain functional refinement allows a candidate that returns the right answer while blowing stack slots, heap allocations, or step budgets!).

For every supported BIR callable entry, prove that each legal caller can resume with the typed return and observe the effects on its storage, loans and grants. The proof must cover finite and divergent executions under the source-safety hypothesis. A changed write can be observable even when both calls return zero and emit no events. The callable semantics and their connection to the registered execution are open obligations. [Proposed callable contract](01-validated-passes-and-ub-free-opt.md#1-pluggable-validatedpass-slots-in-interfacespipeline).

Use `BIR.Execution` for the registered process semantics. Its hosted result predicates alone cannot discharge callable refinement. Preserve project, entry, declarations, function signatures, mutable locals, layouts and the profile limit. Check trap provenance and each function's IR slot count separately. A module-wide maximum can hide a growing helper frame. The registry's composition obligation is `Pipeline.endToEnd` (or `registeredLCBEndToEnd` on its guarded LCB route), under the readiness, validation, trust and successful-run hypotheses that apply there. [Indexed contracts](../../../Concrete/Interfaces/Contracts.lean) · [Registry](../../../Concrete/Interfaces/Pipeline.lean).

---

## <a id="2-the-proposal--validator-boundary"></a>2. The Proposal ↔ Validator Boundary (`OptimizationProposal` / `ObligationBundle`)

The current artifact encoder supplies canonical framing. Add a bounded canonical decoder under R-0528 / AT 13 for these new wire records. Bind the witness to the exact inputs, route, profile and output, and extend the certificate's fixed obligation inventory before claiming replay. [Encoding](../../../Concrete/Artifact/Encoding.lean) · [Certificate](../../../Concrete/Certificate.lean).

### Outbound: `ObligationBundle` (Compiler → Untrusted Tool)
For a selected function (or hot loop/block), the compiler exports a self-contained canonical payload:
- Typed entity identity & canonical digest (`Artifact/Sha256.lean`),
- The selected IR (`Core` or `Interfaces.BIR lang profile`) plus any checked facts available for that slice ([doc 01](01-validated-passes-and-ub-free-opt.md#3-first-class-facttable-carrier-through-bir)),
- Target profile & cost metrics (per-function IR slot count in the first increment; later instruction, spill or step metrics need the matching model or artifact),
- For repair workflows (`R-0593`/`R-0599`): any unproved obligation proposition + concrete failing witness inputs.

### Inbound: `OptimizationProposal` (Untrusted Tool → Compiler)
The external tool returns one of three typed proposals:
1. **IR rewrite + replay witness (`birRewrite`):** Candidate `Interfaces.BIR lang profile` + a compact proof/replay witness (symbolic-execution alignment map, elimination ledger, e-graph rewrite trace, or register-liveness mapping).
2. **Source-level candidate (`sourceCandidate`):** A candidate `.con` function body proposed by an LLM or human against a reference `.con` implementation (`concrete verify-opt ref.con opt.con --fn <name>`).
3. **Layout or backend schedule (`layoutPlan` / `lcbSchedule`):** A candidate `LayoutPlan` ([doc 02](02-layout-plan-synthesis.md)) or LCB instruction/register schedule.

### Gate: Multi-Obligation Kernel-Checked Validator
- **Never trusts the proposer.** Re-parses/re-checks `WF`, runs **all three** proved validators (`validateFunctional`, `validateTrapSites`, `validateResources`) under deterministic work budgets (`R-0528`), and binds the accepted witness into the compilation certificate (`R-0552`, `R-0582`) once the corresponding fixed obligation is registered.

---

## 3. Three Tiers of Validators (Growing Cumulatively Across Slices)

### Tier A — `Interfaces.BIR` ↔ `Interfaces.BIR` Symbolic-Execution & Witness Validator (Start in `B0b` / Slice 1)
1. **Acyclic BIR.** Check symbolic values, effects, branches and trap slots from every callable entry. Compare each function's slot metric and show a nonidentity rewrite passes.
2. **Loop & CFG alignment (Slices 3–5).** Use local cut-point obligations with entry, preservation, coverage and progress proofs. Compose block verification conditions and require a checked well-founded rank on target-only cycles. Chained diamonds can have exponentially many paths. Measure replay cost and admit at least one nonidentity rewrite.
3. **Proof-producing e-graphs (R-0299).** Replay each selected rewrite against its rule, occurrence, substitution and side conditions under a deterministic work limit.

### Tier B — `concrete verify-opt <ref.con> <opt.con> --fn <name>` (Verified LLM-as-Optimizer, Slice 3+)
The proposed command compares a reference function and a candidate with matching signatures and capabilities. Authorize source replacement only for a supported callable subset with a theorem connecting both source bodies to the compared executions. Two backward compilation refinements cannot be inverted to obtain source equivalence.

Bind corresponding traps to the reference source or restrict the first version to trap-free functions. Keep the extracted-model route subject to R-0495 approval. Any supplied Lean proof script must pass the R-0509/R-0702 hostile-input audit as well as kernel checking. Check the declared resource metric independently before substitution.

### Tier C — Joint Backend Solver + Native LCB Validator (Post-`LP0` / Phase E)
An untrusted CP-SAT or ILP solver proposes instruction selection, registers and scheduling. For native schedules, validate the exact instructions, ABI, clobbers, spills, relocations and linked bytes against the selected target. The pinned LCB uses different assembly paths on Linux and macOS. Replay a bounded, canonically decoded witness bound to the exact inputs, route, profile and output. Report proof size, replay time and measured speedup. LLVM-relative speed and proof size are experiment results to collect.

---

## Why This Will Work

- **Proven precedent:** CompCert's register-allocation validator (Rideau & Leroy), Necula's translation validation, `egg`/Cranelift e-graphs, and Unison's CP-SAT joint backend are each individually battle-tested. Concrete's advantage is **unifying all of them inside one Lean 4 kernel-checked pipeline** where BIR has zero UB and LCB already has formal Lean semantics (`LeanCompilerBackend.Verified.Source`).
- **Tiny TCB:** External solvers, e-graphs, MAP-Elites, and LLMs have **zero** trust privilege—every candidate passes through a kernel-checked Lean validator before entering the artifact.
- **Reproducible builds (`R-0577`):** Nondeterministic search runs offline or produces a committed/cached witness file; release builds must bind the witness to the build inputs and replay the deterministic validator. R-0577 compares the resulting artifacts across independent builds.

## Slice Staging & Acceptance Gates

| Increment | Slice | Deliverable | Gate |
|---|---|---|---|
| **1. Multi-obligation indexed BIR validator** | B0b | Acyclic callable-function validator with header invariants, exact trap occurrences and per-function IR slot counts | Accept a nonidentity rewrite. Reject a wrong constant, swapped trap, corrupted uncalled helper and growth of a smaller frame. Measure replay. |
| **2. CFG/cut-point validator + source replacement** | Slices 3–5 | Local verification conditions, progress rank and a source correspondence theorem for the supported subset | Refuse an injected silent cycle and mismatched origins. Check composition through `Pipeline.endToEnd` or `registeredLCBEndToEnd` under its premises. |
| **3. Joint backend solver + LCB validator** | Post-`LP0` / Phase E | External CP-SAT/e-graph solver adapter + proved LCB symbolic regalloc/schedule validator feeding `R-0517` & `R-0727` | Proved LCB validator soundness, cross-backend differential parity, benchmark speedup evidence (`R-0424`) |

**Spec impact:** The validator and wire format are compiler tooling. Extracted-model replacement requires the R-0495 decision. Native validation follows LP0 under R-0299 and R-0517.
