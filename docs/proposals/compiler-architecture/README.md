**PROPOSED RESEARCH & ARCHITECTURE AGENDA (2026-11-27); decides no language rule. Owner decisions explicitly flagged below.**

# Frontier Compiler Architecture Research for Concrete

**Scope.** This research series documents six high-leverage compiler-architecture capabilities that solve open problems in mainstream and verified compilers by exploiting invariants **Concrete already enforces**—strict linear ownership, strictly second-class references (no stored or returned references), explicit dual authority (`with(...)` plus linear resource grants), no hidden traps in accepted safe code, executable Lean 4 semantics, and an LLM-authored / audit-first development model ([docs/PHILOSOPHY.md](../../PHILOSOPHY.md)).

Each document grounds its design in the **current Lean architecture on `main`** ([Concrete/Interfaces/Pipeline.lean](../../../Concrete/Interfaces/Pipeline.lean), [Concrete/Interfaces/Contracts.lean](../../../Concrete/Interfaces/Contracts.lean), [Concrete/Contracts.lean](../../../Concrete/Contracts.lean), [Concrete/Memory.lean](../../../Concrete/Memory.lean), [Concrete/Stages.lean](../../../Concrete/Stages.lean), [Concrete/BIR.lean](../../../Concrete/BIR.lean)), explains **why it works structurally where C/C++/Rust/LLVM/CompCert struggle**, states **what (if anything) touches `ConcreteSpec`**, maps each piece to its **exact vertical slice** in [ROADMAP.md](../../../ROADMAP.md), and lists **concrete acceptance gates** and **owner decisions**.

Nothing in this folder decides any open language question or reopens any decided language rule. Every spec-affecting item (notably default struct layout freedom under `R-0502`/`R-0609` and open-module semantic boundaries under `R-0705`) remains explicitly owner-gated until recorded in [docs/decisions.md](../../decisions.md).

---

## Why Concrete Has an Unfair Structural Advantage

Mainstream systems compilers (LLVM, GCC, `rustc`) and classical verified compilers (CompCert, CakeML) face seven recurring walls. Concrete's existing language invariants remove the root cause of each wall:

| Open Compiler Problem | Why C / C++ / Rust / LLVM / CompCert Hit a Wall | Concrete's Structural Advantage | Research Doc |
|---|---|---|---|
| **UB / `poison` vs. Optimization** | Optimizers rely on `undef`, `poison`, `freeze`, TBAA, and speculative UB assumptions, causing recurring miscompilations (Alive2) or forcing verified compilers to sacrifice speed. | Second-class references + linear ownership give **exact, O(1) structural non-aliasing**; discharged admission tokens prove overflow/bounds safety with **zero UB**. | [01 — Validated Passes & UB-Free Optimization](01-validated-passes-and-ub-free-opt.md) |
| **Automatic Data-Layout Synthesis** (AoS↔SoA, hot/cold split, copy elision) | Data layout drives 5×–50× real-world speedups, but escaping field pointers, `offsetof`, and aliasing make whole-structure layout transforms unsound in C/C++/Rust. | Second-class references never escape or get stored; `Memory.lean` is the single layout authority. Internal non-FFI types can be freely laid out behind a proved `LayoutPlan`. | [02 — Parametric `LayoutPlan` Synthesis](02-layout-plan-synthesis.md) |
| **Phase Ordering & Proof-Effort Coupling** (isel / regalloc / scheduling / LLM rewrites) | Instruction selection, scheduling, and register allocation are jointly NP-hard; proving clever heuristics in Lean is brittle and expensive. | Separate **untrusted search** (CP-SAT/ILP solvers, e-graphs, MAP-Elites, LLMs) from a **tiny kernel-verified Lean validator** (`EvidenceKind.provedValidator`). | [03 — Joint Solvers & Verified LLM Optimization](03-joint-solver-and-llm-verified-opt.md) |
| **Cost-Preserving Compilation** (WCET, stack, allocation budgets) | Verified compilers prove functional equivalence only; optimizations and backend lowering silently alter stack/step/memory bounds. | No hidden runtime, explicit allocators, mandatory `#[variant]` bounds, and `CheckpointSimulation` already sit on every lowering edge; adding a `Cost` monoid preserves bounds structurally. | [04 — Cost-Preserving & Compositional Compilation](04-cost-preserving-and-compositional.md) |
| **Compositional Verified Compilation & Linking** | Shared aliased heaps and re-entrant callbacks across module/FFI boundaries make open-module verification (CompCertO / DimSum) notoriously complex. | Cross-call boundaries carry only values, disjoint scoped borrows, and explicit linear capability grants — yielding a tiny, finite footprint interface for `link` composition and `concrete sim`. | [04 — Cost-Preserving & Compositional Compilation](04-cost-preserving-and-compositional.md) |
| **Compile-Time & Proof-Brittleness Scaling** | Small body edits trigger cascading re-typechecking, re-monomorphization, and brittle proof re-runs; human review of large LLM diffs does not scale. | Rule 13 (semantic identity over spellings), LL(1) grammar, no hidden globals, and explicit per-function contracts enable canonical bodyless `InterfaceSummary` hashing (`QueryDB`) and semantic-delta `concrete diff`. | [05 — Semantic `QueryDB` & Audit Diffing](05-semantic-query-db-and-audit-diff.md) |

---

## Seven Concrete Architectural Upgrades to `Concrete/`

Auditing the live Lean modules on `main` identifies seven concrete structural upgrades that unlock the entire agenda cleanly:

1. **Pluggable `ValidatedPass` slots** in [Concrete/Interfaces/Pipeline.lean](../../../Concrete/Interfaces/Pipeline.lean) — generalize the fixed 10-stage `PassFunctions` funnel with composable `IR → IR` validated pass lists at MIR, Memory, SSA, and BIR ([doc 01](01-validated-passes-and-ub-free-opt.md#1-pluggable-validatedpass-slots-in-interfacespipeline)).
2. **Relax `PreservesOrigins` to sound provenance inclusion + elimination ledger** in [Concrete/Interfaces/Contracts.lean](../../../Concrete/Interfaces/Contracts.lean) — currently `PreservesOrigins` requires exact set equality (`target ⊆ source ∧ source ⊆ target`), which blocks dead-code elimination and constant folding ([doc 01](01-validated-passes-and-ub-free-opt.md#2-fixing-preservesorigins-before-optimizations-land)).
3. **Pass-carried `FactTable` side-channel** across `Core → MIR → Memory → SSA → BIR` — stop collapsing admission evidence to `Option Int` in [Concrete/MIR.lean](../../../Concrete/MIR.lean) so ranges, bitmasks, non-aliasing, purity, and loop bounds survive to every mid-end and backend pass ([doc 01](01-validated-passes-and-ub-free-opt.md#3-first-class-facttable-carrier-through-bir)).
4. **Parametric `LayoutPlan` + `LayoutPolicy`** in [Concrete/Memory.lean](../../../Concrete/Memory.lean) — replace hardcoded per-type layout lookup with a proved lowering parameterized over any validated `LayoutPlan` ([doc 02](02-layout-plan-synthesis.md#2-architecture-layoutpolicy--layoutplan--parametric-lowering)).
5. **Typed `OptimizationProposal` / `ObligationBundle` wire protocol + kernel-verified BIR/LCB validator** — give external solvers, e-graphs, MAP-Elites, and LLMs one sound, zero-TCB interface into `Concrete` ([doc 03](03-joint-solver-and-llm-verified-opt.md#2-the-proposal--validator-boundary)).
6. **Parametric `Cost` monoid + open-component `InteractionSemantics`** in [Concrete/Contracts.lean](../../../Concrete/Contracts.lean) — extend `Semantics` and `CheckpointSimulation` with optional resource-cost preservation and horizontal `link` composition ([doc 04](04-cost-preserving-and-compositional.md#1-cost-preserving-compilation)).
7. **Bodyless `InterfaceSummary` + content-addressed `QueryDB`** over [Concrete/Stages.lean](../../../Concrete/Stages.lean) and [Concrete/Artifact/Sha256.lean](../../../Concrete/Artifact/Sha256.lean) — decouple declaration-level checking/proof lookup from monolithic file pipelines ([doc 05](05-semantic-query-db-and-audit-diff.md#1-bodyless-interfacesummary-and-two-level-merkle-identity)).

---

## Roadmap Mapping, Slice Schedule & Spec Impact

To obey [every fact once](../../PHILOSOPHY.md#the-rules) (Rule 5), work that refines existing roadmap items (`R-0583`, `R-0299`, `R-0580`, `R-0705`, `R-0556`, `R-0149`, `R-0711`, `R-0727`, `R-0201`, `R-0723`, `R-0272`, `R-0728`, `R-0550`, `R-0509`) extends and cross-links those rows; genuinely new architectural workstreams and Stage-0 (`B0b`) engineering multipliers receive provisional rows **`R-0730`–`R-0740`** in [ROADMAP.md](../../../ROADMAP.md).

| Task (provisional) | Title | Target Slice(s) | Touches `ConcreteSpec`? | Owner-gated? | Design Doc |
|---|---|---|---|---|---|
| [R-0730](../../../ROADMAP.md#r-0730) | Pluggable `ValidatedPass` slots, `OriginRel` provenance inclusion, pass-carried `FactTable` & UB-free mid-end optimization | Skeleton in `B0b`/Slice 1; UB-free mid-end opts at Slice 3+ (extends `R-0583`, `R-0711`) | **No** (pure `Concrete/`) | No | [Doc 01](01-validated-passes-and-ub-free-opt.md) |
| [R-0731](../../../ROADMAP.md#r-0731) | Parametric `LayoutPlan` synthesis & proved memory-layout refinement (`LayoutPolicy`, packing, copy elision, AoS↔SoA) | Spec `C3` layout-freedom decision before Slice 6 (`A2c`); packing/copy-elision at Slice 6; AoS↔SoA at Slice 7+ (extends `R-0580`, `R-0502`, `R-0609`) | **Yes** (`C3` default layout freedom for non-`repr(C)` / non-`ByteSafe` types) | **Yes** (revises `R-0502` declaration-order default) | [Doc 02](02-layout-plan-synthesis.md) |
| [R-0732](../../../ROADMAP.md#r-0732) | Kernel-verified BIR/LCB refinement validator, `OptimizationProposal` protocol, untrusted joint backend solver & `concrete verify-opt` | BIR validator skeleton in `B0b`/Slice 1; `concrete verify-opt` at Slice 3+; full joint backend solver post-`LP0` / Phase E (extends `R-0299`, `R-0583`, `R-0727`) | **No** (pure `Concrete/`) | No | [Doc 03](03-joint-solver-and-llm-verified-opt.md) |
| [R-0733](../../../ROADMAP.md#r-0733) | Cost-preserving compilation contracts (`Cost` monoid on `Semantics` & `CheckpointSimulation`, through BIR and `LP0`+) | Lowering contract hook in `B0b`/Slice 1; loop/call step bounds at Slices 4–5; native stack/instruction bounds at `LP0`+ / Phase E (extends `R-0574`, `R-0508`, `R-0572`, `R-0505`) | **No new syntax** (uses Slice 4–5 step model; no source cost syntax per 2026-11-22) | No | [Doc 04](04-cost-preserving-and-compositional.md#1-cost-preserving-compilation) |
| [R-0734](../../../ROADMAP.md#r-0734) | Open-component `InteractionSemantics` & compositional verified compilation / linking | Boundary contract at Slice 8 / before `A2c` (Slice 11); separate-module verified linking in Phase E (extends `R-0705`, `R-0582`, `R-0556`, `R-0717`) | **Yes** (open-module call-boundary relation in `StageInterface` / `C6` / `C10`) | **Yes** (`R-0705` subset, representation & residual assumptions) | [Doc 04](04-cost-preserving-and-compositional.md#2-compositional-verified-compilation-and-verified-linking) |
| [R-0735](../../../ROADMAP.md#r-0735) | Bodyless canonical `InterfaceSummary`, content-addressed `QueryDB` & semantic-delta `concrete diff` backing | Interface summary & hash at Slice 8 (`R-0556`); `concrete diff` at Slice 10 (`R-0201`); full incremental cache in Phase G (`R-0149`, `R-0697`) | **No** (pure `Concrete/` & tooling) | No | [Doc 05](05-semantic-query-db-and-audit-diff.md) |
| [R-0736](../../../ROADMAP.md#r-0736) | Canonical round-trip text + binary IR format (`Core`, `MIR`, `Memory`, `BIR`) & `--emit-ir`/`--from-ir` | Stage 0 (`B0b` / Slice 1) | **No** (pure `Concrete/`) | No | [Doc 06](06-day-one-compiler-foundations.md#1-round-trip-ir-text-format) |
| [R-0737](../../../ROADMAP.md#r-0737) | Dense `Array`-indexed pass tables (`IdTable`) & modular proof-stability library (`ProofLib`) | Stage 0 (`B0b` / pre-Slice-2; extends `R-0723`, `R-0724`, `R-0550`) | **No** (pure `Concrete/`) | No | [Doc 06](06-day-one-compiler-foundations.md#2-dense-arrays-and-proof-stability) |
| [R-0738](../../../ROADMAP.md#r-0738) | Syntax/Core-directed bug minimizer (`concrete reduce`) | Stage 0 (`B0b` / Slices 0–2; extends `R-0272`) | **No** (tooling) | No | [Doc 06](06-day-one-compiler-foundations.md#3-bug-minimizer-concrete-reduce) |
| [R-0739](../../../ROADMAP.md#r-0739) | Early real-world kernel sketches + paired refused twins checked against the combined 12-slice LL(1) grammar | Stage 0 (`B0b` / pre-`A2c`; feeds `R-0728`, `R-0567`, `R-0573`) | **No** (non-normative sketches) | No | [Doc 06](06-day-one-compiler-foundations.md#4-early-kernel-sketches) |
| [R-0740](../../../ROADMAP.md#r-0740) | Live per-commit Trust & Speed Scorecard (`Premises`, stage proofs, replay time, scaling, speed vs `clang -O2`) | Stage 0 (`B0b`+; aggregates `R-0509`, `R-0550`, `R-0702`, `R-0424`) | **No** (tooling) | No | [Doc 06](06-day-one-compiler-foundations.md#5-trust-and-speed-scorecard) |

---

## Sequencing Advice: What to Do Early vs. What Waits

Following the [slice ordering rule](../../../ROADMAP.md#slice-ordering) and the owner's cap of at most two stages in parallel:

1. **Cheap structural prep now (`B0b` / Slice 1 — zero spec churn, prevents painful retrofits):**
   - `R-0730` + `R-0732` skeleton: add `ValidatedPass` slots, split `PreservesOrigins` into provenance inclusion + elimination ledger, and land the minimal Slice-0 BIR↔BIR refinement validator.
   - `R-0733` hook: parameterize `Semantics`/`CheckpointSimulation` with a default-`Unit` `Cost` monoid and prove step/frame-slot bounds on Slice 0's `MIR → Memory → BIR` edge so Slices 1–5 inherit it automatically.
2. **Pre-freeze spec decisions (Batch 2, before Slices 6–11 / `A2c`):**
   - Ask the owner to decide `R-0731` (`C3` layout freedom for non-`repr(C)`, non-`ByteSafe` aggregates) **before Slice 6** so struct layout proofs aren't locked to declaration order if optimization is desired.
   - Define `R-0734`'s open-component semantic boundary in `StageInterface`/`C6`/`C10` at **Slice 8** (modules) before Core freezes at **Slice 11 (`A2c`)**.
3. **High-payoff feature landings at their natural slices:**
   - **Slice 3:** UB-free mid-end optimizer (`R-0730` + `R-0711`) and `concrete verify-opt` (`R-0732`).
   - **Slices 4–5 + `LP0`:** full cost-preserving step/stack bounds (`R-0733`), then joint backend solver + native LCB validator (`R-0732`).
   - **Slices 6–7:** `LayoutPlan` packing, copy elision, and AoS↔SoA (`R-0731`).
   - **Slices 8–11:** `InterfaceSummary` + `QueryDB` (`R-0735`), `concrete diff` (`R-0201`), and open-module composition (`R-0734` / `R-0705`).
