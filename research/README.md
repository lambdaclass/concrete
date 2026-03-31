# Research Notes

Status: exploratory index

This directory contains design notes, open questions, architectural explorations, and long-horizon ideas for Concrete.

These files are exploratory unless they explicitly say otherwise. Once a design becomes a stable project rule or implementation contract, it should move into `docs/`.

## How To Use This Directory

- use `ROADMAP.md` for active project sequencing
- use `docs/` for stable rules and implementation contracts
- use `research/` for design work that is still being explored, sharpened, or staged for later phases

## Directory Layout

- `research/language/` — language-surface decisions, exclusions, and evidence-gated language ideas
- `research/stdlib-runtime/` — stdlib shape, text/string direction, collections, execution/runtime pressure, targets, and resource models
- `research/compiler/` — compiler architecture, backend direction, artifact/dataflow work, semantic diffing, and performance/backend research
- `research/proof-evidence/` — formalization, proof workflows, evidence artifacts, trust bundles, and review/report direction
- `research/packages-tooling/` — package model, testing workflow, developer tooling, authority budgets, and project/workspace UX
- `research/workloads/` — Phase H findings, comparison suites, showcase workloads, and example-driven pressure notes
- `research/process/` — quality/process notes such as testing strategy
- `research/meta/` — broader strategy, gap analysis, candidate ideas, and long-horizon synthesis

## Priority Key

- `P0` = highest-value current research, directly connected to active or next roadmap phases
- `P1` = important follow-on research, likely to matter in later phases
- `P2` = useful background, optional direction, or long-horizon exploration

## Status Key

- `Open` = still exploratory
- `Adopted` = design influenced implementation, but the note remains useful as background
- `Excluded` = intentionally not in the language surface
- `Process` = decision filter or project rule
- `Research` = broader exploration, not a current design commitment
- `Closed` = the design question is no longer open, but the note remains as historical context

## Start Here

If you want the highest-leverage current research first:

1. [workloads/phase-h-findings.md](workloads/phase-h-findings.md) — classified findings from the real-program corpus (`P0`, `Open`)
2. [proof-evidence/formalization-roi.md](proof-evidence/formalization-roi.md) — what to prove first and why (`P0`, `Open`)
3. [proof-evidence/formalization-breakdown.md](proof-evidence/formalization-breakdown.md) — the full formalization effort split into tracks and milestones (`P0`, `Open`)
4. [compiler/artifact-driven-compiler.md](compiler/artifact-driven-compiler.md) — stable artifacts, IDs, traceability, and the real compiler driver (`P0`, `Open`)
5. [packages-tooling/package-model.md](packages-tooling/package-model.md) — the eventual package/dependency model (`P0`, `Open`)
6. [packages-tooling/package-testing-tooling.md](packages-tooling/package-testing-tooling.md) — package/workspace test workflow design (`P0`, `Open`)
7. [proof-evidence/evidence-review-workflows.md](proof-evidence/evidence-review-workflows.md) — evidence bundles, machine-readable reports, and review workflows (`P0`, `Open`)
8. [compiler/semantic-diff-and-trust-drift.md](compiler/semantic-diff-and-trust-drift.md) — semantic/package/release diffing over compiler facts (`P0`, `Open`)
9. [stdlib-runtime/text-and-output-design.md](stdlib-runtime/text-and-output-design.md) — current text/output direction (`P0`, `Open`)
10. [stdlib-runtime/runtime-collections.md](stdlib-runtime/runtime-collections.md) — collection maturity for interpreter/runtime workloads (`P1`, `Open`)
11. [stdlib-runtime/iterators.md](stdlib-runtime/iterators.md) — explicit traversal support without an iterator tower (`P1`, `Open`)
12. [meta/high-leverage-systems-ideas.md](meta/high-leverage-systems-ideas.md) — recurring high-value systems ideas (`P0`, `Open`)
13. [proof-evidence/trust-multipliers.md](proof-evidence/trust-multipliers.md) — the strongest combined differentiators across proof, runtime, and evidence (`P0`, `Open`)
14. [compiler/qbe-backend.md](compiler/qbe-backend.md) — lightweight alternate-backend research (`P1`, `Open`)
15. [meta/ai-assisted-optimization.md](meta/ai-assisted-optimization.md) — optimization/refactoring loops driven by structured compiler reports (`P1`, `Research`)

## Language

- [language/builtin-vs-stdlib.md](language/builtin-vs-stdlib.md) — what belongs in compiler/runtime builtins versus the public stdlib (`P0`, `Open`, partially adopted)
- [language/capability-sandboxing.md](language/capability-sandboxing.md) — restricted authority and sandboxing direction (`P0`, `Open`, partially adopted)
- [language/high-integrity-profile.md](language/high-integrity-profile.md) — stricter profile/subset for critical code (`P0`, `Open`)
- [language/binary-format-dsl.md](language/binary-format-dsl.md) — binary parser/serializer DSL direction (`P2`, `Research`)
- [language/cleanup-ergonomics.md](language/cleanup-ergonomics.md) — reducing ownership/cleanup friction without hiding it (`P1`, `Open`)
- [language/const-generics-comptime.md](language/const-generics-comptime.md) — const generics versus comptime, and where the philosophy line should stay (`P2`, `Research`)
- [language/derived-equality-design.md](language/derived-equality-design.md) — possible derived structural equality (`P2`, `Open`)
- [language/heap-ownership-design.md](language/heap-ownership-design.md) — chosen `Heap<T>` ownership model (`P1`, `Adopted`)
- [language/heap-access-revisited.md](language/heap-access-revisited.md) — follow-up on heap access syntax and tradeoffs (`P2`, `Open`)
- [language/module-qualification.md](language/module-qualification.md) — historical note on qualified access and namespace pressure (`P1`, `Closed`)
- [language/no-closures.md](language/no-closures.md) — why Concrete excludes closures (`P1`, `Excluded`)
- [language/no-trait-objects.md](language/no-trait-objects.md) — why Concrete excludes trait objects (`P1`, `Excluded`)
- [language/pre-post-conditions.md](language/pre-post-conditions.md) — contracts/specification support and why it stays later/optional (`P1`, `Open`)
- [language/trusted-boundary.md](language/trusted-boundary.md) — explicit `trusted fn` / `trusted impl` containment design (`P1`, `Adopted`)
- [language/typestate.md](language/typestate.md) — typestate direction and limits (`P1`, `Open`)
- [language/union.md](language/union.md) — whether unions fit Concrete’s design (`P2`, `Open`)
- [language/unsafe-structure.md](language/unsafe-structure.md) — making `Unsafe` more inspectable without a heavier language (`P1`, `Open`, partially adopted)

## Stdlib And Runtime

- [stdlib-runtime/stdlib-design.md](stdlib-runtime/stdlib-design.md) — stdlib direction, priorities, and style rules (`P1`, `Open`, partially adopted)
- [stdlib-runtime/stdlib-api-cleanup.md](stdlib-runtime/stdlib-api-cleanup.md) — cleaning builtin-shaped names and ownership surprises out of the public stdlib (`P1`, `Open`)
- [stdlib-runtime/text-and-output-design.md](stdlib-runtime/text-and-output-design.md) — mixed-arg printing first, interpolation later if needed (`P0`, `Open`)
- [stdlib-runtime/runtime-collections.md](stdlib-runtime/runtime-collections.md) — collection maturity for interpreter/runtime workloads (`P1`, `Open`)
- [stdlib-runtime/iterators.md](stdlib-runtime/iterators.md) — explicit traversal support and the case against a broad iterator ecosystem (`P1`, `Open`)
- [stdlib-runtime/arena-allocation.md](stdlib-runtime/arena-allocation.md) — arena/bump allocation for parser/interpreter-style workloads (`P1`, `Open`)
- [stdlib-runtime/allocation-budgets.md](stdlib-runtime/allocation-budgets.md) — `NoAlloc`, `BoundedAlloc(N)`, and allocation reporting (`P0`, `Open`)
- [stdlib-runtime/execution-cost.md](stdlib-runtime/execution-cost.md) — structural cost reports and bounded execution direction (`P1`, `Open`)
- [stdlib-runtime/execution-cost-tracking.md](stdlib-runtime/execution-cost-tracking.md) — tracking/externalizing execution-cost obligations (`P1`, `Open`)
- [stdlib-runtime/layout-reports.md](stdlib-runtime/layout-reports.md) — layout/ABI audit report improvements (`P1`, `Open`)
- [stdlib-runtime/concurrency.md](stdlib-runtime/concurrency.md) — near-term concurrency direction (`P1`, `Open`)
- [stdlib-runtime/long-term-concurrency.md](stdlib-runtime/long-term-concurrency.md) — long-horizon layered concurrency model (`P1`, `Open`)
- [stdlib-runtime/no-std-freestanding.md](stdlib-runtime/no-std-freestanding.md) — hosted vs freestanding / `no_std` split (`P1`, `Open`)
- [stdlib-runtime/runtime-execution-pressure.md](stdlib-runtime/runtime-execution-pressure.md) — runtime/stack pressure from deep-recursive workloads (`P1`, `Open`)
- [stdlib-runtime/target-platform-policy.md](stdlib-runtime/target-platform-policy.md) — support tiers, ABI promises, and target policy (`P1`, `Open`)
- [stdlib-runtime/hardware-capability-mapping.md](stdlib-runtime/hardware-capability-mapping.md) — hardware-backed capability ideas (`P2`, `Research`)

## Compiler

- [compiler/artifact-driven-compiler.md](compiler/artifact-driven-compiler.md) — operationalizing named compiler artifacts (`P0`, `Open`)
- [compiler/compiler-dataflow-ideas.md](compiler/compiler-dataflow-ideas.md) — dataflow/query-inspired artifact and inspection ideas (`P1`, `Open`)
- [compiler/external-ll1-checker.md](compiler/external-ll1-checker.md) — external grammar + LL(1) checker (`P1`, `Open`)
- [compiler/file-summary-frontend.md](compiler/file-summary-frontend.md) — summary-based frontend direction (`P1`, `Adopted`)
- [compiler/ll1-grammar.md](compiler/ll1-grammar.md) — strict LL(1) rule and parser cleanup criteria (`P1`, `Process`)
- [compiler/mlir-backend-shape.md](compiler/mlir-backend-shape.md) — where MLIR should sit if it earns its complexity (`P1`, `Research`)
- [compiler/miri-style-interpreter.md](compiler/miri-style-interpreter.md) — interpreter/tooling direction for stronger semantic checking (`P2`, `Research`)
- [compiler/optimization-policy.md](compiler/optimization-policy.md) — optimization goals, non-goals, and observability constraints (`P1`, `Open`)
- [compiler/qbe-backend.md](compiler/qbe-backend.md) — lightweight alternate-backend research (`P1`, `Open`)
- [compiler/qbe-in-concrete.md](compiler/qbe-in-concrete.md) — long-horizon self-hosting/backend-implementation idea (`P2`, `Research`)
- [compiler/semantic-diff-and-trust-drift.md](compiler/semantic-diff-and-trust-drift.md) — semantic/trust-drift diffing over compiler facts (`P0`, `Open`)
- [compiler/semantic-diff.md](compiler/semantic-diff.md) — broader semantic diff/query ideas (`P2`, `Research`)
- [compiler/vec-inline-investigation.md](compiler/vec-inline-investigation.md) — VM/codegen inline-cliff investigation (`P1`, `Adopted`)

## Proof And Evidence

- [proof-evidence/formalization-roi.md](proof-evidence/formalization-roi.md) — best order for proving semantics and guarantees (`P0`, `Open`)
- [proof-evidence/formalization-breakdown.md](proof-evidence/formalization-breakdown.md) — full proof effort split into tracks and dependencies (`P0`, `Open`)
- [proof-evidence/proving-concrete-functions-in-lean.md](proof-evidence/proving-concrete-functions-in-lean.md) — how selected Concrete functions could be proved in Lean 4 (`P0`, `Open`)
- [proof-evidence/proof-addon-architecture.md](proof-evidence/proof-addon-architecture.md) — proof automation as an artifact-consuming addon workflow (`P0`, `Open`)
- [proof-evidence/proof-evidence-artifacts.md](proof-evidence/proof-evidence-artifacts.md) — tying reports, artifacts, proofs, and reproducibility together (`P0`, `Open`)
- [proof-evidence/evidence-review-workflows.md](proof-evidence/evidence-review-workflows.md) — evidence bundles, review workflows, and maintained reports (`P0`, `Open`)
- [proof-evidence/trust-multipliers.md](proof-evidence/trust-multipliers.md) — combined differentiators across proof, runtime, and evidence (`P0`, `Open`)
- [proof-evidence/cryptographic-source-commitments.md](proof-evidence/cryptographic-source-commitments.md) — cryptographic source/bundle provenance (`P1`, `Open`)
- [proof-evidence/proof-carrying-supply-chain.md](proof-evidence/proof-carrying-supply-chain.md) — proof/evidence-aware supply-chain direction (`P2`, `Research`)
- [proof-evidence/self-describing-binaries.md](proof-evidence/self-describing-binaries.md) — binary-to-proof/evidence traceability direction (`P2`, `Research`)

## Packages And Tooling

- [packages-tooling/package-model.md](packages-tooling/package-model.md) — package identity, dependency semantics, and workspaces (`P0`, `Open`)
- [packages-tooling/package-manager-design.md](packages-tooling/package-manager-design.md) — CLI/manifest/lockfile/graph shape (`P0`, `Open`)
- [packages-tooling/package-testing-tooling.md](packages-tooling/package-testing-tooling.md) — package-aware `concrete test` workflow (`P0`, `Open`)
- [packages-tooling/authority-budgets.md](packages-tooling/authority-budgets.md) — authority budgets and dependency policy (`P0`, `Open`)
- [packages-tooling/developer-tooling.md](packages-tooling/developer-tooling.md) — editor/LSP, debugging, and CLI workflow direction (`P1`, `Open`)
- [packages-tooling/repl-and-playground.md](packages-tooling/repl-and-playground.md) — REPL/playground direction (`P2`, `Research`)
- [packages-tooling/standalone-vs-project-ux.md](packages-tooling/standalone-vs-project-ux.md) — single-file versus project UX (`P1`, `Open`)

## Workloads And Examples

- [workloads/phase-h-findings.md](workloads/phase-h-findings.md) — classified findings from real programs (`P0`, `Open`)
- [workloads/phase-h-summary.md](workloads/phase-h-summary.md) — canonical summary of what Phase H taught (`P0`, `Open`)
- [workloads/comparative-program-suite.md](workloads/comparative-program-suite.md) — real-program comparison portfolio against Rust/Zig/C (`P1`, `Open`)
- [workloads/showcase-workloads.md](workloads/showcase-workloads.md) — serious programs Concrete should eventually implement well (`P1`, `Open`)
- [workloads/high-integrity-examples.md](workloads/high-integrity-examples.md) — example shapes for the future high-integrity profile (`P1`, `Open`)
- [workloads/adoption-strategy.md](workloads/adoption-strategy.md) — signature domains, showcases, onboarding, and public pull (`P1`, `Open`)

## Process And Quality

- [process/testing-strategy.md](process/testing-strategy.md) — gaps beyond the current suites: fuzzing, property tests, and differential testing (`P1`, `Open`)

## Meta And Long-Horizon

- [meta/high-leverage-systems-ideas.md](meta/high-leverage-systems-ideas.md) — recurring high-value systems ideas (`P0`, `Open`)
- [meta/ten-x-improvements.md](meta/ten-x-improvements.md) — the biggest long-term multipliers for Concrete (`P0`, `Open`)
- [meta/competitive-gap-analysis.md](meta/competitive-gap-analysis.md) — what other systems languages still have and which gaps matter (`P1`, `Open`)
- [meta/complete-language-system.md](meta/complete-language-system.md) — what still separates a strong compiler from a complete language system (`P1`, `Open`)
- [meta/ai-assisted-optimization.md](meta/ai-assisted-optimization.md) — optimization/refactoring guided by structured report outputs (`P1`, `Research`)
- [meta/candidate-ideas.md](meta/candidate-ideas.md) — Concrete-specific candidate language/compiler/tooling ideas (`P2`, `Research`)
- [meta/external-ideas.md](meta/external-ideas.md) — useful ideas borrowed from other languages (`P2`, `Research`)

## Placement Rule

- stable rule/reference -> `docs/`
- active plan/sequencing -> `ROADMAP.md`
- landed milestone/history -> `CHANGELOG.md`
- exploratory note -> `research/`

The roadmap should only absorb items from here when they become concrete technical work.
