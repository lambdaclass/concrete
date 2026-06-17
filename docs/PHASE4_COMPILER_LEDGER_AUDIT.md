# Phase 4 (CompilerLedger Pipeline And Typed IR) — Status Audit

Status: audit
Date: 2026-06-17
Scope: ROADMAP Phase 4 items #1–#45, audited against code + gates (parallel mapping,
risk items verified by hand).

## Purpose

Phase 4's goal is to make the ordinary compiler pipeline a **typed, replayable fact
pipeline**: one `ProjectContext` loaded by every command, a `CompilerLedger` fact
store, typed pass artifacts, one diagnostic schema, source maps through every
lowering boundary, and backend/target contracts — so reports/tooling render facts
instead of recomputing them. This document maps each item to status, isolates the
genuine active-risk items (verified, not just name-matched), and recommends the
next concrete step. (Same method as `docs/PHASE3_OBLIGATION_CORE_AUDIT.md`.)

## Headline

**The Phase 4 *core* (#1–#17) is done and gated. The open tail (#18–#45) is unbuilt
advanced tooling / perf / fuzz / incrementality, and — unlike Phase 3 — none of it
is an active soundness or dual-truth-source risk.** Two items the parallel mapping
flagged as "risk" were downgraded on hand-verification (see Active-risk section).
So the honest answer to "is there a hidden risky gap?" is **no** — what remains is
feature-completeness that is legitimately workload-gateable, plus ROADMAP staleness
(many open/deferred items carry no marker).

## Status by item

Legend: DONE / PARTIAL / DEFERRED (by design, blocker documented) / OPEN (unbuilt).

### Core pipeline + diagnostics + plumbing (#1–#17) — essentially DONE

| Item | Status | Evidence |
|---|---|---|
| 1 ProjectContext (one load for all commands) | DONE | `Concrete/Project.lean`; commits a1b7d4e1, 3a35be60; `check_cli_plumbing.sh` |
| 2 CompilerLedger / ProjectFacts store | DONE | `Concrete/CompilerLedger.lean` (recordArtifact/Fact/Diagnostic/Dependency/Timing/SourceMap/ReplayCommand); 9531b3f5; `check_compiler_ledger.sh` |
| 3 Frontend pass chain → ledger artifacts | DONE | `Project.lean:391`; 96b09ece; `check_compiler_ledger.sh` |
| 4 Structured diagnostics, human+JSON from one record | DONE | `Concrete/Diagnostic.lean`; resolver/type/ownership/capability/parser families; `check_rich_diagnostics.sh` |
| 5 Explicit canonicalization pass | PARTIAL | `Pipeline.desugar` (pre-check) + `CoreCanonicalize` (post-elab) exist, but split across the boundary rather than one pass "between resolution and type checking"; no gate pins the boundary |
| 6 Multi-level IR no-fact-erasure policy | OPEN | no systematic preservation policy/gate; facts kept ad hoc (e.g. `declSpan`) |
| 7 Distinct `TypedIR` (post-check, pre-obligation) | OPEN | Elab produces Core directly; no separate typed surface IR artifact |
| 8 Attach ownership/capability facts once (not re-inferred in reports) | OPEN (not active risk — see below) | facts checked in `CoreCheck` but not recorded into the ledger; reports do **not** re-infer them today |
| 9 One diagnostic schema | DONE | same record as #4 |
| 10 Diagnostics as data (human/JSON/LSP/tests render one record) | DONE | `Diagnostic.render`/`toJson`; `check_rich_diagnostics.sh` |
| 11 Rich Elm/Gleam rendering + 9 fixtures | PARTIAL | renderer + 5/9 fixtures (parser, unknown-name, type, use-after-move, missing-capability); **missing 4**: array-bounds obligation, solver-policy rejection, vacuous contract, stale proof; gate doesn't assert the missing ones |
| 12a/b tolerant diagnostics driver (+ownership/cap) | DONE | `runFrontendDiagnostics`; b84678bf; partial-facts gate |
| 12c parser decl-level error recovery | DONE | 0d507ef0 (statement-level recovery is future) |
| 12d explicit `unknown`/`invalid` placeholder nodes | OPEN | no such Core/AST variants |
| 13a AST→Core fn-span | DONE | `CFnDef.declSpan`; `check_source_maps.sh` |
| 13b Core→SSA→symbol span | DONE | `SFnDef.declSpan`; SSA dump names source line |
| 13c1 project-code obligation source loc | DONE | `check_source_maps.sh` (`main.divide → file:4`) |
| 13c2 dependency/stdlib obligation source loc | DEFERRED | `buildFnLocMap` stamps entry-point path for all modules; stdlib origin unpinned (documented) |
| 13d expr/stmt-granularity Core spans | OPEN | "the invasive step"; only decl-level today |
| 13e extern-fn + missing-module-file spans | OPEN | bare-tuple extern fns; `resolveModules : Except String` |
| 14a/b/c1 command plumbing (prologue, arg parsing, dep unification) | DONE | 419232cc, 577e6d64, e2fb15bb |
| 14c2 reuse registry/pc/policy across audit/prove | DEFERRED | documented |
| 15a golden CLI behavior matrix | DONE | ee807028; `check_cli_contract.sh` |
| 15b extend matrix to new commands | DEFERRED | (gated by those commands existing) |
| 16a compiler-internal API boundary | DONE | 61f9d968; `check_compiler_api_boundary.sh` |
| 16b `Concrete.Project` boundary module | DONE | 4-stage refactor → 5069d35c |
| 17a backend-contract report | DONE | `--report backend-contracts`; 6de0900a; `check_backend_contracts.sh` |
| 17b backend-contract fixture matrix | DONE | 490f4aeb |
| 17c/d surface backend assumptions in bundles / tighten checks | DEFERRED | documented |

### Advanced tooling / perf / fuzz / incrementality (#18–#45) — largely OPEN

Mostly unbuilt; **all workload-gateable, none an active soundness risk.** Grouped:

- **PARTIAL / has-a-start**: #18 codegen-execution fixtures (`check_codegen_execution.sh`, ~33 pass), #23 crash-triage taxonomy (capture stages, no ICE classes), #28 determinism (`test_determinism.sh` passes; no equivalence/replay probe), #33 fuzz (`test_fuzz.sh`/`test_parser_fuzz.sh`; no resolver/ownership/minimize), #39 JSON diagnostics (`--report diagnostics-json` works; not gated per named family), #40 emit flags (`--emit-llvm/core/ssa` exist; no `--emit-ast/resolved/typed-ir/backend-ir`, no artifact manifest), #44a–e codegen miscompile fixes (DONE+locked) / #44f `check_codegen_differential.sh` (32/0).
- **OPEN (unbuilt)**: #18a interpreter structured diagnostics (Interp still `Except String`), #19 `inspect --ast/--ledger`, #20 `verify-ir` routing (`Verify.lean` exists, unrouted), #21 `--events --json`, #24 intern pool, #25 query/dependency model, #26 incremental artifact deps, #27 hidden-global-state gate, #29 schema-version rejection gate, #30–#32 perf budgets, #34 fuzz minimize/regressions, #35 per-pass invariant ledger, #37 metamorphic gate, #38 source-location-mode (config+flag+gate), #41 `clean`, #42 `audit --compiler` self-audit, #43 backend-IR-as-stable-artifact, #44 docs-drift gate, #45 Phase 4 validation artifact (`examples/compiler_pipeline_probe/` + `check_phase4_pipeline.sh`).
- **DEFERRED**: #36 unified regression corpus.

## Active-risk items (verified by hand, not name-matched)

The Phase 3 lesson — don't trust literal-name or first-pass risk claims — applied:

1. **#8 "reports re-infer ownership/capability" — DOWNGRADED, not an active risk.**
   `Report.lean` does not call `coreCheck`/`elaborate` (verified: grep empty), so no
   report re-infers these facts today. The real content of #8 is that the facts
   aren't *recorded into the ledger* for reuse — a latent centralization gap, not a
   live dual-truth-source. (Contrast Phase 3's `--report contracts`, which genuinely
   re-discharged.)
2. **#44g ref-return codegen miscompile — DOWNGRADED, unreachable from safe code.**
   `Check.lean:2479` blanket-rejects any function whose return type contains a
   reference (the H1 resolution). So the miscompiling path cannot be written in safe
   code; #44g is defense-in-depth for an unreachable case, correctly deferred (see
   the value-model memory / `VALUE_MODEL.md`). Not regression-locked, but not live.
3. **Real but mild gaps** (feature-completeness, not soundness): #11 rich-diagnostic
   fixtures (4/9 missing, gate silent on them); #29 schema-version rejection gate
   absent (a future-proofing gap, no current misread); the #44 docs-drift gate is
   absent (which is *why* ROADMAP staleness keeps recurring — see below).

**Conclusion: no Phase-3-class active risk exists in Phase 4.** The dual-truth-source
work is genuinely Phase-3-local; Phase 4's open items are unbuilt features.

## ROADMAP staleness (to reconcile)

Phase 4's text marks staged sub-items (12a–d, 13a–e) but leaves most top-level items
*unmarked*, including several that are clearly DONE (#1–#4, #9–#10, #14–#17) and many
that are OPEN with no `[OPEN]`/`[DEFERRED]` marker (#41 `clean` is even "NOT-YET" in a
test but unmarked in the ROADMAP; #38, #42, #43 reference commands that don't exist).
Reconciliation: add a status line per top-level item (done in this pass).

## Recommendation — next concrete item

Because **no active risk exists**, the choice is workload-driven / value-driven, not
risk-driven. Ranked:

1. **Reconcile the ROADMAP Phase 4 markers** (cheap, high-value — stops the recurring
   "untracked partially-done phase" trap; done alongside this audit).
2. **Then, if picking one open item**, the highest-leverage bounded candidates are:
   - **#11 — complete the 4 missing rich-diagnostic fixtures + assert them in
     `check_rich_diagnostics.sh`.** Concrete, bounded, locks a user-facing contract
     that currently can bitrot. Best "one item" pick.
   - **#44 docs-drift gate** — DONE 2026-06-17 (`scripts/tests/check_docs_drift.sh`,
     Makefile + CI). Robust grep-pinned core: present-tense docs (CLAIMS_TODAY,
     KNOWN_HOLES, the phase audits, CHANGELOG) may reference only real gates /
     modules / doc-links / stdlib files, and only real `--report` kinds. The
     ambitious Status:/Verified: + stale-prose-marker + command-honesty parts were
     deferred as not mechanically robust (false-positive generators — "concrete" is
     an adjective; a roadmap proposes future commands); the phase audits keep the
     semantic side.
   - **#29 schema-version rejection gate** — cheap future-proofing against silent
     artifact misread; low urgency (no current misread).
   Everything else (#18–#45) is genuinely workload-gateable — do not build speculatively
   (per the deferral-discipline principle).
