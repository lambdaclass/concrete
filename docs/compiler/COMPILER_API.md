# Compiler-internal API boundary (V1)

Status: **V1 — enforced by `scripts/tests/check_compiler_api_boundary.sh`** (ROADMAP Phase 4 #16).

External consumers — editor/LSP integrations, MCP servers, package tooling, and
anything outside the compiler proper — must depend on a small, stable **boundary**
surface, not reach into compiler internals (parser, type checker, elaborator,
report/obligation reconstruction, codegen). Pinning this now, before those
consumers grow, keeps the internals free to change and gives integrators a
contract they can rely on.

This is a boundary definition + guard, not a refactor: the compiler itself
(`Main.lean`, the `Concrete.*` modules) is unrestricted. The rule applies only to
**consumer roots** (see below).

## V1 boundary modules (consumers MAY import these)

| Module | Surface |
| --- | --- |
| `Concrete.Resolve.Project` | Project loading: `findProjectRoot`, `loadProject` → `ProjectContext` (deps + frontend + policy + ledger, loaded once). The way tooling loads a project in-process. |
| `Concrete.Pipeline.Pipeline` | Frontend entry + pass orchestration: `runFrontend`, `runFrontendDiagnostics`, the named artifact types, pass inspection. |
| `Concrete.Report.CompilerLedger` | Non-proof fact store: artifacts, diagnostics-as-facts, timings, source files, dependency/obligation links. Artifact lookup + pass inspection. |
| `Concrete.Proof.ObligationCore` | Proof-obligation ledger queries (statuses, evidence classes, replay). |
| `Concrete.Report.Diagnostic` | Structured diagnostics and their rendering (human + JSON), the one record both outputs share. |
| `Concrete.Report.DebugBundle` | Release / debug bundle capture. |

Project loading is now a first-class boundary module (`Concrete.Resolve.Project`, #16b):
consumers call `findProjectRoot` + `loadProject` in-process to get a
`ProjectContext` without shelling out. The CLI contract (`check_cli_contract.sh`,
#15) remains available for out-of-process consumers — e.g.
`--report compiler-ledger --json`, `--report obligation-ledger --json`,
`--diagnostics-json`.

## Machine-readable schema: version 2 (2026-10-05)

The JSON API (`--report diagnostics-json` facts, `--query` answers, snapshots, proof bundles)
carries one `schema_version`, defined once as `apiSchemaVersion` in `Concrete/Report/Json.lean`.
Per the policy in `COMPILER_PIPELINE.md`, a removed field bumps it, and an artifact of another
version is **rejected** with a regeneration diagnostic (`concrete diff` enforces this); old
artifacts are not migrated.

**v1 → v2 (R-0484 R10).** `is_pure` is **removed** from `effects` and `capability` facts. It was
true for an empty declared capability set, which is not purity: it ignored indirect calls,
calls into code that was never loaded, foreign bindings whose honesty the conclusion rests on,
and mutation through `&mut` arguments. There is **no alias**: a field named `is_pure` with a
weaker meaning would keep the misleading claim alive. Replace it with:

| v2 field | meaning |
|---|---|
| `no_external_authority` | no declared capability, complete call-graph coverage, and no foreign binding assumed. It is NOT purity: mutation through `&mut` arguments and what trusted code does are not excluded. |
| `coverage_complete`, `unresolved_indirect_calls` | whether every call path was resolved; the gaps when not, each with a `kind`: `indirect-call` (through a fn-typed binding), `unloaded-callee` (names no analysed definition) or `type-parameter-dispatch` (a method call on a type parameter) |
| `assumed_foreign_bindings` | the foreign bindings the function may reach, with declared effects; their honesty is assumed. Each carries `descriptor_audit`: `std-audit-descriptor` (in the construction/caller audit's descriptor table), `std-audit-no-descriptor` (audited as taking none) or `none` (no audit covers it). Descriptor classification itself is not compiler-checked under encoding A. Additive. |
| `trusted_boundaries_reached` | how many trusted boundaries its memory safety rests on |
| `trusted_boundaries` | the same boundaries, named: `declaration`, `package_name`, and `absorbs` — the raw operations and foreign bindings the trusted body performs or calls (from the checker's trust edges), or `null` when that could not be determined. Additive; the version stays 2. `--report assumptions` carries the same `absorbs` on each trusted-boundary entry, and `--report unsafe` lists each reached boundary with its dependents and one path. |
| `assumptions_computed`, `dependencies_analysed` | whether the facts above were computed, and with dependencies loaded |

Extern `capability` facts report what calling the binding requires (declared effects, plus
`Unsafe` unless trusted) with `declared_effects_assumed: true`; they never claim
`no_external_authority`. The audit query's capability object renames its flag
`no_declared_capability`, which is exactly what it measures. Human-readable reports follow the
same rule: an empty declared set prints `(none)`, and only a summary-backed conclusion prints
`(no external authority)`; no report prints `(pure)`.

`eligibility` facts carry `admissible` and `admission_reasons`. This is the proof-admission
verdict, which is separate from extractability; it is read from the same assumption summary
(see `docs/verification/EFFECT_PROOF_BOUNDARIES.md` §4.2). The field is additive, so the
version stays 2.

## Off-limits to consumers (compiler internals)

Everything else under `Concrete.*` — including `Parser`, `Lexer`, `Resolve`,
`Check`, `Elab`, `CoreCheck`, `Mono`, `Lower`, `SSA`, `EmitSSA`, `EmitLLVM`,
`Report`, `Policy`, `Core`, `AST`, … — is an internal. Consumers must not import
them, and must not import the bare umbrella `Concrete` (it transitively exposes
every internal). Reach the compiler through the boundary modules above, or through
the CLI contract.

## Consumer roots (scanned by the gate)

`editor/`, `tools/`, `integrations/`, `lsp/`, `mcp/`, `plugins/`. Any `*.lean`
under these that imports a `Concrete.*` module outside the boundary allowlist
(or the bare umbrella) fails the gate.

## Changing the boundary

Add a module to the allowlist here AND in the gate's `BOUNDARY` set in the same
change — the gate asserts the two agree, so the doc cannot drift from what is
enforced.
