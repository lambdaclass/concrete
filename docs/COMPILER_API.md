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
| `Concrete.Pipeline` | Frontend entry + pass orchestration: `runFrontend`, `runFrontendDiagnostics`, the named artifact types, pass inspection. |
| `Concrete.CompilerLedger` | Non-proof fact store: artifacts, diagnostics-as-facts, timings, source files, dependency/obligation links. Artifact lookup + pass inspection. |
| `Concrete.ObligationCore` | Proof-obligation ledger queries (statuses, evidence classes, replay). |
| `Concrete.Diagnostic` | Structured diagnostics and their rendering (human + JSON), the one record both outputs share. |
| `Concrete.DebugBundle` | Release / debug bundle capture. |

`ProjectContext` loading is part of the V1 boundary conceptually, but currently
lives in `Main.lean` (the executable) rather than a library module; migrating it
to a boundary module is tracked under #14/#16 follow-ups. Until then, consumers
load a project by invoking the `concrete` CLI (whose contract is pinned by
`check_cli_contract.sh`, #15) — e.g. `--report compiler-ledger --json`,
`--report obligation-ledger --json`, `--diagnostics-json`.

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
