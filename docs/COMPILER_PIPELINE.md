# CompilerLedger Pipeline And Typed IR

Concrete needs the ordinary compiler pipeline to be as explicit as the
proof/evidence pipeline. The goal is not to add clever machinery. The goal is
to stop later commands from rediscovering facts that an earlier pass already
knows.

The central object is `CompilerLedger` / `ProjectFacts`: the typed, non-proof
fact store for the compiler. `ProjectContext` loads the project once.
Compiler passes write facts and artifacts into the ledger. Reports, editor
tooling, crash bundles, release bundles, cache keys, pass inspectors, and
backend validation render the ledger instead of recomputing facts.

`ObligationCore` remains the proof/evidence ledger. `CompilerLedger` links to
it; it does not replace it.

The target flow is:

```text
ProjectContext
  -> parse
  -> resolve
  -> canonicalize
  -> type check
  -> ownership check
  -> capability check
  -> TypedIR / CheckedIR
  -> Core
  -> obligations and audit facts
  -> interpreter / backend IR / codegen
  -> CompilerLedger artifacts and facts
```

Every command should travel through the same front door:

```text
concrete build
concrete run
concrete test
concrete audit
concrete prove
concrete inspect
concrete fmt
concrete doc
concrete clean
```

Those commands may stop at different stages, but they should not each invent
their own project loading, source discovery, diagnostics, policy loading, or
target-profile logic.

This is a product decision as much as an architecture decision. Concrete should
feel like one coherent toolchain, not a compiler plus a pile of wrapper scripts.
The lesson from languages with strong developer tooling is that `build`, `run`,
`test`, `fmt`, docs, diagnostics, and language-server facts should share the
same project model.

## Non-Goals

This phase does not introduce a new proof system, external compiler framework,
or broad optimizer. It also does not require adopting MLIR. The useful lesson
from multi-level IR systems is the discipline: keep the right facts at the
right level, and do not lower away information before all consumers have used
it.

## ProjectContext

`ProjectContext` is the single loaded view of a project. It should contain:

- source roots and selected files
- module graph
- entry points
- test targets
- policy files
- assumption files
- target profile
- build profile
- oracle manifests
- source-map roots
- toolchain identity
- command mode

All commands should receive this context. A command may request additional
derived facts, but it should not reload the project differently.

## CompilerLedger

`CompilerLedger` is the single typed fact store for ordinary compiler facts.
It should contain:

- names, modules, imports, and resolved identities
- types, layouts, target constants, and backend assumptions
- ownership, move/copy/drop, borrow, and capability facts
- diagnostics and related spans
- source maps and source-location privacy mode
- pass artifacts and artifact dependencies
- pass timings and performance counters
- emitted files and replay commands
- cache/dependency facts
- links to `ObligationCore` ids and summaries

The first API should be small and boring:

```text
recordArtifact
recordDiagnostic
recordFact
recordDependency
recordTiming
recordSourceMap
recordReplayCommand
```

The important invariant is that `build`, `test`, `audit`, `prove`, `inspect`,
`fmt`, `doc`, and release-bundle capture all read from the same project facts.
No command should construct a private fact store that can drift from the rest
of the toolchain.

## Pass Outputs

Each pass should have a named output and a short invariant.

```text
ParsedAST
ResolvedAST
CanonicalIR
TypedIR
CheckedIR
Core
BackendIR
```

The important rule is that later passes consume typed facts, not raw syntax.
For example, audit reports should not rediscover capabilities from source text
if the capability checker already produced capability facts.

Every pass artifact should record:

- artifact id
- pass name
- input artifact ids
- output artifact ids
- facts consumed
- facts produced
- diagnostics emitted
- source maps emitted or preserved
- timing
- replay command
- verifier status

## ResolvedAST

`ResolvedAST` should attach stable identities to names:

- modules
- functions
- local variables
- constants
- spec functions
- proof names
- capability names
- imported symbols

Textual lookup should mostly end here. Later passes should operate on resolved
identities and keep source spans for diagnostics.

## CanonicalIR

Canonicalization removes surface-only syntax before type checking and lowering
spread it through the compiler.

Examples:

- normalize attributes
- resolve field puns
- normalize pattern forms
- desugar `if let` / `while let` once those exist
- make early-return and fall-through edges explicit
- keep the source-span provenance for each canonical node

Canonicalization should not hide authority, allocation, trust, or runtime
failure. It should make them easier to see.

## TypedIR And CheckedIR

`TypedIR` should carry:

- expression types
- lvalue/rvalue classification
- array, struct, and enum shapes
- control-flow form
- source spans
- runtime-failure points

`CheckedIR` should add:

- ownership facts
- move/copy/drop facts
- borrow/reference facts
- capability facts
- trusted/unsafe boundary facts

Reports may render these facts. They should not re-infer them.

## Multi-Level IR Discipline

No pass should erase a fact until every downstream consumer has either used it
or copied it into a typed fact table.

Facts that must survive lowering include:

- source spans
- types
- ownership facts
- capabilities
- runtime-failure points
- target assumptions
- trusted boundaries
- policy-relevant facts

This is the compiler-pipeline equivalent of the evidence ledger discipline.

## Diagnostics As Data

Diagnostics should be structured records first and formatted text second.

Each diagnostic should carry:

```text
code
severity
source span
message
reason
help / next action
related spans
command context
machine-readable payload
```

The same record should render to:

- human terminal text
- JSON
- LSP diagnostics
- tests/snapshots
- release bundles

This prevents every command from growing its own diagnostic language.

## Source Maps

Source maps should survive:

```text
AST -> CanonicalIR
CanonicalIR -> TypedIR
TypedIR -> Core
Core -> BackendIR
BackendIR -> generated code
runtime failure -> source span
audit fact -> source span
obligation -> source span
```

This matters for normal debugging as much as proof work.

## Query And Dependency Model

Before broad incremental compilation or LSP caching, name the compiler facts:

```text
parse(file)
resolve(module)
canonicalize(module)
typecheck(function)
ownership(function)
capabilities(function)
typed_ir(function)
core(function)
obligations(function)
audit_facts(function)
codegen(function)
release_bundle(project)
```

The first implementation can recompute everything. The important part is
recording dependencies now so a later cache does not guess.

The intended shape is query-first: each compiler fact has a named key, stable
inputs, and declared dependencies. This borrows the useful discipline from
query-based compilers without committing Concrete to a caching framework before
the pass boundaries and diagnostics are stable.

## Backend Contract

The backend boundary should name:

- integer overflow profile
- division and modulo semantics
- layout and ABI
- panic/assert behavior
- optimization assumptions
- target triple and data layout
- libc/runtime assumptions
- trusted toolchain components

Source-level claims stop at this boundary unless the backend contract carries
evidence.

## Interpreter Versus Compiled

Every deterministic executable language feature should be testable with:

```text
interpreter result == compiled result
```

This is ordinary compiler validation. It is not proof, but it catches wrong-code
bugs before they become proof or audit confusion.

## Inspect Commands

Concrete should expose stable compiler views:

```text
concrete inspect --ast
concrete inspect --resolved
concrete inspect --canonical
concrete inspect --typed
concrete inspect --core
concrete inspect --backend-ir
concrete inspect --ledger
```

These outputs should be deterministic and redact local paths where needed.
`--ledger` should render the same `CompilerLedger` records consumed by reports
and release bundles.

## Compiler Self-Audit

`concrete audit --compiler` should render the compiler pipeline itself:

- passes run
- artifact ids
- diagnostics count
- source-location privacy mode
- target and toolchain identity
- solver/tool versions
- cache/dependency facts
- replay commands
- backend assumptions
- emitted files
- links to `ObligationCore`

This is the ordinary-compiler counterpart to the proof/evidence audit. It
answers: what did the compiler know, where did each fact come from, which pass
produced it, and how can the run be replayed?

## Events, Crashes, And Retained Artifacts

The pipeline should have three debugging surfaces:

- `concrete build --events --json`: structured start/finish/fail events for
  project-load, parse, resolve, canonicalize, typecheck, ownership,
  capability, obligation collection, codegen, link, and bundle capture.
- `.build/concrete-crash/<id>/`: crash bundles for internal compiler bugs,
  with command, toolchain, redacted inputs, last successful pass, diagnostics
  so far, and a replay command.
- `.build/concrete-artifacts/`: retained pass artifacts from `--keep-artifacts`
  and explicit `--emit-*` commands, with an artifact manifest.

User errors should produce diagnostics, not crash bundles. Crash bundles are
only for compiler bugs.

## Why This Phase Comes Before Usability

Phase 10 broadens everyday language use. That work will add modules, tests,
diagnostics, bytes/text/path, collections, formatting, and more examples. If
the compiler pipeline is still ad hoc when that lands, every feature will add
another special case.

The purpose of this phase is to make the compiler ready to grow.
