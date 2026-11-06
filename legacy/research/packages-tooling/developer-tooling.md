# Developer Tooling And Recovery

Status: exploratory

This note covers the user-facing tooling gaps that matter before a language feels broadly usable:

- semantic error recovery
- editor/LSP support
- debugger/observability expectations
- project-facing CLI workflow

Concrete already has a strong compiler architecture.
This note is about making that architecture feel usable during ordinary development.

## Why This Matters

A language can be technically strong and still feel weak if users get:

- one semantic error at a time
- no editor navigation
- no stable project commands
- weak debugging/inspection support

These are not all equally urgent, but together they define the practical development loop.

## Semantic Error Recovery

Concrete already accumulates diagnostics across functions/modules better than a pure first-error compiler, but recovery inside a single body is still limited.

The goal should not be broad permissive recovery that invents meaning.
The goal should be:

- continue far enough to surface nearby independent errors
- preserve diagnostic honesty
- keep recovery rules explicit and mechanically simple

Good near-term targets:

- recover after local expression/type failures when the surrounding statement/function can still be checked conservatively
- keep recovery bounded to avoid cascades
- make "stopped due to prior error" visible when recovery intentionally gives up

Bad direction:

- aggressive guessed recovery that produces misleading downstream diagnostics

## Editor / LSP Support

Concrete does not need a giant IDE story immediately.
It does need a minimum viable editor workflow eventually.

The likely baseline is:

- diagnostics in-editor
- go-to-definition
- hover/type information
- find references
- basic module/import navigation

This should build on real compiler artifacts rather than a separate editor-only semantic engine.

## Debugging And Observability

For a low-level language, debugging support matters.

This includes:

- debug-info quality
- stack trace usefulness
- symbol fidelity
- source-to-artifact traceability
- report/debug workflow compatibility under optimized builds

Concrete does not need to promise full debugger maturity immediately, but it should make the direction explicit.

## Project-Facing CLI

The compiler binary is not the same thing as a complete user workflow.

Concrete likely needs explicit project commands later, for example:

- `concrete build`
- `concrete test`
- `concrete run`
- report-oriented project commands

These commands should sit on top of the same package/driver/artifact model as the compiler, not become a separate tool silo.

## Cross-Compilation Workflow

Target support policy is not yet the same thing as cross-compilation workflow.

Eventually the project needs an explicit answer for:

- target triples / target naming
- standard target presets
- what it means to cross-compile a package/project
- how reports and artifacts are keyed by target
- what counts as supported vs experimental

This belongs partly to runtime/build/driver work, not only to backend codegen.

## Recommended Roadmap Placement

- **Phase F**: semantic error recovery and better diagnostics ergonomics
- **Phase H**: project-facing CLI expectations (`build/test/run`) as part of the package model
- **Phase J**: maintained editor/LSP surface, debugging/observability, cross-compilation workflow as operational tooling

## What To Avoid

- an editor engine that duplicates compiler semantics separately
- broad recovery that hides root causes
- project commands that are only shell wrappers without a real driver model
- debugger promises without traceability and artifact identity underneath
