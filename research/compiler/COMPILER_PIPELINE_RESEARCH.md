# Compiler Pipeline Research Notes

These notes record what we learned from reading the local source trees of
Rust, Swift, Go, Zig, Roc, Gleam, Dafny, Lean 4, F*, and OCaml while planning
Concrete's compiler pipeline.

This is not a feature wishlist. The point is to extract the parts that fit
Concrete's philosophy: small systems language, explicit authority, typed facts,
visible evidence, replayable failures, and no hidden trust expansion.

For the focused Phase 6.5 follow-up on Zig result-location semantics,
Rust/rustc queries, MLIR pass infrastructure, Hylo second-class references,
Austral linear capabilities, Perceus reuse, and incremental verification caches,
see
[research/compiler/pipeline-lessons-2026-07.md](../research/compiler/pipeline-lessons-2026-07.md).

## Repositories Checked

The comparison used local clones under `/private/tmp`:

- Rust: `/private/tmp/rust-size-20260607`
- Swift: `/private/tmp/swift-size-20260607`
- Go: `/private/tmp/go-size-20260607`
- Zig: `/private/tmp/zig-size-20260607`
- Roc: `/private/tmp/roc-size-20260607`
- Gleam: `/private/tmp/gleam-size-20260607`
- Dafny: `/private/tmp/dafny-size-20260607`
- Lean 4: `/private/tmp/lean4-size-20260607`
- F*: `/private/tmp/fstar-size-20260607`
- OCaml: `/private/tmp/ocaml-size-20260607`

## The Main Lesson

Top-tier compilers are not just parser -> typechecker -> codegen.

They have named intermediate forms, explicit pass boundaries, verifiers,
diagnostics as data, stable identities, tooling hooks, reproducible crash
reports, package/interface artifacts, and regression suites for the pipeline
itself.

Concrete should copy that discipline, not their complexity.

## Rust

Useful shape:

- Many named compiler crates: `rustc_ast`, `rustc_hir`, `rustc_middle`,
  `rustc_mir_build`, `rustc_mir_transform`, `rustc_borrowck`,
  `rustc_incremental`, `rustc_metadata`, `rustc_interface`, and backend crates.
- Query/dependency architecture: compiler facts are named and cached by the
  compiler, rather than rediscovered by every command.
- Stable intermediate forms: AST -> HIR -> MIR -> codegen.
- Diagnostics, lints, docs, package metadata, and language server tooling are
  treated as major product surfaces.

Concrete takeaway:

- Add `ProjectContext`.
- Add named facts such as `parse(file)`, `resolve(module)`,
  `typecheck(function)`, `capabilities(function)`, `typed_ir(function)`,
  `core(function)`, `obligations(function)`, `audit_facts(function)`,
  `codegen(function)`, and `release_bundle(project)`.
- Keep a query-shaped design before implementing broad caching.
- Treat diagnostics, lint output, and proof/audit facts as shared compiler
  data, not command-specific text.

## Swift

Useful shape:

- Strong phase separation: Parse, AST, Sema, SILGen, SIL, SILOptimizer, IRGen.
- SIL has explicit stages: raw, canonical, lowered.
- Verifiers are part of the compiler culture, not an afterthought.
- API/interface stability is treated as a release concern, not a doc concern.

Concrete takeaway:

- Add `TypedIR` and clear Core/backend boundaries.
- Add `concrete verify-ir --pass parsed|resolved|typed|core|backend-ir`.
- Add public API compatibility checking: `concrete api-diff old/ new/ --json`.
- Do not move facts across pass boundaries without naming where they live.

## Go

Useful shape:

- The ordinary toolchain is the product: `go test`, `go fmt`, `go vet`, package
  loading, profiling, tracing, coverage, and JSON test output.
- Frontend and compiler internals have clear packages: scanner, parser, AST,
  types, noder, walk, escape, SSA, codegen.
- The standard library and tooling are inseparable from language usability.

Concrete takeaway:

- `concrete test --json` should be a stable event stream.
- `concrete lint` / `concrete vet` should exist for semantic warnings that are
  not type errors.
- `concrete run --profile`, `concrete test --coverage`, and
  `concrete trace --json` belong in the daily workflow, labelled as
  testing/profiling rather than proof evidence.
- Phase 11 stdlib work must happen before real workload and release claims.

## Zig

Useful shape:

- The pipeline has concrete named forms and central state: `AstGen`, `ZIR`,
  `Sema`, `AIR`, `InternPool`, `Compilation`, `Package`, backend-specific MIR.
- There is strong target/backend awareness and explicit cross-compilation
  infrastructure.
- Crash context and codegen bookkeeping checks are first-class concerns.
- The stdlib is broad, systems-oriented, and tightly integrated with build,
  package, and target work.

Concrete takeaway:

- Add canonical interned identities for names, types, literals, layouts, and
  target facts before caching/package artifacts depend on strings.
- Add crash/repro bundles under `.build/concrete-crash/<id>/`.
- Add backend contract facts: target triple/data layout, ABI/layout, overflow
  profile, division semantics, optimization assumptions, runtime assumptions.
- Keep target/OS/debug-format/compression/archive/threads/atomics/SIMD
  deferred unless a workload forces them.

## Roc

Useful shape:

- Clear product-facing phases: parse, canonicalize, check, postcheck, layout,
  lower/LIR, backend, reporting, formatter, LSP, docs, platform glue.
- Platform/target validation produces user-facing reports.
- Snapshot tests pin compiler output and diagnostics.

Concrete takeaway:

- Add canonicalization as a real pass, not ad hoc desugaring inside later
  phases.
- Add platform/target validation reports before freestanding/embedded work.
- Keep pass inspection output deterministic and snapshot-tested.
- Treat platform glue as an explicit authority/trust boundary.

## Gleam

Useful shape:

- Small language, strong product polish.
- Clear directories for parse, type checking, analysis, exhaustiveness,
  package interfaces, metadata, docs, formatter, Erlang/JS codegen, build,
  LSP, and snapshots.
- The language server reuses compiler data instead of inventing a second
  analysis.

Concrete takeaway:

- Keep the user-facing language small.
- Snapshot docs, diagnostics, formatter output, package interfaces, and LSP
  facts.
- Make `concrete doc --format json/html` a shared source for docs and editor
  tooling.
- The LSP must reuse compiler facts from the same pipeline.

## Dafny

Useful shape:

- Strong separation between resolver, rewriters, verifier, Boogie generation,
  counterexample generation, auditor, driver, and language server.
- Pipeline and verification events are visible to tools.
- Verification output has machine-readable loggers.

Concrete takeaway:

- `ObligationCore` should be the single ledger for contracts, VCs, runtime
  safety, SMT, policies, reports, proof workspaces, and release bundles.
- Add `concrete build --events --json`.
- Keep solver-backed evidence separate from kernel evidence.
- Counterexamples should map back to source variables and obligation ids.

## Lean 4

Useful shape:

- Environment, elaboration, info trees, messages, linters, compiler IR, LCNF,
  server, Lake facets, and trace classes are all structured.
- Tactics expose trace/debug classes and proof search state.
- Lake has target/facet concepts that make build outputs explicit.

Concrete takeaway:

- Use Lean's strengths: structured facts, traceable elaboration, kernel
  checking, and reusable environment data.
- Add named compiler trace/event classes for Concrete passes.
- Treat package/build artifacts as facets with typed data, not loose files.
- Keep generated proof workspaces and replay commands source-linked.

## F*

Useful shape:

- The source tree separates parser, syntax, typechecker, SMT encoding,
  extraction, tactics, interactive mode, and pretty printing.
- SMT is useful, but the encoding boundary is a major trust surface.

Concrete takeaway:

- Keep SMT encoding deliberately narrow.
- Every external solver result needs provenance, replay, policy, and a
  non-kernel evidence class unless Lean replay checks it.
- Unsupported theories should be visible as unsupported or unproven, never
  silently dropped.

## OCaml

Useful shape:

- Mature compact compiler pipeline: parsing, typing, lambda, middle-end,
  bytecode, native backends, driver, utilities.
- The module/interface model is central to separate compilation.
- The compiler is smaller than Rust/Swift/Zig because the language surface and
  backend ambitions are narrower.

Concrete takeaway:

- Compactness is a feature. Concrete should stay closer to OCaml/Gleam/Roc in
  pipeline size than Rust/Swift/Zig unless a workload forces complexity.
- Module/interface artifacts should be defined before packages and release
  claims.
- Keep the backend story narrow until evidence attachment and source maps are
  reliable.

## Concrete Pipeline Principles From The Survey

1. One project context.
   Every command should load `ProjectContext` once: roots, modules, entry
   points, tests, policies, assumptions, target profile, build profile, oracle
   manifests, source maps, and toolchain identity.

2. Named pass outputs.
   Parsed AST, resolved AST, canonical IR, typed surface IR, ownership-checked
   IR, capability-checked IR, Core, ProofCore, backend IR, and release bundle
   facts should be named artifacts.

3. Pass verifiers.
   Every IR level should have a verifier. A pass should fail close to the place
   that broke an invariant.

4. Diagnostics as data.
   Human text, JSON, LSP, snapshots, release bundles, and repair suggestions
   should render the same diagnostic records.

5. Error-tolerant partial facts.
   Parser, resolver, typechecker, ownership, and capability passes should be
   able to emit partial artifacts with explicit `invalid` / `unknown`
   placeholders for tooling. Partial facts are useful for diagnostics, docs,
   formatting, and LSP, but must never feed codegen, proof, policy, or release
   claims as complete facts.

6. Stable identities.
   Names, types, literals, layouts, target facts, obligation ids, and public API
   facts need canonical ids. Re-rendered strings are not enough for caching,
   packages, or proof artifacts.

7. Pipeline events.
   Build/test/prove/audit should be observable as structured events, not just
   logs.

8. Reproducible failures.
   Compiler bugs should produce replayable crash bundles. User errors should
   produce diagnostics, not crash bundles.

9. Backend contracts.
   Integer semantics, division, overflow, layout, ABI, panic/assert behavior,
   optimization assumptions, target triple/data layout, libc/runtime
   assumptions, and debug/source-map obligations must be explicit facts.

10. Tooling reuses compiler facts.
   Formatter, docs, LSP, package artifacts, release bundles, and audit reports
   must not duplicate semantic analysis.

11. Small core, honest deferral.
    Big compiler surfaces are justified only when a workload forces them.
    Compression/archive, broad crypto, threads, atomics, SIMD, target databases,
    full HTTP, dynamic libraries, and broad C/POSIX wrappers stay deferred
    until a checked workload makes them necessary.

## What Concrete Should Not Copy

- Rust's scale before Concrete has Rust's user base.
- Swift's ABI complexity before Concrete has public ABI commitments.
- Zig's full target and stdlib breadth before Concrete has a stable daily
  subset.
- Dafny/F*/Why3-style solver centrality as the default proof story.
- Lean/Coq proof-assistant ceremony in ordinary systems code.

## Roadmap Impact

The survey currently feeds these roadmap areas:

- Phase 4: compiler pipeline, typed IR, verifiers, events, crash bundles,
  canonical identities, diagnostics, source maps, performance, fuzzing.
- Phase 10: daily commands, tests, lint/vet, docs, trace/profile/bench, FFI UX.
- Phase 11: stdlib module plan, compatibility vectors, recipes, workloads.
- Phase 14: backend, ABI/layout, C glue, sanitizer/differential checks.
- Phase 16: release packaging, API diff, performance budgets, installation.
- Phase 17: package docs, package interfaces, dependency evidence.
- Phase 18: editor/LSP/docs/migration/playground surfaces.

The concrete implementation rule remains: every item needs a command, file,
fixture, report, or gate. Vague inspiration is not enough.
