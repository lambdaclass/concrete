# Compiler Pipeline Lessons From Other Languages

Date: 2026-07-09

This note records the pipeline ideas discussed during the Phase 6.5 refactor,
especially the comparison against Zig, Odin, Gleam, Rust, MLIR, Hylo, Austral,
Koka/Lean Perceus, Vale, and Roc.

It is not a second roadmap. The roadmap stays linear. This document is the
research backing for the relevant roadmap items, especially Phase 6.5
(`Compiler Pipeline Refactor And Invariant Hardening`), Phase 9 (proof
automation), Phase 14 (compiler soundness bridge), Phase 19 (editor/tooling),
and Phase 20 (research-gated extensions).

## Sources And Verification Status

This note is a design synthesis from the Phase 6.5 pipeline discussion. It is
not itself a completed external survey. When a claim from this note becomes a
language guarantee, roadmap item, or public comparison, it must be backed by a
checked source pass and recorded with provenance.

Source targets for the follow-up pass:

- Hylo / Val: [Hylo](https://www.hylo-lang.org/) language documentation and
  the Mutable Value Semantics paper
  ([arXiv:2106.12678](https://arxiv.org/abs/2106.12678)).
- Vale: [Vale](https://vale.dev/) documentation and Verdagon design notes on
  generational references, regions, and borrowing.
- Zig: [Zig language reference](https://ziglang.org/documentation/master/),
  release notes, compiler intern-pool design notes, and result-location
  semantics documentation.
- Rust: [rustc query system](https://rustc-dev-guide.rust-lang.org/query.html),
  incremental compilation, red/green dependency tracking, and diagnostic
  infrastructure.
- MLIR: [MLIR pass management](https://mlir.llvm.org/docs/PassManagement/),
  verifier rules, analysis invalidation, pass instrumentation, reproducer docs,
  and the MLIR paper ([arXiv:2002.11054](https://arxiv.org/abs/2002.11054)).
- Verified/certifying compiler peer group:
  [Cogent](https://ts.data61.csiro.au/projects/TS/cogent.pml),
  [CompCert](https://compcert.org/), [CakeML](https://cakeml.org/),
  [F*](https://www.fstar-lang.org/), [Alive2](https://alive2.llvm.org/), and
  [Austral](https://austral-lang.org/).
- [Roc](https://www.roc-lang.org/), Mojo, [Koka](https://koka-lang.github.io/),
  and Lean runtime reuse notes as lower-priority follow-up sources.

Record source counts, adversarially verified claims, and refuted claims only
after the follow-up pass exists as a checked artifact. Until then, any item
marked "research" below should be treated as a direction to validate, not a
fact Concrete depends on.

## Current Concrete Thesis

Concrete is not trying to be "a compiler with a few tests." The pipeline thesis
is stronger:

1. Each source program has exactly one meaning.
2. Each semantic fact is committed once by the owning stage.
3. Later stages read facts; they do not re-derive them independently.
4. Pass boundaries have contracts.
5. Failures produce source-linked diagnostics or replayable counterexamples.
6. Evidence classes stay visible: tests, runtime checks, solver results,
   assumptions, and Lean proofs are not collapsed into one green badge.

Phase 6.5 is the implementation phase for this thesis. `IntArith` is the first
load-bearing semantic axis: integer width, signedness, range, overflow/trap
policy, wrapping, saturation, casts, foldability, interpreter behavior, and
backend helper choice now route through one reference module. The next axes are
capabilities/effects, typed facts, intrinsics, resolved identity, and
certificate-carrying IR.

## The Named Languages: Are Their Pipelines As Good?

Short answer: not on Concrete's axis.

Zig, Odin, and Gleam are pragmatic production compilers/toolchains. They are
trying to make useful languages and strong developer workflows. They are not
trying to build a stage-contracted, proof/evidence-carrying compiler pipeline
for formally verifiable systems code.

That does not make them irrelevant. They each carry ideas worth stealing, but
Concrete should translate those ideas through its own rules: explicit authority,
linearity, no hidden Drop, no semantic darkness, source-linked evidence, and no
second truth source.

## Zig

Zig is the most architecturally interesting of Zig/Odin/Gleam for compiler
construction.

Useful ideas:

- **Named intermediate forms.** Zig's pipeline has concrete named layers such as
  AST/ZIR/Sema/AIR/backend-specific forms. The lesson for Concrete is not to
  copy those exact IRs, but to keep pass outputs named and inspectable.
- **ZIR-like cached per-file IR.** An untyped, per-file, cacheable IR before
  semantic analysis is a useful incremental-compilation pattern. Concrete
  should not implement this immediately, but the idea informs future
  `concrete check`, LSP, and proof-cache work.
- **InternPool-style central identities.** Interned names/types/values reduce
  duplicate representation and make stable keys possible. Concrete's analog is
  stable interned IDs for names, types, target facts, proof obligations, and
  report facts. The useful part to validate is the index-/interned-identity
  discipline: facts should serialize, hash, diff, and cache by stable IDs
  instead of by long-lived object identity.
- **Result Location Semantics.** Zig's destination-aware expression model is a
  useful vocabulary for Concrete's value/place/callArg/destination rules. It is
  especially relevant to linear aggregates, struct/array literals, returns,
  move-destructure, and codegen destination passing. Treat the vocabulary as
  valuable and the exact mechanism as cautionary: destination-sensitive
  lowering is precisely where Concrete must make pass contracts and tests
  stronger, not just more clever.
- **Tested documentation.** Zig compiles and tests language-reference examples.
  Concrete already plans doc-snippet gates and tests-as-docs; this confirms the
  direction.
- **Explicit allocator values.** Zig conventionally passes allocator values to
  allocating APIs. Concrete should keep `with(Alloc)` as the authority and add
  allocator identity as an explicit value, never a hidden global.

What not to copy:

- Broad `comptime` as general metaprogramming. Concrete's audit/proof story
  depends on visible behavior and small trusted boundaries.
- Lazy checking of unused code. Concrete's H12/std migration was explicitly in
  the opposite direction: all code that ships should face the checker.
- A global switch that changes source semantics by profile. Concrete's
  arithmetic policy must remain source-stable.

Roadmap slots:

- Phase 6.5 #23: deterministic pass-output hashes and replay artifacts.
- Phase 6.5 #3b / Phase 14 #13b: certificate-carrying IR and typed facts.
- Phase 7 #8g / Phase 6 #13s: allocator-as-value research.
- Phase 7 #29 / Phase 6 #13r: tests-as-docs and doc-snippet gates.
- Future note: result-location / destination-passing design note for aggregate
  construction and returns.

## Gleam

Gleam's compiler pipeline is conventional compared with Concrete's proof goals,
but its product discipline is excellent.

Useful ideas:

- **Integrated toolchain.** Compiler, build tool, package manager, formatter,
  language server, docs, and package ecosystem are one coherent user surface.
- **Clear diagnostics.** Gleam treats friendly errors as a core product
  feature, not a later layer.
- **Per-module metadata/cache direction.** Gleam's compilation model suggests
  the value of typed module metadata that dependencies can consume without
  re-checking everything.
- **Readable backend output.** Emitting readable target-language source can be a
  trust/debugging lever, though Concrete's systems/backend story is different.

Concrete translation:

- Keep `concrete check`, `concrete test`, formatter, docs, LSP, audit, and
  package facts reading one compiler fact model.
- Make diagnostic quality a pipeline contract, not just editor polish.
- Treat per-module typed metadata as the future shape for fast checking,
  package evidence, proof-cache invalidation, and LSP responsiveness.

Roadmap slots:

- Phase 6.5 #15a: pipeline diagnostic-quality contract.
- Phase 6.5 #23: pass-output replay and deterministic hashes.
- Phase 18: package/dependency evidence.
- Phase 19: editor and human tooling.

## Odin

Odin is deliberately pragmatic and conventional. It has less to teach Concrete
about evidence-oriented compiler pipelines than Rust, Zig, MLIR, Hylo, or
Austral.

Useful ideas:

- **Simple package shape.** A directory is a package; names and imports stay
  understandable.
- **Allocator/context ergonomics.** Odin's context and allocator story helps
  ordinary systems programming.
- **Vet-style tooling.** Semantic lint/vet checks are useful when they remain
  policy-visible.

What not to copy:

- Hidden context as authority. Concrete capabilities must stay visible in
  function headers and reports.
- Tooling warnings that can be ignored indefinitely. Concrete's strict profiles
  should either fix, explicitly allow, or fail.

Roadmap slots:

- Phase 6 #21: `concrete lint` / `concrete vet`.
- Phase 7 allocator research, but only with explicit allocator values and
  visible `with(Alloc)` authority.

## Rust

Rust is the strongest comparison for compiler pipeline architecture among
mainstream systems languages.

Useful ideas:

- **Demand-driven query system.** Compiler facts are function-like queries,
  memoized and dependency-tracked. This is more mature than a pass-only model.
- **Incremental dependency graph.** Rust's red/green tracking is a concrete
  model for deciding what changed and what can be reused safely.
- **Stable identities across sessions.** Incremental compilation requires keys
  that survive unrelated edits.
- **HIR/MIR layering.** Rust's intermediate forms make type/borrow/lowering
  boundaries explicit.

Concrete translation:

- Do not jump to a query engine during Phase 6.5. The language and IR are still
  moving.
- Design facts and pass artifacts so a later query engine is possible:
  deterministic hashes, stable IDs, explicit dependencies, schema versions, and
  replay commands.
- A future `FactGraph` should model dependencies among parse, resolve, type,
  capability, ownership, mono, CoreCheck, proof obligation, report, and codegen
  facts.

Roadmap slots:

- Phase 6.5 #23: replay artifacts and deterministic pass-output hashes.
- Phase 19: editor/LSP responsiveness.
- Phase 18/11: proof and package evidence cache invalidation.
- Potential future item: stable fact-query dependency graph.

## MLIR

MLIR is highly relevant to Phase 6.5 because it treats pass infrastructure as a
first-class system.

Useful ideas:

- **Pass invariants.** Passes state what operations they may inspect or mutate.
- **Nested pass managers.** Passes can be scoped to the IR level they own.
- **Analysis preservation/invalidation.** Analyses are cached and invalidated by
  pass behavior.
- **Pass failure protocol.** Broken invariants stop the pipeline instead of
  leaking into later stages.
- **Crash/failure reproducers.** Failure reproduction is part of the pass
  infrastructure.
- **Dialects with verifiers.** Each IR layer can carry its own verifier.
- **Verifier placement.** The idea to adapt is before-and-after verification:
  a pass must reject invalid input and must also prove it did not introduce
  invalid output.
- **Structural-before-semantic ordering.** Cheap global structure checks should
  fail before expensive semantic/proof obligations. This is the right ordering
  for Concrete's stage contracts, proof extraction, and report generation.

Concrete translation:

- Stage contracts should become pass contracts with owned input/output facts.
- Once Concrete caches analyses, every pass must say which facts it preserves
  and which it invalidates.
- Certificate-carrying IR is Concrete's analog of typed/verifiable dialects,
  but with evidence classes and source contracts attached.

Roadmap slots:

- Phase 6.5 #3: stage contracts.
- Phase 6.5 #3b: certificate-carrying IR.
- Phase 6.5 #21: counterexample-first pipeline debugging.
- Phase 6.5 #23: pass-output replay artifacts.
- Potential future item: analysis preservation/invalidation contract.

## Hylo

Hylo is philosophically important because of second-class references and mutable
value semantics.

Useful ideas:

- **Second-class references as a first principle.** References are not ordinary
  values that can escape freely.
- **Projection/subscript APIs instead of returned references.** This matches
  Concrete's H1 closure direction: scoped callbacks and safe projections
  replace returning references from collections.
- **Exclusivity without making references first-class.** Concrete's callable
  values and scoped collection APIs are in the same family.
- **Parameter and access conventions.** Research Hylo/Val-style conventions
  such as ordinary read-only bindings, `inout`/mutable access, consuming
  parameters, and setter-style projections as possible prior art for Concrete's
  pass-agreement and scoped-access docs. Do not copy syntax without a separate
  design pass.
- **Exclusivity law.** Mutable projection should make the owner inaccessible
  through aliases for the projection's lifetime; immutable projection permits
  concurrent reads but no mutation. This is the rule that makes scoped
  collection mutation sound without returning references.

Concrete translation:

- Keep references non-returnable in safe code.
- Keep collection access scoped: `with_value`, `with_value_mut`, `modify`,
  move-out APIs, not stored borrowed references.
- Treat result-location/destination-passing and projection/subscript rules as
  the theory that keeps this ergonomic without weakening linearity.

Roadmap slots:

- Callable values and collection scoped callbacks.
- Phase 6.5 value-flow/spec work.
- Phase 20 research note if second-class references need a formal model.

## Austral

Austral is a close design relative because it combines linear types with
capability-based security in a small language.

Useful ideas:

- **Small checker, explicit linearity.**
- **Capability security as a language feature.**
- **Manual, explicit resource handling.**

Concrete translation:

- Concrete should keep the linear default: non-Copy values are used exactly
  once.
- Capabilities should remain visible in function headers and reports.
- The next semantic-axis migration after arithmetic should be capabilities as
  one source of truth.

Roadmap slots:

- Phase 6.5 #5: capabilities/effects as one semantic fact source.
- Phase 10 audit/diff/reporting.
- Phase 18 dependency capability budgets.

## Koka And Lean Perceus

Perceus-style reuse analysis is relevant after Concrete's linear model is
stable.

Useful ideas:

- **Drop-guided reuse.** If a value is consumed exactly once, allocation can
  sometimes be turned into in-place reuse.
- **Functional but in-place.** Strong ownership information can pay performance
  dividends without changing source semantics.
- **Lean 4 already uses a related runtime strategy.** This is especially
  relevant because Concrete is implemented in Lean.

Concrete translation:

- Do not add this before correctness and evidence are stable.
- Later, use linearity/Destroy facts to identify consume-then-allocate patterns
  that can be optimized safely.
- Reports must classify reuse optimization as an optimization, not a semantic
  change.

Roadmap slots:

- Phase 20 research-gated extension.
- Potential Phase 15/17 performance work after backend contracts exist.

## Vale

Vale is useful mainly as a contrast case, with one speculative fallback idea to
keep visible.

Useful ideas:

- **Generational references and regions** show another memory-safety design
  point.
- **FFI/memory safety tradeoffs** are worth studying before broad trusted
  boundary work.
- **Dynamic checked fallback.** If Concrete eventually hits a forcing workload
  where the linear checker and second-class-reference invariant cannot prove a
  liveness/provenance fact statically, a generational-reference-style runtime
  check could discharge the obligation as `checked_dynamically`, never as
  `proved`.

Concrete translation:

- Concrete should not switch away from linearity, but Vale helps test whether
  Concrete's FFI/trusted-boundary story is too narrow.
- Do not adopt any "pure zero-cost safety" claim without proof. Region/frozen
  scopes may remove checks within a constrained section, but the broader design
  remains a runtime/static hybrid and must be reported honestly.

Roadmap slots:

- Phase 12/15 trusted boundary and FFI contracts.
- Phase 20 memory-model research.

## Roc

Roc contributes two useful ideas.

Useful ideas:

- **Platform/host split.** Program logic and host/platform authority are
  separated.
- **Opportunistic mutation / surgical linking.** Static analysis can reduce
  runtime cost and rebuild cost when the compiler owns enough facts.

Concrete translation:

- Concrete's capability headers and package evidence can express a stronger
  host/platform split than Roc's, because authority is already typed.
- Surgical linking and reuse analysis are later performance/build topics, not
  Phase 6.5 work.

Roadmap slots:

- Phase 18 package/dependency evidence.
- Phase 16 freestanding/embedded.
- Phase 20 research-gated optimization/build work.

This Roc section, and any Mojo comparison, remains prior-knowledge level until
a narrow follow-up pass verifies the specific compiler-pipeline claims. Do not
use it as release-facing positioning without that pass.

## Missing Peer Group: Verified And Certifying Compilers

The first version of this note compared mostly against pragmatic language
toolchains. Concrete's actual peer group also includes verified and certifying
compiler projects. These are more relevant to the proof/evidence pipeline than
Odin, Zig, or Gleam.

Projects to study:

- **Cogent.** A linearly typed systems language whose toolchain connects
  generated C to a formal embedding through refinement proof artifacts. This is
  one of the closest existing relatives to Concrete's "systems code plus
  evidence" thesis.
- **CompCert.** The baseline reference for proved compiler passes and verified
  compilation. Concrete should use it as prior art for pass-correctness claims,
  not as an excuse to claim the whole native toolchain is proved.
- **CakeML.** A verified compiler with a bootstrap story. Relevant to the
  no-second-truth-source and compiler-TCB dogfooding questions.
- **F*/Low*.** A practical obligation-to-SMT-to-extraction pipeline for
  low-level code. Relevant to Concrete's evidence-class ledger and proof
  automation UX.
- **Alive2.** SMT-based validation of LLVM IR optimizations/equivalence. This
  is the closest prior art for validating the EmitSSA/backend boundary where a
  differential fuzzer can find miscompiles but cannot prove absence.
- **Austral.** Linear types plus capability security in a small language.
  Austral remains a design-relative even though this research note is focused
  on compiler pipeline lessons.
- **Perceus / Lean 4 runtime.** Reuse analysis after ownership is stable; this
  is relevant to Concrete's long-term optimization story, not to v1 semantics.

Roadmap slots:

- Phase 14 compiler soundness bridge and source-semantics agreement.
- Phase 15 translation validation and backend trust boundaries.
- Phase 18 package/evidence replay.
- Phase 20 compiler self-verification research.

## New Ideas To Record

These are the ideas that were not clear enough in the roadmap before the
discussion.

### 1. Result-Location / Destination-Passing Design Note

Concrete has already built pieces of this under other names:

- value/place/callArg modes;
- H11 sub-place rules;
- linear aggregate construction;
- move destructuring;
- stage contracts around where values are consumed.

What is missing is a single design note stating how destinations flow through
expressions:

- expression produces into a destination;
- aggregate literals move fields/elements into that destination;
- returns move into the caller-visible destination;
- by-value projection of non-Copy subplaces is rejected unless it consumes the
  whole owner through a destructure;
- borrow/place contexts never consume;
- call arguments obey parameter-directed pass agreement.

Roadmap fit: Phase 6.5 value-flow / stage-contract work, or Phase 14 if it
becomes part of typed preservation.

### 2. Stable Interned IDs / Fact Interning

Concrete needs stable identities before serious incremental compilation,
package evidence, editor facts, and proof-cache invalidation.

Candidate interned facts:

- resolved names;
- type constructors and generic instantiations;
- capability sets;
- target facts;
- proof obligations;
- report/evidence fact keys;
- source spans after desugaring;
- generated names.

Rules:

- IDs must be deterministic for the same source + compiler version + target.
- IDs used in caches must be stable across unrelated edits where possible.
- Human reports show source names; internal artifacts use stable IDs.

Roadmap fit: Phase 6.5 #11, #16/#17, #23; Phase 18 package evidence; Phase 19
editor tooling.

### 3. Fingerprint-Keyed Incremental Verification Cache

Concrete already has proof fingerprints and evidence classes. The missing step
is to turn that into an incremental verification cache:

- key each obligation result by source fingerprint, typed fact fingerprint,
  dependency fact fingerprints, compiler version, target/profile, policy, and
  proof-tool version;
- reuse only when all dependencies are unchanged or proven equivalent;
- stale evidence becomes `needs_recheck`, never green;
- cache entries store replay commands and evidence class;
- an LLM-synthesized proof is never cached as evidence until Lean replays it.

This is incremental compilation reframed for Concrete's thesis: incremental
proof discharge.

Roadmap fit: Phase 9 proof cache / synthesis; Phase 11 proof-status drift;
Phase 18 package evidence; Phase 19 editor responsiveness.

### 4. Analysis Preservation / Invalidation Contracts

Once Concrete caches analyses or query results, every pass needs a contract:

- which facts it reads;
- which facts it writes;
- which analyses it preserves;
- which analyses it invalidates;
- which stage contract justifies not recomputing a fact.

This is the MLIR/rustc lesson. Without it, caching becomes another second truth
source.

Roadmap fit: after Phase 6.5 #3/#4, before serious incremental/LSP caching.

### 5. Query/Facts Dependency Graph

Longer term, the compiler should be able to explain fact dependencies:

```text
source file
  -> parsed AST
  -> resolved identity
  -> typed program
  -> ownership facts
  -> capability facts
  -> monomorphized Core
  -> CoreCheck facts
  -> obligations
  -> report/audit facts
  -> backend output
```

This does not need to replace the batch pipeline immediately. First, record the
edges as trace/replay metadata. Later, those edges can become a demand-driven
query engine.

Roadmap fit: Phase 6.5 #20/#23, Phase 19 editor tooling.

## What Concrete Should Not Adopt

- Lazy checking of unused code.
- Broad implicit metaprogramming.
- Hidden allocator/context authority.
- Implicit Drop.
- Warning culture where important findings can pile up ignored.
- One green badge that hides evidence classes.
- Runtime/profile switches that change the meaning of source expressions.
- Cache reuse without explicit dependency facts and stale-evidence handling.

## Priority If This Becomes Work

1. Finish Phase 6.5's current path: capability fact source, certificate-carrying
   IR, no hidden second pipeline, typed evidence ledger.
2. Add a result-location / destination-passing design note only if aggregate
   move/destination bugs recur or Phase 14 needs the formalization.
3. Add stable interned IDs before package evidence, editor facts, or proof-cache
   invalidation become broad.
4. Add fingerprint-keyed incremental verification cache after proof replay and
   synthesis are usable enough to benefit from it.
5. Add analysis preservation/invalidation contracts when cached analyses exist.
