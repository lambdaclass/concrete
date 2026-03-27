# Artifact-Driven Compiler Architecture

Status: exploratory

This note covers the biggest architectural multiplier still missing from Concrete's compiler story:

- make artifact boundaries real, not just nominal
- give those artifacts stable identity and serialization
- preserve source-to-Core-to-SSA traceability
- split interface artifacts from body artifacts cleanly
- build a real compiler driver on top of those artifacts

Concrete already has the right broad pipeline shape.
The remaining gap is that the architecture is defined more clearly than it is operationalized.

## Why This Matters

Today, Concrete already has:

- explicit pipeline stages
- named artifact types in `Concrete/Pipeline.lean`
- `ValidatedCore` as a real proof boundary
- good pass isolation in the docs

But several of the biggest future goals all depend on the same missing substrate:

- incremental compilation
- package/workspace compilation
- artifact-aware test reuse
- proof/export tooling
- stronger audit/report reuse
- stable build/review evidence
- debugging/observability tied back to source meaning

That substrate is an artifact-driven compiler architecture.

## The Main Problem

At the moment, some artifact boundaries are explicit in type names but not fully enforced in pass plumbing.

Examples of the gap:

- `ResolvedProgram` exists, but later passes still mostly consume `ParsedProgram + SummaryTable`
- artifacts do not yet have stable serialized forms
- source-to-Core traceability is still mostly future work
- interface and body information are not split cleanly enough for real package/incremental workflows
- the driver layer is still thin compared to the richness of the pass pipeline

This is not a criticism of the current compiler shape.
It is the clearest next architecture multiplier.

## What "Artifact-Driven" Should Mean

A stronger compiler architecture should have:

1. each pass consumes and produces named artifacts directly
2. artifacts have stable identity rules
3. artifacts can be serialized deterministically
4. source/program identity survives across artifact boundaries
5. the compiler driver reasons over artifacts, not just source files and shell commands

## Artifact Ladder

The long-term artifact ladder should look something like:

- parsed source artifact
- summary/interface artifact
- resolved program artifact
- checked program artifact
- elaborated Core artifact
- validated Core artifact
- optional proof/export artifact
- monomorphized Core artifact
- SSA artifact
- emitted backend artifact

The important thing is not only naming them.
The important thing is that pass APIs and tooling actually operate on them.

## Stable Identity

The architecture needs stable identity rules for:

- modules
- declarations
- functions
- types
- monomorphized instances
- report subjects
- proof/export subjects

Without stable IDs, the compiler can still compile programs, but it is much harder to build:

- reusable caches
- stable reports
- reproducible evidence bundles
- traceable proofs
- debugger/inspection tools

### Good ID Properties

Stable IDs should be:

- deterministic
- independent of incidental traversal order
- stable enough across repeated builds of the same source
- precise enough to identify monomorphized and exported entities separately

## Source Traceability

Concrete should preserve traceability across the most important semantic boundaries:

- source declaration -> resolved declaration
- source function -> elaborated Core function
- source function -> validated Core function
- source function -> ProofCore export subject
- source function -> monomorphized instance(s)
- source function -> SSA origin(s)

This does not need to mean perfect fine-grained debug metadata immediately.
It does mean the architecture should stop treating traceability as optional glue.

### Why Traceability Matters

Traceability is not only for debugging.
It is also needed for:

- proof export workflows
- report credibility
- later evidence bundles
- change-to-artifact invalidation
- audit/review workflows

## Interface vs Body Artifacts

One of the most important future splits is:

- interface-bearing artifacts
- body-bearing artifacts

Right now `FileSummary` and related import artifacts still carry more body information than the long-term architecture should want.

That is understandable in the current compiler.
But the architecture should move toward:

- importable/interface-safe artifacts for package/dependency reasoning
- body artifacts only where later semantic passes actually need them

This split is foundational for:

- package model
- dependency graph correctness
- incremental compilation
- separate compilation
- cache invalidation sanity

## Serialization

Artifact serialization should not be treated as a final polish item.
It is the operational form of the artifact architecture.

Serialization needs:

- deterministic format
- versioning policy
- compatibility policy
- clear ownership of what is stable vs ephemeral

Not every artifact needs to be public or permanently stable.
But the architecture should explicitly decide:

- which artifacts are ephemeral internals
- which are cache artifacts
- which are user/tool-visible artifacts
- which are evidence/proof/report artifacts

## CheckedProgram

One obvious missing artifact is a real checked-program artifact.

Today the architecture largely describes:

- Parse
- Resolve
- Check
- Elab

But `Check` currently behaves more like validation over earlier structures than as the constructor of a reusable artifact.

A stronger architecture would probably introduce:

- `CheckedProgram`

This would make later stages cleaner and would clarify where:

- cap-polymorphic call resolution
- linearity/borrow validation
- surface-context-dependent checks

actually become durable compiler facts.

### Why This Is Still A Candidate, Not A Settled Boundary

`CheckedProgram` should not be treated as already-proven architecture.

The current compiler clearly justifies some boundaries:

- `ValidatedCore`
- `MonomorphizedProgram`
- `SSAProgram`

Those are already meaningful semantic objects with obvious downstream consumers.

`CheckedProgram` is different.
It is a strong candidate, but not yet a guaranteed final split, because `Check` still behaves partly like validation over earlier structures rather than an obviously reusable data-product pass.

So the disciplined position is:

- treat `CheckedProgram` as a promising missing boundary
- evaluate whether it really simplifies pass plumbing, caching, reports, and driver logic
- only then promote it from architecture candidate to guaranteed roadmap artifact

The real architectural commitment is not "there must be a `CheckedProgram` type."
The real commitment is:

- pass boundaries should become operationally real
- artifacts should be explicit and reusable
- missing durable boundaries should be introduced where the code proves they help

If `CheckedProgram` turns out not to be the best split, the roadmap should preserve that freedom.

## Driver Layer

The compiler also needs a stronger driver layer above the passes.

That layer should own:

- build graph construction
- package/workspace graph
- target configuration
- artifact cache lookup/store
- report generation from cached artifacts
- build manifests
- invalidation rules

Without this, the pass architecture can remain good while the operational compiler remains thinner than it should be.

### Why The Driver Matters

The driver is where:

- incremental compilation becomes real
- package compilation becomes coherent
- reports become reusable
- evidence bundles become operational

The driver should not become a second semantic pipeline.
It should be an orchestrator over the real semantic artifacts.

## What To Avoid

Concrete should avoid:

- pass-local reconstruction of information that should be artifact-owned
- implicit coupling through ad hoc tables instead of explicit artifacts
- unstable IDs that make reports and caches noisy
- serialization without version/compatibility rules
- letting the driver become a second source of semantic truth

## Recommended Order

The best order for this work is probably:

1. make pass APIs consume/produce artifact types more directly
2. introduce any obviously missing artifacts such as `CheckedProgram`
3. define stable identity rules
4. add source-to-Core traceability
5. split interface/body artifacts more cleanly
6. add deterministic serialization for selected artifacts
7. build a real driver/cache/build-graph layer on top

## Roadmap Placement

This work cuts across multiple roadmap phases:

- **Phase H**: interface/body artifact split, package/dependency graph, summary/import discipline
- **Phase J**: driver layer, serialization policy, stable cache artifacts, operational artifact ownership
- **Later proof/evidence work**: source-to-Core traceability, stable proof/export subjects, evidence bundles

It is not a separate "compiler architecture phase" only because the work is foundational to package, operational, and proof maturity rather than isolated from them.
