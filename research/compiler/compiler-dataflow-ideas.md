# Compiler Dataflow Ideas

Status: open

This note records what seems genuinely useful to Concrete from:

- Burn
- Timely Dataflow / Naiad lineage
- Differential Dataflow
- Noria
- rustc query/incremental architecture
- Salsa
- rust-analyzer frontend/VFS ideas
- MLIR / IREE / TVM IR discipline
- Roc / Gleam / Mojo / Zig / Odin compiler/tooling ideas

The point is not to import "AI compiler" ideas into Concrete because AI is fashionable.
The point is to identify architectural techniques that could materially improve:

- incremental compilation
- package/workspace builds
- report generation
- evidence bundles
- IDE/query responsiveness
- compiler inspection UX

## Main Conclusion

The most useful ideas do **not** come from Burn's tensor or GPU machinery.

They come from:

- incremental derived state
- explicit dependency graphs
- reusable maintained views
- selective materialization
- progress/readiness tracking
- query-driven invalidation

Burn still contributes one useful compiler lesson:

- optional behavior should often be layered as composable services around a core engine, not entangled into every subsystem

## What Burn Adds

Burn is a tensor/ML framework, so much of it is irrelevant to Concrete.
The useful parts are architectural, not domain-specific.

### 1. Decorator architecture for optional services

Burn composes behavior by wrapping backends:

- base backend
- autodiff backend
- fusion backend
- router backend
- remote backend

For Concrete, the analogous idea is:

- one core compiler pipeline
- optional service layers around it

Examples:

- cache/artifact layer
- report layer
- evidence/provenance layer
- profiling/observability layer
- remote/shared-cache layer later

This matters because Concrete should not let:

- reports
- caches
- evidence bundles
- profiling

become tangled into the semantic core of every pass.

### 2. Explicit IR with derived metadata

Burn's IR constructors compute and carry important derived properties early.

The transfer to Concrete is:

- artifacts should carry validated metadata as early as possible
- later passes should consume explicit artifacts, not repeatedly rediscover facts

This supports:

- cacheability
- stable reporting
- narrower invalidation

### 3. Deferred/batched execution is useful in limited places

Burn records operation streams and optimizes/fuses them before forced execution.

Concrete should not copy this literally for codegen.
But the pattern is useful for:

- deferred expensive reports
- batched package/report queries
- evidence generation that should coalesce repeated requests

## What Timely / Differential / Noria Add

These systems are much closer to Concrete's future driver/artifact model.

### 1. Differential-style incremental derived state

Compilers maintain derived facts:

- imports
- symbol tables
- resolved names
- type information
- validated Core
- authority summaries
- layout summaries
- proof eligibility
- package graphs

The useful idea is:

- update those facts by change
- do not recompute everything from scratch after every edit

This is most relevant to:

- incremental compilation
- IDE responsiveness
- package/workspace workflows
- machine-readable reports

### 2. Noria-style partial materialization

Not every derived artifact should be fully stored eagerly.

The useful distinction is:

- hot, high-value artifacts should be materialized eagerly
- expensive or rarely used derived views should be materialized on demand

For Concrete, likely eager artifacts:

- parsed module summary
- interface summary
- resolved program fragment
- validated Core
- package graph
- authority summary

Likely partial/on-demand artifacts:

- deep cross-package authority traces
- full proof-eligibility detail
- expensive layout/report slices
- large evidence bundle components

This matters because Concrete wants rich reports without making every build pay full report cost.

### 3. Explicit graph artifact with reusable cached views

Concrete should eventually have a real graph artifact describing:

- packages
- modules
- imports
- artifact dependencies
- report dependencies
- evidence dependencies later

This graph should not be just an internal accident.
It should become a real driver/build artifact that can support:

- cache lookup
- invalidation
- `concrete graph`
- package/workspace orchestration
- report reuse
- trust/evidence bundles

### 4. Frontier/progress-style artifact readiness

Timely tracks progress/frontiers so the system knows what can still change.

The analogous compiler idea is:

- know when an artifact is ready enough for a consumer
- do not consume unstable derived results too early

For Concrete, useful readiness stages might include:

- parsed
- resolved
- checked
- validated
- lowered
- report-ready
- evidence-ready

This is valuable for:

- incremental builds
- IDE/report queries
- package workflows
- evidence/trust bundle assembly

## What rustc and Salsa Add

These are the closest direct compiler precedents.

### 1. Query-driven compiler architecture

Rust's query system and Salsa both push the same basic idea:

- compiler facts are queries
- queries depend on other queries
- results are cached
- invalidation is selective
- recomputation is demand-driven

This is probably the strongest practical precedent for Concrete.

### 2. Red-green invalidation

One of rustc's most useful ideas is:

- an input can change
- but a derived result may still remain equivalent

That means the system should distinguish:

- "this node was touched"
- from "this node's meaning actually changed"

This is very important for Concrete because:

- reports should not be regenerated just because trivia moved
- evidence bundles should not churn unnecessarily
- downstream package rebuilds should not invalidate just because an upstream file hash changed

This is stronger than naive source-hash invalidation.

### 3. Demand-driven report and artifact queries

Concrete already wants:

- authority reports
- proof-eligibility reports
- layout reports
- evidence bundles

The query-system lesson is:

- these should eventually behave like derived queries over explicit artifacts
- not only as ad hoc one-shot commands bolted onto a CLI

## What rust-analyzer Adds

rust-analyzer is relevant less for semantics and more for change handling.

Useful idea:

- model the frontend as a stream of file changes over a tracked workspace

Why it matters for Concrete:

- package/project builds will need cleaner invalidation than "re-scan the tree"
- later editor support will need explicit file/workspace state
- report queries should work over an updatable workspace model

This reinforces the need for:

- explicit package graph
- explicit file/module graph
- change-driven invalidation

## What MLIR / IREE / TVM Add

Concrete should not import MLIR wholesale today.
But the discipline is useful.

### 1. Explicit multi-level IR ownership

The useful idea is:

- each IR level should own a well-defined set of transformations
- lowering boundaries should be explicit
- pass pipelines should be inspectable

Concrete already has a good start here.
The remaining lesson is operational:

- reports, caches, and debug views should be tied to those boundaries cleanly

### 2. Build/compile graph discipline

Systems like IREE are good reminders that:

- compilation is a graph of transformations and artifacts
- operational orchestration should not be treated as throwaway CLI glue

That supports Concrete's existing driver/artifact direction.

## What Roc Adds

Roc's platform model is interesting because it makes the host/runtime boundary explicit.

That is relevant to Concrete because:

- stdlib vs host vs FFI boundaries matter
- package-level capability stories will eventually need explicit host/platform assumptions

This is more of a package/runtime design lesson than an incremental-compiler lesson, but it fits Concrete well.

## What Gleam Adds

The most useful Gleam-style ideas are not syntax.
They are ecosystem and target discipline.

### 1. Fine-grained target compatibility

Interesting compiler-adjacent lesson:

- compatibility/target support can be tracked more explicitly than "this whole package supports target X"

That may matter later for:

- hosted vs freestanding subsets
- high-integrity profiles
- package/platform constraints

### 2. Explicit dependency/package UX

Gleam is also a good reminder that package UX should stay boring:

- local dependencies are straightforward
- direct dependencies stay visible
- package convenience does not hide the graph

That reinforces Concrete's package direction:

- boring manifests
- explicit direct dependencies
- graph honesty over convenience magic

## What Zig Adds

Zig is one of the strongest build/toolchain references for Concrete.

### 1. Build graph as a first-class toolchain surface

The main lesson is:

- build orchestration belongs inside the toolchain, not as shell folklore

This supports Concrete's existing direction:

- one binary
- graph-shaped project workflow
- driver-owned orchestration

### 2. Incremental compilation as a tested product surface

One of Zig's most useful signals is not merely "incremental compilation exists."
It is:

- incremental compilation has dedicated test coverage and is treated as a real supported workflow

That is a strong lesson for Concrete:

- incremental compilation should not be treated as a hidden optimization
- it should have dedicated tests, bug corpus coverage, and explicit workflow expectations

## What Mojo Adds

The most relevant Mojo-like ideas are tooling-oriented.

### 1. Per-function compilation inspection

- per-function compile inspection should be easy

Concrete would benefit from:

- "show SSA for this function"
- "show emitted LLVM for this function"
- "show reports for this function"

That is useful for:

- optimization work
- auditability
- Phase H investigations

This is a tooling/inspection lesson, not a language-design lesson.

### 2. Explicit target-aware inspection

Mojo's compile-time tooling also makes the target explicit in inspection APIs.

That reinforces a useful idea for Concrete:

- inspection and reports should become more target/profile aware over time

Useful later for:

- target-aware package/report workflows
- cross-compilation diagnostics
- clearer inspection UX

## What Odin Adds

Odin is less interesting for deep compiler architecture than the others, but it is useful as a product reminder.

### 1. Tests/docs/packages are first-class product surfaces

Odin's docs emphasize:

- package docs
- examples
- test runner documentation
- source/tree discoverability

This is not a deep compiler-architecture lesson.
It is still important for Concrete:

- package/testing/doc UX should be treated as part of the language product, not only engineering cleanup

## Concrete-Specific Synthesis

The most promising architecture is:

1. explicit artifact boundaries
2. explicit package/dependency graph artifact
3. incremental derived-state updates over those artifacts
4. partial materialization of expensive reports/views
5. readiness tracking for report/evidence consumers
6. per-function/per-artifact inspection tools
7. finer-grained target/profile compatibility tracking in package/report tooling

That would let Concrete become:

- faster on repeated builds
- much stronger for IDE/query workflows
- much stronger for report generation
- much stronger for evidence bundles
- better at explaining itself to users

without changing the language surface.

## What This Is Not

This is **not** an argument for:

- turning Concrete into a general dataflow system
- adding AI/ML framework concepts to the language
- building a huge query engine immediately
- replacing the existing pipeline with something clever before the package/artifact model is stable
- importing MLIR/TVM/IREE complexity before Concrete has a clear need

The right lesson is narrower:

- use dataflow-style and query-style thinking to improve the driver/artifact system

## Best Near-Term Uses

Once the package/project model exists, the first concrete uses would be:

1. package/module graph artifact
2. cacheable module summaries
3. cacheable `ResolvedProgram` / `ValidatedCore` artifacts
4. package-aware report reuse
5. machine-readable maintained report views
6. better test/build invalidation
7. per-function inspection UX
8. finer-grained target/profile compatibility tracking in package/report tooling

These are much more realistic than trying to build a general differential compiler core immediately.

## Roadmap Fit

This work belongs mostly in:

- **Phase J**
  - package graph
  - incremental compilation
  - explicit driver/build graph
  - cache reuse

- **Phase L**
  - stable artifact identity/versioning
  - maintained machine-readable reports
  - evidence/trust bundles
  - report-first operational workflows

It does **not** belong as an immediate Phase H or language-surface task.

## Recommended First Design Moves

1. stabilize artifact boundaries first
2. make the package graph explicit
3. add incremental cache reuse on top of that
4. only then experiment with maintained derived views for reports
5. only after that consider more ambitious frontier/readiness tracking
6. add per-function inspection before adding heavy new optimizer architecture
7. treat incremental compilation as a tested workflow surface, not just a cache implementation detail

## Closest Existing Precedents

These ideas are not science fiction.
They are adjacent to real systems:

- Rust compiler query/incremental architecture
- Salsa-style incremental derived data
- Bazel/Buck2 explicit build graphs
- Timely / Differential for conceptual incremental dataflow
- Noria for partial materialization and maintained views

Concrete's opportunity is to combine these with:

- explicit authority reports
- proof eligibility
- evidence bundles
- audit-first package workflows

That combination is rarer than ordinary incremental compilation.

## Bottom Line

Burn is mostly a lesson in composable service layers and explicit IR.

Timely / Differential / Noria are much more important:

- they suggest how Concrete could eventually become an incremental,
  artifact-driven, report-maintaining compiler/build system

Rustc and Salsa are the strongest practical compiler precedents.

Zig is the strongest build/toolchain precedent.
Gleam is the strongest package/compatibility-discipline precedent.
Mojo is the strongest per-function inspection precedent.
Odin is a reminder that tests/docs/packages are product surfaces.

The likely highest-value long-term move is:

- move from "serialized artifacts plus file-hash caching"
- toward "explicit graph + incremental queries + maintained reports"

But only after the package/artifact model is made boring and explicit.
