# Goose Design Questions For Concrete

Status: Open — exploratory comparison, no adoption decision
Date: 2026-10-25

Goose is a systems-language reference for runtime-check visibility, compact
storage, validated indices, serialization, and construction directly into a
destination. This note asks which techniques, if any, could fit Concrete's
explicit ownership, authority, contract, and evidence model. It does not
schedule implementation, select syntax, or reopen standing language decisions.

The source snapshot is [aardappel/goose at 94cc64d](https://github.com/aardappel/goose/tree/94cc64daeabcae6f072dc1fe2b864acda5a001bc).
The observations below come from source and documentation review; benchmark
results have not been independently reproduced. Concrete's
[design policy](../../docs/project/DESIGN_POLICY.md),
[recorded decisions](../../docs/project/DECISIONS.md), and
[layout contract](../../docs/platform/LAYOUT_CONTRACT.md) remain authoritative.
The investigation is recorded under [roadmap questions](../../docs/project/ROADMAP_TASKS.md#open-research-questions-goose).

## Remaining Runtime Checks

Goose reports surviving and eliminated bounds checks with `--bce-lines` and
offers `checkfree fn` enforcement. Its analysis can use facts from callers;
a new caller can therefore make a previously accepted function fail. Extern
calls are allowed, other kinds of traps remain possible, and `--no-bce`
disables verification with a warning.

Questions to investigate:

- Could Concrete expose residual checks using existing obligation and lowering
  facts, without a second analysis that disagrees with generated code?
- At what stage would the claim hold: source, SSA, backend IR, or native code?
  How would a source check stay identifiable through specialization and lowering?
- Could an optional policy be checked from explicit contracts and declaration
  summaries, without making acceptance depend on other files' bodies?
- How would disabled analysis, indirect calls, externs, and incomplete coverage
  prevent an unsupported success claim?

A possible experiment is a bounded array kernel with a check inventory and
negative cases for changed bounds, mutation, and unknown calls. Compare
retained checks with emitted code. Keep check elimination, absence of traps,
functional correctness, and kernel-checked evidence as separate claims.

Sources: [specification section 7.14](https://github.com/aardappel/goose/blob/94cc64daeabcae6f072dc1fe2b864acda5a001bc/docs/goose_spec.md#714-checkfree-fn-no-bounds-check-left),
[bounds analysis](https://github.com/aardappel/goose/blob/94cc64daeabcae6f072dc1fe2b864acda5a001bc/src/bce.h).

## Validated Indices And Object Identity

Goose's index types identify particular grow-only global arrays. Validation
at construction can remove repeated traversal checks because the target's
length cannot decrease.

Could Concrete obtain a useful subset through typed IDs, validated containers,
and explicit contracts rather than new index-type syntax? A bounded graph or
parser table could compare plain checked indices with a validated representation.
The experiment should distinguish in-bounds access, container identity, and
continued identity of the original object. Wrong-container IDs, shrinking,
slot reuse, and generation overflow are relevant negative cases.

An ordinary newtype does not by itself establish these properties. Investigate
how invariants cross function boundaries and survive allowed mutation before
claiming that validation can safely be moved out of traversal. This question
belongs with the existing arena/index-safety work, not a new commitment to a
generic arena or first-class references.

Source: [index types and checkfree design](https://github.com/aardappel/goose/blob/94cc64daeabcae6f072dc1fe2b864acda5a001bc/docs/design/index_types.md).

## Compact Representations And Validated Images

Goose uses narrow relative references and inline variable-sized records and
enum payloads. Could explicit offsets and byte buffers with offset tables
provide useful density in a Concrete AST, graph, or immutable lookup table?
Measure total storage, traversal, construction, validation, and cleanup costs;
compactness may trade off against alignment, indexing, and mutation costs.

Variable-sized ordinary structs or enums would affect layout, ABI, indexing,
lowering, and proof semantics together. Is there a forcing workload that cannot
be served by an explicit library representation within the current layout
contract? Until that is demonstrated, the language-level representation remains
an unanswered research question.

Goose's image validator checks framing, tags, lengths, and whether relative links
land at valid element boundaries. Could a Concrete validator similarly return
an owned validated representation whose mutation rules preserve those facts?
Check malformed lengths, arithmetic overflow, links into element interiors,
wrong variants, and resource exhaustion. An in-range byte offset alone is not
a valid typed reference.

What explicit wire-format version, type identity, endianness, and application
invariants would our use case require? Goose leaves container-level versioning
and type identity outside this mechanism. Do not infer a stable file format
from Concrete's ordinary in-memory layout, or functional correctness from
successful structural validation. These questions can inform R-0444/R-0406
without deciding on a serialization builtin or binary-format DSL.

Sources: [language specification](https://github.com/aardappel/goose/blob/94cc64daeabcae6f072dc1fe2b864acda5a001bc/docs/goose_spec.md),
[serialization design](https://github.com/aardappel/goose/blob/94cc64daeabcae6f072dc1fe2b864acda5a001bc/docs/design/serialization.md).

## Construction Into The Destination

Goose propagates construction destinations through calls. Could Concrete avoid
measurable aggregate copies through narrower lowering improvements, without
new syntax or a universal copy-free guarantee?

First identify an actual generated copy and its cost in a large-record or
fixed-buffer workload. Compare generated code before and after any prototype,
including optimization-disabled builds. Check evaluation order, aliasing,
partial initialization, failure paths, and linear cleanup. An existing backend
optimization may already remove the copy; that is a valid reason to do nothing.

Source: [value construction implementation](https://github.com/aardappel/goose/blob/94cc64daeabcae6f072dc1fe2b864acda5a001bc/src/codegen_values.h).

## Benchmark Methodology

Could Goose's workload matrix improve our performance investigations? It
includes compact records, graphs, parsers, mutable caches, and flat numerical
controls, with idiomatic and optimized comparison implementations. Its results
also disclose that aggregate speed and memory figures may select different
implementations; those figures are not one implementation's resource profile.

Any Concrete comparison would need matched semantics, interfaces, inputs,
allocation strategy, safety checks, target, and toolchain, with raw samples and
replay commands. Include construction, validation, teardown, and unfavorable
workloads. Keep reserved address space distinct from committed/peak memory.
R-0416–R-0419 remain the owners of performance evidence; Goose's reported
speedups are hypotheses to investigate, not evidence about Concrete.

Sources: [benchmark design](https://github.com/aardappel/goose/blob/94cc64daeabcae6f072dc1fe2b864acda5a001bc/bench/design.md),
[results discussion](https://github.com/aardappel/goose/blob/94cc64daeabcae6f072dc1fe2b864acda5a001bc/bench/summary.md).

## Compatibility Questions And Existing Constraints

The comparison should record where an idea depends on Goose's wider model:

- Does it require compiler-assigned data stacks and automatic scope reclamation,
  or could explicit bounded storage preserve Concrete's resource discipline?
- Does it rely on nonlocal `return ... from` control flow, inferred reference
  roots, permissive mutable aliasing, or capturing function blocks? Those
  mechanisms conflict with existing explicit-failure and borrowing/closure rules.
- Does it accept a stale reference observing a replacement object in a reused
  slot? Concrete's arena/index-safety direction requires rejection, a dynamic
  check, or explicit trust instead of silent acceptance.
- Could share-nothing workers inform R-0399's concurrency comparison without
  importing implicit per-thread globals, queue authority, or failure behavior?
  Concurrency remains research-gated by its own model and pressure tests.

These are compatibility checks, not newly ratified exclusions or exceptions.
Standing policy wins where the models conflict.

## Possible Investigation Outcome

Residual-check visibility and a validated-index graph are candidate first
experiments, not selected work. For each question pursued, record the forcing
workload, baseline, measured result, contract/report shape, evidence scope,
unsupported cases, and compiler/proof complexity. A recommendation to defer,
reject, or use existing facilities is a successful research outcome. Adoption
would require a separate decision and an update to the owning roadmap task;
this note alone authorizes no language or implementation change.
