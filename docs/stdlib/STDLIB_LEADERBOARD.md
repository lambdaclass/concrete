# Stdlib Leaderboard And Formal Library Plan

Status: planning reference

This document ranks the next standard-library modules by app-unblocking value.
It complements:

- [STDLIB.md](STDLIB.md) — stable direction and design stance.
- [STDLIB_TARGET.md](STDLIB_TARGET.md) — first-release target inventory.
- [STDLIB_DESIGN_PRINCIPLES.md](STDLIB_DESIGN_PRINCIPLES.md) — API filters.
- [STDLIB_VALIDATION_PLAN.md](STDLIB_VALIDATION_PLAN.md) — examples and
  pressure tests that validate the surface.
- [ROADMAP.md](../../ROADMAP.md) — active execution order.

The ranking is not a promise that all 100 modules belong in v1. It is a
pressure-ordering: when the validation project or a real workload asks for
stdlib work, prefer earlier rows unless there is a clear workload-specific
reason to skip ahead.

## Comparison Baseline

Concrete should not copy another language's standard library by size.

- **Gleam** is the size/coherence model for v1: small, consistent, excellent
  `Option`/`Result`/collection/text APIs.
- **Zig** is the allocator and explicit-systems model: allocator identity,
  no hidden allocation, no hidden control flow.
- **Rust** is the layering model: core/no-alloc, alloc-backed, and hosted APIs
  should stay distinguishable.
- **Ada/SPARK** is the assurance model: containers, strings, IO, big/spec
  numbers, formal containers, and lemma libraries need explicit evidence
  classes.
- **Go/Odin** are breadth references, not near-term size targets.
- **Lean** informs proof infrastructure, not ordinary app-stdlib breadth.

Concrete's near-term target is a small, coherent systems stdlib that can carry
capability, allocation, ownership, and proof/evidence facts honestly.

## Top 100 Stdlib Leaderboard

| Rank | Module | Kind | Priority |
|---:|---|---|---|
| 1 | `std.option` | existing | Harden ergonomics and docs |
| 2 | `std.result` | existing | Harden ergonomics and docs |
| 3 | `std.test` | existing | Expand tests-as-docs/oracle helpers |
| 4 | `std.bytes` | existing | Harden raw data backbone |
| 5 | `std.string` | existing | Harden owned text |
| 6 | `std.text` | existing | Harden borrowed/validated text views |
| 7 | `std.ascii` | existing | Keep small and complete |
| 8 | `std.fmt` | existing | Expand formatting surface |
| 9 | `std.parse` | existing | Expand scanner/parser core |
| 10 | `std.io` | existing | Redesign around reader/writer shape |
| 11 | `std.reader` | new | Add input abstraction |
| 12 | `std.writer` | existing | Harden output abstraction |
| 13 | `std.buffered_io` | new | Add buffering adapters |
| 14 | `std.fs` | existing | Expand handle-relative filesystem APIs |
| 15 | `std.path` | existing | Expand path and OS-string APIs |
| 16 | `std.env` | existing | Harden hosted environment access |
| 17 | `std.args` | existing | Harden CLI argument access |
| 18 | `std.cli` | new | Add flags/subcommands/usage |
| 19 | `std.vec` | existing | Harden primary growable collection |
| 20 | `std.slice` | existing | Harden borrowed contiguous views |
| 21 | `std.array` | new | Add fixed-array helpers |
| 22 | `std.map` | existing | Harden hash map |
| 23 | `std.set` | existing | Harden hash set |
| 24 | `std.ordered_map` | existing | Harden deterministic map |
| 25 | `std.ordered_set` | existing | Harden deterministic set |
| 26 | `std.deque` | existing | Harden queue/ring-like collection |
| 27 | `std.heap` | existing | Harden priority queue |
| 28 | `std.bitset` | existing | Harden dense flags/sets |
| 29 | `std.iter` | new | Add callable-values collection surface |
| 30 | `std.sort` | new | Add sorting algorithms |
| 31 | `std.search` | new | Add search/binary-search helpers |
| 32 | `std.alloc` | existing | Redesign around allocator identity |
| 33 | `std.arena` | new/research | Add if allocator-as-value admits it |
| 34 | `std.fixed_buffer` | new | Add fixed-buffer utilities |
| 35 | `std.mem` | existing | Harden size/alignment/raw helpers |
| 36 | `std.ptr` | existing | Harden unsafe/trusted pointer boundary |
| 37 | `std.numeric` | existing | Harden endian/cursor/typed numbers |
| 38 | `std.math` | existing | Harden checked/min/max/clamp helpers |
| 39 | `std.endian` | new | Split if numeric grows too broad |
| 40 | `std.hash` | existing | Harden map/set hash baseline |
| 41 | `std.checksum` | new | Add non-crypto checksums |
| 42 | `std.rand` | existing | Harden deterministic/testing RNG |
| 43 | `std.time` | existing | Harden duration/instant/clock authority |
| 44 | `std.log` | new | Add small structured diagnostics |
| 45 | `std.progress` | new | Add CLI progress/status helpers |
| 46 | `std.hex` | existing | Harden byte encoding baseline |
| 47 | `std.base64` | new | Add first real byte codec |
| 48 | `std.json` | new | Add first structured data format |
| 49 | `std.uri` | new | Add text/path/net boundary utility |
| 50 | `std.decode` | new | Add typed decoding layer |
| 51 | `std.bin` | new | Add binary reader/writer helpers |
| 52 | `std.config` | new | Add simple app config parser |
| 53 | `std.semver` | new | Add package/build version parsing |
| 54 | `std.process` | existing | Harden hosted process authority |
| 55 | `std.net` | existing | Keep narrow until workload pressure |
| 56 | `std.libc` | existing/internal | Harden trusted boundary |
| 57 | `std.os` | new | Add hosted OS abstraction if needed |
| 58 | `std.posix` | new/later | Add only as explicit hosted layer |
| 59 | `std.platform` | new | Add target/profile facts |
| 60 | `std.target` | new | Add target constants/contracts |
| 61 | `std.debug` | new | Add debug/trap helpers |
| 62 | `std.trap` | new | Add span-aware trap/debug UX |
| 63 | `std.report` | new | Add report-facing helpers if needed |
| 64 | `std.profile` | new | Add profile/evidence helper types |
| 65 | `std.contract` | new/formal | Add contract helper vocabulary |
| 66 | `std.proof` | new/formal | Add proof-facing helper vocabulary |
| 67 | `std.lemma` | new/formal | Add reusable lemma library hooks |
| 68 | `std.bigint` | new/formal | Add spec/proof big integers first |
| 69 | `std.rational` | new/formal/later | Add only if proof specs need it |
| 70 | `std.formal_vec` | new/formal | Add mathematical vector/list model |
| 71 | `std.formal_map` | new/formal | Add mathematical map model |
| 72 | `std.formal_set` | new/formal | Add mathematical set model |
| 73 | `std.ct` | new/security | Add constant-time helpers |
| 74 | `std.sha256` | existing | Harden narrow hash module |
| 75 | `std.hmac` | new/security | Add if proof/security examples need it |
| 76 | `std.crypto` | later umbrella | Keep broad crypto package-later |
| 77 | `std.compress` | later | Defer until workload pressure |
| 78 | `std.archive` | later | Defer until workload pressure |
| 79 | `std.http` | later | Keep pure parsing first |
| 80 | `std.tcp` | later/narrow | Split from net only if needed |
| 81 | `std.udp` | later/narrow | Split from net only if needed |
| 82 | `std.thread` | later/research | Defer until concurrency model exists |
| 83 | `std.atomic` | later/research | Defer until memory model hardens |
| 84 | `std.sync` | later/research | Defer until concurrency model exists |
| 85 | `std.channel` | later/research | Defer until concurrency model exists |
| 86 | `std.ffi` | new | Add FFI helper surface |
| 87 | `std.abi` | new | Add ABI/layout contract helpers |
| 88 | `std.layout` | new | Add layout introspection/contracts |
| 89 | `std.align` | new/research | Add only if alignment facts become API |
| 90 | `std.arrayvec` | new | Add fixed-capacity vector if pulled |
| 91 | `std.ring_buffer` | new | Add fixed/ring buffer if pulled |
| 92 | `std.lru` | new/later | Add only if validation workload needs it |
| 93 | `std.queue` | new/later | Prefer `deque` unless vocabulary helps |
| 94 | `std.stack` | new/later | Prefer `vec` unless vocabulary helps |
| 95 | `std.pool` | new/later | Add after allocator model |
| 96 | `std.tempfile` | new | Add with handle-relative FS APIs |
| 97 | `std.glob` | new | Add before regex |
| 98 | `std.regex` | later | Defer; simple glob first |
| 99 | `std.csv` | new | Add if #35 log/data workload needs it |
| 100 | `std.toml` | later | Defer; config scanner first |

## Formal Stdlib Modules

The `std.formal_*` family is proof-facing. These modules are not runtime
collections and should not allocate or wrap runtime containers. They provide
simple mathematical models for contracts, postconditions, loop invariants, and
Lean obligations.

### `std.formal_vec`

Purpose: a finite sequence model for specs.

It should support laws such as:

- `len(push(xs, x)) == len(xs) + 1`
- `get(push(xs, x), len(xs)) == Some(x)`
- `get(push(xs, x), i) == get(xs, i)` when `i < len(xs)`
- append/take/drop length and lookup laws

Runtime relation examples:

- `Vec<T>` implementation invariant: its runtime contents refine a
  `FormalVec<T>` model.
- `Deque<T>` implementation invariant: logical order refines a
  `FormalVec<T>` model even if storage wraps.

### `std.formal_map`

Purpose: a mathematical finite map model for specs.

It should support laws such as:

- `get(insert(m, k, v), k) == Some(v)`
- `get(insert(m, k1, v), k2) == get(m, k2)` when `k1 != k2`
- `contains(remove(m, k), k) == false`
- length changes only on new-key insert and present-key remove
- key-set behavior is explicit

Runtime relation examples:

- `HashMap<K, V>` refines a `FormalMap<K, V>` regardless of buckets, capacity,
  tombstones, hash functions, or probing strategy.
- `OrderedMap<K, V>` refines the same `FormalMap<K, V>` plus an ordering fact
  for traversal.

### `std.formal_set`

Purpose: a mathematical finite set model for specs.

It should support laws such as:

- `contains(insert(s, x), x) == true`
- `contains(remove(s, x), x) == false`
- union/intersection/difference membership laws
- subset and disjointness laws

Runtime relation examples:

- `HashSet<K>` and `OrderedSet<K>` refine the same `FormalSet<K>` while keeping
  their runtime layout/equality/comparator stories separate.

### `std.bigint` and `std.rational`

These are spec/proof tools first, runtime tools later.

SPARK's big-number lesson applies directly: annotations often need unbounded
mathematical integers even when runtime code uses bounded machine integers. In
Concrete, `std.bigint` should first exist so specifications can state facts
without overflow artifacts. Runtime big-number APIs are a separate later
decision.

`std.rational` should stay deferred until a proof workload needs exact ratios.

### `std.contract`, `std.proof`, and `std.lemma`

These modules should expose proof-facing vocabulary only after the proof UX
requires it:

- `std.contract`: common contract predicates and spec helper shapes.
- `std.proof`: evidence/proof helper declarations that are meaningful to
  `concrete prove` and reports.
- `std.lemma`: reusable theorem/lemma hooks, especially for collection and
  arithmetic models.

These modules must not become a second language or an implicit metaprogramming
system. Their contents should be pure, erased where appropriate, evidence-class
tagged, and tied to Lean/proof artifacts through reports.

## What To Update Next

Existing docs already cover most of the stdlib story, but they now need this
leaderboard threaded through them:

1. [STDLIB.md](STDLIB.md): link this document as the ranked build order.
2. [STDLIB_TARGET.md](STDLIB_TARGET.md): eventually reconcile old first-release
   missing/existing notes with current implementation state.
3. [STDLIB_DOCUMENTATION_PLAN.md](STDLIB_DOCUMENTATION_PLAN.md): update examples
   that still mention withdrawn `Option<&T>` borrowed accessors.
4. [STDLIB_VALIDATION_PLAN.md](STDLIB_VALIDATION_PLAN.md): use #35's chosen
   workload as the next validation driver.
5. [STDLIB_SURFACE_FREEZE.md](STDLIB_SURFACE_FREEZE.md): keep frozen/provisional
   status aligned as modules graduate.
6. [PROOF_SEMANTICS_BOUNDARY.md](../PROOF_SEMANTICS_BOUNDARY.md) and
   [PROOF_STORY_MATRIX.md](../PROOF_STORY_MATRIX.md): update only when formal
   modules become real proof inputs, not while they are just planned.

Do not update all of these at once just to chase consistency. Update each when
the corresponding implementation or validation workload lands, and gate the
claim with docs-drift or snippet checks where possible.
