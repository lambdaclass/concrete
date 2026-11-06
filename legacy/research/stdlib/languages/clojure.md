# Clojure Core Library Packet

Status: research

Source pointers: Clojure core, string, set, walk, data, reducers/sequence
ecosystem, Java interop conventions, atoms/refs/agents, and common data-format
libraries.

## What Clojure Has

- Persistent immutable vectors, maps, sets, and lists.
- Sequence abstraction and lazy sequences.
- Rich map/set/string/walk helpers.
- Keywords/symbols and dynamic data manipulation.
- Atoms/refs/agents and STM-like concurrency tools.
- Java interop as a core escape hatch.
- REPL-oriented workflow.
- Common ecosystem libraries for CSV, JSON/EDN, CLI, spec, test.check.

## What Concrete Should Copy

1. **Small data helper coverage.**
   Map/set/string utility functions are useful. Concrete should provide common
   helpers where they do not hide allocation or authority.

2. **Data-shape examples.**
   Clojure is good at teaching transformations over data. Concrete should copy
   that clarity for `parse -> validate -> transform -> emit` examples, not the
   dynamic data model.

3. **Property/generative testing inspiration.**
   Test generators and shrinkers are useful as oracle/testing helpers, not proof.

4. **Persistent collection awareness.**
   Persistent structures are not core, but Concrete should keep a package-later
   bucket for workloads that need immutable snapshots.

## What Concrete Should Not Copy

- Dynamic typing/reflection.
- Runtime vars/dynamic binding as hidden context.
- Lazy sequences as a default traversal model.
- Macro culture.
- STM/agents/refs as a core concurrency model.

## Missing Concrete Items This Pressures

- Property/generator test helpers in `std.test`.
- More map/set helper functions.
- Explicit package-later bucket for persistent immutable collections.
- CSV/EDN-like data-format consideration; CSV is the only small one likely to
  belong in stdlib.

## Concrete Classification

- Copy now: data helper mindset and property-test inspiration.
- Stdlib later: property/oracle helpers, CSV.
- Package later: persistent collections, EDN-like formats.
- Non-goal: dynamic/reflection/macros.

