# The Standard Library

Concrete does not need a huge standard library. It needs a strong one.

The stdlib is part of Concrete's safety and audit story. If the public library surface hides authority, ownership, allocation, cleanup, or foreign behavior, the language becomes harder to reason about even if the compiler itself is sound.

That is why the stdlib direction is:

- explicit about allocation
- explicit about ownership
- explicit about handles and resources
- bytes-first rather than string-first for low-level APIs
- coherent in naming and API shape
- small and sharp rather than broad

## How To Think About The Stdlib

The current stdlib is best understood as a low-level systems layer, not as a convenience-heavy application framework.

The main design rules are:

- keep effects visible in signatures
- keep pointer-level unsafety contained under `trusted fn` / `trusted impl`
- keep foreign boundaries explicit
- keep systems modules consistent in error and handle style
- prefer a few deeply-tested collections over a broad inconsistent zoo

The project is trying to make it easy to answer questions like:

- does this API allocate?
- does this API require a capability?
- does this code rely on `Unsafe` or `trusted` internally?
- who owns this resource?
- where does cleanup happen?

## Current Module Spine

The stdlib already has a real foundation:

- `vec`, `string`, `io`
- `bytes`, `slice`, `text`, `path`
- `fs`, `env`, `process`, `net`
- `fmt`, `hash`, `rand`, `time`, `parse`
- collections including `HashMap`, `HashSet`, `Deque`, `BinaryHeap`, `OrderedMap`, `OrderedSet`, and `BitSet`

The important point is not only that these modules exist. It is that they are trying to feel like one family.

## Bytes-First Design

One of the biggest stdlib choices is that low-level work should stay bytes-first, not string-first.

That means:

- `Bytes` is central for I/O and buffer-oriented work
- `Slice` gives borrowed contiguous views
- `Text` separates borrowed text views from owned `String`
- `Path` exists so filesystem paths do not collapse into ad hoc string handling

This keeps low-level APIs closer to what they actually do and avoids making `String` the accidental center of everything.

### `std.bytes`

`std.bytes` is the owned byte-buffer layer for:

- file I/O
- network I/O
- parsing
- formatting

It uses an explicit checked/unchecked split, so the safe and raw fast paths are both visible.

### `std.slice`

`std.slice` is the borrowed view layer:

- immutable slice views
- mutable slice views
- explicit pointer-and-length style semantics

This is important because Concrete wants borrowed views to stay explicit rather than hiding aliasing and ownership behind convenience wrappers.

### `std.text`

`std.text` separates borrowed text views from owned `String`.

That matters because borrowed text and owned heap strings are not the same thing semantically, and Concrete tries not to blur those boundaries.

### `std.path`

`std.path` gives path-specific types and operations so filesystem path handling does not become generic string manipulation by default.

That is part of the same bytes-first, explicit-surface philosophy.

## Systems Modules

The systems-facing modules should feel like one family:

- `fs`
- `env`
- `process`
- `net`
- `time`

The direction is:

- typed errors where appropriate
- explicit handles for owned resources
- visible capabilities
- no hidden runtime coupling

### `std.fs`

`std.fs` is handle-oriented rather than integer-fd-oriented in the public-facing surface.

Current direction includes:

- owned file handles
- typed errors via `FsError`
- helpers like `read_file`, `write_file`, `append_file`, and `read_to_string`
- explicit open/create/read/write/close style APIs

The goal is to make filesystem effects and ownership legible, not merely accessible.

### `std.env`

`std.env` wraps environment-variable access with a simpler typed surface.

The key point is semantic clarity:

- presence/absence is explicit
- environment access is still visible as environment access
- the module is kept small and boring

### `std.process`

`std.process` is the process-control surface:

- process IDs
- exit
- fork/spawn/wait style operations
- typed results and explicit child/process handles

This is an area where Concrete wants to stay low-level and honest, rather than pretending process control is pure or effect-free.

### `std.net`

`std.net` currently centers on TCP:

- `TcpListener`
- `TcpStream`
- explicit read/write behavior
- typed `NetError`
- owned handle semantics

The goal is networking without hidden runtime magic.

### `std.time`

`std.time` provides low-level timing operations such as:

- monotonic clock access
- sleep
- unix timestamp

This stays aligned with the same systems-module style: explicit, capability-visible, and low-level.

## Utility Modules

Some stdlib modules are pure or mostly pure building blocks rather than resource-owning systems modules.

### `std.fmt`

`std.fmt` provides formatting utilities such as:

- decimal formatting
- hex/bin/oct formatting
- boolean formatting
- width and padding helpers

This matters because Concrete wants formatting to be understandable and auditable, not dependent on a giant implicit formatting framework.

### `std.parse`

`std.parse` is the other half of that story:

- value parsing
- cursor-style parsing support
- eventual `fmt` / `parse` coherence

The ideal is that formatting and parsing are explicit and testable as a pair.

### `std.hash`

`std.hash` currently provides a small explicit hashing foundation.

### `std.rand`

`std.rand` provides deterministic seeding and bounded-range random support. The point is not to become a giant random ecosystem, but to provide a small sharp low-level surface.

## Collections

The collection direction is "few but excellent".

The current collection spine is:

- `Vec`
- `HashMap`
- `HashSet`

And the broader implemented collection family now includes:

- `Deque`
- `BinaryHeap`
- `OrderedMap`
- `OrderedSet`
- `BitSet`

The rule is that collections should earn their place by improving real low-level work while keeping ownership, allocation, and error behavior explicit.

## What Still Needs Work

The stdlib is real now, but it is still being hardened.

The main near-term stdlib work remains:

- deepen `fs`, `net`, and `process`
- keep error and handle conventions uniform
- expand failure-path and integration testing
- keep systems-module polish moving
- avoid broadening the collection surface faster than it can be kept coherent

## Where To Go Deeper

For the stable stdlib direction, read [`docs/STDLIB.md`](../../STDLIB.md).

That document goes deeper on:

- module-by-module direction
- collection priorities
- systems-module conventions
- error/handle/style rules
- what remains to deepen and harden
