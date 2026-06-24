# Standard Library Design Notes

**Status:** Open, partially adopted as ordering guidance

This document records the direction for Concrete's standard library after the core compiler architecture work.

The standard library is not just "APIs we need eventually." It is one of the main ways the language proves that its design is viable for correctness-focused low-level work.

For the stable project direction, see [../docs/STDLIB.md](../docs/STDLIB.md). This note is broader and more exploratory.

## Design Rules

The stdlib should follow a small number of hard rules:

1. **Allocation must be visible.**
   If an API allocates, that fact should be visible in the signature, capability set, or returned ownership shape.

2. **Ownership must be obvious.**
   Owned resources, borrowed views, and transferred values should be easy to distinguish from the type alone.

3. **Effects must stay explicit.**
   I/O, environment access, process control, networking, and time should not be hidden behind convenience wrappers.

4. **Resource-backed APIs should avoid laziness.**
   The stdlib should not hide evaluation order, blocking, cleanup, or resource lifetime behind lazy streams or iterator pipelines.

5. **Safe-facing APIs should prefer typed structure over ambient convention.**
   Use small types and explicit enums, not sentinel values, magic integers, or “just know the convention” APIs.

6. **The stdlib should avoid baking in a runtime model too early.**
   Concurrency/runtime design should be handled by the separate concurrency research track.

7. **The stdlib should optimize for low-level usefulness, explicitness, and composability without magic.**
   Concrete should prefer APIs that are easy to audit over APIs that are merely clever.

## Stdlib Quality Bar

The stdlib should be evaluated against a concrete quality bar, not just a list of good intentions.

For any new module, type, or major API addition, ask:

1. Does ownership remain obvious from the type and signature?
2. Are semantic effects visible to callers?
3. Are allocation, blocking, and host interaction easy to audit?
4. Is the naming consistent with the rest of the stdlib?
5. Does it avoid leaking builtin/runtime-hook vocabulary into the public API?
6. Does it add real low-level value, or only surface area?
7. Does it preserve Concrete’s explicit style better than the alternatives?

If the answer to several of those is “no,” the API should not land yet.

## Module Standards

Every stable stdlib module should aim to satisfy the same baseline standards.

### 1. Surface clarity

- public names should be user-facing, not compiler-hook-shaped
- effects should be visible in signatures
- ownership and borrowing should be obvious
- checked and unchecked operations should be named honestly

### 2. Error design

- use `Result<T, ModuleError>` where failure is expected
- keep module-local error enums small and typed
- do not expose sentinel integers or null-like conventions in safe APIs

### 3. Handle/resource design

- use owned handle types for files, sockets, listeners, child processes, etc.
- use borrowed handle/view types only when they add real value
- no raw fd/socket integers in safe-facing APIs

### 4. Trust/effect honesty

- semantic effects stay in public signatures
- internal pointer-level implementation techniques stay behind `trusted`
- foreign boundaries remain under `with(Unsafe)`
- safe APIs should not inherit implementation-only unsafety

### 5. Integration quality

- modules that touch the OS should have failure-path tests
- modules that exchange data should have integration tests
- utility modules should have property-style or round-trip tests where appropriate

## API Naming Standards

The public stdlib should feel like one library.

### Preferred style

- short, direct verbs: `open`, `create`, `read`, `write`, `close`
- explicit checked/raw split: `get`, `get_unchecked`
- explicit ownership verbs where useful: `append`, `reserve`, `drop`

### Avoid

- builtin/runtime-hook names in public APIs (`print_string`-style naming)
- multiple unrelated names for the same operation across modules
- names that hide consumption, allocation, or borrowing behavior

### Consistency targets

Use the same surface patterns wherever possible:

- `open` / `create` / `close`
- `read` / `read_all`
- `write` / `write_all`
- `get` / `get_unchecked`
- `set` / `set_unchecked`
- `insert` / `remove` / `contains`
- `len` / `is_empty`

## Collection Standards

Collections should be admitted slowly and held to a higher bar than utility modules.

### A collection should earn its place by offering:

- clear low-level value
- explicit allocation behavior
- obvious ownership semantics
- strong invariants
- strong testing depth

### Required standards for collections

1. **API honesty**
   - checked vs unchecked access must be explicit
   - insertion/removal/lookup semantics must be obvious
   - mutation should be visible and unsurprising

2. **Allocation honesty**
   - constructors, growth, reserve paths, and teardown should make allocation visible

3. **Testing depth**
   - deterministic unit tests
   - edge-case tests
   - trace/property tests for operation sequences

4. **Auditability**
   - if a collection uses `trusted`, that boundary should remain small and explainable

### Collection priority order

After the current core (`Vec`, `HashMap`, `HashSet`), the strongest next additions are:

1. deque / ring buffer
2. priority queue
3. ordered map / ordered set
4. bitset / bit array

Later, if justified:

- fixed-capacity vectors/lists
- arenas / slabs / slot maps
- small inline-buffer collections

## Testing Standards

The stdlib should be tested more like infrastructure than like examples.

### Every module should have:

- direct unit tests for basic behavior
- edge-case tests for failure and boundary conditions

### Utility modules should also have:

- property or round-trip tests where natural (`fmt` / `parse`, hashing helpers, etc.)

### Systems modules should also have:

- failure-path tests
- integration tests with real handles/resources where practical

### Collections should also have:

- trace/property tests against a simple reference model
- growth, overwrite, remove, and empty-case coverage

### Reports and compiler-facing APIs should also have:

- consistency tests ensuring reported effects/trust/layout agree with actual semantics

## Current Situation

The `std/` tree exists, but it is still early:

- `vec`
- `string`
- `io`
- `alloc`
- `mem`
- `ptr`
- `option`
- `result`
- `math`
- `test`
- `libc`

This is a useful base, but not yet a mature standard library.

Several current modules are still incomplete or need correctness work:

- `vec`
- `string`
- `io`

Before adding a lot more surface area, those modules need to become trustworthy.

## What Would Make It Excellent

To make the stdlib genuinely strong for Concrete, it should optimize for three things at once:

- low-level usefulness
- explicitness
- composability without hidden behavior

That implies a few broad rules:

1. **Make `bytes` the real center.**
   Not `String`. A low-level stdlib should revolve around owned bytes, borrowed slices, and borrowed text views.

2. **Separate owned from borrowed everywhere.**
   Examples:
   - `Bytes` vs borrowed byte slice/view
   - `String` vs borrowed text view
   - owned path buffer vs borrowed path view
   - owned file/socket handle vs borrowed handle/view

3. **Make handles first-class and explicit.**
   File, socket, listener, subprocess, and directory APIs should revolve around explicit resource types, not raw integer-ish handles.

4. **Keep typed errors small and local.**
   Prefer small enum error types per module. Avoid one giant catch-all error type too early.

5. **Keep allocation visible.**
   The stdlib should not drift into “allocation happens somewhere in the library.”

6. **Prefer eager, explicit APIs over abstraction towers.**
   Concrete should not copy the giant iterator/future culture of larger ecosystems.

7. **Stay concurrency-neutral until the concurrency story is real.**
   `std.io`, `std.net`, and `std.process` should not smuggle in a runtime model too early.

8. **Expose layout and FFI reality cleanly.**
   A good low-level stdlib should work naturally with:
   - `repr(C)`
   - explicit FFI wrappers
   - explicit foreign handles
   - raw pointers where necessary, but not by default

9. **Keep it small but deep.**
   Concrete does not need Rust’s breadth. It needs a smaller, sharper, more coherent core.

## What The Stdlib Needs First

The next milestone should not be "more modules everywhere." It should be "a coherent low-level foundation."

### 1. Make the current core modules solid

First priority:

- `vec`
- `string`
- `io`

They should stop feeling like prototypes and start feeling like stable low-level building blocks.

### 2. Add an owned byte buffer type

Concrete needs an owned byte-oriented buffer more than it needs richer high-level string APIs.

This should become the foundation for:

- file I/O
- network I/O
- parsing
- binary protocols
- formatting

This is the real center of the low-level stdlib. `bytes` should come before “more string helpers.”

### 3. Add borrowed views

Concrete should have explicit non-owning views, such as:

- slice/span-like views over contiguous memory
- borrowed string/text views

These fit the language well because they preserve explicit ownership while still making low-level APIs practical.

This is one of the highest-value design moves the stdlib can make.

### 4. Build real file/process/env/path modules

The stdlib should eventually expose coherent modules for:

- file open/read/write/flush/seek
- paths and owned path buffers
- environment access
- process arguments and process control

These should become the foundation for “real program” boundaries:

- files and directories
- path handling
- process spawning and exit
- environment access
- later, time and networking surfaces

`path` should be explicit, not just a subtopic inside filesystem APIs.

### 5. Wrap networking builtins in a real stdlib layer

The builtins exist, but they still need a higher-level stdlib surface that is explicit without being raw.

### 6. Improve formatting and testing support

Concrete should have:

- a small explicit formatting layer
- stronger stdlib test helpers

### 7. Grow collections carefully

After `vec` is solid, then expand collections further.

The stdlib should prefer:

- explicit allocation
- explicit ownership
- eager operations

It should avoid hidden control flow or trait-heavy abstraction layers.

## Error, Handle, and Allocator Policy

### Error Shape

Safe-facing APIs should prefer:

- small enum error types per module
- no opaque integer-ish error codes
- no hidden sentinel-style failure signaling

### Handle Ownership

For `fs`, `net`, and later `process`, the stdlib should prefer:

- owned handle types
- borrowed handle/view types where clearly useful
- no raw fd/socket integers in safe-facing APIs

### Allocation Policy

Allocator-sensitive APIs should make allocation visible. In practice that means:

- APIs should make owned return values obvious
- allocation-sensitive modules should not hide cost
- allocator/capability-aware APIs should stay explicit where appropriate

Concrete should stay closer to Zig/Odin here than to more ambient-allocation ecosystems.

## Best Ideas To Borrow

Concrete should borrow ideas selectively. The right question is not "what is popular?" but "what strengthens Concrete without betraying its design?"

### From Rust

Useful ideas:

- `Result` / `Option`-centric API design
- borrowed views like slices and string views
- path/path-buffer separation
- `OwnedFd` / `BorrowedFd` style handle ownership
- `BorrowedBuf` / `BorrowedCursor` style explicit buffered I/O APIs

Why these fit:

- they keep ownership visible
- they make I/O and handles safer without hiding the machine
- they map well onto Concrete's ownership/capability story

What not to copy:

- heavy iterator-combinator ecosystems
- trait-driven APIs that hide dispatch

### From Zig

Useful ideas:

- allocator-explicit library design
- direct, practical low-level APIs
- stdlib as a systems interface rather than a convenience layer
- `MultiArrayList` as a future data-oriented collection idea

Why these fit:

- allocator-explicit design is one of the strongest matches for Concrete
- Zig’s stdlib discipline is close to Concrete’s intended culture

### From Go

Useful ideas:

- clear, boring standard-library API surfaces
- straightforward file/network/process packages
- practical interfaces for common systems tasks without abstraction theatrics

Why these fit:

- Go is a good reminder that low-level-adjacent APIs benefit from clarity more than cleverness
- Concrete should learn from Go’s readability and packaging discipline without copying its semantics or weak type/resource model

What not to copy:

- implicit nil-heavy API patterns
- concurrency assumptions leaking into too much of the library surface

### From Swift

Useful ideas:

- clear value-oriented API surfaces
- strong separation between owned values and borrowed/temporary views where performance matters
- practical systems-facing libraries with explicit handle/resource types

Why these fit:

- Swift is a useful reference for keeping APIs readable while still being serious about systems boundaries
- it is a reminder that low-level-facing libraries do not need to become unreadable to stay powerful

What not to copy:

- runtime assumptions that are too tied to Swift’s broader execution model

### From Gleam

Useful ideas:

- standard-library readability
- consistent API shape
- preference for a small, coherent surface over a sprawling one

Why these fit:

- Gleam is a useful counterexample to “serious language means sprawling stdlib surface”
- Concrete should preserve this bias toward coherence and readability even while targeting lower-level work

### From Odin

Useful ideas:

- low-level OS/file APIs with explicit allocators where allocation occurs
- typed error returns instead of vague sentinel-style failure signaling
- explicit resource-handle types instead of raw integer-ish handles in safe-facing APIs

Illustrative API shape from Odin's `core:os` direction:

```odin
data, err := os.read_entire_file(path, context.allocator)
if err != os.Error.None {
    // handle error
}

file, err := os.open(path)
if err != os.Error.None {
    // handle error
}
```

Why these fit:

- allocation stays visible in the API shape
- ownership of returned resources is obvious
- error handling stays explicit and typed
- low-level APIs stay practical without becoming implicit or magical

Concrete should copy the API-shape lesson, not the exact syntax: stdlib modules should make allocation, ownership, and failure modes obvious from signatures and types.

Reference:

- [Moving Towards a New `core:os`](https://odin-lang.org/news/moving-towards-a-new-core-os/)

### From C++

Useful ideas:

- `span` as a non-owning contiguous view
- `string_view` as a non-owning text view

Only the concepts are useful. Concrete should avoid C++-style complexity and customization machinery.

### From Erlang / Elixir

Useful ideas:

- failure-aware API design
- clear process/resource boundaries
- supervision-style thinking for later runtime/process/network work

Why these fit:

- even if Concrete does not copy BEAM-style runtimes, it can learn from how these ecosystems make failure and resource ownership part of the design surface

### From Ada / SPARK

Useful ideas:

- contract-minded API design
- readability over cleverness
- low-level library surfaces designed for high-integrity use

Why these fit:

- Concrete should aim for libraries that remain readable under audit, not just expressive to experts

### From Pony

Useful ideas:

- ownership/isolation-aware concurrent API design
- capability-shaped resource boundaries

Why these fit:

- useful for thinking about later `std.sync` or concurrency-adjacent library design without copying Rust’s async complexity

### From Koka

Useful ideas:

- effect-oriented library surface design

Why these fit:

- Concrete can benefit from libraries whose signatures expose blocking, I/O, and authority boundaries cleanly

### From Clojure

Useful ideas:

- transducers as a separation between transformation logic and concrete collection representation

This is interesting for Concrete only if kept:

- eager
- explicit
- free of hidden control flow

Concrete should not import lazy sequence semantics here.

### From newer research

Useful ideas to study:

- `GhostCell` as a research direction for permission-separated shared mutable structures

This is not a near-term stdlib feature. It is useful mainly as a design reference for future advanced data structures.

## What Concrete Should Add

If the above is translated into Concrete-specific stdlib work, the best next modules are:

### `std.bytes`

An owned byte buffer type.

This should support:

- append
- reserve
- clear
- split
- slice

This becomes the foundation for low-level text and binary work.

This should probably be the first major new stdlib type after `vec` / `string` / `io` are corrected.

### `std.slice`

Borrowed contiguous views.

This should cover:

- immutable slice/span
- mutable slice/span
- explicit pointer + length semantics

This is one of the most important non-owning abstractions in the whole stdlib.

### `std.text`

A borrowed text view separated from owned `String`.

`String` can remain the owned growable text/buffer type, but most APIs should not require ownership.

This is how Concrete avoids turning every text API into an allocation API.

### `std.fs`

Real file-system APIs:

- owned file handle
- borrowed file view where useful
- explicit read/write APIs over buffers
- later: path/path-buffer split

`std.fs` should feel like a small explicit systems interface, not a convenience façade.

### `std.path`

Paths deserve their own module rather than being treated as a detail of `fs`.

Concrete should eventually have:

- borrowed path views
- owned path buffers
- normalization/join/split helpers that stay explicit and allocation-visible

Path handling is a foundational low-level concern and should not be hidden inside unrelated file APIs.

### `std.net`

A real networking layer over the existing builtins:

- owned socket/stream/listener handles
- explicit buffer-oriented I/O
- no raw unsafe integer/socket APIs in safe-facing surfaces

Networking should follow the same ownership and effect rules as files:

- explicit handles
- explicit buffer use
- no hidden runtime commitments

### `std.fmt`

A small explicit formatting layer.

Not macro-heavy formatting. Not hidden allocation.

Formatting should be useful enough for diagnostics and tools, but not become a second string-magic subsystem.

### Later: `std.collections.multi_array`

Only after the basic containers are solid.

This is where a Zig-inspired data-oriented collection could fit.

## High-Value Later Additions

After the core foundation is solid, Concrete should add only a few more modules, carefully.

### `std.time`

Useful for:

- clocks
- durations
- timestamps
- sleeping/timers later, but only once the runtime story is clearer

### `std.rand`

Only if kept explicit and capability-gated.

Useful for practical systems work, but it should not become ambient magic.

### `std.hash`

Useful for:

- hashing APIs
- map/set internals
- checksums and digests later

### `std.collections.map`

Once `Vec`, `bytes`, and borrowed views are solid, a real map API becomes worthwhile.

Concrete should still keep the collection surface small.

### `std.collections.set`

Only after `map` is solid, and probably built on top of it.

### `std.iter`

If Concrete adds an iteration helper layer at all, it should be:

- small
- explicit
- eager

Not a trait-heavy iterator universe.

### `std.sync`

Much later, and only once the concurrency design is clearer.

### `std.ffi`

A small module for explicit FFI-facing helpers and safer wrappers around low-level interop patterns.

### `std.layout`

Potentially useful for exposing layout/size/alignment information in a principled way if the language wants a stdlib-facing low-level reflection surface.

### `std.parse`

Only after bytes/text/fmt are in good shape.

Useful for parsing:

- integers
- floats
- paths
- other small foundational formats

## What Not To Standardize Too Early

Avoid adding these before the low-level foundation is stable:

- huge collection libraries
- lazy streams
- future/promise ecosystems
- runtime-bound async helpers
- too many string-heavy utilities before bytes/slice/text are solid
- heavy collection sprawl before bytes/slice/fs/net are solid
- broad convenience wrappers that hide ownership or effects
- concurrency abstractions that quietly commit the stdlib to one runtime model

## Recommended Build Order

1. ~~fix `vec`, `string`, `io`~~ — done
2. ~~add `bytes`~~ — done
3. ~~add `slice`~~ — done
4. ~~add `text`~~ — done
5. ~~add `path`~~ — done
6. ~~real `std.fs`~~ — done
7. ~~`std.env` and `std.process`~~ — done
8. ~~`std.net`~~ — done
9. small `std.fmt`
10. stronger `std.test`

After that foundation:

11. `std.time`
12. `std.rand`
13. `std.hash`
14. `std.collections.map`
15. `std.collections.set`
16. maybe a small eager `std.iter`
17. later `std.ffi`
18. later `std.parse`
19. much later `std.sync`

## Summary

The stdlib Concrete needs is not huge. It is:

- low-level
- ownership-honest
- resource-honest
- explicit in its effects
- allocator-visible where necessary
- smaller and more coherent than the broad stdlibs of bigger languages

If Concrete wants a genuinely excellent stdlib, the guiding priorities should be:

- bytes before strings
- slices before iterator ecosystems
- owned and borrowed handles before convenience wrappers
- typed errors before catch-all APIs
- explicit allocation before ergonomic shortcuts
- small coherent depth before broad surface area
