# Rust Stdlib Packet

Status: research

Source pointers: Rust `std` documentation, especially `alloc`, `collections`,
`ffi`, `fs`, `io`, `net`, `path`, `process`, `sync`, `thread`, `time`, `env`,
`error`, `fmt`, `hash`, `iter`, `ops`, `panic`, and primitive integer APIs.

## What Rust Has

- Core data: `Option`, `Result`, `Vec`, `String`, slices, boxed/owned values.
- Collections: hash maps/sets, BTree maps/sets, binary heap, linked list,
  VecDeque.
- Iterators as a central abstraction.
- Formatting traits and formatting macros.
- Path/OS string split.
- Filesystem, env, args, process, net, time.
- Threading and synchronization.
- Error traits and panic/unwind infrastructure.
- Extensive primitive numeric APIs: checked/wrapping/saturating/overflowing.
- `std` plus ecosystem split: regex, serde, csv, uuid, http, crypto, tempfiles
  are mostly crates.

## What Concrete Should Copy

1. **The explicit numeric API families.**
   `wrapping_*`, `saturating_*`, and later checked/reporting APIs are a good
   shape. Concrete should keep them as named functions, not ambient modes.

2. **Path vs OS-string clarity.**
   Raw platform strings and validated text are not the same. Concrete should
   keep path/OS-string conversion assumptions visible.

3. **Stdlib/ecosystem split.**
   Rust's std leaves many large domains to crates. Concrete should do the same
   for regex, broad crypto, HTTP, CSV/UUID if they outgrow the small core, and
   compression/archive.

4. **Collections coverage.**
   `Vec`, maps/sets, ordered maps/sets, heap, deque are table stakes.

5. **Doc/test/examples culture.**
   Concrete APIs should have examples plus gates/oracles because the language's
   evidence story depends on them.

## What Concrete Should Not Copy

- Macro-heavy formatting/derive/serde patterns.
- Hidden panics/unwinding as ordinary control flow.
- Trait-heavy conversion/prelude patterns that hide behavior behind typeclass
  resolution.
- Threading/sync as a default early stdlib surface.

## Missing Concrete Items This Pressures

- BTree versus sorted-array collection decision for ordered maps/sets.
- `std.fs.temp`.
- `std.path.glob` or equivalent bounded path matching.
- `std.uuid` parse/format.
- `std.csv`.
- `std.terminal` / no-color / TTY helpers.
- Runtime abort/backtrace/source-location diagnostics.

## Concrete Classification

- Copy now: value/result/errors, collections, path split, numeric families.
- Stdlib later: uuid, csv, tempfile, terminal helpers.
- Package later: regex, serde-like broad serialization, HTTP, broad crypto.
- Non-goal: macro/derive ecosystem.

