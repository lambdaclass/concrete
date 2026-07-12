# Zig Stdlib Packet

Status: research (API-shape claims verified 2026-07-12 — see verified section below)

Source pointers: Zig language documentation and stdlib docs (`std`), including
allocator, fs, io, fmt, json, crypto/hash, compress/archive, net, time, process,
thread/atomic, target, testing, and debug surfaces.

## What Zig Has

- Allocator-first API style: many APIs take an allocator explicitly.
- Strong bytes/buffers/slices story.
- Formatting and parsing without macros.
- Filesystem, process, environment, time, networking.
- JSON and other data helpers.
- Hashing, crypto, random, checksums.
- Compression/archive modules.
- Testing and debug utilities.
- Target/OS/platform introspection.
- Atomics, threading, synchronization.
- Low-level memory, endian, bit, and packed-data helpers.

## What Concrete Should Copy

1. **Explicit allocation pressure.**
   Concrete should keep allocation visible through `with(Alloc)` and later
   allocation budgets, not hidden inside convenience builders.

2. **Small low-level helpers.**
   Endian reads/writes, byte packing, checksums, bounded buffers, and parser
   cursors are exactly Concrete-shaped.

3. **Target/host assumptions as facts.**
   Zig's target awareness maps to Concrete reports: target triple, hosted vs
   freestanding, libc/startup assumptions, endian/layout facts.

4. **Testing as a first-class stdlib surface.**
   Concrete's `std.test` should grow oracle helpers and expected-failure
   helpers before broad examples depend on ad hoc scripts.

5. **Freestanding/no-std discipline.**
   Zig is the main inspiration for a useful freestanding split, but Concrete
   should attach evidence and capability facts to the split.

## What Concrete Should Not Copy

- Broad compile-time execution / comptime as a general metaprogramming system.
- Huge core crypto/compression/archive surface before workloads demand it.
- Threading/atomics in the core stdlib before the concurrency model is designed.
- Target-specific APIs as ambient compile-time conditionals.

## Missing Concrete Items This Pressures

- `std.fs.temp`, metadata, permissions, and symlink policy.
- `std.bin` varint/LEB128 and byte packing.
- CRC32/Adler32 under `std.checksum`.
- Terminal/debug/backtrace/source-location policy.
- Freestanding API split and target assumptions.

## Concrete Classification

- Copy now: bytes, endian, formatting, explicit allocation, tests, target facts.
- Stdlib later: temp files, terminal helpers, CRC/Adler, varint.
- Package later: compression/archive, broad crypto.
- Research later: atomics/threads/sync.

## Verified API-Shape Findings (2026-07-12)

Checked against `ziglang.org` master stdlib source (`lib/std/array_list.zig`,
`lib/std/mem/Allocator.zig`, `lib/std/hash_map.zig`):

- **Master Zig moved `ArrayList`/`HashMap` to the *unmanaged* form** — the
  allocator is threaded per call, not stored; the allocator-storing form is now
  `ArrayList.Managed` and is **deprecated**. Cheap construction is `.empty` (a
  `const`), not `init()`. Direct evidence *for* the allocator-as-value question
  (Phase 7 #2): Zig is moving toward explicit threaded allocators (arenas,
  testability), not away from them.
- **Allocation is signalled by two things together**: an `Allocator` parameter
  *and* an `Allocator.Error!T` return. So **no allocator param + no `!` ⇒
  provably non-allocating** — a syntactic, checkable classification. Concrete's
  `with(Alloc)` + `Result` gives the same property; preserve "absence is provable."
- Ownership transfer is named by verb: `toOwnedSlice` / `dupe` hand the buffer to
  the caller; `deinit` / `free` take it back. Allocation failure is one canonical
  `error{OutOfMemory}` in the error channel — never null, never a sentinel.
- **Anti-pattern under linearity:** `defer x.deinit()` fights move-consumption
  (scope-exit cleanup runs regardless of moves) — do NOT port it; linear
  ownership replaces it. `errdefer` (drop a partially-built value only on the
  error path) is the one idiom worth adapting.

