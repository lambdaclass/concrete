# Stdlib Comparative Inventory

Status: research

Purpose: collect what mature standard libraries and core ecosystems expose, so
Concrete can decide deliberately instead of rediscovering gaps one workload at a
time. This is not a promise to copy the surface. It is an inventory plus a
Concrete classification.

Comparison set: Zig, Odin, Rust, C++, Go, Clojure, Elixir, Roc, Gleam, and
OCaml. These cover low-level systems libraries, pragmatic command-line/runtime
libraries, and functional data/sequence libraries.

Classification:

- **core-now**: should be in Concrete's stdlib before serious workloads depend on
  it.
- **stdlib-later**: belongs in stdlib, but only after earlier foundations.
- **package-later**: useful, but not part of the small core.
- **hosted-only**: requires OS/host authority and must be capability-visible.
- **freestanding-later**: only after the no-std/freestanding target exists.
- **research-later**: needs language/runtime/proof design first.
- **non-goal**: conflicts with Concrete's design.

## Executive Summary

Concrete's Phase 7 roadmap already covers the important small core:
errors, bytes/text/path, collections, formatting/parsing, binary helpers,
capability-scoped I/O, tests/oracles, boundary modules, evidence classes, and
validation workloads.

The strongest missing practical items to keep visible are:

1. CSV/TSV.
2. UUID parse/format and capability-visible generation.
3. MIME/media-type parsing.
4. Bounded glob/path-pattern matching.
5. Temp files/directories and file permission/metadata policy.
6. Terminal/TTY helpers and no-color/ANSI policy.
7. Runtime abort/source-location/backtrace policy.
8. Memory-mapped files as hosted-later.
9. CRC32/Adler32 and varint/LEB128.
10. Explicit package-later buckets for BigInt/decimal/complex, regex, broad
    Unicode, compression/archive, broad crypto/TLS, HTTP client/server,
    persistent collections, and concurrency/sync.

## Existing Concrete Coverage

Already planned or present:

- `Option`, `Result`, test helpers.
- `Bytes`, slices, byte cursors, `ByteView`, `Text`, `String`, ASCII, path.
- `Vec`, maps, sets, ordered maps/sets, deque, heap, bitset, slices.
- `fmt`, `parse`, scanner/parser core.
- `hex`, base64, URI, JSON, binary encoding/decoding, semver/config.
- `hash`, checksum, deterministic random.
- `io`, writer, fs, env, args, process, net, time, libc boundary.
- CLI/log/progress helpers.
- constant-time helpers.
- handle-relative filesystem and boundary modules.
- oracle/vector/workload tests and stdlib evidence classes.
- hosted/freestanding split.

## Comparison Matrix

| Family | Seen In | Concrete Status | Recommendation |
| --- | --- | --- | --- |
| Core option/result/errors | Rust, Zig, Roc, Gleam, OCaml, Elixir | core-now | Already planned. Keep ignored-result behavior explicit. |
| Growable arrays/lists | Rust, Zig, Odin, C++, Go, Clojure, Elixir | core-now | `Vec` plus slices/builders. |
| Maps/sets | Rust, Zig, Odin, C++, Go, Clojure, Elixir, OCaml | core-now | Existing maps/sets; add coherence rules for custom hash/eq/cmp. |
| Ordered maps/sets | Rust, C++, OCaml, Clojure sorted maps | core-now | Existing ordered map/set; gate min/max/range behavior. |
| Heap/priority queue/deque | Rust, C++, Zig/Odin patterns | core-now | Existing heap/deque; add recipes. |
| Queue/stack wrappers | C++, Java-like ecosystems, Go patterns | stdlib-later | Thin wrappers over `Vec`/`Deque`, useful but not essential. |
| Persistent immutable collections | Clojure, Elixir | package-later | Not core; hidden sharing and allocator pressure need design. |
| Iterators/ranges/streams | Rust, C++, Clojure seq, Elixir Stream, Roc Iter | stdlib-later | Explicit iterator/builders only; no closure-backed lazy ecosystem in core. |
| Sorting/searching | All | core-now | Planned. Include stable/unstable decision and oracle vectors. |
| Bytes/buffers/endian | Zig, Rust, Go, Odin, C++ | core-now | Planned; keep endian/offset obligations prominent. |
| Varint/LEB128 | Go ecosystem, Rust crates, binary formats | stdlib-later | Add to `std.bin`; useful for protocol/wasm/tar-like tools. |
| Bitsets/bit operations | C++, Rust, Zig, Odin | core-now | Bitset exists; add bit/word packing helpers as workloads require. |
| Text/strings/ASCII | All | core-now | Planned. Keep UTF-8 validation explicit. |
| Unicode normalization/graphemes/case folding | Rust crates, Clojure/Java, Elixir/Erlang | package-later | Policy in core; full Unicode later/package. |
| Regex | Rust crates, C++, Go, Clojure/Java, Elixir | package-later | Not core; complex control/perf. Prefer parser/cursor/glob. |
| Glob/path pattern | Go filepath, Rust glob crates, shells | stdlib-later | Add bounded `std.path.glob`; no regex semantics. |
| Path/OS strings | Rust, Go, Zig, C++ filesystem | core-now | Planned; include OS-string and normalization assumptions. |
| File metadata/permissions | Rust/Go/Zig/C++ | hosted-only core | Make explicit: size, type, readonly, executable, mtime, symlink. |
| Temp files/dirs | Rust crates, Go patterns, Zig/Odin patterns | hosted-only stdlib-later | Race-safe creation, cleanup, authority. |
| Memory-mapped files | OS libraries, Rust crates, Zig | hosted-later | Capability-visible; not core. |
| Console/terminal/TTY | Rust crates, Go, Zig, Elixir IO | stdlib-later | TTY detection, ANSI/no-color, stderr/stdout policy. |
| Formatting | Rust, Zig, C++, Go, Elixir | core-now | Explicit `std.fmt`; no macros/interpolation. |
| Parsing/scanners | Go bufio/scanner, Rust parsers, Zig readers | core-now | Planned scanner/parser core. |
| CSV/TSV | Go `encoding/csv`, ecosystem libraries | stdlib-later | Add `std.csv`; small, high utility. |
| JSON | Rust/Go/Zig ecosystems, Clojure/Elixir libs | core-now or stdlib-later | Planned tokenizer-first; DOM only with explicit allocation. |
| URI/URL | Rust/Go ecosystems, Gleam/Roc influence | core-now or stdlib-later | Planned URI. Full URL/network authority separate. |
| MIME/media types | Go/Rust ecosystems, HTTP tooling | stdlib-later | Add `std.mime` or `std.net.media_type`. |
| Base64/base32/hex | Go/Rust/Zig/Odin | core-now for hex/base64; package-later base32 | Hex exists; base64 planned; base32 later. |
| Checksums CRC/Adler | Go hash/crc32/adler32, Zig/Rust ecosystems | stdlib-later | Name CRC32 and Adler32 under `std.checksum`. |
| Cryptographic hashes/HMAC | Rust/Zig/Go ecosystems | narrow core, broad package-later | SHA-256/HMAC narrow; TLS/public-key/package later. |
| Random entropy | Rust/Zig/Go | hosted-only | Deterministic RNG core; OS entropy needs `Random` capability. |
| Random distributions/shuffle | C++, Rust crates, Go math/rand | package-later | Uniform/range may be core; distributions later. |
| Time/duration | Rust/Go/C++/Elixir | core-now hosted | Planned; monotonic vs wall-clock must be explicit. |
| Timers/sleep | Rust/Go/Elixir | hosted-later | Capability-visible, maybe `Clock`/`Thread` split later. |
| Process/env/args | Rust/Go/Zig/Odin | hosted-only core | Planned. |
| Networking sockets | Rust/Go/Zig/Odin, Elixir/Erlang | hosted-only core-minimal | Minimal handles and pure parsers; no full HTTP yet. |
| HTTP client/server | Go, Rust crates, Elixir | package-later | Deferred; too large for core. |
| TLS/certificates | Go/Rust/OpenSSL ecosystems | package-later | Broad crypto/trust-store policy later. |
| Compression/archive | Go/Zig/Rust crates, C++ libs | package-later | Deferred. |
| Logging | Rust crates, Go log, Elixir Logger | stdlib-later | Small `std.log`, capability-visible output. |
| Progress/status | CLI ecosystems | stdlib-later | Planned, workload-pulled. |
| CLI argument parsing | Rust crates, Go flag, C++ libs, Elixir OptionParser | stdlib-later | Planned `std.cli`. |
| Config/semver | Ecosystem common | stdlib-later | Planned; choose INI/TOML/key-value scope. |
| UUID | Rust crates, Clojure/Java, Elixir libs | stdlib-later | Add parse/format; generation needs entropy. |
| BigInt/rational/decimal | Clojure, OCaml Zarith, Rust crates, C++ libs | package-later | Not core; proof/runtime implications. |
| Complex numbers | C++, Rust crates, Zig numeric patterns | package-later | Not core. |
| Atomics/sync/threads/channels | Rust/C++/Go/Elixir/Zig/Odin | research-later | Concurrency phase only. |
| Async runtime/tasks | Rust/Go/Elixir, Roc tasks | research-later/non-core | No hidden runtime; explicit concurrency only later. |
| Reflection/dynamic typing | Clojure/Elixir | non-goal | Conflicts with audit/static surface. |
| Macros/code generation | Rust/Clojure/Elixir/C++ templates | non-goal | Rejected; external generators output ordinary source. |
| Serialization frameworks | Rust serde, Clojure datafy, Elixir protocols | package-later | No reflection/macros; typed decode first. |
| OS/platform-specific APIs | Rust/Go/Zig | hosted-later | Capability-visible and target-profile-gated. |
| Debug/backtrace/source locations | Rust/Go/C++/Zig | backend/stdlib-later | Needed for abort diagnostics and release UX. |
| Sanitizer/instrumentation hooks | C/C++/Rust/Zig | validation surface | Planned as tested/runtime_checked, not proof. |

## Language Notes

### Zig

Zig's stdlib is useful as the high-coverage systems reference: allocators,
formatting, json, base64, URI, fs/process/time/net, crypto, compression,
target/OS/debug-format surfaces, atomics, and testing. Concrete should not copy
the breadth into core. Copy the explicitness: allocation, target assumptions,
and authority should be named and reportable.

### Odin

Odin's core library is practical and batteries-included for systems/game code:
collections, strings, OS, math, encoding, and runtime helpers. Concrete should
learn from the "small practical modules" shape, but keep evidence classes and
capabilities visible.

### Rust

Rust's `std` is conservative in some places and ecosystem-heavy in others:
collections, fs/io/net/process/thread/sync/time/path are core, while regex,
serde, uuid, csv, http, and broad crypto live mostly in crates. Concrete should
copy the split: keep the core small and push large protocols/formats to
packages.

### C++

C++ stdlib is wide in containers, algorithms, filesystem, chrono, locale,
regex, random, complex, atomics, threads, and iostreams. Concrete should copy
algorithm/data-structure coverage selectively, but avoid locale/iostream/regex
complexity in the core.

### Go

Go's stdlib is the strongest "batteries included server/tool" reference:
`encoding/csv/json/base64`, `net/http`, `archive`, `compress`, `crypto`,
`regexp`, `flag`, `testing`, `os`, `io`, `bufio`, `time`, `sync`, and `context`.
Concrete should copy the practical coverage list for research visibility, but
not the ambient authority or hidden goroutine/runtime assumptions.

### Clojure

Clojure's core teaches persistent data structures, sequences, rich maps/sets,
string/set/walk helpers, dynamic vars, agents/atoms/refs, and Java interop. For
Concrete, persistent collections and lazy sequences are package-later at most;
dynamic vars/reflection are non-goals.

### Elixir

Elixir's stdlib brings Enum/Stream, IO/File/Path, Date/Time, Regex, Agent,
GenServer, Registry, Supervisor, and rich process/actor runtime conventions.
Concrete should not copy the OTP runtime, but should learn from the clarity of
module boundaries and explicit process/resource naming.

### Roc And Gleam

Roc and Gleam are useful as small-core references: pleasant lists/dicts/sets,
strings, result/option, decoders, builders, and simple effect/host boundaries.
Concrete should copy the teaching value and the compact API shape, not closures,
implicit capture, or hidden platform effects.

### OCaml

OCaml's stdlib and ecosystem suggest a small but expressive functional core:
Map/Set/Hashtbl, Buffer, Bytes/String, Seq, Unix, Str/regex, Bigarray. Concrete
should copy only where it stays explicit: buffers, maps/sets, and Unix-like
hosted boundaries.

## Concrete Additions To Consider

Add these to active planning when a workload pulls them:

1. `std.csv` / TSV parser and writer.
2. `std.uuid` parse/format, with entropy-backed generation hosted-later.
3. `std.mime` / media-type parser.
4. `std.path.glob` bounded glob matching.
5. `std.fs.temp` plus permission/metadata policy.
6. `std.terminal` or `std.io.terminal` for TTY, ANSI/no-color, width.
7. Runtime abort/source-location/backtrace policy.
8. Memory-mapped files as hosted-later.
9. CRC32/Adler32 in `std.checksum`.
10. Varint/LEB128 in `std.bin`.

Explicitly keep these out of the small core:

1. Full regex.
2. Full HTTP client/server.
3. TLS/certificates and broad crypto.
4. Compression/archive.
5. Actor/OTP-style runtime.
6. Threads/channels/atomics/SIMD until concurrency/backend phases.
7. Persistent immutable collections as default stdlib data structures.
8. Reflection/dynamic typing/macros/derive.
9. Broad Unicode normalization/graphemes/case-folding.
10. BigInt/decimal/complex unless a workload forces them.

## How To Use This Note

Use this as the research inventory before editing the roadmap. The roadmap
should absorb only items that become concrete technical work with examples and
gates. For now, the strongest next candidates are CSV, UUID, MIME/media-type,
bounded glob, temp/metadata filesystem policy, terminal helpers, and runtime
abort/backtrace policy.
