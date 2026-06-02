# Stdlib Target: First-Release Core

Status: reference (Phase 3, item 52)

This document names the exact modules and APIs that must exist in the first-release stdlib, marks what is intentionally out of scope, and maps each module to the Phase 2 gap findings that justify it.

For the stdlib design principles that filter every API, see [STDLIB_DESIGN_PRINCIPLES.md](STDLIB_DESIGN_PRINCIPLES.md).
For the existing stdlib direction and module descriptions, see [STDLIB.md](STDLIB.md).
For the execution model layers (core / alloc / hosted), see [EXECUTION_MODEL.md](EXECUTION_MODEL.md).
For Phase 3 exit criteria, see [PHASE_EXIT_CHECKLISTS.md](PHASE_EXIT_CHECKLISTS.md).

---

## Quality Bar

The first-release stdlib is not trying to be large. It is trying to be:

- **Rust/OCaml-clear**: every module has one purpose, one naming convention, one ownership story. A reader should understand what a module does and what it costs from its type signatures alone.
- **Zig/Odin-useful**: parser code, binary protocol code, fixed-capacity code, and FFI-boundary code should feel direct, not ceremonial. If the pressure programs needed it, the stdlib should provide it.
- **Clojure/Elixir-discoverable**: every public API has a docstring. Every module has at least one small example. The "where do I start" question should have one obvious answer per task.

This is a pre-1.0 stdlib. It does not promise backward compatibility across releases. It promises that the supported surface is coherent, tested, and honest about its limitations.

---

## Module Inventory

Modules are organized by execution-model layer. Core-layer modules have no host dependency. Alloc-layer modules require malloc/free. Hosted-layer modules require a POSIX-like OS.

### Core Layer (no host dependency, no allocation)

These modules are proof-friendly, predictable-profile-compatible, and usable in freestanding targets.

#### `std.option`

Generic `Option<T>` with `Some` and `None`.

**Must-have APIs**:
- `is_some`, `is_none` — query
- `unwrap_or(default: T) -> T` — safe extraction with fallback
- `map<U>(fn(&T) -> U) -> Option<U>` — transform if present (deferred until closures/fn-ptr ergonomics settle; library-only workaround acceptable)

**Exists today**: `Option<T>`, `is_some`, `is_none`. Missing: `unwrap_or`, `map`.

**Justifies**: every pressure program that returns success/failure through ad hoc `ok: i32` fields. `pressure_fixcap_ring_buffer.con` returns `IntPopResult { value, ring, ok }` where `Option<(i32, IntRing)>` would be clearer.

#### `std.result`

Generic `Result<T, E>` with `Ok` and `Err`.

**Must-have APIs**:
- `is_ok`, `is_err` — query
- `unwrap_or(default: T) -> T` — safe extraction
- `map_err<F>(fn(&E) -> F) -> Result<T, F>` — error conversion (deferred until closures settle; manual match is the workaround)
- `with_context` / error-conversion helpers — library-first, no syntax sugar required

**Exists today**: `Result<T, E>`, `is_ok`, `is_err`. Missing: `unwrap_or`, `map_err`, conversion helpers.

**Justifies**: `pressure_parse_json_subset.con` encodes errors as `error: i32` fields. `examples/service_errors/` builds 3 error enums and a unified `ServiceError` — `Result` helpers would cut the conversion boilerplate.

#### `std.math`

Numeric helpers.

**Must-have APIs**:
- `min`, `max` — for all integer widths
- `abs` — signed integers
- `clamp(val, lo, hi)` — bounded range

**Exists today**: `max`. Missing: `min`, `abs`, `clamp`.

**Justifies**: `pressure_fixcap_controller.con` and ring-buffer pressure programs use manual min/max/clamp patterns.

#### `std.ascii`

Character classification.

**Must-have APIs**:
- `is_digit`, `is_alpha`, `is_alphanumeric`, `is_whitespace`, `is_upper`, `is_lower` — byte-level classification
- `to_lower`, `to_upper` — byte-level case conversion

**Exists today**: all of the above. This module is complete for first release.

**Justifies**: `pressure_parse_json_subset.con` and `pressure_parse_http_request.con` both hand-roll `is_digit` and `is_whitespace` with magic constants.

#### `std.mem`

Low-level memory primitives.

**Must-have APIs**:
- `sizeof<T>() -> u64` — compile-time type size
- `zeroed<T>() -> T` — zero-initialized value (Copy types only)

**Exists today**: `sizeof`. Missing: `zeroed`.

#### `std.test`

Test assertion helpers.

**Must-have APIs**:
- `assert_eq`, `assert_ne`, `assert_true`, `assert_false` — equality and truth checks
- `assert_gt`, `assert_lt`, `assert_ge`, `assert_le` — comparison assertions
- `str_eq`, `assert_str_eq` — string equality

**Exists today**: all of the above. This module is complete for first release.

#### `std.numeric` (new)

Endian-aware byte reading and checked arithmetic.

**Must-have APIs**:
- `read_u16_be`, `read_u16_le` — 2-byte reads from byte slice/buffer
- `read_u32_be`, `read_u32_le` — 4-byte reads
- `read_u64_be`, `read_u64_le` — 8-byte reads
- `write_u16_be`, `write_u16_le` — 2-byte writes
- `write_u32_be`, `write_u32_le` — 4-byte writes
- `write_u64_be`, `write_u64_le` — 8-byte writes
- `checked_add`, `checked_sub`, `checked_mul` — overflow-returning arithmetic for i32/i64/u64
- `wrapping_add`, `wrapping_sub`, `wrapping_mul` — explicit wrapping variants

**Exists today**: nothing. Every parser pressure program reimplements these.

**Justifies**: `pressure_parse_dns_packet.con` hand-rolls `read_u16_be`. `pressure_parse_binary_endian.con` hand-rolls all four endian read/write functions plus has to store `0xDEADBEEF` through byte-level construction because there is no endian API. This is the single highest-value new core module. Roadmap item 68 specifically calls for it.

### Alloc Layer (requires malloc/free)

These modules allocate on the heap. All require `with(Alloc)` in their constructing APIs. All are linear (must be explicitly dropped).

#### `std.string`

Owned mutable UTF-8 string.

**Must-have APIs**:
- `new`, `from_bytes` — construction
- `len`, `cap`, `is_empty` — queries
- `get(at) -> Option<char>` — checked access
- `push_char(c)` — append character
- `append(&str)` — append borrowed string or literal
- `append_int(n)` — append formatted integer
- `eq`, `clone` — comparison and copy
- `starts_with`, `ends_with`, `contains` — substring search
- `to_lower`, `to_upper` — ASCII case conversion
- `clear`, `drop` — cleanup

**Exists today**: all of the above. Needs polish: `from_bytes` construction path, clearer byte-vs-text boundary.

**Justifies**: `pressure_parse_json_subset.con` cannot use strings at all and works at raw byte level with `[u8; 8]` key storage. The pressure programs reveal that having no stdlib string forces every parser to reinvent byte-to-text conversion.

#### `std.bytes`

Owned byte buffer for I/O, parsing, and binary data.

**Must-have APIs**:
- `new`, `with_capacity` — construction
- `push`, `len`, `cap`, `is_empty` — mutation and queries
- `get`, `set` — checked access (Option/bool)
- `get_unchecked`, `set_unchecked` — unchecked fast paths
- `clear`, `drop` — cleanup
- `eq` — byte equality
- `slice(start, end)` — borrow a subrange (returns `Slice<u8>` or equivalent)

**Exists today**: most of the above. Missing: `eq`, `slice`.

**Justifies**: all 5 parser pressure programs operate on fixed-size `[u8; 256]` arrays because there is no stdlib byte buffer for their input. The `Bytes` type exists but lacks comparison and slicing.

#### `std.vec`

Generic growable array.

**Must-have APIs**:
- `new`, `push`, `pop` — core operations
- `get`, `get_unchecked`, `get_mut` — access
- `len`, `cap`, `is_empty` — queries
- `set`, `remove`, `insert` — mutation
- `clear`, `drop` — cleanup

**Exists today**: most of the above. Needs: `insert`, `remove`.

**Justifies**: `pressure_own_ordered_map.con` and `pressure_own_tree.con` use fixed arrays because Vec exists but lacks insert/remove for ordered operations.

#### `std.text`

Borrowed text view (non-owning reference to a string or substring).

**Must-have APIs**:
- Construction from `&String` or substring range
- `len`, `is_empty` — queries
- `eq` — comparison
- Byte-level iteration/access

**Exists today**: basic implementation. Needs: clearer relationship to `String` and `Bytes`.

#### `std.path`

Path manipulation without hidden filesystem effects.

**Must-have APIs**:
- `Path` (borrowed) and `PathBuf` (owned) types
- `join`, `parent`, `file_name`, `extension` — manipulation
- `to_string` — conversion
- No I/O — purely structural

**Exists today**: basic implementation.

#### `std.fmt`

Pure formatting to strings.

**Must-have APIs**:
- `format_int`, `format_uint` — decimal formatting
- `format_hex`, `format_bin`, `format_oct` — alternate bases
- `format_bool` — boolean
- `pad_left`, `pad_right` — fixed-width padding

**Exists today**: all of the above. This module is complete for first release.

#### `std.parse`

String-to-value parsing, inverse of `fmt`.

**Must-have APIs**:
- `parse_int`, `parse_uint` — decimal parsing
- `parse_hex`, `parse_bin`, `parse_oct` — alternate bases
- `parse_bool` — boolean
- `Cursor` — positioned reader over string input with `peek`, `advance`, `skip_whitespace`, `expect_char`

**Exists today**: all of the above. This module is complete for first release.

#### `std.hash`

Deterministic hashing.

**Must-have APIs**:
- `fnv1a_bytes`, `fnv1a_string` — FNV-1a hash
- `hash_u64`, `hash_i32`, `hash_i64`, `hash_string` — typed hash helpers for map/set
- `eq_u64`, `eq_i32`, `eq_i64`, `eq_string` — typed equality helpers for map/set

**Exists today**: all of the above. Complete for first release.

#### `std.map`

Hash map with explicit hash/eq function pointers.

**Must-have APIs**:
- `new(hash_fn, eq_fn)` — construction with explicit function pointers
- `insert`, `get`, `contains`, `remove` — core operations
- `len`, `is_empty`, `clear`, `drop` — management

**Exists today**: all of the above. Complete for first release.

#### `std.set`

Hash set (thin wrapper around map).

**Must-have APIs**: same shape as `std.map` minus value operations.

**Exists today**: complete.

#### `std.deque`

Double-ended queue / ring buffer.

**Must-have APIs**:
- `new`, `push_front`, `push_back`, `pop_front`, `pop_back`
- `len`, `is_empty`, `cap`
- `get` — checked index access
- `clear`, `drop`

**Exists today**: basic implementation.

**Justifies**: `pressure_fixcap_ring_buffer.con` hand-rolls a ring buffer with explicit head/tail/count management in 120 lines. `Deque` is the stdlib answer.

#### `std.heap` (priority queue)

Min-heap / priority queue.

**Must-have APIs**:
- `new(cmp_fn)` — construction with explicit comparison function
- `push`, `pop` — insert and extract-min
- `peek` — view min without removal
- `len`, `is_empty`, `drop`

**Exists today**: basic implementation.

#### `std.ordered_map` / `std.ordered_set`

Tree-based deterministic-iteration containers.

**Must-have APIs**:
- `new(cmp_fn)` — construction with explicit comparison
- `insert`, `get`, `contains`, `remove`
- `len`, `is_empty`, `clear`, `drop`
- Deterministic iteration order (sorted by key)

**Exists today**: basic implementation.

**Justifies**: `pressure_own_ordered_map.con` hand-rolls a sorted-array map because no ordered container exists in the stdlib.

#### `std.bitset`

Fixed-size or growable bit array.

**Must-have APIs**:
- `new(size)` — construction
- `set`, `clear`, `test` — bit operations
- `count` — popcount
- `and`, `or`, `xor` — set operations
- `drop`

**Exists today**: basic implementation.

#### `std.sha256`

SHA-256 cryptographic hash.

**Must-have APIs**:
- `hash_bytes(data) -> [u8; 32]` or equivalent

**Exists today**: implemented.

#### `std.hex`

Hex encoding/decoding.

**Must-have APIs**:
- `encode`, `decode`

**Exists today**: implemented.

#### `std.slice`

Borrowed contiguous views over Vec, Bytes, arrays.

**Must-have APIs**:
- `Slice<T>` (immutable) and `MutSlice<T>` (mutable)
- `len`, `is_empty` — queries
- `get_unchecked` — access (checked access is the priority gap)
- `get(at) -> Option<&T>` — checked access (missing today)
- `subslice(start, end)` — sub-view without allocation

**Exists today**: `Slice<T>` and `MutSlice<T>` with unchecked access only. Missing: checked `get`, subslice.

**Justifies**: roadmap item 54. Every parser program hand-rolls bounds checking. The checked/unchecked split must be settled here.

### Hosted Layer (requires POSIX libc)

#### `std.io`

Console and basic I/O.

**Must-have APIs**:
- `print`, `println`, `print_int`, `eprint`, `eprintln` — console output
- `File::create`, `File::open` — file handle construction returning `Result<File, IoError>`
- `read_line` — basic stdin (deferred if not needed by pressure examples)

**Exists today**: print functions and File basics.

#### `std.fs`

File system operations.

**Must-have APIs**:
- `read_file`, `write_file`, `append_file` — whole-file operations returning `Result`
- `file_exists` — probe
- `read_to_string` — convenience
- `File` handle with `read_bytes`, `write_bytes`, `seek`, `tell`, `close`

**Exists today**: all of the above. Needs typed error hardening polish.

#### `std.env`

Environment variables.

**Must-have APIs**:
- `get(name) -> Option<String>`, `set(name, value)`, `unset(name)`

**Exists today**: implemented.

#### `std.process`

Process control.

**Must-have APIs**:
- `exit`, `getpid`
- `fork() -> ForkResult`, `spawn(cmd, args) -> Result<Child, ProcessError>`
- `Child::wait() -> Result<ExitStatus, ProcessError>`
- `kill(pid, signal) -> Result<bool, ProcessError>`
- Signal constants

**Exists today**: all of the above. Complete for first release.

#### `std.net`

TCP networking.

**Must-have APIs**:
- `TcpListener::bind`, `TcpListener::accept`
- `TcpStream::connect`, `TcpStream::write_all`, `TcpStream::read_all`
- `write`, `read`, `close`
- Typed `NetError` returns

**Exists today**: all of the above. Complete for first release.

#### `std.time`

Minimal time support.

**Must-have APIs**:
- `Duration` — `from_secs`, `from_millis`, `from_nanos`
- `Instant` — `now()`, `elapsed()`
- `sleep`
- `unix_timestamp`

**Exists today**: implemented.

#### `std.rand`

Deterministic random number generation.

**Must-have APIs**:
- `seed`, `random_int`, `random_range`

**Exists today**: implemented.

#### `std.args`

Command-line argument access.

**Must-have APIs**:
- `count() -> i32`, `get(index) -> String`

**Exists today**: implemented.

---

## Priority Gap Summary

Ranked by how many pressure programs are blocked or degraded:

| Priority | Gap | Programs affected | Roadmap item |
|----------|-----|-------------------|--------------|
| 1 | `std.numeric` endian read/write | All 5 parser pressure programs | 68 |
| 2 | `std.slice` checked access + subslice | All parser + fixed-capacity programs | 54 |
| 3 | `std.option` `unwrap_or` | All programs using ad hoc `ok: i32` fields | 65 |
| 4 | `std.result` `unwrap_or`, `map_err` | All error-propagation programs | 65 |
| 5 | `std.numeric` checked arithmetic | `pressure_parse_binary_endian.con` overflow handling | 58 |
| 6 | `std.bytes` `eq`, `slice` | Parser programs doing byte comparison | 55 |
| 7 | `std.vec` `insert`, `remove` | Ownership-heavy programs building ordered structures | 55 |
| 8 | `std.math` `min`, `abs`, `clamp` | Fixed-capacity programs | 55 |

---

## What Is Intentionally Out of Scope

These are deliberate exclusions, not forgotten items. Each has a reason tied to Concrete's design principles.

### Async / concurrency runtime

No async, no futures, no green threads, no channels, no `tokio`-style runtime. Concrete is abort-only with no unwinding. A concurrency model requires settling the failure strategy under concurrent execution, which is far beyond Phase 3.

**When it might land**: after the execution model and failure strategy are validated in single-threaded production use.

### Full Unicode

The stdlib provides ASCII classification and byte-level operations. Full Unicode (grapheme clusters, normalization, case folding beyond ASCII, bidirectional text) is out of scope. `String` is UTF-8 encoded, but the stdlib does not promise Unicode-aware operations beyond byte-level access and ASCII helpers.

**Why**: Unicode support is a large, specification-heavy surface. Shipping partial Unicode (as many languages do) creates more confusion than shipping honest ASCII-plus-bytes.

**When it might land**: as a separate `std.unicode` module, after the byte/text/string boundary is stable.

### Regex

No regex engine. Parser and validation code should use explicit match/cursor/state-machine patterns.

**Why**: regex engines are large, have complex performance characteristics, and hide control flow. They conflict with predictability and proof-friendliness. Zig made the same choice.

### Iterators / closures / higher-order collection APIs

No `map`, `filter`, `fold` on collections. No lazy iterator chains. No closures.

**Why**: Concrete has no closures and no trait objects. Higher-order collection APIs require at least one of those. Function pointers are available but ergonomically limited for chaining. The pressure programs demonstrate that explicit loops with match/if are readable and predictable. Adding iterator ceremony would not improve the code that exists.

**When it might land**: a small eager `std.iter` may land after closures or generic function-pointer ergonomics improve, if real programs justify it.

### Serialization framework (JSON, TOML, Protobuf, etc.)

No built-in serialization. The stdlib provides byte buffers, cursors, and endian readers. Format-specific parsing is user code or future packages.

**Why**: serialization frameworks are large and opinionated. The pressure programs show that parser code works well with `Cursor` + `Bytes` + endian helpers. A framework would not improve the core; it would bloat it.

### Allocator customization

No pluggable allocator interface. All allocation goes through libc malloc/free. The capability `with(Alloc)` marks allocation sites.

**Why**: allocator customization is useful for production systems work but requires settling the allocator-passing convention, arena lifetimes, and error handling. This is future work.

### Dynamic linking / plugin loading

No `dlopen`, no plugin system, no dynamic dispatch beyond function pointers.

### Cryptography beyond SHA-256

`std.sha256` exists as a building block. No TLS, no public-key crypto, no certificate handling. These belong in packages, not the stdlib.

### OS-specific APIs

The hosted layer assumes POSIX. No Windows-specific, macOS-specific, or Linux-specific APIs. No epoll, kqueue, io_uring, or platform-specific async I/O.

### Floating-point formatting / parsing

`std.fmt` and `std.parse` handle integers and booleans. Floating-point formatting and parsing are not yet implemented. The IEEE 754 formatting problem is large and the pressure programs do not require it.

### Trait-based formatting (Display, Debug)

No `Display` or `Debug` trait. Formatting is done through explicit `fmt` module calls and `append_int`-style methods. Trait-based formatting requires trait dispatch, which Concrete does not have.

### Package / dependency management

No `cargo`-like dependency resolution. The stdlib is a builtin dependency. External packages are future work (roadmap Phase after stdlib freeze).

---

## Mapping to Phase 2 Gap Findings

Each Phase 2 pressure category exposed specific stdlib gaps. This section maps those findings to the modules above.

### Parser/decoder pressure (item 45)

**Findings**: no string type, no byte cursor, no byte comparison, if/else chains instead of match on integers.

**Covered by**: `std.numeric` (endian APIs), `std.bytes` (eq, slice), `std.ascii` (character classification), `std.parse` (Cursor). The if/else-chain issue is a syntax concern (roadmap item 70), not a stdlib concern.

### Ownership-heavy structure pressure (item 46)

**Findings**: no recursive types (array-backed only), no generics (code duplication), linear types require explicit destroy/consume.

**Covered by**: `std.vec` (generic growable container replaces array-backed workarounds), `std.map`/`std.set`/`std.ordered_map` (generic keyed containers). Recursive types are a language feature, not a stdlib concern. The destroy/consume ceremony is the intended design — linear types should be explicit.

### Borrow/aliasing pressure (item 47)

**Findings**: no partial borrows, no iterator pattern.

**Not covered by stdlib**: partial borrows are a language feature. Iterator patterns require closures. The stdlib does not attempt to solve these in first release.

### FFI pressure (item 48)

**Findings**: no string passing to C, C struct interop requires manual layout.

**Partially covered by**: `std.string` (C-compatible internal representation), `std.bytes` (raw buffer for C interop). Full C struct interop and layout guarantees are roadmap item 69 (ABI contract), not a stdlib module.

### Fixed-capacity pressure (item 49)

**Findings**: no generics (duplicate per type), no const generics (capacity hardcoded), fixed-point arithmetic needs manual scaling.

**Partially covered by**: generic `Deque` replaces hand-rolled ring buffers. Const generics are a language feature, not a stdlib concern. Fixed-point arithmetic is out of scope for first release.

### Cleanup/leak pressure (item 50)

**Findings**: defer ordering verification is manual, no scope-guard abstraction.

**Not covered by stdlib in first release**: scope-guard is a pattern that requires closures or function-pointer ergonomics. Defer ordering is verified by the compiler's LIFO guarantee. The pressure programs show that `defer destroy(x)` / `defer x.drop()` works correctly.

---

## Validation Criteria

The stdlib target is validated when:

1. `examples/parse_validate/` works with builtin `Result`/`Option` instead of custom `ParseResult`-style enums.
2. `examples/service_errors/` works with builtin `Result` for all error propagation.
3. `examples/grep/` (or equivalent) uses `std.string`, `std.bytes`, and `std.fs` for a real text-search task.
4. `pressure_parse_dns_packet.con` can be rewritten using `std.numeric` endian readers instead of hand-rolled `read_u16_be`.
5. `pressure_fixcap_ring_buffer.con` can be replaced or simplified using `std.deque`.
6. All public APIs have docstrings.
7. All modules have at least one `#[test]` function that exercises the happy path.
