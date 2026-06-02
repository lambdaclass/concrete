# Stdlib API Review

Status: review (Phase 3, items 59/60)

This document audits the current stdlib for naming consistency, API shape issues, module layout problems, and violations of the design principles in [STDLIB_DESIGN_PRINCIPLES.md](STDLIB_DESIGN_PRINCIPLES.md). Each finding includes a concrete recommendation.

For the target module map, see [STDLIB_TARGET.md](STDLIB_TARGET.md).
For the core/hosted boundary, see [HOSTED_STDLIB_SPLIT.md](HOSTED_STDLIB_SPLIT.md).

---

## 1. Naming Consistency Audit

### 1.1 Verb consistency across modules

The design principles mandate "the same verb means the same thing everywhere" (Principle 2). The stdlib largely follows this. The table below records every public verb pattern and where it appears.

| Verb | Meaning per Principles | Modules that use it correctly | Violations |
|------|----------------------|-------------------------------|------------|
| `new` | Construct (no alloc) | `String`, `Vec`, `Bytes`, `Deque`, `BinaryHeap`, `HashMap`, `HashSet`, `OrderedMap`, `OrderedSet`, `BitSet`, `PathBuf`, `Cursor`, `Writer` | None |
| `with_capacity` | Construct + allocate | `Bytes`, `BitSet` | `Vec` is missing `with_capacity` |
| `len` | Query length | All container/string modules | `BitSet.len()` returns high-water mark, not popcount -- semantics differ from every other module (see 2.6) |
| `cap` | Query capacity | `String`, `Vec`, `Bytes` | `Deque`, `BinaryHeap`, `HashMap` have internal capacity but do not expose `cap()` |
| `is_empty` | Query emptiness | All containers | None |
| `get` | Checked access, returns `Option` | `String`, `Vec`, `Bytes`, `Deque`, `HashMap`, `OrderedMap`, `env.get`, `args.get` | `args.get` returns `String` (empty on OOB), not `Option<String>` -- breaks the pattern |
| `get_unchecked` | Unchecked access | `String`, `Vec`, `Bytes`, `Deque`, `Slice`, `MutSlice`, `Text` | None |
| `set` / `set_unchecked` | Checked/unchecked mutation | `Vec`, `Bytes`, `MutSlice` | None |
| `push` / `pop` | Add/remove from natural end | `Vec`, `Bytes`, `String` (`push_char`), `BinaryHeap`, `PathBuf` | `String` uses `push_char` instead of `push` -- inconsistent with `Vec.push` (see 1.2) |
| `push_front/back`, `pop_front/back` | Deque ends | `Deque` | None |
| `insert` / `remove` | Keyed or positional add/remove | `HashMap`, `HashSet`, `OrderedMap`, `OrderedSet` | `Vec` is missing `insert`/`remove` (documented as TODO) |
| `contains` | Membership test | `String`, `HashMap`, `HashSet`, `OrderedMap`, `OrderedSet` | None |
| `clear` | Reset to empty | All containers | None |
| `drop` | Explicit deallocation | All alloc-layer types | None |
| `clone` | Deep copy | `String` | Missing on `Vec`, `Bytes`, all other alloc types |
| `eq` | Value equality | `String`, `Text` | Missing on `Bytes`, `Vec` -- documented gap in STDLIB_TARGET |
| `fold` / `for_each` | Iteration via fn pointer | `Vec`, `HashMap`, `HashSet` | Missing on `Deque`, `OrderedMap`, `OrderedSet`, `BinaryHeap` |

### 1.2 Specific naming inconsistencies

**`push_char` vs `push`** (String module)

`Vec` uses `push(value: T)`. `Bytes` uses `push(byte: u8)`. `String` uses `push_char(c: char)`. Since `String` is conceptually a container of characters, `push` would be consistent. The `_char` suffix adds noise without disambiguation -- there is no `push_byte` on String.

Recommendation: Rename `String.push_char` to `String.push`. If a `push_byte` is later needed for raw byte appending, it can use the suffix. Until then, the unadorned verb is clearer.

**`append` vs `append_bytes`** (String vs Bytes)

`String` has `append(&String)`. `Bytes` has `append_bytes(&Bytes)`. The `_bytes` suffix on Bytes is redundant -- appending bytes to bytes is the only reasonable interpretation.

Recommendation: Rename `Bytes.append_bytes` to `Bytes.append` for consistency with `String.append`.

**`is_alnum` vs `is_alphanumeric`** (ascii module)

STDLIB_TARGET says the API should be `is_alphanumeric`. The implementation uses `is_alnum`. This is an abbreviation that violates the "guess the name" principle.

Recommendation: Rename `is_alnum` to `is_alphanumeric`.

**`process_exit`, `process_getpid`, `process_fork`, `process_kill`** (process module)

These are free functions with a `process_` prefix. This is a C-style convention, not a Concrete-style convention. Since they live in `std.process`, the module name already provides the namespace: users write `process.process_exit(1)`, which stutters.

Recommendation: Rename to `exit`, `getpid`, `fork`, `kill`. Callers import `std.process.{exit, getpid}` -- the module path provides context.

**`to_string` on Bytes** (bytes module)

`Bytes.to_string()` consumes the Bytes and returns a String by reinterpreting the buffer. The name is accurate but the operation is dangerous: it performs zero validation that the bytes are valid UTF-8. The design principles say "Bytes-to-String conversion is explicit and may fail" (Principle 4).

Recommendation: Rename to `into_string_unchecked` to signal that this is an unchecked ownership transfer. Add a `to_string() -> Option<String>` or `to_string() -> Result<String, E>` that validates UTF-8.

**`hash_raw` in sha256** (sha256 module)

Takes `*mut u8` and `u64` length -- raw pointer API. The public surface should prefer safe types.

Recommendation: Keep `hash_raw` as the low-level escape hatch (useful for FFI). Add `hash_bytes(&Bytes) -> Vec<u32>` alongside the existing `hash_string(&String)` for type safety.

**`encode_bytes` vs `encode_u32_vec` vs `encode_u32`** (hex module)

Three encoding functions with different naming patterns. `encode_bytes` takes `&Vec<u8>`, `encode_u32_vec` takes `&Vec<u32>`, `encode_u32` takes `(u32, &mut String)`.

Recommendation: Standardize as `encode_bytes(&Vec<u8>) -> String`, `encode_words(&Vec<u32>) -> String`, `encode_u32(u32, &mut String)`. The `_vec` suffix is unnecessary since the type signature already says Vec.

**Signal constants as functions** (process module)

`sig_int()`, `sig_kill()`, `sig_term()` are functions that return constants. This works but is ceremonial.

Recommendation: Keep as-is until Concrete supports `pub const`. Document that these are constant functions.

### 1.3 Type naming

| Type | Convention | Issue |
|------|-----------|-------|
| `HashMap<K, V>` | Correct | None |
| `HashSet<K>` | Correct | None |
| `OrderedMap<K, V>` | Correct | None |
| `OrderedSet<K>` | Correct | None |
| `BinaryHeap<T>` | Correct | None |
| `BitSet` | Correct | None |
| `TextFile` (io module) | Questionable | Overlaps with `File` in fs module. See 2.2 |
| `Cursor` (parse module) | Correct | None |
| `Writer` (writer module) | Correct | None |
| `Path` / `PathBuf` (path module) | Correct | Follows Rust convention |
| `Slice<T>` / `MutSlice<T>` | Correct | None |
| `Text` (text module) | Correct | None |

---

## 2. API Shape Issues

### 2.1 option and result: Missing essential combinators

**Current**: `Option<T>` has `is_some`, `is_none`. `Result<T, E>` has `is_ok`, `is_err`.

**Missing**: `unwrap_or(default: T) -> T` on both types. This is the most-needed combinator -- every program that uses Option or Result must write a full match expression for safe extraction. STDLIB_TARGET explicitly lists these as must-have.

**Also missing**: `map`, `and_then`, `map_err`. These are deferred until closures/fn-pointer ergonomics settle per STDLIB_TARGET, which is the correct decision.

Recommendation: Implement `unwrap_or` on both `Option<T>` and `Result<T, E>` as the highest-priority stdlib addition. These do not require closures.

### 2.2 io and fs: Overlapping file abstractions

**Current**: `std.io` defines `TextFile` with `create`, `open`, `write`, `read_byte`, `flush`, `close`. `std.fs` defines `File` with `open`, `create`, `write_bytes`, `read_bytes`, `seek`, `tell`, `close`. Both wrap `fopen`/`fclose`.

**Problem**: Two file types for the same underlying resource. A newcomer must learn which to use. `TextFile.write` takes `&String`; `fs.File.write_bytes` takes raw pointers. Neither is clearly better.

Recommendation: Remove `TextFile` from `std.io`. Consolidate file I/O in `std.fs.File`. Move `print`, `println`, `eprint`, `eprintln`, `read_line` to remain in `std.io` as console-only functions. This gives `std.io` a single job (console) and `std.fs` a single job (files).

### 2.3 writer: Overlapping with io print functions

**Current**: `std.writer.Writer` provides `str`, `char`, `int`, `bool`, `newline` methods. `std.io` provides `print`, `println`, `eprint`, `eprintln`.

**Problem**: Two ways to write to stdout/stderr. `Writer` is allocation-free for int formatting. `io.print` is the simple path. The overlap is not harmful yet, but the boundary should be documented.

Recommendation: Keep both. Document `Writer` as the allocation-free path for structured output (useful in `with(Console)` contexts where `with(Alloc)` is not available). Document `io.print`/`io.println` as the simple convenience path. Consider moving `Writer` into `std.io` since they serve the same domain.

### 2.4 string: `digit_to_int` and `int_to_digit` are misplaced

**Current**: `std.string` exports `digit_to_int<T>(char) -> T` and `int_to_digit<T>(T) -> char`. These are used by `std.fmt` and `std.parse`.

**Problem**: These are character/numeric conversion helpers, not string operations. They violate "one module, one job" (Principle 1).

Recommendation: Move `digit_to_int` and `int_to_digit` to `std.ascii`. They are character classification/conversion functions. `std.fmt` and `std.parse` would then import from `std.ascii`.

### 2.5 hash: `eq_*` functions belong elsewhere

**Current**: `std.hash` exports `eq_u64`, `eq_i32`, `eq_i64`, `eq_string`. These are equality functions, not hash functions.

**Problem**: Equality testing is not hashing. These functions exist to serve as function-pointer arguments to `HashMap` and `HashSet` constructors.

Recommendation: Keep in `std.hash` for now -- they are part of the "hash+eq pair for containers" vocabulary. Document them as "companion functions for HashMap/HashSet construction" rather than as hashing functions. If the stdlib grows, they should move to a future `std.cmp` module.

### 2.6 bitset: `len()` semantics differ from every other module

**Current**: `BitSet.len()` returns the high-water mark (largest bit ever set + 1), not the number of set bits. The number of set bits is `count()`.

**Problem**: Every other module's `len()` returns the number of elements. `BitSet.len()` returning the high-water mark is surprising. A user who writes `if b.len() > 0` to check if the bitset has any set bits will get wrong results after `unset()`.

Recommendation: Rename `len()` to `capacity()` or `high_water()`. Add `len()` as an alias for `count()` (number of set bits), or remove `len()` entirely and keep only `count()`.

### 2.7 test: `str_eq` duplicates `String.eq`

**Current**: `std.test.str_eq(a: &String, b: &String) -> bool` performs byte-by-byte string comparison. `String.eq(&self, other: &String) -> bool` does the same thing using `memcmp`.

**Problem**: Duplicate functionality. `str_eq` predates `String.eq` and should be removed.

Recommendation: Deprecate `std.test.str_eq`. Update `assert_str_eq` to call `a.eq(b)` internally. Keep `assert_str_eq` as a convenience that prints both strings on failure (once diagnostic formatting improves).

### 2.8 path: Free function `path_join` duplicates method

**Current**: `PathBuf` has `push(&mut self, segment: &String)` which appends a segment. There is also a free function `path_join(base: &Path, segment: &String) -> PathBuf`.

**Problem**: Two ways to join paths. `push` mutates in place; `path_join` creates a new PathBuf. The free function name uses an un-Concrete `path_` prefix.

Recommendation: Rename `path_join` to `join` (the module provides namespace). Or remove the free function entirely -- `PathBuf::from_string` + `push` covers the same use case.

### 2.9 Missing symmetry

| Module | Has | Missing | Priority |
|--------|-----|---------|----------|
| `Vec` | `push`, `pop` | `insert(at, value)`, `remove(at)` | High -- needed for ordered operations |
| `Vec` | `get`, `set` | `clone` | Medium |
| `Bytes` | `push`, `get`, `set` | `pop`, `eq`, `slice`, `clone` | High (`eq` blocks parser patterns) |
| `String` | `append` | `from_bytes` validated constructor | Medium |
| `Slice<T>` | `get_unchecked` | `get(at) -> Option<&T>` (checked) | High -- this is the core checked/unchecked gap |
| `MutSlice<T>` | `get_unchecked`, `set_unchecked` | Checked `get`, `set` | High |
| `Deque` | `push/pop front/back`, `get` | `cap()`, `fold`, `for_each` | Low |
| `HashMap` | `insert`, `get`, `remove` | `get_mut` (mutable value access) | Medium |
| `OrderedMap` | `insert`, `get`, `remove` | `get_mut`, `fold`, `for_each` | Medium |
| `Option` | `is_some`, `is_none` | `unwrap_or` | Critical |
| `Result` | `is_ok`, `is_err` | `unwrap_or` | Critical |

### 2.10 Hidden allocation violations (Principle 5)

**`HashMap::new`** allocates 16-slot backing arrays immediately. The design principles say `new()` should not allocate and `with_capacity(n)` should. This is a Principle 5 violation.

Recommendation: Make `HashMap::new` return a zero-capacity map (null pointers, cap=0). Add `HashMap::with_capacity(hash_fn, eq_fn, initial_cap)`. The first `insert` triggers allocation. This matches `Vec::new()`, `String::new()`, and `Bytes::new()`, all of which start at zero capacity.

**`HashSet::new`** has the same issue (delegates to `HashMap::new`).

### 2.11 Capability declaration gaps

**`Vec.get_mut`**: Returns `&mut T` via unsafe pointer cast but the method is not marked `trusted` and does not check bounds. Should either be bounds-checked (returning `Option<&mut T>`) or renamed to `get_mut_unchecked`.

**`Bytes.to_string`**: Performs ownership transfer of raw bytes to String without `with(Alloc)` despite the resulting String being a linear type requiring `drop`. The function itself does not allocate, but the naming does not signal the unchecked nature of the conversion.

---

## 3. Module Layout Issues

### 3.1 Modules that should be merged

**`io` and `writer`**: Both deal with console output. `Writer` is the structured, allocation-free path. `print`/`println` are the simple path. They share the domain (console I/O) and the capability (`Console`). Having them separate forces users to learn two import paths for the same task.

Recommendation: Merge `Writer` into `std.io`. The module has one job: console I/O. File I/O stays in `std.fs`.

### 3.2 Modules that should be split

**`io`**: Currently contains both console functions (`print`, `println`) and file operations (`TextFile`). These are different capabilities (`Console` vs `File`).

Recommendation: As noted in 2.2, remove `TextFile` from `io`. Keep `io` as console-only.

### 3.3 Core vs hosted classification

The current `lib.con` lists modules in a flat sequence. The layer classification exists only in documentation. The following modules are in the wrong conceptual position:

| Module | Listed after | Should be near | Reason |
|--------|-------------|----------------|--------|
| `hash` | `sha256` | `math`, `ascii` | `hash` is core-layer (pure computation), but listed after alloc-layer modules |
| `fmt` | `path` | `ascii`, `parse` | `fmt` is logically core (per HOSTED_STDLIB_SPLIT), but all its functions allocate Strings |
| `parse` | `fmt` | `ascii`, `math` | `parse` is fully core-compatible (pure computation over borrowed strings) |

Recommendation: Reorder `lib.con` to group modules by layer. Suggested order:

```
// Core layer
mod libc;     // internal, not user-facing
mod ptr;      // internal
mod option;
mod result;
mod math;
mod mem;
mod ascii;
mod slice;
mod test;
mod hash;
mod parse;

// Alloc layer
mod alloc;    // internal
mod string;
mod bytes;
mod text;
mod vec;
mod fmt;
mod hex;
mod sha256;
mod path;
mod map;
mod set;
mod deque;
mod heap;
mod ordered_map;
mod ordered_set;
mod bitset;

// Hosted layer
mod io;
mod writer;
mod fs;
mod env;
mod args;
mod rand;
mod time;
mod process;
mod net;
```

### 3.4 Internal modules exposed in lib.con

**`libc`** and **`ptr`** are internal implementation modules. `libc` exposes raw `extern fn` declarations. `ptr` has a single helper. Neither should be part of the public stdlib surface.

Recommendation: Mark `libc` and `ptr` as non-public (remove `pub` from their items, or add a `#[internal]` annotation if the language supports it). They should be importable by other std modules but not by user code.

### 3.5 `std.numeric` — implemented

`std.numeric` is implemented (891d561) with `ByteCursor`, `ByteWriter`, standalone endian functions (`read_be16_at`, `read_be32_at`, `read_le32_at`), and 9 tests. The `examples/packet/` rewrite onto ByteCursor validates the API surface.

---

## 4. Concrete Recommendations Summary

Ranked by impact (high to low):

### Critical (blocks real programs)

| # | Action | Module | Rationale |
|---|--------|--------|-----------|
| 1 | Add `unwrap_or` to `Option<T>` and `Result<T, E>` | option, result | Every program with Option/Result needs safe extraction |
| 2 | Implement `std.numeric` module | new | Endian reads, checked arithmetic -- blocks all parser programs |
| 3 | Add checked `get` to `Slice<T>` and `MutSlice<T>` | slice | Core checked/unchecked split is unfinished |
| 4 | Add `eq` to `Bytes` | bytes | Blocks byte comparison in parsers |

### High (naming consistency, API shape)

| # | Action | Module | Rationale |
|---|--------|--------|-----------|
| 5 | Rename `push_char` to `push` | string | Verb consistency with Vec, Bytes |
| 6 | Rename `append_bytes` to `append` | bytes | Verb consistency with String |
| 7 | Rename `is_alnum` to `is_alphanumeric` | ascii | Principle 2: guessable names |
| 8 | Rename `process_exit/getpid/fork/kill` to `exit/getpid/fork/kill` | process | Module path provides namespace |
| 9 | Rename `Bytes.to_string` to `into_string_unchecked` | bytes | Principle 4: explicit byte/text boundary |
| 10 | Make `HashMap::new` / `HashSet::new` zero-capacity | map, set | Principle 5: no hidden allocation |
| 11 | Add `insert(at, value)` and `remove(at)` to `Vec` | vec | Missing core operations |
| 12 | Add `pop` to `Bytes` | bytes | Symmetry with push |

### Medium (cleanup, organization)

| # | Action | Module | Rationale |
|---|--------|--------|-----------|
| 13 | Move `digit_to_int`/`int_to_digit` to `ascii` | string -> ascii | Principle 1: one module, one job |
| 14 | Remove `TextFile` from `io`; consolidate in `fs.File` | io, fs | Eliminate duplicate file abstraction |
| 15 | Merge `Writer` into `io` | writer -> io | Same domain (console output) |
| 16 | Rename `BitSet.len()` to `capacity()` or `high_water()` | bitset | `len()` semantics differ from all other modules |
| 17 | Deprecate `test.str_eq` | test | Duplicate of `String.eq` |
| 18 | Rename or remove `path_join` free function | path | Redundant with `PathBuf.push` |
| 19 | Reorder `lib.con` by layer | lib | Discoverability |
| 20 | Make `libc` and `ptr` non-public | libc, ptr | Internal implementation modules |
| 21 | Add `clone` to `Vec` and `Bytes` | vec, bytes | Basic completeness |
| 22 | Add `with_capacity` to `Vec` | vec | Consistent with `Bytes.with_capacity` |
| 23 | Change `args.get` to return `Option<String>` | args | Consistency with `get` pattern everywhere else |
| 24 | Make `Vec.get_mut` bounds-checked or rename to `get_mut_unchecked` | vec | Unchecked access should be named as such |

---

## 5. What Is Already Good

The audit also found significant areas where the stdlib follows its own principles well:

- **Ownership story is clear everywhere.** Every alloc-layer type has `new`, `drop`, and linear semantics. The `defer x.drop()` pattern is consistent.
- **Capability declarations are honest.** `with(Alloc)`, `with(Console)`, `with(Unsafe)`, `with(Network)`, `with(Process)`, `with(Random)` appear where they should. No hidden capability escalation was found.
- **Error handling is uniform.** `Result<T, E>` with module-specific error enums (`FsError`, `IoError`, `NetError`, `ProcessError`) is used consistently across all fallible hosted APIs.
- **Test coverage is broad.** Every module has `#[test]` functions. Core modules test happy paths and edge cases. Hosted modules test error paths (bad paths, refused connections, invalid PIDs).
- **The checked/unchecked naming convention is applied.** `get` vs `get_unchecked`, `set` vs `set_unchecked` are consistently named where both exist.
- **Container APIs are predictable.** `new`, `len`, `is_empty`, `clear`, `drop` appear on every container. The vocabulary is learnable.
- **`HashMap` and `HashSet` take explicit function pointers** for hash/eq. This follows Zig's pattern and avoids trait-based magic.
