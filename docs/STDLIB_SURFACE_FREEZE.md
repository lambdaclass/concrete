# Stdlib Surface Freeze: First-Release Record

Status: authoritative freeze record (Phase 3, item 79)

Date: 2026-04-19

This document freezes the first-release stdlib surface. It records which modules, syntax forms, types, and capabilities are stable, which remain experimental, and which are intentionally deferred. It consolidates the design decisions from the Phase 3 stdlib/syntax-freeze work into a single reference.

Nothing in this document introduces new design. Every entry points back to the design document where the decision was made. The purpose is to draw a line: after this freeze, changes to the recorded surface require the explicit unfreeze process described in section 8.

For design rationale behind individual decisions, consult the referenced documents. This file records the decisions, not the reasoning.

---

## 1. What "Frozen" Means

A frozen surface item has the following properties:

1. **Removals are breaking.** A public function, type, module, syntax form, or capability that appears in the frozen record cannot be removed without an explicit unfreeze (section 8).

2. **Renames are breaking.** Changing the name of a frozen item is a removal plus an addition, and requires an unfreeze.

3. **Signature changes are breaking.** Changing the parameter types, return type, or capability requirements of a frozen function requires an unfreeze.

4. **Additions are permitted.** New public functions, types, modules, syntax forms, and capabilities may be added at any time, provided they follow the design principles in [STDLIB_DESIGN_PRINCIPLES.md](STDLIB_DESIGN_PRINCIPLES.md) and do not alter the semantics of existing frozen items.

5. **Bug fixes to behavior are permitted.** If a frozen function's implementation does not match its documented semantics, fixing the implementation is a bug fix, not an unfreeze.

6. **Internal changes are unconstrained.** Private functions, internal module structure, implementation strategies, and codegen details may change freely as long as the public surface and its documented behavior are preserved.

This is a pre-1.0 freeze. It does not promise backward compatibility across major releases. It promises that the team will not drift the supported surface without explicit, recorded justification.

---

## 2. Frozen Stdlib Modules

Organized by execution-model layer. For each module: name, one-line purpose, stability level, and key public API surface.

Stability levels:
- **stable**: frozen in this record. Changes require unfreeze.
- **experimental**: present in the stdlib but the API may change before release. Not covered by the freeze guarantee.
- **internal**: implementation module. Not intended for user import. May change or be removed without notice.

### 2.1 Core Layer (no host dependency, no allocation)

These modules are pure computation. No OS, no allocator, no libc.

#### `std.option` -- stable

Generic absence type.

| Item | Kind |
|------|------|
| `Option<T>` | enum: `Some { value: T }`, `None` |
| `is_some(&self) -> bool` | method |
| `is_none(&self) -> bool` | method |
| `unwrap_or(self, default: T) -> T` | method |
| `ok_or<E>(self, err: E) -> Result<T, E>` | method |

Source: [STDLIB_TARGET.md](STDLIB_TARGET.md), [STDLIB_AUDIT.md](STDLIB_AUDIT.md), [ERROR_HANDLING_DESIGN.md](ERROR_HANDLING_DESIGN.md)

#### `std.result` -- stable

Generic success/failure type.

| Item | Kind |
|------|------|
| `Result<T, E>` | enum: `Ok { value: T }`, `Err { error: E }` |
| `is_ok(&self) -> bool` | method |
| `is_err(&self) -> bool` | method |
| `unwrap_or(self, default: T) -> T` | method |
| `ok(self) -> Option<T>` | method |
| `err(self) -> Option<E>` | method |
| `?` operator (postfix) | syntax, desugars to match + early return |

Tier 2 helpers (`map`, `map_err`, `and_then`, `or_else`, `unwrap_or_else`) are approved additions pending function-pointer-in-generic validation. They are not frozen because they do not yet exist.

Source: [ERROR_HANDLING_DESIGN.md](ERROR_HANDLING_DESIGN.md)

#### `std.math` -- stable

Numeric helpers.

| Item | Kind |
|------|------|
| `max(a, b) -> T` | fn (all integer widths) |
| `min(a, b) -> T` | fn (all integer widths) |
| `clamp(val, lo, hi) -> T` | fn |
| `abs(value: i64) -> i64` | fn |

Source: [STDLIB_TARGET.md](STDLIB_TARGET.md), [STDLIB_AUDIT.md](STDLIB_AUDIT.md)

#### `std.ascii` -- stable

Byte-level character classification.

| Item | Kind |
|------|------|
| `is_digit`, `is_alpha`, `is_alphanumeric` | fn(u8) -> bool |
| `is_whitespace`, `is_upper`, `is_lower` | fn(u8) -> bool |
| `to_lower`, `to_upper` | fn(u8) -> u8 |

Source: [STDLIB_TARGET.md](STDLIB_TARGET.md)

#### `std.mem` -- stable

Compile-time memory queries.

| Item | Kind |
|------|------|
| `sizeof<T>() -> u64` | intrinsic |

`zeroed<T>()` is listed in STDLIB_TARGET but not yet implemented. It is an approved addition, not a frozen item.

Source: [STDLIB_TARGET.md](STDLIB_TARGET.md)

#### `std.test` -- stable

Test assertion helpers.

| Item | Kind |
|------|------|
| `assert_true`, `assert_false` | fn(bool) |
| `assert_eq`, `assert_ne` | fn(T, T) |
| `assert_gt`, `assert_lt`, `assert_ge`, `assert_le` | fn(T, T) |
| `str_eq`, `assert_str_eq` | fn(&String, &String) |

Note: `str_eq` is flagged for deprecation in [STDLIB_API_REVIEW.md](STDLIB_API_REVIEW.md) (duplicates `String.eq`). It is frozen for now but a candidate for removal in a future release.

Source: [STDLIB_TARGET.md](STDLIB_TARGET.md)

#### `std.hash` -- stable

Deterministic hashing and equality helpers for containers.

| Item | Kind |
|------|------|
| `fnv1a_bytes`, `fnv1a_string` | fn |
| `hash_u64`, `hash_i32`, `hash_i64`, `hash_string` | fn (typed hash helpers) |
| `eq_u64`, `eq_i32`, `eq_i64`, `eq_string` | fn (typed equality helpers) |

Source: [STDLIB_TARGET.md](STDLIB_TARGET.md)

#### `std.parse` -- stable

String-to-value parsing and text cursor.

| Item | Kind |
|------|------|
| `parse_int`, `parse_uint` | fn(&String) -> Option<T> |
| `parse_hex`, `parse_bin`, `parse_oct` | fn(&String) -> Option<T> |
| `parse_bool` | fn(&String) -> Option<bool> |
| `Cursor` | struct: positioned reader over `&String` |
| `Cursor::peek`, `advance`, `skip_whitespace`, `expect_char` | methods |

Source: [STDLIB_TARGET.md](STDLIB_TARGET.md)

#### `std.slice` -- experimental

Borrowed contiguous views.

| Item | Kind |
|------|------|
| `Slice<T>`, `MutSlice<T>` | struct |
| `len`, `is_empty` | methods |
| `get_unchecked` | method |

Checked `get(at) -> Option<&T>` is now implemented on both `Slice` and `MutSlice`. `subslice(start, end)` is still a gap. The module is experimental because the full API boundary is not yet settled.

Source: [STDLIB_TARGET.md](STDLIB_TARGET.md), [STDLIB_AUDIT.md](STDLIB_AUDIT.md)

#### `std.numeric` -- experimental (implemented)

Endian-aware byte reading, byte cursor, byte writer. 9 tests pass.

| Item | Kind |
|------|------|
| `ByteCursor` | struct: positioned reader over byte data |
| `ByteWriter` | struct: positioned writer into byte buffer |
| `CursorError` | enum: `UnexpectedEnd`, `InvalidData` |
| `read_u8`, `read_u16_be/le`, `read_u32_be/le`, `read_u64_be/le` | ByteCursor methods |
| `write_u8`, `write_u16_be/le`, `write_u32_be/le`, `write_u64_be/le` | ByteWriter methods |
| `read_bytes`, `skip`, `peek_u8` | ByteCursor methods |
| Standalone `read_u16_be_at`, `read_u32_le_at`, etc. | free functions |
| `checked_add`, `checked_sub`, `checked_mul` | fn (overflow-returning) |
| `wrapping_add`, `wrapping_sub`, `wrapping_mul` | compiler intrinsics |
| `saturating_add`, `saturating_sub`, `saturating_mul` | compiler intrinsics |
| `try_narrow_*` | fn (checked narrowing helpers) |

Implemented in `std/src/numeric.con`. ByteCursor and ByteWriter provide typed endian reads/writes. Validated by `examples/packet/` (uses `ByteCursor` for all header reads) and 9 unit tests in the module. Checked arithmetic and narrowing helpers are not yet implemented. Signed variants (`read_i16_be`, etc.) are deferred.

Source: [BYTE_CURSOR_API.md](BYTE_CURSOR_API.md), [ARITHMETIC_POLICY.md](ARITHMETIC_POLICY.md), [STDLIB_TARGET.md](STDLIB_TARGET.md)

### 2.2 Alloc Layer (requires malloc/free)

These modules allocate on the heap. All require `with(Alloc)` in their constructing/mutating APIs. All allocating types are linear.

#### `std.string` -- stable

Owned mutable UTF-8 string.

| Item | Kind |
|------|------|
| `String` | struct: `{ ptr, len, cap }` |
| `new`, `from_raw` | construction |
| `len`, `cap`, `is_empty` | queries |
| `get`, `get_unchecked` | checked/unchecked byte access |
| `push_char`, `append`, `append_int` | mutation |
| `eq`, `clone` | comparison, copy |
| `starts_with`, `ends_with`, `contains` | substring search |
| `to_lower`, `to_upper` | ASCII case conversion |
| `clear`, `drop` | cleanup |

Note: `push_char` is flagged for rename to `push` in [STDLIB_API_REVIEW.md](STDLIB_API_REVIEW.md) for verb consistency. Frozen under the current name; the rename is an approved future change.

Source: [STDLIB_TARGET.md](STDLIB_TARGET.md), [STRING_TEXT_CONTRACT.md](STRING_TEXT_CONTRACT.md)

#### `std.bytes` -- stable

Owned byte buffer.

| Item | Kind |
|------|------|
| `Bytes` | struct: `{ ptr, len, cap }` |
| `new`, `with_capacity` | construction |
| `push`, `len`, `cap`, `is_empty` | mutation and queries |
| `get`, `get_unchecked`, `set`, `set_unchecked` | access |
| `eq` | byte equality |
| `append_bytes` | append from another Bytes |
| `to_string` | ownership transfer (unchecked UTF-8; see open questions) |
| `clear`, `drop` | cleanup |

`slice` (sub-view returning `Slice<u8>`) is an approved addition, not yet implemented.

Source: [STDLIB_TARGET.md](STDLIB_TARGET.md), [STDLIB_AUDIT.md](STDLIB_AUDIT.md)

#### `std.text` -- experimental

Borrowed text view over `String`.

| Item | Kind |
|------|------|
| `Text` | struct: `{ ptr, len }` |
| `from_string`, `len`, `is_empty`, `get_unchecked`, `eq` | methods |

Experimental because the relationship between `Text`, `&String`, and the borrow checker is not fully settled. See open question on `Text` vs `&str` in [STRING_TEXT_CONTRACT.md](STRING_TEXT_CONTRACT.md).

Source: [STDLIB_TARGET.md](STDLIB_TARGET.md), [STRING_TEXT_CONTRACT.md](STRING_TEXT_CONTRACT.md)

#### `std.vec` -- stable

Generic growable array.

| Item | Kind |
|------|------|
| `Vec<T>` | struct |
| `new`, `push`, `pop` | core operations |
| `get`, `get_unchecked`, `get_mut` | access |
| `set`, `set_unchecked` | mutation |
| `len`, `cap`, `is_empty` | queries |
| `clear`, `drop` | cleanup |
| `fold`, `for_each` | iteration via fn pointer |

`insert(at, value)`, `remove(at)`, `with_capacity`, and `clone` are approved additions not yet implemented.

Source: [STDLIB_TARGET.md](STDLIB_TARGET.md), [STDLIB_API_REVIEW.md](STDLIB_API_REVIEW.md)

#### `std.fmt` -- stable

Pure formatting to strings.

| Item | Kind |
|------|------|
| `format_int`, `format_uint` | fn -> String |
| `format_hex`, `format_bin`, `format_oct` | fn -> String |
| `format_bool` | fn -> String |
| `pad_left`, `pad_right` | fn -> String |

Source: [STDLIB_TARGET.md](STDLIB_TARGET.md)

#### `std.path` -- stable

Path manipulation without filesystem effects.

| Item | Kind |
|------|------|
| `Path`, `PathBuf` | struct |
| `join`, `parent`, `file_name`, `extension` | methods |
| `to_string` | conversion |

Source: [STDLIB_TARGET.md](STDLIB_TARGET.md)

#### `std.map` -- stable

Hash map with explicit hash/eq function pointers.

| Item | Kind |
|------|------|
| `HashMap<K, V>` | struct |
| `new(hash_fn, eq_fn)` | construction |
| `insert`, `get`, `contains`, `remove` | core operations |
| `len`, `is_empty`, `clear`, `drop` | management |
| `fold`, `for_each`, `keys`, `values` | iteration |

Note: `new` currently allocates immediately. Flagged for change to zero-capacity in [STDLIB_API_REVIEW.md](STDLIB_API_REVIEW.md).

Source: [STDLIB_TARGET.md](STDLIB_TARGET.md)

#### `std.set` -- stable

Hash set (thin wrapper around HashMap).

Same shape as `std.map` minus value operations.

Source: [STDLIB_TARGET.md](STDLIB_TARGET.md)

#### `std.deque` -- stable

Double-ended queue / ring buffer.

| Item | Kind |
|------|------|
| `Deque<T>` | struct |
| `new`, `push_front`, `push_back`, `pop_front`, `pop_back` | core |
| `get`, `get_unchecked` | access |
| `len`, `is_empty` | queries |
| `clear`, `drop` | cleanup |

Source: [STDLIB_TARGET.md](STDLIB_TARGET.md)

#### `std.heap` -- stable

Min-heap / priority queue.

| Item | Kind |
|------|------|
| `BinaryHeap<T>` | struct |
| `new(cmp_fn)` | construction with explicit comparison |
| `push`, `pop`, `peek` | core |
| `len`, `is_empty`, `clear`, `drop` | management |

Source: [STDLIB_TARGET.md](STDLIB_TARGET.md)

#### `std.ordered_map` / `std.ordered_set` -- stable

Tree-based deterministic-iteration containers.

| Item | Kind |
|------|------|
| `OrderedMap<K, V>`, `OrderedSet<K>` | struct |
| `new(cmp_fn)` | construction |
| `insert`, `get`, `contains`, `remove` | core |
| `len`, `is_empty`, `clear`, `drop` | management |

Source: [STDLIB_TARGET.md](STDLIB_TARGET.md)

#### `std.bitset` -- stable

Bit array.

| Item | Kind |
|------|------|
| `BitSet` | struct |
| `new`, `with_capacity` | construction |
| `set`, `unset`, `test` | bit operations |
| `count` | popcount |
| `union`, `intersect` | set operations |
| `len`, `clear`, `drop` | management |

Note: `len()` semantics differ from other modules (returns high-water mark, not popcount). Flagged for rename in [STDLIB_API_REVIEW.md](STDLIB_API_REVIEW.md).

Source: [STDLIB_TARGET.md](STDLIB_TARGET.md)

#### `std.sha256` -- stable

SHA-256 cryptographic hash.

| Item | Kind |
|------|------|
| `hash_string`, `hash_raw` | fn |

Source: [STDLIB_TARGET.md](STDLIB_TARGET.md)

#### `std.hex` -- stable

Hex encoding/decoding.

| Item | Kind |
|------|------|
| `encode_bytes`, `encode_u32`, `encode_u32_vec` | fn |

Source: [STDLIB_TARGET.md](STDLIB_TARGET.md)

### 2.3 Hosted Layer (requires POSIX libc)

These modules require an operating system. They declare domain capabilities in their signatures.

#### `std.io` -- stable

Console I/O.

| Item | Kind | Capability |
|------|------|-----------|
| `print`, `println` | fn(&String) | `Console` |
| `print_int` | fn(Int) | `Console` |
| `eprint`, `eprintln` | fn(&String) | `Console` |
| `read_line` | fn() -> String | `Console`, `Alloc` |

Note: `TextFile` is flagged for removal in [STDLIB_API_REVIEW.md](STDLIB_API_REVIEW.md) (consolidate into `std.fs.File`). Frozen under current API; the consolidation is an approved future change.

Source: [STDLIB_TARGET.md](STDLIB_TARGET.md)

#### `std.fs` -- stable

File system operations.

| Item | Kind | Capability |
|------|------|-----------|
| `read_file`, `write_file`, `append_file` | fn -> Result | `File`, `Alloc` |
| `file_exists` | fn -> bool | `File` |
| `read_to_string` | fn -> Result<String, FsError> | `File`, `Alloc` |
| `File::open`, `File::create` | construction -> Result<File, FsError> | `File`, `Alloc` |
| `File::read_bytes`, `File::write_bytes` | methods | `File` |
| `File::seek`, `File::tell`, `File::close` | methods | `File` |
| `FsError` | enum: `OpenFailed`, `ReadFailed`, `WriteFailed`, etc. | -- |

Source: [STDLIB_TARGET.md](STDLIB_TARGET.md)

#### `std.env` -- stable

Environment variables.

| Item | Kind | Capability |
|------|------|-----------|
| `get(name) -> Option<String>` | fn | `Env`, `Alloc` |
| `set(name, value)` | fn | `Env` |
| `unset(name)` | fn | `Env` |

Source: [STDLIB_TARGET.md](STDLIB_TARGET.md)

#### `std.process` -- stable

Process control.

| Item | Kind | Capability |
|------|------|-----------|
| `process_exit`, `process_getpid` | fn | `Process` |
| `process_fork` | fn -> ForkResult | `Process` |
| `spawn(cmd, args) -> Result<Child, ProcessError>` | fn | `Process`, `Alloc` |
| `process_kill(pid, signal)` | fn | `Process` |
| Signal constants: `sig_int`, `sig_kill`, `sig_term` | fn -> i32 | -- |
| `ProcessError` | enum | -- |

Note: `process_` prefix on function names is flagged for removal in [STDLIB_API_REVIEW.md](STDLIB_API_REVIEW.md). Frozen under current names.

Source: [STDLIB_TARGET.md](STDLIB_TARGET.md)

#### `std.net` -- stable

TCP networking.

| Item | Kind | Capability |
|------|------|-----------|
| `TcpListener::bind` | construction | `Network`, `Alloc` |
| `TcpListener::accept` | method -> Result<TcpStream, NetError> | `Network` |
| `TcpStream::connect` | construction | `Network`, `Alloc` |
| `TcpStream::write`, `TcpStream::read` | methods | `Network` |
| `TcpStream::close`, `TcpListener::close` | methods | `Network` |
| `NetError` | enum | -- |

Source: [STDLIB_TARGET.md](STDLIB_TARGET.md)

#### `std.time` -- stable

Minimal time support.

| Item | Kind | Capability |
|------|------|-----------|
| `Duration::from_secs`, `from_millis`, `from_nanos` | construction | -- |
| `Instant::now`, `Instant::elapsed` | methods | `Clock` |
| `sleep` | fn | `Clock` |
| `unix_timestamp` | fn | `Clock` |

Source: [STDLIB_TARGET.md](STDLIB_TARGET.md)

#### `std.rand` -- stable

Deterministic random number generation.

| Item | Kind | Capability |
|------|------|-----------|
| `seed` | fn | `Random` |
| `random_int` | fn -> i64 | `Random` |
| `random_range` | fn(lo, hi) -> i64 | `Random` |

Source: [STDLIB_TARGET.md](STDLIB_TARGET.md)

#### `std.args` -- stable

Command-line argument access.

| Item | Kind | Capability |
|------|------|-----------|
| `count() -> i32` | fn | -- |
| `get(index) -> String` | fn | `Alloc` |

Note: `get` returns `String` (empty on out-of-bounds) instead of `Option<String>`. Flagged in [STDLIB_API_REVIEW.md](STDLIB_API_REVIEW.md) for consistency fix.

Source: [STDLIB_TARGET.md](STDLIB_TARGET.md)

### 2.4 Support Layer (internal, not user-facing)

#### `std.alloc` -- internal

Allocation primitives (`grow`, `dealloc`). Used by Vec, String, Bytes, HashMap internals.

#### `std.libc` -- internal

Raw libc FFI bindings. Should not be imported by user code. Candidate for `pub(pkg)` restriction when package management lands.

Source: [STDLIB_API_REVIEW.md](STDLIB_API_REVIEW.md), [VISIBILITY_AND_MODULE_HYGIENE.md](VISIBILITY_AND_MODULE_HYGIENE.md)

#### `std.ptr` -- internal

Pointer utilities. Single helper function.

#### `std.writer` -- internal (candidate for merge into `std.io`)

Allocation-free console writer. Flagged for merge into `std.io` in [STDLIB_API_REVIEW.md](STDLIB_API_REVIEW.md).

---

## 3. Frozen Syntax Forms

Every syntax construct in the language and its freeze status.

### 3.1 Stable (frozen, changes require unfreeze)

| Form | Description |
|------|-------------|
| `fn` | Function declaration with explicit parameter types and return type |
| `struct` / `struct Copy` | Struct declaration with named fields |
| `enum` / `enum Copy` | Enum declaration with named variants and fields |
| `impl` | Method implementation block |
| `trait` | Trait declaration |
| `match` | Exhaustive pattern matching on enums and integers |
| `let` / `let mut` | Variable binding with optional type annotation |
| `if` / `else` / `else if` | Conditional branching (requires `bool` expression) |
| `while` | Loop with condition |
| `for` | C-style `for (init; cond; step)` loop |
| `return` | Explicit function exit |
| `break` / `continue` | Loop control |
| `defer` | Deferred execution at scope exit (LIFO order) |
| `borrow` / `borrow mut` | Explicit borrow block with region and binding name |
| `import` | Selective named import with braced symbol list |
| `mod` | Module declaration |
| `pub` | Public visibility modifier |
| `trusted` | Trust boundary marker for unsafe internals |
| `extern fn` / `trusted extern fn` | FFI function declarations |
| `with(...)` | Capability declaration on function signatures |
| `?` | Postfix error propagation operator (desugars to match + early return) |
| `as` | Explicit type cast (widening, narrowing, reinterpretation) |
| `::` | Generic instantiation, enum/static qualification (`Type::Variant`, `Type::method(...)`), and module qualification |
| `.` | Module path, field access, method call |
| `&` / `&mut` | Borrow and mutable borrow in expressions and types |
| `*` (dereference) | Pointer/reference dereference |
| String literals (`"..."`) | UTF-8 string with escape sequences `\n`, `\t`, `\r`, `\\`, `\"`, `\0` |
| Char literals (`'c'`) | Single-character literal |
| Integer literals | Decimal, hex (`0x`), binary (`0b`), octal (`0o`) |
| Array literals (`[init; size]`) | Fixed-size array initialization |
| `#[test]` | Test function annotation |

### 3.2 Approved for implementation (not yet in the compiler)

| Form | Description | Design doc |
|------|-------------|------------|
| `let Type::Variant { fields } = expr;` | Irrefutable enum destructuring | [PATTERN_DESTRUCTURING.md](PATTERN_DESTRUCTURING.md) |
| `let StructType { fields } = expr;` | Irrefutable struct destructuring | [PATTERN_DESTRUCTURING.md](PATTERN_DESTRUCTURING.md) |
| `let Type::Variant { fields } = expr else { diverging_body };` | Refutable destructuring with else | [PATTERN_DESTRUCTURING.md](PATTERN_DESTRUCTURING.md) |
| Elaboration-time generic inference | Omit type params when context provides them | [SYNTAX_FREEZE_REVIEW.md](SYNTAX_FREEZE_REVIEW.md) #1, #4 |

These are designed, LL(1)-verified, and approved. They may be implemented before release. Once implemented, they become stable.

### 3.3 Explicitly deferred (NOT in the first release)

| Form | Reason | Reference |
|------|--------|-----------|
| `if let` | `let...else` covers the primary use case; deferred until demand | [PATTERN_DESTRUCTURING.md](PATTERN_DESTRUCTURING.md) |
| `while let` | Requires iterator protocol | [PATTERN_DESTRUCTURING.md](PATTERN_DESTRUCTURING.md) |
| Closures / anonymous functions | Permanent exclusion: hidden capture, hidden data flow | [ANTI_FEATURES.md](ANTI_FEATURES.md) |
| `async` / `await` | No async runtime; permanent for hidden runtimes | [ANTI_FEATURES.md](ANTI_FEATURES.md) |
| Macros | Permanent exclusion: breaks phase separation and auditability | [ANTI_FEATURES.md](ANTI_FEATURES.md) |
| Operator overloading | Permanent for first release: hidden effects | [ANTI_FEATURES.md](ANTI_FEATURES.md) |
| Default function arguments | Hidden parameters; deferred | [ANTI_FEATURES.md](ANTI_FEATURES.md) |
| String interpolation | Requires macros or special parser form; deferred | [SYNTAX_FREEZE_REVIEW.md](SYNTAX_FREEZE_REVIEW.md) |
| `for x in collection` | Requires iterator protocol; deferred | [SYNTAX_FREEZE_REVIEW.md](SYNTAX_FREEZE_REVIEW.md) #18 |
| Compound assignment (`+=`, `-=`) | Low priority sugar; deferred | [SYNTAX_FREEZE_REVIEW.md](SYNTAX_FREEZE_REVIEW.md) #20 |
| Guard clauses in match | Grammar ambiguity with `if`; deferred | [PATTERN_DESTRUCTURING.md](PATTERN_DESTRUCTURING.md) |
| Nested patterns in match/let | Complexity; deferred | [PATTERN_DESTRUCTURING.md](PATTERN_DESTRUCTURING.md) |
| `_` wildcard in destructuring | Can be added without grammar changes; deferred | [PATTERN_DESTRUCTURING.md](PATTERN_DESTRUCTURING.md) |
| Byte-string literals (`b"..."`) | Byte/string boundary not settled; deferred | [SYNTAX_FREEZE_REVIEW.md](SYNTAX_FREEZE_REVIEW.md) #8 |
| Raw strings (`r"..."`) | Deferred | [STRING_TEXT_CONTRACT.md](STRING_TEXT_CONTRACT.md) |
| Multi-line strings | Deferred | [STRING_TEXT_CONTRACT.md](STRING_TEXT_CONTRACT.md) |
| Pre/post conditions (`requires`/`ensures`) | Proof pipeline not mature; deferred | [ANTI_FEATURES.md](ANTI_FEATURES.md) |
| `comptime` evaluation | Blurs parse-time/run-time boundary; deferred | [ANTI_FEATURES.md](ANTI_FEATURES.md) |
| `pub(pkg)` visibility | Deferred until package management | [VISIBILITY_AND_MODULE_HYGIENE.md](VISIBILITY_AND_MODULE_HYGIENE.md) |
| Unifying `#` with `::` | Deferred pending user feedback | [SYNTAX_FREEZE_REVIEW.md](SYNTAX_FREEZE_REVIEW.md) #10 |

---

## 4. Frozen Type Vocabulary

### 4.1 Scalar types

| Type | Width | Signedness | Aliases |
|------|-------|------------|---------|
| `i8` | 8-bit | signed | -- |
| `i16` | 16-bit | signed | -- |
| `i32` | 32-bit | signed | -- |
| `i64` | 64-bit | signed | `Int` |
| `u8` | 8-bit | unsigned | -- |
| `u16` | 16-bit | unsigned | -- |
| `u32` | 32-bit | unsigned | -- |
| `u64` | 64-bit | unsigned | `Uint` |
| `f32` | 32-bit | IEEE 754 | `Float32` |
| `f64` | 64-bit | IEEE 754 | `Float64` |
| `bool` | 1-bit | -- | -- |
| `char` | 8-bit | -- | Byte-oriented first-release surface; see section 6 |

Source: [ARITHMETIC_POLICY.md](ARITHMETIC_POLICY.md)

### 4.2 String / text / byte types

| Type | Encoding | Ownership | Layer |
|------|----------|-----------|-------|
| `String` | UTF-8 | Owned, linear | Alloc |
| `Text` | UTF-8 (borrowed) | Borrowed | Alloc (borrows from String) |
| `Bytes` | Raw bytes | Owned, linear | Alloc |
| `[u8; N]` | Raw bytes, fixed | Copy, stack | Core |

Source: [STRING_TEXT_CONTRACT.md](STRING_TEXT_CONTRACT.md), [STDLIB_DESIGN_PRINCIPLES.md](STDLIB_DESIGN_PRINCIPLES.md) Principle 4

### 4.3 Generic types

| Type | Purpose | Layer |
|------|---------|-------|
| `Option<T>` | Absence | Core |
| `Result<T, E>` | Success/failure | Core |
| `Vec<T>` | Growable array | Alloc |
| `Slice<T>` | Immutable borrowed view | Core |
| `MutSlice<T>` | Mutable borrowed view | Core |
| `HashMap<K, V>` | Hash map | Alloc |
| `HashSet<K>` | Hash set | Alloc |
| `OrderedMap<K, V>` | Sorted map | Alloc |
| `OrderedSet<K>` | Sorted set | Alloc |
| `Deque<T>` | Double-ended queue | Alloc |
| `BinaryHeap<T>` | Priority queue | Alloc |
| `BitSet` | Bit array | Alloc |

### 4.4 Fixed-size arrays

| Form | Meaning |
|------|---------|
| `[T; N]` | Fixed-size array of `N` elements of type `T` |

### 4.5 Reference and pointer types

| Type | Meaning |
|------|---------|
| `&T` | Shared (immutable) reference |
| `&mut T` | Mutable reference |
| `*const T` | Raw immutable pointer (FFI, trusted code) |
| `*mut T` | Raw mutable pointer (FFI, trusted code) |

### 4.6 Function pointer types

| Form | Meaning |
|------|---------|
| `fn(T) -> U` | Function pointer (no capture, C-compatible) |

---

## 5. Frozen Capability Vocabulary

The capability set is fixed. Each capability maps to a class of host resource or trust boundary.

| Capability | Controls | Required for |
|-----------|----------|-------------|
| `File` | Filesystem access | `std.fs` operations |
| `Network` | Socket operations | `std.net` operations |
| `Clock` | Time queries, sleep | `std.time` operations |
| `Env` | Environment variables | `std.env` operations |
| `Random` | RNG state | `std.rand` operations |
| `Process` | Fork, spawn, kill, exit | `std.process` operations |
| `Console` | Stdout/stderr | `std.io` print operations |
| `Alloc` | Heap allocation | Construction/mutation of Vec, String, Bytes, HashMap, etc. |
| `Unsafe` | Raw FFI calls | `extern fn` calls (contained in `trusted` wrappers) |
| `Std` | Convenience alias | Union of all capabilities above |

Rules:
- A function can only call functions whose capabilities are a subset of its own.
- `--report caps` shows the capability set of every function.
- `--report authority` shows transitive capability propagation.
- Core-layer functions have `caps: (pure)` -- no capabilities.
- No new capabilities may be added without an unfreeze.

Source: [HOSTED_STDLIB_SPLIT.md](HOSTED_STDLIB_SPLIT.md), [ANTI_FEATURES.md](ANTI_FEATURES.md)

---

## 6. Post-Freeze Follow-Up Questions and Approved Additions

Phase 3 is closed. The items below are no longer "must resolve before release" blockers; they are the follow-up questions, hardening steps, and approved additions that may still grow around the frozen surface.

### 6.1 Byte/text hardening

- `char` is frozen today as the shipped 8-bit character/byte surface. Renaming it or widening it to a Unicode scalar would be an explicit future surface revision, not an unrecorded drift.
- `Bytes::to_string` validation, `read_to_string` UTF-8 validation, stricter escape handling, and explicit C-string helper APIs remain worthwhile hardening work around the frozen text boundary. They are approved additions or tightening work, not evidence that the current surface is undefined.

### 6.2 Numeric and arithmetic follow-up

- `std.numeric` is shipped: `ByteCursor`, `ByteWriter`, endian reads/writes, and the parser-facing binary helpers are part of the current surface.
- Signed cursor helpers, additional narrowing helpers, and any shift from today's arithmetic implementation to fully enforced checked-by-default semantics remain explicit follow-up work around the published arithmetic policy.

### 6.3 Collection and slice polish

- Checked `get` on `Slice<T>` and `MutSlice<T>` is shipped.
- Additional slice/view APIs (`subslice`, broader view helpers) and allocation-visibility cleanups like a lazier `HashMap::new` remain post-freeze API polish, not missing definition of the frozen core.

### 6.4 Result/Option Tier 2 helpers

- Tier 1 helpers are frozen and shipped.
- Tier 2 helpers (`map`, `map_err`, `and_then`, `or_else`, `unwrap_or_else`) remain approved additions pending function-pointer-in-generic validation. Their absence is deliberate and documented.

---

## 7. Explicitly Deferred Features

The following are NOT in the first release. Each exclusion is a deliberate design decision, not an oversight.

### 7.1 Permanent exclusions (load-bearing constraints)

These cannot be added without rethinking fundamental language invariants.

| Feature | Why excluded |
|---------|-------------|
| Garbage collection | Hides memory behavior; breaks predictable profile |
| Hidden async runtime / event loop | Hidden concurrency and scheduling |
| Exceptions / unwinding | Hidden control flow; breaks abort-only model |
| Closures / anonymous functions | Hidden capture and data flow |
| Source-generating macros | Breaks phase separation and auditability |
| Trait objects / dynamic dispatch | Opaque call targets; breaks static analysis |
| Implicit numeric conversions | Hides data loss; breaks proof model |
| Implicit string conversions | Hides allocation |
| Implicit bool (truthy/falsy) | Ambiguous conditionals |
| Hindley-Milner / global type inference | Worse diagnostics; breaks phase separation |
| Implicit trait resolution | Action-at-a-distance |
| Class inheritance | Implicit dispatch chains |
| Glob imports (`import mod.*`) | Hides where names come from |
| C-style variadic functions | Type-unsafe |

### 7.2 Deferred features (may be reconsidered with evidence)

| Feature | Condition for reconsideration |
|---------|-------------------------------|
| `if let`, `while let` | Post-release demand; `let...else` proves insufficient |
| `for x in collection` | Iterator protocol design compatible with no-closures, no-dyn-dispatch |
| Compound assignment (`+=`) | Sustained post-release feedback |
| Pre/post conditions | Proof pipeline covers broader subset |
| Ghost code / specification types | Erasure discipline designed |
| Comptime evaluation | Clear phase separation story |
| Default function arguments | Real-program pressure demonstrates need |
| Operator overloading (narrow) | Effect-transparent design for math types |
| Allocator customization | Arena/allocator-passing convention designed |
| Full Unicode (`std.unicode`) | After byte/text/string boundary is stable |
| Regex | Library, not language |
| Serialization framework | Library, not language |
| Iterators / lazy collection APIs | After closures or fn-ptr ergonomics improve |
| Dynamic linking / plugins | After capability model handles cross-boundary trust |
| Floating-point formatting/parsing | IEEE 754 formatting is large; demand must justify |
| Concurrency (threads, channels) | Failure strategy under concurrent execution settled |
| Package / dependency management | Phase J design |
| Byte-string literals (`b"..."`) | Byte/string boundary design settled |
| String interpolation | Requires macros (permanently excluded) or special form |
| `pub(pkg)` visibility | Package management lands |

Full rationale for each: [ANTI_FEATURES.md](ANTI_FEATURES.md), [STDLIB_TARGET.md](STDLIB_TARGET.md) (out-of-scope section).

---

## 8. Change Process: How to Propose Unfreezing a Surface

Unfreezing a frozen surface item requires all of the following:

### 8.1 Written proposal

A markdown document in `docs/unfreezes/` that contains:

1. **What is being unfrozen.** The exact module, function, type, syntax form, or capability.
2. **Why.** What real-program evidence (not hypothetical convenience) demonstrates the change is necessary. Must cite at least one concrete example or pressure test.
3. **What replaces it.** If a removal or rename, what the replacement is and how existing code migrates.
4. **Compatibility impact.** Every `.con` file in the repository that would need to change.
5. **Design-principle impact.** Which principles from [STDLIB_DESIGN_PRINCIPLES.md](STDLIB_DESIGN_PRINCIPLES.md) are affected and how the proposal preserves them.

### 8.2 Review

The proposal must be reviewed and accepted before implementation. The reviewer checks:

- Is the evidence from real code, not hypothetical?
- Is the migration path mechanical (greppable, automatable)?
- Does the change preserve the design principles?
- Does the change break any existing tests?

### 8.3 Migration

The change includes:

- Updates to all affected `.con` files in the repository.
- Updates to this freeze document (`STDLIB_SURFACE_FREEZE.md`).
- Updates to any referenced design documents.
- A CHANGELOG entry.

### 8.4 Additions do not require unfreeze

Adding a new public function, type, module, or syntax form does not require the unfreeze process. It requires only:

1. The addition follows the design principles in [STDLIB_DESIGN_PRINCIPLES.md](STDLIB_DESIGN_PRINCIPLES.md).
2. The addition is documented (function-level doc comments, module-level if it is a new module).
3. The addition has at least one test.
4. This freeze document is updated to include the new item.

---

## 9. Validation Status

Per [STDLIB_VALIDATION_PLAN.md](STDLIB_VALIDATION_PLAN.md), the following canonical examples must be validated against the stdlib surface before the first release ships:

| Example | Status | Dependency |
|---------|--------|-----------|
| `examples/parse_validate/` | Required: rewrite with `Result<T, E>` and `?` | `Result.unwrap_or` (done), `?` (done) |
| `examples/service_errors/` | Required: rewrite with `Result` + `map_err` + `?` | `Result.map_err` (Tier 2, pending) |
| `examples/grep/` | Required: use `std.ascii`, `String` methods, remove magic constants | Existing stdlib surface |
| `examples/fixed_capacity/` | Required: replace ad hoc `ValidateResult` with builtin `Result<T, E>` | Existing stdlib surface |
| `examples/elf_header/` | Nice-to-have: use `ByteCursor` | `std.numeric` (implemented, 891d561) |
| `examples/packet/` | Required: rewrite with `ByteCursor` + `ByteWriter` | `std.numeric` (implemented, 891d561) |

Per [STDLIB_DOCUMENTATION_PLAN.md](STDLIB_DOCUMENTATION_PLAN.md), at least Tier 1 and Tier 2 modules (priorities 1-12) must be fully documented before the first release.

---

## 10. Module Map (as declared in `std/src/lib.con`)

Current module list from `std/src/lib.con`, annotated with layer and stability:

```
mod std {
    // --- Internal ---
    mod libc;          // internal: raw libc FFI bindings
    mod ptr;           // internal: pointer utilities

    // --- Core Layer ---
    mod option;        // stable: Option<T>
    mod result;        // stable: Result<T, E>
    mod math;          // stable: min, max, clamp, abs
    mod mem;           // stable: sizeof
    mod ascii;         // stable: character classification
    mod slice;         // experimental: Slice<T>, MutSlice<T>
    mod test;          // stable: assertions
    mod hash;          // stable: FNV-1a, typed helpers
    mod parse;         // stable: parse_int, Cursor

    // --- Alloc Layer ---
    mod alloc;         // internal: allocation primitives
    mod string;        // stable: String
    mod bytes;         // stable: Bytes
    mod text;          // experimental: Text (borrowed view)
    mod vec;           // stable: Vec<T>
    mod fmt;           // stable: format_int, pad_left
    mod hex;           // stable: encode_bytes
    mod sha256;        // stable: hash_string
    mod path;          // stable: Path, PathBuf
    mod map;           // stable: HashMap<K, V>
    mod set;           // stable: HashSet<K>
    mod deque;         // stable: Deque<T>
    mod heap;          // stable: BinaryHeap<T>
    mod ordered_map;   // stable: OrderedMap<K, V>
    mod ordered_set;   // stable: OrderedSet<K>
    mod bitset;        // stable: BitSet

    // --- Hosted Layer ---
    mod io;            // stable: print, println
    mod writer;        // internal: allocation-free Writer (merge into io)
    mod fs;            // stable: File, read_file, write_file
    mod env;           // stable: get, set, unset
    mod args;          // stable: count, get
    mod rand;          // stable: seed, random_int
    mod time;          // stable: Duration, Instant, sleep
    mod process;       // stable: fork, spawn, kill, exit
    mod net;           // stable: TcpListener, TcpStream

    mod numeric;       // experimental: ByteCursor, endian, checked arithmetic helpers
}
```

---

## 11. Summary

This freeze records:

- **The current stdlib module set** in `std/src/lib.con`, including shipped experimental modules such as `slice`, `text`, and `numeric`, plus the explicitly internal support modules.
- **24 stable syntax forms**. 4 forms approved for implementation. 17+ forms explicitly deferred.
- **15 frozen type families** spanning scalars, strings, generics, arrays, references, and function pointers.
- **10 frozen capabilities** covering file, network, clock, env, random, process, console, allocation, unsafe, and the std alias.
- **A small set of documented post-freeze follow-up questions** around text hardening, arithmetic enforcement, and Tier 2 helper growth.
- **14 permanent exclusions** and **20 deferred features** with documented conditions for reconsideration.
- **A change process** requiring written proposal, review, and migration for any frozen surface modification.

The stdlib is small by design. It does not try to be comprehensive. It tries to be coherent, tested, documented, and honest about its limitations. This freeze ensures that the surface recorded here will not drift without explicit, justified change.
