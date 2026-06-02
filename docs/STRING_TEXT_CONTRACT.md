# String and Text Encoding Contract

Status: reference with follow-up hardening notes (Phase 3 items 51-52 closed)

This document defines the encoding contract for strings and text in Concrete. It exists to prevent drift between the stdlib, docs, codegen, and FFI surfaces before the stable subset freeze.

For ownership and value semantics, see [VALUE_MODEL.md](VALUE_MODEL.md).
For stdlib module direction, see [STDLIB.md](STDLIB.md).
For FFI boundaries, see [FFI.md](FFI.md).
For checked indexing, see roadmap item 54 and [STDLIB.md](STDLIB.md) (checked indexing and slice views).

---

## 1. Encoding: UTF-8, Always Valid

Concrete strings are UTF-8 encoded byte sequences. A `String` value must contain well-formed UTF-8 at all times. There are no other string encodings in the language.

This is the same choice Rust and Go make. It is not the same as C (unspecified encoding) or Zig (byte arrays with optional UTF-8 convention).

**Invariant:** Every `String` value reachable through safe code contains valid UTF-8. Code that constructs a `String` from arbitrary bytes must validate or is responsible for maintaining the invariant (see section 6 on boundaries).

**Current reality:** `String` is currently `{ ptr: *mut u8, len: u64, cap: u64 }` and its methods operate at the byte level. `get(index)` returns a byte-oriented `char`, not a Unicode scalar value. `to_lower`/`to_upper` operate on ASCII ranges only. That byte-oriented behavior is the shipped first-release surface; stricter UTF-8 validation and richer text helpers remain follow-up hardening work around it.

---

## 2. Type Vocabulary

### `String` (owned, mutable, heap-allocated)

- Layout: `{ ptr: *mut u8, len: u64, cap: u64 }`
- Linear by default. Must be consumed exactly once (typically via `drop`).
- Allocation visible via `with(Alloc)`.
- Growable. Internal buffer may be reallocated on append.
- Contents must be valid UTF-8.

### `&String` (borrowed, immutable view of an owned String)

- Shared reference to an existing `String`.
- No allocation, no ownership transfer.
- Lifetime tied to the borrow scope.
- `&"literal"` borrows a global constant directly with no heap allocation. The backing `String` struct has `cap = 0` to signal it is not heap-owned.

### `Text` (borrowed, immutable text view)

- Layout: `{ ptr: *const u8, len: u64 }`
- Created from `&String` via `Text::from_string`.
- No ownership, no capacity field, no mutation.
- Intended as the lightweight read-only text view for functions that only need to inspect string content.

### `Bytes` (owned, mutable byte buffer)

- Layout: `{ ptr: *mut u8, len: u64, cap: u64 }`
- Linear by default. Must be consumed exactly once.
- Contains arbitrary bytes. No encoding guarantee.
- `Bytes::to_string` transfers ownership without copying but the caller is responsible for ensuring the bytes are valid UTF-8. This is a trust boundary.

### `char`

- Currently `i8` in LLVM IR (1 byte).
- Represents the first-release byte-oriented character/code-unit surface, not a Unicode scalar value.
- Renaming or widening it would be a later explicit surface revision, not something left ambient or unresolved.

---

## 3. Byte vs Text: Explicit Separation

Concrete maintains an explicit split between byte-level and text-level types. There is no implicit coercion between them.

| Type | Contains | Encoding guarantee | Use for |
|------|----------|--------------------|---------|
| `Bytes` | Arbitrary bytes | None | I/O buffers, binary data, network packets |
| `String` | UTF-8 bytes | Valid UTF-8 (target) | Text that will be printed, compared, searched |
| `Text` | UTF-8 bytes (borrowed) | Inherits from source `String` | Read-only text inspection |
| `[u8; N]` | Arbitrary bytes | None | Fixed-size buffers, wire formats |
| `*const u8` / `*mut u8` | Arbitrary bytes | None | Raw FFI, trusted internals |

### Conversion rules

- `Bytes` to `String`: Explicit `to_string()`. Caller asserts the bytes are valid UTF-8. No runtime validation in the current implementation; a validating conversion (`Bytes::to_string_checked` returning `Result<String, EncodingError>`) should exist before freeze.
- `String` to `Bytes`: Not yet implemented. Should be a zero-copy ownership transfer (the reverse of `to_string`).
- `&String` to `Text`: `Text::from_string(&s)`. Zero-copy, borrows the pointer.
- `Text` to `String`: Requires allocation (clone the viewed bytes into an owned buffer).
- There is no implicit `Bytes` to `String` or `String` to `Bytes` conversion. There is no implicit `[u8; N]` to `String` conversion.

---

## 4. String Literals

### Encoding

String literals in source code are UTF-8. The Lean-based compiler reads the source as a Lean `String` (which is internally a list of Unicode scalar values) and emits the literal as a null-terminated UTF-8 byte array in the LLVM global section.

### Escape sequences (currently supported)

| Escape | Meaning |
|--------|---------|
| `\n` | Newline (0x0A) |
| `\t` | Tab (0x09) |
| `\r` | Carriage return (0x0D) |
| `\\` | Backslash |
| `\"` | Double quote |
| `\0` | Null byte (0x00) |

Any other `\c` sequence currently passes through as the literal character `c`. This is lenient and should be tightened before freeze: unrecognized escape sequences should be a compile error.

### Not yet supported

| Feature | Status |
|---------|--------|
| `\xHH` (hex byte escape) | Not implemented. Should be added before freeze. |
| `\u{HHHH}` (Unicode scalar escape) | Not implemented. Should be added before freeze. |
| Raw strings (`r"..."` or `r#"..."#`) | Not implemented. Deferred. |
| Multi-line strings | Not implemented. Deferred. |
| String interpolation | Not implemented. Deferred. |

### Owned vs borrowed literals

| Form | Allocation | Ownership | Cleanup |
|------|------------|-----------|---------|
| `let s: String = "hello"` | Heap copy (malloc + memcpy) | Caller owns | Must drop |
| `&"hello"` | None (points at global constant) | Borrowed | No drop |

Borrowed literals (`&"..."`) are the preferred form for passing text to functions that take `&String`. They produce zero heap allocation and require no cleanup.

---

## 5. Null Termination

String literal globals are emitted with a trailing `\00` byte. The `len` field does not include the null terminator; the `cap` field does (for owned copies) or is `0` (for borrowed globals).

### Policy

- **Internal representation:** `String` is length-delimited, not null-terminated. The `len` field is authoritative. Embedded null bytes are permitted in the byte sequence.
- **Global constants:** Null-terminated in the LLVM IR for C interop convenience. This is a codegen detail, not a semantic guarantee.
- **Heap-allocated strings:** May or may not have a trailing null. Code that passes `String.ptr` to C functions requiring null termination (like `fopen`, `puts`) must ensure the buffer is null-terminated. The current `fs` module does this implicitly for `read_to_string` but not for all paths.
- **Follow-up hardening:** the stdlib should eventually provide an explicit `as_c_str` or `to_c_str` function that guarantees null termination, rather than relying on callers to know which strings happen to be null-terminated. This is part of the FFI boundary contract (item 75).

---

## 6. Invalid Sequence Handling

### At FFI boundaries

When receiving bytes from C code (file reads, network reads, environment variables, command-line arguments), Concrete currently treats the bytes as valid text without validation. This is wrong for a language that claims UTF-8 strings.

**Target policy:**

- `read_file` returns `Result<Bytes, FsError>`. The caller decides whether to interpret the bytes as text. This is correct.
- `read_to_string` returns `Result<String, FsError>`. It should validate UTF-8 and return an error on invalid sequences. Currently it does not validate.
- `get_args` returns heap-allocated strings from `argv`. It should validate or replace invalid sequences. Currently it does not validate.
- `env::get` returns `Option<String>`. Same: should validate or reject.

**Handling strategies (pick one per API before freeze):**

1. **Reject:** Return an error if the bytes are not valid UTF-8. Simplest. Correct for `read_to_string`.
2. **Replace:** Substitute U+FFFD for each invalid byte or sequence. Correct for best-effort display.
3. **Return bytes:** Give the caller `Bytes` and let them decide. Correct for `read_file`.

The stdlib should not silently accept invalid UTF-8 into a `String`. If it does, the encoding invariant is meaningless.

### At the source level

The compiler reads source files as Lean strings (Unicode). If a source file contains invalid UTF-8, Lean's parser will reject it before Concrete's lexer runs. This is fine.

### At runtime

If trusted or unsafe code constructs a `String` from invalid bytes (via `Bytes::to_string`, `String::from_raw`, or pointer manipulation), the UTF-8 invariant is violated. This is the caller's responsibility, contained within the `trusted`/`Unsafe` boundary. Safe code cannot construct an invalid `String` through safe APIs once the contract is enforced.

---

## 7. Comparison and Ordering

### Equality

`String::eq` performs byte-level comparison via `memcmp`. Two strings are equal if and only if they contain the same bytes in the same order.

This means:
- `"cafe\u{0301}"` (e + combining acute) and `"caf\u{00E9}"` (precomposed e-acute) are **not equal**, even though they render identically.
- This is the same behavior as Rust, Go, and Zig.
- Unicode normalization is not performed. This is intentional. See section 9.

### Ordering

No string ordering (`<`, `>`, `cmp`) is currently implemented. When added:

- **Default ordering shall be byte-level (lexicographic over UTF-8 bytes).** This is fast, deterministic, and consistent with the byte-equality semantics.
- Byte-level ordering of valid UTF-8 produces the same result as Unicode code point ordering. This is a property of UTF-8's design, not something Concrete needs to implement separately.
- Locale-sensitive or language-specific collation is out of scope. See section 9.

### Hashing

`hash::fnv1a_string` hashes the raw bytes. This is consistent with byte-level equality: equal strings produce equal hashes.

---

## 8. Indexing

`String::get(at: u64)` currently returns the byte at position `at`, cast to `char`. This is byte indexing, not character indexing.

**Target behavior before freeze:**

For a UTF-8 string type, byte indexing is the only O(1) operation and should remain the primitive. However, the API should be honest about what it returns:

- `get(at: u64) -> Option<u8>` -- return the byte, not a "char" that is actually a byte.
- A separate `chars()` iterator or `char_at` function that decodes UTF-8 should exist for character-level access, with O(n) cost made obvious.
- The interaction with the checked indexing contract (item 54) applies: `get` is checked (returns `Option`), `get_unchecked` is the raw fast path.

If `char` is kept as a single-byte type, then `get` returning `Option<char>` is byte indexing with misleading naming. If `char` is widened to a Unicode scalar, then `get` must decode. This is coupled to the `char` decision in section 2.

---

## 9. What Is Not Included

The following are explicitly out of scope for the first stable release. They are real problems but they require large, complex libraries that should not block the core language.

| Feature | Status | Rationale |
|---------|--------|-----------|
| Unicode normalization (NFC, NFD, NFKC, NFKD) | Deferred | Large tables, complex algorithm. Library concern. |
| Locale-aware collation | Deferred | ICU-scale dependency. Not a language primitive. |
| Regex | Deferred | Library, not language. |
| Full Unicode case mapping | Deferred | `to_lower`/`to_upper` are ASCII-only. Full Unicode case mapping requires large tables. |
| Grapheme cluster segmentation | Deferred | Required for correct text display but not for systems programming. Library concern. |
| String interpolation / format macros | Deferred | Useful but not required for the encoding contract. |
| `Encoding` trait or pluggable encodings | Rejected | Concrete has one encoding: UTF-8. No abstraction over encodings. |
| UTF-16 or UTF-32 string types | Rejected | Use `Bytes` for non-UTF-8 data. Convert at boundaries. |

This is the same stance as Rust (which defers normalization, collation, and regex to crates) and Go (which provides basic UTF-8 in the stdlib and defers the rest to packages). It is more restrictive than Python or Java, which embed Unicode deeply into the language. For a systems language, this is the right tradeoff.

---

## 10. Interaction With Other Contracts

### Checked indexing (item 55)

String indexing follows the same checked/unchecked split as arrays and slices:

- `get(at)` returns `Option<u8>` (checked, safe default)
- `get_unchecked(at)` returns `u8` (unchecked fast path, caller's responsibility)

The vocabulary should be consistent across `String`, `Bytes`, `Vec`, `Slice`, and any future view types.

### FFI and layout (item 75)

- `String` is not FFI-safe. It cannot be passed directly to C functions.
- Passing string data to C requires extracting the pointer (`s.ptr`) and ensuring null termination.
- An explicit `as_c_str` API should mediate this boundary.
- `#[repr(C)]` structs containing strings are not FFI-safe (the `String` layout is Concrete-internal, not C-compatible).
- The `ptr` field of a `String` pointing to a null-terminated buffer is C-compatible, but this is a raw pointer operation under `Unsafe` or `trusted`.

### Capability model

- `String::new()` does not allocate (zero-length, null pointer). No `Alloc` required.
- `String::push_char`, `String::append`, `String::clone`, and any growth operation require `with(Alloc)`.
- `String::drop` requires `with(Alloc)` (calls `dealloc`).
- I/O operations producing strings (`read_to_string`, `get_args`) require their respective capabilities.

---

## 11. Summary of Decisions

| Question | Decision |
|----------|----------|
| Internal encoding | UTF-8, always |
| Multiple encodings | No. One encoding. Convert at boundaries. |
| Owned string type | `String` (heap-allocated, linear, growable) |
| Borrowed text view | `&String` for borrow, `Text` for lightweight view |
| Byte buffer type | `Bytes` (no encoding guarantee) |
| Implicit byte-to-text coercion | No. Explicit conversion only. |
| Implicit text-to-byte coercion | No. Explicit conversion only. |
| String literal encoding | UTF-8 |
| String literal null termination | Yes in LLVM globals; not a semantic guarantee |
| Equality | Byte-level (`memcmp`) |
| Ordering | Byte-level (lexicographic) when added |
| Unicode normalization | Out of scope |
| Locale-aware operations | Out of scope |
| Invalid UTF-8 at FFI boundary | Reject or return `Bytes`; never silently accept into `String` |
| `char` type | Byte-oriented first-release surface. Any rename/widening is an explicit future revision. |
| Indexing | Byte-level O(1). Character-level access is separate and O(n). |

---

## 12. Follow-Up Hardening Questions After Freeze

1. **`char` follow-up.** The first release keeps the shipped byte-oriented `char` surface. If this proves too misleading in real workloads, the future question is whether to rename it or widen it under an explicit surface revision.

2. **`Bytes::to_string` validation.** Should there be both a checked (`to_string_checked -> Result`) and unchecked (`to_string` in trusted code) path? Current `to_string` is unchecked. At minimum the documentation must be honest about what it does.

3. **`read_to_string` validation.** Must validate UTF-8 before returning `String`. Currently does not.

4. **Null termination guarantee.** Should `String` always maintain a trailing null byte for C interop convenience (like Rust's `CString`)? Or should this remain a separate concern? The former adds one byte of overhead per string but simplifies FFI. The latter is simpler but requires explicit `as_c_str` at every FFI call.

5. **`Text` vs `&str`.** Is `Text` the right borrowed-view type, or should there be a more integrated borrowed string slice (like Rust's `&str`)? `Text` currently has no connection to the type system's borrow checker -- it is a plain struct with a raw pointer. A language-level borrowed slice would be safer but requires deeper compiler support.

6. **Escape sequence completeness.** `\xHH` and `\u{HHHH}` remain approved additions. Tightening unknown escapes into a compile error is follow-up lexer hardening, not a sign that the frozen surface is undefined.
