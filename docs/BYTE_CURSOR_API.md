# Byte Cursor API

Status: design reference (Phase 3, item 65)

This document specifies the endian-aware byte cursor APIs for Concrete's stdlib. These APIs replace the hand-rolled byte-to-integer conversion patterns found in every parser pressure test with checked, library-first primitives. No bitfield syntax is proposed; the approach is plain functions on plain types.

For the module inventory that lists `std.numeric` as priority 1, see [STDLIB_TARGET.md](STDLIB_TARGET.md).
For the byte-vs-text separation, see [STRING_TEXT_CONTRACT.md](STRING_TEXT_CONTRACT.md).
For narrowing/widening cast rules, see [ARITHMETIC_POLICY.md](ARITHMETIC_POLICY.md).
For the existing text cursor, see `std.parse.Cursor`.
For checked indexing direction, see [STDLIB_TARGET.md](STDLIB_TARGET.md) (item 54).

---

## 1. The Problem

Every parser pressure test reimplements the same unsafe pattern: manual bit shifting to assemble multi-byte integers from raw byte arrays, with no bounds checking, no endian helpers, and no cursor abstraction.

### What the pressure tests show

**`pressure_parse_binary_endian.con`** defines four separate endian read functions and four separate endian write functions, all operating on `[u8; 256]` with raw index arithmetic:

```
fn read_u32_be(buf: [u8; 256], offset: i32) -> i32 {
    let b0: i32 = buf[offset] as i32;
    let b1: i32 = buf[offset + 1] as i32;
    let b2: i32 = buf[offset + 2] as i32;
    let b3: i32 = buf[offset + 3] as i32;
    return (b0 << 24) | (b1 << 16) | (b2 << 8) | b3;
}
```

This pattern has multiple problems:

- **No bounds checking.** If `offset + 3 >= 256`, the access is out of bounds. Nothing in the function signature prevents this.
- **Wrong return type.** The function returns `i32` but assembles a `u32` bit pattern. For values above `2^31` (like `0xDEADBEEF`), the result is a negative `i32`. The pressure test comments acknowledge this: "for 0xDEADBEEF (> 2^31), we use the raw bit pattern in i32."
- **Fixed buffer size.** The function only works with `[u8; 256]`. Each pressure test defines its own copy with its own buffer size (`[u8; 64]` in the DNS tests).
- **No cursor abstraction.** Every parser manually tracks an offset integer and passes it through every call. The HTTP request parser (`pressure_parse_http_request.con`) hand-rolls a complete `Cursor` struct with `peek`, `advance`, `advance_n`, and `remaining` -- 70 lines of infrastructure before any parsing begins.

**`pressure_parse_dns_header.con`** and **`pressure_parse_dns_packet.con`** both define their own `read_u16_be` (one uses `(hi << 8) | lo`, the other uses `hi * 256 + lo`). Two programs, same operation, different implementations, both unchecked.

**`examples/elf_header/`** avoids the problem entirely by extracting individual bytes through trusted pointer reads and passing them as `Int` to pure validation functions. The comment explains why: "Concrete does not yet have byte arrays in the proof path."

### The gap

This was identified as the **#1 priority gap** in [STDLIB_TARGET.md](STDLIB_TARGET.md):

> `std.numeric` endian read/write: All 5 parser pressure programs. Priority 1. Roadmap item 68.

The existing `std.parse.Cursor` operates on text (`&String`, character-level `peek`/`advance`). It is the right tool for text parsing but has no byte-level or endian-aware operations. Binary protocol parsing needs a different cursor that operates on raw `u8` data with multi-byte reads.

---

## 2. ByteCursor Type

### Definition

```
pub struct ByteCursor {
    data: *const u8,
    len: u64,
    pos: u64,
}
```

A `ByteCursor` is a borrowed, non-owning view into a byte buffer with an advancing read position. It does not allocate. It does not own the underlying data. It works with any byte source: `Bytes`, `[u8; N]` stack arrays, raw pointers from FFI.

### Design properties

**No allocation.** Construction and all read operations are allocation-free. A `ByteCursor` can be used in the core layer (no `with(Alloc)` required). This matters for embedded, freestanding, and proof-eligible contexts.

**Position advances on read.** Each successful `read_*` call advances `pos` by the number of bytes consumed. This eliminates the manual offset tracking that dominates the pressure tests. Failed reads do not advance position.

**Bounds checking on every read.** Every read method checks whether enough bytes remain before accessing the buffer. On insufficient data, the method returns `Result::Err` with a `CursorError`. No out-of-bounds access is possible through the public API.

**Immutable data.** The cursor borrows the data immutably. It cannot modify the underlying buffer. Write operations use a separate `ByteWriter` type (section 4).

### Construction

```
/// Create a cursor over a Bytes buffer.
pub fn from_bytes(b: &Bytes) -> ByteCursor

/// Create a cursor over a raw pointer and length.
/// Trusted: caller must ensure the pointer is valid for `len` bytes.
trusted pub fn from_raw(data: *const u8, len: u64) -> ByteCursor
```

For fixed-size arrays like `[u8; 256]`, construction goes through `from_raw`:

```
let buf: [u8; 256] = [0; 256];
let cur: ByteCursor = ByteCursor::from_raw(&buf as *const u8, 256);
```

This is a trust boundary: the caller asserts the pointer and length are valid. The cursor itself is safe after construction -- all subsequent operations are bounds-checked.

### Position queries

```
/// Current read position (bytes consumed so far).
pub fn pos(&self) -> u64

/// Number of bytes remaining after the current position.
pub fn remaining(&self) -> u64

/// True if no bytes remain.
pub fn is_empty(&self) -> bool
```

---

## 3. Endian Read API

All read methods return `Result<T, CursorError>`. On success, the cursor position advances by the number of bytes read. On failure, the position is unchanged.

### Single-byte read

```
pub fn read_u8(&mut self) -> Result<u8, CursorError>
```

### 16-bit reads

```
pub fn read_u16_be(&mut self) -> Result<u16, CursorError>
pub fn read_u16_le(&mut self) -> Result<u16, CursorError>
```

### 32-bit reads

```
pub fn read_u32_be(&mut self) -> Result<u32, CursorError>
pub fn read_u32_le(&mut self) -> Result<u32, CursorError>
```

### 64-bit reads

```
pub fn read_u64_be(&mut self) -> Result<u64, CursorError>
pub fn read_u64_le(&mut self) -> Result<u64, CursorError>
```

**Deferred:** Signed read variants (`read_i8`, `read_i16_be/le`, `read_i32_be/le`, `read_i64_be/le`) are not yet implemented. They will be added when a use case demands them. The unsigned variants cover all current pressure tests and examples.

### Raw byte reads

```
/// Skip `n` bytes without reading them. Advances position by `n`.
/// Returns the new position on success.
pub fn skip(&mut self, n: u64) -> Result<u64, CursorError>

/// Peek at the next byte without advancing position.
pub fn peek_u8(&self) -> Result<u8, CursorError>
```

**Deferred:** `read_bytes` (returns `Slice<u8>` sub-view) is not yet implemented.

### Implementation note

Each endian read is a thin wrapper over bounds-checked byte access plus bit shifting. For example, `read_u16_be` is:

```
pub fn read_u16_be(&mut self) -> Result<u16, CursorError> {
    if self.remaining() < 2 {
        return Result::<u16, CursorError>::Err { error: CursorError::UnexpectedEnd };
    }
    let hi: u16 = self.byte_at(self.pos) as u16;
    let lo: u16 = self.byte_at(self.pos + 1) as u16;
    self.pos = self.pos + 2;
    return Result::<u16, CursorError>::Ok { value: (hi << 8) | lo };
}
```

The `byte_at` helper is a trusted internal that dereferences `self.data + offset`. It is safe to call because the public API has already verified `offset < self.len`.

No LLVM intrinsics or platform-specific byte-swap instructions are required in the initial implementation. The shift-and-or pattern compiles to efficient code on all targets. Platform byte-swap optimization is a future concern.

---

## 4. Endian Write API

### ByteWriter type

```
pub struct ByteWriter {
    data: *mut u8,
    len: u64,
    cap: u64,
    pos: u64,
}
```

A `ByteWriter` is a positioned writer into a mutable byte buffer. Like `ByteCursor`, it tracks a position that advances on each write. It can wrap either a heap-allocated `Bytes` buffer or a fixed-capacity stack array.

### Construction

```
/// Create a writer over a mutable Bytes buffer.
/// Writes append starting at the current length; capacity must suffice
/// or the buffer grows (requires Alloc).
pub fn from_bytes(b: &mut Bytes) -> ByteWriter

/// Create a writer over a fixed-capacity raw buffer.
/// Trusted: caller must ensure the pointer is valid for `cap` bytes.
trusted pub fn from_raw(data: *mut u8, cap: u64) -> ByteWriter
```

### Write methods

All write methods return `Result<u64, CursorError>`. On success, the writer position advances and the new position is returned. On failure (not enough capacity), the position is unchanged.

**Implementation note:** The original design specified `Result<(), CursorError>` as the return type. The implementation returns the new position (`u64`) instead, which is more useful for callers that need to track write offsets and avoids a unit-type Result that must still be consumed (linear types). This is the settled API.

```
pub fn write_u8(&mut self, val: u8) -> Result<u64, CursorError>

pub fn write_u16_be(&mut self, val: u16) -> Result<u64, CursorError>
pub fn write_u16_le(&mut self, val: u16) -> Result<u64, CursorError>

pub fn write_u32_be(&mut self, val: u32) -> Result<u64, CursorError>
pub fn write_u32_le(&mut self, val: u32) -> Result<u64, CursorError>

pub fn write_u64_be(&mut self, val: u64) -> Result<u64, CursorError>
pub fn write_u64_le(&mut self, val: u64) -> Result<u64, CursorError>
```

**Deferred:** Signed write variants (`write_i8`, `write_i16_be/le`, `write_i32_be/le`, `write_i64_be/le`) and `write_bytes` are not yet implemented. They will be added when a use case demands them.

### Position queries

```
pub fn pos(&self) -> u64
pub fn remaining(&self) -> u64
pub fn written(&self) -> u64   // alias for pos
```

### Design decision: separate type, not methods on Bytes

The write API is a separate `ByteWriter` type rather than methods on `Bytes` for three reasons:

1. **Position tracking.** `Bytes.push` appends at the end. `ByteWriter` writes at an arbitrary advancing position, which supports overwriting, seek-and-patch patterns (write a length field, fill the body, go back and patch the length).
2. **Fixed-buffer support.** `ByteWriter::from_raw` works with stack arrays. `Bytes` requires heap allocation.
3. **Symmetry.** `ByteCursor` reads; `ByteWriter` writes. Same shape, same error handling, same position model. A function signature `fn encode(w: &mut ByteWriter)` mirrors `fn decode(r: &mut ByteCursor)`.

---

## 5. Fixed-Buffer Parsing Pattern

The primary use case for `ByteCursor` is allocation-free parsing of binary data from fixed-size stack buffers. This is how the pressure tests work today (with manual offset tracking) and how they should work with the cursor API.

### Example: DNS header from a fixed buffer

**Before (current pressure test code):**

```
fn read_u16_be(buf: [u8; 64], offset: i32) -> i32 {
    let hi: i32 = buf[offset] as i32;
    let lo: i32 = buf[offset + 1] as i32;
    return (hi << 8) | lo;
}

fn parse_dns_header(buf: [u8; 64], len: i32) -> DnsParseResult {
    if len < 12 {
        return DnsParseResult { header: empty_header(), error: 1 };
    }
    let id: i32 = read_u16_be(buf, 0);
    let raw_flags: i32 = read_u16_be(buf, 2);
    let qdcount: i32 = read_u16_be(buf, 4);
    let ancount: i32 = read_u16_be(buf, 6);
    let nscount: i32 = read_u16_be(buf, 8);
    let arcount: i32 = read_u16_be(buf, 10);
    // ... build DnsHeader ...
}
```

Problems: unchecked offsets, wrong types (`i32` for unsigned values), manual length validation, `read_u16_be` tied to `[u8; 64]`.

**After (with ByteCursor):**

```
fn parse_dns_header(buf: [u8; 64], len: u64) -> Result<DnsHeader, CursorError> {
    let mut cur: ByteCursor = ByteCursor::from_raw(&buf as *const u8, len);

    let id: u16 = match cur.read_u16_be() {
        Result::Ok { value } => value,
        Result::Err { error } => return Result::<DnsHeader, CursorError>::Err { error: error },
    };
    let raw_flags: u16 = match cur.read_u16_be() {
        Result::Ok { value } => value,
        Result::Err { error } => return Result::<DnsHeader, CursorError>::Err { error: error },
    };
    let qdcount: u16 = match cur.read_u16_be() {
        Result::Ok { value } => value,
        Result::Err { error } => return Result::<DnsHeader, CursorError>::Err { error: error },
    };
    let ancount: u16 = match cur.read_u16_be() {
        Result::Ok { value } => value,
        Result::Err { error } => return Result::<DnsHeader, CursorError>::Err { error: error },
    };
    let nscount: u16 = match cur.read_u16_be() {
        Result::Ok { value } => value,
        Result::Err { error } => return Result::<DnsHeader, CursorError>::Err { error: error },
    };
    let arcount: u16 = match cur.read_u16_be() {
        Result::Ok { value } => value,
        Result::Err { error } => return Result::<DnsHeader, CursorError>::Err { error: error },
    };

    let flags: DnsFlags = parse_flags(raw_flags as i32);

    return Result::<DnsHeader, CursorError>::Ok {
        value: DnsHeader {
            id: id as i32,
            flags: flags,
            qdcount: qdcount as i32,
            ancount: ancount as i32,
            nscount: nscount as i32,
            arcount: arcount as i32,
        }
    };
}
```

Improvements: correct types (`u16` for unsigned 16-bit values), bounds checking at every read, no manual offset tracking, `ByteCursor` works with any buffer size, the function signature communicates that parsing can fail.

**Note on verbosity.** The match-on-every-read pattern is verbose. This is the intended cost today -- Concrete does not have `?` or `try` syntax. When Result ergonomics improve (roadmap item 58), the same code will collapse to:

```
let id: u16 = cur.read_u16_be()?;
let raw_flags: u16 = cur.read_u16_be()?;
let qdcount: u16 = cur.read_u16_be()?;
// ...
```

The cursor API is designed for that future, but works without it.

### Example: binary protocol frame with mixed endianness

The `pressure_parse_binary_endian.con` test defines a 16-byte header with big-endian magic, big-endian sequence number, and little-endian payload length. With `ByteCursor`:

```
fn parse_frame(buf: [u8; 256], len: u64) -> Result<FrameHeader, CursorError> {
    let mut cur: ByteCursor = ByteCursor::from_raw(&buf as *const u8, len);

    let magic: u32 = match cur.read_u32_be() {
        Result::Ok { value } => value,
        Result::Err { error } => return Result::<FrameHeader, CursorError>::Err { error: error },
    };
    if magic != 0xDEADBEEF {
        return Result::<FrameHeader, CursorError>::Err {
            error: CursorError::InvalidData
        };
    }

    let version: u8 = match cur.read_u8() {
        Result::Ok { value } => value,
        Result::Err { error } => return Result::<FrameHeader, CursorError>::Err { error: error },
    };
    let flags: u8 = match cur.read_u8() {
        Result::Ok { value } => value,
        Result::Err { error } => return Result::<FrameHeader, CursorError>::Err { error: error },
    };
    let seq_num: u16 = match cur.read_u16_be() {
        Result::Ok { value } => value,
        Result::Err { error } => return Result::<FrameHeader, CursorError>::Err { error: error },
    };
    // Mixed endianness: payload length is little-endian
    let payload_len: u32 = match cur.read_u32_le() {
        Result::Ok { value } => value,
        Result::Err { error } => return Result::<FrameHeader, CursorError>::Err { error: error },
    };
    let checksum: u32 = match cur.read_u32_be() {
        Result::Ok { value } => value,
        Result::Err { error } => return Result::<FrameHeader, CursorError>::Err { error: error },
    };

    // ... validate and return ...
}
```

The magic number `0xDEADBEEF` is now correctly typed as `u32` (the value `3735928559`), not shoved into an `i32` as the negative value `-559038737`. The endianness of each field is explicit in the method name. Bounds checking happens automatically.

### Example: ELF header from a byte slice

The current `examples/elf_header/` extracts bytes one at a time through trusted pointer reads. With `ByteCursor`:

```
fn parse_elf_ident(data: *const u8, len: u64) -> Result<ElfIdent, CursorError> {
    let mut cur: ByteCursor = ByteCursor::from_raw(data, len);

    let magic0: u8 = match cur.read_u8() { ... };
    let magic1: u8 = match cur.read_u8() { ... };
    let magic2: u8 = match cur.read_u8() { ... };
    let magic3: u8 = match cur.read_u8() { ... };

    if magic0 != 0x7F { return err(CursorError::InvalidData); }
    if magic1 != 0x45 { return err(CursorError::InvalidData); }  // 'E'
    if magic2 != 0x4C { return err(CursorError::InvalidData); }  // 'L'
    if magic3 != 0x46 { return err(CursorError::InvalidData); }  // 'F'

    let class: u8 = match cur.read_u8() { ... };
    let data_encoding: u8 = match cur.read_u8() { ... };
    let version: u8 = match cur.read_u8() { ... };

    return Result::<ElfIdent, CursorError>::Ok {
        value: ElfIdent { class: class, data: data_encoding, version: version }
    };
}
```

The full ELF header (beyond the 7-byte identification) includes 16-bit and 32-bit fields whose endianness depends on `data_encoding` (byte 5). The cursor handles this naturally: after reading the identification, the parser selects `read_u16_be` or `read_u16_le` based on the encoding field.

---

## 6. Error Type

```
pub enum CursorError {
    /// Not enough bytes remaining for the requested read/write.
    UnexpectedEnd,
    /// Data was readable but violated a structural constraint
    /// (wrong magic, invalid version, etc.).
    InvalidData,
}
```

### Design rationale

**Two variants, not more.** Most cursor-level errors are either "ran out of bytes" or "the bytes were wrong." Higher-level errors (wrong magic number, unsupported version) are domain-specific and belong in the caller's error type, not in `CursorError`. The caller maps `CursorError` to their domain error.

**No position information in the error.** The cursor's `pos()` is available to the caller at the point of failure. Embedding position in the error would require either allocation (to format a message) or a larger error type. The caller can capture `cur.pos()` before the failing read if they need it for diagnostics.

### Integration with Result

`CursorError` is a Copy enum (both variants are fieldless). It works with `Result<T, CursorError>` without allocation. Pattern matching extracts the variant:

```
match cur.read_u32_be() {
    Result::Ok { value } => { /* use value */ },
    Result::Err { error } => {
        match error {
            CursorError::UnexpectedEnd => { /* handle truncation */ },
            CursorError::InvalidData => { /* handle bad data */ },
        }
    },
}
```

When Result ergonomics improve (item 58: `?` operator, `unwrap_or`, `map_err`), error propagation will be less verbose. The cursor API is designed to work well with those future additions without requiring changes.

---

## 7. Standalone Endian Functions

In addition to the cursor methods, the same endian operations are available as standalone functions in `std.numeric`. These operate on `Bytes`, `Slice<u8>`, or raw pointers at a given offset, without requiring a cursor.

```
/// Read a big-endian u16 from bytes at the given offset.
/// Returns None if offset + 2 > b.len().
pub fn read_u16_be_at(b: &Bytes, offset: u64) -> Option<u16>

/// Read a little-endian u32 from bytes at the given offset.
pub fn read_u32_le_at(b: &Bytes, offset: u64) -> Option<u32>

// ... same pattern for all widths and endiannesses ...
```

These standalone functions cover the case where a program needs a single read at a known offset without constructing a cursor. The cursor methods are implemented in terms of these functions internally.

---

## 8. Relationship to Existing Types

### ByteCursor vs std.parse.Cursor

| Property | `std.parse.Cursor` | `ByteCursor` |
|----------|-------------------|--------------|
| Input type | `&String` (text) | `*const u8` + length (bytes) |
| Unit of access | `char` (byte cast) | `u8`, `u16`, `u32`, `u64` |
| Endian support | None | Big-endian and little-endian reads |
| Error model | `Option<char>` on peek/advance | `Result<T, CursorError>` on every read |
| Use case | Text parsing (JSON, HTTP, config files) | Binary protocol parsing (DNS, ELF, wire formats) |
| Module | `std.parse` | `std.numeric` |

The two cursor types serve different domains and do not overlap. `std.parse.Cursor` is for text; `ByteCursor` is for binary data. There is no inheritance or trait relationship between them.

### ByteCursor vs Bytes

`Bytes` is an owned, heap-allocated, growable byte buffer. `ByteCursor` is a borrowed, non-owning, read-only view with a position. They complement each other:

- Read file bytes into `Bytes` (allocation, I/O).
- Create a `ByteCursor` over `&Bytes` (no allocation).
- Parse the binary content through cursor reads (no allocation).

### ByteWriter vs Bytes

`Bytes` supports `push` (append at end). `ByteWriter` supports positioned writes with endian encoding. For serialization:

- Create a `Bytes` buffer with `with_capacity`.
- Create a `ByteWriter` over `&mut Bytes`.
- Write fields through `write_u32_be`, `write_u16_le`, etc.

For fixed-buffer serialization (no allocation):

- Declare a stack array: `let buf: [u8; 64] = [0; 64]`.
- Create a `ByteWriter` over the raw pointer.
- Write fields. Check remaining capacity.

---

## 9. Module Placement

The cursor types and endian functions live in `std.numeric`, as specified in [STDLIB_TARGET.md](STDLIB_TARGET.md). The module provides three categories of functionality:

1. **Endian-aware byte reading and writing** (this document): `ByteCursor`, `ByteWriter`, standalone read/write functions.
2. **Checked arithmetic** (see [ARITHMETIC_POLICY.md](ARITHMETIC_POLICY.md)): `checked_add`, `checked_sub`, `checked_mul`.
3. **Wrapping arithmetic**: `wrapping_add`, `wrapping_sub`, `wrapping_mul` (compiler intrinsics, but `std.numeric` re-exports them for discoverability).

Import:

```
import std.numeric.{ByteCursor, ByteWriter, CursorError};
```

---

## 10. Checked Narrowing

The [ARITHMETIC_POLICY.md](ARITHMETIC_POLICY.md) specifies that `as` casts truncate silently and that checked narrowing belongs in a separate stdlib function. The byte cursor API interacts with narrowing in two places:

1. **Byte assembly.** `read_u32_be` assembles four `u8` values into a `u32`. This is widening (safe). No narrowing occurs inside the cursor.
2. **Caller downcasts.** After reading a `u16`, the caller may need to store it in an `i32` field (widening, safe) or check that it fits in a `u8` (narrowing, potentially lossy).

`std.numeric` provides checked narrowing helpers alongside the cursor:

```
/// Try to narrow a u64 to a u32. Returns None if the value does not fit.
pub fn try_narrow_u64_to_u32(val: u64) -> Option<u32>

/// Try to narrow a u32 to a u16. Returns None if the value does not fit.
pub fn try_narrow_u32_to_u16(val: u32) -> Option<u16>

/// Try to narrow a u16 to a u8. Returns None if the value does not fit.
pub fn try_narrow_u16_to_u8(val: u16) -> Option<u8>

/// Try to narrow a i64 to a i32. Returns None if the value does not fit.
pub fn try_narrow_i64_to_i32(val: i64) -> Option<i32>

// ... same pattern for other width combinations ...
```

These are the `try_narrow<T, U>` functions mentioned in [ARITHMETIC_POLICY.md](ARITHMETIC_POLICY.md) section 8, made concrete with explicit type pairs. Generic `try_narrow<T, U>` depends on trait-based dispatch that Concrete does not have; explicit pairs are the library-first approach.

---

## 11. Comparison with Other Languages

### Rust

Rust's ecosystem provides byte cursor functionality across three layers:

- **`std::io::Cursor<T>`**: wraps a `Vec<u8>` or `&[u8]`, implements `Read`/`Write`/`Seek`. General-purpose but requires the `std::io` trait machinery.
- **`byteorder` crate** (de facto standard): provides `ReadBytesExt` and `WriteBytesExt` traits that add `read_u16::<BigEndian>()` methods to any `Read` implementor. Uses generic endianness via phantom types.
- **`bytes` crate**: provides `Bytes` (reference-counted shared bytes) and `Buf`/`BufMut` traits with `get_u16`, `put_u32_le`, etc. Zero-copy slicing via reference counting.

Concrete's approach is closest to `byteorder`'s API surface but without traits: explicit `_be`/`_le` suffixes instead of generic endianness. This is intentional -- Concrete has no traits, and the suffix convention is clearer for readers who do not know what `BigEndian` resolves to.

### Go

Go uses free functions in `encoding/binary`:

```go
binary.BigEndian.Uint16(buf[0:2])
binary.LittleEndian.PutUint32(buf[4:8], value)
```

Plus `io.Reader`/`io.Writer` interfaces for streaming reads. `binary.Read` and `binary.Write` combine I/O with endian conversion.

Concrete's standalone functions (`read_u16_be_at`) are similar to Go's `BigEndian.Uint16`. The cursor type adds positioned reading that Go achieves through `bytes.Reader`.

### Zig

Zig provides low-level memory reads in `std.mem`:

```zig
const value = std.mem.readInt(u16, buf[0..2], .big);
std.mem.writeInt(u32, buf[4..8], value, .little);
```

Plus `std.io.FixedBufferStream` for cursor-like positioned reading/writing over a fixed buffer.

Concrete's cursor is closest to Zig's `FixedBufferStream`. Zig's `readInt` takes an endianness enum parameter; Concrete uses method-name suffixes. Both avoid allocation.

### Summary

| Feature | Concrete (proposed) | Rust (`byteorder`) | Go (`encoding/binary`) | Zig (`std.mem`) |
|---------|--------------------|--------------------|------------------------|-----------------|
| Endian selection | Method suffix (`_be`, `_le`) | Generic type parameter | Method receiver (`BigEndian.`) | Enum parameter (`.big`) |
| Cursor type | `ByteCursor` | `std::io::Cursor` | `bytes.Reader` | `FixedBufferStream` |
| Error model | `Result<T, CursorError>` | `io::Result<T>` | `error` return | `error` return |
| Allocation | None required | None for reads | None for reads | None |
| Write support | `ByteWriter` | `WriteBytesExt` trait | `binary.Write` | `FixedBufferStream` |

---

## 12. What This Does Not Include

### Bitfield syntax

No `packed struct`, no `bitfield { qr: 1, opcode: 4, aa: 1, ... }` syntax. Bitfield extraction is done through shift-and-mask on the values returned by cursor reads, exactly as the pressure tests do today. The shift-and-mask pattern is explicit, portable, and needs no new syntax.

If a future phase adds bitfield support, it will compose with cursor reads (read a `u16`, then extract fields from it), not replace them.

### Streaming I/O integration

`ByteCursor` does not implement a `Read` trait or integrate with `std.fs` file reading. The pattern is:

1. Read file/network bytes into a `Bytes` buffer (I/O layer).
2. Create a `ByteCursor` over the buffer (parsing layer).

Separating I/O from parsing keeps the cursor allocation-free and proof-eligible.

### Varint, LEB128, or other variable-length encodings

Variable-length integer encodings (Protocol Buffers varint, DWARF LEB128, UTF-8 sequence decoding) are not included in the initial API. They can be built on top of `read_u8` and `read_bytes`. If demand justifies it, they may be added as `read_varint` / `read_leb128` helpers in a future revision.

### Seek / rewind

The initial API does not support seeking to arbitrary positions or rewinding. Position only moves forward. This simplifies reasoning about cursor state and matches the linear parsing pattern of the pressure tests. If random-access reading is needed, standalone `read_u16_be_at` functions (section 7) serve that purpose without cursor state.

---

## 13. Implementation Sequence

1. **`CursorError` enum and `ByteCursor` struct** in `std/src/numeric.con`. Constructors, position queries, `read_u8`, `peek_u8`, `skip`.
2. **16-bit and 32-bit endian reads**: `read_u16_be`, `read_u16_le`, `read_u32_be`, `read_u32_le`, plus signed variants.
3. **64-bit endian reads**: `read_u64_be`, `read_u64_le`, plus signed variants.
4. **`read_bytes`**: returns `Slice<u8>` sub-view. Depends on `std.slice` checked access (item 54).
5. **`ByteWriter` struct**: constructors, position queries, all write methods.
6. **Standalone functions**: `read_u16_be_at`, `write_u32_le_at`, etc. on `Bytes` and raw pointers.
7. **Checked narrowing helpers**: `try_narrow_u32_to_u16`, etc.
8. **Tests**: round-trip tests for every width and endianness, bounds-failure tests, cursor position tracking tests.
9. **Rewrite pressure test**: port `pressure_parse_dns_packet.con` to use `ByteCursor` as the validation example from [STDLIB_TARGET.md](STDLIB_TARGET.md).

Steps 1-3 unblock all parser pressure programs. Steps 4-7 complete the module. Step 9 validates the API against real parsing code.

---

## 14. Open Questions

1. **Name: `ByteCursor` vs `ByteReader`?** `ByteCursor` emphasizes the positioned-view nature (consistent with `std.parse.Cursor`). `ByteReader` emphasizes the read-direction nature (consistent with `ByteWriter`). The pressure test `pressure_parse_http_request.con` already uses `Cursor` as its ad hoc name. This document uses `ByteCursor` for disambiguation from the text `Cursor`, but `ByteReader` is equally valid.

2. **Should `ByteWriter` support seek?** The initial design is append-only (position moves forward). But serialization sometimes needs to write a placeholder length, fill the body, then patch the length. This can be handled by returning a `pos` value and providing a `write_at(pos, val)` method, or by deferring seek to a future revision.

3. **Should reads on `Bytes` be methods or free functions?** Adding `bytes.read_u16_be(offset)` as a method on `Bytes` is ergonomic but mixes container concerns (storage) with parsing concerns (endian interpretation). Free functions in `std.numeric` keep the separation clean. This document proposes free functions; methods can be added later if the ergonomic cost proves too high.

4. **`CursorError` extensibility.** Two variants (`UnexpectedEnd`, `InvalidData`) cover the cursor layer. But users will want domain errors (e.g., `BadMagic`, `UnsupportedVersion`). Should `CursorError` have a `Custom { code: i32 }` variant, or should callers wrap `CursorError` in their own error enum? Wrapping is the library-first answer and avoids a grab-bag error type. This matches the approach in [STDLIB_TARGET.md](STDLIB_TARGET.md) where error conversion helpers are listed as a `std.result` concern.
