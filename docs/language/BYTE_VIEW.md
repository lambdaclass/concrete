# Owned Byte Views

Status: IMPLEMENTED — ROADMAP Phase 5 #5a. Design + `std.numeric.ByteView` +
`std.text` UTF-8 path + `examples/byte_view/*`, gated by
`scripts/tests/check_byte_view.sh`.
Date: 2026-06-20 (core), 2026-06-21 (Text/UTF-8 composition)

## Problem

Parsers want to store "this field is bytes [off, off+len) of the input" without
copying. In a language with lifetimes that is a `&[u8]` field; Concrete has **no
lifetimes**, and references are **second-class — never stored in an aggregate and
never returned from safe code** (the H1 resolution, see VALUE_MODEL.md /
references-second-class). So a stored `&Bytes`/`&[u8]` field is not expressible.

Today the only options for a stored parse result are: copy the bytes into an owned
`Bytes` (allocates, defeats zero-copy), or read through a scoped callback
(`with_value`-style, cannot be stored). Neither lets a parser return a struct of
**owned, storable, zero-copy handles** into one input buffer.

## The type

`ByteView` is an **owned, `Copy`, reference-free** offset/length handle:

```
pub struct Copy ByteView {
    off: u64,   // start offset into the source buffer
    len: u64,   // length of the viewed region
}
```

It stores no pointer and borrows nothing, so it is freely storable in struct
fields (`Header { name: ByteView, value: ByteView }`), returnable, and `Copy` —
exactly what `&[u8]` fields do elsewhere, minus the lifetime. The bytes live in a
caller-held buffer; the view is just coordinates.

## Access: back through an explicit buffer, no returned reference

A view never yields a borrow. Access takes the buffer explicitly and returns an
owned, `Copy` `ByteCursor` (std.numeric) scoped to the region — or `None` if the
view does not validly describe that buffer:

```
pub fn cursor(&self, buf: &Bytes) -> Option<ByteCursor>          // bounds checked
pub fn byte(&self, buf: &Bytes, i: u64) -> Option<u8>            // single checked element
pub fn to_text(&self, buf: &Bytes) with(Alloc) -> Option<Text>   // COPIES into an owned Text
pub fn fits(&self, buf: &Bytes) -> bool                          // bounds test, not identity
pub fn off(&self) -> u64
pub fn len(&self) -> u64
pub fn is_empty(&self) -> bool
```

`cursor` returns a `ByteCursor` over `[off, off+len)` (built via the existing
`ByteCursor::from_raw(buf.ptr + off, len)` inside the trusted impl), so all reads
through it are already the bounds-checked cursor reads. The returned `ByteCursor`
is `Copy` and owns no Concrete reference — value-model compliant.

## Safety: what access checks, and what it deliberately does not

Every access validates, in order, and returns `None` on any failure:

1. **No overflow**: `off + len` must not wrap `u64` (checked add).
2. **In bounds**: `off + len <= buf.len()`, against the buffer PASSED IN.

**There is no third check, and there used to be one.** A `buf_len` field recorded
the length of the buffer a view was built from, and access rejected a buffer of a
different length. R-0483 removed it. It was never an identity check: it rejected a
substitution only when the lengths happened to differ, and accepted a *different
buffer of the same length* silently — which is the substitution that actually
causes wrong answers. Worse, it read like validation at the call site, so
`describes(buf)` looked like a guarantee nobody had.

A `ByteView` is **coordinates**. It denotes a range and applies to any buffer that
satisfies its bounds. That is now the stated contract rather than a gap in a brand.
The bounds test is named `fits`, not `describes`, so a reader cannot mistake it for
an identity test.

**Where the owner's identity matters, do not reach for a brand.** Own the buffer and
expose access through the owner, so substituting a different buffer is
unrepresentable rather than rejected. A length is not an identity and should not be
spelled like one.

In **proved/predictable** code, checks (1) and (2) are ordinary runtime-safety
obligations (`off + len` no-overflow and `off + len <= buf.len()`), so a caller that
has established the bounds can discharge the checked-access cost through the
normal obligation machinery rather than paying a runtime branch.

## Construction

```
pub fn new(off: u64, len: u64, buf: &Bytes) -> Option<ByteView>   // checked against buf's bounds
pub fn of_cursor(start: u64, cur: &ByteCursor) -> ByteView        // [start, cur.pos) just consumed
```

`new` performs the same overflow/bounds checks up front (a view that cannot be
valid is never constructed). `of_cursor` is the parser idiom: mark a start, read
fields via the cursor, then capture `[start, cur.pos())` as a stored view.

## Text / UTF-8 composition

A `ByteView` over raw bytes becomes a `Text` view only after **explicit** UTF-8
validation of the region — there is no implicit lossy conversion; raw `ByteView`
stays bytes until validated, matching the Bytes/Text split. `to_text(&buf)`:

1. checks the range fits `buf` (overflow / bounds), then
2. COPIES the region and validates well-formed UTF-8 (RFC 3629 / Unicode
   Table 3-7 — rejecting overlong encodings, surrogates `U+D800..U+DFFF`, and
   code points above `U+10FFFF`),

returning `Some(Text)` only when both hold, else `None`. The returned `Text` OWNS its
storage and does not depend on `buf` afterwards. R-0483: `to_text` copies for exactly
that reason, so the UTF-8 property it establishes holds for the value's whole life. The
previous `try_text` returned a `Text` pointing into `buf`, and mutating the source
afterwards left a "validated" value yielding bytes that were never validated.
`Text` is consequently linear rather than `Copy`. The ASCII-only `AsciiText::try_new`
remains for the owned-ASCII-newtype case.

## Owner-bound access: `BoundView` (R-0483)

`ByteView` stays reusable coordinates. Where the relationship to one owner matters,
`std.numeric.BoundView` is the owning result the paragraph above calls for: it owns the
`Bytes` it describes.

| property | how it holds |
|---|---|
| reads go to the owner held | `byte(i)` takes no buffer, so reading against another buffer cannot be written — **static** (E0262) |
| no mutation, reallocation or destruction while bound | the owner is a private field — **static** (E0298); the view is linear and must be `release`d — **static** (E0208) |
| survives moves, transfer and package boundaries | the owner travels inside the view |
| coordinates came from this owner's content | **not established**: `bind`/`of_view` re-validate bounds only |
| raw pointers (`BytesRaw.ptr`, `RawCursor`) | safe code may hold a pointer value; reading through it needs `Unsafe` — **audited assumption** |

Runtime owner identity was considered and not taken: safe code has no global state to mint
durable ids from, and an address or a length is not identity. Measured on
`examples/packet`: no bytes copied and no allocation for the payload, versus a copy into a
caller buffer. Evidence and fixtures: `tests/regressions/owner_bound/README.md`, gated by
`check_view_lifetime.sh`.

## Limitations (documented, not hidden)

- **A view does not identify its buffer, by design.** Any buffer satisfying the
  bounds is accepted. This was previously listed here as a limitation of the length
  brand; R-0483 resolved it by removing the brand and stating the contract, because
  a guard that catches some substitutions reads like one that catches all of them.
  If a workload needs owner-bound access, the answer is an owning parsed result, not
  a stronger token — `BoundView` (above) is that result for a single range.
- ByteView indexes one **contiguous** buffer; scatter/gather views are out of
  scope.

## Deliverables (landed)

- `std.numeric` (alongside `ByteCursor`): the `ByteView` type + `new`/`of_cursor`/
  `cursor`/`byte`/`to_text`/`fits`/`off`/`len`/`is_empty`. R-0483 made this a plain
  `impl`: with no stored pointer and no brand, nothing in it crosses a trust boundary.
- `std.numeric`: `BoundView` — `bind`/`of_view`/`byte`/`len`/`coords`/`release` (R-0483).
- `std.text`: an owning `Text` with `copy_from_raw` (validated, copying) and
  `from_string`, plus the `validate_utf8` well-formedness checker.
- `examples/byte_view/{http_header_view,tlv_packet_view,utf8_text_slice,wrong_buffer}/`
  — store views in a result struct, access through the buffer, validate a region
  into `Text`, and show the wrong-buffer / overflow / split-codepoint cases
  returning `None` (not silently passing).
- `scripts/tests/check_byte_view.sh` (Makefile `test-byte-view` + CI): proves
  views are storable/returnable owned `Copy` values; access goes back through an
  explicit buffer (no returned ref); the raw→`Text` step is UTF-8-validated; and
  wrong-buffer / overflow / out-of-range / invalid-UTF-8 cases return `None`.
