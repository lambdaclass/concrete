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
    off:     u64,   // start offset into the source buffer
    len:     u64,   // length of the viewed region
    buf_len: u64,   // length of the buffer this view was branded against
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
pub fn cursor(&self, buf: &Bytes) -> Option<ByteCursor>   // bounds + brand checked
pub fn byte(&self, buf: &Bytes, i: u64) -> Option<u8>     // single checked element
pub fn try_text(&self, buf: &Bytes) -> Option<Text>       // UTF-8-validated Text view
pub fn off(&self) -> u64
pub fn len(&self) -> u64
pub fn is_empty(&self) -> bool
```

`cursor` returns a `ByteCursor` over `[off, off+len)` (built via the existing
`ByteCursor::from_raw(buf.ptr + off, len)` inside the trusted impl), so all reads
through it are already the bounds-checked cursor reads. The returned `ByteCursor`
is `Copy` and owns no Concrete reference — value-model compliant.

## Safety: the checks `cursor`/`byte`/`try_text` enforce

Every access validates, in order, and returns `None` on any failure:

1. **No overflow**: `off + len` must not wrap `u64` (checked add).
2. **In bounds**: `off + len <= buf.len()`.
3. **Right buffer (brand)**: `buf.len() == self.buf_len`. A view carries the
   length of the buffer it was created against; accessing it against a
   different-length buffer is rejected. This is a *cheap* wrong-buffer guard, not
   a proof of identity — a different buffer of the *same length* still passes (see
   Limitations). It catches the common mistake (view from buffer A applied to a
   reused/grown/shrunk buffer B) without lifetimes.

In **proved/predictable** code, checks (1) and (2) are ordinary runtime-safety
obligations (`off + len` no-overflow and `off + len <= buf_len`), so a caller that
has established the bounds can discharge the checked-access cost through the
normal obligation machinery rather than paying a runtime branch.

## Construction

```
pub fn new(off: u64, len: u64, buf: &Bytes) -> Option<ByteView>   // checked; brands to buf.len()
pub fn of_cursor(start: u64, cur: &ByteCursor) -> ByteView        // [start, cur.pos) just consumed
```

`new` performs the same overflow/bounds checks up front (a view that cannot be
valid is never constructed). `of_cursor` is the parser idiom: mark a start, read
fields via the cursor, then capture `[start, cur.pos())` as a stored view.

## Text / UTF-8 composition

A `ByteView` over raw bytes becomes a `Text` view only after **explicit** UTF-8
validation of the region — there is no implicit lossy conversion; raw `ByteView`
stays bytes until validated, matching the Bytes/Text split. `try_text(&buf)`:

1. checks the view validly describes `buf` (overflow / bounds / brand), then
2. runs the region through `Text::try_from_raw`, which validates well-formed
   UTF-8 (RFC 3629 / Unicode Table 3-7 — rejecting overlong encodings,
   surrogates `U+D800..U+DFFF`, and code points above `U+10FFFF`),

returning `Some(Text)` only when both hold, else `None`. The returned `Text` is a
non-owning view (`ptr + len`), so the buffer must outlive it — the same scoping
rule as any view. std.text gained `Text::from_raw(ptr, len)` (trusted, unchecked)
and `Text::try_from_raw(ptr, len)` (validated) to support this; the previous
ASCII-only `AsciiText::try_new` remains for the owned-ASCII-newtype case.

## Limitations (documented, not hidden)

- **Same-length wrong buffer** passes the brand check. Stronger branding (a
  buffer role/id token threaded into the view) is a future option if a workload
  shows the length brand is insufficient; the length brand is the cheap 80% guard.
- ByteView indexes one **contiguous** buffer; scatter/gather views are out of
  scope.

## Deliverables (landed)

- `std.numeric` (alongside `ByteCursor`): the `ByteView` type + `new`/`of_cursor`/
  `cursor`/`byte`/`try_text`/`off`/`len`/`is_empty`, in a `trusted impl` whose
  boundary is documented.
- `std.text`: `Text::from_raw(ptr, len)` (trusted, unchecked) and
  `Text::try_from_raw(ptr, len)` (validated) + the `validate_utf8` well-formedness
  checker.
- `examples/byte_view/{http_header_view,tlv_packet_view,utf8_text_slice,wrong_buffer}/`
  — store views in a result struct, access through the buffer, validate a region
  into `Text`, and show the wrong-buffer / overflow / split-codepoint cases
  returning `None` (not silently passing).
- `scripts/tests/check_byte_view.sh` (Makefile `test-byte-view` + CI): proves
  views are storable/returnable owned `Copy` values; access goes back through an
  explicit buffer (no returned ref); the raw→`Text` step is UTF-8-validated; and
  wrong-buffer / overflow / out-of-range / invalid-UTF-8 cases return `None`.
