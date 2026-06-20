# Owned Byte Views (design)

Status: design — ROADMAP Phase 5 #5a / KNOWN_HOLES "Owned ByteView zero-copy
stored idiom". Implementation pending (this doc precedes the std type + examples +
gate).
Date: 2026-06-20

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

## Text / UTF-8 composition (deferred follow-up)

The intended composition is: a `ByteView` over raw bytes becomes a `Text` view
only after explicit UTF-8 validation of the region (no implicit lossy conversion —
raw `ByteView` stays bytes until validated, matching the Bytes/Text split). This is
**not in the first ByteView increment** because `Text` (std.text) has private
fields and only a `from_string(&String)` constructor — a region→`Text` path needs a
`Text::from_raw(*const u8, len)` constructor (and, separately, a UTF-8 validator;
std today validates only ASCII, via `AsciiText::try_new`). Both are tracked as the
follow-up to this increment; the core view+access idiom below ships first and does
not depend on them.

## Limitations (documented, not hidden)

- **Same-length wrong buffer** passes the brand check. Stronger branding (a
  buffer role/id token threaded into the view) is a future option if a workload
  shows the length brand is insufficient; the length brand is the cheap 80% guard.
- ByteView indexes one **contiguous** buffer; scatter/gather views are out of
  scope.

## Deliverables

First increment (this build):
- `std.numeric` (alongside `ByteCursor`): the `ByteView` type + `new`/`of_cursor`/
  `cursor`/`byte`/`off`/`len`/`is_empty`, in a `trusted impl` whose boundary is
  documented.
- `examples/byte_view/{http_header_view,tlv_packet_view,wrong_buffer}/` — store
  views in a result struct, access through the buffer, and show the wrong-buffer /
  overflow cases returning `None` (not silently passing).
- `scripts/tests/check_byte_view.sh`: proves views are storable/returnable owned
  `Copy` values; access goes back through an explicit buffer (no returned ref);
  and wrong-buffer / overflow / out-of-range cases return `None` rather than
  silently passing.

Follow-up (separate increment): `Text::from_raw` + a UTF-8 validator, then
`examples/byte_view/utf8_text_slice/` and a `try_text` access method.
