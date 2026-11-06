# R-0483 owner-bound survey (slice 1, 2026-10-07)

What the current coordinate design does, measured. Gated by `check_view_lifetime.sh`.

| behaviour | observed | class |
|---|---|---|
| zero-copy read (`view_lifetime/current_cursor_valid_access`) | exit 65 | valid |
| moving the owner (`owner_moved`) | exit 11 | valid — same storage |
| owned `Text` after the source mutates (`view_lifetime/current_text_survives_mutation`) | exit 66 | valid — copies, needs `Alloc` |
| read after the owner is consumed (`view_lifetime/byteview_byte_rejected`) | E0205 | rejected statically |
| pointer-holding view without `Unsafe` (`view_lifetime/unsafe_boundary`) | E0521 | rejected statically |
| read past a shrunk/emptied owner (`view_lifetime/current_cursor_refuses_shrunk_buffer`) | `None` | detected at runtime (per-access `fits`) |
| wrong buffer, same length (`view_lifetime/byteview_wrong_buffer_same_length`) | exit 91 | accepted by contract |
| view COORDINATES escaping their owner's scope, then read against an unrelated owner (`view_outlives_scope`) | exit 71 | accepted by contract |
| `ByteView::of_cursor`, read against another buffer (`of_cursor_substitution`) | exit 51 | accepted by contract |
| view from another package, read against another buffer (`xpkg_lib` / `xpkg_app`) | exit 81 | accepted by contract |
| `RawCursor::from_raw` / FFI pointer | `with(Unsafe)` on construction and reads | audited assumption |

**What "escape" means here.** `ByteView` is `{off, len}` and `ByteCursor` is `{off, len, pos}`,
all `u64`: what leaves the owner's scope is a pair of integers, not a borrowed reference or a
pointer, and reading them still requires a live borrow of SOME buffer. No reference or pointer
escape is shown by this survey. The only pointer-holding cursor, `RawCursor`, needs `with(Unsafe)`
to construct and to read. A reference or pointer escaping safe code would be a separate safety
defect, not this gap.

**Scope of the gate.** `check_view_lifetime.sh` passing validates this SURVEY — several of its
rows deliberately confirm misuse that is accepted today. It is not evidence of an owner-binding
repair; that repair must flip those rows.

**Costs of the current design.** One bounds/length check per access (`fits`); `to_text`
copies and needs `Alloc`; `examples/packet`'s `extract_payload` copies into a caller-supplied
buffer to stay allocation-free (keeping the example predictable).

**Superseded for owner-bound use (slice 2): `std.numeric.BoundView`**, see `../owner_bound`.
`ByteView` stays reusable coordinates and these rows stay true of it.

**What remained open at slice 1.** Nothing binds a view to the owner it was made from. A repair may detect
substitution at runtime (owner identity checked per access) or reject it statically (scoped
access); either flips the four "accepted by contract" rows deliberately, and its cost in
copying, allocation and per-access checks must be measured on `examples/packet`.
