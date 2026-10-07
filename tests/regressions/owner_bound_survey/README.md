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
| view escaping its owner's scope, used with an unrelated owner (`view_outlives_scope`) | exit 71 | accepted by contract |
| `ByteView::of_cursor`, read against another buffer (`of_cursor_substitution`) | exit 51 | accepted by contract |
| view from another package, read against another buffer (`xpkg_lib` / `xpkg_app`) | exit 81 | accepted by contract |
| `RawCursor::from_raw` / FFI pointer | `with(Unsafe)` on construction and reads | audited assumption |

**Costs of the current design.** One bounds/length check per access (`fits`); `to_text`
copies and needs `Alloc`; `examples/packet`'s `extract_payload` copies into a caller-supplied
buffer to stay allocation-free (keeping the example predictable).

**What remains open.** Nothing binds a view to the owner it was made from. A repair may detect
substitution at runtime (owner identity checked per access) or reject it statically (scoped
access); either flips the four "accepted by contract" rows deliberately, and its cost in
copying, allocation and per-access checks must be measured on `examples/packet`.
