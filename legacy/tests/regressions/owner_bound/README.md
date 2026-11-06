# R-0483 owner-bound views (slice 2, 2026-10-07)

**Decision: approach A, an owner-bound view built from existing mechanisms (linear ownership and
private fields): `std.numeric.BoundView`. Approach B, runtime owner identity, is not taken.**

## The three things that were being conflated

| thing | what it promises | type |
|---|---|---|
| reusable coordinates | a range; applies to ANY buffer long enough, by contract | `ByteView`, `ByteCursor` (`u64` fields only) |
| owner-bound view | every read goes to the one owner it holds | `BoundView` (owns its `Bytes`) |
| raw pointer | nothing; reading through it is an `Unsafe` obligation | `BytesRaw.ptr`, `RawCursor` |

## Why A, from the evidence

- **B has no durable identity to check.** Concrete has no global mutable state in safe code
  (`LANGUAGE_INVARIANTS.md` §10), so there is no counter to mint per-owner ids from without adding
  trusted state or an FFI id source — a new audited assumption plus 8 bytes per owner and a compare
  on every access. An address or a length is not identity: freed memory may be reused by the
  allocator (a probe on this machine's allocator saw no reuse in 64 tries, which is not evidence
  either way, so no gate rests on it). B would at best DETECT substitution at runtime.
- **A rejects substitution statically.** `BoundView::byte(i)` takes no buffer, so a read against
  another buffer cannot be written (E0262). The owner is a private field (E0298), so it cannot be
  mutated, reallocated or destroyed while bound, and the view is linear (E0208), so it cannot be
  dropped without `release`. No new language feature, no runtime state.

## What each guarantee rests on

| scenario | fixture | how |
|---|---|---|
| zero-copy read | `bound_read` (11) | valid |
| owner moved / transferred | `bound_moved` (13) | valid — the view carries the owner |
| cross-package | `bound_xpkg_lib` + `bound_xpkg_app` (42) | valid — the owner crosses with the view |
| `ByteView::of_cursor` coordinates | `bound_of_cursor` (3) | valid — bounds re-validated against the owner when bound |
| mutation after release | `bound_release_rebind` (202) | valid — mutation needs the owner back |
| wrong-buffer substitution (any length) | `bound_no_buffer_argument` | **rejected statically** (E0262) |
| mutation / reallocation / destruction while bound | `bound_no_owner_mutation`, `bound_must_release` | **rejected statically** (E0298, E0208) |
| escape through wrappers | — | a `BoundView` moved into any wrapper takes its owner along; `coords()` hands out reusable coordinates explicitly |
| raw pointer leaving safe code | `raw_pointer_needs_unsafe` | **audited assumption** — reading through it needs `Unsafe` (E0521) |
| FFI construction | — | no `BoundView` constructor from a pointer; bytes from FFI enter through `Bytes`' trusted constructors |

**Not established:** that coordinates were derived from the owner's CONTENT. `of_view`/`bind`
re-validate bounds against the owner and bind them; coordinates computed from a different buffer of
sufficient length bind successfully. The guarantee is "reads go to the owner held", which is the
one wrong-buffer substitution breaks. `ByteView` itself stays reusable coordinates — the survey rows
for it (`../owner_bound_survey`) still pass, by contract.

## Measured cost on `examples/packet`

| | `extract_payload` (copy) | `payload_view` + `payload_sum` (bound) |
|---|---|---|
| bytes copied | payload length (into a caller buffer) | 0 |
| allocation (source-level: `Bytes::with_capacity`/`push` calls and `Alloc` in the signature; malloc was not instrumented) | a 1500-byte caller buffer, filled by 1500 pushes | none; no `Alloc` |
| per-access checks | cursor `has` + destination `set` bound check | `i < len` + the owner's `get` bound check |
| API change | — | the buffer is moved into the view and `release`d back before reuse |

The predictable profile is unchanged in shape: 1 function fails (`main`, blocking I/O), 16 pass
(was 13; the three new functions all pass). Gated by `check_view_lifetime.sh`; removing the
protection fails it (owner made `pub`: 1 row; `byte` reading a caller buffer: 8 rows).
