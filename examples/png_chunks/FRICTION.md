# png_chunks — friction log (Phase 7, workload 2)

Parse/protocol-shaped workload: a PNG chunk inspector (real binary format,
not a synthetic checklist). Verified against generated PNGs (valid /
corrupted-CRC / truncated / non-PNG) and a real-world file (site logo:
multi-IDAT, ancillary pHYs — all CRCs check). Exit codes are REAL (first
workload written in the u8-main style): 0 clean, 1 bad input, 2 usage.

## Friction found

1. **BUG 033 (compiler, fixed en route):** `discard(e)` with ANY live
   String/struct variable across it failed SSA verification (E0714 —
   aggregate phi). discard desugars to a value-`if`, and the ifExpr merge
   loop phied live aggregate vars instead of using the statement-if's
   alloca path. Third workload-found compiler bug in two workloads —
   first-contact code keeps exercising shapes the corpus lacks.
   See `docs/bugs/033_ifexpr_merge_aggregate_phi.md`.

2. **Big-endian decode is hand-rolled** (`be32` — shift-free multiply form).
   Every binary format starts here. → pull: `std.numeric` endian helpers
   (`be32/le32/be16/le16` over `&Bytes` + offset, bounds-checked variants).

3. **CRC32 is hand-rolled** (bitwise ISO-HDLC variant, ~20 lines, easy to
   get subtly wrong — poly constant, reflection, final xor). PNG, gzip, zip
   all need it. → pull: `std.checksum.crc32` (a checksum, not crypto — does
   not violate the breadth hold).

4. **Reader→Bytes needed a hand loop** (`read_all`: stack chunk buffer +
   per-byte push). Every whole-file parser starts here. → pull:
   `io.read_all(&Reader) -> Bytes` (alloc-carrying) or `Bytes::from_reader`.

5. **String literal comparison needs a temp** — no `s.eq_str("IEND")`;
   had to materialize `let iend: String = "IEND"; ty.eq(&iend); iend.drop();`.
   Small but every parser hits it. → candidate: `eq_str(&self, lit: &String)`
   is what exists; the friction is literal-to-&String at the call site.

6. **No hex formatting** (CRC values would read better as hex). Didn't
   block (status OK/BAD suffices); noted for fmt when pulled harder.

## Pull list (ranked)

| # | Pull | Evidence | Size |
|---|------|----------|------|
| 1 | `io.read_all` / `Bytes::from_reader` | friction 4 — every file parser | std, small |
| 2 | endian helpers (`be32`/`le32`/...) | friction 2 — every binary format | std, small |
| 3 | `std.checksum.crc32` | friction 3 — PNG/gzip/zip recurrence | std, small |
| 4 | literal-vs-String ergonomics | friction 5 — every parser | design question, hold |

Not pulled: hex formatting (cosmetic), chunked/streaming parse (whole-file
Bytes was fine at PNG sizes), std.png (format-specific — stays an example).
