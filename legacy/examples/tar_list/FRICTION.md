# tar_list — friction log (Phase 7, workload 4)

Binary-format-with-ASCII-numerics workload: a ustar archive lister,
byte-identical to `tar -tf` across file-size edges (0/511/512/513 bytes),
directories, and >100-char paths (the ustar prefix/name split). The check
gate diffs against the system tar. Exit codes: 0 clean, 1 bad archive/IO,
2 usage. First workload written WITH std.cli from the start (dogfood —
zero arg-parsing friction, as designed).

## Friction found

1. **NUL-padded fixed-width text fields re-rolled** (`field_string`): every
   binary format with embedded text (tar names, ELF section names, ZIP
   entries) walks bytes-until-NUL into a String. Held at one ask (elf_header
   predates the discipline; counting it, this is arguably ask two) →
   pull candidate: `Bytes.field_string(off, max)` or
   `Text::from_bytes_until_nul`.

2. **Numeral parsing out of a Bytes region re-rolled** (`parse_octal`):
   `std.parse.parse_oct` takes `&String`, but tar's numerals live at an
   offset inside binary data — using std would cost a String allocation per
   header field. Held at one ask → pull candidate: parse variants over
   `&Bytes + offset` (or over the future byte-slice view).

3. **Checked u64 subtraction caught a real bug in MY walk loop** (positive
   friction): the first draft's `while total - off >= 512` underflowed when
   a truncated archive pushed `off` past `total` — the checked-arithmetic
   trap surfaced it in the first test run instead of an infinite/wild loop,
   and the fix is the H2 overflow-safe shape (`off < total` guard first).
   Language sharp edge working exactly as the trap contract promises.

4. **String concatenation is push-char loops** (prefix + '/' + name): no
   `String::push_str(&String)` — the join is a hand loop. Held at one ask
   (base64_cli/hexdump never needed it) → pull when a third workload
   concatenates.

## Deliberately not pulled

- GNU long-name (L-type) entries, pax headers: bsdtar's ustar output
  covers the check matrix; extensions wait for an archive that needs them.
- Size/mtime/mode display (`tar -tvf` shape): -t only, no consumer for
  the verbose row.
- Streaming (header-at-a-time reads): read_all is fine at listing scale;
  same held note as hexdump.
