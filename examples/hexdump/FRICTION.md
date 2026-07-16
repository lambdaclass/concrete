# hexdump — friction log (Phase 7, workload 3)

CLI-shaped workload: an xxd-compatible hex dumper (`[-s skip] [-n len]
<file>`). Byte-identical to real `xxd` for the default form, `-s`/`-n`
combinations, skip-past-EOF, empty files, and a 64KB+7 random file — the
check gate diffs against the system tool. Exit codes: 0 clean, 1 IO
failure, 2 usage/malformed flag value.

## Friction found

1. **Hex formatting re-rolled — SECOND ask.** `std.fmt.format_hex` is
   `0x`-prefixed, minimal-width, and allocates a String per call;
   `fmt.hex_digit` is module-private. A dump loop wants fixed-width
   digit-level control (2-digit bytes, 8-digit offsets) pushed into an
   existing buffer. png_chunks held this pull at one ask ("not pulled:
   hex fmt"); this is the second. → pull candidates: `fmt.hex_digit` made
   pub, plus `push_hex(s, value, width)` (no prefix, zero-padded,
   buffer-appending — no per-field alloc).

2. **Flag parsing is a hand loop — expected, and now measured.** Indexed
   `get(i)` + String literals + `eq` + manual value-flag lookahead +
   positional/flag split + duplicate-positional rejection: ~45 lines for
   two value flags and one positional. Every CLI tool re-writes exactly
   this. This workload is the pull evidence ROADMAP item 2 (`std.cli`)
   asked for; design should cover: value flags, flag=value or
   space-separated, unknown-flag rejection, positional collection, and
   the usage/exit-2 convention from 13t.

3. **String-literal comparison ergonomics.** Comparing an arg against a
   literal takes three statements (bind literal, `eq(&lit)`, drop) ×2
   flags × every iteration. A `&String == literal` form or
   `String::eq_str(&self, lit)` would collapse the pattern. Held at one
   ask (also felt in png_chunks's IEND check — arguably ask two).

4. **Consume-then-rebind in a loop branch rejected (E0207 shape).**
   First draft held the positional as `let mut path: String`, doing
   `path.drop(); path = get(i);` inside a nested else — rejected. The
   legal-and-simpler restructure (track `path_idx: Int`, fetch once after
   the loop) took a minute; recording because it is the same
   control-flow-ownership edge the exemption work tracks, not new
   evidence for a rule change.

## Deliberately not pulled

- Streaming dump (read 16 bytes at a time): read_all + whole-buffer walk
  is fine at every realistic dump size; streaming waits for a workload
  that needs bounded memory.
- `xxd -c/-g/-r` (columns/grouping/reverse): scope creep, no consumer.
- Stdin input (`hexdump -`): wants a `stdin_reader()`; held at one ask.
