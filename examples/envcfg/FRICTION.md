# envcfg — friction log (Phase 7, workload 5)

`envcfg <file.conf>`: KEY=VALUE config resolver with environment override.
First workload compiling `std.env` (the Env capability boundary) and the
first line-oriented text parser. Gate:
`scripts/tests/check_envcfg.sh` (output matrix under controlled env +
13t exit codes).

## Compiler bug found

- **Bug 039 (fixed in this stack):** `import std.env.{get}` compiled to a
  call to `std.args.get` — the program-wide linker-alias pool is
  first-match, and `std.cli`'s `import std.args.{get}` got there first.
  String pointer read as argv index → segfault. Fixed in EmitSSA
  (module-local import aliases now shadow the pool);
  `tests/programs/regress_039_import_alias_collision/` pins it. Notable:
  std.env had interpreter tests only, so the first COMPILED use of the
  module was this workload — module-level "works in tests" said nothing
  about the backend path.

## Pull candidates (workload evidence for std)

1. **Bytes: find/split until delimiter** — THIRD ask (elf_header re-rolled
   NUL-scan, tar_list re-rolled NUL-padded-field extraction, envcfg
   re-rolls newline splitting + `=` search + region trim). A
   `Bytes::index_of(byte, from) -> Option<u64>` plus a way to make a
   String from a byte region would delete ~half this program. This is now
   over the pull threshold; strongest std candidate from workloads so far.
2. **String.split / find** — same gap one level up; `starts_with` exists
   but nothing locates a character.
3. **Multi-value return** — `trim_region` wants to return (start, end);
   packed both into one u64 (high/low 32 bits) because tuples are
   workload-gated. Second ask after png_chunks' similar packing. Ugly but
   local; a 2-field struct would also do — the friction is the ceremony,
   not expressiveness.

## Friction that is design-intended (not re-flagging)

- Linear Strings: every temporary needs an explicit `.drop()`; match arms
  that consume payloads must drop in every arm. Verbose but caught two
  real leaks while writing (E0208 did its job).
- No string interpolation/fmt beyond push_char loops — `out` assembly is
  manual. std.fmt push_hex exists for hex; a `String.push_str(&String)`
  would cover most of the remaining loops (cheap pull candidate).

## Notes

- `std.env.get` allocating a fresh String (copy out of libc's buffer) is
  the right shape — no lifetime coupling to the environment block.
- `env -u` in the gate keeps the matrix hermetic against the CI runner's
  ambient environment.
