### A Constant Before An `if` / `while` / `match` Block Gets An Explaining Diagnostic (bug 212, 2026-10-23)

In a control-flow head an uppercase name followed by `{` is read as the start of a struct literal,
so `if n > LIMIT { … }` with a module constant was refused with `expected }, got =`, pointing inside
the block. The E0001 now says that `LIMIT {` was read as a struct literal and names the
parenthesized form (`if (n > LIMIT) { … }`). The case rule, which was in neither the grammar nor
the specification, is written into `grammar/concrete.ebnf` and SYN-1; the accepted language is
unchanged, and whether a head should admit a struct literal is left open. Found by Phase 7
workload 9 (`examples/lzpack`). Pinned by `error_212_const_in_{if,while,match}_head.con` and
`regress_212_parenthesized_const_head.con`.

### Each Leaking `return` Is Named In Its E0208 (bug 214, 2026-10-23)

A linear binding left unconsumed by two `return`s produced two byte-identical refusals at its
declaration, naming neither exit. Each refusal now carries a related span on its `return`
(`'b' is still unconsumed when this `return` leaves the function`). Which programs are refused is
unchanged. Pinned by `error_214_two_returns_leak.con`.

### Phase 7 Workload 9: lzpack (2026-10-23)

`examples/lzpack` is an LZSS + PackBits RLE compressor and decompressor in a CRC-checked container
(`c [-r]`, `d`, `t`, `selftest`; 13t exit codes). `scripts/tests/check_lzpack_differential.sh`
compares its compressed streams byte for byte with an independent Python encoder of the same
specification (`scripts/tests/lib/lzpack_ref.py`) over real text, source, binary and synthetic
edge inputs, decodes the reference's streams, and checks corrupt streams and exit codes (58
checks); six `@run` cases agree natively and under `--interp`. The workload found bugs 212 and
214 and no miscompile. `examples/lzpack/FRICTION.md` maps its friction to the open language
decisions.
