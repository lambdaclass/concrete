# Bug 295 — the parser appended each match arm, parameter and argument to the list built so far

<!-- bug-ledger
summary: the parser built its lists by appending one element at a time to the list built so far (`xs ++ [x]` in a loop): match arms, OR-pattern heads, parameters, call arguments, type arguments, bounds, capabilities, struct-literal and declaration fields, enum variants and their fields, pattern bindings, impl and trait methods, import symbols and union fields, so a list of n took n² to parse (a match of 20000 arms: parse 2082 ms; a function of 20000 parameters called with 20000 arguments: 3581 ms). Found probing the front end with scaling shapes after R-0550 (task: Resolve/Check/Elab quadratics). Class 17.
coverage: run_tests.sh
class: 17 — the parser's per-element list appends
silent: no
ruling: Violates: unrecorded
-->

**Status:** FIXED (branch `wip/bug-295`)
**Found:** 2026-10-26, probing the front end with shapes beyond the R-0550 gate (`wide-match`,
`many-params`), after bugs 285 – 293.
**Severity:** loud (performance). Compile time only; every AST, diagnostic and output is
unchanged.

## Reproducer

`parse` stage of `--emit-trace-json`, dev host (fede-256gb), main 1bcb7c36:

| shape | 2000 | 8000 | 10000 | 20000 |
|---|---|---|---|---|
| `wide-match` (one `match` on an `i64` with n literal arms) | 10 ms | 160 | 263 | 2082 |
| `many-params` (one function of n `i64` parameters, called once with n arguments) | 14 | 261 | 434 | 3581 |

Allocation heartbeats grow fourfold for fourfold size: the copies are traversals of the prefix
(Lean's `List.appendTR` reverses it), not new cells. Callgrind at 2000: `List.appendTR` from the
`parseMatchArms` loop 44.1 M instructions (2001 calls) in `wide-match`, from `parseParamList` and
from `parseCallArgs` 44.1 M each (1999 calls) in `many-params`.

## Cause

`Frontend/Parser.lean`: `parseMatchArms` (`arms := arms ++ (← parseMatchArm)`), `parseParamList`
(`params := params ++ [p]`) and `parseCallArgs` (`args := args ++ [e]`) append each element to the
growing list, and so do `parseMatchArm` (OR-pattern heads), `parseTypeArgList`,
`parseTypeBounds`, `parseTypeParams`, `parseTypeAndCapParams`, `parseWithCaps`, the function
type's parameters and capabilities in `parseType`, `parseStructLitFields`, `parseBindingList`,
the impl and trait method loops, the struct, enum and union declarations, `parseImport` and the
capability alias (Codex review 2026-10-26 listed every site; block statements and module
declarations were already Arrays or consed, bugs 255 and 273).

## Fix

Each of those loops accumulates in an `Array` and converts it to a list once, in the same order.
The two contract lists (`requires`/`ensures`) and loop invariants are left as they are (a
function's attributes, not a list a program grows). `check_no_quadratic_append.sh`'s Parser
baseline goes from 36 to 10. `--emit-core`, `--emit-ssa` and `--emit-llvm` (stdout, stderr and
exit status, so every parse diagnostic) of every `.con` under `tests/programs` and `examples` are
byte-identical with main's compiler.

## Measurements

`parse` stage, ms, dev host, main 1bcb7c36 → fix:

| shape | 2000 | 8000 | 10000 | 20000 |
|---|---|---|---|---|
| `wide-match` | 10 → 4 | 160 → 15 | 263 → 35 | 2082 → 115 |
| `many-params` | 14 → 5 | 261 → 15 | 434 → 10 | 3581 → 47 |

## Regression gate and leg

`run_tests.sh` parses a `match` of 20000 arms and a function of 20000 parameters called with 20000
arguments within 250 ms each, retried once at 500 ms (main's compiler: 2082 and 3581 ms).

## Structural cause

Class 17 ("pass state in association lists and per-query recomputation"): a list built by
appending each element to the list built so far, the shape of bugs 249, 255 and 273 in the same
file.

## Structural owner

R-0422 (complexity envelopes); `check_no_quadratic_append.sh` ratchets the file's remaining
appends.

Codex review 2026-10-26: CONFIRMED — All three claimed accumulators repeatedly traverse growing list prefixes, producing quadratic parsing work for the described shapes, while block-statement accumulation is already fixed.
