# Bug 288 — EmitSSA's program-wide name and type tables were lists searched per function and per type

<!-- bug-ledger
summary: EmitSSA kept its program-wide tables (names defined under two module paths, defined and method names, link-symbol ambiguity, emitted type names, string-literal lengths, deduplicated declarations) as lists and searched them per function, per call, per type or per literal, so emitting a program of n functions or n types took time n² (100000 functions: --emit-llvm 220 s, of which the in-compiler stages through lower 5.5 s). Found re-measuring the R-0550 declaration shapes at 10^5 with whole compiles. Class 17.
coverage: run_tests.sh
class: 17 — EmitSSA's program-wide tables were lists
silent: no
ruling: Violates: unrecorded
-->

**Status:** FIXED (branch `wip/bug-288`)
**Found:** 2026-10-26, re-measuring the scaling gate's declaration shapes (`functions`,
`call-chain`, `structs`, `enums`) at 10^4 and 10^5 with whole compiles (`--emit-llvm`, `-o`),
which the gate had not run for them (R-0550 follow-up).
**Severity:** loud (performance). Compile time only; the emitted LLVM is unchanged.

## Reproducer

Shapes of `scripts/tests/lib/compile_scaling.py` (`gen`), `concrete <file> --emit-llvm`, wall time
on the dev host (fede-256gb), main cc7a4084:

| shape | 10000 | 20000 | 100000 |
|---|---|---|---|
| `functions` | 1.36 – 1.65 s | 5.4 – 5.9 s | 220 s |
| `call-chain` | 1.48 – 1.62 s | 6.6 s | 179 s |
| `structs` | 0.33 s | 1.6 – 1.7 s | 37 s |
| `enums` | 0.40 – 0.46 s | 1.5 s | 35 s |

The in-compiler stages through `lower` (`--emit-trace-json`) take 0.29 s for `functions` at 10000
and 5.5 s at 100000; native compiles (`-o`) take 272 s at 100000. Callgrind of `functions` at
10000 (12.05 G instructions): `emitSSAProgram` 9.15 G inclusive, of which a `List.any` called 10001
times from a fold (81.8 M string comparisons) 4.55 G and another fold 4.39 G; of `structs` at 10000,
`emitSModule`'s struct fold 2.17 G of 3.2 G, almost all `List.elem` on strings.

## Cause

`Backend/EmitSSA.lean`:
- `emitSSAProgram`: `collidingNames` searched every function definition per definition
  (`fnNamesByModule.any`); `fnSymbols` searched every (name, symbol) pair per pair for an ambiguous
  name; `dedupDecls` searched the kept declarations and the defined names per declaration.
- `EmitSSAState.definedFnNames`, `methodFnNames` and `collidingNames` were `List String`, asked
  with `contains` per call (`resolveAliasedName`, `resolveMethodName`) and per function
  (`llvmFnName`, `moduleCallContext`).
- `emitSModule`: `emittedTypes : List String` was searched per struct and per enum.
- `stringLengths` was a list appended per string literal and searched per use.
- `moduleCallContext`: `ownDefs` was a list searched per extern.

Codex review 2026-10-26 confirmed the two per-function scans and the per-type scan as the dominant
causes of the measured shapes and the others as workload-dependent.

## Fix

The tables are hash sets and maps built once: a name collides when a second definition carries
another module path (one pass keeping each name's first path); a link-symbol name is ambiguous
when a second pair gives it another symbol (one pass keeping each name's first symbol); declarations
are deduplicated with a set of names seen, keeping the first in order; the first entry of a string
literal name wins, as the list's `find?` did. The emitted LLVM is unchanged: `--emit-llvm` of the
four shapes at 10000 and 20000 and `--emit-ssa`/`--emit-llvm` of every `.con` under
`tests/programs` and `examples` are byte-identical with main.

## Measurements

`--emit-llvm` wall time, dev host, main cc7a4084 and the fix:

| shape | 10000 before | after | 20000 before | after | 100000 before | after |
|---|---|---|---|---|---|---|
| `functions` | 1652 ms | 347 | 5896 | 591 | 220 s | 7.5 s |
| `call-chain` | 1622 | 687 | 6569 | 1376 | 179 s | 9.7 s |
| `structs` | 341 | 94 | 1680 | 301 | 37 s | 3.0 s |
| `enums` | 460 | 134 | 1531 | 256 | 35 s | 3.5 s |

With the fix, `emitSSAProgram`'s instruction count grows 1.9 – 2.0 times from 10000 to 20000 for
`structs` (45 M to 87 M) and `functions` (228 M to 452 M).

## Regression gate and leg

`run_tests.sh` times `--emit-llvm` of 20000 generated functions against 2000 ms, retried once at
4000 ms (591 ms with the fix; 5896 ms on main's compiler). The scaling gate's declaration shapes
run their whole compiles at 10^4 and 10^5 once their budgets are measured on the CI host (R-0550
follow-up branch).

## Structural cause

Class 17 ("pass state in association lists and per-query recomputation"): program-wide facts kept
in lists and recomputed by a search per item, the shape of bugs 027 and 279.

## Structural owner

R-0422 (compiler throughput queue, complexity envelopes); the R-0550 gate holds the declaration
shapes' whole compiles.

Codex review 2026-10-26: CONFIRMED — Two per-function full-list searches and growing type-name membership scans independently force quadratic emission work, while the other cited scans contribute according to workload.
