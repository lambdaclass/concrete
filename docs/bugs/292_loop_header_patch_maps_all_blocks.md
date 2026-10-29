# Bug 292 — Lower added each loop header's phis by mapping every finished block (quadratic in loop nesting)

<!-- bug-ledger
summary: Lower added a loop header's phis (`prependInstsToBlock`) and an aggregate merge's stores (`insertStoresBeforeTerm`) to already-finished blocks by mapping the whole finished-block list, so n nested `while` loops took time n² to lower (depth 8000: lower 3011 ms on main). Class 17.
fixtures: regress_292_loop_header_patches.con
class: 17 — every loop header patched by a map over all finished blocks
silent: no
ruling: Violates: unrecorded
-->

**Status:** FIXED (branch `wip/bug-292`, based on `wip/perf-286`)
**Found:** 2026-10-26, probing Lower with nesting shapes beyond the R-0550 gate's (nested `while`),
after bug 286's fix had removed the address-taken re-walks from the same shape.
**Severity:** loud (performance). Compile time only: the emitted IR is unchanged.

## Reproducer

`while s < 1 { ` nested n deep around `s = s + 1;` in `main` (one mutable binding).
`lower` stage of `--emit-trace-json`, ms, fastest of 3, dev host (fede-256gb):

| depth | main cc7a4084 | with bug 286's fix (wip/perf-286) |
|---|---|---|
| 2000 | 128 | 70 |
| 8000 | 3011 | 1084 |
| 10000 | 4851 | 1778 |

Heartbeats grow with the depth (1.26 M to 5.03 M from 2000 to 8000): the cost allocates little.
Callgrind with bug 286's fix, depth 1000 to 2000: `List.mapTR` specialized in
`prependInstsToBlock` 87 M to 354 M instructions (2000 calls from `lowerStmt`'s `while` case),
`List.reverseAux` 36 M to 126 M.

## Cause

`IR/Lower.lean`: `prependInstsToBlock` adds the header phis of a loop, once the loop's back and
continue edges are known, by `s.blocksRev.map` over every finished block of the function, looking
for the header's label (`List.map` is `mapTR`, which also reverses the list). The header of the
k-th loop from the inside is patched when all blocks of the k inner loops are finished, so n nested
loops map n²/2 blocks. `insertStoresBeforeTerm` (the aggregate merge stores of `if` and `match`)
maps the whole list the same way per merge (Codex review 2026-10-26).

## Fix

Both record their instructions in `LowerState.blockPatches` (by label: the prefixes and suffixes,
each newest first), and `lowerFn` applies them once when it assembles the function's blocks
(`applyBlockPatches`), before hoisting the entry allocas, as before. A patch names only a block
already finished (`finishedLabels`), as the map touched only finished blocks; prepends keep their
order (a later one in front) and appends theirs. Both tables are persistent trees: a first version
with hash tables was still quadratic, because the outer loops' lowering keeps the state shared and
each insert copied the table (`lean_copy_expand_array` under `terminateBlock`).

`--emit-ssa` and `--emit-llvm` of every `.con` under `tests/programs` and `examples` and of 3000
programs from `fuzz_differential.py`'s generator are byte-identical with `wip/perf-286`'s compiler.

## Measurements

`lower` stage, ms, fastest of 3, dev host:

| depth | main | wip/perf-286 | this fix |
|---|---|---|---|
| 2000 | 128 | 70 | 36 |
| 8000 | 3011 | 1084 | 279 |
| 10000 | 4851 | 1778 | 521 |

Callgrind with the fix, depth 2000 to 4000: `lowerModule` 103 M to 216 M instructions, the whole
`lower` stage 479 M to 969 M (x2.0 for x2).

## Regression gate and leg

`run_tests.sh`: `regress_292_loop_header_patches.con` (nested loops with `continue` and `break`
edges, whose header phis are patched in, and an aggregate-valued `match` inside a loop, whose result
slot gets one store patched into each arm) returns 42, and the `lower` stage of 8000 nested
`while`s must take at most 500 ms, retried once at 900 ms (main's compiler: 3011 ms; bug 286's:
1084 ms).

## Structural cause

Class 17 ("pass state in association lists and per-query recomputation"): an edit of one block
done by a walk of every block, the shape of bug 178's per-arm block scans.

## Structural owner

ROADMAP #5 (the structured Lower/SSA builder): blocks addressed by handle, never found by a scan
of the finished list; R-0422 holds the throughput envelope meanwhile.

Codex review 2026-10-26: CONFIRMED — each nested while patches its header by mapping and reversing the entire accumulated finished-block list, yielding quadratic work independently of the additional address-taken subtree re-walks.
