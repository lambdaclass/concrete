# Bug 290 — Check walked every non-Copy binding at every match arm, and Check and Lower looked enums up in a list

<!-- bug-ledger
summary: at every arm of every `match`, Check's H9 block-local check (`checkBlockLocalsConsumed`) walked every non-Copy binding in scope and the arms' agreement check walked every pre-existing one, and Check (`lookupEnum`, `isCopyType`) and Lower (`variantIndex`, `variantFields`, enum construction and enum-match arms) found each enum by searching the list of all enums, so a function matching n enum values of n enum types took n² to check and to lower (8000: check 10088 ms, lower 2807 ms). Class 17.
coverage: run_tests.sh, the bug 290 leg: the check stage of a main that builds and matches 8000 enum values of 8000 enum types within 600 ms (retry 1200)
class: 17 — per-arm walks of the binding environment and per-use enum list searches
silent: no
ruling: Outside the spec: performance — Check walked every non-Copy binding at every match arm; enums found by list search
-->

**Status:** FIXED (branch `wip/bug-290`)
**Found:** 2026-10-26, probing Resolve, Check and Elab with scaling shapes (the R-0550 follow-up,
task 3).
**Severity:** loud (performance). Compile time only; every diagnostic and the emitted code are
unchanged.

## Reproducer

`enum-uses` (`~/perfscale-tools/probe_gen.py`; the same program as the `run_tests.sh` leg): n
declarations `enum E_i { A { v: i64 }, B }`, and in `main`, for each i,
`let e_i: E_i = E_i::A { v: 1 };` and `match e_i { E_i::A { v } => { t = t + v; }, E_i::B => { t = t + 2; }, }`.
`--emit-trace-json` on the dev host, main 99084f4f, ms / allocation heartbeats:

| n | `check` | `lower` |
|---|---|---|
| 1000 | 102 / 5.7 M | 59 |
| 2000 | 491 / 21.4 M | 254 |
| 8000 | 10088 / 325.9 M | 2807 |
| 10000 | 15720 / 507.4 M | 3661 |

Callgrind, 1000 to 2000: 3.16 G to 9.25 G instructions; the tree lookups of `VarEnv.ofSeqs`
(behind `VarEnv.linear`) 565 M to 2449 M, `VarEnv.contains` 4.0 M calls at 2000.

## Cause

Codex review, 2026-10-26:

- `Check/CheckHelpers.lean`, `checkBlockLocalsConsumed` (called once per arm): it walks
  `after.linear`, every non-Copy binding in scope, and skips each one `before` has. A consumed
  enum value stays in that subset (it is indexed by `!isCopy`), so at match i the walk visits
  about i bindings.
- `Check/Check.lean`, both `match` paths: each fall-through arm after the first compares every
  binding of `envBefore.vars.linear` between the two arms.
- `lookupEnum` and `isCopyType` search `env.enums` with `find?`; Lower's `variantIndex`,
  `variantFields` and the enum construction and enum-match cases search `enumDefs`.

## Fix

- `VarEnv.linearSince before`: the non-Copy bindings registered since the snapshot `before` (a
  range query from `before.next`, newest first). `checkBlockLocalsConsumed` walks those: every
  older non-Copy binding is one `before` has, which the check skipped (an environment checked
  inside a block descends from the snapshot it started from, as `VarEnv.closeTo` requires).
- The arms' agreement check walks `envBefore.vars.linearAmong` the bindings either arm changed
  since `envBefore` (`touchedSince`), in `linear`'s order: a binding neither arm changed has its
  `envBefore` state in both, so it cannot disagree, and the first disagreement reported is the
  same.
- `TypeEnv.enumByName` and `LowerState.enumByName`: the enums by name, the first of a name (what
  `find?` returned), built once where the environment is made (Lower: once per module, handed to
  `lowerFn`). Bug 291, stacked on this fix, replaced both by its one by-name index of every type
  declaration (`TypeEnv.typeIndex`, Lower's `Layout.Index`), with the same first-of-a-name rule, so
  the program keeps one table per stage.

`--emit-ssa` and `--emit-llvm` (stdout, stderr, exit status) of every `.con` under `tests/programs`
and `examples` (4122 compiles), 3000 `fuzz_linearity.py` cases and 2000 `fuzz_differential.py`
programs are identical with main's compiler.

## Measurements

dev host, ms, before (main 99084f4f) and after:

| n | `check` before | after | `lower` before | after |
|---|---|---|---|---|
| 2000 | 491 | 31 | 254 | 208 |
| 8000 | 10088 | 149 | 2807 | 1778 |
| 10000 | 15720 | 160 | 3661 | 1466 |

Callgrind instruction counts with the fix, 1000 to 2000: total 1.67 G to 3.30 G, `check` and
`lower` x2.1 and x2.0. Still superlinear for this shape and outside this bug: `resolve`,
`elaborate` and `coreCheck` (6, 14 and 4 ms at 1000; 239, 652 and 332 ms at 10000), where type
names are found by list search per use (`typeIdOfSpelling`, CoreCheck's `enums.find?`), the
shape of bug 291.

## Regression gate and leg

`run_tests.sh` times the `check` stage of the shape at 8000 against 600 ms, retried once at 1200
ms (149 ms with the fix; 10088 ms on main).

## Structural cause

Class 17 ("pass state in association lists and per-query recomputation"): a per-arm question
answered by walking every binding in scope, and a name table kept as a list searched per use (the
shapes of bugs 258 and 275).

## Structural owner

R-0422 (compiler throughput); R-0499/R-0514 (row 4, the checker's environments) for the binding
environment, and R-0492/R-0514 (one resolver and declaration table) for the enum tables.

Codex review 2026-10-26: PARTLY CONFIRMED — Check repeatedly scans the growing non-Copy environment per match arm, but neither a per-arm whole-environment rebuild nor an equivalent Lower merge scan explains this shape, and enum-definition searches contribute additional quadratic work.
