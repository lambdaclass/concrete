# Bug 293 — the Report safety gate every compile runs re-walked nested branches at every level

<!-- bug-ledger
summary: every compile (`-o`, `--emit-llvm`) runs Report's safety gate (provenViolationDiagnostics: the bounds, division and overflow obligation walks), whose shared scoped walker asked for the scalars each statement assigns by walking the statement's whole subtree, filtered the whole hypothesis list after every statement, and copied the hypothesis scope at every nesting level, so a function nested n deep took time n² to compile even with no obligation in it (10000 nested ifs: --emit-llvm 9.5 s with bug 286 fixed, of which the stages through lower 0.25 s). Found re-measuring the R-0550 nesting shapes with whole compiles. Class 17.
coverage: run_tests.sh
class: 17 — the scoped obligation walker re-walked and re-copied per level
silent: no
ruling: Violates: unrecorded
-->

**Status:** FIXED (branch `wip/bug-293`)
**Found:** 2026-10-26, timing whole compiles of the scaling gate's `nested-ifs` shape after bug
286 (R-0550 follow-up): the in-compiler stages had become linear, `--emit-llvm` had not.
**Severity:** loud (performance). Compile time only; every report and diagnostic is unchanged.

## Reproducer

`nested-ifs` (`if t > 0 { s = s + 1; ... }` nested n deep in `main`), `concrete <file>
--emit-llvm`, dev host, with bug 286's fix: n = 2000: 0.41 s, 4000: 1.61 s, 8000: 5.99 s,
10000: 9.5 s; 100000 did not finish in 20 minutes. The stages through `lower`
(`--emit-trace-json`) take 0.25 s at 10000. Callgrind of `--emit-llvm` at 2000 (7.1 G
instructions): `List.filter` in `Report.dropStaleHyps` 792 M, `Report.collectIdents` 952 M,
`Report.assignedScalarsGo` 344 M, `List.reverseAux` 943 M.

## Cause

`Main.compileSSA` runs `enforceProvenRuntimeViolations` on every compile, which calls
`Report.provenViolationDiagnostics`: `boundsObligations` and `divObligations` walk every function
body with the shared scoped walker (`scopedWalkSizedB`/`S` in `Report/ReportObligations.lean`), and
`overflowObligations` does for `overflow`-checked functions. For every statement with code after it
the walker asked `assignedScalarsS s`, a walk of the statement's whole subtree, then
`dropStaleHyps`, a filter of the whole hypothesis list (each hypothesis's identifiers collected
again); and it entered every branch with `scope ++ [c]`, a copy of the scope. In an n-deep nest the
subtree walks, the filters and the copies each total n² (Codex review 2026-10-26 traced all three).

## Fix

The walker threads the scope as `ScopeHyps`: the hypotheses newest first (entering a branch is a
cons) with the set of identifiers they name, and hands each leaf the ordered list as a `Thunk`,
forced only by a leaf that emits an obligation. A statement's walk returns the scalars it assigns
(the same set `assignedScalarsS` computes, case by case: nothing for a `borrow … in` block), so no
subtree is walked twice. The invalidation after a statement filters only when the statement assigns
an identifier some hypothesis names; otherwise the filter would keep every hypothesis and the scope
is returned as it was. The order of the hypotheses, the obligations and their keys are unchanged.

`--emit-llvm` and the `obligations`, `obligation-ledger`, `vcs`, `vcgen-diff`, `arithmetic` and
`contracts` reports of every `.con` under `tests/programs` and `examples` are byte-identical with
main's compiler; `check_scoped_collector.sh`, `check_no_duplicate_obligation_walkers.sh`, the
bounds, division, overflow, call-site and assume migration gates and
`check_constructor_coverage.sh` pass. The fact-invalidation mutation
(`check_gate_mutation_coverage.sh`) is re-anchored on the new invalidation arm and still killed by
`check_scoped_collector.sh`.

What remains superlinear: the filter, when it runs, visits every hypothesis in scope. A nest whose
every level adds a guard on a variable it then assigns (filtered at each level) under a growing
set of guards on variables no level assigns (kept, and visited by each filter) keeps a quadratic
term; the gate's shapes and the corpus have no such nest.

## Measurements

`--emit-llvm`, dev host, ms:

| depth | before (main's walker) | after |
|---|---|---|
| 2000 | 891 | 323 |
| 4000 | 3317 | 979 |
| 8000 | 17105 | 2449 |

(both without bug 286's fix, so `lower` is still quadratic in both; with it the remaining time is
linear, see the regression leg.)

## Regression gate and leg

`run_tests.sh` times `--emit-llvm` of 10000 nested ifs against 3000 ms, retried once at 6000 ms.
The scaling gate's `nested-ifs` whole compiles (`llvm`, `native`) are budgeted at 10^4 and 10^5
in the R-0550 follow-up.

## Structural cause

Class 17 ("pass state in association lists and per-query recomputation"): a fact of a subtree
recomputed by a walk at every level above it, and a scope copied per level; the shape of bugs 178,
280, 281 and 286.

## Structural owner

R-0422 (complexity envelopes); the obligation walker is the ONE scoped collector of ROADMAP
Phase 3 #3, which now owns the synthesized assigned-set and the threaded scope.

Codex review 2026-10-26: CONFIRMED — ordinary compilation unconditionally runs Report's safety gate, whose shared scoped walker repeatedly scans nested subtrees and growing guard lists, producing quadratic work even when no relevant obligations exist.
