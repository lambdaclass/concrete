# Bug 297 — Elab and Lower appended each call argument, and Lower each arm's merge input, to the list built so far

<!-- bug-ledger
summary: Elab (elabCallEv and the method, static-method, trait-bound and indirect call cases of elabExprEv) and Lower (lowerExpr's call case, its `&mut` write-backs, and reconcileBranchVars' per-arm incoming list) built their lists with `xs := xs ++ [x]` per argument or per arm, so one call of n arguments took time n² to elaborate and lower (8000 arguments: elaborate 145 ms, lower 126 ms) and one match of n arms took n² to merge each binding it changes (8000 arms: lower 583 ms, half of it the dominator residue of bug 178). Found by scaling probes beyond the R-0550 shapes. Class 17.
fixtures: regress_297_argument_order.con
class: 17 — call arguments and merge inputs appended one at a time
silent: no
-->

**Status:** FIXED (branch `wip/bug-297`)
**Found:** 2026-10-26, probing shapes beyond the R-0550 gate's (one function of n parameters
called with n arguments; one match of n literal arms each assigning a local).
**Severity:** loud (performance). Compile time only: the emitted IR is unchanged.

## Reproducer

Stage times (`--emit-trace-json`, ms, dev host, main 1bcb7c36), n = 2000 → 8000:

| shape | stage | 2000 | 8000 |
|---|---|---|---|
| many-params (`fn f(p0: i64, …)` called as `f(1, 1, …)`) | elaborate | 9 | 145 |
| many-params | lower | 7 | 126 |
| wide-match (`match t { 0 => { s = 0; }, … }`) | lower | 38 | 583 |

Heartbeats grow fourfold: the appends copy cells, not small allocations the counter sees as
quadratic. Callgrind at 2000: `List.appendTR` 44.1 M instructions from the argument loop of
`elabCallEv`, 44.1 M from `lowerExpr`'s, and 88.2 M from `reconcileBranchVars` (4002 calls) in
wide-match.

## Cause

- `Elab/Elab.lean`: `elabCallEv` (ordinary and indirect calls) and `elabExprEv`'s trait-bound,
  instance-method and static-method calls build `cArgs := cArgs ++ [cArg]`; a struct literal's
  fields `cFields ++ [...]`; an enum arm's payload bindings `typedBindings ++ [...]`; a desugared
  `print`/`println`/`append` statement list `stmts ++ [...]` per argument.
- `IR/Lower.lean`: `lowerExpr`'s call case `aVals := aVals ++ [...]` and
  `mutBorrows := mutBorrows ++ [...]` per argument; `reconcileBranchVars`
  `incoming := incoming ++ [(contrib, endLabel)]` per live arm, for every binding the merge visits
  (those an arm wrote and every non-register one).

Each append copies the list built so far: 0 + 1 + … + (n − 1) cells. Codex review 2026-10-26
confirmed the sites and listed the per-field and per-argument sites of the same kind; the
fixed-arity intrinsic argument lists (`vec_push`, `string_append`, …) and the loop break/continue
edge lists are not changed here (fixed arity, or per loop exit).

## Fix

Every one of those lists is consed newest first and reversed once where it is used, so argument
order, write-back order, field order and the merge's incoming order are what the appends built.
`--emit-ssa` and `--emit-llvm` (stdout, stderr, exit status) of every `.con` under
`tests/programs` and `examples`, and 3000 generated programs, are byte-identical with main.

## Measurements

| shape | stage | n | before | after |
|---|---|---|---|---|
| many-params | elaborate | 8000 | 145 | 9 |
| many-params | lower | 8000 | 126 | 5 |
| many-params | elaborate / lower | 12000 | 353 / 324 | 13 / 7 |
| wide-match | lower | 8000 | 583 | 316 |

The rest of wide-match's `lower` is SSAVerify's dominator construction, the residue bug 178
recorded; it is fixed on the stacked bug-178 follow-up branch.

## Regression gate and leg

`run_tests.sh` times `elaborate` and `lower` of a call of 12000 arguments against 60 ms each,
retried once at 120 ms (13 and 7 ms with the fix; 353 and 324 ms on main's compiler).
`regress_297_argument_order.con` pins argument order, `&mut` write-back order, field order and a
match's merged values (binary 42, as `--interp`). `check_no_quadratic_append.sh`'s Lower baseline
goes down 20 → 15 and Elab's 53 → 31 (the tree held 41 before the fix).

## Structural cause

Class 17: a list built by appending each element to the accumulated list (the shape of bugs 236,
249, 255 and 273), in the per-call and per-merge accumulators of two passes.

## Structural owner

R-0422 (complexity envelopes); `check_no_quadratic_append.sh` holds the ratchet.

Codex review 2026-10-26: CONFIRMED — Both call-argument builders and branch reconciliation perform quadratic growing-list appends, with ordinary-call elaboration located in `elabCallEv` and reconciliation also affecting unchanged non-register bindings.
