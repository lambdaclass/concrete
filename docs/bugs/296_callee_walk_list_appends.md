# Bug 296 — the callee walk appended each statement's callees to the list built so far

<!-- bug-ledger
summary: the one callee walk (Proof/CallCollect.lean: collectCallees*, and the function-value and method-value walks beside it) built each list by appending a statement's, argument's, field's, arm's or element's items to the list built so far, and the post-mono capability verifier deduplicated a function's callees with List.eraseDups (pairwise), so a function of n calls took n² in Elab (which collects every body twice) and in the monomorphize stage (20000 calls: elaborate 2681 ms, monomorphize 2440 ms). Found probing the front end after R-0550 (task: Resolve/Check/Elab quadratics). Class 17.
coverage: run_tests.sh
class: 17 — the callee walk's per-statement list appends
silent: no
ruling: Violates: unrecorded
-->

**Status:** FIXED (branch `wip/bug-296`, based on `wip/bug-295`)
**Found:** 2026-10-26, probing the front end with the `many-calls` shape (n one-line functions,
`main` calling each once), after bugs 285 – 293.
**Severity:** loud (performance). Compile time only; every callee list, report and diagnostic is
unchanged.

## Reproducer

`many-calls`, stages of `--emit-trace-json`, dev host (fede-256gb), main's compiler (bug 295's
branch, which changes only the parser):

| n | 2000 | 8000 | 10000 | 20000 |
|---|---|---|---|---|
| `elaborate`, ms | 16 | 262 | 423 | 2681 |
| `monomorphize`, ms | 18 | 386 | 581 | 2440 |

Callgrind at 2000 on main 1bcb7c36: `List.appendTR` called from `collectCalleesStmts` 132.7 M
instructions (12,006 calls), reached from `elabModule`, `spellMethodCallsApart` and
`verifyCapsPostMono`.

## Cause

`Proof/CallCollect.lean`: `collectCalleesStmts` was `ss.foldl (fun acc s => acc ++
collectCalleesStmt s) []`, and the expression, arm and statement cases appended their children's
lists the same way (arguments, fields, arms, elements); the function-value and method-value walks
(`collectFnValueRefs*`, `collectMethodValueRefs*`) repeated the shape. `Elab.elabModule` and
`Elab.spellMethodCallsApart` collect every body, and `Check/Verify.lean`'s
`verifyCapsPostMono` (run inside `Pipeline.monomorphize`) collects every body and then applies
`List.eraseDups` to its (callee, namespace) pairs, a comparison with every pair kept before
(Codex review 2026-10-26).

## Fix

The three walks are one accumulating walk (`collectGo*`, parameterised by the items an expression
contributes itself: a call's callee, a function value, a method value) that visits
sub-expressions, arms and statements in source order, prepends to an accumulator and is reversed
once; `collectCallees*`, `collectFnValueRefs*` and `collectMethodValueRefs*` keep their
signatures and results. `verifyCapsPostMono` keeps the first occurrence of each (callee,
namespace) in order with a hash set. `--emit-llvm` and the `assumptions`, `proof-deps`,
`recursion`, `stack-depth`, `trust-edges`, `caps` and `proof-status` reports of every `.con`
under `tests/programs` and `examples` are byte-identical with bug 295's compiler.

## Measurements

`many-calls`, ms, dev host, before → after:

| n | `elaborate` | `monomorphize` |
|---|---|---|
| 2000 | 16 → 9 | 18 → 3 |
| 8000 | 262 → 87 | 386 → 23 |
| 10000 | 423 → 136 | 581 → 55 |
| 20000 | 2681 → 315 | 2440 → 174 |

Callgrind with the fix: `elaborate` and `monomorphize` grow 2.0 times from 2000 to 4000 calls
(129 M → 259 M and 78 M → 156 M instructions).

## Regression gate and leg

`run_tests.sh` elaborates and monomorphizes a `main` of 20000 calls within 800 and 600 ms,
each retried once at twice its budget (main's compiler: 2681 and 2440 ms).

## Structural cause

Class 17 ("pass state in association lists and per-query recomputation"): a list built by
appending each part's list to the list built so far, three times over in one file, plus a
pairwise deduplication; the shape of bugs 255 and 281.

## Structural owner

R-0422 (complexity envelopes); the one callee walk in `Proof/CallCollect.lean` (the call graph's
single source) now owns the accumulation for all three views.

Codex review 2026-10-26: CONFIRMED — Growing-prefix callee collection creates quadratic work in both timed stages for this shape, and post-mono list deduplication adds an independent quadratic cost.
