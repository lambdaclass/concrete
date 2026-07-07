# Bug 023: Short-circuit `&&`/`||` lowering emits an aggregate phi (invalid IR)

**Status:** Fixed
**Discovered:** 2026-07-07
**Fixed:** 2026-07-07
**Discovered in:** the #35 `conlog` log-analyzer workload (`examples/conlog`)
**Regression test:** `tests/programs/scand_aggregate_in_scope/` (project-mode)

## Symptom

A short-circuit `&&` or `||` expression, evaluated with a promoted aggregate
(e.g. a `String`) live in scope, produced invalid SSA — a `phi` of an
aggregate type — rejected by the SSA verifier:

```
error[ssa-verify]: (E0714) block 'scand.merge1': phi %scandphi.7 has
  aggregate type Concrete.Ty.string — use alloca+store instead
```

```concrete
let s: String = "hello";
let a: Int = 1;
let ok: bool = a == 1 && a == 1;   // <-- E0714, purely because `s` is in scope
s.drop();
```

## Root Cause

**File:** `Concrete/Lower.lean`, the `&&`/`||` short-circuit lowering
(scand/scor merge block).

The merge phis every variable whose SVal differs between the pre-RHS and
post-RHS snapshots. Its skip condition was:

```
if (← isPromoted name).isSome && !(← isAggregateForPromotion preVal.ty) then continue
```

This skips only promoted **scalars**. A promoted **aggregate** (String) was
NOT skipped, so it fell through to the naive `emit (.phi …)` path — and a
promoted variable reloads its value each snapshot, so its SVal "differs"
spuriously even when nothing changed. Result: a `phi %String`, which is
invalid IR (aggregates must be merged via memory, as the `if`/`match` merges
already do with alloca+store+load).

## Fix

Skip **all** promoted vars (they are memory-backed — reads reload, no phi
needed) and **all** aggregates (never phi an aggregate) at the scand/scor
merge:

```
if (← isPromoted name).isSome then continue
if (← isAggregateForPromotion preVal.ty) then continue
```

A variable only "changes" across a boolean `&&`/`||` RHS via reassignment,
which promotes it — so a still-unpromoted aggregate cannot have genuinely
changed, and skipping it is correct (not just safe). This mirrors bug 008
(`if/else` expression aggregate types), which routes aggregates through memory
rather than phi.

## Regression Test

`tests/programs/scand_aggregate_in_scope/` — `&&` and `||` with a live
promoted `String` in scope. Auto-built and run by the project-level test loop
(build must succeed, run must exit 0). Before the fix: build fails with E0714.
After: exit 0.
