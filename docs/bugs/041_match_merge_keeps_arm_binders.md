# Bug 041: Match merge kept arm binders — stale Copy binder poisons a later same-named linear binder

**Status:** Fixed (2026-07-17)
**Fixed in:** Check.lean — both post-match merges (enum-arm path and
value-pattern path) rebuild the var table from `envBefore.vars` instead of
the last arm's env. Arm binders are arm-scoped and no longer survive the
match.
**Regression test:** `tests/programs/regress_041_match_binder_states.con`
(run_ok 42) + the negative `error_041_match_leak_still_caught.con`
(run_err E0208) pins that real leaks still fail.
**Discovered:** 2026-07-17, std compiled-coverage gate (fs fixture:
`match write_file(..)` then `match read_file(..)`).

## Repro

```text
match getn() {                       // Result<u64, u64>
    Result::Err { error } => { discard(error); return 1; },
    Result::Ok { value } => { if value != 3 { return 1; } },   // Copy binder
}
match getb() {                       // Result<Bytes, u64>
    Result::Err { error } => { discard(error); return 1; },
    Result::Ok { value } => { let n: u64 = value.len(); value.drop(); ... },
}
// error[check]: (E0208) linear variable 'value' was never consumed
```

The second `value` IS consumed. Requires the scrutinees to be direct call
results and the first binder to be Copy.

## Root cause

The post-match merge computed `vars' := env.vars.map …` where `env` was the
LAST ARM's environment — including the arm's binder entries — and then
`setEnv { envBefore with vars := vars' }`. So the first match left a stale
`value: u64 (Copy, unconsumed)` in the outer scope. At the second match, the
merge patches states by NAME via `lookupOutermost` (= outermost entry): the
new `value: Bytes (consumed)` entry got its state overwritten from the stale
outermost `value: u64 (unconsumed)` — and the function-exit scope check
raised E0208 on a properly-consumed variable.

Direction matters: linear-then-Copy was checked correctly (the stale
consumed entry patches a Copy binder, harmless), which is why workloads
never hit it — the canonical `Result::Ok { value }` payload name only
collided Copy-then-linear here. The leak direction was verified NOT
fail-open before the fix (a leaked second binder was still caught).

Same family as bugs 039/040: identity/scoping by bare name in a
function-flat table. The fix mirrors bug 040's CoreCheck fix at the Check
(ownership) layer.
