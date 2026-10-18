# Bug 093: a block local that shadowed an outer name escaped the never-consumed check

**Status:** FIXED
**Found:** 2026-10-18, checker ownership hunt (probes `p5.con`, `p8.con`, `p12.con`; controls
`p5q.con`, `p12c.con`).
**Severity:** medium. A leak the checker is meant to reject (E0208) compiled; no memory-safety
violation. The probe `p8.con` leaked a 32 KiB `Heap<Big>` on each of 20000 calls.

## Reproducer

```
struct B { v: Int }
fn work(k: Int) with(Alloc) -> Int {
    let r: Heap<B> = alloc(B { v: k });
    free(r);
    let mut out: Int = 0;
    if k >= 0 {
        let r: Heap<B> = alloc(B { v: k });
        out = r.v;
    }
    return out;
}
```

On `64cfea38` this compiled; the inner `r` was never freed. Renaming the inner binding to `q` gave
the expected `E0208 linear variable 'q' was never consumed`. The same held for a match payload
binder: `let value: R = ..; eat(value); match mk(3) { Option::Some { value } => { .. }, .. }` leaked
the `Heap<Int>` payload, while the same program with the outer binding named `q` was rejected.

## Cause

`checkBlockLocalsConsumed` (`Concrete/Check/CheckHelpers.lean`) decided which entries of the
after-block environment were block locals with `before.any (fun (n, _) => n == name)`: any entry
whose NAME existed before the block was skipped as pre-existing. A shadowing local has the same name
as the binding it shadows, so it was never checked. Shadowing a consumed linear binding is legal
(E0292 rejects only a live one), so the shape was reachable.

The checker environment is a list of `(name, VarInfo)` with no identity per binding, and several
other checks keyed state by name in the same way: the return-path check, the if/else and match
merges (`lookupOutermost`), the post-`if` filter that drops branch locals, and the function
scope-exit check (which looked each local up by name, so a shadowed entry was checked through the
innermost one).

## Fix

`VarInfo` carries a `uid`, assigned by `addVar` from `TypeEnv.nextVarUid`; `setEnv` keeps the
larger counter, so restoring a snapshot never reissues an id. `checkBlockLocalsConsumed`,
`checkReturnPathConsumed`, `mergeVarStates`, `checkNoBranchConsumption`, both match merges and their
rebuild, the branch-local filters and the function scope-exit check now match bindings by `uid`.
`lookupOutermost` is removed.

The per-binding state updates (`consumeVar`, `useVar`, rebind, `defer`, borrow freeze) still mark
every entry with the name; that is bug 094.

## Gate

`scripts/tests/run_tests.sh`: `error_093_shadowing_block_local_leak.con` and
`error_093_shadowing_match_binder_leak.con` must fail with E0208 (both compiled on the previous
compiler); `regress_093_shadowing_locals_consumed.con` (shadowing locals and binders that are
consumed) must compile and return 42. Every other `tests/programs` fixture keeps its check outcome;
`check_examples.sh`, `check_linear_*`, `check_ownership_judgment.sh`, `check_pattern_ergonomics.sh`
pass.
