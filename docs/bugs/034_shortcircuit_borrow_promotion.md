# Bug 034: `&&`/`||` RHS borrow skipped lazy-promotion pre-store — silent neighbor corruption

**Status:** Fixed (2026-07-16)
**Fixed in:** Lower.lean short-circuit `.and_`/`.or_` lowering — now calls
`prePromoteAddrTaken` (the bug-031 fix) before branching, exactly like the
ifElse/ifExpr/match sites already did. The RHS block can address-take a
local (any method call borrows its receiver); without pre-promotion the
lazy alloca+store landed INSIDE the `scand.rhs` block and did not dominate
later uses.
**Regression tests:** `scripts/tests/check_cli_helpers.sh` ("two positionals"
leg — under the pre-fix compiler `cli_tool a b` aborts rc=134, verified by
rebuilding the pre-fix compiler) and
`tests/programs/regress_034_shortcircuit_borrow_promotion.con` (guards the
code path shape; note it does NOT reproduce the original corruption — the
live trigger needed std.cli's String + Vec<String> frame layout, which the
cli gate covers).
**Discovered:** 2026-07-16, std.cli v1 (fourth workload-found compiler bug
family member; the 031/033 cluster's third site).

## Symptom

`std.cli.parse`'s classification line

```text
let is_flag: bool = a.len >= 2 && a.get_unchecked(0) == '-';
```

(field read in the `&&` LHS, receiver-borrowing method call in the RHS,
owned local String, inside a loop) corrupted an ADJACENT local — the
`Vec<String>` of positionals. Downstream: abort (rc 134) when the vec's
Strings were written or destroyed. Two positionals or an arity-error drop
path trapped; one positional happened to survive. Swapping the LHS to the
`a.len()` METHOD call made the trap vanish (both sides then borrow before
the branch), which is what localized the site.

## Root cause

The 2026-07 short-circuit lowering rewrote `&&`/`||` from eager to
branching, but did not inherit bug 031's `prePromoteAddrTaken` call that
every other branch-creating site (ifElse, ifExpr, match) already had. A
local first address-taken inside the RHS block got its alloca+initial-store
emitted there; entry-block allocas are assumed by later stack layout, and
the non-dominating store left the slot holding stale/foreign data on paths
where the RHS did not run — expressed as corruption of whatever the slot
overlapped.

## Class note

Third member of the "branch-created-block misses a whole-function
invariant" cluster (031 lazy promotion, 033 aggregate merge). The Lower
structured-builder work (ROADMAP #5, control-flow-builder memory) exists
precisely to make these sites share one implementation; this bug is fresh
evidence for pulling the &&/|| lowering into it next time any of the four
sites changes.
