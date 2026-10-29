# Bug 214 — a linear binding left unconsumed by two returns is refused twice with the same text and span, naming neither return

<!-- bug-ledger
summary: a linear binding left unconsumed by two `return`s got two byte-identical E0208 refusals at its declaration, naming neither exit; each refusal now carries a related span naming its `return`. Pinned by `error_214_two_returns_leak.con` (two `run_tests.sh` legs). Found by the probe programs of Phase 7 workload 9 (lzpack)
fixtures: error_214_two_returns_leak.con
class: 15 — a per-exit refusal named the binding but not the exit
silent: no
ruling: Violates: unrecorded
-->

**Status:** FIXED (branch `wip/workload-lzpack`)
**Found:** 2026-10-23, by the probe programs of Phase 7 workload 9 (`examples/lzpack`): a probe
that leaked a buffer on an early return and on the final return got two identical E0208 refusals.
**Severity:** diagnostic quality. The program is correctly refused, but the two refusals were
byte-identical (same message, same declaration span), so the user could not tell that two paths
leak, nor which ones.

Codex review 2026-10-25: NOT CONFIRMED — intended behaviour: OWN-1 correctly requires consumption on each exit; both E0208 refusals were valid. No documented contract requires distinct return-site notes.

## Reproducer

`tests/programs/error_214_two_returns_leak.con`:

```con pseudocode
mod m {
    struct R { x: i32 }
    fn f(n: i32) -> i32 {
        let b: R = R { x: 1 };
        if n == 0 { return 0; }
        return n;
    }
    fn main() -> i32 { return f(1); }
}
```

Observed at `eb122a01` (origin/main): two copies of
`8:9: error[check]: (E0208) linear variable 'b' was never consumed`, each pointing at the `let`.
With the fix each refusal carries a related span naming its exit:
`related (9:21): 'b' is still unconsumed when this `return` leaves the function` and
`related (10:9): …`.

## Cause

Bug 149 made every `return` check that each live non-Copy binding is consumed
(`checkReturnAccountsForAll`). It refuses at the binding's declaration span, and the statement
loop recovers and continues, so each leaking `return` produces its own E0208. The refusal carried
the binding (the subject) but not the exit (the path), so the per-path obligation was reported as
if it were a per-binding one.

## Fix

`checkReturnAccountsForAll` attaches the `return` statement's span as a related span with a note
when the primary span is the declaration. The function-end check (`checkScopeExit`) is unchanged:
it has no exit span to name, and it reports a different exit (the fall-through end). Which
programs are refused is unchanged.

## Structural cause

Ownership state not modeled along control-flow paths (class 15), in the diagnostic: the use-once
obligation is discharged per exit path, while the refusal named only the binding.

## Structural owner

R-0499 (S8 use-once and conservation at reachable exits): when the obligation is checked per
reachable exit over Core, the refusal's subject is the pair (binding, exit), and this note becomes
the primary location rather than a related one.

## Regression

`error_214_two_returns_leak.con`: a `run_tests.sh` `run_err` leg on the note, and a leg requiring
one note for the `return` on line 9 and one for the `return` on line 10.
