# Bug 020: Integer Match Does Not Propagate Consumption State

<!-- bug-ledger
summary: Integer Match Does Not Propagate Consumption State
fixtures: bug_int_match_consume.con
area: Check — the record's Root Cause: the linearity checker's value-pattern match; no commit names the bug
ruling: Violates: unrecorded
-->

**Status:** Fixed
**Discovered:** 2026-03-22
**Discovered in:** `tests/programs/bug_int_match_consume.con`

## Description

When all arms of an integer match expression consume a linear resource, the checker reports "never consumed" at scope exit. The consumption state from inside match arms is not propagated to the enclosing scope.

```concrete
match x {
    0 => { consume(r); },
    1 => { consume(r); },
    _ => { consume(r); },
}
// checker says r was never consumed
```

## Root Cause

The value-pattern match handler does not merge consumption state from arms back into the parent scope. This is a known gap in the linearity checker's match handling for integer patterns.

## Regression Test

`tests/programs/bug_int_match_consume.con`
