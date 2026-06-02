# Bug 021: Integer Match Does Not Check Consumption Agreement

**Status:** Fixed
**Discovered:** 2026-03-22
**Discovered in:** `tests/programs/bug_int_match_disagree.con`

## Description

When only some arms of an integer match expression consume a linear resource, the checker should reject it (consumption must agree across all arms). Currently it does not flag this disagreement.

```concrete
match x {
    0 => { consume(r); },
    1 => { /* r not consumed */ },
    _ => { consume(r); },
}
// should error: arms disagree on consumption of r
```

## Root Cause

Same root cause as Bug 020: the value-pattern match handler does not track or compare consumption state across arms.

## Regression Test

`tests/programs/bug_int_match_disagree.con`
