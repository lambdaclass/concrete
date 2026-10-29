# Bug 224 — a failed `let mut` is recovered as an immutable binding

<!-- bug-ledger
summary: a `let mut` whose initializer failed to check was recovered as an immutable binding, so every later assignment to it got a false E0217 next to the real error; the recovered binding now keeps the declaration's mutability. Pinned by `error_224_failed_let_keeps_mutability.con`. Found by the bug 222 reproducer on main (Phase 7 workload 10)
fixtures: error_224_failed_let_keeps_mutability.con
class: 12 — a recovered `let` restated by hand with its mutability fixed to false
silent: no
ruling: Violates: unrecorded
-->

**Status:** FIXED (branch `wip/workload-stackvm-2`)
**Found:** 2026-10-24, by the bug 222 reproducer on main (Phase 7 workload 10): the refused
`total(&b)` initializer was followed by a second, false error on the next line.
**Severity:** loud (diagnostics). A program already refused for a real error got a false second
error: `cannot assign to immutable variable 'r'` (E0217) for a variable declared `let mut r`.

Confirmed real by Codex review, 2026-10-25: Recovery hardcoded mutable=false for let mut, generating false E0217 errors contrary to SAFETY.md and DIAGNOSTICS.md’s non-cascading recovery contract.

## Reproducer

`tests/programs/error_224_failed_let_keeps_mutability.con`:

```con
let mut r: i64 = need(&s);   // E0241: S does not implement A
r = r + 1;                   // E0217 cannot assign to immutable variable 'r'  (false)
let q: i64 = need(&s);       // E0241
q = 2;                       // E0217 (correct: q is not mut)
```

Observed on main `408880cb`: two E0241 and two E0217. Expected: two E0241 and the E0217 for `q`
only.

## Cause

`Check.checkStmtsCore` recovers from a statement that fails: it restores the environment and, for
a `let`, adds the binding with its declared type (or a placeholder) so later statements do not
cascade. It added that binding with `mutable := false` whatever the declaration said; the pattern
bound the declaration's mutability flag to `_`.

## Fix

The recovered binding takes the declaration's mutability. Pinned by
`error_224_failed_let_keeps_mutability.con` (`run_tests.sh`: E0241, and E0217 at line 14 and not
at line 12).

## Structural cause

Class 12 ("one fact kept in two places"): the recovery path re-created the binding by hand instead
of from the declaration, so a fact of the declaration (its mutability) was restated, and restated
wrong. A recovered binding that differs from the declared one turns one error into a cascade.

## Structural owner

R-0499 (S8: one enforcing site per rule) for the checker's binding rules: a binding is introduced
from its declaration in one place, used by the ordinary path and the recovery path alike. Until
then the recovery site reads every field the declaration has.
