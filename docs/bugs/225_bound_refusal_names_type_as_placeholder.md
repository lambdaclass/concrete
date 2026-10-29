# Bug 225 — a refused bound names a builtin type `<type>`, and no E0241 has a position

<!-- bug-ledger
summary: a refused `Copy` bound named every builtin type `<type>` (`v.get_unchecked(0)` on a `Vec<String>`), and no E0241 carried a position: each branch of `checkTraitBounds` rendered the type itself and none had the call's span. One rendering and the call's span for every refusal. Pinned by `error_225_bound_names_builtin_type.con`. Found by the probe programs of Phase 7 workload 10 (stackvm)
fixtures: error_225_bound_names_builtin_type.con
class: 12 — the type a refused bound names, rendered per branch; one copy printed `<type>`
silent: no
ruling: Violates: unrecorded
-->

**Status:** FIXED (branch `wip/workload-stackvm-2`)
**Found:** 2026-10-24, by the probe programs of Phase 7 workload 10 (reading a `Vec<String>`
element with `get_unchecked`).
**Severity:** loud (diagnostics). The refusal is right; the message does not say which type was
refused or where. `v.get_unchecked(0)` on a `Vec<String>` printed
`error[check]: (E0241) type '<type>' does not implement trait 'Copy' required by method
'get_unchecked'`, with no line and column. Every E0241 lacked a position, whatever the type.

Confirmed real by Codex review, 2026-10-25: Bound failures discarded available type names and call spans, violating SAFETY.md/DIAGNOSTICS.md’s source-located semantic diagnostic contract; Copy policy need not change.

## Reproducer

`tests/programs/error_225_bound_names_builtin_type.con`:

```con
fn dup<T: Copy>(x: T) -> T { return x; }
let t: String = dup("ab");                 // line 12
let w: W<String> = W::<String> { v: "cd" };
let u: String = w.get();                   // line 14: impl<T: Copy> W<T>
```

Observed on main `408880cb`:

```text
error_225_bound_names_builtin_type.con: error[check]: (E0241) type '<type>' does not implement trait 'Copy' required by generic function 'dup'
error_225_bound_names_builtin_type.con: error[check]: (E0241) type '<type>' does not implement trait 'Copy' required by method 'get'
```

Expected: `:12:25:` and `:14:25:`, naming `String`.

## Cause

`CheckHelpers.checkTraitBounds` refuses an unsatisfied bound in four branches (Copy, Destroy, a
named trait on a named type or a type variable, a named trait on any other type). Each rendered
the refused type itself: the Copy branch printed every type that is not a named type, a generic or
a type variable as `<type>`, while the Destroy branch and the last branch used `tyToString`. None
of them passed a span, and the function had no span to pass: its two callers (a generic function
call, a method call) had the call's span and did not hand it over.

## Fix

`checkTraitBounds` takes the call's span, and one local `refuse` renders the refused type with one
rule (a named type, generic or type variable by its name; anything else by `tyToString`) and
throws at that span; every branch refuses through it. Both callers pass the call's span. Pinned by
`error_225_bound_names_builtin_type.con` (`run_tests.sh`: E0241 naming `String` at lines 12 and
14, no `<type>`).

## Structural cause

Class 12 ("one fact kept in two places"): how a refused bound names its type was restated per
branch, and one copy fell back to a placeholder. The missing position is the same shape: the
refusal was built where the span was not, instead of being passed the call it judges.

## Structural owner

R-0499 (S8: every rule has one enforcing site): TRAIT-10's bound check is that site, and with it the
one place its diagnostic is built.
