# Bug 221 — a call with the wrong number of explicit type arguments was accepted

**Status:** FIXED (branch `wip/diffhunt-1023`)
**Found:** 2026-10-23, by the diagnostics probes of the post-R-0492 differential hunt.
**Severity:** silent acceptance. `g::<Int, Int>(1)` with one type parameter, `g::<Int>(1)` on a
function with none and `s.m::<Int, u8>(5)` compiled and ran on both backends, the extra arguments
dropped; a short list (`first::<Int>(1, 2)` for two parameters) left the second parameter unbound
and was refused only by an unrelated rule.

## Reproducer

`tests/programs/error_221_extra_type_argument.con`:

```con
fn g<T>(x: T) -> T { return x; }

fn main() -> Int {
    return g::<Int, Int>(1);
}
```

Observed on main `dcc48dc1`: both backends return 1. Expected: refused. TYPE-15@1 refuses the same
shape on a named type (`Box1<i32, bool>`, whose extra argument "was once dropped silently"), and
TYPE-9@1 holds a call to its callee's signature.

## Cause

`Check.checkExprCore` used an explicit type-argument list as given: `sig.typeParams.zip typeArgs`
truncates to the shorter list, so extra arguments vanished and missing ones stayed type
variables. Only the builtins (`vec_new::<T>`, E0269) counted their type arguments.

## Fix

A call to a function, a method call, a call through a function pointer and a method call through a
trait bound whose explicit type-argument list is not exactly as long as the callee's type
parameters (a method's own, after the receiver's; none for a function pointer or a trait method;
the last two added after the Codex review) is refused with E0322 (TYPE-9, new code). An omitted
list is still inferred. Pinned by six `error_221_*.con` fixtures (`run_err ... (E0322)`) and `regress_221_exact_type_arguments.con` (42: exact and omitted lists).
Static method calls (`Type::f::<..>`) are not covered: their type-argument list may also carry the
impl's arguments, and which reading is meant is not specified.

## Structural cause

Class 13 ("documented rules with no enforcing site"): the arity of a type-argument list was enforced
for named types (TYPE-15, Resolve) and builtins (TYPE-10), and for a call's value arguments
(E0262), but nowhere for a call's type arguments; a `zip` stood in for the check.

## Structural owner

R-0499 (S8: every rule has one enforcing site), with R-0510 (S8a checked specialization: an
instantiation is built from a complete parameter-to-argument map). TYPE-9@1's sentence names value
arguments only; its type-argument half is for the specification owner to word.
