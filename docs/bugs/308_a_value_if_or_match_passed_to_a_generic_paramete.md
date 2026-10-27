# Bug 308 — a value if or match passed to a generic parameter left its type variable uninferred, so Check refused the call

<!-- bug-ledger
summary: generic inference types each argument with a side-effect-free peek (`peekExprTypeIn`, shared by Check and Elab) before checking it; the peek had no case for a value `if` or `match` and answered `.placeholder`, so a bare type parameter given only such an argument stayed unbound and Check refused the call (`gid(if c { 1 } else { 2 })`: E0220, "expected <unknown>, got i64")
fixtures: regress_308_generic_arg_value_if.con
class: 12 — an expression's type answered twice, by the full checker and by the inference peek, and the peek lacked two forms
silent: no
-->

**Status:** FIXED (branch `wip/bug-308`)
**Found:** 2026-10-26, probing other spellings of bug 304 during the Lower borrow hunt.
**Severity:** loud. A well-typed program is refused (by `--interp` and the compiler alike).

## Reproducer

```
fn gid<T>(x: T) -> T { return x; }
fn a(c: bool) -> i64 { return gid(if c { 1 } else { 2 }); }
fn main() with(Console) { println(a(true)); }
```

## Observed

On main ea21a2e8 both `--interp` and the compiler refuse it: `error[check]: (E0220) type mismatch in
argument 'x' of 'gid': expected <unknown>, got i64` (TYPE-4@1). Binding the value `if` to an `i64`
first is accepted. A value `match` argument fails the same way. `regress_308_generic_arg_value_if.con`
covers both, arms whose values are arm-local typed `let`s, and a wildcard arm; main refuses it.

## Expected

The argument is an `i64`, so `T := i64`; the program is well typed and runs as `--interp` would.

## Cause

`Resolve/PeekType.lean`, `peekExprTypeIn`: its cases cover literals, names, literals of aggregates,
operators, casts, borrows, places and calls, and every other form falls to `.placeholder`, which
contributes no binding to inference (`recordInferred`). `ifExpr` and `match_` were among the others.
With no binding for `T`, Check compared the argument against the unresolved parameter type
(Codex confirmation, 2026-10-26).

## Fix

The peek answers a value `if` and `match` with the type of the first arm, in order, whose
trailing value it can type (`peekFirstArmValue`, `peekArmValue`). The peek has no scope for the
names an arm binds, and such a name could resolve to an outer constant of the same spelling (Codex
review round 1), so an arm that binds names (a pattern binder other than `_`, or a `let`) is read
only when its trailing value names one of its own `let`s written with a type, which gives that
type; an arm that binds nothing is peeked as any expression; an arm that diverges or ends in `;`
has no value. The peek is the one both passes use, so Check and Elab infer the same `T`.

## Structural cause

Class 12: one fact kept in two places. An expression's type is answered by the full checker and,
ahead of it, by the inference peek, whose case list is a second statement of the same typing rules
and lacked the value-bearing control-flow forms.

## Structural owner

R-0499 (S8: one typing judgment over Core, with inference reading the checked types instead of a
peek). Until then the peek covers the value forms.

Codex confirmation 2026-10-26: CONFIRMED — The shared argument-type peek omits value `if`, causing generic inference to bind the bare parameter to a placeholder and Check to reject a well-typed i64 argument with E0220.
