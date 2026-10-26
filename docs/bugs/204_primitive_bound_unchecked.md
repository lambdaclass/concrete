# Bug 204 — a trait bound on a primitive type argument was never checked

<!-- bug-ledger
summary: a trait bound on a primitive type argument (`call::<Int>` with `T: Tr`) was never checked: with the bound's method unused the program ran on both consumers, and with it called it failed in `llvm-as` / as an undefined function under `--interp`. `checkTraitBounds` now needs an impl of the canonical type (E0241). Pinned by `error_204_primitive_bound_unchecked.con`, `error_204_primitive_bound_unused.con` and `regress_204_primitive_bound_satisfied.con`. Found by the Codex review of the specification's traits chapter (TRAIT-10)
fixtures: error_204_primitive_bound_unchecked.con error_204_primitive_bound_unused.con regress_204_primitive_bound_satisfied.con
class: 13 — a bound on a primitive type argument: `checkTraitBounds` skipped every unnamed shape
silent: yes
exit: 0b
-->

**Status:** FIXED (branch `wip/bug204`, based on `wip/train6` `13bc2527`)
**Found:** 2026-10-23, by the Codex review of the language specification's traits chapter
(TRAIT-10 claims every bound is satisfied by an impl; `checkTraitBounds` skipped primitive type
arguments), then probed on both consumers
**Severity:** silent when the bound's methods are not called: `noop::<Int>` with `T: Tr` and no
impl of `Tr` for `Int` compiled and ran on both consumers. When the body called the bound's method
it failed closed late: natively in `llvm-as` (`@Int_m` undefined), under `--interp` as an
undefined function.

Confirmed real by Codex review, 2026-10-25: docs/spec/traits.md TRAIT-10 requires an impl for every explicit bound; the pre-fix primitive branch skipped that check entirely.

## Reproducer

```con pseudocode
trait Tr { fn m(&self) -> Int; }
fn call<T: Tr>(x: &T) -> Int { return x.m(); }
fn main() -> Int { let n: Int = 42; return call::<Int>(&n); }
```

Expected: refused (E0241, `Int` does not implement `Tr`). Observed on `wip/train6` (spec-t6
`c170057d`): natively `llvm-as` fails with `use of undefined value '@Int_m'`; `--interp` fails
with `undefined function 'T$m'`. With the call to `m` removed, both consumers return 42.

## Cause

`Concrete/Check/CheckHelpers.lean`, `checkTraitBounds`: for a trait other than `Copy` and
`Destroy` it looked up an impl only for a named or generic type argument and a caller's type
parameter; every other shape fell to `| _ => pure ()`, commented "primitive types, skip". Impls
of primitives are legal (TRAIT-D1 resolves `impl Tr for i64` to the canonical type), so the
skipped case was a real one.

## Fix

Every other shape needs an impl whose head is the same canonical type (`tyCanonical`, so `Int`
and `i64` are one type, as coherence reads them). Fixtures: `error_204_primitive_bound_unchecked.con`
and `error_204_primitive_bound_unused.con` (E0241) and `regress_204_primitive_bound_satisfied.con`
(an `impl Tr for i64` used as `Int` and `i64`, run 42), in `run_tests.sh`. A rule-impact scan over
the 2,225 units of the train 6 corpus found no program newly refused.

## Structural cause

Class 13, a documented rule with no enforcing site for one case: the bound check enumerated the
type shapes it knew to have impls and accepted the rest.

## Structural owner

R-0499 (one checker over Core, where a bound is discharged by the impl table for every type).
