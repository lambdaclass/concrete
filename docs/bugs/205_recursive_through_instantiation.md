# Bug 205 — a struct containing itself through a generic instantiation passed the finite-size check

<!-- bug-ledger
summary: a struct holding itself by value through a generic instantiation (`Node { w: W<Node> }`) passed the finite-size check; natively `llvm-as` refused the recursive type while `--interp` ran it. `tyReachesByValue` now follows instantiations with their arguments substituted (E0583). Pinned by `error_205_recursive_through_instantiation.con` and `regress_205_instantiations_without_cycle.con`. Found by the Codex review of the specification's TYPE-16
fixtures: error_205_recursive_through_instantiation.con regress_205_instantiations_without_cycle.con
class: 13 — finite size judged on declarations and assumed for generic instantiations
silent: yes
exit: 0b
ruling: Violates: unrecorded
-->

**Status:** FIXED (branch `wip/bug205`, based on `wip/train6` `13bc2527`)
**Found:** 2026-10-23, by the Codex review of the language specification's TYPE-16 (a struct has a
finite size; `tyReachesByValue` did not traverse generic instantiations), then probed on both
consumers
**Severity:** silent on the interpreter, fails closed natively: `struct Node { a: Int, w: W<Node> }`
with `struct W<T> { v: T }` reached `llvm-as`, which refused the recursive struct type, while
`--interp` ran the program.

Confirmed real by Codex review, 2026-10-25: docs/spec/types.md TYPE-16 forbids by-value recursion through instantiations; the pre-fix traversal skipped generic fields.

## Reproducer

```con pseudocode
struct Copy W<T> { v: T }
struct Copy Node { a: Int, w: W<Node> }
fn main() -> Int { return 42; }
```

Expected: refused (E0583, `Node` has infinite size). Observed on `wip/train6` (spec-t6
`c170057d`): natively `llvm-as` fails with `identified structure type 'struct.W_Node' is
recursive`; `--interp` returns 42.

## Cause

`Concrete/Check/CoreCheck.lean`, `tyReachesByValue`: the walk followed named structs, named
enums and arrays, and stopped at every other type, including `.generic`, on the stated grounds
that an instantiation's fields are "settled during monomorphization". Monomorphization lays the
instantiation out inline, so the cycle reached LLVM.

## Fix

An instantiation is followed through its declaration's fields with the type arguments
substituted (`Layout.substTyVars`). The visited key is the instantiated type (`B<Int>` and
`B<Node>` differ). An instantiation that grows without reaching the target is refused as its
own declaration's target, so the walk's depth bound (64) is reached only by deeper literal
nesting, and it fails closed there. The E0583 hint no longer offers
a reference, which cannot be a field (REF-3). Fixtures:
`error_205_recursive_through_instantiation.con` (E0583) and
`regress_205_instantiations_without_cycle.con` (nested instantiations, run 42), in `run_tests.sh`.
A rule-impact scan over the 2,225 units of the train 6 corpus found no program newly refused.

## Structural cause

Class 13, a documented rule with no enforcing site for one case: finite size was judged on
declarations and assumed for instantiations, which are where layout actually happens.

## Structural owner

R-0502 (one `Layout` authority, which computes every instantiated layout and can refuse an
infinite one where it is computed).
