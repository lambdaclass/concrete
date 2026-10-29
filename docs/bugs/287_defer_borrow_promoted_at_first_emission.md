# Bug 287 — a deferred call that borrows a local promoted it at its first emission, not where it registers

<!-- bug-ledger
summary: a deferred call that borrows a local (`defer copy(sink, &x);`) is emitted at every exit of its scope, and the first emission Lower lowered (a `return`, `break` or `continue` inside a branch) promoted the local lazily there, so the slot's initializing store was on that exit's path only and every other exit read a never-stored slot (silent wrong code)
fixtures: regress_287_defer_borrow_promoted_at_registration.con
class: 8 — where a deferred call's borrowed local gets its slot
silent: yes
exit: 0b
ruling: Violates: MEM-3@1
-->

**Status:** FIXED (branch `wip/bug-287`)
**Found:** 2026-10-26, writing a fixture for bug 286 (whether deferred calls are lowered inside the
region they are emitted in).
**Severity:** silent wrong code. The deferred call reads an uninitialized stack slot on every exit
but the first one lowered; `--interp` is right.

## Reproducer

```
fn copy(dst: &mut i64, src: &i64) { *dst = *src; }
fn deferred(c: bool, sink: &mut i64) -> i64 {
    let mut x: i64 = 4;
    defer copy(sink, &x);
    if c { x = 50; return 1; }
    return 0;
}
fn main() -> Int {
    let mut sink: i64 = 0;
    let a: i64 = deferred(false, &mut sink);
    let mut sink2: i64 = 0;
    let b: i64 = deferred(true, &mut sink2);
    return (sink + a + b + sink2 - 55 + 42) as Int;
}
```

Observed on main cc7a4084: the binary exits 88, `--interp` prints 42. `--emit-ssa` of `deferred`:
the slot `%addr.0` is allocated in the entry block, `store i64 50, %addr.0` is in the `then` block
before its call to `copy`, and the call on the fall-through `return` passes `%addr.0` with no store
on that path. `regress_287_defer_borrow_promoted_at_registration.con` adds two `break`s and a
`continue` out of a loop whose body registers the defer: main's binary exits 208 for it,
`--interp` 42.

## Expected

The deferred call reads the place its borrow names when it runs (OWN-12, docs/spec/ownership.md:
arguments are bound when the `defer` registers and read when it runs): 4 on the fall-through
return, 50 on the other.

## Cause

`IR/Lower.lean`: `lowerStmt`'s `.defer` case only queues the body (`addDeferredToCurrentScope`);
each exit lowers it again (`emitAllDeferredCalls` at a `return`, `emitDeferredUntilLoop` at a
`break` or `continue`, `emitFrameDeferredCalls` at a scope's end). Lowering `&x` there promotes `x`
lazily (`addrOfLocal`): the alloca goes to the entry block and the initializing store to the
current block, and the promotion is registered for the rest of the function. The first exit
lowered gets the store; every later one reuses the slot without it. Bug 031's pre-promotion at the
branch does not cover it: the branch's walk sees no `&x`, because the borrow is in the `defer`
statement outside the branch. A loop's slot-or-phi choice walks `.defer` bodies, but a local
declared inside the loop body is not among the loop's bindings (Codex review, 2026-10-26, traced
the `return`, `break` and `continue` forms).

## Fix

The `.defer` case promotes, where the defer registers, every local its body takes the address of
(`prePromoteAddrTaken` of `cexprAddrTaken` of the body): that point dominates every exit that
emits the call, the store there holds the value at registration, and later writes go through the
slot, so the call reads the value the place holds when it runs. A local already promoted, or an
array, is left as it is.

## Structural cause

Class 8: storage decided ad hoc in Lower. A local's slot was created where the first borrow
happened to be lowered, and a deferred call is lowered at several places that do not dominate one
another; the per-site pre-promotion rule (bugs 031, 034) was never applied to the registration
point.

## Structural owner

R-0501 (S10 Place MIR): the storage of every local decided once per function from the places it
uses; until then the `defer` statement is a pre-promotion site like a branch.

Codex review 2026-10-26: CONFIRMED — deferred address-taking can first initialize a globally registered slot on only one exit path, leaving other exits to read uninitialized storage.
