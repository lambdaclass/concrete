# Bug 307 — a call argument &mut x passed a temporary copy of x and wrote it back after the call, a lowering correct only under the checker's unproven exclusivity guarantee

<!-- bug-ledger
summary: Lower lowered a call argument `&mut x` whose local was not yet slot-backed by copying `x` into a temporary `mutref.` slot, passing the temporary and loading it back into `x` after the call; no defined counterexample was found under G19 (OD15, 2026-10-29: a pointer derived from a loan is valid only while the loan lives), but the copy agrees with `x`'s own slot only because the checker refuses an argument that overlaps the `&mut` one (E0293), the exclusivity guarantee OD15(b) forbids a pass to rely on until it is proven; reclassified 2026-10-29 from silent wrong code (the raw-pointer reproducers dereference a pointer after its loan ended)
fixtures: regress_307_call_arg_mut_is_place.con regress_307_call_arg_owner_types.con
class: 8 — a call argument's `&mut x` had its own storage scheme beside `addrOfLocal`'s, kept in agreement with it by the checker's exclusivity guarantee
silent: no
ruling: Permitted by: REF-14@1 — the raw-pointer reproducers dereference a pointer after its loan ended, so no miscompile; the copy-in/copy-out lowering is removed because it relied on the unproven `&mut` exclusivity guarantee (REF-14@1) and is not the place semantics of REF-13@1
-->

**Status:** FIXED (branch `wip/bug-307-od2`); reclassified 2026-10-29 under OD15: not a miscompile of any defined program, a lowering that relied on the unproven `&mut` exclusivity guarantee (OD15(b)), removed under OD2 part 3
**Found:** 2026-10-27, auditing the wildcard cases of `IR/Lower.lean` for the borrow hunt that found
bugs 301 – 306 (the call case's `.borrowMut (.ident ..)` argument path).
**Severity:** no defined counterexample was found (probes and a Codex re-examination). Filed as
silent wrong code on the raw-pointer programs below; G19 makes those programs undefined, so they
are not evidence of a miscompile.

## Original reproducer (undefined under G19)

```
trusted fn leak(p: &mut i64) -> *mut i64 { return p as *mut i64; }
trusted fn wr(p: *mut i64, v: i64) { *p = v; }
fn a() -> i64 {
    let mut x: i64 = 1;
    let q: *mut i64 = leak(&mut x);
    wr(q, 5);
    return x;
}
fn main() with(Console) { println(a()); }
```

On main ea21a2e8 the binary prints `1`, `--interp` `5`; with `x = 7; return rd(q);` after the call,
the binary prints `1` and `--interp` `7` (re-measured on main 0bf8350a: the old fixture's binary
exits 24, `--interp` 42). Each of these programs dereferences `q` after `leak` returned, that is
after the loan `&mut x` made for the call ended (OD2 part 1: the call's use ends when it returns).
Under G19 (OD15, decided 2026-10-29: a pointer or alias derived from a loan is valid only while the
loan lives) they are undefined, and the binary's result is not wrong.

## Re-examination (2026-10-29)

The copy is taken only when `x` is not slot-backed where the call is, which the address-taken walk
guarantees whenever another address of `x` exists in a branch or loop around the call, and lazy
promotion guarantees when one was taken earlier in straight-line code. Probes on main 0bf8350a,
binary against `--interp`, all agree: a scalar, an array and a `Copy` struct passed whole, a field
argument after a whole one, a generic `put<T: Copy>(&mut x, v)`, a call in a `while` condition and in
a `for` step, nested calls `two(inc(&mut x), inc(&mut y))`, a callback `apply(inc, &mut x)`, a
deferred `add(&mut y)`, a deferred read of `x` after `inc(&mut x)`, and a trusted callee that writes
through `p as *mut i64` while the loan lives. Codex (re-examination, 2026-10-29) found no defined
program that tells the copy from `x`'s slot: NOT-A-BUG-UNDER-G19.

It also found what the copy relies on. Lower decides to copy from `x`'s state before it lowers the
call's other arguments; for `f(&mut x, &x)` it would copy `x` for the first argument, then promote
`x` for the second, and pass two different slots, so a callee writing through one and reading
through the other would see the difference. Only the checker's refusal of overlapping arguments
(E0293, REF-11, OD2 part 2) keeps such a call from reaching Lower. The copy is therefore correct
only under the checker's `&mut` exclusivity guarantee, which OD15(b) says no pass may assume or
exploit until it is proven in Lean.

## Cause

`IR/Lower.lean`, `lowerExpr`'s `.call` case: an argument of the shape `.borrowMut (.ident x _)`
with `x` not promoted got a fresh `mutref.` slot holding a copy of `x`, the call received the
slot, and the slot was loaded back into `x` after the call (`mutBorrows`). Every other `&mut x`
goes through `placeAddr` / `addrOfLocal`, which promotes `x` itself.

## Fix

Call arguments are lowered as the expressions they are: `&mut x` promotes `x` (lazily where the
call is, which dominates the call; a branch or loop around the call promotes `x` before it, from
the address-taken walk), and the copy and write-back are gone. Under OD2 part 3 `&mut` is a
reference to the place and copy-in/copy-out is not the language's semantics; a backend may use it
only as a provably equivalent optimization, and this one's equivalence argument needs the
exclusivity guarantee OD15(b) does not let a pass use. The change is not needed for the
interpreter and the binary to agree on any defined program found (they already agreed on every
one probed); it removes a
lowering whose correctness depended on an unproven checker property, and one storage scheme.

`regress_307_call_arg_mut_is_place.con` is a differential fixture of defined programs (a subset of
the probes above: the callback and deferred-call probes are not in it): binary = `--interp` = 42 on this branch and on main. Because no
defined program observes the copy, a third leg checks the lowering itself: the fixture's LLVM IR
has no `mutref.` copy slot (main emits 23 such names for it; this branch none), so the leg fails if
the copy comes back. The original raw-pointer fixture is withdrawn, since a test may not pin the
result of an undefined program.

**Owner types (review, 2026-10-29).** A review asked whether, with the copy gone, a `&mut v`
argument whose owner is a reference, raw pointer or `Heap` reaches the callee as the pointee
instead of `v`'s slot. It does not: the generic `.borrowMut` path borrows `.ident v T` at `&mut T`,
so `borrowsSlot` holds and `addrOfLocal` promotes `v` to its own slot (Codex, 2026-10-29: not a
defect). `regress_307_call_arg_owner_types.con` pins it: a raw-pointer local read and repointed
through `&mut p` (every raw pointer derived from a named borrow-block loan that lives through each
dereference, so the fixture stays defined under G19), a `Heap<B>` local written and read through
`&mut h`, and a `&mut` parameter lent
to calls in sequence; binary = `--interp` = 42 on main and on this branch. A reference-typed local
cannot exist (E0302), and a read through `& &T` or `&mut &mut T` is refused (E0238).

## Structural cause

Class 8: storage decided ad hoc in Lower. A second storage scheme for one spelling of a borrow (a
copy with a write-back, like the borrow block's of bug 305) beside the owner's slot, kept in
agreement with it not by Lower but by a checker guarantee (no overlapping argument) that is not
proven.

## Structural owner

OD2 part 3 (`&mut` is the place) and OD15(b) (no pass relies on `&mut` exclusivity until it is
proven), decided 2026-10-29; the rule IDs follow when the specification records them. Every
`&mut x` is lowered by one path (`placeAddr` / `addrOfLocal`); then R-0501 (S10 Place MIR: every
reference a place).

Codex confirmation 2026-10-27: CONFIRMED — Direct mutable-borrow call arguments to unpromoted locals use unregistered copy-in/copy-out slots, so escaped raw pointers lose the local's storage identity and produce the reported silent wrong code.

Codex re-examination 2026-10-29 (after OD15): NOT-A-BUG-UNDER-G19 for the reported witnesses; no defined counterexample found; the copy's equivalence with `x`'s slot depends on argument exclusivity (`f(&mut x, &x)` would pass two slots), so justifying it invokes the guarantee OD15(b) requires to be proven; recommendation: keep the change removing the copy.
