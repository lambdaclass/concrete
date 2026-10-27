# Bug 302 — a loop moved an address-taken scalar back into a register at its exit, splitting it from a raw pointer to its slot

<!-- bug-ledger
summary: a loop promotes to a slot every scalar local its body or condition takes the address of, and at its exit Lower loaded the slot into a register and dropped the promotion, so later reads and writes of the local used the register while a raw pointer taken in the loop (`&mut x as *mut i64`) still pointed at the slot: a write on either side was not seen on the other (silent wrong code)
fixtures: regress_302_loop_keeps_addr_taken_slot.con
class: 8 — a local's storage changed back at a loop exit while an alias to the slot lived on
silent: yes
exit: 0b
-->

**Status:** FIXED (branch `wip/bug-302`)
**Found:** 2026-10-26, hunting silent wrong code in Lower's handling of borrows and address-taken
locals after bugs 285 and 287 (hand probes of the loop's slot-or-phi choice).
**Severity:** silent wrong code. After the loop the native binary reads a stale value through the
pointer or in the local; `--interp` is right.

## Reproducer

```
trusted fn rd(p: *mut i64) -> i64 { return *p; }
fn f() -> i64 {
    let mut x: i64 = 1;
    let mut y: i64 = 0;
    let mut i: i64 = 0;
    let mut p: *mut i64 = &mut y as *mut i64;
    while i < 2 { p = &mut x as *mut i64; i = i + 1; }
    x = 5;
    return rd(p);
}
fn main() with(Console) { println(f()); }
```

## Observed

On main ea21a2e8 the binary prints `1`, `--interp` prints `5`. If `x` is first borrowed before the
loop (`let mut p: *mut i64 = &mut x as *mut i64;`) the binary prints `5`: the result depended on
where the first borrow was. `regress_302_loop_keeps_addr_taken_slot.con` (a write to the local
after a `while`, a write through the pointer after a `for`, and a first borrow in the loop
condition, whose pointer outlives the loop): main's binary exits 20, `--interp` 42.

## Expected

`x` is alive after the loop and the raw pointer points at it: a raw pointer is the sanctioned
escape from second-class references (MEMORY_SEMANTICS §15.1), and the interpreter, the reference,
reads `5`.

## Cause

`IR/Lower.lean`, `lowerStmt`'s `.while_` case (a `for` loop elaborates to it): before the loop it
promotes every aggregate and every scalar whose address the condition or body takes
(`addrTaken`, the C9 rule), and records them in `newlyPromotedRev`. At both exits (the stepped
form's and the plain one's) every name in that list was "un-promoted": the slot loaded into a
register, `removePromotedAllocas`, and the name rebound to the loaded value. From then on
`lookupVar` / `setVar` used the register. The pointer `&mut x` produced in the loop is the slot's
address (`addrOfLocal` returns the existing slot), so after the exit the two no longer alias
(Codex confirmation, 2026-10-26, traced both directions).

## Fix

Only aggregates are un-promoted at the exit (they are promoted for being aggregates, whatever the
body borrows; arrays keep their slot as before). A scalar the loop promoted because its address is
taken stays slot-backed for the rest of the function, as a scalar promoted by a borrow outside a
loop always was. A loop inside a branch region cannot leave a slot initialized on one path only:
the enclosing site's walk includes the loop's borrows and promotes the local before it branches
(bug 031), so such a local is already promoted when the loop is lowered.

## Structural cause

Class 8: storage decided ad hoc in Lower. A local's storage was decided per site and could change
back at a site's end, while an alias of the earlier storage (a raw pointer) outlived the site.

## Structural owner

R-0501 (S10 Place MIR): the storage of every local decided once per function from the places it
uses. Until then a local, once slot-backed because its address is taken, stays slot-backed.

Codex confirmation 2026-10-26: CONFIRMED — Loop-exit un-promotion disconnects newly address-taken scalar locals from their escaped raw-pointer aliases, causing silent wrong code after the loop.
