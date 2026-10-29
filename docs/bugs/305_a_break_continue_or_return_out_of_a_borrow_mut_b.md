# Bug 305 — a break, continue or return out of a borrow mut block lost the writes made through its reference

<!-- bug-ledger
summary: Lower pointed a borrow block's reference at a copy of the owner in a fresh slot and loaded the copy back into the owner only when the block fell through its end; a `break`, `continue` (labelled or not) or `return` leaving a `borrow mut` block early skipped that write-back, so the writes made through the reference were lost on that exit: the loop exit or continue edge carried the owner's old value, and an enclosing deferred call reading the owner read it too (silent wrong code)
fixtures: regress_305_borrow_block_early_exit.con regress_305_borrow_block_owners_and_exits.con
class: 8 — a borrow block's reference was a copy with a write-back on one exit
silent: yes
exit: 0b
ruling: Spec gap: REF — a `&mut` refers to the place, and every exit that does not abort runs one exit sequence (OD2 parts 3 and 5, decided 2026-10-29; written as REF-13@1 and STMT-11@1 on wip/spec-od, cited here once they are on main)
-->

**Status:** FIXED (branch `wip/bug-305-od2`), under the owner's decision OD2 (2026-10-29)
**Found:** 2026-10-26, hunting silent wrong code in Lower's handling of borrows and address-taken
locals after bugs 285 and 287 (hand probes of every `CStmt` constructor; `.borrowIn`).
**Severity:** silent wrong code. The native binary loses writes made through the reference on an
early exit; `--interp` is right.

## Reproducer

```
fn brk(c: i64) -> i64 {
    let mut n: i64 = 0;
    let mut i: i64 = 0;
    while i < 3 {
        borrow mut n as r in R {
            *r = *r + 10;
            if i == c { break; }
        }
        i = i + 1;
    }
    return n * 100 + i;
}
fn main() with(Console) { println(brk(1)); }
```

## Observed

On main ea21a2e8 the binary prints `1001`, `--interp` `2001`. The same with `continue` (a loop of
three iterations adding 10 each, continuing out of the block in the second: binary 20, `--interp`
30), and with a `return` out of the block while an enclosing `defer show(&n)` reads the owner: the
deferred call prints the owner's value before the block (binary 1, `--interp` 7).
`regress_305_borrow_block_early_exit.con` (`break`, `continue`, a labelled `break` out of two
loops, a labelled `continue` of the outer loop from a block in the inner one, a read of the owner
after a conditional `break`, a `return` out of the block under the function's `defer`, a `continue`
out of a block in a `for` body, and a `break` out of a block holding a deferred call that takes its
reference, under the loop body's own deferred call): main's binary exits 196 (main 0bf8350a), `--interp` 42.

## Expected

The reference denotes the owner (MEMORY_SEMANTICS §4, "Create the reference": `ref` has type
`&mut T` for the owner), so a write through it is a write to the owner, whichever way the block is
left; the interpreter, the reference, binds the reference to the owner's place (`Interp.lean`,
`.borrowIn`) and gives `2001`, `30` and `7`. OD2 (2026-10-29) states it: `&mut` is a reference to
the place, and every non-abort exit runs one exit sequence.

## Cause

`IR/Lower.lean`, `lowerStmt`'s `.borrowIn` case: it allocated a fresh `borrow.` slot, stored the
owner's current value in it, bound the reference to that slot, and after lowering the body loaded
the slot back into the owner, only if the block had not terminated. The exits (`.break_`,
`.continue_`, `.return_`) snapshot the variable table and branch, so on an early exit the owner
kept its value from before the block (Codex confirmation, 2026-10-26, traced all three).

## Fix

The block's reference is lowered as `&owner` / `&mut owner` is (`lowerExpr` of the borrow of the
owner): the owner's own storage, promoting it to a slot as any address-take does. The
address-taken walk already names a borrow block's owner (`cstmtAddrTaken`'s `.borrowIn` case), so
a branch or loop around the block promotes the owner before it, and a block in straight-line code
promotes it where the block starts, which dominates the body. There is no copy and no write-back,
on any exit.

The repair is OD2 as the owner decided it on 2026-10-29:

- part 3, `&mut` is a reference to the place: a write through the block's reference is the write
  to the owner, immediately. A copy in and a copy out is not the language's semantics, and this
  one was not even equivalent to it (it had no copy out on three of the four exits);
- part 5, one exit sequence on every non-abort exit (fall-through, `return`, `break`, `continue`):
  the exit value, then the deferred calls, innermost scope first, then the end of the scope's
  loans, then the destruction of the remaining locals. With the reference being the owner's
  place, ending the block's loan writes nothing, so the sequence is the same on every exit and
  an enclosing deferred call sees the writes (`ret_in_block` in the fixture: 7 and 9);
- part 4: a deferred call inside the block that takes the reference pins it until the call runs
  (OWN-12, E0206, already on main), so the reference is not written after such a `defer`.

**Owner types (review of the rebased branch, 2026-10-29).** A review (Kimi) reported that the new
lowering gives a reference-, pointer- or heap-typed owner's pointee instead of the owner's place,
because `addrOfLocal` declines those types. It declines them only for a retagging borrow:
`.borrowIn` borrows `.ident owner innerTy` at `refTy` with `innerTy` the pointee of `refTy`, so
`borrowsSlot innerTy refTy` holds by construction and the owner is promoted to its own slot
(MEMORY_SEMANTICS §5's one-hop rule for `Heap`). The fixture derives every raw pointer from a
named borrow-block loan that lives through each dereference, so it stays defined under G19.
Measured (binary = `--interp` on main and on the
branch): a raw-pointer owner repointed through `borrow mut p` (21), read through `borrow p` (3), a
`Heap<B>` owner written through `borrow mut h` with a `continue` and read through `borrow h`; a
reference-parameter owner `borrow p as a` lowers to `%addr = alloca ptr; store ptr %p, ptr %addr`
and passes `%addr`. The review's reproducer (`let mut p: &i64 = &x;`) is refused (E0302), and any
read through the binder of a reference owner is refused (E0238), also in trusted code. Codex
(2026-10-29): not a defect, for this branch and for bug 307's call arguments.
`regress_305_borrow_block_owners_and_exits.con` pins those owner types and adds early exits from a
block nested in the borrow block (`break`, `return`, and a `return` read by an enclosing deferred
call) and from `match` arms inside it (`break`, `continue`, `return`): main's binary exits 58,
`--interp` 42; the branch 42 on both. An IR leg checks the reference-parameter owner: the register
`take` receives is the slot every later read of `p` loads from (main passed a copy slot nothing
read again).

## Structural cause

Class 8: storage decided ad hoc in Lower. The borrow block had its own storage scheme (a copy with
a write-back at the fall-through end) beside the one every other borrow uses (the owner's slot),
and only the fall-through exit was given the step that kept the two in agreement. Under OD2 part 3
there is one meaning for a `&mut` in every spelling (a place), and part 5 one exit sequence; a
per-construct copy needs a per-exit write-back, which is the step that was missing.

## Structural owner

OD2 parts 3 and 5 (decided 2026-10-29; the rule IDs follow when the specification records them),
implemented by lowering every reference a borrow block makes as `&owner` / `&mut owner` is; then
R-0501 (S10 Place MIR: places and loans explicit, every reference a place).

Codex confirmation 2026-10-26: CONFIRMED — Mutable borrow blocks use a separate copy whose write-back is absent from early-exit paths, losing mutations and exposing stale owner values to outer deferred calls.
