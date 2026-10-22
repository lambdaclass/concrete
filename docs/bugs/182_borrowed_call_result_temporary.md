# Bug 182 — a borrowed call result is accepted: a `Heap` temporary is miscompiled and a linear one leaks

**Status:** OPEN
**Found:** 2026-10-23, by the language-tightening probes (witness programs for
[TIGHTENING_DECISIONS.md](../language/TIGHTENING_DECISIONS.md), the REF-5 / G12 row).
**Severity:** memory-unsafe wrong code in safe code, and a silent leak of a linear value. REF-5@1
([MEMORY_SEMANTICS §15.3](../language/MEMORY_SEMANTICS.md)) refuses a borrow of anything but a
canonical place; gap G12 records that a borrow of a literal, call result or aggregate literal is
accepted. These are the two consequences of that gap for a call result of linear or heap type.

## Reproducer

`tests/regressions/bug182_borrowed_call_result_temporary/repro.con`:

```con
struct Copy B { v: Int }
fn peek(b: &Heap<B>) -> Int { return b.v; }
fn main() with(Alloc) -> Int { return peek(&alloc(B { v: 42 })); }
```

Expected: refused by REF-5 (E0238), or exit 42. Observed on main `185d998c` and `wip/train3`
`949d79e9`: the binary is killed by SIGSEGV (valgrind: invalid read at address `0x2a`, the payload's
value); `--interp` prints 42.

`repro_linear_leak.con` in the same directory: `n(&int_to_string(12345))` with
`fn n(s: &String) -> Int`. Both consumers return 5, and valgrind reports 32 bytes definitely lost:
the borrowed `String` temporary is never destroyed, although the checker requires every linear
value to be consumed and nothing in Concrete is destroyed implicitly.

## Cause

`Lower` passes a borrowed call result as the value it produced: for `&alloc(..)` that is the heap
pointer itself (`call @_C.main.peek(ptr %t4)`, `%t4` the `malloc` result), while `&Heap<B>` is the
address of a slot that holds the heap pointer (the one-hop rule of MEMORY_SEMANTICS §5, bugs 102 and
126), so `peek` loads the payload as a pointer. Check (`checkExpr`, the borrow case) accepts the
operand because it is not a place it refuses (only `&(40 + 2)`, constants and temporary `&mut self`
receivers are refused), and no pass gives the temporary a slot or a destruction point, so a linear
temporary is dropped on the floor. A string literal temporary (`&"text"`, 79 corpus sites) is
harmless only because a literal `String` owns no heap memory (capacity 0).

## Structural cause

Two classes. A documented rule with no enforcing site ([structural class](STRUCTURAL_CLASSES.md)
13): REF-5@1 says a borrow names a canonical place, and the checker refuses only some non-places.
And storage decided ad hoc in `Lower` (class 8): a temporary has no place, so the representation of
a borrow of it depends on the operand's type (a heap pointer, an aggregate's address or a scalar's
spill), the shape of bugs 102 and 126.

## Structural owner

Exit 2i (R-0500) enforcing REF-5 over every non-place operand, or REF-5@2 specifying a temporary as
an anonymous local of the enclosing call that is destroyed after it, as decided in
[TIGHTENING_DECISIONS.md](../language/TIGHTENING_DECISIONS.md) (T-REF5); R-0501 (S10 Place MIR) gives
temporaries places for good. Containment under exit 0b: refuse a borrow of a call result or
aggregate literal whose type is not Copy (it is the only form that leaks or is miscompiled).

## Containment while open

`scripts/tests/check_example_outputs.sh` (leg "open-bug reproducers", mode `signals`) requires
`repro.con`'s binary to be killed by a signal; when it exits 42 or is refused, that leg fails until
this document, the corpus entry and the leg are updated together. The leak form is not pinned (an
exit status cannot show it); its fixture lands with the rule.
