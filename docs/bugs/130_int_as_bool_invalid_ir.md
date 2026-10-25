# Bug 130 — `x as bool` with an integer `x` passes the checker and fails in `llvm-as`

<!-- bug-ledger
summary: `x as bool` with `x: Int` passed the checker and failed in `llvm-as` (`bitcast i64 to i1`); CoreCheck no longer admits `<int> as bool` (E0553, the bug 108 fix, ARITHMETIC_POLICY §8.1); pinned by `error_130_int_as_bool_int.con`
fixtures: error_130_int_as_bool_int.con
class: 7
class: 8
class: 13
silent: no
-->

**Status:** FIXED (branch `backend-108-116`, commit 70025ca9, the bug 108 fix)
**Found:** 2026-10-21, by the semantics probes (finding N4, `cast_bool.con`).
**Severity:** loud failure on an accepted program: the checker accepts the cast, LLVM validation
rejects the emitted IR, and the interpreter refuses the program at run time. The reverse direction of
bug 108 (`bool as <int>`).

## Reproducer

`tests/regressions/bug130_int_as_bool_invalid_ir/repro.con`:

```con
fn t(x: Int) -> bool {
    return x as bool;
}

fn main() -> Int {
    if t(2) {
        return 1;
    }
    return 0;
}
```

On main (`5b63cd87`):

```
LLVM IR validation failed for .../repro.con.ll:
llvm-as: .../repro.con.ll:838:17: error: invalid cast opcode for cast from 'i64' to 'i1'
  %t0 = bitcast i64 %x to i1
                ^
```

`--interp` fails `interp: if condition is not a boolean` (its cast produces an integer tagged
`bool`). Expected: a checker diagnostic, since no implementation gives `<int> as bool` a meaning.

## Cause

CoreCheck's cast-validity table in `Concrete/Check/CoreCheck.lean` (`ccCheckExpr`, `.cast`) admits
`isInteger innerTy && targetTy == .bool`. The backend's integer-cast case in
`Concrete/Backend/EmitSSA.lean` picks the opcode from a local width table that has no entry for
`bool`, so `bool` falls to the 64-bit default: an `Int` source is "equal width" and gets `bitcast`, a
narrower source gets `sext`/`zext`, and neither opcode is valid to `i1` (the `trunc` that would be
is never chosen). The interpreter's `evalCast` wraps the integer and tags it `bool`.

## Fix

The bug 108 fix (`70025ca9`) removed the `isInteger innerTy && targetTy == .bool` row from
CoreCheck's cast-validity table, so `<int> as bool` is refused with
`error[core-check]: (E0553) [main] cannot cast Concrete.Ty.int to Concrete.Ty.bool`, as
ARITHMETIC_POLICY §8.1 decides ("integer → `bool`: no, never total"). `isInteger` includes `Int`, so
this exact shape is covered. Permanent leg: `tests/programs/error_130_int_as_bool_int.con` (this
reproducer) in `run_tests.sh` (run_err E0553), beside bug 108's `i32` fixture
`error_108_int_as_bool.con`.

## Structural cause

Semantics implemented several times ([structural class](STRUCTURAL_CLASSES.md) 7): the checker, the
interpreter and the backend each decide what a cast means, and here the checker admits a cast that
neither consumer defines. It is a documented-rule gap as well (class 13): no rule gives
`<int> as bool` a meaning, yet the checker's table had a row for it. The emitted IR is first checked
by `llvm-as` rather than by a validator of Concrete's own (class 8).

## Structural owner

R-0493 (S6 `IntArith`: one definition of every cast, `bool` included) with R-0504 (S12 validators);
the per-width-pair cast rule is decided under exit 2k (R-0500). The repair belonged to exit 2c
(loud failures that keep valid programs out of the interpreter/native comparison) and landed with
the bug 108 fix; no safety or authority defect was demonstrated.
