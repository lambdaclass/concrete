# Bug 056: reassigning a fn-pointer local across control flow emits invalid SSA

**Status:** Open — rejected-valid-program (no wrong-code witness).
**Discovered:** 2026-07-25, while building the R-0002 gate (a case meant to force
a register-dispatched call, rather than the devirtualized one Lower emits for a
statically-known target).
**Provenance:** pre-existing, NOT introduced by R-0002 — the pushed tip
(`e1b3844e`, before the `Callee` change) reproduces it identically, and the
R-0002 diff does not touch `@fnref.` or phi construction.

## Symptom

```con
mod t {
  fn double(x: i64) -> i64 { return x * 2; }
  fn triple(x: i64) -> i64 { return x * 3; }
  pub fn main() -> i64 {
    let mut pick: fn(i64) -> i64 = double;
    if 1 > 0 { pick = triple; }
    return pick(14);
  }
}
```

```
error[ssa-verify]: (E0709) main: block 'merge2': phi uses undefined register %@fnref.triple
error[ssa-verify]: (E0709) main: block 'merge2': phi uses undefined register %@fnref.double
```

The interpreter runs the same program correctly (42), so this is a valid program
the backend refuses, not a miscompile. It fails closed at SSAVerify.

## Root cause

Lower represents a statically-known function reference as an `SVal.reg` whose
register NAME is the sentinel `@fnref.<fnName>` (`Concrete/IR/Lower.lean`, the
call-target resolution reads it back and drops the prefix to emit a direct call —
which is why a straight-line fn-pointer call devirtualizes to `call @double`).

That sentinel is not a real SSA register. It works only as long as the value stays
in Lower's variable map. When two branches bind the same fn-typed variable, the
branch-merge builds a phi over the two variable SVals, and the sentinel names
reach the phi as if they were registers — so SSAVerify correctly reports that the
phi uses undefined registers.

The same shape is why a `while` that reassigns a fn-pointer, or any merge over
two different statically-known targets, cannot compile today.

## Candidate fix

Give a statically-known function reference a real materialized value instead of a
name-encoded sentinel: emit a `ptr`-typed register holding the function's address
(the LLVM form is just the global symbol as a value), and let the call path
consume either that register or a direct symbol. Devirtualization then becomes an
optimization over a real value rather than a decoding of a register name, and a
phi over two fn pointers is an ordinary phi over `ptr`.

Sentinel-in-a-name is the same anti-pattern as bug 050 (identity carried in a
string that a later pass re-interprets); this is its Lower-side instance.

Regression: the program above compiles and returns 42 compiled == interpreted;
a loop that reassigns the pointer each iteration also compiles; the straight-line
case still devirtualizes (no regression in emitted code for the common shape).
