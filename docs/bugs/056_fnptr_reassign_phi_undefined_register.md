# Bug 056: reassigning a fn-pointer local across control flow emits invalid SSA

**Status:** Fixed (2026-07-28, R-0436).
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

## Fix as shipped

Two representations were carrying identity inside strings, and each cost one
defect. The reported symptom was only the first.

**`SVal.reg "@fnref.<name>"`.** A statically-known function was spelled as a
register whose NAME encoded the function. It is not a register, so a merge of
two fn-typed bindings produced a phi over a name no block defines, and SSAVerify
refused it (E0709) — while the interpreter, which never saw the encoding, ran
the program correctly. Three passes carried a `startsWith "@fnref."` exemption
to avoid mistaking it for a register. It is now `SVal.fnRef name ty`, a value.
`svalRegs` answers `[]` for it by its ordinary catch-all, so all three
exemptions were deleted rather than added to.

**`SInst.call`'s `fn : String`.** A bare name meant a direct call, a
`%`-prefixed name meant an indirect call through that register. Fixing the first
defect exposed the second immediately: `replaceRegInInst` could rewrite that
string only into another REGISTER, so folding a fn-pointer phi down to a known
function left the dead `%if.phi.N` in the call. The dangling reference reached
`llvm-as`, not our own verifier — because SSAVerify's `instUses` could not see a
call target hidden in a string at all, so an indirect call through an undefined
register passed verification. It is now `SCallee.direct name` /
`SCallee.indirect target`, and there is deliberately **no** `Coe String SCallee`:
an implicit coercion would let the bare-string form compile again silently,
which is how it survived this long.

Devirtualization is now a decision about a value's constructor. Measured
consequence: `.indirect (.fnRef f)` and `.direct f` emit byte-identical LLVM,
because both reach `svalToOperand`. Lower's `.direct` conversion still matters
for passes that key on a direct callee (`checkCallArity` validates only those),
but EmitSSA is what decides the emitted operand.

## Regression

`scripts/tests/check_fnptr_values.sh` (17 checks, CI + the hook's `fnptr` area
for `Concrete/IR/*` and `Concrete/Backend/*`):

- rebinding a fn pointer across `if`/`else`, across a constant-folded `if`, and
  in a loop body — each compiled AND agreeing with the interpreter, since the
  bug's signature was a program the interpreter ran and the compiler refused;
- a phi mixing a register loaded from a struct field with a known global;
- devirtualization preserved: the straight-line case still emits `call @a`, a
  phi folded to one target still becomes a direct call, and no reference to the
  eliminated phi survives — a fix that made everything indirect would pass a
  correctness gate while deoptimizing every call through a fn-typed local;
- the verifier still refuses an undefined phi operand AND an undefined indirect
  call target, while accepting well-formed ones, so removing the `@fnref.`
  exemptions did not blind it;
- structurally, neither string convention exists anywhere in the compiler.

Mutations #28-#30 are each killed by that gate: the callee escaping
substitution, the callee invisible to use-checking, and a fn reference emitted
as a register instead of a global. Two earlier candidates were discarded for not
proving anything — one was killed only by Lean's unused-binder linter, and one
(removing Lower's devirtualization) left the emitted IR byte-identical.
