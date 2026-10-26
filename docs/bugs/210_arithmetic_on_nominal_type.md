# Bug 210 — arithmetic on a struct, enum or newtype operand was accepted

<!-- bug-ledger
summary: CoreCheck's arithmetic, wrapping and bitwise agreement rules read every named type as a type parameter and skipped TYPE-6, so `p + p` on a `Copy` struct and `e * e` on an enum compiled and ran natively (exit 0) while `--interp` stopped, a struct minus a literal failed in the SSA verifier and a newtype sum ran under `--interp` and failed in `llvm-as`. One predicate (`isOpaqueOperandTy`) now serves every operator rule and the cast rule; refused with E0501. Pinned by six `error_210_*.con` fixtures (two declare the type in a child module). Found by the fuzz farm's mutation generator
fixtures: error_210_struct_arith.con error_210_enum_arith.con error_210_newtype_arith.con error_210_struct_minus_literal.con error_210_child_module_arith.con error_210_child_module_newtype_arith.con
class: 12 — which types an operator rule defers to the instantiation: a local copy per rule read every named type as a parameter
silent: yes
exit: 0b
-->

**Status:** FIXED (branch `wip/diffhunt-1023`)
**Found:** 2026-10-23, by the fuzz farm's mutation generator on `concrete-ci-2` (findings
`crash-d8cc80fc873e`, `crash-4d7ad6b76351`, `crash-86e079505a3e`: a mutated literal turned
`P { .. }` into `P { .. } - 1`), reproducing at main `eb122a01`; reduced by hand in the
post-R-0492 differential hunt.
**Severity:** silent wrong code. `p + p` on a `Copy` struct and `e * e` on an enum compiled, and the
binary ran (exit 0), while `--interp` stopped with "unsupported binop on given value types". A
struct minus a literal failed in the SSA verifier (E0715); a newtype sum ran under `--interp`
(returned 7) and failed in `llvm-as` natively.

Confirmed real by Codex review, 2026-10-25: docs/spec/types.md TYPE-6 restricts arithmetic to numeric operands; declared nominal types incorrectly received the type-parameter exemption.

## Reproducer

`tests/programs/error_210_struct_arith.con`:

```con
struct Copy P { x: i32 }

fn main() -> Int {
    let p: P = P { x: 1 };
    let q: P = p + p;
    return q.x as Int;
}
```

Observed on main `eb122a01`: the binary built and exited 0; `--interp` failed at run time.
Expected: refused by TYPE-6@1 ("arithmetic operators take two operands of one numeric type"), E0501.
The same holds for an enum (`error_210_enum_arith.con`), a newtype (`error_210_newtype_arith.con`,
a newtype is not a numeric type) and the farm's shape (`error_210_struct_minus_literal.con`).

## Cause

The source checker leaves operator typing to CoreCheck (`Check.lean`, `.binOp`: "CoreCheck
validates operator type constraints"). CoreCheck's arithmetic, wrapping/saturating and bitwise
agreement rules (`ccCheckExpr`, `.binOp`) each carried a local `hasTypeVar` that read every
`Ty.named` as a type parameter and skipped the rule, so a declared struct, enum or newtype was never
held to TYPE-6. The cast rule in the same function had been corrected for exactly this loophole
(a `named` type is opaque only when it is not a declared struct, enum or newtype); the operator
rules kept the old copy.

## Fix

One predicate, `isOpaqueOperandTy` in `CoreCheck.lean`: a `typeVar`, or a `named` type that no
module of the program declares as a struct, enum or newtype. The arithmetic, wrapping/saturating and
bitwise agreement rules and the cast rule all read it, so a declared nominal type is concrete in
every operator rule. The declared names are the whole program's (`programTypeNames`, collected over
every module and submodule): the Codex review found that a value of a type declared in another
module (`child::make() + child::make()`) still read as a parameter when only the current module's
tables were consulted. The cast rule's newtype rebrand lookup reads the program's newtypes too
(`findNewtype`), so a foreign newtype's `.0` unwrap stays admitted (TYPE-11, pending R-0500).
Regression legs in `run_tests.sh`, each `run_err ... (E0501)`: `error_210_struct_arith.con`,
`error_210_enum_arith.con`, `error_210_newtype_arith.con`, `error_210_struct_minus_literal.con`,
`error_210_child_module_arith.con` and `error_210_child_module_newtype_arith.con`.

A generic `fn add<T: Copy>(a: T, b: T) -> T { a + b }` instantiated at a struct still reached
`llvm-as`: the opaque parameter is accepted before Mono and nothing re-checked the instance. That is
bug 211 (`211_generic_arithmetic_post_mono.md`).

**Residual (disclosed).** Core spells a type parameter and a declared nominal type the same way
(`Ty.named "T"`), so the predicate decides by name. A `named` spelling of the current function's
type parameter is opaque, which keeps a parameter spelled like some module's type admitted; the
cost is the converse: inside `fn f<T: Copy>`, arithmetic on another module's struct that is also
called `T` is not refused here (the Codex review's witness). Bug 211's post-Mono check sees the
instance, and the name collision itself belongs to class 1 (R-0496: Core types are resolved, a
parameter is a `typeVar`).

## Structural cause

Class 12 ("one fact kept in two places"): which types an operator or cast rule may defer to the
instantiation was decided by a local copy of the predicate at each rule, and correcting one copy
(the cast's) left the others open. Secondary: class 13, TYPE-6@1 had no enforcing site for nominal
operands.

## Structural owner

R-0499 (S8: every rule has one enforcing site over Core types), with R-0496 (S7: Core types are
resolved, so a type parameter is a `typeVar` and never a `named` spelling, which removes the
predicate's second case).
