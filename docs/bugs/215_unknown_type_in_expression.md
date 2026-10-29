# Bug 215 — an unknown type written in a cast or a type-argument list was not refused

<!-- bug-ledger
summary: Resolve did not check the types written inside expressions (cast targets, type-argument lists of calls, method calls, static method calls, struct and enum literals) or a constant's initializer, so `x as Nope` made the backend panic while `--interp` ran it, and an unused unknown type argument compiled on both backends. Every such type now goes through `checkTyDeep` (E0108). Pinned by four `error_215_*.con` fixtures. Found by the fuzz farm's mutation generator
fixtures: error_215_cast_unknown_type.con error_215_turbofish_unknown_type.con error_215_cast_field_spelling.con error_215_const_cast_unknown_type.con
class: 12 — a type written inside an expression was not checked to name a declared type
silent: yes
exit: 0b
ruling: Violates: unrecorded
-->

**Status:** FIXED (branch `wip/diffhunt-1023`)
**Found:** 2026-10-23, by the fuzz farm's mutation generator on `concrete-ci-2` (findings
`crash-7b8a6f578aa1`, `crash-7783960bed65`, `crash-eca71defcd8d`, `crash-5d3cd121011d`,
`crash-452448d15b16`: a mutated `s0.y` became `s0 as y`, `(v1) as _`, `(v2) as v2`), reproducing at
main `eb122a01`; reduced and widened to type-argument lists in the post-R-0492 differential hunt.
**Severity:** a crash, and a silent acceptance. `(a as Nope) as Int` made the backend panic
(`EmitSSA.tyToLLVMTy: unknown named type 'Nope'`) while `--interp` printed 7; a cast whose result
met another operand failed in the SSA verifier (E0715). `sz::<[Nope; 3]>()` with a body that never
uses its parameter compiled and ran on both backends.

Confirmed real by Codex review, 2026-10-25: docs/spec/modules.md MOD-1 requires every used name to resolve; expression type positions bypassed resolution, admitting nonexistent types.

## Reproducer

`tests/programs/error_215_cast_unknown_type.con`:

```con
fn main() -> Int {
    let a: i32 = 7;
    let b: Int = (a as Nope) as Int;
    return b;
}
```

Observed on main `dcc48dc1`: a backend panic natively, 7 under `--interp`. Expected: E0108
(`unknown type 'Nope'`), as for the same name in a `let` annotation.
`error_215_turbofish_unknown_type.con` is the type-argument case, and
`error_215_cast_field_spelling.con` the farm's shape.

## Cause

`Resolve.resolveExpr` walked only the expression parts of `.cast`, `.call`, `.methodCall`,
`.staticMethodCall`, `.structLit` and `.enumLit`: the cast target and the type-argument lists were
never passed to `checkTyDeep`, which `let` annotations and function signatures go through. A
constant's initializer was not walked at all. Check then resolved the cast target with `resolvedOr`, which keeps an unresolvable
type as written, so `Ty.named "Nope"` reached Core, and CoreCheck's cast rule reads an undeclared
`named` type as a type parameter.

## Fix

Resolve checks every type written inside an expression (`checkWrittenTys`, built on
`checkTyDeep`): cast targets and the type-argument lists of calls, method calls, static method
calls, struct literals and enum literals; and a constant's type and initializer are walked like a
function body (the Codex review's witness: `const X: Int = (7 as Nope) as Int`). An unknown name is
E0108 at `resolve`. Regression legs in `run_tests.sh`: the four `error_215_*.con` fixtures,
`run_err ... (E0108)`.

Function bodies and constants in a nested submodule, which Resolve did not walk at all, are
covered since bug 217's fix. Not covered: contracts (`#[requires]`, `#[ensures]`, loop invariants) and spec functions, which are
erased metadata Resolve does not walk; an unknown type there cannot reach the backend, and their
checking belongs to the contract passes.

## Structural cause

Class 12 ("one fact kept in two places"): the rule "a written type names a declared type" was
applied where Resolve walked declarations and statements and missed the type positions inside
expressions. Bug 199 was the same omission for alias resolution at the cast (fixed in Elab only).

## Structural owner

R-0492/R-0514 (one resolver: every written type, wherever it appears, is resolved once to a
declaration identity), with R-0496 (S7: Core types are resolved).
