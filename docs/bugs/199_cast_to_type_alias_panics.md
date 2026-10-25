# Bug 199: a cast to a type alias crashed the compiler

<!-- bug-ledger
summary: a cast to a type alias (`x as IP`) reached the backend unexpanded and the compiler panicked; Elab resolves the cast target through aliases. Pinned by `regress_199_cast_to_alias.con` (`--interp == binary`, 42)
fixtures: regress_199_cast_to_alias.con
class: 12 — a cast target was not resolved through type aliases
silent: no
-->

**Status:** FIXED (branch `soundness-holes`)
**Found:** 2026-10-23, by the Codex review of the bug 198 fix (a witness that spelled a raw-pointer
type through an alias), then reproduced on main.
**Severity:** loud: the compiler panicked (`EmitSSA.tyToLLVMTy: unknown named type 'IP'`) on a valid
program; no binary, no wrong code.

## Reproducer

`tests/programs/regress_199_cast_to_alias.con`:

```con
type IP = *mut Int;
trusted fn main() -> Int {
    let mut n: Int = 42;
    let p: *mut Int = &mut n as IP;
    return *p;
}
```

On main the compiler panics in the backend; `--interp` failed with "unsupported cast".

## Cause

Elab's `.cast` case put the written target type into Core unexpanded, so the alias name reached
Lower and EmitSSA, which know only resolved types. Every other written type (annotations,
signatures, fields) is resolved through aliases in Elab.

## Structural cause

Class 12 in [STRUCTURAL_CLASSES.md](STRUCTURAL_CLASSES.md): one fact (a type's resolution through
aliases) applied at most type positions and missed at one.

## Structural owner

R-0496 (S7 Core semantics: Core types are resolved). Repair: Elab resolves the cast target with the
same `resolveTypeE` every other written type goes through.

## Fix and regression

Elab resolves the cast target through aliases (`resolveTypeE`), for the Core expression and its
evidence. `regress_199_cast_to_alias.con` exits 42 natively and under `--interp` (`run_ok` and the
`--interp == binary` loop); `regress_198_read_owned.con` casts to an alias of a raw pointer too.
