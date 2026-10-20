# Bug 107 — a reference to a shadowed binding reads the shadowing binding's storage

**Status:** OPEN
**Found:** 2026-10-20, Codex review of batch 4 (`land4`): making the checker's borrow source
identity-precise accepted a program that exposed it; the plain shape below needs no checker change.
**Severity:** critical. Silent wrong code in safe code on main: the binary and the interpreter
disagree, and no gate compares them on this shape.

## Reproducer

```con
fn main() -> Int {
    let x: Int = 1;
    let r: &Int = &x;
    let x: Int = 2;
    return *r + x * 0;
}
```

`--interp` returns 1; the compiled binary exits 2. The same holds when the shadowing `let` is in a
nested block:

```con
fn get(r: &Int) -> Int { return *r; }
fn main() -> Int {
    let x: Int = 1;
    let r: &Int = &x;
    if true {
        let x: Int = 5;
        return get(r) * 10 + x;
    }
    return 0;
}
```

`--interp` returns 15; the binary exits 55. Shadowing a Copy binding while a reference to it is live
is accepted by the checker (H16 only rejects shadowing a live non-Copy binding).

## Cause

Elab keeps the surface name for a `let` binder (only match-arm binders are alpha-renamed, bug 045),
so both bindings are `x` in Core (`--emit-core` shows `let x: Int = 1; let r: &Int = &x; let x: Int
= 2;`). `Lower`'s slot table is function-flat and keyed by Core name, so the second `let x` stores
into the first `x`'s alloca (`--emit-ssa`: one `%addr.0`, `store 1`, `store 2`, and the load
through `r` folds to 2). The interpreter keeps a separate value per binding, so it reads 1.

## Structural cause

Binder identity by name, not a `LocalId` ([structural classes](STRUCTURAL_CLASSES.md) 2): Core
binders are surface names, Elab alpha-renames only some binders, and every stage below keys storage
on the name. It is also storage decided ad hoc in `Lower` (class 8): a slot is reused because the
name matches, with no notion of which binding a reference was taken from.

## Structural owner

R-0492 (S5): a `LocalId` for every binder from Resolve on, so the two `x` bindings are different
locals with different storage; R-0501 (S10 Place MIR) gives each local its own place. Until then the
repair belongs to queue exit 0b (wrong code in safe code): give a shadowing `let` a fresh Core name
in Elab (as bug 045 did for match binders), with both programs above as permanent gate legs
comparing the binary with `--interp`.

**Repair owner:** queue exit 0b.
