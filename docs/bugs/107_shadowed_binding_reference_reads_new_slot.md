# Bug 107 — a reference to a shadowed binding reads the shadowing binding's storage

**Status:** FIXED 2026-10-20 (batch 2, bug 091 normalized place rule)
**Found:** 2026-10-20, Codex review of batch 4 (`land4`): making the checker's borrow source
identity-precise accepted a program that exposed it; the plain shape below needs no checker change.
**Severity:** critical. Silent wrong code in safe code on main: the binary and the interpreter
disagree, and no gate compares them on this shape.
**Relation to 087:** the same Elab/Lower defect as bug 087 (a shadowing `let` keeps its Core name and
shares the shadowed binding's slot), seen through a reference while both bindings are live instead
of through a read after the inner block closes. Batch 2 (`land2-fix`) repairs 087 with `bindLocal`,
which alpha-renames every shadowing `let` (same block or outer), and its checker additionally
refuses shadowing a binding that live references borrow: the `land2-fix` build at `91003b79` rejects
both reproducers below with E0238. This record stays OPEN on main until that lands, and adds the two
reference reproducers as regression legs the repair must carry.

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

## Fix

Bug 091's round-3 normalized place rule records reference provenance by binding name, so shadowing
a name that a live reference records as its source or parent is refused (`guardShadowedSource`,
E0238): both reproducers above are rejected at the shadowing `let`. Batch 2's bug 087 repair
(`bindLocal`) additionally gives every shadowing `let` a fresh Core name, so a shadowing binding no
longer shares the shadowed binding's slot when no reference is involved. Permanent rejection
fixtures: `tests/programs/error_107_shadow_under_live_ref.con`,
`tests/programs/error_107_shadow_in_block_under_live_ref.con`. Accepting these programs (Rust does)
is R-0499's: provenance by `LocalId` rather than by name.

## Structural cause

Binder identity by name, not a `LocalId` ([structural classes](STRUCTURAL_CLASSES.md) 2): Core
binders are surface names, Elab alpha-renames only some binders, and every stage below keys storage
on the name. It is also storage decided ad hoc in `Lower` (class 8): a slot is reused because the
name matches, with no notion of which binding a reference was taken from.

## Structural owner

R-0492 (S5): a `LocalId` for every binder from Resolve on, so the two `x` bindings are different
locals with different storage; R-0501 (S10 Place MIR) gives each local its own place. Until then the
repair belongs to queue exit 0b (wrong code in safe code): batch 2's 087 repair (`bindLocal` on
`land2-fix` gives a shadowing `let` a fresh Core name, as bug 045 did for match binders), extended
with both programs above as permanent gate legs comparing the binary with `--interp`.

**Repair owner:** queue exit 0b.
