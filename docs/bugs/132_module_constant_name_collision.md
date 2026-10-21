# Bug 132 — two modules' constants of one name are one constant to Lower

**Status:** FIXED 2026-10-21 (contained, batch 2, `land2-fix`): refused at the use (E0602) until
constants carry their module.
**Found:** 2026-10-21, by batch 2's Codex review of the bug 123 fix (see
[bug 123](123_const_borrow_uninitialized_slot.md)).
**Severity:** silent wrong code in safe code: a submodule's read of its own constant returns another
module's constant of the same name.

## Reproducer

`tests/regressions/bug132_module_constant_name_collision/repro.con`:

```con
const A: Int = 7;

mod m {
    const A: Int = 42;

    pub fn get() -> Int {
        return A;
    }
}

import m.{get};

fn main() -> Int {
    return get();
}
```

On main (`34060604`) the binary exits **7**: `m.get` reads the top-level `A`, not its own (42).
Borrowing one (`*&A[0]` in an initializer) read the other module's value too. Expected: 42, or a
refusal.

## Cause

Elab keeps a constant reference as the bare name, and `Concrete/IR/Lower.lean` flattens every
module's constants into one list by name (`collectAllConstants`); `lowerExpr`'s `.ident` case inlines
the FIRST constant with that name. Core carries no module path for a constant, so two modules'
`A` are indistinguishable once lowered.

## Fix (containment)

`lowerExpr` refuses a constant name defined by more than one module at its use (E0602, "module
constant 'A' is defined by more than one module, and Core cannot tell which one this use names;
rename one of them"), the fail-closed answer, as Mono refuses an ambiguous type name (bug 052
round 3). std defines no module constants, so no std name can collide with a user's. Fixture
`error_132_const_name_collision.con` (E0602); the reproducer above is refused the same way.

## Structural cause

Declaration identity by spelled name, not a resolved `DefId` ([structural class](STRUCTURAL_CLASSES.md)
1, with 052's type names and 082's import spellings): a constant's identity in Core is its bare
spelling.

## Structural owner

R-0492 (S5 `DefId`): a constant reference resolves to its definition's identity, after which the
refusal can be lifted. Under exit 0b the containment is batch 2's E0602 refusal.
