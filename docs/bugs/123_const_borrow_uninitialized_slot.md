# Bug 123 — a borrow of a module constant reads a stack slot that is never initialized

**Status:** FIXED 2026-10-21 (batch 2, `land2-fix`)
**Found:** 2026-10-21, by the exit 2i specification (MEMORY_SEMANTICS §15.3 REF-3, gap G1).
**Severity:** silent wrong code in safe code. A constant of bare reference type is accepted, and
the binary reads an uninitialized stack slot through it.

## Reproducer

`tests/regressions/bug123_const_borrow_uninitialized_slot/repro.con`:

```con
const X: Int = 42;
const R: &Int = &X;

fn get(r: &Int) -> Int {
    return *r;
}

fn main() -> Int {
    return get(R);
}
```

On main (`61ca25d1`) the program compiles and the binary exits **0** (arm64 macOS); the program
reads 42. `--interp` fails `interp: undefined variable 'R'` (the interpreter half is bug 117: the
interpreter does not bind module constants). The emitted `user_main` is:

```llvm
  %addr.0 = alloca i64
  %t1 = call i64 @get(ptr %addr.0)
```

`%addr.0` is never stored to, so the exit value is whatever the slot held (an uninitialized read,
not a stable value: the pin requires only that it is not 42).

Expected: the declaration `const R: &Int = &X;` is rejected with E0239 (REF-3: no reference is
stored, a module constant included).

## Cause

Two defects meet.

1. **Check.** REF-3 forbids a reference in a module constant, but the predicate that checks
   constants, `tyHoldsRefInAggregateV` in `Concrete/Check/Verify.lean` (used by
   `verifyNoStoredRefs` for constant types and initializers), looks through an outer reference
   (`.ref inner => tyHoldsRefInAggregateV inner`), as it must for a bare `&Int` parameter or
   binding. A constant is storage, not a binding, so the bare `&Int` constant type falls through.
   `checkModule` checks only struct, enum and newtype fields, not constants. `const A: [&Int; 1] =
   [&X]` is rejected (E0239, `error_092_const_ref_array.con`) because there the reference is inside
   an aggregate.
2. **Lower.** Elab keeps `R` as an identifier; `lowerExpr` in `Concrete/IR/Lower.lean` resolves an
   identifier that is not a local through its constants table and lowers the initializer `&X` in
   its place. A read of a constant takes that path, but a borrow does not: `addrOfLocal` lowers
   `&X` as the address of a local, `lookupVar "X"` finds nothing (a module constant is not a
   local), and the `| none => pure ()` branch allocates the slot and stores nothing. An unknown name is treated as a local with no value instead of being refused.

## On `land2-fix` (`74f7284a`)

Not closed. Built at that revision (the binary postdates its last compiler-source change,
`42ab7535`): the declaration is still accepted (`const R: &Int = &X; fn main() -> Int { return 0; }`
compiles and exits 0), and every use of `R` as a reference argument is refused with E0238 ("cannot
trace a reference argument: 'R' is not a local binding"), so the reproducer above is rejected
there. But `land2-fix` opens a second spelling of the Lower defect: a borrow of a constant inside a
function body, `return get(&X);` or `let r: &Int = &X; return get(r);`, compiles and exits 0 on
`land2-fix` (the interpreter fails `ref path base 'X' not in env`), while main rejects both with
E0201 ("use of undeclared variable 'X'"). That shape needs a rejection or a correct lowering before
batch 2 lands.

## Fix

Batch 2 (`land2-fix`, 2026-10-21), fail closed at both halves:

1. **The declaration.** `checkModule`'s stored-reference declaration check (E0239) covers module
   constants, with `tyStoresRef` through `expandAliasDeep`, so a bare `&Int` constant counts: a
   constant is storage, not a binding. After monomorphization, `verifyNoStoredRefs` checks a
   constant's type with `tyStoresRefV` (bare references included) instead of
   `tyHoldsRefInAggregateV`. The reproducer is now refused at `const R: &Int = &X;` (E0239,
   `error_123_const_ref_type.con`).
2. **Borrowing a constant.** A module constant has no storage, so a borrow rooted at one is
   refused (E0238, `refuseConstBorrow` in `Concrete/Check/Check.lean`) in each spelling: `&X` in a
   function body (`error_123_const_borrow_in_body.con`), a field `&K.v`
   (`error_123_const_field_borrow.con`), an element `&A[1]` (`error_123_const_index_borrow.con`),
   and the implicit `&self` of `K.get()` (`error_123_const_receiver_borrow.con`). The field and
   receiver spellings miscompiled on main as well (exit 255 for 0). Copying the constant into a
   local first is the spelling that works (`const_copy_then_borrow.con`, native: the interpreter
   half is bug 117). Lower's `addrOfLocal` refuses the address of a module constant too (E0602),
   defense in depth behind the check.

The review of this fix found that Elab resolved a module constant before a local of the same
name and emitted the bare name; with bug 087's alpha-renaming a second shadowing `let X` is
`X.b0` in Core, so `X` and `&X` read the first local (`let X = 1; let X = 2; return X;` gave 1 on
`land2-fix`, 2 on main). Elab now resolves locals before constants, as Check does
(`regress_087_local_shadows_const.con`).

A second review pass found that Lower inlined a constant's initializer in the CALLER's
environment, so a local shadowing a constant the initializer names captured it:
`const Y: Int = X + 1;` read under `let X: Int = 7;` gave 8 for 43, on main as well (silent wrong
code), and `const Y: Int = *&X;` borrowed the local (7 for 42). The initializer is now lowered with
the caller's locals and promoted slots hidden (`lowerExpr`, `.ident` constant case), so a name in it
is the constant and `&X` there reaches `addrOfLocal`'s refusal (E0602,
`error_123_const_initializer_borrow.con`; `regress_123_const_initializer_hygiene.con` prints 43).

A third pass found two more: Check (unlike Elab, now) still resolved a constant before a local
of the same name, so a differently typed local `let X: bool` was typed as the constant's `Int`
(`regress_123_local_shadows_const_other_type.con`); and constants reach Lower flattened by bare
name (Core carries no module path), so a submodule reading its own `const A` read the top-level
`A` (7 for 42, on main too: silent wrong code, with or without a borrow). Check now resolves
locals first; Lower refuses a constant name defined by more than one module at its use (E0602,
`error_123_const_name_collision.con`), the fail-closed answer until constants carry their module
(the same shape as bug 052's type names), and `addrOfLocal` refuses an array constant too
(`error_123_const_initializer_array_borrow.con`). The collision is a separate defect from this
bug's and may deserve its own number; its containment is the E0602 refusal.

A fourth pass found `&mut X` as a call argument in an initializer, which took Lower's
write-back path and fabricated a local named `X` (`const Y = change(&mut X) + X` read 7). Check
does not check constant initializers, so Lower refuses every borrow form (`&`, `&mut`, a `&mut`
call argument, a borrow block) while an initializer is inlined (`inConstInit`, E0602;
`error_123_const_initializer_mut_arg.con`).

The open-bug reproducer row in `check_example_outputs.sh` is dropped, and `run_tests.sh` runs the
fixtures.

## Structural cause

A documented rule with no enforcing site for one of its cases (REF-3 names module constants, the
constant check reuses the binding predicate that is meant to look through an outer reference;
[structural class](STRUCTURAL_CLASSES.md) 13, with 092), and absence read as a fact in Lower: a
name with no local storage is given a fresh, uninitialized slot instead of being refused (class 11,
with 052 and 103). Constants are inlined by Lower, so the checker, the interpreter and Lower each
decide separately what a constant used as a place is (class 7, with 117).

## Structural owner

Exit 0b (silent wrong code): reject a constant of bare reference type with E0239 in `checkModule`
and `verifyNoStoredRefs` (exit 2i REF-3, fixture `error_2i_ref3_const_bare_ref.con`), and make
`addrOfLocal` refuse a name it has no storage for. The whole class is owned by R-0498 (S3, every
documented rule has an enforcing site) and R-0501 (S10, places in Place MIR, where a constant is
not a place a reference can borrow from).

## Containment while open (historical)

`scripts/tests/check_example_outputs.sh` (leg "open-bug reproducers", mode `wrong-exit:42`) builds
and runs the reproducer and requires that it does not exit 42. When the bug is fixed, by rejection
or by a correct value, that leg fails until this document, the corpus entry and the leg are updated
together.
