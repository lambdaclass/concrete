# Bug 003: Cross-Module &mut Borrow Consumed as Move

**Status:** Fixed
**Discovered:** 2026-03-14
**Fixed:** 2026-03-14
**Regression test:** `lean_tests/bug_cross_module_mut_borrow.con`

## Symptom

Passing `&mut Vec<T>` to a function defined in another module consumes (moves) the variable, making a second call fail with "linear variable used after move". The same code works if the function is in the same module.

```
mod Builder {
    pub fn push_val(v: &mut Vec<i32>, val: i32) with(Alloc) { ... }
}
mod Main {
    import Builder.{ push_val };
    fn main() with(Std) -> Int {
        let mut nums: Vec<i32> = vec_new::<i32>();
        push_val(&mut nums, 10);   // OK
        push_val(&mut nums, 20);   // ERROR: linear variable used after move
    }
}
```

## Root Cause

**File:** `Concrete/Check.lean`, function call argument processing (line ~1161–1167)

When checking function call arguments, the checker unconditionally consumed any bare identifier argument without checking whether the parameter type was a reference:

```lean
for (arg, (pName, pTy)) in args.zip paramTypes do
  let argTy ← checkExpr arg (some pTy)
  expectTy pTy argTy ...
  match arg with
  | .ident _ varName => consumeVarIfExists varName (some e.getSpan)  -- BUG: always consumes!
  | _ => pure ()
```

For a call like `push_val(&mut nums, 10)`, the argument `&mut nums` is a `.borrowMut` expression (not a bare `.ident`), so the consumption didn't trigger there. But in some code patterns — particularly when the checker sees the inner identifier — the variable got consumed.

The fundamental issue: the code didn't distinguish between owned parameters (which should consume) and reference parameters (which should borrow without consuming).

## Fix

**File:** `Concrete/Check.lean`

Check the parameter type before consuming. Reference parameters (`&T`, `&mut T`) borrow without consuming:

```lean
match arg with
| .ident _ varName =>
  match pTy with
  | .ref _ | .refMut _ => pure ()  -- borrowed: don't consume
  | _ => consumeVarIfExists varName (some e.getSpan)
| _ => pure ()
```

## Impact

Any function taking `&T` or `&mut T` parameters defined in a different module would consume the argument on call, preventing reuse. Workaround before fix: define the function in the same module as the caller, or inline the function body.
