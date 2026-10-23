# Bug 185: a raw-pointer comparison through a safe generic escaped the authority rule

**Status:** FIXED (branch `soundness-holes-rawptr`, on top of `wip/train1`)
**Found:** 2026-10-21, by the Codex review of the bug 140 comparison rule, which recorded it as a
known limit ("recorded, not contained") in [bug 140's record](140_comparison_consumes_linear.md).
Numbered and fixed 2026-10-23.
**Severity:** an authority violation in safe code. Raw pointers compare only in trusted code or a
function declaring `with(Unsafe)` (bug 140, [ARITHMETIC_POLICY §8a](../language/ARITHMETIC_POLICY.md#8a-comparison-operators)).
Written directly in a safe function the comparison is E0229; reached through a safe `Copy`-bounded
generic it compiled. The comparison has no memory effect (it compares addresses), so this is a
rule bypass, not memory unsafety.

## Observed

```con
fn same<T: Copy>(a: T, b: T) -> bool {
    return a == b;
}

fn main() -> Int {
    let x: Int = 1;
    let p: *const Int = &x as *const Int;
    if same::<*const Int>(p, p) { return 42; }
    return 0;
}
```

Compiled and exited 42 (OVH server, `wip/train1` f92b8f32). `fn f(p: *const Int, q: *const Int)
-> bool { return p == q; }` is refused with E0229.

## Cause

Check decides a comparison at a type parameter once, on the generic body: a `Copy`-bounded `T`
compares. Each instantiation is held to the scalar rule after monomorphization by
`verifyComparisonTypes` (Verify.lean, E0229), which needs the instance's authority to decide a
raw-pointer operand. Mono built each instance as a fresh `CFnDef` listing the fields it copied
(`capSet`, `declSpan`, `foreignCalls`, ...) and left out `isTrusted`, so every instance looked like
safe code. Requiring the authority would then have refused std's trusted `alloc.heap_new`
instances, so the verifier accepted every raw-pointer comparison (`unsafeOk || true`).

## Structural cause

Class 6 in [STRUCTURAL_CLASSES.md](STRUCTURAL_CLASSES.md): capability and authority facts are not
carried across specialization. The instance is the generic's body under a substitution; its
authority is the definition's, exactly like its capability set, which Mono already copied.

## Structural owner

R-0499 (S8 capability containment, re-checked after Mono) and R-0510 (S8a specialization
identity), where an instance would carry its definition by reference instead of a hand-picked
field list. Exit 0b.

## Fix

Both instance constructions in Mono (explicit and inferred type arguments) copy `isTrusted` and
`trustedImplOrigin` with the body, and `comparisonTypeHook` applies the rule Check applies at the
source: a raw-pointer comparison in an instance is accepted only when
`capsAllowUnsafeOp isTrusted capSet` holds for that instance, which is its definition's
authority. No rule changed: the comparison is in the generic's body, and the rule is about the
code a comparison is written in. Std's trusted generics keep compiling (their instances are now
trusted); a safe generic instantiated at a raw pointer is refused whoever calls it, as it would
be if written out by hand.

## Containment and regression

- `tests/programs/error_185_generic_rawptr_eq_safe.con`: the reproducer, E0229 post-mono
  (`run_err`). It compiled and exited 42 before the fix.
- `tests/programs/regress_185_generic_rawptr_eq_authority.con`: a trusted generic and a generic
  declaring `with(Unsafe)`, each instantiated at `*const Int` (equal and unequal pointers) and at
  `Int`; exit 42 (`run_ok`).
