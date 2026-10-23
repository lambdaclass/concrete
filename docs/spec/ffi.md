# Foreign Functions

This chapter is a stub. It holds the decided rule on foreign function values and the reference
rule at the C boundary. The rest of the C boundary,
raw pointers and `trusted` code migrate here and to `trusted.md` later (README §8, step 6),
from [FFI.md](../platform/FFI.md), SAFETY and TWO_AXIS_SAFETY.

## Rules

<a id="FFI-1"></a>
### FFI-1. A foreign function is only called directly

**FFI-1@1.** No `extern fn` is used as a value, and an extern function is only ever called directly.

- Kind: refusal
- Uses: none
- Codes: E0588 (elab, core-check)
- Status: enforced
- Enforced by: Concrete.refuseForeignFnValue
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Decided 2026-10-23 (TIGHTENING_DECISIONS D4). A Concrete function type and a C
function type use different calling conventions. A foreign function used as a Concrete
`fn(..)` value is called with the wrong one (bug 166). Scalars happen to be passed alike, but
the rule does not rest on that coincidence. The rewrite is a Concrete wrapper that calls the
extern directly and is passed instead. The long-term design puts the calling convention in the
function type (`extern "C" fn(A) -> R`, distinct from `fn(A) -> R`). It lifts this refusal when
the first workload passes a callback to C. Enforced 2026-10-23 where a global function becomes a
value, on the resolver's identity of the name, so an import alias, a constant initializer and a
shadowing local are all decided right; CoreCheck re-checks it.

```con reject name=extern-as-value code=E0588 stage=elab pair=wrapper-as-value
mod c {
    pub trusted extern fn abs(x: i32) with() -> i32;
}
mod m {
    import c.{abs};
    pub fn main() -> i32 {
        let f: fn(i32) -> i32 = abs;
        return f(0 - 5);
    }
}
```

```con accept name=wrapper-as-value exit=5
mod c {
    pub trusted extern fn abs(x: i32) with() -> i32;
}
mod m {
    import c.{abs};
    fn abs_wrapper(x: i32) -> i32 {
        return abs(x);
    }
    pub fn main() -> i32 {
        let f: fn(i32) -> i32 = abs_wrapper;
        return f(0 - 5);
    }
}
```

<a id="FFI-2"></a>
### FFI-2. One Layout authority for AAPCS64

**FFI-2@1.** Every size, alignment, field offset and AAPCS64 argument and return classification comes from one `Layout` authority, which the checker, the lowering and the ABI adapter all consult.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0502
- Semantics: none
- Obligation: `Layout` is the single proved authority, and the representation certificates it emits are checked (R-0502)
- Evidence: tested

**Rationale.** When two components compute a layout separately, they can disagree, and the
disagreement shows up as wrong code at the boundary (structural classes 8 and 12). One authority,
used by every consumer and stated for the AAPCS64 calling convention of the native profile,
removes the second copy.

<a id="FFI-3"></a>
### FFI-3. No reference crosses the C boundary

**FFI-3@1.** No `extern fn` has a parameter or result type that contains a reference, also behind a raw pointer.

- Kind: refusal
- Uses: REF-D1
- Codes: E0574 (core-check), E0575 (core-check)
- Status: enforced
- Enforced by: Concrete.ccCheckModuleDecls
- Semantics: none
- Obligation: INV-REF (R-0500) at the foreign boundary (R-0285)
- Evidence: tested

**Rationale.** A reference is second-class and never outlives the call that lends it (REF-1,
REF-3). C could keep a reference it was handed, or hand back the address of a reference slot,
and nothing on the Concrete side could see it. A raw pointer to a value type stays FFI-safe.
Gap G15 (2026-10-23): E0574 and E0575 rejected only a direct reference, and now look through raw
pointers with the REF-D1 judgment. The two codes also refuse other FFI-unsafe types, which is
not yet part of this chapter (README §8, step 6).

```con reject name=returns-pointer-to-ref code=E0575 stage=core-check pair=pointer-to-value
trusted extern fn f() with() -> *const &Int;

fn main() -> Int {
    return 0;
}
```

```con reject name=param-pointer-to-ref code=E0574 stage=core-check pair=pointer-to-value
trusted extern fn keep(p: *const &Int) with();

fn main() -> Int {
    return 0;
}
```

```con accept name=pointer-to-value exit=42
trusted extern fn f() with() -> *const Int;
trusted extern fn keep(p: *const Int) with();

fn main() -> Int {
    return 42;
}
```

