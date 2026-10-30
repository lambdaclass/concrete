# Foreign Functions

The C boundary is explicit: foreign functions are declared `extern fn`, raw pointers are their own
types, and crossing the boundary needs `Unsafe` outside `trusted` code (TRUST-4, TRUST-5). This
chapter covers foreign function values (FFI-1), the one layout authority (FFI-2, pending), extern
declarations (FFI-3), layout attributes (FFI-4), symbol names (FFI-5), compiler intrinsics (FFI-6) and
what layout is promised (FFI-7, FFI-8). The design record is
[FFI.md](../platform/FFI.md), which keeps the wrapper patterns and the calling-convention design;
[LAYOUT_CONTRACT.md](../platform/LAYOUT_CONTRACT.md) keeps the layout reasoning.

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

Decided by the owner on 2026-10-29 (OD4): every foreign function value is refused now; typed `extern "C" fn` types may come later.

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
### FFI-3. An extern declaration declares its effects and FFI-safe types

**FFI-3@1.** An `extern fn` has no body, declares its effects with `with(...)` (`with()` for none, never for a C symbol known to have effects), and takes and returns only FFI-safe types: integers, floats, `bool`, `char`, `Unit`, raw pointers to a type that contains no reference, and `#[repr(C)]` structs.

- Kind: refusal
- Uses: REF-D1
- Codes: E0116 (resolve), E0117 (resolve), E0574 (core-check), E0575 (core-check)
- Status: enforced
- Enforced by: Concrete.coreCheckProgram, Concrete.ccCheckModuleDecls
- Semantics: none
- Obligation: foreign calls are external events governed by their declared contracts (R-0496)
- Evidence: tested

**Rationale.** An undeclared binding is never read as having no effects (bug 113), and a reference
or a Concrete aggregate has no C meaning, so it cannot cross. An `#[intrinsic = "n"]` declaration
that the compiler implements is not a foreign binding and declares no effects. A raw pointer to a
reference is refused too: C could keep the reference it was handed, or hand back the address of a
reference slot, and nothing on the Concrete side could see it. E0574 and E0575 look through raw
pointers with the REF-D1 judgment (gap G15 of MEMORY_SEMANTICS §15.7, closed 2026-10-23); a raw
pointer to a value type stays FFI-safe.

```con reject name=undeclared-effects code=E0116 stage=resolve pair=declared-extern
extern fn abs(x: i32) -> i32;

fn main() -> Int {
    return 0;
}
```

```con reject name=non-ffi-safe-parameter code=E0574 stage=core-check pair=declared-extern
struct S { v: Int }

extern fn f(s: S) with() -> i32;

fn main() -> Int {
    return 0;
}
```

```con accept name=declared-extern exit=42
trusted extern fn abs(x: i32) with() -> i32;

fn main() -> Int {
    return abs(0 - 42) as Int;
}
```

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

<a id="FFI-4"></a>
### FFI-4. Layout attributes are well formed

**FFI-4@1.** A `#[repr(C)]` struct is not generic and has only FFI-safe fields, `#[repr(packed)]` and `#[repr(align(N))]` are not combined, and `N` is a power of two no larger than the struct's natural alignment.

- Kind: refusal
- Uses: none
- Codes: E0572 (core-check), E0573 (core-check), E0576 (core-check), E0577 (core-check), E0585 (core-check)
- Status: enforced
- Enforced by: Concrete.ccCheckModuleDecls
- Semantics: none
- Obligation: layout comes from the one `Layout` authority (FFI-2, R-0502)
- Evidence: tested

**Rationale.** `repr(C)` promises C's layout, which a generic or a non-C field cannot have.
Over-alignment is refused because code generation does not support it yet (E0585).

```con reject name=align-not-power-of-two code=E0577 stage=core-check pair=repr-c
#[repr(C, align(3))]
struct S { v: Int }

fn main() -> Int {
    return 0;
}
```

```con accept name=repr-c exit=42
#[repr(C)]
struct Copy S { a: i32, b: i32 }

fn main() -> Int {
    let s: S = S { a: 40, b: 2 };
    return (s.a + s.b) as Int;
}
```

<a id="FFI-5"></a>
### FFI-5. Concrete symbols cannot collide with C symbols

**FFI-5@1.** Every Concrete-defined function is emitted under a mangled symbol in a namespace disjoint from C identifiers, and only an `extern fn` (which binds its `#[link_name]` or its declared name) and an `#[export_name]` function use a C name.

- Kind: refusal
- Uses: none
- Codes: E0118 (resolve), E0119 (resolve)
- Status: enforced
- Enforced by: Concrete.Pipeline.emit
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** A Concrete function named `malloc` used to replace C's `malloc` (bug 129). Every
Concrete symbol contains `.` or `$`, which no C identifier does, so the namespaces are disjoint by
construction. An export must be a C identifier of a C-compatible shape that no `extern fn` imports
and that is not a runtime symbol the program uses; a missing foreign function is an error, never a
synthesized body.

```con reject name=bad-export-name code=E0119 stage=resolve pair=concrete-malloc
#[export_name = "main"]
fn entry() -> i32 {
    return 0;
}

fn main() -> Int {
    return 0;
}
```

```con accept name=concrete-malloc exit=42
struct Copy B { v: Int }

fn malloc(n: Int) -> Int {
    return n;
}

fn main() with(Std) -> Int {
    let h: Heap<B> = alloc(B { v: 40 });
    let b: B = *h;
    return b.v + malloc(2);
}
```

<a id="FFI-6"></a>
### FFI-6. A compiler intrinsic is not a foreign binding

**FFI-6@1.** A body-less declaration `#[intrinsic = "n"]` whose name is `n`, a compiler intrinsic, and that has no `#[link_name]` names an operation the compiler implements, so it is not a foreign binding, need not declare its effects, and keeps the `with(...)` it is written with; any other body-less declaration is a foreign binding.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Pipeline.resolve
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** FFI-3's effect declaration exists because C can do anything; an intrinsic is implemented by
the compiler, which knows what it does. std declares `sizeof` and `alignof` this way, and any
module may (bug 112: the same declaration outside std was refused as an undeclared foreign
binding). The sentence is Resolve's `isCompilerIntrinsic`; the assumption report
(`Concrete/Report/AssumptionSummary.lean`) classifies more narrowly, as a tooling choice about which
foreign assumptions to list, not a language rule.

```con accept name=user-intrinsic exit=5
mod foo {
    #[intrinsic = "sizeof"]
    pub trusted fn sizeof<T>() -> u64;
}
mod bar {
    import foo.{sizeof};
    pub fn main() -> u64 {
        return sizeof::<u8>() + sizeof::<u32>();
    }
}
```

<a id="FFI-7"></a>
### FFI-7. Layout is unspecified unless a `repr` fixes it

**FFI-7@1.** The field order, offsets, padding, size and alignment of a struct or enum are unspecified except as a struct's `#[repr]` attribute fixes them (FFI-8), and safe code observes them only through `sizeof::<T>()` and `alignof::<T>()`, which give `T`'s size and alignment in bytes as a `u64`.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Layout.tySize, Concrete.Layout.tyAlign
- Semantics: none
- Obligation: layout comes from the one `Layout` authority (FFI-2, R-0502)
- Evidence: tested

**Rationale.** The promised layout is smaller than the implemented one, so the compiler may change
an unannotated layout and a program cannot come to depend on it. Safe code has no other window:
it cannot turn a pointer into an integer (TRUST-1) or read through a raw pointer (TRUST-3). Trusted
code that walks a buffer may also rely on the stride of an array of `T` being `sizeof::<T>()`
([LAYOUT_CONTRACT.md](../platform/LAYOUT_CONTRACT.md) §3.3). An enum has no `repr` and crosses the
foreign boundary as an integer.

```con accept name=observed-through-sizeof exit=42
mod m {
    #[intrinsic = "sizeof"]
    pub trusted fn sizeof<T>() -> u64;
    #[intrinsic = "alignof"]
    pub trusted fn alignof<T>() -> u64;

    struct Copy S { a: u8, b: u32 }

    pub fn main() -> Int {
        if sizeof::<S>() >= 5 && alignof::<S>() >= 1 {
            return 42;
        }
        return 0;
    }
}
```

<a id="FFI-8"></a>
### FFI-8. What `repr(C)` and `repr(packed)` promise

**FFI-8@1.** `#[repr(C)]` lays a struct out as C does, each field in declaration order at the next offset aligned for its type and the size rounded up to the struct's alignment, and `#[repr(packed)]` places the fields in declaration order with no padding and gives the struct alignment 1.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Layout.tySize, Concrete.Layout.tyAlign
- Semantics: none
- Obligation: layout comes from the one `Layout` authority (FFI-2, R-0502)
- Evidence: tested

**Rationale.** `repr(C)` is what a struct needs to cross `extern fn` (FFI-3), and `repr(packed)`
describes wire formats. `#[repr(align(N))]` promises only a minimum alignment, and today only where
it changes nothing (FFI-4, E0585). Which combinations are well formed is FFI-4.

```con accept name=c-and-packed exit=42
mod m {
    #[intrinsic = "sizeof"]
    pub trusted fn sizeof<T>() -> u64;
    #[intrinsic = "alignof"]
    pub trusted fn alignof<T>() -> u64;

    #[repr(C)]
    struct Copy R { v: u8, l: u32 }
    #[repr(C, packed)]
    struct Copy H { v: u8, f: u16, l: u32 }
    #[repr(packed)]
    struct Copy P { v: u8, l: u32 }

    pub fn main() -> Int {
        if sizeof::<R>() == 8 && alignof::<R>() == 4 && sizeof::<H>() == 7 && alignof::<H>() == 1
            && sizeof::<P>() == 5 && alignof::<P>() == 1 {
            return 42;
        }
        return 0;
    }
}
```
