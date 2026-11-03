# Types

Every expression has one type, and there are no implicit conversions. This chapter covers
callable values and dispatch (TYPE-1, TYPE-2), `Text` (TYPE-3), the typing of expressions and
operators (TYPE-4 to TYPE-10), construction rights (TYPE-11), type aliases (TYPE-12) and the
kinds of type parameters (TYPE-13, TYPE-14), generic arguments (TYPE-15), finite size (TYPE-16),
capability sets in types (TYPE-D1, TYPE-17, TYPE-18), the primitive numeric types (TYPE-19), `char`
(TYPE-20), the refusal of newtypes (TYPE-21), the builtin `Option` and `Result` (TYPE-23) and
the refusal of newtype conversions (TYPE-24). Copy itself is defined in the ownership chapter
(OWN-D1, OWN-14). What a capability means and when a function may use one are in the trusted
chapter (TRUST-D1, TRUST-4). The section "Builtin functions" below lists every builtin function
with its current signature.

The design records are [VALIDATED_WRAPPERS.md](../stdlib/VALIDATED_WRAPPERS.md) (a library pattern built on
TYPE-11, not a language rule), [VALUE_MODEL.md](../archive/design-records/VALUE_MODEL.md) (absorbed
into TYPE-21, FFI-7 and FFI-8 on 2026-10-30) and
[CALLABLE_VALUES_AND_CAPABILITIES.md](../archive/design-records/CALLABLE_VALUES_AND_CAPABILITIES.md).
Const generics are not part of the language. Their design is decided, and the build is
deferred until a workload needs it (see "No const generics" below).
Layout attributes (`#[repr(C)]`, `packed`, `align`) belong to the FFI chapter.

## Definitions

<a id="TYPE-D1"></a>
### TYPE-D1. Capability set

**TYPE-D1@2.** A capability set is a set of capabilities (TRUST-D1) and capability variables, written as a comma-separated list in `with(...)` and as `{}` or names joined by `+` in a capability argument, and a function or a struct declares a capability variable as a parameter `<cap C>`.

- Kind: definition
- Uses: TRUST-D1
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.CapSet
- Semantics: none
- Obligation: none
- Evidence: by-users

**Rationale.** Capability sets are extensional and duplicate members are refused (OD108); capability aliases, including `Std`, are removed, and set variables are substituted exactly rather than unioned on inconsistency (authority rules 3–4). Ambiguous inference is [undecided: see the target Core proposal](decisions.md#core-capability-inference).

## Rules

<a id="TYPE-1"></a>
### TYPE-1. A callable value is a function pointer with explicit context

**TYPE-1@1.** A value of function type is a pointer to a named function, and any context the callee needs is passed to it as an explicit argument, so the language has no closures.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Ty
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** A closure captures part of its environment. Capturing a reference would store it,
which REF-3 forbids, and capturing an owned value would need its own ownership and conversion
rules. A function pointer plus an explicit context argument keeps both in view: the context is an
ordinary parameter, borrowed or owned under the ordinary rules, and its capabilities are visible
at the call site (TYPE-17). VERIFIED_PIPELINE's first verified profile excludes closures for the
same reason. The type grammar (`Ty`) has function-pointer types and no closure type, and a nested
`fn` declaration is a parse error: it would add locality but no expressive power, and invite the
expectation that it captures. A function pointer is Copy (OWN-D1); its context is borrowed shared,
lent `&mut` for each call (REF-10) or moved into a single call (OWN-1). A callback cannot reach a
container its caller has borrowed for the call: a context cannot hold a reference (REF-3), and one
call's borrows of a root conflict (REF-11). A first-class value pairing a context with a function
is deferred until a workload needs to store one
([CALLABLE_VALUES_AND_CAPABILITIES.md](../archive/design-records/CALLABLE_VALUES_AND_CAPABILITIES.md) §4).

```con accept name=explicit-context exit=42
struct Ctx { add: i64 }

fn apply(ctx: &Ctx, x: i64, f: fn(&Ctx, i64) -> i64) -> i64 {
    return f(ctx, x);
}

fn plus(c: &Ctx, x: i64) -> i64 {
    return c.add + x;
}

fn example_value() -> i64 {
    let c: Ctx = Ctx { add: 40 };
    let r: i64 = apply(&c, 2, plus);
    let Ctx { add } = c;
    return r + add * 0;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="TYPE-2"></a>
### TYPE-2. No dynamic dispatch

**TYPE-2@1.** There are no trait-object types, so every method call through a trait bound resolves to one implementation at compile time, by monomorphization.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Ty, Concrete.monoProgram
- Semantics: none
- Obligation: the substitution and dispatch theorem of S8a (R-0510)
- Evidence: tested

**Rationale.** A call through a trait bound is specialized at each instantiation, so the code that
runs is known statically. Dynamic dispatch would add a vtable layout, an indirect-call rule and a
second dispatch semantics to the formal model. Indirect calls exist only through function-pointer
values (TYPE-1). The coherence rules for impls (TIGHTENING_DECISIONS D7) belong to the traits
chapter.

```con accept name=static-dispatch exit=42
trait Area {
    fn area(&self) -> i64;
}

struct Copy Sq { s: i64 }

impl Area for Sq {
    fn area(&self) -> i64 { return self.s * self.s; }
}

fn total<T: Area>(x: &T) -> i64 {
    return x.area();
}

fn example_value() -> i64 {
    let q: Sq = Sq { s: 6 };
    return total::<Sq>(&q) + 6;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="TYPE-3"></a>
### TYPE-3. `Text` is owned

**TYPE-3@2.** A `Text` owns an immutable copy of its bytes, never aliases another value's buffer, and is linear.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Enforced by: std/src/text.con#Text
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Ownership of immutable text remains required, but the literal/static-text type and
its conversion to allocating strings are [undecided: see the target Core proposal](decisions.md#core-static-text).
The old allocating library example is retired until it can be stated without choosing that API.

<a id="TYPE-4"></a>
### TYPE-4. One type per expression, no implicit conversion

**TYPE-4@2.** Every expression has exactly one type, and every position a value flows into (a binding, an argument, a return, a field, a branch or arm of a value-producing `if` or `match`) expects that type exactly, with no implicit conversion, except that an expression that never completes (a diverging `return`, `break`, `continue` or call) fits any position other than a condition (TYPE-5).

- Kind: refusal
- Uses: none
- Codes: E0220 (check), E0222 (check), E0224 (check), E0225 (check), E0226 (check), E0500 (core-check), E0560 (core-check)
- Status: enforced
- Enforced by: Concrete.expectTy, Concrete.checkExprCore, Concrete.checkStmt, Concrete.ccCheckExpr, Concrete.ccCheckStmt
- Semantics: none
- Obligation: progress and preservation for the Core type system (R-0507)
- Evidence: tested

**Rationale.** Conversions are explicit (`as`, or a named operation in the arithmetic chapter),
so the type of every value is visible in the source. An integer literal takes the type its
position expects. A literal that does not fit that type is an arithmetic rule. CoreCheck repeats
the variable and return checks on Core as defense in depth.
A `break` carries no value (STMT-6@3, OD27), so it is not one of the positions.
The current compiler still parses `break <expr>;`, evaluating and dropping the value (bug 323).

```con reject name=bool-for-int code=E0220 stage=check pair=one-type
fn example_value() -> i64 {
    let x: i64 = true;
    return x;
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con reject name=branches-disagree code=E0224 stage=check pair=one-type
fn example_value() -> i64 {
    let c: bool = true;
    let x: i64 = if c { 1 } else { false };
    return x;
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=one-type exit=42
fn example_value() -> i64 {
    let c: bool = true;
    let x: i64 = if c { 42 } else { 0 };
    return x;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con reject name=capabilities-not-erased code=E0220 stage=check pair=same-capabilities
fn inc(x: i64) with(Fs) -> i64 {
    return x + 1;
}

fn example_value() with(Fs) -> i64 {
    let f: fn(i64) -> i64 = inc;
    return f(41);
}

fn main() with(Fs) -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=capability-lost-in-argument code=E0220 stage=check pair=same-capabilities
fn apply(f: fn(i64) -> i64, x: i64) -> i64 {
    return f(x);
}

fn inc(x: i64) with(Fs) -> i64 {
    return x + 1;
}

fn example_value() with(Fs) -> i64 {
    return apply(inc, 41);
}

fn main() with(Fs) -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=capability-lost-in-field code=E0220 stage=check pair=same-capabilities
struct Copy Box { f: fn(i64) -> i64 }

fn inc(x: i64) with(Fs) -> i64 {
    return x + 1;
}

fn example_value() with(Fs) -> i64 {
    let b: Box = Box { f: inc };
    let g: fn(i64) -> i64 = b.f;
    return g(41);
}

fn main() with(Fs) -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=same-capabilities exit=42
fn inc(x: i64) with(Fs) -> i64 {
    return x + 1;
}

fn example_value() with(Fs) -> i64 {
    let f: fn(i64) with(Fs) -> i64 = inc;
    return f(41);
}

fn main() with(Fs) -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="TYPE-5"></a>
### TYPE-5. Conditions are `bool`

**TYPE-5@3.** The condition of an `if` or `while` and the operands of `&&`, `||` and `!` have type `bool`, and a comparison has type `bool`.

- Kind: refusal
- Uses: none
- Codes: E0223 (check), E0504 (core-check), E0505 (core-check), E0508 (core-check), E0540 (core-check), E0541 (core-check)
- Status: pending R-0555
- Enforced by: Concrete.checkExprCore, Concrete.checkStmt, Concrete.ccCheckExpr, Concrete.ccCheckStmt
- Semantics: none
- Obligation: progress and preservation for the Core type system (R-0507)
- Evidence: tested

**Rationale.** OD55 removes guards; conditions and logical operands remain boolean.

```con reject name=int-condition-decided code=E0223 stage=check pair=bool-condition pending=R-0555 now=reject:E0223:check
fn main() -> u8 {
    let n: i64 = 1;
    if n {
        return 42;
    }
    return 0;
}
```

```con accept name=bool-condition exit=42
fn main() -> u8 {
    let n: i64 = 1;
    if n != 0 {
        return 42;
    }
    return 0;
}
```

<a id="TYPE-6"></a>
### TYPE-6. Operators take their declared primitive types

**TYPE-6@3.** Arithmetic operators take two operands of one numeric type, bitwise and shift operators take integers, unary minus takes a number, comparisons take two operands of one numeric type or two `bool` or two `char` operands, and no operator takes a raw pointer.

- Kind: refusal
- Uses: none
- Codes: E0228 (check), E0229 (check), E0501 (core-check, post-mono), E0502 (core-check), E0503 (core-check), E0506 (core-check), E0507 (core-check), E0509 (core-check)
- Status: pending R-0555
- Enforced by: Concrete.checkExprCore, Concrete.ccCheckExpr
- Semantics: none
- Obligation: one `IntArith` authority for operand widths (R-0493)
- Evidence: tested

**Rationale.** OD42 admits bool and char comparisons; numeric operands still agree in type and raw-pointer operators remain refused.

```con reject name=compare-structs-decided code=E0229 stage=check pair=same-type-operands pending=R-0555 now=reject:E0229:check
struct Copy P { n: i64 }

fn main() -> u8 {
    let a: P = P { n: 1 };
    let b: P = P { n: 1 };
    if a == b {
        return 42;
    }
    return 0;
}
```

```con accept name=same-type-operands exit=42
struct Copy P { n: i64 }

fn example_value() -> i64 {
    let a: P = P { n: 21 };
    let b: P = P { n: 21 };
    if a.n == b.n {
        return a.n + b.n;
    }
    return 0;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con reject name=pointer-comparison code=E0229 stage=check pair=pointee-comparison pending=bug-322 now=accept:42
trusted fn same(p: *const i64, q: *const i64) -> bool {
    return p == q;
}
fn main() -> u8 {
    let x: i64 = 1;
    let p: *const i64 = &x as *const i64;
    if same(p, p) { return 42; }
    return 0;
}
```

```con accept name=pointee-comparison exit=42
trusted fn same(p: *const i64, q: *const i64) -> bool {
    return *p == *q;
}
fn main() with(Unsafe) -> u8 {
    let x: i64 = 1;
    if same(&x as *const i64, &x as *const i64) { return 42; }
    return 0;
}
```

<a id="TYPE-7"></a>
### TYPE-7. Aggregates are built and read by their declarations

**TYPE-7@2.** A struct literal names a declared struct and gives each of its fields exactly once explicitly, with no struct update `..base`, a field access names a field of a struct, an enum literal or pattern names a variant of a declared enum, an array literal has at least one element, and only an array is indexed, by an integer.

- Kind: refusal
- Uses: none
- Codes: new (parse), E0250 (check), E0251 (check), E0252 (check), E0253 (check), E0254 (check), E0260 (check), E0261 (check), E0306 (check), E0416 (elab), E0550 (core-check), E0551 (core-check), E0552 (core-check), E0316 (check)
- Status: pending R-0555
- Enforced by: Concrete.checkExprCore, Concrete.checkStmt, Concrete.ccCheckExpr
- Semantics: none
- Obligation: progress and preservation for the Core type system (R-0507)
- Evidence: tested

**Rationale.** OD53 removes functional update: every struct field is explicit. Patterns are governed by the patterns chapter and private representation by TYPE-11.

```con reject name=missing-field-decided code=E0252 stage=check pair=every-field pending=R-0555 now=reject:E0252:check
struct Copy P { a: i64, b: i64 }

fn example_value() -> i64 {
    let p: P = P { a: 42 };
    return p.a;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=unknown-field-decided code=E0253 stage=check pair=every-field pending=R-0555 now=reject:E0253:check
struct Copy P { a: i64, b: i64 }

fn example_value() -> i64 {
    let p: P = P { a: 42, b: 0, c: 1 };
    return p.a;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=duplicate-field-decided code=E0306 stage=check pair=every-field pending=R-0555 now=reject:E0306:check
struct Copy P { a: i64, b: i64 }

fn example_value() -> i64 {
    let p: P = P { a: 1, b: 2, a: 40 };
    return p.a + p.b;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```



```con accept name=every-field exit=42
struct Copy P { a: i64, b: i64 }

fn example_value() -> i64 {
    let p: P = P { a: 40, b: 2 };
    let arr: [i64; 2] = [p.a, p.b];
    return arr[0] + arr[1];
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con reject name=struct-update-removed code=new stage=parse pair=every-field pending=R-0555 now=accept:42
struct Copy P { a: u8, b: u8 }
fn main() -> u8 { let p: P = P { a: 1, b: 2 }; let q: P = P { a: 40, ..p }; return q.a + q.b; }
```

<a id="TYPE-8"></a>
### TYPE-8. Only a reference or a raw pointer is dereferenced

**TYPE-8@1.** The operand of `*` is a reference, a raw pointer or an owned `Heap`, and an assignment through `*` needs a `&mut T` or `*mut T`.

- Kind: refusal
- Uses: none
- Codes: E0221 (check), E0554 (core-check), E0555 (core-check)
- Status: enforced
- Enforced by: Concrete.checkExprCore, Concrete.ccCheckExpr, Concrete.ccCheckStmt
- Semantics: none
- Obligation: progress and preservation for the Core type system (R-0507)
- Evidence: tested

**Rationale.** A shared reference grants no write access, so writing through `&T` is refused. A
raw-pointer dereference also needs trusted or `Unsafe` code (TRUST-3). `*h` of an owned
`Heap` moves the value out of its cell (OWN-15, `deref`).

```con reject name=deref-int code=E0221 stage=check pair=deref-ref
fn example_value() -> i64 {
    let x: i64 = 42;
    return *x;
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=deref-ref exit=42
fn read(r: &i64) -> i64 {
    return *r;
}

fn example_value() -> i64 {
    let x: i64 = 42;
    return read(&x);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="TYPE-9"></a>
### TYPE-9. Calls match their signatures

**TYPE-9@2.** A call passes exactly as many arguments as the callee has parameters and, when it writes a type-argument list, exactly as many type arguments as the callee has type parameters, and a method call names a method of the receiver's named type or of a trait that bounds the receiver's type parameter.

- Kind: refusal
- Uses: none
- Codes: E0262 (check), E0264 (check), E0265 (check), E0266 (check), E0522 (core-check), E0313 (check), E0317 (check), E0322 (check)
- Status: enforced
- Enforced by: Concrete.checkExprCore
- Semantics: none
- Obligation: progress and preservation for the Core type system (R-0507)
- Evidence: tested

**Rationale.** There are no default arguments, no variadic functions and no method lookup
through implicit conversions. A method of a type parameter comes only from its bounds, which is
what lets a generic body be checked once (TYPE-2, PASS-1). E0322 enforces the type-argument count (bug 221: an extra type argument was dropped silently). A body-less declaration (an `#[intrinsic]`) keeps no type-parameter
list, so the current compiler still skips the count there (bug 333), pinned by the pending example.

```con reject name=wrong-arity code=E0262 stage=check pair=right-arity
fn add(a: i64, b: i64) -> i64 {
    return a + b;
}

fn example_value() -> i64 {
    return add(42);
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```



```con reject name=receiverless-trait-method code=E0317 stage=check pair=right-arity
trait Make { fn make() -> i64; }
fn call<T: Make>(x: &T) -> i64 { return x.make(); }
fn main() -> u8 { return 0; }
```

```con accept name=right-arity exit=42
fn add(a: i64, b: i64) -> i64 {
    return a + b;
}

fn example_value() -> i64 {
    return add(40, 2);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```





<a id="TYPE-10"></a>
### TYPE-10. Builtins have fixed signatures

**TYPE-10@2.** A builtin is called with the argument count, type-argument count and argument types of its fixed signature, allocation takes an allocator and reports conserving failure, and the removed `free` and `unwrap` forms are refused.

- Kind: refusal
- Uses: none
- Codes: E0268 (check), E0269 (check), E0270 (check), E0271 (check), E0274 (check), E0421 (check, elab), E0314 (check), E0315 (check)
- Status: pending R-0555
- Enforced by: Concrete.checkExprCore
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Signatures and primitive effects are fixed by identity, not by declarations. MEM-5 and MEM-7 govern conserving allocation; OD59 removes `free` and OD155 removes `unwrap`. Complete failure payload contracts are [undecided: see the target Core proposal](decisions.md#core-allocation-contracts).

```con reject name=wrapping-wrong-arity code=E0268 stage=check pair=wrapping-valid-arguments
fn main() -> u8 {
    return wrapping_add(42);
}
```

```con reject name=wrapping-wrong-type code=E0314 stage=check pair=wrapping-valid-arguments
fn main() -> u8 {
    return wrapping_add(true, false);
}
```

```con accept name=wrapping-valid-arguments exit=42
fn main() -> u8 {
    return wrapping_add(40, 2);
}
```

```con reject name=free-non-heap-decided code=E0274 stage=check pair=free-heap pending=R-0555 now=reject:E0274:check
fn main() with(Alloc) -> u8 {
    let x: i64 = 42;
    free(x);
    return 0;
}
```

```con reject name=wrapping-on-non-integers-decided code=E0314 stage=check pair=free-heap pending=R-0555 now=reject:E0314:check
fn main() -> u8 {
    let b: bool = wrapping_add(true, false);
    return 0;
}
```

```con reject name=print-without-arguments-decided code=E0315 stage=check pair=free-heap pending=R-0555 now=reject:E0315:check
fn main() with(Alloc) -> u8 {
    print();
    return 0;
}
```

```con accept name=free-heap exit=42
fn main() -> u8 { let n: u64 = sizeof::<u8>(); if n == 1 { return 42; } return 1; }
```

<a id="TYPE-11"></a>
### TYPE-11. A module keeps its types' representation

**TYPE-11@2.** A struct field not marked `pub` is not named in a struct literal, read or written outside its declaring module, including by a parent module, and newtype declarations and conversions are refused.

- Kind: refusal
- Uses: none
- Codes: E0296 (check), E0297 (check), E0298 (check)
- Status: pending R-0555
- Enforced by: Concrete.checkExprCore, Concrete.checkStmt
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** OD21 protects representation even from a parent module; OD54 removes newtypes rather than introducing conversion exceptions.



```con reject name=read-private-field-decided code=E0298 stage=check pair=public-constructor pending=R-0555 now=reject:E0298:check
// spec-file: main.con
mod box;
import box.{Bx, mk};
fn example_value() -> i64 {
    let b: Bx = mk();
    return b.v;
}
pub fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}

// spec-file: box.con
pub struct Copy Bx { v: i64 }
pub fn mk() -> Bx {
    return Bx { v: 42 };
}

```

```con reject name=destructure-private-field-decided code=E0298 stage=check pair=public-constructor pending=R-0555 now=reject:E0298:check
// spec-file: main.con
mod box;
import box.{Bx, mk};
fn example_value() -> i64 {
    let b: Bx = mk();
    let Bx { v } = b;
    return v;
}
pub fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}

// spec-file: box.con
pub struct Copy Bx { v: i64 }
pub fn mk() -> Bx {
    return Bx { v: 42 };
}

```





```con reject name=parent-reads-child-private-field-decided code=E0298 stage=check pair=public-constructor pending=R-0555 now=reject:E0298:check
// spec-file: main.con
mod kids;
import kids.{Box, mk};
fn example_value() -> i64 {
    let b: Box = mk();
    return b.v;
}
pub fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}

// spec-file: kids.con
pub struct Copy Box { v: i64 }
pub fn mk() -> Box {
    return Box { v: 42 };
}

```




```con accept name=public-constructor exit=42
// spec-file: main.con
mod box;
import box.{Bx, mk, get};
fn main() -> u8 { let b: Bx = mk(); return get(b); }
// spec-file: box.con
pub struct Copy Bx { v: u8 }
pub fn mk() -> Bx { return Bx { v: 42 }; }
pub fn get(b: Bx) -> u8 { return b.v; }
```

<a id="TYPE-12"></a>
### TYPE-12. A type alias is another name for the same type

**TYPE-12@1.** `type A = T;` makes `A` another spelling of `T` in every position, with the same layout, extraction and proof identity, and an alias is never recursive.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.expandAliasDeep, Concrete.closeAliasMap
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** An alias is transparent, not nominal: for a distinct type with its own identity,
use a newtype. Aliases are expanded transitively when types are resolved, so every later stage
sees the underlying type. A program written with an alias and the same program written without
it have byte-identical layout and fingerprint reports. Aliases take no type parameters, but an
alias may name a generic instantiation (`type IntBox = Box<i32>;`). An unknown target is refused
at the declaration (E0108) and so is a cycle (E0112), both in the resolver.

An alias is written `type Name = <type>;` (`type Digest = [u8; 32];`, `type Point = P;`), and
`pub type Meters = i32;` is importable like any public item. Aliases compose: a chain
(`type C = B; type B = A; type A = i32`) resolves `C` to `i32`, and an alias nested in another type
(`type Arr = [E; 3]` with `type E = i32`) expands to `[i32; 3]`. The expansion is structural and
transitive (`closeAliasMap`, `expandAliasDeep`), so layout, the Copy and representation checks,
monomorphization and lowering all see the underlying type: `type Id = i32; struct Copy S { a: Id }`
is Copy. An alias never shifts a struct's size or offsets and never changes a function's semantic
body fingerprint, so it cannot affect proof or evidence identity; `scripts/tests/check_type_alias.sh`
(fixtures in `tests/programs/type_alias/`) asserts the byte-identical `--report layout` and
`--report fingerprints`. The unknown-target refusal holds even for an alias that is never used, and
the cycle refusal (`type A = A;`, or `type A = B; type B = A;`) is its own diagnostic rather than a
later type mismatch. Not provided in V1: alias type parameters (`type Pair<T> = ...`; target a
concrete instantiation instead) and opaque or distinct aliases (a newtype gives a type its own
identity). Implemented as ROADMAP old Phase 6 #3 (2026-06-22).

```con accept name=alias-is-transparent exit=12
type Id = i32;
fn want_underlying(x: i32) -> i32 { return x; }
fn want_alias(x: Id) -> Id { return x; }
fn example_value() -> i64 {
    let a: Id = 5;
    let b: i32 = a;
    let c: Id = want_underlying(b);
    let d: i32 = want_alias(7);
    return (c + d) as i64;
}
fn main() -> u8 {
    if example_value() == 12 {
        return 12;
    }
    return 1;
}
```

<a id="TYPE-13"></a>
### TYPE-13. A bare type parameter is linear-capable

**TYPE-13@3.** A type parameter without a bound may be instantiated at any type, linear ones included, so a generic body uses each value of that parameter exactly once, and `T: Copy` is the only bound that lets a body duplicate one.

- Kind: guarantee
- Uses: OWN-D1
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.isCopyType
- Semantics: none
- Obligation: binder and kind facts survive specialization (PASS-1, PASS-2)
- Evidence: tested

**Rationale.** Decided 2026-10-23 as the normative reading of today's behaviour
(TIGHTENING_DECISIONS D5): Copy is the only property that changes what a body may do, so a
mandatory kind annotation would add syntax without removing a semantic case. There is no
linear-only kind. Duplicating a value of a bare `T` is a use after move (OWN-1).

Decided by the owner on 2026-10-29 (OD5): a bare `<T>` means Any, and `T: Copy` is explicit.

```con accept name=bare-and-copy-parameters exit=42
struct R { v: i64 }

fn pass<T>(x: T) -> T {
    return x;
}

fn twice<T: Copy>(x: T, f: fn(T) -> i64) -> i64 {
    return f(x) + f(x);
}

fn half(n: i64) -> i64 {
    return n / 2;
}

fn example_value() -> i64 {
    let r: R = pass::<R>(R { v: 0 });
    let R { v } = r;
    return twice::<i64>(42, half) + v;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="TYPE-14"></a>
### TYPE-14. Copy is declared, never derived

**TYPE-14@3.** Whether an instantiation of a struct or enum is Copy is decided from its declaration alone, so it is Copy exactly when the struct or enum is declared `Copy` and every type argument is Copy.

- Kind: guarantee
- Uses: OWN-D1
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.Layout.isCopyTyGeneric
- Semantics: none
- Obligation: Copy is decided from the declaration and the type arguments alone, with no fixpoint over the type graph (R-0510)
- Evidence: tested

**Rationale.** Decided 2026-10-23 (TIGHTENING_DECISIONS D6, option (a)) and enforced the same
day. Copy used to be derived from an instantiation's fields, which Mono computed as a fixpoint
over the type graph; the fixpoint is gone. A generic declared `Copy` behaves like a derived
`Copy`: `struct Copy Bx<T> { v: T }` is accepted, its declaration checked once with `T` taken
as Copy (OWN-14), and `Bx<Int>` is Copy while `Bx<String>` is linear. A parameter no field
mentions still counts. A generic not declared `Copy` is never Copy.

Decided by the owner on 2026-10-29 (OD5): Copy for a generic struct or enum is declared and checked, never derived, and a generic declared Copy is Copy when every argument is.

```con accept name=copy-over-bare-parameter-at-copy exit=42
struct Copy Bx<T> { v: T }

fn example_value() -> i64 {
    let b: Bx<i64> = Bx::<i64> { v: 21 };
    let c: Bx<i64> = b;
    return b.v + c.v;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con accept name=copy-over-copy-parameter exit=42
struct Copy Bx<T: Copy> { v: T }

fn example_value() -> i64 {
    let b: Bx<i64> = Bx::<i64> { v: 42 };
    let c: Bx<i64> = b;
    return b.v + c.v * 0;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="TYPE-15"></a>
### TYPE-15. A generic type takes the arguments it declares

**TYPE-15@1.** A named type takes exactly as many type arguments and capability arguments as its declaration has type parameters and capability parameters, each of the matching kind, so a type with capability parameters is never named bare.

- Kind: refusal
- Uses: none
- Codes: E0113 (resolve), E0114 (resolve), E0115 (resolve)
- Status: enforced
- Enforced by: Concrete.resolveProgram
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** An extra type argument was once dropped silently, so `Box1<i32, bool>` compiled
and ran (bug 073), and a missing one surfaced later as an unrelated error on the unresolved
parameter. A capability argument says what a handle may do (R-0484), so a bare `Writer` for a
`Writer<cap C>` would leave the authority to inference; it is refused instead. The parser
classifies each argument as a type or a capability by position, and resolution checks the kind
and the count in both directions. A generic type named bare, with no argument list, is refused
too (bug 200).

```con reject name=extra-type-argument code=E0113 stage=resolve pair=declared-type-arguments
struct Copy Box1<T> { v: T }

fn example_value() -> i64 {
    let b: Box1<i64, bool> = Box1::<i64, bool> { v: 42 };
    return b.v;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=declared-type-arguments exit=42
struct Copy Box1<T> { v: T }

fn example_value() -> i64 {
    let b: Box1<i64> = Box1::<i64> { v: 42 };
    return b.v;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con reject name=bare-generic-type code=E0113 stage=resolve pair=declared-type-arguments
struct Copy Box1<T> { v: i64 }

fn example_value() -> i64 {
    let b: Box1 = Box1 { v: 42 };
    return b.v;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=type-where-capability code=E0114 stage=resolve pair=capability-argument
struct Copy Sink<cap C> { f: fn(i64) with(C) -> i64 }

fn take(s: Sink<i64>) -> i64 {
    return 0;
}

fn main() -> u8 {
    return 42;
}
```

```con reject name=capability-arity code=E0115 stage=resolve pair=capability-argument
struct Copy Sink<cap C> { f: fn(i64) with(C) -> i64 }

fn take(s: Sink<{}, {}>) -> i64 {
    return 0;
}

fn main() -> u8 {
    return 42;
}
```

```con accept name=capability-argument exit=42
struct Copy Sink<cap C> { f: fn(i64) with(C) -> i64 }

fn take(s: Sink<{}>) -> i64 {
    discard(s);
    return 0;
}

fn main() -> u8 {
    return 42;
}
```

<a id="TYPE-16"></a>
### TYPE-16. A struct has a finite size

**TYPE-16@1.** A struct does not contain itself by value, directly or through the fields of other structs and enums or of their instantiations, so a recursive type goes through a `Heap` or a raw pointer.

- Kind: refusal
- Uses: none
- Codes: E0583 (core-check)
- Status: enforced
- Enforced by: Concrete.ccCheckModuleDecls
- Semantics: none
- Obligation: every type has a layout (R-0502)
- Evidence: tested

**Rationale.** A value is laid out inline, so a struct that holds itself by value has no finite
size. CoreCheck refuses it before code generation, where it used to surface as an LLVM
"recursive structure type" error. A reference cannot be a field (REF-3), so it does not break a
cycle. A generic instantiation is followed with its type arguments substituted, so `W<Node>`
inside `Node` is refused too (bug 205; it used to reach `llvm-as` while `--interp` ran it).

```con reject name=struct-contains-itself code=E0583 stage=core-check pair=finite-struct
struct Copy Node { v: i64, next: Node }
fn main() -> u8 { return 0; }
```

```con reject name=through-instantiation code=E0583 stage=core-check pair=finite-struct
struct Copy W<T> { v: T }
struct Copy Node { a: i64, w: W<Node> }
fn main() -> u8 { return 42; }
```

```con accept name=finite-struct exit=42
struct Copy Node { v: i64, w: i64 }
fn example_value() -> i64 { let n: Node = Node { v: 40, w: 2 }; return n.v + n.w; }
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="TYPE-17"></a>
### TYPE-17. A function type carries its capability set

**TYPE-17@2.** The capability set of a function type `fn(..) with(C) -> R` is part of the type, compared without regard to the order of its capabilities, so a function value keeps its capabilities through every binding, argument, field and combinator.

- Kind: guarantee
- Uses: TYPE-D1
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.Ty, Concrete.expectTy
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Authority rule 4 is the canonical current rule (OD93 refers to it): function values
keep exact capability sets without widening. A combinator declares its callback's set plus its own
effects; capability arguments come from checked argument types, not result context (OD45), and
duplicate capabilities are refused (OD108). Ambiguous set inference is
[undecided: see the target Core proposal](decisions.md#core-capability-inference).

```con accept name=set-order exit=42
fn inc(x: i64) with(Fs, Network) -> i64 {
    return x + 1;
}

fn example_value() with(Fs, Network) -> i64 {
    let f: fn(i64) with(Network, Fs) -> i64 = inc;
    return f(41);
}

fn main() with(Fs, Network) -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con accept name=combinator-carries-capabilities exit=42
fn both<cap C, cap D>(f: fn(i64) with(C) -> i64, g: fn(i64) with(D) -> i64) with(C, D) -> i64 {
    return f(g(40));
}

fn inc(x: i64) with(Fs) -> i64 {
    return x + 1;
}

fn inc2(x: i64) with(Network) -> i64 {
    return x + 1;
}

fn example_value() with(Fs, Network) -> i64 {
    return both(inc, inc2);
}

fn main() with(Fs, Network) -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con accept name=field-keeps-capabilities exit=42
struct Copy Sink<cap C> { f: fn(i64) with(C) -> i64 }

fn inc(x: i64) with(Fs, Network) -> i64 {
    return x + 1;
}

fn use_it(s: Sink<Network + Fs>) with(Fs, Network) -> i64 {
    let g: fn(i64) with(Network, Fs) -> i64 = s.f;
    return g(41);
}

fn example_value() with(Fs, Network) -> i64 {
    let s: Sink<Fs + Network> = Sink { f: inc };
    return use_it(s);
}

fn main() with(Fs, Network) -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="TYPE-18"></a>
### TYPE-18. Capabilities have no run-time representation

**TYPE-18@3.** A capability set or capability argument has no run-time representation, so types that differ only in their capability sets or capability arguments have the same size and layout, but generic instantiations differing in a function-type argument’s capability set have distinct specialization symbols.

- Kind: guarantee
- Uses: TYPE-D1
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.Layout.tySize, Concrete.Layout.tyAlign, Concrete.Layout.fieldOffset, Concrete.Layout.tyToLLVM
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** A capability is a compile-time permission, checked and then erased; it matters for
handle types (TYPE-15) and for the C layout of a struct passed across FFI. Whether two capability
instantiations of one generic function share generated code is an implementation choice, not a
language rule; OD109 requires distinct specialization symbols when function-type arguments differ only in their capability sets.

```con accept name=same-layout exit=42



struct Copy Sink<cap C> { f: fn(i64) with(C) -> i64, n: i64 }

pub fn main() -> u8 {
    let a: u64 = sizeof::<Sink<{}>>();
    let b: u64 = sizeof::<Sink<Fs + Network>>();
    let c: u64 = sizeof::<fn(i64) -> i64>();
    let d: u64 = sizeof::<fn(i64) with(Fs) -> i64>();
    let e: u64 = alignof::<Sink<{}>>();
    let f: u64 = alignof::<Sink<Fs + Network>>();
    if a == b && c == d && e == f {
        return 42;
    }
    return 1;
}
```



<a id="TYPE-19"></a>
### TYPE-19. The primitive numeric types

**TYPE-19@2.** The integer types are the signed `i8`, `i16`, `i32` and `i64` and unsigned `u8`, `u16`, `u32` and `u64`, each as many bits wide as its name says, the float types are `f32` and `f64`, and builtin types have only their canonical spellings, with `Int`, `Uint`, `Float64`, `Float32`, `Bool` and `Char` refused.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.parseType, Concrete.tyFromName, Concrete.IntArith.intBitWidth
- Semantics: none
- Obligation: one `IntArith` authority for widths and signedness (R-0493)
- Evidence: tested

**Rationale.** The alias decision and OD102 give each primitive exactly one spelling; source examples use canonical names.

```con accept name=two-spellings exit=42
fn wide(x: u64) -> u64 {
    return x;
}

fn half(x: f64) -> f64 {
    return x / 2.0;
}

fn example_value() -> i64 {
    let a: i64 = 40;
    let b: i64 = a;
    let c: u64 = 1;
    let d: u64 = wide(c);
    let e: f64 = half(3.0);
    if e > 1.0 {
        return b + (d as i64) + 1;
    }
    return 0;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con accept name=widths-and-signedness exit=42
fn main() -> u8 {
    let a: i8 = 127;
    let b: u8 = 255;
    let c: i16 = 32767;
    let d: u16 = 65535;
    let e: i32 = 2147483647;
    let f: u32 = 4294967295;
    let g: i64 = 9223372036854775807;
    let h: u64 = 18446744073709551615;
    if wrapping_add(a, 1) == -128 && wrapping_add(b, 1) == 0 && wrapping_add(c, 1) == -32768
        && wrapping_add(d, 1) == 0 && wrapping_add(e, 1) == -2147483648 && wrapping_add(f, 1) == 0
        && wrapping_add(g, 1) < 0 && wrapping_add(h, 1) == 0 {
        return 42;
    }
    return 0;
}
```

<a id="TYPE-20"></a>
### TYPE-20. A `char` is one byte

**TYPE-20@1.** A `char` is a one-byte code unit, so `sizeof::<char>()` is 1 and every `u8` value converts with `as` to a `char` that converts back to the same `u8`.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: partial R-0555
- Enforced by: Concrete.Layout.tySize, Concrete.ccCheckExpr, Concrete.lowerExpr, Concrete.Interp.evalCast
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Strings are bytes ([STRING_TEXT_CONTRACT.md](../stdlib/STRING_TEXT_CONTRACT.md) §2),
and `char` is the type of one of them, which is why ARITH-5 accepts `u8` to `char` and `char` to any
integer of at least 8 unsigned or 16 signed bits. A Unicode-scalar `char` would be a separate
proposal with its own conversions.

```con accept name=char-is-a-byte exit=42


fn example_value() -> i64 {
    let b: u8 = 255;
    let c: char = b as char;
    let d: u8 = c as u8;
    if sizeof::<char>() == 1 { return (d as i64) - 213; }
    return 0;
}

pub fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con accept name=every-byte-round-trips-decided exit=42 pending=R-0555 now=reject:E0101:resolve
fn example_value() -> i64 {
    let mut i: i64 = 0;
    let mut same: i64 = 0;
    while i < 256 {
        let b: u8 = truncate::<u8>(i);
        let c: char = b as char;
        if (c as u8) == b {
            same = same + 1;
        }
        i = i + 1;
    }
    return same - 214;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="TYPE-21"></a>
### TYPE-21. Newtype declarations are refused

**TYPE-21@2.** Newtype declarations are refused; a named struct is the product type and has the layout specified by FFI-7 and FFI-8.

- Kind: refusal
- Uses: none
- Codes: new (parse)
- Status: pending R-0555
- Enforced by: Concrete.Layout.tySize, Concrete.Layout.tyAlign, Concrete.Layout.resolveNewtype, Concrete.Layout.tyToLLVM
- Semantics: none
- Obligation: layout comes from the one `Layout` authority (FFI-2, R-0502)
- Evidence: tested

**Rationale.** OD54 removes newtypes; nominal wrappers use named structs and explicit field destructuring.





```con reject name=newtype-removed code=new stage=parse pair=struct-wrapper pending=R-0555 now=accept:42
newtype N = u8;
fn main() -> u8 { return 42; }
```

```con accept name=struct-wrapper exit=42
struct N { v: u8 }
fn main() -> u8 { let n: N = N { v: 42 }; let N { v } = n; return v; }
```

<a id="TYPE-22"></a>
### TYPE-22. A constant is evaluated where it is used

**TYPE-22@1.** A module constant's initializer is evaluated at each use of the constant, in a scope of its own that sees no local of the using function, and never at program start, so the initializer of a constant that is not used never runs.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.lowerExpr, Concrete.Interp.evalExpr
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** A constant has no storage: the binary inlines its initializer at each use and the
interpreter evaluates it there, so a trap in an initializer (ARITH-3) happens at the use. A constant
is immutable and holds no capability or token (TRUST-8). Absorbed from
[INTERPRETER_TRUST.md](../verification/INTERPRETER_TRUST.md) §2.

```con accept name=unused-constant-never-runs exit=42
const BIG: i32 = 2147483647 + 1;

fn main() -> u8 {
    return 42;
}
```

```con abort name=constant-traps-at-use trap=overflow
const BIG: i32 = 2147483647 + 1;

fn example_value() -> i64 {
    let x: i32 = BIG;
    return x as i64;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

<a id="TYPE-23"></a>
### TYPE-23. The builtin `Option` and `Result`

**TYPE-23@1.** The compiler provides the generic enums `Option<T>`, whose variants are `Some { value: T }` and `None`, and `Result<T, E>`, whose variants are `Ok { value: T }` and `Err { error: E }`.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.builtinOptionEnum, Concrete.builtinResultEnum
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Builtins that can fail, such as `vec_pop`, and std's error conventions return these
two types, so their shape is part of the language rather than of a library module. Both are declared `Copy`, so an instantiation is Copy exactly when its type
arguments are (TYPE-14). OD43 refuses declarations that collide with builtin type names.

```con accept name=option-and-result exit=42
fn example_value() -> i64 {
    let o: Option<i64> = Option::<i64>::Some { value: 40 };
    let r: Result<i64, i64> = Result::<i64, i64>::Err { error: 2 };
    let a: i64 = match o { Option::Some { value } => value, Option::None => 0 };
    let b: i64 = match r { Result::Ok { value } => value, Result::Err { error } => error };
    return a + b;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="TYPE-24"></a>
### TYPE-24. Newtype conversions are refused

**TYPE-24@2.** Newtype constructors, newtype `.0` projections, newtype `as` conversions and the builtin `unwrap` are refused; destructuring uses `match` or an irrefutable pattern.

- Kind: refusal
- Uses: none
- Codes: new (parse)
- Status: pending R-0555
- Enforced by: Concrete.ccCheckExpr
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** OD54, OD123 and OD155 remove the feature and its associated conversions rather than retaining representation-changing aliases.



```con reject name=unwrap-removed code=new stage=parse pair=match-wrapper pending=R-0555 now=accept:42
newtype N = u8;
fn main() -> u8 { let n: N = N(42); return unwrap(n); }
```

```con accept name=match-wrapper exit=42
struct N { v: u8 }
fn main() -> u8 { let n: N = N { v: 42 }; let N { v } = n; return v; }
```

## Builtin functions

This catalog describes decided requirements, not the frozen compiler's builtin table. TYPE-10
checks signatures, TRUST-4 checks permissions, and FFI-6 fixes primitive effects by identity.

| operation | decided contract | governing rules |
|---|---|---|
| `alloc` | allocator plus input and `with(Alloc)`, returning `Result<Heap<T, A>, AllocFailure<T>>` without losing the input on failure | MEM-5, MEM-7 |
| `*h` | consumes `Heap<T, A>`, returns its payload and releases storage using recorded release information | MEM-5 |
| `destroy` | explicit consuming cleanup returning Unit, requiring at most Alloc; never implicit | OWN-13, TRAIT-11 |
| `discard` | eligible Copy values only, never Result or Option | OWN-5, OWN-7 |
| `free`, `unwrap` | refused | OD59, OD155, TYPE-10, TYPE-24 |
| `sizeof`, `alignof` | size and alignment in bytes as `u64` | FFI-7 |
| `read_owned` | explicit raw ownership transfer with trusted premises | TRUST-7 |
| `abort` | terminal trap with no permission or resource requirement | STMT-13 |
| allocating strings and containers | allocator value plus Alloc, conserving fallible result; `Vec<T, A>` and `String<A>` retain allocator identity | MEM-7 |
| filesystem operations | Directory/File resource plus Fs; lookup stays inside the directory and crosses no mount | authority and filesystem decisions |
| network operations | destination/endpoint/resolver grant or socket plus Network; DNS grants no connection | authority and filesystem decisions |
| `get_args` | Environment plus Env and an allocator plus Alloc when allocating | revised OD156 |
| clock operations | Clock grant plus Time | authority decision |

Complete operation contracts, failure payloads and container Destroy applicability are
[undecided: see the target Core proposal](decisions.md#core-allocation-contracts). Static-text
literal typing and conversion are [undecided: see the target Core proposal](decisions.md#core-static-text).
Target service schemas are [undecided: see the target Core proposal](decisions.md#core-grants).

## Absent by decision

This section is explanatory: it records type-system features the language does not have, why, and
what would reopen them. The grammar (SYN-1) is what refuses them.

### No anonymous tuples

Decided 2026-06-22 (ROADMAP old Phase 6 #5, which asked for tuple types or a recorded decision
against them), pinned by `scripts/tests/check_no_tuples.sh`. There are no tuple types `(A, B)`, no
tuple literals `(a, b)` and no positional tuple indexing; each is a parse error. The named struct is
the one product type, and a function that returns two values returns a small `struct Copy`:

```con pseudocode
struct Copy DivMod { quotient: i32, remainder: i32 }
fn divmod(a: i32, b: i32) -> DivMod {
    return DivMod { quotient: a / b, remainder: a % b };
}
```

`t.0` stays the representation projection of a newtype (`Port(u16)` gives `port.0 : u16`), a named
single-field wrapper, not a tuple. The reasons: field names flow into layout reports, obligation
messages, backend contracts and proofs, and `result.remainder` describes itself where `result.1`
does not; anonymous tuples and named structs overlap almost entirely, so carrying both would double
the surface (patterns, layout, code generation, formatting, documentation); the language is
explicit and named everywhere else (no implicit conversions, named capabilities, named fields); and
no workload needs them, so adding them would be symmetry-driven rather than workload-driven. Tuples
would be reconsidered only if a real workload shows named structs insufficient (for example heavy
generic plumbing where naming every intermediate pair is untenable).

### No units of measure

Evaluated and deferred for v1 (ROADMAP old Phase 6 #15). Unit annotations (bytes or bits,
milliseconds or seconds, block counts or byte offsets, protocol lengths, memory sizes) would catch a
real class of systems mistakes, but they are a type-system feature with proof and report
consequences, not a local addition: a unit-tagged integer must extract to the same `PExpr` as its
untagged form, or units become a proof axis; unit erasure must be an audit fact, never silent, so no
proof status may depend on erasing a unit unless a contract or VC names the conversion; and done
poorly they add ceremony for little near-term value. Programs encode the unit in the name instead
(`len_bytes`, `timeout_ms`, `off_in_block`, `size_bits`, per [STYLE.md](../project/STYLE.md)), and today's checked arithmetic
(ARITH-3; OD10, decided 2026-10-29) removes the silent corruption unit
mix-ups often hide behind. If a real
workload keeps hitting unit bugs, the first step is a report-only prototype (`examples/units_probe/`
over the mistake classes above) with optional annotations erased only after the audit records the
conversion: no implicit unit conversion, no dependent numeric refinements.

### No const generics

Designed and deferred (2026-06-21, ROADMAP old Phase 7 #8f). A type is not parameterized by a
compile-time value (`Buf<T, const N: u64>`). The V1 design is fixed so the feature is unambiguous
when a workload pulls it: integer parameters only, arguments that are literals or constants the
existing folder evaluates, one specialization per distinct value with `N` in its mangled name, and
`N` recorded concretely in the ledger, the layout report and every obligation (`index < 16`, never a
symbolic `index < N`); type-level arithmetic and dependent constraints are rejected. The forcing
probe found no workload that needs it, and it is built only when one does. The design, the probe,
the forcing conditions and the staged build are in
[research/language/const-generics-v1-decision.md](../../research/language/const-generics-v1-decision.md).

## Version history

This appendix is not normative. It records what earlier versions of this chapter's entries said
or why they changed, so that each rationale explains the current version only. The registry,
[ids.tsv](ids.tsv), keeps every version's state and sentence hash.

- TYPE-4@2 drops the `break` value from the list of positions a value flows into, since a `break` carries no value (OD27).
- TYPE-6@1 admitted raw-pointer comparison and `p + k` in trusted or `Unsafe` code (bugs 134 and 140). TYPE-6@2 records OD26 (2026-10-29), which removes both, pointer arithmetic becoming `ptr::offset` (TRUST-1).
- TYPE-9@2 states the type-argument count that E0322 already enforced (bug 221: an extra type argument was dropped silently).
- TYPE-14@1 (pending, never enforced) required a `T: Copy` bound instead and was retired by the option (a) decision (TIGHTENING_DECISIONS D6, 2026-10-23).

- TYPE-D1@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; TYPE-D1@1 is superseded by version.
- TYPE-5@3 (2026-11-03) brings the chapter into line with the recorded owner decisions; TYPE-5@2 is superseded by version.
- TYPE-6@3 (2026-11-03) brings the chapter into line with the recorded owner decisions; TYPE-6@2 is superseded by version.
- TYPE-7@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; TYPE-7@1 is superseded by version.
- TYPE-10@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; TYPE-10@1 is superseded by version.
- TYPE-11@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; TYPE-11@1 is superseded by version.
- TYPE-13@3 (2026-11-03) brings the chapter into line with the recorded owner decisions; TYPE-13@2 is superseded by version.
- TYPE-14@3 (2026-11-03) brings the chapter into line with the recorded owner decisions; TYPE-14@2 is superseded by version.
- TYPE-17@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; TYPE-17@1 is superseded by version.
- TYPE-18@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; TYPE-18@1 is superseded by version.
- TYPE-19@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; TYPE-19@1 is superseded by version.
- TYPE-21@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; TYPE-21@1 is superseded by version.
- TYPE-24@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; TYPE-24@1 is superseded by version.

- TYPE-3@2 (2026-11-03) supersedes TYPE-3@1; pending legacy conformance is explicit.
