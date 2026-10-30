# Types

Every expression has one type, and there are no implicit conversions. This chapter covers
callable values and dispatch (TYPE-1, TYPE-2), `Text` (TYPE-3), the typing of expressions and
operators (TYPE-4 to TYPE-10), construction rights (TYPE-11), type aliases (TYPE-12) and the
kinds of type parameters (TYPE-13, TYPE-14), generic arguments (TYPE-15), finite size (TYPE-16),
capability sets in types (TYPE-D1, TYPE-17, TYPE-18), the primitive numeric types (TYPE-19), `char`
(TYPE-20), the representation of a newtype (TYPE-21), the builtin `Option` and `Result` (TYPE-23) and
the conversions of a newtype (TYPE-24). Copy itself is defined in the ownership chapter
(OWN-D1, OWN-14). What a capability means and when a function may use one are in the trusted
chapter (TRUST-D1, TRUST-4).

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

**TYPE-D1@1.** A capability set is a set of capabilities (TRUST-D1) and capability variables, written as a comma-separated list in `with(...)` and as `{}` or names joined by `+` in a capability argument, and a function or a struct declares a capability variable as a parameter `<cap C>`.

- Kind: definition
- Uses: TRUST-D1
- Codes: none
- Status: enforced
- Enforced by: Concrete.CapSet
- Semantics: none
- Obligation: none
- Evidence: by-users

**Rationale.** A set, not a list: order does not matter (whether a repeated capability does is
OPEN-108), and an alias (`Std`, or one
declared `cap IO = File + Console;`) stands for the capabilities it expands to. Several capability
variables and concrete capabilities may be joined, `fn f<cap C, cap D>(..) with(C, D, Console)`.
How a call binds a capability variable from its arguments is OPEN-45; a capability argument in an
expression turbofish is OPEN-87.

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
struct Ctx { add: Int }

fn apply(ctx: &Ctx, x: Int, f: fn(&Ctx, Int) -> Int) -> Int {
    return f(ctx, x);
}

fn plus(c: &Ctx, x: Int) -> Int {
    return c.add + x;
}

fn main() -> Int {
    let c: Ctx = Ctx { add: 40 };
    let r: Int = apply(&c, 2, plus);
    let Ctx { add } = c;
    return r + add * 0;
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
    fn area(&self) -> Int;
}

struct Copy Sq { s: Int }

impl Area for Sq {
    fn area(&self) -> Int { return self.s * self.s; }
}

fn total<T: Area>(x: &T) -> Int {
    return x.area();
}

fn main() -> Int {
    let q: Sq = Sq { s: 6 };
    return total::<Sq>(&q) + 6;
}
```

<a id="TYPE-3"></a>
### TYPE-3. `Text` is owned

**TYPE-3@1.** A `Text` owns an immutable copy of its bytes, never aliases another value's buffer, and is linear.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: std/src/text.con#Text
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** R-0483 (closed 2026-10-09) replaced the borrowed `Text` view with an owned value.
The old `Copy` pointer-and-length pair could outlive the buffer it pointed into, and its UTF-8
validation went stale when the source was mutated. Every constructor now copies, the fields are
private, and there is no `&mut` access, so the property established at construction holds for the
value's whole life. UTF-8 validity is checked by `Text::copy_from_raw`; `Text::from_string`
relies on `String`'s own UTF-8 invariant, which `String::push_char` does not check today (STRING_TEXT_CONTRACT
§6 and §12), so this entry claims ownership, not validity. The borrowed
form of a string is `&String`.
[STRING_TEXT_CONTRACT.md](../stdlib/STRING_TEXT_CONTRACT.md) is the library contract.

```con accept name=outlives-its-source exit=42 project=text_owned interp-exempt=project-needs-std
mod text_owned {
    import std.string.{String};
    import std.text.{Text};
    fn main() with(Alloc) -> Int {
        let mut s: String = String::new();
        s.push_char(42);
        let t: Text = Text::from_string(&s);
        s.drop();
        let mut out: Int = 0;
        if let Option::Some { value } = t.byte(0) {
            out = value as Int;
        }
        t.drop();
        return out;
    }
}
```

<a id="TYPE-4"></a>
### TYPE-4. One type per expression, no implicit conversion

**TYPE-4@1.** Every expression has exactly one type, and every position a value flows into (a binding, an argument, a return, a field, a branch or arm of a value-producing `if` or `match`, a `break` value) expects that type exactly, with no implicit conversion, except that an expression that never completes (a diverging `return`, `break`, `continue` or call) fits any position other than a condition (TYPE-5).

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

```con reject name=bool-for-int code=E0220 stage=check pair=one-type
fn main() -> Int {
    let x: Int = true;
    return x;
}
```

```con reject name=branches-disagree code=E0224 stage=check pair=one-type
fn main() -> Int {
    let c: bool = true;
    let x: Int = if c { 1 } else { false };
    return x;
}
```

```con accept name=one-type exit=42
fn main() -> Int {
    let c: bool = true;
    let x: Int = if c { 42 } else { 0 };
    return x;
}
```

```con reject name=capabilities-not-erased code=E0220 stage=check pair=same-capabilities
fn inc(x: Int) with(Console) -> Int {
    return x + 1;
}

fn main() with(Console) -> Int {
    let f: fn(Int) -> Int = inc;
    return f(41);
}
```

```con reject name=capability-lost-in-argument code=E0220 stage=check pair=same-capabilities
fn apply(f: fn(Int) -> Int, x: Int) -> Int {
    return f(x);
}

fn inc(x: Int) with(Console) -> Int {
    return x + 1;
}

fn main() with(Console) -> Int {
    return apply(inc, 41);
}
```

```con reject name=capability-lost-in-field code=E0220 stage=check pair=same-capabilities
struct Copy Box { f: fn(Int) -> Int }

fn inc(x: Int) with(Console) -> Int {
    return x + 1;
}

fn main() with(Console) -> Int {
    let b: Box = Box { f: inc };
    let g: fn(Int) -> Int = b.f;
    return g(41);
}
```

```con accept name=same-capabilities exit=42
fn inc(x: Int) with(Console) -> Int {
    return x + 1;
}

fn main() with(Console) -> Int {
    let f: fn(Int) with(Console) -> Int = inc;
    return f(41);
}
```

<a id="TYPE-5"></a>
### TYPE-5. Conditions are `bool`

**TYPE-5@1.** The condition of an `if`, `while` or `for`, a match guard, and the operands of `&&`, `||` and `!` have type `bool`, and a comparison has type `bool`.

- Kind: refusal
- Uses: none
- Codes: E0223 (check), E0504 (core-check), E0505 (core-check), E0508 (core-check), E0540 (core-check), E0541 (core-check)
- Status: enforced
- Enforced by: Concrete.checkExprCore, Concrete.checkStmt, Concrete.ccCheckExpr, Concrete.ccCheckStmt
- Semantics: none
- Obligation: progress and preservation for the Core type system (R-0507)
- Evidence: tested

**Rationale.** There is no truthiness: an integer is not a condition. Until bug 192 was fixed, the
statement forms of `if`, `while` and `for` let an integer through, and the binary ran it with C
truthiness while `--interp` refused it. This entry's example found the bug.

```con reject name=int-condition code=E0223 stage=check pair=bool-condition
fn main() -> Int {
    let n: Int = 1;
    if n {
        return 42;
    }
    return 0;
}
```

```con accept name=bool-condition exit=42
fn main() -> Int {
    let n: Int = 1;
    if n != 0 {
        return 42;
    }
    return 0;
}
```

<a id="TYPE-6"></a>
### TYPE-6. Operators take operands of one numeric type

**TYPE-6@2.** Arithmetic operators take two operands of one numeric type, bitwise and shift operators take integers, unary minus takes a number, comparisons take numbers, and no operator takes a raw pointer; whether comparisons also take `bool` and `char` is OPEN-42.

- Kind: refusal
- Uses: none
- Codes: E0228 (check), E0229 (check), E0501 (core-check, post-mono), E0502 (core-check), E0503 (core-check), E0506 (core-check), E0507 (core-check), E0509 (core-check)
- Status: partial bug-322
- Enforced by: Concrete.checkExprCore, Concrete.ccCheckExpr
- Semantics: none
- Obligation: one `IntArith` authority for operand widths (R-0493)
- Evidence: tested

**Rationale.** Mixed widths and signedness are a classic source of silent conversion bugs, so they
are refused rather than promoted. Comparing aggregates would need a definition of equality per
type, which the language does not have. Version 1 admitted raw-pointer comparison
and `p + k` in trusted or `Unsafe` code (bugs 134 and 140); OD26 removes both, pointer arithmetic
becoming `ptr::offset` (TRUST-1).

Version 2 records the owner's decision of 2026-10-29 (OD26), aligned with TRUST-1: arithmetic,
shifts and comparisons take numbers only, and pointer offset, difference and ordering are
`ptr::offset` and named functions. Whether "numbers only" also removes comparisons on `bool` and
`char` is OPEN-42; main compares them today, and this entry does not decide it. Main still admits pointer arithmetic and comparison in trusted or
`Unsafe` code (bug 322).


```con reject name=compare-structs code=E0229 stage=check pair=same-type-operands
struct Copy P { n: Int }

fn main() -> Int {
    let a: P = P { n: 1 };
    let b: P = P { n: 1 };
    if a == b {
        return 42;
    }
    return 0;
}
```

```con accept name=same-type-operands exit=42
struct Copy P { n: Int }

fn main() -> Int {
    let a: P = P { n: 21 };
    let b: P = P { n: 21 };
    if a.n == b.n {
        return a.n + b.n;
    }
    return 0;
}
```

```con reject name=pointer-comparison code=new stage=check pair=pointee-comparison pending=bug-322 now=accept:42
trusted fn same(p: *const Int, q: *const Int) -> bool {
    return p == q;
}
fn main() -> Int {
    let x: Int = 1;
    let p: *const Int = &x as *const Int;
    if same(p, p) { return 42; }
    return 0;
}
```

```con accept name=pointee-comparison exit=42
trusted fn same(p: *const Int, q: *const Int) -> bool {
    return *p == *q;
}
fn main() -> Int {
    let x: Int = 1;
    let p: *const Int = &x as *const Int;
    if same(p, p) { return 42; }
    return 0;
}
```

<a id="TYPE-7"></a>
### TYPE-7. Aggregates are built and read by their declarations

**TYPE-7@1.** A struct literal names a declared struct and gives each of its fields exactly once (directly or, for the fields it does not name, from a `..base`), a field access names a field of a struct, an enum literal or pattern names a variant of a declared enum, an array literal has at least one element, and only an array is indexed, by an integer.

- Kind: refusal
- Uses: none
- Codes: E0250 (check), E0251 (check), E0252 (check), E0253 (check), E0254 (check), E0260 (check), E0261 (check), E0306 (check), E0416 (elab), E0550 (core-check), E0551 (core-check), E0552 (core-check), E0316 (check)
- Status: enforced
- Enforced by: Concrete.checkExprCore, Concrete.checkStmt, Concrete.ccCheckExpr
- Semantics: none
- Obligation: progress and preservation for the Core type system (R-0507)
- Evidence: tested

**Rationale.** A missing field would leave part of a value uninitialized, and a field given
twice would make the literal ambiguous. A destructuring `let` binds every field (`field: _`
ignores a Copy one, OWN-5). An empty array literal has no element type to infer. A field given
twice used to be accepted, with the checker reading the first occurrence and the binary storing
the last (bug 193).

```con reject name=missing-field code=E0252 stage=check pair=every-field
struct Copy P { a: Int, b: Int }

fn main() -> Int {
    let p: P = P { a: 42 };
    return p.a;
}
```

```con reject name=unknown-field code=E0253 stage=check pair=every-field
struct Copy P { a: Int, b: Int }

fn main() -> Int {
    let p: P = P { a: 42, b: 0, c: 1 };
    return p.a;
}
```

```con reject name=duplicate-field code=E0306 stage=check pair=every-field
struct Copy P { a: Int, b: Int }

fn main() -> Int {
    let p: P = P { a: 1, b: 2, a: 40 };
    return p.a + p.b;
}
```

```con reject name=newtype-field-other-than-zero code=E0316 stage=check pair=every-field
newtype Meters = Int;

fn main() -> Int {
    let m: Meters = Meters(42);
    return m.1;
}
```

```con accept name=every-field exit=42
struct Copy P { a: Int, b: Int }

fn main() -> Int {
    let p: P = P { a: 40, b: 2 };
    let arr: [Int; 2] = [p.a, p.b];
    return arr[0] + arr[1];
}
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
fn main() -> Int {
    let x: Int = 42;
    return *x;
}
```

```con accept name=deref-ref exit=42
fn read(r: &Int) -> Int {
    return *r;
}

fn main() -> Int {
    let x: Int = 42;
    return read(&x);
}
```

<a id="TYPE-9"></a>
### TYPE-9. Calls match their signatures

**TYPE-9@2.** A call passes exactly as many arguments as the callee has parameters and, when it writes a type-argument list, exactly as many type arguments as the callee has type parameters, and a method call names a method of the receiver's named type or of a trait that bounds the receiver's type parameter.

- Kind: refusal
- Uses: none
- Codes: E0262 (check), E0264 (check), E0265 (check), E0266 (check), E0522 (core-check), E0313 (check), E0317 (check), E0322 (check)
- Status: partial bug-333
- Enforced by: Concrete.checkExprCore
- Semantics: none
- Obligation: progress and preservation for the Core type system (R-0507)
- Evidence: tested

**Rationale.** There are no default arguments, no variadic functions and no method lookup
through implicit conversions. A method of a type parameter comes only from its bounds, which is
what lets a generic body be checked once (TYPE-2, PASS-1). Version 2 states the type-argument count E0322 already enforced (bug 221: an extra type
argument was dropped silently). A body-less declaration (an `#[intrinsic]`) keeps no type-parameter
list, so main still skips the count there (bug 333), pinned by the pending example.

```con reject name=wrong-arity code=E0262 stage=check pair=right-arity
fn add(a: Int, b: Int) -> Int {
    return a + b;
}

fn main() -> Int {
    return add(42);
}
```

```con reject name=newtype-constructor-arity code=E0313 stage=check pair=right-arity
newtype Meters = Int;

fn main() -> Int {
    let m: Meters = Meters(40, 2);
    return m.0;
}
```

```con reject name=receiverless-trait-method code=E0317 stage=check pair=right-arity
trait Make { fn make() -> Int; }
fn call<T: Make>(x: &T) -> Int { return x.make(); }
fn main() -> Int { return 0; }
```

```con accept name=right-arity exit=42
fn add(a: Int, b: Int) -> Int {
    return a + b;
}

fn main() -> Int {
    return add(40, 2);
}
```

```con reject name=type-arguments-to-bodyless-declaration code=E0322 stage=check pair=type-argument-to-bodyless-declaration pending=bug-333 now=accept:42 now-interp=fail:sizeof
mod foo {
    #[intrinsic = "sizeof"]
    pub trusted fn sizeof<T>() -> u64;
}
mod bar {
    import foo.{sizeof};
    pub fn main() -> u64 {
        return sizeof::<u8, u32>() + 41;
    }
}
```

```con accept name=type-argument-to-bodyless-declaration exit=42
mod foo {
    #[intrinsic = "sizeof"]
    pub trusted fn sizeof<T>() -> u64;
}
mod bar {
    import foo.{sizeof};
    pub fn main() -> u64 {
        return sizeof::<u8>() + 41;
    }
}
```

<a id="TYPE-10"></a>
### TYPE-10. Builtins have fixed signatures

**TYPE-10@1.** A builtin function is called with the argument count, type-argument count and argument types of its fixed signature, and `free` takes a `Heap`.

- Kind: refusal
- Uses: none
- Codes: E0268 (check), E0269 (check), E0270 (check), E0271 (check), E0274 (check), E0421 (check, elab), E0314 (check), E0315 (check)
- Status: enforced
- Enforced by: Concrete.checkExprCore
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Builtins (`alloc`, `free`, `destroy`, the collection intrinsics) are checked by
their own signatures rather than by a declaration, so each misuse has its own diagnostic.
`destroy` of a value whose type has no `Destroy` implementation is an ownership rule (OWN-13).
The output builtins are the one exception to a fixed argument count: `print` and `println` take
one or more arguments and `append` a `&mut String` and one or more arguments, each argument a
`String`, `&String`, `&mut String`, `Int`, `Uint`, `i8`, `i16`, `i32`, `u8`, `u16`, `u32`, `bool` or `char`, anything else
being E0421; they return `()`, `println` ends with a newline, and the calls they expand into need
`Console` (print) and `Alloc` (append) under TRUST-4. Whether a program may declare its own
function of one of these names is OPEN-133.

```con reject name=append-unsupported-argument code=E0421 stage=check pair=free-heap
fn main() with(Alloc) -> Int {
    let mut out: String = "";
    append(&mut out, 1.5);
    drop_string(out);
    return 0;
}
```

```con reject name=println-unsupported-argument code=E0421 stage=check pair=println-supported-arguments
struct Copy S { x: Int }

fn main() with(Console) -> Int {
    let s: S = S { x: 1 };
    println(s);
    return 42;
}
```

```con accept name=println-supported-arguments exit=42
fn main() with(Console) -> Int {
    let n: u8 = 7;
    println("n = ", n, ' ', true);
    return 42;
}
```

```con reject name=free-non-heap code=E0274 stage=check pair=free-heap
fn main() with(Std) -> Int {
    let x: Int = 42;
    free(x);
    return 0;
}
```

```con reject name=wrapping-on-non-integers code=E0314 stage=check pair=free-heap
fn main() -> Int {
    let b: bool = wrapping_add(true, false);
    return 0;
}
```

```con reject name=print-without-arguments code=E0315 stage=check pair=free-heap
fn main() with(Std) -> Int {
    print();
    return 0;
}
```

```con accept name=free-heap exit=42
struct Copy B { v: Int }

fn main() with(Std) -> Int {
    let h: Heap<B> = alloc(B { v: 42 });
    let out: Int = h.v;
    free(h);
    return out;
}
```

<a id="TYPE-11"></a>
### TYPE-11. A module keeps its types' representation

**TYPE-11@1.** Outside the module that declares it, a newtype is not constructed directly, and a struct field that is not marked `pub` is not named in a struct literal, read or written.

- Kind: refusal
- Uses: none
- Codes: E0296 (check), E0297 (check), E0298 (check)
- Status: partial R-0500
- Enforced by: Concrete.checkExprCore, Concrete.checkStmt
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Smart constructors are only as strong as the construction rules. Without privacy,
safe code could write `NonZeroU32(0)` and bypass `try_new`. `pub` on a type exports its name,
not its representation. Privacy protects invariants. It is not secrecy and not proof evidence,
and it holds in `trusted` callers too. Two follow-on slices are pending (R-0011): private-by-default
enum variants, and privacy of a newtype's payload.

Verified 2026-07-18: safe code in another module could write `NonZeroU32(0)`, bypassing
`try_new`'s zero check, and read and write another module's struct fields (`b.len`,
`b.len = 999`); pointer-bearing fields were protected only by accident (the integer-to-pointer
cast needs `Unsafe`). Every invariant a library type kept, the kernel-proved `NonZeroU32`
non-zero property included, was advisory; representation privacy makes them load-bearing. `pub`
is the only visibility word: there is no `private`, `sealed`, `opaque`, `transparent` or
friend-module tier, and a deliberately transparent wrapper exposes public construction and
access functions. Visibility is per module, Concrete's unit of encapsulation. There is no std
exemption: std uses constructors, accessors, scoped mutation and audited raw-parts functions like
any other module. Enforcement is the same in Resolve, Check, Elab, project mode, the interpreter and
the compiled path (Check refuses; later stages inherit).

The owner decided on 2026-10-29 (OD21) that only the declaring module has these rights: a parent
module gets none over its child's declarations. Until bug 311's fix a parent still had them over
the declarations it imported from its children (bug 262's record); the `parent-*` examples below
pin the refusal.

| operation | same module | other module |
|---|---|---|
| name the type (`let x: T`, a generic argument) | yes | if the type is `pub` |
| call a `pub fn` or method on it | yes | yes |
| struct literal `T { f: ... }` | yes | only if every field is `pub` |
| field read `x.f`, field write `x.f = ...` | yes | only if `f` is `pub` |
| functional update `T { ..x, f: ... }` | yes | only if every named or required field is `pub` |
| destructuring `let T { f } = x` | yes | only if every bound field is `pub` |
| newtype construction `N(v)` | yes | no: use the module's constructor function |
| newtype unwrap to the inner value | yes | no, once the follow-on lands (R-0011) |
| construct or match an enum variant | yes | once the follow-on lands, only a `pub` variant (R-0011); names are matchable meanwhile |

The newtype pattern is `pub newtype NonZeroU32 = u32` with `pub fn try_new(v: u32) ->
Option<NonZeroU32>` and `pub fn get(&self) -> u32`; `NonZeroU32(v)` is private to `numeric.con`.
Under the enum follow-on, `pub enum T` exports the type name and a variant is constructible and
matchable outside only when marked `pub` (`pub Ok { value: T }`), the marker governing the whole
payload; a client that cannot see every variant matches with a wildcard. Slices 1 (newtype
construction) and 2 (struct literal, read and write: E0297, E0298; std migrated to accessors and
`from_raw_unchecked`) are implemented (defect queue 0b); gate `scripts/tests/check_construction_rights.sh`.

```con reject name=construct-foreign-newtype code=E0296 stage=check pair=foreign-constructor
mod num {
    pub newtype NonZero = Int;
    pub fn try_new(v: Int) -> Option<NonZero> {
        if v == 0 {
            return Option::None;
        }
        return Option::Some { value: NonZero(v) };
    }
}
mod m {
    import num.{NonZero};
    pub fn main() -> Int {
        let n: NonZero = NonZero(0);
        return 0;
    }
}
```

```con reject name=read-private-field code=E0298 stage=check pair=foreign-constructor
mod box {
    pub struct Copy Bx { v: Int }
    pub fn mk() -> Bx {
        return Bx { v: 42 };
    }
}
mod m {
    import box.{Bx, mk};
    pub fn main() -> Int {
        let b: Bx = mk();
        return b.v;
    }
}
```

```con reject name=destructure-private-field code=E0298 stage=check pair=foreign-constructor
mod box {
    pub struct Copy Bx { v: Int }
    pub fn mk() -> Bx {
        return Bx { v: 42 };
    }
}
mod m {
    import box.{Bx, mk};
    pub fn main() -> Int {
        let b: Bx = mk();
        let Bx { v } = b;
        return v;
    }
}
```

```con accept name=foreign-constructor exit=42
mod num {
    pub newtype NonZero = Int;
    pub fn try_new(v: Int) -> Option<NonZero> {
        if v == 0 {
            return Option::None;
        }
        return Option::Some { value: NonZero(v) };
    }
    pub fn get(n: NonZero) -> Int {
        return n.0;
    }
}
mod m {
    import num.{NonZero, try_new, get};
    pub fn main() -> Int {
        let mut out: Int = 0;
        match try_new(42) {
            Option::Some { value } => { out = get(value); },
            Option::None => {}
        }
        return out;
    }
}
```

```con reject name=parent-constructs-child-newtype code=E0296 stage=check pair=foreign-constructor
mod outer {
    mod kids {
        pub newtype K = Int;
        pub fn get(k: K) -> Int {
            return k.0;
        }
    }
    import kids.{K, get};
    pub fn main() -> Int {
        let k: K = K(42);
        return get(k);
    }
}
```

```con reject name=parent-reads-child-private-field code=E0298 stage=check pair=foreign-constructor
mod outer {
    mod kids {
        pub struct Copy Box { v: Int }
        pub fn mk() -> Box {
            return Box { v: 42 };
        }
    }
    import kids.{Box, mk};
    pub fn main() -> Int {
        let b: Box = mk();
        return b.v;
    }
}
```

```con reject name=foreign-newtype-unwrap code=new stage=check pair=foreign-constructor pending=R-0500 now=accept:42
mod num {
    pub newtype NonZero = Int;
    pub fn try_new(v: Int) -> Option<NonZero> {
        if v == 0 {
            return Option::None;
        }
        return Option::Some { value: NonZero(v) };
    }
}
mod m {
    import num.{NonZero, try_new};
    pub fn main() -> Int {
        let mut out: Int = 0;
        match try_new(42) {
            Option::Some { value } => { out = value.0; },
            Option::None => {}
        }
        return out;
    }
}
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
mod m {
    type Id = i32;
    fn want_underlying(x: i32) -> i32 { return x; }
    fn want_alias(x: Id) -> Id { return x; }
    fn main() -> Int {
        let a: Id = 5;
        let b: i32 = a;
        let c: Id = want_underlying(b);
        let d: i32 = want_alias(7);
        return (c + d) as Int;
    }
}
```

<a id="TYPE-13"></a>
### TYPE-13. A bare type parameter is linear-capable

**TYPE-13@2.** A type parameter without a bound may be instantiated at any type, linear ones included, so a generic body uses each value of that parameter exactly once, and `T: Copy` is the only bound that lets a body duplicate one.

- Kind: guarantee
- Uses: OWN-D1
- Codes: none
- Status: enforced
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
struct R { v: Int }

fn pass<T>(x: T) -> T {
    return x;
}

fn twice<T: Copy>(x: T, f: fn(T) -> Int) -> Int {
    return f(x) + f(x);
}

fn half(n: Int) -> Int {
    return n / 2;
}

fn main() -> Int {
    let r: R = pass::<R>(R { v: 0 });
    let R { v } = r;
    return twice::<Int>(42, half) + v;
}
```

<a id="TYPE-14"></a>
### TYPE-14. Copy is declared, never derived

**TYPE-14@2.** Whether an instantiation of a struct or enum is Copy is decided from its declaration alone, so it is Copy exactly when the struct or enum is declared `Copy` and every type argument is Copy.

- Kind: guarantee
- Uses: OWN-D1
- Codes: none
- Status: enforced
- Enforced by: Concrete.Layout.isCopyTyGeneric
- Semantics: none
- Obligation: Copy is decided from the declaration and the type arguments alone, with no fixpoint over the type graph (R-0510)
- Evidence: tested

**Rationale.** Decided 2026-10-23 (TIGHTENING_DECISIONS D6, option (a)) and enforced the same
day. Copy used to be derived from an instantiation's fields, which Mono computed as a fixpoint
over the type graph; the fixpoint is gone. A generic declared `Copy` behaves like a derived
`Copy`: `struct Copy Bx<T> { v: T }` is accepted, its declaration checked once with `T` taken
as Copy (OWN-14), and `Bx<Int>` is Copy while `Bx<String>` is linear. A parameter no field
mentions still counts. A generic not declared `Copy` is never Copy. Version 1 (pending, never
enforced) required a `T: Copy` bound instead and was retired by the option (a) decision.

Decided by the owner on 2026-10-29 (OD5): Copy for a generic struct or enum is declared and checked, never derived, and a generic declared Copy is Copy when every argument is.

```con accept name=copy-over-bare-parameter-at-copy exit=42
struct Copy Bx<T> { v: T }

fn main() -> Int {
    let b: Bx<Int> = Bx::<Int> { v: 21 };
    let c: Bx<Int> = b;
    return b.v + c.v;
}
```

```con accept name=copy-over-copy-parameter exit=42
struct Copy Bx<T: Copy> { v: T }

fn main() -> Int {
    let b: Bx<Int> = Bx::<Int> { v: 42 };
    let c: Bx<Int> = b;
    return b.v + c.v * 0;
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

fn main() -> Int {
    let b: Box1<Int, bool> = Box1::<Int, bool> { v: 42 };
    return b.v;
}
```

```con accept name=declared-type-arguments exit=42
struct Copy Box1<T> { v: T }

fn main() -> Int {
    let b: Box1<Int> = Box1::<Int> { v: 42 };
    return b.v;
}
```

```con reject name=bare-generic-type code=E0113 stage=resolve pair=declared-type-arguments
struct Copy Box1<T> { v: Int }

fn main() -> Int {
    let b: Box1 = Box1 { v: 42 };
    return b.v;
}
```

```con reject name=type-where-capability code=E0114 stage=resolve pair=capability-argument
struct Copy Sink<cap C> { f: fn(Int) with(C) -> Int }

fn take(s: Sink<Int>) -> Int {
    return 0;
}

fn main() -> Int {
    return 42;
}
```

```con reject name=capability-arity code=E0115 stage=resolve pair=capability-argument
struct Copy Sink<cap C> { f: fn(Int) with(C) -> Int }

fn take(s: Sink<{}, {}>) -> Int {
    return 0;
}

fn main() -> Int {
    return 42;
}
```

```con accept name=capability-argument exit=42
struct Copy Sink<cap C> { f: fn(Int) with(C) -> Int }

fn take(s: Sink<{}>) -> Int {
    return 0;
}

fn main() -> Int {
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
struct Copy Node { v: Int, next: Node }
fn main() -> Int { return 0; }
```

```con reject name=through-instantiation code=E0583 stage=core-check pair=finite-struct
struct Copy W<T> { v: T }
struct Copy Node { a: Int, w: W<Node> }
fn main() -> Int { return 42; }
```

```con accept name=finite-struct exit=42
struct Copy Node { v: Int, w: Int }
fn main() -> Int { let n: Node = Node { v: 40, w: 2 }; return n.v + n.w; }
```

<a id="TYPE-17"></a>
### TYPE-17. A function type carries its capability set

**TYPE-17@1.** The capability set of a function type `fn(..) with(C) -> R` is part of the type, compared without regard to the order of its capabilities, so a function value keeps its capabilities through every binding, argument, field and combinator.

- Kind: guarantee
- Uses: TYPE-D1
- Codes: none
- Status: enforced
- Enforced by: Concrete.Ty, Concrete.expectTy
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** A callback that silently lost the capabilities of the code it runs would hide an
effect (TRUST-4's smuggling case, `adversarial_neg_cap_fnptr_smuggle.con`). Since the set is part
of the type, TYPE-4 refuses a `with(Console)` function where a capability-free function type is
expected (E0220, its examples `capabilities-not-erased`, `capability-lost-in-argument` and
`capability-lost-in-field`), and a combinator that calls its callback
must hold the callback's set, which a capability parameter names: the combinator carries what its
callback carries, no more and no less. A handle is a struct with capability parameters (TYPE-15)
whose fields are such function types, so `Sink<{}>` is not a `Sink<Console>` either (TYPE-4).
Whether a function value converts to a type with a larger set (`fn(Int) -> Int` to
`fn(Int) with(Console) -> Int`, or `with(File)` to `with(Std)`) is OPEN-93; main refuses it
(E0220). A combinator spelling that infers its set from its callbacks (`with(f)`) is OPEN-92.
Main compares two sets after sorting them and keeps a repeated capability, so
`fn(Int) with(Console, Console) -> Int` is not a `fn(Int) with(Console) -> Int` (E0220); whether it
should be is OPEN-108.

```con accept name=set-order exit=42
fn inc(x: Int) with(Console, File) -> Int {
    return x + 1;
}

fn main() with(Console, File) -> Int {
    let f: fn(Int) with(File, Console) -> Int = inc;
    return f(41);
}
```

```con accept name=combinator-carries-capabilities exit=42
fn both<cap C, cap D>(f: fn(Int) with(C) -> Int, g: fn(Int) with(D) -> Int) with(C, D) -> Int {
    return f(g(40));
}

fn inc(x: Int) with(Console) -> Int {
    return x + 1;
}

fn inc2(x: Int) with(File) -> Int {
    return x + 1;
}

fn main() with(Console, File) -> Int {
    return both(inc, inc2);
}
```

```con accept name=field-keeps-capabilities exit=42
struct Copy Sink<cap C> { f: fn(Int) with(C) -> Int }

fn inc(x: Int) with(Console, File) -> Int {
    return x + 1;
}

fn use_it(s: Sink<File + Console>) with(Console, File) -> Int {
    let g: fn(Int) with(File, Console) -> Int = s.f;
    return g(41);
}

fn main() with(Console, File) -> Int {
    let s: Sink<Console + File> = Sink { f: inc };
    return use_it(s);
}
```

<a id="TYPE-18"></a>
### TYPE-18. Capabilities have no run-time representation

**TYPE-18@1.** A capability set or capability argument has no run-time representation, so types that differ only in their capability sets or capability arguments have the same size and layout.

- Kind: guarantee
- Uses: TYPE-D1
- Codes: none
- Status: enforced
- Enforced by: Concrete.Layout.tySize, Concrete.Layout.tyAlign, Concrete.Layout.fieldOffset, Concrete.Layout.tyToLLVM
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** A capability is a compile-time permission, checked and then erased; it matters for
handle types (TYPE-15) and for the C layout of a struct passed across FFI. Whether two capability
instantiations of one generic function share generated code is an implementation choice, not a
language rule; whether two instantiations that differ only in a function type's capability set are
distinct specializations with distinct symbols is OPEN-109 (main makes them one).

```con accept name=same-layout exit=42
mod m {
    #[intrinsic = "sizeof"]
    pub trusted fn sizeof<T>() -> u64;
    #[intrinsic = "alignof"]
    pub trusted fn alignof<T>() -> u64;

    struct Copy Sink<cap C> { f: fn(Int) with(C) -> Int, n: Int }

    pub fn main() -> Int {
        let a: u64 = sizeof::<Sink<{}>>();
        let b: u64 = sizeof::<Sink<Console + File>>();
        let c: u64 = sizeof::<fn(Int) -> Int>();
        let d: u64 = sizeof::<fn(Int) with(Console) -> Int>();
        let e: u64 = alignof::<Sink<{}>>();
        let f: u64 = alignof::<Sink<Console + File>>();
        if a == b && c == d && e == f {
            return 42;
        }
        return 1;
    }
}
```

```con accept name=same-field-offsets exit=42 interp-exempt=byte-reads-not-modelled
mod m {
    #[intrinsic = "sizeof"]
    pub trusted fn sizeof<T>() -> u64;

    struct Copy Sink<cap C> { f: fn(Int) with(C) -> Int, n: u8 }

    fn inc(x: Int) with(Console) -> Int {
        return x + 1;
    }

    fn dec(x: Int) -> Int {
        return x - 1;
    }

    trusted fn byte_at<T>(s: &T, i: u64) -> u8 {
        let p: *const u8 = s as *const T as *const u8;
        let q: *const u8 = p + (i as Int);
        return *q;
    }

    pub fn main() -> Int {
        let a: Sink<{}> = Sink { f: dec, n: 40 };
        let b: Sink<Console> = Sink { f: inc, n: 2 };
        let at: u64 = sizeof::<fn(Int) -> Int>();
        return (byte_at(&a, at) as Int) + (byte_at(&b, at) as Int);
    }
}
```

<a id="TYPE-19"></a>
### TYPE-19. The primitive numeric types

**TYPE-19@1.** The integer types are the signed `i8`, `i16`, `i32` and `i64` and the unsigned `u8`, `u16`, `u32` and `u64`, each as many bits wide as its name says, the float types are `f32` and `f64`, and `Int`, `Uint` and `Float64` are other spellings of `i64`, `u64` and `f64`.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.parseType, Concrete.tyFromName, Concrete.IntArith.intBitWidth
- Semantics: none
- Obligation: one `IntArith` authority for widths and signedness (R-0493)
- Evidence: tested

**Rationale.** A type written `Int` and one written `i64` are the same type, so either may flow into
a position that expects the other without a conversion (TYPE-4). The parser maps each spelling to
one type. Whether `Float32` is a spelling of `f32` is OPEN-102: the parser does not map it (nor
`Char`), so on main `Float32` in a type position is a different type from `f32`.

```con accept name=two-spellings exit=42
fn wide(x: Uint) -> u64 {
    return x;
}

fn half(x: Float64) -> f64 {
    return x / 2.0;
}

fn main() -> Int {
    let a: i64 = 40;
    let b: Int = a;
    let c: u64 = 1;
    let d: Uint = wide(c);
    let e: f64 = half(3.0);
    if e > 1.0 {
        return b + (d as Int) + 1;
    }
    return 0;
}
```

```con accept name=widths-and-signedness exit=42
fn main() -> Int {
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
- Status: enforced
- Enforced by: Concrete.Layout.tySize, Concrete.ccCheckExpr, Concrete.lowerExpr, Concrete.Interp.evalCast
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Strings are bytes ([STRING_TEXT_CONTRACT.md](../stdlib/STRING_TEXT_CONTRACT.md) §2),
and `char` is the type of one of them, which is why ARITH-5 accepts `u8` to `char` and `char` to any
integer of at least 8 unsigned or 16 signed bits. A Unicode-scalar `char` would be a separate
proposal with its own conversions.

```con accept name=char-is-a-byte exit=42
mod m {
    #[intrinsic = "sizeof"]
    pub trusted fn sizeof<T>() -> u64;

    pub fn main() -> Int {
        let b: u8 = 255;
        let c: char = b as char;
        let d: u8 = c as u8;
        return (sizeof::<char>() as Int) + (d as Int) - 214;
    }
}
```

```con accept name=every-byte-round-trips exit=42
fn main() -> Int {
    let mut i: Int = 0;
    let mut same: Int = 0;
    while i < 256 {
        let b: u8 = i as u8;
        let c: char = b as char;
        if (c as u8) == b {
            same = same + 1;
        }
        i = i + 1;
    }
    return same - 214;
}
```

<a id="TYPE-21"></a>
### TYPE-21. A newtype has its inner type's representation

**TYPE-21@1.** A newtype has the size, alignment and representation of its inner type.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Layout.tySize, Concrete.Layout.tyAlign, Concrete.Layout.resolveNewtype, Concrete.Layout.tyToLLVM
- Semantics: none
- Obligation: layout comes from the one `Layout` authority (FFI-2, R-0502)
- Evidence: tested

**Rationale.** A newtype is a distinct type with no implicit conversion to its inner type (TYPE-4),
and that distinction costs nothing at run time: wrapping (`N(v)`) and unwrapping (`n.0`) move no
bits, and a struct field of the newtype is laid out as a field of the inner type. Whether newtypes
stay in the language is OPEN-54.

```con accept name=newtype-is-erased exit=42
mod m {
    #[intrinsic = "sizeof"]
    pub trusted fn sizeof<T>() -> u64;
    #[intrinsic = "alignof"]
    pub trusted fn alignof<T>() -> u64;

    newtype Meters = Int;
    newtype Small = u8;
    struct Copy W { a: u8, m: Meters }
    struct Copy V { a: u8, m: Int }

    pub fn main() -> Int {
        if sizeof::<Meters>() == sizeof::<Int>() && alignof::<Small>() == alignof::<u8>()
            && sizeof::<W>() == sizeof::<V>() {
            return 42;
        }
        return 0;
    }
}
```

```con accept name=wrap-and-unwrap exit=42
newtype Meters = Int;

struct Copy W { a: u8, m: Meters }

fn grow(m: Meters) -> Meters {
    return Meters(m.0 + 1);
}

fn main() -> Int {
    let w: W = W { a: 1, m: Meters(40) };
    let m: Meters = grow(w.m);
    return m.0 + (w.a as Int);
}
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

fn main() -> Int {
    return 42;
}
```

```con abort name=constant-traps-at-use trap=overflow
const BIG: i32 = 2147483647 + 1;

fn main() -> Int {
    let x: i32 = BIG;
    return x as Int;
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
arguments are (TYPE-14). How a user declaration named `Option` or `Result` resolves against them is
OPEN-43: main withholds the builtin `Result` from a module that declares or imports its own, and
never withholds `Option`. Their layout is that of any enum (FFI-7).

```con accept name=option-and-result exit=42
fn main() -> Int {
    let o: Option<Int> = Option::<Int>::Some { value: 40 };
    let r: Result<Int, Int> = Result::<Int, Int>::Err { error: 2 };
    let a: Int = match o { Option::Some { value } => value, Option::None => 0 };
    let b: Int = match r { Result::Ok { value } => value, Result::Err { error } => error };
    return a + b;
}
```

<a id="TYPE-24"></a>
### TYPE-24. The conversions of a newtype

**TYPE-24@1.** A newtype `N` over `T` converts to `T` by `n.0` or `n as T` and a `T` converts to `N` by `N(v)` or `v as N`, TYPE-11 saying in which modules `N(v)` and `n.0` may be written, with `N`'s type arguments substituted into `T`, and `as` converts a newtype to or from no other type.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.ccCheckExpr
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Each conversion moves no bits (TYPE-21). A cast between a newtype and any type other
than its own inner type, one wrapping step at a time, is refused with ARITH-5's E0553, so `as` cannot
turn a validated wrapper into an unrelated type or build one from one (bug fixed 2026-04-25,
`error_newtype_cast_to_unrelated.con`). Outside the declaring module, `N(v)` is refused (TYPE-11),
while `n.0` and `unwrap(n)` are refused only once R-0500 lands; whether `v as N` and `n as T` are
refused there too is OPEN-123, and main accepts both. Whether newtypes stay is OPEN-54. Main also
has an intrinsic `unwrap(n)`: Check gives it type `T`, but Core keeps it typed `N`, so `unwrap(k) as
Int` is refused (E0553) and `j.0 + unwrap(k)` stops natively at the SSA verifier (E0715), a defect
found while absorbing [VALIDATED_WRAPPERS.md](../stdlib/VALIDATED_WRAPPERS.md), which calls it a
fallback and prefers `.0`; this entry states `.0` only.

```con accept name=newtype-conversions exit=42
newtype Id = u8;

fn main() -> Int {
    let b: u8 = 40;
    let i: Id = b as Id;
    let j: Id = Id(1);
    let a: u8 = i as u8;
    return (a + j.0 + 1) as Int;
}
```

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
