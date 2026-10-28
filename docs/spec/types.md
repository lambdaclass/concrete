# Types

Every expression has one type, and there are no implicit conversions. This chapter covers
callable values and dispatch (TYPE-1, TYPE-2), `Text` (TYPE-3), the typing of expressions and
operators (TYPE-4 to TYPE-10), construction rights (TYPE-11), type aliases (TYPE-12) and the
kinds of type parameters (TYPE-13, TYPE-14), generic arguments (TYPE-15) and finite size (TYPE-16). Copy itself is defined in the ownership chapter
(OWN-D1, OWN-14).

The design records are [CONSTRUCTION_RIGHTS.md](../language/CONSTRUCTION_RIGHTS.md),
[VALIDATED_WRAPPERS.md](../language/VALIDATED_WRAPPERS.md) (a library pattern built on
TYPE-11, not a language rule) and [VALUE_MODEL.md](../language/VALUE_MODEL.md).
Const generics are not part of the language. Their design is decided, and the build is
deferred until a workload needs it ([CONST_GENERICS_V1.md](../language/CONST_GENERICS_V1.md)).
Layout attributes (`#[repr(C)]`, `packed`, `align`) belong to the FFI chapter.

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
at the call site ([CALLABLE_VALUES_AND_CAPABILITIES.md](../language/CALLABLE_VALUES_AND_CAPABILITIES.md)).
VERIFIED_PIPELINE's first verified profile excludes closures for the same reason. The type
grammar (`Ty`) has function-pointer types and no closure type.

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
[STRING_TEXT_CONTRACT.md](../language/STRING_TEXT_CONTRACT.md) is the library contract.

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

**TYPE-6@1.** Arithmetic operators take two operands of one numeric type, except that trusted or `Unsafe` code may apply them to a raw pointer and an integer, bitwise and shift operators take integers, unary minus takes a number, and comparisons are defined only on integers, floats, `bool`, `char` and, in `Unsafe` code, raw pointers.

- Kind: refusal
- Uses: none
- Codes: E0228 (check), E0229 (check), E0501 (core-check, post-mono), E0502 (core-check), E0503 (core-check), E0506 (core-check), E0507 (core-check), E0509 (core-check)
- Status: enforced
- Enforced by: Concrete.checkExprCore, Concrete.ccCheckExpr
- Semantics: none
- Obligation: one `IntArith` authority for operand widths (R-0493)
- Evidence: tested

**Rationale.** Mixed widths and signedness are a classic source of silent conversion bugs, so they
are refused rather than promoted. Comparing aggregates would need a definition of equality per
type, which the language does not have. Comparing raw pointers exposes addresses, so it needs
`Unsafe` (bugs 134 and 140). Pointer arithmetic written as `p + k` is the form TRUST-1 replaces
with `ptr::offset` (pending).

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

**TYPE-9@1.** A call passes exactly as many arguments as the callee has parameters, and a method call names a method of the receiver's named type or of a trait that bounds the receiver's type parameter.

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
what lets a generic body be checked once (TYPE-2, PASS-1).

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

```con reject name=append-unsupported-argument code=E0421 stage=check pair=free-heap
fn main() with(Alloc) -> Int {
    let mut out: String = "";
    append(&mut out, 1.5);
    drop_string(out);
    return 0;
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
and it holds in `trusted` callers too. Two follow-on slices are pending
([CONSTRUCTION_RIGHTS.md](../language/CONSTRUCTION_RIGHTS.md)): private-by-default enum
variants, and privacy of a newtype's payload.

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
(`len_bytes`, `timeout_ms`, `off_in_block`, `size_bits`, per [STYLE.md](../project/STYLE.md)), , and today's checked arithmetic
(ARITH-3, on main and provisional until the owner answers OD10) removes the silent corruption unit
mix-ups often hide behind. If a real
workload keeps hitting unit bugs, the first step is a report-only prototype (`examples/units_probe/`
over the mistake classes above) with optional annotations erased only after the audit records the
conversion: no implicit unit conversion, no dependent numeric refinements.

