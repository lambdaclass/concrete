# Traits

Impl selection is a function. For a type and a trait there is at most one impl, and for a type and
a method name at most one method, and neither answer depends on the order in which impls, modules
or imports are written. The compiler judges this once, over the whole program, before it builds
any method table (`Concrete.Coherence.checkProgram`). The rules below are what that judgment
refuses. They were filed as bug 186 ([record](../bugs/186_trait_impl_selection_is_a_search.md)).

An identity here is never a spelling. A trait is the compiler's builtin `Destroy` or a
declaration (its defining module and name; traits are module-local, they cannot be imported). An
impl's type is the declaration its name resolves to at the impl site, through an import alias if
there is one, so `import m.{Foo as Bar}; impl Destroy for Bar` names `m`'s `Foo`.

Locality follows Austral's instance rule (Austral spec, "Instance Uniqueness") at module
granularity: an impl lives with its trait or with its type. Two consequences follow. Every impl of
one trait for one type is in one module, so uniqueness never needs the whole program. And a
module that defines neither cannot add an impl that changes what another module's code calls.

A qualified call (`Trait::method(x)`) does not exist. TRAIT-6 therefore refuses an ambiguous
call instead of asking for qualification; adding the syntax is a follow-up (README §8).

## Definitions

<a id="TRAIT-D1"></a>
### TRAIT-D1. Impl target

**TRAIT-D1@1.** The target of an impl is the type its type name resolves to at the impl site (a type declaration, a compiler enum, or a primitive by its canonical type), applied to the type arguments written after that name, where a name that is one of the impl's own type parameters is a blanket target.

- Kind: definition
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Coherence.resolveHead, Concrete.Coherence.moduleImpls
- Semantics: none
- Obligation: none
- Evidence: by-users

**Rationale.** A name is resolved the way the checker resolves it: the module's own
declarations and aliases, its imports, a sibling submodule's struct or enum, the compiler's
`Option` and `Result`, the always-visible std types, then the primitives, where `i64` and `Int`
are one type. Before bug 186 the parser consumed and dropped the type arguments of an impl's
target, so `impl Tr for Box<i32>` and `impl<T> Tr for Box<T>` were the same impl to every later
pass. The arguments are now kept on the impl (`targetArgs`) so that TRAIT-2 and TRAIT-4 can judge
them.

## Rules

<a id="TRAIT-1"></a>
### TRAIT-1. One impl of a trait per type

**TRAIT-1@1.** No two impls of one trait have targets that are equal up to the naming of their type parameters.

- Kind: refusal
- Uses: TRAIT-D1
- Codes: E0590 (check)
- Status: enforced
- Enforced by: Concrete.Coherence.pairwise
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Both impls used to enter the method tables under one spelling (`Foo_m`). Native
code then failed in `llvm-as` with a redefined function, and the interpreter refused only when
the call ran. Neither is a language rule; the refusal now happens at the impl.

```con reject name=twice code=E0590 stage=check pair=once
trait Tr { fn m(&self) -> Int; }
struct Copy Foo { a: Int }
impl Tr for Foo { fn m(&self) -> Int { return 42; } }
impl Tr for Foo { fn m(&self) -> Int { return 2; } }
fn call<T: Tr>(x: &T) -> Int { return x.m(); }
pub fn main() -> Int { let f: Foo = Foo { a: 0 }; return call::<Foo>(&f); }
```

```con accept name=once exit=42
trait Tr { fn m(&self) -> Int; }
struct Copy Foo { a: Int }
impl Tr for Foo { fn m(&self) -> Int { return 42; } }
fn call<T: Tr>(x: &T) -> Int { return x.m(); }
pub fn main() -> Int { let f: Foo = Foo { a: 0 }; return call::<Foo>(&f); }
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_186_duplicate_impl.con` | `reject:E0590:TRAIT-1@1` | `coherent` | `-` |
| `tests/programs/error_186_duplicate_generic_impl.con` | `reject:E0590:TRAIT-1@1` | `coherent` | `-` |
| `tests/programs/error_186_duplicate_primitive_spelling.con` | `reject:E0590:TRAIT-1@1` | `primitive` | `-` |
| `tests/programs/regress_186_primitive_impl_spellings.con` | `run:42` | `primitive` | `binary,interp` |
| `tests/programs/regress_186_coherent_impls.con` | `run:42` | `coherent` | `binary,interp` |

<a id="TRAIT-2"></a>
### TRAIT-2. Impls of a trait do not overlap

**TRAIT-2@1.** No two impls of one trait have targets that unify when each impl's type parameters are read as variables.

- Kind: refusal
- Uses: TRAIT-D1
- Codes: E0591 (check)
- Status: enforced
- Enforced by: Concrete.Coherence.pairwise, Concrete.Coherence.unify
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** A generic impl and a specialized one for the same type both apply to the
specialized instantiation. Before bug 186 the binary ran whichever body was emitted first. A
blanket impl (`impl<T> Tr for T`) unifies with every other impl of its trait. TRAIT-4 refuses
each of these impls on its own as well; TRAIT-2 names the pair.

```con reject name=generic-and-specialized code=E0591 stage=check pair=generic-only
trait Tr { fn m(&self) -> Int; }
struct Copy Box2<T> { v: T }
impl<T> Tr for Box2<T> { fn m(&self) -> Int { return 42; } }
impl Tr for Box2<i32> { fn m(&self) -> Int { return 2; } }
pub fn main() -> Int { let b: Box2<i32> = Box2::<i32> { v: 0 }; return b.m(); }
```

```con accept name=generic-only exit=42
trait Tr { fn m(&self) -> Int; }
struct Copy Box2<T> { v: T }
impl<T> Tr for Box2<T> { fn m(&self) -> Int { return 42; } }
pub fn main() -> Int { let b: Box2<i32> = Box2::<i32> { v: 0 }; return b.m(); }
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_186_overlap_generic_impl.con` | `reject:E0591:TRAIT-2@1` | `coherent` | `-` |
| `tests/programs/error_186_overlap_blanket_impl.con` | `reject:E0591:TRAIT-2@1` | `coherent` | `-` |

<a id="TRAIT-3"></a>
### TRAIT-3. An impl lives with its trait or its type

**TRAIT-3@1.** An impl of a trait is declared in the module that declares the trait or in the module that declares its target's type, where the builtin `Destroy` is declared in no module.

- Kind: refusal
- Uses: TRAIT-D1
- Codes: E0592 (check)
- Status: enforced
- Enforced by: Concrete.Coherence.moduleImpls
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** This is Austral's instance locality: local trait for a local type, local trait
for a foreign type, or foreign trait for a local type, and never a foreign trait for a foreign
type. A submodule is its own module. Primitive types (`Int`, `u8`, …) are declared in no module,
so only a local trait can be implemented for them.

```con reject name=foreign-for-foreign code=E0592 stage=check pair=with-its-type
mod defs {
    pub struct Foo { pub a: Int }
    pub fn make() -> Foo { return Foo { a: 42 }; }
}
mod main {
    import defs.{Foo, make};
    impl Destroy for Foo { fn destroy(self) { let Foo { a } = self; } }
    pub fn main() -> Int { let f: Foo = make(); f.destroy(); return 42; }
}
```

```con accept name=with-its-type exit=42
mod defs {
    pub struct Foo { pub a: Int }
    pub fn make() -> Foo { return Foo { a: 42 }; }
    impl Destroy for Foo { pub fn destroy(self) { let Foo { a } = self; } }
}
mod main {
    import defs.{Foo, make};
    pub fn main() -> Int { let f: Foo = make(); f.destroy(); return 42; }
}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_186_orphan_impl.con` | `reject:E0592:TRAIT-3@1` | `coherent` | `-` |
| `tests/programs/error_186_orphan_sibling.con` | `reject:E0592:TRAIT-3@1` | `coherent` | `-` |

<a id="TRAIT-4"></a>
### TRAIT-4. An impl covers every instantiation of its type

**TRAIT-4@1.** The target of an impl is a type that resolves to a single declaration, a compiler enum or a primitive, not a type parameter or an alias, applied to exactly the impl's own type parameters (and, for an inherent impl, then its capability parameters) in declaration order, as many as the declaration takes.

- Kind: refusal
- Uses: TRAIT-D1
- Codes: E0593 (check)
- Status: enforced
- Enforced by: Concrete.Coherence.moduleImpls
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Every pass after the parser keys an impl by its type's name alone, so an impl is
in fact an impl for every instantiation. TRAIT-4 makes the written impl say so. Before bug 186,
`impl Tr for Box2<i32>` let a `Box2<bool>` satisfy `T: Tr` and run the `i32` body: silent wrong
code, natively and under `--interp`. Specialized and blanket impls are not supported; adding
them needs targets keyed by more than the type's identity.

```con reject name=specialized code=E0593 stage=check pair=generic
trait Tr { fn m(&self) -> Int; }
struct Copy Box2<T> { v: Int }
impl Tr for Box2<i32> { fn m(&self) -> Int { return 42; } }
fn call<T: Tr>(x: &T) -> Int { return x.m(); }
pub fn main() -> Int { let b: Box2<bool> = Box2::<bool> { v: 0 }; return call::<Box2<bool>>(&b); }
```

```con accept name=generic exit=42
trait Tr { fn m(&self) -> Int; }
struct Copy Box2<T> { v: Int }
impl<T> Tr for Box2<T> { fn m(&self) -> Int { return 42; } }
fn call<T: Tr>(x: &T) -> Int { return x.m(); }
pub fn main() -> Int { let b: Box2<bool> = Box2::<bool> { v: 0 }; return call::<Box2<bool>>(&b); }
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_186_specialized_impl.con` | `reject:E0593:TRAIT-4@1` | `coherent` | `-` |
| `tests/programs/error_186_imported_alias_target.con` | `reject:E0593:TRAIT-4@1` | `coherent` | `-` |
| `tests/programs/error_186_builtin_arity.con` | `reject:E0593:TRAIT-4@1` | `coherent` | `-` |

<a id="TRAIT-5"></a>
### TRAIT-5. One method of a name per type

**TRAIT-5@1.** No two impls whose targets name the same type declaration both declare a method of the same name, whether each impl is inherent or of a trait.

- Kind: refusal
- Uses: TRAIT-D1
- Codes: E0594 (check)
- Status: enforced
- Enforced by: Concrete.Coherence.pairwise
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** A method call on a known type (`x.m()`) has exactly one candidate, so the trait
is a function of the type and the method name. That is also why the compiler's internal method
spelling (`Type_method`) can omit the trait without two methods colliding. Before bug 186 two
traits of one type with a method `m` both became `Foo_m`: `--interp` called the one declared
first and native code failed in `llvm-as`.

```con reject name=two-traits code=E0594 stage=check pair=renamed
trait A { fn m(&self) -> Int; }
trait B { fn m(&self) -> Int; }
struct Copy Foo { a: Int }
impl A for Foo { fn m(&self) -> Int { return 40; } }
impl B for Foo { fn m(&self) -> Int { return 2; } }
fn call<T: A>(x: &T) -> Int { return x.m(); }
pub fn main() -> Int { let f: Foo = Foo { a: 0 }; return call::<Foo>(&f) + 2; }
```

```con accept name=renamed exit=42
trait A { fn m(&self) -> Int; }
trait B { fn n(&self) -> Int; }
struct Copy Foo { a: Int }
impl A for Foo { fn m(&self) -> Int { return 40; } }
impl B for Foo { fn n(&self) -> Int { return 2; } }
fn call<T: A + B>(x: &T) -> Int { return x.m() + x.n(); }
pub fn main() -> Int { let f: Foo = Foo { a: 0 }; return call::<Foo>(&f); }
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_186_method_clash.con` | `reject:E0594:TRAIT-5@1` | `coherent` | `-` |
| `tests/programs/error_186_inherent_clash.con` | `reject:E0594:TRAIT-5@1` | `coherent` | `-` |

<a id="TRAIT-6"></a>
### TRAIT-6. A bound call has one candidate

**TRAIT-6@1.** A method call on a value whose type is a type parameter is refused when more than one of that parameter's bound traits declares the method.

- Kind: refusal
- Uses: none
- Codes: E0595 (check)
- Status: enforced
- Enforced by: Concrete.boundMethodCandidates, Concrete.checkExpr
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** The first bound that declared the method used to win, so `T: A + B` and
`T: B + A` called different traits. TRAIT-5 already makes such a `T` impossible to instantiate,
but the generic function itself must not depend on the order of its bounds either.

```con reject name=two-bounds code=E0595 stage=check pair=one-bound
trait A { fn m(&self) -> Int; }
trait B { fn m(&self) -> Int; }
fn call<T: A + B>(x: &T) -> Int { return x.m(); }
pub fn main() -> Int { return 42; }
```

```con accept name=one-bound exit=42
trait A { fn m(&self) -> Int; }
trait B { fn m(&self) -> Int; }
fn call<T: A>(x: &T) -> Int { return x.m(); }
pub fn main() -> Int { return 42; }
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_186_ambiguous_bound_method.con` | `reject:E0595:TRAIT-6@1` | `coherent` | `-` |

<a id="TRAIT-7"></a>
### TRAIT-7. Impl selection does not depend on order

**TRAIT-7@1.** A program's meaning does not change when its modules, imports, impls or a function's bounds are written in another order.

- Kind: guarantee
- Uses: TRAIT-D1
- Codes: none
- Status: enforced
- Enforced by: Concrete.Coherence.checkProgram
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** TRAIT-1 to TRAIT-6 compare impls by identity and never take the first match, so
the order of declarations reaches no answer. The two fixtures are one program written in two
orders.

```con accept name=forward exit=42
trait A { fn m(&self) -> Int; }
trait B { fn n(&self) -> Int; }
struct Copy Foo { a: Int }
impl A for Foo { fn m(&self) -> Int { return 40; } }
impl B for Foo { fn n(&self) -> Int { return 2; } }
fn call<T: A + B>(x: &T) -> Int { return x.m() + x.n(); }
pub fn main() -> Int { let f: Foo = Foo { a: 0 }; return call::<Foo>(&f); }
```

```con accept name=reversed exit=42
fn call<T: B + A>(x: &T) -> Int { return x.m() + x.n(); }
impl B for Foo { fn n(&self) -> Int { return 2; } }
impl A for Foo { fn m(&self) -> Int { return 40; } }
struct Copy Foo { a: Int }
trait B { fn n(&self) -> Int; }
trait A { fn m(&self) -> Int; }
pub fn main() -> Int { let f: Foo = Foo { a: 0 }; return call::<Foo>(&f); }
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/regress_186_coherent_impls.con` | `run:42` | `coherent` | `binary,interp` |
| `tests/programs/regress_186_coherent_impls_reversed.con` | `run:42` | `coherent` | `binary,interp` |
