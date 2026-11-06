# Traits

Impl selection is a function. For a type and a trait there is at most one impl, and for a type and
a method name at most one method, and neither answer depends on the order in which impls, modules
or imports are written. The compiler judges this once, over the whole program, before it builds
any method table (`Concrete.Coherence.checkProgram`). TRAIT-1 to TRAIT-7 are what that judgment
refuses. They were filed as bug 186 ([record](../bugs/186_trait_impl_selection_is_a_search.md)).
TRAIT-8 to TRAIT-10 are the per-declaration checks Check and CoreCheck make after it: an impl
names a declared trait, conforms to its trait's signatures, and every bound a caller instantiates
is satisfied. TRAIT-11 states that destruction cannot fail.

An identity here is never a spelling. A trait is the compiler's builtin `Destroy` or a
declaration (its defining module and name; public traits are exported and explicitly imported). An
impl's type is the declaration its name resolves to at the impl site, through an import alias if
there is one, so `import m.{Foo as Bar}; impl Destroy for Bar` names `m`'s `Foo`.

Locality follows Austral's instance rule (Austral spec, "Instance Uniqueness") at module
granularity: an impl lives with its trait or with its type. Two consequences follow. Every impl of
one trait for one type is in one module, so uniqueness never needs the whole program. And a
module that defines neither cannot add an impl that changes what another module's code calls.

An ambiguous bound call uses `Trait::method(&x, ...)` (completed OD6, TRAIT-6).

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

**Rationale.** Impl lookup uses declaration identity with explicit imports or qualification for child declarations (completed OD43); there is no sibling-submodule fallback.

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

**Revised by the owner 2026-11-05** ([row Q2.8](decisions.md#decided-2026-11-05-revisions)): two impls with equal heads may coexist when Copy/Destroy disjointness (OD28) makes their domains disjoint, one bounded `T: Copy` and the other `T: Destroy`. No other coherence or orphan rule changes, and no disjunctive bound is added.

**Rationale.** Both impls used to enter the method tables under one spelling (`Foo_m`). Native
code then failed in `llvm-as` with a redefined function, and the interpreter refused only when
the call ran. Neither is a language rule; the refusal now happens at the impl.

Decided by the owner on 2026-10-29 (OD6): traits are coherent (this entry to TRAIT-7): one impl per trait and type, the orphan rule (TRAIT-3), an ambiguous method call qualified (TRAIT-6), and resolution by typed identity.

```con reject name=twice code=E0590 stage=check pair=once
trait Tr { fn m(&self) -> i64; }
struct Copy Foo { a: i64 }
impl Tr for Foo { fn m(&self) -> i64 { discard(self); return 42; } }
impl Tr for Foo { fn m(&self) -> i64 { discard(self); return 2; } }
fn call<T: Tr>(x: &T) -> i64 { return x.m(); }
fn example_value() -> i64 { let f: Foo = Foo { a: 0 }; return call::<Foo>(&f); }
pub fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=once exit=42
trait Tr { fn m(&self) -> i64; }
struct Copy Foo { a: i64 }
impl Tr for Foo { fn m(&self) -> i64 { discard(self); return 42; } }
fn call<T: Tr>(x: &T) -> i64 { return x.m(); }
fn example_value() -> i64 { let f: Foo = Foo { a: 0 }; return call::<Foo>(&f); }
pub fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_186_duplicate_impl.con` | `reject:E0590:TRAIT-1@1` | `coherent` | `-` |
| `tests/programs/error_186_duplicate_generic_impl.con` | `reject:E0590:TRAIT-1@1` | `coherent` | `-` |
| `tests/programs/error_186_duplicate_primitive_spelling.con` | `reject:E0590:TRAIT-1@1` | `primitive` | `-` |
| `tests/programs/regress_186_primitive_impl_spellings.con` | `run:42` | `primitive` | `binary,interp` |
| `tests/programs/regress_186_visible_targets.con` | `run:42` | `coherent` | `binary,interp` |
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
trait Tr { fn m(&self) -> i64; }
struct Copy Box2<T> { v: T }
impl<T> Tr for Box2<T> { fn m(&self) -> i64 { discard(self); return 42; } }
impl Tr for Box2<i32> { fn m(&self) -> i64 { discard(self); return 2; } }
fn example_value() -> i64 { let b: Box2<i32> = Box2::<i32> { v: 0 }; return (&b).m(); }
pub fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=generic-only exit=42
trait Tr { fn m(&self) -> i64; }
struct Copy Box2<T> { v: T }
impl<T> Tr for Box2<T> { fn m(&self) -> i64 { discard(self); return 42; } }
fn example_value() -> i64 { let b: Box2<i32> = Box2::<i32> { v: 0 }; return (&b).m(); }
pub fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_186_overlap_generic_impl.con` | `reject:E0591:TRAIT-2@1` | `coherent` | `-` |
| `tests/programs/error_186_overlap_blanket_impl.con` | `reject:E0591:TRAIT-2@1` | `coherent` | `-` |

<a id="TRAIT-3"></a>
### TRAIT-3. An impl lives with its trait or its type

**TRAIT-3@2.** An impl of a trait is declared in the module that declares the trait or in the module that declares its target's type, where the builtin `Destroy` is declared in no module.

- Kind: refusal
- Uses: TRAIT-D1
- Codes: E0592 (check)
- Status: pending R-0555
- Enforced by: Concrete.Coherence.moduleImpls
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** This is Austral's instance locality: local trait for a local type, local trait
for a foreign type, or foreign trait for a local type, and never a foreign trait for a foreign
type. A submodule is its own module. Primitive types (`i64`, `u8`, …) are declared in no module,
so only a local trait can be implemented for them.

```con reject name=foreign-for-foreign-decided code=E0592 stage=check pair=with-its-type-decided pending=R-0555 now=reject:E0108:resolve
// spec-file: main.con
mod defs;
import defs.{Foo, make};
impl Destroy for Foo { fn destroy(self) -> Unit { let Foo { a: _ } = self; } }
pub fn main() -> u8 { let f: Foo = make(); f.destroy(); return 42; }

// spec-file: defs.con
pub struct Foo { pub a: i64 }
pub fn make() -> Foo { return Foo { a: 42 }; }

```

```con accept name=with-its-type-decided exit=42 pending=R-0555 now=reject:E0108:resolve
// spec-file: main.con
mod defs;
import defs.{Foo, make};
pub fn main() -> u8 { let f: Foo = make(); f.destroy(); return 42; }

// spec-file: defs.con
pub struct Foo { pub a: i64 }
pub fn make() -> Foo { return Foo { a: 42 }; }
impl Destroy for Foo { pub fn destroy(self) -> Unit { let Foo { a: _ } = self; } }

```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_186_orphan_impl.con` | `reject:E0592:TRAIT-3@2` | `coherent` | `-` |
| `tests/programs/error_186_orphan_sibling.con` | `reject:E0592:TRAIT-3@2` | `coherent` | `-` |

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
trait Tr { fn m(&self) -> i64; }
struct Copy Box2<T> { v: i64 }
impl Tr for Box2<i32> { fn m(&self) -> i64 { discard(self); return 42; } }
fn call<T: Tr>(x: &T) -> i64 { return x.m(); }
fn example_value() -> i64 { let b: Box2<bool> = Box2::<bool> { v: 0 }; return call::<Box2<bool>>(&b); }
pub fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=generic exit=42
trait Tr { fn m(&self) -> i64; }
struct Copy Box2<T> { v: i64 }
impl<T> Tr for Box2<T> { fn m(&self) -> i64 { discard(self); return 42; } }
fn call<T: Tr>(x: &T) -> i64 { return x.m(); }
fn example_value() -> i64 { let b: Box2<bool> = Box2::<bool> { v: 0 }; return call::<Box2<bool>>(&b); }
pub fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
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
trait A { fn m(&self) -> i64; }
trait B { fn m(&self) -> i64; }
struct Copy Foo { a: i64 }
impl A for Foo { fn m(&self) -> i64 { discard(self); return 40; } }
impl B for Foo { fn m(&self) -> i64 { discard(self); return 2; } }
fn call<T: A>(x: &T) -> i64 { return x.m(); }
fn example_value() -> i64 { let f: Foo = Foo { a: 0 }; return call::<Foo>(&f) + 2; }
pub fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=renamed exit=42
trait A { fn m(&self) -> i64; }
trait B { fn n(&self) -> i64; }
struct Copy Foo { a: i64 }
impl A for Foo { fn m(&self) -> i64 { discard(self); return 40; } }
impl B for Foo { fn n(&self) -> i64 { discard(self); return 2; } }
fn call<T: A + B>(x: &T) -> i64 { return x.m() + x.n(); }
fn example_value() -> i64 { let f: Foo = Foo { a: 0 }; return call::<Foo>(&f); }
pub fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_186_method_clash.con` | `reject:E0594:TRAIT-5@1` | `coherent` | `-` |
| `tests/programs/error_186_inherent_clash.con` | `reject:E0594:TRAIT-5@1` | `coherent` | `-` |

<a id="TRAIT-6"></a>
### TRAIT-6. A bound call has one candidate

**TRAIT-6@2.** A method call on a type parameter with multiple bound traits declaring that method is refused unless written `Trait::method(&x, ...)`, resolved by the trait’s identity without relaxing coherence.

- Kind: refusal
- Uses: none
- Codes: E0595 (check)
- Status: pending R-0555
- Enforced by: Concrete.boundMethodCandidates, Concrete.checkExprCore
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** The canonical OD6 rule is completed by the qualified `Trait::method(&x, ...)` syntax; public traits cross module boundaries by explicit import. Qualification does not weaken coherence.

```con reject name=two-bounds-decided code=E0595 stage=check pair=one-bound pending=R-0555 now=reject:E0595:check
trait A { fn m(&self) -> i64; }
trait B { fn m(&self) -> i64; }
fn call<T: A + B>(x: &T) -> i64 { return x.m(); }
pub fn main() -> u8 { return 42; }
```

```con accept name=one-bound exit=42
trait A { fn m(&self) -> i64; }
trait B { fn m(&self) -> i64; }
fn call<T: A>(x: &T) -> i64 { return x.m(); }
pub fn main() -> u8 { return 42; }
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_186_ambiguous_bound_method.con` | `reject:E0595:TRAIT-6@2` | `coherent` | `-` |

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
trait A { fn m(&self) -> i64; }
trait B { fn n(&self) -> i64; }
struct Copy Foo { a: i64 }
impl A for Foo { fn m(&self) -> i64 { discard(self); return 40; } }
impl B for Foo { fn n(&self) -> i64 { discard(self); return 2; } }
fn call<T: A + B>(x: &T) -> i64 { return x.m() + x.n(); }
fn example_value() -> i64 { let f: Foo = Foo { a: 0 }; return call::<Foo>(&f); }
pub fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con accept name=reversed exit=42
fn call<T: B + A>(x: &T) -> i64 { return x.m() + x.n(); }
impl B for Foo { fn n(&self) -> i64 { discard(self); return 2; } }
impl A for Foo { fn m(&self) -> i64 { discard(self); return 40; } }
struct Copy Foo { a: i64 }
trait B { fn n(&self) -> i64; }
trait A { fn m(&self) -> i64; }
fn example_value() -> i64 { let f: Foo = Foo { a: 0 }; return call::<Foo>(&f); }
pub fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/regress_186_coherent_impls.con` | `run:42` | `coherent` | `binary,interp` |
| `tests/programs/regress_186_coherent_impls_reversed.con` | `run:42` | `coherent` | `binary,interp` |

<a id="TRAIT-8"></a>
### TRAIT-8. An impl names a declared trait

**TRAIT-8@1.** A trait impl names a trait the program declares or the builtin `Destroy`, and no program declares a trait of its own named `Destroy`.

- Kind: refusal
- Uses: none
- Codes: E0579 (core-check), E0580 (core-check)
- Status: enforced
- Enforced by: Concrete.ccCheckModuleDecls
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** `Destroy` is the one builtin trait; the compiler gives its impls their meaning
(the destructor), so a user declaration of the same name would be a second trait that the
destructor machinery takes for the builtin.

```con reject name=unknown-trait code=E0580 stage=core-check pair=declared-trait
struct Copy S { v: i64 }
impl Missing for S { fn m(&self) -> i64 { discard(self); return 1; } }
fn main() -> u8 { return 0; }
```

```con reject name=destroy-redeclared code=E0579 stage=core-check pair=declared-trait
trait Destroy { fn destroy(&self); }
fn main() -> u8 { return 0; }
```

```con accept name=declared-trait exit=42
trait Tr { fn m(&self) -> i64; }
struct Copy S { v: i64 }
impl Tr for S { fn m(&self) -> i64 { return self.v; } }
fn example_value() -> i64 { let s: S = S { v: 42 }; return (&s).m(); }
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="TRAIT-9"></a>
### TRAIT-9. An impl conforms to its trait

**TRAIT-9@2.** A trait impl defines every method of its trait with the trait method's receiver, parameter types and return type (reading `Self` as the impl's type), declares no type or capability parameters the trait method does not, and requires no capability the trait method does not declare, with `Destroy::destroy` requiring the one consuming receiver form `fn destroy(self)`.

- Kind: refusal
- Uses: TRAIT-D1
- Codes: E0581 (core-check), E0582 (core-check), E0586 (core-check), E0587 (core-check)
- Status: pending R-0555
- Enforced by: Concrete.ccCheckModuleDecls
- Semantics: none
- Obligation: a call through a bound is checked against the trait's signature and runs the impl's body (R-0499)
- Evidence: tested

**Rationale.** Generic code that calls a method through a bound is checked against the trait's
signature and then specialized to the impl's body without a second check, so any way an impl's
signature exceeds its trait's would pass something the generic caller never agreed to (bug 091).
Capabilities are the load-bearing case: a trait method with no `with(...)` admits only impls with
none (TRUST-4). OD60 requires the one consuming receiver form `fn destroy(self)`. Its capability bound is OWN-13's (E0584).

```con reject name=missing-method-decided code=E0581 stage=core-check pair=conforming-impl pending=R-0555 now=reject:E0581:core-check
trait Tr { fn m(&self) -> i64; fn n(&self) -> i64; }
struct Copy S { v: i64 }
impl Tr for S { fn m(&self) -> i64 { discard(self); return 1; } }
fn main() -> u8 { return 0; }
```

```con reject name=return-type-differs-decided code=E0582 stage=core-check pair=conforming-impl pending=R-0555 now=reject:E0582:core-check
trait Tr { fn m(&self) -> i64; }
struct Copy S { v: i64 }
impl Tr for S { fn m(&self) -> bool { return true; } }
fn main() -> u8 { return 0; }
```

```con reject name=receiver-differs-decided code=E0587 stage=core-check pair=conforming-impl pending=R-0555 now=reject:E0587:core-check
trait Tr { fn m(&self, n: i64) -> i64; }
struct Copy S { v: i64 }
impl Tr for S { fn m(&mut self, n: i64) -> i64 { return n; } }
fn main() -> u8 { return 0; }
```

```con reject name=capabilities-exceed-trait-decided code=E0586 stage=core-check pair=conforming-impl pending=R-0555 now=reject:E0586:core-check
trait Tr { fn m(&self) -> i64; }
struct Copy S { v: i64 }
impl Tr for S { fn m(&self) with(Fs) -> i64 { return self.v; } }
fn main() -> u8 { return 0; }
```

```con accept name=conforming-impl exit=42
trait Tr { fn m(&self, n: i64) -> i64; fn me(&self) -> Self; }
struct Copy S { v: i64 }
impl Tr for S {
    fn m(&self, n: i64) -> i64 { return self.v + n; }
    fn me(&self) -> S { return S { v: self.v }; }
}
fn example_value() -> i64 { let s: S = S { v: 40 }; let t: S = (&s).me(); return (&t).m(2); }
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="TRAIT-10"></a>
### TRAIT-10. A bound is satisfied by an impl

**TRAIT-10@3.** A type argument given for a type parameter with a bound satisfies every trait in the bound: `Copy` by being Copy, and `Destroy` and any other trait only by an impl of it, so a Copy type is not `Destroy`.

- Kind: refusal
- Uses: TRAIT-D1
- Codes: E0241 (check)
- Status: pending R-0555
- Enforced by: Concrete.checkTraitBounds
- Semantics: none
- Obligation: every bound call has a candidate after specialization (TRAIT-6)
- Evidence: tested

**Rationale.** A bound is a promise the caller makes for the generic body: the body calls the
bound's methods on values of the parameter, and specialization then needs exactly one impl
(TRAIT-6). A missing impl is refused at the call that gives the type argument. A primitive type
argument needs an impl too, matched by its canonical type (`i64` and `i64` are one); before bug
204 it was not checked, and the program ran when the body called no bound method.

The recorded answers in [OD35](decisions.md#od35) govern this subject; the frozen compiler’s earlier behavior is not an alternative rule.


```con reject name=bound-unsatisfied-decided code=E0241 stage=check pair=bound-satisfied pending=R-0555 now=reject:E0241:check
trait Tr { fn m(&self) -> i64; }
struct Copy S { v: i64 }
fn call<T: Tr>(x: &T) -> i64 { return x.m(); }
fn example_value() -> i64 { let s: S = S { v: 42 }; return call::<S>(&s); }
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con reject name=primitive-bound-unsatisfied-decided code=E0241 stage=check pair=bound-satisfied pending=R-0555 now=reject:E0241:check
trait Tr { fn m(&self) -> i64; }
fn call<T: Tr>(x: &T) -> i64 { return x.m(); }
fn example_value() -> i64 { let n: i64 = 42; return call::<i64>(&n); }
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=bound-satisfied exit=42
trait Tr { fn m(&self) -> i64; }
struct Copy S { v: i64 }
impl Tr for S { fn m(&self) -> i64 { return self.v; } }
fn call<T: Tr>(x: &T) -> i64 { return x.m(); }
fn example_value() -> i64 { let s: S = S { v: 42 }; return call::<S>(&s); }
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con reject name=copy-type-for-destroy-bound code=E0241 stage=check pair=unbounded-generic pending=bug-324 now=accept:42
fn id<T: Destroy>(x: T) -> T {
    return x;
}
fn example_value() -> i64 {
    return id(42);
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con accept name=unbounded-generic exit=42
fn id<T>(x: T) -> T {
    return x;
}
fn example_value() -> i64 {
    return id(42);
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="TRAIT-11"></a>
### TRAIT-11. Destruction cannot fail

**TRAIT-11@2.** The method `destroy` of the builtin trait `Destroy` returns `Unit`, so destroying a value reports no failure.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.ccCheckModuleDecls
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Cleanup that can fail can be written as a consuming method that returns its outcome
(a `close(self) -> Result`); that is a library pattern, not part of this rule. A destructor runs from `destroy(x)`, from a deferred call and from a collection's
drop glue, none of which has a place to put an error, so an error there would be silently dropped
(OWN-6). An impl whose `destroy` returns anything else is refused by TRAIT-9 (E0582, `expected
return type ()`). Fallible cleanup, such as closing a file whose final flush can fail, is an
ordinary consuming method (`close(self) -> Result<Unit, E>`) that the caller matches. The receiver
form of `destroy` is the consuming `fn destroy(self)` (OD60), and the capabilities it may require are OWN-13's.

```con accept name=destroy-returns-unit-decided exit=42 pending=R-0555 now=reject:E0108:resolve
struct R { v: i64 }

impl Destroy for R {
    fn destroy(self) -> Unit {
        let R { v: _ } = self;
    }
}

fn main() -> u8 {
    let r: R = R { v: 1 };
    destroy(r);
    return 42;
}
```

## Version history

This appendix is not normative. It records what earlier versions of this chapter's entries said
or why they changed, so that each rationale explains the current version only. The registry,
[ids.tsv](ids.tsv), keeps every version's state and sentence hash.

- TRAIT-10@2 records OD28 (2026-10-29): a `Destroy` bound is satisfied only by an impl.

- TRAIT-6@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; TRAIT-6@1 is superseded by version.

- TRAIT-3@2 (2026-11-03) supersedes TRAIT-3@1; pending legacy conformance is explicit.

- TRAIT-10@3 (2026-11-03) supersedes TRAIT-10@2; pending legacy conformance is explicit.

- TRAIT-11@2 (2026-11-03) supersedes TRAIT-11@1; pending legacy conformance is explicit.
