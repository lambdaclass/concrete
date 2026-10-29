# References

References in Concrete are **second-class**. A reference can be a parameter, a call argument,
a borrow-block binder, or a pattern binder scoped to one arm through a borrowed scrutinee. It is
never returned, stored, bound by `let` or assigned. Every reference is a root binding plus a
sequence of projections, and the root binding outlives the reference lexically. No
lifetime analysis is needed to show this, which is what makes the rules checkable by a small
lexical argument (INV-REF, the proof target of R-0500 and V1).

This chapter states the rules. For the design record behind them (probes, status history, gap
analysis G1 to G19), see [MEMORY_SEMANTICS §15](../language/MEMORY_SEMANTICS.md). For the
decisions of 2026-10-23 (D1 to D4), see
[TIGHTENING_DECISIONS §4](../project/TIGHTENING_DECISIONS.md#4-decisions-of-2026-10-23). Those decisions enter this
chapter as pending examples and entries until they are implemented. Raw pointers
(`*const T`, `*mut T`) are not references, and these rules do not cover them. The trusted
boundary is in [ffi.md](ffi.md).

## Definitions

<a id="REF-D1"></a>
### REF-D1. Contains a reference

**REF-D1@1.** A type contains a reference when a reference type (`&T` or `&mut T`, also when it is spelled through a type alias) occurs anywhere in it, including inside raw pointers, `Heap`, arrays, generic arguments and the parameters and result of a function type.

- Kind: definition
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.tyContainsRef
- Semantics: none
- Obligation: none
- Evidence: by-users

**Rationale.** This is the wide predicate. The rules about what may be returned use it (REF-1,
REF-2, REF-4), because a reference must not leave a frame by any route, including behind a
raw pointer or inside a callback's type.

<a id="REF-D2"></a>
### REF-D2. Holds a reference

**REF-D2@1.** A type holds a reference when it is a reference type or an array, `Heap`, heap array or generic instantiation one of whose element or argument types holds a reference, where a function-pointer type and a raw pointer are not looked through.

- Kind: definition
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.tyStoresRef, Concrete.tyHoldsRefInAggregate
- Semantics: none
- Obligation: none
- Evidence: by-users

**Rationale.** This is the narrow predicate. The storage rules use it (REF-3, REF-4, REF-7). A
`fn(&T) -> U` value stores no reference, so a struct may hold a callback that takes a
reference. A named struct or enum can hold a reference only through its type arguments,
because REF-3 forbids reference fields. For a binding, the outer reference is looked through
(`tyHoldsRefInAggregate`): `& &T` and `&[Int; 2]` hold no reference in an aggregate.

## Rules

<a id="REF-1"></a>
### REF-1. No function returns a reference

**REF-1@1.** No function, method or trait method, `trusted` ones included, has a return type that contains a reference.

- Kind: refusal
- Uses: REF-D1
- Codes: E0236 (check, post-mono)
- Status: enforced
- Enforced by: Concrete.checkFn, Concrete.verifyNoReturnedRefs, Concrete.checkModule
- Semantics: none
- Obligation: INV-REF (R-0500; V1, R-0507)
- Evidence: tested

**Rationale.** A returned reference would outlive the frame that made it. The escape for
low-level code is a raw pointer to the referent, returned from trusted code. A trait method
*declaration* is checked like a definition (gap G13, closed 2026-10-23). An `extern fn` whose
parameter or result contains a reference, also behind a raw pointer, is refused at the C boundary
by FFI-3 (gap G15, closed 2026-10-23). A generic reached at a
reference-returning instantiation only through another generic is refused after
monomorphization by the same judgment (gap G2).

Decided by the owner on 2026-10-29 (OD1): references are second-class, never returned (this entry) or stored (REF-3).

```con reject name=return-param code=E0236 stage=check pair=return-value
fn id(x: &Int) -> &Int {
    return x;
}

fn main() -> Int {
    let v: Int = 42;
    return 0;
}
```

```con accept name=return-value exit=42
fn get(x: &Int) -> Int {
    return *x;
}

fn main() -> Int {
    let v: Int = 42;
    return get(&v);
}
```

```con reject name=trait-declaration code=E0236 stage=check pair=return-value
trait Get {
    fn get(&self) -> &Int;
}

fn main() -> Int {
    return 42;
}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_escape_return.con` | `reject:E0236:REF-1@1` | `ref1` | `-` |
| `tests/programs/error_2i_ref1_return_bare.con` | `reject:E0236:REF-1@1` | `ref1` | `-` |
| `tests/programs/error_2i_ref1_return_option.con` | `reject:E0236:REF-1@1` | `ref1` | `-` |
| `tests/programs/error_2i_ref1_return_fnptr_ref_param.con` | `reject:E0236:REF-1@1` | `ref1` | `-` |
| `tests/programs/regress_2i_ref1_return_value.con` | `run:42` | `ref1` | `binary,interp` |
| `tests/programs/error_2i_ref1_trait_decl_returns_ref.con` | `reject:E0236:REF-1@1` | `ref1g13` | `-` |
| `tests/programs/regress_2i_ref1_trait_decl_value.con` | `run:42` | `ref1g13` | `binary,interp` |
| `tests/programs/error_2i_ref1_transitive_generic.con` | `reject:E0236:REF-1@1` | `ref1g2` | `json-exempt:post-mono` |
| `tests/programs/regress_2i_ref1_transitive_generic_value.con` | `run:42` | `ref1g2` | `binary,interp` |

<a id="REF-2"></a>
### REF-2. No function type returns a reference

**REF-2@1.** No function type, wherever it is written, has a return type that contains a reference.

- Kind: refusal
- Uses: REF-D1
- Codes: E0236 (check)
- Status: enforced
- Enforced by: Concrete.resolveType, Concrete.checkModule
- Semantics: none
- Obligation: INV-REF (R-0500)
- Evidence: tested

**Rationale.** A callback that could return a reference could hand a borrowed element back
out of a scoped accessor such as `with_value`. Parameters of reference type are allowed. The
type is refused wherever it is written, also through a type alias, as a field type and in a
trait method signature (gap G16, closed 2026-10-23), although no value of it could be made in
safe code, because every function that could inhabit it is refused by REF-1.

Decided by the owner on 2026-10-29 (OD14): function types that return a reference are refused everywhere, and one structural well-formedness judgment runs before specialization (R-0499).

```con reject name=param-fn-type code=E0236 stage=check pair=callback-takes-ref
fn apply(x: &Int, f: fn(&Int) -> &Int) -> Int {
    return 0;
}

fn main() -> Int {
    return 0;
}
```

```con accept name=callback-takes-ref exit=42
fn apply(x: &Int, f: fn(&Int) -> Int) -> Int {
    return f(x);
}

fn read(r: &Int) -> Int {
    return *r;
}

fn main() -> Int {
    let v: Int = 42;
    return apply(&v, read);
}
```

```con reject name=through-alias code=E0236 stage=check pair=callback-takes-ref
type Cb = fn(&Int) -> &Int;

fn apply(x: &Int, f: Cb) -> Int {
    return 0;
}

fn main() -> Int {
    return 0;
}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_2i_ref2_fn_type_returns_ref.con` | `reject:E0236:REF-2@1` | `ref2` | `-` |
| `tests/programs/regress_2i_ref2_callback_takes_ref.con` | `run:42` | `ref2` | `binary,interp` |
| `tests/programs/error_2i_ref2_alias_fn_type_returns_ref.con` | `reject:E0236:REF-2@1` | `ref2g16` | `-` |
| `tests/programs/error_2i_ref2_unused_alias_fn_type_returns_ref.con` | `reject:E0236:REF-2@1` | `ref2g16` | `-` |
| `tests/programs/error_2i_ref2_field_fn_type_returns_ref.con` | `reject:E0236:REF-2@1` | `ref2g16` | `-` |
| `tests/programs/regress_2i_ref2_callback_field.con` | `run:42` | `ref2g16` | `binary,interp` |
| `tests/programs/regress_2i_ref2_trait_generic_shadows_alias.con` | `run:42` | `ref2g16` | `binary,interp` |

<a id="REF-3"></a>
### REF-3. No reference is stored

**REF-3@1.** No struct or enum field, newtype, struct or enum literal payload, array element, binding, parameter or constant has a type that holds a reference anywhere except as the bare outer type of a binding or parameter.

- Kind: refusal
- Uses: REF-D2
- Codes: E0239 (check, post-mono)
- Status: enforced
- Enforced by: Concrete.checkModule, Concrete.verifyNoStoredRefs
- Semantics: none
- Obligation: INV-REF (R-0500)
- Evidence: tested

**Rationale.** The only storage a reference occupies is a reference binding's own slot, or an
argument slot during a call. A reference in storage would outlive the lexical scope of its
root. Constants are values and never places, so a constant of reference type is refused as
well.

```con reject name=struct-field code=E0239 stage=check pair=fn-pointer-field
struct W { t: &Int }

fn main() -> Int {
    return 0;
}
```

```con accept name=fn-pointer-field exit=42
struct H { f: fn(&Int) -> Int }

fn g(r: &Int) -> Int {
    return *r + 1;
}

fn main() -> Int {
    let h: H = H { f: g };
    let H { f } = h;
    let x: Int = 41;
    return f(&x);
}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_092_ref_struct_field.con` | `reject:E0239:REF-3@1` | `ref3` | `-` |
| `tests/programs/error_092_ref_in_option.con` | `reject:E0239:REF-3@1` | `ref3` | `-` |
| `tests/programs/error_092_ref_in_array.con` | `reject:E0239:REF-3@1` | `ref3` | `-` |
| `tests/programs/error_092_newtype_ref.con` | `reject:E0239:REF-3@1` | `ref3` | `-` |
| `tests/programs/error_092_alias_newtype_ref.con` | `reject:E0239:REF-3@1` | `ref3` | `-` |
| `tests/programs/error_092_alias_struct_field_ref.con` | `reject:E0239:REF-3@1` | `ref3` | `-` |
| `tests/programs/error_092_const_ref_array.con` | `reject:E0239:REF-3@1` | `ref3` | `-` |
| `tests/programs/error_escape_field.con` | `reject:E0239:REF-3@1` | `ref3` | `-` |
| `tests/programs/error_123_const_ref_type.con` | `reject:E0239:REF-3@1` | `ref3` | `-` |
| `tests/programs/regress_2i_ref3_fnptr_field.con` | `run:42` | `ref3` | `binary,interp` |
| `tests/programs/regress_092_refs_flow_down.con` | `run:42` | `ref3-heap` | `binary,interp-exempt:no-heap-allocation-in-the-interpreter` |

<a id="REF-4"></a>
### REF-4. Generic instantiation preserves REF-1 and REF-3

**REF-4@1.** A type parameter is not instantiated at a type that contains a reference when it occurs in the generic's return type or behind a reference or pointer in one of its parameter types (inside a function type's parameters too), and no instantiation makes a field, binding, parameter or constant hold a reference in an aggregate.

- Kind: refusal
- Uses: REF-D1, REF-D2
- Codes: E0236 (check), E0239 (post-mono), E0238 (check, mono)
- Status: enforced
- Enforced by: Concrete.refuseNestedRefInstantiation, Concrete.refuseNestedRefSpecialization, Concrete.verifyNoStoredRefs
- Semantics: none
- Obligation: INV-REF (R-0500); substitution preserves the rules (PASS-1)
- Evidence: tested

**Rationale.** A generic body is checked once, at its opaque type parameter. Instantiating that
parameter at a reference type must not create a returned or stored reference that the check of
the body never saw. A top-level `x: T` parameter and a by-value callback `fn(T) -> U` are not
nested positions, so `ignore::<&Int>(&v, k)` and `apply::<&Int>(&v, read)` with
`f: fn(T) -> Int` are allowed, while `f: fn(&mut T)` puts `T` behind a reference and is refused.

```con reject name=generic-return code=E0236 stage=check pair=top-level-ref-arg
fn wrap<R>(r: R) -> R {
    return r;
}

fn main() -> Int {
    let x: Int = 42;
    let y: Int = *wrap::<&Int>(&x);
    return y;
}
```

```con accept name=top-level-ref-arg exit=42
fn ignore<T: Copy>(x: T, k: Int) -> Int {
    return k;
}

fn apply<T>(x: T, f: fn(T) -> Int) -> Int {
    return f(x);
}

fn read(r: &Int) -> Int {
    return *r;
}

fn main() -> Int {
    let v: Int = 21;
    return ignore::<&Int>(&v, 21) + apply::<&Int>(&v, read);
}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_2i_ref4_generic_return.con` | `reject:E0236:REF-4@1` | `ref4` | `-` |
| `tests/programs/error_092_generic_box_ref.con` | `reject:E0239:REF-4@1` | `ref4` | `json-exempt:post-mono` |
| `tests/programs/error_092_generic_array_ref.con` | `reject:E0239:REF-4@1` | `ref4` | `json-exempt:post-mono` |
| `tests/programs/error_091_norm_generic_fn_mut_param.con` | `reject:E0238:REF-4@1` | `ref4` | `-` |
| `tests/programs/error_091_norm_generic_transitive_ref.con` | `reject:E0238:REF-4@1` | `ref4` | `json-exempt:mono` |
| `tests/programs/regress_2i_ref4_top_level_ref_arg.con` | `run:42` | `ref4` | `binary,interp` |

<a id="REF-5"></a>
### REF-5. A reference is a root binding plus projections

**REF-5@1.** In safe code, a reference is formed only by borrowing a local binding (`&x`, `&mut x`), by a shared borrow of a string literal, or by projecting a field, index or newtype through a reference, where `&s.f` abbreviates the projection `f` of `&s`, so no temporary, constant, call result, branch value or reference read out of storage is ever a reference's root.

- Kind: refusal
- Uses: none
- Codes: E0238 (check)
- Status: enforced
- Enforced by: Concrete.normPlace, Concrete.refuseRefValuedBranch, Concrete.checkRefArg, Concrete.recordRefProvenance, Concrete.refuseTemporaryBorrow
- Semantics: none
- Obligation: INV-REF (R-0500): every reference has a binding root
- Evidence: tested

**Rationale.** Decided 2026-10-23 (TIGHTENING_DECISIONS D2). The core follows Austral, whose
borrows take only a variable and whose references to parts are paths that start in a reference.
The core language has two operations that form a reference: borrowing a variable, and
projecting a field, index or newtype through a reference. Concrete has no `->` (removed
2026-07-14). Projection through a reference `r` is written `&r.f` or `&r[i]`, and a field read
through it is written `r.f`, which auto-dereferences one layer. `&s.f`, `&a[i]` and `&s.inner.x`
are Concrete's own surface sugar, which Austral does not have. Before checking, they are
rewritten to "borrow the root variable, then project", and they add no semantic case and no
typing or loan rule of their own. A string literal is a static, immutable root, its `String`
descriptor included: `&"text"` is allowed, and `&mut "text"`, moving out of a literal and
destroying one are not. A raw-pointer dereference is neither of the two operations: in trusted
or `Unsafe` code, `&*p` forms a reference only under that code's validity premise (TRUST-3). Borrowing a temporary was gap G12 (bug 182: a borrowed call result
miscompiled or leaked), closed 2026-10-23 at every reference-forming site, explicit borrows and
implicit `&self` receivers alike. Loans are keyed by the root (REF-11).

Decided by the owner on 2026-10-29 (OD1, the Austral model): a reference is formed by borrowing a variable or projecting through a reference, `&s.f` is sugar, and a temporary is never a root. Loans are keyed by the root variable (REF-8, REF-11; OD15 part (a), decided 2026-10-29).

```con reject name=borrow-operator code=E0238 stage=check pair=root-plus-projections
fn read(r: &Int) -> Int {
    return *r;
}

fn main() -> Int {
    return read(&(40 + 2));
}
```

```con accept name=root-plus-projections exit=42
struct P { n: Int }
struct W { p: P }

fn get(r: &P) -> Int {
    return r.n;
}

fn read(r: &Int) -> Int {
    return *r;
}

fn through(r: &W) -> Int {
    return get(&r.p) + read(&r.p.n);
}

fn main() -> Int {
    let w: W = W { p: P { n: 10 } };
    let a: [Int; 2] = [0, 12];
    let out: Int = get(&w.p) + read(&a[1]) + through(&w);
    let W { p } = w;
    let P { n } = p;
    return out + n * 0;
}
```

```con reject name=borrow-int-literal code=E0238 stage=check pair=borrow-let-first
fn read(r: &Int) -> Int {
    return *r;
}

fn main() -> Int {
    return read(&42);
}
```

```con reject name=borrow-call-result code=E0238 stage=check pair=borrow-let-first
fn mk() -> Int {
    return 42;
}

fn read(r: &Int) -> Int {
    return *r;
}

fn main() -> Int {
    return read(&mk());
}
```

```con accept name=borrow-let-first exit=42
fn mk() -> Int {
    return 42;
}

fn read(r: &Int) -> Int {
    return *r;
}

fn main() -> Int {
    let v: Int = mk();
    return read(&v);
}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_105_ref_valued_if_arg.con` | `reject:E0238:REF-5@1` | `ref5` | `-` |
| `tests/programs/error_105_ref_valued_if_field.con` | `reject:E0238:REF-5@1` | `ref5` | `-` |
| `tests/programs/error_091_norm_temporaries.con` | `reject:E0238:REF-5@1` | `ref5` | `-` |
| `tests/programs/error_091_norm_if_local_binding.con` | `reject:E0238:REF-5@1` | `ref5` | `-` |
| `tests/programs/error_091_norm_double_deref_read.con` | `reject:E0238:REF-5@1` | `ref5` | `-` |
| `tests/programs/error_091_norm_nested_ref_binder.con` | `reject:E0238:REF-5@1` | `ref5` | `-` |
| `tests/programs/error_2i_ref5_borrow_temporary.con` | `reject:E0238:REF-5@1` | `ref5` | `-` |
| `tests/programs/error_123_const_borrow_in_body.con` | `reject:E0238:REF-5@1` | `ref5` | `-` |
| `tests/programs/regress_2i_ref5_canonical_places.con` | `run:42` | `ref5` | `binary,interp` |
| `tests/programs/error_091_norm_if_projection.con` | `reject:E0238:REF-5@1` | `ref5-branch` | `-` |
| `tests/programs/regress_2i_ref5_bind_branch_first.con` | `run:42` | `ref5-branch` | `binary,interp` |
| `tests/programs/regress_2i_ref5_heap_field_borrow.con` | `run:42` | `ref5-heap` | `binary,interp-exempt:no-heap-allocation-in-the-interpreter` |
| `tests/programs/error_2i_ref5_const_ref_through_raw_ptr.con` | `reject:E0238:REF-5@1` | `ref5` | `-` |
| `tests/programs/error_d2_borrow_literal.con` | `reject:E0238:REF-5@1` | `ref5d2` | `-` |
| `tests/programs/error_d2_borrow_field_of_temporary.con` | `reject:E0238:REF-5@1` | `ref5d2` | `-` |
| `tests/programs/error_d2_temporary_receiver.con` | `reject:E0238:REF-5@1` | `ref5d2` | `-` |
| `tests/programs/error_d2_mut_borrow_string_literal.con` | `reject:E0238:REF-5@1` | `ref5d2` | `-` |
| `tests/programs/error_d2_branch_receiver.con` | `reject:E0238:REF-5@1` | `ref5d2` | `-` |
| `tests/regressions/bug182_borrowed_call_result_temporary/repro.con` | `reject:E0238:REF-5@1` | `ref5d2` | `-` |
| `tests/regressions/bug182_borrowed_call_result_temporary/repro_linear_leak.con` | `reject:E0238:REF-5@1` | `ref5d2` | `-` |
| `tests/programs/regress_d2_roots_and_projections.con` | `run:42` | `ref5d2` | `binary,interp` |

<a id="REF-6"></a>
### REF-6. Reference bindings are never introduced by `let` and never assigned

**REF-6@1.** No `let` binds a reference, and no reference binding (a parameter, a borrow-block binder or an arm-scoped pattern binder) is ever the target of an assignment.

- Kind: refusal
- Uses: none
- Codes: E0302 (check, post-mono), E0303 (check, post-mono), E0234 (check), E0238 (check)
- Status: enforced
- Enforced by: Concrete.checkStmt, Concrete.verifyNoRefBindingsPostMono
- Semantics: none
- Obligation: INV-REF (R-0500): a reference binding's source is fixed at its introduction
- Evidence: tested

**Rationale.** With both halves, a reference binding's source is fixed when the binding is
introduced. Every form that introduces one is lexically enclosed by the scope of its source, so
the flow-sensitive half of the rules disappears. The rewrite is to borrow at each use
(`f(&w.p); g(&w.p);`) or to borrow the owner in a borrow block. Writing *through* a reference
(`*r = v`) assigns the referenced place, not the binding (REF-7).

```con reject name=let-binds-ref code=E0302 stage=check pair=borrow-each-use
struct P { n: Int }

fn get(r: &P) -> Int {
    return r.n;
}

fn main() -> Int {
    let p: P = P { n: 21 };
    let r: &P = &p;
    let out: Int = get(r) + get(r);
    let P { n } = p;
    return out + n * 0;
}
```

```con reject name=param-assigned code=E0303 stage=check pair=borrow-each-use
fn f(r: &Int, q: &Int) -> Int {
    r = q;
    return *r;
}

fn main() -> Int {
    let a: Int = 1;
    let b: Int = 42;
    return f(&a, &b);
}
```

```con accept name=borrow-each-use exit=42
struct P { n: Int }

fn get(r: &P) -> Int {
    return r.n;
}

fn main() -> Int {
    let p: P = P { n: 21 };
    let out: Int = get(&p) + get(&p);
    let P { n } = p;
    return out + n * 0;
}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_2i_ref6_let_shared_ref.con` | `reject:E0302:REF-6@1` | `ref6-place` | `-` |
| `tests/programs/error_2i_ref6_let_inferred_alias.con` | `reject:E0302:REF-6@1` | `ref6-place` | `-` |
| `tests/programs/error_2i_ref6_let_mut_ref.con` | `reject:E0302:REF-6@1` | `ref6-place` | `-` |
| `tests/programs/error_2i_ref6_let_ref_to_ref.con` | `reject:E0302:REF-6@1` | `ref6-place` | `-` |
| `tests/programs/error_104_assigned_ref_heap_block.con` | `reject:E0302:REF-6@1` | `ref6-place` | `-` |
| `tests/programs/error_104_assigned_ref_stack_block.con` | `reject:E0302:REF-6@1` | `ref6-place` | `-` |
| `tests/programs/error_091_ref_repointed_in_branch.con` | `reject:E0302:REF-6@1` | `ref6-place` | `-` |
| `tests/programs/error_091_mut_receiver_reassigned_ref.con` | `reject:E0302:REF-6@1` | `ref6-place` | `-` |
| `tests/programs/error_borrow_escape.con` | `reject:E0302:REF-6@1` | `ref6-place` | `-` |
| `tests/programs/error_2i_ref8_free_under_projected_borrow.con` | `reject:E0302:REF-6@1` | `ref6-place` | `-` |
| `tests/programs/error_2i_ref8_assign_under_shared.con` | `reject:E0302:REF-6@1` | `ref6-place` | `-` |
| `tests/programs/error_named_ref_mut_conflict.con` | `reject:E0302:REF-6@1` | `ref6-place` | `-` |
| `tests/programs/error_091_norm_copied_mut_ref.con` | `reject:E0302:REF-6@1` | `ref6-place` | `-` |
| `tests/programs/error_091_norm_assign_under_shared_ref.con` | `reject:E0302:REF-6@1` | `ref6-place` | `-` |
| `tests/programs/regress_2i_ref6_place_each_use.con` | `run:42` | `ref6-place` | `binary,interp` |
| `tests/programs/regress_2i_ref6_index_once.con` | `run:42` | `ref6-place` | `binary,interp` |
| `tests/programs/error_2i_ref6_borrow_block_copy.con` | `reject:E0302:REF-6@1` | `ref6-block` | `-` |
| `tests/programs/error_2i_ref6_borrow_block_paren_copy.con` | `reject:E0302:REF-6@1` | `ref6-block` | `-` |
| `tests/programs/error_2i_ref6_borrow_block_cast_copy.con` | `reject:E0302:REF-6@1` | `ref6-block` | `-` |
| `tests/programs/error_2i_ref6_borrow_block_reborrow_escape.con` | `reject:E0302:REF-6@1` | `ref6-block` | `-` |
| `tests/programs/regress_2i_ref6_borrow_block_projection.con` | `run:42` | `ref6-block` | `binary,interp` |
| `tests/programs/error_2i_ref6_generic_let_at_ref.con` | `reject:E0302:REF-6@1` | `ref6-generic` | `json-exempt:post-mono` |
| `tests/programs/error_2i_ref6_generic_assign_at_ref.con` | `reject:E0303:REF-6@1` | `ref6-generic` | `json-exempt:post-mono` |
| `tests/programs/regress_2i_ref6_generic_assign_at_value.con` | `run:42` | `ref6-generic` | `binary,interp` |
| `tests/programs/error_2i_ref6_param_repointed.con` | `reject:E0303:REF-6@1` | `ref6-use` | `-` |
| `tests/programs/error_2i_ref6_borrow_binder_assigned.con` | `reject:E0303:REF-6@1` | `ref6-use` | `-` |
| `tests/programs/error_2i_ref6_arm_binder_assigned.con` | `reject:E0303:REF-6@1` | `ref6-use` | `-` |
| `tests/programs/error_131_param_repointed_block_local.con` | `reject:E0303:REF-6@1` | `ref6-use` | `-` |
| `tests/programs/error_131_param_repointed_param.con` | `reject:E0303:REF-6@1` | `ref6-use` | `-` |
| `tests/programs/regress_2i_ref6_arm_binder_through_ref.con` | `run:42` | `ref6-use` | `binary,interp` |
| `tests/programs/error_2i_ref6_let_else_through_ref.con` | `reject:E0302:REF-6@1` | `ref6-let-else` | `-` |
| `tests/programs/regress_2i_ref6_method_let_else.con` | `run:42` | `ref6-let-else` | `binary,interp` |
| `tests/programs/error_2i_ref6_nested_let_else_through_ref.con` | `reject:E0302:REF-6@1` | `ref6-let-else` | `-` |
| `tests/programs/regress_2i_nested_let_destructure.con` | `run:42` | `ref6-let-else` | `binary,interp` |
| `tests/programs/error_2i_ref6_ghost_let_at_type_param.con` | `reject:E0302:REF-6@1` | `ref6-generic` | `-` |
| `tests/programs/error_2i_ref6_struct_destructure_through_ref.con` | `reject:E0302:REF-6@1` | `ref6-destructure` | `-` |
| `tests/programs/regress_2i_ref6_struct_fields_through_ref.con` | `run:42` | `ref6-destructure` | `binary,interp` |
| `tests/programs/error_2i_ref6_erased_assert_let.con` | `reject:E0302:REF-6@1` | `ref6-generic` | `-` |
| `tests/programs/error_2i_ref6_erased_assert_assign.con` | `reject:E0303:REF-6@1` | `ref6-generic` | `-` |
| `tests/programs/error_2i_ref6_ghost_initializer_let.con` | `reject:E0302:REF-6@1` | `ref6-generic` | `-` |
| `tests/programs/regress_2i_ref6_ghost_ptr_and_fn_types.con` | `run:42` | `ref6-generic` | `binary,interp` |
| `tests/programs/regress_2i_value_block_destructure.con` | `run:42` | `ref6-let-else` | `binary,interp,fmt` |
| `tests/programs/error_2i_ref6_const_initializer.con` | `reject:E0302:REF-6@1` | `const-init` | `-` |
| `tests/programs/error_2i_ref6_const_branch_borrow.con` | `reject:E0302:REF-6@1` | `const-init` | `-` |
| `tests/programs/error_2i_ref6_const_destructure_through_borrow.con` | `reject:E0302:REF-6@1` | `const-init` | `-` |
| `tests/programs/error_2i_ref6_const_match_through_borrow.con` | `reject:E0302:REF-6@1` | `const-init` | `-` |
| `tests/programs/error_2i_ref6_const_typed_ref_let.con` | `reject:E0302:REF-6@1` | `const-init` | `-` |
| `tests/programs/error_2i_ref6_const_borrow_block.con` | `reject:E0302:REF-6@1` | `const-init` | `-` |
| `tests/programs/error_2i_ref6_const_cast_to_ref.con` | `reject:E0302:REF-6@1` | `const-init` | `-` |
| `tests/programs/error_2i_ref6_const_aliased_ref.con` | `reject:E0302:REF-6@1` | `const-init` | `-` |
| `tests/programs/error_2i_ref6_erased_forwarded_generic.con` | `reject:E0302:REF-6@1` | `ref6-erased-call` | `-` |
| `tests/programs/error_2i_ref6_erased_generic_call.con` | `reject:E0302:REF-6@1` | `ref6-erased-call` | `-` |
| `tests/programs/regress_2i_ref6_generic_call_outside_assert.con` | `run:42` | `ref6-erased-call` | `binary,interp` |
| `tests/programs/regress_2i_const_nested_if_statement.con` | `run:42` | `const-init` | `binary,interp-exempt:bug-117-the-interpreter-has-no-module-constants` |
| `tests/programs/error_2i_ref6_mut_param_repointed.con` | `reject:E0303:REF-6@1` | `ref6-write` | `-` |
| `tests/programs/regress_2i_ref6_write_through_param.con` | `run:42` | `ref6-write` | `binary,interp` |

<a id="REF-7"></a>
### REF-7. No reference is written through a reference

**REF-7@1.** No assignment through a reference stores a value whose type holds a reference.

- Kind: refusal
- Uses: REF-D2
- Codes: E0238 (check)
- Status: enforced
- Enforced by: Concrete.checkStmt
- Semantics: none
- Obligation: INV-REF (R-0500)
- Evidence: tested

**Rationale.** `*slot = &local` with `slot: &mut &T` would leave the caller holding a reference
to the callee's local. A reference behind a reference (`& &T`) can exist as an argument, but
the inner reference is never read out of it (REF-5) and never written through it (this rule).

```con reject name=store-through-ref code=E0238 stage=check pair=write-value
fn replace(slot: &mut &Int) {
    let local: Int = 42;
    *slot = &local;
}

fn main() -> Int {
    let original: Int = 0;
    borrow original as r in R {
        replace(&mut r);
    }
    return original;
}
```

```con accept name=write-value exit=42
fn set(slot: &mut Int) {
    *slot = 42;
}

fn main() -> Int {
    let mut x: Int = 0;
    set(&mut x);
    return x;
}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_091_norm_ref_write_through_ref.con` | `reject:E0238:REF-7@1` | `ref7` | `-` |
| `tests/programs/regress_2i_ref7_write_value_through_mut_ref.con` | `run:42` | `ref7` | `binary,interp` |

<a id="REF-8"></a>
### REF-8. A reference's root stays the same live binding

**REF-8@1.** While a reference exists (a borrow-block binder for its block, an arm binder for its arm, a borrow argument for its call), its root binding is not moved, consumed, freed or shadowed and is not accessed in a mode that conflicts with the reference, and the borrows a single call makes do not conflict.

- Kind: refusal
- Uses: none
- Codes: E0203 (check), E0204 (check), E0218 (check), E0231 (check), E0232 (check), E0235 (check), E0293 (check), E0238 (check)
- Status: enforced
- Enforced by: Concrete.consumeVar, Concrete.checkCallBorrowConflicts, Concrete.checkWriteAccess
- Semantics: none
- Obligation: INV-REF (R-0500): the root is the same live binding while the reference lives
- Evidence: tested

**Rationale.** REF-6 fixes which binding a reference comes from. This rule keeps that binding
alive and unchanged for as long as the reference can be used. A parameter's source lives in a
caller frame, where the caller's call borrow protects it. Two borrows conflict when at least
one is exclusive. Since REF-11 the key is the root alone. Because REF-9 makes every arm binder
through a reference a reference, a Copy payload binder is a loan too, and writing its root
inside the arm is refused (the G17 Copy case, closed 2026-10-23).

```con reject name=free-in-borrow-block code=E0203 stage=check pair=free-after-block
struct B { v: Int }

fn read(r: &Heap<B>) -> Int {
    return 0;
}

fn main() with(Std) -> Int {
    let h: Heap<B> = alloc(B { v: 42 });
    let mut out: Int = 0;
    borrow h as r in R {
        free(h);
        out = read(r);
    }
    return out;
}
```

```con reject name=call-overlap code=E0293 stage=check pair=free-after-block
fn mix(a: &Int, b: &mut Int) -> Int {
    *b = 1;
    return *a;
}

fn main() -> Int {
    let mut x: Int = 42;
    return mix(&x, &mut x);
}
```

```con accept name=free-after-block exit=42
struct Copy B { v: Int }

fn read(r: &Int) -> Int {
    return *r;
}

fn main() with(Std) -> Int {
    let h: Heap<B> = alloc(B { v: 42 });
    let mut out: Int = 0;
    borrow h as r in R {
        out = 42;
    }
    out = read(&h.v) * 0 + out;
    free(h);
    return out;
}
```

```con reject name=copy-binder-root-written code=E0232 stage=check pair=free-after-block
struct Copy P { n: Int }

fn main() -> Int {
    let mut o: Option<P> = Option::Some { value: P { n: 42 } };
    let mut out: Int = 0;
    match &o {
        Option::Some { value } => {
            o = Option::None;
            out = value.n;
        },
        Option::None => {}
    }
    return out;
}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_borrow_frozen.con` | `reject:E0203:REF-8@1` | `ref8-block` | `-` |
| `tests/programs/error_2i_ref8_free_in_borrow_block.con` | `reject:E0203:REF-8@1` | `ref8-block` | `-` |
| `tests/programs/regress_2i_ref8_free_after_ref_block.con` | `run:42` | `ref8-block` | `binary,interp-exempt:no-heap-allocation-in-the-interpreter` |
| `tests/programs/error_2i_ref8_call_overlap.con` | `reject:E0293:REF-8@1` | `ref8-call` | `-` |
| `tests/programs/error_091_norm_cast_arg_overlap.con` | `reject:E0293:REF-8@1` | `ref8-call` | `-` |
| `tests/programs/error_od2_call_overlapping_path.con` | `reject:E0293:REF-8@1` | `ref8-call` | `-` |
| `tests/programs/regress_2i_ref8_call_disjoint.con` | `run:42` | `ref8-call` | `binary,interp` |
| `tests/programs/regress_2i_ref8_match_borrow_scrutinee.con` | `run:42` | `-` | `binary,interp` |
| `tests/programs/error_181_root_reassigned_match.con` | `reject:E0232:REF-8@1` | `ref8-arm` | `-` |
| `tests/programs/error_181_root_reassigned_guard.con` | `reject:E0232:REF-8@1` | `ref8-arm` | `-` |
| `tests/programs/error_181_root_reassigned_nested.con` | `reject:E0232:REF-8@1` | `ref8-arm` | `-` |
| `tests/programs/error_181_root_reassigned_while_let.con` | `reject:E0232:REF-8@1` | `ref8-arm` | `-` |
| `tests/programs/error_181_root_reassigned_whole_binder.con` | `reject:E0232:REF-8@1` | `ref8-arm` | `-` |
| `tests/programs/regress_181_borrowed_binders.con` | `run:42` | `ref8-arm` | `binary,interp` |

<a id="REF-9"></a>
### REF-9. A binder through a reference is a reference

**REF-9@2.** Every pattern binder that binds through a scrutinee of reference type has reference type, `&T` for a payload of type `T` and the scrutinee's own reference type for a whole-value binder, whether or not `T` is Copy.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.payloadBinderTy, Concrete.wholeBinderTy
- Semantics: none
- Obligation: binder modes are fixed before specialization (PASS-2)
- Evidence: tested

**Rationale.** Decided 2026-10-23 (TIGHTENING_DECISIONS D3), enforced the same day. The binding
mode used to depend on whether the payload is Copy, and monomorphization can change that
property. Check and Elab disagreed about it (bug 181: a segfault in safe code at `T := Int`). With
one mode that substitution cannot change, the class of bugs is gone. A whole-value binder (`n`
in `match r { n => .. }`) is the scrutinee reference itself, so over `&mut T` it is `&mut T`
(version 2; version 1 said `&T` for it too). A Copy payload is read with `*value`.
Literal and range patterns still compare the referenced value. The binder is a loan on the
scrutinee's root for its arm (REF-8).
The owner decided OD3 on 2026-10-29 as enforced here: the binder through a borrowed scrutinee is
always a reference (this rule), its mode is stored in Core as its type (PASS-2), and the root is not
reassigned while the binder's loan lives (REF-8).

```con accept name=copy-payload-is-ref exit=42
fn main() -> Int {
    let o: Option<Int> = Option::Some { value: 42 };
    match &o {
        Option::Some { value } => { return *value; },
        Option::None => { return 0; }
    }
}
```

<a id="REF-10"></a>
### REF-10. A `&mut` argument is an implicit reborrow

**REF-10@2.** Passing a `&mut` reference binding as an argument lends it to the call, the call's use of it ending when the call returns, so a parameter, borrow-block binder or arm binder of type `&mut T` may be passed to any number of calls in sequence.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.consumeCallArgIdent
- Semantics: none
- Obligation: the reborrow is explicit in Core (R-0496)
- Evidence: tested

**Rationale.** Decided by the owner on 2026-10-29 (OD2, part 1): passing a `&mut` on is plain
lending. There is no reborrow concept and no reborrow syntax; a call's use of the reference ends
when the call returns, which is sound because references are second-class (no reference is
returned or stored, REF-1, REF-3). Every call form lends the same way (`consumeCallArgIdent`: a free
function, a method argument, a static method, a function pointer), and a `&mut self` receiver given a
reference binding is lent too. Before the 2026-10-23 change (TIGHTENING_DECISIONS D1, now this
decision) a borrow-block binder was consumed by the first call it was passed to while a parameter was
not; one mode replaced the two, and no program broke. A deferred call keeps the loan of a `&mut` it
was given until it runs (OWN-16). An exclusive reference that is never used may go unused: it owns
nothing. Overlapping arguments in one call are REF-11's refusal.

`&mut T` is linear (not Copy: `consumeVar` tracks it like any linear binding), while `&T` is Copy.
Returning a `&mut T` binding is REF-1's refusal and rebinding it (`let r2 = r;`) REF-6's; passing it
to a call, or calling a `&mut self` method on it, lends it (this entry); reading or writing through
it (`*r`, `*r = v`, `r.f`, `r.f = v`, `r[i]`, `r[i] = v`; MEM-4) uses it without consuming it, since
a dereference does not go through `consumeVar`, an invariant any change to consumption must keep.
Branches agree on a `&mut` binding as on any linear value (OWN-3) and a loop does not consume one
declared outside it (OWN-4), while a `break` or `continue` may leave one unconsumed: OWN-2's exit
checks exempt references, which own nothing. A borrow-block binder need not be consumed:
dereferencing it inside the block is the normal use, and the binder lapses at the block's end, when
the owner is unfrozen. A write through it is a write to the owner's place at once (REF-13). The
earlier gaps of this area are closed: field and element writes through `&mut T` work (MEM-4), a
`&mut T` stored in a struct is REF-3's refusal, and `let s = &mut *r` is REF-6's. A `return` inside a
`borrow mut` block once failed SSA verification; the lowering skips a terminated block's exit code
(`blockTerminated`) and hoists the block's storage to the entry block (`emitEntryAlloca`), pinned by
`adversarial_mut_ref_return_in_borrow.con` and `adversarial_mut_ref_return_deref_in_borrow.con`.

```con accept name=reborrow-in-borrow-block exit=42
fn inc(r: &mut Int) {
    *r = *r + 21;
}

fn main() -> Int {
    let mut x: Int = 0;
    borrow mut x as r in R {
        inc(r);
        inc(r);
    }
    return x;
}
```

<a id="REF-11"></a>
### REF-11. Loans are keyed by root binding

**REF-11@3.** A borrow of any part of a value is a loan on its root binding and freezes the whole owner while it lives, so within one call every argument, the receiver included and a re-lent `&mut` binding included, conflicts with any other argument that borrows the same root when at least one of them is exclusive, whatever their projections.

- Kind: refusal
- Uses: none
- Codes: E0293 (check)
- Status: partial bug-319
- Enforced by: Concrete.pathsOverlap, Concrete.checkCallRootReads
- Semantics: none
- Obligation: INV-REF (R-0500); the loan checker needs no path-overlap analysis
- Evidence: tested

**Rationale.** Decided by the owner on 2026-10-29 (OD15, part (a)): a live borrow of any part of a
value freezes the whole owner, and loans are keyed by the root binding, as OD1 put it and as main
already enforces. This tightens OD2's part 2 as first recorded the same day: an exclusive argument
conflicts with any other argument that borrows the same root, so even disjoint fields conflict and
`h(&mut a.f, &mut a.g)` is refused, as are `upd(&mut s.a, &s.b)` and `add(&mut s.a, s.b)` (E0293);
the version recorded earlier that day, which allowed disjoint siblings, was never published. Two
fields of one owner are lent one call at a time, or read into a `let` first. Lending a reference
binding the caller already holds (`r.add(r.step)` with `r: &mut S`, `self.sift_up(self.len - 1)`
with `self: &mut Self`) lends that reference (REF-10) and makes no new loan, so reading through it
in another argument is not a second loan. A conflict between a borrow and a later use of its
place, outside one call, is REF-8's. History: the rule dates from 2026-10-23 (TIGHTENING_DECISIONS
D2); the measured migration was one std site (`std/src/cli.con:77`) and this fixture, and
`error_d2_call_disjoint_fields.con`, `error_d2_call_field_read.con`,
`error_d2_receiver_projection_read.con`, `error_d2_call_field_read_in_branch.con` and
`error_d2_explicit_mut_receiver_read.con` pin it in run_tests.sh. No pass may assume or exploit this
exclusivity until it is proved (OD15, part (b), REF-14).

The owner decided OD23 on 2026-10-29: the overlap check covers every argument, the receiver and
re-lent `&mut` bindings included, so `r.add(r.step)` with `r: &mut S` is refused; the Copy field is
read into a `let` first. Main still exempts a re-lent reference binding (bug 319), pinned by the
pending example below.


```con reject name=disjoint-fields-one-call code=E0293 stage=check pair=fields-in-sequence
struct Copy W { a: Int, b: Int }

fn two(a: &mut Int, b: &mut Int) {
    *a = 1;
    *b = 1;
}

fn main() -> Int {
    let mut w: W = W { a: 0, b: 0 };
    two(&mut w.a, &mut w.b);
    return w.a + w.b;
}
```

```con accept name=fields-in-sequence exit=2
struct Copy W { a: Int, b: Int }

fn one(a: &mut Int) {
    *a = 1;
}

fn main() -> Int {
    let mut w: W = W { a: 0, b: 0 };
    one(&mut w.a);
    one(&mut w.b);
    return w.a + w.b;
}
```

```con reject name=relend-receiver-read code=E0293 stage=check pair=relend-read-first pending=bug-319 now=accept:42
struct S { step: Int, acc: Int }
impl S {
    fn add(&mut self, n: Int) {
        self.acc = self.acc + n;
    }
}
fn bump(r: &mut S) {
    r.add(r.step);
}
fn main() -> Int {
    let mut s: S = S { step: 42, acc: 0 };
    bump(&mut s);
    let S { step, acc } = s;
    return acc;
}
```

```con accept name=relend-read-first exit=42
struct S { step: Int, acc: Int }
impl S {
    fn add(&mut self, n: Int) {
        self.acc = self.acc + n;
    }
}
fn bump(r: &mut S) {
    let n: Int = r.step;
    r.add(n);
}
fn main() -> Int {
    let mut s: S = S { step: 42, acc: 0 };
    bump(&mut s);
    let S { step, acc } = s;
    return acc;
}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_d2_call_disjoint_fields.con` | `reject:E0293:REF-11@3` | `ref11` | `-` |
| `tests/programs/error_d2_call_field_read.con` | `reject:E0293:REF-11@3` | `ref11` | `-` |
| `tests/programs/error_d2_receiver_projection_read.con` | `reject:E0293:REF-11@3` | `ref11` | `-` |
| `tests/programs/error_d2_call_field_read_in_branch.con` | `reject:E0293:REF-11@3` | `ref11` | `-` |
| `tests/programs/error_d2_explicit_mut_receiver_read.con` | `reject:E0293:REF-11@3` | `ref11` | `-` |
| `tests/programs/regress_2i_ref8_call_disjoint.con` | `run:42` | `ref11` | `binary,interp` |

<a id="REF-12"></a>
### REF-12. Nothing is mutated through a shared reference

**REF-12@1.** No place reached through a shared reference is mutated: it is not borrowed with `&mut`, not the receiver of a `&mut self` method and not assigned.

- Kind: refusal
- Uses: REF-D1
- Codes: E0237 (elab, check)
- Status: enforced
- Enforced by: Concrete.checkMutAccess, Concrete.checkWriteAccess
- Semantics: none
- Obligation: INV-REF (R-0500): a shared reference's target is not written while it lives
- Evidence: tested

**Rationale.** A shared reference may be one of several aliases of the same value, so a write
through it would change what the other aliases read. Exclusivity comes only from `&mut`
(REF-8). Elab refuses a `&mut self` call on a receiver of type `&T` before it builds the
implicit borrow; Check refuses the rest on the normalized place, so `(*r).f`, `r.f.g` and a
receiver reached through a conditional all count. A place Check cannot normalize is refused
(E0238), never assumed to be owned.

```con reject name=assign-through-shared code=E0237 stage=check pair=assign-through-exclusive
struct Copy P { n: Int }

fn set(p: &P) {
    p.n = 42;
}

fn main() -> Int {
    let p: P = P { n: 0 };
    set(&p);
    return p.n;
}
```

```con accept name=assign-through-exclusive exit=42
struct Copy P { n: Int }

fn set(p: &mut P) {
    p.n = 42;
}

fn main() -> Int {
    let mut p: P = P { n: 0 };
    set(&mut p);
    return p.n;
}
```
<a id="REF-13"></a>
### REF-13. A `&mut` refers to the place

**REF-13@1.** A `&mut` reference refers to the place it borrows, so a write through it is the write to that place at once, and no copy of the place is made at the borrow and written back at its end.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0496
- Semantics: none
- Obligation: INV-REF (R-0500)
- Evidence: tested

**Rationale.** Decided by the owner on 2026-10-29 (OD2, part 3). Copy-in and copy-out is not part
of the language: a backend may copy only as an optimization proven equivalent, which REF-11's
exclusivity at a call makes possible. Current main still lowers a borrow block and a `&mut x`
argument by copying the owner into a slot and writing it back at the exit, and an exit can lose
those writes (bugs 305 and 307, under repair); the guarantee holds once that lowering is replaced.

<a id="REF-14"></a>
### REF-14. An alias derived from a loan lives no longer than the loan

**REF-14@1.** A pointer or other alias derived from a loan is valid only while that loan lives, and `&mut` exclusivity is a guarantee the checker enforces, which no compiler pass or optimization assumes or exploits until that guarantee is proved in Lean.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0496
- Semantics: none
- Obligation: the checker's exclusivity guarantee, proved over the Core semantics (R-0499, R-0496), before any pass relies on it
- Evidence: tested

**Rationale.** Decided by the owner on 2026-10-29 (OD15, part (b)), the strict option. The language
guarantees `&mut` exclusivity and the checker enforces it (REF-8, REF-11), but a pass that exploits
it (alias metadata, reordering loads and stores across a call) would turn any checker bug into a
miscompile, so none may until the guarantee is a theorem (R-0499 with R-0496's Core semantics). Until
then the operational loan rules are the definition: a loan starts at its borrow and ends with the
borrow block, the arm or the call that holds it (REF-8), and a raw pointer or other alias derived
from it (G19) is valid only while it lives; using one afterwards is outside safe code's guarantee
(TRUST-3). Main emits no alias assumption today (no `noalias` or alias metadata); the rule is
pending until a gate checks that and the Core semantics defines loan lifetimes (R-0496).

