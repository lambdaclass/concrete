# Trusted Code and Raw Pointers

Concrete's safety model has three parts. **Capabilities** (`with(Fs, Alloc)`) are the effects a
function may exercise, visible in its signature. **`trusted`** contains pointer-level implementation
techniques behind a safe API. **`Unsafe`** is the capability for crossing a foreign or raw-memory
boundary, and a `trusted` body absorbs it. No layer is exempt: compiler builtins, `std` and user
code follow the same rules. This chapter covers what a capability means (TRUST-D1), raw pointers (TRUST-1, TRUST-3), undefined
behaviour (TRUST-2), capabilities (TRUST-4), `trusted` (TRUST-5), the refusal of the trusted-module marker
(TRUST-6, pending), moving a value out of raw memory (TRUST-7), global state (TRUST-8 to TRUST-10), storing through a raw pointer (TRUST-11), the
ownership rules in trusted code (TRUST-12) and taking a reference's address (TRUST-13). The section
"Trusted premises" lists every premise the compiler relies on and does not check. Capability sets, capability parameters and the capabilities of function types and
handles (`Sink<cap C>`) are in the types chapter (TYPE-D1, TYPE-15, TYPE-17, TYPE-18); this specification governs them. The design records are [SAFETY.md](../verification/SAFETY.md),
[CAPABILITY_FACTS.md](../compiler/CAPABILITY_FACTS.md),
[HANDLE_CAPABILITIES.md](../archive/design-records/HANDLE_CAPABILITIES.md),
[CALLABLE_VALUES_AND_CAPABILITIES.md](../archive/design-records/CALLABLE_VALUES_AND_CAPABILITIES.md) and
[MEMORY_SEMANTICS.md](../archive/design-records/MEMORY_SEMANTICS.md) (§10 and §11, absorbed into
TRUST-11 and TRUST-12).

## Definitions

<a id="TRUST-D1"></a>
### TRUST-D1. What a capability means

**TRUST-D1@2.** Capabilities are compile-time permissions with the reserved names `Fs`, `Network`, `Env`, `Time`, `Alloc`, `Device`, `Process` and `Unsafe`, distinct from resource values and never supplied by holding or returning a value, with no aliases or umbrella capabilities.

- Kind: definition
- Uses: none
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.validCaps
- Semantics: none
- Obligation: none
- Evidence: by-users

**Rationale.** Authority rules 1–2 and 13 distinguish permission sets from resource values. Fs covers filesystem operations on Directory/File, Network covers network grants and sockets, Env the Environment grant, Time the Clock grant, Alloc allocator operations, Device device access, Process process services, and Unsafe raw and foreign validity obligations. Holding a value grants no permission; exact target service contracts are [undecided: see the target Core proposal](decisions.md#core-grants).

## Rules

<a id="TRUST-1"></a>
### TRUST-1. Pointer arithmetic and address exposure are named operations

**TRUST-1@1.** Pointer arithmetic is written `ptr::offset`, and a conversion between a pointer and an integer is written `ptr::expose` or `ptr::from_exposed`, so `as` never converts between a pointer and an integer.

- Kind: refusal
- Uses: none
- Codes: new (check)
- Status: pending R-0555
- Semantics: none
- Obligation: offset, expose and from_exposed have their MEM-2 semantics (R-0502)
- Evidence: tested

**Rationale.** A pointer-to-integer `as` cast is lossy: it discards provenance and silently
exposes the block. Pointer arithmetic written as integer arithmetic on a cast loses which block
the result belongs to. Named operations make provenance and exposure explicit (MEM-2). std uses
the cast form today (`std/src/text.con`, `dst as u64 + i`), so the migration is std's
raw-pointer helpers.

```con reject name=pointer-to-integer-cast code=new stage=check pair=no-pointer-cast pending=R-0502 now=accept:0
trusted fn addr(p: *const i64) -> u64 {
    return p as u64;
}

fn main() -> u8 {
    return 0;
}
```

```con accept name=no-pointer-cast exit=0
fn main() -> u8 {
    return 0;
}
```

<a id="TRUST-2"></a>
### TRUST-2. Safe code has no undefined behaviour

**TRUST-2@1.** Safe code has no undefined behaviour, and every undefined behaviour of the language is a named premise of a trusted operation that the trusted code's author discharges.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0506
- Semantics: none
- Obligation: every stuck state of Core semantics is a violated named premise (R-0496, R-0506)
- Evidence: tested

**Rationale.** A safe program either runs to a defined outcome (STMT-1) or is refused. Undefined
behaviour exists only where a trusted operation states a premise it cannot check, such as a raw
dereference needing an aligned, live and initialized pointer. Naming each premise makes the
trusted base a list that can be audited ([TRUSTED_COMPUTING_BASE.md](../verification/TRUSTED_COMPUTING_BASE.md)).

<a id="TRUST-3"></a>
### TRUST-3. A reference from a raw pointer carries a premise

**TRUST-3@1.** In trusted or `Unsafe` code, `&*p` and `&mut *p` form a reference from a raw pointer `p` only under the premise that `p` is aligned, live and initialized for its type, and the reference then obeys every rule of the references chapter.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0506
- Semantics: none
- Obligation: the premise is a named premise of the trusted dereference (TRUST-2)
- Evidence: tested

**Rationale.** A raw-pointer dereference is not one of REF-5's two operations that form a
reference. Trusted code may form a reference this way, but only by discharging the validity
premise itself (TIGHTENING_DECISIONS D2, caveats). The resulting reference is second-class like
any other.

Decided by the owner on 2026-10-29 (OD1): a raw-pointer dereference needs a trusted validity premise.


<a id="TRUST-4"></a>
### TRUST-4. A function exercises only the capabilities it declares

**TRUST-4@3.** Every body uses only its declared `with(...)` permissions and every call requires the callee’s set to be included in the caller’s across trait dispatch, specialization and cleanup, with `trusted` absorbing only `Unsafe` and all raw operations and reference-to-pointer conversions requiring `trusted` or `Unsafe`.

- Kind: refusal
- Uses: none
- Codes: E0121 (resolve), E0240 (check), E0242 (check), E0520 (core-check), E0521 (core-check)
- Status: pending R-0555
- Enforced by: Concrete.ccCheckExpr, Concrete.checkExprCore
- Semantics: none
- Obligation: capability containment over the call graph after specialization (R-0499, R-0510)
- Evidence: tested

**Rationale.** Authority rules 5–7 require effect containment even inside trusted bodies; no Std or user umbrella alias exists. Intrinsic effects come from the primitive table (FFI-6); reference-to-pointer conversion is gated by OD98.

```con reject name=undeclared-capability-decided code=E0520 stage=core-check pair=declared-capability pending=R-0555 now=reject:E0520:core-check
fn needs() with(Network) -> i64 {
    return 1;
}

fn example_value() -> i64 {
    return needs();
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=raw-deref-needs-unsafe-decided code=E0521 stage=core-check pair=declared-capability pending=R-0555 now=reject:E0521:core-check
fn rd(p: *const i64) -> i64 {
    return *p;
}

fn main() -> u8 {
    return 0;
}
```

```con reject name=capability-variable-named-unsafe-decided code=E0121 stage=resolve pair=declared-capability pending=R-0555 now=reject:E0121:resolve
fn apply<cap Unsafe>(f: fn(i64) with(Unsafe) -> i64) with(Unsafe) -> i64 {
    return f(41);
}

fn add_one(x: i64) -> i64 {
    return x + 1;
}

fn example_value() -> i64 {
    return apply(add_one);
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=declared-capability exit=42
fn needs() with(Network) -> i64 {
    return 41;
}

fn rd(p: *const i64) with(Unsafe) -> i64 {
    return *p;
}

fn example_value() with(Network, Unsafe) -> i64 {
    let x: i64 = 1;
    return needs() + rd(&x as *const i64);
}

fn main() with(Network, Unsafe) -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="TRUST-5"></a>
### TRUST-5. `trusted` absorbs `Unsafe` and nothing else

**TRUST-5@2.** Trust is declared per function, never on an `impl` block, and a `trusted fn` may perform raw-pointer operations and call an `extern fn` without declaring `Unsafe`, while every other capability it uses, those an `extern fn` declares included, still appears in its `with(...)` set and binds its callers, and a `trusted extern fn` is a foreign binding callable without `Unsafe`.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.ccCheckExpr
- Semantics: none
- Obligation: every trusted operation states its premises (TRUST-2, R-0506, R-0509)
- Evidence: tested

**Rationale.** `trusted` is an audited containment boundary: the code inside uses techniques safe
code cannot, and that use is concentrated where a reviewer looks for it. It vouches for memory
safety, so it absorbs `Unsafe` (R-0484); it never hides an effect a caller should see, and it
does not relax linearity. A `trusted extern fn` asserts that the foreign function is safe for every
argument its types permit (`sqrt`, `abs`); one whose safety depends on pointer validity, or that can
invalidate a resource another handle owns, stays a plain `extern fn`.

`Unsafe` used to conflate three facts, which are now separate: operational authority, declared
as `with(File, Alloc, ...)`, which the caller supplies; a caller safety obligation, declared as
`with(Unsafe)`, an invariant the language cannot establish; and trust provenance, derived, never
declared: the implementation depends on raw operations, FFI or an audited assertion. An API may be
safe to call while raw inside (`Vec::push`, `String::clone`), so provenance never propagates as a
caller requirement; the four combinations are a safe interface over a checked or a trusted body, and
an obligation-bearing interface over a checked or a trusted body. Per-method `trusted` makes a safe
interface over an audited method expressible, which a whole-impl modifier could not; the owner
decided on 2026-10-29 (OD22) that trust is per method only, so `trusted impl` stays refused (a parse
error, E0001; bug 189's repair, `error_189_trusted_impl_refused.con`), and the two axes stay independent: `pub trusted fn read_unchecked(..) with(Unsafe)` is an audited body whose
caller still owes an obligation. A trusted body discharges an `Unsafe` obligation at a call, scoped
three ways so that it is a discharge and not an erasure: only `Unsafe` (operational capabilities
still bind), `extern` callees included since R-0484 (every `extern` now declares its effects, E0116),
and only the call (the raw-operation gate, E0521, is untouched). The 2026-09-25 migration took std's
public `Unsafe` declarations from 154 to 21 with no operational capability set changed and no
corpus regression; it found the inverse defect too, 15 public std APIs that traffic raw pointers
and declare no obligation (`String::from_raw_unchecked` the worst), recorded with the measurements
in [research/language/two-axis-safety-migration.md](../../research/language/two-axis-safety-migration.md).

```con accept name=trusted-absorbs-unsafe exit=42
trusted fn rd(p: *const i64) -> i64 {
    return *p;
}

trusted fn example_value() -> i64 {
    let x: i64 = 42;
    return rd(&x as *const i64);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="TRUST-6"></a>
### TRUST-6. Trusted code lives in marked modules

**TRUST-6@3.** The `trusted mod` marker is refused; trust is declared per function and externs, intrinsics and minting wrappers are individually inventoried in the trusted base.

- Kind: refusal
- Uses: none
- Codes: E0001 (parse)
- Status: pending R-0555
- Semantics: none
- Obligation: the trusted base inventories primitive and minting identities (R-0509)
- Evidence: tested

**Rationale.** OD61 supersedes OD7’s historical marker. Libraries may declare inventoried externs, only the compiler introduces intrinsics, and only the target runtime mints initial grants.





```con reject name=marker-removed code=E0001 stage=parse pair=per-function-trust pending=R-0555 now=reject:E0001:parse
trusted mod m { fn main() -> u8 { return 42; } }
```

```con accept name=per-function-trust exit=42
trusted fn id(n: u8) -> u8 { return n; }
fn main() -> u8 { return id(42); }
```

<a id="TRUST-7"></a>
### TRUST-7. Moving a value out of raw memory is explicit

**TRUST-7@2.** In trusted or `Unsafe` code, `*p` through a raw pointer `p` reads by value only a Copy pointee, and a non-Copy value is moved out of raw memory only by `read_owned(p)`.

- Kind: refusal
- Uses: OWN-D1
- Codes: E0311 (check)
- Status: pending R-0555
- Enforced by: Concrete.checkExprCore
- Semantics: none
- Obligation: the slot's ownership transfer is a named premise of `read_owned` (TRUST-2, R-0506)
- Evidence: tested

**Rationale.** Decided 2026-10-23 (TIGHTENING_DECISIONS T-RAWDUP; bug 198). `trusted` does not
relax linearity. A by-value `*p` of a non-Copy pointee copied a value the slot still owned, so a
trusted helper could free one heap cell twice. `read_owned(p)` states the transfer; afterwards the
slot is logically uninitialized until a value is stored back, a premise the trusted code's author
discharges (TRUST-2), which no checker tracks yet. Place uses
(`(*p).f`, `&*p`), Copy pointees and stores through `*p` are unchanged.

Decided by the owner on 2026-10-29 (OD13): a value moves out of raw memory only through `read_owned(p)` or an explicit replace or take through `&mut T`, and a raw address carries a trusted ownership obligation; bug 198's repair is final.

```con reject name=raw-deref-moves-linear-decided code=E0311 stage=check pair=read-owned-moves pending=R-0555 now=reject:E0311:check
struct R { v: u8 }
trusted fn dup<K>(x: &mut K) -> K { let p: *mut K = x as *mut K; return *p; }
fn main() -> u8 { let mut r: R = R { v: 40 }; let r2: R = dup::<R>(&mut r); let R { v: a } = r2; let R { v: b } = r; return a + b; }
```

```con accept name=read-owned-moves exit=42
struct R { v: u8 }
trusted fn swap(slot: &mut R, fresh: R) -> R {
 let p: *mut R = slot as *mut R; let old: R = read_owned(p); *p = fresh; return old;
}
fn main() -> u8 { let mut r: R = R { v: 40 }; let old: R = swap(&mut r, R { v: 2 }); let R { v: a } = old; let R { v: b } = r; return a + b; }
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_rawdup_trusted_deref.con` | `reject:E0311:TRUST-7@2` | `trust7` | `-` |
| `tests/programs/regress_198_read_owned.con` | `run:42` | `trust7` | `binary,interp` |

<a id="TRUST-8"></a>
### TRUST-8. A program has no mutable global state

**TRUST-8@2.** No module-level declaration holds mutable state: there is no `static mut` or global cell, and a constant is immutable, has no interior mutability and never contains a resource or handle, while effects are checked independently through `with(...)`.

- Kind: refusal
- Uses: none
- Codes: new (check)
- Status: pending R-0555
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** OD8’s constants-only rule survives; authority rules replace its token-only effect model with resource values plus independent permissions.

```con reject name=constant-holds-token code=new stage=check pair=constant pending=R-0557 now=accept:42
struct Token { v: i64 }

const T: Token = Token { v: 42 };

fn example_value() -> i64 {
    let t: Token = T;
    let Token { v } = t;
    return v;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con accept name=constant exit=42
struct Copy P { v: i64 }

const C: P = P { v: 42 };

fn example_value() -> i64 {
    return C.v;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="TRUST-9"></a>
### TRUST-9. Resources originate in target grants

**TRUST-9@2.** Only the target runtime mints initial grants, `main` declares and consumes the offered grants it needs without a `Root` type or split, and subsequent resources arise only by checked acquisition or unforgeable constructors from existing grants, with every extern, compiler intrinsic and minting wrapper inventoried by identity.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Semantics: none
- Obligation: target grants and acquisition contracts (R-0557, R-0509)
- Evidence: tested

**Rationale.** Authority rules 11–12 replace the root-token model: a grant not declared by main is never minted, no user code runs at startup, and target startup/shutdown effects are explicit. Target grant schemas are [undecided: see the target Core proposal](decisions.md#core-grants).

<a id="TRUST-10"></a>
### TRUST-10. Explicit resources and exact callback permissions

**TRUST-10@2.** Handlers and callbacks receive resource values only by explicit installation, transfer or a lock granting a borrow, and function values retain exact capability sets without conveying permissions, while permissions for future foreign function values remain undecided: see the target Core proposal.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Authority rules separate explicit resource transfer from compile-time permission sets; the canonical exact callback-set rule is authority rule 4. Future foreign callback permissions are [undecided: see the target Core proposal](decisions.md#core-grants).

<a id="TRUST-11"></a>
### TRUST-11. A raw store neither reads nor destroys the old contents

**TRUST-11@2.** In trusted or `Unsafe` code, a store `*p = v` through a raw pointer `p: *mut T` moves `v` into the slot without reading or destroying what the slot held, whether or not `T` is Copy.

- Kind: guarantee
- Uses: OWN-D1
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.checkStmt, Concrete.lowerStmt, Concrete.Interp.evalStmtCore
- Semantics: none
- Obligation: whether the slot held a live value is a named premise of the store (TRUST-2, R-0506)
- Evidence: tested

**Rationale.** This is how trusted collection code writes an uninitialized slot, or refills one that
`read_owned` emptied (TRUST-7). The same store through `&mut T` over a live non-Copy value is
refused (OWN-9), because that value would be lost; through a raw pointer the checker cannot know
whether the slot is live, so overwriting a live linear value leaks it, and avoiding that is the
trusted code author's premise.

```con accept name=refill-emptied-slot exit=42
struct R { v: u8 }
trusted fn swap(slot: &mut R, fresh: R) -> R {
 let p: *mut R = slot as *mut R; let old: R = read_owned(p); *p = fresh; return old;
}
fn main() -> u8 { let mut r: R = R { v: 40 }; let old: R = swap(&mut r, R { v: 2 }); let R { v: a } = old; let R { v: b } = r; return a + b; }
```

<a id="TRUST-12"></a>
### TRUST-12. Trusted code keeps the move, cleanup and freeze rules

**TRUST-12@2.** In a `trusted` function, as in safe code, a moved linear value is not used again, a linear value is consumed on every exit, and an owner frozen by a borrow block is not accessed.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.checkFn
- Semantics: none
- Obligation: the trusted body is checked by the same judgment as safe code (R-0499)
- Evidence: tested

**Rationale.** A double move, a skipped cleanup or an access to a frozen owner in a `trusted fn` is
refused with the same code as in safe code: OWN-1's `trusted-use-after-move`, OWN-2's
`trusted-never-consumed` and REF-8's `trusted-frozen-owner`. These are the rules the design record
(MEMORY_SEMANTICS §11) listed; it also said that `trusted` relaxes no ownership rule, which the current compiler
contradicts twice: trusted code may destroy a value through a reference (E0290 under OWN-8
elsewhere) and release with `vec_free` a vector whose elements are not Copy (E0287 under OWN-5
elsewhere), the drop-glue cases of std's collections. OD113 refuses these ownership exceptions.
The checker's guarantees stop at
raw pointers: what lies behind `*mut T` or `*const T` is not tracked (OWN-D1), which is why a raw
store (TRUST-11) and a move out of raw memory (TRUST-7) carry premises. OD64 confines borrow-derived pointers to their borrow.

```con accept name=trusted-consumes-once-decided exit=42 pending=R-0555 now=reject:E0001:parse
struct R { v: i64 }

fn take(r: R) -> i64 {
    let R { v } = r;
    return v;
}

fn peek(r: &R) -> i64 {
    return r.v;
}

trusted fn use_once(r: R) -> i64 {
    let mut seen: i64 = 0;
    borrow r as rr {
        seen = peek(rr);
    }
    return take(r) + seen * 0;
}

fn example_value() -> i64 {
    return use_once(R { v: 42 });
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="TRUST-13"></a>
### TRUST-13. Reference-to-pointer conversion is gated

**TRUST-13@2.** Reference-to-pointer conversions require `trusted` or `Unsafe`, and a pointer derived from a borrow stays inside that borrow, never escapes or is exposed as an integer, and is passed to externs only through call-only parameters.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.ccCheckExpr
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** OD98 gates the conversion, and OD64 confines the derived pointer to its borrow, including copies, offsets and helper calls; shared provenance never yields writes. Call-only extern annotation syntax and complete unsafe contracts are [undecided: see the target Core proposal](decisions.md#core-unsafe-premises).

```con accept name=reference-to-pointer-in-safe-code exit=42
trusted fn rd(p: *const i64) -> i64 {
    return *p;
}

trusted fn example_value() -> i64 {
    let x: i64 = 42;
    return rd(&x as *const i64);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

## Trusted premises

This section is explanatory and states no rule. It lists each place where the language relies on a
premise that the compiler does not check, who states it, and the entry that names it. Where an
entry states a premise, the table only collects it; where no entry states it yet, the row says so. TRUST-2 (pending) is the rule that every
undefined behaviour is such a named premise, and
[TRUSTED_COMPUTING_BASE.md](../verification/TRUSTED_COMPUTING_BASE.md) records the trusted base of
the compiler and toolchain.

| place | premise the compiler does not check | stated by | entries |
|---|---|---|---|
| a `trusted fn`, or a function with `Unsafe` in its `with(...)` set | the premises of every raw-pointer operation and foreign call in its body | the function's author, by writing `trusted` or `with(Unsafe)` | TRUST-5, TRUST-4, TRUST-2 (pending) |
| a raw-pointer read `*p`, a write `*p = v` or an offset (`ptr::offset`) | the validity premises of `*p`, `*p = v` and `ptr::offset`; TRUST-1 names `ptr::offset`, TRUST-2 (pending) is the rule that each undefined behaviour be a named premise, and no entry lists these premises yet (registry item R-0506) | the author of the trusted or `Unsafe` code | TRUST-4, TRUST-1, TRUST-2 (pending); element stride: FFI-12 |
| `&*p` and `&mut *p` | `p` is aligned, live and initialized for its type | the author of the trusted or `Unsafe` code | TRUST-3 (pending) |
| `read_owned(p)` | what the slot holds before and after the move-out; TRUST-7 makes the move-out explicit, and no entry states this premise yet | the author of the trusted or `Unsafe` code | TRUST-7 |
| a raw store `*p = v` | whether the slot held a live value, which the store does not destroy | the author of the trusted or `Unsafe` code | TRUST-11 |
| a raw pointer taken from a reference | what the pointer may be used for and for how long; REF-14 (pending) is the entry for an alias derived from a loan, and OD64 confines it to its borrow with call-only extern use | the code that uses the pointer | TRUST-13, REF-14 (pending), OPEN-64 |
| an `extern fn` | the declared parameter types, result type and `with(...)` set match the C symbol | the declaration's author | FFI-3, FFI-5; "Abort and failure" in [statements.md](statements.md) |
| a `trusted extern fn` | the foreign function is safe for every argument its types permit and has no effect beyond its `with(...)` set | the declaration's author | FFI-9, TRUST-5 |
| a resource acquisition or minting wrapper | its external-state and authority contract; listing it proves nothing | the wrapper's author; a gate lists the wrappers | TRUST-9 (pending), TRUST-10 (pending) |
| `assume e` in a contract | `e` holds, as a hypothesis whose evidence is `assumed` | the programmer | CON-6 |
| an entry whose `Evidence:` is `trusted: <ID>` | the assumption that ID names in TRUSTED_COMPUTING_BASE.md | the specification ([MAINTAINERS.md](MAINTAINERS.md) §2.1); no entry uses it today | MAINTAINERS.md §2.1 |
| the native profile | the stated premises of the trusted base under which the forward simulation is proved | the proof of PASS-3 | PASS-3 (pending) |
| the LLVM profile | the BIR-to-LLVM printer, LLVM and the linker are correct; the results are differential-tested, not proved | the compiler | PASS-4 (pending) |

TRUST-12 is the entry for the move, cleanup and freeze rules inside a `trusted fn`. The raw-memory
premises listed above (`read_owned`, raw stores) remain separate from those rules. OD113 grants no ownership exceptions to trusted code.

## Absent by decision

This section is explanatory: it records capability features the language does not have, why, and
what would reopen them. The grammar (SYN-1) is what refuses them.

### No capability-set bounds, subtraction or capability values

Decided with the callable-values design (2026-06-12,
[CALLABLE_VALUES_AND_CAPABILITIES.md](../archive/design-records/CALLABLE_VALUES_AND_CAPABILITIES.md)
§6 and §10, after
[research/language/capability-polymorphism.md](../../research/language/capability-polymorphism.md)).
A capability parameter takes no bound (`where C ⊆ {File, Console}`), a capability set is never
written as a difference, and a capability is not a value that can be stored, passed or returned:
capabilities are permissions the checker compares as sets (TYPE-D1), not effects. So there are no
effect handlers and no row-polymorphic effect types either; a combinator's set is its callbacks'
sets, named by capability parameters (TYPE-17). None of these is needed by the higher-order std
surface. A bound would be reconsidered only if a workload needs a combinator that refuses some
capability its callback could carry; global resources are values (TRUST-9), distinct from permissions.

## Version history

This appendix records superseded versions and is not normative.

- TRUST-D1@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; TRUST-D1@1 is superseded by version.
- TRUST-4@3 (2026-11-03) brings the chapter into line with the recorded owner decisions; TRUST-4@2 is superseded by version.
- TRUST-6@3 (2026-11-03) brings the chapter into line with the recorded owner decisions; TRUST-6@2 is superseded by version.
- TRUST-7@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; TRUST-7@1 is superseded by version.
- TRUST-8@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; TRUST-8@1 is superseded by version.
- TRUST-9@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; TRUST-9@1 is superseded by version.
- TRUST-10@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; TRUST-10@1 is superseded by version.
- TRUST-11@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; TRUST-11@1 is superseded by version.
- TRUST-13@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; TRUST-13@1 is superseded by version.

- TRUST-12@2 (2026-11-03) supersedes TRUST-12@1; pending legacy conformance is explicit.
