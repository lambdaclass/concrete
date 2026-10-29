# Trusted Code and Raw Pointers

Concrete's safety model has three parts. **Capabilities** (`with(File, Alloc)`) are the effects a
function may exercise, visible in its signature. **`trusted`** contains pointer-level implementation
techniques behind a safe API. **`Unsafe`** is the capability for crossing a foreign or raw-memory
boundary, and a `trusted` body absorbs it. No layer is exempt: compiler builtins, `std` and user
code follow the same rules. This chapter covers raw pointers (TRUST-1, TRUST-3), undefined
behaviour (TRUST-2), capabilities (TRUST-4), `trusted` (TRUST-5), the trusted-module marker
(TRUST-6, pending) and moving a value out of raw memory (TRUST-7). Handle capabilities (`Sink<cap C>`) and callable values with capabilities are
specified in [HANDLE_CAPABILITIES.md](../language/HANDLE_CAPABILITIES.md) and
[CALLABLE_VALUES_AND_CAPABILITIES.md](../language/CALLABLE_VALUES_AND_CAPABILITIES.md) until they
migrate. The design records are [SAFETY.md](../language/SAFETY.md),
[CAPABILITY_FACTS.md](../language/CAPABILITY_FACTS.md).

## Rules

<a id="TRUST-1"></a>
### TRUST-1. Pointer arithmetic and address exposure are named operations

**TRUST-1@1.** Pointer arithmetic is written `ptr::offset`, and a conversion between a pointer and an integer is written `ptr::expose` or `ptr::from_exposed`, so `as` never converts between a pointer and an integer.

- Kind: refusal
- Uses: none
- Codes: new (check)
- Status: pending R-0502
- Semantics: none
- Obligation: offset, expose and from_exposed have their MEM-2 semantics (R-0502)
- Evidence: tested

**Rationale.** A pointer-to-integer `as` cast is lossy: it discards provenance and silently
exposes the block. Pointer arithmetic written as integer arithmetic on a cast loses which block
the result belongs to. Named operations make provenance and exposure explicit (MEM-2). std uses
the cast form today (`std/src/text.con`, `dst as u64 + i`), so the migration is std's
raw-pointer helpers.

```con reject name=pointer-to-integer-cast code=new stage=check pair=no-pointer-cast pending=R-0502 now=accept:0
trusted fn addr(p: *const Int) -> u64 {
    return p as u64;
}

fn main() -> Int {
    return 0;
}
```

```con accept name=no-pointer-cast exit=0
fn main() -> Int {
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

**TRUST-4@1.** A function may call a function or perform an operation that requires a capability only if the capability is in its own `with(...)` set, whether the requirement comes from a direct call, a call through a trait bound, a function-pointer type or a specialization, and `Unsafe` is such a capability.

- Kind: refusal
- Uses: none
- Codes: E0121 (resolve), E0240 (check), E0242 (check), E0520 (core-check), E0521 (core-check)
- Status: enforced
- Enforced by: Concrete.ccCheckExpr, Concrete.checkExprCore
- Semantics: none
- Obligation: capability containment over the call graph after specialization (R-0499, R-0510)
- Evidence: tested

**Rationale.** Effects stay visible at every call site: a function with an empty capability set
cannot reach the file system or the network. That is an authority claim, not purity (OWN-7):
mutation through `&mut` and foreign effects are not capabilities. The nine capabilities are
`File`, `Network`, `Time`, `Env`, `Random`, `Process`, `Console`, `Alloc` and `Unsafe`; `Std` is
every one but `Unsafe`, and `cap IO = File + Console;` declares an alias. A trait implementation's
method may require only the capabilities its trait method declares, and the specialized call graph
is re-checked after monomorphization. Raw-pointer dereference and assignment, pointer arithmetic,
pointer casts other than a reference to a pointer, and calls to an `extern fn` require `Unsafe`.
A capability variable (`<cap C>`) entails only itself, so it may not be spelled like a capability:
`<cap Unsafe>` would hold the real `Unsafe` by name without being granted it (bug 188, E0121).

```con reject name=undeclared-capability code=E0520 stage=core-check pair=declared-capability
fn needs() with(File) -> Int {
    return 1;
}

fn main() -> Int {
    return needs();
}
```

```con reject name=raw-deref-needs-unsafe code=E0521 stage=core-check pair=declared-capability
fn rd(p: *const Int) -> Int {
    return *p;
}

fn main() -> Int {
    return 0;
}
```

```con reject name=capability-variable-named-unsafe code=E0121 stage=resolve pair=declared-capability
fn apply<cap Unsafe>(f: fn(Int) with(Unsafe) -> Int) with(Unsafe) -> Int {
    return f(41);
}

fn add_one(x: Int) -> Int {
    return x + 1;
}

fn main() -> Int {
    return apply(add_one);
}
```

```con accept name=declared-capability exit=42
fn needs() with(File) -> Int {
    return 41;
}

fn rd(p: *const Int) with(Unsafe) -> Int {
    return *p;
}

fn main() with(File, Unsafe) -> Int {
    let x: Int = 1;
    return needs() + rd(&x as *const Int);
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
trusted fn rd(p: *const Int) -> Int {
    return *p;
}

fn main() -> Int {
    let x: Int = 42;
    return rd(&x as *const Int);
}
```

<a id="TRUST-6"></a>
### TRUST-6. Trusted code lives in marked modules

**TRUST-6@2.** Only a module declared `trusted mod` may contain `trusted` functions, `extern` declarations and raw-pointer operations, and the marker confers no trust by itself.

- Kind: refusal
- Uses: none
- Codes: new (check)
- Status: pending R-0509
- Semantics: none
- Obligation: the trusted base is the list of marked modules (R-0509)
- Evidence: tested

**Rationale.** Decided by the owner on 2026-10-29 (OD7). The marker puts the trusted base in the
source, where a reviewer reads it, and a gate lists every marked module; it never confers trust, so
each function still declares `trusted` (per method, OD22) and its capabilities. Whether and how a
program holds mutable global state (global cells reached through capabilities, no unrestricted
`static mut`) is OD8, still open (OPEN-8); the 2026-10-23 proposal (TIGHTENING_DECISIONS D8) is
recorded there. The compiler's emitted globals (`__concrete_argc`, `__concrete_argv`) and std's
environment mutation are inventoried as trusted base.

```con reject name=trusted-outside-marked-module code=new stage=check pair=trusted-in-marked-module pending=R-0509 now=accept:42
mod m {
    trusted fn rd(p: *const Int) -> Int {
        return *p;
    }
    fn main() -> Int {
        let x: Int = 42;
        return rd(&x as *const Int);
    }
}
```

```con accept name=trusted-in-marked-module exit=42
mod m {
    fn main() -> Int {
        let x: Int = 42;
        return x;
    }
}
```

<a id="TRUST-7"></a>
### TRUST-7. Moving a value out of raw memory is explicit

**TRUST-7@1.** In trusted or `Unsafe` code, `*p` through a raw pointer `p` reads by value only a Copy pointee, and a non-Copy value is moved out of raw memory only by `read_owned(p)`.

- Kind: refusal
- Uses: OWN-D1
- Codes: E0311 (check)
- Status: enforced
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

```con reject name=raw-deref-moves-linear code=E0311 stage=check pair=read-owned-moves
struct Copy B { v: Int }

trusted fn dup<K>(x: &mut K) -> K {
    let p: *mut K = x as *mut K;
    return *p;
}

fn main() with(Alloc) -> Int {
    let mut h: Heap<B> = alloc(B { v: 42 });
    let h2: Heap<B> = dup::<Heap<B>>(&mut h);
    free(h2);
    free(h);
    return 0;
}
```

```con accept name=read-owned-moves exit=42
struct Copy B { v: Int }

trusted fn take_and_replace(slot: &mut Heap<B>, fresh: Heap<B>) -> Heap<B> {
    let p: *mut Heap<B> = slot as *mut Heap<B>;
    let old: Heap<B> = read_owned(p);
    *p = fresh;
    return old;
}

fn main() with(Alloc) -> Int {
    let mut h: Heap<B> = alloc(B { v: 40 });
    let old: Heap<B> = take_and_replace(&mut h, alloc(B { v: 2 }));
    let a: B = free(old);
    let b: B = free(h);
    return a.v + b.v;
}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_rawdup_trusted_deref.con` | `reject:E0311:TRUST-7@1` | `trust7` | `-` |
| `tests/programs/regress_198_read_owned.con` | `run:42` | `trust7` | `binary,interp` |

