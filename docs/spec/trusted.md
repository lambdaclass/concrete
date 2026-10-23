# Trusted Code and Raw Pointers

Concrete's safety model has three parts. **Capabilities** (`with(File, Alloc)`) are the effects a
function may exercise, visible in its signature. **`trusted`** contains pointer-level implementation
techniques behind a safe API. **`Unsafe`** is the capability for crossing a foreign or raw-memory
boundary, and a `trusted` body absorbs it. No layer is exempt: compiler builtins, `std` and user
code follow the same rules. This chapter covers raw pointers (TRUST-1, TRUST-3), undefined
behaviour (TRUST-2), capabilities (TRUST-4), `trusted` (TRUST-5) and the trusted-module marker
(TRUST-6, pending). Handle capabilities (`Sink<cap C>`) and callable values with capabilities are
specified in [HANDLE_CAPABILITIES.md](../language/HANDLE_CAPABILITIES.md) and
[CALLABLE_VALUES_AND_CAPABILITIES.md](../language/CALLABLE_VALUES_AND_CAPABILITIES.md) until they
migrate. The design records are [SAFETY.md](../language/SAFETY.md),
[TWO_AXIS_SAFETY.md](../language/TWO_AXIS_SAFETY.md) and
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

<a id="TRUST-4"></a>
### TRUST-4. A function exercises only the capabilities it declares

**TRUST-4@1.** A function may call a function or perform an operation that requires a capability only if the capability is in its own `with(...)` set, whether the requirement comes from a direct call, a call through a trait bound, a function-pointer type or a specialization, and `Unsafe` is such a capability.

- Kind: refusal
- Uses: none
- Codes: E0240 (check), E0242 (check), E0520 (core-check), E0521 (core-check)
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

**TRUST-5@1.** A `trusted fn` or `trusted impl` may perform raw-pointer operations and call an `extern fn` without declaring `Unsafe`, while every other capability it uses, those an `extern fn` declares included, still appears in its `with(...)` set and binds its callers, and a `trusted extern fn` is a foreign binding callable without `Unsafe`.

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

**TRUST-6@1.** Only a module declared `trusted mod` may contain `trusted` functions, `extern` declarations, raw-pointer operations and global state cells, every global state cell is reached only through functions that require a capability, and there is no unrestricted mutable global.

- Kind: refusal
- Uses: none
- Codes: new (check)
- Status: pending R-0509
- Semantics: none
- Obligation: the trusted base is the list of marked modules, and each cell is part of the world state its capability grants (R-0509, R-0496)
- Evidence: tested

**Rationale.** Decided 2026-10-23 (TIGHTENING_DECISIONS D8 and D10). The marker puts the trusted
base in the source, where a reviewer reads it, and a gate lists every marked module; it never
confers trust, so each function still declares `trusted` and its capabilities. Immutable globals
(`const`, immutable statics) are allowed everywhere. The compiler's emitted globals
(`__concrete_argc`, `__concrete_argv`) and std's environment mutation are inventoried as trusted
base. Cells are built when a workload needs them, allocators being the likely first.

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

