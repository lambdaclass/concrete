# Trusted Code and Raw Pointers

Concrete's safety model has three parts. **Capabilities** (`with(File, Alloc)`) are the effects a
function may exercise, visible in its signature. **`trusted`** contains pointer-level implementation
techniques behind a safe API. **`Unsafe`** is the capability for crossing a foreign or raw-memory
boundary, and a `trusted` body absorbs it. No layer is exempt: compiler builtins, `std` and user
code follow the same rules. This chapter covers what a capability means (TRUST-D1), raw pointers (TRUST-1, TRUST-3), undefined
behaviour (TRUST-2), capabilities (TRUST-4), `trusted` (TRUST-5), the trusted-module marker
(TRUST-6, pending), moving a value out of raw memory (TRUST-7), global state (TRUST-8 to TRUST-10), storing through a raw pointer (TRUST-11), the
ownership rules in trusted code (TRUST-12) and taking a reference's address (TRUST-13). Capability sets, capability parameters and the capabilities of function types and
handles (`Sink<cap C>`) are in the types chapter (TYPE-D1, TYPE-15, TYPE-17, TYPE-18); this specification governs them. The design records are [SAFETY.md](../verification/SAFETY.md),
[CAPABILITY_FACTS.md](../compiler/CAPABILITY_FACTS.md),
[HANDLE_CAPABILITIES.md](../archive/design-records/HANDLE_CAPABILITIES.md),
[CALLABLE_VALUES_AND_CAPABILITIES.md](../archive/design-records/CALLABLE_VALUES_AND_CAPABILITIES.md) and
[MEMORY_SEMANTICS.md](../archive/design-records/MEMORY_SEMANTICS.md) (§10 and §11, absorbed into
TRUST-11 and TRUST-12).

## Definitions

<a id="TRUST-D1"></a>
### TRUST-D1. What a capability means

**TRUST-D1@1.** A capability names logical authority, not where data goes: `Console` is authority over the standard streams the environment handed the process, so a redirected standard output is still `Console`, `File` is authority to name and open filesystem paths, `Network` is authority to create and use sockets, `Random` is authority over external entropy, which running a seeded pseudo-random generator does not need, and `Unsafe` is the authority for the raw-memory and foreign operations TRUST-4 lists.

- Kind: definition
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.validCaps
- Semantics: none
- Obligation: none
- Evidence: by-users

**Rationale.** A meaning by destination would depend on run-time conditions the compiler cannot
see: a program that never named a file cannot know that its output goes to one, and pipes,
terminals and `/dev/null` look alike from inside. SPARK's `Ada.Text_IO` likewise carries one
coarse file-system state and excludes redirection rather than modelling it. `with(...)` lists
capabilities, aliases (`Std`, `cap IO = File + Console;`) and capability variables (TYPE-D1). The
meanings of `Time`, `Env`, `Process` and `Alloc` are stated nowhere yet (OPEN-95), and a name in
`with(...)` that is not a capability, a capability alias or a capability variable is an unresolved name, which MOD-1 refuses and the current compiler accepts
(bug 337). The compiler holds only the names (`validCaps`) and checks every use against the declared
sets (TRUST-4); what a name grants is fixed by the foreign bindings that declare it, since every
`extern fn` declares its effects (FFI-3), and std's bindings are audited against these meanings.
That audit is an assumption about std, not a compiler check. An empty `with()` means no
external authority, not purity: the function may still mutate what it is lent through `&mut`
(TRUST-4), and it is not by that alone callable from a contract (CON-3). What a capability means
does not settle a descriptor that is replaced, reused or aliased; std's handles avoid those cases
([HANDLE_CAPABILITIES_AUDIT.md](../stdlib/HANDLE_CAPABILITIES_AUDIT.md)).

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

**TRUST-4@2.** A function may call a function or perform an operation that requires a capability only if the capability is in its own `with(...)` set, whether the requirement comes from a direct call, a call through a trait bound, a function-pointer type or a specialization, and `Unsafe` is such a capability; the capabilities are `File`, `Network`, `Time`, `Env`, `Random`, `Process`, `Console`, `Alloc` and `Unsafe`, `Std` stands for every one of them but `Unsafe`, and a raw-pointer dereference or assignment, pointer arithmetic, a pointer cast other than a reference to a pointer, and a call of an `extern fn` require `Unsafe`.

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

Until bug 188 a caller holding any capability variable was treated as covering every requirement,
`Unsafe` included, on the theory that the variable is resolved at instantiation; nothing re-checked
the generic body there, so `fn f<cap C>(g: fn(i32) with(C) -> i32) with(C)` could print, call
`with(Process)`, call an untrusted `extern` or dereference a raw pointer, and `main` instantiated
`C := {}` and ran it with no authority. Now a requirement is satisfied by a concrete capability the
caller declares, or by a capability variable only when the variable's declared bound entails it;
a capability parameter declares no bound, so it entails exactly itself, and an unknown variable
fails closed. The decision is made on the generic definition, before monomorphization: CoreCheck
checks a call against the callee's declared concrete set, and Check checks a call to a
capability-polymorphic callee against the instantiated set, `Unsafe` included, with the trusted-body
discharge (TRUST-5); the post-monomorphization re-check stays as defence in depth. Two questions
about `Unsafe` stay distinct: authority (`trusted`, or a set that covers `Unsafe`) gates the raw
operations, while literal membership (the set lists `Unsafe`) is what reports count, and a
capability-polymorphic function has neither. One rule, `Capabilities.missingCaps` (with
`capsContain` its boolean view), serves direct calls, calls through a function pointer, method calls,
calls through a trait bound and the raw-operation gate; where the capability facts live in the
compiler is [docs/compiler/CAPABILITY_FACTS.md](../compiler/CAPABILITY_FACTS.md).

What each capability means is TRUST-D1. Reserved for later: `Device` (memory-mapped hardware, when freestanding code needs
it), `Thread` (with the concurrency model) and `Signal` (deferred), recorded in
`research/language/capability-sandboxing.md` §4a. A capability alias expands at parse time, so the
rest of the compiler sees only capability names; `Std` is a fixed alias, not a macro system.
The safety model as a whole (capabilities, `trusted`, `Unsafe`, the audit reports, the proof
boundary and the high-integrity profile direction) is described in
[docs/verification/SAFETY.md](../verification/SAFETY.md).

```con reject name=undeclared-capability code=E0520 stage=core-check pair=declared-capability
fn needs() with(File) -> Int {
    return 1;
}

fn example_value() -> Int {
    return needs();
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=raw-deref-needs-unsafe code=E0521 stage=core-check pair=declared-capability
fn rd(p: *const Int) -> Int {
    return *p;
}

fn main() -> u8 {
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

fn example_value() -> Int {
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
fn needs() with(File) -> Int {
    return 41;
}

fn rd(p: *const Int) with(Unsafe) -> Int {
    return *p;
}

fn example_value() with(File, Unsafe) -> Int {
    let x: Int = 1;
    return needs() + rd(&x as *const Int);
}

fn main() with(File, Unsafe) -> u8 {

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
trusted fn rd(p: *const Int) -> Int {
    return *p;
}

fn example_value() -> Int {
    let x: Int = 42;
    return rd(&x as *const Int);
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
each function still declares `trusted` (per method, OD22) and its capabilities. Global state is
OD8's, decided 2026-10-29: constants only, and global resources as linear tokens (TRUST-8 to
TRUST-10). The compiler's emitted globals (`__concrete_argc`, `__concrete_argv`) and std's
environment mutation are inventoried as trusted base.

```con reject name=trusted-outside-marked-module code=new stage=check pair=trusted-in-marked-module pending=R-0509 now=accept:42
mod m {
    trusted fn rd(p: *const Int) -> Int {
        return *p;
    }
    fn example_value() -> Int {
        let x: Int = 42;
        return rd(&x as *const Int);
    }
    fn main() -> u8 {
        if example_value() == 42 {
            return 42;
        }
        return 1;
    }
}
```

```con accept name=trusted-in-marked-module exit=42
mod m {
    fn example_value() -> Int {
        let x: Int = 42;
        return x;
    }
    fn main() -> u8 {
        if example_value() == 42 {
            return 42;
        }
        return 1;
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

fn main() with(Alloc) -> u8 {
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

fn example_value() with(Alloc) -> Int {
    let mut h: Heap<B> = alloc(B { v: 40 });
    let old: Heap<B> = take_and_replace(&mut h, alloc(B { v: 2 }));
    let a: B = free(old);
    let b: B = free(h);
    return a.v + b.v;
}

fn main() with(Alloc) -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_rawdup_trusted_deref.con` | `reject:E0311:TRUST-7@1` | `trust7` | `-` |
| `tests/programs/regress_198_read_owned.con` | `run:42` | `trust7` | `binary,interp` |

<a id="TRUST-8"></a>
### TRUST-8. A program has no mutable global state

**TRUST-8@1.** No module-level declaration holds mutable state: there is no `static mut` and no global cell, and a constant is immutable, has no interior mutability and never contains a capability, a token or a handle.

- Kind: refusal
- Uses: none
- Codes: new (check)
- Status: pending R-0557
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Decided by the owner on 2026-10-29 (OD8): global state follows Austral's capability
model, extended. A global that any function can reach is an effect no signature shows, so the only
module-level values are constants, and a constant is a value with nothing behind it to mutate or
spend. The grammar has no mutable global (`static mut N: Int = 0;` is a parse error, SYN-1), so
that half holds today; a constant of a linear type is accepted and each use yields a fresh copy of it, which mints a
linear value the program never received, and its refusal is pending (R-0557). The proposal of
2026-10-23 (declared cells in marked modules, reached through capabilities) was rejected after the
Codex and Grok reviews: interrupt handlers and FFI callbacks re-enter a cell, a cell holding a
linear value has no owner to destroy it, cells need effect polymorphism to stay visible, and they
are shared state once threads exist. Global resources are linear tokens instead (TRUST-9).

```con reject name=constant-holds-token code=new stage=check pair=constant pending=R-0557 now=accept:42
struct Token { v: Int }

const T: Token = Token { v: 42 };

fn example_value() -> Int {
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
struct Copy P { v: Int }

const C: P = P { v: 42 };

fn example_value() -> Int {
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
### TRUST-9. Global resources are linear tokens

**TRUST-9@1.** A global resource is reached only through a linear token: `main` receives the root resource value and splits it, and a hardware or runtime singleton is produced or taken only by a `trusted fn` that returns or takes a linear token, each such function listed by a gate.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0557
- Semantics: none
- Obligation: the trusted external state is the gate's list of token wrappers (R-0557, R-0509)
- Evidence: tested

**Rationale.** Decided by the owner on 2026-10-29 (OD8); the root value's type, how it splits and the
signature `main` declares to receive it are OPEN-38. The root value plays the part of Austral's
`RootCapability`: everything a program may touch outside its own values descends from what `main`
was given. The singletons are the allocator arena, a UART and memory-mapped I/O, the panic buffer,
`errno` and C libraries with internal state; each is minted or consumed by a `trusted fn` wrapper,
and the gate's list of those wrappers is the inventory of trusted external state. Tokens are
ordinary values: every effect shows as a token parameter, and tokens follow the loan and
exclusivity rules (REF-8, REF-11, REF-13) with no separate effect system. Nothing runs before `main`
and there is no cleanup protocol for global state; an abort runs nothing (STMT-1, STMT-2). An
implicit-context mechanism, if ergonomics later asks for one, is a new owner decision and never a
hidden global. This changes STMT-7, whose `main` takes no parameters today: the root parameter
lands with R-0557.

<a id="TRUST-10"></a>
### TRUST-10. Handlers and callbacks hold only the tokens they are given

**TRUST-10@1.** An interrupt handler, an FFI callback or a thread holds only the tokens explicitly given to it, by installation, by transfer or through a lock that grants a borrow.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0557
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Decided by the owner on 2026-10-29 (OD8). Code that runs outside the call tree of
`main` cannot reach a resource through a global (TRUST-8), so what it may touch is exactly what was
handed to it, and the handing-over is visible in the source. Threads do not exist yet; when they do,
they follow the same rule.

<a id="TRUST-11"></a>
### TRUST-11. A raw store neither reads nor destroys the old contents

**TRUST-11@1.** In trusted or `Unsafe` code, a store `*p = v` through a raw pointer `p: *mut T` moves `v` into the slot without reading or destroying what the slot held, whether or not `T` is Copy.

- Kind: guarantee
- Uses: OWN-D1
- Codes: none
- Status: enforced
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
struct Copy B { v: Int }

trusted fn swap_in(slot: &mut Heap<B>, fresh: Heap<B>) -> Heap<B> {
    let p: *mut Heap<B> = slot as *mut Heap<B>;
    let old: Heap<B> = read_owned(p);
    *p = fresh;
    return old;
}

fn example_value() with(Alloc) -> Int {
    let mut h: Heap<B> = alloc(B { v: 40 });
    let old: Heap<B> = swap_in(&mut h, alloc(B { v: 2 }));
    let a: B = free(old);
    let b: B = free(h);
    return a.v + b.v;
}

fn main() with(Alloc) -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="TRUST-12"></a>
### TRUST-12. Trusted code keeps the move, cleanup and freeze rules

**TRUST-12@1.** In a `trusted` function, as in safe code, a moved linear value is not used again, a linear value is consumed on every exit, and an owner frozen by a borrow block is not accessed.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
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
elsewhere), the drop-glue cases of std's collections. Whether those exceptions stay is OPEN-113.
The checker's guarantees stop at
raw pointers: what lies behind `*mut T` or `*const T` is not tracked (OWN-D1), which is why a raw
store (TRUST-11) and a move out of raw memory (TRUST-7) carry premises. A pointer derived from a
borrow that outlives the borrow is OPEN-64's question.

```con accept name=trusted-consumes-once exit=42
struct R { v: Int }

fn take(r: R) -> Int {
    let R { v } = r;
    return v;
}

fn peek(r: &R) -> Int {
    return r.v;
}

trusted fn use_once(r: R) -> Int {
    let mut seen: Int = 0;
    borrow r as rr in Reg {
        seen = peek(rr);
    }
    return take(r) + seen * 0;
}

fn example_value() -> Int {
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
### TRUST-13. Taking a reference's address is safe

**TRUST-13@1.** Converting a reference to a raw pointer requires neither `trusted` nor `Unsafe`, while every use of the pointer that reads, writes or offsets through it does (TRUST-4, TRUST-5).

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.ccCheckExpr
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** The conversion only names an address that a live borrow already denotes; the hazard is
in using the pointer, and that is gated. Which conversions `as` writes, this one included, is
OPEN-98, and what such a pointer may do after its borrow ends is OPEN-64. Absorbed from
[SAFETY.md](../verification/SAFETY.md) ("What trusted permits").

```con accept name=reference-to-pointer-in-safe-code exit=42
trusted fn rd(p: *const Int) -> Int {
    return *p;
}

fn example_value() -> Int {
    let x: Int = 42;
    let p: *const Int = &x as *const Int;
    return rd(p);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

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
capability its callback could carry; global resources as values are linear tokens (TRUST-9), not
capability values.
