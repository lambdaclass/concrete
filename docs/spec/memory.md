# Memory

Safe code owns memory through bindings and `Heap<T>` values and reaches other storage only through
references (the references chapter). Every local binding has an ownership state (MEM-D1). This
chapter covers initialization (MEM-3), field access through
references and heap shells (MEM-4), heap ownership (MEM-5) and its capability (MEM-7), array bounds (MEM-6) and an index proven out of bounds (MEM-9), indexing through a
reference (MEM-8) and the two
layers of the memory model the Core semantics defines (MEM-1, MEM-2, pending). The checker
tracks whole owners: a borrow freezes its whole root (REF-8, REF-11), with no field- or
element-granular borrows. The design records are [MEMORY_SEMANTICS.md](../archive/design-records/MEMORY_SEMANTICS.md)
and [MEMORY_GUARANTEES.md](../verification/MEMORY_GUARANTEES.md), which keeps the public safety claim and
its boundaries.

## Definitions

<a id="MEM-D1"></a>
### MEM-D1. Ownership state

**MEM-D1@1.** At each program point a local binding is unconsumed, used (read or borrowed), consumed (moved), reserved (passed by value to a pending `defer`) or frozen (the owner of an enclosing borrow block, REF-D3), and reading or moving a Copy binding leaves its state unchanged, while a borrow block freezes it as any other.

- Kind: definition
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.VarState
- Semantics: none
- Obligation: none
- Evidence: by-users

**Rationale.** The rules key on these states: a consumed binding is not used again (OWN-1), a
linear binding is consumed or reserved before every exit (OWN-2), a reserved one is not moved
(OWN-12) and a frozen one is not accessed (REF-D3, REF-8). A Copy binding is frozen by a borrow
block like any other. A local passed by borrow to a pending `defer` is pinned, which is not a
state of its own: it stays unconsumed or used and may not be moved (OWN-12, OWN-16).

## Rules

<a id="MEM-1"></a>
### MEM-1. Two-layer memory

**MEM-1@1.** Safe values live in allocations with identities, raw memory is CompCert's block-and-offset model of bytes and pointer fragments with permissions, and a representation relation connects the two layers.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0502
- Semantics: none
- Obligation: the typed-value and raw-memory abstraction lemma (R-0496, R-0502)
- Evidence: tested

**Rationale.** Safe code reasons about structured values and places. Raw operations and FFI need
bytes, offsets and pointer provenance. Keeping both layers, with one relation between them, lets
safe programs be verified without byte-level reasoning, while raw code still has an exact
semantics (VERIFIED_PIPELINE, "IR chain").

<a id="MEM-2"></a>
### MEM-2. Addresses are quasi-concrete

**MEM-2@1.** A block has no numeric address until a pointer into it is exposed, and exposing a pointer is an operation with a semantic effect, never an erasable cast.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0502
- Semantics: none
- Obligation: a finite-address-space policy stated with the memory model (R-0502)
- Evidence: tested

**Rationale.** Treating every address as concrete makes optimizations unsound, and treating every
address as abstract makes integer-pointer round trips undefined. With quasi-concrete addresses,
only exposed blocks have numeric addresses, so unexposed blocks keep their freedom (VERIFIED_PIPELINE,
"Language decisions this requires"). The surface operations are TRUST-1's.

<a id="MEM-3"></a>
### MEM-3. Every binding is initialized

**MEM-3@1.** A binding is always declared with a value (`let x: T = e`), so safe code never reads uninitialized memory.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.parseStmt
- Semantics: none
- Obligation: no uninitialized state in Core's value semantics (R-0496)
- Evidence: tested

**Rationale.** There is no deferred-initialization form: `let x: T;` is a syntax error, and an
assignment only replaces an already valid value, so no definite-assignment analysis is needed.
Indeterminate memory exists only behind trusted or `Unsafe` operations, as a named premise
(TRUST-2).

```con accept name=initialized-bindings exit=42
fn example_value() -> Int {
    let mut x: Int = 1;
    x = 42;
    return x;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="MEM-4"></a>
### MEM-4. Field access dereferences at most one reference

**MEM-4@4.** A field access `e.f`, read or written, requires `e` to be the struct or a reference to it, dereferencing at most that one reference, and a `Heap` or `HeapArray` is opened by an explicit non-consuming place form, whose spelling is OPEN-36, `*` remaining the consuming move-out.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending bug-326
- Enforced by: Concrete.checkExprCore
- Semantics: none
- Obligation: one place representation in every consumer (R-0501, R-0504)
- Evidence: tested

**Rationale.** A reference to any place, a heap shell included, denotes that place: `&h` and
`&mut h` denote the slot `h`. Every consumer gives `e.f` this one meaning; a write through
`&mut Heap<S>` once corrupted the caller's slot (bug 126). Deeper types (`& &S`)
need an explicit `*`. Version 2, still main's behaviour, also opened one `Heap` layer implicitly (a
`HeapArray<T>` only directly, not through a reference), without consuming the heap pointer.

Version 3 records the owner's decision of 2026-10-29 (OD30): a field access no longer opens a heap
cell implicitly. The spelling of the explicit non-consuming place form is OPEN-36. Main still reaches
`f` through one `Heap` layer (`h.f` on `Heap<S>`, bug 326).


```con accept name=one-hop-field-access exit=42
struct Copy B { v: Int }

fn bump(h: &mut Heap<B>) {
    h.v = h.v + 1;
}

fn get(h: &Heap<B>) -> Int {
    return h.v;
}

fn example_value() with(Std) -> Int {
    let mut h: Heap<B> = alloc(B { v: 40 });
    bump(&mut h);
    let out: Int = get(&h) + 1;
    free(h);
    return out;
}

fn main() with(Std) -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="MEM-5"></a>
### MEM-5. A `Heap` value owns one heap cell

**MEM-5@1.** `alloc(v)` moves `v` into a new heap cell and returns the linear `Heap<T>` that owns it, `*h` moves the value out and frees the cell, `free(h)` frees the cell and returns its value, and a heap cell's interior is tracked like any other owner.

- Kind: guarantee
- Uses: OWN-D1
- Codes: none
- Status: enforced
- Enforced by: Concrete.checkExprCore
- Semantics: none
- Obligation: allocation identities of Core's safe layer (MEM-1, R-0496)
- Evidence: tested

**Rationale.** Because `Heap<T>` is linear, every allocation is freed, moved out of, passed on or
destroyed exactly once (OWN-2), and allocation needs `with(Alloc)`. Discarding the value `free`
returns is allowed only when it is Copy (OWN-5), and reading a non-Copy field out of a cell by
value is refused (OWN-8, bug 144). A node is taken apart with `let n = *h;` and a pattern.

```con accept name=heap-ownership exit=42
struct Copy B { v: Int }

fn example_value() with(Std) -> Int {
    let h: Heap<B> = alloc(B { v: 40 });
    let g: Heap<B> = alloc(B { v: 2 });
    let b: B = *h;
    let c: B = free(g);
    return b.v + c.v;
}

fn main() with(Std) -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="MEM-6"></a>
### MEM-6. An array index is checked

**MEM-6@2.** Reading or writing `a[i]` aborts when `i` is negative or at least the array's length.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Pipeline.lower
- Semantics: none
- Obligation: bounds are a trapping operation of Core; a pass removes the check only with a discharged-VC certificate (R-0493, R-0496)
- Evidence: tested

**Rationale.** One unsigned comparison catches both a negative index and one past the end, and the
abort is the same as an arithmetic trap (ARITH-3). There is no global way to turn bounds checks
off; an unchecked access would be a trusted operation with a named premise (TRUST-2).
Version 2 leaves the refusal of an index the compiler proves out of bounds to MEM-9, which names its
code (E0900).

```con accept name=index-in-bounds exit=42
fn example_value() -> Int {
    let a: [Int; 3] = [10, 30, 2];
    let mut i: Int = 0;
    let mut total: Int = 0;
    while i < 3 {
        total = total + a[i];
        i = i + 1;
    }
    return total;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con abort name=index-out-of-bounds trap=out_of_bounds
fn example_value() -> Int {
    let a: [Int; 3] = [1, 2, 3];
    let mut i: Int = 0;
    i = i + 5;
    return a[i];
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

<a id="MEM-7"></a>
### MEM-7. The heap builtins require `Alloc`

**MEM-7@1.** `alloc`, `free`, the move-out `*h` of an owned `Heap`, `vec_new`, `vec_push`, `vec_pop`, `vec_free`, `string_reserve` and `append` require the `Alloc` capability.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.IntrinsicId.capability, Concrete.ccCheckExpr
- Semantics: none
- Obligation: capability containment over the call graph after specialization (R-0499, R-0510)
- Evidence: tested

**Rationale.** Allocation is an effect a signature shows. A function that calls one of these builtins
without `Alloc` in its `with(...)` set is refused by TRUST-4 (E0520). A field access through a
reference to a heap shell is not a move-out and needs no capability (MEM-4). Absorbed from
[EFFECT_PROOF_BOUNDARIES.md](../verification/EFFECT_PROOF_BOUNDARIES.md) §2.

```con accept name=alloc-declares-alloc exit=42
struct Copy B { v: Int }

fn make(v: Int) with(Alloc) -> Heap<B> {
    return alloc(B { v: v });
}

fn example_value() with(Alloc) -> Int {
    let h: Heap<B> = make(42);
    let b: B = free(h);
    return b.v;
}

fn main() with(Alloc) -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="MEM-8"></a>
### MEM-8. Indexing dereferences at most one reference

**MEM-8@1.** An index `a[i]`, read or written, whose base `a` is a `&[T; N]` or `&mut [T; N]` indexes the array the reference refers to, dereferencing that one reference.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.checkExprCore, Concrete.ccCheckExpr
- Semantics: none
- Obligation: one place representation in every consumer (R-0501, R-0504)
- Evidence: tested

**Rationale.** This is MEM-4's one-hop rule for an index: the place is the array behind the
reference, bounds-checked as MEM-6 states, and a write through `&mut [T; N]` writes the caller's
array (REF-13). TYPE-7's "only an array is indexed" is read with this one dereference. Absorbed
from [KNOWN_HOLES.md](../verification/KNOWN_HOLES.md) C10.

```con accept name=index-through-reference exit=42
fn get(a: &[Int; 3]) -> Int {
    return a[1];
}

fn set(a: &mut [Int; 3]) {
    a[2] = 38;
}

fn example_value() -> Int {
    let mut arr: [Int; 3] = [1, 2, 3];
    set(&mut arr);
    return get(&arr) + arr[2] + 2;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="MEM-9"></a>
### MEM-9. An index proven out of bounds is refused

**MEM-9@1.** An index `a[i]` that the compiler proves always out of bounds for the array's length is refused in a function that neither is marked `trusted` nor declares `Unsafe`; whether it is also refused in a function marked `trusted` or declaring `Unsafe` is OPEN-118.

- Kind: refusal
- Uses: none
- Codes: E0900 (runtime-safety)
- Status: partial bug-340
- Enforced by: Concrete.Report.provenViolationDiagnostics
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** MEM-6@1 stated this refusal as the second half of its sentence; it is its own
entry so that the refusal names its code, since only a refusal names codes. The compiler proves
an index out of bounds when the index and the array's length are constants it evaluates; any other
index is checked at run time (MEM-6). Inside a `trusted` function or one declaring `Unsafe`, the
current compiler compiles such an index, and it aborts at run time; the sentence leaves that case to
OPEN-118. The native build
refuses both reject examples below. `--interp` runs them (bug 340): it stops at the out-of-bounds
index in the first, so that example is checked natively only, and it exits 42 in the second, whose
index is on a path that does not run, so that example is pending until the bug is fixed.

```con accept name=index-proven-in-bounds exit=42
fn example_value() -> Int {
    let a: [Int; 3] = [10, 30, 2];
    return a[0] + a[1] + a[2];
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con reject name=index-proven-out-of-bounds code=E0900 stage=runtime-safety pair=index-proven-in-bounds interp-exempt=interp-runs-proven-violations
fn example_value() -> Int {
    let a: [Int; 3] = [10, 30, 2];
    return a[5];
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con reject name=index-proven-out-of-bounds-unrun code=E0900 stage=runtime-safety pair=index-proven-in-bounds pending=bug-340 now=reject:E0900:runtime-safety now-interp=accept:42
fn example_value() -> Int {
    let a: [Int; 3] = [10, 30, 2];
    let c: bool = false;
    if c {
        return a[5];
    }
    return 42;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```
