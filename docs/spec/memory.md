# Memory

Safe code owns memory through bindings and `Heap<T>` values and reaches other storage only through
references (the references chapter). A binding's ownership state moves forward only (unconsumed,
used, consumed; reserved by a pending `defer`; frozen while a borrow block borrows it), and Copy
bindings are not tracked. This chapter covers initialization (MEM-3), field access through
references and heap shells (MEM-4), heap ownership (MEM-5), array bounds (MEM-6) and the two
layers of the memory model the Core semantics defines (MEM-1, MEM-2, pending). The checker
tracks whole owners: a borrow freezes its whole root (REF-8, REF-11), with no field- or
element-granular borrows. The design records are [MEMORY_SEMANTICS.md](../language/MEMORY_SEMANTICS.md)
and [MEMORY_GUARANTEES.md](../verification/MEMORY_GUARANTEES.md), which keeps the public safety claim and
its boundaries.

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
fn main() -> Int {
    let mut x: Int = 1;
    x = 42;
    return x;
}
```

<a id="MEM-4"></a>
### MEM-4. Field access dereferences at most one reference

**MEM-4@3.** A field access `e.f`, read or written, requires `e` to be the struct or a reference to it, dereferencing at most that one reference, and a `Heap` or `HeapArray` is opened by an explicit non-consuming place form, `*` remaining the consuming move-out.

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

fn main() with(Std) -> Int {
    let mut h: Heap<B> = alloc(B { v: 40 });
    bump(&mut h);
    let out: Int = get(&h) + 1;
    free(h);
    return out;
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

fn main() with(Std) -> Int {
    let h: Heap<B> = alloc(B { v: 40 });
    let g: Heap<B> = alloc(B { v: 2 });
    let b: B = *h;
    let c: B = free(g);
    return b.v + c.v;
}
```

<a id="MEM-6"></a>
### MEM-6. An array index is checked

**MEM-6@1.** Reading or writing `a[i]` aborts when `i` is negative or at least the array's length, and an index the compiler proves out of bounds is refused.

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

```con accept name=index-in-bounds exit=42
fn main() -> Int {
    let a: [Int; 3] = [10, 30, 2];
    let mut i: Int = 0;
    let mut total: Int = 0;
    while i < 3 {
        total = total + a[i];
        i = i + 1;
    }
    return total;
}
```

```con abort name=index-out-of-bounds trap=out_of_bounds
fn main() -> Int {
    let a: [Int; 3] = [1, 2, 3];
    let mut i: Int = 0;
    i = i + 5;
    return a[i];
}
```

<a id="MEM-7"></a>
### MEM-7. A raw pointer to a live local denotes that local

**MEM-7@1.** A raw pointer taken to a local that is still live denotes that local across control flow, so a write through the local is seen through the pointer and a write through the pointer is seen through the local.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.lowerStmt
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** A local whose address is taken lives in memory for as long as the pointer can be used; moving
it back into a register at a loop's exit split it from the pointer (bug 302). Dereferencing the
pointer is trusted (TRUST-3); what it denotes is not left to the lowering.

```con accept name=pointer-and-local-agree exit=42
trusted fn rd(p: *mut i64) -> i64 {
    return *p;
}

fn main() -> Int {
    let mut x: i64 = 1;
    let mut y: i64 = 0;
    let mut p: *mut i64 = &mut y as *mut i64;
    let mut i: i64 = 0;
    while i < 2 {
        p = &mut x as *mut i64;
        i = i + 1;
    }
    x = 42;
    return rd(p) as Int;
}
```

