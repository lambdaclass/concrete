# Memory

Safe code owns memory through bindings and `Heap<T, A>` values and reaches other storage only through
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
fn example_value() -> i64 {
    let mut x: i64 = 1;
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

**MEM-4@5.** A field access requires a struct or one reference to it and dereferences at most that reference; a borrow block opens exactly one heap layer without consuming it, preserving the root and lifetime and never upgrading shared access, while a `HeapArray` exposes only a bounded element view whose exact type is undecided: see the target Core proposal.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.checkExprCore
- Semantics: none
- Obligation: one place representation in every consumer (R-0501, R-0504)
- Evidence: tested

**Rationale.** OD30 and OD36 require explicit heap opening by borrow block, including through a shell reference; the exact HeapArray view is [undecided: see the target Core proposal](decisions.md#core-heap-array-view).



<a id="MEM-5"></a>
### MEM-5. A `Heap` value owns one heap cell

**MEM-5@2.** `alloc(a: &mut Allocator<A>, v: T) with(Alloc)` returns `Result<Heap<T, A>, AllocFailure<T>>`, conserving `v` on failure and allocator validity in every outcome, while `*h` consumes a heap and releases its storage returning the payload, `destroy(h)` destroys it explicitly, and `free` is refused.

- Kind: guarantee
- Uses: OWN-D1
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.checkExprCore
- Semantics: none
- Obligation: allocation identities of Core's safe layer (MEM-1, R-0496)
- Evidence: tested

**Rationale.** The Allocation decision preserves inputs on failure, records allocator identity and release information without storing a safe allocator reference, and forbids giving up an allocator with live allocations; OD59 leaves consuming extraction and explicit destruction instead of `free`. Brand binders, dependency discipline and release metadata are [undecided: see the target Core proposal](decisions.md#core-allocator-identities), and failure payloads are [undecided: see the target Core proposal](decisions.md#core-allocation-contracts).



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
fn example_value() -> i64 {
    let a: [i64; 3] = [10, 30, 2];
    let mut i: i64 = 0;
    let mut total: i64 = 0;
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
fn example_value() -> i64 {
    let a: [i64; 3] = [1, 2, 3];
    let mut i: i64 = 0;
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

**MEM-7@2.** Allocation and allocating container or string operations require both an `Allocator<A>` value and `with(Alloc)` and report conserving failure, allocations retain their allocator identity in `Heap<T, A>`, `Vec<T, A>` or `String<A>`, and release through `*h` or `destroy` requires at most `Alloc` without an allocator argument.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.IntrinsicId.capability, Concrete.ccCheckExpr
- Semantics: none
- Obligation: capability containment over the call graph after specialization (R-0499, R-0510)
- Evidence: tested

**Rationale.** OD154 requires allocating string operations to declare Alloc; the Allocation decision also requires an allocator and conserving failure, and static text allocates nothing. Allocator reset consumes the old allocator with no live allocation or conflicting loan and returns a fresh identity; the discipline is [undecided: see the target Core proposal](decisions.md#core-allocator-identities).



<a id="MEM-8"></a>
### MEM-8. Indexing dereferences at most one reference

**MEM-8@2.** An index `a[i]`, read or written, whose base `a` is a `&[T; N]` or `&mut [T; N]` indexes the array the reference refers to, dereferencing that one reference.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.checkExprCore, Concrete.ccCheckExpr
- Semantics: none
- Obligation: one place representation in every consumer (R-0501, R-0504)
- Evidence: tested

**Rationale.** This is MEM-4's one-hop rule for an index: the place is the array behind the
reference, bounds-checked as MEM-6 states, and a write through `&mut [T; N]` writes the caller's
array (REF-13). TYPE-7's "only an array is indexed" is read with this one dereference. Absorbed
from [KNOWN_HOLES.md](../verification/KNOWN_HOLES.md) C10.

```con accept name=index-through-reference-decided exit=42 pending=R-0555 now=reject:E0108:resolve
fn get(a: &[i64; 3]) -> i64 {
    return a[1];
}

fn set(a: &mut [i64; 3]) -> Unit {
    a[2] = 38;
}

fn example_value() -> i64 {
    let mut arr: [i64; 3] = [1, 2, 3];
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

**MEM-9@2.** An index that the compiler proves always out of bounds is refused in every function, including a `trusted` function or one declaring `Unsafe`.

- Kind: refusal
- Uses: none
- Codes: E0900 (runtime-safety)
- Status: pending R-0555
- Enforced by: Concrete.Report.provenViolationDiagnostics
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** OD118 applies the same proven-violation refusal in trusted and Unsafe functions. The proof criterion is [undecided: see the target Core proposal](decisions.md#core-static-refusal); legacy interpreter behavior remains pinned where recorded.

```con accept name=index-proven-in-bounds exit=42
fn example_value() -> i64 {
    let a: [i64; 3] = [10, 30, 2];
    return a[0] + a[1] + a[2];
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con reject name=index-proven-out-of-bounds-decided code=E0900 stage=runtime-safety pair=index-proven-in-bounds interp-exempt=interp-runs-proven-violations pending=R-0555 now=reject:E0900:runtime-safety
fn example_value() -> i64 {
    let a: [i64; 3] = [10, 30, 2];
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
fn example_value() -> i64 {
    let a: [i64; 3] = [10, 30, 2];
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
## Version history

This appendix is not normative. It records what earlier versions of this chapter's entries said
or why they changed, so that each rationale explains the current version only. The registry,
[ids.tsv](ids.tsv), keeps every version's state and sentence hash.

- MEM-4@2 opened one `Heap` layer implicitly in a field access (a `HeapArray<T>` only directly, not through a reference), without consuming the heap pointer. MEM-4@3 recorded OD30 (2026-10-29) and was retired before it was enforced, as the registry records; MEM-4@4 (spec audit 8, 2026-10-30) states the same decision and names OPEN-36 for the spelling of the explicit form.

- MEM-4@5 (2026-11-03) brings the chapter into line with the recorded owner decisions; MEM-4@4 is superseded by version.
- MEM-5@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; MEM-5@1 is superseded by version.
- MEM-7@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; MEM-7@1 is superseded by version.
- MEM-9@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; MEM-9@1 is superseded by version.

- MEM-8@2 (2026-11-03) supersedes MEM-8@1; pending legacy conformance is explicit.
