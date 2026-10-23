# Trusted Code and Raw Pointers

This chapter is a stub. It holds the foundation decisions about undefined behaviour and pointer
operations. The rest of the area (`trusted`, `Unsafe`, capabilities and handle capabilities, raw
dereference) migrates here in README §8, step 6.

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
### TRUST-4. Moving a value out of raw memory is explicit

**TRUST-4@1.** In trusted or `Unsafe` code, `*p` through a raw pointer `p` reads by value only a Copy pointee, and a non-Copy value is moved out of raw memory only by `read_owned(p)`, after which the slot is logically uninitialized until a value is stored back.

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
trusted helper could free one heap cell twice. `read_owned(p)` states the transfer. Place uses
(`(*p).f`, `&*p`), Copy pointees and stores through `*p` are unchanged.

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
| `tests/programs/error_rawdup_trusted_deref.con` | `reject:E0311:TRUST-4@1` | `trust4` | `-` |
| `tests/programs/regress_198_read_owned.con` | `run:42` | `trust4` | `binary,interp` |

