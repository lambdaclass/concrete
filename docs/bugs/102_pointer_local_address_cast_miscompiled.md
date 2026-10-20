# Bug 102 — the address of a pointer-typed local is miscompiled

**Status:** OPEN
**Found:** 2026-10-20 by the RocksDB slice 1 groundwork (`docs/platform/ROCKSDB_SLICE1.md`, G2).
**Severity:** silent wrong code in safe-to-write FFI code. `&mut x as *mut *mut T`, where `x` is a
local of pointer type, passes `x`'s **value** instead of its address; the local never gets a stack
slot. An integer local is compiled correctly. RocksDB reports errors through `char** errptr` and
needs a non-NULL slot, so the obvious binding crashed with SIGSEGV.

```con
trusted fn set_through(p: *mut *mut u8, q: *mut u8) {
    *p = q;
}

trusted fn main() -> Int {
    let mut x: u8 = 5;
    let mut err: *mut u8 = 0 as *mut u8;
    set_through(&mut err as *mut *mut u8, &mut x as *mut u8);
    if err == (0 as *mut u8) { return 1; }
    return 0;
}
```

Expected exit 0; compiled, it traps (exit 133). The emitted IR passes the null constant
(`inttoptr i64 0` → a zero-offset GEP) to `set_through` and later compares the original SSA value,
while `x` gets `alloca i8`. The interpreter cannot run the program (`interp: unsupported cast`), so
interpreter-vs-native differential testing does not see it.

**Repair owner:** queue exit 0b. The fix lands with this program as a permanent regression, and
with interpreter support for the cast so the differential gates cover it. The RocksDB binding's
heap out-slot workaround (`new_err_slot` / `take_err_slot`) can then be removed.

## Structural cause

Two design facts, one per symptom. First, `Lower` decides which locals get storage by recognizing
syntactic address-taking shapes per type (the same promotion logic behind 029, 031, 034 and 038),
rather than lowering from an explicit place representation in which `&mut x` is a place whatever
`x`'s type; a pointer-typed local falls outside the recognized shapes, so its address is its value.
Second, the language's meaning is implemented several times and the interpreter implements less of
it than the backend: it refuses the raw-pointer cast, so the differential gates, which compare the
interpreter with the binary, are blind to exactly the operations the backend handles alone
(structural classes "storage and places decided ad hoc in Lower" and "semantics implemented several
times", [STRUCTURAL_CLASSES.md](STRUCTURAL_CLASSES.md)).

## Structural owner

R-0501 (verified-pipeline stage S10, Place MIR): places, address-taking and storage are explicit
Place MIR constructs consumed by lowering, with a per-pass differential against Core's semantics;
R-0502 (S11) makes layout and memory one authority. R-0496 (S7) makes the interpreter Core's
semantics for every construct the checker accepts, so an unsupported cast is a coverage gap the
differential gate counts (R-0491, exit 2c), not a silent skip. The immediate repair stays under
exit 0b.
