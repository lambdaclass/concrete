# Bug 057: the hardcoded HashMap size undercounts the struct, truncating by-value copies

**Status:** FIXED (2026-07-25, in the R-0003 slice) — `Builtin.hashmapSize` now
matches std's declaration, and `scripts/tests/check_builtin_layout_sizes.sh`
derives the size from `std/src/map.con` and fails if the two ever disagree.
**Discovered:** 2026-07-25, while adding a `tombstones` field for R-0003; the new
field is what made a latent lie observable.
**Provenance:** PRE-EXISTING. The constant said 40 while the struct had been 56
bytes since `hash_fn`/`eq_fn` were added.

## Symptom

Passing a `HashMap` **by value** and then using it **segfaults** (exit 139):

```con
fn consume_and_read(m: HashMap<i32, i32>) with(Alloc, Unsafe) -> Int {
    let k: i32 = 7;
    let got: Option<i32> = m.get(&k);   // <-- SIGSEGV: calls a garbage hash_fn
    ...
}
```

The second, quieter symptom is the one that surfaced it: `HashMap::drop` takes
`self` by value, so after a field was added it read a `cap` that had never been
copied, its destruction loop ran zero times, and every remaining key and value
was leaked — no crash, no diagnostic, just an H18 violation. Two std tests
(`test_map_remove_destroys_key`, `test_map_destroys_entries`) caught it.

## Root cause

`Concrete/Check/Layout.lean` hardcodes layout constants for the builtin generics
that are excluded from struct monomorphization:

```lean
def hashmapSize : Nat := 40  -- ptr + ptr + ptr + i64 + i64
```

The comment enumerates five fields. `std/src/map.con` declares seven at the time
(`keys, values, flags, len, cap, hash_fn, eq_fn`) — the two fn-pointer fields
were never counted, so the constant was 40 where the struct was 56.

Field OFFSETS come from the declaration, but aggregate copies are sized from
this constant, so the two disagreed:

```llvm
%struct.HashMap = type { ptr, ptr, ptr, i64, i64, i64, ptr, ptr }   ; 64 bytes
call void @llvm.memcpy.p0.p0.i64(ptr %agg.0, ptr %self, i64 40, i1 false)
%t3 = getelementptr i8, ptr %ssa.t383, i64 40   ; reads `cap` — never copied
```

Before the new field, everything a by-value path happened to read lived below
offset 40, so the truncation only cost `hash_fn`/`eq_fn` — invisible until a
by-value receiver actually called one. Adding `tombstones` pushed `cap` to
offset 40 and the truncation became load-bearing.

## The fix

`hashmapSize` is now 64, with the field list spelled out next to it, and
`check_builtin_layout_sizes.sh` derives the size from the std declaration and
compares. The gate is verified in both directions: a stale constant fails, and a
field added to std without touching the constant fails. It also refuses to
"pass" if one of these structs gains a sub-64-bit field, since its
8-bytes-per-field derivation would no longer hold.

## The undercount had a test asserting it

`Concrete/Pipeline/PipelineTest.lean` checked `HashMap size == 40` — the same
wrong number, restated a third time. Asserting a hardcoded constant against a
copy of itself verifies only that the plumbing reads the constant; it cannot
notice that the constant is wrong, and here it actively pinned the bug in place.
That is why the new gate derives the size from `std/src/map.con` instead. The
pass-level expectation is now 64 and keeps its narrower job.

## The general hazard

Any hardcoded layout for a type whose declaration lives elsewhere is a
duplicated source of truth that drifts silently, and the drift is only visible
past the truncation point. `vecSize` is the same shape (24, matching `Vec`'s
three fields today) and is now gated too. The durable fix is to derive these
from the declaration rather than restate them; R-0434's payload-write verifier
is the general check that would have caught the read side of this class.
