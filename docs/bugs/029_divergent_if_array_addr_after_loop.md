# Bug 029: early-return `if` + `while` + post-loop array address-of miscompiles

**Status:** Fixed (2026-07-14) — both sites; trust battery green
**Fixed in:** Lower.lean if-merge (`if.load.`) + loop-exit un-promotion (`unpro.`,
for & while variants): `.array` types rebind their merge/promotion ALLOCA ADDRESS
(the arrayLit convention) instead of a by-value load. EmitSSA's store path already
memcpys address-form aggregates, so the stores were correct all along.
**Regression tests:** `tests/programs/regress_029_if_merge_array_addr.con` (42) +
`regress_029_loop_exit_array_addr.con` (7) — one per site.
**Battery:** ssa-verify-agreement 5/0, differential fuzz 450/0 (depths 3+4),
golden 54/0, fast 1637/0, examples 131/0, oracle 70/0.
**Discovered:** 2026-07-14
**Discovered in:** Phase 7 14b — `std.fmt.write_int` stack-buffer rendering
**Minimal repro:** below (front-end accepts; `llvm-as` rejects the emitted IR)

## Symptom

A function containing (1) an `if` whose body RETURNS early, (2) any `while`
loop after it, and (3) `&arr as *const u8` on a local ARRAY after the loop,
emits IR where the array local is reloaded as a VALUE (`[N x i8]`) in a
position expecting its ADDRESS (`ptr`):

```text
llvm-as: error: '%if.load.53' defined with type '[20 x i8]' but expected 'ptr'
```

All three ingredients are required: drop the early-return `if` (probe G), drop
the `while` (H), or use an `if/else` with no early return (K) and the program
compiles and runs correctly. This is a new member of the divergent-branch
wrong-type-slot family (cf. the H7-era through-ref/void-slot cluster): the
post-`if` merge path re-materializes the array local via a load where the
address should flow.

## Minimal repro (front-end green, llvm-as red)

```concrete pseudocode
mod m {
  trusted fn take(p: *const u8, n: u64) -> u64 { return n; }
  trusted fn f(value: i64) -> u64 {
    let buf: [u8; 4] = [0; 4];
    if value == 0 { let z: u8 = 48; return take(&z as *const u8, 1); }
    let mut v: i64 = value;
    while v != 0 { v = v / 10; }
    return take(&buf as *const u8, 4);
  }
  fn main() -> Int { return f(42) as Int; }
}
```

## Workaround (K-shape)

Replace the early-return `if` with an `if/else` that assigns and falls
through; the same loop + array address-of then compiles and runs correctly.
`std.fmt.write_int`/`write_uint` use this shape.

## Secondary finding — SPLIT OUT to bug 030

The front-end accepted `buf[i] = …` on a NON-`mut` array binding inside a
`trusted fn` (probe D) — now tracked as `docs/bugs/030_nonmut_array_write.md`.

## Root cause (located, 2026-07-14)

Two clashing conventions:
- Arrays are bound to their ALLOCA POINTER everywhere: `Lower.lean:1198`
  (`arrayLit` — "Return alloca pointer directly (don't load) so mutations
  work") and `addrOfLocal` (`:476`) assumes it ("arrays are already
  stack-allocated" → returns none, borrow uses the binding as the address).
- The if-merge aggregate path (`Lower.lean:1702-1718`) VIOLATES it: it creates
  an `if.merge.` alloca, `insertStoreBeforeTerm` of each incoming value, then
  a `if.load.` BY-VALUE load and `setVar name (.reg loadReg ty)` — the array
  var is now a `[N x T]` VALUE register. The later `&buf` uses that value
  where its address should flow → llvm-as `[20 x i8] but expected 'ptr'`.

Additionally the store-back half is wrong for arrays even before the load:
each incoming `v` for an array is itself an ADDRESS (branch alloca pointer),
so `store v -> if.merge alloca of [N x T]` stores a pointer into an array
slot. The merge path was designed for by-value aggregates (structs) and is
type-confused for pointer-bound arrays end to end.

## Fix direction (needs a dedicated arc — EmitSSA type model involved)

For `.array` types the merge must keep POINTER identity: either (a) phi over
the incoming ALLOCA POINTERS (requires the phi/SVal to be rendered `ptr`, not
`[N x T]`, in EmitSSA — check how `.reg r ty`-as-address renders in phi
position), or (b) memcpy both branch values into one stable pre-if alloca and
never rebind. Validate with check_ssa_verify_agreement + differential fuzz +
the probes in this file (G/H/I/K + D non-mut-write). Do NOT ship a partial
fix without those batteries: this is the through-ref/wrong-type-slot family.
