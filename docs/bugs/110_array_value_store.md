# Bug 110 — an array value is stored as an aggregate where the SSA value is a pointer

<!-- bug-ledger
summary: an array value held as a pointer to its storage was stored as the aggregate (invalid IR): `a[i] = [3, 4]` elaborated its literal unhinted and Lower coerced it with a whole-array cast (which would have reinterpreted the bytes), and `x as *mut u8` / a newtype wrap of an array spilled the pointer into an array-typed slot; the element type now hints the value and the backend's casts use the storage address; pinned by `regress_110_array_value_store.con`
fixtures: regress_110_array_value_store.con
class: 8
class: 12 — the element-type hint: Check passes it, Elab did not
silent: no
area: Elab — the record's Fix (Elab hints the assigned value); the commit naming it mostly migrated identities
ruling: Violates: PASS-6@1, PASS-9@1
-->

**Status:** FIXED (branch `backend-108-116`, commit f3bba025, follow-up 262d3fe0)
**Found:** 2026-10-20, by the examples-health pass (porting and output-testing every example).
**Severity:** compile failure at LLVM validation.

## Reproducer

`tests/regressions/bug110_array_value_store/repro.con` (the cast shape is `repro_cast.con` beside it):

```concrete
// Bug 110 (witness 1): assigning an array value to an array element stores the
// SSA pointer as if it were the aggregate. Expected: exit 3 (interp prints 3).
// Actual: "'%tN' defined with type 'ptr' but expected '[2 x i64]'".
fn main() -> i32 {
    let mut a: [[i32; 2]; 2] = [[1, 2], [9, 9]];
    a[1] = [3, 4];
    return a[1][0];
}
```

## Observed

Two shapes: assigning an array value to an array element (`a[1] = [3, 4];`) and using an array local cast to a raw pointer (`x as *mut u8` passed to `puts`). Both emit `store [N x T] %tK, ptr ...` where `%tK` is a `ptr`: `'%tK' defined with type 'ptr' but expected '[2 x i64]'` (resp. `'[3 x i8]'`). The interpreter returns 3 for the first shape. The two shapes share the symptom; whether they share a root cause in Lower is not yet established.

## Expected

Store the aggregate (load it first, or memcpy from the pointer).

## Where it showed up

examples/snippets/arrays.con and examples/snippets/hello_world_array.con, manifest `known-bug 110`.

## Cause

The two shapes have different roots, and the fix found a third shape of the second.

1. `a[1] = [3, 4]`: `Elab` elaborated the right-hand side of an array-element assignment with no
   type hint (`Concrete/Elab/Elab.lean`, `.arrayIndexAssign`), unlike a field or deref assignment and
   unlike `Check`, which passes the element type. The literal became `[Int; 2]`, and `Lower`'s
   `storeToPlace` coerced it to `[i32; 2]` with a whole-array `cast`. EmitSSA's fallback for a cast
   between different aggregate types stores the operand into a slot of the source type and reloads
   it as the target: invalid IR here because the array operand is a pointer, and a byte
   reinterpretation (3, 0 rather than 3, 4) had it been accepted.
2. `x as *mut u8`: EmitSSA's aggregate-to-pointer cast spilled the operand into a fresh slot with
   `store [3 x i8] %x`, but an array SSA value is the pointer to its storage (`isKnownPtr`). The
   same assumption sat in the same-LLVM-type aggregate cast, reached by a newtype wrap of an array
   (`Buf(x)` for `newtype Buf = [i32; 3]`), which failed the same way.

## Fix

`Elab` hints the assigned value with the element type, resolved through one ref/ptr/heap layer as
for `.arrayIndex` reads, so no whole-array cast is generated for the assignment. In EmitSSA's
`.cast`, an aggregate operand that is a known pointer is not stored as an aggregate: a cast to a
pointer yields the storage address, and a same-LLVM-type rebrand copies the storage (`memcpy`)
into a fresh slot, keeping the copy semantics of the old path. The Codex review found a fourth
shape: a newtype over an array arriving at a `match` merge or a loop header was phi-transported as
an aggregate (SSA verification refuses it, E0714), because `Lower.isAggregateForPromotion` did not
see through newtypes; it now resolves the newtype first, so such values go through a stable slot
like the array itself. Probed alongside: returns, call
arguments, struct literal fields, whole-array assignment, `if`-expression arms and `*r = [..]` are
all hinted already. Permanent leg: `tests/programs/regress_110_array_value_store.con` in
`run_tests.sh` (run_ok 42: both elements of the assigned row, the same through `&mut`, a `u8`
row, a pointer read through `x as *mut u8` in main and in a callee, a newtype wrap, and the
newtype at a `match` merge and through a loop header, each element read back). The
reproducer `repro.con` exits 3 natively and under `--interp`; the cast shapes are native-only (the
interpreter refuses them: "unsupported cast"). examples/snippets/arrays.con and
hello_world_array.con are promoted to `compile` with `@run` rules.

**Identity consequence.** The element-type hint changes the elaborated Core of every function that
assigns an integer literal to a narrower array element: the literal is now typed with the element
type rather than `Int` plus a Lower coercion. In the frozen identity corpus this moves
`examples/hmac_sha256`'s `abc_block` and `sha256_hash` implementation digests (and the dependency and
reference rows that carry `sha256_hash`'s). Every such literal fits its element type, so the stored
values are unchanged: reverting only the hint restores both digests and every frozen row byte for
byte, and the post-cleanup SSA of both functions is identical to main's with register names erased.
The rows are re-frozen with explained amendments in `docs/verification/identity-rows-amendments.tsv`,
and `Concrete/Proof/GeneratedAttestations.lean` is regenerated. No program's meaning changes: a literal that does
not fit (`b[0] = 300;` on `[u8; N]`) was already refused by Check, which passes the element type
(E0227).

Not repaired: EmitSSA's fallback for a cast between two different aggregate types still
reinterprets bytes. Nothing on the checked surface produces one after this fix (CoreCheck admits no
user cast between distinct array types), but a future Lower coercion of an aggregate would reach it.

## Structural cause

Lower decides ad hoc whether an array value is an SSA aggregate or a pointer to storage, and the store it emits assumes the other one. This is storage and places decided per construct in Lower ([structural class](STRUCTURAL_CLASSES.md) 8, with 078, 095, 102, 105). Confirmed on the fix: the emitter learns whether an aggregate is a value or a pointer from a by-name register set (`ptrRegs`), and each cast branch had to remember to ask. Shape 1 adds class 12: the element-type hint for an element assignment was computed by `Check` and not by `Elab`, the elaborator that decides the literal's type.

## Structural owner

R-0501 (S10 Place MIR: every aggregate has one storage representation and stores go through places), with R-0504 (S12 validators).

