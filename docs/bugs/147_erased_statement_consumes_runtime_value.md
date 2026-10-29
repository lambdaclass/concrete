# Bug 147 — an erased proof statement could move a runtime value (the move never ran)

<!-- bug-ledger
summary: a move inside `assert`, `assume` or `ghost let` (for example `assert(*h == 42)`) discharged the value's obligation in the checker, but those statements are erased, so the release never ran (found by Codex reviewing bug 146). E0243. `error_147_*.con`, `regress_147_assert_reads.con`. Joins exit 0b. ((`safety-eval2`))
fixtures: error_147_assert_consumes_heap.con error_147_ghost_let_consumes_heap.con error_147_assert_revives_binding.con regress_147_assert_reads.con regress_147_ghost_consumes_ghost.con
class: 15 — an erased statement counted as a move
silent: no
exit: 0b
ruling: Violates: OWN-11@1
-->

**Status:** FIXED 2026-10-22 (branch `safety-eval2`)
**Found:** 2026-10-22, by the Codex review of bug 146 (once `*h` frees its cell, a `*h` inside an
erased statement is a release that never happens).
**Severity:** skipped release in safe code: the checker counted a move inside `assert`, `assume` or
`ghost let` as discharging the value's obligation, and those statements are erased before Core.

## Reproducer

`tests/regressions/bug147_erased_statement_consumes/repro.con`:

```con
fn main() with(Alloc) -> Int {
    let h: Heap<Int> = alloc(42);
    assert(*h == 42);
    return 0;
}
```

On main (`0408f4d4`): accepted; the emitted SSA is a `malloc`, a store and `ret 0` (no free). The same
holds for `ghost let x: Int = *h;`. Fixed: refused at the checker, natively and under `--interp`:

```
error[check]: (E0243) a proof-only statement (assert, assume, ghost let) may not move or consume 'h': it is erased before code generation, so the move would never happen
```

## Cause

`Concrete/Check/Check.lean` checked `assert`/`assume` conditions and `ghost let` initializers like
runtime expressions, so a move inside them changed ownership state; Elab then erased the
statement.

## Fix

`refuseErasedConsumption`: after checking an `assert`/`assume` condition or a `ghost let`
initializer, any change of a non-Copy runtime binding's ownership state is E0243
(`erasedConsumesRuntime`): a move or reservation, a new defer pin, and also the reverse, an
assignment that makes a consumed binding live again (Codex review: `free(h); assert(if true { h =
alloc(43); true } else { ... }); free(h);` released the first cell twice). Reads and borrows are
unaffected, and a `ghost let` binding (`VarInfo.isGhost`) may be consumed by proof-only code, since
no runtime obligation is involved. Fixtures `error_147_assert_consumes_heap.con`,
`error_147_ghost_let_consumes_heap.con`, `error_147_assert_revives_binding.con`, and the positive
controls `regress_147_assert_reads.con` and `regress_147_ghost_consumes_ghost.con`. Migration: 0
sites.

## Structural cause

Ownership state not modeled along control-flow paths ([structural class](STRUCTURAL_CLASSES.md) 15):
erasure removes an operation the checker had counted as a consumption.

## Structural owner

Containment: ROADMAP exit 0b. Structurally R-0499 (S8): the Core checker sees only runtime code, so
proof-only expressions must be checked in a read-only mode before erasure.
