# Bug 198: `*p` through a raw pointer duplicated a non-Copy pointee

<!-- bug-ledger
summary: `*p` read by value of a non-Copy pointee through a raw pointer duplicated it (a double free in trusted code). Refused (E0311); the transfer is the intrinsic `read_owned(p)`, which leaves the slot logically uninitialized; 26 std sites migrated. Pinned by `error_rawdup_trusted_deref.con` and `regress_198_read_owned.con` ((T-RAWDUP, the raw-pointer generic leak))
fixtures: error_rawdup_trusted_deref.con regress_198_read_owned.con
class: 13 — `trusted` does not relax linearity, unenforced at `*p`
silent: yes
exit: 0b
ruling: Violates: TRUST-7@1
-->

**Status:** FIXED (branch `soundness-holes`, TIGHTENING_DECISIONS T-RAWDUP)
**Found:** recorded as the "raw-pointer generic leak" (the bug 046 shape; ROADMAP linearity
decision R-0014, merged into R-0514 / V6), reproduced and classified as a HOLE by the
language-tightening probes on 2026-10-23 (TIGHTENING_DECISIONS T-RAWDUP); numbered and fixed the
same day.
**Severity:** memory unsafety in trusted code: SAFETY.md and MEMORY_SEMANTICS §11 state that
`trusted` does not relax linearity, and this shape contradicted them. A double free with no
diagnostic.

Codex review 2026-10-25: NOT CONFIRMED — pending owner decision OD13/exit 2n: TRUST-7’s explicit raw ownership transfer rule remains proposed; a double free inside trusted code alone cannot establish compiler fault.

## Reproducer

`tests/programs/error_rawdup_trusted_deref.con`:

```con
struct Copy B { v: Int }
trusted fn dup<K>(x: &mut K) -> K { let p: *mut K = x as *mut K; return *p; }
fn main() with(Alloc) -> Int {
    let mut h: Heap<B> = alloc(B { v: 42 });
    let h2: Heap<B> = dup::<Heap<B>>(&mut h);
    free(h2);
    free(h);
    return 0;
}
```

On `wip/train3` and main the binary exited 0 after freeing one cell twice; `--interp` on train3
reported a use after free. The non-generic form behaved the same.

## Cause

`Check.lean`'s `.deref` case refused a by-value read of a non-Copy pointee through a REFERENCE
(H11) but returned a raw pointer's pointee unconditionally, so `*p` by value copied a linear value
the slot still owned.

## Structural cause

Class 13 in [STRUCTURAL_CLASSES.md](STRUCTURAL_CLASSES.md): a documented rule (`trusted` does not
relax linearity) with no enforcing site at the one operation that moves a value out of raw memory.

## Structural owner

R-0514 / V6 (the trusted-code proof obligations, R-0509): the formalized trusted subset has exactly
one way to move out of raw memory.

## Fix

`*p` read by value of a non-Copy pointee through a raw pointer is refused (E0311), in trusted and
`Unsafe` code alike; place uses (`(*p).f`, `&*p`), Copy pointees and stores through `*p` (the idiom
for uninitialized slots) are unchanged. The transfer is spelled with the new compiler intrinsic
`read_owned(p)` (`*mut T` or `*const T` to `T`): it moves the value out and leaves the slot
logically UNINITIALIZED, so the caller must not read it again or let it be destroyed until a value
is stored back. It needs trusted or `Unsafe` code (Check, and CoreCheck's raw-operation gate with a
trust edge) and lowers to the same load `*p` did; the interpreter reads the slot.

Migration (`rule_impact.sh` over 2198 units, then iterated on std, which reports one error per
function): 26 sites in std, all moving elements out of collection storage (`vec`, `deque`, `heap`,
`map`, `ordered_map`: `pop`, `swap_remove`, `remove`, rehash and drain paths), rewritten `*p` to
`read_owned(p)`. No test or example site.

## Regression

`error_rawdup_trusted_deref.con` (the reproducer, E0311, `run_err`) and `regress_198_read_owned.con` (a heap cell moved out
with `read_owned`, the slot refilled, both freed once; a pointer spelled through an alias; a Copy
pointee read with `*p`; exit 42, `--interp == binary`). The std tests exercise every migrated site (`run_tests.sh --stdlib`).

Codex review, round 1: the pointee type is resolved through aliases before the load type is taken;
the intrinsic has one spelling (an alias `_read_owned` would have been checked as the intrinsic and
elaborated to a program function named `read_owned`); the interpreter reads a pointer that keeps a
reference path (`&mut x as *mut T`); and `read_owned` counts as a raw operation in the trust and
unsafe reports (`hasRawPtrOpsExpr`).

Round 2: Elab emits the intrinsic as `Callee.runtime "read_owned"`, so its identity travels with the
call and no program function named `read_owned` is taken for it (CoreCheck's raw-operation gate and
the reports' raw-operation detection read the callee form, not the spelling); and bug 199's cast
resolution expands aliases only, so no other cast's Core moves.
