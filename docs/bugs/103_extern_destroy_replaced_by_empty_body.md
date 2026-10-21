# Bug 103 — an extern whose name ends in `_destroy` is replaced by an empty body

**Status:** FIXED 2026-10-18 by `0c507fb9` (bug 052 follow-up, "destructor dispatch is typed, never a name lookup"), on main through `13e92ee9`; verified and recorded 2026-10-21 (branch `symbols`)
**Found:** 2026-10-20 by the RocksDB slice 1 groundwork (`docs/platform/ROCKSDB_SLICE1.md`, G3).
**Severity:** a silent leak of every foreign resource released through a `*_destroy` function, and
a foreign symbol silently redefined by the compiler. `Concrete/IR/Mono.lean` (H18 drop glue)
synthesizes an empty no-op definition for any call it cannot resolve whose name ends in `_destroy`,
on the stated assumption that no extern or builtin has such a name. C libraries routinely do
(`rocksdb_options_destroy`, `pthread_mutex_destroy`, `posix_spawnattr_destroy`).

```con
extern fn widget_create() with(Alloc) -> *mut u8;
extern fn widget_destroy(w: *mut u8) with(Alloc);

trusted fn main() with(Alloc) -> Int {
    let w: *mut u8 = widget_create();
    widget_destroy(w);
    return 0;
}
```

`--emit-llvm` shows `declare ptr @widget_create()` but `define void @widget_destroy(ptr %x0)` with an
empty body. In the RocksDB example the release observer records the options, write-options and
read-options objects acquired once and released zero times, although the program calls each
`destroy`. There is no link-name attribute. A function-pointer local with explicit `Unsafe`
capability bypasses the faulty synthesis (Mono leaves indirect calls unchanged); ordinary direct
calls remain broken.

**Repair owner:** queue exit 0b. Restrict the synthesis to destroy calls the drop-glue rewrite itself
produced (by identity, not by name suffix) and never shadow a declared extern; regression: this
program, checked by `--emit-llvm` (the extern stays a `declare`) and by a linked C stub that counts
the call.

## Fix (verified 2026-10-21)

The defect was already gone from main when it was filed: the record was written from the RocksDB
groundwork branch, whose base predates `0c507fb9`. That commit (merged to main by `13e92ee9` on
2026-10-18; the filing commit `b195bc24` of 2026-10-20 does not contain it) deleted the
`fn.endsWith "_destroy"` branch of `Mono.monoExpr` together with the empty-body synthesis. Since
then destruction keys on identity, never on a name suffix:

- Elab emits a `Destroy`-bounded `x.destroy()` as the reserved callee `destroyDispatchCallee`
  (`$destroy`), which no source identifier can spell; no other call is ever treated as a destructor.
- `Mono.monoDestroyDispatch` decides the destructor from the argument's TYPE: the resolved
  definition of that type's `Destroy` impl (`destroyImpls`, keyed by the impl's definition), a no-op
  only for a Copy type, element-wise glue for an array, and E0814 for anything else.
- Synthesized glue is named `destroyGlueName` (`$destroy$<type>`), also unspellable, so a glue body
  can never stand in for an extern or a user function.

An unresolved call named `*_destroy` is an ordinary call: an `extern fn` stays a `declare` and
reaches C. Verified on this branch: the reproducer's IR has `declare void @widget_destroy(ptr)` and
no `define`, and `check_rocksdb_slice1.sh` inside the devshell passes 33/33, including leg B2 (every
foreign declaration stays a `declare`) and the release observer's exactly-once legs for the three
options objects. The one remaining `_destroy` suffix test, `ProofCore.isFreeCall`, classifies a
call for the proof model's allocation accounting and synthesizes nothing.

Symbol mangling (FFI.md "Symbol Names", the bug 129 fix) adds the second half of the decision's
rule 4: every Concrete-defined function, generated glue included, now lives in a namespace disjoint
from C, so even a future generated definition could not take a foreign symbol's name.

Regression: `tests/programs/regress_103_extern_destroy.con` with its C stub
`regress_103_extern_destroy.c` (run_tests.sh codegen section): the IR has exactly one
`declare void @widget_destroy` and no `define`, and the linked binary exits 42, which needs both
destroy calls to reach C (the bug's empty body exits 40).

## Structural cause

Destruction is dispatched by a synthesized NAME (`<Type>_destroy`), resolved late by string lookup,
and an unresolved name falls back to a default (an empty body) instead of a refusal. Two design facts
compose: identity by spelled name rather than a resolved definition (an extern and a generated
destructor are indistinguishable by name), and absence read as a fact (no definition found is
treated as nothing to do). Bugs 052, 099 and 100 and 091's nested-module `Destroy` case are the same
class ([STRUCTURAL_CLASSES.md](STRUCTURAL_CLASSES.md), "destruction dispatched by synthesized
names").

## Structural owner

R-0510 (verified-pipeline stage S8a, checked specialization): a destroy call resolves to the
`DefId` of the exact `Destroy` impl instance, and a missing or substituted instance is rejected by
the specialization validator rather than synthesized; R-0492 (S5) supplies `DefId`s that keep
externs, intrinsics and generated definitions distinct, with no `String → identity` coercion. The
immediate repair stays under exit 0b.
