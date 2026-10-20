# Bug 103 — an extern whose name ends in `_destroy` is replaced by an empty body

**Status:** OPEN
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
