# RocksDB Slice 1 — Groundwork and Gaps

Status: groundwork built 2026-10-20; **not accepted**. The deliverable is defined in the roadmap
under [feature prerequisites](../../ROADMAP.md#feature-prerequisites) (row 5,
[R-0285](../../ROADMAP.md#task-r-0285)) and is blocked by exits 0b and 2i, which are not done.
This page records what exists, how to run it, and the gaps that stand between it and acceptance.
It makes no acceptance claim: `scripts/tests/check_rocksdb_slice1.sh` is the authority, and it
fails today.

## What exists

| piece | path |
|---|---|
| example (application) | `examples/rocksdb_kv/` — `Concrete.toml` declares `rocksdb = { path = "rocksdb" }`; `src/main.con` uses `import rocksdb.{...}` |
| binding package | `examples/rocksdb_kv/rocksdb/src/lib.con` — hand-written declarations of the RocksDB C API and trusted linear wrappers; its `Concrete.toml` declares the C library, `[native] pkg-config = ["rocksdb"]` ([NATIVE_LIBRARIES.md](NATIVE_LIBRARIES.md)) |
| acceptance gate | `scripts/tests/check_rocksdb_slice1.sh` |
| compile-fail fixtures | `tests/regressions/rocksdb_slice1/handle_never_destroyed`, `tests/regressions/rocksdb_slice1/handle_used_after_close` |
| release observer (gate only) | `tests/regressions/rocksdb_slice1/release_observer.c` |
| dependency | `rocksdb` and `pkg-config` in the default devshell (`flake.nix`, revision pinned by `flake.lock`; RocksDB 10.9.1 at the time of writing) |

The example opens an invalid path (a path under a regular file) and expects a typed
`DbError { kind: Open, message }`, opens a real database, writes one key, reads it back, reads an
absent key and releases everything. Every RocksDB resource is a linear struct with exactly one
consuming release: `Options::destroy`, `WriteOptions::destroy`, `ReadOptions::destroy`,
`Db::close`, `Value::free` (a library-owned `rocksdb_get` buffer, released with `rocksdb_free`)
and `DbError::drop` (whose foreign message buffer is copied and released with `rocksdb_free`
before the value exists).

Trust boundary, as the binding states it: the externs are plain `extern fn`, never
`trusted extern`, so only the `trusted` wrappers call them; each extern's declared effects
(`File` for open/read/write/close, `Alloc` for allocation-only calls) and every ownership fact
(who frees which buffer, with which deallocator) are reviewed assumptions taken from `c.h`, not
checked facts. Nothing checks the declarations against `c.h` beyond the observer's width
assertions (`size_t` = `u64`, `unsigned char` = `u8`, 64-bit pointers); signature checking
belongs to R-0283/R-0287.

## Failure and resource contract

R-0285's failure and resource contract asks, for each fallible operation, what success and each
recoverable error mean for ownership, partial progress, observable state and retry. For the three
operations this slice binds:

| operation | on error | ownership after the error | state and retry |
|---|---|---|---|
| `Db::open(&Options, &String)` | `Err(DbError { kind: Open, message })` | `Options` and the path stay caller-owned (borrowed); no `Db` exists, nothing to close; RocksDB's message buffer is released with `rocksdb_free` inside the wrapper | no rollback is promised: RocksDB may already have created directories or files under the path before failing. Retrying is the caller's decision; the wrapper claims nothing about it |
| `Db::put(&self, &WriteOptions, &Bytes, &Bytes)` | `Err(DbError { kind: Put, message })` | the `Db`, options, key and value stay caller-owned; the `Db` must still be closed | no wrapper-level rollback and no safe-retry guarantee: whether the write reached the WAL or memtable is RocksDB's to say, and the C API reports only a status message |
| `Db::get(&self, &ReadOptions, &Bytes)` | `Err(DbError { kind: Get, message })` | as for `put`; a value buffer returned alongside an error (not expected from c.h) is released with `rocksdb_free` | reads have no observable effect to roll back |

Provenance: the ownership facts come from the pinned 10.9.1 `rocksdb/c.h` (its header comment:
on failure `*errptr` is set to a `malloc()`ed NUL-terminated message, and on success it is left
unchanged) and from `rocksdb_free` being the C API's release for buffers it returns. They are reviewed assumptions, not checked facts. Only the open failure is
exercised by the gate: this slice has no independent observable control for a partial `put` or a
retry (gap G7).

## Running it

```sh
nix develop            # provides lake's toolchain, clang, rocksdb and pkg-config
lake build concrete
bash scripts/tests/check_rocksdb_slice1.sh
```

The gate clones HEAD (committed content only) and builds from the clone with the clone's `std`;
the compiler binary is this checkout's, kept fresh by `require_fresh_binary`. Without pkg-config
or without a `rocksdb.pc` it **fails** (exit 1); it never skips. The repository's skip
convention for absent tools (`check_bv_certificates.sh`, `check_multi_kernel.sh`) covers opt-in
prover shells; RocksDB is the dependency this acceptance is about. Outside nix the smallest
reproducible setup is the distribution package that ships `rocksdb.pc`, for example
`apt-get install librocksdb-dev pkg-config` on the Ubuntu CI image. The gate is not wired into
CI yet: `check_gate_registration.sh` lists it as a declared skip until acceptance is unblocked,
and the CI job will need that install step.

Gate result when the groundwork landed (macOS arm64, devshell): 28 legs pass, 5 fail, exactly
G1 (the no-flags build) and G3. Since G1 closed (2026-10-21, `[native]`), measured on a branch
based on main 9d17ccc5 at commit 82c6cd15: 33 pass, 0 fail — the G3 legs pass on that base as well,
although bug 103 is still recorded OPEN, so G3's status belongs to its owner to confirm. Measured
first on the older base: 29 pass, 4 fail. Leg A,
plain `concrete build` in the fresh clone, now produces the binary, so leg B's manual link is not
used and every behaviour leg runs the normally built binary. On the older base the four failures
were exactly G3: the
compiler-defined `_destroy` symbols, and the three options objects the observer reports never
released. Everything else — the typed error, the value read back, the absent key, the database on
disk, exactly-once release of the database, value buffer and error message, the observer's own
mutation control, and both compile-fail fixtures with their repaired controls — passes.

## Gaps

Each gap names what failed, a minimal reproducer and the observed result. None was repaired by the
groundwork, for which compiler sources were out of scope; G1 has since closed, as its entry says.

### G1. No way to declare a native library (blocks "no manual flags") — closed 2026-10-21

**Closed** by the `[native]` section ([NATIVE_LIBRARIES.md](NATIVE_LIBRARIES.md), gate
`scripts/tests/check_native_libs.sh`): the binding package declares `pkg-config = ["rocksdb"]`,
and the driver resolves it and adds the flags to the final link of every application that depends
on the package. Leg A passes. The record below is the gap as found.

`concrete build` links with a fixed clang command (`clangArgs` in `Main.lean`); `Concrete.toml`
has no section for native libraries (`validateToml` knows `[package]`, `[dependencies]`,
`[policy]`, `[profile]`), and no environment variable or attribute adds a library. Every
foreign symbol outside libc is therefore unresolved:

```
$ cd examples/rocksdb_kv && concrete build
Undefined symbols for architecture arm64:
  "_rocksdb_close", referenced from: _Db_close in main-….o
  …
clang compilation failed
```

Needed: the binding package declares its native dependency (at minimum a pkg-config name, later
the R-0285 build recipe and lock entry), and the driver adds the resulting flags to the link of
every application that depends on it. Until then the gate's leg B links `concrete build
--emit-llvm` output with `pkg-config --libs rocksdb` itself. Related wart: `--emit-llvm` leaves
`src/main.con.ll` beside the sources (ignored by `.gitignore`).

### G2. Address of a pointer-typed local is miscompiled (silent)

`&mut x as *mut *mut T`, where `x` is a local of pointer type, passes `x`'s **value** instead of
its address; the local never gets a stack slot. An integer local is fine. RocksDB reports every
error through `char** errptr` and requires that slot to be non-NULL (`SaveError` in `db/c.cc`),
so a binding written the obvious way hands RocksDB a NULL error slot, and error reporting asserts
or dereferences NULL inside RocksDB; the first run of the example crashed with SIGSEGV.

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

Compiled: traps (exit 133); expected exit 0. The emitted IR passes the null constant
(`%t1 = inttoptr i64 0 to ptr` … `call void @set_through(ptr %t2, …)` with `%t2` a zero-offset GEP
of `%t1`) and later compares the original SSA value, while `x` gets `%addr.3 = alloca i8`. The
interpreter cannot run either program (`interp: unsupported cast`), so differential testing does
not see it. Workaround in the binding: the out-slot lives on the heap (`new_err_slot` /
`take_err_slot`). This is a codegen defect in the safe reference-to-pointer cast, the one
pointer-producing cast FFI.md lists as safe; it belongs in the bug ledger with a regression.
Filed as [bug 102](../bugs/102_pointer_local_address_cast_miscompiled.md), FIXED on 2026-10-21:
`&mut err` on a pointer-typed local is now the local's slot, so the out-slot can live on the stack.
The heap workaround stays in the binding until it is rebuilt and run against a RocksDB install.

### G3. Externs ending in `_destroy` are replaced by an empty body (silent leak)

`Concrete/IR/Mono.lean` (H18 drop-glue) synthesizes an empty no-op definition for any call it
cannot resolve whose name ends in `_destroy`, on the stated assumption that "no extern or builtin
ends in `_destroy`". C libraries routinely do (`rocksdb_options_destroy`,
`pthread_mutex_destroy`, `posix_spawnattr_destroy`). The extern's `declare` disappears and the
module **defines** the symbol, so the library's function never runs:

```con
extern fn widget_create() with(Alloc) -> *mut u8;
extern fn widget_destroy(w: *mut u8) with(Alloc);

trusted fn main() with(Alloc) -> Int {
    let w: *mut u8 = widget_create();
    widget_destroy(w);
    return 0;
}
```

`--emit-llvm` shows `declare ptr @widget_create()` but `define void @widget_destroy(ptr %x0)`
with an empty body. In the example, the observer reports `options`, `writeoptions` and
`readoptions` acquired once and released zero times, although the program calls each `destroy`.
There is no link-name attribute. A function-pointer local with explicit `Unsafe` capability
bypasses the faulty synthesis; ordinary direct calls remain broken, and the binding keeps direct
calls so the gate tracks bug 103. The gate fails until the synthesis is restricted to calls the drop-glue rewrite itself produced, or externs are excluded.

### G4. No opaque foreign types

Every handle is `*mut u8` in the extern signatures, so mixing up `rocksdb_options_t*` and
`rocksdb_t*` in the binding is not a type error. The linear wrapper structs keep them apart
above the binding; generated bindings (R-0285) need a distinct opaque pointer type per C type.

### G5. Binding is hand-written

R-0285's acceptance is "dependency, ordinary import, build", with bindings generated from the
header and a reviewed description. Here the declarations, ownership facts and effects are
written by hand and reviewed by reading `c.h`; nothing regenerates or re-checks them when the
library changes. The roadmap's slice 1 text asks for the dependency and import, not generation,
so this does not block slice 1; it is recorded so the hand-written file is not mistaken for the
generated workflow.

### G6. The fresh clone reuses the checkout's compiler

The gate clones the sources but runs this checkout's compiler binary. Building the compiler in
the clone is the fresh-consumer CI case R-0285 schedules after the first slice runs.

### G7. No partial-progress or retry control

The roadmap's failure contract asks for an independent observable control where the selected API
supports partial progress or retry, or a stated limit. RocksDB's `put` can fail after partial work
(WAL written, memtable not), but nothing in this slice injects such a failure, and the C API
gives the caller no way to observe partial progress. The limit is stated in the table above; the
control belongs to the first writer/runtime adapter that can inject a write failure, as the
roadmap allows.

## Acceptance checklist (from the roadmap) and current state

| roadmap requirement | state |
|---|---|
| declare the RocksDB C library as a dependency | done: binding package under `[dependencies]`; the binding declares the C library, `[native] pkg-config = ["rocksdb"]` (G1 closed) |
| import it with a normal Concrete import | done (`import rocksdb.{...}`) |
| open a database, write a key and read it back | done (behaviour legs pass, leg-B binary) |
| invalid path returns a typed error, not an abort | done (`DbError { kind: Open }` with RocksDB's message naming the path) |
| each linear handle destroyed exactly once | `Db`, value buffer and error message: observed once each; the three options objects: never released (G3) |
| builds and runs from a clean checkout, normal commands, no manual flags | builds and runs with plain `concrete build` in the devshell (leg A, G1 closed); the run still leaks the three options objects (G3) |
| compile-fail: handle never destroyed | rejected, E0208 naming `db`; compiles once repaired |
| compile-fail: handle used after close | rejected, E0205 naming `db`; compiles once repaired |
| gate in the full gate set | not wired; declared skip in `check_gate_registration.sh` until 0b and 2i hold |
