# Bug 179: concurrent compiles of one source shared the intermediate `<input>.ll`

<!-- bug-ledger
summary: the compiler built an executable through `<input>.ll` (file mode) or `<main>.ll` (project mode), a path shared by every compile of that source, so concurrent compiles to different outputs removed or rewrote it under each other's llvm-as / clang (an uncaught `no such file or directory`); every compile now owns a private temp directory (`privateIntermediate`, `IO.FS.withTempDir`), as do test mode, `concrete run` and the reducer, and `--emit-llvm` is unchanged. Found when a devshell toolchain change exposed the race in `run_tests.sh` (bug 102's `--interp == binary` leg against a `run_ok` worker). Pinned by `regress_179_concurrent_compile.con` and `regress_179_concurrent_build` (concurrent compiles in `run_tests.sh`; 35/40 and 9/12 fail on the previous compiler)
fixtures: regress_179_concurrent_compile.con regress_179_concurrent_build/src/main.con
class: 16 — the intermediate `.ll` at `<input>.ll`, shared by concurrent compiles of one source
silent: no
ruling: Violates: unrecorded
-->

**Status:** FIXED (branch `devshell-toolchain`)
**Found:** 2026-10-22, when a devshell toolchain change (Nix clang ahead of the Lean release's
clang on `PATH`) changed the timing of `scripts/tests/run_tests.sh`, and one of its legs started
failing deterministically on the OVH server.
**Severity:** a valid compile fails (an uncaught `no such file or directory` exception, or an
llvm-as / clang error on a truncated file). As observed this is loud. A silent variant was possible
but not observed: two compiles that write different IR to the same path, for example a project
`concrete build` and a file-mode compile of the same `src/main.con`, could each link the other's
IR.

## Reproducer

```
C=.lake/build/bin/concrete
for i in 1 2 3 4 5 6 7 8; do $C tests/programs/regress_179_concurrent_compile.con -o /tmp/b$i & done; wait
```

Observed on the OVH server, five rounds of eight concurrent compiles: 35 of 40 compiles failed
with

```
uncaught exception: no such file or directory (error code: 2)
  file: tests/programs/regress_179_concurrent_compile.con.ll
```

Expected: every compile succeeds and every binary exits 42. With the fix, 0 of 40 fail.

The witness in the suite was the bug 102 / 105 `--interp == binary` loop in `run_tests.sh`. It
compiles `regress_102_pointer_local_address.con` to `$TESTDIR/.r102_bin` while a background
`run_ok` worker is still compiling the same source (`--emit-llvm`, or `-o` on the clang fallback):
`FAIL regress_102_pointer_local_address: --interp gave '63', binary exit ''`. It passed when run
alone.

## Cause

`Main.lean` built an executable through an intermediate LLVM file whose path came from the input:
`let llPath := inputPath ++ ".ll"` in `compileSSA` (file mode) and `mainPath ++ ".ll"` in
`compileBuild` (project mode). Each compile wrote that file, ran `llvm-as` validation and clang on
it, and removed it. Two compiles of the same source shared one file. The first to finish removed
it while the other's llvm-as or clang was reading it, and `IO.FS.removeFile` in the second then
threw. A compile that started while another was writing could read a truncated file.

The same shape appeared in four more places: test mode (`<input>.test.ll` and the test binary
`<input>.test`), project test mode (`<main>.test.ll`, and a binary at
`/tmp/concrete_test_<monoMs>`), `concrete run` (`/tmp/concrete_run_<monoMs>`) and the reducer's
external predicate (`/tmp/concrete-reduce-<monoMs>.con`). A millisecond clock is shared by every
process on the machine, so these collide under concurrency as well.

## Fix

Every compile writes its intermediates into a directory it creates for itself
(`IO.FS.createTempDir`, through `IO.FS.withTempDir` where nothing has to survive). The helper
`privateIntermediate` in `Main.lean` names a file inside that directory after the source's base
name, so llvm-as and clang diagnostics still name the source.

- `compileSSA` and `compileBuild` use `IO.FS.withTempDir`, so the directory is removed on every
  path: success, a failed validation, a refused native-library declaration, a clang failure, and
  an exception. Before the fix the `.ll` survived a clang failure.
- `compileTest` and `compileTestBuild` keep the directory, as before, when a step fails after
  printing where its artifacts are: failed validation or clang (`LLVM IR left at:`), or a failing
  test binary (`LLVM IR at:`, `Binary at:`). Every other outcome removes it: success, a refused
  native-library declaration, and an exception (`withPrivateDirKeptOnFailure`).
- `concrete run` builds into a private directory and removes it after the program exits.
- The reducer writes each candidate into a private directory.
- `--emit-llvm` is unchanged. It prints the IR on stdout and still writes the `<input>.ll`
  side file (`docs/platform/ROCKSDB_SLICE1.md` calls it a wart; `check_examples.sh` cleans it up).
  No compile reads that file back, so concurrent writes to it cannot break a build.

The regression is two `run_tests.sh` legs, one per build path:

- File mode: `tests/programs/regress_179_concurrent_compile.con`, a `run_ok` leg (exit 42) and
  eight concurrent compiles to distinct outputs, five rounds; every compile must succeed and every
  binary exit 42.
- Project mode: the package `tests/programs/regress_179_concurrent_build`, four concurrent
  `concrete build -o` runs to distinct outputs, three rounds; every binary must exit 0. The
  project-level loop also builds and runs it once.

On the OVH server the unfixed compiler fails 35 of the 40 file-mode compiles and 9 of the 12
project builds; the fixed compiler fails none.

## Structural cause

The location of an intermediate artifact was derived from a shared input (the source path, or a
machine-wide clock) instead of being private to the compile that owns it. The driver treated a
compile as the only process touching the source's directory. Nothing named which compile owns
the `.ll`, so a compile could remove a file another compile was reading. No other recorded bug
had this cause when it was filed; it is the first entry of class 16 in
[STRUCTURAL_CLASSES.md](STRUCTURAL_CLASSES.md). Bug 191 (test scratch files at fixed `/tmp`
names) later joined the class.

## Structural owner

The compile driver in `Main.lean`. Its rule is the docstring of `privateIntermediate`: an
intermediate that a compile writes and later reads lives in a fresh directory owned by that
compile, never at a path derived from the input or a clock. The concurrent-compile leg in
`run_tests.sh` enforces the rule for both build paths. A new driver mode that needs an
intermediate should use `privateIntermediate` inside `IO.FS.withTempDir`. No roadmap stage owns
the driver; this record proposes the rule above as its owner until the driver is restructured.

**Integration note (train 3, 2026-10-23).** Landing train 2 carried a partial fix for the same race
from fast-gates (R-0546, `b5b9dbea`), which put the intermediates at per-process paths beside the
source (`<source>.<pid>.ll`). It kept concurrent builds apart, but it left files in the source
tree and did not clean them up on every failure path. Train 3 keeps this record's private
directory and removes that `scratchPath` helper, so the repository has one mechanism: the
structural owner named above. Train 4 merged fast-gates' later version of that helper (under
`$TMPDIR` with the process id and a clock stamp) and dropped it the same way.
