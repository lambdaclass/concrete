# Bug 191: test programs and std tests wrote fixed shared `/tmp` paths

<!-- bug-ledger
summary: test programs (`file_write_read.con`, `file_read_basic.con`) and nine std fs/io tests wrote fixed `/tmp` paths, so on a shared host every user but the first got NULL from `fopen` (the programs segfaulted, rc 139; `concrete test` failed) and concurrent runs shared files. The runner now owns the scratch location: `concrete test` / `--test` (`Main.runTestBinary`), `run_tests.sh` and `test_ssa.sh` set `CONCRETE_TEST_TMPDIR` to a directory private to the run, std tests use `std.test.scratch_path` (no fallback), and the programs check `fopen` (exit 100/101/102). Pinned by the `run_tests.sh` regress_191 legs and a `check_gate_hygiene.sh` leg refusing `/tmp` literals in every `.con` source
fixtures: file_write_read.con file_read_basic.con regress_191_test_scratch.con
class: 16 — test scratch files at fixed `/tmp` names, owned by the first user on a shared host
silent: no
-->

**Status:** FIXED (branch `train3-tmp-paths`, for landing train 3)
**Found:** 2026-10-23, when train 3's pre-push hook ran as user `agent` on the OVH server
(`wip/train3` at ee1fff8e): `test_ssa`, `check_native_libs` and `run_tests` failed there and passed
in CI and for user `dev`.
**Severity:** loud. A valid program crashed (SIGSEGV, rc 139) or a std test failed, for every user
except the one who ran the suite first on the host, and concurrent runs could read or truncate each
other's file. No wrong result was reported as a pass.

## Reproducer

On a host where `/tmp/concrete_test_io.txt` belongs to another user (on OVH it belongs to `dev`),
as user `agent`:

```
D=$(mktemp -d)
concrete tests/programs/file_write_read.con -o $D/fwr && $D/fwr; echo rc=$?
```

prints `rc=139`. The gates reported:

- `test_ssa.sh`: `FAIL file_write_read: expected='5' rc='auto' got rc=139` and the same for
  `file_read_basic`;
- `check_native_libs.sh`: `FAIL concrete test: rc=1` (the test binary of a package runs std's
  tests too, and `std.fs` / `std.io` could not open their files);
- `run_tests.sh`: the same programs and the std module tests.

Expected: the programs return 5 and 12 and the std tests pass, for any user and any number of
concurrent runs.

## Cause

Test code named its scratch files with fixed absolute paths:

- `tests/programs/file_write_read.con` wrote `/tmp/concrete_test_io.txt`, and
  `file_read_basic.con` wrote `/tmp/concrete_test_read.txt`;
- eight `#[test]` functions in `std/src/fs.con` wrote `/tmp/concrete_fs_test_*.txt`, and
  `std/src/io.con` wrote `/tmp/concrete_io_test_close_ok.txt`;
- `examples/verify/bench.sh` wrote `/tmp/conhash_bench_10mb.bin`, and the `json`, `mal` and
  `policy_engine` bench scripts wrote their IR and binaries to fixed `/tmp` names.

`/tmp` is shared by every user and every run on the host, and it is sticky: a file created by `dev`
cannot be opened for writing by `agent`. `fopen(path, "w")` returned NULL, and the two programs
passed that NULL to `fwrite`, `fseek` and `ftell` without checking it, so the environment error
became a segfault that hid its cause. The std helpers did check (`FsError::OpenFailed`), so those
tests failed rather than crashed. Two concurrent runs by one user share the same files as well.

Train 2 had added a gate-hygiene leg refusing fixed `/tmp` paths in gate scripts (R-0546). The
programs those scripts run were not covered.

## Fix

The test runner owns the scratch location, and test code asks for it.

- **The variable.** `CONCRETE_TEST_TMPDIR` names a directory private to the run. Test code builds
  every path it writes from it. There is no fallback: when it is unset the test fails (std's
  `scratch_path` returns None; the two programs exit 100).
- **`concrete test` and `--test`.** `Main.runTestBinary`, shared by `compileTest` and
  `compileTestBuild`, runs the test binary with `CONCRETE_TEST_TMPDIR` set to `<dir>/scratch`,
  where `<dir>` is the run's private intermediate directory (bug 179), created with mode 0700 and
  removed with the run. A value inherited from the caller is overridden. The working directory is
  unchanged, so a test can still read fixtures relative to where it was started.
- **std.** `std.test.scratch_path(name)` returns `$CONCRETE_TEST_TMPDIR/name`, or None. The nine
  fs/io tests use it and declare `Env`.
- **Suite harnesses.** `run_tests.sh` exports a per-run default (`$SUITE_TMPDIR/scratch`) and
  narrows it to one directory per `run_ok` / `run_ok_O2` job, so the same program run twice in one
  pass cannot collide. `test_ssa.sh` gives each program its own directory, reused by the timeout
  re-run.
- **The programs.** `file_write_read.con` and `file_read_basic.con` build their path from
  `CONCRETE_TEST_TMPDIR` and check `fopen`: 100 for an unset variable, 101 for a failed open for
  writing, 102 for a failed open for reading. `error_file_no_cap.con`, a compile-error test that
  never opens its path, no longer names `/tmp`. The four `examples/*/bench.sh` scripts use a
  `mktemp -d` directory removed on exit.

## Containment

- `check_gate_hygiene.sh` gains a leg over every tracked `.con` source (about 2000): a `/tmp/...`
  string literal outside a comment (one that starts with `/tmp/` or has it after a separator, so
  `build//tmp/x` is a relative path) is refused unless that whole literal is listed in `CON_TMP_OK`
  with the reason nothing touches the filesystem through it. The scan tokenizes each file as the
  lexer does (char literals, loop labels, strings spanning lines, `//` comments), and a scanner
  failure fails the gate. The one entry is `std/src/path.con`'s `"/tmp/x"`, a
  `PathBuf` display test that never opens the path. An entry whose literal is gone fails the gate.
  Two controls: a program with a `/tmp` literal is flagged, and comments, a doc example, a trailing
  comment and non-`/tmp` paths are not. The pre-fix `std/src/fs.con` gives 8 hits.
- R-0546's gate-script leg now also scans the tracked `examples/*.sh` scripts (the pre-fix
  `examples/mal/bench.sh` gives 9 hits).
- `run_tests.sh` bug 191 legs: `file_write_read.con` exits 100 with the variable unset and 101
  with it pointing at a directory that does not exist (an environment error is reported by its
  exit code, never as a crash); and `regress_191_test_scratch.con` run with `--test` while the
  caller's `CONCRETE_TEST_TMPDIR` is the sentinel `/nonexistent/r191_sentinel`: its test passes
  only if the variable it sees is set, differs from the sentinel and names a directory it can create
  a file in, so the runner, not the caller, provides the scratch directory. Before the fix the
  binary inherited the sentinel and the test fails.
- On OVH as user `agent`, with dev's files still in `/tmp`, the three failing gates pass (see the
  branch report for counts).

## Structural cause

A test's scratch location was derived from a fixed name in a machine-wide directory instead of from
the run that writes it. The code assumed it was the only process, and the only user, touching
those paths. This is the cause of bug 179 one level down: 179's intermediates were named by the
compile's input, 191's scratch files by the test's source. The test programs also turned an
environment error into a crash by not checking `fopen`.

## Structural owner

The test runner. `Main.runTestBinary` provides `CONCRETE_TEST_TMPDIR` for compiled test binaries,
and the suite harnesses (`run_tests.sh`, `test_ssa.sh`) provide it for the programs they run. Test
code obtains paths through `std.test.scratch_path` (or reads the variable directly in programs that
do not use std). `check_gate_hygiene.sh` enforces the rule over every `.con` source, as R-0546's leg
does for gate scripts. Class 16 in [STRUCTURAL_CLASSES.md](STRUCTURAL_CLASSES.md), widened from
compile intermediates to every artifact a run writes and later reads.

## Residual

None known. A test that writes outside the scratch directory through a path computed at run time
(not a literal) is not seen by the scan; the runner-provided directory is the rule such code must
follow.
