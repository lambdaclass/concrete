# Bug 072 — a child that fails to exec flushes the parent's stdio buffers

**Status:** FIXED 2026-10-01
**Found:** 2026-09-30, by the R-0484 construction/caller audit
(`docs/language/HANDLE_CAPABILITIES_AUDIT.md`, finding F10), while auditing `fork`.
**Severity:** silent data duplication, reachable from ordinary user code through
`std.process.spawn` whenever the command fails to exec. Not a memory-safety defect.

## Symptom

A program writes through a buffered `FILE*` (`fs.File`, `io.TextFile`, or a file
`Writer`), does not flush, and calls `spawn` with a command that does not exist. The
pending output reaches its destination twice.

Measured: one byte written through `fs.File::write_bytes`, then a failed `spawn`, then
`close` — the file held `XX`.

## Mechanism

`spawn` (`std/src/process.con`) forks. The child calls `execvp`; if exec fails, the
child terminated with `exit(127)`. `exit` runs `atexit` handlers and flushes every stdio
stream — and the child's stdio buffers are copies of the parent's, made by `fork`. The
parent later flushes its own copy, so the same bytes are written by both processes.

POSIX's remedy for a forked child that does not exec is `_exit`, which terminates
without running handlers or flushing.

## Fixed

`std/src/libc.con` binds `_exit`, and `spawn`'s exec-failure path calls it (imported as
`libc_exit_now`). The user-facing `process_exit` keeps normal `exit` semantics: it runs
`atexit` handlers and flushes stdio. Those buffers are not necessarily exclusive to the
calling process — called in a forked child, for example after `process_fork`, it can
still flush buffers inherited from the parent. That case belongs to F9, not this fix.

Not fixed here, and recorded in the audit (F9): `process_fork` returns to arbitrary
Concrete code in the child, which still inherits every owning handle and buffered
stream. That is a design question for R-0484 slice 3, not this bug.

## Regression witness

`tests/regressions/spawn_exit/exec_failure_no_double_flush/` writes one buffered byte,
spawns a nonexistent command, waits, and closes. `scripts/tests/check_spawn_exit.sh`
asserts the child exited 127 (so the exec-failure path ran) and that the file holds
exactly `X`. Run by `run_tests.sh` and CI.

The gate was shown to bite: with the fix reverted it fails on `XX`.
