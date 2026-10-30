# Bug 340 — The interpreter runs a program the compiler refuses as a proven runtime-safety violation (E0900)

**Status:** OPEN
**Found:** 2026-10-30, by the outside-reader test of the specification (60 programs written from SPEC.md alone on wip/spec-phase2 d21a4cfd, run native and under `--interp` on main 71e3e804 and on d21a4cfd).
**Severity:** silent: a program the compiler refuses with E0900 (an index proven out of bounds) runs under `--interp`; when the violating index is on a path that does not execute, `--interp` prints the program's result and exits 0, so the interpreter accepts a program the language refuses.

Codex confirmation 2026-10-30: REAL-BUG. "MEM-6 requires that 'an index the compiler proves out of bounds is refused,' and the compiler's E0900 demonstrates that proof for this program. The overview additionally states that '`--interp` runs no program the compiler cannot compile,' directly ruling out the observed execution followed by a trap. PASS-13 alone would be insufficient here because it speaks of accepted programs."

<!-- bug-ledger
summary: The interpreter runs a program the compiler refuses as a proven runtime-safety violation (E0900)
coverage: reproducer tests/regressions/bug340_the_interpreter_runs_a_program_the_compiler_refu/repro.con, found by the outside-reader spec test 2026-10-30; the native refusal pinned by check_example_outputs.sh (open-bug reproducers leg, compile-fails E0900)
class: 18
silent: yes
exit: 0b
ruling: Violates: MEM-6@1
-->

## Reproducer

`tests/regressions/bug340_the_interpreter_runs_a_program_the_compiler_refu/repro.con`:

```con
fn main() -> Int {
    let a: [Int; 3] = [1, 2, 3];
    let c: bool = false;
    if c {
        return a[3];
    }
    return 42;
}
```

## Observed

`concrete repro.con` refuses it: "error[runtime-safety]: (E0900) proven runtime-safety violation:
a[3] is always out of bounds for array size 3". `concrete repro.con --interp` prints 42 and exits
0. With the index on the executed path (`return a[3];` as the body), `--interp` runs it and traps
("interp: array index 3 out of bounds (size 3)", exit 1) instead of refusing it.

## Expected

[MEM-6@1](../spec/memory.md#MEM-6): "an index the compiler proves out of bounds is refused", and
the specification's overview commits that "`--interp` runs no program the compiler cannot
compile". Both consumers refuse the program with E0900 before it runs.

## Cause

`Main.lean`: `compileSSA` and the other two compile paths call
`enforceProvenRuntimeViolations` (`Report.provenViolationDiagnostics`) after
`Pipeline.runFrontend`, but `interpFrontend`, which builds the `ValidatedCore` that `--interp`
runs, does not. The refusal is a per-driver call outside the front end, so a driver that omits it
accepts the program.

## Structural cause

Class 18 (pipeline artifacts not bound to their payload): acceptance is not a property of the
`ValidatedCore` both consumers take; each driver re-assembles its own list of acceptance checks
after the front end. Bug 088 was the same shape (the post-mono boundary was missing from
`interpFrontend` until its fix added it there by hand).

## Structural owner

R-0497 (S5 sealed, payload-bound artifacts): a `ValidatedCore` is produced only after every
refusal the language states, E0900 included, so the interpreter and the backends cannot consume an
artifact that skipped one.

## Containment while open

check_example_outputs.sh's open-bug reproducers leg pins the native refusal (E0900). Until the
fix, a passing `--interp` run is not evidence that a program is accepted: compile it too.
