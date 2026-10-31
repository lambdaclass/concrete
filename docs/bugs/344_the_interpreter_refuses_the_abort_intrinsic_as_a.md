# Bug 344 — The interpreter refuses the abort() intrinsic as an undefined function while the binary aborts

**Status:** OPEN
**Found:** 2026-10-31, by the spec audit's trap probes (one program per trap of ARITH-3, ARITH-5, MEM-6, STMT-2 and STMT-13, native against `--interp`, on main `dee500ee`); confirmed by Codex (real bug, loud, PASS-13@1) and by Grok (real bug, loud, PASS-13@1, on its second round: its first round read an outdated local checkout that has no `evalExtern`, and answered "not a bug" from it)
**Severity:** loud: an accepted program's explicit `abort()` is refused by `--interp` as an undefined function (exit 1), where the binary aborts (SIGABRT, status 134); the interpreter never reports the abort, so a differential run of any program that aborts on purpose fails for the wrong reason

<!-- bug-ledger
summary: The interpreter refuses the abort() intrinsic as an undefined function while the binary aborts
coverage: reproducer tests/regressions/bug344_the_interpreter_refuses_the_abort_intrinsic_as_a/repro.con, pinned by check_example_outputs.sh (open-bug reproducers leg, interp-fails: the binary builds and --interp still says "undefined function 'abort'")
class: 7
silent: no
ruling: Violates: PASS-13@1
-->

## Reproducer

`tests/regressions/bug344_the_interpreter_refuses_the_abort_intrinsic_as_a/repro.con`: `fn main() with(Process) -> u8 { abort(); return 0; }`. The repository's own `tests/programs/abort_basic.con`, which `run_tests.sh` runs natively only (`run_abort`), shows the same refusal under `--interp`.

## Observed

On main `dee500ee`: the binary prints "Aborted" and exits with status **134** (SIGABRT). `--interp` prints `interp: undefined function 'abort'` and exits **1**. Controls: a modelled trap (`7 / 0` in an `i32` function) gives `interp: division by zero`, exit 1, beside the aborting binary; and a program that declares the C function itself (`extern fn abort() with();` called under `with(Unsafe)`) gives `interp: abort() called (the binary aborts)`, exit 1, so the interpreter does model `abort`, only not the intrinsic.

## Expected

[PASS-13@1](../spec/pipeline.md#PASS-13): the interpreter and every compiled profile give an accepted program the behaviour of its checked Core program, with the same traps. The program is accepted and its Core program aborts: [STMT-2@1](../spec/statements.md#STMT-2) (an explicit abort ends the process at once) and [STMT-13@1](../spec/statements.md#STMT-13) (the builtin `abort()`, under `Process`). `--interp` must report that abort, as it reports a modelled trap and as it reports a declared `extern fn abort` ([INTERPRETER_TRUST.md](../verification/INTERPRETER_TRUST.md) §2 lists `abort` as modelled); an "undefined function" refusal misclassifies it.

## Cause

Elab lowers the intrinsic to `CExpr.call "abort" [] [] .never` (`Concrete/Elab/Elab.lean`, the call elaboration: "Intercept abort()"). In `Concrete/Interp/Interp.lean` the call finds no program definition and falls to the builtin and intrinsic cases, which model the String, Vec and print builtins and refuse `clock_monotonic_ns`, but have no `.abort` case; the last arm hands the call to `evalExtern` only when it resolves to a declared foreign function (`externSym` / `resolveExtern`), which the intrinsic is not. The only `"abort"` arm is in `evalExtern`'s table of C functions, so the intrinsic reaches `interp: undefined function 'abort'`.

## Structural cause

Class 7 ([STRUCTURAL_CLASSES.md](STRUCTURAL_CLASSES.md)): semantics implemented several times. The
compiler's intrinsics are dispatched by name separately in Lower/EmitSSA and in the interpreter, and
nothing checks that every intrinsic `resolveIntrinsic` admits has an interpreter case (or an explicit,
documented refusal); `abort` is modelled only as a C function, so the intrinsic of the same meaning
fell through. `check_builtin_semantics.sh` compares the shared builtins, not the intrinsics, and
`run_abort` runs `abort_basic.con` natively only. Nearest instances: bugs 128 and 306 (interpreter
and binary disagreeing on one construct).

## Structural owner

The class-7 owners in [STRUCTURAL_CLASSES.md](STRUCTURAL_CLASSES.md): one semantics for every
consumer, with the interpreter's intrinsic dispatch checked total against `resolveIntrinsic` (each
intrinsic modelled or refused by name with its reason), and the differential gates extended to the
intrinsics, `abort_basic.con` among them (native aborts, `--interp` reports the abort).

## Containment while open

`check_example_outputs.sh` (open-bug reproducers leg, `interp-fails`) pins the refusal. The defect
is loud (no program is accepted with a wrong result), so no containment is required by the
[defect disposition rule](../../ROADMAP.md#the-current-execution-queue). Meanwhile a user runs a
program that aborts on purpose natively, or reads the interpreter's "undefined function 'abort'" as
the abort.
