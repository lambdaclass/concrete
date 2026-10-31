# Bug 346 — a diverging abort() bound to a typed let that is returned, or returned directly, emits ret void in a function with a result and fails LLVM validation

**Status:** OPEN
**Found:** \1, by a hand probe of the builtins catalog of the spec's readability pass 2 (the `abort` row), native against `--interp`; confirmed by Codex and Grok
**Severity:** loud: a program Check accepts fails to build natively in LLVM IR validation (`ret void` in a function with a result), with no Concrete diagnostic

<!-- bug-ledger
summary: a diverging abort() bound to a typed let that is returned, or returned directly, emits ret void in a function with a result and fails LLVM validation
coverage: reproducer tests/regressions/bug346_a_diverging_abort_bound_to_a_typed_let_that_is_r/repro.con, pinned by check_example_outputs.sh (open-bug reproducers leg, compile-fails on the LLVM validation error)
class: 8
silent: no
ruling: Violates: PASS-12@1
-->

## Reproducer

`tests/regressions/bug346_a_diverging_abort_bound_to_a_typed_let_that_is_r/repro.con`: `fn main() with(Process) -> u8 { let y: u8 = abort(); return y; }`.

## Observed

The native build stops in LLVM IR validation: `llvm-as: ... error: value doesn't match function result type 'i8'` at `ret void`. `fn f() with(Process) -> Int { let y: Int = abort(); return y; }` and `fn f() with(Process) -> Int { return abort(); }` fail the same way with `'i64'`. `abort();` as a statement followed by `return 0;` compiles and exits with status 134. `--interp` stops on every one of these with `interp: undefined function 'abort'`, a separate gap the spec's statements chapter already lists.

## Expected

[PASS-12@1](../spec/pipeline.md#PASS-12): a program every rule accepts is compiled, and no stage after Check emits LLVM IR that fails validation. Check gives `abort()` a diverging type, which fits any position other than a condition (TYPE-4), so the program is accepted and must compile to a binary that aborts.

## Cause

Elab gives `abort()` the Core type `.never` (`Concrete/Elab/Elab.lean`, the `intrinsic == some .abort` intercept). Lowering (`Concrete/IR/Lower.lean`) emits the call and then the function's return with the call's value, which has no register, so EmitSSA writes `ret void` in a function whose result is `i8` or `i64`; nothing ends the block with `unreachable` after a call that cannot return.

## Structural cause

Class 8 ([STRUCTURAL_CLASSES.md](STRUCTURAL_CLASSES.md)): control flow is decided ad hoc in `Lower`: a diverging call is lowered as an ordinary call whose value is then used, instead of as a terminator, so the type `.never` that Check and Elab agree on has no counterpart in the lowered control flow.

## Structural owner

R-0501 (S10 Place MIR): divergence becomes a terminator of the lowered control-flow graph, so no value of a diverging call is ever used.

## Containment while open

`check_example_outputs.sh` (open-bug reproducers leg) pins the LLVM validation failure. The defect is loud; meanwhile `abort();` is written as a statement.
