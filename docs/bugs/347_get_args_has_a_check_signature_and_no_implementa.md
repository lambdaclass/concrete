# Bug 347 — get_args has a Check signature and no implementation, so a program whose emitted code calls it fails LLVM validation on an undefined @get_args

**Status:** OPEN
**Found:** \1, by a hand probe of the builtins catalog of the spec's readability pass 2 (the `get_args` row), native against `--interp`; confirmed by Codex and Grok
**Severity:** loud: a program Check accepts fails to build natively in LLVM IR validation (`use of undefined value '@get_args'`), even when nothing calls the function that mentions it

<!-- bug-ledger
summary: get_args has a Check signature and no implementation, so a program whose emitted code calls it fails LLVM validation on an undefined @get_args
coverage: reproducer tests/regressions/bug347_get_args_has_a_check_signature_and_no_implementa/repro.con, pinned by check_example_outputs.sh (open-bug reproducers leg, compile-fails on the undefined @get_args)
class: 12
silent: no
ruling: Violates: PASS-12@1
-->

## Reproducer

`tests/regressions/bug347_get_args_has_a_check_signature_and_no_implementa/repro.con`: an uncalled `fn args() with(Process) -> HeapArray<String> { return get_args(); }` beside a `main` that returns 42.

## Observed

The native build stops in LLVM IR validation: `error: use of undefined value '@get_args'` at `%t0 = call ptr @get_args()`. `--interp` prints 42, because it never reaches the call. `get_args` has a Check signature (Concrete/Resolve/BuiltinSigs.lean, result `HeapArray<String>`, capability `Process`) and an `IntrinsicId`, but no definition anywhere: `git grep get_args` finds it only in BuiltinSigs.lean and Intrinsic.lean, with nothing in the backend, the runtime, std or the interpreter. With `Env` in place of `Process`, Check refuses the call (E0520).

## Expected

[PASS-12@1](../spec/pipeline.md#PASS-12): a program every rule accepts is compiled, and no stage after Check emits LLVM IR that fails validation. A builtin Check accepts must have a definition on both consumers, or Check must refuse its name.

## Cause

`builtinFnSigs` (Concrete/Resolve/BuiltinSigs.lean) and `resolveIntrinsic` (Concrete/Resolve/Intrinsic.lean) declare `get_args`, so Check types the call; the backend's builtin definitions (Concrete/Backend/EmitBuiltins.lean) and the interpreter have none, so the emitted module calls an undeclared `@get_args`. Command-line arguments are read through std's `std.args` instead.

## Structural cause

Class 12 ([STRUCTURAL_CLASSES.md](STRUCTURAL_CLASSES.md)): the set of builtins is kept in several places (the signature table, the intrinsic enumeration, the backend's definitions, the interpreter's cases), and nothing checks that each builtin Check accepts has an implementation in each consumer.

## Structural owner

R-0492/R-0514 (one resolver and one builtin table): each builtin is one table entry carrying its signature, capability and implementations, so a builtin without an implementation cannot be resolved.

## Containment while open

`check_example_outputs.sh` (open-bug reproducers leg) pins the LLVM validation failure. The defect is loud; meanwhile programs read their arguments through `std.args`.
