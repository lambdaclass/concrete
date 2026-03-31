# Bug 013: Alloca Inside Loops Caused Stack Overflow

Status: fixed

## Symptom

Real parser/benchmark workloads could crash with `SIGSEGV` or stack overflow when codegen emitted `alloca` inside loop bodies.

In LLVM IR, `alloca` inside a loop allocates new stack space every iteration and is not freed until the function returns.

## Root Cause

`EmitSSA.lean` emitted several `alloca` instructions inline in the current block instead of hoisting them to the function entry block.

This affected helper paths such as:

- pass-by-value to pointer materialization
- value-as-pointer wrappers for builtin calls like `vec_push`
- string literal materialization

## Fix

- added `entryAllocas` to `EmitSSAState`
- added an `emitEntryAlloca` helper
- routed all `alloca` emission sites through entry-block hoisting
- prepended hoisted allocas to the generated entry block in function emission

## Effect

Programs that previously crashed at large loop counts or deep recursion now run stably.

## Regression Coverage

- loop/benchmark stress coverage added in `lean_tests/`
- real-world confirmation came from the JSON benchmark workload
