# Vec Inline Investigation

Status: completed (2026-03-16)

## Problem

The bytecode VM benchmark showed Concrete ~3x slower than C on fib(35):
- Concrete: ~785ms
- C (heap Vec): ~260ms

The Phase H summary attributed this to "bounds-checked `vec_*` calls." Investigation proved this diagnosis was wrong.

## Actual Root Cause

The generated LLVM IR for `vec_get_4`, `vec_set_4`, `vec_push_4`, `vec_pop_4_4`, and `vec_len` were plain `define` functions with no inlining hints. LLVM at `-O2` chose not to inline them, leaving 17 function calls per dispatch iteration in the VM hot loop.

Key finding: **the vec builtins have no bounds checking in the LLVM IR**. `vec_get` is just a GEP+load — 3 instructions. The cost was entirely the function call/return overhead (register save/restore, branch prediction disruption, lost optimization opportunities across call boundaries).

## Evidence

Compiled VM to LLVM IR, manually added `alwaysinline` attribute:

| Version | fib(35) time |
|---|---|
| Concrete (no inline) | ~785ms |
| Concrete (alwaysinline) | ~257ms |
| C (heap Vec, -O2) | ~260ms |
| C (stack array, -O2) | ~372ms |

The stack-array C version is actually *slower* than heap Vec — likely due to large stack frame pressure.

## Fix

Added `alwaysInline : Bool := false` field to `LLVMFnDef` in `LLVM.lean`. Set `alwaysInline := true` for all vec builtins except `vec_free` in `EmitBuiltins.lean`. Emit `alwaysinline` attribute in `printFnDef` in `EmitLLVM.lean`.

### Files changed

- `Concrete/LLVM.lean` — added `alwaysInline` field to `LLVMFnDef`
- `Concrete/EmitLLVM.lean` — emit `alwaysinline` attribute in `printFnDef`
- `Concrete/EmitBuiltins.lean` — set `alwaysInline := true` for `vec_len`, `vec_new`, `vec_push`, `vec_get`, `vec_set`, `vec_pop`

## Per-opcode cost analysis (before fix)

For a binary arithmetic opcode (ADD, SUB, etc.), Concrete's VM executed:

1. `vec_len(&mut stack)` — function call, GEP + load
2. `vec_get(&mut stack, sp-1)` — function call, GEP + load + mul + GEP, then caller loads from ptr
3. `vec_get(&mut stack, sp-2)` — same
4. `vec_pop(&mut stack)` — function call, GEP + load + icmp + branch + sub + store + GEP + load + mul + GEP + alloca Option + memset + store tag + memcpy + load Option
5. `vec_set(&mut stack, sp-2, result)` — function call, GEP + load + mul + GEP + memcpy

That's 5 function calls per arithmetic opcode. After inlining, LLVM can:
- Eliminate the `vec_pop` Option allocation entirely (result is unused)
- Combine redundant GEP/load chains (data pointer loaded once, reused)
- Replace memcpy of 4 bytes with simple load/store
- Keep the length in a register across operations

## Remaining optimization opportunities

1. **`vec_pop` returns `Option<T>` even when the caller discards the result** — the VM calls `vec_pop` just to shrink the stack. With inlining, LLVM eliminates the dead Option, but a `vec_pop_discard` intrinsic would be cleaner.

2. **`vec_get` returns a pointer, caller dereferences** — for scalar types (i32, i64), returning the value directly would save one indirection. With inlining this is optimized away.

3. **`vec_set` uses `memcpy` for i32** — with inlining, LLVM converts to a simple store. Without inlining, it's a real memcpy call for 4 bytes.

All three are now moot with `alwaysinline`, but would matter if we ever needed to support non-inlined vec paths (e.g., polymorphic dispatch, separate compilation).

## Conclusion

The "3x gap to C" was a **codegen quality issue**, not a language design issue. The fix is 3 lines of compiler change. Concrete's collection model (heap Vec with pointer-based access) generates the same quality code as hand-written C when LLVM can see through the abstraction boundary.
