# Arithmetic Policy

Status: stable policy reference (Phase 3 items 60-61)

This document defines Concrete's arithmetic overflow policy as a public language surface. It specifies what happens on integer overflow, division by zero, shift overflow, and narrowing for each profile, and how the active policy is made visible in source, reports, and proof artifacts.

For the profile definitions, see [PROFILES.md](PROFILES.md).
For the current runtime boundary classification (including overflow as UB), see [PREDICTABLE_BOUNDARIES.md](PREDICTABLE_BOUNDARIES.md).
For the proof-vs-runtime integer gap, see [PROOF_SEMANTICS_BOUNDARY.md](PROOF_SEMANTICS_BOUNDARY.md).
For the failure strategy (abort-only, no unwinding), see [FAILURE_STRATEGY.md](FAILURE_STRATEGY.md).

---

## 1. Current State

### Numeric types

The compiler (`AST.lean`) defines these integer types:

| Type | Width | Signedness | LLVM representation |
|------|-------|------------|---------------------|
| `Int` / `i64` | 64-bit | Signed | `i64` |
| `Uint` / `u64` | 64-bit | Unsigned | `i64` (treated as unsigned for comparisons and division) |
| `i32` | 32-bit | Signed | `i32` |
| `u32` | 32-bit | Unsigned | `i32` (treated as unsigned for comparisons and division) |
| `i16` | 16-bit | Signed | `i16` |
| `u16` | 16-bit | Unsigned | `i16` |
| `i8` | 8-bit | Signed | `i8` |
| `u8` | 8-bit | Unsigned | `i8` |

Float types (`Float64`/`f64`, `Float32`/`f32`) exist but are outside the scope of this policy.

### Current overflow behavior

All integer arithmetic today emits plain LLVM `add`, `sub`, `mul` without `nsw` or `nuw` flags. This means:

- **Addition, subtraction, multiplication**: wrap silently in two's complement. No trap, no diagnostic, no signal. LLVM treats the result as a well-defined value.
- **Division, modulo**: the compiler emits `sdiv`/`udiv`/`srem`/`urem` depending on signedness. Division by zero is undefined behavior in LLVM IR. On x86-64 it triggers a hardware SIGFPE. On other architectures it may produce garbage or trap.
- **Shift**: `shl`, `ashr`, `lshr` with shift amount >= bitwidth produce an LLVM poison value, which is undefined behavior if consumed.
- **Negation**: emitted as `sub 0, x` — wraps silently for `Int.MIN`.

### Current interpreter behavior

The source-level interpreter (`Interp.lean`) uses Lean's arbitrary-precision `Int`. It has no overflow and explicitly traps division by zero with `"interp: division by zero"`. This creates a semantic divergence: the interpreter and compiled code disagree on overflow behavior and division-by-zero handling.

### Current proof behavior

The proof model (`Proof.lean`) uses Lean's unbounded `Int`. Theorems proved over `PExpr` hold for mathematical integers but carry an implicit assumption that runtime values stay within representable range. Division and modulo are not modeled in `PBinOp` at all.

### What the pressure tests revealed

The Phase 2 pressure programs expose real arithmetic pain:

- **`pressure_fixcap_controller.con`**: PID controller uses integer-scaled gains (`Kp*error / 10`). The `meas_push` function subtracts the oldest value then adds the new one — if intermediate sums overflow `i32`, the running average silently corrupts. The comment "gains are scaled x10 to avoid needing floats" documents a manual workaround for the lack of overflow visibility.
- **`pressure_fixcap_ring_buffer.con`**: wrap-around index `(r.head + 1) % 16` is intentionally modular arithmetic. The programmer means wrapping. There is no way to distinguish this intent from accidental overflow.
- **`pressure_parse_binary_endian.con`**: shift-and-or byte assembly (`(b0 << 24) | (b1 << 16) | (b2 << 8) | b3`) produces correct bit patterns, but for values above `2^31` the result is a negative `i32`. The comment "for 0xDEADBEEF (> 2^31), we use the raw bit pattern in i32" documents the confusion.
- **`pressure_fixcap_state_machine.con`**: no overflow risk, but the code has no way to assert that.

The pattern is clear: some code intends wrapping, some code would prefer trapping, and nothing in the source distinguishes them.

---

## 2. Arithmetic Modes

Concrete defines three arithmetic modes for integer operations. Every integer operation executes in exactly one mode. The mode is always determinable from the source.

### 2.1 Checked (trap on overflow)

This is the **default mode** for all profiles.

- Addition, subtraction, and multiplication trap (abort) on signed or unsigned overflow.
- The trap is an abort, consistent with Concrete's abort-only failure strategy. Defer does not run (same as OOM abort).
- Implementation: emit LLVM `llvm.sadd.with.overflow` / `llvm.uadd.with.overflow` (and corresponding sub/mul intrinsics), branch to an abort block on the overflow flag.

**Rationale**: silent wrapping is a correctness bug in almost all code. The predictable profile's promise of "no hidden runtime behavior" is violated by silent wrapping — a reviewer reading `a + b` cannot know whether the result is mathematically correct without reasoning about the ranges of `a` and `b`. Trapping makes the failure visible and immediate.

### 2.2 Wrapping (modular arithmetic)

Explicit opt-in for code that intentionally uses two's-complement modular semantics.

- Operations wrap silently in two's complement, exactly as today.
- Available via named functions: `wrapping_add`, `wrapping_sub`, `wrapping_mul`.
- These are compiler intrinsics, not stdlib functions. They emit plain LLVM `add`/`sub`/`mul` without `nsw`/`nuw`.
- Available for all integer types.

**Use cases**: ring buffer index wrap-around, hash functions, checksum computation, cryptographic operations, bit-pattern assembly from byte shifts.

### 2.3 Saturating

Explicit opt-in for code that wants clamped-to-bounds behavior.

- On overflow, the result clamps to the maximum (or minimum) representable value instead of wrapping or trapping.
- Available via named functions: `saturating_add`, `saturating_sub`, `saturating_mul`.
- These are compiler intrinsics. They emit LLVM `llvm.sadd.sat` / `llvm.uadd.sat` (and corresponding sub/mul intrinsics).
- Available for all integer types.

**Use cases**: audio processing, sensor clamping, bounded accumulators (the `pressure_fixcap_controller.con` integral clamping is a manual version of this).

---

## 3. Profile Mapping

| Profile | Default mode | Wrapping available? | Saturating available? |
|---------|-------------|--------------------|-----------------------|
| Safe | Checked | Yes (explicit intrinsics) | Yes (explicit intrinsics) |
| Predictable | Checked | Yes (explicit intrinsics) | Yes (explicit intrinsics) |
| Provable | Checked | No | No |
| High-integrity (future) | Checked | Only in `trusted` blocks | Only in `trusted` blocks |

The default is the same across all profiles: checked. The difference is that the provable subset excludes wrapping and saturating intrinsics because the proof model does not have modular arithmetic semantics. Wrapping intrinsics may be used in predictable code — the five gates (no recursion, bounded loops, no allocation, no FFI, no blocking) do not restrict arithmetic mode.

---

## 4. Source-Level Visibility

### 4.1 Per-expression, not per-function or per-module

The arithmetic mode is determined per-expression, not per-function or per-module. This is a deliberate choice:

- Per-function or per-module modes create "mode confusion" where a reader must track ambient state to understand a single line of code.
- Per-expression modes mean every arithmetic operation is self-documenting: `a + b` is checked, `wrapping_add(a, b)` is wrapping.
- This matches Zig and Swift's approach and avoids Rust's debug/release divergence problem.

### 4.2 Syntax

Checked arithmetic uses the standard operators:

```
let sum: i32 = a + b;       // traps on overflow
let diff: i32 = a - b;      // traps on overflow
let prod: i32 = a * b;      // traps on overflow
```

Wrapping arithmetic uses named intrinsics:

```
let index: i32 = wrapping_add(head, 1) % 16;    // intentional modular arithmetic
let hash: u32 = wrapping_mul(h, 31);             // hash computation
let bits: i32 = wrapping_add(wrapping_mul(b0, 256), b1);  // byte assembly
```

Saturating arithmetic uses named intrinsics:

```
let integral: i32 = saturating_add(acc, error);  // bounded accumulator
let brightness: u8 = saturating_add(pixel, boost);  // clamped adjustment
```

### 4.3 No mode annotations, no attributes, no pragmas

There is no `#[wrapping]` attribute, no `@setOverflowMode(.wrapping)`, no module-level mode switch. Every arithmetic operation's mode is visible at the call site. This is consistent with Concrete's principle that a reviewer should be able to read one expression and know what it does without consulting ambient context.

### 4.4 Cast between modes

Standard operators are always checked. There is no way to "cast" a function into wrapping mode. If you need wrapping arithmetic, you write `wrapping_add`. This is intentionally verbose — wrapping should feel like a conscious choice.

---

## 5. Division by Zero

**Decision: always trap (abort).**

Division by zero is a program error in every arithmetic mode:

| Operation | Behavior |
|-----------|----------|
| `a / 0` | Abort (trap) |
| `a % 0` | Abort (trap) |
| `wrapping_add(a, 0)` | Normal (wrapping does not affect division) |
| `saturating_add(a, 0)` | Normal (saturating does not affect division) |

There is no `wrapping_div` — division does not have a meaningful modular interpretation.

**Implementation**: emit a zero-check before every `sdiv`/`udiv`/`srem`/`urem` and branch to an abort block on zero. This replaces the current undefined behavior (hardware-dependent SIGFPE on x86, silence on ARM) with a defined abort.

**Why not Result?** Division by zero is not a recoverable error in the same sense as a parse failure. It is a program bug. The abort-only model handles it the same way as OOM: the process terminates. Code that might divide by a potentially-zero value should check the divisor first:

```
if divisor != 0 {
    let result: i32 = numerator / divisor;
    // ...
}
```

Future stdlib may provide `checked_div(a, b) -> Option<i32>` for code that wants a value-level result.

---

## 6. Shift Overflow

**Decision: trap when shift amount >= bitwidth.**

| Operation | Condition | Behavior |
|-----------|-----------|----------|
| `a << n` | `n < bitwidth` | Normal left shift |
| `a << n` | `n >= bitwidth` | Abort (trap) |
| `a >> n` | `n < bitwidth` | Normal right shift (arithmetic for signed, logical for unsigned) |
| `a >> n` | `n >= bitwidth` | Abort (trap) |

**Implementation**: emit a comparison of the shift amount against the bitwidth, branch to abort on out-of-range. This replaces the current LLVM poison value (which is UB if consumed) with a defined abort.

Wrapping variants are not provided for shifts — there is no common use case for "shift by (n mod bitwidth)". If this proves needed, `wrapping_shl` and `wrapping_shr` can be added later.

---

## 7. Signed Overflow

Signed integer overflow is the most dangerous category because C leaves it undefined and optimizers exploit the UB for transformations.

**Decision: checked mode traps on signed overflow. Concrete never emits `nsw`.**

| Operation | Checked mode | Wrapping mode |
|-----------|-------------|---------------|
| `i32::MAX + 1` | Abort (trap) | Wraps to `i32::MIN` |
| `i32::MIN - 1` | Abort (trap) | Wraps to `i32::MAX` |
| `i32::MIN * -1` | Abort (trap) | Wraps to `i32::MIN` |
| `-i32::MIN` | Abort (trap) | Wraps to `i32::MIN` |
| `i32::MIN / -1` | Abort (trap) | Wraps to `i32::MIN` |

The LLVM `nsw` (no signed wrap) flag is never emitted. Checked mode uses overflow-detecting intrinsics that return a value+flag pair. Wrapping mode uses plain `add`/`sub`/`mul`. Neither tells LLVM "assume no overflow" — both produce defined behavior at the LLVM IR level.

This is a departure from C (where signed overflow is UB exploited by optimizers) and from Rust release mode (where signed overflow wraps silently). It matches Swift (which traps) and Zig (which traps in safe builds).

---

## 8. Narrowing and Widening Casts

**Decision: explicit casts only. No silent truncation.**

### Widening (always safe)

Widening casts are lossless. `as` is sufficient:

```
let wide: Int = narrow_val as Int;    // i32 -> i64, zero-extend or sign-extend
let big: u64 = small_val as u64;      // u32 -> u64, zero-extend
```

### Narrowing (may lose data)

Narrowing casts may truncate. The `as` cast truncates silently (it keeps the low bits):

```
let narrow: i32 = wide_val as i32;    // i64 -> i32, truncates high bits
```

This is explicit at the source level (the programmer wrote `as i32`) but does not trap on data loss. For code that needs to detect data loss, a future stdlib function `try_narrow<T, U>(val: T) -> Option<U>` will return `None` when the value does not fit in the target type.

**Design note**: we considered making `as` trap on narrowing overflow, but this conflicts with legitimate bit-pattern reinterpretation (casting a `u32` byte-assembled value to `i32` for API compatibility). The `as` keyword means "I know the bit pattern; give me this type." Data-loss detection belongs in a separate checked function.

### Signed/unsigned reinterpretation

Casting between signed and unsigned types of the same width reinterprets the bit pattern:

```
let signed: i32 = unsigned_val as i32;    // bit pattern preserved
let unsigned: u32 = signed_val as u32;    // bit pattern preserved
```

No trap. The bit pattern is identical. This is necessary for byte-manipulation code like the endian readers in `pressure_parse_binary_endian.con`.

---

## 9. Report and Diagnostic Visibility

### 9.1 `--report arithmetic`

A new report command shows the arithmetic mode for each function:

```
$ concrete project.con --report arithmetic

  function             mode       wrapping_ops  saturating_ops  div_sites  shift_sites
  ─────────────────────────────────────────────────────────────────────────────────────
  int_ring_push        mixed      1             0               0          0
  ctrl_step            mixed      0             1               1          0
  read_u32_be          wrapping   4             0               0          4
  validate_header      checked    0             0               0          0
```

- **checked**: no wrapping or saturating intrinsics used. All arithmetic traps on overflow.
- **wrapping**: at least one wrapping intrinsic used.
- **mixed**: both checked operators and wrapping/saturating intrinsics present.
- **saturating**: at least one saturating intrinsic used.

The report lists exact counts of wrapping, saturating, division, and shift sites so a reviewer can find them.

### 9.2 Diagnostics

When checked arithmetic traps at runtime, the abort message includes:

```
concrete: arithmetic overflow in function 'ctrl_step' at src/controller.con:112
  operation: i32 add
  lhs: 2147483647
  rhs: 1
  policy: checked (default)
```

This tells the developer exactly what overflowed, where, and that the active policy was checked mode. The "policy: checked (default)" line confirms that this is not a wrapping context.

### 9.3 `--report effects` integration

The existing `--report effects` JSON output gains an `"arithmetic"` field per function:

```json
{
  "function": "ctrl_step",
  "arithmetic": "mixed",
  "wrapping_sites": 0,
  "saturating_sites": 1,
  "div_sites": 1,
  "shift_sites": 0
}
```

This integrates with the existing trust/evidence reporting pipeline so that arithmetic mode is part of the per-function evidence record.

---

## 10. Proof Boundaries

### 10.1 Checked arithmetic and proofs

Checked arithmetic makes proofs simpler because the proof model (unbounded integers) and the runtime model (fixed-width with overflow trap) agree on all non-overflowing inputs. When a proof shows `f(x) = y` for mathematical integers, and the runtime traps on overflow, the only way the runtime can disagree is by aborting. The proof result is never silently wrong.

This narrows the integer gap documented in [PROOF_SEMANTICS_BOUNDARY.md](PROOF_SEMANTICS_BOUNDARY.md): instead of "proofs assume no overflow and the runtime wraps silently," the statement becomes "proofs assume no overflow and the runtime aborts on overflow." The proof is still not valid for overflowing inputs, but overflowing inputs are caught rather than producing incorrect results.

### 10.2 Wrapping arithmetic and proofs

Wrapping intrinsics (`wrapping_add`, etc.) are not eligible for the proof subset. The proof model uses Lean's unbounded `Int` and has no modular arithmetic semantics. A function that uses wrapping intrinsics fails the proof eligibility gate.

If wrapping arithmetic needs to be proved, the proof must be written directly in Lean against a modular-arithmetic model, not through the Concrete proof pipeline. This is a future extension.

### 10.3 Division and proofs

Division and modulo are not currently modeled in `PBinOp`. This remains a gap. A function that uses division is proof-eligible (the other gates may pass), but the proof cannot reason about the division result. Until division is added to the proof model, proofs involving division carry an explicit caveat.

### 10.4 Future: overflow preconditions

The long-term path for closing the integer gap is overflow preconditions: the proof obligation for `f(x) = y` includes a machine-checkable condition that all intermediate arithmetic stays within the representable range of the target type. This is not designed here — it depends on the proof model extensions planned for later phases.

---

## 11. Comparison with Other Languages

| Language | Default overflow | Explicit wrapping | Explicit saturating | Division by zero | Shift overflow |
|----------|-----------------|-------------------|--------------------|--------------------|---------------|
| **Concrete** (proposed) | Trap (abort) | `wrapping_add` etc. | `saturating_add` etc. | Trap (abort) | Trap (abort) |
| **Rust** | Debug: trap; Release: wrap | `Wrapping<T>`, `.wrapping_add()` | `.saturating_add()` | Trap (panic) | Masked to bitwidth |
| **Zig** | Trap (`+` is safety-checked) | `+%` operator, `@addWithOverflow` | `+|` operator | Trap (safety check) | Trap (safety check) |
| **Swift** | Trap | `&+`, `&-`, `&*` | Clamping via stdlib | Trap (fatal error) | Trap for `<<`, masked for `&<<` |
| **C** | Signed: UB; Unsigned: wrap | N/A (unsigned is always wrapping) | N/A | UB (hardware-dependent) | UB |
| **Go** | Wrap silently (all modes) | N/A (default is wrapping) | N/A | Trap (panic) | Masked to bitwidth |

### Design choices relative to peers

**Like Swift and Zig, unlike Rust release mode**: the default never wraps silently. A reviewer reading `a + b` in Concrete always knows it will trap on overflow. There is no build-mode divergence.

**Like Rust, unlike Zig**: wrapping is via named functions, not special operators. `wrapping_add(a, b)` is clearer than `a +% b` for readers unfamiliar with the operator. Named functions also compose better with Concrete's existing expression grammar (no new operator precedence rules).

**Unlike all peers**: shift overflow traps instead of masking. Masking (`shift_amount % bitwidth`) silently produces a different result, which is the same problem as wrapping arithmetic. Code that intends to mask should use `a << (n % 32)` explicitly.

**Unlike C**: signed overflow is never undefined behavior. Concrete never emits `nsw`. The optimizer cannot exploit "impossible" overflow for code motion, dead-code elimination, or loop transformations that break programmer expectations.

---

## 12. Migration Path

### What changes from today

| Behavior | Today | After this policy |
|----------|-------|-------------------|
| `a + b` on overflow | Wraps silently | Traps (abort) |
| `a / 0` | UB (hardware-dependent) | Traps (abort) |
| `a << 64` | Poison (UB) | Traps (abort) |
| Ring buffer `(head + 1) % 16` | Works (wrapping) | Must use `wrapping_add(head, 1) % 16` or will trap if `head` is `i32::MAX` (unlikely but possible) |
| Byte assembly `(b0 << 24) \| ...` | Works (wrapping) | Must use `wrapping_shl` or widen to `i64` first |
| `as i32` narrowing | Truncates silently | Truncates silently (unchanged) |

### Pressure set impact

Programs from the Phase 2 pressure set that need updating:

- **`pressure_fixcap_ring_buffer.con`**: `(r.head + 1) % 16` — in practice `head` is always 0..15 so checked arithmetic will not trap, but the intent is modular. Consider `wrapping_add` for clarity, or leave as-is if the domain guarantees no overflow.
- **`pressure_fixcap_controller.con`**: `c.integral + error` — the manual `clamp` already bounds the result, but the intermediate addition could overflow before clamping. Use `saturating_add` or widen to `i64` for the intermediate computation.
- **`pressure_parse_binary_endian.con`**: `(b0 << 24) | ...` — shift of a `u8`-widened-to-`i32` value by 24 produces values above `i32::MAX`. This needs wrapping intrinsics or should use `u32` / `i64` for the intermediate value.

### Interpreter alignment

The interpreter (`Interp.lean`) must be updated to match checked semantics:

1. Add overflow detection for `add`, `sub`, `mul` based on the target type's range.
2. Keep the existing division-by-zero trap (already aligned).
3. Add shift-amount range checking.

This closes the interpreter-vs-compiled divergence for the first time.

---

## 13. Implementation Sequence

1. **Add wrapping intrinsics**: `wrapping_add`, `wrapping_sub`, `wrapping_mul` as compiler intrinsics that emit plain LLVM `add`/`sub`/`mul`. No behavior change for existing code yet.
2. **Add saturating intrinsics**: `saturating_add`, `saturating_sub`, `saturating_mul` emitting LLVM `llvm.*.sat` intrinsics.
3. **Add division-by-zero checks**: emit zero-check + abort branch before every `sdiv`/`udiv`/`srem`/`urem`. This fixes real UB immediately.
4. **Add shift-range checks**: emit bitwidth-check + abort branch before every `shl`/`ashr`/`lshr`.
5. **Switch default arithmetic to checked**: replace `add`/`sub`/`mul` emission with `llvm.*.with.overflow` intrinsics + abort branch. This is the breaking change.
6. **Add `--report arithmetic`**: implement the per-function arithmetic mode report.
7. **Update interpreter**: add overflow/shift/division checks to `evalBinOp`.
8. **Update pressure programs**: fix the handful of programs that rely on wrapping.
9. **Update proof eligibility**: reject wrapping/saturating intrinsics from the proof subset.
10. **Update PREDICTABLE_BOUNDARIES.md**: remove "Integer overflow: silent wrap" from the UB table. Add "Integer overflow: trap (abort)" to the failure paths table.

---

## 14. Summary of Commitments

1. Checked arithmetic (trap on overflow) is the default for all profiles and all integer types.
2. Wrapping arithmetic is available via explicit named intrinsics (`wrapping_add`, `wrapping_sub`, `wrapping_mul`).
3. Saturating arithmetic is available via explicit named intrinsics (`saturating_add`, `saturating_sub`, `saturating_mul`).
4. Division by zero always traps (abort).
5. Shift by >= bitwidth always traps (abort).
6. Signed overflow is never undefined behavior. Concrete never emits LLVM `nsw`.
7. Narrowing casts (`as`) truncate silently. Checked narrowing is a separate stdlib function.
8. Arithmetic mode is per-expression, not per-function or per-module. No ambient mode switches.
9. `--report arithmetic` shows the arithmetic mode and operation counts per function.
10. Wrapping and saturating intrinsics are excluded from the proof-eligible subset.
