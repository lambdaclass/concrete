# Arithmetic Policy (design record)

Archived from docs/language on 2026-10-30: the arithmetic design record (the overflow policy, the
profile mapping, report classification, proof boundaries, the cast decision's reasoning, staging and
history). The rules are in [docs/spec/arithmetic.md](../../spec/arithmetic.md): floats (ARITH-1),
literals (ARITH-2), checked arithmetic (ARITH-3), wrapping and saturating operations (ARITH-4),
conversions (ARITH-5), comparison semantics (ARITH-6), shift fill (ARITH-8), division rounding and the
remainder's sign (OPEN-114), no ambient arithmetic mode, `#[overflow_checked]` included (ARITH-9), and
the named conversions' results (ARITH-10, pending); the numeric types and their spellings are TYPE-19,
`char` is TYPE-20, comparison typing is TYPE-6, array bounds are MEM-6 and the abort is STMT-2. Where
this document and the spec differ, the spec governs.

**Statements the absorption did not carry over (2026-10-30).**

- Open, not decided: §3.1's release profile that may omit a check "explicitly configured off and
  reported", and its provable profile that refuses an unproved overflow site (OPEN-97); §8's `as` as
  the spelling of pointer and reference casts (OPEN-98); §8.2's indicative spellings of the named
  conversions (OPEN-99); §8a's comparison on a `Copy`-bounded type parameter checked per
  instantiation (OPEN-100); §9.2's trap message, which the binary does not write (OPEN-101); §1's
  `Float32` spelling, which the parser does not map (OPEN-102); §8a's comparisons on `bool` and `char`
  (OPEN-42).
- Stale: §7's "wrapping mode" results for `-i32::MIN` and `i32::MIN / -1` describe operations the
  language does not have (there is no wrapping negation or division, ARITH-4); §8a's comparison of
  raw pointers in trusted or `Unsafe` code was removed by OD26 (TYPE-6@2, pending as bug 322); §8a's
  "normative home is this section" is superseded by TYPE-6 and ARITH-6.
- Not language rules: the profile mapping of §3 (which arithmetic the provable subset admits) belongs
  to [PROVABLE_V1.md](../../verification/PROVABLE_V1.md) and [PROFILES.md](../../platform/PROFILES.md);
  the reports of §9 to the compiler's report documentation; the proof boundaries of §10 to
  [PROOF_SEMANTICS_BOUNDARY.md](../../verification/PROOF_SEMANTICS_BOUNDARY.md).

Status: stable policy reference (Phase 3 items 60-61)

This document defines Concrete's arithmetic overflow policy as a public language surface. It specifies what happens on integer overflow, division by zero, shift overflow, and narrowing for each profile, and how the active policy is made visible in source, reports, and proof artifacts.

For the profile definitions, see [PROFILES.md](../../platform/PROFILES.md).
For the current runtime boundary classification, see [PREDICTABLE_BOUNDARIES.md](../../platform/PREDICTABLE_BOUNDARIES.md).
For the proof-vs-runtime integer gap, see [PROOF_SEMANTICS_BOUNDARY.md](../../verification/PROOF_SEMANTICS_BOUNDARY.md).
For the failure strategy (abort-only, no unwinding), see [the spec's "Abort and failure"](../../spec/statements.md#abort-and-failure).

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

**As of ROADMAP #10 Stage 2.x, every ordinary integer operation is CHECKED and
traps (abort) at runtime on the unsafe case, in every profile.** Codegen and the
interpreter agree by construction (verified by the differential oracle).

- **Addition, subtraction, multiplication**: checked — trap on signed or unsigned
  overflow. Codegen routes `+ - *` through per-type `*.with.overflow`→abort
  helpers; the interpreter traps to match. Intentional modular arithmetic uses the
  explicit `wrapping_*`; clamping uses `saturating_*`. (Historically these wrapped
  silently; Stage 2.3 closed that gap.)
- **Division, modulo**: checked — division/modulo by zero traps (abort), and the
  signed `MIN / -1` (and `MIN % -1`) overflow traps. (Stage 2.4/2.5; was previously
  `sdiv`/`udiv`/`srem`/`urem` with div-by-zero left as LLVM UB.) The interpreter
  traps to match.
- **Shift**: checked on the *count*, not the result — a shift amount `>= bitwidth`
  (or negative) traps (abort); with a valid count the bits shifted out are discarded
  without a trap (`i8` `100 << 1` is `-56`; §6). (Stage 2.4/2.5; was previously an
  LLVM poison value / UB.) The interpreter traps to match (`IntArith.evalShift`).
- **Negation**: checked — `-x` traps when `x` is the type's `MIN` (the result is
  unrepresentable), exactly like the `0 - x` checked subtraction. (Was previously
  `sub 0, x` wrapping silently.) The interpreter traps to match.

- **Array indexing**: checked — a raw `a[i]` / `a[i] = v` whose index is out of
  bounds traps (abort) at runtime. A single unsigned compare `(u64)i < len` catches
  both negative and `>= len`; the `@__cc_bounds_check` helper aborts on failure (the
  same trap as the arithmetic above), so compiled code and the interpreter agree.
  Constant-OOB is a hard compile error (C7); a provably in-bounds index folds the
  check away. (KNOWN_HOLES H8, closed 2026-06-28; gate `check_array_bounds.sh`.) A
  named `get_unchecked`-style opt-out behind trusted/Unsafe is a future ergonomic
  addition, parallel to `wrapping_*` — there is no global "disable bounds checks".

### Current interpreter behavior

The source-level interpreter (`Interp.lean`) uses Lean's arbitrary-precision `Int`
and traps on overflow / div-zero / over-width shift / `MIN` negation, matching the
compiled aborts (the earlier interp-vs-compiled arithmetic divergence is closed).
It also traps on out-of-bounds array access — which compiled code now does too
(H8, closed 2026-06-28).

### Current proof behavior

The proof model (`Proof.lean`) uses Lean's unbounded `Int` for ordinary
arithmetic, so theorems carry an implicit assumption that runtime intermediates
stay representable when normal return is part of the claim. `i32`/`u32`
division and modulo have explicit `PBinOp.div`/`.mod` forms; other widths and
the full trap/no-overflow correspondence remain narrower than runtime
semantics.

### Current overflow-obligation policy

Overflow obligations are **not generated for every integer `+`, `-`, or `*` by
default**. Arithmetic is everywhere, and Concrete's runtime overflow behavior is
checked in every profile. The open question is whether a profile requires static
discharge or accepts the runtime check, not what the source operation means.
Emitting a missing obligation for every ordinary arithmetic expression would
bury useful audit output in noise.

Instead, overflow proof obligations are opt-in through an erased source
attribute:

```concrete
#[overflow_checked]
fn add_capacity(len: i32, extra: i32) -> i32 {
    return len + extra;
}
```

Inside an `#[overflow_checked]` function, each fixed-width integer addition,
subtraction, and multiplication generates a runtime-safety obligation stating
that the operation stays within the destination type's range. The obligation may
then be discharged by `omega`, `bv_decide`, a Lean theorem, runtime checking, or
reported as missing according to the normal evidence ladder.

Outside an `#[overflow_checked]` scope, audit output must not silently imply
"overflow proved." It should either omit a static no-overflow claim or report
that no static discharge was requested. The runtime operation remains checked.
This keeps the evidence ledger honest without making every example unreadable.

`#[overflow_checked]` is **not** an arithmetic mode switch. It does not make
wrapping arithmetic wrap, trapping arithmetic trap, or saturating arithmetic
saturate. It only asks the proof/audit pipeline to generate no-overflow claims
for ordinary arithmetic. Explicit wrapping and saturating operations remain
source-visible through their named intrinsics.

### What the pressure tests revealed

The Phase 2 pressure programs exposed the ambiguity that motivated the shipped
policy:

- **`pressure_fixcap_controller.con`**: PID controller uses integer-scaled gains
  (`Kp*error / 10`). Intermediate overflow now traps rather than silently
  corrupting the running average.
- **`pressure_fixcap_ring_buffer.con`**: wrap-around index intent must use the
  explicit modular spelling where arithmetic overflow, rather than `%`, is the
  intended mechanism.
- **`pressure_parse_binary_endian.con`**: shift-and-or byte assembly (`(b0 << 24) | (b1 << 16) | (b2 << 8) | b3`) produces correct bit patterns, but for values above `2^31` the result is a negative `i32`. The comment "for 0xDEADBEEF (> 2^31), we use the raw bit pattern in i32" documents the confusion.
- **`pressure_fixcap_state_machine.con`**: no overflow risk; a static claim still
  requires an obligation/proof rather than being inferred from confidence.

The resulting source rule distinguishes them: ordinary operators are checked,
`wrapping_*` is modular, and `saturating_*` clamps.

---

## 2. Arithmetic Modes

The modes below describe the implemented public arithmetic policy. Static
no-overflow obligations remain opt-in through `#[overflow_checked]`; runtime
checked-by-default arithmetic is already shipped.

Concrete defines three arithmetic modes for integer operations. Every integer operation executes in exactly one mode. The mode is always determinable from the source.

### 2.1 Checked (trap on overflow)

This is the **default mode** for all profiles.

- Addition, subtraction, and multiplication trap (abort) on signed or unsigned overflow.
- The trap is an abort, consistent with Concrete's abort-only failure strategy. Defer does not run (same as OOM abort).
- Implementation: emit LLVM `llvm.sadd.with.overflow` / `llvm.uadd.with.overflow` (and corresponding sub/mul intrinsics), branch to an abort block on the overflow flag.

**Rationale**: silent wrapping is a correctness bug in almost all code. The predictable profile's promise of "no hidden runtime behavior" is violated by silent wrapping — a reviewer reading `a + b` cannot know whether the result is mathematically correct without reasoning about the ranges of `a` and `b`. Trapping makes the failure visible and immediate.

### 2.2 Wrapping (modular arithmetic)

Explicit opt-in for code that intentionally uses two's-complement modular semantics.

- Operations wrap in two's complement by explicit request.
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
| Provable | Checked | `wrapping_add` at `u32` only | No |
| High-integrity (future) | Checked | Only in `trusted` blocks | Only in `trusted` blocks |

The default is the same across all profiles: checked. The provable subset
admits exactly the arithmetic the proof model can faithfully represent. The
model has one modular operator today — `addw 32` (wrapping add mod 2³² at
`u32`), forced by SHA-256's compression rounds — so `wrapping_add` on `u32`
**is** provable and extracts to it (see `Concrete.ProofCore.binOpToPBinOp`).
`wrapping_sub`/`wrapping_mul`, wrapping at other widths, and all saturating
intrinsics are not yet modeled and stay out of the subset. Ordinary checked
`+` on `u32` extracts to the mathematical `add` (sound in its non-trapping
domain — overflow is a defined abort, not wrap), so it must NOT be used where
mod-2³² semantics are intended: that is exactly what `wrapping_add` is for.
Wrapping intrinsics may be used in predictable code — the five gates (no
recursion, bounded loops, no allocation, no FFI, no blocking) do not restrict
arithmetic mode.

### 3.1 Enforcement strategy (per profile)

The *meaning* of `a + b` is checked in every profile (overflow is a bug). What a
profile chooses is **how that checked guarantee is met** — never whether it holds.
This is the axis where profiles differ; it changes enforcement and reporting, not
source meaning.

| Profile | How `checked` is enforced for an overflow-capable `+ - *` site |
|---------|----------------------------------------------------------------|
| Safe / debug | Insert a runtime overflow check → abort, wherever the site is not statically proved safe. |
| Release | Same checked meaning. A discharged proof MAY omit the now-redundant runtime check (sound elision); an unproved site keeps its check unless a check is *explicitly configured off and reported*. **Never silently wraps.** |
| Predictable | Runtime-checked as Safe (the predictable gates do not change arithmetic meaning), or rejects unproved overflow if the project opts into stricter admission. |
| Provable / high-integrity | Overflow must be discharged by proof: an unproved overflow site is **rejected**, not silently downgraded to a runtime check. Static evidence is the admission floor. |

The invariant across the table: a profile may turn an overflow into a
*compile-time rejection*, a *runtime abort*, or a *proof-discharged no-op* — but
never into a wrap. Wrapping/clamping only ever come from the explicit
`wrapping_*` / `saturating_*` spellings.

### 3.2 Audit classification

For auditability, every arithmetic site is classifiable (via `--report arithmetic`
/ `--report effects`, §9) as exactly one of:

- **proved** — overflow discharged by a proof; no runtime check needed.
- **runtime-checked** — a runtime overflow check (abort on overflow) is emitted.
- **explicit-wrapping** — `wrapping_*`, modular by intent.
- **explicit-saturating** — `saturating_*`, clamping by intent.

A reviewer can therefore see, per operation, both its *meaning* (checked vs.
wrapping vs. saturating — from the source spelling) and *how the checked ones are
discharged* (proved vs. runtime-checked — from the profile + proof state).

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

### 4.3 No ambient arithmetic-mode annotations

There is no `#[wrapping]` attribute, no `@setOverflowMode(.wrapping)`, no module-level mode switch. Every arithmetic operation's mode is visible at the call site. This is consistent with Concrete's principle that a reviewer should be able to read one expression and know what it does without consulting ambient context.

This rule is about **semantics**. It does not forbid attributes that request
evidence generation. `#[overflow_checked]` is allowed because it does not change
the meaning of `+`, `-`, or `*`; it makes no-overflow obligations visible in
audit output.

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

**Decision: a shift checks its count, not its result.** With operand width `w` and count `n`:

| Operation | Condition | Behavior |
|-----------|-----------|----------|
| `a << n` | `0 <= n < w` | Left shift; the bits shifted out of the top are discarded (no trap, whatever the sign of the result) |
| `a >> n` | `0 <= n < w` | Right shift: arithmetic (sign-filling) for a signed type, logical (zero-filling) for an unsigned type |
| `a << n`, `a >> n` | `n >= w` or `n < 0` | Abort (trap) |

So `i8` `100 << 1` is `-56` and `u8` `200 << 1` is `144`, with no trap, in every profile. A shift is
therefore *not* overflow-checked the way `+ - *` are: code that needs to detect bits lost by a left
shift checks the operand's range first, or widens before shifting.

**Status: enforced on main** (codegen compares the count with the width and branches to the abort
helper; the interpreter and the folder share `IntArith.evalShift`), probed for both consumers on
2026-10-21. Earlier text in this document that said "shift overflow traps" meant the count check
only.

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

## 8. Numeric Conversions (`as`)

**Decision (2026-10-21; adopted, staged, not yet enforced): `as` performs only conversions that are
lossless for every value of the source type. Every other numeric conversion written with `as` is a
compile error; the program uses a named operation instead.** This replaces the earlier decision
"explicit narrowing may truncate".

**Scope.** Numeric, `bool` and `char` conversions only. `as` stays the spelling of pointer and
reference casts (`as *const T`, `as *mut T`, `as &K`, `as &mut V`: 281 lines in std alone), which
this decision does not change.

### 8.1 What `as` accepts

`as` from `S` to `T` is accepted exactly when every value of `S` converts to a value of `T` that
means the same number (or the same `bool` / byte):

| source → target | accepted by `as` |
|---|---|
| any type to itself | yes |
| integer → wider integer of the same signedness | yes |
| unsigned integer → wider signed integer | yes |
| integer → narrower integer, signed → unsigned, unsigned → signed of the same width | **no** |
| `bool` → integer | yes (`false` 0, `true` 1) |
| integer → `bool` | **no** (never total) |
| `char` → unsigned integer of ≥ 8 bits, or signed integer of ≥ 16 bits | yes (`char` is one byte today, a byte-oriented code unit: STRING_TEXT_CONTRACT §2) |
| `char` → `i8` | **no** (only 0–127 fit) |
| `u8` → `char` | yes (every byte is a `char` today) |
| any other integer → `char` | **no** |
| integer → float, when every value of the source is exactly representable (`i8`, `u8`, `i16`, `u16` → `f32`; integers of ≤ 32 bits → `f64`) | yes |
| any other integer → float (`i32`/`u32` → `f32`; `Int`/`u64` → `f32` or `f64`) | **no** (rounds) |
| float → integer | **no** (fractions, range, NaN and ±∞) |
| `f32` → `f64` | yes |
| `f64` → `f32` | **no** |

A Unicode-scalar `char` would be a separate surface proposal with its own conversion rows.

### 8.2 The named operations (three families)

Every conversion `as` refuses has a named operation, chosen per site by what the program means:

1. **Lossless conversion.** `as` itself, for the rows above.
2. **Checked conversion returning an error.** `try_convert::<T>(x) -> Option<T>` (or a `Result`
   with a reason) returns the value-preserving result or none: none when an integer is out of the
   target's range, when a float is not integral, out of range, NaN or ±∞, and when an integer or a
   float is not exactly representable in the target float. This is the `try_narrow` this document
   promised before, generalized.
3. **Explicit bit-level and rounding operations, with exact semantics.** `truncate::<T>(x)` keeps
   the low `width(T)` bits of the two's-complement representation (`truncate::<u8>(300)` is 44);
   `reinterpret::<T>(x)` changes signedness at the same width, preserving the bit pattern
   (`reinterpret::<u32>(x)` with `x: i32` equal to -1 is 4294967295); float → integer rounding
   operations (`trunc`, `round`, `floor`, `ceil`) each state their result for every input,
   including NaN and ±∞, either as a value or as an error; integer → float and `f64` → `f32`
   conversions name their rounding (round-to-nearest-even). Masked idioms such as `(v & 0xFF)`
   followed by a conversion to `u8` use `truncate` (or `try_convert`, which then always succeeds).

The spellings above are indicative; the API's owner fixes them in stage 1. All three families are
specified in one place, the arithmetic authority (`IntArith`, R-0493), and every consumer (Check's
folder, the interpreter, Lower / EmitSSA, the extracted model, Core) uses it.

**Why a compile error, not a trap.** A trapping `as` (value-preserving or abort, like Zig's
`@intCast` with safety on) was the cheaper migration, since sites that never see a lossy value
would keep compiling. It was not chosen: it turns previously silent results into aborts with no
cleanup ([the spec's "Abort and failure"](../../spec/statements.md#abort-and-failure)) in programs that tolerated them, and it gives
every `as` a value-preservation obligation. With the compile error `as` is total and
obligation-free, which is the property the verified pipeline benefits from most; the cost is
rewriting sites that are in fact safe.

### 8.3 Status on main (`5b63cd87`; probed 2026-10-21 with the binary and `--interp`)

| conversion | today |
|---|---|
| widening | exact, in both consumers |
| integer narrowing, sign change | **silent truncation / reinterpretation**: `300 as u8` is 44, `-1 as u32` is 4294967295, `-1 as u64` is 2^64 − 1, in both consumers |
| float → integer | truncates toward zero (`3.9 as i32` is 3, `-3.9 as i32` is -3) and **traps** when the value is out of the target's range (`1e20 as i32` aborts); the interpreter rejects float literals ("float literals not yet supported"), so only integer-origin floats can be compared |
| integer → float | IEEE rounding in the binary; **identity in the interpreter**: `(9007199254740993 as f64) as Int` is 9007199254740992 natively and 9007199254740993 in `--interp`, a silent disagreement ([bug 128](../../bugs/128_interp_int_to_float_identity.md), exit 0b) |
| integer → `bool` | refused by the checker (E0553), as §8.1 decides; before the fix it passed the checker and failed in `llvm-as` (`bitcast i64 to i1`) ([bug 130](../../bugs/130_int_as_bool_invalid_ir.md), fixed) |
| `bool` → integer | `false` 0, `true` 1 (zext) in both consumers; it was invalid IR ([bug 108](../../bugs/108_bool_cast_trunc.md), fixed) |
| `--report arithmetic` | lists checked float → integer casts but **no integer narrowing cast** (`fn n(x: Int) -> u8 { return x as u8; }` is absent), so silent truncation is invisible in the report meant to classify arithmetic sites |

So the rule is **mostly not enforced**: integer → `bool` is refused (E0553), and every other row
marked "no" in §8.1 is accepted by `as` today.

### 8.4 Staging

1. **Named operations first.** Add the three families of §8.2 to the arithmetic authority and
   `std`, with the interpreter, the folder, the backend and the models agreeing; classify every
   cast in `--report arithmetic` (lossless `as`, lossy `as`, named operation); refuse `Int as bool`
   and repair `bool as` integer (bugs 130 and 108, exit 2c: done, E0553 and zext); repair the
   interpreter's integer → float identity (bug 128, exit 0b).
2. **Rejection with versioned migration diagnostics.** The check lands with its diagnostic in the
   form of [MEMORY_SEMANTICS.md §15.5](MEMORY_SEMANTICS.md#155-versioned-migration-diagnostics): a
   stable code, the rule tag (`CAST-1@1`), the conversion class, and the mechanical rewrite
   (`truncate` / `reinterpret` when the program relies on today's bits, `try_convert` when it
   wants detection, a rounding operation for floats). It first runs as the typed inventory: every
   cast with its location and source and target types, taken before constant folding (folding
   removes casts of constants, so `--emit-ssa`'s `cast` lines are only a supplement).
3. **Migration.** `std`, examples and tests are rewritten with those rewrites, and the check
   becomes a hard error for all code in the same change, with the interpreter, `IntArith` and the
   models. There is no edition switch and no opt-out (the §15.5 policy).

**Migration cost (estimate).** 1 347 `as` occurrences with a numeric, `bool` or `char` target (a
textual count, comments included: `as Int` 513, `as i32` 331, `as u64` 194, `as u8` 177, `as i64`
54, `as u32` 30, `as char` 24, `as u16` 17, `as bool` 5, `as i8` 2, `as f32` / `as f64` 0). The
target type alone does not decide lossiness (`Int` → `u64` is lossy, `u8` → `i32` is not); a rough,
unvalidated estimate of lossy sites is 500 – 700, mostly lengths `Int` → `i32` and byte extraction
to `u8`. Stage 2's inventory replaces the estimate.

**Sequencing with exit 2h.** Exit 2h (R-0493) makes one `IntArith` the arithmetic authority,
casts included. It centralizes the *current* cast semantics faithfully and does not wait for this
decision; stage 3 then changes that one authority and its consumers together.

**Prevents.** The bug 080 class (a narrowing cast modelled as identity: a false-proof surface),
bug 108, the interpreter's integer → float identity (bug 128), the `as bool` failure (bug 130),
and the report gap.
Each is a consumer disagreeing about a lossy cast; a lossless-only `as` removes the lossy cases
from what every consumer must model.

**Owners.** The rule and its class table: ROADMAP exit 2k (R-0500), "cast semantics per width
pair". The authority that implements it: exit 2h (R-0493). The defects: bug 128 in exit 0b (R-0482); bugs 130 and 108
and the permanent differential legs in exit 2c (R-0491). ARITH-5 of [docs/spec/arithmetic.md](../../spec/arithmetic.md#ARITH-5) states the cast rule.

---

## 8a. Comparison operators

**Rule (decided 2026-10-21, user decision (b)).** `==`, `!=`, `<`, `<=`, `>` and `>=` are defined on
integers of every width, on floats, on `bool` and on `char`, and on raw pointers (`*const T`,
`*mut T`) in trusted code or a function declaring `with(Unsafe)`. Both operands have the same type
(E0228 for a mixed-width pair). No other type has a built-in comparison: structs and enums
(fieldless `Copy` enums included), `Option`/`Result`, arrays, `String`, `Heap<T>`, fn pointers and
references are refused with E0229 (`comparisonNotDefined`). A type parameter compares only when it
is `Copy`-bounded, and every instantiation is held to the same scalar rule after monomorphization
(E0229, `post-mono`), so `max<T: Copy>` at a struct is a diagnostic rather than invalid IR.

- **Never consuming.** Every comparable type is `Copy`; a comparison reads its operands and never
  moves them.
- **References have no comparison.** `a == b` with `a, b: &T` is refused: it would have to mean
  either address identity (which Place MIR would then have to preserve) or content (which is a
  per-type decision). Compare the referents when they are scalars (`*a == *b`), or call a named
  function over references (`fn same(a: &T, b: &T) -> bool`).
- **Aggregates compare through named functions.** Collections already take explicit `eq`, `hash`
  and `cmp` function pointers (`HashMap::new(hash_fn, eq_fn)`, `OrderedMap::new(cmp_fn)`). Derived
  or opt-in structural equality is pull-gated by a workload.
- **Float laws.** Comparison follows IEEE 754: `==` is not reflexive (`x == x` is false when `x` is
  NaN, and `x != x` is true), and every ordered comparison with a NaN operand is false. The binary
  emits `fcmp oeq`/`une`/`olt`/... ; no pass may fold a float self-comparison
  (the SSA folder folded `x == x` to `true` for floats, EVALUATION2 D12; filed and repaired as bug 150 on the interpreter-coverage branch).
- **Raw pointers** compare by address, in trusted or `Unsafe` code only. (Known limit: through a
  `Copy`-bounded generic instantiated at a pointer type the post-mono re-check cannot see the
  instance's authority, so such a comparison is accepted; bug 140.)

**Status on main.** Enforced: `Check.lean` `.binOp` (E0229 at the source span), and
`verifyComparisonTypes` after monomorphization. It closes [bug 134](../../bugs/134_aggregate_eq_invalid_ir.md)
(aggregate `==` failed in `llvm-as`) and [bug 140](../../bugs/140_comparison_consumes_linear.md) (linear
operands consumed and leaked; references compared by address). Migration: 2 sites (`examples/hexdump`,
`examples/png_chunks`).

**Owners.** The rule's normative home is this section with VALUE_MODEL under R-0500 (exit 2j/2k);
the enforcing check is R-0499 (S8); float-comparison folding belongs to R-0493's typed-operation
authority.

---

## 9. Report and Diagnostic Visibility

### 9.1 `--report arithmetic` — IMPLEMENTED (2026-06-26)

`concrete <file> --report arithmetic` classifies every arithmetic site (per
function) into exactly one of the §3.2 classes and prints per-function counts +
the per-site detail and a grand total:

```
$ concrete file.con --report arithmetic
=== Arithmetic Site Classification (--report arithmetic) ===

-- m.f  (file.con:12)
   runtime-checked: 9  explicit-wrapping: 1  explicit-saturating: 1  proved: 0
   + [runtime-checked], wrapping_add [explicit-wrapping], / [runtime-checked], ...

Totals: 12 arithmetic sites — 10 runtime-checked, 0 proved, 1 explicit-wrapping, 1 explicit-saturating
```

A *site* is a checked integer op (`+ - * / % << >>`), a unary `-`, or a checked
`float→int` cast (→ **runtime-checked**); a `wrapping_*` (→ **explicit-wrapping**);
or a `saturating_*` (→ **explicit-saturating**). Comparisons, logical ops, pure
bitwise `& | ^`, and float `+ - * /` (IEEE, not the integer trap) are not
classified. **proved** is currently always 0: the proof model discharges
refinement against unbounded-`Int` specs with an implicit no-overflow assumption
rather than discharging overflow obligations, so no site is yet "overflow
proved" — this becomes non-zero only when overflow proofs land (and the
now-redundant runtime check may then be elided). Gate:
`scripts/tests/check_report_arithmetic.sh`.

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

This narrows the integer gap documented in [PROOF_SEMANTICS_BOUNDARY.md](../../verification/PROOF_SEMANTICS_BOUNDARY.md): instead of "proofs assume no overflow and the runtime wraps silently," the statement becomes "proofs assume no overflow and the runtime aborts on overflow." The proof is still not valid for overflowing inputs, but overflowing inputs are caught rather than producing incorrect results.

### 10.2 Wrapping arithmetic and proofs

The proof model has one modular operator: `addw 32` — wrapping add mod 2³² at
`u32`, forced by SHA-256's compression rounds and message schedule. So
`wrapping_add` on `u32` **is** eligible for the proof subset and extracts to it
(`Concrete.ProofCore.binOpToPBinOp .wrappingAdd .u32 = some (.addw 32 false)`).
The proof model faithfully matches the runtime here: `wrapping_add` wraps mod
2³² and never traps, exactly as `addw 32` evaluates. This is why the
proof-carrying SHA-256 example (`examples/hmac_sha256`) uses `wrapping_add`
throughout — ordinary checked `+` would both trap at runtime AND extract to
mathematical `add`, neither of which is the mod-2³² semantics SHA-256 requires.

What is NOT yet modeled, and so stays out of the proof subset: `wrapping_sub`,
`wrapping_mul`, wrapping at widths other than `u32`, and all saturating
intrinsics. A function that uses those fails the proof eligibility gate. They
are append-only follow-ups; until then, code needing them must be proved
directly in Lean against an explicit model, not through the Concrete pipeline.

### 10.3 Division and proofs

`i32`/`u32` division and modulo are modeled by `PBinOp.div`/`.mod`, including
their admitted proof semantics. Other widths and a complete proof that the
runtime trap boundary matches the theorem's domain remain open; a claim that
normal execution returns must state or discharge nonzero-divisor and signed
`MIN / -1` conditions where applicable.

### 10.4 Future: overflow preconditions

The long-term path for closing the integer gap is overflow preconditions: the proof obligation for `f(x) = y` includes a machine-checkable condition that all intermediate arithmetic stays within the representable range of the target type. This is not designed here — it depends on the proof model extensions planned for later phases.

The staged path is `#[overflow_checked]` first: functions opt into these
preconditions explicitly, the audit shows which arithmetic sites are covered,
and release policies can later require the attribute or equivalent arithmetic-
site evidence for selected code.

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
| Byte assembly `(b0 << 24) \| ...` | Works (wrapping) | Unchanged: a valid shift count discards the shifted-out bits without a trap (§6) |
| `as i32` narrowing | Truncates silently | Truncates silently today; a compile error once the 2026-10-21 cast decision lands (§8) |

### Pressure set impact

Programs from the Phase 2 pressure set that need updating:

- **`pressure_fixcap_ring_buffer.con`**: `(r.head + 1) % 16` — in practice `head` is always 0..15 so checked arithmetic will not trap, but the intent is modular. Consider `wrapping_add` for clarity, or leave as-is if the domain guarantees no overflow.
- **`pressure_fixcap_controller.con`**: `c.integral + error` — the manual `clamp` already bounds the result, but the intermediate addition could overflow before clamping. Use `saturating_add` or widen to `i64` for the intermediate computation.
- **`pressure_parse_binary_endian.con`**: `(b0 << 24) | ...` — shift of a `u8`-widened-to-`i32` value by 24 sets the sign bit for `b0 >= 128`. A shift checks only its count (§6), so this does not trap; use `u32` or `i64` for the intermediate value if the result is meant to be non-negative.

### Interpreter alignment

The interpreter (`Interp.lean`) must be updated to match checked semantics:

1. Add overflow detection for `add`, `sub`, `mul` based on the target type's range.
2. Keep the existing division-by-zero trap (already aligned).
3. Add shift-amount range checking.

This closes the interpreter-vs-compiled divergence for the first time.

---

## 13. Historical Implementation Sequence

1. **[DONE — 2026-06-24; ROADMAP #10 Stage 2.1]** Add wrapping intrinsics:
   `wrapping_add`, `wrapping_sub`, `wrapping_mul` as compiler intrinsics that emit
   plain LLVM `add`/`sub`/`mul`. No behavior change for existing code yet. Modeled
   as distinct `BinOp.wrappingAdd/Sub/Mul` (so ordinary `+ - *` can later flip to
   checked while these stay plain); integer-only, same-type operands; interpreter
   wraps both signed and unsigned to match compiled. Gated by
   `scripts/tests/check_wrapping_arith.sh` (`make test-wrapping-arith` + CI step).
2. **[DONE — 2026-06-24] Add saturating intrinsics**:
   `saturating_add`, `saturating_sub`, `saturating_mul`. Add/sub use LLVM
   saturating intrinsics; multiplication uses overflow detection plus a
   signed/unsigned clamp. The interpreter clamps to match.
3. **[DONE — 2026-06-24; Stage 2.4]** Add division-by-zero checks: per-type
   `@__cc_{sdiv,udiv,srem,urem}` helpers abort on `b == 0` (and signed `MIN/-1`
   overflow), else divide. Fixes real UB (was SIGFPE). Interp traps to match.
4. **[DONE — 2026-06-24; Stage 2.5]** Add shift-range checks: per-type
   `@__cc_{shl,ashr,lshr}` helpers abort when the shift amount `>=` bit width
   (was LLVM poison/UB). Interp traps to match.
5. **[DONE — 2026-06-24; Stage 2.3] Switch default arithmetic to checked**:
   replace plain `add`/`sub`/`mul` emission with overflow detection plus an abort
   branch.
6. **Add `--report arithmetic`** **[DONE — 2026-06-26]**: per-function, per-site
   classification into the §3.2 classes (runtime-checked / proved /
   explicit-wrapping / explicit-saturating); gate
   `scripts/tests/check_report_arithmetic.sh`. See §9.1.
7. **[DONE] Update interpreter**: fixed-width checks are shared through
   `Concrete.Semantics.IntArith`.
8. **[DONE] Update pressure programs/corpus**: intentional modular behavior uses
   explicit wrapping operations.
9. **Update proof eligibility** **[DONE — 2026-06-24]**: `wrapping_add` at `u32`
   maps to the proof model's `addw 32` (it is the mod-2³² operator SHA-256
   forces), so it IS in the proof subset; `wrapping_sub`/`wrapping_mul`, other
   widths, and all saturating intrinsics stay out (`binOpToPBinOp → none`).
   Ordinary checked `+` extracts to mathematical `add`, NOT to `addw` (see §10.2).
10. **[DONE] Update current-claim documents**: ordinary overflow and safe OOB
    are classified as defined traps, not UB/silent wrap.
11. **[DONE] Add `#[overflow_checked]` obligation generation**: generate static
    no-overflow obligations for functions/scopes that opt in. Audit output
    distinguishes runtime checking from static proof.

---

## 14. Summary of Commitments

1. Checked arithmetic (trap on overflow) is the default for all profiles and all integer types.
2. Wrapping arithmetic is available via explicit named intrinsics (`wrapping_add`, `wrapping_sub`, `wrapping_mul`).
3. Saturating arithmetic is available via explicit named intrinsics (`saturating_add`, `saturating_sub`, `saturating_mul`).
4. Division by zero always traps (abort).
5. A shift traps (abort) on a count `>= bitwidth` or negative, and otherwise discards the shifted-out bits without a trap (§6).
6. Signed overflow is never undefined behavior. Concrete never emits LLVM `nsw`.
7. `as` performs only lossless numeric conversions; every lossy numeric conversion is a compile error and uses a named operation: checked conversion returning an error, or explicit truncation / reinterpretation / rounding with exact semantics (decided 2026-10-21, staged, not yet enforced: today narrowing `as` still truncates silently; §8).
8. Arithmetic mode is per-expression, not per-function or per-module. No ambient mode switches.
9. `--report arithmetic` shows the arithmetic mode and operation counts per function.
10. Of the wrapping and saturating intrinsics, only `wrapping_add` at `u32` is in the proof-eligible subset (it extracts to `addw 32`, §10.2); every other wrapping or saturating intrinsic is excluded until it is modeled.
11. Overflow proof obligations are opt-in during the staged rollout through
    `#[overflow_checked]`; absence of the attribute must never be displayed as a
    proof of no overflow.
