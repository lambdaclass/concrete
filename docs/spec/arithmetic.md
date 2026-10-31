# Arithmetic

The numeric types are TYPE-19's. Every ordinary integer operation is checked: it
traps on a result its type cannot represent, and the trap is an abort (STMT-2). Wrapping and
saturating arithmetic are named operations. There is no implicit numeric conversion (TYPE-4), and
which operand types an operator takes is TYPE-6. The design records are
[ARITHMETIC_POLICY.md](../archive/design-records/ARITHMETIC_POLICY.md) (absorbed into ARITH-8 to
ARITH-11, TYPE-19 and TYPE-20 on 2026-10-30; it keeps the profile mapping, report classification and
proof boundaries) and NUMERIC_MODEL.md (absorbed into ARITH-2, ARITH-3 and ARITH-5 and deleted).

## Rules

<a id="ARITH-1"></a>
### ARITH-1. Floating point is IEEE-754 with round-to-nearest-even

**ARITH-1@1.** `f32` and `f64` are IEEE-754 binary32 and binary64, every operation rounds to nearest with ties to even, and no operations are contracted (no fused multiply-add unless written).

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0493
- Semantics: none
- Obligation: a total formal reference model for floats, proved for the profile or labelled trusted (VERIFIED_PIPELINE, "Floats")
- Evidence: tested

**Rationale.** A fixed rounding mode and no contraction make float results deterministic across
the interpreter, the LLVM profile and the native profile, so the consumers can be compared and a
reference model can define them. NaN bits, subnormals, signed zero and conversions are specified
together with the reference model.

<a id="ARITH-2"></a>
### ARITH-2. An integer literal fits its type

**ARITH-2@3.** An integer literal takes the integer type its position expects, an integer literal with no expected type is refused, and an integer literal whose value, negation included, is outside its type's range is refused rather than truncated.

- Kind: refusal
- Uses: none
- Codes: E0227 (check)
- Status: partial bug-327
- Enforced by: Concrete.checkExprCore
- Semantics: none
- Obligation: one `IntArith` authority for literal ranges (R-0493)
- Evidence: tested

**Rationale.** A literal must never silently become a different value: `let a: u8 = 300` used to
be 44. The negated value is what is range-checked, so `let a: i8 = -128` is accepted and
`-129` is not. Hex (`0xFF`) and binary (`0b1010`) literals are accepted; suffixes such as `7u8`
are not part of the language (an annotation or a conversion says the type).

A hint (an annotation, a parameter, a field, an arm) gives a literal the hinted type; version 1
typed a literal with no expected type `Int` (`i64`), which is still main's behaviour. Suffixes are
workload-gated: an
annotation already covers the need, so a suffix would be pure convenience, revisited if a real
workload finds annotations too noisy. Gated by `scripts/tests/check_numeric_literals.sh` (ROADMAP
old Phase 6 #6, 2026-06-23).

Version 2 records the owner's decision of 2026-10-29 (OD31): the fallback to `Int` where nothing is
expected is removed and no suffix is required. Whether a float literal follows the same principle
(its context's float type, refused with none), as OD31's note says, is OPEN-63; which positions supply
an expected type is OPEN-138. Main still types an unconstrained integer literal `Int` and
an unconstrained float literal `Float64` (bug 327).

```con reject name=literal-out-of-range code=E0227 stage=check pair=literal-in-range
fn example_value() -> Int {
    let a: u8 = 300;
    return a as Int;
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=literal-in-range exit=42
fn example_value() -> Int {
    let a: i8 = -128;
    let b: u8 = 0xFF;
    return (a as Int) + (b as Int) - 85;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con reject name=unconstrained-literal code=new stage=check pair=constrained-literal pending=bug-327 now=accept:42
fn main() -> u8 {
    if 3 < 4 {
        return 42;
    }
    return 0;
}
```

```con accept name=constrained-literal exit=42
fn main() -> u8 {
    let a: Int = 3;
    if a < 4 {
        return 42;
    }
    return 0;
}
```

<a id="ARITH-3"></a>
### ARITH-3. Integer arithmetic is checked

**ARITH-3@1.** `+`, `-`, `*`, unary `-`, `/` and `%` on integers abort when the mathematical result is not representable in the operand type, when the divisor is zero and for `MIN / -1` and `MIN % -1`, and a shift aborts when its count is negative or at least the operand's width and otherwise discards the bits shifted out.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Pipeline.emit, Concrete.Pipeline.lower
- Semantics: none
- Obligation: traps are behaviours of Core semantics with a ghost kind (R-0493, R-0496); a pass removes a trap only with a discharged-VC certificate
- Evidence: tested

**Rationale.** Silent wrapping is a correctness bug in almost all code, and signed overflow is
the case C leaves undefined. Concrete never emits LLVM's `nsw` or `nuw`: checked operations use
the overflow intrinsics and branch to an abort helper. This holds in every profile; a profile
changes reports, never code generation (whether a profile may omit a check is OPEN-97). The
fill of a right shift that does not abort is ARITH-8; how a division rounds and a remainder's sign are OPEN-114. ARITH-13 refuses an operation the compiler proves always overflows (E0902), but in the current
compiler no operation reaches that proof (bug 350), so E0902 currently provides no compile-time
refusal; the run-time overflow check is the guarantee. Array bounds are
checked the same way, an index out of bounds aborting.

For example `let c: u8 = 200 + 100` compiles and traps at run time: E0902 is never reported
(`provenViolationDiagnostics`, the path that also reports a proven out-of-bounds index, E0900, and
a proven zero divisor, E0901), so this proof path currently refuses no program for overflow; completing the folder
is ROADMAP #10.

Decided by the owner on 2026-10-29 (OD10): integer arithmetic is checked by default and the shift count is checked; wrapping and saturating arithmetic are named operations (ARITH-4), and an abort runs no deferred call (STMT-2).

```con accept name=checked-in-range exit=42
fn example_value() -> Int {
    let a: u8 = 200;
    let s: u8 = a << 1;
    let q: Int = -7 / 2;
    return (s as Int) - 99 + q;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con abort name=unsigned-overflow trap=arithmetic_overflow
fn example_value() -> Int {
    let a: u8 = 200;
    let b: u8 = 100;
    let c: u8 = a + b;
    return c as Int;
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con abort name=divide-by-zero trap=division_by_zero
fn example_value() -> Int {
    let a: Int = 42;
    let z: Int = a - 42;
    return a / z;
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con abort name=shift-count-too-large trap=shift_amount_out_of_range
fn example_value() -> Int {
    let a: u8 = 1;
    let n: u8 = 8;
    let s: u8 = a << n;
    return s as Int;
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

<a id="ARITH-4"></a>
### ARITH-4. Wrapping and saturating arithmetic are named operations

**ARITH-4@1.** `wrapping_add`, `wrapping_sub` and `wrapping_mul` compute the two's-complement result modulo 2 to the operand's width, and `saturating_add`, `saturating_sub` and `saturating_mul` clamp it to the type's range, for every integer type and without a trap.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Pipeline.emit
- Semantics: none
- Obligation: the named operations are typed operations of Core (R-0493)
- Evidence: tested

**Rationale.** Modular arithmetic (hashes, checksums, ring indices) and clamping (accumulators,
sensors) are intentional, so the program says so at the operation. There is no wrapping or
saturating division and no wrapping shift; division has no modular meaning, and a shift by
`n mod width` has had no use.

```con accept name=wrap-and-saturate exit=42
fn example_value() -> Int {
    let a: u8 = 200;
    let w: u8 = wrapping_add(a, 98);
    let s: u8 = saturating_add(a, 100);
    let t: i8 = saturating_sub(-100, 100);
    return (w as Int) + (s as Int) - 255 + (t as Int) + 128;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="ARITH-5"></a>
### ARITH-5. `as` converts without loss

**ARITH-5@2.** A numeric, `bool` or `char` conversion written with `as` is accepted only when every value of the source type converts to a value of the target type that means the same number, `bool` or byte, namely a type to itself, an integer to a wider integer of the same signedness, an unsigned integer to a wider signed one, `bool` to an integer, `char` to an unsigned integer of at least 8 bits or a signed one of at least 16, `u8` to `char`, an integer to a float that represents every value exactly, and `f32` to `f64` (the conversions outside this list that main still accepts, narrowing, a
sign change other than unsigned to wider signed, and float to integer, are this entry's partial part,
refused once R-0500 lands), and every other such conversion is written with a named operation (a checked conversion returning an error, `truncate`, `reinterpret`, or a named float rounding).

- Kind: refusal
- Uses: none
- Codes: E0553 (core-check)
- Status: partial R-0500
- Enforced by: Concrete.ccCheckExpr
- Semantics: none
- Obligation: one `IntArith` authority for conversions (R-0493); a lossless `as` needs no value-preservation obligation
- Evidence: tested

**Rationale.** Decided 2026-10-21 (ARITHMETIC_POLICY §8, exit 2k): `as` is total and
obligation-free, and each lossy conversion names what it does. Version 2 lists the accepted
conversions in the sentence; version 1 gave them here. Today `as` refuses conversions to and from `bool` other than `bool` to an integer
(integer, float and `char` to `bool`, and `bool` to `char`: E0553, bugs 108 and 130). Narrowing
and sign changes are still accepted and truncate or reinterpret, and float to integer truncates
toward zero and aborts on NaN, an infinity or a value outside the target's range (a checked
conversion), until the named operations land and the corpus migrates (about 500 to 700 sites). Pointer and reference casts are not
numeric conversions (TRUST-1 owns the pointer–integer case; which pointer and reference
conversions `as` writes is OPEN-98). The named operations' results are ARITH-10, and their
spellings are OPEN-99.

Until then, on current main, narrowing truncates silently (`300 as u8` is 44, `-1 as u32` is
4294967295, `3.9 as i32` is 3). Even a widening needs `as`: `want_u64(my_u8)` is E0220 (TYPE-4),
so every conversion is visible at the call site.

Decided by the owner on 2026-10-29 (OD11): `as` only when lossless by value domain, and every lossy conversion a named function.

```con reject name=int-to-bool code=E0553 stage=core-check pair=widening
fn main() -> u8 {
    let x: Int = 1;
    let b: bool = x as bool;
    if b {
        return 1;
    }
    return 0;
}
```

```con accept name=widening exit=42
fn example_value() -> Int {
    let a: u8 = 41;
    let b: i32 = a as i32;
    let t: bool = true;
    return (b as Int) + (t as Int);
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con reject name=narrowing code=E0553 stage=core-check pair=widening pending=R-0500 now=accept:44
fn example_value() -> Int {
    let x: Int = 300;
    let t: u8 = x as u8;
    return t as Int;
}
fn main() -> u8 {
    if example_value() == 44 {
        return 44;
    }
    return 1;
}
```

<a id="ARITH-6"></a>
### ARITH-6. Comparisons follow their operand type

**ARITH-6@1.** An integer comparison is signed or unsigned as its operand type is, a float comparison follows IEEE 754 (every ordered comparison with a NaN operand is false, and `x == x` is false for a NaN), and a comparison reads its operands without moving them.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Pipeline.emit, Concrete.checkExprCore
- Semantics: none
- Obligation: no pass folds a float self-comparison (R-0493)
- Evidence: tested

**Rationale.** `(200 : u8) < 100` is false; signedness is a property of the type, never of the
comparison. A comparison between a signed and an unsigned type is refused (TYPE-6), so it never
reinterprets. Which types have comparisons at all is TYPE-6.

```con accept name=unsigned-comparison exit=42
fn example_value() -> Int {
    let a: u8 = 200;
    let b: u8 = 100;
    let c: i8 = -1;
    let d: i8 = 1;
    let mut out: Int = 0;
    if a > b {
        out = out + 40;
    }
    if c < d {
        out = out + 2;
    }
    return out;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="ARITH-7"></a>
### ARITH-7. A float literal is correctly rounded

**ARITH-7@1.** A float literal denotes its decimal value rounded to nearest, ties to even, at the float type it takes, and every consumer uses that value.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: partial bug-332
- Enforced by: Concrete.Pipeline.emit
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** This is ARITH-1's rounding applied to the literal itself: a literal printed with too few digits
in the emitted IR once changed `0.1` (bug 109). The first program below holds only if the literals
are the correctly rounded binary64 values. Main rounds an `f32` literal twice, through binary64,
so a literal just above a binary32 midpoint takes the even neighbour (bug 332), pinned by the
pending example.

```con accept name=float-literal-rounding exit=42
fn main() -> u8 {
    let a: f64 = 0.1;
    let b: f64 = 0.2;
    let c: f64 = 0.30000000000000004;
    if a + b == c {
        return 42;
    }
    return 0;
}
```

```con accept name=f32-literal-rounded-once exit=42 pending=bug-332 now=accept:0
fn main() -> u8 {
    let a: f32 = 1.0000000596046447753906250001;
    let b: f32 = 1.00000011920928955078125;
    if a == b {
        return 42;
    }
    return 0;
}
```

<a id="ARITH-8"></a>
### ARITH-8. The fill of a right shift

**ARITH-8@1.** A right shift that does not abort fills the vacated bits with the sign bit on a signed type and with zeros on an unsigned type.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.IntArith.evalIntShift, Concrete.Pipeline.emit
- Semantics: none
- Obligation: the shift results are `IntArith`'s, the one arithmetic authority (R-0493)
- Evidence: tested

**Rationale.** These are LLVM's `ashr` and `lshr` (the archived ARITHMETIC_POLICY §6). When a shift
aborts instead (a count outside the width) is ARITH-3. The interpreter and the constant folder
compute it in `IntArith`, and the binary through the abort helpers. How an integer division rounds
and which sign a remainder takes is OPEN-114: main truncates toward zero, as C99 does, which the
archived record did not state as a rule.

```con accept name=shift-fill exit=42
fn example_value() -> Int {
    let a: i8 = -128;
    let s: i8 = a >> 7;
    let u: u8 = 128;
    let t: u8 = u >> 7;
    return (s as Int) + (t as Int) + 42;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="ARITH-9"></a>
### ARITH-9. No ambient arithmetic mode

**ARITH-9@1.** No attribute, `#[overflow_checked]` included, and no module or function setting changes what an integer operator or a named arithmetic operation computes, so an operation's mode is given by its spelling alone.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Pipeline.emit, Concrete.Pipeline.lower
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** A reader of `a + b` knows it is checked (ARITH-3) and of `wrapping_add(a, b)` that it
wraps (ARITH-4) without looking for an enclosing `#[wrapping]` attribute or module switch; none
exists. `#[overflow_checked]` asks the proof pipeline for a no-overflow obligation at each
fixed-width `+`, `-` and `*` of the function, which is evidence, not meaning: the operations still
abort on overflow. Whether a build profile may omit a check or refuse an unproved operation is
OPEN-97.

```con accept name=overflow-checked-in-range exit=42
#[overflow_checked]
fn add(a: u8, b: u8) -> u8 {
    return a + b;
}

fn example_value() -> Int {
    return add(40, 2) as Int;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con abort name=overflow-checked-still-traps trap=arithmetic_overflow
#[overflow_checked]
fn add(a: u8, b: u8) -> u8 {
    return a + b;
}

fn example_value() -> Int {
    return add(200, 100) as Int;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=named-operations-unchanged exit=42
#[overflow_checked]
fn named(a: u8, b: u8) -> Int {
    let w: u8 = wrapping_add(a, b);
    let s: u8 = saturating_add(a, b);
    return (w as Int) + (s as Int);
}

fn example_value() -> Int {
    return named(200, 100) - 257;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="ARITH-10"></a>
### ARITH-10. The named conversions' results

**ARITH-10@1.** `truncate::<T>(x)` keeps the low-order bits of `x`'s two's-complement representation, as many as `T`'s width, `reinterpret::<T>(x)` changes the signedness of the integer `x` at the same width and keeps its bit pattern, and `try_convert::<T>(x)` gives the value of `T` that means the same number as `x` when there is one and otherwise reports that there is none (an integer out of range, a float that is not integral, out of range, NaN or infinite, or a value a target float does not represent exactly).

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0500
- Semantics: none
- Obligation: the named conversions are typed operations of the one arithmetic authority (R-0493)
- Evidence: tested

**Rationale.** These are the conversions ARITH-5 sends every lossy `as` to, each with one stated
result, so that no consumer has to guess what a lossy conversion means (bugs 080 and 128).
`truncate::<u8>(300)` is 44 and `reinterpret::<u32>(-1)` is 4294967295, what a narrowing or
sign-changing `as` gives today. Whether `try_convert` reports a failure as `Option::None` or as a
`Result` error with a reason is OPEN-106; the archived record allowed either. The float conversions
are ARITH-11. None of these exists on main yet (R-0500), and their spellings are OPEN-99.

```con accept name=truncate-and-reinterpret exit=42 pending=R-0500 now=reject:E0101:resolve
fn example_value() -> Int {
    let x: Int = 300;
    let t: u8 = truncate::<u8>(x);
    let m: i32 = -1;
    let u: u32 = reinterpret::<u32>(m);
    if u == 4294967295 {
        return (t as Int) - 2;
    }
    return 0;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="ARITH-11"></a>
### ARITH-11. The named float conversions round as stated

**ARITH-11@1.** A named conversion from an integer to a float, or from `f64` to `f32`, rounds to the nearest representable value with ties to even, and each float-to-integer rounding operation (`trunc`, `round`, `floor`, `ceil`) has a stated result for every input, NaN and the infinities included.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0500
- Semantics: none
- Obligation: the named conversions are typed operations of the one arithmetic authority (R-0493)
- Evidence: tested

**Rationale.** These are the conversions ARITH-5 sends a rounding `as` to (`i32` to `f32`, `Int` to
`f64`, `f64` to `f32`, every float to an integer). Today `as` rounds an integer to a float natively
and is the identity in `--interp` (bug 128), and a float-to-integer `as` truncates toward zero and
aborts on NaN, an infinity or a value out of range. Whether a rounding operation gives a value or an
error for those inputs, and which value, is OPEN-107. None of these exists on main yet (R-0500).
The conversion functions are not named yet (OPEN-99), so this entry has no example until they are.

<a id="ARITH-12"></a>
### ARITH-12. Float remainder

**ARITH-12@1.** `x % y` on floats is C's `fmod(x, y)`: the exact value `x - n * y` for the integer `n` that is `x / y` truncated toward zero, which has the sign of `x`, and NaN when `y` is zero or `x` is infinite or either is NaN.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Interp.floatRem, Concrete.Pipeline.emit
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** It is LLVM's `frem`, which the binary emits, and the interpreter computes the same
value. Unlike IEEE 754's `remainder`, it truncates rather than rounding the quotient to nearest.
Absorbed from [INTERPRETER_TRUST.md](../verification/INTERPRETER_TRUST.md) §2.

```con accept name=float-remainder exit=42
fn main() -> u8 {
    let a: f64 = -7.5;
    let b: f64 = 2.0;
    let r: f64 = a % b;
    if r == -1.5 {
        return 42;
    }
    return 1;
}
```

<a id="ARITH-13"></a>
### ARITH-13. A division by a proven zero or a proven overflow is refused

**ARITH-13@1.** An integer `/` or `%` whose divisor the compiler proves zero, and an integer operation the compiler proves always overflows its type, are refused in a function that neither is marked `trusted` nor declares `Unsafe`; whether they are also refused in a function marked `trusted` or declaring `Unsafe` is OPEN-118.

- Kind: refusal
- Uses: none
- Codes: E0901 (runtime-safety), E0902 (runtime-safety)
- Status: partial bug-340
- Enforced by: Concrete.Report.provenViolationDiagnostics
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** These are the compile-time half of ARITH-3's traps: what the compiler can decide
is refused, and everything else traps at run time. E0901 is a divisor that evaluates to the
constant zero. E0902 is never reported. The folder takes only an operation whose operands are all literals, and
the type lookup takes only an identifier or a cast leaf, so no operation reaches the proof (bug 350).
Probed: `return (2147483647 as i32) + 1;` in an `#[overflow_checked]` function compiles, and `let c:
u8 = 200 + 100` traps at run time (ARITH-3). Inside a `trusted` function or one declaring `Unsafe`,
the current compiler compiles a proven zero divisor, and it aborts at run time; the sentence leaves
that case to OPEN-118. The native build refuses both reject examples below. `--interp` runs them, the same divergence as
bug 340's: it stops at the division in the first, so that example is checked natively only, and it
exits 42 in the second, whose division is on a path that does not run, so that example is pending
until the bug is fixed.

```con accept name=divisor-not-zero exit=42
fn example_value() -> Int {
    let n: Int = 84;
    return n / 2;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con reject name=divisor-proven-zero code=E0901 stage=runtime-safety pair=divisor-not-zero interp-exempt=interp-runs-proven-violations
fn example_value() -> Int {
    let n: Int = 84;
    return n / 0;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con reject name=divisor-proven-zero-unrun code=E0901 stage=runtime-safety pair=divisor-not-zero pending=bug-340 now=reject:E0901:runtime-safety now-interp=accept:42
fn example_value() -> Int {
    let n: Int = 84;
    let c: bool = false;
    if c {
        return n / 0;
    }
    return 42;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```
