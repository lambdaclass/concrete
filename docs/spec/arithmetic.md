# Arithmetic

The integer types are `Int` (`i64`), `Uint` (`u64`), `i32`, `u32`, `i16`, `u16`, `i8` and `u8`;
the floating-point types are `f64` and `f32`. Every ordinary integer operation is checked: it
traps on a result its type cannot represent, and the trap is an abort (STMT-2). Wrapping and
saturating arithmetic are named operations. There is no implicit numeric conversion (TYPE-4), and
which operand types an operator takes is TYPE-6. The design records are
[ARITHMETIC_POLICY.md](../language/ARITHMETIC_POLICY.md) (NUMERIC_MODEL.md was absorbed into
ARITH-2, ARITH-3 and ARITH-5 and deleted); the profile mapping, report classification and
proof boundaries stay there.

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

**ARITH-2@2.** An integer literal takes the integer type its position expects, a float literal the float type its position expects, a literal with no expected type is refused, and an integer literal whose value, negation included, is outside its type's range is refused rather than truncated.

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
expected is removed, no suffix is required, and a float literal follows the same principle (its
context's float type, refused with none). Main still types an unconstrained integer literal `Int` and
an unconstrained float literal `Float64` (bug 327).


```con reject name=literal-out-of-range code=E0227 stage=check pair=literal-in-range
fn main() -> Int {
    let a: u8 = 300;
    return a as Int;
}
```

```con accept name=literal-in-range exit=42
fn main() -> Int {
    let a: i8 = -128;
    let b: u8 = 0xFF;
    return (a as Int) + (b as Int) - 85;
}
```

```con reject name=unconstrained-literal code=new stage=check pair=constrained-literal pending=bug-327 now=accept:42
fn main() -> Int {
    if 3 < 4 {
        return 42;
    }
    return 0;
}
```

```con accept name=constrained-literal exit=42
fn main() -> Int {
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
changes reports, never code generation. A right shift is arithmetic for a signed type and logical
for an unsigned one. Integer division truncates toward zero (`-7 / 2` is `-3`), and a remainder
has the sign of its dividend. Constant folding may refuse an operation it proves always overflows (E0900),
but it does not see every such operation, so the run-time trap is the guarantee. Array bounds are
checked the same way, an index out of bounds aborting.

For example `let c: u8 = 200 + 100` compiles and traps at run time: the folder proves overflow
only for what it recognizes, so E0900 is a partial guarantee (`provenViolationDiagnostics`, the
same path as a proven out-of-bounds index or zero divisor); completing the folder is ROADMAP #10.

Decided by the owner on 2026-10-29 (OD10): integer arithmetic is checked by default and the shift count is checked; wrapping and saturating arithmetic are named operations (ARITH-4), and an abort runs no deferred call (STMT-2).

```con accept name=checked-in-range exit=42
fn main() -> Int {
    let a: u8 = 200;
    let s: u8 = a << 1;
    let q: Int = -7 / 2;
    return (s as Int) - 99 + q;
}
```

```con abort name=unsigned-overflow trap=arithmetic_overflow
fn main() -> Int {
    let a: u8 = 200;
    let b: u8 = 100;
    let c: u8 = a + b;
    return c as Int;
}
```

```con abort name=divide-by-zero trap=division_by_zero
fn main() -> Int {
    let a: Int = 42;
    let z: Int = a - 42;
    return a / z;
}
```

```con abort name=shift-count-too-large trap=shift_amount_out_of_range
fn main() -> Int {
    let a: u8 = 1;
    let n: u8 = 8;
    let s: u8 = a << n;
    return s as Int;
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
fn main() -> Int {
    let a: u8 = 200;
    let w: u8 = wrapping_add(a, 98);
    let s: u8 = saturating_add(a, 100);
    let t: i8 = saturating_sub(-100, 100);
    return (w as Int) + (s as Int) - 255 + (t as Int) + 128;
}
```

<a id="ARITH-5"></a>
### ARITH-5. `as` converts without loss

**ARITH-5@1.** A numeric, `bool` or `char` conversion written with `as` is accepted only when every value of the source type converts to a value of the target type that means the same number, `bool` or byte, and every other such conversion is written with a named operation (a checked conversion returning an error, `truncate`, `reinterpret`, or a named float rounding).

- Kind: refusal
- Uses: none
- Codes: E0553 (core-check)
- Status: partial R-0500
- Enforced by: Concrete.ccCheckExpr
- Semantics: none
- Obligation: one `IntArith` authority for conversions (R-0493); a lossless `as` needs no value-preservation obligation
- Evidence: tested

**Rationale.** Decided 2026-10-21 (ARITHMETIC_POLICY §8, exit 2k): `as` is total and
obligation-free, and each lossy conversion names what it does. The accepted conversions are a type
to itself, an integer to a wider integer of the same signedness, an unsigned integer to a wider
signed one, `bool` to an integer, `char` to an unsigned integer of at least 8 bits or a signed one
of at least 16, `u8` to `char`, an integer to a float that represents every value exactly, and
`f32` to `f64`. Today `as` refuses conversions to and from `bool` other than `bool` to an integer
(integer, float and `char` to `bool`, and `bool` to `char`: E0553, bugs 108 and 130). Narrowing
and sign changes are still accepted and truncate or reinterpret, and float to integer truncates
toward zero and aborts on NaN, an infinity or a value outside the target's range (a checked
conversion), until the named operations land and the corpus migrates (about 500 to 700 sites). Pointer and reference casts are not
numeric conversions (TRUST-1 owns the pointer–integer case).

Until then, on current main, narrowing truncates silently (`300 as u8` is 44, `-1 as u32` is
4294967295, `3.9 as i32` is 3). Even a widening needs `as`: `want_u64(my_u8)` is E0220 (TYPE-4),
so every conversion is visible at the call site.

Decided by the owner on 2026-10-29 (OD11): `as` only when lossless by value domain, and every lossy conversion a named function.

```con reject name=int-to-bool code=E0553 stage=core-check pair=widening
fn main() -> Int {
    let x: Int = 1;
    let b: bool = x as bool;
    if b {
        return 1;
    }
    return 0;
}
```

```con accept name=widening exit=42
fn main() -> Int {
    let a: u8 = 41;
    let b: i32 = a as i32;
    let t: bool = true;
    return (b as Int) + (t as Int);
}
```

```con reject name=narrowing code=E0553 stage=core-check pair=widening pending=R-0500 now=accept:44
fn main() -> Int {
    let x: Int = 300;
    let t: u8 = x as u8;
    return t as Int;
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
fn main() -> Int {
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
fn main() -> Int {
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
fn main() -> Int {
    let a: f32 = 1.0000000596046447753906250001;
    let b: f32 = 1.00000011920928955078125;
    if a == b {
        return 42;
    }
    return 0;
}
```
