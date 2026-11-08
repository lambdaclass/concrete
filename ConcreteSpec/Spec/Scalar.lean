import ConcreteSpec.Spec.Outcome

/-!
# Scalars, failure and arithmetic

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). Part of `ConcreteSpec.Spec`;
see `ConcreteSpec/README.md`. Foundation contract C2 (the foundation proposal approved by the owner
on 2026-11-08, "ok all recommended").

An integer value *is* a mathematical integer within its type's range; its bits are a separate,
derived thing (`toBits`, `ofBits`). The widths are the ones the type names say, `i8` … `i64` and
`u8` … `u64` (TYPE-19); there is no target-sized integer here (question 7 keeps `u64` lengths, Q3.5).
Signed values are two's complement.

**Operations** (no hidden traps, 2026-11-06; OD10, OD99, OD114, ARITH-4):

- *Checked* operations (`checked`) return `Except ArithmeticError`: the decided
  `Result<T, ArithmeticError>`, with `ArithmeticError { Overflow, DivisionByZero, InvalidShift }`.
  A handled error is an ordinary value, never a trap.
- *Plain* operations (`plain`) mean the same, but a failure is a trap of the matching kind. Safe code
  is admitted only with closed evidence that a plain operation does not fail; trusted code may reach
  the trap.
- *Wrapping* and *saturating* add, subtract and multiply (`wrapping`, `saturating`) never fail: they
  compute modulo `2^bits`, or clamp to the range.
- Division truncates toward zero and a nonzero remainder takes the dividend's sign (OD114). A zero
  divisor is `DivisionByZero`; signed `MIN / -1` and `MIN % -1` are `Overflow`.
- A shift count `c` is valid when `0 ≤ c < bits`; otherwise `InvalidShift`. A valid left shift
  discards the bits shifted out; a right shift sign-fills a signed value and zero-fills an unsigned
  one.
- Bitwise and, or, xor and not act on the bit patterns.

**Conversions.** The one canonical lossless conversion (owner decision 2026-11-08, item 11(a),
replacing `as`; its spelling is not yet approved) is allowed exactly when every value of the source
type is a value of the target type (OD11's value-domain rule, ARITH-5), and it never changes the
value: between integer types exactly when the source range lies inside the target's (`Lossless`),
and from `bool` to every integer type, `false` as `0` and `true` as `1` (`boolToInt`). The checked
conversion `try_convert` returns `Except OutOfRange` (OD106), a different error type from
`ArithmeticError`. A lossy integer conversion keeps the low bits of the target's
width (`wrap`); the names of the lossy conversions are OD99's.

**Errors are not trap kinds.** `ArithmeticError` and `OutOfRange` are values a program handles;
`TrapKind` (in `Outcome`) names why a run stopped. `ArithmeticError.trapKind` is the one map from
the first to the second, used by the plain operations (owner decision 2026-11-08, item 4).

**Contracts: machine and exact arithmetic** (OD12, OD145; owner decision 2026-11-08, item 10). A
contract term says which arithmetic it means: a *machine* operation at a width (`CTerm.machine`),
which means exactly the runtime operation and is undefined where that one fails, or *exact*
arithmetic on mathematical integers (`CTerm.exact`), which never overflows. The surface spelling of
the exact boundary is not yet approved by the owner; `exact` here is a neutral name for its
meaning only.

Floats come at slice 26; nothing here decides their rules.
-/

namespace ConcreteSpec.Core

/-- The integer types (TYPE-19). -/
inductive IntTy where
  | i8 | i16 | i32 | i64
  | u8 | u16 | u32 | u64
  deriving DecidableEq, Repr

namespace IntTy

/-- The width in bits, as the name says. -/
def bits : IntTy → Nat
  | .i8 | .u8 => 8
  | .i16 | .u16 => 16
  | .i32 | .u32 => 32
  | .i64 | .u64 => 64

/-- Whether the type is signed. -/
def signed : IntTy → Bool
  | .i8 | .i16 | .i32 | .i64 => true
  | _ => false

/-- The smallest value: `-2^(bits-1)` for a signed type, `0` for an unsigned one. -/
def min (t : IntTy) : Int := if t.signed then -((2 : Int) ^ (t.bits - 1)) else 0

/-- The largest value: `2^(bits-1) - 1` for a signed type, `2^bits - 1` for an unsigned one. -/
def max (t : IntTy) : Int :=
  if t.signed then (2 : Int) ^ (t.bits - 1) - 1 else (2 : Int) ^ t.bits - 1

/-- `n` is a value of type `t`. -/
def InRange (t : IntTy) (n : Int) : Prop := t.min ≤ n ∧ n ≤ t.max

instance (t : IntTy) (n : Int) : Decidable (t.InRange n) := by unfold InRange; infer_instance

/-- Every integer type, once each. -/
def all : List IntTy := [.i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64]

end IntTy

/-! ## Bits -/

/-- The bit pattern of `n` in `t`: `n` modulo `2^bits`, a number below `2^bits` (two's
complement for a negative `n`). -/
def toBits (t : IntTy) (n : Int) : Nat := (n % (2 : Int) ^ t.bits).toNat

/-- The value a bit pattern `m` denotes in `t`: `m`, or `m - 2^bits` for a signed type whose top
bit is set. -/
def ofBits (t : IntTy) (m : Nat) : Int :=
  if t.signed = true ∧ m ≥ 2 ^ (t.bits - 1) then (m : Int) - (2 : Int) ^ t.bits else (m : Int)

/-- `n` reduced into `t`'s range modulo `2^bits`: what a wrapping operation and a lossy integer
conversion give. -/
def wrap (t : IntTy) (n : Int) : Int := ofBits t (toBits t n)

/-- `n` clamped to `t`'s range: what a saturating operation gives. -/
def saturate (t : IntTy) (n : Int) : Int :=
  if n < t.min then t.min else if t.max < n then t.max else n

/-- The bit pattern of a `bool`: `0` for `false`, `1` for `true` (TYPE-19, OD126). C4 lays it into
one byte; the old fragment `Rep` already encodes it so. -/
def boolToBits (b : Bool) : Nat := if b then 1 else 0

/-- The `bool` a bit pattern denotes: only `0` and `1` are `bool` values; any other pattern is not
a valid `bool` (reading one is a refused representation, never a value). -/
def boolOfBits : Nat → Option Bool
  | 0 => some false
  | 1 => some true
  | _ => none

/-! ## Errors -/

/-- The error a checked arithmetic operation returns (the no-hidden-traps decision's spellings,
2026-11-06). -/
inductive ArithmeticError where
  | overflow
  | divisionByZero
  | invalidShift
  deriving DecidableEq, Repr

/-- The error `try_convert` returns (OD106): the value is not a value of the target type. -/
inductive OutOfRange where
  | outOfRange
  deriving DecidableEq, Repr

/-- The trap kind of a plain operation that fails with this error. -/
def ArithmeticError.trapKind : ArithmeticError → TrapKind
  | .overflow => .overflow
  | .divisionByZero => .divisionByZero
  | .invalidShift => .invalidShift

/-! ## Operations -/

/-- The binary arithmetic operations. -/
inductive ArithOp where
  | add | sub | mul | div | rem | shl | shr
  deriving DecidableEq, Repr

/-- The operations that have wrapping and saturating forms (ARITH-4). -/
inductive WrapOp where
  | add | sub | mul
  deriving DecidableEq, Repr

/-- The bitwise operations. -/
inductive BitOp where
  | and | or | xor
  deriving DecidableEq, Repr

/-- The comparison operators. -/
inductive CmpOp where
  | eq | ne | lt | le | gt | ge
  deriving DecidableEq, Repr

/-- A comparison of two integers. -/
def CmpOp.eval : CmpOp → Int → Int → Bool
  | .eq, a, b => a == b
  | .ne, a, b => a != b
  | .lt, a, b => decide (a < b)
  | .le, a, b => decide (a ≤ b)
  | .gt, a, b => decide (b < a)
  | .ge, a, b => decide (b ≤ a)

/-- The exact result of a wrapping-capable operation. -/
def WrapOp.exact : WrapOp → Int → Int → Int
  | .add, a, b => a + b
  | .sub, a, b => a - b
  | .mul, a, b => a * b

/-- A result, if it is a value of `t`; `Overflow` otherwise. -/
def inRangeOr (t : IntTy) (r : Int) : Except ArithmeticError Int :=
  if t.InRange r then .ok r else .error .overflow

/-- A shift count is valid when `0 ≤ c < bits`. -/
def validShift (t : IntTy) (c : Int) : Bool := decide (0 ≤ c ∧ c < t.bits)

/-- The checked operations: the result, or the reason there is none. Division truncates toward
zero (`Int.tdiv`) and the remainder takes the dividend's sign (`Int.tmod`); signed `MIN % -1` is
`Overflow`, as decided, although its mathematical remainder is `0`. A left shift keeps the low
`bits` bits; a right shift rounds toward negative infinity, which sign-fills a negative signed value
and zero-fills a non-negative one. -/
def checked (t : IntTy) : ArithOp → Int → Int → Except ArithmeticError Int
  | .add, a, b => inRangeOr t (a + b)
  | .sub, a, b => inRangeOr t (a - b)
  | .mul, a, b => inRangeOr t (a * b)
  | .div, a, b => if b = 0 then .error .divisionByZero else inRangeOr t (a.tdiv b)
  | .rem, a, b =>
    if b = 0 then .error .divisionByZero
    else if t.signed = true ∧ a = t.min ∧ b = -1 then .error .overflow
    else .ok (a.tmod b)
  | .shl, a, c => if validShift t c then .ok (wrap t (a * (2 : Int) ^ c.toNat)) else .error .invalidShift
  | .shr, a, c => if validShift t c then .ok (a / (2 : Int) ^ c.toNat) else .error .invalidShift

/-- Checked negation: `Overflow` for signed `MIN`, and for any nonzero unsigned value. -/
def checkedNeg (t : IntTy) (a : Int) : Except ArithmeticError Int := inRangeOr t (-a)

/-- Plain negation: checked negation, with `Overflow` a trap. -/
def plainNeg (t : IntTy) (a : Int) : Res Int :=
  match checkedNeg t a with
  | .ok r => .ok r
  | .error e => .trap e.trapKind

/-- A plain operation: the checked one, with a failure turned into a trap of its kind. -/
def plain (t : IntTy) (op : ArithOp) (a b : Int) : Res Int :=
  match checked t op a b with
  | .ok r => .ok r
  | .error e => .trap e.trapKind

/-- Wrapping add, subtract and multiply: modulo `2^bits`, never failing. -/
def wrapping (t : IntTy) (op : WrapOp) (a b : Int) : Int := wrap t (op.exact a b)

/-- Saturating add, subtract and multiply: clamped to the range, never failing. -/
def saturating (t : IntTy) (op : WrapOp) (a b : Int) : Int := saturate t (op.exact a b)

/-- A bitwise operation on the bit patterns. -/
def bitwise (t : IntTy) : BitOp → Int → Int → Int
  | .and, a, b => ofBits t (toBits t a &&& toBits t b)
  | .or, a, b => ofBits t (toBits t a ||| toBits t b)
  | .xor, a, b => ofBits t (toBits t a ^^^ toBits t b)

/-- Bitwise not: every bit flipped. -/
def bitNot (t : IntTy) (a : Int) : Int := ofBits t (2 ^ t.bits - 1 - toBits t a)

/-! ## Conversions -/

/-- The lossless conversion from `s` to `t` is allowed: every value of `s` is a value of `t`. -/
def Lossless (s t : IntTy) : Prop := t.min ≤ s.min ∧ s.max ≤ t.max

instance (s t : IntTy) : Decidable (Lossless s t) := by unfold Lossless; infer_instance

/-- The lossless conversion of a `bool` to an integer type: `0` or `1`, a value of every integer
type (ARITH-5). -/
def boolToInt (b : Bool) : Int := if b then 1 else 0

/-- The checked conversion: the same value if it is a value of `t` (OD106). -/
def tryConvert (t : IntTy) (n : Int) : Except OutOfRange Int :=
  if t.InRange n then .ok n else .error .outOfRange

/-- A lossy integer conversion to `t`: the low `bits` bits of the value, read in `t` (`wrap`). The
surface names of the lossy conversions are OD99's; this is their meaning only. -/
def lossyConvert (t : IntTy) (n : Int) : Int := wrap t n

/-! ## Contracts: machine and exact arithmetic -/

/-- A machine operation in a contract: plain (undefined where it would fail), or one of the named
wrapping or saturating operations (OD145). -/
inductive MachineOp where
  | plain (op : ArithOp)
  | wrapping (op : WrapOp)
  | saturating (op : WrapOp)
  deriving DecidableEq, Repr

/-- Whether the second operand is a shift count, which follows its own rule (`validShift`) and is
not required to be a value of the operation's type. -/
def MachineOp.countOperand : MachineOp → Bool
  | .plain .shl | .plain .shr => true
  | _ => false

/-- The operands a machine operation at width `t` is defined on: values of `t`; a shift count is
judged by the operation itself. -/
def MachineOp.operandsOk (t : IntTy) (op : MachineOp) (x y : Int) : Bool :=
  decide (t.InRange x) && (op.countOperand || decide (t.InRange y))

/-- An arithmetic term of a contract. `machine` means the runtime operation at width `t`; `exact`
means mathematical integers. The two are distinct constructors so a reader, a checker and a proof
always know which is meant (owner decision 2026-11-08, item 10; the source spelling of `exact` is
still to be approved). -/
inductive CTerm where
  | lit (n : Int)
  | var (x : Var)
  | machine (t : IntTy) (op : MachineOp) (a b : CTerm)
  | exact (op : WrapOp) (a b : CTerm)

/-- What a contract term denotes, given the values of its variables. `none` where a machine
operation is not defined: an operand that is not a value of its width (the runtime never sees one),
or a plain operation that would fail. A contract that uses such a term carries the obligation that
it is defined. -/
def CTerm.eval (ρ : Var → Option Int) : CTerm → Option Int
  | .lit n => some n
  | .var x => ρ x
  | .machine t op a b => (a.eval ρ).bind fun x => (b.eval ρ).bind fun y =>
    if op.operandsOk t x y = false then none else
    match op with
    | .plain o => match checked t o x y with
      | .ok r => some r
      | .error _ => none
    | .wrapping o => some (wrapping t o x y)
    | .saturating o => some (saturating t o x y)
  | .exact op a b => (a.eval ρ).bind fun x => (b.eval ρ).map fun y => op.exact x y

/-- The exact operation a plain machine add, subtract or multiply means where it is defined. -/
def MachineOp.exactOf? : MachineOp → Option WrapOp
  | .plain .add => some .add
  | .plain .sub => some .sub
  | .plain .mul => some .mul
  | _ => none

/-- A contract term with every plain machine add, subtract and multiply read as exact arithmetic.
Where the original is defined, this one is too, with the same value (`eval_toExact`): a proof may
reason in exact arithmetic once it has the obligations that the machine operations do not fail. -/
def CTerm.toExact : CTerm → CTerm
  | .lit n => .lit n
  | .var x => .var x
  | .machine t op a b => match op.exactOf? with
    | some w => .exact w a.toExact b.toExact
    | none => .machine t op a.toExact b.toExact
  | .exact op a b => .exact op a.toExact b.toExact

/-! ### Examples -/

example : IntTy.all.map IntTy.bits = [8, 16, 32, 64, 8, 16, 32, 64] := rfl

example : IntTy.i8.min = -128 ∧ IntTy.i8.max = 127 ∧ IntTy.u8.max = 255 := by decide

/-- `-2 : i16` is the bit pattern `0xfffe`, and back. -/
example : toBits .i16 (-2) = 65534 ∧ ofBits .i16 65534 = -2 := by decide

/-- Overflow is an error value of a checked operation; the plain operation traps with kind
`overflow`. -/
example : checked .u8 .add 200 56 = .error .overflow := by decide

example : plain .u8 .add 200 56 = .trap .overflow := by decide

example : checked .u8 .add 200 55 = .ok 255 := by decide

/-- Division truncates toward zero; the remainder takes the dividend's sign (OD114). -/
example : checked .i32 .div (-7) 2 = .ok (-3) ∧ checked .i32 .rem (-7) 2 = .ok (-1) ∧
    checked .i32 .rem 7 (-2) = .ok 1 := by decide

/-- A zero divisor, and signed `MIN / -1` and `MIN % -1`. -/
example : checked .i8 .div 5 0 = .error .divisionByZero := by decide

example : checked .i8 .div (-128) (-1) = .error .overflow ∧
    checked .i8 .rem (-128) (-1) = .error .overflow := by decide

/-- Shifts: the count must be below the width; a valid left shift discards outgoing bits; a right
shift sign-fills a signed value and zero-fills an unsigned one. -/
example : checked .u8 .shl 1 8 = .error .invalidShift ∧ checked .u8 .shl 1 (-1) = .error .invalidShift
    := by decide

example : checked .u8 .shl 0x81 1 = .ok 2 ∧ checked .i8 .shl 64 1 = .ok (-128) := by decide

example : checked .i8 .shr (-128) 7 = .ok (-1) ∧ checked .u8 .shr 128 7 = .ok 1 := by decide

/-- Wrapping and saturating forms never fail. -/
example : wrapping .u8 .add 200 56 = 0 ∧ saturating .u8 .add 200 56 = 255 ∧
    wrapping .i8 .mul 16 8 = -128 ∧ saturating .i8 .sub (-100) 100 = -128 := by decide

/-- Bitwise operations act on bit patterns. -/
example : bitwise .i8 .and (-1) 0x0f = 15 ∧ bitNot .u8 0 = 255 ∧ bitNot .i8 0 = -1 := by decide

/-- The lossless conversion is allowed only when the whole source range fits: `u8` to `i16` yes,
`i8` to `u8` no (`-1` has no `u8` value), `u32` to `i32` no. -/
example : Lossless .u8 .i16 ∧ ¬ Lossless .i8 .u8 ∧ ¬ Lossless .u32 .i32 := by decide

/-- `try_convert` reports `OutOfRange`, a value, not a trap. -/
example : tryConvert .u8 300 = .error .outOfRange ∧ tryConvert .u8 255 = .ok 255 := by decide

/-- In a contract, machine `u8` addition of 255 and 1 is undefined; exact addition is 256. -/
example : (CTerm.machine .u8 (.plain .add) (.lit 255) (.lit 1)).eval (fun _ => none) = none ∧
    (CTerm.exact .add (.lit 255) (.lit 1)).eval (fun _ => none) = some 256 ∧
    (CTerm.machine .u8 (.wrapping .add) (.lit 255) (.lit 1)).eval (fun _ => none) = some 0 := by
  decide

/-- A machine term is not defined on an operand outside its width: `300` is no `u8`, so even the
wrapping sum is undefined. A shift count need not be a value of the type, only a valid count. -/
example : (CTerm.machine .u8 (.wrapping .add) (.lit 300) (.lit 1)).eval (fun _ => none) = none ∧
    (CTerm.machine .u8 (.plain .shl) (.lit 1) (.lit 7)).eval (fun _ => none) = some 128 := by
  decide

/-- `bool` patterns: only `0` and `1`. -/
example : boolToBits true = 1 ∧ boolOfBits 0 = some false ∧ boolOfBits 2 = none := by decide

/-- Negation: `-(-128)` overflows `i8` (there is no wrapping negation: ARITH-4 has wrapping add,
subtract and multiply only). A lossy conversion keeps the low bits: `300` to `u8` is `44`, `255`
to `i8` is `-1`. -/
example : plainNeg .i8 (-128) = .trap .overflow ∧ plainNeg .i8 5 = .ok (-5) ∧
    lossyConvert .u8 300 = 44 ∧ lossyConvert .i8 255 = -1 := by decide

end ConcreteSpec.Core
