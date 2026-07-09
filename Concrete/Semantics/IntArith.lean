import Concrete.Frontend.AST

/-!
# Integer arithmetic semantics — the single source of truth

Phase 6.5 #1. Integer arithmetic meaning was hand-maintained in several stages:
the interpreter (`Interp.evalBinOp`), constant folding / DCE (`SSACleanup`),
and backend checked-helper selection (`EmitSSA`). Each had its own copy of "bit
width", "in range?", "does this fold trap?". When those drift, a fold or a
helper can disagree with the interpreter — which is the oracle for differential
testing — and a documented trap can silently vanish (three such soundness holes
were found and fixed when the checked-arithmetic flip landed; see
`docs/ARITHMETIC_POLICY.md`).

This module is the one place that decides:
* `intBitWidth` — bit width and signedness of each fixed-width integer type;
* `intRange` — the inclusive representable range;
* `checkedToType` / `wrapToType` / `saturateToType` / `maskWidth` — the four
  value-normalisation operations backing checked / `wrapping_*` / `saturating_*`
  / unsigned-mask semantics;
* `tdiv` / `tmod` — truncated division and remainder (LLVM `sdiv`/`srem`);
* `evalIntBinOp` — the tri-state evaluation of a binary op on two integer
  values, whose result is one of {value, trap, not-applicable}.

`Interp` evaluates through `evalIntBinOp`; `SSACleanup` asks `foldIntBinOp`
whether a constant fold is legal AND whether it would trap; `EmitSSA` derives
its checked-helper width from `llvmBitWidth`. There is intentionally NO new
policy here — every definition reproduces the behaviour the stages already had,
now stated once.

This is deliberately a leaf module (imports only the AST) so every stage can
depend on it without a cycle.
-/

namespace Concrete
namespace IntArith

/-- `(bit width, signed?)` for each fixed-width integer type, including the
    64-bit `Int`/`Uint`. `none` for non-integer types (and `char`, which is not
    an arithmetic integer type). This is the SEMANTIC width used for range,
    overflow, and wrap decisions — distinct from `llvmBitWidth`, the total
    codegen width. -/
def intBitWidth : Ty → Option (Nat × Bool)
  | .i8 => some (8, true)  | .i16 => some (16, true)  | .i32 => some (32, true)  | .int  => some (64, true)
  | .u8 => some (8, false) | .u16 => some (16, false) | .u32 => some (32, false) | .uint => some (64, false)
  | _   => none

/-- Is `ty` a fixed-width (or `Int`/`Uint`) integer type? -/
def isIntTy (ty : Ty) : Bool := (intBitWidth ty).isSome

/-- Is `ty` a signed integer type? (false for non-integer types) -/
def isSignedInt (ty : Ty) : Bool :=
  match intBitWidth ty with
  | some (_, s) => s
  | none => false

/-- Bit width of an unsigned fixed-width integer type, if any. -/
def unsignedBitWidth (ty : Ty) : Option Nat :=
  match intBitWidth ty with
  | some (w, false) => some w
  | _ => none

/-- Inclusive `(min, max)` representable range of an integer type. `none` for
    non-integer types. -/
def intRange : Ty → Option (Int × Int)
  | ty =>
    match intBitWidth ty with
    | some (w, signed) =>
      if signed then some (-((2 : Int) ^ (w - 1)), (2 : Int) ^ (w - 1) - 1)
      else some (0, (2 : Int) ^ w - 1)
    | none => none

/-- Total codegen bit width used for LLVM type emission (`iN`). Unlike
    `intBitWidth`, this is total: `char` is `i8` and any non-integer falls back
    to 64. This is a *representation* fact, not an arithmetic-semantics fact, so
    it is kept separate from `intBitWidth`. -/
def llvmBitWidth : Ty → Nat
  | .i8 | .u8 | .char => 8
  | .i16 | .u16 => 16
  | .i32 | .u32 => 32
  | _ => 64

/-- True iff `n` fits `ty`'s representable range. For non-fixed-width types
    (not an integer type) this is vacuously `true` — callers that fold on
    non-integer constants keep their prior behaviour. -/
def fitsType (ty : Ty) (n : Int) : Bool :=
  match intRange ty with
  | some (lo, hi) => lo ≤ n && n ≤ hi
  | none => true

/-- Checked normalisation: `some n` if `n` fits `ty`'s range, else `none`
    (overflow → trap). This is what ordinary `+ - * / %` mean. -/
def checkedToType (ty : Ty) (n : Int) : Option Int :=
  match intRange ty with
  | some (lo, hi) => if n < lo || n > hi then none else some n
  | none => some n

/-- True two's-complement wrap of `n` into `ty`'s width, for BOTH signed and
    unsigned — what `wrapping_add/sub/mul` mean and what plain LLVM add/sub/mul
    produce. -/
def wrapToType (ty : Ty) (n : Int) : Int :=
  match intBitWidth ty with
  | some (w, signed) => if signed then (BitVec.ofInt w n).toInt else Int.ofNat (BitVec.ofInt w n).toNat
  | none             => n

/-- Clamp `n` to `ty`'s representable range — what `saturating_*` mean, matching
    the `llvm.{s,u}{add,sub}.sat` intrinsics. -/
def saturateToType (ty : Ty) (n : Int) : Int :=
  match intRange ty with
  | some (lo, hi) => if n < lo then lo else if n > hi then hi else n
  | none => n

/-- Wrap a result to its type's width for UNSIGNED fixed-width types only
    (leaves signed / `Int` as mathematical `Int`). Used for the remainder
    result, matching the proof model's BitVec round-trip. -/
def maskWidth (ty : Ty) (n : Int) : Int :=
  match unsignedBitWidth ty with
  | some w => Int.ofNat (BitVec.ofInt w n).toNat
  | none   => n

/-- Bitwise op on the two's-complement bit patterns at `ty`'s width, matching
    the compiled LLVM `and`/`or`/`xor`. Goes through the `w`-bit pattern (NOT
    `Int.toNat`, which would clamp a negative operand to 0). -/
def bitwiseAtWidth (ty : Ty) (opNat : Nat → Nat → Nat) (a b : Int) : Int :=
  match intBitWidth ty with
  | some (w, signed) =>
    let an := (BitVec.ofInt w a).toNat
    let bn := (BitVec.ofInt w b).toNat
    let rv := BitVec.ofNat w (opNat an bn)
    if signed then rv.toInt else Int.ofNat rv.toNat
  | none => Int.ofNat (opNat a.toNat b.toNat)

/-- Truncated division (toward zero), matching LLVM `sdiv`/`udiv` — NOT Lean's
    floored `/` (which disagrees for negative operands: `-17 / 5 = -3`, not
    `-4`). -/
def tdiv (a b : Int) : Int := Int.tdiv a b

/-- Truncated remainder (sign of dividend), matching LLVM `srem`/`urem` — NOT
    Lean's floored `%`. -/
def tmod (a b : Int) : Int := Int.tmod a b

/-- The outcome of evaluating / folding an integer binary op on two integer
    operands. The three cases are mutually exclusive and must all be handled:

    * `value n ty` — the operation is defined and produces `n : ty`;
    * `trap msg`   — the operation is defined to TRAP at this input (overflow,
                     divide-by-zero, signed `MIN / -1`, shift out of range).
                     A constant fold MUST NOT turn this into a value, and DCE
                     MUST NOT delete the operation;
    * `notApplicable` — this op/type combination is not an integer arithmetic
                     op handled here (e.g. comparisons, bool ops, a non-integer
                     operand). The caller falls back to its own handling.

    Making trap a first-class result (not "returns none, caller guesses why")
    is the core of Phase 6.5 #1: value / trap / not-foldable can never be
    silently confused. -/
inductive ArithResult where
  | value (n : Int) (ty : Ty)
  | trap (msg : String)
  | notApplicable
  deriving Repr, BEq, Inhabited

/-- Evaluate a binary op on two integer operands under the result type `ty`
    (the LHS/value type; for shifts, the shifted value's width). This is the
    single arithmetic evaluator: the interpreter calls it directly, and the
    constant folder calls it (via `foldIntBinOp`) to decide foldability.

    Reproduces exactly the semantics `Interp.evalBinOp` had:
    checked `+ - * / %` (trap on overflow / zero / signed-MIN-over-neg-one),
    `wrapping_*` (two's-complement wrap), `saturating_*` (clamp), `/ %` using
    truncated division, and the shift/bitwise family at the type's width. -/
def evalIntBinOp (op : BinOp) (a : Int) (ty : Ty) (b : Int) : ArithResult :=
  match op with
  | .add => match checkedToType ty (a + b) with
            | some v => .value v ty | none => .trap "arithmetic overflow (checked +)"
  | .sub => match checkedToType ty (a - b) with
            | some v => .value v ty | none => .trap "arithmetic overflow (checked -)"
  | .mul => match checkedToType ty (a * b) with
            | some v => .value v ty | none => .trap "arithmetic overflow (checked *)"
  | .wrappingAdd => .value (wrapToType ty (a + b)) ty
  | .wrappingSub => .value (wrapToType ty (a - b)) ty
  | .wrappingMul => .value (wrapToType ty (a * b)) ty
  | .saturatingAdd => .value (saturateToType ty (a + b)) ty
  | .saturatingSub => .value (saturateToType ty (a - b)) ty
  | .saturatingMul => .value (saturateToType ty (a * b)) ty
  | .div =>
    if b == 0 then .trap "division by zero"
    else match checkedToType ty (tdiv a b) with
         | some v => .value v ty | none => .trap "arithmetic overflow (checked /)"
  | .mod =>
    if b == 0 then .trap "modulo by zero"
    -- signed MIN % -1 is UB (implied quotient overflows); trap via the same
    -- quotient-overflow condition as `/`.
    else match checkedToType ty (tdiv a b) with
         | some _ => .value (maskWidth ty (tmod a b)) ty | none => .trap "arithmetic overflow (checked %)"
  | _ => .notApplicable

/-- Constant-fold outcome for the SSA cleanup pass. Returns:
    * `some (some n)` — fold to the constant `n` (provably non-trapping);
    * `some none`     — the op is integer arithmetic that would TRAP at these
                        constants, so it must NOT be folded and must survive DCE;
    * `none`          — not a foldable integer arithmetic op here.

    Callers must treat `some none` as "keep the op live" — never as "no fold, do
    whatever". This is the foldability tri-state the DCE/fold passes rely on. -/
def foldIntBinOp (op : BinOp) (a : Int) (ty : Ty) (b : Int) : Option (Option Int) :=
  match evalIntBinOp op a ty b with
  | .value n _ => some (some n)
  | .trap _    => some none
  | .notApplicable => none

/-- Is a shift amount `b` in range for shifting a value of type `ty`? A shift by
    ≥ the bit width (or negative) traps. -/
def shiftAmountInRange (ty : Ty) (b : Int) : Bool :=
  match intBitWidth ty with
  | some (w, _) => 0 ≤ b && b < Int.ofNat w
  | none => false

end IntArith
end Concrete
