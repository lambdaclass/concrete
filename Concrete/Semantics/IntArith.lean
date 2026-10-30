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
`docs/spec/arithmetic.md`).

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

/-- Wrap a result into `ty`'s width respecting SIGNEDNESS (two's-complement),
    matching the compiled truncation. Unlike `maskWidth` (which wraps unsigned
    only and leaves signed values as mathematical `Int`), this sign-extends a
    signed overflow: `wrapToWidth i8 200 = -56`, matching LLVM `shl i8 100, 1`.
    Used for the left-shift result, whose signed value can exceed the width. -/
def wrapToWidth (ty : Ty) (n : Int) : Int :=
  match intBitWidth ty with
  | some (w, signed) =>
    let bv := BitVec.ofInt w n
    if signed then bv.toInt else Int.ofNat bv.toNat
  | none => n

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

/-- Did this evaluation trap? Used by the trap-condition locks below, which check the
    `TrapCondition` enumeration against this evaluator on concrete inputs. -/
def ArithResult.isTrap : ArithResult → Bool
  | .trap _ => true
  | _       => false

/-- Evaluate a binary op on two integer operands under the result type `ty`
    (the LHS/value type). This is the single arithmetic evaluator: the
    interpreter calls it directly, and the constant folder calls it (via
    `foldIntBinOp`) to decide foldability.

    Covers exactly the value-producing arithmetic family: checked `+ - * / %`
    (trap on overflow / zero / signed-MIN-over-neg-one), `wrapping_*`
    (two's-complement wrap), and `saturating_*` (clamp); `/ %` use truncated
    division. Every other op returns `.notApplicable`.

    Shifts and bitwise ops are intentionally NOT handled here — they are simple
    enough that callers apply the reference's small helpers directly
    (`shiftAmountInRange` for the trap check, `maskWidth` for the shift result,
    `bitwiseAtWidth` for `& | ^`). Keeping them out of the tri-state evaluator
    avoids threading a shift-result formula through `ArithResult` for no gain. -/
-- spec: ARITH-8 (docs/spec/arithmetic.md; this declaration enforces the rule)
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

/-- Evaluate a UNARY op on an integer operand under type `ty`. The unary
    counterpart of `evalIntBinOp`, and for the same reason: "does `-x` trap?"
    was being re-derived independently by the interpreter, the constant folder,
    EmitSSA, and dead-code elimination. Three of the four got it right; DCE had
    no unary case at all and deleted discarded negations, silently removing the
    documented MIN trap (bug 053). The folder's own comment next door said
    "leave the op live so the checked negation helper traps at runtime" — the
    knowledge existed, adjacent and unconsulted.

    * `neg` is CHECKED: `-x` is `0 - x`, which fails whenever the result is not
      representable — signed MIN, and any nonzero value at an unsigned type.
    * `bitnot` never traps: `~n` at width w is `2^w - 1 - n`, always in range.
    * `not_` is boolean, not integer arithmetic — `notApplicable`. -/
def evalIntUnaryOp (op : UnaryOp) (n : Int) (ty : Ty) : ArithResult :=
  match op with
  | .neg =>
    match checkedToType ty (-n) with
    | some v => .value v ty
    | none   => .trap "arithmetic overflow (checked negation)"
  | .bitnot => .value (maskWidth ty (-(n + 1))) ty
  | .not_ => .notApplicable

/-- THE trap inventory for unary ops: can `op` at type `ty` trap for SOME
    operand? This is the value-independent question dead-code elimination needs
    — it must decide whether an operation may be deleted without knowing what
    flows into it.

    Deliberately conservative: it answers about the type, not the value. A caller
    holding a constant operand can prove a specific instance safe by evaluating
    `evalIntUnaryOp` and checking for `.value`, exactly as the binary path uses
    `constPairNonTrapping`. -/
def unaryOpCanTrap (op : UnaryOp) (ty : Ty) : Bool :=
  match op with
  -- Integer negation only. Float negation is total, and `~`/`!` cannot fail.
  | .neg => isIntTy ty
  | .bitnot | .not_ => false

/-- **Evaluate a checked SHIFT — the shift arm of the reference (R-0460).**

    `evalIntBinOp` deliberately omits shifts, and the omission had a cost: there was no
    single definition of when a shift traps, so the interpreter carried the rule inline,
    `SSACleanup` consulted `shiftAmountInRange` directly, and obligation generation had no
    shift family at all until R-0464. Three consumers, no shared definition — the exact
    shape H24 was.

    This is that definition, and it is what the sufficiency theorem below is proved against.
    Transcribed from the interpreter (the reference semantics, Principle 1) rather than
    invented, including two details that are easy to get wrong:

    * the trap is guarded by `isIntTy ty`, so a non-integer type does NOT trap even though
      `shiftAmountInRange` is `false` for it;
    * `shl` wraps with `wrapToWidth`, not `maskWidth` — the shifted value can exceed the
      SIGNED width (`100 << 1` at `i8` is 200), and `maskWidth` only wraps unsigned, which
      would leave a signed overflow untruncated and diverge from the compiled `shl`
      (which truncates 200 to -56). -/
-- spec: ARITH-8 (docs/spec/arithmetic.md; this declaration enforces the rule)
def evalIntShift (op : BinOp) (a : Int) (ty : Ty) (b : Int) : ArithResult :=
  match op with
  | .shl =>
    if isIntTy ty && !shiftAmountInRange ty b then .trap "shift amount out of range"
    else .value (wrapToWidth ty (a * (2 ^ b.toNat))) ty
  | .shr =>
    if isIntTy ty && !shiftAmountInRange ty b then .trap "shift amount out of range"
    else .value (maskWidth ty (a / (2 ^ b.toNat))) ty
  | _ => .notApplicable

/-! ### Trap conditions as first-class data (R-0464 / H24)

`evalIntBinOp` above is the authority on *whether* a checked op traps at given values.
Obligation generation cannot call it — an obligation is a proposition about symbolic
operands, not a computation on two `Int`s — so it stated its own trap conditions, and stated
them weaker: `divObligations` emitted only `divisor ≠ 0`, missing the signed `MIN / -1`
quotient overflow, and no family collected shifts at all. `collectArithE` matches only
`.add | .sub | .mul`, so nothing else covered them either. Both faults are reproduced in
`examples/trap_semantics_gap/`.

The fix is not to duplicate the rules more carefully. It is to name, in ONE place, which
conditions a checked op owes — then hold that enumeration to `evalIntBinOp` by kernel-checked
example, and hold obligation generation to the enumeration by totality. A new trap rule then
cannot be added to the evaluator without breaking a lock here, and cannot be listed here
without a family claiming it.
-/

/-- One condition a checked operation must discharge to be trap-free. Data, not a
    proposition, so both the evaluator lock below and obligation generation can pattern
    match on it exhaustively — the same discipline as `HypOrigin` in Register C, and for the
    same reason: a catch-all would give a newly added condition zero obligations, silently. -/
inductive TrapCondition where
  /-- `b ≠ 0` for `/` and `%`. -/
  | divisorNonZero
  /-- The quotient fits the type: excludes signed `MIN / -1` (and `MIN % -1`, whose implied
      quotient overflows identically). This is the one `divObligations` omitted. -/
  | quotientInRange
  /-- The result fits the type: `+ - *` overflow. -/
  | resultInRange
  /-- `0 ≤ b < bitWidth` for `<<` and `>>`. No family generated this at all. -/
  | shiftAmountInRange
  deriving DecidableEq, Repr, Inhabited

/-- Every trap condition, for the totality checks that keep families honest. -/
def allTrapConditions : List TrapCondition :=
  [.divisorNonZero, .quotientInRange, .resultInRange, .shiftAmountInRange]

/-- **The complete set of trap conditions a CHECKED binary op owes.**

    Read off `evalIntBinOp`'s trap branches, and locked to them by the examples below rather
    than by trust. `wrapping_*` and `saturating_*` are absent because they are defined not to
    trap; comparison and boolean ops carry no integer trap.

    `bitand`/`bitor`/`bitxor` are total on in-range operands and appear with `[]` for the same
    reason — stated explicitly rather than falling into a catch-all, so that adding a trapping
    bitwise op forces a decision here. -/
def trapConditions : BinOp → List TrapCondition
  | .div | .mod => [.divisorNonZero, .quotientInRange]
  | .add | .sub | .mul => [.resultInRange]
  | .shl | .shr => [.shiftAmountInRange]
  | .wrappingAdd | .wrappingSub | .wrappingMul => []
  | .saturatingAdd | .saturatingSub | .saturatingMul => []
  | .bitand | .bitor | .bitxor => []
  | .eq | .neq | .lt | .gt | .leq | .geq | .and_ | .or_ => []

/-- Does this specific condition hold at these values, for this type? The decidable
    counterpart of what an obligation says symbolically — the bridge that lets the examples
    below check the enumeration against `evalIntBinOp` on concrete inputs. -/
def trapConditionHolds (c : TrapCondition) (op : BinOp) (ty : Ty) (a b : Int) : Bool :=
  match c with
  | .divisorNonZero => b != 0
  | .quotientInRange => b == 0 || (checkedToType ty (tdiv a b)).isSome
  -- Takes the OP, because "the result fits" is not one predicate: `a + b`, `a - b` and
  -- `a * b` overflow at different inputs. R-0464 shipped this as `true` with the comment
  -- "op-specific; the op's own range check covers it" — a placeholder that made this arm
  -- VACUOUS, so any claim of the form "all listed conditions hold, therefore no trap" was
  -- false for `+ - *` while looking discharged. Found by trying to prove exactly that
  -- claim (R-0460), which is the argument for writing the theorem rather than the comment.
  | .resultInRange =>
      match op with
      | .add => (checkedToType ty (a + b)).isSome
      | .sub => (checkedToType ty (a - b)).isSome
      | .mul => (checkedToType ty (a * b)).isSome
      | _    => true
  | .shiftAmountInRange => shiftAmountInRange ty b

/-! #### R-0460: the enumeration is SUFFICIENT, as a theorem

The examples below check the enumeration against the evaluator at chosen inputs. This
proves the general statement they sample: **if every condition `trapConditions` lists for an
op holds, that op does not trap** — at any inputs, not just the ones someone thought to
write down.

This is the semantics half of `VC_BRIDGE_REGISTER.md`'s `div_obligation_sufficient` row,
and it is worth being exact about which half. It says the CONDITIONS are strong enough. It
does not say the emitted obligation *denotes* the conditions — that is the lowering, which
is validated by agreement checks (against a reference evaluator, on sampled assignments)
and not proven. H19 is that gap, and no theorem here closes it.

It does NOT discharge the shift row, and the reason is worth stating so nobody reads the
`∀ op` and assumes otherwise: `evalIntBinOp` does not model shifts at all — it returns
`notApplicable`, so `isTrap` is trivially false and the shift case of this theorem is
VACUOUS. The reference deliberately keeps shifts in `shiftAmountInRange`/`maskWidth`
instead of threading a shift-result formula through `ArithResult`. A shift sufficiency
theorem has to be stated against those helpers; a theorem that holds because its subject is
absent proves nothing.

Writing the theorem is what found the defect it now forbids: `resultInRange` had been
shipped as the constant `true`, so this statement was false for `+ - *` while the row it
serves looked one step from discharged. A sampled example would not have caught it —
`trapConditionHolds .resultInRange` returning `true` agrees with every non-trapping input,
and nobody had written the trapping one. -/
/-- The shape every checked arithmetic arm has: a range check whose `some` branch yields a
    value and whose `none` branch traps. If the check succeeds, the arm does not trap. -/
private theorem isTrap_checked {ty : Ty} {x : Int} {msg : String}
    (hs : (checkedToType ty x).isSome = true) :
    (match checkedToType ty x with
     | some v => ArithResult.value v ty
     | none   => ArithResult.trap msg).isTrap = false := by
  cases hc : checkedToType ty x with
  | none   => simp [hc] at hs
  | some v => rfl

theorem trapConditions_sufficient (op : BinOp) (ty : Ty) (a b : Int)
    (h : ∀ c ∈ trapConditions op, trapConditionHolds c op ty a b = true) :
    (evalIntBinOp op a ty b).isTrap = false := by
  cases op
  -- `+ - *`: the single `resultInRange` condition IS the range check the evaluator runs.
  case add =>
    exact isTrap_checked (by
      simpa [trapConditionHolds] using h .resultInRange (by simp [trapConditions]))
  case sub =>
    exact isTrap_checked (by
      simpa [trapConditionHolds] using h .resultInRange (by simp [trapConditions]))
  case mul =>
    exact isTrap_checked (by
      simpa [trapConditionHolds] using h .resultInRange (by simp [trapConditions]))
  -- `/` and `%`: BOTH conditions are needed, and this is exactly where shipping only the
  -- first (H24) let a division report proved and then abort on signed MIN / -1.
  case div =>
    have h0 := h .divisorNonZero (by simp [trapConditions])
    have hq := h .quotientInRange (by simp [trapConditions])
    simp [trapConditionHolds] at h0 hq
    simp only [evalIntBinOp, ite_eq_right (by simpa using h0 : ¬ (b == 0) = true)]
    exact isTrap_checked (by simpa [h0] using hq)
  case mod =>
    have h0 := h .divisorNonZero (by simp [trapConditions])
    have hq := h .quotientInRange (by simp [trapConditions])
    simp [trapConditionHolds] at h0 hq
    simp only [evalIntBinOp, ite_eq_right (by simpa using h0 : ¬ (b == 0) = true)]
    cases hc : checkedToType ty (tdiv a b) with
    | none   => simp_all
    | some v => rfl
  -- Everything else is total, saturating, wrapping, or not handled by this evaluator
  -- (shifts and bitwise ops use the small helpers directly — see `evalIntBinOp`'s note).
  all_goals rfl

/-- **R-0460: the shift-amount condition is SUFFICIENT.**

    `VC_BRIDGE_REGISTER.md`'s `shift_obligation_sufficient` row. Stated against
    `evalIntShift` — the definition the interpreter now consumes — and NOT against
    `evalIntBinOp`, which does not model shifts and would make this true vacuously. That
    distinction is the whole reason this row stayed open when `trapConditions_sufficient`
    landed: a theorem that holds because its subject is absent proves nothing.

    Hypothesis is `shiftAmountInRange`, which is exactly what `shiftObligations` emits
    (`0 ≤ amount < bitWidth`), taking the width from the SHIFTED operand's type. -/
theorem shift_amount_sufficient (op : BinOp) (ty : Ty) (a b : Int)
    (hop : op = .shl ∨ op = .shr)
    (h : shiftAmountInRange ty b = true) :
    (evalIntShift op a ty b).isTrap = false := by
  rcases hop with rfl | rfl <;> simp [evalIntShift, h, ArithResult.isTrap]

/-- And it is NECESSARY as well as sufficient, for integer types: an out-of-range amount
    does trap. Without this the condition could be strengthened to `False` and the
    sufficiency theorem would still hold — the direction that makes a rule useless rather
    than unsound, and the one a sufficiency proof alone cannot see. -/
theorem shift_amount_necessary (op : BinOp) (ty : Ty) (a b : Int)
    (hop : op = .shl ∨ op = .shr)
    (hty : isIntTy ty = true)
    (h : shiftAmountInRange ty b = false) :
    (evalIntShift op a ty b).isTrap = true := by
  rcases hop with rfl | rfl <;> simp [evalIntShift, h, hty, ArithResult.isTrap]

/-- The value a checked arithmetic op computes, BEFORE the range check — which is exactly
    the expression `overflowObligations` renders and states bounds about. `none` for ops
    whose obligation is not a range check. -/
def rawResult (op : BinOp) (a b : Int) : Option Int :=
  match op with
  | .add => some (a + b)
  | .sub => some (a - b)
  | .mul => some (a * b)
  | _    => none

/-- In-range implies the checked conversion succeeds. The bridge between how the OBLIGATION
    states the property (explicit bounds from `intRange`) and how the EVALUATOR decides it
    (`checkedToType` returning `some`). Two spellings of one fact, and the row below needs
    them related rather than assumed related. -/
theorem checkedToType_isSome_of_inRange {ty : Ty} {n lo hi : Int}
    (hr : intRange ty = some (lo, hi)) (hlo : lo ≤ n) (hhi : n ≤ hi) :
    (checkedToType ty n).isSome = true := by
  simp [checkedToType, hr, Int.not_lt.mpr hlo, Int.not_lt.mpr hhi]

/-- **R-0460: `overflowObligations`'s condition is SUFFICIENT.**

    `VC_BRIDGE_REGISTER.md`'s `overflow_obligation_sufficient` row. The obligation emitted for
    a checked `+ - *` is `lo ≤ e ∧ e ≤ hi` with `(lo, hi)` from the operand type's range; this
    proves that discharging it means the operation does not trap.

    Stated over `rawResult` so the three ops share one statement, and over `intRange`'s own
    bounds rather than literals so it holds at every width. Note the obligation generator
    uses `Report.intRange`, which deliberately returns `none` for `Int`/`Uint` (their overflow
    is profile-dependent) — that narrowing means the generator emits FEWER obligations than
    this theorem covers, which is the safe direction: every obligation it does emit is one
    this theorem discharges. -/
theorem overflow_obligation_sufficient {op : BinOp} {ty : Ty} {a b lo hi v : Int}
    (hraw : rawResult op a b = some v)
    (hr : intRange ty = some (lo, hi)) (hlo : lo ≤ v) (hhi : v ≤ hi) :
    (evalIntBinOp op a ty b).isTrap = false := by
  cases op <;> simp [rawResult] at hraw <;> subst hraw <;>
    exact isTrap_checked (checkedToType_isSome_of_inRange hr hlo hhi)

/-- Out of range implies the checked conversion fails — the converse of
    `checkedToType_isSome_of_inRange`. -/
theorem checkedToType_eq_none_of_outOfRange {ty : Ty} {n lo hi : Int}
    (hr : intRange ty = some (lo, hi)) (h : n < lo ∨ hi < n) :
    checkedToType ty n = none := by
  rcases h with h | h
  · simp [checkedToType, hr, h]
  · simp [checkedToType, hr, h]

/-- **The overflow condition is NECESSARY as well as sufficient.**

    Sufficiency alone cannot tell a useful rule from a vacuous one: strengthen the emitted
    bounds to something unsatisfiable and `overflow_obligation_sufficient` still holds. This
    says the bounds are TIGHT — outside them the operation really does trap — so the rule is
    not over-restrictive either. Same reason `shift_amount_necessary` exists. -/
theorem overflow_obligation_necessary {op : BinOp} {ty : Ty} {a b lo hi v : Int}
    (hraw : rawResult op a b = some v)
    (hr : intRange ty = some (lo, hi)) (h : v < lo ∨ hi < v) :
    (evalIntBinOp op a ty b).isTrap = true := by
  have hn := checkedToType_eq_none_of_outOfRange hr h
  cases op <;> simp [rawResult] at hraw <;>
    simp [evalIntBinOp, hraw, hn, ArithResult.isTrap]

/-- **The div/mod conditions are NECESSARY as well as sufficient.**

    Completes the tightness story for the four half-discharged rows. Two ways a division
    traps, one per condition, so violating EITHER is enough — which is the same fact H24 was
    the negative instance of: shipping only `divisorNonZero` left a real trap unstated. -/
theorem div_obligation_necessary {op : BinOp} {ty : Ty} {a b : Int}
    (hop : op = .div ∨ op = .mod)
    (h : b = 0 ∨ checkedToType ty (tdiv a b) = none) :
    (evalIntBinOp op a ty b).isTrap = true := by
  rcases hop with rfl | rfl
  · by_cases hb : b = 0
    · subst hb; rfl
    · rcases h with hz | hc
      · exact absurd hz hb
      · simp [evalIntBinOp, hb, hc, ArithResult.isTrap]
  · by_cases hb : b = 0
    · subst hb; rfl
    · rcases h with hz | hc
      · exact absurd hz hb
      · simp [evalIntBinOp, hb, hc, ArithResult.isTrap]

/-! #### The lock: the enumeration agrees with the evaluator

If `evalIntBinOp` traps on a `/` or `%` at these inputs, some condition in
`trapConditions` must be false there. These are the exact inputs the gap was reproduced at,
so a regression is a build failure. -/

-- i32 MIN / -1 traps, and `divisorNonZero` alone does NOT explain it — this is the pair
-- whose second element `divObligations` was missing. If `quotientInRange` were dropped from
-- `trapConditions .div`, the first line would still hold and the second would fail.
example : (evalIntBinOp .div (-2147483648) (.i32) (-1)).isTrap = true := rfl
example : trapConditionHolds .divisorNonZero .div (.i32) (-2147483648) (-1) = true := rfl
example : trapConditionHolds .quotientInRange .div (.i32) (-2147483648) (-1) = false := rfl
-- the same for `%`, whose implied quotient overflows identically
example : (evalIntBinOp .mod (-2147483648) (.i32) (-1)).isTrap = true := rfl
example : trapConditionHolds .quotientInRange .div (.i32) (-2147483648) (-1) = false := rfl
-- divide by zero is still caught by the condition that was already there
example : (evalIntBinOp .div 1 (.i32) 0).isTrap = true := rfl
example : trapConditionHolds .divisorNonZero .div (.i32) 1 0 = false := rfl
-- an ordinary division trips neither condition
example : (evalIntBinOp .div 7 (.i32) 2).isTrap = false := rfl
example : trapConditionHolds .divisorNonZero .div (.i32) 7 2 = true := rfl
example : trapConditionHolds .quotientInRange .div (.i32) 7 2 = true := rfl
-- MIN / -1 is type-relative: at 64-bit (`.int`) the same literal operands are fine, and the
-- condition fires only at THAT type's minimum. A rule stated without the type — which is
-- what a hand-written obligation is tempted to do — is wrong at one width or the other.
example : trapConditionHolds .quotientInRange .div (.int) (-2147483648) (-1) = true := rfl
example : trapConditionHolds .quotientInRange .div (.int) (-9223372036854775808) (-1) = false := rfl
-- shift amounts: in range, at the boundary, and over-width (the fixture's `1 << 40`)
example : trapConditionHolds .shiftAmountInRange .shl (.i32) 1 31 = true := rfl
example : trapConditionHolds .shiftAmountInRange .shl (.i32) 1 32 = false := rfl
example : trapConditionHolds .shiftAmountInRange .shl (.i32) 1 40 = false := rfl
example : trapConditionHolds .shiftAmountInRange .shl (.i32) 1 (-1) = false := rfl
-- the checked ops all owe something; the non-trapping families owe nothing
example : trapConditions .div = [.divisorNonZero, .quotientInRange] := rfl
example : trapConditions .wrappingAdd = [] := rfl
example : (trapConditions .shl).length = 1 := rfl

end IntArith
end Concrete
