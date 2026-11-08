import ConcreteSpec.Spec.Scalar

/-!
# Constants: the closed, kernel-checked initializer subset

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). Part of `ConcreteSpec.Spec`;
see `ConcreteSpec/README.md`. Foundation contract C2, with owner decisions 2026-11-08, items 18,
19 and 26.

A module constant (`const N: T = e;`) is computed once at compile time, and its value must be of a
`Copy` type (item 26). Its initializer is in a *closed grammar*: integer literals in range, `true`,
`false`, `()`, references to earlier Core constants, wrapping and saturating arithmetic, bitwise
operations, integer comparisons, boolean operations, lossless conversion, and the fallible scalar
operations `plain` / `plainNeg` and `tryConvert` (item 18, revising TYPE-22). There are no calls,
loops, effects, state, authority or target-profile inputs. Even the scalar operations depend only
on the fixed width in their `IntTy`, not on a target profile.

Evaluation is partial and fuel-bounded. A fallible operation is evaluated with the definitions in
`Spec/Scalar.lean`; its failure is a compile-time refusal carrying the operation and the exact
sub-expression, never a trap. Checked arithmetic forms are not constructors here: their
`Result<T, ArithmeticError>` values have no type in `CTy`, and widening that constant universe is
a separate owner decision. Every operation terminates; fuel bounds the evaluator as item 18
requires, rather than making an operation partial.

**Static admission and kernel checking.** `typeConsts` checks the closed grammar, declared-type
agreement and freshness; because every scalar constant type in `CTy` is `Copy` (`ofC_copy`),
declared-type agreement entails `Copy` without a redundant runtime `isCopy` guard (`admitted_copy`,
`typeConsts_refuses_nonCopy`).
Independently, `evalConsts` evaluates every declaration, used or unused, and reports the first
failing constant, operation failure, invalid expression or fuel exhaustion. `ConstsAdmitted` is
their conjunction and is decidable: concrete admission examples use `decide`, hence Lean's kernel
reduction, never `native_decide` (item 18). With sufficient fuel, well-typed initializers either
compute a valid value of their static type or report a located failure of a fallible sub-expression
(`eval_wellTyped_progress`, `typed_evalConsts_refusal`); the old fallible-free fragment remains
total (`eval_total`); admitted declarations have valid values of their declared types
(`evalConsts_sound`). `eval_agrees` additionally proves exact agreement with the independent
primitive-grounded `ConstEval` relation, so neither swallowed nor fabricated arithmetic failures
are permitted. `semantic_failure_refuses` propagates a semantic failure after any successful
prefix to declaration refusal and non-admission, including unused initializers.

**Source order and elaboration** (owner decision 2026-11-08, item 19). A source initializer may
name a later declaration. `dependsOn` extracts the actual reference relation, including references
nested in operands. `DependencyOrder` is README obligation O1: elaboration supplies a permutation
of the source list in which every referenced declaration occurs earlier (`sortConsts`).
`ElaboratedConsts` requires either such an order or a cycle refusal whose chain is
`c₀ → c₁ → … → c₀`, with every edge an actual reference (`cycle_chain_real`). In `Proofs/Const.lean`,
`cycle_excludes_dependencyOrder` proves that a cycle precludes every dependency order,
`DependencySortingComplete` proves constructively via `sortConsts` that every resolved acyclic
source list has a dependency order, and `ConstOrderIndependent` proves that any two dependency
orders of a source list agree on typing, evaluation, admission and the values of all constants. Core
continues to receive only the ordered form.

The constant types are an interface (`ConstTypes`): the language's type universe, a `Copy`
predicate and an injective embedding of the scalar types (`ofC`), which are all `Copy`
(`ofC_copy`), while `Ty` may also contain non-`Copy` types outside `ofC`. Widening `CTy`, `CVal`
and `CExpr` to other values remains an owner decision.
-/

namespace ConcreteSpec.Core

/-- An initializer of the closed subset. `plain`, `plainNeg` and `tryConvert` are the only
fallible operations; their meanings come from `Spec/Scalar.lean`. -/
inductive CExpr where
  | int (t : IntTy) (n : Int)
  | bool (b : Bool)
  | unit
  | ref (c : ConstId)
  | wrapping (op : WrapOp) (a b : CExpr)
  | saturating (op : WrapOp) (a b : CExpr)
  | plain (op : ArithOp) (a b : CExpr)
  | plainNeg (a : CExpr)
  | bit (op : BitOp) (a b : CExpr)
  | bitNot (a : CExpr)
  | cmp (op : CmpOp) (a b : CExpr)
  | not (a : CExpr)
  | and (a b : CExpr)
  | or (a b : CExpr)
  /-- The lossless conversion to `t`. -/
  | convert (t : IntTy) (a : CExpr)
  /-- The checked conversion to `t`, with `OutOfRange` a compile-time refusal here. -/
  | tryConvert (t : IntTy) (a : CExpr)
  deriving DecidableEq, Repr

namespace CExpr

/-- All constant identities referenced by an initializer, including nested operands. -/
def refs : CExpr → List ConstId
  | .int _ _ | .bool _ | .unit => []
  | .ref c => [c]
  | .wrapping _ a b | .saturating _ a b | .plain _ a b | .bit _ a b | .cmp _ a b
  | .and a b | .or a b => a.refs ++ b.refs
  | .plainNeg a | .bitNot a | .not a | .convert _ a | .tryConvert _ a => a.refs

/-- The old fragment, in which no operation can fail. -/
def fallibleFree : CExpr → Bool
  | .int _ _ | .bool _ | .unit | .ref _ => true
  | .wrapping _ a b | .saturating _ a b | .bit _ a b | .cmp _ a b | .and a b | .or a b =>
    a.fallibleFree && b.fallibleFree
  | .bitNot a | .not a | .convert _ a => a.fallibleFree
  | .plain _ _ _ | .plainNeg _ | .tryConvert _ _ => false

/-- Sufficient fuel for every path through an initializer; operands share the remaining depth
budget, so this is a depth bound rather than an operation-count bound. -/
def height : CExpr → Nat
  | .int _ _ | .bool _ | .unit | .ref _ => 1
  | .wrapping _ a b | .saturating _ a b | .plain _ a b | .bit _ a b | .cmp _ a b
  | .and a b | .or a b => max a.height b.height + 1
  | .plainNeg a | .bitNot a | .not a | .convert _ a | .tryConvert _ a => a.height + 1

end CExpr

/-- The operation named by a compile-time initializer failure. -/
inductive ConstOperation where
  | plain (op : ArithOp)
  | plainNeg
  | tryConvert (t : IntTy)
  deriving DecidableEq, Repr

/-- The scalar reason for an operation failure. Plain arithmetic carries its `TrapKind` only as a
diagnostic category; `CEvalResult.failure` is a refusal, not execution of a trap. -/
inductive ConstFailureError where
  | arithmetic (k : TrapKind)
  | outOfRange
  deriving DecidableEq, Repr

/-- A located scalar failure: this operation failed at exactly this initializer sub-expression. -/
structure ConstFailure where
  operation : ConstOperation
  expression : CExpr
  error : ConstFailureError
  deriving DecidableEq, Repr

/-- A located failure belongs to `e`: its expression is a fallible sub-expression of `e` whose
constructor matches the reported `operation` and `error` category. This is location evidence
only; primitive failure correctness is specified independently by `ConstEval`. -/
def ConstFailure.InExpr (f : ConstFailure) : CExpr → Prop
  | .int _ _ | .bool _ | .unit | .ref _ => False
  | .wrapping _ a b | .saturating _ a b | .bit _ a b | .cmp _ a b
  | .and a b | .or a b => f.InExpr a ∨ f.InExpr b
  | .bitNot a | .not a | .convert _ a => f.InExpr a
  | .plain op a b =>
    (∃ k, f = ⟨.plain op, .plain op a b, .arithmetic k⟩) ∨ f.InExpr a ∨ f.InExpr b
  | .plainNeg a =>
    (∃ k, f = ⟨.plainNeg, .plainNeg a, .arithmetic k⟩) ∨ f.InExpr a
  | .tryConvert t a =>
    f = ⟨.tryConvert t, .tryConvert t a, .outOfRange⟩ ∨ f.InExpr a

/-- The result of fuel-bounded initializer evaluation. `invalid` is static refusal (bad typing, an
out-of-range literal or an unknown reference), distinct from a successfully typed operation that
fails and from an insufficient evaluation budget. -/
inductive CEvalResult where
  | value (v : CVal)
  | failure (f : ConstFailure)
  | outOfFuel
  | invalid (expression : CExpr)
  deriving DecidableEq, Repr

namespace CEvalResult

/-- Propagate the first non-value result. -/
def bind : CEvalResult → (CVal → CEvalResult) → CEvalResult
  | .value v, f => f v
  | .failure f, _ => .failure f
  | .outOfFuel, _ => .outOfFuel
  | .invalid e, _ => .invalid e

end CEvalResult

/-- Two integer operands of one type, or `none`. -/
def sameInt : Option CVal → Option CVal → Option (IntTy × Int × Int)
  | some (.int t x), some (.int t' y) => if t = t' then some (t, x, y) else none
  | _, _ => none

/-- Two boolean operands, or `none`. -/
def twoBools : Option CVal → Option CVal → Option (Bool × Bool)
  | some (.bool x), some (.bool y) => some (x, y)
  | _, _ => none

/-- Evaluate an initializer against earlier constants. Zero fuel is always exhausted; every other
node consumes one unit. Binary operands are evaluated left to right, and the first non-value is
propagated. Plain failures become located refusals, never traps. -/
def CExpr.eval (env : AList ConstId CVal) : Nat → CExpr → CEvalResult
  | 0, _ => .outOfFuel
  | _ + 1, .int t n => if t.InRange n then .value (.int t n) else .invalid (.int t n)
  | _ + 1, .bool b => .value (.bool b)
  | _ + 1, .unit => .value .unit
  | _ + 1, .ref c => match AList.lookup env c with
    | some v => .value v
    | none => .invalid (.ref c)
  | fuel + 1, .wrapping op a b => (a.eval env fuel).bind fun va => (b.eval env fuel).bind fun vb =>
    match sameInt (some va) (some vb) with
    | some (t, x, y) => .value (.int t (Core.wrapping t op x y))
    | none => .invalid (.wrapping op a b)
  | fuel + 1, .saturating op a b => (a.eval env fuel).bind fun va => (b.eval env fuel).bind fun vb =>
    match sameInt (some va) (some vb) with
    | some (t, x, y) => .value (.int t (Core.saturating t op x y))
    | none => .invalid (.saturating op a b)
  | fuel + 1, .plain op a b => (a.eval env fuel).bind fun va => (b.eval env fuel).bind fun vb =>
    match sameInt (some va) (some vb) with
    | some (t, x, y) => match Core.plain t op x y with
      | .ok n => .value (.int t n)
      | .trap k => .failure ⟨.plain op, .plain op a b, .arithmetic k⟩
      | .stuck => .invalid (.plain op a b)
    | none => .invalid (.plain op a b)
  | fuel + 1, .plainNeg a => (a.eval env fuel).bind fun va =>
    match va with
    | .int t x => match Core.plainNeg t x with
      | .ok n => .value (.int t n)
      | .trap k => .failure ⟨.plainNeg, .plainNeg a, .arithmetic k⟩
      | .stuck => .invalid (.plainNeg a)
    | _ => .invalid (.plainNeg a)
  | fuel + 1, .bit op a b => (a.eval env fuel).bind fun va => (b.eval env fuel).bind fun vb =>
    match sameInt (some va) (some vb) with
    | some (t, x, y) => .value (.int t (Core.bitwise t op x y))
    | none => .invalid (.bit op a b)
  | fuel + 1, .bitNot a => (a.eval env fuel).bind fun va =>
    match va with
    | .int t x => .value (.int t (Core.bitNot t x))
    | _ => .invalid (.bitNot a)
  | fuel + 1, .cmp op a b => (a.eval env fuel).bind fun va => (b.eval env fuel).bind fun vb =>
    match sameInt (some va) (some vb) with
    | some (_, x, y) => .value (.bool (op.eval x y))
    | none => .invalid (.cmp op a b)
  | fuel + 1, .not a => (a.eval env fuel).bind fun va =>
    match va with
    | .bool x => .value (.bool (!x))
    | _ => .invalid (.not a)
  | fuel + 1, .and a b => (a.eval env fuel).bind fun va => (b.eval env fuel).bind fun vb =>
    match twoBools (some va) (some vb) with
    | some (x, y) => .value (.bool (x && y))
    | none => .invalid (.and a b)
  | fuel + 1, .or a b => (a.eval env fuel).bind fun va => (b.eval env fuel).bind fun vb =>
    match twoBools (some va) (some vb) with
    | some (x, y) => .value (.bool (x || y))
    | none => .invalid (.or a b)
  | fuel + 1, .convert t a => (a.eval env fuel).bind fun va =>
    match va with
    | .int s x => if Lossless s t then .value (.int t x) else .invalid (.convert t a)
    | .bool b => .value (.int t (boolToInt b))
    | .unit => .invalid (.convert t a)
  | fuel + 1, .tryConvert t a => (a.eval env fuel).bind fun va =>
    match va with
    | .int _ x => match Core.tryConvert t x with
      | .ok n => .value (.int t n)
      | .error .outOfRange => .failure ⟨.tryConvert t, .tryConvert t a, .outOfRange⟩
    | _ => .invalid (.tryConvert t a)

/-- A non-value evaluation outcome (`.failure`, `.outOfFuel`, or `.invalid`), which propagates
immediately left-to-right through enclosing operations. -/
def CEvalResult.NonValue : CEvalResult → Prop
  | .value _ => False
  | .failure _ | .outOfFuel | .invalid _ => True

/-- Immediate child of a unary initializer form. -/
inductive CExpr.UnaryChild : CExpr → CExpr → Prop where
  | plainNeg (a) : UnaryChild (.plainNeg a) a
  | bitNot (a) : UnaryChild (.bitNot a) a
  | not (a) : UnaryChild (.not a) a
  | convert (t a) : UnaryChild (.convert t a) a
  | tryConvert (t a) : UnaryChild (.tryConvert t a) a

/-- Immediate left and right children of a binary initializer form. -/
inductive CExpr.BinaryChildren : CExpr → CExpr → CExpr → Prop where
  | wrapping (op a b) : BinaryChildren (.wrapping op a b) a b
  | saturating (op a b) : BinaryChildren (.saturating op a b) a b
  | plain (op a b) : BinaryChildren (.plain op a b) a b
  | bit (op a b) : BinaryChildren (.bit op a b) a b
  | cmp (op a b) : BinaryChildren (.cmp op a b) a b
  | and (a b) : BinaryChildren (.and a b) a b
  | or (a b) : BinaryChildren (.or a b) a b

/-- Unary forms that require an integer operand value. -/
inductive CExpr.IntUnaryChild : CExpr → CExpr → Prop where
  | plainNeg (a) : IntUnaryChild (.plainNeg a) a
  | bitNot (a) : IntUnaryChild (.bitNot a) a
  | tryConvert (t a) : IntUnaryChild (.tryConvert t a) a

/-- Binary forms that require two integer operand values of the same integer type. -/
inductive CExpr.IntBinaryChildren : CExpr → CExpr → CExpr → Prop where
  | wrapping (op a b) : IntBinaryChildren (.wrapping op a b) a b
  | saturating (op a b) : IntBinaryChildren (.saturating op a b) a b
  | plain (op a b) : IntBinaryChildren (.plain op a b) a b
  | bit (op a b) : IntBinaryChildren (.bit op a b) a b
  | cmp (op a b) : IntBinaryChildren (.cmp op a b) a b

/-- Binary forms that require two boolean operand values. -/
inductive CExpr.BoolBinaryChildren : CExpr → CExpr → CExpr → Prop where
  | and (a b) : BoolBinaryChildren (.and a b) a b
  | or (a b) : BoolBinaryChildren (.or a b) a b

/-- Independent syntax-directed, fuel-indexed evaluation semantics. Each constructor specifies
an inference rule over immediate sub-expressions at the smaller budget `fuel` and grounds scalar
outcomes directly in `Core.wrapping`, `Core.saturating`, `Core.plain`, `Core.plainNeg`,
`Core.bitwise`, `Core.bitNot`, `CmpOp.eval`, `Lossless`, `boolToInt` and `Core.tryConvert`,
without calling `CExpr.eval`, `CEvalResult.bind`, `sameInt` or `twoBools`. -/
inductive ConstEval (env : AList ConstId CVal) : Nat → CExpr → CEvalResult → Prop where
  | exhausted (e) : ConstEval env 0 e .outOfFuel
  | int_ok {fuel t n} (h : t.InRange n) :
      ConstEval env (fuel + 1) (.int t n) (.value (.int t n))
  | int_outOfRange {fuel t n} (h : ¬ t.InRange n) :
      ConstEval env (fuel + 1) (.int t n) (.invalid (.int t n))
  | bool (fuel b) : ConstEval env (fuel + 1) (.bool b) (.value (.bool b))
  | unit (fuel) : ConstEval env (fuel + 1) .unit (.value .unit)
  | ref_ok {fuel c v} (h : AList.lookup env c = some v) :
      ConstEval env (fuel + 1) (.ref c) (.value v)
  | ref_missing {fuel c} (h : AList.lookup env c = none) :
      ConstEval env (fuel + 1) (.ref c) (.invalid (.ref c))
  | unary_nonValue {fuel e a r} (hchild : CExpr.UnaryChild e a)
      (ha : ConstEval env fuel a r) (hr : r.NonValue) :
      ConstEval env (fuel + 1) e r
  | binary_nonValue_left {fuel e a b r} (hchild : CExpr.BinaryChildren e a b)
      (ha : ConstEval env fuel a r) (hr : r.NonValue) :
      ConstEval env (fuel + 1) e r
  | binary_nonValue_right {fuel e a b va r} (hchild : CExpr.BinaryChildren e a b)
      (ha : ConstEval env fuel a (.value va))
      (hb : ConstEval env fuel b r) (hr : r.NonValue) :
      ConstEval env (fuel + 1) e r
  | wrapping_ok {fuel op a b t x y}
      (ha : ConstEval env fuel a (.value (.int t x)))
      (hb : ConstEval env fuel b (.value (.int t y))) :
      ConstEval env (fuel + 1) (.wrapping op a b) (.value (.int t (Core.wrapping t op x y)))
  | saturating_ok {fuel op a b t x y}
      (ha : ConstEval env fuel a (.value (.int t x)))
      (hb : ConstEval env fuel b (.value (.int t y))) :
      ConstEval env (fuel + 1) (.saturating op a b) (.value (.int t (Core.saturating t op x y)))
  | plain_ok {fuel op a b t x y n}
      (ha : ConstEval env fuel a (.value (.int t x)))
      (hb : ConstEval env fuel b (.value (.int t y)))
      (hop : Core.plain t op x y = .ok n) :
      ConstEval env (fuel + 1) (.plain op a b) (.value (.int t n))
  | plain_failure {fuel op a b t x y k}
      (ha : ConstEval env fuel a (.value (.int t x)))
      (hb : ConstEval env fuel b (.value (.int t y)))
      (hop : Core.plain t op x y = .trap k) :
      ConstEval env (fuel + 1) (.plain op a b)
        (.failure ⟨.plain op, .plain op a b, .arithmetic k⟩)
  | plainNeg_ok {fuel a t x n}
      (ha : ConstEval env fuel a (.value (.int t x)))
      (hop : Core.plainNeg t x = .ok n) :
      ConstEval env (fuel + 1) (.plainNeg a) (.value (.int t n))
  | plainNeg_failure {fuel a t x k}
      (ha : ConstEval env fuel a (.value (.int t x)))
      (hop : Core.plainNeg t x = .trap k) :
      ConstEval env (fuel + 1) (.plainNeg a)
        (.failure ⟨.plainNeg, .plainNeg a, .arithmetic k⟩)
  | bit_ok {fuel op a b t x y}
      (ha : ConstEval env fuel a (.value (.int t x)))
      (hb : ConstEval env fuel b (.value (.int t y))) :
      ConstEval env (fuel + 1) (.bit op a b) (.value (.int t (Core.bitwise t op x y)))
  | bitNot_ok {fuel a t x}
      (ha : ConstEval env fuel a (.value (.int t x))) :
      ConstEval env (fuel + 1) (.bitNot a) (.value (.int t (Core.bitNot t x)))
  | cmp_ok {fuel op a b t x y}
      (ha : ConstEval env fuel a (.value (.int t x)))
      (hb : ConstEval env fuel b (.value (.int t y))) :
      ConstEval env (fuel + 1) (.cmp op a b) (.value (.bool (op.eval x y)))
  | not_ok {fuel a x}
      (ha : ConstEval env fuel a (.value (.bool x))) :
      ConstEval env (fuel + 1) (.not a) (.value (.bool (!x)))
  | and_ok {fuel a b x y}
      (ha : ConstEval env fuel a (.value (.bool x)))
      (hb : ConstEval env fuel b (.value (.bool y))) :
      ConstEval env (fuel + 1) (.and a b) (.value (.bool (x && y)))
  | or_ok {fuel a b x y}
      (ha : ConstEval env fuel a (.value (.bool x)))
      (hb : ConstEval env fuel b (.value (.bool y))) :
      ConstEval env (fuel + 1) (.or a b) (.value (.bool (x || y)))
  | convert_int_ok {fuel t a s x}
      (ha : ConstEval env fuel a (.value (.int s x)))
      (hloss : Lossless s t) :
      ConstEval env (fuel + 1) (.convert t a) (.value (.int t x))
  | convert_int_invalid {fuel t a s x}
      (ha : ConstEval env fuel a (.value (.int s x)))
      (hloss : ¬ Lossless s t) :
      ConstEval env (fuel + 1) (.convert t a) (.invalid (.convert t a))
  | convert_bool_ok {fuel t a b}
      (ha : ConstEval env fuel a (.value (.bool b))) :
      ConstEval env (fuel + 1) (.convert t a) (.value (.int t (boolToInt b)))
  | convert_unit_invalid {fuel t a}
      (ha : ConstEval env fuel a (.value .unit)) :
      ConstEval env (fuel + 1) (.convert t a) (.invalid (.convert t a))
  | tryConvert_ok {fuel t a s x n}
      (ha : ConstEval env fuel a (.value (.int s x)))
      (hop : Core.tryConvert t x = .ok n) :
      ConstEval env (fuel + 1) (.tryConvert t a) (.value (.int t n))
  | tryConvert_failure {fuel t a s x}
      (ha : ConstEval env fuel a (.value (.int s x)))
      (hop : Core.tryConvert t x = .error .outOfRange) :
      ConstEval env (fuel + 1) (.tryConvert t a)
        (.failure ⟨.tryConvert t, .tryConvert t a, .outOfRange⟩)
  | intUnary_invalid {fuel e a va} (hchild : CExpr.IntUnaryChild e a)
      (ha : ConstEval env fuel a (.value va))
      (hnot : ∀ t x, va ≠ .int t x) :
      ConstEval env (fuel + 1) e (.invalid e)
  | not_invalid {fuel a va}
      (ha : ConstEval env fuel a (.value va))
      (hnot : ∀ b, va ≠ .bool b) :
      ConstEval env (fuel + 1) (.not a) (.invalid (.not a))
  | intBinary_invalid {fuel e a b va vb} (hchild : CExpr.IntBinaryChildren e a b)
      (ha : ConstEval env fuel a (.value va))
      (hb : ConstEval env fuel b (.value vb))
      (hnot : ∀ t x y, va ≠ .int t x ∨ vb ≠ .int t y) :
      ConstEval env (fuel + 1) e (.invalid e)
  | boolBinary_invalid {fuel e a b va vb} (hchild : CExpr.BoolBinaryChildren e a b)
      (ha : ConstEval env fuel a (.value va))
      (hb : ConstEval env fuel b (.value vb))
      (hnot : ∀ x y, va ≠ .bool x ∨ vb ≠ .bool y) :
      ConstEval env (fuel + 1) e (.invalid e)

/-- Two operand types that are one integer type, or `none`. -/
def sameIntTy : Option CTy → Option CTy → Option IntTy
  | some (.int t), some (.int t') => if t = t' then some t else none
  | _, _ => none

/-- Both operand types are `bool`. -/
def bothBool : Option CTy → Option CTy → Bool
  | some .bool, some .bool => true
  | _, _ => false

/-- The static type of an initializer, given earlier constants' types. This checks the closed
grammar and operand rules but deliberately does not predict arithmetic success. -/
def CExpr.ty? (Γ : AList ConstId CTy) : CExpr → Option CTy
  | .int t n => if t.InRange n then some (.int t) else none
  | .bool _ => some .bool
  | .unit => some .unit
  | .ref c => AList.lookup Γ c
  | .wrapping _ a b | .saturating _ a b | .plain _ a b | .bit _ a b =>
    (sameIntTy (a.ty? Γ) (b.ty? Γ)).map .int
  | .plainNeg a | .bitNot a => match a.ty? Γ with
    | some (.int t) => some (.int t)
    | _ => none
  | .cmp _ a b => (sameIntTy (a.ty? Γ) (b.ty? Γ)).map fun _ => .bool
  | .not a => match a.ty? Γ with
    | some .bool => some .bool
    | _ => none
  | .and a b | .or a b => if bothBool (a.ty? Γ) (b.ty? Γ) then some .bool else none
  | .convert t a => match a.ty? Γ with
    | some (.int s) => if Lossless s t then some (.int t) else none
    | some .bool => some (.int t)
    | _ => none
  | .tryConvert t a => match a.ty? Γ with
    | some (.int _) => some (.int t)
    | _ => none

/-- The language's types as far as constants need them. Every scalar constant type in `CTy` is
`Copy` (`ofC_copy`), while the surrounding type universe `Ty` may also contain non-`Copy` types. -/
structure ConstTypes where
  Ty : Type
  deq : DecidableEq Ty
  isCopy : Ty → Bool
  ofC : CTy → Ty
  ofC_inj : ∀ a b, ofC a = ofC b → a = b
  ofC_copy : ∀ c, isCopy (ofC c) = true

attribute [instance] ConstTypes.deq

/-- A constant declaration: `const id: ty = init;`. -/
structure ConstDecl (U : ConstTypes) where
  id : ConstId
  ty : U.Ty
  init : CExpr
  deriving DecidableEq

/-- The first evaluation refusal, including the identity of the constant being checked. -/
inductive ConstEvalRefusal where
  | operation (id : ConstId) (failure : ConstFailure)
  | outOfFuel (id : ConstId)
  | invalid (id : ConstId) (expression : CExpr)
  deriving DecidableEq, Repr

/-- Evaluate every declaration in Core dependency order, used or unused. Typing is not rechecked
here; it is the separate `typeConsts` judgment. -/
def evalConsts (U : ConstTypes) (fuel : Nat) : List (ConstDecl U) → AList ConstId CVal →
    Except ConstEvalRefusal (AList ConstId CVal)
  | [], env => .ok env
  | d :: ds, env => match d.init.eval env fuel with
    | .value v => evalConsts U fuel ds ((d.id, v) :: env)
    | .failure f => .error (.operation d.id f)
    | .outOfFuel => .error (.outOfFuel d.id)
    | .invalid e => .error (.invalid d.id e)

/-- Independent declaration semantics: all initializers are demanded, including unused ones.
A successful prefix extends the environment; the first non-value stops the list. -/
inductive ConstsRun (U : ConstTypes) (fuel : Nat) :
    List (ConstDecl U) → AList ConstId CVal →
      Except ConstEvalRefusal (AList ConstId CVal) → Prop where
  | nil (env) : ConstsRun U fuel [] env (.ok env)
  | value (d ds env v result) (head : ConstEval env fuel d.init (.value v))
      (tail : ConstsRun U fuel ds ((d.id, v) :: env) result) :
      ConstsRun U fuel (d :: ds) env result
  | failure (d ds env f) (head : ConstEval env fuel d.init (.failure f)) :
      ConstsRun U fuel (d :: ds) env (.error (.operation d.id f))
  | exhausted (d ds env) (head : ConstEval env fuel d.init .outOfFuel) :
      ConstsRun U fuel (d :: ds) env (.error (.outOfFuel d.id))
  | invalid (d ds env e) (head : ConstEval env fuel d.init (.invalid e)) :
      ConstsRun U fuel (d :: ds) env (.error (.invalid d.id e))

/-- Type the constants in dependency order, without evaluating. Each initializer is typed with the
types of earlier constants; its declared type must equal the initializer's scalar type, and a name
may be declared once. Because every scalar type in `CTy` is `Copy` (`U.ofC_copy`), `U.ofC T = d.ty`
already entails `U.isCopy d.ty = true` without a separate runtime `isCopy` check. -/
def typeConsts (U : ConstTypes) : List (ConstDecl U) → AList ConstId CTy →
    Option (AList ConstId CTy)
  | [], Γ => some Γ
  | d :: ds, Γ => match d.init.ty? Γ with
    | some T =>
      if U.ofC T = d.ty ∧ AList.lookup Γ d.id = none then
        typeConsts U ds ((d.id, T) :: Γ)
      else none
    | none => none

/-- Static typing is named separately from evaluation. Admission requires that the whole list types and every
initializer evaluates successfully within `fuel`, whether used or not (owner decision 2026-11-08,
item 18, revising TYPE-22; item 26 for `Copy`). Evaluation refusal identifies the constant and its
failure or exhausted budget. -/
def ConstsTyped (U : ConstTypes) (ds : List (ConstDecl U)) : Prop :=
  (typeConsts U ds []).isSome = true

/-- Every declaration, including unused declarations, computes a value within the supplied fuel.
This is independent of static typing and Copy classification. -/
def ConstsEvaluate (U : ConstTypes) (fuel : Nat) (ds : List (ConstDecl U)) : Prop :=
  (evalConsts U fuel ds []).isOk = true

instance (U : ConstTypes) (ds : List (ConstDecl U)) : Decidable (ConstsTyped U ds) := by
  unfold ConstsTyped
  infer_instance

instance (U : ConstTypes) (fuel : Nat) (ds : List (ConstDecl U)) :
    Decidable (ConstsEvaluate U fuel ds) := by
  unfold ConstsEvaluate
  infer_instance

/-- **Rule: constant declarations are admitted** by both independent checks: statically known
module constants (Q6.2), within the fixed closed evaluator (P17b). Owner decision 2026-11-08,
item 18 revises TYPE-22 to require successful kernel-checked evaluation within fuel, including
unused constants; item 26 requires Copy declared types. -/
def ConstsAdmitted (U : ConstTypes) (fuel : Nat) (ds : List (ConstDecl U)) : Prop :=
  ConstsTyped U ds ∧ ConstsEvaluate U fuel ds

instance (U : ConstTypes) (fuel : Nat) (ds : List (ConstDecl U)) :
    Decidable (ConstsAdmitted U fuel ds) := by
  unfold ConstsAdmitted ConstsTyped ConstsEvaluate
  infer_instance

/-! ### Source dependency order and cycle refusal -/

/-- Actual references in a module's source declarations: `a` depends on `b` exactly when `b` occurs
anywhere in `a`'s initializer. Source declaration order imposes no restriction. -/
def dependsOn (U : ConstTypes) (ds : List (ConstDecl U)) (a b : ConstId) : Bool :=
  ds.any fun d => decide (d.id = a) && d.init.refs.contains b

/-- A nonempty chain of actual references. A one-node chain names a declared constant. -/
def refChain (U : ConstTypes) (ds : List (ConstDecl U)) : List ConstId → Bool
  | [] => false
  | [c] => (ds.map ConstDecl.id).contains c
  | a :: b :: rest => dependsOn U ds a b && refChain U ds (b :: rest)

/-- A cycle diagnostic chain `c₀ → c₁ → … → c₀`. Every adjacent pair is an actual reference. -/
def CycleChain (U : ConstTypes) (ds : List (ConstDecl U)) (chain : List ConstId) : Prop :=
  ∃ c rest, chain = c :: rest ++ [c] ∧ refChain U ds chain = true

/-- An actual source reference, stated using declaration and expression membership rather than
the executable Boolean dependency test. -/
def SourceReference (U : ConstTypes) (ds : List (ConstDecl U)) (a b : ConstId) : Prop :=
  ∃ d ∈ ds, d.id = a ∧ b ∈ d.init.refs

/-- Every edge of a diagnostic is an actual source reference; its final identity is declared. -/
def ActualRefChain (U : ConstTypes) (ds : List (ConstDecl U)) : List ConstId → Prop
  | [] => False
  | [c] => ∃ d ∈ ds, d.id = c
  | a :: b :: rest => SourceReference U ds a b ∧ ActualRefChain U ds (b :: rest)

/-- No cycle of the source reference relation. -/
def Acyclic (U : ConstTypes) (ds : List (ConstDecl U)) : Prop :=
  ∀ chain, ¬ CycleChain U ds chain

/-- Source name resolution: identities are unique and every reference denotes a declaration in
this module. An unresolved name is a static refusal, independently of reference cycles. -/
def SourceNamesResolved (U : ConstTypes) (source : List (ConstDecl U)) : Prop :=
  (source.map ConstDecl.id).Nodup ∧
    ∀ d ∈ source, ∀ c ∈ d.init.refs, c ∈ source.map ConstDecl.id

instance (U : ConstTypes) (source : List (ConstDecl U)) :
    Decidable (SourceNamesResolved U source) := by
  unfold SourceNamesResolved
  infer_instance

/-- Every initializer reference occurs in the earlier prefix. This includes nested operands and
rejects unresolved names as well as self-reference. -/
def ReferencesEarlier (U : ConstTypes) : List (ConstDecl U) → List ConstId → Prop
  | [], _ => True
  | d :: ds, earlier => (∀ c ∈ d.init.refs, c ∈ earlier) ∧
    ReferencesEarlier U ds (d.id :: earlier)

instance instDecidableReferencesEarlier (U : ConstTypes) :
    ∀ (ds : List (ConstDecl U)) (earlier : List ConstId),
      Decidable (ReferencesEarlier U ds earlier)
  | [], _ => isTrue trivial
  | d :: ds, earlier =>
    have := instDecidableReferencesEarlier U ds (d.id :: earlier)
    inferInstanceAs (Decidable ((∀ c ∈ d.init.refs, c ∈ earlier) ∧
      ReferencesEarlier U ds (d.id :: earlier)))

/-- README obligation O1: elaboration supplies a permutation with unique identities and with
all initializer references in the earlier prefix. Source order itself imposes no restriction. -/
def DependencyOrder (U : ConstTypes) (source order : List (ConstDecl U)) : Prop :=
  order.Perm source ∧ (source.map ConstDecl.id).Nodup ∧ ReferencesEarlier U order []

instance (U : ConstTypes) (source order : List (ConstDecl U)) :
    Decidable (DependencyOrder U source order) := by
  unfold DependencyOrder
  infer_instance

/-- Ready when every referenced constant has already been emitted into `earlier`. -/
def readyDecl (U : ConstTypes) (earlier : List ConstId) (d : ConstDecl U) : Bool :=
  d.init.refs.all (fun c => earlier.contains c)

/-- Extract the first declaration whose references are all in `earlier`, returning it together
with the remaining declarations in their relative order. -/
def takeReady (U : ConstTypes) (earlier : List ConstId) :
    List (ConstDecl U) → Option (ConstDecl U × List (ConstDecl U))
  | [] => none
  | d :: ds =>
    if readyDecl U earlier d then
      some (d, ds)
    else
      (takeReady U earlier ds).map fun (picked, rest) => (picked, d :: rest)

/-- Repeatedly emit the first ready declaration; if a cycle blocks progress, append the remaining
declarations unchanged so the function always returns a permutation of the input. -/
def sortConstsAux (U : ConstTypes) :
    Nat → List ConstId → List (ConstDecl U) → List (ConstDecl U)
  | 0, _, remaining => remaining
  | fuel + 1, earlier, remaining =>
    match takeReady U earlier remaining with
    | some (d, rest) => d :: sortConstsAux U fuel (d.id :: earlier) rest
    | none => remaining

/-- Canonical fuel-free topological sort of `source` (Kahn's algorithm with source tie-breaking).
When `source` is name-resolved and acyclic, `sortConsts U source` is a certified `DependencyOrder`
(`DependencySortingComplete` in `Proofs/Const.lean`). -/
def sortConsts (U : ConstTypes) (source : List (ConstDecl U)) : List (ConstDecl U) :=
  sortConstsAux U source.length [] source

/-- After source name resolution, elaboration's required result (owner decision 2026-11-08, item 19): a dependency order for Core,
or refusal with a real cycle chain. This is an O1 obligation on the source elaborator, not an
additional evaluator. -/
inductive ElaboratedConsts (U : ConstTypes) (source : List (ConstDecl U)) where
  | ordered (order : List (ConstDecl U)) (sorted : DependencyOrder U source order)
  | cycle (chain : List ConstId) (realCycle : CycleChain U source chain)

/-! ### Examples

A type universe with the scalar types and one type that is not `Copy`, `text`. -/

namespace ConstDemo

inductive T where
  | c (t : CTy)
  | text
  deriving DecidableEq, Repr

def U : ConstTypes where
  Ty := T
  deq := inferInstance
  isCopy := fun
    | .c _ => true
    | .text => false
  ofC := .c
  ofC_inj := fun _ _ h => by cases h; rfl
  ofC_copy := fun _ => rfl

def cid (n : String) : ConstId := ⟨⟨["m"]⟩, n⟩

/-- Accepted (rule `ConcreteSpec.Core.ConstsAdmitted`): wrapping, bitwise and lossless operations
from the old fragment still compute their values. -/
example : ConstsAdmitted U 20 [⟨cid "MASK", .c (.int .u8), .int .u8 0x0f⟩,
    ⟨cid "HIGH", .c (.int .u8), .wrapping .mul (.ref (cid "MASK")) (.int .u8 16)⟩,
    ⟨cid "BOTH", .c (.int .u8), .bit .or (.ref (cid "MASK")) (.ref (cid "HIGH"))⟩,
    ⟨cid "WIDE", .c (.int .u16), .convert .u16 (.ref (cid "BOTH"))⟩] := by decide

/-- Accepted (rule `ConcreteSpec.Core.ConstsAdmitted`): plain division succeeds as `100 / 7 = 14`,
and an unused successful constant is evaluated and admitted too. -/
example : ConstsAdmitted U 20 [⟨cid "QUOT", .c (.int .i16), .plain .div (.int .i16 100) (.int .i16 7)⟩,
    ⟨cid "UNUSED", .c (.int .u8), .plain .add (.int .u8 2) (.int .u8 3)⟩] := by decide

/-- The accepted division and unused constant have these values. -/
example : (evalConsts U 20 [⟨cid "QUOT", .c (.int .i16),
      .plain .div (.int .i16 100) (.int .i16 7)⟩,
    ⟨cid "UNUSED", .c (.int .u8), .plain .add (.int .u8 2) (.int .u8 3)⟩] []).toOption.bind
      (fun env => AList.lookup env (cid "QUOT")) = some (.int .i16 14) := by decide

/-- Accepted (rule `ConcreteSpec.Core.ConstsAdmitted`): fallible conversion and negation succeed. -/
example : ConstsAdmitted U 20 [⟨cid "N", .c (.int .u8), .tryConvert .u8 (.int .i16 255)⟩,
    ⟨cid "M", .c (.int .i8), .plainNeg (.int .i8 5)⟩] := by decide

/-- Refused (rule `ConcreteSpec.Core.ConstsAdmitted`): `1 / 0` reports division by zero at the
plain-division sub-expression; it is not a trap. -/
example : ¬ ConstsAdmitted U 20 [⟨cid "BAD", .c (.int .i8),
    .plain .div (.int .i8 1) (.int .i8 0)⟩] := by decide

/-- The division refusal names both the constant and the operation. -/
example : evalConsts U 20 [⟨cid "BAD", .c (.int .i8),
    .plain .div (.int .i8 1) (.int .i8 0)⟩] [] =
    .error (.operation (cid "BAD") ⟨.plain .div,
      .plain .div (.int .i8 1) (.int .i8 0), .arithmetic .divisionByZero⟩) := by decide

/-- Refused (rule `ConcreteSpec.Core.ConstsAdmitted`): signed `i8` addition `127 + 1` overflows. -/
example : ¬ ConstsAdmitted U 20 [⟨cid "BAD", .c (.int .i8),
    .plain .add (.int .i8 127) (.int .i8 1)⟩] := by decide

/-- Refused (rule `ConcreteSpec.Core.ConstsAdmitted`): a shift by the width is out of range. -/
example : ¬ ConstsAdmitted U 20 [⟨cid "BAD", .c (.int .u8),
    .plain .shl (.int .u8 1) (.int .u8 8)⟩] := by decide

/-- Refused (rule `ConcreteSpec.Core.ConstsAdmitted`): an UNUSED constant is evaluated and its
failure refuses the module although no other constant refers to it. -/
example : ¬ ConstsAdmitted U 20 [⟨cid "USED", .c (.int .u8), .int .u8 1⟩,
    ⟨cid "UNUSED", .c (.int .i8), .plain .add (.int .i8 127) (.int .i8 1)⟩] := by decide

/-- Refused (rule `ConcreteSpec.Core.ConstsAdmitted`): a finite initializer exhausts an explicitly
too-small fuel bound. -/
example : ¬ ConstsAdmitted U 1 [⟨cid "DEEP", .c (.int .u8),
    .plain .add (.plain .add (.int .u8 1) (.int .u8 2)) (.int .u8 3)⟩] := by decide

/-- The same finite initializer succeeds with adequate fuel. -/
example : ConstsAdmitted U 5 [⟨cid "DEEP", .c (.int .u8),
    .plain .add (.plain .add (.int .u8 1) (.int .u8 2)) (.int .u8 3)⟩] := by decide

/-- Refused (rule `ConcreteSpec.Core.ConstsAdmitted`): the checked conversion reports `OutOfRange`. -/
example : ¬ ConstsAdmitted U 20 [⟨cid "BAD", .c (.int .u8),
    .tryConvert .u8 (.int .i16 256)⟩] := by decide

/-- Refused (rule `ConcreteSpec.Core.ConstsAdmitted`): a non-`Copy` declared type (`text`) lies
outside the `Copy` scalar constant universe (`U.ofC_copy`), so no constant initializer can inhabit
it. -/
example : U.isCopy .text = false ∧
    ¬ ConstsAdmitted U 20 [⟨cid "S", .text, .unit⟩] := by
  exact ⟨rfl, by decide⟩

/-- Refused: a reference later in an already supplied Core dependency order is invalid. -/
example : ¬ ConstsAdmitted U 20 [⟨cid "A", .c (.int .u8), .ref (cid "B")⟩,
    ⟨cid "B", .c (.int .u8), .int .u8 1⟩] := by decide

/-- Refused: a literal out of range, and operands of different types. -/
example : ¬ ConstsAdmitted U 20 [⟨cid "A", .c (.int .u8), .int .u8 256⟩] := by decide

example : ¬ ConstsAdmitted U 20 [⟨cid "A", .c (.int .u16),
    .wrapping .add (.int .u8 1) (.int .u16 1)⟩] := by decide

/-- Refused: one name declared twice. -/
example : ¬ ConstsAdmitted U 20 [⟨cid "A", .c .bool, .bool true⟩,
    ⟨cid "A", .c .bool, .bool false⟩] := by decide

/-- Accepted (rule `ConcreteSpec.Core.ConstsAdmitted`): source `A = B + 1` before `B = 2` is sorted
to `[B, A]` by `sortConsts`; this is item 19's forward reference. -/
example : DependencyOrder U [⟨cid "A", .c (.int .u8),
      .plain .add (.ref (cid "B")) (.int .u8 1)⟩,
    ⟨cid "B", .c (.int .u8), .int .u8 2⟩]
    (sortConsts U [⟨cid "A", .c (.int .u8),
      .plain .add (.ref (cid "B")) (.int .u8 1)⟩,
    ⟨cid "B", .c (.int .u8), .int .u8 2⟩]) ∧
    ConstsAdmitted U 20 (sortConsts U [⟨cid "A", .c (.int .u8),
      .plain .add (.ref (cid "B")) (.int .u8 1)⟩,
    ⟨cid "B", .c (.int .u8), .int .u8 2⟩]) := by
  refine ⟨⟨List.Perm.swap _ _ [], by decide, ?_⟩, by decide⟩
  simp [sortConsts, sortConstsAux, takeReady, readyDecl, ReferencesEarlier, CExpr.refs, cid]

/-- Refused (rule `ConcreteSpec.Core.ConstsAdmitted`): elaboration reports the real two-constant
cycle `A → B → A`, and neither declaration order is admissible. -/
example : CycleChain U [⟨cid "A", .c (.int .u8), .ref (cid "B")⟩,
    ⟨cid "B", .c (.int .u8), .ref (cid "A")⟩] [cid "A", cid "B", cid "A"] := by
  exact ⟨cid "A", [cid "B"], rfl, by decide⟩

/-- Refused (rule `ConcreteSpec.Core.ConstsAdmitted`): self-reference reports `A → A`. -/
example : CycleChain U [⟨cid "A", .c (.int .u8), .ref (cid "A")⟩]
    [cid "A", cid "A"] := by
  exact ⟨cid "A", [], rfl, by decide⟩

/-- A malformed diagnostic: the claimed chain `A → B → A` is
not real when `A` has no reference to `B`, so it cannot justify cycle refusal. -/
example : ¬ CycleChain U [⟨cid "A", .c (.int .u8), .int .u8 1⟩,
    ⟨cid "B", .c (.int .u8), .ref (cid "A")⟩] [cid "A", cid "B", cid "A"] := by
  intro h
  obtain ⟨c, rest, _, href⟩ := h
  have : refChain U [⟨cid "A", .c (.int .u8), .int .u8 1⟩,
      ⟨cid "B", .c (.int .u8), .ref (cid "A")⟩] [cid "A", cid "B", cid "A"] = false := by decide
  rw [this] at href
  cases href

/-- Accepted (rule `ConcreteSpec.Core.ConstsAdmitted`): the sorted forward reference computes
`A = 3`, rather than merely accepting a sorting certificate. -/
example : (evalConsts U 3
    [⟨cid "B", .c (.int .u8), .int .u8 2⟩,
     ⟨cid "A", .c (.int .u8), .plain .add (.ref (cid "B")) (.int .u8 1)⟩] []).toOption.bind
    (fun env => AList.lookup env (cid "A")) = some (.int .u8 3) := by decide

/-- Refused (rule `ConcreteSpec.Core.ConstsAdmitted`): neither order of a two-constant cycle
passes the independent static and evaluation checks. -/
example : ¬ ConstsAdmitted U 3
    [⟨cid "A", .c .bool, .ref (cid "B")⟩, ⟨cid "B", .c .bool, .ref (cid "A")⟩] ∧
    ¬ ConstsAdmitted U 3
    [⟨cid "B", .c .bool, .ref (cid "A")⟩, ⟨cid "A", .c .bool, .ref (cid "B")⟩] := by decide

/-- Refused (rule `ConcreteSpec.Core.ConstsAdmitted`): self-reference has no Core value. -/
example : ¬ ConstsAdmitted U 3 [⟨cid "A", .c .bool, .ref (cid "A")⟩] := by decide

/-- Self-reference also fails the dependency-order contract, even for a singleton list. -/
example : ¬ DependencyOrder U [⟨cid "A", .c .bool, .ref (cid "A")⟩]
    [⟨cid "A", .c .bool, .ref (cid "A")⟩] := by
  intro h
  have hs := h.2.2
  simp [ReferencesEarlier, CExpr.refs] at hs

/-- Fuel refusal carries the identity of the constant, including an unused one. -/
example : evalConsts U 1 [⟨cid "UNUSED", .c (.int .u8),
    .plain .add (.int .u8 1) (.int .u8 2)⟩] [] = .error (.outOfFuel (cid "UNUSED")) := by decide

/-- An unresolved reference is not a dependency-order certificate, even for an acyclic singleton. -/
example : ¬ DependencyOrder U [⟨cid "A", .c .bool, .ref (cid "MISSING")⟩]
    [⟨cid "A", .c .bool, .ref (cid "MISSING")⟩] := by
  intro h
  have hs := h.2.2
  simp [ReferencesEarlier, CExpr.refs] at hs

/-- The evaluation check is independent: this list types but division by zero refuses it. -/
example : ConstsTyped U [⟨cid "BAD", .c (.int .i8),
    .plain .div (.int .i8 1) (.int .i8 0)⟩] ∧
    ¬ ConstsEvaluate U 3 [⟨cid "BAD", .c (.int .i8),
    .plain .div (.int .i8 1) (.int .i8 0)⟩] := by
  exact ⟨by decide, by decide⟩

end ConstDemo

end ConcreteSpec.Core
