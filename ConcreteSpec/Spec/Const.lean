import ConcreteSpec.Spec.Scalar

/-!
# Constants: the closed total initializer subset

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). Part of `ConcreteSpec.Spec`;
see `ConcreteSpec/README.md`. Foundation contract C2, with the owner decision of 2026-11-08,
item 26.

A module constant (`const N: T = e;`) is computed at compile time, and its value must be of a
`Copy` type (owner decision 2026-11-08, item 26: the direction). Its initializer is written in a
*closed total subset* of expressions, defined here, and nothing else:

- literals of the integer types (in range, so a negative literal of a signed type is one literal,
  not a negation), `true`, `false` and `()`;
- references to constants *earlier in the dependency order* (so no constant depends on itself);
- the wrapping and saturating forms of add, subtract and multiply (ARITH-4), the bitwise
  operations, comparisons of integers, and `!`, `&&`, `||` on booleans (both operands of `&&` and
  `||` are evaluated: nothing in the subset can fail or has an effect, so short-circuiting could not
  be observed);
- the one lossless conversion (owner decision 2026-11-08, item 11(a); its spelling is not yet
  approved), from an integer type to one whose range contains it, or from `bool` to an integer
  type (ARITH-5).

Every operation of the subset is *total*: it has a value for all well-typed operands, so a
well-typed initializer always has a value (`eval_total`, in `Proofs/Const.lean`), computed without
fuel, state, authority or the target. An initializer outside the subset, an ill-typed one, a
literal out of its type's range and a reference to a constant not earlier in the order are refused. In
particular plain arithmetic, division, shifts and checked operations are *not* in the subset:
whether a failing operation may appear in an initializer, refused when it fails at compile time,
is a question for the owner, not decided here.

**Order.** Core receives a module's constants as a list in *dependency order*: each initializer
refers only to constants before it. Elaboration supplies that order. Whether the source may name a
constant written later in the file (elaboration then sorting the declarations, and refusing a
cycle) is a question for the owner, not decided here; this module refuses a cycle either way.

**Static admission.** The compiler checks constants by typing them (`typeConsts`), without
evaluating: a list is admitted exactly when it types (`admitted_iff_typed`), so the evaluation of
an admitted list cannot fail, and every constant then has a valid value of its declared type
(`evalConsts_sound`).

**Compile-time evaluation** (owner decision 2026-11-08, item 26, revising Q6.4/TYPE-22's
evaluation at each use). A constant's value is computed once, at compile time, from this closed
subset, and its type must be `Copy`; a use of the constant is that value. The decided compile-time
evaluation reopening conditions (P17b) are still to be checked against this subset: that check is
a task recorded in the report, not an open question.

The constant types are an interface (`ConstTypes`): the language's type universe, a `Copy`
predicate on it, and the embedding of the scalar types the subset computes with, which must all be
`Copy`. Widening the subset (to `Copy` structs, say) revises `CTy`, `CVal` and `CExpr` and is an
owner decision; this interface only fixes what any widening must keep: every value it can produce is
of a `Copy` type and every operation is total.
-/

namespace ConcreteSpec.Core

/-- An initializer of the closed total subset. -/
inductive CExpr where
  | int (t : IntTy) (n : Int)
  | bool (b : Bool)
  | unit
  | ref (c : ConstId)
  | wrapping (op : WrapOp) (a b : CExpr)
  | saturating (op : WrapOp) (a b : CExpr)
  | bit (op : BitOp) (a b : CExpr)
  | bitNot (a : CExpr)
  | cmp (op : CmpOp) (a b : CExpr)
  | not (a : CExpr)
  | and (a b : CExpr)
  | or (a b : CExpr)
  /-- The lossless conversion to `t`. -/
  | convert (t : IntTy) (a : CExpr)
  deriving DecidableEq, Repr

/-- Two integer operands of one type, or `none`. -/
def sameInt : Option CVal → Option CVal → Option (IntTy × Int × Int)
  | some (.int t x), some (.int t' y) => if t = t' then some (t, x, y) else none
  | _, _ => none

/-- Two boolean operands, or `none`. -/
def twoBools : Option CVal → Option CVal → Option (Bool × Bool)
  | some (.bool x), some (.bool y) => some (x, y)
  | _, _ => none

/-- The value of an initializer, given the earlier constants' values. `none`: refused (outside the
subset's typing, a literal out of range, or an unknown constant). -/
def CExpr.eval (env : AList ConstId CVal) : CExpr → Option CVal
  | .int t n => if t.InRange n then some (.int t n) else none
  | .bool b => some (.bool b)
  | .unit => some .unit
  | .ref c => AList.lookup env c
  | .wrapping op a b => (sameInt (a.eval env) (b.eval env)).map fun (t, x, y) =>
      .int t (Core.wrapping t op x y)
  | .saturating op a b => (sameInt (a.eval env) (b.eval env)).map fun (t, x, y) =>
      .int t (Core.saturating t op x y)
  | .bit op a b => (sameInt (a.eval env) (b.eval env)).map fun (t, x, y) =>
      .int t (Core.bitwise t op x y)
  | .bitNot a => match a.eval env with
    | some (.int t x) => some (.int t (Core.bitNot t x))
    | _ => none
  | .cmp op a b => (sameInt (a.eval env) (b.eval env)).map fun (_, x, y) => .bool (op.eval x y)
  | .not a => match a.eval env with
    | some (.bool x) => some (.bool (!x))
    | _ => none
  | .and a b => (twoBools (a.eval env) (b.eval env)).map fun (x, y) => .bool (x && y)
  | .or a b => (twoBools (a.eval env) (b.eval env)).map fun (x, y) => .bool (x || y)
  | .convert t a => match a.eval env with
    | some (.int s x) => if Lossless s t then some (.int t x) else none
    | some (.bool b) => some (.int t (boolToInt b))
    | _ => none

/-- Two operand types that are one integer type, or `none`. -/
def sameIntTy : Option CTy → Option CTy → Option IntTy
  | some (.int t), some (.int t') => if t = t' then some t else none
  | _, _ => none

/-- Both operand types are `bool`. -/
def bothBool : Option CTy → Option CTy → Bool
  | some .bool, some .bool => true
  | _, _ => false

/-- The type of an initializer, given the earlier constants' types; `none`: refused. It mirrors
`eval` without computing, and `eval_total` proves that a well-typed initializer has a value of its
type. -/
def CExpr.ty? (Γ : AList ConstId CTy) : CExpr → Option CTy
  | .int t n => if t.InRange n then some (.int t) else none
  | .bool _ => some .bool
  | .unit => some .unit
  | .ref c => AList.lookup Γ c
  | .wrapping _ a b | .saturating _ a b | .bit _ a b => (sameIntTy (a.ty? Γ) (b.ty? Γ)).map .int
  | .bitNot a => match a.ty? Γ with
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

/-- The language's types as far as constants need them: a `Copy` predicate, and the scalar types
the subset computes with, each of which must be `Copy` (the law a lawful instance proves). -/
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

/-- Compute the constants in dependency order. Each initializer sees only the constants before it;
its declared type must be `Copy` and must be the type of its value; a name may be declared once.
`none`: the declarations are refused. -/
def evalConsts (U : ConstTypes) : List (ConstDecl U) → AList ConstId CVal →
    Option (AList ConstId CVal)
  | [], env => some env
  | d :: ds, env =>
    match d.init.eval env with
    | some v =>
      if U.isCopy d.ty = true ∧ U.ofC v.ty = d.ty ∧ AList.lookup env d.id = none then
        evalConsts U ds ((d.id, v) :: env)
      else none
    | none => none

/-- Type the constants in dependency order, without evaluating: what the compiler checks. Each
initializer is typed with the types of the constants before it; its declared type must be `Copy`
and must be the initializer's type; a name may be declared once. -/
def typeConsts (U : ConstTypes) : List (ConstDecl U) → AList ConstId CTy →
    Option (AList ConstId CTy)
  | [], Γ => some Γ
  | d :: ds, Γ =>
    match d.init.ty? Γ with
    | some T =>
      if U.isCopy d.ty = true ∧ U.ofC T = d.ty ∧ AList.lookup Γ d.id = none then
        typeConsts U ds ((d.id, T) :: Γ)
      else none
    | none => none

/-- **Rule: constant declarations are admitted** when every initializer is in the closed total
subset and well typed, refers only to constants before it in the dependency order, and has a
`Copy` declared type that is its value's type (owner decision 2026-11-08, item 26; statically known module constants, Q6.2;
compile-time evaluation stays within P17b's limits: this is a fixed, closed evaluator, not general
compile-time execution). Their values are the result of `evalConsts`, computed at compile time. -/
def ConstsAdmitted (U : ConstTypes) (ds : List (ConstDecl U)) : Prop :=
  (evalConsts U ds []).isSome = true

instance (U : ConstTypes) (ds : List (ConstDecl U)) : Decidable (ConstsAdmitted U ds) := by
  unfold ConstsAdmitted; infer_instance

/-! ### Examples

A type universe with the scalar types and one type that is not `Copy`, `text`. -/

namespace ConstDemo

inductive T where
  | c (t : CTy)
  | text
  deriving DecidableEq

def U : ConstTypes where
  Ty := T
  deq := inferInstance
  isCopy := fun
    | .c _ => true
    | .text => false
  ofC := .c
  ofC_inj := fun _ _ h => by cases h; rfl
  ofC_copy := fun _ => rfl

def cid (n : String) : ConstId := ConstId.mk (ModuleId.mk ⟨"const"⟩ ["m"]) n

/-- Accepted (rule `ConcreteSpec.Core.ConstsAdmitted`):
```
const MASK: u8 = 0x0f;
const HIGH: u8 = wrapping_mul(MASK, 16);
const BOTH: u8 = MASK | HIGH;
const WIDE: u16 = <lossless conversion>(BOTH);
```
and their values are computed at compile time. -/
example : ConstsAdmitted U [⟨cid "MASK", .c (.int .u8), .int .u8 0x0f⟩,
    ⟨cid "HIGH", .c (.int .u8), .wrapping .mul (.ref (cid "MASK")) (.int .u8 16)⟩,
    ⟨cid "BOTH", .c (.int .u8), .bit .or (.ref (cid "MASK")) (.ref (cid "HIGH"))⟩,
    ⟨cid "WIDE", .c (.int .u16), .convert .u16 (.ref (cid "BOTH"))⟩] := by decide

/-- The same declarations type, without evaluating. -/
example : (typeConsts U [⟨cid "MASK", .c (.int .u8), .int .u8 0x0f⟩,
    ⟨cid "HIGH", .c (.int .u8), .wrapping .mul (.ref (cid "MASK")) (.int .u8 16)⟩,
    ⟨cid "BOTH", .c (.int .u8), .bit .or (.ref (cid "MASK")) (.ref (cid "HIGH"))⟩,
    ⟨cid "WIDE", .c (.int .u16), .convert .u16 (.ref (cid "BOTH"))⟩] []).isSome := by decide

example : (evalConsts U [⟨cid "MASK", .c (.int .u8), .int .u8 0x0f⟩,
    ⟨cid "HIGH", .c (.int .u8), .wrapping .mul (.ref (cid "MASK")) (.int .u8 16)⟩] []).bind
      (fun env => AList.lookup env (cid "HIGH")) = some (.int .u8 0xf0) := by decide

/-- Refused (rule `ConcreteSpec.Core.ConstsAdmitted`): a constant of a type that is not `Copy`. -/
example : ¬ ConstsAdmitted U [⟨cid "S", .text, .unit⟩] := by decide

/-- Refused: a reference to a constant later in the dependency order (or to itself). -/
example : ¬ ConstsAdmitted U [⟨cid "A", .c (.int .u8), .ref (cid "B")⟩,
    ⟨cid "B", .c (.int .u8), .int .u8 1⟩] := by decide

example : ¬ ConstsAdmitted U [⟨cid "A", .c (.int .u8), .ref (cid "A")⟩] := by decide

/-- Refused: a literal out of its type's range, and operands of different types. -/
example : ¬ ConstsAdmitted U [⟨cid "A", .c (.int .u8), .int .u8 256⟩] := by decide

example : ¬ ConstsAdmitted U [⟨cid "A", .c (.int .u16),
    .wrapping .add (.int .u8 1) (.int .u16 1)⟩] := by decide

/-- Accepted (rule `ConcreteSpec.Core.ConstsAdmitted`): `bool` converts losslessly to an integer,
`true` as `1`. -/
example : (evalConsts U [⟨cid "ON", .c .bool, .bool true⟩,
    ⟨cid "N", .c (.int .i8), .convert .i8 (.ref (cid "ON"))⟩] []).bind
      (fun env => AList.lookup env (cid "N")) = some (.int .i8 1) := by decide

/-- Refused: a lossy conversion, and a declared type other than the value's. -/
example : ¬ ConstsAdmitted U [⟨cid "A", .c (.int .u8), .convert .u8 (.int .i8 (-1))⟩] := by decide

example : ¬ ConstsAdmitted U [⟨cid "A", .c (.int .u16), .int .u8 1⟩] := by decide

/-- Refused: one name declared twice. -/
example : ¬ ConstsAdmitted U [⟨cid "A", .c .bool, .bool true⟩, ⟨cid "A", .c .bool, .bool false⟩] := by
  decide

end ConstDemo

end ConcreteSpec.Core
