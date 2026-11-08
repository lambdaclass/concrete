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

**Static admission and kernel checking.** `typeConsts` checks the closed grammar, types, `Copy` and
freshness. Independently, `evalConsts` evaluates every declaration, used or unused, and reports the
first failing constant, operation failure, invalid expression or fuel exhaustion. `ConstsAdmitted`
is their conjunction and is decidable: concrete admission examples use `decide`, hence Lean's
kernel reduction, never `native_decide` (item 18). The old fallible-free fragment remains total
(`eval_total`); admitted declarations have valid values of their declared types
(`evalConsts_sound`).

**Source order and elaboration** (owner decision 2026-11-08, item 19). A source initializer may
name a later declaration. `dependsOn` extracts the actual reference relation, including references
nested in operands. `DependencyOrder` is README obligation O1: elaboration supplies a permutation
of the source list in which every referenced declaration occurs earlier. `ElaboratedConsts`
requires either such an order or a cycle refusal whose chain is `c₀ → c₁ → … → c₀`, with every edge
an actual reference. The source elaboration obligation includes existence of an order for acyclic sources and
order-independence of admitted results. These two global proof obligations remain explicit below;
the chain law proves that a reported cycle consists of real references. Core continues to receive only the
ordered form.

The constant types are an interface (`ConstTypes`): the language's type universe, a `Copy`
predicate and an embedding of the scalar types. A concrete language instance proves separately
that this embedding classifies every scalar type as `Copy`; keeping that proof out of the interface
allows admission examples to isolate a non-`Copy` declared type without making the example
ill-typed. Widening `CTy`, `CVal` and `CExpr` to other values remains an owner decision.
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

/-- The language's types as far as constants need them. A concrete instance separately proves that
`ofC` maps every scalar type to a `Copy` type. -/
structure ConstTypes where
  Ty : Type
  deq : DecidableEq Ty
  isCopy : Ty → Bool
  ofC : CTy → Ty
  ofC_inj : ∀ a b, ofC a = ofC b → a = b

attribute [instance] ConstTypes.deq

/-- A constant declaration: `const id: ty = init;`. -/
structure ConstDecl (U : ConstTypes) where
  id : ConstId
  ty : U.Ty
  init : CExpr

/-- The first evaluation refusal, including the identity of the constant being checked. -/
inductive ConstEvalRefusal where
  | operation (id : ConstId) (failure : ConstFailure)
  | outOfFuel (id : ConstId)
  | invalid (id : ConstId) (expression : CExpr)
  deriving DecidableEq, Repr

/-- Evaluate every declaration in Core dependency order, used or unused. Typing and `Copy` are not
rechecked here; they are the separate `typeConsts` judgment. -/
def evalConsts (U : ConstTypes) (fuel : Nat) : List (ConstDecl U) → AList ConstId CVal →
    Except ConstEvalRefusal (AList ConstId CVal)
  | [], env => .ok env
  | d :: ds, env => match d.init.eval env fuel with
    | .value v => evalConsts U fuel ds ((d.id, v) :: env)
    | .failure f => .error (.operation d.id f)
    | .outOfFuel => .error (.outOfFuel d.id)
    | .invalid e => .error (.invalid d.id e)

/-- Type the constants in dependency order, without evaluating. Each initializer is typed with the
types of earlier constants; its declared type must be `Copy` and equal to the initializer type, and
a name may be declared once. -/
def typeConsts (U : ConstTypes) : List (ConstDecl U) → AList ConstId CTy →
    Option (AList ConstId CTy)
  | [], Γ => some Γ
  | d :: ds, Γ => match d.init.ty? Γ with
    | some T =>
      if U.isCopy d.ty = true ∧ U.ofC T = d.ty ∧ AList.lookup Γ d.id = none then
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

/-- **Rule: constant declarations are admitted** by both independent checks. -/
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

/-- README obligation O1: elaboration permutes the source declarations into dependency order. The
`Pairwise` condition says that no declaration occurs before one it references; the separate
self-edge condition refuses self-reference even in a singleton list. References to undeclared
identities and duplicate declarations are independently rejected by static typing. -/
def DependencyOrder (U : ConstTypes) (source order : List (ConstDecl U)) : Prop :=
  order.Perm source ∧
    (∀ a ∈ order, dependsOn U source a.id a.id = false) ∧
    order.Pairwise fun a b => dependsOn U source a.id b.id = false

/-- Elaboration's required result (owner decision 2026-11-08, item 19): a dependency order for Core,
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
  deriving DecidableEq

def U : ConstTypes where
  Ty := T
  deq := inferInstance
  isCopy := fun
    | .c _ => true
    | .text => false
  ofC := .c
  ofC_inj := fun _ _ h => by cases h; rfl

/-- A deliberately non-`Copy` scalar classification used only to isolate the admission check. -/
def NonCopyU : ConstTypes where
  Ty := T
  deq := inferInstance
  isCopy := U.isCopy
  ofC := fun
    | .int t => .c (.int t)
    | .bool => .c .bool
    | .unit => .text
  ofC_inj := fun a b h => by cases a <;> cases b <;> cases h <;> rfl

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

/-- Refused (rule `ConcreteSpec.Core.ConstsAdmitted`): types agree exactly (`unit` maps to `text`),
and only `isCopy text = false` prevents admission. -/
example : (CExpr.unit.ty? [] = some .unit) ∧ NonCopyU.ofC .unit = .text ∧
    NonCopyU.isCopy .text = false ∧
    ¬ ConstsAdmitted NonCopyU 20 [⟨cid "S", .text, .unit⟩] := by
  exact ⟨rfl, rfl, rfl, by decide⟩

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
to `[B, A]` by elaboration; this is item 19's forward reference. -/
example : DependencyOrder U [⟨cid "A", .c (.int .u8),
      .plain .add (.ref (cid "B")) (.int .u8 1)⟩,
    ⟨cid "B", .c (.int .u8), .int .u8 2⟩]
    [⟨cid "B", .c (.int .u8), .int .u8 2⟩,
    ⟨cid "A", .c (.int .u8), .plain .add (.ref (cid "B")) (.int .u8 1)⟩] ∧
    ConstsAdmitted U 20 [⟨cid "B", .c (.int .u8), .int .u8 2⟩,
    ⟨cid "A", .c (.int .u8), .plain .add (.ref (cid "B")) (.int .u8 1)⟩] := by
  refine ⟨⟨List.Perm.swap _ _ [], ?_⟩, by decide⟩
  simp [List.pairwise_cons, dependsOn, CExpr.refs, cid]

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

end ConstDemo

end ConcreteSpec.Core
