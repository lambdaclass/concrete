import ConcreteSpec.Spec.Slice1Ownership

namespace ConcreteSpec.Admission

/-- A violated Core rule. The enclosing refusal identifies the statement or terminator. -/
inductive Reason where
  | literalOutOfRange (type : IntTy) (value : Int)
  | expressionType (expected : Ty) (actual : Option Ty)
  | additionOverflowUnproven
  | unknownBinding (binding : Var)
  | movedBinding (binding : Var)
  | copyOfLinear (binding : Var)
  | moveOfCopy (binding : Var)
  | additionOperandTypes (left right : Option STy)
  | unknownStruct (name : Name)
  | invalidStructFields (structName : Name) (fields : List Name)
  | discardLinear (type : Ty)
  | invalidPattern (structName : Name) (fields : List Name)
  | conflictingPatternBindings (bindings : List Var)
  | duplicateBinding (binding : Var)
  | unusedBinding (binding : Var)
  | liveLinearBinding (binding : Var)
  deriving DecidableEq

/-- A body position identifies one operation in the straight-line Core payload.
The terminator occupies the position immediately after the last statement. -/
inductive Refusal where
  | duplicateStructNames (names : List Name)
  | reservedStructName (name : Name)
  | duplicateFieldNames (structName : Name) (fields : List Name)
  | atPosition (position : Nat) (reason : Reason)
  deriving DecidableEq

/-- Slice 1 has one dischargeable condition: checked addition cannot overflow.
The position selects the expression in the payload, and the context is the
checker state before either operand is read. -/
structure Discharge where
  position : Nat
  type : IntTy
  left : Atom
  right : Atom
  context : Ctx
  deriving DecidableEq

instance : BEq Discharge where
  beq a b := decide (a = b)

instance : LawfulBEq Discharge where
  eq_of_beq := of_decide_eq_true
  rfl := decide_eq_true rfl

end ConcreteSpec.Admission
