import ConcreteSpec.Spec.Slice1Ownership

namespace ConcreteSpec.Admission

def bindTypes (d : StructDecl) : List (Name × Binder) → TyCtx → TyCtx
  | [], types => types
  | (_, .wild) :: rest, types => bindTypes d rest types
  | (field, .name x) :: rest, types =>
    match Core.AList.lookup d.fields field with
    | some ty => bindTypes d rest ((x, .scalar ty) :: types)
    | none => bindTypes d rest types

/-- Type the complete body independently of ownership and arithmetic safety.
An addition with well-typed operands may overflow under this judgment. -/
inductive BodyTyped (Δ : Decls) : TyCtx → List Stmt → Terminator → Prop where
  | ret {types a} : HasType Δ types (.atom a) (.scalar (.int .u8)) → BodyTyped Δ types [] (.ret a)
  | abort {types site} : BodyTyped Δ types [] (.abort site)
  | letName {types e t x rest ending} : HasType Δ types e t → BodyTyped Δ ((x, t) :: types) rest ending →
      BodyTyped Δ types (.let_ (.name x) t e :: rest) ending
  | letWild {types e t rest ending} : HasType Δ types e t → BodyTyped Δ types rest ending →
      BodyTyped Δ types (.let_ .wild t e :: rest) ending
  | letStruct {types e s d pat rest ending} : HasType Δ types e (.struct s) → findDecl Δ s = some d → PatOk d pat →
      BodyTyped Δ (bindTypes d pat types) rest ending →
      BodyTyped Δ types (.letStruct s pat e :: rest) ending

def StaticallyTyped (program : Program) : Prop :=
  DeclsOk program.decls ∧ BodyTyped program.decls [] program.body program.ending

end ConcreteSpec.Admission
