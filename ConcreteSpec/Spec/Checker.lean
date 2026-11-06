import ConcreteSpec.Spec.Semantics

/-!
# The executable slice-1 checker

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). Part of `ConcreteSpec.Spec`;
see `ConcreteSpec/README.md`.
-/

namespace ConcreteSpec

/-! ### The checker

`check` is an executable checker. It is not the specification: the judgments above are.
`check_iff` (`Proofs/Checker.lean`) proves the two agree, which is what lets the examples below be
decided by running `check`. -/

instance (Γ : Ctx) (x : Var) : Decidable (Fresh Γ x) := by
  unfold Fresh; infer_instance

instance (Γ : Ctx) (pat : List (Name × Binder)) : Decidable (PatFresh Γ pat) := by
  unfold PatFresh; infer_instance

instance (Δ : Decls) (Γ : Ctx) : Decidable (Discharged Δ Γ) := by
  unfold Discharged; infer_instance

instance (Δ : Decls) : Decidable (DeclsOk Δ) := by
  unfold DeclsOk; infer_instance

/-- The scalar type of an atom, if it has one. -/
def atomTy (Γ : TyCtx) : Atom → Option STy
  | .int t n => if t.inRange n then some (.int t) else none
  | .bool _ => some .bool
  | .copy x => match lookupF x Γ with
    | some (.scalar st) => some st
    | _ => none
  | .move x => match lookupF x Γ with
    | some (.scalar st) => some st
    | _ => none

/-- The executable form of `FieldsTy`. -/
def fieldsOkB (Γ : TyCtx) (d : StructDecl) (fs : List (Name × Atom)) : Bool :=
  decide ((fs.map Prod.fst).Nodup) &&
  fs.all (fun p => match lookupF p.1 d.fields with
    | some st => atomTy Γ p.2 == some st
    | none => false) &&
  d.fields.all (fun q => (lookupF q.1 fs).isSome)

/-- The type of an expression, if it has one. -/
def tyOf (Δ : Decls) (Γ : TyCtx) : Expr → Option Ty
  | .atom (.int t n) => if t.inRange n then some (.scalar (.int t)) else none
  | .atom (.bool _) => some (.scalar .bool)
  | .atom (.copy x) => lookupF x Γ
  | .atom (.move x) => lookupF x Γ
  | .add l r => match atomTy Γ l, atomTy Γ r with
    | some (.int t), some (.int t') => if t = t' then some (.scalar (.int t)) else none
    | _, _ => none
  | .mk s fs => match findDecl Δ s with
    | some d => if fieldsOkB Γ d fs then some (.struct s) else none
    | none => none

/-- The context after reading an atom, if the read is allowed. -/
def useAtom (Δ : Decls) (Γ : Ctx) : Atom → Option Ctx
  | .int _ _ => some Γ
  | .bool _ => some Γ
  | .copy x => match ctxFind Γ x with
    | some e => if e.live ∧ e.ty.isCopy Δ = true then some (ctxMark Γ x false) else none
    | none => none
  | .move x => match ctxFind Γ x with
    | some e => if e.live ∧ e.ty.isCopy Δ = false then some (ctxMark Γ x true) else none
    | none => none

/-- The context after reading atoms left to right. -/
def useAtoms (Δ : Decls) (Γ : Ctx) : List Atom → Option Ctx
  | [] => some Γ
  | a :: as => match useAtom Δ Γ a with
    | some Γ₁ => useAtoms Δ Γ₁ as
    | none => none

/-- The context after evaluating an expression, if its reads are allowed. -/
def uses (Δ : Decls) (Γ : Ctx) : Expr → Option Ctx
  | .atom a => useAtom Δ Γ a
  | .add l r => match useAtom Δ Γ l with
    | some Γ₁ => useAtom Δ Γ₁ r
    | none => none
  | .mk _ fs => useAtoms Δ Γ (fs.map Prod.snd)

/-- The executable form of `PatOk`. -/
def patOkB (d : StructDecl) (pat : List (Name × Binder)) : Bool :=
  decide ((pat.map Prod.fst).Nodup) &&
  pat.all (fun p => (lookupF p.1 d.fields).isSome) &&
  d.fields.all (fun q => (lookupF q.1 pat).isSome)

/-- The context after a statement, if the statement is accepted. -/
def stmtOut (Δ : Decls) (Γ : Ctx) : Stmt → Option Ctx
  | .let_ (.name x) t e =>
    if tyOf Δ Γ.tys e = some t ∧ admitted Γ e = true then
      match uses Δ Γ e with
      | some Γ₁ => if Fresh Γ₁ x then
          some ({ name := x, ty := t, live := true, used := false, known := knownExpr Γ e,
                  kfields := knownFields Γ e } :: Γ₁)
          else none
      | none => none
    else none
  | .let_ .wild t e =>
    if tyOf Δ Γ.tys e = some t ∧ admitted Γ e = true ∧ t.isCopy Δ = true then uses Δ Γ e
    else none
  | .letStruct s pat e =>
    if tyOf Δ Γ.tys e = some (.struct s) ∧ admitted Γ e = true then
      match findDecl Δ s with
      | some d => if patOkB d pat then
          match uses Δ Γ e with
          | some Γ₁ => if PatFresh Γ₁ pat then some (bindPat d (knownFields Γ e) pat Γ₁) else none
          | none => none
        else none
      | none => none
    else none

/-- The executable form of `BodyOk`. -/
def bodyOkB (Δ : Decls) (Γ : Ctx) : List Stmt → Atom → Bool
  | [], r => tyOf Δ Γ.tys (.atom r) == some (.scalar (.int .u8)) &&
      match uses Δ Γ (.atom r) with
      | some Γ₁ => decide (Discharged Δ Γ₁)
      | none => false
  | s :: ss, r => match stmtOut Δ Γ s with
    | some Γ₁ => bodyOkB Δ Γ₁ ss r
    | none => false

/-- The executable checker. -/
def check (p : Program) : Bool := decide (DeclsOk p.decls) && bodyOkB p.decls [] p.body p.ret

end ConcreteSpec
