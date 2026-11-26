import ConcreteSpec.Spec.Slice1Semantics
import ConcreteSpec.Admission.Types

/-!
# The executable slice-1 checker

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). Part of `ConcreteSpec.Spec`;
see `ConcreteSpec/README.md`.
-/

namespace ConcreteSpec
open Core (AList)
open Admission (Reason Refusal Discharge)

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
  | .copy x => match AList.lookup Γ x with
    | some (.scalar st) => some st
    | _ => none
  | .move x => match AList.lookup Γ x with
    | some (.scalar st) => some st
    | _ => none

/-- The executable form of `FieldsTy`. -/
def fieldsOkB (Γ : TyCtx) (d : StructDecl) (fs : List (Name × Atom)) : Bool :=
  decide ((fs.map Prod.fst).Nodup) &&
  fs.all (fun p => match AList.lookup d.fields p.1 with
    | some st => atomTy Γ p.2 == some st
    | none => false) &&
  d.fields.all (fun q => (AList.lookup fs q.1).isSome)

/-- The type of an expression, if it has one. -/
def tyOf (Δ : Decls) (Γ : TyCtx) : Expr → Option Ty
  | .atom (.int t n) => if t.inRange n then some (.scalar (.int t)) else none
  | .atom (.bool _) => some (.scalar .bool)
  | .atom (.copy x) => AList.lookup Γ x
  | .atom (.move x) => AList.lookup Γ x
  | .add l r => match atomTy Γ l, atomTy Γ r with
    | some (.int t), some (.int t') => if t = t' then some (.scalar (.int t)) else none
    | _, _ => none
  | .mk s fs => match findDecl Δ s with
    | some d => if fieldsOkB Γ d fs then some (.struct s) else none
    | none => none

/-- Read an atom, distinguishing name, lifetime and read-mode violations. -/
def useAtom (Δ : Decls) (Γ : Ctx) : Atom → Except Reason Ctx
  | .int _ _ | .bool _ => .ok Γ
  | .copy x => match AList.lookup Γ x with
    | some e =>
      if e.live then
        if e.ty.isCopy Δ then .ok (ctxMark Γ x false) else .error (.copyOfLinear x)
      else .error (.movedBinding x)
    | none => .error (.unknownBinding x)
  | .move x => match AList.lookup Γ x with
    | some e =>
      if e.live then
        if e.ty.isCopy Δ then .error (.moveOfCopy x) else .ok (ctxMark Γ x true)
      else .error (.movedBinding x)
    | none => .error (.unknownBinding x)

/-- The executable form of `PatOk`. -/
def patOkB (d : StructDecl) (pat : List (Name × Binder)) : Bool :=
  decide ((pat.map Prod.fst).Nodup) &&
  pat.all (fun p => (AList.lookup d.fields p.1).isSome) &&
  d.fields.all (fun q => (AList.lookup pat q.1).isSome)

private def literalFailure : List Atom → Option Reason
  | .int t n :: tail => if t.inRange n then literalFailure tail else some (.literalOutOfRange t n)
  | _ :: tail => literalFailure tail
  | [] => none

def Expr.atoms : Expr → List Atom
  | .atom a => [a]
  | .add l r => [l, r]
  | .mk _ fs => fs.map Prod.snd

private def typeFailure (Δ : Decls) (types : TyCtx) (expected : Ty) (actual : Option Ty)
    (expr : Expr) : Reason :=
  match actual with
  | some _ => .expressionType expected actual
  | none =>
    match literalFailure expr.atoms with
    | some reason => reason
    | none =>
      match expr with
      | .atom (.copy x) | .atom (.move x) => .unknownBinding x
      | .add l r => .additionOperandTypes (atomTy types l) (atomTy types r)
      | .mk name fields =>
        if (findDecl Δ name).isSome then .invalidStructFields name (fields.map Prod.fst)
        else .unknownStruct name
      | _ => .expressionType expected actual

/-- Read atoms in evaluation order and report the exact rejected read. -/
def readAtoms (Δ : Decls) (Γ : Ctx) : List Atom → Except Reason Ctx
  | [] => .ok Γ
  | a :: as => match useAtom Δ Γ a with
    | .ok Γ₁ => readAtoms Δ Γ₁ as
    | .error reason => .error reason

/-- Check one statement, retaining its output context for the next statement. -/
def checkStmt (Δ : Decls) (Γ : Ctx) : Stmt → Except Reason Ctx
  | .let_ (.name x) t e =>
    let actual := tyOf Δ Γ.tys e
    if actual = some t then
      if admitted Γ e then
        match readAtoms Δ Γ e.atoms with
        | .ok Γ₁ => if Fresh Γ₁ x then
            .ok ((x, { ty := t, live := true, used := false, known := knownExpr Γ e,
                       kfields := knownFields Γ e }) :: Γ₁)
          else .error (.duplicateBinding x)
        | .error reason => .error reason
      else .error .additionOverflowUnproven
    else .error (typeFailure Δ Γ.tys t actual e)
  | .let_ .wild t e =>
    let actual := tyOf Δ Γ.tys e
    if actual = some t then
      if admitted Γ e then
        if t.isCopy Δ then readAtoms Δ Γ e.atoms else .error (.discardLinear t)
      else .error .additionOverflowUnproven
    else .error (typeFailure Δ Γ.tys t actual e)
  | .letStruct s pat e =>
    let actual := tyOf Δ Γ.tys e
    if actual = some (.struct s) then
      if admitted Γ e then
        match findDecl Δ s with
        | some d => if patOkB d pat then
            match readAtoms Δ Γ e.atoms with
            | .ok Γ₁ =>
              if PatFresh Γ₁ pat then .ok (bindPat d (knownFields Γ e) pat Γ₁)
              else .error (.conflictingPatternBindings (patNames pat))
            | .error reason => .error reason
          else .error (.invalidPattern s (pat.map Prod.fst))
        | none => .error (.unknownStruct s)
      else .error .additionOverflowUnproven
    else .error (typeFailure Δ Γ.tys (.struct s) actual e)

/-- Check the obligations that remain at a return or abort. -/
def finishChecks (Δ : Decls) (atReturn : Bool) : Ctx → Except Reason Unit
  | [] => .ok ()
  | (x, entry) :: rest =>
    if entry.used then
      if atReturn && !entry.ty.isCopy Δ && entry.live then .error (.liveLinearBinding x)
      else finishChecks Δ atReturn rest
    else .error (.unusedBinding x)

def checkEnd (Δ : Decls) (Γ : Ctx) : Terminator → Except Reason Unit
  | .abort _ => finishChecks Δ false Γ
  | .ret r =>
    let expected : Ty := .scalar (.int .u8)
    let actual := tyOf Δ Γ.tys (.atom r)
    if actual = some expected then
      match useAtom Δ Γ r with
      | .ok Γ₁ => finishChecks Δ true Γ₁
      | .error reason => .error reason
    else .error (typeFailure Δ Γ.tys expected actual (.atom r))

/-- Successful statement checks establish the typing and safety of this emission.
Only addition has a dischargeable condition in this fragment. -/
def stmtDischarge (position : Nat) (Γ : Ctx) : Stmt → Option Discharge
  | .let_ _ (.scalar (.int t)) (.add l r) => some ⟨position, t, l, r, Γ⟩
  | _ => none

/-- Visit each statement once. Discharges are returned in operation order and
only on whole-body success. -/
def checkCoreBody (Δ : Decls) (Γ : Ctx) (position : Nat) :
    List Stmt → Terminator → Except Refusal (List Discharge)
  | [], ending => match checkEnd Δ Γ ending with
    | .ok () => .ok []
    | .error reason => .error (.atPosition position reason)
  | s :: ss, ending => match checkStmt Δ Γ s with
    | .error reason => .error (.atPosition position reason)
    | .ok Γ₁ => match checkCoreBody Δ Γ₁ (position + 1) ss ending with
      | .error refusal => .error refusal
      | .ok tail => .ok ((stmtDischarge position Γ s).toList ++ tail)

/-- Check declaration-local rules in source order. -/
def checkDeclFields : Decls → Except Refusal Unit
  | [] => .ok ()
  | d :: rest =>
    if d.name = ⟨"main"⟩ then .error (.reservedStructName d.name)
    else if (d.fields.map Prod.fst).Nodup then checkDeclFields rest
    else .error (.duplicateFieldNames d.name (d.fields.map Prod.fst))

def checkDecls (Δ : Decls) : Except Refusal Unit :=
  if (Δ.map StructDecl.name).Nodup then checkDeclFields Δ
  else .error (.duplicateStructNames (Δ.map StructDecl.name))

/-- The sole executable Core checker for the straight-line fragment. -/
def checkProgram (p : Program) : Except Refusal (List Discharge) :=
  match checkDecls p.decls with
  | .ok () => checkCoreBody p.decls [] 0 p.body p.ending
  | .error reason => .error reason

/-- Boolean view of admission for existing spec examples. -/
def bodyOkB (Δ : Decls) (Γ : Ctx) (ss : List Stmt) (r : Terminator) : Bool :=
  (checkCoreBody Δ Γ 0 ss r).isOk

/-- Boolean view of admission for existing callers. -/
def check (p : Program) : Bool := (checkProgram p).isOk

/-- Q8.2, OD62, OD84 and OD138. Core admission checks nominal types, ownership,
binding use, arithmetic evidence and the body's terminator. -/
def ProgramPermitted (p : Program) : Prop := check p = true

end ConcreteSpec
