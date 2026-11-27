import ConcreteSpec.Spec.Slice1Semantics
import ConcreteSpec.Admission.Types

/-!
# The executable slice-1 checker

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). Part of `ConcreteSpec.Spec`;
see `ConcreteSpec/README.md`.
-/

namespace ConcreteSpec
open Core (AList)
open Admission (AtomTypeFailure Reason Refusal Discharge)

/-! ### The checker

`check` is an executable checker. It is not the specification: the judgments above are.
`check_iff` (`Proofs/Checker.lean`) proves the two agree, which is what lets the examples below be
decided by running `check`. -/

instance (Γ : Ctx) (x : Var) : Decidable (Fresh Γ x) := by
  unfold Fresh; infer_instance

instance (Δ : Decls) (Γ : Ctx) : Decidable (Discharged Δ Γ) := by
  unfold Discharged; infer_instance

instance (Δ : Decls) : Decidable (DeclsOk Δ) := by
  unfold DeclsOk; infer_instance

/-- Visit declarations and fields in order, stopping at the first repeat. -/
def firstRepeated? {α : Type} [DecidableEq α] (xs : List α) (seen : List α := []) : Option α :=
  match xs with
  | [] => none
  | x :: rest => if x ∈ seen then some x else firstRepeated? rest (x :: seen)

/-- Convert a detected violation to a refusal with the offending item. -/
def rejectFound {α ε : Type} (found : Option α) (reason : α → ε) : Except ε Unit :=
  match found with
  | none => .ok ()
  | some item => .error (reason item)

/-- Infer an atom's type without erasing the reason it could not be typed. -/
def inferAtomType (Γ : TyCtx) : Atom → Except AtomTypeFailure Ty
  | .int t n => if t.inRange n then .ok (.scalar (.int t)) else .error (.outOfRange t n)
  | .bool _ => .ok (.scalar .bool)
  | .copy x | .move x => match AList.lookup Γ x with
    | some ty => .ok ty
    | none => .error (.unbound x)

def inferAddOperand (Γ : TyCtx) (a : Atom) : Except Reason IntTy :=
  match inferAtomType Γ a with
  | .ok (.scalar (.int t)) => .ok t
  | .ok actual => .error (.additionOperandType a (.wrongType actual))
  | .error cause => .error (.additionOperandType a (.atomFailure cause))

def checkFieldTypes (Γ : TyCtx) (d : StructDecl) : List (Name × Atom) → Except Reason Unit
  | [] => .ok ()
  | (field, atom) :: rest => do
    let some expected := AList.lookup d.fields field | .error (.unknownStructField d.name field)
    let actual ← (inferAtomType Γ atom).mapError (fun cause => .structFieldType d.name field atom expected (.atomFailure cause))
    if actual = .scalar expected then checkFieldTypes Γ d rest
    else .error (.structFieldType d.name field atom expected (.wrongType actual))

/-- The executable form of `FieldsTy`, with the rejected initializer or shape rule. -/
def checkFields (Γ : TyCtx) (d : StructDecl) (fs : List (Name × Atom)) : Except Reason Unit := do
  rejectFound (firstRepeated? (fs.map Prod.fst)) (.duplicateStructInitializer d.name)
  checkFieldTypes Γ d fs
  rejectFound (d.fields.find? (fun q => (AList.lookup fs q.1).isNone))
    (fun q => .missingStructField d.name q.1)

def inferExprType (Δ : Decls) (Γ : TyCtx) : Expr → Except Reason Ty
  | .atom a => (inferAtomType Γ a).mapError fun
    | .unbound x => .unknownBinding x
    | .outOfRange t n => .literalOutOfRange t n
  | .add l r => do
    let lt ← inferAddOperand Γ l
    let rt ← inferAddOperand Γ r
    if lt = rt then .ok (.scalar (.int lt)) else .error (.additionTypeMismatch l r lt rt)
  | .mk s fs => do
    let some d := findDecl Δ s | .error (.unknownStruct s)
    checkFields Γ d fs
    .ok (.struct s)

def requireExprType (Δ : Decls) (Γ : TyCtx) (expected : Ty) (e : Expr) : Except Reason Unit := do
  let actual ← inferExprType Δ Γ e
  if actual = expected then .ok () else .error (.expressionType expected actual)

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

/-- Check each of the three field obligations of `PatOk` in order. -/
def checkPattern (d : StructDecl) (pat : List (Name × Binder)) : Except Reason Unit := do
  rejectFound (firstRepeated? (pat.map Prod.fst)) (.duplicatePatternField d.name)
  rejectFound (pat.find? (fun p => (AList.lookup d.fields p.1).isNone))
    (fun p => .unknownPatternField d.name p.1)
  rejectFound (d.fields.find? (fun q => (AList.lookup pat q.1).isNone))
    (fun q => .missingPatternField d.name q.1)

/-- Check uniqueness within the pattern, then freshness against the existing context. -/
def checkPatternBinders (Γ : Ctx) (pat : List (Name × Binder)) : Except Reason Unit := do
  let names := patNames pat
  rejectFound (firstRepeated? names) .repeatedPatternBinder
  rejectFound (names.find? (fun x => (AList.lookup Γ x).isSome)) .patternBinderAlreadyBound

def Expr.atoms : Expr → List Atom
  | .atom a => [a]
  | .add l r => [l, r]
  | .mk _ fs => fs.map Prod.snd

/-- Read atoms in evaluation order and report the exact rejected read. -/
def readAtoms (Δ : Decls) (Γ : Ctx) : List Atom → Except Reason Ctx
  | [] => .ok Γ
  | a :: as => match useAtom Δ Γ a with
    | .ok Γ₁ => readAtoms Δ Γ₁ as
    | .error reason => .error reason

/-- Check typing and reads before testing arithmetic evidence in the pre-read context. -/
def checkStmtExprWith (evidence : Ctx → Expr → Bool) (Δ : Decls) (Γ : Ctx) (t : Ty) (e : Expr) : Except Reason Ctx := do
  requireExprType Δ Γ.tys t e
  let next ← readAtoms Δ Γ e.atoms
  if evidence Γ e then .ok next
  else .error .additionOverflowUnproven

abbrev checkStmtExpr := checkStmtExprWith admitted

/-- Check one statement, retaining its output context for the next statement. -/
def checkStmtWith (evidence : Ctx → Expr → Bool) (Δ : Decls) (Γ : Ctx) : Stmt → Except Reason Ctx
  | .let_ (.name x) t e => do
    let next ← checkStmtExprWith evidence Δ Γ t e
    if Fresh next x then
      .ok ((x, { ty := t, live := true, used := false, known := knownExpr Γ e,
                 kfields := knownFields Γ e }) :: next)
    else .error (.duplicateBinding x)
  | .let_ .wild t e => do
    let next ← checkStmtExprWith evidence Δ Γ t e
    if t.isCopy Δ then .ok next else .error (.discardLinear t)
  | .letStruct s pat e => do
    let next ← checkStmtExprWith evidence Δ Γ (.struct s) e
    let some d := findDecl Δ s | .error (.unknownStruct s)
    checkPattern d pat
    checkPatternBinders next pat
    .ok (bindPat d (knownFields Γ e) pat next)

abbrev checkStmt := checkStmtWith admitted

/-- Check the obligations that remain at a return or abort. -/
def finishChecks (Δ : Decls) (atReturn : Bool) : Ctx → Except Reason Unit
  | [] => .ok ()
  | (x, entry) :: rest =>
    if atReturn && !entry.ty.isCopy Δ && entry.live then .error (.liveLinearBinding x)
    else if entry.used then finishChecks Δ atReturn rest
    else .error (.unusedBinding x)

def checkEnd (Δ : Decls) (Γ : Ctx) : Terminator → Except Reason Unit
  | .abort _ => finishChecks Δ false Γ
  | .ret r => do
    requireExprType Δ Γ.tys (.scalar (.int .u8)) (.atom r)
    let next ← useAtom Δ Γ r
    finishChecks Δ true next

/-- Successful statement checks establish the typing and safety of this emission.
Only addition has a dischargeable condition in this fragment. -/
def stmtDischarge (position : Nat) (Γ : Ctx) : Stmt → Option Discharge
  | .let_ _ (.scalar (.int t)) (.add l r) => some ⟨position, t, l, r, Γ⟩
  | _ => none

/-- A runtime check issues no evidence that could remove itself. -/
def ArithmeticPolicy.discharge : ArithmeticPolicy → Nat → Ctx → Stmt → Option Discharge
  | .static => stmtDischarge
  | .runtime => fun _ _ _ => none

/-- Visit each statement once. Discharges are returned in operation order and
only on whole-body success. -/
def checkCoreBodyWith (policy : ArithmeticPolicy) (Δ : Decls) (Γ : Ctx) (position : Nat) :
    List Stmt → Terminator → Except Refusal (List Discharge)
  | [], ending => match checkEnd Δ Γ ending with
    | .ok () => .ok []
    | .error reason => .error (.atPosition position reason)
  | s :: ss, ending => match checkStmtWith policy.permits Δ Γ s with
    | .error reason => .error (.atPosition position reason)
    | .ok Γ₁ => match checkCoreBodyWith policy Δ Γ₁ (position + 1) ss ending with
      | .error refusal => .error refusal
      | .ok tail => .ok ((policy.discharge position Γ s).toList ++ tail)

abbrev checkCoreBody := checkCoreBodyWith .static

/-- The index selects the offending occurrence in the located declaration table. -/
def checkDeclsFrom (seen : List Name) (index : Nat) : Decls → Except Refusal Unit
  | [] => .ok ()
  | d :: rest =>
    if d.name ∈ seen then .error (.duplicateStructName index d.name)
    else if d.name = ⟨"main"⟩ then .error (.reservedStructName index d.name)
    else do
      rejectFound (firstRepeated? (d.fields.map Prod.fst)) (.duplicateDeclaredField index d.name)
      checkDeclsFrom (d.name :: seen) (index + 1) rest

def checkDecls := checkDeclsFrom [] 0

/-- The sole executable Core checker for the straight-line fragment. -/
def checkProgramWith (policy : ArithmeticPolicy) (p : Program) : Except Refusal (List Discharge) :=
  match checkDecls p.decls with
  | .ok () => checkCoreBodyWith policy p.decls [] 0 p.body p.ending
  | .error reason => .error reason

abbrev checkProgram := checkProgramWith .static

/-- Boolean view of admission for existing spec examples. -/
def bodyOkB (Δ : Decls) (Γ : Ctx) (ss : List Stmt) (r : Terminator) : Bool :=
  (checkCoreBody Δ Γ 0 ss r).isOk

/-- Boolean view of admission for existing callers. -/
def check (p : Program) : Bool := (checkProgram p).isOk

/-- Q8.2, OD62, OD84 and OD138. Core admission checks nominal types, ownership,
binding use, arithmetic evidence and the body's terminator. -/
def ProgramPermitted (p : Program) : Prop := check p = true

end ConcreteSpec
