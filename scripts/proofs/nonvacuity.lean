import Lean
import ConcreteSpec

/-!
R-0700 checks explicit, kernel-checked witnesses of simultaneous applicability.
It derives the expected proposition from each Law's own dependent telescope.
The witness registry is separate from the denominator, which comes from the
elaborated specification environment.
-/

open Lean Meta

namespace Nonvacuity

private def hasTag (tag doc : String) : Bool :=
  let doc := doc.trimAscii.toString
  doc.startsWith tag && ((doc.drop tag.length).toString.toList.head?).all
    (fun c => c.isWhitespace || ":.(".contains c)

private def category (doc : String) : Option String :=
  ["Law", "Helper", "Representation", "Instance", "Planted"].find? (hasTag · doc)

/-- Existentially bind each argument once, including proof arguments. Later
domains continue to refer to those same arguments. Only the conclusion is
replaced by True. An alias that exposes a further telescope is unresolved:
it may encode a premise-bearing contract that needs an explicit interface. -/
private def jointPremises (type : Expr) : MetaM Expr := do
  forallTelescope type fun args conclusion => do
    let exposed ← withTransparency .all (whnf conclusion)
    if exposed.isForall then
      throwError "unsupported premise telescope hidden behind the conclusion head"
    -- A stuck computation or opaque proposition head could hide another binder.
    -- A known inductive head cannot reduce to a dependent function type.
    let .const head _ := exposed.getAppFn
      | throwError "unsupported conclusion head"
    let some (.inductInfo _) := (← getEnv).find? head
      | throwError "unsupported conclusion head"
    let mut result := mkConst ``True
    for arg in args.reverse do
      result ← mkAppM ``Exists #[← mkLambdaFVars #[arg] result]
    return result

/-- Bind the census to the source population. An unimported module would
otherwise remove its Laws from the denominator. -/
private def checkModuleCoverage (env : Environment) : MetaM Unit := do
  let root ← IO.currentDir
  let source := (#[root / "ConcreteSpec.lean"] ++ (← System.FilePath.walkDir (root / "ConcreteSpec")))
    |>.filter (fun path => path.extension == some "lean")
  let imported := env.header.moduleNames.filterMap fun name =>
    if name.getRoot == `ConcreteSpec then some (modToFilePath root name "lean") else none
  let missing := source.filter (fun path => !imported.contains path)
  let stale := imported.filter (fun path => !source.contains path)
  unless missing.isEmpty && stale.isEmpty do
    throwError "non-vacuity source/import mismatch: omitted modules {missing}; stale modules {stale}"

private def checkWitness (law : TheoremVal) (witness : Name) : MetaM (Option String) := do
  let some (.thmInfo proof) := (← getEnv).find? witness
    | return some "missing witness theorem"
  unless proof.levelParams.length == law.levelParams.length do
    return some "unsupported witness universe parameters"
  let actual := proof.type.instantiateLevelParams proof.levelParams (law.levelParams.map Level.param)
  let expected ← jointPremises law.type
  unless ← withTransparency .all (isDefEq actual expected) do
    return some "witness does not establish the joint premises"
  -- Kernel-only admission for this gate. Pending primitive provenance decisions
  -- from the stopped checker cannot turn an axiom-bearing witness into a pass.
  let axioms ← collectAxioms witness
  unless axioms.isEmpty do
    return some s!"unsupported witness axioms {axioms}"
  return none

private def audit (inScope : Name → Bool) (witnesses : List (Name × Name)) :
    MetaM (Array String × Array String × Nat × Nat) := do
  let env ← getEnv
  let mut registrations : NameMap Name := {}
  let mut errors := #[]
  let mut excluded := #[]
  let mut laws := 0
  let mut covered := 0
  for (law, witness) in witnesses do
    if registrations.contains law then
      errors := errors.push s!"duplicate witness registration: {law}"
    registrations := registrations.insert law witness
  for (name, info) in env.constants do
    if !inScope name then continue
    let some label := category ((← findDocString? env name).getD "")
      | continue
    if label != "Law" then
      excluded := excluded.push s!"excluded {label} {name}"
      continue
    let .thmInfo law := info
      | errors := errors.push s!"Law tag on non-theorem: {name}"; continue
    laws := laws + 1
    let some witness := registrations.find? name
      | errors := errors.push s!"missing joint-premise coverage: {name}"; continue
    registrations := registrations.erase name
    try
      if let some problem ← withoutModifyingState (checkWitness law witness) then
        errors := errors.push s!"{name} ({witness}): {problem}"
      else
        covered := covered + 1
    catch e =>
      errors := errors.push s!"{name} ({witness}): incomplete check: {← e.toMessageData.toString}"
  for (law, _) in registrations.toList do
    errors := errors.push s!"witness registered for a declaration outside the Law inventory: {law}"
  if laws == 0 then errors := errors.push "empty Law inventory"
  return (errors.qsort (· < ·), excluded.qsort (· < ·), covered, laws)

open ConcreteSpec.Core in
private theorem renumberingPremises :
    ∃ (_r : SiteRenumbering) (table : SiteKeyTable) (_ : table.NoDupSites), True :=
  ⟨⟨id, by intro a b h; exact h⟩, [], List.nodup_nil, trivial⟩

-- Keep this concrete witness visible as unsupported until the primitive
-- provenance question is resolved. Its premise types depend on propext.
open ConcreteSpec.Core in
private theorem checkedAdditionPremises :
    ∃ (ty : IntTy) (op : ArithOp) (a b result : Int)
      (_ : ty.InRange a) (_ : checked ty op a b = .ok result), True :=
  ⟨.i8, .add, 7, 8, 15, by decide, rfl, trivial⟩

-- Each entry names a Law and the theorem witnessing its full dependent telescope.
-- Remaining coverage is required before B0b closure (ISS-0104).
private def specWitnesses : List (Name × Name) :=
  [(``ConcreteSpec.Core.SiteKeyTable.nodup_renumber, ``renumberingPremises),
   (``ConcreteSpec.Core.checked_inRange, ``checkedAdditionPremises)]

namespace Control

/-- Law: two compatible conditions on the same dependent argument. -/
theorem realizable (n : Nat) (i : Fin (n + 1)) (h : i.val = n) : i.val ≤ n :=
  Nat.le_of_eq h

theorem together : ∃ (n : Nat) (i : Fin (n + 1)) (_ : i.val = n), True :=
  ⟨0, ⟨0, by decide⟩, rfl, trivial⟩

/-- Law: individually satisfiable premises requiring different values. -/
theorem contradictory (n : Nat) (h₀ : n = 0) (h₁ : n = 1) : False := by
  omega

theorem unconditional : True := trivial

theorem split : (∃ n : Nat, n = 0) ∧ (∃ n : Nat, n = 1) :=
  ⟨⟨0, rfl⟩, ⟨1, rfl⟩⟩

axiom falseWitness : ∃ (n : Nat) (_ : n = 0) (_ : n = 1), True

theorem unsound : ∃ (n : Nat) (_ : n = 0) (_ : n = 1), True := falseWitness

def HiddenContract : Prop := ∀ (_ : False), True

/-- Law: aliased applicability. -/
theorem hidden : HiddenContract := fun _ => trivial

opaque HiddenResult : Prop := False → True
axiom hiddenProof : HiddenResult

/-- Law: no inspectable conclusion head. -/
theorem opaqueContract : HiddenResult := hiddenProof

/-- Helper: excluded. -/
theorem helper : True := trivial

/-- Representation: excluded. -/
theorem representation : True := trivial

/-- Instance: excluded. -/
theorem instanceEvidence : True := trivial

/-- Planted: excluded. -/
theorem planted : True := trivial

end Control

private def controls : MetaM Unit := do
  let positive := [(``Control.realizable, ``Control.together)]
  let positiveScope := fun n => n == ``Control.realizable
  let (errors, _, covered, laws) ← audit positiveScope positive
  unless errors.isEmpty && covered == 1 && laws == 1 do
    throwError "realizable control: {errors}; covered={covered} laws={laws}"
  let (errors, _, _, _) ← audit positiveScope []
  unless errors == #["missing joint-premise coverage: Nonvacuity.Control.realizable"] do
    throwError "omitted law control: {errors}"
  for (law, witness, diagnostic) in [
    (``Control.contradictory, ``Control.split, "witness does not establish the joint premises"),
    (``Control.contradictory, ``Control.unsound, "unsupported witness axioms"),
    (``Control.hidden, ``Control.unconditional, "unsupported premise telescope"),
    (``Control.opaqueContract, ``Control.unconditional, "unsupported conclusion head"),
    (``Control.realizable, `missingWitness, "missing witness theorem")] do
    let (errors, _, _, _) ← audit (· == law) [(law, witness)]
    unless errors.size == 1 && (errors[0]?.getD "").contains diagnostic do
      throwError "negative control {law}: {errors}"
  let excluded := [``Control.helper, ``Control.representation, ``Control.instanceEvidence,
    ``Control.planted]
  let scope := fun n => positiveScope n || excluded.contains n
  let (errors, listed, covered, laws) ← audit scope positive
  unless errors.isEmpty && listed.size == excluded.length && covered == 1 && laws == 1 do
    throwError "Law-only coverage control: {errors}; {listed}"
  for law in excluded do
    let (errors, _, _, _) ← audit scope ((law, ``Control.together) :: positive)
    unless errors == #[s!"witness registered for a declaration outside the Law inventory: {law}"] do
      throwError "excluded registration control {law}: {errors}"
  IO.println "R-0700 controls passed"

#eval show MetaM Unit from do
  match (← IO.getEnv "NONVACUITY_MODE").getD "audit" with
  | "controls" => controls
  | "audit" =>
    controls
    let env ← getEnv
    checkModuleCoverage env
    let (errors, excluded, covered, laws) ← audit (fun n =>
      (env.getModuleIdxFor? n).any (fun i =>
        env.header.moduleNames[i.toNat]?.any (fun m => m.getRoot == `ConcreteSpec))) specWitnesses
    for row in excluded do IO.println row
    for row in errors do IO.println row
    IO.println s!"Law coverage: {covered}/{laws}; excluded tagged declarations: {excluded.size}"
    unless errors.isEmpty do throwError "R-0700 incomplete or invalid Law coverage"
  | mode => throwError "unknown non-vacuity mode: {mode}"

end Nonvacuity
