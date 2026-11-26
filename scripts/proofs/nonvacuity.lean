import Lean
import ConcreteSpec

/-!
R-0700 checks explicit, kernel-checked witnesses of simultaneous applicability.
It derives the expected proposition from each Law's own dependent telescope.
The witness registry is separate from the denominator, which comes from the
elaborated specification environment.
-/

open Lean Meta
set_option autoImplicit false

namespace Nonvacuity

private def hasTag (tag doc : String) : Bool :=
  let doc := doc.trimAscii.toString
  doc.startsWith tag && ((doc.drop tag.length).toString.toList.head?).all
    (fun c => c.isWhitespace || ":.(".contains c)

private def category (doc : String) : Option String :=
  ["Law", "Helper", "Representation", "Instance", "Planted"].find? (hasTag · doc)

private def combinePremises (left right : Expr) : Expr :=
  if left.isConstOf ``True then right
  else if right.isConstOf ``True then left
  else mkApp2 (mkConst ``And) left right

/-- Read the positive contract before substituting any witness arguments. Unfold
aliases one layer at a time: weak-head reduction can select a concrete match
branch and erase the very obligation we need to witness. -/
private partial def jointPremises (type : Expr) : MetaM Expr := withIncRecDepth do
  let type := type.consumeMData.headBeta
  if type.isForall then
    return ← forallTelescope type fun args conclusion => do
      let mut result ← jointPremises conclusion
      for arg in args.reverse do
        result ← mkAppM ``Exists #[← mkLambdaFVars #[arg] result]
      return result
  if let .letE _ _ value body _ := type then
    return ← jointPremises (body.instantiate1 value)
  let .const head levels := type.getAppFn
    | throwError "unsupported conclusion head: {type}"
  let args := type.getAppArgs
  if head == ``And && args.size == 2 then
    let left ← jointPremises args[0]!
    let right ← jointPremises args[1]!
    return combinePremises left right
  if head == ``Or && args.size == 2 then
    let left ← jointPremises args[0]!
    let right ← jointPremises args[1]!
    unless left.isConstOf ``True && right.isConstOf ``True do
      throwError "unsupported conclusion head: disjunction with branch premises"
    return mkConst ``True
  if head == ``Exists && args.size == 2 then
    return ← withLocalDeclD `witness args[0]! fun witness => do
      let body ← jointPremises (mkApp args[1]! witness)
      if body.isConstOf ``True then return body
      return ← mkAppM ``Exists #[← mkLambdaFVars #[witness] body]
  if head == ``Iff && args.size == 2 then
    -- Both directions contribute an antecedent. Retain any premises that
    -- the antecedents themselves contain in positive positions.
    let left ← jointPremises args[0]!
    let right ← jointPremises args[1]!
    return combinePremises (mkApp2 (mkConst ``And) args[0]! args[1]!)
      (combinePremises left right)
  -- Negative conclusions supply no positive applicability assumptions.
  if [``True, ``False, ``Eq, ``HEq, ``Not, ``Ne, ``Nat.le, ``Nat.lt,
      ``Int.le, ``Int.lt].contains head then
    return mkConst ``True
  if [``LE.le, ``LT.lt].contains head && args.size == 4 &&
      (args[0]!.isConstOf ``Nat || args[0]!.isConstOf ``Int) then
    let standard := if args[0]!.isConstOf ``Nat then
      if head == ``LE.le then ``instLENat else ``instLTNat
    else
      if head == ``LE.le then ``Int.instLEInt else ``Int.instLTInt
    if ← isDefEq args[1]! (mkConst standard) then return mkConst ``True
  if head == ``List.Pairwise && args.size == 3 then
    -- A standard list predicate can still be instantiated with a relation
    -- that hides a telescope. Check the relation under its own binders.
    let relation ← withLocalDeclD `first args[0]! fun first =>
      withLocalDeclD `second args[0]! fun second =>
        jointPremises (mkApp2 args[1]! first second)
    if relation.isConstOf ``True then return relation
    throwError "unsupported premises in pairwise relation"
  let env ← getEnv
  if ((env.find? head).any fun info => match info with
      | .recInfo _ => true
      | _ => false) || isMatcherLikeCore env head || [``ite, ``dite].contains head then
    throwError "unsupported conditional conclusion: {head}"
  if let some (.defnInfo info) := env.find? head then
    let value := info.value.instantiateLevelParams info.levelParams levels
    return ← jointPremises (mkAppN value args)
  -- Other connectives and inductive predicates need a defined applicability
  -- interface. Their arguments or constructors can hide arbitrary propositions.
  throwError "unsupported conclusion head: {head}"

/-- Bind the census to the source population. An unimported module would
otherwise remove its Laws from the denominator. -/
private def checkModuleCoverage (env : Environment) (root : System.FilePath) : MetaM Unit := do
  let source := (#[root / "ConcreteSpec.lean"] ++ (← System.FilePath.walkDir (root / "ConcreteSpec")))
    |>.filter (fun path => path.extension == some "lean")
  let imported := env.header.moduleNames.filterMap fun name =>
    if name.getRoot == `ConcreteSpec then some (modToFilePath root name "lean") else none
  let missing := source.filter (fun path => !imported.contains path)
  let stale := imported.filter (fun path => !source.contains path)
  unless missing.isEmpty && stale.isEmpty do
    throwError "non-vacuity source/import mismatch: omitted modules {missing}; stale modules {stale}"

private def checkWitness (law : TheoremVal) (expected : Expr) (witness : Name) :
    MetaM (Option String) := do
  let some (.thmInfo proof) := (← getEnv).find? witness
    | return some "missing witness theorem"
  unless proof.levelParams.length == law.levelParams.length do
    return some "unsupported witness universe parameters"
  let actual := proof.type.instantiateLevelParams proof.levelParams (law.levelParams.map Level.param)
  match Kernel.isDefEq (← getEnv) {} actual expected with
  | .error _ => throwError "kernel could not compare the witness type with the joint premises"
  | .ok false => return some "witness does not establish the joint premises"
  | .ok true => pure ()
  -- Kernel-only admission for this gate. Pending primitive provenance decisions
  -- from the stopped checker cannot turn an axiom-bearing witness into a pass.
  let axioms ← collectAxioms witness
  unless axioms.isEmpty do
    let mut typeAxioms : NameSet := {}
    for constant in actual.getUsedConstants do
      for axiomName in (← collectAxioms constant) do
        typeAxioms := typeAxioms.insert axiomName
    let additional := axioms.filter (!typeAxioms.contains ·)
    return some (s!"unsupported witness axioms {axioms}; " ++
      s!"type dependencies {typeAxioms.toArray.qsort Name.lt}; additional dependencies {additional}")
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
    if !inScope name || (privateToUserName name).isInternal then continue
    let some label := category ((← findDocString? env name).getD "")
      | continue
    if label != "Law" then
      excluded := excluded.push s!"excluded {label} {name}"
      continue
    let .thmInfo law := info
      | errors := errors.push s!"Law tag on non-theorem: {name}"; continue
    laws := laws + 1
    let witness := registrations.find? name
    registrations := registrations.erase name
    if witness.isNone then
      errors := errors.push s!"missing joint-premise coverage: {name}"
    try
      let problem ← withoutModifyingState do
        let expected ← jointPremises law.type
        match witness with
        | some proof => checkWitness law expected proof
        | none => pure none
      if let some message := problem then
        errors := errors.push s!"{name}: {message}"
      else if witness.isSome then
        covered := covered + 1
    catch e =>
      errors := errors.push s!"{name}: incomplete check: {← e.toMessageData.toString}"
  for (law, _) in registrations.toList do
    errors := errors.push s!"witness registered for a declaration outside the Law inventory: {law}"
  if laws == 0 then errors := errors.push "empty Law inventory"
  return (errors.qsort (· < ·), excluded.qsort (· < ·), covered, laws)

namespace SpecWitness
open ConcreteSpec.Core
open ConcreteSpec (Var)

theorem renumbering :
    ∃ (_r : SiteRenumbering) (table : SiteKeyTable) (_ : table.NoDupSites), True :=
  ⟨Vacuity.successor, Vacuity.siteTable, by decide, trivial⟩

theorem renumberingLookup : ∃ (_ : SiteRenumbering) (_ : SiteKeyTable) (_ : SiteId), True :=
  ⟨Vacuity.successor, Vacuity.siteTable, ⟨1⟩, trivial⟩

private def sourcePrefix : SourceText :=
  ⟨[195], fun _ member => by
    cases member with
    | head => decide
    | tail _ rest => cases rest⟩

theorem source : ∃ _ : SourceText, True :=
  ⟨sourcePrefix, trivial⟩

theorem sourceByte : ∃ (_ : SourceText) (_ : UInt8), True :=
  ⟨sourcePrefix, 169, trivial⟩

theorem sourceNonNewline : ∃ (_ : SourceText) (byte : UInt8) (_ : byte ≠ 10), True :=
  ⟨sourcePrefix, 9, by decide, trivial⟩

theorem knownPremise : ∃ (record : PremiseRecord) (_ : record.spec = specVersion), True :=
  ⟨Vacuity.stackRecord, rfl, trivial⟩

theorem premiseReadIff :
    ∃ (reader : SpecVersion) (record : EncodedPremise) (decoded : PremiseRecord),
      record.read reader = some decoded ∧
        (record.spec = reader ∧ reader = specVersion ∧
          ∃ kind : PremiseKind, kind.encode = record.category ∧
            decoded = ⟨kind, record.scope, record.spec⟩) :=
  ⟨specVersion, Vacuity.stackRecord.encode, Vacuity.stackRecord,
    rfl, rfl, rfl, .stack, rfl, rfl⟩

theorem unknownPremise :
    ∃ (_ : SpecVersion) (record : EncodedPremise)
      (_ : ∀ kind : PremiseKind, kind.encode ≠ record.category), True :=
  ⟨specVersion, Vacuity.unknownRecord, by intro kind; cases kind <;> decide, trivial⟩

theorem integer : ∃ (_ : IntTy) (_ : Int), True :=
  ⟨.i8, 0, trivial⟩

theorem bitsInWidth : ∃ (ty : IntTy) (bits : Nat) (_ : bits < 2 ^ ty.bits), True :=
  ⟨.i8, 42, by decide, trivial⟩

theorem inRange : ∃ (ty : IntTy) (n : Int) (_ : ty.InRange n), True :=
  ⟨.i8, 7, by decide, trivial⟩

theorem trapKindInjection :
    ∃ (a b : ArithmeticError) (_ : a.trapKind = b.trapKind), True :=
  ⟨.overflow, .overflow, rfl, trivial⟩

theorem trapNameInjection :
    ∃ (a b : TrapKind) (_ : a.name = b.name), True :=
  ⟨.overflow, .overflow, rfl, trivial⟩

theorem bitwise : ∃ (_ : IntTy) (_ : BitOp) (_ : Int) (_ : Int), True :=
  ⟨.i8, .and, 7, 3, trivial⟩

theorem wrappingBits : ∃ (_ : IntTy) (_ : WrapOp) (_ : Int) (_ : Int), True :=
  ⟨.i8, .add, 127, 1, trivial⟩

theorem checkedWrapping :
    ∃ (ty : IntTy) (op : WrapOp) (a b result : Int)
      (_ : checked ty op.arith a b = .ok result), True :=
  ⟨.i8, .add, 7, 8, 15, rfl, trivial⟩

theorem checkedShift :
    ∃ (ty : IntTy) (a count result : Int) (_ : checked ty .shl a count = .ok result), True :=
  ⟨.i8, 7, 2, 28, rfl, trivial⟩

theorem belowRange : ∃ (ty : IntTy) (n : Int) (_ : n < ty.min), True :=
  ⟨.i8, -129, by decide, trivial⟩

theorem aboveRange : ∃ (ty : IntTy) (n : Int) (_ : ty.max < n), True :=
  ⟨.i8, 128, by decide, trivial⟩

theorem plainExact :
    ∃ (ρ : Var → Option Int) (ty : IntTy) (op : WrapOp) (a b : CTerm) (r : Int)
      (_ : (CTerm.machine ty (.plain op.arith) a b).eval ρ = some r), True :=
  ⟨fun _ => none, .i8, .add, .lit 7, .lit 8, 15, rfl, trivial⟩

theorem evalToExact :
    ∃ (ρ : Var → Option Int) (e : CTerm) (r : Int) (_ : e.eval ρ = some r), True :=
  ⟨fun _ => none, .machine .i8 (.plain .add) (.lit 7) (.lit 8), 15, rfl, trivial⟩

-- The audit reports any primitive dependencies while the provenance policy is open.
theorem checkedAddition :
    ∃ (ty : IntTy) (op : ArithOp) (a b result : Int)
      (_ : ty.InRange a) (_ : checked ty op a b = .ok result), True :=
  ⟨.i8, .add, 7, 8, 15, by decide, rfl, trivial⟩

private theorem emptyWidthEnv : WidthEnv (fun _ => none) (fun _ => none) :=
  fun _ _ _ h _ => by cases h

theorem typedEvaluation :
    ∃ (Γ : Var → Option IntTy) (ρ : Var → Option Int) (e : CTerm) (ty : IntTy)
      (_ : e.HasWidth Γ ty) (_ : WidthEnv Γ ρ)
      (r : Int) (_ : e.eval ρ = some r), True :=
  ⟨fun _ => none, fun _ => none, .lit 7, .i8, .lit (by decide),
    emptyWidthEnv, 7, rfl, trivial⟩

theorem machineOperands :
    ∃ (Γ : Var → Option IntTy) (ρ : Var → Option Int) (t u : IntTy)
      (op : MachineOp) (a b : CTerm) (_ : a.HasWidth Γ t) (_ : b.HasWidth Γ u)
      (_ : op.countOperand = true ∨ u = t) (_ : WidthEnv Γ ρ)
      (x y : Int) (_ : a.eval ρ = some x) (_ : b.eval ρ = some y), True :=
  ⟨fun _ => none, fun _ => none, .i8, .i8, .plain .add, .lit 7, .lit 8,
    .lit (by decide), .lit (by decide), .inr rfl, emptyWidthEnv, 7, 8, rfl, rfl, trivial⟩

theorem machineRange :
    ∃ (Γ : Var → Option IntTy) (ρ : Var → Option Int) (t u : IntTy)
      (op : MachineOp) (a b : CTerm) (_ : a.HasWidth Γ t) (_ : b.HasWidth Γ u)
      (_ : op.countOperand = true ∨ u = t) (_ : WidthEnv Γ ρ)
      (r : Int) (_ : (CTerm.machine t op a b).eval ρ = some r), True :=
  ⟨fun _ => none, fun _ => none, .i8, .i8, .plain .add, .lit 7, .lit 8,
    .lit (by decide), .lit (by decide), .inr rfl, emptyWidthEnv, 15, rfl, trivial⟩

private def cycleDecls : List (ConstDecl ConstDemo.U) :=
  [⟨ConstDemo.cid "A", .c .bool, .ref (ConstDemo.cid "B")⟩,
   ⟨ConstDemo.cid "B", .c .bool, .ref (ConstDemo.cid "A")⟩]

theorem dependencyCycle :
    ∃ (U : ConstTypes) (decls : List (ConstDecl U)) (chain : List ConstId)
      (_ : List (ConstDecl U)) (_ : CycleChain U decls chain), True :=
  ⟨ConstDemo.U, cycleDecls, [ConstDemo.cid "A", ConstDemo.cid "B", ConstDemo.cid "A"], cycleDecls,
    ⟨ConstDemo.cid "A", [ConstDemo.cid "B"], rfl, by decide⟩, trivial⟩

theorem codePoint : ∃ _ : UInt32, True := ⟨0xd800, trivial⟩

theorem character : ∃ _ : Char, True := ⟨'A', trivial⟩

theorem asciiByte : ∃ _ : UInt8, True := ⟨128, trivial⟩

private def chainEnv (decls : List (ConstDecl ConstDemo.U)) : AList ConstId CVal :=
  decls.reverse.map fun decl => (decl.id, .bool (decl.id == ConstDemo.cid "B"))

theorem constantOrder :
    ∃ (U : ConstTypes) (fuel : Nat) (source first second : List (ConstDecl U))
      (_ : DependencyOrder U source first) (_ : DependencyOrder U source second),
      (ConstsTyped U first ∧ ConstsTyped U second) ∧
      (ConstsEvaluate U fuel first ∧ ConstsEvaluate U fuel second) ∧
      (ConstsAdmitted U fuel first ∧ ConstsAdmitted U fuel second) ∧
      ∃ (env₁ env₂ : AList ConstId CVal) (_ : evalConsts U fuel first [] = .ok env₁)
        (_ : evalConsts U fuel second [] = .ok env₂) (_ : ConstId), True := by
  refine ⟨ConstDemo.U, 3, chainSource, adjacentOrder, separatedOrder, by decide, by decide,
    ⟨by decide, by decide⟩, ⟨by decide, by decide⟩, ⟨by decide, by decide⟩,
    chainEnv adjacentOrder, chainEnv separatedOrder, ?_, ?_, ConstDemo.cid "A", trivial⟩
  all_goals decide

theorem constantOrderOuterOnly :
    ∃ (U : ConstTypes) (_fuel : Nat) (source first second : List (ConstDecl U))
      (_ : DependencyOrder U source first) (_ : DependencyOrder U source second), True :=
  ⟨ConstDemo.U, 3, chainSource, adjacentOrder, separatedOrder, by decide, by decide, trivial⟩

open ConcreteSpec.Rep in
theorem representationRoundtrip :
    ∃ (decls : ConcreteSpec.Decls) (value : ConcreteSpec.Val) (ty : ConcreteSpec.Ty)
      (_ : RepVal decls value ty), True :=
  ⟨[], .scalar (.bool true), .scalar .bool, .scalar .bool, trivial⟩

section Accounting
open ConcreteSpec.Core.Leak

private def entryMemory : Memory := fun _ => reportEntry

private theorem reportEntryWF : reportEntry.WF := ⟨by decide, by decide⟩

private theorem entryMemoryWF : ∀ l, (entryMemory l).WF := fun _ => reportEntryWF

theorem accountWF : ∃ (account : Account) (_ : account.WF), True :=
  ⟨reportEntry, reportEntryWF, trivial⟩

theorem allocationZero : ∃ (m : Memory) (_ : ∀ l, (m l).WF) (_ : LineageId), True :=
  ⟨entryMemory, entryMemoryWF, ⟨0⟩, trivial⟩

theorem freeWithinLive :
    ∃ (m : Memory) (l : LineageId) (bytes : Nat) (_ : bytes ≤ (m l).live)
      (_ : Memory) (other : LineageId) (_ : other ≠ l), True :=
  ⟨entryMemory, ⟨0⟩, 1, by decide, Memory.set entryMemory ⟨0⟩ (reportEntry.free 1),
    ⟨1⟩, by decide, trivial⟩

end Accounting

end SpecWitness

-- The Law census is independent of this registry. Coverage still blocks B0b (ISS-0104).
private def specWitnesses : List (Name × Name) :=
  [(``ConcreteSpec.Core.SiteKeyTable.nodup_renumber, ``SpecWitness.renumbering),
   (``ConcreteSpec.Core.SiteKeyTable.lookup_renumber, ``SpecWitness.renumberingLookup),
   (``ConcreteSpec.Core.SourceText.lineAt_zero, ``SpecWitness.source),
   (``ConcreteSpec.Core.SourceText.columnAt_zero, ``SpecWitness.source),
   (``ConcreteSpec.Core.SourceText.lineAt_append_newline, ``SpecWitness.source),
   (``ConcreteSpec.Core.SourceText.columnAt_append_newline, ``SpecWitness.source),
   (``ConcreteSpec.Core.SourceText.columnAt_append_tab, ``SpecWitness.source),
   (``ConcreteSpec.Core.SourceText.lineAt_append, ``SpecWitness.sourceByte),
   (``ConcreteSpec.Core.SourceText.columnAt_append, ``SpecWitness.sourceByte),
   (``ConcreteSpec.Core.SourceText.lineAt_append_other, ``SpecWitness.sourceNonNewline),
   (``ConcreteSpec.Core.EncodedPremise.read_encode, ``SpecWitness.knownPremise),
   (``ConcreteSpec.Core.EncodedPremise.read_iff, ``SpecWitness.premiseReadIff),
   (``ConcreteSpec.Core.EncodedPremise.read_unknown, ``SpecWitness.unknownPremise),
   (``ConcreteSpec.Core.checked_inRange, ``SpecWitness.checkedAddition),
   (``ConcreteSpec.Core.toBits_lt, ``SpecWitness.integer),
   (``ConcreteSpec.Core.wrap_inRange, ``SpecWitness.integer),
   (``ConcreteSpec.Core.saturate_inRange, ``SpecWitness.integer),
   (``ConcreteSpec.Core.ofBits_inRange, ``SpecWitness.bitsInWidth),
   (``ConcreteSpec.Core.ofBits_toBits, ``SpecWitness.inRange),
   (``ConcreteSpec.Core.wrap_of_inRange, ``SpecWitness.inRange),
   (``ConcreteSpec.Core.saturate_of_inRange, ``SpecWitness.inRange),
   (``ConcreteSpec.Core.bitwise_inRange, ``SpecWitness.bitwise),
   (``ConcreteSpec.Core.toBits_bitwise, ``SpecWitness.bitwise),
   (``ConcreteSpec.Core.toBits_bitNot, ``SpecWitness.integer),
   (``ConcreteSpec.Core.toBits_wrap, ``SpecWitness.integer),
   (``ConcreteSpec.Core.toBits_ofBits, ``SpecWitness.bitsInWidth),
   (``ConcreteSpec.Core.toBits_wrapping, ``SpecWitness.wrappingBits),
   (``ConcreteSpec.Core.toBits_shl, ``SpecWitness.checkedShift),
   (``ConcreteSpec.Core.wrapping_of_checked, ``SpecWitness.checkedWrapping),
   (``ConcreteSpec.Core.saturating_of_checked, ``SpecWitness.checkedWrapping),
   (``ConcreteSpec.Core.saturate_below, ``SpecWitness.belowRange),
   (``ConcreteSpec.Core.saturate_above, ``SpecWitness.aboveRange),
   (``ConcreteSpec.Core.machine_plain_eq_exact, ``SpecWitness.plainExact),
   (``ConcreteSpec.Core.machine_operands, ``SpecWitness.machineOperands),
   (``ConcreteSpec.Core.machine_inRange, ``SpecWitness.machineRange),
   (``ConcreteSpec.Core.eval_toExact, ``SpecWitness.evalToExact),
   (``ConcreteSpec.Core.TrapKind.name_injective, ``SpecWitness.trapNameInjection),
   (``ConcreteSpec.Core.ArithmeticError.trapKind_injective, ``SpecWitness.trapKindInjection),
   (``ConcreteSpec.Core.typed_eval_range, ``SpecWitness.typedEvaluation),
   (``ConcreteSpec.Core.charFromCodePoint_map, ``SpecWitness.codePoint),
   (``ConcreteSpec.Core.charFromCodePoint_roundtrip, ``SpecWitness.character),
   (``ConcreteSpec.Core.charFromAscii_map, ``SpecWitness.asciiByte),
   (``ConcreteSpec.Rep.rep_roundtrip, ``SpecWitness.representationRoundtrip),
   (``ConcreteSpec.Core.Leak.Account.alloc_zero, ``SpecWitness.accountWF),
   (``ConcreteSpec.Core.Leak.apply_alloc_zero, ``SpecWitness.allocationZero),
   (``ConcreteSpec.Core.Leak.free_conservation, ``SpecWitness.freeWithinLive),
   (``ConcreteSpec.Core.ConstOrderIndependent, ``SpecWitness.constantOrder),
   (``ConcreteSpec.Core.cycle_excludes_dependencyOrder, ``SpecWitness.dependencyCycle)]

private def auditSpecification (root : System.FilePath) :
    MetaM (Array String × Array String × Nat × Nat) := do
  let env ← getEnv
  checkModuleCoverage env root
  audit (fun n =>
    (env.getModuleIdxFor? n).any (fun i =>
      env.header.moduleNames[i.toNat]?.any (fun m => m.getRoot == `ConcreteSpec))) specWitnesses

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

/-- Law: a contradictory inner telescope under a conjunction. -/
theorem nestedAnd : (∀ n : Nat, n = 0 → n = 1 → False) ∧ True :=
  ⟨contradictory, trivial⟩

/-- Law: a contradictory inner telescope under a disjunction. -/
theorem nestedOr (n : Nat) : (n = 0 → n = 1 → False) ∨ False :=
  .inl (contradictory n)

structure Wrapped : Prop where
  proof : False → True

/-- Law: a contradictory inner telescope in a proposition structure. -/
theorem nestedStructure : Wrapped := ⟨fun _ => trivial⟩

theorem someNat : ∃ _ : Nat, True := ⟨0, trivial⟩

/-- Law: a contradictory inner telescope under an existential. -/
theorem nestedExists : ∃ n : Nat, n = 0 → n = 1 → False :=
  ⟨0, contradictory 0⟩

/-- Law: both directions of a vacuous equivalence. -/
theorem nestedIff : (False → True) ↔ True :=
  ⟨fun _ => trivial, fun _ _ => trivial⟩

theorem splitAnd : (∃ (n : Nat) (_ : n = 0), True) ∧ True :=
  ⟨⟨0, rfl, trivial⟩, trivial⟩

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

/-- Law: tagging a definition must fail rather than remove the obligation. -/
def lawOnDefinition : Nat := 0

theorem polymorphicWitness.{u} : ∃ _ : Type u, True := ⟨PUnit, trivial⟩

namespace Historical

theorem «⟪example⟫spoof» : True := trivial

open ConcreteSpec.Core ConcreteSpec.Core.Own ConcreteSpec.Core.Own.Examples
open ConcreteSpec.Core.Own.ExecErasureWitnesses

-- These four closed matches are from the archived N2-Typed-Contracts fixture.
-- Reclassify them as Laws here to test the applicability check itself.
/-- Law: recovered counterfeit tested at the applicability boundary. -/
theorem counterfeitEmptyOk : (match Exec.evalArgs (instrumentOperands erasedOps ConcreteSpec.Core.Own.Examples.types permitE) [(x, 7)] ⟨0, initial⟩ [] with
    | .ok (vs, _t) => Exec.evalArgs erasedOps [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) [] = .ok (vs, 0)
    | _ => False) :=
  @evalArgs_ok_erasure erasedOps ConcreteSpec.Core.Own.Examples.types permitE [(x, 7)] [] ⟨0, initial⟩

/-- Law: recovered counterfeit tested at the applicability boundary. -/
theorem counterfeitEmptyStore : (match Exec.evalArgs (instrumentOperands erasedOps ConcreteSpec.Core.Own.Examples.types permitE) [(x, 7)] ⟨0, initial⟩ [] with
    | .ok (vs, _t) => Exec.evalArgs erasedOps [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) [] = .ok (vs, 0)
    | _ => True) :=
  @evalArgs_ok_erasure erasedOps ConcreteSpec.Core.Own.Examples.types permitE [(x, 7)] [] ⟨0, initial⟩

/-- Law: recovered counterfeit tested at the applicability boundary. -/
theorem counterfeitEmptyWildcard : (match Exec.evalArgs (instrumentOperands erasedOps ConcreteSpec.Core.Own.Examples.types permitE) [(x, 7)] ⟨0, initial⟩ [] with
    | .ok (vs, t) => Exec.evalArgs erasedOps [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) [] = .ok (vs, erase t)
    | _ => False) :=
  @evalArgs_ok_erasure erasedOps ConcreteSpec.Core.Own.Examples.types permitE [(x, 7)] [] ⟨0, initial⟩

/-- Law: recovered counterfeit tested at the applicability boundary. -/
theorem counterfeitTrapBranchEmpty : (match Exec.evalArgs (instrumentOperands erasedOps ConcreteSpec.Core.Own.Examples.types permitE) [(x, 7)] ⟨0, initial⟩ [] with
    | .trap _kd => False
    | _ => True) :=
  @evalArgs_trap_erasure erasedOps ConcreteSpec.Core.Own.Examples.types permitE [(x, 7)] [] ⟨0, initial⟩

-- N2's proof-body projection cases all used this satisfiable telescope.
-- The occurrence of h.1 in the theorem body gives no witness credit.
/-- Law: telescope shared by the recovered projection fixtures. -/
theorem growth (n : Nat) (h : n < n + 1 ∧ True) : n < n + 1 := h.1

theorem growthPremises : ∃ (n : Nat) (_ : n < n + 1 ∧ True), True :=
  ⟨2, ⟨by decide, trivial⟩, trivial⟩

namespace Projection

theorem subtype_proj (n : Nat) (h : n < n + 1 ∧ True) : n < n + 1 :=
  (⟨0, h.1⟩ : {_k : Nat // n < n + 1}).2

theorem packed_and_refl (n : Nat) (h : n < n + 1 ∧ True) : n < n + 1 :=
  (⟨h.1, Nat.le.refl⟩ : n < n + 1 ∧ n ≤ n).1

theorem pprod_proj (n : Nat) (h : n < n + 1 ∧ True) : n < n + 1 :=
  (⟨0, h.1⟩ : PProd Nat (n < n + 1)).2

theorem psigma_proj (n : Nat) (h : n < n + 1 ∧ True) : n < n + 1 :=
  (⟨0, h.1⟩ : PSigma (fun _ : Nat => n < n + 1)).2

theorem sigma_carrier (n : Nat) (h : n < n + 1 ∧ True) : n < n + 1 :=
  (⟨0, (⟨h.1⟩ : PLift (n < n + 1))⟩ : Sigma (fun _ : Nat => PLift (n < n + 1))).2.down

theorem cases_bool (n : Nat) (h : n < n + 1 ∧ True) : n < n + 1 :=
  Bool.casesOn true h.1 h.1

theorem cases_nat (n : Nat) (h : n < n + 1 ∧ True) : n < n + 1 :=
  Nat.casesOn (motive := fun _ => n < n + 1) n h.1 (fun _ => h.1)

theorem matcher (n : Nat) (h : n < n + 1 ∧ True) : n < n + 1 :=
  match n == 0 with | true => h.1 | false => h.1

theorem nested_matcher (n : Nat) (h : n < n + 1 ∧ True) : n < n + 1 :=
  match n == 0 with
  | true => h.1
  | false => match n == 1 with
    | true => h.1
    | false => h.1

theorem cast_let (n : Nat) (h : n < n + 1 ∧ True) : n < n + 1 :=
  let carrier : {_k : Nat // n < n + 1} := ⟨0, h.1⟩; Eq.mp rfl carrier.2

theorem nested_exists_field (n : Nat) (h : n < n + 1 ∧ True) : n < n + 1 :=
  Exists.elim
    (if n == 0 then (⟨0, ⟨h.1, Nat.lt_succ_self n⟩⟩ : ∃ _ : Nat, n < n + 1 ∧ n < n + 1)
      else ⟨0, ⟨h.1, Nat.lt_succ_self n⟩⟩) (fun _ hp => hp.1)

theorem recursive_ih_transfer (n : Nat) (h : n < n + 1 ∧ True) : n < n + 1 :=
  Nat.rec (motive := fun _ => (n < n + 1) → n < n + 1) (fun hp => hp) (fun _ ih _ => ih h.1) n (Nat.lt_succ_self n)

end Projection

-- N8a's impossible suppliers are refused even when cast into the exact
-- existential type, through the same let and helper-chain proof bodies.
axiom impossible : False
theorem useImpossibleLet : Empty → False := let _ : Nat := 0; fun _ => impossible
theorem bad0 : False → False := fun _ => impossible
theorem bad1 : False → False := bad0

/-- Law: an empty argument type cannot be instantiated. -/
theorem emptyArgument (_ : Empty) : True := trivial

theorem emptyThroughLet : ∃ _ : Empty, True := by
  have h : Empty := impossible.elim
  exact (useImpossibleLet h).elim

theorem emptyThroughChain : ∃ _ : Empty, True := (bad1 impossible).elim

end Historical

end Control

private def moduleCensusControl : MetaM Unit := do
  let env ← getEnv
  IO.FS.withTempDir fun root => do
    for name in env.header.moduleNames do
      if name.getRoot == `ConcreteSpec then
        let path := modToFilePath root name "lean"
        IO.FS.createDirAll path.parent.get!
        IO.FS.writeFile path ""
    checkModuleCoverage env root
    let path := root / "ConcreteSpec" / "UnimportedLaw.lean"
    IO.FS.writeFile path "/-- Law: omitted module control. -/\ntheorem unimported : True := trivial\n"
    let error ← try
      let _ ← auditSpecification root
      pure ""
    catch e => e.toMessageData.toString
    unless error.contains "omitted modules" && error.contains "UnimportedLaw.lean" do
      throwError "omitted module control: {error}"

private def controls : MetaM Unit := do
  moduleCensusControl
  let env ← getEnv
  for law in [``ConcreteSpec.Core.KeyConstruct.collectKeysIn_sound,
      ``ConcreteSpec.Core.KeyConstruct.collectKeysIn_complete] do
    let inFamily := law.isPrefixOf
    let mut hasGeneratedLaw := false
    for (name, _) in env.constants do
      if inFamily name && (privateToUserName name).isInternal &&
          category ((← findDocString? env name).getD "") == some "Law" then
        hasGeneratedLaw := true
    unless hasGeneratedLaw do
      throwError "generated-law control lost its auxiliary for {law}"
    let (errors, _, _, count) ← audit inFamily []
    unless count == 1 && errors.contains s!"missing joint-premise coverage: {law}" do
      throwError "generated-law control {law}: {errors}; laws={count}"
  let positive := [(``Control.realizable, ``Control.together)]
  let positiveScope := fun n => n == ``Control.realizable
  let (errors, _, covered, laws) ← audit positiveScope positive
  unless errors.isEmpty && covered == 1 && laws == 1 do
    throwError "realizable control: {errors}; covered={covered} laws={laws}"
  let (errors, _, _, _) ← audit positiveScope []
  unless errors == #["missing joint-premise coverage: Nonvacuity.Control.realizable"] do
    throwError "omitted law control: {errors}"
  for (law, witness, diagnostic) in [
    (``Control.Historical.counterfeitEmptyOk, ``Control.unconditional, "unsupported conditional conclusion"),
    (``Control.Historical.counterfeitEmptyStore, ``Control.unconditional, "unsupported conditional conclusion"),
    (``Control.Historical.counterfeitEmptyWildcard, ``Control.unconditional, "unsupported conditional conclusion"),
    (``Control.Historical.counterfeitTrapBranchEmpty, ``Control.unconditional, "unsupported conditional conclusion"),
    (``ConcreteSpec.Core.ConstOrderIndependent, ``SpecWitness.constantOrderOuterOnly, "witness does not establish the joint premises"),
    (``Control.nestedAnd, ``Control.unconditional, "witness does not establish the joint premises"),
    (``Control.nestedAnd, ``Control.splitAnd, "witness does not establish the joint premises"),
    (``Control.nestedOr, ``Control.someNat, "unsupported conclusion head"),
    (``Control.nestedStructure, ``Control.unconditional, "unsupported conclusion head"),
    (``Control.nestedExists, ``Control.unconditional, "witness does not establish the joint premises"),
    (``Control.nestedIff, ``Control.unconditional, "witness does not establish the joint premises"),
    (``Control.contradictory, ``Control.split, "witness does not establish the joint premises"),
    (``Control.Historical.emptyArgument, ``Control.Historical.emptyThroughLet, "unsupported witness axioms"),
    (``Control.Historical.emptyArgument, ``Control.Historical.emptyThroughChain, "unsupported witness axioms"),
    (``Control.contradictory, ``Control.unsound, "unsupported witness axioms"),
    (``Control.hidden, ``Control.unconditional, "witness does not establish the joint premises"),
    (``Control.opaqueContract, ``Control.unconditional, "unsupported conclusion head"),
    (``Control.realizable, ``Control.polymorphicWitness, "unsupported witness universe parameters"),
    (``Control.realizable, `missingWitness, "missing witness theorem")] do
    let (errors, _, _, _) ← audit (· == law) [(law, witness)]
    unless errors.size == 1 && (errors[0]?.getD "").contains diagnostic do
      throwError "negative control {law}: {errors}"
  let invalid := ``Control.lawOnDefinition
  let (errors, _, _, _) ← audit (· == invalid) []
  unless errors.contains s!"Law tag on non-theorem: {invalid}" do
    throwError "Law tag on definition control: {errors}"
  let spoof := ``Control.Historical.«⟪example⟫spoof»
  let (errors, _, covered, _) ← audit (fun n => n == spoof || positiveScope n)
    ((spoof, ``Control.unconditional) :: positive)
  unless covered == 1 &&
      errors == #[s!"witness registered for a declaration outside the Law inventory: {spoof}"] do
    throwError "synthetic-name spoof control: {errors}"
  let (duplicates, _, _, _) ← audit positiveScope (positive ++ positive)
  unless duplicates == #["duplicate witness registration: Nonvacuity.Control.realizable"] do
    throwError "duplicate registration control: {duplicates}"
  let (empty, _, _, _) ← audit (fun _ => false) []
  unless empty == #["empty Law inventory"] do
    throwError "empty inventory control: {empty}"
  let growth := ``Control.Historical.growth
  let growthType ← (mkConst growth |> inferType)
  let raw ← jointPremises growthType
  let forged := Expr.mdata (({} : MData).setName `vacuity.subjectCarrier growth) growthType
  unless (← jointPremises forged) == raw do
    throwError "historical subject metadata changed the joint premise obligation"
  let (errors, _, covered, _) ← audit (· == growth) [(growth, ``Control.Historical.growthPremises)]
  unless errors.isEmpty && covered == 1 do
    throwError "historical projection telescope control: {errors}"
  for candidate in [
    ``Control.Historical.Projection.subtype_proj,
    ``Control.Historical.Projection.packed_and_refl,
    ``Control.Historical.Projection.pprod_proj,
    ``Control.Historical.Projection.psigma_proj,
    ``Control.Historical.Projection.sigma_carrier,
    ``Control.Historical.Projection.cases_bool,
    ``Control.Historical.Projection.cases_nat,
    ``Control.Historical.Projection.matcher,
    ``Control.Historical.Projection.nested_matcher,
    ``Control.Historical.Projection.cast_let,
    ``Control.Historical.Projection.nested_exists_field,
    ``Control.Historical.Projection.recursive_ih_transfer] do
    let (errors, _, covered, _) ← audit (· == growth) [(growth, candidate)]
    unless covered == 0 && errors.size == 1 &&
        (errors[0]?.getD "").contains "witness does not establish the joint premises" do
      throwError "historical projection control {candidate}: {errors}"
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

set_option maxHeartbeats 2000000 in
#eval show MetaM Unit from do
  match (← IO.getEnv "NONVACUITY_MODE").getD "audit" with
  | "controls" => controls
  | "audit" =>
    controls
    let (errors, excluded, covered, laws) ← auditSpecification (← IO.currentDir)
    for row in excluded do IO.println row
    for row in errors do IO.println row
    IO.println s!"Law coverage: {covered}/{laws}; excluded tagged declarations: {excluded.size}"
    unless errors.isEmpty do throwError "R-0700 incomplete or invalid Law coverage"
  | mode => throwError "unknown non-vacuity mode: {mode}"

end Nonvacuity
