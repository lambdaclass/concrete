import Lean
import ConcreteSpec
import ConcreteSpecEvidence

/-!
A Law is non-vacuous iff its explicit premises are jointly satisfiable.
The audit telescopes every binder and generates an existential over the
whole telescope ending in True, using conjunction for proof premises
when the rest of the telescope does not depend on the proof. A registered
kernel-checked theorem must have exactly that type up to definitional
equality and satisfy the witness axiom policy. Non-vacuity certifies
only that the premises are satisfiable and says nothing about how
representative the witness is. The conclusion contributes no evidence.
The audit checks the heads of logical connectives and their
propositional operands, keeping named predicates folded. Unsupported
forms require restating the Law with explicit premises.
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

private partial def exposeConclusion (expr : Expr) : MetaM Expr := withIncRecDepth do
  let expr := expr.consumeMData
  let reduced := expr.getAppFn.betaRev expr.getAppRevArgs (useZeta := true)
  if reduced != expr then return ← exposeConclusion reduced
  match expr with
  | .letE _ _ value body _ => exposeConclusion (body.instantiate1 value)
  | other => pure other

-- Expose local syntax at each logical operand without unfolding predicates.
private partial def supportedConclusion (expr : Expr) : MetaM Bool := withIncRecDepth do
  let conclusion ← exposeConclusion expr
  let .const name _ := conclusion.getAppFn.consumeMData | return false
  if name == ``And || name == ``Or then
    let #[left, right] := conclusion.getAppArgs | return false
    return (← supportedConclusion left) && (← supportedConclusion right)
  if name == ``Not then
    let #[operand] := conclusion.getAppArgs | return false
    return ← supportedConclusion operand
  if name == ``Exists then
    let #[domain, predicate] := conclusion.getAppArgs | return false
    if (← isProp domain) && !(← supportedConclusion domain) then
      return false
    return ← withLocalDeclD `witness domain fun witness =>
      supportedConclusion (mkApp predicate witness)
  if name == ``Eq || name == ``HEq then
    return ← conclusion.getAppArgs.allM fun arg => do
      if ← isProp arg then supportedConclusion arg else pure true
  if name == ``True || name == ``False then return true
  let env ← getEnv
  let recursorSuffix := match name with
    | .str _ suffix => ["rec", recOnSuffix, brecOnSuffix, casesOnSuffix].contains suffix
    | _ => false
  if [``Iff, ``ite, ``dite].contains name || recursorSuffix ||
      isAuxRecursor env name || isCasesOnLike env name ||
      isMatcherCore env name || isMatcherLikeCore env name then
    return false
  let some info := env.find? name | return false
  -- A polymorphic eliminator specialized to Prop cannot qualify as a predicate.
  match info with
  | .axiomInfo _ | .defnInfo _ | .opaqueInfo _ | .inductInfo _ =>
    forallTelescope info.type fun params result =>
      pure (params.size == conclusion.getAppNumArgs && result.consumeMData == mkSort .zero)
  | _ => return false

private def jointPremises (type : Expr) : MetaM Expr :=
  forallTelescope type.consumeMData fun args conclusion => do
    unless ← supportedConclusion conclusion do
      throwError "restate with explicit premises"
    args.foldrM (init := mkConst ``True) fun arg rest => do
      let domain ← inferType arg
      if (← isProp domain) && !rest.containsFVar arg.fvarId! then
        return mkApp2 (mkConst ``And) domain rest
      mkAppM ``Exists #[← mkLambdaFVars #[arg] rest]

/-- Bind the census to the source population. An unimported module would
otherwise remove its Laws from the denominator. -/
private def checkModuleCoverage (env : Environment) (root : System.FilePath) : MetaM Unit := do
  let entry := root / "ConcreteSpec.lean"
  let source := ((if ← entry.pathExists then #[entry] else #[]) ++ (← System.FilePath.walkDir (root / "ConcreteSpec")))
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
  -- The existing authoritative proof policy permits propositional
  -- extensionality. The old proof-body transfer policy concerns a different
  -- audit: an explicit witness receives no credit for user axioms or choice.
  let axioms ← collectAxioms witness
  unless (axioms.filter (· != ``propext)).isEmpty do
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
    let some label := category ((← findSimpleDocString? env name (includeBuiltin := false)).getD "")
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
      (record.read reader = some decoded ∧
        (record.spec = reader ∧ reader = specVersion ∧
          ∃ kind : PremiseKind, kind.encode = record.category ∧
            decoded = ⟨kind, record.scope, record.spec⟩)) ∧
      (∃ _ : PremiseKind, True) :=
  ⟨specVersion, Vacuity.stackRecord.encode, Vacuity.stackRecord,
    ⟨rfl, rfl, rfl, .stack, rfl, rfl⟩, ⟨.stack, trivial⟩⟩

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

theorem checkedUnsignedShift :
    ∃ (ty : IntTy) (_ : ty.signed = false) (a count result : Int)
      (_ : ty.InRange a) (_ : checked ty .shr a count = .ok result), True :=
  ⟨.u8, rfl, 8, 1, 4, by decide, rfl, trivial⟩

theorem losslessSameWidth :
    ∃ s t : IntTy, (Lossless s t ∧ (∀ n, s.InRange n → t.InRange n)) ∧
      (∃ (n : Int) (_ : s.InRange n), True) :=
  ⟨.i8, .i8, ⟨by decide, fun _ h => h⟩, 7, by decide, trivial⟩

theorem checkedNegation :
    ∃ (ty : IntTy) (a r : Int) (_ : ty.InRange a),
      (checkedNeg ty a = .ok r ∧
        (r = -a ∧ if ty.signed = true then a ≠ ty.min else a = 0)) ∧
      (if ty.signed = true then True else True) :=
  ⟨.i8, 5, -5, by decide, ⟨rfl, rfl, by decide⟩, trivial⟩

theorem checkedNegationFailure :
    ∃ (ty : IntTy) (a : Int) (err : ArithmeticError) (_ : ty.InRange a),
      (checkedNeg ty a = .error err ∧
        (err = .overflow ∧ ¬ (if ty.signed = true then a ≠ ty.min else a = 0))) ∧
      (if ty.signed = true then True else True) :=
  ⟨.i8, -128, .overflow, by decide, ⟨rfl, rfl, by decide⟩, trivial⟩

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
    ∃ (U : ConstTypes) (_fuel : Nat) (source first second : List (ConstDecl U)),
      DependencyOrder U source first ∧ DependencyOrder U source second ∧ True :=
  ⟨ConstDemo.U, 0, [], [], [], ⟨.nil, .nil, trivial⟩, ⟨.nil, .nil, trivial⟩, trivial⟩

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
    ∃ (m : Memory) (l : LineageId) (bytes : Nat) (_ : bytes ≤ (m l).live) (m' : Memory),
      (apply m (.free l bytes) = some m' ∧
        (m' l).requested = (m l).requested ∧ (m' l).live + bytes = (m l).live ∧
        (m' l).peak = (m l).peak ∧ (m' l).functionPeak = (m l).functionPeak ∧
        (∀ other, other ≠ l → m' other = m other)) ∧
      (∃ (other : LineageId) (_ : other ≠ l), True) := by
  refine ⟨entryMemory, ⟨0⟩, 1, by decide, Memory.set entryMemory ⟨0⟩ (reportEntry.free 1),
    ⟨rfl, rfl, rfl, rfl, rfl, ?_⟩, ⟨⟨1⟩, by decide, trivial⟩⟩
  intro other different
  exact ite_eq_right different

theorem resetLiveAccount :
    ∃ (m : Memory) (l : LineageId) (m' : Memory),
      (apply m (.reset l) = some m' ∧
        ResetConservation (m l) (m' l) (resetOutflow l m (.reset l)) ∧
        (∀ other, other ≠ l → m' other = m other)) ∧
      (∃ (other : LineageId) (_ : other ≠ l), True) := by
  refine ⟨entryMemory, ⟨0⟩, Memory.set entryMemory ⟨0⟩ reportEntry.reset,
    ⟨rfl, ⟨rfl, rfl, rfl, rfl, rfl⟩, ?_⟩, ⟨⟨1⟩, by decide, trivial⟩⟩
  intro other different
  exact ite_eq_right different

theorem runFreeWithinLive :
    ∃ (m : Memory) (actions : List Action) (_ : ∀ l, (m l).WF)
      (_ : (run m actions).isSome = true) (m' : Memory),
      (run m actions = some m' ∧ (∀ l, (m' l).WF)) ∧ (∃ _ : LineageId, True) := by
  refine ⟨entryMemory, [.free ⟨0⟩ 1], entryMemoryWF, rfl,
    Memory.set entryMemory ⟨0⟩ (reportEntry.free 1), ⟨rfl, ?_⟩, ⟨⟨0⟩, trivial⟩⟩
  intro l
  by_cases equal : l = ⟨0⟩
  · subst l
    exact ⟨by decide, by decide⟩
  · show (if l = ⟨0⟩ then reportEntry.free 1 else entryMemory l).WF
    rw [ite_eq_right equal]
    exact entryMemoryWF l

end Accounting

section Erasure
open ConcreteSpec.Core.Own ConcreteSpec.Core.Own.Examples

-- This fixture uses the existing concrete evaluator and permission
-- transitions. Primitive actions are outside argument evaluation.
private abbrev argumentOps : Exec.Operands where
  Val := Int
  Expr := E
  Ty := Unit
  Store := Int
  Prim := Unit
  Event := Unit
  unitVal := 0
  unitTy := ()
  isCopy := fun _ => false
  eval := erasedEval
  life := fun _ s => some s
  test := fun _ => none
  allows := fun _ _ _ _ => false

private def okArmHasCounterexample : Prop :=
  ∃ (O : Exec.Operands) (_types : Var → Option Descriptor)
    (_permit : AList Var O.Val → O.Expr → Own.State → Option Own.State)
    (locals : AList Var O.Val) (args : List O.Expr) (s : Store O.Store)
    (values : List O.Val) (out : Store O.Store),
    Exec.evalArgs O locals (erase s) args ≠ .ok (values, erase out)

/-- The erased evaluator does not return an extra value for an empty list. -/
private theorem refuteOkArm : okArmHasCounterexample :=
  ⟨argumentOps, types, permitE, [], [], ⟨0, initial⟩, [1], ⟨0, initial⟩, by
    intro h
    cases h⟩

private def trapArmHasCounterexample : Prop :=
  ∃ (O : Exec.Operands)
    (locals : AList Var O.Val) (args : List O.Expr) (s : Store O.Store)
    (kind : TrapKind), Exec.evalArgs O locals (erase s) args ≠ .trap kind

/-- An empty list also cannot produce a trap. -/
private theorem refuteTrapArm : trapArmHasCounterexample :=
  ⟨argumentOps, [], [], ⟨0, initial⟩, .overflow, by
    intro h
    cases h⟩

theorem evalArgsOk :
    ∃ (O : Exec.Operands) (types : Var → Option Descriptor)
      (permit : AList Var O.Val → O.Expr → Own.State → Option Own.State)
      (locals : AList Var O.Val) (args : List O.Expr) (s : Store O.Store),
      args ≠ [] ∧ (match Exec.evalArgs (instrumentOperands O types permit) locals s args with
        | .ok (values, out) =>
          okArmHasCounterexample ∧ Exec.evalArgs O locals (erase s) args = .ok (values, erase out)
        | _ => ¬ True) :=
  ⟨argumentOps, types, permitE, [(x, 7)], [.read x, .move x], ⟨0, initial⟩,
    List.cons_ne_nil _ _, refuteOkArm, rfl⟩

theorem evalArgsTrap :
    ∃ (O : Exec.Operands) (types : Var → Option Descriptor)
      (permit : AList Var O.Val → O.Expr → Own.State → Option Own.State)
      (locals : AList Var O.Val) (args : List O.Expr) (s : Store O.Store),
      args ≠ [] ∧ (match Exec.evalArgs (instrumentOperands O types permit) locals s args with
        | .trap kind =>
          trapArmHasCounterexample ∧ Exec.evalArgs O locals (erase s) args = .trap kind
        | _ => ¬ True) :=
  ⟨argumentOps, types, permitE, [(x, 7)], [.move x, .fail .overflow], ⟨0, initial⟩,
    List.cons_ne_nil _ _, refuteTrapArm, rfl⟩

end Erasure

section Hybrid
open ConcreteSpec.Core.Hybrid

theorem materializeReuse :
    ∃ (_ : Target) (st : Hybrid.State) (p : Place) (t : CTy) (r : Range)
      (_ : st.places p = some ⟨t, .materialized r⟩)
      (_ : AddrSpace) (_base _align : Nat) (_ : List Mem.MByte), True :=
  ⟨linuxX8664, Audit.before, ⟨0⟩, .int .u8, Audit.range, rfl, ⟨0⟩, 60, 1, [], trivial⟩

theorem admittedEncoding :
    ∃ (_ : Target) (v : Value) (_ : v.Storable), True :=
  ⟨linuxX8664, .int .u8 1, by decide, trivial⟩

theorem refusedEncoding :
    ∃ (_ : Target) (v : Value) (_ : ¬ v.Storable), True :=
  ⟨linuxX8664, .bool true, by decide, trivial⟩

theorem structuredRead :
    ∃ (_ : Target) (st : Hybrid.State) (p : Place) (t : CTy) (v : Value)
      (_ : st.places p = some ⟨t, .structured v⟩) (_ : v.ty = t) (_ : v.Storable), True :=
  ⟨linuxX8664, Audit.structState, ⟨0⟩, .int .u8, .int .u8 1, rfl, rfl, by decide, trivial⟩

theorem decodedByte :
    ∃ (tgt : Target) (t : CTy) (bytes : List Mem.MByte),
      (match decode tgt t bytes with
      | some value => match value with
        | .int _ _ => True
        | _ => False
      | none => False) :=
  ⟨linuxX8664, .int .u8, [.init 1 none], trivial⟩

end Hybrid

theorem nonCopyDeclaration :
    ∃ (U : ConstTypes) (ds : List (ConstDecl U)) (d : ConstDecl U)
      (_ : d ∈ ds) (_ : U.isCopy d.ty = false), True :=
  ⟨ConstDemo.U, copyAuditDecls, ⟨ConstDemo.cid "S", .text, .bool true⟩,
    List.Mem.tail _ (List.Mem.head _), rfl, trivial⟩

section Exec
open ConcreteSpec.Core.Exec

theorem stepRet :
    ∃ (O : Operands) (_ : Program O) (e : O.Expr) (_ : SiteId)
      (L : AList Var O.Val) (st st' : O.Store) (_ : List (Frame O)) (v : O.Val)
      (_ : O.eval L st e = .ok (v, st')), True :=
  ⟨Demo.ops, Demo.prog, .arith .add (.move Demo.p) .counter, Demo.s, [(Demo.p, 7)],
    ⟨0, [[]]⟩, Demo.DState.onTop (Demo.p :: ·) ⟨0, [[]]⟩,
    [.scope ⟨1⟩ [Demo.p] [⟨Demo.bumpF, [], Demo.s⟩]], 7, rfl, trivial⟩

end Exec

theorem siblingInsertion :
    ∃ (_ : FnId) (_ _ : Nat) (_ : KeyConstruct.ParentContext)
      (_ _ : List KeyConstruct) (_ : KeyConstruct), True :=
  ⟨Vacuity.function, 7, 19, .children .body [Vacuity.unnamedLoop] Vacuity.nestedBody [],
    [], [Vacuity.nestedBody, Vacuity.insertedCall], Vacuity.insertedCall, trivial⟩

theorem keysOutsideEdit :
    ∃ (before after : FnId → List KeyConstruct) (_ _ : FnId → Nat) (edited : FnId)
      (_ : ∀ f, f ≠ edited → after f = before f)
      (f : FnId) (_ : f ≠ edited), True := by
  refine ⟨fun _ => [], fun f => if f = Vacuity.function then [Vacuity.unnamedLoop] else [],
    Vacuity.startSitesBefore, Vacuity.startSitesAfter, Vacuity.function, ?_,
    Vacuity.otherFunction, by decide, trivial⟩
  intro f outside
  exact ite_eq_right outside

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
   (``ConcreteSpec.Core.Own.evalArgs_ok_erasure, ``SpecWitness.evalArgsOk),
   (``ConcreteSpec.Core.Own.evalArgs_trap_erasure, ``SpecWitness.evalArgsTrap),
   (``ConcreteSpec.Core.Hybrid.materialize_reuse, ``SpecWitness.materializeReuse),
   (``ConcreteSpec.Core.typeConsts_refuses_nonCopy, ``SpecWitness.nonCopyDeclaration),
   (``ConcreteSpec.Core.Exec.step_ret, ``SpecWitness.stepRet),
   (``ConcreteSpec.Core.KeyConstruct.assignFunctionTable_insert_locality, ``SpecWitness.siblingInsertion),
   (``ConcreteSpec.Core.KeyConstruct.assignProgramTables_outsideEdit, ``SpecWitness.keysOutsideEdit),
   (``ConcreteSpec.Core.Hybrid.rep_roundtrip, ``SpecWitness.admittedEncoding),
   (``ConcreteSpec.Core.Hybrid.encode_refuses_unadmitted, ``SpecWitness.refusedEncoding),
   (``ConcreteSpec.Core.Hybrid.read_structured, ``SpecWitness.structuredRead),
   (``ConcreteSpec.Core.Hybrid.decode_valid, ``SpecWitness.decodedByte),
   (``ConcreteSpec.Core.toBits_shr_unsigned, ``SpecWitness.checkedUnsignedShift),
   (``ConcreteSpec.Core.lossless_iff, ``SpecWitness.losslessSameWidth),
   (``ConcreteSpec.Core.checkedNeg_error_iff, ``SpecWitness.checkedNegationFailure),
   (``ConcreteSpec.Core.checkedNeg_ok_arithmetic, ``SpecWitness.checkedNegation),
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
   (``ConcreteSpec.Core.bitNot_inRange, ``SpecWitness.integer),
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
   (``ConcreteSpec.Core.Leak.reset_conservation, ``SpecWitness.resetLiveAccount),
   (``ConcreteSpec.Core.Leak.run_wf, ``SpecWitness.runFreeWithinLive),
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

theorem together : ∃ (n : Nat) (i : Fin (n + 1)), i.val = n ∧ True :=
  ⟨0, ⟨0, by decide⟩, rfl, trivial⟩

theorem dataWithoutPremise : ∃ (n : Nat) (_ : Fin (n + 1)), True :=
  ⟨0, ⟨0, by decide⟩, trivial⟩

/-- Law: individually satisfiable premises requiring different values. -/
theorem contradictory (n : Nat) (h₀ : n = 0) (h₁ : n = 1) : False := by
  omega

theorem unconditional : True := trivial

theorem split : (∃ n : Nat, n = 0) ∧ (∃ n : Nat, n = 1) :=
  ⟨⟨0, rfl⟩, ⟨1, rfl⟩⟩

axiom falseWitness : ∃ n : Nat, n = 0 ∧ n = 1 ∧ True

theorem unsound : ∃ n : Nat, n = 0 ∧ n = 1 ∧ True := falseWitness

def HiddenContract : Prop := ∀ (_ : False), True

/-- Law: aliased applicability. -/
theorem hidden : HiddenContract := fun _ => trivial

/-- Law: a contradictory inner telescope under a conjunction. -/
theorem nestedAnd : (∀ n : Nat, n = 0 → n = 1 → False) ∧ True :=
  ⟨(fun _ h₀ h₁ => Nat.noConfusion (h₀.symm.trans h₁)), trivial⟩

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

/-- Law: the outer result cannot be detached from the inner antecedent. -/
theorem detached : ∃ n : Nat, n = 0 ∧ (n = 1 → False) :=
  ⟨0, rfl, by decide⟩

theorem detachedWitness : ∃ (n : Nat) (_ : n = 1), True :=
  ⟨1, rfl, trivial⟩

/-- Law: a universally quantified constraint fixes the witness function. -/
theorem detachedFunction :
    ∃ f : Nat → Option Nat, (∀ x, f x = none) ∧ (∀ x y, f x = some y → False) :=
  ⟨fun _ => none, fun _ => rfl, fun _ _ h => nomatch h⟩

theorem detachedFunctionWitness :
    ∃ f : Nat → Option Nat, (∃ _ : Nat, True) ∧
      (∃ (x y : Nat) (_ : f x = some y), True) :=
  ⟨fun _ => some 0, ⟨0, trivial⟩, ⟨0, 0, rfl, trivial⟩⟩

/-- Law: the proof-valued existential binder contains a vacuous telescope. -/
theorem proofDomain : ∃ (_ : ∀ n : Nat, n = 0 → n = 1 → False), True :=
  ⟨(fun _ h₀ h₁ => Nat.noConfusion (h₀.symm.trans h₁)), trivial⟩

/-- Law: double negation does not erase the underlying telescope. -/
theorem doubleNegated : ¬ ¬ (∀ n : Nat, n = 0 → n = 1 → False) :=
  fun h => h contradictory

/-- Law: equality of propositions cannot conceal a vacuous telescope. -/
theorem propositionEquality :
    (∀ n : Nat, n = 0 → n = 1 → False) = (∀ n : Nat, n = 0 → n = 1 → False) := rfl

/-- Law: extensional equality cannot hide a proof-valued antecedent. -/
theorem propositionEqualityTrue : (False → True) = True :=
  propext ⟨fun _ => trivial, fun _ _ => trivial⟩

/-- Law: both the result constraint and the nested antecedent hold at one value. -/
theorem attached : ∃ n : Nat, n = 1 ∧ (n = 1 → n = 1) :=
  ⟨1, rfl, id⟩

theorem attachedWitness :
    ∃ n : Nat, (n = 1 ∧ (n = 1 → n = 1)) ∧ (∃ (_ : n = 1), True) :=
  ⟨1, ⟨rfl, id⟩, rfl, trivial⟩

/-- Law: separate case witnesses can disagree on the shared universal input. -/
theorem disjoint (n : Nat) : (n = 0 → n = 0) ∧ (n = 1 → n = 1) :=
  ⟨id, id⟩

theorem disjointWitness :
    (∃ (n : Nat) (_ : n = 0), True) ∧ (∃ (n : Nat) (_ : n = 1), True) :=
  ⟨⟨0, rfl, trivial⟩, ⟨1, rfl, trivial⟩⟩

/-- Law: both nested hypotheses must use the same outer value. -/
theorem sharedInput (n : Nat) : (n = 1 → n = 1) ∧ (n < 2 → n < 2) :=
  ⟨id, id⟩

theorem sharedInputWitness :
    ∃ n : Nat, (∃ _ : n = 1, True) ∧ (∃ _ : n < 2, True) :=
  ⟨1, ⟨rfl, trivial⟩, ⟨by decide, trivial⟩⟩

/-- Law: a compound tautological fallback does not establish applicability. -/
theorem vacuousFallback : if (0 : Nat) = 1 then (1 : Nat) = 1 else True ∧ True :=
  ⟨trivial, trivial⟩

/-- Law: an existential fallback cannot cover an unreachable arm. -/
theorem existentialFallback : if (0 : Nat) = 1 then (1 : Nat) = 1 else ∃ _ : Unit, True :=
  ⟨(), trivial⟩

/-- Law: a universal fallback cannot cover an unreachable arm. -/
theorem universalFallback : if (0 : Nat) = 1 then (1 : Nat) = 1 else ∀ _ : Unit, True :=
  fun _ => trivial

/-- Law: an inhabited fallback constraint cannot cover a second substantive arm. -/
theorem competingArms : if (0 : Nat) = 1 then (1 : Nat) = 1 else ∃ n : Nat, n = 0 :=
  ⟨0, rfl⟩

theorem unitFallbackWitness : ∃ _ : Unit, True := ⟨(), trivial⟩

/-- Law: a decidable guard can itself contain a vacuous premise. -/
theorem vacuousGuard : if (False → True) then (0 : Nat) = 0 else False := rfl

/-- Law: dependent conditionals inspect their guard too. -/
theorem vacuousDependentGuard : dite (False → True) (fun _ => (0 : Nat) = 0) (fun _ => False) := by
  split
  · rfl
  · rename_i h
    exact h (fun _ => trivial)

/-- Law: a tautological disjunct does not supply the nested premises. -/
theorem trivialDisjunct : True ∨ (∀ n : Nat, n = 0 → n = 1 → False) := .inl trivial

theorem trivialDisjunctWitness :
    True ∨ ((∀ n : Nat, n = 0 → n = 1 → False) ∧
      (∃ n : Nat, n = 0 ∧ n = 1 ∧ True)) :=
  .inl trivial

/-- Law: a competing disjunct does not supply a nested premise. -/
theorem competingDisjunct (n : Nat) : n = 0 ∨ (n = 1 → n = 2 → False) :=
  .inr fun h₁ h₂ => by omega

theorem zeroDisjunctWitness :
    ∃ n : Nat, n = 0 ∨ ((n = 1 → n = 2 → False) ∧ (∃ (_ : n = 1) (_ : n = 2), True)) :=
  ⟨0, .inl rfl⟩

/-- Law: predicate equality cannot flatten a nested telescope. -/
theorem predicateEquality :
    (fun n : Nat => n = 0 → n = 1 → False) = (fun n : Nat => n = 0 → n = 1 → False) := rfl

/-- Law: a container can carry proposition values. -/
theorem containerEquality : (some (False → True) : Option Prop) = some (False → True) := rfl

/-- Law: a subtype can carry a proof field. -/
theorem subtypeDomain : ∃ _ : { n : Nat // n = 0 ∧ (n = 1 → False) }, True :=
  ⟨⟨0, rfl, by decide⟩, trivial⟩

/-- Law: a lifted proposition remains a proof carrier. -/
theorem liftedDomain : ∃ _ : PLift (∀ n : Nat, n = 0 → n = 1 → False), True :=
  ⟨⟨fun _ h₀ h₁ => Nat.noConfusion (h₀.symm.trans h₁)⟩, trivial⟩

structure TypedProof : Type where
  proof : False → True

/-- Law: a custom data structure can carry proof fields. -/
theorem structureDomain : ∃ _ : TypedProof, True := ⟨⟨fun _ => trivial⟩, trivial⟩

/-- Law: a type-level conditional can choose a trivial carrier. -/
theorem conditionalDomain :
    ∃ _ : (if (False → True) then Unit else TypedProof), True := ⟨(), trivial⟩

/-- Law: inequality cannot discard a proposition-bearing operand type. -/
theorem carrierInequality : (none : Option Prop) ≠ some (False → True) := by
  intro h
  cases h

/-- Law: heterogeneous equality cannot discard a proposition-bearing operand type. -/
theorem heterogeneousCarrierEquality :
    HEq (some (False → True) : Option Prop) (some (False → True) : Option Prop) := HEq.rfl

/-- Law: both substantive disjuncts admit the same input. -/
theorem reachableDisjuncts (n : Nat) : n = n ∨ n < n + 1 := .inl rfl

theorem reachableDisjunctsWitness : ∃ n : Nat, n = n ∧ n < n + 1 := ⟨0, rfl, by decide⟩

/-- Law: the empty pairwise proof never exercises the relation. -/
theorem emptyPairwise : List.Pairwise (fun (_ _ : Nat) => False) [] := .nil

/-- Law: a premise forcing the empty list cannot exercise the relation. -/
theorem contradictoryPairwise (xs : List Nat) (h : xs = []) :
    List.Pairwise (fun a _ => a = 0 ∧ a = 1) xs := h.symm ▸ .nil

theorem emptyPairwiseWitness : ∃ (xs : List Nat) (_ : xs = []), True :=
  ⟨[], rfl, trivial⟩

/-- Law: the substantive branch of an if can be reached. -/
theorem reachableIf (n : Nat) : if n = 1 then n = 1 else True := by
  split
  · assumption
  · trivial

theorem reachableIfWitness :
    ∃ n : Nat, (if n = 1 then n = 1 ∧ (∃ m : Nat, m ≠ 1) ∧ n = 1
      else n ≠ 1 ∧ ¬ True) :=
  ⟨1, rfl, ⟨0, by decide⟩, rfl⟩

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

/-- Law: a reflexive fallback is not contract evidence. -/
theorem reflexiveFallback : (match false with | true => False | false => (0 : Nat) = 0) := rfl

/-- Law: a proof argument must have applicable hypotheses of its own. -/
theorem vacuousProofArgument (_ : ∀ n : Nat, n = 0 → n = 1 → False) : True := trivial

theorem vacuousProofWitness : ∃ (_ : ∀ n : Nat, n = 0 → n = 1 → False), True :=
  ⟨(fun _ h₀ h₁ => Nat.noConfusion (h₀.symm.trans h₁)), trivial⟩

/-- Law: ex falso in a supplied function does not prove its assumptions. -/
theorem falseElimArgument (_ : ∀ n : Nat, n = 0 → n = 1 → (0 : Nat) = 1) : True := trivial

theorem falseElimWitness : ∃ (_ : ∀ n : Nat, n = 0 → n = 1 → (0 : Nat) = 1), True :=
  ⟨(fun _ h₀ h₁ => False.elim (Nat.noConfusion (h₀.symm.trans h₁))), trivial⟩

/-- Law: a proposition value requires concrete evidence of its applicability. -/
theorem propositionArgument (_ : Prop) : True := trivial

theorem concreteProposition : ∃ p : Prop, p := ⟨True, trivial⟩
theorem unprovedProposition : ∃ _ : Prop, True := ⟨False, trivial⟩

theorem reachableIfWithoutGuard : ∃ n : Nat, (if n = 1 then True else False) :=
  ⟨1, trivial⟩

/-- Law: a dependent branch keeps the guard from its actual data input. -/
theorem reachableDite (n : Nat) : dite (n = 1) (fun _ => n = 1) (fun _ => True) :=
  if h : n = 1 then dite_eq_left h ▸ h else dite_eq_right h ▸ trivial

theorem reachableDiteWitness :
    ∃ n : Nat, dite (n = 1) (fun _ => n = 1 ∧ (∃ m : Nat, m ≠ 1) ∧ n = 1)
      (fun _ => n ≠ 1 ∧ ¬ True) :=
  ⟨1, rfl, ⟨0, by decide⟩, rfl⟩

/-- Law: the selected branch can introduce its own dependent hypotheses. -/
theorem branchPremise (n : Nat) :
    if n = 1 then ∀ m : Nat, m = n → m = 1 else True := by
  split
  · rename_i h
    exact fun _ hm => hm.trans h
  · trivial

theorem branchPremiseWitness :
    ∃ n : Nat, if n = 1 then n = 1 ∧
      (∃ q : Nat, ¬ (∀ m : Nat, m = q → m = 1)) ∧
      (∀ m : Nat, m = n → m = 1) ∧
      (∃ (m : Nat) (_ : m = n), True)
    else n ≠ 1 ∧ ¬ True :=
  ⟨1, rfl, ⟨0, fun h => Nat.noConfusion (h 0 rfl)⟩, (fun _ h => h), 1, rfl, trivial⟩

/-- Law: a proof argument cannot conceal a premise in a proposition structure. -/
theorem wrappedArgument (_ : Wrapped) : True := trivial

theorem wrappedArgumentWitness : ∃ _ : Wrapped, True := ⟨nestedStructure, trivial⟩

def selectedProofDomain : Prop := if true = true then False → True else False

/-- Law: an instantiated proof domain may reduce to an implication. -/
theorem conditionalProofArgument (_ : selectedProofDomain) : True := trivial

theorem conditionalProofWitness : ∃ _ : selectedProofDomain, True :=
  ⟨(fun _ => trivial), trivial⟩

namespace RoundSix

-- These proof premises are inhabited. The first two functions eliminate
-- incompatible equations, the structure has an inhabitant, and the
-- conditional selects the inhabited branch.
theorem vacuousProofPremises : (∀ n : Nat, n = 0 → n = 1 → False) ∧ True := by
  refine ⟨fun _ h₀ h₁ => ?_, trivial⟩
  let zeroCase (n : Nat) : Prop := match n with
    | 0 => True
    | _ + 1 => False
  exact Eq.mp (congrArg zeroCase (h₀.symm.trans h₁)) True.intro

theorem falseElimPremises : (∀ n : Nat, n = 0 → n = 1 → (0 : Nat) = 1) ∧ True :=
  ⟨fun n h₀ h₁ => (vacuousProofPremises.1 n h₀ h₁).elim, trivial⟩

theorem wrappedPremises : Wrapped ∧ True := ⟨nestedStructure, trivial⟩

theorem selectedProofPremises : selectedProofDomain ∧ True := ⟨fun _ => trivial, trivial⟩

-- The empty list establishes the explicit constraint. Pairwise is the
-- conclusion, so the audit requires no evidence of an exercised relation.
theorem emptyListPremises : ∃ xs : List Nat, xs = [] ∧ True := ⟨[], rfl, trivial⟩


/-- Law: authored underscore names belong in the census even without a registration. -/
theorem _hidden (h : False) : True := h.elim

/-- Law: a primitive recursor hides an unreachable arm. -/
theorem primitiveRecursor : Bool.rec (motive := fun _ => Prop) True False false :=
  trivial

/-- Law: an auxiliary recursor hides the same arm. -/
theorem auxiliaryRecursor : Bool.recOn (motive := fun _ => Prop) false True False :=
  trivial

/-- Law: a proposition variable is not a supported conclusion head. -/
theorem variableConclusion (p : Prop) (h : p) : p := h

theorem variablePremises : ∃ p : Prop, p ∧ True := ⟨True, trivial, trivial⟩

-- Universal data, proof and proposition-valued binders are all included.
/-- Law: a universal data carrier. -/
theorem lifted (_ : PLift (False → True)) : True := trivial

theorem liftedPremises : ∃ _ : PLift (False → True), True :=
  ⟨⟨fun _ => trivial⟩, trivial⟩

inductive Packed : Prop where
  | intro : PLift (False → True) → Packed

/-- Law: a proof premise with a data field. -/
theorem packed (_ : Packed) : True := trivial

-- The function field has an inhabitant and its constructor establishes Packed.
theorem packedPremises : Packed ∧ True := ⟨.intro ⟨fun _ => trivial⟩, trivial⟩

/-- Law: a packed field must establish its explicit proof, even when another field is vacuous. -/
theorem packedImpossible (_ : Packed ∧ False) : True := trivial

axiom packedContradiction : False

theorem packedImpossiblePremises : (Packed ∧ False) ∧ True :=
  ⟨⟨packedPremises.1, packedContradiction⟩, trivial⟩

/-- Law: disequality as an explicit premise. -/
theorem unequal (_ : (none : Option Prop) ≠ some (False → True)) : True := trivial

theorem unequalPremises : ((none : Option Prop) ≠ some (False → True)) ∧ True :=
  ⟨carrierInequality, trivial⟩

-- The former audit accepted these existential encodings of proof premises.
theorem packedPrevious : ∃ _ : Packed, True := ⟨.intro ⟨fun _ => trivial⟩, trivial⟩

theorem unequalPrevious : ∃ _ : (none : Option Prop) ≠ some (False → True), True :=
  ⟨carrierInequality, trivial⟩

/-- Law: a conditional proof premise. -/
theorem conditionalPremise (_ : if (False → True) then True else False) : True := trivial

theorem conditionalPremises : (if (False → True) then True else False) ∧ True :=
  ⟨trivial, trivial⟩

theorem conditionalPrevious : ∃ _ : (if (False → True) then True else False), True :=
  ⟨trivial, trivial⟩

/-- Law: a contingent fallback still hides the branch applicability. -/
theorem fallback (n : Nat) (h : n = 0) : if (0 : Nat) = 1 then False else n = 0 := h

theorem fallbackPremises : ∃ n : Nat, n = 0 ∧ True := ⟨0, rfl, trivial⟩

theorem fallbackPrevious :
    ∃ (n : Nat) (_ : n = 0),
      if (0 : Nat) = 1 then 0 = 1 ∧ ¬ False ∧ False
      else ¬ ((0 : Nat) = 1) ∧ (∃ m : Nat, m ≠ 0) ∧ n = 0 :=
  ⟨0, rfl, by decide, ⟨1, by decide⟩, rfl⟩

/-- Law: a Decidable eliminator still hides the branch applicability. -/
theorem decidableMatch (n : Nat) (h : n = 0) :
    Decidable.casesOn (inferInstance : Decidable (False → True))
      (fun b _ => if b then n = 0 else False) := h

/-- Law: a Boolean match also hides applicability. -/
theorem decideMatch (n : Nat) (h : n = 0) :
    (match decide (False → True) with | true => n = 0 | false => False) := h

theorem decideMatchPrevious :
    ∃ (n : Nat) (_ : n = 0),
      (match decide (False → True) with
       | true => (∃ m : Nat, m ≠ 0) ∧ n = 0
       | false => ¬ False ∧ False) :=
  ⟨0, rfl, ⟨1, by decide⟩, rfl⟩

/-- Law: membership with a proposition carrier in the conclusion. -/
theorem membership : (False → True) ∈ [False → True] := .head _

/-- Law: equality over an empty function domain in the conclusion. -/
theorem abstractDomain : (fun _ : Nat.lt 1 0 => 0) = (fun _ : Nat.lt 1 0 => 1) :=
  funext fun h => (Nat.not_succ_le_zero _ h).elim

/-- Law: type, instance and proof-dependent binders. -/
theorem dependent {α : Type} [Inhabited α] (h : True) (_ : h = True.intro) :
    (default : α) = default := rfl

theorem dependentPremises :
    ∃ (α : Type) (_ : Inhabited α) (h : True), h = True.intro ∧ True :=
  ⟨Unit, inferInstance, trivial, rfl, trivial⟩

/-- Law: an implication underneath local syntax must be made explicit. -/
theorem localImplication : let p := False; p → True := fun _ => trivial

end RoundSix

namespace Historical

theorem «⟪example⟫spoof» : True := trivial

open ConcreteSpec.Core ConcreteSpec.Core.Own ConcreteSpec.Core.Own.Examples

-- These four closed matches are from the archived N2-Typed-Contracts fixture.
-- Reclassify them as Laws here to test the applicability check itself.
/-- Law: recovered counterfeit tested at the applicability boundary. -/
theorem counterfeitEmptyOk : (match Exec.evalArgs (instrumentOperands erasedOps ConcreteSpec.Core.Own.Examples.types permitE) [(x, 7)] ⟨0, initial⟩ [] with
    | .ok (vs, _t) => Exec.evalArgs erasedOps [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) [] = .ok (vs, 0)
    | _ => False) :=
  @evalArgs_ok_erasure erasedOps ConcreteSpec.Core.Own.Examples.types permitE [(x, 7)] [] ⟨0, initial⟩

/-- Law: a conjunction cannot hide the archived empty-list match. -/
theorem wrappedEmptyOk : (match Exec.evalArgs (instrumentOperands erasedOps ConcreteSpec.Core.Own.Examples.types permitE) [(x, 7)] ⟨0, initial⟩ [] with
    | .ok (vs, _t) => Exec.evalArgs erasedOps [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) [] = .ok (vs, 0)
    | _ => False) ∧ True :=
  ⟨counterfeitEmptyOk, trivial⟩

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

/-- Law: a projection must preserve the argument-list applicability constraint. -/
theorem projectedEmptyOk : (match
    (Exec.evalArgs (instrumentOperands erasedOps ConcreteSpec.Core.Own.Examples.types permitE)
      [(x, 7)] ⟨0, initial⟩ [], ()).1 with
    | .ok (vs, _t) => Exec.evalArgs erasedOps [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) [] = .ok (vs, 0)
    | _ => False) := counterfeitEmptyOk

-- The archived projection cases use this satisfiable telescope. This control
-- checks applicability and metadata independence; see R-0219 for body provenance.
/-- Law: telescope shared by the recovered projection fixtures. -/
theorem growth (n : Nat) (h : n < n + 1 ∧ True) : n < n + 1 := h.1

theorem growthPremises : ∃ n : Nat, (n < n + 1 ∧ True) ∧ True :=
  ⟨2, ⟨by decide, trivial⟩, trivial⟩

-- Archived R-0219 proof-body probes, with their original shared telescope.
-- A proof of the conclusion alone cannot register as applicability evidence.
namespace Projection

/-- Law: archived subtype-proj probe. -/
theorem subtype_proj (n : Nat) (h : n < n+1 ∧ True) : n < n+1 :=
  (⟨0, h.1⟩ : {_k : Nat // n < n+1}).2

/-- Law: archived packed-and-refl probe. -/
theorem packed_and_refl (n : Nat) (h : n < n+1 ∧ True) : n < n+1 :=
  (⟨h.1, Nat.le.refl⟩ : n < n+1 ∧ n ≤ n).1

/-- Law: archived pprod-proj probe. -/
theorem pprod_proj (n : Nat) (h : n < n+1 ∧ True) : n < n+1 :=
  (⟨0, h.1⟩ : PProd Nat (n < n+1)).2

/-- Law: archived psigma-proj probe. -/
theorem psigma_proj (n : Nat) (h : n < n+1 ∧ True) : n < n+1 :=
  (⟨0, h.1⟩ : PSigma (fun _ : Nat => n < n+1)).2

/-- Law: archived cases-bool probe. -/
theorem cases_bool (n : Nat) (h : n < n+1 ∧ True) : n < n+1 :=
  Bool.casesOn true h.1 h.1

/-- Law: archived cases-nat probe. -/
theorem cases_nat (n : Nat) (h : n < n+1 ∧ True) : n < n+1 :=
  Nat.casesOn (motive := fun _ => n < n+1) n h.1 (fun _ => h.1)

/-- Law: archived matcher probe. -/
theorem matcher (n : Nat) (h : n < n+1 ∧ True) : n < n+1 :=
  match n == 0 with | true => h.1 | false => h.1

/-- Law: archived nested-matcher probe. -/
theorem nested_matcher (n : Nat) (h : n < n+1 ∧ True) : n < n+1 :=
  match n == 0 with | true => h.1 | false => match n == 1 with | true => h.1 | false => h.1

/-- Law: archived mixed-minors probe. -/
theorem mixed_minors (n : Nat) (h : n < n+1 ∧ True) : n < n+1 :=
  Bool.casesOn (n == 0) (Nat.lt_succ_self n) h.1

/-- Law: archived recursor-function probe. -/
theorem recursor_function (n : Nat) (h : n < n+1 ∧ True) : n < n+1 :=
  (Bool.casesOn (motive := fun _ => (n < n+1) → n < n+1) (n == 0) (fun hp => hp) (fun hp => hp)) h.1

/-- Law: archived match-carrier probe. -/
theorem match_carrier (n : Nat) (h : n < n+1 ∧ True) : n < n+1 :=
  (match n == 0 with | true => (⟨0, h.1⟩ : {_k : Nat // n < n+1}) | false => ⟨0, h.1⟩).2

/-- Law: archived psum-carrier probe. -/
theorem psum_carrier (n : Nat) (h : n < n+1 ∧ True) : n < n+1 :=
  PSum.casesOn (motive := fun _ : PSum Nat (n < n+1) => n < n+1) (if n == 0 then PSum.inl n else PSum.inr h.1) (fun _ => Nat.lt_succ_self n) (fun hp => hp)

/-- Law: archived nested-exists-field probe. -/
theorem nested_exists_field (n : Nat) (h : n < n+1 ∧ True) : n < n+1 :=
  Exists.elim (if n == 0 then (⟨0, ⟨h.1, Nat.lt_succ_self n⟩⟩ : ∃ _ : Nat, n < n+1 ∧ n < n+1) else ⟨0, ⟨h.1, Nat.lt_succ_self n⟩⟩) (fun _ hp => hp.1)

/-- Law: archived minor-function-application probe. -/
theorem minor_function_application (n : Nat) (h : n < n+1 ∧ True) : n < n+1 :=
  Exists.elim (if n == 0 then (⟨0, fun hp => hp⟩ : ∃ _ : Nat, (n < n+1) → n < n+1) else ⟨0, fun hp => hp⟩) (fun _ f => f h.1)

/-- Law: archived recursive-ih-transfer probe. -/
theorem recursive_ih_transfer (n : Nat) (h : n < n+1 ∧ True) : n < n+1 :=
  Nat.rec (motive := fun _ => (n < n+1) → n < n+1) (fun hp => hp) (fun _ ih _ => ih h.1) n (Nat.lt_succ_self n)

/-- Law: archived cast-let probe. -/
theorem cast_let (n : Nat) (h : n < n+1 ∧ True) : n < n+1 :=
  let carrier : {_k : Nat // n < n+1} := ⟨0, h.1⟩; Eq.mp rfl carrier.2

/-- Law: archived iff-field probe. -/
theorem iff_field (n : Nat) (h : n < n+1 ∧ True) : n < n+1 :=
  ((Iff.intro (fun _ : True => h.1) (fun _ => True.intro)).mp True.intro)

/-- Law: archived exists-carrier probe. -/
theorem exists_carrier (n : Nat) (h : n < n+1 ∧ True) : n < n+1 :=
  Exists.elim (⟨0, h.1⟩ : ∃ _ : Nat, n < n+1) (fun _ hp => hp)

/-- Law: archived sigma-carrier probe. -/
theorem sigma_carrier (n : Nat) (h : n < n+1 ∧ True) : n < n+1 :=
  (⟨0, (⟨h.1⟩ : PLift (n < n+1))⟩ : Sigma (fun _ : Nat => PLift (n < n+1))).2.down

end Projection

structure Fieldless where

/-- Law: a fieldless subject is still a telescope binder. -/
theorem fieldless (_ : Fieldless) : True := trivial

theorem fieldlessPremises : ∃ _ : Fieldless, True := ⟨⟨⟩, trivial⟩

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

-- Replay the reachable helper as a proof of the exact premise, with both
-- a dead arm and a contradictory reached arm in the same witness.
axiom opaqueP : Prop
axiom hp : opaqueP
axiom hn : ¬ opaqueP

theorem branchDischarge (_k : Nat) (bad : Bool) (p : Prop) (positive : p)
    (negative : ¬p) : 2 = (2 : Nat) := by
  cases bad
  · rfl
  · exact False.elim (negative positive)

/-- Law: the premise from the archived reachable joint-helper fixture. -/
theorem helperGrowth (n : Nat) (h : ∀ _k : Nat, n = 2) : n < n + 1 :=
  (h 0).symm ▸ Nat.lt_succ_self 2

theorem throughReachableHelper : ∃ n : Nat, (∀ _k : Nat, n = 2) ∧ True :=
  ⟨2, fun k => Eq.trans (branchDischarge k false opaqueP hp hn)
    (branchDischarge k true opaqueP hp hn), trivial⟩

theorem independentHelperWitness :
    ∃ n : Nat, (∀ _k : Nat, n = 2) ∧ True :=
  ⟨2, (fun _ => rfl), trivial⟩

theorem directHelperApplication : 2 < (2 : Nat) + 1 :=
  helperGrowth 2 (fun _ => rfl)

theorem contradictoryHelperApplication : 2 < (2 : Nat) + 1 :=
  helperGrowth 2 (fun k => Eq.trans (branchDischarge k false opaqueP hp hn)
    (branchDischarge k true opaqueP hp hn))

theorem unrelatedHelperResult : 2 < (2 : Nat) + 1 := Nat.lt_succ_self 2

/-- Law: the list has operands but only the fallback arm is selected. -/
theorem nonemptyWrongArm :
    (match Exec.evalArgs (instrumentOperands erasedOps ConcreteSpec.Core.Own.Examples.types permitE)
        [(x, 7)] ⟨0, initial⟩ [.fail .overflow] with
      | .ok (vs, t) => Exec.evalArgs erasedOps [(x, 7)] 0 [.fail .overflow] = .ok (vs, erase t)
      | _ => True) :=
  @evalArgs_ok_erasure erasedOps ConcreteSpec.Core.Own.Examples.types permitE
    [(x, 7)] [.fail .overflow] ⟨0, initial⟩

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
    let stale := root / "ConcreteSpec.lean"
    IO.FS.removeFile stale
    let error ← try
      checkModuleCoverage env root
      pure ""
    catch e => e.toMessageData.toString
    unless error.contains s!"stale modules [{stale}]" do
      throwError "stale module control: {error}"
    IO.FS.writeFile stale ""
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
    let mut labelledTheorems := 0
    let mut expectedErrors := #[]
    for (name, info) in env.constants do
      if inFamily name &&
          category ((← findSimpleDocString? env name (includeBuiltin := false)).getD "") == some "Law" then
        match info with
        | .thmInfo _ =>
          labelledTheorems := labelledTheorems + 1
          expectedErrors := expectedErrors.push s!"missing joint-premise coverage: {name}"
        | _ => expectedErrors := expectedErrors.push s!"Law tag on non-theorem: {name}"
    unless expectedErrors.size > 1 do
      throwError "generated-law control lost its labelled auxiliary for {law}"
    let (errors, _, _, count) ← audit inFamily []
    unless count == labelledTheorems && expectedErrors.all errors.contains do
      throwError "generated-law control {law}: {errors}; laws={count}"
  let positive := [(``Control.realizable, ``Control.together)]
  let positiveScope := fun n => n == ``Control.realizable
  let (errors, _, covered, laws) ← audit positiveScope positive
  unless errors.isEmpty && covered == 1 && laws == 1 do
    throwError "realizable control: {errors}; covered={covered} laws={laws}"
  let (errors, _, _, _) ← audit positiveScope []
  unless errors == #["missing joint-premise coverage: Nonvacuity.Control.realizable"] do
    throwError "omitted law control: {errors}"
  let hidden := ``Control.RoundSix._hidden
  let (errors, _, covered, laws) ← audit (fun n => n == hidden || positiveScope n) positive
  unless errors == #[s!"missing joint-premise coverage: {hidden}"] &&
      covered == 1 && laws == 2 do
    throwError "authored underscore Law census: {errors}; covered={covered} laws={laws}"
  for (law, witness) in [
      (``Control.RoundSix.primitiveRecursor, ``Control.unconditional),
      (``Control.RoundSix.auxiliaryRecursor, ``Control.unconditional),
      (``Control.RoundSix.variableConclusion, ``Control.RoundSix.variablePremises)] do
    let (errors, _, covered, laws) ← audit (· == law) [(law, witness)]
    unless errors == #[s!"{law}: incomplete check: restate with explicit premises"] &&
        covered == 0 && laws == 1 do
      throwError "unsupported conclusion control {law}: {errors}; covered={covered} laws={laws}"
  let mut falsePasses := #[]
  for (law, witness) in [(``Control.vacuousProofArgument, ``Control.vacuousProofWitness),
      (``Control.falseElimArgument, ``Control.falseElimWitness),
      (``Control.wrappedArgument, ``Control.wrappedArgumentWitness),
      (``Control.conditionalProofArgument, ``Control.conditionalProofWitness),
      (``Control.realizable, ``Control.dataWithoutPremise),
      (``Control.realizable, ``Control.unconditional),
      (``Control.disjoint, ``Control.disjointWitness),
      (``Control.reachableIf, ``Control.reachableIfWithoutGuard),
      (``Control.existentialFallback, ``Control.unitFallbackWitness),
      (``Control.universalFallback, ``Control.unitFallbackWitness),
      (``Control.competingArms, ``Control.someNat),
      (``Control.trivialDisjunct, ``Control.trivialDisjunctWitness),
      (``Control.competingDisjunct, ``Control.zeroDisjunctWitness),
      (``Control.subtypeDomain, ``Control.subtypeDomain),
      (``Control.liftedDomain, ``Control.liftedDomain),
      (``Control.structureDomain, ``Control.structureDomain),
      (``Control.conditionalDomain, ``Control.unitFallbackWitness),
      (``Control.contradictoryPairwise, ``Control.emptyPairwiseWitness)] do
    let (errors, _, covered, laws) ← audit (· == law) [(law, witness)]
    unless covered == 0 && laws == 1 && errors.size == 1 do
      falsePasses := falsePasses.push law
  unless falsePasses.isEmpty do
    throwError "planted false passes accepted: {falsePasses}"
  -- Every accepted registration supplies a proof of the generated telescope.
  for (law, proof) in [
      (``Control.vacuousProofArgument, ``Control.RoundSix.vacuousProofPremises),
      (``Control.falseElimArgument, ``Control.RoundSix.falseElimPremises),
      (``Control.wrappedArgument, ``Control.RoundSix.wrappedPremises),
      (``Control.conditionalProofArgument, ``Control.RoundSix.selectedProofPremises),
      (``Control.contradictoryPairwise, ``Control.RoundSix.emptyListPremises),
      -- These Laws have no explicit binders. Their conclusions earn zero credit.
      (``Control.subtypeDomain, ``Control.unconditional),
      (``Control.liftedDomain, ``Control.unconditional),
      (``Control.structureDomain, ``Control.unconditional),
      (``Control.conditionalDomain, ``Control.unconditional),
      (``Control.propositionArgument, ``Control.unprovedProposition),
      (``Control.predicateEquality, ``Control.unconditional),
      (``Control.containerEquality, ``Control.unconditional),
      (``Control.carrierInequality, ``Control.unconditional),
      (``Control.heterogeneousCarrierEquality, ``Control.unconditional),
      (``Control.emptyPairwise, ``Control.unconditional),
      (``Control.nestedStructure, ``Control.unconditional),
      (``Control.hidden, ``Control.unconditional),
      (``Control.opaqueContract, ``Control.unconditional),
      (``Control.RoundSix.lifted, ``Control.RoundSix.liftedPremises),
      (``Control.RoundSix.packed, ``Control.RoundSix.packedPremises),
      (``Control.RoundSix.unequal, ``Control.RoundSix.unequalPremises),
      (``Control.RoundSix.conditionalPremise, ``Control.RoundSix.conditionalPremises),
      (``Control.RoundSix.membership, ``Control.unconditional),
      (``Control.RoundSix.abstractDomain, ``Control.unconditional),
      (``Control.RoundSix.dependent, ``Control.RoundSix.dependentPremises),
      (``Control.Historical.fieldless, ``Control.Historical.fieldlessPremises)] do
    let (errors, _, covered, laws) ← audit (· == law) [(law, proof)]
    unless errors.isEmpty && covered == 1 && laws == 1 do
      throwError "exact telescope control {law}: {errors}"
  let mut badDiagnostics := #[]
  for (law, witness, diagnostic) in [
    (``Control.reflexiveFallback, ``Control.unconditional, "restate with explicit premises"),
    (``Control.reachableIf, ``Control.someNat, "restate with explicit premises"),
    (``Control.existentialFallback, ``Control.unconditional, "restate with explicit premises"),
    (``Control.universalFallback, ``Control.unconditional, "restate with explicit premises"),
    (``Control.competingArms, ``Control.unconditional, "restate with explicit premises"),
    (``Control.vacuousGuard, ``Control.unconditional, "restate with explicit premises"),
    (``Control.vacuousDependentGuard, ``Control.unconditional, "restate with explicit premises"),
    (``Control.Historical.projectedEmptyOk, ``Control.unconditional, "restate with explicit premises"),
    (``Control.RoundSix.lifted, ``Control.unconditional, "witness does not establish the joint premises"),
    (``Control.RoundSix.packedImpossible, ``Control.RoundSix.packedImpossiblePremises, "unsupported witness axioms"),
    (``Control.RoundSix.packed, ``Control.RoundSix.packedPrevious, "witness does not establish the joint premises"),
    (``Control.RoundSix.unequal, ``Control.RoundSix.unequalPrevious, "witness does not establish the joint premises"),
    (``Control.RoundSix.conditionalPremise, ``Control.RoundSix.conditionalPrevious, "witness does not establish the joint premises"),
    (``Control.RoundSix.fallback, ``Control.RoundSix.fallbackPremises, "restate with explicit premises"),
    (``Control.RoundSix.fallback, ``Control.RoundSix.fallbackPrevious, "restate with explicit premises"),
    (``Control.RoundSix.decidableMatch, ``Control.RoundSix.fallbackPremises, "restate with explicit premises"),
    (``Control.RoundSix.decideMatch, ``Control.RoundSix.decideMatchPrevious, "restate with explicit premises"),
    (``Control.Historical.fieldless, ``Control.unconditional, "witness does not establish the joint premises"),
    (``Control.RoundSix.localImplication, ``Control.unconditional, "restate with explicit premises"),
    (``Control.Historical.counterfeitEmptyOk, ``Control.unconditional, "restate with explicit premises"),
    (``Control.Historical.counterfeitEmptyStore, ``Control.unconditional, "restate with explicit premises"),
    (``Control.Historical.counterfeitEmptyWildcard, ``Control.unconditional, "restate with explicit premises"),
    (``Control.Historical.counterfeitTrapBranchEmpty, ``Control.unconditional, "restate with explicit premises"),
    (``Control.Historical.wrappedEmptyOk, ``Control.unconditional, "restate with explicit premises"),
    (``ConcreteSpec.Core.ConstOrderIndependent, ``SpecWitness.constantOrderOuterOnly, "restate with explicit premises"),
    (``Control.Historical.nonemptyWrongArm, ``Control.unconditional, "restate with explicit premises"),
    (``Control.vacuousFallback, ``Control.unconditional, "restate with explicit premises"),
    (``Control.detached, ``Control.detachedWitness, "restate with explicit premises"),
    (``Control.detachedFunction, ``Control.detachedFunctionWitness, "restate with explicit premises"),
    (``Control.nestedAnd, ``Control.splitAnd, "restate with explicit premises"),
    (``Control.disjoint, ``Control.someNat, "restate with explicit premises"),
    (``Control.competingDisjunct, ``Control.someNat, "restate with explicit premises"),
    (``Control.trivialDisjunct, ``Control.unconditional, "restate with explicit premises"),
    (``Control.detached, ``Control.unconditional, "restate with explicit premises"),
    (``Control.detachedFunction, ``Control.unconditional, "restate with explicit premises"),
    (``Control.proofDomain, ``Control.unconditional, "restate with explicit premises"),
    (``Control.doubleNegated, ``Control.unconditional, "restate with explicit premises"),
    (``Control.propositionEqualityTrue, ``Control.unconditional, "restate with explicit premises"),
    (``Control.propositionEquality, ``Control.unconditional, "restate with explicit premises"),
    (``Control.nestedAnd, ``Control.unconditional, "restate with explicit premises"),
    (``Control.nestedOr, ``Control.someNat, "restate with explicit premises"),
    (``Control.nestedExists, ``Control.unconditional, "restate with explicit premises"),
    (``Control.nestedIff, ``Control.unconditional, "restate with explicit premises"),
    (``Control.contradictory, ``Control.split, "witness does not establish the joint premises"),
    (``Control.Historical.helperGrowth, ``Control.Historical.throughReachableHelper, "unsupported witness axioms"),
    (``Control.Historical.emptyArgument, ``Control.Historical.emptyThroughLet, "unsupported witness axioms"),
    (``Control.Historical.emptyArgument, ``Control.Historical.emptyThroughChain, "unsupported witness axioms"),
    (``Control.contradictory, ``Control.unsound, "unsupported witness axioms"),
    (``Control.realizable, ``Control.polymorphicWitness, "unsupported witness universe parameters"),
    (``Control.realizable, `missingWitness, "missing witness theorem"),
    (``Control.realizable, `together, "missing witness theorem")] do
    let (errors, _, covered, laws) ← audit (· == law) [(law, witness)]
    unless covered == 0 && laws == 1 && errors.size == 1 &&
        (errors[0]?.getD "").contains diagnostic do
      badDiagnostics := badDiagnostics.push s!"{law}: {errors}"
  unless badDiagnostics.isEmpty do
    throwError "negative controls: {badDiagnostics}"
  for (law, witness) in [
      (``Control.realizable, ``Control.together),
      (``Control.Historical.helperGrowth, ``Control.Historical.independentHelperWitness),
      (``ConcreteSpec.Core.bitNot_inRange, ``SpecWitness.integer)] do
    let (errors, _, covered, _) ← audit (· == law) [(law, witness)]
    unless errors.isEmpty && covered == 1 do
      throwError "positive applicability control {law}: {errors}"
  let helper := ``Control.Historical.helperGrowth
  for proof in [``Control.Historical.directHelperApplication,
      ``Control.Historical.unrelatedHelperResult] do
    let (errors, _, covered, _) ← audit (· == helper) [(helper, proof)]
    unless covered == 0 && errors.size == 1 &&
        (errors[0]?.getD "").contains "witness does not establish the joint premises" do
      throwError "result theorem used as a witness: {errors}"
  let (errors, _, covered, _) ← audit (· == helper)
    [(helper, ``Control.Historical.contradictoryHelperApplication)]
  unless covered == 0 && errors.size == 1 &&
      (errors[0]?.getD "").contains "witness does not establish the joint premises" do
    throwError "untrusted result theorem used as a witness: {errors}"
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
  let archived := [
      ``Control.Historical.Projection.subtype_proj,
      ``Control.Historical.Projection.packed_and_refl,
      ``Control.Historical.Projection.pprod_proj,
      ``Control.Historical.Projection.psigma_proj,
      ``Control.Historical.Projection.cases_bool,
      ``Control.Historical.Projection.cases_nat,
      ``Control.Historical.Projection.matcher,
      ``Control.Historical.Projection.nested_matcher,
      ``Control.Historical.Projection.mixed_minors,
      ``Control.Historical.Projection.recursor_function,
      ``Control.Historical.Projection.match_carrier,
      ``Control.Historical.Projection.psum_carrier,
      ``Control.Historical.Projection.nested_exists_field,
      ``Control.Historical.Projection.minor_function_application,
      ``Control.Historical.Projection.recursive_ih_transfer,
      ``Control.Historical.Projection.cast_let,
      ``Control.Historical.Projection.iff_field,
      ``Control.Historical.Projection.exists_carrier,
      ``Control.Historical.Projection.sigma_carrier]
  let archivedScope := (`Nonvacuity.Control.Historical.Projection).isPrefixOf
  let (errors, _, covered, laws) ← audit archivedScope (archived.map fun law => (law, law))
  unless covered == 0 && laws == archived.length && errors.size == archived.length &&
      archived.all (fun law => errors.contains s!"{law}: witness does not establish the joint premises") do
    throwError "archived proof-body probes used as evidence: {errors}"
  -- At n = 0, Nat.lt_succ_self proves the first conjunct and trivial the second.
  -- The original projection bodies therefore share a satisfiable telescope.
  let (errors, _, covered, laws) ← audit archivedScope
    (archived.map fun law => (law, ``Control.Historical.growthPremises))
  unless errors.isEmpty && covered == archived.length && laws == archived.length do
    throwError "archived probes lost their satisfiable telescope: {errors}"
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
