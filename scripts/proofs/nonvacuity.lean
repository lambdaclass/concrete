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

private def closePremises (args : Array Expr) (body : Expr) : MetaM Expr :=
  args.foldrM (fun arg result => do mkAppM ``Exists #[← mkLambdaFVars #[arg] result]) body

-- Only unfold a definition at the head. Never reduce its discriminant.
private def unfoldContractAlias (type : Expr) : MetaM (Option Expr) := do
  let .const head levels := type.getAppFn | return none
  let env ← getEnv
  if isMatcherLikeCore env head || [``ite, ``dite].contains head then return none
  let some (.defnInfo info) := env.find? head | return none
  return some (mkAppN (info.value.instantiateLevelParams info.levelParams levels) type.getAppArgs)

-- Reduce only the projection shell. In particular, keep a conditional or
-- operation in the selected field intact for applicability analysis.
private partial def reduceProjection? (expr : Expr) : MetaM (Option Expr) := withIncRecDepth do
  let head := expr.getAppFn
  let args := expr.getAppArgs
  let (subject, field, remaining) ← match head with
    | .proj _ field subject => pure (subject, field, args)
    | .const name _ => do
      let some info := (← getEnv).getProjectionFnInfo? name | return none
      let some subject := args[info.numParams]? | return none
      pure (subject, info.i, args.extract (info.numParams + 1) args.size)
    | _ => return none
  let some value ← fieldValue? subject field | return none
  return some (mkAppN value remaining)
where
  fieldValue? (subject : Expr) (field : Nat) : MetaM (Option Expr) := withIncRecDepth do
    let subject := subject.consumeMData.headBeta
    if let .letE _ _ value body _ := subject then
      return ← fieldValue? (body.instantiate1 value) field
    if let some value ← projectCore? subject field then return some value
    if let some reduced ← reduceProjection? subject then return ← fieldValue? reduced field
    if let some unfolded ← unfoldContractAlias subject then return ← fieldValue? unfolded field
    return none

/-- Classify only structurally constant arms while their discriminator is still
intact. A quantifier with a constant affirmative body can be a fallback even
when the quantified type's inhabitation is unknown. -/
private partial def literalTruth? (body : Expr) : MetaM (Option Bool) := withIncRecDepth do
  let body := body.consumeMData.headBeta
  if body.isConstOf ``True then return some true
  if body.isConstOf ``False then return some false
  if body.isForall then
    return ← forallTelescope body fun _ conclusion => do
      return if (← literalTruth? conclusion) == some true then some true else none
  if body.isAppOfArity ``Exists 2 then
    let args := body.getAppArgs
    return ← withLocalDeclD `witness args[0]! fun witness =>
      literalTruth? (mkApp args[1]! witness)
  if body.isAppOfArity ``Not 1 then
    return (← literalTruth? body.getAppArgs[0]!).map (! ·)
  if body.isAppOfArity ``And 2 || body.isAppOfArity ``Or 2 then
    let args := body.getAppArgs
    let disjunctive := body.isAppOf ``Or
    let left ← literalTruth? args[0]!
    let right ← literalTruth? args[1]!
    if left == some disjunctive || right == some disjunctive then
      return some disjunctive
    if left == some (!disjunctive) && right == some (!disjunctive) then
      return some (!disjunctive)
    return none
  if let .letE _ _ value rest _ := body then
    return ← literalTruth? (rest.instantiate1 value)
  if let some reduced ← reduceProjection? body then
    return ← literalTruth? reduced
  if let some unfolded ← unfoldContractAlias body then
    return ← literalTruth? unfolded
  return none

-- A carrier computed by a type-level match is unsupported. Unfold wrappers
-- without reducing that match to one of its potentially trivial branches.
private partial def normalizeCarrierHead (type : Expr) : MetaM Expr := withIncRecDepth do
  let type := type.consumeMData.headBeta
  if let .letE _ _ value body _ := type then
    return ← normalizeCarrierHead (body.instantiate1 value)
  if let some reduced ← reduceProjection? type then
    return ← normalizeCarrierHead reduced
  if let some unfolded ← unfoldContractAlias type then
    return ← normalizeCarrierHead unfolded
  return type

-- Inspect instantiated constructor fields, including fields in Type. An
-- unknown or higher-order carrier is refused unless its shape is inspectable.
private partial def checkDataCarrier (type : Expr) (allowAbstract : Bool)
    (seen : Array Expr := #[]) : MetaM Unit := withIncRecDepth do
  let type ← normalizeCarrierHead type
  if seen.contains type then return
  if allowAbstract && [``Nat.le, ``Nat.lt, ``Int.le, ``Int.lt].contains type.getAppFn.constName then
    return
  if type.isSort || (← isProp type) then
    throwError "unsupported proof or proposition carrier: {type}"
  let seen := seen.push type
  if type.isForall then
    return ← forallTelescope type fun fields result => do
      for field in fields do
        checkDataCarrier (← inferType field) allowAbstract seen
      checkDataCarrier result allowAbstract seen
  let head := type.getAppFn
  if allowAbstract then
    if type.isFVar then return
    if let .proj _ _ subject := type then
      if subject.isFVar then return
  let .const name levels := head
    | throwError "unsupported data carrier: {type}"
  let env ← getEnv
  let some (.inductInfo info) := env.find? name
    | throwError "unsupported data carrier: {type}"
  unless info.numIndices == 0 && type.getAppNumArgs == info.numParams do
    throwError "unsupported indexed data carrier: {type}"
  for ctor in info.ctors do
    let ctorInfo ← getConstInfoCtor ctor
    let ctorType := ctorInfo.type.instantiateLevelParams ctorInfo.levelParams levels
    let fields ← instantiateForall ctorType type.getAppArgs
    forallTelescope fields fun args _ => do
      for arg in args do
        checkDataCarrier (← inferType arg) allowAbstract seen

private partial def argumentList? (expr : Expr) : MetaM (Option Expr) := withIncRecDepth do
  let expr := expr.consumeMData.headBeta
  if expr.isAppOfArity ``ConcreteSpec.Core.Exec.evalArgs 4 then
    return expr.getAppArgs.back?
  if let .letE _ _ value body _ := expr then
    return ← argumentList? (body.instantiate1 value)
  if let some reduced ← reduceProjection? expr then
    return ← argumentList? reduced
  if expr.getAppFn.isProj then
    throwError "unsupported projection in conditional discriminator: {expr}"
  if let some unfolded ← unfoldContractAlias expr then
    return ← argumentList? unfolded
  if (expr.find? (·.isConstOf ``ConcreteSpec.Core.Exec.evalArgs)).isSome then
    throwError "unsupported argument evaluation discriminator: {expr}"
  return none

/-- Inspect the contract without reducing a match on its actual discriminant.
For a Law conclusion that chooses a witness, require the original body and
the nested applicability obligation at the very same witness. -/
private partial def jointPremises (type : Expr) (anchored : Bool := false)
    (negative : Bool := false) : MetaM Expr := withIncRecDepth do
  let type := type.consumeMData.headBeta
  if type.isForall then
    if negative then throwError "unsupported negated telescope: {type}"
    return ← forallTelescope type fun args conclusion => do
      -- Independent cases can have distinct inputs. Once a theorem has
      -- stated a shared premise (or chosen an existential), keep that
      -- subject fixed across the conjunction.
      let mut shared := anchored
      for arg in args do
        if ← isProp (← inferType arg) then shared := true
      let conclusion := conclusion.consumeMData.headBeta
      if !shared && conclusion.isAppOfArity ``And 2 then
        let left ← jointPremises conclusion.getAppArgs[0]!
        let right ← jointPremises conclusion.getAppArgs[1]!
        if !left.isConstOf ``True && !right.isConstOf ``True then
          return combinePremises (← closePremises args left) (← closePremises args right)
        return ← closePremises args (combinePremises left right)
      closePremises args (← jointPremises conclusion shared)
  if let .letE _ _ value body _ := type then
    return ← jointPremises (body.instantiate1 value) anchored negative
  if let .proj ``Membership 0 instanceValue := type.getAppFn then
    let projection ← whnf type.getAppFn
    if projection != type.getAppFn then
      return ← jointPremises (mkAppN projection type.getAppArgs) anchored negative
    throwError "unsupported membership instance: {instanceValue}"
  let .const head _ := type.getAppFn | do
    if let some reduced ← reduceProjection? type then
      return ← jointPremises reduced anchored negative
    throwError "unsupported conclusion head: {type}"
  let args := type.getAppArgs
  if head == ``Not && args.size == 1 then
    return ← jointPremises args[0]! anchored (!negative)
  if head == ``Ne && args.size == 3 then
    if (← normalizeCarrierHead args[0]!) != .sort .zero then
      checkDataCarrier args[0]! true
      return mkConst ``True
  if head == ``And && args.size == 2 then
    let left ← jointPremises args[0]! anchored negative
    let right ← jointPremises args[1]! anchored negative
    unless !negative || (left.isConstOf ``True && right.isConstOf ``True) do
      throwError "unsupported negated conjunction: {type}"
    return combinePremises left right
  if head == ``Or && args.size == 2 then
    if negative then throwError "unsupported negated disjunction: {type}"
    let mut result := mkConst ``True
    let mut substantive := false
    for arm in args do
      let premises ← jointPremises arm true
      unless (← literalTruth? arm).isSome do
        substantive := true
        result := combinePremises result (combinePremises arm premises)
    unless substantive do throwError "disjunction has no substantive arm: {type}"
    return result
  if head == ``Exists && args.size == 2 then
    if negative then throwError "unsupported negated existential: {type}"
    return ← withLocalDeclD `witness args[0]! fun witness => do
      let original := (mkApp args[1]! witness).headBeta
      let premises ← jointPremises original true
      -- A proposition-valued binder itself carries a proof. Inspect its
      -- domain as well so the proof cannot hide a vacuous contract.
      let domain ← if ← isProp args[0]! then jointPremises args[0]! true
        else do
          checkDataCarrier args[0]! false
          pure (mkConst ``True)
      return combinePremises domain (← closePremises #[witness] (combinePremises original premises))
  let propositionEq ← if head == ``Eq && args.size == 3 then
    pure ((← normalizeCarrierHead args[0]!) == .sort .zero) else pure false
  if (head == ``Iff && args.size == 2) || propositionEq then
    if negative then throwError "unsupported negated propositional equivalence: {type}"
    let left := args[args.size - 2]!
    let right := args.back!
    let premises ← combinePremises <$> jointPremises left anchored <*> jointPremises right anchored
    return combinePremises (mkApp2 (mkConst ``And) left right) premises
  if head == ``Eq && args.size == 3 then
    checkDataCarrier args[0]! true
    return mkConst ``True
  if head == ``HEq && args.size == 4 then
    checkDataCarrier args[0]! true
    checkDataCarrier args[2]! true
    return mkConst ``True
  if [``True, ``False, ``Nat.le, ``Nat.lt, ``Int.le, ``Int.lt,
      ``List.Mem, ``List.Perm].contains head then
    return mkConst ``True
  if [``LE.le, ``LT.lt].contains head && args.size == 4 &&
      (args[0]!.isConstOf ``Nat || args[0]!.isConstOf ``Int) then
    let standard := if args[0]!.isConstOf ``Nat then
      if head == ``LE.le then ``instLENat else ``instLTNat
    else
      if head == ``LE.le then ``Int.instLEInt else ``Int.instLTInt
    if ← isDefEq args[1]! (mkConst standard) then return mkConst ``True
  if head == ``List.Pairwise then
    throwError "unsupported pairwise conclusion: no evidence that the relation is exercised"
  let env ← getEnv
  let conditional := if let some info := getMatcherInfoCore? env head then
      if args.size == info.arity then
        some (info.getAltRange.toArray, info.getDiscrRange.toArray)
      else none
    else if let some (.recInfo info) := env.find? head then
      if args.size == info.getMajorIdx + 1 && info.numMotives == 1 &&
          info.numIndices == 0 && info.rules.length == info.numMinors then
        some ((List.range info.numMinors).toArray.map (info.getFirstMinorIdx + ·),
          #[info.getMajorIdx])
      else none
    else if head == ``ite && args.size == 5 then some (#[3, 4], #[1])
    else if head == ``dite && args.size == 5 then some (#[3, 4], #[1])
    else none
  if let some (arms, discriminants) := conditional then
    if negative then throwError "unsupported negated conditional: {type}"
    let mut guarded := args
    let mut substantiveArms := 0
    for i in arms do
      let (arm, substantive) ← forallTelescopeReducing (← inferType args[i]!) fun fields _ => do
        let body := (mkAppN args[i]! fields).headBeta
        unless ← isProp body do
          throwError "unsupported conditional arm: {body}"
        -- A recursive minor premise reduces through previous results.
        -- Rewriting such a premise needs an explicit applicability interface.
        if let some (.recInfo info) := env.find? head then
          if fields.size != info.rules[i - info.getFirstMinorIdx]!.nfields then
            throwError "unsupported recursive conditional conclusion: {head}"
        let premises ← jointPremises body true
        let substantive := (← literalTruth? body).isNone
        let result := if substantive then premises else mkConst ``False
        return (← mkLambdaFVars fields result, substantive)
      if substantive then substantiveArms := substantiveArms + 1
      guarded := guarded.set! i arm
    if substantiveArms > 1 then
      throwError "unsupported conditional with competing substantive arms: {type}"
    let mut result := mkAppN type.getAppFn guarded
    check result
    unless ← isProp result do throwError "unsupported conditional motive: {head}"
    if head == ``ite || head == ``dite then
      return combinePremises (← jointPremises args[1]! true) result
    for i in discriminants do
      if let some operands ← argumentList? args[i]! then
        -- A nil argument list executes neither operand semantics nor
        -- instrumentation, even when the success arm is selected.
        let .app (.const ``List _) element ← whnf (← inferType operands)
          | throwError "expected a list of operands: {operands}"
        let nil ← mkAppOptM ``List.nil #[some element]
        result := combinePremises (← mkAppM ``Ne #[operands, nil]) result
    return result
  if ((env.find? head).any fun info => match info with
      | .recInfo _ => true
      | _ => false) || isMatcherLikeCore env head || [``ite, ``dite].contains head then
    throwError "unsupported conditional conclusion: {head}"
  if let some reduced ← reduceProjection? type then
    return ← jointPremises reduced anchored negative
  if let some unfolded ← unfoldContractAlias type then
    return ← jointPremises unfolded anchored negative
  throwError "unsupported conclusion head: {head}"

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
    let mut typeAxioms : NameSet := {}
    for constant in actual.getUsedConstants do
      for axiomName in (← collectAxioms constant) do
        typeAxioms := typeAxioms.insert axiomName
    let additional := axioms.filter (!typeAxioms.contains ·)
    return some (s!"unsupported witness axioms {axioms}; " ++
      s!"type dependencies {typeAxioms.toArray.qsort Name.lt}; additional dependencies {additional}")
  return none

private inductive Evidence where
  | theoremProof (name : Name)
  | closedApplication (name : Name)

/-- Build the existential using only the actual call arguments. The application
result and the enclosing example proof supply no applicability evidence. -/
private partial def packApplication (expected : Expr) (inputs : List Expr) :
    MetaM (Option Expr) := withIncRecDepth do
  let expected ← whnf expected
  match inputs with
  | [] => return if expected.isConstOf ``True then some (mkConst ``True.intro) else none
  | input :: remaining =>
    let .const ``Exists levels := expected.getAppFn | return none
    let args := expected.getAppArgs
    let some body ← packApplication (mkApp args[1]! input) remaining | return none
    return some (mkAppN (mkConst ``Exists.intro levels) (args ++ #[input, body]))

private def checkApplication (law : TheoremVal) (expected : Expr) (name : Name) :
    MetaM (Option String) := do
  let env ← getEnv
  if (privateToUserName name).isInternal then return some "generated application source theorem"
  let some (.thmInfo source) := env.find? name | return some "missing application source theorem"
  unless source.levelParams.length == law.levelParams.length do
    return some "unsupported witness universe parameters"
  let levels := law.levelParams.map Level.param
  let proof := source.value.instantiateLevelParams source.levelParams levels
  let arity ← forallTelescope law.type fun args _ => pure args.size
  let some call := proof.find? fun e =>
    e.getAppFn == mkConst law.name levels && e.getAppNumArgs == arity && !e.hasLooseBVars
    | return some "application source has no closed call of the Law"
  let some witness ← packApplication expected call.getAppArgs.toList
    | return some "application does not establish the joint premises"
  let .ok actual := Kernel.check env {} witness
    | throwError "kernel could not check the constructed witness"
  match Kernel.isDefEq env {} actual expected with
  | .error _ => throwError "kernel could not compare the constructed witness with the joint premises"
  | .ok false => return some "application does not establish the joint premises"
  | .ok true => pure ()
  let mut axioms : NameSet := {}
  for constant in witness.getUsedConstants do
    for axiomName in ← collectAxioms constant do
      axioms := axioms.insert axiomName
  let unsupported := axioms.toArray.filter (· != ``propext)
  if unsupported.isEmpty then return none
  return some s!"unsupported application witness axioms {unsupported.qsort Name.lt}"

private def audit (inScope : Name → Bool) (witnesses : List (Name × Name))
    (applications : List (Name × Name) := []) :
    MetaM (Array String × Array String × Nat × Nat) := do
  let env ← getEnv
  let mut registrations : NameMap Evidence := {}
  let mut errors := #[]
  let mut excluded := #[]
  let mut laws := 0
  let mut covered := 0
  let evidence := (witnesses.map fun (law, name) => (law, Evidence.theoremProof name)) ++
    (applications.map fun (law, name) => (law, Evidence.closedApplication name))
  for (law, witness) in evidence do
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
        | some (.theoremProof proof) => checkWitness law expected proof
        | some (.closedApplication source) => checkApplication law expected source
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

theorem evalArgsOk :
    ∃ (O : Exec.Operands) (types : Var → Option Descriptor)
      (permit : AList Var O.Val → O.Expr → Own.State → Option Own.State)
      (locals : AList Var O.Val) (args : List O.Expr) (s : Store O.Store),
      args ≠ [] ∧ (match Exec.evalArgs (instrumentOperands O types permit) locals s args with
        | .ok _ => True
        | _ => False) :=
  ⟨argumentOps, types, permitE, [(x, 7)], [.read x, .move x], ⟨0, initial⟩,
    List.cons_ne_nil _ _, trivial⟩

theorem evalArgsTrap :
    ∃ (O : Exec.Operands) (types : Var → Option Descriptor)
      (permit : AList Var O.Val → O.Expr → Own.State → Option Own.State)
      (locals : AList Var O.Val) (args : List O.Expr) (s : Store O.Store),
      args ≠ [] ∧ (match Exec.evalArgs (instrumentOperands O types permit) locals s args with
        | .trap _ => True
        | _ => False) :=
  ⟨argumentOps, types, permitE, [(x, 7)], [.move x, .fail .overflow], ⟨0, initial⟩,
    List.cons_ne_nil _ _, trivial⟩

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

-- These source theorems supply concrete arguments to a named Law. They
-- are never counted in the Law inventory.
private def applicationWitnesses : List (Name × Name) :=
  [(``ConcreteSpec.Core.Exec.entry_refusal_report,
    ``ConcreteSpec.Core.Exec.LifecycleEvidence.entry_report_instance),
   (``ConcreteSpec.Core.Exec.evalArgs_first_trap,
    ``ConcreteSpec.Core.Exec.CleanupEvidence.evalArgs_first_trap_example),
   (``ConcreteSpec.Core.Exec.lookup_restrict,
    ``ConcreteSpec.Core.Exec.CoreEvidence.lookup_restrict_instance),
   (``ConcreteSpec.Core.Exec.mapDestroyOrder_entry,
    ``ConcreteSpec.Core.Exec.DestructionEvidence.map_entry_instance),
   (``ConcreteSpec.Core.Exec.mapDestroyOrder_storage,
    ``ConcreteSpec.Core.Exec.DestructionEvidence.map_storage_instance),
   (``ConcreteSpec.Core.Exec.refused_stops,
    ``ConcreteSpec.Core.Exec.LifecycleEvidence.refused_stop_instance),
   (``ConcreteSpec.Core.Exec.restrict_dom_drops,
    ``ConcreteSpec.Core.Exec.CoreEvidence.restrict_dom_drops_instance),
   (``ConcreteSpec.Core.Exec.seqDestroyOrder_elem,
    ``ConcreteSpec.Core.Exec.DestructionEvidence.sequence_element_instance),
   (``ConcreteSpec.Core.Exec.seqDestroyOrder_storage,
    ``ConcreteSpec.Core.Exec.DestructionEvidence.sequence_storage_instance),
   (``ConcreteSpec.Core.Exec.step_return_caller,
    ``ConcreteSpec.Core.Exec.CoreEvidence.step_return_caller_instance),
   (``ConcreteSpec.Core.Hybrid.materialize_idempotent,
    ``ConcreteSpec.Core.Hybrid.Audit.materialize_idempotent_example_secondCall),
   (``ConcreteSpec.Core.Hybrid.write_frame,
    ``ConcreteSpec.Core.Hybrid.Audit.Frame.write_frame_example_otherPlace),
   (``ConcreteSpec.Core.KeyConstruct.assignFunctionTable_named_scope,
    ``ConcreteSpec.Core.Vacuity.assignFunctionTable_named_scope_example_namedNested),
   (``ConcreteSpec.Core.KeyConstruct.assignFunctionTable_pin,
    ``ConcreteSpec.Core.Vacuity.assignFunctionTable_pin_example_nestedBody),
   (``ConcreteSpec.Core.KeyConstruct.assignFunctionTable_rename_named,
    ``ConcreteSpec.Core.Vacuity.assignFunctionTable_rename_named_example_namedCall),
   (``ConcreteSpec.Core.KeyConstruct.collectKeysIn_complete,
    ``ConcreteSpec.Core.Vacuity.collectKeysIn_sound_example_nestedBody),
   (``ConcreteSpec.Core.KeyConstruct.descendant_insert_differentKind,
    ``ConcreteSpec.Core.Vacuity.descendant_insert_differentKind_example_nestedBody),
   (``ConcreteSpec.Core.KeyConstruct.descendant_insert_named,
    ``ConcreteSpec.Core.Vacuity.descendant_insert_named_example_nestedBody),
   (``ConcreteSpec.Core.KeyConstruct.descendant_pin_other,
    ``ConcreteSpec.Core.Vacuity.descendant_pin_other_example_nestedBody),
   (``ConcreteSpec.Core.KeyConstruct.descendant_rename_other,
    ``ConcreteSpec.Core.Vacuity.descendant_rename_other_example_nestedBody),
   (``ConcreteSpec.Core.KeyConstruct.key_insert_differentKind,
    ``ConcreteSpec.Core.Vacuity.key_insert_differentKind_example_unnamedBinding),
   (``ConcreteSpec.Core.KeyConstruct.key_insert_named,
    ``ConcreteSpec.Core.Vacuity.key_insert_named_example_unnamedBinding),
   (``ConcreteSpec.Core.KeyConstruct.key_pin_other,
    ``ConcreteSpec.Core.Vacuity.key_pin_other_example_unnamedBinding),
   (``ConcreteSpec.Core.KeyConstruct.key_rename_other,
    ``ConcreteSpec.Core.Vacuity.key_rename_other_example_namedBinding),
   (``ConcreteSpec.Core.KeyConstruct.pathAt_insert_after,
    ``ConcreteSpec.Core.Vacuity.pathAt_insert_after_example_nestedBody),
   (``ConcreteSpec.Core.KeyConstruct.pathAt_insert_differentKind,
    ``ConcreteSpec.Core.Vacuity.pathAt_insert_differentKind_example_nestedBody),
   (``ConcreteSpec.Core.KeyConstruct.pathAt_insert_named,
    ``ConcreteSpec.Core.Vacuity.pathAt_insert_named_example_nestedBody),
   (``ConcreteSpec.Core.LoadedModules.load_refusal_sound,
    ``ConcreteSpec.Core.Vacuity.load_refusal_sound_example_collidingTop),
   (``ConcreteSpec.Core.Mem.access_after_free,
    ``ConcreteSpec.Core.Mem.Audit.access_after_free_example_freed),
   (``ConcreteSpec.Core.Mem.access_no_provenance,
    ``ConcreteSpec.Core.Mem.Audit.access_no_provenance_example_nonzero),
   (``ConcreteSpec.Core.Mem.access_success,
    ``ConcreteSpec.Core.Mem.Audit.access_success_example_interior),
   (``ConcreteSpec.Core.Mem.alloc_block,
    ``ConcreteSpec.Core.Mem.Audit.alloc_block_example_first),
   (``ConcreteSpec.Core.Mem.alloc_failure,
    ``ConcreteSpec.Core.Mem.Audit.alloc_failure_example_align),
   (``ConcreteSpec.Core.Mem.decodeInt_encodeInt,
    ``ConcreteSpec.Core.Mem.Audit.decodeInt_encodeInt_example_i8),
   (``ConcreteSpec.Core.Mem.decodeInt_inRange,
    ``ConcreteSpec.Core.Mem.Audit.decodeInt_inRange_example_i8),
   (``ConcreteSpec.Core.Mem.expose_load,
    ``ConcreteSpec.Core.Mem.Audit.expose_load_example_samePtr),
   (``ConcreteSpec.Core.Mem.expose_success,
    ``ConcreteSpec.Core.Mem.Audit.expose_success_example_live),
   (``ConcreteSpec.Core.Mem.load_prefix,
    ``ConcreteSpec.Core.Mem.Audit.load_prefix_example_twoToOne),
   (``ConcreteSpec.Core.Mem.store_load_other,
    ``ConcreteSpec.Core.Mem.Audit.store_load_other_example_disjointProv),
   (``ConcreteSpec.Core.Mem.store_other_block,
    ``ConcreteSpec.Core.Mem.Audit.store_other_block_example_otherBlock),
   (``ConcreteSpec.Core.SpecializationTable.canonicalize_edge_sound,
    ``ConcreteSpec.Core.Vacuity.canonicalize_edge_sound_example_edgeZero),
   (``ConcreteSpec.Core.SpecializationTable.canonicalize_nodup,
    ``ConcreteSpec.Core.Vacuity.canonicalize_nodup_example_requestZero)]

private def auditSpecification (root : System.FilePath) :
    MetaM (Array String × Array String × Nat × Nat) := do
  let env ← getEnv
  checkModuleCoverage env root
  audit (fun n =>
    (env.getModuleIdxFor? n).any (fun i =>
      env.header.moduleNames[i.toNat]?.any (fun m => m.getRoot == `ConcreteSpec))) specWitnesses applicationWitnesses

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
  ⟨contradictory, trivial⟩

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

/-- Law: independent branches each have a real input. -/
theorem disjoint (n : Nat) : (n = 0 → n = 0) ∧ (n = 1 → n = 1) :=
  ⟨id, id⟩

theorem disjointWitness :
    (∃ (n : Nat) (_ : n = 0), True) ∧ (∃ (n : Nat) (_ : n = 1), True) :=
  ⟨⟨0, rfl, trivial⟩, ⟨1, rfl, trivial⟩⟩

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
      (∃ (n : Nat) (_ : n = 0) (_ : n = 1), True)) :=
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
theorem reachableIf (n : Nat) : if n = 1 then n = n else True := by
  split
  · rfl
  · trivial

theorem reachableIfWitness : ∃ n : Nat, (if n = 1 then True else False) :=
  ⟨1, trivial⟩

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

theorem growthPremises : ∃ (n : Nat) (_ : n < n + 1 ∧ True), True :=
  ⟨2, ⟨by decide, trivial⟩, trivial⟩

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

theorem throughReachableHelper : ∃ (n : Nat) (_ : ∀ _k : Nat, n = 2), True :=
  ⟨2, fun k => Eq.trans (branchDischarge k false opaqueP hp hn)
    (branchDischarge k true opaqueP hp hn), trivial⟩

theorem independentHelperWitness : ∃ (n : Nat) (_ : ∀ _k : Nat, n = 2), True :=
  ⟨2, fun _ => rfl, trivial⟩

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
  let mut falsePasses := #[]
  for (law, witness) in [(``Control.existentialFallback, ``Control.unitFallbackWitness),
      (``Control.universalFallback, ``Control.unitFallbackWitness),
      (``Control.competingArms, ``Control.someNat),
      (``Control.vacuousGuard, ``Control.unconditional),
      (``Control.vacuousDependentGuard, ``Control.unconditional),
      (``Control.trivialDisjunct, ``Control.trivialDisjunctWitness),
      (``Control.competingDisjunct, ``Control.zeroDisjunctWitness),
      (``Control.predicateEquality, ``Control.unconditional),
      (``Control.containerEquality, ``Control.unconditional),
      (``Control.subtypeDomain, ``Control.subtypeDomain),
      (``Control.liftedDomain, ``Control.liftedDomain),
      (``Control.structureDomain, ``Control.structureDomain),
      (``Control.conditionalDomain, ``Control.unitFallbackWitness),
      (``Control.carrierInequality, ``Control.unconditional),
      (``Control.heterogeneousCarrierEquality, ``Control.unconditional),
      (``Control.emptyPairwise, ``Control.unconditional),
      (``Control.contradictoryPairwise, ``Control.emptyPairwiseWitness),
      (``Control.Historical.projectedEmptyOk, ``Control.unconditional)] do
    let (errors, _, covered, laws) ← audit (· == law) [(law, witness)]
    unless covered == 0 && laws == 1 && errors.size == 1 do
      falsePasses := falsePasses.push law
  unless falsePasses.isEmpty do
    throwError "planted false passes accepted: {falsePasses}"
  for law in [``Control.Historical.counterfeitEmptyOk, ``Control.Historical.counterfeitEmptyStore,
      ``Control.Historical.counterfeitEmptyWildcard, ``Control.Historical.counterfeitTrapBranchEmpty,
      ``Control.Historical.projectedEmptyOk] do
    let expected ← jointPremises (← inferType (mkConst law))
    let goal ← mkFreshExprMVar (mkApp (mkConst ``Not) expected)
    let (h, contradiction) ← goal.mvarId!.intro1
    contradiction.withContext do
      let nonempty ← mkAppM ``And.left #[mkFVar h]
      let premise ← whnf (← inferType nonempty)
      forallTelescope premise fun args _ => do
        let equality ← inferType args[0]!
        let reflexivity ← mkEqRefl equality.getAppArgs.back!
        contradiction.assign (mkApp nonempty reflexivity)
    let proof ← instantiateMVars goal
    unless (Kernel.check env {} proof).isOk do
      throwError "empty-list applicability obligation is not refuted: {law}"
  let mut badDiagnostics := #[]
  for (law, witness, diagnostic) in [
    (``Control.Historical.counterfeitEmptyOk, ``Control.unconditional, "witness does not establish the joint premises"),
    (``Control.Historical.counterfeitEmptyStore, ``Control.unconditional, "witness does not establish the joint premises"),
    (``Control.Historical.counterfeitEmptyWildcard, ``Control.unconditional, "witness does not establish the joint premises"),
    (``Control.Historical.counterfeitTrapBranchEmpty, ``Control.unconditional, "witness does not establish the joint premises"),
    (``ConcreteSpec.Core.ConstOrderIndependent, ``SpecWitness.constantOrderOuterOnly, "incomplete check"),
    (``Control.Historical.nonemptyWrongArm, ``Control.unconditional, "witness does not establish the joint premises"),
    (``Control.vacuousFallback, ``Control.unconditional, "witness does not establish the joint premises"),
    (``Control.detached, ``Control.detachedWitness, "witness does not establish the joint premises"),
    (``Control.detachedFunction, ``Control.detachedFunctionWitness, "witness does not establish the joint premises"),
    (``Control.proofDomain, ``Control.unconditional, "witness does not establish the joint premises"),
    (``Control.doubleNegated, ``Control.unconditional, "witness does not establish the joint premises"),
    (``Control.propositionEqualityTrue, ``Control.unconditional, "witness does not establish the joint premises"),
    (``Control.propositionEquality, ``Control.unconditional, "witness does not establish the joint premises"),
    (``Control.nestedAnd, ``Control.unconditional, "witness does not establish the joint premises"),
    (``Control.nestedAnd, ``Control.splitAnd, "witness does not establish the joint premises"),
    (``Control.nestedOr, ``Control.someNat, "witness does not establish the joint premises"),
    (``Control.nestedStructure, ``Control.unconditional, "unsupported conclusion head"),
    (``Control.nestedExists, ``Control.unconditional, "witness does not establish the joint premises"),
    (``Control.nestedIff, ``Control.unconditional, "witness does not establish the joint premises"),
    (``Control.contradictory, ``Control.split, "witness does not establish the joint premises"),
    (``Control.Historical.helperGrowth, ``Control.Historical.throughReachableHelper, "unsupported witness axioms"),
    (``Control.Historical.emptyArgument, ``Control.Historical.emptyThroughLet, "unsupported witness axioms"),
    (``Control.Historical.emptyArgument, ``Control.Historical.emptyThroughChain, "unsupported witness axioms"),
    (``Control.contradictory, ``Control.unsound, "unsupported witness axioms"),
    (``Control.hidden, ``Control.unconditional, "witness does not establish the joint premises"),
    (``Control.opaqueContract, ``Control.unconditional, "unsupported conclusion head"),
    (``Control.realizable, ``Control.polymorphicWitness, "unsupported witness universe parameters"),
    (``Control.realizable, `missingWitness, "missing witness theorem")] do
    let (errors, _, _, _) ← audit (· == law) [(law, witness)]
    unless errors.size == 1 && (errors[0]?.getD "").contains diagnostic do
      badDiagnostics := badDiagnostics.push s!"{law}: {errors}"
  unless badDiagnostics.isEmpty do
    throwError "negative controls: {badDiagnostics}"
  for (law, witness) in [(``Control.attached, ``Control.attachedWitness),
      (``Control.reachableIf, ``Control.reachableIfWitness),
      (``Control.reachableDisjuncts, ``Control.reachableDisjunctsWitness),
      (``Control.disjoint, ``Control.disjointWitness),
      (``Control.Historical.helperGrowth, ``Control.Historical.independentHelperWitness),
      (``ConcreteSpec.Core.Own.evalArgs_ok_erasure, ``SpecWitness.evalArgsOk),
      (``ConcreteSpec.Core.Own.evalArgs_trap_erasure, ``SpecWitness.evalArgsTrap),
      (``ConcreteSpec.Core.bitNot_inRange, ``SpecWitness.integer)] do
    let (errors, _, covered, _) ← audit (· == law) [(law, witness)]
    unless errors.isEmpty && covered == 1 do
      throwError "positive applicability control {law}: {errors}"
  let helper := ``Control.Historical.helperGrowth
  let (errors, _, covered, _) ← audit (· == helper) []
    [(helper, ``Control.Historical.directHelperApplication)]
  unless errors.isEmpty && covered == 1 do
    throwError "closed application control: {errors}"
  for (source, diagnostic) in [
    (``Control.Historical.contradictoryHelperApplication, "unsupported application witness axioms"),
    (``Control.Historical.unrelatedHelperResult, "no closed call of the Law")] do
    let (errors, _, covered, _) ← audit (· == helper) [] [(helper, source)]
    unless covered == 0 && errors.size == 1 && (errors[0]?.getD "").contains diagnostic do
      throwError "closed application refusal {source}: {errors}"
  let member := ``ConcreteSpec.Core.KeyConstruct.collectKeysIn_complete
  let (errors, _, covered, _) ← audit (· == member) [] (applicationWitnesses.filter (·.1 == member))
  unless errors.isEmpty && covered == 1 do
    throwError "list membership applicability control: {errors}"
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
