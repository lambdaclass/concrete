import Concrete.Interfaces.Pipeline
import Lean.Elab.Command

namespace EndToEndAudit
open Concrete.Interfaces ConcreteSpec.StageInterface ConcreteSpec.StageGrammar
open Lean Elab Command

private def modelTypes : List Name :=
  [``CoreLanguage, ``ConcreteSpec.Core.Exec.Operands, ``LoweringLanguage, ``SharedGrammar,
    ``TableVerifier, ``VerificationInputs, ``ParserEngine, ``Parser, ``CheckedTable,
    ``Concrete.Contracts.Semantics, ``Stage, ``Pipeline, ``Registry]

private def dropBinders : Nat → Expr → Expr
  | n + 1, .forallE _ _ body _ => dropBinders n body
  | _, type => type

private partial def resultIsType (env : Environment) (seen : Std.HashSet Name := {}) : Expr → Bool
  | .forallE _ _ body _ => resultIsType env seen body
  | .mdata _ body => resultIsType env seen body
  | .letE _ _ value body _ => resultIsType env seen (body.instantiate1 value).headBeta
  | .sort (.succ _) => true
  | e =>
    let e := e.headBeta
    match e.getAppFn.constName? with
    | some name =>
      if seen.contains name then false
      else match env.find? name with
        | some (.defnInfo info) =>
          resultIsType env (seen.insert name) ((mkAppN info.value e.getAppArgs).headBeta)
        | _ => false
    | none => false

private def instantiateBinders (count : Nat) (type : Expr) (args : Array Expr) : Expr :=
  go count 0 type
where
  go : Nat → Nat → Expr → Expr
    | 0, _, e => e
    | rem + 1, idx, .forallE _ _ body _ =>
      if h : idx < args.size then
        go rem (idx + 1) (body.instantiate1 args[idx]).headBeta
      else
        go rem (idx + 1) body
    | _ + 1, _, e => e

private partial def modelBinder (env : Environment) (type : Expr) (functionType := false)
    (seen : Std.HashSet Name := {}) (forbidAnyTypeFn := false) : Bool :=
  let type := type.headBeta
  match type with
  | .mdata _ body => modelBinder env body functionType seen forbidAnyTypeFn
  | .letE _ _ value body _ =>
    modelBinder env (body.instantiate1 value).headBeta functionType seen forbidAnyTypeFn
  | .forallE _ _ body _ => modelBinder env body true seen forbidAnyTypeFn
  | .lam _ _ body _ => modelBinder env body false seen forbidAnyTypeFn
  | .sort _ => true
  | _ =>
    match type.getAppFn.constName? with
    | none => false
    | some name =>
      let args := type.getAppArgs
      modelTypes.contains name ||
        (functionType && [``Bool, ``Nat, ``UInt8, ``ByteArray, ``List, ``Option, ``Except,
          ``ConcreteSpec.Core.Res].contains name) ||
        if seen.contains name then false else
          let seen' := seen.insert name
          match env.find? name with
          | some (.defnInfo info) =>
            modelBinder env ((mkAppN info.value args).headBeta) functionType seen' forbidAnyTypeFn
          | some (.recInfo info) =>
            let minorStart := info.numParams + info.numMotives
            let minorStop := minorStart + info.numMinors
            (List.range info.numMinors).any fun offset =>
              let idx := minorStart + offset
              if h : idx < args.size then
                idx < minorStop && modelBinder env args[idx] false seen' forbidAnyTypeFn
              else false
          | some (.inductInfo info) =>
            -- PassFunctions has a separately checked executable schema.
            -- Behavior carries observations, including trace data indexed by time.
            ![``PassFunctions, ``Behavior].contains name && resultIsType env {} info.type &&
              ((functionType && forbidAnyTypeFn && ![``Unit, ``PUnit, ``Empty, ``PEmpty].contains name) ||
                appliedTypeParamsCarryModel env info.numParams 0 info.type args seen forbidAnyTypeFn ||
                indexDomainsCarryModel env info.numParams
                  (instantiateBinders info.numParams info.type args) args seen' forbidAnyTypeFn ||
                info.ctors.any fun ctor =>
                  match env.find? ctor with
                  | some (.ctorInfo ctor) =>
                    fieldsCarryModel env (instantiateBinders ctor.numParams ctor.type args)
                      seen' forbidAnyTypeFn
                  | _ => false)
          | _ => false
where
  appliedTypeParamsCarryModel (env : Environment) (numParams idx : Nat) (e : Expr)
      (args : Array Expr) (seen : Std.HashSet Name) (forbidAnyTypeFn : Bool) : Bool :=
    if idx < numParams then
      match e with
      | .forallE _ paramDomain body _ =>
        if h : idx < args.size then
          let arg := args[idx]
          let argCarries :=
            if resultIsType env {} paramDomain then
              modelBinder env arg false seen forbidAnyTypeFn
            else false
          argCarries ||
            appliedTypeParamsCarryModel env numParams (idx + 1)
              (body.instantiate1 arg).headBeta args seen forbidAnyTypeFn
        else false
      | _ => false
    else false
  indexDomainsCarryModel (env : Environment) (idx : Nat) (e : Expr)
      (args : Array Expr) (seen : Std.HashSet Name) (forbidAnyTypeFn : Bool) : Bool :=
    match e with
    | .forallE _ indexDomain body _ =>
      modelBinder env indexDomain false seen forbidAnyTypeFn ||
        let next :=
          if h : idx < args.size then (body.instantiate1 args[idx]).headBeta else body
        indexDomainsCarryModel env (idx + 1) next args seen forbidAnyTypeFn
    | _ => false
  fieldsCarryModel (env : Environment) (type : Expr) (seen : Std.HashSet Name)
      (forbidAnyTypeFn : Bool) : Bool :=
    match type with
    | .forallE _ field body _ =>
      modelBinder env field false seen forbidAnyTypeFn ||
        fieldsCarryModel env body seen forbidAnyTypeFn
    | _ => false

private partial def passTypeCarriesModel (env : Environment) (type : Expr)
    (seen : Std.HashSet Name := {}) : Bool :=
  let type := type.headBeta
  match type with
  | .mdata _ body => passTypeCarriesModel env body seen
  | .letE _ _ value body _ => passTypeCarriesModel env (body.instantiate1 value).headBeta seen
  | .forallE _ domain body _ =>
    modelBinder env domain false {} true || passTypeCarriesModel env body seen
  | type =>
    match type.getAppFn.constName? with
    | some name =>
      if !seen.contains name then
        match env.find? name with
        | some (.defnInfo info) =>
          let unfolded := (mkAppN info.value type.getAppArgs).headBeta
          if unfolded.isForall then
            passTypeCarriesModel env unfolded (seen.insert name)
          else
            modelBinder env type false {} true ||
              type.getAppArgs.any (passTypeCarriesModel env · seen)
        | _ =>
          modelBinder env type false {} true ||
            type.getAppArgs.any (passTypeCarriesModel env · seen)
      else
        modelBinder env type false {} true
    | none =>
      modelBinder env type false {} true ||
        type.getAppArgs.any (passTypeCarriesModel env · seen)

private def badPassField (env : Environment) (record : Name) : Option Name := do
  let structureInfo ← getStructureInfo? env record
  let some (.inductInfo recordInfo) := env.find? record | none
  structureInfo.fieldNames.find? fun field =>
    match env.find? (record ++ field) with
    | some (.defnInfo projection) =>
      passTypeCarriesModel env (dropBinders (recordInfo.numParams + 1) projection.type)
    | _ => true

private partial def resultIsProp : Expr → Bool
  | .forallE _ _ body _ => resultIsProp body
  | .sort .zero => true
  | _ => false

private def usedInConstructorResult : Expr → Nat → Bool
  | .forallE _ _ body _, index => usedInConstructorResult body (index + 1)
  | result, index => result.hasLooseBVar index

private def hasNamePart (accepts : String → Bool) : Name → Bool
  | .anonymous => false
  | .num rest _ => hasNamePart accepts rest
  | .str rest part => accepts part || hasNamePart accepts rest

/-- Follow the definitions on both branches of the registered pipeline,
including contracts under structure fields and aliases of model types. -/
private partial def inspect (env : Environment) (origin : Name) (expr : Expr) :
    StateM (Std.HashSet Name) (Option (Name × Name)) := do
  -- A negated existential forbids every possible witness. Its predicate's
  -- lambda binds the decoder even when Exists has no explicit forall in the AST.
  let negativeDomain := if expr.isAppOfArity ``Not 1 then some expr.appArg!
    else match expr with
      | .forallE _ domain body _ => if body.isConstOf ``False then some domain else none
      | _ => none
  if let some domain := negativeDomain then
    if domain.isAppOfArity ``Exists 2 && modelBinder env domain.getAppArgs[0]! then
      let binder := match domain.appArg! with | .lam n .. => n | _ => `witness
      return some (origin, binder)
  match expr with
  | .forallE name domain body _ =>
    if modelBinder env domain then return some (origin, name)
    return (← inspect env origin domain) <|> (← inspect env origin body)
  | .lam _ domain body _ =>
    return (← inspect env origin domain) <|> (← inspect env origin body)
  | .letE _ type value body _ =>
    return (← inspect env origin type) <|> (← inspect env origin value) <|> (← inspect env origin body)
  | .app fn arg => return (← inspect env origin fn) <|> (← inspect env origin arg)
  | .mdata _ body | .proj _ _ body => inspect env origin body
  | .const name _ =>
    let root := privateToUserName name
    -- Generated eliminators quantify over motives and their arguments. The
    -- source branches are already traversed as arguments at each call site.
    if hasNamePart (fun part =>
        ["brecOn", "below", "recOn", "casesOn"].contains part || "match_".isPrefixOf part) root then
      return none
    unless [`Concrete, `ConcreteSpec, `EndToEndAudit].any (·.isPrefixOf root) do
      return none
    if (← get).contains name then return none
    modify (·.insert name)
    match env.find? name with
    | some (.defnInfo info) => inspect env root info.value
    | some (.inductInfo info) =>
      if resultIsProp info.type then
        for ctor in info.ctors do
          if let some (.ctorInfo ctorInfo) := env.find? ctor then
            if let some hit ← inspectConstructor env root (dropBinders ctorInfo.numParams ctorInfo.type) then
              return some hit
      return none
    | _ => return none
  | _ => return none
where
  inspectConstructor (env : Environment) (origin : Name) (type : Expr) :
      StateM (Std.HashSet Name) (Option (Name × Name)) := do
    match type with
    | .forallE name domain body _ =>
      -- The primitive continuation is fixed by the dispatch equality in Step.prim.
      let dispatchContinuation := origin == ``ConcreteSpec.Core.Exec.Step && name == `k
      if !dispatchContinuation && !usedInConstructorResult body 0 && modelBinder env domain then
        return some (origin, name)
      return (← inspect env origin domain) <|> (← inspectConstructor env origin body)
    | _ => return none

private def check (env : Environment) (type : Expr) : Option (Name × Name) :=
  ((inspect env `endToEnd type).run {}).1

-- Ties the audited field to the actual premise of the composition theorem.
example {A R B : Type} (edge : Stage A R B) :
    edge.Trusted = (∀ a b, edge.inputInvariant a → Concrete.Contracts.Graph edge.run a b →
      edge.trust a b) := rfl

example {A : Type} : (Pipeline.done (A := A)).Trusted = True := rfl

example {A R B C : Type} (edge : Stage A R B) (rest : Pipeline B C) :
    (Pipeline.next edge rest).Trusted = (edge.Trusted ∧ rest.Trusted) := rfl

/-- Read the trust field on the composed edges. The final constructor fixes
both the BIR input and the backend output at this slice and profile. -/
private inductive FixedTrust (slice : Slice) (profile : Profile) :
    {A B : Type} → Pipeline A B → Prop
  | llvm {edge : Stage (BIR (sliceLoweringLanguage slice) profile) StageFailure (LLVMInput profile)} :
      edge.kind = .namedPremise "P_print" →
      edge.trust = @P_print (sliceLanguage slice) (sliceLoweringLanguage slice) profile →
      FixedTrust slice profile (.next edge .done)
  | lcb {edge : Stage (BIR (sliceLoweringLanguage slice) profile) StageFailure (LCBInput profile)} :
      edge.kind = .directProof → edge.trust = (fun _ _ => True) →
      FixedTrust slice profile (.next edge .done)
  | step {A R B C} {edge : Stage A R B} {rest : Pipeline B C} :
      (edge.kind = .directProof ∨ edge.kind = .provedValidator) →
      edge.trust = (fun _ _ => True) → FixedTrust slice profile rest →
      FixedTrust slice profile (.next edge rest)

example (slice : Slice) (profile : Profile) (passes : Passes slice profile) (unchecked : Bool) :
    FixedTrust slice profile (Registry.pipeline passes unchecked) ∧
    FixedTrust slice profile ((registry passes).lcbPipeline unchecked) := by
  obtain ⟨backend, target, slots, bytes⟩ := profile
  cases backend <;> cases unchecked <;> constructor <;>
    repeat' first
      | exact FixedTrust.llvm rfl rfl
      | exact FixedTrust.lcb rfl rfl
      | apply FixedTrust.step (Or.inl rfl) rfl
      | apply FixedTrust.step (Or.inr rfl) rfl

-- This mutation hides a caller-selected model inside an unfolded relation.
private abbrev Model := CoreLanguage

private def plantedStatement (profile : Profile) (passes : Passes slice0 profile) : Prop :=
  let lexer := {(registry passes).lexer with
    trust := fun _ _ => ∀ model : Model, model = model}
  let pipeline := Pipeline.next lexer .done
  pipeline.Trusted

private def plantedRelation (profile : Profile) (passes : Passes slice0 profile) : Prop :=
  let lexer := {(registry passes).lexer with
    success := fun source stream =>
      ∀ meaning : Source → TokenStream → Prop, meaning source stream}
  ∀ source stream, (Pipeline.next lexer .done).Corresponds source stream

private def plantedCodec (profile : Profile) (passes : Passes slice0 profile) : Prop :=
  let lexer := {(registry passes).lexer with
    trust := fun _ _ => ∀ decode : ByteArray → Option UInt8, decode ByteArray.empty = none}
  (Pipeline.next lexer .done).Trusted

private structure WrappedModel where
  decode : ByteArray → Option UInt8

private def plantedWrapped : Prop := ∀ wrapped : WrappedModel, wrapped = wrapped

-- A data-valued quantified function in a logical premise remains caller-selectable.
private def plantedDataBinder : Prop := Nonempty ((decode : ByteArray → Option UInt8) → PLift (decode ByteArray.empty = none))

-- Identity can also carry a quantified type as its value. That binder is open.
private def plantedCodecIdentity : Prop :=
  Nonempty (id ((decode : ByteArray → Option UInt8) → PLift (decode ByteArray.empty = none)))

private def plantedCodecPair : Prop :=
  Nonempty (Prod.fst (((decode : ByteArray → Option UInt8) → PLift (decode ByteArray.empty = none)), Nat))

private def plantedCodecOption : Prop :=
  Nonempty ((some ((decode : ByteArray → Option UInt8) → PLift (decode ByteArray.empty = none))).getD PUnit)

private def plantedCodecList : Prop :=
  Nonempty ([((decode : ByteArray → Option UInt8) → PLift (decode ByteArray.empty = none))].headD PUnit)

private def plantedModelPair : Prop :=
  Nonempty (Prod.fst (((model : Model) → PLift (model = model)), Nat))

private def plantedModelOption : Prop :=
  Nonempty ((some ((model : Model) → PLift (model = model))).getD PUnit)

private def plantedModelList : Prop :=
  Nonempty ([((model : Model) → PLift (model = model))].headD PUnit)

private inductive PlantedInductive : Prop where
  | intro (evidence : ∀ decode : ByteArray → Option UInt8, decode ByteArray.empty = none)

private inductive PlantedConstructor : Prop where
  | intro (decode : ByteArray → Option UInt8)

private structure PlantedPassBundle where
  run : Source → Except LexRefusal WrappedModel

-- Indexed type-family controls (ISS-0114): hidden decoder, model-valued index,
-- polymorphic family instantiated with a model, alias, nested executable results,
-- and genuine data-only indexed families (custom and registered) that must pass.
private inductive PlantedIndexedDecoder : Nat → Type where
  | intro (decode : ByteArray → Option UInt8) : PlantedIndexedDecoder 0

private def plantedIndexedDecoder : Prop :=
  ∀ carrier : PlantedIndexedDecoder 0, carrier = carrier

private abbrev PlantedIndexedAlias (n : Nat) := PlantedIndexedDecoder n

private def plantedIndexedAlias : Prop :=
  ∀ carrier : PlantedIndexedAlias 0, carrier = carrier

private inductive PlantedModelIndex : (ByteArray → Option UInt8) → Type where
  | unit : PlantedModelIndex (fun _ => none)

private def plantedModelIndex : Prop :=
  ∀ carrier : PlantedModelIndex (fun _ => none), carrier = carrier

private inductive PlantedCoreIndex : CoreLanguage → Type where
  | unit : PlantedCoreIndex (sliceLanguage slice0)

private def plantedCoreIndex : Prop :=
  ∀ carrier : PlantedCoreIndex (sliceLanguage slice0), carrier = carrier

private inductive DataIndexedFamily.{u} (α : Type u) : Nat → Type u where
  | zero : DataIndexedFamily α 0
  | cons {n : Nat} (head : α) (tail : DataIndexedFamily α n) : DataIndexedFamily α (n + 1)

private def plantedPolyIndexedModel : Prop :=
  ∀ carrier : DataIndexedFamily CoreLanguage 1, carrier = carrier

private def plantedPolyIndexedNested : Prop :=
  ∀ carrier : DataIndexedFamily (DataIndexedFamily (ByteArray → Option UInt8) 0) 1,
    carrier = carrier

private def plantedPolyIndexedAlias : Prop :=
  ∀ carrier : DataIndexedFamily Model 0, carrier = carrier

private def plantedMatchIndexed : Bool → Type
  | true => Nat
  | false => ByteArray → Option UInt8

private def plantedMatchIndexedProp (flag : Bool) : Prop :=
  ∀ carrier : plantedMatchIndexed flag, carrier = carrier

private structure PlantedIndexedPassBundle where
  run : Source → Except LexRefusal (Option (PlantedIndexedDecoder 0))

private structure PlantedPolyIndexedPassBundle where
  run : Source → Except LexRefusal (List (DataIndexedFamily WrappedModel 1))

private structure PlantedModelIndexPassBundle where
  run : Source → Except LexRefusal (PlantedModelIndex (fun _ => none))

private structure PlantedNestedFnPassBundle where
  run : Source → Except LexRefusal (Option (Source → TokenStream))

private abbrev PassResultAlias (α : Type) := Source → Except LexRefusal α

private structure PlantedAliasPassBundle where
  run : PassResultAlias (PlantedIndexedDecoder 0)

private def passingDataIndexed : Prop :=
  ∀ carrier : DataIndexedFamily Nat 2, carrier = carrier

private def passingRegisteredIndexed (ty : (sliceLanguage slice0).operands.Ty)
    (tys : List (sliceLanguage slice0).operands.Ty) : Prop :=
  ∀ (r : Read (sliceLanguage slice0).operands.Ty (sliceLoweringLanguage slice0).literal ty)
    (args : Args (Read (sliceLanguage slice0).operands.Ty (sliceLoweringLanguage slice0).literal) tys),
    r = r ∧ args = args

private structure PassingDataIndexedPassBundle where
  run : Source → Except LexRefusal (DataIndexedFamily UInt8 3)

private structure PassingAliasPassBundle where
  run : PassResultAlias (DataIndexedFamily UInt8 3)

-- The explicit schema also catches unstructured relation parameters at the
-- boundary, even if their types are never declared as semantic model records.
example {slice : Slice} {profile : Profile} (passes : Passes slice profile) (unchecked : Bool)
    {input : Source} {output : Registry.Output profile}
    (validated : (Registry.pipeline passes unchecked).Validated)
    (trusted : (Registry.pipeline passes unchecked).Trusted)
    (ready : (Registry.pipeline passes unchecked).Ready input)
    (ran : (Registry.pipeline passes unchecked).run input = .ok output) :
    (Registry.pipeline passes unchecked).Corresponds input output :=
  @Pipeline.endToEnd profile slice passes unchecked input output validated trusted ready ran

example {slice : Slice} {profile : Profile} (passes : Passes slice profile) (unchecked : Bool)
    {input : Source} {output : LCBInput profile}
    (validated : ((registry passes).lcbPipeline unchecked).Validated)
    (ready : ((registry passes).lcbPipeline unchecked).Ready input)
    (ran : ((registry passes).lcbPipeline unchecked).run input = .ok output) :
    ((registry passes).lcbPipeline unchecked).Corresponds input output :=
  @registeredLCBEndToEnd profile slice passes unchecked input output validated ready ran

run_cmd do
  let env ← getEnv
  -- A model packaged inside the pass argument would still be caller-selected.
  let fields := #[`lex, `elaborate, `checkCore, `lowerCore, `lowerUnchecked, `lowerMemory,
    `buildSSA, `buildBIR, `emitLLVM, `emitLCB]
  unless (getStructureInfo? env ``PassFunctions).map (·.fieldNames) == some fields do
    throwError "The pass argument must contain only the registered executable functions"
  if let some field := badPassField env ``PassFunctions then
    throwError "The registered pass field {field} contains an open semantic model"
  for name in [``Pipeline.endToEnd, ``registeredLCBEndToEnd] do
    let some theoremInfo := env.find? name
      | throwError "Missing registered end-to-end theorem {name}"
    if let some (origin, binder) := check env theoremInfo.type then
      throwError "End-to-end statement has an open semantic binder {binder} in {origin}"
  let some soundInfo := env.find? ``Stage.Sound
    | throwError "Missing registered stage soundness"
  if let some (origin, binder) := check env soundInfo.type <|> check env (.const ``Stage.Sound []) then
    throwError "Stage soundness has an open semantic binder {binder} in {origin}"
  -- Each wrapper extracts the original quantified type inside Nonempty.
  -- Check the actual binder so unrelated findings cannot pass the controls.
  for (name, binder) in [(``plantedDataBinder, `decode),
      (``plantedCodecIdentity, `decode),
      (``plantedCodecPair, `decode),
      (``plantedCodecOption, `decode),
      (``plantedCodecList, `decode),
      (``plantedModelPair, `model),
      (``plantedModelOption, `model),
      (``plantedModelList, `model)] do
    unless check env (.const name []) == some (privateToUserName name, binder) do
      throwError "End-to-end audit accepted a caller-selected binder in {name}"
  unless (check env (.const ``plantedStatement []) == some (`EndToEndAudit.plantedStatement, `model)) do
    throwError "End-to-end audit accepted a planted semantic model parameter"
  unless (check env (.const ``plantedRelation []) == some (`EndToEndAudit.plantedRelation, `meaning)) do
    throwError "End-to-end audit accepted a planted relation parameter"
  unless (check env (.const ``plantedCodec []) == some (`EndToEndAudit.plantedCodec, `decode)) do
    throwError "End-to-end audit accepted a planted codec parameter"
  unless (check env (.const ``plantedWrapped []) == some (`EndToEndAudit.plantedWrapped, `wrapped)) do
    throwError "End-to-end audit accepted a planted wrapped decoder"
  unless (check env (.const ``PlantedInductive []) == some (`EndToEndAudit.PlantedInductive, `decode)) do
    throwError "End-to-end audit accepted a planted decoder under an inductive constructor"
  unless (check env (.const ``PlantedConstructor []) == some (`EndToEndAudit.PlantedConstructor, `decode)) do
    throwError "End-to-end audit accepted a planted decoder as an inductive constructor field"
  unless badPassField env ``PlantedPassBundle == some `run do
    throwError "End-to-end audit accepted a planted decoder in the executable pass bundle"
  -- Indexed type-family controls (ISS-0114): each planted indexed family must fail
  -- with its exact open semantic binder, and genuine data-only indexed families must pass.
  for (name, binder) in [(``plantedIndexedDecoder, `carrier),
      (``plantedIndexedAlias, `carrier),
      (``plantedModelIndex, `carrier),
      (``plantedCoreIndex, `carrier),
      (``plantedPolyIndexedModel, `carrier),
      (``plantedPolyIndexedNested, `carrier),
      (``plantedPolyIndexedAlias, `carrier),
      (``plantedMatchIndexedProp, `carrier)] do
    unless check env (.const name []) == some (privateToUserName name, binder) do
      throwError "End-to-end audit accepted a planted indexed semantic family in {name}"
  for bundle in [``PlantedIndexedPassBundle, ``PlantedPolyIndexedPassBundle,
      ``PlantedModelIndexPassBundle, ``PlantedNestedFnPassBundle, ``PlantedAliasPassBundle] do
    unless badPassField env bundle == some `run do
      throwError "End-to-end audit accepted a planted indexed family in executable pass bundle {bundle}"
  for passing in [``passingDataIndexed, ``passingRegisteredIndexed] do
    if let some (origin, binder) := check env (.const passing []) then
      throwError "End-to-end audit rejected genuine data-only indexed family {passing} ({binder} in {origin})"
  for bundle in [``PassingDataIndexedPassBundle, ``PassingAliasPassBundle] do
    if let some field := badPassField env bundle then
      throwError "End-to-end audit rejected genuine data-only indexed pass bundle {bundle} at field {field}"

end EndToEndAudit
