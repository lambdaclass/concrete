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

private partial def modelBinder (env : Environment) (type : Expr) (functionType := false)
    (seen : Std.HashSet Name := {}) : Bool :=
  match type with
  | .forallE _ _ body _ => modelBinder env body true seen
  | .sort _ => true
  | _ =>
    match type.getAppFn.constName? with
    | none => false
    | some name =>
      modelTypes.contains name ||
        (functionType && [``Bool, ``Nat, ``UInt8, ``ByteArray, ``List, ``Option, ``Except,
          ``ConcreteSpec.Core.Res].contains name) ||
        if seen.contains name then false else
          let seen := seen.insert name
          match env.find? name with
          | some (.defnInfo info) =>
            modelBinder env ((mkAppN info.value type.getAppArgs).headBeta) functionType seen
          | some (.inductInfo info) =>
            -- PassFunctions has a separately checked executable schema.
            -- Behavior carries observations, including trace data indexed by time.
            ![``PassFunctions, ``Behavior].contains name && !(dropBinders info.numParams info.type).isForall &&
              (match dropBinders info.numParams info.type with | .sort (.succ _) => true | _ => false) &&
              info.ctors.any fun ctor =>
                match env.find? ctor with
                | some (.ctorInfo ctor) => fieldsCarryModel env (dropBinders ctor.numParams ctor.type) seen
                | _ => false
          | _ => false
where
  fieldsCarryModel (env : Environment) (type : Expr) (seen : Std.HashSet Name) : Bool :=
    match type with
    | .forallE _ field body _ => modelBinder env field false seen || fieldsCarryModel env body seen
    | _ => false

private partial def passTypeCarriesModel (env : Environment) (type : Expr) : Bool :=
  match type with
  | .forallE _ domain body _ =>
    modelBinder env domain || passTypeCarriesModel env body
  | type => modelBinder env type || type.getAppArgs.any (passTypeCarriesModel env)

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
  match expr with
  | .forallE name domain body _ =>
    if modelBinder env domain then return some (origin, name)
    return (← inspect env origin domain) <|> (← inspect env origin body)
  | .lam _ domain body _ =>
    return (← inspect env origin domain) <|> (← inspect env origin body)
  | .letE _ type value body _ =>
    return (← inspect env origin type) <|> (← inspect env origin value) <|> (← inspect env origin body)
  | .app fn arg =>
    -- The type annotation on an identity application describes its supplied
    -- value. Inspect that value even when it is itself a quantified type.
    if expr.isAppOfArity ``id 2 then return ← inspect env origin arg
    return (← inspect env origin fn) <|> (← inspect env origin arg)
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

private def openModelBinder (env : Environment) (type : Expr) : Option (Name × Name) :=
  ((inspect env `endToEnd type).run {}).1

-- Ties the audited field to the actual premise of the composition theorem.
example {A R B : Type} (edge : Stage A R B) :
    edge.Trusted = (∀ a b, edge.inputInvariant a → Concrete.Contracts.Graph edge.run a b →
      edge.trust a b) := rfl

example {A : Type} : (Pipeline.done (A := A)).Trusted = True := rfl

example {A R B C : Type} (edge : Stage A R B) (rest : Pipeline B C) :
    (Pipeline.next edge rest).Trusted = (edge.Trusted ∧ rest.Trusted) := rfl

/-- The registered slice fixes the only permitted trust fields. The LLVM
premise refers to this slice's language and lowering and this exact profile. -/
private inductive AllowedTrust (slice : Slice) (profile : Profile) :
    {A B : Type} → Concrete.Contracts.EvidenceKind → (A → B → Prop) → Prop
  | direct {A B} : AllowedTrust slice profile .directProof (fun (_ : A) (_ : B) => True)
  | validator {A B} : AllowedTrust slice profile .provedValidator (fun (_ : A) (_ : B) => True)
  | llvm : AllowedTrust slice profile (.namedPremise "P_print")
      (@P_print (sliceLanguage slice) (sliceLoweringLanguage slice) profile)

private def permittedTrust (slice : Slice) (profile : Profile) {A B : Type} : Pipeline A B → Prop
  | .done => True
  | .next edge rest => AllowedTrust slice profile edge.kind edge.trust ∧ permittedTrust slice profile rest

-- Closing this goal uses only constructor application and definitional equality.
local macro "audit_trust" : tactic =>
  `(tactic|
    (repeat' first
      | apply And.intro
      | apply AllowedTrust.direct
      | apply AllowedTrust.validator
      | apply AllowedTrust.llvm
      | exact True.intro) <;> done)

example (slice : Slice) (profile : Profile) (passes : Passes slice profile) (unchecked : Bool) :
    permittedTrust slice profile (Registry.pipeline passes unchecked) ∧
    permittedTrust slice profile ((registry passes).lcbPipeline unchecked) := by
  obtain ⟨backend, target, slots, bytes⟩ := profile
  cases backend <;> cases unchecked <;> audit_trust

-- Replacing the lexer's trust in a one-edge pipeline exercises the same
-- obligation as the full pipeline. A definitionally wrapped True is allowed.
private def lexerTrust (profile : Profile) (passes : Passes slice0 profile) (premise : Prop) :=
  Pipeline.next {(registry passes).lexer with trust := fun _ _ => premise} .done

private abbrev Decoder := ByteArray → Option UInt8

example (profile : Profile) (passes : Passes slice0 profile) :
    permittedTrust slice0 profile (lexerTrust profile passes (id True)) := by
  let viaId := ¬ (id Exists) (fun (decode : Decoder) => decode ByteArray.empty ≠ none)
  let viaLet := let witness := ∃ decode : Decoder, decode ByteArray.empty ≠ none; ¬ witness
  let viaPair := (True, ¬ ∃ decode : Decoder, decode ByteArray.empty ≠ none).2
  have : viaId = viaLet := rfl
  have : viaId = viaPair := rfl
  have : ¬ viaId := fun h => h ⟨fun _ => some 0, by decide⟩
  fail_if_success have : permittedTrust slice0 profile (lexerTrust profile passes viaId) := by audit_trust
  fail_if_success have : permittedTrust slice0 profile (lexerTrust profile passes viaLet) := by audit_trust
  fail_if_success have : permittedTrust slice0 profile (lexerTrust profile passes viaPair) := by audit_trust
  audit_trust

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

-- The argument of id is an ordinary function type; the supplied function is fixed.
private def closedCodec : Prop :=
  (id (α := (ByteArray → Option UInt8) → Option UInt8) (fun decode => decode ByteArray.empty))
    (fun _ => none) = none

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
    if let some (origin, binder) := openModelBinder env theoremInfo.type then
      throwError "End-to-end statement has an open semantic binder {binder} in {origin}"
  let some soundInfo := env.find? ``Stage.Sound
    | throwError "Missing registered stage soundness"
  if let some (origin, binder) := openModelBinder env soundInfo.type <|> openModelBinder env (.const ``Stage.Sound []) then
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
    unless openModelBinder env (.const name []) == some (privateToUserName name, binder) do
      throwError "End-to-end audit accepted a caller-selected binder in {name}"
  if (openModelBinder env (.const ``closedCodec [])).isSome then
    throwError "End-to-end audit rejected a fixed executable codec"
  unless (openModelBinder env (.const ``plantedStatement []) == some (`EndToEndAudit.plantedStatement, `model)) do
    throwError "End-to-end audit accepted a planted semantic model parameter"
  unless (openModelBinder env (.const ``plantedRelation []) == some (`EndToEndAudit.plantedRelation, `meaning)) do
    throwError "End-to-end audit accepted a planted relation parameter"
  unless (openModelBinder env (.const ``plantedCodec []) == some (`EndToEndAudit.plantedCodec, `decode)) do
    throwError "End-to-end audit accepted a planted codec parameter"
  unless (openModelBinder env (.const ``plantedWrapped []) == some (`EndToEndAudit.plantedWrapped, `wrapped)) do
    throwError "End-to-end audit accepted a planted wrapped decoder"
  unless (openModelBinder env (.const ``PlantedInductive []) == some (`EndToEndAudit.PlantedInductive, `decode)) do
    throwError "End-to-end audit accepted a planted decoder under an inductive constructor"
  unless (openModelBinder env (.const ``PlantedConstructor []) == some (`EndToEndAudit.PlantedConstructor, `decode)) do
    throwError "End-to-end audit accepted a planted decoder as an inductive constructor field"
  unless badPassField env ``PlantedPassBundle == some `run do
    throwError "End-to-end audit accepted a planted decoder in the executable pass bundle"

end EndToEndAudit
