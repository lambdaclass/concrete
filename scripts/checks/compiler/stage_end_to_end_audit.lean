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
private partial def inspect (env : Environment) (origin : Name) (expr : Expr) (inComputation := false) :
    StateRefT (Std.HashSet Name) MetaM (Option (Name × Name)) := do
  match expr with
  | .forallE name domain body info =>
    -- A function type used by a closed computation can have exactly the same
    -- domain as a quantified model. Only a data type passed to executable code
    -- can introduce this arrow without quantifying the contract.
    if modelBinder env domain && (!inComputation || (← Meta.isProp expr)) then return some (origin, name)
    let found ← inspect env origin domain inComputation
    Meta.withLocalDecl name info domain fun x => do
      return found <|> (← inspect env origin (body.instantiate1 x) inComputation)
  | .lam name domain body info =>
    let found ← inspect env origin domain inComputation
    Meta.withLocalDecl name info domain fun x => do
      return found <|> (← inspect env origin (body.instantiate1 x) inComputation)
  | .letE name type value body nondep =>
    let found := (← inspect env origin type inComputation) <|> (← inspect env origin value inComputation)
    Meta.withLetDecl name type value (nondep := nondep) fun x => do
      return found <|> (← inspect env origin (body.instantiate1 x) inComputation)
  | .app .. =>
    if let some hit ← inspect env origin expr.getAppFn then return some hit
    -- Predicates restore the logical context even inside a closed computation.
    -- A type-valued match motive inherits the context of the computation that supplies it.
    let computational := !(← Meta.isProp expr) && (inComputation || !(← Meta.isType expr))
    for arg in expr.getAppArgs do
      if let some hit ← inspect env origin arg computational then return some hit
    return none
  | .mdata _ body | .proj _ _ body => inspect env origin body inComputation
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
            if let some hit ← Meta.forallBoundedTelescope ctorInfo.type (some ctorInfo.numParams)
                (fun _ fields => inspectConstructor env root fields) then
              return some hit
      return none
    | _ => return none
  | _ => return none
where
  inspectConstructor (env : Environment) (origin : Name) (type : Expr) :
      StateRefT (Std.HashSet Name) MetaM (Option (Name × Name)) := do
    match type with
    | .forallE name domain body info =>
      -- The primitive continuation is fixed by the dispatch equality in Step.prim.
      let dispatchContinuation := origin == ``ConcreteSpec.Core.Exec.Step && name == `k
      if !dispatchContinuation && !usedInConstructorResult body 0 && modelBinder env domain then
        return some (origin, name)
      let found ← inspect env origin domain
      Meta.withLocalDecl name info domain fun x => do
        return found <|> (← inspectConstructor env origin (body.instantiate1 x))
    | _ => return none

private def check (env : Environment) (type : Expr) : MetaM (Option (Name × Name)) :=
  (inspect env `endToEnd type).run' {}

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

run_cmd liftTermElabM do
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
    if let some (origin, binder) ← check env theoremInfo.type then
      throwError "End-to-end statement has an open semantic binder {binder} in {origin}"
  let some soundInfo := env.find? ``Stage.Sound
    | throwError "Missing registered stage soundness"
  if let some (origin, binder) := (← check env soundInfo.type) <|> (← check env (.const ``Stage.Sound [])) then
    throwError "Stage soundness has an open semantic binder {binder} in {origin}"
  unless (← check env (.const ``plantedDataBinder [])) == some (`EndToEndAudit.plantedDataBinder, `decode) do
    throwError "End-to-end audit accepted a quantified codec inside a data-valued premise"
  if (← check env (.const ``closedCodec [])).isSome then
    throwError "End-to-end audit rejected a fixed executable codec"
  unless ((← check env (.const ``plantedStatement [])) == some (`EndToEndAudit.plantedStatement, `model)) do
    throwError "End-to-end audit accepted a planted semantic model parameter"
  unless ((← check env (.const ``plantedRelation [])) == some (`EndToEndAudit.plantedRelation, `meaning)) do
    throwError "End-to-end audit accepted a planted relation parameter"
  unless ((← check env (.const ``plantedCodec [])) == some (`EndToEndAudit.plantedCodec, `decode)) do
    throwError "End-to-end audit accepted a planted codec parameter"
  unless ((← check env (.const ``plantedWrapped [])) == some (`EndToEndAudit.plantedWrapped, `wrapped)) do
    throwError "End-to-end audit accepted a planted wrapped decoder"
  unless ((← check env (.const ``PlantedInductive [])) == some (`EndToEndAudit.PlantedInductive, `decode)) do
    throwError "End-to-end audit accepted a planted decoder under an inductive constructor"
  unless ((← check env (.const ``PlantedConstructor [])) == some (`EndToEndAudit.PlantedConstructor, `decode)) do
    throwError "End-to-end audit accepted a planted decoder as an inductive constructor field"
  unless badPassField env ``PlantedPassBundle == some `run do
    throwError "End-to-end audit accepted a planted decoder in the executable pass bundle"

end EndToEndAudit
