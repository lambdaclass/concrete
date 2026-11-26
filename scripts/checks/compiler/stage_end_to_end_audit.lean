import Concrete.Interfaces.Pipeline
import Lean.Elab.Command

namespace EndToEndAudit
open Concrete.Interfaces ConcreteSpec.StageInterface ConcreteSpec.StageGrammar
open Lean Elab Command

private def modelTypes : List Name :=
  [``CoreLanguage, ``ConcreteSpec.Core.Exec.Operands, ``LoweringLanguage, ``SharedGrammar,
    ``TableVerifier, ``VerificationInputs, ``ParserEngine, ``Parser, ``CheckedTable,
    ``Concrete.Contracts.Semantics, ``Stage, ``Pipeline, ``Registry]

private partial def modelBinder (env : Environment) (type : Expr) (functionType := false) : Bool :=
  match type with
  | .forallE _ _ body _ => modelBinder env body true
  | .sort _ => true
  | _ =>
    match type.getAppFn.constName? with
    | none => false
    | some name =>
      modelTypes.contains name ||
        (functionType && [``Bool, ``Nat, ``UInt8, ``ByteArray, ``List, ``Option, ``Except,
          ``ConcreteSpec.Core.Res].contains name) ||
        match env.find? name with
        | some (.defnInfo info) =>
          modelBinder env ((mkAppN info.value type.getAppArgs).headBeta) functionType
        | _ => false

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
    | _ => return none
  | _ => return none

private def check (env : Environment) (type : Expr) : Option (Name × Name) :=
  ((inspect env `endToEnd type).run {}).1

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

run_cmd do
  let env ← getEnv
  -- A model packaged inside the pass argument would still be caller-selected.
  let fields := #[`lex, `elaborate, `checkCore, `lowerCore, `lowerUnchecked, `lowerMemory,
    `buildSSA, `buildBIR, `emitLLVM, `emitLCB]
  unless (getStructureInfo? env ``PassFunctions).map (·.fieldNames) == some fields do
    throwError "The pass argument must contain only the registered executable functions"
  let some theoremInfo := env.find? ``Pipeline.endToEnd
    | throwError "Missing registered end-to-end theorem"
  if let some (origin, binder) := check env theoremInfo.type then
    throwError "End-to-end statement has an open semantic binder {binder} in {origin}"
  let some soundInfo := env.find? ``Stage.Sound
    | throwError "Missing registered stage soundness"
  if let some (origin, binder) := check env soundInfo.type <|> check env (.const ``Stage.Sound []) then
    throwError "Stage soundness has an open semantic binder {binder} in {origin}"
  unless (check env (.const ``plantedStatement []) == some (`EndToEndAudit.plantedStatement, `model)) do
    throwError "End-to-end audit accepted a planted semantic model parameter"
  unless (check env (.const ``plantedRelation []) == some (`EndToEndAudit.plantedRelation, `meaning)) do
    throwError "End-to-end audit accepted a planted relation parameter"
  unless (check env (.const ``plantedCodec []) == some (`EndToEndAudit.plantedCodec, `decode)) do
    throwError "End-to-end audit accepted a planted codec parameter"

end EndToEndAudit
