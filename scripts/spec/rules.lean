-- The gate appends this same exporter body to fixtures after their declarations.
import ConcreteSpec.Spec.Rules
import Lean
open Lean Elab Command

-- EXPORTER BODY
/-- One bounded traversal of reduced data and its type arguments. Types and proof
propositions must be closed; proof bodies do not identify the example and are skipped.
Carrier parameters are closed type metadata, not data fields (they can include
function-valued typeclass dictionaries). Non-propositional indices and fields must
themselves be constructor/literal data.
The fuel bounds tree depth; Lean's heartbeat bound also bounds each reduction. -/
def concreteData (input : Expr) : MetaM Bool := do
  let rec visit (fuel : Nat) (e : Expr) (typePosition : Bool) : MetaM Bool := do
    let fuel + 1 := fuel | return false
    if e.hasFVar || e.hasMVar || e.hasLooseBVars then return false
    if ← Meta.isProof e then
      -- Only callers inspecting constructor arguments may skip a proof field.
      return typePosition && !(← Meta.inferType e).hasFVar &&
        !(← Meta.inferType e).hasMVar && !(← Meta.inferType e).hasLooseBVars
    if typePosition && (← Meta.isProp e) then return true
    let e ← Meta.withTransparency .default <| Meta.whnf e
    if e.hasFVar || e.hasMVar || e.hasLooseBVars then return false
    if typePosition then
      if e.isSort then return true
      let .const n _ := e.getAppFn | return false
      let some (.inductInfo i) := (← getEnv).find? n | return false
      for arg in e.getAppArgs[i.numParams:] do
        let proofOrType ← if ← Meta.isProof arg then pure true
          else pure (← Meta.whnf (← Meta.inferType arg)).isSort
        if !(← visit fuel arg proofOrType) then return false
      return true
    match e with
    | .lit _ => return true
    | _ =>
      let .const n _ := e.getAppFn | return false
      let some (.ctorInfo c) := (← getEnv).find? n | return false
      let args := e.getAppArgs
      if args.size != c.numParams + c.numFields then return false
      if !(← visit fuel (← Meta.inferType e) true) then return false
      for arg in args[c.numParams:] do
        let ty ← Meta.inferType arg
        if ← Meta.isProof arg then
          if ty.hasFVar || ty.hasMVar || ty.hasLooseBVars then return false
        else if !(← visit fuel arg (← Meta.whnf ty).isSort) then return false
      return true
  try
    if ← Meta.isProof input then return false
    visit 1024 input false
  catch _ => return false

set_option maxHeartbeats 200000 in
#eval show CommandElabM Unit from do
  let env ← getEnv
  for r in ConcreteSpec.Core.registeredRules do
    let doc := (← findDocString? env r.name).getD ""
    IO.println (Json.compress (Json.mkObj [("kind", "rule"), ("name", toString r.name),
      ("rev", r.rev), ("exists", toJson (env.contains r.name)), ("doc", doc)]))
  for e in ConcreteSpec.Core.registeredRuleExamples do
    let ci := env.find? e.declaration
    let proved := match ci with
      | some (.thmInfo _) => true
      | _ => false
    -- Trust is independently inventoried, not inferred from reducibility.
    let trusted ← match ci with
      | none => pure false
      | some _ => do
        let axioms ← collectAxioms e.declaration
        pure (axioms.all fun n => [``propext, ``Classical.choice, ``Quot.sound].contains n)
    let (verdict, usesRule) ← liftTermElabM do
      let some c := ci | return ("Invalid", false)
      if !c.levelParams.isEmpty then return ("Invalid", false)
      let ty := c.type
      let args := ty.getAppArgs
      let head := ty.getAppFn.constName?
      let verdict := if head == some ``ConcreteSpec.Core.RuleAccepted then "Accepted"
        else if head == some ``ConcreteSpec.Core.RuleRefused then "Refused" else "Invalid"
      if verdict == "Invalid" || args.size != 3 then return ("Invalid", false)
      let linked := args[1]!.isConstOf e.rule
      let concrete ← if trusted then concreteData args[2]! else pure false
      return (if concrete then verdict else "Invalid", linked)
    IO.println (Json.compress (Json.mkObj [("kind", "example"), ("rule", toString e.rule),
      ("name", toString e.declaration),
      ("verdict", verdict),
      ("elaborated", toJson (proved && trusted)), ("usesRule", toJson usesRule)]))
  IO.println (Json.compress (Json.mkObj [("end", toJson true)]))
