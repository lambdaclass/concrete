import ConcreteSpec.Proofs.LeakRegistry
-- The gate appends this same exporter body to fixtures after their declarations.
import ConcreteSpec.Spec.Rules
import Lean
open Lean Elab Command

-- EXPORTER BODY
/-- Execute the single inspectable-example rule documented on RuleExample.
Fuel bounds recursive depth; heartbeats bound reduction. All errors fail closed. -/
def concreteData (input : Expr) : MetaM Bool := do
  let rec visit (fuel : Nat) (e : Expr) (runtimeValue : Bool) : MetaM Bool := do
    let fuel + 1 := fuel | return false
    if e.hasFVar || e.hasMVar || e.hasLooseBVars then return false
    if ← Meta.isProof e then return true
    let typePosition := (← Meta.whnf (← Meta.inferType e)).isSort
    if runtimeValue && typePosition then return false
    let e ← Meta.whnf e
    if e.hasFVar || e.hasMVar || e.hasLooseBVars then return false
    if typePosition && e.isSort then return true
    match e with
    | .lit _ => return !typePosition
    | _ =>
      let .const n _ := e.getAppFn | return false
      let info := (← getEnv).find? n
      let fieldStart ← if typePosition then
        let some (.inductInfo _) := info | return false
        pure e.getAppArgs.size
      else
        let some (.ctorInfo c) := info | return false
        if e.getAppArgs.size != c.numParams + c.numFields then return false
        pure c.numParams
      for i in [:e.getAppArgs.size] do
        let arg := e.getAppArgs[i]!
        if !(← visit fuel arg (!typePosition && i >= fieldStart)) then return false
      if !typePosition then
        if !(← visit fuel (← Meta.inferType e) false) then return false
      return true
  try
    Meta.withTransparency .default do
      if ← Meta.isProof input then return false
      visit 1024 input true
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
