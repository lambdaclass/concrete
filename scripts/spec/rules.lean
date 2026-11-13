-- The gate appends this same exporter body to fixtures after their declarations.
import ConcreteSpec.Spec.Rules
import Lean
open Lean Elab Command

-- EXPORTER BODY
#eval show CommandElabM Unit from do
  let env ← getEnv
  for r in ConcreteSpec.Core.registeredRules do
    let doc := (← findDocString? env r.name).getD ""
    IO.println (Json.compress (Json.mkObj [("kind", "rule"), ("name", toString r.name),
      ("rev", r.rev), ("exists", toJson (env.contains r.name)), ("doc", doc)]))
  for e in ConcreteSpec.Core.registeredRuleExamples do
    let ci := env.find? e.declaration
    let axioms ← match ci with
      | some _ => liftCoreM (collectAxioms e.declaration)
      | none => pure #[]
    let proved := match ci with
      | some (.thmInfo _) => true
      | _ => false
    let usesRule := match ci with
      | some c => c.getUsedConstantsAsSet.contains e.rule
      | none => false
    let trusted := axioms.all fun a => [``propext, ``Classical.choice, ``Quot.sound].contains a
    IO.println (Json.compress (Json.mkObj [("kind", "example"), ("rule", toString e.rule),
      ("name", toString e.declaration),
      ("verdict", match e.verdict with | .accepted => "Accepted" | .refused => "Refused"),
      ("elaborated", toJson (proved && trusted)), ("usesRule", toJson usesRule)]))
  IO.println (Json.compress (Json.mkObj [("end", toJson true)]))
