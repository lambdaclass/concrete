-- Emits one JSON object per registered rule (ConcreteSpec/Spec/Rules.lean), for scripts/spec/rules.py.
-- Run from the repository root: `lake env lean scripts/spec/rules.lean > rules.jsonl`.
import ConcreteSpec
import Lean
open Lean Elab Command

#eval show CommandElabM Unit from do
  let env ← getEnv
  for r in ConcreteSpec.Core.registeredRules do
    let doc := (← findDocString? env r.name).getD ""
    IO.println (Json.compress (Json.mkObj [("name", toString r.name), ("rev", r.rev),
      ("exists", toJson (env.contains r.name)), ("doc", doc)]))
  IO.println (Json.compress (Json.mkObj [("end", toJson true)]))
