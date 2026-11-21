import ConcreteSpec.Spec.Own
import Lean

open Lean Elab Command

-- Use the built, unmutated module so a failed proof cannot hide its own range.
#eval show CommandElabM Unit from do
  let env ← getEnv
  let some own := env.getModuleIdx? `ConcreteSpec.Spec.Own
    | throwError "missing ownership module"
  let mut ranges := []
  for (name, info) in env.constants do
    let .thmInfo _ := info | continue
    if env.getModuleIdxFor? name != some own then continue
    let some range ← findDeclarationRangesCore? name | continue
    ranges := (toString name, toJson (range.range.pos.line, range.range.endPos.line)) :: ranges
  IO.println (Json.mkObj ranges).compress
