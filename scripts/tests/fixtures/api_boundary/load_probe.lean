-- API-boundary consumer probe (ROADMAP Phase 4 #16b / #16): a minimal external
-- consumer that loads a Concrete project using ONLY the boundary module
-- `Concrete.Project` — no umbrella, no internals. Proves the project-loading
-- boundary is actually usable in-process. Run via `lake env lean --run`.
import Concrete.Resolve.Project
open Concrete

def main : IO Unit := do
  match ← findProjectRoot "examples/project" with
  | none => IO.eprintln "PROBE-FAIL: no project found"
  | some root =>
    match ← loadProject root with
    | .error _ => IO.eprintln "PROBE-FAIL: loadProject errored"
    | .ok ctx =>
      IO.println s!"PROBE-OK: loaded {ctx.validCore.coreModules.length} core modules, {ctx.depNames.length} deps via the boundary"
