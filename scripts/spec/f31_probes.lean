import Lean

/-!
F31 migration tool. Inventory written commands from Lean's frontend snapshots and retain the
persistent declarations from the same elaboration. Run through f31_surface.py.
-/

open Lean Language

private def commandRow (ctx : Parser.InputContext) (stx : Syntax) : IO Json := do
  let some range := stx.getRange? | throw <| IO.userError "F31: missing command range"
  let pos := ctx.fileMap.toPosition range.start
  let source := range.start.extract ctx.inputString range.stop
  return toJson (("probe-command", pos.line), s!"{pos.column}: {source}")

def main (args : List String) : IO Unit := do
  let [path, sourcePath, output] := args
    | throw <| IO.userError "usage: f31_probes.lean PATH SOURCE OUTPUT"
  unsafe enableInitializersExecution
  initSearchPath (← findSysroot)
  let ctx := Parser.mkInputContext (← IO.FS.readFile sourcePath) path
  let mainModuleName ← moduleNameOfFileName path none
  let setup header := pure <| .ok {
    mainModuleName
    imports := header.imports
    isModule := header.isModule
    opts := {}
  }
  let snapshot ← Language.Lean.process setup none { ctx with }
  if ← (toSnapshotTree snapshot).runAndReport {} false {} then
    throw <| IO.userError s!"F31: elaboration failed for {path}"
  let some header := snapshot.processedResult.get
    | throw <| IO.userError s!"F31: failed to import {path}"

  let `(Parser.Module.header| $[module%$moduleTk?]? $[prelude%$preludeTk?]? $imports*) := snapshot.stx
    | throw <| IO.userError s!"F31: unrecognized module header in {path}"
  let headerCommands := #[moduleTk?, preludeTk?].filterMap id ++ imports.map (·.raw)
  let mut rows ← headerCommands.mapM (commandRow ctx)
  let mut command := header.firstCmdSnap.get
  repeat
    unless command.stx.isOfKind ``Parser.Command.eoi do
      rows := rows.push (← commandRow ctx command.stx)
    let some next := command.nextCmdSnap? | break
    command := next.get

  let env := command.elabSnap.resultSnap.get.cmdState.env
  for (name, _) in env.constants.map₂.toList do
    if (env.getModuleIdxFor? name).isNone then
      rows := rows.push (toJson (("probe-declaration", 1), toString name))
  IO.FS.writeFile output (Json.compress (toJson rows))
