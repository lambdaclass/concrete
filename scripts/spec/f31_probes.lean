import Lean

/-!
F31 migration tool. Inventory written commands from Lean's frontend snapshots and retain the
persistent declarations from the same elaboration. Run through f31_surface.py.
-/

open Lean Language

private def commandRow (ctx : Parser.InputContext) (kind : String) (stx : Syntax) : IO Json := do
  let some range := stx.getRange? | throw <| IO.userError "F31: missing command range"
  let pos := ctx.fileMap.toPosition range.start
  let source := range.start.extract ctx.inputString range.stop
  return toJson ((kind, pos.line), s!"{pos.column}: {source}")

-- Nested commands use the same parser category as top-level commands. Syntax quotations
-- contain command-shaped data, so do not walk into them.
private partial def commandRows (ctx : Parser.InputContext) (kinds : Parser.SyntaxNodeKindSet)
    (library : Bool) (stx : Syntax) (rows : Array Json) (knownCommand := true) : IO (Array Json) := do
  if stx.isQuot then return rows
  let mut rows := rows
  if knownCommand || kinds.contains stx.getKind then
    let isExample := stx.isOfKind ``Parser.Command.declaration &&
      stx[1].isOfKind ``Parser.Command.example
    let kind := if !library then "probe-command"
      else if isExample then "anonymous-example" else "library-command"
    rows := rows.push (← commandRow ctx kind stx)
  if stx.isOfKind ``Parser.Command.in then
    -- The RHS is parsed with the scope introduced by the LHS. Its command kind
    -- may be absent from the parser category outside that scope.
    rows ← commandRows ctx kinds library stx[0] rows
    rows ← commandRows ctx kinds library stx[2] rows
  else
    for child in stx.getArgs do
      rows ← commandRows ctx kinds library child rows false
  return rows

private def probe (library : Bool) (root path sourcePath output : String) : IO Unit := do
  let ctx := Parser.mkInputContext (← IO.FS.readFile sourcePath) path
  let mainModuleName ← moduleNameOfFileName (System.FilePath.mk root / path) (some root)
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
  let headerKind := if library then "library-command" else "probe-command"
  let mut rows ← headerCommands.mapM (commandRow ctx headerKind)
  let mut env := header.cmdState.env
  let mut command := header.firstCmdSnap.get
  repeat
    unless command.stx.isOfKind ``Parser.Command.eoi do
      let some category := Parser.getParserCategory? env `command
        | throw <| IO.userError "F31: missing command parser category"
      rows ← commandRows ctx category.kinds library command.stx rows
    env := command.elabSnap.resultSnap.get.cmdState.env
    let some next := command.nextCmdSnap? | break
    command := next.get

  unless library do
    for (name, _) in env.constants.map₂.toList do
      if (env.getModuleIdxFor? name).isNone then
        rows := rows.push (toJson (("probe-declaration", 1), toString name))
  IO.FS.writeFile output (Json.compress (toJson rows))

def main (args : List String) : IO Unit := do
  unsafe enableInitializersExecution
  initSearchPath (← findSysroot)
  match args with
  | ["library-sources"] =>
    let env ← importModules #[{ module := `ConcreteSpec }, { module := `Concrete }] {}
    -- Use the resolved artifact to distinguish repository modules from dependency modules.
    let some packageLib := (← realPathNormalized (← findOLean `Concrete)).parent
      | throw <| IO.userError "F31: missing library artifact directory"
    let mut paths := #[]
    for name in env.header.moduleNames do
      let artifact ← realPathNormalized (← findOLean name)
      if artifact == modToFilePath packageLib name "olean" then
        paths := paths.push (modToFilePath "." name "lean").toString
    IO.println (toJson paths).compress
  | [mode, root, path, sourcePath, output] =>
    unless mode == "library" || mode == "standalone" do
      throw <| IO.userError s!"F31: unknown probe mode {mode}"
    probe (mode == "library") root path sourcePath output
  | _ =>
    throw <| IO.userError "usage: f31_probes (library-sources | MODE ROOT PATH SOURCE OUTPUT)"
