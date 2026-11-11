/- Environment and parser census for the pinned landing snapshot. No source-text declaration scan. -/
import Lean
open Lean

private def emit (fields : List (String × Json)) : IO Unit :=
  IO.println (Json.compress (Json.mkObj fields))

private partial def containsExample (s : Syntax) : Bool :=
  s.isOfKind ``Parser.Command.example || s.getArgs.any containsExample

private partial def exampleToken (s : Syntax) : Option Nat :=
  match s with
  | .atom _ "example" => s.getPos?.map (·.byteIdx)
  | _ => s.getArgs.findSome? exampleToken

private partial def identifiers (s : Syntax) : Array String :=
  match s with
  | .ident _ _ n _ => #[n.toString]
  | _ => s.getArgs.flatMap identifiers

private partial def commands (ctx : Parser.InputContext) (env : Environment)
    (ps : Parser.ModuleParserState) (messages : MessageLog) : IO Unit := do
  let (stx, next, messages) := Parser.parseCommand ctx { env, options := {} } ps messages
  if messages.hasErrors then
    for msg in messages.toList do IO.eprintln (← msg.toString)
    throw (IO.userError s!"parser failed: {ctx.fileName}")
  unless Parser.isTerminalCommand stx do
    let start := ctx.fileMap.toPosition (stx.getPos?.getD 0)
    let stop := ctx.fileMap.toPosition (stx.getTailPos?.getD 0)
    emit [("record", toJson "command"), ("file", toJson ctx.fileName),
      ("syntax", toJson stx.getKind.toString), ("example", toJson (containsExample stx)), ("exampleToken", toJson (exampleToken stx)), ("line", toJson start.line),
      ("endLine", toJson stop.line), ("identifiers", toJson (identifiers stx))]
    commands ctx env next messages

unsafe def main (args : List String) : IO Unit := do
  initSearchPath (← findSysroot)
  enableInitializersExecution
  match args with
  | "parse" :: files =>
    let env ← importModules (loadExts := true) #[{module := `Lean}] {} 0
    for file in files do
      let ctx := Parser.mkInputContext (← IO.FS.readFile file) file
      let (header, ps, messages) ← Parser.parseHeader ctx
      if messages.hasErrors then throw (IO.userError "header parse failed")
      let mut imports : Array Name := #[]
      if let `(Parser.Module.header| $[module]? $[prelude]? $importsStx*) := header then
        for stx in importsStx do
          if let `(Parser.Module.import| $[public]? $[meta]? import $[all]? $mod) := stx then
            imports := imports.push mod.getId
      emit [("record", toJson "imports"), ("file", toJson file),
        ("imports", toJson (imports.map Name.toString))]
      commands ctx env ps messages
  | ["constants", modulesFile] =>
    let modules := (← IO.FS.lines modulesFile).map (fun s => s.toName)
    let env ← importModules (loadExts := true) (modules.map fun m => { module := m }) {} 0
    for (n, ci) in env.constants.map₁.toList do
      let some idx := env.getModuleIdxFor? n | continue
      let m := env.header.moduleNames[idx.toNat]!
      unless modules.contains m do continue
      let kind := match ci with
        | .thmInfo _ => "theorem" | .defnInfo _ => "def" | .inductInfo _ => "inductive"
        | .ctorInfo _ => "constructor" | .axiomInfo _ => "axiom" | .opaqueInfo _ => "opaque"
        | .recInfo _ => "recursor" | .quotInfo _ => "quot"
      -- Ranges include private helpers; generated declarations without a range remain in the census.
      let ranges := declRangeExt.find? env n
      emit [("record", toJson "declaration"), ("name", toJson n.toString),
        ("kind", toJson kind), ("userName", toJson (privateToUserName n).toString), ("module", toJson m.toString),
        ("line", toJson (ranges.map (·.selectionRange.pos.line) |>.getD 0)),
        ("uses", toJson (ci.getUsedConstantsAsSet.toList.map Name.toString))]
  | _ => throw (IO.userError "expected: parse FILE | constants MODULES_FILE")
