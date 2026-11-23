import Lean

open Lean

namespace F31Census

private def marker : Name := `F31Census.commandBoundary

-- Run only on the observation parse. Carry boundaries in the returned tree so
-- failed alternatives and lookahead cannot contribute events.
private def observeCategory (category : Name) : Parser.ParserFn := fun ctx state =>
  let result := Parser.categoryParserFnImpl category ctx state
  if category == `command && ctx.quotDepth == 0 && !result.hasError &&
      result.stackSize == state.stackSize + 1 then
    result.popSyntax.pushSyntax (Syntax.node .none marker #[result.stxStack.back])
  else result

private partial def strip (stx : Syntax) : Syntax :=
  match stx with
  | .node info kind args =>
    if kind == marker then strip args[0]!
    else .node info kind (args.map strip)
  | _ => stx

private partial def boundaries (stx : Syntax) : Array Syntax :=
  if stx.isOfKind marker then #[stx[0]] ++ boundaries stx[0]
  else stx.getArgs.flatMap boundaries

private def emit (out : IO.FS.Handle) (fields : List (String × Json)) : IO Unit :=
  out.putStrLn (Json.compress (Json.mkObj fields))

private def location (ctx : Parser.InputContext) (stx : Syntax) : List (String × Json) :=
  let start := stx.getPos?.getD 0
  [("file", toJson ctx.fileName), ("line", toJson (ctx.fileMap.toPosition start).line),
   ("start", toJson start.byteIdx), ("stop", toJson (stx.getTailPos?.getD start).byteIdx)]

private def uses (names : NameSet) : Json :=
  toJson (names.toList.map Name.toString |>.mergeSort (· ≤ ·))

private def enclosingCommand (commands : Array Syntax) (stx : Syntax) : Nat := Id.run do
  let some start := stx.getPos? | return 0
  let some stop := stx.getTailPos? | return 0
  -- Descendants follow their parents, and sibling spans do not overlap.
  return ((List.range commands.size).reverse.find? fun i =>
    (commands[i]!.getPos?.any (· ≤ start)) &&
      (commands[i]!.getTailPos?.any (stop ≤ ·))).getD 0

private def termUses (commands : Array Syntax) (tree : Elab.InfoTree) :
    IO (Array NameSet) :=
  tree.foldInfoM (fun ctx info names => do
    let .ofTermInfo term := info | return names
    term.runMetaM ctx do
      let expr ← instantiateMVars term.expr
      let index := enclosingCommand commands term.stx
      return names.modify index (· ++ expr.getUsedConstantsAsSet))
    (Array.replicate commands.size {})

private partial def walk (out : IO.FS.Handle) (ctx : Parser.InputContext) (parserState : Parser.ModuleParserState)
    (before : Elab.Command.State) (snapshot : Language.Lean.CommandParsedSnapshot)
    (seen : NameSet) : IO Unit := do
  let after := snapshot.elabSnap.resultSnap.get.cmdState
  if after.messages.hasErrors then
    for msg in after.messages.reportedPlusUnreported.toList do IO.eprintln (← msg.toString)
    throw (IO.userError s!"elaboration failed: {ctx.fileName}")
  unless Parser.isTerminalCommand snapshot.stx do
    let scope := before.scopes.head!
    let env := Parser.categoryParserFnExtension.modifyState before.env (fun _ => observeCategory)
    let (observed, next, messages) := Parser.parseCommand ctx
      { env, options := scope.opts, currNamespace := scope.currNamespace, openDecls := scope.openDecls }
      parserState .empty
    unless !messages.hasErrors && (strip observed).eqWithInfo snapshot.stx &&
        next.pos == snapshot.parserState.pos && observed.isOfKind marker do
      throw (IO.userError s!"parser observation changed the command: {ctx.fileName}:{parserState.pos.byteIdx}")
    let commands := boundaries observed
    let some tree := snapshot.elabSnap.infoTreeSnap.get.infoTree?
      | throw (IO.userError s!"missing elaboration information: {ctx.fileName}")
    let terms ← termUses commands tree
    for i in [:commands.size] do
      let command := commands[i]!
      emit out (location ctx command ++ [("record", toJson "command"),
        ("kind", toJson command.getKind.toString), ("uses", uses terms[i]!)])
  let mut seen := seen
  for (name, info) in after.env.constants.map₂.toList do
    unless seen.contains name do
      emit out (location ctx snapshot.stx ++ [("record", toJson "declaration"),
        ("name", toJson name.toString), ("uses", uses info.getUsedConstantsAsSet)])
      seen := seen.insert name
  if let some next := snapshot.nextCmdSnap? then
    walk out ctx snapshot.parserState after next.get seen

end F31Census

unsafe def main (args : List String) : IO Unit := do
  let [file, moduleName, output] := args | throw (IO.userError "expected source path, module name and output path")
  initSearchPath (← findSysroot)
  enableInitializersExecution
  let ctx := Parser.mkInputContext (← IO.FS.readFile file) file (normalizeLineEndings := true)
  let (header, parserState, messages) ← Parser.parseHeader ctx
  let opts := ({} : Options).setBool `autoImplicit false
    |>.setBool `warningAsError true
    |>.setBool `Elab.async false
    |>.setBool `internal.cmdlineSnapshots false
  let (env, messages) ← Elab.processHeader ⟨header⟩ opts messages ctx (mainModule := moduleName.toName)
  let state := Elab.Command.mkState env messages opts
  let snapshot ← Language.Lean.processCommands ctx parserState state
  let out ← IO.FS.Handle.mk output .write
  let mut modules := #[]
  if let `(Parser.Module.header| $[module]? $[prelude]? $imports*) := header then
    for item in imports do
      if let `(Parser.Module.import| $[public]? $[meta]? import $[all]? $mod) := item then
        modules := modules.push mod.getId.toString
  F31Census.emit out [("record", toJson "header"), ("file", toJson file),
    ("imports", toJson modules), ("uses", toJson (#[] : Array String))]
  F31Census.walk out ctx parserState state snapshot.get {}
