import Lean
import Concrete.Premises

/- Gate-only frontend. Lean parses commands in their real import/namespace context.
   Retain anonymous examples as named noncomputable definitions, then audit their
   axiom closures AND all generated declarations (including local recursion).
   No source lexer or textual import insertion participates in the trust check. -/
open Lean Elab Frontend Command

def sortAdded (xs : Array (Name × ConstantInfo)) : Array (Name × ConstantInfo) :=
  xs.qsort fun a b =>
    let ea := a.1.eraseMacroScopes.toString
    let eb := b.1.eraseMacroScopes.toString
    ea < eb || (ea == eb && a.1.toString < b.1.toString)

def freshExampleName (base : String) : Command.CommandElabM Name := do
  let env ← getEnv
  let ns ← getCurrNamespace
  let checkFree (c : Name) : Bool :=
    let full := ns ++ c
    !env.contains full && !env.contains (mkPrivateName env full)
  let candidate := Name.mkSimple base
  if checkFree candidate then return candidate
  let u ← liftCoreM mkFreshId
  return Name.mkSimple s!"{base}:{u}"

def makeExampleDef (stx : Syntax) (file : String) (map : FileMap) :
    Command.CommandElabM (Syntax × Name) := do
  let exStx := stx[1]
  let pos := map.toPosition (exStx.getPos?.getD (stx.getPos?.getD 0))
  let n ← freshExampleName s!"⟪example⟫{file}:{pos.line}:{pos.column}"
  let sig : TSyntax ``Parser.Command.optDeclSig := ⟨exStx[1]⟩
  let val : TSyntax ``Parser.Command.declVal := ⟨exStx[2]⟩
  let id := mkIdent n
  let replacement ← `(command| def $id:ident $sig:optDeclSig $val:declVal)
  return (stx.setArg 1 replacement.raw[1], (← getCurrNamespace) ++ n)

-- This elaborator participates in recursive command elaboration, including commands
-- produced by imported macros. Quoted command syntax is never elaborated here.
def retainExample : CommandElab := fun stx => do
  unless stx[1].getKind == ``Parser.Command.example do throwUnsupportedSyntax
  let (replacement, _) ← makeExampleDef stx (← getFileName) (← getFileMap)
  elabCommand replacement

partial def hasPartialModifier (stx : Syntax) : Bool :=
  if stx.getKind == ``Parser.Command.declaration then
    stx[0].find? (fun s => s.isAtom && s.getAtomVal == "partial") |>.isSome
  else stx.getArgs.any hasPartialModifier

def effectiveOwners (env : Environment) (n : Name)
    (cs : List Concrete.Premises.Component) : List Concrete.Premises.Component :=
  let direct := Concrete.Premises.declarationOwners n cs
  if !direct.isEmpty then direct
  else match Compiler.isUnsafeRecName? n with
    | some parent =>
        if (env.find? n).any (·.isPartial) && (env.find? parent).isSome then
          Concrete.Premises.declarationOwners parent cs
        else []
    | none => []

def checkOwnerKind (n : Name) (owners : List Concrete.Premises.Component) : CommandElabM Bool := do
  match owners with
  | [] => return false
  | [owner] =>
      unless #[Concrete.Premises.ComponentKind.implementation, .backend, .runtime].contains owner.kind do
        IO.println s!"FAIL trusted declaration {n} belongs to non-code component {owner.id}"
        throwError "trusted declaration owner audit failed"
      return true
  | _ =>
      IO.println s!"FAIL trusted declaration {n} is owned by {owners.length} components, expected exactly one"
      throwError "trusted declaration owner audit failed"

partial def commands (file : String) (csList : List Concrete.Premises.Component) : FrontendM Unit := do
  updateCmdPos
  let cs ← getCommandState
  let input ← getInputContext
  let scope := cs.scopes.head!
  let (cmd, ps, msgs) := Parser.parseCommand input
    { env := cs.env, options := scope.opts, currNamespace := scope.currNamespace, openDecls := scope.openDecls }
    (← getParserState) cs.messages
  setParserState ps
  setMessages msgs
  elabCommandAtFrontend cmd
  let messages := (← getCommandState).messages
  if messages.hasErrors then
    for msg in messages.toList do IO.eprintln (← msg.toString)
    throw <| IO.userError "source command elaboration failed"
  runCommandElabM do
    let env ← getEnv
    let allowed := Concrete.Premises.allowedAxioms csList
    let mut added : Array (Name × ConstantInfo) := #[]
    for (n, ci) in env.constants.toList do
      unless cs.env.contains n || n.isAnonymous do
        added := added.push (n, ci)
    for (n, ci) in sortAdded added do
      do
        let kinds ← liftCoreM <| Concrete.Premises.trustedDeclarationKinds env n ci
        do
          let axs ← liftCoreM (collectAxioms n)
          IO.println <| "example-json " ++ (Json.mkObj [
            ("name", toJson n.toString), ("axioms", toJson (axs.map toString))]).compress
          for ax in axs do
            unless allowed.contains ax do
              IO.println s!"FAIL unlisted example axiom {ax} in {file}:{(input.fileMap.toPosition (cmd.getPos?.getD 0)).line}"
              throwError "example axiom audit failed"
        let owners := effectiveOwners env n csList
        if hasPartialModifier cmd && !n.isInternalDetail && (n.toString.splitOn "⟪example⟫").length == 1 && (Compiler.isUnsafeRecName? n).isNone then
          unless (← checkOwnerKind n owners) do
            IO.println s!"FAIL unlisted partial declaration {n} in {file}"
            throwError "partial modifier audit failed"
        unless kinds.isEmpty do
          unless (← checkOwnerKind n owners) do
            IO.println s!"FAIL unlisted example trusted declaration {n} [{", ".intercalate kinds}]"
            throwError "example trusted declaration audit failed"
  unless Parser.isTerminalCommand cmd do commands file csList

def configuredComponents : IO (List Concrete.Premises.Component) := do
  match (← IO.getEnv "CONCRETE_TRUSTED_DECLARATIONS") with
  | none => return Concrete.Premises.components
  | some spec =>
      let mut cs := Concrete.Premises.components
      for entry in spec.splitOn ";" do
        if let [compId, decls] := entry.splitOn ":" then
          let names := decls.splitOn "," |>.filter (!·.isEmpty)
          cs := cs.map fun c =>
            if c.id == compId then { c with declarations := c.declarations ++ names } else c
      return cs

unsafe def main (files : List String) : IO UInt32 := do
  commandElabAttribute.addBuiltin ``Parser.Command.declaration `retainExample retainExample
  initSearchPath (← findSysroot)
  let csList ← configuredComponents
  for file in files do
    let text ← IO.FS.readFile file
    let input := Parser.mkInputContext text file
    let (header, ps, msgs) ← Parser.parseHeader input
    enableInitializersExecution
    let (env, msgs) ← processHeader header {} msgs input
    if msgs.hasErrors then
      for msg in msgs.toList do IO.eprintln (← msg.toString)
      return 1
    let action : FrontendM Unit := do
      runCommandElabM do elabCommand (← `(command| noncomputable section))
      commands file csList
    let (_, state) ← StateRefT'.run (action.run { inputCtx := input })
      { commandState := Command.mkState env msgs {}, parserState := ps, cmdPos := ps.pos }
    if state.commandState.messages.hasErrors then
      for msg in state.commandState.messages.toList do IO.eprintln (← msg.toString)
      return 1
  return 0
