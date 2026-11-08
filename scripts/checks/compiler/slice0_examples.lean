import Lean
import Concrete.Premises

/- Gate-only frontend. Lean parses commands in their real import/namespace context.
   Retain anonymous examples as named noncomputable definitions, then audit their
   axiom closures AND all generated declarations (including local recursion).
   No source lexer or textual import insertion participates in the trust check. -/
open Lean Elab Frontend Command

partial def retainExamples (stx : Syntax) (file : String) (map : FileMap) :
    Command.CommandElabM (Syntax × Array Name) := do
  if stx.getKind == ``Parser.Command.example then
    let line := map.toPosition (stx.getPos?.getD 0) |>.line
    let n := Name.mkSimple s!"⟪example⟫{file}:{line}"
    let sig : TSyntax ``Parser.Command.optDeclSig := ⟨stx[1]⟩
    let val : TSyntax ``Parser.Command.declVal := ⟨stx[2]⟩
    let id := mkIdent n
    let replacement ← `(command| noncomputable def $id:ident $sig:optDeclSig $val:declVal)
    return (replacement.raw[1], #[n])
  let mut names := #[]
  let mut out := stx
  for i in [:stx.getNumArgs] do
    let (arg, ns) ← retainExamples stx[i] file map
    out := out.setArg i arg
    names := names ++ ns
  return (out, names)

partial def hasPartialModifier (stx : Syntax) : Bool :=
  if stx.getKind == ``Parser.Command.declaration then
    stx[0].find? (fun s => s.isAtom && s.getAtomVal == "partial") |>.isSome
  else if stx.getKind == ``Parser.Command.mutual then
    stx.getArgs.any hasPartialModifier
  else false

partial def commands (file : String) : FrontendM Unit := do
  updateCmdPos
  let cs ← getCommandState
  let input ← getInputContext
  let scope := cs.scopes.head!
  let (cmd, ps, msgs) := Parser.parseCommand input
    { env := cs.env, options := scope.opts, currNamespace := scope.currNamespace, openDecls := scope.openDecls }
    (← getParserState) cs.messages
  setParserState ps
  setMessages msgs
  let (retained, names) ← runCommandElabM <| retainExamples cmd file input.fileMap
  elabCommandAtFrontend retained
  if !names.isEmpty || hasPartialModifier cmd then
    runCommandElabM do
      let env ← getEnv
      for (n, ci) in env.constants.toList do
        unless cs.env.contains n do
          let axs ← liftCoreM (collectAxioms n)
          IO.println <| "example-json " ++ (Json.mkObj [
            ("name", toJson n.toString), ("axioms", toJson (axs.map toString))]).compress
          for ax in axs do
            unless Concrete.Premises.allowedAxioms.contains ax do
              IO.println s!"FAIL unlisted example axiom {ax} in {file}:{(input.fileMap.toPosition (cmd.getPos?.getD 0)).line}"
              throwError "example axiom audit failed"
          if hasPartialModifier cmd && (Concrete.Premises.declarationOwners n Concrete.Premises.components).isEmpty then
            IO.println s!"FAIL unlisted partial declaration {n} in {file}"
            throwError "partial modifier audit failed"
          let kinds := Concrete.Premises.trustedDeclarationKinds env n ci
          unless kinds.isEmpty do
            throwError "FAIL unlisted example trusted declaration {n} [{", ".intercalate kinds}]"
  unless Parser.isTerminalCommand cmd do commands file

unsafe def main (files : List String) : IO UInt32 := do
  enableInitializersExecution
  initSearchPath (← findSysroot)
  for file in files do
    let text ← IO.FS.readFile file
    -- Conservative candidate selection: comments and strings can only add work.
    if (text.splitOn "example").length ≤ 1 && (text.splitOn "partial").length ≤ 1 then continue
    let input := Parser.mkInputContext text file
    let (header, ps, msgs) ← Parser.parseHeader input
    let (env, msgs) ← processHeader header {} msgs input
    let action : FrontendM Unit := do
      runCommandElabM do elabCommand (← `(command| noncomputable section))
      commands file
    let (_, state) ← StateRefT'.run (action.run { inputCtx := input })
      { commandState := Command.mkState env msgs {}, parserState := ps, cmdPos := ps.pos }
    if state.commandState.messages.hasErrors then
      for msg in state.commandState.messages.toList do IO.eprintln (← msg.toString)
      return 1
  return 0
