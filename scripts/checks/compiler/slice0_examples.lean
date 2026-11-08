import Lean
import Concrete.Premises

/- Gate-only frontend. Lean parses commands in their real import/namespace context.
   Retain anonymous examples as named noncomputable definitions, then audit their
   axiom closures AND all generated declarations (including local recursion).
   No source lexer or textual import insertion participates in the trust check. -/
open Lean Elab Frontend Command

def isIdentChar (c : Char) : Bool :=
  c.isAlphanum || c == '_' || c == '\''

def hasIdentifierWord (text word : String) : Bool :=
  let parts := text.splitOn word
  if parts.length ≤ 1 then false
  else Id.run do
    for i in [:parts.length - 1] do
      let prevOk := match parts[i]!.toList.reverse.head? with
        | some c => !isIdentChar c
        | none => true
      let nextOk := match parts[i + 1]!.toList.head? with
        | some c => !isIdentChar c
        | none => true
      if prevOk && nextOk then return true
    return false

def candidateTokens : List String :=
  ["example", "partial", "while", "repeat", "Loop", "unsafe", "extern", "implemented_by", "csimp", "opaque"]

def hasCandidateToken (text : String) : Bool :=
  candidateTokens.any (hasIdentifierWord text)

def makeExampleDef (exStx : Syntax) (file : String) (map : FileMap) :
    Command.CommandElabM (Syntax × Name) := do
  let pos := map.toPosition (exStx.getPos?.getD 0)
  let n := Name.mkSimple s!"⟪example⟫{file}:{pos.line}:{pos.column}"
  let sig : TSyntax ``Parser.Command.optDeclSig := ⟨exStx[1]⟩
  let val : TSyntax ``Parser.Command.declVal := ⟨exStx[2]⟩
  let id := mkIdent n
  let replacement ← `(command| noncomputable def $id:ident $sig:optDeclSig $val:declVal)
  return (replacement.raw, n)

partial def retainExamples (stx : Syntax) (file : String) (map : FileMap) :
    Command.CommandElabM (Syntax × Array Name) := do
  if stx.getKind == ``Parser.Command.example then
    let (decl, n) ← makeExampleDef stx file map
    return (decl[1], #[n])
  -- Declaration bodies may contain quoted command syntax. Those are data, not
  -- source commands; never rewrite an example inside a macro/template literal.
  if stx.getKind == ``Parser.Command.declaration &&
      stx[1].getKind != ``Parser.Command.example then return (stx, #[])
  if stx.getKind.toString.startsWith "Lean.Parser.Term." then return (stx, #[])
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
  let (retained, names) ← runCommandElabM <| retainExamples cmd file input.fileMap
  elabCommandAtFrontend retained
  runCommandElabM do
    let env ← getEnv
    let allowed := Concrete.Premises.allowedAxioms csList
    let isExOrPartial := !names.isEmpty || hasPartialModifier cmd
    for (n, ci) in env.constants.map₂.toList do
      unless cs.env.contains n || n.isAnonymous do
        let kinds := Concrete.Premises.trustedDeclarationKinds env n ci
        if isExOrPartial || !kinds.isEmpty then
          let axs ← liftCoreM (collectAxioms n)
          IO.println <| "example-json " ++ (Json.mkObj [
            ("name", toJson n.toString), ("axioms", toJson (axs.map toString))]).compress
          for ax in axs do
            unless allowed.contains ax do
              IO.println s!"FAIL unlisted example axiom {ax} in {file}:{(input.fileMap.toPosition (cmd.getPos?.getD 0)).line}"
              throwError "example axiom audit failed"
        let owners := effectiveOwners env n csList
        if hasPartialModifier cmd && !n.isInternalDetail && (Compiler.isUnsafeRecName? n).isNone then
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
  enableInitializersExecution
  initSearchPath (← findSysroot)
  let csList ← configuredComponents
  for file in files do
    let text ← IO.FS.readFile file
    -- Token-bounded candidate selection: comments and strings can only add work,
    -- while substrings like "examples" in proof modules do not trigger re-elaboration.
    unless files.length == 1 || hasCandidateToken text do continue
    let input := Parser.mkInputContext text file
    let (header, ps, msgs) ← Parser.parseHeader input
    let (env, msgs) ← processHeader header {} msgs input
    let action : FrontendM Unit := do
      runCommandElabM do elabCommand (← `(command| noncomputable section))
      commands file csList
    let (_, state) ← StateRefT'.run (action.run { inputCtx := input })
      { commandState := Command.mkState env msgs {}, parserState := ps, cmdPos := ps.pos }
    if state.commandState.messages.hasErrors then
      for msg in state.commandState.messages.toList do IO.eprintln (← msg.toString)
      return 1
  return 0
