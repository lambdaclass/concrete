import Lean
import Concrete.Premises

/- Gate-only frontend. Lean parses commands in their real import/namespace context.
   Retain anonymous examples as named noncomputable definitions, then audit every
   file-local declaration (and any module-local trust attribute/extension target)
   against the file's FINAL environment after all commands have been elaborated.
   No source lexer or textual import insertion participates in the trust check. -/
open Lean Elab Frontend Command

structure FileAuditState where
  nextCmdIdx : Nat := 0
  declMeta : Std.HashMap Name (Nat × Nat) := {}
  extraCSimp : NameSet := {}

def sortLocalDecls (declMeta : Std.HashMap Name (Nat × Nat))
    (xs : Array (Name × ConstantInfo)) : Array (Name × ConstantInfo) :=
  xs.qsort fun a b =>
    let ca := (declMeta.get? a.1).map (·.1) |>.getD 0
    let cb := (declMeta.get? b.1).map (·.1) |>.getD 0
    if ca != cb then ca < cb
    else
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
def retainExample (retained : IO.Ref NameSet) : CommandElab := fun stx => do
  unless stx[1].getKind == ``Parser.Command.example do throwUnsupportedSyntax
  let (replacement, userN) ← makeExampleDef stx (← getFileName) (← getFileMap)
  retained.modify (·.insert userN)
  elabCommand replacement

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

def recordCommandState (cmd : Syntax) (auditRef : IO.Ref FileAuditState) : FrontendM Unit := do
  let cs ← getCommandState
  let input ← getInputContext
  let env := cs.env
  let line := (input.fileMap.toPosition (cmd.getPos?.getD 0)).line
  let st ← auditRef.get
  let cmdIdx := st.nextCmdIdx
  let mut declMeta := st.declMeta
  for (n, _) in env.constants.map₂.toList do
    unless n.isAnonymous || declMeta.contains n do
      declMeta := declMeta.insert n (cmdIdx, line)
  for n in Concrete.Premises.moduleAttributedDeclarations env do
    unless n.isAnonymous || declMeta.contains n do
      declMeta := declMeta.insert n (cmdIdx, line)
  let extraCSimp := (Compiler.CSimp.ext.getState env).thmNames.foldStage2
    (fun acc n _ => acc.insert n) st.extraCSimp
  auditRef.set { nextCmdIdx := cmdIdx + 1, declMeta, extraCSimp }

partial def commands (auditRef : IO.Ref FileAuditState) : FrontendM Unit := do
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
  recordCommandState cmd auditRef
  unless Parser.isTerminalCommand cmd do commands auditRef

def auditFileEnvironment (file : String) (csList : List Concrete.Premises.Component)
    (retained : IO.Ref NameSet) (auditRef : IO.Ref FileAuditState)
    (initConsts : PHashMap Name ConstantInfo) : CommandElabM Unit := do
  let env ← getEnv
  let retainedSet ← retained.get
  let st ← auditRef.get
  let allowed := Concrete.Premises.allowedAxioms csList
  let mut localDecls : Array (Name × ConstantInfo) := #[]
  let mut seen : NameSet := {}
  for (n, ci) in env.constants.map₂.toList do
    unless initConsts.contains n || n.isAnonymous do
      seen := seen.insert n
      localDecls := localDecls.push (n, ci)
  for n in Concrete.Premises.moduleAttributedDeclarations env ++ st.extraCSimp.toList do
    unless seen.contains n || n.isAnonymous do
      if let some ci := env.find? n then
        seen := seen.insert n
        localDecls := localDecls.push (n, ci)
  for (n, ci) in sortLocalDecls st.declMeta localDecls do
    let userN := (privateToUserName? n).getD n
    let axs ← liftCoreM (collectAxioms n)
    if retainedSet.contains userN then
      IO.println <| "example-json " ++ (Json.mkObj [
        ("name", toJson n.toString), ("axioms", toJson (axs.map toString))]).compress
    let line := (st.declMeta.get? n).map (·.2) |>.getD 1
    for ax in axs do
      unless allowed.contains ax do
        IO.println s!"FAIL unlisted example axiom {ax} in {file}:{line}"
        throwError "example axiom audit failed"
    let kinds ← liftCoreM <| Concrete.Premises.trustedDeclarationKinds env n ci st.extraCSimp
    unless kinds.isEmpty do
      let owners := Concrete.Premises.declarationOwners n csList (some env)
      unless (← checkOwnerKind n owners) do
        IO.println s!"FAIL unlisted example trusted declaration {n} [{", ".intercalate kinds}]"
        throwError "example trusted declaration audit failed"

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
  let retained ← IO.mkRef NameSet.empty
  let auditRef ← IO.mkRef ({} : FileAuditState)
  commandElabAttribute.addBuiltin ``Parser.Command.declaration `retainExample (retainExample retained)
  initSearchPath (← findSysroot)
  let csList ← configuredComponents
  for file in files do
    retained.set {}
    auditRef.set {}
    let text ← IO.FS.readFile file
    let input := Parser.mkInputContext text file
    let (header, ps, msgs) ← Parser.parseHeader input
    enableInitializersExecution
    let (env, msgs) ← processHeader header {} msgs input
    if msgs.hasErrors then
      for msg in msgs.toList do IO.eprintln (← msg.toString)
      return 1
    let initConsts := env.constants.map₂
    let action : FrontendM Unit := do
      runCommandElabM do elabCommand (← `(command| noncomputable section))
      commands auditRef
      runCommandElabM do auditFileEnvironment file csList retained auditRef initConsts
    let (_, state) ← StateRefT'.run (action.run { inputCtx := input })
      { commandState := Command.mkState env msgs {}, parserState := ps, cmdPos := ps.pos }
    if state.commandState.messages.hasErrors then
      for msg in state.commandState.messages.toList do IO.eprintln (← msg.toString)
      return 1
  return 0
