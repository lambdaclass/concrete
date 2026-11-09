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
  extraAttributed : NameSet := {}

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

/- Retain trust-extension events (`csimp`, `cpass`, `extern`, `implemented_by`,
   `init`, `builtin_init`, `export`) during recursive command elaboration and
   attribute registration, before any local scope (`attribute [local ...] ... in`,
   `open ... in`, `section ... end`, `namespace ... end`, or macro scope) closes. -/
def recordEnvTrustState (env : Environment) (fileMap : FileMap) (stx : Syntax)
    (auditRef : IO.Ref FileAuditState) (includeAllNewConsts : Bool := false) : IO Unit := do
  let line := (fileMap.toPosition (stx.getPos?.getD 0)).line
  let st ← auditRef.get
  let cmdIdx := st.nextCmdIdx
  let mut declMeta := st.declMeta
  let mut extraAttributed := st.extraAttributed
  if includeAllNewConsts then
    for (n, _) in env.constants.map₂.toList do
      unless n.isAnonymous || declMeta.contains n do
        declMeta := declMeta.insert n (cmdIdx, line)
  for n in Concrete.Premises.moduleAttributedDeclarations env do
    unless n.isAnonymous do
      extraAttributed := extraAttributed.insert n
      unless declMeta.contains n do
        declMeta := declMeta.insert n (cmdIdx, line)
  let extraCSimp := (Compiler.CSimp.ext.getState env).thmNames.foldStage2
    (fun acc n _ => acc.insert n) st.extraCSimp
  let extraCSimp := (Compiler.CSimp.ext.ext.getState env).stateStack.foldl
    (fun acc top => top.state.thmNames.foldStage2 (fun a n _ => a.insert n) acc) extraCSimp
  auditRef.set { nextCmdIdx := cmdIdx + 1, declMeta, extraCSimp, extraAttributed }

def installAuditHooks (auditRef : IO.Ref FileAuditState) : IO Unit := do
  attributeMapRef.modify fun m =>
    m.fold (init := {}) fun acc name impl =>
      acc.insert name { impl with
        add := fun decl stx kind => do
          impl.add decl stx kind
          recordEnvTrustState (← getEnv) (← getFileMap) stx auditRef
      }
  commandElabAttribute.tableRef.modify fun tbl =>
    tbl.fold (init := ({} : KeyedDeclsAttribute.Table CommandElab)) fun acc key entries =>
      let includeConsts :=
        key == ``Parser.Command.declaration ||
        key == ``Parser.Command.«mutual» ||
        key == ``Parser.Command.«initialize»
      let entries' := entries.map fun entry =>
        { entry with
          value := fun stx => do
            recordEnvTrustState (← getEnv) (← getFileMap) stx auditRef
            entry.value stx
            recordEnvTrustState (← getEnv) (← getFileMap) stx auditRef includeConsts
        }
      acc.insert key entries'

def recordCommandState (cmd : Syntax) (auditRef : IO.Ref FileAuditState) : FrontendM Unit := do
  let cs ← getCommandState
  let input ← getInputContext
  recordEnvTrustState cs.env input.fileMap cmd auditRef true

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
  IO.withStdout (← IO.getStderr) (elabCommandAtFrontend cmd)
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
  for n in Concrete.Premises.moduleAttributedDeclarations env ++ st.extraAttributed.toList ++ st.extraCSimp.toList do
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

def isHeavyImport (m : Name) : Bool :=
  m.getRoot == `Lean || m.getRoot == `LeanCompilerBackend ||
  m == `Concrete || m == `Concrete.Premises || (`Concrete.Backend.LCB).isPrefixOf m

def isLightHeader (file : String) : IO Bool := do
  let text ← IO.FS.readFile file
  let (header, _, _) ← Parser.parseHeader (Parser.mkInputContext text file)
  return !(headerToImports header).any (fun imp => isHeavyImport imp.module)

abbrev HeaderCacheKey := Bool × Array (Name × Bool × Bool × Bool)

def headerCacheKey (header : HeaderSyntax) : HeaderCacheKey :=
  (header.isModule, (headerToImports header).map fun i => (i.module, i.importAll, i.isExported, i.isMeta))

unsafe def auditSingleFile (retained : IO.Ref NameSet) (auditRef : IO.Ref FileAuditState)
    (envCache : IO.Ref (Std.HashMap HeaderCacheKey Environment))
    (csList : List Concrete.Premises.Component) (file : String) : IO UInt32 := do
  retained.set {}
  auditRef.set {}
  let text ← IO.FS.readFile file
  let input := Parser.mkInputContext text file
  let (header, ps, msgs) ← Parser.parseHeader input
  if msgs.hasErrors then
    for msg in msgs.toList do IO.eprintln (← msg.toString)
    return 1
  let key := headerCacheKey header
  let cached? := (← envCache.get).get? key
  let (env, msgs) ← match cached? with
    | some cachedEnv => pure (cachedEnv, msgs)
    | none =>
        enableInitializersExecution
        let (env, msgs) ← processHeader header {} msgs input
        unless msgs.hasErrors do
          envCache.modify (·.insert key env)
        pure (env, msgs)
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

unsafe def auditMain (csList : List Concrete.Premises.Component) (files : List String) : IO UInt32 := do
  let retained ← IO.mkRef NameSet.empty
  let auditRef ← IO.mkRef ({} : FileAuditState)
  let envCache ← IO.mkRef ({} : Std.HashMap HeaderCacheKey Environment)
  commandElabAttribute.addBuiltin ``Parser.Command.declaration `retainExample (retainExample retained)
  installAuditHooks auditRef
  initSearchPath (← findSysroot)
  let (perFile, inProcess, files) := match files with
    | "--per-file" :: rest => (true, true, rest)
    | "--in-process" :: rest => (false, true, rest)
    | _ => (false, files.length <= 2, files)
  if perFile then
    for file in files do
      let started ← IO.monoMsNow
      let rc ← try
        auditSingleFile retained auditRef envCache csList file
      catch e =>
        IO.eprintln e.toString
        pure 1
      (← IO.getStdout).flush
      (← IO.getStderr).flush
      IO.println s!"__FILE_MILLISECONDS__ {file} {(← IO.monoMsNow) - started}"
      IO.println s!"__FILE_RC__ {file} {rc}"
      (← IO.getStdout).flush
    return 0
  if inProcess then
    for file in files do
      let rc ← auditSingleFile retained auditRef envCache csList file
      if rc != 0 then return rc
    return 0
  let mut lightFiles : List String := []
  let mut heavyFiles : List String := []
  for file in files do
    if (← isLightHeader file) then
      lightFiles := lightFiles ++ [file]
    else
      heavyFiles := heavyFiles ++ [file]
  let runChild (args : List String) : IO UInt32 := do
    let out ← IO.Process.output {
      cmd := (← IO.appPath).toString
      args := (#["--run", "scripts/checks/compiler/slice0_examples.lean"] ++ args.toArray)
    }
    unless out.stdout.isEmpty do IO.print out.stdout
    unless out.stderr.isEmpty do IO.eprint out.stderr
    return out.exitCode
  if heavyFiles.isEmpty then
    for file in lightFiles do
      let rc ← auditSingleFile retained auditRef envCache csList file
      if rc != 0 then return rc
    return 0
  unless lightFiles.isEmpty do
    let rc ← runChild ("--in-process" :: lightFiles)
    if rc != 0 then return rc
  let lastHeavy? := heavyFiles.getLast?
  for file in heavyFiles do
    if some file != lastHeavy? then
      let rc ← runChild [file]
      if rc != 0 then return rc
    else
      let rc ← auditSingleFile retained auditRef envCache csList file
      if rc != 0 then return rc
  return 0

-- Production authority is exclusively the checked-in registry.
unsafe def main (files : List String) : IO UInt32 :=
  auditMain Concrete.Premises.components files
