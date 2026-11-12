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

private partial def hasAtom (token : String) (s : Syntax) : Bool :=
  match s with
  | .atom _ value => value == token
  | _ => s.getArgs.any (hasAtom token)

private partial def declarationModifiers (s : Syntax) : Option Syntax :=
  if s.isOfKind ``Parser.Command.declModifiers then some s
  else s.getArgs.findSome? declarationModifiers

private def noncomputableModifier (s : Syntax) : Bool :=
  (declarationModifiers s).any (hasAtom "noncomputable")

-- noncomputable precedes unsafe/partial/nonrec in Lean's modifier grammar.
private def noncomputableInsert (s : Syntax) : Option Nat := do
  let modifiers ← declarationModifiers s
  let pos? := (modifiers.getArgs[5]?.bind Syntax.getPos?) <|>
              (modifiers.getArgs[6]?.bind Syntax.getPos?)
  return (← pos?).byteIdx

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
      ("noncomputable", toJson (noncomputableModifier stx)),
      ("noncomputableInsert", toJson (noncomputableInsert stx)),
      ("syntax", toJson stx.getKind.toString), ("example", toJson (containsExample stx)), ("exampleToken", toJson (exampleToken stx)), ("line", toJson start.line),
      ("endLine", toJson stop.line), ("identifiers", toJson (identifiers stx))]
    commands ctx env next messages

-- Observe the environment after each elaborated command. This is extraction provenance,
-- including zero-range generated helpers; names and term-consumer guesses are insufficient.
private partial def nameJson : Name → Json
  | .anonymous => Json.arr #[]
  | .str p s => Json.arr #[toJson "str", nameJson p, toJson s]
  | .num p n => Json.arr #[toJson "num", nameJson p, toJson n]

private partial def levelJson : Level → Json
  | .zero => toJson "zero"
  | .succ l => Json.arr #[toJson "succ", levelJson l]
  | .max a b => Json.arr #[toJson "max", levelJson a, levelJson b]
  | .imax a b => Json.arr #[toJson "imax", levelJson a, levelJson b]
  | .param n => Json.arr #[toJson "param", nameJson n]
  | .mvar n => Json.arr #[toJson "mvar", nameJson n.name]

private structure ShapeState where
  memo : Std.HashMap Expr Nat := {}
  nodes : Array Json := #[]

-- Serialize a DAG, not an expanded pretty-print of a shared expression. Metadata does not
-- affect the kernel term. Like scripts/proofs/theorems.lean, erase binder presentation and
-- let nondep flags (ignored by Expr BEq); retain computational nodes and declaration fields.
private partial def exprNode (e : Expr) : StateM ShapeState Nat := do
  let e := e.consumeMData
  if let some i ← modifyGet (fun s => (s.memo[e]?, s)) then return i
  let node : Json ← match e with
    | .bvar n => pure <| Json.arr #[toJson "bvar", toJson n]
    | .fvar n => pure <| Json.arr #[toJson "fvar", nameJson n.name]
    | .mvar n => pure <| Json.arr #[toJson "mvar", nameJson n.name]
    | .sort l => pure <| Json.arr #[toJson "sort", levelJson l]
    | .const n ls => pure <| Json.arr #[toJson "const", nameJson n, toJson (ls.map levelJson)]
    | .app f a => do pure <| Json.arr #[toJson "app", toJson (← exprNode f), toJson (← exprNode a)]
    | .lam _ t b _ => do pure <| Json.arr #[toJson "lam", toJson (← exprNode t), toJson (← exprNode b)]
    | .forallE _ t b _ => do pure <| Json.arr #[toJson "forall", toJson (← exprNode t), toJson (← exprNode b)]
    | .letE _ t v b _ => do pure <| Json.arr #[toJson "let", toJson (← exprNode t), toJson (← exprNode v), toJson (← exprNode b)]
    | .lit l => pure <| Json.arr #[toJson "lit", toJson (reprStr l)]
    | .mdata _ _ => unreachable!
    | .proj n i b => do pure <| Json.arr #[toJson "proj", nameJson n, toJson i, toJson (← exprNode b)]
  modifyGet fun s =>
    let i := s.nodes.size
    (i, { memo := s.memo.insert e i, nodes := s.nodes.push node })

private def shape (ci : ConstantInfo) : String := Id.run do
  let build : StateM ShapeState Json := do
    let type ← exprNode ci.type
    let extra ← match ci with
      -- An existing proof of exactly the same kernel type can be retained.
      | .thmInfo _ => pure <| toJson "theorem"
      | .defnInfo v =>
        let hints := match v.hints with
          | .opaque => toJson "opaque" | .abbrev => toJson "abbrev"
          | .regular h => Json.arr #[toJson "regular", toJson h.toNat]
        return Json.arr #[toJson "def", toJson (← exprNode v.value), hints,
          toJson (reprStr v.safety), toJson (v.all.map nameJson)]
      | .opaqueInfo v => do pure <| Json.arr #[toJson "opaque", toJson (← exprNode v.value), toJson (v.all.map nameJson)]
      | .recInfo v =>
        let rules ← v.rules.mapM fun r => do
          return Json.arr #[nameJson r.ctor, toJson r.nfields, toJson (← exprNode r.rhs)]
        pure <| Json.arr #[toJson "recursor", toJson (v.all.map nameJson), toJson v.numParams,
          toJson v.numIndices, toJson v.numMotives, toJson v.numMinors, toJson rules, toJson v.k]
      | .ctorInfo v => pure <| Json.arr #[toJson "constructor", nameJson v.induct, toJson v.cidx, toJson v.numParams, toJson v.numFields]
      | .inductInfo v => pure <| Json.arr #[toJson "inductive", toJson (v.all.map nameJson), toJson (v.ctors.map nameJson), toJson v.numParams,
          toJson v.numIndices, toJson v.numNested, toJson v.isRec, toJson v.isReflexive]
      | .axiomInfo _ => pure <| toJson "axiom"
      | .quotInfo v => pure <| toJson (match v.kind with
          | .type => "quot.type" | .ctor => "quot.ctor" | .lift => "quot.lift" | .ind => "quot.ind")
    return Json.arr #[toJson (ci.levelParams.map nameJson), toJson type, toJson ci.isUnsafe, toJson ci.isPartial, extra]
  let (payload, state) := build.run {}
  return Json.compress (Json.arr #[payload, Json.arr state.nodes])

private def fingerprint (input output : IO.FS.Handle) (ci : ConstantInfo) : IO String := do
  input.putStrLn (Json.compress (toJson (shape ci)))
  input.flush
  let digest := (← output.getLine).trimAscii.toString
  unless digest.length == 64 do throw (IO.userError "kernel fingerprint worker failed")
  return digest

private partial def docComment? (s : Syntax) : Option String :=
  if s.isOfKind ``Parser.Command.docComment then some (TSyntax.getDocString ⟨s⟩)
  else s.getArgs.findSome? docComment?

private def resolvedMetadata (ctx : Parser.InputContext) (stx : Syntax)
    (state : Elab.Command.State) : IO Unit := do
  let scope := state.scopes.head!
  let resolve (id : Name) : IO String := do
    let candidates := ResolveName.resolveGlobalName state.env scope.opts scope.currNamespace scope.openDecls id
    let names := candidates.filterMap fun (n, fields) => if fields.isEmpty then some n else none
    match names with
    | [n] => return n.toString
    | _ => throw (IO.userError s!"unresolved or ambiguous inventory annotation: {id}")
  let mut targets : Array String := #[]
  if stx.isOfKind ``Parser.Command.attribute then
    for id in stx[4].getArgs do targets := targets.push (← resolve id.getId)
  let mut laws : Array String := #[]
  let mut mutants : Array String := #[]
  if let some doc := docComment? stx then
    if doc.trimAscii.toString.startsWith "Instance (" || doc.trimAscii.toString.startsWith "Planted (" then
      let parts := doc.splitOn "`"
      for i in [:parts.length / 2] do
        let label := parts[2*i]!
        let name := parts[2*i+1]!.toName
        if label.endsWith "law " then laws := laws.push (← resolve name)
        if label.endsWith "mutant " then mutants := mutants.push (← resolve name)
  emit [("record", toJson "resolved-command"), ("file", toJson ctx.fileName),
    ("line", toJson (ctx.fileMap.toPosition (stx.getPos?.getD 0)).line),
    ("targets", toJson targets), ("laws", toJson laws), ("mutants", toJson mutants)]

private partial def originCommands (input output : IO.FS.Handle) (ctx : Parser.InputContext) (before : NameSet)
    (snap : Lean.Language.Lean.CommandParsedSnapshot) : IO Environment := do
  let state := snap.elabSnap.resultSnap.get.cmdState
  let after := state.env
  let stx := snap.stx
  resolvedMetadata ctx stx state
  let start := ctx.fileMap.toPosition (stx.getPos?.getD 0)
  let stop := ctx.fileMap.toPosition (stx.getTailPos?.getD 0)
  let mut names := before
  for (n, ci) in after.constants.map₂.toList do
    unless before.contains n do
      emit [("record", toJson "origin"), ("name", toJson n.toString),
        ("file", toJson ctx.fileName), ("line", toJson start.line),
        ("endLine", toJson stop.line), ("shape", toJson (← fingerprint input output ci)),
        ("uses", toJson (ci.getUsedConstantsAsSet.toList.map Name.toString))]
      names := names.insert n
  if state.messages.hasErrors then
    for msg in state.messages.toList do IO.eprintln (← msg.toString)
    throw (IO.userError "origin elaboration failed")
  if let some next := snap.nextCmdSnap? then
    originCommands input output ctx names next.get
  else
    return after

unsafe def main (args : List String) : IO Unit := do
  initSearchPath (← findSysroot)
  enableInitializersExecution
  let hashScript := "import sys,json,hashlib\nfor line in sys.stdin:\n if not line.strip(): break\n print(hashlib.sha256(json.loads(line).encode()).hexdigest(),flush=True)"
  let child ← IO.Process.spawn { cmd := "python3", args := #["-u", "-c", hashScript], stdin := .piped, stdout := .piped }
  let input := child.stdin
  let output := child.stdout
  try
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
    | ["origins", file, mod, requests] =>
      let ctx := Parser.mkInputContext (← IO.FS.readFile file) file
      let (header, ps, messages) ← Parser.parseHeader ctx
      -- Match runFrontend's defaults: async elaboration affects auxiliary name generators.
      let opts := ({} : Options).setBool `autoImplicit false
        |>.setBool `warningAsError true
        |>.setBool `Elab.async true
        -- Retain syntax in snapshots so origin ranges survive command-line minimization.
        |>.setBool `internal.cmdlineSnapshots false
      let (env, messages) ← Elab.processHeader ⟨header⟩ opts messages ctx
        (mainModule := mod.toName)
      let state := Elab.Command.mkState env messages opts
      let task ← Language.Lean.processCommands ctx ps state
      let final ← originCommands input output ctx {} task.get
      if requests != "-" then
        let targets ← IO.FS.lines requests
        let action : CoreM Unit := do
          for target in targets do
            let n := target.toName
            unless (← getEnv).contains n do
              try executeReservedNameAction n
              catch ex => if ex.isInterrupt then throw ex else pure ()
        let (_, realized) ← action.toIO { fileName := file, fileMap := ctx.fileMap, options := opts } { env := final }
        let existing := final.constants.map₂
        for (n, ci) in realized.env.constants.map₂.toList do
          unless existing.contains n do
            emit [("record", toJson "origin"), ("name", toJson n.toString),
              ("file", toJson file), ("line", toJson (0 : Nat)), ("endLine", toJson (0 : Nat)),
              ("shape", toJson (← fingerprint input output ci)),
              ("uses", toJson (ci.getUsedConstantsAsSet.toList.map Name.toString))]
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
          ("shape", toJson (← fingerprint input output ci)), ("kind", toJson kind), ("userName", toJson (privateToUserName n).toString), ("module", toJson m.toString),
          ("line", toJson (ranges.map (·.selectionRange.pos.line) |>.getD 0)),
          ("uses", toJson (ci.getUsedConstantsAsSet.toList.map Name.toString))]
    | _ => throw (IO.userError "expected: parse FILE | constants MODULES_FILE")
  finally
    input.putStrLn ""
    input.flush
    let _ ← child.wait
