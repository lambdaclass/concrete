import Lean

open Lean Meta

-- All inspection happens in MetaM: instantiate dependent binders before reducing aliases
-- and field families. Inductive semantic ASTs remain opaque carriers: their field
-- names are not local spellings. Structure fields are inspected in their local context.
def normalType (e : Expr) : MetaM Expr := do
  withTransparency .all <| whnf (← instantiateMVars e)

def withStructureFields (e : Expr) (k : Array Expr → MetaM α) : MetaM α := do
  let e ← normalType e
  let env ← getEnv
  let .const n levels := e.getAppFn | return ← k #[]
  if !isStructure env n then return ← k #[]
  let some (.inductInfo info) := env.find? n | return ← k #[]
  if e.getAppNumArgs != info.numParams then return ← k #[]
  let t ← inferType (mkAppN (mkConst info.ctors.head! levels) e.getAppArgs)
  forallTelescopeReducing t fun fields _ => do
    let types ← fields.mapM fun field => do normalType (← inferType field)
    k types

def combine (a b : Bool × Bool) : Bool × Bool := (a.1 || b.1, a.2 || b.2)

-- Carrier summaries cross containers and structure fields. Product payloads preserve
-- container boundaries: a field-name/Atom pair is not itself a local spelling.
partial def carriers (e : Expr) (seen : List Name := []) (flatten : Bool := false) (records : Bool := true) : MetaM (Bool × Bool) := do
  let e ← normalType e
  if e.isConstOf `ConcreteSpec.Var then return (true, false)
  if e.isConstOf `String then return (false, true)
  if e.isForall then
    return ← forallTelescopeReducing e fun xs result => do
      let mut found ← carriers result seen false records
      for x in xs do found := combine found (← carriers (← inferType x) seen false records)
      return found
  let .const n _ := e.getAppFn | return (false, false)
  if seen.contains n then return (false, false)
  let ancestors := seen
  let seen := n :: seen
  withStructureFields e fun fields => do
    if fields.size == 1 then return ← carriers fields[0]! seen false records
    if n == `Prod || n == `Sigma || n == `PSigma then
      let mut found := (false, false)
      for field in fields do
        let a ← carriers field seen flatten records
        if !flatten && !a.1 && !a.2 then return (false, false)
        found := combine found a
      return found
    -- Includes nested records and indexed predicates, including Prop.
    let mut found := (false, false)
    if records then
      for field in fields do
        found := combine found (← carriers field seen false records)
    for arg in e.getAppArgs do
      if ← isType arg then
        found := combine found (← carriers arg ancestors false records)
      else
        found := combine found (← carriers (← inferType arg) ancestors false records)
    return found

-- At an interface's outer telescope, a local and a field-name parameter are legal.
-- Inside any parameter, field or index, ALL binders and the result contribute:
-- Var → String → Bool and Var → String → Prop are tables too.
partial def paired (e : Expr) (seen : List Name := []) (interface : Bool := false) : MetaM Bool := do
  let e ← normalType e
  if !interface then
    let a ← carriers e [] true
    if a.1 && a.2 then return true
  if e.isForall then
    return ← forallTelescopeReducing e fun xs result => do
      -- The outer interface also forbids direct name-conversion functions. At this
      -- boundary semantic records/judgments are opaque endpoints: an AST's field names
      -- do not make an AST conversion a Var/String conversion. Each endpoint is still
      -- checked independently below with full field/index inspection.
      let output ← if ← isProp result then pure (false, false) else carriers result [] true false
      for x in xs do
        let domain ← inferType x
        let input ← carriers domain [] false false
        if (input.1 && !input.2 && output.2 && !output.1) ||
            (input.2 && !input.1 && output.1 && !output.2) then return true
        if ← paired domain seen then return true
      paired result seen
  let .const n _ := e.getAppFn | return false
  if seen.contains n then return false
  let seen := n :: seen
  let fieldsPaired ← withStructureFields e fun fields => do
    let mut found := (false, false)
    for field in fields do
      found := combine found (← carriers field [] true)
      if ← paired field seen then return true
    return found.1 && found.2
  if fieldsPaired then return true
  for arg in e.getAppArgs do
    if ← isType arg then
      if ← paired arg seen then return true
  return false

-- The only presentation records in Examples are complete programs and function bodies
-- (input context, statements, return atom). No operation or arbitrary Elaborated payload is exempt.
def presentationRecord (type : Expr) : Bool := Id.run do
  if !type.getAppFn.isConstOf `ConcreteSpec.Elaborated then return false
  let args := type.getAppArgs
  if args.size != 1 then return false
  let code := args[0]!
  if code.isConstOf `ConcreteSpec.Program then return true
  let prod := mkConst `Prod [Level.zero, Level.zero]
  let listStmt := mkApp (mkConst `List [Level.zero]) (mkConst `ConcreteSpec.Stmt)
  let body := mkApp2 prod (mkConst `ConcreteSpec.Ctx)
    (mkApp2 prod listStmt (mkConst `ConcreteSpec.Atom))
  return code == body

def main (args : List String) : IO Unit := do
  let [target] := args | throw (IO.userError "expected one import module")
  unsafe enableInitializersExecution
  initSearchPath (← findSysroot)
  let env ← importModules #[{module := target.toName}] {} (loadExts := true)
  let some (.ctorInfo ctor) := env.find? `ConcreteSpec.Var.mk
    | throw (IO.userError "missing Var constructor")
  unless ctor.numFields == 1 && ctor.numParams == 0 &&
      ctor.type.bindingDomain! == mkConst `Nat do
    throw (IO.userError "Var must have exactly one Nat field")
  unless isStructure env `ConcreteSpec.LocalNames do
    throw (IO.userError "LocalNames must be a nominal structure")
  -- Reverse reachability visits each dependency edge once, including recursive components.
  let mut users : Std.HashMap Name (Array Name) := {}
  for (n, ci) in env.constants.toList do
    for c in ci.getUsedConstantsAsSet do
      users := users.insert c ((users[c]?.getD #[]).push n)
  let mut tainted : Std.HashSet Name := {}
  let mut queue := #[`ConcreteSpec.LocalNames, `ConcreteSpec.Elaborated]
  -- Inspect every declaration's instantiated telescope, including inductive indices
  -- and Prop-valued judgments. The same analysis handles fields and function values.
  let inspect : MetaM (Array Name) := do
    let mut seeds := #[]
    for (n, ci) in env.constants.toList do
      let some i := env.getModuleIdxFor? n | continue
      let m := env.header.moduleNames[i.toNat]!.toString
      if !(m == "ConcreteSpec.Spec" || m.startsWith "ConcreteSpec.Spec.") &&
          !isStructure env n then continue
      -- Lean-generated eliminators expose constructor fields as continuation arguments.
      -- They are not user semantic interfaces; dependencies still propagate through them.
      if isMatcherCore env n || isAuxRecursor env n || (ci matches .recInfo _) then continue
      let predicate ← forallTelescopeReducing ci.type fun _ result => do
        return (← normalType result) == mkSort Level.zero
      let interface := !predicate && match ci with
        | .inductInfo _ => false
        | _ => true
      let structurePair ← if isStructure env n then
        forallTelescopeReducing ci.type fun params _ => do
          let type := mkAppN (mkConst n (ci.levelParams.map mkLevelParam)) params
          paired type
        else pure false
      if structurePair || (← paired ci.type [] interface) then seeds := seeds.push n
    return seeds
  let (seeds, _) ← (inspect.run' {}).toIO { fileName := "local_identity", fileMap := default } { env := env }
  queue := queue ++ seeds
  let mut pos := 0
  while pos < queue.size do
    let n := queue[pos]!
    pos := pos + 1
    if tainted.contains n then continue
    tainted := tainted.insert n
    queue := queue ++ (users[n]?.getD #[])
  let mut bad := false
  for (n, ci) in env.constants.toList do
    let some i := env.getModuleIdxFor? n | continue
    let m := env.header.moduleNames[i.toNat]!.toString
    if !(m == "ConcreteSpec.Spec" || m.startsWith "ConcreteSpec.Spec.") then continue
    -- Examples contains conformance data, never semantic rules. Only its presentation
    -- records and the namesBound validator (including its generated matcher) may depend on
    -- diagnostics. Anonymous example checks are not exported semantic declarations.
    if m == "ConcreteSpec.Spec.Examples" &&
        (presentationRecord ci.type ||
         n == `ConcreteSpec.Examples.namesBound.match_1 ||
         n == `ConcreteSpec.Examples.namesBound) then continue
    if tainted.contains n then
      IO.eprintln s!"semantic diagnostics dependency: {n}"
      bad := true
  if bad then throw (IO.userError "semantic definitions refer to LocalNames/Elaborated")
  IO.println "numeric Var and semantic diagnostics separation: pass"
