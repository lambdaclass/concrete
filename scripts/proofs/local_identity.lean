import Lean

open Lean Meta

-- Normalize aliases and dependent field families in their instantiated contexts.
def normalType (e : Expr) : MetaM Expr := do
  withTransparency .all <| whnf (← instantiateMVars e)

def combine (a b : Bool × Bool) : Bool × Bool := (a.1 || b.1, a.2 || b.2)

-- Declaration names are nominal text, not local spellings. All other inductive
-- constructors are inspected, including ASTs, containers, structures and indices.
partial def carriers (e : Expr) (seen : List Name := []) : StateRefT (Std.HashSet Expr) MetaM (Bool × Bool) := do
  if e.isConstOf `ConcreteSpec.Numbers then return (false, false)
  -- Preserve proposition indices as declared: reducing membership on numeric
  -- data expands its list implementation and discards the data annotation.
  let e ← if ← isProp e then instantiateMVars e else normalType e
  if e.isConstOf `ConcreteSpec.Var then return (true, false)
  if e.isConstOf `ConcreteSpec.Name then return (false, false)
  -- Spellings occur as strings, characters, scalar lists and UTF-8 byte lists
  -- in the surface spec. Nat alone remains numeric (in particular, Var.id).
  let textList := e.isAppOfArity `List 1 &&
    (e.getAppArgs[0]!.isConstOf `Nat || e.getAppArgs[0]!.isConstOf `UInt8)
  if textList || e.isConstOf `Char || e.isConstOf `String || e.isConstOf `ConcreteSpec.LocalNames then return (false, true)
  if e.isForall then
    return ← forallTelescopeReducing e fun xs result => do
      let mut found ← carriers result seen
      for x in xs do found := combine found (← carriers (← inferType x) seen)
      return found
  let .const n levels := e.getAppFn | return (false, false)
  let mut found := (false, false)
  let mut signature ← inferType e.getAppFn
  for arg in e.getAppArgs do
    signature ← normalType signature
    if ← isType arg then
      found := combine found (← carriers arg seen)
    else
      -- Use the declared index type: inferring a cons expression would erase
      -- the Numbers annotation and mistake numeric data for spelling scalars.
      found := combine found (← carriers signature.bindingDomain! seen)
    signature := signature.bindingBody!.instantiate1 arg
  if seen.contains n then return found
  if (← get).contains e then return found
  modify (·.insert e)
  let ancestors := n :: seen
  let env ← getEnv
  if let some (.inductInfo info) := env.find? n then
    if e.getAppNumArgs >= info.numParams then
      for ctor in info.ctors do
        let t ← inferType (mkAppN (mkConst ctor levels) (e.getAppArgs.extract 0 info.numParams))
        let payload ← forallTelescopeReducing t fun fields _ => do
          let mut payload := (false, false)
          for field in fields do
            payload := combine payload (← carriers (← inferType field) ancestors)
          return payload
        found := combine found payload
  return found

-- Every semantic telescope, parameter, index and nested payload obeys the same
-- rule: locals may accompany declaration names, but never local spellings.
def paired (e : Expr) : MetaM Bool := do
  let a ← (carriers e).run' {}
  return a.1 && a.2

-- The only presentation records in Examples are complete programs and function bodies.
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
  let some (.ctorInfo nameCtor) := env.find? `ConcreteSpec.Name.mk
    | throw (IO.userError "missing nominal declaration Name constructor")
  unless isStructure env `ConcreteSpec.Name && nameCtor.numFields == 1 &&
      nameCtor.numParams == 0 && nameCtor.type.bindingDomain! == mkConst `String do
    throw (IO.userError "declaration Name must have exactly one String field")
  let some (.defnInfo numbers) := env.find? `ConcreteSpec.Numbers
    | throw (IO.userError "missing numeric list alias")
  unless numbers.value == mkApp (mkConst `List [Level.zero]) (mkConst `Nat) do
    throw (IO.userError "Numbers must be exactly List Nat")
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
      let structurePair ← if isStructure env n then
        forallTelescopeReducing ci.type fun params _ => do
          paired (mkAppN (mkConst n (ci.levelParams.map mkLevelParam)) params)
        else pure false
      if structurePair || (← paired ci.type) then seeds := seeds.push n
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
