import Lean

open Lean

-- Inspect type aliases and pairing constructors without conflating a semantic AST
-- (which may contain struct-field names) with a local spelling table.
-- A Var/String pairing is data carrying both, or a function directly connecting the two.
partial def typeResult (e : Expr) : Bool :=
  match e with
  | .forallE _ _ b _ => typeResult b
  | .sort u => u != Level.zero
  | _ => false

-- Instantiate a structure's fields at its actual type arguments. This catches a generic
-- `Record Var String` as well as a record declaring those field types directly.
def structureFields (env : Environment) (e : Expr) : List Expr := Id.run do
  let .const n _ := e.getAppFn | return []
  if !isStructure env n then return []
  let some (.inductInfo info) := env.find? n | return []
  let args := e.getAppArgs
  if args.size != info.numParams then return []
  let some (.ctorInfo ctor) := env.find? info.ctors.head! | return []
  let mut t := ctor.type
  for arg in args do
    if !t.isForall then return []
    t := t.bindingBody!.instantiate1 arg
  let mut result := #[]
  while t.isForall do
    result := result.push t.bindingDomain!
    t := t.bindingBody!
  -- A proposition's evidence is not a spelling record.
  let some ci := env.find? n | return []
  let mut kind := ci.type
  while kind.isForall do kind := kind.bindingBody!
  if kind == mkSort Level.zero then return []
  return result.toList

-- Only spelling carriers count: semantic records and predicates remain opaque. In particular,
-- a field-name/Atom pair is not a spelling, and an environment entry is not a String.
partial def atoms (env : Environment) (e : Expr) (seen : List Name := []) : Bool × Bool := Id.run do
  if e.isConstOf `ConcreteSpec.Var then return (true, false)
  if e.isConstOf `String then return (false, true)
  if let .const n _ := e.getAppFn then
    if !seen.contains n then
      if let some (.defnInfo d) := env.find? n then
        if typeResult d.type then return atoms env (d.value.beta e.getAppArgs) (n :: seen)
    -- A single-field nominal wrapper preserves the carrier (for example, Text : String).
    if !seen.contains n then
      if let [field] := structureFields env e then
        return atoms env field (n :: seen)
    let args := e.getAppArgs
    if (n == `List || n == `Option) && args.size == 1 then
      return atoms env args[0]! seen
    if n == `Prod && args.size == 2 then
      let a := atoms env args[0]! seen
      let b := atoms env args[1]! seen
      if (a.1 || a.2) && (b.1 || b.2) then return (a.1 || b.1, a.2 || b.2)
  return (false, false)

-- Flatten contiguous product payloads, preserving container boundaries. Thus Var × (String × Nat)
-- carries a spelling, while (an environment) × (field-name/Atom pairs) does not pair a local
-- with a spelling. Opaque semantic records remain opaque unless their own fields pair them.
partial def components (env : Environment) (e : Expr) (seen : List Name := []) : Bool × Bool := Id.run do
  if let .const n _ := e.getAppFn then
    if !seen.contains n then
      if let some (.defnInfo d) := env.find? n then
        if typeResult d.type then return components env (d.value.beta e.getAppArgs) (n :: seen)
    let args := e.getAppArgs
    if n == `Prod && args.size == 2 then
      let a := components env args[0]! seen
      let b := components env args[1]! seen
      return (a.1 || b.1, a.2 || b.2)
  return atoms env e seen

partial def codomain (e : Expr) : Expr :=
  match e with
  | .forallE _ _ b _ => codomain b
  | _ => e

partial def paired (env : Environment) (e : Expr) (seen : List Name := []) : Bool := Id.run do
  if let .const n _ := e.getAppFn then
    if !seen.contains n then
      if let some (.defnInfo d) := env.find? n then
        if typeResult d.type then
          if paired env (d.value.beta e.getAppArgs) (n :: seen) then return true
  if let .const n _ := e.getAppFn then
    if !seen.contains n then
      let mut v := false
      let mut s := false
      for field in structureFields env e do
        let a := components env field
        v := v || a.1
        s := s || a.2
        if paired env field (n :: seen) then return true
      if v && s then return true
  match e with
  | .forallE _ d b _ =>
    let a := atoms env d
    let z := components env (codomain b)
    return (a.1 && !a.2 && z.2 && !z.1) ||
      (a.2 && !a.1 && z.1 && !z.2) ||
      paired env d seen || paired env b seen
  | .app f x =>
    if e.getAppFn.isConstOf `Prod then
      let args := e.getAppArgs
      if args.size == 2 then
        let a := components env e
        if a.1 && a.2 then return true
    return paired env f seen || paired env x seen
  | _ => return false

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
  -- Structures can split a spelling association over several fields. Seed their names too;
  -- reverse reachability then catches aliases, nested structures and consumers transitively.
  for (n, _) in env.constants.toList do
    if !isStructure env n then continue
    let some ci := env.find? n | continue
    let mut result := ci.type
    while result.isForall do result := result.bindingBody!
    if result == mkSort Level.zero then continue
    if let some (.inductInfo d) := env.find? n then
      let mut v := false
      let mut s := false
      let mut hasPair := false
      for c in d.ctors do
        if let some ci := env.find? c then
          let mut t := ci.type
          while t.isForall do
            hasPair := hasPair || paired env t.bindingDomain!
            let a := components env t.bindingDomain!
            v := v || a.1
            s := s || a.2
            t := t.bindingBody!
      if (v && s) || hasPair then queue := queue.push n
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
    if tainted.contains n || paired env ci.type then
      IO.eprintln s!"semantic diagnostics dependency: {n}"
      bad := true
  if bad then throw (IO.userError "semantic definitions refer to LocalNames/Elaborated")
  IO.println "numeric Var and semantic diagnostics separation: pass"
