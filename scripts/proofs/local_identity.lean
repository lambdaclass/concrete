import Lean

open Lean Meta

-- Normalize aliases and dependent field families in their instantiated contexts.
def normalType (e : Expr) : MetaM Expr := do
  withTransparency .all <| whnf (← instantiateMVars e)

def combine (a b : Bool × Bool) : Bool × Bool := (a.1 || b.1, a.2 || b.2)

-- Declaration names are nominal text, not local spellings. All other inductive
-- constructors are inspected, including ASTs, containers, structures and indices.
-- Ancestors are part of the key: a recursive field inspected while its
-- constructor is open must not cache an incomplete answer for another context.
abbrev CarrierCache := Std.HashMap (Expr × List Name) (Bool × Bool)

mutual
partial def carriers (e : Expr) (seen : List Name := []) : StateRefT CarrierCache MetaM (Bool × Bool) := do
  if let some found := (← get)[(e, seen)]? then return found
  let found ← carrierCore e seen
  modify (·.insert (e, seen) found)
  return found

partial def carrierCore (e : Expr) (seen : List Name := []) : StateRefT CarrierCache MetaM (Bool × Bool) := do
  if e.isConstOf `ConcreteSpec.Numbers then return (false, false)
  -- These validated nominal boundaries intentionally hide their representation.
  if e.isConstOf `ConcreteSpec.Var then return (true, false)
  if e.isConstOf `ConcreteSpec.Name then return (false, false)
  -- These atomic spelling carriers have no type-expression children. Their
  -- implementation fields add no information to the spelling classification.
  if e.isConstOf `Char || e.isConstOf `String || e.isConstOf `ConcreteSpec.LocalNames then
    return (false, true)
  let textList := e.isAppOfArity `List 1 &&
    (e.getAppArgs[0]!.isConstOf `Nat || e.getAppArgs[0]!.isConstOf `UInt8)
  let mut found := (false, textList)
  -- Visit syntax before reduction: beta/zeta reduction and aliases can erase
  -- arguments. Open binders in context so higher-kinded and dependent parameters
  -- have their actual types, at arbitrary universe levels.
  match e with
  | .forallE _ domain body bi | .lam _ domain body bi =>
    found := combine found (← carriers domain seen)
    return ← withLocalDecl `parameter bi domain fun x => do
      return combine found (← carriers (body.instantiate1 x) seen)
  | .letE _ type value body _ =>
    found := combine found (← carriers type seen)
    found := combine found (← typeSubterms value seen)
    return ← withLetDecl `parameter type value fun x => do
      return combine found (← carriers (body.instantiate1 x) seen)
  | .mdata _ body => return combine found (← carriers body seen)
  | .proj _ _ value =>
    found := combine found (← typeSubterms value seen)
    found := combine found (← carriers (← inferType e) seen)
  | .fvar _ => return combine found (← carriers (← inferType e) seen)
  | .app .. =>
    let head := e.getAppFn
    -- A parameter (including a class projection) is just as capable of carrying
    -- Var as a known inductive constructor. Never discard its arguments.
    found := combine found (← carriers head seen)
    let mut signature ← inferType head
    for arg in e.getAppArgs do
      signature ← normalType signature
      -- Values contribute their declared domain and every embedded type
      -- subterm, without expanding the representation of the value itself.
      if ← isType arg then
        found := combine found (← carriers arg seen)
      else
        found := combine found (← typeSubterms arg seen)
      -- Preserve declared index annotations (especially Numbers) instead of
      -- inferring a cons expression's expanded List Nat type.
      found := combine found (← carriers signature.bindingDomain! seen)
      signature := signature.bindingBody!.instantiate1 arg
  | _ => pure ()
  -- Keep proposition indices as declared; normalization of membership can
  -- discard numeric data annotations. All original subterms were visited above.
  let reduced ← if (← isType e) && !(← isProp e) then normalType e else instantiateMVars e
  if reduced != e then return combine found (← carriers reduced seen)
  let .const n levels := e.getAppFn | return found
  if seen.contains n then return found
  let ancestors := n :: seen
  let env ← getEnv
  if let some (.inductInfo info) := env.find? n then
    if e.getAppNumArgs >= info.numParams then
      for ctor in info.ctors do
        let t ← inferType (mkAppN (mkConst ctor levels) (e.getAppArgs.extract 0 info.numParams))
        -- The constructor's whole type includes fields, binder domains and
        -- result indices; reducing its telescope first can erase subterms.
        let payload ← carriers t ancestors
        found := combine found payload
  return found

-- Opaque numeric/name data still cannot hide type arguments (for example a
-- numeric-valued function applied to Var). Walk its syntax without interpreting
-- its string/list representation as a semantic spelling interface.
partial def typeSubterms (e : Expr) (seen : List Name) : StateRefT CarrierCache MetaM (Bool × Bool) := do
  if ← isType e then return ← carriers e seen
  match e with
  | .app f a => return combine (← typeSubterms f seen) (← typeSubterms a seen)
  | .forallE _ domain body bi | .lam _ domain body bi =>
    let found ← carriers domain seen
    withLocalDecl `parameter bi domain fun x => do
      return combine found (← typeSubterms (body.instantiate1 x) seen)
  | .letE _ type value body _ =>
    let found := combine (← carriers type seen) (← typeSubterms value seen)
    withLetDecl `parameter type value fun x => do
      return combine found (← typeSubterms (body.instantiate1 x) seen)
  | .mdata _ body | .proj _ _ body => typeSubterms body seen
  | _ => return (false, false)
end

-- Every semantic telescope, parameter, index and nested payload obeys the same
-- rule: locals may accompany declaration names, but never local spellings.
def paired (e : Expr) : StateRefT CarrierCache MetaM Bool := do
  let a ← carriers e
  return a.1 && a.2

-- Body-local pairings matter even when the exported result erases the pair.
-- Check each embedded type independently: unrelated numeric literals in a body
-- must not combine with a Var binder to invent a spelling interface.
partial def bodyPaired (e : Expr) : StateRefT CarrierCache MetaM Bool := do
  if ← isType e then return ← paired e
  match e with
  | .app .. =>
    -- Type arguments alone may be split across a generic constructor application.
    -- Its instantiated result exposes the actual pairing. Do not inspect partial
    -- application spines as independent interfaces.
    if ← paired (← inferType e) then return true
    for arg in e.getAppArgs do
      if ← bodyPaired arg then return true
    return ← bodyPaired e.getAppFn
  | .forallE _ domain body bi | .lam _ domain body bi =>
    if ← paired domain then return true
    withLocalDecl `parameter bi domain fun x => bodyPaired (body.instantiate1 x)
  | .letE _ type value body _ =>
    if (← paired type) || (← bodyPaired value) then return true
    withLetDecl `parameter type value fun x => bodyPaired (body.instantiate1 x)
  | .mdata _ body | .proj _ _ body => bodyPaired body
  | _ => return false

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
  -- Inspect the complete dependency closure of semantic declarations. Module
  -- ownership determines the roots to protect, never which helpers can seed taint.
  let mut reachable : Std.HashSet Name := {}
  let mut pending := #[]
  for (n, _) in env.constants.toList do
    let some i := env.getModuleIdxFor? n | continue
    let m := env.header.moduleNames[i.toNat]!.toString
    if m == "ConcreteSpec.Spec" || m.startsWith "ConcreteSpec.Spec." then
      pending := pending.push n
  let mut next := 0
  while next < pending.size do
    let n := pending[next]!
    next := next + 1
    if reachable.contains n then continue
    reachable := reachable.insert n
    if let some ci := env.find? n then
      for dependency in ci.getUsedConstantsAsSet do
        pending := pending.push dependency
  let mut tainted : Std.HashSet Name := {}
  let mut queue := #[`ConcreteSpec.LocalNames, `ConcreteSpec.Elaborated]
  -- Inspect every declaration's instantiated telescope, including inductive indices
  -- and Prop-valued judgments. The same analysis handles fields and function values.
  let inspect : StateRefT CarrierCache MetaM (Array Name) := do
    let mut seeds := #[]
    for (n, ci) in env.constants.toList do
      if !reachable.contains n then continue
      -- Lean-generated eliminators expose constructor fields as continuation arguments.
      -- They are not user semantic interfaces; dependencies still propagate through them.
      if isMatcherCore env n || isAuxRecursor env n || (ci matches .recInfo _) then continue
      -- Bound each declaration, rather than charging the entire imported
      -- environment against one declaration's default heartbeat allowance.
      let isPair ← Core.withCurrHeartbeats do
        let structurePair ← if isStructure env n then
          forallTelescopeReducing ci.type fun params _ => do
            paired (mkAppN (mkConst n (ci.levelParams.map mkLevelParam)) params)
          else pure false
        let bodyPair ← match ci.value? with
          | some value => bodyPaired value
          | none => pure false
        return structurePair || (← paired ci.type) || bodyPair
      if isPair then seeds := seeds.push n
    return seeds
  let (seeds, _) ← ((inspect.run' {}).run' {}).toIO { fileName := "local_identity", fileMap := default } { env := env }
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
