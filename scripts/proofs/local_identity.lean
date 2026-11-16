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
abbrev CarrierCache := Std.HashMap ((Nat × Expr) × List Name) (Bool × Bool)

-- Keys close over their local context. Only fully closed, metavariable-free
-- carrier results cross declaration boundaries; recursion ancestors remain part
-- of every carrier key.
structure ScanCache where
  carrier : CarrierCache := {}
  closedCarrier : CarrierCache := {}
  semantic : Std.HashMap (Nat × Expr) Bool := {}

abbrev ScanM := StateRefT ScanCache MetaM

-- A cache key closes over exactly the free-variable dependency context, including
-- local let values. Alpha-equivalent openings therefore share results without
-- conflating variables whose types or local definitions differ. The prefix count
-- separates the captured context from binders already in the original expression;
-- without it, an open body could alias its closed lambda or let expression.
-- No reduction occurs while making this key: Numbers/Name annotations survive.
def contextualKey (e : Expr) : MetaM (Nat × Expr) := do
  if !e.hasFVar then return (0, e)
  let mut vars := collectFVars {} e
  let mut next := 0
  let mut visited : FVarIdSet := {}
  while next < vars.fvarIds.size do
    let id := vars.fvarIds[next]!
    next := next + 1
    if visited.contains id then continue
    visited := visited.insert id
    let decl ← id.getDecl
    vars := collectFVars vars decl.type
    if let some value := decl.value? then vars := collectFVars vars value
  let xs := (← getLCtx).foldl (fun xs decl =>
    if vars.fvarSet.contains decl.fvarId then xs.push decl.toExpr else xs) #[]
  let closed ← mkLambdaFVars xs e (usedLetOnly := false) (generalizeNondepLet := false)
  return (xs.size, closed)

-- Exercise the production key, including aliases whose unreduced annotations
-- have different meanings to the carrier scanner. These controls never alter
-- the scanner's declaration population, admission rules or resource limits.
def contextualKeyControls : MetaM Unit := do
  let nat := mkConst ``Nat
  let first ← withLocalDeclD `x nat contextualKey
  let renamed ← withLocalDeclD `another nat contextualKey
  unless first == renamed do throwError "cache: alpha-equivalent contexts differ"
  let unrelated ← withLocalDeclD `unused (mkConst ``String) fun _ =>
    withLocalDeclD `another nat contextualKey
  unless first == unrelated do throwError "cache: unused context affects key"
  let otherType ← withLocalDeclD `x (mkConst ``String) contextualKey
  unless first != otherType do throwError "cache: local type omitted"
  let one ← withLetDecl `x nat (mkNatLit 1) contextualKey
  let two ← withLetDecl `x nat (mkNatLit 2) contextualKey
  unless one != two do throwError "cache: local let value omitted"
  let closed ← contextualKey (.lam `x nat (.bvar 0) .default)
  unless first.2 == closed.2 && first != closed do
    throwError "cache: original expression/context boundary omitted"
  let list ← withLocalDeclD `T (mkSort (.succ .zero)) fun t =>
    withLocalDeclD `x (mkApp (mkConst ``List [.zero]) t) contextualKey
  let sameList ← withLocalDeclD `U (mkSort (.succ .zero)) fun t =>
    withLocalDeclD `ys (mkApp (mkConst ``List [.zero]) t) contextualKey
  unless list == sameList do throwError "cache: dependent context not closed"
  let option ← withLocalDeclD `T (mkSort (.succ .zero)) fun t =>
    withLocalDeclD `x (mkApp (mkConst ``Option [.zero]) t) contextualKey
  unless list != option do throwError "cache: dependent domain omitted"
  let chain ← withLetDecl `x nat (mkNatLit 1) fun x =>
    withLetDecl `y nat x contextualKey
  let sameChain ← withLetDecl `a nat (mkNatLit 1) fun a =>
    withLetDecl `b nat a contextualKey
  unless chain == sameChain do throwError "cache: let dependencies not closed"
  let numbers ← withLocalDeclD `x (mkConst `ConcreteSpec.Numbers) contextualKey
  let scalars ← withLocalDeclD `x (mkApp (mkConst ``List [.zero]) nat) contextualKey
  unless numbers != scalars do throwError "cache: numeric annotation erased"

mutual
partial def carriers (e : Expr) (seen : List Name := []) : ScanM (Bool × Bool) := do
  -- Closed expressions cannot depend on a declaration's local context. Keep
  -- those reusable results, but never retain a local or a loose bound variable.
  let key ← contextualKey e
  let closed := !key.2.hasFVar && !key.2.hasMVar && !key.2.hasLooseBVars
  let cache ← get
  if let some found := (if closed then cache.closedCarrier else cache.carrier)[(key, seen)]? then
    return found
  let found ← carrierCore e seen
  modify fun s => if closed then
    { s with closedCarrier := s.closedCarrier.insert (key, seen) found }
    else { s with carrier := s.carrier.insert (key, seen) found }
  return found

partial def carrierCore (e : Expr) (seen : List Name := []) : ScanM (Bool × Bool) := do
  if e.isConstOf `ConcreteSpec.Numbers || e.isConstOf `ConcreteSpec.Core.ScalarChar then
    return (false, false)
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
partial def typeSubterms (e : Expr) (seen : List Name) : ScanM (Bool × Bool) := do
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
def paired (e : Expr) : ScanM Bool := do
  let a ← carriers e
  return a.1 && a.2

-- Semantic rule: every subterm of every reachable declaration's elaborated
-- type and value is checked using its complete inferred type. Type expressions
-- are also checked as carriers themselves (their inferred type is merely Sort).
-- Bindings are instantiated with fresh locals before descending, so inferType
-- sees the actual dependent context. Applications include their function spine
-- and instance/type arguments; projections include their receiver. No reduction
-- precedes this walk, and no matcher, auxiliary or recursor is exempt.
-- Soundness relative to carriers: any variable/spelling interface in the
-- elaborated dependency closure occurs as a type expression or the type of a
-- subterm. Exhaustive Expr descent checks that occurrence in its local context;
-- closure traversal checks named helpers by the identical rule, and reverse
-- reachability attributes their refusal to every semantic caller. Thus erasing
-- an intermediate result or moving it into a callback/helper cannot hide it.
mutual
partial def semanticPaired (e : Expr) : ScanM Bool := do
  let key ← contextualKey e
  if let some found := (← get).semantic[key]? then return found
  let found ← semanticCore e
  modify fun s => { s with semantic := s.semantic.insert key found }
  return found

partial def semanticCore (e : Expr) : ScanM Bool := do
  if ← paired (← inferType e) then return true
  if (← isType e) && (← paired e) then return true
  match e with
  | .app f a =>
    return (← semanticPaired f) || (← semanticPaired a)
  | .forallE name domain body bi | .lam name domain body bi =>
    if ← semanticPaired domain then return true
    withLocalDecl name bi domain fun x => semanticPaired (body.instantiate1 x)
  | .letE name type value body _ =>
    if (← semanticPaired type) || (← semanticPaired value) then return true
    withLetDecl name type value fun x => semanticPaired (body.instantiate1 x)
  | .mdata _ body | .proj _ _ body => semanticPaired body
  | _ => return false

end

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

-- A batch request protects exactly the semantic modules a standalone import
-- would expose. Other fixtures in the shared environment are not its roots.
def importClosure (env : Environment) (target : Name) : Std.HashSet String := Id.run do
  let mut modules : Std.HashSet String := {}
  let mut pending := #[target]
  let mut next := 0
  while next < pending.size do
    let name := pending[next]!
    next := next + 1
    if modules.contains name.toString then continue
    modules := modules.insert name.toString
    if let some idx := env.getModuleIdx? name then
      for dependency in env.header.moduleData[idx.toNat]!.imports do
        pending := pending.push dependency.module
  return modules

def main (args : List String) : IO Unit := do
  let batch := args.head? == some "--batch"
  let targets := if batch then args.drop 1 else args
  unless !targets.isEmpty && (batch || targets.length == 1) do
    throw (IO.userError "expected one import module or --batch modules...")
  unsafe enableInitializersExecution
  initSearchPath (← findSysroot)
  let profile := (← IO.getEnv "LOCAL_IDENTITY_PROFILE") == some "1"
  let started ← IO.monoMsNow
  let env ← importModules (targets.toArray.map fun target => {module := target.toName}) {} (loadExts := true)
  if profile then IO.eprintln s!"profile: import_ms={(← IO.monoMsNow) - started}"
  let graphStart ← IO.monoMsNow
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
  -- Scalar program values may accompany locals; raw diagnostic Char remains a spelling
  -- carrier. Validate the annotation's exact representation before trusting that boundary.
  if let some info := env.find? `ConcreteSpec.Core.ScalarChar then
    unless (match info with
      | .defnInfo d => d.value == mkConst `Char
      | _ => false) do
      throw (IO.userError "ScalarChar must be exactly Char")
  let _ ← contextualKeyControls.run' {} |>.toIO
    { fileName := "local_identity_key_controls", fileMap := default } { env := env }
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
  -- Only reachable callers can affect a protected root. Build their reverse
  -- edges once, after closure discovery; unrelated imported declarations cannot
  -- carry taint back into this closed dependency set.
  let mut users : Std.HashMap Name (Array Name) := {}
  for n in reachable.toArray do
    if let some ci := env.find? n then
      for dependency in ci.getUsedConstantsAsSet do
        users := users.insert dependency ((users[dependency]?.getD #[]).push n)
  let mut tainted : Std.HashSet Name := {}
  let mut queue := #[`ConcreteSpec.LocalNames, `ConcreteSpec.Elaborated]
  -- Inspect every declaration's instantiated telescope, including inductive indices
  -- and Prop-valued judgments. The same analysis handles fields and function values.
  if profile then IO.eprintln s!"profile: graph_ms={(← IO.monoMsNow) - graphStart} reachable={reachable.size}"
  let scanStart ← IO.monoMsNow
  let mut seeds := #[]
  let mut closedCarrier : CarrierCache := {}
  for (n, ci) in env.constants.toList do
    if !reachable.contains n then continue
    -- Local carrier, semantic and Meta inference caches belong to one declaration.
    -- Only closed carrier expressions cross this boundary: they cannot contain
    -- local contexts, and the imported environment remains immutable.
    let inspect : ScanM Bool := Core.withCurrHeartbeats do
      let structurePair ← if isStructure env n then
        forallTelescopeReducing ci.type fun params _ => do
          paired (mkAppN (mkConst n (ci.levelParams.map mkLevelParam)) params)
        else pure false
      let bodyPair ← match ci.value? with
        | some value => semanticPaired value
        | none => pure false
      return structurePair || (← semanticPaired ci.type) || bodyPair
    let ((isPair, cache), _) ← ((inspect.run { closedCarrier := closedCarrier }).run' {}).toIO
      { fileName := "local_identity", fileMap := default } { env := env }
    closedCarrier := cache.closedCarrier
    if isPair then seeds := seeds.push n
  if profile then IO.eprintln s!"profile: traversal_ms={(← IO.monoMsNow) - scanStart} closed_carriers={closedCarrier.size}"
  queue := queue ++ seeds
  let mut pos := 0
  while pos < queue.size do
    let n := queue[pos]!
    pos := pos + 1
    if tainted.contains n then continue
    tainted := tainted.insert n
    queue := queue ++ (users[n]?.getD #[])
  let mut bad := false
  let mut refusals : Std.HashMap String (Array Json) := {}
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
      if !batch then IO.eprintln s!"semantic diagnostics dependency: {n}"
      refusals := refusals.insert m ((refusals[m]?.getD #[]).push (toJson n.toString))
      bad := true
  if profile then IO.eprintln s!"profile: total_ms={(← IO.monoMsNow) - started}"
  if batch then
    for target in targets do
      let modules := importClosure env target.toName
      let mut names := #[]
      for (module, refused) in refusals.toList do
        if modules.contains module then names := names ++ refused
      IO.println <| Json.compress <| Json.mkObj
        [("module", toJson target), ("refusals", toJson names)]
    return
  if bad then throw (IO.userError "semantic definitions refer to LocalNames/Elaborated")
  IO.println "numeric Var and semantic diagnostics separation: pass"
