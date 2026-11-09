#!/usr/bin/env python3
"""
N2 Phase B, Slice 2a — Round 5 Reproducible Bounded Prototype & Audit Harness.
Executes:
  1. All 16 vacuity_certificates.py controls under the unchanged 20,000 work budget
     with complete 10-category walk accounting.
  2. All 7 vacuity_n8a_repro.py observations + vacuity_solver_boundary.py singletonCertificate.
  3. All per-row positive/negative controls (Tables 4.1a & 4.1b) and same-state retry controls,
     elaborated with the pinned Lean 4.34.1 compiler.
  4. All 22 single-row transfer mutations across Tables 4.1a & 4.1b, demonstrating exact
     mutation sensitivity on each row.
"""
import hashlib
import os
import sys
import tempfile
from pathlib import Path

HERE = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(HERE))
from vacuity_driver import command, compiled_consumer
import vacuity_certificates as certs
import vacuity_n8a_repro as n8a
import vacuity_solver_boundary as boundary

R5_DISCHARGE = r"""
structure Template where
  key : TemplateKey
  root : TermId
  bodyRoot : TermId := 0
  bodyArity : Nat := 0
  deadMask : Nat := 0
  declaredType : TermId := 0
  forallArity : Nat := 0
  forallDomains : Array TermId := #[]
  forallCodomain : TermId := 0
  domainKeepMask : Nat := 0
  typeKeepMask : Nat := 0
  hasLetInForall : Bool := false
  bvarCodomain : Bool := false
  numParams : Nat := 0
  rules : Array (Name × Nat × TermId) := #[]
  transfers : Array Transfer := #[]
  deriving Inhabited

inductive Value where
  | closure (node : TermId) (bvars : Array Input) (hypothetical : Array Input)
  | partialCall (key : TemplateKey) (args : Array Input) (hypothetical : Array Input)
  | ctor (name : Name) (numParams : Nat) (args : Array Input)
  deriving BEq, Hashable, Inhabited

structure Query where
  template : TemplateKey
  inputs : Array Input
  entryInputs : Array Input := #[]
  guards : Array Guard
  hypothetical : Array Input
  mode : Mode
  deriving BEq, Hashable, Inhabited

structure TypeQuery where
  node : TermId
  bvars : Array Input
  entryInputs : Array Input := #[]
  guards : Array Guard
  hypothetical : Array Input
  isDomainPrefix : Bool := false
  deriving BEq, Hashable, Inhabited

inductive ValueKind where
  | closure (node : TermId)
  | partialCall (key : TemplateKey)
  | ctor (name : Name) (numParams : Nat)
  | minor (arity : Nat)
  deriving BEq, Hashable, Inhabited

structure ValueQuery where
  kind : ValueKind
  term : TermId
  origin : OriginId := 0
  inputs : Array Input
  entryInputs : Array Input := #[]
  guards : Array Guard
  hypothetical : Array Input
  mode : Mode
  deriving BEq, Hashable, Inhabited

structure NodeQuery where
  node : TermId
  bvars : Array Input
  args : Array Input
  entryInputs : Array Input := #[]
  guards : Array Guard
  hypothetical : Array Input
  mode : Mode
  deriving BEq, Hashable, Inhabited

inductive CacheKey where
  | query (q : Query)
  | typeQuery (tq : TypeQuery)
  | valueQuery (vq : ValueQuery)
  | nodeQuery (nq : NodeQuery)
  deriving BEq, Hashable, Inhabited

abbrev DerivationId := Nat

structure CompletedNode where
  key : CacheKey
  output : Input
  localTransfers : Array Input := #[]
  localExclusions : Array Input := #[]
  children : Array DerivationId := #[]
  firstTransfer : Option Input := none
  exclusions : Array Input := #[]
  deriving Inhabited

structure Graph where
  terms : Array Expr := #[]
  termIndex : Std.HashMap Expr TermId := {}
  origins : Array Origin := #[]
  originIndex : Std.HashMap Origin OriginId := {}
  suppliedOrigins : Std.HashSet OriginId := {}
  hypotheticalOrigins : Std.HashSet OriginId := {}
  canonicalHypOrigin : OriginId := 0
  nodes : Std.HashMap TermId Transfer := {}
  values : Std.HashMap TermId Value := {}
  templates : Std.HashMap TemplateKey Template := {}
  termFVars : Std.HashMap TermId (Array FVarId) := {}
  termFVarMask : Std.HashMap TermId Nat := {}
  fvarBits : Std.HashMap FVarId Nat := {}
  termLiveBVars : Std.HashMap TermId (Array Nat) := {}
  deriving Inhabited

inductive Work where
  | term (id : TermId)
  | template (key : TemplateKey)
  | query (query : Query)
  | certificate (entry : Nat)
  deriving Inhabited

structure CertificateEntry where
  instanceName : Name
  law : Name
  caseName : String
  query : Query
  target : TermId
  deriving Inhabited

structure CertificateSupplier where
  declaration : Name
  input : Input
  deriving Inhabited

structure CertificateDerivation where
  entry : CertificateEntry
  candidate : Certificate
  environment : Environment
  transfers : Array Input
  structuralExclusions : Array Input
  rootNodes : Array DerivationId := #[]
  rank : Nat := 0

inductive CertificateResult where
  | admitted (derivation : CertificateDerivation)
  | rejectedSuppliedContradiction (supplier : CertificateSupplier)
  | incomplete (finding : Finding)

structure SolverState where
  graph : Graph := {}
  remaining : Nat := 20000
  worklist : Array Work := #[]
  worklistSteps : Nat := 0
  queryCount : Nat := 0
  componentCount : Nat := 0
  certificates : Nat := 0
  pendingQueries : Std.HashSet Query := {}
  completedQueries : Std.HashMap Query DerivationId := {}
  pendingTypes : Std.HashSet TypeQuery := {}
  completedTypes : Std.HashMap TypeQuery DerivationId := {}
  pendingValues : Std.HashSet ValueQuery := {}
  completedValues : Std.HashMap ValueQuery DerivationId := {}
  pendingNodes : Std.HashSet NodeQuery := {}
  completedNodes : Std.HashMap NodeQuery DerivationId := {}
  projBvarsCache : Std.HashMap (TermId × Array Input) (Array Input × Nat) := {}
  projNodeCtxCache : Std.HashMap (TermId × Array Input × Array Input) (Array Input × Array Input) := {}
  projHypCache : Std.HashMap (TermId × Array Input × Array Input) (Array Input) := {}
  projInputsHypCache : Std.HashMap (Array Input × Array Input) (Array Input) := {}
  projMaskHypCache : Std.HashMap (Nat × Array Input) (Array Input) := {}
  projTypeArgsCache : Std.HashMap (TermId × Array Input) (Array Input × Nat) := {}
  derivationNodes : Array CompletedNode := #[]
  verifiedDerivations : Std.HashSet DerivationId := {}
  runStartNodeId : Nat := 0
  activeChildren : Array DerivationId := #[]
  activeChildrenSet : Std.HashSet DerivationId := {}
  activeFirstTransfer : Option Input := none
  activeExclusions : Array Input := #[]
  transfers : Array Input := #[]
  structuralExclusions : Array Input := #[]
  cConstruct : Nat := 0
  cEntry : Nat := 0
  cCall : Nat := 0
  cTypeInst : Nat := 0
  cNode : Nat := 0
  cComplete : Nat := 0
  cReplay : Nat := 0
  cVerify : Nat := 0
  cCollect : Nat := 0
  cProj : Nat := 0
  cProjBvars : Nat := 0
  cProjHyp : Nat := 0
  cProjInputsHyp : Nat := 0
  cProjTypeArgs : Nat := 0
  mutatedRow : String := ""
  deriving Inhabited

abbrev Solver := StateRefT SolverState MetaM

def charge : Solver Unit := do
  let s ← get
  if s.remaining == 0 then throwError "false-discharge-budget"
  set { s with remaining := s.remaining - 1, worklistSteps := s.worklistSteps + 1 }

def chargeN (n : Nat) : Solver Unit := do
  if n == 0 then return
  let s ← get
  if s.remaining < n then
    set { s with worklistSteps := s.worklistSteps + s.remaining, remaining := 0 }
    throwError "false-discharge-budget"
  set { s with remaining := s.remaining - n, worklistSteps := s.worklistSteps + n }

def chargeConstruct (n : Nat := 1) : Solver Unit := do
  chargeN n
  modify fun s => { s with cConstruct := s.cConstruct + n }

def chargeEntry (n : Nat := 1) : Solver Unit := do
  chargeN n
  modify fun s => { s with cEntry := s.cEntry + n }

def chargeCall (n : Nat := 1) : Solver Unit := do
  chargeN n
  modify fun s => { s with cCall := s.cCall + n }

def chargeTypeInst (n : Nat := 1) : Solver Unit := do
  chargeN n
  modify fun s => { s with cTypeInst := s.cTypeInst + n }

def chargeNode (n : Nat := 1) : Solver Unit := do
  chargeN n
  modify fun s => { s with cNode := s.cNode + n }

def chargeComplete (n : Nat := 1) : Solver Unit := do
  chargeN n
  modify fun s => { s with cComplete := s.cComplete + n }

def chargeReplay (n : Nat := 1) : Solver Unit := do
  chargeN n
  modify fun s => { s with cReplay := s.cReplay + n }

def chargeVerify (n : Nat := 1) : Solver Unit := do
  chargeN n
  modify fun s => { s with cVerify := s.cVerify + n }

def chargeCollect (n : Nat := 1) : Solver Unit := do
  chargeN n
  modify fun s => { s with cCollect := s.cCollect + n }

def chargeProjBvars (n : Nat := 1) : Solver Unit := do
  chargeN n
  modify fun s => { s with cProj := s.cProj + n, cProjBvars := s.cProjBvars + n }

def chargeProjHyp (n : Nat := 1) : Solver Unit := do
  chargeN n
  modify fun s => { s with cProj := s.cProj + n, cProjHyp := s.cProjHyp + n }

def chargeProjInputsHyp (n : Nat := 1) : Solver Unit := do
  chargeN n
  modify fun s => { s with cProj := s.cProj + n, cProjInputsHyp := s.cProjInputsHyp + n }

def chargeProjTypeArgs (n : Nat := 1) : Solver Unit := do
  chargeN n
  modify fun s => { s with cProj := s.cProj + n, cProjTypeArgs := s.cProjTypeArgs + n }

def dummyInput : Input := { term := 0, type := 0, origin := 0 }

def isDummyInput (inp : Input) : Bool :=
  inp.term == 0 && inp.type == 0 && inp.origin == 0

partial def cleanSpine (e : Expr) : Expr :=
  match e with
  | .mdata _ b => cleanSpine b
  | .app f a => .app (cleanSpine f) (cleanSpine a)
  | _ => e

def internTerm (e : Expr) : StateM Graph TermId := do
  let g ← get
  if let some id := g.termIndex[e]? then
    return id
  let id := g.terms.size
  set { g with terms := g.terms.push e, termIndex := g.termIndex.insert e id }
  return id

def internOrigin (o : Origin) : StateM Graph OriginId := do
  let g ← get
  if let some id := g.originIndex[o]? then
    return id
  let id := g.origins.size
  let isSup := match o with
    | .supplied .. => true
    | .field p _ => g.suppliedOrigins.contains p
    | .substitution _ ins => ins.any (fun i => g.suppliedOrigins.contains i)
    | .hypothetical .. => false
  let isHyp := match o with
    | .hypothetical .. => true
    | .field p _ => g.hypotheticalOrigins.contains p
    | .substitution _ ins => ins.any (fun i => g.hypotheticalOrigins.contains i)
    | .supplied .. => false
  let supSet := if isSup then g.suppliedOrigins.insert id else g.suppliedOrigins
  let hypSet := if isHyp then g.hypotheticalOrigins.insert id else g.hypotheticalOrigins
  let canonHyp := if g.canonicalHypOrigin == 0 && (match o with | .hypothetical .. => true | _ => false) then id else g.canonicalHypOrigin
  set {
    g with
      origins := g.origins.push o,
      originIndex := g.originIndex.insert o id,
      suppliedOrigins := supSet,
      hypotheticalOrigins := hypSet,
      canonicalHypOrigin := canonHyp
  }
  return id

def getFVarBit (f : FVarId) : Solver Nat := do
  let g := (← get).graph
  if let some bit := g.fvarBits[f]? then return bit
  let bit := 1 <<< g.fvarBits.size
  modify fun s => { s with graph.fvarBits := s.graph.fvarBits.insert f bit }
  return bit

def getTermFVarMask (id : TermId) : Solver Nat := do
  let g := (← get).graph
  if let some m := g.termFVarMask[id]? then return m
  let e := g.terms[id]!
  if !e.hasFVar then
    modify fun s => { s with graph.termFVarMask := s.graph.termFVarMask.insert id 0 }
    return 0
  let fvs := if let some fs := g.termFVars[id]? then fs else (collectFVars {} e).fvarIds
  if !g.termFVars.contains id then
    chargeConstruct fvs.size
  let mut mask := 0
  for f in fvs do
    let bit ← getFVarBit f
    mask := mask ||| bit
  modify fun s => {
    s with
      graph.termFVars := s.graph.termFVars.insert id fvs,
      graph.termFVarMask := s.graph.termFVarMask.insert id mask
  }
  return mask

def term (e : Expr) : Solver TermId := do
  let (id, graph) := internTerm e |>.run (← get).graph
  modify fun s => { s with graph }
  return id

def origin (o : Origin) : Solver OriginId := do
  let (id, graph) := internOrigin o |>.run (← get).graph
  modify fun s => { s with graph }
  return id

def enqueue (work : Work) : Solver Unit :=
  modify fun s => { s with worklist := s.worklist.push work }

def fvarsOfTerm (g : Graph) (id : TermId) : Array FVarId :=
  if let some fs := g.termFVars[id]? then fs
  else
    let e := g.terms[id]!
    if !e.hasFVar then #[]
    else (collectFVars {} e).fvarIds

partial def isDirectHypotheticalOrigin (g : Graph) (oid : OriginId) : Bool :=
  match g.origins[oid]? with
  | some (.hypothetical ..) => true
  | some (.field p _) => isDirectHypotheticalOrigin g p
  | _ => false

def hasSuppliedOrigin (g : Graph) (oid : OriginId) : Bool :=
  g.suppliedOrigins.contains oid

def hasHypotheticalOrigin (g : Graph) (oid : OriginId) : Bool :=
  g.hypotheticalOrigins.contains oid

def canonicalizeInput (inp : Input) : Solver Input := do
  if isDummyInput inp then return inp
  let g := (← get).graph
  if g.suppliedOrigins.contains inp.origin || isDirectHypotheticalOrigin g inp.origin then
    return inp
  if g.hypotheticalOrigins.contains inp.origin && !(fvarsOfTerm g inp.term).isEmpty && g.canonicalHypOrigin != 0 then
    let o ← origin (.substitution inp.term #[g.canonicalHypOrigin])
    return { inp with origin := o }
  let o ← origin (.substitution inp.term #[])
  return { inp with origin := o }

def pushHypothetical (xs : Array Input) (x : Input) : Array Input := Id.run do
  if xs.any (fun item => item.term == x.term) then return xs
  if xs.isEmpty || xs.back!.term < x.term then return xs.push x
  let mut out : Array Input := Array.mkEmpty (xs.size + 1)
  let mut inserted := false
  for item in xs do
    if !inserted && x.term < item.term then
      out := out.push x
      inserted := true
    out := out.push item
  if !inserted then out := out.push x
  return out

def mergeHypothetical (xs ys : Array Input) : Array Input := Id.run do
  if xs.isEmpty then return ys
  if ys.isEmpty then return xs
  let mut out := xs
  for y in ys do
    out := pushHypothetical out y
  return out

def freshLevelFor (liveH : Array Input) (g : Graph) (domTermId : TermId) : Nat := Id.run do
  let domFVars := fvarsOfTerm g domTermId
  let mut lvl := 0
  while true do
    let fId : FVarId := { name := ((`_n2_var).num lvl).num domTermId }
    let inDom := domFVars.contains fId
    let inHyp := liveH.any fun inp =>
      match g.terms[inp.term]! with
      | .fvar f => f == fId
      | _ => false
    if !inDom && !inHyp then
      return lvl
    lvl := lvl + 1
  return 0

def withHypotheticalLCtx (liveH : Array Input) (k : Solver α) : Solver α := do
  let g := (← get).graph
  let mut lctx ← getLCtx
  for inp in liveH do
    if let .fvar fId := g.terms[inp.term]! then
      let domExpr := g.terms[inp.type]!
      lctx := lctx.mkLocalDecl fId (`x) domExpr .default
  let insts ← getLocalInstances
  withLCtx lctx insts k

def withCanonicalBinder (domTermId : TermId) (orig : Option OriginId) (liveH : Array Input)
    (k : Input → Solver α) : Solver α := do
  let g := (← get).graph
  let lvl := freshLevelFor liveH g domTermId
  let fId : FVarId := { name := ((`_n2_var).num lvl).num domTermId }
  let xExpr := Expr.fvar fId
  let xTermId ← term xExpr
  let o ← match orig with
    | some existing => pure existing
    | none => origin (.hypothetical lvl domTermId)
  let xInput : Input := { term := xTermId, type := domTermId, origin := o }
  let domExpr := (← get).graph.terms[domTermId]!
  let mut lctx ← getLCtx
  for inp in liveH do
    if let .fvar f := (← get).graph.terms[inp.term]! then
      lctx := lctx.mkLocalDecl f (`x) (← get).graph.terms[inp.type]! .default
  lctx := lctx.mkLocalDecl fId (`x) domExpr .default
  let insts ← getLocalInstances
  withLCtx lctx insts (k xInput)

partial def stripMatchingBvars (declTy body : Expr) (n : Nat := 0) (deadMask : Nat := 0) : Nat × Nat × Expr :=
  match cleanSpine declTy, cleanSpine body with
  | .forallE _ dTy dBody _, .lam _ bTy bBody _ =>
    if (cleanSpine dTy).eqv (cleanSpine bTy) then
      let nextMask := if !(cleanSpine bBody).hasLooseBVar 0 then deadMask ||| (1 <<< n) else deadMask
      stripMatchingBvars dBody bBody (n + 1) nextMask
    else
      (n, deadMask, cleanSpine body)
  | _, _ => (n, deadMask, cleanSpine body)

partial def analyzeDeclType (ty : Expr) (idx : Nat := 0) (doms : Array Expr := #[])
    (domKeepMask : Nat := 0) (keepMask : Nat := 0) : Nat × Array Expr × Expr × Nat × Nat × Bool × Bool :=
  match cleanSpine ty with
  | .forallE _ d body _ =>
    let cd := cleanSpine d
    let cbody := cleanSpine body
    let nextDomMask := if cd.hasLooseBVars then Id.run do
      let mut m := domKeepMask
      for j in [:idx] do
        if cd.hasLooseBVar (idx - 1 - j) then
          m := m ||| (1 <<< j)
      m
    else domKeepMask
    let nextMask := if cbody.hasLooseBVar 0 then keepMask ||| (1 <<< idx) else keepMask
    analyzeDeclType cbody (idx + 1) (doms.push cd) nextDomMask nextMask
  | .letE .. =>
    (idx, doms, cleanSpine ty, domKeepMask, keepMask, true, false)
  | other =>
    (idx, doms, other, domKeepMask, keepMask, false, other.getAppFn'.isBVar)

def stripRuleBvars (n : Nat) (e : Expr) : Expr :=
  match n, e with
  | n + 1, .lam _ _ b _ => stripRuleBvars n b
  | n + 1, .mdata _ b => stripRuleBvars (n + 1) b
  | _, _ => e

def compileNode (id : TermId) : Solver Unit := do
  if (← get).graph.nodes.contains id then return
  let rawE := (← get).graph.terms[id]!
  let mutRow := (← get).mutatedRow
  let e := if mutRow == "mut-row6-mdata" && rawE.isMData then rawE else cleanSpine rawE
  let g0 := (← get).graph
  let fvs : Array FVarId := if e.hasFVar then
    if let some fs := g0.termFVars[id]? then fs else (collectFVars {} e).fvarIds
  else #[]
  if !fvs.isEmpty && !g0.termFVars.contains id then
    chargeConstruct fvs.size
    modify fun s => { s with graph.termFVars := s.graph.termFVars.insert id fvs }
  let liveIndices : Array Nat := if e.hasLooseBVars then Id.run do
    let mut arr : Array Nat := #[]
    for j in [:e.looseBVarRange] do
      if e.hasLooseBVar j then arr := arr.push j
    arr
  else #[]
  let mut children : Array Expr := #[]
  let transfer ← match e with
    | .app .. | .const .. => do
      let fn := e.getAppFn'
      let args := e.getAppArgs
      let inputs ← args.mapM term
      children := args
      match fn with
      | .const name levels =>
        let key := { name, levels, mode := .transfer : TemplateKey }
        if (← getMatcherInfo? name).isSome then
          pure (.branch key inputs)
        else
          match (← getEnv).find? name with
          | some (.ctorInfo _) => pure (.constructor name inputs)
          | some (.recInfo _) => pure (.branch key inputs)
          | _ => pure (.call key inputs)
      | _ =>
        children := children.push fn
        pure (.application (← term fn) inputs)
    | .lam _ type body _ => do
      children := #[type, body]
      pure (.binder (← term type) (← term body))
    | .forallE _ type body _ => do
      children := #[type, body]
      pure (.forallBinder (← term type) (← term body))
    | .letE _ type value body _ => do
      children := #[type, value, body]
      pure (.letBind (← term type) (← term value) (← term body))
    | .proj name field receiver => do
      children := #[receiver]
      pure (.projection name field (← term receiver))
    | .mdata _ body => do
      if mutRow == "mut-row6-mdata" then
        pure (.result id)
      else
        children := #[body]
        pure (.result (← term body))
    | .mvar _ =>
      if mutRow == "mut-row3-mvar" then
        pure (.result id)
      else
        pure (.unresolved id "uninstantiated-metavariable")
    | _ => pure (.result id)
  modify fun s => {
    s with
      graph.nodes := s.graph.nodes.insert id transfer,
      graph.termFVars := s.graph.termFVars.insert id fvs,
      graph.termLiveBVars := s.graph.termLiveBVars.insert id liveIndices
  }
  for child in children do
    let childId ← term child
    unless (← get).graph.nodes.contains childId do enqueue (.term childId)

def compileTemplate (key : TemplateKey) : Solver Unit := do
  if (← get).graph.templates.contains key then return
  let ci ← getConstInfo key.name
  let instType := ci.type.instantiateLevelParams ci.levelParams key.levels
  let declaredType ← term instType
  let body := (ci.value? (allowOpaque := true)).getD (mkConst key.name key.levels)
  let instBody := body.instantiateLevelParams ci.levelParams key.levels
  let root ← term instBody
  let (bodyArity, deadMask, strippedBody) := if (ci.value? (allowOpaque := true)).isSome then stripMatchingBvars instType instBody else (0, 0, instBody)
  let (forallArity, rawDoms, rawCod, domainKeepMask, typeKeepMask, hasLetInForall, bvarCodomain) := analyzeDeclType instType
  let forallDomains ← rawDoms.mapM term
  let forallCodomain ← term rawCod
  let bodyRoot ← term strippedBody
  let numParams := match ci with
    | .ctorInfo c => c.numParams + c.numFields
    | .recInfo r => r.getMajorIdx + 1
    | _ => bodyArity
  let rules ← match ci with
    | .recInfo r =>
      r.rules.toArray.mapM fun rule => do
        let instRhs := rule.rhs.instantiateLevelParams ci.levelParams key.levels
        let stripped := stripRuleBvars (r.numParams + r.numMotives + r.numMinors + rule.nfields) instRhs
        let rhsId ← term stripped
        enqueue (.term rhsId)
        pure (rule.ctor, rule.nfields, rhsId)
    | _ => pure #[]
  enqueue (.term declaredType)
  for dId in forallDomains do enqueue (.term dId)
  enqueue (.term forallCodomain)
  enqueue (.term root)
  enqueue (.term bodyRoot)
  let tmpl : Template := {
    key, root, bodyRoot, bodyArity, deadMask, declaredType,
    forallArity, forallDomains, forallCodomain, domainKeepMask,
    typeKeepMask, hasLetInForall, bvarCodomain, numParams, rules
  }
  modify fun s => { s with graph.templates := s.graph.templates.insert key tmpl }

def drainConstructionWork : Solver Unit := do
  while !(← get).worklist.isEmpty do
    let some work := (← get).worklist.back? | break
    match work with
    | .term id =>
      chargeConstruct 1
      modify fun s => { s with worklist := s.worklist.pop }
      compileNode id
    | .template key =>
      chargeConstruct 1
      modify fun s => { s with worklist := s.worklist.pop }
      compileTemplate key
    | _ => break

def ensureNode (id : TermId) : Solver Transfer := do
  if let some tr := (← get).graph.nodes[id]? then
    return tr
  enqueue (.term id)
  drainConstructionWork
  return (← get).graph.nodes[id]!

def ensureTemplate (key : TemplateKey) : Solver Template := do
  if let some tmpl := (← get).graph.templates[key]? then
    return tmpl
  enqueue (.template key)
  drainConstructionWork
  return (← get).graph.templates[key]!

def mkInput (e : Expr) (orig : OriginId) : Solver Input := do
  let e := if (← get).mutatedRow == "mut-row6-mdata" && e.isMData then e else cleanSpine e
  let id ← term e
  let ty ← if e.hasLooseBVars || e.isMVar then pure (mkSort .zero) else inferType e
  let tyId ← term (cleanSpine ty)
  return ← canonicalizeInput { term := id, type := tyId, origin := orig }

def recordValue (id : TermId) (v : Value) : Solver Unit :=
  modify fun s => { s with graph.values := s.graph.values.insert id v }

def liveBvarsForM (id : TermId) (bvars : Array Input) : Solver (Array Input × Nat) := do
  if bvars.isEmpty then return (#[], 0)
  let s ← get
  let g := s.graph
  let raw := g.terms[id]!
  if !raw.hasLooseBVars then return (#[], 0)
  let r := Nat.min bvars.size raw.looseBVarRange
  if r == 0 then return (#[], 0)
  if r == 1 && raw.looseBVarRange == 1 then
    let last := bvars.back!
    chargeProjBvars 1
    if isDummyInput last then return (#[], 0)
    let m ← getTermFVarMask last.term
    return (#[last], m)
  if let some cached := s.projBvarsCache[(id, bvars)]? then
    chargeProjBvars 1
    return cached
  let liveIndices := if let some idxs := g.termLiveBVars[id]? then idxs else Id.run do
    let mut arr : Array Nat := #[]
    for j in [:raw.looseBVarRange] do
      if raw.hasLooseBVar j then arr := arr.push j
    arr
  chargeProjBvars (if liveIndices.isEmpty then 1 else liveIndices.size)
  let mut temp : Array Input := Array.replicate r dummyInput
  let mut mask := 0
  for j in liveIndices do
    if j < r then
      let v := bvars[bvars.size - 1 - j]!
      if !isDummyInput v then
        temp := temp.set! (r - 1 - j) v
        let vm ← getTermFVarMask v.term
        mask := mask ||| vm
  let mut start := 0
  while start < temp.size && isDummyInput temp[start]! do
    start := start + 1
  let out := if start == 0 then temp else if start == temp.size then #[] else temp.extract start temp.size
  modify fun st => { st with projBvarsCache := st.projBvarsCache.insert (id, bvars) (out, mask) }
  return (out, mask)

def filterHypByMask (mask : Nat) (hypothetical : Array Input) (isInputs : Bool) : Solver (Array Input) := do
  if mask == 0 || hypothetical.isEmpty then
    return #[]
  let s ← get
  if let some cached := s.projMaskHypCache[(mask, hypothetical)]? then
    if isInputs then chargeProjInputsHyp 1 else chargeProjHyp 1
    return cached
  if isInputs then chargeProjInputsHyp hypothetical.size else chargeProjHyp hypothetical.size
  let mut out : Array Input := #[]
  for inp in hypothetical do
    let im ← getTermFVarMask inp.term
    if (mask &&& im) != 0 then
      out := pushHypothetical out inp
  modify fun st => { st with projMaskHypCache := st.projMaskHypCache.insert (mask, hypothetical) out }
  return out

def projectNodeCtxM (id : TermId) (bvars : Array Input) (hypothetical : Array Input) : Solver (Array Input × Array Input) := do
  let s ← get
  let raw := s.graph.terms[id]!
  if (!raw.hasLooseBVars || bvars.isEmpty) && (!raw.hasFVar || hypothetical.isEmpty) then
    return (#[], #[])
  if let some cached := s.projNodeCtxCache[(id, bvars, hypothetical)]? then
    chargeProjBvars 1
    return cached
  let (liveBvars, bMask) ← liveBvarsForM id bvars
  let nMask ← if raw.hasFVar then getTermFVarMask id else pure 0
  let liveHyp ← filterHypByMask (bMask ||| nMask) hypothetical false
  modify fun st => { st with projNodeCtxCache := st.projNodeCtxCache.insert (id, bvars, hypothetical) (liveBvars, liveHyp) }
  return (liveBvars, liveHyp)

def liveHypForM (id : TermId) (bvars : Array Input) (hypothetical : Array Input) : Solver (Array Input) := do
  if hypothetical.isEmpty then return #[]
  let s ← get
  let g := s.graph
  let raw := g.terms[id]!
  if !raw.hasFVar && (!raw.hasLooseBVars || bvars.isEmpty) then
    return #[]
  if let some cached := s.projHypCache[(id, bvars, hypothetical)]? then
    chargeProjHyp 1
    return cached
  let mut mask ← if raw.hasFVar then do
    chargeProjHyp 1
    getTermFVarMask id
  else pure 0
  if raw.hasLooseBVars then
    let liveIndices := if let some idxs := g.termLiveBVars[id]? then idxs else Id.run do
      let mut arr : Array Nat := #[]
      for j in [:raw.looseBVarRange] do
        if raw.hasLooseBVar j then arr := arr.push j
      arr
    chargeProjHyp (if liveIndices.isEmpty then 1 else liveIndices.size)
    for j in liveIndices do
      if j < bvars.size then
        let b := bvars[bvars.size - 1 - j]!
        if !isDummyInput b && (← get).graph.terms[b.term]!.hasFVar then
          let bm ← getTermFVarMask b.term
          mask := mask ||| bm
  let out ← filterHypByMask mask hypothetical false
  modify fun st => { st with projHypCache := st.projHypCache.insert (id, bvars, hypothetical) out }
  return out

def liveHypForInputsM (inputs : Array Input) (hypothetical : Array Input) : Solver (Array Input) := do
  if hypothetical.isEmpty || inputs.isEmpty then return #[]
  let s ← get
  if let some cached := s.projInputsHypCache[(inputs, hypothetical)]? then
    chargeProjInputsHyp 1
    return cached
  chargeProjInputsHyp inputs.size
  let mut mask := 0
  for a in inputs do
    if !isDummyInput a && (← get).graph.terms[a.term]!.hasFVar then
      let am ← getTermFVarMask a.term
      mask := mask ||| am
  let out ← filterHypByMask mask hypothetical true
  modify fun st => { st with projInputsHypCache := st.projInputsHypCache.insert (inputs, hypothetical) out }
  return out

partial def filterTypeArgsPure (ty : Expr) (args : Array Input) (idx : Nat := 0) (dropTail : Bool := false) : Array Input :=
  if h : idx < args.size then
    match ty with
    | .forallE _ _ body _ =>
      let rest := filterTypeArgsPure body args (idx + 1) dropTail
      let kept := if body.hasLooseBVar 0 then args[idx] else dummyInput
      #[kept] ++ rest
    | .letE _ _ val body _ =>
      filterTypeArgsPure (body.instantiate1 val) args idx dropTail
    | .mdata _ b =>
      filterTypeArgsPure b args idx dropTail
    | _ =>
      if dropTail || ty.getAppFn'.isBVar then #[] else args.extract idx args.size
  else
    #[]

def filterTypeArgsM (tmpl : Template) (ty : Expr) (args : Array Input) : Solver (Array Input × Nat) := do
  if args.isEmpty then return (#[], 0)
  let dropTail := (← get).mutatedRow == "mut-row9b-type-alias-drop"
  if !dropTail && !tmpl.hasLetInForall && (args.size <= tmpl.forallArity || tmpl.bvarCodomain) then
    let effLen := Nat.min args.size tmpl.forallArity
    let prefixMask := (1 <<< effLen) - 1
    let masked := tmpl.typeKeepMask &&& prefixMask
    if masked == prefixMask && args.size == effLen then
      chargeProjTypeArgs 1
      return (args, 0)
    if masked == 0 then
      chargeProjTypeArgs 1
      return (Array.replicate effLen dummyInput, 0)
    let s ← get
    if let some cached := s.projTypeArgsCache[(tmpl.declaredType, args)]? then
      chargeProjTypeArgs 1
      return cached
    let mut out : Array Input := Array.replicate effLen dummyInput
    let mut keptCount := 0
    let mut mask := 0
    for idx in [:effLen] do
      if (masked &&& (1 <<< idx)) != 0 then
        let a := args[idx]!
        out := out.set! idx a
        keptCount := keptCount + 1
        let am ← getTermFVarMask a.term
        mask := mask ||| am
    chargeProjTypeArgs (if keptCount == 0 then 1 else keptCount)
    modify fun st => { st with projTypeArgsCache := st.projTypeArgsCache.insert (tmpl.declaredType, args) (out, mask) }
    return (out, mask)
  let s ← get
  if !dropTail then
    if let some cached := s.projTypeArgsCache[(tmpl.declaredType, args)]? then
      chargeProjTypeArgs 1
      return cached
  chargeProjTypeArgs args.size
  let out := filterTypeArgsPure ty args 0 dropTail
  let mut mask := 0
  for a in out do
    if !isDummyInput a then
      let am ← getTermFVarMask a.term
      mask := mask ||| am
  modify fun st => { st with projTypeArgsCache := st.projTypeArgsCache.insert (tmpl.declaredType, args) (out, mask) }
  return (out, mask)

def exprHasLambdaDepth (e : Expr) : Nat → Bool
  | 0 => true
  | k + 1 =>
    match cleanSpine e with
    | .lam _ _ body _ => exprHasLambdaDepth body k
    | _ => false

def hasLambdaDepth (g : Graph) (id : TermId) (n : Nat) : Bool :=
  exprHasLambdaDepth g.terms[id]! n

def ruleMinorArity (r : RecursorVal) (rule : RecursorRule) : Nat := Id.run do
  let mut e := rule.rhs
  for _ in [:r.numParams + r.numMotives + r.numMinors + rule.nfields] do
    match e with
    | .lam _ _ b _ => e := b
    | _ => break
  return e.getAppNumArgs

def wrapSupplierOrigin (appId : TermId) (extraOrigins : Array OriginId) (sup : CertificateSupplier) :
    Solver CertificateSupplier := do
  let mut comb := #[sup.input.origin]
  for o in extraOrigins do
    if o != 0 && !comb.contains o then
      comb := comb.push o
  let errOrigin ← origin (.substitution appId comb)
  return { sup with input := { sup.input with origin := errOrigin } }

def isKeyPending (s : SolverState) (k : CacheKey) : Bool :=
  match k with
  | .query q => s.pendingQueries.contains q
  | .typeQuery tq => s.pendingTypes.contains tq
  | .valueQuery vq => s.pendingValues.contains vq
  | .nodeQuery nq => s.pendingNodes.contains nq

def lookupKeyCompleted (s : SolverState) (k : CacheKey) : Option DerivationId :=
  match k with
  | .query q => s.completedQueries[q]?
  | .typeQuery tq => s.completedTypes[tq]?
  | .valueQuery vq => s.completedValues[vq]?
  | .nodeQuery nq => s.completedNodes[nq]?

partial def verifyDerivation (dId : DerivationId) : Solver Unit := do
  let s ← get
  if s.verifiedDerivations.contains dId then
    return
  let some dNode := s.derivationNodes[dId]? | throwError "missing-derivation-node"
  if isKeyPending s dNode.key then
    throwError "cyclic-certificate-dependency"
  unless lookupKeyCompleted s dNode.key == some dId do
    throwError "invalidated-certificate-prerequisite"
  chargeVerify (1 + dNode.children.size)
  modify fun st => { st with verifiedDerivations := st.verifiedDerivations.insert dId }
  for childId in dNode.children do
    verifyDerivation childId

def pushLocalTransfer (inp : Input) : Solver Unit :=
  modify fun s => {
    s with
      transfers := s.transfers.push inp,
      activeFirstTransfer := if s.activeFirstTransfer.isSome then s.activeFirstTransfer else some inp
  }

def pushLocalExclusion (inp : Input) : Solver Unit := do
  chargeCollect 1
  modify fun s => {
    s with
      structuralExclusions := if s.structuralExclusions.contains inp then s.structuralExclusions else s.structuralExclusions.push inp,
      activeExclusions := if s.activeExclusions.contains inp then s.activeExclusions else s.activeExclusions.push inp
  }

def recordChildDerivation (dId : DerivationId) : Solver Unit := do
  let s ← get
  if s.activeChildrenSet.contains dId then return
  let dNode := s.derivationNodes[dId]!
  if !dNode.exclusions.isEmpty then
    chargeCollect dNode.exclusions.size
  let mut excls := s.activeExclusions
  for ex in dNode.exclusions do
    unless excls.contains ex do
      excls := excls.push ex
  modify fun st => {
    st with
      activeChildren := st.activeChildren.push dId,
      activeChildrenSet := st.activeChildrenSet.insert dId,
      activeFirstTransfer := if st.activeFirstTransfer.isSome then st.activeFirstTransfer else dNode.firstTransfer,
      activeExclusions := excls
  }

def replayCompleted (dId : DerivationId) : Solver Input := do
  chargeReplay 1
  if dId < (← get).runStartNodeId then
    verifyDerivation dId
  recordChildDerivation dId
  return (← get).derivationNodes[dId]!.output

def recordCompletedNode (k : CacheKey) (out : Input) (localTrans : Array Input)
    (localExcl : Array Input) (children : Array DerivationId)
    (firstTrans : Option Input) (exclArr : Array Input) : Solver DerivationId := do
  chargeCollect 1
  let s ← get
  let dId := s.derivationNodes.size
  let node : CompletedNode := {
    key := k,
    output := out,
    localTransfers := localTrans,
    localExclusions := localExcl,
    children := children,
    firstTransfer := firstTrans,
    exclusions := exclArr
  }
  let s' := match k with
    | .query q => { s with completedQueries := s.completedQueries.insert q dId }
    | .typeQuery tq => { s with completedTypes := s.completedTypes.insert tq dId }
    | .valueQuery vq => { s with completedValues := s.completedValues.insert vq dId }
    | .nodeQuery nq => { s with completedNodes := s.completedNodes.insert nq dId }
  set {
    s' with
      derivationNodes := s'.derivationNodes.push node,
      verifiedDerivations := s'.verifiedDerivations.insert dId
  }
  return dId

def collectDerivationEvidence : Solver (Array Input × Array Input) := do
  chargeCollect 1
  let s ← get
  let finalTrans := match s.activeFirstTransfer with
    | some t => if s.transfers.isEmpty then #[t] else s.transfers
    | none => s.transfers
  return (finalTrans, s.activeExclusions)

def instantiateExpr (id : TermId) (bvars : Array Input) : Solver Expr := do
  let g := (← get).graph
  let raw := g.terms[id]!
  if !raw.hasLooseBVars || bvars.isEmpty then
    return raw
  let subst := bvars.map (fun inp => if isDummyInput inp then mkConst ``True else g.terms[inp.term]!)
  return raw.instantiateRev subst
"""

R5_MUTUAL = r"""
def runDerivationFrame (k : CacheKey) (step : Solver (Except CertificateSupplier Input)) :
    Solver (Except CertificateSupplier Input) := do
  let s0 ← get
  let prevTrans := s0.transfers
  let prevExcl := s0.structuralExclusions
  let prevChildren := s0.activeChildren
  let prevSet := s0.activeChildrenSet
  let prevFirst := s0.activeFirstTransfer
  let prevActiveExcl := s0.activeExclusions
  let s1 := match k with
    | .query q => { s0 with pendingQueries := s0.pendingQueries.insert q }
    | .typeQuery tq => { s0 with pendingTypes := s0.pendingTypes.insert tq }
    | .valueQuery vq => { s0 with pendingValues := s0.pendingValues.insert vq }
    | .nodeQuery nq => { s0 with pendingNodes := s0.pendingNodes.insert nq }
  set {
    s1 with
      transfers := #[],
      structuralExclusions := #[],
      activeChildren := #[],
      activeChildrenSet := {},
      activeFirstTransfer := none,
      activeExclusions := #[]
  }
  let restore (st : SolverState) : SolverState :=
    let st1 := match k with
      | .query q => { st with pendingQueries := st.pendingQueries.erase q }
      | .typeQuery tq => { st with pendingTypes := st.pendingTypes.erase tq }
      | .valueQuery vq => { st with pendingValues := st.pendingValues.erase vq }
      | .nodeQuery nq => { st with pendingNodes := st.pendingNodes.erase nq }
    {
      st1 with
        transfers := prevTrans,
        structuralExclusions := prevExcl,
        activeChildren := prevChildren,
        activeChildrenSet := prevSet,
        activeFirstTransfer := prevFirst,
        activeExclusions := prevActiveExcl
    }
  let res ← tryCatchRuntimeEx step (fun ex => do
    modify restore
    throw ex)
  let sAfter ← get
  let myTrans := sAfter.transfers
  let myExcl := sAfter.structuralExclusions
  let myChildren := sAfter.activeChildren
  let myFirst := sAfter.activeFirstTransfer
  let myActiveExcl := sAfter.activeExclusions
  modify restore
  match res with
  | .error sup => return .error sup
  | .ok out =>
    let dId ← recordCompletedNode k out myTrans myExcl myChildren myFirst myActiveExcl
    recordChildDerivation dId
    return .ok out

partial def needsValueCompletion (g : Graph) (termId : TermId) : Bool :=
  match g.values[termId]? with
  | none => false
  | some (.closure ..) | some (.partialCall ..) => true
  | some (.ctor _ _ cArgs) => cArgs.any (fun a => !isDummyInput a && needsValueCompletion g a.term)

mutual
partial def completePartialCall (entry : CertificateEntry) (key : TemplateKey)
    (savedArgs : Array Input) (lHyp : Array Input) (appId : TermId) (callInput : Input)
    (checkDeclType : Bool) : Solver (Except CertificateSupplier Input) := do
  let tmpl ← ensureTemplate key
  let ci ← getConstInfo key.name
  if checkDeclType then
    if let .error sup ← withHypotheticalLCtx lHyp (evalDeclaredType entry key tmpl savedArgs lHyp appId) then
      return .error (← wrapSupplierOrigin appId (savedArgs.map (·.origin)) sup)
  for aInp in savedArgs do
    chargeComplete 1
    if !isDummyInput aInp then
      if let .error sup ← withHypotheticalLCtx lHyp (completeValue entry aInp lHyp) then
        return .error (← wrapSupplierOrigin appId (savedArgs.map (·.origin)) sup)
  if (ci.value? (allowOpaque := true)).isSome then
    match ← withHypotheticalLCtx lHyp (evalNode entry tmpl.root #[] savedArgs lHyp .body) with
    | .error sup =>
      return .error (← wrapSupplierOrigin appId (savedArgs.map (·.origin)) sup)
    | .ok _ =>
      return .ok callInput
  else
    return .ok callInput

partial def completeValue (entry : CertificateEntry) (inp : Input) (hypothetical : Array Input) :
    Solver (Except CertificateSupplier Input) := do
  if isDummyInput inp then return .ok inp
  let g0 := (← get).graph
  if !needsValueCompletion g0 inp.term then return .ok inp
  let some v := g0.values[inp.term]? | return .ok inp
  let vKey : ValueQuery ← match v with
    | .closure nodeId savedBvars savedHyp => do
      let mHyp := mergeHypothetical savedHyp hypothetical
      let (lBvars, lHyp) ← projectNodeCtxM nodeId savedBvars mHyp
      pure { kind := .closure nodeId, term := inp.term, origin := inp.origin, inputs := lBvars, entryInputs := entry.query.inputs, guards := entry.query.guards, hypothetical := lHyp, mode := .body }
    | .partialCall key savedArgs savedHyp => do
      let mHyp := mergeHypothetical savedHyp hypothetical
      let lHyp ← liveHypForInputsM savedArgs mHyp
      pure { kind := .partialCall key, term := inp.term, origin := inp.origin, inputs := savedArgs, entryInputs := entry.query.inputs, guards := entry.query.guards, hypothetical := lHyp, mode := .body }
    | .ctor cName numParams cArgs => do
      let lHyp ← liveHypForInputsM cArgs hypothetical
      pure { kind := .ctor cName numParams, term := inp.term, origin := inp.origin, inputs := cArgs, entryInputs := entry.query.inputs, guards := entry.query.guards, hypothetical := lHyp, mode := .body }
  if (← get).pendingValues.contains vKey then
    throwError "cyclic-certificate-dependency"
  if let some dId := (← get).completedValues[vKey]? then
    let _ ← replayCompleted dId
    return .ok inp
  chargeComplete 1
  let lHyp := vKey.hypothetical
  runDerivationFrame (.valueQuery vKey) do
    match v with
    | .closure nodeId _ _ =>
      let lBvars := vKey.inputs
      let res ← withHypotheticalLCtx lHyp (evalNode entry nodeId lBvars #[] lHyp .body)
      match res with
      | .error sup =>
        return .error (← wrapSupplierOrigin inp.term #[inp.origin] sup)
      | .ok _ =>
        return .ok inp
    | .partialCall key savedArgs _ =>
      completePartialCall entry key savedArgs lHyp inp.term inp true
    | .ctor _ numParams cArgs =>
      for h : idx in [:cArgs.size] do
        chargeComplete 1
        let fInp := cArgs[idx]
        if !isDummyInput fInp then
          if let .error sup ← withHypotheticalLCtx lHyp (completeValue entry fInp lHyp) then
            let fOrig ← if idx >= numParams then origin (.field inp.origin (idx - numParams)) else pure inp.origin
            return .error (← wrapSupplierOrigin inp.term #[fOrig, fInp.origin] sup)
      return .ok inp

partial def applyClosureArgs (entry : CertificateEntry) (curId : TermId) (curBvars : Array Input)
    (args : Array Input) (argIdx : Nat) (mHyp : Array Input) (mode : Mode) :
    Solver (Except CertificateSupplier Input) := do
  if h : argIdx < args.size then
    let tr ← ensureNode curId
    match tr with
    | .binder _ bodyId =>
      chargeNode 1
      let usedBvars := if (← get).graph.terms[curId]!.hasLooseBVars then curBvars else #[]
      let arg0 := args[argIdx]
      let bodyNode := (← get).graph.terms[bodyId]!
      if !isDummyInput arg0 && !bodyNode.hasLooseBVar 0 && (← get).mutatedRow != "mut-row10-app" then
        if let .error sup ← withHypotheticalLCtx mHyp (completeValue entry arg0 mHyp) then
          return .error (← wrapSupplierOrigin curId (#[arg0.origin] ++ usedBvars.map (·.origin)) sup)
      let boundArg := if bodyNode.hasLooseBVar 0 then arg0 else dummyInput
      let res ← applyClosureArgs entry bodyId (usedBvars.push boundArg) args (argIdx + 1) mHyp mode
      match res with
      | .error sup =>
        return .error (← wrapSupplierOrigin curId (#[arg0.origin] ++ usedBvars.map (·.origin)) sup)
      | .ok bodyOut =>
        if !isDummyInput arg0 && bodyNode.hasLooseBVar 0 && (← get).mutatedRow != "mut-row10-app" then
          if let some (.ctor ..) := (← get).graph.values[arg0.term]? then
            if let .error sup ← withHypotheticalLCtx mHyp (completeValue entry arg0 mHyp) then
              return .error (← wrapSupplierOrigin curId (#[arg0.origin] ++ usedBvars.map (·.origin)) sup)
        return .ok bodyOut
    | _ =>
      let rest := args.extract argIdx args.size
      withHypotheticalLCtx mHyp (evalNode entry curId curBvars rest mHyp mode)
  else
    let tr ← ensureNode curId
    match tr with
    | .forallBinder .. =>
      evalCachedInvariantNode entry curId curBvars mHyp
    | _ =>
      let curExpr := (← get).graph.terms[curId]!
      let effBvars := if curExpr.hasLooseBVars then curBvars else #[]
      withHypotheticalLCtx mHyp (evalNode entry curId effBvars #[] mHyp mode)

partial def applyValue (entry : CertificateEntry) (fnInput : Input) (args : Array Input)
    (hypothetical : Array Input) (mode : Mode) : Solver (Except CertificateSupplier Input) := do
  if args.isEmpty then
    if mode == .body then
      return ← completeValue entry fnInput hypothetical
    return .ok fnInput
  match (← get).graph.values[fnInput.term]? with
  | some (.closure nodeId savedBvars savedHyp) =>
    let mHyp := mergeHypothetical savedHyp hypothetical
    applyClosureArgs entry nodeId savedBvars args 0 mHyp mode
  | some (.partialCall key savedArgs savedHyp) =>
    let mHyp := mergeHypothetical savedHyp hypothetical
    withHypotheticalLCtx mHyp (evalCall entry key (savedArgs ++ args) mHyp mode)
  | _ =>
    let g := (← get).graph
    let fnExpr := g.terms[fnInput.term]!
    let argExprs := args.map (fun a => g.terms[a.term]!)
    let appExpr := mkAppN fnExpr argExprs
    let reduced := appExpr.headBeta
    if reduced != appExpr then
      let redId ← term reduced
      match ← withHypotheticalLCtx hypothetical (evalNode entry redId #[] #[] hypothetical mode) with
      | .error sup =>
        return .error (← wrapSupplierOrigin redId (#[fnInput.origin] ++ args.map (·.origin)) sup)
      | .ok out =>
        let o ← origin (.substitution redId (#[out.origin, fnInput.origin] ++ args.map (·.origin)))
        return .ok (← canonicalizeInput { out with origin := o })
    else
      let appId ← term appExpr
      if (← get).mutatedRow != "mut-row2b-neutral-fvar-arg" then
        for a in args do
          if let .error sup ← completeValue entry a hypothetical then
            return .error (← wrapSupplierOrigin appId (#[fnInput.origin] ++ args.map (·.origin)) sup)
      let o ← origin (.substitution appId (#[fnInput.origin] ++ args.map (·.origin)))
      let inp ← withHypotheticalLCtx hypothetical (mkInput appExpr o)
      return .ok inp

partial def openMinorFieldsStep (entry : CertificateEntry) (minorInput : Input) (majorInput : Input)
    (extraArgs : Array Input) (minorArity : Nat) (idx : Nat) (liveH : Array Input) :
    Solver (Except CertificateSupplier Input) := do
  if idx >= minorArity then
    withHypotheticalLCtx liveH (applyValue entry minorInput extraArgs liveH .body)
  else
    let mExpr := (← get).graph.terms[minorInput.term]!
    let mTy ← withHypotheticalLCtx liveH ( do whnfD (← inferType mExpr) )
    if let .forallE _ dom _ _ := mTy then
      let domId ← term (cleanSpine dom)
      let gNow := (← get).graph
      let fOrig ← if hasSuppliedOrigin gNow majorInput.origin || !entry.query.inputs.isEmpty then
          some <$> origin (.field majorInput.origin idx)
        else
          pure none
      withCanonicalBinder domId fOrig liveH fun xInput => do
        let nextHyp := pushHypothetical liveH xInput
        match ← applyValue entry minorInput #[xInput] nextHyp .closure with
        | .error sup => return .error sup
        | .ok nextMinor =>
          openMinorFieldsStep entry nextMinor majorInput extraArgs minorArity (idx + 1) nextHyp
    else
      withHypotheticalLCtx liveH (applyValue entry minorInput extraArgs liveH .body)

partial def computeMinorNeededMask (g : Graph) (curNodeId : TermId) (minorArity : Nat) (extraCount : Nat) : Nat := Id.run do
  let mut doms : Array Expr := Array.mkEmpty minorArity
  let mut e := cleanSpine g.terms[curNodeId]!
  for _ in [:minorArity] do
    match e with
    | .lam _ d b _ =>
      doms := doms.push (cleanSpine d)
      e := cleanSpine b
    | _ => return (1 <<< minorArity) - 1
  let mut peeledExtra := 0
  for _ in [:extraCount] do
    match e with
    | .lam _ _ b _ =>
      e := cleanSpine b
      peeledExtra := peeledExtra + 1
    | _ => break
  let rhs := e
  let mut mask := 0
  for j in [:minorArity] do
    if rhs.hasLooseBVar (peeledExtra + minorArity - 1 - j) then
      mask := mask ||| (1 <<< j)
  let mut idx := minorArity
  while idx > 0 do
    idx := idx - 1
    if (mask &&& (1 <<< idx)) != 0 then
      let d := doms[idx]!
      for j in [:idx] do
        if d.hasLooseBVar (idx - 1 - j) then
          mask := mask ||| (1 <<< j)
  return mask

partial def peelMinorClosure (entry : CertificateEntry) (curNodeId : TermId)
    (curBvars : Array Input) (minorInput : Input) (majorInput : Input)
    (extraArgs : Array Input) (minorArity : Nat) (neededMask : Nat) (idx : Nat) (liveH : Array Input) :
    Solver (Except CertificateSupplier Input) := do
  if idx >= minorArity then
    let curExpr := (← get).graph.terms[curNodeId]!
    let effBvars := if curExpr.hasLooseBVars then curBvars else #[]
    if extraArgs.isEmpty then
      let res ← withHypotheticalLCtx liveH (evalNode entry curNodeId effBvars #[] liveH .body)
      match res with
      | .error sup =>
        return .error (← wrapSupplierOrigin minorInput.term #[minorInput.origin] sup)
      | .ok out =>
        return .ok out
    else
      let res ← applyClosureArgs entry curNodeId effBvars extraArgs 0 liveH .body
      match res with
      | .error sup =>
        return .error (← wrapSupplierOrigin minorInput.term (#[minorInput.origin] ++ extraArgs.map (·.origin)) sup)
      | .ok out =>
        return .ok out
  else
    let tr ← ensureNode curNodeId
    match tr with
    | .binder typeId bodyId =>
      let usedBvars := if (← get).graph.terms[curNodeId]!.hasLooseBVars then curBvars else #[]
      let bodyNode := (← get).graph.terms[bodyId]!
      let gNow := (← get).graph
      let needFieldOrigin := hasSuppliedOrigin gNow majorInput.origin || !entry.query.inputs.isEmpty
      let isNeeded := (neededMask &&& (1 <<< idx)) != 0
      if (!isNeeded || !bodyNode.hasLooseBVar 0) && !needFieldOrigin then
        peelMinorClosure entry bodyId (usedBvars.push dummyInput) minorInput majorInput extraArgs minorArity neededMask (idx + 1) liveH
      else
        let domExpr ← instantiateExpr typeId usedBvars
        let domId ← term (cleanSpine domExpr)
        let fOrig ← if needFieldOrigin then some <$> origin (.field majorInput.origin idx) else pure none
        withCanonicalBinder domId fOrig liveH fun xInput => do
          let boundArg := if isNeeded && bodyNode.hasLooseBVar 0 then xInput else dummyInput
          let nextHyp := if isNeeded && bodyNode.hasLooseBVar 0 then pushHypothetical liveH xInput else liveH
          peelMinorClosure entry bodyId (usedBvars.push boundArg) minorInput majorInput extraArgs minorArity neededMask (idx + 1) nextHyp
    | _ =>
      openMinorFieldsStep entry minorInput majorInput extraArgs minorArity idx liveH

partial def openMinorFields (entry : CertificateEntry) (minorInput : Input) (majorInput : Input)
    (extraArgs : Array Input) (minorArity : Nat) (idx : Nat) (hypothetical : Array Input) :
    Solver (Except CertificateSupplier Input) := do
  let liveH ← liveHypForInputsM (#[minorInput] ++ extraArgs) hypothetical
  let g0 := (← get).graph
  let runStep : Solver (Except CertificateSupplier Input) := do
    if let some (.closure nodeId savedBvars savedHyp) := (← get).graph.values[minorInput.term]? then
      if hasLambdaDepth g0 nodeId minorArity then
        let mHyp := mergeHypothetical savedHyp liveH
        let neededMask := computeMinorNeededMask g0 nodeId minorArity extraArgs.size
        return ← peelMinorClosure entry nodeId savedBvars minorInput majorInput extraArgs minorArity neededMask idx mHyp
    openMinorFieldsStep entry minorInput majorInput extraArgs minorArity idx liveH
  if !hasSuppliedOrigin g0 majorInput.origin && entry.query.inputs.isEmpty then
    let mKey : ValueQuery := {
      kind := .minor minorArity,
      term := minorInput.term,
      origin := minorInput.origin,
      inputs := extraArgs,
      entryInputs := entry.query.inputs,
      guards := entry.query.guards,
      hypothetical := liveH,
      mode := .body
    }
    if (← get).pendingValues.contains mKey then
      throwError "cyclic-certificate-dependency"
    if let some dId := (← get).completedValues[mKey]? then
      let out ← replayCompleted dId
      return .ok out
    runDerivationFrame (.valueQuery mKey) runStep
  else
    runStep

partial def evalCachedInvariantNode (entry : CertificateEntry) (id : TermId)
    (bvars : Array Input) (hypothetical : Array Input) : Solver (Except CertificateSupplier Input) := do
  if let .bvar idx := (← get).graph.terms[id]! then
    if idx < bvars.size then
      let inp := bvars[bvars.size - 1 - idx]!
      if let .error sup ← withHypotheticalLCtx hypothetical (completeValue entry inp hypothetical) then
        return .error sup
      return .ok inp
  let (liveBvars, liveHyp) ← projectNodeCtxM id bvars hypothetical
  let tKey : TypeQuery := {
    node := id,
    bvars := liveBvars,
    entryInputs := entry.query.inputs,
    guards := entry.query.guards,
    hypothetical := liveHyp
  }
  if (← get).pendingTypes.contains tKey then
    throwError "cyclic-certificate-dependency"
  if let some dId := (← get).completedTypes[tKey]? then
    let out ← replayCompleted dId
    return .ok out
  let nKey : NodeQuery := {
    node := id,
    bvars := liveBvars,
    args := #[],
    entryInputs := entry.query.inputs,
    guards := entry.query.guards,
    hypothetical := liveHyp,
    mode := .closure
  }
  if (← get).pendingNodes.contains nKey then
    throwError "cyclic-certificate-dependency"
  if let some dId := (← get).completedNodes[nKey]? then
    let out ← replayCompleted dId
    if let .error sup ← withHypotheticalLCtx liveHyp (completeValue entry out liveHyp) then
      return .error sup
    return .ok out
  runDerivationFrame (.typeQuery tKey) do
    match ← withHypotheticalLCtx liveHyp (evalNodeDirect entry id liveBvars #[] liveHyp .closure) with
    | .error sup => return .error sup
    | .ok out =>
      if let .error sup ← withHypotheticalLCtx liveHyp (completeValue entry out liveHyp) then
        return .error sup
      return .ok out

partial def evalTypeInstantiate (entry : CertificateEntry) (id : TermId) (bvars : Array Input)
    (typeArgs : Array Input) (hypothetical : Array Input) : Solver (Except CertificateSupplier Input) := do
  chargeTypeInst 1
  let transfer ← ensureNode id
  match transfer with
  | .forallBinder typeId bodyId =>
    if typeArgs.isEmpty then
      evalCachedInvariantNode entry id bvars hypothetical
    else
      let usedBvars := if (← get).graph.terms[id]!.hasLooseBVars then bvars else #[]
      match ← evalCachedInvariantNode entry typeId usedBvars hypothetical with
      | .error sup => return .error sup
      | .ok _ =>
        let arg0 := typeArgs[0]!
        let rest := typeArgs.extract 1 typeArgs.size
        let bodyNode := (← get).graph.terms[bodyId]!
        let boundArg := if bodyNode.hasLooseBVar 0 then arg0 else dummyInput
        evalTypeInstantiate entry bodyId (usedBvars.push boundArg) rest hypothetical
  | .letBind typeId valueId bodyId =>
    match ← evalCachedInvariantNode entry typeId bvars hypothetical with
    | .error sup => return .error sup
    | .ok _ =>
      match ← evalNode entry valueId bvars #[] hypothetical .closure with
      | .error sup => return .error sup
      | .ok valInput =>
        let bodyNode := (← get).graph.terms[bodyId]!
        if !bodyNode.hasLooseBVar 0 then
          if let .error sup ← completeValue entry valInput hypothetical then
            return .error sup
        let boundVal := if bodyNode.hasLooseBVar 0 then valInput else dummyInput
        let res ← evalTypeInstantiate entry bodyId (bvars.push boundVal) typeArgs hypothetical
        match res with
        | .error sup => return .error sup
        | .ok out =>
          if bodyNode.hasLooseBVar 0 then
            if let some (.ctor ..) := (← get).graph.values[valInput.term]? then
              if let .error sup ← completeValue entry valInput hypothetical then
                return .error sup
          return .ok out
  | .call key inputIds =>
    if !typeArgs.isEmpty then
      if (← get).mutatedRow == "mut-row9b-type-alias" then
        return .ok dummyInput
      let ci ← getConstInfo key.name
      if (ci.value? (allowOpaque := true)).isSome then
        let mut evalInputs : Array Input := #[]
        for childId in inputIds do
          match ← evalNode entry childId bvars #[] hypothetical .closure with
          | .error sup => return .error sup
          | .ok inp => evalInputs := evalInputs.push inp
        let tmpl ← ensureTemplate key
        let g := (← get).graph
        let argExprs := evalInputs.map (fun a => g.terms[a.term]!)
        let appExpr := mkAppN (mkConst key.name key.levels) argExprs
        let appId ← term appExpr
        match ← evalDeclaredType entry key tmpl evalInputs hypothetical appId with
        | .error sup => return .error sup
        | .ok _ =>
          return ← evalTypeInstantiate entry tmpl.root #[] (evalInputs ++ typeArgs) hypothetical
    evalCachedInvariantNode entry id bvars hypothetical
  | .binder typeId bodyId =>
    if !typeArgs.isEmpty then
      let usedBvars := if (← get).graph.terms[id]!.hasLooseBVars then bvars else #[]
      match ← evalCachedInvariantNode entry typeId usedBvars hypothetical with
      | .error sup => return .error sup
      | .ok _ =>
        let arg0 := typeArgs[0]!
        let rest := typeArgs.extract 1 typeArgs.size
        let bodyNode := (← get).graph.terms[bodyId]!
        if !isDummyInput arg0 && !bodyNode.hasLooseBVar 0 then
          if let .error sup ← completeValue entry arg0 hypothetical then
            return .error sup
        let boundArg := if bodyNode.hasLooseBVar 0 then arg0 else dummyInput
        return ← evalTypeInstantiate entry bodyId (usedBvars.push boundArg) rest hypothetical
    evalCachedInvariantNode entry id bvars hypothetical
  | _ =>
    evalCachedInvariantNode entry id bvars hypothetical

partial def evalDeclaredType (entry : CertificateEntry) (key : TemplateKey) (tmpl : Template)
    (actualArgs : Array Input) (hypothetical : Array Input) (appId : TermId) :
    Solver (Except CertificateSupplier Input) := do
  if (← get).mutatedRow == "mut-row12a-decltype" then
    return .ok dummyInput
  let declTyExpr := (← get).graph.terms[tmpl.declaredType]!
  let (typeInputs, typeMask) ← filterTypeArgsM tmpl declTyExpr actualArgs
  let typeHyp ← if typeInputs == actualArgs then pure hypothetical else filterHypByMask typeMask hypothetical true
  let qType : Query := {
    template := { key with mode := .invariant },
    inputs := typeInputs,
    entryInputs := entry.query.inputs,
    guards := entry.query.guards,
    hypothetical := typeHyp,
    mode := .invariant
  }
  if (← get).pendingQueries.contains qType then
    throwError "cyclic-certificate-dependency"
  if let some dId := (← get).completedQueries[qType]? then
    let cached ← replayCompleted dId
    let mut combOrigins := #[cached.origin]
    for a in actualArgs do
      unless combOrigins.contains a.origin do
        combOrigins := combOrigins.push a.origin
    let outOrigin ← origin (.substitution appId combOrigins)
    return .ok (← canonicalizeInput { cached with origin := outOrigin })
  enqueue (.query qType)
  modify fun s => { s with worklist := s.worklist.pop, queryCount := s.queryCount + 1 }
  let res ← runDerivationFrame (.query qType) do
    if !tmpl.hasLetInForall && tmpl.forallArity > 0 && typeInputs.size >= tmpl.forallArity then
      let k := tmpl.forallArity
      let mut domMask := 0
      let domInputs : Array Input ← if tmpl.domainKeepMask == 0 then pure #[] else if typeInputs.size == k && tmpl.domainKeepMask == (tmpl.typeKeepMask &&& ((1 <<< k) - 1)) then do
        domMask := typeMask
        pure typeInputs
      else do
        let mut arr : Array Input := Array.mkEmpty k
        for j in [:k] do
          if (tmpl.domainKeepMask &&& (1 <<< j)) != 0 then
            let v := typeInputs[j]!
            arr := arr.push v
            let vm ← getTermFVarMask v.term
            domMask := domMask ||| vm
          else
            arr := arr.push dummyInput
        pure arr
      let domHyp ← if domInputs.isEmpty || typeHyp.isEmpty then pure #[] else if domInputs == typeInputs then pure typeHyp else filterHypByMask domMask typeHyp true
      let tDomKey : TypeQuery := {
        node := tmpl.declaredType,
        bvars := domInputs,
        entryInputs := entry.query.inputs,
        guards := entry.query.guards,
        hypothetical := domHyp,
        isDomainPrefix := true
      }
      if (← get).pendingTypes.contains tDomKey then
        throwError "cyclic-certificate-dependency"
      let domRes : Except CertificateSupplier Input ← if let some dId := (← get).completedTypes[tDomKey]? then
        let out ← replayCompleted dId
        pure (.ok out)
      else
        runDerivationFrame (.typeQuery tDomKey) do
          for idx in [:k] do
            chargeTypeInst 1
            let domId := tmpl.forallDomains[idx]!
            let pref := if idx == 0 || domInputs.isEmpty then #[] else domInputs.extract 0 idx
            match ← withHypotheticalLCtx domHyp (evalCachedInvariantNode entry domId pref domHyp) with
            | .error sup => return .error sup
            | .ok _ => pure ()
          return .ok dummyInput
      match domRes with
      | .error sup => return .error sup
      | .ok _ =>
        let codBvars := typeInputs.extract 0 k
        let restArgs := typeInputs.extract k typeInputs.size
        if restArgs.isEmpty then
          withHypotheticalLCtx typeHyp (evalCachedInvariantNode entry tmpl.forallCodomain codBvars typeHyp)
        else
          withHypotheticalLCtx typeHyp (evalTypeInstantiate entry tmpl.forallCodomain codBvars restArgs typeHyp)
    else
      withHypotheticalLCtx typeHyp (evalTypeInstantiate entry tmpl.declaredType #[] typeInputs typeHyp)
  match res with
  | .ok typeOut =>
    let mut combOrigins := #[typeOut.origin]
    for a in actualArgs do
      unless combOrigins.contains a.origin do
        combOrigins := combOrigins.push a.origin
    let outOrigin ← origin (.substitution appId combOrigins)
    return .ok (← canonicalizeInput { typeOut with origin := outOrigin })
  | .error sup =>
    return .error (← wrapSupplierOrigin appId (actualArgs.map (·.origin)) sup)
"""

R5_MUTUAL_2 = r"""
partial def evalCall (entry : CertificateEntry) (key : TemplateKey) (actualArgs : Array Input)
    (hypothetical : Array Input) (mode : Mode) (optArgsMask : Option Nat := none) : Solver (Except CertificateSupplier Input) := do
  let queryHyp ← match optArgsMask with
    | some m => filterHypByMask m hypothetical true
    | none => liveHypForInputsM actualArgs hypothetical
  let q : Query := {
    template := key,
    inputs := actualArgs,
    entryInputs := entry.query.inputs,
    guards := entry.query.guards,
    hypothetical := queryHyp,
    mode := mode
  }
  if (← get).pendingQueries.contains q then
    throwError "cyclic-certificate-dependency"
  if let some dId := (← get).completedQueries[q]? then
    let out ← replayCompleted dId
    return .ok out
  if mode == .closure && !actualArgs.isEmpty then
    let qBody : Query := { q with mode := .body }
    if let some dId := (← get).completedQueries[qBody]? then
      let out ← replayCompleted dId
      return .ok out
  enqueue (.query q)
  modify fun s => { s with worklist := s.worklist.pop, queryCount := s.queryCount + 1 }
  runDerivationFrame (.query q) do
    chargeCall 1
    let tmpl ← ensureTemplate key
    let ci ← getConstInfo key.name
    let g := (← get).graph
    let argExprs := actualArgs.map (fun a => g.terms[a.term]!)
    let appExpr := mkAppN (mkConst key.name key.levels) argExprs
    let appId ← term appExpr
    let callOrigin ← origin (.substitution appId (actualArgs.map (·.origin)))
    let callInput ← withHypotheticalLCtx queryHyp (mkInput appExpr callOrigin)
    match ← withHypotheticalLCtx queryHyp (evalDeclaredType entry key tmpl actualArgs queryHyp appId) with
    | .error sup => return .error sup
    | .ok _ => pure ()
    let hasVal := (ci.value? (allowOpaque := true)).isSome
    let res : Except CertificateSupplier Input ← match hasVal with
      | true => do
        if (← get).mutatedRow == "mut-row11-const-val" || ((← get).mutatedRow == "mut-row13-matcher" && (← getMatcherInfo? key.name).isSome) then
          pushLocalTransfer callInput
          pure (.ok callInput)
        else if actualArgs.isEmpty && mode == .closure then
          let rootTransfer ← ensureNode (if tmpl.bodyArity > 0 then tmpl.root else tmpl.bodyRoot)
          let isLam := tmpl.bodyArity > 0 || (match rootTransfer with | .binder .. => true | _ => false)
          if isLam then
            pushLocalTransfer callInput
            recordValue callInput.term (.partialCall key #[] queryHyp)
            pure (.ok callInput)
          else
            match ← withHypotheticalLCtx queryHyp (evalNode entry tmpl.root #[] actualArgs queryHyp mode) with
            | .ok bodyOut =>
              let mut combOrigins := #[bodyOut.origin]
              for a in actualArgs do
                unless combOrigins.contains a.origin do
                  combOrigins := combOrigins.push a.origin
              let outOrigin ← origin (.substitution appId combOrigins)
              let outInput ← canonicalizeInput { bodyOut with origin := outOrigin }
              pushLocalTransfer outInput
              pure (.ok outInput)
            | .error sup =>
              pure (.error (← wrapSupplierOrigin appId (actualArgs.map (·.origin)) sup))
        else if tmpl.bodyArity > 0 && actualArgs.size >= tmpl.bodyArity then
          let k := tmpl.bodyArity
          let bodyNode := (← get).graph.terms[tmpl.bodyRoot]!
          let mut initBvars : Array Input := Array.mkEmpty k
          for idx in [:k] do
            let arg := actualArgs[idx]!
            let isDeadAtBinder := (tmpl.deadMask &&& (1 <<< idx)) != 0
            if !isDummyInput arg && isDeadAtBinder && (← get).mutatedRow != "mut-row10-app" then
              if let .error sup ← withHypotheticalLCtx queryHyp (completeValue entry arg queryHyp) then
                return .error (← wrapSupplierOrigin appId (actualArgs.map (·.origin)) sup)
            let isUsed := bodyNode.hasLooseBVar (k - 1 - idx)
            initBvars := initBvars.push (if isUsed then arg else dummyInput)
          let restArgs := actualArgs.extract k actualArgs.size
          match ← withHypotheticalLCtx queryHyp (evalNode entry tmpl.bodyRoot initBvars restArgs queryHyp mode) with
          | .ok bodyOut =>
            for idx in [:k] do
              let arg := actualArgs[idx]!
              let isDeadAtBinder := (tmpl.deadMask &&& (1 <<< idx)) != 0
              if !isDummyInput arg && !isDeadAtBinder && (← get).mutatedRow != "mut-row10-app" then
                if let some (.ctor ..) := (← get).graph.values[arg.term]? then
                  if let .error sup ← withHypotheticalLCtx queryHyp (completeValue entry arg queryHyp) then
                    return .error (← wrapSupplierOrigin appId (actualArgs.map (·.origin)) sup)
            let mut combOrigins := #[bodyOut.origin]
            for a in actualArgs do
              unless combOrigins.contains a.origin do
                combOrigins := combOrigins.push a.origin
            let outOrigin ← origin (.substitution appId combOrigins)
            let outInput ← canonicalizeInput { bodyOut with origin := outOrigin }
            pushLocalTransfer outInput
            pure (.ok outInput)
          | .error sup =>
            pure (.error (← wrapSupplierOrigin appId (actualArgs.map (·.origin)) sup))
        else
          match ← withHypotheticalLCtx queryHyp (evalNode entry tmpl.root #[] actualArgs queryHyp mode) with
          | .ok bodyOut =>
            let mut combOrigins := #[bodyOut.origin]
            for a in actualArgs do
              unless combOrigins.contains a.origin do
                combOrigins := combOrigins.push a.origin
            let outOrigin ← origin (.substitution appId combOrigins)
            let outInput ← canonicalizeInput { bodyOut with origin := outOrigin }
            pushLocalTransfer outInput
            pure (.ok outInput)
          | .error sup =>
            pure (.error (← wrapSupplierOrigin appId (actualArgs.map (·.origin)) sup))
      | false => do
        match ci with
        | .opaqueInfo _ | .axiomInfo _ =>
          for a in actualArgs do
            if let .error sup ← withHypotheticalLCtx queryHyp (completeValue entry a queryHyp) then
              return .error (← wrapSupplierOrigin appId (actualArgs.map (·.origin)) sup)
          let declTyExpr := (← get).graph.terms[tmpl.declaredType]!
          if !declTyExpr.isForall && !declTyExpr.isLet && actualArgs.isEmpty then
            let supOrigin ← origin (.supplied entry.instanceName 0 appId)
            let supInput : Input := { callInput with origin := supOrigin }
            pushLocalTransfer supInput
            pure (.error { declaration := key.name, input := supInput })
          else if actualArgs.size < tmpl.forallArity then
            if mode == .body then
              completePartialCall entry key actualArgs queryHyp appId callInput false
            else
              pushLocalTransfer callInput
              recordValue callInput.term (.partialCall key actualArgs queryHyp)
              pure (.ok callInput)
          else if !(← isProp (← withHypotheticalLCtx queryHyp (inferType appExpr))) then
            pushLocalTransfer callInput
            pure (.ok callInput)
          else if !key.name.isInternal && (key.name.getRoot == `ConcreteSpec || key.name.getRoot == `ConcreteSpecControl || key.name.getRoot == `CertificateRepro || key.name.getRoot == `Baseline || key.name == `impossible || key.name == `suppliedByBool || key.name == `ax) then
            let baseSup ← origin (.supplied entry.instanceName 0 appId)
            let supOrigin ← if actualArgs.isEmpty then
                pure baseSup
              else
                origin (.substitution appId (#[baseSup] ++ actualArgs.map (·.origin)))
            let supInput : Input := { callInput with origin := supOrigin }
            pushLocalTransfer supInput
            pure (.error { declaration := key.name, input := supInput })
          else if (← get).mutatedRow == "mut-row12b-opaque" then
            pushLocalTransfer callInput
            pure (.ok callInput)
          else
            pushLocalTransfer callInput
            throwError "unavailable-certificate-transfer {key.name}"
        | .ctorInfo c =>
          pushLocalTransfer callInput
          if actualArgs.size < c.numParams + c.numFields then
            if (← get).mutatedRow == "mut-row12c-ctor-partial" then
              pure (.ok callInput)
            else if mode == .body then
              completePartialCall entry key actualArgs queryHyp appId callInput false
            else
              recordValue callInput.term (.partialCall key actualArgs queryHyp)
              pure (.ok callInput)
          else
            if (← get).mutatedRow != "mut-row13a-ctor-saturated" then
              recordValue callInput.term (.ctor key.name c.numParams actualArgs)
            pure (.ok callInput)
        | .recInfo r =>
          pushLocalTransfer callInput
          let majorIdx := r.getMajorIdx
          let recRes : Except CertificateSupplier Input ← if let some majorInput := actualArgs[majorIdx]? then
            if r.rules.isEmpty then
              if (← get).mutatedRow != "mut-row14a-empty-rec" then
                if let .error sup ← withHypotheticalLCtx queryHyp (completeValue entry majorInput queryHyp) then
                  return .error (← wrapSupplierOrigin appId (actualArgs.map (·.origin)) sup)
                if mode == .body then
                  for a in actualArgs do
                    if let .error sup ← withHypotheticalLCtx queryHyp (completeValue entry a queryHyp) then
                      return .error (← wrapSupplierOrigin appId (actualArgs.map (·.origin)) sup)
              let gNow := (← get).graph
              if isDirectHypotheticalOrigin gNow majorInput.origin then
                pushLocalExclusion majorInput
              pure (.ok callInput)
            else
              let mut matchedRule : Option (Name × Nat × TermId × Array Input) := none
              if let some (.ctor cName numParams cArgs) := (← get).graph.values[majorInput.term]? then
                if let some (_, nfields, rhsId) := tmpl.rules.find? (fun (cn, _, _) => cn == cName) then
                  if cArgs.size >= numParams + nfields then
                    matchedRule := some (cName, nfields, rhsId, cArgs.extract numParams (numParams + nfields))
              if matchedRule.isNone then
                let majorExpr := (← get).graph.terms[majorInput.term]!
                let mut redMajor ← withHypotheticalLCtx queryHyp (whnfD majorExpr)
                if let .lit l := redMajor then
                  let skipNat := (← get).mutatedRow == "mut-row5a-natlit" && (match l with | .natVal _ => true | _ => false)
                  let skipStr := ((← get).mutatedRow == "mut-row5b-strlit" || (← get).mutatedRow == "mut-row5b-strlit-open") && (match l with | .strVal _ => true | _ => false)
                  if !skipNat && !skipStr then
                    redMajor ← redMajor.toCtorIfLit
                if let .const cName _ := redMajor.getAppFn' then
                  if let some (.ctorInfo cInfo) := (← getEnv).find? cName then
                    if let some (_, nfields, rhsId) := tmpl.rules.find? (fun (cn, _, _) => cn == cName) then
                      let rawArgs := redMajor.getAppArgs
                      if rawArgs.size >= cInfo.numParams + nfields then
                        let mut fInputs : Array Input := #[]
                        for idx in [:nfields] do
                          let fExpr := rawArgs[cInfo.numParams + idx]!
                          let fO ← origin (.field majorInput.origin idx)
                          let fInp ← withHypotheticalLCtx queryHyp (mkInput fExpr fO)
                          if let .lit (.natVal fn) := cleanSpine fExpr then
                            if (← get).mutatedRow != "mut-row5a-natlit" then
                              if fn == 0 then
                                recordValue fInp.term (.ctor ``Nat.zero 0 #[])
                              else
                                let predExpr := mkNatLit (fn - 1)
                                let predInp ← withHypotheticalLCtx queryHyp (mkInput predExpr fO)
                                recordValue fInp.term (.ctor ``Nat.succ 0 #[predInp])
                          fInputs := fInputs.push fInp
                        matchedRule := some (cName, nfields, rhsId, fInputs)
              if let some (cName, nfields, rhsId, fieldInputs) := matchedRule then
                let rhsNode := (← get).graph.terms[rhsId]!
                let prefixLen := r.numParams + r.numMotives + r.numMinors
                let mut paramsAndMinors := actualArgs.extract 0 prefixLen
                if (← get).mutatedRow == "mut-row14b-rec-ctor" then
                  let minorStart := r.numParams + r.numMotives
                  for (cn, _, _) in tmpl.rules, mIdx in [:r.numMinors] do
                    if cn != cName && minorStart + mIdx < paramsAndMinors.size then
                      paramsAndMinors := paramsAndMinors.set! (minorStart + mIdx) dummyInput
                let extraArgs := if actualArgs.size > majorIdx + 1 then actualArgs.extract (majorIdx + 1) actualArgs.size else #[]
                let rhsBvars := paramsAndMinors ++ fieldInputs
                let numTotal := rhsNode.getAppNumArgs
                let numIH := numTotal - nfields
                let mut fastMinor : Option (Input × Nat) := none
                if numIH > 0 && numTotal >= nfields then
                  if let .bvar minorBvarIdx := rhsNode.getAppFn' then
                    if let some mInp := rhsBvars[rhsBvars.size - 1 - minorBvarIdx]? then
                      let gNow := (← get).graph
                      if let some (.closure nodeId _ _) := gNow.values[mInp.term]? then
                        if hasLambdaDepth gNow nodeId numTotal then
                          let neededMask := computeMinorNeededMask gNow nodeId numTotal extraArgs.size
                          if ((neededMask >>> nfields) &&& ((1 <<< numIH) - 1)) == 0 then
                            fastMinor := some (mInp, neededMask)
                if let some (mInp, neededMask) := fastMinor then
                  if (← get).mutatedRow != "mut-row14b-rec-ctor" then
                    for h : fIdx in [:fieldInputs.size] do
                      if (neededMask &&& (1 <<< fIdx)) == 0 then
                        if let .error sup ← withHypotheticalLCtx queryHyp (completeValue entry fieldInputs[fIdx] queryHyp) then
                          return .error (← wrapSupplierOrigin appId (actualArgs.map (·.origin)) sup)
                  let callArgs := fieldInputs ++ Array.replicate numIH dummyInput ++ extraArgs
                  withHypotheticalLCtx queryHyp (applyValue entry mInp callArgs queryHyp mode)
                else
                  if (← get).mutatedRow != "mut-row14b-rec-ctor" then
                    for h : fIdx in [:fieldInputs.size] do
                      if !rhsNode.hasLooseBVar (fieldInputs.size - 1 - fIdx) then
                        if let .error sup ← withHypotheticalLCtx queryHyp (completeValue entry fieldInputs[fIdx] queryHyp) then
                          return .error (← wrapSupplierOrigin appId (actualArgs.map (·.origin)) sup)
                  withHypotheticalLCtx queryHyp (evalNode entry rhsId rhsBvars extraArgs queryHyp mode)
              else
                let mRow := (← get).mutatedRow
                if mRow == "mut-row14c-rec-symbolic" || mRow == "mut-row5a-natlit" || mRow == "mut-row5b-strlit-open" then
                  pure (.ok callInput)
                else
                  if let .error sup ← withHypotheticalLCtx queryHyp (completeValue entry majorInput queryHyp) then
                    return .error (← wrapSupplierOrigin appId (actualArgs.map (·.origin)) sup)
                  let minorStart := r.numParams + r.numMotives
                  let extraArgs := if actualArgs.size > majorIdx + 1 then actualArgs.extract (majorIdx + 1) actualArgs.size else #[]
                  let rulesArr := r.rules.toArray
                  let mut minorErr : Option CertificateSupplier := none
                  for (_, nfields, _) in tmpl.rules, idx in [:r.numMinors] do
                    if minorErr.isNone then
                      if let some minorInput := actualArgs[minorStart + idx]? then
                        let minorArity := match rulesArr[idx]? with
                          | some rule => ruleMinorArity r rule
                          | none => nfields
                        match ← withHypotheticalLCtx queryHyp (openMinorFields entry minorInput majorInput extraArgs minorArity 0 queryHyp) with
                        | .error sup => minorErr := some sup
                        | .ok _ => pure ()
                  if let some sup := minorErr then pure (.error sup) else pure (.ok callInput)
          else
            if (← get).mutatedRow == "mut-row14d-rec-underapplied" then
              pure (.ok callInput)
            else if mode == .body then
              completePartialCall entry key actualArgs queryHyp appId callInput false
            else
              recordValue callInput.term (.partialCall key actualArgs queryHyp)
              pure (.ok callInput)
          match recRes with
          | .ok out =>
            let mut combOrigins := #[out.origin]
            for a in actualArgs do
              unless combOrigins.contains a.origin do
                combOrigins := combOrigins.push a.origin
            let outOrigin ← origin (.substitution appId combOrigins)
            pure (.ok (← canonicalizeInput { out with origin := outOrigin }))
          | .error sup =>
            pure (.error (← wrapSupplierOrigin appId (actualArgs.map (·.origin)) sup))
        | .quotInfo qVal =>
          if (← get).mutatedRow != "mut-row12d-induct-quot" then
            for a in actualArgs do
              if let .error sup ← withHypotheticalLCtx queryHyp (completeValue entry a queryHyp) then
                return .error (← wrapSupplierOrigin appId (actualArgs.map (·.origin)) sup)
          if qVal.kind matches .type | .ctor || (← get).mutatedRow == "mut-row12d-induct-quot" then
            pushLocalTransfer callInput
            pure (.ok callInput)
          else
            pushLocalTransfer callInput
            throwError "unresolved-certificate-quotient {key.name}"
        | .inductInfo _ =>
          if (← get).mutatedRow != "mut-row12d-induct-quot" then
            for a in actualArgs do
              if let .error sup ← withHypotheticalLCtx queryHyp (completeValue entry a queryHyp) then
                return .error (← wrapSupplierOrigin appId (actualArgs.map (·.origin)) sup)
          pushLocalTransfer callInput
          pure (.ok callInput)
        | _ =>
          pushLocalTransfer callInput
          pure (.ok callInput)
    match res with
    | .error sup => return .error sup
    | .ok out =>
      if mode == .body then
        if let .error sup ← withHypotheticalLCtx queryHyp (completeValue entry out queryHyp) then
          return .error (← wrapSupplierOrigin appId (actualArgs.map (·.origin)) sup)
      return .ok out
"""

R5_MUTUAL_3 = r"""
partial def evalNodeDirect (entry : CertificateEntry) (id : TermId) (bvars : Array Input)
    (args : Array Input) (hypothetical : Array Input) (mode : Mode) :
    Solver (Except CertificateSupplier Input) := do
  chargeNode 1
  let transfer ← ensureNode id
  match transfer with
  | .unresolved _ reason => throwError reason
  | .demand inh => evalNode entry inh bvars args hypothetical mode
  | .result termId =>
    let raw := (← get).graph.terms[termId]!
    match raw with
    | .bvar i =>
      let some bound := bvars[bvars.size - 1 - i]? | throwError "unbound-certificate-variable"
      if (← get).mutatedRow == "mut-row1-bvar" then
        return .ok dummyInput
      applyValue entry bound args hypothetical mode
    | .fvar _ =>
      let g := (← get).graph
      if let some found := hypothetical.find? (fun inp => g.terms[inp.term]? == some raw) then
        applyValue entry found args hypothetical mode
      else if (← get).mutatedRow == "mut-row2-fvar" then
        let o ← origin (.substitution termId #[])
        let inp ← withHypotheticalLCtx hypothetical (mkInput raw o)
        applyValue entry inp args hypothetical mode
      else
        throwError "unpermitted-certificate-assumption"
    | .lit (.natVal n) =>
      let o ← origin (.substitution termId #[])
      let inp ← withHypotheticalLCtx hypothetical (mkInput raw o)
      if (← get).mutatedRow != "mut-row5a-natlit" then
        if n == 0 then
          recordValue inp.term (.ctor ``Nat.zero 0 #[])
        else
          let predExpr := mkNatLit (n - 1)
          let predInp ← withHypotheticalLCtx hypothetical (mkInput predExpr o)
          recordValue inp.term (.ctor ``Nat.succ 0 #[predInp])
      applyValue entry inp args hypothetical mode
    | .sort _ =>
      if (← get).mutatedRow == "mut-row4-sort" then
        throwError "unresolved-sort"
      let o ← origin (.substitution termId #[])
      let inp ← withHypotheticalLCtx hypothetical (mkInput raw o)
      applyValue entry inp args hypothetical mode
    | .lit _ =>
      if (← get).mutatedRow == "mut-row5b-strlit" then
        throwError "unresolved-strlit"
      let o ← origin (.substitution termId #[])
      let inp ← withHypotheticalLCtx hypothetical (mkInput raw o)
      applyValue entry inp args hypothetical mode
    | .mvar _ =>
      if (← get).mutatedRow == "mut-row3-mvar" then
        return .ok dummyInput
      throwError "uninstantiated-certificate-metavariable"
    | .mdata _ _ =>
      if (← get).mutatedRow == "mut-row6-mdata" then
        return .ok dummyInput
      evalNode entry termId bvars args hypothetical mode
    | _ =>
      if raw == (← get).graph.terms[id]! then
        throwError "unsupported-certificate-expression"
      else
        evalNode entry termId bvars args hypothetical mode
  | _ =>
    evalNodeCompound entry id transfer bvars args hypothetical mode

partial def evalForallRest (entry : CertificateEntry) (curId : TermId) (typeId : TermId) (bodyId : TermId)
    (curBvars : Array Input) (curHyp : Array Input) (isFirst : Bool := true) : Solver (Except CertificateSupplier Unit) := do
  let bodyNode := (← get).graph.terms[bodyId]!
  if (← get).mutatedRow == "mut-row4-sort" && bodyNode.isSort then
    return .ok ()
  match ← evalCachedInvariantNode entry typeId curBvars curHyp with
  | .error sup => return .error sup
  | .ok domInp =>
    let bodyTransfer ← ensureNode bodyId
    if !bodyNode.hasLooseBVar 0 then
      let nextBvars := if bodyNode.hasLooseBVars then curBvars.push dummyInput else #[]
      match bodyTransfer with
      | .forallBinder nextTypeId nextBodyId =>
        chargeNode 1
        evalForallRest entry bodyId nextTypeId nextBodyId nextBvars curHyp isFirst
      | _ =>
        match ← evalCachedInvariantNode entry bodyId nextBvars curHyp with
        | .error sup => return .error sup
        | .ok _ => return .ok ()
    else
      let liveH ← if isFirst then pure curHyp else liveHypForM curId curBvars curHyp
      withCanonicalBinder domInp.term none liveH fun xInput => do
        let nextHyp := pushHypothetical liveH xInput
        let nextBvars := curBvars.push xInput
        match bodyTransfer with
        | .forallBinder nextTypeId nextBodyId =>
          chargeNode 1
          evalForallRest entry bodyId nextTypeId nextBodyId nextBvars nextHyp false
        | _ =>
          match ← evalCachedInvariantNode entry bodyId nextBvars nextHyp with
          | .error sup => return .error sup
          | .ok _ => return .ok ()

partial def evalNode (entry : CertificateEntry) (id : TermId) (bvars : Array Input)
    (args : Array Input) (hypothetical : Array Input) (mode : Mode) :
    Solver (Except CertificateSupplier Input) := do
  if args.isEmpty then
    let transfer ← ensureNode id
    if let .forallBinder .. := transfer then
      return ← evalCachedInvariantNode entry id bvars hypothetical
    let raw := (← get).graph.terms[id]!
    let isClosed := !raw.hasLooseBVars && !raw.hasFVar
    let shouldMemo : Bool := isClosed && (match transfer with
      | .letBind .. | .application .. | .projection .. | .binder .. => true
      | .call _ ins | .branch _ ins | .constructor _ ins => !ins.isEmpty
      | _ => false)
    if shouldMemo then
      let (liveBvars, liveHyp) ← if isClosed then pure (#[], #[]) else projectNodeCtxM id bvars hypothetical
      let nKey : NodeQuery := {
        node := id,
        bvars := liveBvars,
        args := #[],
        entryInputs := entry.query.inputs,
        guards := entry.query.guards,
        hypothetical := liveHyp,
        mode := mode
      }
      if (← get).pendingNodes.contains nKey then
        throwError "cyclic-certificate-dependency"
      if let some dId := (← get).completedNodes[nKey]? then
        let out ← replayCompleted dId
        return .ok out
      return ← runDerivationFrame (.nodeQuery nKey) do
        withHypotheticalLCtx liveHyp (evalNodeDirect entry id liveBvars #[] liveHyp mode)
  evalNodeDirect entry id bvars args hypothetical mode

partial def evalNodeCompound (entry : CertificateEntry) (id : TermId) (transfer : Transfer)
    (bvars : Array Input) (args : Array Input) (hypothetical : Array Input) (mode : Mode) :
    Solver (Except CertificateSupplier Input) := do
  match transfer with
  | .letBind typeId valueId bodyId =>
    match ← evalCachedInvariantNode entry typeId bvars hypothetical with
    | .error sup =>
      return .error (← wrapSupplierOrigin id (bvars.map (·.origin)) sup)
    | .ok _ =>
      let bodyNode := (← get).graph.terms[bodyId]!
      if (← get).mutatedRow == "mut-row7-let" && !bodyNode.hasLooseBVar 0 then
        return ← evalNode entry bodyId (bvars.push dummyInput) args hypothetical mode
      let valMode := if !bodyNode.hasLooseBVar 0 then .body else .closure
      match ← evalNode entry valueId bvars #[] hypothetical valMode with
      | .error sup =>
        return .error (← wrapSupplierOrigin id (bvars.map (·.origin)) sup)
      | .ok valInput =>
        if !bodyNode.hasLooseBVar 0 then
          if let .error sup ← completeValue entry valInput hypothetical then
            return .error (← wrapSupplierOrigin id (bvars.map (·.origin)) sup)
        let boundVal := if bodyNode.hasLooseBVar 0 then valInput else dummyInput
        let res ← evalNode entry bodyId (bvars.push boundVal) args hypothetical mode
        match res with
        | .error sup =>
          return .error (← wrapSupplierOrigin id (bvars.map (·.origin)) sup)
        | .ok bodyOut =>
          if bodyNode.hasLooseBVar 0 && (← get).mutatedRow != "mut-row7-let" then
            if let some (.ctor ..) := (← get).graph.values[valInput.term]? then
              if let .error sup ← completeValue entry valInput hypothetical then
                return .error (← wrapSupplierOrigin id (bvars.map (·.origin)) sup)
          return .ok bodyOut
  | .forallBinder typeId bodyId =>
    let usedBvars := if (← get).graph.terms[id]!.hasLooseBVars then bvars else #[]
    if (← get).mutatedRow == "mut-row9-forall" then
      let rawPi ← instantiateExpr id usedBvars
      let piO ← origin (.substitution id (usedBvars.map (·.origin)))
      let piInp ← withHypotheticalLCtx hypothetical (mkInput rawPi piO)
      return .ok piInp
    match ← evalForallRest entry id typeId bodyId usedBvars hypothetical with
    | .error sup =>
      return .error (← wrapSupplierOrigin id (usedBvars.map (·.origin)) sup)
    | .ok () =>
      let rawPi ← instantiateExpr id usedBvars
      let piO ← origin (.substitution id (usedBvars.map (·.origin)))
      let piInp ← withHypotheticalLCtx hypothetical (mkInput rawPi piO)
      return .ok piInp
  | .binder typeId bodyId =>
    let usedBvars := if (← get).graph.terms[id]!.hasLooseBVars then bvars else #[]
    let domRes : Except CertificateSupplier Input ← if (← get).mutatedRow == "mut-row8a-lam-closure" then
      let dExpr ← instantiateExpr typeId usedBvars
      let dId ← term (cleanSpine dExpr)
      pure (.ok { term := dId, type := 0, origin := 0 })
    else
      evalCachedInvariantNode entry typeId usedBvars hypothetical
    match domRes with
    | .error sup =>
      return .error (← wrapSupplierOrigin id (usedBvars.map (·.origin)) sup)
    | .ok domInp =>
      if !args.isEmpty then
        let arg0 := args[0]!
        let rest := args.extract 1 args.size
        let bodyNode := (← get).graph.terms[bodyId]!
        if !isDummyInput arg0 && !bodyNode.hasLooseBVar 0 && (← get).mutatedRow != "mut-row10-app" then
          if let .error sup ← completeValue entry arg0 hypothetical then
            return .error (← wrapSupplierOrigin id (#[arg0.origin] ++ usedBvars.map (·.origin)) sup)
        let boundArg := if bodyNode.hasLooseBVar 0 then arg0 else dummyInput
        let res ← evalNode entry bodyId (usedBvars.push boundArg) rest hypothetical mode
        match res with
        | .error sup =>
          return .error (← wrapSupplierOrigin id (#[arg0.origin] ++ usedBvars.map (·.origin)) sup)
        | .ok bodyOut =>
          if !isDummyInput arg0 && bodyNode.hasLooseBVar 0 && (← get).mutatedRow != "mut-row10-app" then
            if let some (.ctor ..) := (← get).graph.values[arg0.term]? then
              if let .error sup ← completeValue entry arg0 hypothetical then
                return .error (← wrapSupplierOrigin id (#[arg0.origin] ++ usedBvars.map (·.origin)) sup)
          return .ok bodyOut
      else if mode == .body && (← get).mutatedRow != "mut-row8b-lam-body" then
        let bodyNode := (← get).graph.terms[bodyId]!
        if !bodyNode.hasLooseBVar 0 then
          match ← evalNode entry bodyId (usedBvars.push dummyInput) #[] hypothetical .body with
          | .error sup =>
            return .error (← wrapSupplierOrigin id (usedBvars.map (·.origin)) sup)
          | .ok bodyOut =>
            return .ok bodyOut
        else
          let liveH ← liveHypForM id usedBvars hypothetical
          withCanonicalBinder domInp.term none liveH fun xInput => do
            let nextHyp := pushHypothetical liveH xInput
            match ← evalNode entry bodyId (usedBvars.push xInput) #[] nextHyp .body with
            | .error sup =>
              return .error (← wrapSupplierOrigin id (usedBvars.map (·.origin)) sup)
            | .ok bodyOut =>
              return .ok bodyOut
      else
        let rawLam ← instantiateExpr id usedBvars
        let gNow := (← get).graph
        let rawNode := gNow.terms[id]!
        let mut capOrigins : Array OriginId := #[]
        for h : idx in [:usedBvars.size] do
          if rawNode.hasLooseBVar (usedBvars.size - 1 - idx) then
            let o := usedBvars[idx].origin
            if o != 0 && !capOrigins.contains o then
              capOrigins := capOrigins.push o
        let mut capHyp : Array Input := #[]
        if rawLam.hasFVar && !hypothetical.isEmpty then
          let lamFVars := (collectFVars {} rawLam).fvarSet
          for inp in hypothetical do
            if let .fvar f := gNow.terms[inp.term]! then
              if lamFVars.contains f then
                capHyp := pushHypothetical capHyp inp
                if inp.origin != 0 && !capOrigins.contains inp.origin then
                  capOrigins := capOrigins.push inp.origin
        let lamO ← origin (.substitution id capOrigins)
        let lamInp ← withHypotheticalLCtx capHyp (mkInput rawLam lamO)
        if (← get).mutatedRow != "mut-row8c-lam-closure" then
          recordValue lamInp.term (.closure id usedBvars capHyp)
        return .ok lamInp
  | .application fnId inputIds =>
    let mut evalInputs : Array Input := #[]
    for childId in inputIds do
      match ← evalNode entry childId bvars #[] hypothetical .closure with
      | .error sup =>
        return .error (← wrapSupplierOrigin id (evalInputs.map (·.origin) ++ bvars.map (·.origin)) sup)
      | .ok inp => evalInputs := evalInputs.push inp
    let allArgs := if (← get).mutatedRow == "mut-row16b-application" then args else evalInputs ++ args
    if let .bvar i := (← get).graph.terms[fnId]! then
      let some bound := bvars[bvars.size - 1 - i]? | throwError "unbound-certificate-variable"
      if (← get).mutatedRow == "mut-row1-bvar" then
        return .ok dummyInput
      applyValue entry bound allArgs hypothetical mode
    else
      evalNode entry fnId bvars allArgs hypothetical mode
  | .constructor name inputIds =>
    let mut evalInputs : Array Input := #[]
    let mut argsMask := 0
    for childId in inputIds do
      match ← evalNode entry childId bvars #[] hypothetical .closure with
      | .error sup =>
        return .error (← wrapSupplierOrigin id (evalInputs.map (·.origin) ++ bvars.map (·.origin)) sup)
      | .ok inp =>
        evalInputs := evalInputs.push inp
        let im ← getTermFVarMask inp.term
        argsMask := argsMask ||| im
    let allArgs := evalInputs ++ args
    let key : TemplateKey := { name, levels := ((← get).graph.terms[id]!).getAppFn'.constLevels!, mode := .transfer }
    evalCall entry key allArgs hypothetical mode (if args.isEmpty then some argsMask else none)
  | .branch key inputIds =>
    let mut evalInputs : Array Input := #[]
    let mut argsMask := 0
    for childId in inputIds do
      match ← evalNode entry childId bvars #[] hypothetical .closure with
      | .error sup =>
        return .error (← wrapSupplierOrigin id (evalInputs.map (·.origin) ++ bvars.map (·.origin)) sup)
      | .ok inp =>
        evalInputs := evalInputs.push inp
        let im ← getTermFVarMask inp.term
        argsMask := argsMask ||| im
    let allArgs := evalInputs ++ args
    evalCall entry key allArgs hypothetical mode (if args.isEmpty then some argsMask else none)
  | .call key inputIds =>
    let mut evalInputs : Array Input := #[]
    let mut argsMask := 0
    for childId in inputIds do
      match ← evalNode entry childId bvars #[] hypothetical .closure with
      | .error sup =>
        return .error (← wrapSupplierOrigin id (evalInputs.map (·.origin) ++ bvars.map (·.origin)) sup)
      | .ok inp =>
        evalInputs := evalInputs.push inp
        let im ← getTermFVarMask inp.term
        argsMask := argsMask ||| im
    let allArgs := evalInputs ++ args
    evalCall entry key allArgs hypothetical mode (if args.isEmpty then some argsMask else none)
  | .projection typeName idx receiverId =>
    match ← evalNode entry receiverId bvars #[] hypothetical .closure with
    | .error sup =>
      return .error (← wrapSupplierOrigin id (bvars.map (·.origin)) sup)
    | .ok recvInput =>
      let mut fieldInput : Option Input := none
      if let some (.ctor cName numParams cArgs) := (← get).graph.values[recvInput.term]? then
        let _ := cName
        if (← get).mutatedRow != "mut-row11-proj" then
          for h : cIdx in [:cArgs.size] do
            if cIdx != numParams + idx then
              let otherArg := cArgs[cIdx]
              if !isDummyInput otherArg then
                if let .error sup ← completeValue entry otherArg hypothetical then
                  let fOrig ← if cIdx >= numParams then origin (.field recvInput.origin (cIdx - numParams)) else pure recvInput.origin
                  return .error (← wrapSupplierOrigin id #[fOrig, otherArg.origin] sup)
        if let some fInp := cArgs[numParams + idx]? then
          let fO ← origin (.field recvInput.origin idx)
          let combO ← origin (.substitution fInp.term #[fInp.origin, fO])
          fieldInput := some (← canonicalizeInput { fInp with origin := combO })
      if fieldInput.isNone then
        let recvExpr := (← get).graph.terms[recvInput.term]!
        let projExpr := Expr.proj typeName idx recvExpr
        let redProj ← withHypotheticalLCtx hypothetical (whnfD projExpr)
        let fO ← origin (.field recvInput.origin idx)
        if redProj != projExpr then
          let redId ← term redProj
          match ← evalNode entry redId #[] #[] hypothetical .closure with
          | .error sup =>
            return .error (← wrapSupplierOrigin id #[fO] sup)
          | .ok inp =>
            let combO ← origin (.substitution inp.term #[inp.origin, fO])
            fieldInput := some (← canonicalizeInput { inp with origin := combO })
        else
          if (← get).mutatedRow != "mut-row11-proj" then
            if let .error sup ← completeValue entry recvInput hypothetical then
              return .error (← wrapSupplierOrigin id #[recvInput.origin] sup)
          fieldInput := some (← withHypotheticalLCtx hypothetical (mkInput projExpr fO))
      let fInp := fieldInput.get!
      applyValue entry fInp args hypothetical mode
  | _ => throwError "unexpected-transfer"
end

def validateCertificate (entry : CertificateEntry) (candidate : Certificate) :
    Solver CertificateResult := do
  let base := (← get).worklist.size
  enqueue (.certificate base)
  tryCatchRuntimeEx (do
    let qRoot := { entry.query with entryInputs := entry.query.inputs }
    let qTransferBody : Query := { qRoot with template := { qRoot.template with mode := .transfer }, hypothetical := #[], mode := .body }
    let qTransferClosure : Query := { qRoot with template := { qRoot.template with mode := .transfer }, hypothetical := #[], mode := .closure }
    let qTransferInvariant : Query := { qRoot with template := { qRoot.template with mode := .invariant }, hypothetical := #[], mode := .invariant }
    let initPending : Std.HashSet Query :=
      ({} : Std.HashSet Query).insert entry.query
        |>.insert qRoot
        |>.insert qTransferBody
        |>.insert qTransferClosure
        |>.insert qTransferInvariant
    modify fun s => {
      s with
        worklist := s.worklist.pop,
        pendingQueries := initPending,
        pendingTypes := {},
        pendingValues := {},
        pendingNodes := {},
        verifiedDerivations := {},
        runStartNodeId := s.derivationNodes.size,
        activeChildren := #[],
        activeChildrenSet := {},
        activeFirstTransfer := none,
        activeExclusions := #[],
        transfers := #[],
        structuralExclusions := #[]
    }
    chargeEntry 1
    let graph := (← get).graph
    let some proof := graph.terms[candidate.proof]? | throwError "missing-certificate-proof"
    let some target := graph.terms[entry.target]? | throwError "missing-certificate-target"
    if (proof.hasMVar || target.hasMVar) && (← get).mutatedRow != "mut-row3-mvar" then
      throwError "uninstantiated-certificate-metavariable"
    let mut permitted : Array FVarId := #[]
    for input in candidate.assumptions do
      chargeEntry 1
      let some assumption := (← get).graph.terms[input.term]? | throwError "missing-certificate-assumption"
      let .fvar f := assumption | throwError "nonlocal-certificate-assumption"
      let some (.hypothetical ..) := (← get).graph.origins[input.origin]?
        | throwError "supplied-certificate-assumption"
      unless entry.query.hypothetical.contains input do throwError "unpermitted-certificate-origin"
      let some type := (← get).graph.terms[input.type]? | throwError "missing-certificate-assumption-type"
      if type.hasMVar && (← get).mutatedRow != "mut-row3-mvar" then throwError "uninstantiated-certificate-metavariable"
      Lean.Meta.check type
      unless ← isDefEq (← inferType assumption) type do throwError "certificate-assumption-type"
      permitted := permitted.push f
    for input in candidate.assumptions do
      let type := (← get).graph.terms[input.type]!
      if (← get).mutatedRow != "mut-row2-fvar" then
        for f in (collectFVars {} type).fvarIds do
          unless permitted.contains f do throwError "unpermitted-certificate-assumption"
      if (← get).mutatedRow != "mut-row2a-assumption-type" then
        match ← evalNode entry input.type #[] #[] candidate.assumptions .closure with
        | .error sup => return .rejectedSuppliedContradiction sup
        | .ok tyOut =>
          if let .error sup ← completeValue entry tyOut candidate.assumptions then
            return .rejectedSuppliedContradiction sup
    if (← get).mutatedRow != "mut-row2-fvar" then
      for e in [proof, target] do
        for f in (collectFVars {} e).fvarIds do
          unless permitted.contains f do throwError "unpermitted-certificate-assumption"
    unless candidate.guards == entry.query.guards do throwError "certificate-guard-mismatch"
    if (← get).mutatedRow != "mut-row3-mvar" then
      Lean.Meta.check proof
      Lean.Meta.check target
      unless ← isDefEq (← inferType proof) target do throwError "certificate-type-mismatch"
    match ← evalNode entry candidate.proof #[] #[] candidate.assumptions .body with
    | .error sup => return .rejectedSuppliedContradiction sup
    | .ok proofOut =>
      if let .error sup ← completeValue entry proofOut candidate.assumptions then
        return .rejectedSuppliedContradiction sup
      match ← evalNode entry entry.target #[] #[] candidate.assumptions .body with
      | .error sup => return .rejectedSuppliedContradiction sup
      | .ok targetOut =>
        if let .error sup ← completeValue entry targetOut candidate.assumptions then
          return .rejectedSuppliedContradiction sup
        for guard in candidate.guards do
          chargeEntry 1
          for id in [guard.type, guard.lhs, guard.rhs] do
            if (← get).graph.terms[id]?.isNone then throwError "missing-certificate-guard-term"
            match ← evalNode entry id #[] #[] candidate.assumptions .body with
            | .error sup => return .rejectedSuppliedContradiction sup
            | .ok gOut =>
              if let .error sup ← completeValue entry gOut candidate.assumptions then
                return .rejectedSuppliedContradiction sup
          let g := (← get).graph
          for id in [guard.lhs, guard.rhs] do
            unless ← isDefEq (← inferType g.terms[id]!) g.terms[guard.type]! do
              throwError "certificate-guard-type"
        let rootChildren := (← get).activeChildren
        let (finalTrans, finalExcl) ← collectDerivationEvidence
        let s ← get
        set {
          s with
            certificates := s.certificates + 1,
            pendingQueries := {},
            pendingTypes := {},
            pendingValues := {},
            pendingNodes := {},
            transfers := finalTrans,
            structuralExclusions := finalExcl
        }
        return .admitted {
          entry,
          candidate,
          environment := (← getEnv),
          transfers := finalTrans,
          structuralExclusions := finalExcl,
          rootNodes := rootChildren,
          rank := if finalExcl.isEmpty then 0 else 1
        }
  ) (fun ex => do
    modify fun s => {
      s with
        worklist := s.worklist.extract 0 base,
        pendingQueries := {},
        pendingTypes := {},
        pendingValues := {},
        pendingNodes := {},
        activeChildren := #[],
        activeChildrenSet := {},
        activeFirstTransfer := none,
        activeExclusions := #[],
        transfers := #[],
        structuralExclusions := #[]
    }
    return .incomplete { status := "INCOMPLETE", code := "analysis-budget-or-error", name := entry.instanceName, detail := s!"stage=certificate-dependencies law={entry.law} case={entry.caseName} {← ex.toMessageData.toString}" })

end Discharge
"""

PER_ROW_AND_MUTATION_SUITE = r"""
open Lean Meta Vacuity.Discharge

axiom impossible : False
def PickType : Bool → Prop | true => False | false => True
axiom suppliedByBool (b : Bool) : PickType b

-- Row 1: Expr.bvar
def bvarHonest : Empty → False := fun e => (fun (f : Empty → False) => f e) (fun x => Empty.elim x)
def bvarAppPoison : True := (fun (f : Unit → True) => f ()) (fun _ => False.elim impossible)
def bvarClosurePoison : True := (fun (f : True → False) => True.intro) (fun _ => False.elim impossible)

-- Row 2b: Expr.fvar canonical binder applied as neutral head
def neutralFvarArgPoison : ((Unit → False) → True) → True :=
  fun g => g (fun _ => impossible)

-- Row 4: Expr.sort recognition & dependency poison
def sortRecognitionHonest : @id Prop True := True.intro
def sortTypePoison (b : Bool) : Prop := Bool.casesOn b True (False.elim impossible)
theorem sortDepPoison (x : sortTypePoison true) : False := False.elim impossible

-- Row 5a: Expr.lit (.natVal n)
def natLitHonest : Empty → False := fun e =>
  @Nat.rec (fun _ => Unit → False) (fun _ => False.elim impossible) (fun _ _ _ => Empty.elim e) 1 ()
def natLitPoison : False :=
  @Nat.rec (fun _ => Unit → False) (fun _ => False.elim impossible) (fun _ ih u => ih u) 1 ()

-- Row 5b: Expr.lit (.strVal s) with 2-argument minor (b : ByteArray) (h : b.IsValidUTF8)
def strLitSimpleHonest : True := (fun (_ : String) => True.intro) "hello"
def strLitRecHonest : True :=
  @String.rec (fun _ => True) (fun (b : ByteArray) (_h : b.IsValidUTF8) => True.intro) "hello"
def strLitRecPoison : True :=
  @String.rec (fun _ => True) (fun (b : ByteArray) (_h : b.IsValidUTF8) => False.elim impossible) "hello"
def strLitHonest : Empty → False := fun e =>
  @String.rec (fun _ => Unit → False)
    (fun (b : ByteArray) (_h : b.IsValidUTF8) =>
      ByteArray.casesOn (motive := fun _ => Unit → False) b (fun arr =>
        Array.casesOn (motive := fun _ => Unit → False) arr (fun l =>
          List.casesOn (motive := fun _ => Unit → False) l (fun _ => False.elim impossible) (fun _ _ _ => Empty.elim e))))
    "a" ()
def strLitPoison : False :=
  @String.rec (fun _ => Unit → False)
    (fun (b : ByteArray) (_h : b.IsValidUTF8) =>
      ByteArray.casesOn (motive := fun _ => Unit → False) b (fun arr =>
        Array.casesOn (motive := fun _ => Unit → False) arr (fun l =>
          List.casesOn (motive := fun _ => Unit → False) l (fun _ => False.elim impossible) (fun _ _ _ => False.elim impossible))))
    "a" ()

-- Row 7: Expr.letE
def letDeadHonest : Empty → False := fun e => let _ : Nat := 0; Empty.elim e
def letUsedCtorPoison : True :=
  let _p : PProd True (Unit → False) := @PProd.mk True (Unit → False) True.intro (fun _ => False.elim impossible)
  True.intro

-- Row 8a/8b/8c: Expr.lam
def lamClosureHonest : Empty → False := fun e =>
  let _f : True → True := fun x => x
  Empty.elim e
def lamDomainPoison : True :=
  let _f : (x : Bool.casesOn true (False.elim impossible) True) → True := fun _ => True.intro
  True.intro
def boxedBodyPoison : True :=
  let _box : PProd (Unit → False) Unit := PProd.mk (fun _ => False.elim impossible) ()
  True.intro

-- Row 9a/9b: Expr.forallE & unfolded Pi-type aliases
theorem forallDomainPoison (h : (x : Bool.casesOn true (False.elim impossible) True) → True) : True := True.intro
theorem forallCodomainPoison (h : (x : Empty) → (let _ : False := impossible; x = x)) : True := True.intro
def badDepCodomain (n : Nat) : Type :=
  let _ : Prop := Nat.casesOn (motive := fun _ => Prop) n True (fun _ => let _ : False := impossible; True)
  Unit
abbrev HiddenDepCodomain : Type := (n : Nat) → badDepCodomain n
def depAliasHelper : HiddenDepCodomain := fun _ => ()
def depTypeAliasHonest : Unit := depAliasHelper 0
def depTypeAliasPoison : Unit := depAliasHelper 1

-- Row 10/11: evalDeclaredType & Constant with value
theorem hiddenType : (let _ : False := impossible; True) := True.intro
theorem usesHiddenType : True := hiddenType
abbrev IdFn := False → False
def aliasHelper : IdFn := fun h => h
def appAliasHonest : Empty → False := fun e => aliasHelper (Empty.elim e)
def appAliasPoison : False := aliasHelper (False.elim impossible)

-- Row 13a/13b: ConstantInfo.ctorInfo (saturated & partial)
def ctorParamPoison : (let _ : False := impossible; Unit) × Unit :=
  @Prod.mk (let _ : False := impossible; Unit) Unit () ()
def ctorPartialBodyPoison : Unit → PProd (Unit → False) Unit :=
  @PProd.mk (Unit → False) Unit (fun _ => False.elim impossible)
def ctorPartialConsumerPoison : Unit → PProd (Unit → False) Unit :=
  id (@PProd.mk (Unit → False) Unit (fun _ => False.elim impossible))
def ctorPartialHonest : True → PProd True True := PProd.mk True.intro
def ctorPartialConsumerHonest : True → PProd True True := id (PProd.mk True.intro)

-- Row 14a/14b/14c/14d: RecursorVal
def emptyRecPoisonMajor (e : Empty) : Unit → False :=
  @Empty.rec (fun _ => (Unit → False) → Unit → False) e (fun _ => False.elim impossible)
def recCtorUnusedFieldPoison : Unit → True :=
  @Nat.rec (fun _ => Unit → True) (fun _ => False.elim impossible) (fun _ ih => ih) 1
def symbolicRecMinorPoison (y : Nat) : Unit → True :=
  Nat.casesOn (motive := fun _ => Unit → True) y (fun _ => True.intro) (fun _ _ => False.elim impossible)
def recUnderappliedMinorPoison : ((n : Nat) → (Unit → False) → Unit → False) → Nat → Unit → False :=
  @Nat.rec (fun _ => Unit → False) (fun _ => False.elim impossible)
def recUnderappliedConsumerPoison : Nat → Unit → True :=
  id (@Nat.rec (fun _ => Unit → True) (fun _ => False.elim impossible) (fun _ _ _ => True.intro))
def recUnderappliedMinorLamPoison : Nat → True :=
  @Nat.rec (fun _ => True) True.intro (fun _ _ => False.elim impossible)
def recUnderappliedHonest : Nat → True :=
  @Nat.rec (fun _ => True) True.intro (fun _ _ => True.intro)
def recUnderappliedConsumerHonest : Nat → True :=
  id (@Nat.rec (fun _ => True) True.intro (fun _ _ => True.intro))

-- Row 15/16a: ConstantInfo.quotInfo & inductInfo
def quotMkArgPoison : Quot (fun (_ _ : Unit → False) => True) :=
  @Quot.mk (Unit → False) (fun _ _ => True) (fun _ => False.elim impossible)
def quotMkHonest : Quot (fun (_ _ : Unit) => True) :=
  @Quot.mk Unit (fun _ _ => True) ()
def quotLiftUnresolved (q : Quot (fun (_ _ : Nat) => True)) : Nat :=
  Quot.lift (fun (_ : Nat) => (0 : Nat)) (fun _ _ _ => rfl) q
theorem inductParamPoison (x : @Eq (Unit → False) (fun _ => False.elim impossible) (fun _ => False.elim impossible)) : True := True.intro

-- Row 16b/17: Transfer.application & Expr.proj
def matcherSplitter (b : Bool) : False :=
  match b with
  | true => False.elim impossible
  | false => False.elim impossible
theorem nestedMatchPoison (b : Bool) : False := matcherSplitter b
def projCtorHonest : Empty → False := fun e => (@PProd.mk False True (Empty.elim e) True.intro).1
def projCtorOtherFieldPoison : PProd True (Unit → False) := (@PProd.mk True (Unit → False) True.intro (fun _ => False.elim impossible))

-- Helpers for Section 4.2 same-state retry controls
def leafHelper : True := True.intro
def midHelper : True := leafHelper
def topHelper : True := midHelper
def sharedSymHelper (y : Nat) : True :=
  Nat.casesOn (motive := fun _ => True) y True.intro (fun _ => True.intro)

def runProbe (label : String) (proof : Expr) (expected : String)
    (assumptions : Array Expr := #[]) (mutRow : String := "")
    (targetOverride : Option Expr := none) : MetaM Unit := do
  let action : Solver Unit := do
    modify fun s => { s with mutatedRow := mutRow }
    let mut inputs := #[]
    for h : idx in [:assumptions.size] do
      let x := assumptions[idx]
      let id ← term x
      let type ← term (← inferType x)
      let o ← origin (.hypothetical idx type)
      inputs := inputs.push ({ term := id, type, origin := o } : Input)
    let proofId ← term proof
    let target ← match targetOverride with
      | some t => term t
      | none => if proof.hasMVar then term (mkConst ``False) else term (← inferType proof)
    let query : Query := {
      template := { name := `RowControl.branch, levels := [], mode := .body },
      inputs := #[],
      guards := #[],
      hypothetical := inputs,
      mode := .body
    }
    let entry : CertificateEntry := {
      instanceName := `RowControl.instance,
      law := `RowControl.law,
      caseName := label,
      query,
      target
    }
    let result ← validateCertificate entry { proof := proofId, assumptions := inputs, guards := #[] }
    let actual := match result with
      | .admitted d => s!"admitted rank={d.rank} exclusions={d.structuralExclusions.size}"
      | .rejectedSuppliedContradiction s => s!"rejected-supplied-contradiction supplier={s.declaration}"
      | .incomplete f => s!"{f.status} {f.code} {f.name} {f.detail}"
    let s ← get
    if mutRow == "" then
      IO.println s!"ROW_VEC {label}: total={s.worklistSteps} construct={s.cConstruct} entry={s.cEntry} call={s.cCall} typeInst={s.cTypeInst} node={s.cNode} complete={s.cComplete} replay={s.cReplay} verify={s.cVerify} collect={s.cCollect} proj={s.cProj}"
      IO.println s!"ROW_CTRL {label}: {actual}"
    else
      IO.println s!"MUT_CTRL {mutRow} ({label}): {actual}"
    unless actual == expected do
      throwError "control {label} (mut={mutRow}): expected '{expected}', got '{actual}'"
  let _ ← action.run {}

set_option maxHeartbeats 2000000 in
#eval show MetaM Unit from do
  -- Per-row positive and negative controls (unmutated)
  withLocalDeclD `e (mkConst ``Empty) fun e => do
    runProbe "row1-bvar-honest" (mkApp (mkConst ``bvarHonest) e) "admitted rank=1 exclusions=1" #[e]
  runProbe "row1-bvar-app-poison" (mkConst ``bvarAppPoison) "rejected-supplied-contradiction supplier=impossible"
  runProbe "row1-bvar-poison" (mkConst ``bvarClosurePoison) "rejected-supplied-contradiction supplier=impossible"
  let poisonedAssumpTy : Expr := .letE `u (mkConst ``False) (mkConst ``impossible) (mkConst ``True) false
  withLocalDeclD `hAssump poisonedAssumpTy fun hAssump => do
    runProbe "row2a-assumption-type-poison" (mkConst ``True.intro) "rejected-supplied-contradiction supplier=impossible" #[hAssump]
  runProbe "row2b-neutral-fvar-arg-poison" (mkConst ``neutralFvarArgPoison) "rejected-supplied-contradiction supplier=impossible"
  withLocalDeclD `e (mkConst ``Empty) fun e => do
    let unpermPrf ← mkAppOptM ``Empty.elim #[some (mkConst ``False), some e]
    runProbe "row2-fvar-unpermitted" unpermPrf
      "INCOMPLETE analysis-budget-or-error RowControl.instance stage=certificate-dependencies law=RowControl.law case=row2-fvar-unpermitted unpermitted-certificate-assumption" #[]
  let mvarPrf ← mkFreshExprMVar (mkConst ``False)
  runProbe "row3-mvar-unresolved" mvarPrf
    "INCOMPLETE analysis-budget-or-error RowControl.instance stage=certificate-dependencies law=RowControl.law case=row3-mvar-unresolved uninstantiated-certificate-metavariable"
  runProbe "row4-sort-recognition-honest" (mkConst ``sortRecognitionHonest) "admitted rank=0 exclusions=0"
  runProbe "row4-sort-dep-poison" (mkConst ``sortDepPoison) "rejected-supplied-contradiction supplier=impossible"
  withLocalDeclD `e (mkConst ``Empty) fun e => do
    runProbe "row5a-nat-lit-honest" (mkApp (mkConst ``natLitHonest) e) "admitted rank=1 exclusions=1" #[e]
    runProbe "row5b-str-lit-honest" (mkApp (mkConst ``strLitHonest) e) "admitted rank=1 exclusions=1" #[e]
  runProbe "row5a-nat-lit-poison" (mkConst ``natLitPoison) "rejected-supplied-contradiction supplier=impossible"
  runProbe "row5b-str-lit-simple-honest" (mkConst ``strLitSimpleHonest) "admitted rank=0 exclusions=0"
  runProbe "row5b-str-lit-rec-honest" (mkConst ``strLitRecHonest) "admitted rank=0 exclusions=0"
  runProbe "row5b-str-lit-rec-poison" (mkConst ``strLitRecPoison) "rejected-supplied-contradiction supplier=impossible"
  runProbe "row5b-str-lit-poison" (mkConst ``strLitPoison) "rejected-supplied-contradiction supplier=impossible"
  let mdataPoison := Expr.mdata (KVMap.empty.insert `tag (DataValue.ofString "test")) (mkConst ``impossible)
  runProbe "row6-mdata-poison" mdataPoison "rejected-supplied-contradiction supplier=impossible"
  withLocalDeclD `e (mkConst ``Empty) fun e => do
    let elimPrf ← mkAppOptM ``Empty.elim #[some (mkConst ``False), some e]
    let mdataHonest := Expr.mdata (KVMap.empty.insert `tag (DataValue.ofString "ok")) elimPrf
    runProbe "row6-mdata-honest" mdataHonest "admitted rank=1 exclusions=1" #[e]
    runProbe "row7-let-dead-honest" (mkApp (mkConst ``letDeadHonest) e) "admitted rank=1 exclusions=1" #[e]
  runProbe "row7-let-used-ctor-poison" (mkConst ``letUsedCtorPoison) "rejected-supplied-contradiction supplier=impossible"
  withLocalDeclD `e (mkConst ``Empty) fun e => do
    runProbe "row8a-lam-closure-honest" (mkApp (mkConst ``lamClosureHonest) e) "admitted rank=1 exclusions=1" #[e]
  let cleanTrueFnTy : Expr := .forallE `x (mkConst ``True) (mkConst ``True) .default
  let lamDomainPoisonExpr : Expr := .lam `x poisonedAssumpTy (mkConst ``True.intro) .default
  runProbe "row8a-lam-domain-poison" lamDomainPoisonExpr "rejected-supplied-contradiction supplier=impossible" #[] "" (some cleanTrueFnTy)
  let unappliedLamPoison : Expr := .lam `x (mkConst ``Empty) (mkApp2 (mkConst ``False.elim [.zero]) (mkConst ``False) (mkConst ``impossible)) .default
  runProbe "row8b-lam-body-poison" unappliedLamPoison "rejected-supplied-contradiction supplier=impossible"
  runProbe "row8c-boxed-body-poison" (mkConst ``boxedBodyPoison) "rejected-supplied-contradiction supplier=impossible"
  runProbe "row9-forall-domain-poison" (mkConst ``forallDomainPoison) "rejected-supplied-contradiction supplier=impossible"
  runProbe "row9a-forall-codomain-poison" (mkConst ``forallCodomainPoison) "rejected-supplied-contradiction supplier=impossible"
  runProbe "row9b-dep-type-alias-honest" (mkConst ``depTypeAliasHonest) "admitted rank=0 exclusions=0"
  runProbe "row9b-dep-type-alias-poison" (mkConst ``depTypeAliasPoison) "rejected-supplied-contradiction supplier=impossible"
  runProbe "row10-hidden-decl-type-poison" (mkConst ``usesHiddenType) "rejected-supplied-contradiction supplier=impossible"
  withLocalDeclD `e (mkConst ``Empty) fun e => do
    runProbe "row10-app-alias-honest" (mkApp (mkConst ``appAliasHonest) e) "admitted rank=1 exclusions=1" #[e]
  runProbe "row10-app-alias-poison" (mkConst ``appAliasPoison) "rejected-supplied-contradiction supplier=impossible"
  withLocalDeclD `e (mkConst ``Empty) fun e => do
    runProbe "row11-proj-ctor-honest" (mkApp (mkConst ``projCtorHonest) e) "admitted rank=1 exclusions=1" #[e]
  let projOtherFieldExpr : Expr := Expr.proj ``PProd 0 (mkConst ``projCtorOtherFieldPoison)
  runProbe "row11-proj-other-field-poison" projOtherFieldExpr "rejected-supplied-contradiction supplier=impossible"
  runProbe "row13a-ctor-param-poison" (mkConst ``ctorParamPoison) "rejected-supplied-contradiction supplier=impossible"
  runProbe "row12c-ctor-partial-poison" (mkConst ``ctorPartialBodyPoison) "rejected-supplied-contradiction supplier=impossible"
  runProbe "row13b-ctor-partial-consumer-poison" (mkConst ``ctorPartialConsumerPoison) "rejected-supplied-contradiction supplier=impossible"
  runProbe "row13b-ctor-partial-honest" (mkConst ``ctorPartialHonest) "admitted rank=0 exclusions=0"
  runProbe "row13b-ctor-partial-consumer-honest" (mkConst ``ctorPartialConsumerHonest) "admitted rank=0 exclusions=0"
  runProbe "row12d-induct-param-poison" (mkConst ``inductParamPoison) "rejected-supplied-contradiction supplier=impossible"
  runProbe "row12d-quot-lift-unresolved" (mkConst ``quotLiftUnresolved)
    "INCOMPLETE analysis-budget-or-error RowControl.instance stage=certificate-dependencies law=RowControl.law case=row12d-quot-lift-unresolved unresolved-certificate-quotient Quot.lift"
  runProbe "row15-quot-mk-arg-poison" (mkConst ``quotMkArgPoison) "rejected-supplied-contradiction supplier=impossible"
  runProbe "row15-quot-mk-honest" (mkConst ``quotMkHonest) "admitted rank=0 exclusions=0"
  withLocalDeclD `b (mkConst ``Bool) fun b => do
    runProbe "row13-matcher-poison" (mkApp (mkConst ``nestedMatchPoison) b) "rejected-supplied-contradiction supplier=impossible" #[b]
  withLocalDeclD `e (mkConst ``Empty) fun e => do
    runProbe "row14a-empty-rec-poison" (mkApp (mkConst ``emptyRecPoisonMajor) e) "rejected-supplied-contradiction supplier=impossible" #[e]
  runProbe "row14b-rec-ctor-unused-field-poison" (mkConst ``recCtorUnusedFieldPoison) "rejected-supplied-contradiction supplier=impossible"
  withLocalDeclD `y (mkConst ``Nat) fun y => do
    runProbe "row14c-symbolic-rec-minor-poison" (mkApp (mkConst ``symbolicRecMinorPoison) y) "rejected-supplied-contradiction supplier=impossible" #[y]
  runProbe "row14d-rec-underapplied-poison" (mkConst ``recUnderappliedMinorPoison) "rejected-supplied-contradiction supplier=impossible"
  runProbe "row14d-rec-underapplied-consumer-poison" (mkConst ``recUnderappliedConsumerPoison) "rejected-supplied-contradiction supplier=impossible"
  runProbe "row14d-rec-underapplied-minor-lam-poison" (mkConst ``recUnderappliedMinorLamPoison) "rejected-supplied-contradiction supplier=impossible"
  runProbe "row14d-rec-underapplied-honest" (mkConst ``recUnderappliedHonest) "admitted rank=0 exclusions=0"
  runProbe "row14d-rec-underapplied-consumer-honest" (mkConst ``recUnderappliedConsumerHonest) "admitted rank=0 exclusions=0"
  let fnTy := Expr.forallE `u (mkConst ``Unit) (mkConst ``True) .default
  let letAppPoison : Expr := .app (.letE `n (mkConst ``Nat) (mkNatLit 0) (.lam `f fnTy (.app (.bvar 0) (mkConst ``Unit.unit)) .default) false) (.lam `u (mkConst ``Unit) (mkApp2 (mkConst ``False.elim [.zero]) (mkConst ``True) (mkConst ``impossible)) .default)
  runProbe "row16b-let-app-poison" letAppPoison "rejected-supplied-contradiction supplier=impossible"

  -- Section 4.2 Same-state retry controls
  -- 1. Cross-entry entryInputs separation & pending prerequisite cycle detection
  let prereqRetryAction : Solver Unit := do
    let pTop ← term (mkConst ``topHelper)
    let pMid ← term (mkConst ``midHelper)
    let tTrue ← term (mkConst ``True)
    let qTop : Query := { template := { name := `Retry.top, levels := [], mode := .body }, inputs := #[], guards := #[], hypothetical := #[], mode := .body }
    let eTop : CertificateEntry := { instanceName := `Retry.instance, law := `Retry.law, caseName := "top-populate", query := qTop, target := tTrue }
    let rTop ← validateCertificate eTop { proof := pTop, assumptions := #[], guards := #[] }
    unless (match rTop with | .admitted _ => true | _ => false) do throwError "expected topCert admitted"
    -- Direct pending query cycle
    let qMid : Query := { template := { name := ``midHelper, levels := [], mode := .body }, inputs := #[], guards := #[], hypothetical := #[], mode := .body }
    let eMid : CertificateEntry := { instanceName := `Retry.instance, law := `Retry.law, caseName := "cross-entry-pending-direct-reuse", query := qMid, target := tTrue }
    let rMid ← validateCertificate eMid { proof := pMid, assumptions := #[], guards := #[] }
    let aMid := match rMid with | .incomplete f => f.detail | _ => "unexpected"
    unless aMid == "stage=certificate-dependencies law=Retry.law case=cross-entry-pending-direct-reuse cyclic-certificate-dependency" do
      throwError "expected direct cycle, got {aMid}"
    -- Transitive prerequisite cycle (root is leafHelper, proof calls midHelper which is in completedQueries)
    let qLeaf : Query := { template := { name := ``leafHelper, levels := [], mode := .body }, inputs := #[], guards := #[], hypothetical := #[], mode := .body }
    let eLeaf : CertificateEntry := { instanceName := `Retry.instance, law := `Retry.law, caseName := "cross-entry-pending-prereq-reuse", query := qLeaf, target := tTrue }
    let rLeaf ← validateCertificate eLeaf { proof := pMid, assumptions := #[], guards := #[] }
    let aLeaf := match rLeaf with | .incomplete f => f.detail | _ => "unexpected"
    unless aLeaf == "stage=certificate-dependencies law=Retry.law case=cross-entry-pending-prereq-reuse cyclic-certificate-dependency" do
      throwError "expected transitive prereq cycle, got {aLeaf}"
    let sAfter ← get
    IO.println s!"SAME_STATE cross-entry-pending-prereq-reuse: direct={aMid} -> transitive={aLeaf} (work={sAfter.worklistSteps}, cVerify={sAfter.cVerify})"
  let _ ← prereqRetryAction.run {}

  -- 2. Sticky budget retry & reserved-slice budget-abort-poison-cache-retry in the SAME SolverState
  let retryAction : Solver Unit := do
    modify fun s => { s with remaining := 15 }
    let pHonest ← term (mkConst ``letUsedCtorPoison)
    let tHonest ← term (mkConst ``True)
    let q1 : Query := { template := { name := `Retry.branch1, levels := [], mode := .body }, inputs := #[], guards := #[], hypothetical := #[], mode := .body }
    let e1 : CertificateEntry := { instanceName := `Retry.instance, law := `Retry.law, caseName := "budget-abort-attempt1", query := q1, target := tHonest }
    let r1 ← validateCertificate e1 { proof := pHonest, assumptions := #[], guards := #[] }
    let a1 := match r1 with
      | .incomplete f => s!"{f.status} {f.code}"
      | _ => "unexpected"
    unless a1 == "INCOMPLETE analysis-budget-or-error" do
      throwError "expected budget abort on attempt 1, got {a1}"
    -- Sticky retry with s.remaining = 0
    let eSticky : CertificateEntry := { instanceName := `Retry.instance, law := `Retry.law, caseName := "budget-exhausted-sticky-retry", query := q1, target := tHonest }
    let rSticky ← validateCertificate eSticky { proof := pHonest, assumptions := #[], guards := #[] }
    let aSticky := match rSticky with
      | .incomplete f => s!"{f.status} {f.code}"
      | _ => "unexpected"
    unless aSticky == "INCOMPLETE analysis-budget-or-error" do
      throwError "expected sticky budget abort, got {aSticky}"
    -- Allocate caller's reserved retry slice in the SAME state (without resetting worklistSteps or clearing caches)
    modify fun s => { s with remaining := s.remaining + 200 }
    let q2 : Query := { template := { name := `Retry.branch2, levels := [], mode := .body }, inputs := #[], guards := #[], hypothetical := #[], mode := .body }
    let e2 : CertificateEntry := { instanceName := `Retry.instance, law := `Retry.law, caseName := "budget-abort-poison-cache-retry", query := q2, target := tHonest }
    let r2 ← validateCertificate e2 { proof := pHonest, assumptions := #[], guards := #[] }
    let a2 := match r2 with
      | .rejectedSuppliedContradiction sup => s!"rejected-supplied-contradiction supplier={sup.declaration}"
      | _ => "unexpected"
    unless a2 == "rejected-supplied-contradiction supplier=impossible" do
      throwError "expected poison rejection on same-state retry, got {a2}"
    let sEnd ← get
    IO.println s!"SAME_STATE budget-abort-poison-cache-retry: {a1} -> sticky={aSticky} -> retry={a2} (work={sEnd.worklistSteps}, verified={sEnd.verifiedDerivations.size})"
  let _ ← retryAction.run {}

  -- 3. Dependent type-alias cache separation in the SAME SolverState (dep-type-alias-cache-retry)
  let aliasRetryAction : Solver Unit := do
    let p0 ← term (mkConst ``depTypeAliasHonest)
    let t0 ← term (← inferType (mkConst ``depTypeAliasHonest))
    let q0 : Query := { template := { name := `Retry.alias0, levels := [], mode := .body }, inputs := #[], guards := #[], hypothetical := #[], mode := .body }
    let e0 : CertificateEntry := { instanceName := `Retry.instance, law := `Retry.law, caseName := "dep-type-alias-honest-1", query := q0, target := t0 }
    let r0 ← validateCertificate e0 { proof := p0, assumptions := #[], guards := #[] }
    unless (match r0 with | .admitted _ => true | _ => false) do throwError "expected depTypeAliasHonest admitted"
    let p1 ← term (mkConst ``depTypeAliasPoison)
    let t1 ← term (← inferType (mkConst ``depTypeAliasPoison))
    let q1 : Query := { template := { name := `Retry.alias1, levels := [], mode := .body }, inputs := #[], guards := #[], hypothetical := #[], mode := .body }
    let e1 : CertificateEntry := { instanceName := `Retry.instance, law := `Retry.law, caseName := "dep-type-alias-cache-retry", query := q1, target := t1 }
    let r1 ← validateCertificate e1 { proof := p1, assumptions := #[], guards := #[] }
    let a1 := match r1 with | .rejectedSuppliedContradiction sup => s!"rejected-supplied-contradiction supplier={sup.declaration}" | _ => "unexpected"
    unless a1 == "rejected-supplied-contradiction supplier=impossible" do throwError "expected depTypeAliasPoison rejected in same state, got {a1}"
    let sAlias ← get
    IO.println s!"SAME_STATE dep-type-alias-cache-retry: admitted -> {a1} (work={sAlias.worklistSteps})"
  let _ ← aliasRetryAction.run {}

  -- Single-row mutation executions (verifying each single-row mutation causes fail-open or targeted diagnostic change)
  runProbe "row1-bvar-app-poison" (mkConst ``bvarAppPoison) "admitted rank=0 exclusions=0" #[] "mut-row1-bvar"
  withLocalDeclD `hAssump poisonedAssumpTy fun hAssump => do
    runProbe "row2a-assumption-type-poison" (mkConst ``True.intro) "admitted rank=0 exclusions=0" #[hAssump] "mut-row2a-assumption-type"
  runProbe "row2b-neutral-fvar-arg-poison" (mkConst ``neutralFvarArgPoison) "admitted rank=0 exclusions=0" #[] "mut-row2b-neutral-fvar-arg"
  withLocalDeclD `e (mkConst ``Empty) fun e => do
    let unpermPrf ← mkAppOptM ``Empty.elim #[some (mkConst ``False), some e]
    runProbe "row2-fvar-unpermitted" unpermPrf "admitted rank=0 exclusions=0" #[] "mut-row2-fvar"
  runProbe "row3-mvar-unresolved" mvarPrf "admitted rank=0 exclusions=0" #[] "mut-row3-mvar"
  runProbe "row4-sort-recognition-honest" (mkConst ``sortRecognitionHonest)
    "INCOMPLETE analysis-budget-or-error RowControl.instance stage=certificate-dependencies law=RowControl.law case=row4-sort-recognition-honest unresolved-sort" #[] "mut-row4-sort"
  runProbe "row5a-nat-lit-poison" (mkConst ``natLitPoison) "admitted rank=0 exclusions=0" #[] "mut-row5a-natlit"
  runProbe "row5b-str-lit-simple-honest" (mkConst ``strLitSimpleHonest)
    "INCOMPLETE analysis-budget-or-error RowControl.instance stage=certificate-dependencies law=RowControl.law case=row5b-str-lit-simple-honest unresolved-strlit" #[] "mut-row5b-strlit"
  runProbe "row5b-str-lit-rec-honest" (mkConst ``strLitRecHonest)
    "INCOMPLETE analysis-budget-or-error RowControl.instance stage=certificate-dependencies law=RowControl.law case=row5b-str-lit-rec-honest unresolved-strlit" #[] "mut-row5b-strlit"
  runProbe "row5b-str-lit-poison" (mkConst ``strLitPoison) "admitted rank=0 exclusions=0" #[] "mut-row5b-strlit-open"
  runProbe "row6-mdata-poison" mdataPoison "admitted rank=0 exclusions=0" #[] "mut-row6-mdata"
  runProbe "row7-let-used-ctor-poison" (mkConst ``letUsedCtorPoison) "admitted rank=0 exclusions=0" #[] "mut-row7-let"
  runProbe "row8a-lam-domain-poison" lamDomainPoisonExpr "admitted rank=0 exclusions=0" #[] "mut-row8a-lam-closure" (some cleanTrueFnTy)
  runProbe "row8b-lam-body-poison" unappliedLamPoison "admitted rank=0 exclusions=0" #[] "mut-row8b-lam-body"
  runProbe "row8c-boxed-body-poison" (mkConst ``boxedBodyPoison) "admitted rank=0 exclusions=0" #[] "mut-row8c-lam-closure"
  runProbe "row9-forall-domain-poison" (mkConst ``forallDomainPoison) "admitted rank=0 exclusions=0" #[] "mut-row9-forall"
  runProbe "row9a-forall-codomain-poison" (mkConst ``forallCodomainPoison) "admitted rank=0 exclusions=0" #[] "mut-row9-forall"
  runProbe "row9b-dep-type-alias-poison" (mkConst ``depTypeAliasPoison) "admitted rank=0 exclusions=0" #[] "mut-row9b-type-alias"
  runProbe "row9b-dep-type-alias-honest" (mkConst ``depTypeAliasHonest) "rejected-supplied-contradiction supplier=impossible" #[] "mut-row9b-type-alias-drop"
  runProbe "row10-app-unused-arg-poison" (mkConst ``bvarClosurePoison) "admitted rank=0 exclusions=0" #[] "mut-row10-app"
  runProbe "row11-proj-other-field-poison" projOtherFieldExpr "admitted rank=0 exclusions=0" #[] "mut-row11-proj"
  runProbe "row12a-decltype-poison" (mkConst ``usesHiddenType) "admitted rank=0 exclusions=0" #[] "mut-row12a-decltype"
  runProbe "row11-const-val-poison" (mkConst ``appAliasPoison) "admitted rank=0 exclusions=0" #[] "mut-row11-const-val"
  withLocalDeclD `h (mkConst ``False) fun h => do
    let hypPropext ← mkAppM ``of_eq_true #[← mkAppM ``eq_true #[h]]
    runProbe "row12b-opaque-propext" hypPropext "admitted rank=0 exclusions=0" #[h] "mut-row12b-opaque"
  runProbe "row8c-boxed-body-poison" (mkConst ``boxedBodyPoison) "admitted rank=0 exclusions=0" #[] "mut-row13a-ctor-saturated"
  runProbe "row12c-ctor-partial-poison" (mkConst ``ctorPartialBodyPoison) "admitted rank=0 exclusions=0" #[] "mut-row12c-ctor-partial"
  runProbe "row12d-induct-param-poison" (mkConst ``inductParamPoison) "admitted rank=0 exclusions=0" #[] "mut-row12d-induct-quot"
  runProbe "row12d-quot-lift-unresolved" (mkConst ``quotLiftUnresolved) "admitted rank=0 exclusions=0" #[] "mut-row12d-induct-quot"
  runProbe "row15-quot-mk-arg-poison" (mkConst ``quotMkArgPoison) "admitted rank=0 exclusions=0" #[] "mut-row12d-induct-quot"
  withLocalDeclD `b (mkConst ``Bool) fun b => do
    runProbe "row13-matcher-poison" (mkApp (mkConst ``nestedMatchPoison) b) "admitted rank=0 exclusions=0" #[b] "mut-row13-matcher"
  withLocalDeclD `e (mkConst ``Empty) fun e => do
    runProbe "row14a-empty-rec-poison" (mkApp (mkConst ``emptyRecPoisonMajor) e) "admitted rank=1 exclusions=1" #[e] "mut-row14a-empty-rec"
  runProbe "row14b-rec-ctor-unused-field-poison" (mkConst ``recCtorUnusedFieldPoison) "admitted rank=0 exclusions=0" #[] "mut-row14b-rec-ctor"
  withLocalDeclD `y (mkConst ``Nat) fun y => do
    runProbe "row14c-symbolic-rec-minor-poison" (mkApp (mkConst ``symbolicRecMinorPoison) y) "admitted rank=0 exclusions=0" #[y] "mut-row14c-rec-symbolic"
  runProbe "row14d-rec-underapplied-poison" (mkConst ``recUnderappliedMinorPoison) "admitted rank=0 exclusions=0" #[] "mut-row14d-rec-underapplied"
  runProbe "row14d-rec-underapplied-minor-lam-poison" (mkConst ``recUnderappliedMinorLamPoison) "admitted rank=0 exclusions=0" #[] "mut-row14d-rec-underapplied"
  runProbe "row16b-let-app-poison" letAppPoison "admitted rank=0 exclusions=0" #[] "mut-row16b-application"
"""


def build_consumer_source() -> str:
    src = (HERE / "vacuity.lean").read_text()
    lines = src.splitlines()
    return (
        "\n".join(lines[:409])
        + "\n"
        + R5_DISCHARGE
        + "\n"
        + R5_MUTUAL
        + "\n"
        + R5_MUTUAL_2
        + "\n"
        + R5_MUTUAL_3
        + "\n"
        + "\n".join(lines[1421:])
    )


def main() -> int:
    proto_path = Path(__file__).resolve()
    proto_sha256 = hashlib.sha256(proto_path.read_bytes()).hexdigest()
    print(f"PROTOTYPE_SHA256 {proto_sha256} ({proto_path.relative_to(HERE.parent)})")

    consumer_src = build_consumer_source()
    assert "remaining : Nat := 20000\n" in consumer_src, "Work cap must remain 20,000"

    with compiled_consumer(consumer_text=consumer_src) as meta:
        with tempfile.TemporaryDirectory(prefix="n2-r5-audit-") as temp:
            work = Path(temp)
            env = dict(os.environ, LEAN_PATH=str(meta))

            # Suite 1: All 16 vacuity_certificates.py controls with 10-category vectors
            drv = certs.DRIVER.replace(
                'IO.println s!"COST {label}: work={s.worklistSteps}',
                'IO.println s!"VEC {label}: total={s.worklistSteps} construct={s.cConstruct} entry={s.cEntry} call={s.cCall} typeInst={s.cTypeInst} node={s.cNode} complete={s.cComplete} replay={s.cReplay} verify={s.cVerify} collect={s.cCollect} proj={s.cProj} (b={s.cProjBvars},h={s.cProjHyp},ih={s.cProjInputsHyp},ta={s.cProjTypeArgs})"\n    IO.println s!"COST {label}: work={s.worklistSteps}',
            )
            negative = (HERE / "fixtures/N2-Certificate-Origin-Repro.lean").read_text().replace("import Lean\n", "", 1)
            bad_chain = "\n".join(f"theorem badDependency{i} : False → False := badDependency{i - 1}" for i in range(1, 81))
            chain = "\n".join(f"theorem dependency{i} : False → False := dependency{i - 1}" for i in range(1, 81))
            sources = [
                "set_option linter.defProp false\nset_option linter.unusedVariables false\n" + negative + drv + certs.NEGATIVE.replace("BAD_CHAIN", bad_chain),
                "set_option linter.defProp false\nset_option linter.unusedVariables false\n" + drv + certs.POSITIVE.replace("CHAIN", chain),
            ]
            cert_lines = []
            for idx, source in enumerate(sources):
                path = work / f"CertControl{idx}.lean"
                path.write_text("import N2Meta\n" + source)
                res = command(["lean", path], work, env)
                print(res.stdout, end="", flush=True)
                if res.returncode != 0:
                    return 1
                cert_lines.extend(l for l in res.stdout.splitlines() if l.startswith("CERTIFICATE "))

            # Suite 2: N8a reproducer observations + boundary singletonCertificate
            n8a_drv = n8a.DRIVER.replace(
                'IO.println s!"OBSERVATION {label}: {actual}; work={s.worklistSteps}',
                'IO.println s!"N8A_VEC {label}: total={s.worklistSteps} construct={s.cConstruct} entry={s.cEntry} call={s.cCall} typeInst={s.cTypeInst} node={s.cNode} complete={s.cComplete} replay={s.cReplay} verify={s.cVerify} collect={s.cCollect} proj={s.cProj}"\n    IO.println s!"OBSERVATION {label}: {actual}; work={s.worklistSteps}',
            ) + r"""
def simpProof : MetaM Expr := do
  let goal ← mkFreshExprMVar (mkConst ``False)
  let ctx ← Simp.mkContext { failIfUnchanged := false } (simpTheorems := #[(← getSimpTheorems)])
  let (remaining, _) ← simpAll goal.mvarId! ctx
  unless remaining.isNone do throwError "missing-simp-certificate"
  instantiateMVars goal
set_option maxHeartbeats 2000000 in
#eval show MetaM Unit from do
  withLocalDeclD `p (mkSort .zero) fun p => do
    withLocalDeclD `hp p fun hp => do
      withLocalDeclD `hn (← mkArrow p (mkConst ``False)) fun hn => do
        observe "joint-simp" (← simpProof)
          "INCOMPLETE analysis-budget-or-error Baseline.instance stage=certificate-dependencies law=Baseline.law case=joint-simp unavailable-certificate-transfer propext" #[p, hp, hn]
set_option maxHeartbeats 2000000 in
#eval show MetaM Unit from do
  withLocalDeclD `y (mkConst ``Nat) fun y => do
    let eq ← mkEq y (mkNatLit 0)
    let dec ← mkAppOptM ``decide #[some eq, none]
    let dec ← instantiateMVars dec
    withLocalDeclD `hd (← mkEq dec (mkConst ``Bool.true)) fun hd => do
      withLocalDeclD `hn (← mkArrow eq (mkConst ``False)) fun hn => do
        observe "F3b-simp" (← simpProof)
          "INCOMPLETE analysis-budget-or-error Baseline.instance stage=certificate-dependencies law=Baseline.law case=F3b-simp unavailable-certificate-transfer propext" #[y, hd, hn]
"""
            n8a_path = work / "N8aBaseline.lean"
            n8a_path.write_text("import N2Meta\nset_option linter.defProp false\n" + n8a_drv)
            res_n8a = command(["lean", n8a_path], work, env)
            print(res_n8a.stdout, end="", flush=True)
            if res_n8a.returncode != 0:
                return 1

            bnd_drv = boundary.DRIVER.replace(
                "      IO.println actual",
                '      let s ← get\n      IO.println s!"BND_VEC finite-collection-simp-positive: total={s.worklistSteps} construct={s.cConstruct} entry={s.cEntry} call={s.cCall} typeInst={s.cTypeInst} node={s.cNode} complete={s.cComplete} replay={s.cReplay} verify={s.cVerify} collect={s.cCollect} proj={s.cProj}"\n      IO.println actual',
            )
            bnd_path = work / "Boundary.lean"
            bnd_path.write_text("import N2Meta\n" + bnd_drv)
            res_bnd = command(["lean", bnd_path], work, env)
            print(res_bnd.stdout, end="", flush=True)
            if res_bnd.returncode != 0:
                return 1

            # Suite 3 & 4: Per-row controls + same-state retry + single-row mutations
            row_path = work / "PerRowSuite.lean"
            row_path.write_text(
                "import N2Meta\nset_option linter.defProp false\nset_option linter.unusedVariables false\n"
                + PER_ROW_AND_MUTATION_SUITE
            )
            res_row = command(["lean", row_path], work, env)
            print(res_row.stdout, end="", flush=True)
            if res_row.returncode != 0:
                return 1

    print("ALL ROUND-5 PROTOTYPE SUITES PASSED")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
