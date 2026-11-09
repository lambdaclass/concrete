import Lean

/- N2 consumer, injected into the shared theorems.lean driver by vacuity.sh.
   No meta code or attributes are added to the Init-only specification. -/
open Lean Meta
namespace Vacuity

structure Tagged where
  name : Name
  info : ConstantInfo
  doc : String
  deriving Inhabited

/-- Evidence references are resolved once. Pairing compares declaration identities, not the
    spelling of a qualified, relative or quoted reference in a doc comment. -/
structure Evidence extends Tagged where
  law : Name
  key : String
  subject : Option Name := none
  mutant : Option Name := none

structure Finding where
  status : String
  code : String
  name : Name
  detail : String
  deriving BEq

/-- Mutable accounting survives Meta rollback, but belongs to exactly one analysis. -/
structure AnalysisUsage where
  work : Nat := 0
  projection : Nat := 0
  modules : Std.HashMap (Name × Name) Bool := {}
  constants : Std.HashMap Expr (Array Name) := {}
  mentions : Std.HashMap (Expr × Name) Bool := {}
  mutations : Std.HashMap (Expr × Name × Name × Array Name) Expr := {}
  deriving Inhabited

abbrev WorkBudget := IO.Ref AnalysisUsage

def chargeWork (budget : WorkBudget) (reason : String := "work-budget") : MetaM Unit := do
  let usage ← budget.get
  if usage.work >= 20000 then throwError reason
  budget.set { usage with work := usage.work + 1 }

/-- Optional only for standalone helper probes; production always supplies its job budget. -/
def useWork (budget : Option WorkBudget) (reason : String := "work-budget") : MetaM Unit := do
  if let some budget := budget then chargeWork budget reason

structure AuditState where
  findings : Array Finding := #[]
  budget : Option WorkBudget := none
  lastUsage : AnalysisUsage := {}
  lastHeartbeats : Nat := 0
  sources : Std.HashMap String String := {}
  contexts : Std.HashMap Name (Name × List OpenDecl) := {}
  resolutions : Std.HashMap (Name × String) (Option Name) := {}

abbrev Audit := StateRefT AuditState MetaM

def emit (status code : String) (n : Name) (detail : String := "") : Audit Unit :=
  modify fun state =>
    let f := { status, code, name := n, detail : Finding }
    if code != "analysis-result" && state.findings.contains f then state
    else { state with findings := state.findings.push f }

def fail (code : String) (n : Name) (detail : String := "") : Audit Unit := emit "FAIL" code n detail

def between (s left right : String) : Option String := do
  let tail ← (s.splitOn left)[1]?
  let parts := tail.splitOn right
  if parts.length < 2 then none else some parts.head!

/-- Backticks inside Lean's quoted identifiers are name characters, not doc delimiters. -/
def docNames (s : String) : Array String × Bool := Id.run do
  let mut active := false
  let mut quotedIdent := false
  let mut buffer := ""
  let mut names := #[]
  for c in s.toList do
    if !active then
      if c == '`' then active := true; buffer := ""
    else if c == '`' && !quotedIdent then
      names := names.push buffer
      active := false
    else
      if c == '«' then quotedIdent := true
      if c == '»' then quotedIdent := false
      buffer := buffer.push c
  return (names, active || quotedIdent)

def quoted (s : String) : Array String := (docNames s).1

def fields (doc key : String) : Array String :=
  ((doc.splitOn (key ++ " `")).drop 1).filterMap (fun tail => (quoted ("`" ++ tail))[0]?) |>.toArray

def field (doc key : String) : Option String := (fields doc key)[0]?

def closed (e : Expr) : Bool :=
  !e.hasFVar && !e.hasMVar && !e.hasLooseBVars &&
    !e.hasLevelParam && !e.hasLevelMVar

/-- Recover command-level namespace/open context with Lean's parser, not a prose scan.
    Tags still come exclusively from the shared environment population. -/
def sourceFile (path : String) : Audit String := do
  if let some source := (← get).sources[path]? then return source
  let source ← IO.FS.readFile path
  modify fun state => { state with sources := state.sources.insert path source }
  return source

def sourceOpens (owner : Name) : Audit (Name × List OpenDecl) := do
  if let some cached := (← get).contexts[owner]? then return cached
  let env ← getEnv
  let some ranges ← findDeclarationRanges? owner | throwError "resolution-provenance"
  let path := match env.getModuleIdxFor? owner with
    | some i => (toString env.header.moduleNames[i.toNat]!).replace "." "/" ++ ".lean"
    | none => "ConcreteSpec.lean"
  let input := Parser.mkInputContext (← sourceFile path) path
  let (_, initial, initialMessages) ← Parser.parseHeader input
  let target := input.fileMap.ofPosition ranges.selectionRange.pos
  let mut targets : Array (Nat × Name) := #[]
  for (name, _) in env.constants.map₁.toList ++ env.constants.map₂.toList do
    if env.getModuleIdxFor? name == env.getModuleIdxFor? owner then
      if let some range ← findDeclarationRanges? name then
        targets := targets.push ((input.fileMap.ofPosition range.selectionRange.pos).byteIdx, name)
  targets := targets.qsort (fun a b => a.1 < b.1)
  let mut index := 0
  let mut contexts := (← get).contexts
  let mut state := initial
  let mut messages := initialMessages
  let mut current := Name.anonymous
  let mut opens : List OpenDecl := []
  let mut stack : Array (Name × List OpenDecl) := #[]
  let openNames := fun (ns : Name) (opened : List OpenDecl) (id : Name) =>
    Lean.ResolveName.resolveNamespace env ns opened id
  let addOpens := fun (ns : Name) (opened : List OpenDecl) (parsed : Syntax) => do
    let mut result := opened
    match (⟨parsed⟩ : TSyntax ``Parser.Command.openDecl) with
    | `(Parser.Command.openDecl| $nss*) =>
      for id in nss do
        for resolved in openNames ns result id.getId do
          result := .simple resolved [] :: result
    | `(Parser.Command.openDecl| scoped $nss*) => discard <| pure nss
    | `(Parser.Command.openDecl| $id hiding $ids*) =>
      for resolved in openNames ns result id.getId do
        result := .simple resolved (ids.toList.map (·.getId)) :: result
    | `(Parser.Command.openDecl| $id ($ids*)) =>
      for resolved in openNames ns result id.getId do
        for name in ids do result := .explicit name.getId (resolved ++ name.getId) :: result
    | `(Parser.Command.openDecl| $id renaming $[$froms -> $tos],*) =>
      for resolved in openNames ns result id.getId do
        for (original, alias) in froms.zip tos do
          result := .explicit alias.getId (resolved ++ original.getId) :: result
    | _ => throwError "unsupported-open-context"
    return result
  repeat
    let context : Parser.ParserModuleContext := { env, options := (← getOptions), currNamespace := current, openDecls := opens }
    let (parsed, next, log) := Parser.parseCommand input context state messages
    if log.hasErrors then throwError "resolution-source-parser"
    let ending := parsed.getTailPos?.getD next.pos
    let mut commandOpens := opens
    if parsed.isOfKind ``Parser.Command.«in» then
      let mut nested := parsed
      while nested.isOfKind ``Parser.Command.«in» do
        let leading := nested[0]
        if leading.isOfKind ``Parser.Command.«open» then
          commandOpens ← addOpens current commandOpens leading[1]
        nested := nested[2]
    else if parsed.isOfKind ``Parser.Command.«namespace» then
      stack := stack.push (current, opens)
      let id := parsed[1].getId
      current := if rootNamespace.isPrefixOf id then removeRoot id else current ++ id
    else if parsed.isOfKind ``Parser.Command.«section» then
      stack := stack.push (current, opens)
    else if parsed.isOfKind ``Parser.Command.«end» then
      if let some previous := stack.back? then
        (current, opens) := previous
        stack := stack.pop
    else if parsed.isOfKind ``Parser.Command.«open» then
      opens ← addOpens current opens parsed[1]
    let visibleOpens := if parsed.isOfKind ``Parser.Command.«in» then commandOpens else opens
    while index < targets.size && targets[index]!.1 <= ending.byteIdx do
      contexts := contexts.insert targets[index]!.2 (current, visibleOpens)
      index := index + 1
    if Parser.isTerminalCommand parsed then break
    if next.pos <= state.pos then throwError "resolution-source-progress"
    state := next
    messages := log
  modify fun state => { state with contexts }
  let _ := target
  match contexts[owner]? with
  | some context => return context
  | none => throwError "resolution-context-missing"

def resolveUncached (all : List (Name × ConstantInfo)) (owner : Name) (s : String) : Audit (Option Name) := do
  let .ok stx := Parser.runParserCategory (← getEnv) `term s
    | fail "unknown-name" owner s; return none
  unless stx.isIdent do fail "unknown-name" owner s; return none
  let requested := stx.getId.replacePrefix (Name.mkSimple "_root_") .anonymous
  let env ← getEnv
  let context ← match (← get).contexts[owner]? with
    | some value => pure value
    | none =>
      let value ← sourceOpens owner
      modify fun state => { state with contexts := state.contexts.insert owner value }
      pure value
  let (ns, opens) := context
  let hits := (Lean.ResolveName.resolveGlobalName env (← getOptions) ns opens stx.getId).filter fun (n, projections) =>
    projections.isEmpty && (!isPrivateName n || env.getModuleIdxFor? n == env.getModuleIdxFor? owner)
  match hits.eraseDups with
  | [(n, _)] => return some n
  | [] => pure ()
  | _ => fail "ambiguous-name" owner s; return none
  -- Private source names are intentionally absent from the public resolver index.
  -- Search only private declarations in this module after normal Lean resolution.
  let mut namespacePrefix := if (Name.mkSimple "_root_").isPrefixOf stx.getId then .anonymous else ns
  repeat
    let candidate := namespacePrefix ++ requested
    let hits := all.filter fun (n, _) =>
      isPrivateName n && privateToUserName n == candidate &&
      env.getModuleIdxFor? n == env.getModuleIdxFor? owner
    match hits with
    | [(n, _)] => return some n
    | [] => pure ()
    | _ => fail "ambiguous-name" owner s; return none
    if namespacePrefix.isAnonymous then break
    namespacePrefix := namespacePrefix.getPrefix
  fail "unknown-name" owner s
  return none

def resolve (all : List (Name × ConstantInfo)) (owner : Name) (s : String) : Audit (Option Name) := do
  if let some cached := (← get).resolutions[(owner, s)]? then return cached
  let result ← resolveUncached all owner s
  modify fun state => { state with resolutions := state.resolutions.insert (owner, s) result }
  return result

/-- Provenance: the selection range must spell this declaration, not its generated parent.
    Fresh source compilation emits selection ranges for authored declarations. Generated
    proof/equation auxiliaries have no source selection (or inherit a different name).
    vacuity_driver.py compiles the sources afresh, including their range extension;
    no pre-existing (possibly stripped) environment is accepted. -/
def authored (n : Name) : Audit (Option Bool) := do
  if (← getProjectionFnInfo? n).isSome then return some false
  if (← getMatcherInfo? n).isSome || (← isEqnThm n) then return some false
  let some r ← findDeclarationRanges? n | do
    -- A generated child inherits ownership from its source-declared parent.
    let mut parent := n.getPrefix
    while !parent.isAnonymous do
      let userParent := privateToUserName parent
      if (← getMatcherInfo? parent).isSome || (← getMatcherInfo? userParent).isSome ||
          isAuxRecursor (← getEnv) userParent || ((← getEnv).find? userParent).any (· matches .recInfo _) then return some false
      if (← findDeclarationRangesCore? parent).isSome then return some false
      parent := parent.getPrefix
    return none
  let env ← getEnv
  let path ← match env.getModuleIdxFor? n with
    | some i => pure ((toString env.header.moduleNames[i.toNat]!).replace "." "/" ++ ".lean")
    | none => pure (← getFileName)
  let src ← try sourceFile path catch _ => return none
  let lines := src.splitOn "\n"
  let a := r.selectionRange.pos
  let b := r.selectionRange.endPos
  if a.line != b.line then return none
  let some sourceLine := lines[a.line - 1]? | return none
  let line := sourceLine.toList
  let token := String.ofList ((line.drop a.column).take (b.column - a.column))
  let .ok stx := Parser.runParserCategory env `term token | return some false
  unless stx.isIdent do return some false
  let selected := stx.getId
  let root := Name.mkSimple "_root_"
  let user := privateToUserName n
  return some (if root.isPrefixOf selected then user == selected.replacePrefix root .anonymous
    else selected.isSuffixOf user)

/-- Strip metadata throughout an application spine, preserving all arguments. -/
def cleanSpine : Expr → Expr
  | .mdata _ e => cleanSpine e
  | .app f a => mkApp (cleanSpine f) a
  | e => e

/-- Cache complete answers by elaborated expression, including local identities.
    Every reachable proof field is inspected; sharing does not consume fuel twice. -/
structure ProjectionState where
  budget : Option WorkBudget := none
  remaining : Nat
  answers : Std.HashMap Expr Bool := {}

mutual
/-- Inspect the complete proof DAG under one declared distinct-expression bound.
    Raw delta unfolds one definition per visit; heartbeats also bound meta work. -/
partial def direct (n : Name) (e : Expr) : StateRefT ProjectionState Audit Bool := do
  let e := cleanSpine e
  if let some answer := (← get).answers[e]? then return answer
  let fuel := (← get).remaining
  if fuel == 0 then throwError "projection-budget"
  if let some b := (← get).budget then b.modify fun s => { s with projection := s.projection + 1 }
  modify fun state => { state with remaining := fuel - 1 }
  let answer ← directStep n e
  modify fun state => { state with answers := state.answers.insert e answer }
  return answer

partial def directStep (n : Name) (e : Expr) : StateRefT ProjectionState Audit Bool := do
  if e.getAppFn'.isFVar then return true
  if let .proj _ _ receiver := e.getAppFn' then return ← direct n receiver
  match e with
  | .lam _ _ _ _ => lambdaTelescope e fun _ body => direct n body
  | .letE _ _ v b _ => direct n (b.instantiate1 v)
  | .fvar _ => return true
  | .proj _ _ receiver => direct n receiver
  | _ =>
    if let .const h _ := e.getAppFn' then
      if let some projection ← getProjectionFnInfo? h then
        if let some receiver := e.getAppArgs[projection.numParams]? then
          return ← direct n receiver
        return false
      if [``Eq.refl, ``rfl, ``HEq.refl, ``Iff.refl].contains h then return true
      if let some (.recInfo r) := (← getEnv).find? h then
        if r.numMinors == 1 then
          let args := e.getAppArgs
          let majorIndex := r.numParams + r.numMotives + r.numMinors + r.numIndices
          if let some major := args[majorIndex]? then
            if ← direct n major then
              if let some minor := args[r.numParams + r.numMotives]? then
                if ← direct n minor then return true
      if let some (.ctorInfo c) := (← getEnv).find? h then
        let mut all := true
        for arg in e.getAppArgs[c.numParams:] do
          if ← isProof arg then
            if !(← direct n arg) then all := false
        return all
    -- Raw delta, not whnfR/smart unfolding: exactly one definition per fuel unit.
    if let .const h levels := e.getAppFn' then
      if let some (.defnInfo d) := (← getEnv).find? h then
        let body := d.value.instantiateLevelParams d.levelParams levels
        return ← direct n (mkAppN body e.getAppArgs).headBeta
    let reduced := e.headBeta
    if reduced != e then direct n reduced else return false

end

/-- A checked refutation, never a syntactic guess that a context is impossible. -/
def refutableType (domain : Expr) : MetaM Bool := withoutModifyingState do
  let normalized ← whnfD domain
  if let .const name _ := normalized.getAppFn' then
    if let some (.inductInfo info) := (← getEnv).find? name then
      if info.ctors.isEmpty then return true
  if ← isProp domain then
    try
      let proof ← mkDecideProof (mkApp (mkConst ``Not) domain)
      let proof ← instantiateMVars proof
      if proof.hasMVar then return false
      Lean.Meta.check proof
      return true
    catch ex =>
      if ex.isMaxHeartbeat || ex.isMaxRecDepth || ex.isInterrupt then throw ex
      return false
  return false

/-- Certify an unreachable hypothetical context, including contradictions requiring
    several binders. The certificate is constructed from the local hypotheses,
    independently of the proof being audited, and type checked before use.
    Failure to find a certificate grants no exemption. -/
def refutableContext (simplify : Bool := false) : MetaM Bool := withoutModifyingState do
  let goal ← mkFreshExprMVar (mkConst ``False)
  if !(← goal.mvarId!.contradictionCore { useDecide := false }) then
    if !simplify then return false
    let ctx ← Simp.mkContext { failIfUnchanged := false } (simpTheorems := #[(← getSimpTheorems)])
    let (remaining, _) ← simpAll goal.mvarId! ctx
    if remaining.isSome then return false
  let proof ← instantiateMVars goal
  if proof.hasMVar then return false
  Lean.Meta.check proof
  return true

/- Solver identities are scoped to one immutable environment and one Instance.
    Terms retain bound variables; opening a closure is a separate query mode. -/
namespace Discharge

inductive Mode where
  | closure | body | transfer | invariant
  deriving BEq, Hashable, Inhabited

abbrev TermId := Nat
abbrev OriginId := Nat

/-- Origins form a DAG independently of the term DAG: equal terms can have
    different suppliers. Joint application dependencies are never collapsed. -/
inductive Origin where
  | supplied (instanceName : Name) (argument : Nat) (producer : TermId)
  | hypothetical (scope : Nat) (binder : TermId)
  | field (parent : OriginId) (index : Nat)
  | substitution (callee : TermId) (inputs : Array OriginId)
  deriving BEq, Hashable, Inhabited

structure Input where
  term : TermId
  type : TermId
  origin : OriginId
  deriving BEq, Hashable, Inhabited

structure TemplateKey where
  name : Name
  levels : List Level
  mode : Mode
  deriving BEq, Hashable, Inhabited

/-- A guard is a typed equation, not a Boolean reachability approximation. -/
structure Guard where
  type : TermId
  lhs : TermId
  rhs : TermId
  deriving BEq, Hashable, Inhabited

/-- A certificate retains its permitted assumptions. Substitution must check
    the instantiated proof and its type again before excluding a branch. -/
structure Certificate where
  proof : TermId
  assumptions : Array Input
  guards : Array Guard
  deriving Inhabited

inductive Transfer where
  | result (term : TermId)
  | application (fn : TermId) (inputs : Array TermId)
  | constructor (name : Name) (inputs : Array TermId)
  | projection (name : Name) (field : Nat) (receiver : TermId)
  | binder (type body : TermId)
  | branch (recursor : TemplateKey) (inputs : Array TermId)
  | call (callee : TemplateKey) (inputs : Array TermId)
  | demand (inhabitant : TermId)
  | unresolved (term : TermId) (reason : String)
  deriving Inhabited

structure Template where
  key : TemplateKey
  root : TermId
  transfers : Array Transfer := #[]
  dependencies : Array TemplateKey := #[]
  deriving Inhabited

/-- The whole typed substitution, provenance and guard context identify a
    query. HashMap equality checks structural equality after a hash match. -/
structure Query where
  template : TemplateKey
  inputs : Array Input
  guards : Array Guard
  hypothetical : Array Input
  mode : Mode
  deriving BEq, Hashable, Inhabited

structure Graph where
  terms : Array Expr := #[]
  termIds : Std.HashMap Expr TermId := {}
  origins : Array Origin := #[]
  originIds : Std.HashMap Origin OriginId := {}
  templates : Std.HashMap TemplateKey Template := {}
  nodes : Std.HashMap TermId Transfer := {}
  deriving Inhabited

/-- Interning does not beta-expand or open binders. Expression equality includes
    all universe instantiations and de Bruijn indices. Children are interned on
    demand by graph construction, under the same worklist budget as queries. -/
def internTerm (term : Expr) : StateM Graph TermId := do
  if let some id := (← get).termIds[term]? then return id
  let id := (← get).terms.size
  modify fun g => { g with terms := g.terms.push term, termIds := g.termIds.insert term id }
  return id

def internOrigin (origin : Origin) : StateM Graph OriginId := do
  if let some id := (← get).originIds[origin]? then return id
  let id := (← get).origins.size
  modify fun g => { g with origins := g.origins.push origin, originIds := g.originIds.insert origin id }
  return id

/-- Construction and semantic obligations share one queue and one work counter.
    No call creates a fresh allowance. A state belongs to one Instance. -/
inductive Work where
  | term (term : TermId)
  | template (key : TemplateKey)
  | query (query : Query)
  | certificate (query : Query) (certificate : Certificate)
  | invariant (component : Array TemplateKey)
  deriving Inhabited

structure SolverState where
  budget : Option WorkBudget := none
  graph : Graph := {}
  remaining : Nat := 20000
  worklist : Array Work := #[]
  worklistSteps : Nat := 0
  queryCount : Nat := 0
  componentCount : Nat := 0
  certificates : Nat := 0
  deriving Inhabited

abbrev Solver := StateRefT SolverState MetaM

def charge : Solver Unit := do
  let s ← get
  if let some budget := s.budget then chargeWork budget "false-discharge-budget"
  else if s.remaining == 0 then throwError "false-discharge-budget"
  set { s with remaining := s.remaining - 1, worklistSteps := s.worklistSteps + 1 }

def term (e : Expr) : Solver TermId := do
  let (id, graph) := internTerm e |>.run (← get).graph
  modify fun s => { s with graph }
  return id

def enqueue (work : Work) : Solver Unit :=
  modify fun s => { s with worklist := s.worklist.push work }

/-- Edges retain formal expressions. Constructing a callee does not instantiate
    its body at this call and cannot recursively grow an expression context. -/
def compileNode (id : TermId) : Solver Unit := do
  if (← get).graph.nodes.contains id then return
  let e := (← get).graph.terms[id]!
  let mut children : Array Expr := #[]
  let transfer ← match e with
    | .app .. => do
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
    | .lam _ type body _ | .forallE _ type body _ => do
      children := #[type, body]
      pure (.binder (← term type) (← term body))
    | .letE _ type value body _ => do
      children := #[type, value, body]
      pure (.application (← term (.lam `_ type body .default)) #[← term value])
    | .proj name field receiver => do
      children := #[receiver]
      pure (.projection name field (← term receiver))
    | .mdata _ body => do
      children := #[body]
      pure (.result (← term body))
    | .mvar _ => pure (.unresolved id "uninstantiated-metavariable")
    | _ => pure (.result id)
  modify fun s => { s with graph.nodes := s.graph.nodes.insert id transfer }
  for child in children do
    let childId ← term child
    unless (← get).graph.nodes.contains childId do enqueue (.term childId)

/-- Templates are raw closed declaration bodies at their actual universes.
    Dependencies come from term structure, with no pretty-name or library filter. -/
def compileTemplate (key : TemplateKey) : Solver Unit := do
  if (← get).graph.templates.contains key then return
  let ci ← getConstInfo key.name
  let body := (ci.value? (allowOpaque := true)).getD (mkConst key.name key.levels)
  let root ← term (body.instantiateLevelParams ci.levelParams key.levels)
  let template : Template := { key, root }
  modify fun s => { s with graph.templates := s.graph.templates.insert key template }
  enqueue (.term root)

/-- Identity of the retained consumer, even when dependency nodes are shared. -/
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

/-- No exclusion evidence is assumed while constructing this derivation. All
    recorded transfers have completed; recursive pending lookups are unresolved.
    Later solver slices may add *admitted* prerequisites with strictly lower rank. -/
structure CertificateDerivation where
  entry : CertificateEntry
  candidate : Certificate
  transfers : Array Input
  environment : Environment
  structuralExclusions : Array Input := #[]
  rank : Nat := 0

inductive CertificateResult where
  | admitted (derivation : CertificateDerivation)
  | rejectedSuppliedContradiction (supplier : CertificateSupplier)
  | incomplete (finding : Finding)

structure DependencyState where
  pending : Std.HashSet (Expr × Array FVarId) := {}
  completed : Std.HashSet (Expr × Array FVarId) := {}
  transfers : Array Input := #[]
  hypothetical : Array Input := #[]
  structuralExclusions : Array Input := #[]

abbrev Dependencies := ReaderT CertificateEntry (StateRefT DependencyState Solver)

/-- Drain only construction work appended by this validation; the enclosing
    solver's earlier obligations remain queued. Construction shares charge. -/
def certificateConstruction (base : Nat) : Solver Unit := do
  while (← get).worklist.size > base do
    let work := (← get).worklist.back!
    modify fun s => { s with worklist := s.worklist.pop }
    charge
    match work with
    | .term id => compileNode id
    | .template key => compileTemplate key
    | _ => throwError "unexpected-certificate-construction-work"

/-- Trace the actual application, not an opened declaration with fresh proof
    parameters. This phase never calls refutableContext/simp or consumes a pending
    exclusion, so an impossible producer cannot erase its own prerequisite.
    Types are expressions too, and are checked through this same traversal. -/
partial def certificateDependency (e : Expr) : Dependencies (Option CertificateSupplier) := do
  let e := cleanSpine e
  let key := (e, (collectFVars {} e).fvarIds)
  if (← get).completed.contains key then return none
  if (← get).pending.contains key then throwError "cyclic-certificate-dependency"
  modify fun s => { s with pending := s.pending.insert key }
  liftM (m := Solver) charge
  let id ← liftM (m := Solver) (term e)
  -- Use the slice-1 node identities and construction, charged to the same state.
  let base := (← liftM (m := Solver) get).worklist.size
  liftM (m := Solver) (enqueue (.term id))
  liftM (m := Solver) (certificateConstruction base)
  let finish : Dependencies (Option CertificateSupplier) := do
    modify fun s => { s with pending := s.pending.erase key, completed := s.completed.insert key }
    return none
  if e.hasMVar then throwError "uninstantiated-certificate-metavariable"
  if e.hasLooseBVars then throwError "unbound-certificate-variable"
  -- inferType does not unfold or replace the producing expression.
  unless e.isSort do
    if let some bad ← certificateDependency (← inferType e) then return some bad
  match e with
  | .app .. | .const .. =>
    let fn := e.getAppFn'
    let args := e.getAppArgs
    -- Arguments retain their actual origins, including proof and data arguments.
    for arg in args do
      if let some bad ← certificateDependency arg then return some bad
    if let .const name levels := fn then
      let ci ← getConstInfo name
      -- Preserve domains erased from the application's result type. Earlier
      -- parameters are substituted with actual arguments before later domains
      -- are inspected; supplied proofs never become hypothetical binders.
      let mut declared := ci.type.instantiateLevelParams ci.levelParams levels
      for arg in args do
        unless declared.isForall do
          if let some bad ← certificateDependency declared then return some bad
          declared ← whnfD declared
        let .forallE _ domain body _ := declared
          | throwError "unsupported-certificate-function-type {name}"
        if let some bad ← certificateDependency domain then return some bad
        declared := body.instantiate1 arg
      if let some bad ← certificateDependency declared then return some bad
      let type ← liftM (m := Solver) (term (← inferType e))
      let (origin, graph) := internOrigin (.supplied (← readThe CertificateEntry).instanceName 0 id)
        |>.run (← liftM (m := Solver) get).graph
      liftM (m := Solver) (modify fun s => { s with graph })
      let input : Input := { term := id, type, origin }
      modify fun s => { s with transfers := s.transfers.push input }
      let template : TemplateKey := { name, levels, mode := .transfer }
      let base := (← liftM (m := Solver) get).worklist.size
      liftM (m := Solver) (enqueue (.template template))
      liftM (m := Solver) (certificateConstruction base)
      if let some value := ci.value? (allowOpaque := true) then
        let applied := (mkAppN (value.instantiateLevelParams ci.levelParams levels) args).headBeta
        if let some bad ← certificateDependency applied then return some bad
      else
        match ci with
        | .axiomInfo _ | .opaqueInfo _ =>
          if ← refutableType (← inferType e) then
            return some { declaration := name, input }
          throwError "unavailable-certificate-transfer {name}"
        | .recInfo _ =>
          -- All motives, minors and actual inputs were validated above. The
          -- kernel recursor metadata supplies its structural transfer rule;
          -- reduction may additionally expose an instantiated transfer.
          let reduced ← whnfD e
          if reduced != e then
            if let some bad ← certificateDependency reduced then return some bad
        | .quotInfo _ => throwError "unresolved-certificate-quotient {name}"
        | _ => pure () -- inductive and constructor metadata are structural rules
    else
      let reduced := e.headBeta
      if reduced != e then
        if let some bad ← certificateDependency reduced then return some bad
      else
        if let some bad ← certificateDependency fn then return some bad
  | .lam name type body bi | .forallE name type body bi =>
    if let some bad ← certificateDependency type then return some bad
    let bad ← withLocalDecl name bi type fun x => do
      let id ← liftM (m := Solver) (term x)
      let typeId ← liftM (m := Solver) (term type)
      let (origin, graph) := internOrigin (.hypothetical id id)
        |>.run (← liftM (m := Solver) get).graph
      liftM (m := Solver) (modify fun s => { s with graph })
      let input : Input := { term := id, type := typeId, origin }
      let previous := (← get).hypothetical
      modify fun s => { s with hypothetical := previous.push input }
      -- A direct empty-inductive assumption is a finite structural refutation,
      -- independent of this candidate and of every global proof producer.
      -- No simp/decide-generated exclusion is assumed here.
      let empty ← match type.getAppFn' with
        | .const n _ => pure <| match (← getEnv).find? n with
          | some (.inductInfo info) => info.ctors.isEmpty
          | _ => false
        | _ => pure false
      let bad ← if empty then do
          modify fun s => { s with structuralExclusions := s.structuralExclusions.push input }
          pure none
        else certificateDependency (body.instantiate1 x)
      modify fun s => { s with hypothetical := previous }
      return bad
    if bad.isSome then return bad
  | .letE _ type value body _ =>
    if let some bad ← certificateDependency type then return some bad
    if let some bad ← certificateDependency value then return some bad
    if let some bad ← certificateDependency (body.instantiate1 value) then return some bad
  | .proj _ _ receiver =>
    if let some bad ← certificateDependency receiver then return some bad
  | .mdata _ body =>
    if let some bad ← certificateDependency body then return some bad
  | .fvar _ =>
    let graph := (← liftM (m := Solver) get).graph
    unless (← get).hypothetical.any (fun input => graph.terms[input.term]? == some e) do
      throwError "unpermitted-certificate-assumption"
  | .sort _ | .lit _ => pure ()
  | _ => throwError "unsupported-certificate-expression"
  finish

/-- Validate a candidate without changing any consumer or admitting it into a
    cache. The caller's SolverState supplies the existing 20,000-work allowance;
    this function neither resets it nor starts a new heartbeat deadline. -/
def validateCertificate (entry : CertificateEntry) (candidate : Certificate) :
    Solver CertificateResult := do
  let base := (← get).worklist.size
  enqueue (.certificate entry.query candidate)
  tryCatchRuntimeEx (do
    -- Take this root from the shared worklist without consuming older work.
    modify fun s => { s with worklist := s.worklist.pop }
    charge
    let graph := (← get).graph
    let some proof := graph.terms[candidate.proof]? | throwError "missing-certificate-proof"
    let some target := graph.terms[entry.target]? | throwError "missing-certificate-target"
    if proof.hasMVar || target.hasMVar then throwError "uninstantiated-certificate-metavariable"
    let mut permitted : Array FVarId := #[]
    for input in candidate.assumptions do
      charge
      let some assumption := graph.terms[input.term]? | throwError "missing-certificate-assumption"
      let .fvar f := assumption | throwError "nonlocal-certificate-assumption"
      let some (.hypothetical ..) := graph.origins[input.origin]?
        | throwError "supplied-certificate-assumption"
      unless entry.query.hypothetical.contains input do throwError "unpermitted-certificate-origin"
      let some type := graph.terms[input.type]? | throwError "missing-certificate-assumption-type"
      unless ← isDefEq (← inferType assumption) type do throwError "certificate-assumption-type"
      permitted := permitted.push f
    for e in [proof, target] do
      for f in (collectFVars {} e).fvarIds do
        unless permitted.contains f do throwError "unpermitted-certificate-assumption"
    unless candidate.guards == entry.query.guards do throwError "certificate-guard-mismatch"
    Lean.Meta.check proof
    Lean.Meta.check target
    unless ← isDefEq (← inferType proof) target do throwError "certificate-type-mismatch"
    let action : Dependencies (Option CertificateSupplier) := do
      if let some bad ← certificateDependency proof then return some bad
      if let some bad ← certificateDependency target then return some bad
      for guard in candidate.guards do
        for id in [guard.type, guard.lhs, guard.rhs] do
          let some e := (← liftM (m := Solver) get).graph.terms[id]?
            | throwError "missing-certificate-guard-term"
          if let some bad ← certificateDependency e then return some bad
        let graph := (← liftM (m := Solver) get).graph
        for id in [guard.lhs, guard.rhs] do
          unless ← isDefEq (← inferType graph.terms[id]!) graph.terms[guard.type]! do
            throwError "certificate-guard-type"
      return none
    let (bad, dependencies) ← (action.run entry).run { hypothetical := candidate.assumptions }
    if let some supplier := bad then return .rejectedSuppliedContradiction supplier
    modify fun s => { s with certificates := s.certificates + 1 }
    return .admitted { entry, candidate, environment := (← getEnv), transfers := dependencies.transfers, structuralExclusions := dependencies.structuralExclusions, rank := if dependencies.structuralExclusions.isEmpty then 0 else 1 }
  ) (fun ex => do
    modify fun s => { s with worklist := s.worklist.extract 0 base }
    return .incomplete { status := "INCOMPLETE", code := "analysis-budget-or-error", name := entry.instanceName, detail := s!"stage=certificate-dependencies law={entry.law} case={entry.caseName} {← ex.toMessageData.toString}" })

end Discharge

structure DischargeState where
  budget : Option WorkBudget := none
  remaining : Nat := 20000
  visited : Std.HashSet (Expr × Array FVarId) := {}

/-- Inspect helpers at their actual arguments, including open applications. Only
    a checked refutation of the hypothetical local context can exempt an impossible
    branch. In particular, contradictory supplied arguments remain supplied: they
    are substituted before inspecting the helper, never replaced by fresh binders.
    The shared expression graph and fuel bound dependency traversal. -/
partial def falseDischarge (e : Expr) : StateRefT DischargeState MetaM Bool := do
  let e := cleanSpine e
  -- The same expression in a different hypothetical context is a different
  -- audit state: an unreachable branch cannot mask a later supplied use.
  let key := (e, (← getLCtx).getFVarIds)
  if (← get).visited.contains key then return false
  let fuel := (← get).remaining
  if let some budget := (← get).budget then chargeWork budget "false-discharge-budget"
  else if fuel == 0 then throwError "false-discharge-budget"
  modify fun state => { state with remaining := fuel - 1, visited := state.visited.insert key }
  let refuted ← refutableType (← inferType e)
  if refuted && !e.hasFVar then return !(← refutableContext true)
  let e ← whnfD e
  if let .const n levels := e.getAppFn' then
    if [``False.elim, ``False.casesOn, ``absurd, ``Empty.elim, ``Empty.casesOn,
        ``PEmpty.elim, ``PEmpty.casesOn].contains n && !e.hasFVar then return !(← refutableContext true)
    if let some matcher ← getMatcherInfo? n then
      if matcher.numAlts == 0 && !e.hasFVar then return !(← refutableContext true)
    if let some (.recInfo r) := (← getEnv).find? n then
      if r.rules.isEmpty && !e.hasFVar then return !(← refutableContext true)
    if let some ci := (← getEnv).find? n then
      if let some value := ci.value? (allowOpaque := true) then
        let body := value.instantiateLevelParams ci.levelParams levels
        let applied := (mkAppN body e.getAppArgs).headBeta
        let reduced ← whnfD applied
        if ← falseDischarge reduced then return true
      else if refuted then
        -- An opaque supplied producer of an impossible inhabitant is real
        -- evidence. Transparent derivations are followed to their suppliers;
        -- hypothetical local contradictions are not mistaken for suppliers.
        if let .axiomInfo .. := ci then return !(← refutableContext true)
  match e with
  | .app .. =>
    for arg in e.getAppArgs do
      if (← isProof arg) || (← refutableType (← inferType arg)) then
        if ← falseDischarge arg then return true
    return false
  | .lam .. => lambdaTelescope e fun xs body => do
    for x in xs do
      if ← refutableType (← inferType x) then return false
    if ← refutableContext then return false
    falseDischarge body
  | .letE _ _ v b _ => falseDischarge (b.instantiate1 v)
  | .mdata _ e | .proj _ _ e => falseDischarge e
  | _ => return false

/-- Enumerating constants is itself a traversal, charged before each node. -/
partial def workConstants (e : Expr) (budget : Option WorkBudget) : MetaM (Array Name) := do
  useWork budget
  if let some b := budget then
    if let some cached := (← b.get).constants[e]? then return cached
  let result ← match e with
    | .const n _ => pure #[n]
    | .app f a => pure ((← workConstants f budget) ++ (← workConstants a budget)).toList.eraseDups.toArray
    | .lam _ t b _ | .forallE _ t b _ => pure ((← workConstants t budget) ++ (← workConstants b budget)).toList.eraseDups.toArray
    | .letE _ t v b _ => pure ((← workConstants t budget) ++ (← workConstants v budget) ++ (← workConstants b budget)).toList.eraseDups.toArray
    | .mdata _ b | .proj _ _ b => workConstants b budget
    | _ => pure #[]
  if let some b := budget then b.modify fun s => { s with constants := s.constants.insert e result }
  return result

/-- An imported module cannot refer forward to a subject outside its import DAG.
    This prunes unrelated library bodies using provenance, not a syntactic guess.
    Every inspected module/edge is charged; the cache is private to the analysis. -/
partial def moduleDepends (module target : Name) (budget : Option WorkBudget) : MetaM Bool := do
  useWork budget
  if module == target then return true
  if let some b := budget then
    if let some answer := (← b.get).modules[(module, target)]? then return answer
  let env ← getEnv
  let some idx := env.getModuleIdx? module | return true
  let mut answer := false
  for imported in env.header.moduleData[idx.toNat]!.imports do
    useWork budget
    if ← moduleDepends imported.module target budget then answer := true; break
  if let some b := budget then b.modify fun s => { s with modules := s.modules.insert (module, target) answer }
  return answer

def mayMention (n subject : Name) (budget : Option WorkBudget) : MetaM Bool := do
  useWork budget
  let env ← getEnv
  let some source := env.getModuleIdxFor? n | return true
  let some target := env.getModuleIdxFor? subject | return false
  moduleDepends env.header.moduleNames[source.toNat]! env.header.moduleNames[target.toNat]! budget

partial def mentionsVisit (e : Expr) (subject : Name) (budget : Option WorkBudget := none) : StateRefT (Nat × Std.HashSet Name) MetaM Bool := do
  let (fuel, _) ← get
  if budget.isSome then useWork budget
  else if fuel == 0 then throwError "work-budget"
  modify fun (_, visited) => (fuel - 1, visited)
  let constants ← workConstants e budget
  if constants.contains subject then return true
  for n in constants do
    if (← get).2.contains n then continue
    modify fun (left, visited) => (left, visited.insert n)
    unless ← mayMention n subject budget do continue
    if let some (.defnInfo d) := (← getEnv).find? n then
      if ← mentionsVisit d.value subject budget then return true
  return false

def mentions (e : Expr) (subject : Name) (budget : Option WorkBudget := none) : StateRefT (Nat × Std.HashSet Name) MetaM Bool := do
  useWork budget
  if let some b := budget then
    if let some cached := (← b.get).mentions[(e, subject)]? then return cached
  let result ← mentionsVisit e subject budget
  if let some b := budget then b.modify fun s => { s with mentions := s.mentions.insert (e, subject) result }
  return result

partial def mutate (e : Expr) (subject mutant : Name) (boundaries : Array Name := #[])
    (budget : Option WorkBudget := none) : StateRefT Nat MetaM Expr := do
  let fuel ← get
  if budget.isSome then useWork budget
  else if fuel == 0 then throwError "work-budget"
  modify (· - 1)
  if let some b := budget then
    if let some cached := (← b.get).mutations[(e, subject, mutant, boundaries)]? then return cached
  let result ← go e
  if let some b := budget then b.modify fun s => { s with mutations := s.mutations.insert (e, subject, mutant, boundaries) result }
  return result
where
  go (e : Expr) : StateRefT Nat MetaM Expr := do
   match e with
   | .const n us =>
     if n == subject then return .const mutant us
     if boundaries.contains n then return e
     if let some (.defnInfo d) := (← getEnv).find? n then
       -- Query-local visited set; nested helpers never receive another allowance.
       let (occurs, state) ← (mentions d.value subject budget).run (← get, {})
       set state.1
       if occurs then
         return ← mutate (d.value.instantiateLevelParams d.levelParams us) subject mutant boundaries budget
     return e
   | .app f a => return mkApp (← mutate f subject mutant boundaries budget) (← mutate a subject mutant boundaries budget)
   | .lam n t b bi => return .lam n (← mutate t subject mutant boundaries budget) (← mutate b subject mutant boundaries budget) bi
   | .forallE n t b bi => return .forallE n (← mutate t subject mutant boundaries budget) (← mutate b subject mutant boundaries budget) bi
   | .letE n t v b nd => return .letE n (← mutate t subject mutant boundaries budget) (← mutate v subject mutant boundaries budget) (← mutate b subject mutant boundaries budget) nd
   | .mdata md b => return .mdata md (← mutate b subject mutant boundaries budget)
   | .proj n i b => return .proj n i (← mutate b subject mutant boundaries budget)
   | _ => return e

structure InstanceCase where
  decl : Name
  law : Name
  key : String
  args : Array Expr
  levels : List Level

/-- Consume the complete telescope, exposing definition aliases as necessary. Proof arguments
    are hypotheses; other arguments are parameters. Mutate the statement before substituting
    the closed Instance arguments, preserving their exact values. -/
def counterexample (law : ConstantInfo) (ic : InstanceCase) (s m : Name) (subjects : Array Name) (budget : Option WorkBudget := none) : MetaM Expr := do
  let budget ← match budget with
    | some b => pure b
    | none => IO.mkRef {}
  let mut original := law.type.instantiateLevelParams law.levelParams ic.levels
  let (changed, _) ← (mutate original s m subjects (some budget)).run 0
  let mut mutated := changed
  let mut hyps : Array Expr := #[]
  for arg in ic.args do
    original ← whnfD original
    mutated ← whnfD mutated
    let .forallE _ domain body _ := original | throwError "instance telescope mismatch"
    let .forallE _ domainM bodyM _ := mutated | throwError "mutated telescope mismatch"
    if ← isProp domain then hyps := hyps.push domainM
    original := body.instantiate1 arg
    mutated := bodyM.instantiate1 arg
  let mut result ← mkAppM ``Not #[mutated]
  for h in hyps.reverse do result ← mkAppM ``And #[h, result]
  return result

/-- Expand only dependencies of protectedNames subjects, then abstract the subjects.
    Definitional comparison may reduce contracts, but cannot erase semantic arguments. -/
def withProtected (a b : Expr) (subjects : Array Name) (check : Expr → Expr → MetaM Bool)
    (budget : Option WorkBudget := none) : MetaM Bool := do
  let budget ← match budget with
    | some b => pure b
    | none => IO.mkRef {}
  let mut a := a
  let mut b := b
  for subject in subjects do
    a := (← (mutate a subject subject subjects (some budget)).run 0).1
    b := (← (mutate b subject subject subjects (some budget)).run 0).1
  let collected ← IO.mkRef ({} : Std.HashSet Expr)
  let rec collect (e : Expr) (terms : Array Expr) : MetaM (Array Expr) := do
    chargeWork budget
    if (← collected.get).contains e then return terms
    collected.modify (·.insert e)
    match e with
    | .const n _ => return if subjects.contains n && !terms.contains e then terms.push e else terms
    | .app f a => collect a (← collect f terms)
    | .lam _ t b _ | .forallE _ t b _ => collect b (← collect t terms)
    | .letE _ t v b _ => collect b (← collect v (← collect t terms))
    | .mdata _ b | .proj _ _ b => collect b terms
    | _ => return terms
  let protectedTerms ← collect b (← collect a #[])
  let replacements ← IO.mkRef ({} : Std.HashMap (Expr × Expr × Expr) Expr)
  let rec replace (e term value : Expr) : MetaM Expr := do
    chargeWork budget
    if let some cached := (← replacements.get)[(e, term, value)]? then return cached
    let result ← (do
      if e == term then return value
      match e with
      | .app f a => return mkApp (← replace f term value) (← replace a term value)
      | .lam n t b bi => return .lam n (← replace t term value) (← replace b term value) bi
      | .forallE n t b bi => return .forallE n (← replace t term value) (← replace b term value) bi
      | .letE n t v b nd => return .letE n (← replace t term value) (← replace v term value) (← replace b term value) nd
      | .mdata md b => return .mdata md (← replace b term value)
      | .proj n i b => return .proj n i (← replace b term value)
      | _ => return e)
    replacements.modify (·.insert (e, term, value) result)
    return result
  let rec compare (i : Nat) (a b : Expr) : MetaM Bool := do
    chargeWork budget
    if i >= protectedTerms.size then return ← check a b
    let term := protectedTerms[i]!
    withLocalDeclD `subject (← inferType term) fun subjectVar => do
      compare (i + 1) (← replace a term subjectVar) (← replace b term subjectVar)
  compare 0 a b


def protectedEq (a b : Expr) (subjects : Array Name) (budget : Option WorkBudget := none) : MetaM Bool :=
  -- Smart unfolding intentionally leaves compiled matchers stuck on abstract
  -- discriminants. Protection makes subject applications abstract, so compare
  -- contracts using ordinary definitional unfolding after protection instead.
  withProtected a b subjects (fun x y =>
    withOptions (fun opts => opts.setBool `smartUnfolding false) (isDefEq x y)) budget

/-- Only syntactic binders belong to the declaration telescope. Unfolding a
    closed conclusion may expose quantifiers belonging to its predicate. -/
def closedTail (ty : Expr) (_subjects : Array Name) : MetaM Bool :=
  pure (!(ty.consumeMData matches .forallE ..))

def approval (t : Tagged) : Audit Unit := do
  let some ref := between t.doc "(approved: " ")" | fail "missing-approval" t.name; return
  let reference := ref.trimAscii.toString.toLower
  if ["", "pending", "todo", "tbd"].contains reference then fail "unresolved-approval" t.name ref
  if reference == "self" then fail "self-approved" t.name ref
  let writer := between t.doc "(writer: " ")"
  let reviewer := between t.doc "(reviewer: " ")"
  -- The final carrier requires an independent approval reference. Optional identity
  -- attestations are checked when supplied; their authenticity belongs to review of record.
  match writer, reviewer with
  | some w, some r =>
    let w := w.trimAscii.toString.toLower
    let r := r.trimAscii.toString.toLower
    if w == "" || r == "" then fail "approval-identity-missing" t.name
    else if w == r then fail "self-approved" t.name w
  | none, none => pure ()
  | _, _ => fail "approval-identity-missing" t.name "an identity attestation needs both writer and reviewer"

/-- Private driver trace survives a killed audit process, so a budget failure names
    the exact stage/declaration in flight rather than only `audit.lean`. -/
def progress (stage : String) (name : Name) : MetaM Unit := do
  if let some path ← IO.getEnv "VACUITY_TRACE" then
    IO.FS.withFile path .append fun handle => do
      handle.putStrLn s!"{← IO.monoMsNow}\t{stage}\t{name}"


/-- Frozen analysis identity and action. Dependencies refer only to earlier jobs. -/
structure AnalysisJob where
  name : Name
  stage : String
  law : Option Name := none
  caseName : String := "none"
  extra : String := ""
  dependencies : Array Nat := #[]
  action : Audit Unit := pure ()
  deriving Inhabited

def AnalysisJob.fields (job : AnalysisJob) : String :=
  s!"stage={job.stage} law={job.law.map toString |>.getD "none"} case={job.caseName}"

def AnalysisJob.usage (job : AnalysisJob) (heartbeats : Nat) (usage : AnalysisUsage) : String :=
  s!"heartbeats={heartbeats}/2000000 work={usage.work}/20000 projection={usage.projection}/4096" ++
    (if job.extra == "" then "" else " " ++ job.extra)

/-- Runtime exception isolation restores Meta state, never accounting or earlier findings.
    Only this entry point establishes a fresh deadline; jobs cannot call each other. -/
def isolated (deadline : Core.Context) (job : AnalysisJob) : Audit Bool := do
  let work ← IO.mkRef ({} : AnalysisUsage)
  modify fun s => { s with budget := some work }
  let saved ← liftM (Meta.saveState : MetaM Meta.SavedState)
  let global := (← get).findings
  modify fun s => { s with findings := #[], resolutions := {} }
  let start ← IO.getNumHeartbeats
  tryCatchRuntimeEx
    (withTheReader Core.Context (fun _ => { deadline with initHeartbeats := start }) do
      Core.checkMaxHeartbeats "vacuity-analysis"
      job.action
      Core.checkMaxHeartbeats "vacuity-analysis")
    (fun ex => do
      let message ← ex.toMessageData.toString
      liftM (saved.restore : MetaM Unit)
      let reason := if ex.isMaxHeartbeat then "heartbeats"
        else if message == "false-discharge-budget" then "false-discharge-budget"
        else if message == "work-budget" then "work-budget"
        else if message == "projection-budget" then "projection-budget"
        else "analysis-error"
      emit "INCOMPLETE" "analysis-budget-or-error" job.name
        s!"{job.fields} reason={reason} {job.usage (((← IO.getNumHeartbeats) - start) / 1000) (← work.get)}"
      emit "DETAIL" "analysis-error" job.name s!"{job.fields} {message}")
  let heartbeats := ((← IO.getNumHeartbeats) - start) / 1000
  liftM (saved.restore : MetaM Unit)
  let findings := (← get).findings
  let incomplete := findings.any (·.status == "INCOMPLETE")
  let result := if incomplete then "INCOMPLETE" else if findings.any (·.status == "FAIL") then "FAIL" else "PASS"
  emit "NOTE" "analysis-result" job.name s!"{job.fields} result={result} {job.usage heartbeats (← work.get)}"
  let localFindings := (← get).findings
  modify fun s => { s with findings := global }
  for f in localFindings do emit f.status f.code f.name f.detail
  let usage ← work.get
  modify fun s => { s with budget := none, lastUsage := usage, lastHeartbeats := heartbeats }
  return !incomplete

/-- Account only scheduler work, excluding the complete time spent in analyses. -/
def schedulerPart (deadline : Core.Context) (ticks heartbeats : IO.Ref Nat)
    (action : Audit α) : Audit α := do
  let used ← ticks.get
  if used >= 20000 then throwError "scheduler-work-budget"
  ticks.set (used + 1)
  let start ← IO.getNumHeartbeats
  let spent ← heartbeats.get
  tryCatchRuntimeEx
    (do
      let result ← withTheReader Core.Context (fun _ => { deadline with initHeartbeats := start - spent }) do
        Core.checkMaxHeartbeats "vacuity-scheduler"
        let value ← action
        Core.checkMaxHeartbeats "vacuity-scheduler"
        pure value
      heartbeats.set (spent + (← IO.getNumHeartbeats) - start)
      return result)
    (fun ex => do
      heartbeats.set (spent + (← IO.getNumHeartbeats) - start)
      throw ex)

/-- Reserve the entire allowance before admission, without refunds. Population is
    immutable before the first action, so cap position cannot depend on costs. -/
def runJobs (deadline : Core.Context) (jobs : Array AnalysisJob) (cap : Nat)
    (schedulerTicks schedulerHeartbeats : IO.Ref Nat) : Audit Unit := do
  if cap > jobs.size * 20000 then throwError "invalid-whole-run-cap"
  schedulerPart deadline schedulerTicks schedulerHeartbeats do
    emit "NOTE" "analysis-population" .anonymous s!"N={jobs.size} C={cap} K={cap / 20000}"
  let mut outcomes : Array Bool := #[]
  let mut reserved := 0
  let mut attempted := 0
  let mut completed := 0
  let mut capHit : Option Nat := none
  let mut actualWork := 0
  let mut actualProjection := 0
  let mut actualHeartbeats := 0
  for i in [:jobs.size] do
    let job := jobs[i]!
    let decision ← schedulerPart deadline schedulerTicks schedulerHeartbeats do
      if capHit.isSome || reserved + 20000 > cap then
        emit "INCOMPLETE" "analysis-budget-or-error" job.name
          s!"{job.fields} reason=whole-run-cap {job.usage 0 {}}"
        emit "NOTE" "analysis-result" job.name s!"{job.fields} result=INCOMPLETE {job.usage 0 {}}"
        return 0
      if job.dependencies.any (fun d => d >= outcomes.size || !outcomes[d]!) then
        emit "INCOMPLETE" "analysis-budget-or-error" job.name
          s!"{job.fields} reason=dependency-incomplete {job.usage 0 {}}"
        emit "NOTE" "analysis-result" job.name s!"{job.fields} result=INCOMPLETE {job.usage 0 {}}"
        return 1
      return 2
    if decision == 0 then
      if capHit.isNone then capHit := some i
      outcomes := outcomes.push false
    else
      reserved := reserved + 20000
      attempted := attempted + 1
      if decision == 1 then outcomes := outcomes.push false
      else
        let complete ← isolated deadline job
        outcomes := outcomes.push complete
        if complete then completed := completed + 1
        actualHeartbeats := actualHeartbeats + (← get).lastHeartbeats
        actualWork := actualWork + (← get).lastUsage.work
        actualProjection := actualProjection + (← get).lastUsage.projection
  let unrun := jobs.size - attempted
  let hit := match capHit with
    | none => "none"
    | some i => s!"{i}:{jobs[i]!.name}:{jobs[i]!.stage}"
  schedulerPart deadline schedulerTicks schedulerHeartbeats do
    emit "NOTE" "analysis-summary" .anonymous
      s!"N={jobs.size} C={cap} K={cap / 20000} reserved={reserved} cap-hit={hit} attempted={attempted} unrun={unrun} completed={completed} incomplete={jobs.size - completed} reported={outcomes.size} work={actualWork} projection={actualProjection} heartbeats={actualHeartbeats} heartbeat-bound={cap / 20000 * 2000000} work-bound={cap / 20000 * 20000} projection-bound={cap / 20000 * 4096}"

def enumerateEvidence (all : List (Name × ConstantInfo)) (tags : Array Tagged) : Audit (Array Evidence) := do
  let mut evidence : Array Evidence := #[]
  for t in tags do
    for _ in [()] do
      for tag in ["Instance (", "Planted ("] do
        for part in (t.doc.splitOn tag).drop 1 do
          let errorCode := if tag == "Planted (" then "planted-tag" else "evidence-tag"
          let some payload := between (tag ++ part) tag ")." | fail errorCode t.name; continue
          let doc := tag ++ payload ++ ")."
          let keys := if tag == "Planted (" then ["law", "case", "subject", "mutant"] else ["law", "case"]
          unless keys.all (fun key => (fields doc key).size == 1) do
            fail errorCode t.name; continue
          let mut residue := payload
          for key in keys do
            residue := residue.replace (key ++ " `" ++ (field doc key).getD "" ++ "`") ""
          unless residue.toList.all (fun c => c.isWhitespace || c == ',') &&
            (residue.splitOn ",").length == keys.length do
            fail errorCode t.name; continue
          let some lt := field doc "law" | fail "evidence-tag" t.name; continue
          let some law ← resolve all t.name lt | continue
          let some key := field doc "case" | fail "evidence-tag" t.name; continue
          let mut subject := none
          let mut mutant := none
          if tag == "Planted (" then
            let some st := field doc "subject" | fail "planted-tag" t.name; continue
            let some sn ← resolve all t.name st | continue
            let some mt := field doc "mutant" | fail "planted-tag" t.name; continue
            let some mn ← resolve all t.name mt | continue
            subject := some sn
            mutant := some mn
          if evidence.any (fun e => e.name == t.name && e.law == law && e.key == key && e.subject == subject && e.mutant == mutant) then
            if tag == "Instance (" then fail "duplicate-instance" t.name
            continue
          evidence := evidence.push { toTagged := { t with doc := doc }, law, key, subject, mutant }
  return evidence

def checkInstance (env : Environment) (all : List (Name × ConstantInfo))
    (tags : Array Tagged) (t : Evidence) (existing : Array InstanceCase) : Audit (Array InstanceCase) := do
  let mut instances := existing
  for _ in [()] do
    progress "instance" t.name
    let law := t.law
    let key := t.key
    let some value := t.info.value? | fail "instance-application" t.name s!"law={law} case={key}"; continue
    let value := cleanSpine value
    let .const head levels := value.getAppFn' | fail "instance-application" t.name s!"law={law} case={key}"; continue
    unless head == law && closed value && closed t.info.type && t.info.levelParams.isEmpty do
      fail "instance-application" t.name s!"law={law} case={key}"; continue
    let args := value.getAppArgs
    let lawInfo := (env.find? law).get!
    let mut ty := lawInfo.type.instantiateLevelParams lawInfo.levelParams levels
    let mut valid := true
    let mut falseHyp := false
    let work := (← get).budget
    let mut discharge : DischargeState := { budget := work }
    for arg in args do
      ty := ty.consumeMData
      if let .forallE _ domain body _ := ty then
        if ← isProp domain then
          let (bad, next) ← (falseDischarge arg).run discharge
          discharge := next
          falseHyp := falseHyp || bad
        ty := body.instantiate1 arg
      else valid := false
    let mut protectedNames := #[]
    if let some l := tags.find? (·.name == law) then
      for st in quoted ((between l.doc "Law (subject " ").").getD "") do
        if let some n ← resolve all l.name st then protectedNames := protectedNames.push n
    unless valid && (← closedTail ty protectedNames) && (← protectedEq ty t.info.type protectedNames (← get).budget) do fail "instance-application" t.name s!"law={law} case={key}"; continue
    if falseHyp then
      fail "instance-false-discharge" t.name
      continue
    if instances.any (fun i => i.law == law && i.key == key) then fail "duplicate-instance" t.name
    instances := instances.push ⟨t.name, law, key, args, levels⟩
  return instances

def check (population : List (Name × ConstantInfo)) : MetaM (Array Finding) := do
  let deadline ← readThe Core.Context
  let run : Audit Unit := do
    let schedulerStart ← IO.getNumHeartbeats
    let schedulerWork ← IO.mkRef (0 : Nat)
    let step : Audit Unit := do
      Core.checkMaxHeartbeats "vacuity-scheduler"
      let used ← schedulerWork.get
      if used >= 20000 then throwError "scheduler-work-budget"
      schedulerWork.set (used + 1)
    let build : Audit (Array AnalysisJob) := do
      let env ← getEnv
      let all := env.constants.map₁.toList ++ env.constants.map₂.toList
      let scope := (← IO.getEnv "VACUITY_SCOPE").getD ""
      let scopes := scope.splitOn ","
      let budget := min 4096 (((← IO.getEnv "VACUITY_BUDGET").getD "4096").toNat?.getD 4096)
      let roadmap ← IO.FS.readFile "ROADMAP.md"
      let mut jobs : Array AnalysisJob := #[]
      let provenance ← IO.mkRef (false, ({} : Std.HashSet String))
      let validInstances ← IO.mkRef (#[] : Array InstanceCase)
      let mut classifications : Std.HashMap Name Nat := {}
      let mut tagJobs : Std.HashMap Name Nat := {}
      let mut instanceJobs : Std.HashMap Name Nat := {}
      let mut tags : Array Tagged := #[]
      let ordered := population.toArray.qsort (fun a b => toString a.1 < toString b.1)
      for (n, ci) in ordered do
        step
        let module := (env.getModuleIdxFor? n).map fun i => toString env.header.moduleNames[i.toNat]!
        if scope != "" && !module.any scopes.contains && !(toString n).startsWith "ConcreteSpecControl." then continue
        let doc := (← findDocString? env n).getD ""
        let annotated := ["Law (subject ", "Helper (", "Representation lemma.", "Obligation (",
          "Instance (", "Planted (", "Exemption", "Exempt:", "Manual-audit", "Manual audit", "manual-audit"].any
          fun tag => (doc.splitOn tag).length > 1
        -- Provenance and doc parsing do not reduce or inspect proof terms.
        if ci.isTheorem || annotated then
          let origin ← authored n
          unless origin == some false && !(doc.splitOn "Obligation (").length > 1 do
            tags := tags.push ⟨n, ci, doc⟩
        classifications := classifications.insert n jobs.size
        jobs := jobs.push {
          name := n, stage := "classification", action := do
            let mut tags : Array Tagged := #[]
            let mut hasAuthored := (← provenance.get).1
            let mut authoredModules := (← provenance.get).2
            for _ in [()] do
              progress "classification" n
              let doc := (← findDocString? env n).getD ""
              let isExample := ((toString n).splitOn "⟪example⟫").length > 1
              if ci.isTheorem || (!isExample && (ci.value? (allowOpaque := true)).isSome && (← isProof (mkConst n (ci.levelParams.map Level.param)))) then
                match ← authored n with
                | none => emit "INCOMPLETE" "missing-provenance" n
                | some false =>
                  unless (doc.splitOn "Obligation (").length > 1 do continue
                | some true =>
                  hasAuthored := true
                  if let some m := module then authoredModules := authoredModules.insert m
                  let classes := ["Law (subject ", "Helper (", "Representation lemma.", "Obligation ("]
                  let count := classes.foldl (fun k c => k + (doc.splitOn c).length - 1) 0
                  if count != 1 then fail "classification-count" n (toString count)
              let annotated := ["Law (subject ", "Helper (", "Representation lemma.", "Obligation (",
                "Instance (", "Planted (", "Exemption", "Exempt:", "Manual-audit", "Manual audit", "manual-audit"].any
                fun tag => (doc.splitOn tag).length > 1
              if !ci.isTheorem && !annotated then continue
              if annotated then
                hasAuthored := true
                if let some m := module then authoredModules := authoredModules.insert m
              if !ci.isTheorem && !(← isProof (mkConst n (ci.levelParams.map Level.param))) && ((doc.splitOn "Helper (").length > 1 || (doc.splitOn "Representation lemma.").length > 1) then
                fail "classification-kind" n
              tags := tags.push ⟨n, ci, doc⟩
              if (docNames doc).2 then fail "malformed-backticks" n
              -- Cases are labels, not declarations. Every other backtick resolves, including prose.
              for q in quoted doc do
                if (fields doc "case").contains q then continue
                discard <| resolve all n q
              if (doc.splitOn "Helper (").length > 1 then
                let category := (between doc "Helper (" ")").getD ""
                unless ["inversion", "projection", "congruence", "arithmetic", "computation"].contains category do
                  fail "helper-kind" n category
                let reason := ((between doc ("Helper (" ++ category ++ ").") "(approved:").getD "").trimAscii.toString
                if reason == "" then fail "classification-reason" n
              if (doc.splitOn "Representation lemma.").length > 1 then
                let reason := ((between doc "Representation lemma." "(approved:").getD "").trimAscii.toString
                if reason == "" then fail "classification-reason" n
              for label in ["Helper (", "Representation lemma.", "Exemption", "Exempt:", "Manual-audit", "Manual audit", "manual-audit"] do
                if (doc.splitOn label).length > 1 then
                  emit "REPORT" label n doc
                  approval ⟨n, ci, doc⟩
              if (doc.splitOn "Obligation (").length > 1 then
                emit "REPORT" "Obligation" n doc
                let propDef ← match ci with
                  | .defnInfo _ => forallTelescopeReducing ci.type fun _ t => isDefEq t (mkSort .zero)
                  | _ => pure false
                let projection := (← getProjectionFnInfo? n).isSome
                unless propDef || (projection && (← forallTelescopeReducing ci.type fun _ t => do
                  return (← isProp t) || (← isDefEq t (mkSort .zero)))) do
                  fail "obligation-kind" n
                let some header := between doc "Obligation (discharged at slice " ")." | fail "obligation-format" n; continue
                let parts := header.splitOn ", R-"
                unless parts.length == 2 && parts[0]!.toNat?.isSome && parts[1]!.length == 4 && parts[1]!.toNat?.isSome do
                  fail "obligation-format" n; continue
                let row := parts[1]!
                unless (roadmap.splitOn ("| R-" ++ row ++ " |")).length > 1 do fail "obligation-roadmap" n ("R-" ++ row)
            provenance.set (hasAuthored, authoredModules)
        }
      let classificationDeps := (jobs.mapIdx fun i _ => i)
      jobs := jobs.push {
        name := .anonymous, stage := "scope", dependencies := classificationDeps, action := do
          let (hasAuthored, authoredModules) ← provenance.get
          unless hasAuthored do fail "empty-authored-scope" .anonymous scope
          if scope != "" then
            for m in scopes do
              unless authoredModules.contains m do fail "empty-authored-scope" .anonymous m
      }
      -- Freeze evidence identities before admitting any solver job. Parser findings
      -- are replayed in their own evidence-tags result, not lost during planning.
      let mut evidence : Array Evidence := #[]
      for t in tags do
        step
        let (parsed, parserState) ← (enumerateEvidence all #[t]).run { (← get) with findings := #[], resolutions := {} }
        let findings := parserState.findings
        evidence := evidence ++ parsed
        tagJobs := tagJobs.insert t.name jobs.size
        jobs := jobs.push {
          name := t.name, stage := "evidence-tags",
          dependencies := #[classifications[t.name]!], action := do
            for f in findings do emit f.status f.code f.name f.detail
        }
      for t in evidence do
        step
        jobs := jobs.push {
          name := t.name, stage := "evidence-kind", law := some t.law, caseName := t.key,
          extra := match t.subject with | none => "" | some subject => s!"subject={subject} mutant={t.mutant.map toString |>.getD "none"}",
          dependencies := #[tagJobs[t.name]!], action := do
            for _ in [()] do
              unless ((toString t.name).splitOn "⟪example⟫").length > 1 do
                fail "evidence-not-example" t.name
              let target := t.law
              let lt := toString target
              let candidate := tags.find? (·.name == target)
              match candidate with
              | some l =>
                unless l.info.isTheorem && ((l.doc.splitOn "Law (subject ").length > 1 ) do
                  fail "evidence-law-kind" t.name lt
              | none => fail "evidence-law-kind" t.name lt
              if (t.doc.splitOn "Planted (").length > 1 then
                let subjects := tags.filter fun l => l.name == target && (l.doc.splitOn "Law (subject ").length > 1
                if subjects.isEmpty then continue
                let mut declared : Array Name := #[]
                for l in subjects do
                  for st in quoted ((between l.doc "Law (subject " ").").getD "") do
                    if let some subject ← resolve all l.name st then declared := declared.push subject
                unless t.subject.any declared.contains do
                  fail "planted-subject" t.name ((field t.doc "subject").getD "")
        }
      for t in evidence do
        if !(t.doc.splitOn "Instance (").length > 1 then continue
        step
        instanceJobs := instanceJobs.insert t.name jobs.size
        jobs := jobs.push {
          name := t.name, stage := "instance", law := some t.law, caseName := t.key,
          dependencies := #[tagJobs[t.name]!], action := do
            let instances ← checkInstance env all tags t (← validInstances.get)
            validInstances.set instances
        }
      for p in evidence do
        if !(p.doc.splitOn "Planted (").length > 1 then continue
        step
        let deps := (evidence.filter fun i => i.law == p.law && i.key == p.key && instanceJobs.contains i.name).map
          fun i => instanceJobs[i.name]!
        jobs := jobs.push {
          name := p.name, stage := "pairing", law := some p.law, caseName := p.key,
          extra := s!"subject={p.subject.map toString |>.getD "none"} mutant={p.mutant.map toString |>.getD "none"}",
          dependencies := deps.push tagJobs[p.name]!, action := do
            let instances ← validInstances.get
            for _ in [()] do
              if (p.doc.splitOn "Planted (").length > 1 then
                unless instances.any (fun i => p.law == i.law && p.key == i.key) do
                  fail "unpaired-planted" p.name
                  if evidence.any (fun i => i.law == p.law && i.key == p.key &&
                      (i.doc.splitOn "Instance (").length > 1) then
                    fail "planted-proposition" p.name s!"law={p.law} case={p.key} no-valid-closed-instance"
        }
      for law in tags do
        unless (law.doc.splitOn "Law (subject ").length > 1 do continue
        step
        let deps := (evidence.filter fun i => i.law == law.name && instanceJobs.contains i.name).map
          fun i => instanceJobs[i.name]!
        jobs := jobs.push {
          name := law.name, stage := "law", law := some law.name,
          dependencies := deps.push classifications[law.name]!, action := do
            for _ in [()] do
              progress "law" law.name
              unless law.info.isTheorem do fail "law-kind" law.name; continue
              let subjects := quoted ((between law.doc "Law (subject " ").").getD "")
              if subjects.isEmpty then fail "law-subject" law.name
              let cases := (← validInstances.get).filter (·.law == law.name)
              if cases.isEmpty then fail "missing-instance" law.name
              for st in subjects do
                let some subject ← resolve all law.name st | continue
                unless ((env.find? subject).get! matches .defnInfo _) do fail "subject-kind" law.name st; continue
                let (occurs, _) ← (mentions law.info.type subject (← get).budget).run (0, {})
                unless occurs do fail "subject-absent" law.name st
                for ic in cases do
                  let planted := evidence.filter fun t => t.law == law.name &&
                    t.key == ic.key && t.subject == some subject &&
                    (t.doc.splitOn "Planted (").length > 1
                  if planted.isEmpty then fail "missing-planted" law.name (ic.key ++ "/" ++ st)
                  discard <| pure planted
        }
        jobs := jobs.push {
          name := law.name, stage := "projection", law := some law.name,
          dependencies := #[classifications[law.name]!], action := do
            if law.info.isTheorem then
              let trivialStatement ← forallTelescopeReducing law.info.type fun xs conclusion => do
                for x in xs do
                  if ← isProof x then
                    if ← isDefEq (← inferType x) conclusion then return true
                let args := conclusion.getAppArgs
                if conclusion.isAppOf ``Eq && args.size == 3 then
                  return ← isDefEq args[1]! args[2]!
                if conclusion.isAppOf ``Iff && args.size == 2 then
                  return ← isDefEq args[0]! args[1]!
                return false
              if trivialStatement then fail "projection-law" law.name
              if let some proof := law.info.value? (allowOpaque := true) then
                let (projection, cost) ← (direct law.name proof).run { remaining := budget, budget := (← get).budget }
                emit "NOTE" "projection-cost" law.name s!"visited={budget - cost.remaining} limit={budget} cached={cost.answers.size}"
                if projection then fail "projection-law" law.name
        }
        let enumerationFindings := (← get).findings
        let enumerationResolutions := (← get).resolutions
        let mut declaredSubjects : Array Name := #[]
        for st in quoted ((between law.doc "Law (subject " ").").getD "") do
          if let some subject ← resolve all law.name st then declaredSubjects := declaredSubjects.push subject
        modify fun s => { s with findings := enumerationFindings, resolutions := enumerationResolutions }
        -- Each syntactically paired Planted has a frozen identity, even if its
        -- Instance later proves invalid. A completed invalid Instance is known
        -- negative data; an unfinished one is a dependency INCOMPLETE.
        for p in evidence do
          unless law.info.isTheorem && p.law == law.name &&
            (p.doc.splitOn "Planted (").length > 1 && p.subject.any declaredSubjects.contains do continue
          let subject := p.subject.getD .anonymous
          unless (env.find? subject).any (fun ci => ci matches .defnInfo _) do continue
          for t in evidence do
            unless t.law == law.name && t.key == p.key && instanceJobs.contains t.name do continue
            step
            let key := p.key
            jobs := jobs.push {
              name := p.name, stage := "planted", law := some law.name, caseName := key,
              extra := s!"instance={t.name} subject={subject} mutant={p.mutant.map toString |>.getD "none"}", dependencies := #[instanceJobs[t.name]!, tagJobs[p.name]!, classifications[law.name]!], action := do
                let some ic := (← validInstances.get).find? (·.decl == t.name) | return
                let subjects := quoted ((between law.doc "Law (subject " ").").getD "")
                for _ in [()] do
                  let some mutant := p.mutant | fail "planted-tag" p.name; continue
                  let mi := (env.find? mutant).get!
                  unless mutant != subject && (mi matches .defnInfo _) &&
                    mi.levelParams.length == ((env.find? subject).get!).levelParams.length do
                    fail "mutant-kind" p.name; continue
                  let si := (env.find? subject).get!
                  if ← isDefEq (mkConst subject (si.levelParams.map Level.param))
                    (mkConst mutant (si.levelParams.map Level.param)) then
                    fail "mutant-defeq" p.name; continue
                  let mt := mi.type.instantiateLevelParams mi.levelParams (si.levelParams.map Level.param)
                  unless ← isDefEq si.type mt do fail "mutant-type" p.name; continue
                  let mut boundaries := #[]
                  for name in subjects do
                    if let some resolved ← resolve all law.name name then boundaries := boundaries.push resolved
                  let expected ← counterexample law.info ic subject mutant boundaries (← get).budget
                  Lean.Meta.check expected
                  unless closed p.info.type && (← protectedEq p.info.type expected (boundaries.push mutant) (← get).budget) do
                    fail "planted-proposition" p.name s!"law={law.name} case={ic.key}"
            }
      return jobs
    let saved ← liftM (Meta.saveState : MetaM Meta.SavedState)
    let planned ← tryCatchRuntimeEx
      (withTheReader Core.Context (fun ctx => { ctx with initHeartbeats := schedulerStart, maxHeartbeats := deadline.maxHeartbeats }) do
        let (jobs, _) ← build.run { (← get) with findings := #[], resolutions := {} }
        Core.checkMaxHeartbeats "vacuity-scheduler"
        return some jobs)
      (fun ex => do
        liftM (saved.restore : MetaM Unit)
        let message ← ex.toMessageData.toString
        let reason := if ex.isMaxHeartbeat then "heartbeats" else if message == "scheduler-work-budget" then "work-budget" else "analysis-error"
        emit "INCOMPLETE" "analysis-budget-or-error" .anonymous
          s!"stage=scheduler law=none case=none reason={reason} heartbeats={((← IO.getNumHeartbeats) - schedulerStart) / 1000}/2000000 work={← schedulerWork.get}/20000 projection=0/4096"
        emit "DETAIL" "analysis-error" .anonymous (← ex.toMessageData.toString)
        return none)
    emit "NOTE" "scheduler-usage" .anonymous
      s!"heartbeats={((← IO.getNumHeartbeats) - schedulerStart) / 1000}/2000000 work={← schedulerWork.get}/20000"
    let schedulerHeartbeats ← IO.mkRef ((← IO.getNumHeartbeats) - schedulerStart)
    if let some jobs := planned then
      tryCatchRuntimeEx
        (do
          let capText ← IO.getEnv "VACUITY_WHOLE_RUN_CAP"
          let cap ← match capText with
            | none => pure (jobs.size * 20000)
            | some text =>
              let some cap := text.toNat? | throwError "invalid-whole-run-cap"
              if cap > jobs.size * 20000 then throwError "invalid-whole-run-cap"
              pure cap
          runJobs deadline jobs cap schedulerWork schedulerHeartbeats)
        (fun ex => do
          let message ← ex.toMessageData.toString
          let reason := if ex.isMaxHeartbeat then "heartbeats" else if message == "scheduler-work-budget" then "work-budget" else "analysis-error"
          emit "INCOMPLETE" "analysis-budget-or-error" .anonymous
            s!"stage=scheduler law=none case=none reason={reason} heartbeats={(← schedulerHeartbeats.get) / 1000}/2000000 work={← schedulerWork.get}/20000 projection=0/4096"
          emit "DETAIL" "analysis-error" .anonymous (← ex.toMessageData.toString))
      emit "NOTE" "scheduler-total" .anonymous s!"heartbeats={(← schedulerHeartbeats.get) / 1000}/2000000 work={← schedulerWork.get}/20000"
  let (_, state) ← withTheReader Core.Context (fun ctx => { ctx with maxHeartbeats := 0 }) (run.run {})
  return state.findings

end Vacuity
