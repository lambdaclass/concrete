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

structure AuditState where
  findings : Array Finding := #[]
  sources : Std.HashMap String String := {}
  contexts : Std.HashMap Name (Name × List OpenDecl) := {}
  resolutions : Std.HashMap (Name × String) (Option Name) := {}

abbrev Audit := StateRefT AuditState MetaM

def emit (status code : String) (n : Name) (detail : String := "") : Audit Unit :=
  modify fun state =>
    let f := { status, code, name := n, detail : Finding }
    if state.findings.contains f then state
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
  remaining : Nat
  answers : Std.HashMap Expr Bool := {}

mutual
/-- Inspect the complete proof DAG under one declared distinct-expression bound.
    Raw delta unfolds one definition per visit; heartbeats also bound meta work. -/
partial def direct (n : Name) (e : Expr) : StateRefT ProjectionState Audit Bool := do
  let e := cleanSpine e
  if let some answer := (← get).answers[e]? then return answer
  let fuel := (← get).remaining
  if fuel == 0 then emit "INCOMPLETE" "projection-budget" n; return false
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
  | forallBinder (type body : TermId)
  | letBind (type value body : TermId)
  | branch (recursor : TemplateKey) (inputs : Array TermId)
  | call (callee : TemplateKey) (inputs : Array TermId)
  | demand (inhabitant : TermId)
  | unresolved (term : TermId) (reason : String)
  deriving Inhabited

structure Template where
  key : TemplateKey
  root : TermId
  declaredType : TermId := 0
  rules : Array (Name × Nat × TermId) := #[]
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

inductive Value where
  | closure (node : TermId) (bvars : Array Input) (hypothetical : Array Input)
  | partialCall (key : TemplateKey) (args : Array Input) (hypothetical : Array Input)
  | ctor (name : Name) (numParams : Nat) (args : Array Input)
  deriving Inhabited

structure Graph where
  terms : Array Expr := #[]
  termIds : Std.HashMap Expr TermId := {}
  origins : Array Origin := #[]
  originIds : Std.HashMap Origin OriginId := {}
  templates : Std.HashMap TemplateKey Template := {}
  nodes : Std.HashMap TermId Transfer := {}
  values : Std.HashMap TermId Value := {}
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

structure SolverState where
  graph : Graph := {}
  remaining : Nat := 20000
  worklist : Array Work := #[]
  worklistSteps : Nat := 0
  queryCount : Nat := 0
  componentCount : Nat := 0
  certificates : Nat := 0
  pendingQueries : Std.HashSet Query := {}
  completedQueries : Std.HashMap Query Input := {}
  validatedTypes : Std.HashMap (TermId × Array Input × Array Input) Input := {}
  validatedClosures : Std.HashSet (TermId × Array OriginId) := {}
  transfers : Array Input := #[]
  structuralExclusions : Array Input := #[]
  deriving Inhabited

abbrev Solver := StateRefT SolverState MetaM

def charge : Solver Unit := do
  let s ← get
  if s.remaining == 0 then throwError "false-discharge-budget"
  set { s with remaining := s.remaining - 1, worklistSteps := s.worklistSteps + 1 }

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

def freshLevelFor (lctx : LocalContext) (domTermId : TermId) : Nat := Id.run do
  let mut lvl := 0
  while lctx.contains { name := ((`_n2_var).num lvl).num domTermId } do
    lvl := lvl + 1
  return lvl

def withCanonicalBinder (domTermId : TermId) (orig : Option OriginId)
    (k : Input → Solver α) : Solver α := do
  let lctx ← getLCtx
  let lvl := freshLevelFor lctx domTermId
  let fId : FVarId := { name := ((`_n2_var).num lvl).num domTermId }
  let xExpr := Expr.fvar fId
  let xTermId ← term xExpr
  let o ← match orig with
    | some existing => pure existing
    | none => origin (.hypothetical lvl domTermId)
  let xInput : Input := { term := xTermId, type := domTermId, origin := o }
  let domExpr := (← get).graph.terms[domTermId]!
  let lctx' := lctx.mkLocalDecl fId (`x) domExpr .default
  let insts ← getLocalInstances
  withLCtx lctx' insts (k xInput)

/-- Edges retain formal expressions. Constructing a callee does not instantiate
    its body at this call and cannot recursively grow an expression context. -/
def compileNode (id : TermId) : Solver Unit := do
  if (← get).graph.nodes.contains id then return
  let e := cleanSpine (← get).graph.terms[id]!
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
  let declaredType ← term (ci.type.instantiateLevelParams ci.levelParams key.levels)
  let body := (ci.value? (allowOpaque := true)).getD (mkConst key.name key.levels)
  let root ← term (body.instantiateLevelParams ci.levelParams key.levels)
  let mut rules : Array (Name × Nat × TermId) := #[]
  if let .recInfo r := ci then
    for rule in r.rules do
      let rhsId ← term (rule.rhs.instantiateLevelParams ci.levelParams key.levels)
      rules := rules.push (rule.ctor, rule.nfields, rhsId)
      unless (← get).graph.nodes.contains rhsId do enqueue (.term rhsId)
  let template : Template := { key, root, declaredType, rules }
  modify fun s => { s with graph.templates := s.graph.templates.insert key template }
  unless (← get).graph.nodes.contains declaredType do enqueue (.term declaredType)
  unless (← get).graph.nodes.contains root do enqueue (.term root)

/-- Drain only construction work appended by this validation; the enclosing
    solver's earlier obligations remain queued. Construction shares charge. -/
def drainConstruction (base : Nat) : Solver Unit := do
  while (← get).worklist.size > base do
    let work := (← get).worklist.back!
    modify fun s => { s with worklist := s.worklist.pop }
    charge
    match work with
    | .term id => compileNode id
    | .template key => compileTemplate key
    | _ => throwError "unexpected-certificate-construction-work"

def ensureNode (id : TermId) : Solver Transfer := do
  if let some tr := (← get).graph.nodes[id]? then return tr
  let base := (← get).worklist.size
  enqueue (.term id)
  drainConstruction base
  return (← get).graph.nodes[id]!

def ensureTemplate (key : TemplateKey) : Solver Template := do
  if let some tmpl := (← get).graph.templates[key]? then return tmpl
  let base := (← get).worklist.size
  enqueue (.template key)
  drainConstruction base
  return (← get).graph.templates[key]!

def instantiateExpr (id : TermId) (bvars : Array Input) : Solver Expr := do
  let g := (← get).graph
  let raw := g.terms[id]!
  if !raw.hasLooseBVars || bvars.isEmpty then return raw
  let substExprs := bvars.map (fun inp => g.terms[inp.term]!)
  return raw.instantiateRev substExprs

def mkInput (e : Expr) (orig : OriginId) : Solver Input := do
  let e := cleanSpine e
  let id ← term e
  let ty ← if e.hasLooseBVars then pure (mkSort .zero) else inferType e
  let tyId ← term (cleanSpine ty)
  return { term := id, type := tyId, origin := orig }

def recordValue (id : TermId) (v : Value) : Solver Unit :=
  modify fun s => { s with graph.values := s.graph.values.insert id v }

def mergeHypothetical (xs ys : Array Input) : Array Input := Id.run do
  let mut out := xs
  for y in ys do
    unless out.contains y do
      out := out.push y
  return out

partial def hasSuppliedOrigin (g : Graph) (oid : OriginId) (seen : Std.HashSet OriginId := {}) : Bool :=
  if seen.contains oid then false
  else
    let seen := seen.insert oid
    match g.origins[oid]? with
    | some (.supplied ..) => true
    | some (.field p _) => hasSuppliedOrigin g p seen
    | some (.substitution _ ins) => ins.any (fun i => hasSuppliedOrigin g i seen)
    | _ => false

partial def hasHypotheticalOrigin (g : Graph) (oid : OriginId) (seen : Std.HashSet OriginId := {}) : Bool :=
  if seen.contains oid then false
  else
    let seen := seen.insert oid
    match g.origins[oid]? with
    | some (.hypothetical ..) => true
    | some (.field p _) => hasHypotheticalOrigin g p seen
    | some (.substitution _ ins) => ins.any (fun i => hasHypotheticalOrigin g i seen)
    | _ => false

partial def isDirectHypotheticalOrigin (g : Graph) (oid : OriginId) : Bool :=
  match g.origins[oid]? with
  | some (.hypothetical ..) => true
  | some (.field p _) => isDirectHypotheticalOrigin g p
  | _ => false

def liveBvarsFor (g : Graph) (id : TermId) (bvars : Array Input) : Array Input := Id.run do
  let raw := g.terms[id]!
  if !raw.hasLooseBVars || bvars.isEmpty then return #[]
  let mut out : Array Input := Array.mkEmpty bvars.size
  for h : idx in [:bvars.size] do
    if raw.hasLooseBVar (bvars.size - 1 - idx) then
      out := out.push bvars[idx]
    else
      out := out.push { term := 0, type := 0, origin := 0 }
  return out

def liveHypFor (g : Graph) (id : TermId) (bvars : Array Input) (hypothetical : Array Input) : Array Input := Id.run do
  let raw := g.terms[id]!
  let mut fvars : FVarIdSet := if raw.hasFVar then (collectFVars {} raw).fvarSet else {}
  if raw.hasLooseBVars then
    for h : idx in [:bvars.size] do
      if raw.hasLooseBVar (bvars.size - 1 - idx) then
        let be := g.terms[bvars[idx].term]!
        if be.hasFVar then
          fvars := (collectFVars {} be).fvarIds.foldl (fun s f => s.insert f) fvars
  if fvars.isEmpty then return #[]
  return hypothetical.filter fun inp =>
    match g.terms[inp.term]! with
    | .fvar f => fvars.contains f
    | _ => false

partial def filterTypeArgs (ty : Expr) (args : Array Input) (idx : Nat := 0) : Array Input :=
  if h : idx < args.size then
    match ty with
    | .forallE _ _ body _ =>
      let rest := filterTypeArgs body args (idx + 1)
      let kept := if body.hasLooseBVar 0 then args[idx] else { term := 0, type := 0, origin := 0 }
      #[kept] ++ rest
    | .letE _ _ val body _ =>
      filterTypeArgs (body.instantiate1 val) args idx
    | .mdata _ b =>
      filterTypeArgs b args idx
    | _ => args.extract idx args.size
  else
    #[]

def isUnappliedValue (g : Graph) (id : TermId) : Bool :=
  match g.values[id]? with
  | some (.closure ..) | some (.partialCall ..) => true
  | _ => false

def ruleMinorArity (r : RecursorVal) (rule : RecursorRule) : Nat := Id.run do
  let mut e := rule.rhs
  for _ in [:r.numParams + r.numMotives + r.numMinors + rule.nfields] do
    match e with
    | .lam _ _ b _ => e := b
    | _ => break
  return e.getAppNumArgs

mutual
partial def applyValue (entry : CertificateEntry) (fnInput : Input) (args : Array Input)
    (hypothetical : Array Input) (mode : Mode) : Solver (Except CertificateSupplier Input) := do
  if args.isEmpty then
    if mode == .body then
      match (← get).graph.values[fnInput.term]? with
      | some (.closure nodeId savedBvars savedHyp) =>
        let cKey := (fnInput.term, savedBvars.map (·.origin))
        if (← get).validatedClosures.contains cKey then
          return .ok fnInput
        modify fun s => { s with validatedClosures := s.validatedClosures.insert cKey }
        return ← evalNode entry nodeId savedBvars #[] (mergeHypothetical savedHyp hypothetical) .body
      | some (.partialCall key savedArgs savedHyp) =>
        let cKey := (fnInput.term, savedArgs.map (·.origin))
        if (← get).validatedClosures.contains cKey then
          return .ok fnInput
        modify fun s => { s with validatedClosures := s.validatedClosures.insert cKey }
        return ← evalCall entry key savedArgs (mergeHypothetical savedHyp hypothetical) .body
      | _ => return .ok fnInput
    return .ok fnInput
  match (← get).graph.values[fnInput.term]? with
  | some (.closure nodeId savedBvars savedHyp) =>
    evalNode entry nodeId savedBvars args (mergeHypothetical savedHyp hypothetical) mode
  | some (.partialCall key savedArgs savedHyp) =>
    evalCall entry key (savedArgs ++ args) (mergeHypothetical savedHyp hypothetical) mode
  | _ =>
    let g := (← get).graph
    let fnExpr := g.terms[fnInput.term]!
    let argExprs := args.map (fun a => g.terms[a.term]!)
    let appExpr := mkAppN fnExpr argExprs
    let reduced := appExpr.headBeta
    if reduced != appExpr then
      let redId ← term reduced
      evalNode entry redId #[] #[] hypothetical mode
    else
      for a in args do
        if isUnappliedValue (← get).graph a.term then
          if let .error sup ← applyValue entry a #[] hypothetical .body then
            return .error sup
      let appId ← term appExpr
      let o ← origin (.substitution appId (#[fnInput.origin] ++ args.map (·.origin)))
      let inp ← mkInput appExpr o
      return .ok inp

partial def openMinorFields (entry : CertificateEntry) (minorInput : Input) (majorInput : Input)
    (extraArgs : Array Input) (minorArity : Nat) (idx : Nat) (hypothetical : Array Input) :
    Solver (Except CertificateSupplier Input) := do
  if idx >= minorArity then
    match ← applyValue entry minorInput extraArgs hypothetical .body with
    | .error sup => return .error sup
    | .ok out =>
      if isUnappliedValue (← get).graph out.term then
        applyValue entry out #[] hypothetical .body
      else
        return .ok out
  else
    let mExpr := (← get).graph.terms[minorInput.term]!
    let mTy ← whnfD (← inferType mExpr)
    if let .forallE _ dom _ _ := mTy then
      let domId ← term (cleanSpine dom)
      let fOrig ← origin (.field majorInput.origin idx)
      withCanonicalBinder domId (some fOrig) fun xInput => do
        match ← applyValue entry minorInput #[xInput] (hypothetical.push xInput) .closure with
        | .error sup => return .error sup
        | .ok nextMinor =>
          openMinorFields entry nextMinor majorInput extraArgs minorArity (idx + 1) (hypothetical.push xInput)
    else
      match ← applyValue entry minorInput extraArgs hypothetical .body with
      | .error sup => return .error sup
      | .ok out =>
        if isUnappliedValue (← get).graph out.term then
          applyValue entry out #[] hypothetical .body
        else
          return .ok out

partial def evalCachedInvariantNode (entry : CertificateEntry) (id : TermId)
    (bvars : Array Input) (hypothetical : Array Input) : Solver (Except CertificateSupplier Input) := do
  let g := (← get).graph
  let liveBvars := liveBvarsFor g id bvars
  let liveHyp := liveHypFor g id liveBvars hypothetical
  let cacheKey := (id, liveBvars, liveHyp)
  if let some cached := (← get).validatedTypes[cacheKey]? then
    return .ok cached
  match ← evalNode entry id liveBvars #[] liveHyp .invariant with
  | .error sup => return .error sup
  | .ok out =>
    modify fun s => { s with validatedTypes := s.validatedTypes.insert cacheKey out }
    return .ok out

partial def evalTypeInstantiate (entry : CertificateEntry) (id : TermId) (bvars : Array Input)
    (typeArgs : Array Input) (hypothetical : Array Input) : Solver (Except CertificateSupplier Input) := do
  charge
  let transfer ← ensureNode id
  match transfer with
  | .forallBinder typeId bodyId =>
    let usedBvars := if (← get).graph.terms[id]!.hasLooseBVars then bvars else #[]
    match ← evalCachedInvariantNode entry typeId usedBvars hypothetical with
    | .error sup => return .error sup
    | .ok domInp =>
      if !typeArgs.isEmpty then
        let arg0 := typeArgs[0]!
        let rest := typeArgs.extract 1 typeArgs.size
        evalTypeInstantiate entry bodyId (usedBvars.push arg0) rest hypothetical
      else
        let rawPi ← instantiateExpr id usedBvars
        withCanonicalBinder domInp.term none fun xInput => do
          match ← evalTypeInstantiate entry bodyId (usedBvars.push xInput) #[] (hypothetical.push xInput) with
          | .error sup => return .error sup
          | .ok _ =>
            let piO ← origin (.substitution id (usedBvars.map (·.origin)))
            let piInp ← mkInput rawPi piO
            return .ok piInp
  | .letBind typeId valueId bodyId =>
    match ← evalCachedInvariantNode entry typeId bvars hypothetical with
    | .error sup => return .error sup
    | .ok _ =>
      match ← evalNode entry valueId bvars #[] hypothetical .closure with
      | .error sup => return .error sup
      | .ok valInput =>
        let bodyNode := (← get).graph.terms[bodyId]!
        if !bodyNode.hasLooseBVar 0 && isUnappliedValue (← get).graph valInput.term then
          if let .error sup ← applyValue entry valInput #[] hypothetical .body then
            return .error sup
        evalTypeInstantiate entry bodyId (bvars.push valInput) typeArgs hypothetical
  | _ =>
    evalCachedInvariantNode entry id bvars hypothetical

partial def evalDeclaredType (entry : CertificateEntry) (key : TemplateKey) (tmpl : Template)
    (actualArgs : Array Input) (hypothetical : Array Input) (appId : TermId) :
    Solver (Except CertificateSupplier Input) := do
  let gInit := (← get).graph
  let declTyExpr := gInit.terms[tmpl.declaredType]!
  let typeInputs := filterTypeArgs declTyExpr actualArgs
  let mut typeFVars : FVarIdSet := {}
  for a in typeInputs do
    if a.term != 0 then
      let e := gInit.terms[a.term]!
      if e.hasFVar then
        typeFVars := (collectFVars {} e).fvarIds.foldl (fun s f => s.insert f) typeFVars
  let typeHyp := hypothetical.filter fun inp =>
    match gInit.terms[inp.term]! with
    | .fvar f => typeFVars.contains f
    | _ => false
  let qType : Query := {
    template := { key with mode := .invariant },
    inputs := typeInputs,
    guards := entry.query.guards,
    hypothetical := typeHyp,
    mode := .invariant
  }
  if let some cached := (← get).completedQueries[qType]? then
    let mut combOrigins := #[cached.origin]
    for a in actualArgs do
      unless combOrigins.contains a.origin do
        combOrigins := combOrigins.push a.origin
    let outOrigin ← origin (.substitution appId combOrigins)
    return .ok { cached with origin := outOrigin }
  if (← get).pendingQueries.contains qType then
    throwError "cyclic-certificate-dependency"
  enqueue (.query qType)
  modify fun s => {
    s with
      worklist := s.worklist.pop,
      queryCount := s.queryCount + 1,
      pendingQueries := s.pendingQueries.insert qType
  }
  charge
  let res ← evalTypeInstantiate entry tmpl.declaredType #[] typeInputs typeHyp
  modify fun s => { s with pendingQueries := s.pendingQueries.erase qType }
  match res with
  | .ok typeOut =>
    if isUnappliedValue (← get).graph typeOut.term then
      if let .error sup ← applyValue entry typeOut #[] typeHyp .body then
        let mut combOrigins := #[sup.input.origin]
        for a in actualArgs do
          unless combOrigins.contains a.origin do
            combOrigins := combOrigins.push a.origin
        let errOrigin ← origin (.substitution appId combOrigins)
        return .error { sup with input := { sup.input with origin := errOrigin } }
    modify fun s => { s with completedQueries := s.completedQueries.insert qType typeOut }
    let mut combOrigins := #[typeOut.origin]
    for a in actualArgs do
      unless combOrigins.contains a.origin do
        combOrigins := combOrigins.push a.origin
    let outOrigin ← origin (.substitution appId combOrigins)
    return .ok { typeOut with origin := outOrigin }
  | .error sup =>
    let mut combOrigins := #[sup.input.origin]
    for a in actualArgs do
      unless combOrigins.contains a.origin do
        combOrigins := combOrigins.push a.origin
    let errOrigin ← origin (.substitution appId combOrigins)
    return .error { sup with input := { sup.input with origin := errOrigin } }

partial def evalCall (entry : CertificateEntry) (key : TemplateKey) (actualArgs : Array Input)
    (hypothetical : Array Input) (mode : Mode) : Solver (Except CertificateSupplier Input) := do
  let gInit := (← get).graph
  let mut argFVars : FVarIdSet := {}
  for a in actualArgs do
    let e := gInit.terms[a.term]!
    if e.hasFVar then
      argFVars := (collectFVars {} e).fvarIds.foldl (fun s f => s.insert f) argFVars
  let queryHyp := hypothetical.filter fun inp =>
    match gInit.terms[inp.term]! with
    | .fvar f => argFVars.contains f
    | _ => false
  let q : Query := { template := key, inputs := actualArgs, guards := entry.query.guards, hypothetical := queryHyp, mode }
  if let some cached := (← get).completedQueries[q]? then
    return .ok cached
  if (← get).pendingQueries.contains q then
    throwError "cyclic-certificate-dependency"
  enqueue (.query q)
  modify fun s => {
    s with
      worklist := s.worklist.pop,
      queryCount := s.queryCount + 1,
      pendingQueries := s.pendingQueries.insert q
  }
  charge
  let tmpl ← ensureTemplate key
  let ci ← getConstInfo key.name
  let g := (← get).graph
  let argExprs := actualArgs.map (fun a => g.terms[a.term]!)
  let appExpr := mkAppN (mkConst key.name key.levels) argExprs
  let appId ← term appExpr
  let callOrigin ← origin (.substitution appId (actualArgs.map (·.origin)))
  let callInput ← mkInput appExpr callOrigin
  match ← evalDeclaredType entry key tmpl actualArgs hypothetical appId with
  | .error sup =>
    modify fun s => { s with pendingQueries := s.pendingQueries.erase q }
    return .error sup
  | .ok _ => pure ()
  let hasVal := (ci.value? (allowOpaque := true)).isSome
  let res : Except CertificateSupplier Input ← match hasVal with
    | true => do
      let rootTransfer ← ensureNode tmpl.root
      let isLam := match rootTransfer with | .binder .. => true | _ => false
      if actualArgs.isEmpty && mode == .closure && isLam then
        modify fun s => { s with transfers := s.transfers.push callInput }
        recordValue callInput.term (.partialCall key #[] hypothetical)
        pure (.ok callInput)
      else
        match ← evalNode entry tmpl.root #[] actualArgs hypothetical mode with
        | .ok bodyOut =>
          let mut combOrigins := #[bodyOut.origin]
          for a in actualArgs do
            unless combOrigins.contains a.origin do
              combOrigins := combOrigins.push a.origin
          let outOrigin ← origin (.substitution appId combOrigins)
          let outInput : Input := { bodyOut with origin := outOrigin }
          modify fun s => { s with transfers := s.transfers.push outInput }
          pure (.ok outInput)
        | .error sup =>
          let mut combOrigins := #[sup.input.origin]
          for a in actualArgs do
            unless combOrigins.contains a.origin do
              combOrigins := combOrigins.push a.origin
          let errOrigin ← origin (.substitution appId combOrigins)
          pure (.error { sup with input := { sup.input with origin := errOrigin } })
    | false => do
      match ci with
      | .axiomInfo _ | .opaqueInfo _ =>
        for a in actualArgs do
          if isUnappliedValue (← get).graph a.term then
            if let .error sup ← applyValue entry a #[] hypothetical .body then
              return .error sup
        let gNow := (← get).graph
        let mut hasProofHyp := false
        let mut allProofsHyp := true
        let mut anySupplied := false
        for a in actualArgs do
          if hasSuppliedOrigin gNow a.origin then
            anySupplied := true
          if ← Meta.isProof gNow.terms[a.term]! then
            if hasHypotheticalOrigin gNow a.origin && !hasSuppliedOrigin gNow a.origin then
              hasProofHyp := true
            else
              allProofsHyp := false
        let purelyHypothetical := hasProofHyp && allProofsHyp && !anySupplied
        let isRefutable ← refutableType (← inferType appExpr)
        if isRefutable && !purelyHypothetical then
          let baseSup ← origin (.supplied entry.instanceName 0 appId)
          let supOrigin ← if actualArgs.isEmpty then
              pure baseSup
            else
              origin (.substitution appId (#[baseSup] ++ actualArgs.map (·.origin)))
          let supInput : Input := { callInput with origin := supOrigin }
          modify fun s => { s with transfers := s.transfers.push supInput }
          pure (.error { declaration := key.name, input := supInput })
        else
          modify fun s => { s with transfers := s.transfers.push callInput }
          throwError "unavailable-certificate-transfer {key.name}"
      | .ctorInfo c =>
        modify fun s => { s with transfers := s.transfers.push callInput }
        recordValue callInput.term (.ctor key.name c.numParams actualArgs)
        pure (.ok callInput)
      | .recInfo r =>
        modify fun s => { s with transfers := s.transfers.push callInput }
        let majorIdx := r.getMajorIdx
        let recRes : Except CertificateSupplier Input ← if let some majorInput := actualArgs[majorIdx]? then
          if r.rules.isEmpty then
            let gNow := (← get).graph
            if isDirectHypotheticalOrigin gNow majorInput.origin then
              modify fun s => {
                s with structuralExclusions :=
                  if s.structuralExclusions.contains majorInput then s.structuralExclusions
                  else s.structuralExclusions.push majorInput
              }
            pure (.ok callInput)
          else
            let mut matchedRule : Option (Name × Nat × TermId × Array Input) := none
            if let some (.ctor cName numParams cArgs) := (← get).graph.values[majorInput.term]? then
              if let some (_, nfields, rhsId) := tmpl.rules.find? (fun (cn, _, _) => cn == cName) then
                if cArgs.size >= numParams + nfields then
                  matchedRule := some (cName, nfields, rhsId, cArgs.extract numParams (numParams + nfields))
            if matchedRule.isNone then
              let majorExpr := (← get).graph.terms[majorInput.term]!
              let mut redMajor ← whnfD majorExpr
              if let .lit (.natVal n) := redMajor then
                redMajor := if n == 0 then mkConst ``Nat.zero else mkApp (mkConst ``Nat.succ) (mkNatLit (n - 1))
              if let .const cName _ := redMajor.getAppFn' then
                if let some (.ctorInfo cInfo) := (← getEnv).find? cName then
                  if let some (_, nfields, rhsId) := tmpl.rules.find? (fun (cn, _, _) => cn == cName) then
                    let rawArgs := redMajor.getAppArgs
                    if rawArgs.size >= cInfo.numParams + nfields then
                      let mut fInputs : Array Input := #[]
                      for idx in [:nfields] do
                        let fExpr := rawArgs[cInfo.numParams + idx]!
                        let fO ← origin (.field majorInput.origin idx)
                        fInputs := fInputs.push (← mkInput fExpr fO)
                      matchedRule := some (cName, nfields, rhsId, fInputs)
            if let some (cName, _, rhsId, fieldInputs) := matchedRule then
              let minorStart := r.numParams + r.numMotives
              let mut paramsAndMinors : Array Input := Array.mkEmpty (minorStart + r.numMinors)
              for idx in [:minorStart] do
                paramsAndMinors := paramsAndMinors.push actualArgs[idx]!
              for (ruleCtor, _, _) in tmpl.rules, idx in [:r.numMinors] do
                if ruleCtor == cName then
                  paramsAndMinors := paramsAndMinors.push actualArgs[minorStart + idx]!
                else
                  paramsAndMinors := paramsAndMinors.push { term := 0, type := 0, origin := 0 }
              let extraArgs := if actualArgs.size > majorIdx + 1 then actualArgs.extract (majorIdx + 1) actualArgs.size else #[]
              let rhsArgs := paramsAndMinors ++ fieldInputs ++ extraArgs
              evalNode entry rhsId #[] rhsArgs hypothetical mode
            else
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
                    match ← openMinorFields entry minorInput majorInput extraArgs minorArity 0 hypothetical with
                    | .error sup => minorErr := some sup
                    | .ok _ => pure ()
              if let some sup := minorErr then pure (.error sup) else pure (.ok callInput)
        else
          recordValue callInput.term (.partialCall key actualArgs hypothetical)
          pure (.ok callInput)
        match recRes with
        | .ok out =>
          let mut combOrigins := #[out.origin]
          for a in actualArgs do
            unless combOrigins.contains a.origin do
              combOrigins := combOrigins.push a.origin
          let outOrigin ← origin (.substitution appId combOrigins)
          pure (.ok { out with origin := outOrigin })
        | .error sup =>
          let mut combOrigins := #[sup.input.origin]
          for a in actualArgs do
            unless combOrigins.contains a.origin do
              combOrigins := combOrigins.push a.origin
          let errOrigin ← origin (.substitution appId combOrigins)
          pure (.error { sup with input := { sup.input with origin := errOrigin } })
      | .quotInfo qVal =>
        for a in actualArgs do
          if isUnappliedValue (← get).graph a.term then
            if let .error sup ← applyValue entry a #[] hypothetical .body then
              return .error sup
        if qVal.kind matches .type | .ctor then
          pure (.ok callInput)
        else
          throwError "unresolved-certificate-quotient {key.name}"
      | .inductInfo _ =>
        for a in actualArgs do
          if isUnappliedValue (← get).graph a.term then
            if let .error sup ← applyValue entry a #[] hypothetical .body then
              return .error sup
        pure (.ok callInput)
      | _ => pure (.ok callInput)
  modify fun s => { s with pendingQueries := s.pendingQueries.erase q }
  match res with
  | .ok out =>
    modify fun s => { s with completedQueries := s.completedQueries.insert q out }
    return .ok out
  | .error sup => return .error sup

partial def evalNode (entry : CertificateEntry) (id : TermId) (bvars : Array Input)
    (args : Array Input) (hypothetical : Array Input) (mode : Mode) :
    Solver (Except CertificateSupplier Input) := do
  charge
  let transfer ← ensureNode id
  match transfer with
  | .unresolved _ reason => throwError reason
  | .demand inh => evalNode entry inh bvars args hypothetical mode
  | .result termId =>
    let raw := (← get).graph.terms[termId]!
    match raw with
    | .bvar i =>
      let some bound := bvars[bvars.size - 1 - i]? | throwError "unbound-certificate-variable"
      applyValue entry bound args hypothetical mode
    | .fvar _ =>
      let g := (← get).graph
      let some found := hypothetical.find? (fun inp => g.terms[inp.term]? == some raw)
        | throwError "unpermitted-certificate-assumption"
      applyValue entry found args hypothetical mode
    | .lit (.natVal n) =>
      let o ← origin (.substitution termId #[])
      let inp ← mkInput raw o
      if n == 0 then
        recordValue inp.term (.ctor ``Nat.zero 0 #[])
      else
        let predExpr := mkNatLit (n - 1)
        let predInp ← mkInput predExpr o
        recordValue inp.term (.ctor ``Nat.succ 0 #[predInp])
      applyValue entry inp args hypothetical mode
    | .sort _ | .lit _ =>
      let o ← origin (.substitution termId #[])
      let inp ← mkInput raw o
      applyValue entry inp args hypothetical mode
    | _ =>
      if raw == (← get).graph.terms[id]! then
        throwError "unsupported-certificate-expression"
      else
        evalNode entry termId bvars args hypothetical mode
  | .letBind typeId valueId bodyId =>
    match ← evalCachedInvariantNode entry typeId bvars hypothetical with
    | .error sup => return .error sup
    | .ok _ =>
      let bodyNode := (← get).graph.terms[bodyId]!
      let valMode := if !bodyNode.hasLooseBVar 0 then .body else .closure
      match ← evalNode entry valueId bvars #[] hypothetical valMode with
      | .error sup => return .error sup
      | .ok valInput =>
        if !bodyNode.hasLooseBVar 0 && isUnappliedValue (← get).graph valInput.term then
          if let .error sup ← applyValue entry valInput #[] hypothetical .body then
            return .error sup
        evalNode entry bodyId (bvars.push valInput) args hypothetical mode
  | .forallBinder typeId bodyId =>
    let usedBvars := if (← get).graph.terms[id]!.hasLooseBVars then bvars else #[]
    match ← evalCachedInvariantNode entry typeId usedBvars hypothetical with
    | .error sup => return .error sup
    | .ok domInp =>
      let rawPi ← instantiateExpr id usedBvars
      withCanonicalBinder domInp.term none fun xInput => do
        match ← evalCachedInvariantNode entry bodyId (usedBvars.push xInput) (hypothetical.push xInput) with
        | .error sup => return .error sup
        | .ok _ =>
          let piO ← origin (.substitution id (usedBvars.map (·.origin)))
          let piInp ← mkInput rawPi piO
          return .ok piInp
  | .binder typeId bodyId =>
    let usedBvars := if (← get).graph.terms[id]!.hasLooseBVars then bvars else #[]
    match ← evalCachedInvariantNode entry typeId usedBvars hypothetical with
    | .error sup => return .error sup
    | .ok domInp =>
      if !args.isEmpty then
        let arg0 := args[0]!
        let rest := args.extract 1 args.size
        let bodyNode := (← get).graph.terms[bodyId]!
        if arg0.term != 0 && !bodyNode.hasLooseBVar 0 && isUnappliedValue (← get).graph arg0.term then
          if let .error sup ← applyValue entry arg0 #[] hypothetical .body then
            return .error sup
        evalNode entry bodyId (usedBvars.push arg0) rest hypothetical mode
      else if mode == .body then
        withCanonicalBinder domInp.term none fun xInput => do
          evalNode entry bodyId (usedBvars.push xInput) #[] (hypothetical.push xInput) .body
      else
        let rawLam ← instantiateExpr id usedBvars
        let gNow := (← get).graph
        let rawNode := gNow.terms[id]!
        let lamFVars := (collectFVars {} rawLam).fvarSet
        let mut capOrigins : Array OriginId := #[]
        for h : idx in [:usedBvars.size] do
          if rawNode.hasLooseBVar (usedBvars.size - 1 - idx) then
            let o := usedBvars[idx].origin
            unless capOrigins.contains o do
              capOrigins := capOrigins.push o
        for inp in hypothetical do
          if let .fvar f := gNow.terms[inp.term]! then
            if lamFVars.contains f && !capOrigins.contains inp.origin then
              capOrigins := capOrigins.push inp.origin
        let lamO ← origin (.substitution id capOrigins)
        let lamInp ← mkInput rawLam lamO
        recordValue lamInp.term (.closure id usedBvars hypothetical)
        return .ok lamInp
  | .application fnId inputIds =>
    let argMode := if mode == .invariant then .invariant else .closure
    let mut evalInputs : Array Input := #[]
    for childId in inputIds do
      match ← evalNode entry childId bvars #[] hypothetical argMode with
      | .error sup =>
        let errO ← origin (.substitution id (#[sup.input.origin] ++ evalInputs.map (·.origin)))
        return .error { sup with input := { sup.input with origin := errO } }
      | .ok inp => evalInputs := evalInputs.push inp
    let allArgs := evalInputs ++ args
    evalNode entry fnId bvars allArgs hypothetical mode
  | .constructor name inputIds =>
    let argMode := if mode == .invariant then .invariant else .closure
    let mut evalInputs : Array Input := #[]
    for childId in inputIds do
      match ← evalNode entry childId bvars #[] hypothetical argMode with
      | .error sup =>
        let errO ← origin (.substitution id (#[sup.input.origin] ++ evalInputs.map (·.origin)))
        return .error { sup with input := { sup.input with origin := errO } }
      | .ok inp => evalInputs := evalInputs.push inp
    let allArgs := evalInputs ++ args
    let key : TemplateKey := { name, levels := ((← get).graph.terms[id]!).getAppFn'.constLevels!, mode := .transfer }
    evalCall entry key allArgs hypothetical mode
  | .branch key inputIds =>
    let argMode := if mode == .invariant then .invariant else .closure
    let mut evalInputs : Array Input := #[]
    for childId in inputIds do
      match ← evalNode entry childId bvars #[] hypothetical argMode with
      | .error sup =>
        let errO ← origin (.substitution id (#[sup.input.origin] ++ evalInputs.map (·.origin)))
        return .error { sup with input := { sup.input with origin := errO } }
      | .ok inp => evalInputs := evalInputs.push inp
    let allArgs := evalInputs ++ args
    evalCall entry key allArgs hypothetical mode
  | .call key inputIds =>
    let argMode := if mode == .invariant then .invariant else .closure
    let mut evalInputs : Array Input := #[]
    for childId in inputIds do
      match ← evalNode entry childId bvars #[] hypothetical argMode with
      | .error sup =>
        let errO ← origin (.substitution id (#[sup.input.origin] ++ evalInputs.map (·.origin)))
        return .error { sup with input := { sup.input with origin := errO } }
      | .ok inp => evalInputs := evalInputs.push inp
    let allArgs := evalInputs ++ args
    evalCall entry key allArgs hypothetical mode
  | .projection typeName idx receiverId =>
    let argMode := if mode == .invariant then .invariant else .closure
    match ← evalNode entry receiverId bvars #[] hypothetical argMode with
    | .error sup => return .error sup
    | .ok recvInput =>
      let mut fieldInput : Option Input := none
      if let some (.ctor cName numParams cArgs) := (← get).graph.values[recvInput.term]? then
        let _ := cName
        for h : cIdx in [numParams:cArgs.size] do
          if cIdx != numParams + idx then
            let otherArg := cArgs[cIdx]
            if isUnappliedValue (← get).graph otherArg.term then
              if let .error sup ← applyValue entry otherArg #[] hypothetical .body then
                return .error sup
        if let some fInp := cArgs[numParams + idx]? then
          let fO ← origin (.field recvInput.origin idx)
          fieldInput := some { fInp with origin := fO }
      if fieldInput.isNone then
        let recvExpr := (← get).graph.terms[recvInput.term]!
        let projExpr := Expr.proj typeName idx recvExpr
        let redProj ← whnfD projExpr
        let fO ← origin (.field recvInput.origin idx)
        if redProj != projExpr then
          let redId ← term redProj
          match ← evalNode entry redId #[] #[] hypothetical argMode with
          | .error sup => return .error sup
          | .ok inp => fieldInput := some { inp with origin := fO }
        else
          fieldInput := some (← mkInput projExpr fO)
      applyValue entry fieldInput.get! args hypothetical mode
end

/-- Validate a candidate without changing any consumer or admitting it into a
    cache. The caller's SolverState supplies the existing 20,000-work allowance;
    this function neither resets it nor starts a new heartbeat deadline. -/
def validateCertificate (entry : CertificateEntry) (candidate : Certificate) :
    Solver CertificateResult := do
  let base := (← get).worklist.size
  enqueue (.certificate entry.query candidate)
  tryCatchRuntimeEx (do
    let qTransferBody : Query := { entry.query with template := { entry.query.template with mode := .transfer }, hypothetical := #[], mode := .body }
    let qTransferClosure : Query := { entry.query with template := { entry.query.template with mode := .transfer }, hypothetical := #[], mode := .closure }
    let qTransferInvariant : Query := { entry.query with template := { entry.query.template with mode := .transfer }, hypothetical := #[], mode := .invariant }
    let initPending : Std.HashSet Query :=
      ({} : Std.HashSet Query).insert entry.query
        |>.insert qTransferBody
        |>.insert qTransferClosure
        |>.insert qTransferInvariant
    modify fun s => { s with worklist := s.worklist.pop, pendingQueries := initPending, transfers := #[], structuralExclusions := #[] }
    charge
    let graph := (← get).graph
    let some proof := graph.terms[candidate.proof]? | throwError "missing-certificate-proof"
    let some target := graph.terms[entry.target]? | throwError "missing-certificate-target"
    if proof.hasMVar || target.hasMVar then throwError "uninstantiated-certificate-metavariable"
    let mut permitted : Array FVarId := #[]
    for input in candidate.assumptions do
      charge
      let some assumption := (← get).graph.terms[input.term]? | throwError "missing-certificate-assumption"
      let .fvar f := assumption | throwError "nonlocal-certificate-assumption"
      let some (.hypothetical ..) := (← get).graph.origins[input.origin]?
        | throwError "supplied-certificate-assumption"
      unless entry.query.hypothetical.contains input do throwError "unpermitted-certificate-origin"
      let some type := (← get).graph.terms[input.type]? | throwError "missing-certificate-assumption-type"
      if type.hasMVar then throwError "uninstantiated-certificate-metavariable"
      Lean.Meta.check type
      unless ← isDefEq (← inferType assumption) type do throwError "certificate-assumption-type"
      permitted := permitted.push f
    for input in candidate.assumptions do
      let type := (← get).graph.terms[input.type]!
      for f in (collectFVars {} type).fvarIds do
        unless permitted.contains f do throwError "unpermitted-certificate-assumption"
      match ← evalNode entry input.type #[] #[] candidate.assumptions .invariant with
      | .error sup => return .rejectedSuppliedContradiction sup
      | .ok tyOut =>
        if isUnappliedValue (← get).graph tyOut.term then
          if let .error sup ← applyValue entry tyOut #[] candidate.assumptions .body then
            return .rejectedSuppliedContradiction sup
    for e in [proof, target] do
      for f in (collectFVars {} e).fvarIds do
        unless permitted.contains f do throwError "unpermitted-certificate-assumption"
    unless candidate.guards == entry.query.guards do throwError "certificate-guard-mismatch"
    Lean.Meta.check proof
    Lean.Meta.check target
    unless ← isDefEq (← inferType proof) target do throwError "certificate-type-mismatch"
    match ← evalNode entry candidate.proof #[] #[] candidate.assumptions .body with
    | .error sup => return .rejectedSuppliedContradiction sup
    | .ok proofOut =>
      if isUnappliedValue (← get).graph proofOut.term then
        if let .error sup ← applyValue entry proofOut #[] candidate.assumptions .body then
          return .rejectedSuppliedContradiction sup
      match ← evalNode entry entry.target #[] #[] candidate.assumptions .body with
      | .error sup => return .rejectedSuppliedContradiction sup
      | .ok targetOut =>
        if isUnappliedValue (← get).graph targetOut.term then
          if let .error sup ← applyValue entry targetOut #[] candidate.assumptions .body then
            return .rejectedSuppliedContradiction sup
        for guard in candidate.guards do
          for id in [guard.type, guard.lhs, guard.rhs] do
            if (← get).graph.terms[id]?.isNone then throwError "missing-certificate-guard-term"
            match ← evalNode entry id #[] #[] candidate.assumptions .body with
            | .error sup => return .rejectedSuppliedContradiction sup
            | .ok gOut =>
              if isUnappliedValue (← get).graph gOut.term then
                if let .error sup ← applyValue entry gOut #[] candidate.assumptions .body then
                  return .rejectedSuppliedContradiction sup
          let g := (← get).graph
          for id in [guard.lhs, guard.rhs] do
            unless ← isDefEq (← inferType g.terms[id]!) g.terms[guard.type]! do
              throwError "certificate-guard-type"
        let s ← get
        set { s with certificates := s.certificates + 1 }
        return .admitted {
          entry,
          candidate,
          environment := (← getEnv),
          transfers := s.transfers,
          structuralExclusions := s.structuralExclusions,
          rank := if s.structuralExclusions.isEmpty then 0 else 1
        }
  ) (fun ex => do
    modify fun s => { s with worklist := s.worklist.extract 0 base }
    return .incomplete { status := "INCOMPLETE", code := "analysis-budget-or-error", name := entry.instanceName, detail := s!"stage=certificate-dependencies law={entry.law} case={entry.caseName} {← ex.toMessageData.toString}" })

end Discharge

structure DischargeState where
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
  if fuel == 0 then throwError "false-discharge-budget"
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

/-- Traverse definitions only along dependencies of the semantic subject. Definitions unrelated
    to S keep their original parsed. A shared fuel bounds the entire dependency search. -/
partial def mentions (e : Expr) (subject : Name) : StateRefT (Nat × Std.HashSet Name) MetaM Bool := do
  let (fuel, _) ← get
  if fuel == 0 then throwError "subject-budget"
  modify fun (_, visited) => (fuel - 1, visited)
  if e.getUsedConstants.contains subject then return true
  for n in e.getUsedConstants do
    if (← get).2.contains n then continue
    modify fun (left, visited) => (left, visited.insert n)
    if let some (.defnInfo d) := (← getEnv).find? n then
      if ← mentions d.value subject then return true
  return false

partial def mutate (e : Expr) (subject mutant : Name) (boundaries : Array Name := #[]) : StateRefT Nat MetaM Expr := do
  let fuel ← get
  if fuel == 0 then throwError "subject-budget"
  modify (· - 1)
  match e with
  | .const n us =>
    if n == subject then return .const mutant us
    if boundaries.contains n then return e
    if let some (.defnInfo d) := (← getEnv).find? n then
      let (occurs, _) ← (mentions d.value subject).run (20000, {})
      if occurs then
        return ← mutate (d.value.instantiateLevelParams d.levelParams us) subject mutant boundaries
    return e
  | .app f a => return mkApp (← mutate f subject mutant boundaries) (← mutate a subject mutant boundaries)
  | .lam n t b bi => return .lam n (← mutate t subject mutant boundaries) (← mutate b subject mutant boundaries) bi
  | .forallE n t b bi => return .forallE n (← mutate t subject mutant boundaries) (← mutate b subject mutant boundaries) bi
  | .letE n t v b nd => return .letE n (← mutate t subject mutant boundaries) (← mutate v subject mutant boundaries) (← mutate b subject mutant boundaries) nd
  | .mdata md b => return .mdata md (← mutate b subject mutant boundaries)
  | .proj n i b => return .proj n i (← mutate b subject mutant boundaries)
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
def counterexample (law : ConstantInfo) (ic : InstanceCase) (s m : Name) (subjects : Array Name) : MetaM Expr := do
  let mut original := law.type.instantiateLevelParams law.levelParams ic.levels
  let (changed, _) ← (mutate original s m subjects).run 20000
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
def withProtected (a b : Expr) (subjects : Array Name) (check : Expr → Expr → MetaM Bool) : MetaM Bool := do
  let mut a := a
  let mut b := b
  for subject in subjects do
    a := (← (mutate a subject subject subjects).run 20000).1
    b := (← (mutate b subject subject subjects).run 20000).1
  let rec collect (e : Expr) (terms : Array Expr) : Array Expr :=
    match e with
    | .const n _ => if subjects.contains n && !terms.contains e then terms.push e else terms
    | .app f a => collect a (collect f terms)
    | .lam _ t b _ | .forallE _ t b _ => collect b (collect t terms)
    | .letE _ t v b _ => collect b (collect v (collect t terms))
    | .mdata _ b | .proj _ _ b => collect b terms
    | _ => terms
  let protectedTerms := collect b (collect a #[])
  let rec compare (i : Nat) (a b : Expr) : MetaM Bool := do
    if i >= protectedTerms.size then return ← check a b
    let term := protectedTerms[i]!
    withLocalDeclD `subject (← inferType term) fun subjectVar => do
      let replace := fun e => e.replace fun node =>
        if node == term then some subjectVar else none
      compare (i + 1) (replace a) (replace b)
  compare 0 a b


def protectedEq (a b : Expr) (subjects : Array Name) : MetaM Bool :=
  -- Smart unfolding intentionally leaves compiled matchers stuck on abstract
  -- discriminants. Protection makes subject applications abstract, so compare
  -- contracts using ordinary definitional unfolding after protection instead.
  withProtected a b subjects fun x y =>
    withOptions (fun opts => opts.setBool `smartUnfolding false) (isDefEq x y)

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


/-- Isolate one named analysis without renewing its heartbeat allowance. The
    enclosing scheduler disables heartbeat checks only for bookkeeping; every
    analysis receives the original command deadline. Runtime exceptions need an
    explicit catch (ordinary Lean `try` deliberately rethrows them). -/
def isolated (deadline : Core.Context) (name : Name) (detail : String)
    (fallback : α) (action : Audit α) : Audit α := do
  let saved ← liftM (Meta.saveState : MetaM Meta.SavedState)
  tryCatchRuntimeEx
    (withTheReader Core.Context (fun _ => deadline) do
      Core.checkMaxHeartbeats "vacuity-analysis"
      action)
    (fun e => do
      let message ← addMessageContext e.toMessageData
      liftM (saved.restore : MetaM Unit)
      emit "INCOMPLETE" "analysis-budget-or-error" name
        (detail ++ " " ++ (← message.toString))
      return fallback)


def check (population : List (Name × ConstantInfo)) : MetaM (Array Finding) := do
  let env ← getEnv
  let all := env.constants.map₁.toList ++ env.constants.map₂.toList
  let scope := (← IO.getEnv "VACUITY_SCOPE").getD ""
  let scopes := scope.splitOn ","
  let budget := ((← IO.getEnv "VACUITY_BUDGET").getD "4096").toNat?.getD 4096
  let roadmap ← IO.FS.readFile "ROADMAP.md"
  let deadline ← readThe Core.Context
  let run : Audit Unit := do
    let mut tags : Array Tagged := #[]
    let mut hasAuthored := false
    let mut authoredModules : Std.HashSet String := {}
    for (n, ci) in population do
      let module := (env.getModuleIdxFor? n).map fun i => toString env.header.moduleNames[i.toNat]!
      if scope != "" && !module.any scopes.contains && !(toString n).startsWith "ConcreteSpecControl." then continue
      let result ← isolated deadline n "stage=classification" (tags, hasAuthored, authoredModules) do
        let mut tags := tags
        let mut hasAuthored := hasAuthored
        let mut authoredModules := authoredModules
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
        return (tags, hasAuthored, authoredModules)
      tags := result.1
      hasAuthored := result.2.1
      authoredModules := result.2.2
    unless hasAuthored do fail "empty-authored-scope" .anonymous scope
    if scope != "" then
      for m in scopes do
        unless authoredModules.contains m do fail "empty-authored-scope" .anonymous m
    let mut evidence : Array Evidence := #[]
    for t in tags do
      let result ← isolated deadline t.name "stage=evidence-tags" evidence do
        let mut evidence := evidence
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
              evidence := evidence.push { toTagged := { t with doc := doc }, law, key, subject, mutant }
        return evidence
      evidence := result
    for t in evidence do
      isolated deadline t.name s!"stage=evidence-kind law={t.law} case={t.key}" () do
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
    let mut instances : Array InstanceCase := #[]
    for t in evidence do
      if !(t.doc.splitOn "Instance (").length > 1 then continue
      let result ← isolated deadline t.name s!"stage=instance law={t.law} case={t.key}" instances do
        let mut instances := instances
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
          for arg in args do
            ty := ty.consumeMData
            if let .forallE _ domain body _ := ty then
              if ← isProp domain then
                let (bad, _) ← (falseDischarge arg).run {}
                falseHyp := falseHyp || bad
              ty := body.instantiate1 arg
            else valid := false
          let mut protectedNames := #[]
          if let some l := tags.find? (·.name == law) then
            for st in quoted ((between l.doc "Law (subject " ").").getD "") do
              if let some n ← resolve all l.name st then protectedNames := protectedNames.push n
          unless valid && (← closedTail ty protectedNames) && (← protectedEq ty t.info.type protectedNames) do fail "instance-application" t.name s!"law={law} case={key}"; continue
          if falseHyp then
            fail "instance-false-discharge" t.name
            continue
          if instances.any (fun i => i.law == law && i.key == key) then fail "duplicate-instance" t.name
          instances := instances.push ⟨t.name, law, key, args, levels⟩
        return instances
      instances := result
    for p in evidence do
      if !(p.doc.splitOn "Planted (").length > 1 then continue
      isolated deadline p.name s!"stage=pairing law={p.law} case={p.key}" () do
        for _ in [()] do
          if (p.doc.splitOn "Planted (").length > 1 then
            unless instances.any (fun i => p.law == i.law && p.key == i.key) do
              fail "unpaired-planted" p.name
              if evidence.any (fun i => i.law == p.law && i.key == p.key &&
                  (i.doc.splitOn "Instance (").length > 1) then
                fail "planted-proposition" p.name s!"law={p.law} case={p.key} no-valid-closed-instance"
    for law in tags do
      unless (law.doc.splitOn "Law (subject ").length > 1 do continue
      isolated deadline law.name "stage=law" () do
        for _ in [()] do
          progress "law" law.name
          unless law.info.isTheorem do fail "law-kind" law.name; continue
          isolated deadline law.name "stage=projection" () do
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
              let (projection, cost) ← (direct law.name proof).run { remaining := budget }
              emit "NOTE" "projection-cost" law.name s!"visited={budget - cost.remaining} limit={budget} cached={cost.answers.size}"
              if projection then fail "projection-law" law.name
          let subjects := quoted ((between law.doc "Law (subject " ").").getD "")
          if subjects.isEmpty then fail "law-subject" law.name
          let cases := instances.filter (·.law == law.name)
          if cases.isEmpty then fail "missing-instance" law.name
          for st in subjects do
            let some subject ← resolve all law.name st | continue
            unless ((env.find? subject).get! matches .defnInfo _) do fail "subject-kind" law.name st; continue
            let (occurs, _) ← (mentions law.info.type subject).run (20000, {})
            unless occurs do fail "subject-absent" law.name st
            for ic in cases do
              let planted := evidence.filter fun t => t.law == law.name &&
                t.key == ic.key && t.subject == some subject &&
                (t.doc.splitOn "Planted (").length > 1
              if planted.isEmpty then fail "missing-planted" law.name (ic.key ++ "/" ++ st)
              for p in planted do
                isolated deadline p.name s!"stage=planted law={law.name} case={ic.key} instance={ic.decl} subject={subject}" () do
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
                    let expected ← counterexample law.info ic subject mutant boundaries
                    Lean.Meta.check expected
                    unless closed p.info.type && (← protectedEq p.info.type expected (boundaries.push mutant)) do
                      fail "planted-proposition" p.name s!"law={law.name} case={ic.key}"
  -- Bookkeeping must survive the exhausted deadline so each subsequent
  -- skipped analysis still gets its own named INCOMPLETE. No analysis is given
  -- fresh heartbeats: `isolated` restores the original deadline on every call.
  let (_, state) ← withTheReader Core.Context (fun ctx => { ctx with maxHeartbeats := 0 }) (run.run {})
  return state.findings

end Vacuity
