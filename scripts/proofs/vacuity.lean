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

/-- Inspect the proof head, recursor branches and constructor proof fields. One shared
    counter bounds traversal and every raw definition unfolding; heartbeats bound meta work. -/
partial def direct (n : Name) (e : Expr) : StateRefT Nat Audit Bool := do
  let fuel ← get
  if fuel == 0 then emit "INCOMPLETE" "projection-budget" n; return false
  set (fuel - 1)
  let e := cleanSpine e
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

/-- Follow only the supplied hypothesis proof, including opaque proof helpers.
    Kernel reduction uses all transparency levels: opaque proofs supplied inside
    discharging lemmas must select their reachable branches before inspection.
    This is semantic reduction, independent of proof names or nesting depth.
    The visited set and shared budget bound the entire dependency traversal. -/
partial def falseDischarge (e : Expr) : StateRefT (Nat × Std.HashSet Expr) MetaM Bool := do
  let e ← withTransparency .all <| whnf (cleanSpine e)
  let (fuel, visited) ← get
  if fuel == 0 then throwError "false-discharge-budget"
  set (fuel - 1, visited)
  if let .const n levels := e.getAppFn' then
    if [``False.elim, ``False.casesOn, ``absurd, ``Empty.elim, ``Empty.casesOn,
        ``PEmpty.elim, ``PEmpty.casesOn].contains n then return true
    if let some matcher ← getMatcherInfo? n then
      if matcher.numAlts == 0 then return true
    if let some (.recInfo r) := (← getEnv).find? n then
      if r.rules.isEmpty then return true
    unless (← get).2.contains e do
      modify fun (left, seen) => (left, seen.insert e)
      if let some ci := (← getEnv).find? n then
        if let some value := ci.value? (allowOpaque := true) then
          let applied := (mkAppN (value.instantiateLevelParams ci.levelParams levels) e.getAppArgs).headBeta
          let reduced ← whnfD applied
          if ← falseDischarge reduced then return true
  match e with
  | .app .. =>
    for arg in e.getAppArgs do
      if ← isProof arg then
        if ← falseDischarge arg then return true
    return false
  | .lam .. => lambdaTelescope e fun _ body => falseDischarge body
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
  withProtected a b subjects isDefEq

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


def check (population : List (Name × ConstantInfo)) : MetaM (Array Finding) := do
  let env ← getEnv
  let all := env.constants.map₁.toList ++ env.constants.map₂.toList
  let scope := (← IO.getEnv "VACUITY_SCOPE").getD ""
  let scopes := scope.splitOn ","
  let budget := ((← IO.getEnv "VACUITY_BUDGET").getD "64").toNat?.getD 64
  let roadmap ← IO.FS.readFile "ROADMAP.md"
  let run : Audit Unit := do
    let mut tags : Array Tagged := #[]
    let mut hasAuthored := false
    let mut authoredModules : Std.HashSet String := {}
    for (n, ci) in population do
      let module := (env.getModuleIdxFor? n).map fun i => toString env.header.moduleNames[i.toNat]!
      if scope != "" && !module.any scopes.contains && !(toString n).startsWith "ConcreteSpecControl." then continue
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
    unless hasAuthored do fail "empty-authored-scope" .anonymous scope
    if scope != "" then
      for m in scopes do
        unless authoredModules.contains m do fail "empty-authored-scope" .anonymous m
    let mut evidence : Array Evidence := #[]
    for t in tags do
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
    for t in evidence do
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
            let (bad, _) ← (falseDischarge arg).run (20000, {})
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
    for p in evidence do
      if (p.doc.splitOn "Planted (").length > 1 then
        unless instances.any (fun i => p.law == i.law && p.key == i.key) do
          fail "unpaired-planted" p.name
          if evidence.any (fun i => i.law == p.law && i.key == p.key &&
              (i.doc.splitOn "Instance (").length > 1) then
            fail "planted-proposition" p.name s!"law={p.law} case={p.key} no-valid-closed-instance"
    for law in tags do
      unless (law.doc.splitOn "Law (subject ").length > 1 do continue
      progress "law" law.name
      unless law.info.isTheorem do fail "law-kind" law.name; continue
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
        let (projection, _) ← (direct law.name proof).run budget
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
  let safe : Audit Unit := do
    try run
    catch e => emit "INCOMPLETE" "analysis-budget-or-error" .anonymous (← e.toMessageData.toString)
  let (_, state) ← safe.run {}
  return state.findings

end Vacuity
