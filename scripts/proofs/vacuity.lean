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

structure Finding where
  status : String
  code : String
  name : Name
  detail : String
  deriving BEq

abbrev Audit := StateRefT (Array Finding) MetaM

def emit (status code : String) (n : Name) (detail : String := "") : Audit Unit :=
  modify fun found =>
    let f := { status, code, name := n, detail : Finding }
    if found.contains f then found else found.push f

def fail (code : String) (n : Name) (detail : String := "") : Audit Unit := emit "FAIL" code n detail

def between (s left right : String) : Option String := do
  let tail ← (s.splitOn left)[1]?
  return (tail.splitOn right).head!

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

def resolve (all : List (Name × ConstantInfo)) (owner : Name) (s : String) : Audit (Option Name) := do
  let .ok stx := Parser.runParserCategory (← getEnv) `term s
    | fail "unknown-name" owner s; return none
  unless stx.isIdent do fail "unknown-name" owner s; return none
  let requested := stx.getId.replacePrefix (Name.mkSimple "_root_") .anonymous
  let hits := all.filter fun (n, _) => n == requested || privateToUserName n == requested ||
    privateToUserName n == (privateToUserName owner).getPrefix ++ requested
  match hits with
  | [(n, _)] => return some n
  | [] => fail "unknown-name" owner s; return none
  | _ => fail "ambiguous-name" owner s; return none

/-- Provenance: the selection range must spell this declaration, not its generated parent.
    Fresh source compilation emits selection ranges for authored declarations. Generated
    proof/equation auxiliaries have no source selection (or inherit a different name).
    vacuity_driver.py compiles the sources afresh, including their range extension;
    no pre-existing (possibly stripped) environment is accepted. -/
def authored (n : Name) : MetaM (Option Bool) := do
  if (← getProjectionFnInfo? n).isSome then return some false
  let some r ← findDeclarationRanges? n | return some false
  let env ← getEnv
  let path ← match env.getModuleIdxFor? n with
    | some i => pure ((toString env.header.moduleNames[i.toNat]!).replace "." "/" ++ ".lean")
    | none => pure (← getFileName)
  let src ← IO.FS.readFile path
  let lines := src.splitOn "\n"
  let a := r.selectionRange.pos
  let b := r.selectionRange.endPos
  if a.line != b.line then return some false
  let line := (lines[a.line - 1]!).toList
  let token := String.ofList ((line.drop a.column).take (b.column - a.column))
  let .ok stx := Parser.runParserCategory env `term token | return some false
  unless stx.isIdent do return some false
  let selected := stx.getId
  let root := Name.mkSimple "_root_"
  let user := privateToUserName n
  return some (if root.isPrefixOf selected then user == selected.replacePrefix root .anonymous
    else selected.isSuffixOf user)

/-- Only the proof head and constructor fields are inspected. Each normalization spends a
    separate explicit unit; the Meta heartbeat additionally bounds whnfR itself. -/
partial def direct (n : Name) (e : Expr) (fuel : Nat) : Audit Bool := do
  if fuel == 0 then emit "INCOMPLETE" "projection-budget" n; return false
  let e := e.consumeMData
  if e.getAppFn.isFVar then return true
  if e.getAppFn matches .proj .. then return true
  match e with
  | .lam _ _ _ _ => lambdaTelescope e fun _ body => direct n body (fuel - 1)
  | .letE _ _ v b _ => direct n (b.instantiate1 v) (fuel - 1)
  | .fvar _ => return true
  | .proj .. => return true
  | _ =>
    if let .const h _ := e.getAppFn then
      if (← getProjectionFnInfo? h).isSome then return true
      if [``Eq.refl, ``rfl, ``HEq.refl, ``Iff.refl].contains h then return true
      if let some (.ctorInfo c) := (← getEnv).find? h then
        let mut all := true
        let mut hasProof := false
        for arg in e.getAppArgs[c.numParams:] do
          if ← isProof arg then
            hasProof := true
            if !(← direct n arg (fuel - 1)) then all := false
        return hasProof && all
    let reduced ← withTransparency .reducible (whnfR e)
    if reduced != e then direct n reduced (fuel - 1) else return false

partial def falseDischarge (e : Expr) : MetaM Bool := do
  if let .const n _ := e.getAppFn then
    if [``False.elim, ``absurd].contains n then return true
    if let some matcher ← getMatcherInfo? n then
      if matcher.numAlts == 0 then return true
    if let some (.recInfo r) := (← getEnv).find? n then
      if r.rules.isEmpty then return true
  match e with
  | .app f a => return (← falseDischarge f) || (← falseDischarge a)
  | .lam _ t b _ | .forallE _ t b _ => return (← falseDischarge t) || (← falseDischarge b)
  | .letE _ t v b _ => return (← falseDischarge t) || (← falseDischarge v) || (← falseDischarge b)
  | .mdata _ e | .proj _ _ e => falseDischarge e
  | _ => return false

/-- Traverse definitions only along dependencies of the semantic subject. Definitions unrelated
    to S keep their original syntax. A shared fuel bounds the entire dependency search. -/
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

partial def mutate (e : Expr) (subject mutant : Name) : StateRefT Nat MetaM Expr := do
  let fuel ← get
  if fuel == 0 then throwError "subject-budget"
  modify (· - 1)
  match e with
  | .const n us =>
    if n == subject then return .const mutant us
    if let some (.defnInfo d) := (← getEnv).find? n then
      let (occurs, _) ← (mentions d.value subject).run (20000, {})
      if occurs then
        return ← mutate (d.value.instantiateLevelParams d.levelParams us) subject mutant
    return e
  | .app f a => return mkApp (← mutate f subject mutant) (← mutate a subject mutant)
  | .lam n t b bi => return .lam n (← mutate t subject mutant) (← mutate b subject mutant) bi
  | .forallE n t b bi => return .forallE n (← mutate t subject mutant) (← mutate b subject mutant) bi
  | .letE n t v b nd => return .letE n (← mutate t subject mutant) (← mutate v subject mutant) (← mutate b subject mutant) nd
  | .mdata md b => return .mdata md (← mutate b subject mutant)
  | .proj n i b => return .proj n i (← mutate b subject mutant)
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
def counterexample (law : ConstantInfo) (ic : InstanceCase) (s m : Name) : MetaM Expr := do
  let mut original := law.type.instantiateLevelParams law.levelParams ic.levels
  let (changed, _) ← (mutate original s m).run 20000
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

def approval (t : Tagged) : Audit Unit := do
  let some ref := between t.doc "(approved: " ")" | fail "missing-approval" t.name; return
  let reference := ref.trimAscii.toString.toLower
  if ["", "pending", "todo", "tbd"].contains reference then fail "unresolved-approval" t.name ref
  if reference == "self" then fail "self-approved" t.name ref
  let writer := between t.doc "(writer: " ")"
  let reviewer := between t.doc "(reviewer: " ")"
  -- References must bind an independent reviewer, rather than trusting a bare approval word.
  match writer, reviewer with
  | some w, some r =>
    let w := w.trimAscii.toString.toLower
    let r := r.trimAscii.toString.toLower
    if w == "" || r == "" then fail "approval-identity-missing" t.name
    else if w == r then fail "self-approved" t.name w
  | _, _ => fail "approval-identity-missing" t.name "need (writer: identity) and (reviewer: identity)"

def check (population : List (Name × ConstantInfo)) : MetaM (Array Finding) := do
  let env ← getEnv
  let all := env.constants.map₁.toList ++ env.constants.map₂.toList
  let scope := (← IO.getEnv "VACUITY_SCOPE").getD ""
  let budget := ((← IO.getEnv "VACUITY_BUDGET").getD "64").toNat?.getD 64
  let roadmap ← IO.FS.readFile "ROADMAP.md"
  let run : Audit Unit := do
    let mut tags : Array Tagged := #[]
    let mut hasAuthored := false
    for (n, ci) in population do
      let module := (env.getModuleIdxFor? n).map fun i => toString env.header.moduleNames[i.toNat]!
      if scope != "" && module != some scope && !(toString n).startsWith "ConcreteSpecControl." then continue
      let doc := (← findDocString? env n).getD ""
      let isExample := ((toString n).splitOn "⟪example⟫").length > 1
      if ci.isTheorem then
        match ← authored n with
        | none => emit "INCOMPLETE" "missing-provenance" n
        | some false =>
          unless (doc.splitOn "Obligation (").length > 1 do continue
        | some true =>
          hasAuthored := true
          let classes := ["Law (subject ", "Helper (", "Representation lemma.", "Obligation ("]
          let count := classes.foldl (fun k c => k + (doc.splitOn c).length - 1) 0
          if count != 1 then fail "classification-count" n (toString count)
      let annotated := ["Law (subject ", "Helper (", "Representation lemma.", "Obligation (",
        "Instance (", "Planted (", "Exemption", "Exempt:", "Manual-audit", "Manual audit", "manual-audit"].any
        fun tag => (doc.splitOn tag).length > 1
      if !ci.isTheorem && !isExample && !annotated then continue
      if !ci.isTheorem && ((doc.splitOn "Helper (").length > 1 || (doc.splitOn "Representation lemma.").length > 1) then
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
        let reason := ((between doc ")." "(approved:").getD "").trimAscii.toString
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
        let some row := between doc ", R-" ")" | fail "obligation-roadmap" n; continue
        unless (roadmap.splitOn ("| R-" ++ row ++ " |")).length > 1 do fail "obligation-roadmap" n ("R-" ++ row)
    unless hasAuthored do fail "empty-authored-scope" .anonymous scope
    let mut evidence : Array Tagged := #[]
    for t in tags do
      for tag in ["Instance (", "Planted ("] do
        for part in (t.doc.splitOn tag).drop 1 do
          let doc := tag ++ (part.splitOn ").").head! ++ ")."
          evidence := evidence.push { t with doc := doc }
    for t in evidence do
      unless ((toString t.name).splitOn "⟪example⟫").length > 1 do
        fail "evidence-not-example" t.name
      let some lt := field t.doc "law" | fail "evidence-tag" t.name; continue
      let some target ← resolve all t.name lt | continue
      let candidate := tags.find? (·.name == target)
      match candidate with
      | some l =>
        unless l.info.isTheorem && ((l.doc.splitOn "Law (subject ").length > 1 || l.doc == "") do
          fail "evidence-law-kind" t.name lt
      | none => fail "evidence-law-kind" t.name lt
      if (t.doc.splitOn "Planted (").length > 1 then
        let subjects := tags.filter fun l => l.name == target && (l.doc.splitOn "Law (subject ").length > 1
        if subjects.isEmpty then continue
        let some st := field t.doc "subject" | fail "planted-tag" t.name; continue
        unless subjects.any (fun l => (quoted ((between l.doc "Law (subject " ").").getD "")).contains st) do
          fail "planted-subject" t.name st
    let mut instances : Array InstanceCase := #[]
    for t in evidence do
      if !(t.doc.splitOn "Instance (").length > 1 then continue
      let some lawText := field t.doc "law" | fail "instance-tag" t.name; continue
      let some law ← resolve all t.name lawText | continue
      let some key := field t.doc "case" | fail "instance-tag" t.name; continue
      let some value := t.info.value? | fail "instance-application" t.name; continue
      let value := value.consumeMData
      let .const head levels := value.getAppFn | fail "instance-application" t.name; continue
      unless head == law && closed value && closed t.info.type && t.info.levelParams.isEmpty do
        fail "instance-application" t.name; continue
      let args := value.getAppArgs
      let lawInfo := (env.find? law).get!
      let mut ty := lawInfo.type.instantiateLevelParams lawInfo.levelParams levels
      let mut valid := true
      for arg in args do
        ty ← whnfD ty
        if let .forallE _ _ body _ := ty then ty := body.instantiate1 arg
        else valid := false
      let finalType ← whnfD ty
      if finalType matches .forallE .. then valid := false
      unless valid && (← isDefEq ty t.info.type) do fail "instance-application" t.name; continue
      if ← falseDischarge value then
        fail "instance-false-discharge" t.name
        continue
      if instances.any (fun i => i.law == law && i.key == key) then fail "duplicate-instance" t.name
      instances := instances.push ⟨t.name, law, key, args, levels⟩
    for p in evidence do
      if (p.doc.splitOn "Planted (").length > 1 then
        unless instances.any (fun i => field p.doc "law" == some (toString i.law) && field p.doc "case" == some i.key) do
          fail "unpaired-planted" p.name
    for law in tags do
      unless (law.doc.splitOn "Law (subject ").length > 1 do continue
      unless law.info.isTheorem do fail "law-kind" law.name; continue
      if let some proof := law.info.value? (allowOpaque := true) then
        if ← direct law.name proof budget then fail "projection-law" law.name
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
          let planted := evidence.filter fun t => field t.doc "law" == some (toString law.name) &&
            field t.doc "case" == some ic.key && field t.doc "subject" == some st &&
            (t.doc.splitOn "Planted (").length > 1
          if planted.isEmpty then fail "missing-planted" law.name (ic.key ++ "/" ++ st)
          for p in planted do
            let some mt := field p.doc "mutant" | fail "planted-tag" p.name; continue
            let some mutant ← resolve all p.name mt | continue
            let mi := (env.find? mutant).get!
            unless mutant != subject && (mi matches .defnInfo _) &&
              mi.levelParams.length == ((env.find? subject).get!).levelParams.length do
              fail "mutant-kind" p.name; continue
            let si := (env.find? subject).get!
            let mt := mi.type.instantiateLevelParams mi.levelParams (si.levelParams.map Level.param)
            unless ← isDefEq si.type mt do fail "mutant-type" p.name; continue
            let expected ← counterexample law.info ic subject mutant
            Lean.Meta.check expected
            unless closed p.info.type && (← isDefEq p.info.type expected) do
              fail "planted-proposition" p.name s!"law={law.name} case={ic.key}"
  let safe : Audit Unit := do
    try run
    catch e => emit "INCOMPLETE" "analysis-budget-or-error" .anonymous (← e.toMessageData.toString)
  let (_, findings) ← safe.run #[]
  return findings

end Vacuity
