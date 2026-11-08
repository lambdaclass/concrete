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

abbrev Audit := StateRefT (Array Finding) MetaM

def emit (status code : String) (n : Name) (detail : String := "") : Audit Unit :=
  modify (·.push ⟨status, code, n, detail⟩)

def fail (code : String) (n : Name) (detail : String := "") : Audit Unit := emit "FAIL" code n detail

def between (s left right : String) : Option String := do
  let tail ← (s.splitOn left)[1]?
  return (tail.splitOn right).head!

def quoted (s : String) : Array String :=
  ((s.splitOn "`").zipIdx.filterMap fun (p, i) => if i % 2 == 1 then some p else none).toArray

def field (doc key : String) : Option String := between doc (key ++ " `") "`"

def closed (e : Expr) : Bool :=
  !e.hasFVar && !e.hasMVar && !e.hasLooseBVars &&
    !e.hasLevelParam && !e.hasLevelMVar

def resolve (all : List (Name × ConstantInfo)) (owner : Name) (s : String) : Audit (Option Name) := do
  let hits := all.filter fun (n, _) => toString n == s || toString (privateToUserName n) == s ||
    toString (privateToUserName n) == toString owner.getPrefix ++ "." ++ s
  match hits with
  | [(n, _)] => return some n
  | [] => fail "unknown-name" owner s; return none
  | _ => fail "ambiguous-name" owner s; return none

/-- Provenance: the selection range must spell this declaration, not its generated parent.
    An absent range is incomplete, never permission to drop a theorem. -/
def authored (n : Name) : MetaM (Option Bool) := do
  if (← getProjectionFnInfo? n).isSome then return some false
  let some r ← findDeclarationRanges? n | return none
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
  let user := toString (privateToUserName n)
  return some (user == token || user.endsWith ("." ++ token) ||
    user.endsWith ("." ++ String.ofList (token.toList.drop 1 |>.reverse.drop 1 |>.reverse)))

/-- Only the proof head and constructor fields are inspected. Each normalization spends a
    separate explicit unit; the Meta heartbeat additionally bounds whnfR itself. -/
partial def direct (n : Name) (e : Expr) (fuel : Nat) : Audit Bool := do
  if fuel == 0 then emit "INCOMPLETE" "projection-budget" n; return false
  let e := e.consumeMData
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
        for arg in e.getAppArgs[c.numParams:] do
          if ← isProof arg then
            if !(← direct n arg (fuel - 1)) then all := false
          else if !arg.isFVar then all := false
        return all
    let reduced ← withTransparency .reducible (whnfR e)
    if reduced != e then direct n reduced (fuel - 1) else return false

partial def falseDischarge (e : Expr) : MetaM Bool := do
  if let .const n _ := e.getAppFn then
    if [``False.elim, ``absurd].contains n then return true
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

/-- Consume the written telescope, without unfolding the conclusion: a law may conclude a
    defined universally quantified contract. Proof arguments are hypotheses; other arguments
    are parameters. The original telescope determines this split before mutation. -/
def counterexample (law : ConstantInfo) (ic : InstanceCase) (s m : Name) : MetaM Expr := do
  let mut original := law.type.instantiateLevelParams law.levelParams ic.levels
  let (changed, _) ← (mutate original s m).run 20000
  let mut mutated := changed
  let mut hyps : Array Expr := #[]
  for arg in ic.args do
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
  if ref.trimAscii.toString == "" || ref == "PENDING" then fail "unresolved-approval" t.name ref
  let writer := between t.doc "(writer: " ")"
  let reviewer := between t.doc "(reviewer: " ")"
  -- References must bind an independent reviewer, rather than trusting a bare approval word.
  match writer, reviewer with
  | some w, some r => if w == r then fail "self-approved" t.name w
  | _, _ => fail "approval-identity-missing" t.name "need (writer: identity) and (reviewer: identity)"

def check (population : List (Name × ConstantInfo)) : MetaM (Array Finding) := do
  let env ← getEnv
  let all := env.constants.map₁.toList ++ env.constants.map₂.toList
  let scope := (← IO.getEnv "VACUITY_SCOPE").getD ""
  let budget := ((← IO.getEnv "VACUITY_BUDGET").getD "64").toNat?.getD 64
  let roadmap ← IO.FS.readFile "ROADMAP.md"
  let run : Audit Unit := do
    let mut tags : Array Tagged := #[]
    for (n, ci) in population do
      let module := (env.getModuleIdxFor? n).map fun i => toString env.header.moduleNames[i.toNat]!
      if scope != "" && module != some scope && !(toString n).startsWith "ConcreteSpecControl." then continue
      let doc := (← findDocString? env n).getD ""
      let isExample := ((toString n).splitOn "⟪example⟫").length > 1
      if ci.isTheorem then
        match ← authored n with
        | none => emit "INCOMPLETE" "missing-provenance" n
        | some false => continue
        | some true =>
          let classes := ["Law (subject ", "Helper (", "Representation lemma.", "Obligation ("]
          let count := classes.foldl (fun k c => k + (doc.splitOn c).length - 1) 0
          if count != 1 then fail "classification-count" n (toString count)
      if !ci.isTheorem && !isExample && !((doc.splitOn "Obligation (").length > 1) then continue
      tags := tags.push ⟨n, ci, doc⟩
      -- Cases are labels, not declarations. Every other backtick resolves, including prose.
      for q in quoted doc do
        if field doc "case" == some q then continue
        discard <| resolve all n q
      for label in ["Helper (", "Representation lemma.", "Exemption", "Manual-audit", "Manual audit"] do
        if (doc.splitOn label).length > 1 then
          emit "REPORT" label n doc
          if label != "Manual-audit" && label != "Manual audit" then approval ⟨n, ci, doc⟩
      if (doc.splitOn "Obligation (").length > 1 then
        emit "REPORT" "Obligation" n doc
        let propDef ← match ci with
          | .defnInfo _ => isDefEq ci.type (mkSort .zero)
          | _ => pure false
        let projection := (← getProjectionFnInfo? n).isSome
        unless propDef || (projection && (← forallTelescopeReducing ci.type fun _ t => isDefEq t (mkSort .zero))) do
          fail "obligation-kind" n
        let some row := between doc ", R-" ")" | fail "obligation-roadmap" n; continue
        unless (roadmap.splitOn ("| R-" ++ row ++ " |")).length > 1 do fail "obligation-roadmap" n ("R-" ++ row)
    let mut instances : Array InstanceCase := #[]
    for t in tags do
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
        if let .forallE _ _ body _ := ty then ty := body.instantiate1 arg
        else valid := false
      if ty matches .forallE .. then valid := false
      unless valid && (← isDefEq ty t.info.type) do fail "instance-application" t.name; continue
      if ← falseDischarge value then fail "instance-false-discharge" t.name
      if instances.any (fun i => i.law == law && i.key == key) then fail "duplicate-instance" t.name
      instances := instances.push ⟨t.name, law, key, args, levels⟩
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
          let planted := tags.filter fun t => field t.doc "law" == some (toString law.name) &&
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
            unless closed p.info.type && (← isDefEq p.info.type expected) do
              fail "planted-proposition" p.name s!"law={law.name} case={ic.key}"
  let (_, findings) ← run.run #[]
  return findings

end Vacuity
