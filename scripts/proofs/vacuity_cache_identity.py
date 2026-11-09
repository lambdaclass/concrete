"""Binder-sensitive refutation regressions from N15a; exact expected results."""
import os
import tempfile
from pathlib import Path
from vacuity_driver import compiled_consumer, command
D=r'''import N2Meta
open Lean Meta Vacuity
set_option linter.defProp false
class Box : Type where val : Nat
axiom boxOp : Box → Box
axiom boxA : Box
axiom boxB : Box
def cacheSubject : Nat := 0
axiom suppliedFalse : False
def H1 : Prop := (heq : boxOp boxA = boxB) → (f : (x : Box) → Nat → Prop) → (h1 : f (boxOp boxA) 1) → (h2 : ¬ f boxB 1) → True
def H2 : Prop := (heq : boxOp boxA = boxB) → (f : [Box] → Nat → Prop) → (h1 : @f (boxOp boxA) 1) → (h2 : ¬ @f boxB 1) → True
theorem b1 : H1 := fun _ _ _ _ => id (False.elim suppliedFalse)
theorem b2 : H2 := fun _ _ _ _ => False.elim suppliedFalse
theorem b3 : H1 := fun _ _ _ _ => False.elim suppliedFalse
theorem forward (_a : H1) (_b : H2) : True := True.intro
theorem reverse (_a : H2) (_b : H1) : True := True.intro
def witnessForward : True := forward b1 b2
def witnessReverse : True := reverse b2 b1
#eval show MetaM Unit from do
  let env ← getEnv
  for (name, law) in [(`witnessForward, `forward), (`witnessReverse, `reverse)] do
    let (_, state) ← (checkInstance env [] #[]
      { name, info := (← getConstInfo name), doc := "", law, key := "binder" } #[]).run {}
    for f in state.findings do IO.println s!"PIPE {f.status} {f.code} {f.name} {f.detail}"
#eval show MetaM Unit from do
  for order in [[`b1, `b2], [`b2, `b1]] do
    let mut state : DischargeState := {}
    for n in order do
      let (bad, next) ← (falseDischarge (mkConst n)).run state
      state := next
      IO.println s!"ORDER {order} {n} bad={bad}"
  for order in [[`b3, `b2], [`b2, `b3]] do
    let mut state : DischargeState := {}
    for n in order do
      let value := ((← getConstInfo n).value? (allowOpaque := true)).get!
      let (bad, next) ← (falseDischarge value).run state
      state := next
      IO.println s!"ROOT {order} {n} bad={bad}"
  forallTelescopeReducing (mkConst `H1) fun xs _ => do
    let (before, state) ← (dischargeRefutableContext true).run {}
    let typ ← forallTelescopeReducing (mkConst `H2) fun ys _ => inferType ys[1]!
    let changed := (← getLCtx).setType xs[1]!.fvarId! typ
    let (after, _) ← withFreshCache <| withLCtx changed (← getLocalInstances) <| (dischargeRefutableContext true).run state
    let (fresh, _) ← withFreshCache <| withLCtx changed (← getLocalInstances) <| (dischargeRefutableContext true).run {}
    IO.println s!"SETTYPE before={before} after={after} fresh={fresh}"
    let e := mkApp2 (mkConst ``False.elim [.zero]) (mkConst ``True) (mkConst `suppliedFalse)
    let (before, state) ← (falseDischarge e).run {}
    let (after, _) ← withFreshCache <| withLCtx changed (← getLocalInstances) <| (falseDischarge e).run state
    unless !before && after do throwError "visited-context-collapsed"
    IO.println "VISITED setType before=false after=true"

#eval show MetaM Unit from do
  let base := mkForall `x .default (mkConst ``Nat) (mkConst ``Nat)
  let pairs := #[
    (base, mkForall `x .implicit (mkConst ``Nat) (mkConst ``Nat)),
    (base, mkForall `renamed .default (mkConst ``Nat) (mkConst ``Nat)),
    (mkConst ``List [.zero], mkConst ``List [.succ .zero])]
  for (base, variant) in pairs do
    let ((a, b), graph) := (do
      let a ← Discharge.internTerm base
      let b ← Discharge.internTerm variant
      pure (a, b)).run ({} : Discharge.Graph)
    unless a != b && graph.terms.size == 2 do throwError "interning-collapsed-syntax"
    let budget ← IO.mkRef ({} : AnalysisUsage)
    let (a, _) ← (mutate base `absent `alsoAbsent #[] (some budget)).run 0
    let (b, _) ← (mutate variant `absent `alsoAbsent #[] (some budget)).run 0
    unless Expr.equal a base && Expr.equal b variant do throwError "mutation-collapsed-syntax"
    -- Protect a computational value in WELL-TYPED contracts. Nat itself is a
    -- representation type and must no longer be replaced by an arbitrary type.
    let subjectEq ← mkEq (mkConst ``cacheSubject) (mkConst ``cacheSubject)
    let left ← mkAppM ``And #[← mkEq base base, subjectEq]
    let right ← mkAppM ``And #[← mkEq variant variant, subjectEq]
    let separated ← withProtected left right #[``cacheSubject] (fun a b => do
      Lean.Meta.check a
      Lean.Meta.check b
      if a.getUsedConstants.contains ``cacheSubject || b.getUsedConstants.contains ``cacheSubject then
        throwError "replacement-did-not-abstract-subject"
      pure (!Expr.equal a b)) (some budget)
    unless separated do throwError "replacement-collapsed-syntax"
    let ((_, projection), _) ← ((do
      discard <| direct `absent base
      discard <| direct `absent variant).run { remaining := 64 }).run {}
    unless projection.answers.size == 2 do throwError "projection-collapsed-syntax"
  IO.println "KEYS interning mutation replacement projection binder-info binder-name universe distinct"
#eval show MetaM Unit from do
  let a ← dischargeRawContext
  let b ← withOptions (fun o => o.setBool `smartUnfolding false) dischargeRawContext
  if a == b then throwError "options-collapsed"
  let c ← withTransparency .all dischargeRawContext
  if a == c then throwError "mode-collapsed"
  IO.println "KEYS options mode distinct"

#eval show MetaM Unit from do
  let env ← getEnv
  -- Pin the importer contract used by the cheap forward-reference rejection.
  for i in [:env.header.moduleData.size] do
    for imported in env.header.moduleData[i]!.imports do
      if let some j := env.getModuleIdx? imported.module then
        unless j.toNat < i do throwError "module-import-order"
  let budget ← IO.mkRef ({} : AnalysisUsage)
  let positive ← moduleDepends `Lean `Init (some budget)
  let negative ← moduleDepends `Init `Lean (some budget)
  let self ← moduleDepends `Lean `Lean (some budget)
  unless positive && !negative && self do throwError "module-dependency-direction"
  IO.println "MODULES import-order checked forward=false backward=true self=true"

#eval show MetaM Unit from do
  let harmless := mkLambda `x .default (mkConst ``Nat) (.bvar 0)
  let (bad, state) ← (falseDischarge harmless).run {}
  unless !bad && state.refutedContexts.isEmpty do
    throwError "eager-harmless-refutation"
  IO.println "DEMAND harmless-body context-searches=0"

#eval show MetaM Unit from do
  withLocalDeclD `same (mkConst ``Nat) fun x => do
    let original ← getLCtx
    let raw ← dischargeRawContext
    let key ← dischargeClosure (mkConst ``False)
    for changed in #[original.setUserName x.fvarId! `renamed,
        original.setBinderInfo x.fvarId! .instImplicit,
        original.setKind x.fvarId! .implDetail] do
      withLCtx changed (← getLocalInstances) do
        if (← dischargeRawContext) == raw then throwError "raw-declaration-field-collapsed"
        if (← dischargeClosure (mkConst ``False)) == key then throwError "closed-declaration-field-collapsed"
  IO.println "KEYS declaration-name binder-info kind distinct"

#eval show MetaM Unit from do
  withLocalDeclD `sameProjection (mkConst ``Nat) fun x => do
    let ((_, before), _) ← ((direct `absent x).run { remaining := 64 }).run {}
    let changed := (← getLCtx).setType x.fvarId! (mkConst ``False)
    let ((_, after), _) ← withFreshCache <| withLCtx changed (← getLocalInstances) <|
      ((direct `absent x).run before).run {}
    unless after.answers.size == 2 do throwError "projection-context-collapsed"
  IO.println "KEYS projection same-FVarId context distinct"

#eval show MetaM Unit from do
  let proof := mkConst ``True.intro
  let ((before, state), _) ← ((direct `absent proof).run { remaining := 64 }).run {}
  let changed := { state with origins := { state.origins with premiseOnly := true } }
  let ((after, _), _) ← ((direct `absent proof).run changed).run {}
  unless before && !after do throwError "projection-mode-collapsed"
  IO.println "KEYS projection premise-only mode distinct"

#eval show MetaM Unit from do
  withLocalDeclD `sameOrigin (mkConst ``True) fun x => do
    let ((before, state), _) ← ((direct `absent x).run { remaining := 64 }).run {}
    let changed := { state with origins := { state.origins with minorOrigins := #[(x.fvarId!, false)] } }
    let ((after, _), _) ← ((direct `absent x).run changed).run {}
    unless before && !after do throwError "projection-origin-collapsed"
  IO.println "KEYS projection same-FVarId origin distinct"

#eval show MetaM Unit from do
  withLocalDeclD `sameFieldOrigin (mkConst ``True) fun x => do
    let carrier ← mkAppM ``And.intro #[x, mkConst ``True.intro]
    let ((before, state), _) ← ((directConstructorField `absent ``And.intro 0 carrier).run
      { remaining := 64, origins := { premiseOnly := true } }).run {}
    let changed := { state with origins := { state.origins with minorOrigins := #[(x.fvarId!, false)] } }
    let ((after, _), _) ← ((directConstructorField `absent ``And.intro 0 carrier).run changed).run {}
    unless before && !after do throwError "projection-field-origin-collapsed"
  IO.println "KEYS constructor-field origin distinct"

#eval show MetaM Unit from do
  let tail ← mkArrow (mkConst ``Nat) (mkSort .zero)
  let explicit := mkForall `x .default (mkConst `Box) tail
  let implicit := mkForall `x .instImplicit (mkConst `Box) tail
  let (_, state) ← (dischargeRefutableType explicit).run {}
  let (_, state) ← (dischargeRefutableType implicit).run state
  unless state.rawRefutedTypes.size == 2 && state.refutedTypes.size == 2 do
    throwError "refuted-type-binder-info-collapsed"
  IO.println "KEYS raw-refuted-types closed-refuted-types binder-info distinct"

axiom transportP : Prop
axiom suppliedTransportP : transportP
axiom transportEquality : transportP = False
def transportedFalse : False := Eq.mp transportEquality suppliedTransportP
theorem falseLaw (_h : False) : True := True.intro
def transportWitness : True := falseLaw transportedFalse
#eval show MetaM Unit from do
  let (bad, _) ← (falseDischarge (mkConst `transportedFalse)).run {}
  unless bad do throwError "transport-false-discharge-missed"
  let (_, state) ← (checkInstance (← getEnv) [] #[]
    { name := `transportWitness, info := (← getConstInfo `transportWitness), doc := "", law := `falseLaw, key := "transport" } #[]).run {}
  let some finding := state.findings[0]? | throwError "transport-instance-finding-missed"
  unless state.findings.size == 1 && finding.status == "FAIL" &&
      finding.code == "instance-false-discharge" do
    throwError "transport-instance-finding-missed"
  IO.println "TRANSPORT bad=true exact=FAIL-instance-false-discharge"
'''
def controls(meta):
    with tempfile.TemporaryDirectory(prefix='n2-cache-identity-') as temp:
        path = Path(temp) / 'Identity.lean'
        path.write_text(D)
        result = command(['lean', path], temp, dict(os.environ, LEAN_PATH=str(meta)), seconds=60)
    assert result.returncode == 0, result.stdout
    expected = [
        'PIPE FAIL instance-false-discharge witnessForward',
        'PIPE FAIL instance-false-discharge witnessReverse',
        'ORDER [b1, b2] b1 bad=false', 'ORDER [b1, b2] b2 bad=true',
        'ORDER [b2, b1] b2 bad=true', 'ORDER [b2, b1] b1 bad=false',
        'ROOT [b3, b2] b3 bad=false', 'ROOT [b3, b2] b2 bad=true',
        'ROOT [b2, b3] b2 bad=true', 'ROOT [b2, b3] b3 bad=false',
        'SETTYPE before=true after=false fresh=false',
        'VISITED setType before=false after=true',
        'KEYS interning mutation replacement projection binder-info binder-name universe distinct',
        'KEYS options mode distinct',
        'MODULES import-order checked forward=false backward=true self=true',
        'DEMAND harmless-body context-searches=0',
        'KEYS declaration-name binder-info kind distinct',
        'KEYS projection same-FVarId context distinct',
        'KEYS projection premise-only mode distinct',
        'KEYS projection same-FVarId origin distinct',
        'KEYS constructor-field origin distinct',
        'KEYS raw-refuted-types closed-refuted-types binder-info distinct',
        'TRANSPORT bad=true exact=FAIL-instance-false-discharge',
    ]
    assert [r.rstrip() for r in result.stdout.splitlines()] == expected, result.stdout
    for line in expected: print('PASS identity ' + line, flush=True)
    return 0

def planted_controls():
    from vacuity_driver import HERE
    consumer = (HERE / 'vacuity.lean').read_text()
    ordinary = consumer.replace('local instance auditExprBEq : BEq Expr := ⟨Expr.equal⟩', '')
    visited = consumer.replace('visited : Std.HashSet (Expr × DischargeRawContext)',
                               'visited : Std.HashSet (Expr × Array FVarId)')
    prefix, body = visited.split('partial def falseDischarge', 1)
    visited = prefix + 'partial def falseDischarge' + body.replace('  let key := (e, context)', '  let key := (e, (← getLCtx).getFVarIds)', 1)
    projection_type = 'answers : Std.HashMap (Expr × DischargeRawContext × ProjectionOrigins) Bool'
    projection_key = '  let key := (e, context, (← get).origins)'
    assert consumer.count(projection_type) == consumer.count(projection_key) == 1
    projection = consumer.replace(projection_type, 'answers : Std.HashMap Expr Bool')
    projection = projection.replace(projection_key, '  let key := e', 1)
    missing_type = consumer.replace('  let refuted ← dischargeRefutableTypeAt context (← inferType e)\n  if refuted && !e.hasFVar then return !(← dischargeRefutableContextAt context true)', '  let refuted := false')
    for label, text in [('ordinary-expr-equality', ordinary), ('fvar-only-visited', visited), ('expression-only-projection', projection), ('missing-result-obligation', missing_type)]:
        assert text != consumer
        with compiled_consumer(consumer_text=text) as meta, tempfile.TemporaryDirectory(prefix='n2-identity-plant-') as temp:
            path = Path(temp) / 'Identity.lean'
            path.write_text(D)
            result = command(['lean', path], temp, dict(os.environ, LEAN_PATH=str(meta)), seconds=60)
        if label == 'ordinary-expr-equality':
            # These are the planted unsound answers, not an arbitrary Lean failure.
            assert 'ROOT [b3, b2] b2 bad=false' in result.stdout.splitlines(), result.stdout
            assert 'SETTYPE before=true after=true fresh=false' in result.stdout.splitlines(), result.stdout
        else:
            assert result.returncode == 1, result.stdout
            errors = [line.split('error: ', 1)[1] for line in result.stdout.splitlines() if 'error: ' in line]
            expected = {'fvar-only-visited': ['visited-context-collapsed'],
                        'expression-only-projection': ['projection-context-collapsed', 'projection-mode-collapsed', 'projection-origin-collapsed', 'projection-field-origin-collapsed'],
                        'missing-result-obligation': ['transport-false-discharge-missed']}[label]
            assert errors == expected, result.stdout
        print('PASS identity planted ' + label + ': exact stale-result diagnostic', flush=True)

if __name__ == '__main__':
    with compiled_consumer() as meta:
        controls(meta)
    planted_controls()
