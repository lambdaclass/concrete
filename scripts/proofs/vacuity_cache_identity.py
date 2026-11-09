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
  let variants := #[
    mkForall `x .implicit (mkConst ``Nat) (mkConst ``Nat),
    mkForall `renamed .default (mkConst ``Nat) (mkConst ``Nat),
    mkForall `x .default (mkSort (.succ .zero)) (mkConst ``Nat)]
  for variant in variants do
    let (_, graph) := (do
      let a ← Discharge.internTerm base
      let b ← Discharge.internTerm variant
      if a == b then panic! "interning-collapsed-syntax").run ({} : Discharge.Graph)
    unless graph.terms.size == 2 do throwError "interning-collapsed-syntax"
    let budget ← IO.mkRef ({} : AnalysisUsage)
    let (a, _) ← (mutate base `absent `alsoAbsent #[] (some budget)).run 0
    let (b, _) ← (mutate variant `absent `alsoAbsent #[] (some budget)).run 0
    unless Expr.equal a base && Expr.equal b variant do throwError "mutation-collapsed-syntax"
    let ((_, projection), _) ← ((do
      discard <| direct `absent base
      discard <| direct `absent variant).run { remaining := 64 }).run {}
    unless projection.answers.size == 2 do throwError "projection-collapsed-syntax"
  IO.println "KEYS interning mutation projection binder-info binder-name universe distinct"
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
  unless !bad && state.refutedContexts.isEmpty && state.refutedTypes.isEmpty do
    throwError "eager-harmless-refutation"
  IO.println "DEMAND harmless-body searches=0"

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
        'KEYS interning mutation projection binder-info binder-name universe distinct',
        'KEYS options mode distinct',
        'MODULES import-order checked forward=false backward=true self=true',
        'DEMAND harmless-body searches=0',
        'KEYS declaration-name binder-info kind distinct',
    ]
    assert [r.rstrip() for r in result.stdout.splitlines()] == expected, result.stdout
    for line in expected: print('PASS identity ' + line, flush=True)
    return 0

if __name__ == '__main__':
    with compiled_consumer() as meta:
        raise SystemExit(controls(meta))
