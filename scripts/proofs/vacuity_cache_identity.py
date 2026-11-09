"""Binder-sensitive refutation regressions from N15a; exact expected results."""
import os
import tempfile
from pathlib import Path
from vacuity_driver import compiled_consumer, command
D=r'''import N2Meta
open Lean Meta Vacuity
class Box : Type where val : Nat
axiom boxOp : Box → Box
axiom boxA : Box
axiom boxB : Box
axiom suppliedFalse : False
def H1 : Prop := (heq : boxOp boxA = boxB) → (f : (x : Box) → Nat → Prop) → (h1 : f (boxOp boxA) 1) → (h2 : ¬ f boxB 1) → True
def H2 : Prop := (heq : boxOp boxA = boxB) → (f : [Box] → Nat → Prop) → (h1 : @f (boxOp boxA) 1) → (h2 : ¬ @f boxB 1) → True
theorem b1 : H1 := fun _ _ _ _ => id (False.elim suppliedFalse)
theorem b2 : H2 := fun _ _ _ _ => False.elim suppliedFalse
#eval show MetaM Unit from do
  for order in [[`b1, `b2], [`b2, `b1]] do
    let mut state : DischargeState := {}
    for n in order do
      let (bad, next) ← (falseDischarge (mkConst n)).run state
      state := next
      IO.println s!"ORDER {order} {n} bad={bad}"
  forallTelescopeReducing (mkConst `H1) fun xs _ => do
    let (before, state) ← (dischargeRefutableContext true).run {}
    let typ ← forallTelescopeReducing (mkConst `H2) fun ys _ => inferType ys[1]!
    let changed := (← getLCtx).setType xs[1]!.fvarId! typ
    let (after, _) ← withFreshCache <| withLCtx changed (← getLocalInstances) <| (dischargeRefutableContext true).run state
    let (fresh, _) ← withFreshCache <| withLCtx changed (← getLocalInstances) <| (dischargeRefutableContext true).run {}
    IO.println s!"SETTYPE before={before} after={after} fresh={fresh}"

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
  IO.println "KEYS interning mutation binder-info binder-name universe distinct"
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
'''
def controls(meta):
    with tempfile.TemporaryDirectory(prefix='n2-cache-identity-') as temp:
        path = Path(temp) / 'Identity.lean'
        path.write_text(D)
        result = command(['lean', path], temp, dict(os.environ, LEAN_PATH=str(meta)), seconds=60)
    assert result.returncode == 0, result.stdout
    expected = [
        'ORDER [b1, b2] b1 bad=false', 'ORDER [b1, b2] b2 bad=true',
        'ORDER [b2, b1] b2 bad=true', 'ORDER [b2, b1] b1 bad=false',
        'SETTYPE before=true after=false fresh=false',
        'KEYS interning mutation binder-info binder-name universe distinct',
        'KEYS options mode distinct',
        'MODULES import-order checked forward=false backward=true self=true',
    ]
    assert result.stdout.splitlines() == expected, result.stdout
    for line in expected: print('PASS identity ' + line, flush=True)
    return 0

if __name__ == '__main__':
    with compiled_consumer() as meta:
        raise SystemExit(controls(meta))
