"""Focused body-access inventory controls, including computational/proof separation."""
import functools
import os
import tempfile
from pathlib import Path

from vacuity_controls import controls
from vacuity_driver import ROOT, compiled_consumer, command

DRIVER = r'''import N2Meta
open Lean Meta Vacuity
namespace BodyAccess
def subject (n : Nat) := n+1
def mutant (n : Nat) := n
/-- Proof-only references are not computational subject occurrences. -/
theorem selfEq (n : Nat) : subject n = subject n := rfl
set_option maxHeartbeats 2000000 in
#eval show MetaM Unit from do
  let budget ← IO.mkRef ({} : AnalysisUsage)
  for _ in [:2] do
    let (occurs, _) ← (mentions (mkConst ``selfEq) ``subject (some budget)).run (0, {})
    if occurs then throwError "proof-reference-became-computational"
  unless (← budget.get).mentions[(mkConst ``selfEq, ``subject)]? == some false do
    throwError "computational-negative-not-cached"
  IO.println "BODY mentionsVisit/cache: proof-only subject absent (twice)"
  let proof := mkApp (mkConst ``selfEq) (mkNatLit 2)
  let type ← inferType proof
  let proposition := Expr.letE `h type proof type false
  Lean.Meta.check proposition
  let job : AnalysisJob := {
    name := `BodyAccess.mutation, stage := "planted"
    action := do
      let (changed, _) ← (mutate proposition ``subject ``mutant #[] (← get).budget).run 0
      Lean.Meta.check changed
  }
  let (_, state) ← (isolated (← readThe Core.Context) job).run {}
  for f in state.findings do IO.println s!"{f.status} {f.code} {f.name} {f.detail}"
  unless state.findings.any (fun f => f.status == "INCOMPLETE" && f.code == "analysis-budget-or-error") do
    throwError "ill-typed-mutated-proof-not-incomplete"
  -- Template compilation has explicit raw theorem access, independent of conversion.
  let key : Discharge.TemplateKey := { name := ``selfEq, levels := [], mode := .body }
  let (_, solver) ← (Discharge.compileTemplate key).run {}
  let template := solver.graph.templates[key]!
  unless Expr.equal solver.graph.terms[template.root]! ((← getConstInfo ``selfEq).value? (allowOpaque := true)).get! do
    throwError "template-skipped-theorem-body"
  IO.println "BODY compileTemplate: raw theorem body retained"
set_option maxHeartbeats 2000000 in
#eval show MetaM Unit from do
  for label in ["illTyped", "typeSubject"] do
    let job : AnalysisJob := {
      name := Name.str `BodyAccess label, stage := "instance"
      action := do
        if label == "illTyped" then
          let bad := mkApp (mkConst ``Nat.succ) (mkConst ``True.intro)
          discard <| protectedEq bad bad #[] (← get).budget
        else
          discard <| protectedEq (mkConst ``Nat) (mkConst ``Nat) #[``Nat] (← get).budget
    }
    let (_, state) ← (isolated (← readThe Core.Context) job).run {}
    for f in state.findings do IO.println s!"{f.status} {f.code} {f.name} {f.detail}"
    unless state.findings.any (fun f => f.status == "INCOMPLETE" && f.code == "analysis-budget-or-error") do
      throwError "unsupported-protection-not-incomplete"
end BodyAccess
'''


def main():
    from vacuity_driver import run
    labels = {"hidden-law-body", "hidden-planted-body", "obligation-theorem",
              "unclassified", "unclassified-opaque", "opaque-helper-positive",
              "false-discharge", "helper-false", "projection-helper-def",
              "projection-helper-theorem", "projection-helper-opaque", "projection-unavailable"}
    with compiled_consumer() as meta:
        if controls(functools.partial(run, meta_path=meta), ROOT, labels=labels):
            return 1
        from vacuity_controls import BASE
        approved = "/-- Helper (computation). Inventory control (approved: independent:confirm). -/\n"
        hidden = BASE.replace("/-- Law", "axiom impossible : False\n" + approved + "theorem hidden : 2 = 2 := False.elim impossible\n/-- Law", 1).replace("growth 2 rfl", "growth 2 hidden")
        theorem_instance = BASE.replace("example : 2 < S 2", "theorem instanceTwo : 2 < S 2", 1)
        theorem_subject = ("namespace ConcreteSpecControl\n" + approved + "theorem S : True := trivial\n"
            + "/-- Law (subject `ConcreteSpecControl.S`). -/\ntheorem growth : 2 < 3 := by decide\nend ConcreteSpecControl\n")
        proof_only = ("namespace ConcreteSpecControl\ndef S (n : Nat) := n+1\n" + approved +
            "theorem selfEq (n : Nat) : True := by have : S n = S n := rfl; trivial\n" +
            "/-- Law (subject `ConcreteSpecControl.S`). -/\ntheorem growth (n : Nat) : (let _ := selfEq n; n < n+1) := Nat.lt_succ_self n\nend ConcreteSpecControl\n")
        theorem_mutant = BASE.replace("def mutant (n : Nat) : Nat := n", approved + "theorem mutant : True := trivial").replace("example : 2 = 2 ∧ ¬ 2 < mutant 2 := by decide", "example : 2 = 2 := rfl")
        for label, fixture, code, name in [
            ("theorem-instance", theorem_instance, "instance-application", "ConcreteSpecControl.instanceTwo"),
            ("hidden-false-discharge", hidden, "instance-false-discharge", None),
            ("theorem-subject", theorem_subject, "subject-kind", "ConcreteSpecControl.growth"),
            ("subject-proof-only", proof_only, "subject-absent", "ConcreteSpecControl.growth"),
            ("theorem-mutant", theorem_mutant, "mutant-kind", None),
        ]:
            rc, output = run(ROOT, 'ConcreteSpec', fixture=fixture, meta_path=meta)
            print(f"BODY {label} exit={rc}\n{output}", end='', flush=True)
            assert rc == 1 and 'N2 FAIL' in output and 'INCOMPLETE ' not in output, output
            if name is None:
                line = next(i for i, row in enumerate(fixture.splitlines(), 1)
                            if row.startswith('example :') and
                            ('Instance (' if label == 'hidden-false-discharge' else 'Planted (') in fixture.splitlines()[i-2])
                name = f'ConcreteSpecControl.«⟪example⟫ConcreteSpec.lean:{line}»'
            detail = ('law=ConcreteSpecControl.growth case=two' if code == 'instance-application'
                      else 'ConcreteSpecControl.S' if code in {'subject-kind', 'subject-absent'} else '')
            exact = f'FAIL {code} {name} {detail}'.rstrip()
            assert exact in [r.rstrip() for r in output.splitlines()], (exact, output)
        with tempfile.TemporaryDirectory(prefix="n2-body-access-") as temp:
            path = Path(temp) / 'Body.lean'
            path.write_text(DRIVER)
            result = command(['lean', path], temp, dict(os.environ, LEAN_PATH=str(meta)), seconds=60)
            print(result.stdout, end='', flush=True)
            assert result.returncode == 0, result.stdout
            assert 'invalid let declaration' in result.stdout and 'selfEq 2' in result.stdout, result.stdout
            assert 'BODY compileTemplate: raw theorem body retained' in result.stdout, result.stdout
            assert 'DETAIL analysis-error BodyAccess.typeSubject stage=instance law=none case=none protected-type-valued-subject' in result.stdout, result.stdout
            assert 'DETAIL analysis-error BodyAccess.illTyped stage=instance law=none case=none Application type mismatch:' in result.stdout, result.stdout
            assert 'True.intro' in result.stdout and 'Nat' in result.stdout, result.stdout
        from vacuity_certificates import DRIVER as CERTIFICATE_DRIVER
        source = ('import N2Meta\naxiom impossible : False\n'
                  'theorem hiddenCertificate : ¬ False := False.elim impossible\n' + CERTIFICATE_DRIVER +
                  '\n#eval show MetaM Unit from do\n'
                  '  probe "hidden-theorem" (mkConst ``hiddenCertificate) "rejected-supplied-contradiction supplier=impossible"\n')
        with tempfile.TemporaryDirectory(prefix="n2-body-certificate-") as temp:
            path = Path(temp) / 'Certificate.lean'
            path.write_text(source)
            result = command(['lean', path], temp, dict(os.environ, LEAN_PATH=str(meta)), seconds=60)
            print(result.stdout, end='', flush=True)
            assert result.returncode == 0, result.stdout
            assert 'CERTIFICATE hidden-theorem: rejected-supplied-contradiction supplier=impossible' in result.stdout, result.stdout
    print('Body-access controls: PASS', flush=True)
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
