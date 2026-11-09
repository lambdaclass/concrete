"""Bounded N8a baseline observations; these are defect reproductions, not admission controls."""
import argparse
import os
import tempfile
from pathlib import Path
from vacuity_driver import HERE, command, compiled_consumer

DRIVER = r"""
open Lean Meta Vacuity.Discharge
axiom impossible : False
def useImpossibleLet : Empty → False := let _ : Nat := 0; fun _ => impossible
def PickType : Bool → Prop | true => False | false => True
axiom suppliedByBool (b : Bool) : PickType b
def directApp : False := suppliedByBool true
def letApp : False := (let _ : Nat := 0; fun (b : Bool) => suppliedByBool b) true
theorem bad0 : False → False := fun _ => impossible
theorem bad1 : False → False := bad0
theorem cert01 : ¬ (0 = 1) := by decide

def observe (label : String) (proof : Expr) (expected : String)
    (assumptions : Array Expr := #[]) : MetaM Unit := do
  let action : Solver Unit := do
    let mut inputs := #[]
    for x in assumptions do
      let id ← term x
      let type ← term (← inferType x)
      let o ← origin (.hypothetical id id)
      inputs := inputs.push ({ term := id, type, origin := o } : Input)
    let proofId ← term proof
    let target ← term (← inferType proof)
    let query : Query := { template := { name := `Baseline.branch, levels := [], mode := .body }, inputs := #[], guards := #[], hypothetical := inputs, mode := .body }
    let entry : CertificateEntry := { instanceName := `Baseline.instance, law := `Baseline.law, caseName := label, query, target }
    let result ← validateCertificate entry { proof := proofId, assumptions := inputs, guards := #[] }
    let actual := match result with
      | .admitted _ => "admitted"
      | .rejectedSuppliedContradiction s => s!"rejected supplier={s.declaration}"
      | .incomplete f => s!"{f.status} {f.code} {f.name} {f.detail}"
    unless actual == expected do throwError "{label}: expected {expected}; got {actual}"
    let s ← get
    IO.println s!"OBSERVATION {label}: {actual}; work={s.worklistSteps} terms={s.graph.terms.size} nodes={s.graph.nodes.size} templates={s.graph.templates.size} queries={s.queryCount} components={s.componentCount}"
  let _ ← action.run {}

set_option maxHeartbeats 2000000 in
#eval show MetaM Unit from do
  observe "bad-chain" (mkConst ``bad1) "rejected supplier=impossible"
set_option maxHeartbeats 2000000 in
#eval show MetaM Unit from do
  withLocalDeclD `h (mkConst ``Empty) fun h => do
    observe "useImpossibleLet" (mkApp (mkConst ``useImpossibleLet) h) "rejected supplier=impossible" #[h]
set_option maxHeartbeats 2000000 in
#eval show MetaM Unit from do
  observe "directApp" (mkConst ``directApp) "rejected supplier=suppliedByBool"
set_option maxHeartbeats 2000000 in
#eval show MetaM Unit from do
  observe "letApp" (mkConst ``letApp) "rejected supplier=suppliedByBool"
set_option maxHeartbeats 2000000 in
#eval show MetaM Unit from do
  observe "cert01" (mkConst ``cert01) "admitted"
"""


def observations():
    # Use the same simpAll reconstruction as the production candidate search.
    source = DRIVER + r"""
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
    IO.println s!"F3B decide-term {← ppExpr dec}"
    withLocalDeclD `hd (← mkEq dec (mkConst ``Bool.true)) fun hd => do
      withLocalDeclD `hn (← mkArrow eq (mkConst ``False)) fun hn => do
        observe "F3b-simp" (← simpProof)
          "INCOMPLETE analysis-budget-or-error Baseline.instance stage=certificate-dependencies law=Baseline.law case=F3b-simp unavailable-certificate-transfer propext" #[y, hd, hn]
"""
    with compiled_consumer() as meta, tempfile.TemporaryDirectory(prefix="n8a-baseline-") as temp:
        work = Path(temp)
        path = work / "Baseline.lean"
        path.write_text("import N2Meta\nset_option linter.defProp false\n" + source)
        result = command(["lean", path], work, dict(os.environ, LEAN_PATH=str(meta)))
        print(result.stdout, end="", flush=True)
        labels = [line.split(":", 1)[0] for line in result.stdout.splitlines()
                  if line.startswith("OBSERVATION ")]
        expected = ["OBSERVATION " + label for label in
                    ["bad-chain", "useImpossibleLet", "directApp", "letApp",
                     "cert01", "joint-simp", "F3b-simp"]]
        return int(result.returncode != 0 or labels != expected)


def historical_controls(remove_cycle_check=False, poison_leaf=False):
    import vacuity_certificates as certificates
    if poison_leaf:
        anchor = "theorem dependency0 : False → False := fun h => h"
        assert certificates.POSITIVE.count(anchor) == 1
        certificates.POSITIVE = certificates.POSITIVE.replace(anchor,
            "axiom impossible : False\ntheorem dependency0 : False → False := fun _ => impossible")
        print("MUTATION: dependency leaf replaced by impossible", flush=True)
    source = (HERE / "vacuity.lean").read_text()
    if remove_cycle_check:
        anchor = '  if (← get).pendingQueries.contains q then\n    throwError "cyclic-certificate-dependency"\n'
        assert source.count(anchor) == 1
        source = source.replace(anchor, "")
        print("MUTATION: pending-cycle rejection deleted in private consumer", flush=True)
    with compiled_consumer(consumer_text=source) as meta:
        return certificates.controls(meta)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--historical", action="store_true")
    parser.add_argument("--remove-cycle-check", action="store_true")
    parser.add_argument("--poison-leaf", action="store_true")
    args = parser.parse_args()
    if args.historical or args.remove_cycle_check or args.poison_leaf:
        return historical_controls(args.remove_cycle_check, args.poison_leaf)
    return observations()


if __name__ == "__main__":
    raise SystemExit(main())
