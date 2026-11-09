"""Focused accepted-design boundary probe; no production consumer switch."""
import os
import tempfile
from pathlib import Path
from vacuity_driver import command, compiled_consumer

DRIVER = r"""
open Lean Meta Vacuity.Discharge

theorem singletonCertificate : ∀ x : Nat, x ∈ [2] → x = 2 := by simp

set_option maxHeartbeats 2000000 in
#eval show MetaM Unit from do
  let ci ← getConstInfo ``singletonCertificate
  let some proof := ci.value? (allowOpaque := true) | throwError "missing-control-body"
  let (legacyBad, _) ← (Vacuity.falseDischarge proof).run {}
  unless !legacyBad do throwError "unexpected-existing-consumer-verdict"
  IO.println "EXISTING falseDischarge=false"
  let primitive ← getConstInfo ``propext
  unless primitive.value? (allowOpaque := true) |>.isNone do
    throwError "unexpected-propext-body"
  unless primitive matches .axiomInfo _ do throwError "unexpected-propext-kind"
  IO.println "DEPENDENCY propext kind=axiom body=unavailable"
  IO.println s!"TERM {← ppExpr proof}"
  let action : Solver Unit := do
    let proofId ← term proof
    let target ← term ci.type
    let query : Query := { template := { name := ``singletonCertificate, levels := [], mode := .body }, inputs := #[], guards := #[], hypothetical := #[], mode := .body }
    let entry : CertificateEntry := { instanceName := `Boundary.instance, law := `Boundary.law, caseName := "finite-collection-simp-positive", query, target }
    match ← validateCertificate entry { proof := proofId, assumptions := #[], guards := #[] } with
    | .incomplete finding =>
      let actual := s!"{finding.status} {finding.code} {finding.name} {finding.detail}"
      IO.println actual
    | .admitted _ => IO.println "ADMITTED"
    | .rejectedSuppliedContradiction supplier => IO.println s!"REJECTED {supplier.declaration}"
  let _ ← action.run {}
"""


def main():
    with compiled_consumer() as meta, tempfile.TemporaryDirectory(prefix="n2-boundary-") as temp:
        work = Path(temp)
        path = work / "Boundary.lean"
        path.write_text("import N2Meta\n" + DRIVER)
        result = command(["lean", path], work, dict(os.environ, LEAN_PATH=str(meta)))
        print(result.stdout, end="", flush=True)
        expected = "INCOMPLETE analysis-budget-or-error Boundary.instance stage=certificate-dependencies law=Boundary.law case=finite-collection-simp-positive unavailable-certificate-transfer propext"
        findings = [line for line in result.stdout.splitlines()
                    if line.startswith(("INCOMPLETE ", "ADMITTED", "REJECTED "))]
        if result.returncode or findings != [expected] or "EXISTING falseDischarge=false\n" not in result.stdout or "DEPENDENCY propext kind=axiom body=unavailable\n" not in result.stdout:
            print(f"FAIL boundary expected exact diagnostic {expected!r}; got {findings!r}")
            return 1
        print("PASS boundary reproduced: required positive reaches unavailable propext")
        return 0


if __name__ == "__main__":
    raise SystemExit(main())
