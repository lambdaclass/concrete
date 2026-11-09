"""Direct, light controls for slice-2a certificate admission (no consumer switch)."""
import os
import tempfile
from pathlib import Path

from vacuity_driver import HERE, command, compiled_consumer

DRIVER = r"""
open Lean Meta Vacuity.Discharge

def probe (label : String) (proof : Expr) (expected : String)
    (producer : Name := `CertificateControl.branch) (spent : Nat := 0) : MetaM Unit := do
  let action : Solver String := do
    -- Model work already spent by the containing Instance, without renewing it.
    for _ in [:spent] do charge
    let proofId ← term proof
    let target ← term (← inferType proof)
    let query : Query := { template := { name := producer, levels := [], mode := .body }, inputs := #[], guards := #[], hypothetical := #[], mode := .body }
    let entry : CertificateEntry := { instanceName := `CertificateControl.instance, law := `CertificateControl.law, caseName := label, query, target }
    let before := (← get).certificates
    let result ← validateCertificate entry { proof := proofId, assumptions := #[], guards := #[] }
    let rendered ← match result with
      | .admitted derivation => do
        unless derivation.rank == (if derivation.structuralExclusions.isEmpty then 0 else 1) && !derivation.transfers.isEmpty do
          throwError "missing-finite-derivation"
        pure "admitted"
      | .rejectedSuppliedContradiction supplier => do
        let some (.supplied instanceName _ producing) := (← get).graph.origins[supplier.input.origin]?
          | throwError "lost-supplier-origin"
        unless instanceName == entry.instanceName && producing == supplier.input.term do
          throwError "wrong-supplier-origin"
        pure s!"rejected-supplied-contradiction supplier={supplier.declaration}"
      | .incomplete finding =>
        pure s!"{finding.status} {finding.code} {finding.name} {finding.detail}"
    if rendered != "admitted" && (← get).certificates != before then
      throwError "pending-certificate-admitted"
    unless (← get).remaining + (← get).worklistSteps == 20000 do
      throwError "renewed-work-budget"
    return rendered
  let (actual, _) ← action.run {}
  unless actual == expected do throwError "{label}: expected {expected}, got {actual}"
  IO.println s!"CERTIFICATE {label}: {actual}"
"""

NEGATIVE = r"""
set_option maxHeartbeats 2000000 in
#eval show MetaM Unit from do
  let some proof ← contextCertificate | throwError "missing-origin-certificate"
  probe "origin" proof "rejected-supplied-contradiction supplier=impossible"
  -- The pending candidate would exclude the branch reaching this very producer.
  -- It must remain unavailable throughout validation, including through simp helpers.
  probe "non-circular" proof "rejected-supplied-contradiction supplier=impossible" `reachableDischarge
"""

POSITIVE = r"""
theorem genuineCertificate : ¬ False := by decide

theorem dependency0 : False → False := fun h => h
CHAIN
set_option maxHeartbeats 2000000 in
#eval show MetaM Unit from do
  probe "library-decide-matcher" (mkConst ``genuineCertificate) "admitted"
  probe "dependency-chain-baseline" (mkConst ``dependency80) "admitted"
  probe "shared-budget" (mkConst ``dependency80)
    "INCOMPLETE analysis-budget-or-error CertificateControl.instance stage=certificate-dependencies law=CertificateControl.law case=shared-budget false-discharge-budget"
    (spent := 19970)
"""


def controls(meta):
    expected = [
        "CERTIFICATE origin: rejected-supplied-contradiction supplier=impossible",
        "CERTIFICATE non-circular: rejected-supplied-contradiction supplier=impossible",
        "CERTIFICATE library-decide-matcher: admitted",
        "CERTIFICATE dependency-chain-baseline: admitted",
        "CERTIFICATE shared-budget: INCOMPLETE analysis-budget-or-error CertificateControl.instance stage=certificate-dependencies law=CertificateControl.law case=shared-budget false-discharge-budget",
    ]
    negative = (HERE / "fixtures/N2-Certificate-Origin-Repro.lean").read_text().replace("import Lean\n", "", 1)
    chain = "\n".join(f"theorem dependency{i} : False → False := dependency{i - 1}" for i in range(1, 81))
    sources = [negative + DRIVER + NEGATIVE, DRIVER + POSITIVE.replace("CHAIN", chain)]
    lines = []
    with tempfile.TemporaryDirectory(prefix="n2-certificate-") as temp:
        work = Path(temp)
        for index, source in enumerate(sources):
            path = work / f"Control{index}.lean"
            path.write_text("import N2Meta\n" + source)
            result = command(["lean", path], work, dict(os.environ, LEAN_PATH=str(meta)))
            print(result.stdout, end="", flush=True)
            if result.returncode:
                return 1
            lines.extend(line for line in result.stdout.splitlines() if line.startswith("CERTIFICATE "))
    if lines != expected:
        print(f"FAIL certificate-results expected={expected!r} actual={lines!r}")
        return 1
    print("Certificate controls: 5 PASS, 0 FAIL", flush=True)
    return 0


def main():
    with compiled_consumer() as meta:
        return controls(meta)


if __name__ == "__main__":
    raise SystemExit(main())
