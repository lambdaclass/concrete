"""Focused typed-result protection controls (no specification census)."""
import os
import tempfile
from pathlib import Path

from vacuity_driver import command, compiled_consumer

DRIVER = r'''
import N2Meta
open Lean Meta Vacuity
namespace ProtectedResults
def structuresOnly : Bool := STRUCTURES_ONLY
structure Ops where
  zero : Nat
  step : Nat → Nat
def subject : Ops := ⟨0, fun n => n+1⟩
def pair (o : Ops) (n : Nat) : Nat × Nat := (o.step n, o.zero)
def original : Prop := match pair subject 0 with | (x, z) => x = x ∧ z = z
def restated : Prop := let v := pair subject 0; v.1 = v.1 ∧ v.2 = v.2
def counterfeit : Prop := match pair subject 0 with | (x, z) => x = x ∧ z = 0
def nestedOriginal : Prop := match pair subject 0 with
  | (x, z) => match x == 0 with | true => z = z | false => z = z
def nestedCounterfeit : Prop := match pair subject 0 with
  | (x, z) => match x == 0 with | true => z = 0 | false => z = z
def packaged : Prop := (∃ v : Nat × Nat, v = pair subject 0) ↔
  (∃ v : Nat × Nat, v = pair subject 0)
def dependent (n : Nat) : Prop := ∃ x : {v : Nat // v ≤ subject.step n}, x.1 < subject.step x.1
def sigmaDependent (n : Nat) : Prop :=
  ∃ x : Sigma (fun _v : Nat => Fin (subject.step n)), x.2.val < subject.step x.1
set_option maxRecDepth 4096 in
set_option maxHeartbeats 2000000 in
#eval show MetaM Unit from do
  for (label, a, b, expected) in [
      ("structure-positive", mkConst ``original, mkConst ``restated, true),
      ("structure-counterfeit", mkConst ``original, mkConst ``counterfeit, false),
      ("nested-counterfeit", mkConst ``nestedOriginal, mkConst ``nestedCounterfeit, false),
      ("iff-exists-packaging", mkConst ``packaged, mkConst ``packaged, true),
      ("dependent-domain", mkApp (mkConst ``dependent) (mkNatLit 2), mkApp (mkConst ``dependent) (mkNatLit 2), true),
      ("sigma-dependent-domain", mkApp (mkConst ``sigmaDependent) (mkNatLit 2), mkApp (mkConst ``sigmaDependent) (mkNatLit 2), true)] do
    if structuresOnly && (label == "dependent-domain" || label == "sigma-dependent-domain") then continue
    let job : AnalysisJob := {
      name := Name.str `ProtectedResults label, stage := "instance"
      action := do
        let equal ← protectedEq a b #[``subject] (← get).budget
        IO.println s!"PROTECTED {label} equal={equal} expected={expected}"
        unless equal == expected do throwError "protected-result-verdict {label}"
        -- Ordinary metadata and a let alias must not change protection.
        let tagged := Expr.mdata (({} : MData).setString `control "ordinary") a
        let alias := Expr.letE `contract (mkSort .zero) tagged (.bvar 0) false
        unless (← protectedEq alias b #[``subject] (← get).budget) == expected do
          throwError "protected-metadata-let-verdict {label}"
    }
    let (_, state) ← (isolated (← readThe Core.Context) job).run {}
    for finding in state.findings do
      IO.println s!"{finding.status} {finding.code} {finding.name} {finding.detail}"
    unless state.findings.all (fun f => f.status == "NOTE") do throwError "protected-result-incomplete {label}"
end ProtectedResults
'''


def main(meta=None, *, structures_only=False):
    from contextlib import nullcontext
    with (compiled_consumer() if meta is None else nullcontext(meta)) as meta:
        with tempfile.TemporaryDirectory(prefix="n2-protected-results-") as temp:
            path = Path(temp) / "Protected.lean"
            path.write_text(DRIVER.replace("STRUCTURES_ONLY", str(structures_only).lower()))
            result = command(["lean", path], temp, dict(os.environ, LEAN_PATH=str(meta)))
    print(result.stdout, end="", flush=True)
    assert result.returncode == 0, result.stdout
    cases = [
        ("structure-positive", "true"), ("structure-counterfeit", "false"),
        ("nested-counterfeit", "false"), ("iff-exists-packaging", "true"),
        ("dependent-domain", "true"), ("sigma-dependent-domain", "true")]
    if structures_only:
        cases = cases[:4]
    expected = {f"PROTECTED {label} equal={value} expected={value}" for label, value in cases}
    assert {r for r in result.stdout.splitlines() if r.startswith("PROTECTED ")} == expected
    print(f"Protected result controls: {len(cases)} PASS", flush=True)
    return 0


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--structures-only", action="store_true")
    raise SystemExit(main(structures_only=parser.parse_args().structures_only))
