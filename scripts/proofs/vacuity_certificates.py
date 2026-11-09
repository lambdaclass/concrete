"""Direct, light controls for slice-2a certificate admission (no consumer switch)."""
import os
import tempfile
from pathlib import Path

from vacuity_driver import HERE, command, compiled_consumer

DRIVER = r"""
open Lean Meta Vacuity.Discharge

partial def hasLeafSupplied (g : Graph) (oid : OriginId) (inst : Name) (prod : TermId)
    (seen : Std.HashSet OriginId := {}) : Bool :=
  if seen.contains oid then false
  else
    let seen := seen.insert oid
    match g.origins[oid]? with
    | some (.supplied i _ p) => i == inst && p == prod
    | some (.field p _) => hasLeafSupplied g p inst prod seen
    | some (.substitution _ ins) => ins.any (fun i => hasLeafSupplied g i inst prod seen)
    | _ => false

partial def mentionsTermInOrigin (g : Graph) (oid : OriginId) (targetTerm : TermId)
    (seen : Std.HashSet OriginId := {}) : Bool :=
  if seen.contains oid then false
  else
    let seen := seen.insert oid
    match g.origins[oid]? with
    | some (.supplied _ _ p) => p == targetTerm
    | some (.field p _) => mentionsTermInOrigin g p targetTerm seen
    | some (.substitution callee ins) =>
      callee == targetTerm || ins.any (fun i => mentionsTermInOrigin g i targetTerm seen)
    | _ => false

def probe (label : String) (proof : Expr) (expected : String)
    (producer : Name := `CertificateControl.branch) (spent : Nat := 0)
    (assumptions : Array Expr := #[]) (entryArg : Option Expr := none) : MetaM Unit := do
  let action : Solver String := do
    -- Model work already spent by the containing Instance, without renewing it.
    for _ in [:spent] do charge
    let mut inputs := #[]
    for x in assumptions do
      let id ← term x
      let type ← term (← inferType x)
      let o ← origin (.hypothetical id id)
      inputs := inputs.push ({ term := id, type, origin := o } : Input)
    let proofId ← term proof
    let entryArgId ← entryArg.mapM term
    let target ← term (← inferType proof)
    let query : Query := { template := { name := producer, levels := [], mode := .body }, inputs := #[], guards := #[], hypothetical := inputs, mode := .body }
    let entry : CertificateEntry := { instanceName := `CertificateControl.instance, law := `CertificateControl.law, caseName := label, query, target }
    let before := (← get).certificates
    let result ← validateCertificate entry { proof := proofId, assumptions := inputs, guards := #[] }
    let rendered ← match result with
      | .admitted derivation => do
        unless derivation.rank == (if derivation.structuralExclusions.isEmpty then 0 else 1) && !derivation.transfers.isEmpty do
          throwError "missing-finite-derivation"
        pure s!"admitted rank={derivation.rank} exclusions={derivation.structuralExclusions.size}"
      | .rejectedSuppliedContradiction supplier => do
        let g := (← get).graph
        let some (.substitution callee ins) := g.origins[supplier.input.origin]?
          | throwError "expected-transferred-substitution-origin"
        unless callee != supplier.input.term && !ins.isEmpty do
          throwError "trivial-self-compared-supplier-origin"
        unless hasLeafSupplied g supplier.input.origin entry.instanceName supplier.input.term do
          throwError "lost-leaf-supplier-origin"
        if let some argId := entryArgId then
          unless mentionsTermInOrigin g supplier.input.origin argId do
            throwError "missing-entry-argument-origin-transfer"
        pure s!"rejected-supplied-contradiction supplier={supplier.declaration}"
      | .incomplete finding =>
        pure s!"{finding.status} {finding.code} {finding.name} {finding.detail}"
    if !(rendered.startsWith "admitted") && (← get).certificates != before then
      throwError "pending-certificate-admitted"
    unless (← get).remaining + (← get).worklistSteps == 20000 do
      throwError "renewed-work-budget"
    let s ← get
    IO.println s!"COST {label}: work={s.worklistSteps} terms={s.graph.terms.size} nodes={s.graph.nodes.size} templates={s.graph.templates.size} queries={s.queryCount} components={s.componentCount}"
    return rendered
  let (actual, _) ← action.run {}
  unless actual == expected do throwError "{label}: expected {expected}, got {actual}"
  IO.println s!"CERTIFICATE {label}: {actual}"
"""

NEGATIVE = r"""
def useImpossibleBinder : Empty → False := fun _ => impossible
def useImpossibleLet : Empty → False := let _ : Nat := 0; fun _ => impossible
def PickType : Bool → Prop | true => False | false => True
axiom suppliedByBool (b : Bool) : PickType b
def letApp : False := (let _ : Nat := 0; fun (b : Bool) => suppliedByBool b) true
theorem cyclicHelper : False := False.elim impossible
theorem cyclicCaller : False := cyclicHelper
theorem badDependency0 : False → False := fun _ => impossible
BAD_CHAIN

set_option maxHeartbeats 2000000 in
#eval show MetaM Unit from do
  let some proof ← contextCertificate | throwError "missing-origin-certificate"
  let entryArg := proof.getAppArgs.back!
  probe "origin" proof "rejected-supplied-contradiction supplier=impossible"
    (entryArg := some entryArg)
  probe "unapplied-empty-binder" (mkConst ``useImpossibleBinder)
    "rejected-supplied-contradiction supplier=impossible"
  withLocalDeclD `h (mkConst ``Empty) fun h => do
    probe "use-impossible-let" (mkApp (mkConst ``useImpossibleLet) h)
      "rejected-supplied-contradiction supplier=impossible"
      (assumptions := #[h])
  probe "let-app-supplied" (mkConst ``letApp)
    "rejected-supplied-contradiction supplier=suppliedByBool"
  -- The pending candidate would exclude the branch reaching `cyclicHelper`.
  -- It remains in `pendingQueries` throughout validation, so a dependency cycle
  -- back to `cyclicHelper` fails with `cyclic-certificate-dependency`.
  probe "non-circular" (mkConst ``cyclicCaller)
    "INCOMPLETE analysis-budget-or-error CertificateControl.instance stage=certificate-dependencies law=CertificateControl.law case=non-circular cyclic-certificate-dependency"
    `cyclicHelper
  probe "dependency-chain-poisoned" (mkConst ``badDependency80)
    "rejected-supplied-contradiction supplier=impossible"
"""

POSITIVE = r"""
theorem genuineCertificate : ¬ (0 = 1) := by decide

theorem dependency0 : False → False := fun h => h
CHAIN

def simpAllContradiction : MetaM Expr := do
  let goal ← mkFreshExprMVar (mkConst ``False)
  let ctx ← Simp.mkContext { failIfUnchanged := false } (simpTheorems := #[(← getSimpTheorems)])
  let (remaining, _) ← simpAll goal.mvarId! ctx
  unless remaining.isNone do throwError "missing-simp-certificate"
  instantiateMVars goal

set_option maxHeartbeats 2000000 in
#eval show MetaM Unit from do
  withLocalDeclD `h (mkConst ``Empty) fun h => do
    let elimPrf ← mkAppOptM ``Empty.elim #[some (mkSort .zero), some h]
    probe "structural-empty-elim" elimPrf "admitted rank=1 exclusions=1"
      (assumptions := #[h])
  withLocalDeclD `h (mkConst ``False) fun h => do
    let hypPropext ← mkAppM ``of_eq_true #[← mkAppM ``eq_true #[h]]
    probe "hypothetical-propext" hypPropext
      "INCOMPLETE analysis-budget-or-error CertificateControl.instance stage=certificate-dependencies law=CertificateControl.law case=hypothetical-propext unavailable-certificate-transfer propext"
      (assumptions := #[h])
  probe "library-decide-matcher" (mkConst ``genuineCertificate) "admitted rank=0 exclusions=0"
  withLocalDeclD `y (mkConst ``Nat) fun y => do
    let eq ← mkEq y (mkNatLit 0)
    let dec ← instantiateMVars (← mkAppOptM ``decide #[some eq, none])
    withLocalDeclD `hd (← mkEq dec (mkConst ``Bool.true)) fun hd => do
      withLocalDeclD `hn (← mkArrow eq (mkConst ``False)) fun hn => do
        let directPrf := mkApp hn (← mkAppM ``of_decide_eq_true #[hd])
        probe "f3b-decide-matcher" directPrf "admitted rank=0 exclusions=0" (assumptions := #[y, hd, hn])
        let simpPrf ← simpAllContradiction
        probe "f3b-simp-all" simpPrf
          "INCOMPLETE analysis-budget-or-error CertificateControl.instance stage=certificate-dependencies law=CertificateControl.law case=f3b-simp-all unavailable-certificate-transfer propext"
          (assumptions := #[y, hd, hn])
  probe "dependency-chain-baseline" (mkConst ``dependency80) "admitted rank=0 exclusions=0"
  probe "shared-budget" (mkConst ``dependency80)
    "INCOMPLETE analysis-budget-or-error CertificateControl.instance stage=certificate-dependencies law=CertificateControl.law case=shared-budget false-discharge-budget"
    (spent := 19970)
"""


def controls(meta):
    expected = [
        "CERTIFICATE origin: rejected-supplied-contradiction supplier=impossible",
        "CERTIFICATE unapplied-empty-binder: rejected-supplied-contradiction supplier=impossible",
        "CERTIFICATE use-impossible-let: rejected-supplied-contradiction supplier=impossible",
        "CERTIFICATE let-app-supplied: rejected-supplied-contradiction supplier=suppliedByBool",
        "CERTIFICATE non-circular: INCOMPLETE analysis-budget-or-error CertificateControl.instance stage=certificate-dependencies law=CertificateControl.law case=non-circular cyclic-certificate-dependency",
        "CERTIFICATE dependency-chain-poisoned: rejected-supplied-contradiction supplier=impossible",
        "CERTIFICATE structural-empty-elim: admitted rank=1 exclusions=1",
        "CERTIFICATE hypothetical-propext: INCOMPLETE analysis-budget-or-error CertificateControl.instance stage=certificate-dependencies law=CertificateControl.law case=hypothetical-propext unavailable-certificate-transfer propext",
        "CERTIFICATE library-decide-matcher: admitted rank=0 exclusions=0",
        "CERTIFICATE f3b-decide-matcher: admitted rank=0 exclusions=0",
        "CERTIFICATE f3b-simp-all: INCOMPLETE analysis-budget-or-error CertificateControl.instance stage=certificate-dependencies law=CertificateControl.law case=f3b-simp-all unavailable-certificate-transfer propext",
        "CERTIFICATE dependency-chain-baseline: admitted rank=0 exclusions=0",
        "CERTIFICATE shared-budget: INCOMPLETE analysis-budget-or-error CertificateControl.instance stage=certificate-dependencies law=CertificateControl.law case=shared-budget false-discharge-budget",
    ]
    negative = (HERE / "fixtures/N2-Certificate-Origin-Repro.lean").read_text().replace("import Lean\n", "", 1)
    bad_chain = "\n".join(f"theorem badDependency{i} : False → False := badDependency{i - 1}" for i in range(1, 81))
    chain = "\n".join(f"theorem dependency{i} : False → False := dependency{i - 1}" for i in range(1, 81))
    sources = [
        "set_option linter.defProp false\n" + negative + DRIVER + NEGATIVE.replace("BAD_CHAIN", bad_chain),
        "set_option linter.defProp false\n" + DRIVER + POSITIVE.replace("CHAIN", chain),
    ]
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
    print(f"Certificate controls: {len(expected)} PASS, 0 FAIL", flush=True)
    return 0


def main():
    with compiled_consumer() as meta:
        return controls(meta)


if __name__ == "__main__":
    raise SystemExit(main())
