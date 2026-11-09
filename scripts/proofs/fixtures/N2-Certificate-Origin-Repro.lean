import Lean

/- Meta-level design counterexample, outside ConcreteSpec. This does not run the
   proposed solver or claim a phase-B control PASS. -/
open Lean Meta

axiom impossible : False
theorem injectedFalse : False := impossible

theorem reachableDischarge (_k : Nat) : 2 = 2 := False.elim impossible

def contextCertificate : MetaM (Option Expr) := withoutModifyingState do
  let goal ← mkFreshExprMVar (mkConst ``False)
  unless ← goal.mvarId!.contradictionCore { useDecide := false } do
    let ctx ← Simp.mkContext { failIfUnchanged := false }
      (simpTheorems := #[(← getSimpTheorems)])
    let (remaining, _) ← simpAll goal.mvarId! ctx
    if remaining.isSome then return none
  let proof ← instantiateMVars goal
  if proof.hasMVar then throwError "unexpected-certificate-metavariable"
  Lean.Meta.check proof
  unless ← isDefEq (← inferType proof) (mkConst ``False) do
    throwError "unexpected-certificate-type"
  return some proof

set_option maxHeartbeats 2000000 in
#eval show MetaM Unit from do
  unless (← getLCtx).getFVarIds.isEmpty do throwError "unexpected-local-context"
  unless (← contextCertificate).isNone do throwError "unexpected-baseline-certificate"
  IO.println "REPRO baseline: no context refutation"

attribute [local simp] injectedFalse

set_option maxHeartbeats 2000000 in
#eval show MetaM Unit from do
  unless (← getLCtx).getFVarIds.isEmpty do throwError "unexpected-local-context"
  let some proof ← contextCertificate | throwError "missing-injected-certificate"
  let rendered := toString (← ppExpr proof)
  unless rendered == "of_eq_true injectedFalse._simp_1" do
    throwError "unexpected-certificate {rendered}"
  let ci ← getConstInfo ``injectedFalse
  unless ci.value? (allowOpaque := true) == some (mkConst ``impossible) do
    throwError "unexpected-certificate-supplier"
  IO.println s!"REPRO kernel-checked certificate: {rendered}"
  IO.println "REPRO certificate type: False; local assumptions: 0"
  IO.println "REPRO supplied dependency: injectedFalse := impossible"
