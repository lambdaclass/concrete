/-
  FORCING-PROBE FIXTURE — operational VC auto-discharge (ROADMAP Phase 9 #16a).
  POSITIVE half: operational VCs that close with a FIXED, MECHANICAL tactic —
  the kind an auto-discharger runs with ZERO per-obligation human input.

  The mechanical tactic is allowed ONLY:
    - unfold the evaluator (standard reusable eval-lowering lemma set),
    - unfold the extracted body def and the named spec it refines,
    - the reusable Int<->Nat<->BitVec round-trip collapse lemmas,
    - a mechanical conjunction split / guard `by_cases`,
    - route the resulting leaf to `bv_decide` / `omega` / `rfl` / `simp`.
  It is NOT allowed any per-proof `rw [show … by omega]`, bespoke helper lemma,
  or hand-picked rewrite. Those mark the boundary (see boundary.lean).

  This file MUST type-check clean (gate asserts exit 0). It locks the finding
  that the common operational-VC classes — pure bitwise word functions,
  straight-line loop-body preservation, and branching postconditions (with a
  mechanical guard split) — are mechanically dischargeable today.

  Gate: scripts/tests/check_operational_vc_auto_discharge.sh
  Design note: research/proof-evidence/operational-vc-auto-discharge.md
-/
import Std.Tactic.BVDecide
import Concrete.Proof
import Concrete.Examples.HmacSha256.Proofs
import Concrete.Examples.LoopInvariant.Proofs
import Concrete.Examples.ParseValidate.Proofs

set_option linter.unusedSimpArgs false
set_option maxHeartbeats 1000000

open Concrete
open Concrete.Proof
open Examples.HmacSha256.Proofs
open Examples.LoopInvariant.Proofs
open Examples.ParseValidate.Proofs

-- TEST 1 — `ch`: pure bitwise word function. eval-unfold → bv_decide.
theorem probe_ch (X Y Z : BitVec 32) (fuel : Nat) :
    eval (fun _ => none)
      (((Env.empty.bind "x" (.int X.toNat)).bind "y" (.int Y.toNat)).bind
        "z" (.int Z.toNat))
      (fuel + 1) chExpr
      = some (.int (Sha256Spec.ch X Y Z).toNat) := by
  simp only [chExpr, eval, Env.bind, evalBinOp, Sha256Spec.ch,
    beq_self_eq_true, if_true, beq_iff_eq, if_false, String.reduceEq,
    ofInt_natCast_toNat, Option.some.injEq,
    PVal.int.injEq, Int.ofNat_eq_natCast, Int.natCast_inj, BitVec.toNat_inj]
  bv_decide

-- TEST 2 — `maj`: pure bitwise word function.
theorem probe_maj (X Y Z : BitVec 32) (fuel : Nat) :
    eval (fun _ => none)
      (((Env.empty.bind "x" (.int X.toNat)).bind "y" (.int Y.toNat)).bind
        "z" (.int Z.toNat))
      (fuel + 1) majExpr
      = some (.int (Sha256Spec.maj X Y Z).toNat) := by
  simp only [majExpr, eval, Env.bind, evalBinOp, Sha256Spec.maj,
    beq_self_eq_true, if_true, beq_iff_eq, if_false, String.reduceEq,
    ofInt_natCast_toNat, Int.ofNat_eq_natCast, Int.natCast_inj, BitVec.toNat_inj]
  <;> bv_decide

-- TEST 3 — loop invariant preservation: straight-line body, omega leaf.
theorem probe_loop (fns : FnTable) (env : Env) (k acc0 : Int) (fuel : Nat)
    (hacc : env "acc" = some (.int acc0)) (hi : env "i" = some (.int k))
    (hlo : 0 ≤ k) (_hhi : k ≤ 8) (hguard : k < 8) :
    eval.evalAssigns fns env (fuel + 2) count_upBody
        = some ((env.bind "acc" (.int (acc0 + k))).bind "i" (.int (k + 1)))
      ∧ (0 ≤ k + 1 ∧ k + 1 ≤ 8) := by
  refine ⟨?_, ?_, ?_⟩ <;>
    first
    | omega
    | simp only [count_upBody, eval.evalAssigns, eval, evalBinOp, hacc, hi,
        Env.bind, beq_iff_eq, String.reduceEq, if_false]

-- TEST 4 — branching parser postcondition WITH a mechanical guard split.
theorem probe_version_split (v : Int) (fuel : Nat) :
    eval parseValidateFns (Env.empty.bind "v" (.int v)) (fuel + 2) validateVersionExpr
    = some (.int (if v = 1 then 0 else 1)) := by
  by_cases h : v = 1 <;>
    simp_all [validateVersionExpr, eval, Env.bind, evalBinOp, BEq.beq]
