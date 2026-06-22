/-
  FORCING-PROBE FIXTURE — operational VC auto-discharge (ROADMAP Phase 9 #16a).
  BOUNDARY half: operational VCs that the FIXED mechanical tactic CANNOT close
  today — these mark exactly the fragment V1 must build.

  This file is EXPECTED TO FAIL to type-check (the gate asserts a nonzero exit).
  It is a tripwire: if a future operational-VC-auto-discharge implementation
  (the cast-normalization fragment) lands, these cases will start to close, the
  gate will flip, and that flip is the signal to move them into closes.lean and
  update the roadmap + design note.

  Same mechanical-tactic rules as closes.lean — NO per-proof `rw [show …]`,
  helper lemma, or guard split. The point is to show what that buys you and
  what it does not.

  Gate: scripts/tests/check_operational_vc_auto_discharge.sh
  Design note: research/proof-evidence/operational-vc-auto-discharge.md
-/
import Std.Tactic.BVDecide
import Concrete.Proof
import Concrete.Examples.HmacSha256.Proofs
import Concrete.Examples.ParseValidate.Proofs

set_option linter.unusedSimpArgs false
set_option maxHeartbeats 1000000

open Concrete
open Concrete.Proof
open Examples.HmacSha256.Proofs
open Examples.ParseValidate.Proofs

-- BOUNDARY 1 — `rotr`: shift amount `32 - n` leaves `(↑n).toNat` /
-- `(32 - ↑n).toNat` casts (eval binds Int, spec shifts by Nat). bv_decide is
-- not a pure-BitVec goal; rfl is not def-eq. The human proof bridges with one
-- mechanical line: `rw [show ((n:Int)).toNat = n by omega, …]` — i.e. generate
-- the toNat/ofInt + linear-bound side-goals and discharge them with omega
-- BEFORE the BitVec leaf. That normalization is the V1 fragment.
theorem probe_rotr (fns : FnTable) (X : BitVec 32) (n : Nat) (hn : n ≤ 32) (fuel : Nat) :
    eval fns ((Env.empty.bind "x" (.int X.toNat)).bind "n" (.int (n:Int))) (fuel + 1) rotrExpr
      = some (.int (Sha256Spec.rotr X n).toNat) := by
  simp only [rotrExpr, eval, Env.bind, evalBinOp, Sha256Spec.rotr,
    beq_self_eq_true, if_true, beq_iff_eq, if_false, String.reduceEq, reduceCtorEq,
    ofInt_natCast_toNat, Option.some.injEq, PVal.int.injEq,
    Int.ofNat_eq_natCast, Int.natCast_inj, BitVec.toNat_inj]
  <;> first | bv_decide | omega | rfl

-- BOUNDARY 2 — branching postcondition WITHOUT the mechanical guard split:
-- the goal stalls on an unevaluated `if decide … then … else …`. The fix is a
-- mechanical `by_cases` on the branch condition (see probe_version_split in
-- closes.lean) — so this is "the discharger must split guards", a fragment
-- feature, not a wall.
theorem probe_version_nosplit (v : Int) (fuel : Nat) :
    eval parseValidateFns (Env.empty.bind "v" (.int v)) (fuel + 2) validateVersionExpr
    = some (.int (if v = 1 then 0 else 1)) := by
  simp only [validateVersionExpr, eval, Env.bind, evalBinOp, BEq.beq]
  <;> first | omega | bv_decide | rfl
