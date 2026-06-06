import Concrete.Proof

/-!
# elf_header — example proofs

Proof theorems for the ELF-header field validators, moved out of `Concrete.Proof`
into this per-example namespace. The registered spec PExprs
(`checkMagicExpr`/`checkClassExpr`/`checkDataExpr`/`checkVersionExpr`/`validateHeaderExpr`)
and the `elfFns` table + `*Fn` defs stay in `Concrete.Proof`.
-/

namespace Examples.ElfHeader.Proofs

open Concrete.Proof

/-- check_magic returns 1 iff all four bytes are the ELF magic sequence. -/
theorem check_magic_correct (b0 b1 b2 b3 : Int) (fuel : Nat) :
    eval elfFns
      ((((Env.empty.bind "b0" (.int b0)).bind "b1" (.int b1)).bind "b2" (.int b2)).bind "b3" (.int b3))
      (fuel + 5) checkMagicExpr
    = some (.int (if b0 = 127 ∧ b1 = 69 ∧ b2 = 76 ∧ b3 = 70 then 1 else 0)) := by
  by_cases h0 : b0 = 127 <;> by_cases h1 : b1 = 69 <;>
    by_cases h2 : b2 = 76 <;> by_cases h3 : b3 = 70 <;>
    simp_all [checkMagicExpr, eval, Env.bind, evalBinOp, BEq.beq]

/-- check_class returns 1 iff cls is 1 or 2. -/
theorem check_class_correct (cls : Int) (fuel : Nat) :
    eval elfFns (Env.empty.bind "cls" (.int cls)) (fuel + 3) checkClassExpr
    = some (.int (if cls = 1 ∨ cls = 2 then 1 else 0)) := by
  by_cases h1 : cls = 1 <;> by_cases h2 : cls = 2 <;>
    simp_all [checkClassExpr, eval, Env.bind, evalBinOp, BEq.beq]

/-- check_data returns 1 iff encoding is 1 or 2. -/
theorem check_data_correct (encoding : Int) (fuel : Nat) :
    eval elfFns (Env.empty.bind "encoding" (.int encoding)) (fuel + 3) checkDataExpr
    = some (.int (if encoding = 1 ∨ encoding = 2 then 1 else 0)) := by
  by_cases h1 : encoding = 1 <;> by_cases h2 : encoding = 2 <;>
    simp_all [checkDataExpr, eval, Env.bind, evalBinOp, BEq.beq]

/-- check_version returns 1 iff ver is 1. -/
theorem check_version_correct (ver : Int) (fuel : Nat) :
    eval elfFns (Env.empty.bind "ver" (.int ver)) (fuel + 2) checkVersionExpr
    = some (.int (if ver = 1 then 1 else 0)) := by
  by_cases h : ver = 1 <;>
    simp_all [checkVersionExpr, eval, Env.bind, evalBinOp, BEq.beq]

/-- Full correctness of validate_header: returns 1 iff all ELF field
    constraints hold (magic = 0x7F 'E' 'L' 'F', class ∈ {1,2},
    encoding ∈ {1,2}, version = 1). Sound + complete in one theorem. -/
theorem validate_header_correct (b0 b1 b2 b3 cls encoding ver : Int) (fuel : Nat) :
    let env := ((((((Env.empty.bind "b0" (.int b0)).bind "b1" (.int b1)).bind "b2" (.int b2)).bind "b3" (.int b3)).bind "cls" (.int cls)).bind "encoding" (.int encoding)).bind "ver" (.int ver)
    eval elfFns env (fuel + 10) validateHeaderExpr
    = some (.int (if b0 = 127 ∧ b1 = 69 ∧ b2 = 76 ∧ b3 = 70 ∧
                     (cls = 1 ∨ cls = 2) ∧ (encoding = 1 ∨ encoding = 2) ∧ ver = 1
                  then 1 else 0)) := by
  -- Substitute magic bytes, then case-split cls/encoding/ver (8 concrete cases)
  by_cases h0 : b0 = 127 <;> by_cases h1 : b1 = 69 <;>
    by_cases h2 : b2 = 76 <;> by_cases h3 : b3 = 70
  -- Positive magic (all match): case-split remaining 3 fields
  · by_cases hc1 : cls = 1 <;> by_cases hc2 : cls = 2 <;>
      by_cases he1 : encoding = 1 <;> by_cases he2 : encoding = 2 <;>
      by_cases hv : ver = 1 <;>
      simp_all [validateHeaderExpr, eval, eval.evalArgs, elfFns,
          checkMagicFn, checkMagicExpr, checkClassFn, checkClassExpr,
          checkDataFn, checkDataExpr, checkVersionFn, checkVersionExpr,
          Env.bind, evalBinOp, bindArgs, BEq.beq]
  -- Negative magic cases (any byte wrong): check_magic returns 0, short-circuit
  all_goals simp_all [validateHeaderExpr, eval, eval.evalArgs, elfFns,
      checkMagicFn, checkMagicExpr, Env.bind, evalBinOp, bindArgs,
      BEq.beq]
end Examples.ElfHeader.Proofs
