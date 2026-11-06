import Concrete.Proof.Proof

/-!
# png_chunks — signature-check proof

The png_chunks workload (`examples/png_chunks`) refuses a file whose first eight bytes are not the
PNG signature. The decision is the pure helper `png_sig_ok`; the specification below is its
EXTRACTED body (as `--report lean-stubs` emits it), and the theorem says it accepts exactly the
signature of PNG specification section 5.2, `89 50 4E 47 0D 0A 1A 0A`.

The theorem quantifies over every `Int` for each byte. The source parameters are `u8`, whose values
are a subset, so the claim covers every input the program can pass.

What this does NOT cover: `is_png_sig`, which reads the bytes out of a `Bytes` buffer after a length
check. It extracts, but only as calls to `Bytes_len` / `Bytes_get_unchecked`, which no proof
`FnTable` models, so no theorem can evaluate it: the claim is about the decision, not about which
bytes reach it. The per-chunk CRC32 is std's `crc32`, which is refused outright (a data-dependent
loop, and `u64` remainder, division and bitwise operators the extractor does not model; see
docs/verification/PROOF_EXTRACTION_GAPS.md). And `PExpr` semantics are the proof model, not the
compiled code (H19).
-/

namespace Examples.PngChunks.Proofs

open Concrete.Proof

/-- Spec = the extracted body of `png_chunks.png_sig_ok`. -/
def pngSigOkExpr : PExpr :=
  .ifThenElse (.binOp .ne (.var "b0") (.lit (.int 137))) (.lit (.bool false))
  (.ifThenElse (.binOp .ne (.var "b1") (.lit (.int 80))) (.lit (.bool false))
  (.ifThenElse (.binOp .ne (.var "b2") (.lit (.int 78))) (.lit (.bool false))
  (.ifThenElse (.binOp .ne (.var "b3") (.lit (.int 71))) (.lit (.bool false))
  (.ifThenElse (.binOp .ne (.var "b4") (.lit (.int 13))) (.lit (.bool false))
  (.ifThenElse (.binOp .ne (.var "b5") (.lit (.int 10))) (.lit (.bool false))
  (.ifThenElse (.binOp .ne (.var "b6") (.lit (.int 26))) (.lit (.bool false))
  (.binOp .eq (.var "b7") (.lit (.int 10)))))))))

set_option linter.unusedSimpArgs false in
/-- `png_sig_ok(b0, …, b7)` is true exactly when the eight bytes are the PNG signature
    `89 50 4E 47 0D 0A 1A 0A` (PNG specification, section 5.2). -/
theorem png_sig_ok_correct (b0 b1 b2 b3 b4 b5 b6 b7 : Int) (fuel : Nat) :
    eval FnTable.empty
      (((((((((Env.empty.bind "b0" (.int b0)).bind "b1" (.int b1)).bind "b2" (.int b2)).bind
        "b3" (.int b3)).bind "b4" (.int b4)).bind "b5" (.int b5)).bind "b6" (.int b6)).bind
        "b7" (.int b7)))
      (fuel + 8) pngSigOkExpr
    = some (.bool (decide (b0 = 0x89 ∧ b1 = 0x50 ∧ b2 = 0x4E ∧ b3 = 0x47 ∧
                           b4 = 0x0D ∧ b5 = 0x0A ∧ b6 = 0x1A ∧ b7 = 0x0A))) := by
  by_cases h0 : b0 = 137 <;> by_cases h1 : b1 = 80 <;> by_cases h2 : b2 = 78 <;>
    by_cases h3 : b3 = 71 <;> by_cases h4 : b4 = 13 <;> by_cases h5 : b5 = 10 <;>
    by_cases h6 : b6 = 26 <;>
    simp_all [pngSigOkExpr, eval, Env.bind, evalBinOp, BEq.beq, bne]

/-! ## Specification parameters (bug 079) -/
def pngSigOkSpec : SpecDecl :=
  { params := ["b0", "b1", "b2", "b3", "b4", "b5", "b6", "b7"], body := pngSigOkExpr }

end Examples.PngChunks.Proofs
