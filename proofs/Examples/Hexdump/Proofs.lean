import Concrete.Proof.Proof

/-!
# hexdump — output-format proofs

The hexdump workload (`examples/hexdump`) is byte-identical to `xxd` for its default form. Two
pure helpers decide what each output line contains; each specification below is the EXTRACTED body
of that helper (as `--report lean-stubs` emits it), and each theorem states the xxd rule it
implements:

| helper        | rule                                                                 |
|---------------|----------------------------------------------------------------------|
| `gutter_byte` | the ASCII gutter shows a printable ASCII byte (0x20-0x7E) as itself and every other byte as '.' (0x2E) |
| `line_len`    | a line holds the remaining bytes, but never more than 16             |

Both theorems quantify over every `Int`. The source parameters are `u64`, whose values are a
subset, so the claims cover every input the program can pass.

What this does NOT cover: `dump_line` and `dump` themselves (String building, `Bytes` reads,
`Console` output) are outside the extractable subset, so nothing here proves the whole line format;
and `PExpr` semantics are the proof model, not the compiled code (H19).
-/

namespace Examples.Hexdump.Proofs

open Concrete.Proof

/-- Spec = the extracted body of `hexdump.gutter_byte`. -/
def gutterByteExpr : PExpr :=
  .ifThenElse (.binOp .lt (.var "b") (.lit (.int 32)))
    (.lit (.int 46))
    (.ifThenElse (.binOp .gt (.var "b") (.lit (.int 126)))
      (.lit (.int 46))
      (.var "b"))

/-- Spec = the extracted body of `hexdump.line_len`. -/
def lineLenExpr : PExpr :=
  .ifThenElse (.binOp .gt (.var "remaining") (.lit (.int 16)))
    (.lit (.int 16))
    (.var "remaining")

set_option linter.unusedSimpArgs false in
/-- `gutter_byte(b)` is `b` when `b` is printable ASCII (0x20 ≤ b ≤ 0x7E) and '.' (0x2E)
    otherwise. -/
theorem gutter_byte_correct (b : Int) (fuel : Nat) :
    eval FnTable.empty (Env.empty.bind "b" (.int b)) (fuel + 3) gutterByteExpr
      = some (.int (if 0x20 ≤ b ∧ b ≤ 0x7E then b else 0x2E)) := by
  by_cases h1 : b < 32
  · have : ¬ (32 ≤ b ∧ b ≤ 126) := by omega
    simp [gutterByteExpr, eval, Env.bind, evalBinOp, h1, this]
  · by_cases h2 : b > 126
    · have : ¬ (32 ≤ b ∧ b ≤ 126) := by omega
      simp [gutterByteExpr, eval, Env.bind, evalBinOp, h1, h2, this]
    · have : 32 ≤ b ∧ b ≤ 126 := by omega
      simp [gutterByteExpr, eval, Env.bind, evalBinOp, h1, h2, this]

set_option linter.unusedSimpArgs false in
/-- `line_len(remaining)` is `min remaining 16`: the whole remainder when it fits in one line,
    otherwise a full 16-byte line. -/
theorem line_len_correct (remaining : Int) (fuel : Nat) :
    eval FnTable.empty (Env.empty.bind "remaining" (.int remaining)) (fuel + 2) lineLenExpr
      = some (.int (min remaining 16)) := by
  by_cases h : remaining > 16
  · have hm : min remaining 16 = 16 := by omega
    simp [lineLenExpr, eval, Env.bind, evalBinOp, h, hm]
  · have hm : min remaining 16 = remaining := by omega
    simp [lineLenExpr, eval, Env.bind, evalBinOp, h, hm]

/-! ## Specification parameters (bug 079) -/
def gutterByteSpec : SpecDecl := { params := ["b"], body := gutterByteExpr }
def lineLenSpec : SpecDecl := { params := ["remaining"], body := lineLenExpr }

end Examples.Hexdump.Proofs
