import Concrete.Proof.Proof

/-!
# toml — lexer character-class proofs

The TOML workload (`examples/toml`) lexes with four pure character-class predicates. Each
specification below is the EXTRACTED body of that predicate (as `--report lean-stubs` emits it),
and each theorem says EXACTLY which code points the predicate accepts, stated independently of how
the body tests them. Three of those sets are TOML v1.0.0 ABNF character classes; the fourth,
`is_newline`, is the lexer's per-character newline test, which is NOT the ABNF `newline` rule:

| predicate          | accepts exactly                                        |
|--------------------|--------------------------------------------------------|
| `is_ws`            | ABNF `wschar = %x20 / %x09`                            |
| `is_newline`       | LF (0x0A) or CR (0x0D), each on its own. ABNF `newline = %x0A / %x0D.0A` is a two-character sequence for CRLF; this predicate accepts a lone CR and checks no sequencing, so the theorem establishes nothing about CRLF handling |
| `is_digit`         | ABNF `DIGIT = %x30-39`                                 |
| `is_bare_key_char` | one character of ABNF `unquoted-key = 1*( ALPHA / DIGIT / %x2D / %x5F )`, `ALPHA = %x41-5A / %x61-7A` |

The theorems quantify over every `Int`, not only over code points: the model's `ch` is the
mathematical integer the source's `Int` parameter denotes, and the claim holds for all of them.

What this does NOT cover: the callers (`skip_ws`, `parse_key`, `parse_integer`, ...) are not
extractable (String reads, unbounded loops), so nothing here says the parser as a whole accepts
the TOML grammar; and `PExpr` semantics are the proof model, not the compiled code (H19).
-/

namespace Examples.Toml.Proofs

open Concrete.Proof

/-- Spec = the extracted body of `toml_parser.is_ws`. -/
def isWsExpr : PExpr :=
  .ifThenElse (.binOp .eq (.var "ch") (.lit (.int 32)))
    (.lit (.bool true))
    (.binOp .eq (.var "ch") (.lit (.int 9)))

/-- Spec = the extracted body of `toml_parser.is_newline`. -/
def isNewlineExpr : PExpr :=
  .ifThenElse (.binOp .eq (.var "ch") (.lit (.int 10)))
    (.lit (.bool true))
    (.binOp .eq (.var "ch") (.lit (.int 13)))

/-- Spec = the extracted body of `toml_parser.is_digit`. -/
def isDigitExpr : PExpr :=
  .ifThenElse (.binOp .lt (.var "ch") (.lit (.int 48)))
    (.lit (.bool false))
    (.binOp .le (.var "ch") (.lit (.int 57)))

/-- Spec = the extracted body of `toml_parser.is_bare_key_char`. -/
def isBareKeyCharExpr : PExpr :=
  .ifThenElse (.binOp .lt (.var "ch") (.lit (.int 45))) (.lit (.bool false))
  (.ifThenElse (.binOp .eq (.var "ch") (.lit (.int 45))) (.lit (.bool true))
  (.ifThenElse (.binOp .lt (.var "ch") (.lit (.int 48))) (.lit (.bool false))
  (.ifThenElse (.binOp .le (.var "ch") (.lit (.int 57))) (.lit (.bool true))
  (.ifThenElse (.binOp .lt (.var "ch") (.lit (.int 65))) (.lit (.bool false))
  (.ifThenElse (.binOp .le (.var "ch") (.lit (.int 90))) (.lit (.bool true))
  (.ifThenElse (.binOp .eq (.var "ch") (.lit (.int 95))) (.lit (.bool true))
  (.ifThenElse (.binOp .lt (.var "ch") (.lit (.int 97))) (.lit (.bool false))
  (.binOp .le (.var "ch") (.lit (.int 122))))))))))

set_option linter.unusedSimpArgs false in
/-- `is_ws(ch)` is true exactly for the TOML `wschar` class: space (0x20) or tab (0x09). -/
theorem is_ws_correct (ch : Int) (fuel : Nat) :
    eval FnTable.empty (Env.empty.bind "ch" (.int ch)) (fuel + 2) isWsExpr
      = some (.bool (decide (ch = 0x20 ∨ ch = 0x09))) := by
  by_cases h1 : ch = 32
  · subst h1; simp [isWsExpr, eval, Env.bind, evalBinOp]
  · have e1 : (ch == 32) = false := by simp [h1]
    by_cases h2 : ch = 9
    · subst h2; simp [isWsExpr, eval, Env.bind, evalBinOp]
    · have e2 : (ch == 9) = false := by simp [h2]
      simp [isWsExpr, eval, Env.bind, evalBinOp, h1, h2, e1, e2]

set_option linter.unusedSimpArgs false in
/-- `is_newline(ch)` is true exactly for line feed (0x0A) and carriage return (0x0D). -/
theorem is_newline_correct (ch : Int) (fuel : Nat) :
    eval FnTable.empty (Env.empty.bind "ch" (.int ch)) (fuel + 2) isNewlineExpr
      = some (.bool (decide (ch = 0x0A ∨ ch = 0x0D))) := by
  by_cases h1 : ch = 10
  · subst h1; simp [isNewlineExpr, eval, Env.bind, evalBinOp]
  · have e1 : (ch == 10) = false := by simp [h1]
    by_cases h2 : ch = 13
    · subst h2; simp [isNewlineExpr, eval, Env.bind, evalBinOp]
    · have e2 : (ch == 13) = false := by simp [h2]
      simp [isNewlineExpr, eval, Env.bind, evalBinOp, h1, h2, e1, e2]

set_option linter.unusedSimpArgs false in
/-- `is_digit(ch)` is true exactly for the ABNF `DIGIT` range 0x30-0x39 ('0'-'9'). -/
theorem is_digit_correct (ch : Int) (fuel : Nat) :
    eval FnTable.empty (Env.empty.bind "ch" (.int ch)) (fuel + 2) isDigitExpr
      = some (.bool (decide (0x30 ≤ ch ∧ ch ≤ 0x39))) := by
  by_cases h1 : ch < 48
  · have : ¬ (48 ≤ ch ∧ ch ≤ 57) := by omega
    simp [isDigitExpr, eval, Env.bind, evalBinOp, h1, this]
  · simp [isDigitExpr, eval, Env.bind, evalBinOp, h1]
    omega

set_option linter.unusedSimpArgs false in
/-- `is_bare_key_char(ch)` is true exactly for the characters TOML allows in an unquoted key:
    `ALPHA` (0x41-0x5A, 0x61-0x7A), `DIGIT` (0x30-0x39), '-' (0x2D) and '_' (0x5F). -/
theorem is_bare_key_char_correct (ch : Int) (fuel : Nat) :
    eval FnTable.empty (Env.empty.bind "ch" (.int ch)) (fuel + 9) isBareKeyCharExpr
      = some (.bool (decide ((0x41 ≤ ch ∧ ch ≤ 0x5A) ∨ (0x61 ≤ ch ∧ ch ≤ 0x7A) ∨
                             (0x30 ≤ ch ∧ ch ≤ 0x39) ∨ ch = 0x2D ∨ ch = 0x5F))) := by
  simp only [isBareKeyCharExpr, eval, Env.bind, evalBinOp]
  by_cases h1 : ch < 45
  · simp [h1]; omega
  by_cases h2 : ch = 45
  · subst h2; simp
  have e2 : (ch == 45) = false := by simp [h2]
  by_cases h3 : ch < 48
  · simp [h1, e2, h3]; omega
  by_cases h4 : ch ≤ 57
  · simp [h1, e2, h3, h4]; omega
  by_cases h5 : ch < 65
  · simp [h1, e2, h3, h4, h5]; omega
  by_cases h6 : ch ≤ 90
  · simp [h1, e2, h3, h4, h5, h6]; omega
  by_cases h7 : ch = 95
  · subst h7; simp
  have e7 : (ch == 95) = false := by simp [h7]
  by_cases h8 : ch < 97
  · simp [h1, e2, h3, h4, h5, h6, e7, h8]; omega
  · by_cases h9 : ch ≤ 122 <;> simp [h1, e2, h3, h4, h5, h6, e7, h8, h9] <;> omega

/-! ## Specification parameters (bug 079)

The subject binding compares a claimed function's parameters with its specification's. These
specifications have no `PFnDef`, so their inputs are declared here, by name and in order. -/
def isWsSpec : SpecDecl := { params := ["ch"], body := isWsExpr }
def isNewlineSpec : SpecDecl := { params := ["ch"], body := isNewlineExpr }
def isDigitSpec : SpecDecl := { params := ["ch"], body := isDigitExpr }
def isBareKeyCharSpec : SpecDecl := { params := ["ch"], body := isBareKeyCharExpr }

end Examples.Toml.Proofs
