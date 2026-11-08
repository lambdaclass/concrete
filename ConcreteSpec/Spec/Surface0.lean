/-!
# Settled pieces of the slice-0 surface

Only UTF-8 source validity, NUL/BOM refusal, ASCII identifier characters and line comments
are specified here. Tokens, keywords, punctuation, literals, other whitespace, grammar,
diagnostics and CLI spellings are not defined. There is no shebang or block-comment rule.
-/

namespace ConcreteSpec.Surface0
namespace Utf8

/-- A Unicode scalar value: below U+110000 and outside the surrogate interval. -/
def Scalar (c : Nat) : Prop := c < 0x110000 ∧ (c < 0xD800 ∨ 0xE000 ≤ c)

/-- Table 3-7's one-byte row. -/
def One (a : UInt8) : Prop := a.toNat ≤ 0x7F

/-- Table 3-7's two-byte row (C2..DF, 80..BF). -/
def Two (a b : UInt8) : Prop :=
  0xC2 ≤ a.toNat ∧ a.toNat ≤ 0xDF ∧ 0x80 ≤ b.toNat ∧ b.toNat ≤ 0xBF

/-- Table 3-7's three-byte rows, including the E0 and ED restrictions. -/
def Three (a b c : UInt8) : Prop :=
  0xE0 ≤ a.toNat ∧ a.toNat ≤ 0xEF ∧
  0x80 ≤ b.toNat ∧ b.toNat ≤ 0xBF ∧ 0x80 ≤ c.toNat ∧ c.toNat ≤ 0xBF ∧
  (a.toNat = 0xE0 → 0xA0 ≤ b.toNat) ∧ (a.toNat = 0xED → b.toNat ≤ 0x9F)

/-- Table 3-7's four-byte rows, including the F0 and F4 restrictions. -/
def Four (a b c d : UInt8) : Prop :=
  0xF0 ≤ a.toNat ∧ a.toNat ≤ 0xF4 ∧
  0x80 ≤ b.toNat ∧ b.toNat ≤ 0xBF ∧ 0x80 ≤ c.toNat ∧ c.toNat ≤ 0xBF ∧
  0x80 ≤ d.toNat ∧ d.toNat ≤ 0xBF ∧
  (a.toNat = 0xF0 → 0x90 ≤ b.toNat) ∧ (a.toNat = 0xF4 → b.toNat ≤ 0x8F)

/-- A scalar's encoding, expressed as byte ranges and the positional value equation;
this judgment does not run a decoder. All other lengths are excluded. -/
def Encodes (s : Nat) (bs : List UInt8) : Prop :=
  match bs with
  | [a] => One a ∧ s = a.toNat
  | [a, b] => Two a b ∧ s = (a.toNat - 0xC0) * 64 + (b.toNat - 0x80)
  | [a, b, c] => Three a b c ∧
      s = (a.toNat - 0xE0) * 4096 + (b.toNat - 0x80) * 64 + (c.toNat - 0x80)
  | [a, b, c, d] => Four a b c d ∧
      s = (a.toNat - 0xF0) * 262144 + (b.toNat - 0x80) * 4096 +
        (c.toNat - 0x80) * 64 + (d.toNat - 0x80)
  | _ => False

/-- Bytes decode to scalars exactly when they concatenate their encodings in order. -/
inductive Decodes : List UInt8 → List Nat → Prop
  | nil : Decodes [] []
  | cons {s : Nat} {chunk bs : List UInt8} {cs : List Nat} :
      Encodes s chunk → Decodes bs cs → Decodes (chunk ++ bs) (s :: cs)

end Utf8

/-- A source has a decoding with neither NUL nor BOM anywhere. -/
def SourceOk (bs : List UInt8) : Prop :=
  ∃ cs, Utf8.Decodes bs cs ∧ ∀ c ∈ cs, c ≠ 0 ∧ c ≠ 0xFEFF

/-- ASCII letters and underscore start identifiers. -/
def IdentStart (c : Nat) : Prop :=
  (65 ≤ c ∧ c ≤ 90) ∨ (97 ≤ c ∧ c ≤ 122) ∨ c = 95

/-- Only ASCII letters, underscore and digits continue identifiers. -/
def IdentContinue (c : Nat) : Prop := IdentStart c ∨ (48 ≤ c ∧ c ≤ 57)

/-- A comment body ends before the first LF or CR, or at EOF. The terminating line
break stays in the remainder; every body scalar differs from both terminators. -/
inductive CommentBody : List Nat → List Nat → List Nat → Prop
  | eof : CommentBody [] [] []
  | lf (rest : List Nat) : CommentBody (10 :: rest) [] (10 :: rest)
  | cr (rest : List Nat) : CommentBody (13 :: rest) [] (13 :: rest)
  | cons {c : Nat} {input body rest : List Nat} : c ≠ 10 → c ≠ 13 →
      CommentBody input body rest → CommentBody (c :: input) (c :: body) rest

/-- A line comment starts with two slashes. The output pair is the whole comment
(including its slashes) and the unconsumed remainder. No other opener is admitted. -/
inductive LineComment : List Nat → (List Nat × List Nat) → Prop
  | intro {input body rest : List Nat} : CommentBody input body rest →
      LineComment (47 :: 47 :: input) (47 :: 47 :: body, rest)

/-- Scalar membership is decidable by its interval bounds. -/
instance (c : Nat) : Decidable (Utf8.Scalar c) := inferInstanceAs (Decidable (_ ∧ _))
/-- Identifier-start membership is decidable by ASCII bounds. -/
instance (c : Nat) : Decidable (IdentStart c) := inferInstanceAs (Decidable (_ ∨ _))
/-- Identifier-continuation membership is decidable by ASCII bounds. -/
instance (c : Nat) : Decidable (IdentContinue c) := inferInstanceAs (Decidable (_ ∨ _))
/-- The one-byte range constraint is decidable. -/
instance (a : UInt8) : Decidable (Utf8.One a) := inferInstanceAs (Decidable (_ ≤ _))
/-- The two-byte range constraints are decidable. -/
instance (a b : UInt8) : Decidable (Utf8.Two a b) := inferInstanceAs (Decidable (_ ∧ _))
/-- The three-byte range constraints are decidable. -/
instance (a b c : UInt8) : Decidable (Utf8.Three a b c) := inferInstanceAs (Decidable (_ ∧ _))
/-- The four-byte range constraints are decidable. -/
instance (a b c d : UInt8) : Decidable (Utf8.Four a b c d) :=
  inferInstanceAs (Decidable (_ ∧ _))

/-- Encoding membership is decidable directly from the range constraints. -/
instance (c : Nat) (bs : List UInt8) : Decidable (Utf8.Encodes c bs) := by
  unfold Utf8.Encodes
  split <;> infer_instance

example : Utf8.Encodes 65 [65] := by decide
example : Utf8.Encodes 0xA2 [0xC2, 0xA2] := by decide
example : Utf8.Encodes 0x20AC [0xE2, 0x82, 0xAC] := by decide
example : Utf8.Encodes 0x1F600 [0xF0, 0x9F, 0x98, 0x80] := by decide
example : ¬ Utf8.Encodes 0 [0xC0, 0x80] := by decide
example : ¬ Utf8.Encodes 0xD800 [0xED, 0xA0, 0x80] := by decide
example : ¬ Utf8.Encodes 0x110000 [0xF4, 0x90, 0x80, 0x80] := by decide
example : ¬ Utf8.Encodes 0x20AC [0xE2, 0x82] := by decide
example : ¬ Utf8.Encodes 0x80 [0x80] := by decide
example : Utf8.Encodes 0 [0] := by decide
example : Utf8.Encodes 0xFEFF [0xEF, 0xBB, 0xBF] := by decide

example : Utf8.Scalar 0x10FFFF := by decide
example : ¬ Utf8.Scalar 0xD800 := by decide
example : IdentStart 95 ∧ IdentContinue 48 ∧ ¬ IdentStart 48 := by decide
example : ¬ IdentContinue 0xE9 := by decide

example : LineComment [47, 47, 32,120,10,114] ([47, 47, 32,120], [10,114]) :=
  .intro (.cons (by decide) (by decide) (.cons (by decide) (by decide) (.lf _)))
example : LineComment [47, 47, 32,120,13] ([47, 47, 32,120], [13]) :=
  .intro (.cons (by decide) (by decide) (.cons (by decide) (by decide) (.cr _)))
example : LineComment [47, 47, 32,120] ([47, 47, 32,120], []) :=
  .intro (.cons (by decide) (by decide) (.cons (by decide) (by decide) .eof))
example (out : List Nat × List Nat) : ¬ LineComment [47, 42, 32,120,32,42,47] out := by
  intro h; cases h

end ConcreteSpec.Surface0
