import ConcreteSpec.Spec.Borrow

/-!
# Settled pieces of the slice-0 surface

**PROPOSED: not normative until the owner freezes Core (ROADMAP A2c).**
Part of ConcreteSpec.Spec; see ConcreteSpec/README.md.

Authority: docs/decisions.md, owner-approved language list item 30 (OPEN-1),
SYN-5@3 and SYN-6@3. Core Q4.2 permits embedded NUL in static text VALUES
(e.g. through the `\0` escape), not in raw source bytes. Item 30 says no BOM;
together with the owner’s no-hidden-surprises ruling (2026-11-08; see also
docs/PHILOSOPHY.md, principle 3, transparency) this excludes U+FEFF anywhere,
including inside comments and literals. Item 30 also makes bare CR end a comment.

Only UTF-8 source validity, NUL/BOM refusal, ASCII identifier characters and line comments
are specified here. Tokens, keywords, punctuation, literals, other whitespace, grammar,
diagnostics and CLI spellings are not defined. There is no shebang or block-comment rule.
-/

namespace ConcreteSpec.Surface0
namespace Utf8

/-- Decision: owner-approved language list item 30 (OPEN-1), SYN-5@3 / SYN-6@3.
 A Unicode scalar value: below U+110000 and outside the surrogate interval. -/
def Scalar (c : Nat) : Prop := c < 0x110000 ∧ (c < 0xD800 ∨ 0xE000 ≤ c)

/-- Decision: item 30 (OPEN-1), SYN-5@3. The shared UTF-8 one-byte row. -/
def One (a : UInt8) : Prop := Borrow.isScalar [a.toNat] = true
/-- Decision: item 30 (OPEN-1), SYN-5@3. The shared UTF-8 two-byte row. -/
def Two (a b : UInt8) : Prop := Borrow.isScalar [a.toNat, b.toNat] = true
/-- Decision: item 30 (OPEN-1), SYN-5@3. The shared UTF-8 three-byte rows. -/
def Three (a b c : UInt8) : Prop := Borrow.isScalar [a.toNat, b.toNat, c.toNat] = true
/-- Decision: item 30 (OPEN-1), SYN-5@3. The shared UTF-8 four-byte rows. -/
def Four (a b c d : UInt8) : Prop :=
  Borrow.isScalar [a.toNat, b.toNat, c.toNat, d.toNat] = true

/-- Decision: owner-approved language list item 30 (OPEN-1), SYN-5@3 / SYN-6@3.
 A scalar's encoding, expressed as byte ranges and the positional value equation;
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

/-- Decision: owner-approved language list item 30 (OPEN-1), SYN-5@3 / SYN-6@3.
 Bytes decode to scalars exactly when they concatenate their encodings in order. -/
inductive Decodes : List UInt8 → List Nat → Prop
  | nil : Decodes [] []
  | cons {s : Nat} {chunk bs : List UInt8} {cs : List Nat} :
      Encodes s chunk → Decodes bs cs → Decodes (chunk ++ bs) (s :: cs)

end Utf8

/-- Decision: item 30 (OPEN-1), SYN-5@3. A partial scalar admits a
well-formed continuation exactly when it is a prefix of an encoding. -/
def ScalarPrefix (bytes : List UInt8) : Prop :=
  ∃ suffix value, Utf8.Encodes value (bytes ++ suffix)

/-- Decision: item 30 (OPEN-1), SYN-5@3. A local first failure: all shorter
prefixes can still become one scalar, and the prefix before failure is unfinished.
At EOF the scalar is unfinished; otherwise
the byte at `offset` makes any well-formed continuation impossible. -/
def ScalarFailure (bytes : List UInt8) (offset : Nat) : Prop :=
  (∀ j ≤ offset, ScalarPrefix (bytes.take j)) ∧
  (¬ ∃ value, Utf8.Encodes value (bytes.take offset)) ∧
  ((offset < bytes.length ∧ ¬ ScalarPrefix (bytes.take (offset + 1))) ∨
   (offset = bytes.length ∧ ¬ ∃ value, Utf8.Encodes value bytes))

/-- Decision: item 30 (OPEN-1), SYN-5@3. The first failed scalar follows a
well-formed byte prefix. The offset is the first byte where no scalar continuation
exists, or EOF when an otherwise extendible scalar is incomplete. -/
def FirstError (bytes : List UInt8) (offset : Nat) : Prop :=
  ∃ pre rest cs localOffset,
    bytes = pre ++ rest ∧ Utf8.Decodes pre cs ∧
    ScalarFailure rest localOffset ∧ rest ≠ [] ∧ offset = pre.length + localOffset

/-- Decision: item 30 (OPEN-1), SYN-5@3. The first raw-source failure:
an invalid scalar, NUL, or U+FEFF, following only permitted complete scalars.
NUL/BOM offsets name the start of the refused scalar. -/
inductive SourceFailure : List UInt8 → Nat → Prop
  | utf8 {bytes : List UInt8} {k : Nat} : bytes ≠ [] → ScalarFailure bytes k → SourceFailure bytes k
  | nul {chunk rest : List UInt8} : Utf8.Encodes 0 chunk → SourceFailure (chunk ++ rest) 0
  | bom {chunk rest : List UInt8} : Utf8.Encodes 0xFEFF chunk → SourceFailure (chunk ++ rest) 0
  | cons {c : Nat} {chunk rest : List UInt8} {k : Nat} :
      Utf8.Encodes c chunk → c ≠ 0 → c ≠ 0xFEFF → SourceFailure rest k →
      SourceFailure (chunk ++ rest) (chunk.length + k)

/-- Decision: owner-approved language list item 30 (OPEN-1), SYN-5@3 / SYN-6@3.
 A source has a decoding with neither NUL nor BOM anywhere. -/
def SourceOk (bs : List UInt8) : Prop :=
  ∃ cs, Utf8.Decodes bs cs ∧ ∀ c ∈ cs, c ≠ 0 ∧ c ≠ 0xFEFF

/-- Decision: owner-approved language list item 30 (OPEN-1), SYN-5@3 / SYN-6@3.
 ASCII letters and underscore start identifiers. -/
def IdentStart (c : Nat) : Prop :=
  (65 ≤ c ∧ c ≤ 90) ∨ (97 ≤ c ∧ c ≤ 122) ∨ c = 95

/-- Decision: owner-approved language list item 30 (OPEN-1), SYN-5@3 / SYN-6@3.
 Only ASCII letters, underscore and digits continue identifiers. -/
def IdentContinue (c : Nat) : Prop := IdentStart c ∨ (48 ≤ c ∧ c ≤ 57)

/-- Decision: item 30 (OPEN-1), SYN-6@3. Skip through the first LF, CR or EOF;
the terminator stays in the suffix. No discarded body is constructed. -/
inductive CommentBody : List Nat → List Nat → Prop
  | eof : CommentBody [] []
  | lf (rest : List Nat) : CommentBody (10 :: rest) (10 :: rest)
  | cr (rest : List Nat) : CommentBody (13 :: rest) (13 :: rest)
  | cons {c : Nat} {input rest : List Nat} : c ≠ 10 → c ≠ 13 →
      CommentBody input rest → CommentBody (c :: input) rest

/-- Decision: item 30 (OPEN-1), SYN-6@3. Two slashes open a line comment;
the result is its unconsumed suffix. -/
inductive LineComment : List Nat → List Nat → Prop
  | intro {input rest : List Nat} : CommentBody input rest →
      LineComment (47 :: 47 :: input) rest

/-- Decision: owner-approved language list item 30 (OPEN-1), SYN-5@3 / SYN-6@3.
 Scalar membership is decidable by its interval bounds. -/
instance (c : Nat) : Decidable (Utf8.Scalar c) := inferInstanceAs (Decidable (_ ∧ _))
/-- Decision: owner-approved language list item 30 (OPEN-1), SYN-5@3 / SYN-6@3.
 Identifier-start membership is decidable by ASCII bounds. -/
instance (c : Nat) : Decidable (IdentStart c) := inferInstanceAs (Decidable (_ ∨ _))
/-- Decision: owner-approved language list item 30 (OPEN-1), SYN-5@3 / SYN-6@3.
 Identifier-continuation membership is decidable by ASCII bounds. -/
instance (c : Nat) : Decidable (IdentContinue c) := inferInstanceAs (Decidable (_ ∨ _))
/-- Decision: item 30 (OPEN-1). Reflection in the shared scalar checker. -/
instance (a : UInt8) : Decidable (Utf8.One a) := inferInstanceAs (Decidable (_ = true))
/-- Decision: item 30 (OPEN-1). Reflection in the shared scalar checker. -/
instance (a b : UInt8) : Decidable (Utf8.Two a b) := inferInstanceAs (Decidable (_ = true))
/-- Decision: item 30 (OPEN-1). Reflection in the shared scalar checker. -/
instance (a b c : UInt8) : Decidable (Utf8.Three a b c) := inferInstanceAs (Decidable (_ = true))
/-- Decision: item 30 (OPEN-1). Reflection in the shared scalar checker. -/
instance (a b c d : UInt8) : Decidable (Utf8.Four a b c d) :=
  inferInstanceAs (Decidable (_ = true))

/-- Decision: owner-approved language list item 30 (OPEN-1), SYN-5@3 / SYN-6@3.
 Encoding membership is decidable directly from the range constraints. -/
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

example : LineComment [47, 47, 32,120,10,114] [10,114] :=
  .intro (.cons (by decide) (by decide) (.cons (by decide) (by decide) (.lf _)))
example : LineComment [47, 47, 32,120,13] [13] :=
  .intro (.cons (by decide) (by decide) (.cons (by decide) (by decide) (.cr _)))
example : LineComment [47, 47, 32,120] [] :=
  .intro (.cons (by decide) (by decide) (.cons (by decide) (by decide) .eof))
example (out : List Nat) : ¬ LineComment [47, 42, 32,120,32,42,47] out := by
  intro h; cases h

end ConcreteSpec.Surface0
