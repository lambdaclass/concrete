/-!
# Settled pieces of the slice-0 surface

UTF-8 source validity, NUL/BOM refusal, ASCII identifiers, the four settled whitespace scalars,
line comments and maximal trivia runs are specified here. String bodies have only the six decided
escapes and take the undecided treatment of other raw scalars as a parameter. Longest match is
parametric in an arbitrary table, and decimal integer lexemes refuse leading zeroes. Keywords,
reserved words, builtins, concrete punctuation and operators, suffixes, other literals, grammar,
diagnostics and CLI spellings are not defined. There is no shebang or block-comment rule; no other
Unicode whitespace is included.
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

/-- Exactly space, tab, LF and CR are whitespace. -/
def Whitespace (c : Nat) : Prop := c = 32 ∨ c = 9 ∨ c = 10 ∨ c = 13

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

/-- A maximal run of whitespace scalars and line comments. The result is the consumed trivia
and the unconsumed remainder; the remainder starts with neither whitespace nor two slashes. -/
inductive Trivia : List Nat → (List Nat × List Nat) → Prop
  | stop (rest : List Nat) :
      (∀ c cs, rest = c :: cs → ¬ Whitespace c) →
      (∀ cs, rest ≠ 47 :: 47 :: cs) →
      Trivia rest ([], rest)
  | whitespace {c : Nat} {input trivia rest : List Nat} :
      Whitespace c → Trivia input (trivia, rest) →
      Trivia (c :: input) (c :: trivia, rest)
  | comment {input comment rest trivia final : List Nat} :
      LineComment input (comment, rest) → Trivia rest (trivia, final) →
      Trivia input (comment ++ trivia, final)

/-- The longest continuation run after an identifier's first scalar. -/
inductive IdentBody : List Nat → List Nat → List Nat → Prop
  | stop (rest : List Nat) :
      (∀ c cs, rest = c :: cs → ¬ IdentContinue c) → IdentBody rest [] rest
  | cons {c : Nat} {input body rest : List Nat} : IdentContinue c →
      IdentBody input body rest → IdentBody (c :: input) (c :: body) rest

/-- An ASCII identifier is one start scalar followed by its maximal continuation run.
Lexemes are exact scalar lists, so case is significant. -/
inductive Identifier : List Nat → (List Nat × List Nat) → Prop
  | intro {c : Nat} {input body rest : List Nat} : IdentStart c → IdentBody input body rest →
      Identifier (c :: input) (c :: body, rest)

/-- The six decided string escapes and their decoded scalar values. -/
def StringEscape (escaped decoded : Nat) : Prop :=
  (escaped = 110 ∧ decoded = 10) ∨ (escaped = 116 ∧ decoded = 9) ∨
  (escaped = 114 ∧ decoded = 13) ∨ (escaped = 92 ∧ decoded = 92) ∨
  (escaped = 34 ∧ decoded = 34) ∨ (escaped = 48 ∧ decoded = 0)

/-- A string body after its opening quote. Raw scalars other than quote, backslash, LF and CR
are governed by `rawOk`; this parameter chooses no rule for them. EOF and raw line breaks cannot
end a body, and only `StringEscape` permits a backslash. -/
inductive StringBody (rawOk : Nat → Prop) : List Nat → (List Nat × List Nat) → Prop
  | close (rest : List Nat) : StringBody rawOk (34 :: rest) ([], rest)
  | raw {c : Nat} {input decoded rest : List Nat} :
      c ≠ 34 → c ≠ 92 → c ≠ 10 → c ≠ 13 → rawOk c → StringBody rawOk input (decoded, rest) →
      StringBody rawOk (c :: input) (c :: decoded, rest)
  | escape {escaped decodedScalar : Nat} {input decoded rest : List Nat} :
      StringEscape escaped decodedScalar → StringBody rawOk input (decoded, rest) →
      StringBody rawOk (92 :: escaped :: input) (decodedScalar :: decoded, rest)

/-- Longest match over an arbitrary table. Empty entries are not tokens. A successful result
is a nonempty table entry and its unconsumed suffix, and no matching entry is longer. `none`
says exactly that no nonempty entry is a prefix. This defines no Concrete token table. -/
inductive LongestMatch (table : List (List Nat)) :
    List Nat → Option (List Nat × List Nat) → Prop
  | some {input token rest : List Nat} : token ∈ table → token ≠ [] → input = token ++ rest →
      (∀ other ∈ table, ∀ tail, input = other ++ tail → other.length ≤ token.length) →
      LongestMatch table input (some (token, rest))
  | none {input : List Nat} :
      (∀ token ∈ table, token ≠ [] → ∀ rest, input ≠ token ++ rest) →
      LongestMatch table input none

/-- Exactly ASCII digits zero through nine. -/
def DecimalDigit (c : Nat) : Prop := 48 ≤ c ∧ c ≤ 57

/-- The maximal decimal-digit run at the front of an input. -/
inductive DigitRun : List Nat → List Nat → List Nat → Prop
  | stop (rest : List Nat) : (∀ c cs, rest = c :: cs → ¬ DecimalDigit c) → DigitRun rest [] rest
  | cons {c : Nat} {input digits rest : List Nat} : DecimalDigit c → DigitRun input digits rest →
      DigitRun (c :: input) (c :: digits) rest

/-- A nonempty digit run is accepted exactly for `0` alone or a nonzero first digit. -/
def DigitsAccepted : List Nat → Prop
  | [] => False
  | [48] => True
  | 48 :: _ => False
  | _ :: _ => True

/-- Positional decimal value; the scanner-independent digit run supplies only ASCII digits. -/
def decimalValue (digits : List Nat) : Nat :=
  digits.foldl (fun value c => value * 10 + (c - 48)) 0

/-- A maximal accepted decimal-digit lexeme, its value and its remainder. Nothing is specified
about the remainder beyond the digit run's maximality. -/
inductive DecimalInteger : List Nat → (List Nat × Nat × List Nat) → Prop
  | intro {input digits rest : List Nat} {value : Nat} : DigitRun input digits rest →
      DigitsAccepted digits → value = decimalValue digits → DecimalInteger input (digits, value, rest)

/-- Scalar membership is decidable by its interval bounds. -/
instance (c : Nat) : Decidable (Utf8.Scalar c) := inferInstanceAs (Decidable (_ ∧ _))
/-- Whitespace membership is decidable by the four scalar equalities. -/
instance (c : Nat) : Decidable (Whitespace c) := inferInstanceAs (Decidable (_ ∨ _))
/-- Identifier-start membership is decidable by ASCII bounds. -/
instance (c : Nat) : Decidable (IdentStart c) := inferInstanceAs (Decidable (_ ∨ _))
/-- Identifier-continuation membership is decidable by ASCII bounds. -/
instance (c : Nat) : Decidable (IdentContinue c) := inferInstanceAs (Decidable (_ ∨ _))
/-- The escape relation is decidable by its six scalar equalities. -/
instance (escaped decoded : Nat) : Decidable (StringEscape escaped decoded) :=
  inferInstanceAs (Decidable (_ ∨ _))
/-- Decimal-digit membership is decidable by its interval bounds. -/
instance (c : Nat) : Decidable (DecimalDigit c) := inferInstanceAs (Decidable (_ ∧ _))
/-- Leading-zero acceptance is decidable by the digit-list shape. -/
instance (digits : List Nat) : Decidable (DigitsAccepted digits) := by
  unfold DigitsAccepted
  split <;> infer_instance
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
