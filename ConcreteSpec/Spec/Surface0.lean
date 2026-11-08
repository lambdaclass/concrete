import ConcreteSpec.Spec.Borrow
import ConcreteSpec.Spec.Slice1Syntax

/-!
# Settled pieces of the slice-0 surface

**PROPOSED: not normative until the owner freezes Core (ROADMAP A2c).**
Part of ConcreteSpec.Spec; see ConcreteSpec/README.md.

Authority: docs/decisions.md, owner-approved language list item 30 (OPEN-1),
SYN-5@3 and SYN-6@3, and the literal decisions recorded on 2026-11-08.
Core Q4.2 permits embedded NUL in static text VALUES (e.g. through the `\0`
escape), not in raw source bytes. Item 30 and the owner's no-hidden-surprises
ruling exclude U+FEFF anywhere, including comments and literals. Bare CR ends
a comment.

UTF-8 source validity, NUL/BOM refusal, ASCII identifiers, the four settled whitespace scalars,
line comments and maximal trivia runs are specified here. Strings and comments refuse raw Cc,
U+2028/U+2029 and bidi-control scalars, except that comments permit raw TAB; LF and CR end comments.
Byte literals admit only printable-ASCII raw scalars and exactly six escapes; labels are quote
followed by an identifier. Longest match is parametric in an arbitrary table. Integers have the
lowercase `0x`/`0b`/`0o` basePrefixes, lowercase hexadecimal digits and eight optional integer-type
suffixes. Decimal leading zeroes are refused, and malformed adjacent identifier-continuation text
is one refused token rather than an integer followed by an identifier. Keywords, reserved words,
builtins, concrete punctuation and operators, float literals, grammar, diagnostics and CLI
spellings are not defined. There is no shebang or block-comment rule; no other Unicode whitespace
is included.
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
 A scalar's encoding combines Borrow.isScalar with the positional value equation;
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

/-- Exactly space, tab, LF and CR are whitespace. -/
def Whitespace (c : Nat) : Prop := c = 32 ∨ c = 9 ∨ c = 10 ∨ c = 13

/-- ASCII letters and underscore start identifiers. -/
def IdentStart (c : Nat) : Prop :=
  (65 ≤ c ∧ c ≤ 90) ∨ (97 ≤ c ∧ c ≤ 122) ∨ c = 95

/-- Decision: owner-approved language list item 30 (OPEN-1), SYN-5@3 / SYN-6@3.
 Only ASCII letters, underscore and digits continue identifiers. -/
def IdentContinue (c : Nat) : Prop := IdentStart c ∨ (48 ≤ c ∧ c ≤ 57)

/-- Unicode general category Cc: C0, DEL and C1. -/
def Control (c : Nat) : Prop := c ≤ 0x1F ∨ (0x7F ≤ c ∧ c ≤ 0x9F)

/-- The bidi controls refused raw by the owner on 2026-11-08. -/
def BidiControl (c : Nat) : Prop :=
  c = 0x061C ∨ (0x200E ≤ c ∧ c ≤ 0x200F) ∨ (0x202A ≤ c ∧ c ≤ 0x202E) ∨
    (0x2066 ≤ c ∧ c ≤ 0x2069)

/-- Raw scalars forbidden in both strings and comments. -/
def ForbiddenRaw (c : Nat) : Prop :=
  Control c ∨ c = 0x2028 ∨ c = 0x2029 ∨ BidiControl c

/-- A raw comment scalar: TAB alone is exempt from `ForbiddenRaw`. LF and CR are not raw body
scalars because they terminate the comment. -/
def CommentRaw (c : Nat) : Prop := c = 9 ∨ ¬ ForbiddenRaw c

/-- A comment body ends before the first LF or CR, or at EOF. The terminating line
break stays in the suffix; every body scalar is permitted raw and differs from both
terminators. No discarded body is constructed. -/
inductive CommentBody : List Nat → List Nat → Prop
  | eof : CommentBody [] []
  | lf (rest : List Nat) : CommentBody (10 :: rest) (10 :: rest)
  | cr (rest : List Nat) : CommentBody (13 :: rest) (13 :: rest)
  | cons {c : Nat} {input rest : List Nat} : c ≠ 10 → c ≠ 13 → CommentRaw c →
      CommentBody input rest → CommentBody (c :: input) rest

/-- Two slashes open a line comment; only its unconsumed suffix is returned. -/
inductive LineComment : List Nat → List Nat → Prop
  | intro {input rest : List Nat} : CommentBody input rest →
      LineComment (47 :: 47 :: input) rest

/-- A maximal run of whitespace and line comments; only the unconsumed suffix is returned. -/
inductive Trivia : List Nat → List Nat → Prop
  | stop (rest : List Nat) :
      (∀ c cs, rest = c :: cs → ¬ Whitespace c) →
      (∀ cs, rest ≠ 47 :: 47 :: cs) → Trivia rest rest
  | whitespace {c : Nat} {input rest : List Nat} :
      Whitespace c → Trivia input rest → Trivia (c :: input) rest
  | comment {input rest final : List Nat} :
      LineComment input rest → Trivia rest final → Trivia input final

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

/-- A raw string scalar excludes its delimiters and every owner-forbidden raw scalar. -/
def StringRaw (c : Nat) : Prop := c ≠ 34 ∧ c ≠ 92 ∧ ¬ ForbiddenRaw c

/-- A string body after its opening quote. EOF cannot end a body, raw forbidden scalars are
refused, and only `StringEscape` permits a backslash. -/
inductive StringBody : List Nat → (List Nat × List Nat) → Prop
  | close (rest : List Nat) : StringBody (34 :: rest) ([], rest)
  | raw {c : Nat} {input decoded rest : List Nat} :
      StringRaw c → StringBody input (decoded, rest) → StringBody (c :: input) (c :: decoded, rest)
  | escape {escaped decodedScalar : Nat} {input decoded rest : List Nat} :
      StringEscape escaped decodedScalar → StringBody input (decoded, rest) →
      StringBody (92 :: escaped :: input) (decodedScalar :: decoded, rest)

/-- A raw byte scalar is U+0020 through U+007E, except single quote and backslash. -/
def ByteRaw (c : Nat) : Prop := 32 ≤ c ∧ c ≤ 126 ∧ c ≠ 39 ∧ c ≠ 92

/-- Exactly the six byte escapes decided by the owner on 2026-11-08: `\n`, `\t`, `\r`, `\0`,
`\\` and `\'`. In particular, `\"` and `\xHH` are not included. -/
def ByteEscape (escaped decoded : Nat) : Prop :=
  (escaped = 110 ∧ decoded = 10) ∨ (escaped = 116 ∧ decoded = 9) ∨
  (escaped = 114 ∧ decoded = 13) ∨ (escaped = 92 ∧ decoded = 92) ∨
  (escaped = 39 ∧ decoded = 39) ∨ (escaped = 48 ∧ decoded = 0)

/-- An ASCII byte literal and its `u8` mathematical value. A closing quote after one raw scalar
or one escape distinguishes it from a label; escaped values must fit in eight bits. -/
inductive ByteLiteral : List Nat → (List Nat × Nat × List Nat) → Prop
  | raw {c : Nat} {rest : List Nat} : ByteRaw c →
      ByteLiteral (39 :: c :: 39 :: rest) ([39, c, 39], c, rest)
  | escaped {escaped decoded : Nat} {rest : List Nat} : ByteEscape escaped decoded → decoded < 256 →
      ByteLiteral (39 :: 92 :: escaped :: 39 :: rest)
        ([39, 92, escaped, 39], decoded, rest)

/-- A label is a single quote followed by an ASCII identifier (SYN-6). -/
inductive Label : List Nat → (List Nat × List Nat) → Prop
  | intro {input name rest : List Nat} : Identifier input (name, rest) →
      Label (39 :: input) (39 :: name, rest)

/-- Longest match over an arbitrary table. Empty entries are not tokens. A successful result
is a nonempty table entry and its unconsumed suffix, and no matching entry is longer. `none`
says exactly that no nonempty entry is a basePrefix. This defines no Concrete token table. -/
inductive LongestMatch (table : List (List Nat)) :
    List Nat → Option (List Nat × List Nat) → Prop
  | some {input token rest : List Nat} : token ∈ table → token ≠ [] → input = token ++ rest →
      (∀ other ∈ table, ∀ tail, input = other ++ tail → other.length ≤ token.length) →
      LongestMatch table input (some (token, rest))
  | none {input : List Nat} :
      (∀ token ∈ table, token ≠ [] → ∀ rest, input ≠ token ++ rest) →
      LongestMatch table input none

/-- The four integer bases. Only lowercase `0b`, `0o` and `0x` introduce non-decimal digits. -/
inductive Radix where
  | decimal | binary | octal | hexadecimal
  deriving DecidableEq, Repr

namespace Radix

/-- The positional base. -/
def base : Radix → Nat
  | .decimal => 10 | .binary => 2 | .octal => 8 | .hexadecimal => 16

/-- The exact lowercase basePrefix; decimal has none. -/
def basePrefix : Radix → List Nat
  | .decimal => [] | .binary => [48, 98] | .octal => [48, 111] | .hexadecimal => [48, 120]

end Radix

/-- Digits valid for a base. Hexadecimal digits are lowercase only. -/
def BaseDigit (radix : Radix) (c : Nat) : Prop :=
  match radix with
  | .decimal => 48 ≤ c ∧ c ≤ 57
  | .binary => c = 48 ∨ c = 49
  | .octal => 48 ≤ c ∧ c ≤ 55
  | .hexadecimal => (48 ≤ c ∧ c ≤ 57) ∨ (97 ≤ c ∧ c ≤ 102)

/-- The maximal digit run for a base at the front of an input. -/
inductive DigitRun (radix : Radix) : List Nat → List Nat → List Nat → Prop
  | stop (rest : List Nat) : (∀ c cs, rest = c :: cs → ¬ BaseDigit radix c) → DigitRun radix rest [] rest
  | cons {c : Nat} {input digits rest : List Nat} : BaseDigit radix c → DigitRun radix input digits rest →
      DigitRun radix (c :: input) (c :: digits) rest

/-- Every base needs a digit; only decimal refuses a leading zero. -/
def DigitsAccepted (radix : Radix) : List Nat → Prop
  | [] => False
  | [48] => True
  | 48 :: _ => radix ≠ .decimal
  | _ :: _ => True

/-- Positional value in the selected base, with `a` through `f` valued ten through fifteen. -/
def positionalValue (radix : Radix) (digits : List Nat) : Nat :=
  digits.foldl (fun value c => value * radix.base +
    (if 97 ≤ c ∧ c ≤ 102 then c - 97 + 10 else c - 48)) 0

/-- The canonical suffix spelling of each decided integer type (TYPE-19). -/
def intTySpelling : ConcreteSpec.IntTy → List Nat
  | .i8 => [105, 56] | .i16 => [105, 49, 54] | .i32 => [105, 51, 50] | .i64 => [105, 54, 52]
  | .u8 => [117, 56] | .u16 => [117, 49, 54] | .u32 => [117, 51, 50] | .u64 => [117, 54, 52]

example : intTySpelling .i8 = "i8".toList.map Char.toNat := by decide
example : intTySpelling .i16 = "i16".toList.map Char.toNat := by decide
example : intTySpelling .i32 = "i32".toList.map Char.toNat := by decide
example : intTySpelling .i64 = "i64".toList.map Char.toNat := by decide
example : intTySpelling .u8 = "u8".toList.map Char.toNat := by decide
example : intTySpelling .u16 = "u16".toList.map Char.toNat := by decide
example : intTySpelling .u32 = "u32".toList.map Char.toNat := by decide
example : intTySpelling .u64 = "u64".toList.map Char.toNat := by decide
example : Radix.basePrefix .decimal = "".toList.map Char.toNat := by decide
example : Radix.basePrefix .binary = "0b".toList.map Char.toNat := by decide
example : Radix.basePrefix .octal = "0o".toList.map Char.toNat := by decide
example : Radix.basePrefix .hexadecimal = "0x".toList.map Char.toNat := by decide
example (e d : Nat) : ByteEscape e d ↔ (e = 'n'.toNat ∧ d = '\n'.toNat) ∨ (e = 't'.toNat ∧ d = '\t'.toNat) ∨ (e = 'r'.toNat ∧ d = '\r'.toNat) ∨ (e = '\\'.toNat ∧ d = '\\'.toNat) ∨ (e = '\''.toNat ∧ d = '\''.toNat) ∨ (e = '0'.toNat ∧ d = '\x00'.toNat) := by rfl
example (e d : Nat) : StringEscape e d ↔ (e = 'n'.toNat ∧ d = '\n'.toNat) ∨ (e = 't'.toNat ∧ d = '\t'.toNat) ∨ (e = 'r'.toNat ∧ d = '\r'.toNat) ∨ (e = '\\'.toNat ∧ d = '\\'.toNat) ∨ (e = '"'.toNat ∧ d = '"'.toNat) ∨ (e = '0'.toNat ∧ d = '\x00'.toNat) := by rfl

/-- The empty suffix or exactly one of the eight decided integer-type names. -/
def SuffixAccepted (suffix : List Nat) (typeSuffix : Option ConcreteSpec.IntTy) : Prop :=
  (suffix = [] ∧ typeSuffix = none) ∨
  (suffix = intTySpelling .i8 ∧ typeSuffix = some .i8) ∨
  (suffix = intTySpelling .i16 ∧ typeSuffix = some .i16) ∨
  (suffix = intTySpelling .i32 ∧ typeSuffix = some .i32) ∨
  (suffix = intTySpelling .i64 ∧ typeSuffix = some .i64) ∨
  (suffix = intTySpelling .u8 ∧ typeSuffix = some .u8) ∨
  (suffix = intTySpelling .u16 ∧ typeSuffix = some .u16) ∨
  (suffix = intTySpelling .u32 ∧ typeSuffix = some .u32) ∨
  (suffix = intTySpelling .u64 ∧ typeSuffix = some .u64)

/-- An integer token's exact lexeme, mathematical value, optional type suffix and remainder.

CHECKER OBLIGATION (owner, 2026-11-08): suffix typing is complementary, not lexical. The checker
must require a suffix where no OD138 expected-type position exists and refuse one where such a
position does exist. Q6.2's example consequently becomes `S { a: 200 }`. This lexer only records
the optional suffix. -/
structure IntegerToken where
  lexeme : List Nat
  value : Nat
  suffix : Option ConcreteSpec.IntTy
  rest : List Nat
  deriving DecidableEq, Repr

/-- A based digit run followed by its maximal identifier-continuation suffix candidate. The whole
candidate must be accepted, so malformed suffixes, invalid base digits and `_` separators have no
integer judgment. -/
inductive IntegerLiteral : List Nat → IntegerToken → Prop
  | intro {radix : Radix} {input digitInput digits suffixInput suffix rest : List Nat}
      {typeSuffix : Option ConcreteSpec.IntTy} :
      input = radix.basePrefix ++ digitInput → DigitRun radix digitInput digits suffixInput →
      DigitsAccepted radix digits → IdentBody suffixInput suffix rest →
      SuffixAccepted suffix typeSuffix →
      IntegerLiteral input ⟨radix.basePrefix ++ digits ++ suffix, positionalValue radix digits,
        typeSuffix, rest⟩

/-- Scalar membership is decidable by its interval bounds. -/
instance (c : Nat) : Decidable (Utf8.Scalar c) := inferInstanceAs (Decidable (_ ∧ _))
/-- Whitespace membership is decidable by the four scalar equalities. -/
instance (c : Nat) : Decidable (Whitespace c) := inferInstanceAs (Decidable (_ ∨ _))
/-- Identifier-start membership is decidable by ASCII bounds. -/
instance (c : Nat) : Decidable (IdentStart c) := inferInstanceAs (Decidable (_ ∨ _))
/-- Decision: owner-approved language list item 30 (OPEN-1), SYN-5@3 / SYN-6@3.
 Identifier-continuation membership is decidable by ASCII bounds. -/
instance (c : Nat) : Decidable (IdentContinue c) := inferInstanceAs (Decidable (_ ∨ _))
/-- The string escape relation is decidable by its six scalar equalities. -/
instance (escaped decoded : Nat) : Decidable (StringEscape escaped decoded) :=
  inferInstanceAs (Decidable (_ ∨ _))
/-- Control membership is decidable by its intervals. -/
instance (c : Nat) : Decidable (Control c) := inferInstanceAs (Decidable (_ ∨ _))
/-- Bidi-control membership is decidable by its equalities and intervals. -/
instance (c : Nat) : Decidable (BidiControl c) := inferInstanceAs (Decidable (_ ∨ _))
/-- Forbidden-raw membership is decidable by its constituent predicates. -/
instance (c : Nat) : Decidable (ForbiddenRaw c) := inferInstanceAs (Decidable (_ ∨ _))
/-- Raw-comment membership is the TAB exception or a decidable refusal. -/
instance (c : Nat) : Decidable (CommentRaw c) := inferInstanceAs (Decidable (_ ∨ _))
/-- Raw-string membership is decidable by delimiter equalities and forbidden-scalar predicates. -/
instance (c : Nat) : Decidable (StringRaw c) := inferInstanceAs (Decidable (_ ∧ _))
/-- Raw-byte membership is decidable by its ASCII bounds and delimiter equalities. -/
instance (c : Nat) : Decidable (ByteRaw c) := inferInstanceAs (Decidable (_ ∧ _))
/-- The byte escape relation is decidable by its six scalar equalities. -/
instance (escaped decoded : Nat) : Decidable (ByteEscape escaped decoded) :=
  inferInstanceAs (Decidable (_ ∨ _))
/-- Base-digit membership is decidable by its intervals and equalities. -/
instance (radix : Radix) (c : Nat) : Decidable (BaseDigit radix c) := by
  unfold BaseDigit
  split <;> infer_instance
/-- Leading-zero acceptance is decidable by the radix and digit-list shape. -/
instance (radix : Radix) (digits : List Nat) : Decidable (DigitsAccepted radix digits) := by
  unfold DigitsAccepted
  split <;> infer_instance
/-- Reflection in the shared scalar checker. -/
instance (a : UInt8) : Decidable (Utf8.One a) := inferInstanceAs (Decidable (_ = true))
instance (a b : UInt8) : Decidable (Utf8.Two a b) := inferInstanceAs (Decidable (_ = true))
instance (a b c : UInt8) : Decidable (Utf8.Three a b c) := inferInstanceAs (Decidable (_ = true))
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
  .intro (.cons (by decide) (by decide) (by decide)
    (.cons (by decide) (by decide) (by decide) (.lf _)))
example : LineComment [47, 47, 32,120,13] [13] :=
  .intro (.cons (by decide) (by decide) (by decide)
    (.cons (by decide) (by decide) (by decide) (.cr _)))
example : LineComment [47, 47, 32,120] [] :=
  .intro (.cons (by decide) (by decide) (by decide)
    (.cons (by decide) (by decide) (by decide) .eof))
example (out : List Nat) : ¬ LineComment [47, 42, 32,120,32,42,47] out := by
  intro h; cases h

end ConcreteSpec.Surface0
