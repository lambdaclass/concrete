namespace Concrete.Backend.LLVM

/-! The decimal integer token subset used at this boundary follows LLVM's
`LLLexer::LexDigitOrNegative`. The emitter uses canonical magnitudes (a single
zero, or an ASCII nonzero digit followed by ASCII digits). -/

def DecimalMagnitude (digits : List Char) : Prop :=
  digits ≠ [] ∧ (∀ c ∈ digits, c.isDigit = true) ∧
    (digits.head? ≠ some '0' ∨ digits = ['0'])

/-- The grammar and value of a complete decimal integer token. -/
def DecimalToken (token : String) (value : Int) : Prop :=
  (DecimalMagnitude token.toList ∧ value = Int.ofNat (Nat.ofDigitChars 10 token.toList 0)) ∨
  ∃ digits, token.toList = '-' :: digits ∧ DecimalMagnitude digits ∧ digits ≠ ['0'] ∧
    value = -Int.ofNat (Nat.ofDigitChars 10 digits 0)

private def parseMagnitude (digits : List Char) : Option Nat :=
  if !digits.isEmpty && digits.all Char.isDigit &&
      (digits.head? != some '0' || digits == ['0']) then
    some (Nat.ofDigitChars 10 digits 0)
  else none

def parseDecimal (token : String) : Option Int :=
  match token.toList with
  | '-' :: digits =>
    if digits = ['0'] then none else (parseMagnitude digits).map fun n => -Int.ofNat n
  | digits => (parseMagnitude digits).map Int.ofNat

private theorem parseMagnitude_exact (digits : List Char) (n : Nat) :
    parseMagnitude digits = some n ↔
      DecimalMagnitude digits ∧ n = Nat.ofDigitChars 10 digits 0 := by
  simp [parseMagnitude, DecimalMagnitude, Bool.and_eq_true, Bool.or_eq_true,
    List.all_eq_true, eq_comm (a := n), and_assoc]

/-- The parser accepts exactly the specified tokens and returns their specified values. -/
theorem parseDecimal_exact (token : String) (value : Int) :
    parseDecimal token = some value ↔ DecimalToken token value := by
  have mapped (digits : List Char) (signed : Nat → Int) :
      (parseMagnitude digits).map signed = some value ↔
        DecimalMagnitude digits ∧ value = signed (Nat.ofDigitChars 10 digits 0) := by
    simp only [Option.map_eq_some_iff, parseMagnitude_exact]
    constructor
    · rintro ⟨_, ⟨hm, rfl⟩, hv⟩
      exact ⟨hm, hv.symm⟩
    · rintro ⟨hm, rfl⟩
      exact ⟨_, ⟨hm, rfl⟩, rfl⟩
  have neg : ∀ digits, ¬ DecimalMagnitude ('-' :: digits) := by
    intro digits h
    have bad := h.2.1 '-' (by simp)
    contradiction
  cases h : token.toList with
  | nil => simp [parseDecimal, DecimalToken, h, parseMagnitude, DecimalMagnitude]
  | cons c digits =>
    by_cases hc : c = '-'
    · subst c
      by_cases hz : digits = ['0'] <;> simp [parseDecimal, DecimalToken, h, neg, mapped, hz]
    · simp [parseDecimal, DecimalToken, h, hc, mapped]

private theorem positive_digits_head (n : Nat) (h : 0 < n) :
    (Nat.toDigits 10 n).head? ≠ some '0' := by
  induction n using Nat.strongRecOn with
  | ind n ih =>
    by_cases small : n < 10
    · rw [Nat.toDigits_of_lt_base small]
      change some n.digitChar ≠ some '0'
      simpa using Nat.ne_of_gt h
    · have large : 10 ≤ n := by omega
      rw [Nat.toDigits_of_base_le (by decide) large]
      cases hd : Nat.toDigits 10 (n / 10) with
      | nil => exact (Nat.toDigits_ne_nil hd).elim
      | cons c cs =>
        simpa [hd] using ih (n / 10) (by omega) (by omega)

private theorem repr_magnitude (n : Nat) : DecimalMagnitude n.repr.toList := by
  rw [Nat.toList_repr]
  refine ⟨Nat.toDigits_ne_nil, fun c hc => Nat.isDigit_of_mem_toDigits (by decide) (by decide) hc, ?_⟩
  cases n with
  | zero => exact Or.inr (Nat.toDigits_zero 10)
  | succ n => exact Or.inl (positive_digits_head (n + 1) (by omega))

/-- Render an unsigned LLVM integer token in canonical decimal. -/
def decimal (n : Nat) : String := n.repr

/-- The decimal formatter emits a complete token denoting its input. -/
theorem decimal_token (n : Nat) : DecimalToken (decimal n) (Int.ofNat n) :=
  Or.inl ⟨repr_magnitude n, by
    rw [decimal, Nat.toList_repr, Nat.ofDigitChars_ten_toDigits]⟩

end Concrete.Backend.LLVM
