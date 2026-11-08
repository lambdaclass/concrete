import ConcreteSpec.Spec.Surface0

/-! Proofs of the source rules, owner-approved item 30 (OPEN-1). -/
namespace ConcreteSpec.Surface0
namespace Utf8

theorem one_iff (a : UInt8) : One a ↔ a.toNat ≤ 0x7F := by
  simp [One, Borrow.isScalar]; omega

theorem two_iff (a b : UInt8) : Two a b ↔
    0xC2 ≤ a.toNat ∧ a.toNat ≤ 0xDF ∧ 0x80 ≤ b.toNat ∧ b.toNat ≤ 0xBF := by
  simp [Two, Borrow.isScalar, Borrow.cont, Bool.and_eq_true, and_assoc]

theorem three_iff (a b c : UInt8) : Three a b c ↔
    0xE0 ≤ a.toNat ∧ a.toNat ≤ 0xEF ∧
    0x80 ≤ b.toNat ∧ b.toNat ≤ 0xBF ∧ 0x80 ≤ c.toNat ∧ c.toNat ≤ 0xBF ∧
    (a.toNat = 0xE0 → 0xA0 ≤ b.toNat) ∧ (a.toNat = 0xED → b.toNat ≤ 0x9F) := by
  simp [Three, Borrow.isScalar, Borrow.cont, Bool.and_eq_true, Bool.or_eq_true]
  omega

theorem four_iff (a b c d : UInt8) : Four a b c d ↔
    0xF0 ≤ a.toNat ∧ a.toNat ≤ 0xF4 ∧
    0x80 ≤ b.toNat ∧ b.toNat ≤ 0xBF ∧ 0x80 ≤ c.toNat ∧ c.toNat ≤ 0xBF ∧
    0x80 ≤ d.toNat ∧ d.toNat ≤ 0xBF ∧
    (a.toNat = 0xF0 → 0x90 ≤ b.toNat) ∧ (a.toNat = 0xF4 → b.toNat ≤ 0x8F) := by
  simp [Four, Borrow.isScalar, Borrow.cont, Bool.and_eq_true, Bool.or_eq_true]
  omega

end Utf8
/-- Every encoded value is a Unicode scalar. -/
theorem encodes_scalar {c : Nat} {bs : List UInt8} (h : Utf8.Encodes c bs) :
    Utf8.Scalar c := by
  unfold Utf8.Encodes at h
  split at h <;> simp only [Utf8.one_iff, Utf8.two_iff, Utf8.three_iff, Utf8.four_iff] at h <;>
    unfold Utf8.Scalar <;> omega

/-- UTF-8 is prefix-free: one scalar step has a unique value and remainder. -/
theorem encodes_step_unique {s t : Nat} {chunk other bs ds : List UInt8}
    (hc : Utf8.Encodes s chunk) (hd : Utf8.Encodes t other)
    (he : chunk ++ bs = other ++ ds) : s = t ∧ bs = ds := by
  unfold Utf8.Encodes at hc hd
  split at hc <;> split at hd <;>
    simp only [Utf8.one_iff, Utf8.two_iff, Utf8.three_iff, Utf8.four_iff] at hc hd <;>
    try contradiction
  all_goals simp only [List.cons_append, List.nil_append, List.cons.injEq] at he
  all_goals rcases he with ⟨rfl, he⟩
  all_goals try omega
  all_goals repeat rcases he with ⟨rfl, he⟩
  all_goals exact ⟨by omega, rfl⟩

/-- A fixed byte chunk has a unique scalar value. -/
theorem encodes_determ {s t : Nat} {chunk : List UInt8}
    (hc : Utf8.Encodes s chunk) (hd : Utf8.Encodes t chunk) : s = t :=
  (encodes_step_unique (bs := []) (ds := []) hc hd rfl).1

/-- The declarative decoding judgment is deterministic. -/
theorem decodes_unique {bs : List UInt8} {cs ds : List Nat}
    (hc : Utf8.Decodes bs cs) (hd : Utf8.Decodes bs ds) : cs = ds := by
  have aux : ∀ bytes xs ys, Utf8.Decodes bytes xs → Utf8.Decodes bytes ys → xs = ys := by
    intro bytes xs ys hx
    induction hx generalizing ys with
    | nil =>
      intro hy
      generalize he : ([] : List UInt8) = bytes at hy
      cases hy with
      | nil => rfl
      | cons enc tail =>
        have hz := List.append_eq_nil_iff.mp he.symm
        obtain ⟨hz, _⟩ := hz
        subst_vars
        exact False.elim enc
    | @cons s chunk bs cs enc tail ih =>
      intro hy
      generalize he : chunk ++ bs = bytes at hy
      cases hy with
      | nil =>
        have hz := (List.append_eq_nil_iff.mp he).1
        subst chunk; simp [Utf8.Encodes] at enc
      | cons enc' tail' =>
        obtain ⟨rfl, rfl⟩ := encodes_step_unique enc enc' he
        exact congrArg (s :: ·) (ih _ tail')
  exact aux bs cs ds hc hd

/-- A comment body determines one suffix. -/
theorem commentBody_unique {cs r s : List Nat}
    (hr : CommentBody cs r) (hs : CommentBody cs s) : r = s := by
  induction hr with
  | eof => cases hs; rfl
  | lf rest => cases hs with
    | lf => rfl
    | cons hn _ _ => exact False.elim (hn rfl)
  | cr rest => cases hs with
    | cr => rfl
    | cons _ hn _ => exact False.elim (hn rfl)
  | cons hn hm _ ih => cases hs with
    | lf => exact False.elim (hn rfl)
    | cr => exact False.elim (hm rfl)
    | cons _ _ ht => exact ih ht

/-- A line comment determines one suffix. -/
theorem lineComment_unique {cs r s : List Nat}
    (hr : LineComment cs r) (hs : LineComment cs s) : r = s := by
  cases hr with
  | intro hb => cases hs with
    | intro hc => exact commentBody_unique hb hc

end ConcreteSpec.Surface0
