import ConcreteSpec.Spec.Surface0

/-! Proofs of the source rules defined in ConcreteSpec.Spec.Surface0.
Evidence disposition: docs/decisions.md#decided-2026-11-08-source-evidence. -/
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
    | cons hn _ _ _ => exact False.elim (hn rfl)
  | cr rest => cases hs with
    | cr => rfl
    | cons _ hn _ _ => exact False.elim (hn rfl)
  | cons hn hm _ _ ih => cases hs with
    | lf => exact False.elim (hn rfl)
    | cr => exact False.elim (hm rfl)
    | cons _ _ _ ht => exact ih ht

/-- A line comment determines one suffix. -/
theorem lineComment_unique {cs r s : List Nat}
    (hr : LineComment cs r) (hs : LineComment cs s) : r = s := by
  cases hr with
  | intro hb => cases hs with
    | intro hc => exact commentBody_unique hb hc

/-- Erasing scalar values uses the authoritative text UTF-8 checker. -/
theorem encodes_isScalar {c : Nat} {bs : List UInt8} (h : Utf8.Encodes c bs) :
    Borrow.isScalar (bs.map UInt8.toNat) = true := by
  unfold Utf8.Encodes at h
  split at h <;> try contradiction
  all_goals exact h.1

/-- Erasing decoded values yields the authoritative text UTF-8 judgment. -/
theorem decodes_utf8 {bs : List UInt8} {cs : List Nat} (h : Utf8.Decodes bs cs) :
    Borrow.Utf8 (bs.map UInt8.toNat) := by
  induction h with
  | nil => exact .nil
  | cons enc _ ih =>
    rw [List.map_append]
    exact .cons (encodes_isScalar enc) ih

/-- The empty scalar prefix is extendible. -/
theorem scalarPrefix_nil : ScalarPrefix [] := ⟨[0], 0, by decide⟩

/-- One-byte prefixes are exactly ASCII or permitted multibyte leads. -/
theorem scalarPrefix_one (a : UInt8) : ScalarPrefix [a] ↔
    a.toNat ≤ 0x7F ∨ (0xC2 ≤ a.toNat ∧ a.toNat ≤ 0xF4) := by
  constructor
  · rintro ⟨suffix, value, h⟩
    match suffix with
    | [] | [_] | [_,_] | [_,_,_] =>
      simp only [List.cons_append, List.nil_append, Utf8.Encodes,
        Utf8.one_iff, Utf8.two_iff, Utf8.three_iff, Utf8.four_iff] at h
      omega
    | _ :: _ :: _ :: _ :: _ => exact False.elim h
  · intro h
    by_cases h1 : a.toNat ≤ 0x7F
    · exact ⟨[], a.toNat, (Utf8.one_iff a).mpr h1, rfl⟩
    · have ha : 0xC2 ≤ a.toNat ∧ a.toNat ≤ 0xF4 := by omega
      by_cases h2 : a.toNat ≤ 0xDF
      · refine ⟨[0x80], (a.toNat - 0xC0) * 64, ?_⟩
        change Utf8.Two a 0x80 ∧ _
        exact ⟨(Utf8.two_iff _ _).mpr (by have : (0x80 : UInt8).toNat = 128 := rfl; omega), by simp⟩
      · by_cases h3 : a.toNat ≤ 0xEF
        · by_cases he : a.toNat = 0xE0
          · refine ⟨[0xA0,0x80], (a.toNat - 0xE0) * 4096 + 32 * 64, ?_⟩
            change Utf8.Three a 0xA0 0x80 ∧ _
            constructor
            · rw [Utf8.three_iff]; simp; omega
            · simp
          · refine ⟨[0x80,0x80], (a.toNat - 0xE0) * 4096, ?_⟩
            change Utf8.Three a 0x80 0x80 ∧ _
            constructor
            · rw [Utf8.three_iff]; simp; omega
            · simp
        · by_cases he : a.toNat = 0xF0
          · refine ⟨[0x90,0x80,0x80], (a.toNat - 0xF0) * 262144 + 16 * 4096, ?_⟩
            change Utf8.Four a 0x90 0x80 0x80 ∧ _
            constructor
            · rw [Utf8.four_iff]; simp; omega
            · simp
          · refine ⟨[0x80,0x80,0x80], (a.toNat - 0xF0) * 262144, ?_⟩
            change Utf8.Four a 0x80 0x80 0x80 ∧ _
            constructor
            · rw [Utf8.four_iff]; simp; omega
            · simp

/-- Two bytes extend to a scalar precisely when the shared checker accepts
one of the two-, three- or four-byte completions. -/
theorem scalarPrefix_two (a b : UInt8) : ScalarPrefix [a,b] ↔
    Utf8.Two a b ∨ Utf8.Three a b 0x80 ∨ Utf8.Four a b 0x80 0x80 := by
  constructor
  · rintro ⟨suffix, value, h⟩
    match suffix with
    | [] | [_] | [_,_] =>
      simp only [List.cons_append, List.nil_append, Utf8.Encodes,
        Utf8.two_iff, Utf8.three_iff, Utf8.four_iff] at h
      simp only [Utf8.two_iff, Utf8.three_iff, Utf8.four_iff]
      have : (0x80 : UInt8).toNat = 128 := rfl
      omega
    | _ :: _ :: _ :: _ => exact False.elim h
  · intro h
    rcases h with h | h | h
    · exact ⟨[], _, h, rfl⟩
    · exact ⟨[0x80], _, h, rfl⟩
    · exact ⟨[0x80,0x80], _, h, rfl⟩

/-- Three bytes can be a complete three-byte scalar or the prefix of a four-byte one. -/
theorem scalarPrefix_three (a b c : UInt8) : ScalarPrefix [a,b,c] ↔
    Utf8.Three a b c ∨ Utf8.Four a b c 0x80 := by
  constructor
  · rintro ⟨suffix, value, h⟩
    match suffix with
    | [] | [_] =>
      simp only [List.cons_append, List.nil_append, Utf8.Encodes,
        Utf8.three_iff, Utf8.four_iff] at h
      simp only [Utf8.three_iff, Utf8.four_iff]
      have : (0x80 : UInt8).toNat = 128 := rfl
      omega
    | _ :: _ :: _ => exact False.elim h
  · intro h
    rcases h with h | h
    · exact ⟨[], _, h, rfl⟩
    · exact ⟨[0x80], _, h, rfl⟩

/-- Four-byte prefixes must already be a complete four-byte scalar. -/
theorem scalarPrefix_four (a b c d : UInt8) : ScalarPrefix [a,b,c,d] ↔ Utf8.Four a b c d := by
  constructor
  · rintro ⟨suffix, value, h⟩
    cases suffix with
    | nil => exact h.1
    | cons => exact False.elim h
  · intro h; exact ⟨[], _, h, rfl⟩

/-- Every shorter prefix of an extendible scalar is extendible. -/
theorem scalarPrefix_take {bytes : List UInt8} (h : ScalarPrefix bytes) (n : Nat) :
    ScalarPrefix (bytes.take n) := by
  obtain ⟨suffix, value, enc⟩ := h
  refine ⟨bytes.drop n ++ suffix, value, ?_⟩
  simpa only [← List.append_assoc, List.take_append_drop] using enc

/-- A viable prefix followed by an impossible next byte determines the local first failure. -/
theorem scalarFailure_byte {bytes : List UInt8} {k : Nat}
    (hk : k < bytes.length) (good : ScalarPrefix (bytes.take k))
    (unfinished : ¬ ∃ value, Utf8.Encodes value (bytes.take k))
    (bad : ¬ ScalarPrefix (bytes.take (k + 1))) : ScalarFailure bytes k := by
  refine ⟨?_, unfinished, Or.inl ⟨hk, bad⟩⟩
  intro j hj
  have ht := scalarPrefix_take good j
  simpa only [List.take_take, Nat.min_eq_left hj] using ht

/-- EOF is the failure position of an extendible but unfinished scalar. -/
theorem scalarFailure_eof {bytes : List UInt8} (good : ScalarPrefix bytes)
    (bad : ¬ ∃ value, Utf8.Encodes value bytes) : ScalarFailure bytes bytes.length :=
  ⟨fun j _ => scalarPrefix_take good j, by simpa only [List.take_length] using bad,
    Or.inr ⟨rfl, bad⟩⟩

/-- Empty input is an unfinished scalar when a scalar step was requested. -/
theorem scalarFailure_nil : ScalarFailure [] 0 :=
  scalarFailure_eof scalarPrefix_nil (by rintro ⟨_, h⟩; exact h)

/-- Local failure starts a first error after an empty completed prefix. -/
theorem firstError_here {bytes : List UInt8} {k : Nat} (h : ScalarFailure bytes k) (hn : bytes ≠ []) :
    FirstError bytes k := ⟨[], bytes, [], k, rfl, .nil, h, hn, by simp⟩

/-- Prepending a complete encoded scalar shifts a first error by exactly its byte length. -/
theorem firstError_cons {c : Nat} {chunk bytes : List UInt8} {k : Nat}
    (enc : Utf8.Encodes c chunk) (h : FirstError bytes k) :
    FirstError (chunk ++ bytes) (chunk.length + k) := by
  obtain ⟨pre, rest, cs, localOffset, he, hd, hf, hn, hk⟩ := h
  refine ⟨chunk ++ pre, rest, c :: cs, localOffset, ?_, .cons enc hd, hf, hn, ?_⟩
  · simp only [List.append_assoc, he]
  · simp only [List.length_append, hk, Nat.add_assoc]

/-- A proper scalar prefix cannot begin with a complete scalar (UTF-8 is prefix-free). -/
theorem scalarPrefix_unfinished_no_head {bytes chunk rest : List UInt8} {c : Nat}
    (good : ScalarPrefix bytes) (unfinished : ¬ ∃ value, Utf8.Encodes value bytes)
    (enc : Utf8.Encodes c chunk) (he : bytes = chunk ++ rest) : False := by
  obtain ⟨suffix, value, whole⟩ := good
  have eqn : chunk ++ (rest ++ suffix) = (bytes ++ suffix) ++ [] := by
    simp only [he, List.append_assoc, List.append_nil]
  have ht := (encodes_step_unique enc whole eqn).2
  obtain ⟨hr, hs⟩ := List.append_eq_nil_iff.mp ht
  subst rest
  subst suffix
  simp only [List.append_nil] at he whole
  exact unfinished ⟨value, whole⟩

/-- A local first scalar failure cannot begin with any complete encoded scalar. -/
theorem scalarFailure_no_head {chunk rest : List UInt8} {c k : Nat}
    (enc : Utf8.Encodes c chunk) (h : ScalarFailure (chunk ++ rest) k) : False := by
  obtain ⟨good, unfinished, bad⟩ := h
  by_cases hk : chunk.length ≤ k
  · apply scalarPrefix_unfinished_no_head (rest := rest.take (k - chunk.length))
      (good k (by omega)) unfinished enc
    simp only [List.take_append, List.take_of_length_le hk]
  · have hsmall : k + 1 ≤ chunk.length := by omega
    have hp := scalarPrefix_take (show ScalarPrefix chunk from ⟨[], c, by simpa using enc⟩) (k + 1)
    rcases bad with ⟨_, hb⟩ | ⟨he, _⟩
    · apply hb
      simpa only [List.take_append, Nat.sub_eq_zero_of_le hsmall, List.take_zero, List.append_nil] using hp
    · simp only [List.length_append] at he
      omega

/-- A local first-failure offset is unique. -/
theorem scalarFailure_unique {bytes : List UInt8} {k l : Nat}
    (hk : ScalarFailure bytes k) (hl : ScalarFailure bytes l) : k = l := by
  have kb : k ≤ bytes.length := by rcases hk.2.2 with h | h <;> omega
  have lb : l ≤ bytes.length := by rcases hl.2.2 with h | h <;> omega
  by_cases he : k = l
  · exact he
  apply False.elim
  rcases Nat.lt_or_gt_of_ne he with h | h
  · rcases hk.2.2 with ⟨_, bad⟩ | ⟨heof, _⟩
    · exact bad (hl.1 (k + 1) (by omega))
    · omega
  · rcases hl.2.2 with ⟨_, bad⟩ | ⟨heof, _⟩
    · exact bad (hk.1 (l + 1) (by omega))
    · omega

/-- A completed scalar can be removed from the front of a first-error judgment. -/
theorem firstError_cons_inv {c : Nat} {chunk bytes : List UInt8} {offset : Nat}
    (enc : Utf8.Encodes c chunk) (h : FirstError (chunk ++ bytes) offset) :
    ∃ k, FirstError bytes k ∧ offset = chunk.length + k := by
  obtain ⟨pre, rest, cs, localOffset, he, hd, hf, hn, hk⟩ := h
  cases hd with
  | nil =>
    simp only [List.nil_append] at he
    exact False.elim (scalarFailure_no_head enc (he ▸ hf))
  | @cons d other pre cs hd ht =>
    have he' : chunk ++ bytes = other ++ (pre ++ rest) := by
      simpa only [List.append_assoc] using he
    obtain ⟨_, tailEq⟩ := encodes_step_unique enc hd he'
    have chunkEq : chunk = other := List.append_cancel_right (by simpa only [tailEq] using he')
    refine ⟨pre.length + localOffset, ⟨pre, rest, cs, localOffset, tailEq, ht, hf, hn, rfl⟩, ?_⟩
    simp only [List.length_append, ← chunkEq, Nat.add_assoc] at hk
    exact hk

/-- The first declarative error offset is unique, across scalar boundaries as well. -/
theorem firstError_unique {bytes : List UInt8} {k l : Nat}
    (hk : FirstError bytes k) (hl : FirstError bytes l) : k = l := by
  obtain ⟨pre, rest, cs, localOffset, he, hd, hf, hn, hoff⟩ := hk
  subst bytes
  subst k
  induction hd generalizing l with
  | nil =>
    simp only [List.nil_append, List.length_nil, Nat.zero_add] at *
    obtain ⟨other, suffix, ds, j, he, ht, hg, _, ho⟩ := hl
    cases ht with
    | nil =>
      simp only [List.nil_append, List.length_nil, Nat.zero_add] at he ho
      subst suffix
      exact ho ▸ scalarFailure_unique hf hg
    | cons enc tail =>
      have he' : _ := he
      rw [List.append_assoc] at he'
      exact False.elim (scalarFailure_no_head enc (he' ▸ hf))
  | @cons c chunk pre cs enc tail ih =>
    have hl' : FirstError (chunk ++ (pre ++ rest)) l := by
      simpa only [List.append_assoc] using hl
    obtain ⟨j, hj, heq⟩ := firstError_cons_inv enc hl'
    have hh := ih hj
    simp only [List.length_append, heq, Nat.add_assoc, hh]

/-- A scalar encoding consumes at least one byte. -/
theorem encodes_nonempty {c : Nat} {chunk : List UInt8} (h : Utf8.Encodes c chunk) :
    chunk ≠ [] := by
  intro he
  subst chunk
  exact h

/-- A declarative first error rules out every full decoding. -/
theorem firstError_not_decodes {bytes : List UInt8} {k : Nat} (h : FirstError bytes k) :
    ∀ cs, ¬ Utf8.Decodes bytes cs := by
  intro cs hd
  induction hd generalizing k with
  | nil =>
    obtain ⟨pre, rest, _, _, he, _, _, hn, _⟩ := h
    exact hn (List.append_eq_nil_iff.mp he.symm).2
  | cons enc _ ih =>
    obtain ⟨_, ht, _⟩ := firstError_cons_inv enc h
    exact ih ht

/-- Scalar-value decoding adds a value equation to the one authoritative UTF-8 checker. -/
theorem encodes_exists_iff (bs : List UInt8) :
    (∃ c, Utf8.Encodes c bs) ↔ Borrow.isScalar (bs.map UInt8.toNat) = true := by
  constructor
  · rintro ⟨_, h⟩; exact encodes_isScalar h
  · intro h
    match bs with
    | [] => exact False.elim (by cases h)
    | [a] => exact ⟨_, h, rfl⟩
    | [a,b] => exact ⟨_, h, rfl⟩
    | [a,b,c] => exact ⟨_, h, rfl⟩
    | [a,b,c,d] => exact ⟨_, h, rfl⟩
    | _ :: _ :: _ :: _ :: _ :: _ => exact False.elim (by cases h)

/-- Value decoding exists exactly for authoritative well-formed UTF-8 text. -/
theorem decodes_exists_iff_utf8 (bs : List UInt8) :
    (∃ cs, Utf8.Decodes bs cs) ↔ Borrow.Utf8 (bs.map UInt8.toNat) := by
  constructor
  · rintro ⟨_, h⟩; exact decodes_utf8 h
  · intro h
    generalize he : bs.map UInt8.toNat = bytes at h
    induction h generalizing bs with
    | nil =>
      have hn : bs = [] := List.map_eq_nil_iff.mp he
      subst bs
      exact ⟨[], .nil⟩
    | @cons chunk rest hc ht ih =>
      obtain ⟨chunk', rest', heq, hchunk, hrest⟩ := List.map_eq_append_iff.mp he
      obtain ⟨c, enc⟩ := (encodes_exists_iff chunk').mpr (by simpa only [hchunk] using hc)
      obtain ⟨cs, dec⟩ := ih rest' hrest
      subst bs
      exact ⟨c :: cs, .cons enc dec⟩

/-- Removing an allowed scalar preserves the first source failure and shifts its offset. -/
theorem sourceFailure_cons_inv {c : Nat} {chunk bytes : List UInt8} {offset : Nat}
    (enc : Utf8.Encodes c chunk) (h0 : c ≠ 0) (hB : c ≠ 0xFEFF)
    (h : SourceFailure (chunk ++ bytes) offset) :
    ∃ k, SourceFailure bytes k ∧ offset = chunk.length + k := by
  generalize he : chunk ++ bytes = input at h
  cases h with
  | utf8 hn hf => exact False.elim (scalarFailure_no_head enc (he ▸ hf))
  | nul other => exact False.elim (h0 (encodes_step_unique enc other he).1)
  | bom other => exact False.elim (hB (encodes_step_unique enc other he).1)
  | @cons d other rest k hd _ _ hf =>
    obtain ⟨_, ht⟩ := encodes_step_unique enc hd he
    have hc : chunk = other := List.append_cancel_right (by simpa only [ht] using he)
    exact ⟨k, ht ▸ hf, by rw [hc]⟩

/-- A forbidden first scalar determines offset zero for every source-failure derivation. -/
theorem sourceFailure_forbidden_offset {c : Nat} {chunk bytes : List UInt8} {k : Nat}
    (enc : Utf8.Encodes c chunk) (bad : c = 0 ∨ c = 0xFEFF)
    (h : SourceFailure (chunk ++ bytes) k) : k = 0 := by
  generalize he : chunk ++ bytes = input at h
  cases h with
  | utf8 hn hf => exact False.elim (scalarFailure_no_head enc (he ▸ hf))
  | nul => rfl
  | bom => rfl
  | cons hd h0 hB hf =>
    have hc := (encodes_step_unique enc hd he).1
    rcases bad with hz | hz
    · exact False.elim (h0 (hc ▸ hz))
    · exact False.elim (hB (hc ▸ hz))

/-- The first source-failure offset is unique, including UTF-8/NUL/BOM alternatives. -/
theorem sourceFailure_unique {bytes : List UInt8} {k l : Nat}
    (hk : SourceFailure bytes k) (hl : SourceFailure bytes l) : k = l := by
  induction hk generalizing l with
  | utf8 hn hf =>
    cases hl with
    | utf8 _ hg => exact scalarFailure_unique hf hg
    | nul enc => exact False.elim (scalarFailure_no_head enc hf)
    | bom enc => exact False.elim (scalarFailure_no_head enc hf)
    | cons enc _ _ _ => exact False.elim (scalarFailure_no_head enc hf)
  | nul enc => exact (sourceFailure_forbidden_offset enc (Or.inl rfl) hl).symm
  | bom enc => exact (sourceFailure_forbidden_offset enc (Or.inr rfl) hl).symm
  | cons enc h0 hB hf ih =>
    obtain ⟨j, hj, he⟩ := sourceFailure_cons_inv enc h0 hB hl
    have hh := ih hj
    omega

/-- A declarative decoding with an encoded prefix starts with its scalar and decodes its suffix. -/
theorem decodes_head_inv {c : Nat} {chunk bytes : List UInt8} {cs : List Nat}
    (enc : Utf8.Encodes c chunk) (h : Utf8.Decodes (chunk ++ bytes) cs) :
    ∃ ds, cs = c :: ds ∧ Utf8.Decodes bytes ds := by
  generalize he : chunk ++ bytes = input at h
  cases h with
  | nil => exact False.elim (encodes_nonempty enc (List.append_eq_nil_iff.mp he).1)
  | cons hd ht =>
    obtain ⟨hc, hb⟩ := encodes_step_unique enc hd he
    exact ⟨_, by rw [hc], hb ▸ ht⟩

/-- No first source failure admits a source-valid declarative decoding. -/
theorem sourceFailure_not_sourceOk {bytes : List UInt8} {k : Nat}
    (h : SourceFailure bytes k) : ¬ SourceOk bytes := by
  induction h with
  | @utf8 bytes k hn hf =>
    rintro ⟨cs, hd, _⟩
    generalize he : bytes = input at hd
    cases hd with
    | nil => exact hn he
    | cons enc tail =>
      exact scalarFailure_no_head enc (he ▸ hf)
  | nul enc =>
    rintro ⟨cs, hd, good⟩
    obtain ⟨ds, rfl, _⟩ := decodes_head_inv enc hd
    exact (good 0 (by simp)).1 rfl
  | bom enc =>
    rintro ⟨cs, hd, good⟩
    obtain ⟨ds, rfl, _⟩ := decodes_head_inv enc hd
    exact (good 0xFEFF (by simp)).2 rfl
  | cons enc h0 hB hf ih =>
    rintro ⟨cs, hd, good⟩
    obtain ⟨ds, rfl, ht⟩ := decodes_head_inv enc hd
    exact ih ⟨ds, ht, fun c hc => good c (by simp [hc])⟩

end ConcreteSpec.Surface0
