import ConcreteSpec.Proofs.Surface0

/-!
# Settled lexer components

Total byte-list UTF-8 decoding and source checking, ASCII identifier tests and line comments.
No tokens, literal rules, whitespace rules, shebang handling or block comments are introduced.
Error constructors are internal data, not diagnostic wording. Offsets are zero-based byte offsets;
truncation points at EOF. Error offsets satisfy the declarative first-failure judgments
in Surface0; source NUL/BOM offsets point to the start of the refused scalar.
-/


namespace Concrete.Lex
open ConcreteSpec.Surface0
open ConcreteSpec
export ConcreteSpec.Surface0 (encodes_scalar decodes_unique encodes_nonempty)

/-- Internal refusal categories; no user-visible diagnostic spelling is fixed. -/
inductive DecodeReason where
  | truncated | invalidContinuation | overlong | surrogate | tooLarge | unexpectedContinuation | nul | bom
  deriving DecidableEq, Repr

/-- A refusal and its zero-based byte offset (EOF for missing bytes). -/
structure DecodeError where
  offset : Nat
  reason : DecodeReason
  deriving DecidableEq, Repr

/-- Classify a refused lead. Continuation bytes cannot start a scalar. -/
def leadReason (a : UInt8) : DecodeReason :=
  if a.toNat < 0xC0 then .unexpectedContinuation
  else if a.toNat < 0xC2 then .overlong else .tooLarge

/-- Classify a refused second byte after a permitted lead. -/
def secondReason (a b : UInt8) : DecodeReason :=
  if !Borrow.cont b.toNat then .invalidContinuation
  else if (a.toNat = 0xE0 ∧ b.toNat < 0xA0) ∨
      (a.toNat = 0xF0 ∧ b.toNat < 0x90) then .overlong
  else if a.toNat = 0xED then .surrogate else .tooLarge

/-- One consumed scalar, with its independent encoding and input decomposition.
Proof fields are erased; the executable result stores only the chunk, value and suffix. -/
structure ScalarStep (input : List UInt8) where
  chunk : List UInt8
  value : Nat
  rest : List UInt8
  encodes : Utf8.Encodes value chunk
  split : input = chunk ++ rest

/-- A successful step strictly reduces the unconsumed bytes. -/
theorem ScalarStep.shorter {input : List UInt8} (s : ScalarStep input) :
    s.rest.length < input.length := by
  have hn := encodes_nonempty s.encodes
  have hl := congrArg List.length s.split
  simp only [List.length_append] at hl
  have : 0 < s.chunk.length := List.length_pos_iff.mpr hn
  omega

/-- Whether a second byte can follow this three- or four-byte lead. -/
def secondOk (a b : UInt8) : Bool :=
  Borrow.cont b.toNat &&
  (!(a.toNat == 0xE0) || 0xA0 ≤ b.toNat) &&
  (!(a.toNat == 0xED) || b.toNat ≤ 0x9F) &&
  (!(a.toNat == 0xF0) || 0x90 ≤ b.toNat) &&
  (!(a.toNat == 0xF4) || b.toNat ≤ 0x8F)

/-- Decode exactly one scalar. Dispatch on the lead before inspecting further bytes;
each continuation is checked before the next byte is inspected. -/
def decodeStep (off : Nat) (input : List UInt8) : Except DecodeError (ScalarStep input) :=
  match input with
  | [] => .error ⟨off, .truncated⟩
  | a :: bs =>
    if ha : Utf8.One a then .ok ⟨[a], a.toNat, bs, ⟨ha, rfl⟩, rfl⟩
    else if a.toNat < 0xC2 ∨ 0xF4 < a.toNat then .error ⟨off, leadReason a⟩
    else match bs with
    | [] => .error ⟨off + 1, .truncated⟩
    | b :: bs =>
      if hb : Utf8.Two a b then
        .ok ⟨[a,b], (a.toNat - 0xC0) * 64 + (b.toNat - 0x80), bs, ⟨hb, rfl⟩, rfl⟩
      else if a.toNat < 0xE0 ∨ !secondOk a b then .error ⟨off + 1, secondReason a b⟩
      else match bs with
      | [] => .error ⟨off + 2, .truncated⟩
      | c :: bs =>
        if hc : Utf8.Three a b c then
          .ok ⟨[a,b,c], (a.toNat - 0xE0) * 4096 + (b.toNat - 0x80) * 64 +
            (c.toNat - 0x80), bs, ⟨hc, rfl⟩, rfl⟩
        else if a.toNat < 0xF0 ∨ !Borrow.cont c.toNat then
          .error ⟨off + 2, .invalidContinuation⟩
        else match bs with
        | [] => .error ⟨off + 3, .truncated⟩
        | d :: bs =>
          if hd : Utf8.Four a b c d then
            .ok ⟨[a,b,c,d], (a.toNat - 0xF0) * 262144 + (b.toNat - 0x80) * 4096 +
              (c.toNat - 0x80) * 64 + (d.toNat - 0x80), bs, ⟨hd, rfl⟩, rfl⟩
          else .error ⟨off + 3, .invalidContinuation⟩

/-- A permitted three-byte row admits its second byte. -/
theorem secondOk_three {a b c : UInt8} (h : Utf8.Three a b c) : secondOk a b = true := by
  simp only [Utf8.three_iff] at h
  simp [secondOk, Borrow.cont, Bool.and_eq_true, Bool.or_eq_true]
  omega

/-- A permitted four-byte row admits its second byte. -/
theorem secondOk_four {a b c d : UInt8} (h : Utf8.Four a b c d) : secondOk a b = true := by
  simp only [Utf8.four_iff] at h
  simp [secondOk, Borrow.cont, Bool.and_eq_true, Bool.or_eq_true]
  omega

/-- A declarative encoding makes the scalar step succeed with that value and suffix. -/
theorem decodeStep_complete {c : Nat} {chunk bs : List UInt8}
    (h : Utf8.Encodes c chunk) (off : Nat) :
    ∃ s, decodeStep off (chunk ++ bs) = .ok s ∧ s.value = c ∧ s.rest = bs := by
  match chunk with
  | [] => exact False.elim h
  | [a] =>
    change Utf8.One a ∧ c = _ at h
    obtain ⟨hr, he⟩ := h
    subst c
    refine ⟨⟨[a], a.toNat, bs, ⟨hr, rfl⟩, rfl⟩, ?_, rfl, rfl⟩
    simp only [List.cons_append, List.nil_append, decodeStep, hr, ↓reduceDIte]
  | [a,b] =>
    change Utf8.Two a b ∧ c = _ at h
    obtain ⟨hr, he⟩ := h
    subst c
    have h1 : ¬ Utf8.One a := by
      have hh := (Utf8.two_iff a b).mp hr
      rw [Utf8.one_iff]; omega
    have hl : ¬ (a.toNat < 0xC2 ∨ 0xF4 < a.toNat) := by
      have hh := (Utf8.two_iff a b).mp hr
      omega
    refine ⟨⟨[a,b], (a.toNat - 0xC0) * 64 + (b.toNat - 0x80), bs, ⟨hr, rfl⟩, rfl⟩, ?_, rfl, rfl⟩
    simp only [List.cons_append, List.nil_append, decodeStep, hr, h1, hl, ↓reduceDIte, ↓reduceIte]
  | [a,b,d] =>
    change Utf8.Three a b d ∧ c = _ at h
    obtain ⟨hr, he⟩ := h
    subst c
    have h1 : ¬ Utf8.One a := by
      have hh := (Utf8.three_iff a b d).mp hr
      rw [Utf8.one_iff]; omega
    have h2 : ¬ Utf8.Two a b := by
      have hh := (Utf8.three_iff a b d).mp hr
      rw [Utf8.two_iff]; omega
    have hl : ¬ (a.toNat < 0xC2 ∨ 0xF4 < a.toNat) := by
      have hh := (Utf8.three_iff a b d).mp hr
      omega
    have hs := secondOk_three hr
    have hm : ¬ (a.toNat < 0xE0 ∨ (!secondOk a b) = true) := by
      have hh := (Utf8.three_iff a b d).mp hr
      simp only [hs, Bool.not_true, Bool.false_eq_true, or_false]
      omega
    refine ⟨⟨[a,b,d], (a.toNat - 0xE0) * 4096 + (b.toNat - 0x80) * 64 + (d.toNat - 0x80), bs, ⟨hr, rfl⟩, rfl⟩, ?_, rfl, rfl⟩
    simp only [List.cons_append, List.nil_append, decodeStep, hr, h1, h2, hl, hm, ↓reduceDIte, ↓reduceIte]
  | [a,b,d,e] =>
    change Utf8.Four a b d e ∧ c = _ at h
    obtain ⟨hr, he⟩ := h
    subst c
    have h1 : ¬ Utf8.One a := by
      have hh := (Utf8.four_iff a b d e).mp hr
      rw [Utf8.one_iff]; omega
    have h2 : ¬ Utf8.Two a b := by
      have hh := (Utf8.four_iff a b d e).mp hr
      rw [Utf8.two_iff]; omega
    have h3 : ¬ Utf8.Three a b d := by
      have hh := (Utf8.four_iff a b d e).mp hr
      rw [Utf8.three_iff]; omega
    have hl : ¬ (a.toNat < 0xC2 ∨ 0xF4 < a.toNat) := by
      have hh := (Utf8.four_iff a b d e).mp hr
      omega
    have hs := secondOk_four hr
    have hm : ¬ (a.toNat < 0xE0 ∨ (!secondOk a b) = true) := by
      have hh := (Utf8.four_iff a b d e).mp hr
      simp only [hs, Bool.not_true, Bool.false_eq_true, or_false]
      omega
    have ht : ¬ (a.toNat < 0xF0 ∨ (!Borrow.cont d.toNat) = true) := by
      have hh := (Utf8.four_iff a b d e).mp hr
      simp [Borrow.cont]
      omega
    refine ⟨⟨[a,b,d,e], (a.toNat - 0xF0) * 262144 + (b.toNat - 0x80) * 4096 + (d.toNat - 0x80) * 64 + (e.toNat - 0x80), bs, ⟨hr, rfl⟩, rfl⟩, ?_, rfl, rfl⟩
    simp only [List.cons_append, List.nil_append, decodeStep, hr, h1, h2, h3, hl, hm, ht, ↓reduceDIte, ↓reduceIte]
  | _ :: _ :: _ :: _ :: _ :: _ => exact False.elim h

/-- An error from a scalar step has exactly the declarative first-failure offset. -/
theorem decodeStep_error_first {off : Nat} {input : List UInt8} {e : DecodeError}
    (h : decodeStep off input = .error e) :
    ∃ k, ScalarFailure input k ∧ e.offset = off + k := by
  unfold decodeStep at h
  split at h
  · cases h
    exact ⟨0, scalarFailure_nil, by simp⟩
  · rename_i a bs
    split at h
    · cases h
    · rename_i ha
      split at h
      · rename_i bad
        cases h
        refine ⟨0, scalarFailure_byte (by simp) scalarPrefix_nil (by rintro ⟨_, hf⟩; exact hf) ?_, by simp⟩
        simp only [List.take_succ_cons, List.take_zero, scalarPrefix_one]
        rw [Utf8.one_iff] at ha
        omega
      · rename_i lead
        split at h
        · cases h
          refine ⟨1, ?_, rfl⟩
          apply scalarFailure_eof
          · rw [scalarPrefix_one, Utf8.one_iff] at *; omega
          · rintro ⟨v, hv⟩; exact ha hv.1
        · rename_i b bs
          split at h
          · cases h
          · rename_i hb
            split at h
            · rename_i bad
              cases h
              refine ⟨1, scalarFailure_byte (by simp) ?_ (by
                simp only [List.take_succ_cons, List.take_zero]
                rintro ⟨_, hf⟩; exact ha hf.1) ?_, rfl⟩
              · simp only [List.take_succ_cons, List.take_zero, scalarPrefix_one]
                rw [Utf8.one_iff] at ha
                omega
              · simp only [List.take_succ_cons, List.take_zero, scalarPrefix_two]
                simp only [Utf8.two_iff, Utf8.three_iff, Utf8.four_iff] at *
                simp [secondOk, Borrow.cont] at bad
                have : (0x80 : UInt8).toNat = 128 := rfl
                omega
            · rename_i second
              split at h
              · cases h
                refine ⟨2, ?_, rfl⟩
                apply scalarFailure_eof
                · rw [scalarPrefix_two]
                  simp only [Utf8.two_iff, Utf8.three_iff, Utf8.four_iff] at *
                  simp [secondOk, Borrow.cont] at second
                  have : (0x80 : UInt8).toNat = 128 := rfl
                  omega
                · rintro ⟨v, hv⟩; exact hb hv.1
              · rename_i c bs
                split at h
                · cases h
                · rename_i hc
                  split at h
                  · rename_i bad
                    cases h
                    refine ⟨2, scalarFailure_byte (by simp) ?_ (by
                simp only [List.take_succ_cons, List.take_zero]
                rintro ⟨_, hf⟩; exact hb hf.1) ?_, rfl⟩
                    · simp only [List.take_succ_cons, List.take_zero, scalarPrefix_two]
                      simp only [Utf8.two_iff, Utf8.three_iff, Utf8.four_iff] at *
                      simp [secondOk, Borrow.cont] at second
                      have : (0x80 : UInt8).toNat = 128 := rfl
                      omega
                    · simp only [List.take_succ_cons, List.take_zero, scalarPrefix_three]
                      simp only [Utf8.three_iff, Utf8.four_iff] at *
                      simp [Borrow.cont] at bad
                      omega
                  · rename_i third
                    split at h
                    · cases h
                      refine ⟨3, ?_, rfl⟩
                      apply scalarFailure_eof
                      · rw [scalarPrefix_three]
                        simp only [Utf8.three_iff, Utf8.four_iff] at *
                        simp [secondOk, Borrow.cont] at second
                        simp [Borrow.cont] at third
                        have : (0x80 : UInt8).toNat = 128 := rfl
                        omega
                      · rintro ⟨v, hv⟩; exact hc hv.1
                    · rename_i d bs
                      split at h
                      · cases h
                      · rename_i hd
                        cases h
                        refine ⟨3, scalarFailure_byte (by simp) ?_ (by
                simp only [List.take_succ_cons, List.take_zero]
                rintro ⟨_, hf⟩; exact hc hf.1) ?_, rfl⟩
                        · simp only [List.take_succ_cons, List.take_zero, scalarPrefix_three]
                          simp only [Utf8.three_iff, Utf8.four_iff] at *
                          simp [secondOk, Borrow.cont] at second
                          simp [Borrow.cont] at third
                          have : (0x80 : UInt8).toNat = 128 := rfl
                          omega
                        · simpa only [List.take_succ_cons, List.take_zero, scalarPrefix_four] using hd

/-- Common decoder/source driver. The accumulator makes recursion tail recursive;
source refusals happen before the recursive call, as do scalar-step errors. -/
def decodeLoop (source : Bool) (off : Nat) (input : List UInt8) (acc : List Nat) :
    Except DecodeError (List Nat) :=
  match _hinput : input with
  | [] => .ok acc.reverse
  | _ :: _ =>
    match decodeStep off input with
    | .error e => .error e
    | .ok s =>
      if source && s.value == 0 then .error ⟨off, .nul⟩
      else if source && s.value == 0xFEFF then .error ⟨off, .bom⟩
      else decodeLoop source (off + s.chunk.length) s.rest (s.value :: acc)
termination_by input.length
decreasing_by simpa only [_hinput] using s.shorter

/-- Decode UTF-8 from a byte offset. -/
def decodeAt (off : Nat) (bs : List UInt8) : Except DecodeError (List Nat) :=
  decodeLoop false off bs []
/-- Decode from byte zero. -/
def decodeUtf8 (bs : List UInt8) : Except DecodeError (List Nat) := decodeAt 0 bs
/-- Check source restrictions while decoding, short circuiting at the first refusal. -/
def sourceAt (off : Nat) (bs : List UInt8) : Except DecodeError (List Nat) :=
  decodeLoop true off bs []
/-- Source checking from byte zero. Item 30 excludes raw NUL and U+FEFF everywhere;
Q4.2 concerns static text values, which can contain NUL via the `\0` escape. -/
def checkSource (bs : List UInt8) : Except DecodeError (List Nat) := sourceAt 0 bs

/-- The source flag adds exactly the raw-source scalar restrictions of item 30. -/
def Allowed (source : Bool) (cs : List Nat) : Prop :=
  ∀ c ∈ cs, source = true → c ≠ 0 ∧ c ≠ 0xFEFF

/-- Loop success decomposes into the accumulated prefix and a valid remaining decoding. -/
theorem decodeLoop_sound (source : Bool) (off : Nat) (input : List UInt8)
    (acc out : List Nat) (h : decodeLoop source off input acc = .ok out) :
    ∃ cs, Utf8.Decodes input cs ∧ Allowed source cs ∧ out = acc.reverse ++ cs := by
  induction off, input, acc using decodeLoop.induct source generalizing out with
  | case1 off acc =>
    rw [decodeLoop.eq_def] at h
    exact ⟨[], .nil, by simp [Allowed], by simpa using (Except.ok.inj h).symm⟩
  | case2 off acc a bs e he =>
    rw [decodeLoop.eq_def] at h
    simp only [he] at h
    cases h
  | case3 off acc a bs s hs hn =>
    rw [decodeLoop.eq_def] at h
    simp only [hs, hn, ↓reduceIte] at h
    cases h
  | case4 off acc a bs s hs hn hb =>
    rw [decodeLoop.eq_def] at h
    simp only [hs, hn, hb, Bool.false_eq_true, ↓reduceIte] at h
    cases h
  | case5 off acc a bs s hs hn hb ih =>
    rw [decodeLoop.eq_def] at h
    simp only [hs, hn, hb, Bool.false_eq_true, ↓reduceIte] at h
    obtain ⟨cs, hd, good, hout⟩ := ih out h
    refine ⟨s.value :: cs, ?_, ?_, ?_⟩
    · exact Eq.mpr (congrArg (fun bytes => Utf8.Decodes bytes (s.value :: cs)) s.split)
        (.cons s.encodes hd)
    · intro c hc hsrc
      rcases List.mem_cons.mp hc with he | ht
      · subst c
        simp only [hsrc, Bool.true_and, beq_iff_eq] at hn hb
        exact ⟨hn, hb⟩
      · exact good c ht hsrc
    · simpa only [List.reverse_cons, List.append_assoc, List.singleton_append] using hout

/-- A declarative decoding is accepted by the loop with exactly its accumulated prefix. -/
theorem decodeLoop_complete {input : List UInt8} {cs : List Nat}
    (h : Utf8.Decodes input cs) (source : Bool) (good : Allowed source cs)
    (off : Nat) (acc : List Nat) :
    decodeLoop source off input acc = .ok (acc.reverse ++ cs) := by
  induction h generalizing off acc with
  | nil => simp only [decodeLoop, List.append_nil]
  | @cons c chunk bs cs enc tail ih =>
    obtain ⟨s, hs, hv, hr⟩ := decodeStep_complete (bs := bs) enc off
    have hn : chunk ++ bs ≠ [] := by
      intro he
      exact encodes_nonempty enc (List.append_eq_nil_iff.mp he).1
    have gn := good c (by simp)
    have gt : Allowed source cs := fun x hx => good x (by simp [hx])
    rw [decodeLoop.eq_def]
    split
    · contradiction
    · rw [hs]
      have h0 : ¬ (source && c == 0) = true := by
        simp only [Bool.and_eq_true, beq_iff_eq]
        rintro ⟨ht, he⟩
        exact (gn ht).1 he
      have hB : ¬ (source && c == 0xFEFF) = true := by
        simp only [Bool.and_eq_true, beq_iff_eq]
        rintro ⟨ht, he⟩
        exact (gn ht).2 he
      simp only [hr, hv, h0, hB, Bool.false_eq_true, ↓reduceIte]
      simpa only [List.reverse_cons, List.append_assoc, List.singleton_append] using
        ih gt (off + s.chunk.length) (c :: acc)

/-- Every success satisfies the independent decoding judgment. -/
theorem decodeAt_sound (bs : List UInt8) (off : Nat) (cs : List Nat)
    (h : decodeAt off bs = .ok cs) : Utf8.Decodes bs cs := by
  obtain ⟨ds, hd, _, he⟩ := decodeLoop_sound false off bs [] cs h
  simpa using he ▸ hd

/-- Every declarative decoding is accepted at any starting offset. -/
theorem decodeAt_complete {bs : List UInt8} {cs : List Nat} (h : Utf8.Decodes bs cs)
    (off : Nat) : decodeAt off bs = .ok cs :=
  decodeLoop_complete h false (by simp [Allowed]) off []

/-- Successful decoding is sound. -/
theorem decodeUtf8_sound {bs : List UInt8} {cs : List Nat}
    (h : decodeUtf8 bs = .ok cs) : Utf8.Decodes bs cs := decodeAt_sound bs 0 cs h

/-- Every declarative decoding is returned by the decoder. -/
theorem decodeUtf8_complete {bs : List UInt8} {cs : List Nat}
    (h : Utf8.Decodes bs cs) : decodeUtf8 bs = .ok cs := decodeAt_complete h 0

/-- Successful source checking is exactly decoding with item 30's restrictions. -/
theorem sourceAt_iff (off : Nat) (bs : List UInt8) (cs : List Nat) :
    sourceAt off bs = .ok cs ↔
      decodeAt off bs = .ok cs ∧ ∀ c ∈ cs, c ≠ 0 ∧ c ≠ 0xFEFF := by
  constructor
  · intro h
    obtain ⟨ds, hd, good, he⟩ := decodeLoop_sound true off bs [] cs h
    simp only [List.reverse_nil, List.nil_append] at he
    subst ds
    exact ⟨decodeAt_complete hd off, fun c hc => good c hc rfl⟩
  · rintro ⟨hd, good⟩
    exact decodeLoop_complete (decodeAt_sound bs off cs hd) true
      (fun c hc _ => good c hc) off []

/-- A successful source check has the stated scalars and satisfies `SourceOk`. -/
theorem checkSource_sound {bs : List UInt8} {cs : List Nat}
    (h : checkSource bs = .ok cs) :
    Utf8.Decodes bs cs ∧ (∀ c ∈ cs, c ≠ 0 ∧ c ≠ 0xFEFF) ∧ SourceOk bs := by
  obtain ⟨hd, hs⟩ := (sourceAt_iff 0 bs cs).mp h
  have dec := decodeUtf8_sound hd
  exact ⟨dec, hs, cs, dec, hs⟩

/-- Every source-valid decoding is returned with exactly its scalars. -/
theorem checkSource_complete {bs : List UInt8} {cs : List Nat}
    (hd : Utf8.Decodes bs cs) (hs : ∀ c ∈ cs, c ≠ 0 ∧ c ≠ 0xFEFF) :
    checkSource bs = .ok cs :=
  (sourceAt_iff 0 bs cs).mpr ⟨decodeUtf8_complete hd, hs⟩

/-- Successful decoding is exactly the declarative relation. -/
theorem decode_ok_iff {bs : List UInt8} {cs : List Nat} :
    decodeUtf8 bs = .ok cs ↔ Utf8.Decodes bs cs :=
  ⟨decodeUtf8_sound, decodeUtf8_complete⟩

/-- Refusal rules out every declarative decoding. -/
theorem decode_error_not_decodes {bs : List UInt8} {e : DecodeError}
    (h : decodeUtf8 bs = .error e) : ∀ cs, ¬ Utf8.Decodes bs cs := by
  intro cs hd
  have hs := decodeUtf8_complete hd
  rw [h] at hs
  cases hs

/-- UTF-8 loop errors report the first declarative failure, independent of the accumulator. -/
theorem decodeLoop_error_first (off : Nat) (input : List UInt8) (acc : List Nat)
    {e : DecodeError} (h : decodeLoop false off input acc = .error e) :
    ∃ k, FirstError input k ∧ e.offset = off + k := by
  induction off, input, acc using decodeLoop.induct false with
  | case1 off acc => rw [decodeLoop.eq_def] at h; cases h
  | case2 off acc a bs e' he =>
    rw [decodeLoop.eq_def] at h
    simp only [he] at h
    cases h
    obtain ⟨k, hf, hk⟩ := decodeStep_error_first he
    exact ⟨k, firstError_here hf (by simp), hk⟩
  | case3 off acc a bs s hs hn => simp at hn
  | case4 off acc a bs s hs hn hb => simp at hb
  | case5 off acc a bs s hs hn hb ih =>
    rw [decodeLoop.eq_def] at h
    simp only [hs, Bool.false_and, Bool.false_eq_true, ↓reduceIte] at h
    obtain ⟨k, hf, hk⟩ := ih h
    refine ⟨s.chunk.length + k, ?_, by omega⟩
    exact Eq.mpr (congrArg (fun bytes => FirstError bytes (s.chunk.length + k)) s.split)
      (firstError_cons s.encodes hf)

/-- Decoder errors return exactly the first byte with no well-formed scalar continuation,
or EOF for an unfinished scalar; previously completed scalars form a valid prefix. -/
theorem decode_error_first {bs : List UInt8} {e : DecodeError}
    (h : decodeUtf8 bs = .error e) : FirstError bs e.offset := by
  obtain ⟨k, hf, hk⟩ := decodeLoop_error_first 0 bs [] h
  simp only [Nat.zero_add] at hk
  exact hk ▸ hf

/-- Once decoding refuses, its offset is exactly the unique declarative first error. -/
theorem decode_error_offset_iff {bs : List UInt8} {e : DecodeError} {k : Nat}
    (h : decodeUtf8 bs = .error e) : FirstError bs k ↔ e.offset = k := by
  have hf := decode_error_first h
  constructor
  · exact firstError_unique hf
  · intro he; exact he ▸ hf

/-- Every declarative first error is returned, at exactly its stated offset. -/
theorem decode_error_complete {bs : List UInt8} {k : Nat} (hf : FirstError bs k) :
    ∃ e, decodeUtf8 bs = .error e ∧ e.offset = k := by
  cases he : decodeUtf8 bs with
  | ok cs => exact False.elim (firstError_not_decodes hf cs (decodeUtf8_sound he))
  | error e => exact ⟨e, rfl, firstError_unique (decode_error_first he) hf⟩

/-- Decoder failure, including its offset, is exactly the declarative first error. -/
theorem decode_error_iff {bs : List UInt8} {k : Nat} :
    (∃ e, decodeUtf8 bs = .error e ∧ e.offset = k) ↔ FirstError bs k := by
  constructor
  · rintro ⟨e, he, hk⟩
    exact hk ▸ decode_error_first he
  · exact decode_error_complete

/-- Source loop errors stop at the first invalid or forbidden scalar. -/
theorem sourceLoop_error_first (off : Nat) (input : List UInt8) (acc : List Nat)
    {e : DecodeError} (h : decodeLoop true off input acc = .error e) :
    ∃ k, SourceFailure input k ∧ e.offset = off + k := by
  induction off, input, acc using decodeLoop.induct true with
  | case1 off acc => rw [decodeLoop.eq_def] at h; cases h
  | case2 off acc a bs e' he =>
    rw [decodeLoop.eq_def] at h
    simp only [he] at h
    cases h
    obtain ⟨k, hf, hk⟩ := decodeStep_error_first he
    exact ⟨k, .utf8 (by simp) hf, hk⟩
  | case3 off acc a bs s hs hn =>
    rw [decodeLoop.eq_def] at h
    simp only [hs, hn, ↓reduceIte] at h
    cases h
    have hv : s.value = 0 := by simpa using hn
    refine ⟨0, ?_, by simp⟩
    exact Eq.mpr (congrArg (fun bytes => SourceFailure bytes 0) s.split)
      (.nul (hv ▸ s.encodes))
  | case4 off acc a bs s hs hn hb =>
    rw [decodeLoop.eq_def] at h
    simp only [hs, hn, hb, Bool.false_eq_true, ↓reduceIte] at h
    cases h
    have hv : s.value = 0xFEFF := by simpa using hb
    refine ⟨0, ?_, by simp⟩
    exact Eq.mpr (congrArg (fun bytes => SourceFailure bytes 0) s.split)
      (.bom (hv ▸ s.encodes))
  | case5 off acc a bs s hs hn hb ih =>
    rw [decodeLoop.eq_def] at h
    simp only [hs, hn, hb, Bool.false_eq_true, ↓reduceIte] at h
    obtain ⟨k, hf, hk⟩ := ih h
    have h0 : s.value ≠ 0 := by simpa using hn
    have hB : s.value ≠ 0xFEFF := by simpa using hb
    refine ⟨s.chunk.length + k, ?_, by omega⟩
    exact Eq.mpr (congrArg (fun bytes => SourceFailure bytes (s.chunk.length + k)) s.split)
      (.cons s.encodes h0 hB hf)

/-- Source checking reports the first source failure, with scalar-start NUL/BOM offsets. -/
theorem checkSource_error_first {bs : List UInt8} {e : DecodeError}
    (h : checkSource bs = .error e) : SourceFailure bs e.offset := by
  obtain ⟨k, hf, hk⟩ := sourceLoop_error_first 0 bs [] h
  simp only [Nat.zero_add] at hk
  exact hk ▸ hf

/-- A refused source has no permitted declarative decoding. -/
theorem checkSource_error_not_sourceOk {bs : List UInt8} {e : DecodeError}
    (h : checkSource bs = .error e) : ¬ SourceOk bs := by
  rintro ⟨cs, hd, good⟩
  have hs := checkSource_complete hd good
  rw [h] at hs
  cases hs

/-- Test the settled ASCII identifier-start predicate. -/
def identStart? (c : Nat) : Bool := decide (IdentStart c)

/-- Test the settled ASCII identifier-continuation predicate. -/
def identContinue? (c : Nat) : Bool := decide (IdentContinue c)

/-- The start test agrees with the declarative predicate. -/
theorem identStart?_iff (c : Nat) : identStart? c = true ↔ IdentStart c := by
  simp [identStart?]

/-- The continuation test agrees with the declarative predicate. -/
theorem identContinue?_iff (c : Nat) : identContinue? c = true ↔ IdentContinue c := by
  simp [identContinue?]

/-- Skip a comment body, retaining its first LF or CR. Tail recursive. -/
def scanLine : List Nat → List Nat
  | [] => []
  | c :: cs => if c = 10 ∨ c = 13 then c :: cs else scanLine cs

/-- Skip a line comment only when the input begins with two slashes. -/
def lineComment : List Nat → Option (List Nat)
  | 47 :: 47 :: cs => some (scanLine cs)
  | _ => none

/-- Skipping satisfies the declarative stopping rule. -/
theorem scanLine_sound (cs : List Nat) : CommentBody cs (scanLine cs) := by
  induction cs with
  | nil => exact .eof
  | cons c cs ih =>
    by_cases hl : c = 10
    · subst c; simpa [scanLine] using CommentBody.lf cs
    · by_cases hr : c = 13
      · subst c; simpa [scanLine] using CommentBody.cr cs
      · simpa [scanLine, hl, hr] using CommentBody.cons hl hr ih

/-- Every declarative suffix is returned by the scanner. -/
theorem scanLine_complete {cs rest : List Nat} (h : CommentBody cs rest) :
    scanLine cs = rest := by
  induction h with
  | eof => rfl
  | lf rest => simp [scanLine]
  | cr rest => simp [scanLine]
  | cons hl hr _ ih => simp [scanLine, hl, hr, ih]

/-- Successful line-comment scanning satisfies the independent judgment. -/
theorem lineComment_sound {cs out : List Nat}
    (h : lineComment cs = some out) : LineComment cs out := by
  unfold lineComment at h
  split at h
  · rename_i body
    cases Option.some.inj h
    exact .intro (scanLine_sound body)
  · cases h

/-- Every declarative comment is recognized with the same suffix. -/
theorem lineComment_complete {cs out : List Nat}
    (h : LineComment cs out) : lineComment cs = some out := by
  cases h with
  | intro hb => simp [lineComment, scanLine_complete hb]

/-- Comment skipping is exactly the declarative relation. -/
theorem lineComment_some_iff {cs out : List Nat} :
    lineComment cs = some out ↔ LineComment cs out :=
  ⟨lineComment_sound, lineComment_complete⟩


example : decodeUtf8 [65, 95, 57] = .ok [65, 95, 57] := by
  simp [decodeUtf8, decodeAt, decodeLoop, decodeStep, Utf8.One, Borrow.isScalar]
example : decodeUtf8 [0xC2, 0xA2] = .ok [0xA2] := by
  simp [decodeUtf8, decodeAt, decodeLoop, decodeStep, Utf8.One, Utf8.Two, Borrow.isScalar, Borrow.cont]
example : decodeUtf8 [0xE2, 0x82, 0xAC] = .ok [0x20AC] := by
  simp [decodeUtf8, decodeAt, decodeLoop, decodeStep, secondOk, Utf8.One, Utf8.Two, Utf8.Three, Borrow.isScalar, Borrow.cont]
example : decodeUtf8 [0xF0, 0x9F, 0x98, 0x80] = .ok [0x1F600] := by
  simp [decodeUtf8, decodeAt, decodeLoop, decodeStep, secondOk, Utf8.One, Utf8.Two, Utf8.Three, Utf8.Four, Borrow.isScalar, Borrow.cont]
example : decodeUtf8 [0xC0, 0x80] = .error ⟨0, .overlong⟩ := by
  simp [decodeUtf8, decodeAt, decodeLoop, decodeStep, leadReason, Utf8.One, Borrow.isScalar]
example : decodeUtf8 [0xED, 0xA0, 0x80] = .error ⟨1, .surrogate⟩ := by
  simp [decodeUtf8, decodeAt, decodeLoop, decodeStep, secondReason, secondOk, Utf8.One, Utf8.Two, Borrow.isScalar, Borrow.cont]
example : decodeUtf8 [0xF4, 0x90, 0x80, 0x80] = .error ⟨1, .tooLarge⟩ := by
  simp [decodeUtf8, decodeAt, decodeLoop, decodeStep, secondReason, secondOk, Utf8.One, Utf8.Two, Borrow.isScalar, Borrow.cont]
example : decodeUtf8 [0xE2, 0x82] = .error ⟨2, .truncated⟩ := by
  simp [decodeUtf8, decodeAt, decodeLoop, decodeStep, secondOk, Utf8.One, Utf8.Two, Borrow.isScalar, Borrow.cont]
example : decodeUtf8 [0x80] = .error ⟨0, .unexpectedContinuation⟩ := by
  simp [decodeUtf8, decodeAt, decodeLoop, decodeStep, leadReason, Utf8.One, Borrow.isScalar]
example : checkSource [0] = .error ⟨0, .nul⟩ := by
  simp [checkSource, sourceAt, decodeLoop, decodeStep, Utf8.One, Borrow.isScalar]
example : checkSource [0xEF, 0xBB, 0xBF] = .error ⟨0, .bom⟩ := by
  simp [checkSource, sourceAt, decodeLoop, decodeStep, secondOk, Utf8.One, Utf8.Two, Utf8.Three, Borrow.isScalar, Borrow.cont]
example : checkSource [65, 0] = .error ⟨1, .nul⟩ := by
  simp [checkSource, sourceAt, decodeLoop, decodeStep, Utf8.One, Borrow.isScalar]
example : checkSource [65, 0xEF, 0xBB, 0xBF] = .error ⟨1, .bom⟩ := by
  simp [checkSource, sourceAt, decodeLoop, decodeStep, secondOk, Utf8.One, Utf8.Two, Utf8.Three, Borrow.isScalar, Borrow.cont]
example : checkSource [0, 0x80] = .error ⟨0, .nul⟩ := by
  simp [checkSource, sourceAt, decodeLoop, decodeStep, Utf8.One, Borrow.isScalar]
example : checkSource [0xC2, 0xA2, 0] = .error ⟨2, .nul⟩ := by
  simp [checkSource, sourceAt, decodeLoop, decodeStep, Utf8.One, Utf8.Two, Borrow.isScalar, Borrow.cont]
example : checkSource [65] = .ok [65] := by
  simp [checkSource, sourceAt, decodeLoop, decodeStep, Utf8.One, Borrow.isScalar]
example : identStart? 0xE9 = false ∧ identContinue? 0xE9 = false := by decide

example : lineComment [47,47,32,120,10,114,101,115,116] =
    some [10,114,101,115,116] := by rfl
example : lineComment [47,47,32,120,13] = some [13] := by rfl
example : lineComment [47,47,32,120] = some [] := by rfl
example : lineComment [47,42,32,120,32,42,47] = none := by rfl

example : decodeUtf8 [0xC2,65] = .error ⟨1, .invalidContinuation⟩ := by
  simp [decodeUtf8, decodeAt, decodeLoop, decodeStep, secondReason, secondOk, Utf8.One, Utf8.Two, Borrow.isScalar, Borrow.cont]
example : decodeUtf8 [0xE2,0x82,65] = .error ⟨2, .invalidContinuation⟩ := by
  simp [decodeUtf8, decodeAt, decodeLoop, decodeStep, secondOk, Utf8.One, Utf8.Two, Utf8.Three, Borrow.isScalar, Borrow.cont]
example : decodeUtf8 [0xF0,0x90,0x80,65] = .error ⟨3, .invalidContinuation⟩ := by
  simp [decodeUtf8, decodeAt, decodeLoop, decodeStep, secondOk, Utf8.One, Utf8.Two, Utf8.Three, Utf8.Four, Borrow.isScalar, Borrow.cont]
example : decodeUtf8 [65,0x80] = .error ⟨1, .unexpectedContinuation⟩ := by
  simp [decodeUtf8, decodeAt, decodeLoop, decodeStep, leadReason, Utf8.One, Borrow.isScalar]
example : decodeUtf8 [0xC2] = .error ⟨1, .truncated⟩ := by
  simp [decodeUtf8, decodeAt, decodeLoop, decodeStep, Utf8.One, Borrow.isScalar]
example : decodeUtf8 [0xE0,0x80] = .error ⟨1, .overlong⟩ := by
  simp [decodeUtf8, decodeAt, decodeLoop, decodeStep, secondReason, secondOk, Utf8.One, Utf8.Two, Borrow.isScalar, Borrow.cont]

end Concrete.Lex
