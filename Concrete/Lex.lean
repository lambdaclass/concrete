import Concrete.Interfaces.Carriers
import Concrete.Refusal
import ConcreteSpec.Proofs.Surface0

/-!
# Settled lexer components

Total byte-list UTF-8 decoding and source checking, ASCII identifiers, the four settled whitespace
scalars, line comments and maximal trivia scanning. Strings and comments refuse raw Cc,
U+2028/U+2029 and bidi controls with internal errors; comments alone permit raw TAB. Byte literals
have printable-ASCII raw scalars and exactly the six decided escapes; labels are quote plus an
identifier. Longest match uses an arbitrary table. Integers implement decimal and the lowercase
`0x`/`0b`/`0o` bases, lowercase hexadecimal digits and eight suffixes. Malformed adjacent text is
one refused token, and decimal leading-zero refusal is retained. The slice-0 entry point
combines these components using the word and punctuation classifications in Surface0.
Error constructors are internal data, not
diagnostic wording. Offsets are zero-based byte offsets;
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
private def decodeLoopSpec (source : Bool) (off : Nat) (input : List UInt8) (acc : List Nat) :
    Except DecodeError (List Nat) :=
  match _hinput : input with
  | [] => .ok acc.reverse
  | _ :: _ =>
    match decodeStep off input with
    | .error e => .error e
    | .ok s =>
      if source && s.value == 0 then .error ⟨off, .nul⟩
      else if source && s.value == 0xFEFF then .error ⟨off, .bom⟩
      else decodeLoopSpec source (off + s.chunk.length) s.rest (s.value :: acc)
termination_by input.length
decreasing_by simpa only [_hinput] using s.shorter

/-- Each successful scalar consumes at least one byte, so one unit of fuel per byte suffices. -/
private def decodeFuel (source : Bool) :
    Nat → Nat → List UInt8 → List Nat → Except DecodeError (List Nat)
  | _, _, [], acc => .ok acc.reverse
  | 0, off, _ :: _, _ => .error ⟨off, .truncated⟩
  | fuel + 1, off, a :: bs, acc =>
    match decodeStep off (a :: bs) with
    | .error e => .error e
    | .ok s =>
      if source && s.value == 0 then .error ⟨off, .nul⟩
      else if source && s.value == 0xFEFF then .error ⟨off, .bom⟩
      else decodeFuel source fuel (off + s.chunk.length) s.rest (s.value :: acc)

/-- Decode by structural recursion so concrete source checks reduce in the kernel. -/
def decodeLoop (source : Bool) (off : Nat) (input : List UInt8) (acc : List Nat) :
    Except DecodeError (List Nat) :=
  decodeFuel source input.length off input acc

private theorem decodeFuel_suffices (source : Bool) (fuel off : Nat)
    (input : List UInt8) (acc : List Nat) (bound : input.length ≤ fuel) :
    decodeFuel source fuel off input acc = decodeLoopSpec source off input acc := by
  induction fuel generalizing off input acc with
  | zero =>
    have h : input = [] := List.length_eq_zero_iff.mp (by omega)
    subst input
    rw [decodeLoopSpec.eq_def]
    rfl
  | succ fuel ih =>
    rw [decodeLoopSpec.eq_def]
    cases input with
    | nil => rfl
    | cons a bs =>
      cases h : decodeStep off (a :: bs) with
      | error e => simp [decodeFuel, h]
      | ok step =>
        simp only [decodeFuel, h]
        split
        · rfl
        · split
          · rfl
          · apply ih
            have := step.shorter
            omega

private theorem decodeLoop_eq_spec (source : Bool) (off : Nat) (input : List UInt8)
    (acc : List Nat) : decodeLoop source off input acc = decodeLoopSpec source off input acc :=
  decodeFuel_suffices source input.length off input acc (Nat.le_refl _)

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
  rw [decodeLoop_eq_spec] at h
  induction off, input, acc using decodeLoopSpec.induct source generalizing out with
  | case1 off acc =>
    rw [decodeLoopSpec.eq_def] at h
    exact ⟨[], .nil, by simp [Allowed], by simpa using (Except.ok.inj h).symm⟩
  | case2 off acc a bs e he =>
    rw [decodeLoopSpec.eq_def] at h
    simp only [he] at h
    cases h
  | case3 off acc a bs s hs hn =>
    rw [decodeLoopSpec.eq_def] at h
    simp only [hs, hn, ↓reduceIte] at h
    cases h
  | case4 off acc a bs s hs hn hb =>
    rw [decodeLoopSpec.eq_def] at h
    simp only [hs, hn, hb, Bool.false_eq_true, ↓reduceIte] at h
    cases h
  | case5 off acc a bs s hs hn hb ih =>
    rw [decodeLoopSpec.eq_def] at h
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
  rw [decodeLoop_eq_spec]
  induction h generalizing off acc with
  | nil => simp only [decodeLoopSpec, List.append_nil]
  | @cons c chunk bs cs enc tail ih =>
    obtain ⟨s, hs, hv, hr⟩ := decodeStep_complete (bs := bs) enc off
    have hn : chunk ++ bs ≠ [] := by
      intro he
      exact encodes_nonempty enc (List.append_eq_nil_iff.mp he).1
    have gn := good c (by simp)
    have gt : Allowed source cs := fun x hx => good x (by simp [hx])
    rw [decodeLoopSpec.eq_def]
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
  rw [decodeLoop_eq_spec] at h
  induction off, input, acc using decodeLoopSpec.induct false with
  | case1 off acc => rw [decodeLoopSpec.eq_def] at h; cases h
  | case2 off acc a bs e' he =>
    rw [decodeLoopSpec.eq_def] at h
    simp only [he] at h
    cases h
    obtain ⟨k, hf, hk⟩ := decodeStep_error_first he
    exact ⟨k, firstError_here hf (by simp), hk⟩
  | case3 off acc a bs s hs hn => simp at hn
  | case4 off acc a bs s hs hn hb => simp at hb
  | case5 off acc a bs s hs hn hb ih =>
    rw [decodeLoopSpec.eq_def] at h
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
  rw [decodeLoop_eq_spec] at h
  induction off, input, acc using decodeLoopSpec.induct true with
  | case1 off acc => rw [decodeLoopSpec.eq_def] at h; cases h
  | case2 off acc a bs e' he =>
    rw [decodeLoopSpec.eq_def] at h
    simp only [he] at h
    cases h
    obtain ⟨k, hf, hk⟩ := decodeStep_error_first he
    exact ⟨k, .utf8 (by simp) hf, hk⟩
  | case3 off acc a bs s hs hn =>
    rw [decodeLoopSpec.eq_def] at h
    simp only [hs, hn, ↓reduceIte] at h
    cases h
    have hv : s.value = 0 := by simpa using hn
    refine ⟨0, ?_, by simp⟩
    exact Eq.mpr (congrArg (fun bytes => SourceFailure bytes 0) s.split)
      (.nul (hv ▸ s.encodes))
  | case4 off acc a bs s hs hn hb =>
    rw [decodeLoopSpec.eq_def] at h
    simp only [hs, hn, hb, Bool.false_eq_true, ↓reduceIte] at h
    cases h
    have hv : s.value = 0xFEFF := by simpa using hb
    refine ⟨0, ?_, by simp⟩
    exact Eq.mpr (congrArg (fun bytes => SourceFailure bytes 0) s.split)
      (.bom (hv ▸ s.encodes))
  | case5 off acc a bs s hs hn hb ih =>
    rw [decodeLoopSpec.eq_def] at h
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

/-- Successful source checking is exactly source-valid value decoding. -/
theorem checkSource_ok_iff {bs : List UInt8} {cs : List Nat} :
    checkSource bs = .ok cs ↔ Utf8.Decodes bs cs ∧ ∀ c ∈ cs, c ≠ 0 ∧ c ≠ 0xFEFF := by
  constructor
  · intro h; exact ⟨(checkSource_sound h).1, (checkSource_sound h).2.1⟩
  · rintro ⟨hd, good⟩; exact checkSource_complete hd good

/-- A source error's offset is exactly the unique declarative source failure. -/
theorem checkSource_error_offset_iff {bs : List UInt8} {e : DecodeError} {k : Nat}
    (h : checkSource bs = .error e) : SourceFailure bs k ↔ e.offset = k := by
  have hf := checkSource_error_first h
  constructor
  · exact sourceFailure_unique hf
  · intro he; exact he ▸ hf

/-- Every declarative source failure is returned at its exact offset. -/
theorem checkSource_error_complete {bs : List UInt8} {k : Nat} (hf : SourceFailure bs k) :
    ∃ e, checkSource bs = .error e ∧ e.offset = k := by
  cases he : checkSource bs with
  | ok cs => exact False.elim (sourceFailure_not_sourceOk hf (checkSource_sound he).2.2)
  | error e => exact ⟨e, rfl, sourceFailure_unique (checkSource_error_first he) hf⟩

/-- Source refusal, including its offset, is exactly the declarative first source failure. -/
theorem checkSource_error_iff {bs : List UInt8} {k : Nat} :
    (∃ e, checkSource bs = .error e ∧ e.offset = k) ↔ SourceFailure bs k := by
  constructor
  · rintro ⟨e, he, hk⟩; exact hk ▸ checkSource_error_first he
  · exact checkSource_error_complete

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

/-- Test exactly the four settled whitespace scalars. -/
def whitespace? (c : Nat) : Bool := decide (Whitespace c)

/-- The whitespace test agrees with the declarative predicate. -/
theorem whitespace?_iff (c : Nat) : whitespace? c = true ↔ Whitespace c := by
  simp [whitespace?]

/-- Internal comment refusals; constructor names are not user-visible diagnostics. -/
inductive CommentError where
  | rawForbidden (c : Nat)
  deriving DecidableEq, Repr

/-- Skip a comment body, retaining its first LF or CR. Tail recursive, with raw refusals. -/
def scanLine : List Nat → Except CommentError (List Nat)
  | [] => .ok []
  | c :: cs =>
    if c = 10 ∨ c = 13 then .ok (c :: cs)
    else if CommentRaw c then scanLine cs else .error (.rawForbidden c)

/-- Skip a line comment only when its two opening slashes are present. -/
def lineComment : List Nat → Except CommentError (Option (List Nat))
  | 47 :: 47 :: cs =>
    match scanLine cs with
    | .ok rest => .ok (some rest)
    | .error e => .error e
  | _ => .ok none

/-- A successful body scan satisfies the declarative stopping and raw-scalar rules. -/
theorem scanLine_sound {cs out : List Nat}
    (h : scanLine cs = .ok out) : CommentBody cs out := by
  induction cs generalizing out with
  | nil => cases Except.ok.inj h; exact .eof
  | cons c cs ih =>
    by_cases hl : c = 10
    · subst c
      simp only [scanLine, true_or, ↓reduceIte] at h
      cases Except.ok.inj h
      exact .lf cs
    · by_cases hr : c = 13
      · subst c
        simp only [scanLine, hl, false_or, ↓reduceIte] at h
        cases Except.ok.inj h
        exact .cr cs
      · by_cases hc : CommentRaw c
        · simp only [scanLine, hl, hr, hc, ↓reduceIte] at h
          exact .cons hl hr hc (ih h)
        · simp [scanLine, hl, hr, hc] at h

/-- Every declarative body suffix is returned successfully. -/
theorem scanLine_complete {cs rest : List Nat} (h : CommentBody cs rest) :
    scanLine cs = .ok rest := by
  induction h with
  | eof => rfl
  | lf rest => simp [scanLine]
  | cr rest => simp [scanLine]
  | cons hl hr hc _ ih => simp [scanLine, hl, hr, hc, ih]

/-- Successful line-comment scanning satisfies the independent judgment. -/
theorem lineComment_sound {cs out : List Nat}
    (h : lineComment cs = .ok (some out)) : LineComment cs out := by
  unfold lineComment at h
  split at h
  · rename_i input
    cases hscan : scanLine input with
    | error e => simp [hscan] at h
    | ok rest =>
      simp only [hscan, Except.ok.injEq, Option.some.injEq] at h
      cases h
      exact .intro (scanLine_sound hscan)
  · cases h

/-- Every declarative comment is recognized with the same suffix. -/
theorem lineComment_complete {cs out : List Nat}
    (h : LineComment cs out) : lineComment cs = .ok (some out) := by
  cases h with
  | intro hb => simp [lineComment, scanLine_complete hb]

/-- Successful comment skipping is exactly the declarative relation. -/
theorem lineComment_some_iff {cs out : List Nat} :
    lineComment cs = .ok (some out) ↔ LineComment cs out :=
  ⟨lineComment_sound, lineComment_complete⟩

/-- A non-comment result is exactly absence of an opening double slash.
Malformed comment bodies produce errors rather than a non-comment result. -/
theorem lineComment_none_iff {cs : List Nat} :
    lineComment cs = .ok none ↔ ∀ tail, cs ≠ 47 :: 47 :: tail := by
  unfold lineComment
  split
  · rename_i input
    cases scanLine input <;> simp
  · rename_i h
    simp only [true_iff]
    exact h

/-- A comment error is exactly an opening double slash with no valid body suffix. -/
theorem lineComment_error_iff {cs : List Nat} :
    (∃ e, lineComment cs = .error e) ↔
      ∃ body, cs = 47 :: 47 :: body ∧ ∀ out, ¬ CommentBody body out := by
  constructor
  · rintro ⟨e, he⟩
    unfold lineComment at he
    split at he
    · rename_i body
      refine ⟨body, rfl, ?_⟩
      intro out hb
      simp [scanLine_complete hb] at he
    · cases he
  · rintro ⟨body, rfl, hb⟩
    cases hs : scanLine body with
    | error e => exact ⟨e, by simp [lineComment, hs]⟩
    | ok out => exact (hb out (scanLine_sound hs)).elim

/-- A comment body's remainder is no longer than its input. -/
theorem commentBody_rest_length_le {input rest : List Nat}
    (h : CommentBody input rest) : rest.length ≤ input.length := by
  induction h with
  | eof => simp
  | lf rest => simp
  | cr rest => simp
  | cons _ _ _ _ ih => simp only [List.length_cons]; omega

/-- Scanning a line comment consumes at least its two slashes. -/
theorem lineComment_rest_length_lt {input rest : List Nat}
    (h : LineComment input rest) : rest.length < input.length := by
  cases h with
  | intro hb =>
    have hle := commentBody_rest_length_le hb
    simp only [List.length_cons]
    omega

/-- Structural tail-recursive scanner used with fuel at least the input length.
Comment errors propagate immediately; no discarded trivia prefix is constructed. -/
def skipTriviaAux : Nat → List Nat → Except CommentError (List Nat)
  | _, [] => .ok []
  | 0, cs => .ok cs
  | fuel + 1, c :: input =>
    if Whitespace c then skipTriviaAux fuel input
    else
      match lineComment (c :: input) with
      | .ok (some rest) => skipTriviaAux fuel rest
      | .ok none => .ok (c :: input)
      | .error e => .error e

/-- Skip a maximal run of whitespace scalars and valid line comments, returning its suffix. -/
def skipTrivia (cs : List Nat) : Except CommentError (List Nat) :=
  skipTriviaAux cs.length cs

/-- With enough fuel, a successful structural scan satisfies the maximal-trivia judgment. -/
theorem skipTriviaAux_sound {cs out : List Nat}
    (fuel : Nat) (hfuel : cs.length ≤ fuel) (h : skipTriviaAux fuel cs = .ok out) :
    Trivia cs out := by
  induction fuel generalizing cs out with
  | zero =>
    cases cs with
    | nil =>
      cases Except.ok.inj h
      exact .stop [] (by intro c cs h; cases h) (by intro cs h; cases h)
    | cons c input => simp at hfuel
  | succ fuel ih =>
    cases cs with
    | nil =>
      cases Except.ok.inj h
      exact .stop [] (by intro c cs h; cases h) (by intro cs h; cases h)
    | cons c input =>
      by_cases hw : Whitespace c
      · rw [skipTriviaAux.eq_def] at h
        simp only [hw, ↓reduceIte] at h
        exact .whitespace hw (ih (by simp only [List.length_cons] at hfuel; omega) h)
      · rw [skipTriviaAux.eq_def] at h
        simp only [hw, ↓reduceIte] at h
        cases hline : lineComment (c :: input) with
        | error e => simp [hline] at h
        | ok scanned =>
          cases scanned with
          | none =>
            simp only [hline] at h
            cases Except.ok.inj h
            apply Trivia.stop
            · intro d ds heq; cases heq; exact hw
            · exact lineComment_none_iff.mp hline
          | some rest =>
            simp only [hline] at h
            have hcomment := lineComment_sound hline
            apply Trivia.comment hcomment
            apply ih _ h
            have hlt := lineComment_rest_length_lt hcomment
            simp only [List.length_cons] at hfuel hlt
            omega

/-- With enough fuel, every declarative suffix is the structural scanner's result. -/
theorem skipTriviaAux_complete {cs out : List Nat}
    (h : Trivia cs out) (fuel : Nat) (hfuel : cs.length ≤ fuel) :
    skipTriviaAux fuel cs = .ok out := by
  induction h generalizing fuel with
  | stop rest hw hc =>
    cases rest with
    | nil => cases fuel <;> rfl
    | cons c input =>
      have hline : lineComment (c :: input) = .ok none :=
        lineComment_none_iff.mpr hc
      cases fuel with
      | zero => simp at hfuel
      | succ fuel =>
        rw [skipTriviaAux.eq_def]
        simp only [hw c input rfl, ↓reduceIte, hline]
  | whitespace hw _ ih =>
    cases fuel with
    | zero => simp at hfuel
    | succ fuel =>
      rw [skipTriviaAux.eq_def]
      simp only [hw, ↓reduceIte]
      exact ih fuel (by simp only [List.length_cons] at hfuel; omega)
  | comment hl _ ih =>
    cases hl with
    | intro hb =>
      cases fuel with
      | zero => simp at hfuel
      | succ fuel =>
        have hline := lineComment_complete (LineComment.intro hb)
        have hlt := lineComment_rest_length_lt (LineComment.intro hb)
        have hrec := ih fuel (by simp only [List.length_cons] at hfuel hlt; omega)
        rw [skipTriviaAux.eq_def]
        simp only [show ¬ Whitespace 47 from by decide, ↓reduceIte, hline]
        exact hrec

/-- A successful public trivia scan satisfies the independent maximal-trivia judgment. -/
theorem skipTrivia_sound {cs out : List Nat}
    (h : skipTrivia cs = .ok out) : Trivia cs out :=
  skipTriviaAux_sound cs.length (Nat.le_refl _) h

/-- Every declarative maximal-trivia suffix is the scanner's result. -/
theorem skipTrivia_complete {cs out : List Nat}
    (h : Trivia cs out) : skipTrivia cs = .ok out :=
  skipTriviaAux_complete h cs.length (Nat.le_refl _)

/-- Maximal-trivia suffixes are unique. -/
theorem trivia_unique {cs out₁ out₂ : List Nat}
    (h₁ : Trivia cs out₁) (h₂ : Trivia cs out₂) : out₁ = out₂ := by
  have h := (skipTrivia_complete h₁).symm.trans (skipTrivia_complete h₂)
  exact Except.ok.inj h

/-- Scan the longest identifier-continuation run. -/
def scanIdentBody : List Nat → List Nat × List Nat
  | [] => ([], [])
  | c :: cs =>
    if IdentContinue c then let (body, rest) := scanIdentBody cs; (c :: body, rest)
    else ([], c :: cs)

/-- Scan an ASCII identifier only when its first scalar is a settled start scalar. -/
def identifier : List Nat → Option (List Nat × List Nat)
  | [] => none
  | c :: cs =>
    if IdentStart c then
      let (body, rest) := scanIdentBody cs
      some (c :: body, rest)
    else none

/-- The continuation scanner satisfies the independent maximal-run judgment. -/
theorem scanIdentBody_sound (cs : List Nat) :
    IdentBody cs (scanIdentBody cs).1 (scanIdentBody cs).2 := by
  induction cs with
  | nil =>
    exact .stop [] (by intro c cs h; cases h)
  | cons c cs ih =>
    by_cases hc : IdentContinue c
    · simpa [scanIdentBody, hc] using IdentBody.cons hc ih
    · simpa [scanIdentBody, hc] using
        IdentBody.stop (c :: cs) (by intro d ds h; cases h; exact hc)

/-- Every declarative continuation decomposition is the scanner's result. -/
theorem scanIdentBody_complete {cs body rest : List Nat} (h : IdentBody cs body rest) :
    scanIdentBody cs = (body, rest) := by
  induction h with
  | stop rest hc =>
    cases rest with
    | nil => rfl
    | cons c cs => simp [scanIdentBody, hc c cs rfl]
  | cons hc _ ih => simp [scanIdentBody, hc, ih]

/-- Successful identifier scanning satisfies the independent judgment. -/
theorem identifier_sound {cs : List Nat} {out : List Nat × List Nat}
    (h : identifier cs = some out) : Identifier cs out := by
  cases cs with
  | nil => simp [identifier] at h
  | cons c cs =>
    by_cases hc : IdentStart c
    · simp only [identifier, hc, ↓reduceIte, Option.some.injEq] at h
      cases h
      exact .intro hc (scanIdentBody_sound cs)
    · simp [identifier, hc] at h

/-- Every declarative identifier is recognized with the same lexeme and remainder. -/
theorem identifier_complete {cs : List Nat} {out : List Nat × List Nat}
    (h : Identifier cs out) : identifier cs = some out := by
  cases h with
  | intro hc hb => simp [identifier, hc, scanIdentBody_complete hb]

/-- Identifier decompositions are unique. -/
theorem identifier_unique {cs : List Nat} {out₁ out₂ : List Nat × List Nat}
    (h₁ : Identifier cs out₁) (h₂ : Identifier cs out₂) : out₁ = out₂ := by
  have h := (identifier_complete h₁).symm.trans (identifier_complete h₂)
  exact Option.some.inj h

/-- Decode exactly the six settled string escapes. -/
def decodeEscape : Nat → Option Nat
  | 110 => some 10
  | 116 => some 9
  | 114 => some 13
  | 92 => some 92
  | 34 => some 34
  | 48 => some 0
  | _ => none

/-- Every scanner-approved string escape is in the declarative escape relation. -/
theorem decodeEscape_sound {escaped decoded : Nat} (h : decodeEscape escaped = some decoded) :
    StringEscape escaped decoded := by
  unfold decodeEscape at h
  split at h <;> simp_all [StringEscape]

/-- Every declarative string escape is decoded to its settled scalar. -/
theorem decodeEscape_complete {escaped decoded : Nat} (h : StringEscape escaped decoded) :
    decodeEscape escaped = some decoded := by
  rcases h with (⟨rfl, rfl⟩ | ⟨rfl, rfl⟩ | ⟨rfl, rfl⟩ | ⟨rfl, rfl⟩ | ⟨rfl, rfl⟩ | ⟨rfl, rfl⟩)
  all_goals rfl

/-- Internal string-body refusals; their constructor names are not user-visible diagnostics. -/
inductive StringError where
  | unterminated | invalidEscape (escaped : Nat) | rawForbidden (c : Nat)
  deriving DecidableEq, Repr

/-- Prepend a decoded scalar to a successful string-body result. -/
def stringPrepend (c : Nat) : Except StringError (List Nat × List Nat) →
    Except StringError (List Nat × List Nat)
  | .error e => .error e
  | .ok (decoded, rest) => .ok (c :: decoded, rest)

/-- Scan a string body after its opening quote, refusing every owner-forbidden raw scalar. -/
def stringBody : List Nat → Except StringError (List Nat × List Nat)
  | [] => .error .unterminated
  | 34 :: rest => .ok ([], rest)
  | 92 :: escaped :: input =>
    match decodeEscape escaped with
    | some decoded => stringPrepend decoded (stringBody input)
    | none => .error (.invalidEscape escaped)
  | 92 :: [] => .error .unterminated
  | c :: input =>
    if ForbiddenRaw c then .error (.rawForbidden c) else stringPrepend c (stringBody input)

/-- Successful prepending exposes the successful tail and decoded list. -/
theorem stringPrepend_ok {c : Nat} {r : Except StringError (List Nat × List Nat)}
    {out : List Nat × List Nat} (h : stringPrepend c r = .ok out) :
    ∃ decoded rest, r = .ok (decoded, rest) ∧ out = (c :: decoded, rest) := by
  cases r with
  | error e => simp [stringPrepend] at h
  | ok tail =>
    cases tail with
    | mk decoded rest =>
      cases Except.ok.inj h
      exact ⟨decoded, rest, rfl, rfl⟩

/-- Every scanner result satisfies the independent string-body judgment. -/
theorem stringBody_sound {cs : List Nat} {out : List Nat × List Nat}
    (h : stringBody cs = .ok out) : StringBody cs out := by
  induction cs using stringBody.induct generalizing out with
  | case1 => simp [stringBody] at h
  | case2 rest =>
    cases Except.ok.inj h
    exact .close rest
  | case3 escaped input decodedScalar he ih =>
    simp only [stringBody, he] at h
    obtain ⟨decoded, rest, ht, rfl⟩ := stringPrepend_ok h
    exact .escape (decodeEscape_sound he) (ih ht)
  | case4 escaped input he => simp [stringBody, he] at h
  | case5 => simp [stringBody] at h
  | case6 c input hq hb hl hraw => simp [stringBody, hraw] at h
  | case7 c input hq hb hl hraw ih =>
    simp only [stringBody, hraw, ↓reduceIte] at h
    obtain ⟨decoded, rest, ht, rfl⟩ := stringPrepend_ok h
    have hb' : c ≠ 92 := by
      intro heq
      cases input with
      | nil => exact hl heq rfl
      | cons escaped tail => exact hb escaped tail heq rfl
    exact .raw ⟨hq, hb', hraw⟩ (ih ht)

/-- Every declarative string body is recognized with the same decoding and remainder. -/
theorem stringBody_complete {cs : List Nat} {out : List Nat × List Nat}
    (h : StringBody cs out) : stringBody cs = .ok out := by
  induction h with
  | close rest => rfl
  | raw hraw _ ih => simp [stringBody, hraw.1, hraw.2.1, hraw.2.2, ih, stringPrepend]
  | escape he _ ih => simp [stringBody, decodeEscape_complete he, ih, stringPrepend]

/-- String-body decompositions are unique. -/
theorem stringBody_unique {cs : List Nat} {out₁ out₂ : List Nat × List Nat}
    (h₁ : StringBody cs out₁) (h₂ : StringBody cs out₂) : out₁ = out₂ := by
  have h := (stringBody_complete h₁).symm.trans (stringBody_complete h₂)
  exact Except.ok.inj h

/-- Decode exactly the six byte escapes decided by the owner on 2026-11-08. -/
def decodeByteEscape : Nat → Option Nat
  | 110 => some 10
  | 116 => some 9
  | 114 => some 13
  | 92 => some 92
  | 39 => some 39
  | 48 => some 0
  | _ => none

/-- Every scanner-approved byte escape is in the decided relation. -/
theorem decodeByteEscape_sound {escaped decoded : Nat}
    (h : decodeByteEscape escaped = some decoded) : ByteEscape escaped decoded := by
  unfold decodeByteEscape at h
  split at h <;> simp_all [ByteEscape]

/-- Every decided byte escape is decoded to its specified value. -/
theorem decodeByteEscape_complete {escaped decoded : Nat} (h : ByteEscape escaped decoded) :
    decodeByteEscape escaped = some decoded := by
  rcases h with (⟨rfl, rfl⟩ | ⟨rfl, rfl⟩ | ⟨rfl, rfl⟩ | ⟨rfl, rfl⟩ | ⟨rfl, rfl⟩ | ⟨rfl, rfl⟩)
  all_goals rfl

/-- Internal byte-literal refusals; no diagnostic wording is fixed. -/
inductive ByteError where
  | invalidRaw (c : Nat) | invalidEscape (escaped : Nat) | unterminated
  deriving DecidableEq, Repr

/-- Scan exactly the decided byte literal. Other quote-led inputs are left for labels. -/
def byteLiteral : List Nat → Except ByteError (Option (List Nat × Nat × List Nat))
  | 39 :: 92 :: escaped :: 39 :: rest =>
    match decodeByteEscape escaped with
    | some decoded => .ok (some ([39, 92, escaped, 39], decoded, rest))
    | none => .error (.invalidEscape escaped)
  | 39 :: 92 :: escaped :: _ =>
    match decodeByteEscape escaped with
    | some _ => .error .unterminated
    | none => .error (.invalidEscape escaped)
  | 39 :: 92 :: [] => .error .unterminated
  | 39 :: c :: 39 :: rest =>
    if ByteRaw c then .ok (some ([39, c, 39], c, rest)) else .error (.invalidRaw c)
  | _ => .ok none

/-- Successful byte scanning satisfies the independent literal judgment. -/
theorem byteLiteral_sound {cs : List Nat} {out : List Nat × Nat × List Nat}
    (h : byteLiteral cs = .ok (some out)) : ByteLiteral cs out := by
  unfold byteLiteral at h
  split at h
  · split at h
    · rename_i decoded hdecode
      cases Except.ok.inj h
      apply ByteLiteral.escaped (decodeByteEscape_sound hdecode)
      have he := decodeByteEscape_sound hdecode
      rcases he with (⟨rfl, rfl⟩ | ⟨rfl, rfl⟩ | ⟨rfl, rfl⟩ | ⟨rfl, rfl⟩ | ⟨rfl, rfl⟩ |
        ⟨rfl, rfl⟩) <;> decide
    · cases h
  · split at h <;> cases h
  · cases h
  · split at h
    · cases Except.ok.inj h
      exact .raw (by assumption)
    · cases h
  · cases h

/-- Every declarative byte literal is recognized with the same value and remainder. -/
theorem byteLiteral_complete {cs : List Nat} {out : List Nat × Nat × List Nat}
    (h : ByteLiteral cs out) : byteLiteral cs = .ok (some out) := by
  cases h with
  | raw hraw => simp [byteLiteral, hraw, hraw.2.2.2]
  | escaped he _ => simp [byteLiteral, decodeByteEscape_complete he]

/-- Byte literal decompositions, including decoded values, are unique. -/
theorem byteLiteral_unique {cs : List Nat} {out₁ out₂ : List Nat × Nat × List Nat}
    (h₁ : ByteLiteral cs out₁) (h₂ : ByteLiteral cs out₂) : out₁ = out₂ := by
  exact Option.some.inj (Except.ok.inj
    ((byteLiteral_complete h₁).symm.trans (byteLiteral_complete h₂)))

/-- Only inputs lacking both an escape opener and a one-scalar closing quote can be
left for another token class. In particular, escape-led inputs can never be labels. -/
theorem byteLiteral_none_iff {cs : List Nat} :
    byteLiteral cs = .ok none ↔
      (∀ tail, cs ≠ 39 :: 92 :: tail) ∧
      (∀ c rest, cs ≠ 39 :: c :: 39 :: rest) := by
  unfold byteLiteral
  split
  · rename_i e rest
    cases decodeByteEscape e <;> simp
  · rename_i e tail _
    cases decodeByteEscape e <;> simp
  · simp
  · rename_i c rest _
    split <;> simp
  · rename_i hescape hbad hshort hraw
    simp only [true_iff]
    constructor
    · intro tail heq
      subst cs
      cases tail with
      | nil => exact hshort rfl
      | cons e rest => exact hbad e rest rfl
    · intro c rest heq
      exact hraw c rest heq

example : byteLiteral [39,92,39] = .error .unterminated := by rfl
example : byteLiteral [39,92,110] = .error .unterminated := by rfl
example : byteLiteral [39,92] = .error .unterminated := by rfl
example : byteLiteral [39,92,110,97,39] = .error .unterminated := by rfl

/-- Scan quote followed by an identifier as a label. -/
def label : List Nat → Option (List Nat × List Nat)
  | 39 :: input =>
    match identifier input with
    | some (name, rest) => some (39 :: name, rest)
    | none => none
  | _ => none

/-- Successful label scanning satisfies the independent judgment. -/
theorem label_sound {cs : List Nat} {out : List Nat × List Nat}
    (h : label cs = some out) : Label cs out := by
  unfold label at h
  split at h
  · split at h
    · rename_i name rest hid
      cases Option.some.inj h
      exact .intro (identifier_sound hid)
    · cases h
  · cases h

/-- Every declarative label is recognized with the same lexeme and remainder. -/
theorem label_complete {cs : List Nat} {out : List Nat × List Nat}
    (h : Label cs out) : label cs = some out := by
  cases h with
  | intro hi => simp [label, identifier_complete hi]

/-- Label decompositions are unique. -/
theorem label_unique {cs : List Nat} {out₁ out₂ : List Nat × List Nat}
    (h₁ : Label cs out₁) (h₂ : Label cs out₂) : out₁ = out₂ := by
  have h := (label_complete h₁).symm.trans (label_complete h₂)
  exact Option.some.inj h

/-- Strip a token from the front of an input, or report that it is not a prefix. -/
def prefixRest : List Nat → List Nat → Option (List Nat)
  | [], input => some input
  | _ :: _, [] => none
  | a :: token, b :: input => if a = b then prefixRest token input else none

/-- Successful prefix stripping gives the corresponding concatenation. -/
theorem prefixRest_sound {token input rest : List Nat} (h : prefixRest token input = some rest) :
    input = token ++ rest := by
  induction token generalizing input with
  | nil =>
    cases Option.some.inj h
    rfl
  | cons a token ih =>
    cases input with
    | nil => simp [prefixRest] at h
    | cons b input =>
      by_cases hab : a = b
      · subst b
        simp only [prefixRest, ↓reduceIte] at h
        simp only [List.cons_append]
        rw [ih h]
      · simp [prefixRest, hab] at h

/-- Every concatenated prefix is stripped to exactly its suffix. -/
theorem prefixRest_complete {token input rest : List Nat} (h : input = token ++ rest) :
    prefixRest token input = some rest := by
  subst input
  induction token with
  | nil => rfl
  | cons a token ih => simp [prefixRest, ih]

/-- Scan an arbitrary table for a longest nonempty prefix. Ties choose the earlier entry, but
any equal-length prefixes of one input are the same scalar list. -/
def longestMatchAux (input : List Nat) : List (List Nat) → Option (List Nat × List Nat)
  | [] => none
  | token :: table =>
    match prefixRest token input with
    | none => longestMatchAux input table
    | some rest =>
      if token = [] then longestMatchAux input table
      else
        match longestMatchAux input table with
        | none => some (token, rest)
        | some (other, otherRest) =>
          if other.length < token.length then some (token, rest) else some (other, otherRest)

/-- Longest match over the supplied abstract table; this is not a Concrete token set. -/
def longestMatch (table : List (List Nat)) (input : List Nat) : Option (List Nat × List Nat) :=
  longestMatchAux input table

/-- A tail-table longest match remains longest when the head cannot be a nonempty match. -/
theorem longestMatch_cons_of_no_match {token : List Nat} {table : List (List Nat)}
    {input : List Nat} {out : Option (List Nat × List Nat)}
    (h : LongestMatch table input out)
    (hhead : ∀ rest, token ≠ [] → input ≠ token ++ rest) :
    LongestMatch (token :: table) input out := by
  cases h with
  | none hnone =>
    apply LongestMatch.none
    intro other hother hne rest hprefix
    rcases List.mem_cons.mp hother with rfl | hmem
    · exact hhead rest hne hprefix
    · exact hnone other hmem hne rest hprefix
  | @some other otherRest hmem hne hprefix hmax =>
    apply LongestMatch.some (List.mem_cons_of_mem _ hmem) hne hprefix
    intro candidate hcandidate tail htail
    rcases List.mem_cons.mp hcandidate with rfl | htailMem
    · by_cases hempty : candidate = []
      · subst candidate; simp
      · exact (hhead tail hempty htail).elim
    · exact hmax candidate htailMem tail htail

/-- The table scanner always returns a declarative longest match, including `none`. -/
theorem longestMatchAux_sound (table : List (List Nat)) (input : List Nat) :
    LongestMatch table input (longestMatchAux input table) := by
  induction table with
  | nil =>
    apply LongestMatch.none
    intro token h
    cases h
  | cons token table ih =>
    cases hp : prefixRest token input with
    | none =>
      simp only [longestMatchAux, hp]
      apply longestMatch_cons_of_no_match ih
      intro rest _ hprefix
      rw [prefixRest_complete hprefix] at hp
      cases hp
    | some rest =>
      have hprefix := prefixRest_sound hp
      by_cases hne : token = []
      · subst token
        simp only [longestMatchAux, hp, ↓reduceIte]
        exact longestMatch_cons_of_no_match ih (fun _ h => (h rfl).elim)
      · simp only [longestMatchAux, hp, hne, ↓reduceIte]
        cases htail : longestMatchAux input table with
        | none =>
          rw [htail] at ih <;> simp only
          cases ih with
          | none hnone =>
            apply LongestMatch.some (by simp) hne hprefix
            intro other hother tail htailPrefix
            rcases List.mem_cons.mp hother with rfl | hmem
            · exact Nat.le_refl _
            · by_cases hempty : other = []
              · subst other; simp
              · exact (hnone other hmem hempty tail htailPrefix).elim
        | some out =>
          cases out with
          | mk other otherRest =>
            rw [htail] at ih <;> simp only
            cases ih with
            | some hmem hneOther hprefixOther hmax =>
              by_cases hlt : other.length < token.length
              · simp only [hlt, ↓reduceIte]
                apply LongestMatch.some (by simp) hne hprefix
                intro candidate hcandidate tail htailPrefix
                rcases List.mem_cons.mp hcandidate with rfl | hcandidateMem
                · exact Nat.le_refl _
                · exact Nat.le_trans (hmax candidate hcandidateMem tail htailPrefix)
                    (Nat.le_of_lt hlt)
              · simp only [hlt, ↓reduceIte]
                apply LongestMatch.some (List.mem_cons_of_mem _ hmem) hneOther hprefixOther
                intro candidate hcandidate tail htailPrefix
                rcases List.mem_cons.mp hcandidate with rfl | hcandidateMem
                · exact Nat.le_of_not_gt hlt
                · exact hmax candidate hcandidateMem tail htailPrefix

/-- The public longest-match scanner satisfies the independent judgment. -/
theorem longestMatch_sound (table : List (List Nat)) (input : List Nat) :
    LongestMatch table input (longestMatch table input) :=
  longestMatchAux_sound table input

/-- Longest-match outcomes are unique, including the remainder and the no-match outcome. -/
theorem longestMatch_unique {table : List (List Nat)} {input : List Nat}
    {out₁ out₂ : Option (List Nat × List Nat)}
    (h₁ : LongestMatch table input out₁) (h₂ : LongestMatch table input out₂) : out₁ = out₂ := by
  cases h₁ with
  | none hnone =>
    cases h₂ with
    | none => rfl
    | @some token rest hmem hne hprefix _ => exact (hnone token hmem hne rest hprefix).elim
  | @some token rest hmem hne hprefix hmax =>
    cases h₂ with
    | none hnone => exact (hnone token hmem hne rest hprefix).elim
    | @some other otherRest hmemOther _ hprefixOther hmaxOther =>
      have hlen : token.length = other.length :=
        Nat.le_antisymm (hmaxOther token hmem rest hprefix)
          (hmax other hmemOther otherRest hprefixOther)
      have htoken : token = other := by
        calc
          token = (token ++ rest).take token.length := by simp
          _ = input.take token.length := by rw [← hprefix]
          _ = input.take other.length := by rw [hlen]
          _ = (other ++ otherRest).take other.length := by rw [hprefixOther]
          _ = other := by simp
      subst other
      have hrest : rest = otherRest := by
        calc
          rest = (token ++ rest).drop token.length := by simp
          _ = input.drop token.length := by rw [← hprefix]
          _ = (token ++ otherRest).drop token.length := by rw [hprefixOther]
          _ = otherRest := by simp
      subst otherRest
      rfl

/-- Every declarative longest match is exactly the scanner's result. -/
theorem longestMatch_complete {table : List (List Nat)} {input : List Nat}
    {out : Option (List Nat × List Nat)} (h : LongestMatch table input out) :
    longestMatch table input = out :=
  longestMatch_unique (longestMatch_sound table input) h

/-- Scan the maximal valid digit run for a base. -/
def scanDigits (radix : Radix) : List Nat → List Nat × List Nat
  | [] => ([], [])
  | c :: cs =>
    if BaseDigit radix c then let (digits, rest) := scanDigits radix cs; (c :: digits, rest)
    else ([], c :: cs)

/-- The digit scanner satisfies the independent maximal-run judgment. -/
theorem scanDigits_sound (radix : Radix) (cs : List Nat) :
    DigitRun radix cs (scanDigits radix cs).1 (scanDigits radix cs).2 := by
  induction cs with
  | nil => exact .stop [] (by intro c cs h; cases h)
  | cons c cs ih =>
    by_cases hc : BaseDigit radix c
    · simpa [scanDigits, hc] using DigitRun.cons hc ih
    · simpa [scanDigits, hc] using
        DigitRun.stop (c :: cs) (by intro d ds h; cases h; exact hc)

/-- Every declarative digit decomposition is the scanner's result. -/
theorem scanDigits_complete {radix : Radix} {cs digits rest : List Nat}
    (h : DigitRun radix cs digits rest) : scanDigits radix cs = (digits, rest) := by
  induction h with
  | stop rest hc =>
    cases rest with
    | nil => rfl
    | cons c cs => simp [scanDigits, hc c cs rfl]
  | cons hc _ ih => simp [scanDigits, hc, ih]

/-- Decode the empty suffix or exactly one of the eight integer-type suffixes. -/
def decodeSuffix (suffix : List Nat) : Option (Option ConcreteSpec.IntTy) :=
  if suffix = [] then some none
  else if suffix = intTySpelling .i8 then some (some .i8)
  else if suffix = intTySpelling .i16 then some (some .i16)
  else if suffix = intTySpelling .i32 then some (some .i32)
  else if suffix = intTySpelling .i64 then some (some .i64)
  else if suffix = intTySpelling .u8 then some (some .u8)
  else if suffix = intTySpelling .u16 then some (some .u16)
  else if suffix = intTySpelling .u32 then some (some .u32)
  else if suffix = intTySpelling .u64 then some (some .u64)
  else none

/-- Every scanner-approved suffix is in the declarative suffix relation. -/
theorem decodeSuffix_sound {suffix : List Nat} {typeSuffix : Option ConcreteSpec.IntTy}
    (h : decodeSuffix suffix = some typeSuffix) : SuffixAccepted suffix typeSuffix := by
  unfold decodeSuffix at h
  repeat first | split at h | (solve | simp_all [SuffixAccepted])

/-- Every declarative suffix is decoded to its specified optional type. -/
theorem decodeSuffix_complete {suffix : List Nat} {typeSuffix : Option ConcreteSpec.IntTy}
    (h : SuffixAccepted suffix typeSuffix) : decodeSuffix suffix = some typeSuffix := by
  rcases h with (⟨rfl, rfl⟩ | ⟨rfl, rfl⟩ | ⟨rfl, rfl⟩ | ⟨rfl, rfl⟩ | ⟨rfl, rfl⟩ |
    ⟨rfl, rfl⟩ | ⟨rfl, rfl⟩ | ⟨rfl, rfl⟩ | ⟨rfl, rfl⟩) <;> decide

/-- Internal integer-literal refusals; constructor names are not user-visible diagnostics. -/
inductive IntegerError where
  | missingDigits (radix : Radix) | leadingZero | invalidSuffix (suffix : List Nat)
  deriving DecidableEq, Repr

private def finishIntegerDigits (radix : Radix) (digitOut : List Nat × List Nat) :
    Except IntegerError (Option IntegerToken) :=
  let suffixOut := scanIdentBody digitOut.2
  match digitOut.1 with
  | [] => if radix = .decimal then .ok none else .error (.missingDigits radix)
  | digits =>
    if DigitsAccepted radix digits then
      match decodeSuffix suffixOut.1 with
      | some typeSuffix =>
        .ok (some ⟨radix.basePrefix ++ digits ++ suffixOut.1,
          positionalValue radix digits, typeSuffix, suffixOut.2⟩)
      | none => .error (.invalidSuffix suffixOut.1)
    else .error .leadingZero

/-- Unbounded reference scanner for the literal correspondence proofs. -/
def integerDigits (radix : Radix) (input : List Nat) : Except IntegerError (Option IntegerToken) :=
  finishIntegerDigits radix (scanDigits radix input)

/-- Select exactly a lowercase non-decimal prefix, or decimal with no consumed prefix. -/
def splitRadix : List Nat → Radix × List Nat
  | 48 :: 98 :: rest => (.binary, rest)
  | 48 :: 111 :: rest => (.octal, rest)
  | 48 :: 120 :: rest => (.hexadecimal, rest)
  | input => (.decimal, input)

/-- The selected radix and digit input reassemble the original input. -/
theorem splitRadix_spec (cs : List Nat) :
    cs = (splitRadix cs).1.basePrefix ++ (splitRadix cs).2 := by
  unfold splitRadix
  split <;> rfl

/-- Scan an integer, leaving nonnumeric input for other token scanners. -/
def integerLiteral (cs : List Nat) : Except IntegerError (Option IntegerToken) :=
  integerDigits (splitRadix cs).1 (splitRadix cs).2

private theorem integerDigits_witness {radix : Radix} {input : List Nat} {out : IntegerToken}
    (h : integerDigits radix input = .ok (some out)) :
    ∃ digits suffixInput suffix,
      DigitRun radix input digits suffixInput ∧ DigitsAccepted radix digits ∧
      IdentBody suffixInput suffix out.rest ∧ SuffixAccepted suffix out.suffix ∧
      out.lexeme = radix.basePrefix ++ digits ++ suffix ∧
      out.value = positionalValue radix digits := by
  unfold integerDigits finishIntegerDigits at h
  generalize hd : scanDigits radix input = digitOut at h
  cases digitOut with
  | mk digits suffixInput =>
    generalize hi : scanIdentBody suffixInput = suffixOut at h
    cases suffixOut with
    | mk suffix rest =>
      have hrun : DigitRun radix input digits suffixInput := by
        have hs := scanDigits_sound radix input
        rw [hd] at hs
        exact hs
      have hident : IdentBody suffixInput suffix rest := by
        have hs := scanIdentBody_sound suffixInput
        rw [hi] at hs
        exact hs
      cases digits with
      | nil =>
        by_cases hr : radix = .decimal
        · subst radix; simp at h
        · simp [hr] at h
      | cons d ds =>
        by_cases hacc : DigitsAccepted radix (d :: ds)
        · simp only [hacc, ↓reduceIte] at h
          cases hs : decodeSuffix suffix with
          | none => simp [hs] at h
          | some typeSuffix =>
            simp only [hs] at h
            cases Except.ok.inj h
            exact ⟨_, _, _, hrun, hacc, hident, decodeSuffix_sound hs, rfl, rfl⟩
        · simp [hacc] at h

/-- Successful based-digit scanning satisfies the independent integer judgment. -/
theorem integerDigits_sound {radix : Radix} {input : List Nat} {out : IntegerToken}
    (h : integerDigits radix input = .ok (some out)) :
    IntegerLiteral (radix.basePrefix ++ input) out := by
  obtain ⟨digits, suffixInput, suffix, hrun, hacc, hident, hsuffix, hlexeme, hvalue⟩ :=
    integerDigits_witness h
  cases out
  subst_vars
  exact .intro rfl hrun hacc hident hsuffix

/-- Every declarative based-digit decomposition is recognized by its radix scanner. -/
theorem integerDigits_complete {radix : Radix} {input digits suffixInput suffix rest : List Nat}
    {typeSuffix : Option ConcreteSpec.IntTy}
    (hrun : DigitRun radix input digits suffixInput) (hacc : DigitsAccepted radix digits)
    (hident : IdentBody suffixInput suffix rest) (hsuffix : SuffixAccepted suffix typeSuffix) :
    integerDigits radix input = .ok (some ⟨radix.basePrefix ++ digits ++ suffix,
      positionalValue radix digits, typeSuffix, rest⟩) := by
  simp only [integerDigits, finishIntegerDigits, scanDigits_complete hrun, scanIdentBody_complete hident]
  cases digits with
  | nil => simp [DigitsAccepted] at hacc
  | cons d ds => simp only [hacc, ↓reduceIte, decodeSuffix_complete hsuffix]

/-- Successful integer scanning satisfies the independent judgment. -/
theorem integerLiteral_sound {cs : List Nat} {out : IntegerToken}
    (h : integerLiteral cs = .ok (some out)) : IntegerLiteral cs out := by
  unfold integerLiteral at h
  generalize hs : splitRadix cs = split at h
  cases split with
  | mk radix input =>
    have hspec := splitRadix_spec cs
    rw [hs] at hspec
    rw [hspec]
    exact integerDigits_sound h

/-- A decimal judgment cannot have a lowercase base prefix: its suffix candidate begins with
that prefix's letter, which none of the eight accepted suffixes starts with. -/
theorem decimal_prefixed_refused {c : Nat} {tail digits suffixInput suffix rest : List Nat}
    {typeSuffix : Option ConcreteSpec.IntTy} (hdigit : ¬ BaseDigit .decimal c)
    (hcontinue : IdentContinue c) (hci : c ≠ 105) (hcu : c ≠ 117)
    (hrun : DigitRun .decimal (48 :: c :: tail) digits suffixInput)
    (hident : IdentBody suffixInput suffix rest) (hsuffix : SuffixAccepted suffix typeSuffix) :
    False := by
  have hd := scanDigits_complete hrun
  simp [scanDigits, hdigit] at hd
  cases hd
  have hi := scanIdentBody_complete hident
  have hfirst : scanIdentBody (c :: tail) =
      (c :: (scanIdentBody tail).1, (scanIdentBody tail).2) := by
    simp [scanIdentBody, hcontinue]
  rw [hfirst] at hi
  cases hi
  rcases hsuffix with (⟨hs, _⟩ | ⟨hs, _⟩ | ⟨hs, _⟩ | ⟨hs, _⟩ | ⟨hs, _⟩ |
    ⟨hs, _⟩ | ⟨hs, _⟩ | ⟨hs, _⟩ | ⟨hs, _⟩) <;>
    simp [intTySpelling, hci, hcu] at hs

/-- Every declarative integer is recognized with the same token and remainder. -/
theorem integerLiteral_complete {cs : List Nat} {out : IntegerToken}
    (h : IntegerLiteral cs out) : integerLiteral cs = .ok (some out) := by
  cases h with
  | @intro radix input digitInput digits suffixInput suffix rest typeSuffix hinput
      hrun hacc hident hsuffix =>
    cases hinput
    cases radix with
    | binary =>
      simpa [integerLiteral, splitRadix, Radix.basePrefix] using
        (integerDigits_complete hrun hacc hident hsuffix)
    | octal =>
      simpa [integerLiteral, splitRadix, Radix.basePrefix] using
        (integerDigits_complete hrun hacc hident hsuffix)
    | hexadecimal =>
      simpa [integerLiteral, splitRadix, Radix.basePrefix] using
        (integerDigits_complete hrun hacc hident hsuffix)
    | decimal =>
      have hsplit : splitRadix digitInput = (.decimal, digitInput) := by
        cases digitInput with
        | nil => rfl
        | cons c tail =>
          by_cases hc : c = 48
          · subst c
            cases tail with
            | nil => rfl
            | cons d rest =>
              by_cases hb : d = 98
              · subst d
                exact (decimal_prefixed_refused (by decide) (by decide) (by decide) (by decide)
                  hrun hident hsuffix).elim
              · by_cases ho : d = 111
                · subst d
                  exact (decimal_prefixed_refused (by decide) (by decide) (by decide) (by decide)
                    hrun hident hsuffix).elim
                · by_cases hx : d = 120
                  · subst d
                    exact (decimal_prefixed_refused (by decide) (by decide) (by decide) (by decide)
                      hrun hident hsuffix).elim
                  · simp [splitRadix, hb, ho, hx]
          · simp [splitRadix, hc]
      simpa [integerLiteral, hsplit, Radix.basePrefix] using
        (integerDigits_complete hrun hacc hident hsuffix)

/-- Integer-literal decompositions are unique. -/
theorem integerLiteral_unique {cs : List Nat} {out₁ out₂ : IntegerToken}
    (h₁ : IntegerLiteral cs out₁) (h₂ : IntegerLiteral cs out₂) : out₁ = out₂ := by
  have h := (integerLiteral_complete h₁).symm.trans (integerLiteral_complete h₂)
  exact Option.some.inj (Except.ok.inj h)

/-! ## Kernel-checked conformance cases -/


/-- Equality of `Except` results for kernel-evaluated conformance cases. -/
local instance {error ok : Type} [DecidableEq error] [DecidableEq ok] :
    DecidableEq (Except error ok) := by
  intro x y
  cases x with
  | error e => cases y with
    | error f => exact decidable_of_iff (e = f) ⟨congrArg Except.error, Except.error.inj⟩
    | ok _ => exact isFalse (by intro h; cases h)
  | ok cs => cases y with
    | error _ => exact isFalse (by intro h; cases h)
    | ok ds => exact decidable_of_iff (cs = ds) ⟨congrArg Except.ok, Except.ok.inj⟩

example : decodeUtf8 [65, 95, 57] = .ok [65, 95, 57] := by decide
example : decodeUtf8 [0xC2, 0xA2] = .ok [0xA2] := by decide
example : decodeUtf8 [0xE2, 0x82, 0xAC] = .ok [0x20AC] := by decide
example : decodeUtf8 [0xF0, 0x9F, 0x98, 0x80] = .ok [0x1F600] := by decide
example : decodeUtf8 [0xC0, 0x80] = .error ⟨0, .overlong⟩ := by decide
example : decodeUtf8 [0xED, 0xA0, 0x80] = .error ⟨1, .surrogate⟩ := by decide
example : decodeUtf8 [0xF4, 0x90, 0x80, 0x80] = .error ⟨1, .tooLarge⟩ := by decide
example : decodeUtf8 [0xE2, 0x82] = .error ⟨2, .truncated⟩ := by decide
example : decodeUtf8 [0x80] = .error ⟨0, .unexpectedContinuation⟩ := by decide
example : checkSource [0] = .error ⟨0, .nul⟩ := by decide
example : checkSource [0xEF, 0xBB, 0xBF] = .error ⟨0, .bom⟩ := by decide
example : checkSource [65, 0] = .error ⟨1, .nul⟩ := by decide
example : checkSource [65, 0xEF, 0xBB, 0xBF] = .error ⟨1, .bom⟩ := by decide
example : checkSource [0, 0x80] = .error ⟨0, .nul⟩ := by decide
example : checkSource [0xC2, 0xA2, 0] = .error ⟨2, .nul⟩ := by decide
example : checkSource [65] = .ok [65] := by decide
example : identStart? 0xE9 = false ∧ identContinue? 0xE9 = false := by decide

example : lineComment [47,47,32,120,10,114,101,115,116] =
    .ok (some [10,114,101,115,116]) := by decide
example : lineComment [47,47,32,120,13] = .ok (some [13]) := by decide
example : lineComment [47,47,32,120] = .ok (some []) := by decide
example : lineComment [47,47,9,120,10] = .ok (some [10]) := by decide
example : lineComment [47,42,32,120,32,42,47] = .ok none := by decide

example : lineComment [47,47,0] = .error (.rawForbidden 0) := by decide
example : lineComment [47,47,8] = .error (.rawForbidden 8) := by decide
example : lineComment [47,47,11] = .error (.rawForbidden 11) := by decide
example : lineComment [47,47,31] = .error (.rawForbidden 31) := by decide
example : lineComment [47,47,127] = .error (.rawForbidden 127) := by decide
example : lineComment [47,47,159] = .error (.rawForbidden 159) := by decide
example : lineComment [47,47,0x2028] = .error (.rawForbidden 0x2028) := by decide
example : lineComment [47,47,0x2029] = .error (.rawForbidden 0x2029) := by decide
example : lineComment [47,47,0x061C] = .error (.rawForbidden 0x061C) := by decide
example : lineComment [47,47,0x200E] = .error (.rawForbidden 0x200E) := by decide
example : lineComment [47,47,0x200F] = .error (.rawForbidden 0x200F) := by decide
example : lineComment [47,47,0x202A] = .error (.rawForbidden 0x202A) := by decide
example : lineComment [47,47,0x202E] = .error (.rawForbidden 0x202E) := by decide
example : lineComment [47,47,0x2066] = .error (.rawForbidden 0x2066) := by decide
example : lineComment [47,47,0x2069] = .error (.rawForbidden 0x2069) := by decide
example :
    CommentRaw 9 ∧ CommentRaw 32 ∧ CommentRaw 126 ∧ CommentRaw 160 ∧
    CommentRaw 0x061B ∧ CommentRaw 0x061D ∧ CommentRaw 0x200D ∧ CommentRaw 0x2010 ∧
    CommentRaw 0x2027 ∧ CommentRaw 0x202F ∧ CommentRaw 0x2065 ∧ CommentRaw 0x206A := by decide

example : skipTrivia [32,65] = .ok [65] := by decide
example : skipTrivia [9,65] = .ok [65] := by decide
example : skipTrivia [10,65] = .ok [65] := by decide
example : skipTrivia [13,65] = .ok [65] := by decide
example : skipTrivia [13,10,65] = .ok [65] := by decide
example : skipTrivia [47,47,120,10,65] = .ok [65] := by decide
example : skipTrivia [47,47,9,120,10,65] = .ok [65] := by decide
example : skipTrivia [47,47,120,13,65] = .ok [65] := by decide
example : skipTrivia [47,47,120] = .ok [] := by decide
example : skipTrivia [47,47,0] = .error (.rawForbidden 0) := by decide
example : skipTrivia [9,32,47,47,120,13,10,65] =
    .ok [65] := by decide

example : skipTrivia [47,42,32,42,47] = .ok [47,42,32,42,47] := by decide
example : skipTrivia [35,33,65] = .ok [35,33,65] := by decide
example : skipTrivia [0xA0,32] = .ok [0xA0,32] := by decide
example : skipTrivia [0xFEFF,32] = .ok [0xFEFF,32] := by decide
example : skipTrivia [47] = .ok [47] := by decide
example : skipTrivia [65,32] = .ok [65,32] := by decide

example : identifier [65,98,99,33] = some ([65,98,99], [33]) := by decide
example : identifier [97,98,99] = some ([97,98,99], []) := by decide
example : identifier [95,48,57] = some ([95,48,57], []) := by decide
example : ([65,98,99] : List Nat) ≠ [97,98,99] := by decide
example : identifier [48,98,99] = none := by decide
example : identifier [0xE9,65] = none := by decide

example : stringBody [92,110,34,65] = .ok ([10], [65]) := by decide
example : stringBody [92,110,92,116,92,114,92,92,92,34,92,48,34,65] =
    .ok ([10,9,13,92,34,0], [65]) := by decide
example : stringBody [92,113,34] = .error (.invalidEscape 113) := by decide
example : stringBody [] = .error .unterminated := by rfl
example : stringBody [92,110] = .error .unterminated := by rfl
example : stringBody [92] = .error .unterminated := by rfl
example : stringBody [32,126,160,34] = .ok ([32,126,160], []) := by decide

example :
    stringBody [0,34] = .error (.rawForbidden 0) ∧
    stringBody [9,34] = .error (.rawForbidden 9) ∧
    stringBody [10,34] = .error (.rawForbidden 10) ∧
    stringBody [13,34] = .error (.rawForbidden 13) ∧
    stringBody [31,34] = .error (.rawForbidden 31) ∧
    stringBody [127,34] = .error (.rawForbidden 127) ∧
    stringBody [159,34] = .error (.rawForbidden 159) := by decide
example :
    stringBody [0x2028,34] = .error (.rawForbidden 0x2028) ∧
    stringBody [0x2029,34] = .error (.rawForbidden 0x2029) ∧
    stringBody [0x061C,34] = .error (.rawForbidden 0x061C) ∧
    stringBody [0x200E,34] = .error (.rawForbidden 0x200E) ∧
    stringBody [0x200F,34] = .error (.rawForbidden 0x200F) ∧
    stringBody [0x202A,34] = .error (.rawForbidden 0x202A) ∧
    stringBody [0x202E,34] = .error (.rawForbidden 0x202E) ∧
    stringBody [0x2066,34] = .error (.rawForbidden 0x2066) ∧
    stringBody [0x2069,34] = .error (.rawForbidden 0x2069) := by decide
example :
    StringRaw 32 ∧ StringRaw 126 ∧ StringRaw 160 ∧
    StringRaw 0x061B ∧ StringRaw 0x061D ∧ StringRaw 0x200D ∧ StringRaw 0x2010 ∧
    StringRaw 0x2027 ∧ StringRaw 0x202F ∧ StringRaw 0x2065 ∧ StringRaw 0x206A := by decide

example : byteLiteral [39,97,39,65] = .ok (some ([39,97,39], 97, [65])) := by decide
example : byteLiteral [39,32,39] = .ok (some ([39,32,39], 32, [])) := by decide
example : byteLiteral [39,126,39] = .ok (some ([39,126,39], 126, [])) := by decide
example : byteLiteral [39,31,39] = .error (.invalidRaw 31) := by decide
example : byteLiteral [39,127,39] = .error (.invalidRaw 127) := by decide
example : byteLiteral [39,0xE9,39] = .error (.invalidRaw 0xE9) := by decide
example : ¬ ByteRaw 39 ∧ ¬ ByteRaw 92 := by decide
example : byteLiteral [39,92,110,39] = .ok (some ([39,92,110,39], 10, [])) := by decide
example : byteLiteral [39,92,116,39] = .ok (some ([39,92,116,39], 9, [])) := by decide
example : byteLiteral [39,92,114,39] = .ok (some ([39,92,114,39], 13, [])) := by decide
example : byteLiteral [39,92,48,39] = .ok (some ([39,92,48,39], 0, [])) := by decide
example : byteLiteral [39,92,92,39] = .ok (some ([39,92,92,39], 92, [])) := by decide
example : byteLiteral [39,92,39,39] = .ok (some ([39,92,39,39], 39, [])) := by decide
example : byteLiteral [39,92,34,39] = .error (.invalidEscape 34) := by decide
example : byteLiteral [39,92,120,52,49,39] = .error (.invalidEscape 120) := by decide
example : byteLiteral [39,97] = .ok none := by decide
example : byteLiteral [39,97,98] = .ok none := by decide
example : label [39,97] = some ([39,97], []) := by decide
example : label [39,97,98] = some ([39,97,98], []) := by decide

-- Arbitrary numeric lists only: this is deliberately not a Concrete token table.
example : longestMatch [[1], [1,2,3], [1,2], [9]] [1,2,4] = some ([1,2], [4]) := by decide
example : longestMatch [[1,2], [1,2]] [1,2,3] = some ([1,2], [3]) := by decide
example : longestMatch [[], [1,2], [3]] [1,9] = none := by decide
example : longestMatch [[1], []] [] = none := by decide

example : integerLiteral ("1i23".toList.map Char.toNat) =
    .error (.invalidSuffix ("i23".toList.map Char.toNat)) := by decide
example : integerLiteral ("1u23".toList.map Char.toNat) =
    .error (.invalidSuffix ("u23".toList.map Char.toNat)) := by decide
example : decodeSuffix ("i32".toList.map Char.toNat) = some (some .i32) := by decide
example : decodeSuffix ("u32".toList.map Char.toNat) = some (some .u32) := by decide
example : integerLiteral [48,32] = .ok (some ⟨[48], 0, none, [32]⟩) := by decide
example : integerLiteral [48,65] = .error (.invalidSuffix [65]) := by decide
example : integerLiteral [55] = .ok (some ⟨[55], 7, none, []⟩) := by decide
example : integerLiteral [50,48,48,32] = .ok (some ⟨[50,48,48], 200, none, [32]⟩) := by decide
example : integerLiteral [50,48,48,117,56] =
    .ok (some ⟨[50,48,48,117,56], 200, some .u8, []⟩) := by decide
example : integerLiteral [49,105,56] = .ok (some ⟨[49,105,56], 1, some .i8, []⟩) := by decide
example : integerLiteral [48,98,49,48,49] = .ok (some ⟨[48,98,49,48,49], 5, none, []⟩) := by decide
example : integerLiteral [48,111,49,55] = .ok (some ⟨[48,111,49,55], 15, none, []⟩) := by decide
example : integerLiteral [48,120,102,102] = .ok (some ⟨[48,120,102,102], 255, none, []⟩) := by decide
example : integerLiteral [48,120,49,102] = .ok (some ⟨[48,120,49,102], 31, none, []⟩) := by decide
example : integerLiteral [48,98,48,48] = .ok (some ⟨[48,98,48,48], 0, none, []⟩) := by decide
example : integerLiteral [48,111,48,55] = .ok (some ⟨[48,111,48,55], 7, none, []⟩) := by decide
example : integerLiteral [48,120,48,102] = .ok (some ⟨[48,120,48,102], 15, none, []⟩) := by decide

example : integerLiteral [48,98,49,48,49,105,49,54] =
    .ok (some ⟨[48,98,49,48,49,105,49,54], 5, some .i16, []⟩) := by decide
example : integerLiteral [48,111,49,55,105,51,50] =
    .ok (some ⟨[48,111,49,55,105,51,50], 15, some .i32, []⟩) := by decide
example : integerLiteral [48,120,102,102,105,54,52] =
    .ok (some ⟨[48,120,102,102,105,54,52], 255, some .i64, []⟩) := by decide
example : integerLiteral [48,98,49,117,49,54] =
    .ok (some ⟨[48,98,49,117,49,54], 1, some .u16, []⟩) := by decide
example : integerLiteral [48,111,55,117,51,50] =
    .ok (some ⟨[48,111,55,117,51,50], 7, some .u32, []⟩) := by decide
example : integerLiteral [48,120,102,117,54,52] =
    .ok (some ⟨[48,120,102,117,54,52], 15, some .u64, []⟩) := by decide
example : integerLiteral [48,120,102,102,117,56] =
    .ok (some ⟨[48,120,102,102,117,56], 255, some .u8, []⟩) := by decide

example : integerLiteral [48,48,55] = .error .leadingZero := by decide
example : integerLiteral [48,48] = .error .leadingZero := by decide
example : integerLiteral [48,120] = .error (.missingDigits .hexadecimal) := by decide
example : integerLiteral [48,98] = .error (.missingDigits .binary) := by decide
example : integerLiteral [48,111] = .error (.missingDigits .octal) := by decide
example : integerLiteral [65] = .ok none := by decide

example :
    integerLiteral [48,88,102,102] = .error (.invalidSuffix [88,102,102]) ∧
    integerLiteral [48,66,49] = .error (.invalidSuffix [66,49]) ∧
    integerLiteral [48,79,55] = .error (.invalidSuffix [79,55]) ∧
    integerLiteral [48,120,65] = .error (.missingDigits .hexadecimal) ∧
    integerLiteral [48,120,70] = .error (.missingDigits .hexadecimal) ∧
    integerLiteral [48,120,49,65] = .error (.invalidSuffix [65]) ∧
    integerLiteral [48,120,49,70] = .error (.invalidSuffix [70]) := by decide
example :
    integerLiteral [49,50,51,117,57] = .error (.invalidSuffix [117,57]) ∧
    integerLiteral [50,48,48,85,56] = .error (.invalidSuffix [85,56]) ∧
    integerLiteral [49,102,51,50] = .error (.invalidSuffix [102,51,50]) ∧
    integerLiteral [48,120,102,102,117,57] = .error (.invalidSuffix [117,57]) ∧
    integerLiteral [50,48,48,117,56,102,111,111] =
      .error (.invalidSuffix [117,56,102,111,111]) ∧
    integerLiteral [49,117,56,117,56] = .error (.invalidSuffix [117,56,117,56]) := by decide
example :
    integerLiteral [49,95,50] = .error (.invalidSuffix [95,50]) ∧
    integerLiteral [49,50,97,98,99] = .error (.invalidSuffix [97,98,99]) ∧
    integerLiteral [48,98,49,48,50] = .error (.invalidSuffix [50]) ∧
    integerLiteral [48,111,55,56] = .error (.invalidSuffix [56]) ∧
    integerLiteral [48,120,49,103] = .error (.invalidSuffix [103]) := by decide

example : decodeUtf8 [0xC2,65] = .error ⟨1, .invalidContinuation⟩ := by decide
example : decodeUtf8 [0xE2,0x82,65] = .error ⟨2, .invalidContinuation⟩ := by decide
example : decodeUtf8 [0xF0,0x90,0x80,65] = .error ⟨3, .invalidContinuation⟩ := by decide
example : decodeUtf8 [65,0x80] = .error ⟨1, .unexpectedContinuation⟩ := by decide
example : decodeUtf8 [0xC2] = .error ⟨1, .truncated⟩ := by decide
example : decodeUtf8 [0xE0,0x80] = .error ⟨1, .overlong⟩ := by decide

end Concrete.Lex

namespace Concrete.Lex
open ConcreteSpec.Surface0

/-- R-0528, lexer-policy revision 1: maximum digits per integer token, excluding
the radix prefix and type suffix. The first excess valid digit takes precedence over
a leading-zero or malformed-suffix error. Whole-source decoding still runs first. -/
def integerDigitLimit : Nat := 1024

/-- Stop at the first excess valid digit. No magnitude or suffix is computed here. -/
private def boundedDigits (radix : Radix) : Nat → List Nat → Option (List Nat × List Nat)
  | _, [] => some ([], [])
  | budget, c :: cs =>
    if BaseDigit radix c then
      match budget with
      | 0 => none
      | budget + 1 => (boundedDigits radix budget cs).map fun (digits, rest) => (c :: digits, rest)
    else some ([], c :: cs)

private theorem boundedDigits_spec (radix : Radix) (budget : Nat) (input : List Nat) :
    boundedDigits radix budget input =
      if (scanDigits radix input).1.length ≤ budget then some (scanDigits radix input) else none := by
  induction input generalizing budget with
  | nil => simp [boundedDigits, scanDigits]
  | cons c cs ih =>
    by_cases hc : BaseDigit radix c
    · cases budget with
      | zero =>
        simp only [boundedDigits, scanDigits, hc, ↓reduceIte]
        cases scanDigits radix cs <;> simp
      | succ budget =>
        simp only [boundedDigits, scanDigits, hc, ↓reduceIte, ih budget]
        cases scanDigits radix cs with
        | mk ds rest =>
          by_cases hn : ds.length ≤ budget
          · simp [hn]
          · simp [hn]
    · cases budget <;> simp [boundedDigits, scanDigits, hc]

private theorem boundedDigits_eq {radix : Radix} {budget : Nat} {input : List Nat}
    {result : List Nat × List Nat} (h : boundedDigits radix budget input = some result) :
    scanDigits radix input = result := by
  rw [boundedDigits_spec] at h
  split at h
  · exact Option.some.inj h
  · contradiction

/-- Decoded literals whose parser-facing terminals currently carry no payload. -/
inductive LosslessValue where
  | parsed (value : TokenValue)
  | stringLiteral (text : String)
  | byteLiteral (byte : UInt8)
  | label (name : String)
  deriving DecidableEq, Repr

def LosslessValue.parserValue : LosslessValue → TokenValue
  | .parsed value => value
  | .stringLiteral _ => .fixed .stringLiteral
  | .byteLiteral _ => .fixed .byteLiteral
  | .label _ => .fixed .label

/-- Source-order pieces. Each trivia run between tokens is trailing trivia of the
previous token and leading trivia of the next, with a single stored range. -/
inductive Piece where
  | token (value : LosslessValue) (span : Span)
  | whitespace (span : Span)
  | lineComment (span : Span)
  deriving DecidableEq, Repr

def Piece.span : Piece → Span
  | .token _ span | .whitespace span | .lineComment span => span

def Piece.parserToken : Piece → Option Token
  | .token value span => some ⟨value.parserValue, span⟩
  | _ => none

private def recordSpace (fileId : ConcreteSpec.Core.FileId) (start stop : Position) : List Piece → List Piece
  | .whitespace span :: rest => .whitespace { span with stop := stop.byte } :: rest
  | pieces => .whitespace (start.span stop fileId) :: pieces

private def extendComment (fileId : ConcreteSpec.Core.FileId) (start stop : Position) : List Piece → List Piece
  | .lineComment span :: rest => .lineComment { span with stop := stop.byte } :: rest
  | pieces => .lineComment (start.span stop fileId) :: pieces

private theorem recordSpace_view (fileId : ConcreteSpec.Core.FileId) (start stop : Position) (pieces : List Piece) :
    (recordSpace fileId start stop pieces).reverse.filterMap Piece.parserToken =
      pieces.reverse.filterMap Piece.parserToken := by
  cases pieces with
  | nil => rfl
  | cons piece rest => cases piece <;> simp [recordSpace, List.filterMap_cons, Piece.parserToken]

private theorem extendComment_view (fileId : ConcreteSpec.Core.FileId) (start stop : Position) (pieces : List Piece) :
    (extendComment fileId start stop pieces).reverse.filterMap Piece.parserToken =
      pieces.reverse.filterMap Piece.parserToken := by
  cases pieces with
  | nil => rfl
  | cons piece rest => cases piece <;> simp [extendComment, List.filterMap_cons, Piece.parserToken]

private def punct (input : List Nat) : Option (LosslessValue × List Nat × List Nat) := do
  let (lexeme, rest) ← longestMatch punctuationScalars input
  let (_, kind) ← punctuation.find? fun (s, _) => s == scalarText lexeme
  return (.parsed (.fixed kind), lexeme, rest)

private def nextToken (input : List Nat) (pos : Position) :
    Except Refusal (LosslessValue × List Nat × List Nat) := do
  if let some (name, rest) := identifier input then
    return (.parsed (wordValue (scalarText name)), name, rest)
  let (radix, digitInput) := splitRadix input
  let some digits := boundedDigits radix integerDigitLimit digitInput
    | throw (.lexerDigitLimit pos.byte integerDigitLimit)
  match finishIntegerDigits radix digits with
  | .ok (some i) => return (.parsed (.integer i.value radix i.suffix), i.lexeme, i.rest)
  | .error _ => throw (.syntaxError s!"malformed integer at byte {pos.byte}")
  | .ok none =>
    if let some p := punct input then return p
    if let 34 :: cs := input then
      match stringBody cs with
      | .ok (decoded, rest) =>
        return (.stringLiteral (scalarText decoded), input.take (input.length - rest.length), rest)
      | .error _ => throw (.syntaxError s!"malformed string at byte {pos.byte}")
    match byteLiteral input with
    | .error _ => throw (.syntaxError s!"malformed byte literal at byte {pos.byte}")
    | .ok (some (lexeme, decoded, rest)) => return (.byteLiteral (UInt8.ofNat decoded), lexeme, rest)
    | .ok none =>
      if let some (lexeme, rest) := label input then return (.label (scalarText lexeme.tail), lexeme, rest)
      else throw (.syntaxError s!"unexpected character at byte {pos.byte}")

-- The budget is one step per input scalar. Every successful step consumes at least one.
private def tokens (fileId : ConcreteSpec.Core.FileId) : Nat → List Nat → Position → Bool → List Piece → Except Refusal (List Piece)
  | _, [], _, _, acc => .ok acc.reverse
  | 0, _, _, _, _ => .error (.internal "lexer exhausted its progress bound")
  | fuel + 1, input@(c :: cs), pos, comment, acc => do
    if comment && c != 10 && c != 13 then
      if decide (CommentRaw c) then
        let stop := pos.advance c
        tokens fileId fuel cs stop true (extendComment fileId pos stop acc)
      else throw (.syntaxError s!"forbidden comment character at byte {pos.byte}")
    else if whitespace? c then
      let stop := pos.advance c
      tokens fileId fuel cs stop false (recordSpace fileId pos stop acc)
    else if let 47 :: 47 :: rest := input then
      let stop := (pos.advance 47).advance 47
      tokens fileId fuel rest stop true (.lineComment (pos.span stop fileId) :: acc)
    else
      let (value, lexeme, rest) ← nextToken input pos
      let stop := lexeme.foldl Position.advance pos
      tokens fileId fuel rest stop false (.token value (pos.span stop fileId) :: acc)

/-- Reassemble contiguous ranges in a single traversal of the original bytes. -/
private def replay : List Piece → List UInt8 → List UInt8 → List UInt8
  | [], _, acc => acc.reverse
  | piece :: rest, bytes, acc =>
    let width := piece.span.stop - piece.span.start
    let (raw, tail) := bytes.splitAt width
    replay rest tail (raw.reverseAux acc)

/-- Decoded payloads are tied to the independent scalar-level lexical judgments. -/
def LosslessValue.Payload (value : LosslessValue) (input spelling rest : List Nat) : Prop :=
  match value with
  | .parsed (.fixed kind) => kind ≠ .stringLiteral ∧ kind ≠ .byteLiteral ∧ kind ≠ .label
  | .parsed _ => True
  | .stringLiteral text => ∃ body decoded, input = 34 :: body ∧
      StringBody body (decoded, rest) ∧ text = scalarText decoded
  | .byteLiteral byte => ∃ decoded, ByteLiteral input (spelling, decoded, rest) ∧
      byte = UInt8.ofNat decoded
  | .label name => Label input (spelling, rest) ∧ name = scalarText spelling.tail

private theorem punct_sound {input lexeme rest : List Nat} {value : LosslessValue}
    (h : punct input = some (value, lexeme, rest)) :
    Tokenizes input lexeme rest value.parserValue ∧ value.Payload input lexeme rest := by
  cases hm : longestMatch punctuationScalars input with
  | none => simp [punct, hm] at h
  | some pair =>
    obtain ⟨spelling, tail⟩ := pair
    cases hf : punctuation.find? (fun (s, _) => s == scalarText spelling) with
    | none => simp [punct, hm, hf] at h
    | some entry =>
      obtain ⟨s, kind⟩ := entry
      have hmemb := List.mem_of_find?_eq_some hf
      have heq := List.find?_some hf
      simp at heq
      subst s
      simp [punct, hm, hf] at h
      obtain ⟨rfl, rfl, rfl⟩ := h
      refine ⟨.punct (hm ▸ longestMatch_sound punctuationScalars input) hmemb, ?_⟩
      have allowed : ∀ entry ∈ punctuation,
          entry.2 ≠ .stringLiteral ∧ entry.2 ≠ .byteLiteral ∧ entry.2 ≠ .label := by decide
      exact allowed _ hmemb

private theorem stringBody_suffix {input : List Nat} {out : List Nat × List Nat}
    (h : StringBody input out) : ∃ taken, input = taken ++ out.2 := by
  induction h with
  | close rest => exact ⟨[34], rfl⟩
  | raw _ _ ih =>
    obtain ⟨taken, rfl⟩ := ih
    exact ⟨_ :: taken, rfl⟩
  | escape _ _ ih =>
    obtain ⟨taken, rfl⟩ := ih
    exact ⟨92 :: _ :: taken, rfl⟩

private theorem nextToken_payload {input lexeme rest : List Nat} {pos : Position}
    {value : LosslessValue} (h : nextToken input pos = .ok (value, lexeme, rest)) :
    Tokenizes input lexeme rest value.parserValue ∧ value.Payload input lexeme rest := by
  unfold nextToken at h
  split at h
  · rename_i name tail hi
    cases Except.ok.inj h
    refine ⟨.word (identifier_sound hi), ?_⟩
    dsimp only [wordValue]
    by_cases hw : (word (scalarText lexeme) == .ident) = true
    · simp [hw, LosslessValue.Payload]
    · simp only [ite_eq_right hw, LosslessValue.Payload]
      generalize scalarText lexeme = wordText
      unfold word
      cases found : wordTokens.find? (fun entry => entry.1 == wordText) with
      | none => decide
      | some entry =>
        have allowed : ∀ entry ∈ wordTokens,
            entry.2 ≠ .stringLiteral ∧ entry.2 ≠ .byteLiteral ∧ entry.2 ≠ .label := by decide
        exact allowed entry (List.mem_of_find?_eq_some found)
  · cases hb : boundedDigits (splitRadix input).1 integerDigitLimit (splitRadix input).2 with
    | none => simp [hb] at h
    | some digitOut =>
      simp only [hb] at h
      split at h
      · rename_i i hi
        cases Except.ok.inj h
        have hi' : integerDigits (splitRadix input).1 (splitRadix input).2 = .ok (some i) := by
          simpa only [integerDigits, boundedDigits_eq hb] using hi
        obtain ⟨digits, suffixInput, suffix, hrun, hacc, hident, hsuffix, he, hv⟩ :=
          integerDigits_witness hi'
        rw [he, hv]
        refine ⟨?_, trivial⟩
        exact Eq.mpr (congrArg (fun src => Tokenizes src _ _ _) (splitRadix_spec input))
          (.integer hrun hacc hident hsuffix)
      · contradiction
      · split at h
        · rename_i p hp
          cases Except.ok.inj h
          exact punct_sound hp
        · split at h
          · rename_i body _ _ _
            cases hs : stringBody body with
            | error _ =>
              rw [hs] at h
              contradiction
            | ok pair =>
              obtain ⟨decoded, tail⟩ := pair
              simp [hs, pure, Except.pure] at h
              obtain ⟨rfl, rfl, rfl⟩ := h
              have hbody := stringBody_sound hs
              obtain ⟨spelling, hspell⟩ := stringBody_suffix hbody
              have he : 34 :: body = (34 :: spelling) ++ tail := by rw [hspell]; rfl
              have htake : (34 :: body).take ((34 :: body).length - tail.length) =
                  34 :: spelling := by rw [he, List.length_append, Nat.add_sub_cancel, List.take_left]
              rw [List.length_cons] at htake
              exact ⟨.string hbody (by rw [htake]; exact he), ⟨_, _, rfl, hbody, rfl⟩⟩
          ·
            split at h
            · contradiction
            · rename_i spelling decoded tail hb
              cases Except.ok.inj h
              exact ⟨.byte (byteLiteral_sound hb), ⟨_, byteLiteral_sound hb, rfl⟩⟩
            · rename_i hb
              split at h
              · rename_i spelling tail hl
                cases Except.ok.inj h
                refine ⟨.label (label_sound hl) ?_, ⟨label_sound hl, rfl⟩⟩
                intro bytes decoded tail hbyte
                rw [byteLiteral_complete hbyte] at hb
                simp at hb
              · contradiction

private theorem nextToken_sound {input lexeme rest : List Nat} {pos : Position}
    {value : LosslessValue} (h : nextToken input pos = .ok (value, lexeme, rest)) :
    Tokenizes input lexeme rest value.parserValue :=
  (nextToken_payload h).1

private theorem advance_width {c : Nat} {chunk : List UInt8}
    (h : Utf8.Encodes c chunk) (pos : Position) :
    (pos.advance c).byte = pos.byte + chunk.length := by
  have hw : (if c < 0x80 then 1 else if c < 0x800 then 2
      else if c < 0x10000 then 3 else 4) = chunk.length := by
    by_cases h1 : c < 0x80 <;> by_cases h2 : c < 0x800 <;> by_cases h3 : c < 0x10000
    all_goals
      simp only [h1, h2, h3, ite_false, ite_true]
      unfold Utf8.Encodes at h
      split at h <;> simp only [Utf8.one_iff, Utf8.two_iff, Utf8.three_iff, Utf8.four_iff] at h <;>
        simp only [List.length_cons, List.length_nil] <;> omega
  by_cases hcr : (c == 13) = true <;> by_cases hlf : (c == 10) = true <;>
    simp [Position.advance, ConcreteSpec.Core.Position.step, hw, hcr, hlf]

theorem decoded_width {bytes : List UInt8} {cs : List Nat}
    (h : Utf8.Decodes bytes cs) (pos : Position) :
    (cs.foldl Position.advance pos).byte = pos.byte + bytes.length := by
  induction h generalizing pos with
  | nil => simp
  | cons enc _ ih => simp [ih, advance_width enc, Nat.add_assoc]

private theorem identBody_split {input body rest : List Nat} (h : IdentBody input body rest) :
    input = body ++ rest := by
  induction h with
  | stop => rfl
  | cons _ _ ih => simp [ih]

private theorem token_split {input spelling rest : List Nat} {value : TokenValue}
    (h : Tokenizes input spelling rest value) : input = spelling ++ rest ∧ spelling ≠ [] := by
  cases h with
  | word h =>
    cases h with
    | intro _ hb => exact ⟨congrArg (_ :: ·) (identBody_split hb), by simp⟩
  | integer hrun hacc hbody _ =>
    have hr : ∀ {radix input digits tail}, DigitRun radix input digits tail →
        input = digits ++ tail := by
      intro radix input digits tail h
      induction h with
      | stop => rfl
      | cons _ _ ih => simp [ih]
    refine ⟨by rw [hr hrun, identBody_split hbody]; simp [List.append_assoc], ?_⟩
    rename_i radix digitInput digits suffixInput suffix rest ty
    cases digits <;> simp_all [DigitsAccepted]
  | string hb he =>
    refine ⟨he, ?_⟩
    intro hn
    obtain ⟨taken, htake⟩ := stringBody_suffix hb
    simp only [hn, List.nil_append] at he
    have lengths := congrArg List.length htake
    simp only [List.length_append, ← he, List.length_cons] at lengths
    omega
  | byte h => cases h <;> exact ⟨rfl, by simp⟩
  | label h _ =>
    cases h with
    | intro h => cases h with
      | intro _ hb => exact ⟨congrArg (39 :: _ :: ·) (identBody_split hb), by simp⟩
  | punct h _ => cases h with | some _ hn he _ => exact ⟨he, hn⟩

/-- The suffix is the actual remaining decoded input. An open comment is used
only while scanning; the public stream requires every piece to be closed. -/
inductive PieceMeaning (fileId : ConcreteSpec.Core.FileId) (start : Position) : Bool → List Nat → List Nat → Piece → Prop
  | whitespace {spelling rest} : spelling ≠ [] → (∀ c ∈ spelling, Whitespace c) →
      PieceMeaning fileId start false spelling rest
        (.whitespace (start.span (spelling.foldl Position.advance start) fileId))
  | comment {openComment body rest} :
      (∀ c ∈ body, c ≠ 10 ∧ c ≠ 13 ∧ CommentRaw c) →
      (openComment = false → LineComment (47 :: 47 :: (body ++ rest)) rest) →
      PieceMeaning fileId start openComment (47 :: 47 :: body) rest
        (.lineComment (start.span ((47 :: 47 :: body).foldl Position.advance start) fileId))
  | token {spelling rest value} :
      Trivia (spelling ++ rest) (spelling ++ rest) →
      Tokenizes (spelling ++ rest) spelling rest value.parserValue →
      value.Payload (spelling ++ rest) spelling rest →
      PieceMeaning fileId start false spelling rest (.token value (start.span (spelling.foldl Position.advance start) fileId))

/-- Each consecutive byte slice decodes to the scalars that determine the piece's
kind, payload and location. The cursor advances by those same scalars. -/
inductive PieceStream (fileId : ConcreteSpec.Core.FileId) : Position → List UInt8 → List Nat → List Piece → Prop
  | nil {pos} : PieceStream fileId pos [] [] []
  | cons {pos chunk bytes spelling suffix piece rest} :
      Utf8.Decodes chunk spelling → PieceMeaning fileId pos false spelling suffix piece →
      PieceStream fileId (spelling.foldl Position.advance pos) bytes suffix rest →
      PieceStream fileId pos (chunk ++ bytes) (spelling ++ suffix) (piece :: rest)

private inductive Scanned (fileId : ConcreteSpec.Core.FileId) : Bool → List Nat → List Nat → Position → List Piece → Prop
  | nil {rest} : Scanned fileId false [] rest {} []
  | cons {openComment consumed pos spelling suffix piece pieces} :
      Scanned fileId false consumed (spelling ++ suffix) pos pieces →
      PieceMeaning fileId pos openComment spelling suffix piece →
      Scanned fileId openComment (consumed ++ spelling) suffix
        (spelling.foldl Position.advance pos) (piece :: pieces)

private theorem commentBody_prepend {body rest : List Nat}
    (raw : ∀ c ∈ body, c ≠ 10 ∧ c ≠ 13 ∧ CommentRaw c)
    (stop : CommentBody rest rest) : CommentBody (body ++ rest) rest := by
  induction body with
  | nil => exact stop
  | cons c cs ih =>
    obtain ⟨lf, cr, valid⟩ := raw c (by simp)
    exact .cons lf cr valid (ih (fun d hd => raw d (by simp [hd])))

private theorem Scanned.finish {fileId : ConcreteSpec.Core.FileId} {openComment consumed suffix pos acc}
    (h : Scanned fileId openComment consumed suffix pos acc)
    (stop : openComment = true → CommentBody suffix suffix) :
    Scanned fileId false consumed suffix pos acc := by
  cases openComment with
  | false => exact h
  | true =>
    cases h with
    | cons hs hm =>
      cases hm with
      | comment hb _ => exact .cons hs (.comment hb (fun _ => .intro (commentBody_prepend hb (stop rfl))))

private theorem Scanned.space {fileId : ConcreteSpec.Core.FileId} {consumed pos acc c cs}
    (h : Scanned fileId false consumed (c :: cs) pos acc) (hc : Whitespace c) :
    Scanned fileId false (consumed ++ [c]) cs (pos.advance c) (recordSpace fileId pos (pos.advance c) acc) := by
  have fresh : PieceMeaning fileId pos false [c] cs (.whitespace (pos.span (pos.advance c) fileId)) :=
    .whitespace (by simp) (by simpa using hc)
  cases h with
  | nil => exact .cons .nil fresh
  | @cons _ pre start spelling _ piece pieces hs hm =>
    cases hm with
    | @whitespace spelling _ _ hw =>
      have hw' : ∀ d ∈ spelling ++ [c], Whitespace d := by
        simp only [List.mem_append, List.mem_singleton]
        intro d hd
        rcases hd with hd | rfl
        · exact hw d hd
        · exact hc
      have hs' : Scanned fileId false pre ((spelling ++ [c]) ++ cs) start pieces := by
        simpa only [List.append_assoc, List.singleton_append] using hs
      simpa only [recordSpace, List.append_assoc, List.foldl_append, List.foldl_cons,
        List.foldl_nil, Position.span] using Scanned.cons hs' (.whitespace (by simp) hw')
    | comment hb stop => exact .cons (.cons hs (.comment hb stop)) fresh
    | token trivia ht hp => exact .cons (.cons hs (.token trivia ht hp)) fresh

private theorem Scanned.comment {fileId : ConcreteSpec.Core.FileId} {consumed pos acc c cs}
    (h : Scanned fileId true consumed (c :: cs) pos acc) (hc : c ≠ 10 ∧ c ≠ 13 ∧ CommentRaw c) :
    Scanned fileId true (consumed ++ [c]) cs (pos.advance c) (extendComment fileId pos (pos.advance c) acc) := by
  cases h with
  | @cons _ pre start spelling _ piece pieces hs hm =>
    cases hm with
    | @comment _ body _ hb _ =>
      have hb' : ∀ d ∈ body ++ [c], d ≠ 10 ∧ d ≠ 13 ∧ CommentRaw d := by
        simp only [List.mem_append, List.mem_singleton]
        intro d hd
        rcases hd with hd | rfl
        · exact hb d hd
        · exact hc
      have hs' : Scanned fileId false pre ((47 :: 47 :: (body ++ [c])) ++ cs) start pieces := by
        simpa only [List.cons_append, List.append_assoc, List.singleton_append, List.nil_append] using hs
      simpa only [extendComment, ← List.cons_append, List.append_assoc, List.foldl_append,
        List.foldl_cons, List.foldl_nil, Position.span] using
          Scanned.cons hs' (.comment hb' (by simp) :
            PieceMeaning fileId start true (47 :: 47 :: (body ++ [c])) cs _)

private theorem tokens_scanned (fileId : ConcreteSpec.Core.FileId) (fuel : Nat) (input consumed : List Nat) (pos : Position)
    (comment : Bool) (acc out : List Piece) (h : tokens fileId fuel input pos comment acc = .ok out)
    (ha : Scanned fileId comment consumed input pos acc) :
    Scanned fileId false (consumed ++ input) [] (input.foldl Position.advance pos) out.reverse := by
  induction fuel generalizing input consumed pos comment acc with
  | zero =>
    cases input with
    | nil => cases Except.ok.inj h; simpa using ha.finish (fun _ => .eof)
    | cons => contradiction
  | succ fuel ih =>
    cases input with
    | nil => cases Except.ok.inj h; simpa using ha.finish (fun _ => .eof)
    | cons c cs =>
      simp only [tokens] at h
      split at h
      · rename_i hc
        have hcontinue : comment = true ∧ c ≠ 10 ∧ c ≠ 13 := by
          simpa [Bool.and_eq_true, and_assoc] using hc
        obtain ⟨rfl, hlf, hcr⟩ := hcontinue
        split at h
        · rename_i hraw
          simpa only [List.append_assoc, List.singleton_append, List.foldl_cons] using
            ih cs (consumed ++ [c]) (pos.advance c) true _ h
              (ha.comment ⟨hlf, hcr, of_decide_eq_true hraw⟩)
        · contradiction
      · rename_i hstop
        have closed := ha.finish (by
          intro hc
          have newline : c = 10 ∨ c = 13 := by
            simp [hc] at hstop
            omega
          rcases newline with rfl | rfl
          · exact .lf cs
          · exact .cr cs)
        split at h
        · rename_i hw
          simpa only [List.append_assoc, List.singleton_append, List.foldl_cons] using
            ih cs (consumed ++ [c]) (pos.advance c) false _ h
              (closed.space ((whitespace?_iff c).mp hw))
        · rename_i hw
          split at h
          · rename_i tail he
            cases he
            simpa only [List.append_assoc, List.cons_append, List.nil_append, List.foldl_cons,
              List.foldl_nil] using
              ih tail (consumed ++ [47, 47]) _ true _ h
                (.cons closed (.comment (body := []) (by simp) (by simp)))
          · rename_i notComment
            cases ht : nextToken (c :: cs) pos with
            | error e => simp [ht, bind, Except.bind] at h
            | ok triple =>
              obtain ⟨value, spelling, tail⟩ := triple
              simp only [ht, bind, Except.bind] at h
              obtain ⟨htoken, hp⟩ := nextToken_payload ht
              obtain ⟨he, _⟩ := token_split htoken
              have trivia : Trivia (c :: cs) (c :: cs) :=
                .stop _ (by intro d ds heq; cases heq; simpa [whitespace?_iff] using hw) notComment
              have hm : PieceMeaning fileId pos false spelling tail
                  (.token value (pos.span (spelling.foldl Position.advance pos) fileId)) :=
                .token (he ▸ trivia) (he ▸ htoken) (he ▸ hp)
              change Scanned fileId false (consumed ++ (c :: cs)) [] ((c :: cs).foldl Position.advance pos) out.reverse
              rw [he, List.foldl_append, ← List.append_assoc]
              exact ih tail _ _ false _ h (.cons (he ▸ closed) hm)

private theorem split_decodes (consumed : List Nat) {tail : List Nat} {bytes : List UInt8}
    (h : Utf8.Decodes bytes (consumed ++ tail)) :
    ∃ first last, bytes = first ++ last ∧ Utf8.Decodes first consumed ∧ Utf8.Decodes last tail := by
  induction consumed generalizing bytes with
  | nil => exact ⟨[], bytes, rfl, .nil, h⟩
  | cons c cs ih =>
    cases h with
    | @cons _ chunk rest _ hc hr =>
      obtain ⟨first, last, rfl, hf, hl⟩ := ih hr
      exact ⟨chunk ++ first, last, (List.append_assoc ..).symm, .cons hc hf, hl⟩

private theorem Scanned.close {fileId : ConcreteSpec.Core.FileId} {openComment consumed pos suffix pieces}
    (h : Scanned fileId openComment consumed suffix pos pieces) (closed : openComment = false)
    {bytes rest tail} (hd : Utf8.Decodes bytes consumed) (ht : PieceStream fileId pos rest suffix tail) :
    PieceStream fileId {} (bytes ++ rest) (consumed ++ suffix) (pieces.reverse ++ tail) := by
  induction h generalizing bytes rest tail with
  | nil => cases hd; exact ht
  | @cons openComment pre start spelling suffix piece pieces hs hm ih =>
    subst openComment
    obtain ⟨first, last, rfl, hf, hl⟩ := split_decodes pre hd
    simpa only [List.append_assoc, List.reverse_cons, List.singleton_append] using
      ih rfl hf (PieceStream.cons hl hm ht)

theorem PieceMeaning.span_eq {fileId : ConcreteSpec.Core.FileId} {pos openComment spelling suffix piece}
    (h : PieceMeaning fileId pos openComment spelling suffix piece) :
    piece.span = pos.span (spelling.foldl Position.advance pos) fileId := by
  cases h <;> rfl

theorem PieceMeaning.spelling_nonempty {fileId : ConcreteSpec.Core.FileId}
    {pos openComment spelling suffix piece} (h : PieceMeaning fileId pos openComment spelling suffix piece) :
    spelling ≠ [] := by
  cases h with
  | whitespace ne _ => exact ne
  | comment => simp
  | token _ ht _ => exact (token_split ht).2

/-- One copy of the exact bytes, scoped to all ranges in this stream. -/
structure LosslessStream where
  private mk ::
  source : ByteArray
  fileId : ConcreteSpec.Core.FileId
  pieces : List Piece
  meaning : ∃ scalars, PieceStream fileId {} source.data.toList scalars pieces

def LosslessStream.parserView (stream : LosslessStream) : List Token :=
  stream.pieces.filterMap Piece.parserToken

def LosslessStream.reconstruct (stream : LosslessStream) : List UInt8 :=
  replay stream.pieces stream.source.data.toList []

private theorem PieceStream.replay_eq {fileId : ConcreteSpec.Core.FileId} {pos bytes scalars pieces} (h : PieceStream fileId pos bytes scalars pieces)
    (acc : List UInt8) : replay pieces bytes acc = acc.reverse ++ bytes := by
  induction h generalizing acc with
  | nil => simp [replay]
  | @cons pos chunk bytes spelling suffix piece rest hd hm _ ih =>
    simp only [replay, hm.span_eq, Position.span, decoded_width hd, Nat.add_sub_cancel_left,
      List.splitAt_eq, List.take_left, List.drop_left]
    rw [ih, List.reverseAux_eq, List.reverse_append, List.reverse_reverse, List.append_assoc]

/-- The ranges form an ordered, gap-free partition of the retained bytes. -/
theorem LosslessStream.reconstruct_eq (stream : LosslessStream) :
    stream.reconstruct = stream.source.data.toList := by
  simpa only [LosslessStream.reconstruct, List.reverse_nil, List.nil_append] using
    stream.meaning.choose_spec.replay_eq []

def lexLossless (src : ByteArray) (fileId : ConcreteSpec.Core.FileId := ⟨0⟩) : Except Refusal LosslessStream :=
  match hs : checkSource src.data.toList with
  | .error e => .error (.syntaxError s!"invalid source at byte {e.offset}: {repr e.reason}")
  | .ok cs =>
    match ht : tokens fileId cs.length cs {} false [] with
    | .error e => .error e
    | .ok pieces => .ok ⟨src, fileId, pieces, by
        have scan := tokens_scanned fileId cs.length cs [] {} false [] pieces ht .nil
        refine ⟨cs, ?_⟩
        simpa using scan.close rfl (checkSource_sound hs).1 (.nil (pos := cs.foldl Position.advance {}))⟩

def lex (src : ByteArray) (fileId : ConcreteSpec.Core.FileId := ⟨0⟩) : Except Refusal (List Token) :=
  (lexLossless src fileId).map LosslessStream.parserView

theorem lexLossless_origin {src : ByteArray} {fileId : ConcreteSpec.Core.FileId}
    {stream : LosslessStream} (h : lexLossless src fileId = .ok stream) :
    stream.source = src ∧ stream.fileId = fileId := by
  unfold lexLossless at h
  split at h
  · contradiction
  · split at h
    · contradiction
    · cases Except.ok.inj h
      exact ⟨rfl, rfl⟩

/-- A successful stream reconstructs the exact source bytes from its stored ranges. -/
theorem lexLossless_roundTrip {src : ByteArray} {fileId : ConcreteSpec.Core.FileId} {stream : LosslessStream}
    (h : lexLossless src fileId = .ok stream) : stream.reconstruct = src.data.toList := by
  rw [stream.reconstruct_eq, (lexLossless_origin h).1]

/-- Every emitted piece is justified by a consecutive slice of the original UTF-8
bytes and the independent lexical judgments for its decoded scalars. -/
theorem lexLossless_pieces {src : ByteArray} {fileId : ConcreteSpec.Core.FileId} {stream : LosslessStream}
    (h : lexLossless src fileId = .ok stream) :
    ∃ scalars, PieceStream fileId {} src.data.toList scalars stream.pieces := by
  obtain ⟨hs, hf⟩ := lexLossless_origin h
  rw [← hs, ← hf]
  exact stream.meaning

/-- Every piece retains the stream's file-table index after extraction. -/
theorem PieceStream.pieceFileId {fileId : ConcreteSpec.Core.FileId} {pos bytes scalars pieces}
    (meaning : PieceStream fileId pos bytes scalars pieces) {piece : Piece}
    (member : piece ∈ pieces) : piece.span.fileId = fileId := by
  induction meaning with
  | nil => simp at member
  | cons _ hm _ ih =>
    rcases List.mem_cons.mp member with rfl | tail
    · rw [hm.span_eq]
      rfl
    · exact ih tail

section ProjectLexer
open ConcreteSpec.Core.SourcePathSpan

/-- The issuing table resolves the file and supplies all bytes for its pieces. -/
structure ProjectStream (project : Project) (fileId : ConcreteSpec.Core.FileId) where
  file : File
  pieces : List Piece
  issued : project.fileAt fileId = some file
  meaning : ∃ scalars, PieceStream fileId {} file.source.toBytes scalars pieces

/-- Resolve a file in the issuing table and lex its exact bytes. The stream
retains the loaded file, so it shares the source already held by the project. -/
def lexProject (project : Project) (fileId : ConcreteSpec.Core.FileId) :
    Except Refusal (ProjectStream project fileId) :=
  match hf : project.fileAt fileId with
  | none => .error (.internal s!"unknown project file ID {fileId.index}")
  | some file =>
    let bytes : ByteArray := ⟨file.source.toBytes.toArray⟩
    match hs : lexLossless bytes fileId with
    | .error e => .error e
    | .ok stream => .ok ⟨file, stream.pieces, hf, by simpa [bytes] using lexLossless_pieces hs⟩

/-- Issue a range for source rebinding only from a piece belonging to this stream. -/
def ProjectStream.scopePiece {project : Project} {fileId : ConcreteSpec.Core.FileId}
    (stream : ProjectStream project fileId) (piece : Piece) (_member : piece ∈ stream.pieces) :
    Option (ScopedSpan project) :=
  projectSpanAt project fileId piece.span.start piece.span.stop

end ProjectLexer

private theorem prependTrivia {fileId : ConcreteSpec.Core.FileId} {pos : Position} {taken input : List Nat} {out : List Token}
    (prepend : ∀ {rest}, Trivia input rest → Trivia (taken ++ input) rest)
    (h : TokenStream fileId (taken.foldl Position.advance pos) input out) :
    TokenStream fileId pos (taken ++ input) out := by
  cases h with
  | eof h => exact .eof (prepend h)
  | @cons _ trivia remaining lexeme rest value out ht htkn hs =>
    have ht' : Trivia ((taken ++ trivia) ++ remaining) remaining := by
      rw [List.append_assoc]
      exact prepend ht
    rw [← List.append_assoc]
    simpa only [List.foldl_append] using TokenStream.cons
      (pos := pos) (trivia := taken ++ trivia) ht' htkn
      (by simpa only [List.foldl_append] using hs)

-- Inside a comment, expose the consumed body separately so its opener can turn it
-- into trivia. The position of the remaining stream includes every consumed scalar.
private theorem tokens_sound (fileId : ConcreteSpec.Core.FileId) (fuel : Nat) (input : List Nat) (pos : Position)
    (comment : Bool) (acc out : List Piece) (h : tokens fileId fuel input pos comment acc = .ok out) :
    ∃ suffix, out.filterMap Piece.parserToken = acc.reverse.filterMap Piece.parserToken ++ suffix ∧
      if comment then ∃ body rest, input = body ++ rest ∧ CommentBody input rest ∧
        TokenStream fileId (body.foldl Position.advance pos) rest suffix
      else TokenStream fileId pos input suffix := by
  induction fuel generalizing input pos comment acc out with
  | zero =>
    cases input with
    | nil =>
      cases Except.ok.inj h
      have eof : TokenStream fileId pos [] [] := .eof (.stop [] (by simp) (by simp))
      refine ⟨[], by simp, ?_⟩
      cases comment
      · exact eof
      · exact ⟨[], [], rfl, .eof, eof⟩
    | cons c cs => contradiction
  | succ fuel ih =>
    cases input with
    | nil =>
      cases Except.ok.inj h
      have eof : TokenStream fileId pos [] [] := .eof (.stop [] (by simp) (by simp))
      refine ⟨[], by simp, ?_⟩
      cases comment
      · exact eof
      · exact ⟨[], [], rfl, .eof, eof⟩
    | cons c cs =>
      cases comment
      · simp only [tokens, Bool.false_and, Bool.false_eq_true, ↓reduceIte] at h
        split at h
        · rename_i hw
          obtain ⟨suffix, he, hs⟩ := ih cs (pos.advance c) false (recordSpace fileId pos (pos.advance c) acc) out h
          exact ⟨suffix, by simpa only [recordSpace_view] using he, prependTrivia (taken := [c])
            (Trivia.whitespace ((whitespace?_iff c).mp hw)) hs⟩
        · rename_i hw
          split at h
          · rename_i tail heq
            cases heq
            obtain ⟨suffix, he, body, rest, hb, hc, hs⟩ :=
              ih tail ((pos.advance 47).advance 47) true
                (.lineComment (pos.span ((pos.advance 47).advance 47) fileId) :: acc) out h
            simp [List.filterMap_cons, Piece.parserToken] at he
            have hs' : TokenStream fileId pos ((47 :: 47 :: body) ++ rest) suffix :=
              prependTrivia (fun ht => by
                simp only [List.cons_append, ← hb]
                exact .comment (.intro hc) ht) hs
            exact ⟨suffix, by simpa only [List.filterMap_reverse] using he, by simpa only [Bool.false_eq_true, ↓reduceIte, List.cons_append, ← hb] using hs'⟩
          · rename_i hn
            cases ht : nextToken (c :: cs) pos with
            | error e => simp [ht, bind, Except.bind] at h
            | ok triple =>
              obtain ⟨value, lexeme, tail⟩ := triple
              let stop := lexeme.foldl Position.advance pos
              let piece := Piece.token value (pos.span stop fileId)
              let tok : Token := ⟨value.parserValue, pos.span stop fileId⟩
              simp only [ht, bind, Except.bind] at h
              obtain ⟨suffix, he, hs⟩ := ih tail stop false (piece :: acc) out h
              refine ⟨tok :: suffix, by simpa [piece, tok, Piece.parserToken, List.append_assoc] using he, ?_⟩
              exact TokenStream.cons (trivia := [])
                (.stop _ (by intro d ds heq; cases heq; simpa [whitespace?_iff] using hw) hn)
                (nextToken_sound ht) hs
      · simp only [tokens, Bool.true_and] at h
        by_cases hc : c ≠ 10 ∧ c ≠ 13
        · have hcontinue : (c != 10 && c != 13) = true := by simpa using hc
          simp only [hcontinue, ↓reduceIte] at h
          split at h
          · rename_i hraw
            obtain ⟨suffix, he, body, rest, hb, hbody, hs⟩ :=
              ih cs (pos.advance c) true (extendComment fileId pos (pos.advance c) acc) out h
            exact ⟨suffix, by simpa only [extendComment_view] using he, c :: body, rest, congrArg (c :: ·) hb,
              .cons hc.1 hc.2 (of_decide_eq_true hraw) hbody, hs⟩
          · contradiction
        · have hstop : c = 10 ∨ c = 13 := by omega
          have hcontinue : (c != 10 && c != 13) = false := by
            rcases hstop with rfl | rfl <;> decide
          have hw : Whitespace c := by rcases hstop with rfl | rfl <;> decide
          simp only [hcontinue, Bool.false_eq_true, ↓reduceIte, (whitespace?_iff c).mpr hw] at h
          obtain ⟨suffix, he, hs⟩ := ih cs (pos.advance c) false (recordSpace fileId pos (pos.advance c) acc) out h
          refine ⟨suffix, by simpa only [recordSpace_view] using he, [], c :: cs, rfl, ?_, prependTrivia (taken := [c])
            (Trivia.whitespace hw) hs⟩
          rcases hstop with rfl | rfl
          · exact .lf cs
          · exact .cr cs

/-- The lossless scanner projects to the independent parser-facing lexical relation. -/
theorem lexLossless_correct {src : ByteArray} {fileId : ConcreteSpec.Core.FileId} {result : LosslessStream}
    (h : lexLossless src fileId = .ok result) : Lexes src result.parserView fileId := by
  unfold lexLossless at h
  split at h
  · contradiction
  · rename_i cs hs
    split at h
    · contradiction
    · rename_i pieces ht
      cases Except.ok.inj h
      obtain ⟨suffix, he, stream⟩ := tokens_sound fileId cs.length cs {} false [] pieces ht
      obtain ⟨decodes, allowed, _⟩ := checkSource_sound hs
      exact ⟨cs, decodes, allowed, by simpa [LosslessStream.parserView, he] using stream⟩

/-- Every successful compiler lexing result satisfies the surface lexical relation. -/
theorem lex_correct {src : ByteArray} {fileId : ConcreteSpec.Core.FileId} {toks : List Token} (h : lex src fileId = .ok toks) :
    Lexes src toks fileId := by
  cases hs : lexLossless src fileId with
  | error e => simp [lex, hs, Except.map] at h
  | ok result =>
    simp [lex, hs, Except.map] at h
    subst toks
    exact lexLossless_correct hs

-- Reference projection from before the lossless stream. The optional bound
-- qualifies the exact digit runs visited by that scanner, using the full run length.
private def previousPunct (input : List Nat) : Option (TokenValue × List Nat × List Nat) := do
  let (lexeme, rest) ← longestMatch punctuationScalars input
  let (_, kind) ← punctuation.find? fun (s, _) => s == scalarText lexeme
  return (.fixed kind, lexeme, rest)

private def previousNextToken (budget : Option Nat) (input : List Nat) (pos : Position) :
    Except Refusal (TokenValue × List Nat × List Nat) := do
  if let some (name, rest) := identifier input then
    return (wordValue (scalarText name), name, rest)
  if let some bound := budget then
    if bound < (scanDigits (splitRadix input).1 (splitRadix input).2).1.length then
      throw (.lexerDigitLimit pos.byte bound)
  match integerLiteral input with
  | .ok (some i) => return (.integer i.value (splitRadix input).1 i.suffix, i.lexeme, i.rest)
  | .error _ => throw (.syntaxError s!"malformed integer at byte {pos.byte}")
  | .ok none =>
    if let some p := previousPunct input then return p
    if let 34 :: cs := input then
      match stringBody cs with
      | .ok (_, rest) => return (.fixed .stringLiteral, input.take (input.length - rest.length), rest)
      | .error _ => throw (.syntaxError s!"malformed string at byte {pos.byte}")
    match byteLiteral input with
    | .error _ => throw (.syntaxError s!"malformed byte literal at byte {pos.byte}")
    | .ok (some (lexeme, _, rest)) => return (.fixed .byteLiteral, lexeme, rest)
    | .ok none =>
      if let some (lexeme, rest) := label input then return (.fixed .label, lexeme, rest)
      else throw (.syntaxError s!"unexpected character at byte {pos.byte}")

private def previousTokens (budget : Option Nat) (fileId : ConcreteSpec.Core.FileId) : Nat → List Nat → Position → Bool → List Token → Except Refusal (List Token)
  | _, [], _, _, acc => .ok acc.reverse
  | 0, _, _, _, _ => .error (.internal "lexer exhausted its progress bound")
  | fuel + 1, input@(c :: cs), pos, comment, acc => do
    if comment && c != 10 && c != 13 then
      if decide (CommentRaw c) then previousTokens budget fileId fuel cs (pos.advance c) true acc
      else throw (.syntaxError s!"forbidden comment character at byte {pos.byte}")
    else if whitespace? c then previousTokens budget fileId fuel cs (pos.advance c) false acc
    else if let 47 :: 47 :: rest := input then
      previousTokens budget fileId fuel rest ((pos.advance 47).advance 47) true acc
    else
      let (value, lexeme, rest) ← previousNextToken budget input pos
      let stop := lexeme.foldl Position.advance pos
      previousTokens budget fileId fuel rest stop false (⟨value, pos.span stop fileId⟩ :: acc)

private def previousLex (budget : Option Nat) (src : ByteArray) (fileId : ConcreteSpec.Core.FileId) : Except Refusal (List Token) := do
  match checkSource src.data.toList with
  | .error e => throw (.syntaxError s!"invalid source at byte {e.offset}: {repr e.reason}")
  | .ok cs => previousTokens budget fileId cs.length cs {} false []

private def projectToken (result : LosslessValue × List Nat × List Nat) :
    TokenValue × List Nat × List Nat :=
  (result.1.parserValue, result.2)

private theorem punct_projects (input : List Nat) :
    (punct input).map projectToken = previousPunct input := by
  unfold punct previousPunct
  cases h : longestMatch punctuationScalars input with
  | none => simp
  | some result =>
    obtain ⟨spelling, rest⟩ := result
    cases hfind : punctuation.find? (fun (s, _) => s == scalarText spelling) with
    | none => simp [hfind, bind, Option.bind]
    | some entry => cases entry; simp [hfind, bind, Option.bind, pure, projectToken,
        LosslessValue.parserValue]

private theorem nextToken_projects (input : List Nat) (pos : Position) :
    (nextToken input pos).map projectToken =
      previousNextToken (some integerDigitLimit) input pos := by
  unfold nextToken previousNextToken integerLiteral integerDigits
  cases hi : identifier input with
  | some result => cases result; simp [projectToken, LosslessValue.parserValue, pure, Except.pure, Except.map]
  | none =>
    simp only [boundedDigits_spec]
    by_cases hn : (scanDigits (splitRadix input).1 (splitRadix input).2).1.length ≤ integerDigitLimit
    · have hlt : ¬integerDigitLimit < (scanDigits (splitRadix input).1 (splitRadix input).2).1.length := by
        omega
      simp only [hn, hlt, ↓reduceIte, bind, Except.bind, pure, Except.pure]
      split
      · rfl
      · rfl
      · rw [← punct_projects input]
        cases hp : punct input with
        | some result => cases result; rfl
        | none =>
          simp only [Option.map]
          split
          · rename_i _ _ body _
            cases stringBody body with
            | ok result => cases result; rfl
            | error reason => rfl
          · split
            · rfl
            · rfl
            · split <;> rfl
    · have hlt : integerDigitLimit < (scanDigits (splitRadix input).1 (splitRadix input).2).1.length := by
        omega
      simp only [hn, hlt, ↓reduceIte]
      rfl

private theorem tokens_project (fileId : ConcreteSpec.Core.FileId) (fuel : Nat) (input : List Nat) (pos : Position)
    (comment : Bool) (acc : List Piece) :
    (tokens fileId fuel input pos comment acc).map (List.filterMap Piece.parserToken) =
      previousTokens (some integerDigitLimit) fileId fuel input pos comment
        (acc.filterMap Piece.parserToken) := by
  induction fuel generalizing input pos comment acc with
  | zero => cases input <;> simp [tokens, previousTokens, Except.map]
  | succ fuel ih =>
    cases input with
    | nil => simp [tokens, previousTokens, Except.map]
    | cons c cs =>
      simp only [tokens, previousTokens]
      split
      · split
        · rw [ih]
          congr 1
          simpa only [List.filterMap_reverse, List.reverse_inj] using extendComment_view fileId pos (pos.advance c) acc
        · rfl
      · split
        · rw [ih]
          congr 1
          simpa only [List.filterMap_reverse, List.reverse_inj] using recordSpace_view fileId pos (pos.advance c) acc
        · split
          · rw [ih]
            rfl
          · rw [← nextToken_projects]
            cases ht : nextToken (c :: cs) pos with
            | error => rfl
            | ok result =>
              obtain ⟨value, spelling, rest⟩ := result
              exact ih rest _ false _

/-- The parser view equals the previous scanner with precisely the new digit
budget applied. The reference checks the full run; production stops at overflow. -/
theorem lex_projection (src : ByteArray) (fileId : ConcreteSpec.Core.FileId := ⟨0⟩) :
    lex src fileId = previousLex (some integerDigitLimit) src fileId := by
  unfold lex lexLossless previousLex
  split
  · rename_i error hs
    rw [hs]
    rfl
  · rename_i cs hs
    conv => rhs; rw [hs]
    have h := tokens_project fileId cs.length cs {} false []
    split
    · rename_i error ht
      simpa only [ht, Except.map, List.filterMap_nil] using h
    · rename_i pieces ht
      simpa only [ht, Except.map, List.filterMap_nil, LosslessStream.parserView] using h

/-! These controls reduce the production entry points in the kernel. -/

private def payloadSource : ByteArray := " //é\r\n\"a\\n\" '\\t' 'loop //tail".toUTF8

example : (lexLossless payloadSource).map LosslessStream.pieces = .ok [
    .whitespace ⟨0, 1, 1, 1, ⟨0⟩⟩, .lineComment ⟨1, 5, 1, 2, ⟨0⟩⟩, .whitespace ⟨5, 7, 1, 6, ⟨0⟩⟩,
    .token (.stringLiteral "a\n") ⟨7, 12, 2, 1, ⟨0⟩⟩, .whitespace ⟨12, 13, 2, 6, ⟨0⟩⟩,
    .token (.byteLiteral 9) ⟨13, 17, 2, 7, ⟨0⟩⟩, .whitespace ⟨17, 18, 2, 11, ⟨0⟩⟩,
    .token (.label "loop") ⟨18, 23, 2, 12, ⟨0⟩⟩, .whitespace ⟨23, 24, 2, 17, ⟨0⟩⟩,
    .lineComment ⟨24, 30, 2, 18, ⟨0⟩⟩] := rfl

example : (lexLossless payloadSource).map LosslessStream.reconstruct =
    .ok payloadSource.data.toList := rfl

example : lex payloadSource = .ok [
    ⟨.fixed .stringLiteral, ⟨7, 12, 2, 1, ⟨0⟩⟩⟩, ⟨.fixed .byteLiteral, ⟨13, 17, 2, 7, ⟨0⟩⟩⟩,
    ⟨.fixed .label, ⟨18, 23, 2, 12, ⟨0⟩⟩⟩] := rfl

example : lex ("// lead\n" ++ r#"fn main()->u8{ return 255; } // tail"#).toUTF8 = .ok [
    ⟨.fixed .fn, ⟨8, 10, 2, 1, ⟨0⟩⟩⟩, ⟨.name "main", ⟨11, 15, 2, 4, ⟨0⟩⟩⟩,
    ⟨.fixed .lparen, ⟨15, 16, 2, 8, ⟨0⟩⟩⟩, ⟨.fixed .rparen, ⟨16, 17, 2, 9, ⟨0⟩⟩⟩,
    ⟨.fixed .arrow, ⟨17, 19, 2, 10, ⟨0⟩⟩⟩, ⟨.fixed .u8, ⟨19, 21, 2, 12, ⟨0⟩⟩⟩,
    ⟨.fixed .lbrace, ⟨21, 22, 2, 14, ⟨0⟩⟩⟩, ⟨.fixed .return_, ⟨23, 29, 2, 16, ⟨0⟩⟩⟩,
    ⟨.integer 255 .decimal none, ⟨30, 33, 2, 23, ⟨0⟩⟩⟩,
    ⟨.fixed .semi, ⟨33, 34, 2, 26, ⟨0⟩⟩⟩, ⟨.fixed .rbrace, ⟨35, 36, 2, 28, ⟨0⟩⟩⟩] := rfl

example : lex "01bad".toUTF8 = .error (.syntaxError "malformed integer at byte 0") := rfl

example : boundedDigits .hexadecimal 2 [97, 102, 117, 56] = some ([97, 102], [117, 56]) := rfl
example : boundedDigits .hexadecimal 2 [97, 102, 48, 117, 56] = none := rfl

end Concrete.Lex

namespace Concrete.Lex
open ConcreteSpec.Surface0

def scannerFailure : Refusal → Prop
  | .syntaxError _ | .lexerDigitLimit .. => True
  | _ => False

private theorem nextToken_failure {input : List Nat} {pos : Position} {e : Refusal}
    (h : nextToken input pos = .error e) : scannerFailure e := by
  unfold nextToken at h
  repeat' first | split at h | contradiction | cases h <;> trivial

private theorem tokens_failure {fileId : ConcreteSpec.Core.FileId} (fuel : Nat)
    (input : List Nat) (pos : Position) (comment : Bool) (acc : List Piece) {e : Refusal}
    (bound : input.length ≤ fuel) (h : tokens fileId fuel input pos comment acc = .error e) :
    scannerFailure e := by
  induction fuel generalizing input pos comment acc with
  | zero =>
    have : input = [] := List.length_eq_zero_iff.mp (by omega)
    subst input
    contradiction
  | succ fuel ih =>
    cases input with
    | nil => contradiction
    | cons c cs =>
      simp only [List.length_cons] at bound
      simp only [tokens] at h
      split at h
      · split at h
        · exact ih _ _ _ _ (by omega) h
        · cases h; trivial
      · split at h
        · exact ih _ _ _ _ (by omega) h
        · split at h
          · rename_i tail eqn
            cases eqn
            simp only [List.length_cons] at bound
            exact ih _ _ _ _ (by omega) h
          · cases ht : nextToken (c :: cs) pos with
            | error err =>
              simp [ht, Bind.bind, Except.bind] at h
              subst err
              exact nextToken_failure ht
            | ok result =>
              obtain ⟨value, spelling, rest⟩ := result
              have ⟨he, hn⟩ := token_split (nextToken_sound ht)
              have lengths := congrArg List.length he
              simp only [List.length_cons, List.length_append] at lengths
              have positive := List.length_pos_iff.mpr hn
              exact ih rest _ _ _ (by omega) (by simpa [ht, Bind.bind, Except.bind] using h)

theorem lexLossless_failure {src : ByteArray} {fileId : ConcreteSpec.Core.FileId}
    {e : Refusal} (h : lexLossless src fileId = .error e) : scannerFailure e := by
  unfold lexLossless at h
  split at h
  · cases h; trivial
  · split at h
    · rename_i _ ht
      cases h
      exact tokens_failure _ _ _ _ _ (Nat.le_refl _) ht
    · contradiction

end Concrete.Lex

namespace Concrete.Lex
open ConcreteSpec.Surface0
namespace Bridge
open ConcreteSpec.StageInterface (FileSpan Source RetainedToken RawPiece)

/-- The semantic lexer already distinguishes the three decoded literal classes.
A plain fixed token is either a punctuation entry or a reserved word. -/
private theorem fixed_plain {input spelling tail : List Nat} {kind : Terminal}
    (ht : Tokenizes input spelling tail (.fixed kind))
    (hp : (LosslessValue.parsed (.fixed kind)).Payload input spelling tail) :
    ConcreteSpec.StageInterface.Terminal.HasPayload kind = false := by
  generalize eqn : TokenValue.fixed kind = value at ht
  cases ht with
  | word h =>
    simp only [wordValue] at eqn
    split at eqn
    · contradiction
    · rename_i separate
      unfold word at eqn separate
      cases found : wordTokens.find? (fun entry => entry.1 == scalarText spelling) with
      | none => simp [found] at separate
      | some entry =>
        simp [found] at eqn
        have allowed : ∀ entry ∈ wordTokens,
            ConcreteSpec.StageInterface.Terminal.HasPayload entry.2 = false := by decide
        exact eqn.symm ▸ allowed entry (List.mem_of_find?_eq_some found)
  | integer => contradiction
  | string => cases eqn; exact False.elim (hp.1 rfl)
  | byte => cases eqn; exact False.elim (hp.2.1 rfl)
  | label => cases eqn; exact False.elim (hp.2.2 rfl)
  | punct _ member =>
    cases eqn
    have allowed : ∀ e ∈ punctuation,
        ConcreteSpec.StageInterface.Terminal.HasPayload e.2 = false := by decide
    exact allowed _ member

theorem fixed_in_stream {fileId pos bytes scalars pieces kind span}
    (h : PieceStream fileId pos bytes scalars pieces)
    (member : Piece.token (.parsed (.fixed kind)) span ∈ pieces) :
    ConcreteSpec.StageInterface.Terminal.HasPayload kind = false := by
  induction h with
  | nil => contradiction
  | cons _ meaning _ ih =>
    rcases List.mem_cons.mp member with eqn | rest
    · subst_vars
      cases meaning with | token _ ht hp => exact fixed_plain ht hp
    · exact ih rest

abbrev RetainedPiece := RawPiece ⊕ RetainedToken

def fileSpan (s : Span) : FileSpan := ⟨s.fileId,s.start,s.stop,s.line,s.column⟩

def retainPiece (stream : LosslessStream) (piece : {p // p ∈ stream.pieces}) : RetainedPiece :=
  let raw (s : Span) : RawPiece := ⟨fileSpan s, stream.source.extract s.start s.stop⟩
  match eqn : piece.val with
  | .whitespace s | .lineComment s => .inl (raw s)
  | .token value s =>
    let decoded : ConcreteSpec.StageInterface.TokenValue := match hv : value with
      | .parsed (.fixed kind) => .fixed kind (by
          obtain ⟨_, meaning⟩ := stream.meaning
          exact fixed_in_stream meaning (by simpa only [eqn, hv] using piece.property :
            Piece.token (.parsed (.fixed kind)) s ∈ stream.pieces))
      | .parsed (.name name) => .name name
      | .parsed (.integer n radix suffix) => .integer n radix suffix
      | .stringLiteral text => .stringLiteral text.toUTF8.data.toList
      | .byteLiteral byte => .byteLiteral byte
      | .label name => .label ⟨name⟩
    .inr ⟨⟨fileSpan s, decoded⟩, (raw s).value⟩

/-- Each token is preceded by exactly its trivia. The final run belongs to EOF. -/
structure Runs where
  entries : List (List RawPiece × RetainedToken)
  trailing : List RawPiece

def Runs.prepend (p : RetainedPiece) (runs : Runs) : Runs :=
  match p with
  | .inr t => ⟨([],t) :: runs.entries, runs.trailing⟩
  | .inl raw =>
    match runs.entries with
    | [] => ⟨[],raw :: runs.trailing⟩
    | (before,t) :: rest => ⟨(raw :: before,t) :: rest,runs.trailing⟩

def group (pieces : List RetainedPiece) : Runs := pieces.foldr Runs.prepend ⟨[],[]⟩

def Runs.stream (runs : Runs) (source : Source) : ConcreteSpec.StageInterface.TokenStream :=
  ⟨source, (runs.entries.map Prod.snd).toArray,
    ((runs.entries.map (List.toArray ∘ Prod.fst)) ++ [runs.trailing.toArray]).toArray⟩

def retain (source : Source) (stream : LosslessStream) : ConcreteSpec.StageInterface.TokenStream :=
  (group (stream.pieces.attach.map (retainPiece stream))).stream source

end Bridge

/-- Resolve the exact project file and retain the scanner's located pieces in
the registered representation. The digit-limit channel preserves its offset. -/
def lexStream (source : ConcreteSpec.StageInterface.Source) :
    Except Concrete.Interfaces.LexRefusal ConcreteSpec.StageInterface.TokenStream :=
  match source.project.fileAt source.entryFile with
  | none => .error (.missingFile source.entryFile)
  | some file =>
    match h : lexLossless ⟨file.source.toBytes.toArray⟩ source.entryFile with
    | .ok stream => .ok (Bridge.retain source stream)
    | .error e =>
      have hf := lexLossless_failure h
      match e with
      | .syntaxError diagnostic => .error (.malformed diagnostic)
      | .lexerDigitLimit offset limit => .error (.digitLimit offset limit)
      | .internal _ | .parser _ | .unsupported _ | .checker _ | .backendResource ..
      | .backendProfile .. | .toolFailure _ | .manifest _ | .usageError _ => False.elim hf

end Concrete.Lex
