import ConcreteSpec.Proofs.Surface0

/-!
# Settled lexer components

Total byte-list UTF-8 decoding and source checking, ASCII identifier tests and line comments.
No tokens, literal rules, whitespace rules, shebang handling or block comments are introduced.
Error constructors are internal data, not diagnostic wording. Offsets are zero-based byte offsets;
truncation points at EOF. TODO: prove the first non-extendible-prefix property of error offsets.
The success correspondence theorems below do not claim error-position correctness.
-/


namespace Concrete.Lex
open ConcreteSpec.Surface0

/-- Internal refusal categories; no user-visible diagnostic spelling is fixed. -/
inductive DecodeReason where
  | truncated | overlong | surrogate | tooLarge | unexpectedContinuation | nul | bom
  deriving DecidableEq, Repr

/-- A refusal and its zero-based byte offset (EOF for missing bytes). -/
structure DecodeError where
  offset : Nat
  reason : DecodeReason
  deriving DecidableEq, Repr

/-- Diagnose an invalid UTF-8 sequence from its leading byte, checking each byte in order.
`unexpectedContinuation` also covers a non-continuation where a continuation was required. -/
def badSequence (off : Nat) : List UInt8 → DecodeError
  | [] => ⟨off, .truncated⟩
  | a :: rest =>
    if a.toNat < 0xC0 then ⟨off, .unexpectedContinuation⟩
    else if a.toNat < 0xC2 then ⟨off, .overlong⟩
    else if a.toNat > 0xF4 then ⟨off, .tooLarge⟩
    else
      let n := if a.toNat < 0xE0 then 1 else if a.toNat < 0xF0 then 2 else 3
      let rec check (i : Nat) (left : Nat) (xs : List UInt8) : DecodeError :=
        match left, xs with
        | 0, _ => ⟨off, .unexpectedContinuation⟩
        | _ + 1, [] => ⟨off + i, .truncated⟩
        | k + 1, b :: bs =>
          if b.toNat < 0x80 ∨ b.toNat > 0xBF then ⟨off + i, .unexpectedContinuation⟩
          else if i = 1 ∧ ((a.toNat = 0xE0 ∧ b.toNat < 0xA0) ∨
              (a.toNat = 0xF0 ∧ b.toNat < 0x90)) then ⟨off + i, .overlong⟩
          else if i = 1 ∧ a.toNat = 0xED ∧ b.toNat > 0x9F then ⟨off + i, .surrogate⟩
          else if i = 1 ∧ a.toNat = 0xF4 ∧ b.toNat > 0x8F then ⟨off + i, .tooLarge⟩
          else check (i + 1) k bs
      check 1 n rest

/-- Prepend a decoded scalar, preserving a refusal unchanged. -/
def prepend (c : Nat) : Except DecodeError (List Nat) → Except DecodeError (List Nat)
  | .ok cs => .ok (c :: cs)
  | .error e => .error e

/-- Decode by structural recursion on the unconsumed byte list. -/
def decodeAt (off : Nat) : List UInt8 → Except DecodeError (List Nat)
  | [] => .ok []
  | a :: bs =>
    if Utf8.One a then prepend a.toNat (decodeAt (off + 1) bs)
    else match bs with
    | [] => .error (badSequence off [a])
    | b :: bs =>
      if Utf8.Two a b then
        prepend ((a.toNat - 0xC0) * 64 + (b.toNat - 0x80)) (decodeAt (off + 2) bs)
      else match bs with
      | [] => .error (badSequence off [a, b])
      | c :: bs =>
        if Utf8.Three a b c then
          prepend ((a.toNat - 0xE0) * 4096 + (b.toNat - 0x80) * 64 + (c.toNat - 0x80))
            (decodeAt (off + 3) bs)
        else match bs with
        | [] => .error (badSequence off [a, b, c])
        | d :: bs =>
          if Utf8.Four a b c d then
            prepend ((a.toNat - 0xF0) * 262144 + (b.toNat - 0x80) * 4096 +
              (c.toNat - 0x80) * 64 + (d.toNat - 0x80)) (decodeAt (off + 4) bs)
          else .error (badSequence off (a :: b :: c :: d :: bs))

/-- Decode a source byte list, starting at byte zero. -/
def decodeUtf8 (bs : List UInt8) : Except DecodeError (List Nat) := decodeAt 0 bs

/-- Successful prepending exposes a successful tail and its scalar list. -/
theorem prepend_ok {c : Nat} {r : Except DecodeError (List Nat)} {cs : List Nat} :
    prepend c r = .ok cs → ∃ ds, r = .ok ds ∧ cs = c :: ds := by
  cases r with
  | error e => intro h; cases h
  | ok ds => intro h; exact ⟨ds, rfl, (Except.ok.inj h).symm⟩

/-- Every success of the offset-carrying decoder satisfies the independent judgment. -/
theorem decodeAt_sound (bs : List UInt8) (off : Nat) (cs : List Nat)
    (h : decodeAt off bs = .ok cs) : Utf8.Decodes bs cs := by
  induction off, bs using decodeAt.induct generalizing cs with
  | case1 off =>
    simp only [decodeAt.eq_def, Except.ok.injEq] at h
    subst cs
    exact Utf8.Decodes.nil
  | case2 off a bs ha ih =>
    rw [decodeAt.eq_def] at h
    simp only [ha, ↓reduceIte] at h
    obtain ⟨ds, hd, rfl⟩ := prepend_ok h
    exact Utf8.Decodes.cons (show Utf8.Encodes a.toNat [a] from ⟨ha, rfl⟩) (ih ds hd)
  | case3 off a ha =>
    rw [decodeAt.eq_def] at h
    simp [ha] at h
  | case4 off a ha b bs hb ih =>
    rw [decodeAt.eq_def] at h
    simp only [ha, hb, ↓reduceIte] at h
    obtain ⟨ds, hd, rfl⟩ := prepend_ok h
    exact Utf8.Decodes.cons (show Utf8.Encodes _ [a, b] from ⟨hb, rfl⟩) (ih ds hd)
  | case5 off a ha b hb =>
    rw [decodeAt.eq_def] at h
    simp [ha, ↓reduceIte, hb] at h
  | case6 off a ha b hb c bs hc ih =>
    rw [decodeAt.eq_def] at h
    simp only [ha, hb, hc, ↓reduceIte] at h
    obtain ⟨ds, hd, rfl⟩ := prepend_ok h
    exact Utf8.Decodes.cons (show Utf8.Encodes _ [a, b, c] from ⟨hc, rfl⟩) (ih ds hd)
  | case7 off a ha b hb c hc =>
    rw [decodeAt.eq_def] at h
    simp [ha, ↓reduceIte, hb, hc] at h
  | case8 off a ha b hb c hc d bs hd ih =>
    rw [decodeAt.eq_def] at h
    simp only [ha, hb, hc, hd, ↓reduceIte] at h
    obtain ⟨ds, he, rfl⟩ := prepend_ok h
    exact Utf8.Decodes.cons (show Utf8.Encodes _ [a, b, c, d] from ⟨hd, rfl⟩) (ih ds he)
  | case9 off a ha b hb c hc d bs hd =>
    rw [decodeAt.eq_def] at h
    simp [ha, ↓reduceIte, hb, hc, hd] at h

/-- Every well-formed concatenation is accepted, at every starting offset. -/
theorem decodeAt_complete {bs : List UInt8} {cs : List Nat} (h : Utf8.Decodes bs cs)
    (off : Nat) : decodeAt off bs = .ok cs := by
  induction h generalizing off with
  | nil => rfl
  | @cons s chunk bs cs enc tail ih =>
    match chunk with
    | [] => simp [Utf8.Encodes] at enc
    | [a] =>
      change Utf8.One a ∧ _ at enc
      obtain ⟨ha, he⟩ := enc
      subst s
      simp only [List.cons_append, List.nil_append]
      rw [decodeAt.eq_def]
      simp only [ha, ↓reduceIte, ih, prepend]
    | [a, b] =>
      change Utf8.Two a b ∧ _ at enc
      obtain ⟨hab, he⟩ := enc
      subst s
      have ha : ¬ Utf8.One a := by simp only [Utf8.one_iff, Utf8.two_iff] at *; omega
      simp only [List.cons_append, List.nil_append]
      rw [decodeAt.eq_def]
      simp only [ha, ↓reduceIte, hab, ih, prepend]
    | [a, b, c] =>
      change Utf8.Three a b c ∧ _ at enc
      obtain ⟨habc, he⟩ := enc
      subst s
      have ha : ¬ Utf8.One a := by simp only [Utf8.one_iff, Utf8.three_iff] at *; omega
      have hb : ¬ Utf8.Two a b := by simp only [Utf8.two_iff, Utf8.three_iff] at *; omega
      simp only [List.cons_append, List.nil_append]
      rw [decodeAt.eq_def]
      simp only [ha, ↓reduceIte, hb, habc, ih, prepend]
    | [a, b, c, d] =>
      change Utf8.Four a b c d ∧ _ at enc
      obtain ⟨habcd, he⟩ := enc
      subst s
      have ha : ¬ Utf8.One a := by simp only [Utf8.one_iff, Utf8.four_iff] at *; omega
      have hb : ¬ Utf8.Two a b := by simp only [Utf8.two_iff, Utf8.four_iff] at *; omega
      have hc : ¬ Utf8.Three a b c := by simp only [Utf8.three_iff, Utf8.four_iff] at *; omega
      simp only [List.cons_append, List.nil_append]
      rw [decodeAt.eq_def]
      simp only [ha, ↓reduceIte, hb, hc, habcd, ih, prepend]
    | _ :: _ :: _ :: _ :: _ :: _ => simp [Utf8.Encodes] at enc

/-- Successful decoding is sound. -/
theorem decodeUtf8_sound {bs : List UInt8} {cs : List Nat}
    (h : decodeUtf8 bs = .ok cs) : Utf8.Decodes bs cs := decodeAt_sound bs 0 cs h

/-- Every declarative decoding is returned by the decoder. -/
theorem decodeUtf8_complete {bs : List UInt8} {cs : List Nat}
    (h : Utf8.Decodes bs cs) : decodeUtf8 bs = .ok cs := decodeAt_complete h 0

/-- Refuse NUL or BOM before inspecting the remaining source. -/
def sourcePrepend (off c : Nat) (r : Except DecodeError (List Nat)) :
    Except DecodeError (List Nat) :=
  if c = 0 then .error ⟨off, .nul⟩
  else if c = 0xFEFF then .error ⟨off, .bom⟩
  else prepend c r

/-- Decode and enforce the two source refusals in byte order. -/
def sourceAt (off : Nat) : List UInt8 → Except DecodeError (List Nat)
  | [] => .ok []
  | a :: bs =>
    if Utf8.One a then sourcePrepend off a.toNat (sourceAt (off + 1) bs)
    else match bs with
    | [] => .error (badSequence off [a])
    | b :: bs =>
      if Utf8.Two a b then
        sourcePrepend off ((a.toNat - 0xC0) * 64 + (b.toNat - 0x80)) (sourceAt (off + 2) bs)
      else match bs with
      | [] => .error (badSequence off [a, b])
      | c :: bs =>
        if Utf8.Three a b c then
          sourcePrepend off ((a.toNat - 0xE0) * 4096 + (b.toNat - 0x80) * 64 + (c.toNat - 0x80))
            (sourceAt (off + 3) bs)
        else match bs with
        | [] => .error (badSequence off [a, b, c])
        | d :: bs =>
          if Utf8.Four a b c d then
            sourcePrepend off ((a.toNat - 0xF0) * 262144 + (b.toNat - 0x80) * 4096 +
              (c.toNat - 0x80) * 64 + (d.toNat - 0x80)) (sourceAt (off + 4) bs)
          else .error (badSequence off (a :: b :: c :: d :: bs))

/-- Source checking from byte zero. -/
def checkSource (bs : List UInt8) : Except DecodeError (List Nat) := sourceAt 0 bs

/-- Successful source prepending exposes both the scalar restrictions and the tail. -/
theorem sourcePrepend_ok {off c : Nat} {r : Except DecodeError (List Nat)} {cs : List Nat}
    (h : sourcePrepend off c r = .ok cs) :
    c ≠ 0 ∧ c ≠ 0xFEFF ∧ ∃ ds, r = .ok ds ∧ cs = c :: ds := by
  unfold sourcePrepend at h
  split at h
  · cases h
  · rename_i hn
    split at h
    · cases h
    · rename_i hb
      exact ⟨hn, hb, prepend_ok h⟩

/-- Source checking succeeds exactly for a decoding without NUL or BOM. -/
theorem sourceAt_iff (off : Nat) (bs : List UInt8) (cs : List Nat) :
    sourceAt off bs = .ok cs ↔
      decodeAt off bs = .ok cs ∧ ∀ c ∈ cs, c ≠ 0 ∧ c ≠ 0xFEFF := by
  induction off, bs using sourceAt.induct generalizing cs with
  | case1 off =>
    simp only [sourceAt, decodeAt]
    constructor
    · intro h
      cases Except.ok.inj h
      exact ⟨rfl, by simp⟩
    · intro h; exact h.1
  | case2 off a bs ha ih =>
    rw [sourceAt.eq_def, decodeAt.eq_def]
    simp only [ha, ↓reduceIte]
    constructor
    · intro h
      obtain ⟨hn, hb, ds, hd, rfl⟩ := sourcePrepend_ok h
      obtain ⟨ht, hs⟩ := (ih ds).mp hd
      exact ⟨by rw [ht]; rfl, List.forall_mem_cons.mpr ⟨⟨hn, hb⟩, hs⟩⟩
    · rintro ⟨h, hs⟩
      obtain ⟨ds, hd, rfl⟩ := prepend_ok h
      have hp := (List.forall_mem_cons.mp hs)
      have ht := (ih ds).mpr ⟨hd, hp.2⟩
      simp only [sourcePrepend, hp.1.1, hp.1.2, ↓reduceIte, ht, prepend]
  | case3 off a ha =>
    rw [sourceAt.eq_def, decodeAt.eq_def]
    simp only [ha, ↓reduceIte]
    simp
  | case4 off a ha b bs hb ih =>
    rw [sourceAt.eq_def, decodeAt.eq_def]
    simp only [ha, hb, ↓reduceIte]
    constructor
    · intro h
      obtain ⟨hn, hb, ds, hd, rfl⟩ := sourcePrepend_ok h
      obtain ⟨ht, hs⟩ := (ih ds).mp hd
      exact ⟨by rw [ht]; rfl, List.forall_mem_cons.mpr ⟨⟨hn, hb⟩, hs⟩⟩
    · rintro ⟨h, hs⟩
      obtain ⟨ds, hd, rfl⟩ := prepend_ok h
      have hp := (List.forall_mem_cons.mp hs)
      have ht := (ih ds).mpr ⟨hd, hp.2⟩
      simp only [sourcePrepend, hp.1.1, hp.1.2, ↓reduceIte, ht, prepend]
  | case5 off a ha b hb =>
    rw [sourceAt.eq_def, decodeAt.eq_def]
    simp only [ha, hb, ↓reduceIte]
    simp
  | case6 off a ha b hb c bs hc ih =>
    rw [sourceAt.eq_def, decodeAt.eq_def]
    simp only [ha, hb, hc, ↓reduceIte]
    constructor
    · intro h
      obtain ⟨hn, hb, ds, hd, rfl⟩ := sourcePrepend_ok h
      obtain ⟨ht, hs⟩ := (ih ds).mp hd
      exact ⟨by rw [ht]; rfl, List.forall_mem_cons.mpr ⟨⟨hn, hb⟩, hs⟩⟩
    · rintro ⟨h, hs⟩
      obtain ⟨ds, hd, rfl⟩ := prepend_ok h
      have hp := (List.forall_mem_cons.mp hs)
      have ht := (ih ds).mpr ⟨hd, hp.2⟩
      simp only [sourcePrepend, hp.1.1, hp.1.2, ↓reduceIte, ht, prepend]
  | case7 off a ha b hb c hc =>
    rw [sourceAt.eq_def, decodeAt.eq_def]
    simp only [ha, hb, hc, ↓reduceIte]
    simp
  | case8 off a ha b hb c hc d bs hd ih =>
    rw [sourceAt.eq_def, decodeAt.eq_def]
    simp only [ha, hb, hc, hd, ↓reduceIte]
    constructor
    · intro h
      obtain ⟨hn, hb, ds, hd, rfl⟩ := sourcePrepend_ok h
      obtain ⟨ht, hs⟩ := (ih ds).mp hd
      exact ⟨by rw [ht]; rfl, List.forall_mem_cons.mpr ⟨⟨hn, hb⟩, hs⟩⟩
    · rintro ⟨h, hs⟩
      obtain ⟨ds, hd, rfl⟩ := prepend_ok h
      have hp := (List.forall_mem_cons.mp hs)
      have ht := (ih ds).mpr ⟨hd, hp.2⟩
      simp only [sourcePrepend, hp.1.1, hp.1.2, ↓reduceIte, ht, prepend]
  | case9 off a ha b hb c hc d bs hd =>
    rw [sourceAt.eq_def, decodeAt.eq_def]
    simp only [ha, hb, hc, hd, ↓reduceIte]
    simp

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

/-! ## Kernel-checked conformance cases -/

section Conformance

/-- Equality of decoder results for kernel-evaluated conformance cases. -/
local instance : DecidableEq (Except DecodeError (List Nat)) := by
  intro x y
  cases x with
  | error e => cases y with
    | error f => exact decidable_of_iff (e = f) ⟨congrArg Except.error, Except.error.inj⟩
    | ok _ => exact isFalse (by intro h; cases h)
  | ok cs => cases y with
    | error _ => exact isFalse (by intro h; cases h)
    | ok ds => exact decidable_of_iff (cs = ds) ⟨congrArg Except.ok, Except.ok.inj⟩

example : decodeUtf8 [65, 95, 57] = .ok [65, 95, 57] := by rfl
example : decodeUtf8 [0xC2, 0xA2] = .ok [0xA2] := by rfl
example : decodeUtf8 [0xE2, 0x82, 0xAC] = .ok [0x20AC] := by rfl
example : decodeUtf8 [0xF0, 0x9F, 0x98, 0x80] = .ok [0x1F600] := by rfl
example : decodeUtf8 [0xC0, 0x80] = .error ⟨0, .overlong⟩ := by rfl
example : decodeUtf8 [0xED, 0xA0, 0x80] = .error ⟨1, .surrogate⟩ := by rfl
example : decodeUtf8 [0xF4, 0x90, 0x80, 0x80] = .error ⟨1, .tooLarge⟩ := by rfl
example : decodeUtf8 [0xE2, 0x82] = .error ⟨2, .truncated⟩ := by rfl
example : decodeUtf8 [0x80] = .error ⟨0, .unexpectedContinuation⟩ := by rfl
example : checkSource [0] = .error ⟨0, .nul⟩ := by rfl
example : checkSource [0xEF, 0xBB, 0xBF] = .error ⟨0, .bom⟩ := by
  rw [checkSource, sourceAt.eq_def]
  simp only [show ¬ Utf8.One (0xEF : UInt8) by decide,
    show ¬ Utf8.Two (0xEF : UInt8) 0xBB by decide,
    show Utf8.Three (0xEF : UInt8) 0xBB 0xBF by decide, ↓reduceIte]
  have hv : ((0xEF : UInt8).toNat - 0xE0) * 4096 +
      ((0xBB : UInt8).toNat - 0x80) * 64 + ((0xBF : UInt8).toNat - 0x80) = 0xFEFF := by decide
  rw [hv]
  simp only [sourcePrepend, ↓reduceIte, show (0xFEFF : Nat) ≠ 0 by decide]
example : checkSource [65, 0] = .error ⟨1, .nul⟩ := by rfl
example : checkSource [65, 0xEF, 0xBB, 0xBF] = .error ⟨1, .bom⟩ := by rfl
example : checkSource [0, 0x80] = .error ⟨0, .nul⟩ := by rfl
example : checkSource [0xC2, 0xA2, 0] = .error ⟨2, .nul⟩ := by
  rw [checkSource, sourceAt.eq_def]
  simp only [show ¬ Utf8.One (0xC2 : UInt8) by decide,
    show Utf8.Two (0xC2 : UInt8) 0xA2 by decide, ↓reduceIte]
  have hv : ((0xC2 : UInt8).toNat - 0xC0) * 64 + ((0xA2 : UInt8).toNat - 0x80) = 0xA2 := by decide
  rw [hv]
  simp only [sourcePrepend, show (0xA2 : Nat) ≠ 0 by decide,
    show (0xA2 : Nat) ≠ 0xFEFF by decide, ↓reduceIte]
  have ht : sourceAt 2 [0] = .error ⟨2, .nul⟩ := rfl
  rw [ht]
  rfl
example : checkSource [65] = .ok [65] := by rfl
example : identStart? 0xE9 = false ∧ identContinue? 0xE9 = false := by decide

example : lineComment [47,47,32,120,10,114,101,115,116] =
    some [10,114,101,115,116] := by rfl
example : lineComment [47,47,32,120,13] = some [13] := by rfl
example : lineComment [47,47,32,120] = some [] := by rfl
example : lineComment [47,42,32,120,32,42,47] = none := by rfl

end Conformance
end Concrete.Lex
