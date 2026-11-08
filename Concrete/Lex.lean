import ConcreteSpec.Spec.Surface0

/-!
# Settled lexer components

Total byte-list UTF-8 decoding and source checking, ASCII identifiers, the four settled whitespace
scalars, line comments and maximal trivia scanning. String bodies implement only the six decided
escapes and parameterize all other raw scalars. Longest match uses an arbitrary table, and decimal
integer lexemes refuse leading zeroes. No keyword, reserved-word, builtin, concrete operator or
punctuation set, literal suffix, other literal rule, shebang handling or block comment is introduced;
no other Unicode whitespace is included. Error constructors are internal data, not diagnostic
wording. Offsets are zero-based byte offsets;
truncation points at EOF. TODO: prove the first non-extendible-prefix property of error offsets.
The success correspondence theorems below do not claim error-position correctness.
-/

set_option maxRecDepth 2048

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
      obtain ⟨ha, rfl⟩ := enc
      simp only [List.cons_append, List.nil_append]
      rw [decodeAt.eq_def]
      simp only [ha, ↓reduceIte, ih, prepend]
    | [a, b] =>
      obtain ⟨hab, rfl⟩ := enc
      have ha : ¬ Utf8.One a := by unfold Utf8.One; unfold Utf8.Two at hab; omega
      simp only [List.cons_append, List.nil_append]
      rw [decodeAt.eq_def]
      simp only [ha, ↓reduceIte, hab, ih, prepend]
    | [a, b, c] =>
      obtain ⟨habc, rfl⟩ := enc
      have ha : ¬ Utf8.One a := by unfold Utf8.One; unfold Utf8.Three at habc; omega
      have hb : ¬ Utf8.Two a b := by unfold Utf8.Two; unfold Utf8.Three at habc; omega
      simp only [List.cons_append, List.nil_append]
      rw [decodeAt.eq_def]
      simp only [ha, ↓reduceIte, hb, habc, ih, prepend]
    | [a, b, c, d] =>
      obtain ⟨habcd, rfl⟩ := enc
      have ha : ¬ Utf8.One a := by unfold Utf8.One; unfold Utf8.Four at habcd; omega
      have hb : ¬ Utf8.Two a b := by unfold Utf8.Two; unfold Utf8.Four at habcd; omega
      have hc : ¬ Utf8.Three a b c := by unfold Utf8.Three; unfold Utf8.Four at habcd; omega
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

/-- The independent decoding judgment is deterministic. -/
theorem decodes_unique {bs : List UInt8} {cs ds : List Nat}
    (hc : Utf8.Decodes bs cs) (hd : Utf8.Decodes bs ds) : cs = ds := by
  have := (decodeUtf8_complete hc).symm.trans (decodeUtf8_complete hd)
  exact Except.ok.inj this

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
    (hd : Utf8.Decodes bs cs) (hs : SourceOk bs) : checkSource bs = .ok cs := by
  obtain ⟨ds, hds, good⟩ := hs
  have eq := decodes_unique hds hd
  subst ds
  exact (sourceAt_iff 0 bs cs).mpr ⟨decodeUtf8_complete hd, good⟩

/-- Every encoded value is a Unicode scalar; the range judgment excludes surrogates. -/
theorem encodes_scalar {c : Nat} {bs : List UInt8} (h : Utf8.Encodes c bs) :
    Utf8.Scalar c := by
  unfold Utf8.Encodes at h
  split at h <;> simp only [Utf8.One, Utf8.Two, Utf8.Three, Utf8.Four] at h <;>
    unfold Utf8.Scalar <;> omega

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

/-- Scan a comment body, retaining its first LF or CR in the remainder. -/
def scanLine : List Nat → List Nat × List Nat
  | [] => ([], [])
  | c :: cs =>
    if c = 10 ∨ c = 13 then ([], c :: cs)
    else let (body, rest) := scanLine cs; (c :: body, rest)

/-- Scan a line comment only when the input begins with two slashes. -/
def lineComment : List Nat → Option (List Nat × List Nat)
  | 47 :: 47 :: cs =>
      let (body, rest) := scanLine cs
      some (47 :: 47 :: body, rest)
  | _ => none

/-- Scanning a body satisfies the declarative stopping rule. -/
theorem scanLine_sound (cs : List Nat) :
    CommentBody cs (scanLine cs).1 (scanLine cs).2 := by
  induction cs with
  | nil => exact .eof
  | cons c cs ih =>
    by_cases hl : c = 10
    · subst c; simpa [scanLine] using CommentBody.lf cs
    · by_cases hr : c = 13
      · subst c; simpa [scanLine] using CommentBody.cr cs
      · simpa [scanLine, hl, hr] using CommentBody.cons hl hr ih

/-- Every declarative body decomposition is the scanner's result. -/
theorem scanLine_complete {cs body rest : List Nat} (h : CommentBody cs body rest) :
    scanLine cs = (body, rest) := by
  induction h with
  | eof => rfl
  | lf rest => simp [scanLine]
  | cr rest => simp [scanLine]
  | cons hl hr _ ih => simp [scanLine, hl, hr, ih]

/-- Successful line-comment scanning satisfies the independent judgment. -/
theorem lineComment_sound {cs : List Nat} {out : List Nat × List Nat}
    (h : lineComment cs = some out) : LineComment cs out := by
  unfold lineComment at h
  split at h
  · rename_i body
    cases Option.some.inj h
    exact .intro (scanLine_sound body)
  · cases h

/-- Every declarative line comment is recognized, with the same body and remainder. -/
theorem lineComment_complete {cs : List Nat} {out : List Nat × List Nat}
    (h : LineComment cs out) : lineComment cs = some out := by
  cases h with
  | intro hb => simp [lineComment, scanLine_complete hb]

/-- A comment body's remainder is no longer than its input. -/
theorem commentBody_rest_length_le {input body rest : List Nat}
    (h : CommentBody input body rest) : rest.length ≤ input.length := by
  induction h with
  | eof => simp
  | lf rest => simp
  | cr rest => simp
  | cons _ _ _ ih => simp only [List.length_cons]; omega

/-- Scanning a line comment consumes at least its two slashes. -/
theorem lineComment_rest_length_lt {input comment rest : List Nat}
    (h : LineComment input (comment, rest)) : rest.length < input.length := by
  cases h with
  | intro hb =>
    have hle := commentBody_rest_length_le hb
    simp only [List.length_cons]
    omega

/-- Structural scanner used with fuel at least the input length. -/
def skipTriviaAux : Nat → List Nat → List Nat × List Nat
  | _, [] => ([], [])
  | 0, cs => ([], cs)
  | fuel + 1, c :: input =>
    if _h : Whitespace c then
      let out := skipTriviaAux fuel input
      (c :: out.1, out.2)
    else
      match _hline : lineComment (c :: input) with
      | some (comment, rest) =>
        let out := skipTriviaAux fuel rest
        (comment ++ out.1, out.2)
      | none => ([], c :: input)

/-- Skip a maximal run of whitespace scalars and line comments. -/
def skipTrivia (cs : List Nat) : List Nat × List Nat := skipTriviaAux cs.length cs

/-- With enough fuel, the structural scanner satisfies the maximal-trivia judgment. -/
theorem skipTriviaAux_sound (cs : List Nat) (fuel : Nat) (hfuel : cs.length ≤ fuel) :
    Trivia cs (skipTriviaAux fuel cs) := by
  induction fuel generalizing cs with
  | zero =>
    cases cs with
    | nil =>
      rw [skipTriviaAux.eq_def]
      exact .stop [] (by intro c cs h; cases h) (by intro cs h; cases h)
    | cons c input => simp at hfuel
  | succ fuel ih =>
    cases cs with
    | nil =>
      rw [skipTriviaAux.eq_def]
      exact .stop [] (by intro c cs h; cases h) (by intro cs h; cases h)
    | cons c input =>
      by_cases hw : Whitespace c
      · rw [skipTriviaAux.eq_def]
        simp only [hw]
        apply Trivia.whitespace hw
        apply ih
        simp only [List.length_cons] at hfuel
        omega
      · cases hline : lineComment (c :: input) with
        | none =>
          rw [skipTriviaAux.eq_def]
          simp only [hw]
          rw [hline]
          apply Trivia.stop
          · intro d ds heq
            cases heq
            exact hw
          · intro tail heq
            cases heq
            simp [lineComment] at hline
        | some out =>
          cases out with
          | mk comment rest =>
            rw [skipTriviaAux.eq_def]
            simp only [hw]
            rw [hline]
            apply Trivia.comment (lineComment_sound hline)
            apply ih
            have hlt := lineComment_rest_length_lt (lineComment_sound hline)
            simp only [List.length_cons] at hfuel hlt
            omega

/-- With enough fuel, every declarative decomposition is the structural scanner's result. -/
theorem skipTriviaAux_complete {cs : List Nat} {out : List Nat × List Nat}
    (h : Trivia cs out) (fuel : Nat) (hfuel : cs.length ≤ fuel) :
    skipTriviaAux fuel cs = out := by
  induction h generalizing fuel with
  | stop rest hw hc =>
    cases rest with
    | nil => cases fuel <;> rfl
    | cons c input =>
      have hline : lineComment (c :: input) = none := by
        unfold lineComment
        split
        · rename_i tail heq
          exact (hc tail heq).elim
        · rfl
      cases fuel with
      | zero => simp at hfuel
      | succ fuel =>
        rw [skipTriviaAux.eq_def]
        simp only [hw c input rfl]
        rw [hline]
        simp
  | whitespace hw _ ih =>
    cases fuel with
    | zero => simp at hfuel
    | succ fuel =>
      rw [skipTriviaAux.eq_def]
      simp only [hw]
      simp
      have hrec := ih fuel (by simp only [List.length_cons] at hfuel; omega)
      exact ⟨congrArg Prod.fst hrec, congrArg Prod.snd hrec⟩
  | comment hl _ ih =>
    cases hl with
    | intro hb =>
      cases fuel with
      | zero => simp at hfuel
      | succ fuel =>
        have hline := lineComment_complete (LineComment.intro hb)
        rw [skipTriviaAux.eq_def]
        simp only [show ¬ Whitespace 47 from by decide]
        rw [hline]
        simp
        have hlt := lineComment_rest_length_lt (LineComment.intro hb)
        have hrec := ih fuel (by simp only [List.length_cons] at hfuel hlt; omega)
        exact ⟨congrArg Prod.fst hrec, congrArg Prod.snd hrec⟩

/-- The scanner's result satisfies the independent maximal-trivia judgment. -/
theorem skipTrivia_sound (cs : List Nat) : Trivia cs (skipTrivia cs) :=
  skipTriviaAux_sound cs cs.length (Nat.le_refl _)

/-- Every declarative maximal-trivia decomposition is the scanner's result. -/
theorem skipTrivia_complete {cs : List Nat} {out : List Nat × List Nat}
    (h : Trivia cs out) : skipTrivia cs = out :=
  skipTriviaAux_complete h cs.length (Nat.le_refl _)

/-- Maximal-trivia decompositions are unique. -/
theorem trivia_unique {cs : List Nat} {out₁ out₂ : List Nat × List Nat}
    (h₁ : Trivia cs out₁) (h₂ : Trivia cs out₂) : out₁ = out₂ := by
  rw [← skipTrivia_complete h₁, ← skipTrivia_complete h₂]

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

/-- Decode exactly the six settled escapes. -/
def decodeEscape : Nat → Option Nat
  | 110 => some 10
  | 116 => some 9
  | 114 => some 13
  | 92 => some 92
  | 34 => some 34
  | 48 => some 0
  | _ => none

/-- Every scanner-approved escape is in the declarative escape relation. -/
theorem decodeEscape_sound {escaped decoded : Nat} (h : decodeEscape escaped = some decoded) :
    StringEscape escaped decoded := by
  unfold decodeEscape at h
  split at h <;> simp_all [StringEscape]

/-- Every declarative escape is decoded to its settled scalar. -/
theorem decodeEscape_complete {escaped decoded : Nat} (h : StringEscape escaped decoded) :
    decodeEscape escaped = some decoded := by
  rcases h with (⟨rfl, rfl⟩ | ⟨rfl, rfl⟩ | ⟨rfl, rfl⟩ | ⟨rfl, rfl⟩ | ⟨rfl, rfl⟩ | ⟨rfl, rfl⟩)
  all_goals rfl

/-- Prepend a decoded scalar to a successful string-body result. -/
def stringPrepend (c : Nat) : Option (List Nat × List Nat) → Option (List Nat × List Nat)
  | none => none
  | some (decoded, rest) => some (c :: decoded, rest)

/-- Scan a string body after its opening quote. `rawOk` remains an explicit parameter and does
not choose whether any other raw control scalar is accepted. -/
def stringBody (rawOk : Nat → Prop) [DecidablePred rawOk] :
    List Nat → Option (List Nat × List Nat)
  | [] => none
  | 34 :: rest => some ([], rest)
  | 92 :: escaped :: input =>
    match decodeEscape escaped with
    | some decoded => stringPrepend decoded (stringBody rawOk input)
    | none => none
  | 92 :: [] => none
  | 10 :: _ => none
  | 13 :: _ => none
  | c :: input => if rawOk c then stringPrepend c (stringBody rawOk input) else none

/-- Successful prepending exposes the successful tail and decoded list. -/
theorem stringPrepend_ok {c : Nat} {r : Option (List Nat × List Nat)}
    {out : List Nat × List Nat} (h : stringPrepend c r = some out) :
    ∃ decoded rest, r = some (decoded, rest) ∧ out = (c :: decoded, rest) := by
  cases r with
  | none => simp [stringPrepend] at h
  | some tail =>
    cases tail with
    | mk decoded rest =>
      cases Option.some.inj h
      exact ⟨decoded, rest, rfl, rfl⟩

/-- Every scanner result satisfies the independent string-body judgment for the same `rawOk`. -/
theorem stringBody_sound (rawOk : Nat → Prop) [DecidablePred rawOk]
    {cs : List Nat} {out : List Nat × List Nat}
    (h : stringBody rawOk cs = some out) : StringBody rawOk cs out := by
  induction cs using stringBody.induct (rawOk := rawOk) generalizing out with
  | case1 => simp [stringBody] at h
  | case2 rest =>
    cases Option.some.inj h
    exact .close rest
  | case3 escaped input decodedScalar he ih =>
    simp only [stringBody, he] at h
    obtain ⟨decoded, rest, ht, rfl⟩ := stringPrepend_ok h
    exact .escape (decodeEscape_sound he) (ih ht)
  | case4 escaped input he => simp [stringBody, he] at h
  | case5 => simp [stringBody] at h
  | case6 rest => simp [stringBody] at h
  | case7 rest => simp [stringBody] at h
  | case8 c input hq hb hl hr hc hraw ih =>
    simp only [stringBody, hraw, ↓reduceIte] at h
    obtain ⟨decoded, rest, ht, rfl⟩ := stringPrepend_ok h
    have hb' : c ≠ 92 := by
      intro heq
      cases input with
      | nil => exact hl heq rfl
      | cons escaped tail => exact hb escaped tail heq rfl
    exact .raw hq hb' hr hc hraw (ih ht)
  | case9 c input hq hb hl hr hc hraw => simp [stringBody, hraw] at h

/-- Every declarative string body is recognized with the same decoding and remainder. -/
theorem stringBody_complete (rawOk : Nat → Prop) [DecidablePred rawOk]
    {cs : List Nat} {out : List Nat × List Nat}
    (h : StringBody rawOk cs out) : stringBody rawOk cs = some out := by
  induction h with
  | close rest => rfl
  | raw hq hb hl hr hc _ ih => simp [stringBody, hb, hc, ih, stringPrepend]
  | escape he _ ih => simp [stringBody, decodeEscape_complete he, ih, stringPrepend]

/-- String-body decompositions are unique for every raw-scalar parameter. -/
theorem stringBody_unique (rawOk : Nat → Prop) [DecidablePred rawOk]
    {cs : List Nat} {out₁ out₂ : List Nat × List Nat}
    (h₁ : StringBody rawOk cs out₁) (h₂ : StringBody rawOk cs out₂) : out₁ = out₂ := by
  have h := (stringBody_complete rawOk h₁).symm.trans (stringBody_complete rawOk h₂)
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

/-- Scan the maximal ASCII decimal-digit run. -/
def scanDigits : List Nat → List Nat × List Nat
  | [] => ([], [])
  | c :: cs =>
    if DecimalDigit c then let (digits, rest) := scanDigits cs; (c :: digits, rest)
    else ([], c :: cs)

/-- Scan an accepted decimal lexeme and its positional value. The remainder is only the suffix
left by maximal digit scanning; no suffix, base prefix, separator or type-range rule is added. -/
def decimalInteger (cs : List Nat) : Option (List Nat × Nat × List Nat) :=
  let out := scanDigits cs
  match out.1 with
  | [] => none
  | [48] => some (out.1, decimalValue out.1, out.2)
  | 48 :: _ => none
  | _ :: _ => some (out.1, decimalValue out.1, out.2)

/-- The digit scanner satisfies the independent maximal-run judgment. -/
theorem scanDigits_sound (cs : List Nat) :
    DigitRun cs (scanDigits cs).1 (scanDigits cs).2 := by
  induction cs with
  | nil => exact .stop [] (by intro c cs h; cases h)
  | cons c cs ih =>
    by_cases hc : DecimalDigit c
    · simpa [scanDigits, hc] using DigitRun.cons hc ih
    · simpa [scanDigits, hc] using
        DigitRun.stop (c :: cs) (by intro d ds h; cases h; exact hc)

/-- Every declarative digit decomposition is the scanner's result. -/
theorem scanDigits_complete {cs digits rest : List Nat} (h : DigitRun cs digits rest) :
    scanDigits cs = (digits, rest) := by
  induction h with
  | stop rest hc =>
    cases rest with
    | nil => rfl
    | cons c cs => simp [scanDigits, hc c cs rfl]
  | cons hc _ ih => simp [scanDigits, hc, ih]

/-- Successful decimal scanning satisfies the independent judgment. -/
theorem decimalInteger_sound {cs : List Nat} {out : List Nat × Nat × List Nat}
    (h : decimalInteger cs = some out) : DecimalInteger cs out := by
  unfold decimalInteger at h
  generalize heq : scanDigits cs = scanned at h
  cases scanned with
  | mk digits rest =>
    have hrun : DigitRun cs digits rest := by
      have hs := scanDigits_sound cs
      rw [heq] at hs
      simpa using hs
    cases digits with
    | nil => simp at h
    | cons d ds =>
      by_cases hd : d = 48
      · subst d
        cases ds with
        | nil =>
          cases h
          exact DecimalInteger.intro (digits := [48]) (rest := rest)
            (value := decimalValue [48]) hrun (by decide) rfl
        | cons e ds => simp at h
      · simp [hd] at h
        cases h
        exact .intro hrun (by simp [DigitsAccepted, hd]) rfl

/-- Every declarative decimal integer is recognized with the same lexeme, value and remainder. -/
theorem decimalInteger_complete {cs : List Nat} {out : List Nat × Nat × List Nat}
    (h : DecimalInteger cs out) : decimalInteger cs = some out := by
  cases h with
  | @intro digits rest value hrun hacc hval =>
    simp only [decimalInteger, scanDigits_complete hrun]
    cases digits with
    | nil => simp [DigitsAccepted] at hacc
    | cons d ds =>
      by_cases hd : d = 48
      · subst d
        cases ds with
        | nil => simp [hval]
        | cons e ds => simp [DigitsAccepted] at hacc
      · simp [hd, hval]

/-- Decimal-integer decompositions are unique. -/
theorem decimalInteger_unique {cs : List Nat} {out₁ out₂ : List Nat × Nat × List Nat}
    (h₁ : DecimalInteger cs out₁) (h₂ : DecimalInteger cs out₂) : out₁ = out₂ := by
  have h := (decimalInteger_complete h₁).symm.trans (decimalInteger_complete h₂)
  exact Option.some.inj h

/-! ## Kernel-checked conformance cases -/

section Conformance
-- The raised limit is for these `decide` cases only.
set_option maxRecDepth 100000

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
    some ([47,47,32,120], [10,114,101,115,116]) := by decide
example : lineComment [47,47,32,120,13] = some ([47,47,32,120], [13]) := by decide
example : lineComment [47,47,32,120] = some ([47,47,32,120], []) := by decide
example : lineComment [47,42,32,120,32,42,47] = none := by decide

example : skipTrivia [32,65] = ([32], [65]) := by decide
example : skipTrivia [9,65] = ([9], [65]) := by decide
example : skipTrivia [10,65] = ([10], [65]) := by decide
example : skipTrivia [13,65] = ([13], [65]) := by decide
example : skipTrivia [13,10,65] = ([13,10], [65]) := by decide
example : skipTrivia [47,47,120,10,65] = ([47,47,120,10], [65]) := by decide
example : skipTrivia [47,47,120,13,65] = ([47,47,120,13], [65]) := by decide
example : skipTrivia [47,47,120] = ([47,47,120], []) := by decide
example : skipTrivia [9,32,47,47,120,13,10,65] =
    ([9,32,47,47,120,13,10], [65]) := by decide

example : skipTrivia [47,42,32,42,47] = ([], [47,42,32,42,47]) := by decide
example : skipTrivia [35,33,65] = ([], [35,33,65]) := by decide
example : skipTrivia [0xA0,32] = ([], [0xA0,32]) := by decide
example : skipTrivia [0xFEFF,32] = ([], [0xFEFF,32]) := by decide
example : skipTrivia [47] = ([], [47]) := by decide
example : skipTrivia [65,32] = ([], [65,32]) := by decide

example : identifier [65,98,99,33] = some ([65,98,99], [33]) := by decide
example : identifier [97,98,99] = some ([97,98,99], []) := by decide
example : identifier [95,48,57] = some ([95,48,57], []) := by decide
example : ([65,98,99] : List Nat) ≠ [97,98,99] := by decide
example : identifier [48,98,99] = none := by decide
example : identifier [0xE9,65] = none := by decide

-- `False` is only a trivial test parameter that excludes raw scalars while exercising escapes
-- and refusals. These cases choose nothing about raw tabs or other raw control scalars.
example : stringBody (fun _ => False) [92,110,34,65] = some ([10], [65]) := by decide
example : stringBody (fun _ => False)
    [92,110,92,116,92,114,92,92,92,34,92,48,34,65] =
    some ([10,9,13,92,34,0], [65]) := by decide
example : stringBody (fun _ => False) [92,113,34] = none := by decide
example : stringBody (fun _ => False) [10,34] = none := by decide
example : stringBody (fun _ => False) [13,34] = none := by decide
example : stringBody (fun _ => False) [] = none := by decide
example : stringBody (fun _ => False) [92,110] = none := by decide
example : stringBody (fun _ => False) [92] = none := by decide

-- Arbitrary numeric lists only: this is deliberately not a Concrete token table.
example : longestMatch [[1], [1,2,3], [1,2], [9]] [1,2,4] = some ([1,2], [4]) := by decide
example : longestMatch [[1,2], [1,2]] [1,2,3] = some ([1,2], [3]) := by decide
example : longestMatch [[], [1,2], [3]] [1,9] = none := by decide
example : longestMatch [[1], []] [] = none := by decide

example : decimalInteger [48,65] = some ([48], 0, [65]) := by decide
example : decimalInteger [55] = some ([55], 7, []) := by decide
example : decimalInteger [50,48,48,32] = some ([50,48,48], 200, [32]) := by decide
example : decimalInteger [50,53,53] = some ([50,53,53], 255, []) := by decide
example : decimalInteger [48,48,55] = none := by decide
example : decimalInteger [48,48] = none := by decide
example : decimalInteger [65] = none := by decide

end Conformance

end Concrete.Lex
