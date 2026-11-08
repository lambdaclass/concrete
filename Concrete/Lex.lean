import ConcreteSpec.Spec.Surface0

/-!
# Settled lexer components

Total byte-list UTF-8 decoding and source checking, ASCII identifiers, the four settled whitespace
scalars, line comments and maximal trivia scanning. Strings and comments refuse raw Cc,
U+2028/U+2029 and bidi controls with internal errors; comments alone permit raw TAB. Byte literals
have printable-ASCII raw scalars and exactly the six decided escapes; labels are quote plus an
identifier. Longest match uses an arbitrary table. Integers implement decimal and the lowercase
`0x`/`0b`/`0o` bases, lowercase hexadecimal digits and eight suffixes. Malformed adjacent text is
one refused token, and decimal leading-zero refusal is retained. No keyword, reserved-word,
builtin, concrete operator or punctuation set, float literal, shebang handling or block comment is
introduced; no other Unicode whitespace is included. Error constructors are internal data, not
diagnostic wording. Offsets are zero-based byte offsets;
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

/-- Internal comment refusals; constructor names are not user-visible diagnostics. -/
inductive CommentError where
  | rawForbidden (c : Nat)
  deriving DecidableEq, Repr

/-- Prepend a scalar to a successful comment-body result. -/
def commentPrepend (c : Nat) : Except CommentError (List Nat × List Nat) →
    Except CommentError (List Nat × List Nat)
  | .error e => .error e
  | .ok (body, rest) => .ok (c :: body, rest)

/-- Scan a comment body, retaining its first LF or CR and refusing forbidden raw scalars. -/
def scanLine : List Nat → Except CommentError (List Nat × List Nat)
  | [] => .ok ([], [])
  | c :: cs =>
    if c = 10 ∨ c = 13 then .ok ([], c :: cs)
    else if CommentRaw c then commentPrepend c (scanLine cs)
    else .error (.rawForbidden c)

/-- Scan a line comment only when the input begins with two slashes. -/
def lineComment : List Nat → Except CommentError (Option (List Nat × List Nat))
  | 47 :: 47 :: cs =>
    match scanLine cs with
    | .ok (body, rest) => .ok (some (47 :: 47 :: body, rest))
    | .error e => .error e
  | _ => .ok none

/-- A successful body scan satisfies the declarative stopping and raw-scalar rules. -/
theorem scanLine_sound {cs : List Nat} {out : List Nat × List Nat}
    (h : scanLine cs = .ok out) : CommentBody cs out.1 out.2 := by
  induction cs generalizing out with
  | nil =>
    cases Except.ok.inj h
    exact .eof
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
          cases hrec : scanLine cs with
          | error e => rw [hrec] at h; cases h
          | ok tail =>
            cases tail with
            | mk body rest =>
              rw [hrec] at h
              cases Except.ok.inj h
              exact .cons hl hr hc (ih hrec)
        · simp [scanLine, hl, hr, hc] at h

/-- Every declarative body decomposition is the scanner's successful result. -/
theorem scanLine_complete {cs body rest : List Nat} (h : CommentBody cs body rest) :
    scanLine cs = .ok (body, rest) := by
  induction h with
  | eof => rfl
  | lf rest => simp [scanLine]
  | cr rest => simp [scanLine]
  | cons hl hr hc _ ih => simp [scanLine, hl, hr, hc, ih, commentPrepend]

/-- Successful line-comment scanning satisfies the independent judgment. -/
theorem lineComment_sound {cs : List Nat} {out : List Nat × List Nat}
    (h : lineComment cs = .ok (some out)) : LineComment cs out := by
  unfold lineComment at h
  split at h
  · rename_i input
    cases hscan : scanLine input with
    | error e => simp [hscan] at h
    | ok scanned =>
      cases scanned with
      | mk body rest =>
        simp only [hscan, Except.ok.injEq, Option.some.injEq] at h
        cases h
        exact .intro (scanLine_sound hscan)
  · cases h

/-- Every declarative line comment is recognized, with the same body and remainder. -/
theorem lineComment_complete {cs : List Nat} {out : List Nat × List Nat}
    (h : LineComment cs out) : lineComment cs = .ok (some out) := by
  cases h with
  | intro hb => simp [lineComment, scanLine_complete hb]

/-- A comment body's remainder is no longer than its input. -/
theorem commentBody_rest_length_le {input body rest : List Nat}
    (h : CommentBody input body rest) : rest.length ≤ input.length := by
  induction h with
  | eof => simp
  | lf rest => simp
  | cr rest => simp
  | cons _ _ _ _ ih => simp only [List.length_cons]; omega

/-- Scanning a line comment consumes at least its two slashes. -/
theorem lineComment_rest_length_lt {input comment rest : List Nat}
    (h : LineComment input (comment, rest)) : rest.length < input.length := by
  cases h with
  | intro hb =>
    have hle := commentBody_rest_length_le hb
    simp only [List.length_cons]
    omega

/-- Prepend whitespace to a successful trivia result. -/
def triviaPrepend (c : Nat) : Except CommentError (List Nat × List Nat) →
    Except CommentError (List Nat × List Nat)
  | .error e => .error e
  | .ok (trivia, rest) => .ok (c :: trivia, rest)

/-- Prepend a comment to a successful trivia result. -/
def triviaAppend (comment : List Nat) : Except CommentError (List Nat × List Nat) →
    Except CommentError (List Nat × List Nat)
  | .error e => .error e
  | .ok (trivia, rest) => .ok (comment ++ trivia, rest)

/-- Structural scanner used with fuel at least the input length. Errors in comments propagate. -/
def skipTriviaAux : Nat → List Nat → Except CommentError (List Nat × List Nat)
  | _, [] => .ok ([], [])
  | 0, cs => .ok ([], cs)
  | fuel + 1, c :: input =>
    if _h : Whitespace c then triviaPrepend c (skipTriviaAux fuel input)
    else
      match _hline : lineComment (c :: input) with
      | .ok (some (comment, rest)) => triviaAppend comment (skipTriviaAux fuel rest)
      | .ok none => .ok ([], c :: input)
      | .error e => .error e

/-- Skip a maximal run of whitespace scalars and valid line comments. -/
def skipTrivia (cs : List Nat) : Except CommentError (List Nat × List Nat) :=
  skipTriviaAux cs.length cs

/-- With enough fuel, a successful structural scan satisfies the maximal-trivia judgment. -/
theorem skipTriviaAux_sound {cs : List Nat} {out : List Nat × List Nat}
    (fuel : Nat) (hfuel : cs.length ≤ fuel) (h : skipTriviaAux fuel cs = .ok out) :
    Trivia cs out := by
  induction fuel generalizing cs out with
  | zero =>
    cases cs with
    | nil =>
      simp only [skipTriviaAux] at h
      cases Except.ok.inj h
      exact .stop [] (by intro c cs h; cases h) (by intro cs h; cases h)
    | cons c input => simp at hfuel
  | succ fuel ih =>
    cases cs with
    | nil =>
      simp only [skipTriviaAux] at h
      cases Except.ok.inj h
      exact .stop [] (by intro c cs h; cases h) (by intro cs h; cases h)
    | cons c input =>
      by_cases hw : Whitespace c
      · rw [skipTriviaAux.eq_def] at h
        simp only [hw] at h
        cases hrec : skipTriviaAux fuel input with
        | error e => rw [hrec] at h; cases h
        | ok tail =>
          cases tail with
          | mk trivia rest =>
            rw [hrec] at h
            cases Except.ok.inj h
            apply Trivia.whitespace hw
            apply ih
            · simp only [List.length_cons] at hfuel; omega
            · exact hrec
      · cases hline : lineComment (c :: input) with
        | error e =>
          rw [skipTriviaAux.eq_def] at h
          simp only [hw] at h
          rw [hline] at h
          simp at h
        | ok scanned =>
          cases scanned with
          | none =>
            rw [skipTriviaAux.eq_def] at h
            simp only [hw] at h
            rw [hline] at h
            simp at h
            cases h
            apply Trivia.stop
            · intro d ds heq
              cases heq
              exact hw
            · intro tail heq
              cases heq
              cases hs : scanLine tail with
              | error e => simp [lineComment, hs] at hline
              | ok out => simp [lineComment, hs] at hline
          | some commentOut =>
            cases commentOut with
            | mk comment rest =>
              rw [skipTriviaAux.eq_def] at h
              simp only [hw] at h
              rw [hline] at h
              simp at h
              cases hrec : skipTriviaAux fuel rest with
              | error e => rw [hrec] at h; cases h
              | ok tail =>
                cases tail with
                | mk trivia final =>
                  rw [hrec] at h
                  cases Except.ok.inj h
                  have hcomment := lineComment_sound hline
                  apply Trivia.comment hcomment
                  apply ih
                  · have hlt := lineComment_rest_length_lt hcomment
                    simp only [List.length_cons] at hfuel hlt
                    omega
                  · exact hrec

/-- With enough fuel, every declarative decomposition is the structural scanner's result. -/
theorem skipTriviaAux_complete {cs : List Nat} {out : List Nat × List Nat}
    (h : Trivia cs out) (fuel : Nat) (hfuel : cs.length ≤ fuel) :
    skipTriviaAux fuel cs = .ok out := by
  induction h generalizing fuel with
  | stop rest hw hc =>
    cases rest with
    | nil => cases fuel <;> rfl
    | cons c input =>
      have hline : lineComment (c :: input) = .ok none := by
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
        rfl
  | whitespace hw _ ih =>
    cases fuel with
    | zero => simp at hfuel
    | succ fuel =>
      have hrec := ih fuel (by simp only [List.length_cons] at hfuel; omega)
      rw [skipTriviaAux.eq_def]
      simp only [hw]
      rw [hrec]
      rfl
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
        simp only [show ¬ Whitespace 47 from by decide]
        rw [hline]
        simp [hrec, triviaAppend]

/-- A successful public trivia scan satisfies the independent maximal-trivia judgment. -/
theorem skipTrivia_sound {cs : List Nat} {out : List Nat × List Nat}
    (h : skipTrivia cs = .ok out) : Trivia cs out :=
  skipTriviaAux_sound cs.length (Nat.le_refl _) h

/-- Every declarative maximal-trivia decomposition is the scanner's result. -/
theorem skipTrivia_complete {cs : List Nat} {out : List Nat × List Nat}
    (h : Trivia cs out) : skipTrivia cs = .ok out :=
  skipTriviaAux_complete h cs.length (Nat.le_refl _)

/-- Maximal-trivia decompositions are unique. -/
theorem trivia_unique {cs : List Nat} {out₁ out₂ : List Nat × List Nat}
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
  | invalidRaw (c : Nat) | invalidEscape (escaped : Nat)
  deriving DecidableEq, Repr

/-- Scan exactly the decided byte literal. Other quote-led inputs are left for labels. -/
def byteLiteral : List Nat → Except ByteError (Option (List Nat × Nat × List Nat))
  | 39 :: 92 :: escaped :: 39 :: rest =>
    match decodeByteEscape escaped with
    | some decoded => .ok (some ([39, 92, escaped, 39], decoded, rest))
    | none => .error (.invalidEscape escaped)
  | 39 :: 92 :: escaped :: _ =>
    match decodeByteEscape escaped with
    | some _ => .ok none
    | none => .error (.invalidEscape escaped)
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
def decodeSuffix : List Nat → Option (Option ConcreteSpec.IntTy)
  | [] => some none
  | [105, 56] => some (some .i8)
  | [105, 49, 54] => some (some .i16)
  | [105, 50, 51] => some (some .i32)
  | [105, 54, 52] => some (some .i64)
  | [117, 56] => some (some .u8)
  | [117, 49, 54] => some (some .u16)
  | [117, 50, 51] => some (some .u32)
  | [117, 54, 52] => some (some .u64)
  | _ => none

/-- Every scanner-approved suffix is in the declarative suffix relation. -/
theorem decodeSuffix_sound {suffix : List Nat} {typeSuffix : Option ConcreteSpec.IntTy}
    (h : decodeSuffix suffix = some typeSuffix) : SuffixAccepted suffix typeSuffix := by
  unfold decodeSuffix at h
  split at h <;> simp_all [SuffixAccepted, intTySpelling]

/-- Every declarative suffix is decoded to its specified optional type. -/
theorem decodeSuffix_complete {suffix : List Nat} {typeSuffix : Option ConcreteSpec.IntTy}
    (h : SuffixAccepted suffix typeSuffix) : decodeSuffix suffix = some typeSuffix := by
  rcases h with (⟨rfl, rfl⟩ | ⟨rfl, rfl⟩ | ⟨rfl, rfl⟩ | ⟨rfl, rfl⟩ | ⟨rfl, rfl⟩ |
    ⟨rfl, rfl⟩ | ⟨rfl, rfl⟩ | ⟨rfl, rfl⟩ | ⟨rfl, rfl⟩) <;> rfl

/-- Internal integer-literal refusals; constructor names are not user-visible diagnostics. -/
inductive IntegerError where
  | missingDigits (radix : Radix) | leadingZero | invalidSuffix (suffix : List Nat)
  deriving DecidableEq, Repr

/-- Scan digits and the complete adjacent suffix candidate after a selected radix. -/
def integerDigits (radix : Radix) (input : List Nat) : Except IntegerError (Option IntegerToken) :=
  let digitOut := scanDigits radix input
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

/-- Successful based-digit scanning satisfies the independent integer judgment. -/
theorem integerDigits_sound {radix : Radix} {input : List Nat} {out : IntegerToken}
    (h : integerDigits radix input = .ok (some out)) :
    IntegerLiteral (radix.basePrefix ++ input) out := by
  unfold integerDigits at h
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
      simp only [hi] at h
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
            exact .intro rfl hrun hacc hident (decodeSuffix_sound hs)
        · simp [hacc] at h

/-- Every declarative based-digit decomposition is recognized by its radix scanner. -/
theorem integerDigits_complete {radix : Radix} {input digits suffixInput suffix rest : List Nat}
    {typeSuffix : Option ConcreteSpec.IntTy}
    (hrun : DigitRun radix input digits suffixInput) (hacc : DigitsAccepted radix digits)
    (hident : IdentBody suffixInput suffix rest) (hsuffix : SuffixAccepted suffix typeSuffix) :
    integerDigits radix input = .ok (some ⟨radix.basePrefix ++ digits ++ suffix,
      positionalValue radix digits, typeSuffix, rest⟩) := by
  simp only [integerDigits, scanDigits_complete hrun, scanIdentBody_complete hident]
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

section Conformance
-- The raised limit is for these `decide` cases only.
set_option maxRecDepth 100000

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
    .ok (some ([47,47,32,120], [10,114,101,115,116])) := by decide
example : lineComment [47,47,32,120,13] = .ok (some ([47,47,32,120], [13])) := by decide
example : lineComment [47,47,32,120] = .ok (some ([47,47,32,120], [])) := by decide
example : lineComment [47,47,9,120,10] = .ok (some ([47,47,9,120], [10])) := by decide
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

example : skipTrivia [32,65] = .ok ([32], [65]) := by decide
example : skipTrivia [9,65] = .ok ([9], [65]) := by decide
example : skipTrivia [10,65] = .ok ([10], [65]) := by decide
example : skipTrivia [13,65] = .ok ([13], [65]) := by decide
example : skipTrivia [13,10,65] = .ok ([13,10], [65]) := by decide
example : skipTrivia [47,47,120,10,65] = .ok ([47,47,120,10], [65]) := by decide
example : skipTrivia [47,47,9,120,10,65] = .ok ([47,47,9,120,10], [65]) := by decide
example : skipTrivia [47,47,120,13,65] = .ok ([47,47,120,13], [65]) := by decide
example : skipTrivia [47,47,120] = .ok ([47,47,120], []) := by decide
example : skipTrivia [47,47,0] = .error (.rawForbidden 0) := by decide
example : skipTrivia [9,32,47,47,120,13,10,65] =
    .ok ([9,32,47,47,120,13,10], [65]) := by decide

example : skipTrivia [47,42,32,42,47] = .ok ([], [47,42,32,42,47]) := by decide
example : skipTrivia [35,33,65] = .ok ([], [35,33,65]) := by decide
example : skipTrivia [0xA0,32] = .ok ([], [0xA0,32]) := by decide
example : skipTrivia [0xFEFF,32] = .ok ([], [0xFEFF,32]) := by decide
example : skipTrivia [47] = .ok ([], [47]) := by decide
example : skipTrivia [65,32] = .ok ([], [65,32]) := by decide

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
example : stringBody [] = .error .unterminated := by decide
example : stringBody [92,110] = .error .unterminated := by decide
example : stringBody [92] = .error .unterminated := by decide
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
example : integerLiteral [48,111,49,55,105,50,51] =
    .ok (some ⟨[48,111,49,55,105,50,51], 15, some .i32, []⟩) := by decide
example : integerLiteral [48,120,102,102,105,54,52] =
    .ok (some ⟨[48,120,102,102,105,54,52], 255, some .i64, []⟩) := by decide
example : integerLiteral [48,98,49,117,49,54] =
    .ok (some ⟨[48,98,49,117,49,54], 1, some .u16, []⟩) := by decide
example : integerLiteral [48,111,55,117,50,51] =
    .ok (some ⟨[48,111,55,117,50,51], 7, some .u32, []⟩) := by decide
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

end Conformance

end Concrete.Lex
