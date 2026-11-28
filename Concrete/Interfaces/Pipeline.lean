import Concrete.Interfaces.Contracts
import Concrete.Interfaces.Formation
import ConcreteSpec.StageGrammar
import ConcreteSpec.Admission.Early

namespace Concrete.Interfaces

open ConcreteSpec.Core ConcreteSpec.StageInterface ConcreteSpec.StageGrammar
open Concrete.Contracts (EvidenceKind Graph)

variable {count : Nat} {grammar : SharedGrammar count} {language : CoreLanguage}
  {lang : LoweringLanguage language.operands.Ty} {profile : Profile}

/-- Executable intra-IR pass at insertion point `k`. Returning `none` declines
transformation and falls back to the input with `Account.identityFor`. -/
abbrev IRPass {Ty : Type} (lang : LoweringLanguage Ty) (profile : Profile) (k : IRPoint) : Type :=
  AtLang lang profile k → Option (AtLang lang profile k × Account)

/-- Executable signatures are collected separately from the declarative spec and models.
The registry below builds its graphs from these values. -/
structure PassFunctions {count : Nat} (grammar : SharedGrammar count) (language : CoreLanguage)
    (lang : LoweringLanguage language.operands.Ty) (profile : Profile) where
  lex : Source → Except LexRefusal TokenStream
  elaborate : Parsed (grammar.value grammar.start) → Except ElabRefusal (AdmissionInput language)
  checkCore : CoreInput language → Except AdmissionFailure (Discharges language)
  lowerCore : Admitted language → Except StageFailure (MIR lang profile)
  lowerUnchecked : CoreInput language → Except StageFailure (MIR lang profile)
  lowerMemory : MIR lang profile → Except StageFailure (Memory lang profile)
  buildSSA : Memory lang profile → Except StageFailure (SSA lang profile)
  buildBIR : SSA lang profile → Except StageFailure (BIR lang profile)
  emit : BIR lang profile → Except StageFailure (BackendOutput profile)
  passesAt : (k : IRPoint) → List (IRPass lang profile k) := fun _ => []

def Source.WF (source : Source) : Prop :=
  (source.project.fileAt source.entryFile).isSome = true

/-- Source-order monotonicity on a `ParserView` token stream, required for multi-token
surface statement spans (`Surface0.statementSpan`) to remain same-file and non-reversed.
Owned by the lexer→parser boundary and discharged for every lexed stream by
`Lexes.tokensOrdered`. Delegates directly to `ConcreteSpec.StageGrammar.TokensOrdered`
so the definition lives in one place. -/
abbrev TokensOrdered (input : ParserView) : Prop :=
  ConcreteSpec.StageGrammar.TokensOrdered input.tokens.toList

section ReconstructTokensOrdered

private theorem advanceByte_byte (pos : Position) (b : Nat) :
    (pos.advanceByte b).byte = pos.byte + 1 := by
  unfold Position.advanceByte Position.step
  split
  · rfl
  · split <;> rfl

private theorem foldl_advanceByte_byte (xs : List UInt8) (pos : Position) :
    (xs.foldl (fun p (byte : UInt8) => p.advanceByte byte.toNat) pos).byte =
      pos.byte + xs.length := by
  induction xs generalizing pos with
  | nil => simp
  | cons b rest ih =>
    simp only [List.foldl_cons, List.length_cons, ih, advanceByte_byte]
    omega

private theorem array_foldl_advanceByte_byte (arr : Array UInt8) (pos : Position) :
    (arr.foldl (fun p (byte : UInt8) => p.advanceByte byte.toNat) pos).byte =
      pos.byte + arr.size := by
  rw [← Array.foldl_toList, foldl_advanceByte_byte, Array.length_toList]

private def reconstructAppend (entry : ConcreteSpec.Core.FileId)
    (state : ByteArray × Position) (piece : RawPiece) : Option (ByteArray × Position) :=
  let (bytes, position) := state
  if piece.span.file == entry && piece.span.startOffset == position.byte &&
      piece.span.endOffset == position.byte + piece.value.size && piece.value.size > 0 &&
      piece.span.line == position.line && piece.span.col == position.column then
    some (bytes ++ piece.value, piece.value.data.foldl
      (fun pos byte => pos.advanceByte byte.toNat) position)
  else none

private theorem reconstructAppend_spec {entry : ConcreteSpec.Core.FileId}
    {s s' : ByteArray × Position} {piece : RawPiece}
    (h : reconstructAppend entry s piece = some s') :
    piece.span.file = entry ∧
    piece.span.startOffset = s.2.byte ∧
    piece.span.endOffset = s.2.byte + piece.value.size ∧
    s'.2.byte = piece.span.endOffset ∧
    s.2.byte ≤ s'.2.byte := by
  rcases s with ⟨bytes, position⟩
  unfold reconstructAppend at h
  dsimp only at h
  split at h
  · rename_i hcond
    cases Option.some.inj h
    simp only [Bool.and_eq_true, beq_iff_eq, decide_eq_true_eq] at hcond
    obtain ⟨⟨⟨⟨⟨hfile, hstart⟩, hend⟩, _⟩, _⟩, _⟩ := hcond
    have hbyte : (piece.value.data.foldl
        (fun pos byte => pos.advanceByte byte.toNat) position).byte = position.byte + piece.value.size :=
      array_foldl_advanceByte_byte piece.value.data position
    refine ⟨hfile, hstart, hend, hbyte.trans hend.symm, by dsimp only; omega⟩
  · cases h

private theorem foldlM_reconstructAppend_mono {entry : ConcreteSpec.Core.FileId}
    {pieces : List RawPiece} {s s' : ByteArray × Position}
    (h : pieces.foldlM (reconstructAppend entry) s = some s') :
    s.2.byte ≤ s'.2.byte := by
  induction pieces generalizing s with
  | nil =>
    cases Option.some.inj h
    exact Nat.le_refl _
  | cons p rest ih =>
    simp only [List.foldlM_cons, Bind.bind, Option.bind] at h
    cases hp : reconstructAppend entry s p with
    | none => rw [hp] at h; cases h
    | some mid =>
      rw [hp] at h
      exact Nat.le_trans (reconstructAppend_spec hp).2.2.2.2 (ih h)

private theorem array_foldlM_reconstructAppend_mono {entry : ConcreteSpec.Core.FileId}
    {pieces : Array RawPiece} {s s' : ByteArray × Position}
    (h : pieces.foldlM (reconstructAppend entry) s = some s') :
    s.2.byte ≤ s'.2.byte := by
  rw [← Array.foldlM_toList] at h
  exact foldlM_reconstructAppend_mono h

private theorem foldlM_range_tokens {entry : ConcreteSpec.Core.FileId}
    {tokens : Array RetainedToken}
    (step : (ByteArray × Position) → Nat → Option (ByteArray × Position))
    (hstep : ∀ s i s', step s i = some s' →
      ∃ tok, tokens[i]? = some tok ∧
        tok.decoded.span.file = entry ∧
        s.2.byte ≤ tok.decoded.span.startOffset ∧
        tok.decoded.span.startOffset ≤ tok.decoded.span.endOffset ∧
        tok.decoded.span.endOffset = s'.2.byte) :
    ∀ (idxs : List Nat) (s s' : ByteArray × Position),
      idxs.foldlM step s = some s' →
      s.2.byte ≤ s'.2.byte ∧
      (∀ i ∈ idxs, ∃ tok, tokens[i]? = some tok ∧
        tok.decoded.span.file = entry ∧
        s.2.byte ≤ tok.decoded.span.startOffset ∧
        tok.decoded.span.startOffset ≤ tok.decoded.span.endOffset ∧
        tok.decoded.span.endOffset ≤ s'.2.byte) ∧
      idxs.Pairwise (fun i j =>
        ∀ ti tj, tokens[i]? = some ti → tokens[j]? = some tj →
          ti.decoded.span.file = tj.decoded.span.file ∧
          ti.decoded.span.startOffset ≤ tj.decoded.span.endOffset) := by
  intro idxs
  induction idxs with
  | nil =>
    intro s s' h
    cases Option.some.inj h
    exact ⟨Nat.le_refl _, fun _ hm => (List.not_mem_nil hm).elim, List.Pairwise.nil⟩
  | cons i rest ih =>
    intro s s' h
    simp only [List.foldlM_cons, Bind.bind, Option.bind] at h
    cases hi : step s i with
    | none => rw [hi] at h; cases h
    | some mid =>
      rw [hi] at h
      obtain ⟨ti, hti, hfilei, hsle, hstarti, hendi⟩ := hstep s i mid hi
      obtain ⟨hmidle, hmem, hpw⟩ := ih mid s' h
      refine ⟨by omega, ?_, ?_⟩
      · intro k hk
        rcases List.mem_cons.mp hk with rfl | hrest
        · exact ⟨ti, hti, hfilei, hsle, hstarti, by omega⟩
        · obtain ⟨tk, htk, hfilek, hmidk, hstartk, hendk⟩ := hmem k hrest
          exact ⟨tk, htk, hfilek, by omega, hstartk, hendk⟩
      · refine List.pairwise_cons.mpr ⟨?_, hpw⟩
        intro j hj ti' tj hti' htj
        have htiEq : ti' = ti := Option.some.inj (hti'.symm.trans hti)
        subst htiEq
        obtain ⟨tj', htj', hfilej, hmidj, hstartj, _⟩ := hmem j hj
        have htjEq : tj = tj' := Option.some.inj (htj.symm.trans htj')
        subst htjEq
        exact ⟨hfilei.trans hfilej.symm, by omega⟩

private theorem list_pairwise_of_range {α : Type} {R : α → α → Prop} (xs : List α)
    (h : (List.range xs.length).Pairwise (fun i j =>
      ∀ a b, xs[i]? = some a → xs[j]? = some b → R a b)) :
    xs.Pairwise R := by
  have hgo : ∀ (start : Nat) (ys : List α),
      (∀ k a, ys[k]? = some a → xs[start + k]? = some a) →
      (List.range' start ys.length).Pairwise (fun i j =>
        ∀ a b, xs[i]? = some a → xs[j]? = some b → R a b) →
      ys.Pairwise R := by
    intro start ys
    induction ys generalizing start with
    | nil => intros; exact List.Pairwise.nil
    | cons a rest ih =>
      intro halign hpw
      simp only [List.length_cons, List.range'_succ, List.pairwise_cons] at hpw
      refine List.pairwise_cons.mpr ⟨?_, ih (start + 1) ?_ hpw.2⟩
      · intro b hb
        obtain ⟨k, hk, hget⟩ := List.getElem_of_mem hb
        have hget? : rest[k]? = some b := by rw [List.getElem?_eq_getElem hk, hget]
        have ha : xs[start]? = some a := by
          simpa using halign 0 a rfl
        have hbk : xs[start + (k + 1)]? = some b := halign (k + 1) b hget?
        have hmem : start + 1 + k ∈ List.range' (start + 1) rest.length := by
          rw [List.mem_range']
          exact ⟨k, hk, by omega⟩
        exact hpw.1 (start + 1 + k) hmem a b ha (by simpa [Nat.add_assoc, Nat.add_comm 1 k] using hbk)
      · intro k b hget
        have := halign (k + 1) b hget
        simpa [Nat.add_assoc, Nat.add_comm 1 k] using this
  apply hgo 0 xs (fun k a hget => by simpa using hget)
  simpa [List.range_eq_range'] using h

/-- Whole-file reconstruction implies source-order token monotonicity on `stream.parserView`. -/
theorem reconstruct_tokensOrdered {source : Source} {stream : TokenStream} {bytes : ByteArray}
    (hrec : reconstruct source.entryFile stream.tokens stream.trivia = some bytes) :
    TokensOrdered stream.parserView := by
  unfold reconstruct at hrec
  cases hsz : (stream.trivia.size != stream.tokens.size + 1)
  · rw [hsz] at hrec
    dsimp only at hrec
    let step : (ByteArray × Position) → Nat → Option (ByteArray × Position) :=
      fun state index => do
        let before ← stream.trivia[index]?
        let state ← before.foldlM (fun x y => reconstructAppend source.entryFile x y) state
        let token ← stream.tokens[index]?
        reconstructAppend source.entryFile state ⟨token.decoded.span, token.spelling⟩
    have hfold : ∃ s', (List.range stream.tokens.size).foldlM step (ByteArray.empty, {}) = some s' := by
      cases hf : (List.range stream.tokens.size).foldlM step (ByteArray.empty, {}) with
      | none =>
        have hrec' : Option.bind ((List.range stream.tokens.size).foldlM step (ByteArray.empty, {}))
            (fun state => Option.bind stream.trivia[stream.tokens.size]? fun trailing =>
              Option.bind (trailing.foldlM (reconstructAppend source.entryFile) state)
                fun state => some state.1) = some bytes := hrec
        rw [hf] at hrec'
        cases hrec'
      | some s' => exact ⟨s', rfl⟩
    obtain ⟨s', hs'⟩ := hfold
    have hstep : ∀ s i s'', step s i = some s'' →
        ∃ tok, stream.tokens[i]? = some tok ∧
          tok.decoded.span.file = source.entryFile ∧
          s.2.byte ≤ tok.decoded.span.startOffset ∧
          tok.decoded.span.startOffset ≤ tok.decoded.span.endOffset ∧
          tok.decoded.span.endOffset = s''.2.byte := by
      intro s i s'' h
      dsimp only [step, Bind.bind, Option.bind] at h
      cases htr : stream.trivia[i]? with
      | none => rw [htr] at h; cases h
      | some before =>
        rw [htr] at h; dsimp only at h
        cases hmid : before.foldlM (fun x y => reconstructAppend source.entryFile x y) s with
        | none => rw [hmid] at h; cases h
        | some mid =>
          rw [hmid] at h; dsimp only at h
          cases htok : stream.tokens[i]? with
          | none => rw [htok] at h; cases h
          | some tok =>
            rw [htok] at h; dsimp only at h
            have hmono := array_foldlM_reconstructAppend_mono hmid
            obtain ⟨hfile, hstart, hend, hs''eq, _⟩ := reconstructAppend_spec h
            dsimp only at hfile hstart hend hs''eq
            exact ⟨tok, rfl, hfile, by omega, by omega, hs''eq.symm⟩
    have hpw := (foldlM_range_tokens step hstep _ _ _ hs').2.2
    unfold TokensOrdered ConcreteSpec.StageGrammar.TokensOrdered TokenStream.parserView
    dsimp only
    apply list_pairwise_of_range
    rw [List.length_map, Array.length_toList]
    refine List.Pairwise.imp ?_ hpw
    intro i j hij a b ha hb
    rw [List.getElem?_map] at ha hb
    cases hi : stream.tokens.toList[i]? with
    | none => rw [hi] at ha; cases ha
    | some ti =>
      rw [hi, Option.map_some] at ha
      cases Option.some.inj ha
      cases hj : stream.tokens.toList[j]? with
      | none => rw [hj] at hb; cases hb
      | some tj =>
        rw [hj, Option.map_some] at hb
        cases Option.some.inj hb
        exact hij ti tj (by simpa [Array.getElem?_toList] using hi)
          (by simpa [Array.getElem?_toList] using hj)
  · rw [hsz] at hrec
    cases hrec

/-- Every lexed token stream satisfies `TokensOrdered stream.parserView`. -/
theorem Lexes.tokensOrdered {source : Source} {stream : TokenStream} (h : Lexes source stream) :
    TokensOrdered stream.parserView := by
  obtain ⟨_, _, _, _, bytes, hrec, _⟩ := h
  exact reconstruct_tokensOrdered hrec

end ReconstructTokensOrdered

def PassFunctions.lexer (passes : PassFunctions grammar language lang profile) : Stage Source LexRefusal TokenStream where
  run := passes.lex
  kind := .directProof
  inputInvariant := Source.WF
  outputInvariant stream := stream.parserView.WF ∧ TokensOrdered stream.parserView
  success := Lexes
  refusal := LexRefuses

/-- Registered parser stage backed by a certified totality proof `nonExhausted`,
eliminating any `.getD` exhaustion fallback. -/
def parserEntry (parser : ParserEngine grammar)
    (nonExhausted : ∀ input : ParserView, (parser.run input).2.isSome = true) :
    Stage TokenStream ParseRefusal (Parsed (grammar.value grammar.start)) where
  run stream := (parser.run stream.parserView).2.get (nonExhausted stream.parserView)
  kind := .directProof
  inputInvariant stream := stream.parserView.WF ∧ TokensOrdered stream.parserView
  outputInvariant ast := Source.WF ast.source
  success stream output := (parser.run stream.parserView).2 = some (.ok output) ∧
    ParsesWith parser stream.parserView output ∧
    (parser.run stream.parserView).1 ≤ parser.stepsPerToken * (stream.parserView.tokens.size + 1)
  refusal stream failure := (parser.run stream.parserView).2 = some (.error failure) ∧
    Refuses grammar stream.parserView failure ∧
    (parser.run stream.parserView).1 ≤ parser.stepsPerToken * (stream.parserView.tokens.size + 1)

theorem parserEntry_run_eq_some (parser : ParserEngine grammar)
    (nonExhausted : ∀ input : ParserView, (parser.run input).2.isSome = true)
    (stream : TokenStream) :
    some ((parserEntry parser nonExhausted).run stream) = (parser.run stream.parserView).2 :=
  Option.some_get (nonExhausted stream.parserView)

theorem parserEntry_run_eq_ok_iff (parser : ParserEngine grammar)
    (nonExhausted : ∀ input : ParserView, (parser.run input).2.isSome = true)
    (stream : TokenStream) (output : Parsed (grammar.value grammar.start)) :
    (parserEntry parser nonExhausted).run stream = .ok output ↔
      (parser.run stream.parserView).2 = some (.ok output) := by
  constructor
  · intro h
    rw [← parserEntry_run_eq_some parser nonExhausted stream, h]
  · intro h
    exact Option.some.inj ((parserEntry_run_eq_some parser nonExhausted stream).trans h)

theorem parserEntry_run_eq_error_iff (parser : ParserEngine grammar)
    (nonExhausted : ∀ input : ParserView, (parser.run input).2.isSome = true)
    (stream : TokenStream) (failure : ParseRefusal) :
    (parserEntry parser nonExhausted).run stream = .error failure ↔
      (parser.run stream.parserView).2 = some (.error failure) := by
  constructor
  · intro h
    rw [← parserEntry_run_eq_some parser nonExhausted stream, h]
  · intro h
    exact Option.some.inj ((parserEntry_run_eq_some parser nonExhausted stream).trans h)

/-- The fixed grammar supplies a surface tree from a coordinate-valid, source-ordered
token stream. Any successful canonical elaboration must retain locations valid in the
issuing project. -/
def Parsed.ElaborationDomain (slice : Slice)
    (ast : Parsed ((sliceGrammar slice).value (sliceGrammar slice).start)) : Prop :=
  Source.WF ast.source ∧ ast.source.slice = slice ∧
  (∃ stream : ParserView, stream.WF ∧ TokensOrdered stream ∧
    ((sliceParser slice).run stream).2 = some (.ok ast)) ∧
  (∃ result, SliceElaboration slice ast.source.project (cast (sliceGrammarStartShape slice) ast.tree) result) ∧
  ∀ input, SliceElaboration slice ast.source.project (cast (sliceGrammarStartShape slice) ast.tree) (.ok input) →
    input.Ready slice

/-- Resolve representable source before unchecked Core typing. The slice is the
one selected by the registry, independently of the implementation language.
Checker refusals use the single `.admission (.refused cause)` form. -/
def eligibilityEntry (language : CoreLanguage) (slice : Slice) :
    Stage (AdmissionInput language) StageFailure (CoreInput language) where
  run input := if input.slice = slice then
      (input.prepare .eligibility).mapError fun cause => .admission (.refused cause)
    else .error (.admission (.fragment (.sliceMismatch slice input.slice)))
  kind := .directProof
  inputInvariant input := input.Ready slice
  outputInvariant input := input.slice = slice ∧ input.WF ∧ input.InFragment
  success input output := input.slice = slice ∧ input.prepare .eligibility = .ok output
  refusal input failure := if input.slice = slice then
      ∃ cause, input.prepare .eligibility = .error cause ∧ failure = .admission (.refused cause)
    else failure = .admission (.fragment (.sliceMismatch slice input.slice))

theorem eligibilityEntry_validated (language : CoreLanguage) (slice : Slice) :
    (eligibilityEntry language slice).Validated := by
  constructor
  · intro input output ready ran
    simp only [Graph, eligibilityEntry, ready.1, ↓reduceIte] at ran
    cases prepared : input.prepare .eligibility with
    | error reason => simp [prepared, Except.mapError] at ran
    | ok i =>
      simp only [prepared, Except.mapError, Except.ok.injEq] at ran
      subst output
      exact ⟨⟨(input.preparedSlice prepared).trans ready.1, ready.prepared prepared⟩, ready.1, prepared⟩
  · intro input failure ready ran
    change (eligibilityEntry language slice).run input = .error failure at ran
    simp only [eligibilityEntry, ready.1, ↓reduceIte] at ran ⊢
    cases prepared : input.prepare .eligibility with
    | ok i => simp [prepared, Except.mapError] at ran
    | error cause => exact ⟨cause, rfl, by simpa [prepared, Except.mapError] using ran.symm⟩

/-- Static typing and ownership gate on the unchecked route. On-slice refusals carry
the single checker-refusal form `.admission (.refused cause)` preserving the exact
upstream cause, registered rule, primary span, and related spans via `StaticRefuses`. -/
def typingEntry (language : CoreLanguage) (slice : Slice) :
    Stage (CoreInput language) StageFailure (CoreInput language) where
  run input := if input.slice = slice then
      (input.checkStatic.map (fun _ => input)).mapError StageFailure.admission
    else .error (.invalidInput ⟨``CoreInput.slice, 1⟩)
  kind := .directProof
  inputInvariant input := input.slice = slice ∧ input.WF ∧ input.InFragment
  outputInvariant input := input.slice = slice ∧ input.UncheckedWF
  success input output := input = output
  refusal input failure := if input.slice = slice then
      ∃ cause, input.checkStatic = .error (.refused cause) ∧ input.StaticRefuses cause ∧
        failure = .admission (.refused cause)
    else failure = .invalidInput ⟨``CoreInput.slice, 1⟩

theorem typingEntry_validated_of_render (language : CoreLanguage) (slice : Slice)
    (renderTotal : ∀ (v : AdmissionView) (r : ConcreteSpec.Admission.Refusal),
      ConcreteSpec.checkProgramWith .runtime v.program = .error r →
        ∃ cause, v.render r = some cause) :
    (typingEntry language slice).Validated := by
  constructor
  · intro i o ready ran
    change (if i.slice = slice then (i.checkStatic.map (fun _ => i)).mapError StageFailure.admission
      else .error (.invalidInput ⟨``CoreInput.slice, 1⟩)) = Except.ok o at ran
    rw [ite_eq_left ready.1] at ran
    cases h : i.checkStatic with
    | error r => simp [h, Except.map, Except.mapError] at ran
    | ok u =>
      cases u
      simp [h, Except.map, Except.mapError] at ran
      exact ⟨ran ▸ ⟨ready.1, (ConcreteSpec.Admission.checkStatic_iff ready.2.1).1 h⟩, ran⟩
  · intro i failure ready ran
    change (if i.slice = slice then (i.checkStatic.map (fun _ => i)).mapError StageFailure.admission
      else .error (.invalidInput ⟨``CoreInput.slice, 1⟩)) = Except.error failure at ran
    rw [ite_eq_left ready.1] at ran
    change (if i.slice = slice then _ else _)
    rw [ite_eq_left ready.1]
    obtain ⟨v, hv⟩ := ready.2.2
    have hdec : i.decode = .ok v := (i.decode_iff v).mpr hv
    cases hchk : ConcreteSpec.checkProgramWith .runtime v.program with
    | ok _ =>
      have hok : i.checkStatic = .ok () := by
        simp [CoreInput.checkStatic, hdec, hchk, Except.mapError, Bind.bind, Except.bind,
          Pure.pure, Except.pure]
      simp [hok, Except.map, Except.mapError] at ran
    | error reason =>
      obtain ⟨cause, hren⟩ := renderTotal v reason hchk
      have herr : i.checkStatic = .error (.refused cause) := by
        simp [CoreInput.checkStatic, hdec, hchk, AdmissionView.failure, hren,
          Except.mapError, Bind.bind, Except.bind]
      have hstatic : i.StaticRefuses cause := ConcreteSpec.Admission.checkStatic_refused herr
      refine ⟨cause, herr, hstatic, ?_⟩
      simpa [herr, Except.map, Except.mapError] using ran.symm

/-- Registration requires rejection of every input tagged for a different slice,
including off-domain inputs passed directly to the stage. -/
structure AdmissionEntry (language : CoreLanguage) (slice : Slice) extends
    Stage (AdmissionInput language) AdmissionFailure (Admitted language) where
  rejectsMismatch : ∀ input, input.slice ≠ slice →
    toStage.run input = .error (.fragment (.sliceMismatch slice input.slice))

def PassFunctions.admissionEntry (passes : PassFunctions grammar language lang profile) (slice : Slice) :
    AdmissionEntry language slice where
  run input := if input.slice = slice then do
      let prepared ← input.prepare.mapError AdmissionFailure.refused
      (passes.checkCore prepared).map fun discharges => ⟨prepared, discharges⟩
    else .error (.fragment (.sliceMismatch slice input.slice))
  kind := .directProof
  inputInvariant input := input.Ready slice
  outputInvariant := Admitted.Valid
  success input output := input.slice = slice ∧ ∃ prepared, input.prepare = .ok prepared ∧ Admits prepared output
  refusal input
    | .refused diagnostic => input.slice = slice ∧ input.CheckerRefuses diagnostic
    | .fragment (.sliceMismatch expected actual) => expected = slice ∧ actual = input.slice ∧ actual ≠ expected
    | _ => False
  rejectsMismatch input wrong := by simp [wrong]

/-! ### Strengthened IR handoff invariants (`MIR`, `Memory`, `SSA`, `BIR`) -/

/-- Declaration inventory well-formedness: unique `DeclId` keys, valid spec struct
declarations (`DeclsOk`), and matching qualified/unqualified struct names. -/
def DeclarationsWF (declarations : Declarations) : Prop :=
  (List.map Prod.fst declarations).Nodup ∧
  ConcreteSpec.DeclsOk (List.map Prod.snd declarations) ∧
  ∀ id decl, (id, decl) ∈ declarations → decl.name = id.name

/-- Mutable-local table well-formedness: unique function keys, every entry refers to
a declared function, and every listed mutable root is declared in that function's locals. -/
def MutableLocalsWF {Ty G : Type} (functions : AList FnId (LocalTypes Ty × G))
    (mutableLocals : AList FnId (List PlaceId)) : Prop :=
  (List.map Prod.fst mutableLocals).Nodup ∧
  ∀ fn roots, (fn, roots) ∈ mutableLocals →
    ∃ locals graph, (fn, locals, graph) ∈ functions ∧
      ∀ root ∈ roots, ∃ ty, (root, ty) ∈ locals

def Memory.HandoffWF {Ty : Type} {lowering : LoweringLanguage Ty}
    (memory : Memory lowering profile) : Prop :=
  memory.TablesWF ∧ memory.entry ∈ List.map Prod.fst memory.functions ∧
  (∀ origin ∈ memory.origins, ∃ span, origin.lexical.Binds memory.project span) ∧
  memory.LayoutsWF ∧
  DeclarationsWF memory.declarations ∧
  MutableLocalsWF memory.functions memory.mutableLocals

theorem Memory.HandoffWF.tablesWF {Ty : Type} {lowering : LoweringLanguage Ty}
    {memory : Memory lowering profile} (h : memory.HandoffWF) : memory.TablesWF :=
  h.1

theorem Memory.HandoffWF.entry_mem {Ty : Type} {lowering : LoweringLanguage Ty}
    {memory : Memory lowering profile} (h : memory.HandoffWF) :
    memory.entry ∈ List.map Prod.fst memory.functions :=
  h.2.1

theorem Memory.HandoffWF.origins_bind {Ty : Type} {lowering : LoweringLanguage Ty}
    {memory : Memory lowering profile} (h : memory.HandoffWF) :
    ∀ origin ∈ memory.origins, ∃ span, origin.lexical.Binds memory.project span :=
  h.2.2.1

theorem Memory.HandoffWF.layoutsWF {Ty : Type} {lowering : LoweringLanguage Ty}
    {memory : Memory lowering profile} (h : memory.HandoffWF) : memory.LayoutsWF :=
  h.2.2.2.1

theorem Memory.HandoffWF.declarationsWF {Ty : Type} {lowering : LoweringLanguage Ty}
    {memory : Memory lowering profile} (h : memory.HandoffWF) :
    DeclarationsWF memory.declarations :=
  h.2.2.2.2.1

theorem Memory.HandoffWF.mutableLocalsWF {Ty : Type} {lowering : LoweringLanguage Ty}
    {memory : Memory lowering profile} (h : memory.HandoffWF) :
    MutableLocalsWF memory.functions memory.mutableLocals :=
  h.2.2.2.2.2

/-- MIR handoff invariant: logical-view handoff (table uniqueness, CFG and signature
formation, entry presence, project-bound origins, valid declarations and mutable locals)
together with target `Materializable` on `profile`. -/
def MIR.HandoffWF (mir : MIR lang profile) : Prop :=
  (mir.logicalView (language := language)).HandoffWF ∧
  Materializable (language := language) mir

abbrev MIRHandoffWF {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} (mir : MIR lang profile) : Prop :=
  MIR.HandoffWF (language := language) mir

theorem MIR.HandoffWF.logicalView {mir : MIR lang profile}
    (h : mir.HandoffWF (language := language)) :
    (mir.logicalView (language := language)).HandoffWF :=
  h.1

theorem MIR.HandoffWF.materializable {mir : MIR lang profile}
    (h : mir.HandoffWF (language := language)) :
    Materializable (language := language) mir :=
  h.2

theorem MIR.HandoffWF.declarationsWF {mir : MIR lang profile}
    (h : mir.HandoffWF (language := language)) :
    DeclarationsWF mir.declarations :=
  h.1.declarationsWF

def SSA.WF (ssa : SSA lang profile) : Prop :=
  ssa.RegisterFormation (language := language) ∧
  (ssa.memoryView (language := language)).HandoffWF

theorem SSA.WF.registerFormation {ssa : SSA lang profile}
    (h : ssa.WF (language := language)) : ssa.RegisterFormation (language := language) :=
  h.1

theorem SSA.WF.memoryView {ssa : SSA lang profile}
    (h : ssa.WF (language := language)) : (ssa.memoryView (language := language)).HandoffWF :=
  h.2

theorem SSA.WF.layoutsWF {ssa : SSA lang profile}
    (h : ssa.WF (language := language)) : (ssa.memoryView (language := language)).LayoutsWF :=
  h.2.layoutsWF

def BIR.WF (bir : BIR lang profile) : Prop :=
  bir.RegisterFormation (language := language) ∧
  bir.TrapSlotsWF ∧ bir.frameSlots ≤ profile.maxBIRSlots ∧
  (∀ origin ∈ bir.origins, ∃ span, origin.lexical.Binds bir.project span) ∧
  (∀ site ∈ bir.sites.toList, ∃ span, site.origin.lexical.Binds bir.project span) ∧
  (bir.memoryView (language := language)).HandoffWF

theorem BIR.WF.registerFormation {bir : BIR lang profile}
    (h : bir.WF (language := language)) : bir.RegisterFormation (language := language) :=
  h.1

theorem BIR.WF.trapSlotsWF {bir : BIR lang profile}
    (h : bir.WF (language := language)) : bir.TrapSlotsWF :=
  h.2.1

theorem BIR.WF.frameSlots_le {bir : BIR lang profile}
    (h : bir.WF (language := language)) : bir.frameSlots ≤ profile.maxBIRSlots :=
  h.2.2.1

theorem BIR.WF.origins_bind {bir : BIR lang profile}
    (h : bir.WF (language := language)) :
    ∀ origin ∈ bir.origins, ∃ span, origin.lexical.Binds bir.project span :=
  h.2.2.2.1

theorem BIR.WF.sites_bind {bir : BIR lang profile}
    (h : bir.WF (language := language)) :
    ∀ site ∈ bir.sites.toList, ∃ span, site.origin.lexical.Binds bir.project span :=
  h.2.2.2.2.1

theorem BIR.WF.memoryView {bir : BIR lang profile}
    (h : bir.WF (language := language)) : (bir.memoryView (language := language)).HandoffWF :=
  h.2.2.2.2.2

theorem BIR.WF.layoutsWF {bir : BIR lang profile}
    (h : bir.WF (language := language)) : (bir.memoryView (language := language)).LayoutsWF :=
  h.2.2.2.2.2.layoutsWF

/-! ### Intra-IR pass postconditions and list folding (`PassPost`, `foldPasses`) -/

def AtLang.entry {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile} :
    {k : IRPoint} → AtLang lang profile k → FnId
  | .mir, mir => MIR.entry mir
  | .memory, mem => Memory.entry mem
  | .ssa, ssa => SSA.entry ssa
  | .bir, bir => BIR.entry bir

def AtLang.declarations {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile} :
    {k : IRPoint} → AtLang lang profile k → Declarations
  | .mir, mir => MIR.declarations mir
  | .memory, mem => Memory.declarations mem
  | .ssa, ssa => SSA.declarations ssa
  | .bir, bir => BIR.declarations bir

def AtLang.mutableLocals {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile} :
    {k : IRPoint} → AtLang lang profile k → AList FnId (List PlaceId)
  | .mir, mir => MIR.mutableLocals mir
  | .memory, mem => Memory.mutableLocals mem
  | .ssa, ssa => SSA.mutableLocals ssa
  | .bir, bir => BIR.mutableLocals bir

def AtLang.WF {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} : {k : IRPoint} → AtLang lang profile k → Prop
  | .mir, mir => MIR.HandoffWF (language := language) mir
  | .memory, memory => Memory.HandoffWF memory
  | .ssa, ssa => SSA.WF (language := language) ssa
  | .bir, bir => BIR.WF (language := language) bir

def At.WF {slice : Slice} {profile : Profile} {k : IRPoint} (x : At slice profile k) : Prop :=
  AtLang.WF (language := sliceLanguage slice) x

def AtLang.Execution {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} : (k : IRPoint) → Concrete.Contracts.Semantics (AtLang lang profile k) language.operands.Event
  | .mir => MIR.Execution (language := language)
  | .memory => Memory.Execution (language := language)
  | .ssa => SSA.Execution (language := language)
  | .bir => BIR.Execution (language := language)

/-- Postcondition of one intra-IR pass at insertion point `k`:
the output satisfies `AtLang.WF`, keeps the issuing project, entry function, and
declarations without inventing new mutable locals, accounts for every occurrence via
`AccountsForLang`, and refines the input's execution semantics at `k`. -/
def PassPostLang {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} {k : IRPoint}
    (a b : AtLang lang profile k) (account : Account) : Prop :=
  AtLang.WF (language := language) b ∧
  AtLang.project b = AtLang.project a ∧
  AtLang.entry b = AtLang.entry a ∧
  AtLang.declarations b = AtLang.declarations a ∧
  (∀ fn roots, (fn, roots) ∈ AtLang.mutableLocals b →
    ∀ root ∈ roots, ∃ srcRoots, (fn, srcRoots) ∈ AtLang.mutableLocals a ∧ root ∈ srcRoots) ∧
  AccountsForLang a b account ∧
  BehaviorRefinement (AtLang.Execution (language := language) k)
    (AtLang.Execution (language := language) k)
    (AtLang.Execution (language := language) k).result
    (AtLang.Execution (language := language) k).result a b

/-- Slice-selected intra-IR pass postcondition at insertion point `k`. -/
def PassPost {slice : Slice} {profile : Profile} {k : IRPoint}
    (a b : At slice profile k) (account : Account) : Prop :=
  PassPostLang (language := sliceLanguage slice) a b account

theorem PassPostLang.identity {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} {k : IRPoint} {a : AtLang lang profile k}
    (hwf : AtLang.WF (language := language) a) :
    PassPostLang (language := language) a a (Account.identityFor a) :=
  ⟨hwf, rfl, rfl, rfl, fun _ roots h _ hr => ⟨roots, h, hr⟩,
    AccountsForLang.identity a, behaviorRefl a⟩

theorem PassPost.identity {slice : Slice} {profile : Profile} {k : IRPoint}
    {a : At slice profile k} (hwf : At.WF a) :
    PassPost a a (Account.identityFor a) :=
  PassPostLang.identity hwf

theorem PassPostLang.compose {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} {k : IRPoint} {a b c : AtLang lang profile k} {first second : Account}
    (h1 : PassPostLang (language := language) a b first)
    (h2 : PassPostLang (language := language) b c second)
    (hElim : ComposedEliminationsJustified a first second) :
    PassPostLang (language := language) a c (Account.compose first second) :=
  ⟨h2.1, h2.2.1.trans h1.2.1, h2.2.2.1.trans h1.2.2.1, h2.2.2.2.1.trans h1.2.2.2.1,
    fun fn roots hmem root hr =>
      let ⟨midRoots, hmid, hrmid⟩ := h2.2.2.2.2.1 fn roots hmem root hr
      h1.2.2.2.2.1 fn midRoots hmid root hrmid,
    AccountsForLang.compose h1.2.2.2.2.2.1 h2.2.2.2.2.2.1 hElim,
    behaviorTrans h1.2.2.2.2.2.2 h2.2.2.2.2.2.2⟩

theorem PassPost.compose {slice : Slice} {profile : Profile} {k : IRPoint}
    {a b c : At slice profile k} {first second : Account}
    (h1 : PassPost a b first) (h2 : PassPost b c second)
    (hElim : ComposedEliminationsJustified a first second) :
    PassPost a c (Account.compose first second) :=
  PassPostLang.compose h1 h2 hElim

/-- Execute one intra-IR pass with identity fallback on a declined (`none`) candidate. -/
def IRPass.runOrIdentity {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    {k : IRPoint} (pass : IRPass lang profile k) (input : AtLang lang profile k) :
    Except StageFailure (AtLang lang profile k) :=
  match pass input with
  | some (candidate, _) => .ok candidate
  | none => .ok input

/-- Wrap one intra-IR pass as a registered `Stage` at insertion point `k`.
Declined candidates (`none`) fall back to `input` with `Account.identityFor input`;
accepted candidates (`some (candidate, account)`) must satisfy `PassPostLang` for the
exact returned `account`. -/
def IRPass.toStage {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} {k : IRPoint} (pass : IRPass lang profile k) :
    Stage (AtLang lang profile k) StageFailure (AtLang lang profile k) where
  run := pass.runOrIdentity
  kind := .provedValidator
  inputInvariant := AtLang.WF (language := language)
  outputInvariant := AtLang.WF (language := language)
  success input output :=
    match pass input with
    | some (candidate, account) => output = candidate ∧ PassPostLang (language := language) input candidate account
    | none => output = input ∧ PassPostLang (language := language) input input (Account.identityFor input)
  refusal _ _ := False
  trust _ _ := True

theorem IRPass.toStage_run_none {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} {k : IRPoint} (pass : IRPass lang profile k)
    {input : AtLang lang profile k} (h : pass input = none) :
    (pass.toStage (language := language)).run input = .ok input := by
  simp [IRPass.toStage, IRPass.runOrIdentity, h]

theorem IRPass.toStage_run_some {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} {k : IRPoint} (pass : IRPass lang profile k)
    {input candidate : AtLang lang profile k} {account : Account}
    (h : pass input = some (candidate, account)) :
    (pass.toStage (language := language)).run input = .ok candidate := by
  simp [IRPass.toStage, IRPass.runOrIdentity, h]

theorem IRPass.toStage_fallback {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} {k : IRPoint} (pass : IRPass lang profile k)
    {input : AtLang lang profile k} (hwf : AtLang.WF (language := language) input)
    (h : pass input = none) :
    (pass.toStage (language := language)).run input = .ok input ∧
    (pass.toStage (language := language)).outputInvariant input ∧
    (pass.toStage (language := language)).success input input := by
  refine ⟨IRPass.toStage_run_none pass h, hwf, ?_⟩
  dsimp only [IRPass.toStage]
  rw [h]
  exact ⟨rfl, PassPostLang.identity hwf⟩

theorem IRPass.toStage_validated_iff {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} {k : IRPoint} (pass : IRPass lang profile k) :
    (pass.toStage (language := language)).Validated ↔
      ∀ input candidate account, AtLang.WF (language := language) input →
        pass input = some (candidate, account) →
        PassPostLang (language := language) input candidate account := by
  constructor
  · intro hval input candidate account hwf hpass
    have hrun : Graph (pass.toStage (language := language)).run input candidate :=
      IRPass.toStage_run_some pass hpass
    have hok := (hval.1 input candidate hwf hrun).2
    dsimp only [IRPass.toStage] at hok
    rw [hpass] at hok
    exact hok.2
  · intro hall
    constructor
    · intro input output hwf ran
      change pass.runOrIdentity input = .ok output at ran
      unfold IRPass.runOrIdentity at ran
      cases hpass : pass input with
      | none =>
        rw [hpass] at ran
        cases Except.ok.inj ran
        have hid := PassPostLang.identity (language := language) hwf
        refine ⟨hwf, ?_⟩
        dsimp only [IRPass.toStage]
        rw [hpass]
        exact ⟨rfl, hid⟩
      | some pair =>
        rcases pair with ⟨candidate, account⟩
        rw [hpass] at ran
        cases Except.ok.inj ran
        have hpost := hall input output account hwf hpass
        refine ⟨hpost.1, ?_⟩
        dsimp only [IRPass.toStage]
        rw [hpass]
        exact ⟨rfl, hpost⟩
    · intro input failure _ ran
      change pass.runOrIdentity input = .error failure at ran
      unfold IRPass.runOrIdentity at ran
      cases hpass : pass input with
      | none => rw [hpass] at ran; cases ran
      | some pair => rw [hpass] at ran; cases ran

theorem IRPass.declined_validated {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} {k : IRPoint} :
    (IRPass.toStage (language := language)
      (fun (_ : AtLang lang profile k) => (none : Option (AtLang lang profile k × Account)))).Validated :=
  (IRPass.toStage_validated_iff _).mpr fun _ _ _ _ h => by cases h

/-- Fold a list of intra-IR passes at insertion point `k` into a sequential `Pipeline`. -/
def foldPasses {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} {k : IRPoint} :
    List (IRPass lang profile k) → Pipeline (AtLang lang profile k) (AtLang lang profile k)
  | [] => .done
  | pass :: rest => .next (pass.toStage (language := language)) (foldPasses rest)

theorem foldPasses_nil {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} {k : IRPoint} :
    foldPasses (language := language) ([] : List (IRPass lang profile k)) = .done := rfl

theorem foldPasses_cons {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} {k : IRPoint} (pass : IRPass lang profile k) (rest : List (IRPass lang profile k)) :
    foldPasses (language := language) (pass :: rest) =
      .next (pass.toStage (language := language)) (foldPasses (language := language) rest) := rfl

theorem foldPasses_nil_run {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} {k : IRPoint} (input : AtLang lang profile k) :
    (foldPasses (language := language) ([] : List (IRPass lang profile k))).run input = .ok input := rfl

theorem foldPasses_cons_run {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} {k : IRPoint} (pass : IRPass lang profile k)
    (rest : List (IRPass lang profile k)) (input : AtLang lang profile k) :
    (foldPasses (language := language) (pass :: rest)).run input =
      match pass input with
      | some (candidate, _) => (foldPasses (language := language) rest).run candidate
      | none => (foldPasses (language := language) rest).run input := by
  simp only [foldPasses, Pipeline.run, IRPass.toStage, IRPass.runOrIdentity]
  cases pass input <;> rfl

theorem foldPasses_nil_validated {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} {k : IRPoint} :
    (foldPasses (language := language) ([] : List (IRPass lang profile k))).Validated :=
  .done

theorem foldPasses_cons_validated {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} {k : IRPoint} {pass : IRPass lang profile k} {rest : List (IRPass lang profile k)}
    (hpass : (pass.toStage (language := language)).Validated)
    (hrest : (foldPasses (language := language) rest).Validated) :
    (foldPasses (language := language) (pass :: rest)).Validated := by
  cases rest with
  | nil => exact .last hpass
  | cons next tail => exact .next hpass (fun _ h => h) hrest

theorem foldPasses_validated_iff {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} {k : IRPoint} (passes : List (IRPass lang profile k)) :
    (foldPasses (language := language) passes).Validated ↔
      ∀ pass ∈ passes, (pass.toStage (language := language)).Validated := by
  induction passes with
  | nil => exact ⟨fun _ _ hm => (List.not_mem_nil hm).elim, fun _ => Pipeline.Validated.done⟩
  | cons pass rest ih =>
    constructor
    · intro hval p hp
      have ⟨hpass, hrest⟩ : (pass.toStage (language := language)).Validated ∧
          (foldPasses (language := language) rest).Validated := by
        cases rest with
        | nil =>
          cases hval with
          | last hv => exact ⟨hv, .done⟩
        | cons _ _ =>
          cases hval with
          | next hv _ htail => exact ⟨hv, htail⟩
      rcases List.mem_cons.mp hp with rfl | htail
      · exact hpass
      · exact (ih.mp hrest) p htail
    · intro hall
      exact foldPasses_cons_validated (hall pass List.mem_cons_self)
        (ih.mpr fun p hp => hall p (List.mem_cons_of_mem _ hp))

theorem foldPasses_trusted {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} {k : IRPoint} (passes : List (IRPass lang profile k)) :
    (foldPasses (language := language) passes).Trusted := by
  induction passes with
  | nil => trivial
  | cons pass rest ih => exact ⟨fun _ _ _ _ => trivial, ih⟩

theorem foldPasses_compatible_left {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} {k : IRPoint} {A R : Type}
    (edge : Stage A R (AtLang lang profile k)) (passes : List (IRPass lang profile k))
    (hcompat : ∀ x, edge.outputInvariant x → AtLang.WF (language := language) x) :
    (Pipeline.next edge .done).Compatible (foldPasses (language := language) passes) := by
  cases passes with
  | nil => trivial
  | cons _ _ => exact hcompat

theorem foldPasses_compatible_right {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} {k : IRPoint} {R B C : Type}
    (passes : List (IRPass lang profile k)) (after : Stage (AtLang lang profile k) R B)
    (rest : Pipeline B C)
    (hcompat : ∀ x, AtLang.WF (language := language) x → after.inputInvariant x) :
    (foldPasses (language := language) passes).Compatible (.next after rest) := by
  induction passes with
  | nil => trivial
  | cons _ tail ih =>
    cases tail with
    | nil => exact hcompat
    | cons _ _ => exact ih

private theorem prependStageOverPasses_validated
    {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} {k : IRPoint} {A R S B C : Type}
    (edge : Stage A R (AtLang lang profile k))
    (passes : List (IRPass lang profile k))
    (after : Stage (AtLang lang profile k) S B)
    (rest : Pipeline B C)
    (hedge : edge.Validated)
    (hpasses : ∀ p ∈ passes, (p.toStage (language := language)).Validated)
    (htail : (Pipeline.next after rest).Validated)
    (hleft : ∀ x, edge.outputInvariant x → AtLang.WF (language := language) x)
    (hright : ∀ x, AtLang.WF (language := language) x → after.inputInvariant x) :
    (Pipeline.next edge (pipelineAppend (foldPasses (language := language) passes)
      (.next after rest))).Validated := by
  have hApp : (pipelineAppend (foldPasses (language := language) passes) (.next after rest)).Validated :=
    pipelineAppend_validated ((foldPasses_validated_iff passes).mpr hpasses) htail
      (foldPasses_compatible_right passes after rest hright)
  cases passes with
  | nil =>
    exact .next hedge (fun x hx => hright x (hleft x hx)) hApp
  | cons _ _ =>
    exact .next hedge hleft hApp

/-! ### Two-module LLVM premise `P_print` and single selected emitter -/

/-- Maximal observations of the external LLVM execution. The event type is
fixed by the source language at this edge. -/
inductive Behavior (E : Type) where
  | terminates (result : Concrete.Contracts.Observation) (trace : List (Concrete.Contracts.ProcessEvent E))
  | diverges (trace : Nat → List (Concrete.Contracts.ProcessEvent E))
  | wrong

/-- Trusted interpretation boundary for the exact emitted two-module LLVM package
bytes (`.component` and `.launcher`). Lean does not interpret LLVM here. The opaque
default only supplies an inhabitant. -/
opaque llvmBehaviors {E : Type} : LLVMPackageBytes → Behavior E → Prop := default

def P_print (bir : BIR lang profile) (output : LLVMInput profile) : Prop :=
  let source := BIR.Execution (language := language)
  ProgramSafe source bir →
    ∀ behavior : Behavior language.operands.Event, llvmBehaviors output.packageBytes behavior →
      match behavior with
      | .terminates result trace =>
        ∃ p, source.finite bir p ∧ source.result bir p result ∧ source.trace bir p = trace
      | .diverges trace =>
        ∃ run, source.diverges bir run ∧
          Concrete.Contracts.SameInfiniteTrace (fun i => source.trace bir (run i)) trace
      | .wrong => False

private def LCBInput.validateJobs (output : LCBInput profile) :
    Except LeanCompilerBackend.Verified.CompileError Unit := do
  LeanCompilerBackend.Verified.validateJob (Backend.LCBJob.lcbFunctionJob output.body)
  output.additionalJobs.forM fun pair => LeanCompilerBackend.Verified.validateJob pair.2

/-- The hosted LCB word reserves 0 through 255 for ordinary exits. -/
def LCBInput.trapCapacity : Nat := 2 ^ 32 - 256

def LCBInput.trapCapacityRefusal (sites : Nat) : ResourceRefusal :=
  ⟨⟨``LCBInput.trapCapacity, 1⟩, sites, LCBInput.trapCapacity⟩

/-- Guarded execution of the profile-selected emitter.
On `.lcb`, enforces in exact order:
1. hosted result-word trap capacity (`LCBInput.trapCapacity`);
2. single-job linking boundary (`BIR.requiresLCBLinking`);
3. frozen backend policy (`firstUnsupported`);
4. emitter execution and keyed LCB job validation (`validateJobs`).
On `.llvm`, enforces `firstUnsupported` before running the emitter. -/
def emitGuarded {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty} :
    {profile : Profile} →
    (BIR lang profile → Except StageFailure (BackendOutput profile)) →
    BIR lang profile → Except StageFailure (BackendOutput profile)
  | ⟨.llvm, target, maxSlots, maxStack⟩, emitFn, bir =>
    match firstUnsupported (language := language) bir with
    | some cause => .error (.unsupportedFeature ⟨.llvm, target, maxSlots, maxStack⟩ cause)
    | none => emitFn bir
  | ⟨.lcb, target, maxSlots, maxStack⟩, emitFn, bir =>
    if LCBInput.trapCapacity < bir.sites.size then
      .error (.resource (LCBInput.trapCapacityRefusal bir.sites.size))
    else if bir.requiresLCBLinking then
      .error .lcbLinkingUnavailable
    else match firstUnsupported (language := language) bir with
    | some cause => .error (.unsupportedFeature ⟨.lcb, target, maxSlots, maxStack⟩ cause)
    | none => do
      let output ← emitFn bir
      output.validateJobs.mapError StageFailure.invalidLCBJob
      return output

def PassFunctions.lowerUncheckedEntry (passes : PassFunctions grammar language lang profile) :
    Stage (CoreInput language) StageFailure (MIR lang profile) where
  run := passes.lowerUnchecked
  kind := .directProof
  inputInvariant := CoreInput.UncheckedWF
  outputInvariant := MIR.HandoffWF (language := language)
  success := LowersUnchecked (language := language)
  refusal _ _ := False

/-- Single profile-selected backend emitter stage. -/
def PassFunctions.emitEntry :
    {profile : Profile} →
    PassFunctions grammar language lang profile →
    Stage (BIR lang profile) StageFailure (BackendOutput profile)
  | ⟨.llvm, target, maxSlots, maxStack⟩, passes =>
    { run := emitGuarded (language := language) passes.emit
      kind := .namedPremise "P_print"
      inputInvariant := BIR.WF (language := language)
      outputInvariant := fun output => ∃ bir : BIR lang ⟨.llvm, target, maxSlots, maxStack⟩, LLVMMetadataMatches bir output
      success := fun bir output =>
        firstUnsupported (language := language) bir = none ∧
        LLVMMetadataMatches bir output
      refusal := fun bir failure =>
        ∃ cause, firstUnsupported (language := language) bir = some cause ∧
          failure = .unsupportedFeature ⟨.llvm, target, maxSlots, maxStack⟩ cause
      trust := P_print (language := language) }
  | ⟨.lcb, target, maxSlots, maxStack⟩, passes =>
    { run := emitGuarded (language := language) passes.emit
      kind := .directProof
      inputInvariant := BIR.WF (language := language)
      outputInvariant := fun output => ∃ bir : BIR lang ⟨.lcb, target, maxSlots, maxStack⟩, EmitsLCB language bir output
      success := fun bir output =>
        firstUnsupported (language := language) bir = none ∧
        EmitsLCB language bir output
      refusal := fun bir failure =>
        (LCBInput.trapCapacity < bir.sites.size ∧
          failure = .resource (LCBInput.trapCapacityRefusal bir.sites.size)) ∨
        (¬ (LCBInput.trapCapacity < bir.sites.size) ∧
          bir.requiresLCBLinking = true ∧
          failure = .lcbLinkingUnavailable) ∨
        (¬ (LCBInput.trapCapacity < bir.sites.size) ∧
          bir.requiresLCBLinking = false ∧
          ∃ cause, firstUnsupported (language := language) bir = some cause ∧
            failure = .unsupportedFeature ⟨.lcb, target, maxSlots, maxStack⟩ cause) ∨
        (¬ (LCBInput.trapCapacity < bir.sites.size) ∧
          bir.requiresLCBLinking = false ∧
          firstUnsupported (language := language) bir = none ∧
          ∃ reason output, passes.emit bir = .ok output ∧
            LCBEncoding language bir output ∧ output.validateJobs = .error reason ∧
            failure = .invalidLCBJob reason)
      trust := fun _ _ => True }

theorem PassFunctions.emitEntry_inputInvariant {profile : Profile}
    (passes : PassFunctions grammar language lang profile) (bir : BIR lang profile) :
    (passes.emitEntry).inputInvariant bir ↔ BIR.WF (language := language) bir := by
  cases profile with
  | mk backend _ _ _ => cases backend <;> exact Iff.rfl

theorem PassFunctions.emitEntry_trusted_of_lcb {profile : Profile}
    (passes : PassFunctions grammar language lang profile) (hLCB : profile.backend = .lcb) :
    (passes.emitEntry).Trusted := by
  cases profile with
  | mk backend target maxSlots maxStack =>
    cases backend
    · cases hLCB
    · intro _ _ _ _; trivial

/-! ### Registry, private `RoutePipeline`, and end-to-end composition -/

/-- Only pass implementations vary at the public boundary. Their semantic and
grammar indices are selected by the repository. -/
abbrev Passes (slice : Slice) (profile : Profile) :=
  PassFunctions (sliceGrammar slice) (sliceLanguage slice) (sliceLoweringLanguage slice) profile

/-- Each field is a stage or intra-IR pass pipeline at the repository-selected slice
and profile. Construction is confined to `registry`, which installs each fixed contract. -/
structure Registry (slice : Slice) (profile : Profile) where
  private mk ::
  lexer : Stage Source LexRefusal TokenStream
  parser : Stage TokenStream ParseRefusal (Parsed ((sliceGrammar slice).value (sliceGrammar slice).start))
  elaborator : Stage (Parsed ((sliceGrammar slice).value (sliceGrammar slice).start)) ElabRefusal (AdmissionInput (sliceLanguage slice))
  eligibility : Stage (AdmissionInput (sliceLanguage slice)) StageFailure (CoreInput (sliceLanguage slice))
  typing : Stage (CoreInput (sliceLanguage slice)) StageFailure (CoreInput (sliceLanguage slice))
  admission : AdmissionEntry (sliceLanguage slice) slice
  lowerCore : Stage (Admitted (sliceLanguage slice)) StageFailure (MIR (sliceLoweringLanguage slice) profile)
  lowerUnchecked : Stage (CoreInput (sliceLanguage slice)) StageFailure (MIR (sliceLoweringLanguage slice) profile)
  mirPasses : Pipeline (MIR (sliceLoweringLanguage slice) profile) (MIR (sliceLoweringLanguage slice) profile)
  lowerMemory : Stage (MIR (sliceLoweringLanguage slice) profile) StageFailure (Memory (sliceLoweringLanguage slice) profile)
  memoryPasses : Pipeline (Memory (sliceLoweringLanguage slice) profile) (Memory (sliceLoweringLanguage slice) profile)
  buildSSA : Stage (Memory (sliceLoweringLanguage slice) profile) StageFailure (SSA (sliceLoweringLanguage slice) profile)
  ssaPasses : Pipeline (SSA (sliceLoweringLanguage slice) profile) (SSA (sliceLoweringLanguage slice) profile)
  buildBIR : Stage (SSA (sliceLoweringLanguage slice) profile) StageFailure (BIR (sliceLoweringLanguage slice) profile)
  birPasses : Pipeline (BIR (sliceLoweringLanguage slice) profile) (BIR (sliceLoweringLanguage slice) profile)
  emit : Stage (BIR (sliceLoweringLanguage slice) profile) StageFailure (BackendOutput profile)

def registry {slice : Slice} (passes : Passes slice profile) : Registry slice profile where
  lexer := { passes.lexer with
    inputInvariant := fun input => Source.WF input ∧ input.slice = slice
    outputInvariant := fun stream =>
      stream.parserView.WF ∧ TokensOrdered stream.parserView ∧ stream.source.slice = slice }
  parser := { parserEntry (sliceParser slice) (sliceParser_nonExhausted slice) with
    inputInvariant := fun stream =>
      stream.parserView.WF ∧ TokensOrdered stream.parserView ∧ stream.source.slice = slice
    outputInvariant := Parsed.ElaborationDomain slice }
  elaborator :=
    { run := passes.elaborate
      kind := .directProof
      inputInvariant := Parsed.ElaborationDomain slice
      outputInvariant := (·.Ready slice)
      success := fun ast input =>
        SliceElaboration slice ast.source.project (cast (sliceGrammarStartShape slice) ast.tree) (.ok input)
      refusal := fun ast failure =>
        SliceElaboration slice ast.source.project (cast (sliceGrammarStartShape slice) ast.tree) (.error failure) }
  eligibility := eligibilityEntry _ slice
  typing := typingEntry _ slice
  admission := passes.admissionEntry slice
  lowerCore := ⟨passes.lowerCore, .directProof, Admitted.Valid,
    MIR.HandoffWF (language := sliceLanguage slice), LowersCore (language := sliceLanguage slice),
    fun _ _ => False, fun _ _ => True⟩
  lowerUnchecked := passes.lowerUncheckedEntry
  mirPasses := foldPasses (language := sliceLanguage slice) (passes.passesAt .mir)
  lowerMemory := ⟨passes.lowerMemory, .directProof, MIR.HandoffWF (language := sliceLanguage slice),
    Memory.HandoffWF, (fun mir memory =>
      LowersMemory (language := sliceLanguage slice) mir memory ∧ memory.mutableLocals = mir.mutableLocals),
    fun _ _ => False, fun _ _ => True⟩
  memoryPasses := foldPasses (language := sliceLanguage slice) (passes.passesAt .memory)
  buildSSA := ⟨passes.buildSSA, .provedValidator, Memory.HandoffWF,
    SSA.WF (language := sliceLanguage slice), (fun memory ssa =>
      BuildsSSA (language := sliceLanguage slice) memory ssa ∧ ssa.mutableLocals = memory.mutableLocals),
    fun _ _ => False, fun _ _ => True⟩
  ssaPasses := foldPasses (language := sliceLanguage slice) (passes.passesAt .ssa)
  buildBIR := ⟨passes.buildBIR, .provedValidator, SSA.WF (language := sliceLanguage slice),
    BIR.WF (language := sliceLanguage slice), (fun ssa bir =>
      BuildsBIR (language := sliceLanguage slice) ssa bir ∧ bir.mutableLocals = ssa.mutableLocals),
    fun ssa failure =>
      match failure with
      | .resource bound =>
        bound.rule = ⟨``BIR.frameSlots, 1⟩ ∧ bound.limit = profile.maxBIRSlots ∧
          bound.measured = ssa.frameSlots ∧ bound.limit < bound.measured
      | _ => False,
    fun _ _ => True⟩
  birPasses := foldPasses (language := sliceLanguage slice) (passes.passesAt .bir)
  emit := passes.emitEntry

inductive Edge where
  | lexer | parser | elaborator | eligibility | typing | admission | lowerCore | lowerUnchecked
  | lowerMemory | buildSSA | buildBIR | emit
  deriving DecidableEq, Repr

/-- Soundness refers to the selected repository contract for the executable edge. -/
def Stage.Sound {slice : Slice} (passes : Passes slice profile) : Edge → Prop
  | .lexer => (registry passes).lexer.Checks
  | .parser => (registry passes).parser.Checks
  | .elaborator => (registry passes).elaborator.Checks
  | .eligibility => (registry passes).eligibility.Checks
  | .typing => (registry passes).typing.Checks
  | .admission => (registry passes).admission.Checks
  | .lowerCore => (registry passes).lowerCore.Checks
  | .lowerUnchecked => (registry passes).lowerUnchecked.Checks
  | .lowerMemory => (registry passes).lowerMemory.Checks
  | .buildSSA => (registry passes).buildSSA.Checks
  | .buildBIR => (registry passes).buildBIR.Checks
  | .emit => (registry passes).emit.Checks

/-- Registered end-to-end pipeline statically indexed by slice, full compilation profile
(backend, platform target, BIR slot budget, and image stack budget), and route
(`.checked` vs `.unchecked`). Construction is private to `Concrete.Interfaces.Pipeline`,
so callers can only obtain a `RoutePipeline` via `Registry.pipeline`. -/
structure RoutePipeline (slice : Slice) (profile : Profile) (route : Route) where
  private mk ::
  pipeline : Pipeline Source (BackendOutput profile)

def RoutePipeline.run {slice : Slice} {profile : Profile} {route : Route}
    (rp : RoutePipeline slice profile route) (input : Source) :
    Except (Σ R : Type, R) (BackendOutput profile) :=
  rp.pipeline.run input

def RoutePipeline.Validated {slice : Slice} {profile : Profile} {route : Route}
    (rp : RoutePipeline slice profile route) : Prop :=
  rp.pipeline.Validated

def RoutePipeline.Trusted {slice : Slice} {profile : Profile} {route : Route}
    (rp : RoutePipeline slice profile route) : Prop :=
  rp.pipeline.Trusted

def RoutePipeline.Sound {slice : Slice} {profile : Profile} {route : Route}
    (rp : RoutePipeline slice profile route) : Prop :=
  rp.pipeline.Sound

def RoutePipeline.Ready {slice : Slice} {profile : Profile} {route : Route}
    (rp : RoutePipeline slice profile route) (input : Source) : Prop :=
  rp.pipeline.Ready input

def RoutePipeline.Corresponds {slice : Slice} {profile : Profile} {route : Route}
    (rp : RoutePipeline slice profile route) (input : Source) (output : BackendOutput profile) : Prop :=
  rp.pipeline.Corresponds input output

private def Registry.loweringChain {slice : Slice} (r : Registry slice profile) :
    Pipeline (MIR (sliceLoweringLanguage slice) profile) (BackendOutput profile) :=
  pipelineAppend r.mirPasses <|
  .next r.lowerMemory <|
  pipelineAppend r.memoryPasses <|
  .next r.buildSSA <|
  pipelineAppend r.ssaPasses <|
  .next r.buildBIR <|
  pipelineAppend r.birPasses <|
  .next r.emit .done

private def Registry.chain {slice : Slice} (r : Registry slice profile) (route : Route) :
    Pipeline Source (BackendOutput profile) :=
  let lowering := r.loweringChain
  let core := match route with
    | .checked => Pipeline.next r.admission.toStage (.next r.lowerCore lowering)
    | .unchecked => Pipeline.next r.eligibility (.next r.typing (.next r.lowerUnchecked lowering))
  .next r.lexer (.next r.parser (.next r.elaborator core))

/-- Assemble the registered `RoutePipeline` for `(slice, profile, route)`, folding all
four `passesAt` lists directly after MIR, Memory, SSA, and BIR on both checked and
unchecked routes and terminating in the single profile-selected `emit` stage. -/
def Registry.pipeline {slice : Slice} (passes : Passes slice profile) (route : Route) :
    RoutePipeline slice profile route :=
  ⟨(registry passes).chain route⟩

private theorem Registry.fromLowerMemory_validated {slice : Slice} {profile : Profile}
    (passes : Passes slice profile)
    (hLowerMemory : (registry passes).lowerMemory.Validated)
    (hMemPasses : ∀ p ∈ passes.passesAt .memory, (p.toStage (language := sliceLanguage slice)).Validated)
    (hBuildSSA : (registry passes).buildSSA.Validated)
    (hSsaPasses : ∀ p ∈ passes.passesAt .ssa, (p.toStage (language := sliceLanguage slice)).Validated)
    (hBuildBIR : (registry passes).buildBIR.Validated)
    (hBirPasses : ∀ p ∈ passes.passesAt .bir, (p.toStage (language := sliceLanguage slice)).Validated)
    (hEmit : (registry passes).emit.Validated) :
    (Pipeline.next (registry passes).lowerMemory
      (pipelineAppend (registry passes).memoryPasses
        (.next (registry passes).buildSSA
          (pipelineAppend (registry passes).ssaPasses
            (.next (registry passes).buildBIR
              (pipelineAppend (registry passes).birPasses
                (.next (registry passes).emit .done))))))).Validated := by
  have hFromBuildBIR :=
    prependStageOverPasses_validated (language := sliceLanguage slice) (k := .bir)
      (registry passes).buildBIR (passes.passesAt .bir) (registry passes).emit .done
      hBuildBIR hBirPasses (.last hEmit)
      (fun _ h => h)
      (fun bir h => (passes.emitEntry_inputInvariant bir).mpr h)
  have hFromBuildSSA :=
    prependStageOverPasses_validated (language := sliceLanguage slice) (k := .ssa)
      (registry passes).buildSSA (passes.passesAt .ssa) (registry passes).buildBIR _
      hBuildSSA hSsaPasses hFromBuildBIR
      (fun _ h => h) (fun _ h => h)
  exact prependStageOverPasses_validated (language := sliceLanguage slice) (k := .memory)
    (registry passes).lowerMemory (passes.passesAt .memory) (registry passes).buildSSA _
    hLowerMemory hMemPasses hFromBuildSSA
    (fun _ h => h) (fun _ h => h)

theorem Registry.pipeline_validated_checked {slice : Slice} {profile : Profile}
    (passes : Passes slice profile)
    (hLex : (registry passes).lexer.Validated)
    (hParse : (registry passes).parser.Validated)
    (hElab : (registry passes).elaborator.Validated)
    (hAdmit : (registry passes).admission.toStage.Validated)
    (hLowerCore : (registry passes).lowerCore.Validated)
    (hMirPasses : ∀ p ∈ passes.passesAt .mir, (p.toStage (language := sliceLanguage slice)).Validated)
    (hLowerMemory : (registry passes).lowerMemory.Validated)
    (hMemPasses : ∀ p ∈ passes.passesAt .memory, (p.toStage (language := sliceLanguage slice)).Validated)
    (hBuildSSA : (registry passes).buildSSA.Validated)
    (hSsaPasses : ∀ p ∈ passes.passesAt .ssa, (p.toStage (language := sliceLanguage slice)).Validated)
    (hBuildBIR : (registry passes).buildBIR.Validated)
    (hBirPasses : ∀ p ∈ passes.passesAt .bir, (p.toStage (language := sliceLanguage slice)).Validated)
    (hEmit : (registry passes).emit.Validated) :
    (Registry.pipeline passes .checked).Validated := by
  have hFromLowerMem := Registry.fromLowerMemory_validated passes
    hLowerMemory hMemPasses hBuildSSA hSsaPasses hBuildBIR hBirPasses hEmit
  have hCore := prependStageOverPasses_validated (language := sliceLanguage slice) (k := .mir)
    (registry passes).lowerCore (passes.passesAt .mir) (registry passes).lowerMemory _
    hLowerCore hMirPasses hFromLowerMem
    (fun _ h => h) (fun _ h => h)
  exact .next hLex (fun _ h => h)
    (.next hParse (fun _ h => h)
      (.next hElab (fun _ h => h)
        (.next hAdmit (fun _ h => h) hCore)))

theorem Registry.pipeline_validated_unchecked {slice : Slice} {profile : Profile}
    (passes : Passes slice profile)
    (hLex : (registry passes).lexer.Validated)
    (hParse : (registry passes).parser.Validated)
    (hElab : (registry passes).elaborator.Validated)
    (hElig : (registry passes).eligibility.Validated)
    (hTyp : (registry passes).typing.Validated)
    (hLowerUnchecked : (registry passes).lowerUnchecked.Validated)
    (hMirPasses : ∀ p ∈ passes.passesAt .mir, (p.toStage (language := sliceLanguage slice)).Validated)
    (hLowerMemory : (registry passes).lowerMemory.Validated)
    (hMemPasses : ∀ p ∈ passes.passesAt .memory, (p.toStage (language := sliceLanguage slice)).Validated)
    (hBuildSSA : (registry passes).buildSSA.Validated)
    (hSsaPasses : ∀ p ∈ passes.passesAt .ssa, (p.toStage (language := sliceLanguage slice)).Validated)
    (hBuildBIR : (registry passes).buildBIR.Validated)
    (hBirPasses : ∀ p ∈ passes.passesAt .bir, (p.toStage (language := sliceLanguage slice)).Validated)
    (hEmit : (registry passes).emit.Validated) :
    (Registry.pipeline passes .unchecked).Validated := by
  have hFromLowerMem := Registry.fromLowerMemory_validated passes
    hLowerMemory hMemPasses hBuildSSA hSsaPasses hBuildBIR hBirPasses hEmit
  have hUnchecked := prependStageOverPasses_validated (language := sliceLanguage slice) (k := .mir)
    (registry passes).lowerUnchecked (passes.passesAt .mir) (registry passes).lowerMemory _
    hLowerUnchecked hMirPasses hFromLowerMem
    (fun _ h => h) (fun _ h => h)
  exact .next hLex (fun _ h => h)
    (.next hParse (fun _ h => h)
      (.next hElab (fun _ h => h)
        (.next hElig (fun _ h => h)
          (.next hTyp (fun _ h => h.2) hUnchecked))))

/-- On any LCB profile, every registered stage and every folded intra-IR pass has
trivial trust, so `Registry.pipeline passes route` is unconditionally `Trusted`. -/
theorem Registry.pipeline_trusted_of_lcb {slice : Slice} {profile : Profile}
    (passes : Passes slice profile) (route : Route) (hLCB : profile.backend = .lcb) :
    (Registry.pipeline passes route).Trusted := by
  have hLow : (registry passes).loweringChain.Trusted := by
    unfold Registry.loweringChain
    rw [pipelineAppend_trusted]
    refine ⟨foldPasses_trusted _, ⟨fun _ _ _ _ => trivial, ?_⟩⟩
    rw [pipelineAppend_trusted]
    refine ⟨foldPasses_trusted _, ⟨fun _ _ _ _ => trivial, ?_⟩⟩
    rw [pipelineAppend_trusted]
    refine ⟨foldPasses_trusted _, ⟨fun _ _ _ _ => trivial, ?_⟩⟩
    rw [pipelineAppend_trusted]
    exact ⟨foldPasses_trusted _, ⟨passes.emitEntry_trusted_of_lcb hLCB, trivial⟩⟩
  cases route with
  | checked =>
    exact ⟨fun _ _ _ _ => trivial, fun _ _ _ _ => trivial, fun _ _ _ _ => trivial,
      fun _ _ _ _ => trivial, fun _ _ _ _ => trivial, hLow⟩
  | unchecked =>
    exact ⟨fun _ _ _ _ => trivial, fun _ _ _ _ => trivial, fun _ _ _ _ => trivial,
      fun _ _ _ _ => trivial, fun _ _ _ _ => trivial, fun _ _ _ _ => trivial, hLow⟩

theorem Pipeline.endToEnd {slice : Slice} (passes : Passes slice profile) (route : Route)
    {input : Source} {output : BackendOutput profile}
    (validated : (Registry.pipeline passes route).Validated)
    (trusted : (Registry.pipeline passes route).Trusted)
    (ready : (Registry.pipeline passes route).Ready input)
    (ran : (Registry.pipeline passes route).run input = .ok output) :
    (Registry.pipeline passes route).Corresponds input output :=
  Pipeline.compose validated trusted ready ran

/-- The guarded LCB pipeline composes from validation alone on both checked and
unchecked routes and with arbitrary intra-IR pass lists. Any program that exceeds
hosted trap capacity, needs linked execution, or uses an unsupported backend feature
is refused by the registered backend edge. -/
theorem registeredLCBEndToEnd {slice : Slice} {profile : Profile}
    (passes : Passes slice profile) (route : Route)
    (hLCB : profile.backend = .lcb)
    {input : Source} {output : BackendOutput profile}
    (validated : (Registry.pipeline passes route).Validated)
    (ready : (Registry.pipeline passes route).Ready input)
    (ran : (Registry.pipeline passes route).run input = .ok output) :
    (Registry.pipeline passes route).Corresponds input output :=
  Pipeline.compose validated (Registry.pipeline_trusted_of_lcb passes route hLCB) ready ran

/-! ### Inline F7 freeze controls -/

-- Slice substitution on `Registry.admission` must fail during elaboration.
example {profile} (passes : Passes ⟨⟨3, by decide⟩⟩ profile) : Registry ⟨⟨3, by decide⟩⟩ profile := by
  fail_if_success exact {registry passes with admission := passes.admissionEntry sliceOne}
  exact {registry passes with admission := passes.admissionEntry ⟨⟨3, by decide⟩⟩}

-- `RoutePipeline` rejects cross-slice, cross-route, cross-backend, cross-slot-budget,
-- cross-stack-budget, and cross-platform substitutions at elaboration time.
example (passes : Passes slice0 ⟨.lcb, .x86_64Linux, 8, 4096⟩) :
    RoutePipeline slice0 ⟨.lcb, .x86_64Linux, 8, 4096⟩ .checked := by
  let rp := Registry.pipeline passes .checked
  fail_if_success exact (rp : RoutePipeline sliceOne ⟨.lcb, .x86_64Linux, 8, 4096⟩ .checked)
  fail_if_success exact (rp : RoutePipeline slice0 ⟨.lcb, .x86_64Linux, 8, 4096⟩ .unchecked)
  fail_if_success exact (rp : RoutePipeline slice0 ⟨.llvm, .x86_64Linux, 8, 4096⟩ .checked)
  fail_if_success exact (rp : RoutePipeline slice0 ⟨.lcb, .x86_64Linux, 4, 4096⟩ .checked)
  fail_if_success exact (rp : RoutePipeline slice0 ⟨.lcb, .x86_64Linux, 8, 8192⟩ .checked)
  fail_if_success exact (rp : RoutePipeline slice0 ⟨.lcb, .aarch64Linux, 8, 4096⟩ .checked)
  exact rp

-- Empty and cons `foldPasses` + identity fallback control on declined candidates.
example {slice : Slice} {profile : Profile} {k : IRPoint} (x : At slice profile k)
    (hwf : At.WF x) :
    let decline : IRPass (sliceLoweringLanguage slice) profile k := fun _ => none
    (foldPasses (language := sliceLanguage slice) ([] : List (IRPass (sliceLoweringLanguage slice) profile k))).run x = .ok x ∧
    (foldPasses (language := sliceLanguage slice) [decline, decline]).run x = .ok x ∧
    (foldPasses (language := sliceLanguage slice) [decline, decline]).Validated ∧
    PassPost x x (Account.identityFor x) :=
  ⟨rfl, rfl,
    foldPasses_cons_validated IRPass.declined_validated
      (foldPasses_cons_validated IRPass.declined_validated foldPasses_nil_validated),
    PassPost.identity hwf⟩

-- Two-module LLVM `P_print` binds `output.packageBytes` (both `.component` and `.launcher`).
example {profile : Profile} (bir : BIR (sliceLoweringLanguage slice0) profile)
    (out : LLVMInput profile) :
    out.packageBytes.component = out.bytesAt .component ∧
    out.packageBytes.launcher = out.bytesAt .launcher ∧
    (P_print (language := sliceLanguage slice0) bir out →
      ProgramSafe (BIR.Execution (language := sliceLanguage slice0)) bir →
      ¬ llvmBehaviors (E := (sliceLanguage slice0).operands.Event) out.packageBytes .wrong) :=
  ⟨rfl, rfl, fun hp hsafe hwrong => hp hsafe .wrong hwrong⟩

end Concrete.Interfaces
