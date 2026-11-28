import ConcreteSpec.StageGrammar
import ConcreteSpec.Proofs.SliceGrammar
import Concrete.Interfaces.SurfaceElaboration
import Concrete.Lex
import Concrete.Lex.StreamProofs
import Concrete.Refusal

/-! Driver parser cutover to the certified 78-production `sliceParser slice0`.

There is one parser: `ConcreteSpec.StageGrammar.sliceParser slice0`, paired with
the frozen single-pass CST decoder `Concrete.Elab.surfaceProgram` (`decodeSurface0`)
and the lossless scanner `Concrete.Lex.lexStream`. All hand-written Slice 0 tables
and secondary refusal parsers are retired. -/

namespace Concrete.Parse

open ConcreteSpec.Surface0
open ConcreteSpec.StageInterface
open ConcreteSpec.StageGrammar
open ConcreteSpec.Core (RuleId RegisteredRule SourceText mainSourcePath)
open ConcreteSpec.Core.SourcePathSpan (Project File)
open ConcreteSpec.GrammarData (Nonterminal ProductionId)
open ConcreteSpec.GrammarVerification (earlyProposalTree earlyRawGrammar)
open Concrete.LL1

/-- Convert a lex-stage refusal into the driver's refusal vocabulary. -/
def ofLexRefusal : Concrete.Interfaces.LexRefusal → Refusal
  | .missingFile fileId => .internal s!"unknown project file ID {fileId.index}"
  | .source cause => .source cause

/-- Project a retained `TokenStream` to its `Surface0.Token` list, matching
`ConcreteSpec.StageGrammar.toSurface0Token`. -/
def surfaceTokens (stream : TokenStream) : List Token :=
  stream.parserView.tokens.toList.map toSurface0Token

/-- Run a certified early parser engine and decode its Slice 0 CST with the frozen
`Elab.surfaceProgram`, retaining the exact registry `ParseRefusal` on refusal. -/
def parseStreamWithRefusalFor (parser : ParserEngine earlySharedGrammar)
    (stream : TokenStream) : Except (Option ParseRefusal) Program :=
  match (parser.run stream.parserView).2 with
  | none => .error none
  | some (.error refusal) => .error (some refusal)
  | some (.ok parsed) =>
    match parsed.tree with
    | .inl tree =>
      match Concrete.Elab.surfaceProgram tree with
      | some program => .ok program
      | none => .error none
    | .inr empty => nomatch empty

/-- Run a certified early parser engine on a retained `TokenStream` and map any
refusal into the driver's `Refusal` vocabulary. -/
def parseStreamFor (parser : ParserEngine earlySharedGrammar)
    (stream : TokenStream) : Except Refusal Program :=
  match parseStreamWithRefusalFor parser stream with
  | .ok program => .ok program
  | .error (some refusal) => .error (Refusal.ofParseRefusal refusal)
  | .error none =>
    .error (.source (.parser (.internal (.advance 0)) (eofFileSpan stream.source).surface))

/-- Run the registered `sliceParser slice0` and frozen `Elab.surfaceProgram`,
retaining the exact `ParseRefusal` on failure. -/
def parseStreamWithRefusal (stream : TokenStream) : Except (Option ParseRefusal) Program :=
  parseStreamWithRefusalFor (sliceParser slice0) stream

/-- Parse a retained `TokenStream` through the registered `sliceParser slice0`
and frozen `Elab.surfaceProgram`. -/
def parseStream (stream : TokenStream) : Except Refusal Program :=
  parseStreamFor (sliceParser slice0) stream

/-- Driver front-end parser: lexes `source` through `Lex.lexStream`, runs
`sliceParser slice0`, and decodes through the frozen `Elab.surfaceProgram`. -/
def parseForDriver (source : Source) : Except Refusal Program := do
  let stream ← (Concrete.Lex.lexStream source).mapError ofLexRefusal
  parseStream stream

/-- Build a single-file Slice 0 `Source` using the canonical `main.con` path. -/
def sourceOfBytes (bytes : ByteArray) : Source :=
  ⟨Project.singleton mainSourcePath (SourceText.ofBytes bytes.data.toList), ⟨0⟩, slice0⟩

/-- Lex and parse raw in-memory bytes as a single-file Slice 0 source through
`parseForDriver`. -/
def parseBytes (bytes : ByteArray) : Except Refusal Program :=
  parseForDriver (sourceOfBytes bytes)

/-! ## Lex-stream token projection agreement -/

private theorem runs_prepend_tokens (p : Lex.Bridge.RetainedPiece) (runs : Lex.Bridge.Runs) :
    (runs.prepend p).entries.map Prod.snd =
      match p with
      | .inl _ => runs.entries.map Prod.snd
      | .inr t => t :: runs.entries.map Prod.snd := by
  cases p with
  | inr t => rfl
  | inl raw =>
    cases runs with
    | mk entries trailing =>
      cases entries <;> rfl

private theorem group_tokens (pieces : List Lex.Bridge.RetainedPiece) :
    (Lex.Bridge.group pieces).entries.map Prod.snd =
      pieces.filterMap fun
        | .inl _ => none
        | .inr t => some t := by
  induction pieces with
  | nil => rfl
  | cons p rest ih =>
    change ((Lex.Bridge.group rest).prepend p).entries.map Prod.snd = _
    rw [runs_prepend_tokens, ih]
    cases p <;> rfl

private theorem retainPiece_surfaceToken (ref : Lex.LosslessStream)
    (piece : {p // p ∈ ref.pieces}) :
    (match Lex.Bridge.retainPiece ref piece with
      | .inl _ => none
      | .inr t => some (toSurface0Token t.decoded)) = piece.val.parserToken := by
  rcases piece with ⟨p, hp⟩
  cases p with
  | whitespace s => rfl
  | lineComment s => rfl
  | token val s =>
    cases val with
    | parsed v =>
      cases v with
      | fixed kind plain =>
        cases s
        rfl
      | name name => rfl
      | integer n radix suffix => rfl
    | stringLiteral text => rfl
    | byteLiteral byte => rfl
    | label name => rfl

private theorem filterMap_attach_eq {α β : Type} (xs : List α)
    (f : {x // x ∈ xs} → Option β) (g : α → Option β)
    (h : ∀ p : {x // x ∈ xs}, f p = g p.val) :
    xs.attach.filterMap f = xs.filterMap g := by
  induction xs with
  | nil => rfl
  | cons x rest ih =>
    simp only [List.attach_cons, List.filterMap_cons, List.filterMap_map, Function.comp_def]
    rw [h ⟨x, List.mem_cons_self⟩]
    have hrest := ih (fun p => f ⟨p.val, List.mem_cons_of_mem _ p.property⟩)
      (fun p => h ⟨p.val, List.mem_cons_of_mem _ p.property⟩)
    cases g x <;> simp [hrest]

private theorem retain_surfaceTokens (source : Source) (ref : Lex.LosslessStream) :
    surfaceTokens (Lex.Bridge.retain source ref) = ref.parserView := by
  unfold surfaceTokens TokenStream.parserView Lex.Bridge.retain Lex.Bridge.Runs.stream
    Lex.LosslessStream.parserView
  simp only [List.map_map, Function.comp_def]
  have hgrp :
      ((Lex.Bridge.group (ref.pieces.attach.map (Lex.Bridge.retainPiece ref))).entries.map Prod.snd) =
        (ref.pieces.attach.map (Lex.Bridge.retainPiece ref)).filterMap (fun
          | .inl _ => none
          | .inr t => some t) :=
    group_tokens _
  calc
    List.map (fun x => toSurface0Token x.2.decoded)
        (Lex.Bridge.group (ref.pieces.attach.map (Lex.Bridge.retainPiece ref))).entries
      = ((Lex.Bridge.group (ref.pieces.attach.map (Lex.Bridge.retainPiece ref))).entries.map Prod.snd).map
          (fun t => toSurface0Token t.decoded) := by
            rw [List.map_map]; rfl
    _ = ((ref.pieces.attach.map (Lex.Bridge.retainPiece ref)).filterMap (fun
          | .inl _ => none
          | .inr t => some t)).map (fun t => toSurface0Token t.decoded) := by
            rw [hgrp]
    _ = ref.pieces.attach.filterMap (fun piece =>
          match Lex.Bridge.retainPiece ref piece with
          | .inl _ => none
          | .inr t => some (toSurface0Token t.decoded)) := by
            rw [List.map_filterMap, List.filterMap_map]
            congr 1
            funext piece
            dsimp only [Function.comp_def]
            cases Lex.Bridge.retainPiece ref piece <;> rfl
    _ = ref.pieces.filterMap Lex.Piece.parserToken :=
          filterMap_attach_eq ref.pieces _ Lex.Piece.parserToken (retainPiece_surfaceToken ref)

/-- `Lex.lexStream` preserves the input `Source` verbatim. -/
theorem lexStream_source_eq {source : Source} {stream : TokenStream}
    (hlex : Concrete.Lex.lexStream source = .ok stream) :
    stream.source = source := by
  unfold Concrete.Lex.lexStream at hlex
  split at hlex
  · cases hlex
  · split at hlex
    · cases Except.ok.inj hlex; rfl
    · rename_i e herr
      have hf := Concrete.Lex.lexLossless_failure herr
      cases e with
      | source c =>
        cases c with
        | lexical _ _ => cases hlex
        | parser _ _ | surface _ | core _ _ _ _ => exact False.elim hf
      | path _ | missingLocation _ | backendResource _ _ _ _ | backendProfile _ _
      | toolFailure _ | manifest _ | usageError _ | internal _ => exact False.elim hf

/-- For any loaded project file, `Lex.lexStream` produces the exact same
`Surface0.Token` stream as `Lex.lex`. -/
theorem lexStream_surfaceTokens_eq {source : Source} {file : File} {stream : TokenStream}
    (hfile : source.project.fileAt source.entryFile = some file)
    (hlex : Concrete.Lex.lexStream source = .ok stream) :
    Concrete.Lex.lex ⟨file.source.toBytes.toArray⟩ source.entryFile = .ok (surfaceTokens stream) := by
  unfold Concrete.Lex.lexStream at hlex
  rw [hfile] at hlex
  dsimp only at hlex
  split at hlex
  · rename_i ref hloss
    cases Except.ok.inj hlex
    rw [retain_surfaceTokens]
    unfold Concrete.Lex.lex
    rw [hloss]
    rfl
  · rename_i e herr
    have hf := Concrete.Lex.lexLossless_failure herr
    cases e with
    | source c =>
      cases c with
      | lexical _ _ => cases hlex
      | parser _ _ | surface _ | core _ _ _ _ => exact False.elim hf
    | path _ | missingLocation _ | backendResource _ _ _ _ | backendProfile _ _
    | toolFailure _ | manifest _ | usageError _ | internal _ => exact False.elim hf

/-! ## Certified parser soundness and driver-registry parity -/

theorem parseStreamWithRefusalFor_error_some_iff (parser : ParserEngine earlySharedGrammar)
    (stream : TokenStream) (refusal : ParseRefusal) :
    parseStreamWithRefusalFor parser stream = .error (some refusal) ↔
      (parser.run stream.parserView).2 = some (.error refusal) := by
  unfold parseStreamWithRefusalFor
  cases h : (parser.run stream.parserView).2 with
  | none =>
    constructor <;> intro heq <;> cases heq
  | some res =>
    cases res with
    | error r =>
      constructor
      · intro heq
        cases Option.some.inj (Except.error.inj heq)
        rfl
      · intro heq
        cases Except.error.inj (Option.some.inj heq)
        rfl
    | ok parsed =>
      rcases parsed with ⟨src, tree⟩
      change (match tree with
        | .inl t =>
          match Concrete.Elab.surfaceProgram t with
          | some program => Except.ok program
          | none => Except.error none
        | .inr empty => nomatch empty) = .error (some refusal) ↔ _
      cases tree with
      | inr e => exact nomatch e
      | inl t =>
        dsimp only
        cases hdec : Concrete.Elab.surfaceProgram t with
        | none =>
          constructor <;> intro heq <;> cases heq
        | some p =>
          constructor <;> intro heq <;> cases heq

/-- Every Slice 0 program accepted by `parseStreamFor parser` is derived by
`Surface0.Derives` from the stream's `Surface0.Token` projection. -/
theorem parseStreamFor_derives (parser : ParserEngine earlySharedGrammar)
    {stream : TokenStream} {ast : Program}
    (hslice : stream.source.slice = slice0)
    (hok : parseStreamFor parser stream = .ok ast) :
    Derives (surfaceTokens stream) ast := by
  unfold parseStreamFor parseStreamWithRefusalFor at hok
  cases hrun : (parser.run stream.parserView).2 with
  | none => rw [hrun] at hok; cases hok
  | some res =>
    rw [hrun] at hok
    cases res with
    | error r => cases hok
    | ok parsed =>
      obtain ⟨_, _, tree, prog, htree, hdec, hsurf, _, _, _⟩ :=
        earlyParser_slice0_decodes parser stream.parserView parsed hslice hrun
      dsimp only at hok
      rw [htree] at hok
      change (match match Concrete.Elab.surfaceProgram tree with
        | some program => Except.ok program
        | none => Except.error (none : Option ParseRefusal) with
        | Except.ok program => Except.ok program
        | Except.error (some refusal) => Except.error (Refusal.ofParseRefusal refusal)
        | Except.error none => _) = Except.ok ast at hok
      have hsurfDec : Concrete.Elab.surfaceProgram tree = some prog := hdec
      rw [hsurfDec] at hok
      cases Except.ok.inj hok
      exact hsurf

/-- On a coordinate-valid `ParserView`, any `ParseRefusal` returned by
`ParserEngine.run` is grammar-level (never `.invalidStream`), transports to a
`.source` cause with the identical primary span, and matches the engine's exact
registered rule when the engine carries the pinned early refusal rules. -/
theorem parserEngine_run_error_of_wf (parser : ParserEngine earlySharedGrammar)
    (view : ParserView) (refusal : ParseRefusal)
    (hwf : view.WF)
    (herr : (parser.run view).2 = some (.error refusal))
    (hmal : parser.malformedRule = earlyMalformedRule)
    (hprod : parser.unsupportedProductionRule = earlyUnsupportedProductionRule)
    (hlit : parser.unsupportedLiteralRule = earlyUnsupportedLiteralRule) :
    ∃ cause, RefusalCause.ofParseRefusal? refusal = some cause ∧
      Refusal.ofParseRefusal refusal = .source cause ∧
      cause.citation = refusal.registeredRule ∧
      cause.primary = refusal.span := by
  unfold ParserEngine.run at herr
  have hinv : view.invalidLocation = none := hwf
  rw [hinv] at herr
  dsimp only at herr
  cases hps : parseSymbol parser.table view.tokens view.source.slice (eofFileSpan view.source)
      earlySharedGrammar.start ⟨0, slice0, none, 0⟩ (parser.stepsPerToken * (view.tokens.size + 1)) with
  | mk c value =>
    rw [hps] at herr
    dsimp only at herr
    cases value with
    | error stop =>
      cases stop with
      | exhausted => cases herr
      | malformed =>
        cases Except.error.inj (Option.some.inj herr)
        have hSpan : ((view.tokens[c.offset]?.map Located.span).getD (eofFileSpan view.source)).surface.fileId =
            ((view.tokens[c.offset]?.map Located.span).getD (eofFileSpan view.source)).file := rfl
        split
        · refine ⟨.parser .unexpectedToken _, rfl, rfl, ?_, by cases ((view.tokens[c.offset]?.map Located.span).getD (eofFileSpan view.source)); rfl⟩
          simp [ParseRefusal.registeredRule, hmal, earlyMalformedRule, RefusalCause.citation]
        · refine ⟨.parser .unexpectedEnd _, rfl, rfl, ?_, by cases ((view.tokens[c.offset]?.map Located.span).getD (eofFileSpan view.source)); rfl⟩
          simp [ParseRefusal.registeredRule, hmal, earlyMalformedRule, RefusalCause.citation]
    | ok ast =>
      dsimp only at herr
      cases htrail : (c.offset != view.tokens.size) with
      | true =>
        simp only [htrail, ↓reduceIte, Option.some.injEq, Except.error.injEq] at herr
        subst herr
        refine ⟨.parser .trailingTokens _, rfl, rfl, ?_, by cases ((view.tokens[c.offset]?.map Located.span).getD (eofFileSpan view.source)); rfl⟩
        simp [ParseRefusal.registeredRule, hmal, earlyMalformedRule, RefusalCause.citation]
      | false =>
        simp only [htrail, Bool.false_eq_true, ↓reduceIte] at herr
        cases hhit : c.firstUnsupportedHit with
        | none => rw [hhit] at herr; cases herr
        | some hit =>
          rcases hit with ⟨hitSpan, cause⟩
          rw [hhit] at herr
          cases Except.error.inj (Option.some.inj herr)
          cases cause with
          | production id =>
            refine ⟨.parser .outsideProduction hitSpan.surface, rfl, rfl, ?_, by cases hitSpan; rfl⟩
            simp [ParseRefusal.registeredRule, hprod, earlyUnsupportedProductionRule, RefusalCause.citation]
          | nonDecimal radix =>
            refine ⟨.parser .nonDecimal hitSpan.surface, rfl, rfl, ?_, by cases hitSpan; rfl⟩
            simp [ParseRefusal.registeredRule, hlit, earlyUnsupportedLiteralRule, RefusalCause.citation]

/-- Polymorphic stream-level parity between `parseStreamFor parser` and a raw
`ParserEngine.run` + `Elab.surfaceProgram` execution. -/
theorem parseStreamFor_parity (parser : ParserEngine earlySharedGrammar)
    (stream : TokenStream) (hslice : stream.source.slice = slice0) :
    (∀ parsed, (parser.run stream.parserView).2 = some (.ok parsed) →
      ∃ tree prog, parsed.tree = .inl tree ∧
        Concrete.Elab.surfaceProgram tree = some prog ∧
        parseStreamWithRefusalFor parser stream = .ok prog ∧
        parseStreamFor parser stream = .ok prog ∧
        Derives (surfaceTokens stream) prog) ∧
    (∀ refusal, (parser.run stream.parserView).2 = some (.error refusal) →
      parseStreamWithRefusalFor parser stream = .error (some refusal) ∧
      parseStreamFor parser stream = .error (Refusal.ofParseRefusal refusal)) := by
  constructor
  · intro parsed hrun
    obtain ⟨_, _, tree, prog, htree, hdec, hsurf, _, _, _⟩ :=
      earlyParser_slice0_decodes parser stream.parserView parsed hslice hrun
    have hsurfDec : Concrete.Elab.surfaceProgram tree = some prog := hdec
    have hraw : parseStreamWithRefusalFor parser stream = .ok prog := by
      unfold parseStreamWithRefusalFor
      rw [hrun]
      dsimp only
      rw [htree]
      dsimp only
      rw [hsurfDec]
    have hdrv : parseStreamFor parser stream = .ok prog := by
      unfold parseStreamFor
      rw [hraw]
    exact ⟨tree, prog, htree, hsurfDec, hraw, hdrv, hsurf⟩
  · intro refusal hrun
    have hraw : parseStreamWithRefusalFor parser stream = .error (some refusal) :=
      (parseStreamWithRefusalFor_error_some_iff parser stream refusal).mpr hrun
    have hdrv : parseStreamFor parser stream = .error (Refusal.ofParseRefusal refusal) := by
      unfold parseStreamFor
      rw [hraw]
    exact ⟨hraw, hdrv⟩

/-- Exact driver-to-registry parser parity for Slice 0: `parseForDriver` and the
registered front end (`Lex.lexStream` → `sliceParser slice0` → `Elab.surfaceProgram`)
produce the identical `Surface0.Program` on success and the identical `ParseRefusal`
(with matching rule revision and primary span on coordinate-valid streams) on failure. -/
theorem driver_registry_parse_parity (source : Source) (hslice : source.slice = slice0) :
    (∀ lexErr, Concrete.Lex.lexStream source = .error lexErr →
      parseForDriver source = .error (ofLexRefusal lexErr)) ∧
    (∀ stream parsed, Concrete.Lex.lexStream source = .ok stream →
      ((sliceParser slice0).run stream.parserView).2 = some (.ok parsed) →
      ∃ tree prog, parsed.tree = .inl tree ∧
        Concrete.Elab.surfaceProgram tree = some prog ∧
        parseStreamWithRefusal stream = .ok prog ∧
        parseStream stream = .ok prog ∧
        parseForDriver source = .ok prog ∧
        Derives (surfaceTokens stream) prog) ∧
    (∀ stream refusal, Concrete.Lex.lexStream source = .ok stream →
      ((sliceParser slice0).run stream.parserView).2 = some (.error refusal) →
      parseStreamWithRefusal stream = .error (some refusal) ∧
      parseStream stream = .error (Refusal.ofParseRefusal refusal) ∧
      parseForDriver source = .error (Refusal.ofParseRefusal refusal) ∧
      ∃ cause, RefusalCause.ofParseRefusal? refusal = some cause ∧
        parseForDriver source = .error (.source cause) ∧
        cause.citation = refusal.registeredRule ∧
        cause.primary = refusal.span) := by
  refine ⟨?_, ?_, ?_⟩
  · intro lexErr hlex
    unfold parseForDriver
    simp only [hlex, Except.mapError, Bind.bind, Except.bind]
  · intro stream parsed hlex hrun
    have hstreamSlice : stream.source.slice = slice0 := by
      rw [lexStream_source_eq hlex, hslice]
    obtain ⟨tree, prog, htree, hdec, hraw, hdrv, hsurf⟩ :=
      (parseStreamFor_parity (sliceParser slice0) stream hstreamSlice).1 parsed hrun
    have hfor : parseForDriver source = .ok prog := by
      unfold parseForDriver
      simp only [hlex, Except.mapError, Bind.bind, Except.bind]
      exact hdrv
    exact ⟨tree, prog, htree, hdec, hraw, hdrv, hfor, hsurf⟩
  · intro stream refusal hlex hrun
    have hstreamSlice : stream.source.slice = slice0 := by
      rw [lexStream_source_eq hlex, hslice]
    obtain ⟨hraw, hdrv⟩ :=
      (parseStreamFor_parity (sliceParser slice0) stream hstreamSlice).2 refusal hrun
    have hfor : parseForDriver source = .error (Refusal.ofParseRefusal refusal) := by
      unfold parseForDriver
      simp only [hlex, Except.mapError, Bind.bind, Except.bind]
      exact hdrv
    have hwf : stream.parserView.WF := (Concrete.Lex.Bridge.lexStream_lexes hlex).2.1
    obtain ⟨cause, hcause, href, hcit, hspan⟩ :=
      parserEngine_run_error_of_wf (sliceParser slice0) stream.parserView refusal hwf hrun rfl rfl rfl
    exact ⟨hraw, hdrv, hfor, cause, hcause, by rw [hfor, href], hcit, hspan⟩

/-- Every Slice 0 program returned by `parseStream` is derived by `Surface0.Derives`. -/
theorem parseStream_derives {stream : TokenStream} {ast : Program}
    (hslice : stream.source.slice = slice0) (hok : parseStream stream = .ok ast) :
    Derives (surfaceTokens stream) ast :=
  parseStreamFor_derives (sliceParser slice0) hslice hok

/-- Every Slice 0 program returned by `parseForDriver` comes from a lexed stream
whose `Surface0.Token` projection derives `ast` under `Surface0.Derives`. -/
theorem parseForDriver_derives {source : Source} {ast : Program}
    (hslice : source.slice = slice0) (hok : parseForDriver source = .ok ast) :
    ∃ stream, Concrete.Lex.lexStream source = .ok stream ∧
      parseStream stream = .ok ast ∧
      Derives (surfaceTokens stream) ast := by
  unfold parseForDriver at hok
  cases hlex : Concrete.Lex.lexStream source with
  | error e => simp [hlex, Except.mapError, Bind.bind, Except.bind] at hok
  | ok stream =>
    simp only [hlex, Except.mapError, Bind.bind, Except.bind] at hok
    have hstreamSlice : stream.source.slice = slice0 := by
      rw [lexStream_source_eq hlex, hslice]
    exact ⟨stream, rfl, hok, parseStream_derives hstreamSlice hok⟩

/-! ## Proof-only sparse-lookup evaluation equivalence

`earlyProposal` is a 23,815-word array materialized from `earlyProposalTree`, so
kernel proofs should not unfold `acceptedCells earlyProposal earlySharedGrammar`.
The lemmas below rewrite `(sliceParser slice0).run` to use `earlyProposalTree.lookup`
via `earlyTable_lookup_eq`, enabling fast kernel reduction via `decide +kernel`
without altering the production parser (`sliceParser slice0`). -/

namespace KernelEval

/-- Logarithmic-depth sparse lookup matching `(sliceParser slice0).table.lookup`. -/
def earlyTreeLookup (n : Fin Nonterminal.count) (look : Option ConcreteSpec.StageInterface.Terminal) : Option Nat :=
  let w := earlyProposalTree.lookup (earlyRawGrammar.layout.cell n.val (terminalColumn look))
  if w == 0 then none else some (w.toNat - 1)

theorem sliceParser_table_lookup_eq (n : Fin Nonterminal.count)
    (look : Option ConcreteSpec.StageInterface.Terminal) :
    (sliceParser slice0).table.lookup n look = earlyTreeLookup n look :=
  earlyTable_lookup_eq n look

mutual
  def parseSymbolSparse (tokens : Array SurfaceToken) (targetSlice : Slice) (eof : FileSpan)
      (n : Fin Nonterminal.count) (c : ParseCursor) : Nat → ParseWork (earlySharedGrammar.value n)
    | 0 => (c, .error .exhausted)
    | fuel + 1 =>
      let c := {c with steps := c.steps + 1}
      match earlyTreeLookup n (tokens[c.offset]?.map fun t => t.value.kind) with
      | none => (c, .error .malformed)
      | some index =>
        match earlySharedGrammar.productions[index]? with
        | none => (c, .error .malformed)
        | some production =>
          if h : production.lhs = n then
            let lookSpan := (tokens[c.offset]?.map Located.span).getD eof
            let c := c.require targetSlice production.firstSlice (.production production.id) lookSpan
            match parseArgsSparse tokens targetSlice eof production.id fuel production.rhs c with
            | (c, .error stop) => (c, .error stop)
            | (c, .ok args) =>
              let (cost, value) := production.action.eval args
              ({c with steps := c.steps + cost}, .ok (h ▸ value))
          else (c, .error .malformed)
  termination_by structural fuel => fuel

  def parseArgsSparse (tokens : Array SurfaceToken) (targetSlice : Slice) (eof : FileSpan)
      (prodId : ProductionId) :
      (fuel : Nat) → (rhs : List (Sym ConcreteSpec.StageInterface.Terminal (Fin Nonterminal.count))) →
        ParseCursor → ParseWork (ActionArgs earlySharedGrammar.value rhs)
    | 0, _, c => (c, .error .exhausted)
    | _ + 1, [], c => ({c with steps := c.steps + 1}, .ok ())
    | fuel + 1, .t kind :: rest, c =>
      let c := {c with steps := c.steps + 1}
      match tokens[c.offset]? with
      | none => (c, .error .malformed)
      | some token =>
        if token.value.kind ≠ kind then (c, .error .malformed)
        else
          let c := c.require targetSlice (earlySharedGrammar.required token.value)
            (tokenUnsupportedCause prodId token.value) token.span
          let c := {c with offset := c.offset + 1}
          let (c, args) := parseArgsSparse tokens targetSlice eof prodId fuel rest c
          (c, args.map fun args => (token, args))
    | fuel + 1, .n n :: rest, c =>
      let c := {c with steps := c.steps + 1}
      match parseSymbolSparse tokens targetSlice eof n c fuel with
      | (c, .error stop) => (c, .error stop)
      | (c, .ok child) =>
        let (c, args) := parseArgsSparse tokens targetSlice eof prodId fuel rest c
        (c, args.map fun args => (child, args))
  termination_by structural fuel => fuel
end

private theorem parseSymbol_parseArgs_eq_sparse
    (table : CheckedTable earlySharedGrammar)
    (hlook : ∀ n look, table.lookup n look = earlyTreeLookup n look)
    (tokens : Array SurfaceToken) (targetSlice : Slice) (eof : FileSpan) (fuel : Nat) :
    (∀ n c, parseSymbol table tokens targetSlice eof n c fuel =
      parseSymbolSparse tokens targetSlice eof n c fuel) ∧
    (∀ prodId rhs c, parseArgs table tokens targetSlice eof prodId fuel rhs c =
      parseArgsSparse tokens targetSlice eof prodId fuel rhs c) := by
  induction fuel with
  | zero => exact ⟨fun _ _ => rfl, fun _ _ _ => rfl⟩
  | succ fuel ih =>
    obtain ⟨ihSym, ihArgs⟩ := ih
    constructor
    · intro n c
      unfold parseSymbol parseSymbolSparse
      dsimp only
      rw [hlook]
      cases earlyTreeLookup n (tokens[c.offset]?.map fun t => t.value.kind) with
      | none => rfl
      | some index =>
        dsimp only
        cases earlySharedGrammar.productions[index]? with
        | none => rfl
        | some production =>
          dsimp only
          cases decEq production.lhs n with
          | isTrue h =>
            simp only [h, ↓reduceDIte, ihArgs]
            cases parseArgsSparse tokens targetSlice eof production.id fuel production.rhs
                ({ c with steps := c.steps + 1 }.require targetSlice production.firstSlice
                  (.production production.id) ((tokens[c.offset]?.map Located.span).getD eof)) with
            | mk c' res =>
              cases res <;> rfl
          | isFalse h => simp only [h, ↓reduceDIte]
    · intro prodId rhs c
      cases rhs with
      | nil => rfl
      | cons sym rest =>
        cases sym with
        | t kind =>
          unfold parseArgs parseArgsSparse
          dsimp only
          cases tokens[c.offset]? with
          | none => rfl
          | some token =>
            dsimp only
            split
            · rfl
            · rw [ihArgs]
        | n child =>
          unfold parseArgs parseArgsSparse
          dsimp only
          rw [ihSym]
          cases parseSymbolSparse tokens targetSlice eof child {c with steps := c.steps + 1} fuel with
          | mk c' res =>
            cases res with
            | error _ => rfl
            | ok _ =>
              dsimp only
              rw [ihArgs]

def runEarlySparse (input : ParserView) :
    Nat × Option (Except ParseRefusal (Parsed (earlySharedGrammar.value earlySharedGrammar.start))) :=
  match input.invalidLocation with
  | some (span, reason) => (1, some (.error (.invalidStream span reason)))
  | none =>
    let eof := eofFileSpan input.source
    let (c, value) := parseSymbolSparse input.tokens input.source.slice eof earlySharedGrammar.start
      ⟨0, slice0, none, 0⟩ (earlyStepsPerToken * (input.tokens.size + 1))
    let span := (input.tokens[c.offset]?.map Located.span).getD eof
    match value with
    | .error .exhausted => (c.steps + 1, none)
    | .error .malformed =>
      let cause := if c.offset < input.tokens.size then MalformedCause.unexpectedToken else .unexpectedEnd
      (c.steps + 1, some (.error (.malformed earlyMalformedRule cause span)))
    | .ok ast =>
      if c.offset != input.tokens.size then
        (c.steps + 1, some (.error (.malformed earlyMalformedRule .trailingTokens span)))
      else
        match c.firstUnsupportedHit with
        | some (hitSpan, cause) =>
          let rule := match cause with
            | .production _ => earlyUnsupportedProductionRule
            | .nonDecimal _ => earlyUnsupportedLiteralRule
          (c.steps + 1, some (.error (.unsupported rule cause hitSpan c.required)))
        | none =>
          (c.steps + 1, some (.ok ⟨input.source, ast⟩))

private theorem parserEngine_run_eq_sparse
    (parser : ParserEngine earlySharedGrammar)
    (hlook : ∀ n look, parser.table.lookup n look = earlyTreeLookup n look)
    (hsteps : parser.stepsPerToken = earlyStepsPerToken)
    (hmal : parser.malformedRule = earlyMalformedRule)
    (hprod : parser.unsupportedProductionRule = earlyUnsupportedProductionRule)
    (hlit : parser.unsupportedLiteralRule = earlyUnsupportedLiteralRule)
    (input : ParserView) :
    parser.run input = runEarlySparse input := by
  unfold ParserEngine.run runEarlySparse
  cases input.invalidLocation with
  | some _ => rfl
  | none =>
    dsimp only
    rw [hsteps, hmal, hprod, hlit]
    have hps := (parseSymbol_parseArgs_eq_sparse parser.table hlook input.tokens input.source.slice
      (eofFileSpan input.source) (earlyStepsPerToken * (input.tokens.size + 1))).1
      earlySharedGrammar.start ⟨0, slice0, none, 0⟩
    rw [hps]
    cases parseSymbolSparse input.tokens input.source.slice (eofFileSpan input.source)
        earlySharedGrammar.start ⟨0, slice0, none, 0⟩
        (earlyStepsPerToken * (input.tokens.size + 1)) with
    | mk c res =>
      cases res with
      | error stop => cases stop <;> rfl
      | ok ast =>
        dsimp only
        cases c.offset != input.tokens.size with
        | true => simp only [↓reduceIte]
        | false =>
          simp only [Bool.false_eq_true, ↓reduceIte]
          cases c.firstUnsupportedHit with
          | none => rfl
          | some hit =>
            cases hit with
            | mk hitSpan cause => cases cause <;> rfl

theorem sliceParser_run_eq_sparse (input : ParserView) :
    ParserEngine.run (g := earlySharedGrammar) (sliceParser slice0) input = runEarlySparse input :=
  parserEngine_run_eq_sparse (sliceParser slice0) sliceParser_table_lookup_eq rfl rfl rfl rfl input

def parseStreamSparse (stream : TokenStream) : Except Refusal Program :=
  match (runEarlySparse stream.parserView).2 with
  | none =>
    .error (.source (.parser (.internal (.advance 0)) (eofFileSpan stream.source).surface))
  | some (.error refusal) => .error (Refusal.ofParseRefusal refusal)
  | some (.ok parsed) =>
    match parsed.tree with
    | .inl tree =>
      match Concrete.Elab.surfaceProgram tree with
      | some program => .ok program
      | none =>
        .error (.source (.parser (.internal (.advance 0)) (eofFileSpan stream.source).surface))
    | .inr empty => nomatch empty

def parseForDriverSparse (source : Source) : Except Refusal Program := do
  let stream ← (Concrete.Lex.lexStream source).mapError ofLexRefusal
  parseStreamSparse stream

theorem parseStream_eq_sparse (stream : TokenStream) :
    parseStream stream = parseStreamSparse stream := by
  unfold parseStream parseStreamFor parseStreamWithRefusalFor parseStreamSparse
  have hrun : (ParserEngine.run (g := earlySharedGrammar) (sliceParser slice0) stream.parserView).2 =
      (runEarlySparse stream.parserView).2 :=
    congrArg Prod.snd (sliceParser_run_eq_sparse stream.parserView)
  rw [hrun]
  cases (runEarlySparse stream.parserView).2 with
  | none => rfl
  | some res =>
    cases res with
    | error _ => rfl
    | ok parsed =>
      rcases parsed with ⟨src, tree⟩
      change (match match tree with
        | .inl t =>
          match Concrete.Elab.surfaceProgram t with
          | some program => Except.ok program
          | none => Except.error none
        | .inr empty => nomatch empty with
        | .ok program => Except.ok program
        | .error (some refusal) => Except.error (Refusal.ofParseRefusal refusal)
        | .error none =>
          Except.error (.source (.parser (.internal (.advance 0)) (eofFileSpan stream.source).surface))) =
        match tree with
        | .inl t =>
          match Concrete.Elab.surfaceProgram t with
          | some program => Except.ok program
          | none =>
            Except.error (.source (.parser (.internal (.advance 0)) (eofFileSpan stream.source).surface))
        | .inr empty => nomatch empty
      cases tree with
      | inr e => exact nomatch e
      | inl t =>
        dsimp only
        cases Concrete.Elab.surfaceProgram t <;> rfl

theorem parseForDriver_eq_sparse (source : Source) :
    parseForDriver source = parseForDriverSparse source := by
  unfold parseForDriver parseForDriverSparse
  cases Concrete.Lex.lexStream source with
  | error _ => rfl
  | ok stream =>
    simp only [Except.mapError, Bind.bind, Except.bind]
    exact parseStream_eq_sparse stream

theorem parseBytes_eq_sparse (bytes : ByteArray) :
    parseBytes bytes = parseForDriverSparse (sourceOfBytes bytes) :=
  parseForDriver_eq_sparse (sourceOfBytes bytes)

/-- Project an `Except Refusal` result to its decidably comparable error option. -/
def exceptError {α : Type} : Except Refusal α → Option Refusal
  | .error e => some e
  | .ok _ => none

theorem eq_error_of_exceptError {α : Type} {x : Except Refusal α} {e : Refusal}
    (h : exceptError x = some e) : x = .error e := by
  cases x with
  | error _ =>
    cases Option.some.inj h
    rfl
  | ok _ => cases h

theorem parseBytes_eq_error_of_sparse {bytes : ByteArray} {e : Refusal}
    (h : exceptError (parseForDriverSparse (sourceOfBytes bytes)) = some e) :
    parseBytes bytes = .error e := by
  rw [parseBytes_eq_sparse]
  exact eq_error_of_exceptError h

theorem ok_of_isOk {α : Type} {x : Except Refusal α} (h : x.isOk = true) :
    x = .ok (x.toOption.get (by cases x <;> cases h <;> rfl)) := by
  cases x with
  | error _ => cases h
  | ok _ => rfl

def exprEq : Expr → Expr → Bool
  | .integer n1 s1 sp1, .integer n2 s2 sp2 => n1 == n2 && s1 == s2 && sp1 == sp2
  | .local x1 sp1, .local x2 sp2 => x1 == x2 && sp1 == sp2
  | .add l1 r1 sp1, .add l2 r2 sp2 => exprEq l1 l2 && exprEq r1 r2 && sp1 == sp2
  | _, _ => false

private theorem exprEq_sound : ∀ {a b : Expr}, exprEq a b = true → a = b
  | .integer _ _ _, .integer _ _ _, h => by
    simp only [exprEq, Bool.and_eq_true, beq_iff_eq] at h
    rcases h with ⟨⟨rfl, rfl⟩, rfl⟩; rfl
  | .local _ _, .local _ _, h => by
    simp only [exprEq, Bool.and_eq_true, beq_iff_eq] at h
    rcases h with ⟨rfl, rfl⟩; rfl
  | .add _ _ _, .add _ _ _, h => by
    simp only [exprEq, Bool.and_eq_true, beq_iff_eq] at h
    rcases h with ⟨⟨hl, hr⟩, rfl⟩
    rw [exprEq_sound hl, exprEq_sound hr]
  | .integer .., .local .., h | .integer .., .add .., h
  | .local .., .integer .., h | .local .., .add .., h
  | .add .., .integer .., h | .add .., .local .., h => by cases h

def bindingEq (a b : Binding) : Bool :=
  a.name == b.name && a.span == b.span && exprEq a.rhs b.rhs

private theorem bindingEq_sound {a b : Binding} (h : bindingEq a b = true) : a = b := by
  cases a; cases b
  simp only [bindingEq, Bool.and_eq_true, beq_iff_eq] at h
  rcases h with ⟨⟨rfl, rfl⟩, hv⟩
  rw [exprEq_sound hv]

def stmtEq : Statement → Statement → Bool
  | .bind b1 sp1, .bind b2 sp2 => bindingEq b1 b2 && sp1 == sp2
  | .ret e1 sp1, .ret e2 sp2 => exprEq e1 e2 && sp1 == sp2
  | _, _ => false

private theorem stmtEq_sound : ∀ {a b : Statement}, stmtEq a b = true → a = b
  | .bind _ _, .bind _ _, h => by
    simp only [stmtEq, Bool.and_eq_true, beq_iff_eq] at h
    rcases h with ⟨hb, rfl⟩; rw [bindingEq_sound hb]
  | .ret _ _, .ret _ _, h => by
    simp only [stmtEq, Bool.and_eq_true, beq_iff_eq] at h
    rcases h with ⟨he, rfl⟩; rw [exprEq_sound he]
  | .bind .., .ret .., h | .ret .., .bind .., h => by cases h

def stmtsEq : List Statement → List Statement → Bool
  | [], [] => true
  | x :: xs, y :: ys => stmtEq x y && stmtsEq xs ys
  | _, _ => false

private theorem stmtsEq_sound : ∀ {xs ys : List Statement}, stmtsEq xs ys = true → xs = ys
  | [], [], _ => rfl
  | _ :: _, _ :: _, h => by
    simp only [stmtsEq, Bool.and_eq_true] at h
    rw [stmtEq_sound h.1, stmtsEq_sound h.2]
  | [], _ :: _, h | _ :: _, [], h => by cases h

def programEq (a b : Program) : Bool :=
  a.name == b.name && a.nameSpan == b.nameSpan && stmtsEq a.body b.body && a.closeBrace == b.closeBrace

theorem programEq_sound {a b : Program} (h : programEq a b = true) : a = b := by
  cases a; cases b
  simp only [programEq, Bool.and_eq_true, beq_iff_eq] at h
  rcases h with ⟨⟨⟨rfl, rfl⟩, hb⟩, rfl⟩
  rw [stmtsEq_sound hb]

def matchesProgram (res : Except Refusal Program) (expected : Program) : Bool :=
  match res with
  | .ok p => programEq p expected
  | .error _ => false

theorem parseStream_eq_ok_of_sparse {stream : TokenStream} {expected : Program}
    (h : matchesProgram (parseStreamSparse stream) expected = true) :
    parseStream stream = .ok expected := by
  rw [parseStream_eq_sparse]
  unfold matchesProgram at h
  cases hp : parseStreamSparse stream with
  | error _ => rw [hp] at h; cases h
  | ok p =>
    rw [hp] at h
    exact congrArg Except.ok (programEq_sound h)

theorem parseBytes_eq_ok_of_sparse {bytes : ByteArray} {expected : Program}
    (h : matchesProgram (parseForDriverSparse (sourceOfBytes bytes)) expected = true) :
    parseBytes bytes = .ok expected := by
  rw [parseBytes_eq_sparse]
  unfold matchesProgram at h
  cases hp : parseForDriverSparse (sourceOfBytes bytes) with
  | error _ => rw [hp] at h; cases h
  | ok p =>
    rw [hp] at h
    exact congrArg Except.ok (programEq_sound h)

end KernelEval

end Concrete.Parse
