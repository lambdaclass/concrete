import ConcreteSpec.Proofs.Slice1
import ConcreteSpec.Spec.Basics
import ConcreteSpec.Spec.Ident
import ConcreteSpec.Spec.Outcome
import ConcreteSpec.Spec.Rep
import ConcreteSpec.Spec.Slice1Checker
import ConcreteSpec.Spec.Slice1Ownership
import ConcreteSpec.Spec.Slice1Semantics
import ConcreteSpec.Spec.Slice1Syntax
import ConcreteSpec.Spec.Surface0
import ConcreteSpec.Surface1
import ConcreteSpec.Proofs.Surface0
import ConcreteSpec.Proofs.Checker
import ConcreteSpec.Proofs.Observe
import ConcreteSpec.Proofs.Rep
import ConcreteSpec.Spec.Exec

namespace ConcreteSpec.Surface1
open Surface0 (Lexes)
open Core (SiteId SourcePath SourceText)

/-- The executable Core checker and the declarative admission judgment agree once the
independent surface derivation has fixed the elaboration. -/
theorem admitted_iff_check (bytes : ByteArray) (out : Elaborated) :
    Admitted bytes out ↔
      (∃ tokens ast, Lexes bytes tokens ∧ Derives tokens ast ∧ elaborate ast = some out) ∧
      check out.core = true :=
  and_congr_right fun _ => (check_iff out.core).symm

theorem sourcePermitted_iff {src : SourceProgram} {out : Elaborated}
    (h : elaborate src = some out) : SourcePermitted src ↔ Checked out.core := by
  simpa [SourcePermitted, h] using check_iff out.core

/-- R-0588 at the source-to-Core boundary. The source's checked elaboration terminates
within its straight-line fuel budget, preserves the written abort site and admits no
implicit trap. Each conclusion holds with every larger budget. -/
theorem admitted_behaviour {bytes : ByteArray} {out : Elaborated} (h : Admitted bytes out) :
    ∀ fuel ≥ out.core.body.length + 1,
      (∃ code, code < 256 ∧ run out.core fuel = .exit code) ∨
      ∃ site span, out.core.ending = .abort site ∧
        out.sites.getLast? = some (site, span) ∧ run out.core fuel = .aborted site := by
  intro fuel enough
  obtain ⟨o, hexec, hresult⟩ := checked_finishes h.2
  have runAtFuel : run out.core fuel = o :=
    runBody_mono (exec_complete hexec) (exec_ne_outOfFuel hexec) fuel enough
  rcases hresult with ⟨code, rfl⟩ | ⟨site, hs, rfl⟩
  · exact .inl ⟨code, runBody_exit_u8 h.2.2 .nil fuel code runAtFuel, runAtFuel⟩
  · obtain ⟨_, _, _, _, helab⟩ := h.1
    obtain ⟨span, _, hspan⟩ := elaborate_abort helab hs
    exact .inr ⟨site, span, hs, hspan, runAtFuel⟩

/-- At the admitted boundary, the specified abort is reached with any sufficient fuel.
The converse recovers the Core terminator from the outcome. -/
theorem admitted_abort_exact {bytes : ByteArray} {out : Elaborated} (h : Admitted bytes out)
    {fuel : Nat} (enough : out.core.body.length + 1 ≤ fuel) (site : SiteId) :
    run out.core fuel = .aborted site ↔ out.core.ending = .abort site := by
  refine ⟨runBody_abort_site, ?_⟩
  intro hend
  obtain ⟨_, _, _, _, hrun⟩ := runBody_reaches_finish h.2.2 Corr.nil fuel enough
  change run out.core fuel = _ at hrun
  simpa [hend, finish] using hrun

/-- The hosted observation preserves normal status or the exact abort site on descriptor 2. -/
theorem admitted_process_result {bytes : ByteArray} {out : Elaborated} (h : Admitted bytes out)
    (path : SourcePath) {fuel : Nat} (enough : out.core.body.length + 1 ≤ fuel) :
    (∃ code, code < 256 ∧ run out.core fuel = .exit code ∧
      processResult path out (run out.core fuel) = some ⟨code, ByteArray.empty, ByteArray.empty⟩) ∨
    ∃ site span, out.sites.getLast? = some (site, span) ∧ run out.core fuel = .aborted site ∧
      processResult path out (run out.core fuel) =
        some ⟨134, ByteArray.empty, (abortLine path span site).toUTF8⟩ := by
  rcases admitted_behaviour h fuel enough with ⟨code, hrange, hrun⟩ | ⟨site, span, _, hs, hrun⟩
  · exact .inl ⟨code, hrange, hrun, by simp [hrun, processResult]⟩
  · exact .inr ⟨site, span, hs, hrun, by simp [hrun, hs, processResult]⟩

/-- Each empty struct has zero payload bytes. Its declared type remains the key for decoding
those bytes, recovering the nominal value. -/
theorem empty_struct_representation {decls : Decls} {name : Name} {decl : StructDecl}
    (hd : findDecl decls name = some decl) (hf : decl.fields = []) :
    Rep.encodeVal (.struct name []) = [] ∧
      Rep.Ty.size decls (.struct name) = some 0 ∧
      Rep.decodeVal decls (.struct name) [] = some (.struct name []) :=
  ⟨rfl, Rep.rep_roundtrip (Rep.RepVal.empty hd hf)⟩

end ConcreteSpec.Surface1

namespace ConcreteSpec.Surface1.Witness
open Surface0 (Span Token TokenValue Terminal TokenStream Tokenizes Position Lexes)
open Core (SiteId SourcePath SourceText)

private def s : Name := ⟨"S"⟩
private def sTy : Ty := .struct s
private def decls : Decls := [⟨s, false, []⟩]
private def slot : Slot := .val (.struct s [])
private def abortSite : SiteId := ⟨0⟩

private def context (used : Bool) : Ctx :=
  [(⟨0⟩, { ty := sTy, live := true, used, known := none })]

theorem live_used_abort :
    BodyOk decls (context true) [] (.abort abortSite) ∧
      bodyOkB decls (context true) [] (.abort abortSite) = true :=
  ⟨bodyOkB_iff.mp rfl, rfl⟩

theorem live_used_return_refused :
    (¬ Discharged decls (context true)) ∧
      (¬ BodyOk decls (context true) [] (.ret (.int .u8 134))) ∧
      bodyOkB decls (context true) [] (.ret (.int .u8 134)) = false :=
  ⟨by decide, fun h => by have := bodyOkB_iff.mpr h; contradiction, rfl⟩

theorem live_unused_abort_refused :
    (¬ BodyOk decls (context false) [] (.abort abortSite)) ∧
      bodyOkB decls (context false) [] (.abort abortSite) = false :=
  ⟨fun h => by have := bodyOkB_iff.mpr h; contradiction, rfl⟩

open Core.Exec in
private abbrev abortOps : Operands where
  Val := Slot
  Expr := Unit
  Ty := Ty
  Store := Unit
  Prim := Empty
  Event := Unit
  unitVal := .val (.scalar (.int .u8 0))
  unitTy := u8
  isCopy := Ty.isCopy decls
  eval := fun _ _ _ => .stuck
  life := fun _ _ => none
  test := fun _ => none
  allows := fun p => nomatch p

open Core.Exec in
private def abortStart : Config abortOps :=
  ⟨.exec (.abort abortSite), [(⟨0⟩, slot)], (),
    [.scope ⟨0⟩ [] [⟨Demo.logF, [], ⟨1⟩⟩]]⟩

open Core.Exec in
private def abortFinal : Config abortOps := { abortStart with ctrl := .trapped .abort abortSite }

open Core.Exec in
theorem live_slot_abort_terminal :
    Rules ⟨[]⟩ abortStart none .silent abortFinal ∧
      abortFinal.final? = some (.trapped .abort abortSite) ∧
      step ⟨[]⟩ abortFinal = none :=
  ⟨.abort, rfl, rfl⟩

/-- A lexeme with its preceding trivia, kept as bytes so each source witness checks spelling
and source positions through the shared relational lexer. -/
private structure Piece where
  trivia : List Nat
  lexeme : List Nat
  value : TokenValue

private def chars (s : String) : List Nat := s.toList.map Char.toNat

private def word (trivia spelling : String) : Piece :=
  ⟨chars trivia, chars spelling, Surface0.wordValue spelling⟩

private def punct (trivia spelling : String) (kind : Terminal) : Piece :=
  ⟨chars trivia, chars spelling, .fixed kind⟩

private def decimal (trivia spelling : String) (value : Nat) : Piece :=
  ⟨chars trivia, chars spelling, .integer value .decimal none⟩

private def scalars (pieces : List Piece) : List Nat :=
  pieces.flatMap fun p => p.trivia ++ p.lexeme

private def tokens (pos : Position) : List Piece → List Token
  | [] => []
  | p :: ps =>
    let start := p.trivia.foldl Position.advance pos
    let stop := p.lexeme.foldl Position.advance start
    ⟨p.value, start.span stop⟩ :: tokens stop ps

private def Valid : List Piece → Prop
  | [] => True
  | p :: ps =>
    Surface0.Trivia (p.trivia ++ (p.lexeme ++ scalars ps)) (p.lexeme ++ scalars ps) ∧
      Tokenizes (p.lexeme ++ scalars ps) p.lexeme (scalars ps) p.value ∧ Valid ps

private theorem tokenStream {pieces : List Piece} (h : Valid pieces) (pos : Position) :
    TokenStream pos (scalars pieces) (tokens pos pieces) := by
  induction pieces generalizing pos with
  | nil => exact .eof (.stop [] (by simp) (by simp))
  | cons p ps ih =>
    rw [show scalars (p :: ps) = p.trivia ++ (p.lexeme ++ scalars ps) from
      List.append_assoc _ _ _]
    exact TokenStream.cons h.1 h.2.1 (ih h.2.2 _)

private theorem ascii_decodes (cs : List Nat) (hc : ∀ c ∈ cs, c < 128) :
    Surface0.Utf8.Decodes (cs.map UInt8.ofNat) cs := by
  induction cs with
  | nil => exact .nil
  | cons c cs ih =>
    have hb := hc c (by simp)
    have hn : (UInt8.ofNat c).toNat = c := by simp; omega
    have henc : Surface0.Utf8.Encodes c [UInt8.ofNat c] := by
      change Surface0.Utf8.One (UInt8.ofNat c) ∧ c = (UInt8.ofNat c).toNat
      exact ⟨(Surface0.Utf8.one_iff _).mpr (by omega), hn.symm⟩
    exact .cons henc (ih fun d hd => hc d (by simp [hd]))

private def bytes (pieces : List Piece) : ByteArray :=
  ⟨((scalars pieces).map UInt8.ofNat).toArray⟩

private theorem lexes {pieces : List Piece} (h : Valid pieces)
    (hc : (scalars pieces).all (fun c => decide (0 < c ∧ c < 128)) = true) :
    Lexes (bytes pieces) (tokens {} pieces) := by
  have hbounded : ∀ c ∈ scalars pieces, 0 < c ∧ c < 128 := by
    intro c hm
    simpa using List.all_eq_true.mp hc c hm
  refine ⟨scalars pieces, ?_, fun c hm => ?_, tokenStream h {}⟩
  · simpa [bytes] using ascii_decodes _ (fun c hm => (hbounded c hm).2)
  · have := hbounded c hm
    omega

private def declaration : List Piece :=
  [word "" "struct", word " " "S", punct " " r#"{"# .lbrace, punct "" r#"}"# .rbrace]

private def header (trivia : String) : List Piece :=
  [word trivia "fn", word " " "main", punct "" "(" .lparen, punct "" ")" .rparen,
   punct " " "->" .arrow, word " " "u8", punct " " r#"{"# .lbrace]

private def binding : List Piece :=
  [word "\n  " "let", word " " "t", punct "" ":" .colon, word " " "S", punct " " "=" .equal,
   word " " "S", punct " " r#"{"# .lbrace, punct "" r#"}"# .rbrace, punct "" ";" .semi]

private def takeBinding : List Piece :=
  [word "\n  " "let", word " " "u", punct "" ":" .colon, word " " "S", punct " " "=" .equal,
   word " " "move", word " " "t", punct "" ";" .semi]

private def destructure (name : String) : List Piece :=
  [word "\n  " "let", word " " "S", punct " " r#"{"# .lbrace, punct "" r#"}"# .rbrace,
   punct " " "=" .equal, word " " "move", word " " name, punct "" ";" .semi]

private def abortCall : List Piece :=
  [word "\n  " "abort", punct "" "(" .lparen, punct "" ")" .rparen, punct "" ";" .semi]

private def returned (expression : List Piece) : List Piece :=
  [word "\n  " "return"] ++ expression ++ [punct "" ";" .semi]

private def close : List Piece := [punct "\n" r#"}"# .rbrace]

private inductive Fixture where
  | component | consumedAbort | unusedAbort | ordinary134

private def pieces : Fixture → List Piece
  | .component => declaration ++ header "\n" ++ binding ++ takeBinding ++ destructure "u" ++
      returned [decimal " " "20" 20, punct " " "+" .plus, decimal " " "22" 22] ++ close
  | .consumedAbort => declaration ++ header "\n" ++ binding ++ destructure "t" ++ abortCall ++ close
  | .unusedAbort => declaration ++ header "\n" ++ binding ++ abortCall ++ close
  | .ordinary134 => header "" ++ returned [decimal " " "134" 134] ++ close

private theorem triviaFrom (pre rest : List Nat)
    (hpre : pre.all (fun c => decide (Surface0.Whitespace c)) = true)
    (hpost : rest.head?.all (fun c => decide (¬ Surface0.Whitespace c)) = true)
    (hcomment : rest.take 2 ≠ [47, 47]) :
    Surface0.Trivia (pre ++ rest) rest := by
  induction pre with
  | nil =>
    apply Surface0.Trivia.stop
    · intro c cs h
      simp only [List.nil_append] at h
      subst rest
      simpa using hpost
    · intro cs h
      simp only [List.nil_append] at h
      subst rest
      exact hcomment rfl
  | cons c cs ih =>
    have hc := List.all_eq_true.mp hpre
    exact .whitespace (of_decide_eq_true (hc c (by simp)))
      (ih (List.all_eq_true.mpr fun d hd => hc d (by simp [hd])))

private theorem digitsFrom (digits rest : List Nat)
    (h : digits.all (fun c => decide (Surface0.BaseDigit .decimal c)) = true)
    (hstop : rest.head?.all (fun c => decide (¬ Surface0.BaseDigit .decimal c)) = true) :
    Surface0.DigitRun .decimal (digits ++ rest) digits rest := by
  induction digits with
  | nil =>
    apply Surface0.DigitRun.stop
    intro c cs h
    simp only [List.nil_append] at h
    subst rest
    simpa using hstop
  | cons c cs ih =>
    have hc := List.all_eq_true.mp h
    exact .cons (of_decide_eq_true (hc c (by simp)))
      (ih (List.all_eq_true.mpr fun d hd => hc d (by simp [hd])))

private theorem decimalFrom (digits rest : List Nat)
    (h : digits.all (fun c => decide (Surface0.BaseDigit .decimal c)) = true)
    (hstop : rest.head?.all (fun c => decide (¬ Surface0.BaseDigit .decimal c)) = true)
    (hsuffix : rest.head?.all (fun c => decide (¬ Surface0.IdentContinue c)) = true)
    (hok : Surface0.DigitsAccepted .decimal digits) :
    Tokenizes (digits ++ rest) digits rest
      (.integer (Surface0.positionalValue .decimal digits) .decimal none) :=
  by
    have token : Tokenizes (digits ++ rest) (digits ++ []) rest
        (.integer (Surface0.positionalValue .decimal digits) .decimal none) :=
      Tokenizes.integer (radix := .decimal) (suffix := []) (ty := none)
        (digitsFrom digits rest h hstop) hok
        (.stop _ (fun c cs h => by subst rest; simpa using hsuffix)) (.inl ⟨rfl, rfl⟩)
    simpa only [List.append_nil] using token

private theorem punctuationFrom (spelling rest : List Nat) (kind : Terminal)
    (hmem : spelling ∈ Surface0.punctuationScalars) (hne : spelling ≠ [])
    (hkind : (Surface0.scalarText spelling, kind) ∈ Surface0.punctuation)
    (hmax : Surface0.punctuationScalars.all
      (fun other => decide (other <+: spelling ++ rest → other.length ≤ spelling.length)) = true) :
    Tokenizes (spelling ++ rest) spelling rest (.fixed kind) :=
  .punct (.some hmem hne rfl fun other ho tail he =>
    of_decide_eq_true (List.all_eq_true.mp hmax other ho) ⟨tail, he.symm⟩) hkind

private theorem validPieces (f : Fixture) : Valid (pieces f) := by
  cases f <;> repeat' apply And.intro
  all_goals first
    | exact True.intro
    | (apply triviaFrom <;> decide)
    | (apply Tokenizes.word
       apply Surface0.Identifier.intro
       · decide
       · repeat' apply Surface0.IdentBody.cons <;> try decide
         apply Surface0.IdentBody.stop
         intro c cs h
         injection h with hc _
         subst c
         decide)
    | (apply decimalFrom <;> decide)
    | (apply punctuationFrom <;> decide)

private def location (f : Fixture) (i : Nat) : Span :=
  ((tokens {} (pieces f))[i]?.map Token.span).getD ⟨0, 0, 1, 1⟩

private def bindingAst : Statement := .bind "t" (.typed sTy) (.construct "S")
private def consumeAst (name : String) : Statement := .bind "S" .destructure (.move name)

private def ast : Fixture → SourceProgram
  | .component => ⟨["S"], "main",
      [bindingAst, .bind "u" (.typed sTy) (.move "t"), consumeAst "u",
       .ret (.add (.integer 20 none) (.integer 22 none) (location .component 38))]⟩
  | .consumedAbort => ⟨["S"], "main",
      [bindingAst, consumeAst "t", .call "abort" (location .consumedAbort 28)]⟩
  | .unusedAbort => ⟨["S"], "main", [bindingAst, .call "abort" (location .unusedAbort 20)]⟩
  | .ordinary134 => ⟨[], "main", [.ret (.integer 134 none)]⟩

private theorem parsed (f : Fixture) : Derives (tokens {} (pieces f)) (ast f) := by
  let sp := location f
  have structDecl := DeclsDerive.cons "S" (sp 0) (sp 1) (sp 2) (sp 3) .nil
  have bind rest body (h : BodyDerives rest body) :=
    BodyDerives.bind (name := "t") (sp 11) (sp 12) (sp 15) (sp 19)
      (.typed (sp 13) (.struct "S" (sp 14)))
      (.expr (.construct "S" (sp 16) (sp 17) (sp 18)) (.nil _)) h
  have consume (start : Nat) (name : String) rest body (h : BodyDerives rest body) :=
    BodyDerives.bind (name := "S") (sp start) (sp (start + 1)) (sp (start + 4)) (sp (start + 7))
      (.destructure (sp (start + 2)) (sp (start + 3)))
      (.expr (.move name (sp (start + 5)) (sp (start + 6))) (.nil _)) h
  have abort (i : Nat) :=
    BodyDerives.call (name := "abort") (sp i) (sp (i + 1)) (sp (i + 2)) (sp (i + 3)) .nil
  cases f with
  | component =>
    have ret := BodyDerives.ret (sp 36) (sp 40)
      (.expr (.integer 20 none (sp 37)) (.more (site := sp 38) (.integer 22 none (sp 39)) (.nil _)))
      .nil
    have take := BodyDerives.bind (name := "u") (sp 20) (sp 21) (sp 24) (sp 27)
      (.typed (sp 22) (.struct "S" (sp 23)))
      (.expr (.move "t" (sp 25) (sp 26)) (.nil _)) (consume 28 "u" _ _ ret)
    exact Derives.program "main" (sp 4) (sp 5) (sp 6) (sp 7) (sp 8) (sp 9) (sp 10) (sp 41)
      structDecl (bind _ _ take)
  | consumedAbort =>
    exact Derives.program "main" (sp 4) (sp 5) (sp 6) (sp 7) (sp 8) (sp 9) (sp 10) (sp 32)
      structDecl (bind _ _ (consume 20 "t" _ _ (abort 28)))
  | unusedAbort =>
    exact Derives.program "main" (sp 4) (sp 5) (sp 6) (sp 7) (sp 8) (sp 9) (sp 10) (sp 24)
      structDecl (bind _ _ (abort 20))
  | ordinary134 =>
    exact Derives.program "main" (sp 0) (sp 1) (sp 2) (sp 3) (sp 4) (sp 5) (sp 6) (sp 10)
      .nil (.ret (sp 7) (sp 9) (.expr (.integer 134 none (sp 8)) (.nil _)) .nil)

private def result (f : Fixture) : Elaborated :=
  (elaborate (ast f)).getD ⟨⟨[], [], .ret (.int .u8 0)⟩, []⟩

private theorem derived (f : Fixture) :
    Lexes (bytes (pieces f)) (tokens {} (pieces f)) ∧
      Derives (tokens {} (pieces f)) (ast f) ∧ elaborate (ast f) = some (result f) := by
  refine ⟨lexes (validPieces f) ?_, parsed f, ?_⟩
  · cases f <;> decide
  · cases f <;> rfl

private theorem admittedFixture (f : Fixture) (h : check (result f).core = true) :
    Admitted (bytes (pieces f)) (result f) :=
  ⟨⟨_, _, derived f⟩, (check_iff _).mp h⟩

theorem component_returns_42 :
    Admitted (bytes (pieces .component)) (result .component) ∧
      run (result .component).core 5 = .exit 42 :=
  ⟨admittedFixture .component (by decide), by decide⟩

private def mainPath : SourcePath :=
  ⟨SourceText.ofBytes "src/main.con".toUTF8.data.toList, by decide⟩

theorem consumed_abort_source_bytes :
    (bytes (pieces .consumedAbort)).data =
      r#"struct S {}
fn main() -> u8 {
  let t: S = S {};
  let S {} = move t;
  abort();
}"#.toUTF8.data :=
  rfl

theorem consumed_abort_observed :
    Admitted (bytes (pieces .consumedAbort)) (result .consumedAbort) ∧
      run (result .consumedAbort).core 3 = .aborted abortSite ∧
      processResult mainPath (result .consumedAbort)
        (run (result .consumedAbort).core 3) =
      some ⟨134, ByteArray.empty, "concrete: trap: abort at src/main.con:5:3 (site 0)\n".toUTF8⟩ :=
  ⟨admittedFixture .consumedAbort (by decide), by decide, rfl⟩

theorem unused_abort_source_refused :
    Lexes (bytes (pieces .unusedAbort)) (tokens {} (pieces .unusedAbort)) ∧
      Derives (tokens {} (pieces .unusedAbort)) (ast .unusedAbort) ∧
      elaborate (ast .unusedAbort) = some (result .unusedAbort) ∧
      ¬ Admitted (bytes (pieces .unusedAbort)) (result .unusedAbort) := by
  refine ⟨(derived _).1, (derived _).2.1, (derived _).2.2, ?_⟩
  intro h
  have hc := (check_iff _).mpr h.2
  contradiction

theorem ordinary_134_observed :
    Admitted (bytes (pieces .ordinary134)) (result .ordinary134) ∧
      processResult mainPath (result .ordinary134) (run (result .ordinary134).core 1) =
        some ⟨134, ByteArray.empty, ByteArray.empty⟩ :=
  ⟨admittedFixture .ordinary134 (by decide), rfl⟩

theorem nonempty_payload_outside_slice (a b c d e f g : Span) (names : List String) :
    ¬ DeclsDerive [⟨.fixed .struct_, a⟩, ⟨.name "S", b⟩, ⟨.fixed .lbrace, c⟩,
      ⟨.name "x", d⟩, ⟨.fixed .colon, e⟩, ⟨.fixed .u8, f⟩, ⟨.fixed .rbrace, g⟩] names := by
  intro h
  cases h

private def span : Span := ⟨0, 5, 1, 1⟩
private def takeS : Statement := consumeAst "t"
private def ret : Statement := .ret (.integer 0 none)
private def callAbort : Statement := .call "abort" span

theorem consumeThenAbort : SourcePermitted ⟨["S"], "main", [bindingAst, takeS, callAbort]⟩ := by decide

theorem freshDestructuring : SourcePermitted
    ⟨["S"], "main", [.bind "S" .destructure (.construct "S"), ret]⟩ := by decide

theorem wildcardCopy : SourcePermitted
    ⟨[], "main", [.bind "_" (.typed u8) (.integer 1 none), ret]⟩ := by decide

theorem overflow : ¬ SourcePermitted
    ⟨[], "main", [.ret (.add (.integer 200 none) (.integer 56 none) span)]⟩ := by decide

theorem liveLinearAtReturn : ¬ SourcePermitted ⟨["S"], "main", [bindingAst, ret]⟩ := by decide

theorem codeAfterAbort : ¬ SourcePermitted ⟨[], "main", [callAbort, ret]⟩ := by decide

theorem useAfterMove : ¬ SourcePermitted ⟨["S"], "main", [bindingAst, takeS, takeS, ret]⟩ := by decide

theorem otherCall : ¬ SourcePermitted ⟨[], "main", [.call "other" span]⟩ := by decide

theorem reservedAbort : ¬ SourcePermitted
    ⟨[], "main", [.bind "abort" (.typed u8) (.integer 1 none), callAbort]⟩ := by decide

theorem nominalMismatch : ¬ SourcePermitted
    ⟨["S", "T"], "main", [.bind "T" .destructure (.construct "S"), ret]⟩ := by decide

theorem wildcardLinear : ¬ SourcePermitted
    ⟨["S"], "main", [.bind "_" (.typed sTy) (.construct "S"), ret]⟩ := by decide

theorem bareLinearRead : ¬ SourcePermitted
    ⟨["S"], "main", [bindingAst, .bind "S" .destructure (.local "t"), ret]⟩ := by decide

theorem moveCopy : ¬ SourcePermitted
    ⟨[], "main", [.bind "t" (.typed u8) (.integer 1 none), .ret (.move "t")]⟩ := by decide

theorem nonSliceType : ¬ SourcePermitted
    ⟨[], "main", [.bind "_" (.typed (.scalar (.int .u16))) (.integer 1 none), ret]⟩ := by decide

end ConcreteSpec.Surface1.Witness
