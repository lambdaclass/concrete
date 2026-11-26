import Concrete.Interfaces.Pipeline

open Concrete
open Concrete.Interfaces
open ConcreteSpec Core Core.SourcePathSpan StageInterface StageGrammar
open Concrete.LL1
open LeanCompilerBackend.Verified (validateJob)

namespace StageInterfaceControls

private def production : Production (fun (_ : Unit) => Bool) :=
  ⟨(), [], ⟨0, by decide⟩, fun _ => true⟩

private def grammar : SharedGrammar Unit := ⟨fun _ => Bool, (), [production]⟩

private def table : CheckedTable grammar where
  lookup _ look := if look.isNone then some 0 else none
  predicts := by
    intro n look i
    cases n
    change _ ↔ Predicts grammar.grammar i look ∧ ∃ rhs, [((), [])][i]? = some ((), rhs)
    cases i with
    | zero =>
      simp only [List.getElem?_cons_zero, Option.some.injEq, Prod.mk.injEq, true_and,
        exists_eq', and_true]
      constructor
      · intro h
        have eof : look = none := by cases look <;> simp_all
        subst look
        exact ⟨(), [], rfl, Or.inr ⟨by simp [NullableSeq], .start⟩⟩
      · rintro ⟨n, rhs, row, first | ⟨_, follow⟩⟩
        change some ((), []) = some (n, rhs) at row
        cases row
        · obtain ⟨_, _, pre, post, _, first⟩ := first
          rcases first with terminal | ⟨_, nonterminal, _⟩ <;> simp at *
        · cases follow with
          | start => rfl
          | first row _ => simp [SharedGrammar.grammar, grammar, production] at row
          | tail row _ _ => simp [SharedGrammar.grammar, grammar, production] at row
    | succ i => cases look <;> simp
  rank _ := 0
  acyclic := by
    intro p member before child after rhs _
    have eq : p = production := List.mem_singleton.mp member
    subst p
    simp [production] at rhs

private def verifier : TableVerifier grammar where
  Proposal := Unit
  verify _ := .tick fun _ => .done (some table)
  terminals := [ .fn, .let_, .return_, .ident, .integer, .reserved, .u8, .lparen,
    .rparen, .lbrace, .rbrace, .arrow, .colon, .equal, .semi, .plus, .minus, .star,
    .slash, .percent, .stringLiteral, .byteLiteral, .label, .mut_, .if_, .else_, .struct_, .move_ ]
  terminalCoverage := by intro t; cases t <;> decide
  coefficient := 1
  bounded := by intro _; decide
  productive := by
    intro _ _ _
    refine ⟨?_, by decide⟩
    intro p member
    have eq : p = production := List.mem_singleton.mp member
    subst p
    exact ⟨[], (), 0, .nil⟩

private def testSite : SiteId := ⟨4⟩
private def span : FileSpan := ⟨⟨0⟩, 0, 1, 1, 1⟩
private def rule : RuleId := ⟨`control, 1⟩

private def testLanguage : CoreLanguage where
  operands := {
    Val := Unit
    Expr := Unit
    Ty := Unit
    Store := Hybrid.State
    Prim := Empty
    Event := Nat
    unitVal := ()
    unitTy := ()
    isCopy := fun _ => true
    eval := fun _ _ _ => .trap .divisionByZero
    life := fun _ store => some store
    test := fun _ => some true
    allows := fun p => nomatch p
  }
  hybridStore := rfl
  Declaration := Unit
  safety
    | .expression _ | .call _ (_ :: _) =>
      [⟨0, .divisionByZero, [((), .literal [0])]⟩]
    | _ => []
  permit := fun _ _ _ own => some own

private def testLowering : LoweringLanguage testLanguage.operands.Ty :=
  ⟨fun _ _ => Unit, ()⟩

private def testProfile : Profile := ⟨.lcb, .x86_64Linux, 2, 4096⟩

private def testOrigin : Origin := ⟨testSite, span, ⟨.function Exec.Demo.mainF, ⟨[]⟩⟩⟩

private def testCore (project : Project) : CoreInput testLanguage :=
  ⟨project, ⟨0, by decide⟩, ⟨[],
    ⟨[(Exec.Demo.mainF, ⟨[], (), ⟨0⟩, .ret () testSite⟩)]⟩,
    Exec.Demo.mainF, [(testSite, testOrigin)], []⟩⟩

private def testMIR (project : Project) (checks : List OperationCheck) :
    MIR testLowering testProfile :=
  let branch : CFGEdge (Sigma (Read Unit (Literal Unit))) := ⟨⟨1⟩, []⟩
  let guarded : CFGEdge (Sigma (Read Unit (Literal Unit))) := ⟨⟨2⟩, []⟩
  let read : Read Unit (Literal Unit) () := .literal ⟨[0]⟩
  let entry : Block _ _ _ := ⟨⟨0⟩, [], [], .branch read branch guarded⟩
  let raw : Block _ _ _ := ⟨⟨1⟩, [], [⟨testOrigin, .op (⟨⟨0⟩, []⟩ : Place Unit ()) ()
    (.cons read .nil) checks⟩], .ret none⟩
  let trap : Block _ _ _ := ⟨⟨2⟩, [], [], .trap .divisionByZero testOrigin⟩
  ⟨project, Exec.Demo.mainF, [], [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[entry, raw, trap]⟩)]⟩

private def badBody : LeanCompilerBackend.Verified.Source.Function := ⟨.word, #[], #[], #[]⟩

private def testBIR (project : Project) : BIR testLowering testProfile :=
  ⟨project, Exec.Demo.mainF, [], [], [], #[]⟩

private def testOutput : LCBInput testProfile := ⟨badBody, [], #[], ByteArray.empty⟩

private def testPasses (ast : Bool) (checks : List OperationCheck) :
    Passes grammar testLanguage testLowering testProfile where
  lex _ := .error (.malformed rule span)
  parser := ⟨table, verifier, (), rfl, fun _ view => .tick fun _ => .done (.ok ⟨view.source, ast⟩), 1⟩
  elaborate _ := .error ⟨rule, span, []⟩
  lowerCore input := .ok (testMIR input.core.project checks)
  lowerUnchecked input := .ok (testMIR input.project checks)
  lowerMemory _ := .error (.invalidInput rule)
  buildSSA _ := .error (.invalidInput rule)
  buildBIR _ := .error (.invalidInput rule)
  emitLLVM bir := .ok ⟨ByteArray.empty, bir.sites⟩
  emitLCB _ := .ok testOutput

private def testFrontend : FrontendSpec Bool testLanguage :=
  ⟨fun _ _ => True, fun _ => ByteArray.empty, fun _ _ _ => False, fun _ _ => False,
    fun _ _ => True, fun _ _ => False⟩

private def testInvariants : FrontendInvariants Bool testLanguage :=
  ⟨fun _ => True, fun _ => False, fun _ => True, fun _ => True⟩

private def unsafeDischarge : Discharge testLanguage :=
  ⟨testSite, testOrigin.evidenceKey, ⟨testSite, .safety, 0⟩, .divisionByZero,
    [((), .literal [0])]⟩

private def unsafeDischarges : Discharges testLanguage := [(testSite, [unsafeDischarge])]

private def testAdmission : Admission testLanguage :=
  ⟨fun _ _ => True, fun _ => True, fun _ => True, fun _ _ => False, fun _ => .ok unsafeDischarges⟩

private def testFailures : LoweringFailures testLanguage testLowering testProfile :=
  ⟨fun _ _ => True, fun _ _ => True, fun _ _ => True, fun _ _ => True,
    fun _ _ => True, fun _ _ => True, fun _ _ => True⟩

private abbrev r (m : LoweringModels testLanguage testLowering testProfile)
    (ast : Bool := false) (checks : List OperationCheck := []) :=
  registry (testPasses ast checks) testFrontend testInvariants testAdmission m testFailures

private def tokenStream (project : Project) : TokenStream :=
  ⟨⟨project, ⟨0⟩, ⟨0, by decide⟩⟩, #[], #[#[]]⟩

-- The parser sees all decoded payloads without consulting the raw source.
example (source : Source) :
    let view := TokenStream.parserView ⟨source, #[⟨span, .stringLiteral [65, 0]⟩,
      ⟨span, .byteLiteral 255⟩, ⟨span, .label ⟨"next"⟩⟩], #[#[], #[], #[], #[]]⟩
    view.tokens[0]?.map Located.value = some (.stringLiteral [65, 0]) ∧
      view.tokens[1]?.map Located.value = some (.byteLiteral 255) ∧
      view.tokens[2]?.map Located.value = some (.label ⟨"next"⟩) :=
  ⟨rfl, rfl, rfl⟩

-- Even a caller-supplied empty lexer invariant cannot hide the wrong AST from parser soundness.
example (project : Project) (m : LoweringModels testLanguage testLowering testProfile) :
    ¬ (r m).parser.Sound := by
  intro sound
  have result := (sound.1 (tokenStream project) ⟨(tokenStream project).source, false⟩ trivial rfl).2
  obtain ⟨_, _, derives, _, _⟩ := result.1
  generalize eqValue : false = value at derives
  cases derives with
  | apply p _ _ member _ _ =>
    have eq : p = production := List.mem_singleton.mp member
    subst p
    contradiction

-- The correctly timed action for the same input has a checked-table and declarative derivation.
example (project : Project) (m : LoweringModels testLanguage testLowering testProfile) :
    (r m true).parser.success (tokenStream project) ⟨(tokenStream project).source, true⟩ :=
  by
    refine ⟨⟨rfl, 1, ?_, ?_, Nat.le_refl _⟩, Nat.le_refl _⟩
    · exact Derives.apply (g := grammar) production [] ()
        (List.mem_cons_self ..) (Nat.le_refl _) .nil
    · exact Predictive.apply (table := table) production 0 [] [] ()
        rfl rfl (Nat.le_refl _) (.nil [])

-- The branch goes directly to a raw operation. An operation in a different block cannot cover it.
example (project : Project) (m : LoweringModels testLanguage testLowering testProfile) :
    ¬ (r m).lowerUnchecked.success (testCore project) (testMIR project []) := by
  intro success
  have consumes := success.2.2.2.1
  have covers := consumes.2.2.1
  let op : MIR.Occurrence Unit := ⟨Exec.Demo.mainF, ⟨1⟩, 0, testOrigin, [((), .literal [0])], []⟩
  obtain ⟨conditions, ⟨sourceOp, member, fromSource⟩, _, fromMIR, _⟩ :=
    covers op (List.mem_cons_self ..)
  have eq : sourceOp = .expression () := by
    simpa [op, testOrigin, testCore, Payload.operations, stmtOperations] using member
  subst sourceOp
  rw [fromSource] at fromMIR
  contradiction

-- The registered checker claims a discharge, but the matching reachable Core operation traps.
example (project : Project) (m : LoweringModels testLanguage testLowering testProfile) :
    ¬ (r m).admission.success (testCore project) ⟨testCore project, unsafeDischarges⟩ := by
  intro success
  have valid := success.2.2.2.2
  have member : unsafeDischarges.Contains testSite unsafeDischarge :=
    ⟨[unsafeDischarge], List.mem_cons_self .., List.mem_cons_self ..⟩
  have safety := (valid.2.2.2 testSite unsafeDischarge member).2.2.2.2.2
  let core := testCore project
  let start := core.payload.initial
  let trapped : Exec.Config core.payload.machine := {start with ctrl := .trapped .divisionByZero testSite}
  exact (safety start [] (.expression ()) (.refl start) rfl).2 .silent trapped (.silent rfl) rfl

-- A fabricated effect cannot accompany a silent Exec transition in the registered Core model.
example (m : LoweringModels testLanguage testLowering testProfile) (project : Project)
    (before after : (r m).coreModel.model.execution.State)
    (h : (r m).coreModel.model.execution.step (testCore project) before after)
    (silent : Exec.step (testCore project).payload.body ((r m).coreModel.config (testCore project) before) =
      some (.next ((r m).coreModel.config (testCore project) after))) :
    ¬ (r m).coreModel.specTrace (testCore project) after =
      (r m).coreModel.specTrace (testCore project) before ++ [show testLanguage.operands.Event from (7 : Nat)] := by
  intro fabricated
  obtain ⟨label, step, exactTrace⟩ := (r m).coreModel.stepsExact _ _ _ h
  cases step with
  | silent _ => simp [Exec.Label.events, exactTrace] at fabricated
  | prim request _ _ => rw [silent] at request; contradiction

-- Memory and ISA are computed from the registry's resolved platform.
example : testProfile.isa = .x86_64 ∧
    ¬ testProfile.memoryTarget = { linuxX8664 with byteOrder := .big } := by
  constructor
  · rfl
  · intro h
    have order := congrArg Target.byteOrder h
    contradiction

-- The wrong backend's registered emitter cannot report success for the LCB profile.
example (project : Project) (m : LoweringModels testLanguage testLowering testProfile) :
    ¬ (r m).emitLLVM.success (testBIR project) ⟨ByteArray.empty, #[]⟩ := by
  intro success
  have backend := success.1
  contradiction

-- Three BIR definitions exceed the registered profile's two-slot ceiling.
example (project : Project) (m : LoweringModels testLanguage testLowering testProfile) :
    let block : Block _ _ _ := ⟨⟨0⟩, [⟨(), ⟨0⟩⟩, ⟨(), ⟨1⟩⟩, ⟨(), ⟨2⟩⟩], [], .ret none⟩
    let bir : BIR testLowering testProfile :=
      { testBIR project with functions := [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[block]⟩)] }
    ¬ (r m).emitLCB.inputInvariant bir := by
  dsimp
  intro eligible
  exact (show ¬ 3 ≤ 2 from by decide) eligible.2.2

-- The emitter actually returns this body, but its exact job fails validation.
example (project : Project) (m : LoweringModels testLanguage testLowering testProfile)
    (valid : (r m).emitLCB.inputInvariant (testBIR project)) : ¬ (r m).emitLCB.Sound := by
  intro sound
  have validated := (sound.1 (testBIR project) testOutput valid rfl).2.2.1
  have rejected : validateJob (Backend.LCB.job badBody) ≠ .ok () := by
    intro h
    have result := congrArg Except.isOk h
    with_unfolding_all contradiction
  exact rejected validated

end StageInterfaceControls
