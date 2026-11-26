import Concrete.Interfaces.Pipeline

open Concrete
open Concrete.Interfaces
open ConcreteSpec Core Core.SourcePathSpan StageInterface StageGrammar
open Concrete.LL1
open LeanCompilerBackend.Verified (validateJob)

namespace StageInterfaceControls

private def early : Slice := ⟨0, by decide⟩
private def later : Slice := ⟨2, by decide⟩

private def production : Production (fun (_ : Fin 1) => Bool) :=
  ⟨0, [.t .integer], early, .constant true⟩

private def terminals : Array Terminal :=
  #[.fn, .let_, .return_, .ident, .integer, .reserved, .u8, .lparen, .rparen,
    .lbrace, .rbrace, .arrow, .colon, .equal, .semi, .plus, .minus, .star,
    .slash, .percent, .stringLiteral, .byteLiteral, .label, .mut_, .if_,
    .else_, .struct_, .move_]

private def grammar : SharedGrammar 1 :=
  { terminalSlices := Array.replicate 28 early
    terminalSliceCoverage := by intro kind; cases kind <;> decide
    terminalSliceExact := by
      have indices : ∀ i : Fin 28, terminals[i.val].ctorIdx = i.val := by decide
      intro i
      exact ⟨terminals[i.val], indices i⟩
    integerSlices := Array.replicate 9 early ++ Array.replicate 27 later
    integerSliceCoverage := by
      intro radix suffix
      cases radix <;> cases suffix <;> first | (rename_i ty; cases ty <;> decide) | decide
    nonDecimalLater := by
      intro radix suffix nonDecimal
      cases radix <;> cases suffix <;> first
        | contradiction
        | (rename_i ty; cases ty <;> decide)
        | decide
    start := 0
    value := fun _ => Bool
    productions := #[production] }

private def cells : Array (Option Nat) :=
  (Array.replicate grammar.terminalColumns none).set! (terminalColumn (some .integer)) (some 0)

private def table : CheckedTable grammar where
  cells := cells
  dimensions := rfl
  predicts := by
    intro n look i
    have hn : n = 0 := Subsingleton.elim _ _
    subst n
    have entry : tableLookup grammar.terminalColumns cells (0 : Fin 1) look =
        if look = some Surface0.Terminal.integer then some 0 else none := by
      cases look with
      | none => rfl
      | some t => cases t <;> rfl
    rw [entry]
    change _ ↔ Predicts grammar.grammar i look ∧
      ∃ rhs, [((0 : Fin 1), [Sym.t Surface0.Terminal.integer])][i]? = some (0, rhs)
    cases i with
    | zero =>
      simp only [List.getElem?_cons_zero, Option.some.injEq, Prod.mk.injEq, true_and, exists_eq', and_true]
      constructor
      · intro hit
        have h : look = some Surface0.Terminal.integer := by
          split at hit
          · assumption
          · contradiction
        exact ⟨0, [.t .integer], rfl, .inl ⟨.integer, h, [], [],
          ⟨[], rfl, by simp⟩, .inl rfl⟩⟩
      · rintro ⟨n, rhs, row, first | ⟨nullable, _⟩⟩
        · change some ((0 : Fin 1), [Sym.t Surface0.Terminal.integer]) = some (n, rhs) at row
          have hr := (Prod.mk.inj (Option.some.inj row)).2
          subst rhs
          obtain ⟨a, rfl, pre, post, ⟨xs, rfl, _⟩, same | ⟨_, same, _⟩⟩ := first
          · cases xs <;> simp_all
          · cases xs <;> simp_all
        · change some ((0 : Fin 1), [Sym.t Surface0.Terminal.integer]) = some (n, rhs) at row
          have hr := (Prod.mk.inj (Option.some.inj row)).2
          subst rhs
          obtain ⟨xs, eq, _⟩ := nullable
          cases xs <;> simp at eq
    | succ i => split <;> simp
  rank _ := 0
  acyclic := by
    intro p member before child after rhs _
    have eq : p = production := List.mem_singleton.mp member
    subst p
    cases before <;> simp [production] at rhs

private def proposal : Array UInt64 :=
  (Array.replicate grammar.terminalColumns 0).set! (terminalColumn (some .integer)) 1

private def testSite : SiteId := ⟨4⟩
private def span : FileSpan := ⟨⟨0⟩, 0, 1, 1, 1⟩
private def rule : RuleId := ⟨`control, 1⟩

private def testLanguage : CoreLanguage where
  operands := {
    Val := CoreValue Empty
    Expr := Unit
    Ty := Unit
    Store := Hybrid.State
    Prim := Empty
    Event := Nat
    unitVal := .scalar .unit
    unitTy := ()
    isCopy := fun _ => true
    eval := fun _ _ _ => .trap .divisionByZero
    life := fun _ store => some store
    test := fun _ => some true
    allows := fun p => nomatch p
  }
  hybridStore := rfl
  Declaration := Unit
  expressionOperator _ := .arithmetic (.plain .div)
  primitiveOperator p := nomatch p
  argumentRead _ := none
  argumentReadNoTrap := by simp
  OtherValue := Empty
  valueShape := rfl
  safety
    | .expression _ | .call _ (_ :: _) =>
      [⟨0, .divisionByZero, [((), .literal [0])]⟩]
    | _ => []
  permit := fun _ _ _ own => some own

private def testLowering : LoweringLanguage testLanguage.operands.Ty :=
  ⟨fun _ _ _ => Unit, ()⟩

private def testProfile : Profile := ⟨.lcb, .x86_64Linux, 2, 4096⟩

private def testOrigin : Origin := ⟨testSite, span, ⟨.function Exec.Demo.mainF, ⟨[]⟩⟩⟩

private def testCore (project : Project) (body : Exec.Stmt (testLanguage.instrumented []) := .ret () testSite) : CoreInput testLanguage :=
  ⟨project, ⟨0, by decide⟩, ⟨[],
    ⟨[(Exec.Demo.mainF, ⟨[], (), ⟨0⟩, body⟩)]⟩,
    Exec.Demo.mainF, [(testSite, testOrigin)], []⟩⟩

private def testMIR (project : Project) (checks : List OperationCheck)
    (op : Operator := .arithmetic (.plain .div)) (withCall : Bool := false)
    {profile : Profile} : MIR testLowering profile :=
  let branch : CFGEdge (Sigma (Read Unit (Literal Unit))) := ⟨⟨1⟩, []⟩
  let guarded : CFGEdge (Sigma (Read Unit (Literal Unit))) := ⟨⟨2⟩, []⟩
  let read : Read Unit (Literal Unit) () := .literal ⟨[0]⟩
  let entry : Block _ _ _ := ⟨⟨0⟩, [], [], .branch read branch guarded, []⟩
  let operation : MIRInst testLowering := .op (opcode := op) (⟨⟨0⟩, []⟩ : Place Unit ()) ()
    (.cons read .nil) checks
  let call : MIRInst testLowering := .call Exec.Demo.mainF none [⟨(), read⟩] []
  let instructions := if withCall then [operation, call] else [operation]
  let raw : Block _ _ _ := ⟨⟨1⟩, [], instructions.map (LocatedOp.mk testOrigin), .ret none, []⟩
  let trap : Block _ _ _ := ⟨⟨2⟩, [], [], .trap .divisionByZero testOrigin, []⟩
  ⟨project, Exec.Demo.mainF, [], [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[entry, raw, trap]⟩)]⟩

private def badBody : LeanCompilerBackend.Verified.Source.Function := ⟨.word, #[], #[], #[]⟩

private def testBIR (project : Project) {profile : Profile} : BIR testLowering profile :=
  ⟨project, Exec.Demo.mainF, [], [], [], #[]⟩

private def testOutput {profile : Profile} : LCBInput profile := ⟨badBody, [], #[], ByteArray.empty⟩

private def testPasses {profile : Profile} (parser : Parser grammar) (checks : List OperationCheck) :
    Passes grammar testLanguage testLowering profile where
  lex _ := .error (.malformed rule span)
  parser := parser
  elaborate _ := .error ⟨rule, span, []⟩
  lowerCore input := .ok (testMIR input.core.project checks)
  lowerUnchecked input := .ok (testMIR input.project checks)
  lowerMemory _ := .error (.invalidInput rule)
  buildSSA _ := .error (.invalidInput rule)
  buildBIR _ := .error (.invalidInput rule)
  emitLLVM bir := .ok ⟨testProfile.llvmTriple, testProfile.llvmDataLayout, [], bir.sites⟩
  emitLCB _ := .ok testOutput

private def testFrontend : FrontendSpec Bool testLanguage :=
  ⟨fun _ _ => True, fun _ => ByteArray.empty, fun _ _ _ => False, fun _ _ => False,
    fun _ _ => True, fun _ _ => False⟩

private def testInvariants : FrontendInvariants Bool testLanguage :=
  ⟨fun _ => True, fun _ => True, fun _ => True⟩

private def unsafeDischarge : Discharge testLanguage :=
  ⟨testSite, testOrigin.evidenceKey, ⟨testSite, .safety, 0⟩, .divisionByZero,
    [((), .literal [0])]⟩

private def unsafeDischarges : Discharges testLanguage := [(testSite, [unsafeDischarge])]

private def testAdmission : Admission testLanguage :=
  ⟨fun _ _ => True, fun _ => True, fun _ => True, fun _ _ => False, fun _ => .ok unsafeDischarges⟩

private def testFailures {profile : Profile} : LoweringFailures testLanguage testLowering profile :=
  ⟨fun _ _ => True, fun _ _ => True, fun _ _ => True, fun _ _ => True,
    fun _ _ => True, fun _ _ => True, fun _ _ => True⟩

private abbrev r {profile : Profile} (m : LoweringModels testLanguage testLowering profile)
    (parser : Parser grammar) (checks : List OperationCheck := []) :=
  registry (testPasses parser checks) testFrontend testInvariants testAdmission m testFailures

private def tokenStream (radix : Surface0.Radix)
    (slice : Slice := early) : TokenStream :=
  ⟨⟨tabProject, ⟨0⟩, slice⟩, #[⟨span, .integer 42 radix none⟩], #[#[], #[]]⟩

private def testParser (verifier : TableVerifier grammar) (size : proposal.size = verifier.words)
    (accept : (verifier.run proposal).2 = some true) (stepsPerToken : Nat := 4) : Parser grammar :=
  ⟨table, verifier, proposal, size, accept, by decide, stepsPerToken, rule, rule⟩

section
variable (verifier : TableVerifier grammar) (size : proposal.size = verifier.words)
  (accept : (verifier.run proposal).2 = some true)

local notation "parser" => testParser verifier size accept

-- The parser sees all decoded payloads without consulting the raw source.
example (source : Source) :
    let view := TokenStream.parserView ⟨source, #[⟨span, .stringLiteral [65, 0]⟩,
      ⟨span, .byteLiteral 255⟩, ⟨span, .label ⟨"next"⟩⟩], #[#[], #[], #[], #[]]⟩
    view.tokens[0]?.map Located.value = some (.stringLiteral [65, 0]) ∧
      view.tokens[1]?.map Located.value = some (.byteLiteral 255) ∧
      view.tokens[2]?.map Located.value = some (.label ⟨"next"⟩) :=
  ⟨rfl, rfl, rfl⟩

-- A fabricated AST from the registered parser's success channel fails the exact run check.
example (m : LoweringModels testLanguage testLowering testProfile) :
    ¬ (r m parser).parser.success (tokenStream .decimal)
      ⟨(tokenStream .decimal).source, false⟩ := by
  intro h
  have run := congrArg (fun x => x.map (fun y => y.toOption.map Parsed.tree)) h.1
  exact (show some (some true) ≠ some (some false) from by decide) run

-- The same registry returns 42's AST for decimal, refuses hexadecimal at Slice 0,
-- and accepts the hexadecimal token when that production's spelling is available.
example (m : LoweringModels testLanguage testLowering testProfile) :
    (r m parser).parser.run (tokenStream .decimal) =
      .ok ⟨(tokenStream .decimal).source, true⟩ ∧
    (r m parser).parser.run (tokenStream .hexadecimal) =
      .error (.unsupported rule span later) ∧
    (r m parser).parser.run (tokenStream .hexadecimal later) =
      .ok ⟨(tokenStream .hexadecimal later).source, true⟩ :=
  ⟨rfl, rfl, rfl⟩

example (m : LoweringModels testLanguage testLowering testProfile) :
    (r m parser).parser.success (tokenStream .decimal)
      ⟨(tokenStream .decimal).source, true⟩ := by
  refine ⟨rfl, ⟨rfl, 3, ?_, ?_, by change 3 ≤ 5; decide, rfl⟩, by change 5 ≤ 8; decide⟩
  · exact Derives.apply (g := grammar) production _ _
      (List.mem_cons_self ..) (Nat.le_refl _) (.token rfl (by change 0 ≤ 0; decide) .nil)
  · exact Predictive.apply (table := table) production 0 _ [] _
      rfl rfl (Nat.le_refl _) (.token rfl (by change 0 ≤ 0; decide) (.nil []))

example (m : LoweringModels testLanguage testLowering testProfile) :
    (r m parser).parser.refusal (tokenStream .hexadecimal) (.unsupported rule span later) := by
  refine ⟨rfl, ⟨by change 0 < 2; decide, ⟨true, 3, Derives.apply (g := grammar) production _ _
    (List.mem_cons_self ..) (by decide) (.token rfl (by change 2 ≤ 2; decide) .nil)⟩, ?_⟩, by change 5 ≤ 8; decide⟩
  intro earlier tooEarly
  rintro ⟨tree, steps, derives⟩
  have required := Derives.tokensEligible derives _ (List.mem_cons_self ..)
  exact Nat.not_lt_of_ge required tooEarly

-- A correct AST does not make a run with more than the permitted transitions sound.
-- The machine spends five transitions; a two-step-per-token profile permits four.
example (m : LoweringModels testLanguage testLowering testProfile) :
    ¬ (r m (testParser verifier size accept 2)).parser.Sound := by
  intro sound
  have h := sound.1 (tokenStream .decimal)
    ⟨(tokenStream .decimal).source, true⟩ rfl rfl
  exact (show ¬ 5 ≤ 4 from by decide) h.2.2.2

private def corruptTokenStream (location : FileSpan) : TokenStream :=
  {tokenStream .decimal with tokens := #[⟨location, .integer 42 .decimal none⟩]}

-- Valid and corrupt streams have the same grammar projection. The registered
-- parser rejects both a fabricated file and an out-of-bounds byte range.
example (m : LoweringModels testLanguage testLowering testProfile) :
    let edge := (r m parser).parser
    let wrongFile := {span with file := ⟨1⟩}
    let wrongEnd := {span with endOffset := 4}
    let wrongLine := {span with line := 2}
    edge.inputInvariant (tokenStream .decimal) ∧
    (∀ location ∈ [wrongFile, wrongEnd, wrongLine],
      ¬ edge.inputInvariant (corruptTokenStream location) ∧
      edge.run (corruptTokenStream location) = .error (.invalidStream location) ∧
      edge.refusal (corruptTokenStream location) (.invalidStream location)) := by
  refine ⟨rfl, ?_⟩
  intro location member
  simp only [List.mem_cons, List.not_mem_nil, or_false] at member
  rcases member with rfl | rfl | rfl <;>
    exact ⟨fun h => by contradiction, rfl, rfl, rfl, by change 1 ≤ 8; decide⟩

-- The checked lexer/parser link has no model-selected token invariant.
example (m : LoweringModels testLanguage testLowering testProfile) :
    (r m parser).lexer.Compatible (r m parser).parser := fun _ h => h

-- Extra proposal columns cannot be counted as terminals to inflate the budget.
example (g : SharedGrammar 1) : g.terminalColumns ≤ 29 := by
  apply Nat.le_of_not_gt
  intro oversized
  have room : 28 < g.terminalSlices.size := by
    change 29 < g.terminalSlices.size + 1 at oversized
    omega
  obtain ⟨kind, index⟩ := g.terminalSliceExact ⟨28, room⟩
  change kind.ctorIdx = 28 at index
  cases kind <;> contradiction

-- An accepting callback could hide work. Here even a loop with no changing
-- memory reaches the limit without producing a verifier verdict.
example : verifyRun #[.ifEqual 0 0 0 0] ⟨0, #[0]⟩ 8 = (8, none) := rfl

-- The branch goes directly to a raw operation. An operation in a different block cannot cover it.
example (project : Project) (m : LoweringModels testLanguage testLowering testProfile) :
    ¬ (r m parser).lowerUnchecked.success (testCore project) (testMIR project []) := by
  intro success
  have consumes := success.2.2.2.1
  have covers := consumes.2.2.1
  let op : MIR.Occurrence Unit := ⟨Exec.Demo.mainF, ⟨1⟩, 0, testOrigin, .operator (.arithmetic (.plain .div)), [((), .literal [0])], []⟩
  obtain ⟨conditions, ⟨sourceOp, member, _, fromSource⟩, _, fromMIR, _⟩ :=
    covers op (List.mem_cons_self ..)
  have eq : sourceOp = .expression () := by
    simpa [op, testOrigin, testCore, Payload.operations, stmtOperations] using member
  subst sourceOp
  rw [fromSource] at fromMIR
  contradiction

private def residual : OperationCheck := ⟨0, .divisionByZero, .residual⟩

-- The exact reads and check for division cannot justify a subtraction.
example (project : Project) (m : LoweringModels testLanguage testLowering testProfile) :
    ¬ (r m parser).lowerUnchecked.success (testCore project)
      (testMIR project [residual] (.arithmetic (.plain .sub))) := by
  intro success
  let op : MIR.Occurrence Unit := ⟨Exec.Demo.mainF, ⟨1⟩, 0, testOrigin,
    .operator (.arithmetic (.plain .sub)), [((), .literal [0])], [residual]⟩
  obtain ⟨_, ⟨sourceOp, member, opcode, _⟩, _⟩ :=
    success.2.2.2.1.2.2.1 op (List.mem_cons_self ..)
  have eq : sourceOp = .expression () := by
    simpa [op, testOrigin, testCore, Payload.operations, stmtOperations] using member
  subst sourceOp
  exact (show testLanguage.opcode (.expression ()) ≠ op.opcode from by decide) opcode

-- A separate instruction using the call's origin and reads cannot guard the call.
example (project : Project) (m : LoweringModels testLanguage testLowering testProfile) :
    ¬ (r m parser).lowerUnchecked.success
      (testCore project (.call none Exec.Demo.mainF [()] testSite))
      (testMIR project [residual] (.arithmetic (.plain .div)) true) := by
  intro success
  let call : MIR.Occurrence Unit := ⟨Exec.Demo.mainF, ⟨1⟩, 1, testOrigin,
    .call Exec.Demo.mainF, [((), .literal [0])], []⟩
  have mem : call ∈ (testMIR project [residual] (.arithmetic (.plain .div)) true
    (profile := testProfile)).operations := List.mem_cons_of_mem _ (List.mem_cons_self ..)
  obtain ⟨_, ⟨sourceOp, member, _, fromSource⟩, _, fromMIR, _⟩ :=
    success.2.2.2.1.2.2.1 call mem
  have eq : sourceOp = .call Exec.Demo.mainF [()] := by
    simpa [call, testOrigin, testCore, Payload.operations, stmtOperations] using member
  subst sourceOp
  rw [fromSource] at fromMIR
  contradiction

-- The program itself reaches an explicit trap after its table has been removed.
example :
    let original := testCore tabProject (.abort testSite)
    let core := {original with payload := {original.payload with origins := []}}
    (∃ state, Exec.Steps core.payload.body core.payload.initial [] state ∧
      state.final? = some (.trapped .abort testSite)) ∧ ¬ core.LocationsWF := by
  dsimp only
  let original := testCore tabProject (.abort testSite)
  let core := {original with payload := {original.payload with origins := []}}
  let trapped : Exec.Config core.payload.machine := {core.payload.initial with ctrl := .trapped .abort testSite}
  have steps : Exec.Steps core.payload.body core.payload.initial [] trapped :=
    .step (l := .silent) (.silent rfl) (.refl trapped)
  refine ⟨⟨trapped, steps, rfl⟩, ?_⟩
  intro wf
  obtain ⟨_, lookup⟩ := wf.2.2.2.2.1 trapped [] .abort testSite steps rfl
  contradiction

-- A return retains a source origin even when the block has no instructions.
example (project : Project) :
    let value : Read Unit (Literal Unit) () := .literal ⟨[42]⟩
    let block : Block _ _ _ := ⟨⟨0⟩, [], [], .ret (some ⟨(), value⟩), [testOrigin]⟩
    let mir : MIR testLowering testProfile :=
      ⟨project, Exec.Demo.mainF, [], [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[block]⟩)]⟩
    mir.operations = [] ∧ PreservesOrigins [testOrigin] mir.origins := by
  exact ⟨rfl, fun _ h => h, fun _ h => h⟩

-- SSA promotes the virtual local to its literal value and retains its binding origin.
example (project : Project) :
    let place : MemoryPlace Unit () := ⟨⟨⟨0⟩, []⟩, .virtual⟩
    let memory : Memory testLowering testProfile :=
      ⟨project, Exec.Demo.mainF, [],
        [(Exec.Demo.mainF, [(⟨0⟩, ())], ⟨⟨0⟩, #[⟨⟨0⟩, [],
          [⟨testOrigin, .bind place (.literal ⟨[42]⟩)⟩],
          .ret (some ⟨(), .copy place⟩), []⟩]⟩)],
        [], [(Exec.Demo.mainF, [(⟨0⟩, .virtual)])]⟩
    let ssa : SSA testLowering testProfile :=
      ⟨project, Exec.Demo.mainF, [],
        [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[⟨⟨0⟩, [], [],
          .ret (some ⟨(), .literal ⟨[42]⟩⟩), [testOrigin]⟩]⟩)], []⟩
    PreservesOrigins memory.origins ssa.origins :=
  ⟨fun _ h => h, fun _ h => h⟩

-- The registered checker claims a discharge, but the matching reachable Core operation traps.
example (project : Project) (m : LoweringModels testLanguage testLowering testProfile) :
    ¬ (r m parser).admission.success (testCore project) ⟨testCore project, unsafeDischarges⟩ := by
  intro success
  have valid := success.2.2.2.2
  have member : unsafeDischarges.Contains testSite unsafeDischarge :=
    ⟨[unsafeDischarge], List.mem_cons_self .., List.mem_cons_self ..⟩
  have safety := (valid.2.2.2 testSite unsafeDischarge member).2.2.2.2.2
  let core := testCore project
  let start := core.payload.initial
  let trapped : Exec.Config core.payload.machine := {start with ctrl := .trapped .divisionByZero testSite}
  exact (safety start [] (.expression ()) (.refl start) rfl).2 .silent trapped (.silent rfl) rfl

private def argTemp : Var := ⟨8⟩
private def argPlace : Place Unit () := ⟨⟨8⟩, []⟩
private def callee : FnId := {Exec.Demo.mainF with name := ⟨"callee"⟩}
private def callSite : SiteId := ⟨5⟩
private def callOrigin : Origin := ⟨callSite, span, ⟨.function callee, ⟨[]⟩⟩⟩
private def divReads : List (Unit × OperandRead) := [((), .literal [8]), ((), .literal [2])]
private def divOpcode : Operator := .arithmetic (.plain .div)

private def argLanguage : CoreLanguage where
  operands := {
    Val := CoreValue Empty
    Expr := Bool
    Ty := Unit
    Store := Hybrid.State
    Prim := Empty
    Event := Empty
    unitVal := .scalar .unit
    unitTy := ()
    isCopy := fun _ => true
    eval := fun locals state atomic =>
      if atomic then
        match AList.lookup locals argTemp with
        | some value => .ok (value, state)
        | none => .stuck
      else .ok (.scalar (.int .u8 4), state)
    life := fun _ state => some state
    test := fun _ => some true
    allows := fun p => nomatch p
  }
  hybridStore := rfl
  Declaration := Unit
  expressionOperator atomic := if atomic then .read else divOpcode
  primitiveOperator p := nomatch p
  argumentRead atomic := if atomic then some ((), .copy argPlace.root []) else none
  argumentReadNoTrap := by
    intro expr _ locals state kind
    cases expr
    · intro h; contradiction
    · cases h : AList.lookup locals argTemp <;> simp [h]
  OtherValue := Empty
  valueShape := rfl
  safety
    | .expression false => [⟨0, .divisionByZero, divReads⟩]
    | _ => []
  permit := fun _ _ _ own => some own

private def argLowering : LoweringLanguage argLanguage.operands.Ty :=
  ⟨fun _ _ _ => Unit, ()⟩

private def argCore (project : Project) (body : Exec.Stmt (argLanguage.instrumented [])) : CoreInput argLanguage :=
  ⟨project, early, ⟨[], ⟨[(Exec.Demo.mainF, ⟨[], (), ⟨0⟩, body⟩)]⟩,
    Exec.Demo.mainF, [(testSite, testOrigin), (callSite, callOrigin)], []⟩⟩

private def checkedArgCore (project : Project) : CoreInput argLanguage :=
  argCore project (.seq (.bind argTemp false testSite) (.call none callee [true] callSite))

private def argCheck : OperationCheck := ⟨0, .divisionByZero, .discharged⟩

private def argDischarge : Discharge argLanguage :=
  ⟨testSite, testOrigin.evidenceKey, ⟨testSite, .safety, 0⟩, .divisionByZero, divReads⟩

private def argDischarges : Discharges argLanguage := [(testSite, [argDischarge])]

private def checkedArgMIR (project : Project) : MIR argLowering testProfile :=
  let div : MIRInst argLowering := .op (opcode := divOpcode) (types := [(), ()]) argPlace ()
    (.cons (.literal ⟨[8]⟩) (.cons (.literal ⟨[2]⟩) .nil)) [argCheck]
  let call : MIRInst argLowering := .call callee none [⟨(), .copy argPlace⟩] []
  ⟨project, Exec.Demo.mainF, [], [(Exec.Demo.mainF, [(argPlace.root, ())],
    ⟨⟨0⟩, #[⟨⟨0⟩, [], [⟨testOrigin, div⟩, ⟨callOrigin, call⟩], .ret none, []⟩]⟩)]⟩

private def argDivOccurrence : MIR.Occurrence argLanguage.operands.Ty :=
  ⟨Exec.Demo.mainF, ⟨0⟩, 0, testOrigin, .operator divOpcode, divReads, [argCheck]⟩

private def argCallOccurrence : MIR.Occurrence argLanguage.operands.Ty :=
  ⟨Exec.Demo.mainF, ⟨0⟩, 1, callOrigin, .call callee, [((), .copy argPlace.root [])], []⟩

private theorem argDivInventory (project : Project) :
    (checkedArgCore project).payload.ConditionsAt testSite (.operator divOpcode)
      [⟨0, .divisionByZero, divReads⟩] :=
  ⟨.expression false, List.mem_cons_self .., rfl, rfl⟩

private theorem argCallInventory (project : Project) :
    (checkedArgCore project).payload.ConditionsAt callSite (.call callee) [] :=
  ⟨.call callee [true], List.mem_cons_of_mem _ (List.mem_cons_self ..), rfl, rfl⟩

-- f(8 / x) is represented by a checked binding followed by a call reading its result.
-- The division owns its evidence and emitted instruction. The call owns only its own reads.
example (project : Project) :
    (checkedArgMIR project).operations = [argDivOccurrence, argCallOccurrence] ∧
    ConsumesEvidence (checkedArgCore project) argDischarges (checkedArgMIR project) := by
  refine ⟨rfl, ?_, ?_, ?_, ?_, ?_⟩
  · change ([(Exec.Demo.mainF, (⟨0⟩ : BlockId), 0), (Exec.Demo.mainF, ⟨0⟩, 1)] : List (FnId × BlockId × Nat)).Nodup
    decide
  · intro site opcode conditions source nonempty
    obtain ⟨op, member, opcodeEq, rfl⟩ := source
    change (site, op) ∈ [(testSite, .expression false), (callSite, .call callee [true])] at member
    simp only [List.mem_cons, List.not_mem_nil, or_false] at member
    rcases member with h | h <;> cases h
    · exact ⟨argDivOccurrence, List.mem_cons_self .., rfl, opcodeEq⟩
    · exact False.elim (nonempty rfl)
  · intro op member
    change op ∈ [argDivOccurrence, argCallOccurrence] at member
    simp only [List.mem_cons, List.not_mem_nil, or_false] at member
    rcases member with rfl | rfl
    · refine ⟨_, argDivInventory project, by change ([0] : List Nat).Nodup; decide, rfl, ?_, ?_⟩
      · intro _ h; contradiction
      · intro check mem _
        change check ∈ [argCheck] at mem
        cases List.mem_singleton.mp mem
        exact ⟨argDischarge, ⟨_, List.mem_cons_self .., List.mem_cons_self ..⟩,
          rfl, rfl, rfl, rfl, rfl, rfl⟩
    · refine ⟨[], argCallInventory project, List.nodup_nil, rfl, ?_, ?_⟩
      · intro f h
        have eq : callee = f := Opcode.call.inj h
        subst f
        exact ⟨[true], List.mem_cons_of_mem _ (List.mem_cons_self ..), rfl⟩
      · intro check mem _
        exact False.elim (List.not_mem_nil mem)
  · intro site d member
    obtain ⟨group, entry, belongs⟩ := member
    cases List.mem_singleton.mp entry
    cases List.mem_singleton.mp belongs
    refine ⟨argDivOccurrence, List.mem_cons_self .., argCheck, List.mem_cons_self ..,
      ⟨rfl, rfl, rfl, rfl, rfl, rfl⟩, ?_⟩
    intro other mem check present _
    change other ∈ [argDivOccurrence, argCallOccurrence] at mem
    simp only [List.mem_cons, List.not_mem_nil, or_false] at mem
    rcases mem with rfl | rfl
    · exact ⟨rfl, List.mem_singleton.mp present⟩
    · exact False.elim (List.not_mem_nil present)
  · intro site op member
    change (site, op) ∈ [(testSite, .expression false), (callSite, .call callee [true])] at member
    simp only [List.mem_cons, List.not_mem_nil, or_false] at member
    rcases member with h | h <;> cases h
    · trivial
    · intro expr occurs
      cases List.mem_singleton.mp occurs
      rfl

-- The unnormalized checked argument has no valid handoff, even when the caller
-- supplies the same division evidence and the same emitted instructions.
example (project : Project) :
    ¬ ConsumesEvidence (argCore project (.call none callee [false] callSite))
      argDischarges (checkedArgMIR project) := by
  intro consumes
  have args := consumes.2.2.2.2 callSite (.call callee [false]) (List.mem_cons_self ..)
    false (List.mem_cons_self ..)
  contradiction


private def storeRoot : PlaceId := ⟨0⟩
private def storePlace : MemoryPlace Unit () := ⟨⟨storeRoot, []⟩, .materialized⟩
private def storeValue : testLanguage.operands.Val := .scalar (.int .u8 42)

private def storeActions : MemoryActions testLanguage testLowering testProfile where
  values := {
    literal := fun _ _ => some storeValue
    field := fun _ _ => none
    replaceField := fun _ _ _ => none
    fieldRange := fun _ _ _ => none
    encode := fun _ _ _ => none
    decode := fun target _ bytes => (Hybrid.decode target (.int .u8) bytes).map CoreValue.scalar
  }
  operation := fun _ _ _ => False
  destructure := fun _ _ _ _ => False
  call := fun _ _ _ _ _ _ => False
  transfer := fun _ _ _ _ => False
  returnToCaller := fun _ _ _ _ => False

private def storingMemory (project : Project) : Memory testLowering testProfile :=
  let block : MemoryBlock testLowering := ⟨⟨0⟩, [],
    [⟨testOrigin, .beginLifetime storeRoot⟩, ⟨testOrigin, .bind storePlace (.literal ⟨[42]⟩)⟩],
    .ret none, []⟩
  ⟨project, Exec.Demo.mainF, [], [(Exec.Demo.mainF, [(storeRoot, ())], ⟨⟨0⟩, #[block]⟩)],
    [(Exec.Demo.mainF, [(storeRoot, ⟨1, 1⟩)])], [(Exec.Demo.mainF, [(storeRoot, .materialized)])]⟩

private def storeInitial : MemoryRegisters testLanguage testProfile :=
  ⟨Exec.Demo.mainF, ⟨0⟩, 0, [], [], [], [], Mem.Memory.empty⟩

private def storeBlock : Mem.Block :=
  ⟨⟨0⟩, 1, 1, 1, true, false, [.uninit]⟩

private def storeRange : Hybrid.Range :=
  ⟨⟨⟨0⟩, 1, some ⟨0⟩⟩, 1, 1⟩

private def storeBefore : MemoryRegisters testLanguage testProfile :=
  {storeInitial with
    instruction := 1
    live := [storeRoot]
    materialized := [(storeRoot, storeRange)]
    bytes := ⟨[(⟨0⟩, storeBlock)], 1⟩}

private def storeAfter : MemoryRegisters testLanguage testProfile :=
  {storeBefore with
    instruction := 2
    initialized := [storeRoot]
    bytes := ⟨[(⟨0⟩, {storeBlock with bytes := [.init 42 none]})], 1⟩}

example (project : Project) : (storingMemory project).Step storeActions storeInitial storeBefore := by
  refine ⟨_, rfl, {storeBefore with instruction := 0}, ⟨by decide, Or.inr ?_⟩, rfl⟩
  exact ⟨rfl, ⟨⟨1, 1⟩, ⟨0⟩, 1, ⟨0⟩, storeBefore.bytes, rfl, rfl, rfl⟩⟩

example (project : Project) : (storingMemory project).Step storeActions storeBefore storeAfter := by
  refine ⟨_, rfl, storeValue, storeBefore, {storeAfter with instruction := 1}, rfl, ?_, rfl⟩
  refine ⟨rfl, rfl, rfl, fun _ => ⟨storeRange, [(0, [.init 42 none])], rfl, rfl, ?_, ?_⟩⟩
  · exact fun _ => ⟨0, [.init 42 none], List.mem_cons_self .., by decide⟩
  · intro offset bytes member
    have eq := List.mem_singleton.mp member
    cases eq
    refine ⟨by decide, ?_, rfl⟩
    intro byte hb
    cases List.mem_singleton.mp hb
    decide

-- Even with a truthful PC and lifetime, an erased byte write cannot satisfy the
-- registered Memory success contract. Its projection leaves the allocated byte uninitialized.
example (project : Project) (m : LoweringModels testLanguage testLowering testProfile)
    (hActions : m.memoryActions = storeActions) (q next : m.memory.execution.State)
    (reachable : m.memory.execution.finite (storingMemory project) q)
    (step : m.memory.execution.step (storingMemory project) q next)
    (before : m.memoryState q = storeBefore)
    (erased : m.memoryState next = {storeAfter with bytes := storeBefore.bytes}) :
    ¬ LowersMemory m (testMIR project []) (storingMemory project) := by
  intro lowers
  have exactStep := lowers.2.2.2.2.2.2.1.2.1 q next reachable step
  rw [hActions, before, erased] at exactStep
  obtain ⟨block, fetched, instruction⟩ := exactStep
  have eq : (storingMemory project).blockAt storeBefore = some block := fetched
  change some _ = some block at eq
  cases eq
  obtain ⟨value, afterRead, stored, read, write, finish⟩ := instruction
  have eqRead : some (storeValue, storeBefore) = some (value, afterRead) := read
  cases eqRead
  have eqWrite : some {storeAfter with instruction := 1} = some stored := write.2.1
  cases eqWrite
  have bytes := congrArg (fun state => (MemoryState.bytes state).blocks) finish
  exact (show storeBefore.bytes.blocks ≠ storeAfter.bytes.blocks from by decide) bytes


-- A fabricated effect cannot accompany a silent Exec transition in the registered Core model.
example (m : LoweringModels testLanguage testLowering testProfile) (project : Project)
    (before after : (r m parser).coreModel.model.execution.State)
    (h : (r m parser).coreModel.model.execution.step (testCore project) before after)
    (silent : Exec.step (testCore project).payload.body ((r m parser).coreModel.config (testCore project) before) =
      some (.next ((r m parser).coreModel.config (testCore project) after))) :
    ¬ (r m parser).coreModel.specTrace (testCore project) after =
      (r m parser).coreModel.specTrace (testCore project) before ++ [show testLanguage.operands.Event from (7 : Nat)] := by
  intro fabricated
  obtain ⟨label, step, exactTrace⟩ := (r m parser).coreModel.stepsExact _ _ _ h
  cases step with
  | silent _ => simp [Exec.Label.events, exactTrace] at fabricated
  | prim request _ _ => rw [silent] at request; contradiction

-- Even a self-consistent downstream model cannot remap these terminal statuses.
example (m : LoweringModels testLanguage testLowering testProfile) (project : Project)
    (state : (r m parser).coreModel.model.execution.State)
    (finished : ((r m parser).coreModel.config (testCore project) state).final? =
      some (.returned (.scalar (.int .u8 42)))) :
    ¬ (r m parser).coreModel.model.result (testCore project) state
      (.returned 0 ByteArray.empty) := by
  intro result
  obtain ⟨outcome, final, projected⟩ := ((r m parser).coreModel.terminalExact ..).mp result
  rw [finished] at final
  cases final
  have statuses := congrArg (Option.map (fun o => match o with
    | .returned status _ => status
    | _ => 255)) projected
  exact (show some (42 : UInt8) ≠ some 0 from by decide) statuses

example (m : LoweringModels testLanguage testLowering testProfile) (project : Project)
    (state : (r m parser).coreModel.model.execution.State)
    (finished : ((r m parser).coreModel.config (testCore project) state).final? =
      some (.trapped .divisionByZero testSite)) (stderr : ByteArray) :
    ¬ (r m parser).coreModel.model.result (testCore project) state
      (.trapped 0 .divisionByZero span stderr) := by
  intro result
  obtain ⟨outcome, final, projected⟩ := ((r m parser).coreModel.terminalExact ..).mp result
  rw [finished] at final
  cases final
  have statuses := congrArg (Option.map (fun o => match o with
    | .trapped status _ _ _ => status
    | _ => 255)) projected
  exact (show some (134 : UInt8) ≠ some 0 from by decide) statuses

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
    ¬ (r m parser).emitLLVM.success (testBIR project) ⟨testProfile.llvmTriple, testProfile.llvmDataLayout, [], #[]⟩ := by
  intro success
  have backend := success.1
  contradiction

private def macLLVM : Profile := ⟨.llvm, .aarch64Macos, 2, 4096⟩

-- The emitter really returns Linux x86-64 bytes under the macOS AArch64 LLVM
-- profile. Handoff and refinement premises cannot make that result sound.
example (project : Project) (m : LoweringModels testLanguage testLowering macLLVM)
    (eligible : (r m parser).emitLLVM.inputInvariant (testBIR project)) :
    ¬ (r m parser).emitLLVM.Sound := by
  intro sound
  have success := (sound.1 (testBIR project)
    ⟨testProfile.llvmTriple, testProfile.llvmDataLayout, [], #[]⟩ eligible rfl).2
  exact (show testProfile.llvmTriple ≠ macLLVM.llvmTriple from by decide) success.2.1

example (project : Project) (m : LoweringModels testLanguage testLowering macLLVM) :
    ¬ (r m parser).emitLLVM.success (testBIR project)
      ⟨macLLVM.llvmTriple, testProfile.llvmDataLayout, [], #[]⟩ := by
  intro success
  exact (show testProfile.llvmDataLayout ≠ macLLVM.llvmDataLayout from by decide) success.2.2.1

-- Three BIR definitions exceed the registered profile's two-slot ceiling.
example (project : Project) (m : LoweringModels testLanguage testLowering testProfile) :
    let block : Block _ _ _ := ⟨⟨0⟩, [⟨(), ⟨0⟩⟩, ⟨(), ⟨1⟩⟩, ⟨(), ⟨2⟩⟩], [], .ret none, []⟩
    let bir : BIR testLowering testProfile :=
      { testBIR project (profile := testProfile) with functions := [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[block]⟩)] }
    ¬ (r m parser).emitLCB.inputInvariant bir := by
  dsimp
  intro eligible
  exact (show ¬ 3 ≤ 2 from by decide) eligible.2.2

-- The emitter actually returns this body, but its exact job fails validation.
example (project : Project) (m : LoweringModels testLanguage testLowering testProfile)
    (valid : (r m parser).emitLCB.inputInvariant (testBIR project)) : ¬ (r m parser).emitLCB.Sound := by
  intro sound
  have validated := (sound.1 (testBIR project) testOutput valid rfl).2.2.1
  have rejected : validateJob (Backend.LCB.job badBody) ≠ .ok () := by
    intro h
    have result := congrArg Except.isOk h
    with_unfolding_all contradiction
  exact rejected validated

end

end StageInterfaceControls
