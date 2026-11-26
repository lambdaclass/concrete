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

private abbrev testLanguage : CoreLanguage where
  operands := {
    Val := CoreValue Unit
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
  OtherValue := Unit
  valueShape := rfl
  layout := fun _ _ => some ⟨1, 1⟩
  scalarType := fun _ => some (.int .u8)
  literal := fun _ _ => some (.scalar (.int .u8 42))
  typeDecidableEq := inferInstance
  fieldType := fun _ _ => none
  activeFields := fun _ _ => none
  operation := fun _ _ _ _ _ _ _ _ _ _ => False
  checkTrap := fun _ _ _ => false
  field := fun _ _ => none
  replaceField := fun _ _ _ => none
  fieldRange := fun _ _ _ => none
  encodeOther := fun _ _ _ => none
  decodeOther := fun _ _ _ => none
  safety
    | .expression _ | .call _ (_ :: _) =>
      [⟨0, .divisionByZero, [((), .literal [0])]⟩]
    | .abort => [⟨0, .abort, []⟩]
    | _ => []
  permit := fun _ _ _ own => some own

private def testLowering : LoweringLanguage Unit :=
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
  obtain ⟨conditions, ⟨sourceOp, member, _, _, fromSource⟩, _, fromMIR, _⟩ :=
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
  obtain ⟨_, ⟨sourceOp, member, _, opcode, _⟩, _⟩ :=
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
  obtain ⟨_, ⟨sourceOp, member, _, _, fromSource⟩, _, fromMIR, _⟩ :=
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
  layout := fun _ _ => some ⟨1, 1⟩
  scalarType := fun _ => some (.int .u8)
  literal := fun _ bytes => bytes.head?.map fun byte => .scalar (.int .u8 (Int.ofNat byte.toNat))
  typeDecidableEq := inferInstance
  fieldType := fun _ _ => none
  activeFields := fun _ _ => none
  operation := fun _ _ _ _ _ _ _ _ _ _ => False
  checkTrap := fun _ _ _ => false
  field := fun _ _ => none
  replaceField := fun _ _ _ => none
  fieldRange := fun _ _ _ => none
  encodeOther := fun _ _ other => nomatch other
  decodeOther := fun _ _ _ => none
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
  ⟨.expression false, List.mem_cons_self .., trivial, rfl, rfl⟩

private theorem argCallInventory (project : Project) :
    (checkedArgCore project).payload.ConditionsAt callSite (.call callee) [] :=
  ⟨.call callee [true], List.mem_cons_of_mem _ (List.mem_cons_self ..), trivial, rfl, rfl⟩

-- f(8 / x) is represented by a checked binding followed by a call reading its result.
-- The division owns its evidence and emitted instruction. The call owns only its own reads.
example (project : Project) :
    (checkedArgMIR project).operations = [argDivOccurrence, argCallOccurrence] ∧
    ConsumesEvidence (checkedArgCore project) argDischarges (checkedArgMIR project) := by
  refine ⟨rfl, ?_, ?_, ?_, ?_, ?_⟩
  · change ([(Exec.Demo.mainF, (⟨0⟩ : BlockId), 0), (Exec.Demo.mainF, ⟨0⟩, 1)] : List (FnId × BlockId × Nat)).Nodup
    decide
  · intro site opcode conditions source nonempty
    obtain ⟨op, member, _, opcodeEq, rfl⟩ := source
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

private def storingMemory (project : Project) : Memory testLowering testProfile :=
  let block : MemoryBlock testLowering := ⟨⟨0⟩, [],
    [⟨testOrigin, .beginLifetime storeRoot⟩, ⟨testOrigin, .bind storePlace (.literal ⟨[42]⟩)⟩],
    .ret none, []⟩
  ⟨project, Exec.Demo.mainF, [], [(Exec.Demo.mainF, [(storeRoot, ())], ⟨⟨0⟩, #[block]⟩)],
    [(Exec.Demo.mainF, [(storeRoot, ⟨1, 1⟩)])], [(Exec.Demo.mainF, [(storeRoot, .materialized)])]⟩

private def storeInitial : MemoryRegisters testLanguage testProfile :=
  ⟨⟨Exec.Demo.mainF, ⟨0⟩, 0, [], [], [], [], []⟩, Mem.Memory.empty, [], [], {}⟩

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

example (project : Project) : (storingMemory project).Step (language := testLanguage) storeInitial storeBefore := by
  refine ⟨_, rfl, {storeBefore with instruction := 0}, ⟨by decide, Or.inr ?_⟩, rfl⟩
  exact ⟨rfl, ⟨⟨1, 1⟩, rfl, Or.inr ⟨by decide, ⟨0⟩, 1, ⟨0⟩, storeBefore.bytes, rfl, rfl⟩⟩⟩

private theorem storingWrite (project : Project) :
    (storingMemory project).Writes (language := testLanguage) storeBefore storePlace storeValue
      {storeAfter with instruction := 1} := by
  refine ⟨⟨rfl, fun _ => rfl⟩, (fun _ => ⟨⟨1, 1⟩, rfl, by decide⟩), rfl, rfl, fun _ => Or.inr ⟨storeRange, [(0, [.init 42 none])], rfl, rfl, ?_, ?_⟩⟩
  · exact fun _ => ⟨0, [.init 42 none], List.mem_cons_self .., by decide⟩
  · intro offset bytes member
    have eq := List.mem_singleton.mp member
    cases eq
    refine ⟨by decide, ?_, rfl⟩
    intro byte hb
    cases List.mem_singleton.mp hb
    decide

example (project : Project) : (storingMemory project).Step (language := testLanguage) storeBefore storeAfter :=
  ⟨_, rfl, storeValue, storeBefore, {storeAfter with instruction := 1}, rfl, storingWrite project, rfl⟩

-- Even with a truthful PC and lifetime, an erased byte write cannot satisfy the
-- registered Memory success contract. Its projection leaves the allocated byte uninitialized.
example (project : Project) (m : LoweringModels testLanguage testLowering testProfile)
    (q next : m.memory.execution.State)
    (reachable : m.memory.execution.finite (storingMemory project) q)
    (step : m.memory.execution.step (storingMemory project) q next)
    (before : m.memoryState q = storeBefore)
    (erased : m.memoryState next = {storeAfter with bytes := storeBefore.bytes}) :
    ¬ LowersMemory m (testMIR project []) (storingMemory project) := by
  intro lowers
  have exactStep := lowers.2.2.2.2.2.2.1.2.1 q next reachable step
  rw [before, erased] at exactStep
  obtain ⟨block, fetched, instruction⟩ := exactStep
  have eq : (storingMemory project).blockAt storeBefore = some block := fetched
  change some _ = some block at eq
  cases eq
  obtain ⟨value, afterRead, stored, read, write, finish⟩ := instruction
  have eqRead : some (storeValue, storeBefore) = some (value, afterRead) := read
  cases eqRead
  have eqWrite : some {storeAfter with instruction := 1} = some stored := write.2.2.1
  cases eqWrite
  have bytes := congrArg (fun state => (MemoryState.bytes state).blocks) finish
  exact (show storeBefore.bytes.blocks ≠ storeAfter.bytes.blocks from by decide) bytes


private def returningMIR (project : Project) : MIR testLowering testProfile :=
  ⟨project, Exec.Demo.mainF, [(Exec.Demo.mainF, ⟨[], some ()⟩)],
    [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[⟨⟨0⟩, [], [], .ret (some ⟨(), .literal ⟨[42]⟩⟩), []⟩]⟩)]⟩

private def returningMemory (project : Project) : Memory testLowering testProfile :=
  ⟨project, Exec.Demo.mainF, (returningMIR project).signatures,
    [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[⟨⟨0⟩, [], [], .ret (some ⟨(), .literal ⟨[0]⟩⟩), []⟩]⟩)],
    [], []⟩

private def terminalModel (A : Type) (observed : Observation) (runtime : Concrete.Contracts.Observation)
    (same : observed.SameRuntimeResult runtime) : Model A Nat where
  execution := {
    State := Unit
    initial := fun _ _ => True
    initialized := fun _ => ⟨(), trivial⟩
    step := fun _ _ _ => False
    result := fun _ _ o => o = runtime
    terminal := fun _ _ _ _ _ h => h
    safe := fun _ _ => True
    trace := fun _ _ => runtime.stderr.data.toList.map .stderr
    permitted := fun _ _ => True
    resultTrace := by
      intro _ _ result equals
      subst result
      generalize runtime.stderr.data.toList = bytes
      induction bytes with
      | nil => rfl
      | cons byte rest ih =>
        change byte :: Concrete.Contracts.stderrBytes (rest.map .stderr) = byte :: rest
        rw [ih]
    traceGrows := fun _ _ _ h => False.elim h
  }
  result := fun _ _ o => o = observed
  projects := fun _ o legacy => o = observed ∧ legacy = runtime
  terminalExact := by
    intro _ _ _
    exact ⟨fun h => ⟨_, rfl, rfl, h⟩, fun ⟨_, _, _, h⟩ => h⟩
  completeProjection := fun _ _ _ h => ⟨_, h, rfl⟩
  runtimeExact := by
    rintro _ _ _ ⟨rfl, rfl⟩
    exact same

private def returnsStatus (A : Type) (status : UInt8) : Model A Nat :=
  terminalModel A (.returned status ByteArray.empty) (.returned status ByteArray.empty) ⟨rfl, rfl⟩

private def terminalSimulation {A B : Type} {a : A} {b : B} {o runtime}
    {same : o.SameRuntimeResult runtime} :
    Concrete.Contracts.Simulates (terminalModel A o runtime same).execution
      (terminalModel B o runtime same).execution a b where
  simulation := ⟨fun _ _ => True, fun _ _ => 0, fun _ _ _ _ step => step.elim⟩
  initialMatch _ _ := ⟨(), trivial, trivial⟩
  prefixMatch _ _ _ _ _ := ⟨trivial, trivial, fun _ _ => ⟨trivial, trivial⟩, [], List.append_nil _⟩
  resultMatch p _ _ _ _ _ result := ⟨p, .nil _, result, rfl⟩
  progress _ _ := Or.inr ⟨runtime, rfl⟩
  divergentTrace _ run := (run.2 0).elim

private def falseReturnModels (m : LoweringModels testLanguage testLowering testProfile) :
    LoweringModels testLanguage testLowering testProfile :=
  {m with
    mir := returnsStatus _ 42
    memory := returnsStatus _ 42
    memoryState := fun _ => storeInitial
    mirState := fun _ => storeInitial}

-- The current return instruction really yields zero.
example (project : Project) :
    (returningMemory project).terminalObservation storeInitial =
      some (.returned 0 ByteArray.empty) := rfl

-- A recursive activation of the entry returns to its caller. A trap in that
-- activation still reports the fetched terminator's kind and lexical origin.
example (project : Project) :
    (returningMemory project).terminalObservation
      {storeInitial with callers := [⟨storeInitial.toMemoryFrame, none⟩]} = none := rfl

example (project : Project) :
    let memory : Memory testLowering testProfile := {returningMemory project with
      functions := [(Exec.Demo.mainF, [], ⟨⟨0⟩,
        #[⟨⟨0⟩, [], [], .trap .divisionByZero testOrigin, []⟩]⟩)]}
    memory.terminalObservation {storeInitial with callers := [⟨storeInitial.toMemoryFrame, none⟩]} =
      some (.trapped 134 .divisionByZero testOrigin.lexical
        (trapDiagnostic project .divisionByZero testOrigin.operation testOrigin.lexical)) := rfl

-- The two models agree on 42, but the full handoff must reject their claim:
-- the actual Memory terminator returns zero from its initial state.
example (project : Project) (m : LoweringModels testLanguage testLowering testProfile) :
    ¬ LowersMemory (falseReturnModels m) (returningMIR project) (returningMemory project) := by
  intro lowers
  have terminal := lowers.2.2.2.2.2.2.1.2.2.2 () (.returned 42 ByteArray.empty)
    ⟨(), trivial, .nil ()⟩
  have wrong := terminal.mp rfl
  have status := congrArg (Option.map fun o => match o with
    | .returned code _ => code
    | _ => 255) wrong
  exact (show some (0 : UInt8) ≠ some 42 from by decide) status


-- A structurally correct entry and return cannot carry an extra host effect.
example (project : Project) (m : LoweringModels testLanguage testLowering testProfile)
    (q : m.memory.execution.State) (reachable : m.memory.execution.finite (returningMemory project) q)
    (entry : m.memoryState q = storeInitial)
    (forged : m.memory.execution.trace (returningMemory project) q = [.effect (show testLanguage.operands.Event from (7 : Nat))]) :
    ¬ LowersMemory m (returningMIR project) (returningMemory project) := by
  intro lowers
  have trace := lowers.2.2.2.2.2.2.1.1.2 q reachable
  rw [forged, entry] at trace
  exact (show ([.effect 7] : List (Concrete.Contracts.ProcessEvent Nat)) ≠ [] from nofun) trace

private def twoCallMemory (project : Project) : Memory testLowering testProfile :=
  let caller : MemoryBlock testLowering :=
    ⟨⟨0⟩, [], [⟨testOrigin, .call callee none []⟩, ⟨testOrigin, .call callee none []⟩],
      .ret (some ⟨(), .literal ⟨[0]⟩⟩), []⟩
  let helper : MemoryBlock testLowering := ⟨⟨0⟩, [], [], .ret none, []⟩
  ⟨project, Exec.Demo.mainF, [(Exec.Demo.mainF, ⟨[], some ()⟩), (callee, ⟨[], none⟩)],
    [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[caller]⟩), (callee, [], ⟨⟨0⟩, #[helper]⟩)], [], []⟩

private def resumeAt (index : Nat) : MemoryRegisters testLanguage testProfile :=
  {storeInitial with instruction := index}

private def helperReturning (index : Nat) : MemoryRegisters testLanguage testProfile :=
  {storeInitial with frame := callee, callers := [⟨(resumeAt index).toMemoryFrame, none⟩]}

-- Both calls use the same callee PC and byte memory. Each return pops its own
-- continuation and the second return cannot resume the first call's successor.
example (project : Project) :
    let memory := twoCallMemory project
    memory.Step storeInitial (helperReturning 1) ∧
    memory.Step (helperReturning 1) (resumeAt 1) ∧
    memory.Step (resumeAt 1) (helperReturning 2) ∧
    memory.Step (helperReturning 2) (resumeAt 2) ∧
    (¬ memory.Step (helperReturning 2) (resumeAt 1)) ∧
    memory.terminalObservation (resumeAt 2) = some (.returned 0 ByteArray.empty) := by
  dsimp only
  refine ⟨?_, ?_, ?_, ?_, ?_, rfl⟩
  · exact ⟨_, rfl, rfl, [], _, _, _, rfl, rfl, rfl, rfl, .nil _⟩
  · exact ⟨_, rfl, rfl, _, [], [], _, _, rfl, rfl, .nil _, rfl, .nil _⟩
  · exact ⟨_, rfl, rfl, [], _, _, _, rfl, rfl, rfl, rfl, .nil _⟩
  · exact ⟨_, rfl, rfl, _, [], [], _, _, rfl, rfl, .nil _, rfl, .nil _⟩
  · rintro ⟨block, fetched, instruction⟩
    have fetched : some _ = some block := fetched
    cases fetched
    obtain ⟨_, caller, rest, values, read, ended, stack, evaluated, released, _, written⟩ :=
      instruction
    have stack : _ :: [] = caller :: rest := stack
    cases stack
    have evaluated : some ([], helperReturning 2) = some (values, read) := evaluated
    cases evaluated
    cases released
    cases written

private abbrev zeroLanguage : CoreLanguage :=
  {testLanguage with
    operands := {testLanguage.operands with isCopy := fun _ => false}
    safety := fun _ => []
    layout := fun _ _ => some ⟨0, 1⟩
    scalarType := fun _ => none
    literal := fun _ _ => some (.scalar .unit)}

private def zeroMemory (project : Project) : Memory testLowering testProfile :=
  let block : MemoryBlock testLowering := ⟨⟨0⟩, [],
    [⟨testOrigin, .beginLifetime storeRoot⟩, ⟨testOrigin, .bind storePlace (.literal ⟨[]⟩)⟩,
      ⟨testOrigin, .endLifetime storeRoot⟩], .ret none, []⟩
  {(storingMemory project) with
    functions := [(Exec.Demo.mainF, [(storeRoot, ())], ⟨⟨0⟩, #[block]⟩)]
    layouts := [(Exec.Demo.mainF, [(storeRoot, ⟨0, 1⟩)])]}

private def zeroStarted : MemoryRegisters zeroLanguage testProfile :=
  {storeInitial with instruction := 1, live := [storeRoot]}

private def zeroBound : MemoryRegisters zeroLanguage testProfile :=
  {zeroStarted with
    instruction := 2
    initialized := [storeRoot]
    zeroValues := [(storeRoot, .scalar .unit)]}

-- A materialized zero-byte value goes through begin, bind, linear move, and
-- end with empty byte memory throughout. A subsequent move has no value.
example (project : Project) :
    let memory := zeroMemory project
    let moved := {zeroBound with initialized := [], zeroValues := []}
    memory.Step (language := zeroLanguage) storeInitial zeroStarted ∧
    memory.Step (language := zeroLanguage) zeroStarted zeroBound ∧
    memory.readValue (language := zeroLanguage) zeroBound (.move storePlace) = some (.scalar .unit, moved) ∧
    memory.readValue (language := zeroLanguage) moved (.move storePlace) = none ∧
    memory.Step (language := zeroLanguage) moved {storeInitial with instruction := 3} := by
  dsimp only
  refine ⟨?_, ?_, rfl, rfl, ?_⟩
  · exact ⟨_, rfl, {zeroStarted with instruction := 0},
      ⟨by decide, Or.inr ⟨rfl, ⟨0, 1⟩, rfl, Or.inl ⟨rfl, rfl⟩⟩⟩, rfl⟩
  · refine ⟨_, rfl, _, zeroStarted, {zeroBound with instruction := 1}, rfl, ?_, rfl⟩
    exact ⟨⟨rfl, nofun⟩, (fun _ => ⟨⟨0, 1⟩, rfl, by decide⟩), rfl, rfl,
      fun _ => Or.inl rfl⟩
  · exact ⟨_, rfl, {storeInitial with instruction := 2},
      ⟨⟨by decide, rfl⟩, Or.inr ⟨rfl, Or.inl ⟨⟨0, 1⟩, rfl, rfl, rfl⟩⟩⟩, rfl⟩

private def splitField : FieldId := ⟨.struct ⟨Exec.Demo.mainF.module, ⟨"record"⟩⟩, ⟨"item"⟩⟩

private abbrev splitLanguage : CoreLanguage :=
  {zeroLanguage with
    fieldType := fun _ _ => some ()
    activeFields := fun _ _ => some [splitField]}

-- Destructuring a linear field cannot erase it or create two copies.
example :
    Concrete.Interfaces.Memory.DestructureFields splitLanguage () (.scalar .unit) [(splitField, ())] ∧
    ¬ Concrete.Interfaces.Memory.DestructureFields splitLanguage () (.scalar .unit) [] ∧
    ¬ Concrete.Interfaces.Memory.DestructureFields splitLanguage () (.scalar .unit)
      [(splitField, ()), (splitField, ())] := by
  refine ⟨⟨by decide, [splitField], rfl, ?_, ?_⟩, ?_, ?_⟩
  · intro binding h
    cases List.mem_singleton.mp h
    exact ⟨List.mem_cons_self .., rfl⟩
  · intro field member absent
    exact False.elim (absent member)
  · rintro ⟨_, fields, hfields, _, omitted⟩
    have hfields : some [splitField] = some fields := hfields
    cases hfields
    obtain ⟨_, _, copy⟩ := omitted splitField (List.mem_cons_self ..) (by decide)
    contradiction
  · exact fun h => (show ¬ [splitField, splitField].Nodup from by decide) h.1

private def shadowRoot : PlaceId := ⟨1⟩

private def shadowMIR (project : Project) : MIR testLowering testProfile :=
  let block : MIRGraph testLowering := ⟨⟨0⟩, #[⟨⟨0⟩, [],
    [⟨testOrigin, .bind (ty := ()) ⟨storeRoot, []⟩ (.literal ⟨[]⟩)⟩], .ret none, []⟩]⟩
  ⟨project, Exec.Demo.mainF, [], [(Exec.Demo.mainF, [(storeRoot, ())], block)]⟩

private def shadowMemory (project : Project) : Memory testLowering testProfile :=
  let place : MemoryPlace Unit () := ⟨⟨shadowRoot, []⟩, .virtual⟩
  let block : MemoryGraph testLowering := ⟨⟨0⟩, #[⟨⟨0⟩, [],
    [⟨testOrigin, .beginLifetime shadowRoot⟩, ⟨testOrigin, .bind place (.literal ⟨[]⟩)⟩],
    .ret none, []⟩]⟩
  {(zeroMemory project) with
    functions := [(Exec.Demo.mainF, [(storeRoot, ()), (shadowRoot, ())], block)]
    storage := [(Exec.Demo.mainF, [(storeRoot, .materialized), (shadowRoot, .virtual)])]}

-- A fresh virtual shadow cannot stand in for the required materialized root.
example (project : Project) (m : LoweringModels zeroLanguage testLowering testProfile) :
    ¬ LowersMemory m (shadowMIR project) (shadowMemory project) := by
  intro lowers
  obtain ⟨ty, locals, _, _, _, source, member, _⟩ :=
    lowers.2.2.2.2.2.1.2 Exec.Demo.mainF _ shadowRoot
      (List.mem_cons_self ..) (List.mem_cons_of_mem _ (List.mem_cons_self ..))
  have falseKey : ((shadowRoot, ty) : PlaceId × Unit) ∉ [(storeRoot, ())] := by
    cases ty
    decide
  have eq : locals = [(storeRoot, ())] := congrArg (fun item => item.2.1) (List.mem_singleton.mp source)
  exact falseKey (eq ▸ member)

-- Even a proposed semantics that freezes the MIR projection at its entry
-- must realize the reachable binding and the logical value it initializes.
example (project : Project) (m : LoweringModels zeroLanguage testLowering testProfile)
    (p : m.mir.execution.State) (reachable : m.mir.execution.finite (shadowMIR project) p)
    (erased : ∀ q, m.mirState q = storeInitial) :
    ¬ (shadowMIR project).ExecutionExact (language := zeroLanguage) m := by
  intro exact
  let next : MemoryRegisters zeroLanguage testProfile := {zeroBound with
    instruction := 1, zeroValues := [], virtualValues := [(storeRoot, .scalar .unit)]}
  have binding : (shadowMIR project).Step (language := zeroLanguage) storeInitial next := by
    refine ⟨rfl, _, storeInitial, {next with instruction := 0}, rfl, ?_, rfl⟩
    let view := (shadowMIR project).logicalView (language := zeroLanguage)
    let place : MemoryPlace Unit () := ⟨⟨storeRoot, []⟩, .virtual⟩
    simp only [ite_true]
    with_unfolding_all
      change view.WriteParams (language := zeroLanguage) [⟨(), place⟩]
        [.scalar .unit] storeInitial {next with instruction := 0}
    refine Memory.WriteParams.cons (language := zeroLanguage) (started := {storeInitial with live := [storeRoot]}) ?_ ?_ (.nil _)
    · exact ⟨by decide, Or.inl ⟨rfl, rfl⟩⟩
    · exact ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩
  have projected : (shadowMIR project).Step (language := zeroLanguage) (m.mirState p) next := erased p ▸ binding
  obtain ⟨q, _, hq⟩ := exact.2.2.1 p next reachable projected
  rw [erased] at hq
  exact (show (0 : Nat) ≠ 1 from by decide) (congrArg (fun state : MemoryRegisters zeroLanguage testProfile => state.instruction) hq)

private def abortEvidence : Discharge testLanguage :=
  {unsafeDischarge with trap := .abort, operands := []}

-- The spec instance advertises a purported safety condition at abort.
-- Execution reaches the trap, so admission must reject the supplied evidence.
example (project : Project) :
    let core := testCore project (.abort testSite)
    (∃ state, Exec.Steps core.payload.body core.payload.initial [] state ∧
      state.final? = some (.trapped .abort testSite)) ∧
    ¬ ValidDischarges core [(testSite, [abortEvidence])] := by
  dsimp only
  refine ⟨⟨_, .step (l := .silent) (.silent rfl) (.refl _), rfl⟩, ?_⟩
  intro valid
  obtain ⟨_, _, ⟨op, member, eligible, _, _⟩, _⟩ :=
    (valid.2.2.2 testSite abortEvidence
      ⟨_, List.mem_cons_self .., List.mem_cons_self ..⟩).2.2.2.1
  have abort : op = .abort := by
    simpa [testCore, Payload.operations, stmtOperations] using member
  cases abort
  exact eligible

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


private def projectedStorePlace : MemoryPlace Unit () :=
  ⟨⟨storeRoot, [splitField]⟩, .materialized⟩

private abbrev projectedLanguage : CoreLanguage :=
  {testLanguage with
    fieldType := fun _ _ => some ()
    fieldRange := fun _ _ _ => some (0, ⟨1, 1⟩)}

private abbrev linearProjectedLanguage : CoreLanguage :=
  {projectedLanguage with
    operands := {projectedLanguage.operands with isCopy := fun _ => false}
    safety := fun _ => []}

-- A projected write can replace an initialized field. It cannot revive a
-- consumed root, even when the physical bytes of the moved value remain.
example (project : Project) :
    let memory := storingMemory project
    let moved := {storeAfter with initialized := []}
    memory.readValue (language := linearProjectedLanguage) storeAfter (.move storePlace) =
      some (storeValue, moved) ∧
    memory.writeValue (language := linearProjectedLanguage) moved projectedStorePlace storeValue = none ∧
    memory.writeValue (language := projectedLanguage) storeAfter projectedStorePlace storeValue =
      some storeAfter := ⟨rfl, rfl, rfl⟩

private abbrev projectedZeroLanguage : CoreLanguage :=
  {splitLanguage with
    operands := {splitLanguage.operands with isCopy := fun _ => true}
    safety := fun _ => []
    field := fun _ _ => some (.scalar .unit)
    replaceField := fun _ _ value => some value}

-- Both projected reads and writes execute against the initialized zero-byte
-- root. An otherwise identical uninitialized root refuses the write.
example (project : Project) :
    let memory := zeroMemory project
    memory.readValue (language := projectedZeroLanguage) zeroBound (.copy projectedStorePlace) =
      some (.scalar .unit, zeroBound) ∧
    memory.Writes (language := projectedZeroLanguage) zeroBound projectedStorePlace (.scalar .unit) zeroBound ∧
    memory.writeValue (language := projectedZeroLanguage) zeroStarted projectedStorePlace (.scalar .unit) =
      none := by
  exact ⟨rfl, ⟨⟨rfl, fun _ => rfl⟩, (fun _ => ⟨⟨0, 1⟩, rfl, by decide⟩), rfl,
    rfl, fun _ => Or.inl rfl⟩, rfl⟩

example (project : Project) (next : MemoryRegisters zeroLanguage testProfile) :
    ¬ (zeroMemory project).Ends (language := zeroLanguage) zeroBound storeRoot next := by
  intro ends
  have h : false = true := by
    with_unfolding_all exact ends.1.2
  contradiction

private def alternateRange : Hybrid.Range :=
  {storeRange with ptr := {storeRange.ptr with addr := 2}}

private def alternateBacking : MemoryRegisters testLanguage testProfile :=
  {storeAfter with
    materialized := [(storeRoot, alternateRange)]
    bytes := ⟨[(⟨0⟩, {storeBlock with base := 2, bytes := [.init 42 none]})], 1⟩}

private def resolvedStore (range : Hybrid.Range) : OperationAddress Unit :=
  ⟨(), storeRoot, [], some range⟩

-- Two valid placements of the same initialized value provide different
-- pointers to the operation. Substituting the first pointer for the second
-- fails the address-operand premise even though the stored bytes agree.
example (project : Project) :
    let memory := storingMemory project
    let address := ⟨(), storePlace⟩
    memory.resolveAddress storeAfter false address = some (resolvedStore storeRange) ∧
    memory.resolveAddress alternateBacking false address = some (resolvedStore alternateRange) ∧
    memory.resolveAddress alternateBacking false address ≠ some (resolvedStore storeRange) := by
  refine ⟨rfl, rfl, ?_⟩
  intro wrong
  have pointer := congrArg (Option.bind · (fun operand : OperationAddress Unit =>
    operand.range.map (·.ptr.addr))) wrong
  exact (show some (2 : Nat) ≠ some 1 from by decide) pointer

private abbrev residualLanguage : CoreLanguage :=
  {testLanguage with
    checkTrap := fun _ _ values =>
      match values with
      | [.scalar (.int _ 0)] => true
      | _ => false}

private def runtimeCheck : OperationCheck := ⟨0, .divisionByZero, .residual⟩

private def residualMIR (project : Project) : MIR testLowering testProfile :=
  let block : Block _ _ _ := ⟨⟨0⟩, [],
    [⟨testOrigin, .op (opcode := .arithmetic (.plain .div)) (types := [()]) (ty := ()) ⟨storeRoot, []⟩ ()
      (.cons (.literal ⟨[0]⟩) .nil) [runtimeCheck]⟩], .trap .abort testOrigin, []⟩
  ⟨project, Exec.Demo.mainF, [(Exec.Demo.mainF, ⟨[], none⟩)],
    [(Exec.Demo.mainF, [(storeRoot, ())], ⟨⟨0⟩, #[block]⟩)]⟩

private def residualMemory (project : Project) (checks : List OperationCheck) :
    Memory testLowering testProfile :=
  let block : MemoryBlock testLowering := ⟨⟨0⟩, [],
    [⟨testOrigin, .op (opcode := .arithmetic (.plain .div)) (types := [()]) (ty := ())
      ⟨⟨storeRoot, []⟩, .virtual⟩ ()
      (.cons (.literal ⟨[0]⟩) .nil) checks⟩], .trap .abort testOrigin, []⟩
  ⟨project, Exec.Demo.mainF, (residualMIR project).signatures,
    [(Exec.Demo.mainF, [(storeRoot, ())], ⟨⟨0⟩, #[block]⟩)], [],
    [(Exec.Demo.mainF, [(storeRoot, .virtual)])]⟩

-- The residual trap takes place at the same operation in both IRs, with
-- no synthetic boolean local. Deleting the check loses the observation.
example (project : Project) :
    let trap := .trapped 134 .divisionByZero testOrigin.lexical
      (trapDiagnostic project .divisionByZero testOrigin.operation testOrigin.lexical)
    (residualMIR project).terminalObservation (language := residualLanguage) storeInitial = some trap ∧
    (residualMemory project [runtimeCheck]).terminalObservation
      (language := residualLanguage) storeInitial = some trap ∧
    (residualMemory project []).terminalObservation
      (language := residualLanguage) storeInitial = none := by
  with_unfolding_all exact ⟨rfl, rfl, rfl⟩

private def lcbReturnJob (status : BitVec 32) : LCBInput testProfile :=
  let body : LeanCompilerBackend.Verified.Source.Function :=
    ⟨.word, #[], #[], #[⟨#[], #[], .ret (.literal (.word status))⟩]⟩
  ⟨body, [], #[], (Backend.Runtime.qbe []).toUTF8⟩

-- Structural job validation accepts both functions. Imported execution
-- observes their different returns from the supplied body and runtime input.
example (project : Project) :
    let valid := fun status => LeanCompilerBackend.Verified.validateJob
      (Backend.LCB.job (lcbReturnJob status).body) = .ok ()
    let execution := LCBInput.Execution (profile := testProfile) Nat project
    valid 0 ∧ valid 42 ∧ (lcbReturnJob 0).RuntimeMatches project ∧
    execution.observes (lcbReturnJob 0) (.returned 0 ByteArray.empty) ∧
    execution.observes (lcbReturnJob 42) (.returned 42 ByteArray.empty) := by
  with_unfolding_all
    exact ⟨rfl, rfl, ⟨[], rfl, rfl⟩, ⟨_, ⟨_, rfl, .nil _⟩, rfl⟩,
      ⟨_, ⟨_, rfl, .nil _⟩, rfl⟩⟩

-- The forged BIR and adapter models both say 0, but the emitted valid
-- source job returns 42 under the imported execution.
example (project : Project) (m : LoweringModels testLanguage testLowering testProfile) :
    ¬ EmitsLCB {m with
      bir := returnsStatus _ 0
      lcb := returnsStatus _ 0
      lcbHandoff := fun _ _ => True} (testBIR project) (lcbReturnJob 42) := by
  intro emits
  have actual : (LCBInput.Execution (profile := testProfile) Nat project).observes
      (lcbReturnJob 42) (.returned 42 ByteArray.empty) := by
    with_unfolding_all exact ⟨_, ⟨_, rfl, .nil _⟩, rfl⟩
  obtain ⟨q, finite, result⟩ := actual
  obtain ⟨_, _, wrong, _⟩ := emits.2.2.2.2.2.2.1.2.1 q _ finite result
  have status := congrArg (fun o : Concrete.Contracts.Observation =>
    match o with | .returned code _ => code | _ => 255) wrong
  exact (show (42 : UInt8) ≠ 0 from by decide) status

private abbrev erasureLanguage : CoreLanguage :=
  {zeroLanguage with
    operands := testLanguage.operands
    safety := fun _ => []
    layout := fun _ _ => some ⟨1, 1⟩
    literal := fun _ encoding =>
      if encoding.isEmpty then some (.other ()) else some (.scalar (.int .u8 0))
    encodeOther := fun _ _ _ => some [(0, [.init 42 none])]
    decodeOther := fun _ _ bytes =>
      if bytes == [.init 42 none] then some () else none}

private def immediateReturn (project : Project) : Memory testLowering testProfile :=
  {(zeroMemory project) with
    signatures := [(Exec.Demo.mainF, ⟨[], some ()⟩)]
    functions := [(Exec.Demo.mainF, [(storeRoot, ())], ⟨⟨0⟩,
      #[⟨⟨0⟩, [], [], .ret (some ⟨(), .literal ⟨[0]⟩⟩), [testOrigin]⟩]⟩)]
    layouts := [(Exec.Demo.mainF, [(storeRoot, ⟨1, 1⟩)])]}

private def erasureSource (project : Project) : MIR testLowering testProfile :=
  {(shadowMIR project) with
    signatures := (immediateReturn project).signatures
    functions := [(Exec.Demo.mainF, [(storeRoot, ())], ⟨⟨0⟩,
      #[⟨⟨0⟩, [], [⟨testOrigin, .bind (ty := ()) ⟨storeRoot, []⟩ (.literal ⟨[]⟩)⟩],
        .ret (some ⟨(), .literal ⟨[0]⟩⟩), []⟩]⟩)]}

private def erasureBound : MemoryRegisters erasureLanguage testProfile :=
  {storeInitial with
    instruction := 1
    live := [storeRoot]
    initialized := [storeRoot]
    virtualValues := [(storeRoot, .other ())]}

private theorem erasureBinding (project : Project) :
    (erasureSource project).Step (language := erasureLanguage) storeInitial erasureBound := by
  with_unfolding_all
    refine ⟨rfl, CoreValue.other (), storeInitial, {erasureBound with instruction := 0}, rfl, ?_, rfl⟩
  simp only [ite_true]
  refine Memory.WriteParams.cons (language := erasureLanguage)
    (started := {storeInitial with live := [storeRoot]}) ?_ ?_ (.nil _)
  · exact ⟨by decide, Or.inl ⟨rfl, rfl⟩⟩
  · exact ⟨⟨rfl, fun _ => rfl⟩, nofun, rfl, rfl, nofun⟩

-- Retaining a materialized declaration, its layout and its origin metadata
-- cannot justify an immediate return with no live target local.
example (project : Project) (m : LoweringModels erasureLanguage testLowering testProfile)
    (initial : ∀ p, m.mir.execution.initial (erasureSource project) p →
      m.mirState p = storeInitial) :
    ¬ LowersMemory {m with memory := returnsStatus _ 0, memoryState := fun _ => storeInitial}
      (erasureSource project) (immediateReturn project) := by
  intro lowers
  obtain ⟨p, hp⟩ := m.mir.execution.initialized (erasureSource project)
  have reachable : m.mir.execution.finite (erasureSource project) p :=
    ⟨p, hp, Concrete.Contracts.Steps.nil p⟩
  have exactSource := lowers.2.2.2.2.2.2.2.1
  have step : (erasureSource project).Step (language := erasureLanguage) (m.mirState p) erasureBound := by
    rw [initial p hp]
    exact erasureBinding project
  have sourceStep := exactSource.2.2.1 p erasureBound reachable step
  obtain ⟨p', moved, projected⟩ := sourceStep
  obtain ⟨sim, _, coversInitial, coversStep, represents⟩ := lowers.2.2.2.2.2.2.2.2.2.2
  obtain ⟨q, hq, relation⟩ := coversInitial p hp
  have targetReachable : (returnsStatus (Memory testLowering testProfile) 0).execution.finite
      (immediateReturn project) q := ⟨q, hq, Concrete.Contracts.Steps.nil q⟩
  obtain ⟨q', _, related⟩ := coversStep p p' q reachable targetReachable relation moved
  cases q'
  have sourceReachable : m.mir.execution.finite (erasureSource project) p' :=
    ⟨p, hp, Concrete.Contracts.Steps.cons moved (.nil p')⟩
  have physical := (represents p' () sourceReachable ⟨(), trivial, .nil ()⟩ related).1
  rw [projected] at physical
  have missing : storeRoot ∈ ([] : List PlaceId) :=
    physical.2.2.1 storeRoot (List.mem_cons_self ..)
  exact List.not_mem_nil missing

private def residualObserved (project : Project) : Observation :=
  .trapped 134 .divisionByZero testOrigin.lexical
    (trapDiagnostic project .divisionByZero testOrigin.operation testOrigin.lexical)

private def residualRuntime (project : Project) : Concrete.Contracts.Observation :=
  .trapped 134 .divisionByZero
    ⟨⟨testOrigin.operation.id⟩, ⟨"", testOrigin.lexical.line, testOrigin.lexical.col⟩⟩
    (residualObserved project).stderr

private theorem residualSame (project : Project) :
    (residualObserved project).SameRuntimeResult (residualRuntime project) :=
  ⟨rfl, rfl, rfl, rfl, rfl⟩

private def residualModel (A : Type) (project : Project) : Model A Nat :=
  terminalModel A (residualObserved project) (residualRuntime project) (residualSame project)

private def residualModels (m : LoweringModels residualLanguage testLowering testProfile)
    (project : Project) : LoweringModels residualLanguage testLowering testProfile :=
  {m with
    mir := residualModel _ project
    memory := residualModel _ project
    mirState := fun _ => storeInitial
    memoryState := fun _ => storeInitial}

private theorem residualStops (project : Project) (next : MemoryRegisters residualLanguage testProfile) :
    ¬ (residualMemory project [runtimeCheck]).Step (language := residualLanguage) storeInitial next := by
  rintro ⟨block, fetched, step⟩
  have equality : some _ = some block := fetched
  cases equality
  obtain ⟨values, state, _, _, _, _, _, read, checked, _⟩ := step
  have readExact : some ([.scalar (.int .u8 0)], storeInitial) = some (values, state) := read
  cases readExact
  have impossible : some TrapKind.divisionByZero = none := checked
  contradiction

private theorem residualLogicalStops (project : Project)
    (next : MemoryRegisters residualLanguage testProfile) :
    ¬ (residualMIR project).Step (language := residualLanguage) storeInitial next := by
  intro step
  have impossible : some TrapKind.divisionByZero = none := step.1
  contradiction

private theorem residualTables (project : Project) :
    (residualMemory project [runtimeCheck]).TablesWF := by
  refine ⟨?_, ?_, ?_, ?_, ?_, nofun, ?_⟩
  · change [Exec.Demo.mainF].Nodup
    simp
  · change [Exec.Demo.mainF].Nodup
    simp
  · change [Exec.Demo.mainF].Nodup
    simp
  · change ([] : List FnId).Nodup
    simp
  · intro _ _ member
    cases List.mem_singleton.mp member
    decide
  · intro fn locals graph member
    cases List.mem_singleton.mp member
    refine ⟨by decide, ⟨by decide, ⟨_, List.mem_singleton_self _, rfl⟩, ?_⟩, ?_⟩
    · intro block inGraph
      cases List.mem_singleton.mp inGraph
      exact fun _ => nofun
    · refine ⟨_, rfl, ⟨_, List.mem_singleton_self _, rfl, rfl⟩, ?_⟩
      intro block inGraph
      cases List.mem_singleton.mp inGraph
      refine ⟨trivial, ?_⟩
      intro op inBlock
      cases List.mem_singleton.mp inBlock
      trivial

-- The complete contract has a model for a branch-free operation that traps at
-- its residual check. The same models reject dropping the check.
example (project : Project) (m : LoweringModels residualLanguage testLowering testProfile) :
    LowersMemory (residualModels m project) (residualMIR project) (residualMemory project [runtimeCheck]) ∧
    ¬ LowersMemory (residualModels m project) (residualMIR project) (residualMemory project []) := by
  constructor
  · refine ⟨rfl, rfl, ⟨fun _ h => h, fun _ h => h⟩, ?_, rfl, ?_, ?_, ?_, ?_, ?_, ?_⟩
    · constructor
      · intro fn locals graph member place ty localMember
        cases List.mem_singleton.mp member
        have localEntry : (place, ty) ∈ ([(storeRoot, ())] : LocalTypes Unit) := localMember
        cases List.mem_singleton.mp localEntry
        exact ⟨_, rfl, Or.inl rfl⟩
      · nofun
    · have noEscape : ¬ (residualMIR project).Escapes (language := residualLanguage)
          Exec.Demo.mainF storeRoot := by
        have ops : (residualMIR project).operations =
            [⟨Exec.Demo.mainF, ⟨0⟩, 0, testOrigin, .operator (.arithmetic (.plain .div)),
              [((), .literal [0])], [runtimeCheck]⟩] := by with_unfolding_all rfl
        rintro ⟨op, member, _, ty, path, addressed⟩
        have singleton : op ∈ [⟨Exec.Demo.mainF, ⟨0⟩, 0, testOrigin,
            .operator (.arithmetic (.plain .div)), [((), .literal [0])], [runtimeCheck]⟩] :=
          ops ▸ member
        cases List.mem_singleton.mp singleton
        exact OperandRead.noConfusion (congrArg Prod.snd (List.mem_singleton.mp addressed))
      constructor
      · intro fn locals graph member place ty localMember
        cases List.mem_singleton.mp member
        have localEntry : (place, ty) ∈ ([(storeRoot, ())] : LocalTypes Unit) := localMember
        cases List.mem_singleton.mp localEntry
        exact ⟨_, .virtual, rfl, rfl, ⟨fun _ => ⟨rfl, noEscape⟩, fun _ => rfl⟩⟩
      · intro fn classes place member localMember
        simp only [residualMemory, List.mem_singleton, Prod.mk.injEq] at member
        rcases member with ⟨rfl, rfl⟩
        simp only [List.mem_singleton, Prod.mk.injEq, and_true] at localMember
        subst place
        exact ⟨(), _, _, _, _, List.mem_singleton_self _, List.mem_singleton_self _,
          List.mem_singleton_self _, List.mem_singleton_self _, rfl, noEscape⟩
    · refine ⟨⟨fun _ _ => ?_, fun _ _ => ?_⟩,
        fun _ _ _ h => h.elim, fun _ next _ step => (residualStops project next step).elim, ?_⟩
      · exact ⟨rfl, rfl, ⟨_, rfl, rfl⟩, ⟨_, rfl, rfl⟩, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩
      · with_unfolding_all rfl
      · intro _ _ _
        with_unfolding_all exact ⟨fun h => congrArg some h.symm, fun h => (Option.some.inj h).symm⟩
    · refine ⟨⟨fun _ _ => ?_, fun _ _ => ?_⟩,
        fun _ _ _ h => h.elim, fun _ next _ step => (residualLogicalStops project next step).elim, ?_⟩
      · with_unfolding_all
          exact ⟨rfl, rfl, ⟨_, rfl, rfl⟩, ⟨_, rfl, rfl⟩, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩
      · with_unfolding_all rfl
      · intro _ _ _
        with_unfolding_all exact ⟨fun h => congrArg some h.symm, fun h => (Option.some.inj h).symm⟩
    · exact residualTables project
    · have view : (residualMIR project).logicalView (language := residualLanguage) =
          residualMemory project [runtimeCheck] := by
        with_unfolding_all rfl
      exact view.symm ▸ residualTables project
    · refine ⟨terminalSimulation (same := residualSame project), ?_, fun _ _ => ⟨(), trivial, trivial⟩,
        fun _ _ _ _ _ _ step => step.elim, ?_⟩
      · exact fun p _ _ _ _ _ result => ⟨p, .nil _, result, rfl⟩
      · intro _ _ _ _ _
        refine ⟨?_, rfl, nofun⟩
        change (residualMemory project [runtimeCheck]).RepresentsActivation
          (language := residualLanguage) storeInitial.toMemoryFrame storeInitial.toMemoryFrame Mem.Memory.empty
        exact ⟨⟨⟨nofun, by decide, nofun, nofun, nofun, nofun, nofun, nofun⟩,
          by decide, by decide, nofun⟩, rfl, nofun, by decide, by decide, by decide, by decide,
          nofun, (fun _ => ⟨nofun, nofun⟩), nofun, nofun, nofun, nofun⟩
  · intro lowers
    have exactTarget := lowers.2.2.2.2.2.2.1.2.2.2 () (residualObserved project)
      ⟨(), trivial, .nil _⟩
    have impossible : none = some (residualObserved project) := exactTarget.mp rfl
    contradiction

private abbrev addressLanguage : CoreLanguage :=
  {testLanguage with
    operation := fun _ _ _ _ _ operands before result effects after =>
      ∃ (range : Hybrid.Range),
        operands = [resolvedStore range] ∧
        result = .scalar (.int .u8 range.ptr.addr) ∧ effects = [] ∧ after = before}

private def addressResultPlace : MemoryPlace Unit () :=
  ⟨⟨shadowRoot, []⟩, .virtual⟩

private def addressMemory (project : Project) : Memory testLowering testProfile :=
  {(storingMemory project) with
    functions := List.map (fun (fn, locals, graph) =>
      (fn, (shadowRoot, ()) :: locals, graph)) (storingMemory project).functions
    storage := [(Exec.Demo.mainF, [(storeRoot, .materialized), (shadowRoot, .virtual)])]}

private def addressInstruction : MemoryInst testLowering :=
  .op (opcode := .named ⟨"address"⟩) (types := []) (ty := ()) addressResultPlace () .nil []
    [⟨(), storePlace⟩]

private def addressReady (base : MemoryRegisters addressLanguage testProfile) :
    MemoryRegisters addressLanguage testProfile :=
  {base with live := shadowRoot :: base.live}

private def addressWritten (base : MemoryRegisters addressLanguage testProfile) (address : Nat) :
    MemoryRegisters addressLanguage testProfile :=
  {(addressReady base) with
    initialized := shadowRoot :: base.initialized
    virtualValues := [(shadowRoot, .scalar (.int .u8 address))]}

/-- The fixture operation returns the address supplied by the instruction
judgment, so storage equality cannot make two allocations interchangeable. -/
example (project : Project) :
    let memory := addressMemory project
    memory.InstructionStep (language := addressLanguage) (addressReady storeAfter)
      (addressWritten storeAfter 1).advance false addressInstruction ∧
    memory.InstructionStep (language := addressLanguage) (addressReady alternateBacking)
      (addressWritten alternateBacking 2).advance false addressInstruction ∧
    ¬ memory.InstructionStep (language := addressLanguage) (addressReady alternateBacking)
      (addressWritten alternateBacking 1).advance false addressInstruction := by
  dsimp only
  refine ⟨?_, ?_, ?_⟩
  · refine ⟨[], _, [resolvedStore storeRange], _, [], _, addressWritten storeAfter 1,
      rfl, rfl, rfl, ⟨storeRange, rfl, rfl, rfl, rfl⟩, ?_, rfl⟩
    exact ⟨⟨rfl, fun _ => rfl⟩, nofun, rfl, rfl, nofun⟩
  · refine ⟨[], _, [resolvedStore alternateRange], _, [], _, addressWritten alternateBacking 2,
      rfl, rfl, rfl, ⟨alternateRange, rfl, rfl, rfl, rfl⟩, ?_, rfl⟩
    exact ⟨⟨rfl, fun _ => rfl⟩, nofun, rfl, rfl, nofun⟩
  · rintro ⟨values, afterRead, addresses, result, effects, changed, stored,
      reads, _, resolved, ⟨range, operands, value, _, unchanged⟩, writes, next⟩
    have readEq : some ([], addressReady alternateBacking) = some (values, afterRead) := reads
    cases readEq
    have resolvedEq : some [resolvedStore alternateRange] = some addresses := resolved
    cases resolvedEq
    have same : alternateRange = range := by
      have sameAddress := (List.cons.inj operands).1
      exact Option.some.inj (congrArg OperationAddress.range sameAddress)
    subst range
    subst changed
    subst result
    have written : some (addressWritten alternateBacking 2) = some stored := writes.2.2.1
    cases written
    have contradictionAtRoot := congrArg
      (fun state : MemoryRegisters addressLanguage testProfile => state.virtualValues) next
    cases contradictionAtRoot

private def zeroBIR (project : Project) : BIR testLowering testProfile :=
  let block : Block _ _ _ := ⟨⟨0⟩, [], [], .ret (some ⟨(), .literal ⟨[0]⟩⟩), []⟩
  {(testBIR project (profile := testProfile)) with
    functions := [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[block]⟩)]}

-- The same contract accepts the valid emitted job whose imported execution
-- yields the source result, with the exact runtime bytes.
example (project : Project) (m : LoweringModels testLanguage testLowering testProfile) :
    EmitsLCB {m with
      bir := returnsStatus _ 0
      lcb := returnsStatus _ 0
      lcbHandoff := fun _ _ => True} (zeroBIR project) (lcbReturnJob 0) := by
  refine ⟨rfl, by with_unfolding_all rfl, nofun, by simp [lcbReturnJob], rfl,
    ⟨[], rfl, rfl⟩, ?_, trivial, ⟨terminalSimulation (same := (show
      (Observation.returned 0 ByteArray.empty).SameRuntimeResult (.returned 0 ByteArray.empty)
      from ⟨rfl, rfl⟩)), ?_⟩⟩
  · let execution := LCBInput.Execution (profile := testProfile) Nat project
    let entry : execution.State :=
      (LeanCompilerBackend.Verified.Source.initial (lcbReturnJob 0).body #[]).toOption
    have result : execution.result (lcbReturnJob 0) entry (.returned 0 ByteArray.empty) := by
      with_unfolding_all rfl
    have only : ∀ p, execution.initial (lcbReturnJob 0) p → p = entry := fun _ h => h
    have reachable : ∀ q, execution.finite (lcbReturnJob 0) q → q = entry := by
      intro q
      rintro ⟨p, initial, steps⟩
      have atEntry := only p initial
      subst p
      cases steps with
      | nil => rfl
      | cons step _ => exact (execution.terminal _ _ _ result _ step).elim
    refine ⟨?_, ?_, ?_, ?_⟩
    · intro q finite
      obtain rfl := reachable q finite
      refine ⟨Or.inr ?_, fun event member => ?_, (), ⟨(), trivial, .nil _⟩, trivial, [], ?_⟩
      · with_unfolding_all rfl
      · have impossible : event ∈ ([] : List (Concrete.Contracts.ProcessEvent Nat)) := by
          with_unfolding_all exact member
        exact (List.not_mem_nil impossible).elim
      · with_unfolding_all rfl
    · intro q o finite observed
      obtain rfl := reachable q finite
      have matching : some (Concrete.Contracts.Observation.returned 0 ByteArray.empty) = some o := by
        with_unfolding_all exact observed
      refine ⟨(), ⟨(), trivial, .nil _⟩, (Option.some.inj matching).symm, ?_⟩
      with_unfolding_all rfl
    · intro run diverges
      have start := only (run 0) diverges.1
      have step := diverges.2 0
      rw [start] at step
      exact (execution.terminal _ _ _ result _ step).elim
    · intro q stuck
      have same := reachable q stuck.1
      subst q
      exact stuck.2.2 _ result
  · exact fun p _ _ _ _ _ result => ⟨p, .nil _, result, rfl⟩

private theorem storedBytesWF : storeAfter.bytes.WellFormed testProfile.memoryTarget := by
  let stored := {storeBlock with bytes := [.init 42 none]}
  have lookup : ∀ block value, storeAfter.bytes.block block = some value →
      block = ⟨0⟩ ∧ value = stored := by
    intro block value found
    change (if (⟨0⟩ : BlockId) = block then some stored else none) = some value at found
    split at found
    · rename_i eq
      exact ⟨eq.symm, (Option.some.inj found).symm⟩
    · contradiction
  refine ⟨?_, by decide, ?_, ?_, ?_, ?_, ?_, ?_⟩
  · intro entry member
    change entry ∈ [(⟨0⟩, stored)] at member
    cases List.mem_singleton.mp member
    rfl
  · intro block value found
    rcases lookup block value found with ⟨rfl, rfl⟩
    decide
  · intro block value found
    rcases lookup block value found with ⟨_, rfl⟩
    rfl
  · intro block value found
    rcases lookup block value found with ⟨_, rfl⟩
    decide
  · intro block value found
    rcases lookup block value found with ⟨_, rfl⟩
    decide
  · intro block value found
    rcases lookup block value found with ⟨_, rfl⟩
    decide
  · intro b x c y first second distinct _ _
    exact (distinct ((lookup b x first).1.trans (lookup c y second).1.symm)).elim

private def materializedBinding (project : Project) : Memory testLowering testProfile :=
  let block : MemoryBlock testLowering := ⟨⟨0⟩, [],
    [⟨testOrigin, .beginLifetime storeRoot⟩, ⟨testOrigin, .bind storePlace (.literal ⟨[]⟩)⟩],
    .ret (some ⟨(), .literal ⟨[0]⟩⟩), []⟩
  {(immediateReturn project) with
    functions := [(Exec.Demo.mainF, [(storeRoot, ())], ⟨⟨0⟩, #[block]⟩)]}

private theorem aggregateWrite (project : Project) :
    (materializedBinding project).Writes (language := erasureLanguage) storeBefore storePlace
      (.other ()) {storeAfter with instruction := 1} := by
  refine ⟨⟨rfl, fun _ => rfl⟩, (fun _ => ⟨⟨1, 1⟩, rfl, by decide⟩),
    rfl, rfl, fun _ => Or.inr ⟨storeRange, [(0, [.init 42 none])], rfl, rfl, ?_, ?_⟩⟩
  · exact fun _ => ⟨0, [.init 42 none], List.mem_cons_self .., by decide⟩
  · intro offset bytes member
    cases List.mem_singleton.mp member
    refine ⟨by decide, ?_, rfl⟩
    intro byte included
    cases List.mem_singleton.mp included
    decide

-- The same binding that the erased lowering omits has a covered execution
-- with live backing bytes, satisfying representation and preserving its return.
example (project : Project) :
    let memory := materializedBinding project
    (erasureSource project).Step (language := erasureLanguage) storeInitial erasureBound ∧
    Concrete.Contracts.Steps (memory.Step (language := erasureLanguage)) storeInitial storeAfter ∧
    memory.RepresentsActivation (language := erasureLanguage)
      erasureBound.toMemoryFrame storeAfter.toMemoryFrame storeAfter.bytes ∧
    (erasureSource project).terminalObservation (language := erasureLanguage) erasureBound =
      some (.returned 0 ByteArray.empty) ∧
    memory.terminalObservation (language := erasureLanguage) storeAfter =
      some (.returned 0 ByteArray.empty) := by
  dsimp only
  refine ⟨erasureBinding project, .cons (b := storeBefore) ?_ (.cons ?_ (.nil _)), ?_, ?_, ?_⟩
  · refine ⟨_, rfl, {storeBefore with instruction := 0}, ⟨by decide, Or.inr ?_⟩, rfl⟩
    exact ⟨rfl, ⟨⟨1, 1⟩, rfl, Or.inr ⟨by decide, ⟨0⟩, 1, ⟨0⟩, storeBefore.bytes, rfl, rfl⟩⟩⟩
  · exact ⟨_, rfl, CoreValue.other (), storeBefore, {storeAfter with instruction := 1},
      rfl, aggregateWrite project, rfl⟩
  · refine ⟨⟨storedBytesWF, by decide, by decide, fun _ h => h⟩, rfl, fun _ h => h,
      by decide, by decide, by decide, by decide, nofun, ?_, ?_, nofun, ?_, ?_⟩
    · intro place
      change (place ∈ [storeRoot]) ↔ (AList.lookup [(storeRoot, CoreValue.other ())] place :
        Option (CoreValue Unit)).isSome = true
      by_cases equal : storeRoot = place
      · subst place
        decide
      · simp [AList.lookup, equal, Ne.symm equal]
    · intro place value member
      exact List.mem_singleton.mpr (congrArg Prod.fst (List.mem_singleton.mp member))
    · intro place range member
      cases List.mem_singleton.mp member
      exact ⟨List.mem_singleton_self _, rfl,
        ⟨⟨1, 1⟩, ⟨0⟩, _, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩⟩
    · intro place member
      have eq := List.mem_singleton.mp member
      subst place
      refine ⟨_, _, (), .materialized, List.mem_singleton_self _, rfl, rfl, ?_, ?_⟩
      · intro _
        exact ⟨⟨1, 1⟩, rfl, ⟨nofun, nofun⟩⟩
      · intro value lookedUp
        have same : some (CoreValue.other ()) = some value := lookedUp
        cases same
        exact (aggregateWrite project).2.2.2
  · with_unfolding_all rfl
  · with_unfolding_all rfl

end StageInterfaceControls
