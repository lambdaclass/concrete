import Concrete.IntegratedCore
import Concrete.Interfaces.Pipeline
import Concrete.Memory.Execution
import Concrete.Lex
import Concrete.Parse
import ConcreteSpec.Admission.Early
import ConcreteSpec.Proofs.Checker
import ConcreteSpec.Proofs.Memory
import ConcreteSpecEvidence.ExecExamples

open Concrete
open Concrete.Interfaces
open ConcreteSpec Core Core.SourcePathSpan StageInterface StageGrammar
open Concrete.LL1
open LeanCompilerBackend.Verified (validateJob)

namespace Concrete.Interfaces

open Concrete.Contracts (ProcessEvent Semantics)
open ConcreteSpec.Core ConcreteSpec.Core.SourcePathSpan ConcreteSpec.StageInterface

-- Candidate models are useful for negative tests of the projection rules.
-- The registered edges themselves execute the fixed graph definitions.
def Observation.SameRuntimeResult (observed : Observation)
    (legacy : Concrete.Contracts.Observation) : Prop :=
  match observed, legacy with
  | .returned a bytes, .returned b other => a = b ∧ bytes = other
  | .trapped a k span bytes, .trapped b j site other =>
    a = b ∧ k = j ∧ span.line = site.location.line ∧ span.col = site.location.col ∧ bytes = other
  | .startupRefused a bytes, .startupRefused b other => a = b ∧ bytes = other
  | .resourceExhausted, .resourceExhausted => True
  | _, _ => False

/-- A precise terminal projection over the same transition system used by the composed theorem.
The bridge retains the old runtime diagnostic equality during migration. -/
structure Model (A E : Type) where
  execution : Semantics A E
  result : A → execution.State → Observation → Prop
  projects : A → Observation → Concrete.Contracts.Observation → Prop
  terminalExact : ∀ a state legacy, execution.result a state legacy ↔
    ∃ observed, result a state observed ∧ projects a observed legacy
  completeProjection : ∀ a state observed, result a state observed →
    ∃ legacy, projects a observed legacy
  runtimeExact : ∀ a observed legacy, projects a observed legacy →
    observed.SameRuntimeResult legacy

def Model.Refines {A B E : Type} (source : Model A E) (target : Model B E)
    (a : A) (b : B) : Prop :=
  BehaviorRefinement source.execution target.execution source.result target.result a b

def coreRuntimeEvents {language : CoreLanguage} (core : CoreInput language)
    (state : Exec.Config core.machineProgram.machine) : List (ProcessEvent language.operands.Event) :=
  match state.final? with
  | some (.trapped kind site) => (coreTrapDiagnostic core kind site).data.toList.map .stderr
  | _ => []

def coreObservation {language : CoreLanguage} (core : CoreInput language)
    (outcome : Exec.Final language.operands.Val) : Option Observation := do
  match ← language.hostedResult outcome with
  | .returned status => return .returned status ByteArray.empty
  | .trapped status kind site =>
    let origin ← AList.lookup core.machineProgram.origins site
    return .trapped status kind origin.lexical (coreTrapDiagnostic core kind site)

/-- Relate the shared theorem model to actual spec configurations and spec transition labels. -/
structure CoreModel (language : CoreLanguage) where
  model : Model (CoreInput language) language.operands.Event
  config : (a : CoreInput language) → model.execution.State → Exec.Config a.machineProgram.machine
  specTrace : CoreInput language → model.execution.State → List language.operands.Event
  initialExact : ∀ a state, model.execution.initial a state →
    config a state = a.machineProgram.initial ∧ specTrace a state = [] ∧
      model.execution.trace a state = []
  traceExact : ∀ a state, model.execution.trace a state =
    (specTrace a state).map ProcessEvent.effect ++ coreRuntimeEvents a (config a state)
  authorityExact : ∀ a e, model.execution.permitted a e ↔
    ∃ state events, Exec.Steps a.machineProgram.body a.machineProgram.initial events state ∧
      (e ∈ coreRuntimeEvents a state ∨
        ∃ label next, Exec.Step a.machineProgram.body state label next ∧
          e ∈ label.events.map ProcessEvent.effect)
  stepsExact : ∀ a state next, model.execution.step a state next →
    ∃ label, Exec.Step a.machineProgram.body (config a state) label (config a next) ∧
      specTrace a next = specTrace a state ++ label.events
  stepsComplete : ∀ a state label q, model.execution.finite a state →
    Exec.Step a.machineProgram.body (config a state) label q →
    ∃ next, model.execution.step a state next ∧ config a next = q ∧
      specTrace a next = specTrace a state ++ label.events
  safetyCoversTraps : ∀ (a : CoreInput language) state events site op label next kind,
    Exec.Steps a.machineProgram.body a.machineProgram.initial events state →
    a.machineProgram.activeOperation state = some (site, op) →
    Exec.Step a.machineProgram.body state label next → next.ctrl = .trapped kind site →
    ∃ condition ∈ language.safety op, condition.trap = kind
  terminalExact : ∀ a state observed, model.result a state observed ↔
    ∃ outcome, (config a state).final? = some outcome ∧ coreObservation a outcome = some observed

structure LoweringModels (language : CoreLanguage) (lang : LoweringLanguage language.operands.Ty)
    (profile : Profile) where
  core : CoreModel language
  mir : Model (MIR lang profile) language.operands.Event
  memory : Model (Memory lang profile) language.operands.Event
  ssa : Model (SSA lang profile) language.operands.Event
  mirInvariant : MIR lang profile → Prop
  memoryInvariant : Memory lang profile → Prop
  ssaInvariant : SSA lang profile → Prop
  birInvariant : BIR lang profile → Prop
  memoryState : memory.execution.State → MemoryRegisters language profile
  mirState : mir.execution.State → MemoryRegisters language profile

variable {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty} {profile : Profile}

private def executionTrace (state : MemoryRegisters language profile)
    (observation : Option Observation) : List (ProcessEvent language.operands.Event) :=
  state.effects.map ProcessEvent.effect ++
    match observation with
    | some (.trapped _ _ _ diagnostic) => diagnostic.data.toList.map ProcessEvent.stderr
    | _ => []

def Memory.ExecutionExact (memory : Memory lang profile)
    (m : LoweringModels language lang profile) : Prop :=
  ((∀ q, m.memory.execution.initial memory q → memory.Initial (m.memoryState q)) ∧
   (∀ q, m.memory.execution.finite memory q → m.memory.execution.trace memory q =
     executionTrace (m.memoryState q) (memory.terminalObservation (m.memoryState q)))) ∧
  (∀ q next, m.memory.execution.finite memory q → m.memory.execution.step memory q next →
    memory.Step (m.memoryState q) (m.memoryState next)) ∧
  (∀ q state, m.memory.execution.finite memory q → memory.Step (m.memoryState q) state →
    ∃ next, m.memory.execution.step memory q next ∧ m.memoryState next = state) ∧
  ∀ q result, m.memory.execution.finite memory q →
    (m.memory.result memory q result ↔
      memory.terminalObservation (m.memoryState q) = some result)

def MIR.ExecutionExact (mir : MIR lang profile)
    (m : LoweringModels language lang profile) : Prop :=
  ((∀ p, m.mir.execution.initial mir p → mir.logicalView.Initial (m.mirState p)) ∧
   (∀ p, m.mir.execution.finite mir p → m.mir.execution.trace mir p =
     executionTrace (m.mirState p) (mir.terminalObservation (m.mirState p)))) ∧
  (∀ p next, m.mir.execution.finite mir p → m.mir.execution.step mir p next →
    mir.Step (m.mirState p) (m.mirState next)) ∧
  (∀ p state, m.mir.execution.finite mir p → mir.Step (m.mirState p) state →
    ∃ next, m.mir.execution.step mir p next ∧ m.mirState next = state) ∧
  ∀ p result, m.mir.execution.finite mir p →
    (m.mir.result mir p result ↔ mir.terminalObservation (m.mirState p) = some result)

/-- Each target execution is matched against a source path whose checkpoints
retain the required physical representations. -/
def MemorySimulationWitness (m : LoweringModels language lang profile)
    (mir : MIR lang profile) (memory : Memory lang profile) : Prop :=
  memory.project = mir.project ∧ memory.entry = mir.entry ∧
  PreservesOrigins mir.origins memory.origins ∧ LayoutsMatch language.layout memory ∧
  memory.signatures = mir.signatures ∧ PromotionSound (language := language) mir memory ∧
  memory.ExecutionExact m ∧ mir.ExecutionExact m ∧ memory.TablesWF ∧ mir.logicalView.TablesWF ∧
  m.mir.Refines m.memory mir memory ∧
  ∃ sim : CheckpointSimulation m.mir.execution m.memory.execution m.mir.result m.memory.result mir memory,
    ∀ p q, m.mir.execution.finite mir p → m.memory.execution.finite memory q →
      sim.related p q → memory.RepresentsCheckpoint (m.mirState p) (m.memoryState q)

end Concrete.Interfaces

namespace StageInterfaceControls

private def early : Slice := ⟨0, by decide⟩
private def later : Slice := ⟨2, by decide⟩

private def production : Production (fun (_ : Fin 1) => SyntaxTree ⊕ Bool) :=
  ⟨0, [.t .integer], early, .constant (.inr true)⟩

private def grammar : SharedGrammar 1 :=
  { terminalSlices := Array.replicate Surface0.Terminal.count early
    terminalSliceCoverage := Surface0.Terminal.index_lt_count
    terminalSliceExact := fun i =>
      ⟨Surface0.Terminal.ofNat i.val, Surface0.Terminal.index_ofNat ⟨i.val, i.isLt⟩⟩
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
    value := fun _ => SyntaxTree ⊕ Bool
    extraAst := Bool
    startShape := rfl
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
  expressionOperator _ := .arithmetic (.plain .div)
  primitiveOperator p := nomatch p
  argumentRead _ := none
  argumentReadNoTrap := by simp
  OtherValue := Unit
  valueShape := rfl
  layout := fun _ _ _ => some ⟨1, 1, .nil⟩
  scalarType := fun _ => some (.int .u8)
  literal := fun _ bytes => (sliceLanguage slice0).literal (.inl (.scalar (.int .u8))) bytes |>.bind fun
    | .scalar v => some (.scalar v)
    | .other _ => none
  typeDecidableEq := inferInstance
  fieldType := fun _ _ => none
  activeFields := fun _ _ _ => none
  operation := fun _ _ _ _ _ _ _ _ _ _ _ => False
  checkTrap := fun _ _ _ => false
  field := fun _ _ => none
  replaceField := fun _ _ _ => none
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

private def testProfile : Profile := ⟨.lcb, .x86_64Linux, 3, 4096⟩

private def testOrigin : Origin := ⟨testSite, span, ⟨.function Exec.Demo.mainF, ⟨[]⟩⟩⟩

private def testCore (project : Project) (body : Exec.Stmt (testLanguage.instrumented []) := .ret () testSite) : CoreInput testLanguage :=
  ⟨project, ⟨0, by decide⟩, .integrated ⟨[], [],
    ⟨[(Exec.Demo.mainF, ⟨[], (), ⟨0⟩, body⟩)]⟩,
    Exec.Demo.mainF, [(testSite, testOrigin)]⟩⟩

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
  ⟨project, Exec.Demo.mainF, [], [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[entry, raw, trap]⟩)], [], []⟩

private def badBody : LeanCompilerBackend.Verified.Source.Function := ⟨.word, #[], #[], #[]⟩

private def testBIR (project : Project) {profile : Profile} : BIR testLowering profile :=
  ⟨project, Exec.Demo.mainF, [], [], [], #[], [], []⟩

private def testOutput {profile : Profile} : LCBInput profile := ⟨badBody, [], #[], ByteArray.empty⟩

private def unsafeDischarge : Discharge testLanguage :=
  ⟨testSite, testOrigin.evidenceKey, ⟨testSite, .safety, 0⟩, .divisionByZero,
    [((), .literal [0])]⟩

private def unsafeDischarges : Discharges testLanguage := [(testSite, [unsafeDischarge])]

private def testPasses {profile : Profile} (checks : List OperationCheck) :
    PassFunctions grammar testLanguage testLowering profile where
  lex _ := .error (.malformed "control")
  elaborate _ := .error (.diagnostic rule span [])
  checkCore _ := .ok unsafeDischarges
  lowerCore input := .ok (testMIR input.core.project checks)
  lowerUnchecked input := .ok (testMIR input.project checks)
  lowerMemory _ := .error (.invalidInput rule)
  buildSSA _ := .error (.invalidInput rule)
  buildBIR _ := .error (.invalidInput rule)
  emitLLVM bir := .ok ⟨testProfile.llvmTriple, testProfile.llvmDataLayout, [], bir.sites⟩
  emitLCB _ := .ok testOutput

private structure FixtureEntries (profile : Profile) where
  lexer : Stage Source LexRefusal TokenStream
  parser : Stage TokenStream ParseRefusal (Parsed (grammar.value grammar.start))
  admission : Stage (AdmissionInput testLanguage) AdmissionFailure (Admitted testLanguage)
  lowerUnchecked : Stage (CoreInput testLanguage) StageFailure (MIR testLowering profile)
  emitLLVM : Stage (BIR testLowering profile) StageFailure (LLVMInput profile)
  emitLCB : Stage (BIR testLowering profile) StageFailure (LCBInput profile)

-- A fixed parser entry also fixes the entire refusal, including its rule and span.
example {count : Nat} {g : SharedGrammar count} (engine : ParserEngine g) (stream : TokenStream)
    (first second : ParseRefusal) (h₁ : (parserEntry engine).refusal stream first)
    (h₂ : (parserEntry engine).refusal stream second) : first = second :=
  Except.error.inj (Option.some.inj (h₁.1.symm.trans h₂.1))

private def fixtureEntries {profile : Profile} (parser : Parser grammar)
    (passes : PassFunctions grammar testLanguage testLowering profile) : FixtureEntries profile :=
  ⟨passes.lexer, parserEntry parser.engine, (passes.admissionEntry early).toStage, passes.lowerUncheckedEntry,
    passes.emitLLVMEntry, passes.emitLCBEntry⟩

private abbrev r {profile : Profile} (_m : LoweringModels testLanguage testLowering profile)
    (parser : Parser grammar) (checks : List OperationCheck := []) :=
  fixtureEntries parser (testPasses (profile := profile) checks)

private def tokenStream (radix : Surface0.Radix)
    (slice : Slice := early) : TokenStream :=
  ⟨⟨tabProject, ⟨0⟩, slice⟩, #[⟨⟨span, .integer 42 radix none⟩, "a".toUTF8⟩], #[#[], #[]]⟩

private def testParser (verifier : TableVerifier grammar) (size : proposal.size = verifier.words)
    (accept : (verifier.run proposal).2 = some true) (stepsPerToken : Nat := 4) : Parser grammar :=
  ⟨table, verifier, proposal, size, accept, by
    intro i
    have hi : i.val < grammar.terminalColumns := by
      simpa [table, cells, Array.set!_eq_setIfInBounds] using i.isLt
    have fits : terminalColumn (some .integer) < grammar.terminalColumns := by decide
    change ∃ word, proposal[i.val]? = some word ∧ cells[i.val]? = _
    simp only [proposal, cells, Array.set!_eq_setIfInBounds, Array.getElem?_setIfInBounds,
      Array.getElem?_replicate, Array.size_replicate, fits, hi, ↓reduceIte]
    split <;> first | exact ⟨1, rfl, rfl⟩ | exact ⟨0, rfl, rfl⟩, stepsPerToken, rule, rule⟩

section
variable (verifier : TableVerifier grammar) (size : proposal.size = verifier.words)
  (accept : (verifier.run proposal).2 = some true)

local notation "parser" => testParser verifier size accept

-- The parser sees all decoded payloads without consulting the raw source.
example (source : Source) :
    let view := TokenStream.parserView ⟨source, #[⟨⟨span, .stringLiteral [65, 0]⟩, ByteArray.empty⟩,
      ⟨⟨span, .byteLiteral 255⟩, ByteArray.empty⟩,
      ⟨⟨span, .label ⟨"next"⟩⟩, ByteArray.empty⟩], #[#[], #[], #[], #[]]⟩
    view.tokens[0]?.map Located.value = some (.stringLiteral [65, 0]) ∧
      view.tokens[1]?.map Located.value = some (.byteLiteral 255) ∧
      view.tokens[2]?.map Located.value = some (.label ⟨"next"⟩) :=
  ⟨rfl, rfl, rfl⟩

private def spacedStream : TokenStream :=
  let source : Source := ⟨Project.singleton mainSourcePath (SourceText.ofBytes [52, 50, 32, 13, 10]), ⟨0⟩, early⟩
  ⟨source, #[⟨⟨{span with endOffset := 2}, .integer 42 .decimal none⟩, "42".toUTF8⟩],
    #[#[], #[⟨{span with startOffset := 2, endOffset := 5, col := 3}, " \r\n".toUTF8⟩]]⟩

-- The mutant retains the same token and the same number of trivia groups.
-- Only reconstruction of the retained pieces can distinguish it.
example :
    let dropped := {spacedStream with trivia := #[#[], #[]]}
    let mutant := ({testPasses (profile := testProfile) [] with
      lex := fun _ => .ok dropped}).lexer
    dropped.parserView = spacedStream.parserView ∧
    mutant.success spacedStream.source spacedStream ∧
    ¬ mutant.Validated := by
  refine ⟨rfl, ⟨rfl, rfl, _, rfl, _, rfl, rfl, _, rfl, rfl⟩, ?_⟩
  intro validated
  obtain ⟨_, _, file, lookup, bytes, rebuilt, exactBytes, _⟩ :=
    (validated.1 spacedStream.source _ rfl rfl).2
  have short : some "42".toUTF8 = some bytes := rebuilt
  cases Option.some.inj short
  have found : some (File.load mainSourcePath (SourceText.ofBytes [52, 50, 32, 13, 10])) = some file := lookup
  obtain rfl := Option.some.inj found
  exact (show ¬ ([52, 50] : List UInt8) = [52, 50, 32, 13, 10] by decide) exactBytes

-- The retained bytes and coordinates are identical. Only the reported value changes.
example :
    let wrong := {spacedStream with tokens :=
      #[⟨⟨{span with endOffset := 2}, .integer 43 .decimal none⟩, "42".toUTF8⟩]}
    let mutant := ({testPasses (profile := testProfile) [] with
      lex := fun _ => .ok wrong}).lexer
    reconstruct wrong.source.entryFile wrong.tokens wrong.trivia =
      reconstruct spacedStream.source.entryFile spacedStream.tokens spacedStream.trivia ∧
    wrong.parserView.WF ∧ ¬ mutant.Validated := by
  refine ⟨rfl, rfl, ?_⟩
  intro valid
  obtain ⟨_, _, _, _, bytes, restored, _, reference, scanned, pieces⟩ :=
    (valid.1 spacedStream.source _ rfl rfl).2
  obtain rfl := Option.some.inj (show some "42 \r\n".toUTF8 = some bytes from restored)
  have expected : Concrete.Lex.lexLossless "42 \r\n".toUTF8 ⟨0⟩ = .ok _ := rfl
  obtain rfl := Except.ok.inj (expected.symm.trans scanned)
  cases pieces

-- A fabricated AST from the registered parser's success channel fails the exact run check.
example (m : LoweringModels testLanguage testLowering testProfile) :
    ¬ (r m parser).parser.success (tokenStream .decimal)
      ⟨(tokenStream .decimal).source, .inr false⟩ := by
  intro h
  have run := congrArg (fun x => x.map (fun y => y.toOption.map Parsed.tree)) h.1
  have wrong : true = false := Sum.inr.inj (Option.some.inj (Option.some.inj run))
  cases wrong

-- The same registry returns 42's AST for decimal, refuses hexadecimal at Slice 0,
-- and accepts the hexadecimal token when that production's spelling is available.
example (m : LoweringModels testLanguage testLowering testProfile) :
    (r m parser).parser.run (tokenStream .decimal) =
      .ok ⟨(tokenStream .decimal).source, .inr true⟩ ∧
    (r m parser).parser.run (tokenStream .hexadecimal) =
      .error (.unsupported rule span later) ∧
    (r m parser).parser.run (tokenStream .hexadecimal later) =
      .ok ⟨(tokenStream .hexadecimal later).source, .inr true⟩ :=
  ⟨rfl, rfl, rfl⟩

example (m : LoweringModels testLanguage testLowering testProfile) :
    (r m parser).parser.success (tokenStream .decimal)
      ⟨(tokenStream .decimal).source, .inr true⟩ := by
  refine ⟨rfl, ⟨rfl, 3, ?_, ?_, by change 3 ≤ 5; decide, rfl⟩, by change 5 ≤ 8; decide⟩
  · exact Derives.apply (g := grammar) production _ _
      (List.mem_cons_self ..) (Nat.le_refl _) (.token rfl (by change 0 ≤ 0; decide) .nil)
  · exact Predictive.apply (table := table) production 0 _ [] _
      rfl rfl (Nat.le_refl _) (.token rfl (by change 0 ≤ 0; decide) (.nil []))

example (m : LoweringModels testLanguage testLowering testProfile) :
    (r m parser).parser.refusal (tokenStream .hexadecimal) (.unsupported rule span later) := by
  refine ⟨rfl, ⟨by change 0 < 2; decide, ⟨.inr true, 3, Derives.apply (g := grammar) production _ _
    (List.mem_cons_self ..) (by decide) (.token rfl (by change 2 ≤ 2; decide) .nil)⟩, ?_⟩, by change 5 ≤ 8; decide⟩
  intro earlier tooEarly
  rintro ⟨tree, steps, derives⟩
  have required := Derives.tokensEligible derives _ (List.mem_cons_self ..)
  exact Nat.not_lt_of_ge required tooEarly

-- A correct AST does not make a run with more than the permitted transitions sound.
-- The machine spends five transitions; a two-step-per-token profile permits four.
example (m : LoweringModels testLanguage testLowering testProfile) :
    ¬ (r m (testParser verifier size accept 2)).parser.Checks := by
  intro sound
  have h := sound.1.1 (tokenStream .decimal)
    ⟨(tokenStream .decimal).source, .inr true⟩ rfl rfl
  exact (show ¬ 5 ≤ 4 from by decide) h.2.2.2

private def literalSourceText (n : Nat) := "fn main() -> u8 { return " ++ toString n ++ "; }"

private def literalSource (n : Nat) : Source :=
  ⟨Project.singleton mainSourcePath (SourceText.ofBytes (literalSourceText n).toUTF8.data.toList),
    ⟨0⟩, early⟩

private def elaborationSourceText := literalSourceText 42
private def elaborationSource := literalSource 42

private def entryNameSpan : Surface0.Span := ⟨3, 7, 1, 4, ⟨0⟩⟩
private def resultSpan : Surface0.Span := ⟨25, 27, 1, 26, ⟨0⟩⟩

private def literalProgram (n : Nat) : Surface0.Program :=
  ⟨"main", entryNameSpan, [.ret (.integer n none resultSpan) ⟨18, 28, 1, 19, ⟨0⟩⟩]⟩

private abbrev returnProgram := literalProgram 42

private def returnSyntax (n : Nat) : SyntaxTree :=
  .node .program [.leaf ⟨⟨⟨0⟩, 3, 7, 1, 4⟩, .name "main"⟩, .node .body [
    .node .return_ [.leaf ⟨⟨⟨0⟩, 18, 24, 1, 19⟩, .fixed .return_ rfl⟩,
      .leaf ⟨⟨⟨0⟩, 25, 27, 1, 26⟩, .integer n .decimal none⟩,
      .leaf ⟨⟨⟨0⟩, 27, 28, 1, 28⟩, .fixed .semi rfl⟩]]]

private def parsedReturn : Parsed (SyntaxTree ⊕ Bool) :=
  ⟨elaborationSource, .inl (returnSyntax 42)⟩

private abbrev earlyFixtureLanguage := sliceLanguage slice0

private def returnViewCore (n : Nat) := AdmissionView.zero ⟨[], .lit n, ⟨18, 28, 1, 19, ⟨0⟩⟩, resultSpan⟩

private def elaboratedReturn (n : Nat) : CoreInput earlyFixtureLanguage :=
  ⟨elaborationSource.project, early, .straightLine (returnViewCore n) rfl⟩

private def pendingLiteral (n : Nat) : AdmissionInput earlyFixtureLanguage :=
  .pending elaborationSource.project (.zero (literalProgram n)) rfl

private abbrev pendingReturn := pendingLiteral 42

private abbrev fixtureElaborate := IntegratedCore.elaborate slice0

private theorem representsReturn (n : Nat) : (returnViewCore n).Represents (elaboratedReturn n) :=
  ⟨rfl, rfl, rfl⟩

private theorem typedReturn : (elaboratedReturn 42).StaticallyTyped :=
  ⟨_, representsReturn 42, ⟨by decide, .ret (.int rfl)⟩⟩

set_option maxRecDepth 2048 in
private theorem returnedCoreWF (n : Nat) : (elaboratedReturn n).WF := by
  let core := elaboratedReturn n
  let origin : SiteId × Origin := ⟨⟨0⟩, ⟨⟨0⟩, ⟨⟨0⟩, 18, 28, 1, 19⟩,
    ⟨.function earlyEntry, ⟨[⟨.return, .ordinal 0⟩]⟩⟩⟩⟩
  refine ⟨⟨by change [SiteId.mk 0].Nodup; simp, by change [SiteId.mk 0].Nodup; simp,
    by change [origin.2.evidenceKey].Nodup; simp, ?_, ?_, ?_⟩, ?_, ?_⟩
  · intro site op member
    change (site, op) ∈ [(⟨0⟩, .expression (.inl (.read (.literal (.int .u8 n)))))] at member
    cases List.mem_singleton.mp member
    exact ⟨origin.2, rfl⟩
  · intro state events kind site reached trapped
    let start := core.machineProgram.initial
    let next (q : Exec.Config core.machineProgram.machine) :=
      match Exec.step core.machineProgram.body q with
      | some (.next result) => result
      | _ => q
    let returned := next start
    let unscoped := next returned
    let finished := next unscoped
    have peel (current successor : Exec.Config core.machineProgram.machine)
        (dispatch : Exec.step core.machineProgram.body current = some (.next successor))
        (nontrap : current.final? ≠ some (.trapped kind site)) :
        ∀ trace, Exec.Steps core.machineProgram.body current trace state →
          ∃ rest, Exec.Steps core.machineProgram.body successor rest state := by
      intro trace path
      cases path with
      | refl => exact (nontrap trapped).elim
      | step tick rest =>
        cases tick with
        | silent tick =>
          rw [dispatch] at tick
          cases Option.some.inj tick
          exact ⟨_, rest⟩
        | prim tick _ _ => rw [dispatch] at tick; contradiction
    obtain ⟨_, reached⟩ := peel start returned (by with_unfolding_all rfl)
      (by with_unfolding_all exact nofun) _ reached
    obtain ⟨_, reached⟩ := peel returned unscoped (by with_unfolding_all rfl)
      (by with_unfolding_all exact nofun) _ reached
    obtain ⟨_, reached⟩ := peel unscoped finished (by with_unfolding_all rfl)
      (by with_unfolding_all exact nofun) _ reached
    have blocked (current final : Exec.Config core.machineProgram.machine)
        (stopped : Exec.step core.machineProgram.body current = none)
        (nontrap : current.final? ≠ some (.trapped kind site))
        (trace : List core.machineProgram.machine.Event)
        (path : Exec.Steps core.machineProgram.body current trace final) :
        final.final? ≠ some (.trapped kind site) := by
      cases path with
      | refl => exact nontrap
      | step tick _ =>
        cases tick with
        | silent tick => rw [stopped] at tick; contradiction
        | prim tick _ _ => rw [stopped] at tick; contradiction
    exact (blocked finished state (by with_unfolding_all rfl)
      (by with_unfolding_all exact nofun) _ reached trapped).elim
  · intro site found member
    change (site, found) ∈ [origin] at member
    have same := List.mem_singleton.mp member
    obtain rfl := congrArg Prod.fst same
    obtain rfl := congrArg Prod.snd same
    let specSpan : ConcreteSpec.Core.Span := ⟨⟨0⟩, 18, 28, by decide, 1, 19, by decide, by decide⟩
    exact ⟨rfl, specSpan, _, rfl, rfl, by decide, rfl, rfl, rfl, rfl⟩
  · intro site op member
    change (site, op) ∈ [(⟨0⟩, .expression (.inl (.read (.literal (.int .u8 n)))))] at member
    cases List.mem_singleton.mp member
    trivial
  · intro location member
    change location ∈ [resultSpan] at member
    cases List.mem_singleton.mp member
    exact ⟨⟨⟨0⟩, 25, 27, by decide, 1, 26, by decide, by decide⟩, _, rfl, rfl, by decide, rfl, rfl, rfl, rfl⟩

example :
    (Concrete.Lex.lex elaborationSourceText.toUTF8).bind
      (Concrete.Parse.parseForDriver (Concrete.Parse.eofSpan elaborationSourceText.toUTF8)) = .ok returnProgram :=
  rfl

private def returnView (n : Nat := 42) : ParserView :=
  let stop := 25 + (toString n).length
  let tok := fun start stop value => (⟨⟨⟨0⟩, start, stop, 1, start + 1⟩, value⟩ : SurfaceToken)
  ⟨literalSource n, #[
    tok 0 2 (.fixed .fn rfl), tok 3 7 (.name "main"), tok 7 8 (.fixed .lparen rfl),
    tok 8 9 (.fixed .rparen rfl), tok 10 12 (.fixed .arrow rfl), tok 13 15 (.fixed .u8 rfl),
    tok 16 17 (.fixed .lbrace rfl), tok 18 24 (.fixed .return_ rfl), tok 25 stop (.integer n .decimal none),
    tok stop (stop + 1) (.fixed .semi rfl), tok (stop + 2) (stop + 3) (.fixed .rbrace rfl)]⟩

private def returnStream (n : Nat := 42) : TokenStream :=
  let view := returnView n
  let bytes := (literalSourceText n).toUTF8
  let tokens := view.tokens.map fun decoded =>
    (⟨decoded, bytes.extract decoded.span.startOffset decoded.span.endOffset⟩ : RetainedToken)
  let trivia := view.tokens.toList.zipIdx.map fun (token, i) =>
    let stop := token.span.startOffset
    let start := if i = 0 then 0 else
      ((view.tokens[i - 1]?).map (fun previous => previous.span.endOffset)).getD 0
    if start = stop then #[] else
      #[⟨⟨⟨0⟩, start, stop, 1, start + 1⟩, bytes.extract start stop⟩]
  ⟨view.source, tokens, trivia.toArray.push #[]⟩

-- The verifier accepts the fixed proposal and rejects a changed prediction row.
set_option maxRecDepth 4096 in
example :
    let inputs := sliceVerifierInputs slice0
    let budget := (sliceGrammar slice0).verificationBudget
    let changed := inputs.proposal.set! (terminalColumn (some .fn)) 0
    (verifyRun inputs.program ⟨0, inputs.proposal⟩ budget).2 = some true ∧
    (verifyRun inputs.program ⟨0, changed⟩ budget).2 = some false :=
  ⟨(sliceVerifierInputs slice0).accepted, by with_unfolding_all rfl⟩

private def returnPasses (elaborate : Parsed (SyntaxTree ⊕ Bool) → Except ElabRefusal (AdmissionInput earlyFixtureLanguage)) :
    Passes slice0 testProfile where
  lex _ := .ok returnStream
  elaborate := elaborate
  checkCore := ConcreteSpec.Admission.checkInput
  lowerCore _ := .error (.invalidInput rule)
  lowerUnchecked _ := .error (.invalidInput rule)
  lowerMemory _ := .error (.invalidInput rule)
  buildSSA _ := .error (.invalidInput rule)
  buildBIR _ := .error (.invalidInput rule)
  emitLLVM _ := .error (.invalidInput rule)
  emitLCB _ := .error (.invalidInput rule)

set_option maxRecDepth 4096 in
private theorem parsedReturnDomain : Parsed.ElaborationDomain slice0 parsedReturn := by
  refine ⟨rfl, rfl, ⟨returnView, by with_unfolding_all rfl, by with_unfolding_all rfl⟩,
    ⟨.ok (pendingReturn), returnProgram, rfl, rfl⟩, ?_⟩
  intro payload matching
  obtain ⟨program, decoded, matching⟩ := matching
  have same : returnProgram = program := Option.some.inj (show some returnProgram = some program from decoded)
  subst program
  have eq : pendingReturn = payload := Except.ok.inj matching
  subst payload
  refine ⟨rfl, ?_⟩
  intro purpose
  cases purpose <;> exact returnedCoreWF 42

private theorem firstTokenConsumes {count : Nat} {g : SharedGrammar count} {slice : Slice}
    {rhs tokens args steps} (parsed : DerivesArgs g slice rhs tokens args steps) :
    (∃ terminal tail, rhs = .t terminal :: tail) → tokens ≠ [] := by
  cases parsed with
  | nil =>
    rintro ⟨_, _, impossible⟩
    cases impossible
  | token => exact fun _ => List.cons_ne_nil _ _
  | child =>
    rintro ⟨_, _, impossible⟩
    cases impossible

private theorem firstTokenDerivation {count : Nat} {g : SharedGrammar count} {slice : Slice}
    {n tokens tree steps} (parsed : Derives g slice n tokens tree steps)
    (first : ∀ p ∈ g.productions.toList, ∃ terminal tail, p.rhs = .t terminal :: tail) :
    tokens ≠ [] := by
  cases parsed with
  | apply p tokens args member _ parsed => exact firstTokenConsumes parsed (first p member)

-- A malformed stream has an admissible parser refusal. A fabricated tree
-- cannot be passed on to the elaborator as a successful parser output.
set_option maxRecDepth 4096 in
example :
    let edge := (registry (returnPasses fixtureElaborate)).parser
    let empty := {returnStream with tokens := #[]}
    edge.inputInvariant empty ∧
      ∃ failure, edge.run empty = .error failure ∧ edge.refusal empty failure := by
  dsimp only
  let failure : ParseRefusal := .malformed (sliceParser slice0).malformedRule ⟨⟨0⟩, 0, 0, 1, 1⟩
  refine ⟨⟨by with_unfolding_all rfl, rfl⟩, failure, by with_unfolding_all rfl,
    by with_unfolding_all rfl, ?_, by with_unfolding_all decide⟩
  rintro ⟨tree, slice, steps, derives⟩
  have first (p : Production (sliceGrammar slice0).value)
      (member : p ∈ (sliceGrammar slice0).productions.toList) :
      ∃ terminal tail, p.rhs = .t terminal :: tail := by
    have same := List.mem_singleton.mp member
    subst p
    exact ⟨.fn, _, rfl⟩
  exact (firstTokenDerivation derives first) rfl

set_option maxRecDepth 4096 in
example :
    let registered := registry (returnPasses fixtureElaborate)
    registered.parser.Compatible registered.elaborator ∧
    ¬ registered.elaborator.inputInvariant {parsedReturn with tree := .inl (.node .body [])} ∧
    registered.elaborator.Compatible registered.eligibility ∧
    registered.eligibility.Compatible registered.typing ∧
    registered.typing.Compatible registered.lowerUnchecked := by
  refine ⟨fun _ h => h, ?_, fun _ h => h, fun _ h => h, fun _ h => h.2⟩
  rintro ⟨_, _, _, ⟨result, matching⟩, _⟩
  obtain ⟨_, impossible, _⟩ := matching
  cases impossible

set_option maxRecDepth 4096 in
example :
    let registered := registry (returnPasses fixtureElaborate)
    registered.typing.outputInvariant (elaboratedReturn 42) := by
  exact ⟨rfl, (ConcreteSpec.Admission.checkStatic_iff (returnedCoreWF 42)).1 rfl⟩

set_option maxRecDepth 4096 in
private theorem return256IllTyped : ¬ (elaboratedReturn 256).StaticallyTyped := by
  rintro ⟨v, represented, _, typed⟩
  cases v.unique represented (representsReturn 256)
  cases typed with
  | ret atom => cases atom with | int fits => cases fits

set_option maxRecDepth 4096 in
example :
    let failure : StageFailure := .invalidInput ⟨``ConcreteSpec.Core.Slice0Rules.LiteralTyped, 1⟩
    let passes := returnPasses fixtureElaborate
    let edge := (registry passes).typing
    edge.inputInvariant (elaboratedReturn 256) ∧
    edge.run (elaboratedReturn 256) = .error failure ∧
    edge.refusal (elaboratedReturn 256) failure ∧
    ¬ edge.refusal (elaboratedReturn 42) failure := by
  refine ⟨⟨rfl, returnedCoreWF 256, ⟨_, representsReturn 256⟩⟩,
    rfl, ⟨_, rfl, rfl⟩, ?_⟩
  rintro ⟨_, h, _⟩
  cases h

-- Exercise the exact registered parse and elaboration calls before the unchecked
-- typing refusal. A literal-only checker suffices for this fixture.
set_option maxRecDepth 4096 in
example :
    let failure : StageFailure := .invalidInput ⟨``ConcreteSpec.Core.Slice0Rules.LiteralTyped, 1⟩
    let passes := {returnPasses fixtureElaborate with lex := fun _ => .ok (returnStream 256)}
    ((registry passes).lcbPipeline true).run (literalSource 256) = .error ⟨StageFailure, failure⟩ := by
  with_unfolding_all rfl

-- The unchecked type judgment permits a defined arithmetic trap.
set_option maxRecDepth 4096 in
example :
    let program : EarlyProgram :=
      ⟨[⟨⟨⟨0⟩, .add (.lit 255) (.lit 1)⟩, resultSpan, resultSpan, (resultSpan, resultSpan), .unnamed none⟩], .var ⟨0⟩, resultSpan, resultSpan⟩
    let core : CoreInput earlyFixtureLanguage :=
      ⟨elaborationSource.project, slice0,  .straightLine (.zero program) rfl⟩
    let typing := (registry (returnPasses fixtureElaborate)).typing
    typing.run core = .ok core ∧
    ConcreteSpec.run (AdmissionView.zero program).program 3 = .trap .arithmeticOverflow ∧
    ∀ failure, ¬ typing.refusal core failure := by
  refine ⟨rfl, rfl, ?_⟩
  rintro _ ⟨_, impossible, _⟩
  cases impossible

set_option maxRecDepth 4096 in
private theorem fixtureElaboratorValidated : (registry (returnPasses fixtureElaborate)).elaborator.Validated :=
  IntegratedCore.validated _ rfl

set_option maxRecDepth 4096 in
private theorem wrongLiteralElaboratorInvalid (passes : Passes slice0 testProfile)
    (wrong : passes.elaborate parsedReturn = .ok (pendingLiteral 43)) :
    ¬ (registry passes).elaborator.Validated := by
  intro validated
  have matching := (validated.1 parsedReturn _ parsedReturnDomain wrong).2
  change SliceElaboration slice0 elaborationSource.project (.inl (returnSyntax 42))
    (.ok (pendingLiteral 43)) at matching
  obtain ⟨program, decoded, matching⟩ := matching
  obtain rfl := Option.some.inj (show some returnProgram = some program from decoded)
  cases matching

set_option maxRecDepth 4096 in
private theorem returnLexerContract :
    Lexes elaborationSource returnStream := by
  refine ⟨rfl, by with_unfolding_all rfl, _, rfl, elaborationSourceText.toUTF8, ?_, rfl, _, rfl, ?_⟩
  all_goals with_unfolding_all rfl

set_option maxRecDepth 4096 in
private theorem returnParserContract :
    (registry (returnPasses fixtureElaborate)).parser.success returnStream parsedReturn := by
  let g := sliceGrammar slice0
  let p := g.productions[0]'(by with_unfolding_all decide)
  let ts := returnView.tokens
  let args : ActionArgs g.value p.rhs :=
    ⟨ts[0]'(by decide), ts[1]'(by decide), ts[2]'(by decide), ts[3]'(by decide),
      ts[4]'(by decide), ts[5]'(by decide), ts[6]'(by decide), ts[7]'(by decide),
      ts[8]'(by decide), ts[9]'(by decide), ts[10]'(by decide), ()⟩
  let steps := 11 + 1 + (p.action.eval args).1
  refine ⟨by with_unfolding_all rfl, ⟨rfl, steps, ?_, ?_, by with_unfolding_all decide,
    returnLexerContract.2.1⟩, by with_unfolding_all decide⟩
  · with_unfolding_all
      exact Derives.apply p ts.toList args (List.mem_singleton_self _) (by decide)
        (.token rfl (by decide) (.token rfl (by decide) (.token rfl (by decide)
        (.token rfl (by decide) (.token rfl (by decide) (.token rfl (by decide)
        (.token rfl (by decide) (.token rfl (by decide) (.token rfl (by decide)
        (.token rfl (by decide) (.token rfl (by decide) .nil)))))))))))
  · with_unfolding_all
      exact Predictive.apply p 0 ts.toList [] args rfl rfl (by decide)
        (.token rfl (by decide) (.token rfl (by decide) (.token rfl (by decide)
        (.token rfl (by decide) (.token rfl (by decide) (.token rfl (by decide)
        (.token rfl (by decide) (.token rfl (by decide) (.token rfl (by decide)
        (.token rfl (by decide) (.token rfl (by decide) (.nil []))))))))))))

-- Read the literal and locations from the grammar's token leaves. The registered
-- elaborator consumes the AST returned by the same checked parser.
set_option maxRecDepth 4096 in
example :
    let good := registry (returnPasses fixtureElaborate)
    let mutant := registry (returnPasses (fun _ => .ok (pendingLiteral 43)))
    let refusing := registry (returnPasses (fun _ => .error (.diagnostic rule span [])))
    good.lexer.success elaborationSource returnStream ∧
    good.lexer.run elaborationSource = .ok returnStream ∧
    good.parser.run returnStream = .ok parsedReturn ∧
    good.elaborator.run parsedReturn = .ok pendingReturn ∧
    good.elaborator.success parsedReturn pendingReturn ∧
    ¬ mutant.elaborator.Validated ∧ ¬ refusing.elaborator.Validated := by
  refine ⟨?_, rfl, by with_unfolding_all rfl, rfl,
    ⟨returnProgram, rfl, rfl⟩, ?_, ?_⟩
  · exact returnLexerContract
  · exact wrongLiteralElaboratorInvalid _ rfl
  · intro validated
    have matching := validated.2 parsedReturn _ parsedReturnDomain rfl
    change SliceElaboration slice0 elaborationSource.project (.inl (returnSyntax 42)) (.error (.diagnostic rule span [])) at matching
    obtain ⟨program, decoded, matching⟩ := matching
    obtain rfl := Option.some.inj (show some returnProgram = some program from decoded)
    cases matching

-- The spec admits the reachable return program with no operation discharges.
-- The same fixed judgment rules out an unconditional checker refusal.
set_option maxRecDepth 2048 in
private theorem returnAdmissionContract :
    Admits (elaboratedReturn 42) ⟨elaboratedReturn 42, []⟩ ∧
    ∀ failure, ¬ (elaboratedReturn 42).CheckerRefuses failure := by
  have fragment := representsReturn 42
  have accepted : (elaboratedReturn 42).Accepted := by
    refine ⟨(ConcreteSpec.Admission.checkStatic_iff (returnedCoreWF 42)).1 rfl, _, fragment, ?_⟩
    exact (ConcreteSpec.check_iff _).mp rfl
  refine ⟨⟨rfl, accepted, rfl, by simp, ?_, ?_⟩, ?_⟩
  · simp
  · rintro _ _ ⟨_, h, _⟩
    simp at h
  · exact fun _ refusal => refusal.1 accepted

private def corruptTokenStream (location : FileSpan) : TokenStream :=
  {tokenStream .decimal with tokens := #[⟨⟨location, .integer 42 .decimal none⟩, "a".toUTF8⟩]}

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
example (g : SharedGrammar 1) : g.terminalColumns ≤ Surface0.Terminal.count + 1 := by
  apply Nat.le_of_not_gt
  intro oversized
  have room : Surface0.Terminal.count < g.terminalSlices.size := by
    change Surface0.Terminal.count + 1 < g.terminalSlices.size + 1 at oversized
    omega
  obtain ⟨kind, index⟩ := g.terminalSliceExact ⟨_, room⟩
  exact (Nat.ne_of_lt (Surface0.Terminal.index_lt_count kind)) index

-- An accepting callback could hide work. Here even a loop with no changing
-- memory reaches the limit without producing a verifier verdict.
example : verifyRun #[.ifEqual 0 0 0 0] ⟨0, #[0]⟩ 8 = (8, none) := rfl

-- The branch goes directly to a raw operation. An operation in a different block cannot cover it.
example (project : Project) (m : LoweringModels testLanguage testLowering testProfile) :
    ¬ (r m parser).lowerUnchecked.success (testCore project) (testMIR project []) := by
  rintro ⟨_, lowered⟩
  have consumes := lowered.2.2.2.1
  have covers := consumes.2.2.1
  let op : MIR.Occurrence Unit := ⟨Exec.Demo.mainF, ⟨1⟩, 0, testOrigin, .operator (.arithmetic (.plain .div)), [((), .literal [0])], []⟩
  obtain ⟨conditions, ⟨sourceOp, member, _, _, fromSource⟩, _, fromMIR, _⟩ :=
    covers op (List.mem_cons_self ..)
  have eq : sourceOp = .expression () := by
    simpa [op, testOrigin, testCore, CoreInput.machineProgram, Payload.execution, MachineProgram.operations, stmtOperations] using member
  subst sourceOp
  rw [fromSource] at fromMIR
  contradiction

private def residual : OperationCheck := ⟨0, .divisionByZero, .residual⟩

-- The exact reads and check for division cannot justify a subtraction.
example (project : Project) (m : LoweringModels testLanguage testLowering testProfile) :
    ¬ (r m parser).lowerUnchecked.success (testCore project)
      (testMIR project [residual] (.arithmetic (.plain .sub))) := by
  rintro ⟨_, lowered⟩
  let op : MIR.Occurrence Unit := ⟨Exec.Demo.mainF, ⟨1⟩, 0, testOrigin,
    .operator (.arithmetic (.plain .sub)), [((), .literal [0])], [residual]⟩
  obtain ⟨_, ⟨sourceOp, member, _, opcode, _⟩, _⟩ :=
    lowered.2.2.2.1.2.2.1 op (List.mem_cons_self ..)
  have eq : sourceOp = .expression () := by
    simpa [op, testOrigin, testCore, CoreInput.machineProgram, Payload.execution, MachineProgram.operations, stmtOperations] using member
  subst sourceOp
  exact (show testLanguage.opcode (.expression ()) ≠ op.opcode from by decide) opcode

-- A separate instruction using the call's origin and reads cannot guard the call.
example (project : Project) (m : LoweringModels testLanguage testLowering testProfile) :
    ¬ (r m parser).lowerUnchecked.success
      (testCore project (.call none Exec.Demo.mainF [()] testSite))
      (testMIR project [residual] (.arithmetic (.plain .div)) true) := by
  rintro ⟨_, lowered⟩
  let call : MIR.Occurrence Unit := ⟨Exec.Demo.mainF, ⟨1⟩, 1, testOrigin,
    .call Exec.Demo.mainF, [((), .literal [0])], []⟩
  have mem : call ∈ (testMIR project [residual] (.arithmetic (.plain .div)) true
    (profile := testProfile)).operations := List.mem_cons_of_mem _ (List.mem_cons_self ..)
  obtain ⟨_, ⟨sourceOp, member, _, _, fromSource⟩, _, fromMIR, _⟩ :=
    lowered.2.2.2.1.2.2.1 call mem
  have eq : sourceOp = .call Exec.Demo.mainF [()] := by
    simpa [call, testOrigin, testCore, CoreInput.machineProgram, Payload.execution, MachineProgram.operations, stmtOperations] using member
  subst sourceOp
  rw [fromSource] at fromMIR
  contradiction

-- The program itself reaches an explicit trap after its table has been removed.
example :
    let original := testCore tabProject (.abort testSite)
    let core := {original with payload := .integrated {original.machineProgram with origins := []}}
    (∃ state, Exec.Steps core.machineProgram.body core.machineProgram.initial [] state ∧
      state.final? = some (.trapped .abort testSite)) ∧ ¬ core.LocationsWF := by
  dsimp only
  let original := testCore tabProject (.abort testSite)
  let core := {original with payload := .integrated {original.machineProgram with origins := []}}
  let trapped : Exec.Config core.machineProgram.machine := {core.machineProgram.initial with ctrl := .trapped .abort testSite}
  have steps : Exec.Steps core.machineProgram.body core.machineProgram.initial [] trapped :=
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
      ⟨project, Exec.Demo.mainF, [], [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[block]⟩)], [], []⟩
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
        [], [(Exec.Demo.mainF, [(⟨0⟩, .virtual)])], [], []⟩
    let ssa : SSA testLowering testProfile :=
      ⟨project, Exec.Demo.mainF, [],
        [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[⟨⟨0⟩, [], [],
          .ret (some ⟨(), .literal ⟨[42]⟩⟩), [testOrigin]⟩]⟩)], [], [], []⟩
    PreservesOrigins memory.origins ssa.origins :=
  ⟨fun _ h => h, fun _ h => h⟩

-- The registered checker claims a discharge, but the matching reachable Core operation traps.
example (project : Project) (m : LoweringModels testLanguage testLowering testProfile) :
    ¬ (r m parser).admission.success (.core (testCore project)) ⟨testCore project, unsafeDischarges⟩ := by
  rintro ⟨_, _, prepared, success⟩
  cases Except.ok.inj prepared
  have valid : ValidDischarges (testCore project) unsafeDischarges := success.2.2
  have member : unsafeDischarges.Contains testSite unsafeDischarge :=
    ⟨[unsafeDischarge], List.mem_cons_self .., List.mem_cons_self ..⟩
  have safety := (valid.2.2.2 testSite unsafeDischarge member).2.2.2.2.2
  let core := testCore project
  let start := core.machineProgram.initial
  let trapped : Exec.Config core.machineProgram.machine := {start with ctrl := .trapped .divisionByZero testSite}
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
  expressionOperator atomic := if atomic then .read else divOpcode
  primitiveOperator p := nomatch p
  argumentRead atomic := if atomic then some ((), .copy argPlace.root []) else none
  argumentReadNoTrap := by
    intro expr _ _ locals state kind
    cases expr
    · intro h; contradiction
    · cases h : AList.lookup locals argTemp <;> simp [h]
  OtherValue := Empty
  valueShape := rfl
  layout := fun _ _ _ => some ⟨1, 1, .nil⟩
  scalarType := fun _ => some (.int .u8)
  literal := fun _ bytes => bytes.head?.map fun byte => .scalar (.int .u8 (Int.ofNat byte.toNat))
  typeDecidableEq := inferInstance
  fieldType := fun _ _ => none
  activeFields := fun _ _ _ => none
  operation := fun _ _ _ _ _ _ _ _ _ _ _ => False
  checkTrap := fun _ _ _ => false
  field := fun _ _ => none
  replaceField := fun _ _ _ => none
  encodeOther := fun _ _ other => nomatch other
  decodeOther := fun _ _ _ => none
  safety
    | .expression false => [⟨0, .divisionByZero, divReads⟩]
    | _ => []
  permit := fun _ _ _ own => some own

private def argLowering : LoweringLanguage argLanguage.operands.Ty :=
  ⟨fun _ _ _ => Unit, ()⟩

private def argCore (project : Project) (body : Exec.Stmt (argLanguage.instrumented [])) : CoreInput argLanguage :=
  ⟨project, early, .integrated ⟨[], [], ⟨[(Exec.Demo.mainF, ⟨[], (), ⟨0⟩, body⟩)]⟩,
    Exec.Demo.mainF, [(testSite, testOrigin), (callSite, callOrigin)]⟩⟩

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
    ⟨⟨0⟩, #[⟨⟨0⟩, [], [⟨testOrigin, div⟩, ⟨callOrigin, call⟩], .ret none, []⟩]⟩)], [], []⟩

private def argDivOccurrence : MIR.Occurrence argLanguage.operands.Ty :=
  ⟨Exec.Demo.mainF, ⟨0⟩, 0, testOrigin, .operator divOpcode, divReads, [argCheck]⟩

private def argCallOccurrence : MIR.Occurrence argLanguage.operands.Ty :=
  ⟨Exec.Demo.mainF, ⟨0⟩, 1, callOrigin, .call callee, [((), .copy argPlace.root [])], []⟩

private theorem argDivInventory (project : Project) :
    (checkedArgCore project).machineProgram.ConditionsAt testSite (.operator divOpcode)
      [⟨0, .divisionByZero, divReads⟩] :=
  ⟨.expression false, List.mem_cons_self .., trivial, rfl, rfl⟩

private theorem argCallInventory (project : Project) :
    (checkedArgCore project).machineProgram.ConditionsAt callSite (.call callee) [] :=
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
    [(Exec.Demo.mainF, [(storeRoot, .scalar 1 1)])], [(Exec.Demo.mainF, [(storeRoot, .materialized)])],
    [(Exec.Demo.mainF, [storeRoot])], []⟩

private def storeInitial : MemoryRegisters testLanguage testProfile :=
  ⟨⟨Exec.Demo.mainF, ⟨0⟩, 0, [], [], [], [], [], [], []⟩, Mem.Memory.empty, [], [], {}, 0, false⟩

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
  exact ⟨rfl, ⟨⟨1, 1, .nil⟩, rfl, Or.inr ⟨by decide, ⟨0⟩, 1, ⟨0⟩, storeBefore.bytes, rfl, rfl⟩⟩⟩

private theorem storingWrite (project : Project) :
    (storingMemory project).Writes (language := testLanguage) storeBefore storePlace storeValue
      {storeAfter with instruction := 1} := by
  refine ⟨⟨rfl, fun _ => rfl⟩, (fun _ => ⟨⟨1, 1, .nil⟩, rfl, by decide⟩), rfl, rfl, fun _ => Or.inr ⟨storeRange, [(0, [.init 42 none])], rfl, rfl, ?_, ?_⟩⟩
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
    ¬ MemorySimulationWitness m (testMIR project []) (storingMemory project) := by
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
    [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[⟨⟨0⟩, [], [], .ret (some ⟨(), .literal ⟨[42]⟩⟩), []⟩]⟩)], [], []⟩

private def returningMemory (project : Project) : Memory testLowering testProfile :=
  ⟨project, Exec.Demo.mainF, (returningMIR project).signatures,
    [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[⟨⟨0⟩, [], [], .ret (some ⟨(), .literal ⟨[0]⟩⟩), []⟩]⟩)],
    [], [], [], []⟩

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

private theorem terminalRefines {A B : Type} {a : A} {b : B} {o runtime}
    {same : o.SameRuntimeResult runtime} :
    (terminalModel A o runtime same).Refines (terminalModel B o runtime same) a b := by
  intro _
  exact ⟨fun _ _ _ h => ⟨(), ⟨(), trivial, .nil ()⟩, h, rfl⟩,
    fun _ h => (h.2 0).elim, fun _ h => h.2.2 runtime rfl⟩

private def falseReturnModels (m : LoweringModels testLanguage testLowering testProfile) :
    LoweringModels testLanguage testLowering testProfile :=
  {m with
    mir := returnsStatus _ 42
    memory := returnsStatus _ 42
    memoryState := fun _ => storeInitial
    mirState := fun _ => storeInitial}

-- The current return instruction really yields zero.
set_option maxRecDepth 2048 in
example (project : Project) :
    (returningMemory project).terminalObservation storeInitial =
      some (.returned 0 ByteArray.empty) := by with_unfolding_all rfl

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
    ¬ MemorySimulationWitness (falseReturnModels m) (returningMIR project) (returningMemory project) := by
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
    ¬ MemorySimulationWitness m (returningMIR project) (returningMemory project) := by
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
    [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[caller]⟩), (callee, [], ⟨⟨0⟩, #[helper]⟩)], [], [], [], []⟩

private def resumeAt (index : Nat) : MemoryRegisters testLanguage testProfile :=
  {storeInitial with instruction := index}

private def helperReturning (index : Nat) : MemoryRegisters testLanguage testProfile :=
  {storeInitial with frame := callee, callers := [⟨(resumeAt index).toMemoryFrame, none⟩]}

-- Both calls use the same callee PC and byte memory. Each return pops its own
-- continuation and the second return cannot resume the first call's successor.
set_option maxRecDepth 2048 in
example (project : Project) :
    let memory := twoCallMemory project
    memory.Step storeInitial (helperReturning 1) ∧
    memory.Step (helperReturning 1) (resumeAt 1) ∧
    memory.Step (resumeAt 1) (helperReturning 2) ∧
    memory.Step (helperReturning 2) (resumeAt 2) ∧
    (¬ memory.Step (helperReturning 2) (resumeAt 1)) ∧
    memory.terminalObservation (resumeAt 2) = some (.returned 0 ByteArray.empty) := by
  dsimp only
  refine ⟨?_, ?_, ?_, ?_, ?_, by with_unfolding_all rfl⟩
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
    layout := fun _ _ _ => some ⟨0, 1, .nil⟩
    scalarType := fun _ => none
    literal := fun _ _ => some (.scalar .unit)}

private def zeroMemory (project : Project) : Memory testLowering testProfile :=
  let block : MemoryBlock testLowering := ⟨⟨0⟩, [],
    [⟨testOrigin, .beginLifetime storeRoot⟩, ⟨testOrigin, .bind storePlace (.literal ⟨[]⟩)⟩,
      ⟨testOrigin, .endLifetime storeRoot⟩], .ret none, []⟩
  {(storingMemory project) with
    functions := [(Exec.Demo.mainF, [(storeRoot, ())], ⟨⟨0⟩, #[block]⟩)]
    layouts := [(Exec.Demo.mainF, [(storeRoot, ⟨0, 1, .nil⟩)])]}

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
      ⟨by decide, Or.inr ⟨rfl, ⟨0, 1, .nil⟩, rfl, Or.inl ⟨rfl, rfl⟩⟩⟩, rfl⟩
  · refine ⟨_, rfl, _, zeroStarted, {zeroBound with instruction := 1}, rfl, ?_, rfl⟩
    exact ⟨⟨rfl, nofun⟩, (fun _ => ⟨⟨0, 1, .nil⟩, rfl, by decide⟩), rfl, rfl,
      fun _ => Or.inl rfl⟩
  · exact ⟨_, rfl, {storeInitial with instruction := 2},
      ⟨⟨by decide, rfl⟩, Or.inr ⟨rfl, Or.inl ⟨⟨0, 1, .nil⟩, rfl, rfl, rfl⟩⟩⟩, rfl⟩

private def splitField : FieldId := ⟨.struct ⟨Exec.Demo.mainF.module, ⟨"record"⟩⟩, ⟨"item"⟩⟩

private abbrev splitLanguage : CoreLanguage :=
  {zeroLanguage with
    fieldType := fun _ _ => some ()
    activeFields := fun _ _ _ => some [splitField]}

-- Destructuring a linear field cannot erase it or create two copies.
example :
    Concrete.Interfaces.Memory.DestructureFields splitLanguage [] () (.scalar .unit) [(splitField, ())] ∧
    ¬ Concrete.Interfaces.Memory.DestructureFields splitLanguage [] () (.scalar .unit) [] ∧
    ¬ Concrete.Interfaces.Memory.DestructureFields splitLanguage [] () (.scalar .unit)
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
  ⟨project, Exec.Demo.mainF, [], [(Exec.Demo.mainF, [(storeRoot, ())], block)], [], []⟩

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
    ¬ MemorySimulationWitness m (shadowMIR project) (shadowMemory project) := by
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
    (∃ state, Exec.Steps core.machineProgram.body core.machineProgram.initial [] state ∧
      state.final? = some (.trapped .abort testSite)) ∧
    ¬ ValidDischarges core [(testSite, [abortEvidence])] := by
  dsimp only
  refine ⟨⟨_, .step (l := .silent) (.silent rfl) (.refl _), rfl⟩, ?_⟩
  intro valid
  obtain ⟨_, _, ⟨op, member, eligible, _, _⟩, _⟩ :=
    (valid.2.2.2 testSite abortEvidence
      ⟨_, List.mem_cons_self .., List.mem_cons_self ..⟩).2.2.2.1
  have abort : op = .abort := by
    simpa [testCore, CoreInput.machineProgram, Payload.execution, MachineProgram.operations, stmtOperations] using member
  cases abort
  exact eligible

-- A fabricated effect cannot accompany a silent Exec transition in the registered Core model.
example (m : LoweringModels testLanguage testLowering testProfile) (project : Project)
    (before after : m.core.model.execution.State)
    (h : m.core.model.execution.step (testCore project) before after)
    (silent : Exec.step (testCore project).machineProgram.body (m.core.config (testCore project) before) =
      some (.next (m.core.config (testCore project) after))) :
    ¬ m.core.specTrace (testCore project) after =
      m.core.specTrace (testCore project) before ++ [show testLanguage.operands.Event from (7 : Nat)] := by
  intro fabricated
  obtain ⟨label, step, exactTrace⟩ := m.core.stepsExact _ _ _ h
  cases step with
  | silent _ => simp [Exec.Label.events, exactTrace] at fabricated
  | prim request _ _ => rw [silent] at request; contradiction

-- Even a self-consistent downstream model cannot remap these terminal statuses.
example (m : LoweringModels testLanguage testLowering testProfile) (project : Project)
    (state : m.core.model.execution.State)
    (finished : (m.core.config (testCore project) state).final? =
      some (.returned (.scalar (.int .u8 42)))) :
    ¬ m.core.model.result (testCore project) state
      (.returned 0 ByteArray.empty) := by
  intro result
  obtain ⟨outcome, final, projected⟩ := (m.core.terminalExact ..).mp result
  rw [finished] at final
  cases final
  have statuses := congrArg (Option.map (fun o => match o with
    | .returned status _ => status
    | _ => 255)) projected
  exact (show some (42 : UInt8) ≠ some 0 from by decide) statuses

example (m : LoweringModels testLanguage testLowering testProfile) (project : Project)
    (state : m.core.model.execution.State)
    (finished : (m.core.config (testCore project) state).final? =
      some (.trapped .divisionByZero testSite)) (stderr : ByteArray) :
    ¬ m.core.model.result (testCore project) state
      (.trapped 0 .divisionByZero span stderr) := by
  intro result
  obtain ⟨outcome, final, projected⟩ := (m.core.terminalExact ..).mp result
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
    ¬ (r m parser).emitLLVM.Checks := by
  intro sound
  have success := (sound.1.1 (testBIR project)
    ⟨testProfile.llvmTriple, testProfile.llvmDataLayout, [], #[]⟩ eligible rfl).2
  exact (show testProfile.llvmTriple ≠ macLLVM.llvmTriple from by decide) success.2.1

example (project : Project) (m : LoweringModels testLanguage testLowering macLLVM) :
    ¬ (r m parser).emitLLVM.success (testBIR project)
      ⟨macLLVM.llvmTriple, testProfile.llvmDataLayout, [], #[]⟩ := by
  intro success
  exact (show testProfile.llvmDataLayout ≠ macLLVM.llvmDataLayout from by decide) success.2.2.1

-- Changing the module body while preserving its metadata leaves the exact
-- printer invocation under P_print. Neither input gains a semantic judgment
-- from the structural handoff check.
example (project : Project) (m : LoweringModels testLanguage testLowering macLLVM) :
    let ir (status : Nat) : LLVMInput macLLVM :=
      ⟨macLLVM.llvmTriple, macLLVM.llvmDataLayout,
        [.define false (.integer .u32) .body [], .label .entry,
          .terminator (.ret (.integer .u32, .immediate ⟨status⟩)), .endFunction], #[]⟩
    (r m parser).emitLLVM.kind = .namedPremise "P_print" ∧
    LLVMMetadataMatches (testBIR project) (ir 0) ∧
    LLVMMetadataMatches (testBIR project) (ir 42) ∧
    (ir 0).moduleBytes.data.toList ≠ (ir 42).moduleBytes.data.toList := by
  refine ⟨rfl, ⟨rfl, rfl, rfl, rfl⟩, ⟨rfl, rfl, rfl, rfl⟩, ?_⟩
  decide

-- Four BIR definitions exceed the registered profile's three-slot ceiling.
example (project : Project) (m : LoweringModels testLanguage testLowering testProfile) :
    let block : Block _ _ _ := ⟨⟨0⟩, [⟨(), ⟨0⟩⟩, ⟨(), ⟨1⟩⟩, ⟨(), ⟨2⟩⟩, ⟨(), ⟨3⟩⟩], [], .ret none, []⟩
    let bir : BIR testLowering testProfile :=
      { testBIR project (profile := testProfile) with functions := [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[block]⟩)] }
    ¬ (r m parser).emitLCB.inputInvariant bir := by
  dsimp
  intro eligible
  exact (show ¬ 4 ≤ 3 from by decide) eligible.2.1

-- Validate the exact output of the candidate emitter before publishing its job.
example (project : Project) (m : LoweringModels testLanguage testLowering testProfile) :
    ∃ reason, (r m parser).emitLCB.run (testBIR project) = .error (.invalidLCBJob reason) :=
  ⟨_, by with_unfolding_all rfl⟩

end


private def projectedStorePlace : MemoryPlace Unit () :=
  ⟨⟨storeRoot, [splitField]⟩, .materialized⟩

private abbrev projectedLanguage : CoreLanguage :=
  {testLanguage with
    fieldType := fun _ _ => some ()
    layout := fun _ _ _ => some ⟨1, 1, .cons splitField 0 (.scalar 1 1) .nil⟩}

private abbrev linearProjectedLanguage : CoreLanguage :=
  {projectedLanguage with
    operands := {projectedLanguage.operands with isCopy := fun _ => false}
    safety := fun _ => []}

-- A projected write can replace an initialized field. It cannot revive a
-- consumed root, even when the physical bytes of the moved value remain.
example (project : Project) :
    let memory := {storingMemory project with layouts :=
      [(Exec.Demo.mainF, [(storeRoot, ⟨1, 1, .cons splitField 0 (.scalar 1 1) .nil⟩)])]}
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
  exact ⟨rfl, ⟨⟨rfl, fun _ => rfl⟩, (fun _ => ⟨⟨0, 1, .nil⟩, rfl, by decide⟩), rfl,
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
  ⟨(), storeRoot, [], some range, ⟨0⟩, .bytes⟩

-- Two valid placements of the same initialized value provide different
-- pointers to the operation. Substituting the first pointer for the second
-- fails the address-operand premise even though the stored bytes agree.
example (project : Project) :
    let memory := storingMemory project
    let address := ⟨(), storePlace⟩
    memory.resolveAddress (Memory.prepareAddresses storeAfter [address]) false address = some (resolvedStore storeRange) ∧
    memory.resolveAddress (Memory.prepareAddresses alternateBacking [address]) false address = some (resolvedStore alternateRange) ∧
    memory.resolveAddress (Memory.prepareAddresses alternateBacking [address]) false address ≠ some (resolvedStore storeRange) := by
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
    [(Exec.Demo.mainF, [(storeRoot, ())], ⟨⟨0⟩, #[block]⟩)], [], []⟩

private def residualMemory (project : Project) (checks : List OperationCheck) :
    Memory testLowering testProfile :=
  let block : MemoryBlock testLowering := ⟨⟨0⟩, [],
    [⟨testOrigin, .op (opcode := .arithmetic (.plain .div)) (types := [()]) (ty := ())
      ⟨⟨storeRoot, []⟩, .virtual⟩ ()
      (.cons (.literal ⟨[0]⟩) .nil) checks⟩], .trap .abort testOrigin, []⟩
  ⟨project, Exec.Demo.mainF, (residualMIR project).signatures,
    [(Exec.Demo.mainF, [(storeRoot, ())], ⟨⟨0⟩, #[block]⟩)], [],
    [(Exec.Demo.mainF, [(storeRoot, .virtual)])], [], []⟩

private abbrev ownerFixtureLowering := sliceLoweringLanguage slice0
private def ownerFixtureType : earlyFixtureLanguage.operands.Ty := .inl (.scalar (.int .u8))

private def ownerFixtureMIR (consuming mutable : Bool) : MIR ownerFixtureLowering testProfile :=
  let place : Place _ ownerFixtureType := ⟨storeRoot, []⟩
  let read : Read _ ownerFixtureLowering.literal ownerFixtureType :=
    if consuming then .move place else .copy place
  let block : Block _ _ _ := ⟨⟨0⟩, [],
    [⟨testOrigin, .bind place (.literal ⟨[42]⟩)⟩], .ret (some ⟨ownerFixtureType, read⟩), []⟩
  ⟨tabProject, earlyEntry, [(earlyEntry, ⟨[], some ownerFixtureType⟩)],
    [(earlyEntry, [(storeRoot, ownerFixtureType)], ⟨⟨0⟩, #[block]⟩)],
    if mutable then [(earlyEntry, [storeRoot])] else [], []⟩

private def ownerFixtureStart : MemoryRegisters earlyFixtureLanguage testProfile :=
  {frame := earlyEntry, block := ⟨0⟩, instruction := 0, virtualValues := [],
    materialized := [], live := [], initialized := [], bytes := Mem.Memory.empty}

private def ownerFixtureBound : MemoryRegisters earlyFixtureLanguage testProfile :=
  {ownerFixtureStart with
    instruction := 1
    live := [storeRoot]
    initialized := [storeRoot]
    virtualValues := [(storeRoot, .scalar (.int .u8 42))]}

set_option maxRecDepth 4096 in
example :
    (ownerFixtureMIR true false).Step ownerFixtureStart ownerFixtureBound ∧
    (ownerFixtureMIR false false).Step ownerFixtureStart ownerFixtureBound ∧
    (ownerFixtureMIR true false).terminalObservation ownerFixtureBound = none ∧
    (ownerFixtureMIR false false).terminalObservation ownerFixtureBound = some (.returned 42 ByteArray.empty) := by
  have binds (consuming : Bool) : (ownerFixtureMIR consuming false).Step ownerFixtureStart ownerFixtureBound := by
    with_unfolding_all
      refine ⟨rfl, _, ownerFixtureStart, {ownerFixtureBound with instruction := 0}, rfl, ?_, rfl⟩
      apply Memory.WriteParams.cons (started := {ownerFixtureStart with live := [storeRoot]})
      · exact ⟨nofun, Or.inl ⟨rfl, rfl⟩⟩
      · exact ⟨⟨rfl, fun _ => rfl⟩, nofun, rfl, rfl, nofun⟩
      · exact .nil _
  exact ⟨binds true, binds false, by with_unfolding_all rfl, by with_unfolding_all rfl⟩

set_option maxRecDepth 4096 in
example :
    let place : MemoryPlace _ ownerFixtureType := ⟨⟨storeRoot, []⟩, .virtual⟩
    let inst : MemoryInst ownerFixtureLowering := .assign place (.literal ⟨[25]⟩)
    let before := ownerFixtureBound
    let immutable := (ownerFixtureMIR false false).logicalView
    let mutable := (ownerFixtureMIR false true).logicalView
    (∀ next, ¬ immutable.InstructionStep before next false inst) ∧
      ∃ next, mutable.InstructionStep before next false inst ∧
        next.virtualValues = [(storeRoot, .scalar (.int .u8 25))] := by
  dsimp only
  constructor
  · rintro next ⟨value, after, stored, read, write, _⟩
    have result : some (CoreValue.scalar (.int .u8 25), ownerFixtureBound) = some (value, after) := by
      with_unfolding_all exact read
    cases Option.some.inj result
    have impossible : none = some stored := by with_unfolding_all exact write.2.2.1
    cases impossible
  · with_unfolding_all
      exact ⟨_, ⟨_, _, _, rfl, ⟨⟨rfl, fun _ => rfl⟩, nofun, rfl, rfl, nofun⟩, rfl⟩, rfl⟩

-- Check the same decisions with an explicitly instrumented owner and no loans.
set_option maxRecDepth 4096 in
example :
    let memory := (ownerFixtureMIR false false).logicalView
    let place : MemoryPlace _ ownerFixtureType := ⟨⟨storeRoot, []⟩, .virtual⟩
    let bound (mutable : Bool) : MemoryRegisters earlyFixtureLanguage testProfile :=
      {ownerFixtureBound with
        addressIds := [(storeRoot, ⟨0⟩)]
        nextAddressId := 1
        ownership :=
          {slots := [⟨⟨0⟩, 0, ⟨.int .u8, true, mutable⟩, .owned⟩]
           frames := [⟨0, [(⟨0⟩, ⟨0⟩)], []⟩]
           nextPlace := 1
           nextActivation := 1}}
    memory.readValue (bound false) (.move place) = none ∧
      (memory.readValue (bound false) (.copy place)).isSome = true ∧
      memory.writeValue (bound false) place (.scalar (.int .u8 25)) = none ∧
      (memory.writeValue (bound true) place (.scalar (.int .u8 25))).isSome = true := by
  with_unfolding_all exact ⟨rfl, rfl, rfl, rfl⟩

private def retainedOwnerCore : CoreInput earlyFixtureLanguage :=
  ⟨elaborationSource.project, early,  .straightLine (.zero
    ⟨[⟨⟨⟨storeRoot.id⟩, .atom (.lit 42)⟩, resultSpan, resultSpan, (), .unnamed none⟩], .var ⟨storeRoot.id⟩, resultSpan, resultSpan⟩) rfl⟩

private def retainedOwnerMIR (substituted : Bool) : MIR ownerFixtureLowering testProfile :=
  let dst : Place _ ownerFixtureType := ⟨storeRoot, []⟩
  let origin (site : Nat) := (AList.lookup retainedOwnerCore.machineProgram.origins ⟨site⟩).getD testOrigin
  let binding : LocatedOp (MIRInst ownerFixtureLowering) := ⟨origin 0, .bind dst (.literal ⟨[42]⟩)⟩
  let assignment : LocatedOp (MIRInst ownerFixtureLowering) := ⟨origin 0, .assign dst (.literal ⟨[42]⟩)⟩
  let block : Block _ _ _ := ⟨⟨0⟩, [], binding :: (if substituted then [assignment] else []),
    .ret (some ⟨ownerFixtureType, .copy dst⟩), [origin 1]⟩
  ⟨retainedOwnerCore.project, earlyEntry, [(earlyEntry, ⟨[], some ownerFixtureType⟩)],
    [(earlyEntry, [(storeRoot, ownerFixtureType)], ⟨⟨0⟩, #[block]⟩)],
    if substituted then [(earlyEntry, [storeRoot])] else [], []⟩

set_option maxRecDepth 4096 in
private theorem retainedOwnerAccepted :
    PreservesOwnerMutability retainedOwnerCore (retainedOwnerMIR false) := by
  refine ⟨?_, nofun⟩
  intro fn locals graph member root ty localMember
  obtain ⟨fnEq, contents⟩ := Prod.mk.inj (List.mem_singleton.mp member)
  have localsEq : locals = [(storeRoot, ownerFixtureType)] := congrArg Prod.fst contents
  subst locals
  have rootEq : root = storeRoot := congrArg Prod.fst (List.mem_singleton.mp localMember)
  rw [fnEq, rootEq]
  with_unfolding_all rfl

set_option maxRecDepth 4096 in
private theorem substitutedOwnerRefused :
    ¬ PreservesOwnerMutability retainedOwnerCore (retainedOwnerMIR true) := by
  intro h
  have incompatible := h.1 earlyEntry _ _ (List.mem_singleton_self _) storeRoot ownerFixtureType
    (List.mem_singleton_self _)
  with_unfolding_all change true = false at incompatible
  contradiction

set_option maxRecDepth 4096 in
private theorem substitutedOriginsPreserved :
    PreservesOrigins (List.map Prod.snd retainedOwnerCore.machineProgram.origins) (retainedOwnerMIR true).origins := by
  let bound := (AList.lookup retainedOwnerCore.machineProgram.origins ⟨0⟩).getD testOrigin
  let returned := (AList.lookup retainedOwnerCore.machineProgram.origins ⟨1⟩).getD testOrigin
  with_unfolding_all
    change PreservesOrigins [bound, returned] [bound, bound, returned]
  constructor
  · intro o member
    simp only [List.mem_cons, List.not_mem_nil, or_false] at member ⊢
    exact member.elim Or.inl id
  · intro o member
    simp only [List.mem_cons, List.not_mem_nil, or_false] at member ⊢
    exact member.elim Or.inl (fun h => Or.inr (Or.inr h))

set_option maxRecDepth 4096 in
private theorem substitutedOwnerAssigns :
    (retainedOwnerMIR true).Step ownerFixtureBound ownerFixtureBound.advance := by
  with_unfolding_all
    exact ⟨rfl, _, _, _, rfl, ⟨⟨rfl, fun _ => rfl⟩, nofun, rfl, rfl, nofun⟩, rfl⟩

set_option maxRecDepth 4096 in
private theorem ownerFixtureObservations (substituted : Bool) :
    let mir := retainedOwnerMIR substituted
    let done := if substituted then ownerFixtureBound.advance else ownerFixtureBound
    mir.terminalObservation done = some (.returned 42 ByteArray.empty) ∧
      (MIR.Execution (language := earlyFixtureLanguage)).trace mir done = [] := by
  cases substituted <;> with_unfolding_all exact ⟨rfl, rfl⟩

example :
    PreservesOwnerMutability retainedOwnerCore (retainedOwnerMIR false) ∧
    PreservesOrigins (List.map Prod.snd retainedOwnerCore.machineProgram.origins) (retainedOwnerMIR true).origins ∧
    (retainedOwnerMIR true).Step ownerFixtureBound ownerFixtureBound.advance ∧
    (retainedOwnerMIR true).terminalObservation ownerFixtureBound.advance =
      (retainedOwnerMIR false).terminalObservation ownerFixtureBound ∧
    (MIR.Execution (language := earlyFixtureLanguage)).trace
        (retainedOwnerMIR true) ownerFixtureBound.advance =
      (MIR.Execution (language := earlyFixtureLanguage)).trace
        (retainedOwnerMIR false) ownerFixtureBound ∧
    ¬ LowersUnchecked retainedOwnerCore (retainedOwnerMIR true) ∧
    ¬ LowersCore ⟨retainedOwnerCore, []⟩ (retainedOwnerMIR true) :=
  ⟨retainedOwnerAccepted, substitutedOriginsPreserved, substitutedOwnerAssigns,
    (ownerFixtureObservations true).1.trans (ownerFixtureObservations false).1.symm,
    (ownerFixtureObservations true).2.trans (ownerFixtureObservations false).2.symm,
    fun ⟨_, lowered⟩ => substitutedOwnerRefused lowered.2.2.2.2.1.1, fun h => substitutedOwnerRefused h.2.2.2.2.1.1⟩

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

private abbrev returnLowering := sliceLoweringLanguage slice0

private def returnType : earlyFixtureLanguage.operands.Ty := .inl (.scalar (.int .u8))

private def singleBlockGraph {P I T : Type} (ending : T) (origins : List Origin) : CFG P I T :=
  ⟨⟨0⟩, #[⟨⟨0⟩, [], [], ending, origins⟩]⟩

private def lowerLiteralReturn (core : CoreInput earlyFixtureLanguage) :
    Except StageFailure (MIR returnLowering testProfile) :=
  match core.machineProgram.operations with
  | [(site, .expression (.inl (.read (.literal (.int .u8 value)))))] =>
    match AList.lookup core.machineProgram.origins site with
    | some origin =>
      let graph : MIRGraph returnLowering :=
        singleBlockGraph (.ret (some ⟨returnType, .literal ⟨[UInt8.ofNat value.toNat]⟩⟩)) [origin]
      .ok ⟨core.project, core.machineProgram.entry, [(core.machineProgram.entry, ⟨[], some returnType⟩)],
        [(core.machineProgram.entry, [], graph)], [], []⟩
    | none => .error (.invalidInput rule)
  | _ => .error (.invalidInput rule)

private def memoryLiteral (memory : Memory returnLowering testProfile) :
    Except StageFailure (List UInt8 × List Origin) :=
  match AList.lookup memory.functions memory.entry with
  | some (_, ⟨_, #[block]⟩) =>
    match block.ending with
    | .ret (some ⟨_, .literal value⟩) => .ok (value.encoding, block.boundaryOrigins)
    | _ => .error (.invalidInput rule)
  | _ => .error (.invalidInput rule)

private def ssaLiteral (ssa : SSA returnLowering testProfile) :
    Except StageFailure (List UInt8 × List Origin) :=
  memoryLiteral (ssa.memoryView (language := earlyFixtureLanguage))

private def literalReturnPasses : Passes slice0 testProfile :=
  { returnPasses fixtureElaborate with
    checkCore := ConcreteSpec.Admission.checkInput
    lowerCore := fun input => lowerLiteralReturn input.core
    lowerUnchecked := lowerLiteralReturn
    lowerMemory := fun mir => .ok (mir.logicalView (language := earlyFixtureLanguage))
    buildSSA := fun memory => do
      let (bytes, origins) ← memoryLiteral memory
      let graph : SSAGraph returnLowering :=
        singleBlockGraph (.ret (some ⟨returnType, .literal ⟨bytes⟩⟩)) origins
      return ⟨memory.project, memory.entry, memory.signatures, [(memory.entry, [], graph)], memory.layouts, [], memory.declarations⟩
    buildBIR := fun ssa => do
      let (bytes, origins) ← ssaLiteral ssa
      let graph : BIRGraph returnLowering :=
        singleBlockGraph (.ret (some ⟨returnType, .literal ⟨bytes⟩⟩)) origins
      return ⟨ssa.project, ssa.entry, ssa.signatures, [(ssa.entry, [], graph)], ssa.layouts, #[], [], ssa.declarations⟩
    emitLCB := fun bir => do
      let (bytes, _) ← memoryLiteral (bir.memoryView (language := earlyFixtureLanguage))
      match bytes with
      | [status] => return lcbReturnJob (BitVec.ofNat 32 status.toNat)
      | _ => throw (.invalidInput rule) }

set_option maxRecDepth 4096 in
example :
    let edge := (registry literalReturnPasses).admission
    let core := elaboratedReturn 256
    edge.inputInvariant (.core core) ∧
      ∃ failure, edge.run (.core core) = .error failure ∧ edge.refusal (.core core) failure := by
  let reason := ConcreteSpec.Admission.Refusal.atPosition 0 (.literalOutOfRange .u8 256)
  refine ⟨⟨rfl, returnedCoreWF 256, _, representsReturn 256⟩, _, rfl, rfl, ?_, _, reason, representsReturn 256, rfl, rfl⟩
  exact fun accepted => return256IllTyped accepted.1.2.1

-- Compose both registered routes through LCB job validation, then observe the exact
-- emitted body with the fixed LCB semantics. This control exercises a single source.
set_option maxRecDepth 4096 in
example (unchecked : Bool) :
    (Registry.pipeline literalReturnPasses unchecked).run elaborationSource = .ok (lcbReturnJob 42) ∧
    ((registry literalReturnPasses).lcbPipeline unchecked).run elaborationSource =
      .ok (lcbReturnJob 42) ∧
    (LCBInput.Execution Empty elaborationSource.project).observes
      (lcbReturnJob 42) (.returned 42 ByteArray.empty) := by
  have ran : ((registry literalReturnPasses).lcbPipeline unchecked).run elaborationSource =
      .ok (lcbReturnJob 42) := by cases unchecked <;> with_unfolding_all rfl
  refine ⟨ran, ran, ?_⟩
  · with_unfolding_all exact ⟨_, ⟨_, rfl, .nil _⟩, rfl⟩

set_option maxRecDepth 4096 in
example (unchecked : Bool) :
    let mutant := { literalReturnPasses with
      lowerCore := fun _ => .error (.invalidInput rule)
      lowerUnchecked := fun _ => .error (.invalidInput rule) }
    ¬ ∃ output, (Registry.pipeline mutant unchecked).run elaborationSource = .ok output := by
  intro mutant
  have refused : (Registry.pipeline mutant unchecked).run elaborationSource =
      .error ⟨StageFailure, .invalidInput rule⟩ := by
    cases unchecked <;> with_unfolding_all rfl
  rintro ⟨_, ran⟩
  rw [refused] at ran
  cases ran

set_option maxRecDepth 4096 in
example (unchecked : Bool) :
    let mutant : Passes slice0 testProfile :=
      { literalReturnPasses with elaborate := fun _ => .ok (pendingLiteral 43) }
    (Registry.pipeline mutant unchecked).run elaborationSource = .ok (lcbReturnJob 43) ∧
    ¬ (registry mutant).elaborator.Validated := by
  refine ⟨?_, wrongLiteralElaboratorInvalid _ rfl⟩
  cases unchecked <;> with_unfolding_all rfl

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

private abbrev erasureLanguage : CoreLanguage :=
  {zeroLanguage with
    operands := testLanguage.operands
    safety := fun _ => []
    layout := fun _ _ _ => some ⟨1, 1, .nil⟩
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
    layouts := [(Exec.Demo.mainF, [(storeRoot, ⟨1, 1, .nil⟩)])]}

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
  refine Memory.WriteParams.cons (language := erasureLanguage)
    (started := {storeInitial with live := [storeRoot]}) ?_ ?_ (.nil _)
  · exact ⟨by decide, Or.inl ⟨rfl, rfl⟩⟩
  · exact ⟨⟨rfl, fun _ => rfl⟩, nofun, rfl, rfl, nofun⟩

private theorem erasureBindingUnique (project : Project)
    (next : MemoryRegisters erasureLanguage testProfile)
    (step : (erasureSource project).Step (language := erasureLanguage) storeInitial next) :
    next = erasureBound := by
  obtain ⟨_, value, afterRead, stored, read, writes, rfl⟩ := step
  have readExact : some (CoreValue.other (), storeInitial) = some (value, afterRead) := read
  cases readExact
  have params : ((erasureSource project).logicalView (language := erasureLanguage)).WriteParams (language := erasureLanguage)
      [⟨(), ⟨⟨storeRoot, []⟩, .virtual⟩⟩] [CoreValue.other ()] storeInitial stored false := by
    simp only [Bool.true_or, ite_true] at writes
    with_unfolding_all exact writes
  cases params with
  | cons started write rest =>
    cases rest
    rcases started.2 with ⟨_, rfl⟩ | ⟨impossible, _, _, _⟩
    · have written : some {erasureBound with instruction := 0} = some stored := write.2.2.1
      cases written
      rfl
    · have wrong : some StorageClass.virtual = some .materialized := impossible
      cases wrong

-- Retaining a materialized declaration, its layout and its origin metadata
-- cannot justify an immediate return with no live target local.
example (project : Project) (m : LoweringModels erasureLanguage testLowering testProfile)
    (initial : ∀ p, m.mir.execution.initial (erasureSource project) p →
      m.mirState p = storeInitial) :
    ¬ MemorySimulationWitness {m with memory := returnsStatus _ 0, memoryState := fun _ => storeInitial}
      (erasureSource project) (immediateReturn project) := by
  intro lowers
  have exactSource := lowers.2.2.2.2.2.2.2.1
  obtain ⟨_, sim, represents⟩ := lowers.2.2.2.2.2.2.2.2.2.2
  have hq : (returnsStatus (Memory testLowering testProfile) 0).execution.initial
      (immediateReturn project) () := trivial
  obtain ⟨start, starts, relation⟩ := sim.targetInitial () hq
  have sourceReachable : m.mir.execution.finite (erasureSource project) start :=
    ⟨start, starts, .nil _⟩
  have targetReachable : (returnsStatus (Memory testLowering testProfile) 0).execution.finite
      (immediateReturn project) () := ⟨(), hq, .nil _⟩
  obtain ⟨finish, path, result, _⟩ :=
    sim.targetFinish start () (.returned 0 ByteArray.empty) sourceReachable targetReachable relation rfl
  cases path with
  | nil =>
      have impossible := (exactSource.2.2.2 start _ sourceReachable).mp result
      rw [initial start starts] at impossible
      have absent : (erasureSource project).terminalObservation (language := erasureLanguage) storeInitial = none := by
        with_unfolding_all rfl
      rw [absent] at impossible
      cases impossible
  | @cons _ next _ tick _ =>
      have step := exactSource.2.1 start next sourceReachable tick.1
      rw [initial start starts] at step
      have physical := (represents next () ⟨start, starts, .cons tick.1 (.nil _)⟩
        targetReachable tick.2).1
      rw [erasureBindingUnique project _ step] at physical
      exact List.not_mem_nil (physical.2.2.1 storeRoot (List.mem_cons_self ..))

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
  intro tick
  have noOutcome (s : MemoryRegisters residualLanguage testProfile) (mode : Bool)
      (raw : (residualMemory project [runtimeCheck]).StepOutcome (language := residualLanguage) storeInitial s mode) : False := by
    obtain ⟨block, fetched, step⟩ := raw
    have equality : some _ = some block := fetched
    cases equality
    obtain ⟨values, state, _, _, _, _, _, read, checked, _⟩ := step
    have readExact : some ([.scalar (.int .u8 0)], storeInitial) = some (values, state) := read
    cases readExact
    have impossible : some TrapKind.divisionByZero = none := by with_unfolding_all exact checked
    contradiction
  exact noOutcome _ false (tick.normal (language := residualLanguage) fun s => noOutcome s true)

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
    MemorySimulationWitness (residualModels m project) (residualMIR project) (residualMemory project [runtimeCheck]) ∧
    ¬ MemorySimulationWitness (residualModels m project) (residualMIR project) (residualMemory project []) := by
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
      · exact ⟨rfl, rfl, ⟨_, rfl, rfl⟩, ⟨_, rfl, rfl⟩, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩
      · with_unfolding_all rfl
      · intro _ _ _
        with_unfolding_all exact ⟨fun h => congrArg some h.symm, fun h => (Option.some.inj h).symm⟩
    · refine ⟨⟨fun _ _ => ?_, fun _ _ => ?_⟩,
        fun _ _ _ h => h.elim, fun _ next _ step => (residualLogicalStops project next step).elim, ?_⟩
      · with_unfolding_all
          exact ⟨rfl, rfl, ⟨_, rfl, rfl⟩, ⟨_, rfl, rfl⟩, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩
      · with_unfolding_all rfl
      · intro _ _ _
        with_unfolding_all exact ⟨fun h => congrArg some h.symm, fun h => (Option.some.inj h).symm⟩
    · exact residualTables project
    · have view : (residualMIR project).logicalView (language := residualLanguage) =
          residualMemory project [runtimeCheck] := by
        with_unfolding_all rfl
      exact view.symm ▸ residualTables project
    · refine ⟨terminalRefines (same := residualSame project), ?_, ?_⟩
      · refine ⟨fun _ _ => True, fun _ _ => 0, fun _ _ => ⟨(), trivial, trivial⟩, ?_, ?_⟩
        · exact fun _ _ _ _ _ _ tick => tick.elim
        · exact fun p _ _ _ _ _ result => ⟨p, .nil _, result, rfl⟩
      · intro _ _ _ _ _
        refine ⟨?_, rfl, nofun, ⟨rfl, nofun⟩, List.nodup_nil, rfl, rfl⟩
        change (residualMemory project [runtimeCheck]).RepresentsActivation
          (language := residualLanguage) storeInitial.toMemoryFrame storeInitial.toMemoryFrame Mem.Memory.empty
        exact ⟨⟨⟨⟨nofun, by decide, nofun, nofun, nofun, nofun, nofun, nofun⟩,
          by decide, by decide, nofun⟩, rfl⟩, rfl, nofun, by decide, by decide, by decide, by decide,
          nofun, (fun _ => ⟨nofun, nofun⟩), nofun, nofun, nofun, nofun⟩
  · intro lowers
    have exactTarget := lowers.2.2.2.2.2.2.1.2.2.2 () (residualObserved project)
      ⟨(), trivial, .nil _⟩
    have impossible : none = some (residualObserved project) := exactTarget.mp rfl
    contradiction

private def abstractStore : OperationAddress Unit :=
  ⟨(), storeRoot, [], none, ⟨0⟩, .logical⟩

private def emptyStoreAddress : OperationAddress Unit :=
  ⟨(), storeRoot, [], none, ⟨0⟩, .zero⟩

private abbrev addressLanguage : CoreLanguage :=
  {testLanguage with
    operation := fun _ opcode _ _ args _ operands before result effects after =>
      match opcode with
      | .named operation =>
        if operation = ⟨"address"⟩ then
          ((∃ (range : Hybrid.Range), operands = [resolvedStore range] ∧
              result = CoreValue.scalar (.int .u8 range.ptr.addr)) ∨
            ((operands = [abstractStore] ∨ operands = [emptyStoreAddress]) ∧ result = CoreValue.other ())) ∧
          effects = [] ∧ after = before
        else
          operands = [] ∧ result = storeValue ∧ effects = [] ∧
          ((args = [CoreValue.other ()] ∧
            AList.lookup before.addressedValues ⟨0⟩ = some storeValue ∧
            after = {before with
              addressedValues := AList.update before.addressedValues ⟨0⟩ (fun _ => CoreValue.scalar (.int .u8 99))}) ∨
           ∃ ptr bytes, args = [CoreValue.scalar (.int .u8 ptr.addr)] ∧
             before.bytes.load testProfile.memoryTarget ptr 1 1 = some [.init 42 none] ∧
             before.bytes.store testProfile.memoryTarget ptr [.init 99 none] 1 = some bytes ∧
             after = {before with bytes := bytes})
      | _ => False
    representsValue := fun _ ranges _ logical physical =>
      match logical with
      | .scalar _ => logical = physical
      | .other () =>
        match AList.lookup ranges ⟨0⟩ with
        | some (some range) => physical = CoreValue.scalar (.int .u8 range.ptr.addr)
        | some none => physical = logical
        | none => False}

private def addressResultPlace : MemoryPlace Unit () :=
  ⟨⟨shadowRoot, []⟩, .virtual⟩

private def addressInstruction : MemoryInst testLowering :=
  .op (opcode := .named ⟨"address"⟩) (types := []) (ty := ()) addressResultPlace () .nil []
    [⟨(), storePlace⟩]

private def indirectInstruction : MemoryInst testLowering :=
  .op (opcode := .named ⟨"indirectReadWrite"⟩) (types := [()]) addressResultPlace ()
    (.cons (.copy addressResultPlace) .nil)

private def addressBlock : MemoryBlock testLowering :=
  ⟨⟨0⟩, [],
    [⟨testOrigin, .beginLifetime storeRoot⟩, ⟨testOrigin, .bind storePlace (.literal ⟨[42]⟩)⟩,
      ⟨testOrigin, .beginLifetime shadowRoot⟩, ⟨testOrigin, addressInstruction⟩,
      ⟨testOrigin, indirectInstruction⟩], .ret (some ⟨(), .copy addressResultPlace⟩), []⟩

private def addressMemory (project : Project) : Memory testLowering testProfile :=
  {(storingMemory project) with
    mutableLocals := [(Exec.Demo.mainF, [storeRoot, shadowRoot])]
    signatures := [(Exec.Demo.mainF, ⟨[], some ()⟩)]
    functions := [(Exec.Demo.mainF, [(storeRoot, ()), (shadowRoot, ())], ⟨⟨0⟩, #[addressBlock]⟩)]
    storage := [(Exec.Demo.mainF, [(storeRoot, .materialized), (shadowRoot, .virtual)])]}

private def addressReady (base : MemoryRegisters addressLanguage testProfile) :
    MemoryRegisters addressLanguage testProfile :=
  {base with instruction := 3, live := shadowRoot :: base.live}

private def addressWritten (base : MemoryRegisters addressLanguage testProfile) (address : Nat) :
    MemoryRegisters addressLanguage testProfile :=
  {(addressReady base) with
    initialized := shadowRoot :: base.initialized
    virtualValues := [(shadowRoot, .scalar (.int .u8 address))]
    addressIds := [(storeRoot, ⟨0⟩)]
    nextAddressId := 1}

/-- The fixture operation returns the address supplied by the instruction
judgment, so storage equality cannot make two allocations interchangeable. -/
private theorem addressSteps (project : Project) :
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
      rfl, rfl, rfl, ⟨Or.inl ⟨storeRange, rfl, rfl⟩, rfl, rfl⟩, rfl, ?_, rfl⟩
    exact ⟨⟨rfl, fun _ => rfl⟩, nofun, rfl, rfl, nofun⟩
  · refine ⟨[], _, [resolvedStore alternateRange], _, [], _, addressWritten alternateBacking 2,
      rfl, rfl, rfl, ⟨Or.inl ⟨alternateRange, rfl, rfl⟩, rfl, rfl⟩, rfl, ?_, rfl⟩
    exact ⟨⟨rfl, fun _ => rfl⟩, nofun, rfl, rfl, nofun⟩
  · rintro ⟨values, afterRead, addresses, result, effects, changed, stored,
      reads, _, resolved, operates, _, writes, next⟩
    have readEq : some ([], addressReady alternateBacking) = some (values, afterRead) := reads
    cases readEq
    have resolvedEq : some [resolvedStore alternateRange] = some addresses := resolved
    cases resolvedEq
    have meaning : ((∃ range : Hybrid.Range, [resolvedStore alternateRange] = [resolvedStore range] ∧
          result = CoreValue.scalar (.int .u8 range.ptr.addr)) ∨
        (([resolvedStore alternateRange] = [abstractStore] ∨
          [resolvedStore alternateRange] = [emptyStoreAddress]) ∧ result = CoreValue.other ())) ∧
        effects = [] ∧ changed = Memory.operationStore
          (Memory.prepareAddresses (addressReady alternateBacking) [⟨(), storePlace⟩]) false := by
      with_unfolding_all exact operates
    rcases meaning with ⟨⟨range, operands, value⟩ | ⟨impossible, _⟩, _, unchanged⟩
    · have same : alternateRange = range := by
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
    · rcases impossible with wrong | wrong
      all_goals
        have range := congrArg OperationAddress.range (List.cons.inj wrong).1
        cases range

private def logicalAddressProgram (project : Project) : MIR testLowering testProfile :=
  let address : MIRInst testLowering :=
    .op (opcode := .named ⟨"address"⟩) (ty := ()) ⟨shadowRoot, []⟩ () .nil [] [⟨(), ⟨storeRoot, []⟩⟩]
  let indirect : MIRInst testLowering :=
    .op (opcode := .named ⟨"indirectReadWrite"⟩) (types := [()]) (ty := ()) ⟨shadowRoot, []⟩ ()
      (.cons (.copy ⟨shadowRoot, []⟩) .nil) []
  ⟨project, Exec.Demo.mainF, (addressMemory project).signatures,
    [(Exec.Demo.mainF, [(storeRoot, ()), (shadowRoot, ())], ⟨⟨0⟩, #[⟨⟨0⟩, [],
      [⟨testOrigin, .bind (ty := ()) ⟨storeRoot, []⟩ (.literal ⟨[42]⟩)⟩,
        ⟨testOrigin, address⟩, ⟨testOrigin, indirect⟩], .ret (some ⟨(), .copy ⟨shadowRoot, []⟩⟩), []⟩]⟩)],
    [(Exec.Demo.mainF, [shadowRoot])], []⟩

private def logicalAddressBefore : MemoryRegisters addressLanguage testProfile :=
  {storeInitial with
    instruction := 1, live := [storeRoot], initialized := [storeRoot]
    virtualValues := [(storeRoot, storeValue)]}

private def logicalAddressAfter : MemoryRegisters addressLanguage testProfile :=
  {logicalAddressBefore with
    instruction := 2, live := [shadowRoot, storeRoot], initialized := [shadowRoot, storeRoot]
    virtualValues := [(shadowRoot, .other ()), (storeRoot, storeValue)]
    addressIds := [(storeRoot, ⟨0⟩)], nextAddressId := 1}

-- One logical step serves both physical placements. A pointer to the other
-- placement fails the value relation even when the pointee bytes coincide.
private theorem logicalAddressStep (project : Project) :
    (logicalAddressProgram project).Step (language := addressLanguage)
      logicalAddressBefore logicalAddressAfter ∧
    addressLanguage.representsValue testProfile.memoryTarget [(⟨0⟩, some storeRange)] ()
      (CoreValue.other ()) (CoreValue.scalar (.int .u8 1)) ∧
    addressLanguage.representsValue testProfile.memoryTarget [(⟨0⟩, some alternateRange)] ()
      (CoreValue.other ()) (CoreValue.scalar (.int .u8 2)) ∧
    ¬ addressLanguage.representsValue testProfile.memoryTarget [(⟨0⟩, some alternateRange)] ()
      (CoreValue.other ()) (CoreValue.scalar (.int .u8 1)) := by
  refine ⟨?_, rfl, rfl, (show CoreValue.scalar (.int .u8 1) ≠ (CoreValue.scalar (.int .u8 2) : CoreValue Unit) from by intro h; cases h)⟩
  refine ⟨rfl, [], _, [abstractStore], CoreValue.other (), [], _, {logicalAddressAfter with instruction := 1},
    rfl, rfl, rfl, ⟨Or.inr ⟨Or.inl rfl, rfl⟩, rfl, rfl⟩, rfl, ?_, rfl⟩
  with_unfolding_all
    refine Memory.WriteParams.cons (language := addressLanguage)
      (started := {logicalAddressBefore with
        live := [shadowRoot, storeRoot]
        addressIds := [(storeRoot, ⟨0⟩)], nextAddressId := 1}) ?_ ?_ (.nil _)
  · exact ⟨by decide, Or.inl ⟨rfl, rfl⟩⟩
  · exact ⟨⟨rfl, fun _ => rfl⟩, nofun, rfl, rfl, nofun⟩

private def zeroAddressMemory (project : Project) : Memory testLowering testProfile :=
  {addressMemory project with layouts := (zeroMemory project).layouts}

private def zeroAddressReady : MemoryRegisters addressLanguage testProfile :=
  {storeInitial with
    live := [shadowRoot, storeRoot], initialized := [storeRoot], zeroValues := [(storeRoot, storeValue)]}

private def zeroAddressWritten : MemoryRegisters addressLanguage testProfile :=
  {zeroAddressReady with
    initialized := [shadowRoot, storeRoot]
    virtualValues := [(shadowRoot, CoreValue.other ())]
    addressIds := [(storeRoot, ⟨0⟩)]
    nextAddressId := 1}

example (project : Project) :
    let memory := zeroAddressMemory project
    let address : Sigma (MemoryPlace Unit) := ⟨(), storePlace⟩
    memory.resolveAddress (Memory.prepareAddresses zeroAddressReady [address]) false address =
      some emptyStoreAddress ∧
    memory.InstructionStep (language := addressLanguage) zeroAddressReady
      {zeroAddressReady with
        instruction := 1, initialized := [shadowRoot, storeRoot]
        virtualValues := [(shadowRoot, .other ())], addressIds := [(storeRoot, ⟨0⟩)], nextAddressId := 1}
      false addressInstruction := by
  refine ⟨rfl, ?_⟩
  refine ⟨[], _, [emptyStoreAddress], CoreValue.other (), [], _, zeroAddressWritten, rfl, rfl, rfl,
    ⟨Or.inr ⟨Or.inr rfl, rfl⟩, rfl, rfl⟩, rfl, ?_, rfl⟩
  exact ⟨⟨rfl, fun _ => rfl⟩, nofun, rfl, rfl, nofun⟩

private def suspendedAddress : MemoryRegisters addressLanguage testProfile :=
  {storeInitial with
    live := [shadowRoot], initialized := [shadowRoot], virtualValues := [(shadowRoot, .other ())]
    callers := [⟨logicalAddressAfter.toMemoryFrame, none⟩], nextAddressId := 1}

private def updatedSuspendedAddress : MemoryRegisters addressLanguage testProfile :=
  let caller := {logicalAddressAfter.toMemoryFrame with
    virtualValues := [(shadowRoot, .other ()), (storeRoot, .scalar (.int .u8 99))]}
  {suspendedAddress with
    instruction := 1, virtualValues := [(shadowRoot, storeValue)]
    callers := [⟨caller, none⟩]}

private def indirectWriteStore : OperationStore (CoreValue Unit) :=
  {Memory.operationStore (language := addressLanguage) suspendedAddress true with
    addressedValues := [(⟨0⟩, CoreValue.scalar (.int .u8 99))]}

-- Read an escaped value through a reference and update the suspended frame
-- that owns it. Plant a lowering that discards the indirect write.
example (project : Project) :
    let memory := (logicalAddressProgram project).logicalView (language := addressLanguage)
    memory.InstructionStep suspendedAddress updatedSuspendedAddress true indirectInstruction ∧
    ¬ memory.InstructionStep suspendedAddress
      {updatedSuspendedAddress with callers := suspendedAddress.callers} true indirectInstruction := by
  dsimp only
  constructor
  · refine ⟨[CoreValue.other ()], _, [], storeValue, [], _, {updatedSuspendedAddress with instruction := 0},
      rfl, rfl, rfl, ⟨rfl, rfl, rfl, Or.inl ⟨rfl, rfl, rfl⟩⟩, rfl, ?_, rfl⟩
    with_unfolding_all
      exact Memory.WriteParams.cons (language := addressLanguage) rfl
        ⟨⟨rfl, fun _ => rfl⟩, nofun, rfl, rfl, nofun⟩ (.nil _)
  · rintro ⟨values, afterRead, operands, result, effects, changed, stored, reads, _, addresses,
      operates, _, writes, next⟩
    have actualRead : some ([CoreValue.other ()], suspendedAddress) = some (values, afterRead) := by
      with_unfolding_all exact reads
    cases actualRead
    have resolved : some [] = some operands := addresses
    cases resolved
    have observed : ([] : List (OperationAddress Unit)) = [] ∧ result = storeValue ∧ effects = [] ∧
        (([CoreValue.other ()] = [CoreValue.other ()] ∧
          AList.lookup (Memory.operationStore suspendedAddress true).addressedValues ⟨0⟩ = some storeValue ∧
          changed = indirectWriteStore) ∨
          ∃ ptr bytes, [CoreValue.other ()] = [CoreValue.scalar (.int .u8 ptr.addr)] ∧
            suspendedAddress.bytes.load testProfile.memoryTarget ptr 1 1 = some [.init 42 none] ∧
            suspendedAddress.bytes.store testProfile.memoryTarget ptr [.init 99 none] 1 = some bytes ∧
            changed = {Memory.operationStore suspendedAddress true with bytes := bytes}) := by
      with_unfolding_all exact operates
    obtain ⟨_, rfl, rfl, meaning⟩ := observed
    rcases meaning with ⟨_, _, rfl⟩ | ⟨_, _, impossible, _⟩
    · have params : ((logicalAddressProgram project).logicalView (language := addressLanguage)).WriteParams (language := addressLanguage)
          [⟨(), addressResultPlace⟩] [storeValue] (Memory.applyOperation suspendedAddress true indirectWriteStore) stored false := by
        simp only [Bool.true_or, ite_true] at writes
        with_unfolding_all exact writes
      cases params with
      | cons same write rest =>
        cases rest
        have before : _ = Memory.applyOperation suspendedAddress true indirectWriteStore := same
        subst_vars
        have actualWrite : some {updatedSuspendedAddress with instruction := 0} = some stored := write.2.2.1
        cases actualWrite
        have callers := congrArg (fun state : MemoryRegisters addressLanguage testProfile => state.callers) next
        cases callers
    · cases impossible

private def suspendedPhysical : MemoryRegisters addressLanguage testProfile :=
  {storeInitial with
    live := [shadowRoot], initialized := [shadowRoot]
    virtualValues := [(shadowRoot, CoreValue.scalar (.int .u8 1))]
    bytes := storeAfter.bytes
    callers := [⟨(addressWritten storeAfter 1).toMemoryFrame, none⟩], nextAddressId := 1}

private def updatedSuspendedPhysical : MemoryRegisters addressLanguage testProfile :=
  {suspendedPhysical with
    virtualValues := [(shadowRoot, storeValue)]
    bytes := ⟨[(⟨0⟩, {storeBlock with bytes := [.init 99 none]})], 1⟩}

example (project : Project) :
    (addressMemory project).InstructionStep (language := addressLanguage)
      suspendedPhysical updatedSuspendedPhysical.advance false indirectInstruction ∧
    (Memory.operationStore (language := addressLanguage) updatedSuspendedAddress true).addressedValues =
      [(⟨0⟩, CoreValue.scalar (.int .u8 99))] ∧
    updatedSuspendedPhysical.bytes.load testProfile.memoryTarget storeRange.ptr 1 1 =
      some [.init 99 none] := by
  refine ⟨?_, rfl, rfl⟩
  refine ⟨[CoreValue.scalar (.int .u8 1)], _, [], storeValue, [], _, updatedSuspendedPhysical,
    rfl, rfl, rfl, ⟨rfl, rfl, rfl, Or.inr ⟨storeRange.ptr, _, rfl, rfl, rfl, rfl⟩⟩, rfl, ?_, rfl⟩
  exact ⟨⟨rfl, fun _ => rfl⟩, nofun, rfl, rfl, nofun⟩

private def logicalAddressFinal : MemoryRegisters addressLanguage testProfile :=
  {logicalAddressAfter with
    instruction := 3
    virtualValues := [(shadowRoot, storeValue), (storeRoot, CoreValue.scalar (.int .u8 99))]}

private def physicalAddressFinal (base : MemoryRegisters addressLanguage testProfile) (address : Nat)
    (bytes : Mem.Memory) : MemoryRegisters addressLanguage testProfile :=
  {addressWritten base address with instruction := 5, virtualValues := [(shadowRoot, storeValue)], bytes := bytes}

-- Give the wrong compiler the exact input graph and the other live allocation.
-- Its substituted result cannot be projected from a certified Memory step.
example (project : Project) (m : LoweringModels addressLanguage testLowering testProfile)
    (q next : m.memory.execution.State)
    (reachable : m.memory.execution.finite (addressMemory project) q)
    (step : m.memory.execution.step (addressMemory project) q next)
    (before : m.memoryState q = addressReady alternateBacking)
    (wrong : m.memoryState next = (addressWritten alternateBacking 1).advance) :
    ¬ MemorySimulationWitness m (logicalAddressProgram project) (addressMemory project) := by
  intro lowers
  have tick := lowers.2.2.2.2.2.2.1.2.1 q next reachable step
  rw [before, wrong] at tick
  obtain ⟨block, fetched, runs⟩ := tick
  have fetchedAddress := (addressSteps project).2.2
  have same : some _ = some block := fetched
  cases same
  exact fetchedAddress runs

private def birReturnBlock (alternate : Bool) :
    Block (Sigma (SSAValue Unit)) (LocatedOp (SSAInst testLowering))
      (BIRTerm (Sigma (SSARead Unit testLowering.literal)) (SSARead Unit testLowering.literal ())) :=
  let literal : UInt8 := if alternate then 42 else 0
  ⟨⟨0⟩, [], [], .ret (some ⟨(), .literal ⟨[literal]⟩⟩), []⟩

private def birReturn (project : Project) (alternate : Bool) : BIR testLowering testProfile :=
  {(testBIR project (profile := testProfile)) with
    functions := [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[birReturnBlock alternate]⟩)]}

private abbrev birExecution := BIR.Execution (language := testLanguage)
  (lang := testLowering) (profile := testProfile)

private theorem birReturnResult (project : Project) (alternate : Bool) :
    birExecution.result (birReturn project alternate) (BIR.entryState (birReturn project alternate))
      (.returned (if alternate then 42 else 0) ByteArray.empty) := by
  cases alternate <;> with_unfolding_all rfl

private theorem birReturnReachable (project : Project) (alternate : Bool) (state : birExecution.State)
    (finite : birExecution.finite (birReturn project alternate) state) :
    state = BIR.entryState (birReturn project alternate) := by
  obtain ⟨start, initial, steps⟩ := finite
  change start = BIR.entryState (birReturn project alternate) at initial
  subst start
  cases steps with
  | nil => rfl
  | cons step _ => exact (birExecution.terminal _ _ _ (birReturnResult project alternate) _ step).elim

private theorem mismatchLCB (project : Project) (alternate : Bool) :
    ¬ EmitsLCB testLanguage (birReturn project alternate)
      (lcbReturnJob (if alternate then 0 else 42)) := by
  intro emits
  let execution := LCBInput.Execution (profile := testProfile) Nat project
  have actual : execution.observes (lcbReturnJob (if alternate then 0 else 42))
      (.returned (if alternate then 0 else 42) ByteArray.empty) := by
    cases alternate <;> with_unfolding_all exact ⟨_, ⟨_, rfl, .nil _⟩, rfl⟩
  obtain ⟨q, finite, result⟩ := actual
  have safe : ProgramSafe birExecution (birReturn project alternate) := by
    intro state stuck
    have equal := birReturnReachable project alternate state stuck.1
    subst state
    exact stuck.2.2 _ (birReturnResult project alternate)
  obtain ⟨p, reachable, wrong, _⟩ := (emits.2.2.2.2.2.2.1 safe).1 q _ finite result
  rw [birReturnReachable project alternate p reachable] at wrong
  have correct := birReturnResult project alternate
  change BIR.hostedResult _ _ = some _ at wrong correct
  have status := congrArg (fun o : Concrete.Contracts.Observation =>
    match o with | .returned code _ => code | _ => 255)
    (Option.some.inj (correct.symm.trans wrong))
  cases alternate
  · exact (show (0 : UInt8) ≠ 42 from by decide) status
  · exact (show (42 : UInt8) ≠ 0 from by decide) status

-- Independently substitute the emitted job and the source graph.
example (project : Project) :
    (¬ EmitsLCB testLanguage (birReturn project false) (lcbReturnJob 42)) ∧
    (¬ EmitsLCB testLanguage (birReturn project true) (lcbReturnJob 0)) :=
  ⟨mismatchLCB project false, mismatchLCB project true⟩

set_option maxHeartbeats 1000000 in
example (project : Project) :
    EmitsLCB testLanguage (birReturn project false) (lcbReturnJob 0) := by
  refine ⟨rfl, by with_unfolding_all rfl, nofun, by simp [lcbReturnJob], rfl,
    ⟨[], rfl, rfl⟩, ?_, by simp [birReturn], by simp [lcbReturnJob], fun _ => Iff.rfl, by with_unfolding_all rfl, ?_⟩
  case refine_2 =>
    intro fn member block included slot trap
    cases List.mem_singleton.mp member
    have eq : block = birReturnBlock false := List.mem_singleton.mp included
    subst block
    cases trap
  let execution := LCBInput.Execution (profile := testProfile) Nat project
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
  intro _
  refine ⟨?_, ?_, ?_⟩
  · intro q o finite observed
    obtain rfl := reachable q finite
    have matching : some (Concrete.Contracts.Observation.returned 0 ByteArray.empty) = some o := by
      with_unfolding_all exact observed
    have same := Option.some.inj matching
    subst o
    refine ⟨_, ⟨_, rfl, .nil _⟩, birReturnResult project false, ?_⟩
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

private theorem reachableTerminal {A E : Type} (s : Concrete.Contracts.Semantics A E)
    (a : A) (entry : s.State) (o : Concrete.Contracts.Observation)
    (initial : ∀ p, s.initial a p → p = entry) (terminal : s.result a entry o)
    (p : s.State) (reach : s.finite a p) : p = entry := by
  obtain ⟨start, starts, path⟩ := reach
  have atEntry := initial start starts
  subst start
  cases path with
  | nil => rfl
  | cons tick _ => exact (s.terminal a entry o terminal _ tick).elim

private theorem stationaryRefines {A B E O : Type} {a : A} {b : B}
    (s : Concrete.Contracts.Semantics A E) (t : Concrete.Contracts.Semantics B E)
    (sr : A → s.State → O → Prop) (tr : B → t.State → O → Prop)
    (p : s.State) (q : t.State) (terminal : Concrete.Contracts.Observation) (raw : O)
    (sInitial : ∀ x, s.initial a x ↔ x = p) (tInitial : ∀ x, t.initial b x ↔ x = q)
    (_sResult : s.result a p terminal) (tResult : t.result b q terminal)
    (sRaw : sr a p raw) (tRaw : ∀ o, tr b q o ↔ o = raw)
    (sameTrace : s.trace a p = t.trace b q) :
    BehaviorRefinement s t sr tr a b := by
  have ps : s.finite a p := ⟨p, (sInitial p).mpr rfl, .nil _⟩
  have onlyT := reachableTerminal t b q terminal (fun x => (tInitial x).mp) tResult
  intro _
  refine ⟨?_, ?_, ?_⟩
  · intro x o reach result
    obtain rfl := onlyT x reach
    obtain rfl := (tRaw o).mp result
    exact ⟨p, ps, sRaw, sameTrace⟩
  · intro run infinite
    exact (t.terminal b q terminal tResult _
      ((tInitial _).mp infinite.1 ▸ infinite.2 0)).elim
  · intro x stuck
    obtain rfl := onlyT x stuck.1
    exact stuck.2.2 terminal tResult

private theorem stationarySafe {A E : Type} (s : Concrete.Contracts.Semantics A E)
    (a : A) (q : s.State) (o : Concrete.Contracts.Observation)
    (initial : ∀ p, s.initial a p → p = q) (result : s.result a q o) :
    ProgramSafe s a := by
  intro state stuck
  obtain rfl := reachableTerminal s a q o initial result state stuck.1
  exact stuck.2.2 o result

private theorem stationaryRejects {A B E : Type} {a : A} {b : B}
    (s : Concrete.Contracts.Semantics A E) (t : Concrete.Contracts.Semantics B E)
    (tr : B → t.State → Concrete.Contracts.Observation → Prop) (p : s.State) (q : t.State) (expected actual : Concrete.Contracts.Observation)
    (initial : ∀ x, s.initial a x → x = p) (sResult : s.result a p expected)
    (tInitial : t.initial b q) (tResult : tr b q actual)
    (wrong : ¬ s.result a p actual) :
    ¬ BehaviorRefinement s t s.result tr a b := by
  intro preserves
  obtain ⟨state, finite, observation, _⟩ :=
    (preserves (stationarySafe s a p expected initial sResult)).1 q actual
      ⟨q, tInitial, .nil _⟩ tResult
  obtain rfl := reachableTerminal s a p expected initial sResult state finite
  exact wrong observation

private theorem refinesToTerminal {A B E : Type} {a : A} {b : B}
    (s : Concrete.Contracts.Semantics A E) (t : Concrete.Contracts.Semantics B E)
    (p : s.State) (q : t.State) (result : Concrete.Contracts.Observation)
    (reached : s.finite a p) (sResult : s.result a p result) (tResult : t.result b q result)
    (tInitial : ∀ x, t.initial b x → x = q)
    (onlyResult : ∀ o, t.result b q o → o = result)
    (trace : s.trace a p = t.trace b q) :
    BehaviorRefinement s t s.result t.result a b := by
  have onlyT := reachableTerminal t b q result tInitial tResult
  intro _
  refine ⟨?_, ?_, ?_⟩
  · intro x o path observed
    obtain rfl := onlyT x path
    obtain rfl := onlyResult o observed
    exact ⟨p, reached, sResult, trace⟩
  · intro run infinite
    exact (t.terminal b q result tResult _ (tInitial _ infinite.1 ▸ infinite.2 0)).elim
  · intro x stuck
    obtain rfl := onlyT x stuck.1
    exact stuck.2.2 result tResult

private theorem matchingRefinement {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} {sourceProgram : MIR lang profile} {output : Memory lang profile}
    (strict : BehaviorRefinement (MIR.Execution (language := language)) Memory.Execution
      MIR.Execution.result Memory.Execution.result sourceProgram output) :
    BehaviorRefinement (MIR.Execution (language := language)) Memory.Execution
      MIR.Execution.result Memory.MatchingResult sourceProgram output := by
  intro safe
  obtain ⟨finish, infinite, progress⟩ := strict safe
  exact ⟨fun q o hp h => finish q o hp (Memory.matchingResult_result h), infinite, progress⟩

private abbrev literalCore := elaboratedReturn 42
private abbrev literalCoreExecution := CoreInput.Execution (language := earlyFixtureLanguage)
private abbrev literalMIRExecution := MIR.Execution (language := earlyFixtureLanguage)
  (lang := returnLowering) (profile := testProfile)
private abbrev literalMemoryExecution := Memory.Execution (language := earlyFixtureLanguage)
  (lang := returnLowering) (profile := testProfile)
private abbrev literalSSAExecution := SSA.Execution (language := earlyFixtureLanguage)
  (lang := returnLowering) (profile := testProfile)
private abbrev literalBIRExecution := BIR.Execution (language := earlyFixtureLanguage)
  (lang := returnLowering) (profile := testProfile)
private abbrev literalJobExecution := LCBInput.Execution (profile := testProfile)
  earlyFixtureLanguage.operands.Event elaborationSource.project

private def literalOrigin : Origin :=
  ⟨⟨0⟩, ⟨⟨0⟩, 18, 28, 1, 19⟩, ⟨.function earlyEntry, ⟨[⟨.return, .ordinal 0⟩]⟩⟩⟩

private def literalSignatures : AList FnId (FunctionSignature earlyFixtureLanguage.operands.Ty) :=
  [(earlyEntry, ⟨[], some returnType⟩)]

private def literalMIR : MIR returnLowering testProfile :=
  ⟨elaborationSource.project, earlyEntry, literalSignatures,
    [(earlyEntry, [], singleBlockGraph (.ret (some ⟨returnType, .literal ⟨[42]⟩⟩)) [literalOrigin])], [], []⟩

private def literalMemory : Memory returnLowering testProfile := literalMIR.logicalView

private def literalSSA : SSA returnLowering testProfile :=
  ⟨elaborationSource.project, earlyEntry, literalSignatures,
    [(earlyEntry, [], singleBlockGraph (.ret (some ⟨returnType, .literal ⟨[42]⟩⟩)) [literalOrigin])], [], [], []⟩

private def literalBIR : BIR returnLowering testProfile :=
  ⟨elaborationSource.project, earlyEntry, literalSignatures,
    [(earlyEntry, [], singleBlockGraph (.ret (some ⟨returnType, .literal ⟨[42]⟩⟩)) [literalOrigin])], [], #[], [], []⟩

private def literalRegisters : MemoryRegisters earlyFixtureLanguage testProfile :=
  {ownerFixtureStart with frame := earlyEntry}

private def literalResult : Concrete.Contracts.Observation := .returned 42 ByteArray.empty

set_option maxRecDepth 4096 in
private theorem literalCoreReaches :
    ∃ s, literalCoreExecution.finite literalCore s ∧
      literalCoreExecution.result literalCore s literalResult ∧ literalCoreExecution.trace literalCore s = [] := by
  let start := literalCore.machineProgram.initial
  let next (q : Exec.Config literalCore.machineProgram.machine) :=
    match Exec.step literalCore.machineProgram.body q with
    | some (.next result) => result
    | _ => q
  let frame (q : Exec.Config literalCore.machineProgram.machine) : literalCoreExecution.State :=
    (⟨(literalCore.machineProgram.ownership, literalCore.machineProgram.declarations), q⟩, [])
  let returned := next start
  let unscoped := next returned
  let finished := next unscoped
  have tick (p : Exec.Config literalCore.machineProgram.machine)
      (dispatch : Exec.step literalCore.machineProgram.body p = some (.next (next p)))
      (active : p.final? = none)
      (trace : literalCoreExecution.trace literalCore (frame p) = [])
      (afterTrace : literalCoreExecution.trace literalCore (frame (next p)) = []) :
      literalCoreExecution.step literalCore (frame p) (frame (next p)) := by
    refine ⟨?_, ⟨p, next p, .silent, rfl, rfl, .silent dispatch, rfl⟩, ?_⟩
    · with_unfolding_all
        change Option.bind p.final? _ = none
        rw [active]
        rfl
    · change Concrete.Contracts.TracePrefix (literalCoreExecution.trace literalCore (frame p))
          (literalCoreExecution.trace literalCore (frame (next p)))
      rw [trace, afterTrace]
      exact ⟨[], rfl⟩
  refine ⟨frame finished, ⟨frame start, rfl,
    .cons (tick start ?_ ?_ ?_ ?_)
      (.cons (tick returned ?_ ?_ ?_ ?_) (.cons (tick unscoped ?_ ?_ ?_ ?_) (.nil _)))⟩, ?_, ?_⟩
  all_goals with_unfolding_all first | rfl | exact nofun

set_option maxRecDepth 4096 in
private theorem literalTables : literalMemory.TablesWF := by
  with_unfolding_all
    refine ⟨.cons nofun .nil, .cons nofun .nil, .cons nofun .nil, .nil, ?_, nofun, ?_⟩
    · intro fn classes member
      cases List.mem_singleton.mp member
      exact .nil
    · intro fn locals graph member
      cases List.mem_singleton.mp member
      refine ⟨.nil, ⟨.cons nofun .nil, ⟨_, List.mem_singleton_self _, rfl⟩, ?_⟩,
        ⟨_, rfl, ⟨_, List.mem_singleton_self _, rfl, rfl⟩, ?_⟩⟩
      · intro block present edge follows
        cases List.mem_singleton.mp present
        cases follows
      · intro block present
        cases List.mem_singleton.mp present
        exact ⟨rfl, nofun⟩

set_option maxRecDepth 4096 in
private theorem literalHandoff : literalMemory.HandoffWF := by
  refine ⟨literalTables, List.mem_singleton_self _, ?_, nofun⟩
  intro origin present
  have member : origin ∈ [literalOrigin] := by with_unfolding_all exact present
  obtain rfl := List.mem_singleton.mp member
  let checkedSpan : ConcreteSpec.Core.Span :=
    ⟨⟨0⟩, 18, 28, by decide, 1, 19, by decide, by decide⟩
  exact ⟨checkedSpan, _, rfl, rfl, by decide, rfl, rfl, rfl, rfl⟩

set_option maxRecDepth 4096 in
private theorem literalConsumes : ConsumesEvidence literalCore [] literalMIR := by
  refine ⟨.nil, ?_, nofun, ?_, ?_⟩
  · rintro site opcode conditions ⟨op, present, _, _, rfl⟩ nonempty
    cases List.mem_singleton.mp present
    exact (nonempty rfl).elim
  · rintro _ _ ⟨_, found, _⟩
    cases found
  · exact (returnedCoreWF 42).2.1

set_option maxRecDepth 4096 in
private theorem literalUnchecked :
    LowersUnchecked (language := earlyFixtureLanguage) literalCore literalMIR := by
  with_unfolding_all
    obtain ⟨reached, finite, result, trace⟩ := literalCoreReaches
    refine ⟨(ConcreteSpec.Admission.checkStatic_iff (returnedCoreWF 42)).1 rfl, rfl, rfl, ⟨fun _ h => h, fun _ h => h⟩, literalConsumes, ⟨⟨?_, nofun⟩, rfl⟩, ?_⟩
    · intro fn locals graph present
      cases List.mem_singleton.mp present
      exact nofun
    apply refinesToTerminal literalCoreExecution literalMIRExecution reached literalRegisters literalResult
      finite result
    · with_unfolding_all rfl
    · intro _ h
      exact h
    · intro _ h
      exact (Option.some.inj h).symm
    · exact trace

set_option maxRecDepth 4096 in
private theorem literalMemoryContract : LowersMemory (language := earlyFixtureLanguage) literalMIR literalMemory := by
  with_unfolding_all
    have sourceFinal : literalMIRExecution.result literalMIR literalRegisters literalResult := by with_unfolding_all rfl
    have targetFinal : literalMemoryExecution.result literalMemory literalRegisters literalResult := by with_unfolding_all rfl
    have sourceOnly := reachableTerminal literalMIRExecution literalMIR literalRegisters literalResult (fun _ h => h) sourceFinal
    have targetOnly := reachableTerminal literalMemoryExecution literalMemory literalRegisters literalResult (fun _ h => h) targetFinal
    have layout : LayoutsMatch earlyFixtureLanguage.layout literalMemory := by
      refine ⟨?_, nofun⟩
      intro fn locals graph present place ty member
      cases List.mem_singleton.mp present
      cases member
    have promotes : PromotionSound (language := earlyFixtureLanguage) literalMIR literalMemory := by
      constructor
      · intro fn locals graph present place ty member
        cases List.mem_singleton.mp present
        cases member
      · intro fn classes place present member
        cases List.mem_singleton.mp present
        cases member
    refine ⟨rfl, rfl, ⟨fun _ h => h, fun _ h => h⟩, layout, ⟨rfl, rfl⟩, promotes, literalTables, literalTables,
      matchingRefinement (language := earlyFixtureLanguage) (refinesToTerminal literalMIRExecution literalMemoryExecution literalRegisters literalRegisters literalResult
        ⟨literalRegisters, by with_unfolding_all rfl, .nil _⟩ sourceFinal targetFinal (fun _ h => h) (fun _ h => (Option.some.inj h).symm) rfl), ?_⟩
    intro _
    refine ⟨⟨fun p q => p = q, fun _ _ => 0, ?_, ?_, ?_⟩, ?_⟩
    · intro q atEntry
      obtain rfl := atEntry
      exact ⟨literalRegisters, rfl, rfl⟩
    · intro p q q' _ finite _ tick
      obtain rfl := targetOnly q finite
      exact (literalMemoryExecution.terminal _ _ _ targetFinal _ tick).elim
    · intro p q o _ finite same observed
      obtain rfl := targetOnly q finite
      subst p
      obtain rfl := (Option.some.inj (targetFinal.symm.trans observed)).symm
      exact ⟨literalRegisters, .nil _, sourceFinal, rfl⟩
    · intro p q finite reached _
      obtain rfl := sourceOnly p finite
      obtain rfl := targetOnly q reached
      have frame : literalMemory.RepresentsActivation (language := earlyFixtureLanguage)
          literalRegisters.toMemoryFrame literalRegisters.toMemoryFrame literalRegisters.bytes := by
        exact ⟨⟨⟨Mem.empty_wellFormed _, .nil, .nil, nofun⟩, rfl⟩, rfl, nofun, .nil, .nil, .nil, .nil, nofun,
          fun _ => ⟨nofun, nofun⟩, nofun, nofun, nofun, nofun⟩
      exact ⟨⟨frame, rfl, nofun, ⟨rfl, nofun⟩, .nil, rfl, rfl⟩, rfl⟩

set_option maxRecDepth 4096 in
private theorem literalSSAContract : BuildsSSA (language := earlyFixtureLanguage) literalMemory literalSSA := by
  with_unfolding_all
    refine ⟨rfl, rfl, rfl, ⟨rfl, rfl⟩, ⟨fun _ h => h, fun _ h => h⟩, ?_⟩
    exact refinesToTerminal literalMemoryExecution literalSSAExecution literalRegisters literalRegisters literalResult
      ⟨_, rfl, .nil _⟩ (by with_unfolding_all rfl) (by with_unfolding_all rfl)
      (fun _ h => h) (fun _ h => (Option.some.inj h).symm) rfl

set_option maxRecDepth 4096 in
private theorem literalBIRContract : BuildsBIR (language := earlyFixtureLanguage) literalSSA literalBIR := by
  with_unfolding_all
    refine ⟨rfl, ?_, ?_, ⟨fun _ h => h, fun _ h => h⟩, by decide, ⟨rfl, rfl, rfl⟩, BIR.refinesOfViewEq rfl⟩
    · intro fn present block inside slot trap
      cases List.mem_singleton.mp present
      cases List.mem_singleton.mp inside
      cases trap
    · intro fn present block inside kind site trap
      cases List.mem_singleton.mp present
      cases List.mem_singleton.mp inside
      cases trap

set_option maxRecDepth 4096 in
private theorem literalLCBContract : EmitsLCB earlyFixtureLanguage literalBIR (lcbReturnJob 42) := by
  with_unfolding_all
    let job := lcbReturnJob 42
    let jobEntry : literalJobExecution.State := (LeanCompilerBackend.Verified.Source.initial job.body #[]).toOption
    refine ⟨rfl, by with_unfolding_all rfl, nofun, .cons nofun .nil, rfl, ⟨[], rfl, rfl⟩, ?_,
      .cons nofun .nil, .cons nofun .nil, fun _ => Iff.rfl, by with_unfolding_all rfl, literalBIRContract.2.1⟩
    exact refinesToTerminal literalBIRExecution literalJobExecution literalRegisters jobEntry literalResult
      ⟨_, rfl, .nil _⟩ (by with_unfolding_all rfl) (by with_unfolding_all rfl)
      (fun _ h => h) (fun _ h => (Option.some.inj h).symm) rfl

set_option maxRecDepth 4096 in
example (unchecked : Bool) :
    let pipeline := (registry literalReturnPasses).lcbPipeline unchecked
    pipeline.Ready elaborationSource ∧ pipeline.Corresponds elaborationSource (lcbReturnJob 42) := by
  have static := returnAdmissionContract.1.2.1.1
  have checked := returnAdmissionContract.1
  have admitted : (⟨literalCore, []⟩ : Admitted earlyFixtureLanguage).Valid :=
    checked.2
  have fragment : literalCore.InFragment := ⟨_, representsReturn 42⟩
  have elaborates : (registry literalReturnPasses).elaborator.success parsedReturn pendingReturn :=
    ⟨returnProgram, rfl, rfl⟩
  have birWF : BIR.WF (language := earlyFixtureLanguage) literalBIR :=
    ⟨literalBIRContract.2.1, by decide, .cons nofun .nil, List.mem_singleton_self _, nofun, by with_unfolding_all exact literalTables, nofun⟩
  have ready : Source.WF elaborationSource ∧ elaborationSource.slice = slice0 := ⟨parsedReturnDomain.1, rfl⟩
  have front := returnLexerContract
  have parse := returnParserContract
  have lowerCore : LowersCore (language := earlyFixtureLanguage) ⟨literalCore, []⟩ literalMIR := literalUnchecked.2
  have back :
      ∃ bir, Concrete.Contracts.Graph (registry literalReturnPasses).buildBIR.run literalSSA bir ∧
        (registry literalReturnPasses).buildBIR.outputInvariant bir ∧
        (registry literalReturnPasses).buildBIR.success literalSSA bir ∧
        (registry literalReturnPasses).buildBIR.trust literalSSA bir ∧
        (Pipeline.next (registry literalReturnPasses).emitLCB .done).Corresponds bir (lcbReturnJob 42) := by
    exact ⟨literalBIR, by with_unfolding_all rfl, birWF, ⟨literalBIRContract, rfl⟩, trivial, lcbReturnJob 42,
      by with_unfolding_all rfl, ⟨literalBIR, literalLCBContract⟩, literalLCBContract, trivial, rfl⟩
  cases unchecked
  · exact ⟨ready, returnStream, rfl, ⟨front.2.1, rfl⟩, front, trivial,
      parsedReturn, by with_unfolding_all rfl, parsedReturnDomain, parse, trivial,
      pendingReturn, rfl, parsedReturnDomain.2.2.2.2 _ elaborates, elaborates, trivial,
      ⟨literalCore, []⟩, by with_unfolding_all rfl, admitted, ⟨rfl, _, rfl, checked⟩, trivial,
      literalMIR, rfl, literalHandoff, lowerCore, trivial,
      literalMemory, rfl, literalHandoff, ⟨literalMemoryContract, rfl⟩, trivial,
      literalSSA, by with_unfolding_all rfl, by with_unfolding_all exact literalHandoff, ⟨literalSSAContract, rfl⟩, trivial, back⟩
  · exact ⟨ready, returnStream, rfl, ⟨front.2.1, rfl⟩, front, trivial,
      parsedReturn, by with_unfolding_all rfl, parsedReturnDomain, parse, trivial,
      pendingReturn, rfl, parsedReturnDomain.2.2.2.2 _ elaborates, elaborates, trivial,
      literalCore, rfl, ⟨rfl, returnedCoreWF 42, fragment⟩, ⟨rfl, rfl⟩, trivial,
      literalCore, by with_unfolding_all rfl, ⟨rfl, static⟩, rfl, trivial,
      literalMIR, rfl, literalHandoff, literalUnchecked, trivial,
      literalMemory, rfl, literalHandoff, ⟨literalMemoryContract, rfl⟩, trivial,
      literalSSA, by with_unfolding_all rfl, by with_unfolding_all exact literalHandoff, ⟨literalSSAContract, rfl⟩, trivial, back⟩

private theorem birReturnTables (project : Project) (profile : Profile) :
    let original := birReturn project false
    let input : BIR testLowering profile :=
      ⟨project, original.entry, [(Exec.Demo.mainF, ⟨[], some ()⟩)],
        original.functions, original.layouts, original.sites, [], []⟩
    (input.memoryView (language := testLanguage)).TablesWF := by
  with_unfolding_all
    refine ⟨(by exact .cons nofun .nil), (by exact .cons nofun .nil), (by exact .cons nofun .nil), .nil, ?_, nofun, ?_⟩
    · intro fn classes member
      cases List.mem_singleton.mp member
      exact List.nodup_nil
    · intro fn locals graph member
      cases List.mem_singleton.mp member
      refine ⟨List.nodup_nil, ⟨(by exact .cons nofun .nil), ⟨_, List.mem_singleton_self _, rfl⟩, ?_⟩,
        ⟨_, rfl, ⟨_, List.mem_singleton_self _, rfl, rfl⟩, ?_⟩⟩
      · intro block inside edge successor
        cases List.mem_singleton.mp inside
        cases successor
      · intro block inside
        cases List.mem_singleton.mp inside
        exact ⟨rfl, nofun⟩

private def llvmReturn (status : Nat) : LLVMInput macLLVM :=
  ⟨macLLVM.llvmTriple, macLLVM.llvmDataLayout,
    [.define false (.integer .u32) .body [], .label .entry,
      .terminator (.ret (.integer .u32, .immediate ⟨status⟩)), .endFunction], #[]⟩

-- Both registered emitters produce structurally valid LLVM for the selected
-- profile. The fixed behavior interpretation distinguishes their bytes.
set_option maxRecDepth 4096 in
example (parser : Parser grammar) (project : Project) :
    let stages (status : Nat) := fixtureEntries parser
      {testPasses [] with emitLLVM := fun _ => .ok (llvmReturn status)}
    let original := birReturn project false
    let input : BIR testLowering macLLVM :=
      ⟨project, original.entry, [(Exec.Demo.mainF, ⟨[], some ()⟩)],
        original.functions, original.layouts, original.sites, [], []⟩
    let expected : Behavior Nat := .terminates (.returned 0 ByteArray.empty) []
    (stages 0).emitLLVM.run input = .ok (llvmReturn 0) ∧
      (stages 42).emitLLVM.run input = .ok (llvmReturn 42) ∧
      (stages 42).emitLLVM.success input (llvmReturn 42) ∧
      ((∀ behavior, llvmBehaviors (llvmReturn 0).moduleBytes behavior → behavior = expected) →
        (stages 0).emitLLVM.trust input (llvmReturn 0)) ∧
      (llvmBehaviors (llvmReturn 42).moduleBytes
        (.terminates (.returned 42 ByteArray.empty) ([] : List (Concrete.Contracts.ProcessEvent Nat))) →
        ¬ (stages 42).emitLLVM.Checks) := by
  intro stages original input expected
  let start := BIR.entryState (language := testLanguage) input
  refine ⟨rfl, rfl, ⟨rfl, rfl, rfl, rfl⟩, ?_, ?_⟩
  · intro exactLLVM _ behavior emitted
    obtain rfl := exactLLVM behavior emitted
    with_unfolding_all exact ⟨start, ⟨start, rfl, .nil _⟩, rfl, rfl⟩
  intro observed sound
  have eligible : (stages 42).emitLLVM.inputInvariant input := by
    refine ⟨?_, (by decide : 0 ≤ 2), by simp [input, original, birReturn, testBIR],
      by simp [input, original, birReturn, testBIR], nofun, birReturnTables project macLLVM, nofun⟩
    intro fn member block included slot trap
    cases List.mem_singleton.mp member
    cases List.mem_singleton.mp included
    cases trap
  have trusted := sound.2 input (llvmReturn 42) eligible rfl
  let exec := BIR.Execution (language := testLanguage) (lang := testLowering) (profile := macLLVM)
  let zero : Concrete.Contracts.Observation := .returned 0 ByteArray.empty
  have initial : ∀ p, exec.initial input p → p = start := fun _ h => h
  have result : exec.result input start zero := by with_unfolding_all rfl
  have safe := stationarySafe exec input start zero initial result
  obtain ⟨state, finite, terminal, _⟩ := trusted safe _ observed
  rw [reachableTerminal exec input start zero initial result state finite] at terminal
  have wrong : some (Concrete.Contracts.Observation.returned 0 ByteArray.empty) =
      some (.returned 42 ByteArray.empty) := by with_unfolding_all exact terminal
  cases wrong

private def trapEntry : TrapEntry := ⟨.divisionByZero, testOrigin⟩

private def trapBIR : BIR testLowering testProfile :=
  {(birReturn tabProject false) with
    functions := [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[{birReturnBlock false with ending := .trap ⟨0⟩}]⟩)]
    sites := #[trapEntry]}

private def trapJob : LCBInput testProfile :=
  let diagnostic :=
    s!"concrete: trap: {TrapKind.divisionByZero.name} at {mainSourcePath.render.text}:{span.line}:{span.col} (site {testSite.id})\n"
  {lcbReturnJob 256 with
    sites := trapBIR.sites
    runtimeInput := (Backend.Runtime.qbeDiagnostics [diagnostic]).toUTF8}

private def trapResult : Concrete.Contracts.Observation :=
  .trapped 134 .divisionByZero ⟨⟨testSite.id⟩, ⟨mainSourcePath.render.text, span.line, span.col⟩⟩
    (trapDiagnostic tabProject .divisionByZero testSite span)

-- A trapping entry is a valid terminal observation for the exact hosted job.
set_option maxRecDepth 2048 in
private theorem trapRefines :
    BehaviorRefinement birExecution (LCBInput.Execution Nat tabProject)
      birExecution.result (LCBInput.Execution (profile := testProfile) Nat tabProject).result trapBIR trapJob := by
  refine stationaryRefines _ _ _ _ _ _ trapResult trapResult
    (fun _ => Iff.rfl) (fun _ => Iff.rfl) ?_ ?_ ?_ ?_ ?_
  all_goals first
    | (intro o; with_unfolding_all exact ⟨fun h => (Option.some.inj h).symm, fun h => congrArg some h.symm⟩)
    | with_unfolding_all rfl

-- The exact hosted job carries both the trap result and its rendered runtime diagnostics.
example : EmitsLCB testLanguage trapBIR trapJob := by
  refine ⟨rfl, by with_unfolding_all rfl, nofun, by simp [trapJob, lcbReturnJob], rfl,
    ⟨_, by with_unfolding_all rfl, by with_unfolding_all rfl⟩, trapRefines,
    by simp [trapBIR, birReturn], by simp [trapJob, lcbReturnJob], fun _ => Iff.rfl,
    by with_unfolding_all rfl, ?_⟩
  intro fn member block included slot trapped
  cases List.mem_singleton.mp member
  cases List.mem_singleton.mp included
  cases BIRTerm.trap.inj trapped
  exact ⟨trapEntry, rfl⟩

-- Preserve the trap table and runtime, but replace the emitted trap word
-- with an ordinary exit status. Job validation still accepts the body.
set_option maxRecDepth 2048 in
example :
    let output := {trapJob with body := (lcbReturnJob 134).body}
    validateJob (Backend.LCB.job output.body) = .ok () ∧
      ¬ EmitsLCB testLanguage trapBIR output := by
  refine ⟨by with_unfolding_all rfl, ?_⟩
  intro emitted
  apply stationaryRejects birExecution (LCBInput.Execution Nat tabProject) (LCBInput.Execution Nat tabProject).result
    (BIR.entryState (language := testLanguage) trapBIR) (some ⟨0, 0, #[]⟩)
    trapResult (.returned 134 ByteArray.empty) (fun _ h => h) ?_ ?_ ?_ ?_ emitted.2.2.2.2.2.2.1
  · with_unfolding_all rfl
  · with_unfolding_all rfl
  · with_unfolding_all rfl
  · intro observed
    have wrong : some trapResult = some (.returned 134 ByteArray.empty) := by
      with_unfolding_all exact observed
    cases wrong

private abbrev choiceLanguage : CoreLanguage :=
  {testLanguage with
    operation := fun _ _ _ _ _ _ _ before result effects after =>
      (result = .scalar (.int .u8 0) ∨ result = .scalar (.int .u8 1)) ∧
      effects = [] ∧ after = before}

private def choiceBIR (project : Project) : BIR testLowering testProfile :=
  let block := {birReturnBlock false with
    body := [⟨testOrigin, .op (opcode := .named ⟨"choose"⟩) (ty := ()) ⟨0⟩ () .nil⟩]
    ending := .ret (some ⟨(), .value ⟨0⟩⟩)}
  {(birReturn project false) with functions := [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[block]⟩)]}

private abbrev choiceExecution := BIR.Execution (language := choiceLanguage)
  (lang := testLowering) (profile := testProfile)

set_option maxRecDepth 3072 in
private theorem choiceReturns (project : Project) (alternate : Bool) :
    ∃ state, choiceExecution.finite (choiceBIR project) state ∧
      choiceExecution.result (choiceBIR project) state (.returned (if alternate then 1 else 0) ByteArray.empty) ∧
      choiceExecution.trace (choiceBIR project) state = [] := by
  let bir := choiceBIR project
  let start := BIR.entryState (language := choiceLanguage) bir
  let value : CoreValue Unit := .scalar (.int .u8 (if alternate then 1 else 0))
  let live : MemoryRegisters choiceLanguage testProfile := {start with live := [⟨0⟩]}
  let stored : MemoryRegisters choiceLanguage testProfile :=
    {live with initialized := [⟨0⟩], virtualValues := [(⟨0⟩, value)]}
  have step : choiceExecution.step bir start stored.advance := by
    with_unfolding_all
      refine ⟨rfl, ⟨_, rfl, [], start, [], value, [], Memory.operationStore start false,
        stored, rfl, rfl, rfl, ⟨?_, rfl, rfl⟩, rfl, ?_, rfl⟩, [], ?_⟩
    · cases alternate
      · exact Or.inl rfl
      · exact Or.inr rfl
    · apply Memory.WriteParams.cons (started := live)
      · with_unfolding_all exact ⟨nofun, Or.inl ⟨rfl, rfl⟩⟩
      · with_unfolding_all exact ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩
      · exact .nil _
    · cases alternate <;> with_unfolding_all rfl
  refine ⟨_, ⟨start, rfl, .cons step (.nil _)⟩, ?_, ?_⟩
  all_goals cases alternate <;> with_unfolding_all rfl

-- The source actually reaches both outcomes. The exact emitted job selects one.
set_option maxRecDepth 3072 in
set_option maxHeartbeats 1000000 in
example (project : Project) :
    choiceExecution.observes (choiceBIR project) (.returned 1 ByteArray.empty) ∧
    EmitsLCB choiceLanguage (choiceBIR project) (lcbReturnJob 0) := by
  obtain ⟨another, reachesAnother, observesAnother, _⟩ := choiceReturns project true
  refine ⟨⟨another, reachesAnother, observesAnother⟩, rfl, by with_unfolding_all rfl, nofun,
    by simp [lcbReturnJob], rfl, ⟨[], rfl, rfl⟩, ?_, by simp [choiceBIR], by simp [lcbReturnJob],
    fun _ => Iff.rfl, by with_unfolding_all rfl, ?_⟩
  · let exec := LCBInput.Execution (profile := testProfile) Nat project
    let output := lcbReturnJob 0
    let result : Concrete.Contracts.Observation := .returned 0 ByteArray.empty
    let entry : exec.State := (LeanCompilerBackend.Verified.Source.initial output.body #[]).toOption
    have terminal : exec.result output entry result := by with_unfolding_all rfl
    have reached := reachableTerminal exec output entry result (fun _ h => h) terminal
    obtain ⟨source, sourceReached, observed, sourceTrace⟩ := choiceReturns project false
    intro _
    refine ⟨?_, ?_, ?_⟩
    · intro state _ path observation
      obtain rfl := reached state path
      have same := Option.some.inj (terminal.symm.trans observation)
      exact ⟨source, sourceReached, same ▸ observed, sourceTrace.trans (by with_unfolding_all rfl)⟩
    · intro run infinite
      exact (exec.terminal output entry result terminal _
        ((show run 0 = entry from infinite.1) ▸ infinite.2 0)).elim
    · intro state stuck
      obtain rfl := reached state stuck.1
      exact stuck.2.2 result terminal
  · intro fn member block included slot trap
    cases List.mem_singleton.mp member
    cases List.mem_singleton.mp included
    cases trap

-- The selected job still returns zero, but the wrong runtime bytes fail
-- the LCB boundary's exact runtime match.
example (project : Project) :
    ¬ EmitsLCB choiceLanguage (choiceBIR project)
      {lcbReturnJob 0 with runtimeInput := ByteArray.empty} := by
  intro emitted
  obtain ⟨diagnostics, decoded, bytes⟩ := emitted.2.2.2.2.2.1
  have empty : some [] = some diagnostics := by with_unfolding_all exact decoded
  obtain rfl := (Option.some.inj empty).symm
  have wrong := congrArg ByteArray.size bytes
  have different : (ByteArray.empty.size = (Backend.Runtime.qbeDiagnostics []).toUTF8.size) → False := by decide
  exact different wrong

private def bundledBIR (project : Project) : BIR testLowering testProfile :=
  let entry := birReturn project false
  {entry with
    functions := List.append entry.functions [(callee, [], ⟨⟨0⟩, #[birReturnBlock true]⟩)]
    signatures := [(callee, ⟨[], some ()⟩)]}

private def leafJob (value : BitVec 32) : LeanCompilerBackend.Verified.FunctionJob :=
  let job := Backend.LCB.job (lcbReturnJob value).body
  {job with code := {job.code with symbol := {job.code.symbol with name := "concrete_callee"}}}

private def bundledJobs (value : BitVec 32) : LCBInput testProfile :=
  {lcbReturnJob 0 with additionalJobs := [(callee, leafJob value)]}

-- An unused helper's code may change from 42 in BIR to 7 in the emitted bundle.
set_option maxHeartbeats 1500000 in
example (project : Project) :
    EmitsLCB testLanguage (bundledBIR project) (bundledJobs 7) := by
  have distinct : [Exec.Demo.mainF, callee].Nodup := by decide
  refine ⟨rfl, by with_unfolding_all rfl, ?_, by decide, rfl, ⟨[], rfl, rfl⟩, ?_,
    distinct, distinct, fun _ => Iff.rfl, by with_unfolding_all rfl, ?_⟩
  · intro _ member
    cases List.mem_singleton.mp member
    with_unfolding_all rfl
  · refine stationaryRefines _ _ _ _ _ _ (.returned 0 ByteArray.empty)
      (.returned 0 ByteArray.empty) (fun _ => Iff.rfl) (fun _ => Iff.rfl) ?_ ?_ ?_ ?_ ?_
    all_goals first
      | (intro o; with_unfolding_all exact ⟨fun h => (Option.some.inj h).symm, fun h => congrArg some h.symm⟩)
      | exact Or.inl (by with_unfolding_all rfl)
      | exact Or.inr (by with_unfolding_all rfl)
      | with_unfolding_all rfl
  · intro fn member block included slot trap
    have member : fn ∈ [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[birReturnBlock false]⟩),
        (callee, [], ⟨⟨0⟩, #[birReturnBlock true]⟩)] := member
    simp only [List.mem_cons, List.not_mem_nil, or_false] at member
    rcases member with rfl | rfl
    all_goals
      cases List.mem_singleton.mp included
      cases trap

-- A hosted invocation that is missing an entry argument cannot return via
-- an unused block parameter.
example (project : Project) :
    let block := {birReturnBlock false with params := [⟨(), ⟨0⟩⟩]}
    let bir : BIR testLowering testProfile := {(birReturn project false) with
      functions := [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[block]⟩)]}
    BIR.hostedResult (language := testLanguage) bir (BIR.entryState bir) = none := by
  with_unfolding_all rfl

private def invalidTrapBlock : Block (Sigma (SSAValue Unit)) (LocatedOp (SSAInst testLowering))
    (BIRTerm (Sigma (SSARead Unit testLowering.literal)) (SSARead Unit testLowering.literal ())) :=
  ⟨⟨0⟩, [], [], .trap ⟨0⟩, []⟩

private def invalidTrapCallee (project : Project) : BIR testLowering testProfile :=
  let entry := {birReturnBlock false with body := [⟨testOrigin, .call callee none []⟩]}
  {(birReturn project false) with
    functions := [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[entry]⟩), (callee, [], ⟨⟨0⟩, #[invalidTrapBlock]⟩)]
    signatures := [(callee, ⟨[], none⟩)]}

-- A call to a void helper with a valid trap slot is refused before emission.
example (m : LoweringModels testLanguage testLowering testProfile) (parser : Parser grammar) :
    let bir := {invalidTrapCallee tabProject with sites := #[trapEntry]}
    (r m parser).emitLCB.run bir = .error .lcbLinkingUnavailable := by
  with_unfolding_all rfl

-- The same call block is accepted when disconnected and refused when its
-- predecessor jumps to it. The array order differs from traversal order.
example (parser : Parser grammar) :
    let callBlock := {birReturnBlock false with
      id := ⟨1⟩
      body := [⟨testOrigin, .call callee none []⟩]}
    let entry (linked : Bool) :=
      if linked then {birReturnBlock false with ending := .jump ⟨⟨1⟩, []⟩}
      else birReturnBlock false
    let input (linked : Bool) : BIR testLowering testProfile :=
      {bundledBIR tabProject with
        functions := [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[callBlock, entry linked]⟩),
          (callee, [], ⟨⟨0⟩, #[birReturnBlock true]⟩)]}
    let stages := fixtureEntries parser {testPasses [] with emitLCB := fun _ => .ok (bundledJobs 7)}
    stages.emitLCB.run (input false) = .ok (bundledJobs 7) ∧
      stages.emitLCB.run (input true) = .error .lcbLinkingUnavailable := by
  with_unfolding_all exact ⟨rfl, rfl⟩

-- The verified job edge requires no external behavior hypothesis.
example (m : LoweringModels testLanguage testLowering testProfile) (parser : Parser grammar) :
    (r m parser).emitLCB.Trusted := fun _ _ _ _ => trivial

private theorem oversizedLCBRefused (passes : Passes slice0 testProfile)
    (bir : BIR returnLowering testProfile) (over : LCBInput.trapCapacity < bir.sites.size) :
    let edge := (registry passes).emitLCB
    let failure := StageFailure.resource (LCBInput.trapCapacityRefusal bir.sites.size)
    edge.run bir = .error failure ∧ edge.refusal bir failure := by
  refine ⟨?_, rfl, over⟩
  change passes.emitSingleLCB bir = .error _
  simp [PassFunctions.emitSingleLCB, over]

private def singletonTrapBIR (sites : TrapTable) (slot : TrapIndex) :
    BIR returnLowering testProfile :=
  ⟨tabProject, earlyEntry, [(earlyEntry, ⟨[], none⟩)],
    [(earlyEntry, [], singleBlockGraph (.trap slot) [])], [], sites, [], []⟩

private theorem singletonTrapWF (sites : TrapTable) (slot : TrapIndex)
    (resolves : ∃ entry, sites[slot.index]? = some entry)
    (locations : ∀ site ∈ sites.toList, ∃ span, site.origin.lexical.Binds tabProject span) :
    BIR.WF (language := earlyFixtureLanguage) (singletonTrapBIR sites slot) := by
  obtain ⟨entry, found⟩ := resolves
  refine ⟨?_, Nat.zero_le _, .cons nofun .nil, List.mem_singleton_self _, locations, ?_, nofun⟩
  · intro fn member block inside index trapped
    cases List.mem_singleton.mp member
    cases List.mem_singleton.mp inside
    cases BIRTerm.trap.inj trapped
    exact ⟨entry, found⟩
  · with_unfolding_all
      refine ⟨.cons nofun .nil, .cons nofun .nil, .cons nofun .nil, .nil, ?_, nofun, ?_⟩
      · intro fn classes member
        cases List.mem_singleton.mp member
        exact .nil
      · intro fn locals graph member
        cases List.mem_singleton.mp member
        refine ⟨.nil, ⟨.cons nofun .nil, ⟨_, List.mem_singleton_self _, rfl⟩, ?_⟩,
          ⟨_, rfl, ⟨_, List.mem_singleton_self _, rfl, rfl⟩, ?_⟩⟩
        · intro block inside edge follows
          cases List.mem_singleton.mp inside
          change edge ∈ (match sites[slot.index]? with
            | some site => (.trap site.kind site.origin : MemoryTerm returnLowering)
            | none => .invalid).successors at follows
          rw [found] at follows
          cases follows
        · intro block inside
          let terminator : MemoryTerm returnLowering :=
            match sites[slot.index]? with
            | some site => .trap site.kind site.origin
            | none => .invalid
          let decoded : MemoryBlock returnLowering := ⟨⟨0⟩, [], [], terminator, []⟩
          have blockEq : block = decoded := List.mem_singleton.mp inside
          have endingEq : decoded.ending = .trap entry.kind entry.origin := by
            change terminator = _
            simp only [terminator, found]
          rw [blockEq]
          constructor
          · rw [endingEq]
            trivial
          · exact nofun

-- Prove the table facts for a symbolic length so the kernel never constructs
-- the oversized array.
private def syntheticTrapSites (n : Nat) : TrapTable :=
  (Array.replicate n ⟨.divisionByZero, testOrigin⟩).push ⟨.abort, testOrigin⟩

private theorem syntheticTrapSize (n : Nat) : (syntheticTrapSites n).size = n + 1 := by
  simp [syntheticTrapSites]

private theorem syntheticTrapWF (n : Nat) :
    BIR.WF (language := earlyFixtureLanguage) (singletonTrapBIR (syntheticTrapSites n) ⟨n⟩) := by
  apply singletonTrapWF
  · change ∃ entry, (syntheticTrapSites n)[n]? = some entry
    exact ⟨⟨.abort, testOrigin⟩, by
      simp only [syntheticTrapSites, Array.getElem?_push, Array.size_replicate, ite_true]⟩
  · intro site member
    have same : site.origin = testOrigin := by
      simp only [syntheticTrapSites, Array.toList_push, Array.toList_replicate, List.mem_append,
        List.mem_singleton] at member
      rcases member with repeated | rfl
      · rw [List.eq_of_mem_replicate repeated]
      · rfl
    rw [same]
    let checkedSpan : ConcreteSpec.Core.Span :=
      ⟨⟨0⟩, 0, 1, by decide, 1, 1, by decide, by decide⟩
    exact ⟨checkedSpan, _, rfl, rfl, by decide, rfl, rfl, rfl, rfl⟩

private def OversizedTrapGate (n : Nat) : Prop :=
  let bir := singletonTrapBIR (syntheticTrapSites n) ⟨n⟩
  let passes := {literalReturnPasses with emitLCB := fun _ => .ok (lcbReturnJob 42)}
  let edge := (registry passes).emitLCB
  let failure := StageFailure.resource (LCBInput.trapCapacityRefusal bir.sites.size)
  bir.sites.size = LCBInput.trapCapacity + 1 ∧ edge.inputInvariant bir ∧
    edge.run bir = .error failure ∧ edge.refusal bir failure

set_option maxRecDepth 4096 in
private theorem oversizedTrapGate (n : Nat) (capacity : LCBInput.trapCapacity = n) :
    OversizedTrapGate n := by
  unfold OversizedTrapGate
  intro bir passes edge failure
  have size : bir.sites.size = LCBInput.trapCapacity + 1 := syntheticTrapSize n |>.trans
    (congrArg (· + 1) capacity.symm)
  exact ⟨size, syntheticTrapWF n, oversizedLCBRefused passes bir (size.symm ▸ Nat.lt_succ_self _)⟩

example : OversizedTrapGate LCBInput.trapCapacity := oversizedTrapGate _ rfl

-- A zero-slot self-loop remains a real graph and needs no result slot.
set_option maxRecDepth 4096 in
example :
    let ssa : SSA returnLowering testProfile :=
      ⟨tabProject, earlyEntry, [(earlyEntry, ⟨[], none⟩)],
        [(earlyEntry, [], singleBlockGraph (.jump ⟨⟨0⟩, []⟩) [])], [], [], []⟩
    let bir : BIR returnLowering testProfile :=
      ⟨tabProject, ssa.entry, ssa.signatures,
        [(earlyEntry, [], singleBlockGraph (.jump ⟨⟨0⟩, []⟩) [])], [], #[], [], []⟩
    let edge := (registry {literalReturnPasses with buildBIR := fun _ => .ok bir}).buildBIR
    ssa.frameSlots = 0 ∧ edge.inputInvariant ssa ∧ edge.outputInvariant bir ∧
      edge.run ssa = .ok bir ∧ edge.success ssa bir := by
  intro ssa bir edge
  have tables : (ssa.memoryView (language := earlyFixtureLanguage)).TablesWF := by
    with_unfolding_all
      refine ⟨.cons nofun .nil, .cons nofun .nil, .cons nofun .nil, .nil, ?_, nofun, ?_⟩
      · intro fn classes present
        cases List.mem_singleton.mp present
        exact .nil
      · intro fn locals graph present
        cases List.mem_singleton.mp present
        refine ⟨.nil, ⟨.cons nofun .nil, ⟨_, List.mem_singleton_self _, rfl⟩, ?_⟩,
          ⟨_, rfl, ⟨_, List.mem_singleton_self _, rfl, rfl⟩, ?_⟩⟩
        · intro block inside edge target
          cases List.mem_singleton.mp inside
          cases List.mem_singleton.mp target
          exact ⟨_, List.mem_singleton_self _, rfl, rfl⟩
        · intro block inside
          cases List.mem_singleton.mp inside
          exact ⟨trivial, nofun⟩
  have noTrap : bir.TrapSlotsWF := by
    intro fn present block inside site trap
    cases List.mem_singleton.mp present
    cases List.mem_singleton.mp inside
    cases trap
  refine ⟨rfl, ⟨tables, List.mem_singleton_self _, by with_unfolding_all exact ⟨nofun, nofun⟩⟩,
    ⟨noTrap, by decide, .cons nofun .nil,
    List.mem_singleton_self _, nofun, ?_⟩, rfl,
    ⟨rfl, noTrap, ?_, ?_, by decide, ⟨rfl, rfl, by with_unfolding_all rfl⟩, ?_⟩, rfl⟩
  · with_unfolding_all exact ⟨tables, nofun⟩
  · intro fn present block inside kind site trap
    cases List.mem_singleton.mp present
    cases List.mem_singleton.mp inside
    cases trap
  · with_unfolding_all exact ⟨nofun, nofun⟩
  · exact BIR.refinesOfViewEq (by with_unfolding_all rfl)

-- A signature missing from an otherwise eligible return graph is rejected
-- by the decoded Memory tables at both backend entries.
example :
    ¬ BIR.WF (language := testLanguage) (birReturn tabProject false) := by
  intro wf
  have checked := wf.2.2.2.2.2.1
  obtain ⟨_, _, signature⟩ := checked.2.2.2.2.2.2 _ _ _ (List.mem_singleton_self _)
  obtain ⟨_, found, _⟩ := signature
  cases found

-- Missing and duplicate entries cannot enter the backend stage.
example :
    let input := birReturn tabProject false
    (¬ BIR.WF (language := testLanguage) ({input with functions := []} : BIR testLowering testProfile)) ∧
    (¬ BIR.WF (language := testLanguage) ({input with functions := input.functions ++ input.functions} : BIR testLowering testProfile)) := by
  refine ⟨fun h => ?_, fun h => ?_⟩
  · exact List.not_mem_nil h.2.2.2.1
  · simpa [birReturn] using h.2.2.1

example :
    let input := birReturn tabProject false
    let unbound := {testOrigin with lexical := {testOrigin.lexical with file := ⟨99⟩}}
    ¬ BIR.WF (language := testLanguage) ({input with sites := #[⟨.abort, unbound⟩]} : BIR testLowering testProfile) := by
  dsimp only
  intro invalid
  obtain ⟨_, _, found, _⟩ := invalid.2.2.2.2.1 _ (List.mem_singleton_self _)
  cases found

private theorem noInvalidEnding {memory : Memory testLowering testProfile} (wellFormed : memory.TablesWF)
    {fn locals graph} (present : (fn, locals, graph) ∈ memory.functions)
    {block} (inGraph : block ∈ graph.blocks.toList) : block.ending ≠ .invalid := by
  intro invalid
  obtain ⟨_, _, _, signatureMatches⟩ := (wellFormed.2.2.2.2.2.2 fn locals graph present).2.2
  have result := (signatureMatches block inGraph).1
  rw [invalid] at result
  exact result

-- A spurious typed linking refusal cannot validate on the registered edge.
example (parser : Parser grammar) :
    let stages := fixtureEntries parser
      {testPasses (profile := testProfile) [] with emitLCB := fun _ => .error .lcbLinkingUnavailable}
    ¬ stages.emitLCB.Validated := by
  intro stages valid
  let input : BIR testLowering testProfile := {birReturn tabProject false with signatures := [(Exec.Demo.mainF, ⟨[], some ()⟩)]}
  have eligible : stages.emitLCB.inputInvariant input := by
    refine ⟨?_, (by decide : 0 ≤ 3), by simp [input, birReturn, testBIR], by simp [input, birReturn, testBIR], nofun, ?_⟩
    · intro fn member block included slot trap
      cases List.mem_singleton.mp member
      cases List.mem_singleton.mp included
      cases trap
    · exact ⟨birReturnTables tabProject testProfile, nofun⟩
  have wrong : false = true := by
    with_unfolding_all exact valid.2 input .lcbLinkingUnavailable eligible rfl
  cases wrong

private def atInvalidTrap (project : Project) : MemoryRegisters testLanguage testProfile :=
  {(BIR.entryState (invalidTrapCallee project)) with
    frame := callee
    callers := [⟨((BIR.entryState (invalidTrapCallee project)).advance).toMemoryFrame, none⟩]}

-- The void callee reaches the malformed terminator and can neither return to
-- its caller nor manufacture a terminal observation. The job edge refuses it.
set_option maxHeartbeats 1000000 in
example (project : Project) :
    birExecution.step (invalidTrapCallee project) (BIR.entryState (invalidTrapCallee project))
      (atInvalidTrap project) ∧
    (∀ q, ¬ birExecution.step (invalidTrapCallee project) (atInvalidTrap project) q) ∧
    (∀ o, ¬ birExecution.result (invalidTrapCallee project) (atInvalidTrap project) o) ∧
    ¬ EmitsLCB testLanguage (invalidTrapCallee project) (bundledJobs 42) := by
  let physical : MemoryBlock testLowering := ⟨⟨0⟩, [], [], .invalid, []⟩
  refine ⟨?_, ?_, ?_, ?_⟩
  · with_unfolding_all
      exact ⟨rfl, ⟨_, rfl, rfl, [], _, _, _, rfl, rfl, rfl, rfl, .nil _⟩, [], rfl⟩
  · intro q execution
    obtain ⟨_, _, block, fetched, tick⟩ := Memory.Step.exists_outcome execution.2.1
    have eq : some physical = some block := by with_unfolding_all exact fetched
    obtain rfl := Option.some.inj eq
    exact tick.2
  · intro observed result
    have impossible : none = some observed := by with_unfolding_all exact result
    cases impossible
  · intro emits
    obtain ⟨entry, found⟩ := emits.2.2.2.2.2.2.2.2.2.2.2
      (callee, [], ⟨⟨0⟩, #[invalidTrapBlock]⟩)
      (List.mem_cons_of_mem _ (List.mem_singleton_self _)) invalidTrapBlock
      (List.mem_singleton_self _) ⟨0⟩ rfl
    cases found

private def backingFits (space : AddrSpace) (base : Nat) : Prop :=
  Mem.Memory.empty.Fits testProfile.memoryTarget (Mem.candidate space base 1 1)

private def backingRange (space : AddrSpace) (base : Nat) : Hybrid.Range :=
  ⟨⟨space, base, some ⟨0⟩⟩, 1, 1⟩

private def backingBefore (space : AddrSpace) (base : Nat) : MemoryRegisters erasureLanguage testProfile :=
  {storeBefore with
    materialized := [(storeRoot, backingRange space base)]
    bytes := ⟨[(⟨0⟩, Mem.candidate space base 1 1)], 1⟩}

private def backingAfter (space : AddrSpace) (base : Nat) (byte : Fin 256 := 42) : MemoryRegisters erasureLanguage testProfile :=
  {backingBefore space base with
    instruction := 2, initialized := [storeRoot]
    bytes := ⟨[(⟨0⟩, {Mem.candidate space base 1 1 with bytes := [.init byte none]})], 1⟩}

private def secondSplitField : FieldId := {splitField with name := ⟨"second"⟩}

private abbrev twoFieldLanguage : CoreLanguage :=
  {erasureLanguage with
    operands := {erasureLanguage.operands with isCopy := fun _ => false}
    safety := fun _ => []
    operation := fun _ opcode _ _ values _ addresses before result effects after =>
      opcode = .named ⟨"sumFields"⟩ ∧
      values = [.scalar (.int .u8 17), .scalar (.int .u8 25)] ∧ addresses = [] ∧
      result = .scalar (.int .u8 42) ∧ effects = [] ∧ after = before
    fieldType := fun _ _ => some ()
    activeFields := fun _ _ _ => some [splitField, secondSplitField]
    field := fun _ path =>
      if path = [splitField] then some (.scalar (.int .u8 17))
      else if path = [secondSplitField] then some (.scalar (.int .u8 25)) else none}

private def partialRead (path : List FieldId) : Read Unit testLowering.literal () :=
  .move ⟨storeRoot, path⟩

private def partialMIR (sibling : List FieldId) : MIR testLowering testProfile :=
  let body : List (MIRInst testLowering) :=
    [.bind (ty := ()) ⟨storeRoot, []⟩ (.literal ⟨[]⟩),
      .bind ⟨⟨1⟩, []⟩ (partialRead [splitField]),
      .bind ⟨⟨2⟩, []⟩ (partialRead sibling),
      .endLifetime storeRoot,
      .op (opcode := .named ⟨"sumFields"⟩) (types := [(), ()]) (ty := ()) ⟨⟨3⟩, []⟩ ()
        (.cons (.move ⟨⟨1⟩, []⟩) (.cons (.move ⟨⟨2⟩, []⟩) .nil)) []]
  ⟨tabProject, Exec.Demo.mainF, [(Exec.Demo.mainF, ⟨[], some ()⟩)],
    [(Exec.Demo.mainF, [(storeRoot, ()), (⟨1⟩, ()), (⟨2⟩, ()), (⟨3⟩, ())],
      ⟨⟨0⟩, #[⟨⟨0⟩, [], body.map (LocatedOp.mk testOrigin),
        .ret (some ⟨(), .move ⟨⟨3⟩, []⟩⟩), []⟩]⟩)], [], []⟩

private def firstMoved : MemoryRegisters twoFieldLanguage testProfile :=
  {erasureBound with
    instruction := 2
    live := [⟨1⟩, storeRoot]
    initialized := [⟨1⟩, storeRoot]
    movedFields := [(storeRoot, [splitField])]
    virtualValues := [(⟨1⟩, .scalar (.int .u8 17)), (storeRoot, .other ())]}

private def bothMoved : MemoryRegisters twoFieldLanguage testProfile :=
  {firstMoved with
    instruction := 3
    live := ⟨2⟩ :: firstMoved.live
    initialized := ⟨2⟩ :: firstMoved.initialized
    movedFields := (storeRoot, [secondSplitField]) :: firstMoved.movedFields
    virtualValues := (⟨2⟩, .scalar (.int .u8 25)) :: firstMoved.virtualValues}

-- Starting at entry, each linear field moves once. The remaining sibling is
-- usable, and the aggregate can end only when both linear fields are gone.
set_option maxRecDepth 4096 in
set_option maxHeartbeats 1000000 in
example :
    let program := partialMIR [secondSplitField]
    (program.logicalView (language := twoFieldLanguage)).Initial storeInitial ∧
    Concrete.Contracts.Steps (program.Step (language := twoFieldLanguage)) storeInitial bothMoved ∧
    (program.logicalView (language := twoFieldLanguage)).canEnd firstMoved storeRoot = false ∧
    (program.logicalView (language := twoFieldLanguage)).canEnd bothMoved storeRoot = true ∧
    (∀ next, ¬ (partialMIR [splitField]).Step (language := twoFieldLanguage) firstMoved next) ∧
    (∀ next, ¬ (partialMIR []).Step (language := twoFieldLanguage) firstMoved next) := by
  refine ⟨?_, ?_, rfl, rfl, ?_, ?_⟩
  · with_unfolding_all
      exact ⟨rfl, rfl, ⟨_, rfl, rfl⟩, ⟨_, rfl, rfl⟩,
        rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩
  · refine .cons (b := erasureBound) ?_ (.cons (b := firstMoved) ?_ (.cons ?_ (.nil _)))
    · with_unfolding_all
        refine ⟨rfl, .other (), storeInitial, {erasureBound with instruction := 0}, rfl, ?_, rfl⟩
      rw [ite_eq_left (show (true || false) = true from rfl)]
      with_unfolding_all apply Memory.WriteParams.cons (language := twoFieldLanguage) (started := {storeInitial with live := [storeRoot]})
      · with_unfolding_all exact ⟨nofun, Or.inl ⟨rfl, rfl⟩⟩
      · with_unfolding_all exact ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩
      · exact .nil _
    · let afterRead : MemoryRegisters twoFieldLanguage testProfile :=
        {erasureBound with movedFields := firstMoved.movedFields}
      with_unfolding_all
        refine ⟨rfl, .scalar (.int .u8 17), afterRead, {firstMoved with instruction := 1}, rfl, ?_, rfl⟩
      rw [ite_eq_left (show (true || false) = true from rfl)]
      with_unfolding_all apply Memory.WriteParams.cons (language := twoFieldLanguage) (started := {afterRead with live := firstMoved.live})
      · with_unfolding_all exact ⟨nofun, Or.inl ⟨rfl, rfl⟩⟩
      · with_unfolding_all exact ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩
      · exact .nil _
    · let afterRead : MemoryRegisters twoFieldLanguage testProfile :=
        {firstMoved with movedFields := bothMoved.movedFields}
      with_unfolding_all
        refine ⟨rfl, .scalar (.int .u8 25), afterRead, {bothMoved with instruction := 2}, rfl, ?_, rfl⟩
      rw [ite_eq_left (show (true || false) = true from rfl)]
      with_unfolding_all apply Memory.WriteParams.cons (language := twoFieldLanguage) (started := {afterRead with live := bothMoved.live})
      · with_unfolding_all exact ⟨nofun, Or.inl ⟨rfl, rfl⟩⟩
      · with_unfolding_all exact ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩
      · exact .nil _
  · intro next tick
    obtain ⟨_, _, _, _, read, _, _⟩ := tick
    with_unfolding_all contradiction
  · intro next tick
    obtain ⟨_, _, _, _, read, _, _⟩ := tick
    with_unfolding_all contradiction

private abbrev loanFieldLanguage : CoreLanguage :=
  {earlyFixtureLanguage with
    fieldType := fun _ field =>
      if field = splitField then some (.inr false)
      else if field = secondSplitField then some (.inl (.scalar (.int .u8))) else none
    activeFields := fun _ _ _ => some [splitField, secondSplitField]
    replaceField := fun _ path value => if path = [secondSplitField] then some value else none
    field := fun _ path =>
      if path = [splitField] then some (.scalar (.int .u8 17))
      else if path = [secondSplitField] then some (.scalar (.int .u8 25)) else none}

private def loanLowering : LoweringLanguage loanFieldLanguage.operands.Ty :=
  ⟨fun _ _ _ => Unit, .inr false⟩

private def loanMemory : Memory loanLowering testProfile :=
  ⟨tabProject, Exec.Demo.mainF, [],
    [(Exec.Demo.mainF, [(storeRoot, .inr false)], ⟨⟨0⟩, #[]⟩)],
    [], [(Exec.Demo.mainF, [(storeRoot, .virtual)])], [], []⟩

private def loanRoot : Hybrid.Place := ⟨0⟩

private def moveBeforeBorrow : MemoryRegisters loanFieldLanguage testProfile :=
  { frame := Exec.Demo.mainF
    block := ⟨0⟩
    instruction := 0
    live := [storeRoot]
    initialized := [storeRoot]
    virtualValues := [(storeRoot, .scalar (.int .u8 42))]
    materialized := []
    bytes := Mem.Memory.empty
    addressIds := [(storeRoot, loanRoot)]
    nextAddressId := 1
    ownership := {
      slots := [⟨loanRoot, 0, ⟨.int .u8, false, true⟩, .owned⟩]
      frames := [⟨0, [], []⟩]
      nextPlace := 1
      nextActivation := 1 } }

private def liveFieldLoan (mode : Own.Mode := .shared) :
    Option (MemoryRegisters loanFieldLanguage testProfile × MemoryRegisters loanFieldLanguage testProfile) := do
  let (_, moved) ← loanMemory.readValue moveBeforeBorrow
    (.move (ty := .inr false) ⟨⟨storeRoot, [splitField]⟩, .virtual⟩)
  let address ← loanMemory.resolveAddress moved true
    ⟨.inl (.scalar (.int .u8)), ⟨⟨storeRoot, [secondSplitField]⟩, .virtual⟩⟩
  let borrowed ← Own.borrow moved.ownership address.provenance
    (address.fields.map Own.Projection.field) mode
  let released ← Own.endLoan borrowed ⟨0⟩
  return ({moved with ownership := borrowed}, {moved with ownership := released})

-- The only linear field moves out. A loan of the surviving Copy field
-- keeps the root alive, and releasing that loan permits the same end.
set_option maxRecDepth 4096 in
example : ∃ borrowed released, liveFieldLoan = some (borrowed, released) ∧
    (∀ next, ¬ loanMemory.Ends borrowed storeRoot next) ∧
    ∃ next, loanMemory.Ends released storeRoot next := by
  refine ⟨_, _, rfl, ?_, ?_⟩
  · intro next ended
    have h := ended.1.2
    change false = true at h
    contradiction
  · exact ⟨_, ⟨List.mem_cons_self .., rfl⟩, Or.inl ⟨rfl, rfl⟩⟩

-- Both modes exclude owner writes. Only a mutable loan excludes owner copies.
private def ownerFieldAccess (mode : Own.Mode) : Option (Bool × Bool × Bool × Bool) := do
  let (borrowed, released) ← liveFieldLoan mode
  let place : MemoryPlace loanFieldLanguage.operands.Ty (.inl (.scalar (.int .u8))) :=
    ⟨⟨storeRoot, [secondSplitField]⟩, .virtual⟩
  let write state := (loanMemory.writeValue state place (.scalar (.int .u8 25))).isSome
  let copy state := (loanMemory.readValue state (.copy place)).isSome
  return (write borrowed, copy borrowed, write released, copy released)

set_option maxRecDepth 4096 in
example : ownerFieldAccess .shared = some (false, true, true, true) ∧
    ownerFieldAccess .mutable = some (false, false, true, true) :=
  ⟨by with_unfolding_all rfl, by with_unfolding_all rfl⟩

private def partialOwnerOverwrite (mutable : Bool) : Option (Bool × Bool × Bool × Bool) := do
  let initial := {moveBeforeBorrow with ownership := {moveBeforeBorrow.ownership with
    slots := [⟨loanRoot, 0, ⟨.int .u8, false, mutable⟩, .owned⟩]}}
  let (_, moved) ← loanMemory.readValue initial
    (.move (ty := .inr false) ⟨⟨storeRoot, [splitField]⟩, .virtual⟩)
  let field : MemoryPlace loanFieldLanguage.operands.Ty (.inl (.scalar (.int .u8))) :=
    ⟨⟨storeRoot, [secondSplitField]⟩, .virtual⟩
  let root : MemoryPlace loanFieldLanguage.operands.Ty (.inr false) :=
    ⟨⟨storeRoot, []⟩, .virtual⟩
  let surviving := (loanMemory.readValue moved (.copy field)).isSome
  let direct := (loanMemory.writeValue moved field (.scalar (.int .u8 42))).isSome
  let whole := (loanMemory.writeValue moved root (.scalar (.int .u8 42))).isSome
  let realOwner := Own.ownerAccess moved.ownership loanRoot .mutable
  return (surviving, direct, whole, realOwner)

set_option maxRecDepth 4096 in
example : partialOwnerOverwrite false = some (true, false, false, false) ∧
    partialOwnerOverwrite true = some (true, true, true, true) :=
  ⟨by with_unfolding_all rfl, by with_unfolding_all rfl⟩

-- All moved descendants are cleared on replacement. A surviving linear
-- sibling still prevents an overwrite.
example :
    let memory := (partialMIR [secondSplitField]).logicalView (language := twoFieldLanguage)
    let root : MemoryPlace Unit () := ⟨⟨storeRoot, []⟩, .virtual⟩
    memory.writeValue firstMoved root (.other ()) = none ∧
    (memory.writeValue bothMoved root (.other ())).map (·.movedFields) = some [] :=
  ⟨rfl, rfl⟩

private def enumDecl : DeclId := ⟨Exec.Demo.mainF.module, ⟨"choice"⟩⟩
private def enumMoved : FieldId := ⟨.variant ⟨enumDecl, ⟨"left"⟩⟩, ⟨"linear"⟩⟩
private def enumSurviving : FieldId := ⟨.variant ⟨enumDecl, ⟨"left"⟩⟩, ⟨"copy"⟩⟩
private def enumInactive : FieldId := ⟨.variant ⟨enumDecl, ⟨"right"⟩⟩, ⟨"other"⟩⟩

private abbrev enumFieldLanguage : CoreLanguage :=
  {loanFieldLanguage with
    fieldType := fun _ field =>
      if field = enumMoved then some (.inr false)
      else if field == enumSurviving || field == enumInactive then some (.inl (.scalar (.int .u8))) else none
    activeFields := fun _ _ _ => some [enumMoved, enumSurviving]
    field := fun value path =>
      if path = [enumMoved] then some (.scalar (.int .u8 17))
      else if path = [enumSurviving] then some value else none}

private def enumState (n : Nat) : MemoryRegisters enumFieldLanguage testProfile :=
  {moveBeforeBorrow with
    virtualValues := [(storeRoot, .scalar (.int .u8 n))]
    movedFields := [(storeRoot, [enumMoved])]}

private def enumLogical : LogicalFrame enumFieldLanguage.operands.Val :=
  ⟨Exec.Demo.mainF, [storeRoot], (enumState 25).virtualValues, (enumState 25).movedFields⟩

-- The old range of the representation obligation included this initialized
-- type-valid path, although the selected variant cannot supply a value there.
example : (enumFieldLanguage.fieldType (.inr false) enumInactive).isSome = true ∧
    Memory.initializedAt (enumState 25) storeRoot [enumInactive] = true ∧
    enumFieldLanguage.field (.scalar (.int .u8 25)) [enumInactive] = none :=
  ⟨rfl, rfl, rfl⟩

-- The alternate variant's field has a type but no value. The active surviving
-- field still has to agree with the logical value.
set_option maxRecDepth 4096 in
example : loanMemory.Represents (language := enumFieldLanguage) enumLogical (enumState 25) ∧
    ¬ loanMemory.Represents (language := enumFieldLanguage) enumLogical (enumState 24) := by
  constructor
  · refine ⟨⟨⟨Mem.empty_wellFormed _, ?_, ?_⟩, rfl⟩, rfl, fun _ h => h,
      ?_, ?_, ?_, ?_, nofun, ?_, ?_, ?_, nofun, ?_⟩
    · change [storeRoot].Nodup
      simp
    · change [storeRoot].Nodup ∧ ∀ place ∈ [storeRoot], place ∈ [storeRoot]
      exact ⟨by simp, fun _ h => h⟩
    · change [storeRoot].Nodup
      simp
    · exact List.nodup_nil
    · change [storeRoot].Nodup
      simp
    · exact List.nodup_nil
    · intro place
      change place ∈ [storeRoot] ↔
        (AList.lookup ([(storeRoot, CoreValue.scalar (.int .u8 25))] :
          AList PlaceId (CoreValue DeclId)) place).isSome = true
      by_cases eq : place = storeRoot
      · subst place
        exact ⟨fun _ => rfl, fun _ => List.mem_cons_self ..⟩
      · simp [AList.lookup, eq, Ne.symm eq]
    · intro place value member
      have eq : place = storeRoot := congrArg Prod.fst (List.mem_singleton.mp member)
      cases eq
      exact List.mem_cons_self ..
    · intro place value member
      have eq : place = storeRoot := congrArg Prod.fst (List.mem_singleton.mp member)
      cases eq
      exact ⟨List.mem_cons_self .., rfl⟩
    · intro place member
      obtain rfl := List.mem_singleton.mp member
      refine ⟨_, _, .inr false, .virtual, List.mem_cons_self .., rfl, rfl, nofun, ?_⟩
      intro value found
      have eq : CoreValue.scalar (.int .u8 25) = value := Option.some.inj found
      cases eq
      intro path ty field live typed active
      change (if path = [enumMoved] then some (.scalar (.int .u8 17))
        else if path = [enumSurviving] then some (.scalar (.int .u8 25)) else none) =
        some field at active
      split at active
      · rename_i moved
        subst path
        have impossible : false = true := live
        cases impossible
      · split at active
        · rename_i surviving
          subst path
          have same : some (.inl (.scalar (.int .u8))) = some ty := typed
          obtain rfl := Option.some.inj same
          obtain rfl := Option.some.inj active
          exact ⟨_, rfl, rfl, nofun⟩
        · contradiction
  · intro h
    have fields := h.2.2.2.2.2.2.2.2.2.2.2.2 storeRoot (List.mem_cons_self ..)
    obtain ⟨locals, graph, ty, storage, mem, lookup, storageClass, _, related⟩ := fields
    obtain eq := List.mem_singleton.mp mem
    cases eq
    obtain rfl := Option.some.inj lookup
    obtain rfl := Option.some.inj storageClass
    obtain ⟨physical, rfl, loaded, _⟩ := related (.scalar (.int .u8 25)) rfl
      [enumSurviving] (.inl (.scalar (.int .u8))) (.scalar (.int .u8 25)) rfl rfl rfl
    cases loaded

private def pairLayout : Layout :=
  ⟨2, 1, .cons splitField 0 (.scalar 1 1) (.cons secondSplitField 1 (.scalar 1 1) .nil)⟩

private abbrev physicalFieldLanguage : CoreLanguage :=
  {linearProjectedLanguage with layout := fun _ _ _ => some pairLayout}

private def physicalFields : MemoryRegisters physicalFieldLanguage testProfile :=
  {storeInitial with
    live := [storeRoot]
    initialized := [storeRoot]
    materialized := [(storeRoot, {storeRange with extent := 2})]
    bytes := ⟨[(⟨0⟩, {storeBlock with size := 2, bytes := [.init 17 none, .init 25 none]})], 1⟩}

private def physicalFieldLoads : List (LocatedOp (SSAInst testLowering)) :=
  [SSAInst.load (ty := ()) ⟨0⟩ ⟨storeRoot, [splitField]⟩,
    SSAInst.load (ty := ()) ⟨1⟩ ⟨storeRoot, [secondSplitField]⟩].map (LocatedOp.mk testOrigin)

private def physicalFieldSSA : SSA testLowering testProfile :=
  ⟨tabProject, Exec.Demo.mainF, [],
    [(Exec.Demo.mainF, [(storeRoot, ())], ⟨⟨0⟩, #[⟨⟨0⟩, [],
      physicalFieldLoads, .ret none, []⟩]⟩)],
    [(Exec.Demo.mainF, [(storeRoot, pairLayout)])], [], []⟩

private def physicalFieldBIR : BIR testLowering testProfile :=
  ⟨tabProject, Exec.Demo.mainF, [],
    [(Exec.Demo.mainF, [(storeRoot, ())], ⟨⟨0⟩, #[⟨⟨0⟩, [], physicalFieldLoads, .ret none, []⟩]⟩)],
    physicalFieldSSA.layouts, #[], [], []⟩

-- Both register stages consume a projected linear load through the shared
-- byte interpreter. The sibling remains readable from its separate range.
set_option maxRecDepth 4096 in
example :
    let memory := physicalFieldBIR.memoryView (language := physicalFieldLanguage)
    let first : MemoryRegisters physicalFieldLanguage testProfile :=
      {physicalFields with movedFields := [(storeRoot, [splitField])]}
    memory.readValue physicalFields (.move (ty := ()) ⟨⟨storeRoot, [splitField]⟩, .materialized⟩) =
      some (.scalar (.int .u8 17), first) ∧
    memory.readValue first (.move (ty := ()) ⟨⟨storeRoot, [splitField]⟩, .materialized⟩) = none ∧
    memory.readValue first (.move (ty := ()) ⟨⟨storeRoot, [secondSplitField]⟩, .materialized⟩) =
      some (.scalar (.int .u8 25), {first with
        movedFields := (storeRoot, [secondSplitField]) :: first.movedFields}) ∧
    ∃ next,
      (SSA.Execution (language := physicalFieldLanguage)).step physicalFieldSSA physicalFields next ∧
      (BIR.Execution (language := physicalFieldLanguage)).step physicalFieldBIR physicalFields next ∧
      next.movedFields = first.movedFields := by
  let memory := physicalFieldBIR.memoryView (language := physicalFieldLanguage)
  let first : MemoryRegisters physicalFieldLanguage testProfile :=
    {physicalFields with movedFields := [(storeRoot, [splitField])]}
  let place : MemoryPlace Unit () := ⟨⟨⟨storeRoot.id + 1⟩, []⟩, .virtual⟩
  let ready : MemoryRegisters physicalFieldLanguage testProfile :=
    {first with live := place.logical.root :: first.live}
  let stored : MemoryRegisters physicalFieldLanguage testProfile :=
    {ready with
      initialized := place.logical.root :: first.initialized
      virtualValues := [(place.logical.root, .scalar (.int .u8 17))]}
  have tick : memory.InstructionStepWith physicalFields stored.advance false true
      (.bind place (.move ⟨⟨storeRoot, [splitField]⟩, .materialized⟩)) := by
    with_unfolding_all
      refine ⟨.scalar (.int .u8 17), first, stored, rfl, ?_, rfl⟩
    apply Memory.WriteParams.cons (started := ready)
    · exact ⟨nofun, Or.inl ⟨rfl, rfl⟩⟩
    · exact ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩
    · exact .nil _
  with_unfolding_all
    exact ⟨rfl, rfl, rfl, stored.advance,
      ⟨rfl, ⟨_, rfl, tick⟩, [], rfl⟩, ⟨rfl, ⟨_, rfl, tick⟩, [], rfl⟩, rfl⟩

private def splitRegisters : List (FieldId × Sigma (SSAValue Unit)) :=
  [(splitField, ⟨(), ⟨0⟩⟩), (secondSplitField, ⟨(), ⟨1⟩⟩)]

private def splitSSA (project : Project) (dropSecond : Bool := false) : SSA testLowering testProfile :=
  ⟨project, Exec.Demo.mainF, [],
    [(Exec.Demo.mainF, [(storeRoot, ())], ⟨⟨0⟩, #[⟨⟨0⟩, [],
      [⟨testOrigin, .destructure (.materialized (ty := ()) ⟨storeRoot, []⟩)
        (if dropSecond then splitRegisters.take 1 else splitRegisters)⟩], .ret none, []⟩]⟩)],
    [(Exec.Demo.mainF, [(storeRoot, ⟨1, 1, .nil⟩)])], [], []⟩

private def splitBIR (project : Project) : BIR testLowering testProfile :=
  ⟨project, Exec.Demo.mainF, [],
    [(Exec.Demo.mainF, [(storeRoot, ())], ⟨⟨0⟩, #[⟨⟨0⟩, [],
      [⟨testOrigin, .destructure (.materialized (ty := ()) ⟨storeRoot, []⟩)
        splitRegisters⟩], .ret none, []⟩]⟩)],
    [(Exec.Demo.mainF, [(storeRoot, ⟨1, 1, .nil⟩)])], #[], [], []⟩

private def splitBefore : MemoryRegisters twoFieldLanguage testProfile :=
  {backingAfter ⟨0⟩ 1 with instruction := 0}

private def splitAfter : MemoryRegisters twoFieldLanguage testProfile :=
  {splitBefore with
    instruction := 1
    live := [⟨2⟩, ⟨1⟩, storeRoot]
    initialized := [⟨2⟩, ⟨1⟩]
    virtualValues := [(⟨2⟩, .scalar (.int .u8 25)), (⟨1⟩, .scalar (.int .u8 17))]}

private def fieldRegister (id : Nat) : MemoryPlace Unit () := ⟨⟨⟨id⟩, []⟩, .virtual⟩

private def splitBlock : MemoryBlock testLowering := ⟨⟨0⟩, [],
  [⟨testOrigin, .destructure storePlace
    [(splitField, ⟨(), fieldRegister 1⟩), (secondSplitField, ⟨(), fieldRegister 2⟩)]⟩], .ret none, []⟩

private def splitMemory (project : Project) : Memory testLowering testProfile :=
  ⟨project, Exec.Demo.mainF, [],
    [(Exec.Demo.mainF, [(storeRoot, ()), (⟨1⟩, ()), (⟨2⟩, ())], ⟨⟨0⟩, #[splitBlock]⟩)],
    [(Exec.Demo.mainF, [(storeRoot, ⟨1, 1, .nil⟩)])],
    [(Exec.Demo.mainF, [(storeRoot, .materialized), (⟨1⟩, .virtual), (⟨2⟩, .virtual)])],
    [(Exec.Demo.mainF, [⟨1⟩, ⟨2⟩])], []⟩

private theorem splitShape :
    Memory.DestructureFields twoFieldLanguage [] () (.other ())
      [(splitField, ()), (secondSplitField, ())] := by
  refine ⟨by decide, [splitField, secondSplitField], rfl, ?_, ?_⟩
  · intro binding member
    simp only [List.mem_cons, List.not_mem_nil, or_false] at member
    rcases member with rfl | rfl
    · exact ⟨List.mem_cons_self .., rfl⟩
    · exact ⟨List.mem_cons_of_mem _ (List.mem_cons_self ..), rfl⟩
  · exact fun _ member missing => False.elim (missing member)

set_option maxRecDepth 3072 in
set_option maxHeartbeats 1000000 in
example (project : Project) :
    (splitMemory project).Step (language := twoFieldLanguage) splitBefore splitAfter ∧
    (SSA.Execution (language := twoFieldLanguage)).step (splitSSA project) splitBefore splitAfter ∧
    (BIR.Execution (language := twoFieldLanguage)).step (splitBIR project) splitBefore splitAfter ∧
    (splitMemory project).readValue (language := twoFieldLanguage) splitAfter (.move storePlace) = none ∧
    (splitMemory project).readValue (language := twoFieldLanguage) splitAfter (.move (fieldRegister 1)) =
      some (.scalar (.int .u8 17), {splitAfter with
        initialized := [⟨2⟩]
        virtualValues := [(⟨2⟩, .scalar (.int .u8 25))]}) := by
  have tick : (splitMemory project).InstructionStepWith (language := twoFieldLanguage) splitBefore splitAfter
      false false (.destructure storePlace
        [(splitField, ⟨(), fieldRegister 1⟩), (secondSplitField, ⟨(), fieldRegister 2⟩)]) := by
    refine ⟨.other (), {splitBefore with initialized := []},
      [.scalar (.int .u8 17), .scalar (.int .u8 25)], {splitAfter with instruction := 0},
      by with_unfolding_all rfl, splitShape, by with_unfolding_all rfl, ?_, rfl⟩
    let afterRead : MemoryRegisters twoFieldLanguage testProfile := {splitBefore with initialized := []}
    let first : MemoryRegisters twoFieldLanguage testProfile := {afterRead with
      live := [⟨1⟩, storeRoot]
      initialized := [⟨1⟩]
      virtualValues := [(⟨1⟩, .scalar (.int .u8 17))]}
    apply Memory.WriteParams.cons (started := {afterRead with live := [⟨1⟩, storeRoot]}) (stored := first)
    · with_unfolding_all exact ⟨nofun, Or.inl ⟨rfl, rfl⟩⟩
    · with_unfolding_all exact ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩
    · apply Memory.WriteParams.cons (started := {first with live := [⟨2⟩, ⟨1⟩, storeRoot]})
      · with_unfolding_all exact ⟨nofun, Or.inl ⟨rfl, rfl⟩⟩
      · with_unfolding_all exact ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩
      · exact .nil _
  let block : MemoryBlock testLowering := ⟨⟨0⟩, [],
    [⟨testOrigin, .destructure storePlace
      [(splitField, ⟨(), fieldRegister 1⟩), (secondSplitField, ⟨(), fieldRegister 2⟩)]⟩], .ret none, []⟩
  refine ⟨⟨block, by with_unfolding_all rfl, by with_unfolding_all exact tick⟩, ?_, ?_, ?_, ?_⟩
  · exact ⟨by with_unfolding_all rfl, ⟨block, by with_unfolding_all rfl, by with_unfolding_all exact tick⟩,
      [], by with_unfolding_all rfl⟩
  · exact ⟨by with_unfolding_all rfl, ⟨block, by with_unfolding_all rfl, by with_unfolding_all exact tick⟩,
      [], by with_unfolding_all rfl⟩
  · with_unfolding_all rfl
  · with_unfolding_all rfl


private abbrev copySplitLanguage : CoreLanguage :=
  {twoFieldLanguage with
    operands := {twoFieldLanguage.operands with isCopy := fun _ => true}
    safety := fun _ => []}

private def copySplitGraph {T : Type} (ending : T) :
    CFG (Sigma (SSAValue Unit)) (LocatedOp (SSAInst testLowering)) T :=
  ⟨⟨0⟩, #[⟨⟨0⟩, [⟨(), ⟨0⟩⟩],
    [⟨testOrigin, .destructure (.register (ty := ()) ⟨0⟩)
      [(splitField, ⟨(), ⟨1⟩⟩), (secondSplitField, ⟨(), ⟨2⟩⟩)]⟩], ending, []⟩]⟩

private def copySplitSSA : SSA testLowering testProfile :=
  ⟨tabProject, Exec.Demo.mainF, [],
    [(Exec.Demo.mainF, [], copySplitGraph (.ret none))], [], [], []⟩

private def copySplitBIR : BIR testLowering testProfile :=
  ⟨tabProject, Exec.Demo.mainF, [],
    [(Exec.Demo.mainF, [], copySplitGraph (.ret none))], [], #[], [], []⟩

private def copySplitMIR : MIR testLowering testProfile :=
  let block : Block _ _ _ := ⟨⟨0⟩, [⟨(), ⟨storeRoot, []⟩⟩],
    [⟨testOrigin, .destructure (ty := ()) ⟨storeRoot, []⟩
      [(splitField, ⟨(), ⟨⟨1⟩, []⟩⟩), (secondSplitField, ⟨(), ⟨⟨2⟩, []⟩⟩)]⟩], .ret none, []⟩
  ⟨tabProject, Exec.Demo.mainF, [],
    [(Exec.Demo.mainF, [(storeRoot, ()), (⟨1⟩, ()), (⟨2⟩, ())], ⟨⟨0⟩, #[block]⟩)],
    [(Exec.Demo.mainF, [storeRoot, ⟨1⟩, ⟨2⟩])], []⟩

set_option maxRecDepth 4096 in
example :
    let memory := copySplitMIR.logicalView (language := copySplitLanguage)
    let before : MemoryRegisters copySplitLanguage testProfile :=
      {storeInitial with
        live := [storeRoot]
        initialized := [storeRoot]
        virtualValues := [(storeRoot, .other ())]}
    ∃ next, copySplitMIR.Step before next ∧ memory.Step before next ∧
      (SSA.Execution (language := copySplitLanguage)).step copySplitSSA before next ∧
      (BIR.Execution (language := copySplitLanguage)).step copySplitBIR before next ∧
      memory.readValue next (.copy (fieldRegister 0)) = some (.other (), next) ∧
      memory.readValue next (.copy (fieldRegister 1)) = some (.scalar (.int .u8 17), next) ∧
      memory.readValue next (.copy (fieldRegister 2)) = some (.scalar (.int .u8 25), next) := by
  intro memory before
  let first : MemoryRegisters copySplitLanguage testProfile :=
    {before with
      live := [⟨1⟩, storeRoot]
      initialized := [⟨1⟩, storeRoot]
      virtualValues := (⟨1⟩, .scalar (.int .u8 17)) :: before.virtualValues}
  let stored : MemoryRegisters copySplitLanguage testProfile :=
    {first with
      live := ⟨2⟩ :: first.live
      initialized := ⟨2⟩ :: first.initialized
      virtualValues := (⟨2⟩, .scalar (.int .u8 25)) :: first.virtualValues}
  let dests : List (FieldId × Sigma (MemoryPlace Unit)) :=
    [(splitField, ⟨(), fieldRegister 1⟩), (secondSplitField, ⟨(), fieldRegister 2⟩)]
  have tick : memory.InstructionStepWith before stored.advance false false
      (.destructure (fieldRegister 0) dests) := by
    refine ⟨.other (), before, [.scalar (.int .u8 17), .scalar (.int .u8 25)], stored,
      by with_unfolding_all rfl, ?_, by with_unfolding_all rfl, ?_, rfl⟩
    · exact ⟨splitShape.1, splitShape.2.choose, splitShape.2.choose_spec.1,
        splitShape.2.choose_spec.2.1, fun _ _ _ => ⟨(), rfl, rfl⟩⟩
    · apply Memory.WriteParams.cons (started := {before with live := first.live}) (stored := first)
      · with_unfolding_all exact ⟨nofun, Or.inl ⟨rfl, rfl⟩⟩
      · with_unfolding_all exact ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩
      · apply Memory.WriteParams.cons (started := {first with live := stored.live})
        · with_unfolding_all exact ⟨nofun, Or.inl ⟨rfl, rfl⟩⟩
        · with_unfolding_all exact ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩
        · exact .nil _
  with_unfolding_all
    exact ⟨stored.advance, ⟨rfl, tick⟩, ⟨_, rfl, tick⟩, ⟨rfl, ⟨_, rfl, tick⟩, [], rfl⟩,
      ⟨rfl, ⟨_, rfl, tick⟩, [], rfl⟩, rfl, rfl, rfl⟩

-- Each field register is consumed by its first move, even though the original
-- backing bytes remain allocated until the source root's lifetime ends.
set_option maxRecDepth 2048 in
example (project : Project) :
    let memory := splitMemory project
    let args : List (Sigma (MemoryRead Unit (Literal Unit))) :=
      [⟨(), .move (fieldRegister 1)⟩, ⟨(), .move (fieldRegister 2)⟩]
    let consumed : MemoryRegisters twoFieldLanguage testProfile :=
      {splitAfter with initialized := [], virtualValues := []}
    memory.readOperands (language := twoFieldLanguage) args splitAfter =
      some ([.scalar (.int .u8 17), .scalar (.int .u8 25)], consumed) ∧
    memory.readOperands (language := twoFieldLanguage) args consumed = none := by
  with_unfolding_all exact ⟨rfl, rfl⟩

-- A consuming split cannot omit a linear field.
set_option maxRecDepth 2048 in
example (project : Project) :
    ∀ next, ¬ (SSA.Execution (language := twoFieldLanguage)).step
      (splitSSA project true) splitBefore next := by
  intro next execution
  obtain ⟨_, _, block, fetch, tick⟩ := Memory.Step.exists_outcome execution.2.1
  let expected : MemoryBlock testLowering := ⟨⟨0⟩, [],
    [⟨testOrigin, .destructure storePlace [(splitField, ⟨(), fieldRegister 1⟩)]⟩], .ret none, []⟩
  have exactBlock : some expected = some block := by with_unfolding_all exact fetch
  obtain rfl := Option.some.inj exactBlock
  obtain ⟨_, _, _, _, _, shape, _, _, _⟩ := tick
  obtain ⟨_, fields, active, _, omitted⟩ := shape
  have same : some [splitField, secondSplitField] = some fields := active
  cases same
  obtain ⟨_, _, copy⟩ := omitted secondSplitField
    (List.mem_cons_of_mem _ (List.mem_cons_self ..)) (by decide)
  exact Bool.noConfusion copy

private def fullSplitBody (dropSecond : Bool := false) : List (LocatedOp (SSAInst testLowering)) :=
  let split : SSAInst testLowering := .destructure (.materialized (ty := ()) ⟨storeRoot, []⟩)
    (if dropSecond then splitRegisters.take 1 else splitRegisters)
  let sum : SSAInst testLowering := .op (opcode := .named ⟨"sumFields"⟩) (types := [(), ()]) (ty := ()) ⟨2⟩ ()
    (.cons (.value ⟨0⟩) (.cons (.value ⟨1⟩) .nil))
  [.beginLifetime storeRoot, .store (ty := ()) ⟨storeRoot, []⟩ (.literal ⟨[]⟩), split,
    .endLifetime storeRoot, sum].map (LocatedOp.mk testOrigin)

private def fullSplitReturn : Option (Sigma (SSARead Unit testLowering.literal)) :=
  some ⟨(), .value ⟨2⟩⟩

private def fullSplitBIR (project : Project) (dropSecond : Bool := false) : BIR testLowering testProfile :=
  let block : Block _ _ _ := ⟨⟨0⟩, [], fullSplitBody dropSecond, .ret fullSplitReturn, []⟩
  {(splitBIR project) with
    functions := [(Exec.Demo.mainF, [(storeRoot, ())], ⟨⟨0⟩, #[block]⟩)]}

private def fullSplitSSA (project : Project) : SSA testLowering testProfile :=
  let block : Block _ _ _ := ⟨⟨0⟩, [], fullSplitBody, .ret fullSplitReturn, []⟩
  {(splitSSA project) with
    functions := [(Exec.Demo.mainF, [(storeRoot, ())], ⟨⟨0⟩, #[block]⟩)]}

private def fullSplitMemory (project : Project) : Memory testLowering testProfile :=
  let memory := (fullSplitBIR project).memoryView (language := twoFieldLanguage)
  let body : List (MemoryInst testLowering) :=
    [.beginLifetime storeRoot, .assign storePlace (.literal ⟨[]⟩),
      .destructure storePlace [(splitField, ⟨(), fieldRegister 1⟩),
        (secondSplitField, ⟨(), fieldRegister 2⟩)],
      .endLifetime storeRoot, .beginLifetime ⟨3⟩,
      .op (opcode := .named ⟨"sumFields"⟩) (fieldRegister 3) ()
        (.cons (.move (fieldRegister 1)) (.cons (.move (fieldRegister 2)) .nil))]
  let block : MemoryBlock testLowering :=
    ⟨⟨0⟩, [], body.map (LocatedOp.mk testOrigin), .ret (some ⟨(), .move (fieldRegister 3)⟩), []⟩
  {memory with
    functions := [(Exec.Demo.mainF, [(storeRoot, ()), (⟨1⟩, ()), (⟨2⟩, ()), (⟨3⟩, ())],
      ⟨⟨0⟩, #[block]⟩)]}

-- Starting with an empty machine, BIR allocates and stores the aggregate,
-- transfers its two linear fields, frees the aggregate and consumes both
-- fields to return 42.
set_option maxRecDepth 4096 in
set_option maxHeartbeats 1600000 in
private theorem fullSplitReturns (project : Project) :
    (BIR.Execution (language := twoFieldLanguage)).observes (fullSplitBIR project)
      (.returned 42 ByteArray.empty) ∧
    (∃ start finish : MemoryRegisters twoFieldLanguage testProfile,
      (fullSplitMemory project).Initial start ∧
      Concrete.Contracts.Steps (fullSplitMemory project).Step start finish ∧
      (fullSplitMemory project).terminalObservation finish = some (.returned 42 ByteArray.empty)) := by
  let bir := fullSplitBIR project
  let exec := BIR.Execution (language := twoFieldLanguage) (lang := testLowering) (profile := testProfile)
  let start := BIR.entryState (language := twoFieldLanguage) bir
  let allocated : MemoryRegisters twoFieldLanguage testProfile := backingBefore ⟨0⟩ 1
  let bound : MemoryRegisters twoFieldLanguage testProfile := backingAfter ⟨0⟩ 1
  let split : MemoryRegisters twoFieldLanguage testProfile := {splitAfter with instruction := 3}
  let freed := (split.bytes.free ⟨0⟩).getD Mem.Memory.empty
  let released : MemoryRegisters twoFieldLanguage testProfile :=
    {split with
      instruction := 4
      bytes := freed
      live := [⟨2⟩, ⟨1⟩]
      materialized := []}
  let finished : MemoryRegisters twoFieldLanguage testProfile :=
    {released with
      instruction := 5
      live := [⟨3⟩, ⟨2⟩, ⟨1⟩]
      initialized := [⟨3⟩]
      virtualValues := [(⟨3⟩, .scalar (.int .u8 42))]}
  have allocate : exec.step bir start allocated := by
    with_unfolding_all
      refine ⟨rfl, ⟨_, rfl, {allocated with instruction := 0},
        ⟨nofun, Or.inr ⟨rfl, ⟨1, 1, .nil⟩, rfl, Or.inr ?_⟩⟩, rfl⟩, [], rfl⟩
      exact ⟨by decide, ⟨0⟩, 1, ⟨0⟩, allocated.bytes, rfl, rfl⟩
  have bind : exec.step bir allocated bound := by
    with_unfolding_all
      refine ⟨rfl, ⟨_, rfl, .other (), allocated, {bound with instruction := 1}, rfl, ?_, rfl⟩, [], rfl⟩
      refine ⟨⟨rfl, nofun⟩, fun _ => ⟨⟨1, 1, .nil⟩, rfl, ⟨nofun, nofun⟩⟩, rfl, rfl, ?_⟩
      intro _
      refine Or.inr ⟨backingRange ⟨0⟩ 1, [(0, [.init 42 none])], rfl, rfl,
        fun _ => ⟨0, [.init 42 none], List.mem_singleton_self _, nofun⟩, ?_⟩
      intro offset bytes member
      cases List.mem_singleton.mp member
      exact ⟨by decide, by simp, rfl⟩
  have moveFields : exec.step bir bound split := by
    let afterRead : MemoryRegisters twoFieldLanguage testProfile := {bound with initialized := []}
    let first : MemoryRegisters twoFieldLanguage testProfile := {afterRead with
      live := [⟨1⟩, storeRoot]
      initialized := [⟨1⟩]
      virtualValues := [(⟨1⟩, .scalar (.int .u8 17))]}
    with_unfolding_all
      refine ⟨rfl, ⟨_, rfl, .other (), afterRead,
        [.scalar (.int .u8 17), .scalar (.int .u8 25)], {split with instruction := 2},
        rfl, splitShape, rfl, ?_, rfl⟩, [], rfl⟩
    apply Memory.WriteParams.cons (started := {afterRead with live := [⟨1⟩, storeRoot]}) (stored := first)
    · with_unfolding_all exact ⟨nofun, Or.inl ⟨rfl, rfl⟩⟩
    · with_unfolding_all exact ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩
    · apply Memory.WriteParams.cons (started := {first with live := [⟨2⟩, ⟨1⟩, storeRoot]})
      · with_unfolding_all exact ⟨nofun, Or.inl ⟨rfl, rfl⟩⟩
      · with_unfolding_all exact ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩
      · exact .nil _
  have release : exec.step bir split released := by
    with_unfolding_all
      exact ⟨rfl, ⟨_, rfl, {released with instruction := 3},
        ⟨⟨by decide, rfl⟩,
          Or.inr ⟨rfl, Or.inr ⟨backingRange ⟨0⟩ 1, ⟨0⟩, freed, rfl, rfl, rfl, rfl⟩⟩⟩,
        rfl⟩, [], rfl⟩
  have add : exec.step bir released finished := by
    let read : MemoryRegisters twoFieldLanguage testProfile :=
      {released with initialized := [], virtualValues := []}
    let stored : MemoryRegisters twoFieldLanguage testProfile := {finished with instruction := 4}
    with_unfolding_all
      refine ⟨rfl, ⟨_, rfl, [.scalar (.int .u8 17), .scalar (.int .u8 25)], read,
        [], .scalar (.int .u8 42), [], Memory.operationStore read false, stored,
        rfl, rfl, rfl, ⟨rfl, rfl, rfl, rfl, rfl, rfl⟩, rfl, ?_, rfl⟩, [], rfl⟩
    apply Memory.WriteParams.cons (started := {read with live := [⟨3⟩, ⟨2⟩, ⟨1⟩]})
    · with_unfolding_all exact ⟨nofun, Or.inl ⟨rfl, rfl⟩⟩
    · with_unfolding_all exact ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩
    · exact .nil _
  constructor
  · exact ⟨finished, ⟨start, rfl, .cons allocate (.cons bind (.cons moveFields
      (.cons release (.cons add (.nil _)))))⟩, by with_unfolding_all rfl⟩
  · let memory := fullSplitMemory project
    let ready : MemoryRegisters twoFieldLanguage testProfile :=
      {released with instruction := 5, live := [⟨3⟩, ⟨2⟩, ⟨1⟩]}
    let result : MemoryRegisters twoFieldLanguage testProfile := {finished with instruction := 6}
    have starts : memory.Initial start := by
      with_unfolding_all
        exact ⟨rfl, rfl, ⟨_, rfl, rfl⟩, ⟨_, rfl, rfl⟩, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩
    have memoryAllocate : memory.Step start allocated := by
      obtain ⟨_, ⟨block, found, tick⟩, _⟩ := allocate
      have found' : some (((bir.memoryView (language := twoFieldLanguage)).blockAt start).getD splitBlock) =
          some block := by with_unfolding_all exact found
      have equal := Option.some.inj found'
      subst block
      exact ⟨_, by with_unfolding_all rfl, by with_unfolding_all exact tick⟩
    have memoryBind : memory.Step allocated bound := by
      with_unfolding_all
        refine ⟨_, rfl, .other (), allocated, {bound with instruction := 1}, rfl, ?_, rfl⟩
        refine ⟨⟨rfl, nofun⟩, fun _ => ⟨⟨1, 1, .nil⟩, rfl, ⟨nofun, nofun⟩⟩, rfl, rfl, ?_⟩
        intro _
        refine Or.inr ⟨backingRange ⟨0⟩ 1, [(0, [.init 42 none])], rfl, rfl,
          fun _ => ⟨0, [.init 42 none], List.mem_singleton_self _, nofun⟩, ?_⟩
        intro offset bytes member
        cases List.mem_singleton.mp member
        exact ⟨by decide, by simp, rfl⟩
    have memoryMoveFields : memory.Step bound split := by
      let afterRead : MemoryRegisters twoFieldLanguage testProfile := {bound with initialized := []}
      let first : MemoryRegisters twoFieldLanguage testProfile := {afterRead with
        live := [⟨1⟩, storeRoot]
        initialized := [⟨1⟩]
        virtualValues := [(⟨1⟩, .scalar (.int .u8 17))]}
      with_unfolding_all
        refine ⟨_, rfl, .other (), afterRead,
          [.scalar (.int .u8 17), .scalar (.int .u8 25)], {split with instruction := 2},
          rfl, splitShape, rfl, ?_, rfl⟩
      apply Memory.WriteParams.cons (started := {afterRead with live := [⟨1⟩, storeRoot]}) (stored := first)
      · with_unfolding_all exact ⟨nofun, Or.inl ⟨rfl, rfl⟩⟩
      · with_unfolding_all exact ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩
      · apply Memory.WriteParams.cons (started := {first with live := [⟨2⟩, ⟨1⟩, storeRoot]})
        · with_unfolding_all exact ⟨nofun, Or.inl ⟨rfl, rfl⟩⟩
        · with_unfolding_all exact ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩
        · exact .nil _
    have memoryRelease : memory.Step split released := by
      obtain ⟨_, ⟨block, found, tick⟩, _⟩ := release
      have found' : some (((bir.memoryView (language := twoFieldLanguage)).blockAt split).getD splitBlock) =
          some block := by with_unfolding_all exact found
      have equal := Option.some.inj found'
      subst block
      exact ⟨_, by with_unfolding_all rfl, by with_unfolding_all exact tick⟩
    have openResult : memory.Step released ready := by
      with_unfolding_all exact ⟨_, rfl, {ready with instruction := 4},
        ⟨nofun, Or.inl ⟨rfl, rfl⟩⟩, rfl⟩
    have sum : memory.Step ready result := by
      let read : MemoryRegisters twoFieldLanguage testProfile :=
        {ready with initialized := [], virtualValues := []}
      with_unfolding_all
        exact ⟨_, rfl, [.scalar (.int .u8 17), .scalar (.int .u8 25)], read,
          [], .scalar (.int .u8 42), [], Memory.operationStore read false,
          {result with instruction := 5}, rfl, rfl, rfl, ⟨rfl, rfl, rfl, rfl, rfl, rfl⟩, rfl,
          ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩, rfl⟩
    exact ⟨start, result, starts,
      .cons memoryAllocate (.cons memoryBind (.cons memoryMoveFields
        (.cons memoryRelease (.cons openResult (.cons sum (.nil _)))))),
      by with_unfolding_all rfl⟩

set_option maxRecDepth 2048 in
example (project : Project) :
    (SSA.Execution (language := twoFieldLanguage)).observes (fullSplitSSA project)
      (.returned 42 ByteArray.empty) := by
  with_unfolding_all exact (fullSplitReturns project).1

-- The reachable aggregate witness also satisfies the function-preservation
-- check. Removing either binding changes the decoded program.
example (project : Project) :
    BuildsBIR (language := twoFieldLanguage) (fullSplitSSA project) (fullSplitBIR project) := by
  refine ⟨rfl, ?_, ?_, ?_, by change 3 ≤ 3; decide, ⟨rfl, rfl, by with_unfolding_all rfl⟩,
    BIR.refinesOfViewEq (by with_unfolding_all rfl)⟩
  · intro fn member block included slot trap
    cases List.mem_singleton.mp member
    cases List.mem_singleton.mp included
    cases trap
  · intro fn member block included kind origin trap
    cases List.mem_singleton.mp member
    cases List.mem_singleton.mp included
    cases trap
  · with_unfolding_all exact ⟨fun _ h => h, fun _ h => h⟩

set_option maxRecDepth 2048 in
example (project : Project) :
    ¬ BuildsBIR (language := twoFieldLanguage) (fullSplitSSA project) (fullSplitBIR project true) := by
  intro builds
  have bindings := congrArg (fun program => do
    let graph ← program
    let block ← graph.blocks[0]?
    let operation ← block.body[2]?
    let .destructure _ fields := operation.instruction | none
    pure fields.length) builds.2.2.2.2.2.1.2.2
  have wrong : some 2 = some 1 := by with_unfolding_all exact bindings
  cases wrong

private def returnSSA (project : Project) : SSA testLowering testProfile :=
  let block : Block _ _ _ := ⟨⟨0⟩, [], [], .ret (some ⟨(), .literal ⟨[0]⟩⟩), []⟩
  ⟨project, Exec.Demo.mainF, [], [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[block]⟩)], [], [], []⟩

-- The substituted project leaves the reached behavior and all the old
-- handoff checks unchanged. The project equality rejects it on this edge.
set_option maxRecDepth 4096 in
example (original forged : Project) (different : forged ≠ original) :
    let ssa := returnSSA original
    let bir := birReturn forged false
    (bir.TrapSlotsWF ∧ bir.TrapsMatch ssa ∧ PreservesOrigins ssa.origins bir.origins ∧
      bir.frameSlots ≤ testProfile.maxBIRSlots ∧ BIR.EntryMatches (language := testLanguage) ssa bir ∧
      BIR.PreservesProgram (language := testLanguage) ssa bir) ∧
    ¬ BuildsBIR (language := testLanguage) ssa bir := by
  refine ⟨⟨?_, ?_, ?_, (by decide : 0 ≤ 3), ⟨rfl, rfl, by with_unfolding_all rfl⟩, ?_⟩, ?_⟩
  · intro fn member block included index trap
    cases List.mem_singleton.mp member
    cases List.mem_singleton.mp included
    cases trap
  · intro fn member block included kind origin trap
    cases List.mem_singleton.mp member
    cases List.mem_singleton.mp included
    cases trap
  · exact ⟨nofun, nofun⟩
  · refine stationaryRefines _ _ _ _ storeInitial storeInitial
      (.returned 0 ByteArray.empty) (.returned 0 ByteArray.empty)
      (fun _ => by with_unfolding_all rfl) (fun _ => by with_unfolding_all rfl) ?_ ?_ ?_ ?_ ?_
    all_goals first
      | (intro o; with_unfolding_all exact ⟨fun h => (Option.some.inj h).symm, fun h => congrArg some h.symm⟩)
      | with_unfolding_all rfl
  · exact fun built => different built.1

private def callingSSA (kind : TrapKind) : SSA testLowering testProfile :=
  let entry : Block _ _ _ := ⟨⟨0⟩, [], [⟨testOrigin, .call callee none []⟩],
    .ret (some ⟨(), .literal ⟨[0]⟩⟩), []⟩
  let helper : Block _ _ _ := ⟨⟨0⟩, [], [], .trap kind testOrigin, []⟩
  ⟨tabProject, Exec.Demo.mainF, [(callee, ⟨[], none⟩)],
    [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[entry]⟩), (callee, [], ⟨⟨0⟩, #[helper]⟩)], [], [], []⟩

private abbrev memoryExecution := Memory.Execution (language := testLanguage)
  (lang := testLowering) (profile := testProfile)

private abbrev ssaExecution := SSA.Execution (language := testLanguage)
  (lang := testLowering) (profile := testProfile)

private def helperState : MemoryRegisters testLanguage testProfile :=
  {storeInitial with frame := callee, callers := [⟨storeInitial.advance.toMemoryFrame, none⟩]}

private def helperTrap (kind : TrapKind) : Concrete.Contracts.Observation :=
  .trapped 134 kind ⟨⟨testSite.id⟩, ⟨mainSourcePath.render.text, span.line, span.col⟩⟩
    (trapDiagnostic tabProject kind testSite span)

private theorem callAtEntry (kind : TrapKind) :
    memoryExecution.step ((callingSSA kind).memoryView (language := testLanguage)) storeInitial helperState := by
  with_unfolding_all
    exact ⟨rfl, ⟨_, rfl, rfl, [], _, _, _, rfl, rfl, rfl, rfl, .nil _⟩, _, rfl⟩

private theorem callOutcomeUnique {startDefinitions : Bool} (kind : TrapKind) (q : memoryExecution.State) (mode : Bool)
    (raw : ((callingSSA kind).memoryView (language := testLanguage)).StepOutcome storeInitial q mode startDefinitions) :
    q = helperState ∧ mode = false := by
  obtain ⟨block, found, call⟩ := raw
  let entry : MemoryBlock testLowering := ⟨⟨0⟩, [], [⟨testOrigin, .call callee none []⟩],
    .ret (some ⟨(), .literal ⟨[0]⟩⟩), []⟩
  have fetched : some entry = some block := by with_unfolding_all exact found
  have sameBlock := Option.some.inj fetched
  subst block
  change _ ∧ ((callingSSA kind).memoryView (language := testLanguage)).Call callee none [] storeInitial q mode at call
  obtain ⟨values, afterRead, graph, calleeEntry, read, graphFound, blockFound, _, params⟩ := call.2
  have read' : some ([], storeInitial) = some (values, afterRead) := read
  obtain ⟨rfl, rfl⟩ := Prod.mk.inj (Option.some.inj read')
  let helper : MemoryBlock testLowering := ⟨⟨0⟩, [], [], .trap kind testOrigin, []⟩
  have graphFound' : some ⟨⟨0⟩, #[helper]⟩ = some graph := by with_unfolding_all exact graphFound
  have sameGraph := Option.some.inj graphFound'
  subst graph
  have found' : some helper = some calleeEntry := by with_unfolding_all exact blockFound
  have sameEntry := Option.some.inj found'
  subst calleeEntry
  with_unfolding_all exact Memory.ParamOutcome.empty params

private theorem callAtEntryUnique {startDefinitions : Bool} (kind : TrapKind) (q : memoryExecution.State)
    (tick : ((callingSSA kind).memoryView (language := testLanguage)).Step storeInitial q startDefinitions) :
    q = helperState := by
  exact (callOutcomeUnique kind _ _ (Memory.Step.normal tick fun _ h => Bool.noConfusion (callOutcomeUnique kind _ _ h).2)).1

-- The mutation changes only the reached helper's trap. All metadata is retained.
set_option maxRecDepth 4096 in
example :
    let source := (callingSSA .divisionByZero).memoryView (language := testLanguage)
    ¬ BuildsSSA (language := testLanguage) source (callingSSA .abort) := by
  dsimp only
  intro lowered
  let source := (callingSSA .divisionByZero).memoryView (language := testLanguage)
  have terminal : memoryExecution.result source helperState (helperTrap .divisionByZero) := by
    with_unfolding_all rfl
  have limits : ∀ q, memoryExecution.finite source q → q = storeInitial ∨ q = helperState := by
    rintro q ⟨p, starts, path⟩
    have eq : p = storeInitial := by with_unfolding_all exact starts
    rw [eq] at path
    cases path with
    | nil => exact .inl rfl
    | cons tick rest =>
      obtain rfl := callAtEntryUnique .divisionByZero _ tick.2.1
      cases rest with
      | nil => exact .inr rfl
      | cons next _ => exact (memoryExecution.terminal _ _ _ terminal _ next).elim
  have safe : ProgramSafe memoryExecution source := by
    intro q stuck
    rcases limits q stuck.1 with rfl | rfl
    · exact stuck.2.1 helperState (callAtEntry .divisionByZero)
    · exact stuck.2.2 _ terminal
  have targetTick : ssaExecution.step (callingSSA .abort) storeInitial helperState := by
    with_unfolding_all
      exact ⟨rfl, ⟨_, rfl, rfl, [], _, _, _, rfl, rfl, rfl, rfl, .nil _⟩, _, rfl⟩
  have targetResult : ssaExecution.result (callingSSA .abort) helperState (helperTrap .abort) := by
    with_unfolding_all rfl
  obtain ⟨q, reached, result, _⟩ := (lowered.2.2.2.2.2 safe).1 helperState (helperTrap .abort)
    ⟨storeInitial, by with_unfolding_all rfl, .cons targetTick (.nil _)⟩ targetResult
  rcases limits q reached with rfl | rfl
  · have absent : (none : Option Concrete.Contracts.Observation) = some (helperTrap .abort) := by
      with_unfolding_all exact result
    cases absent
  · have wrong : some (helperTrap .divisionByZero) = some (helperTrap .abort) := by
      with_unfolding_all exact result
    have eq := Option.some.inj wrong
    change Concrete.Contracts.Observation.trapped _ _ _ _ = _ at eq
    cases eq

private def abortingCore : CoreInput testLanguage := testCore tabProject (.abort testSite)

private abbrev coreExecution := CoreInput.Execution (language := testLanguage)

private def coreStart : coreExecution.State :=
  (⟨(abortingCore.machineProgram.ownership, abortingCore.machineProgram.declarations), abortingCore.machineProgram.initial⟩, [])

private def coreTrapped : coreExecution.State :=
  (⟨(abortingCore.machineProgram.ownership, abortingCore.machineProgram.declarations),
    {abortingCore.machineProgram.initial with ctrl := .trapped .abort testSite}⟩, [])

private theorem coreAbortTick : coreExecution.step abortingCore coreStart coreTrapped := by
  with_unfolding_all
    exact ⟨rfl, ⟨_, _, .silent, rfl, rfl, Exec.Step.silent rfl, rfl⟩, _, rfl⟩

private theorem coreAbortTickUnique (q : coreExecution.State)
    (tick : coreExecution.step abortingCore coreStart q) : q = coreTrapped := by
  obtain ⟨_, ⟨p, next, label, initial, endState, step, effects⟩, _⟩ := tick
  have same : (⟨(abortingCore.machineProgram.ownership, abortingCore.machineProgram.declarations), abortingCore.machineProgram.initial⟩ :
      Σ env : AList Var Own.Descriptor × Declarations, Exec.Config (testLanguage.instrumented env.1 env.2)) =
        ⟨(abortingCore.machineProgram.ownership, abortingCore.machineProgram.declarations), p⟩ := initial
  cases eq_of_heq (Sigma.mk.inj same).2
  have expected : Exec.step abortingCore.machineProgram.body abortingCore.machineProgram.initial =
      some (Exec.Out.next {abortingCore.machineProgram.initial with ctrl := .trapped .abort testSite}) := by
    with_unfolding_all rfl
  cases step with
  | silent h =>
    rw [expected] at h
    have equal := Exec.Out.next.inj (Option.some.inj h)
    subst next
    exact Prod.ext endState effects
  | prim h =>
    rw [expected] at h
    cases h

-- The target's immediate return is a real MIR result. The wrong body
-- survives no Core-to-MIR proof even if a supplied model would call it a trap.
set_option maxRecDepth 4096 in
private def wrongCoreMIR : MIR testLowering testProfile :=
  let block : Block _ _ _ := ⟨⟨0⟩, [], [], .ret (some ⟨(), .literal ⟨[42]⟩⟩), [testOrigin]⟩
  {(returningMIR tabProject) with functions := [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[block]⟩)]}

private theorem coreWrongBody : ¬ LowersCore (language := testLanguage) ⟨abortingCore, []⟩ wrongCoreMIR := by
  intro lowered
  have terminal : coreExecution.result abortingCore coreTrapped (helperTrap .abort) := by
    with_unfolding_all rfl
  have reached : ∀ q, coreExecution.finite abortingCore q → q = coreStart ∨ q = coreTrapped := by
    rintro q ⟨p, starts, path⟩
    have eq : p = coreStart := starts
    rw [eq] at path
    cases path with
    | nil => exact .inl rfl
    | cons tick rest =>
      obtain rfl := coreAbortTickUnique _ tick
      cases rest with
      | nil => exact .inr rfl
      | cons next _ => exact (coreExecution.terminal _ _ _ terminal _ next).elim
  have safe : ProgramSafe coreExecution abortingCore := by
    intro q stuck
    rcases reached q stuck.1 with rfl | rfl
    · exact stuck.2.1 coreTrapped coreAbortTick
    · exact stuck.2.2 _ terminal
  let target := MIR.Execution (language := testLanguage) (lang := testLowering) (profile := testProfile)
  let answer := Concrete.Contracts.Observation.returned 42 ByteArray.empty
  have returns : target.result wrongCoreMIR storeInitial answer := by with_unfolding_all rfl
  obtain ⟨q, finite, result, _⟩ := (lowered.2.2.2.2.2 safe).1 storeInitial answer
    ⟨storeInitial, by with_unfolding_all rfl, .nil _⟩ returns
  rcases reached q finite with rfl | rfl
  · have absent : (none : Option Concrete.Contracts.Observation) = some answer := by
      with_unfolding_all exact result
    cases absent
  · have wrong : some (helperTrap .abort) = some answer := by with_unfolding_all exact result
    have eq := Option.some.inj wrong
    change Concrete.Contracts.Observation.trapped _ _ _ _ = .returned 42 ByteArray.empty at eq
    cases eq

private def abortMIR (kind : TrapKind) : MIR testLowering testProfile :=
  let block : Block _ _ _ := ⟨⟨0⟩, [], [], .trap kind testOrigin, []⟩
  ⟨tabProject, Exec.Demo.mainF, [(Exec.Demo.mainF, ⟨[], none⟩)], [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[block]⟩)], [], []⟩

private abbrev mirExecution := MIR.Execution (language := testLanguage)
  (lang := testLowering) (profile := testProfile)

private theorem abortLowering :
    LowersCore (language := testLanguage) ⟨abortingCore, []⟩ (abortMIR .abort) := by
  refine ⟨rfl, rfl, ⟨fun _ h => h, fun _ h => h⟩, ?_, ⟨⟨?_, nofun⟩, rfl⟩, ?_⟩
  · refine ⟨List.nodup_nil, ?_, nofun, ?_, ?_⟩
    · rintro site _ _ ⟨op, present, notAbort, _, _⟩ _
      have member : (testSite, CoreOperation.abort) = (site, op) := by
        apply Eq.symm
        exact List.mem_singleton.mp present
      have same := congrArg Prod.snd member
      cases same
      exact False.elim notAbort
    · rintro site evidence ⟨bucket, member, _⟩
      exact (List.not_mem_nil member).elim
    · intro _ op present
      have member := List.mem_singleton.mp present
      cases member
      trivial
  · intro fn locals graph present
    cases List.mem_singleton.mp present
    exact nofun
  · intro _
    have terminal : mirExecution.result (abortMIR .abort) storeInitial (helperTrap .abort) := by
      with_unfolding_all rfl
    have sourceResult : coreExecution.result abortingCore coreTrapped (helperTrap .abort) := by
      with_unfolding_all rfl
    have reached := reachableTerminal mirExecution (abortMIR .abort) storeInitial (helperTrap .abort)
      (fun _ h => by with_unfolding_all exact h) terminal
    refine ⟨?_, ?_, ?_⟩
    · intro state result finite observed
      obtain rfl := reached state finite
      have same : some (helperTrap .abort) = some result := by with_unfolding_all exact observed
      obtain rfl := Option.some.inj same
      exact ⟨coreTrapped, ⟨coreStart, rfl, .cons coreAbortTick (.nil _)⟩, sourceResult,
        by with_unfolding_all rfl⟩
    · intro run infinite
      have initial : run 0 = storeInitial := by with_unfolding_all exact infinite.1
      exact (mirExecution.terminal _ _ _ terminal _ (initial ▸ infinite.2 0)).elim
    · intro state stuck
      obtain rfl := reached state stuck.1
      exact stuck.2.2 _ terminal

example :
    LowersCore (language := testLanguage) ⟨abortingCore, []⟩ (abortMIR .abort) ∧
    ¬ LowersCore (language := testLanguage) ⟨abortingCore, []⟩ wrongCoreMIR := by
  exact ⟨abortLowering, coreWrongBody⟩

private def unsafeMIR : MIR testLowering testProfile :=
  let block : Block _ _ _ := ⟨⟨0⟩, [], [], .invalid, []⟩
  {(returningMIR tabProject) with functions := [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[block]⟩)]}

private theorem unsafeEntryStuck : mirExecution.stuck unsafeMIR storeInitial := by
  refine ⟨⟨storeInitial, by with_unfolding_all rfl, .nil _⟩, ?_, ?_⟩
  · intro next tick
    obtain ⟨_, ⟨block, found, step⟩, _⟩ := tick
    let entry : MemoryBlock testLowering := ⟨⟨0⟩, [], [], .invalid, []⟩
    have same : some entry = some block := by with_unfolding_all exact found
    have equal := Option.some.inj same
    subst block
    exact step.2
  · intro o returned
    have impossible : none = some o := by with_unfolding_all exact returned
    cases impossible

-- An invalid terminator remains executable only as a malformed input to the
-- semantics. It is excluded from the registered lowering handoff.
example : ¬ LowersMemory (language := testLanguage) unsafeMIR (returningMemory tabProject) := by
  rintro ⟨_, _, _, _, _, _, _, tables, _, _⟩
  let block : MemoryBlock testLowering := ⟨⟨0⟩, [], [], .invalid, []⟩
  have present : (Exec.Demo.mainF, [], ⟨⟨0⟩, #[block]⟩) ∈
      (unsafeMIR.logicalView (language := testLanguage)).functions :=
    by with_unfolding_all exact List.mem_singleton_self _
  exact noInvalidEnding tables present (List.mem_singleton_self block) rfl

-- Requiring an unconditional checkpoint simulation would reject the same
-- program after its confirmed startup fault.
example :
    ¬ ∃ _ : CheckpointSimulation mirExecution memoryExecution mirExecution.result memoryExecution.result
      unsafeMIR (returningMemory tabProject), True := by
  rintro ⟨sim, _⟩
  let target := returningMemory tabProject
  have targetInitial : memoryExecution.initial target storeInitial := by with_unfolding_all rfl
  obtain ⟨source, sourceInitial, related⟩ := sim.targetInitial storeInitial targetInitial
  have entry : source = storeInitial := by with_unfolding_all exact sourceInitial
  rw [entry] at related
  have observed : memoryExecution.result target storeInitial (.returned 0 ByteArray.empty) := by
    with_unfolding_all rfl
  obtain ⟨state, path, result, _⟩ := sim.targetFinish storeInitial storeInitial _
    unsafeEntryStuck.1 ⟨storeInitial, targetInitial, .nil _⟩ related observed
  cases path with
  | nil => exact unsafeEntryStuck.2.2 _ result
  | cons tick _ => exact unsafeEntryStuck.2.1 _ tick.1

private abbrev nonzeroAddressLanguage : CoreLanguage :=
  {erasureLanguage with
    operation := fun _ _ _ _ _ _ operands before result effects after =>
      (∃ address, operands = [address] ∧ address.root = shadowRoot) ∧
      result = .scalar (.int .u8 17) ∧ effects = [] ∧ after = before}

private def nonzeroAddressBIR (project : Project) : BIR testLowering testProfile :=
  let block := {birReturnBlock false with
    body := [⟨testOrigin, .op (opcode := .named ⟨"address"⟩) (ty := ()) ⟨0⟩ () .nil
      [⟨(), ⟨shadowRoot, []⟩⟩]⟩]}
  {(birReturn project false) with
    functions := [(Exec.Demo.mainF, [(shadowRoot, ())], ⟨⟨0⟩, #[block]⟩)]
    layouts := [(Exec.Demo.mainF, [(shadowRoot, ⟨1, 1, .nil⟩)])]}

private def addressAtOne : MemoryRegisters nonzeroAddressLanguage testProfile :=
  {splitBefore with
    live := [shadowRoot]
    initialized := [shadowRoot]
    materialized := [(shadowRoot, backingRange ⟨0⟩ 1)]}

-- The operation checks the exact spec root. Renumbering materialized root 1
-- to 2 invalidates this step, even though the backing bytes are unchanged.
set_option maxRecDepth 3072 in
set_option maxHeartbeats 1000000 in
example (project : Project) :
    let exec := BIR.Execution (language := nonzeroAddressLanguage)
    ∃ next, exec.step (nonzeroAddressBIR project) addressAtOne next ∧
      next.addressIds = [(shadowRoot, ⟨0⟩)] ∧
      next.virtualValues = [(⟨2⟩, .scalar (.int .u8 17))] := by
  let addr : MemoryPlace Unit () := ⟨⟨shadowRoot, []⟩, .materialized⟩
  let dst := fieldRegister 2
  let block : MemoryBlock testLowering := ⟨⟨0⟩, [],
    [⟨testOrigin, .op (opcode := .named ⟨"address"⟩) (ty := ()) dst () .nil [] [⟨(), addr⟩]⟩],
    .ret (some ⟨(), .literal ⟨[0]⟩⟩), []⟩
  let ready : MemoryRegisters nonzeroAddressLanguage testProfile := {addressAtOne with
    addressIds := [(shadowRoot, ⟨0⟩)]
    nextAddressId := 1}
  let started : MemoryRegisters nonzeroAddressLanguage testProfile := {ready with live := [⟨2⟩, shadowRoot]}
  let stored : MemoryRegisters nonzeroAddressLanguage testProfile := {started with
    initialized := [⟨2⟩, shadowRoot]
    virtualValues := [(⟨2⟩, .scalar (.int .u8 17))]}
  let next : MemoryRegisters nonzeroAddressLanguage testProfile := {stored with instruction := 1}
  let address : OperationAddress Unit := ⟨(), shadowRoot, [], some (backingRange ⟨0⟩ 1), ⟨0⟩, .bytes⟩
  refine ⟨next, ⟨by with_unfolding_all rfl, ⟨block, by with_unfolding_all rfl, ?_⟩,
    [], by with_unfolding_all rfl⟩, rfl, rfl⟩
  refine ⟨[], addressAtOne, [address], .scalar (.int .u8 17), [], Memory.operationStore ready false,
    stored, rfl, rfl, by with_unfolding_all rfl, ⟨⟨address, rfl, rfl⟩, rfl, rfl, rfl⟩, rfl, ?_, rfl⟩
  apply Memory.WriteParams.cons (started := started) (stored := stored)
  · with_unfolding_all exact ⟨nofun, Or.inl ⟨rfl, rfl⟩⟩
  · with_unfolding_all exact ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩
  · exact .nil _

-- An undeclared physical lifetime operand cannot start the synthetic storage
-- reserved for a later register definition.
private def unknownLifetimeBIR (project : Project) : BIR testLowering testProfile :=
  let block := {birReturnBlock false with
    body := [⟨testOrigin, .beginLifetime shadowRoot⟩,
      ⟨testOrigin, .op (opcode := .named ⟨"unused"⟩) (ty := ()) ⟨0⟩ () .nil⟩]}
  {(birReturn project false) with
    functions := [(Exec.Demo.mainF, [(storeRoot, ())], ⟨⟨0⟩, #[block]⟩)]}

example (project : Project) :
    let bir := unknownLifetimeBIR project
    let view := bir.memoryView (language := testLanguage)
    view.classAt (language := testLanguage) bir.entry shadowRoot = none ∧
    ∀ next, ¬ view.Begins (language := testLanguage) (BIR.entryState bir) shadowRoot next := by
  refine ⟨by with_unfolding_all rfl, ?_⟩
  intro next begins
  rcases begins.2 with virtual | materialized
  · have impossible : none = some StorageClass.virtual := by with_unfolding_all exact virtual.1
    cases impossible
  · have impossible : none = some StorageClass.materialized := by with_unfolding_all exact materialized.1
    cases impossible

private theorem emptyBytesWF : (Mem.Memory.empty).WellFormed testProfile.memoryTarget :=
  ⟨nofun, by decide, nofun, nofun, nofun, nofun, nofun, nofun⟩

private theorem storedBytesWF (space : AddrSpace) (base : Nat) (fits : backingFits space base)
    (byte : Fin 256 := 42) :
    (backingAfter space base byte).bytes.WellFormed testProfile.memoryTarget := by
  let stored := {Mem.candidate space base 1 1 with bytes := [.init byte none]}
  have lookup : ∀ block value, (backingAfter space base byte).bytes.block block = some value →
      block = ⟨0⟩ ∧ value = stored := by
    intro block value found
    change (if (⟨0⟩ : BlockId) = block then some stored else none) = some value at found
    split at found
    · rename_i eq
      exact ⟨eq.symm, (Option.some.inj found).symm⟩
    · contradiction
  refine ⟨?_, (by simp : [_].Nodup), ?_, ?_, ?_, ?_, ?_, ?_⟩
  · intro entry member
    change entry ∈ [(⟨0⟩, stored)] at member
    cases List.mem_singleton.mp member
    rfl
  · intro block value found
    rcases lookup block value found with ⟨rfl, rfl⟩
    exact Nat.zero_lt_one
  · intro block value found
    rcases lookup block value found with ⟨_, rfl⟩
    rfl
  · intro block value found
    rcases lookup block value found with ⟨_, rfl⟩
    exact fits.2.2.1
  · intro block value found
    rcases lookup block value found with ⟨_, rfl⟩
    exact fits.2.2.2.2.2.2
  · intro block value found
    rcases lookup block value found with ⟨_, rfl⟩
    exact ⟨fits.1, fits.2.1⟩
  · intro b x c y first second distinct _ _
    exact (distinct ((lookup b x first).1.trans (lookup c y second).1.symm)).elim

private def materializedBindingBlock : MemoryBlock testLowering :=
  ⟨⟨0⟩, [],
    [⟨testOrigin, .beginLifetime storeRoot⟩, ⟨testOrigin, .bind storePlace (.literal ⟨[]⟩)⟩],
    .ret (some ⟨(), .literal ⟨[0]⟩⟩), []⟩

private def materializedBinding (project : Project) : Memory testLowering testProfile :=
  {(immediateReturn project) with
    functions := [(Exec.Demo.mainF, [(storeRoot, ())], ⟨⟨0⟩, #[materializedBindingBlock]⟩)]}

private theorem backingAccess (space : AddrSpace) (base : Nat) :
    (backingBefore space base).bytes.access testProfile.memoryTarget (backingRange space base).ptr 1 1 =
      some (some (⟨0⟩, 0)) := by
  simp [backingBefore, backingRange, Mem.Memory.access, Mem.Memory.block, AList.lookup, Mem.candidate, Nat.mod_one]

private theorem aggregateWrite (project : Project) (space : AddrSpace) (base : Nat) :
    (materializedBinding project).Writes (language := erasureLanguage) (backingBefore space base) storePlace
      (.other ()) {backingAfter space base with instruction := 1} := by
  have stored : (backingBefore space base).bytes.store testProfile.memoryTarget
      (backingRange space base).ptr [.init 42 none] 1 = some (backingAfter space base).bytes := by
    unfold Mem.Memory.store
    rw [show ([Mem.MByte.init 42 none]).length = 1 from rfl, backingAccess]
    simp [backingBefore, backingAfter, Mem.Memory.modify, AList.update, Mem.candidate]
  have loaded : (backingAfter space base).bytes.load testProfile.memoryTarget
      (backingRange space base).ptr 1 1 = some [.init 42 none] := by
    unfold Mem.Memory.load
    rw [show (backingAfter space base).bytes.access testProfile.memoryTarget
      (backingRange space base).ptr 1 1 = some (some (⟨0⟩, 0)) by
        simp [backingAfter, backingBefore, backingRange, Mem.Memory.access, Mem.Memory.block,
          AList.lookup, Mem.candidate, Nat.mod_one]]
    rfl
  refine ⟨⟨rfl, fun _ => rfl⟩, (fun _ => ⟨⟨1, 1, .nil⟩, rfl, ⟨nofun, nofun⟩⟩),
    ?_, ?_, fun _ => Or.inr ⟨backingRange space base, [(0, [.init 42 none])], rfl, rfl, ?_, ?_⟩⟩
  · simp [Memory.writeValue, Memory.markWritten, Memory.storeRuns, Memory.atOffset, Memory.mayInitialize, Memory.authorizeWrite, Memory.writeOwnerView, Memory.ownerView, Memory.zeroRoot, Memory.rangeAt, Memory.byteRuns,
      Memory.layoutAt, materializedBinding, immediateReturn, zeroMemory, storePlace,
      backingBefore, backingAfter, backingRange, storeBefore, storeInitial, storeRoot,
      Mem.Memory.store, Mem.Memory.access, Mem.Memory.block, Mem.Memory.modify,
      AList.lookup, AList.update, Mem.candidate, Nat.mod_one, erasureLanguage] <;> rfl
  · with_unfolding_all
      change (do
        let bytes ← (backingAfter space base).bytes.load testProfile.memoryTarget
          (backingRange space base).ptr 1 1
        let value ← erasureLanguage.decodeOther testProfile.memoryTarget () bytes
        pure (CoreValue.other value)) = _
    rw [loaded]
    rfl
  · exact fun _ => ⟨0, [.init 42 none], List.mem_cons_self .., by decide⟩
  · intro offset bytes member
    cases List.mem_singleton.mp member
    refine ⟨Nat.le_refl 1, ?_, loaded⟩
    intro byte included
    cases List.mem_singleton.mp included
    decide

private theorem materializedRepresents (project : Project) (space : AddrSpace) (base : Nat)
    (fits : backingFits space base) :
    (materializedBinding project).RepresentsActivation (language := erasureLanguage)
      erasureBound.toMemoryFrame (backingAfter space base).toMemoryFrame (backingAfter space base).bytes := by
  refine ⟨⟨⟨storedBytesWF space base fits, (by simp : [storeRoot].Nodup), (by simp : [storeRoot].Nodup), fun _ h => h⟩, rfl⟩, rfl, fun _ h => h,
    (by simp : [storeRoot].Nodup), (by simp : [storeRoot].Nodup), List.nodup_nil, List.nodup_nil, nofun, ?_, ?_, nofun, ?_, ?_⟩
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
      ⟨⟨1, 1, .nil⟩, ⟨0⟩, _, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩⟩
  · intro place member
    have eq := List.mem_singleton.mp member
    subst place
    refine ⟨_, _, (), .materialized, List.mem_singleton_self _, rfl, rfl, ?_, ?_⟩
    · intro _
      exact ⟨⟨1, 1, .nil⟩, rfl, ⟨nofun, nofun⟩⟩
    · intro value lookedUp
      have same : some (CoreValue.other ()) = some value := lookedUp
      cases same
      exact ⟨_, rfl, (aggregateWrite project space base).2.2.2⟩

private inductive Materialization where
  | initial
  | allocated (space : AddrSpace) (base : Nat) (fits : backingFits space base)
  | bound (space : AddrSpace) (base : Nat) (fits : backingFits space base)

private def Materialization.state : Materialization → MemoryRegisters erasureLanguage testProfile
  | .initial => storeInitial
  | .allocated space base _ => backingBefore space base
  | .bound space base _ => backingAfter space base

private def Materialization.finished : Materialization → Bool
  | .bound .. => true
  | _ => false

private inductive Materialization.Tick : Materialization → Materialization → Prop
  | allocate (space base) (fits : backingFits space base) : Tick .initial (.allocated space base fits)
  | bind (space base) (fits : backingFits space base) : Tick (.allocated space base fits) (.bound space base fits)

private def materializedSourceState (done : Bool) : MemoryRegisters erasureLanguage testProfile :=
  if done then erasureBound else storeInitial

private theorem materializationNormal (project : Project) (p : Materialization) (next : MemoryRegisters erasureLanguage testProfile) :
    (materializedBinding project).StepOutcome (language := erasureLanguage) p.state next false ↔
      ∃ q, p.Tick q ∧ q.state = next := by
  constructor
  · intro step
    cases p with
    | initial =>
      obtain ⟨block, fetched, tick⟩ := step
      have same : some _ = some block := fetched
      cases same
      with_unfolding_all
        change ∃ started, (materializedBinding project).Begins
          (language := erasureLanguage) storeInitial storeRoot started ∧ next = started.advance at tick
      obtain ⟨started, begins, rfl⟩ := tick
      rcases begins.2 with ⟨impossible, _⟩ | ⟨_, layout, lookup, isZero | allocated⟩
      · have wrong : some StorageClass.materialized = some .virtual := impossible
        cases wrong
      · have same : some (⟨1, 1, .nil⟩ : Layout) = some layout := lookup
        cases same
        exact (Nat.one_ne_zero isZero.1).elim
      · have same : some (⟨1, 1, .nil⟩ : Layout) = some layout := lookup
        cases same
        obtain ⟨_, space, base, id, bytes, alloc, rfl⟩ := allocated
        have result : Mem.Memory.empty.alloc testProfile.memoryTarget space base 1 1 = .ok (id, bytes) := alloc
        unfold Mem.Memory.alloc at result
        dsimp only at result
        split at result
        · rename_i fits
          cases result
          exact ⟨.allocated space base fits, .allocate space base fits, rfl⟩
        · contradiction
    | allocated space base fits =>
      obtain ⟨block, fetched, tick⟩ := step
      have same : some _ = some block := fetched
      cases same
      with_unfolding_all
        change (materializedBinding project).InstructionStep (language := erasureLanguage)
          (backingBefore space base) next false (.bind storePlace (.literal ⟨[]⟩)) at tick
      obtain ⟨value, afterRead, stored, read, writes, rfl⟩ := tick
      have readExact : some (CoreValue.other (), backingBefore space base) = some (value, afterRead) := read
      cases readExact
      have writeExact := (aggregateWrite project space base).2.2.1
      have stores := writeExact.symm.trans writes.2.2.1
      cases stores
      exact ⟨.bound space base fits, .bind space base fits, rfl⟩
    | bound space base fits =>
      obtain ⟨block, fetched, tick⟩ := step
      have same : some _ = some block := fetched
      cases same
      with_unfolding_all
        change _ ∧ (materializedBinding project).Return (language := erasureLanguage)
          (some ⟨(), .literal ⟨[0]⟩⟩) (backingAfter space base) next at tick
      obtain ⟨_, caller, rest, _, _, _, callers, _⟩ := tick
      change [] = caller :: rest at callers
      cases callers
  · rintro ⟨q, tick, rfl⟩
    cases tick with
    | allocate space base fits =>
      refine ⟨_, rfl, {backingBefore space base with instruction := 0}, ⟨by decide, Or.inr ?_⟩, rfl⟩
      refine ⟨rfl, ⟨⟨1, 1, .nil⟩, rfl, Or.inr ⟨by decide, space, base, ⟨0⟩, _, ?_, rfl⟩⟩⟩
      change Mem.Memory.empty.alloc testProfile.memoryTarget space base 1 1 = _
      unfold Mem.Memory.alloc
      have ok : Mem.Memory.empty.Fits testProfile.memoryTarget (Mem.candidate space base 1 1) := fits
      simp only [ok, ite_true]
      rfl
    | bind space base fits =>
      exact ⟨_, rfl, .other (), backingBefore space base, {backingAfter space base with instruction := 1},
        rfl, aggregateWrite project space base, rfl⟩

private theorem materializationExact (project : Project) (p : Materialization) (next : MemoryRegisters erasureLanguage testProfile) :
    (materializedBinding project).Step (language := erasureLanguage) p.state next ↔
      ∃ q, p.Tick q ∧ q.state = next := by
  constructor
  · intro tick
    apply (materializationNormal project p next).mp
    apply tick.normal (language := erasureLanguage)
    intro partialState failed
    have block : (materializedBinding project).blockAt p.state = some materializedBindingBlock := by cases p <;> with_unfolding_all rfl
    cases p with
    | initial =>
      have ok := (materializationNormal project .initial (.allocated ⟨0⟩ 1 (by unfold backingFits; decide) : Materialization).state).mpr
        ⟨_, .allocate ⟨0⟩ 1 _, rfl⟩
      have exhausted : (materializedBinding project).BeginExhausted (language := erasureLanguage) storeInitial storeRoot :=
        failed.instructionAt (op := ⟨testOrigin, .beginLifetime storeRoot⟩) block (by with_unfolding_all rfl)
      obtain ⟨started, begins, _⟩ := ok.instructionAt (op := ⟨testOrigin, .beginLifetime storeRoot⟩) block (by with_unfolding_all rfl)
      exact exhausted.unavailable ⟨started, begins⟩
    | allocated space base fits =>
      exact Memory.bind_cannot_exhaust (language := erasureLanguage)
        (failed.instructionAt (op := ⟨testOrigin, .bind storePlace (.literal ⟨[]⟩)⟩) block (by with_unfolding_all rfl))
    | bound space base fits =>
      obtain ⟨caller, rest, _, _, _, callers, _⟩ := failed.terminatorAt block (by with_unfolding_all rfl)
      change [] = caller :: rest at callers
      cases callers
  · rintro ⟨q, tick, rfl⟩
    have normal := (materializationNormal project p q.state).mpr ⟨q, tick, rfl⟩
    cases tick <;> exact normal

private def logicalBindingBlock : MemoryBlock testLowering :=
  ⟨⟨0⟩, [], [⟨testOrigin, .bind (ty := ()) ⟨⟨storeRoot, []⟩, .virtual⟩ (.literal ⟨[]⟩)⟩],
    .ret (some ⟨(), .literal ⟨[0]⟩⟩), []⟩

private theorem logicalBindingFunctions (project : Project) :
    ((erasureSource project).logicalView (language := erasureLanguage)).functions =
      [(Exec.Demo.mainF, [(storeRoot, ())], ⟨⟨0⟩, #[logicalBindingBlock]⟩)] := by
  with_unfolding_all rfl

private theorem materializedSourceExact (project : Project) (p : Bool)
    (next : MemoryRegisters erasureLanguage testProfile) :
    (erasureSource project).Step (language := erasureLanguage) (materializedSourceState p) next ↔
      ∃ q : Bool, (p = false ∧ q = true) ∧ materializedSourceState q = next := by
  constructor
  · intro step
    cases p
    · exact ⟨true, ⟨rfl, rfl⟩, (erasureBindingUnique project _ step).symm⟩
    · obtain ⟨block, fetched, tick⟩ := step
      have same : some logicalBindingBlock = some block := by with_unfolding_all exact fetched
      cases same
      with_unfolding_all
        change _ ∧ (erasureSource project).logicalView.Return (language := erasureLanguage)
          (some ⟨(), .literal ⟨[0]⟩⟩) erasureBound next at tick
      obtain ⟨_, caller, rest, _, _, _, callers, _⟩ := tick
      change [] = caller :: rest at callers
      cases callers
  · rintro ⟨_, ⟨rfl, rfl⟩, rfl⟩
    exact erasureBinding project

private def finiteReturnModel (A S : Type) (start : S) (step : S → S → Prop)
    (finished : S → Bool) (continues : ∀ p q, step p q → finished p = false)
    (status : UInt8 := 0) : Model A Nat where
  execution := {
    State := S
    initial := fun _ p => p = start
    initialized := fun _ => ⟨start, rfl⟩
    step := fun _ => step
    result := fun _ p result => finished p = true ∧ result = .returned status ByteArray.empty
    terminal := fun _ p _ result q tick => Bool.noConfusion ((continues p q tick).symm.trans result.1)
    safe := fun _ _ => True
    trace := fun _ _ => []
    permitted := fun _ _ => True
    resultTrace := by rintro _ _ _ ⟨_, rfl⟩; rfl
    traceGrows := fun _ _ _ _ => ⟨[], rfl⟩
  }
  result := fun _ p o => finished p = true ∧ o = .returned status ByteArray.empty
  projects := fun _ o result => o = .returned status ByteArray.empty ∧ result = .returned status ByteArray.empty
  terminalExact := by
    intro _ _ _
    exact ⟨fun ⟨h, eq⟩ => ⟨_, ⟨h, rfl⟩, rfl, eq⟩, fun ⟨_, ⟨h, _⟩, _, eq⟩ => ⟨h, eq⟩⟩
  completeProjection := fun _ _ _ h => ⟨_, h.2, rfl⟩
  runtimeExact := by rintro _ _ _ ⟨rfl, rfl⟩; exact ⟨rfl, rfl⟩

-- A target self-loop cannot keep a checkpoint before the source's binding
-- while excluding the state that contains it.
example :
    let source := finiteReturnModel Unit Bool false (fun p q => p = false ∧ q = true) id
      (fun _ _ tick => tick.1)
    let target := finiteReturnModel Unit Unit () (fun _ _ => True) (fun _ => false)
      (fun _ _ _ => rfl)
    ∀ sim : CheckpointSimulation source.execution target.execution source.result target.result () (),
      sim.related false () → ¬ sim.related true () → False := by
  dsimp only
  intro sim related missing
  obtain progresses | stutters := sim.targetAdvance false () ()
    ⟨false, rfl, .nil _⟩ ⟨(), rfl, .nil _⟩ related trivial
  · obtain ⟨before, after, _, step, matched⟩ := progresses
    rw [step.2] at matched
    exact missing matched
  · exact (Nat.lt_irrefl _) stutters.2

private def branchOrLoop : Model Unit Nat :=
  finiteReturnModel Unit Bool false (fun p _ => p = false) id (fun _ _ tick => tick)

-- The source can loop or take the exit. A compiler may select the exit.
example : branchOrLoop.Refines (returnsStatus Unit 0) () () := by
  intro _
  refine ⟨?_, ?_, ?_⟩
  · intro _ _ _ result
    exact ⟨true, ⟨false, rfl, .cons rfl (.nil _)⟩, ⟨rfl, result⟩, rfl⟩
  · exact fun _ infinite => (infinite.2 0).elim
  · exact fun _ stuck => stuck.2.2 (.returned 0 ByteArray.empty) rfl

-- Selecting the exit does not license changing its status.
example : ¬ branchOrLoop.Refines (returnsStatus Unit 42) () () := by
  intro refines
  have safe : ProgramSafe branchOrLoop.execution () := by
    intro p stuck
    cases p
    · exact stuck.2.1 false rfl
    · exact stuck.2.2 (.returned 0 ByteArray.empty) ⟨rfl, rfl⟩
  obtain ⟨_, _, result, _⟩ := (refines safe).1 () (.returned 42 ByteArray.empty)
    ⟨(), trivial, .nil _⟩ rfl
  cases result.2

private def allocationChoices (addresses : List UInt8) : Concrete.Contracts.Semantics Unit where
  State := Option UInt8
  initial _ state := state = none
  initialized _ := ⟨none, rfl⟩
  step _ before after := before = none ∧ ∃ address ∈ addresses, after = some address
  result _ state observed := ∃ address, state = some address ∧ observed = .returned address ByteArray.empty
  terminal := by rintro _ _ _ ⟨a, rfl, _⟩ _ ⟨e, _⟩; cases e
  safe _ _ := True
  trace _ _ := []
  permitted _ _ := True
  resultTrace := by rintro _ _ _ ⟨_, _, rfl⟩; rfl
  traceGrows := fun _ _ _ _ => ⟨[], rfl⟩

private theorem allocationSafe (addresses : List UInt8) (existsAddress : ∃ a, a ∈ addresses) :
    ProgramSafe (allocationChoices addresses) () := by
  intro state stuck
  cases state with
  | none =>
      obtain ⟨a, allowed⟩ := existsAddress
      exact stuck.2.1 (some a) ⟨rfl, a, allowed, rfl⟩
  | some a => exact stuck.2.2 (.returned a ByteArray.empty) ⟨a, rfl, rfl⟩

-- An early allocation may select one of the possible exposed addresses.
-- Selecting an address outside the source choices changes the behavior.
example :
    let source := allocationChoices [1, 2]
    let chosen := allocationChoices [1]
    let wrong := allocationChoices [3]
    BehaviorRefinement source chosen source.result chosen.result () () ∧
    ¬ BehaviorRefinement source wrong source.result wrong.result () () := by
  dsimp
  constructor
  · intro _
    refine ⟨?_, ?_, allocationSafe [1] ⟨1, by simp⟩⟩
    · rintro state _ ⟨start, rfl, path⟩ ⟨address, rfl, rfl⟩
      cases path with
      | cons first rest =>
          obtain ⟨_, a, allowed, equal⟩ := first
          subst equal
          cases rest with
          | nil =>
              have member : address ∈ ([1, 2] : List UInt8) :=
                List.mem_cons.mpr (Or.inl (List.mem_singleton.mp allowed))
              exact ⟨some address, ⟨none, rfl, .cons ⟨rfl, address, member, rfl⟩ (.nil _)⟩,
                ⟨address, rfl, rfl⟩, rfl⟩
          | cons second _ => cases second.1
    · intro run divergent
      obtain ⟨_, a, _, equal⟩ := divergent.2 0
      have impossible := (divergent.2 1).1
      rw [equal] at impossible
      cases impossible
  · intro refines
    obtain ⟨state, ⟨start, rfl, path⟩, ⟨address, rfl, returned⟩, _⟩ :=
      (refines (allocationSafe [1, 2] ⟨1, by simp⟩)).1 (some 3) (.returned 3 ByteArray.empty)
        ⟨none, rfl, .cons ⟨rfl, 3, by simp, rfl⟩ (.nil _)⟩ ⟨3, rfl, rfl⟩
    cases returned
    cases path with
    | cons first rest =>
        obtain ⟨_, a, allowed, equal⟩ := first
        subst equal
        cases rest with
        | nil => exact (show (3 : UInt8) ∉ [1, 2] from by decide) allowed
        | cons second _ => cases second.1

-- A reachable fault in the source defeats the safety premise instead of
-- requiring every successful lowering to prove progress for an unsafe input.
example :
    let source := allocationChoices []
    let target := allocationChoices [1]
    BehaviorRefinement source target source.result target.result () () := by
  dsimp
  intro safe
  exact (safe none ⟨⟨none, rfl, .nil _⟩,
    by rintro _ ⟨_, _, member, _⟩; exact List.not_mem_nil member,
    by rintro _ ⟨_, equality, _⟩; cases equality⟩).elim

private theorem materializedTables (project : Project) :
    (materializedBinding project).TablesWF ∧
      ((erasureSource project).logicalView (language := erasureLanguage)).TablesWF := by
  constructor
  all_goals
    with_unfolding_all
      refine ⟨(by simp : [_].Nodup), (by simp : [_].Nodup), (by simp : [_].Nodup), ?_, ?_, ?_, ?_⟩
    · first | exact (by simp : [_].Nodup) | exact List.nodup_nil
    · intro fn classes h
      cases List.mem_singleton.mp h
      exact (by simp : [_].Nodup)
    · intro fn layouts h
      first | (cases List.mem_singleton.mp h; exact (by simp : [_].Nodup)) | exact (List.not_mem_nil h).elim
    · intro fn locals graph member
      try rw [logicalBindingFunctions] at member
      cases List.mem_singleton.mp member
      refine ⟨(by simp : [_].Nodup), ⟨(by simp : [_].Nodup), ⟨_, List.mem_singleton_self _, rfl⟩, ?_⟩,
        _, rfl, ⟨_, List.mem_singleton_self _, rfl, rfl⟩, ?_⟩
      · intro block included
        cases List.mem_singleton.mp included
        exact fun _ => nofun
      · intro block included
        cases List.mem_singleton.mp included
        refine ⟨rfl, ?_⟩
        intro op h
        first
        | rcases List.mem_cons.mp h with rfl | last
          · trivial
          · cases List.mem_singleton.mp last; trivial
        | cases List.mem_singleton.mp h; trivial

private theorem allocatedBytesWF (space : AddrSpace) (base : Nat) (fits : backingFits space base) :
    (backingBefore space base).bytes.WellFormed testProfile.memoryTarget := by
  -- The allocation has the same geometry and live block as the initialized snapshot.
  let allocated := Mem.candidate space base 1 1
  have lookup : ∀ block value, (backingBefore space base).bytes.block block = some value →
      block = ⟨0⟩ ∧ value = allocated := by
    intro block value found
    change (if (⟨0⟩ : BlockId) = block then some allocated else none) = some value at found
    split at found
    · rename_i eq
      exact ⟨eq.symm, (Option.some.inj found).symm⟩
    · contradiction
  refine ⟨?_, (by simp : [_].Nodup), ?_, ?_, ?_, ?_, ?_, ?_⟩
  · intro entry member
    change entry ∈ [(⟨0⟩, allocated)] at member
    cases List.mem_singleton.mp member
    rfl
  · intro block value found
    rcases lookup block value found with ⟨rfl, rfl⟩
    exact Nat.zero_lt_one
  · intro block value found
    rcases lookup block value found with ⟨_, rfl⟩
    rfl
  · intro block value found
    rcases lookup block value found with ⟨_, rfl⟩
    exact fits.2.2.1
  · intro block value found
    rcases lookup block value found with ⟨_, rfl⟩
    exact fits.2.2.2.2.2.2
  · intro block value found
    rcases lookup block value found with ⟨_, rfl⟩
    exact ⟨fits.1, fits.2.1⟩
  · intro b x c y first second distinct _ _
    exact (distinct ((lookup b x first).1.trans (lookup c y second).1.symm)).elim

private theorem materializationRepresents (project : Project) (q : Materialization) :
    (materializedBinding project).RepresentsActivation (language := erasureLanguage)
      (materializedSourceState q.finished).toMemoryFrame q.state.toMemoryFrame q.state.bytes := by
  cases q with
  | initial =>
    exact ⟨⟨⟨emptyBytesWF, List.nodup_nil, List.nodup_nil, nofun⟩, rfl⟩,
      rfl, nofun, List.nodup_nil, List.nodup_nil, List.nodup_nil, List.nodup_nil,
      nofun, (fun _ => ⟨nofun, nofun⟩), nofun, nofun, nofun, nofun⟩
  | allocated space base fits =>
    refine ⟨⟨⟨allocatedBytesWF space base fits, (by simp : [_].Nodup), List.nodup_nil, nofun⟩, rfl⟩,
      rfl, nofun, List.nodup_nil, (by simp : [_].Nodup), List.nodup_nil, List.nodup_nil,
      nofun, (fun _ => ⟨nofun, nofun⟩), nofun, nofun, ?_, ?_⟩
    · intro place range member
      cases List.mem_singleton.mp member
      exact ⟨List.mem_singleton_self _, rfl,
        ⟨⟨1, 1, .nil⟩, ⟨0⟩, _, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩⟩
    · intro place member
      cases List.mem_singleton.mp member
      exact ⟨_, _, (), .materialized, List.mem_singleton_self _, rfl, rfl,
        fun _ => ⟨⟨1, 1, .nil⟩, rfl, ⟨nofun, nofun⟩⟩, fun _ => nofun⟩
  | bound space base fits => exact materializedRepresents project space base fits

private theorem materializationStatic (project : Project) :
    PreservesOrigins (erasureSource project).origins (materializedBinding project).origins ∧
    LayoutsMatch erasureLanguage.layout (materializedBinding project) ∧
    PromotionSound (language := erasureLanguage) (erasureSource project) (materializedBinding project) := by
  refine ⟨⟨?_, ?_⟩, ?_, ?_⟩
  · intro x h
    have member : x ∈ [testOrigin, testOrigin] := h
    rcases List.mem_cons.mp member with equal | rest
    · exact List.mem_singleton.mpr equal
    · exact rest
  · intro x h
    have member : x ∈ [testOrigin] := h
    exact List.mem_cons_of_mem _ member
  · constructor
    · intro fn locals graph member place ty localMember
      cases List.mem_singleton.mp member
      cases List.mem_singleton.mp localMember
      exact ⟨_, rfl, Or.inr ⟨rfl, _, _, rfl, rfl, rfl⟩⟩
    · intro fn layouts place layout member slot
      cases List.mem_singleton.mp member
      cases List.mem_singleton.mp slot
      exact ⟨_, _, (), _, List.mem_singleton_self _, List.mem_singleton_self _, rfl, rfl, rfl⟩
  · constructor
    · intro fn locals graph member place ty localMember
      cases List.mem_singleton.mp member
      cases List.mem_singleton.mp localMember
      exact ⟨_, .materialized, rfl, rfl, ⟨nofun, fun h => Bool.noConfusion (show false = true from h.1)⟩⟩
    · intro fn classes place member localMember
      cases List.mem_singleton.mp member
      cases List.mem_singleton.mp localMember

private structure AddressAllocation where
  space : AddrSpace
  base : Nat
  fits : backingFits space base

private def AddressAllocation.range (allocation : AddressAllocation) : Hybrid.Range :=
  backingRange allocation.space allocation.base

private def AddressAllocation.before (allocation : AddressAllocation) :
    MemoryRegisters addressLanguage testProfile :=
  backingBefore allocation.space allocation.base

private def AddressAllocation.bound (allocation : AddressAllocation) :
    MemoryRegisters addressLanguage testProfile :=
  backingAfter allocation.space allocation.base

private def AddressAllocation.updatedBytes (allocation : AddressAllocation) : Mem.Memory :=
  ⟨[(⟨0⟩, {Mem.candidate allocation.space allocation.base 1 1 with bytes := [.init 99 none]})], 1⟩

private theorem AddressAllocation.access (allocation : AddressAllocation) (byte : Fin 256) :
    (⟨[(⟨0⟩, {Mem.candidate allocation.space allocation.base 1 1 with bytes := [.init byte none]})], 1⟩ :
      Mem.Memory).access testProfile.memoryTarget allocation.range.ptr 1 1 = some (some (⟨0⟩, 0)) := by
  simp [AddressAllocation.range, backingRange, Mem.Memory.access, Mem.Memory.block, AList.lookup,
    Mem.candidate, Nat.mod_one]

private theorem AddressAllocation.accessBound (allocation : AddressAllocation) :
    allocation.bound.bytes.access testProfile.memoryTarget allocation.range.ptr 1 1 = some (some (⟨0⟩, 0)) :=
  allocation.access 42

private theorem AddressAllocation.loadBound (allocation : AddressAllocation) :
    allocation.bound.bytes.load testProfile.memoryTarget allocation.range.ptr 1 1 = some [.init 42 none] := by
  unfold Mem.Memory.load
  rw [allocation.accessBound]
  rfl

private theorem AddressAllocation.storeBound (allocation : AddressAllocation) :
    allocation.bound.bytes.store testProfile.memoryTarget allocation.range.ptr [.init 99 none] 1 =
      some allocation.updatedBytes := by
  unfold Mem.Memory.store
  rw [show ([Mem.MByte.init 99 none]).length = 1 from rfl, allocation.accessBound]
  rfl

private theorem AddressAllocation.storeScalar (allocation : AddressAllocation) (project : Project) :
    (addressMemory project).Writes (language := addressLanguage) allocation.before storePlace storeValue
      {allocation.bound with instruction := 1} := by
  have encoded : Hybrid.encode testProfile.memoryTarget (.int .u8 42) = some [.init 42 none] := rfl
  refine ⟨⟨rfl, fun _ => rfl⟩, (fun _ => ⟨⟨1, 1, .nil⟩, rfl, ⟨nofun, nofun⟩⟩),
    ?_, ?_, fun _ => Or.inr ⟨allocation.range, [(0, [.init 42 none])], rfl, rfl, ?_, ?_⟩⟩
  · simp [Memory.writeValue, Memory.markWritten, Memory.storeRuns, Memory.atOffset, Memory.mayInitialize, Memory.authorizeWrite, Memory.writeOwnerView, Memory.ownerView, Memory.zeroRoot, Memory.rangeAt, Memory.byteRuns, Memory.layoutAt,
      addressMemory, storingMemory, storePlace, AddressAllocation.before, AddressAllocation.bound,
      backingBefore, backingAfter, backingRange, storeBefore, storeInitial, storeRoot,
      Mem.Memory.store, Mem.Memory.access, Mem.Memory.block, Mem.Memory.modify,
      AList.lookup, AList.update, Mem.candidate, Nat.mod_one, addressLanguage, storeValue, encoded] <;> rfl
  · change (do
      let bytes ← allocation.bound.bytes.load testProfile.memoryTarget allocation.range.ptr 1 1
      let value ← Hybrid.decode testProfile.memoryTarget (.int .u8) bytes
      pure (CoreValue.scalar value : CoreValue Unit)) = some storeValue
    rw [allocation.loadBound]
    rfl
  · exact fun _ => ⟨0, [.init 42 none], List.mem_cons_self .., by decide⟩
  · intro offset bytes member
    cases List.mem_singleton.mp member
    refine ⟨Nat.le_refl 1, ?_, allocation.loadBound⟩
    intro byte included
    cases List.mem_singleton.mp included
    decide

private inductive AddressSourcePhase where
  | start | bound | reference | done

private def AddressSourcePhase.state : AddressSourcePhase → MemoryRegisters addressLanguage testProfile
  | .start => storeInitial
  | .bound => logicalAddressBefore
  | .reference => logicalAddressAfter
  | .done => logicalAddressFinal

private def AddressSourcePhase.next : AddressSourcePhase → Option AddressSourcePhase
  | .start => some .bound
  | .bound => some .reference
  | .reference => some .done
  | .done => none

private inductive AddressTargetPhase where
  | start
  | allocated (allocation : AddressAllocation)
  | bound (allocation : AddressAllocation)
  | ready (allocation : AddressAllocation)
  | reference (allocation : AddressAllocation)
  | done (allocation : AddressAllocation)

private def AddressTargetPhase.state : AddressTargetPhase → MemoryRegisters addressLanguage testProfile
  | .start => storeInitial
  | .allocated allocation => allocation.before
  | .bound allocation => allocation.bound
  | .ready allocation => addressReady allocation.bound
  | .reference allocation => (addressWritten allocation.bound allocation.base).advance
  | .done allocation => physicalAddressFinal allocation.bound allocation.base allocation.updatedBytes

private def AddressTargetPhase.source : AddressTargetPhase → AddressSourcePhase
  | .start | .allocated _ => .start
  | .bound _ | .ready _ => .bound
  | .reference _ => .reference
  | .done _ => .done

private inductive AddressTargetPhase.Tick : AddressTargetPhase → AddressTargetPhase → Prop
  | allocate (a) : Tick .start (.allocated a)
  | bind (a) : Tick (.allocated a) (.bound a)
  | beginDestination (a) : Tick (.bound a) (.ready a)
  | takeAddress (a) : Tick (.ready a) (.reference a)
  | indirect (a) : Tick (.reference a) (.done a)

private theorem physicalAddressExact (project : Project) (allocation : AddressAllocation)
    (next : MemoryRegisters addressLanguage testProfile) :
    (addressMemory project).InstructionStep (language := addressLanguage)
      (addressReady allocation.bound) next false addressInstruction ↔
    next = (AddressTargetPhase.reference allocation).state := by
  have resolved : (addressMemory project).resolveAddress
      (Memory.prepareAddresses (addressReady allocation.bound) [⟨(), storePlace⟩]) false
      ⟨(), storePlace⟩ = some (resolvedStore allocation.range) := by
    with_unfolding_all
      change (do
        let _ ← allocation.bound.bytes.access testProfile.memoryTarget allocation.range.ptr 1 1
        pure (resolvedStore allocation.range)) = _
    rw [allocation.accessBound]
    rfl
  constructor
  · rintro ⟨values, readState, addresses, result, effects, changed, stored, read, _, fetched, op, _, writes, nextEq⟩
    have readEq : some ([], addressReady allocation.bound) = some (values, readState) := read
    cases readEq
    have fetchedEq : some [resolvedStore allocation.range] = some addresses := by
      change (do
        let a ← (addressMemory project).resolveAddress
          (Memory.prepareAddresses (addressReady allocation.bound) [⟨(), storePlace⟩]) false ⟨(), storePlace⟩
        pure [a]) = some addresses at fetched
      rw [resolved] at fetched
      exact fetched
    cases fetchedEq
    have meaning : ((∃ range, [resolvedStore allocation.range] = [resolvedStore range] ∧
          result = CoreValue.scalar (.int .u8 range.ptr.addr)) ∨
        (([resolvedStore allocation.range] = [abstractStore] ∨
          [resolvedStore allocation.range] = [emptyStoreAddress]) ∧ result = CoreValue.other ())) ∧
        effects = [] ∧ changed = Memory.operationStore
          (Memory.prepareAddresses (addressReady allocation.bound) [⟨(), storePlace⟩]) false := by
      with_unfolding_all exact op
    rcases meaning with ⟨⟨range, operands, rfl⟩ | ⟨impossible, _⟩, rfl, rfl⟩
    · have same : allocation.range = range :=
        Option.some.inj (congrArg OperationAddress.range (List.cons.inj operands).1)
      subst range
      have written : some (addressWritten allocation.bound allocation.base) = some stored := writes.2.2.1
      cases written
      exact nextEq
    · rcases impossible with wrong | wrong
      all_goals
        have range := congrArg OperationAddress.range (List.cons.inj wrong).1
        cases range
  · rintro rfl
    refine ⟨[], _, [resolvedStore allocation.range], CoreValue.scalar (.int .u8 allocation.base), [], _,
      addressWritten allocation.bound allocation.base, rfl, rfl, ?_,
      ⟨Or.inl ⟨allocation.range, rfl, rfl⟩, rfl, rfl⟩, rfl, ?_, rfl⟩
    · change (do
        let a ← (addressMemory project).resolveAddress
          (Memory.prepareAddresses (addressReady allocation.bound) [⟨(), storePlace⟩]) false ⟨(), storePlace⟩
        pure [a]) = _
      rw [resolved]
      rfl
    · exact ⟨⟨rfl, fun _ => rfl⟩, nofun, rfl, rfl, nofun⟩

private theorem logicalWritesUnique (memory : Memory testLowering testProfile)
    (place : MemoryPlace Unit ()) (value : CoreValue Unit)
    (before started stored expected : MemoryRegisters addressLanguage testProfile)
    (unique : ∀ other, (if before.live.contains place.logical.root then other = before
      else memory.Begins (language := addressLanguage) before place.logical.root other) → other = started)
    (writes : memory.WriteParams (language := addressLanguage) [⟨(), place⟩]
      [value] before stored false)
    (writing : memory.writeValue (language := addressLanguage) started place value = some expected) :
    stored = expected := by
  cases writes with
  | cons begin write rest =>
    cases rest
    have starts := unique _ begin
    subst_vars
    exact Option.some.inj (write.2.2.1.symm.trans writing)


private abbrev logicalAddressView (project : Project) : Memory testLowering testProfile :=
  @MIR.logicalView addressLanguage testLowering testProfile (logicalAddressProgram project)

private def logicalAddressBlock : MemoryBlock testLowering :=
  let root : MemoryPlace Unit () := ⟨⟨storeRoot, []⟩, .virtual⟩
  let address : MemoryInst testLowering :=
    .op (opcode := .named ⟨"address"⟩) addressResultPlace () .nil [] [⟨(), root⟩]
  ⟨⟨0⟩, [], [⟨testOrigin, .bind root (.literal ⟨[42]⟩)⟩, ⟨testOrigin, address⟩,
    ⟨testOrigin, indirectInstruction⟩], .ret (some ⟨(), .copy addressResultPlace⟩), []⟩

private theorem logicalAddressExact (project : Project) (phase : AddressSourcePhase)
    (next : MemoryRegisters addressLanguage testProfile) :
    (logicalAddressProgram project).Step (language := addressLanguage) phase.state next ↔
      ∃ q, phase.next = some q ∧ q.state = next := by
  constructor
  · intro run
    cases phase with
    | start =>
      obtain ⟨_, value, state, stored, read, write, rfl⟩ := run
      have actual : some (storeValue, storeInitial) = some (value, state) := read
      cases actual
      have params : (logicalAddressView project).WriteParams (language := addressLanguage)
          [⟨(), ⟨⟨storeRoot, []⟩, .virtual⟩⟩] [storeValue] storeInitial stored false := by
        simp only [Bool.true_or, ite_true] at write
        with_unfolding_all exact write
      have unique : ∀ state, (logicalAddressView project).Begins
          (language := addressLanguage) storeInitial storeRoot state →
          state = {storeInitial with live := [storeRoot]} := by
        intro state h
        rcases h.2 with ⟨_, eq⟩ | ⟨bad, _⟩
        · exact eq
        · have impossible : some StorageClass.virtual = some .materialized := bad
          cases impossible
      have eq := logicalWritesUnique (logicalAddressView project) ⟨⟨storeRoot, []⟩, .virtual⟩
        storeValue storeInitial {storeInitial with live := [storeRoot]} stored
        {logicalAddressBefore with instruction := 0} unique params rfl
      cases eq
      exact ⟨.bound, rfl, rfl⟩
    | bound =>
      obtain ⟨_, values, state, operands, result, effects, changed, stored, read, _, resolved, op, _, write, rfl⟩ := run
      have actual : some ([], logicalAddressBefore) = some (values, state) := read
      cases actual
      have address : some [abstractStore] = some operands := by with_unfolding_all exact resolved
      cases address
      have meaning : ((∃ range, [abstractStore] = [resolvedStore range] ∧
            result = CoreValue.scalar (.int .u8 range.ptr.addr)) ∨
          (([abstractStore] = [abstractStore] ∨ [abstractStore] = [emptyStoreAddress]) ∧ result = CoreValue.other ())) ∧
          effects = [] ∧ changed = Memory.operationStore
            (Memory.prepareAddresses logicalAddressBefore [⟨(), ⟨⟨storeRoot, []⟩, .virtual⟩⟩]) true := by
        with_unfolding_all exact op
      rcases meaning with ⟨⟨_, impossible, _⟩ | ⟨_, rfl⟩, rfl, rfl⟩
      · have range := congrArg OperationAddress.range (List.cons.inj impossible).1
        cases range
      · let ready : MemoryRegisters addressLanguage testProfile := {logicalAddressBefore with
          addressIds := [(storeRoot, ⟨0⟩)]
          nextAddressId := 1}
        have params : (logicalAddressView project).WriteParams (language := addressLanguage)
            [⟨(), addressResultPlace⟩] [CoreValue.other ()] ready stored false := by
          simp only [Bool.true_or, ite_true] at write
          with_unfolding_all exact write
        have unique : ∀ state, (logicalAddressView project).Begins
            (language := addressLanguage) ready shadowRoot state →
            state = {ready with live := [shadowRoot, storeRoot]} := by
          intro state h
          rcases h.2 with ⟨_, eq⟩ | ⟨bad, _⟩
          · exact eq
          · have impossible : some StorageClass.virtual = some .materialized := bad
            cases impossible
        have eq := logicalWritesUnique (logicalAddressView project) addressResultPlace
          (CoreValue.other ()) ready {ready with live := [shadowRoot, storeRoot]} stored
          {logicalAddressAfter with instruction := 1} unique params rfl
        cases eq
        exact ⟨.reference, rfl, rfl⟩
    | reference =>
      obtain ⟨_, values, state, operands, result, effects, changed, stored, read, _, resolved, op, _, write, rfl⟩ := run
      have actual : some ([CoreValue.other ()], logicalAddressAfter) = some (values, state) := by
        with_unfolding_all exact read
      cases actual
      have address : some [] = some operands := resolved
      cases address
      let modified : OperationStore (CoreValue Unit) :=
        {Memory.operationStore (language := addressLanguage) logicalAddressAfter true with
          addressedValues := [(⟨0⟩, CoreValue.scalar (.int .u8 99))]}
      have meaning : ([] : List (OperationAddress Unit)) = [] ∧ result = storeValue ∧ effects = [] ∧
          (([CoreValue.other ()] = [CoreValue.other ()] ∧
            AList.lookup (Memory.operationStore logicalAddressAfter true).addressedValues ⟨0⟩ = some storeValue ∧
            changed = modified) ∨
            ∃ ptr bytes, [CoreValue.other ()] = [CoreValue.scalar (.int .u8 ptr.addr)] ∧
              logicalAddressAfter.bytes.load testProfile.memoryTarget ptr 1 1 = some [.init 42 none] ∧
              logicalAddressAfter.bytes.store testProfile.memoryTarget ptr [.init 99 none] 1 = some bytes ∧
              changed = {Memory.operationStore logicalAddressAfter true with bytes := bytes}) := by
        with_unfolding_all exact op
      obtain ⟨_, rfl, rfl, meaning⟩ := meaning
      rcases meaning with ⟨_, _, rfl⟩ | ⟨_, _, impossible, _⟩
      · let ready := Memory.applyOperation (language := addressLanguage) logicalAddressAfter true modified
        have params : (logicalAddressView project).WriteParams (language := addressLanguage)
            [⟨(), addressResultPlace⟩] [storeValue] ready stored false := by
          simp only [Bool.true_or, ite_true] at write
          with_unfolding_all exact write
        have eq := logicalWritesUnique (logicalAddressView project) addressResultPlace
          storeValue ready ready stored {logicalAddressFinal with instruction := 2}
          (fun _ h => h) params rfl
        cases eq
        exact ⟨.done, rfl, rfl⟩
      · cases impossible
    | done =>
      obtain ⟨block, found, step⟩ := run
      have pulled : some logicalAddressBlock = some block := by with_unfolding_all exact found
      cases pulled
      with_unfolding_all
        change _ ∧ (logicalAddressView project).Return (language := addressLanguage)
          (some ⟨(), .copy addressResultPlace⟩) logicalAddressFinal next at step
      obtain ⟨_, caller, rest, _, _, _, callers, _⟩ := step
      change [] = caller :: rest at callers
      cases callers
  · rintro ⟨q, step, rfl⟩
    cases phase
    · have same : some AddressSourcePhase.bound = some q := step
      cases same
      with_unfolding_all
        refine ⟨rfl, storeValue, storeInitial, {logicalAddressBefore with instruction := 0}, rfl, ?_, rfl⟩
      exact Memory.WriteParams.cons (language := addressLanguage)
        (started := {storeInitial with live := [storeRoot]}) ⟨by decide, Or.inl ⟨rfl, rfl⟩⟩
        ⟨⟨rfl, fun _ => rfl⟩, nofun, rfl, rfl, nofun⟩ (.nil _)
    · have same : some AddressSourcePhase.reference = some q := step
      cases same
      exact (logicalAddressStep project).1
    · have same : some AddressSourcePhase.done = some q := step
      cases same
      refine ⟨rfl, [CoreValue.other ()], logicalAddressAfter, [], storeValue, [], _,
        {logicalAddressFinal with instruction := 2}, rfl, rfl, rfl,
        ⟨rfl, rfl, rfl, Or.inl ⟨rfl, rfl, rfl⟩⟩, rfl, ?_, rfl⟩
      with_unfolding_all
        exact Memory.WriteParams.cons (language := addressLanguage) rfl
          ⟨⟨rfl, fun _ => rfl⟩, nofun, rfl, rfl, nofun⟩ (.nil _)
    · have impossible : (none : Option AddressSourcePhase) = some q := step
      cases impossible


private theorem AddressAllocation.pointerUnique (a : AddressAllocation) (ptr : Mem.Ptr)
    (base : ptr.addr = a.base)
    (load : a.bound.bytes.load testProfile.memoryTarget ptr 1 1 = some [.init 42 none]) :
    ptr = a.range.ptr := by
  obtain ⟨space, address, provenance⟩ := ptr
  change address = a.base at base
  subst address
  cases provenance with
  | none =>
    have impossible : (none : Option (List Mem.MByte)) = some [.init 42 none] := load
    cases impossible
  | some block =>
    unfold Mem.Memory.load Mem.Memory.access at load
    simp only [Nat.one_ne_zero, ite_false] at load
    -- Select the only live byte block in the allocation.
    by_cases same : (⟨0⟩ : BlockId) = block
    · subst block
      by_cases inSpace : a.space = space
      · subst space
        rfl
      · simp [AddressAllocation.bound, backingAfter, backingBefore, Mem.Memory.block,
          AList.lookup, Mem.candidate, inSpace] at load
    · simp [AddressAllocation.bound, backingAfter, backingBefore, Mem.Memory.block,
        AList.lookup, same] at load

private theorem physicalIndirectExact (project : Project) (a : AddressAllocation)
    (next : MemoryRegisters addressLanguage testProfile) :
    (addressMemory project).InstructionStep (language := addressLanguage)
      (AddressTargetPhase.reference a).state next false indirectInstruction ↔
    next = (AddressTargetPhase.done a).state := by
  let before := (AddressTargetPhase.reference a).state
  let storedState : MemoryRegisters addressLanguage testProfile :=
    {(AddressTargetPhase.done a).state with instruction := 4}
  constructor
  · rintro ⟨values, afterRead, operands, result, effects, changed, stored, reads, _, resolves, op, _, writes, ends⟩
    have read : some ([CoreValue.scalar (.int .u8 a.base)], before) = some (values, afterRead) := reads
    cases read
    have resolve : some [] = some operands := resolves
    cases resolve
    have meaning : ([] : List (OperationAddress Unit)) = [] ∧ result = storeValue ∧ effects = [] ∧
        (([CoreValue.scalar (.int .u8 a.base)] = [CoreValue.other ()] ∧
          AList.lookup (Memory.operationStore before false).addressedValues ⟨0⟩ = some storeValue ∧
          changed = {Memory.operationStore before false with
            addressedValues := AList.update (Memory.operationStore before false).addressedValues ⟨0⟩
              (fun _ => CoreValue.scalar (.int .u8 99))}) ∨
          ∃ ptr bytes, [(CoreValue.scalar (.int .u8 a.base) : CoreValue Unit)] = [CoreValue.scalar (.int .u8 ptr.addr)] ∧
            a.bound.bytes.load testProfile.memoryTarget ptr 1 1 = some [.init 42 none] ∧
            a.bound.bytes.store testProfile.memoryTarget ptr [.init 99 none] 1 = some bytes ∧
            changed = {Memory.operationStore before false with bytes := bytes}) := by
      with_unfolding_all exact op
    obtain ⟨_, rfl, rfl, meaning⟩ := meaning
    rcases meaning with ⟨impossible, _⟩ | ⟨ptr, bytes, argument, loaded, written, rfl⟩
    · cases impossible
    · have addr : ptr.addr = a.base := by
        have equal := (List.cons.inj argument).1
        injection equal with el
        injection el with _ number
        exact_mod_cast number.symm
      have actual := a.pointerUnique ptr addr loaded
      subst ptr
      have exactBytes := Option.some.inj (a.storeBound.symm.trans written)
      subst bytes
      have exactWrite : some storedState = some stored := by
        with_unfolding_all exact writes.2.2.1
      cases exactWrite
      exact ends
  · rintro rfl
    refine ⟨[CoreValue.scalar (.int .u8 a.base)], before, [], storeValue, [], _, storedState,
      rfl, rfl, rfl, ⟨rfl, rfl, rfl, Or.inr ⟨a.range.ptr, a.updatedBytes, rfl, ?_, ?_, rfl⟩⟩, rfl, ?_, rfl⟩
    · exact a.loadBound
    · exact a.storeBound
    · exact ⟨⟨rfl, fun _ => rfl⟩, nofun, rfl, rfl, nofun⟩

private theorem addressTargetNormal (project : Project) (phase : AddressTargetPhase)
    (next : MemoryRegisters addressLanguage testProfile) :
    (addressMemory project).StepOutcome (language := addressLanguage) phase.state next false ↔
      ∃ q, phase.Tick q ∧ q.state = next := by
  have block : (addressMemory project).blockAt phase.state = some addressBlock := by
    cases phase <;> rfl
  constructor
  · rintro ⟨actual, fetched, step⟩
    rw [block] at fetched
    have equal := Option.some.inj fetched
    subst actual
    cases phase with
    | start =>
      change (∃ begun, (addressMemory project).Begins (language := addressLanguage)
        storeInitial storeRoot begun ∧ next = begun.advance) at step
      obtain ⟨begun, begins, rfl⟩ := step
      rcases begins.2 with ⟨impossible, _⟩ | ⟨_, layout, found, zero | allocated⟩
      · have wrong : some StorageClass.materialized = some .virtual := impossible
        cases wrong
      · have same : some (⟨1, 1, .nil⟩ : Layout) = some layout := found
        cases same
        exact (Nat.one_ne_zero zero.1).elim
      · have same : some (⟨1, 1, .nil⟩ : Layout) = some layout := found
        cases same
        obtain ⟨_, space, base, id, bytes, alloc, rfl⟩ := allocated
        have result : Mem.Memory.empty.alloc testProfile.memoryTarget space base 1 1 = .ok (id, bytes) := alloc
        unfold Mem.Memory.alloc at result
        dsimp only at result
        split at result
        · rename_i fits
          cases result
          exact ⟨.allocated ⟨space, base, fits⟩, .allocate _, rfl⟩
        · contradiction
    | allocated a =>
      change (addressMemory project).InstructionStep (language := addressLanguage)
        a.before next false (.bind storePlace (.literal ⟨[42]⟩)) at step
      obtain ⟨value, afterRead, stored, read, write, rfl⟩ := step
      have same : some (storeValue, a.before) = some (value, afterRead) := read
      cases same
      have exactWrite := (a.storeScalar project).2.2.1.symm.trans write.2.2.1
      cases exactWrite
      exact ⟨.bound a, .bind _, rfl⟩
    | bound a =>
      change (∃ begun, (addressMemory project).Begins (language := addressLanguage)
        a.bound shadowRoot begun ∧ next = begun.advance) at step
      obtain ⟨begun, begins, rfl⟩ := step
      rcases begins.2 with ⟨_, rfl⟩ | ⟨bad, _⟩
      · exact ⟨.ready a, .beginDestination _, rfl⟩
      · have impossible : some StorageClass.virtual = some .materialized := bad
        cases impossible
    | ready a =>
      have nextIs := (physicalAddressExact project a next).1 step
      exact ⟨.reference a, .takeAddress _, nextIs.symm⟩
    | reference a =>
      have nextIs := (physicalIndirectExact project a next).1 step
      exact ⟨.done a, .indirect _, nextIs.symm⟩
    | done a =>
      change _ ∧ (addressMemory project).Return (language := addressLanguage)
        (some ⟨(), .copy addressResultPlace⟩) (AddressTargetPhase.done a).state next at step
      obtain ⟨_, caller, rest, _, _, _, callers, _⟩ := step
      change [] = caller :: rest at callers
      cases callers
  · rintro ⟨q, tick, rfl⟩
    refine ⟨addressBlock, block, ?_⟩
    cases tick with
    | allocate a =>
      refine ⟨{a.before with instruction := 0}, ⟨by decide, Or.inr ⟨rfl, ⟨1, 1, .nil⟩, rfl, Or.inr ?_⟩⟩, rfl⟩
      refine ⟨by decide, a.space, a.base, ⟨0⟩, a.before.bytes, ?_, rfl⟩
      change Mem.Memory.empty.alloc testProfile.memoryTarget a.space a.base 1 1 = _
      unfold Mem.Memory.alloc
      have fits : Mem.Memory.empty.Fits testProfile.memoryTarget (Mem.candidate a.space a.base 1 1) := a.fits
      simp only [fits, ite_true]
      rfl
    | bind a => exact ⟨storeValue, a.before, {a.bound with instruction := 1}, rfl, a.storeScalar project, rfl⟩
    | beginDestination a =>
      exact ⟨{a.bound with live := [shadowRoot, storeRoot]}, ⟨nofun, Or.inl ⟨rfl, rfl⟩⟩, rfl⟩
    | takeAddress a => exact (physicalAddressExact project a _).2 rfl
    | indirect a => exact (physicalIndirectExact project a _).2 rfl


private theorem addressTargetExact (project : Project) (phase : AddressTargetPhase)
    (next : MemoryRegisters addressLanguage testProfile) :
    (addressMemory project).Step (language := addressLanguage) phase.state next ↔
      ∃ q, phase.Tick q ∧ q.state = next := by
  constructor
  · intro tick
    apply (addressTargetNormal project phase next).mp
    apply tick.normal (language := addressLanguage)
    intro partialState failed
    have block : (addressMemory project).blockAt phase.state = some addressBlock := by cases phase <;> with_unfolding_all rfl
    cases phase with
    | start =>
      have ok := (addressTargetNormal project .start (.allocated ⟨⟨0⟩, 1, by unfold backingFits; decide⟩ : AddressTargetPhase).state).mpr
        ⟨_, .allocate _, rfl⟩
      have exhausted : (addressMemory project).BeginExhausted (language := addressLanguage) storeInitial storeRoot :=
        failed.instructionAt (op := ⟨testOrigin, .beginLifetime storeRoot⟩) block (by with_unfolding_all rfl)
      obtain ⟨started, begins, _⟩ := ok.instructionAt (op := ⟨testOrigin, .beginLifetime storeRoot⟩) block (by with_unfolding_all rfl)
      exact exhausted.unavailable ⟨started, begins⟩
    | allocated a =>
      exact Memory.bind_cannot_exhaust (language := addressLanguage)
        (failed.instructionAt (op := ⟨testOrigin, .bind storePlace (.literal ⟨[42]⟩)⟩) block (by with_unfolding_all rfl))
    | bound a =>
      have bad : (addressMemory project).BeginExhausted (language := addressLanguage) a.bound shadowRoot :=
        failed.instructionAt (op := ⟨testOrigin, .beginLifetime shadowRoot⟩) block (by with_unfolding_all rfl)
      have no : some StorageClass.virtual = some .materialized := by with_unfolding_all exact bad.materialized
      contradiction
    | ready a =>
      exact Memory.op_cannot_exhaust (language := addressLanguage)
        (failed.instructionAt (op := ⟨testOrigin, addressInstruction⟩) block (by with_unfolding_all rfl))
    | reference a =>
      exact Memory.op_cannot_exhaust (language := addressLanguage)
        (failed.instructionAt (op := ⟨testOrigin, indirectInstruction⟩) block (by with_unfolding_all rfl))
    | done a =>
      obtain ⟨caller, rest, _, _, _, callers, _⟩ := failed.terminatorAt block (by with_unfolding_all rfl)
      change [] = caller :: rest at callers
      cases callers
  · rintro ⟨q, tick, rfl⟩
    have normal := (addressTargetNormal project phase q.state).mpr ⟨q, tick, rfl⟩
    cases tick <;> exact normal

private theorem addressStatic (project : Project) :
    PreservesOrigins (logicalAddressProgram project).origins (addressMemory project).origins ∧
    LayoutsMatch addressLanguage.layout (addressMemory project) ∧
    PromotionSound (language := addressLanguage) (logicalAddressProgram project) (addressMemory project) ∧
    (addressMemory project).TablesWF ∧ (logicalAddressView project).TablesWF := by
  refine ⟨⟨?_, ?_⟩, ⟨?_, ?_⟩, ?_, ?_, ?_⟩
  · intro origin member
    change origin ∈ [testOrigin, testOrigin, testOrigin, testOrigin, testOrigin] at member
    have equal : origin = testOrigin := by simpa using member
    exact List.mem_cons.mpr (Or.inl equal)
  · intro origin member
    have list : origin ∈ [testOrigin, testOrigin, testOrigin] := member
    have equal : origin = testOrigin := by simpa using list
    exact List.mem_cons.mpr (Or.inl equal)
  · intro fn locals graph included root ty declared
    cases List.mem_singleton.mp included
    rcases List.mem_cons.mp declared with h | h
    · cases h
      exact ⟨_, rfl, Or.inr ⟨rfl, _, _, rfl, rfl, rfl⟩⟩
    · cases List.mem_singleton.mp h
      exact ⟨_, rfl, Or.inl rfl⟩
  · intro fn slots root layout listed slot
    cases List.mem_singleton.mp listed
    cases List.mem_singleton.mp slot
    exact ⟨_, _, (), _, List.mem_singleton_self _, List.mem_cons_self .., rfl, rfl, rfl⟩
  · have occurrences : (logicalAddressProgram project).operations =
        [⟨Exec.Demo.mainF, ⟨0⟩, 1, testOrigin, .operator (.named ⟨"address"⟩),
          [((), .address storeRoot [])], []⟩,
         ⟨Exec.Demo.mainF, ⟨0⟩, 2, testOrigin, .operator (.named ⟨"indirectReadWrite"⟩),
          [((), .copy shadowRoot [])], []⟩] := by with_unfolding_all rfl
    have escapes : (logicalAddressProgram project).Escapes (language := addressLanguage)
        Exec.Demo.mainF storeRoot := by
      refine ⟨_, occurrences ▸ List.mem_cons_self .., rfl, (), [], List.mem_singleton_self _⟩
    have noEscape : ¬ (logicalAddressProgram project).Escapes (language := addressLanguage)
        Exec.Demo.mainF shadowRoot := by
      rintro ⟨op, member, _, ty, path, argument⟩
      rw [occurrences] at member
      rcases List.mem_cons.mp member with rfl | rest
      · have same := congrArg Prod.snd (List.mem_singleton.mp argument)
        cases same
      · cases List.mem_singleton.mp rest
        have same := congrArg Prod.snd (List.mem_singleton.mp argument)
        cases same
    constructor
    · intro fn locals graph known place ty declared
      cases List.mem_singleton.mp known
      rcases List.mem_cons.mp declared with h | h
      · cases h
        exact ⟨_, .materialized, rfl, rfl, ⟨nofun, fun h => False.elim (h.2 escapes)⟩⟩
      · cases List.mem_singleton.mp h
        exact ⟨_, .virtual, rfl, rfl, ⟨fun _ => ⟨rfl, noEscape⟩, fun _ => rfl⟩⟩
    · intro fn classes place listed declared
      cases List.mem_singleton.mp listed
      rcases List.mem_cons.mp declared with h | h
      · cases h
      · cases List.mem_singleton.mp h
        exact ⟨(), _, _, _, _, List.mem_singleton_self _, List.mem_cons_of_mem _ (List.mem_singleton_self _),
          List.mem_singleton_self _, List.mem_cons_of_mem _ (List.mem_singleton_self _), rfl, noEscape⟩
  all_goals
    refine ⟨(by simp : [_].Nodup), (by simp : [_].Nodup), (by simp : [_].Nodup), ?_, ?_, ?_, ?_⟩
    · first | exact (by simp : [_].Nodup) | exact List.nodup_nil
    · intro fn classes membership
      cases List.mem_singleton.mp membership
      exact (by decide : [storeRoot, shadowRoot].Nodup)
    · intro fn slots membership
      first | (cases List.mem_singleton.mp membership; exact (by simp : [_].Nodup)) | exact (List.not_mem_nil membership).elim
    · intro fn locals graph member
      have logicalFunctions : (logicalAddressView project).functions =
          [(Exec.Demo.mainF, [(storeRoot, ()), (shadowRoot, ())], ⟨⟨0⟩, #[logicalAddressBlock]⟩)] := by
        with_unfolding_all rfl
      try rw [logicalFunctions] at member
      cases List.mem_singleton.mp member
      refine ⟨(by decide : [storeRoot, shadowRoot].Nodup),
        ⟨(by simp : [_].Nodup), ⟨_, List.mem_singleton_self _, rfl⟩, ?_⟩,
        _, rfl, ⟨_, List.mem_singleton_self _, rfl, rfl⟩, ?_⟩
      · intro block included
        cases List.mem_singleton.mp included
        exact fun _ => nofun
      · intro block included
        cases List.mem_singleton.mp included
        refine ⟨rfl, ?_⟩
        intro inst present
        first
        | have list : inst ∈ addressBlock.body := present
          simp only [addressBlock, List.mem_cons, List.mem_nil_iff, or_false] at list
          rcases list with rfl | rfl | rfl | rfl | rfl <;> trivial
        | have list : inst ∈ logicalAddressBlock.body := present
          simp only [logicalAddressBlock, List.mem_cons, List.mem_nil_iff, or_false] at list
          rcases list with rfl | rfl | rfl <;> trivial

private theorem AddressAllocation.loadUpdated (a : AddressAllocation) :
    a.updatedBytes.load testProfile.memoryTarget a.range.ptr 1 1 = some [.init 99 none] := by
  have access : a.updatedBytes.access testProfile.memoryTarget a.range.ptr 1 1 = some (some (⟨0⟩, 0)) :=
    a.access 99
  unfold Mem.Memory.load
  rw [access]
  rfl

private theorem addressRepresents (project : Project) (q : AddressTargetPhase) :
    (addressMemory project).RepresentsActivation (language := addressLanguage)
      q.source.state.toMemoryFrame q.state.toMemoryFrame q.state.bytes q.state.toMemoryFrame.backingRanges := by
  refine ⟨⟨⟨?_, ?_, ?_, ?_⟩, ?_⟩, ?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_⟩
  · cases q with
    | start => exact emptyBytesWF
    | allocated a => exact allocatedBytesWF a.space a.base a.fits
    | done a => exact storedBytesWF a.space a.base a.fits 99
    | _ a => exact storedBytesWF a.space a.base a.fits
  · cases q <;> first
    | exact List.nodup_nil
    | exact (by simp : [storeRoot].Nodup)
    | exact (by decide : [shadowRoot, storeRoot].Nodup)
  · cases q <;> first
    | exact List.nodup_nil
    | exact (by simp : [storeRoot].Nodup)
    | exact (by decide : [shadowRoot, storeRoot].Nodup)
  · cases q <;> first
    | exact fun _ h => (List.not_mem_nil h).elim
    | exact fun _ h => h
    | exact fun _ h => List.mem_cons_of_mem _ h
  · cases q <;> rfl
  · cases q <;> rfl
  · cases q <;> first
    | exact fun _ h => h
    | exact fun _ h => List.mem_cons_of_mem _ h
  · cases q <;> first
    | exact List.nodup_nil
    | exact (by simp : [storeRoot].Nodup)
    | exact (by decide : [shadowRoot, storeRoot].Nodup)
  · cases q <;> first | exact List.nodup_nil | exact (by simp : [storeRoot].Nodup)
  · cases q <;> first | exact List.nodup_nil | exact (by simp : [shadowRoot].Nodup)
  · cases q <;> exact List.nodup_nil
  · cases q <;> exact nofun
  · intro place
    cases q <;> first
    | exact ⟨fun h => (List.not_mem_nil h).elim, fun h => Bool.noConfusion (show false = true from h)⟩
    | (change (place ∈ [storeRoot]) ↔
          (AList.lookup [(storeRoot, storeValue)] place).isSome = true
       by_cases equal : storeRoot = place
       · subst place; exact ⟨fun _ => rfl, fun _ => List.mem_singleton_self _⟩
       · simp [AList.lookup, equal, Ne.symm equal])
    | (by_cases eqShadow : shadowRoot = place
       · subst place; exact ⟨fun _ => rfl, fun _ => List.mem_cons_self ..⟩
       · by_cases eqRoot : storeRoot = place
         · subst place; exact ⟨fun _ => rfl, fun _ => List.mem_cons_of_mem _ (List.mem_singleton_self _)⟩
         · with_unfolding_all
             simp [AddressSourcePhase.state, AddressTargetPhase.state, AddressTargetPhase.source,
               logicalAddressAfter, logicalAddressFinal, physicalAddressFinal, addressWritten, addressReady, MemoryRegisters.advance,
               AddressAllocation.bound, backingAfter, backingBefore,
               storeBefore, storeInitial, AList.lookup, eqRoot, eqShadow, Ne.symm eqRoot, Ne.symm eqShadow])
  · intro place value member
    have live : place ∈ List.map Prod.fst q.source.state.virtualValues :=
      List.mem_map.mpr ⟨(place, value), member, rfl⟩
    cases q <;> exact live
  · intro place value member
    cases q <;> first
    | exact (List.not_mem_nil member).elim
    | (cases List.mem_singleton.mp member
       exact ⟨List.mem_cons_self .., rfl⟩)
  · intro place range member
    cases q
    · exact (List.not_mem_nil member).elim
    all_goals
      cases List.mem_singleton.mp member
      refine ⟨?_, rfl, ⟨⟨1, 1, .nil⟩, ⟨0⟩, _, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩⟩
      first | exact List.mem_singleton_self _ | exact List.mem_cons_of_mem _ (List.mem_singleton_self _)
  · intro place member
    cases q with
    | start => exact (List.not_mem_nil member).elim
    | allocated a =>
      cases List.mem_singleton.mp member
      exact ⟨_, _, (), .materialized, List.mem_singleton_self _, rfl, rfl,
        fun _ => ⟨⟨1, 1, .nil⟩, rfl, ⟨nofun, nofun⟩⟩, fun _ => nofun⟩
    | bound a | ready a | reference a | done a =>
      have membership : place = storeRoot ∨ place = shadowRoot := by
        first
        | exact Or.inl (List.mem_singleton.mp member)
        | rcases List.mem_cons.mp member with eq | tail
          · exact Or.inr eq
          · exact Or.inl (List.mem_singleton.mp tail)
      rcases membership with rfl | rfl
      all_goals
        first
        | (refine ⟨_, _, (), .materialized, List.mem_singleton_self _, rfl, rfl,
              fun _ => ⟨⟨1, 1, .nil⟩, rfl, ⟨nofun, nofun⟩⟩, ?_⟩
           intro value found
           first
           | (have same : some storeValue = some value := found
              cases same
              exact ⟨storeValue, rfl, (a.storeScalar project).2.2.2⟩)
           | (have same : some (CoreValue.scalar (.int .u8 99)) = some value := found
              cases same
              refine ⟨CoreValue.scalar (.int .u8 99), rfl, ?_, ?_⟩
              · change (do
                  let bytes ← a.updatedBytes.load testProfile.memoryTarget a.range.ptr 1 1
                  let value ← Hybrid.decode testProfile.memoryTarget (.int .u8) bytes
                  pure (CoreValue.scalar value : CoreValue Unit)) = some (CoreValue.scalar (.int .u8 99))
                rw [a.loadUpdated]
                rfl
              · intro _
                refine Or.inr ⟨a.range, [(0, [.init 99 none])], rfl, rfl,
                  fun _ => ⟨0, [.init 99 none], List.mem_cons_self .., by decide⟩, ?_⟩
                intro offset bytes written
                cases List.mem_singleton.mp written
                refine ⟨Nat.le_refl 1, ?_, a.loadUpdated⟩
                intro byte included
                cases List.mem_singleton.mp included
                decide))
        | (refine ⟨_, _, (), .virtual, List.mem_singleton_self _, rfl, rfl, nofun, ?_⟩
           intro value found
           first
           | exact (List.not_mem_nil member).elim
           | (have impossible : (none : Option (CoreValue Unit)) = some value := found; cases impossible)
           | (have same : some (CoreValue.other ()) = some value := found
              cases same
              exact ⟨CoreValue.scalar (.int .u8 a.base), rfl, rfl, nofun⟩)
           | (have same : some storeValue = some value := found
              cases same
              exact ⟨storeValue, rfl, rfl, nofun⟩))


private theorem addressCheckpoint (project : Project) (q : AddressTargetPhase) :
    (addressMemory project).RepresentsCheckpoint q.source.state q.state := by
  refine ⟨?_, ?_, ?_, ?_, ?_, ?_, ?_⟩
  · have empty : q.state.callers = [] := by cases q <;> rfl
    change (addressMemory project).RepresentsActivation (language := addressLanguage)
      q.source.state.toMemoryFrame q.state.toMemoryFrame q.state.bytes
      (q.state.toMemoryFrame.backingRanges ++ q.state.callers.flatMap (·.caller.backingRanges))
    rw [empty, List.flatMap_nil, List.append_nil]
    exact addressRepresents project q
  · cases q <;> rfl
  · cases q <;> exact nofun
  · cases q <;> first
    | exact ⟨rfl, fun _ _ h => (List.not_mem_nil h).elim⟩
    | (refine ⟨rfl, ?_⟩
       intro root id member
       cases List.mem_singleton.mp member
       exact ⟨List.mem_cons_of_mem _ (List.mem_singleton_self _), Or.inr rfl⟩)
  · cases q <;> first | exact List.nodup_nil | exact (by simp : [(⟨0⟩ : Hybrid.Place)].Nodup)
  · cases q <;> rfl
  · cases q <;> rfl


-- The finite phase analysis below certifies the registered executions. It
-- covers every reachable machine state, including every legal allocation.
private theorem reachedPhase {A E P : Type} (exec : Concrete.Contracts.Semantics A E)
    (input : A) (state : P → exec.State) (start : P)
    (initial : ∀ s, exec.initial input s → state start = s)
    (closed : ∀ p s, exec.step input (state p) s → ∃ q, state q = s)
    {s : exec.State} (finite : exec.finite input s) : ∃ p, state p = s := by
  obtain ⟨first, begins, steps⟩ := finite
  have fromPhase {a b : exec.State} (path : Concrete.Contracts.Steps (exec.step input) a b) :
      (∃ p, state p = a) → ∃ p, state p = b := by
    induction path with
    | nil => exact id
    | cons step _ ih =>
      rintro ⟨p, rfl⟩
      exact ih (closed p _ step)
  exact fromPhase steps ⟨start, initial first begins⟩

private theorem finiteProgressRefines {A B E : Type} (source : Concrete.Contracts.Semantics A E)
    (target : Concrete.Contracts.Semantics B E) (input : A) (output : B)
    (result : Concrete.Contracts.Observation)
    (p : source.State) (pReach : source.finite input p) (pResult : source.result input p result)
    (progress : ∀ q, target.finite output q → (∃ q', target.step output q q') ∨ target.result output q result)
    (exactResult : ∀ q o, target.finite output q → target.result output q o →
      o = result ∧ source.trace input p = target.trace output q)
    (rank : target.State → Nat)
    (decrease : ∀ q q', target.finite output q → target.step output q q' → rank q' < rank q) :
    BehaviorRefinement source target source.result target.result input output := by
  intro _
  refine ⟨?_, ?_, ?_⟩
  · intro q o finite observes
    obtain ⟨rfl, trace⟩ := exactResult q o finite observes
    exact ⟨p, pReach, pResult, trace⟩
  · intro run diverges
    have extend {a b c : target.State}
        (path : Concrete.Contracts.Steps (target.step output) a b)
        (tick : target.step output b c) : Concrete.Contracts.Steps (target.step output) a c := by
      induction path with
      | nil => exact .cons tick (.nil _)
      | cons first _ ih => exact .cons first (ih tick)
    have reach (i : Nat) : target.finite output (run i) := by
      induction i with
      | zero => exact ⟨_, diverges.1, .nil _⟩
      | succ i ih =>
        obtain ⟨entry, begins, path⟩ := ih
        exact ⟨entry, begins, extend path (diverges.2 i)⟩
    have bound (i : Nat) : rank (run i) + i ≤ rank (run 0) := by
      induction i with
      | zero => simp
      | succ i ih =>
        have strictly := decrease _ _ (reach i) (diverges.2 i)
        omega
    have impossible := bound (rank (run 0) + 1)
    omega
  · intro q stuck
    rcases progress q stuck.1 with ⟨next, tick⟩ | terminal
    · exact stuck.2.1 next tick
    · exact stuck.2.2 result terminal

private abbrev bindingMIR := MIR.Execution (language := erasureLanguage)
  (lang := testLowering) (profile := testProfile)

private abbrev bindingMemory := Memory.Execution (language := erasureLanguage)
  (lang := testLowering) (profile := testProfile)

private def bindingResult : Concrete.Contracts.Observation := .returned 0 ByteArray.empty

private theorem bindingMIRTick (project : Project) :
    bindingMIR.step (erasureSource project) storeInitial erasureBound := by
  exact ⟨by with_unfolding_all rfl, erasureBinding project, ⟨[], by with_unfolding_all rfl⟩⟩

private theorem materializationTick (project : Project) {p q : Materialization} (tick : p.Tick q) :
    bindingMemory.step (materializedBinding project) p.state q.state := by
  refine ⟨?_, (materializationExact project p q.state).mpr ⟨q, tick, rfl⟩, ?_⟩
  · cases tick <;> with_unfolding_all rfl
  · cases tick <;> exact ⟨[], by with_unfolding_all rfl⟩

private theorem materializationReachable (project : Project) (q : bindingMemory.State)
    (finite : bindingMemory.finite (materializedBinding project) q) :
    ∃ phase : Materialization, phase.state = q :=
  reachedPhase bindingMemory _ Materialization.state .initial (fun _ h => by with_unfolding_all exact h.symm)
    (fun p next h => by
      obtain ⟨phase, _, state⟩ := (materializationExact project p next).mp h.2.1
      exact ⟨phase, state⟩) finite

set_option maxRecDepth 4096 in
set_option maxHeartbeats 2000000 in
example (project : Project) :
    LowersMemory (language := erasureLanguage) (erasureSource project) (materializedBinding project) := by
  have static := materializationStatic project
  refine ⟨rfl, rfl, static.1, static.2.1, ⟨rfl, rfl⟩, static.2.2,
    (materializedTables project).1, (materializedTables project).2, ?_, ?_⟩
  · apply matchingRefinement (language := erasureLanguage)
    refine finiteProgressRefines bindingMIR bindingMemory _ _ bindingResult erasureBound
      ⟨storeInitial, by with_unfolding_all rfl, .cons (bindingMIRTick project) (.nil _)⟩
      (by with_unfolding_all rfl) ?_ ?_ (fun state => 2 - state.instruction) ?_
    · intro q finite
      obtain ⟨phase, rfl⟩ := materializationReachable project q finite
      cases phase with
      | initial =>
        let fits : backingFits ⟨0⟩ 1 := by unfold backingFits; decide
        exact .inl ⟨_, materializationTick project (.allocate ⟨0⟩ 1 fits)⟩
      | allocated space base fits =>
        exact .inl ⟨_, materializationTick project (.bind space base fits)⟩
      | bound => exact .inr (by with_unfolding_all rfl)
    · intro q o finite result
      obtain ⟨phase, rfl⟩ := materializationReachable project q finite
      cases phase
      all_goals first
        | (have impossible : none = some o := by with_unfolding_all exact result
           cases impossible)
        | (have same : some bindingResult = some o := by with_unfolding_all exact result
           exact ⟨(Option.some.inj same).symm, by with_unfolding_all rfl⟩)
    · intro q q' finite step
      obtain ⟨phase, rfl⟩ := materializationReachable project q finite
      obtain ⟨next, tick, rfl⟩ := (materializationExact project phase q').mp step.2.1
      cases tick <;> first
        | exact (by decide : 0 < 1)
        | exact (by decide : 1 < 2)
        | exact (by decide : 2 < 3)
        | exact (by decide : 3 < 4)
        | exact (by decide : 4 < 5)
  · intro _
    let related : bindingMIR.State → bindingMemory.State → Prop :=
      fun p q => ∃ phase : Materialization, materializedSourceState phase.finished = p ∧ phase.state = q
    refine ⟨⟨related, (fun _ q => 2 - q.instruction), ?_, ?_, ?_⟩, ?_⟩
    · intro q initial
      have same : q = storeInitial := initial
      subst q
      exact ⟨storeInitial, by with_unfolding_all rfl, .initial, rfl, rfl⟩
    · rintro _ _ q' _ _ ⟨phase, rfl, rfl⟩ step
      obtain ⟨next, tick, rfl⟩ := (materializationExact project phase q').mp step.2.1
      cases tick with
      | allocate space base fits =>
        exact .inr ⟨⟨.allocated space base fits, rfl, rfl⟩, (by decide : 1 < 2)⟩
      | bind space base fits =>
        exact .inl ⟨_, _, .nil _, bindingMIRTick project, .bound space base fits, rfl, rfl⟩
    · rintro _ _ o _ _ ⟨phase, rfl, rfl⟩ observed
      cases phase
      all_goals first
        | (have absent : none = some o := by with_unfolding_all exact observed
           cases absent)
        | (have same : some bindingResult = some o := by with_unfolding_all exact observed
           obtain rfl := Option.some.inj same
           exact ⟨_, .nil _, by with_unfolding_all rfl, by with_unfolding_all rfl⟩)
    · rintro _ _ _ _ ⟨phase, rfl, rfl⟩
      refine ⟨⟨materializationRepresents project phase, ?_, ?_, ?_, ?_, ?_, ?_⟩, ?_⟩
      all_goals cases phase <;> first | rfl | exact ⟨rfl, nofun⟩ | exact List.nodup_nil | exact nofun

-- This mutant returns the right value and retains the declaration and layout.
-- The actual checkpoint relation rejects removing the live aggregate.
set_option maxRecDepth 4096 in
example (project : Project) :
    ¬ LowersMemory (language := erasureLanguage) (erasureSource project) (immediateReturn project) := by
  intro lowers
  have starts : bindingMIR.initial (erasureSource project) storeInitial := by with_unfolding_all rfl
  have terminal : bindingMIR.result (erasureSource project) erasureBound bindingResult := by
    with_unfolding_all rfl
  have safe : ProgramSafe bindingMIR (erasureSource project) := by
    intro q stuck
    obtain ⟨phase, rfl⟩ := reachedPhase bindingMIR (erasureSource project) materializedSourceState false
      (fun _ h => by with_unfolding_all exact h.symm) (fun p next step => by
        obtain ⟨finish, _, state⟩ := (materializedSourceExact project p next).mp step.2.1
        exact ⟨finish, state⟩) stuck.1
    cases phase
    · exact stuck.2.1 _ (bindingMIRTick project)
    · exact stuck.2.2 _ terminal
  obtain ⟨sim, represents⟩ := lowers.2.2.2.2.2.2.2.2.2 safe
  have targetInitial : bindingMemory.initial (immediateReturn project) storeInitial := by with_unfolding_all rfl
  have targetReach : bindingMemory.finite (immediateReturn project) storeInitial :=
    ⟨_, targetInitial, .nil _⟩
  obtain ⟨p, pInitial, related⟩ := sim.targetInitial storeInitial targetInitial
  have initial : p = storeInitial := by with_unfolding_all exact pInitial
  rw [initial] at related
  have observed : bindingMemory.result (immediateReturn project) storeInitial bindingResult := by
    with_unfolding_all rfl
  obtain ⟨finish, path, result, _⟩ := sim.targetFinish storeInitial storeInitial bindingResult
    ⟨_, starts, .nil _⟩ targetReach related observed
  cases path with
  | nil =>
    have absent : none = some bindingResult := by with_unfolding_all exact result
    cases absent
  | @cons _ next _ tick _ =>
    have bound := erasureBindingUnique project next tick.1.2.1
    have physical := (represents next storeInitial ⟨_, starts, .cons tick.1 (.nil _)⟩
      targetReach tick.2).1.1
    rw [bound] at physical
    exact List.not_mem_nil (physical.2.2.1 storeRoot (List.mem_cons_self ..))


private abbrev addressMIRExecution := MIR.Execution (language := addressLanguage)
  (lang := testLowering) (profile := testProfile)

private abbrev addressMemoryExecution := Memory.Execution (language := addressLanguage)
  (lang := testLowering) (profile := testProfile)

private def addressResult : Concrete.Contracts.Observation := .returned 42 ByteArray.empty

private theorem addressMIRTick (project : Project) {p q : AddressSourcePhase} (next : p.next = some q) :
    addressMIRExecution.step (logicalAddressProgram project) p.state q.state := by
  refine ⟨?_, (logicalAddressExact project p q.state).mpr ⟨q, next, rfl⟩, ?_⟩
  · cases p <;> first | (with_unfolding_all rfl) | cases next
  · cases p <;> cases q <;> first | (exact ⟨[], by with_unfolding_all rfl⟩) | cases next

private theorem addressMemoryTick (project : Project) {p q : AddressTargetPhase} (tick : p.Tick q) :
    addressMemoryExecution.step (addressMemory project) p.state q.state := by
  refine ⟨?_, (addressTargetExact project p q.state).mpr ⟨q, tick, rfl⟩, ?_⟩
  · cases tick <;> with_unfolding_all rfl
  · cases tick <;> exact ⟨[], by with_unfolding_all rfl⟩

private theorem addressMemoryReachable (project : Project) (q : addressMemoryExecution.State)
    (finite : addressMemoryExecution.finite (addressMemory project) q) :
    ∃ phase : AddressTargetPhase, phase.state = q :=
  reachedPhase addressMemoryExecution _ AddressTargetPhase.state .start (fun _ h => by with_unfolding_all exact h.symm)
    (fun p next h => by
      obtain ⟨phase, _, state⟩ := (addressTargetExact project p next).mp h.2.1
      exact ⟨phase, state⟩) finite

set_option maxRecDepth 4096 in
set_option maxHeartbeats 3000000 in
example (project : Project) :
    LowersMemory (language := addressLanguage) (logicalAddressProgram project) (addressMemory project) := by
  have static := addressStatic project
  refine ⟨rfl, rfl, static.1, static.2.1, ⟨rfl, rfl⟩, static.2.2.1, static.2.2.2.1, static.2.2.2.2, ?_, ?_⟩
  · apply matchingRefinement (language := addressLanguage)
    refine finiteProgressRefines addressMIRExecution addressMemoryExecution _ _ addressResult
      AddressSourcePhase.done.state
      ⟨AddressSourcePhase.start.state, by with_unfolding_all rfl, .cons (addressMIRTick project (p := .start) (q := .bound) rfl)
        (.cons (addressMIRTick project (p := .bound) (q := .reference) rfl)
          (.cons (addressMIRTick project (p := .reference) (q := .done) rfl) (.nil _)))⟩
      (by with_unfolding_all rfl) ?_ ?_ (fun state => 5 - state.instruction) ?_
    · intro q finite
      obtain ⟨phase, rfl⟩ := addressMemoryReachable project q finite
      cases phase
      · let a : AddressAllocation := ⟨⟨0⟩, 1, by unfold backingFits; decide⟩
        exact .inl ⟨_, addressMemoryTick project (.allocate a)⟩
      · exact .inl ⟨_, addressMemoryTick project (.bind _)⟩
      · exact .inl ⟨_, addressMemoryTick project (.beginDestination _)⟩
      · exact .inl ⟨_, addressMemoryTick project (.takeAddress _)⟩
      · exact .inl ⟨_, addressMemoryTick project (.indirect _)⟩
      · exact .inr (by with_unfolding_all rfl)
    · intro q o finite result
      obtain ⟨phase, rfl⟩ := addressMemoryReachable project q finite
      cases phase
      all_goals first
        | (have absent : none = some o := by with_unfolding_all exact result
           cases absent)
        | (have same : some addressResult = some o := by with_unfolding_all exact result
           exact ⟨(Option.some.inj same).symm, by with_unfolding_all rfl⟩)
    · intro q q' finite step
      obtain ⟨phase, rfl⟩ := addressMemoryReachable project q finite
      obtain ⟨next, tick, rfl⟩ := (addressTargetExact project phase q').mp step.2.1
      cases tick <;> first
        | exact (by decide : 0 < 1)
        | exact (by decide : 1 < 2)
        | exact (by decide : 2 < 3)
        | exact (by decide : 3 < 4)
        | exact (by decide : 4 < 5)
  · intro _
    let related : addressMIRExecution.State → addressMemoryExecution.State → Prop :=
      fun p q => ∃ phase : AddressTargetPhase, phase.source.state = p ∧ phase.state = q
    refine ⟨⟨related, (fun _ q => 5 - q.instruction), ?_, ?_, ?_⟩, ?_⟩
    · intro q initial
      have same : q = storeInitial := initial
      subst q
      exact ⟨storeInitial, by with_unfolding_all rfl, .start, rfl, rfl⟩
    · rintro _ _ q' _ _ ⟨phase, rfl, rfl⟩ step
      obtain ⟨next, tick, rfl⟩ := (addressTargetExact project phase q').mp step.2.1
      cases tick with
      | allocate a => exact .inr ⟨⟨.allocated a, rfl, rfl⟩, (by decide : 4 < 5)⟩
      | bind a =>
        exact .inl ⟨_, _, .nil _, addressMIRTick project (p := .start) (q := .bound) rfl, .bound a, rfl, rfl⟩
      | beginDestination a => exact .inr ⟨⟨.ready a, rfl, rfl⟩, (by decide : 2 < 3)⟩
      | takeAddress a =>
        exact .inl ⟨_, _, .nil _, addressMIRTick project (p := .bound) (q := .reference) rfl, .reference a, rfl, rfl⟩
      | indirect a =>
        exact .inl ⟨_, _, .nil _, addressMIRTick project (p := .reference) (q := .done) rfl, .done a, rfl, rfl⟩
    · rintro _ _ o _ _ ⟨phase, rfl, rfl⟩ observed
      cases phase
      all_goals first
        | (have absent : none = some o := by with_unfolding_all exact observed
           cases absent)
        | (have same : some addressResult = some o := by with_unfolding_all exact observed
           obtain rfl := Option.some.inj same
           exact ⟨_, .nil _, by with_unfolding_all rfl, by with_unfolding_all rfl⟩)
    · rintro _ _ _ _ ⟨q, rfl, rfl⟩
      exact ⟨addressCheckpoint project q, by cases q <;> rfl⟩


private def addressWrongReturn (project : Project) : Memory testLowering testProfile :=
  let block := {addressBlock with
    body := []
    ending := .ret (some ⟨(), .literal ⟨[0]⟩⟩)
    boundaryOrigins := [testOrigin]}
  {addressMemory project with
    functions := [(Exec.Demo.mainF, [(storeRoot, ()), (shadowRoot, ())], ⟨⟨0⟩, #[block]⟩)]}

-- Keep the declarations, layout and origins while replacing the reached
-- read/write computation with a return of the wrong value.
set_option maxRecDepth 4096 in
set_option maxHeartbeats 2500000 in
example (project : Project) :
    ¬ LowersMemory (language := addressLanguage) (logicalAddressProgram project) (addressWrongReturn project) := by
  intro lowered
  let input := logicalAddressProgram project
  let output := addressWrongReturn project
  have reachable (s : addressMIRExecution.State) (finite : addressMIRExecution.finite input s) :
      ∃ phase : AddressSourcePhase, phase.state = s :=
    reachedPhase addressMIRExecution input AddressSourcePhase.state .start
      (fun _ h => by with_unfolding_all exact h.symm)
      (fun p next h => by
        obtain ⟨phase, _, state⟩ := (logicalAddressExact project p next).mp h.2.1
        exact ⟨phase, state⟩) finite
  have safe : ProgramSafe addressMIRExecution input := by
    intro s stuck
    obtain ⟨phase, equal⟩ := reachable s stuck.1
    rw [← equal] at stuck
    cases phase
    · exact stuck.2.1 _ (addressMIRTick project (p := .start) (q := .bound) rfl)
    · exact stuck.2.1 _ (addressMIRTick project (p := .bound) (q := .reference) rfl)
    · exact stuck.2.1 _ (addressMIRTick project (p := .reference) (q := .done) rfl)
    · exact stuck.2.2 addressResult (by with_unfolding_all rfl)
  have initial : addressMemoryExecution.initial output storeInitial := by with_unfolding_all rfl
  let wrong : Concrete.Contracts.Observation := .returned 0 ByteArray.empty
  have result : addressMemoryExecution.result output storeInitial wrong := by with_unfolding_all rfl
  obtain ⟨s, finite, observed, _⟩ :=
    (lowered.2.2.2.2.2.2.2.2.1 safe).1 storeInitial wrong ⟨_, initial, .nil _⟩ result
  obtain ⟨phase, equal⟩ := reachable s finite
  rw [← equal] at observed
  cases phase
  all_goals first
    | (have absent : none = some wrong := by with_unfolding_all exact observed
       cases absent)
    | (have mismatch : some addressResult = some wrong := by with_unfolding_all exact observed
       have incompatible := Option.some.inj mismatch
       change Concrete.Contracts.Observation.returned 42 _ =
         Concrete.Contracts.Observation.returned 0 _ at incompatible
       cases incompatible)


-- The reachable-helper trap substitution above has a positive counterpart
-- on the very same Memory → SSA edge.
set_option maxRecDepth 4096 in
set_option maxHeartbeats 1400000 in
example :
    let ssa := callingSSA .divisionByZero
    BuildsSSA (language := testLanguage) (ssa.memoryView (language := testLanguage)) ssa := by
  let ssa := callingSSA .divisionByZero
  let memory := ssa.memoryView (language := testLanguage)
  let outcome := helperTrap .divisionByZero
  refine ⟨rfl, rfl, rfl, ⟨rfl, rfl⟩, by with_unfolding_all exact ⟨fun _ h => h, fun _ h => h⟩, ?_⟩
  have targetTick : ssaExecution.step ssa storeInitial helperState := by
    with_unfolding_all exact ⟨rfl, ⟨_, rfl, rfl, [], _, _, _, rfl, rfl, rfl, rfl, .nil _⟩, _, rfl⟩
  have targetOnly (q : ssaExecution.State) (tick : ssaExecution.step ssa storeInitial q) :
      q = helperState := by
    exact callAtEntryUnique (startDefinitions := true) .divisionByZero q tick.2.1
  have terminal : ssaExecution.result ssa helperState outcome := by with_unfolding_all rfl
  have reachable (q : ssaExecution.State) (finite : ssaExecution.finite ssa q) :
      q = storeInitial ∨ q = helperState := by
    obtain ⟨start, initial, path⟩ := finite
    have equal : start = storeInitial := by with_unfolding_all exact initial
    rw [equal] at path
    cases path with
    | nil => exact .inl rfl
    | cons step rest =>
      obtain rfl := targetOnly _ step
      cases rest with
      | nil => exact .inr rfl
      | cons step _ => exact (ssaExecution.terminal _ _ _ terminal _ step).elim
  refine finiteProgressRefines memoryExecution ssaExecution memory ssa outcome helperState
    ⟨storeInitial, by with_unfolding_all rfl, .cons (callAtEntry .divisionByZero) (.nil _)⟩
    (by with_unfolding_all rfl) ?_ ?_ (fun q => if q.frame == callee then 0 else 1) ?_
  · intro q finite
    rcases reachable q finite with rfl | rfl
    · exact .inl ⟨_, targetTick⟩
    · exact .inr terminal
  · intro q o finite result
    rcases reachable q finite with rfl | rfl
    · have absent : none = some o := by with_unfolding_all exact result
      cases absent
    · have same : some outcome = some o := by with_unfolding_all exact result
      exact ⟨(Option.some.inj same).symm, by with_unfolding_all rfl⟩
  · intro q q' finite step
    rcases reachable q finite with rfl | rfl
    · obtain rfl := targetOnly q' step
      exact (by decide : 0 < 1)
    · exact (ssaExecution.terminal _ _ _ terminal _ step).elim


private def abortMemory (kind : TrapKind) : Memory testLowering testProfile :=
  (abortMIR kind).logicalView (language := testLanguage)

private def abortSSA (kind : TrapKind) : SSA testLowering testProfile :=
  let block : Block _ _ _ := ⟨⟨0⟩, [], [], .trap kind testOrigin, []⟩
  ⟨tabProject, Exec.Demo.mainF, (abortMIR kind).signatures,
    [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[block]⟩)], [], [], []⟩

private def abortBIR (kind : TrapKind) : BIR testLowering testProfile :=
  let block := {birReturnBlock false with ending := .trap ⟨0⟩}
  ⟨tabProject, Exec.Demo.mainF, (abortMIR kind).signatures,
    [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[block]⟩)], [], #[⟨kind, testOrigin⟩], [], []⟩

private def abortJob (kind : TrapKind) : LCBInput testProfile :=
  let diagnostic :=
    s!"concrete: trap: {kind.name} at {mainSourcePath.render.text}:{span.line}:{span.col} (site {testSite.id})\n"
  {lcbReturnJob 256 with
    sites := (abortBIR kind).sites
    runtimeInput := (Backend.Runtime.qbeDiagnostics [diagnostic]).toUTF8}

private theorem abortTables (kind : TrapKind) : (abortMemory kind).TablesWF := by
  refine ⟨?_, ?_, ?_, List.nodup_nil, ?_, nofun, ?_⟩
  · with_unfolding_all change [Exec.Demo.mainF].Nodup
    simp
  · change [Exec.Demo.mainF].Nodup
    simp
  · with_unfolding_all change [Exec.Demo.mainF].Nodup
    simp
  · intro fn cls included
    with_unfolding_all change (fn, cls) ∈ [(Exec.Demo.mainF, [])] at included
    cases List.mem_singleton.mp included
    exact List.nodup_nil
  · intro fn locals graph included
    let block : MemoryBlock testLowering := ⟨⟨0⟩, [], [], .trap kind testOrigin, []⟩
    have expected : (fn, locals, graph) ∈ [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[block]⟩)] := by
      with_unfolding_all exact included
    cases List.mem_singleton.mp expected
    refine ⟨List.nodup_nil, ⟨by simp, ⟨block, List.mem_singleton_self _, rfl⟩, ?_⟩,
      _, rfl, ⟨block, List.mem_singleton_self _, rfl, rfl⟩, ?_⟩
    · intro _ member _ edge
      cases List.mem_singleton.mp member
      exact (List.not_mem_nil edge).elim
    · intro _ member
      cases List.mem_singleton.mp member
      exact ⟨trivial, nofun⟩

private theorem abortMemoryCheckpoint :
    (abortMemory .abort).RepresentsCheckpoint storeInitial storeInitial := by
  refine ⟨?_, rfl, nofun, ⟨rfl, nofun⟩, List.nodup_nil, rfl, rfl⟩
  change (abortMemory .abort).RepresentsActivation
    storeInitial.toMemoryFrame storeInitial.toMemoryFrame Mem.Memory.empty
  exact ⟨⟨⟨⟨nofun, by decide, nofun, nofun, nofun, nofun, nofun, nofun⟩,
    by decide, by decide, nofun⟩, rfl⟩, rfl, nofun, by decide, by decide, by decide, by decide,
    nofun, (fun _ => ⟨nofun, nofun⟩), nofun, nofun, nofun, nofun⟩

-- The same abort used in the Core→MIR pair is terminal at each of the
-- remaining stages. Each mutant changes only the output of its own edge.
set_option maxRecDepth 4096 in
set_option maxHeartbeats 1500000 in
example :
    LowersMemory (language := testLanguage) (abortMIR .abort) (abortMemory .abort) ∧
    ¬ LowersMemory (language := testLanguage) (abortMIR .abort) (abortMemory .divisionByZero) := by
  have source : mirExecution.result (abortMIR .abort) storeInitial (helperTrap .abort) :=
    by with_unfolding_all rfl
  have target : memoryExecution.result (abortMemory .abort) storeInitial (helperTrap .abort) :=
    by with_unfolding_all rfl
  have atSource : ∀ p, mirExecution.initial (abortMIR .abort) p → p = storeInitial :=
    fun _ h => by with_unfolding_all exact h
  have atTarget : ∀ q, memoryExecution.initial (abortMemory .abort) q → q = storeInitial :=
    fun _ h => by with_unfolding_all exact h
  constructor
  · refine ⟨rfl, rfl, ?_, ?_, ⟨rfl, rfl⟩, ?_, abortTables _, abortTables _, ?_, ?_⟩
    · with_unfolding_all exact ⟨fun _ x => x, fun _ x => x⟩
    · refine ⟨?_, nofun⟩
      intro fn ls graph included place ty member
      let block : MemoryBlock testLowering := ⟨⟨0⟩, [], [], .trap .abort testOrigin, []⟩
      have original : (fn, ls, graph) ∈ [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[block]⟩)] := by
        with_unfolding_all exact included
      have empty : ls = [] := congrArg (fun fn => fn.2.1) (List.mem_singleton.mp original)
      exact (List.not_mem_nil (empty ▸ member)).elim
    · constructor
      · intro fn ls graph included place ty member
        let entry : MIRGraph testLowering := ⟨⟨0⟩, #[⟨⟨0⟩, [], [], .trap .abort testOrigin, []⟩]⟩
        have original : (fn, ls, graph) ∈ [(Exec.Demo.mainF, [], entry)] := included
        have empty : ls = [] := congrArg (fun fn => fn.2.1) (List.mem_singleton.mp original)
        exact (List.not_mem_nil (empty ▸ member)).elim
      · intro fn classes place included member
        have original : (fn, classes) ∈ [(Exec.Demo.mainF, [])] := by
          with_unfolding_all exact included
        cases List.mem_singleton.mp original
        exact (List.not_mem_nil member).elim
    · refine stationaryRefines _ _ _ _ storeInitial storeInitial
        (helperTrap .abort) (helperTrap .abort)
        (fun _ => by with_unfolding_all rfl) (fun _ => by with_unfolding_all rfl)
        source target source ?_ ?_
      · intro o
        with_unfolding_all exact ⟨fun h => (Option.some.inj h).symm, fun h => congrArg some h.symm⟩
      · with_unfolding_all rfl
    · intro _
      let related : mirExecution.State → memoryExecution.State → Prop :=
        fun p q => p = storeInitial ∧ q = storeInitial
      refine ⟨⟨related, fun _ _ => 0, ?_, ?_, ?_⟩, ?_⟩
      · exact fun q starts => ⟨storeInitial, by with_unfolding_all rfl, rfl, atTarget q starts⟩
      · rintro _ _ q' _ _ ⟨rfl, rfl⟩ tick
        exact (memoryExecution.terminal _ _ _ target q' tick).elim
      · rintro _ _ o _ _ ⟨rfl, rfl⟩ observes
        have equal := Option.some.inj (target.symm.trans observes)
        exact ⟨_, .nil _, equal ▸ source, by with_unfolding_all rfl⟩
      · rintro _ _ _ _ ⟨rfl, rfl⟩
        exact ⟨abortMemoryCheckpoint, rfl⟩
  · intro lowered
    apply stationaryRejects (b := abortMemory .divisionByZero) mirExecution memoryExecution
      (Memory.MatchingResult (language := testLanguage)) storeInitial storeInitial
      (helperTrap .abort) (helperTrap .divisionByZero) atSource source
      (by with_unfolding_all rfl) (by with_unfolding_all rfl) ?_
      lowered.2.2.2.2.2.2.2.2.1
    intro observes
    have wrong : some (helperTrap .abort) = some (helperTrap .divisionByZero) := by
      with_unfolding_all exact observes
    have equal := Option.some.inj wrong
    change Concrete.Contracts.Observation.trapped _ .abort _ _ =
      Concrete.Contracts.Observation.trapped _ .divisionByZero _ _ at equal
    cases equal

set_option maxRecDepth 4096 in
example :
    BuildsSSA (language := testLanguage) (abortMemory .abort) (abortSSA .abort) ∧
    ¬ BuildsSSA (language := testLanguage) (abortMemory .abort) (abortSSA .divisionByZero) := by
  constructor
  · refine ⟨rfl, rfl, rfl, ⟨rfl, rfl⟩, by with_unfolding_all exact ⟨fun _ h => h, fun _ h => h⟩, ?_⟩
    refine stationaryRefines _ _ _ _ storeInitial storeInitial
      (helperTrap .abort) (helperTrap .abort)
      (fun _ => by with_unfolding_all rfl) (fun _ => by with_unfolding_all rfl) ?_ ?_ ?_ ?_ ?_
    all_goals first
      | (intro o; with_unfolding_all exact ⟨fun h => (Option.some.inj h).symm, fun h => congrArg some h.symm⟩)
      | with_unfolding_all rfl
  · intro formed
    apply stationaryRejects memoryExecution ssaExecution ssaExecution.result storeInitial storeInitial
      (helperTrap .abort) (helperTrap .divisionByZero) (fun _ h => by with_unfolding_all exact h)
      (by with_unfolding_all rfl) (by with_unfolding_all rfl) (by with_unfolding_all rfl) ?_
      formed.2.2.2.2.2
    intro observes
    have wrong : some (helperTrap .abort) = some (helperTrap .divisionByZero) := by
      with_unfolding_all exact observes
    have equal := Option.some.inj wrong
    change Concrete.Contracts.Observation.trapped _ .abort _ _ =
      Concrete.Contracts.Observation.trapped _ .divisionByZero _ _ at equal
    cases equal

set_option maxRecDepth 4096 in
example :
    BuildsBIR (language := testLanguage) (abortSSA .abort) (abortBIR .abort) ∧
    ¬ BuildsBIR (language := testLanguage) (abortSSA .abort) (abortBIR .divisionByZero) := by
  constructor
  · refine ⟨rfl, ?_, ?_, ?_, by decide, ⟨rfl, rfl, by with_unfolding_all rfl⟩,
      BIR.refinesOfViewEq (by with_unfolding_all rfl)⟩
    · intro fn member block included slot trapped
      cases List.mem_singleton.mp member
      cases List.mem_singleton.mp included
      cases BIRTerm.trap.inj trapped
      exact ⟨⟨.abort, testOrigin⟩, rfl⟩
    · intro fn member block included kind origin trapped
      cases List.mem_singleton.mp member
      cases List.mem_singleton.mp included
      cases Terminator.trap.inj trapped with
      | intro kindEq originEq =>
        subst kind
        subst origin
        exact ⟨_, List.mem_singleton_self _, rfl, _, List.mem_singleton_self _, rfl,
          ⟨0⟩, rfl, ⟨.abort, testOrigin⟩, rfl, rfl, rfl⟩
    · with_unfolding_all exact ⟨fun _ h => h, fun _ h => h⟩
  · intro formed
    have same := congrArg (fun block => do
      let graph ← block
      let entry ← graph.blocks[0]?
      match entry.ending with
      | Terminator.trap kind _ => some kind
      | _ => none) formed.2.2.2.2.2.1.2.2
    have wrong : some TrapKind.abort = some TrapKind.divisionByZero := by with_unfolding_all exact same
    cases wrong

set_option maxRecDepth 4096 in
example :
    EmitsLCB testLanguage (abortBIR .abort) (abortJob .abort) ∧
    ¬ EmitsLCB testLanguage (abortBIR .abort) {abortJob .abort with body := (lcbReturnJob 0).body} := by
  constructor
  · refine ⟨rfl, by with_unfolding_all rfl, nofun, by simp [abortJob, lcbReturnJob], rfl,
      ⟨_, by with_unfolding_all rfl, by with_unfolding_all rfl⟩, ?_,
      by simp [abortBIR], by simp [abortJob, lcbReturnJob], fun _ => Iff.rfl,
      by with_unfolding_all rfl, ?_⟩
    · refine stationaryRefines _ _ _ _ _ _ (helperTrap .abort) (helperTrap .abort)
        (fun _ => Iff.rfl) (fun _ => Iff.rfl) ?_ ?_ ?_ ?_ ?_
      all_goals first
        | (intro o; with_unfolding_all exact ⟨fun h => (Option.some.inj h).symm, fun h => congrArg some h.symm⟩)
        | with_unfolding_all rfl
    · intro fn member block included slot trapped
      cases List.mem_singleton.mp member
      cases List.mem_singleton.mp included
      cases BIRTerm.trap.inj trapped
      exact ⟨⟨.abort, testOrigin⟩, rfl⟩
  · intro emitted
    apply stationaryRejects birExecution (LCBInput.Execution Nat tabProject) (LCBInput.Execution Nat tabProject).result
      (BIR.entryState (language := testLanguage) (abortBIR .abort)) (some ⟨0, 0, #[]⟩)
      (helperTrap .abort) (.returned 0 ByteArray.empty) (fun _ h => h)
      (by with_unfolding_all rfl) (by with_unfolding_all rfl) (by with_unfolding_all rfl) ?_
      emitted.2.2.2.2.2.2.1
    intro observed
    have wrong : some (helperTrap .abort) = some (.returned 0 ByteArray.empty) := by
      with_unfolding_all exact observed
    have equal := Option.some.inj wrong
    change Concrete.Contracts.Observation.trapped _ .abort _ _ =
      Concrete.Contracts.Observation.returned 0 _ at equal
    cases equal

end StageInterfaceControls
