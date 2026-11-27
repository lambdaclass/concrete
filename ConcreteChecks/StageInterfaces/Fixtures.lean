import Concrete.Interfaces.Pipeline
import ConcreteSpecEvidence.ExecExamples
import ConcreteChecks.StageInterfaces.Models

open Concrete
open Concrete.Interfaces
open ConcreteSpec Core Core.SourcePathSpan StageInterface StageGrammar
open Concrete.LL1

namespace StageInterfaceControls

def early : Slice := ⟨0, by decide⟩
def later : Slice := ⟨2, by decide⟩

def production : Production (fun (_ : Fin 1) => SyntaxTree ⊕ Bool) :=
  ⟨0, [.t .integer], early, .constant (.inr true)⟩

def grammar : SharedGrammar 1 :=
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

def table : CheckedTable grammar where
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

def proposal : Array UInt64 :=
  (Array.replicate grammar.terminalColumns 0).set! (terminalColumn (some .integer)) 1

def testSite : SiteId := ⟨4⟩
def span : FileSpan := ⟨⟨0⟩, 0, 1, 1, 1⟩
def rule : RuleId := ⟨`control, 1⟩

abbrev testLanguage : CoreLanguage where
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
  layout := fun _ _ _ => some ⟨1, 1⟩
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
  fieldRange := fun _ _ _ => none
  encodeOther := fun _ _ _ => none
  decodeOther := fun _ _ _ => none
  safety
    | .expression _ | .call _ (_ :: _) =>
      [⟨0, .divisionByZero, [((), .literal [0])]⟩]
    | .abort => [⟨0, .abort, []⟩]
    | _ => []
  permit := fun _ _ _ own => some own

def testLowering : LoweringLanguage Unit :=
  ⟨fun _ _ _ => Unit, ()⟩

def testProfile : Profile := ⟨.lcb, .x86_64Linux, 3, 4096⟩

def testOrigin : Origin := ⟨testSite, span, ⟨.function Exec.Demo.mainF, ⟨[]⟩⟩⟩

def testCore (project : Project) (body : Exec.Stmt (testLanguage.instrumented []) := .ret () testSite) : CoreInput testLanguage :=
  ⟨project, ⟨0, by decide⟩, .integrated ⟨[], [],
    ⟨[(Exec.Demo.mainF, ⟨[], (), ⟨0⟩, body⟩)]⟩,
    Exec.Demo.mainF, [(testSite, testOrigin)]⟩⟩

def testMIR (project : Project) (checks : List OperationCheck)
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

def testBIR (project : Project) {profile : Profile} : BIR testLowering profile :=
  ⟨project, Exec.Demo.mainF, [], [], [], #[], [], []⟩

private def testOutput {profile : Profile} : LCBInput profile := ⟨badBody, [], #[], ByteArray.empty⟩

def unsafeDischarge : Discharge testLanguage :=
  ⟨testSite, testOrigin.evidenceKey, ⟨testSite, .safety, 0⟩, .divisionByZero,
    [((), .literal [0])]⟩

def unsafeDischarges : Discharges testLanguage := [(testSite, [unsafeDischarge])]

def testPasses {profile : Profile} (checks : List OperationCheck) :
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

structure FixtureEntries (profile : Profile) where
  lexer : Stage Source LexRefusal TokenStream
  parser : Stage TokenStream ParseRefusal (Parsed (grammar.value grammar.start))
  admission : Stage (AdmissionInput testLanguage) AdmissionFailure (Admitted testLanguage)
  lowerUnchecked : Stage (CoreInput testLanguage) StageFailure (MIR testLowering profile)
  emitLLVM : Stage (BIR testLowering profile) StageFailure (LLVMInput profile)
  emitLCB : Stage (BIR testLowering profile) StageFailure (LCBInput profile)

def fixtureEntries {profile : Profile} (parser : Parser grammar)
    (passes : PassFunctions grammar testLanguage testLowering profile) : FixtureEntries profile :=
  ⟨passes.lexer, parserEntry parser.engine, (passes.admissionEntry early).toStage, passes.lowerUncheckedEntry,
    passes.emitLLVMEntry, passes.emitLCBEntry⟩

abbrev r {profile : Profile} (_m : LoweringModels testLanguage testLowering profile)
    (parser : Parser grammar) (checks : List OperationCheck := []) :=
  fixtureEntries parser (testPasses (profile := profile) checks)

def tokenStream (radix : Surface0.Radix)
    (slice : Slice := early) : TokenStream :=
  ⟨⟨tabProject, ⟨0⟩, slice⟩, #[⟨⟨span, .integer 42 radix none⟩, "a".toUTF8⟩], #[#[], #[]]⟩

def testParser (verifier : TableVerifier grammar) (size : proposal.size = verifier.words)
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

def callee : FnId := {Exec.Demo.mainF with name := ⟨"callee"⟩}
def callSite : SiteId := ⟨5⟩
def callOrigin : Origin := ⟨callSite, span, ⟨.function callee, ⟨[]⟩⟩⟩

end StageInterfaceControls
