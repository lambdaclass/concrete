import ConcreteSpec.Admission.Early
import ConcreteSpec.Proofs.Memory

namespace ConcreteSpec.Admission.Controls

private def return42 : StageInterface.EarlyProgram :=
  let span := (⟨0, 2, 1, 1, ⟨0⟩⟩ : Surface0.Span)
  ⟨[], .lit 42, span, span⟩

private def return256 : StageInterface.EarlyProgram :=
  { return42 with ret := .lit 256 }

theorem return42_admitted : (admitView (.zero return42)).isOk = true := rfl

theorem return256_refused :
    admitView (.zero return256) = .error (.atPosition 0 (.literalOutOfRange .u8 256)) := rfl

private def addition : Program :=
  ⟨[], [.let_ (.name ⟨0⟩) (.scalar (.int .u8)) (.add (.int .u8 19) (.int .u8 23))],
    .ret (.copy ⟨0⟩)⟩

private def issued : Discharge := ⟨0, .u8, .int .u8 19, .int .u8 23, []⟩

private def batch : Batch addition := ⟨[issued], rfl⟩

theorem issued_admitted : admitProgram addition = .ok batch := rfl

theorem issued_valid : batch.validates [issued] = true := by decide

theorem residual_valid : batch.validates [] = true := rfl

theorem wrong_site_refused : batch.validates [{ issued with position := 1 }] = false := by decide

theorem wrong_operand_refused :
    batch.validates [{ issued with left := issued.right, right := issued.left }] = false := by decide

theorem wrong_context_refused :
    batch.validates [{ issued with context := [(⟨0⟩, ⟨.scalar (.int .u8), true, true, none, []⟩)] }]
      = false := by decide

theorem duplicate_refused : batch.validates [issued, issued] = false := by decide

-- The same batch binds exact source reads to the integrated operation and key.
example (project : Core.SourcePathSpan.Project) :
    let span := return42.returnSpan
    let view := StageInterface.AdmissionView.zero
      ⟨[⟨⟨⟨0⟩, .add (.lit 19) (.lit 23)⟩, span, span, (span, span), .unnamed none⟩], .var ⟨0⟩, span, span⟩
    let input : StageInterface.CoreInput (StageInterface.sliceLanguage view.slice) :=
      ⟨project, view.slice, .straightLine view rfl⟩
    let exactBatch : InputBatch input := ⟨view, ⟨rfl, rfl, rfl⟩, batch⟩
    let origin := view.originAt 0
    let encoded := exactBatch.encode [issued]
    (encoded.bind List.head?).map Prod.fst = some ⟨0⟩ ∧
    (((encoded.bind List.head?).bind fun row => row.2.head?).map (·.key)) = origin.map (·.evidenceKey) ∧
    exactBatch.encode [] = some [] ∧ exactBatch.encode [issued, issued] = none := by
  with_unfolding_all exact ⟨rfl, rfl, rfl, rfl⟩

private def x : Var := ⟨0⟩
private def y : Var := ⟨1⟩
private def u8 : Ty := .scalar (.int .u8)
private def token : Name := ⟨"Token"⟩
private def item : Name := ⟨"Item"⟩
private def flag : Name := ⟨"flag"⟩
private def tokenDecl : StructDecl := ⟨token, false, []⟩
private def count : Name := ⟨"count"⟩
private def itemDecl : StructDecl := ⟨item, true, [(count, .int .u8), (flag, .bool)]⟩

private def itemWithFlag (atom : Atom) : Stmt :=
  .let_ .wild (.struct item) (.mk item [(count, .int .u8 1), (flag, atom)])
private def ret42 : Terminator := .ret (.int .u8 42)

theorem addition_unbound :
    checkProgram ⟨[], [.let_ .wild u8 (.add (.copy x) (.int .u8 1))], ret42⟩ =
      .error (.atPosition 0 (.additionOperandType (.copy x) (.atomFailure (.unbound x)))) := rfl

theorem addition_non_integer :
    checkProgram ⟨[], [.let_ .wild u8 (.add (.int .u8 1) (.bool true))], ret42⟩ =
      .error (.atPosition 0 (.additionOperandType (.bool true) (.wrongType (.scalar .bool)))) := rfl

theorem addition_mismatched_widths :
    checkProgram ⟨[], [.let_ .wild u8 (.add (.int .u8 1) (.int .u16 1))], ret42⟩ =
      .error (.atPosition 0 (.additionTypeMismatch (.int .u8 1) (.int .u16 1) .u8 .u16)) := rfl

theorem initializer_unbound :
    checkProgram ⟨[itemDecl], [itemWithFlag (.copy x)], ret42⟩ =
      .error (.atPosition 0 (.structFieldType item flag (.copy x) .bool (.atomFailure (.unbound x)))) := rfl

theorem initializer_wrong_type :
    checkProgram ⟨[itemDecl], [itemWithFlag (.int .u8 1)], ret42⟩ =
      .error (.atPosition 0 (.structFieldType item flag (.int .u8 1) .bool (.wrongType u8))) := rfl

private def itemValue : Expr := .mk item [(count, .int .u8 41), (flag, .bool true)]

private def destructure (pat : List (Name × Binder)) (preceding : List Stmt := []) : Program :=
  ⟨[itemDecl], preceding ++ [.letStruct item pat itemValue], ret42⟩

theorem pattern_duplicate_field :
    checkProgram (destructure [(count, .wild), (flag, .wild), (flag, .wild)]) =
      .error (.atPosition 0 (.duplicatePatternField item flag)) := rfl

theorem pattern_unknown_field :
    checkProgram (destructure [(count, .wild), (flag, .wild), (⟨"extra"⟩, .wild)]) =
      .error (.atPosition 0 (.unknownPatternField item ⟨"extra"⟩)) := rfl

theorem pattern_missing_field :
    checkProgram (destructure [(flag, .wild)]) =
      .error (.atPosition 0 (.missingPatternField item count)) := rfl

theorem pattern_repeated_binder :
    checkProgram (destructure [(count, .name y), (flag, .name y)]) =
      .error (.atPosition 0 (.repeatedPatternBinder y)) := rfl

private def namedPattern : List (Name × Binder) := [(count, .name x), (flag, .name y)]

private def bindScalar (v : Var) : Stmt := .let_ (.name v) u8 (.atom (.int .u8 1))

theorem pattern_first_binder_already_bound :
    checkProgram (destructure namedPattern [bindScalar x]) =
      .error (.atPosition 1 (.patternBinderAlreadyBound x)) := rfl

theorem pattern_second_binder_already_bound :
    checkProgram (destructure namedPattern [bindScalar y]) =
      .error (.atPosition 1 (.patternBinderAlreadyBound y)) := rfl

theorem initializer_duplicate_field :
    checkProgram ⟨[itemDecl], [.let_ .wild (.struct item)
      (.mk item [(count, .int .u8 1), (flag, .bool true), (flag, .bool false)])], ret42⟩ =
      .error (.atPosition 0 (.duplicateStructInitializer item flag)) := rfl

theorem declaration_duplicate_struct :
    checkProgram ⟨[tokenDecl, itemDecl, itemDecl], [], ret42⟩ =
      .error (.duplicateStructName 2 item) := rfl

theorem declaration_duplicate_field :
    checkProgram ⟨[{ itemDecl with fields := itemDecl.fields ++ [(flag, .bool)] }], [], ret42⟩ =
      .error (.duplicateDeclaredField 0 item flag) := rfl

theorem addition_move_of_copy :
    checkProgram ⟨[], [.let_ (.name x) u8 (.atom (.int .u8 41)),
      .let_ .wild u8 (.add (.move x) (.int .u8 1))], ret42⟩ =
      .error (.atPosition 1 (.moveOfCopy x)) := rfl

-- Exercise read precedence for a context whose operand is already marked moved.
theorem addition_moved_binding :
    checkStmt [] [(x, ⟨u8, false, true, none, []⟩)]
        (.let_ .wild u8 (.add (.move x) (.int .u8 1))) = .error (.movedBinding x) := rfl

private def bindToken : Stmt := .let_ (.name x) (.struct token) (.mk token [])

theorem return_live_linear :
    checkProgram ⟨[tokenDecl], [bindToken], ret42⟩ =
      .error (.atPosition 1 (.liveLinearBinding x)) := rfl

theorem return_unused_copy :
    checkProgram ⟨[], [.let_ (.name x) u8 (.atom (.int .u8 1))], ret42⟩ =
      .error (.atPosition 1 (.unusedBinding x)) := rfl

theorem abort_unused_linear :
    checkProgram ⟨[tokenDecl], [bindToken], .abort ⟨0⟩⟩ =
      .error (.atPosition 1 (.unusedBinding x)) := rfl

private def overflow : Program :=
  ⟨[], [.let_ .wild u8 (.add (.int .u8 200) (.int .u8 56))], ret42⟩

theorem overflow_statically_typed : StaticallyTyped overflow :=
  ⟨by decide, .letWild (.add (.int (by decide)) (.int (by decide))) (.ret (.int (by decide)))⟩

theorem overflow_refused :
    admitProgram overflow = .error (.atPosition 0 .additionOverflowUnproven) := rfl

theorem overflow_traps : run overflow 2 = .trap .arithmeticOverflow := rfl

private def bind41 : Stmt := .let_ (.name x) u8 (.atom (.int .u8 41))
private def addOne : Stmt := .let_ (.name y) u8 (.add (.copy x) (.int .u8 1))
private def afterBinding : Program := ⟨[], [bind41, addOne], .ret (.copy y)⟩
private def beforeAdd : Env := [(x, .val (.scalar (.int .u8 41)))]
private def afterBindingDischarge : Discharge :=
  ⟨1, .u8, .copy x, .int .u8 1, [(x, ⟨u8, true, false, some (.int .u8 41), []⟩)]⟩
private def afterBindingBatch : Batch afterBinding := ⟨[afterBindingDischarge], rfl⟩

theorem discharge_after_binding :
    Corr afterBinding.decls afterBindingDischarge.context beforeAdd ∧
    ∃ next, execStmt afterBinding.decls beforeAdd addOne = .ok next := by
  have reached : Reaches afterBinding.decls [] afterBinding.body 1 addOne beforeAdd :=
    .next (s := bind41) (env' := beforeAdd) rfl .here
  have h := afterBindingBatch.dischargeSafety (d := afterBindingDischarge) (by simp [afterBindingBatch]) reached
  exact ⟨h.2.1, h.2.2.2.2⟩

/-! ### Freeze F1a stage-boundary controls -/

section StageBoundaryControls

open ConcreteSpec.Core ConcreteSpec.Core.SourcePathSpan ConcreteSpec.StageInterface

private def mkSpan (start stop : Nat) : Surface0.Span := ⟨start, stop, 1, start + 1, ⟨0⟩⟩
private def braceSpan : Surface0.Span := mkSpan 31 32
private def nameSpan : Surface0.Span := mkSpan 3 7

-- Codex 5: empty body and non-empty fallthrough both cite `Slice0Rules.Returns@1` at `closeBrace`.
theorem fallthrough_cites_closeBrace :
    let emptyProg : Surface0.Program := ⟨"main", nameSpan, [], braceSpan⟩
    let nonEmptyProg : Surface0.Program :=
      ⟨"main", nameSpan, [.bind ⟨"x", mkSpan 22 23, .integer 1 none (mkSpan 30 31)⟩ (mkSpan 18 31)], braceSpan⟩
    SurfacePreparation.prepareSurface0 emptyProg = .error (.surface (.fallthrough braceSpan)) ∧
    SurfacePreparation.prepareSurface0 nonEmptyProg = .error (.surface (.fallthrough braceSpan)) ∧
    (RefusalCause.surface (.fallthrough braceSpan)).rule = ⟨``Slice0Rules.Returns, 1⟩ ∧
    (RefusalCause.surface (.fallthrough braceSpan)).primary = surfaceFileSpan braceSpan :=
  ⟨rfl, rfl, rfl, rfl⟩

-- Stage-order `prepareSurface0`: surface preparation checks surface rules left-to-right only,
-- never preempting a later surface violation with a Core check (`LiteralTyped` / `AdditionFits` / `LocalsUsed`).
theorem prepareSurface0_stage_order :
    let bind256 : Surface0.Statement :=
      .bind ⟨"x", mkSpan 22 23, .integer 256 none (mkSpan 30 33)⟩ (mkSpan 18 34)
    let bindOption : Surface0.Statement :=
      .bind ⟨"Option", mkSpan 39 45, .integer 1 none (mkSpan 52 53)⟩ (mkSpan 35 54)
    let retX : Surface0.Statement := .ret (.local "x" (mkSpan 62 63)) (mkSpan 55 64)
    let ret42 : Surface0.Statement := .ret (.integer 42 none (mkSpan 25 27)) (mkSpan 18 28)
    let ret256 : Surface0.Statement := .ret (.integer 256 none (mkSpan 25 28)) (mkSpan 18 29)
    let ret256PlusMissing : Surface0.Statement :=
      .ret (.add (.integer 256 none (mkSpan 26 29)) (.local "missing" (mkSpan 32 39)) (mkSpan 30 31)) (mkSpan 18 41)
    let retMissing : Surface0.Statement := .ret (.local "missing" (mkSpan 25 32)) (mkSpan 18 33)
    -- `let x: u8 = 256; let Option: u8 = 1; return x;` → UnreservedLocal@2 at Option, not Core LiteralTyped
    SurfacePreparation.prepareSurface0 ⟨"main", nameSpan, [bind256, bindOption, retX], braceSpan⟩ =
      .error (.surface (.reservedBinder "Option" (mkSpan 39 45))) ∧
    -- `return (256 + missing);` → LocalBound@1 at missing, not Core LiteralTyped
    SurfacePreparation.prepareSurface0 ⟨"main", nameSpan, [ret256PlusMissing], braceSpan⟩ =
      .error (.surface (.unknownLocal "missing" (mkSpan 32 39))) ∧
    -- `return 42; let Option: u8 = 1;` → ReachableBody@1 at full statement span [35, 54), not UnreservedLocal
    SurfacePreparation.prepareSurface0 ⟨"main", nameSpan, [ret42, bindOption], braceSpan⟩ =
      .error (.surface (.unreachable (mkSpan 35 54))) ∧
    -- `return 256; let Option: u8 = 1;` → ReachableBody@1 at full statement span, not Core LiteralTyped
    SurfacePreparation.prepareSurface0 ⟨"main", nameSpan, [ret256, bindOption], braceSpan⟩ =
      .error (.surface (.unreachable (mkSpan 35 54))) ∧
    -- `return missing; return 42;` → LocalBound@1 at missing (left-to-right before trailing statement)
    SurfacePreparation.prepareSurface0 ⟨"main", nameSpan, [retMissing, ret42], braceSpan⟩ =
      .error (.surface (.unknownLocal "missing" (mkSpan 25 32))) ∧
    -- Both routes share identical surface preparation.
    SurfacePreparation.prepareSurface0 ⟨"main", nameSpan, [bind256, bindOption, retX], braceSpan⟩ .eligibility =
      SurfacePreparation.prepareSurface0 ⟨"main", nameSpan, [bind256, bindOption, retX], braceSpan⟩ .admission :=
  ⟨rfl, rfl, rfl, rfl, rfl, rfl⟩

-- Slice 1 located destructure refusal: `fn main() -> u8 { let Option {} = 1; return 0; }`
-- cites registered `Surface1.StructDeclared@1` at `Option` [22, 28) before lowering RHS `1`.
theorem slice1_destructure_unknown_struct_located :
    let optionSpan : Surface0.Span := ⟨22, 28, 1, 23, ⟨0⟩⟩
    let prog : Surface1.SourceProgram :=
      ⟨[], "main", [.bind "Option" .destructure (.integer 1 none) optionSpan, .ret (.integer 0 none)]⟩
    let declProg : Surface1.SourceProgram :=
      ⟨["Token"], "main", [.bind "Token" .destructure (.construct "Token") optionSpan, .ret (.integer 0 none)]⟩
    Surface1.elaborateChecked prog = .error (.unknownDestructuredStruct "Option" optionSpan) ∧
    (Surface1.ElaborationRefusal.unknownDestructuredStruct "Option" optionSpan).ruleId =
      some ⟨``Surface1.StructDeclared, 1⟩ ∧
    (validateRule ⟨``Surface1.StructDeclared, 1⟩).isSome = true ∧
    (Surface1.elaborateChecked declProg).isOk = true := by
  exact ⟨rfl, rfl, by decide, rfl⟩

-- `Slice0Rules.AdmittedProduction` aligned with `GrammarData.ProductionId` and `AdmittedAt 0`.
theorem admittedProduction_catalog_aligned :
    Slice0Rules.AdmittedProduction .plus ∧
    Slice0Rules.AdmittedProduction .endDecls ∧
    ¬ Slice0Rules.AdmittedProduction .minus ∧
    ¬ Slice0Rules.AdmittedProduction .destructure ∧
    ¬ Slice0Rules.AdmittedProduction .ifStmt ∧
    (validateRule ⟨``Slice0Rules.AdmittedProduction, 3⟩).isSome = true ∧
    (validateRule ⟨``Slice0Rules.AdmittedProduction, 2⟩).isNone = true := by
  unfold Slice0Rules.AdmittedProduction GrammarData.AdmittedAt
  decide

-- `eofFileSpan` (trailing trivia included) and `ParserView.invalidLocation` (all 5 `LocationRefusal` variants).
theorem parserView_locations_and_eof :
    let proj := Project.singleton mainSourcePath (SourceText.ofBytes "a\nb  ".toUTF8.data.toList)
    let src : Source := ⟨proj, ⟨0⟩, slice0⟩
    let tok (span : FileSpan) : Located TokenValue := ⟨span, .fixed .semi rfl⟩
    let view (span : FileSpan) : ParserView := ⟨src, #[tok span]⟩
    eofFileSpan src = ⟨⟨0⟩, 5, 5, 2, 4⟩ ∧
    (view ⟨⟨0⟩, 0, 1, 1, 1⟩).invalidLocation = none ∧
    (view ⟨⟨1⟩, 0, 1, 1, 1⟩).invalidLocation = some (⟨⟨1⟩, 0, 1, 1, 1⟩, .unknownFile) ∧
    (view ⟨⟨0⟩, 2, 1, 2, 1⟩).invalidLocation = some (⟨⟨0⟩, 2, 1, 2, 1⟩, .reversedRange) ∧
    (view ⟨⟨0⟩, 0, 6, 1, 1⟩).invalidLocation = some (⟨⟨0⟩, 0, 6, 1, 1⟩, .pastEnd 5) ∧
    (view ⟨⟨0⟩, 0, 1, 2, 1⟩).invalidLocation = some (⟨⟨0⟩, 0, 1, 2, 1⟩, .wrongLine 1) ∧
    (view ⟨⟨0⟩, 0, 1, 1, 2⟩).invalidLocation = some (⟨⟨0⟩, 0, 1, 1, 2⟩, .wrongColumn 1) ∧
    (locationRefusalRule .unknownFile).val = ⟨``SourcePathSpan.SlotResolves, 1⟩ ∧
    (locationRefusalRule .reversedRange).val = ⟨``SourcePathSpan.OrderedOffsets, 2⟩ ∧
    (locationRefusalRule (.pastEnd 5)).val = ⟨``SourcePathSpan.EndInSource, 2⟩ ∧
    (locationRefusalRule (.wrongLine 1)).val = ⟨``SourcePathSpan.LineMatches, 2⟩ ∧
    (locationRefusalRule (.wrongColumn 1)).val = ⟨``SourcePathSpan.ColumnMatches, 2⟩ := by
  with_unfolding_all exact ⟨rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩

-- ISS-0125 `Layout.WellFormed`: strict ceiling `align < addressCeiling`, overlapping sibling rejection,
-- duplicate sibling name rejection, adjacent siblings acceptance, zero-sized sibling acceptance,
-- and `sliceLanguage_layout_wf` on integers and logical empty nominals.
private def ctrlDecl : DeclId := ⟨earlyEntry.module, ⟨"S"⟩⟩
private def fldA : FieldId := ⟨.struct ctrlDecl, ⟨"a"⟩⟩
private def fldB : FieldId := ⟨.struct ctrlDecl, ⟨"b"⟩⟩

theorem layout_wellFormed_strict_and_disjoint :
    let ceiling := linuxX8664.addressCeiling
    -- Strict alignment ceiling rejects align = addressCeiling even at size 0.
    (Layout.scalar 0 ceiling).WellFormed linuxX8664 = false ∧
    -- Overlapping sibling fields [0, 4) and [2, 6) are rejected.
    (Layout.mk 8 4 (.cons fldA 0 (.scalar 4 4) (.cons fldB 2 (.scalar 4 2) .nil))).WellFormed linuxX8664 = false ∧
    -- Duplicate sibling field names are rejected even at disjoint offsets.
    (Layout.mk 8 4 (.cons fldA 0 (.scalar 4 4) (.cons fldA 4 (.scalar 4 4) .nil))).WellFormed linuxX8664 = false ∧
    -- Adjacent non-overlapping sibling fields [0, 4) and [4, 8) are accepted.
    (Layout.mk 8 4 (.cons fldA 0 (.scalar 4 4) (.cons fldB 4 (.scalar 4 4) .nil))).WellFormed linuxX8664 = true ∧
    -- Zero-sized sibling field sharing offset with a nonempty field is accepted (Q3.8).
    (Layout.mk 4 4 (.cons fldA 0 (.scalar 4 4) (.cons fldB 0 (.scalar 0 1) .nil))).WellFormed linuxX8664 = true ∧
    -- Logical empty nominal in `sliceLanguage` has layout (0, 1) and satisfies `Layout.WellFormed`.
    (sliceLanguage sliceOne).layout [(ctrlDecl, ⟨ctrlDecl.name, false, []⟩)] linuxX8664 (.nominal ctrlDecl) =
      some (.scalar 0 1) := by
  refine ⟨by decide, by decide, by decide, by decide, by decide, rfl⟩

theorem sliceLanguage_layout_wf_examples :
    (Layout.scalar 0 1).WellFormed linuxX8664 = true ∧
    (Layout.scalar 4 4).WellFormed linuxX8664 = true :=
  ⟨sliceLanguage_layout_wf sliceOne [(ctrlDecl, ⟨ctrlDecl.name, false, []⟩)] linuxX8664
      (.nominal ctrlDecl) (.scalar 0 1) rfl,
   sliceLanguage_layout_wf slice0 [] linuxX8664 (.scalar (.int .u32)) (.scalar 4 4) rfl⟩

/-- A hypothetical operation relation that frees a live incoming block on `linuxX8664`. -/
private def freeingOperation {O : Exec.Operands}
    (_env : Declarations) (op : Operator) (_inputs : List O.Ty) (_output : O.Ty)
    (_values : List O.Val) (_target : Target) (_addresses : List (OperationAddress O.Ty))
    (before : OperationStore O.Val) (result : O.Val) (_effects : List O.Event)
    (after : OperationStore O.Val) : Prop :=
  op = .named ⟨"freeBlock"⟩ ∧ result = O.unitVal ∧
    ∃ id freed, before.bytes.free id = some freed ∧ after = { before with bytes := freed }

-- ISS-0125 negative control: no `CoreLanguage` instance can admit a freeing operation;
-- `CoreLanguage.preservesResidentHandles` is directly refuted on an allocated live block.
theorem freeing_operation_violates_preservesResidentHandles :
    ¬ ∃ lang : CoreLanguage,
      ∀ env op inputs output values target addresses before result effects after,
        freeingOperation (O := lang.operands) env op inputs output values target addresses
          before result effects after →
        lang.operation env op inputs output values target addresses before result effects after := by
  rintro ⟨lang, hfree⟩
  have halloc := Mem.alloc_wellFormed linuxX8664 Mem.Memory.empty (Mem.empty_wellFormed linuxX8664) ⟨0⟩ 1 1 1
  have allocEq : Mem.Memory.empty.alloc linuxX8664 ⟨0⟩ 1 1 1 =
      .ok (⟨0⟩, ⟨[(⟨0⟩, Mem.candidate ⟨0⟩ 1 1 1)], 1⟩) := rfl
  rw [allocEq] at halloc
  let allocated : Mem.Memory := ⟨[(⟨0⟩, Mem.candidate ⟨0⟩ 1 1 1)], 1⟩
  let freed : Mem.Memory := ⟨[(⟨0⟩, {Mem.candidate ⟨0⟩ 1 1 1 with live := false, exposed := false})], 1⟩
  let before : OperationStore lang.operands.Val := ⟨allocated, {}, []⟩
  let after : OperationStore lang.operands.Val := { before with bytes := freed }
  have hstep : lang.operation [] (.named ⟨"freeBlock"⟩) [] lang.operands.unitTy []
      linuxX8664 [] before lang.operands.unitVal [] after :=
    hfree _ _ _ _ _ _ _ _ _ _ _ ⟨rfl, rfl, ⟨0⟩, freed, rfl, rfl⟩
  have ⟨_, _, hpres⟩ := lang.preservesResidentHandles _ _ _ _ _ _ _ _ _ _ _ hstep halloc
  obtain ⟨block', found, live, _⟩ := hpres ⟨0⟩ (Mem.candidate ⟨0⟩ 1 1 1) rfl rfl
  cases found
  cases live

/-! ### Freeze F1a round 2: Core checking source-order controls (both routes) -/

private def slice0Project (src : String) : Project :=
  Project.singleton mainSourcePath (SourceText.ofBytes src.toUTF8.data.toList)

private def runBothRoutes (src : String) (prog : Surface0.Program) :
    Option ((RuleId × Nat × Nat) × (RuleId × Nat × Nat)) := do
  let prepared ← (SurfacePreparation.prepareSurface0 prog).toOption
  let input : CoreInput (sliceLanguage slice0) :=
    ⟨slice0Project src, slice0, .straightLine (.zero prepared) rfl⟩
  let checked ← match checkInput input with
    | .error (.refused c) => some (c.rule, c.primary.startOffset, c.primary.endOffset)
    | _ => none
  let unchecked ← match input.checkStatic with
    | .error (.refused c) => some (c.rule, c.primary.startOffset, c.primary.endOffset)
    | _ => none
  return (checked, unchecked)

private def sliceChars (src : String) (start stop : Nat) : String :=
  String.ofList ((src.toList.drop start).take (stop - start))

-- Required source-order controls on BOTH checked (`checkInput`) and unchecked (`checkStatic`) routes:
-- 1. `fn main() -> u8 { return 300 + (1 + 400); }` → `300` [25, 28)
-- 2. `fn main() -> u8 { return (1 + 400) + 300; }` → `400` [30, 33) (and compact `(1+400)` → [28, 31))
-- 3. `fn main() -> u8 { return 300 + 1; }` → `300` [25, 28)
-- 4. `fn main() -> u8 { return 300 + (1 + 2); }` → `300` [25, 28)
theorem core_checking_source_order_controls :
    let litRule : RuleId := ⟨``Slice0Rules.LiteralTyped, 1⟩
    let sRightBad := r"fn main() -> u8 { return 300 + (1 + 400); }"
    let pRightBad : Surface0.Program := ⟨"main", mkSpan 3 7,
      [.ret (.add (.integer 300 none (mkSpan 25 28))
        (.add (.integer 1 none (mkSpan 32 33)) (.integer 400 none (mkSpan 36 39)) (mkSpan 34 35))
        (mkSpan 29 30)) (mkSpan 18 41)], mkSpan 42 43⟩
    let sLeftBad := r"fn main() -> u8 { return (1 + 400) + 300; }"
    let pLeftBad : Surface0.Program := ⟨"main", mkSpan 3 7,
      [.ret (.add
        (.add (.integer 1 none (mkSpan 26 27)) (.integer 400 none (mkSpan 30 33)) (mkSpan 28 29))
        (.integer 300 none (mkSpan 37 40))
        (mkSpan 35 36)) (mkSpan 18 41)], mkSpan 42 43⟩
    let sLeftBadCompact := r"fn main() -> u8 { return (1+400) + 300; }"
    let pLeftBadCompact : Surface0.Program := ⟨"main", mkSpan 3 7,
      [.ret (.add
        (.add (.integer 1 none (mkSpan 26 27)) (.integer 400 none (mkSpan 28 31)) (mkSpan 27 28))
        (.integer 300 none (mkSpan 35 38))
        (mkSpan 33 34)) (mkSpan 18 39)], mkSpan 40 41⟩
    let sFlat := r"fn main() -> u8 { return 300 + 1; }"
    let pFlat : Surface0.Program := ⟨"main", mkSpan 3 7,
      [.ret (.add (.integer 300 none (mkSpan 25 28)) (.integer 1 none (mkSpan 31 32))
        (mkSpan 29 30)) (mkSpan 18 33)], mkSpan 34 35⟩
    let sRightGood := r"fn main() -> u8 { return 300 + (1 + 2); }"
    let pRightGood : Surface0.Program := ⟨"main", mkSpan 3 7,
      [.ret (.add (.integer 300 none (mkSpan 25 28))
        (.add (.integer 1 none (mkSpan 32 33)) (.integer 2 none (mkSpan 36 37)) (mkSpan 34 35))
        (mkSpan 29 30)) (mkSpan 18 39)], mkSpan 40 41⟩
    sliceChars sRightBad 25 28 = "300" ∧
    runBothRoutes sRightBad pRightBad = some ((litRule, 25, 28), (litRule, 25, 28)) ∧
    sliceChars sLeftBad 30 33 = "400" ∧
    runBothRoutes sLeftBad pLeftBad = some ((litRule, 30, 33), (litRule, 30, 33)) ∧
    sliceChars sLeftBadCompact 28 31 = "400" ∧
    runBothRoutes sLeftBadCompact pLeftBadCompact = some ((litRule, 28, 31), (litRule, 28, 31)) ∧
    sliceChars sFlat 25 28 = "300" ∧
    runBothRoutes sFlat pFlat = some ((litRule, 25, 28), (litRule, 25, 28)) ∧
    sliceChars sRightGood 25 28 = "300" ∧
    runBothRoutes sRightGood pRightGood = some ((litRule, 25, 28), (litRule, 25, 28)) := by
  exact ⟨rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩

-- Nested operands on BOTH sides, mixed literal/overflow order, stage-order preservation,
-- and valid right-nested acceptance on both routes.
theorem core_checking_nested_both_sides_and_mixed_controls :
    let litRule : RuleId := ⟨``Slice0Rules.LiteralTyped, 1⟩
    let addRule : RuleId := ⟨``Slice0Rules.AdditionFits, 1⟩
    -- Nested on both sides: bad literal in right subtree (`300` before `400`).
    let sBothRight := r"fn main() -> u8 { return (1 + 2) + (300 + (4 + 400)); }"
    let pBothRight : Surface0.Program := ⟨"main", mkSpan 3 7,
      [.ret (.add
        (.add (.integer 1 none (mkSpan 26 27)) (.integer 2 none (mkSpan 30 31)) (mkSpan 28 29))
        (.add (.integer 300 none (mkSpan 36 39))
          (.add (.integer 4 none (mkSpan 43 44)) (.integer 400 none (mkSpan 47 50)) (mkSpan 45 46))
          (mkSpan 40 41))
        (mkSpan 33 34)) (mkSpan 18 53)], mkSpan 54 55⟩
    -- Nested on both sides: bad literal in left subtree (`300` before `400` and `500`).
    let sBothLeft := r"fn main() -> u8 { return (300 + (1 + 400)) + (2 + 500); }"
    let pBothLeft : Surface0.Program := ⟨"main", mkSpan 3 7,
      [.ret (.add
        (.add (.integer 300 none (mkSpan 26 29))
          (.add (.integer 1 none (mkSpan 33 34)) (.integer 400 none (mkSpan 37 40)) (mkSpan 35 36))
          (mkSpan 30 31))
        (.add (.integer 2 none (mkSpan 46 47)) (.integer 500 none (mkSpan 50 53)) (mkSpan 48 49))
        (mkSpan 43 44)) (mkSpan 18 55)], mkSpan 56 57⟩
    -- Left bad literal before right overflowing addition: both routes report `300` [25, 28).
    let sLitBeforeOv := r"fn main() -> u8 { return 300 + (200 + 100); }"
    let pLitBeforeOv : Surface0.Program := ⟨"main", mkSpan 3 7,
      [.ret (.add (.integer 300 none (mkSpan 25 28))
        (.add (.integer 200 none (mkSpan 32 35)) (.integer 100 none (mkSpan 38 41)) (mkSpan 36 37))
        (mkSpan 29 30)) (mkSpan 18 43)], mkSpan 44 45⟩
    -- Left overflowing addition before right bad literal: checked reports inner `+` [30, 31),
    -- unchecked skips runtime overflow and reports `400` [39, 42).
    let sOvBeforeLit := r"fn main() -> u8 { return (200 + 100) + 400; }"
    let pOvBeforeLit : Surface0.Program := ⟨"main", mkSpan 3 7,
      [.ret (.add
        (.add (.integer 200 none (mkSpan 26 29)) (.integer 100 none (mkSpan 32 35)) (mkSpan 30 31))
        (.integer 400 none (mkSpan 39 42))
        (mkSpan 37 38)) (mkSpan 18 43)], mkSpan 44 45⟩
    -- Stage order preserved: surface `missing` in nested right still beats Core `300` on left.
    let sStageOrder := r"fn main() -> u8 { return 300 + (1 + missing); }"
    let pStageOrder : Surface0.Program := ⟨"main", mkSpan 3 7,
      [.ret (.add (.integer 300 none (mkSpan 25 28))
        (.add (.integer 1 none (mkSpan 32 33)) (.local "missing" (mkSpan 36 43)) (mkSpan 34 35))
        (mkSpan 29 30)) (mkSpan 18 45)], mkSpan 46 47⟩
    -- Valid right-nested program: accepted on both routes and evaluates to 6.
    let sValidRight := r"fn main() -> u8 { return 1 + (2 + 3); }"
    let pValidRight : Surface0.Program := ⟨"main", mkSpan 3 7,
      [.ret (.add (.integer 1 none (mkSpan 25 26))
        (.add (.integer 2 none (mkSpan 30 31)) (.integer 3 none (mkSpan 34 35)) (mkSpan 32 33))
        (mkSpan 27 28)) (mkSpan 18 37)], mkSpan 38 39⟩
    sliceChars sBothRight 36 39 = "300" ∧
    runBothRoutes sBothRight pBothRight = some ((litRule, 36, 39), (litRule, 36, 39)) ∧
    sliceChars sBothLeft 26 29 = "300" ∧
    runBothRoutes sBothLeft pBothLeft = some ((litRule, 26, 29), (litRule, 26, 29)) ∧
    sliceChars sLitBeforeOv 25 28 = "300" ∧
    runBothRoutes sLitBeforeOv pLitBeforeOv = some ((litRule, 25, 28), (litRule, 25, 28)) ∧
    sliceChars sOvBeforeLit 30 31 = "+" ∧ sliceChars sOvBeforeLit 39 42 = "400" ∧
    runBothRoutes sOvBeforeLit pOvBeforeLit = some ((addRule, 30, 31), (litRule, 39, 42)) ∧
    sliceChars sStageOrder 36 43 = "missing" ∧
    SurfacePreparation.prepareSurface0 pStageOrder = .error (.surface (.unknownLocal "missing" (mkSpan 36 43))) ∧
    (match SurfacePreparation.prepareSurface0 pValidRight with
      | .ok ep =>
        let inp : CoreInput (sliceLanguage slice0) :=
          ⟨slice0Project sValidRight, slice0, .straightLine (.zero ep) rfl⟩
        (checkInput inp).isOk && inp.checkStatic.isOk &&
          decide (run (AdmissionView.zero ep).program 10 = .exit 6)
      | .error _ => false) = true := by
  exact ⟨rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩

-- Rich `CoreInput.checkStatic`: passes unchecked overflow (while `checkInput` refuses `AdditionFits@1`),
-- and refuses static violations with exact `RegisteredRule`, primary span, related spans, and `StaticRefuses`.
theorem checkStatic_rich_controls (project : Project) :
    let declSpan := mkSpan 22 23
    let opSpan := mkSpan 30 37
    let endSpan := mkSpan 40 49
    let overflowView : AdmissionView :=
      .zero ⟨[⟨⟨⟨0⟩, .add (.lit 200) (.lit 56)⟩, opSpan, declSpan, (mkSpan 30 33, mkSpan 36 38), .unnamed none⟩],
        .var ⟨0⟩, endSpan, endSpan⟩
    let overflowInput : CoreInput (sliceLanguage slice0) := ⟨project, slice0, .straightLine overflowView rfl⟩
    let liveLinearView : AdmissionView :=
      .one ⟨[(token, declSpan)],
        [⟨.bind (.name x) (.nominal token) (.construct token), opSpan, declSpan, .unnamed none⟩],
        .ret (.lit 0), endSpan⟩
    let liveLinearInput : CoreInput (sliceLanguage sliceOne) := ⟨project, sliceOne, .straightLine liveLinearView rfl⟩
    let liveLinearReason := Refusal.atPosition 1 (.liveLinearBinding x)
    let liveLinearCause := RefusalCause.core .returning (surfaceFileSpan endSpan)
      [(⟨"binding"⟩, surfaceFileSpan declSpan)] liveLinearReason
    let unknownStructView : AdmissionView :=
      .one ⟨[], [⟨.destructure token (.construct token), opSpan, declSpan, .unnamed none⟩],
        .ret (.lit 0), endSpan⟩
    let unknownStructInput : CoreInput (sliceLanguage sliceOne) := ⟨project, sliceOne, .straightLine unknownStructView rfl⟩
    let unknownStructReason := Refusal.atPosition 0 (.unknownStruct token)
    let unknownStructCause := RefusalCause.core .returning (surfaceFileSpan opSpan) [] unknownStructReason
    overflowInput.checkStatic = .ok () ∧
    checkInput overflowInput = .error (.refused (.core .slice0 (surfaceFileSpan opSpan) [] (.atPosition 0 .additionOverflowUnproven))) ∧
    liveLinearInput.checkStatic = .error (.refused liveLinearCause) ∧
    liveLinearCause.rule = ⟨``OwnershipRule.ReturnDischarged, 1⟩ ∧
    liveLinearInput.StaticRefuses liveLinearCause ∧
    unknownStructInput.checkStatic = .error (.refused unknownStructCause) ∧
    unknownStructCause.rule = ⟨``Surface1.StructDeclared, 1⟩ ∧
    unknownStructInput.StaticRefuses unknownStructCause := by
  refine ⟨rfl, rfl, rfl, rfl, checkStatic_refused (failure := _) rfl, rfl, rfl, checkStatic_refused (failure := _) rfl⟩

end StageBoundaryControls

end ConcreteSpec.Admission.Controls
