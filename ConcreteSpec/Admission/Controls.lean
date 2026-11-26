import ConcreteSpec.Admission.Early

namespace ConcreteSpec.Admission.Controls

private def return42 : StageInterface.EarlyProgram :=
  ⟨[], .lit 42, ⟨0, 2, 1, 1, ⟨0⟩⟩⟩

private def return256 : StageInterface.EarlyProgram :=
  { return42 with ret := .lit 256 }

theorem return42_admitted : (admitEarly return42).isOk = true := rfl

theorem return256_refused :
    admitEarly return256 = .error (.atPosition 0 (.literalOutOfRange .u8 256)) := rfl

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
      .error (.duplicateStructName item) := rfl

theorem declaration_duplicate_field :
    checkProgram ⟨[{ itemDecl with fields := itemDecl.fields ++ [(flag, .bool)] }], [], ret42⟩ =
      .error (.duplicateDeclaredField item flag) := rfl

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

end ConcreteSpec.Admission.Controls
