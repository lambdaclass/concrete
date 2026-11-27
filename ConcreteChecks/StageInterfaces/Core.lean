import ConcreteChecks.StageInterfaces.Fixtures

open Concrete
open Concrete.Interfaces
open ConcreteSpec Core Core.SourcePathSpan StageInterface StageGrammar

namespace StageInterfaceControls

section
variable (verifier : TableVerifier grammar) (size : proposal.size = verifier.words)
  (accept : (verifier.run proposal).2 = some true)

local notation "parser" => testParser verifier size accept

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

end

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

end StageInterfaceControls
