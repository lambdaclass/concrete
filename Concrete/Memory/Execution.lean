import Concrete.Interfaces.Contracts

namespace Concrete.Interfaces.Memory

open ConcreteSpec.Core ConcreteSpec.StageInterface

variable {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty} {profile : Profile}

theorem Step.normal {memory : Memory lang profile} {q next : MemoryRegisters language profile}
    {startDefinitions} (tick : memory.Step q next startDefinitions)
    (available : ∀ partialState, ¬ memory.StepOutcome q partialState true startDefinitions) :
    memory.StepOutcome q next false startDefinitions := by
  unfold Step at tick
  split at tick
  · contradiction
  split at tick
  · obtain ⟨_, s, failed⟩ := tick
    exact (available s failed).elim
  · exact tick

/-- Both a committed step and an aborted step fetched the instruction at the
original PC. Ruling out both raw outcomes thus rules out a successor. -/
theorem Step.exists_outcome {memory : Memory lang profile} {q next : MemoryRegisters language profile}
    {startDefinitions} (tick : memory.Step q next startDefinitions) :
    ∃ s mode, memory.StepOutcome q s mode startDefinitions := by
  unfold Step at tick
  split at tick
  · contradiction
  split at tick
  · obtain ⟨_, s, failed⟩ := tick
    exact ⟨s, true, failed⟩
  · exact ⟨next, false, tick⟩

theorem StepOutcome.instructionAt {memory : Memory lang profile} {q next : MemoryRegisters language profile}
    {exhausting block op} (tick : memory.StepOutcome q next exhausting)
    (found : memory.blockAt q = some block) (pc : block.body[q.instruction]? = some op) :
    memory.InstructionStep q next false op.instruction exhausting := by
  obtain ⟨_, lookup, body⟩ := tick
  cases Option.some.inj (lookup.symm.trans found)
  simpa only [pc, InstructionStep] using body

theorem StepOutcome.terminatorAt {memory : Memory lang profile} {q next : MemoryRegisters language profile}
    {startDefinitions exhausting block} (tick : memory.StepOutcome q next exhausting startDefinitions)
    (found : memory.blockAt q = some block) (pc : block.body[q.instruction]? = none) :
    memory.ControlStep q next block.ending exhausting := by
  obtain ⟨_, lookup, body⟩ := tick
  cases Option.some.inj (lookup.symm.trans found)
  rw [pc] at body
  exact body.2

theorem ParamOutcome.empty {memory : Memory lang profile} {q next : MemoryRegisters language profile}
    {exhausting} (h : memory.ParamOutcome [] [] q next exhausting) : next = q ∧ exhausting = false := by
  cases exhausting
  · cases h
    exact ⟨rfl, rfl⟩
  · exact (h.2.2 trivial).elim

/-- A running jump with a valid complete transfer and no physical joint
placement has a successor at its original checkpoint and a terminal result. -/
theorem jump_exhausted_of_no_space {memory : Memory lang profile} {q read : MemoryRegisters language profile}
    {entry dst : MemoryBlock lang} {edge values}
    (running : q.resourceExhausted = false) (lookup : memory.blockAt q = some entry)
    (pc : q.instruction = entry.body.length) (ending : entry.ending = .jump edge)
    (args : memory.readOperands edge.arguments q = some (values, read))
    (target : memory.blockAt {q with block := edge.target} = some dst)
    (types : edge.arguments.map Sigma.fst = dst.params.map Sigma.fst)
    (valid : memory.ParamsValid dst.params values {read with block := edge.target, instruction := 0})
    (unavailable : ¬ memory.ParamSpaceFits dst.params {read with block := edge.target, instruction := 0}) :
    let stopped := {q with resourceExhausted := true}
    Execution.step memory q stopped ∧ Execution.result memory stopped .resourceExhausted := by
  have endPC : entry.body[q.instruction]? = none := List.getElem?_eq_none (by omega)
  refine ⟨silent_step ?_ ?_ rfl, memory.exhausted_result rfl⟩
  · simp [terminalObservation, running, lookup, pc, ending]
  · simp only [Step, running, Bool.false_eq_true, ite_false, ite_true, true_and]
    refine ⟨{read with block := edge.target, instruction := 0}, entry, lookup, ?_⟩
    simp only [endPC, ending]
    refine ⟨pc, ?_⟩
    exact ⟨_, _, _, args, target, types, valid.exhausted_of_no_space unavailable⟩

theorem bind_cannot_exhaust {memory : Memory lang profile} {q next : MemoryRegisters language profile}
    {ty} {dst : MemoryPlace _ ty} {src} : ¬ memory.InstructionStep q next false (.bind dst src) true := by
  rintro ⟨_, _, _, _, impossible, _⟩
  exact impossible

theorem op_cannot_exhaust {memory : Memory lang profile} {q next : MemoryRegisters language profile}
    {opcode types ty dst signature args checks addresses} :
    ¬ memory.InstructionStep q next false (@MemoryInst.op _ lang opcode types ty dst signature args checks addresses) true := by
  rintro ⟨_, _, _, _, _, _, _, _, _, _, _, _, impossible, _⟩
  exact impossible

theorem matchingResult_result {memory : Memory lang profile} {q : MemoryRegisters language profile} {o}
    (h : memory.MatchingResult q o) : Memory.Execution.result memory q o := by
  unfold Memory.MatchingResult at h
  split at h
  · contradiction
  · exact h

/-- Every terminal observation either matches through the ordinary hosted
result or identifies an exhausted physical reservation. -/
theorem result_cases {memory : Memory lang profile} {q : MemoryRegisters language profile} {o}
    (h : Memory.Execution.result memory q o) : memory.MatchingResult q o ∨ o = .resourceExhausted := by
  unfold Memory.MatchingResult
  split
  · rename_i flag
    exact .inr (Option.some.inj (h.symm.trans (memory.exhausted_result flag)))
  · exact .inl h

end Concrete.Interfaces.Memory
