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
  · exact (h.2 ⟨q, .nil q⟩).elim

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
