import Concrete.Memory.Placement

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

/-- A running jump after fetch, argument evaluation and target signature checking. -/
structure JumpAt (memory : Memory lang profile) (q read : MemoryRegisters language profile)
    (dst : MemoryBlock lang) (edge : CFGEdge (Sigma (MemoryRead _ lang.literal))) (values : List language.operands.Val) : Prop where
  running : q.resourceExhausted = false
  source : ∃ entry, memory.blockAt q = some entry ∧ q.instruction = entry.body.length ∧ entry.ending = .jump edge
  args : memory.readOperands edge.arguments q = some (values, read)
  target : memory.blockAt {q with block := edge.target} = some dst
  types : edge.arguments.map Sigma.fst = dst.params.map Sigma.fst

private theorem read_frame {memory : Memory lang profile} {q read : MemoryRegisters language profile}
    {ty} {op : MemoryRead _ lang.literal ty} {v} (args : memory.readValue q op = some (v, read)) :
    read.effects = q.effects ∧ read.resourceExhausted = q.resourceExhausted := by
  cases op with
  | literal =>
    simp only [readValue] at args
    split at args <;> obtain ⟨_, _, eq⟩ := Option.bind_eq_some_iff.mp args <;>
      cases eq <;> exact ⟨rfl, rfl⟩
  | copy p =>
    obtain rfl := ((memory.copyRead_iff q p v read).mp args).2.2.2
    exact ⟨rfl, rfl⟩
  | move p =>
    simp only [readValue] at args
    generalize memory.ownerView q p.logical.root ty .owned = view at args
    obtain ⟨owners, id⟩ := view
    obtain ⟨_, _, body⟩ := Option.bind_eq_some_iff.mp args
    obtain ⟨_, _, body⟩ := Option.bind_eq_some_iff.mp body
    split at body <;> cases body <;> exact ⟨rfl, rfl⟩

private theorem reads_frame {memory : Memory lang profile} {q read : MemoryRegisters language profile}
    {ops vs} (args : memory.readOperands ops q = some (vs, read)) :
    read.effects = q.effects ∧ read.resourceExhausted = q.resourceExhausted := by
  induction ops generalizing q vs with
  | nil => cases args; exact ⟨rfl, rfl⟩
  | cons op rest ih =>
    obtain ⟨ty, op⟩ := op
    obtain ⟨⟨_, after⟩, head, tail⟩ := Option.bind_eq_some_iff.mp args
    obtain ⟨⟨_, final⟩, more, eq⟩ := Option.bind_eq_some_iff.mp tail
    cases eq
    have one := read_frame head
    have allReads := ih more
    exact ⟨allReads.1.trans one.1, allReads.2.trans one.2⟩

/-- Exhaustion at the parameter checkpoint characterizes unlimited validity
together with the absence of a physical placement. -/
theorem ParamOutcome.exhausted_iff {memory : Memory lang profile} (wf : memory.LayoutsWF)
    {ps vs q} :
    (∃ partialState, memory.ParamOutcome (language := language) ps vs q partialState true) ↔
      (∃ ceiling, memory.ParamsValid ps vs q ceiling) ∧ ¬ memory.ParamSpaceFits ps q := by
  constructor
  · rintro ⟨_, failed⟩
    exact ⟨⟨_, failed.valid_of_exhausted⟩, failed.2.2⟩
  · rintro ⟨⟨_, valid⟩, absent⟩
    exact ⟨q, (valid.at_bound wf).exhausted_of_no_space absent⟩

theorem JumpAt.exhausted_iff {memory : Memory lang profile} (wf : memory.LayoutsWF)
    {q read dst edge vs} (jump : memory.JumpAt (language := language) q read dst edge vs) :
    let arrive := {read with block := edge.target, instruction := 0}
    (Execution.step memory q {q with resourceExhausted := true} ∧
      Execution.result memory {q with resourceExhausted := true} .resourceExhausted) ↔
      (∃ ceiling, memory.ParamsValid dst.params vs arrive ceiling) ∧ ¬ memory.ParamSpaceFits dst.params arrive := by
  dsimp only
  obtain ⟨running, ⟨entry, lookup, pc, ending⟩, args, target, types⟩ := jump
  constructor
  · rintro ⟨tick, _⟩
    have step : memory.Step q {q with resourceExhausted := true} := tick.2.1
    simp only [Step, running, Bool.false_eq_true, ite_false, ite_true] at step
    obtain ⟨_, partialState, raw⟩ := step
    have transfer := raw.terminatorAt lookup (List.getElem?_eq_none (by omega))
    rw [ending] at transfer
    obtain ⟨_, _, _, got, found, _, outcome⟩ := transfer
    cases Option.some.inj (args.symm.trans got)
    cases Option.some.inj (target.symm.trans found)
    exact (ParamOutcome.exhausted_iff wf).mp ⟨partialState, outcome⟩
  · rintro ⟨⟨_, valid⟩, absent⟩
    exact jump_exhausted_of_no_space running lookup pc ending args target types (valid.at_bound wf) absent

theorem JumpAt.normal_of_fit {memory : Memory lang profile} (wf : memory.LayoutsWF)
    {q read dst edge vs} (jump : memory.JumpAt (language := language) q read dst edge vs) {c}
    (valid : memory.ParamsValid dst.params vs {read with block := edge.target, instruction := 0} c)
    (fits : memory.ParamSpaceFits dst.params {read with block := edge.target, instruction := 0}) :
    ∃ next, Execution.step memory q next ∧ next.resourceExhausted = false := by
  obtain ⟨running, ⟨entry, lookup, pc, ending⟩, args, target, types⟩ := jump
  obtain ⟨next, writes, effects, flag⟩ := valid.physical wf fits
  have before := reads_frame args
  have normal := (flag.trans before.2).trans running
  refine ⟨next, silent_step ?_ ?_ (effects.trans before.1), normal⟩
  · simp [terminalObservation, running, lookup, pc, ending]
  · simp only [Step, running, normal, Bool.false_eq_true, ite_false]
    refine ⟨entry, lookup, ?_⟩
    simp only [List.getElem?_eq_none (by omega : entry.body.length ≤ q.instruction), ending]
    exact ⟨pc, _, _, _, args, target, types, writes⟩

/-- For an unlimited-valid transfer, a normal successor exists exactly when
the complete list of fresh roots admits a physical placement. -/
theorem JumpAt.normal_iff_fit {memory : Memory lang profile} (wf : memory.LayoutsWF)
    {q read dst edge vs} (jump : memory.JumpAt (language := language) q read dst edge vs) {c}
    (valid : memory.ParamsValid dst.params vs {read with block := edge.target, instruction := 0} c) :
    (∃ next, Execution.step memory q next ∧ next.resourceExhausted = false) ↔
      memory.ParamSpaceFits dst.params {read with block := edge.target, instruction := 0} := by
  refine ⟨?_, jump.normal_of_fit wf valid⟩
  rintro ⟨next, tick, normal⟩
  obtain ⟨running, ⟨entry, lookup, pc, ending⟩, args, target, _⟩ := jump
  have raw : memory.Step q next := tick.2.1
  simp only [Step, running, normal, Bool.false_eq_true, ite_false] at raw
  have step := raw.terminatorAt lookup (List.getElem?_eq_none (by omega))
  rw [ending] at step
  obtain ⟨_, _, _, got, found, _, writes⟩ := step
  cases Option.some.inj (args.symm.trans got)
  cases Option.some.inj (target.symm.trans found)
  exact ParamsValid.spaceFits wf ⟨valid.1.atCeiling (Nat.min_le_left ..), next, writes⟩

theorem JumpAt.progress {memory : Memory lang profile} (wf : memory.LayoutsWF)
    {q read dst edge vs} (jump : memory.JumpAt (language := language) q read dst edge vs) {c}
    (valid : memory.ParamsValid dst.params vs {read with block := edge.target, instruction := 0} c) :
    ∃ next, Execution.step memory q next := by
  by_cases fits : memory.ParamSpaceFits dst.params {read with block := edge.target, instruction := 0}
  · obtain ⟨next, step, _⟩ := jump.normal_of_fit wf valid fits
    exact ⟨next, step⟩
  · exact ⟨_, ((jump.exhausted_iff wf).mpr ⟨⟨_, valid⟩, fits⟩).1⟩

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
