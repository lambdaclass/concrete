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

theorem StepOutcome.instructionAtWith {memory : Memory lang profile} {q next : MemoryRegisters language profile}
    {startDefinitions exhausting block op} (tick : memory.StepOutcome q next exhausting startDefinitions)
    (found : memory.blockAt q = some block) (pc : block.body[q.instruction]? = some op) :
    memory.InstructionStepWith q next false startDefinitions op.instruction exhausting := by
  obtain ⟨_, lookup, body⟩ := tick
  cases Option.some.inj (lookup.symm.trans found)
  rw [pc] at body
  exact body

theorem StepOutcome.instructionAt {memory : Memory lang profile} {q next : MemoryRegisters language profile}
    {exhausting block op} (tick : memory.StepOutcome q next exhausting)
    (found : memory.blockAt q = some block) (pc : block.body[q.instruction]? = some op) :
    memory.InstructionStep q next false op.instruction exhausting :=
  tick.instructionAtWith found pc

theorem StepOutcome.terminatorAt {memory : Memory lang profile} {q next : MemoryRegisters language profile}
    {startDefinitions exhausting block} (tick : memory.StepOutcome q next exhausting startDefinitions)
    (found : memory.blockAt q = some block) (pc : block.body[q.instruction]? = none) :
    memory.ControlStep q next block.ending exhausting := by
  obtain ⟨_, lookup, body⟩ := tick
  cases Option.some.inj (lookup.symm.trans found)
  rw [pc] at body
  exact body.2

theorem blockAt_congr {memory : Memory lang profile} {a b : MemoryRegisters language profile}
    (frame : a.frame = b.frame) (block : a.block = b.block) :
    memory.blockAt a = memory.blockAt b := by
  simp only [blockAt, frame, block]

theorem instructionTrap_call {memory : Memory lang profile}
    {q read : MemoryRegisters language profile} {callee dst args checks values}
    (evalArgs : memory.readOperands args q = some (values, read))
    (noTrap : firstTrap (.call callee) values checks = none) :
    memory.instructionTrap q (.call callee dst args checks) = none := by
  simp only [instructionTrap, bind, Option.bind, evalArgs, noTrap]

theorem instructionTrap_beginLifetime {memory : Memory lang profile}
    {q : MemoryRegisters language profile} {root} :
    memory.instructionTrap q (.beginLifetime root) = none := by
  simp only [instructionTrap]

theorem instructionTrap_destructure {memory : Memory lang profile}
    {q : MemoryRegisters language profile} {ty} {src : MemoryPlace _ ty} {bindings} :
    memory.instructionTrap q (.destructure src bindings) = none := by
  simp only [instructionTrap]

theorem terminalObservation_of_instruction {memory : Memory lang profile}
    {q : MemoryRegisters language profile} {block op}
    (running : q.resourceExhausted = false) (found : memory.blockAt q = some block)
    (pc : block.body[q.instruction]? = some op)
    (noTrap : memory.instructionTrap q op.instruction = none) :
    memory.terminalObservation q = none := by
  unfold terminalObservation
  simp only [running, Bool.false_eq_true, ite_false, found, Option.bind_eq_bind,
    Option.bind_some, pc, noTrap, Option.bind_none]

theorem terminalObservation_of_jump {memory : Memory lang profile}
    {q : MemoryRegisters language profile} {block edge}
    (running : q.resourceExhausted = false) (found : memory.blockAt q = some block)
    (pc : q.instruction = block.body.length) (ending : block.ending = .jump edge) :
    memory.terminalObservation q = none := by
  have endPC : block.body[block.body.length]? = none := List.getElem?_eq_none (Nat.le_refl _)
  unfold terminalObservation
  simp only [running, Bool.false_eq_true, ite_false, found, Option.bind_eq_bind,
    Option.bind_some, pc, endPC, bne_self_eq_false, ending]

theorem terminalObservation_of_branch {memory : Memory lang profile}
    {q : MemoryRegisters language profile} {block test yes no}
    (running : q.resourceExhausted = false) (found : memory.blockAt q = some block)
    (pc : q.instruction = block.body.length) (ending : block.ending = .branch test yes no) :
    memory.terminalObservation q = none := by
  have endPC : block.body[block.body.length]? = none := List.getElem?_eq_none (Nat.le_refl _)
  unfold terminalObservation
  simp only [running, Bool.false_eq_true, ite_false, found, Option.bind_eq_bind,
    Option.bind_some, pc, endPC, bne_self_eq_false, ending]

theorem terminalObservation_of_nonEntry_ret {memory : Memory lang profile}
    {q : MemoryRegisters language profile} {block operand caller rest}
    (running : q.resourceExhausted = false) (found : memory.blockAt q = some block)
    (pc : q.instruction = block.body.length) (ending : block.ending = .ret operand)
    (callers : q.callers = caller :: rest) :
    memory.terminalObservation q = none := by
  have endPC : block.body[block.body.length]? = none := List.getElem?_eq_none (Nat.le_refl _)
  unfold terminalObservation
  cases operand with
  | none =>
    simp only [running, Bool.false_eq_true, ite_false, found, Option.bind_eq_bind,
      Option.bind_some, pc, endPC, bne_self_eq_false, ending]
  | some pair =>
    obtain ⟨_, op⟩ := pair
    simp only [running, Bool.false_eq_true, ite_false, found, Option.bind_eq_bind,
      Option.bind_some, pc, endPC, bne_self_eq_false, ending, callers, List.isEmpty_cons,
      Bool.not_false, Bool.true_or, ite_true]

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

/-- Reading an operand leaves control, backing ranges, callers, effects and the
exhaustion flag unchanged. -/
structure ReadFrame (q read : MemoryRegisters language profile) : Prop where
  frame : read.frame = q.frame
  block : read.block = q.block
  instruction : read.instruction = q.instruction
  materialized : read.materialized = q.materialized
  callers : read.callers = q.callers
  effects : read.effects = q.effects
  resourceExhausted : read.resourceExhausted = q.resourceExhausted

theorem ReadFrame.refl (q : MemoryRegisters language profile) : ReadFrame q q :=
  ⟨rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩

theorem ReadFrame.trans {a b c : MemoryRegisters language profile}
    (ab : ReadFrame a b) (bc : ReadFrame b c) : ReadFrame a c :=
  ⟨bc.frame.trans ab.frame, bc.block.trans ab.block, bc.instruction.trans ab.instruction,
   bc.materialized.trans ab.materialized, bc.callers.trans ab.callers,
   bc.effects.trans ab.effects, bc.resourceExhausted.trans ab.resourceExhausted⟩

theorem readValue_frame {memory : Memory lang profile} {q read : MemoryRegisters language profile}
    {ty} {op : MemoryRead _ lang.literal ty} {v} (args : memory.readValue q op = some (v, read)) :
    ReadFrame q read := by
  cases op with
  | literal =>
    simp only [readValue] at args
    obtain ⟨_, _, eq⟩ := Option.bind_eq_some_iff.mp args
    cases eq
    exact .refl _
  | copy p =>
    obtain rfl := ((memory.copyRead_iff q p v read).mp args).2.2.2
    exact .refl _
  | move p =>
    simp only [readValue] at args
    generalize memory.ownerView q p.logical.root ty .owned = view at args
    obtain ⟨owners, id⟩ := view
    obtain ⟨_, _, body⟩ := Option.bind_eq_some_iff.mp args
    obtain ⟨_, _, body⟩ := Option.bind_eq_some_iff.mp body
    split at body <;> cases body <;> exact ⟨rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩

theorem readOperands_frame {memory : Memory lang profile} {q read : MemoryRegisters language profile}
    {ops vs} (args : memory.readOperands ops q = some (vs, read)) :
    ReadFrame q read := by
  induction ops generalizing q vs with
  | nil => cases args; exact .refl _
  | cons op rest ih =>
    obtain ⟨ty, op⟩ := op
    obtain ⟨⟨_, after⟩, head, tail⟩ := Option.bind_eq_some_iff.mp args
    obtain ⟨⟨_, final⟩, more, eq⟩ := Option.bind_eq_some_iff.mp tail
    cases eq
    exact (readValue_frame head).trans (ih more)

/-- Zero-sized materialized locals never carry a byte-backing range.
This rules out ambiguous `Ends` transitions when releasing a callee frame. -/
def ZeroUnbacked (memory : Memory lang profile) (q : MemoryRegisters language profile) : Prop :=
  ∀ place layout, memory.layoutAt q.frame place = some layout → layout.size = 0 →
    AList.lookup q.materialized place = none

theorem Represents.zeroUnbacked {memory : Memory lang profile} {logical}
    {q : MemoryRegisters language profile} {ranges}
    (rep : memory.Represents logical q ranges) : memory.ZeroUnbacked q := by
  intro place layout hlayout hz
  cases hlook : AList.lookup q.materialized place with
  | none => rfl
  | some range =>
    have ⟨live, cls, _⟩ := rep.2.2.2.2.2.2.2.2.2.2.2.1 place range (AList.lookup_mem hlook)
    have ⟨_, _, _, storage, _, _, hcls, hmat, _⟩ := rep.2.2.2.2.2.2.2.2.2.2.2.2 place live
    cases Option.some.inj (cls.symm.trans hcls)
    obtain ⟨layout', hlayout', hzero⟩ := hmat rfl
    cases Option.some.inj (hlayout.symm.trans hlayout')
    have isNone := hzero.mp hz
    rw [hlook] at isNone
    contradiction

theorem RepresentsCheckpoint.zeroUnbacked {memory : Memory lang profile}
    {p q : MemoryRegisters language profile} (rep : memory.RepresentsCheckpoint p q) :
    memory.ZeroUnbacked q :=
  rep.1.zeroUnbacked

theorem Ends.frame {memory : Memory lang profile} {q next : MemoryRegisters language profile}
    {place} (h : memory.Ends q place next) :
    next.frame = q.frame ∧ next.block = q.block ∧ next.instruction = q.instruction ∧
    next.callers = q.callers ∧ next.effects = q.effects ∧ next.resourceExhausted = q.resourceExhausted := by
  rcases h.2 with ⟨_, rfl⟩ | ⟨_, ⟨_, _, _, rfl⟩ | ⟨_, _, _, _, _, _, rfl⟩⟩ <;>
    exact ⟨rfl, rfl, rfl, rfl, rfl, rfl⟩

theorem EndLocals.frame {memory : Memory lang profile} {places} {q next : MemoryRegisters language profile}
    (h : memory.EndLocals places q next) :
    next.frame = q.frame ∧ next.block = q.block ∧ next.instruction = q.instruction ∧
    next.callers = q.callers ∧ next.effects = q.effects ∧ next.resourceExhausted = q.resourceExhausted := by
  induction h with
  | nil => exact ⟨rfl, rfl, rfl, rfl, rfl, rfl⟩
  | cons step _ ih =>
    have one := step.frame
    exact ⟨ih.1.trans one.1, ih.2.1.trans one.2.1, ih.2.2.1.trans one.2.2.1,
      ih.2.2.2.1.trans one.2.2.2.1, ih.2.2.2.2.1.trans one.2.2.2.2.1,
      ih.2.2.2.2.2.trans one.2.2.2.2.2⟩

theorem Ends.zeroUnbacked {memory : Memory lang profile} {q next : MemoryRegisters language profile}
    {place} (unbacked : memory.ZeroUnbacked q) (h : memory.Ends q place next) :
    memory.ZeroUnbacked next := by
  have eqMat : next.frame = q.frame ∧ next.materialized = AList.erase q.materialized place := by
    rcases h.2 with ⟨_, rfl⟩ | ⟨_, ⟨_, _, _, rfl⟩ | ⟨_, _, _, _, _, _, rfl⟩⟩ <;> exact ⟨rfl, rfl⟩
  intro p layout hlayout hz
  rw [eqMat.1] at hlayout
  rw [eqMat.2, AList.lookup_erase]
  split
  · rfl
  · exact unbacked p layout hlayout hz

theorem Ends.unique {memory : Memory lang profile} {q a b : MemoryRegisters language profile}
    {place} (unbacked : memory.ZeroUnbacked q) (ha : memory.Ends q place a) (hb : memory.Ends q place b) :
    a = b := by
  rcases ha.2 with ⟨va, rfl⟩ | ⟨ma, ⟨la, hla, hza, rfl⟩ | ⟨ra, ba, byta, hma, hpa, hfa, rfl⟩⟩ <;>
    rcases hb.2 with ⟨vb, rfl⟩ | ⟨mb, ⟨lb, hlb, hzb, rfl⟩ | ⟨rb, bb, bytb, hmb, hpb, hfb, rfl⟩⟩
  · rfl
  · cases va.symm.trans mb
  · cases va.symm.trans mb
  · cases ma.symm.trans vb
  · rfl
  · cases (unbacked place la hla hza).symm.trans hmb
  · cases ma.symm.trans vb
  · cases (unbacked place lb hlb hzb).symm.trans hma
  · cases Option.some.inj (hma.symm.trans hmb)
    cases Option.some.inj (hpa.symm.trans hpb)
    cases Option.some.inj (hfa.symm.trans hfb)
    rfl

theorem EndLocals.unique {memory : Memory lang profile} {places} {q a b : MemoryRegisters language profile}
    (unbacked : memory.ZeroUnbacked q) (ha : memory.EndLocals places q a) (hb : memory.EndLocals places q b) :
    a = b := by
  induction ha generalizing b with
  | nil => cases hb; rfl
  | cons stepA _ ih =>
    cases hb with
    | cons stepB restB =>
      obtain rfl := stepA.unique unbacked stepB
      exact ih (stepA.zeroUnbacked unbacked) restB

private theorem writeValue_live {memory : Memory lang profile} {q next : MemoryRegisters language profile}
    {ty} {p : MemoryPlace _ ty} {v c} (h : memory.writeValue q p v c = some next) :
    q.live.contains p.logical.root = true := by
  unfold writeValue at h
  cases hlive : q.live.contains p.logical.root
  · simp only [hlive, Bool.not_false, Bool.true_or, ite_true] at h
    cases h
  · rfl

theorem StepOutcome.startDefinitions_of_false {memory : Memory lang profile}
    {q next : MemoryRegisters language profile} {exhausting}
    (h : memory.StepOutcome q next exhausting false) :
    memory.StepOutcome q next exhausting true := by
  obtain ⟨block, lookup, body⟩ := h
  refine ⟨block, lookup, ?_⟩
  cases pc : block.body[q.instruction]? with
  | none =>
    rw [pc] at body
    exact body
  | some op =>
    rw [pc] at body
    dsimp only at body ⊢
    obtain ⟨_, inst⟩ := op
    cases inst with
    | bind dst src =>
      obtain ⟨value, afterRead, stored, read, write, eq⟩ := body
      cases exhausting
      · refine ⟨value, afterRead, stored, read, ?_, eq⟩
        exact .cons (ite_eq_left (writeValue_live write.2.2.1) ▸ rfl) write (.nil _)
      · exact write.elim
    | assign dst src => exact body
    | op dst opcode args checks addresses =>
      obtain ⟨values, afterReads, operands, result, effects, changed, stored,
        hargs, htrap, haddr, hop, hkeys, write, eq⟩ := body
      cases exhausting
      · refine ⟨values, afterReads, operands, result, effects, changed, stored,
          hargs, htrap, haddr, hop, hkeys, ?_, eq⟩
        exact .cons (ite_eq_left (writeValue_live write.2.2.1) ▸ rfl) write (.nil _)
      · exact write.elim
    | addWithOverflow t dst flag lhs rhs =>
      obtain ⟨ht, hbool, hdisj, lv, afterLhs, rv, afterReads, x, y, stored,
        hread1, hread2, hlv, hrv, hwrite, heq⟩ := body
      cases exhausting
      · obtain ⟨mid, w1, w2⟩ := hwrite
        refine ⟨ht, hbool, hdisj, lv, afterLhs, rv, afterReads, x, y, stored,
          hread1, hread2, hlv, hrv, ?_, heq⟩
        exact .cons (ite_eq_left (writeValue_live w1.2.2.1) ▸ rfl) w1
          (.cons (ite_eq_left (writeValue_live w2.2.2.1) ▸ rfl) w2 (.nil _))
      · exact hwrite.elim
    | beginLifetime place => exact body
    | endLifetime place => exact body
    | destructure src bindings => exact body
    | call callee dst args checks => exact body

theorem Step.startDefinitions_of_false {memory : Memory lang profile}
    {q next : MemoryRegisters language profile}
    (h : memory.Step q next false) : memory.Step q next true := by
  cases hq : q.resourceExhausted <;> cases hn : next.resourceExhausted <;>
    simp only [Step, hq, hn, Bool.false_eq_true, ite_false, ite_true] at h ⊢
  · exact h.startDefinitions_of_false
  · obtain ⟨eq, s, raw⟩ := h
    exact ⟨eq, s, raw.startDefinitions_of_false⟩

theorem ssa_step_of_memory {ssa : SSA lang profile} {q next : MemoryRegisters language profile}
    (h : Memory.Execution.step (ssa.memoryView (language := language)) q next) :
    SSA.Execution.step ssa q next :=
  ⟨h.1, h.2.1.startDefinitions_of_false, h.2.2⟩

theorem bir_step_of_memory {bir : BIR lang profile} {q next : MemoryRegisters language profile}
    (h : Memory.Execution.step (bir.memoryView (language := language)) q next) :
    BIR.Execution.step bir q next :=
  ⟨h.1, h.2.1.startDefinitions_of_false, h.2.2⟩

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
  have before := readOperands_frame args
  have normal := (flag.trans before.resourceExhausted).trans running
  refine ⟨next, silent_step ?_ ?_ (effects.trans before.effects), normal⟩
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
