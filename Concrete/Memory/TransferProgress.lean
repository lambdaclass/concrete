import Concrete.Memory.Execution

namespace Concrete.Interfaces.Memory

open ConcreteSpec.Core ConcreteSpec.StageInterface

variable {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty} {profile : Profile}

/-- Forward step interface for a running checkpoint whose sole allocation
obligation is a single `ParamOutcome` transfer. Covers jumps, branches, calls,
non-entry returns and destructuring in both Memory and register (SSA/BIR)
execution. -/
structure ParamForward (memory : Memory lang profile) (startDefinitions : Bool)
    (q arrive : MemoryRegisters language profile)
    (ps : List (Sigma (MemoryPlace language.operands.Ty)))
    (vs : List language.operands.Val)
    (finish : MemoryRegisters language profile → MemoryRegisters language profile := id) : Prop where
  running : q.resourceExhausted = false
  ready : memory.terminalObservation q = none
  arriveEffects : arrive.effects = q.effects
  arriveRunning : arrive.resourceExhausted = false
  finishFrame : ∀ s, (finish s).effects = s.effects ∧ (finish s).resourceExhausted = s.resourceExhausted
  step_of_outcome : ∀ stored mode, memory.ParamOutcome ps vs arrive stored mode →
    memory.StepOutcome q (finish stored) mode startDefinitions

/-- Full checkpoint interface adding step inversion back to the unique
`ParamOutcome` transfer. -/
structure ParamCheckpoint (memory : Memory lang profile) (startDefinitions : Bool)
    (q arrive : MemoryRegisters language profile)
    (ps : List (Sigma (MemoryPlace language.operands.Ty)))
    (vs : List language.operands.Val)
    (finish : MemoryRegisters language profile → MemoryRegisters language profile := id) : Prop
    extends ParamForward memory startDefinitions q arrive ps vs finish where
  outcome_of_step : ∀ next mode, memory.StepOutcome q next mode startDefinitions →
    ∃ stored, memory.ParamOutcome ps vs arrive stored mode ∧ next = finish stored

theorem ParamForward.step_exhausted_of_no_space {memory : Memory lang profile} (wf : memory.LayoutsWF)
    {startDefinitions q arrive ps vs finish}
    (cp : memory.ParamForward (language := language) startDefinitions q arrive ps vs finish) {c}
    (valid : memory.ParamsValid ps vs arrive c) (unavailable : ¬ memory.ParamSpaceFits ps arrive) :
    memory.Step q {q with resourceExhausted := true} startDefinitions := by
  simp only [Step, cp.running, Bool.false_eq_true, ite_false, ite_true, true_and]
  exact ⟨finish arrive, cp.step_of_outcome arrive true ((valid.at_bound wf).exhausted_of_no_space unavailable)⟩

theorem ParamForward.exhausted_of_no_space {memory : Memory lang profile} (wf : memory.LayoutsWF)
    {q arrive ps vs finish}
    (cp : memory.ParamForward (language := language) false q arrive ps vs finish) {c}
    (valid : memory.ParamsValid ps vs arrive c) (unavailable : ¬ memory.ParamSpaceFits ps arrive) :
    Execution.step memory q {q with resourceExhausted := true} ∧
      Execution.result memory {q with resourceExhausted := true} .resourceExhausted :=
  ⟨silent_step cp.ready (cp.step_exhausted_of_no_space wf valid unavailable) rfl,
    memory.exhausted_result rfl⟩

theorem ParamForward.step_normal_of_fit {memory : Memory lang profile} (wf : memory.LayoutsWF)
    {startDefinitions q arrive ps vs finish}
    (cp : memory.ParamForward (language := language) startDefinitions q arrive ps vs finish) {c}
    (valid : memory.ParamsValid ps vs arrive c) (fits : memory.ParamSpaceFits ps arrive) :
    ∃ next, memory.Step q next startDefinitions ∧
      next.effects = q.effects ∧ next.resourceExhausted = false := by
  obtain ⟨stored, writes, effects, flag⟩ := valid.physical wf fits
  have ff := cp.finishFrame stored
  have normal : (finish stored).resourceExhausted = false := (ff.2.trans flag).trans cp.arriveRunning
  refine ⟨finish stored, ?_, (ff.1.trans effects).trans cp.arriveEffects, normal⟩
  simp only [Step, cp.running, normal, Bool.false_eq_true, ite_false]
  exact cp.step_of_outcome stored false writes

theorem ParamForward.normal_of_fit {memory : Memory lang profile} (wf : memory.LayoutsWF)
    {q arrive ps vs finish}
    (cp : memory.ParamForward (language := language) false q arrive ps vs finish) {c}
    (valid : memory.ParamsValid ps vs arrive c) (fits : memory.ParamSpaceFits ps arrive) :
    ∃ next, Execution.step memory q next ∧ next.resourceExhausted = false := by
  obtain ⟨next, step, effects, normal⟩ := cp.step_normal_of_fit wf valid fits
  exact ⟨next, silent_step cp.ready step effects, normal⟩

theorem ParamForward.step_progress {memory : Memory lang profile} (wf : memory.LayoutsWF)
    {startDefinitions q arrive ps vs finish}
    (cp : memory.ParamForward (language := language) startDefinitions q arrive ps vs finish) {c}
    (valid : memory.ParamsValid ps vs arrive c) :
    ∃ next, memory.Step q next startDefinitions := by
  by_cases fits : memory.ParamSpaceFits ps arrive
  · obtain ⟨next, step, _⟩ := cp.step_normal_of_fit wf valid fits
    exact ⟨next, step⟩
  · exact ⟨_, cp.step_exhausted_of_no_space wf valid fits⟩

theorem ParamForward.progress {memory : Memory lang profile} (wf : memory.LayoutsWF)
    {q arrive ps vs finish}
    (cp : memory.ParamForward (language := language) false q arrive ps vs finish) {c}
    (valid : memory.ParamsValid ps vs arrive c) :
    ∃ next, Execution.step memory q next := by
  by_cases fits : memory.ParamSpaceFits ps arrive
  · obtain ⟨next, step, _⟩ := cp.normal_of_fit wf valid fits
    exact ⟨next, step⟩
  · exact ⟨_, (cp.exhausted_of_no_space wf valid fits).1⟩

theorem ParamCheckpoint.step_exhausted_iff {memory : Memory lang profile} (wf : memory.LayoutsWF)
    {startDefinitions q arrive ps vs finish}
    (cp : memory.ParamCheckpoint (language := language) startDefinitions q arrive ps vs finish) :
    memory.Step q {q with resourceExhausted := true} startDefinitions ↔
      (∃ ceiling, memory.ParamsValid ps vs arrive ceiling) ∧ ¬ memory.ParamSpaceFits ps arrive := by
  constructor
  · intro step
    simp only [Step, cp.running, Bool.false_eq_true, ite_false, ite_true] at step
    obtain ⟨_, partialState, raw⟩ := step
    obtain ⟨stored, outcome, _⟩ := cp.outcome_of_step partialState true raw
    exact (ParamOutcome.exhausted_iff wf).mp ⟨stored, outcome⟩
  · rintro ⟨⟨_, valid⟩, absent⟩
    exact cp.step_exhausted_of_no_space wf valid absent

theorem ParamCheckpoint.exhausted_iff {memory : Memory lang profile} (wf : memory.LayoutsWF)
    {q arrive ps vs finish}
    (cp : memory.ParamCheckpoint (language := language) false q arrive ps vs finish) :
    (Execution.step memory q {q with resourceExhausted := true} ∧
      Execution.result memory {q with resourceExhausted := true} .resourceExhausted) ↔
      (∃ ceiling, memory.ParamsValid ps vs arrive ceiling) ∧ ¬ memory.ParamSpaceFits ps arrive := by
  constructor
  · rintro ⟨tick, _⟩
    exact (cp.step_exhausted_iff wf).mp tick.2.1
  · rintro ⟨⟨_, valid⟩, absent⟩
    exact cp.exhausted_of_no_space wf valid absent

theorem ParamCheckpoint.step_normal_iff_fit {memory : Memory lang profile} (wf : memory.LayoutsWF)
    {startDefinitions q arrive ps vs finish}
    (cp : memory.ParamCheckpoint (language := language) startDefinitions q arrive ps vs finish) {c}
    (valid : memory.ParamsValid ps vs arrive c) :
    (∃ next, memory.Step q next startDefinitions ∧ next.resourceExhausted = false) ↔
      memory.ParamSpaceFits ps arrive := by
  constructor
  · rintro ⟨next, raw, normal⟩
    simp only [Step, cp.running, normal, Bool.false_eq_true, ite_false] at raw
    obtain ⟨stored, writes, _⟩ := cp.outcome_of_step next false raw
    exact ParamsValid.spaceFits wf ⟨valid.1.atCeiling (Nat.min_le_left ..), stored, writes⟩
  · intro fits
    obtain ⟨next, step, _, normal⟩ := cp.step_normal_of_fit wf valid fits
    exact ⟨next, step, normal⟩

theorem ParamCheckpoint.normal_iff_fit {memory : Memory lang profile} (wf : memory.LayoutsWF)
    {q arrive ps vs finish}
    (cp : memory.ParamCheckpoint (language := language) false q arrive ps vs finish) {c}
    (valid : memory.ParamsValid ps vs arrive c) :
    (∃ next, Execution.step memory q next ∧ next.resourceExhausted = false) ↔
      memory.ParamSpaceFits ps arrive := by
  refine ⟨?_, cp.normal_of_fit wf valid⟩
  rintro ⟨next, tick, normal⟩
  exact (cp.step_normal_iff_fit wf valid).mp ⟨next, tick.2.1, normal⟩

theorem JumpAt.checkpoint {memory : Memory lang profile} {q read dst edge vs}
    (jump : memory.JumpAt (language := language) q read dst edge vs) (startDefinitions : Bool := false) :
    memory.ParamCheckpoint startDefinitions q {read with block := edge.target, instruction := 0}
      dst.params vs where
  running := jump.running
  ready := by
    obtain ⟨entry, lookup, pc, ending⟩ := jump.source
    have endPC : entry.body[entry.body.length]? = none := List.getElem?_eq_none (Nat.le_refl _)
    unfold terminalObservation
    simp only [jump.running, Bool.false_eq_true, ite_false, lookup, Option.bind_eq_bind,
      Option.bind_some, pc, endPC, bne_self_eq_false, ending]
  arriveEffects := (readOperands_frame jump.args).effects
  arriveRunning := (readOperands_frame jump.args).resourceExhausted.trans jump.running
  finishFrame _ := ⟨rfl, rfl⟩
  step_of_outcome stored mode outcome := by
    obtain ⟨entry, lookup, pc, ending⟩ := jump.source
    have endPC : entry.body[q.instruction]? = none := List.getElem?_eq_none (Nat.le_of_eq pc.symm)
    refine ⟨entry, lookup, ?_⟩
    simp only [endPC, ending]
    exact ⟨pc, vs, read, dst, jump.args, jump.target, jump.types, outcome⟩
  outcome_of_step next mode raw := by
    obtain ⟨entry, lookup, pc, ending⟩ := jump.source
    have transfer := raw.terminatorAt lookup (List.getElem?_eq_none (Nat.le_of_eq pc.symm))
    rw [ending] at transfer
    obtain ⟨_, _, _, got, found, _, outcome⟩ := transfer
    cases Option.some.inj (jump.args.symm.trans got)
    cases Option.some.inj (jump.target.symm.trans found)
    exact ⟨next, outcome, rfl⟩

/-- A running branch after fetch, condition evaluation, taken-edge argument
evaluation and target signature checking. -/
structure BranchAt (memory : Memory lang profile) (q read : MemoryRegisters language profile)
    (dst : MemoryBlock lang) (test : MemoryRead _ lang.literal lang.bool)
    (yes no : CFGEdge (Sigma (MemoryRead _ lang.literal))) (flag : Bool)
    (values : List language.operands.Val) : Prop where
  running : q.resourceExhausted = false
  source : ∃ entry, memory.blockAt q = some entry ∧ q.instruction = entry.body.length ∧
    entry.ending = .branch test yes no
  cond : ∃ value afterTest, memory.readValue q test = some (value, afterTest) ∧
    language.operands.test value = some flag ∧
    memory.readOperands (if flag then yes else no).arguments afterTest = some (values, read)
  target : memory.blockAt {q with block := (if flag then yes else no).target} = some dst
  types : (if flag then yes else no).arguments.map Sigma.fst = dst.params.map Sigma.fst

def BranchAt.arrive (read : MemoryRegisters language profile)
    (yes no : CFGEdge (Sigma (MemoryRead _ lang.literal))) (flag : Bool) :
    MemoryRegisters language profile :=
  {read with block := (if flag then yes else no).target, instruction := 0}

theorem BranchAt.checkpoint {memory : Memory lang profile} {q read dst test yes no flag vs}
    (branch : memory.BranchAt (language := language) q read dst test yes no flag vs)
    (startDefinitions : Bool := false) :
    memory.ParamCheckpoint startDefinitions q (BranchAt.arrive read yes no flag) dst.params vs where
  running := branch.running
  ready := by
    obtain ⟨entry, lookup, pc, ending⟩ := branch.source
    have endPC : entry.body[entry.body.length]? = none := List.getElem?_eq_none (Nat.le_refl _)
    unfold terminalObservation
    simp only [branch.running, Bool.false_eq_true, ite_false, lookup, Option.bind_eq_bind,
      Option.bind_some, pc, endPC, bne_self_eq_false, ending]
  arriveEffects := by
    obtain ⟨_, _, readTest, _, readArgs⟩ := branch.cond
    exact (readOperands_frame readArgs).effects.trans (readValue_frame readTest).effects
  arriveRunning := by
    obtain ⟨_, _, readTest, _, readArgs⟩ := branch.cond
    exact ((readOperands_frame readArgs).resourceExhausted.trans
      (readValue_frame readTest).resourceExhausted).trans branch.running
  finishFrame _ := ⟨rfl, rfl⟩
  step_of_outcome stored mode outcome := by
    obtain ⟨entry, lookup, pc, ending⟩ := branch.source
    obtain ⟨val, afterTest, readTest, tested, readArgs⟩ := branch.cond
    have endPC : entry.body[q.instruction]? = none := List.getElem?_eq_none (Nat.le_of_eq pc.symm)
    have sameTarget : memory.blockAt {afterTest with block := (if flag then yes else no).target} = some dst := by
      simpa only [blockAt, (readValue_frame readTest).frame] using branch.target
    refine ⟨entry, lookup, ?_⟩
    simp only [endPC, ending]
    exact ⟨pc, val, afterTest, flag, readTest, tested, vs, read, dst, readArgs, sameTarget, branch.types, outcome⟩
  outcome_of_step next mode raw := by
    obtain ⟨entry, lookup, pc, ending⟩ := branch.source
    obtain ⟨val, afterTest, readTest, tested, readArgs⟩ := branch.cond
    have ctrl := raw.terminatorAt lookup (List.getElem?_eq_none (Nat.le_of_eq pc.symm))
    rw [ending] at ctrl
    obtain ⟨val', afterTest', flag', readTest', tested', vs', read', dst', readArgs', target', _, outcome⟩ := ctrl
    cases Option.some.inj (readTest.symm.trans readTest')
    cases Option.some.inj (tested.symm.trans tested')
    cases Option.some.inj (readArgs.symm.trans readArgs')
    have sameTarget : memory.blockAt {afterTest with block := (if flag then yes else no).target} = some dst := by
      simpa only [blockAt, (readValue_frame readTest).frame] using branch.target
    cases Option.some.inj (sameTarget.symm.trans target')
    exact ⟨next, outcome, rfl⟩

theorem BranchAt.exhausted_of_no_space {memory : Memory lang profile} (wf : memory.LayoutsWF)
    {q read dst test yes no flag vs}
    (branch : memory.BranchAt (language := language) q read dst test yes no flag vs) {c}
    (valid : memory.ParamsValid dst.params vs (BranchAt.arrive read yes no flag) c)
    (unavailable : ¬ memory.ParamSpaceFits dst.params (BranchAt.arrive read yes no flag)) :
    Execution.step memory q {q with resourceExhausted := true} ∧
      Execution.result memory {q with resourceExhausted := true} .resourceExhausted :=
  (branch.checkpoint false).exhausted_of_no_space wf valid unavailable

theorem BranchAt.exhausted_iff {memory : Memory lang profile} (wf : memory.LayoutsWF)
    {q read dst test yes no flag vs}
    (branch : memory.BranchAt (language := language) q read dst test yes no flag vs) :
    let arrive := BranchAt.arrive read yes no flag
    (Execution.step memory q {q with resourceExhausted := true} ∧
      Execution.result memory {q with resourceExhausted := true} .resourceExhausted) ↔
      (∃ ceiling, memory.ParamsValid dst.params vs arrive ceiling) ∧ ¬ memory.ParamSpaceFits dst.params arrive :=
  (branch.checkpoint false).exhausted_iff wf

theorem BranchAt.normal_of_fit {memory : Memory lang profile} (wf : memory.LayoutsWF)
    {q read dst test yes no flag vs}
    (branch : memory.BranchAt (language := language) q read dst test yes no flag vs) {c}
    (valid : memory.ParamsValid dst.params vs (BranchAt.arrive read yes no flag) c)
    (fits : memory.ParamSpaceFits dst.params (BranchAt.arrive read yes no flag)) :
    ∃ next, Execution.step memory q next ∧ next.resourceExhausted = false :=
  (branch.checkpoint false).normal_of_fit wf valid fits

theorem BranchAt.normal_iff_fit {memory : Memory lang profile} (wf : memory.LayoutsWF)
    {q read dst test yes no flag vs}
    (branch : memory.BranchAt (language := language) q read dst test yes no flag vs) {c}
    (valid : memory.ParamsValid dst.params vs (BranchAt.arrive read yes no flag) c) :
    (∃ next, Execution.step memory q next ∧ next.resourceExhausted = false) ↔
      memory.ParamSpaceFits dst.params (BranchAt.arrive read yes no flag) :=
  (branch.checkpoint false).normal_iff_fit wf valid

theorem BranchAt.progress {memory : Memory lang profile} (wf : memory.LayoutsWF)
    {q read dst test yes no flag vs}
    (branch : memory.BranchAt (language := language) q read dst test yes no flag vs) {c}
    (valid : memory.ParamsValid dst.params vs (BranchAt.arrive read yes no flag) c) :
    ∃ next, Execution.step memory q next :=
  (branch.checkpoint false).progress wf valid

/-- A running call after fetch, residual-trap clearance, argument evaluation
and callee entry resolution. -/
structure CallAt (memory : Memory lang profile) (q read : MemoryRegisters language profile)
    (callee : FnId) (dst : Option (Sigma (MemoryPlace language.operands.Ty)))
    (args : List (Sigma (MemoryRead _ lang.literal))) (checks : List OperationCheck)
    (calleeEntry : MemoryBlock lang) (values : List language.operands.Val) : Prop where
  running : q.resourceExhausted = false
  source : ∃ block op, memory.blockAt q = some block ∧ block.body[q.instruction]? = some op ∧
    op.instruction = .call callee dst args checks
  evalArgs : memory.readOperands args q = some (values, read)
  noTrap : firstTrap (.call callee) values checks = none
  target : ∃ graph, memory.graphAt callee = some graph ∧
    graph.blocks.find? (·.id == graph.entry) = some calleeEntry
  types : args.map Sigma.fst = calleeEntry.params.map Sigma.fst

def CallAt.arrive (callee : FnId) (entryId : BlockId)
    (dst : Option (Sigma (MemoryPlace language.operands.Ty)))
    (read : MemoryRegisters language profile) : MemoryRegisters language profile :=
  { toMemoryFrame := ⟨callee, entryId, 0, [], [], [], [], [], [], []⟩
    bytes := read.bytes
    ownership := read.ownership
    nextAddressId := read.nextAddressId
    effects := read.effects
    callers := ⟨read.advance.toMemoryFrame, dst⟩ :: read.callers }

theorem CallAt.checkpoint {memory : Memory lang profile} {q read callee dst args checks calleeEntry vs}
    (call : memory.CallAt (language := language) q read callee dst args checks calleeEntry vs)
    (startDefinitions : Bool := false) :
    memory.ParamCheckpoint startDefinitions q (CallAt.arrive callee calleeEntry.id dst read)
      calleeEntry.params vs where
  running := call.running
  ready := by
    obtain ⟨block, op, lookup, pc, inst⟩ := call.source
    have noInstTrap : memory.instructionTrap q (.call callee dst args checks) = none := by
      simp only [instructionTrap, bind, Option.bind, call.evalArgs, call.noTrap]
    unfold terminalObservation
    simp only [call.running, Bool.false_eq_true, ite_false, lookup, Option.bind_eq_bind,
      Option.bind_some, pc, inst, noInstTrap, Option.bind_none]
  arriveEffects := (readOperands_frame call.evalArgs).effects
  arriveRunning := rfl
  finishFrame _ := ⟨rfl, rfl⟩
  step_of_outcome stored mode outcome := by
    obtain ⟨block, op, lookup, pc, inst⟩ := call.source
    obtain ⟨graph, hgraph, hentry⟩ := call.target
    have hbeq := Array.find?_some hentry
    have idEq : calleeEntry.id = graph.entry := beq_iff_eq.mp hbeq
    have noInstTrap : memory.instructionTrap q (.call callee dst args checks) = none := by
      simp only [instructionTrap, bind, Option.bind, call.evalArgs, call.noTrap]
    refine ⟨block, lookup, ?_⟩
    rw [pc]
    dsimp only
    rw [inst]
    refine ⟨noInstTrap, vs, read, graph, calleeEntry, call.evalArgs, hgraph, hentry, call.types, ?_⟩
    simpa only [CallAt.arrive, idEq, id] using outcome
  outcome_of_step next mode raw := by
    obtain ⟨block, op, lookup, pc, inst⟩ := call.source
    obtain ⟨graph, hgraph, hentry⟩ := call.target
    have hbeq := Array.find?_some hentry
    have idEq : calleeEntry.id = graph.entry := beq_iff_eq.mp hbeq
    have step := raw.instructionAtWith lookup pc
    rw [inst] at step
    obtain ⟨_, vs', read', graph', entry', hargs', hgraph', hentry', _, outcome⟩ := step
    cases Option.some.inj (call.evalArgs.symm.trans hargs')
    cases Option.some.inj (hgraph.symm.trans hgraph')
    cases Option.some.inj (hentry.symm.trans hentry')
    refine ⟨next, ?_, rfl⟩
    simpa only [CallAt.arrive, idEq, id] using outcome

theorem CallAt.exhausted_of_no_space {memory : Memory lang profile} (wf : memory.LayoutsWF)
    {q read callee dst args checks calleeEntry vs}
    (call : memory.CallAt (language := language) q read callee dst args checks calleeEntry vs) {c}
    (valid : memory.ParamsValid calleeEntry.params vs (CallAt.arrive callee calleeEntry.id dst read) c)
    (unavailable : ¬ memory.ParamSpaceFits calleeEntry.params (CallAt.arrive callee calleeEntry.id dst read)) :
    Execution.step memory q {q with resourceExhausted := true} ∧
      Execution.result memory {q with resourceExhausted := true} .resourceExhausted :=
  (call.checkpoint false).exhausted_of_no_space wf valid unavailable

theorem CallAt.exhausted_iff {memory : Memory lang profile} (wf : memory.LayoutsWF)
    {q read callee dst args checks calleeEntry vs}
    (call : memory.CallAt (language := language) q read callee dst args checks calleeEntry vs) :
    let arrive := CallAt.arrive callee calleeEntry.id dst read
    (Execution.step memory q {q with resourceExhausted := true} ∧
      Execution.result memory {q with resourceExhausted := true} .resourceExhausted) ↔
      (∃ ceiling, memory.ParamsValid calleeEntry.params vs arrive ceiling) ∧
        ¬ memory.ParamSpaceFits calleeEntry.params arrive :=
  (call.checkpoint false).exhausted_iff wf

theorem CallAt.normal_of_fit {memory : Memory lang profile} (wf : memory.LayoutsWF)
    {q read callee dst args checks calleeEntry vs}
    (call : memory.CallAt (language := language) q read callee dst args checks calleeEntry vs) {c}
    (valid : memory.ParamsValid calleeEntry.params vs (CallAt.arrive callee calleeEntry.id dst read) c)
    (fits : memory.ParamSpaceFits calleeEntry.params (CallAt.arrive callee calleeEntry.id dst read)) :
    ∃ next, Execution.step memory q next ∧ next.resourceExhausted = false :=
  (call.checkpoint false).normal_of_fit wf valid fits

theorem CallAt.normal_iff_fit {memory : Memory lang profile} (wf : memory.LayoutsWF)
    {q read callee dst args checks calleeEntry vs}
    (call : memory.CallAt (language := language) q read callee dst args checks calleeEntry vs) {c}
    (valid : memory.ParamsValid calleeEntry.params vs (CallAt.arrive callee calleeEntry.id dst read) c) :
    (∃ next, Execution.step memory q next ∧ next.resourceExhausted = false) ↔
      memory.ParamSpaceFits calleeEntry.params (CallAt.arrive callee calleeEntry.id dst read) :=
  (call.checkpoint false).normal_iff_fit wf valid

theorem CallAt.progress {memory : Memory lang profile} (wf : memory.LayoutsWF)
    {q read callee dst args checks calleeEntry vs}
    (call : memory.CallAt (language := language) q read callee dst args checks calleeEntry vs) {c}
    (valid : memory.ParamsValid calleeEntry.params vs (CallAt.arrive callee calleeEntry.id dst read) c) :
    ∃ next, Execution.step memory q next :=
  (call.checkpoint false).progress wf valid

/-- A running non-entry return after fetch, operand evaluation, callee lifetime
release and destination signature checking. -/
structure ReturnAt (memory : Memory lang profile) (q read ended : MemoryRegisters language profile)
    (operand : Option (Sigma (MemoryRead _ lang.literal)))
    (caller : MemoryContinuation language.operands.Ty language.operands.Val)
    (rest : List (MemoryContinuation language.operands.Ty language.operands.Val))
    (values : List language.operands.Val) : Prop where
  running : q.resourceExhausted = false
  source : ∃ block, memory.blockAt q = some block ∧ q.instruction = block.body.length ∧
    block.ending = .ret operand
  callers : q.callers = caller :: rest
  args : memory.readOperands operand.toList q = some (values, read)
  release : memory.EndLocals read.live read ended
  types : operand.toList.map Sigma.fst = caller.destination.toList.map Sigma.fst

def ReturnAt.resume (caller : MemoryContinuation language.operands.Ty language.operands.Val)
    (rest : List (MemoryContinuation language.operands.Ty language.operands.Val))
    (ended : MemoryRegisters language profile) : MemoryRegisters language profile :=
  { toMemoryFrame := caller.caller
    bytes := ended.bytes
    callers := rest
    effects := ended.effects
    ownership := ended.ownership
    nextAddressId := ended.nextAddressId }

theorem ReturnAt.forward {memory : Memory lang profile} {q read ended operand caller rest vs}
    (ret : memory.ReturnAt (language := language) q read ended operand caller rest vs)
    (startDefinitions : Bool := false) :
    memory.ParamForward startDefinitions q (ReturnAt.resume caller rest ended)
      caller.destination.toList vs where
  running := ret.running
  ready := by
    obtain ⟨block, lookup, pc, ending⟩ := ret.source
    have endPC : block.body[block.body.length]? = none := List.getElem?_eq_none (Nat.le_refl _)
    unfold terminalObservation
    cases operand with
    | none =>
      simp only [ret.running, Bool.false_eq_true, ite_false, lookup, Option.bind_eq_bind,
        Option.bind_some, pc, endPC, bne_self_eq_false, ending]
    | some pair =>
      obtain ⟨_, op⟩ := pair
      simp only [ret.running, Bool.false_eq_true, ite_false, lookup, Option.bind_eq_bind,
        Option.bind_some, pc, endPC, bne_self_eq_false, ending, ret.callers, List.isEmpty_cons,
        Bool.not_false, Bool.true_or, ite_true]
  arriveEffects := ret.release.frame.2.2.2.2.1.trans (readOperands_frame ret.args).effects
  arriveRunning := rfl
  finishFrame _ := ⟨rfl, rfl⟩
  step_of_outcome stored mode outcome := by
    obtain ⟨block, lookup, pc, ending⟩ := ret.source
    have endPC : block.body[q.instruction]? = none := List.getElem?_eq_none (Nat.le_of_eq pc.symm)
    refine ⟨block, lookup, ?_⟩
    simp only [endPC, ending]
    exact ⟨pc, caller, rest, vs, read, ended, ret.callers, ret.args, ret.release, ret.types, outcome⟩

theorem ReturnAt.checkpoint {memory : Memory lang profile} {q read ended operand caller rest vs}
    (ret : memory.ReturnAt (language := language) q read ended operand caller rest vs)
    (unbacked : memory.ZeroUnbacked q) (startDefinitions : Bool := false) :
    memory.ParamCheckpoint startDefinitions q (ReturnAt.resume caller rest ended)
      caller.destination.toList vs where
  toParamForward := ret.forward startDefinitions
  outcome_of_step next mode raw := by
    obtain ⟨block, lookup, pc, ending⟩ := ret.source
    have ctrl := raw.terminatorAt lookup (List.getElem?_eq_none (Nat.le_of_eq pc.symm))
    rw [ending] at ctrl
    obtain ⟨caller', rest', vs', read', ended', hcallers', hargs', hrel', _, outcome⟩ := ctrl
    obtain ⟨rfl, rfl⟩ := List.cons.inj (ret.callers.symm.trans hcallers')
    cases Option.some.inj (ret.args.symm.trans hargs')
    have rf := readOperands_frame ret.args
    have unbackedRead : memory.ZeroUnbacked read := by
      intro p l hl hz
      rw [rf.frame] at hl
      rw [rf.materialized]
      exact unbacked p l hl hz
    obtain rfl := ret.release.unique unbackedRead hrel'
    exact ⟨next, outcome, rfl⟩

theorem ReturnAt.exhausted_of_no_space {memory : Memory lang profile} (wf : memory.LayoutsWF)
    {q read ended operand caller rest vs}
    (ret : memory.ReturnAt (language := language) q read ended operand caller rest vs) {c}
    (valid : memory.ParamsValid caller.destination.toList vs (ReturnAt.resume caller rest ended) c)
    (unavailable : ¬ memory.ParamSpaceFits caller.destination.toList (ReturnAt.resume caller rest ended)) :
    Execution.step memory q {q with resourceExhausted := true} ∧
      Execution.result memory {q with resourceExhausted := true} .resourceExhausted :=
  (ret.forward false).exhausted_of_no_space wf valid unavailable

theorem ReturnAt.exhausted_iff {memory : Memory lang profile} (wf : memory.LayoutsWF)
    {q read ended operand caller rest vs}
    (ret : memory.ReturnAt (language := language) q read ended operand caller rest vs)
    (unbacked : memory.ZeroUnbacked q) :
    let resume := ReturnAt.resume caller rest ended
    (Execution.step memory q {q with resourceExhausted := true} ∧
      Execution.result memory {q with resourceExhausted := true} .resourceExhausted) ↔
      (∃ ceiling, memory.ParamsValid caller.destination.toList vs resume ceiling) ∧
        ¬ memory.ParamSpaceFits caller.destination.toList resume :=
  (ret.checkpoint unbacked false).exhausted_iff wf

theorem ReturnAt.normal_of_fit {memory : Memory lang profile} (wf : memory.LayoutsWF)
    {q read ended operand caller rest vs}
    (ret : memory.ReturnAt (language := language) q read ended operand caller rest vs) {c}
    (valid : memory.ParamsValid caller.destination.toList vs (ReturnAt.resume caller rest ended) c)
    (fits : memory.ParamSpaceFits caller.destination.toList (ReturnAt.resume caller rest ended)) :
    ∃ next, Execution.step memory q next ∧ next.resourceExhausted = false :=
  (ret.forward false).normal_of_fit wf valid fits

theorem ReturnAt.normal_iff_fit {memory : Memory lang profile} (wf : memory.LayoutsWF)
    {q read ended operand caller rest vs}
    (ret : memory.ReturnAt (language := language) q read ended operand caller rest vs)
    (unbacked : memory.ZeroUnbacked q) {c}
    (valid : memory.ParamsValid caller.destination.toList vs (ReturnAt.resume caller rest ended) c) :
    (∃ next, Execution.step memory q next ∧ next.resourceExhausted = false) ↔
      memory.ParamSpaceFits caller.destination.toList (ReturnAt.resume caller rest ended) :=
  (ret.checkpoint unbacked false).normal_iff_fit wf valid

theorem ReturnAt.progress {memory : Memory lang profile} (wf : memory.LayoutsWF)
    {q read ended operand caller rest vs}
    (ret : memory.ReturnAt (language := language) q read ended operand caller rest vs) {c}
    (valid : memory.ParamsValid caller.destination.toList vs (ReturnAt.resume caller rest ended) c) :
    ∃ next, Execution.step memory q next :=
  (ret.forward false).progress wf valid

/-- A running destructure after fetch, source read, active-field validation
and field extraction. -/
structure DestructureAt (memory : Memory lang profile) (q read : MemoryRegisters language profile)
    {ty : language.operands.Ty} (src : MemoryPlace _ ty)
    (bindings : List (FieldId × Sigma (MemoryPlace language.operands.Ty)))
    (fields : List language.operands.Val) : Prop where
  running : q.resourceExhausted = false
  source : ∃ block op, memory.blockAt q = some block ∧ block.body[q.instruction]? = some op ∧
    op.instruction = .destructure src bindings
  readSrc : ∃ value,
    memory.readValue q (if language.operands.isCopy ty then .copy src else .move src) = some (value, read) ∧
    DestructureFields language memory.declarations ty value (bindings.map fun (field, dst) => (field, dst.1)) ∧
    bindings.mapM (fun binding => language.field value [binding.1]) = some fields

theorem DestructureAt.checkpoint {memory : Memory lang profile} {q read ty} {src : MemoryPlace _ ty}
    {bindings fields}
    (destr : memory.DestructureAt (language := language) q read src bindings fields)
    (startDefinitions : Bool := false) :
    memory.ParamCheckpoint startDefinitions q read (bindings.map Prod.snd) fields
      MemoryRegisters.advance where
  running := destr.running
  ready := by
    obtain ⟨block, op, lookup, pc, inst⟩ := destr.source
    unfold terminalObservation
    simp only [destr.running, Bool.false_eq_true, ite_false, lookup, Option.bind_eq_bind,
      Option.bind_some, pc, inst, instructionTrap, Option.bind_none]
  arriveEffects := by
    obtain ⟨_, hread, _, _⟩ := destr.readSrc
    exact (readValue_frame hread).effects
  arriveRunning := by
    obtain ⟨_, hread, _, _⟩ := destr.readSrc
    exact (readValue_frame hread).resourceExhausted.trans destr.running
  finishFrame _ := ⟨rfl, rfl⟩
  step_of_outcome stored mode outcome := by
    obtain ⟨block, op, lookup, pc, inst⟩ := destr.source
    obtain ⟨value, hread, hfields, hextract⟩ := destr.readSrc
    refine ⟨block, lookup, ?_⟩
    rw [pc]
    dsimp only
    rw [inst]
    exact ⟨value, read, fields, stored, hread, hfields, hextract, outcome, rfl⟩
  outcome_of_step next mode raw := by
    obtain ⟨block, op, lookup, pc, inst⟩ := destr.source
    obtain ⟨value, hread, _, hextract⟩ := destr.readSrc
    have step := raw.instructionAtWith lookup pc
    rw [inst] at step
    obtain ⟨value', read', fields', stored, hread', _, hextract', outcome, eq⟩ := step
    cases Option.some.inj (hread.symm.trans hread')
    cases Option.some.inj (hextract.symm.trans hextract')
    exact ⟨stored, outcome, eq⟩

theorem DestructureAt.exhausted_of_no_space {memory : Memory lang profile} (wf : memory.LayoutsWF)
    {q read ty} {src : MemoryPlace _ ty} {bindings fields}
    (destr : memory.DestructureAt (language := language) q read src bindings fields) {c}
    (valid : memory.ParamsValid (bindings.map Prod.snd) fields read c)
    (unavailable : ¬ memory.ParamSpaceFits (bindings.map Prod.snd) read) :
    Execution.step memory q {q with resourceExhausted := true} ∧
      Execution.result memory {q with resourceExhausted := true} .resourceExhausted :=
  (destr.checkpoint false).exhausted_of_no_space wf valid unavailable

theorem DestructureAt.exhausted_iff {memory : Memory lang profile} (wf : memory.LayoutsWF)
    {q read ty} {src : MemoryPlace _ ty} {bindings fields}
    (destr : memory.DestructureAt (language := language) q read src bindings fields) :
    (Execution.step memory q {q with resourceExhausted := true} ∧
      Execution.result memory {q with resourceExhausted := true} .resourceExhausted) ↔
      (∃ ceiling, memory.ParamsValid (bindings.map Prod.snd) fields read ceiling) ∧
        ¬ memory.ParamSpaceFits (bindings.map Prod.snd) read :=
  (destr.checkpoint false).exhausted_iff wf

theorem DestructureAt.normal_of_fit {memory : Memory lang profile} (wf : memory.LayoutsWF)
    {q read ty} {src : MemoryPlace _ ty} {bindings fields}
    (destr : memory.DestructureAt (language := language) q read src bindings fields) {c}
    (valid : memory.ParamsValid (bindings.map Prod.snd) fields read c)
    (fits : memory.ParamSpaceFits (bindings.map Prod.snd) read) :
    ∃ next, Execution.step memory q next ∧ next.resourceExhausted = false :=
  (destr.checkpoint false).normal_of_fit wf valid fits

theorem DestructureAt.normal_iff_fit {memory : Memory lang profile} (wf : memory.LayoutsWF)
    {q read ty} {src : MemoryPlace _ ty} {bindings fields}
    (destr : memory.DestructureAt (language := language) q read src bindings fields) {c}
    (valid : memory.ParamsValid (bindings.map Prod.snd) fields read c) :
    (∃ next, Execution.step memory q next ∧ next.resourceExhausted = false) ↔
      memory.ParamSpaceFits (bindings.map Prod.snd) read :=
  (destr.checkpoint false).normal_iff_fit wf valid

theorem DestructureAt.progress {memory : Memory lang profile} (wf : memory.LayoutsWF)
    {q read ty} {src : MemoryPlace _ ty} {bindings fields}
    (destr : memory.DestructureAt (language := language) q read src bindings fields) {c}
    (valid : memory.ParamsValid (bindings.map Prod.snd) fields read c) :
    ∃ next, Execution.step memory q next :=
  (destr.checkpoint false).progress wf valid

end Concrete.Interfaces.Memory
