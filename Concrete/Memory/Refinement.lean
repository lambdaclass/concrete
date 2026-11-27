import Concrete.Memory.Execution
import Concrete.Memory.Lifetimes

namespace Concrete.Memory

open Concrete.Interfaces ConcreteSpec.Core ConcreteSpec.StageInterface
open Concrete.Contracts (Steps Semantics TracePrefix SameInfiniteTrace)

variable {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty} {profile : Profile}

private abbrev Src := Interfaces.MIR.Execution (language := language) (lang := lang) (profile := profile)
private abbrev Dst := Interfaces.Memory.Execution (language := language) (lang := lang) (profile := profile)

private def isBegin {Ty : Type} {lowering : LoweringLanguage Ty} : MemoryInst lowering → Bool
  | .beginLifetime _ => true
  | _ => false

/-- Suspended PCs are return addresses. They use the same instruction map as
the running frame, including when the following source instruction has a prefix. -/
def FrameAligned (memory : Interfaces.Memory lang profile)
    (logical physical : MemoryFrame language.operands.Val) : Prop :=
  logical.frame = physical.frame ∧ logical.block = physical.block ∧
  ∃ graph block, memory.graphAt physical.frame = some graph ∧
    graph.blocks.find? (·.id == physical.block) = some block ∧
    physical.instruction ≤ block.body.length ∧
    logical.instruction = ((block.body.take physical.instruction).filter
      fun op => !isBegin op.instruction).length

theorem FrameAligned.unique_pc {memory : Interfaces.Memory lang profile} {a b q}
    (ha : FrameAligned (language := language) memory a q) (hb : FrameAligned memory b q) :
    a.instruction = b.instruction := by
  obtain ⟨_, _, g, block, graph, found, _, lhs⟩ := ha
  obtain ⟨_, _, g', block', graph', found', _, rhs⟩ := hb
  cases Option.some.inj (graph.symm.trans graph')
  cases Option.some.inj (found.symm.trans found')
  exact lhs.trans rhs.symm

private def logicalDestination (d : Option (Sigma (MemoryPlace language.operands.Ty))) :=
  d.map fun p => (⟨p.1, p.2.logical⟩ : Sigma (Place language.operands.Ty))

structure Related (memory : Interfaces.Memory lang profile)
    (p q : MemoryRegisters language profile) : Prop where
  representation : memory.RepresentsCheckpoint p q
  active : FrameAligned memory p.toMemoryFrame q.toMemoryFrame
  suspended : ∀ pair ∈ p.callers.zip q.callers,
    FrameAligned memory pair.1.caller pair.2.caller ∧
      logicalDestination pair.1.destination = logicalDestination pair.2.destination
  effects : p.effects = q.effects

private theorem load_after_begin (memory : Interfaces.Memory lang profile)
    (state : MemoryRegisters language profile) (newRoot : PlaceId) {ty : language.operands.Ty}
    (place : MemoryPlace _ ty) (live : place.logical.root ∈ state.live) :
    memory.loadValue {state with live := newRoot :: state.live} place = memory.loadValue state place := by
  unfold Interfaces.Memory.loadValue
  change (if Interfaces.Memory.initializedAt state place.logical.root place.logical.path then _ else none) =
    (if Interfaces.Memory.initializedAt state place.logical.root place.logical.path then _ else none)
  split
  · have old : state.live.contains place.logical.root := List.contains_iff_mem.mpr live
    have now : (newRoot :: state.live).contains place.logical.root := List.contains_iff_mem.mpr (.tail _ live)
    change (if !memory.validPlace state place then none else
      if !(newRoot :: state.live).contains place.logical.root then none else _) =
        (if !memory.validPlace state place then none else if !state.live.contains place.logical.root then none else _)
    rw [now, old]
    rfl
  · rfl

/-- Starting a declared virtual root does not invalidate the bytes, provenance,
values or permissions of any activation. This includes the checkpoint after
the extra physical step and before the source definition has executed. -/
theorem virtual_begin_represents {memory : Interfaces.Memory lang profile}
    {p q q' : MemoryRegisters language profile} {root locals graph ty}
    (related : memory.RepresentsCheckpoint p q)
    (declared : (q.frame, locals, graph) ∈ memory.functions)
    (typeOf : AList.lookup locals root = some ty)
    (virtual : memory.classAt q.frame root = some .virtual)
    (step : memory.InstructionStep q q' false (.beginLifetime root)) :
    memory.RepresentsCheckpoint p q' := by
  obtain ⟨started, begins, rfl⟩ := step
  obtain ⟨fresh, choices⟩ := begins
  rcases choices with ⟨_, rfl⟩ | ⟨materialized, _⟩
  case inr => rw [virtual] at materialized; contradiction
  obtain ⟨active, depth, suspended, address, uniqueIds, nextId, owners⟩ := related
  refine ⟨?_, depth, suspended, address, uniqueIds, nextId, owners⟩
  obtain ⟨⟨bytes, moved⟩, frame, lives, logicalKeys, rangeKeys, virtualKeys, zeroKeys, zero,
    initialized, valuesLive, virtualValues, materialized, places⟩ := active
  refine ⟨⟨⟨bytes.1, List.nodup_cons.mpr ⟨fresh, bytes.2.1⟩, bytes.2.2.1,
    fun s h => .tail _ (bytes.2.2.2 s h)⟩, moved⟩, frame, fun s h => .tail _ (lives s h),
    logicalKeys, rangeKeys, virtualKeys, zeroKeys, zero, initialized, valuesLive, virtualValues, ?_, ?_⟩
  · intro s range h
    obtain ⟨live, rest⟩ := materialized s range h
    exact ⟨.tail _ live, rest⟩
  · intro s member
    rcases List.mem_cons.mp member with rfl | live
    · refine ⟨locals, graph, ty, .virtual, declared, typeOf, virtual, nofun, ?_⟩
      intro value loaded
      exact (fresh (bytes.2.2.2 _ ((initialized _).mpr (Option.isSome_iff_exists.mpr ⟨_, loaded⟩)))).elim
    · obtain ⟨ls, g, t, cls, declared', lookup, classOf, backing, vals⟩ := places _ live
      refine ⟨ls, g, t, cls, declared', lookup, classOf, backing, ?_⟩
      intro value loaded
      have rep := vals value loaded
      change (if Interfaces.Memory.initializedAt q s [] then _ else _) at rep ⊢
      split at rep
      · rename_i init
        rw [ite_eq_left init]
        obtain ⟨v, shape, reads, runs⟩ := rep
        exact ⟨v, shape, (load_after_begin memory _ root _ live).trans reads, runs⟩
      · rename_i moved
        rw [ite_eq_right moved]
        intro path fieldTy field init typed proj
        obtain ⟨v, shape, reads, runs⟩ := rep path fieldTy field init typed proj
        exact ⟨v, shape, (load_after_begin memory _ root _ live).trans reads, runs⟩

/-- A begin advances within its current block, so the unexecuted suffix
strictly shrinks. Source-matched control transfers may reset this rank. -/
def remainingInstructions (memory : Interfaces.Memory lang profile) (q : MemoryRegisters language profile) : Nat :=
  ((memory.blockAt q).map fun block => block.body.length - q.instruction).getD 0

private theorem begins_frame {memory : Interfaces.Memory lang profile} {q q' : MemoryRegisters language profile}
    {root : PlaceId} (h : memory.Begins q root q') :
    q'.frame = q.frame ∧ q'.block = q.block ∧ q'.instruction = q.instruction := by
  rcases h.2 with ⟨_, rfl⟩ | ⟨_, _, _, ⟨_, rfl⟩ | ⟨_, _, _, _, _, _, rfl⟩⟩ <;> exact ⟨rfl, rfl, rfl⟩

theorem begin_decreases {memory : Interfaces.Memory lang profile} {q q' : MemoryRegisters language profile}
    {block op root} (atBlock : memory.blockAt q = some block)
    (atPC : block.body[q.instruction]? = some op) (kind : op.instruction = .beginLifetime root)
    (tick : memory.StepOutcome q q' false) : remainingInstructions memory q' < remainingInstructions memory q := by
  obtain ⟨b, found, inst⟩ := tick
  rw [atBlock] at found
  cases found
  simp only [atPC, kind] at inst
  obtain ⟨opened, begin, rfl⟩ := inst
  obtain ⟨frame, label, pc⟩ := begins_frame begin
  have same : memory.blockAt opened.advance = memory.blockAt q := by
    simp only [Interfaces.Memory.blockAt, MemoryRegisters.advance, frame, label]
  obtain ⟨within, _⟩ := List.getElem?_eq_some_iff.mp atPC
  unfold remainingInstructions
  rw [same, atBlock]
  change block.body.length - (opened.instruction + 1) < block.body.length - q.instruction
  rw [pc]
  omega

/-- Every fresh root with a storage class and its required layout has a
physical begin successor. Allocation choices range over the whole target. -/
theorem begin_progress {memory : Interfaces.Memory lang profile} {q : MemoryRegisters language profile}
    {block op root} (running : q.resourceExhausted = false)
    (found : memory.blockAt q = some block) (pc : block.body[q.instruction]? = some op)
    (inst : op.instruction = .beginLifetime root) (fresh : root ∉ q.live)
    (declared : memory.classAt q.frame root = some .virtual ∨
      (memory.classAt q.frame root = some .materialized ∧ ∃ layout, memory.layoutAt q.frame root = some layout)) :
    (∃ opened, memory.Begins q root opened ∧ Dst.step memory q opened.advance) ∨
      (Dst.step memory q {q with resourceExhausted := true} ∧
        Dst.result memory {q with resourceExhausted := true} .resourceExhausted) := by
  have ready : memory.terminalObservation q = none := by
    simp [Interfaces.Memory.terminalObservation, running, found, pc, inst, Interfaces.Memory.instructionTrap]
  by_cases space : ∃ opened, memory.Begins q root opened
  · obtain ⟨opened, began⟩ := space
    have flags : opened.resourceExhausted = false ∧ opened.effects = q.effects := by
      rcases began.2 with ⟨_, rfl⟩ | ⟨_, _, _, ⟨_, rfl⟩ | ⟨_, _, _, _, _, _, rfl⟩⟩ <;> exact ⟨running, rfl⟩
    refine .inl ⟨opened, began, Interfaces.Memory.silent_step ready ?_ flags.2⟩
    unfold Interfaces.Memory.Step
    have active : opened.advance.resourceExhausted = false := flags.1
    simp only [running, active, Bool.false_eq_true, ite_false]
    refine ⟨block, found, ?_⟩
    simp only [pc, inst]
    exact ⟨opened, began, rfl⟩
  · have failed : memory.BeginExhausted q root := by
      rcases declared with virtual | ⟨cls, layout, hl⟩
      · exact (space ⟨_, fresh, .inl ⟨virtual, rfl⟩⟩).elim
      · refine ⟨fresh, cls, ⟨layout, hl, Nat.pos_of_ne_zero (fun zero => ?_)⟩, space⟩
        exact space ⟨_, fresh, .inr ⟨cls, layout, hl, .inl ⟨zero, rfl⟩⟩⟩
    refine .inr ⟨Interfaces.Memory.silent_step ready ?_ rfl, memory.exhausted_result rfl⟩
    unfold Interfaces.Memory.Step
    simp only [running, Bool.false_eq_true, ite_false, ite_true, true_and]
    refine ⟨q, block, found, ?_⟩
    simp only [pc, inst]
    exact failed

/-- An allocation failure consumes the final stutter even inside an atomic
parameter transfer. Its terminal state retains the last committed checkpoint. -/
def stutterRank (memory : Interfaces.Memory lang profile) (q : MemoryRegisters language profile) : Nat :=
  if q.resourceExhausted then 0 else remainingInstructions memory q + 1

/-- The remaining ISS-0119 obligations are local to reachable executions of the
two production carriers. Rank decrease and infinite-run correspondence are consequences. -/
structure LocalCorrespondence (mir : MIR lang profile) (memory : Interfaces.Memory lang profile) : Prop where
  initial : ∀ q, Dst.initial memory q → ∃ p, Src.initial mir p ∧ Related memory p q
  advance : ∀ p q q', Src.finite mir p → Dst.finite memory q → Related memory p q → Dst.step memory q q' →
    q'.resourceExhausted = false →
    (∃ p', Src.step mir p p' ∧ Related memory p' q') ∨
      (Related memory p q' ∧ ∃ block op root, memory.blockAt q = some block ∧
        block.body[q.instruction]? = some op ∧ op.instruction = .beginLifetime root)
  finish : ∀ p q o, Src.finite mir p → Dst.finite memory q → Related memory p q →
    memory.MatchingResult q o → Src.result mir p o
  progress : ∀ p q, Src.finite mir p → Dst.finite memory q → Related memory p q →
    (∃ o, Dst.result memory q o) ∨ ∃ q', Dst.step memory q q'

private theorem LocalCorrespondence.matchStep {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    (corr : LocalCorrespondence (language := language) mir memory) {p q q'}
    (hp : Src.finite mir p) (hq : Dst.finite memory q) (r : Related memory p q) (tick : Dst.step memory q q') :
    (∃ p', Src.step mir p p' ∧ Related memory p' q') ∨
      (Related memory p q' ∧ stutterRank memory q' < stutterRank memory q) := by
  have running : q.resourceExhausted = false := by
    cases hf : q.resourceExhausted
    · rfl
    · have terminal : Dst.result memory q .resourceExhausted := memory.exhausted_result hf
      exact (Dst.terminal _ _ _ terminal _ tick).elim
  have raw : memory.Step q q' := tick.2.1
  unfold Interfaces.Memory.Step at raw
  rw [ite_eq_right (by simp [running])] at raw
  cases hf : q'.resourceExhausted with
  | true =>
    rw [ite_eq_left hf] at raw
    obtain ⟨rfl, _⟩ := raw
    exact .inr ⟨⟨r.representation, r.active, r.suspended, r.effects⟩, by simp [stutterRank, running]⟩
  | false =>
    rcases corr.advance _ _ _ hp hq r tick hf with matched | ⟨rel, _, _, _, hb, ho, hi⟩
    · exact .inl matched
    · rw [ite_eq_right (by simp [hf])] at raw
      exact .inr ⟨rel, by simpa only [stutterRank, hf, running, Bool.false_eq_true, ite_false,
        Nat.add_lt_add_iff_right] using begin_decreases hb ho hi raw⟩

theorem finite_invariant {A E : Type} (sem : Semantics A E) (program : A) (P : sem.State → Prop)
    (initial : ∀ q, sem.initial program q → P q)
    (advance : ∀ p q, P p → sem.step program p q → P q) {q} (finite : sem.finite program q) : P q := by
  obtain ⟨start, atStart, path⟩ := finite
  have follow {p q} (steps : Steps (sem.step program) p q) (h : P p) : P q := by
    induction steps with
    | nil => exact h
    | cons tick _ ih => exact ih (advance _ _ h tick)
  exact follow path (initial _ atStart)

theorem finite_next {A E : Type} {s : Semantics A E} {a : A} {p q : s.State}
    (reached : s.finite a p) (tick : s.step a p q) : s.finite a q := by
  obtain ⟨start, atEntry, path⟩ := reached
  refine ⟨start, atEntry, ?_⟩
  clear atEntry
  induction path with
  | nil => exact .cons tick (.nil _)
  | cons h _ ih => exact .cons h (ih tick)

private theorem run_finite {A E : Type} {s : Semantics A E} {a : A} {run}
    (infinite : s.diverges a run) (i : Nat) : s.finite a (run i) := by
  induction i with
  | zero => exact ⟨_, infinite.1, .nil _⟩
  | succ i ih => exact finite_next ih (infinite.2 i)

private theorem LocalCorrespondence.reached {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    (localSteps : LocalCorrespondence (language := language) mir memory) {q}
    (finite : Dst.finite memory q) : ∃ p, Src.finite mir p ∧ Related memory p q := by
  obtain ⟨start, initial, path⟩ := finite
  obtain ⟨p, atEntry, related⟩ := localSteps.initial _ initial
  have follow {a b} (steps : Steps (Dst.step memory) a b) :
      Dst.finite memory a → ∀ p, Src.finite mir p → Related memory p a →
        ∃ next, Src.finite mir next ∧ Related memory next b := by
    induction steps with
    | nil => exact fun _ p h r => ⟨p, h, r⟩
    | cons tick _ ih =>
      intro finite p hs r
      rcases localSteps.matchStep hs finite r tick with ⟨p', step, r'⟩ | ⟨r', _⟩
      · exact ih (finite_next finite tick) _ (finite_next hs step) r'
      · exact ih (finite_next finite tick) _ hs r'
  exact follow path ⟨start, initial, .nil _⟩ p ⟨p, atEntry, .nil _⟩ related

private theorem trace_at_result {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    {p q : MemoryRegisters language profile} {o} (r : Related memory p q)
    (hs : Src.result mir p o) (ht : Dst.result memory q o) :
    Src.trace mir p = Dst.trace memory q := by
  change (_ : Option Concrete.Contracts.Observation) = some o at hs ht
  change p.effects.map _ ++ (Option.map _ _).getD [] =
    q.effects.map _ ++ (Option.map _ _).getD []
  rw [hs, ht, r.effects]

private theorem trace_on_run {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    {p p' q q' : MemoryRegisters language profile} (r : Related memory p q)
    (hs : Src.step mir p p') (ht : Dst.step memory q q') :
    Src.trace mir p = Dst.trace memory q := by
  have noSource := hs.1
  have noTarget := ht.1
  change p.effects.map _ ++ (Option.map _ _).getD [] =
    q.effects.map _ ++ (Option.map _ _).getD []
  rw [noSource, noTarget, r.effects]

private theorem prefix_trans {α : Type} {xs ys zs : List α}
    (a : TracePrefix xs ys) (b : TracePrefix ys zs) : TracePrefix xs zs := by
  obtain ⟨left, rfl⟩ := a
  obtain ⟨right, rfl⟩ := b
  exact ⟨left ++ right, (List.append_assoc ..).symm⟩

private theorem run_trace_mono {A E : Type} {s : Semantics A E} {a : A} {run}
    (infinite : s.diverges a run) {i j : Nat} (order : i ≤ j) :
    TracePrefix (s.trace a (run i)) (s.trace a (run j)) := by
  induction order with
  | refl => exact ⟨[], List.append_nil _⟩
  | step _ ih => exact prefix_trans ih (s.traceGrows _ _ _ (infinite.2 _))

/-- Actual target times at which the chosen logical run has advanced. -/
private structure Checkpoint (mir : MIR lang profile) (memory : Interfaces.Memory lang profile)
    (run : Nat → MemoryRegisters language profile) where
  time : Nat
  logical : MemoryRegisters language profile
  finite : Src.finite mir logical
  related : Related memory logical (run time)

theorem LocalCorrespondence.divergence {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    (localSteps : LocalCorrespondence (language := language) mir memory) {run}
    (infinite : Dst.diverges memory run) :
    ∃ matching, Src.diverges mir matching ∧ SameInfiniteTrace
      (fun i => Src.trace mir (matching i)) (fun i => Dst.trace memory (run i)) := by
  let Point := Checkpoint (language := language) mir memory run
  obtain ⟨p, initial, related⟩ := localSteps.initial _ infinite.1
  let start : Point := ⟨0, p, ⟨p, initial, .nil _⟩, related⟩
  have advance (a : Point) : ∃ b : Point, a.time < b.time ∧ Src.step mir a.logical b.logical := by
    generalize rankEq : stutterRank memory (run a.time) = n
    induction n using Nat.strongRecOn generalizing a with
    | ind n ih =>
      have tick := infinite.2 a.time
      rcases localSteps.matchStep a.finite (run_finite infinite _) a.related tick with
        ⟨next, step, r⟩ | ⟨r, lower⟩
      · exact ⟨⟨a.time + 1, next, finite_next a.finite step, r⟩, Nat.lt_succ_self _, step⟩
      · let later : Point := ⟨a.time + 1, a.logical, a.finite, r⟩
        obtain ⟨b, lt, step⟩ := ih _ (rankEq ▸ lower) later rfl
        exact ⟨b, Nat.lt_trans (Nat.lt_succ_self _) lt, step⟩
  let points : Nat → Point := Nat.rec start (fun _ a => (advance a).choose)
  have moves (i : Nat) : (points i).time < (points (i+1)).time ∧
      Src.step mir (points i).logical (points (i+1)).logical := (advance (points i)).choose_spec
  have cofinal (i : Nat) : i ≤ (points i).time := by
    induction i with
    | zero => exact Nat.zero_le _
    | succ i ih => exact Nat.lt_of_le_of_lt ih (moves i).1
  have traces (i : Nat) : Src.trace mir (points i).logical = Dst.trace memory (run (points i).time) :=
    trace_on_run (points i).related (moves i).2 (infinite.2 _)
  refine ⟨fun i => (points i).logical, ⟨initial, fun i => (moves i).2⟩, ?_⟩
  intro events
  constructor
  · rintro ⟨i, h⟩
    exact ⟨(points i).time, by simpa only [← traces i] using h⟩
  · rintro ⟨i, h⟩
    exact ⟨i, by simpa only [traces i] using prefix_trans h (run_trace_mono infinite (cofinal i))⟩

/-- All dynamic premises refer to individual reachable steps or results. The
trace and divergence clauses follow for the exact programs that lowerGraph relates. -/
theorem lowerGraph_refines {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    (ok : lowerGraph (language := language) mir = .ok memory)
    (source : mir.logicalView.HandoffWF)
    (steps : ProgramSafe Src mir → LocalCorrespondence (language := language) mir memory) :
    LowersMemory (language := language) mir memory := by
  have static := lowerGraph_handoff ok source
  refine ⟨static.project, static.entry, static.origins, static.layouts, static.signatures,
    static.promotion, static.handoff.1, source.1, ?_, ?_⟩
  · intro safe
    have corr := steps safe
    refine ⟨?_, fun _ run => corr.divergence run, ?_⟩
    · intro q o finite obs
      obtain ⟨p, hp, r⟩ := corr.reached finite
      have finish := corr.finish _ _ _ hp finite r obs
      exact ⟨p, hp, finish, trace_at_result r finish (Interfaces.Memory.matchingResult_result obs)⟩
    · intro q stuck
      obtain ⟨p, hp, r⟩ := corr.reached stuck.1
      rcases corr.progress _ _ hp stuck.1 r with ⟨o, obs⟩ | ⟨q', tick⟩
      · exact stuck.2.2 o obs
      · exact stuck.2.1 q' tick
  · intro safe
    have corr := steps safe
    refine ⟨⟨Related memory, fun _ q => stutterRank memory q, corr.initial, ?_, ?_⟩,
      fun _ _ _ _ r => ⟨r.representation, r.effects⟩⟩
    · intro p q q' hp hq r tick
      rcases corr.matchStep hp hq r tick with ⟨p', step, rel⟩ | pause
      · exact .inl ⟨p, p', .nil _, step, rel⟩
      · exact .inr pause
    · intro p q o hp hq r obs
      have finish := corr.finish _ _ _ hp hq r obs
      exact ⟨p, .nil _, finish, trace_at_result r finish (Interfaces.Memory.matchingResult_result obs)⟩

/-- Classify every observed result of the production Memory execution. Ordinary
returns and traps match an actual MIR execution, including its trace. -/
theorem LowersMemory.result_or_exhausted {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    (contract : LowersMemory (language := language) mir memory) (safe : ProgramSafe Src mir)
    {q o} (reached : Dst.finite memory q) (outcome : Dst.result memory q o) :
    (∃ p, Src.finite mir p ∧ Src.result mir p o ∧ Src.trace mir p = Dst.trace memory q) ∨
      o = .resourceExhausted := by
  rcases Memory.result_cases outcome with ordinary | exhausted
  · exact .inl ((contract.2.2.2.2.2.2.2.2.1 safe).1 q o reached ordinary)
  · exact .inr exhausted

/-- The resource policy can recover strict result matching by ruling out the
exceptional observation on every reachable target run. -/
theorem LowersMemory.refines_of_bound {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    (contract : LowersMemory (language := language) mir memory)
    (bound : ¬ Dst.observes memory .resourceExhausted) :
    BehaviorRefinement Src Dst Src.result Dst.result mir memory := by
  intro safe
  refine ⟨?_, (contract.2.2.2.2.2.2.2.2.1 safe).2⟩
  intro q o reached outcome
  rcases LowersMemory.result_or_exhausted contract safe reached outcome with matched | rfl
  · exact matched
  · exact (bound ⟨q, reached, outcome⟩).elim

end Concrete.Memory
