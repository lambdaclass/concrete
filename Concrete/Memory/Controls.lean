import Concrete.Memory.Refinement
import ConcreteSpec.Proofs.Memory

namespace Concrete.Memory

open Concrete.Interfaces ConcreteSpec.Core ConcreteSpec.StageInterface
open Concrete.Contracts (Steps Semantics)

namespace BindReturn

private abbrev mir := bindReturnControl slice0
private abbrev Language := sliceLanguage slice0
private abbrev Lowering := sliceLoweringLanguage slice0
private abbrev profile : Profile := ⟨.lcb, .x86_64Linux, 0, 4096⟩
private abbrev State := MemoryRegisters Language profile
private abbrev M := Interfaces.Memory Lowering profile
private abbrev target := Interfaces.Memory.Execution (language := Language) (lang := Lowering) (profile := profile)
private abbrev source := Interfaces.MIR.Execution (language := Language) (lang := Lowering) (profile := profile)

private abbrev u8Ty : Language.operands.Ty := .inl (.scalar (.int .u8))
private def root : MemoryPlace _ u8Ty := ⟨⟨⟨0⟩, []⟩, .virtual⟩

private def originalOp := mir.functions[0].2.2.blocks[0].body[0]
private def entryBlock : MemoryBlock Lowering :=
  ⟨⟨0⟩, [], [⟨originalOp.origin, .beginLifetime ⟨0⟩⟩,
    ⟨originalOp.origin, .bind root (.literal ⟨[42]⟩)⟩], .ret (some ⟨u8Ty, .copy root⟩), []⟩

-- This normal form keeps kernel reduction small. The next equality checks every
-- field against the actual lowering, including the return operand and origins.
private def memory : M := {
  project := mir.project, entry := mir.entry, signatures := mir.signatures
  functions := [(mir.entry, [(⟨0⟩, u8Ty)], ⟨⟨0⟩, #[entryBlock]⟩)]
  storage := [(mir.entry, [(⟨0⟩, .virtual)])], layouts := [(mir.entry, [])] }

private theorem lowered : lowerGraph (language := Language) mir = .ok memory := by with_unfolding_all rfl

private def start : State := ⟨⟨mir.entry, ⟨0⟩, 0, [], [], [], [], [], [], []⟩, .empty, [], [], {}, 0, false⟩
private def opened : State := {start with live := [⟨0⟩]}
private def value : Language.operands.Val := .scalar (.int .u8 42)
private def stored : State := {opened with initialized := [⟨0⟩], virtualValues := [(⟨0⟩, value)]}
private def result : Concrete.Contracts.Observation := .returned 42 ByteArray.empty

/-- The entry has the same fresh bind as the returning control; its successor
is a silent self-loop. -/
private def looping : MIR Lowering profile :=
  let first : MIRGraph Lowering := mir.functions[0].2.2
  let entry := first.blocks[0]'(by with_unfolding_all decide)
  let again := Terminator.jump (Arg := Sigma (Read _ Lowering.literal)) (Cond := Read _ Lowering.literal Lowering.bool)
    ⟨⟨1⟩, []⟩
  {mir with functions := [(mir.entry, [(⟨0⟩, u8Ty)],
    ⟨⟨0⟩, #[{entry with ending := again}, ⟨⟨1⟩, [], [], again, []⟩]⟩)]}

private def loopMemory : M :=
  let again : MemoryTerm Lowering := .jump ⟨⟨1⟩, []⟩
  {memory with functions := [(mir.entry, [(⟨0⟩, u8Ty)],
    ⟨⟨0⟩, #[{entryBlock with ending := again}, ⟨⟨1⟩, [], [], again, []⟩]⟩)]}

private def loopState : State := {stored with block := ⟨1⟩, instruction := 0}

private def fixtureMemory (loops : Bool) : M := if loops then loopMemory else memory
private def fixtureMIR (loops : Bool) : MIR Lowering profile := if loops then looping else mir
private def fixtureEntry (loops : Bool) : MemoryBlock Lowering :=
  if loops then {entryBlock with ending := .jump ⟨⟨1⟩, []⟩} else entryBlock

private theorem opens : target.step memory start opened.advance := by
  with_unfolding_all
    refine ⟨rfl, ⟨_, rfl, opened, ⟨by decide, Or.inl ⟨rfl, rfl⟩⟩, rfl⟩, [], rfl⟩

private theorem writes (pc : Nat) : memory.Writes {opened with instruction := pc} root value
    {stored with instruction := pc} := by
  with_unfolding_all
    exact ⟨⟨rfl, fun _ => rfl⟩, nofun, rfl, rfl, nofun⟩

private theorem binds : target.step memory opened.advance {stored with instruction := 2} := by
  with_unfolding_all
    exact ⟨rfl, ⟨_, rfl, value, opened.advance, _, rfl, writes 1, rfl⟩, [], rfl⟩

/-- A hosted execution from the lowering result itself. The second state is
still uninitialized. The hosted return has to read the bound slot at PC 2. -/
theorem observes_42 : ∃ m, lowerGraph (language := Language) mir = .ok m ∧
    target.observes m (.returned 42 ByteArray.empty) := by
  exact ⟨memory, lowered, _, ⟨start, by with_unfolding_all rfl,
    .cons opens (.cons binds (.nil _))⟩, by with_unfolding_all rfl⟩

private def unplaced : M := {mir.logicalView with layouts := [(mir.entry, [])]}

private def beforePlacement (p : PreparedGraphs Lowering profile) : M :=
  {mir.logicalView with functions := p.functions, storage := p.storage, layouts := p.layouts}

private theorem prepared_control :
    (prepareGraph (language := Language) mir).map beforePlacement = .ok unplaced := by with_unfolding_all rfl

/-- Removing the inserted begin recovers the old one-instruction graph on this
virtual local. It cannot take its first physical step on any choice of successor. -/
theorem unplaced_stuck : target.stuck unplaced start := by
  refine ⟨⟨start, by with_unfolding_all rfl, .nil _⟩, ?_, ?_⟩
  · intro next tick
    let block := unplaced.functions[0].2.2.blocks[0]'(by with_unfolding_all decide)
    have found : unplaced.blockAt start = some block := by with_unfolding_all rfl
    have atBind {s : State} {mode} (h : unplaced.StepOutcome start s mode) :
        unplaced.InstructionStep start s false (.bind root (.literal ⟨[42]⟩)) mode :=
      h.instructionAt found (by with_unfolding_all rfl)
    exact physical_bind_requires_live unplaced start next root (.literal ⟨[42]⟩) (by decide)
      (atBind (Interfaces.Memory.Step.normal tick.2.1 fun _ h => Interfaces.Memory.bind_cannot_exhaust (atBind h)))
  · intro o obs
    have impossible : none = some o := by with_unfolding_all exact obs
    contradiction

private abbrev finalM : State := {stored with instruction := 2}
private abbrev finalL : State := stored.advance

private theorem logicalBind (loops : Bool) : source.step (fixtureMIR loops) start finalL := by
  with_unfolding_all
    cases loops <;> refine ⟨rfl, ⟨rfl, value, start, stored, rfl, ?_, rfl⟩, [], rfl⟩
    all_goals
      exact .cons (started := opened) (stored := stored) ⟨by decide, Or.inl ⟨rfl, rfl⟩⟩
        ⟨⟨rfl, fun _ => rfl⟩, nofun, rfl, rfl, nofun⟩ (.nil _)

private theorem opening_only (loops : Bool) {q : State} (tick : target.step (fixtureMemory loops) start q) : q = opened.advance := by
  have atBegin {s : State} {mode} (h : (fixtureMemory loops).StepOutcome start s mode) :
      (fixtureMemory loops).InstructionStep start s false (.beginLifetime ⟨0⟩) mode := by
    cases loops
    · exact h.instructionAt (block := fixtureEntry false) (by with_unfolding_all rfl) rfl
    · exact h.instructionAt (block := fixtureEntry true) (by with_unfolding_all rfl) rfl
  have step := atBegin (Interfaces.Memory.Step.normal tick.2.1 fun s h => by
    have failed : (fixtureMemory loops).BeginExhausted start ⟨0⟩ := atBegin h
    exact failed.unavailable ⟨opened, by cases loops <;> with_unfolding_all exact ⟨by decide, Or.inl ⟨rfl, rfl⟩⟩⟩)
  obtain ⟨s, began, rfl⟩ := step
  rcases began.2 with ⟨_, rfl⟩ | ⟨materialized, _⟩
  · rfl
  · have no : some StorageClass.virtual = some StorageClass.materialized := by cases loops <;> with_unfolding_all exact materialized
    contradiction

private theorem binding_only (loops : Bool) {q : State} (tick : target.step (fixtureMemory loops) opened.advance q) : q = finalM := by
  have atBind {s : State} {mode} (h : (fixtureMemory loops).StepOutcome opened.advance s mode) :
      (fixtureMemory loops).InstructionStep opened.advance s false (.bind root (.literal ⟨[42]⟩)) mode := by
    cases loops
    · exact h.instructionAt (block := fixtureEntry false) (by with_unfolding_all rfl) rfl
    · exact h.instructionAt (block := fixtureEntry true) (by with_unfolding_all rfl) rfl
  have step := atBind (Interfaces.Memory.Step.normal tick.2.1 fun _ h => Interfaces.Memory.bind_cannot_exhaust (atBind h))
  obtain ⟨v, afterRead, s, read, writes, rfl⟩ := step
  have e : some (value, opened.advance) = some (v, afterRead) := by cases loops <;> with_unfolding_all exact read
  cases e
  have storedEq : some ({stored with instruction := 1} : State) = some s := by
    cases loops <;> with_unfolding_all exact writes.2.2.1
  cases storedEq
  rfl

private theorem logical_only (loops : Bool) {q : State} (tick : source.step (fixtureMIR loops) start q) : q = finalL := by
  have step : (fixtureMIR loops).logicalView.InstructionStep start q true (.bind root (.literal ⟨[42]⟩)) := by
    cases loops <;> with_unfolding_all exact tick.2.1.2
  obtain ⟨v, afterRead, s, read, writes, rfl⟩ := step
  have e : some (value, start) = some (v, afterRead) := by cases loops <;> with_unfolding_all exact read
  cases e
  cases writes with
  | cons openedHere written tail =>
    cases tail
    rcases openedHere.2 with ⟨_, rfl⟩ | ⟨materialized, _⟩
    · have eq : some stored = some s := by cases loops <;> with_unfolding_all exact written.2.2.1
      cases eq
      rfl
    · have no : some StorageClass.virtual = some StorageClass.materialized := by cases loops <;> with_unfolding_all exact materialized
      contradiction

/-- Run the same observation assertion on the exact tables and body returned
by PR 263, packaged only with the source metadata that preparation retains. -/
theorem prepared_cannot_return :
    ∃ m, (prepareGraph (language := Language) mir).map beforePlacement = .ok m ∧
      ¬ target.observes m (.returned 42 ByteArray.empty) := by
  refine ⟨unplaced, prepared_control, ?_⟩
  rintro ⟨q, finite, observes⟩
  have eq := finite_invariant target unplaced (· = start)
    (fun _ initial => by with_unfolding_all exact initial)
    (fun _ _ eq tick => False.elim (unplaced_stuck.2.1 _ (eq ▸ tick))) finite
  exact unplaced_stuck.2.2 _ (eq ▸ observes)

private theorem target_cases {q : State} (reached : target.finite memory q) :
    q = start ∨ q = opened.advance ∨ q = finalM := by
  apply finite_invariant target memory _ (fun _ h => .inl (by with_unfolding_all exact h)) _ reached
  rintro _ q (rfl | rfl | rfl) tick
  · exact .inr (.inl (opening_only false tick))
  · exact .inr (.inr (binding_only false tick))
  · exact (target.terminal memory finalM result (by with_unfolding_all rfl) q tick).elim

private theorem source_cases {p : State} (reached : source.finite mir p) : p = start ∨ p = finalL := by
  apply finite_invariant source mir _ (fun _ h => .inl (by with_unfolding_all exact h)) _ reached
  rintro _ q (rfl | rfl) tick
  · exact .inr (logical_only false tick)
  · exact (source.terminal mir finalL result (by with_unfolding_all rfl) q tick).elim

private theorem represents_start (m : M) : m.RepresentsCheckpoint start start := by
  with_unfolding_all
    exact ⟨⟨⟨⟨Mem.empty_wellFormed _, .nil, .nil, nofun⟩, rfl⟩, rfl, nofun, .nil, .nil,
      .nil, .nil, nofun, fun _ => ⟨nofun, nofun⟩, nofun, nofun, nofun, nofun⟩,
      rfl, nofun, ⟨rfl, nofun⟩, .nil, rfl, rfl⟩

private theorem represents_opened (loops : Bool) : (fixtureMemory loops).RepresentsCheckpoint start opened.advance := by
  with_unfolding_all
    cases loops <;> exact virtual_begin_represents (root := ⟨0⟩) (ty := u8Ty) (represents_start _) (.head _)
      rfl rfl ⟨opened, ⟨by decide, Or.inl ⟨rfl, rfl⟩⟩, rfl⟩

private theorem represents_bound (blocks : Array (MemoryBlock Lowering)) :
    ({memory with functions := [(mir.entry, [(⟨0⟩, u8Ty)], ⟨⟨0⟩, blocks⟩)]} : M).RepresentsCheckpoint finalL finalM := by
  with_unfolding_all
    refine ⟨⟨⟨⟨Mem.empty_wellFormed _, by decide, by decide, fun _ h => h⟩, rfl⟩, rfl,
      fun _ h => h, by decide, .nil, by decide, .nil, nofun, ?_, ?_, ?_, nofun, ?_⟩,
      rfl, nofun, ⟨rfl, nofun⟩, .nil, rfl, rfl⟩
    · intro r
      change r ∈ [⟨0⟩] ↔ ((AList.lookup [(⟨0⟩, value)] r).isSome)
      by_cases e : (⟨0⟩ : PlaceId) = r <;> simp_all [AList.lookup, eq_comm]
    · intro r v h
      cases List.mem_singleton.mp h
      exact .head _
    · intro r v h
      cases List.mem_singleton.mp h
      exact ⟨.head _, rfl⟩
    · intro r h
      cases List.mem_singleton.mp h
      refine ⟨_, _, u8Ty, .virtual, .head _, rfl, rfl, nofun, ?_⟩
      intro v found
      have e : some value = some v := by with_unfolding_all exact found
      cases e
      exact ⟨value, rfl, rfl, nofun⟩

private theorem atStart : Related memory start start := by
  with_unfolding_all
    exact ⟨represents_start _, ⟨rfl, rfl, _, _, rfl, rfl, by decide, rfl⟩, nofun, rfl⟩

private theorem atPrefix : Related memory start opened.advance := by
  with_unfolding_all
    exact ⟨represents_opened false, ⟨rfl, rfl, _, _, rfl, rfl, by decide, rfl⟩, nofun, rfl⟩

private theorem atReturn : Related memory finalL finalM := by
  with_unfolding_all
    exact ⟨represents_bound _, ⟨rfl, rfl, _, _, rfl, rfl, by decide, rfl⟩, nofun, rfl⟩

private inductive Phase : State → State → Prop
  | start : Phase start start
  | opened : Phase start opened.advance
  | returned : Phase finalL finalM

private theorem phase {p q : State} (hp : source.finite mir p) (hq : target.finite memory q)
    (r : Related memory p q) : Phase p q := by
  rcases source_cases hp with rfl | rfl <;> rcases target_cases hq with rfl | rfl | rfl
  · exact .start
  · exact .opened
  · exact absurd (r.active.unique_pc atReturn.active) (by decide)
  · exact absurd (r.active.unique_pc atStart.active) (by decide)
  · exact absurd (r.active.unique_pc atPrefix.active) (by decide)
  · exact .returned

private theorem done_target : target.result memory finalM result := by with_unfolding_all rfl
private theorem done_source : source.result mir finalL result := by with_unfolding_all rfl

private theorem starts_before_bind : ∃ block op rootId, memory.blockAt start = some block ∧
    block.body[start.instruction]? = some op ∧ op.instruction = .beginLifetime rootId :=
  ⟨entryBlock, _, ⟨0⟩, by with_unfolding_all rfl, rfl, rfl⟩

private theorem correspondence : LocalCorrespondence (language := Language) mir memory := by
  constructor
  · intro q initial
    have eq : q = start := by with_unfolding_all exact initial
    rw [eq]
    exact ⟨start, by with_unfolding_all rfl, atStart⟩
  · intro p q q' hp hq r tick _
    cases phase hp hq r with
    | start =>
      rw [opening_only false tick]
      exact .inr ⟨atPrefix, starts_before_bind⟩
    | opened =>
      rw [binding_only false tick]
      exact .inl ⟨finalL, logicalBind false, atReturn⟩
    | returned => exact (target.terminal _ _ _ done_target _ tick).elim
  · intro p q o hp hq r obs
    cases phase hp hq r with
    | start => exact (target.terminal _ _ _ (Interfaces.Memory.matchingResult_result obs) _ opens).elim
    | opened => exact (target.terminal _ _ _ (Interfaces.Memory.matchingResult_result obs) _ binds).elim
    | returned =>
      have eq := Option.some.inj (done_target.symm.trans obs)
      exact eq ▸ done_source
  · intro _ q _ hq _
    rcases target_cases hq with rfl | rfl | rfl
    · exact .inr ⟨_, opens⟩
    · exact .inr ⟨_, binds⟩
    · exact .inl ⟨result, done_target⟩

/-- Instantiate every premise of the frozen contract on the typed Slice 0 fixture.
Both machines are safe, so the safe-source obligations have a reachable witness. -/
theorem refines_42 :
    ∃ m, lowerGraph (language := Language) mir = .ok m ∧
      ProgramSafe source mir ∧ LowersMemory (language := Language) mir m ∧
      source.observes mir result ∧ target.observes m result := by
  letI := Language.typeDecidableEq
  have handoff : mir.logicalView.HandoffWF := (checkHandoff_iff _).mp (by with_unfolding_all decide)
  refine ⟨memory, lowered, ?_, lowerGraph_refines lowered handoff (fun _ => correspondence),
    ⟨finalL, ⟨start, by with_unfolding_all rfl, .cons (logicalBind false) (.nil _)⟩, by with_unfolding_all rfl⟩, ?_⟩
  · intro p stuck
    rcases source_cases stuck.1 with rfl | rfl
    · exact stuck.2.1 _ (logicalBind false)
    · exact stuck.2.2 result (by with_unfolding_all rfl)
  · obtain ⟨_, got, obs⟩ := observes_42
    cases lowered.symm.trans got
    exact obs

/-- Every reachable result of the lowered fixture matches a reachable MIR result
and trace. The two nonterminal PCs have successors and the last one returns 42,
which discharges the bound premise for strict refinement. -/
theorem bounded_refinement_42 :
    (∀ q o, target.finite memory q → target.result memory q o →
      ∃ p, source.finite mir p ∧ source.result mir p o ∧ source.trace mir p = target.trace memory q) ∧
    BehaviorRefinement source target source.result target.result mir memory := by
  obtain ⟨m, got, safe, contract, _⟩ := refines_42
  cases lowered.symm.trans got
  have bound : ¬ target.observes memory .resourceExhausted := by
    rintro ⟨q, reached, observed⟩
    rcases target_cases reached with rfl | rfl | rfl
    · exact target.terminal _ _ _ observed _ opens
    · exact target.terminal _ _ _ observed _ binds
    · cases Option.some.inj (done_target.symm.trans observed)
  refine ⟨?_, LowersMemory.refines_of_bound contract bound⟩
  intro q o reached observed
  rcases LowersMemory.result_or_exhausted contract safe reached observed with matched | rfl
  · exact matched
  · exact (bound ⟨q, reached, observed⟩).elim

private def physicalLoop : Nat → State
  | 0 => start
  | 1 => opened.advance
  | 2 => finalM
  | _ + 3 => loopState

private theorem physicalLoop_run : target.diverges loopMemory physicalLoop := by
  refine ⟨by with_unfolding_all rfl, ?_⟩
  intro i
  match i with
  | 0 => with_unfolding_all exact
      ⟨rfl, ⟨_, rfl, opened, ⟨by decide, Or.inl ⟨rfl, rfl⟩⟩, rfl⟩, [], rfl⟩
  | 1 => with_unfolding_all exact
      ⟨rfl, ⟨_, rfl, value, opened.advance, _, rfl, writes 1, rfl⟩, [], rfl⟩
  | 2 | _ + 3 => with_unfolding_all exact
      ⟨rfl, ⟨_, rfl, rfl, [], _, _, rfl, rfl, rfl, .nil _⟩, [], rfl⟩

private theorem empty_transfer {m : M} {s q : State} {mode : Bool} {label : BlockId}
    {block} (found : m.blockAt {s with block := label} = some block) (params : block.params = [])
    (h : m.Transfer ⟨label, []⟩ s q mode) :
    q = {s with block := label, instruction := 0} ∧ mode = false := by
  obtain ⟨vals, afterRead, _, read, lookup, _, writes⟩ := h
  have eq : some ([], s) = some (vals, afterRead) := read
  cases eq
  cases Option.some.inj (found.symm.trans lookup)
  rw [params] at writes
  exact writes.empty

private theorem loop_jump_only {s q : State} (hs : s = finalM ∨ s = loopState)
    (h : target.step loopMemory s q) : q = loopState := by
  have transferred {next : State} {mode} (raw : loopMemory.StepOutcome s next mode) :
      next = loopState ∧ mode = false := by
    let body := loopMemory.functions[0].2.2.blocks
    rcases hs with rfl | rfl
    · apply empty_transfer (s := finalM) (m := loopMemory) (block := body[1]'(by with_unfolding_all decide))
        (label := ⟨1⟩) (by with_unfolding_all rfl) rfl
      exact raw.terminatorAt (block := body[0]'(by with_unfolding_all decide)) (by with_unfolding_all rfl) rfl
    · apply empty_transfer (s := loopState) (m := loopMemory) (block := body[1]'(by with_unfolding_all decide))
        (label := ⟨1⟩) (by with_unfolding_all rfl) rfl
      exact raw.terminatorAt (block := body[1]'(by with_unfolding_all decide)) (by with_unfolding_all rfl) rfl
  exact (transferred (Interfaces.Memory.Step.normal h.2.1 fun _ tick => Bool.noConfusion (transferred tick).2)).1

private theorem loop_source_cases {p : State} (hp : source.finite looping p) :
    p = start ∨ p = finalL ∨ p = loopState := by
  apply finite_invariant source looping _ (fun _ h => .inl (by with_unfolding_all exact h)) _ hp
  rintro _ next (rfl | rfl | rfl) tick
  · exact .inr (.inl (logical_only true tick))
  · let body := looping.logicalView.functions[0].2.2.blocks
    obtain ⟨blk, found, _, move⟩ := tick.2.1
    have same : some (body[0]'(by with_unfolding_all decide)) = some blk := by with_unfolding_all exact found
    cases Option.some.inj same
    exact .inr (.inr (empty_transfer (m := looping.logicalView) (s := finalL) (mode := false)
      (block := body[1]'(by with_unfolding_all decide)) (label := ⟨1⟩) (by with_unfolding_all rfl)
      (by with_unfolding_all rfl) (by with_unfolding_all exact move)).1)
  · let body := looping.logicalView.functions[0].2.2.blocks
    obtain ⟨blk, found, _, move⟩ := tick.2.1
    have same : some (body[1]'(by with_unfolding_all decide)) = some blk := by with_unfolding_all exact found
    cases Option.some.inj same
    exact .inr (.inr (empty_transfer (m := looping.logicalView) (s := loopState) (mode := false)
      (block := body[1]'(by with_unfolding_all decide)) (label := ⟨1⟩) (by with_unfolding_all rfl)
      (by with_unfolding_all rfl) (by with_unfolding_all exact move)).1)

private theorem loop_target_cases {q : State} (hq : target.finite loopMemory q) :
    q = start ∨ q = opened.advance ∨ q = finalM ∨ q = loopState := by
  apply finite_invariant target loopMemory _ (fun _ h => .inl (by with_unfolding_all exact h)) _ hq
  rintro _ next (rfl | rfl | rfl | rfl) tick
  · exact .inr (.inl (opening_only true tick))
  · exact .inr (.inr (.inl (binding_only true tick)))
  · exact .inr (.inr (.inr (loop_jump_only (.inl rfl) tick)))
  · exact .inr (.inr (.inr (loop_jump_only (.inr rfl) tick)))

private theorem loopInitial : Related loopMemory start start := by with_unfolding_all exact
    ⟨represents_start _, ⟨rfl, rfl, _, _, rfl, rfl, by decide, rfl⟩, nofun, rfl⟩
private theorem loopOpen : Related loopMemory start opened.advance := by with_unfolding_all exact
    ⟨represents_opened true, ⟨rfl, rfl, _, _, rfl, rfl, by decide, rfl⟩, nofun, rfl⟩
private theorem loopBound : Related loopMemory finalL finalM := by with_unfolding_all exact
    ⟨represents_bound (loopMemory.functions[0].2.2.blocks), ⟨rfl, rfl, _, _, rfl, rfl, by decide, rfl⟩, nofun, rfl⟩
private theorem loopAgain : Related loopMemory loopState loopState := by with_unfolding_all exact
    ⟨represents_bound (loopMemory.functions[0].2.2.blocks), ⟨rfl, rfl, _, _, rfl, rfl, by decide, rfl⟩, nofun, rfl⟩

private theorem loop_correspondence : LocalCorrespondence (language := Language) looping loopMemory := by
  constructor
  · intro q h
    have eq : q = start := by with_unfolding_all exact h
    subst q
    exact ⟨start, by with_unfolding_all rfl, loopInitial⟩
  · intro p q q' hp hq r tick _
    rcases loop_source_cases hp with rfl | rfl | rfl <;>
      rcases loop_target_cases hq with rfl | rfl | rfl | rfl
    · rw [opening_only true tick]
      exact .inr ⟨loopOpen, _, _, ⟨0⟩, by with_unfolding_all rfl, rfl, rfl⟩
    · rw [binding_only true tick]
      exact .inl ⟨finalL, logicalBind true, loopBound⟩
    · exact absurd (r.active.unique_pc loopBound.active) (by decide)
    · exact absurd r.active.2.1 (by decide)
    · exact absurd (r.active.unique_pc loopInitial.active) (by decide)
    · exact absurd (r.active.unique_pc loopOpen.active) (by decide)
    · rw [loop_jump_only (.inl rfl) tick]
      exact .inl ⟨loopState, by with_unfolding_all exact
        ⟨rfl, ⟨_, rfl, rfl, [], _, _, rfl, rfl, rfl, .nil _⟩, [], rfl⟩, loopAgain⟩
    · exact absurd r.active.2.1 (by decide)
    · exact absurd r.active.2.1 (by decide)
    · exact absurd r.active.2.1 (by decide)
    · exact absurd r.active.2.1 (by decide)
    · rw [loop_jump_only (.inr rfl) tick]
      exact .inl ⟨loopState, by with_unfolding_all exact
        ⟨rfl, ⟨_, rfl, rfl, [], _, _, rfl, rfl, rfl, .nil _⟩, [], rfl⟩, loopAgain⟩
  · intro _ q o _ hq _ obs
    rcases loop_target_cases hq with rfl | rfl | rfl | rfl
    all_goals have bad : none = some o := by with_unfolding_all exact obs
    all_goals contradiction
  · intro _ q _ hq _
    rcases loop_target_cases hq with rfl | rfl | rfl | rfl
    · exact .inr ⟨_, physicalLoop_run.2 0⟩
    · exact .inr ⟨_, physicalLoop_run.2 1⟩
    · exact .inr ⟨_, physicalLoop_run.2 2⟩
    · exact .inr ⟨_, physicalLoop_run.2 3⟩

/-- The correspondence constructs a cofinal MIR run through the inserted begin. -/
theorem looping_diverges : ∃ m, lowerGraph (language := Language) looping = .ok m ∧
    target.diverges m physicalLoop ∧ ∃ matching, source.diverges looping matching ∧
      Concrete.Contracts.SameInfiniteTrace (fun i => source.trace looping (matching i))
        (fun i => target.trace m (physicalLoop i)) :=
  ⟨loopMemory, by with_unfolding_all rfl, physicalLoop_run, loop_correspondence.divergence physicalLoop_run⟩

private def arithmetic (right : UInt8) (declareResult : Bool) : MIR Lowering profile :=
  let op : MIRInst Lowering := .op (opcode := .arithmetic (.plain .add)) (types := [u8Ty, u8Ty]) root.logical ()
    (.cons (.literal ⟨[20]⟩) (.cons (.literal ⟨[right]⟩) .nil)) [⟨0, .overflow, .residual⟩] []
  {mir with functions := [(mir.entry, if declareResult then [(⟨0⟩, u8Ty)] else [],
    ⟨⟨0⟩, #[⟨⟨0⟩, [], [⟨originalOp.origin, op⟩], .ret (some ⟨u8Ty, .copy root.logical⟩), []⟩]⟩)]}

private def addMemory (n : UInt8) : M :=
  let op : MemoryInst Lowering := .op (opcode := .arithmetic (.plain .add)) (types := [u8Ty, u8Ty]) root ()
    (.cons (.literal ⟨[20]⟩) (.cons (.literal ⟨[n]⟩) .nil)) [⟨0, .overflow, .residual⟩] []
  {memory with functions := [(mir.entry, [(⟨0⟩, u8Ty)], ⟨⟨0⟩, #[{entryBlock with body :=
    [⟨originalOp.origin, .beginLifetime ⟨0⟩⟩, ⟨originalOp.origin, op⟩]}]⟩)]}

theorem checked_add_returns : ∃ m, lowerGraph (language := Language) (arithmetic 22 true) = .ok m ∧
    target.observes m result := by
  with_unfolding_all
    refine ⟨addMemory 22, rfl, finalM, ⟨start, rfl, .cons (b := opened.advance) ?_ (.cons ?_ (.nil _))⟩, rfl⟩
    · exact ⟨rfl, ⟨_, rfl, opened, ⟨by decide, Or.inl ⟨rfl, rfl⟩⟩, rfl⟩, [], rfl⟩
    · refine ⟨rfl, ⟨_, rfl, _, opened.advance, [], value, [], _, ({stored with instruction := 1} : State), rfl, rfl, rfl, ?_, rfl,
        ?_, rfl⟩, [], rfl⟩
      · exact ⟨rfl, rfl, rfl, rfl⟩
      · exact writes 1

/-- A residual check traps before attempting to define its result. In particular
a missing result declaration cannot turn a reachable overflow into a stuck begin. -/
theorem checked_add_traps : ∃ m, lowerGraph (language := Language) (arithmetic 250 false) = .ok m ∧
    m.terminalObservation (language := Language) start = some (.trapped 134 .overflow originalOp.origin.lexical
      (trapDiagnostic mir.project .overflow originalOp.origin.operation originalOp.origin.lexical)) := by
  with_unfolding_all exact ⟨_, rfl, rfl⟩

/-- The destination is declared. Its inserted begin executes before the
original checked addition traps with its source site and diagnostic. -/
theorem declared_add_traps : ∃ m, lowerGraph (language := Language) (arithmetic 250 true) = .ok m ∧
    target.initial m start ∧ target.step m start opened.advance ∧
    target.result m opened.advance (.trapped 134 .overflow
      ⟨⟨originalOp.origin.operation.id⟩, ⟨mainSourcePath.render.text, originalOp.origin.lexical.line, originalOp.origin.lexical.col⟩⟩
      (trapDiagnostic mir.project .overflow originalOp.origin.operation originalOp.origin.lexical)) := by
  with_unfolding_all
    exact ⟨addMemory 250, rfl, rfl, ⟨rfl, ⟨_, rfl, opened, ⟨by decide, Or.inl ⟨rfl, rfl⟩⟩, rfl⟩, _, rfl⟩, rfl⟩

private def wrongDeclBlock : MemoryBlock Lowering :=
  {entryBlock with ending := .ret (some ⟨u8Ty, .literal ⟨[42]⟩⟩)}

private def wrongDecl : M := {memory with functions :=
  [(mir.entry, [(⟨0⟩, .inl (.scalar (.int .u16)))], ⟨⟨0⟩, #[wrongDeclBlock]⟩)]}

/-- The begin is reachable. The u8 bind fails its declared-place check even
though the following terminator would return literal 42. -/
theorem wrong_decl_stuck : target.stuck wrongDecl opened.advance := by
  refine ⟨⟨start, by with_unfolding_all rfl, .cons (b := opened.advance) ?_ (.nil _)⟩, ?_, ?_⟩
  · with_unfolding_all exact ⟨rfl, ⟨_, rfl, opened, ⟨by decide, Or.inl ⟨rfl, rfl⟩⟩, rfl⟩, [], rfl⟩
  · intro q tick
    have atBind {s : State} {mode} (h : wrongDecl.StepOutcome opened.advance s mode) :
        wrongDecl.InstructionStep opened.advance s false (.bind root (.literal ⟨[42]⟩)) mode :=
      h.instructionAt (block := wrongDeclBlock) (by with_unfolding_all rfl) rfl
    obtain ⟨v, s, _, read, written, _⟩ := atBind (Interfaces.Memory.Step.normal tick.2.1
      fun _ h => Interfaces.Memory.bind_cannot_exhaust (atBind h))
    have eq : some (value, opened.advance) = some (v, s) := by with_unfolding_all exact read
    cases eq
    have bad : false = true := by with_unfolding_all exact written.1.1
    contradiction
  · intro o obs
    have bad : none = some o := by with_unfolding_all exact obs
    contradiction

/-- Re-entering the defining block without ending its root gives it both
incoming lifetime states. Ending the root on the back edge permits one placement. -/
private def backEdge (ends : Bool) : MIR Lowering profile :=
  let original := mir.functions[0].2.2.blocks[0]
  let body := if ends then original.body ++ [⟨originalOp.origin, .endLifetime ⟨0⟩⟩] else original.body
  {mir with functions := [(mir.entry, [(⟨0⟩, u8Ty)], ⟨⟨0⟩, #[{original with
    body, ending := .jump ⟨⟨0⟩, []⟩}]⟩)]}

#guard match lowerGraph (language := Language) (backEdge false) with
  | .error (.lifetime j) => j.fn == mir.entry && j.block == ⟨0⟩ && j.root == ⟨0⟩
  | _ => false
private def beginsAndEndsSameRoot : List (LocatedOp (MemoryInst Lowering)) → Bool
  | [a, b, c] => match a.instruction, b.instruction, c.instruction with
    | .beginLifetime id, .bind dst _, .endLifetime ended => id == dst.logical.root && id == ended
    | _, _, _ => false
  | _ => false

#guard match lowerGraph (language := Language) (backEdge true) with
  | .ok m =>
    match m.functions.head?.bind (fun f => f.2.2.blocks[0]?) with
    | some b => beginsAndEndsSameRoot b.body
    | none => false
  | _ => false

end BindReturn

end Concrete.Memory
