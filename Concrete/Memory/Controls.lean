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

private def start : State := ⟨⟨mir.entry, ⟨0⟩, 0, [], [], [], [], [], [], []⟩, .empty, [], [], {}, 0⟩
private def opened : State := {start with live := [⟨0⟩]}
private def value : Language.operands.Val := .scalar (.int .u8 42)
private def stored : State := {opened with initialized := [⟨0⟩], virtualValues := [(⟨0⟩, value)]}
private def result : Concrete.Contracts.Observation := .returned 42 ByteArray.empty

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
  · rintro next ⟨_, ⟨block, found, step⟩, _⟩
    let original := unplaced.functions[0].2.2.blocks[0]'(by with_unfolding_all decide)
    have lookup : unplaced.blockAt start = some original := by with_unfolding_all rfl
    have eq := Option.some.inj (lookup.symm.trans found)
    subst block
    with_unfolding_all
      exact physical_bind_requires_live unplaced start next root (.literal ⟨[42]⟩) (by decide) step
  · intro o obs
    have impossible : none = some o := by with_unfolding_all exact obs
    contradiction

private abbrev finalM : State := {stored with instruction := 2}
private abbrev finalL : State := stored.advance

private theorem logicalBind : source.step mir start finalL := by
  with_unfolding_all
    refine ⟨rfl, ⟨rfl, value, start, stored, rfl, ?_, rfl⟩, [], rfl⟩
    exact .cons (started := opened) (stored := stored)
      ⟨by decide, Or.inl ⟨rfl, rfl⟩⟩ (by exact ⟨⟨rfl, fun _ => rfl⟩, nofun, rfl, rfl, nofun⟩) (.nil _)

private theorem opening_only {q : State} (tick : target.step memory start q) : q = opened.advance := by
  obtain ⟨_, ⟨block, found, step⟩, _⟩ := tick
  let first := memory.functions[0]'(by with_unfolding_all decide)
  let entry := first.2.2.blocks[0]'(by with_unfolding_all decide)
  have lookup : memory.blockAt start = some entry := by with_unfolding_all rfl
  have eq := Option.some.inj (lookup.symm.trans found)
  subst block
  with_unfolding_all change ∃ started, memory.Begins start ⟨0⟩ started ∧ q = started.advance at step
  obtain ⟨s, began, rfl⟩ := step
  rcases began.2 with ⟨_, rfl⟩ | ⟨materialized, _⟩
  · rfl
  · have no : some StorageClass.virtual = some StorageClass.materialized := by with_unfolding_all exact materialized
    contradiction

private theorem binding_only {q : State} (tick : target.step memory opened.advance q) : q = finalM := by
  obtain ⟨_, ⟨block, found, step⟩, _⟩ := tick
  let first := memory.functions[0]'(by with_unfolding_all decide)
  let entry := first.2.2.blocks[0]'(by with_unfolding_all decide)
  have lookup : memory.blockAt opened.advance = some entry := by with_unfolding_all rfl
  have eq := Option.some.inj (lookup.symm.trans found)
  subst block
  with_unfolding_all change memory.InstructionStep opened.advance q false (.bind root (.literal ⟨[42]⟩)) at step
  obtain ⟨v, afterRead, s, read, writes, rfl⟩ := step
  have e : some (value, opened.advance) = some (v, afterRead) := by with_unfolding_all exact read
  cases e
  have storedEq : some ({stored with instruction := 1} : State) = some s := by
    with_unfolding_all exact writes.2.2.1
  cases storedEq
  rfl

private theorem logical_only {q : State} (tick : source.step mir start q) : q = finalL := by
  have step := tick.2.1
  change _ ∧ mir.logicalView.InstructionStep start q true (.bind root (.literal ⟨[42]⟩)) at step
  obtain ⟨v, afterRead, s, read, writes, rfl⟩ := step.2
  have e : some (value, start) = some (v, afterRead) := by with_unfolding_all exact read
  cases e
  cases writes with
  | cons openedHere written tail =>
    cases tail
    rcases openedHere.2 with ⟨_, rfl⟩ | ⟨materialized, _⟩
    · have eq : some stored = some s := by with_unfolding_all exact written.2.2.1
      cases eq
      rfl
    · have no : some StorageClass.virtual = some StorageClass.materialized := by with_unfolding_all exact materialized
      contradiction

private theorem invariant {A E : Type} (sem : Semantics A E) (program : A) (P : sem.State → Prop)
    (initial : ∀ q, sem.initial program q → P q)
    (advance : ∀ p q, P p → sem.step program p q → P q) {q} (finite : sem.finite program q) : P q := by
  obtain ⟨start, atStart, path⟩ := finite
  have follow {p q} (steps : Steps (sem.step program) p q) (h : P p) : P q := by
    induction steps with
    | nil => exact h
    | cons tick _ ih => exact ih (advance _ _ h tick)
  exact follow path (initial _ atStart)

/-- Run the same observation assertion on the exact tables and body returned
by PR 263, packaged only with the source metadata that preparation retains. -/
theorem prepared_cannot_return :
    ∃ m, (prepareGraph (language := Language) mir).map beforePlacement = .ok m ∧
      ¬ target.observes m (.returned 42 ByteArray.empty) := by
  refine ⟨unplaced, prepared_control, ?_⟩
  rintro ⟨q, finite, observes⟩
  have eq := invariant target unplaced (· = start)
    (fun _ initial => by with_unfolding_all exact initial)
    (fun _ _ eq tick => False.elim (unplaced_stuck.2.1 _ (eq ▸ tick))) finite
  exact unplaced_stuck.2.2 _ (eq ▸ observes)

private theorem target_cases {q : State} (reached : target.finite memory q) :
    q = start ∨ q = opened.advance ∨ q = finalM := by
  apply invariant target memory _ (fun _ h => .inl (by with_unfolding_all exact h)) _ reached
  rintro _ q (rfl | rfl | rfl) tick
  · exact .inr (.inl (opening_only tick))
  · exact .inr (.inr (binding_only tick))
  · exact (target.terminal memory finalM result (by with_unfolding_all rfl) q tick).elim

private theorem source_cases {p : State} (reached : source.finite mir p) : p = start ∨ p = finalL := by
  apply invariant source mir _ (fun _ h => .inl (by with_unfolding_all exact h)) _ reached
  rintro _ q (rfl | rfl) tick
  · exact .inr (logical_only tick)
  · exact (source.terminal mir finalL result (by with_unfolding_all rfl) q tick).elim

private theorem represents_start : memory.RepresentsCheckpoint start start := by
  with_unfolding_all
    exact ⟨⟨⟨⟨Mem.empty_wellFormed _, .nil, .nil, nofun⟩, rfl⟩, rfl, nofun, .nil, .nil,
      .nil, .nil, nofun, fun _ => ⟨nofun, nofun⟩, nofun, nofun, nofun, nofun⟩,
      rfl, nofun, ⟨rfl, nofun⟩, .nil, rfl, rfl⟩

private theorem represents_opened : memory.RepresentsCheckpoint start opened.advance := by
  with_unfolding_all
    exact virtual_begin_represents (root := ⟨0⟩) (ty := u8Ty) represents_start (.head _)
      rfl rfl ⟨opened, ⟨by decide, Or.inl ⟨rfl, rfl⟩⟩, rfl⟩

private theorem represents_bound : memory.RepresentsCheckpoint finalL finalM := by
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
    exact ⟨represents_start, ⟨rfl, rfl, _, _, rfl, rfl, by decide, rfl⟩, nofun, rfl⟩

private theorem atPrefix : Related memory start opened.advance := by
  with_unfolding_all
    exact ⟨represents_opened, ⟨rfl, rfl, _, _, rfl, rfl, by decide, rfl⟩, nofun, rfl⟩

private theorem atReturn : Related memory finalL finalM := by
  with_unfolding_all
    exact ⟨represents_bound, ⟨rfl, rfl, _, _, rfl, rfl, by decide, rfl⟩, nofun, rfl⟩

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
  · intro p q q' hp hq r tick
    cases phase hp hq r with
    | start =>
      rw [opening_only tick]
      exact .inr ⟨atPrefix, starts_before_bind⟩
    | opened =>
      rw [binding_only tick]
      exact .inl ⟨finalL, logicalBind, atReturn⟩
    | returned => exact (target.terminal _ _ _ done_target _ tick).elim
  · intro p q o hp hq r obs
    cases phase hp hq r with
    | start => exact (target.terminal _ _ _ obs _ opens).elim
    | opened => exact (target.terminal _ _ _ obs _ binds).elim
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
    ⟨finalL, ⟨start, by with_unfolding_all rfl, .cons logicalBind (.nil _)⟩, by with_unfolding_all rfl⟩, ?_⟩
  · intro p stuck
    rcases source_cases stuck.1 with rfl | rfl
    · exact stuck.2.1 _ logicalBind
    · exact stuck.2.2 result (by with_unfolding_all rfl)
  · obtain ⟨_, got, obs⟩ := observes_42
    cases lowered.symm.trans got
    exact obs

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

private def physicalLoop : Nat → State
  | 0 => start
  | 1 => opened.advance
  | 2 => finalM
  | _ + 3 => loopState

private def logicalLoop : Nat → State
  | 0 => start
  | 1 => finalL
  | _ + 2 => loopState

/-- Non-vacuous silent divergence through an inserted begin. There is a
matching prefix for every time on each of the two actual infinite runs. -/
theorem looping_diverges : ∃ m, lowerGraph (language := Language) looping = .ok m ∧
    target.diverges m physicalLoop ∧ source.diverges looping logicalLoop ∧
    Concrete.Contracts.SameInfiniteTrace (fun i => source.trace looping (logicalLoop i))
      (fun i => target.trace m (physicalLoop i)) := by
  refine ⟨loopMemory, by with_unfolding_all rfl, ⟨by with_unfolding_all rfl, ?_⟩,
    ⟨by with_unfolding_all rfl, ?_⟩, ?_⟩
  · intro i
    match i with
    | 0 => with_unfolding_all exact
        ⟨rfl, ⟨_, rfl, opened, ⟨by decide, Or.inl ⟨rfl, rfl⟩⟩, rfl⟩, [], rfl⟩
    | 1 => with_unfolding_all exact
        ⟨rfl, ⟨_, rfl, value, opened.advance, _, rfl, writes 1, rfl⟩, [], rfl⟩
    | 2 | _ + 3 => with_unfolding_all exact
        ⟨rfl, ⟨_, rfl, rfl, [], _, _, rfl, rfl, rfl, .nil _⟩, [], rfl⟩
  · intro i
    match i with
    | 0 =>
      with_unfolding_all
        refine ⟨rfl, ⟨rfl, value, start, stored, rfl, ?_, rfl⟩, [], rfl⟩
        exact .cons (started := opened) (stored := stored) ⟨by decide, Or.inl ⟨rfl, rfl⟩⟩
          ⟨⟨rfl, fun _ => rfl⟩, nofun, rfl, rfl, nofun⟩ (.nil _)
    | 1 | _ + 2 => with_unfolding_all exact
        ⟨rfl, ⟨_, rfl, rfl, [], _, _, rfl, rfl, rfl, .nil _⟩, [], rfl⟩
  · intro events
    have logical (n : Nat) : source.trace looping (logicalLoop n) = [] := by
      match n with | 0 | 1 | _ + 2 => with_unfolding_all rfl
    have physical (n : Nat) : target.trace loopMemory (physicalLoop n) = [] := by
      match n with | 0 | 1 | 2 | _ + 3 => with_unfolding_all rfl
    simp only [logical, physical]

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

/-- Execute the bind fixture's certified two-step path. Virtual Begins selects
its unique successor. The bytes decoded, value stored, and status returned all
come from the output graph through the frozen Memory operations. -/
private def executeBind (m : M) : Option State := do
  let entry ← m.blockAt start
  let begin ← entry.body[start.instruction]?
  let .beginLifetime r := begin.instruction | none
  if m.classAt start.frame r != some .virtual then none else
  let live := MemoryRegisters.advance (language := Language) ({start with live := r :: start.live} : State)
  let block ← m.blockAt live
  let op ← block.body[live.instruction]?
  let .bind dst src := op.instruction | none
  let (v, afterRead) ← m.readValue live src
  let afterWrite ← m.writeValue afterRead dst v
  return afterWrite.advance

def exitCode : Option UInt8 := do
  let m ← (lowerGraph (language := Language) mir).toOption
  let reached ← executeBind m
  let .returned status _ ← m.terminalObservation (language := Language) reached | none
  return status

theorem executeBind_reaches {m : M} {q : State} (got : lowerGraph (language := Language) mir = .ok m)
    (ran : executeBind m = some q) : target.finite m q := by
  cases lowered.symm.trans got
  have eq : some ({stored with instruction := 2} : State) = some q := by with_unfolding_all exact ran
  cases eq
  exact ⟨start, by with_unfolding_all rfl, .cons opens (.cons binds (.nil _))⟩

example : exitCode = some 42 := by with_unfolding_all rfl
example : executeBind unplaced = none := by with_unfolding_all rfl

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
