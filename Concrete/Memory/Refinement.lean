import Concrete.Memory.Execution
import Concrete.Memory.Lifetimes
import Concrete.Memory.TransferProgress
import ConcreteSpec.Proofs.Memory

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
  have same : memory.blockAt opened.advance = memory.blockAt q :=
    Interfaces.Memory.blockAt_congr frame label
  obtain ⟨within, _⟩ := List.getElem?_eq_some_iff.mp atPC
  unfold remainingInstructions
  rw [same, atBlock]
  change block.body.length - (opened.instruction + 1) < block.body.length - q.instruction
  rw [pc]
  omega

private def decExistsLt (P : Nat → Prop) [DecidablePred P] :
    (bound : Nat) → Decidable (∃ n < bound, P n)
  | 0 => isFalse fun ⟨_, hlt, _⟩ => Nat.not_lt_zero _ hlt
  | k + 1 =>
    match decExistsLt P k with
    | isTrue h => isTrue (let ⟨n, hlt, hp⟩ := h; ⟨n, Nat.lt_succ_of_lt hlt, hp⟩)
    | isFalse hlt =>
      match inferInstanceAs (Decidable (P k)) with
      | isTrue hk => isTrue ⟨k, Nat.lt_succ_self k, hk⟩
      | isFalse hnk => isFalse fun ⟨n, hlt', hp⟩ =>
        match Nat.eq_or_lt_of_le (Nat.le_of_lt_succ hlt') with
        | .inl rfl => hnk hp
        | .inr hlt'' => hlt ⟨n, hlt'', hp⟩

private def decExistsMem {α : Type} (P : α → Prop) [DecidablePred P] :
    (xs : List α) → Decidable (∃ x ∈ xs, P x)
  | [] => isFalse fun ⟨_, hm, _⟩ => nomatch hm
  | x :: rest =>
    match inferInstanceAs (Decidable (P x)) with
    | isTrue hx => isTrue ⟨x, .head _, hx⟩
    | isFalse hnx =>
      match decExistsMem P rest with
      | isTrue h => isTrue (let ⟨y, hy, hp⟩ := h; ⟨y, .tail _ hy, hp⟩)
      | isFalse hrest => isFalse fun ⟨y, hy, hp⟩ =>
        match List.mem_cons.mp hy with
        | .inl rfl => hnx hp
        | .inr hy' => hrest ⟨y, hy', hp⟩

/-- Finite, constructive decision procedure for `ReservationsFit`: every fitting
allocation chooses a space from the finite list `target.spaces` and a base below `ceiling`. -/
def decidableReservationsFit (target : Target) (ceiling : Nat) :
    (layouts : List Layout) → (heap : Mem.Memory) →
    Decidable (Interfaces.Memory.ReservationsFit target ceiling layouts heap)
  | [], _ => isTrue trivial
  | layout :: rest, heap =>
    let step (space : AddrSpace) (base : Nat) : Prop :=
      heap.Fits target (Mem.candidate space base layout.size layout.align) ceiling ∧
      Interfaces.Memory.ReservationsFit target ceiling rest
        ⟨(⟨heap.next⟩, Mem.candidate space base layout.size layout.align) :: heap.blocks, heap.next + 1⟩
    letI : ∀ space base, Decidable (step space base) := fun space base =>
      letI := decidableReservationsFit target ceiling rest
        ⟨(⟨heap.next⟩, Mem.candidate space base layout.size layout.align) :: heap.blocks, heap.next + 1⟩
      inferInstanceAs (Decidable (_ ∧ _))
    letI : ∀ space, Decidable (∃ base < ceiling, step space base) := fun space =>
      decExistsLt (step space) ceiling
    match decExistsMem (fun space => ∃ base < ceiling, step space base) target.spaces with
    | isTrue h =>
      isTrue (by
        obtain ⟨space, _, base, _, hfits, hrest⟩ := h
        exact ⟨space, base, ⟨heap.next⟩, _, by unfold Mem.Memory.alloc; dsimp only; exact ite_eq_left hfits, hrest⟩)
    | isFalse hno =>
      isFalse (by
        rintro ⟨space, base, _, _, halloc, hrest⟩
        unfold Mem.Memory.alloc at halloc
        dsimp only at halloc
        split at halloc
        · rename_i hfits
          cases halloc
          exact hno ⟨space, hfits.2.2.2.2.2.2, base,
            Nat.lt_of_le_of_lt (Nat.le_add_right _ _) hfits.2.2.1, hfits, hrest⟩
        · cases halloc)

/-- Constructive decision procedure for `ParamSpaceFits`. -/
def decidableParamSpaceFits (memory : Interfaces.Memory lang profile)
    (params : List (Sigma (MemoryPlace language.operands.Ty)))
    (state : MemoryRegisters language profile)
    (ceiling : Nat := profile.memoryTarget.addressCeiling) :
    Decidable (memory.ParamSpaceFits params state ceiling) :=
  decidableReservationsFit profile.memoryTarget ceiling
    (memory.freshLayouts state (params.map fun p : Sigma (MemoryPlace language.operands.Ty) => p.2.logical.root))
    state.bytes

/-- Constructive decision procedure for whether `memory.Begins q root` admits a successor. -/
def decidableBeginsExists (memory : Interfaces.Memory lang profile)
    (q : MemoryRegisters language profile) (root : PlaceId)
    (ceiling : Nat := profile.memoryTarget.addressCeiling) :
    Decidable (∃ opened, memory.Begins q root opened ceiling) :=
  if hlive : root ∈ q.live then
    isFalse fun ⟨_, hfresh, _⟩ => hfresh hlive
  else
    match hcls : memory.classAt q.frame root with
    | none =>
      isFalse (by
        rintro ⟨_, _, ⟨hv, _⟩ | ⟨hm, _⟩⟩
        · rw [hcls] at hv; cases hv
        · rw [hcls] at hm; cases hm)
    | some .virtual =>
      isTrue ⟨{q with live := root :: q.live}, hlive, .inl ⟨hcls, rfl⟩⟩
    | some .materialized =>
      match hlay : memory.layoutAt q.frame root with
      | none =>
        isFalse (by
          rintro ⟨_, _, ⟨hv, _⟩ | ⟨_, _, hl, _⟩⟩
          · rw [hcls] at hv; cases hv
          · rw [hlay] at hl; cases hl)
      | some layout =>
        if hz : layout.size = 0 then
          isTrue ⟨{q with live := root :: q.live}, hlive, .inr ⟨hcls, layout, hlay, .inl ⟨hz, rfl⟩⟩⟩
        else
          match decidableReservationsFit profile.memoryTarget ceiling [layout] q.bytes with
          | isTrue h =>
            isTrue (by
              obtain ⟨space, base, block, bytes, halloc, _⟩ := h
              let opened : MemoryRegisters language profile :=
                { q with
                  live := root :: q.live
                  bytes := bytes
                  materialized := (root, ⟨⟨space, base, some block⟩, layout.size, layout.align⟩) :: q.materialized }
              exact ⟨opened, hlive, .inr ⟨hcls, layout, hlay,
                .inr ⟨Nat.pos_of_ne_zero hz, space, base, block, bytes, halloc, rfl⟩⟩⟩)
          | isFalse hno =>
            isFalse (by
              rintro ⟨_, _, ⟨hv, _⟩ | ⟨_, layout', hlay', hsz⟩⟩
              · rw [hcls] at hv; cases hv
              · cases Option.some.inj (hlay.symm.trans hlay')
                rcases hsz with ⟨hz', _⟩ | ⟨_, space, base, block, bytes, halloc, _⟩
                · exact hz hz'
                · exact hno ⟨space, base, block, bytes, halloc, trivial⟩)

/-- A begin that is valid with unlimited space can either commit on this target
or report exhaustion at its original checkpoint. -/
theorem begin_progress {memory : Interfaces.Memory lang profile} {q : MemoryRegisters language profile}
    {block op root} (running : q.resourceExhausted = false)
    (found : memory.blockAt q = some block) (pc : block.body[q.instruction]? = some op)
    (inst : op.instruction = .beginLifetime root)
    (valid : memory.BeginValid q root) :
    (∃ opened, memory.Begins q root opened ∧ Dst.step memory q opened.advance) ∨
      (Dst.step memory q {q with resourceExhausted := true} ∧
        Dst.result memory {q with resourceExhausted := true} .resourceExhausted) := by
  have ready : memory.terminalObservation q = none :=
    Interfaces.Memory.terminalObservation_of_instruction running found pc
      (inst ▸ Interfaces.Memory.instructionTrap_beginLifetime)
  match decidableBeginsExists memory q root with
  | isTrue space =>
    obtain ⟨opened, began⟩ := space
    have flags : opened.resourceExhausted = false ∧ opened.effects = q.effects := by
      rcases began.2 with ⟨_, rfl⟩ | ⟨_, _, _, ⟨_, rfl⟩ | ⟨_, _, _, _, _, _, rfl⟩⟩ <;> exact ⟨running, rfl⟩
    refine .inl ⟨opened, began, Interfaces.Memory.silent_step ready ?_ flags.2⟩
    unfold Interfaces.Memory.Step
    have active : opened.advance.resourceExhausted = false := flags.1
    simp only [running, active, Bool.false_eq_true, ite_false]
    refine ⟨block, found, ?_⟩
    simp only [pc, inst]
    exact ⟨opened, began, rfl⟩
  | isFalse space =>
    have failed : memory.BeginExhausted q root := ⟨valid, space⟩
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

theorem LocalCorrespondence.matchStep {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
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
  refine ⟨static.project, static.entry, static.origins, static.layouts, ⟨static.signatures, static.declarations⟩,
    static.promotion, static.retainedRoots, static.handoff.layoutsWF, static.handoff.1, source.1, ?_, ?_⟩
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
  · exact .inl ((contract.2.2.2.2.2.2.2.2.2.2.1 safe).1 q o reached ordinary)
  · exact .inr exhausted

/-- The resource policy can recover strict result matching by ruling out the
exceptional observation on every reachable target run. -/
theorem LowersMemory.refines_of_bound {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    (contract : LowersMemory (language := language) mir memory)
    (bound : ¬ Dst.observes memory .resourceExhausted) :
    BehaviorRefinement Src Dst Src.result Dst.result mir memory := by
  intro safe
  refine ⟨?_, (contract.2.2.2.2.2.2.2.2.2.2.1 safe).2⟩
  intro q o reached outcome
  rcases LowersMemory.result_or_exhausted contract safe reached outcome with matched | rfl
  · exact matched
  · exact (bound ⟨q, reached, outcome⟩).elim

/-! ## ISS-0125: Generic `CoreLanguage` resident-handle residency and `Represents` bridge

Proves constructively (no `Classical.choice`) for **every** `CoreLanguage` (relying on
`CoreLanguage.preservesResidentHandles`), every `Memory` / `SSA` / `BIR` execution, and
every reachable state `q` that:
1. every incoming materialized handle across both the active frame and every suspended
   caller frame stays resident in `q.bytes` with disjoint backing block IDs (`ResidentHandles`);
2. every `CoreLanguage.operation` transition preserves `ResidentHandles` across active
   and suspended frames (`ResidentHandles.of_operation`);
3. every reachable state yields a valid `TransferCheckpoint` at any ceiling `≥ addressCeiling`
   (`ResidentHandles.transferCheckpoint`, `reachable_transferCheckpoint`), including at
   suspended-caller resumption (`ResidentHandles.return_resume_transferCheckpoint`);
4. `LowersMemory` connects reachable `RepresentsCheckpoint` states directly to
   `ResidentHandles`, `TransferCheckpoint`, `ZeroUnbacked`, and the `TransferProgress`
   corollaries (`JumpAt`, `BranchAt`, `CallAt`, `ReturnAt`, `DestructureAt`).
-/

section ResidentHandlesSection

private theorem target_null_eq_zero (p : Profile) : p.memoryTarget.null = 0 := by
  cases p with
  | mk _ tgt _ _ => cases tgt <;> rfl

/-- Unpack `Interfaces.Memory.resident` into its explicit residency fields. -/
theorem resident_iff {memory : Interfaces.Memory lang profile}
    {q : MemoryRegisters language profile} {place : PlaceId} {range : Hybrid.Range} :
    memory.resident q place range = true ↔
      ∃ layout id blk,
        memory.layoutAt q.frame place = some layout ∧
        range.ptr.prov = some id ∧
        q.bytes.block id = some blk ∧
        Interfaces.Memory.CoversBlock range blk ∧
        range.extent = layout.size ∧
        range.align = layout.align ∧
        layout.align ∣ range.ptr.addr ∧
        profile.memoryTarget.null < range.ptr.addr ∧
        range.ptr.space ∈ profile.memoryTarget.spaces ∧
        id.id < q.bytes.next := by
  unfold Interfaces.Memory.resident
  cases hlay : memory.layoutAt q.frame place with
  | none =>
    simp only [Bind.bind, Option.bind, Option.getD_none, Bool.false_eq_true, false_iff]
    rintro ⟨_, _, _, h, _⟩; cases h
  | some layout =>
    cases hprov : range.ptr.prov with
    | none =>
      simp only [Bind.bind, Option.bind, Option.getD_none, Bool.false_eq_true, false_iff]
      rintro ⟨_, _, _, _, h, _⟩; cases h
    | some id =>
      cases hblk : q.bytes.block id with
      | none =>
        simp only [Bind.bind, Option.bind, hblk, Option.getD_none, Bool.false_eq_true, false_iff]
        rintro ⟨_, _, _, _, hp, hb, _⟩
        cases Option.some.inj hp; rw [hb] at hblk; cases hblk
      | some blk =>
        simp only [Bind.bind, Option.bind, hblk, pure, Option.getD_some, decide_eq_true_eq]
        constructor
        · intro h; exact ⟨layout, id, blk, rfl, rfl, hblk, h⟩
        · rintro ⟨l', id', blk', hl', hp', hb', hrest⟩
          cases Option.some.inj hl'
          cases Option.some.inj hp'
          cases Option.some.inj (hblk.symm.trans hb')
          exact hrest

/-- Every materialized handle in one activation frame is resident in `heap`, with
unique keys and all materialized roots live. -/
structure FrameResident (memory : Interfaces.Memory lang profile)
    (frame : MemoryFrame language.operands.Val) (heap : Mem.Memory) : Prop where
  liveNodup : frame.live.Nodup
  matNodup : (List.map Prod.fst frame.materialized).Nodup
  matSubLive : ∀ place range, (place, range) ∈ frame.materialized → place ∈ frame.live
  resident : ∀ place range, (place, range) ∈ frame.materialized →
    memory.resident { toMemoryFrame := frame, bytes := heap } place range = true

/-- Block IDs backing the materialized handles of a single frame. -/
def residentFrameProvIds {Value : Type} (frame : MemoryFrame Value) : List BlockId :=
  frame.materialized.filterMap fun (_, r) => r.ptr.prov

/-- Block IDs backing all materialized handles across the active frame and every
suspended caller frame. -/
def residentStateProvIds (q : MemoryRegisters language profile) : List BlockId :=
  residentFrameProvIds q.toMemoryFrame ++ q.callers.flatMap fun c => residentFrameProvIds c.caller

/-- Reachable handle-residency invariant (`ISS-0125`) across active and suspended frames. -/
structure ResidentHandles (memory : Interfaces.Memory lang profile)
    (q : MemoryRegisters language profile) : Prop where
  bytesWF : q.bytes.WellFormed profile.memoryTarget
  active : FrameResident memory q.toMemoryFrame q.bytes
  callers : ∀ c ∈ q.callers, FrameResident memory c.caller q.bytes
  provsNodup : (residentStateProvIds q).Nodup

private theorem mem_residentFrameProvIds {Value : Type} {frame : MemoryFrame Value}
    {place : PlaceId} {range : Hybrid.Range} {id : BlockId}
    (hmem : (place, range) ∈ frame.materialized) (hprov : range.ptr.prov = some id) :
    id ∈ residentFrameProvIds frame :=
  List.mem_filterMap.mpr ⟨(place, range), hmem, hprov⟩

private theorem frameResident_heap_congr {memory : Interfaces.Memory lang profile}
    {frame : MemoryFrame language.operands.Val} {h1 h2 : Mem.Memory}
    (hfr : FrameResident memory frame h1)
    (hnext : h1.next ≤ h2.next)
    (hblks : ∀ id ∈ residentFrameProvIds frame, ∀ blk, h1.block id = some blk → blk.live = true →
      ∃ blk', h2.block id = some blk' ∧
        blk'.live = true ∧ blk'.space = blk.space ∧ blk'.base = blk.base ∧
        blk'.size = blk.size ∧ blk'.align = blk.align) :
    FrameResident memory frame h2 := by
  refine ⟨hfr.liveNodup, hfr.matNodup, hfr.matSubLive, ?_⟩
  intro place range hmem
  obtain ⟨layout, id, blk, hlay, hprov, hblk, ⟨hlive, hspace, hbase, hsize, halign⟩,
    hext, hral, hald, hnull, hsp, hfresh⟩ := (resident_iff (language := language)).mp (hfr.resident place range hmem)
  obtain ⟨blk', hblk', hl', hs', hb', hsz', ha'⟩ :=
    hblks id (mem_residentFrameProvIds hmem hprov) blk hblk hlive
  exact (resident_iff (language := language)).mpr
    ⟨layout, id, blk', hlay, hprov, hblk',
      ⟨hl', hs'.trans hspace, hb'.trans hbase, hsz'.trans hsize, ha'.trans halign⟩,
      hext, hral, hald, hnull, hsp, Nat.lt_of_lt_of_le hfresh hnext⟩

private theorem frameResident_id_lt_next {memory : Interfaces.Memory lang profile}
    {frame : MemoryFrame language.operands.Val} {heap : Mem.Memory}
    (hfr : FrameResident memory frame heap) {id : BlockId} (hid : id ∈ residentFrameProvIds frame) :
    id.id < heap.next := by
  obtain ⟨⟨place, range⟩, hmem, hprov⟩ := List.mem_filterMap.mp hid
  obtain ⟨_, id', _, _, hprov', _, _, _, _, _, _, _, hfresh⟩ :=
    (resident_iff (language := language)).mp (hfr.resident place range hmem)
  cases Option.some.inj (hprov.symm.trans hprov')
  exact hfresh

private theorem residentHandles_id_lt_next {memory : Interfaces.Memory lang profile}
    {q : MemoryRegisters language profile} (h : ResidentHandles memory q)
    {id : BlockId} (hid : id ∈ residentStateProvIds q) : id.id < q.bytes.next := by
  rcases List.mem_append.mp hid with hact | hcall
  · exact frameResident_id_lt_next h.active hact
  · obtain ⟨c, hc, hidc⟩ := List.mem_flatMap.mp hcall
    exact frameResident_id_lt_next (h.callers c hc) hidc

/-- Every `ResidentHandles` state satisfies `TransferCheckpoint` at any ceiling
`≥ profile.memoryTarget.addressCeiling`. -/
theorem ResidentHandles.transferCheckpoint {memory : Interfaces.Memory lang profile}
    {q : MemoryRegisters language profile} (h : ResidentHandles memory q)
    {ceiling : Nat} (hge : profile.memoryTarget.addressCeiling ≤ ceiling) :
    memory.TransferCheckpoint q ceiling := by
  refine ⟨?_, ?_⟩
  · intro e he
    exact h.active.resident e.1 e.2 he
  · intro e he _
    rw [Nat.min_eq_left hge]
    exact h.bytesWF.fits e.1 e.2 (h.bytesWF.canonical e he)

/-- Every `ResidentHandles` state yields `memory.rootRange` for every active
materialized handle (the backing clause of `Represents`). -/
theorem ResidentHandles.rootRange {memory : Interfaces.Memory lang profile}
    {q : MemoryRegisters language profile} (h : ResidentHandles memory q)
    {place : PlaceId} {range : Hybrid.Range} (hmem : (place, range) ∈ q.materialized) :
    memory.rootRange q place range := by
  obtain ⟨layout, id, blk, hlay, hprov, hblk, hcov, hext, hral, _⟩ :=
    (resident_iff (language := language)).mp (h.active.resident place range hmem)
  exact ⟨layout, id, blk, hlay, hext, hral, hprov, hblk, hcov⟩

private theorem frameResident_of_same_fields {memory : Interfaces.Memory lang profile}
    {f1 f2 : MemoryFrame language.operands.Val} {heap : Mem.Memory}
    (hfr : FrameResident memory f1 heap)
    (hframe : f2.frame = f1.frame) (hlive : f2.live = f1.live)
    (hmat : f2.materialized = f1.materialized) :
    FrameResident memory f2 heap := by
  refine ⟨hlive ▸ hfr.liveNodup, hmat ▸ hfr.matNodup,
    fun p r hm => hlive ▸ hfr.matSubLive p r (hmat ▸ hm), ?_⟩
  intro p r hm
  have hr := hfr.resident p r (hmat ▸ hm)
  have heq : memory.resident { toMemoryFrame := f2, bytes := heap } p r =
      memory.resident { toMemoryFrame := f1, bytes := heap } p r := by
    simp only [Interfaces.Memory.resident, hframe]
  exact heq.trans hr

/-- Any generic `CoreLanguage.operation` transition preserves `ResidentHandles`
across both the active frame and every suspended caller frame, using
`CoreLanguage.preservesResidentHandles` (`ISS-0125`). -/
theorem ResidentHandles.of_operation {memory : Interfaces.Memory lang profile}
    {q : MemoryRegisters language profile} {logical : Bool}
    {env op inputs output values addresses result effects changed}
    (h : ResidentHandles memory q)
    (hOp : language.operation env op inputs output values profile.memoryTarget addresses
      (Interfaces.Memory.operationStore q logical) result effects changed) :
    ResidentHandles memory (Interfaces.Memory.applyOperation q logical changed) := by
  obtain ⟨hWF', hNext, hLive⟩ :=
    language.preservesResidentHandles env op inputs output values profile.memoryTarget addresses
      (Interfaces.Memory.operationStore q logical) result effects changed hOp h.bytesWF
  refine ⟨hWF', ?_, ?_, ?_⟩
  · have hAct := frameResident_heap_congr h.active hNext (fun id _ blk hblk hl => hLive id blk hblk hl)
    cases logical <;> exact frameResident_of_same_fields hAct rfl rfl rfl
  · intro c hc
    obtain ⟨c0, hc0, rfl⟩ := List.mem_map.mp hc
    have hC := frameResident_heap_congr (h.callers c0 hc0) hNext (fun id _ blk hblk hl => hLive id blk hblk hl)
    cases logical <;> exact frameResident_of_same_fields hC rfl rfl rfl
  · have hProvs : residentStateProvIds (Interfaces.Memory.applyOperation q logical changed) = residentStateProvIds q := by
      unfold residentStateProvIds
      have h1 : residentFrameProvIds (Interfaces.Memory.applyOperation q logical changed).toMemoryFrame =
          residentFrameProvIds q.toMemoryFrame := by
        cases logical <;> rfl
      have h2 : (Interfaces.Memory.applyOperation q logical changed).callers.flatMap (fun c => residentFrameProvIds c.caller) =
          q.callers.flatMap (fun c => residentFrameProvIds c.caller) := by
        simp only [Interfaces.Memory.applyOperation, List.flatMap_map]
        congr 1
        funext c
        cases logical <;> rfl
      rw [h1, h2]
    rw [hProvs]
    exact h.provsNodup

private theorem residentHandles_of_same_layout {memory : Interfaces.Memory lang profile}
    {q q' : MemoryRegisters language profile} (h : ResidentHandles memory q)
    (hframe : q'.frame = q.frame) (hlive : q'.live = q.live)
    (hmat : q'.materialized = q.materialized) (hbytes : q'.bytes = q.bytes)
    (hcallers : q'.callers = q.callers) :
    ResidentHandles memory q' := by
  refine ⟨hbytes ▸ h.bytesWF, ?_, ?_, ?_⟩
  · refine ⟨hlive ▸ h.active.liveNodup, hmat ▸ h.active.matNodup,
      fun p r hm => hlive ▸ h.active.matSubLive p r (hmat ▸ hm), ?_⟩
    intro p r hm
    have hr := h.active.resident p r (hmat ▸ hm)
    have heq : memory.resident { toMemoryFrame := q'.toMemoryFrame, bytes := q'.bytes } p r =
        memory.resident { toMemoryFrame := q.toMemoryFrame, bytes := q.bytes } p r := by
      simp only [Interfaces.Memory.resident, hframe, hbytes]
    exact heq.trans hr
  · intro c hc
    rw [hcallers] at hc
    rw [hbytes]
    exact h.callers c hc
  · have hp : residentStateProvIds q' = residentStateProvIds q := by
      unfold residentStateProvIds residentFrameProvIds
      rw [hmat, hcallers]
    rw [hp]
    exact h.provsNodup

private theorem store_preserves_live_blocks {target : Target} {m m' : Mem.Memory}
    {ptr : Mem.Ptr} {bs : List Mem.MByte} {align : Nat}
    (h : m.store target ptr bs align = some m') :
    m'.next = m.next ∧
    ∀ id blk, m.block id = some blk → blk.live = true →
      ∃ blk', m'.block id = some blk' ∧
        blk'.live = true ∧ blk'.space = blk.space ∧ blk'.base = blk.base ∧
        blk'.size = blk.size ∧ blk'.align = blk.align := by
  unfold Mem.Memory.store at h
  cases ha : m.access target ptr bs.length align with
  | none => rw [ha] at h; cases h
  | some res =>
    rw [ha] at h
    cases res with
    | none =>
      cases h
      exact ⟨rfl, fun id blk hb hl => ⟨blk, hb, hl, rfl, rfl, rfl, rfl⟩⟩
    | some pair =>
      obtain ⟨bid, off⟩ := pair
      cases h
      refine ⟨rfl, ?_⟩
      intro id blk hb hl
      simp only [Mem.block_modify]
      by_cases heq : id = bid
      · subst heq
        rw [ite_eq_left rfl, hb]
        exact ⟨_, rfl, hl, rfl, rfl, rfl, rfl⟩
      · rw [ite_eq_right heq]
        exact ⟨blk, hb, hl, rfl, rfl, rfl, rfl⟩

private theorem storeRuns_preserves {target : Target} {range : Hybrid.Range}
    {runs : List (Nat × List Mem.MByte)} {m m' : Mem.Memory}
    (hwf : m.WellFormed target)
    (h : Interfaces.Memory.storeRuns target target.addressCeiling range runs m = some m') :
    m'.WellFormed target ∧ m'.next = m.next ∧
    ∀ id blk, m.block id = some blk → blk.live = true →
      ∃ blk', m'.block id = some blk' ∧
        blk'.live = true ∧ blk'.space = blk.space ∧ blk'.base = blk.base ∧
        blk'.size = blk.size ∧ blk'.align = blk.align := by
  induction runs generalizing m with
  | nil =>
    cases h
    exact ⟨hwf, rfl, fun id blk hb hl => ⟨blk, hb, hl, rfl, rfl, rfl, rfl⟩⟩
  | cons run rest ih =>
    rcases run with ⟨off, bs⟩
    simp only [Interfaces.Memory.storeRuns, List.foldlM_cons, bind, Option.bind] at h
    by_cases hle : off + bs.length ≤ range.extent
    · simp only [hle, ↓reduceIte] at h
      cases hs : m.store target (Interfaces.Memory.atOffset range.ptr off) bs 1 target.addressCeiling with
      | none => rw [hs] at h; cases h
      | some mid =>
        rw [hs] at h
        have hmidWF : mid.WellFormed target := by
          have := Mem.store_wellFormed target m hwf (Interfaces.Memory.atOffset range.ptr off) bs 1
          rw [hs] at this
          exact this
        obtain ⟨hmidNext, hmidLive⟩ := store_preserves_live_blocks hs
        obtain ⟨hfinWF, hfinNext, hfinLive⟩ := ih hmidWF h
        refine ⟨hfinWF, hfinNext.trans hmidNext, ?_⟩
        intro id blk hb hl
        obtain ⟨blk1, hblk1, hl1, hs1, hb1, hsz1, ha1⟩ := hmidLive id blk hb hl
        obtain ⟨blk2, hblk2, hl2, hs2, hb2, hsz2, ha2⟩ := hfinLive id blk1 hblk1 hl1
        exact ⟨blk2, hblk2, hl2, hs2.trans hs1, hb2.trans hb1, hsz2.trans hsz1, ha2.trans ha1⟩
    · simp only [hle, ↓reduceIte] at h
      cases h

private theorem residentHandles_of_writeValue {memory : Interfaces.Memory lang profile}
    {q next : MemoryRegisters language profile} {ty : language.operands.Ty}
    {place : MemoryPlace _ ty} {value : language.operands.Val}
    (h : ResidentHandles memory q)
    (hw : memory.writeValue q place value = some next) :
    ResidentHandles memory next := by
  simp only [Interfaces.Memory.writeValue] at hw
  split at hw
  · cases hw
  · simp only [bind, Option.bind] at hw
    cases ha : memory.authorizeWrite q place with
    | none => simp [ha] at hw
    | some owners =>
      simp only [ha] at hw
      cases hst : place.storage with
      | virtual =>
        simp only [hst] at hw
        cases hr : Interfaces.Memory.replaceLocal (language := language)
            (Interfaces.Memory.markWritten q place owners).virtualValues place.logical value with
        | none => simp [hr] at hw
        | some tbl =>
          simp only [hr, pure, Option.some.injEq] at hw
          subst hw
          exact residentHandles_of_same_layout h rfl rfl rfl rfl rfl
      | materialized =>
        simp only [hst] at hw
        split at hw
        · cases hr : Interfaces.Memory.replaceLocal (language := language)
              (Interfaces.Memory.markWritten q place owners).zeroValues place.logical value with
          | none => simp [hr] at hw
          | some tbl =>
            simp only [hr, pure, Option.some.injEq] at hw
            subst hw
            exact residentHandles_of_same_layout h rfl rfl rfl rfl rfl
        · cases hrg : memory.rangeAt (Interfaces.Memory.markWritten q place owners) place.logical with
          | none => simp [hrg] at hw
          | some rg =>
            simp only [hrg] at hw
            cases hbr : Interfaces.Memory.byteRuns profile.memoryTarget ty value with
            | none => simp [hbr] at hw
            | some runs =>
              simp only [hbr] at hw
              cases hsr : Interfaces.Memory.storeRuns profile.memoryTarget
                  profile.memoryTarget.addressCeiling rg runs (Interfaces.Memory.markWritten q place owners).bytes with
              | none => simp [hsr] at hw
              | some bs =>
                simp only [hsr, pure, Option.some.injEq] at hw
                subst hw
                obtain ⟨hbsWF, hbsNext, hbsLive⟩ := storeRuns_preserves h.bytesWF hsr
                have hAct' := frameResident_heap_congr h.active (Nat.le_of_eq hbsNext.symm)
                  (fun id _ blk hblk hl => hbsLive id blk hblk hl)
                refine ⟨hbsWF, frameResident_of_same_fields hAct' rfl rfl rfl,
                  fun c hc => frameResident_heap_congr (h.callers c hc) (Nat.le_of_eq hbsNext.symm)
                    (fun id _ blk hblk hl => hbsLive id blk hblk hl),
                  h.provsNodup⟩

private theorem residentHandles_of_readValue {memory : Interfaces.Memory lang profile}
    {q next : MemoryRegisters language profile} {ty : language.operands.Ty}
    {read : MemoryRead _ lang.literal ty} {v : language.operands.Val}
    (h : ResidentHandles memory q)
    (hr : memory.readValue q read = some (v, next)) :
    ResidentHandles memory next := by
  have hrf := Interfaces.Memory.readValue_frame hr
  have hlive : next.live = q.live := readValue_live memory q next read hr
  have hbytes : next.bytes = q.bytes := by
    cases read with
    | literal _ =>
      simp only [Interfaces.Memory.readValue] at hr
      obtain ⟨_, _, eq⟩ := Option.bind_eq_some_iff.mp hr
      cases eq; rfl
    | copy p =>
      obtain rfl := ((Interfaces.Memory.copyRead_iff ..).mp hr).2.2.2
      rfl
    | move p =>
      simp only [Interfaces.Memory.readValue, bind, Option.bind] at hr
      cases ho : Own.move (memory.ownerView q p.logical.root ty .owned).1
          (memory.ownerView q p.logical.root ty .owned).2 <;>
        cases hv : memory.loadValue q p <;> simp only [ho, hv] at hr <;> try contradiction
      split at hr <;> obtain ⟨_, rfl⟩ := Prod.mk.inj (Option.some.inj hr) <;> rfl
  exact residentHandles_of_same_layout h hrf.frame hlive hrf.materialized hbytes hrf.callers

private theorem residentHandles_of_readOperands {memory : Interfaces.Memory lang profile}
    {ops : List (Sigma (MemoryRead _ lang.literal))} {q next : MemoryRegisters language profile} {vs}
    (h : ResidentHandles memory q) (hr : memory.readOperands ops q = some (vs, next)) :
    ResidentHandles memory next := by
  induction ops generalizing q vs with
  | nil => cases hr; exact h
  | cons op rest ih =>
    rcases op with ⟨ty, rd⟩
    obtain ⟨⟨_, mid⟩, h1, h2⟩ := Option.bind_eq_some_iff.mp hr
    obtain ⟨⟨_, fin⟩, h3, eq⟩ := Option.bind_eq_some_iff.mp h2
    cases eq
    exact ih (residentHandles_of_readValue h h1) h3

private theorem residentHandles_of_readArgs {memory : Interfaces.Memory lang profile}
    {types : List language.operands.Ty} {args : Args (MemoryRead _ lang.literal) types}
    {q next : MemoryRegisters language profile} {vs}
    (h : ResidentHandles memory q) (hr : memory.readArgs args q = some (vs, next)) :
    ResidentHandles memory next := by
  induction args generalizing q vs with
  | nil => cases hr; exact h
  | cons rd rest ih =>
    obtain ⟨⟨_, mid⟩, h1, h2⟩ := Option.bind_eq_some_iff.mp hr
    obtain ⟨⟨_, fin⟩, h3, eq⟩ := Option.bind_eq_some_iff.mp h2
    cases eq
    exact ih (residentHandles_of_readValue h h1) h3

private theorem residentHandles_of_prepareAddresses {memory : Interfaces.Memory lang profile}
    {addrs : List (Sigma (MemoryPlace language.operands.Ty))}
    {q : MemoryRegisters language profile} (h : ResidentHandles memory q) :
    ResidentHandles memory (Interfaces.Memory.prepareAddresses q addrs) := by
  unfold Interfaces.Memory.prepareAddresses
  induction addrs generalizing q with
  | nil => exact h
  | cons a rest ih =>
    simp only [List.foldl_cons]
    apply ih
    split
    · exact h
    · exact residentHandles_of_same_layout h rfl rfl rfl rfl rfl

private theorem residentHandles_of_begins {memory : Interfaces.Memory lang profile}
    {q next : MemoryRegisters language profile} {place : PlaceId}
    (h : ResidentHandles memory q) (hb : memory.Begins q place next) :
    ResidentHandles memory next := by
  obtain ⟨hfresh, hcases⟩ := hb
  rcases hcases with ⟨_, rfl⟩ | ⟨_, layout, hlay, ⟨_, rfl⟩ | ⟨hpos, space, base, blkId, bytes', halloc, rfl⟩⟩
  · refine ⟨h.bytesWF, ⟨List.nodup_cons.mpr ⟨hfresh, h.active.liveNodup⟩, h.active.matNodup,
      fun p r hm => .tail _ (h.active.matSubLive p r hm), h.active.resident⟩,
      h.callers, h.provsNodup⟩
  · refine ⟨h.bytesWF, ⟨List.nodup_cons.mpr ⟨hfresh, h.active.liveNodup⟩, h.active.matNodup,
      fun p r hm => .tail _ (h.active.matSubLive p r hm), h.active.resident⟩,
      h.callers, h.provsNodup⟩
  · obtain ⟨rfl, rfl, hfits⟩ := Mem.alloc_result profile.memoryTarget q.bytes space base layout.size layout.align blkId bytes' halloc
    have hWF' : (⟨(⟨q.bytes.next⟩, Mem.candidate space base layout.size layout.align) :: q.bytes.blocks,
        q.bytes.next + 1⟩ : Mem.Memory).WellFormed profile.memoryTarget := by
      have := Mem.alloc_wellFormed profile.memoryTarget q.bytes h.bytesWF space base layout.size layout.align
      rw [halloc] at this
      exact this
    have hOldBlocks : ∀ id blk, q.bytes.block id = some blk → blk.live = true →
        ∃ blk', (⟨(⟨q.bytes.next⟩, Mem.candidate space base layout.size layout.align) :: q.bytes.blocks,
          q.bytes.next + 1⟩ : Mem.Memory).block id = some blk' ∧
          blk'.live = true ∧ blk'.space = blk.space ∧ blk'.base = blk.base ∧
          blk'.size = blk.size ∧ blk'.align = blk.align := by
      intro id blk hblk hl
      have hne : (⟨q.bytes.next⟩ : BlockId) ≠ id := by
        have := Mem.alloc_ne_existing profile.memoryTarget q.bytes h.bytesWF space base layout.size layout.align id blk hblk
        rw [halloc] at this
        exact this
      refine ⟨blk, ?_, hl, rfl, rfl, rfl, rfl⟩
      simp only [Mem.Memory.block, AList.lookup, ite_eq_right hne]
      exact hblk
    have hnotMat : place ∉ List.map Prod.fst q.materialized := by
      intro hm
      obtain ⟨⟨p, r⟩, hpr, hp⟩ := List.mem_map.mp hm
      dsimp only at hp; subst hp
      exact hfresh (h.active.matSubLive p r hpr)
    have hNewRes : memory.resident
        { toMemoryFrame := { q.toMemoryFrame with
            live := place :: q.live,
            materialized := (place, ⟨⟨space, base, some ⟨q.bytes.next⟩⟩, layout.size, layout.align⟩) :: q.materialized },
          bytes := ⟨(⟨q.bytes.next⟩, Mem.candidate space base layout.size layout.align) :: q.bytes.blocks, q.bytes.next + 1⟩ }
        place ⟨⟨space, base, some ⟨q.bytes.next⟩⟩, layout.size, layout.align⟩ = true := by
      refine (resident_iff (language := language)).mpr
        ⟨layout, ⟨q.bytes.next⟩, Mem.candidate space base layout.size layout.align,
          hlay, rfl, by simp [Mem.Memory.block, AList.lookup],
          ⟨rfl, rfl, rfl, rfl, rfl⟩, rfl, rfl, Nat.dvd_of_mod_eq_zero hfits.2.1, ?_,
          hfits.2.2.2.2.2.2, Nat.lt_succ_self _⟩
      rw [target_null_eq_zero]
      have hne0 : base ≠ 0 := by
        have := hfits.2.2.2.2.1 hpos
        rwa [target_null_eq_zero] at this
      exact Nat.pos_of_ne_zero hne0
    have hActOld := frameResident_heap_congr h.active (Nat.le_succ _)
      (fun id _ blk hb hl => hOldBlocks id blk hb hl)
    have hNotInProvs : (⟨q.bytes.next⟩ : BlockId) ∉ residentStateProvIds q := by
      intro hm
      have hlt := residentHandles_id_lt_next h hm
      exact Nat.lt_irrefl _ hlt
    refine ⟨hWF', ⟨List.nodup_cons.mpr ⟨hfresh, h.active.liveNodup⟩,
      List.nodup_cons.mpr ⟨hnotMat, h.active.matNodup⟩, ?_, ?_⟩,
      fun c hc => frameResident_heap_congr (h.callers c hc) (Nat.le_succ _)
        (fun id _ blk hb hl => hOldBlocks id blk hb hl),
      List.nodup_cons.mpr ⟨hNotInProvs, h.provsNodup⟩⟩
    · intro p r hm
      rcases List.mem_cons.mp hm with hEq | hTail
      · cases hEq; exact .head _
      · exact .tail _ (h.active.matSubLive p r hTail)
    · intro p r hm
      rcases List.mem_cons.mp hm with hEq | hTail
      · cases hEq; exact hNewRes
      · exact hActOld.resident p r hTail

private theorem alist_erase_sublist {α β : Type} [DecidableEq α] (xs : AList α β) (k : α) :
    (AList.erase xs k).Sublist xs := by
  induction xs with
  | nil => exact .slnil
  | cons x rest ih =>
    simp only [AList.erase]
    split
    · exact ih.cons x
    · exact ih.cons_cons x

private theorem filterMap_sublist {α β : Type} (f : α → Option β) {xs ys : List α}
    (h : xs.Sublist ys) : (xs.filterMap f).Sublist (ys.filterMap f) := by
  induction h with
  | slnil => exact .slnil
  | cons a _ ih =>
    simp only [List.filterMap_cons]
    cases f a with
    | none => exact ih
    | some b => exact ih.cons b
  | cons_cons a _ ih =>
    simp only [List.filterMap_cons]
    cases f a with
    | none => exact ih
    | some b => exact ih.cons_cons b

private theorem erase_mem_iff_nodup {α : Type} [BEq α] [LawfulBEq α] {xs : List α}
    (hnd : xs.Nodup) (x y : α) : y ∈ xs.erase x ↔ y ∈ xs ∧ y ≠ x := by
  induction xs with
  | nil => simp
  | cons a rest ih =>
    obtain ⟨hnot, hrest⟩ := List.nodup_cons.mp hnd
    simp only [List.erase_cons]
    cases hbeq : (a == x) with
    | true =>
      have rfl_ax : a = x := eq_of_beq hbeq
      subst rfl_ax
      simp only [↓reduceIte, List.mem_cons]
      exact ⟨fun hy => ⟨.inr hy, fun h => hnot (h ▸ hy)⟩,
        fun ⟨h1, h2⟩ => h1.elim (fun h => (h2 h).elim) id⟩
    | false =>
      have hne : a ≠ x := ne_of_beq_false hbeq
      simp only [Bool.false_eq_true, ↓reduceIte, List.mem_cons, ih hrest]
      exact ⟨fun h => h.elim (fun h => ⟨.inl h, h ▸ hne⟩) (fun ⟨hm, hn⟩ => ⟨.inr hm, hn⟩),
        fun ⟨h1, h2⟩ => h1.elim .inl (fun hm => .inr ⟨hm, h2⟩)⟩

private theorem alist_erase_mem_iff_local {α β : Type} [DecidableEq α] (xs : AList α β) (k : α)
    (e : α × β) : e ∈ AList.erase xs k ↔ e ∈ xs ∧ e.1 ≠ k := by
  induction xs with
  | nil => simp [AList.erase]
  | cons x rest ih =>
    simp only [AList.erase]
    split
    · rename_i hx
      rw [ih]
      exact ⟨fun ⟨hm, hn⟩ => ⟨.tail _ hm, hn⟩,
        fun ⟨hm, hn⟩ => (List.mem_cons.mp hm).elim (fun h => (hn (h ▸ hx)).elim) (fun h => ⟨h, hn⟩)⟩
    · rename_i hx
      simp only [List.mem_cons, ih]
      exact ⟨fun h => h.elim (fun he => ⟨.inl he, he ▸ hx⟩) (fun ⟨hm, hn⟩ => ⟨.inr hm, hn⟩),
        fun ⟨h1, h2⟩ => h1.elim .inl (fun hm => .inr ⟨hm, h2⟩)⟩

private theorem not_mem_filterMap_erase_of_nodup {α : Type} [DecidableEq α]
    {xs : AList α Hybrid.Range} {k : α} {r : Hybrid.Range} {id : BlockId}
    (hkeyNd : (List.map Prod.fst xs).Nodup)
    (hprovNd : (xs.filterMap fun (_, rg) => rg.ptr.prov).Nodup)
    (hmem : (k, r) ∈ xs) (hprov : r.ptr.prov = some id) :
    id ∉ (AList.erase xs k).filterMap (fun (_, rg) => rg.ptr.prov) := by
  induction xs with
  | nil => cases hmem
  | cons hd rest ih =>
    rcases hd with ⟨k', r'⟩
    obtain ⟨hk'Not, hrestKeyNd⟩ := List.nodup_cons.mp hkeyNd
    simp only [AList.erase]
    rcases List.mem_cons.mp hmem with hEq | hTail
    · cases hEq
      simp only [↓reduceIte, List.filterMap_cons, hprov] at hprovNd ⊢
      obtain ⟨hidNot, _⟩ := List.nodup_cons.mp hprovNd
      intro hm
      exact hidNot ((filterMap_sublist _ (alist_erase_sublist rest k)).subset hm)
    · have hne : k' ≠ k := by
        intro heq; subst heq
        exact hk'Not (List.mem_map.mpr ⟨(k', r), hTail, rfl⟩)
      simp only [hne, ↓reduceIte, List.filterMap_cons]
      cases hpr' : r'.ptr.prov with
      | none =>
        simp only [List.filterMap_cons, hpr'] at hprovNd
        exact ih hrestKeyNd hprovNd hTail
      | some id' =>
        simp only [List.filterMap_cons, hpr'] at hprovNd
        obtain ⟨hid'Not, hrestProvNd⟩ := List.nodup_cons.mp hprovNd
        intro hm
        rcases List.mem_cons.mp hm with rfl | hrest
        · exact hid'Not (List.mem_filterMap.mpr ⟨(k, r), hTail, hprov⟩)
        · exact ih hrestKeyNd hrestProvNd hTail hrest

private theorem residentHandles_of_ends {memory : Interfaces.Memory lang profile}
    {q next : MemoryRegisters language profile} {place : PlaceId}
    (h : ResidentHandles memory q) (he : memory.Ends q place next) :
    ResidentHandles memory next := by
  have hliveNd : (q.live.erase place).Nodup := h.active.liveNodup.erase place
  have hmatSub : (AList.erase q.materialized place).Sublist q.materialized :=
    alist_erase_sublist q.materialized place
  have hmatNd : (List.map Prod.fst (AList.erase q.materialized place)).Nodup :=
    (hmatSub.map Prod.fst).nodup h.active.matNodup
  have hmatSubLive : ∀ p r, (p, r) ∈ AList.erase q.materialized place → p ∈ q.live.erase place := by
    intro p r hm
    obtain ⟨hm0, hne⟩ := (alist_erase_mem_iff_local q.materialized place (p, r)).mp hm
    exact (erase_mem_iff_nodup h.active.liveNodup place p).mpr ⟨h.active.matSubLive p r hm0, hne⟩
  have hProvSub : (residentFrameProvIds { q.toMemoryFrame with materialized := AList.erase q.materialized place }).Sublist
      (residentFrameProvIds q.toMemoryFrame) :=
    filterMap_sublist _ hmatSub
  have hStateProvNd : (residentFrameProvIds { q.toMemoryFrame with materialized := AList.erase q.materialized place } ++
      q.callers.flatMap fun c => residentFrameProvIds c.caller).Nodup :=
    (hProvSub.append (List.Sublist.refl _)).nodup h.provsNodup
  rcases he.2 with ⟨_, rfl⟩ | ⟨_, ⟨_, _, _, rfl⟩ | ⟨range, blkId, bytes', hlu, hprov, hfree, rfl⟩⟩
  · refine ⟨h.bytesWF, ⟨hliveNd, hmatNd, hmatSubLive,
      fun p r hm => h.active.resident p r (hmatSub.subset hm)⟩, h.callers, hStateProvNd⟩
  · refine ⟨h.bytesWF, ⟨hliveNd, hmatNd, hmatSubLive,
      fun p r hm => h.active.resident p r (hmatSub.subset hm)⟩, h.callers, hStateProvNd⟩
  · have hWF' : bytes'.WellFormed profile.memoryTarget := by
      have := Mem.free_wellFormed profile.memoryTarget q.bytes h.bytesWF blkId
      rw [hfree] at this
      exact this
    obtain ⟨_, _, _, hnextEq⟩ := Mem.free_retains_inv q.bytes blkId bytes' hfree
    have hOtherBlock : ∀ id, id ≠ blkId → bytes'.block id = q.bytes.block id := by
      intro id hne
      unfold Mem.Memory.free at hfree
      cases hb : q.bytes.block blkId with
      | none => rw [hb] at hfree; cases hfree
      | some x =>
        rw [hb] at hfree
        cases hxl : x.live with
        | true =>
          simp only [hxl, ↓reduceIte, Option.some.injEq] at hfree
          subst hfree
          simp only [Mem.block_modify, ite_eq_right hne]
        | false =>
          simp only [hxl, Bool.false_eq_true, ↓reduceIte] at hfree
          cases hfree
    have hmemRange : (place, range) ∈ q.materialized := AList.lookup_mem hlu
    have hblkInAct : blkId ∈ residentFrameProvIds q.toMemoryFrame := mem_residentFrameProvIds hmemRange hprov
    have hblkNotInErased : blkId ∉ residentFrameProvIds { q.toMemoryFrame with materialized := AList.erase q.materialized place } :=
      not_mem_filterMap_erase_of_nodup h.active.matNodup (List.nodup_append.mp h.provsNodup).1 hmemRange hprov
    have hblkNotInCallers : blkId ∉ q.callers.flatMap (fun c => residentFrameProvIds c.caller) := by
      intro hm
      exact (List.nodup_append.mp h.provsNodup).2.2 blkId hblkInAct blkId hm rfl
    have hActBefore : FrameResident memory { q.toMemoryFrame with
        live := q.live.erase place, materialized := AList.erase q.materialized place } q.bytes :=
      ⟨hliveNd, hmatNd, hmatSubLive, fun p r hm => h.active.resident p r (hmatSub.subset hm)⟩
    have hActAfter : FrameResident memory { q.toMemoryFrame with
        live := q.live.erase place, materialized := AList.erase q.materialized place } bytes' :=
      frameResident_heap_congr hActBefore (Nat.le_of_eq hnextEq.symm) fun id hid blk hblk hl => by
        have hne : id ≠ blkId := fun heq => hblkNotInErased (heq ▸ hid)
        exact ⟨blk, (hOtherBlock id hne).trans hblk, hl, rfl, rfl, rfl, rfl⟩
    refine ⟨hWF', frameResident_of_same_fields hActAfter rfl rfl rfl, ?_, hStateProvNd⟩
    intro c hc
    exact frameResident_heap_congr (h.callers c hc) (Nat.le_of_eq hnextEq.symm) fun id hid blk hblk hl => by
      have hne : id ≠ blkId := fun heq =>
        hblkNotInCallers (List.mem_flatMap.mpr ⟨c, hc, heq ▸ hid⟩)
      exact ⟨blk, (hOtherBlock id hne).trans hblk, hl, rfl, rfl, rfl, rfl⟩

private theorem residentHandles_of_endLocals {memory : Interfaces.Memory lang profile}
    {places : List PlaceId} {q ended : MemoryRegisters language profile}
    (h : ResidentHandles memory q) (hel : memory.EndLocals places q ended) :
    ResidentHandles memory ended := by
  induction hel with
  | nil _ => exact h
  | cons step _ ih => exact ih (residentHandles_of_ends h step)

private theorem endLocals_clears_materialized {memory : Interfaces.Memory lang profile}
    {places : List PlaceId} {q ended : MemoryRegisters language profile}
    (hsub : ∀ p r, (p, r) ∈ q.materialized → p ∈ places)
    (hel : memory.EndLocals places q ended) :
    ended.materialized = [] := by
  induction hel with
  | nil s =>
    cases hmat : s.materialized with
    | nil => rfl
    | cons e _ => exact (List.not_mem_nil (hsub e.1 e.2 (hmat ▸ .head _))).elim
  | @cons place rest s mid fin hstep _ ih =>
    apply ih
    intro p r hm
    have hmatEq : mid.materialized = AList.erase s.materialized place := by
      rcases hstep.2 with ⟨_, rfl⟩ | ⟨_, ⟨_, _, _, rfl⟩ | ⟨_, _, _, _, _, _, rfl⟩⟩ <;> rfl
    rw [hmatEq] at hm
    obtain ⟨hm0, hne⟩ := (alist_erase_mem_iff_local s.materialized place (p, r)).mp hm
    rcases List.mem_cons.mp (hsub p r hm0) with rfl | hrest
    · exact (hne rfl).elim
    · exact hrest

private theorem residentHandles_of_writeParams {memory : Interfaces.Memory lang profile}
    {ps : List (Sigma (MemoryPlace language.operands.Ty))} {vs : List language.operands.Val}
    {q next : MemoryRegisters language profile}
    (h : ResidentHandles memory q) (hwp : memory.WriteParams ps vs q next false) :
    ResidentHandles memory next := by
  have aux : ∀ {ps vs q next ex c}, memory.WriteParams ps vs q next ex c →
      ex = false → c = profile.memoryTarget.addressCeiling → ResidentHandles memory q →
      ResidentHandles memory next := by
    intro ps vs q next ex c hw hex hc hq
    induction hw with
    | nil _ => exact hq
    | @cons _ pl _ _ _ s0 started stored _ _ _ hbeg hwr _ ih =>
      subst hc
      have hst : ResidentHandles memory started := by
        split at hbeg
        · subst hbeg; exact hq
        · exact residentHandles_of_begins hq hbeg
      exact ih hex rfl (residentHandles_of_writeValue hst hwr.2.2.1)
    | exhausted _ => cases hex
  exact aux hwp rfl rfl h

private theorem residentHandles_of_stepOutcome {memory : Interfaces.Memory lang profile}
    {q next : MemoryRegisters language profile} {startDefinitions : Bool}
    (h : ResidentHandles memory q) (hs : memory.StepOutcome q next false startDefinitions) :
    ResidentHandles memory next := by
  obtain ⟨block, _, hbody⟩ := hs
  cases hpc : block.body[q.instruction]? with
  | none =>
    rw [hpc] at hbody
    obtain ⟨_, hctrl⟩ := hbody
    cases hend : block.ending with
    | jump edge =>
      rw [hend] at hctrl
      obtain ⟨_, afterRead, _, hreads, _, _, hwp⟩ := hctrl
      have h1 := residentHandles_of_readOperands h hreads
      have h2 : ResidentHandles memory { afterRead with block := edge.target, instruction := 0 } :=
        residentHandles_of_same_layout h1 rfl rfl rfl rfl rfl
      exact residentHandles_of_writeParams h2 hwp
    | branch test yes no =>
      rw [hend] at hctrl
      obtain ⟨_, afterTest, flag, hrt, _, _, afterRead, _, hreads, _, _, hwp⟩ := hctrl
      have h1 := residentHandles_of_readValue h hrt
      have h2 := residentHandles_of_readOperands h1 hreads
      have h3 : ResidentHandles memory { afterRead with block := (if flag then yes else no).target, instruction := 0 } :=
        residentHandles_of_same_layout h2 rfl rfl rfl rfl rfl
      exact residentHandles_of_writeParams h3 hwp
    | ret op =>
      rw [hend] at hctrl
      obtain ⟨caller, rest, _, afterRead, ended, hcallers, hreads, hel, _, hwp⟩ := hctrl
      have h1 := residentHandles_of_readOperands h hreads
      have h2 := residentHandles_of_endLocals h1 hel
      have hEmptyMat : ended.materialized = [] :=
        endLocals_clears_materialized h1.active.matSubLive hel
      have hCallEq : ended.callers = caller :: rest :=
        hel.frame.2.2.2.1.trans ((Interfaces.Memory.readOperands_frame hreads).callers.trans hcallers)
      have hRes : ResidentHandles memory (Interfaces.Memory.ReturnAt.resume caller rest ended) := by
        refine ⟨h2.bytesWF, h2.callers caller (hCallEq ▸ .head _),
          fun c hc => h2.callers c (hCallEq ▸ .tail _ hc), ?_⟩
        have hProvs := h2.provsNodup
        unfold residentStateProvIds at hProvs ⊢
        rw [hCallEq, residentFrameProvIds, hEmptyMat] at hProvs
        exact hProvs
      exact residentHandles_of_writeParams hRes hwp
    | trap _ _ | invalid => rw [hend] at hctrl; exact hctrl.elim
  | some op =>
    rw [hpc] at hbody
    dsimp only at hbody
    cases hinst : op.instruction with
    | bind dst src =>
      rw [hinst] at hbody
      obtain ⟨_, afterRead, stored, hrd, hw, rfl⟩ := hbody
      have h1 := residentHandles_of_readValue h hrd
      have h2 : ResidentHandles memory stored := by
        cases startDefinitions
        · exact residentHandles_of_writeValue h1 hw.2.2.1
        · exact residentHandles_of_writeParams h1 hw
      exact residentHandles_of_same_layout h2 rfl rfl rfl rfl rfl
    | assign dst src =>
      rw [hinst] at hbody
      obtain ⟨_, afterRead, stored, hrd, hw, rfl⟩ := hbody
      have h1 := residentHandles_of_readValue h hrd
      have h2 := residentHandles_of_writeValue h1 hw.2.2.1
      exact residentHandles_of_same_layout h2 rfl rfl rfl rfl rfl
    | op dst opcode args checks addresses =>
      rw [hinst] at hbody
      obtain ⟨_, afterReads, _, _, _, changed, stored, hargs, _, _, hop, _, hw, rfl⟩ := hbody
      have h1 := residentHandles_of_readArgs h hargs
      have h2 := residentHandles_of_prepareAddresses (addrs := addresses) h1
      have h3 := ResidentHandles.of_operation h2 hop
      have h4 : ResidentHandles memory stored := by
        cases startDefinitions
        · exact residentHandles_of_writeValue h3 hw.2.2.1
        · exact residentHandles_of_writeParams h3 hw
      exact residentHandles_of_same_layout h4 rfl rfl rfl rfl rfl
    | addWithOverflow t dst flag lhs rhs =>
      rw [hinst] at hbody
      obtain ⟨_, _, _, _, afterLhs, _, afterReads, _, _, stored, hr1, hr2, _, _, hw, rfl⟩ := hbody
      have h1 := residentHandles_of_readValue h hr1
      have h2 := residentHandles_of_readValue h1 hr2
      have h3 : ResidentHandles memory stored := by
        cases startDefinitions
        · obtain ⟨mid, hw1, hw2⟩ := hw
          exact residentHandles_of_writeValue (residentHandles_of_writeValue h2 hw1.2.2.1) hw2.2.2.1
        · exact residentHandles_of_writeParams h2 hw
      exact residentHandles_of_same_layout h3 rfl rfl rfl rfl rfl
    | beginLifetime root =>
      rw [hinst] at hbody
      obtain ⟨started, hb, rfl⟩ := hbody
      exact residentHandles_of_same_layout (residentHandles_of_begins h hb) rfl rfl rfl rfl rfl
    | endLifetime root =>
      rw [hinst] at hbody
      obtain ⟨ended, he, rfl⟩ := hbody
      exact residentHandles_of_same_layout (residentHandles_of_ends h he) rfl rfl rfl rfl rfl
    | destructure src bindings =>
      rw [hinst] at hbody
      obtain ⟨_, afterRead, _, stored, hrd, _, _, hwp, rfl⟩ := hbody
      have h1 := residentHandles_of_readValue h hrd
      have h2 := residentHandles_of_writeParams h1 hwp
      exact residentHandles_of_same_layout h2 rfl rfl rfl rfl rfl
    | call callee dst args checks =>
      rw [hinst] at hbody
      obtain ⟨_, _, afterRead, graph, entry, hargs, _, _, _, hwp⟩ := hbody
      have h1 := residentHandles_of_readOperands h hargs
      have hCallee : ResidentHandles memory
          { toMemoryFrame := ⟨callee, graph.entry, 0, [], [], [], [], [], [], []⟩
            bytes := afterRead.bytes
            ownership := afterRead.ownership
            nextAddressId := afterRead.nextAddressId
            effects := afterRead.effects
            callers := ⟨afterRead.advance.toMemoryFrame, dst⟩ :: afterRead.callers } := by
        refine ⟨h1.bytesWF, ⟨.nil, .nil, nofun, nofun⟩, ?_, h1.provsNodup⟩
        intro c hc
        rcases List.mem_cons.mp hc with rfl | htail
        · exact frameResident_of_same_fields h1.active rfl rfl rfl
        · exact h1.callers c htail
      exact residentHandles_of_writeParams hCallee hwp

/-- Every physical `Memory.Step` (for both `Memory` with `startDefinitions = false` and
`SSA`/`BIR` with `startDefinitions = true`) preserves `ResidentHandles` (`ISS-0125`). -/
theorem ResidentHandles.of_step {memory : Interfaces.Memory lang profile}
    {q next : MemoryRegisters language profile} {startDefinitions : Bool}
    (h : ResidentHandles memory q) (hs : memory.Step q next startDefinitions) :
    ResidentHandles memory next := by
  unfold Interfaces.Memory.Step at hs
  cases hq : q.resourceExhausted with
  | true => simp [hq] at hs
  | false =>
    simp only [hq, Bool.false_eq_true, ↓reduceIte] at hs
    cases hn : next.resourceExhausted with
    | true =>
      simp only [hn, ↓reduceIte] at hs
      obtain ⟨rfl, _⟩ := hs
      exact residentHandles_of_same_layout h rfl rfl rfl rfl rfl
    | false =>
      simp only [hn, Bool.false_eq_true, ↓reduceIte] at hs
      exact residentHandles_of_stepOutcome h hs

/-- Any empty-heap, empty-locals, empty-callers initial state satisfies `ResidentHandles`. -/
theorem ResidentHandles.initial {memory : Interfaces.Memory lang profile}
    {q : MemoryRegisters language profile}
    (hEmpty : q.live = [] ∧ q.materialized = [] ∧ q.bytes = Mem.Memory.empty ∧ q.callers = []) :
    ResidentHandles memory q := by
  obtain ⟨hl, hm, hb, hc⟩ := hEmpty
  refine ⟨hb ▸ Mem.empty_wellFormed profile.memoryTarget,
    ⟨hl ▸ .nil, hm ▸ .nil,
      (fun _ _ h => by rw [hm] at h; cases h),
      (fun _ _ h => by rw [hm] at h; cases h)⟩,
    (fun c h => by rw [hc] at h; cases h), ?_⟩
  unfold residentStateProvIds residentFrameProvIds
  rw [hm, hc]
  exact .nil

/-- Every reachable state of `Memory.Execution` satisfies `ResidentHandles` (`ISS-0125`),
for **any** `CoreLanguage` (relying on `CoreLanguage.preservesResidentHandles`). -/
theorem reachable_residentHandles {memory : Interfaces.Memory lang profile}
    {q : MemoryRegisters language profile} (reached : Dst.finite memory q) :
    ResidentHandles memory q :=
  finite_invariant Dst memory (ResidentHandles memory)
    (fun _ hinit => ResidentHandles.initial (by
      have heq : _ = _ := hinit
      rw [heq]; exact ⟨rfl, rfl, rfl, rfl⟩))
    (fun _ _ h tick => h.of_step tick.2.1) reached

/-- Every reachable state of `Memory.Execution` satisfies `TransferCheckpoint` at any
ceiling `≥ profile.memoryTarget.addressCeiling` (`ISS-0125`). -/
theorem reachable_transferCheckpoint {memory : Interfaces.Memory lang profile}
    {q : MemoryRegisters language profile} (reached : Dst.finite memory q)
    {ceiling : Nat} (hge : profile.memoryTarget.addressCeiling ≤ ceiling) :
    memory.TransferCheckpoint q ceiling :=
  (reachable_residentHandles reached).transferCheckpoint hge

/-- Every reachable state of `SSA.Execution` satisfies `ResidentHandles` (`ISS-0125`). -/
theorem reachable_ssa_residentHandles {ssa : SSA lang profile}
    {q : MemoryRegisters language profile}
    (reached : (Interfaces.SSA.Execution (language := language)).finite ssa q) :
    ResidentHandles (ssa.memoryView (language := language)) q :=
  finite_invariant (Interfaces.SSA.Execution (language := language)) ssa
    (ResidentHandles (ssa.memoryView (language := language)))
    (fun _ hinit => ResidentHandles.initial (by
      have heq : _ = _ := hinit
      rw [heq]; exact ⟨rfl, rfl, rfl, rfl⟩))
    (fun _ _ h tick => h.of_step tick.2.1) reached

/-- Every reachable state of `BIR.Execution` satisfies `ResidentHandles` (`ISS-0125`). -/
theorem reachable_bir_residentHandles {bir : BIR lang profile}
    {q : MemoryRegisters language profile}
    (reached : (Interfaces.BIR.Execution (language := language)).finite bir q) :
    ResidentHandles (bir.memoryView (language := language)) q :=
  finite_invariant (Interfaces.BIR.Execution (language := language)) bir
    (ResidentHandles (bir.memoryView (language := language)))
    (fun _ hinit => ResidentHandles.initial (by
      have heq : _ = _ := hinit
      rw [heq]; exact ⟨rfl, rfl, rfl, rfl⟩))
    (fun _ _ h tick => h.of_step tick.2.1) reached

/-- At a non-entry return (`ReturnAt`), releasing callee locals leaves the resumed
suspended caller with `ResidentHandles` and a valid `TransferCheckpoint` (`ISS-0125`,
connecting to U02's `ReturnAt` transfer-progress corollaries). -/
theorem ResidentHandles.return_resume_transferCheckpoint {memory : Interfaces.Memory lang profile}
    {q read ended : MemoryRegisters language profile}
    {operand caller rest vs}
    (h : ResidentHandles memory q)
    (ret : memory.ReturnAt (language := language) q read ended operand caller rest vs)
    {ceiling : Nat} (hge : profile.memoryTarget.addressCeiling ≤ ceiling) :
    ResidentHandles memory (Interfaces.Memory.ReturnAt.resume caller rest ended) ∧
      memory.TransferCheckpoint (Interfaces.Memory.ReturnAt.resume caller rest ended) ceiling := by
  have h1 := residentHandles_of_readOperands h ret.args
  have h2 := residentHandles_of_endLocals h1 ret.release
  have hEmptyMat : ended.materialized = [] :=
    endLocals_clears_materialized h1.active.matSubLive ret.release
  have hCallEq : ended.callers = caller :: rest :=
    ret.release.frame.2.2.2.1.trans ((Interfaces.Memory.readOperands_frame ret.args).callers.trans ret.callers)
  have hRes : ResidentHandles memory (Interfaces.Memory.ReturnAt.resume caller rest ended) := by
    refine ⟨h2.bytesWF, h2.callers caller (hCallEq ▸ .head _),
      fun c hc => h2.callers c (hCallEq ▸ .tail _ hc), ?_⟩
    have hProvs := h2.provsNodup
    unfold residentStateProvIds at hProvs ⊢
    rw [hCallEq, residentFrameProvIds, hEmptyMat] at hProvs
    exact hProvs
  exact ⟨hRes, hRes.transferCheckpoint hge⟩

/-- Reachable target states of a safe `LowersMemory` refinement simultaneously satisfy
`RepresentsCheckpoint`, `ResidentHandles`, `TransferCheckpoint`, and `ZeroUnbacked`
(`ISS-0125`), discharging the checkpoint preconditions of U02's `TransferProgress`
corollaries across active and suspended frames. -/
theorem LowersMemory.reachable_represents_resident {mir : MIR lang profile}
    {memory : Interfaces.Memory lang profile}
    (contract : LowersMemory (language := language) mir memory)
    (safe : ProgramSafe Src mir) :
    ∀ q, Dst.finite memory q →
      ResidentHandles memory q ∧
      (∀ ceiling, profile.memoryTarget.addressCeiling ≤ ceiling → memory.TransferCheckpoint q ceiling) ∧
      ∃ p, Src.finite mir p ∧ memory.RepresentsCheckpoint p q ∧ memory.ZeroUnbacked q ∧ p.effects = q.effects := by
  obtain ⟨sim, hSimRep⟩ := contract.2.2.2.2.2.2.2.2.2.2.2 safe
  have hSimReach : ∀ q, Dst.finite memory q → ∃ p, Src.finite mir p ∧ sim.related p q := by
    intro q hfin
    obtain ⟨start, hinit, path⟩ := hfin
    obtain ⟨p0, hp0, hr0⟩ := sim.targetInitial start hinit
    have follow : ∀ {a b}, Steps (Dst.step memory) a b → Dst.finite memory a →
        ∀ p, Src.finite mir p → sim.related p a → ∃ p', Src.finite mir p' ∧ sim.related p' b := by
      intro a b steps
      induction steps with
      | nil => exact fun _ p hp hr => ⟨p, hp, hr⟩
      | cons tick _ ih =>
        intro ha p hp hr
        rcases sim.targetAdvance p _ _ hp ha hr tick with
          ⟨before, after, hOld, hStep, hRel⟩ | ⟨hRel, _⟩
        · have hBefore : Src.finite mir before := by
            clear hStep hRel ih tick ha hr
            induction hOld with
            | nil => exact hp
            | cons h1 _ ihOld => exact ihOld (finite_next hp h1.1)
          exact ih (finite_next ha tick) after (finite_next hBefore hStep) hRel
        · exact ih (finite_next ha tick) p hp hRel
    exact follow path ⟨start, hinit, .nil _⟩ p0 ⟨p0, hp0, .nil _⟩ hr0
  intro q hfin
  have hRes := reachable_residentHandles hfin
  obtain ⟨p, hp, hrel⟩ := hSimReach q hfin
  obtain ⟨hRep, hEff⟩ := hSimRep p q hp hfin hrel
  exact ⟨hRes, fun _ hge => hRes.transferCheckpoint hge, p, hp, hRep, hRep.zeroUnbacked, hEff⟩

/-! ### U02 `TransferProgress` checkpoint bridges from `ResidentHandles` -/

theorem transferCeiling_ge (memory : Interfaces.Memory lang profile)
    (state : MemoryRegisters language profile) (roots : List PlaceId) :
    profile.memoryTarget.addressCeiling ≤ memory.transferCeiling state roots := by
  unfold Interfaces.Memory.transferCeiling
  omega

/-- Any `ResidentHandles` state discharges the `TransferCheckpoint` half of `ParamsValid`
at the default widened `transferCeiling`. -/
theorem ResidentHandles.paramsValid_of_writes {memory : Interfaces.Memory lang profile}
    {q : MemoryRegisters language profile} {ps vs next}
    (h : ResidentHandles memory q)
    (hwp : memory.WriteParams ps vs q next false
      (memory.transferCeiling q (ps.map fun p => p.2.logical.root))) :
    memory.ParamsValid ps vs q :=
  ⟨h.transferCheckpoint (transferCeiling_ge memory q _), next, hwp⟩

theorem ResidentHandles.jump_arrive {memory : Interfaces.Memory lang profile}
    {q read : MemoryRegisters language profile} {dst edge vs}
    (h : ResidentHandles memory q) (jump : memory.JumpAt (language := language) q read dst edge vs) :
    ResidentHandles memory { read with block := edge.target, instruction := 0 } :=
  residentHandles_of_same_layout (residentHandles_of_readOperands h jump.args) rfl rfl rfl rfl rfl

theorem ResidentHandles.branch_arrive {memory : Interfaces.Memory lang profile}
    {q read : MemoryRegisters language profile} {dst test yes no flag vs}
    (h : ResidentHandles memory q)
    (branch : memory.BranchAt (language := language) q read dst test yes no flag vs) :
    ResidentHandles memory (Interfaces.Memory.BranchAt.arrive read yes no flag) := by
  obtain ⟨_, _, hrt, _, hargs⟩ := branch.cond
  exact residentHandles_of_same_layout
    (residentHandles_of_readOperands (residentHandles_of_readValue h hrt) hargs) rfl rfl rfl rfl rfl

theorem ResidentHandles.call_arrive {memory : Interfaces.Memory lang profile}
    {q read : MemoryRegisters language profile} {callee dst args checks calleeEntry vs}
    (h : ResidentHandles memory q)
    (call : memory.CallAt (language := language) q read callee dst args checks calleeEntry vs) :
    ResidentHandles memory (Interfaces.Memory.CallAt.arrive callee calleeEntry.id dst read) := by
  have h1 := residentHandles_of_readOperands h call.evalArgs
  refine ⟨h1.bytesWF, ⟨.nil, .nil, nofun, nofun⟩, ?_, h1.provsNodup⟩
  intro c hc
  rcases List.mem_cons.mp hc with rfl | htail
  · exact frameResident_of_same_fields h1.active rfl rfl rfl
  · exact h1.callers c htail

theorem ResidentHandles.destructure_read {memory : Interfaces.Memory lang profile}
    {q read : MemoryRegisters language profile} {ty} {src : MemoryPlace _ ty} {bindings fields}
    (h : ResidentHandles memory q)
    (destr : memory.DestructureAt (language := language) q read src bindings fields) :
    ResidentHandles memory read := by
  obtain ⟨_, hrd, _, _⟩ := destr.readSrc
  exact residentHandles_of_readValue h hrd

/-- At any reachable `LowersMemory` return checkpoint, `RepresentsCheckpoint.zeroUnbacked`
and `ResidentHandles` together supply the full `ReturnAt.exhausted_iff` characterization
from U02 (`TransferProgress.lean`). -/
theorem LowersMemory.reachable_return_exhausted_iff {mir : MIR lang profile}
    {memory : Interfaces.Memory lang profile}
    (contract : LowersMemory (language := language) mir memory)
    (safe : ProgramSafe Src mir)
    {q read ended : MemoryRegisters language profile} {operand caller rest vs}
    (reached : Dst.finite memory q)
    (ret : memory.ReturnAt (language := language) q read ended operand caller rest vs) :
    let resume := Interfaces.Memory.ReturnAt.resume caller rest ended
    ResidentHandles memory resume ∧
    ((Dst.step memory q { q with resourceExhausted := true } ∧
      Dst.result memory { q with resourceExhausted := true } .resourceExhausted) ↔
      (∃ ceiling, memory.ParamsValid caller.destination.toList vs resume ceiling) ∧
        ¬ memory.ParamSpaceFits caller.destination.toList resume) := by
  obtain ⟨hRes, _, _, _, _, hUnbacked, _⟩ := LowersMemory.reachable_represents_resident contract safe q reached
  exact ⟨(hRes.return_resume_transferCheckpoint ret (Nat.le_refl _)).1,
    ret.exhausted_iff contract.layoutsWF hUnbacked⟩

end ResidentHandlesSection

end Concrete.Memory
