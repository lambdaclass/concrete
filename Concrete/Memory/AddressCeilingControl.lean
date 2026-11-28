import Concrete.Memory.Refinement
import ConcreteSpec.Proofs.Memory

namespace Concrete.Memory.AddressCeilingControl
open Concrete.Interfaces ConcreteSpec.Core ConcreteSpec.StageInterface

/-- The aggregate encoder includes an empty run at its one-past address.
The backing object occupies every physically allocatable byte. -/
private abbrev baseLang := sliceLanguage slice0
local instance : DecidableEq baseLang.operands.Ty := baseLang.typeDecidableEq
private abbrev decl : DeclId := ⟨(bindReturnControl slice0).entry.module, ⟨"T"⟩⟩
private abbrev backingDecl : DeclId := ⟨(bindReturnControl slice0).entry.module, ⟨"Backing"⟩⟩
private abbrev aggregateTy : baseLang.operands.Ty := .nominal decl
private abbrev blockTy : baseLang.operands.Ty := .nominal backingDecl
private abbrev aggregateValue : baseLang.operands.Val := .other decl
private def runs : List (Nat × List Mem.MByte) := [(0, [.init 42 none]), (1, [])]
private abbrev full := 2^64 - 2
private def layout (env : Declarations) (target : Target) (ty : baseLang.operands.Ty) : Option Layout :=
  if ty == aggregateTy then some ⟨1, 1, .nil⟩ else if ty == blockTy then some ⟨full, 1, .nil⟩ else baseLang.layout env target ty
private def literal (ty : baseLang.operands.Ty) (bs : List UInt8) : Option baseLang.operands.Val :=
  if (ty == aggregateTy || ty == blockTy) && bs == [42] then some aggregateValue else baseLang.literal ty bs
private def encode (_ : Target) (ty : baseLang.operands.Ty) (v : DeclId) :=
  if v != decl then none
  else if ty == blockTy then some [(0, [.init 42 none])]
  else if ty == aggregateTy then some runs else none
private def decode (_ : Target) (ty : baseLang.operands.Ty) (bs : List Mem.MByte) : Option DeclId :=
  if ty == blockTy && bs.head? == some (.init 42 none) then some decl
  else if ty == aggregateTy && bs == [.init 42 none] then some decl else none
private def copyTy (ty : baseLang.operands.Ty) := ty == aggregateTy || ty == blockTy || baseLang.operands.isCopy ty
private abbrev Language : CoreLanguage := {sliceLanguage slice0 with
  operands := {baseLang.operands with isCopy := copyTy}
  safety := fun op => match op with
    | .expression expr => baseLang.safety (.expression expr)
    | .primitive prim args => baseLang.safety (.primitive prim args)
    | .call callee args => baseLang.safety (.call callee args)
    | .abort => baseLang.safety .abort
  layout, literal, encodeOther := encode, decodeOther := decode}
private abbrev lowering := sliceLoweringLanguage slice0
private abbrev profile : Profile := ⟨.lcb, .x86_64Linux, 4, 4096⟩
private abbrev State := MemoryRegisters Language profile
private abbrev M := Interfaces.Memory lowering profile
private abbrev exec := Interfaces.Memory.Execution (language := Language) (lang := lowering) (profile := profile)
private abbrev mir := bindReturnControl slice0
private abbrev origin := mir.functions[0].2.2.blocks[0].body[0].origin
private def dst : MemoryPlace _ aggregateTy := ⟨⟨⟨0⟩, []⟩, .materialized⟩
private def backing : PlaceId := ⟨1⟩
private def params : List (Sigma (MemoryPlace Language.operands.Ty)) := [⟨aggregateTy, dst⟩]
private def edge : CFGEdge (Sigma (MemoryRead _ lowering.literal)) := ⟨⟨1⟩, [⟨aggregateTy, .literal ⟨[42]⟩⟩]⟩
private def backingPlace : MemoryPlace _ blockTy := ⟨⟨backing, []⟩, .materialized⟩
private def entry : MemoryBlock lowering :=
  ⟨⟨0⟩, [], [⟨origin, .beginLifetime backing⟩, ⟨origin, .bind backingPlace (.literal ⟨[42]⟩)⟩], .jump edge, []⟩
private def dest : MemoryBlock lowering := ⟨⟨1⟩, params, [], .ret (some ⟨.scalar (.int .u8), .literal ⟨[42]⟩⟩), []⟩
private def physical : M :=
  {project := mir.project, entry := mir.entry, signatures := mir.signatures
   functions := [(mir.entry, [(dst.logical.root, aggregateTy), (backing, blockTy)], ⟨⟨0⟩, #[entry, dest]⟩)]
   storage := [(mir.entry, [(dst.logical.root, .materialized), (backing, .materialized)])]
   layouts := [(mir.entry, [(dst.logical.root, ⟨1, 1, .nil⟩), (backing, ⟨full, 1, .nil⟩)])]}
private def start : State := ⟨⟨mir.entry, ⟨0⟩, 0, [], [], [], [], [], [], []⟩, .empty, [], [], {}, 0, false⟩
private def reserve (s : State) (root : PlaceId) (base size : Nat) : State :=
  let ptr : Mem.Ptr := ⟨⟨0⟩, base, some ⟨s.bytes.next⟩⟩
  {s with live := root :: s.live, materialized := (root, ⟨ptr, size, 1⟩) :: s.materialized
          bytes := ⟨(⟨s.bytes.next⟩, Mem.candidate ⟨0⟩ base size 1) :: s.bytes.blocks, s.bytes.next + 1⟩}
private def openBacking := (reserve start backing 1 full).advance
private def setBacking := (physical.writeValue (language := Language) openBacking backingPlace aggregateValue).getD start
private def atJump := setBacking.advance
private def arrive : State := {atJump with block := edge.target, instruction := 0}
private theorem openStep : exec.step physical start openBacking := by
  with_unfolding_all exact ⟨rfl, ⟨_, rfl, _, ⟨nofun, .inr ⟨rfl, _, rfl, .inr
    ⟨by decide, ⟨0⟩, 1, ⟨0⟩, _, rfl, rfl⟩⟩⟩, rfl⟩, [], rfl⟩

private theorem backingWrite : physical.Writes (language := Language) openBacking backingPlace aggregateValue setBacking := by
  with_unfolding_all
    refine ⟨⟨rfl, nofun⟩, fun _ => ⟨_, rfl, nofun, nofun⟩, rfl, rfl,
      fun _ => .inr ⟨_, [(0, [.init 42 none])], rfl, rfl, fun _ => ⟨0, _, .head _, by decide⟩, ?_⟩⟩
    intro off bs mem
    cases List.mem_singleton.mp mem
    exact ⟨by decide, by decide, rfl⟩

private theorem bindStep : exec.step physical openBacking atJump := by
  with_unfolding_all exact ⟨rfl, ⟨_, rfl, aggregateValue, openBacking, setBacking, rfl, backingWrite, rfl⟩, [], rfl⟩

private theorem spaceFailure : ¬ physical.ParamSpaceFits (language := Language) params arrive := by
  rintro ⟨space, base, id, bytes, alloc, _⟩
  have fits := (Mem.alloc_result _ _ _ _ _ _ _ _ alloc).2.2
  obtain rfl : space = ⟨0⟩ := List.mem_singleton.mp fits.2.2.2.2.2.2
  have nz : base ≠ 0 := fits.2.2.2.2.1 Nat.zero_lt_one
  have bound : base + 1 < 2^64 := fits.2.2.1
  let block : Mem.Block := {Mem.candidate ⟨0⟩ 1 full 1 with bytes := [.init 42 none] ++ List.replicate (full - 1) .uninit}
  apply fits.2.2.2.2.2.1 (⟨0⟩, block) (by with_unfolding_all exact .head _) rfl
  exact ⟨rfl, (by decide : 0 < full), Nat.zero_lt_one, by change 1 < base + 1; omega,
    by change base < 1 + (2^64 - 2); omega⟩

private def opened := reserve arrive dst.logical.root (2^64 - 1) 1
private def ceiling := physical.transferCeiling (language := Language) arrive [dst.logical.root]
private def written := (physical.writeValue (language := Language) opened dst aggregateValue ceiling).getD opened

private theorem valid : physical.ParamsValid (language := Language) params [aggregateValue] arrive := by
  refine ⟨by with_unfolding_all decide, written, Interfaces.Memory.WriteParams.cons (language := Language) (started := opened) ?_ ?_ (.nil _)⟩
  · with_unfolding_all exact ⟨by decide, .inr ⟨rfl, _, rfl, .inr
      ⟨by decide, ⟨0⟩, 2^64 - 1, ⟨1⟩, _, rfl, rfl⟩⟩⟩
  · with_unfolding_all
      refine ⟨⟨rfl, nofun⟩, fun _ => ⟨_, rfl, nofun, nofun⟩, rfl, rfl,
        fun _ => .inr ⟨_, runs, rfl, rfl, fun _ => ⟨0, [.init 42 none], by decide, by decide⟩, ?_⟩⟩
      intro offset bs member
      rcases List.mem_cons.mp member with eq | tail
      · cases eq; exact ⟨by decide, by decide, rfl⟩
      · cases List.mem_singleton.mp tail; exact ⟨by decide, nofun, rfl⟩

private def halted : State := {atJump with resourceExhausted := true}

private theorem exhausts : exec.step physical atJump halted ∧ exec.result physical halted .resourceExhausted :=
  Interfaces.Memory.jump_exhausted_of_no_space (language := Language) (memory := physical) (q := atJump) (entry := entry)
    (edge := edge) (dst := dest) (read := atJump) (values := [aggregateValue]) rfl
    (by with_unfolding_all rfl) rfl rfl (by with_unfolding_all rfl) (by with_unfolding_all rfl)
    rfl valid spaceFailure

private def sourceMir : MIR lowering profile := {mir with
  functions := [(mir.entry, [(dst.logical.root, aggregateTy), (backing, blockTy)], ⟨⟨0⟩, #[
    ⟨⟨0⟩, [], [⟨origin, .bind backingPlace.logical (.literal ⟨[42]⟩)⟩], .jump ⟨⟨1⟩, [⟨aggregateTy, .literal ⟨[42]⟩⟩]⟩, []⟩,
    ⟨⟨1⟩, [⟨aggregateTy, dst.logical⟩], [], .ret (some ⟨.scalar (.int .u8), .literal ⟨[42]⟩⟩), []⟩]⟩)]}
private theorem lowered : lowerGraph (language := Language) sourceMir = .ok physical := by with_unfolding_all rfl

private abbrev source := Interfaces.MIR.Execution (language := Language) (lang := lowering) (profile := profile)
private abbrev logical := sourceMir.logicalView (language := Language)
private def logicalInit : State := {start with
  live := [backing], initialized := [backing], virtualValues := [(backing, aggregateValue)]}
private def logicalJump := logicalInit.advance
private def logicalEntry : State := {logicalJump with block := edge.target, instruction := 0}
private def returned : State := {logicalEntry with
  live := [dst.logical.root, backing], initialized := [dst.logical.root, backing], virtualValues := [(dst.logical.root, aggregateValue), (backing, aggregateValue)]}
private theorem initial_binding : logical.WriteParams (language := Language) [⟨blockTy, {backingPlace with storage := .virtual}⟩] [aggregateValue] start logicalInit := by
  with_unfolding_all
    exact .cons (started := {start with live := [backing]})
      ⟨nofun, .inl ⟨rfl, rfl⟩⟩ ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩ (.nil _)
private theorem jump_bindings : logical.WriteParams (language := Language) [⟨aggregateTy, {dst with storage := .virtual}⟩] [aggregateValue] logicalEntry returned := by
  with_unfolding_all
    exact .cons (started := {logicalEntry with live := [dst.logical.root, backing]})
      ⟨by decide, .inl ⟨rfl, rfl⟩⟩ ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩ (.nil _)
private theorem bind_iff (next : State) : source.step sourceMir start next ↔ next = logicalJump := by
  constructor
  · intro step
    obtain ⟨v, read, stored, hr, writes, rfl⟩ := step.2.1.2
    have eq : some (aggregateValue, start) = some (v, read) := by with_unfolding_all exact hr
    cases eq
    cases writes with
    | cons opened write tail =>
      cases tail
      rcases opened.2 with ⟨_, rfl⟩ | ⟨cls, _⟩
      · exact congrArg MemoryRegisters.advance (Option.some.inj (by with_unfolding_all exact write.2.2.1.symm))
      · cases cls
  · rintro rfl
    with_unfolding_all exact ⟨rfl, ⟨rfl, aggregateValue, start, logicalInit, rfl, initial_binding, rfl⟩, [], rfl⟩

private theorem jump_iff (next : State) : source.step sourceMir logicalJump next ↔ next = returned := by
  let entry := logical.functions[0].2.2.blocks[0]'(by with_unfolding_all decide)
  let dst := logical.functions[0].2.2.blocks[1]'(by with_unfolding_all decide)
  constructor
  · intro step
    obtain ⟨block, hb, _, tick⟩ := step.2.1
    have lookup : some entry = some block := by with_unfolding_all exact hb
    cases lookup
    rw [show entry.ending = .jump edge by with_unfolding_all rfl] at tick
    obtain ⟨vs, read, block, hr, hb, _, writes⟩ := tick
    have args : some ([aggregateValue], logicalJump) = some (vs, read) := by with_unfolding_all exact hr
    cases args
    have found : some dst = some block := by with_unfolding_all exact hb
    cases found
    with_unfolding_all
      change logical.WriteParams (language := Language) [⟨aggregateTy, {AddressCeilingControl.dst with storage := .virtual}⟩]
        [aggregateValue] logicalEntry next at writes
    cases writes with
    | cons opened write tail =>
      cases tail
      rcases opened.2 with ⟨_, rfl⟩ | ⟨cls, _⟩
      · exact Option.some.inj (by with_unfolding_all exact write.2.2.1.symm)
      · cases cls
  · rintro rfl
    with_unfolding_all exact ⟨rfl, ⟨entry, rfl, rfl, _, _, dst, rfl, rfl, rfl, jump_bindings⟩, [], rfl⟩

private theorem sourceResult : source.result sourceMir returned (.returned 42 ByteArray.empty) := by
  with_unfolding_all rfl

private theorem source_cases {s : State} (reached : source.finite sourceMir s) :
    s = start ∨ s = logicalJump ∨ s = returned := by
  apply finite_invariant source sourceMir _ (fun _ h => .inl (by with_unfolding_all exact h)) _ reached
  rintro _ next (rfl | rfl | rfl) tick
  · exact .inr (.inl ((bind_iff _).mp tick))
  · exact .inr (.inr ((jump_iff _).mp tick))
  · exact (source.terminal _ _ _ sourceResult _ tick).elim

private theorem source_safe : ProgramSafe source sourceMir := by
  intro s stuck
  rcases source_cases stuck.1 with rfl | rfl | rfl
  · exact stuck.2.1 _ ((bind_iff _).mpr rfl)
  · exact stuck.2.1 _ ((jump_iff _).mpr rfl)
  · exact stuck.2.2 _ sourceResult

private theorem open_only {next : State} (tick : exec.step physical start next) : next = openBacking := by
  have atBegin {s : State} {mode} (raw : physical.StepOutcome (language := Language) start s mode) :
      physical.InstructionStep (language := Language) start s false (.beginLifetime backing) mode :=
    raw.instructionAt (language := Language) (memory := physical) (q := start) (block := entry) (by with_unfolding_all rfl) (by with_unfolding_all rfl)
  have begins : physical.Begins (language := Language) start backing (reserve start backing 1 full) := by
    with_unfolding_all exact ⟨nofun, .inr ⟨rfl, _, rfl, .inr ⟨by decide, ⟨0⟩, 1, ⟨0⟩, _, rfl, rfl⟩⟩⟩
  obtain ⟨s, fresh, rfl⟩ := atBegin (Interfaces.Memory.Step.normal tick.2.1
    fun _ raw => (atBegin raw).unavailable ⟨_, begins⟩)
  rcases fresh.2 with ⟨cls, _⟩ | ⟨_, l, hl, zero | placed⟩
  · cases cls
  all_goals have shape : some (⟨full, 1, .nil⟩ : Layout) = some l := by with_unfolding_all exact hl
  all_goals cases shape
  · exact ((by decide : full ≠ 0) zero.1).elim
  obtain ⟨_, space, base, id, bytes, alloc, rfl⟩ := placed
  obtain ⟨rfl, rfl, fits⟩ := Mem.alloc_result _ _ _ _ _ _ _ _ alloc
  obtain rfl : space = ⟨0⟩ := List.mem_singleton.mp fits.2.2.2.2.2.2
  have nz : base ≠ 0 := fits.2.2.2.2.1 (by decide : 0 < full)
  have cap : base + (2^64 - 2) < 2^64 := fits.2.2.1
  obtain rfl : base = 1 := by omega
  rfl

private theorem bind_only {next : State} (tick : exec.step physical openBacking next) : next = atJump := by
  have atBind {s : State} {mode} (raw : physical.StepOutcome (language := Language) openBacking s mode) :
      physical.InstructionStep (language := Language) openBacking s false (.bind backingPlace (.literal ⟨[42]⟩)) mode :=
    raw.instructionAt (language := Language) (memory := physical) (q := openBacking) (block := entry) (by with_unfolding_all rfl) (by with_unfolding_all rfl)
  obtain ⟨v, read, stored, readEq, write, rfl⟩ := atBind (Interfaces.Memory.Step.normal tick.2.1
    fun _ raw => Interfaces.Memory.bind_cannot_exhaust (atBind raw))
  have args : some (aggregateValue, openBacking) = some (v, read) := by with_unfolding_all exact readEq
  cases args
  exact congrArg MemoryRegisters.advance (Option.some.inj (write.2.2.1.symm.trans backingWrite.2.2.1))

private theorem jump_only {next : State} (tick : exec.step physical atJump next) : next = halted := by
  have step : physical.Step (language := Language) atJump next := tick.2.1
  unfold Interfaces.Memory.Step at step
  rw [show atJump.resourceExhausted = false from rfl] at step
  simp only [Bool.false_eq_true, ite_false] at step
  split at step
  · exact step.1
  have jump := step.terminatorAt (language := Language) (memory := physical) (q := atJump) (block := entry) (by with_unfolding_all rfl) (by with_unfolding_all rfl)
  change physical.Transfer (language := Language) edge atJump next at jump
  obtain ⟨vs, read, block, args, found, _, writes⟩ := jump
  cases (by with_unfolding_all exact args : some ([aggregateValue], atJump) = some (vs, read))
  cases (by with_unfolding_all exact found : some dest = some block)
  change physical.WriteParams (language := Language) params [aggregateValue] arrive next at writes
  cases writes with
  | cons opened _ _ =>
    rcases opened.2 with ⟨cls, _⟩ | ⟨_, l, hl, zero | placed⟩
    · cases cls
    all_goals have shape : some (⟨1, 1, .nil⟩ : Layout) = some l := by with_unfolding_all exact hl
    all_goals cases shape
    · exact (Nat.one_ne_zero zero.1).elim
    · obtain ⟨_, sp, b, id, bs, alloc, _⟩ := placed
      exact (spaceFailure ⟨sp, b, id, bs, alloc, trivial⟩).elim

private theorem target_cases {s : State} (reached : exec.finite physical s) :
    s = start ∨ s = openBacking ∨ s = atJump ∨ s = halted := by
  apply finite_invariant exec physical _ (fun _ h => .inl (by with_unfolding_all exact h)) _ reached
  rintro _ next (rfl | rfl | rfl | rfl) tick
  · exact .inr (.inl (open_only tick))
  · exact .inr (.inr (.inl (bind_only tick)))
  · exact .inr (.inr (.inr (jump_only tick)))
  · exact (exec.terminal _ _ _ exhausts.2 _ tick).elim

private theorem related_start : Related (language := Language) physical start start := by
  with_unfolding_all
    exact ⟨⟨⟨⟨⟨Mem.empty_wellFormed _, .nil, .nil, nofun⟩, rfl⟩, rfl, nofun, .nil, .nil,
      .nil, .nil, nofun, fun _ => ⟨nofun, nofun⟩, nofun, nofun, nofun, nofun⟩,
      rfl, nofun, ⟨rfl, nofun⟩, .nil, rfl, rfl⟩,
      ⟨rfl, rfl, _, _, rfl, rfl, by decide, rfl⟩, nofun, rfl⟩

private theorem related_open : Related (language := Language) physical start openBacking := by
  have wf : openBacking.bytes.WellFormed profile.memoryTarget :=
    Mem.alloc_wellFormed profile.memoryTarget .empty (Mem.empty_wellFormed _) ⟨0⟩ 1 full 1
  with_unfolding_all
    refine ⟨⟨⟨⟨⟨wf, by decide, .nil, nofun⟩, rfl⟩, rfl, nofun, .nil, by decide, .nil, .nil,
      nofun, fun _ => ⟨nofun, nofun⟩, nofun, nofun, ?_, ?_⟩, rfl, nofun,
      ⟨rfl, nofun⟩, .nil, rfl, rfl⟩, ⟨rfl, rfl, _, _, rfl, rfl, by decide, rfl⟩, nofun, rfl⟩
  · intro root range member
    cases List.mem_singleton.mp member
    exact ⟨.head _, rfl, _, _, _, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩
  · intro r h
    cases List.mem_singleton.mp h
    with_unfolding_all exact ⟨_, _, blockTy, .materialized, .head _, rfl, rfl, fun _ => ⟨_, rfl, nofun, nofun⟩,
      fun _ => nofun⟩

private theorem related_bound : Related (language := Language) physical logicalJump atJump := by
  let ptr : Mem.Ptr := ⟨⟨0⟩, 1, some ⟨0⟩⟩
  have before : openBacking.bytes.WellFormed profile.memoryTarget :=
    Mem.alloc_wellFormed profile.memoryTarget .empty (Mem.empty_wellFormed _) ⟨0⟩ 1 full 1
  have wf : atJump.bytes.WellFormed profile.memoryTarget :=
    by with_unfolding_all exact Mem.store_wellFormed profile.memoryTarget openBacking.bytes before ptr [.init 42 none] 1
  with_unfolding_all
    refine ⟨⟨⟨⟨⟨wf, by decide, by decide, fun _ h => h⟩, rfl⟩, rfl, fun _ h => h, by decide,
      by decide, .nil, .nil, nofun, ?_, ?_, nofun, ?_, ?_⟩, rfl, nofun, ⟨rfl, nofun⟩,
      .nil, rfl, rfl⟩, ⟨rfl, rfl, _, _, rfl, rfl, by decide, rfl⟩, nofun, rfl⟩
  · intro r
    change r ∈ [backing] ↔ (AList.lookup [(backing, aggregateValue)] r).isSome
    by_cases eq : backing = r <;> simp_all [AList.lookup, eq_comm]
  · intro r v h
    cases List.mem_singleton.mp h
    exact .head _
  · intro r range h
    cases List.mem_singleton.mp h
    exact ⟨.head _, rfl, _, _, _, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩
  · intro r h
    cases List.mem_singleton.mp h
    refine ⟨_, _, blockTy, .materialized, .head _, rfl, rfl, fun _ => ⟨_, rfl, nofun, nofun⟩, ?_⟩
    intro v found
    cases (by with_unfolding_all exact found : some aggregateValue = some v)
    exact ⟨aggregateValue, rfl, backingWrite.2.2.2⟩

private theorem correspondence : LocalCorrespondence (language := Language) sourceMir physical := by
  constructor
  · intro state initial
    obtain rfl : state = start := by with_unfolding_all exact initial
    exact ⟨start, by with_unfolding_all rfl, related_start⟩
  · intro logicalState s next hp hs rel tick running
    have src := source_cases hp
    rcases target_cases hs with rfl | rfl | rfl | rfl
    · cases open_only tick
      rcases src with rfl | rfl | rfl
      · exact .inr ⟨related_open, entry, _, backing, by with_unfolding_all rfl, rfl, rfl⟩
      · exact absurd (rel.active.unique_pc related_start.active) (by decide)
      · exact absurd rel.active.2.1 (by decide)
    · cases bind_only tick
      rcases src with rfl | rfl | rfl
      · exact .inl ⟨_, (bind_iff _).mpr rfl, related_bound⟩
      · exact absurd (rel.active.unique_pc related_open.active) (by decide)
      · exact absurd rel.active.2.1 (by decide)
    · cases jump_only tick
      cases running
    · exact (exec.terminal _ _ _ exhausts.2 _ tick).elim
  · intro _ s o _ hs _ obs
    rcases target_cases hs with rfl | rfl | rfl | rfl
    · exact (exec.terminal _ _ _ (Interfaces.Memory.matchingResult_result obs) _ openStep).elim
    · exact (exec.terminal _ _ _ (Interfaces.Memory.matchingResult_result obs) _ bindStep).elim
    · exact (exec.terminal _ _ _ (Interfaces.Memory.matchingResult_result obs) _ exhausts.1).elim
    · exact False.elim obs
  · intro _ s _ hs _
    rcases target_cases hs with rfl | rfl | rfl | rfl
    · exact .inr ⟨_, openStep⟩
    · exact .inr ⟨_, bindStep⟩
    · exact .inr ⟨_, exhausts.1⟩
    · exact .inl ⟨_, exhausts.2⟩

/-- The safe MIR returns 42. Its actual lowering exhausts when the trailing
empty run lies at 2^64 and satisfies the full frozen lowering contract. -/
theorem trailing_empty_run_refines :
    ProgramSafe source sourceMir ∧ source.observes sourceMir (.returned 42 ByteArray.empty) ∧
    lowerGraph (language := Language) sourceMir = .ok physical ∧ exec.observes physical .resourceExhausted ∧
    LowersMemory (language := Language) sourceMir physical := by
  letI := Language.typeDecidableEq
  refine ⟨source_safe, ⟨returned, ⟨start, by with_unfolding_all rfl,
    .cons ((bind_iff _).mpr rfl) (.cons ((jump_iff _).mpr rfl) (.nil _))⟩, by with_unfolding_all rfl⟩,
    lowered, ⟨halted, ⟨start, by with_unfolding_all rfl,
      .cons openStep (.cons bindStep (.cons exhausts.1 (.nil _)))⟩, exhausts.2⟩,
    lowerGraph_refines lowered ((checkHandoff_iff _).mp (by with_unfolding_all decide)) (fun _ => correspondence)⟩

end Concrete.Memory.AddressCeilingControl
