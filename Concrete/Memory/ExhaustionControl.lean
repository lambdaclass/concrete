import Concrete.Memory.Refinement

namespace Concrete.Memory

open Concrete.Interfaces ConcreteSpec.Core ConcreteSpec.StageInterface

namespace RecursiveControl

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
private def start : State := ⟨⟨mir.entry, ⟨0⟩, 0, [], [], [], [], [], [], []⟩, .empty, [], [], {}, 0, false⟩
private def opened : State := {start with live := [⟨0⟩]}
private def value : Language.operands.Val := .scalar (.int .u8 42)
private def stored : State := {opened with initialized := [⟨0⟩], virtualValues := [(⟨0⟩, value)]}
private abbrev finalL : State := stored.advance
private def recurDest : Option (Sigma (Place Language.operands.Ty)) := some ⟨u8Ty, root.logical⟩

/-- The unreachable read takes the local’s address. The reachable body binds one byte
and recurses with the caller still live. -/
private def recurMIR : MIR Lowering profile :=
  let original := mir.functions[0].2.2.blocks[0]
  let addrOp : MIRInst Lowering := .op (opcode := .read) root.logical ()
    (.cons (.literal (ty := u8Ty) ⟨[42]⟩) .nil) [] [⟨u8Ty, root.logical⟩]
  {mir with functions := [(mir.entry, [(⟨0⟩, u8Ty)],
    ⟨⟨0⟩, #[{original with body := original.body ++ [⟨originalOp.origin, .call mir.entry recurDest [] []⟩]},
      {original with id := ⟨1⟩, body := [⟨originalOp.origin, addrOp⟩]}]⟩)]}

private theorem registered : recurMIR.logicalView.HandoffWF := by
  letI := Language.typeDecidableEq
  exact (checkHandoff_iff _).mp (by with_unfolding_all decide)

private abbrev Stack := List (MemoryContinuation Language.operands.Ty Language.operands.Val)
private def logicalEntry (k : Stack) : State := {start with callers := k}
private def logicalBound (k : Stack) : State := {finalL with callers := k}
private def pushed (k : Stack) : Stack := ⟨(logicalBound k).advance.toMemoryFrame, some ⟨u8Ty, root⟩⟩ :: k

private theorem logicalBind (k : Stack) : source.step recurMIR (logicalEntry k) (logicalBound k) := by
  with_unfolding_all
    refine ⟨rfl, ⟨rfl, value, logicalEntry k, {stored with callers := k}, rfl, ?_, rfl⟩, [], rfl⟩
    exact .cons (started := {opened with callers := k}) (stored := {stored with callers := k})
      ⟨nofun, Or.inl ⟨rfl, rfl⟩⟩ ⟨⟨rfl, fun _ => rfl⟩, nofun, rfl, rfl, nofun⟩ (.nil _)

private theorem logicalCall (k : Stack) : source.step recurMIR (logicalBound k) (logicalEntry (pushed k)) := by
  with_unfolding_all
    exact ⟨rfl, ⟨rfl, rfl, [], _, _, _, rfl, rfl, rfl, rfl, .nil _⟩, [], rfl⟩

private theorem logicalBindOnly (k : Stack) {q : State} (tick : source.step recurMIR (logicalEntry k) q) : q = logicalBound k := by
  have step := tick.2.1
  change _ ∧ recurMIR.logicalView.InstructionStep (logicalEntry k) q true (.bind root (.literal ⟨[42]⟩)) at step
  obtain ⟨v, afterRead, s, read, writes, rfl⟩ := step.2
  have e : some (value, logicalEntry k) = some (v, afterRead) := by with_unfolding_all exact read
  cases e
  cases writes with
  | cons openedHere written tail =>
    cases tail
    rcases openedHere.2 with ⟨_, rfl⟩ | ⟨materialized, _⟩
    · have eq : some ({stored with callers := k} : State) = some s := by with_unfolding_all exact written.2.2.1
      cases eq
      rfl
    · have no : some StorageClass.virtual = some StorageClass.materialized := by with_unfolding_all exact materialized
      contradiction

private theorem logicalCallOnly (k : Stack) {q : State} (tick : source.step recurMIR (logicalBound k) q) : q = logicalEntry (pushed k) := by
  have step := tick.2.1
  change _ ∧ _ ∧ recurMIR.logicalView.Call mir.entry (some ⟨u8Ty, root⟩) [] (logicalBound k) q at step
  obtain ⟨_, _, vals, s, g, e, hr, hg, he, _, params⟩ := step
  have vr : some ([], logicalBound k) = some (vals, s) := by with_unfolding_all exact hr
  cases vr
  let graph := recurMIR.logicalView.functions[0].2.2
  have gr : some graph = some g := by with_unfolding_all exact hg
  have geq : g = graph := (Option.some.inj gr).symm
  subst g
  let entry := graph.blocks[0]'(by with_unfolding_all decide)
  have er : some entry = some e := by with_unfolding_all exact he
  have eeq : e = entry := (Option.some.inj er).symm
  subst e
  have ep : entry.params = [] := by with_unfolding_all rfl
  rw [ep] at params
  cases params
  with_unfolding_all rfl

private theorem safe : ProgramSafe source recurMIR := by
  intro q stuck
  have casesP : (∃ k, q = logicalEntry k) ∨ (∃ k, q = logicalBound k) := by
    apply finite_invariant source recurMIR _ (fun _ h => .inl ⟨[], by with_unfolding_all exact h⟩) _ stuck.1
    rintro _ s (⟨k, rfl⟩ | ⟨k, rfl⟩) tick
    · exact .inr ⟨k, logicalBindOnly k tick⟩
    · exact .inl ⟨_, logicalCallOnly k tick⟩
  rcases casesP with ⟨k, rfl⟩ | ⟨k, rfl⟩
  · exact stuck.2.1 _ (logicalBind k)
  · exact stuck.2.1 _ (logicalCall k)

private def slot : MemoryPlace _ u8Ty := ⟨root.logical, .materialized⟩
private def physical : M :=
  let bind : MemoryInst Lowering := .bind slot (.literal ⟨[42]⟩)
  let call : MemoryInst Lowering := .call mir.entry (some ⟨u8Ty, slot⟩) [] []
  let addr : MemoryInst Lowering := .op (opcode := .read) slot ()
    (.cons (.literal (ty := u8Ty) ⟨[42]⟩) .nil) [] [⟨u8Ty, slot⟩]
  let body (ops : List (MemoryInst Lowering)) := ops.map fun op => ⟨originalOp.origin, op⟩
  let ret : MemoryTerm Lowering := .ret (some ⟨u8Ty, .copy slot⟩)
  {project := mir.project, entry := mir.entry, signatures := mir.signatures
   functions := [(mir.entry, [(⟨0⟩, u8Ty)], ⟨⟨0⟩, #[
     ⟨⟨0⟩, [], body [.beginLifetime ⟨0⟩, bind, call], ret, []⟩,
     ⟨⟨1⟩, [], body [.beginLifetime ⟨0⟩, addr], ret, []⟩]⟩)]
   storage := [(mir.entry, [(⟨0⟩, .materialized)])], layouts := [(mir.entry, [(⟨0⟩, ⟨1, 1⟩)])]}

private theorem lowered : lowerGraph (language := Language) recurMIR = .ok physical := by with_unfolding_all rfl

/-- Address zero is null and the last address must fit the one-past pointer. -/
private def capacity := 2 ^ profile.memoryTarget.addrBits - 2

private def candidate (n : Nat) := Mem.candidate ⟨0⟩ (n+1) 1 1
private def written (n : Nat) : Mem.Block := {candidate n with bytes := [.init 42 none]}
private def blocks : Nat → AList ConcreteSpec.Core.BlockId Mem.Block
  | 0 => []
  | n+1 => (⟨n⟩, written n) :: blocks n

private theorem mem_blocks {n : Nat} {e} :
    e ∈ blocks n ↔ ∃ i < n, e = (⟨i⟩, written i) := by
  induction n with
  | zero => simp [blocks]
  | succ n ih =>
    rw [blocks, List.mem_cons, ih]
    constructor
    · rintro (rfl | ⟨i, hi, rfl⟩)
      · exact ⟨n, by omega, rfl⟩
      · exact ⟨i, by omega, rfl⟩
    · rintro ⟨i, hi, rfl⟩
      rcases Nat.lt_or_eq_of_le (Nat.le_of_lt_succ hi) with h | rfl
      · exact .inr ⟨i, h, rfl⟩
      · exact .inl rfl

private def range (n : Nat) : Hybrid.Range := ⟨⟨⟨0⟩, n+1, some ⟨n⟩⟩, 1, 1⟩
private def boundFrame (n : Nat) : MemoryFrame Language.operands.Val :=
  {start.toMemoryFrame with live := [⟨0⟩], materialized := [(⟨0⟩, range n)], initialized := [⟨0⟩], instruction := 2}
private def callers : Nat → Stack
  | 0 => []
  | n+1 => ⟨{boundFrame n with instruction := 3}, some ⟨u8Ty, slot⟩⟩ :: callers n

private def entry (n : Nat) : State := {start with bytes := ⟨blocks n, n⟩, callers := callers n}
private def allocated (n : Nat) : State :=
  {entry n with live := [⟨0⟩], materialized := [(⟨0⟩, range n)], instruction := 1
                bytes := ⟨(⟨n⟩, candidate n) :: blocks n, n+1⟩}
private def bound (n : Nat) : State :=
  {entry n with toMemoryFrame := boundFrame n, bytes := ⟨blocks (n+1), n+1⟩}

private theorem allocation_fits (n : Nat) (h : n < capacity) :
    (entry n).bytes.Fits profile.memoryTarget (candidate n) := by
  refine ⟨(by decide : IsPow2 1), Nat.mod_one _, ?_, (by decide : 1 ≤ profile.memoryTarget.maxObject),
    fun _ => Nat.succ_ne_zero n, ?_, (by decide : (⟨0⟩ : AddrSpace) ∈ profile.memoryTarget.spaces)⟩
  · change n + 1 + 1 ≤ 2 ^ 64 - 1
    change n < 2 ^ 64 - 2 at h
    omega
  · intro e member _ overlap
    obtain ⟨i, hi, rfl⟩ := mem_blocks.mp member
    have above := overlap.2.2.2.2
    change n+1 < i+1+1 at above
    omega

private theorem begin_at (n : Nat) (h : n < capacity) : target.step physical (entry n) (allocated n) := by
  with_unfolding_all
    refine ⟨rfl, ⟨_, rfl, {allocated n with instruction := 0}, ⟨nofun, Or.inr ?_⟩, rfl⟩, [], rfl⟩
    refine ⟨rfl, ⟨1, 1⟩, rfl, Or.inr ⟨by decide, ⟨0⟩, n+1, ⟨n⟩, _, ?_, rfl⟩⟩
    unfold Mem.Memory.alloc
    exact ite_eq_left (allocation_fits n h)

private theorem writes_byte (n : Nat) :
    physical.Writes (allocated n) slot value {bound n with instruction := 1} := by
  have access (bytes : List Mem.MByte) : (⟨(⟨n⟩, {candidate n with bytes}) :: blocks n, n+1⟩ : Mem.Memory).access
      profile.memoryTarget (range n).ptr 1 1 = some (some (⟨n⟩, 0)) := by
    simp [range, Mem.Memory.access, Mem.Memory.block, AList.lookup, candidate, Mem.candidate, Nat.mod_one]
  have loaded : (bound n).bytes.load profile.memoryTarget (range n).ptr 1 1 = some [.init 42 none] := by
    unfold Mem.Memory.load
    rw [show (bound n).bytes.access profile.memoryTarget (range n).ptr 1 1 = _ from access [.init 42 none]]
    simp [bound, blocks, written, Mem.Memory.block, AList.lookup]
  with_unfolding_all
    refine ⟨⟨rfl, fun _ => rfl⟩, fun _ => ⟨⟨1, 1⟩, rfl, ⟨nofun, nofun⟩⟩, ?_, ?_,
      fun _ => Or.inr ⟨range n, [(0, [.init 42 none])], rfl, rfl, ?_, ?_⟩⟩
    · have encoded : Hybrid.encode profile.memoryTarget (.int .u8 42) = some [.init 42 none] := rfl
      simp [Interfaces.Memory.writeValue, Interfaces.Memory.mayInitialize, Interfaces.Memory.authorizeWrite,
        Interfaces.Memory.writeOwnerView, Interfaces.Memory.ownerView, Interfaces.Memory.zeroRoot,
        Interfaces.Memory.rangeAt, Interfaces.Memory.byteRuns, Interfaces.Memory.layoutAt, physical,
        allocated, bound, entry, boundFrame, blocks, slot, root, value, written, range, candidate,
        Mem.Memory.store, Mem.Memory.access, Mem.Memory.block, Mem.Memory.modify, AList.lookup, AList.update,
        Mem.candidate, Nat.mod_one, start, encoded] <;> rfl
    · change (do
        let bs ← (bound n).bytes.load profile.memoryTarget (range n).ptr 1 1
        let v ← Hybrid.decode profile.memoryTarget (.int .u8) bs
        pure (CoreValue.scalar v : Language.operands.Val)) = some value
      rw [loaded]
      rfl
    · exact fun _ => ⟨0, [.init 42 none], .head _, by decide⟩
    · intro offset bs member
      cases List.mem_singleton.mp member
      exact ⟨Nat.le_refl 1, fun byte hb => by cases List.mem_singleton.mp hb; decide, loaded⟩

private theorem next_call (n : Nat) (h : n < capacity) (reached : target.finite physical (entry n)) :
    target.finite physical (entry (n+1)) := by
  have bind : target.step physical (allocated n) (bound n) := by
    with_unfolding_all exact ⟨rfl, ⟨_, rfl, value, allocated n, _, rfl, writes_byte n, rfl⟩, [], rfl⟩
  have call : target.step physical (bound n) (entry (n+1)) := by
    with_unfolding_all exact ⟨rfl, ⟨_, rfl, rfl, [], _, _, _, rfl, rfl, rfl, rfl, .nil _⟩, [], rfl⟩
  exact finite_next (finite_next (finite_next reached (begin_at n h)) bind) call

private theorem full : physical.BeginExhausted (entry capacity) ⟨0⟩ := by
  refine ⟨nofun, by with_unfolding_all rfl, ⟨⟨1, 1⟩, by with_unfolding_all rfl, by decide⟩, ?_⟩
  rintro ⟨_, _, ⟨hv, _⟩ | ⟨_, layout, hl, zero | alloc⟩⟩
  · have no : some StorageClass.materialized = some .virtual := by with_unfolding_all exact hv
    contradiction
  all_goals have eq : some (⟨1, 1⟩ : Layout) = some layout := by with_unfolding_all exact hl
  all_goals cases eq
  · exact Nat.one_ne_zero zero.1
  · obtain ⟨_, space, base, _, _, chosen, _⟩ := alloc
    unfold Mem.Memory.alloc at chosen
    dsimp only at chosen
    split at chosen
    · rename_i fits
      have spaceEq : space = ⟨0⟩ := List.mem_singleton.mp fits.2.2.2.2.2.2
      subst space
      have nonnull : base ≠ 0 := fits.2.2.2.2.1 Nat.zero_lt_one
      have hb : base + 1 ≤ 2 ^ 64 - 1 := fits.2.2.1
      obtain ⟨i, rfl⟩ := Nat.exists_eq_succ_of_ne_zero nonnull
      have hi : i < capacity := by change i < 2 ^ 64 - 2; omega
      have collides := fits.2.2.2.2.2.1 (⟨i⟩, written i) (mem_blocks.mpr ⟨i, hi, rfl⟩) rfl
      exact collides ⟨rfl, Nat.zero_lt_one, Nat.zero_lt_one, Nat.lt_succ_self _, Nat.lt_succ_self _⟩
    · contradiction

/-- Each call retains its initialized byte. For this legal allocation schedule
the next entry reaches a typed terminal after every usable u8 address is occupied. -/
theorem recursive_exhaustion : recurMIR.logicalView.HandoffWF ∧ ProgramSafe source recurMIR ∧
    ∃ m, lowerGraph (language := Language) recurMIR = .ok m ∧ target.observes m .resourceExhausted := by
  have reach (n : Nat) (le : n ≤ capacity) : target.finite physical (entry n) := by
    induction n with
    | zero => exact ⟨_, by with_unfolding_all rfl, .nil _⟩
    | succ n ih => exact next_call n (by omega) (ih (by omega))
  let function := physical.functions[0]
  let first := function.2.2.blocks[0]'(by with_unfolding_all decide)
  have progress := begin_progress (memory := physical) (q := entry capacity) (block := first)
    (root := ⟨0⟩) rfl (by with_unfolding_all rfl) (by with_unfolding_all rfl) (by with_unfolding_all rfl)
    full.fresh (.inr ⟨full.materialized, ⟨⟨1, 1⟩, by with_unfolding_all rfl⟩⟩)
  rcases progress with ⟨s, began, _⟩ | ⟨stop, outcome⟩
  · exact (full.unavailable ⟨s, began⟩).elim
  · exact ⟨registered, safe, physical, lowered, _, finite_next (reach _ (Nat.le_refl _)) stop, outcome⟩

end RecursiveControl
end Concrete.Memory
