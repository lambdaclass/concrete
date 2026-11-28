import Concrete.Memory.Refinement
import Concrete.Memory.TransferProgress

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

private abbrev u8Ty : Language.operands.Ty := .scalar (.int .u8)
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
   storage := [(mir.entry, [(⟨0⟩, .materialized)])], layouts := [(mir.entry, [(⟨0⟩, ⟨1, 1, .nil⟩)])]}

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

private theorem allocation_fits (n ceiling : Nat)
    (h : n + 2 < ceiling) :
    (entry n).bytes.Fits profile.memoryTarget (candidate n) ceiling := by
  refine ⟨(by decide : IsPow2 1), Nat.mod_one _, ?_, (by decide : 1 ≤ profile.memoryTarget.maxObject),
    fun _ => Nat.succ_ne_zero n, ?_, (by decide : (⟨0⟩ : AddrSpace) ∈ profile.memoryTarget.spaces)⟩
  · exact h
  · intro e member _ overlap
    obtain ⟨i, hi, rfl⟩ := mem_blocks.mp member
    have above := overlap.2.2.2.2
    change n+1 < i+1+1 at above
    omega

private theorem begin_at (n : Nat) (h : n < capacity) : target.step physical (entry n) (allocated n) := by
  with_unfolding_all
    refine ⟨rfl, ⟨_, rfl, {allocated n with instruction := 0}, ⟨nofun, Or.inr ?_⟩, rfl⟩, [], rfl⟩
    refine ⟨rfl, ⟨1, 1, .nil⟩, rfl, Or.inr ⟨by decide, ⟨0⟩, n+1, ⟨n⟩, _, ?_, rfl⟩⟩
    unfold Mem.Memory.alloc
    exact ite_eq_left (allocation_fits n _ (by change n < 2^64 - 2 at h; change n + 2 < 2^64; omega))

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
    refine ⟨⟨rfl, fun _ => rfl⟩, fun _ => ⟨⟨1, 1, .nil⟩, rfl, ⟨nofun, nofun⟩⟩, ?_, ?_,
      fun _ => Or.inr ⟨range n, [(0, [.init 42 none])], rfl, rfl, ?_, ?_⟩⟩
    · have encoded : Hybrid.encode profile.memoryTarget (.int .u8 42) = some [.init 42 none] := rfl
      simp [Interfaces.Memory.writeValue, Interfaces.Memory.markWritten, Interfaces.Memory.storeRuns, Interfaces.Memory.atOffset, Interfaces.Memory.mayInitialize, Interfaces.Memory.authorizeWrite,
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
  constructor
  · with_unfolding_all
      refine ⟨{allocated capacity with instruction := 0}, nofun, .inr ⟨rfl, ⟨1, 1, .nil⟩, rfl, .inr
        ⟨by decide, ⟨0⟩, capacity + 1, ⟨capacity⟩, _, ?_, rfl⟩⟩⟩
      unfold Mem.Memory.alloc
      exact ite_eq_left (allocation_fits capacity _ (by with_unfolding_all decide))
  · rintro ⟨_, _, ⟨hv, _⟩ | ⟨_, layout, hl, zero | alloc⟩⟩
    · have no : some StorageClass.materialized = some .virtual := by with_unfolding_all exact hv
      contradiction
    all_goals have eq : some (⟨1, 1, .nil⟩ : Layout) = some layout := by with_unfolding_all exact hl
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
        have hb : base + 1 < 2 ^ 64 := fits.2.2.1
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
    full.valid
  rcases progress with ⟨s, began, _⟩ | ⟨stop, outcome⟩
  · exact (full.unavailable ⟨s, began⟩).elim
  · exact ⟨registered, safe, physical, lowered, _, finite_next (reach _ (Nat.le_refl _)) stop, outcome⟩

/-! ### Suspended SSA/BIR caller exhaustion (ISS-0119)

Each recursive SSA/BIR activation opens its materialized `u8` local, stores `42`,
and suspends waiting for its virtual call destination `regRet` while keeping its
materialized byte live on the heap. After `capacity` suspended callers occupy
every usable 1-byte address, the next `beginLifetime` exhausts atomically in both
`SSA.Execution` and `BIR.Execution`, retaining all `capacity` suspended callers. -/

private abbrev ssaProfile : Profile := ⟨.lcb, .x86_64Linux, 1, 4096⟩
private abbrev SSAState := MemoryRegisters Language ssaProfile
private abbrev ssaExec := Interfaces.SSA.Execution (language := Language) (lang := Lowering) (profile := ssaProfile)
private abbrev birExec := Interfaces.BIR.Execution (language := Language) (lang := Lowering) (profile := ssaProfile)

private def regRet : SSAValue Language.operands.Ty u8Ty := ⟨0⟩
private def regDest : MemoryPlace Language.operands.Ty u8Ty := ⟨⟨⟨1⟩, []⟩, .virtual⟩

private def regBlock {Term : Type} (ending : Term) :
    Block (Sigma (SSAValue Language.operands.Ty)) (LocatedOp (SSAInst Lowering)) Term :=
  ⟨⟨0⟩, [],
    [⟨originalOp.origin, .beginLifetime ⟨0⟩⟩,
     ⟨originalOp.origin, .store root.logical (.literal ⟨[42]⟩)⟩,
     ⟨originalOp.origin, .call mir.entry (some ⟨u8Ty, regRet⟩) []⟩],
    ending, []⟩

private def recurSSA : SSA Lowering ssaProfile :=
  { project := mir.project, entry := mir.entry, signatures := mir.signatures
    functions := [(mir.entry, [(⟨0⟩, u8Ty)], ⟨⟨0⟩, #[regBlock (.ret (some ⟨u8Ty, .value regRet⟩))]⟩)]
    layouts := [(mir.entry, [(⟨0⟩, ⟨1, 1, .nil⟩)])] }

private def recurBIR : BIR Lowering ssaProfile :=
  { project := mir.project, entry := mir.entry, signatures := mir.signatures
    functions := [(mir.entry, [(⟨0⟩, u8Ty)], ⟨⟨0⟩, #[regBlock (.ret (some ⟨u8Ty, .value regRet⟩))]⟩)]
    layouts := [(mir.entry, [(⟨0⟩, ⟨1, 1, .nil⟩)])]
    sites := #[] }

private abbrev ssaView := recurSSA.memoryView (language := Language)

private def ssaViewNorm : Interfaces.Memory Lowering ssaProfile :=
  { project := mir.project, entry := mir.entry, signatures := mir.signatures
    functions := [(mir.entry, [(⟨0⟩, u8Ty), (⟨1⟩, u8Ty)], ⟨⟨0⟩, #[
      ⟨⟨0⟩, [],
        [⟨originalOp.origin, .beginLifetime ⟨0⟩⟩,
         ⟨originalOp.origin, .assign slot (.literal ⟨[42]⟩)⟩,
         ⟨originalOp.origin, .call mir.entry (some ⟨u8Ty, regDest⟩) [] []⟩],
        .ret (some ⟨u8Ty, .copy regDest⟩), []⟩]⟩)]
    layouts := [(mir.entry, [(⟨0⟩, ⟨1, 1, .nil⟩)])]
    storage := [(mir.entry, [(⟨0⟩, .materialized), (⟨1⟩, .virtual)])]
    mutableLocals := [(mir.entry, [⟨1⟩])] }

private theorem ssaView_eq : ssaView = ssaViewNorm := by with_unfolding_all rfl
private theorem birView_eq : recurBIR.memoryView (language := Language) = ssaViewNorm := by with_unfolding_all rfl
private theorem ssa_bir_view_eq :
    recurSSA.memoryView (language := Language) = recurBIR.memoryView (language := Language) :=
  ssaView_eq.trans birView_eq.symm

private def ssaStart : SSAState :=
  ⟨⟨mir.entry, ⟨0⟩, 0, [], [], [], [], [], [], []⟩, .empty, [], [], {}, 0, false⟩

private def ssaCallers : Nat → Stack
  | 0 => []
  | n+1 => ⟨{boundFrame n with instruction := 3}, some ⟨u8Ty, regDest⟩⟩ :: ssaCallers n

private theorem ssaCallers_length (n : Nat) : (ssaCallers n).length = n := by
  induction n with
  | zero => rfl
  | succ n ih => exact congrArg (· + 1) ih

private def ssaEntry (n : Nat) : SSAState :=
  {ssaStart with bytes := ⟨blocks n, n⟩, callers := ssaCallers n}
private def ssaAllocated (n : Nat) : SSAState :=
  {ssaEntry n with live := [⟨0⟩], materialized := [(⟨0⟩, range n)], instruction := 1
                   bytes := ⟨(⟨n⟩, candidate n) :: blocks n, n+1⟩}
private def ssaBound (n : Nat) : SSAState :=
  {ssaEntry n with toMemoryFrame := boundFrame n, bytes := ⟨blocks (n+1), n+1⟩}

private theorem ssa_wf : recurSSA.WF (language := Language) := by
  letI := Language.typeDecidableEq
  exact (checkHandoff_iff _).mp (by with_unfolding_all decide)

private theorem bir_wf : recurBIR.WF (language := Language) := by
  refine ⟨?_, by with_unfolding_all decide, by with_unfolding_all decide,
    by with_unfolding_all decide, fun _ h => (List.not_mem_nil h).elim, ?_⟩
  · intro fn member block present _ _
    cases List.mem_singleton.mp member
    change block ∈ [_] at present
    cases List.mem_singleton.mp present
    contradiction
  · rw [← ssa_bir_view_eq]
    exact ⟨ssa_wf.1, ssa_wf.2.2.2⟩

private theorem ssa_builds_bir : BuildsBIR (language := Language) recurSSA recurBIR := by
  refine ⟨rfl, bir_wf.1, ?_, ⟨fun _ h => h, fun _ h => h⟩, by with_unfolding_all decide,
    ⟨rfl, rfl, by rw [ssa_bir_view_eq]; rfl⟩, Interfaces.BIR.refinesOfViewEq ssa_bir_view_eq⟩
  intro fn member block present _ _ trap
  cases List.mem_singleton.mp member
  change block ∈ [_] at present
  cases List.mem_singleton.mp present
  contradiction

private theorem ssa_begin_at (n : Nat) (h : n < capacity) :
    ssaExec.step recurSSA (ssaEntry n) (ssaAllocated n) := by
  have step : (Interfaces.Memory.Execution (language := Language)).step ssaViewNorm (ssaEntry n) (ssaAllocated n) := by
    with_unfolding_all
      refine ⟨rfl, ⟨_, rfl, {ssaAllocated n with instruction := 0}, ⟨nofun, Or.inr ?_⟩, rfl⟩, [], rfl⟩
      refine ⟨rfl, ⟨1, 1, .nil⟩, rfl, Or.inr ⟨by decide, ⟨0⟩, n+1, ⟨n⟩, _, ?_, rfl⟩⟩
      unfold Mem.Memory.alloc
      exact ite_eq_left (allocation_fits n _ (by change n < 2^64 - 2 at h; change n + 2 < 2^64; omega))
  exact Interfaces.Memory.ssa_step_of_memory
    (Eq.subst (motive := fun m => (Interfaces.Memory.Execution (language := Language)).step m _ _) ssaView_eq.symm step)

private theorem ssa_writes_byte (n : Nat) :
    ssaViewNorm.Writes (ssaAllocated n) slot value {ssaBound n with instruction := 1} := by
  have access (bytes : List Mem.MByte) : (⟨(⟨n⟩, {candidate n with bytes}) :: blocks n, n+1⟩ : Mem.Memory).access
      ssaProfile.memoryTarget (range n).ptr 1 1 = some (some (⟨n⟩, 0)) := by
    simp [range, Mem.Memory.access, Mem.Memory.block, AList.lookup, candidate, Mem.candidate, Nat.mod_one]
  have loaded : (ssaBound n).bytes.load ssaProfile.memoryTarget (range n).ptr 1 1 = some [.init 42 none] := by
    unfold Mem.Memory.load
    rw [show (ssaBound n).bytes.access ssaProfile.memoryTarget (range n).ptr 1 1 = _ from access [.init 42 none]]
    simp [ssaBound, blocks, written, Mem.Memory.block, AList.lookup]
  with_unfolding_all
    refine ⟨⟨rfl, fun _ => rfl⟩, fun _ => ⟨⟨1, 1, .nil⟩, rfl, ⟨nofun, nofun⟩⟩, ?_, ?_,
      fun _ => Or.inr ⟨range n, [(0, [.init 42 none])], rfl, rfl, ?_, ?_⟩⟩
    · have encoded : Hybrid.encode ssaProfile.memoryTarget (.int .u8 42) = some [.init 42 none] := rfl
      simp [Interfaces.Memory.writeValue, Interfaces.Memory.markWritten, Interfaces.Memory.storeRuns,
        Interfaces.Memory.atOffset, Interfaces.Memory.mayInitialize, Interfaces.Memory.authorizeWrite,
        Interfaces.Memory.writeOwnerView, Interfaces.Memory.ownerView, Interfaces.Memory.zeroRoot,
        Interfaces.Memory.rangeAt, Interfaces.Memory.byteRuns, Interfaces.Memory.layoutAt,
        ssaViewNorm, ssaAllocated, ssaBound, ssaEntry, ssaStart, boundFrame, blocks, slot, root, value, written,
        range, candidate, Mem.Memory.store, Mem.Memory.access, Mem.Memory.block, Mem.Memory.modify,
        AList.lookup, AList.update, Mem.candidate, Nat.mod_one, encoded] <;> rfl
    · change (do
        let bs ← (ssaBound n).bytes.load ssaProfile.memoryTarget (range n).ptr 1 1
        let v ← Hybrid.decode ssaProfile.memoryTarget (.int .u8) bs
        pure (CoreValue.scalar v : Language.operands.Val)) = some value
      rw [loaded]
      rfl
    · exact fun _ => ⟨0, [.init 42 none], .head _, by decide⟩
    · intro offset bs member
      cases List.mem_singleton.mp member
      exact ⟨Nat.le_refl 1, fun byte hb => by cases List.mem_singleton.mp hb; decide, loaded⟩

private theorem ssa_next_call (n : Nat) (h : n < capacity) (reached : ssaExec.finite recurSSA (ssaEntry n)) :
    ssaExec.finite recurSSA (ssaEntry (n+1)) := by
  have storeMem : (Interfaces.Memory.Execution (language := Language)).step ssaViewNorm (ssaAllocated n) (ssaBound n) := by
    with_unfolding_all exact ⟨rfl, ⟨_, rfl, value, ssaAllocated n, _, rfl, ssa_writes_byte n, rfl⟩, [], rfl⟩
  have callMem : (Interfaces.Memory.Execution (language := Language)).step ssaViewNorm (ssaBound n) (ssaEntry (n+1)) := by
    with_unfolding_all exact ⟨rfl, ⟨_, rfl, rfl, [], _, _, _, rfl, rfl, rfl, rfl, .nil _⟩, [], rfl⟩
  have store : ssaExec.step recurSSA (ssaAllocated n) (ssaBound n) :=
    Interfaces.Memory.ssa_step_of_memory
      (Eq.subst (motive := fun m => (Interfaces.Memory.Execution (language := Language)).step m _ _) ssaView_eq.symm storeMem)
  have call : ssaExec.step recurSSA (ssaBound n) (ssaEntry (n+1)) :=
    Interfaces.Memory.ssa_step_of_memory
      (Eq.subst (motive := fun m => (Interfaces.Memory.Execution (language := Language)).step m _ _) ssaView_eq.symm callMem)
  exact finite_next (finite_next (finite_next reached (ssa_begin_at n h)) store) call

private theorem ssa_full : ssaViewNorm.BeginExhausted (ssaEntry capacity) ⟨0⟩ := by
  constructor
  · with_unfolding_all
      refine ⟨{ssaAllocated capacity with instruction := 0}, nofun, .inr ⟨rfl, ⟨1, 1, .nil⟩, rfl, .inr
        ⟨by decide, ⟨0⟩, capacity + 1, ⟨capacity⟩, _, ?_, rfl⟩⟩⟩
      unfold Mem.Memory.alloc
      exact ite_eq_left (allocation_fits capacity _ (by with_unfolding_all decide))
  · rintro ⟨_, _, ⟨hv, _⟩ | ⟨_, layout, hl, zero | alloc⟩⟩
    · have no : some StorageClass.materialized = some .virtual := by with_unfolding_all exact hv
      contradiction
    all_goals have eq : some (⟨1, 1, .nil⟩ : Layout) = some layout := by with_unfolding_all exact hl
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
        have hb : base + 1 < 2 ^ 64 := fits.2.2.1
        obtain ⟨i, rfl⟩ := Nat.exists_eq_succ_of_ne_zero nonnull
        have hi : i < capacity := by change i < 2 ^ 64 - 2; omega
        have collides := fits.2.2.2.2.2.1 (⟨i⟩, written i) (mem_blocks.mpr ⟨i, hi, rfl⟩) rfl
        exact collides ⟨rfl, Nat.zero_lt_one, Nat.zero_lt_one, Nat.lt_succ_self _, Nat.lt_succ_self _⟩
      · contradiction

private theorem ssa_reach (n : Nat) (le : n ≤ capacity) : ssaExec.finite recurSSA (ssaEntry n) := by
  induction n with
  | zero => exact ⟨_, by with_unfolding_all rfl, .nil _⟩
  | succ n ih => exact ssa_next_call n (by omega) (ih (by omega))

private theorem ssa_exhaust_step :
    ssaExec.step recurSSA (ssaEntry capacity) {ssaEntry capacity with resourceExhausted := true} ∧
    ssaExec.result recurSSA {ssaEntry capacity with resourceExhausted := true} .resourceExhausted := by
  let first := (ssaViewNorm.functions[0]).2.2.blocks[0]'(by with_unfolding_all decide)
  have progress := begin_progress (memory := ssaViewNorm) (q := ssaEntry capacity) (block := first)
    (root := ⟨0⟩) rfl (by with_unfolding_all rfl) (by with_unfolding_all rfl) (by with_unfolding_all rfl)
    ssa_full.valid
  rcases progress with ⟨s, began, _⟩ | ⟨stop, outcome⟩
  · exact (ssa_full.unavailable ⟨s, began⟩).elim
  · rw [← ssaView_eq] at stop outcome
    exact ⟨Interfaces.Memory.ssa_step_of_memory stop, outcome⟩

private theorem ssa_exhaust_unique {next : SSAState}
    (tick : ssaExec.step recurSSA (ssaEntry capacity) next) :
    next = {ssaEntry capacity with resourceExhausted := true} := by
  have raw : ssaViewNorm.Step (ssaEntry capacity) next true := ssaView_eq ▸ tick.2.1
  unfold Interfaces.Memory.Step at raw
  simp only [show (ssaEntry capacity).resourceExhausted = false from rfl, Bool.false_eq_true, ite_false] at raw
  split at raw
  · exact raw.1
  · let first := (ssaViewNorm.functions[0]).2.2.blocks[0]'(by with_unfolding_all decide)
    have atBegin := raw.instructionAtWith (block := first) (by with_unfolding_all rfl) rfl
    obtain ⟨started, began, _⟩ := atBegin
    exact (ssa_full.unavailable ⟨started, began⟩).elim

/-- Boundary control: when address space is still available (`n = 0`), SSA and BIR
cannot exhaust at the initial `beginLifetime`. -/
theorem ssa_bir_entry0_no_exhaust :
    ¬ ssaExec.step recurSSA (ssaEntry 0) {ssaEntry 0 with resourceExhausted := true} ∧
    ¬ birExec.step recurBIR (ssaEntry 0) {ssaEntry 0 with resourceExhausted := true} := by
  have noSSA : ¬ ssaExec.step recurSSA (ssaEntry 0) {ssaEntry 0 with resourceExhausted := true} := by
    intro tick
    have raw : ssaViewNorm.Step (ssaEntry 0) {ssaEntry 0 with resourceExhausted := true} true :=
      ssaView_eq ▸ tick.2.1
    simp only [Interfaces.Memory.Step, show (ssaEntry 0).resourceExhausted = false from rfl,
      Bool.false_eq_true, ite_false, ite_true] at raw
    obtain ⟨_, partialState, out⟩ := raw
    let first := (ssaViewNorm.functions[0]).2.2.blocks[0]'(by with_unfolding_all decide)
    have failed : ssaViewNorm.BeginExhausted (ssaEntry 0) ⟨0⟩ :=
      out.instructionAtWith (block := first) (by with_unfolding_all rfl) rfl
    apply failed.unavailable
    refine ⟨{ssaAllocated 0 with instruction := 0}, nofun, Or.inr ⟨by with_unfolding_all rfl, ⟨1, 1, .nil⟩,
      by with_unfolding_all rfl, Or.inr ⟨by decide, ⟨0⟩, 1, ⟨0⟩, _, ?_, rfl⟩⟩⟩
    unfold Mem.Memory.alloc
    exact ite_eq_left (allocation_fits 0 _ (by decide))
  refine ⟨noSSA, ?_⟩
  intro tick
  apply noSSA
  with_unfolding_all exact tick

/-- Recursive SSA execution suspends `capacity` callers, each holding its live
materialized byte and virtual return destination, then exhausts atomically at the
next `beginLifetime` with no normal successor. -/
theorem ssa_recursive_exhaustion :
    recurSSA.WF (language := Language) ∧
    ssaExec.finite recurSSA (ssaEntry capacity) ∧
    (ssaEntry capacity).callers.length = capacity ∧
    ssaExec.step recurSSA (ssaEntry capacity) {ssaEntry capacity with resourceExhausted := true} ∧
    ssaExec.result recurSSA {ssaEntry capacity with resourceExhausted := true} .resourceExhausted ∧
    (∀ next, ssaExec.step recurSSA (ssaEntry capacity) next →
      next = {ssaEntry capacity with resourceExhausted := true}) ∧
    ssaExec.observes recurSSA .resourceExhausted :=
  ⟨ssa_wf, ssa_reach capacity (Nat.le_refl _), ssaCallers_length capacity,
   ssa_exhaust_step.1, ssa_exhaust_step.2, fun _ => ssa_exhaust_unique,
   _, finite_next (ssa_reach capacity (Nat.le_refl _)) ssa_exhaust_step.1, ssa_exhaust_step.2⟩

/-- The exact same recursive suspended-caller exhaustion holds in `BIR.Execution`,
witnessed through `BuildsBIR recurSSA recurBIR`. -/
theorem bir_recursive_exhaustion :
    recurBIR.WF (language := Language) ∧
    BuildsBIR (language := Language) recurSSA recurBIR ∧
    birExec.finite recurBIR (ssaEntry capacity) ∧
    (ssaEntry capacity).callers.length = capacity ∧
    birExec.step recurBIR (ssaEntry capacity) {ssaEntry capacity with resourceExhausted := true} ∧
    birExec.result recurBIR {ssaEntry capacity with resourceExhausted := true} .resourceExhausted ∧
    (∀ next, birExec.step recurBIR (ssaEntry capacity) next →
      next = {ssaEntry capacity with resourceExhausted := true}) ∧
    birExec.observes recurBIR .resourceExhausted := by
  have birReach : birExec.finite recurBIR (ssaEntry capacity) := by
    with_unfolding_all exact ssa_reach capacity (Nat.le_refl _)
  have birStep : birExec.step recurBIR (ssaEntry capacity) {ssaEntry capacity with resourceExhausted := true} := by
    with_unfolding_all exact ssa_exhaust_step.1
  have birRes : birExec.result recurBIR {ssaEntry capacity with resourceExhausted := true} .resourceExhausted := by
    with_unfolding_all exact ssa_exhaust_step.2
  refine ⟨bir_wf, ssa_builds_bir, birReach, ssaCallers_length capacity, birStep, birRes, ?_,
    _, finite_next birReach birStep, birRes⟩
  intro next tick
  exact ssa_exhaust_unique (by with_unfolding_all exact tick)

end RecursiveControl

/-! ### ISS-0125 transfer progress and exhaustion controls

Exercise `BranchAt`, `CallAt`, `ReturnAt` and `DestructureAt` in both regimes:
full physical memory (0 bytes free, atomic exhaustion with no normal successor)
and available physical memory (1 byte free, normal completion with no exhausted
step). For `ReturnAt`, the available-space regime keeps memory 100% full before
`EndLocals` releases the callee's 1-byte materialized local, proving that space
availability is judged on the post-release heap `ended.bytes`. -/
namespace TransferProgressControls

private abbrev baseLang := sliceLanguage slice0
local instance : DecidableEq baseLang.operands.Ty := baseLang.typeDecidableEq
private abbrev u8Ty : baseLang.operands.Ty := .inl (.scalar (.int .u8))
private abbrev boolTy : baseLang.operands.Ty := .inl (.scalar .bool)
private abbrev backingTy : baseLang.operands.Ty := .inr false
private abbrev pairTy : baseLang.operands.Ty := .inr true
private abbrev lowering := sliceLoweringLanguage slice0
private abbrev profile : Profile := ⟨.lcb, .x86_64Linux, 4, 4096⟩
private abbrev mir := bindReturnControl slice0
private abbrev origin := mir.functions[0].2.2.blocks[0].body[0].origin
private abbrev calleeFn : FnId := ⟨mir.entry.module, ⟨"callee"⟩⟩
private abbrev pairDecl : DeclId := ⟨mir.entry.module, ⟨"Pair"⟩⟩
private def pairFld : FieldId := ⟨.struct pairDecl, ⟨"x"⟩⟩
private abbrev pairVal : baseLang.operands.Val := .other pairDecl
private abbrev u8Val : baseLang.operands.Val := .scalar (.int .u8 42)
private abbrev trueVal : baseLang.operands.Val := .scalar (.bool true)

private def backingSize (full : Bool) : Nat := if full then 2^64 - 2 else 2^64 - 3
private def backingBase (full : Bool) : Nat := if full then 1 else 2

private abbrev Language (full : Bool) : CoreLanguage := {baseLang with
  operands := {baseLang.operands with
    isCopy := fun ty => ty == pairTy || ty == backingTy || baseLang.operands.isCopy ty}
  safety := fun op => match op with
    | .expression expr => baseLang.safety (.expression expr)
    | .primitive prim args => baseLang.safety (.primitive prim args)
    | .call callee args => baseLang.safety (.call callee args)
    | .abort => baseLang.safety .abort
  layout := fun env target ty =>
    if ty == backingTy then some (.scalar (backingSize full) 1)
    else if ty == pairTy then some (.scalar 1 1)
    else baseLang.layout env target ty
  literal := fun ty bs =>
    if ty == boolTy && bs == [1] then some trueVal
    else if ty == pairTy && bs == [42] then some pairVal
    else baseLang.literal ty bs
  activeFields := fun _ ty v =>
    match ty, v with
    | .inr true, .other d => if d == pairDecl then some [pairFld] else none
    | _, _ => none
  fieldType := fun ty f =>
    if ty == pairTy && f == pairFld then some u8Ty else none
  field := fun v path =>
    match v with
    | .other d => if d == pairDecl && path == [pairFld] then some u8Val else none
    | _ => none}

private abbrev State (full : Bool) := MemoryRegisters (Language full) profile
private abbrev exec (full : Bool) :=
  Interfaces.Memory.Execution (language := Language full) (lang := lowering) (profile := profile)
private abbrev M := Interfaces.Memory lowering profile

private def backingRoot : PlaceId := ⟨0⟩
private def dstRoot : PlaceId := ⟨1⟩
private def auxRoot : PlaceId := ⟨2⟩
private def dstPlace : MemoryPlace baseLang.operands.Ty u8Ty := ⟨⟨dstRoot, []⟩, .materialized⟩
private def pairPlace : MemoryPlace baseLang.operands.Ty pairTy := ⟨⟨auxRoot, []⟩, .virtual⟩
private def u8Param : List (Sigma (MemoryPlace baseLang.operands.Ty)) := [⟨u8Ty, dstPlace⟩]
private def u8Arg : List (Sigma (MemoryRead baseLang.operands.Ty lowering.literal)) :=
  [⟨u8Ty, .literal ⟨[42]⟩⟩]

private def backingBlock (full : Bool) : Mem.Block :=
  Mem.candidate ⟨0⟩ (backingBase full) (backingSize full) 1
private def backingRange (full : Bool) : Hybrid.Range :=
  ⟨⟨⟨0⟩, backingBase full, some ⟨0⟩⟩, backingSize full, 1⟩
private def heapWithBacking (full : Bool) : Mem.Memory := ⟨[(⟨0⟩, backingBlock full)], 1⟩

private def reserveU8 {full : Bool} (s : State full) (root : PlaceId) (base : Nat) : State full :=
  let ptr : Mem.Ptr := ⟨⟨0⟩, base, some ⟨s.bytes.next⟩⟩
  {s with live := root :: s.live, materialized := (root, ⟨ptr, 1, 1⟩) :: s.materialized
          bytes := ⟨(⟨s.bytes.next⟩, Mem.candidate ⟨0⟩ base 1 1) :: s.bytes.blocks, s.bytes.next + 1⟩}

private theorem no_u8_in_full {heap : Mem.Memory}
    (hasBacking : (⟨0⟩, backingBlock true) ∈ heap.blocks) :
    ¬ Interfaces.Memory.ReservationsFit profile.memoryTarget (2^64) [⟨1, 1, .nil⟩] heap := by
  rintro ⟨space, base, id, reserved, alloc, _⟩
  have fits := (Mem.alloc_result _ _ _ _ _ _ _ _ alloc).2.2
  obtain rfl : space = ⟨0⟩ := List.mem_singleton.mp fits.2.2.2.2.2.2
  have nz : base ≠ 0 := fits.2.2.2.2.1 Nat.zero_lt_one
  have hi : base + 1 < 2^64 := fits.2.2.1
  exact fits.2.2.2.2.2.1 (⟨0⟩, backingBlock true) hasBacking rfl
    ⟨rfl, by decide, Nat.zero_lt_one, by change 1 < base + 1; omega,
      by change base < 1 + (2^64 - 2); omega⟩

private theorem no_u8_when_callee_holds_byte :
    let heap : Mem.Memory := ⟨[(⟨1⟩, Mem.candidate ⟨0⟩ 1 1 1), (⟨0⟩, backingBlock false)], 2⟩
    ¬ Interfaces.Memory.ReservationsFit profile.memoryTarget (2^64) [⟨1, 1, .nil⟩] heap := by
  dsimp only
  rintro ⟨space, base, id, reserved, alloc, _⟩
  have fits := (Mem.alloc_result _ _ _ _ _ _ _ _ alloc).2.2
  obtain rfl : space = ⟨0⟩ := List.mem_singleton.mp fits.2.2.2.2.2.2
  have nz : base ≠ 0 := fits.2.2.2.2.1 Nat.zero_lt_one
  have hi : base + 1 < 2^64 := fits.2.2.1
  by_cases atOne : base = 1
  · subst base
    exact fits.2.2.2.2.2.1 (⟨1⟩, Mem.candidate ⟨0⟩ 1 1 1) (.head _) rfl
      ⟨rfl, Nat.zero_lt_one, Nat.zero_lt_one, by decide, by decide⟩
  · exact fits.2.2.2.2.2.1 (⟨0⟩, backingBlock false) (.tail _ (.head _)) rfl
      ⟨rfl, by decide, Nat.zero_lt_one, by change 2 < base + 1; omega,
        by change base < 2 + (2^64 - 3); omega⟩

-- Shared helper: writing 42 into a freshly allocated u8 at `base = 2^64` under `ceiling = 2^64 + 2`.
private def farCeiling : Nat := 2^64 + 2

private theorem u8_isCopy (full : Bool) : (Language full).operands.isCopy u8Ty = true := by
  cases full <;> with_unfolding_all rfl

private theorem u8_byteRuns (full : Bool) :
    Interfaces.Memory.byteRuns (language := Language full) profile.memoryTarget u8Ty u8Val =
      some [(0, [.init 42 none])] := by
  cases full <;> with_unfolding_all rfl

private theorem u8_scalarType (full : Bool) :
    (Language full).scalarType u8Ty = some (.int .u8) := by
  cases full <;> with_unfolding_all rfl

private theorem u8_decode :
    Hybrid.decode profile.memoryTarget (.int .u8) [.init 42 none] = some (.int .u8 42) := by
  with_unfolding_all rfl

private theorem discard_nil {full : Bool} {m : M} {s : State full} {ty}
    (p : MemoryPlace _ ty) (allow : Bool) (c : Nat) (h : s.movedFields = []) :
    m.discardRemainder (language := Language full) s p allow c = false := by
  cases s with
  | mk fr bytes callers effects own nextId ex =>
    cases fr with
    | mk fn blk pc vv mat live init zv addr moved =>
      dsimp only at h
      subst h
      with_unfolding_all rfl

private theorem markWritten_nil {full : Bool} {s : State full} {ty}
    (p : MemoryPlace _ ty) (ow : Own.State)
    (ha : s.addressIds = []) (hm : s.movedFields = []) :
    Interfaces.Memory.markWritten (language := Language full) s p ow =
      {s with initialized :=
        if s.initialized.contains p.logical.root then s.initialized else p.logical.root :: s.initialized} := by
  cases s with
  | mk fr bytes callers effects own nextId ex =>
    cases fr with
    | mk fn blk pc vv mat live init zv addr moved =>
      dsimp only at ha hm
      subst ha hm
      cases p with
      | mk lp st =>
        cases lp with
        | mk r path =>
          cases path <;> with_unfolding_all rfl

private theorem authorizeWrite_fresh {full : Bool} {m : M} {s : State full} (ceiling : Nat)
    (notInit : s.initialized.contains dstRoot = false)
    (ha : s.addressIds = []) (hm : s.movedFields = []) (ho : s.ownership = {}) :
    ∃ ow, m.authorizeWrite (language := Language full) s dstPlace ceiling = some ow := by
  have discT : m.discardRemainder (language := Language full) s dstPlace true ceiling = false :=
    discard_nil dstPlace true ceiling hm
  have discF : m.discardRemainder (language := Language full) s dstPlace false ceiling = false :=
    discard_nil dstPlace false ceiling hm
  unfold Interfaces.Memory.authorizeWrite Interfaces.Memory.writeOwnerView Interfaces.Memory.ownerView
  rw [discT, discF]
  simp only [dstPlace, notInit, Bool.not_false, Bool.true_or, Bool.false_and,
    Bool.false_eq_true, ite_false, ite_true, ha, ho, AList.lookup, Option.getD_none, Own.slot,
    List.find?_nil, Option.map_none, Own.reinitialize, List.filter_nil, List.all_nil,
    List.find?_cons_of_pos, beq_self_eq_true]
  exact ⟨_, rfl⟩

private theorem validPlace_frame {full : Bool} {m : M} {s₁ s₂ : State full} {ty}
    (p : MemoryPlace _ ty) (h : s₁.frame = s₂.frame) :
    m.validPlace (language := Language full) s₁ p = m.validPlace (language := Language full) s₂ p := by
  unfold Interfaces.Memory.validPlace
  rw [h]

private theorem write_u8_at {full : Bool} {m : M} {q : State full} (base ceiling : Nat)
    (frameOk : m.validPlace (language := Language full) (reserveU8 q dstRoot base) dstPlace = true)
    (layoutOk : m.layoutAt (reserveU8 q dstRoot base).frame dstRoot = some ⟨1, 1, .nil⟩)
    (notInit : q.initialized.contains dstRoot = false)
    (noAddr : q.addressIds = []) (noMoved : q.movedFields = [])
    (noOwn : q.ownership = {}) :
    ∃ stored, m.Writes (language := Language full) (reserveU8 q dstRoot base) dstPlace u8Val stored ceiling := by
  let opened := reserveU8 q dstRoot base
  let ptr : Mem.Ptr := ⟨⟨0⟩, base, some ⟨q.bytes.next⟩⟩
  let b : Mem.Block := {Mem.candidate ⟨0⟩ base 1 1 with bytes := [.init 42 none]}
  let stored : State full := {opened with
    initialized := dstRoot :: q.initialized
    bytes := ⟨(⟨q.bytes.next⟩, b) :: q.bytes.blocks, q.bytes.next + 1⟩}
  have access (bs : List Mem.MByte) :
      (⟨(⟨q.bytes.next⟩, {Mem.candidate ⟨0⟩ base 1 1 with bytes := bs}) :: q.bytes.blocks, q.bytes.next + 1⟩ : Mem.Memory).access
        profile.memoryTarget ptr 1 1 ceiling = some (some (⟨q.bytes.next⟩, 0)) := by
    simp [ptr, Mem.Memory.access, Mem.Memory.block, AList.lookup, Mem.candidate, Nat.mod_one]
  have loaded : stored.bytes.load profile.memoryTarget ptr 1 1 ceiling = some [.init 42 none] := by
    unfold Mem.Memory.load
    rw [show stored.bytes.access profile.memoryTarget ptr 1 1 ceiling = _ from access [.init 42 none]]
    simp [stored, b, Mem.Memory.block, AList.lookup]
  have rangeEq : m.rangeAt (language := Language full) opened dstPlace.logical = some ⟨ptr, 1, 1⟩ := by
    simp [Interfaces.Memory.rangeAt, opened, reserveU8, dstPlace, AList.lookup, ptr]
  have zeroFalse : m.zeroRoot (language := Language full) q.frame dstRoot = false := by
    unfold Interfaces.Memory.zeroRoot
    rw [show m.layoutAt (language := Language full) q.frame dstRoot = some ⟨1, 1, .nil⟩ from layoutOk]
    rfl
  have storeOk : opened.bytes.store profile.memoryTarget ptr [.init 42 none] 1 ceiling = some stored.bytes := by
    unfold Mem.Memory.store
    have hacc : opened.bytes.access profile.memoryTarget ptr ([Mem.MByte.init 42 none]).length 1 ceiling =
        some (some (⟨q.bytes.next⟩, 0)) := access [.uninit]
    rw [hacc]
    simp [opened, reserveU8, stored, b, Mem.Memory.modify, AList.update, Mem.candidate]
  have put : m.writeValue (language := Language full) opened dstPlace u8Val ceiling = some stored := by
    have liveTrue : opened.live.contains dstRoot = true := by
      simp only [opened, reserveU8, List.contains_cons, beq_self_eq_true, Bool.true_or]
    have mayInit : m.mayInitialize (language := Language full) opened dstPlace ceiling = true := by
      unfold Interfaces.Memory.mayInitialize
      change (if !q.initialized.contains dstRoot then ([] : List FieldId).isEmpty else _) = true
      rw [notInit]
      rfl
    obtain ⟨ow, haw⟩ := authorizeWrite_fresh (m := m) (s := opened) ceiling notInit noAddr noMoved noOwn
    have markEq : Interfaces.Memory.markWritten (language := Language full) opened dstPlace ow =
        {opened with initialized := dstRoot :: q.initialized} := by
      rw [markWritten_nil (s := opened) dstPlace ow noAddr noMoved]
      change {opened with initialized := if q.initialized.contains dstRoot then q.initialized else dstRoot :: q.initialized} = _
      rw [notInit]
      rfl
    unfold Interfaces.Memory.writeValue
    change (if !opened.live.contains dstRoot || !m.mayInitialize (language := Language full) opened dstPlace ceiling
      then none else _) = some stored
    rw [liveTrue, mayInit]
    simp only [Bool.not_true, Bool.false_or, Bool.false_eq_true, ite_false,
      Option.bind_eq_bind, haw, Option.bind_some, markEq]
    change (if m.zeroRoot (language := Language full) q.frame dstRoot then _ else _) = some stored
    rw [zeroFalse]
    simp only [Bool.false_eq_true, ite_false]
    change ((m.rangeAt (language := Language full) {opened with initialized := dstRoot :: q.initialized} dstPlace.logical).bind
      fun range => (Interfaces.Memory.byteRuns (language := Language full) profile.memoryTarget u8Ty u8Val).bind
      fun runs => (Interfaces.Memory.storeRuns profile.memoryTarget ceiling range runs opened.bytes).bind
      fun bs => some ({opened with initialized := dstRoot :: q.initialized, bytes := bs} : State full)) = some stored
    have rangeMarked : m.rangeAt (language := Language full) {opened with initialized := dstRoot :: q.initialized} dstPlace.logical =
        some ⟨ptr, 1, 1⟩ := by
      simp [Interfaces.Memory.rangeAt, opened, reserveU8, dstPlace, AList.lookup, ptr]
    rw [rangeMarked]
    simp only [Option.bind_some, u8_byteRuns full, Interfaces.Memory.storeRuns, List.foldlM_cons, List.foldlM_nil]
    rw [ite_eq_left (by decide : 0 + ([Mem.MByte.init 42 none]).length ≤ 1)]
    rw [show Interfaces.Memory.atOffset ptr 0 = ptr from rfl, storeOk]
    rfl
  refine ⟨stored, ⟨frameOk, fun _ => u8_isCopy full⟩, fun _ => ⟨⟨1, 1, .nil⟩, layoutOk, ?_⟩, put, ?_, ?_⟩
  · change (1 : Nat) = 0 ↔ (AList.lookup ((dstRoot, ⟨ptr, 1, 1⟩) :: q.materialized) dstRoot).isNone
    simp only [AList.lookup, ite_true, Option.isNone_some, Bool.false_eq_true, Nat.one_ne_zero]
  · unfold Interfaces.Memory.loadValue Interfaces.Memory.loadBacking Interfaces.Memory.loadMaterialized
    have initTrue : Interfaces.Memory.initializedAt stored dstRoot [] = true := by
      unfold Interfaces.Memory.initializedAt
      have hmv : stored.movedFields = [] := noMoved
      rw [hmv]
      simp only [stored, List.contains_cons, beq_self_eq_true, Bool.true_or, List.any_nil,
        Bool.not_false, Bool.and_self]
    have liveTrue : stored.live.contains dstRoot = true := by
      simp only [stored, opened, reserveU8, List.contains_cons, beq_self_eq_true, Bool.true_or]
    have validStored : m.validPlace (language := Language full) stored dstPlace = true :=
      (validPlace_frame dstPlace rfl).trans frameOk
    change (if Interfaces.Memory.initializedAt stored dstRoot [] then
      if !m.validPlace (language := Language full) stored dstPlace then none else
      if !stored.live.contains dstRoot then none else
      if m.zeroRoot (language := Language full) q.frame dstRoot then _ else _ else none) = some u8Val
    rw [initTrue, validStored, liveTrue, zeroFalse]
    simp only [ite_true, Bool.not_true, Bool.false_eq_true, ite_false]
    have rangeStored : m.rangeAt (language := Language full) stored dstPlace.logical = some ⟨ptr, 1, 1⟩ := by
      simp [Interfaces.Memory.rangeAt, stored, opened, reserveU8, dstPlace, AList.lookup, ptr]
    rw [rangeStored]
    simp only [Option.bind_eq_bind, Option.bind_some]
    rw [show stored.bytes.load profile.memoryTarget ptr 1 1 ceiling = some [.init 42 none] from loaded,
      u8_scalarType full]
    simp only [Option.bind_some, u8_decode]
    rfl
  · intro _
    refine .inr ⟨⟨ptr, 1, 1⟩, [(0, [.init 42 none])], rangeEq, u8_byteRuns full,
      fun _ => ⟨0, [.init 42 none], .head _, by decide⟩, ?_⟩
    intro offset bs member
    cases List.mem_singleton.mp member
    exact ⟨Nat.le_refl 1, fun byte hb => by cases List.mem_singleton.mp hb; decide, loaded⟩

private theorem params_valid_far {full : Bool} {m : M} {q : State full}
    (ck : m.TransferCheckpoint (language := Language full) q farCeiling)
    (fresh : dstRoot ∉ q.live)
    (cls : m.classAt q.frame dstRoot = some .materialized)
    (layoutOk : m.layoutAt q.frame dstRoot = some ⟨1, 1, .nil⟩)
    (frameOk : m.validPlace (language := Language full) (reserveU8 q dstRoot (2^64)) dstPlace = true)
    (notInit : q.initialized.contains dstRoot = false)
    (noAddr : q.addressIds = []) (noMoved : q.movedFields = []) (noOwn : q.ownership = {})
    (below : ∀ e ∈ q.bytes.blocks, e.2.live = true → e.2.base + e.2.size < 2^64) :
    m.ParamsValid (language := Language full) u8Param [u8Val] q farCeiling := by
  refine ⟨ck, ?_⟩
  obtain ⟨stored, writes⟩ := write_u8_at (2^64) farCeiling frameOk layoutOk notInit noAddr noMoved noOwn
  refine ⟨stored, .cons (started := reserveU8 q dstRoot (2^64)) ?_ writes (.nil _)⟩
  have notLive : q.live.contains dstPlace.logical.root ≠ true :=
    fun h => fresh (List.contains_iff_mem.mp h)
  rw [ite_eq_right notLive]
  refine ⟨fresh, .inr ⟨cls, ⟨1, 1, .nil⟩, layoutOk, .inr
    ⟨Nat.zero_lt_one, ⟨0⟩, 2^64, ⟨q.bytes.next⟩, _, ?_, rfl⟩⟩⟩
  unfold Mem.Memory.alloc
  refine ite_eq_left ⟨by decide, Nat.mod_one _, by decide, by decide, fun _ => by decide, ?_, by decide⟩
  intro e hmem hlive hoverlap
  have bnd := below e hmem hlive
  have cross := hoverlap.2.2.2.2
  change 2^64 < e.2.base + e.2.size at cross
  omega

private def baseStart (full : Bool) : State full :=
  ⟨⟨mir.entry, ⟨0⟩, 0, [], [], [], [], [], [], []⟩, .empty, [], [], {}, 0, false⟩

private def afterBacking (full : Bool) : State full :=
  {baseStart full with
    instruction := 1
    live := [backingRoot]
    materialized := [(backingRoot, backingRange full)]
    bytes := heapWithBacking full}

private theorem open_backing_step {full : Bool} {m : M} {blk : MemoryBlock lowering}
    (initEq : m.blockAt (language := Language full) (baseStart full) = some blk)
    (pc0 : blk.body[0]? = some ⟨origin, .beginLifetime backingRoot⟩)
    (cls : m.classAt (language := Language full) mir.entry backingRoot = some .materialized)
    (layoutOk : m.layoutAt (language := Language full) mir.entry backingRoot = some (.scalar (backingSize full) 1)) :
    (exec full).step m (baseStart full) (afterBacking full) := by
  have ready : m.terminalObservation (language := Language full) (baseStart full) = none :=
    Interfaces.Memory.terminalObservation_of_instruction rfl initEq pc0
      Interfaces.Memory.instructionTrap_beginLifetime
  refine Interfaces.Memory.silent_step ready ?_ rfl
  simp only [Interfaces.Memory.Step, show (baseStart full).resourceExhausted = false from rfl,
    show (afterBacking full).resourceExhausted = false from rfl, Bool.false_eq_true, ite_false]
  refine ⟨blk, initEq, ?_⟩
  rw [show (baseStart full).instruction = 0 from rfl, pc0]
  refine ⟨{afterBacking full with instruction := 0}, ⟨nofun, .inr ⟨cls, _, layoutOk, .inr
    ⟨by cases full <;> decide, ⟨0⟩, backingBase full, ⟨0⟩, _, ?_, rfl⟩⟩⟩, rfl⟩
  cases full <;> with_unfolding_all rfl

/-! #### 1. Branch (`BranchAt`) control -/

private def branchYesEdge : CFGEdge (Sigma (MemoryRead baseLang.operands.Ty lowering.literal)) :=
  ⟨⟨1⟩, u8Arg⟩
private def branchNoEdge : CFGEdge (Sigma (MemoryRead baseLang.operands.Ty lowering.literal)) :=
  ⟨⟨2⟩, []⟩
private def branchEntry : MemoryBlock lowering :=
  ⟨⟨0⟩, [], [⟨origin, .beginLifetime backingRoot⟩],
    .branch (.literal (ty := boolTy) ⟨[1]⟩) branchYesEdge branchNoEdge, []⟩
private def branchYesDst : MemoryBlock lowering :=
  ⟨⟨1⟩, u8Param, [], .ret (some ⟨u8Ty, .copy dstPlace⟩), []⟩
private def branchNoDst : MemoryBlock lowering :=
  ⟨⟨2⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[0]⟩⟩), []⟩
private def branchLocals : LocalTypes baseLang.operands.Ty :=
  [(backingRoot, backingTy), (dstRoot, u8Ty)]
private def branchMemory (full : Bool) : M :=
  { project := mir.project, entry := mir.entry, signatures := mir.signatures
    functions := [(mir.entry, branchLocals, ⟨⟨0⟩, #[branchEntry, branchYesDst, branchNoDst]⟩)]
    storage := [(mir.entry, [(backingRoot, .materialized), (dstRoot, .materialized)])]
    layouts := [(mir.entry, [(backingRoot, .scalar (backingSize full) 1), (dstRoot, .scalar 1 1)])] }

/-- Control for `BranchAt` corollaries (`ISS-0125`): full memory exhausts atomically
with no normal successor, available memory completes normally with no exhausted step,
and `BranchAt.progress` covers both uniformly. -/
theorem branch_transfer_control (full : Bool) :
    (branchMemory full).HandoffWF ∧
    (exec full).finite (branchMemory full) (afterBacking full) ∧
    (∃ next, (exec full).step (branchMemory full) (afterBacking full) next) ∧
    (if full then
      ((exec full).step (branchMemory full) (afterBacking full) {(afterBacking full) with resourceExhausted := true} ∧
       (exec full).result (branchMemory full) {(afterBacking full) with resourceExhausted := true} .resourceExhausted ∧
       ¬ ∃ next, (exec full).step (branchMemory full) (afterBacking full) next ∧ next.resourceExhausted = false)
    else
      ((∃ next, (exec full).step (branchMemory full) (afterBacking full) next ∧ next.resourceExhausted = false) ∧
       ¬ ((exec full).step (branchMemory full) (afterBacking full) {(afterBacking full) with resourceExhausted := true} ∧
          (exec full).result (branchMemory full) {(afterBacking full) with resourceExhausted := true} .resourceExhausted))) := by
  have wf : (branchMemory full).HandoffWF :=
    (checkHandoff_iff _).mp (by cases full <;> with_unfolding_all decide)
  have reached : (exec full).finite (branchMemory full) (afterBacking full) :=
    ⟨baseStart full, by cases full <;> with_unfolding_all rfl,
      .cons (open_backing_step (by cases full <;> with_unfolding_all rfl) rfl
        (by cases full <;> with_unfolding_all rfl) (by cases full <;> with_unfolding_all rfl)) (.nil _)⟩
  have br : (branchMemory full).BranchAt (language := Language full)
      (afterBacking full) (afterBacking full) branchYesDst (.literal ⟨[1]⟩) branchYesEdge branchNoEdge true [u8Val] :=
    ⟨rfl, ⟨branchEntry, by cases full <;> with_unfolding_all rfl, rfl, rfl⟩,
     ⟨trueVal, afterBacking full, by cases full <;> with_unfolding_all rfl, by cases full <;> with_unfolding_all rfl,
      by cases full <;> with_unfolding_all rfl⟩,
     by cases full <;> with_unfolding_all rfl, rfl⟩
  let arrive := Interfaces.Memory.BranchAt.arrive (afterBacking full) branchYesEdge branchNoEdge true
  have valid : (branchMemory full).ParamsValid (language := Language full) u8Param [u8Val] arrive farCeiling :=
    params_valid_far (by cases full <;> with_unfolding_all decide) (by cases full <;> decide)
      (by cases full <;> with_unfolding_all rfl) (by cases full <;> with_unfolding_all rfl)
      (by cases full <;> with_unfolding_all rfl) rfl rfl rfl rfl (by
        intro e hm _
        cases List.mem_singleton.mp hm
        cases full <;> decide)
  refine ⟨wf, reached, Interfaces.Memory.BranchAt.progress (language := Language full) wf.2.2.2 br valid, ?_⟩
  cases full with
  | true =>
    have noFit : ¬ (branchMemory true).ParamSpaceFits (language := Language true) u8Param arrive :=
      no_u8_in_full (.head _)
    have ex := (Interfaces.Memory.BranchAt.exhausted_iff (language := Language true) wf.2.2.2 br).mpr
      ⟨⟨farCeiling, valid⟩, noFit⟩
    exact ⟨ex.1, ex.2, fun h => noFit ((Interfaces.Memory.BranchAt.normal_iff_fit (language := Language true) wf.2.2.2 br valid).mp h)⟩
  | false =>
    have fits : (branchMemory false).ParamSpaceFits (language := Language false) u8Param arrive := by
      with_unfolding_all exact ⟨⟨0⟩, 1, ⟨1⟩, _, rfl, trivial⟩
    exact ⟨Interfaces.Memory.BranchAt.normal_of_fit (language := Language false) wf.2.2.2 br valid fits,
      fun h => ((Interfaces.Memory.BranchAt.exhausted_iff (language := Language false) wf.2.2.2 br).mp h).2 fits⟩

/-! #### 2. Call (`CallAt`) control -/

private def callCallerBlock : MemoryBlock lowering :=
  ⟨⟨0⟩, [],
    [⟨origin, .beginLifetime backingRoot⟩,
     ⟨origin, .call calleeFn (some ⟨u8Ty, dstPlace⟩) u8Arg []⟩],
    .ret (some ⟨u8Ty, .literal ⟨[0]⟩⟩), []⟩
private def callCalleeEntry : MemoryBlock lowering :=
  ⟨⟨0⟩, u8Param, [], .ret (some ⟨u8Ty, .copy dstPlace⟩), []⟩
private def callMemory (full : Bool) : M :=
  { project := mir.project, entry := mir.entry
    signatures := [(mir.entry, ⟨[], some u8Ty⟩), (calleeFn, ⟨[u8Ty], some u8Ty⟩)]
    functions := [(mir.entry, branchLocals, ⟨⟨0⟩, #[callCallerBlock]⟩),
                  (calleeFn, [(dstRoot, u8Ty)], ⟨⟨0⟩, #[callCalleeEntry]⟩)]
    storage := [(mir.entry, [(backingRoot, .materialized), (dstRoot, .materialized)]),
                (calleeFn, [(dstRoot, .materialized)])]
    layouts := [(mir.entry, [(backingRoot, .scalar (backingSize full) 1), (dstRoot, .scalar 1 1)]),
                (calleeFn, [(dstRoot, .scalar 1 1)])] }

/-- Control for `CallAt` corollaries (`ISS-0125`): calling a function with a
materialized `u8` parameter exhausts atomically when full, completes normally
when space fits, and satisfies `CallAt.progress` in both regimes. -/
theorem call_transfer_control (full : Bool) :
    (callMemory full).HandoffWF ∧
    (exec full).finite (callMemory full) (afterBacking full) ∧
    (∃ next, (exec full).step (callMemory full) (afterBacking full) next) ∧
    (if full then
      ((exec full).step (callMemory full) (afterBacking full) {(afterBacking full) with resourceExhausted := true} ∧
       (exec full).result (callMemory full) {(afterBacking full) with resourceExhausted := true} .resourceExhausted ∧
       ¬ ∃ next, (exec full).step (callMemory full) (afterBacking full) next ∧ next.resourceExhausted = false)
    else
      ((∃ next, (exec full).step (callMemory full) (afterBacking full) next ∧ next.resourceExhausted = false) ∧
       ¬ ((exec full).step (callMemory full) (afterBacking full) {(afterBacking full) with resourceExhausted := true} ∧
          (exec full).result (callMemory full) {(afterBacking full) with resourceExhausted := true} .resourceExhausted))) := by
  have wf : (callMemory full).HandoffWF :=
    (checkHandoff_iff _).mp (by cases full <;> with_unfolding_all decide)
  have reached : (exec full).finite (callMemory full) (afterBacking full) :=
    ⟨baseStart full, by cases full <;> with_unfolding_all rfl,
      .cons (open_backing_step (by cases full <;> with_unfolding_all rfl) rfl
        (by cases full <;> with_unfolding_all rfl) (by cases full <;> with_unfolding_all rfl)) (.nil _)⟩
  have call : (callMemory full).CallAt (language := Language full)
      (afterBacking full) (afterBacking full) calleeFn (some ⟨u8Ty, dstPlace⟩) u8Arg [] callCalleeEntry [u8Val] :=
    ⟨rfl, ⟨callCallerBlock, _, by cases full <;> with_unfolding_all rfl, rfl, rfl⟩,
     by cases full <;> with_unfolding_all rfl, rfl,
     ⟨⟨⟨0⟩, #[callCalleeEntry]⟩, by cases full <;> with_unfolding_all rfl, rfl⟩, rfl⟩
  let arrive := Interfaces.Memory.CallAt.arrive calleeFn callCalleeEntry.id (some ⟨u8Ty, dstPlace⟩) (afterBacking full)
  have valid : (callMemory full).ParamsValid (language := Language full) u8Param [u8Val] arrive farCeiling :=
    params_valid_far (by cases full <;> with_unfolding_all decide) nofun
      (by cases full <;> with_unfolding_all rfl) (by cases full <;> with_unfolding_all rfl)
      (by cases full <;> with_unfolding_all rfl) rfl rfl rfl rfl (by
        intro e hm _
        cases List.mem_singleton.mp hm
        cases full <;> decide)
  refine ⟨wf, reached, Interfaces.Memory.CallAt.progress (language := Language full) wf.2.2.2 call valid, ?_⟩
  cases full with
  | true =>
    have noFit : ¬ (callMemory true).ParamSpaceFits (language := Language true) u8Param arrive :=
      no_u8_in_full (.head _)
    have ex := (Interfaces.Memory.CallAt.exhausted_iff (language := Language true) wf.2.2.2 call).mpr
      ⟨⟨farCeiling, valid⟩, noFit⟩
    exact ⟨ex.1, ex.2, fun h => noFit ((Interfaces.Memory.CallAt.normal_iff_fit (language := Language true) wf.2.2.2 call valid).mp h)⟩
  | false =>
    have fits : (callMemory false).ParamSpaceFits (language := Language false) u8Param arrive := by
      with_unfolding_all exact ⟨⟨0⟩, 1, ⟨1⟩, _, rfl, trivial⟩
    exact ⟨Interfaces.Memory.CallAt.normal_of_fit (language := Language false) wf.2.2.2 call valid fits,
      fun h => ((Interfaces.Memory.CallAt.exhausted_iff (language := Language false) wf.2.2.2 call).mp h).2 fits⟩

/-! #### 3. Return (`ReturnAt`) control -/

private def retCallerBlock : MemoryBlock lowering :=
  ⟨⟨0⟩, [],
    [⟨origin, .beginLifetime backingRoot⟩,
     ⟨origin, .call calleeFn (some ⟨u8Ty, dstPlace⟩) [] []⟩],
    .ret (some ⟨u8Ty, .copy dstPlace⟩), []⟩
private def retCalleeBlock (full : Bool) : MemoryBlock lowering :=
  ⟨⟨0⟩, [], if full then [] else [⟨origin, .beginLifetime auxRoot⟩],
    .ret (some ⟨u8Ty, .literal ⟨[42]⟩⟩), []⟩
private def retMemory (full : Bool) : M :=
  { project := mir.project, entry := mir.entry
    signatures := [(mir.entry, ⟨[], some u8Ty⟩), (calleeFn, ⟨[], some u8Ty⟩)]
    functions := [(mir.entry, branchLocals, ⟨⟨0⟩, #[retCallerBlock]⟩),
                  (calleeFn, if full then [] else [(auxRoot, u8Ty)], ⟨⟨0⟩, #[retCalleeBlock full]⟩)]
    storage := [(mir.entry, [(backingRoot, .materialized), (dstRoot, .materialized)]),
                (calleeFn, if full then [] else [(auxRoot, .materialized)])]
    layouts := [(mir.entry, [(backingRoot, .scalar (backingSize full) 1), (dstRoot, .scalar 1 1)]),
                (calleeFn, if full then [] else [(auxRoot, .scalar 1 1)])] }

private def retCont (full : Bool) :
    MemoryContinuation baseLang.operands.Ty baseLang.operands.Val :=
  ⟨(afterBacking full).advance.toMemoryFrame, some ⟨u8Ty, dstPlace⟩⟩

private def retCalleeEntry (full : Bool) : State full :=
  { toMemoryFrame := ⟨calleeFn, ⟨0⟩, 0, [], [], [], [], [], [], []⟩
    bytes := heapWithBacking full
    callers := [retCont full] }

private def retState (full : Bool) : State full :=
  if full then retCalleeEntry true
  else
    let base := retCalleeEntry false
    {base with
      instruction := 1
      live := [auxRoot]
      materialized := [(auxRoot, ⟨⟨⟨0⟩, 1, some ⟨1⟩⟩, 1, 1⟩)]
      bytes := ⟨[(⟨1⟩, Mem.candidate ⟨0⟩ 1 1 1), (⟨0⟩, backingBlock false)], 2⟩}

private def retEnded (full : Bool) : State full :=
  if full then retCalleeEntry true
  else
    {retCalleeEntry false with
      instruction := 1
      bytes := ⟨[(⟨1⟩, {Mem.candidate ⟨0⟩ 1 1 1 with live := false}), (⟨0⟩, backingBlock false)], 2⟩}

/-- Control for `ReturnAt` corollaries (`ISS-0125`):
- When `full = true`, the caller fills all usable memory so returning into its
  unopened materialized `u8` destination exhausts atomically at the callee return PC.
- When `full = false`, the callee holds the last remaining byte at address 1 so
  memory is 100% full before `EndLocals`, yet `ReturnAt.normal_of_fit` completes
  normally after `EndLocals` releases that byte and `ReturnAt.exhausted_iff` rules
  out exhaustion. -/
theorem return_transfer_control (full : Bool) :
    (retMemory full).HandoffWF ∧
    (exec full).finite (retMemory full) (retState full) ∧
    (∃ next, (exec full).step (retMemory full) (retState full) next) ∧
    (if full then
      ((exec full).step (retMemory full) (retState full) {(retState full) with resourceExhausted := true} ∧
       (exec full).result (retMemory full) {(retState full) with resourceExhausted := true} .resourceExhausted ∧
       ¬ ∃ next, (exec full).step (retMemory full) (retState full) next ∧ next.resourceExhausted = false)
    else
      ((¬ Interfaces.Memory.ReservationsFit profile.memoryTarget (2^64) [⟨1, 1, .nil⟩] (retState full).bytes) ∧
       (∃ next, (exec full).step (retMemory full) (retState full) next ∧ next.resourceExhausted = false) ∧
       ¬ ((exec full).step (retMemory full) (retState full) {(retState full) with resourceExhausted := true} ∧
          (exec full).result (retMemory full) {(retState full) with resourceExhausted := true} .resourceExhausted))) := by
  have wf : (retMemory full).HandoffWF :=
    (checkHandoff_iff _).mp (by cases full <;> with_unfolding_all decide)
  have step1 : (exec full).step (retMemory full) (baseStart full) (afterBacking full) :=
    open_backing_step (by cases full <;> with_unfolding_all rfl) rfl
      (by cases full <;> with_unfolding_all rfl) (by cases full <;> with_unfolding_all rfl)
  have step2 : (exec full).step (retMemory full) (afterBacking full) (retCalleeEntry full) := by
    cases full <;> with_unfolding_all exact ⟨rfl, ⟨_, rfl, rfl, [], _, _, _, rfl, rfl, rfl, rfl, .nil _⟩, [], rfl⟩
  have reached : (exec full).finite (retMemory full) (retState full) := by
    cases full with
    | true => exact ⟨baseStart true, by with_unfolding_all rfl, .cons step1 (.cons step2 (.nil _))⟩
    | false =>
      have step3 : (exec false).step (retMemory false) (retCalleeEntry false) (retState false) := by
        with_unfolding_all
          exact ⟨rfl, ⟨_, rfl, _, ⟨nofun, .inr ⟨rfl, _, rfl, .inr ⟨by decide, ⟨0⟩, 1, ⟨1⟩, _, rfl, rfl⟩⟩⟩, rfl⟩, [], rfl⟩
      exact ⟨baseStart false, by with_unfolding_all rfl, .cons step1 (.cons step2 (.cons step3 (.nil _)))⟩
  have release : (retMemory full).EndLocals (language := Language full) (retState full).live (retState full) (retEnded full) := by
    cases full with
    | true => exact .nil _
    | false =>
      refine .cons ?_ (.nil _)
      with_unfolding_all exact ⟨⟨.head _, rfl⟩, .inr ⟨rfl, .inr ⟨_, ⟨1⟩, _, rfl, rfl, rfl, rfl⟩⟩⟩
  have ret : (retMemory full).ReturnAt (language := Language full)
      (retState full) (retState full) (retEnded full) (some ⟨u8Ty, .literal ⟨[42]⟩⟩) (retCont full) [] [u8Val] :=
    ⟨by cases full <;> rfl,
     ⟨retCalleeBlock full, by cases full <;> with_unfolding_all rfl, by cases full <;> rfl, rfl⟩,
     by cases full <;> rfl,
     by cases full <;> with_unfolding_all rfl, release, rfl⟩
  have unbacked : (retMemory full).ZeroUnbacked (language := Language full) (retState full) := by
    intro p l hl hz
    cases full with
    | true => rfl
    | false =>
      have hne : mir.entry ≠ calleeFn := by with_unfolding_all decide
      have lookupEq : (retMemory false).layoutAt calleeFn p =
          (if auxRoot = p then some (.scalar 1 1) else none) := by
        unfold Interfaces.Memory.layoutAt retMemory
        simp only [Bool.false_eq_true, ite_false, AList.lookup, ite_eq_right hne, ite_true,
          Option.bind_eq_bind, Option.bind_some]
      have hl' : (if auxRoot = p then some (.scalar 1 1) else none) = some l :=
        lookupEq.symm.trans hl
      split at hl'
      · cases Option.some.inj hl'
        exact (Nat.one_ne_zero hz).elim
      · cases hl'
  let resume := Interfaces.Memory.ReturnAt.resume (retCont full) [] (retEnded full)
  have valid : (retMemory full).ParamsValid (language := Language full) u8Param [u8Val] resume farCeiling :=
    params_valid_far (by cases full <;> with_unfolding_all decide) (by cases full <;> decide)
      (by cases full <;> with_unfolding_all rfl) (by cases full <;> with_unfolding_all rfl)
      (by cases full <;> with_unfolding_all rfl) rfl rfl rfl (by cases full <;> rfl) (by
        intro e hm hlive
        cases full with
        | true =>
          cases List.mem_singleton.mp hm
          decide
        | false =>
          rcases List.mem_cons.mp hm with rfl | tail
          · cases hlive
          · cases List.mem_singleton.mp tail
            decide)
  refine ⟨wf, reached, Interfaces.Memory.ReturnAt.progress (language := Language full) wf.2.2.2 ret valid, ?_⟩
  cases full with
  | true =>
    have noFit : ¬ (retMemory true).ParamSpaceFits (language := Language true) u8Param resume :=
      no_u8_in_full (.head _)
    have ex := (Interfaces.Memory.ReturnAt.exhausted_iff (language := Language true) wf.2.2.2 ret unbacked).mpr
      ⟨⟨farCeiling, valid⟩, noFit⟩
    exact ⟨ex.1, ex.2, fun h => noFit ((Interfaces.Memory.ReturnAt.normal_iff_fit (language := Language true) wf.2.2.2 ret unbacked valid).mp h)⟩
  | false =>
    have fits : (retMemory false).ParamSpaceFits (language := Language false) u8Param resume := by
      with_unfolding_all exact ⟨⟨0⟩, 1, ⟨2⟩, _, rfl, trivial⟩
    exact ⟨no_u8_when_callee_holds_byte,
      Interfaces.Memory.ReturnAt.normal_of_fit (language := Language false) wf.2.2.2 ret valid fits,
      fun h => ((Interfaces.Memory.ReturnAt.exhausted_iff (language := Language false) wf.2.2.2 ret unbacked).mp h).2 fits⟩

/-! #### 4. Destructure (`DestructureAt`) control -/

private def destrBindings : List (FieldId × Sigma (MemoryPlace baseLang.operands.Ty)) :=
  [(pairFld, ⟨u8Ty, dstPlace⟩)]
private def destrBlock : MemoryBlock lowering :=
  ⟨⟨0⟩, [],
    [⟨origin, .beginLifetime backingRoot⟩,
     ⟨origin, .beginLifetime auxRoot⟩,
     ⟨origin, .bind pairPlace (.literal ⟨[42]⟩)⟩,
     ⟨origin, .destructure pairPlace destrBindings⟩],
    .ret (some ⟨u8Ty, .copy dstPlace⟩), []⟩
private def destrLocals : LocalTypes baseLang.operands.Ty :=
  [(backingRoot, backingTy), (dstRoot, u8Ty), (auxRoot, pairTy)]
private def destrMemory (full : Bool) : M :=
  { project := mir.project, entry := mir.entry, signatures := mir.signatures
    functions := [(mir.entry, destrLocals, ⟨⟨0⟩, #[destrBlock]⟩)]
    storage := [(mir.entry, [(backingRoot, .materialized), (dstRoot, .materialized), (auxRoot, .virtual)])]
    layouts := [(mir.entry, [(backingRoot, .scalar (backingSize full) 1), (dstRoot, .scalar 1 1)])] }

private def destrOpenedPair (full : Bool) : State full :=
  {afterBacking full with instruction := 2, live := [auxRoot, backingRoot]}
private def destrState (full : Bool) : State full :=
  {destrOpenedPair full with
    instruction := 3
    initialized := [auxRoot]
    virtualValues := [(auxRoot, pairVal)]}

/-- Control for `DestructureAt` corollaries (`ISS-0125`): destructuring a 1-field
aggregate into an unopened materialized `u8` local exhausts atomically when full,
completes normally when space fits, and satisfies `DestructureAt.progress` in both regimes. -/
theorem destructure_transfer_control (full : Bool) :
    (destrMemory full).HandoffWF ∧
    (exec full).finite (destrMemory full) (destrState full) ∧
    (∃ next, (exec full).step (destrMemory full) (destrState full) next) ∧
    (if full then
      ((exec full).step (destrMemory full) (destrState full) {(destrState full) with resourceExhausted := true} ∧
       (exec full).result (destrMemory full) {(destrState full) with resourceExhausted := true} .resourceExhausted ∧
       ¬ ∃ next, (exec full).step (destrMemory full) (destrState full) next ∧ next.resourceExhausted = false)
    else
      ((∃ next, (exec full).step (destrMemory full) (destrState full) next ∧ next.resourceExhausted = false) ∧
       ¬ ((exec full).step (destrMemory full) (destrState full) {(destrState full) with resourceExhausted := true} ∧
          (exec full).result (destrMemory full) {(destrState full) with resourceExhausted := true} .resourceExhausted))) := by
  have wf : (destrMemory full).HandoffWF :=
    (checkHandoff_iff _).mp (by cases full <;> with_unfolding_all decide)
  have step1 : (exec full).step (destrMemory full) (baseStart full) (afterBacking full) :=
    open_backing_step (by cases full <;> with_unfolding_all rfl) rfl
      (by cases full <;> with_unfolding_all rfl) (by cases full <;> with_unfolding_all rfl)
  have step2 : (exec full).step (destrMemory full) (afterBacking full) (destrOpenedPair full) := by
    cases full <;> with_unfolding_all
      exact ⟨rfl, ⟨_, rfl, {destrOpenedPair _ with instruction := 1}, ⟨by decide, .inl ⟨rfl, rfl⟩⟩, rfl⟩, [], rfl⟩
  have step3 : (exec full).step (destrMemory full) (destrOpenedPair full) (destrState full) := by
    cases full <;> with_unfolding_all
      exact ⟨rfl, ⟨_, rfl, pairVal, destrOpenedPair _, {destrState _ with instruction := 2}, rfl,
        ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩, rfl⟩, [], rfl⟩
  have reached : (exec full).finite (destrMemory full) (destrState full) :=
    ⟨baseStart full, by cases full <;> with_unfolding_all rfl,
      .cons step1 (.cons step2 (.cons step3 (.nil _)))⟩
  have df : Interfaces.Memory.DestructureFields (Language full) (destrMemory full).declarations
      pairTy pairVal (destrBindings.map fun (f, dst) => (f, dst.1)) := by
    refine ⟨by cases full <;> with_unfolding_all decide, [pairFld], by cases full <;> with_unfolding_all rfl, ?_, ?_⟩
    · intro b hb
      cases List.mem_singleton.mp hb
      exact ⟨.head _, by cases full <;> with_unfolding_all rfl⟩
    · intro f hf hnot
      cases List.mem_singleton.mp hf
      exact (hnot (.head _)).elim
  have destr : (destrMemory full).DestructureAt (language := Language full)
      (destrState full) (destrState full) pairPlace destrBindings [u8Val] :=
    ⟨rfl, ⟨destrBlock, _, by cases full <;> with_unfolding_all rfl, rfl, rfl⟩,
     ⟨pairVal, by cases full <;> with_unfolding_all rfl, df, by cases full <;> with_unfolding_all rfl⟩⟩
  have valid : (destrMemory full).ParamsValid (language := Language full)
      (destrBindings.map Prod.snd) [u8Val] (destrState full) farCeiling :=
    params_valid_far (by cases full <;> with_unfolding_all decide) (by cases full <;> decide)
      (by cases full <;> with_unfolding_all rfl) (by cases full <;> with_unfolding_all rfl)
      (by cases full <;> with_unfolding_all rfl) (by cases full <;> decide) rfl rfl rfl (by
        intro e hm _
        cases List.mem_singleton.mp hm
        cases full <;> decide)
  refine ⟨wf, reached, Interfaces.Memory.DestructureAt.progress (language := Language full) wf.2.2.2 destr valid, ?_⟩
  cases full with
  | true =>
    have noFit : ¬ (destrMemory true).ParamSpaceFits (language := Language true)
        (destrBindings.map Prod.snd) (destrState true) :=
      no_u8_in_full (.head _)
    have ex := (Interfaces.Memory.DestructureAt.exhausted_iff (language := Language true) wf.2.2.2 destr).mpr
      ⟨⟨farCeiling, valid⟩, noFit⟩
    exact ⟨ex.1, ex.2, fun h => noFit ((Interfaces.Memory.DestructureAt.normal_iff_fit (language := Language true) wf.2.2.2 destr valid).mp h)⟩
  | false =>
    have fits : (destrMemory false).ParamSpaceFits (language := Language false)
        (destrBindings.map Prod.snd) (destrState false) := by
      with_unfolding_all exact ⟨⟨0⟩, 1, ⟨1⟩, _, rfl, trivial⟩
    exact ⟨Interfaces.Memory.DestructureAt.normal_of_fit (language := Language false) wf.2.2.2 destr valid fits,
      fun h => ((Interfaces.Memory.DestructureAt.exhausted_iff (language := Language false) wf.2.2.2 destr).mp h).2 fits⟩

end TransferProgressControls

end Concrete.Memory
