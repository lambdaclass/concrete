import ConcreteSpec.Spec.Frame
import ConcreteSpec.Proofs.Memory
import ConcreteSpec.Proofs.Exec

/-!
# Laws of logical footprints and frames (foundation C10)

Byte-range disjointness, preservation under sequential composition, and the frame laws of C4 byte
store, release, exposure and fresh allocation; fresh-block disjointness and C1 callee-contract
framing. Call entry and return restore caller locals and continuations, including after cleanup.
The ABI laws cover typed locations, callee-saved and non-clobbered registers, stack restoration and
accounting, paired `kept` retention, and launcher admission and entry/exit relations.
-/
namespace ConcreteSpec.Core.Frame
open ConcreteSpec.Core.Mem
open ConcreteSpec.Core.Hybrid

/-- Helper (congruence). Reflexivity of the preservation relation; used for idle execution. (approved: PENDING)
 The empty footprint frames every operation. -/
theorem preserves_refl (st : State) : Footprint.empty.Preserves st st := by
  refine ⟨?_, ?_, ?_⟩
  · intro b i x hx ho
    simp only [hx, Option.map_some]
  · intro b x hx ho
    simp only [hx, Option.map_some]
  · intro p hp
    rfl

/-- Helper (inversion). Decomposes byte nonmembership in a footprint union. (approved: PENDING)
 Membership outside a union is membership outside both components. -/
theorem byteOutside_union {f g : Footprint} {b : MemBlockId} {i : Nat}
    (h : (f.union g).ByteOutside b i) : f.ByteOutside b i ∧ g.ByteOutside b i := by
  constructor <;> intro r hr <;> apply h r <;> simp [Footprint.union, hr]

/-- Helper (inversion). Decomposes metadata nonmembership in a footprint union. (approved: PENDING)
 Block metadata outside a union is outside both components. -/
theorem blockOutside_union {f g : Footprint} {b : MemBlockId}
    (h : (f.union g).BlockOutside b) : f.BlockOutside b ∧ g.BlockOutside b := by
  have hb : b ∉ f.blocks ++ g.blocks := h
  simp only [List.mem_append, not_or] at hb
  exact hb

/-- Helper (inversion). Decomposes place nonmembership in a footprint union. (approved: PENDING)
 Place membership outside a union is membership outside both components. -/
theorem placeOutside_union {f g : Footprint} {p : Place}
    (h : (f.union g).PlaceOutside p) : f.PlaceOutside p ∧ g.PlaceOutside p := by
  have hp : p ∉ f.places ++ g.places := h
  simp only [List.mem_append, not_or] at hp
  exact hp

/-- Helper (inversion). Extracts identity inequality from singleton nonmembership. (approved: PENDING)
 A lifecycle footprint excludes metadata framing for its own block, even when that block is
zero-sized. -/
theorem blockOutside_block {b c : MemBlockId} (h : (Footprint.block b).BlockOutside c) : c ≠ b := by
  intro hcb
  subst c
  exact h (by simp [Footprint.block])

/-- Helper (congruence). Transitivity under footprint union, used by execution induction. (approved: PENDING)
 Sequential composition frames the union of the two operations' footprints. -/
theorem preserves_seq {st st' st'' : State} {f g : Footprint}
    (h₁ : f.Preserves st st') (h₂ : g.Preserves st' st'') : (f.union g).Preserves st st'' := by
  refine ⟨?_, ?_, ?_⟩
  · intro b i x hx ho
    obtain ⟨hf, hg⟩ := byteOutside_union ho
    have he₁ := h₁.1 b i x hx hf
    cases hy : st'.memory.block b with
    | none => simp [hy] at he₁
    | some y =>
      simp only [hy, Option.map_some, Option.some.injEq] at he₁
      have he₂ := h₂.1 b i y hy hg
      exact he₂.trans (congrArg some he₁)
  · intro b x hx ho
    obtain ⟨hf, hg⟩ := blockOutside_union ho
    have hm₁ := h₁.2.1 b x hx hf
    cases hy : st'.memory.block b with
    | none => simp [hy] at hm₁
    | some y =>
      simp only [hy, Option.map_some, Option.some.injEq] at hm₁
      have hm₂ := h₂.2.1 b y hy hg
      exact hm₂.trans (congrArg some hm₁)
  · intro p hp
    obtain ⟨hf, hg⟩ := placeOutside_union hp
    exact (h₂.2.2 p hg).trans (h₁.2.2 p hf)

/-- Helper (inversion). Decomposes successful C4 stores for the instruction and execution soundness laws. (approved: PENDING)
 A successful byte store preserves every old byte outside the exact access range, every block's
metadata (including the written block's), and every structured place. A zero-byte store has the
empty footprint. -/
theorem store_frame (tgt : Target) (st : State) (hw : st.memory.WellFormed tgt) (m' : Memory)
    (p : Ptr) (bs : List MByte) (a : Nat) (access : Option (Option (MemBlockId × Nat)))
    (ha : st.memory.access tgt p bs.length a = access)
    (h : st.memory.store tgt p bs a = some m') :
    Footprint.Preserves st { st with memory := m' } (accessFootprint access bs.length) := by
  cases access with
  | none => simp [Memory.store, ha] at h
  | some r =>
    cases r with
    | none =>
      simp only [Memory.store, ha, Option.some.injEq] at h
      cases h
      exact preserves_refl st
    | some pair =>
      obtain ⟨b, off⟩ := pair
      refine ⟨?_, ?_, ?_⟩
      · intro c i x hx ho
        by_cases hc : c = b
        · subst c
          apply store_same_block_frame tgt st.memory _ hw p bs a b off i x ha hx h
          have hn : ¬ ByteRange.Contains ⟨b, off, bs.length⟩ b i :=
            ho _ (by simp [accessFootprint, Footprint.byte])
          simp only [ByteRange.Contains, true_and] at hn
          omega
        · rw [store_other_block tgt st.memory m' p bs a b off c ha h hc, hx]
          rfl
      · intro c x hx ho
        by_cases hc : c = b
        · subst c
          simp only [Memory.store, ha, Option.some.injEq] at h
          subst m'
          simp [blockMetadata, block_modify, hx]
        · rw [store_other_block tgt st.memory m' p bs a b off c ha h hc, hx]
          rfl
      · intro q hq
        rfl

private def metadataStoreBlock : Block :=
  ⟨⟨0⟩, 4096, 2, 1, true, false, [.uninit, .uninit]⟩

private def metadataStoreBefore : State :=
  ⟨[], ⟨[(⟨0⟩, metadataStoreBlock)], 1⟩⟩

/-- Helper (computation). Constructs the closed memory fixture used by the execution witnesses. (approved: PENDING) -/
private theorem metadataStoreWellFormed : metadataStoreBefore.memory.WellFormed linuxX8664 :=
  alloc_wellFormed linuxX8664 Memory.empty (empty_wellFormed _) ⟨0⟩ 4096 2 1 ⟨0⟩
    metadataStoreBefore.memory rfl

private def metadataStorePtr : Ptr := ⟨⟨0⟩, 4096, some ⟨0⟩⟩

private def metadataStoreAfter : State :=
  { metadataStoreBefore with memory := metadataStoreBefore.memory.modify ⟨0⟩ fun x =>
    { x with bytes := [.init 7 none, .uninit] } }

private def metadataStoreBadAfter : State :=
  { metadataStoreAfter with memory := metadataStoreAfter.memory.modify ⟨0⟩ fun x =>
    { x with live := false } }

/-- Accepted: an actual nonempty store and its composition with an empty footprint preserve the
written block's metadata as well as the unwritten byte. -/
example :
    Footprint.Preserves metadataStoreBefore metadataStoreAfter (.byte ⟨⟨0⟩, 0, 1⟩) ∧
    Footprint.Preserves metadataStoreBefore metadataStoreAfter
      (Footprint.union .empty (.byte ⟨⟨0⟩, 0, 1⟩)) := by
  have hs : Footprint.Preserves metadataStoreBefore metadataStoreAfter (.byte ⟨⟨0⟩, 0, 1⟩) :=
    store_frame linuxX8664 metadataStoreBefore metadataStoreWellFormed metadataStoreAfter.memory
      metadataStorePtr [.init 7 none] 1 (some (some (⟨0⟩, 0))) (by decide) rfl
  exact ⟨hs, preserves_seq (preserves_refl _) hs⟩

/-- Refused: changing the written block's liveness violates the store and sequence frame laws;
their byte footprints do not excuse a metadata mutation. -/
example :
    ¬ Footprint.Preserves metadataStoreBefore metadataStoreBadAfter (.byte ⟨⟨0⟩, 0, 1⟩) ∧
    ¬ Footprint.Preserves metadataStoreBefore metadataStoreBadAfter
      (Footprint.union .empty (.byte ⟨⟨0⟩, 0, 1⟩)) := by
  constructor <;> intro h
  all_goals
    have hm := h.2.1 ⟨0⟩ metadataStoreBlock (by decide) (by decide)
    simp [metadataStoreBadAfter, metadataStoreAfter, metadataStoreBefore, metadataStoreBlock,
      blockMetadata, Memory.block, Memory.modify, AList.update, AList.lookup] at hm

/-- Helper (inversion). Decomposes successful C4 release for the instruction and execution soundness laws. (approved: PENDING)
 Releasing a block may change its lifecycle metadata, but frames all bytes and all metadata of
other blocks and every structured place. -/
theorem free_frame (st : State) (b : MemBlockId) (x : Block) (m' : Memory)
    (hx : st.memory.block b = some x) (h : st.memory.free b = some m') :
    Footprint.Preserves st { st with memory := m' } (.block b) := by
  unfold Memory.free at h
  rw [hx] at h
  by_cases hl : x.live = true
  · simp only [hl, ↓reduceIte, Option.some.injEq] at h
    subst m'
    refine ⟨?_, ?_, ?_⟩
    · intro c i y hy ho
      by_cases hc : c = b
      · subst c
        rw [hx] at hy
        cases hy
        simp only [block_modify, ↓reduceIte, hx, Option.map_some]
      · simp only [block_modify, hc, ↓reduceIte, hy, Option.map_some]
    · intro c y hy ho
      have hc : c ≠ b := blockOutside_block ho
      simp only [block_modify, hc, ↓reduceIte, hy, Option.map_some]
    · intro q hq
      rfl
  · simp [hl] at h

/-- Helper (inversion). Decomposes successful C4 exposure for the instruction and execution soundness laws. (approved: PENDING)
 Exposure may change only the exposed flag of the provenance block; every byte and all metadata
of other blocks and every place are preserved. -/
theorem expose_frame (st : State) (m' : Memory) (p : Ptr) (b : MemBlockId) (x : Block)
    (hp : p.prov = some b) (hx : st.memory.block b = some x) (h : st.memory.expose p = some m') :
    Footprint.Preserves st { st with memory := m' } (.block b) := by
  obtain ⟨c, y, hp', hx', hl, rfl⟩ := expose_result st.memory m' p h
  rw [hp] at hp'
  cases hp'
  rw [hx] at hx'
  cases hx'
  refine ⟨?_, ?_, ?_⟩
  · intro d i z hz ho
    by_cases hd : d = b
    · subst d
      rw [hx] at hz
      cases hz
      simp only [block_modify, ↓reduceIte, hx, Option.map_some]
    · simp only [block_modify, hd, ↓reduceIte, hz, Option.map_some]
  · intro d z hz ho
    have hd : d ≠ b := blockOutside_block ho
    simp only [block_modify, hd, ↓reduceIte, hz, Option.map_some]
  · intro q hq
    rfl

/-- Helper (inversion). Decomposes successful C4 allocation for the instruction and execution soundness laws. (approved: PENDING)
 Fresh allocation preserves every old byte, every old block's metadata and every place. The
footprint is the complete new block; old bytes are necessarily outside it because their allocation
identities differ. -/
theorem alloc_frame (tgt : Target) (st : State) (hw : st.memory.WellFormed tgt)
    (space : AddrSpace) (base size align : Nat) (b : MemBlockId) (m' : Memory)
    (h : st.memory.alloc tgt space base size align = .ok (b, m')) :
    Footprint.Preserves st { st with memory := m' } (.byte (fullBlock b size)) := by
  obtain ⟨hb, hm, hfits⟩ := alloc_result tgt st.memory space base size align b m' h
  refine ⟨?_, ?_, ?_⟩
  · intro c i x hx ho
    have hne : b ≠ c := alloc_ne_existing tgt st.memory hw space base size align b c m' x h hx
    subst b
    rw [hm]
    change AList.lookup st.memory.blocks c = some x at hx
    simp only [Memory.block, AList.lookup, hne, ↓reduceIte, hx, Option.map_some]
  · intro c x hx ho
    have hne : b ≠ c := alloc_ne_existing tgt st.memory hw space base size align b c m' x h hx
    subst b
    rw [hm]
    change AList.lookup st.memory.blocks c = some x at hx
    simp only [Memory.block, AList.lookup, hne, ↓reduceIte, hx, Option.map_some]
  · intro p hp
    rfl

/-- Helper (computation). Computes the protected observations of a single structured-place update. (approved: PENDING)
 Replacing one structured place has exactly that place's footprint; bytes and block metadata are
untouched. -/
theorem set_frame (st : State) (p : Place) (c : Cell) :
    (Footprint.place p).Preserves st (st.set p c) := by
  refine ⟨?_, ?_, ?_⟩
  · intro b i x hx ho
    simp only [State.memory_set, hx, Option.map_some]
  · intro b x hx ho
    simp only [State.memory_set, hx, Option.map_some]
  · intro q hq
    have hqp : q ≠ p := by simpa [Footprint.place, Footprint.PlaceOutside] using hq
    have hpq : p ≠ q := Ne.symm hqp
    simp [hpq]

/-- Helper (inversion). Reduces a successful Hybrid structured write to the place-update case. (approved: PENDING)
 A successful typed write to structured storage has exactly the written place's footprint. -/
theorem write_structured_frame (tgt : Target) (st st' : State) (p : Place) (c : Cell)
    (old v : Value) (hc : st.places p = some c) (hs : c.storage = .structured old)
    (h : st.write tgt p v = some st') : (Footprint.place p).Preserves st st' := by
  unfold State.write at h
  simp only [hc] at h
  dsimp at h
  rw [hs] at h
  split at h
  · simp only [Option.some.injEq] at h
    subst st'
    exact set_frame st p { c with storage := .structured v }
  · simp at h

/-- Helper (inversion). Reduces a successful Hybrid materialized write to its C4 store. (approved: PENDING)
 A successful typed write to materialized storage has the byte footprint of its actual store. -/
theorem write_materialized_frame (tgt : Target) (st st' : State) (hw : st.memory.WellFormed tgt)
    (p : Place) (c : Cell) (r : Range) (v : Value)
    (hc : st.places p = some c) (hs : c.storage = .materialized r)
    (access : Option (Option (MemBlockId × Nat)))
    (ha : st.memory.access tgt r.ptr (encode tgt v).length r.align = access)
    (h : st.write tgt p v = some st') :
    (accessFootprint access (encode tgt v).length).Preserves st st' := by
  unfold State.write at h
  simp only [hc] at h
  dsimp at h
  rw [hs] at h
  split at h
  · cases hm : st.memory.store tgt r.ptr (encode tgt v) r.align with
    | none => simp [hm] at h
    | some m =>
      simp only [hm, Option.bind_some, Option.some.injEq] at h
      subst st'
      exact store_frame tgt st hw m r.ptr (encode tgt v) r.align access ha hm
  · simp at h

/-- Helper (inversion). Reduces a successful Hybrid raw write to its C4 store. (approved: PENDING)
 A raw Hybrid write has the byte footprint of its underlying memory store and touches no place. -/
theorem rawWrite_frame (tgt : Target) (st st' : State) (hw : st.memory.WellFormed tgt)
    (p : Ptr) (bs : List MByte) (align : Nat)
    (access : Option (Option (MemBlockId × Nat)))
    (ha : st.memory.access tgt p bs.length align = access)
    (h : st.rawWrite tgt p bs align = some st') :
    (accessFootprint access bs.length).Preserves st st' := by
  unfold State.rawWrite at h
  cases hm : st.memory.store tgt p bs align with
  | none => simp [hm] at h
  | some m =>
    simp [hm] at h
    subst st'
    exact store_frame tgt st hw m p bs align access ha hm

/-- Accepted: the same actual byte store, through the raw Hybrid interface, preserves the written
block's metadata. -/
example : Footprint.Preserves metadataStoreBefore metadataStoreAfter (.byte ⟨⟨0⟩, 0, 1⟩) :=
  rawWrite_frame linuxX8664 metadataStoreBefore metadataStoreAfter metadataStoreWellFormed
    metadataStorePtr [.init 7 none] 1 (some (some (⟨0⟩, 0))) (by decide) rfl

/-- Refused: the raw-write frame law rejects the same planted liveness mutation. -/
example : ¬ Footprint.Preserves metadataStoreBefore metadataStoreBadAfter (.byte ⟨⟨0⟩, 0, 1⟩) := by
  intro h
  have hm := h.2.1 ⟨0⟩ metadataStoreBlock (by decide) (by decide)
  simp [metadataStoreBadAfter, metadataStoreAfter, metadataStoreBefore, metadataStoreBlock,
    blockMetadata, Memory.block, Memory.modify, AList.update, AList.lookup] at hm

/-- Law (subject `ConcreteSpec.Core.Frame.fullBlock`).
The allocated identity is disjoint from every range whose block existed before allocation. -/
theorem alloc_fresh_disjoint (tgt : Target) (st : State)
    (space : AddrSpace) (base size align : Nat) (b : MemBlockId) (m' : Memory)
    (r : ByteRange) (x : Block)
    (hw : st.memory.WellFormed tgt)
    (h : st.memory.alloc tgt space base size align = .ok (b, m'))
    (hx : st.memory.block r.block = some x) :
    (fullBlock b size).Disjoint r :=
  .inl (alloc_ne_existing tgt st.memory hw space base size align b r.block m' x h hx)

private def metadataAllocMemory : Memory :=
  ⟨[(⟨1⟩, candidate ⟨0⟩ 8192 2 1), (⟨0⟩, metadataStoreBlock)], 2⟩

/-- Instance (law `ConcreteSpec.Core.Frame.alloc_fresh_disjoint`, case `fresh_block`). -/
example : (fullBlock ⟨1⟩ 2).Disjoint ⟨⟨0⟩, 0, 1⟩ :=
  alloc_fresh_disjoint linuxX8664 metadataStoreBefore ⟨0⟩ 8192 2 1 ⟨1⟩ metadataAllocMemory
    ⟨⟨0⟩, 0, 1⟩ metadataStoreBlock metadataStoreWellFormed rfl rfl

/-- A planted allocation range constructor reuses the existing block identity `⟨0⟩` instead of
recording the newly allocated block identity. -/
def aliasingFullBlock (_b : MemBlockId) (size : Nat) : ByteRange := ⟨⟨0⟩, 0, size⟩

/-- Planted (law `ConcreteSpec.Core.Frame.alloc_fresh_disjoint`, case `fresh_block`,
subject `ConcreteSpec.Core.Frame.fullBlock`, mutant `ConcreteSpec.Core.Frame.aliasingFullBlock`). -/
example : metadataStoreBefore.memory.WellFormed linuxX8664 ∧
    metadataStoreBefore.memory.alloc linuxX8664 ⟨0⟩ 8192 2 1 = .ok (⟨1⟩, metadataAllocMemory) ∧
    metadataStoreBefore.memory.block (⟨⟨0⟩, 0, 1⟩ : ByteRange).block = some metadataStoreBlock ∧
    ¬ (aliasingFullBlock ⟨1⟩ 2).Disjoint ⟨⟨0⟩, 0, 1⟩ :=
  ⟨metadataStoreWellFormed, rfl, rfl, by decide⟩

/-- Helper (computation). Computes the C1 call-entry step used by caller restoration. (approved: PENDING)
 Entering a C1 call saves the caller's locals in a `.ret` frame, binds the evaluated arguments to
callee parameters, and preserves the caller's continuation below that frame. -/
theorem step_call_saves_caller {O : Exec.Operands} (P : Exec.Program O) (dst : Option Var)
    (f : FnId) (args : List O.Expr) (site : SiteId) (caller : AList Var O.Val)
    (st stArgs stEnter : O.Store) (k : List (Exec.Frame O)) (vs : List O.Val) (fn : Exec.Fn O)
    (hv : Exec.evalArgs O caller st args = .ok (vs, stArgs))
    (hf : AList.lookup P.fns f = some fn) (hlen : fn.params.length = vs.length)
    (hl : O.life (.enter (fn.params.map Prod.fst)) stArgs = some stEnter) :
    Exec.step P ⟨.exec (.call dst f args site), caller, st, k⟩ =
      some (.next ⟨.exec fn.body, (fn.params.map Prod.fst).zip vs, stEnter,
        .scope fn.block (fn.params.map Prod.fst) [] :: .ret dst caller :: k⟩) := by
  simp [Exec.step, Exec.onRes, hv, hf, hlen, hl]

/-- Helper (computation). Computes the C1 final return step used by caller restoration. (approved: PENDING)
 A returned value reaching a `.ret` frame restores the caller's locals and continuation, binding
the result at any requested destination. Leave and destination-bind instrumentation thread the shared
state through the same step. -/
theorem step_return_restores_caller {O : Exec.Operands} (P : Exec.Program O) (v : O.Val)
    (dst : Option Var) (caller callee : AList Var O.Val) (st stLeave stBind : O.Store)
    (k : List (Exec.Frame O)) (hl : O.life .leave st = some stLeave)
    (hb : Exec.bindLife O dst stLeave = some stBind) :
    Exec.step P ⟨.exiting (.ret v), callee, st, .ret dst caller :: k⟩ =
      some (.next ⟨.done, Exec.bindDst dst v caller, stBind, k⟩) :=
  Exec.step_return_caller P v dst caller callee st k stLeave stBind hl hb

/-- Helper (inversion). Inverts the complete-call derivation for the caller-restoration law,
connecting the entry `.call` step that pushes `.ret dst caller :: k` to the final return step
that pops it and binds `dst` in `caller`. (approved: PENDING) -/
theorem c1Call_restores_caller {O : Exec.Operands} {f : FnId} {P : Exec.Program O}
    {entry exit : Exec.Config O} (h : C1Call O f P entry exit) :
    ∃ (fn : Exec.Fn O) (dst : Option Var) (result : O.Val) (caller : AList Var O.Val)
      (k : List (Exec.Frame O)) (args : List O.Expr) (site : SiteId)
      (vs : List O.Val) (st stEnter : O.Store),
      entry = ⟨.exec (.call dst f args site), caller, st, k⟩ ∧
      Exec.step P entry =
        some (.next ⟨.exec fn.body, (fn.params.map Prod.fst).zip vs, stEnter,
          .scope fn.block (fn.params.map Prod.fst) [] :: .ret dst caller :: k⟩) ∧
      exit.ctrl = .done ∧ exit.locals = Exec.bindDst dst result caller ∧ exit.frames = k := by
  obtain ⟨fn, dst, caller, k, args, site, vs, st, stArgs, stEnter, stReturn, callee, tr, result,
    hf, he, hv, hlen, hl, hreturn⟩ := h
  have hCallStep : Exec.step P entry =
      some (.next ⟨.exec fn.body, (fn.params.map Prod.fst).zip vs, stEnter,
        .scope fn.block (fn.params.map Prod.fst) [] :: .ret dst caller :: k⟩) := by
    rw [he]
    exact step_call_saves_caller P dst f args site caller st stArgs stEnter k vs fn hv hf hlen hl
  refine ⟨fn, dst, result, caller, k, args, site, vs, st, stEnter, he, hCallStep, ?_⟩
  cases hreturn with
  | inl hexplicit =>
    obtain ⟨hs, hr⟩ := hexplicit
    cases hl' : O.life .leave stReturn with
    | none => simp [Exec.step, Exec.stepExit, hl'] at hr
    | some stLeave =>
      cases hb : Exec.bindLife O dst stLeave with
      | none => simp [Exec.step, Exec.stepExit, hl', hb] at hr
      | some stBind =>
        have hRet := step_return_restores_caller P result dst caller callee stReturn stLeave stBind k hl' hb
        rw [hRet] at hr
        simp only [Option.some.injEq, Exec.Out.next.injEq] at hr
        cases hr
        exact ⟨rfl, rfl, rfl⟩
  | inr hfall =>
    obtain ⟨hresult, hs, hr⟩ := hfall
    subst result
    cases hl' : O.life .leave stReturn with
    | none => simp [Exec.step, hl'] at hr
    | some stLeave =>
      cases hb : Exec.bindLife O dst stLeave with
      | none => simp [Exec.step, hl', hb] at hr
      | some stBind =>
        simp only [Exec.step, hl', Option.bind_some, hb, Option.map_some,
          Option.some.injEq, Exec.Out.next.injEq] at hr
        cases hr
        exact ⟨rfl, rfl, rfl⟩

private def c1Operands : Exec.Operands where
  Val := Nat
  Expr := Nat
  Ty := Unit
  Store := Unit
  Prim := Unit
  Event := Unit
  unitVal := 0
  unitTy := ()
  isCopy := fun _ => true
  eval := fun _ _ n => .ok (n, ())
  life := fun _ st => some st
  test := fun n => some (n != 0)
  allows := fun _ _ _ _ => false

private def c1FnId : FnId := ⟨⟨[]⟩, "seven"⟩

private def c1Fn : Exec.Fn c1Operands := ⟨[], (), ⟨0⟩, .ret (7 : Nat) ⟨0⟩⟩

private def c1Program : Exec.Program c1Operands := ⟨[(c1FnId, c1Fn)]⟩

private def c1SavedVar : Var := ⟨"caller_saved", 0⟩

private def c1DstVar : Var := ⟨"out", 1⟩

private def c1CallerLocals : AList Var Nat := [(c1SavedVar, (42 : Nat))]

private def c1Entry : Exec.Config c1Operands :=
  ⟨.exec (.call (some c1DstVar) c1FnId [] ⟨0⟩), c1CallerLocals, (), []⟩

private def c1Boundary : Exec.Config c1Operands :=
  ⟨.exiting (.ret (7 : Nat)), [], (), [.ret (some c1DstVar) c1CallerLocals]⟩

private def c1Exit : Exec.Config c1Operands :=
  ⟨.done, [(c1DstVar, (7 : Nat)), (c1SavedVar, (42 : Nat))], (), []⟩

/-- Helper (computation). Constructs the closed explicit-return execution witness. (approved: PENDING) -/
private theorem c1ExplicitCall : C1Call c1Operands c1FnId c1Program c1Entry c1Exit := by
  have hs : Exec.Steps c1Program c1Entry [] c1Boundary := by
    apply Exec.steps_next rfl
    apply Exec.steps_next rfl
    apply Exec.steps_next rfl
    exact Exec.Steps.refl _
  exact ⟨c1Fn, some c1DstVar, c1CallerLocals, [], [], ⟨0⟩, [], (), (), (), (), [], [], (7 : Nat),
    rfl, rfl, rfl, rfl, rfl, .inl ⟨hs, rfl⟩⟩

/-- Accepted: an explicit return reaches its own `.ret` frame after evaluating and cleaning up the
callee, then takes the final return step. -/
example : C1Call c1Operands c1FnId c1Program c1Entry c1Exit := c1ExplicitCall

private def c1UnitFnId : FnId := ⟨⟨[]⟩, "unit"⟩

private def c1UnitFn : Exec.Fn c1Operands := ⟨[], (), ⟨0⟩, .skip⟩

private def c1UnitProgram : Exec.Program c1Operands := ⟨[(c1UnitFnId, c1UnitFn)]⟩

private def c1UnitEntry : Exec.Config c1Operands :=
  ⟨.exec (.call none c1UnitFnId [] ⟨0⟩), [], (), []⟩

private def c1UnitBoundary : Exec.Config c1Operands :=
  ⟨.done, [], (), [.ret none []]⟩

private def c1UnitExit : Exec.Config c1Operands := ⟨.done, [], (), []⟩

/-- Accepted: a Unit body can reach its `.ret` frame by falling through; the actual final step then
binds `unitVal` and restores the caller. -/
example : C1Call c1Operands c1UnitFnId c1UnitProgram c1UnitEntry c1UnitExit := by
  have hs : Exec.Steps c1UnitProgram c1UnitEntry [] c1UnitBoundary := by
    apply Exec.steps_next rfl
    apply Exec.steps_next rfl
    apply Exec.steps_next rfl
    exact Exec.Steps.refl _
  exact ⟨c1UnitFn, none, [], [], [], ⟨0⟩, [], (), (), (), (), [], [], (0 : Nat),
    rfl, rfl, rfl, rfl, rfl, .inr ⟨rfl, hs, rfl⟩⟩

/-- Law (subject `ConcreteSpec.Core.Exec.bindDst`).
Complete calls push the caller's locals and continuation on entry and restore them with the
returned value bound at `dst` on exit. -/
theorem caller_restoration (O : Exec.Operands) (f : FnId) (P : Exec.Program O)
    (entry exit : Exec.Config O) (call : C1Call O f P entry exit) :
    ∃ (fn : Exec.Fn O) (dst : Option Var) (result : O.Val) (caller : AList Var O.Val)
      (k : List (Exec.Frame O)) (args : List O.Expr) (site : SiteId)
      (vs : List O.Val) (st stEnter : O.Store),
      entry = ⟨.exec (.call dst f args site), caller, st, k⟩ ∧
      Exec.step P entry =
        some (.next ⟨.exec fn.body, (fn.params.map Prod.fst).zip vs, stEnter,
          .scope fn.block (fn.params.map Prod.fst) [] :: .ret dst caller :: k⟩) ∧
      exit.ctrl = .done ∧ exit.locals = Exec.bindDst dst result caller ∧ exit.frames = k :=
  c1Call_restores_caller call

/-- Instance (law `ConcreteSpec.Core.Frame.caller_restoration`, case `explicit_return`). -/
example :
    ∃ (fn : Exec.Fn c1Operands) (dst : Option Var) (result : Nat) (caller : AList Var Nat)
      (k : List (Exec.Frame c1Operands)) (args : List Nat) (site : SiteId)
      (vs : List Nat) (st stEnter : Unit),
      c1Entry = ⟨.exec (.call dst c1FnId args site), caller, st, k⟩ ∧
      Exec.step c1Program c1Entry =
        some (.next ⟨.exec fn.body, (fn.params.map Prod.fst).zip vs, stEnter,
          .scope fn.block (fn.params.map Prod.fst) [] :: .ret dst caller :: k⟩) ∧
      c1Exit.ctrl = .done ∧ c1Exit.locals = Exec.bindDst dst result caller ∧ c1Exit.frames = k :=
  caller_restoration c1Operands c1FnId c1Program c1Entry c1Exit c1ExplicitCall

/-- A planted destination binder drops the caller's saved locals upon return instead of restoring
them alongside the bound destination. -/
def dropCallerBindDst {V : Type} (dst : Option Var) (v : V) (_caller : AList Var V) :
    AList Var V :=
  match dst with
  | none => []
  | some x => [(x, v)]

/-- Planted (law `ConcreteSpec.Core.Frame.caller_restoration`, case `explicit_return`,
subject `ConcreteSpec.Core.Exec.bindDst`, mutant `ConcreteSpec.Core.Frame.dropCallerBindDst`). -/
example : C1Call c1Operands c1FnId c1Program c1Entry c1Exit ∧
    ¬ (∃ (fn : Exec.Fn c1Operands) (dst : Option Var) (result : Nat) (caller : AList Var Nat)
      (k : List (Exec.Frame c1Operands)) (args : List Nat) (site : SiteId)
      (vs : List Nat) (st stEnter : Unit),
      c1Entry = ⟨.exec (.call dst c1FnId args site), caller, st, k⟩ ∧
      Exec.step c1Program c1Entry =
        some (.next ⟨.exec fn.body, (fn.params.map Prod.fst).zip vs, stEnter,
          .scope fn.block (fn.params.map Prod.fst) [] :: .ret dst caller :: k⟩) ∧
      c1Exit.ctrl = .done ∧
      c1Exit.locals = dropCallerBindDst dst result caller ∧
      c1Exit.frames = k) := by
  refine ⟨c1ExplicitCall, ?_⟩
  intro h
  obtain ⟨fn, dst, result, caller, k, args, site, vs, st, stEnter,
    he, _hstep, _hctrl, hlocals, _hframes⟩ := h
  have hdst : dst = some c1DstVar := by
    have := congrArg Exec.Config.ctrl he
    cases this
    rfl
  subst dst
  have hlen := congrArg List.length hlocals
  change 2 = 1 at hlen
  contradiction

/-- Law (subject `ConcreteSpec.Core.Frame.runEffect`).
Actual C4 and Hybrid instruction execution matches definedness and preserves bytes, metadata and
places outside its computed effects. -/
theorem runEffect_frame (tgt : Target) (before : State) (command : EffectCommand)
    (wellFormed : before.memory.WellFormed tgt) :
    EffectResult tgt before command (runEffect tgt before command) := by
  cases command with
  | store pointer bytes alignment =>
    cases hs : before.memory.store tgt pointer bytes alignment with
    | none => simp [runEffect, EffectResult, EffectCommand.Defined, hs]
    | some memory =>
      simp only [runEffect, hs, Option.map_some, EffectResult, EffectCommand.Defined, Option.isSome]
      exact ⟨ trivial, store_frame tgt before wellFormed memory pointer bytes alignment _ rfl hs ⟩
  | free block =>
    cases hb : before.memory.block block with
    | none => simp [runEffect, Memory.free, hb, EffectResult, EffectCommand.Defined]
    | some x =>
      cases hs : before.memory.free block with
      | none => simp [runEffect, hs, EffectResult, EffectCommand.Defined]
      | some memory =>
        simp only [runEffect, hs, Option.map_some, EffectResult, EffectCommand.Defined, Option.isSome]
        exact ⟨ trivial, free_frame before block x memory hb hs ⟩
  | expose pointer =>
    cases hp : pointer.prov with
    | none => simp [runEffect, hp, EffectResult, EffectCommand.Defined]
    | some block =>
      cases hb : before.memory.block block with
      | none => simp [runEffect, hp, Memory.expose, hb, EffectResult, EffectCommand.Defined]
      | some x =>
        cases hs : before.memory.expose pointer with
        | none => simp [runEffect, hp, hs, EffectResult, EffectCommand.Defined]
        | some memory =>
          simp only [runEffect, hp, Option.bind_some, hs, Option.map_some, EffectResult,
            EffectCommand.Defined, Option.isSome]
          exact ⟨ trivial, expose_frame before memory pointer block x hp hb hs ⟩
  | alloc space base size alignment =>
    cases ha : before.memory.alloc tgt space base size alignment with
    | error _ => simp [runEffect, ha, EffectResult, EffectCommand.Defined]
    | ok output =>
      obtain ⟨block, memory⟩ := output
      simp only [runEffect, ha, EffectResult, EffectCommand.Defined]
      exact ⟨ trivial, alloc_frame tgt before wellFormed space base size alignment block memory ha ⟩
  | set place cell =>
    exact ⟨ trivial, set_frame before place cell ⟩
  | write place value =>
    cases hw : before.write tgt place value with
    | none => simp [runEffect, hw, EffectResult, EffectCommand.Defined]
    | some after =>
      simp only [runEffect, hw, Option.map_some, EffectResult, EffectCommand.Defined, Option.isSome]
      refine ⟨ trivial, ?_ ⟩
      have hplaces : ∃ c, before.places place = some c := by
        unfold State.write at hw
        cases hc : before.places place with
        | none => simp [hc] at hw
        | some c => exact ⟨c, rfl⟩
      obtain ⟨c, hc⟩ := hplaces
      cases c with
      | mk ty storage =>
        cases storage with
        | structured old =>
          simp only [writeFootprint, hc]
          exact write_structured_frame tgt before after place ⟨ty, .structured old⟩ old value hc rfl hw
        | materialized r =>
          simp only [writeFootprint, hc]
          exact write_materialized_frame tgt before after wellFormed place ⟨ty, .materialized r⟩ r value hc rfl _ rfl hw
  | rawWrite pointer bytes alignment =>
    cases hr : before.rawWrite tgt pointer bytes alignment with
    | none => simp [runEffect, hr, EffectResult, EffectCommand.Defined]
    | some after =>
      simp only [runEffect, hr, Option.map_some, EffectResult, EffectCommand.Defined, Option.isSome]
      exact ⟨ trivial, rawWrite_frame tgt before after wellFormed pointer bytes alignment _ rfl hr ⟩

/-- Instance (law `ConcreteSpec.Core.Frame.runEffect_frame`, case `byte_store`). -/
example : EffectResult linuxX8664 metadataStoreBefore
    (.store metadataStorePtr [.init 7 none] 1)
    (runEffect linuxX8664 metadataStoreBefore (.store metadataStorePtr [.init 7 none] 1)) :=
  runEffect_frame linuxX8664 metadataStoreBefore (.store metadataStorePtr [.init 7 none] 1)
    metadataStoreWellFormed

/-- A planted executor corrupts block metadata after executing the instruction. -/
def corruptEffect (tgt : Target) (before : State) (command : EffectCommand) :
    Option (State × Footprint) :=
  (runEffect tgt before command).map fun (after, footprint) =>
    ({ after with memory := after.memory.modify ⟨0⟩ fun block => { block with live := false } }, footprint)

/-- Planted (law `ConcreteSpec.Core.Frame.runEffect_frame`, case `byte_store`,
subject `ConcreteSpec.Core.Frame.runEffect`, mutant `ConcreteSpec.Core.Frame.corruptEffect`). -/
example : metadataStoreBefore.memory.WellFormed linuxX8664 ∧
    ¬ EffectResult linuxX8664 metadataStoreBefore
      (.store metadataStorePtr [.init 7 none] 1)
      (corruptEffect linuxX8664 metadataStoreBefore (.store metadataStorePtr [.init 7 none] 1)) := by
  refine ⟨metadataStoreWellFormed, ?_⟩
  intro h
  have hm := h.2.2.1 ⟨0⟩ metadataStoreBlock (by decide) (by decide)
  simp [metadataStoreBefore, metadataStoreBlock,
    metadataStorePtr, blockMetadata, Memory.modify, Memory.block,
    AList.update, AList.lookup] at hm

/-- A planted executor refuses every command, even when the underlying C4 operation is defined. -/
def droppingEffect (_tgt : Target) (_before : State) (_command : EffectCommand) :
    Option (State × Footprint) := none

/-- Planted (law `ConcreteSpec.Core.Frame.runEffect_frame`, case `byte_store`,
subject `ConcreteSpec.Core.Frame.runEffect`, mutant `ConcreteSpec.Core.Frame.droppingEffect`). -/
example : metadataStoreBefore.memory.WellFormed linuxX8664 ∧
    ¬ EffectResult linuxX8664 metadataStoreBefore
      (.store metadataStorePtr [.init 7 none] 1)
      (droppingEffect linuxX8664 metadataStoreBefore (.store metadataStorePtr [.init 7 none] 1)) := by
  refine ⟨metadataStoreWellFormed, ?_⟩
  intro h
  exact h rfl

/-- Helper (congruence). Widen static permissions without changing an execution.
(approved: PENDING) -/
theorem preserves_covers {before after : State} {outer inner : Footprint}
    (h : inner.Preserves before after) (cover : outer.Covers inner) :
    outer.Preserves before after := by
  refine ⟨?_, ?_, ?_⟩
  · intro b i x hx ho
    exact h.1 b i x hx (cover.1 b i ho)
  · intro b x hx ho
    exact h.2.1 b x hx (cover.2.1 b ho)
  · intro p hp
    exact h.2.2 p (cover.2.2 p hp)

/-- Helper (congruence). Compose two executions with the same advertised permission set.
(approved: PENDING) -/
theorem preserves_trans {before middle after : State} {f : Footprint}
    (h₁ : f.Preserves before middle) (h₂ : f.Preserves middle after) :
    f.Preserves before after := by
  apply preserves_covers (preserves_seq h₁ h₂)
  refine ⟨?_, ?_, ?_⟩
  · intro b i h r hr
    simp only [Footprint.union, List.mem_append] at hr
    exact hr.elim (h r) (h r)
  · intro b h
    simpa [Footprint.BlockOutside, Footprint.union] using h
  · intro p h
    simpa [Footprint.PlaceOutside, Footprint.union] using h

/-- Helper (inversion). Structural induction over `C4Effects` proves that every derived effect
step or sequence preserves state outside its footprint. (approved: PENDING) -/
theorem c4Execution_frame (tgt : Target) (before : State) (f : Footprint) (after : State)
    (effects : c4Execution tgt before f after) :
    f.Preserves before after := by
  induction effects with
  | idle st => exact preserves_refl st
  | set st p cell => exact set_frame st p cell
  | store st m' p bs align access hw ha hs =>
    exact store_frame tgt st hw m' p bs align access ha hs
  | free st b block m' hb hf => exact free_frame st b block m' hb hf
  | expose st m' p b block hp hb he => exact expose_frame st m' p b block hp hb he
  | alloc st space base size align b m' hw ha =>
    exact alloc_frame tgt st hw space base size align b m' ha
  | writeStructured st st' p c old v hc hs hw =>
    exact write_structured_frame tgt st st' p c old v hc hs hw
  | writeMaterialized st st' p c r v access hw hc hs ha hwrite =>
    exact write_materialized_frame tgt st st' hw p c r v hc hs access ha hwrite
  | rawWrite st st' p bs align access hw ha hraw =>
    exact rawWrite_frame tgt st st' hw p bs align access ha hraw
  | seq _ _ ih₁ ih₂ => exact preserves_seq ih₁ ih₂

/-- Helper (inversion). Every C4 effect derivation preserves `Memory.WellFormed`.
(approved: PENDING) -/
theorem c4Execution_wellFormed (tgt : Target) (before : State) (f : Footprint) (after : State)
    (effects : c4Execution tgt before f after) (hw : before.memory.WellFormed tgt) :
    after.memory.WellFormed tgt := by
  induction effects with
  | idle _ => exact hw
  | set _ _ _ => exact hw
  | store st m' p bs align _ _ _ hs =>
    exact store_wellFormed tgt st.memory m' hw p bs align hs
  | free st b _ m' _ hf =>
    exact free_wellFormed tgt st.memory m' hw b hf
  | expose st m' p _ _ _ _ he =>
    exact expose_wellFormed tgt st.memory m' hw p he
  | alloc st space base size align b m' _ ha =>
    exact alloc_wellFormed tgt st.memory hw space base size align b m' ha
  | writeStructured st st' p c old v hc hs hwrite =>
    unfold State.write at hwrite
    simp only [hc] at hwrite
    dsimp at hwrite
    rw [hs] at hwrite
    split at hwrite
    · simp only [Option.some.injEq] at hwrite
      subst st'
      exact hw
    · simp at hwrite
  | writeMaterialized st st' p c r v _ _ hc hs _ hwrite =>
    unfold State.write at hwrite
    simp only [hc] at hwrite
    dsimp at hwrite
    rw [hs] at hwrite
    split at hwrite
    · cases hm : st.memory.store tgt r.ptr (encode tgt v) r.align with
      | none => simp [hm] at hwrite
      | some m' =>
        simp only [hm, Option.bind_some, Option.some.injEq] at hwrite
        subst st'
        exact store_wellFormed tgt st.memory m' hw r.ptr (encode tgt v) r.align hm
    · simp at hwrite
  | rawWrite st st' p bs align _ _ _ hraw =>
    unfold State.rawWrite at hraw
    cases hm : st.memory.store tgt p bs align with
    | none => simp [hm] at hraw
    | some m' =>
      simp [hm] at hraw
      subst st'
      exact store_wellFormed tgt st.memory m' hw p bs align hm
  | seq _ _ ih₁ ih₂ => exact ih₂ (ih₁ hw)

/-- Law (subject `ConcreteSpec.Core.Frame.Footprint.union`).
Two consecutive C4 effect derivations preserve every byte, block metadata entry and structured
place outside the union of their footprints. -/
theorem c4_effects_frame (tgt : Target) (before middle after : State) (f g : Footprint)
    (first : c4Execution tgt before f middle) (second : c4Execution tgt middle g after) :
    (f.union g).Preserves before after :=
  preserves_seq (c4Execution_frame tgt before f middle first)
    (c4Execution_frame tgt middle g after second)

private def metadataStoreAndSetAfter : State :=
  metadataStoreAfter.set ⟨0⟩ ⟨.bool, .structured (.bool true)⟩

/-- Helper (computation). Constructs the closed C4 byte-store execution step for composition
and C1 call witnesses. (approved: PENDING) -/
private theorem metadataStoreExecution :
    c4Execution linuxX8664 metadataStoreBefore (.byte ⟨⟨0⟩, 0, 1⟩) metadataStoreAfter :=
  C4Effects.store metadataStoreBefore metadataStoreAfter.memory metadataStorePtr [.init 7 none]
    1 (some (some (⟨0⟩, 0))) metadataStoreWellFormed (by decide) rfl

/-- Helper (computation). Constructs the closed C4 place-update execution step for sequential
composition witnesses. (approved: PENDING) -/
private theorem metadataSetExecution :
    c4Execution linuxX8664 metadataStoreAfter (.place ⟨0⟩) metadataStoreAndSetAfter :=
  C4Effects.set metadataStoreAfter ⟨0⟩ ⟨.bool, .structured (.bool true)⟩

/-- Instance (law `ConcreteSpec.Core.Frame.c4_effects_frame`, case `store_then_set`). -/
example : ((Footprint.byte ⟨⟨0⟩, 0, 1⟩).union (.place ⟨0⟩)).Preserves
    metadataStoreBefore metadataStoreAndSetAfter :=
  c4_effects_frame linuxX8664 metadataStoreBefore metadataStoreAfter metadataStoreAndSetAfter
    (.byte ⟨⟨0⟩, 0, 1⟩) (.place ⟨0⟩) metadataStoreExecution metadataSetExecution

/-- A planted footprint combiner drops the second operation's footprint. -/
def leftOnlyUnion (f _g : Footprint) : Footprint := f

/-- Planted (law `ConcreteSpec.Core.Frame.c4_effects_frame`, case `store_then_set`,
subject `ConcreteSpec.Core.Frame.Footprint.union`,
mutant `ConcreteSpec.Core.Frame.leftOnlyUnion`). -/
example :
    c4Execution linuxX8664 metadataStoreBefore (.byte ⟨⟨0⟩, 0, 1⟩) metadataStoreAfter ∧
    c4Execution linuxX8664 metadataStoreAfter (.place ⟨0⟩) metadataStoreAndSetAfter ∧
    ¬ (leftOnlyUnion (.byte ⟨⟨0⟩, 0, 1⟩) (.place ⟨0⟩)).Preserves
      metadataStoreBefore metadataStoreAndSetAfter := by
  refine ⟨metadataStoreExecution, metadataSetExecution, ?_⟩
  intro h
  have hp := h.2.2 ⟨0⟩ (by decide)
  simp [metadataStoreAndSetAfter, metadataStoreAfter, metadataStoreBefore,
    State.set, State.places, AList.lookup] at hp

/-- Helper (congruence). A footprint covered by either side of a union is covered by the union.
(approved: PENDING) -/
theorem covers_of_or {body leave inner : Footprint}
    (h : body.Covers inner ∨ leave.Covers inner) :
    (body.union leave).Covers inner := by
  rcases h with hb | hl
  · exact ⟨fun b i ho => hb.1 b i (byteOutside_union ho).1,
      fun b ho => hb.2.1 b (blockOutside_union ho).1,
      fun p ho => hb.2.2 p (placeOutside_union ho).1⟩
  · exact ⟨fun b i ho => hl.1 b i (byteOutside_union ho).2,
      fun b ho => hl.2.1 b (blockOutside_union ho).2,
      fun p ho => hl.2.2 p (placeOutside_union ho).2⟩

/-- Helper (inversion). Induction over actual C1 steps threads memory well-formedness and
reachability from `entry`, composing each step's C4 effect derivation. (approved: PENDING) -/
theorem c1_steps_frame {O : Exec.Operands} {P : Exec.Program O} {project : O.Store → State}
    {tgt : Target} {bodyFootprint leaveFootprint : Footprint} {entry : Exec.Config O}
    (implementation : C1EffectImplementation O P project tgt bodyFootprint leaveFootprint entry)
    {middle exit : Exec.Config O} {prefixTrace trace : List O.Event}
    (reached : Exec.Steps P entry prefixTrace middle)
    (wellFormed : (project middle.store).memory.WellFormed tgt)
    (steps : Exec.Steps P middle trace exit) :
    (bodyFootprint.union leaveFootprint).Preserves (project middle.store) (project exit.store) ∧
      (project exit.store).memory.WellFormed tgt := by
  induction steps generalizing prefixTrace with
  | refl middle =>
    refine ⟨?_, wellFormed⟩
    apply preserves_covers (preserves_refl (project middle.store))
    refine ⟨?_, ?_, ?_⟩
    · intro b i _ r hr
      cases hr
    · intro b _ hr
      cases hr
    · intro p _ hr
      cases hr
  | @step before label next trace after step _ ih =>
    obtain ⟨effectFootprint, effects, cover⟩ :=
      implementation prefixTrace before label next reached wellFormed step
    have hFrameStep : (bodyFootprint.union leaveFootprint).Preserves
        (project before.store) (project next.store) :=
      preserves_covers (c4Execution_frame tgt _ _ _ effects) (covers_of_or cover)
    have hWfNext : (project next.store).memory.WellFormed tgt :=
      c4Execution_wellFormed tgt _ _ _ effects wellFormed
    have hReachedNext : Exec.Steps P entry (prefixTrace ++ label.events) next :=
      Exec.Steps.trans reached (Exec.Steps.single step)
    obtain ⟨hFrameRest, hWfAfter⟩ := ih hReachedNext hWfNext
    exact ⟨preserves_trans hFrameStep hFrameRest, hWfAfter⟩

/-- Helper (inversion). Lifts step soundness and well-formedness preservation through the
complete-call derivation, including its final return/pop step. (approved: PENDING) -/
theorem c1_call_frame_of_implementation {O : Exec.Operands} {f : FnId} {P : Exec.Program O}
    {project : O.Store → State} {tgt : Target} {bodyFootprint leaveFootprint : Footprint}
    {entry exit : Exec.Config O}
    (wellFormed : (project entry.store).memory.WellFormed tgt)
    (implementation : C1EffectImplementation O P project tgt bodyFootprint leaveFootprint entry)
    (call : C1Call O f P entry exit) :
    (bodyFootprint.union leaveFootprint).Preserves (project entry.store) (project exit.store) := by
  obtain ⟨fn, dst, caller, k, args, site, vs, st, stArgs, stEnter, stReturn, callee, tr, result,
    _hf, _he, _hv, _hlen, _hl, returned⟩ := call
  rcases returned with ⟨steps, pop⟩ | ⟨_unit, steps, pop⟩
  all_goals
    obtain ⟨framedPrefix, wfBoundary⟩ :=
      c1_steps_frame implementation (Exec.Steps.refl entry) wellFormed steps
    obtain ⟨effectFootprint, effects, cover⟩ :=
      implementation tr _ .silent exit steps wfBoundary (.silent pop)
    exact preserves_trans framedPrefix
      (preserves_covers (c4Execution_frame tgt _ _ _ effects) (covers_of_or cover))

/-- Law (subject `ConcreteSpec.Core.Frame.Footprint.union`).
Local decompositions of actual reachable C1 steps from a well-formed entry state into
independently executed C4 effects establish complete-call framing across the union of the
callee body and leave footprints. -/
theorem c1_call_frame (tgt : Target) (O : Exec.Operands) (f : FnId) (P : Exec.Program O)
    (project : O.Store → State) (bodyFootprint leaveFootprint : Footprint)
    (entry exit : Exec.Config O)
    (wellFormed : (project entry.store).memory.WellFormed tgt)
    (implementation : C1EffectImplementation O P project tgt bodyFootprint leaveFootprint entry)
    (call : C1Call O f P entry exit) :
    (bodyFootprint.union leaveFootprint).Preserves (project entry.store) (project exit.store) :=
  c1_call_frame_of_implementation wellFormed implementation call

private abbrev StoringOp := storingOperands linuxX8664 metadataStorePtr [.init 7 none] 1

private def storingProg : Exec.Program StoringOp :=
  storingProgram linuxX8664 metadataStorePtr [.init 7 none] 1

private def storingEntry : Exec.Config StoringOp :=
  ⟨.exec (.call none storingFn [] ⟨0⟩), [], metadataStoreBefore, []⟩

private def storingEntered : Exec.Config StoringOp :=
  ⟨.exec (.ret () ⟨0⟩), [], metadataStoreBefore, [.scope ⟨0⟩ [] [], .ret none []]⟩

private def storingReturning : Exec.Config StoringOp :=
  ⟨.exiting (.ret (7 : Nat)), [], metadataStoreAfter, [.scope ⟨0⟩ [] [], .ret none []]⟩

private def storingBoundary : Exec.Config StoringOp :=
  ⟨.exiting (.ret (7 : Nat)), [], metadataStoreAfter, [.ret none []]⟩

private def storingExit : Exec.Config StoringOp :=
  ⟨.done, [], metadataStoreAfter, []⟩

private def StoringReachable (c : Exec.Config StoringOp) : Prop :=
  c = storingEntry ∨ c = storingEntered ∨ c = storingReturning ∨
    c = storingBoundary ∨ c = storingExit

/-- Helper (inversion). One step of `storingProg` from a reachable configuration stays within
the 5-state trajectory and either leaves the store idle or performs the authorized C4 byte store.
(approved: PENDING) -/
private theorem storing_step_inv {before after : Exec.Config StoringOp}
    {label : Exec.Label StoringOp}
    (hr : StoringReachable before) (hstep : Exec.Step storingProg before label after) :
    StoringReachable after ∧
      (after.store = before.store ∨
        (before.store = metadataStoreBefore ∧ after.store = metadataStoreAfter)) := by
  cases hstep with
  | prim _ ha _ => cases ha
  | silent hs =>
    rcases hr with rfl | rfl | rfl | rfl | rfl
    · change some (Exec.Out.next storingEntered) = some (.next after) at hs
      cases hs
      exact ⟨.inr (.inl rfl), .inl rfl⟩
    · change some (Exec.Out.next storingReturning) = some (.next after) at hs
      cases hs
      exact ⟨.inr (.inr (.inl rfl)), .inr ⟨rfl, rfl⟩⟩
    · change some (Exec.Out.next storingBoundary) = some (.next after) at hs
      cases hs
      exact ⟨.inr (.inr (.inr (.inl rfl))), .inl rfl⟩
    · change some (Exec.Out.next storingExit) = some (.next after) at hs
      cases hs
      exact ⟨.inr (.inr (.inr (.inr rfl))), .inl rfl⟩
    · change (none : Option (Exec.Out StoringOp)) = some (.next after) at hs
      cases hs

/-- Helper (inversion). Multi-step reachability in `storingProg` preserves `StoringReachable`.
(approved: PENDING) -/
private theorem storing_steps_reachable {c before : Exec.Config StoringOp}
    {trace : List Unit} (hr : StoringReachable c)
    (hsteps : Exec.Steps storingProg c trace before) :
    StoringReachable before := by
  induction hsteps with
  | refl => exact hr
  | step hstep _ ih => exact ih (storing_step_inv hr hstep).1

/-- Helper (computation). Constructs the closed `C1EffectImplementation` witness for
`storingProg` from `storingEntry`. (approved: PENDING) -/
private theorem storingImplementation :
    C1EffectImplementation StoringOp storingProg id linuxX8664
      (.byte ⟨⟨0⟩, 0, 1⟩) .empty storingEntry := by
  intro trace before label after hsteps _hw hstep
  have hr : StoringReachable before := storing_steps_reachable (.inl rfl) hsteps
  obtain ⟨_, hstore⟩ := storing_step_inv hr hstep
  rcases hstore with heq | ⟨hbef, haft⟩
  · refine ⟨.empty, ?_, .inr ⟨fun _ _ h => h, fun _ h => h, fun _ h => h⟩⟩
    dsimp [id]
    rw [heq]
    exact C4Effects.idle before.store
  · refine ⟨.byte ⟨⟨0⟩, 0, 1⟩, ?_, .inl ⟨fun _ _ h => h, fun _ h => h, fun _ h => h⟩⟩
    dsimp [id]
    rw [hbef, haft]
    exact metadataStoreExecution

/-- Helper (computation). Constructs the closed `C1Call` witness for `storingProg`.
(approved: PENDING) -/
private theorem storingExplicitCall :
    C1Call StoringOp storingFn storingProg storingEntry storingExit := by
  have hs : Exec.Steps storingProg storingEntry [] storingBoundary := by
    apply Exec.steps_next rfl
    apply Exec.steps_next rfl
    apply Exec.steps_next rfl
    exact Exec.Steps.refl _
  exact ⟨⟨[], (), ⟨0⟩, .ret () ⟨0⟩⟩, none, [], [], [], ⟨0⟩, [],
    metadataStoreBefore, metadataStoreBefore, metadataStoreBefore, metadataStoreAfter,
    [], [], (7 : Nat), rfl, rfl, rfl, rfl, rfl, .inl ⟨hs, rfl⟩⟩

/-- Instance (law `ConcreteSpec.Core.Frame.c1_call_frame`, case `storing_callee`). -/
example : ((Footprint.byte ⟨⟨0⟩, 0, 1⟩).union .empty).Preserves
    (id storingEntry.store) (id storingExit.store) :=
  c1_call_frame linuxX8664 StoringOp storingFn storingProg id
    (.byte ⟨⟨0⟩, 0, 1⟩) .empty storingEntry storingExit
    metadataStoreWellFormed storingImplementation storingExplicitCall

/-- A planted call-footprint combiner ignores the callee body's footprint and accounts only for
the final return step. -/
def droppingBodyUnion (_body leave : Footprint) : Footprint := leave

/-- Planted (law `ConcreteSpec.Core.Frame.c1_call_frame`, case `storing_callee`,
subject `ConcreteSpec.Core.Frame.Footprint.union`,
mutant `ConcreteSpec.Core.Frame.droppingBodyUnion`). -/
example :
    (id storingEntry.store).memory.WellFormed linuxX8664 ∧
    C1EffectImplementation StoringOp storingProg id linuxX8664
      (.byte ⟨⟨0⟩, 0, 1⟩) .empty storingEntry ∧
    C1Call StoringOp storingFn storingProg storingEntry storingExit ∧
    ¬ (droppingBodyUnion (.byte ⟨⟨0⟩, 0, 1⟩) .empty).Preserves
      (id storingEntry.store) (id storingExit.store) := by
  refine ⟨metadataStoreWellFormed, storingImplementation, storingExplicitCall, ?_⟩
  intro h
  have byte := h.1 ⟨0⟩ 0 metadataStoreBlock (by decide) (by decide)
  have different : (metadataStoreAfter.memory.block ⟨0⟩).map (fun b => b.bytes[0]?) ≠
      some (metadataStoreBlock.bytes[0]?) := by decide
  exact different byte



namespace ABI

/-- Law (subject `ConcreteSpec.Core.Frame.ABI.layoutBytes`).
Rounding up raw slot bytes to a positive alignment never shrinks the frame and produces a multiple
of the alignment. -/
theorem layout_aligned (plan : LayoutPlan) (positive : 0 < plan.alignment) :
    rawSlotBytes plan ≤ layoutBytes plan ∧ layoutBytes plan % plan.alignment = 0 := by
  refine ⟨?_, Nat.mul_mod_left _ _⟩
  dsimp [layoutBytes]
  have hdiv := Nat.div_add_mod (rawSlotBytes plan + plan.alignment - 1) plan.alignment
  have hmod := Nat.mod_lt (rawSlotBytes plan + plan.alignment - 1) positive
  rw [Nat.mul_comm]
  omega

/-- Instance (law `ConcreteSpec.Core.Frame.ABI.layout_aligned`, case `two_slots`). -/
example : rawSlotBytes ⟨8, [3, 6]⟩ ≤ layoutBytes ⟨8, [3, 6]⟩ ∧
    layoutBytes ⟨8, [3, 6]⟩ % 8 = 0 :=
  layout_aligned ⟨8, [3, 6]⟩ (by decide)

/-- A planted layout skips alignment rounding and returns the unrounded byte sum. -/
def wrongLayoutBytes (plan : LayoutPlan) : Nat := rawSlotBytes plan

/-- Planted (law `ConcreteSpec.Core.Frame.ABI.layout_aligned`, case `two_slots`,
subject `ConcreteSpec.Core.Frame.ABI.layoutBytes`, mutant `ConcreteSpec.Core.Frame.ABI.wrongLayoutBytes`). -/
example : 0 < (8 : Nat) ∧
    ¬ (rawSlotBytes ⟨8, [3, 6]⟩ ≤ wrongLayoutBytes ⟨8, [3, 6]⟩ ∧
      wrongLayoutBytes ⟨8, [3, 6]⟩ % 8 = 0) := by
  decide

/-- A planted layout rounds down instead of up, shrinking the frame below its raw slot bytes. -/
def truncatingLayoutBytes (plan : LayoutPlan) : Nat :=
  (rawSlotBytes plan / plan.alignment) * plan.alignment

/-- Planted (law `ConcreteSpec.Core.Frame.ABI.layout_aligned`, case `two_slots`,
subject `ConcreteSpec.Core.Frame.ABI.layoutBytes`,
mutant `ConcreteSpec.Core.Frame.ABI.truncatingLayoutBytes`). -/
example : 0 < (8 : Nat) ∧
    ¬ (rawSlotBytes ⟨8, [3, 6]⟩ ≤ truncatingLayoutBytes ⟨8, [3, 6]⟩ ∧
      truncatingLayoutBytes ⟨8, [3, 6]⟩ % 8 = 0) := by
  decide

/-- Helper (computation). Structural induction showing operational stack samples conform to the
static stack bound (`ArtifactStackConforms`). (approved: PENDING) -/
theorem stackCode_conforms (code : StackCode) : StackSafe code := by
  induction code with
  | done =>
    intro base sample hs
    simp only [stackSamples, List.mem_cons, List.not_mem_nil, or_false] at hs
    subst sample
    simp [computedStackBound]
  | seq first second ih₁ ih₂ =>
    intro base sample hs
    simp only [stackSamples, List.mem_append] at hs
    rcases hs with hs | hs
    · have h := ih₁ base sample hs
      have hm := Nat.le_max_left (computedStackBound first) (computedStackBound second)
      change sample ≤ base + max (computedStackBound first) (computedStackBound second)
      omega
    · have h := ih₂ base sample hs
      have hm := Nat.le_max_right (computedStackBound first) (computedStackBound second)
      change sample ≤ base + max (computedStackBound first) (computedStackBound second)
      omega
  | frame bytes body ih =>
    intro base sample hs
    simp only [stackSamples, List.mem_cons, List.mem_append, List.not_mem_nil, or_false] at hs
    rcases hs with rfl | hs | rfl
    · simp [computedStackBound]
    · have h := ih (base + bytes) sample hs
      simpa [computedStackBound, Nat.add_assoc] using h
    · simp [computedStackBound]

/-- Law (subject `ConcreteSpec.Core.Frame.ABI.computedStackBound`).
The static bound covers every sample of an independently described nested stack execution. -/
theorem stackSamples_bounded (code : StackCode) (base sample : Nat)
    (inSamples : sample ∈ stackSamples code base) :
    sample ≤ base + computedStackBound code :=
  stackCode_conforms code base sample inSamples

/-- Instance (law `ConcreteSpec.Core.Frame.ABI.stackSamples_bounded`, case `nested_call`). -/
example : 48 ≤ 0 + computedStackBound (.frame 16 (.seq (.frame 32 .done) (.frame 8 .done))) :=
  stackSamples_bounded (.frame 16 (.seq (.frame 32 .done) (.frame 8 .done))) 0 48 (by decide)

/-- A planted stack-bound calculator counts only the outer frame and ignores nested calls. -/
def unnestedStackBound : StackCode → Nat
  | .frame bytes _ => bytes
  | _ => 0

/-- Planted (law `ConcreteSpec.Core.Frame.ABI.stackSamples_bounded`, case `nested_call`,
subject `ConcreteSpec.Core.Frame.ABI.computedStackBound`,
mutant `ConcreteSpec.Core.Frame.ABI.unnestedStackBound`). -/
example :
    48 ∈ stackSamples (.frame 16 (.seq (.frame 32 .done) (.frame 8 .done))) 0 ∧
    ¬ (48 ≤ 0 + unnestedStackBound (.frame 16 (.seq (.frame 32 .done) (.frame 8 .done)))) := by
  decide

/-- Helper (computation). Establishes that the toy ABI signature satisfies alignment, frame-bound,
and register-disjointness invariants. (approved: PENDING) -/
theorem toySignature_wellFormed : toySignature.WellFormed := by
  refine ⟨⟨by decide, by decide⟩, by decide, by decide, ?_⟩
  intro r hr
  simp only [toySignature, List.mem_cons, List.not_mem_nil, or_false] at hr ⊢
  omega

/-- Helper (computation). Running `toyBodyAt` preserves callee-saved register 0, caller stack slots,
and the stack pointer while modifying at most `resultReg`. (approved: PENDING) -/
theorem toyBodyAt_preserved (before : Machine Nat Nat) (resultReg result : Nat)
    (_hnc : 0 ≠ resultReg) :
    Machine.Preserved before (runToy (toyBodyAt resultReg result) before) [0] ∧
      Machine.RegisterFrame before (runToy (toyBodyAt resultReg result) before) [resultReg] := by
  refine ⟨⟨?_, ?_, ?_⟩, ?_⟩
  · intro r hr
    simp only [List.mem_cons, List.not_mem_nil, or_false] at hr
    subst r
    simp [toyBodyAt, runToy]
  · intro slot hs
    have hn1 : slot ≠ before.stackPointer := by omega
    have hn2 : slot ≠ before.stackPointer + 1 := by omega
    simp [toyBodyAt, runToy, hn1, hn2]
  · simp [toyBodyAt, runToy]
  · intro r hr
    have hnr : r ≠ resultReg := by
      simpa only [List.mem_cons, List.not_mem_nil, or_false] using hr
    by_cases hr0 : r = 0
    · subst r
      simp [toyBodyAt, runToy]
    · simp [toyBodyAt, runToy, hr0, hnr]

/-- Helper (inversion). Establishes the relation form of toy-call conformance from its
independent execution witnesses. (approved: PENDING) -/
theorem toyTransition_conforms : TransitionConforms toySignature toyTransition := by
  intro before after args result h
  obtain ⟨base, arg, rfl, rfl, rfl⟩ := h
  have hp := toyBodyAt_preserved (toyInput base arg) 1 result (by decide)
  refine ⟨?_, ?_, hp.1, hp.2⟩
  · simp [toySignature, ArgumentsHold, Binding.Holds, Location.read, toyInput, runToy]
  · simp [toySignature, Binding.Holds, Location.read, toyBody, toyBodyAt, toyInput, runToy]

/-- A populated base machine, including a caller-owned stack slot. -/
def toyBase : Machine Nat Nat := ⟨fun r => 10 + r, fun slot => 20 + slot, 1⟩

/-- Law (subject `ConcreteSpec.Core.Frame.ABI.toyCall`).
Executing input packing and the independent toy body establishes a well-formed signature and the
complete ABI contract, including in-frame spilling and restoration of callee-saved register 0. -/
theorem toyCall_conforms (base : Machine Nat Nat) (arg result : Nat) :
    toySignature.WellFormed ∧
      toySignature.Call (toyInput base arg) (toyCall base arg result) [arg] result :=
  ⟨toySignature_wellFormed, toyTransition_conforms _ _ _ _ ⟨base, arg, rfl, rfl, rfl⟩⟩

/-- Instance (law `ConcreteSpec.Core.Frame.ABI.toyCall_conforms`, case `spill`). -/
example : toySignature.WellFormed ∧
    toySignature.Call (toyInput toyBase 11) (toyCall toyBase 11 7) [11] 7 :=
  toyCall_conforms toyBase 11 7

/-- A planted call clobbers a callee-saved register without restoring it. -/
def wrongToySaved (base : Machine Nat Nat) (arg result : Nat) : Machine Nat Nat :=
  runToy [.register 0 99] (toyCall base arg result)
/-- A planted call overwrites a caller-owned stack slot below `base.stackPointer`. -/
def wrongToySlot (base : Machine Nat Nat) (arg result : Nat) : Machine Nat Nat :=
  runToy [.slot 1 99] (toyCall base arg result)
/-- A planted call leaks its frame. -/
def wrongToyPointer (base : Machine Nat Nat) (arg result : Nat) : Machine Nat Nat :=
  runToy [.reserve 8] (toyCall base arg result)
/-- A planted call overwrites the returned value. -/
def wrongToyResult (base : Machine Nat Nat) (arg result : Nat) : Machine Nat Nat :=
  runToy [.register 1 99] (toyCall base arg result)

/-- Planted (law `ConcreteSpec.Core.Frame.ABI.toyCall_conforms`, case `spill`,
subject `ConcreteSpec.Core.Frame.ABI.toyCall`, mutant `ConcreteSpec.Core.Frame.ABI.wrongToySaved`). -/
example : ¬ (toySignature.WellFormed ∧
    toySignature.Call (toyInput toyBase 11) (wrongToySaved toyBase 11 7) [11] 7) := by
  intro ⟨_, h⟩
  have bad := h.preserved.1 0 (by simp [toySignature])
  simp [wrongToySaved, toyCall, toyInput, toyBody, toyBodyAt, toyBase, runToy] at bad

/-- Planted (law `ConcreteSpec.Core.Frame.ABI.toyCall_conforms`, case `spill`,
subject `ConcreteSpec.Core.Frame.ABI.toyCall`, mutant `ConcreteSpec.Core.Frame.ABI.wrongToySlot`). -/
example : ¬ (toySignature.WellFormed ∧
    toySignature.Call (toyInput toyBase 11) (wrongToySlot toyBase 11 7) [11] 7) := by
  intro ⟨_, h⟩
  have bad := h.preserved.2.1 0 (by decide)
  simp [wrongToySlot, toyCall, toyInput, toyBody, toyBodyAt, toyBase, runToy] at bad

/-- Planted (law `ConcreteSpec.Core.Frame.ABI.toyCall_conforms`, case `spill`,
subject `ConcreteSpec.Core.Frame.ABI.toyCall`, mutant `ConcreteSpec.Core.Frame.ABI.wrongToyPointer`). -/
example : ¬ (toySignature.WellFormed ∧
    toySignature.Call (toyInput toyBase 11) (wrongToyPointer toyBase 11 7) [11] 7) := by
  intro ⟨_, h⟩
  have bad := h.preserved.2.2
  simp [wrongToyPointer, toyCall, toyInput, toyBody, toyBodyAt, toyBase, runToy] at bad

/-- Planted (law `ConcreteSpec.Core.Frame.ABI.toyCall_conforms`, case `spill`,
subject `ConcreteSpec.Core.Frame.ABI.toyCall`, mutant `ConcreteSpec.Core.Frame.ABI.wrongToyResult`). -/
example : ¬ (toySignature.WellFormed ∧
    toySignature.Call (toyInput toyBase 11) (wrongToyResult toyBase 11 7) [11] 7) := by
  intro ⟨_, h⟩
  have bad := h.result
  simp [toySignature, Binding.Holds, Location.read, wrongToyResult, toyCall,
    toyInput, toyBody, toyBodyAt, toyBase, runToy] at bad

/-- Helper (congruence). Transitivity of protected machine observations across calls. (approved: PENDING)
 Preserved machine state composes across sequential calls with the same saved-register set. -/
theorem machine_preserved_trans {Reg Word : Type} {before middle after : Machine Reg Word}
    {saved : List Reg} (h₁ : Machine.Preserved before middle saved)
    (h₂ : Machine.Preserved middle after saved) : Machine.Preserved before after saved := by
  refine ⟨?_, ?_, ?_⟩
  · intro r hr
    exact (h₂.1 r hr).trans (h₁.1 r hr)
  · intro slot hs
    have hs' : slot < middle.stackPointer := by rwa [h₁.2.2]
    exact (h₂.2.1 slot hs').trans (h₁.2.1 slot hs)
  · exact h₂.2.2.trans h₁.2.2

/-- Helper (congruence). Transitivity of register observations outside combined clobber sets. (approved: PENDING)
 Sequential calls preserve every register outside the union of their clobber sets. -/
theorem machine_registerFrame_union {Reg Word : Type} {before middle after : Machine Reg Word}
    {clobbers₁ clobbers₂ : List Reg} (h₁ : Machine.RegisterFrame before middle clobbers₁)
    (h₂ : Machine.RegisterFrame middle after clobbers₂) :
    Machine.RegisterFrame before after (clobbers₁ ++ clobbers₂) := by
  intro r hr
  have hr' : r ∉ clobbers₁ ∧ r ∉ clobbers₂ := by
    simpa only [List.mem_append, not_or] using hr
  exact (h₂ r hr'.2).trans (h₁ r hr'.1)

/-- Law (subject `ConcreteSpec.Core.Frame.ABI.toyCallSeq`).
Two sequential toy calls preserve the caller's saved register, stack slots, and stack pointer
across both calls while confining register modifications to the union of their clobber sets. -/
theorem toyCallSeq_conforms (base : Machine Nat Nat) (arg result₁ result₂ : Nat) :
    Machine.Preserved (toyInput base arg) (toyCallSeq base arg result₁ result₂) [0] ∧
      Machine.RegisterFrame (toyInput base arg) (toyCallSeq base arg result₁ result₂) ([1] ++ [2]) := by
  have h₁ := (toyCall_conforms base arg result₁).2
  have h₂ := toyBodyAt_preserved (toyCall base arg result₁) 2 result₂ (by decide)
  exact ⟨machine_preserved_trans h₁.preserved h₂.1, machine_registerFrame_union h₁.clobbers h₂.2⟩

/-- Instance (law `ConcreteSpec.Core.Frame.ABI.toyCallSeq_conforms`, case `two_calls`). -/
example :
    Machine.Preserved (toyInput toyBase 11) (toyCallSeq toyBase 11 7 9) [0] ∧
      Machine.RegisterFrame (toyInput toyBase 11) (toyCallSeq toyBase 11 7 9) ([1] ++ [2]) :=
  toyCallSeq_conforms toyBase 11 7 9

/-- A planted sequential call fails to restore callee-saved register 0 on the second call. -/
def clobberingSavedToyCallSeq (base : Machine Nat Nat) (arg result₁ result₂ : Nat) : Machine Nat Nat :=
  runToy [.register 0 88] (toyCallSeq base arg result₁ result₂)

/-- Planted (law `ConcreteSpec.Core.Frame.ABI.toyCallSeq_conforms`, case `two_calls`,
subject `ConcreteSpec.Core.Frame.ABI.toyCallSeq`,
mutant `ConcreteSpec.Core.Frame.ABI.clobberingSavedToyCallSeq`). -/
example : ¬ (Machine.Preserved (toyInput toyBase 11) (clobberingSavedToyCallSeq toyBase 11 7 9) [0] ∧
    Machine.RegisterFrame (toyInput toyBase 11) (clobberingSavedToyCallSeq toyBase 11 7 9) ([1] ++ [2])) := by
  intro ⟨hp, _⟩
  have bad := hp.1 0 (by decide)
  simp [clobberingSavedToyCallSeq, toyCallSeq, toyCall, toyInput, toyBody, toyBodyAt, toyBase, runToy] at bad

/-- A planted sequential call clobbers register 3 outside the combined clobber set `[1] ++ [2]`. -/
def undeclaredClobberToyCallSeq (base : Machine Nat Nat) (arg result₁ result₂ : Nat) : Machine Nat Nat :=
  runToy [.register 3 77] (toyCallSeq base arg result₁ result₂)

/-- Planted (law `ConcreteSpec.Core.Frame.ABI.toyCallSeq_conforms`, case `two_calls`,
subject `ConcreteSpec.Core.Frame.ABI.toyCallSeq`,
mutant `ConcreteSpec.Core.Frame.ABI.undeclaredClobberToyCallSeq`). -/
example : ¬ (Machine.Preserved (toyInput toyBase 11) (undeclaredClobberToyCallSeq toyBase 11 7 9) [0] ∧
    Machine.RegisterFrame (toyInput toyBase 11) (undeclaredClobberToyCallSeq toyBase 11 7 9) ([1] ++ [2])) := by
  intro ⟨_, hf⟩
  have bad := hf 3 (by decide)
  simp [undeclaredClobberToyCallSeq, toyCallSeq, toyCall, toyInput, toyBody, toyBodyAt, toyBase, runToy] at bad

/-- Helper (computation). Characterizes the retention specification; execution evidence is
`ConcreteSpec.Core.Frame.ABI.transferKept_sound`. (approved: PENDING)
 An extern unrelated to both sides of a pair cannot validly return either retention value. -/
theorem extern_result_rejects_unrelated_retention {F H : Type} (pair : KeptPair F H) (f : F) (h : H)
    (hfs : f ≠ pair.submit) (hfc : f ≠ pair.complete) :
    ¬ ExternResult.Valid pair f (.receipt h) ∧ ¬ ExternResult.Valid pair f (.owned h) := by
  constructor
  · intro hr
    exact hfs hr.1
  · intro ho
    exact hfc ho.1

/-- Helper (computation). Characterizes the retention specification; execution evidence is
`ConcreteSpec.Core.Frame.ABI.transferKept_sound`. (approved: PENDING)
 A well-formed kept pair validly returns its submission receipt and completion-owned handle.
The exact result constraints already imply both `ExternResult.Valid` facts. -/
theorem keptPair_results_valid {F H : Type} (pair : KeptPair F H)
    (submitParam : ExternParam H) (submitResult completeParam completeResult : ExternType H)
    (h : pair.WellFormed submitParam submitResult completeParam completeResult) :
    ExternResult.Valid pair pair.submit submitResult ∧
      ExternResult.Valid pair pair.complete completeResult := by
  obtain ⟨hne, hsp, hsr, hcp, hcr⟩ := h
  subst submitResult
  subst completeResult
  exact ⟨⟨rfl, rfl⟩, ⟨rfl, rfl⟩⟩

/-- Accepted: the correctly paired declarations are a non-trivial instance of endpoint validity. -/
example : ExternResult.Valid (⟨0, 1, 7⟩ : KeptPair Nat Nat) 0 (.receipt 7) ∧
    ExternResult.Valid ⟨0, 1, 7⟩ 1 (.owned 7) := by
  exact keptPair_results_valid ⟨0, 1, 7⟩ ⟨.owned 7, .kept⟩
    (.receipt 7) (.receipt 7) (.owned 7) ⟨by decide, rfl, rfl, rfl, rfl⟩

/-- Helper (computation). Characterizes the retention specification; execution evidence is
`ConcreteSpec.Core.Frame.ABI.transferKept_sound`. (approved: PENDING)
 A kept receipt has exactly one retention transition: the paired completion returns the same
handle to Concrete ownership. -/
theorem kept_only_paired_completion {F H : Type} (pair : KeptPair F H) (h : H)
    (event : ForeignEvent F H) (after : RetentionState H)
    (hs : KeptStep pair (.kept h) event after) :
    event = .call pair.complete h ∧ after = .owned pair.handle := by
  cases event with
  | call f h' =>
    cases after with
    | owned h'' =>
      obtain ⟨hf, _hne, hh, hh', hh''⟩ := hs
      cases hf
      cases hh
      cases hh'
      cases hh''
      exact ⟨rfl, rfl⟩
    | kept h'' => cases hs

/-- Helper (computation). Characterizes the retention specification; execution evidence is
`ConcreteSpec.Core.Frame.ABI.transferKept_sound`. (approved: PENDING)
 Submission of an owned handle calls the paired submitting extern and produces a receipt for the
same handle. -/
theorem submit_produces_paired_receipt {F H : Type} (pair : KeptPair F H) (h : H)
    (event : ForeignEvent F H) (after : RetentionState H)
    (hs : KeptStep pair (.owned h) event after) :
    event = .call pair.submit h ∧ after = .kept pair.handle := by
  cases event with
  | call f h' =>
    cases after with
    | owned h'' => cases hs
    | kept h'' =>
      obtain ⟨hf, hh, hh', hh''⟩ := hs
      cases hf
      cases hh
      cases hh'
      cases hh''
      exact ⟨rfl, rfl⟩

/-- Helper (computation). Characterizes the retention specification; execution evidence is
`ConcreteSpec.Core.Frame.ABI.transferKept_sound`. (approved: PENDING)
 The submitting extern cannot consume its own receipt, even though it is the other member of the
pair: completion requires a distinct extern identity. -/
theorem kept_receipt_rejects_submission {F H : Type} (pair : KeptPair F H) (h h' : H)
    (after : RetentionState H) : ¬ KeptStep pair (.kept h) (.call pair.submit h') after := by
  intro hs
  cases after with
  | owned h'' => exact hs.2.1 hs.1
  | kept h'' => cases hs

/-- Helper (computation). Characterizes the retention specification; execution evidence is
`ConcreteSpec.Core.Frame.ABI.transferKept_sound`. (approved: PENDING)
 No other extern can consume a kept receipt. -/
theorem kept_receipt_rejects_other_extern {F H : Type} (pair : KeptPair F H) (h h' : H)
    (f : F) (after : RetentionState H) (hf : f ≠ pair.complete) :
    ¬ KeptStep pair (.kept h) (.call f h') after := by
  intro hs
  have he := (kept_only_paired_completion pair h (.call f h') after hs).1
  cases he
  exact hf rfl

/-- Helper (computation). Computes the refusal branch of the launcher admission specification. (approved: PENDING)
 A reservation below the final artifact bound refuses startup with status 125. -/
theorem stack_under_reservation {Input : Type} (bound reserved : Nat) (input : Input)
    (h : reserved < bound) : stackAdmission bound reserved input = .refused125 := by
  simp [stackAdmission, h]

/-- Helper (computation). Computes the entry branch of the launcher admission specification. (approved: PENDING)
 A reservation at least the bound admits the same launcher input. -/
theorem stack_adequate {Input : Type} (bound reserved : Nat) (input : Input)
    (h : bound ≤ reserved) : stackAdmission bound reserved input = .entered input := by
  simp [stackAdmission, Nat.not_lt_of_ge h]

/-- Helper (projection). Describes the contract or its arithmetic; independent execution
evidence must be supplied separately. (approved: PENDING)
 A launcher run relates its exact retained closure to the stated entry and exit observations. -/
theorem launcher_entry_exit {Artifact Input Result Observation : Type}
    {l : Launcher Artifact Input Result Observation} {artifact : Artifact} {reserved : Nat}
    {input : Input} {result : Result} {observation : Observation}
    (run : l.Run artifact reserved input result observation) :
    l.retainedClosure artifact ∧ l.entry artifact input ∧ l.exit artifact result observation :=
  ⟨run.closure, run.entry, run.exit⟩

/-- Helper (projection). Describes the contract or its arithmetic; independent execution
evidence must be supplied separately. (approved: PENDING)
 Every launched run passes stack admission with its recorded reservation. -/
theorem launcher_run_admitted {Artifact Input Result Observation : Type}
    {l : Launcher Artifact Input Result Observation} {artifact : Artifact} {reserved : Nat}
    {input : Input} {result : Result} {observation : Observation}
    (run : l.Run artifact reserved input result observation) :
    stackAdmission (l.stackBound artifact) reserved input = .entered input :=
  stack_adequate _ _ _ run.adequateStack

/-- Helper (inversion). An accepted declaration-check result meets the descriptor contract.
(approved: PENDING)
Declaration checking extracts a protocol whose four descriptors satisfy the specification. -/
theorem checkKeptPair_result {F H : Type} [DecidableEq F] [DecidableEq H]
    (submit complete : F) (sp : ExternParam H) (sr cp cr : ExternType H) :
    ∀ pair, checkKeptPair submit complete sp sr cp cr = some pair →
      pair.WellFormed sp sr cp cr := by
  intro pair hp
  cases sp with
  | mk t retention =>
    cases t <;> cases retention <;> cases sr <;> cases cp <;> cases cr <;>
      simp only [checkKeptPair] at hp <;> try contradiction
    split at hp <;> try contradiction
    next hne =>
      split at hp <;> try contradiction
      next hh =>
        cases hp
        obtain ⟨ha, hb, hc⟩ := hh
        subst_vars
        exact ⟨hne, rfl, rfl, rfl, rfl⟩

/-- Law (subject `ConcreteSpec.Core.Frame.ABI.checkKeptPair`).
Declaration checking produces a protocol satisfying its independent descriptor contract. -/
theorem checkKeptPair_sound {F H : Type} [DecidableEq F] [DecidableEq H]
    (submit complete : F) (sp : ExternParam H) (sr cp cr : ExternType H) :
    CheckedPairCorrect sp sr cp cr (checkKeptPair submit complete sp sr cp cr) := by
  cases checked : checkKeptPair submit complete sp sr cp cr with
  | none => trivial
  | some pair => exact checkKeptPair_result submit complete sp sr cp cr pair checked

/-- Law (subject `ConcreteSpec.Core.Frame.ABI.transferKept`).
Executing a transfer of the tracked token follows the paired retention protocol. -/
theorem transferKept_sound {F H : Type} (pair : KeptPair F H)
    (instruction : RetentionInstruction) (before : RetentionState H)
    (distinct : pair.submit ≠ pair.complete)
    (tracked : before = .owned pair.handle ∨ before = .kept pair.handle) :
    TransferCorrect pair before (transferKept pair instruction before) := by
  rcases tracked with rfl | rfl <;> cases instruction
  · exact ⟨rfl, rfl, rfl, rfl⟩
  · trivial
  · trivial
  · exact ⟨rfl, distinct, rfl, rfl, rfl⟩

/-- Instance (law `ConcreteSpec.Core.Frame.ABI.checkKeptPair_sound`, case `paired`). -/
example : CheckedPairCorrect ⟨.owned (7 : Nat), .kept⟩ (.receipt 7) (.receipt 7) (.owned 7)
    (checkKeptPair (0 : Nat) 1 ⟨.owned 7, .kept⟩ (.receipt 7) (.receipt 7) (.owned 7)) :=
  @checkKeptPair_sound Nat Nat inferInstance inferInstance 0 1
    ⟨.owned 7, .kept⟩ (.receipt 7) (.receipt 7) (.owned 7)

/-- A planted declaration extractor aliases the two endpoint identities. -/
def wrongKeptChecker {F H : Type} [DecidableEq F] [DecidableEq H]
    (submit complete : F) (sp : ExternParam H) (sr cp cr : ExternType H) :
    Option (KeptPair F H) :=
  (checkKeptPair submit complete sp sr cp cr).map fun pair =>
    { pair with complete := pair.submit }

/-- Planted (law `ConcreteSpec.Core.Frame.ABI.checkKeptPair_sound`, case `paired`,
subject `ConcreteSpec.Core.Frame.ABI.checkKeptPair`, mutant `ConcreteSpec.Core.Frame.ABI.wrongKeptChecker`). -/
example : ¬ CheckedPairCorrect ⟨.owned (7 : Nat), .kept⟩ (.receipt 7) (.receipt 7) (.owned 7)
    (wrongKeptChecker (0 : Nat) 1 ⟨.owned 7, .kept⟩ (.receipt 7) (.receipt 7) (.owned 7)) := by
  intro h
  exact h.1 rfl

/-- Instance (law `ConcreteSpec.Core.Frame.ABI.transferKept_sound`, case `completion`). -/
example : TransferCorrect (⟨0, 1, 7⟩ : KeptPair Nat Nat) (.kept 7)
    (transferKept ⟨0, 1, 7⟩ .complete (.kept 7)) :=
  @transferKept_sound Nat Nat ⟨0, 1, 7⟩ .complete (.kept 7)
    (by decide) (Or.inr rfl)

/-- A planted transfer dispatches completion to the submitting endpoint. -/
def wrongKeptEndpoint {F H : Type} (pair : KeptPair F H) :
    RetentionInstruction → RetentionState H → Option (ForeignEvent F H × RetentionState H)
  | .complete, .kept h => some (.call pair.submit h, .owned h)
  | instruction, before => transferKept pair instruction before

/-- Planted (law `ConcreteSpec.Core.Frame.ABI.transferKept_sound`, case `completion`,
subject `ConcreteSpec.Core.Frame.ABI.transferKept`, mutant `ConcreteSpec.Core.Frame.ABI.wrongKeptEndpoint`). -/
example : (0 : Nat) ≠ 1 ∧
    ((.kept 7 : RetentionState Nat) = .owned 7 ∨ (.kept 7 : RetentionState Nat) = .kept 7) ∧
    ¬ TransferCorrect (⟨0, 1, 7⟩ : KeptPair Nat Nat) (.kept 7)
      (wrongKeptEndpoint ⟨0, 1, 7⟩ .complete (.kept 7)) := by
  refine ⟨by decide, Or.inr rfl, ?_⟩
  intro h
  have bad : (0 : Nat) = 1 := h.1
  cases bad

/-- A planted transfer returns a receipt after completion instead of ownership. -/
def wrongKeptHandle {F H : Type} (pair : KeptPair F H) :
    RetentionInstruction → RetentionState H → Option (ForeignEvent F H × RetentionState H)
  | .complete, .kept h => some (.call pair.complete h, .kept h)
  | instruction, before => transferKept pair instruction before

/-- Planted (law `ConcreteSpec.Core.Frame.ABI.transferKept_sound`, case `completion`,
subject `ConcreteSpec.Core.Frame.ABI.transferKept`, mutant `ConcreteSpec.Core.Frame.ABI.wrongKeptHandle`). -/
example : (0 : Nat) ≠ 1 ∧
    ((.kept 7 : RetentionState Nat) = .owned 7 ∨ (.kept 7 : RetentionState Nat) = .kept 7) ∧
    ¬ TransferCorrect (⟨0, 1, 7⟩ : KeptPair Nat Nat) (.kept 7)
      (wrongKeptHandle ⟨0, 1, 7⟩ .complete (.kept 7)) := by
  refine ⟨by decide, Or.inr rfl, ?_⟩
  intro h
  exact h

/-- Helper (computation). The dependency-chain traversal visits every reachable module.
(approved: PENDING) -/
theorem retainChain_complete (root : Nat) : ∀ dependency, dependency ≤ root →
    dependency ∈ retainChain root := by
  induction root with
  | zero =>
    intro dependency h
    have he : dependency = 0 := by omega
    subst dependency
    simp [retainChain]
  | succ root ih =>
    intro dependency h
    by_cases he : dependency = root + 1
    · simp [retainChain, he]
    · have hd : dependency ≤ root := by omega
      simp [retainChain, ih dependency hd]

/-- Helper (computation). The loader retains all and only reachable modules, so
extra retained modules cannot satisfy the exact closure contract. (approved: PENDING) -/
theorem retainChain_membership (root dependency : Nat) :
    dependency ∈ retainChain root ↔ dependency ≤ root := by
  induction root with
  | zero => simp [retainChain]
  | succ root ih =>
    simp only [retainChain, List.mem_cons, ih]
    omega

/-- Helper (computation). The loader's module count agrees with the independent entry contract.
(approved: PENDING) -/
theorem retainChain_length (root : Nat) : (retainChain root).length = root + 1 := by
  induction root with
  | zero => rfl
  | succ root ih => simp [retainChain, ih]

/-- Helper (computation). Repeated successor implements the exit contract's arithmetic encoding.
(approved: PENDING) -/
theorem encodeToyExit_correct (root result : Nat) : encodeToyExit root result = result + root := by
  induction root with
  | zero => simp [encodeToyExit]
  | succ root ih => simp [encodeToyExit, ih, Nat.add_assoc]

/-- Law (subject `ConcreteSpec.Core.Frame.ABI.toyLaunch`).
Independent loading and launch execution establish the closure, entry/exit correspondence and
stack admission contract. No resulting relation is carried as an execution premise. -/
theorem toyLaunch_conforms (root reserved result : Nat) (adequate : 8 ≤ reserved) :
    ToyLaunchCorrect reserved (toyLaunch root reserved result) := by
  have hadmit : stackAdmission 8 reserved () = .entered () := stack_adequate 8 reserved () adequate
  simp only [toyLaunch, hadmit, ToyLaunchCorrect]
  exact ⟨retainChain_membership root, retainChain_length root,
    encodeToyExit_correct root result, adequate⟩

/-- Instance (law `ConcreteSpec.Core.Frame.ABI.toyLaunch_conforms`, case `dependency_chain`). -/
example : ToyLaunchCorrect 8 (toyLaunch 2 8 7) := toyLaunch_conforms 2 8 7 (by decide)

/-- A planted launcher appends one spurious byte to the exit encoding. -/
def wrongToyLaunch (root reserved result : Nat) : Startup ((Nat × List Nat) × Nat × Nat × Nat) :=
  match toyLaunch root reserved result with
  | .refused125 => .refused125
  | .entered (artifact, input, returned, observation) =>
    .entered (artifact, input, returned, observation + 1)

/-- Planted (law `ConcreteSpec.Core.Frame.ABI.toyLaunch_conforms`, case `dependency_chain`,
subject `ConcreteSpec.Core.Frame.ABI.toyLaunch`, mutant `ConcreteSpec.Core.Frame.ABI.wrongToyLaunch`). -/
example : 8 ≤ (8 : Nat) ∧ ¬ ToyLaunchCorrect 8 (wrongToyLaunch 2 8 7) := by
  refine ⟨by decide, ?_⟩
  intro h
  have bad := h.exit
  change 10 = 9 at bad
  contradiction


/-- A planted loader retains an unrelated module while leaving entry and exit unchanged. -/
def overRetainingToyLaunch (root reserved result : Nat) :
    Startup ((Nat × List Nat) × Nat × Nat × Nat) :=
  match toyLaunch root reserved result with
  | .refused125 => .refused125
  | .entered ((module, loaded), input, returned, observation) =>
    .entered ((module, (root + 1) :: loaded), input, returned, observation)

/-- Planted (law `ConcreteSpec.Core.Frame.ABI.toyLaunch_conforms`, case `dependency_chain`,
subject `ConcreteSpec.Core.Frame.ABI.toyLaunch`,
mutant `ConcreteSpec.Core.Frame.ABI.overRetainingToyLaunch`). -/
example : 8 ≤ (8 : Nat) ∧ ¬ ToyLaunchCorrect 8 (overRetainingToyLaunch 2 8 7) := by
  refine ⟨by decide, ?_⟩
  intro h
  have bad : 3 ≤ 2 := (h.closure 3).mp (by decide)
  omega

/-- A planted loader keeps only the root, omitting its transitive dependencies. -/
def underRetainingToyLaunch (root reserved result : Nat) :
    Startup ((Nat × List Nat) × Nat × Nat × Nat) :=
  match toyLaunch root reserved result with
  | .refused125 => .refused125
  | .entered ((module, _loaded), input, returned, observation) =>
    .entered ((module, [root]), input, returned, observation)

/-- Planted (law `ConcreteSpec.Core.Frame.ABI.toyLaunch_conforms`, case `dependency_chain`,
subject `ConcreteSpec.Core.Frame.ABI.toyLaunch`,
mutant `ConcreteSpec.Core.Frame.ABI.underRetainingToyLaunch`). -/
example : 8 ≤ (8 : Nat) ∧ ¬ ToyLaunchCorrect 8 (underRetainingToyLaunch 2 8 7) := by
  refine ⟨by decide, ?_⟩
  intro h
  have bad : 0 ∈ ([2] : List Nat) := (h.closure 0).mpr (by decide)
  contradiction


end ABI

end ConcreteSpec.Core.Frame
