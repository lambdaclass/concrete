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

/-- The empty footprint frames every operation. -/
theorem preserves_refl (st : State) : Footprint.empty.Preserves st st := by
  refine ⟨?_, ?_, ?_⟩
  · intro b i x hx ho
    simp only [hx, Option.map_some]
  · intro b x hx ho
    simp only [hx, Option.map_some]
  · intro p hp
    rfl

/-- Membership outside a union is membership outside both components. -/
theorem byteOutside_union {f g : Footprint} {b : MemBlockId} {i : Nat}
    (h : (f.union g).ByteOutside b i) : f.ByteOutside b i ∧ g.ByteOutside b i := by
  constructor <;> intro r hr <;> apply h r <;> simp [Footprint.union, hr]

/-- Block metadata outside a union is outside both components. -/
theorem blockOutside_union {f g : Footprint} {b : MemBlockId}
    (h : (f.union g).BlockOutside b) : f.BlockOutside b ∧ g.BlockOutside b := by
  have hb : b ∉ f.blocks ++ g.blocks := h
  simp only [List.mem_append, not_or] at hb
  exact hb

/-- Place membership outside a union is membership outside both components. -/
theorem placeOutside_union {f g : Footprint} {p : Place}
    (h : (f.union g).PlaceOutside p) : f.PlaceOutside p ∧ g.PlaceOutside p := by
  have hp : p ∉ f.places ++ g.places := h
  simp only [List.mem_append, not_or] at hp
  exact hp

/-- A lifecycle footprint excludes metadata framing for its own block, even when that block is
zero-sized. -/
theorem blockOutside_block {b c : MemBlockId} (h : (Footprint.block b).BlockOutside c) : c ≠ b := by
  intro hcb
  subst c
  exact h (by simp [Footprint.block])

/-- Sequential composition frames the union of the two operations' footprints. -/
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

/-- A successful byte store preserves every old byte outside the exact access range, every block's
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

/-- Releasing a block may change its lifecycle metadata, but frames all bytes and all metadata of
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

/-- Exposure may change only the exposed flag of the provenance block; every byte and all metadata
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

/-- Fresh allocation preserves every old byte, every old block's metadata and every place. The
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

/-- Replacing one structured place has exactly that place's footprint; bytes and block metadata are
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

/-- A successful typed write to structured storage has exactly the written place's footprint. -/
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

/-- A successful typed write to materialized storage has the byte footprint of its actual store. -/
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

/-- A raw Hybrid write has the byte footprint of its underlying memory store and touches no place. -/
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

/-- The allocated identity is disjoint from every range whose block existed before allocation. -/
theorem alloc_fresh_disjoint (tgt : Target) (st : State) (hw : st.memory.WellFormed tgt)
    (space : AddrSpace) (base size align : Nat) (b : MemBlockId) (m' : Memory)
    (h : st.memory.alloc tgt space base size align = .ok (b, m'))
    (r : ByteRange) (x : Block) (hx : st.memory.block r.block = some x) :
    (fullBlock b size).Disjoint r :=
  .inl (alloc_ne_existing tgt st.memory hw space base size align b r.block m' x h hx)

/-- Entering a C1 call saves the caller's locals in a `.ret` frame, binds the evaluated arguments to
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

/-- A returned value reaching a `.ret` frame restores the caller's locals and continuation, binding
the result at any requested destination. Leave and destination-bind instrumentation thread the shared
state through the same step. -/
theorem step_return_restores_caller {O : Exec.Operands} (P : Exec.Program O) (v : O.Val)
    (dst : Option Var) (caller callee : AList Var O.Val) (st stLeave stBind : O.Store)
    (k : List (Exec.Frame O)) (hl : O.life .leave st = some stLeave)
    (hb : Exec.bindLife O dst stLeave = some stBind) :
    Exec.step P ⟨.exiting (.ret v), callee, st, .ret dst caller :: k⟩ =
      some (.next ⟨.done, Exec.bindDst dst v caller, stBind, k⟩) :=
  Exec.step_return_caller P v dst caller callee st k stLeave stBind hl hb

/-- Successful cleanup does not disturb call-frame ownership: after every exited scope has run its
deferred calls, return restores the caller and binds its result at any requested destination. -/
theorem return_cleanup_restores_caller {O : Exec.Operands} (P : Exec.Program O) (v : O.Val)
    (dst : Option Var) (caller : AList Var O.Val) (k pre : List (Exec.Frame O)) {L : AList Var O.Val}
    {st stClean stLeave stBind : O.Store} {tr : List O.Event}
    (h : Exec.Cleans P pre L st tr stClean) (hp : ∀ f ∈ pre, Exec.Passes (.ret v) f)
    (hl : O.life .leave stClean = some stLeave)
    (hb : Exec.bindLife O dst stLeave = some stBind) :
    Exec.Steps P ⟨.exiting (.ret v), L, st, pre ++ .ret dst caller :: k⟩ tr
      ⟨.done, Exec.bindDst dst v caller, stBind, k⟩ :=
  Exec.return_reaches_caller P v dst caller k h hp stLeave stBind hl hb

/-- Every complete `C1Call` ends with its result bound into the caller locals and at the caller
continuation saved by its entry statement. The conclusion follows from the actual final return or
Unit fall-through step, including successful leave and destination-bind instrumentation. -/
theorem c1Call_restores_caller {O : Exec.Operands} {f : FnId} {P : Exec.Program O}
    {entry exit : Exec.Config O} (h : C1Call O f P entry exit) :
    ∃ (dst : Option Var) (result : O.Val) (caller : AList Var O.Val)
      (k : List (Exec.Frame O)) (args : List O.Expr) (site : SiteId) (st : O.Store),
      entry = ⟨.exec (.call dst f args site), caller, st, k⟩ ∧
      exit.ctrl = .done ∧ exit.locals = Exec.bindDst dst result caller ∧ exit.frames = k := by
  obtain ⟨fn, dst, caller, k, args, site, vs, st, stArgs, stEnter, stReturn, callee, tr, result,
    hf, he, hv, hlen, hl, hreturn⟩ := h
  refine ⟨dst, result, caller, k, args, site, st, he, ?_⟩
  cases hreturn with
  | inl hexplicit =>
    obtain ⟨hs, hr⟩ := hexplicit
    cases hl' : O.life .leave stReturn with
    | none => simp [Exec.step, Exec.stepExit, hl'] at hr
    | some stLeave =>
      cases hb : Exec.bindLife O dst stLeave with
      | none => simp [Exec.step, Exec.stepExit, hl', hb] at hr
      | some stBind =>
        simp only [Exec.step, Exec.stepExit, hl', Option.bind_some, hb, Option.map_some,
          Option.some.injEq, Exec.Out.next.injEq] at hr
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

private def c1Entry : Exec.Config c1Operands :=
  ⟨.exec (.call none c1FnId [] ⟨0⟩), [], (), []⟩

private def c1Boundary : Exec.Config c1Operands :=
  ⟨.exiting (.ret (7 : Nat)), [], (), [.ret none []]⟩

private def c1Exit : Exec.Config c1Operands := ⟨.done, [], (), []⟩

private theorem c1ExplicitCall : C1Call c1Operands c1FnId c1Program c1Entry c1Exit := by
  have hs : Exec.Steps c1Program c1Entry [] c1Boundary := by
    apply Exec.steps_next rfl
    apply Exec.steps_next rfl
    apply Exec.steps_next rfl
    exact Exec.Steps.refl _
  exact ⟨c1Fn, none, [], [], [], ⟨0⟩, [], (), (), (), (), [], [], (7 : Nat),
    rfl, rfl, rfl, rfl, rfl, .inl ⟨hs, rfl⟩⟩

/-- Accepted: an explicit return reaches its own `.ret` frame after evaluating and cleaning up the
callee, then takes the final return step. -/
example : C1Call c1Operands c1FnId c1Program c1Entry c1Exit := c1ExplicitCall

/-- Accepted: caller restoration has a non-vacuous explicit-return instance with the caller's empty
locals and continuation restored. -/
example : c1Exit.locals = [] ∧ c1Exit.frames = [] := by
  obtain ⟨dst, result, caller, k, args, site, st, he, hctrl, hlocals, hframes⟩ :=
    c1Call_restores_caller c1ExplicitCall
  have hk : k = [] := (congrArg Exec.Config.frames he).symm
  rw [hk] at hframes
  refine ⟨?_, hframes⟩
  cases dst <;> simp [c1Exit, Exec.bindDst] at hlocals ⊢

private def c1UnitFnId : FnId := ⟨⟨[]⟩, "unit"⟩

private def c1UnitFn : Exec.Fn c1Operands := ⟨[], (), ⟨0⟩, .skip⟩

private def c1UnitProgram : Exec.Program c1Operands := ⟨[(c1UnitFnId, c1UnitFn)]⟩

private def c1UnitEntry : Exec.Config c1Operands :=
  ⟨.exec (.call none c1UnitFnId [] ⟨0⟩), [], (), []⟩

private def c1UnitBoundary : Exec.Config c1Operands :=
  ⟨.done, [], (), [.ret none []]⟩

/-- Accepted: a Unit body can reach its `.ret` frame by falling through; the actual final step then
binds `unitVal` and restores the caller. -/
example : C1Call c1Operands c1UnitFnId c1UnitProgram c1UnitEntry c1Exit := by
  have hs : Exec.Steps c1UnitProgram c1UnitEntry [] c1UnitBoundary := by
    apply Exec.steps_next rfl
    apply Exec.steps_next rfl
    apply Exec.steps_next rfl
    exact Exec.Steps.refl _
  exact ⟨c1UnitFn, none, [], [], [], ⟨0⟩, [], (), (), (), (), [], [], (0 : Nat),
    rfl, rfl, rfl, rfl, rfl, .inr ⟨rfl, hs, rfl⟩⟩

private def c1BadExit : Exec.Config c1Operands := ⟨.done, [], (), [.ret none []]⟩

/-- Law (subject `ConcreteSpec.Core.Frame.C1Call`).
Complete calls restore the exact caller saved by entry, by evaluating their actual return step. -/
theorem caller_restoration (O : Exec.Operands) (f : FnId) (P : Exec.Program O) :
    CallerRestoration O f P := by
  intro entry exit call
  exact c1Call_restores_caller call

/-- Instance (law `ConcreteSpec.Core.Frame.caller_restoration`, case `explicit_return`). -/
example : CallerRestoration c1Operands c1FnId c1Program :=
  caller_restoration c1Operands c1FnId c1Program

/-- A planted completion rule accepts calls before their return frame is popped. -/
def incompleteC1Call (_O : Exec.Operands) (_f : FnId) (_P : Exec.Program _O)
    (_entry _exit : Exec.Config _O) : Prop := True

/-- Planted (law `ConcreteSpec.Core.Frame.caller_restoration`, case `explicit_return`,
subject `ConcreteSpec.Core.Frame.C1Call`, mutant `ConcreteSpec.Core.Frame.incompleteC1Call`). -/
example : ¬ (∀ entry exit, incompleteC1Call c1Operands c1FnId c1Program entry exit →
    ∃ (dst : Option Var) (result : Nat) (caller : AList Var Nat)
      (k : List (Exec.Frame c1Operands)) (args : List Nat) (site : SiteId) (st : Unit),
      entry = ⟨.exec (.call dst c1FnId args site), caller, st, k⟩ ∧
      exit.ctrl = .done ∧ exit.locals = Exec.bindDst dst result caller ∧ exit.frames = k) := by
  intro h
  obtain ⟨dst, result, caller, k, args, site, st, he, _ctrl, _locals, hframes⟩ :=
    h c1Entry c1BadExit trivial
  have hk : k = [] := (congrArg Exec.Config.frames he).symm
  rw [hk] at hframes
  simp [c1BadExit] at hframes

/-- Law (subject `ConcreteSpec.Core.Frame.runEffect`).
Actual C4 instruction execution preserves bytes, metadata and places outside its computed effects. -/
theorem runEffect_frame (tgt : Target) (before : State) (command : EffectCommand)
    (wellFormed : before.memory.WellFormed tgt) : EffectResult before (runEffect tgt before command) := by
  cases command with
  | store pointer bytes alignment =>
    cases hs : before.memory.store tgt pointer bytes alignment with
    | none => simp [runEffect, hs, EffectResult]
    | some memory =>
      simp only [runEffect, hs, Option.map_some, EffectResult]
      exact store_frame tgt before wellFormed memory pointer bytes alignment _ rfl hs
  | free block =>
    cases hb : before.memory.block block with
    | none => simp [runEffect, Memory.free, hb, EffectResult]
    | some x =>
      cases hs : before.memory.free block with
      | none => simp [runEffect, hs, EffectResult]
      | some memory =>
        simp only [runEffect, hs, Option.map_some, EffectResult]
        exact free_frame before block x memory hb hs
  | expose pointer =>
    cases hp : pointer.prov with
    | none => simp [runEffect, hp, EffectResult]
    | some block =>
      cases hb : before.memory.block block with
      | none => simp [runEffect, hp, Memory.expose, hb, EffectResult]
      | some x =>
        cases hs : before.memory.expose pointer with
        | none => simp [runEffect, hp, hs, EffectResult]
        | some memory =>
          simp only [runEffect, hp, Option.bind_some, hs, Option.map_some, EffectResult]
          exact expose_frame before memory pointer block x hp hb hs
  | alloc space base size alignment =>
    cases ha : before.memory.alloc tgt space base size alignment with
    | error _ => simp [runEffect, ha, EffectResult]
    | ok output =>
      obtain ⟨block, memory⟩ := output
      simp only [runEffect, ha, EffectResult]
      exact alloc_frame tgt before wellFormed space base size alignment block memory ha
  | set place cell => exact set_frame before place cell

/-- Instance (law `ConcreteSpec.Core.Frame.runEffect_frame`, case `byte_store`). -/
example : EffectResult metadataStoreBefore
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
    ¬ EffectResult metadataStoreBefore
      (corruptEffect linuxX8664 metadataStoreBefore (.store metadataStorePtr [.init 7 none] 1)) := by
  refine ⟨metadataStoreWellFormed, ?_⟩
  intro h
  have hm := h.2.1 ⟨0⟩ metadataStoreBlock (by decide) (by decide)
  simp [metadataStoreBefore, metadataStoreBlock,
    metadataStorePtr, blockMetadata, Memory.modify, Memory.block,
    AList.update, AList.lookup] at hm

/-- Helper (computation). The actual four C1 steps implement the successful C4 store, ending
with `.done` and an empty caller continuation. (approved: PENDING) -/
theorem runStoringCall_result (tgt : Target) (before : State) (pointer : Ptr)
    (bytes : List MByte) (alignment : Nat) :
    runStoringCall tgt before pointer bytes alignment =
      (before.memory.store tgt pointer bytes alignment).map fun memory => { before with memory := memory } := by
  cases hs : before.memory.store tgt pointer bytes alignment <;>
    simp [runStoringCall, silentSteps, storingProgram, storingOperands, storingFn, Exec.step,
      Exec.stepExit, Exec.onRes, Exec.evalArgs, Exec.dropOut, Exec.bindLife, Exec.dead, Exec.dom,
      Exec.restrict, Exec.bindDst, AList.lookup, hs]

/-- Law (subject `ConcreteSpec.Core.Frame.runStoringCall`).
A complete C1 callee really modifies the requested byte range, while its actual entry, return,
scope cleanup and return-frame pop preserve unrelated state. The proof uses the C4 execution,
not a frame contract supplied as a premise. -/
theorem storingCall_frame (tgt : Target) (before : State) (pointer : Ptr) (bytes : List MByte)
    (alignment : Nat) (wellFormed : before.memory.WellFormed tgt) :
    CallFrameResult before
      (accessFootprint (before.memory.access tgt pointer bytes.length alignment) bytes.length)
      (runStoringCall tgt before pointer bytes alignment) := by
  rw [runStoringCall_result]
  cases hs : before.memory.store tgt pointer bytes alignment with
  | none => trivial
  | some memory => exact store_frame tgt before wellFormed memory pointer bytes alignment _ rfl hs

/-- Instance (law `ConcreteSpec.Core.Frame.storingCall_frame`, case `modifying_callee`). -/
example : CallFrameResult metadataStoreBefore (.byte ⟨⟨0⟩, 0, 1⟩)
    (runStoringCall linuxX8664 metadataStoreBefore metadataStorePtr [.init 7 none] 1) :=
  storingCall_frame linuxX8664 metadataStoreBefore metadataStorePtr [.init 7 none] 1 metadataStoreWellFormed

/-- A planted C1 execution corrupts unrelated metadata after returning from the real callee. -/
def corruptStoringCall (tgt : Target) (before : State) (pointer : Ptr) (bytes : List MByte)
    (alignment : Nat) : Option State :=
  (runStoringCall tgt before pointer bytes alignment).map fun after =>
    { after with memory := after.memory.modify ⟨0⟩ fun block => { block with live := false } }

/-- Planted (law `ConcreteSpec.Core.Frame.storingCall_frame`, case `modifying_callee`,
subject `ConcreteSpec.Core.Frame.runStoringCall`, mutant `ConcreteSpec.Core.Frame.corruptStoringCall`). -/
example : metadataStoreBefore.memory.WellFormed linuxX8664 ∧
    ¬ CallFrameResult metadataStoreBefore (.byte ⟨⟨0⟩, 0, 1⟩)
      (corruptStoringCall linuxX8664 metadataStoreBefore metadataStorePtr [.init 7 none] 1) := by
  refine ⟨metadataStoreWellFormed, ?_⟩
  intro h
  rw [corruptStoringCall, runStoringCall_result] at h
  have hm := h.2.1 ⟨0⟩ metadataStoreBlock (by decide) (by decide)
  simp [metadataStoreBefore, metadataStoreBlock, metadataStorePtr, blockMetadata,
    Memory.modify, Memory.block, AList.update, AList.lookup] at hm

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

/-- Law (subject `ConcreteSpec.Core.Frame.c4Execution`).
An independently described C4 execution preserves everything outside its computed footprint. -/
theorem c4_effects_frame (tgt : Target) :
    ∀ before f after, c4Execution tgt before f after → f.Preserves before after := by
  intro before f after h
  induction h with
  | idle st => exact preserves_refl st
  | set st p cell => exact set_frame st p cell
  | store st m' p bs align access hw ha hs =>
    exact store_frame tgt st hw m' p bs align access ha hs
  | free st b block m' hb hf => exact free_frame st b block m' hb hf
  | expose st m' p b block hp hb he => exact expose_frame st m' p b block hp hb he
  | alloc st space base size align b m' hw ha =>
    exact alloc_frame tgt st hw space base size align b m' ha
  | seq _ _ ih₁ ih₂ => exact preserves_seq ih₁ ih₂

/-- Instance (law `ConcreteSpec.Core.Frame.c4_effects_frame`, case `byte_store`). -/
example : ∀ before f after, c4Execution linuxX8664 before f after →
    f.Preserves before after := c4_effects_frame linuxX8664

/-- Accepted: a nonempty C4 execution changes byte 0 and keeps the unrelated byte 1. -/
example : c4Execution linuxX8664 metadataStoreBefore (.byte ⟨⟨0⟩, 0, 1⟩)
    metadataStoreAfter :=
  C4Effects.store metadataStoreBefore metadataStoreAfter.memory metadataStorePtr [.init 7 none]
    1 (some (some (⟨0⟩, 0))) metadataStoreWellFormed (by decide) rfl

/-- A planted effect rule admits an undeclared mutation. -/
def unframedExecution (_tgt : Target) (_before : State) (_f : Footprint) (_after : State) : Prop :=
  True

/-- Planted (law `ConcreteSpec.Core.Frame.c4_effects_frame`, case `byte_store`,
subject `ConcreteSpec.Core.Frame.c4Execution`, mutant `ConcreteSpec.Core.Frame.unframedExecution`). -/
example : ¬ (∀ before f after, unframedExecution linuxX8664 before f after →
    f.Preserves before after) := by
  intro h
  have hm := (h metadataStoreBefore (.byte ⟨⟨0⟩, 0, 1⟩) metadataStoreBadAfter trivial).2.1
    ⟨0⟩ metadataStoreBlock (by decide) (by decide)
  simp [metadataStoreBadAfter, metadataStoreAfter, metadataStoreBefore, metadataStoreBlock,
    blockMetadata, Memory.block, Memory.modify, AList.update, AList.lookup] at hm

/-- Helper (inversion). Induction over actual C1 steps composes their independently justified
C4 effects, including request/answer transitions. (approved: PENDING) -/
theorem c1_steps_frame {O : Exec.Operands} {P : Exec.Program O} {project : O.Store → State}
    {tgt : Target} {f : Footprint} (implementation : C1EffectImplementation O P project tgt f)
    {entry exit : Exec.Config O} {trace : List O.Event} (steps : Exec.Steps P entry trace exit) :
    f.Preserves (project entry.store) (project exit.store) := by
  induction steps with
  | refl entry =>
    apply preserves_covers (preserves_refl (project entry.store))
    refine ⟨?_, ?_, ?_⟩
    · intro b i ho r hr
      cases hr
    · intro b ho hr
      cases hr
    · intro p ho hr
      cases hr
  | @step before label middle trace after step _ ih =>
    obtain ⟨effectFootprint, effects, cover⟩ := implementation before label middle step
    exact preserves_trans (preserves_covers (c4_effects_frame tgt _ _ _ effects) cover) ih

/-- Law (subject `ConcreteSpec.Core.Frame.C1Call`).
Complete-call framing follows from the callee's actual steps and their C4 decompositions,
including its final return/pop. The call contract is the conclusion, never a premise. -/
theorem c1_call_frame {O : Exec.Operands} {f : FnId} {P : Exec.Program O}
    {project : O.Store → State} {tgt : Target} {advertised : Footprint}
    (implementation : C1EffectImplementation O P project tgt advertised) :
    C1Frames O f P project advertised := by
  intro entry exit call
  obtain ⟨fn, dst, caller, k, args, site, vs, st, stArgs, stEnter, stReturn, callee, tr, result,
    _hf, _he, _hv, _hlen, _hl, returned⟩ := call
  rcases returned with ⟨steps, pop⟩ | ⟨_unit, steps, pop⟩
  all_goals
    have framedPrefix := c1_steps_frame implementation steps
    obtain ⟨effectFootprint, effects, cover⟩ := implementation _ _ _ (.silent pop)
    exact preserves_trans framedPrefix (preserves_covers (c4_effects_frame tgt _ _ _ effects) cover)



namespace ABI

/-- Law (subject `ConcreteSpec.Core.Frame.ABI.layoutBytes`).
Executing a layout plan produces a size aligned to its declared positive alignment. -/
theorem layout_aligned (plan : LayoutPlan) (_positive : 0 < plan.alignment) :
    layoutBytes plan % plan.alignment = 0 := by
  exact Nat.mul_mod_left _ _

/-- Instance (law `ConcreteSpec.Core.Frame.ABI.layout_aligned`, case `two_slots`). -/
example : layoutBytes ⟨8, [1, 2]⟩ % 8 = 0 :=
  layout_aligned ⟨8, [1, 2]⟩ (by decide)

/-- A planted layout forgets to include its final byte in alignment rounding. -/
def wrongLayoutBytes (plan : LayoutPlan) : Nat := layoutBytes plan + 1

/-- Planted (law `ConcreteSpec.Core.Frame.ABI.layout_aligned`, case `two_slots`,
subject `ConcreteSpec.Core.Frame.ABI.layoutBytes`, mutant `ConcreteSpec.Core.Frame.ABI.wrongLayoutBytes`). -/
example : 0 < (8 : Nat) ∧ ¬ wrongLayoutBytes ⟨8, [1, 2]⟩ % 8 = 0 := by decide

/-- Law (subject `ConcreteSpec.Core.Frame.ABI.stackSamples`).
The static bound covers every sample of an independently described nested stack execution. -/
theorem stackSamples_bounded (code : StackCode) : StackSafe code := by
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

/-- Instance (law `ConcreteSpec.Core.Frame.ABI.stackSamples_bounded`, case `nested_call`). -/
example : StackSafe (.frame 16 (.seq (.frame 32 .done) (.frame 8 .done))) :=
  stackSamples_bounded (.frame 16 (.seq (.frame 32 .done) (.frame 8 .done)))

/-- A planted runtime consumes an extra byte at every stack sample. -/
def overBudgetSamples (code : StackCode) (base : Nat) : List Nat :=
  (stackSamples code base).map (· + 1)

/-- Planted (law `ConcreteSpec.Core.Frame.ABI.stackSamples_bounded`, case `nested_call`,
subject `ConcreteSpec.Core.Frame.ABI.stackSamples`, mutant `ConcreteSpec.Core.Frame.ABI.overBudgetSamples`). -/
example : ¬ (∀ base sample,
    sample ∈ overBudgetSamples (.frame 16 (.seq (.frame 32 .done) (.frame 8 .done))) base →
    sample ≤ base + computedStackBound (.frame 16 (.seq (.frame 32 .done) (.frame 8 .done)))) := by
  intro h
  have bad := h 0 49 (by decide)
  contradiction


/-- Helper (inversion). Establish the relation form of toy-call conformance from its
independent execution witnesses. (approved: PENDING)
The independently executed toy call satisfies the complete typed ABI contract for every base
machine, argument and return value, including its private spill and balanced stack operations. -/
theorem toyTransition_conforms : TransitionConforms toySignature toyTransition := by
  intro before after args result h
  obtain ⟨base, arg, rfl, rfl, rfl⟩ := h
  refine ⟨?_, ?_, ?_, ?_⟩
  · simp [toySignature, ArgumentsHold, Binding.Holds, Location.read, toyInput, runToy]
  · simp [toySignature, Binding.Holds, Location.read, toyBody, toyInput, runToy]
  · refine ⟨?_, ?_, ?_⟩
    · intro r hr
      simp only [toySignature, List.mem_cons, List.not_mem_nil, or_false] at hr
      subst r
      simp [toyInput, toyBody, runToy]
    · intro slot hs
      have hn : slot ≠ base.stackPointer + 8 := by
        change slot < base.stackPointer at hs
        omega
      simp [toyInput, toyBody, runToy, hn]
    · simp [toyInput, toyBody, runToy]
  · intro r hr
    have hn : r ≠ 1 := by simpa [toySignature] using hr
    simp [toyInput, toyBody, runToy, hn]

/-- A populated base machine, including a caller-owned stack slot. -/
def toyBase : Machine Nat Nat := ⟨fun r => 10 + r, fun slot => 20 + slot, 1⟩

/-- Law (subject `ConcreteSpec.Core.Frame.ABI.toyCall`).
Executing input packing and the independent toy body establishes the complete ABI contract. -/
theorem toyCall_conforms (base : Machine Nat Nat) (arg result : Nat) :
    toySignature.Call (toyInput base arg) (toyCall base arg result) [arg] result :=
  toyTransition_conforms _ _ _ _ ⟨base, arg, rfl, rfl, rfl⟩

/-- Instance (law `ConcreteSpec.Core.Frame.ABI.toyCall_conforms`, case `spill`). -/
example : toySignature.Call (toyInput toyBase 11) (toyCall toyBase 11 7) [11] 7 :=
  toyCall_conforms toyBase 11 7

/-- A planted call clobbers a callee-saved register. -/
def wrongToySaved (base : Machine Nat Nat) (arg result : Nat) : Machine Nat Nat :=
  runToy [.register 0 99] (toyCall base arg result)
/-- A planted call overwrites a caller-owned stack slot. -/
def wrongToySlot (base : Machine Nat Nat) (arg result : Nat) : Machine Nat Nat :=
  runToy [.release 1, .slot 0 99, .reserve 1] (toyCall base arg result)
/-- A planted call leaks its frame. -/
def wrongToyPointer (base : Machine Nat Nat) (arg result : Nat) : Machine Nat Nat :=
  runToy [.reserve 8] (toyCall base arg result)
/-- A planted call overwrites the returned value. -/
def wrongToyResult (base : Machine Nat Nat) (arg result : Nat) : Machine Nat Nat :=
  runToy [.register 1 99] (toyCall base arg result)

/-- Planted (law `ConcreteSpec.Core.Frame.ABI.toyCall_conforms`, case `spill`,
subject `ConcreteSpec.Core.Frame.ABI.toyCall`, mutant `ConcreteSpec.Core.Frame.ABI.wrongToySaved`). -/
example : ¬ toySignature.Call (toyInput toyBase 11) (wrongToySaved toyBase 11 7) [11] 7 := by
  intro h
  have bad := h.preserved.1 0 (by simp [toySignature])
  simp [wrongToySaved, toyCall, toyInput, toyBody, toyBase, runToy] at bad

/-- Planted (law `ConcreteSpec.Core.Frame.ABI.toyCall_conforms`, case `spill`,
subject `ConcreteSpec.Core.Frame.ABI.toyCall`, mutant `ConcreteSpec.Core.Frame.ABI.wrongToySlot`). -/
example : ¬ toySignature.Call (toyInput toyBase 11) (wrongToySlot toyBase 11 7) [11] 7 := by
  intro h
  have bad := h.preserved.2.1 0 (by decide)
  simp [wrongToySlot, toyCall, toyInput, toyBody, toyBase, runToy] at bad

/-- Planted (law `ConcreteSpec.Core.Frame.ABI.toyCall_conforms`, case `spill`,
subject `ConcreteSpec.Core.Frame.ABI.toyCall`, mutant `ConcreteSpec.Core.Frame.ABI.wrongToyPointer`). -/
example : ¬ toySignature.Call (toyInput toyBase 11) (wrongToyPointer toyBase 11 7) [11] 7 := by
  intro h
  have bad := h.preserved.2.2
  simp [wrongToyPointer, toyCall, toyInput, toyBody, toyBase, runToy] at bad

/-- Planted (law `ConcreteSpec.Core.Frame.ABI.toyCall_conforms`, case `spill`,
subject `ConcreteSpec.Core.Frame.ABI.toyCall`, mutant `ConcreteSpec.Core.Frame.ABI.wrongToyResult`). -/
example : ¬ toySignature.Call (toyInput toyBase 11) (wrongToyResult toyBase 11 7) [11] 7 := by
  intro h
  have bad := h.result
  simp [toySignature, Binding.Holds, Location.read, wrongToyResult, toyCall,
    toyInput, toyBody, toyBase, runToy] at bad

/-- Preserved machine state composes across sequential calls with the same saved-register set. -/
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

/-- Sequential calls preserve every register outside the union of their clobber sets. -/
theorem machine_registerFrame_union {Reg Word : Type} {before middle after : Machine Reg Word}
    {clobbers₁ clobbers₂ : List Reg} (h₁ : Machine.RegisterFrame before middle clobbers₁)
    (h₂ : Machine.RegisterFrame middle after clobbers₂) :
    Machine.RegisterFrame before after (clobbers₁ ++ clobbers₂) := by
  intro r hr
  have hr' : r ∉ clobbers₁ ∧ r ∉ clobbers₂ := by
    simpa only [List.mem_append, not_or] using hr
  exact (h₂ r hr'.2).trans (h₁ r hr'.1)

/-- Helper (projection). Extracts an ABI contract obligation; implementation evidence is
`ConcreteSpec.Core.Frame.ABI.toyCall_conforms`. (approved: PENDING)
 A well-formed returning ABI call restores every callee-saved register exactly: well-formedness
puts those registers outside the clobber set, and the call's register frame preserves that set. -/
theorem callee_saved_registers_equal {Ty Val Reg Word : Type} {sig : Signature Ty Val Reg Word}
    {before after : Machine Reg Word} {args : List Val} {result : Val}
    (hw : sig.WellFormed) (h : sig.Call before after args result) :
    ∀ r ∈ sig.calleeSaved, after.regs r = before.regs r :=
  fun r hr => h.clobbers r (hw.2.2.2 r hr)

/-- Helper (projection). Extracts an ABI contract obligation; implementation evidence is
`ConcreteSpec.Core.Frame.ABI.toyCall_conforms`. (approved: PENDING)
 Registers outside the clobber set are unchanged; equivalently, clobbers are the only registers
permitted to differ after return. -/
theorem clobbers_only_changed_registers {Ty Val Reg Word : Type} {sig : Signature Ty Val Reg Word}
    {before after : Machine Reg Word} {args : List Val} {result : Val}
    (h : sig.Call before after args result) :
    ∀ r, r ∉ sig.clobbers → after.regs r = before.regs r :=
  h.clobbers

/-- Helper (projection). Extracts an ABI contract obligation; implementation evidence is
`ConcreteSpec.Core.Frame.ABI.toyCall_conforms`. (approved: PENDING)
 The caller's stack slots below its stack pointer are restored exactly. -/
theorem caller_stack_slots_equal {Ty Val Reg Word : Type} {sig : Signature Ty Val Reg Word}
    {before after : Machine Reg Word} {args : List Val} {result : Val}
    (h : sig.Call before after args result) :
    ∀ slot, slot < before.stackPointer → after.slots slot = before.slots slot :=
  h.preserved.2.1

/-- Helper (projection). Extracts an ABI contract obligation; implementation evidence is
`ConcreteSpec.Core.Frame.ABI.toyCall_conforms`. (approved: PENDING)
 Returning restores the caller's stack pointer exactly. -/
theorem stack_pointer_restored {Ty Val Reg Word : Type} {sig : Signature Ty Val Reg Word}
    {before after : Machine Reg Word} {args : List Val} {result : Val}
    (h : sig.Call before after args result) : after.stackPointer = before.stackPointer :=
  h.preserved.2.2

/-- Helper (projection). Extracts an ABI contract obligation; implementation evidence is
`ConcreteSpec.Core.Frame.ABI.toyCall_conforms`. (approved: PENDING)
 Typed arguments occupy their signature locations at entry. -/
theorem typed_arguments_at_entry {Ty Val Reg Word : Type} {sig : Signature Ty Val Reg Word}
    {before after : Machine Reg Word} {args : List Val} {result : Val}
    (h : sig.Call before after args result) : ArgumentsHold sig.encode sig.arguments args before :=
  h.arguments

/-- Helper (projection). Extracts an ABI contract obligation; implementation evidence is
`ConcreteSpec.Core.Frame.ABI.toyCall_conforms`. (approved: PENDING)
 The typed result occupies its return-convention locations after return. -/
theorem typed_result_at_return {Ty Val Reg Word : Type} {sig : Signature Ty Val Reg Word}
    {before after : Machine Reg Word} {args : List Val} {result : Val}
    (h : sig.Call before after args result) : sig.result.Holds sig.encode result after :=
  h.result

/-- Helper (projection). Describes the contract or its arithmetic; independent execution
evidence must be supplied separately. (approved: PENDING)
 A well-formed signature's frame size is a multiple of its positive power-of-two alignment. -/
theorem frame_size_aligned {Ty Val Reg Word : Type} {sig : Signature Ty Val Reg Word}
    (h : sig.WellFormed) : IsPow2 sig.frameAlign ∧ sig.frameSize % sig.frameAlign = 0 :=
  ⟨h.1, h.2.1⟩

/-- Helper (projection). Describes the contract or its arithmetic; independent execution
evidence must be supplied separately. (approved: PENDING)
 The callee's own frame is included in its final-artifact stack bound. -/
theorem frame_within_stack_bound {Ty Val Reg Word : Type} {sig : Signature Ty Val Reg Word}
    (h : sig.WellFormed) : sig.frameSize ≤ sig.stackBound :=
  h.2.2.1

/-- Helper (projection). Describes the contract or its arithmetic; independent execution
evidence must be supplied separately. (approved: PENDING)
 Stack accounting of a call is exactly caller frame plus callee bound. -/
theorem call_stack_account {Ty Val Reg Word : Type} (callerFrame : Nat)
    (sig : Signature Ty Val Reg Word) : callStack callerFrame sig = callerFrame + sig.stackBound :=
  rfl

/-- Helper (projection). Describes the contract or its arithmetic; independent execution
evidence must be supplied separately. (approved: PENDING)
 Whole-artifact accounting adds launcher/runtime bytes to that same handoff. -/
theorem artifact_stack_account {Ty Val Reg Word : Type} (launcherRuntime callerFrame : Nat)
    (sig : Signature Ty Val Reg Word) :
    artifactStack launcherRuntime callerFrame sig =
      launcherRuntime + callerFrame + sig.stackBound := by
  simp [artifactStack, callStack, Nat.add_assoc]

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

/-- A reservation below the final artifact bound refuses startup with status 125. -/
theorem stack_under_reservation {Input : Type} (bound reserved : Nat) (input : Input)
    (h : reserved < bound) : stackAdmission bound reserved input = .refused125 := by
  simp [stackAdmission, h]

/-- A reservation at least the bound admits the same launcher input. -/
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
  exact ⟨retainChain_complete root, retainChain_length root,
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


end ABI

end ConcreteSpec.Core.Frame
