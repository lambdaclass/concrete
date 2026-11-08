import ConcreteSpec.Spec.Frame
import ConcreteSpec.Proofs.Memory

/-!
# Laws of logical footprints and frames (foundation C10)

Byte-range disjointness, preservation under sequential composition, and the frame laws of C4 byte
store, release, exposure and fresh allocation. The allocation theorem also shows that the fresh block
is disjoint from every range in an old block.
-/
namespace ConcreteSpec.Core.Frame
open ConcreteSpec.Core.Mem
open ConcreteSpec.Core.Hybrid

/-- The empty footprint frames every operation. -/
theorem preserves_refl (st : State) : Footprint.empty.Preserves st st := by
  constructor
  · intro b i x hx ho
    simp only [hx, Option.map_some]
  · intro p hp
    rfl

/-- Membership outside a union is membership outside both components. -/
theorem byteOutside_union {f g : Footprint} {b : MemBlockId} {i : Nat}
    (h : (f.union g).ByteOutside b i) : f.ByteOutside b i ∧ g.ByteOutside b i := by
  constructor <;> intro r hr <;> apply h r <;> simp [Footprint.union, hr]

/-- Place membership outside a union is membership outside both components. -/
theorem placeOutside_union {f g : Footprint} {p : Place}
    (h : (f.union g).PlaceOutside p) : f.PlaceOutside p ∧ g.PlaceOutside p := by
  have hp : p ∉ f.places ++ g.places := h
  simp only [List.mem_append, not_or] at hp
  exact hp

/-- Sequential composition frames the union of the two operations' footprints. -/
theorem preserves_seq {st st' st'' : State} {f g : Footprint}
    (h₁ : f.Preserves st st') (h₂ : g.Preserves st' st'') : (f.union g).Preserves st st'' := by
  constructor
  · intro b i x hx ho
    obtain ⟨hf, hg⟩ := byteOutside_union ho
    have he₁ := h₁.1 b i x hx hf
    cases hy : st'.memory.block b with
    | none => simp [hy] at he₁
    | some y =>
      simp only [hy, Option.map_some, Option.some.injEq] at he₁
      have he₂ := h₂.1 b i y hy hg
      exact he₂.trans (congrArg some he₁)
  · intro p hp
    obtain ⟨hf, hg⟩ := placeOutside_union hp
    exact (h₂.2 p hg).trans (h₁.2 p hf)

/-- A successful byte store preserves every old byte outside the exact access range and every
structured place. A zero-byte store has the empty footprint. -/
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
      constructor
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
      · intro q hq
        rfl

/-- Releasing a block may change its lifecycle metadata, but frames all bytes outside its full-block
footprint and every structured place. -/
theorem free_frame (st : State) (b : MemBlockId) (x : Block) (m' : Memory)
    (hx : st.memory.block b = some x) (h : st.memory.free b = some m') :
    Footprint.Preserves st { st with memory := m' } (.byte (fullBlock b x.size)) := by
  unfold Memory.free at h
  rw [hx] at h
  by_cases hl : x.live = true
  · simp only [hl, ↓reduceIte, Option.some.injEq] at h
    subst m'
    constructor
    · intro c i y hy ho
      by_cases hc : c = b
      · subst c
        rw [hx] at hy
        cases hy
        simp only [block_modify, ↓reduceIte, hx, Option.map_some]
      · simp only [block_modify, hc, ↓reduceIte, hy, Option.map_some]
    · intro q hq
      rfl
  · simp [hl] at h

/-- Exposure may change only the exposed flag of the provenance block; every byte outside its
full-block footprint and every place is preserved. -/
theorem expose_frame (st : State) (m' : Memory) (p : Ptr) (b : MemBlockId) (x : Block)
    (hp : p.prov = some b) (hx : st.memory.block b = some x) (h : st.memory.expose p = some m') :
    Footprint.Preserves st { st with memory := m' } (.byte (fullBlock b x.size)) := by
  obtain ⟨c, y, hp', hx', hl, rfl⟩ := expose_result st.memory m' p h
  rw [hp] at hp'
  cases hp'
  rw [hx] at hx'
  cases hx'
  constructor
  · intro d i z hz ho
    by_cases hd : d = b
    · subst d
      rw [hx] at hz
      cases hz
      simp only [block_modify, ↓reduceIte, hx, Option.map_some]
    · simp only [block_modify, hd, ↓reduceIte, hz, Option.map_some]
  · intro q hq
    rfl

/-- Fresh allocation preserves every old byte and place. The footprint is the complete new block;
old bytes are necessarily outside it because their allocation identities differ. -/
theorem alloc_frame (tgt : Target) (st : State) (hw : st.memory.WellFormed tgt)
    (space : AddrSpace) (base size align : Nat) (b : MemBlockId) (m' : Memory)
    (h : st.memory.alloc tgt space base size align = .ok (b, m')) :
    Footprint.Preserves st { st with memory := m' } (.byte (fullBlock b size)) := by
  obtain ⟨hb, hm, hfits⟩ := alloc_result tgt st.memory space base size align b m' h
  constructor
  · intro c i x hx ho
    have hne : b ≠ c := alloc_ne_existing tgt st.memory hw space base size align b c m' x h hx
    subst b
    rw [hm]
    change AList.lookup st.memory.blocks c = some x at hx
    simp only [Memory.block, AList.lookup, hne, ↓reduceIte, hx, Option.map_some]
  · intro p hp
    rfl

/-- The allocated identity is disjoint from every range whose block existed before allocation. -/
theorem alloc_fresh_disjoint (tgt : Target) (st : State) (hw : st.memory.WellFormed tgt)
    (space : AddrSpace) (base size align : Nat) (b : MemBlockId) (m' : Memory)
    (h : st.memory.alloc tgt space base size align = .ok (b, m'))
    (r : ByteRange) (x : Block) (hx : st.memory.block r.block = some x) :
    (fullBlock b size).Disjoint r :=
  .inl (alloc_ne_existing tgt st.memory hw space base size align b r.block m' x h hx)

/-- A complete C1 call has exactly the frame law supplied by its callee contract. -/
theorem c1_call_frame {O : Exec.Operands} {f : FnId} {P : Exec.Program O}
    {project : O.Store → State} (contract : C1CallContract O f P project)
    {entry exit : Exec.Config O} (h : C1Call O f P entry exit) :
    contract.footprint.Preserves (project entry.store) (project exit.store) :=
  contract.frame h

end ConcreteSpec.Core.Frame
