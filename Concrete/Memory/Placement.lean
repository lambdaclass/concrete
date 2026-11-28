import Concrete.Interfaces.Contracts
import ConcreteSpec.Proofs.Memory
import ConcreteSpec.Tactics

namespace Concrete.Interfaces.Memory

open ConcreteSpec.Core ConcreteSpec.StageInterface

/-- The live allocation backing a nonempty materialized root. Its one-past
address is representable, so even an empty run there can be read or written. -/
private structure Backed (target : Target) (c : Nat) (heap : Mem.Memory)
    (range : Hybrid.Range) (layout : Layout) (id : BlockId) (block : Mem.Block) : Prop where
  lookup : heap.block id = some block
  prov : range.ptr.prov = some id
  covers : CoversBlock range block
  extent : range.extent = layout.size
  alignment : range.align = layout.align
  aligned : layout.align ∣ range.ptr.addr
  nonnull : target.null < range.ptr.addr
  fits : range.ptr.addr + layout.size < c
  validSpace : range.ptr.space ∈ target.spaces
  fresh : id.id < heap.next

private theorem Backed.access {target c heap range root id block offset size align}
    (h : Backed target c heap range root id block)
    (bounds : offset + size ≤ root.size) (aligned : align ∣ range.ptr.addr + offset) :
    heap.access target (atOffset range.ptr offset) size align c =
      some (if size = 0 then none else some (id, offset)) := by
  have low := h.nonnull
  have high := h.fits
  obtain ⟨live, space, bp, blockSize, _⟩ := h.covers
  have extent := blockSize.trans h.extent
  have rem := Nat.mod_eq_zero_of_dvd aligned
  by_cases z : size = 0
  · simp [Mem.Memory.access, z, atOffset, Mem.Ptr.Valid, rem, h.validSpace,
      show range.ptr.addr + offset ≠ target.null by omega,
      show range.ptr.addr + offset < c by omega]
  · simp [Mem.Memory.access, z, atOffset, h.prov, h.lookup, live, space, bp, extent, rem,
      show range.ptr.addr + offset + size ≤ range.ptr.addr + root.size by omega]

/-- Every successful layout projection can use the actual Memory access
primitive at any aligned fitting reservation, with the same block-relative
offset at every placement. -/
private theorem Backed.project_access {target c heap range root id block path off leaf}
    (backing : Backed target c heap range root id block)
    (wf : root.WellFormed target) (found : root.project path = some (off, leaf)) :
    heap.access target (atOffset range.ptr off) leaf.size leaf.align c =
      some (if leaf.size = 0 then none else some (id, off)) := by
  obtain ⟨aligned, bounds⟩ := Layout.project_aligned wf found backing.aligned
  exact backing.access bounds aligned

variable {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty} {profile : Profile}

private abbrev Ranges := AList PlaceId Hybrid.Range

/-- A physical checkpoint has a valid backing for every materialized handle.
Virtual and zero-sized locals require no entry. -/
private def BackingsWF (memory : Memory lang profile) (state : MemoryRegisters language profile)
    (ceiling : Nat := profile.memoryTarget.addressCeiling) : Prop :=
  ∀ key range, AList.lookup state.materialized key = some range →
    ∃ layout id block, memory.layoutAt state.frame key = some layout ∧
      Backed profile.memoryTarget ceiling state.bytes range layout id block

private structure Matched (memory : Memory lang profile) (fn : FnId) (key : PlaceId)
    (c d : Nat) (m n : Mem.Memory) (r s : Hybrid.Range) : Prop where
  backing : ∃ layout id x y, memory.layoutAt fn key = some layout ∧ layout.WellFormed profile.memoryTarget ∧
    Backed profile.memoryTarget c m r layout id x ∧ Backed profile.memoryTarget d n s layout id y ∧
    x.bytes = y.bytes

private structure Rel (memory : Memory lang profile) (fn : FnId) (c d : Nat)
    (rs ss : Ranges) (m n : Mem.Memory) : Prop where
  next : m.next = n.next
  roots : ∀ key, Option.Rel (Matched memory fn key c d m n)
    (AList.lookup rs key) (AList.lookup ss key)

private def reback (q : MemoryRegisters language profile) (ss : Ranges) (n : Mem.Memory) :
    MemoryRegisters language profile := {q with materialized := ss, bytes := n}

private theorem layout_checked {memory : Memory lang profile} (wf : memory.LayoutsWF)
    {fn key l} (found : memory.layoutAt fn key = some l) : l.WellFormed profile.memoryTarget := by
  obtain ⟨table, t, k⟩ := Option.bind_eq_some_iff.mp found
  exact wf _ (AList.lookup_mem t) _ (AList.lookup_mem k)

private theorem Backed.mono {target c d m r l id x} (h : Backed target c m r l id x) (le : c ≤ d) :
    Backed target d m r l id x := {h with fits := Nat.lt_of_lt_of_le h.fits le}

private theorem Rel.refl {memory : Memory lang profile} (q : MemoryRegisters language profile)
    {c d} (wf : memory.LayoutsWF) (hc : memory.BackingsWF q c) (hd : memory.BackingsWF q d) :
    Rel memory q.frame c d q.materialized q.materialized q.bytes q.bytes := by
  refine ⟨rfl, fun key => ?_⟩
  cases found : AList.lookup q.materialized key with
  | none => exact .none
  | some r =>
    obtain ⟨l, i, x, eq, a⟩ := hc _ _ found
    obtain ⟨k, j, y, eq', b⟩ := hd _ _ found
    cases Option.some.inj (eq.symm.trans eq')
    cases Option.some.inj (a.prov.symm.trans b.prov)
    cases Option.some.inj (a.lookup.symm.trans b.lookup)
    exact .some ⟨_, _, _, _, eq, layout_checked wf eq, a, b, rfl⟩

private def splice (off : Nat) (bs : List Mem.MByte) (b : Mem.Block) : Mem.Block :=
  {b with bytes := b.bytes.take off ++ bs ++ b.bytes.drop (off + bs.length)}

private theorem Backed.splice {t c m r l id x} (h : Backed t c m r l id x)
    (key : BlockId) (off : Nat) (bs : List Mem.MByte) :
    Backed t c (m.modify key (splice off bs)) r l id (if id = key then splice off bs x else x) := by
  split
  · rename_i eq
    subst key
    exact {h with lookup := by simp [Mem.block_modify, h.lookup]}
  · rename_i ne
    exact {h with lookup := by simp [Mem.block_modify, ne, h.lookup]}

private theorem Rel.splice {memory : Memory lang profile} {fn c d rs ss m n}
    (h : Rel memory fn c d rs ss m n) (key : BlockId) (off : Nat) (bs : List Mem.MByte) :
    Rel memory fn c d rs ss (m.modify key (splice off bs)) (n.modify key (splice off bs)) := by
  refine ⟨h.next, fun root => ?_⟩
  generalize ea : AList.lookup rs root = a
  generalize eb : AList.lookup ss root = b
  have related := h.roots root
  rw [ea, eb] at related
  cases related with
  | none => exact .none
  | some pair =>
    obtain ⟨l, id, x, y, found, wf, a, b, eq⟩ := pair.backing
    refine .some ⟨_, _, _, _, found, wf, a.splice key off bs, b.splice key off bs, ?_⟩
    by_cases same : id = key <;> simp [same, Memory.splice, eq]

private structure Projection (memory : Memory lang profile) (fn : FnId) (rs ss : Ranges)
    {ty : language.operands.Ty} (p : Place _ ty) (a b : Hybrid.Range) : Prop where
  projected : ∃ r s l off leaf, AList.lookup rs p.root = some r ∧ AList.lookup ss p.root = some s ∧
    memory.layoutAt fn p.root = some l ∧ l.project p.path = some (off, leaf) ∧
    a = fieldRange r off leaf ∧ b = fieldRange s off leaf

private theorem rangeAt_project {memory : Memory lang profile} {q : MemoryRegisters language profile}
    {ty} (p : Place _ ty) {r l} (root : AList.lookup q.materialized p.root = some r)
    (layout : memory.layoutAt q.frame p.root = some l) (wf : l.WellFormed profile.memoryTarget)
    (extent : r.extent = l.size) (align : r.align = l.align) :
    memory.rangeAt q p = (l.project p.path).map (fun (off, leaf) => fieldRange r off leaf) := by
  obtain ⟨key, path⟩ := p
  cases path with
  | nil =>
    simp only [rangeAt, root, List.isEmpty_nil, ite_true, Layout.project, Option.map_some]
    cases r; simp_all [fieldRange, atOffset]
  | cons k tail =>
    simp only [rangeAt, root, List.isEmpty_cons, Bool.false_eq_true, ite_false, layout]
    cases found : l.project (k :: tail) with
    | none => simp [found]
    | some pair =>
      obtain ⟨off, leaf⟩ := pair
      have bounds := (Layout.project_aligned (base := 0) wf found (Nat.dvd_zero _)).2
      simp [found, extent, bounds, fieldRange, atOffset]

private theorem Rel.rangeAt {memory : Memory lang profile} {q : MemoryRegisters language profile}
    {ss n c d} (h : Rel memory q.frame c d q.materialized ss q.bytes n) {ty} (p : Place _ ty) :
    Option.Rel (Projection memory q.frame q.materialized ss p)
      (memory.rangeAt q p) (memory.rangeAt (reback q ss n) p) := by
  have related := h.roots p.root
  cases ra : AList.lookup q.materialized p.root <;> rw [ra] at related <;>
    cases rb : AList.lookup ss p.root <;> rw [rb] at related
  · simp [Memory.rangeAt, ra, reback, rb]
  · cases related
  · cases related
  · obtain ⟨l, id, x, y, shape, wf, a, b, _⟩ := (Option.rel_some_some.mp related).backing
    rw [rangeAt_project p ra shape wf a.extent a.alignment,
      rangeAt_project (q := reback q ss n) p rb shape wf b.extent b.alignment]
    cases found : l.project p.path with
    | none => exact .none
    | some pair =>
      obtain ⟨off, leaf⟩ := pair
      exact .some ⟨_, _, _, _, _, ra, rb, shape, found, rfl, rfl⟩

private theorem Projection.accessPair {memory : Memory lang profile} {fn rs ss c d m n}
    {ty} {p : Place language.operands.Ty ty} {a b : Hybrid.Range}
    (w : Projection memory fn rs ss p a b) (h : Rel memory fn c d rs ss m n)
    {k len align : Nat} (bounds : k + len ≤ a.extent)
    (alignment : (k = 0 ∧ align = a.align) ∨ align = 1) :
    ∃ id off x y, m.block id = some x ∧ n.block id = some y ∧ x.bytes = y.bytes ∧
      m.access profile.memoryTarget (atOffset a.ptr k) len align c =
        some (if len = 0 then none else some (id, off)) ∧
      n.access profile.memoryTarget (atOffset b.ptr k) len align d =
        some (if len = 0 then none else some (id, off)) := by
  obtain ⟨r, s, l, off, leaf, ra, rb, layout, found, rfl, rfl⟩ := w.projected
  have pair := h.roots p.root
  rw [ra, rb] at pair
  obtain ⟨_, id, x, y, shape, wf, a, b, eq⟩ := (Option.rel_some_some.mp pair).backing
  cases Option.some.inj (shape.symm.trans layout)
  have left := Layout.project_aligned wf found a.aligned
  have right := Layout.project_aligned wf found b.aligned
  change k + len ≤ leaf.size at bounds
  have hi : (off + k) + len ≤ l.size := by omega
  have aligned (base : Nat) (div : leaf.align ∣ base + off) : align ∣ base + (off + k) := by
    rcases alignment with ⟨rfl, rfl⟩ | rfl
    · exact div
    · exact Nat.one_dvd _
  refine ⟨id, off + k, x, y, a.lookup, b.lookup, eq, ?_, ?_⟩
  · simpa only [fieldRange, atOffset, Nat.add_assoc] using a.access hi (aligned _ left.1)
  · simpa only [fieldRange, atOffset, Nat.add_assoc] using b.access hi (aligned _ right.1)

private theorem Projection.load {memory : Memory lang profile} {fn rs ss c d m n}
    {ty} {p : Place language.operands.Ty ty} {a b : Hybrid.Range}
    (w : Projection memory fn rs ss p a b) (h : Rel memory fn c d rs ss m n)
    {k len align : Nat} (bounds : k + len ≤ a.extent)
    (alignment : (k = 0 ∧ align = a.align) ∨ align = 1) :
    m.load profile.memoryTarget (atOffset a.ptr k) len align c =
      n.load profile.memoryTarget (atOffset b.ptr k) len align d := by
  obtain ⟨_, _, _, _, ha, hb, eq, left, right⟩ := w.accessPair h bounds alignment
  simp only [Mem.Memory.load, left, right]
  split <;> simp_all

private theorem Projection.geometry {memory : Memory lang profile} {fn rs ss ty}
    {p : Place language.operands.Ty ty} {a b} (w : Projection memory fn rs ss p a b) :
    a.extent = b.extent ∧ a.align = b.align := by
  obtain ⟨_, _, _, _, _, _, _, _, _, rfl, rfl⟩ := w.projected
  exact ⟨rfl, rfl⟩

private theorem Rel.loadAt {memory : Memory lang profile} {q : MemoryRegisters language profile}
    {ss n c d} (h : Rel memory q.frame c d q.materialized ss q.bytes n) {ty} (p : Place _ ty) :
    ((memory.rangeAt q p).bind fun a => q.bytes.load profile.memoryTarget a.ptr a.extent a.align c) =
    ((memory.rangeAt (reback q ss n) p).bind fun b => n.load profile.memoryTarget b.ptr b.extent b.align d) := by
  have v := h.rangeAt p
  generalize ea : memory.rangeAt q p = a at v ⊢
  generalize eb : memory.rangeAt (reback q ss n) p = b at v ⊢
  cases v with
  | none => rfl
  | @some a b w =>
    have eq := w.load h (k := 0) (len := a.extent) (align := a.align) (by simp) (Or.inl ⟨rfl, rfl⟩)
    simpa [atOffset, ← w.geometry.1, ← w.geometry.2] using eq

/-- A reservation only observes the live block geometry. -/
private def occupied (m : Mem.Memory) : List Mem.Block := List.map (fun e => {e.2 with bytes := []}) m.blocks

private theorem occupied_splice (m : Mem.Memory) (key off bs) :
    occupied (m.modify key (splice off bs)) = occupied m := by
  unfold occupied Mem.Memory.modify
  induction m.blocks with
  | nil => rfl
  | cons e tail ih =>
    obtain ⟨id, x⟩ := e
    simp only [AList.update]
    split
    · rfl
    · exact congrArg (_ :: ·) ih

private theorem fits_congr {target c m n candidate} (eq : occupied m = occupied n) :
    Mem.Memory.Fits target m candidate c ↔ n.Fits target candidate c := by
  have disjoint (heap : Mem.Memory) :
      (∀ e ∈ heap.blocks, e.2.live = true → ¬ e.2.Overlaps candidate) ↔
      ∀ b ∈ occupied heap, b.live = true → ¬ b.Overlaps candidate := by
    constructor
    · rintro h _ member
      obtain ⟨e, em, rfl⟩ := List.mem_map.mp member
      exact h e em
    · intro h e em
      exact h {e.2 with bytes := []} (List.mem_map_of_mem em)
  simp only [Mem.Memory.Fits, disjoint, eq]

private theorem fit_congr {target c ls m n} (eq : occupied m = occupied n) :
    ReservationsFit target c ls m → ReservationsFit target c ls n := by
  induction ls generalizing m n with
  | nil => exact id
  | cons l ls ih =>
    rintro ⟨space, base, id, reserved, placed, rest⟩
    simp only [Mem.Memory.alloc] at placed
    split at placed
    · rename_i fits
      cases placed
      refine ⟨space, base, ⟨n.next⟩, _, ite_eq_left ((fits_congr eq).mp fits), ih ?_ rest⟩
      exact congrArg (_ :: ·) eq
    · contradiction

private theorem Projection.storeRuns {memory : Memory lang profile} {fn rs ss c d m n}
    {ty} {p : Place language.operands.Ty ty} {a b : Hybrid.Range}
    (w : Projection memory fn rs ss p a b) (h : Rel memory fn c d rs ss m n)
    (runs : List (Nat × List Mem.MByte)) {m'} (put : storeRuns profile.memoryTarget c a runs m = some m') :
    ∃ n', storeRuns profile.memoryTarget d b runs n = some n' ∧ Rel memory fn c d rs ss m' n' ∧
      occupied n' = occupied n ∧ occupied m' = occupied m := by
  induction runs generalizing m n with
  | nil => cases put; exact ⟨_, rfl, h, rfl, rfl⟩
  | cons run runs ih =>
    obtain ⟨k, bs⟩ := run
    simp only [Memory.storeRuns, List.foldlM_cons, Option.bind_eq_bind, Option.bind_eq_some_iff] at put ⊢
    obtain ⟨mid, first, tail⟩ := put
    split at first
    · rename_i bounds
      have bounds' := w.geometry.1 ▸ bounds
      obtain ⟨id, off, x, y, _, _, _, left, right⟩ := w.accessPair h bounds (Or.inr rfl)
      change m.store profile.memoryTarget (atOffset a.ptr k) bs 1 c = some mid at first
      simp only [Mem.Memory.store, left] at first
      by_cases empty : bs.length = 0
      · simp only [empty, ite_true, Option.some.injEq] at first
        subst mid
        obtain ⟨n', runTail, rel, on', om'⟩ := ih h tail
        refine ⟨n', ⟨n, ?_, runTail⟩, rel, on', om'⟩
        rw [ite_eq_left bounds']
        change n.store _ (atOffset b.ptr k) bs 1 d = some n
        simp only [Mem.Memory.store, right, ite_eq_left empty]
      · simp only [ite_eq_right empty, Option.some.injEq] at first
        subst mid
        obtain ⟨n', runTail, rel, on', om'⟩ := ih (h.splice id off bs) tail
        refine ⟨n', ⟨_, ?_, runTail⟩, rel, on'.trans (occupied_splice ..), om'.trans (occupied_splice ..)⟩
        rw [ite_eq_left bounds']
        change n.store _ (atOffset b.ptr k) bs 1 d = some _
        simp only [Mem.Memory.store, right, ite_eq_right empty]
        rfl
    · contradiction

private theorem Rel.loadBacking {memory : Memory lang profile} {q : MemoryRegisters language profile}
    {ss n c d} (h : Rel memory q.frame c d q.materialized ss q.bytes n) {ty} (p : MemoryPlace _ ty) :
    memory.loadBacking q p c = memory.loadBacking (reback q ss n) p d := by
  unfold Memory.loadBacking
  rw [show memory.validPlace (reback q ss n) p = memory.validPlace q p by rfl]
  rw [show (reback q ss n).live = q.live by rfl]
  split <;> first | rfl | skip
  split <;> first | rfl | skip
  cases p.storage <;> first | rfl | skip
  change (if memory.zeroRoot q.frame p.logical.root then _ else _) = (if memory.zeroRoot q.frame p.logical.root then _ else _)
  split <;> first | rfl | skip
  unfold Memory.loadMaterialized
  rw [h.loadAt p.logical]
  rfl

private theorem Rel.permits {memory : Memory lang profile} {q : MemoryRegisters language profile}
    {ss n c d} (h : Rel memory q.frame c d q.materialized ss q.bytes n) {ty} (p : MemoryPlace _ ty) :
    memory.mayInitialize q p c = memory.mayInitialize (reback q ss n) p d ∧
    memory.authorizeWrite q p c = memory.authorizeWrite (reback q ss n) p d := by
  have discard (copy : Bool) : memory.discardRemainder q p copy c =
      memory.discardRemainder (reback q ss n) p copy d := by
    unfold Memory.discardRemainder
    rw [h.loadBacking p]
    rfl
  constructor
  · unfold mayInitialize
    rw [discard]
    rfl
  · unfold authorizeWrite writeOwnerView
    rw [discard, discard]
    rfl

private theorem Rel.storesValue {memory : Memory lang profile} {q : MemoryRegisters language profile}
    {ss n c d} (h : Rel memory q.frame c d q.materialized ss q.bytes n) {ty} (p : MemoryPlace _ ty) {v}
    (stored : memory.StoresValue q p v c) : memory.StoresValue (reback q ss n) p v d := by
  refine ⟨?_, fun mat => ?_⟩
  · have val := stored.1
    unfold loadValue at val ⊢
    rw [← h.loadBacking p]
    exact val
  · rcases stored.2 mat with zero | ⟨a, runs, found, enc, nonempty, loaded⟩
    · exact .inl zero
    have v := h.rangeAt p.logical
    rw [found] at v
    generalize eb : memory.rangeAt (reback q ss n) p.logical = dst at v
    cases v with
    | @some _ b w =>
      refine .inr ⟨b, runs, rfl, enc, w.geometry.1 ▸ nonempty, ?_⟩
      intro k bs member
      obtain ⟨bounds, init, left⟩ := loaded k bs member
      exact ⟨w.geometry.1 ▸ bounds, init, (w.load h bounds (Or.inr rfl)).symm.trans left⟩


private structure WriteFrame (q stored : MemoryRegisters language profile) : Prop where
  frame : stored.frame = q.frame
  live : stored.live = q.live
  ranges : stored.materialized = q.materialized
  effects : stored.effects = q.effects
  flag : stored.resourceExhausted = q.resourceExhausted

private theorem Rel.writeValue {memory : Memory lang profile} {q stored : MemoryRegisters language profile}
    {ss n c d} (h : Rel memory q.frame c d q.materialized ss q.bytes n) {ty} {p : MemoryPlace _ ty} {v}
    (put : memory.writeValue q p v c = some stored) :
    ∃ n', memory.writeValue (reback q ss n) p v d = some (reback stored ss n') ∧
      Rel memory stored.frame c d stored.materialized ss stored.bytes n' ∧
      WriteFrame q stored ∧ occupied n' = occupied n ∧ occupied stored.bytes = occupied q.bytes := by
  obtain ⟨init, auth⟩ := h.permits p
  simp only [Memory.writeValue, ← init, ← auth, show (reback q ss n).live = q.live by frame_auto] at put ⊢
  crush_ite at put
  · rename_i ok _ _
    rw [ite_eq_right ok]
    crush_option at put
    subst stored
    exact ⟨n, Option.bind_eq_some_iff.mpr ⟨_, ‹_›, Option.bind_eq_some_iff.mpr ⟨_, ‹_›, rfl⟩⟩,
      h, by frame_auto, rfl, rfl⟩
  · rename_i ok _ _
    rw [ite_eq_right ok]
    obtain ⟨owners, permitted, put⟩ := Option.bind_eq_some_iff.mp put
    simp only [permitted, Option.bind_eq_bind, Option.bind_some]
    let changed := markWritten q p owners
    have same : markWritten (reback q ss n) p owners = reback changed ss n := rfl
    rw [same]
    crush_ite at put
    · rename_i zero
      rw [ite_eq_left (by exact zero)]
      crush_option at put
      subst stored
      exact ⟨n, Option.bind_eq_some_iff.mpr ⟨_, ‹_›, rfl⟩, h, by frame_auto, rfl, rfl⟩
    · rename_i nz
      rw [ite_eq_right (by exact nz)]
      crush_option at put
      subst stored
      have h' : Rel memory changed.frame c d changed.materialized ss changed.bytes n := h
      have ranges := h'.rangeAt p.logical
      rw [show memory.rangeAt changed p.logical = some _ from ‹_›] at ranges
      generalize hb : memory.rangeAt (reback changed ss n) p.logical = rb at ranges
      cases ranges with
      | @some _ b w =>
        obtain ⟨n', tail, agree, on', om'⟩ := w.storeRuns h' _ ‹_›
        refine ⟨n', ?_, agree, by frame_auto, on', om'⟩
        exact Option.bind_eq_some_iff.mpr ⟨b, rfl, Option.bind_eq_some_iff.mpr
          ⟨_, ‹_›, Option.bind_eq_some_iff.mpr ⟨n', tail, rfl⟩⟩⟩

private theorem Rel.writes {memory : Memory lang profile} {q stored : MemoryRegisters language profile}
    {ss n c d} (h : Rel memory q.frame c d q.materialized ss q.bytes n) {ty} {p : MemoryPlace _ ty} {v}
    (write : memory.Writes q p v stored c) :
    ∃ n', memory.Writes (reback q ss n) p v (reback stored ss n') d ∧
      Rel memory stored.frame c d stored.materialized ss stored.bytes n' ∧
      WriteFrame q stored ∧ occupied n' = occupied n ∧ occupied stored.bytes = occupied q.bytes := by
  obtain ⟨n', put, agree, frame, after, before⟩ := h.writeValue write.2.2.1
  refine ⟨n', ⟨write.1, ?_, put, agree.storesValue p write.2.2.2⟩, agree, frame, after, before⟩
  intro mat
  obtain ⟨l, shape, size⟩ := write.2.1 mat
  refine ⟨l, shape, ?_⟩
  have pair := h.roots p.logical.root
  generalize ea : AList.lookup q.materialized p.logical.root = a at size pair
  change l.size = 0 ↔ (AList.lookup ss p.logical.root).isNone
  generalize eb : AList.lookup ss p.logical.root = b at pair ⊢
  cases pair <;> exact size

private theorem TransferCheckpoint.backings {memory : Memory lang profile} {q : MemoryRegisters language profile}
    {c d} (ck : memory.TransferCheckpoint q c) (le : min profile.memoryTarget.addressCeiling c ≤ d) :
    memory.BackingsWF q d := by
  intro key r found
  have resident := ck.1 _ (AList.lookup_mem found)
  change memory.resident q key r = true at resident
  unfold Memory.resident at resident
  cases shape : memory.layoutAt q.frame key <;> simp only [shape, Option.bind_eq_bind, Option.bind_none,
    Option.getD_none, Bool.false_eq_true, Option.bind_some] at resident
  cases prov : r.ptr.prov <;> simp only [prov, Option.bind_none, Option.getD_none,
    Bool.false_eq_true, Option.bind_some] at resident
  rename_i id
  cases block : q.bytes.block id <;> simp only [block, Option.bind_none, Option.getD_none,
    Bool.false_eq_true, Option.bind_some, Option.pure_def, Option.getD_some, decide_eq_true_eq] at resident
  obtain ⟨covers, extent, alignment, aligned, lo, valid, fresh⟩ := resident
  have fits := ck.2 _ (AList.lookup_mem block) covers.1
  refine ⟨_, _, _, rfl, block, prov, covers, extent, alignment, aligned, lo, ?_, valid, fresh⟩
  simpa only [covers.2.2.1, covers.2.2.2.1, extent] using Nat.lt_of_lt_of_le fits le

theorem TransferCheckpoint.atCeiling {memory : Memory lang profile} {q : MemoryRegisters language profile}
    {c d} (ck : memory.TransferCheckpoint q c) (le : min profile.memoryTarget.addressCeiling c ≤ d) :
    memory.TransferCheckpoint q d :=
  ⟨ck.1, fun e mem live => Nat.lt_of_lt_of_le (ck.2 e mem live) (Nat.le_min.mpr ⟨Nat.min_le_left .., le⟩)⟩

private def allocated (m : Mem.Memory) (space : AddrSpace) (base : Nat) (l : Layout) : Mem.Memory :=
  ⟨(⟨m.next⟩, Mem.candidate space base l.size l.align) :: m.blocks, m.next + 1⟩

private def reservation (m : Mem.Memory) (space : AddrSpace) (base : Nat) (l : Layout) : Hybrid.Range :=
  ⟨⟨space, base, some ⟨m.next⟩⟩, l.size, l.align⟩

private theorem alloc_inv {target c m space base l id m'}
    (ok : Mem.Memory.alloc target m space base l.size l.align c = .ok (id, m')) :
    id = ⟨m.next⟩ ∧ m' = allocated m space base l ∧ m.Fits target (Mem.candidate space base l.size l.align) c := by
  simp only [Mem.Memory.alloc] at ok
  split at ok
  · cases ok; exact ⟨rfl, rfl, by assumption⟩
  · contradiction

private theorem Backed.extend {target c m r root id x} (a : Backed target c m r root id x)
    (space base l) : Backed target c (allocated m space base l) r root id x := by
  have fresh := a.fresh
  have ne : (⟨m.next⟩ : BlockId) ≠ id := by intro eq; cases eq; exact (Nat.lt_irrefl _ fresh)
  exact {a with lookup := by simpa [allocated, Mem.Memory.block, AList.lookup, ne] using a.lookup,
                fresh := Nat.lt_succ_of_lt fresh}

private theorem backed_new {c m space base l}
    (ok : m.Fits profile.memoryTarget (Mem.candidate space base l.size l.align) c) (positive : 0 < l.size) :
    Backed profile.memoryTarget c (allocated m space base l) (reservation m space base l) l
      ⟨m.next⟩ (Mem.candidate space base l.size l.align) := by
  have lo := ok.2.2.2.2.1 positive
  have null : profile.memoryTarget.null = 0 := by unfold Profile.memoryTarget; split <;> rfl
  exact ⟨by simp [allocated, Mem.Memory.block, AList.lookup], rfl, ⟨rfl, rfl, rfl, rfl, rfl⟩, rfl, rfl,
    Nat.dvd_of_mod_eq_zero ok.2.1, by change profile.memoryTarget.null < base; change base ≠ _ at lo; omega,
    ok.2.2.1, ok.2.2.2.2.2.2, Nat.lt_succ_self _⟩

/-- In actual allocated memory, every declared field path has a valid access
at its block-relative offset. Zero-sized fields use the one-past rule. -/
theorem allocated_project_access {heap next : Mem.Memory} {root leaf : Layout} {space base id c path off}
    (wf : root.WellFormed profile.memoryTarget) (positive : 0 < root.size)
    (allocatedRoot : heap.alloc profile.memoryTarget space base root.size root.align c = .ok (id, next))
    (found : root.project path = some (off, leaf)) :
    next.access profile.memoryTarget ⟨space, base + off, some id⟩ leaf.size leaf.align c =
      some (if leaf.size = 0 then none else some (id, off)) := by
  obtain ⟨rfl, rfl, fits⟩ := alloc_inv (l := root) allocatedRoot
  exact (backed_new fits positive).project_access wf found

private theorem Rel.extend {memory : Memory lang profile} {fn c d rs ss m n}
    (h : Rel memory fn c d rs ss m n) {root l} (shape : memory.layoutAt fn root = some l)
    (wf : l.WellFormed profile.memoryTarget) (pos : 0 < l.size) {sp base sp' base'}
    (left : m.Fits profile.memoryTarget (Mem.candidate sp base l.size l.align) c)
    (right : n.Fits profile.memoryTarget (Mem.candidate sp' base' l.size l.align) d) :
    Rel memory fn c d ((root, reservation m sp base l) :: rs) ((root, reservation n sp' base' l) :: ss)
      (allocated m sp base l) (allocated n sp' base' l) := by
  refine ⟨congrArg (· + 1) h.next, fun key => ?_⟩
  simp only [AList.lookup]
  split
  · rename_i eq
    subst key
    have a := backed_new left pos
    have b := backed_new right pos
    rw [← h.next] at b
    exact .some ⟨_, _, _, _, shape, wf, a, b, rfl⟩
  · have related := h.roots key
    generalize ea : AList.lookup rs key = a at related ⊢
    generalize eb : AList.lookup ss key = b at related ⊢
    cases related with
    | none => exact .none
    | some pair =>
      obtain ⟨k, id, x, y, found, checked, a, b, eq⟩ := pair.backing
      exact .some ⟨_, _, _, _, found, checked, a.extend .., b.extend .., eq⟩

/-- Either replay the same choices under a larger ceiling or consume a
preselected joint placement. The latter may change both space and base. -/
private def Available (t : Target) (c d : Nat) (m n : Mem.Memory) (ls : List Layout) :=
  (c ≤ d ∧ occupied m = occupied n) ∨ ReservationsFit t d ls n

private theorem Available.write {t c d ls m n m' n'} (avail : Available t c d m n ls)
    (left : occupied m' = occupied m) (right : occupied n' = occupied n) : Available t c d m' n' ls := by
  rcases avail with ⟨le, eq⟩ | plan
  · exact .inl ⟨le, left.trans (eq.trans right.symm)⟩
  · exact .inr (fit_congr right.symm plan)

private theorem Available.reserve {t c d m n l ls sp base}
    (avail : Available t c d m n (l :: ls)) (fit : m.Fits t (Mem.candidate sp base l.size l.align) c) :
    ∃ sp' base', n.Fits t (Mem.candidate sp' base' l.size l.align) d ∧
      Available t c d (allocated m sp base l) (allocated n sp' base' l) ls := by
  rcases avail with ⟨le, eq⟩ | ⟨space, atBase, _, _, placed, tail⟩
  · obtain ⟨pow, aligned, hi, maxSize, nz, disjoint, valid⟩ := (fits_congr eq).mp fit
    exact ⟨sp, base, ⟨pow, aligned, Nat.lt_of_lt_of_le hi le, maxSize, nz, disjoint, valid⟩,
      .inl ⟨le, congrArg (_ :: ·) eq⟩⟩
  · obtain ⟨_, rfl, fits⟩ := alloc_inv (l := l) placed
    exact ⟨space, atBase, fits, .inr tail⟩

private theorem replayBegin {memory : Memory lang profile} (wf : memory.LayoutsWF)
    {q started : MemoryRegisters language profile} {c d ss n root tail}
    (h : Rel memory q.frame c d q.materialized ss q.bytes n)
    (opens : if q.live.contains root then started = q else memory.Begins q root started c)
    (avail : Available profile.memoryTarget c d q.bytes n (memory.freshLayoutsFrom q.frame q.live (root :: tail))) :
    ∃ ss' n', (if q.live.contains root then reback started ss' n' = reback q ss n
          else memory.Begins (reback q ss n) root (reback started ss' n') d) ∧
      Rel memory started.frame c d started.materialized ss' started.bytes n' ∧
      Available profile.memoryTarget c d started.bytes n' (memory.freshLayoutsFrom started.frame started.live tail) := by
  split at opens
  · rename_i live
    subst started
    simp only [ite_eq_left live]
    simp only [freshLayoutsFrom, ite_eq_left (List.contains_iff_mem.mp live)] at avail
    exact ⟨ss, n, rfl, h, avail⟩
  · rename_i fresh
    simp only [ite_eq_right fresh]
    obtain ⟨absent, virtual | materialized⟩ := opens
    all_goals simp only [freshLayoutsFrom, ite_eq_right absent] at avail
    · obtain ⟨cls, rfl⟩ := virtual
      simp only [cls] at avail
      exact ⟨ss, n, ⟨absent, .inl ⟨cls, rfl⟩⟩, h, avail⟩
    · obtain ⟨cls, l, shape, zero | placed⟩ := materialized
      · obtain ⟨z, rfl⟩ := zero
        simp only [cls, ite_true, shape, z] at avail
        exact ⟨ss, n, ⟨absent, .inr ⟨cls, l, shape, .inl ⟨z, rfl⟩⟩⟩, h, avail⟩
      · obtain ⟨pos, sp, base, _, _, placed, rfl⟩ := placed
        obtain ⟨rfl, rfl, fits⟩ := alloc_inv (l := l) placed
        simp only [cls, ite_true, shape, ite_eq_right (Nat.ne_of_gt pos)] at avail
        obtain ⟨sp', base', fits', rest⟩ := avail.reserve fits
        refine ⟨(root, reservation n sp' base' l) :: ss, allocated n sp' base' l, ?_,
          h.extend shape (layout_checked wf shape) pos fits fits', rest⟩
        exact ⟨absent, .inr ⟨cls, l, shape, .inr ⟨pos, sp', base', ⟨n.next⟩, _, ite_eq_left fits', rfl⟩⟩⟩

private theorem begin_reservation {memory : Memory lang profile} {q started : MemoryRegisters language profile}
    {c root tail} (opens : if q.live.contains root then started = q else memory.Begins q root started c)
    (rest : ReservationsFit profile.memoryTarget c (memory.freshLayoutsFrom started.frame started.live tail) started.bytes) :
    ReservationsFit profile.memoryTarget c (memory.freshLayoutsFrom q.frame q.live (root :: tail)) q.bytes := by
  split at opens
  · rename_i live
    subst started
    simpa only [freshLayoutsFrom, ite_eq_left (List.contains_iff_mem.mp live)] using rest
  · obtain ⟨absent, virtual | materialized⟩ := opens
    all_goals simp only [freshLayoutsFrom, ite_eq_right absent]
    · obtain ⟨cls, rfl⟩ := virtual
      simpa [cls] using rest
    · obtain ⟨cls, l, shape, zero | placed⟩ := materialized
      · obtain ⟨z, rfl⟩ := zero
        simpa only [cls, ite_true, shape, z] using rest
      · obtain ⟨pos, sp, base, id, heap, alloc, rfl⟩ := placed
        simp only [cls, ite_true, shape, ite_eq_right (Nat.ne_of_gt pos)]
        exact ⟨sp, base, id, heap, alloc, rest⟩

private theorem replay {memory : Memory lang profile} (wf : memory.LayoutsWF)
    {ps vs q out ex c} (path : memory.WriteParams (language := language) ps vs q out ex c) (normal : ex = false)
    {d ss n} (h : Rel memory q.frame c d q.materialized ss q.bytes n)
    (avail : Available profile.memoryTarget c d q.bytes n (memory.freshLayouts q (ps.map fun p => p.2.logical.root))) :
    memory.ParamSpaceFits ps q c ∧ ∃ next, memory.WriteParams ps vs (reback q ss n) next false d ∧
      next.effects = q.effects ∧ next.resourceExhausted = q.resourceExhausted := by
  induction path generalizing ss n d with
  | nil => exact ⟨trivial, _, .nil _, rfl, rfl⟩
  | @cons ty p ps v vs q started stored out ex c begin write _ ih =>
    subst ex
    obtain ⟨ss', n', opens, related, space⟩ := replayBegin wf h begin avail
    obtain ⟨n'', writes, agree, frame, target, source⟩ := related.writes write
    have available := space.write source target
    rw [← frame.live, ← frame.frame] at available
    obtain ⟨fits, next, tail, effects, flag⟩ := ih rfl agree available
    have keep : started.effects = q.effects ∧ started.resourceExhausted = q.resourceExhausted := by
      split at begin
      · subst started; exact ⟨rfl, rfl⟩
      · obtain ⟨_, ⟨_, rfl⟩ | ⟨_, _, _, ⟨_, rfl⟩ | ⟨_, _, _, _, _, _, rfl⟩⟩⟩ := begin <;> exact ⟨rfl, rfl⟩
    have rest := fit_congr source fits
    unfold freshLayouts at rest
    rw [frame.frame, frame.live] at rest
    exact ⟨begin_reservation begin rest, next, .cons opens writes tail, effects.trans (frame.effects.trans keep.1),
      flag.trans (frame.flag.trans keep.2)⟩
  | exhausted => contradiction

private theorem fresh_checked {memory : Memory lang profile} (wf : memory.LayoutsWF) (fn live roots) :
    ∀ l ∈ memory.freshLayoutsFrom fn live roots, l.WellFormed profile.memoryTarget := by
  induction roots generalizing live with
  | nil => exact nofun
  | cons root rest ih =>
    simp only [freshLayoutsFrom]
    split
    · exact ih _
    split
    · split
      · rename_i l found
        split
        · exact ih _
        · intro k mem
          rcases List.mem_cons.mp mem with rfl | member
          · exact layout_checked wf found
          · exact ih _ _ member
      · exact ih _
    · exact ih _

private theorem fit_after {t : Target} {limit cursor : Nat} {m : Mem.Memory} {ls : List Layout}
    (valid : t.WellFormed) (lo : t.null < cursor)
    (clear : ∀ e ∈ m.blocks, e.2.live = true → e.2.base + e.2.size ≤ cursor)
    (checked : ∀ l ∈ ls, l.WellFormed t)
    (hi : cursor + (ls.map fun l => (l.align - 1) + l.size).sum < limit) :
    ReservationsFit t limit ls m := by
  induction ls generalizing cursor m with
  | nil => trivial
  | cons l ls ih =>
    have wf := checked l (.head _)
    have laws : Slice0Layout.StorageGeometry t l.size l.align := by
      cases l
      exact (of_decide_eq_true (Bool.and_eq_true_iff.mp wf).1).1
    have pos := laws.alignPow2.1
    let base := ((cursor + (l.align - 1)) / l.align) * l.align
    have floor : base ≤ cursor + (l.align - 1) := Nat.div_mul_le_self ..
    have start : cursor ≤ base := by
      have modlt := Nat.mod_lt (cursor + (l.align - 1)) pos
      have eq := Nat.mod_add_div (cursor + (l.align - 1)) l.align
      dsimp [base]; rw [Nat.mul_comm] at eq; omega
    simp only [List.map_cons, List.sum_cons] at hi
    have fits : m.Fits t (Mem.candidate t.dataSpace base l.size l.align) limit := by
      refine ⟨laws.alignPow2, Nat.mod_eq_zero_of_dvd (Nat.dvd_mul_left ..),
        by change base + l.size < limit; omega, laws.fits, fun _ => Nat.ne_of_gt (Nat.lt_of_lt_of_le lo start),
        ?_, valid.2.2.2.2.2.2.2.1⟩
      intro e member live overlap
      have endAt := clear e member live
      have cross := overlap.2.2.2.2
      change base < e.2.base + e.2.size at cross
      omega
    refine ⟨t.dataSpace, base, ⟨m.next⟩, allocated m t.dataSpace base l, ite_eq_left fits, ?_⟩
    apply ih (cursor := base + l.size) (lo := by omega)
    · intro e member live
      rcases List.mem_cons.mp member with rfl | old
      · exact Nat.le_refl _
      · exact Nat.le_trans (clear e old live) (by omega)
    · exact fun k member => checked k (.tail _ member)
    · omega

theorem ParamSpaceFits.at_bound {memory : Memory lang profile} (wf : memory.LayoutsWF)
    {q : MemoryRegisters language profile} {c} (ck : memory.TransferCheckpoint q c) (ps) :
    memory.ParamSpaceFits ps q (memory.transferCeiling q (ps.map fun p => p.2.logical.root)) := by
  apply fit_after (cursor := profile.memoryTarget.addressCeiling) profile.targetWF
  · exact profile.targetWF.2.2.2.2.2.2.2.2.1
  · intro e member live
    exact Nat.le_trans (Nat.le_of_lt (ck.2 e member live)) (Nat.min_le_left ..)
  · exact fresh_checked wf _ _ _
  · unfold transferCeiling
    omega

private theorem ParamsValid.replay {memory : Memory lang profile} (wf : memory.LayoutsWF)
    {ps vs q c d} (valid : memory.ParamsValid (language := language) ps vs q c)
    (inside : min profile.memoryTarget.addressCeiling c ≤ d)
    (available : Available profile.memoryTarget c d q.bytes q.bytes (memory.freshLayouts q (ps.map fun p => p.2.logical.root))) :
    memory.ParamSpaceFits ps q c ∧ ∃ next, memory.WriteParams ps vs q next false d ∧ next.effects = q.effects ∧
      next.resourceExhausted = q.resourceExhausted := by
  obtain ⟨ck, out, path⟩ := valid
  exact Memory.replay wf path rfl (Rel.refl q wf (ck.backings (Nat.min_le_right ..)) (ck.backings inside)) available

/-- Keeping the same reservations replays the complete transfer under any larger ceiling. -/
theorem ParamsValid.mono {memory : Memory lang profile} (wf : memory.LayoutsWF)
    {ps vs q c d} (valid : memory.ParamsValid (language := language) ps vs q c) (le : c ≤ d) :
    memory.ParamsValid ps vs q d := by
  have inside := Nat.le_trans (Nat.min_le_right profile.memoryTarget.addressCeiling c) le
  obtain ⟨next, path, _⟩ := (valid.replay wf inside (.inl ⟨le, rfl⟩)).2
  exact ⟨valid.1.atCeiling inside, next, path⟩

/-- Any successful transfer, even one using arbitrarily high addresses, can
place its fresh roots at the computed ceiling and replay every write. -/
theorem ParamsValid.at_bound {memory : Memory lang profile} (wf : memory.LayoutsWF)
    {ps vs q c} (valid : memory.ParamsValid (language := language) ps vs q c) :
    memory.ParamsValid ps vs q := by
  have fits := ParamSpaceFits.at_bound wf valid.1 ps
  have inside : min profile.memoryTarget.addressCeiling c ≤ memory.transferCeiling q (ps.map fun p => p.2.logical.root) :=
    Nat.le_trans (Nat.min_le_left ..) (by unfold transferCeiling; omega)
  obtain ⟨next, path, _⟩ := (valid.replay wf inside (.inr fits)).2
  exact ⟨valid.1.atCeiling inside, next, path⟩

/-- Replay at an actual joint placement, retaining effects and the running flag. -/
theorem ParamsValid.physical {memory : Memory lang profile} (wf : memory.LayoutsWF)
    {ps vs q c} (valid : memory.ParamsValid (language := language) ps vs q c) (fits : memory.ParamSpaceFits ps q) :
    ∃ next, memory.WriteParams ps vs q next false ∧ next.effects = q.effects ∧
      next.resourceExhausted = q.resourceExhausted :=
  (valid.replay wf (Nat.min_le_left ..) (.inr fits)).2

/-- A successful write sequence witnesses the actual joint root reservations
at its ceiling. Intermediate writes preserve the occupied intervals. -/
theorem ParamsValid.spaceFits {memory : Memory lang profile} (wf : memory.LayoutsWF)
    {ps vs q c} (valid : memory.ParamsValid (language := language) ps vs q c) :
    memory.ParamSpaceFits ps q c :=
  (valid.replay wf (Nat.min_le_right ..) (.inl ⟨Nat.le_refl _, rfl⟩)).1

/-- The bounded run is a finite characterization of success with unlimited space. -/
theorem paramsValid_iff_some_ceiling {memory : Memory lang profile} (wf : memory.LayoutsWF)
    {ps vs q} : memory.ParamsValid (language := language) ps vs q ↔ ∃ c, memory.ParamsValid ps vs q c :=
  ⟨fun valid => ⟨_, valid⟩, fun ⟨_, valid⟩ => valid.at_bound wf⟩

end Concrete.Interfaces.Memory
