import Concrete.Memory.StepSimulation
import Concrete.Interfaces.Pipeline

/-!
# Production MIR-to-Memory lowering and universal stage validation

Provides the total production `lowerMemory` pass over the frozen `MIR.HandoffWF` domain
and proves `(registry passes).lowerMemory.Validated` and `Stage.Sound passes .lowerMemory`
universally across every slice and compilation profile.

1. `prepareGraph_ok_of_materializable`: `MIR.HandoffWF`'s `Materializable` guarantees
   that `prepareGraph` succeeds on every well-formed MIR input.
2. Fast path + ambiguous-join specialization: `lowerMemory` runs `lowerGraph` directly
   when all lifetime joins are unambiguous (producing the exact `lowerGraph` output),
   and specializes only roots with ambiguous joins via `specializeMIR` otherwise so
   that `lowerMemory` never refuses a `MIR.HandoffWF` input (matching the frozen
   `refusal := fun _ _ => False` contract on `(registry passes).lowerMemory`).
3. `MIRCovering` proves an exact 1-to-1 bisimulation between `mir` and `specializeMIR mir`,
   composing constructively (zero `Classical.choice`, zero `sorry`) with U03's
   `slice_lowerGraph_lowersMemory` / `slice_lowerGraph_correspondence`, F7's
   `lowerGraph_handoff` (`RetainsTypedRoots`, `Memory.HandoffWF`), and `ISS-0125`'s
   `ResidentHandles` / U02 `TransferProgress` bridges in `Concrete/Memory/Refinement.lean`.
-/

namespace Concrete.Memory.Validate

open Concrete.Memory Concrete.Interfaces ConcreteSpec.Core ConcreteSpec.StageInterface
open Concrete.Contracts (Steps Semantics TracePrefix SameInfiniteTrace)

/-! ### 1. `Materializable` guarantees `prepareGraph` succeeds -/

private theorem filterMapM_ok_of_forall {α β ε : Type} (f : α → Except ε (Option β)) (xs : List α)
    (h : ∀ x ∈ xs, ∃ opt, f x = .ok opt) : ∃ ys, xs.filterMapM f = .ok ys := by
  induction xs with
  | nil => exact ⟨[], rfl⟩
  | cons x rest ih =>
    obtain ⟨opt, hopt⟩ := h x (.head _)
    obtain ⟨ys, hys⟩ := ih (fun a ha => h a (.tail _ ha))
    cases opt with
    | none =>
      refine ⟨ys, ?_⟩
      rw [List.filterMapM_cons, hopt]
      simp only [bind, Except.bind, hys]
    | some b =>
      refine ⟨b :: ys, ?_⟩
      rw [List.filterMapM_cons, hopt]
      simp only [bind, Except.bind, pure, Except.pure, hys]

private theorem mapM_ok_of_forall {α β ε : Type} (f : α → Except ε β) (xs : List α)
    (h : ∀ x ∈ xs, ∃ y, f x = .ok y) : ∃ ys, xs.mapM f = .ok ys := by
  induction xs with
  | nil => exact ⟨[], rfl⟩
  | cons x rest ih =>
    obtain ⟨y, hy⟩ := h x (.head _)
    obtain ⟨ys, hys⟩ := ih (fun a ha => h a (.tail _ ha))
    refine ⟨y :: ys, ?_⟩
    rw [List.mapM_cons, hy, hys]
    rfl

private theorem bind_ok_of_ok {ε α β : Type} {x : Except ε α} {f : α → Except ε β}
    (hx : ∃ a, x = .ok a) (hf : ∀ a, ∃ b, f a = .ok b) : ∃ b, (x >>= f) = .ok b := by
  obtain ⟨a, rfl⟩ := hx
  exact hf a

/-- Every `Materializable` MIR program admits target layouts for all non-scalar or
address-escaping roots, so `prepareGraph` never fails with `MissingLayout`. -/
theorem prepareGraph_ok_of_materializable {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {mir : MIR lang profile}
    (hMat : Materializable (language := language) mir) :
    ∃ prepared, prepareGraph (language := language) mir = .ok prepared := by
  unfold prepareGraph
  refine bind_ok_of_ok ?_ (fun _ => ⟨_, rfl⟩)
  apply mapM_ok_of_forall
  rintro ⟨fn, locals, graph⟩ hmem
  unfold MIR.logicalView at hmem
  simp only [AList.map, List.mem_map] at hmem
  obtain ⟨⟨fn', locals', mirGraph⟩, hMirMem, hEq⟩ := hmem
  cases hEq
  refine bind_ok_of_ok ?_ (fun slots => ⟨(fn', slots), rfl⟩)
  with_unfolding_all
    apply filterMapM_ok_of_forall
    rintro ⟨root, ty⟩ hLoc
    cases hcls : storageClass (language := language) mir fn' root ty with
    | virtual => exact ⟨none, by simp only [hcls]; rfl⟩
    | materialized =>
      have hNeed : (language.scalarType ty).isNone = true ∨ mir.Escapes (language := language) fn' root := by
        unfold storageClass at hcls
        cases hsc : (language.scalarType ty) with
        | none => exact .inl rfl
        | some _ =>
          cases hesc : escapes (language := language) mir fn' root with
          | true => exact .inr ((escapes_iff mir fn' root).mp hesc)
          | false => simp [hsc, hesc] at hcls
      obtain ⟨d, hLay, hWF⟩ := hMat fn' locals' mirGraph hMirMem root ty hLoc hNeed
      refine ⟨some (root, d), ?_⟩
      change (match storageClass (language := language) mir fn' root ty with
        | .virtual => pure none
        | .materialized =>
          match language.layout mir.declarations profile.memoryTarget ty with
          | some layout =>
            if layout.WellFormed profile.memoryTarget then pure (some (root, layout))
            else throw (MissingLayout.mk fn' root ty)
          | none => throw (MissingLayout.mk fn' root ty)) =
        (Except.ok (some (root, d)) : Except (MissingLayout language.operands.Ty) (Option (PlaceId × Layout)))
      simp only [hcls, hLay, hWF, ↓reduceIte]
      rfl

/-! ### 2. Single-root CFG specialization for ambiguous lifetime joins -/

abbrev MIRBlock {Ty : Type} (lang : LoweringLanguage Ty) :=
  Block (Sigma (Place Ty)) (LocatedOp (MIRInst lang))
    (Terminator (Sigma (Read Ty lang.literal)) (Read Ty lang.literal lang.bool))

/-- Bijective encoding of `(BlockId, live : Bool)` into fresh `BlockId`s. -/
def encodeBlockId (id : BlockId) (live : Bool) : BlockId :=
  ⟨2 * id.id + if live then 1 else 0⟩

/-- Left inverse of `encodeBlockId`, projecting both copies back to the source block ID. -/
def decodeBlockId (id : BlockId) : BlockId :=
  ⟨id.id / 2⟩

theorem decodeBlockId_encode (id : BlockId) (live : Bool) :
    decodeBlockId (encodeBlockId id live) = id := by
  cases id with
  | mk n =>
    cases live <;> simp [decodeBlockId, encodeBlockId] <;> omega

theorem encodeBlockId_inj {id₁ id₂ : BlockId} {b₁ b₂ : Bool}
    (h : encodeBlockId id₁ b₁ = encodeBlockId id₂ b₂) :
    id₁ = id₂ ∧ b₁ = b₂ := by
  cases id₁ with
  | mk n₁ =>
    cases id₂ with
    | mk n₂ =>
      cases b₁ <;> cases b₂ <;> simp [encodeBlockId] at h
      · exact ⟨congrArg BlockId.mk (by omega), rfl⟩
      · omega
      · omega
      · exact ⟨congrArg BlockId.mk (by omega), rfl⟩

/-- Map outgoing terminator target IDs via `f`. -/
def mapTermTarget {Arg Cond : Type} (f : BlockId → BlockId) :
    Terminator Arg Cond → Terminator Arg Cond
  | .jump e => .jump { e with target := f e.target }
  | .branch c y n => .branch c { y with target := f y.target } { n with target := f n.target }
  | .ret v => .ret v
  | .trap k o => .trap k o
  | .invalid => .invalid

private theorem mapTermTarget_id {Arg Cond : Type} (t : Terminator Arg Cond) :
    mapTermTarget id t = t := by
  cases t <;> rfl

private theorem mapTermTarget_comp {Arg Cond : Type} (f g : BlockId → BlockId)
    (t : Terminator Arg Cond) :
    mapTermTarget (f ∘ g) t = mapTermTarget f (mapTermTarget g t) := by
  cases t <;> rfl

private theorem mapTermTarget_origins {Arg Cond : Type} (f : BlockId → BlockId)
    (t : Terminator Arg Cond) :
    (mapTermTarget f t).origins = t.origins := by
  cases t <;> rfl

private theorem mapTermTarget_successors {Arg Cond : Type} (f : BlockId → BlockId)
    (t : Terminator Arg Cond) :
    (mapTermTarget f t).successors =
      t.successors.map (fun e => { e with target := f e.target }) := by
  cases t <;> rfl

/-- MIR-level lifetime transfer of one instruction for root `r`, matching `Lifetimes.lean`. -/
def mirInstChange {Ty : Type} {lang : LoweringLanguage Ty} (inst : MIRInst lang) (r : PlaceId) : Option Bool :=
  match inst with
  | .bind dst _ | .op dst .. => if dst.root = r then some true else none
  | .endLifetime id => if id = r then some false else none
  | .call _ dst .. => if dst.any (fun p : Sigma (Place Ty) => p.2.root == r) then some true else none
  | .destructure _ bindings =>
    if bindings.any (fun b : FieldId × Sigma (Place Ty) => b.2.2.root == r) then some true else none
  | .assign .. => none

/-- Outgoing liveness of root `r` after executing MIR block `b` from incoming liveness `incoming`. -/
def mirBlockOut {Ty : Type} {lang : LoweringLanguage Ty} (b : MIRBlock lang)
    (r : PlaceId) (incoming : Bool) : Bool :=
  let live := incoming || b.params.any (fun p : Sigma (Place Ty) => p.2.root == r)
  b.body.foldl (fun fact op => (mirInstChange op.instruction r).getD fact) live

/-- One live-state copy (`incoming = false` or `true`) of MIR block `b` when splitting on `r`. -/
def splitBlock {Ty : Type} {lang : LoweringLanguage Ty} (r : PlaceId)
    (b : MIRBlock lang) (incoming : Bool) : MIRBlock lang :=
  let outLive := mirBlockOut b r incoming
  { b with
    id := encodeBlockId b.id incoming
    ending := mapTermTarget (fun tid => encodeBlockId tid outLive) b.ending }

/-- Split every block of `graph` into `(false, true)` copies tracking root `r`'s entry liveness. -/
def splitGraphForRoot {Ty : Type} {lang : LoweringLanguage Ty} (r : PlaceId)
    (graph : MIRGraph lang) : MIRGraph lang :=
  { entry := encodeBlockId graph.entry false
    blocks := (graph.blocks.toList.flatMap fun b => [splitBlock r b false, splitBlock r b true]).toArray }

/-- Iteratively split `graph` across a list of ambiguous roots. -/
def splitGraphForRoots {Ty : Type} {lang : LoweringLanguage Ty}
    (rs : List PlaceId) (graph : MIRGraph lang) : MIRGraph lang :=
  rs.foldl (fun g r => splitGraphForRoot r g) graph

private def toLogicalPlace {Ty : Type} {ty : Ty} (p : Place Ty ty) : MemoryPlace Ty ty :=
  ⟨p, .virtual⟩

private def toLogicalRead {Ty : Type} {lang : LoweringLanguage Ty} {ty : Ty} :
    Read Ty lang.literal ty → MemoryRead Ty lang.literal ty
  | .literal v => .literal v
  | .copy p => .copy (toLogicalPlace p)
  | .move p => .move (toLogicalPlace p)

private def toLogicalArgs {Ty : Type} {lang : LoweringLanguage Ty} : {types : List Ty} →
    Args (Read Ty lang.literal) types → Args (MemoryRead Ty lang.literal) types
  | _, .nil => .nil
  | _, .cons a rest => .cons (toLogicalRead a) (toLogicalArgs rest)

private def toLogicalInst {Ty : Type} {lang : LoweringLanguage Ty} :
    MIRInst lang → MemoryInst lang
  | .bind dst src => .bind (toLogicalPlace dst) (toLogicalRead src)
  | .assign dst src => .assign (toLogicalPlace dst) (toLogicalRead src)
  | .op dst op args checks addresses =>
    .op (toLogicalPlace dst) op (toLogicalArgs args) checks
      (addresses.map fun a => ⟨a.1, toLogicalPlace a.2⟩)
  | .endLifetime p => .endLifetime p
  | .destructure src bindings =>
    .destructure (toLogicalPlace src)
      (bindings.map fun (f, d) => (f, ⟨d.1, toLogicalPlace d.2⟩))
  | .call callee dst args checks =>
    .call callee (dst.map fun d => ⟨d.1, toLogicalPlace d.2⟩)
      (args.map fun a => ⟨a.1, toLogicalRead a.2⟩) checks

private def toLogicalEdge {Ty : Type} {lang : LoweringLanguage Ty}
    (e : CFGEdge (Sigma (Read Ty lang.literal))) :
    CFGEdge (Sigma (MemoryRead Ty lang.literal)) :=
  { e with arguments := e.arguments.map fun a => ⟨a.1, toLogicalRead a.2⟩ }

private def toLogicalTerm {Ty : Type} {lang : LoweringLanguage Ty} :
    Terminator (Sigma (Read Ty lang.literal)) (Read Ty lang.literal lang.bool) → MemoryTerm lang
  | .jump e => .jump (toLogicalEdge e)
  | .branch c y n => .branch (toLogicalRead c) (toLogicalEdge y) (toLogicalEdge n)
  | .ret v => .ret (v.map fun a => ⟨a.1, toLogicalRead a.2⟩)
  | .trap k o => .trap k o
  | .invalid => .invalid

private def toLogicalBlock {Ty : Type} {lang : LoweringLanguage Ty}
    (b : MIRBlock lang) : MemoryBlock lang where
  id := b.id
  params := b.params.map fun p => ⟨p.1, toLogicalPlace p.2⟩
  body := b.body.map fun op => ⟨op.origin, toLogicalInst op.instruction⟩
  ending := toLogicalTerm b.ending
  boundaryOrigins := b.boundaryOrigins

/-- Pure graph-level mirror of `MIR.logicalView`'s per-function CFG translation. -/
def toLogicalGraph {Ty : Type} {lang : LoweringLanguage Ty} (g : MIRGraph lang) : MemoryGraph lang where
  entry := g.entry
  blocks := g.blocks.map toLogicalBlock

private def mirDefinedRoot {Ty : Type} {lang : LoweringLanguage Ty}
    (locals : LocalTypes Ty) (inst : MIRInst lang) : Option PlaceId :=
  let dest := match inst with
    | .bind dst _ | .op dst .. => some dst.root
    | _ => none
  dest.filter fun root => (AList.lookup locals root).isSome

/-- Roots in `locals` that both appear as a `bind`/`op` definition destination in `graph`
and have an ambiguous lifetime join under `Lifetimes.hasAmbiguousJoin`. -/
def ambiguousRoots {Ty : Type} {lang : LoweringLanguage Ty}
    (locals : LocalTypes Ty) (graph : MIRGraph lang) : List PlaceId :=
  (List.map Prod.fst locals).filter fun root =>
    graph.blocks.any (fun b => b.body.any fun op => mirDefinedRoot locals op.instruction == some root) &&
    hasAmbiguousJoin (toLogicalGraph graph) root

/-- Specialize every function in `mir` on its ambiguous-join roots only.
Functions with no ambiguous joins are untouched (`ambiguousRoots = []`). -/
def specializeMIR {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (mir : MIR lang profile) : MIR lang profile :=
  { mir with
    functions := mir.functions.map fun (locals, graph) =>
      (locals, splitGraphForRoots (ambiguousRoots locals graph) graph) }

/-! ### 3. Exact graph covering (`GraphCovering`, `MIRCovering`) -/

private theorem toList_toArray {α : Type} (xs : List α) : xs.toArray.toList = xs := rfl

private theorem nodup_map_inj {α β : Type} {f : α → β} (hf : ∀ a b, f a = f b → a = b)
    {xs : List α} (h : xs.Nodup) : (xs.map f).Nodup := by
  induction xs with
  | nil => exact .nil
  | cons a rest ih =>
    obtain ⟨hnot, hrest⟩ := List.nodup_cons.mp h
    refine List.nodup_cons.mpr ⟨?_, ih hrest⟩
    intro hm
    obtain ⟨b, hb, hfb⟩ := List.mem_map.mp hm
    exact hnot (hf b a hfb ▸ hb)

private theorem nodup_flatMap_pair {α β : Type} {xs : List α} {f g : α → β}
    (hnd : (xs.map f).Nodup)
    (hfg : ∀ x y, f x ≠ g y)
    (hg : ∀ x y, g x = g y → f x = f y) :
    (xs.flatMap fun x => [f x, g x]).Nodup := by
  induction xs with
  | nil => exact .nil
  | cons x rest ih =>
    obtain ⟨hfxNot, hrestNd⟩ := List.nodup_cons.mp hnd
    have hTail := ih hrestNd
    have hfxNotIn : f x ∉ rest.flatMap (fun y => [f y, g y]) := by
      intro hm
      obtain ⟨y, hy, hxy⟩ := List.mem_flatMap.mp hm
      simp only [List.mem_cons, List.not_mem_nil, or_false] at hxy
      rcases hxy with hEq | hEq
      · exact hfxNot (List.mem_map.mpr ⟨y, hy, hEq.symm⟩)
      · exact hfg x y hEq
    have hgxNotIn : g x ∉ rest.flatMap (fun y => [f y, g y]) := by
      intro hm
      obtain ⟨y, hy, hxy⟩ := List.mem_flatMap.mp hm
      simp only [List.mem_cons, List.not_mem_nil, or_false] at hxy
      rcases hxy with hEq | hEq
      · exact hfg y x hEq.symm
      · exact hfxNot (List.mem_map.mpr ⟨y, hy, (hg x y hEq).symm⟩)
    simp only [List.flatMap_cons, List.cons_append, List.nil_append]
    exact List.nodup_cons.mpr ⟨fun hm =>
      (List.mem_cons.mp hm).elim (hfg x x) hfxNotIn,
      List.nodup_cons.mpr ⟨hgxNotIn, hTail⟩⟩

/-- Block-projection covering of `dst` onto `src` along `proj : BlockId → BlockId`. -/
structure GraphCovering {Ty : Type} {lang : LoweringLanguage Ty}
    (proj : BlockId → BlockId) (src dst : MIRGraph lang) : Prop where
  entry : proj dst.entry = src.entry
  srcWF : src.WF Terminator.successors Sigma.fst Sigma.fst
  dstWF : dst.WF Terminator.successors Sigma.fst Sigma.fst
  forward : ∀ b2 ∈ dst.blocks.toList, ∃ b1 ∈ src.blocks.toList,
    b1.id = proj b2.id ∧ b1.params = b2.params ∧ b1.body = b2.body ∧
    b1.boundaryOrigins = b2.boundaryOrigins ∧
    b1.ending = mapTermTarget proj b2.ending
  surjective : ∀ b1 ∈ src.blocks.toList, ∃ b2 ∈ dst.blocks.toList,
    b1.id = proj b2.id ∧ b1.params = b2.params ∧ b1.body = b2.body ∧
    b1.boundaryOrigins = b2.boundaryOrigins ∧
    b1.ending = mapTermTarget proj b2.ending

private theorem GraphCovering.refl {Ty : Type} {lang : LoweringLanguage Ty}
    {g : MIRGraph lang} (hWF : g.WF Terminator.successors Sigma.fst Sigma.fst) :
    GraphCovering id g g where
  entry := rfl
  srcWF := hWF
  dstWF := hWF
  forward b hb := ⟨b, hb, rfl, rfl, rfl, rfl, (mapTermTarget_id b.ending).symm⟩
  surjective b hb := ⟨b, hb, rfl, rfl, rfl, rfl, (mapTermTarget_id b.ending).symm⟩

private theorem GraphCovering.trans {Ty : Type} {lang : LoweringLanguage Ty}
    {p1 p2 : BlockId → BlockId} {g1 g2 g3 : MIRGraph lang}
    (h12 : GraphCovering p1 g1 g2) (h23 : GraphCovering p2 g2 g3) :
    GraphCovering (p1 ∘ p2) g1 g3 where
  entry := by simp only [Function.comp_apply, h23.entry, h12.entry]
  srcWF := h12.srcWF
  dstWF := h23.dstWF
  forward b3 hb3 := by
    obtain ⟨b2, hb2, hid2, hp2, hbd2, hbo2, hend2⟩ := h23.forward b3 hb3
    obtain ⟨b1, hb1, hid1, hp1, hbd1, hbo1, hend1⟩ := h12.forward b2 hb2
    exact ⟨b1, hb1, hid1.trans (congrArg p1 hid2), hp1.trans hp2, hbd1.trans hbd2,
      hbo1.trans hbo2, hend1.trans ((congrArg (mapTermTarget p1) hend2).trans (mapTermTarget_comp p1 p2 _).symm)⟩
  surjective b1 hb1 := by
    obtain ⟨b2, hb2, hid1, hp1, hbd1, hbo1, hend1⟩ := h12.surjective b1 hb1
    obtain ⟨b3, hb3, hid2, hp2, hbd2, hbo2, hend2⟩ := h23.surjective b2 hb2
    exact ⟨b3, hb3, hid1.trans (congrArg p1 hid2), hp1.trans hp2, hbd1.trans hbd2,
      hbo1.trans hbo2, hend1.trans ((congrArg (mapTermTarget p1) hend2).trans (mapTermTarget_comp p1 p2 _).symm)⟩

private theorem splitGraphForRoot_wf {Ty : Type} {lang : LoweringLanguage Ty}
    (r : PlaceId) {g : MIRGraph lang}
    (hWF : g.WF Terminator.successors Sigma.fst Sigma.fst) :
    (splitGraphForRoot r g).WF Terminator.successors Sigma.fst Sigma.fst := by
  obtain ⟨hIds, ⟨eb, heb, heid⟩, hEdges⟩ := hWF
  refine ⟨?_, ?_, ?_⟩
  · have hMap : (splitGraphForRoot r g).blocks.toList.map Block.id =
        g.blocks.toList.flatMap fun b => [encodeBlockId b.id false, encodeBlockId b.id true] := by
      simp only [splitGraphForRoot, toList_toArray, List.map_flatMap]
      rfl
    rw [hMap]
    have hFnd : (g.blocks.toList.map fun b => encodeBlockId b.id false).Nodup := by
      rw [show (g.blocks.toList.map fun b => encodeBlockId b.id false) =
          (g.blocks.toList.map Block.id).map (fun id => encodeBlockId id false) by
        rw [List.map_map]; rfl]
      exact nodup_map_inj (fun _ _ h => (encodeBlockId_inj h).1) hIds
    exact nodup_flatMap_pair hFnd
      (fun x y h => Bool.noConfusion (encodeBlockId_inj h).2)
      (fun x y h => congrArg (fun id => encodeBlockId id false) (encodeBlockId_inj h).1)
  · refine ⟨splitBlock r eb false, ?_, by simp [splitBlock, splitGraphForRoot, heid]⟩
    simp only [splitGraphForRoot, toList_toArray, List.mem_flatMap]
    exact ⟨eb, heb, .head _⟩
  · intro b2 hb2 edge hedge
    simp only [splitGraphForRoot, toList_toArray, List.mem_flatMap] at hb2
    obtain ⟨b1, hb1, hmem⟩ := hb2
    simp only [List.mem_cons, List.not_mem_nil, or_false] at hmem
    have hEdgeCase : ∀ (inc : Bool),
        edge ∈ (splitBlock r b1 inc).ending.successors →
        ∃ next ∈ (splitGraphForRoot r g).blocks.toList,
          next.id = edge.target ∧
          edge.arguments.map Sigma.fst = next.params.map Sigma.fst := by
      intro inc he
      have hsucc := mapTermTarget_successors (fun tid => encodeBlockId tid (mirBlockOut b1 r inc)) b1.ending
      rw [splitBlock] at he
      dsimp only at he
      rw [hsucc] at he
      obtain ⟨e1, he1, rfl⟩ := List.mem_map.mp he
      obtain ⟨next1, hnext1, hnid, htypes⟩ := hEdges b1 hb1 e1 he1
      refine ⟨splitBlock r next1 (mirBlockOut b1 r inc), ?_, ?_, htypes⟩
      · simp only [splitGraphForRoot, toList_toArray, List.mem_flatMap]
        refine ⟨next1, hnext1, ?_⟩
        cases mirBlockOut b1 r inc <;> simp
      · simp only [splitBlock, hnid]
    rcases hmem with rfl | rfl
    · exact hEdgeCase false hedge
    · exact hEdgeCase true hedge

private theorem GraphCovering.of_splitGraphForRoot {Ty : Type} {lang : LoweringLanguage Ty}
    (r : PlaceId) {g : MIRGraph lang}
    (hWF : g.WF Terminator.successors Sigma.fst Sigma.fst) :
    GraphCovering decodeBlockId g (splitGraphForRoot r g) where
  entry := decodeBlockId_encode g.entry false
  srcWF := hWF
  dstWF := splitGraphForRoot_wf r hWF
  forward b2 hb2 := by
    simp only [splitGraphForRoot, toList_toArray, List.mem_flatMap] at hb2
    obtain ⟨b1, hb1, hmem⟩ := hb2
    simp only [List.mem_cons, List.not_mem_nil, or_false] at hmem
    rcases hmem with rfl | rfl
    · refine ⟨b1, hb1, (decodeBlockId_encode b1.id false).symm, rfl, rfl, rfl, ?_⟩
      simp only [splitBlock, ← mapTermTarget_comp]
      have hcomp : decodeBlockId ∘ (fun tid => encodeBlockId tid (mirBlockOut b1 r false)) = id := by
        funext tid; exact decodeBlockId_encode tid _
      rw [hcomp, mapTermTarget_id]
    · refine ⟨b1, hb1, (decodeBlockId_encode b1.id true).symm, rfl, rfl, rfl, ?_⟩
      simp only [splitBlock, ← mapTermTarget_comp]
      have hcomp : decodeBlockId ∘ (fun tid => encodeBlockId tid (mirBlockOut b1 r true)) = id := by
        funext tid; exact decodeBlockId_encode tid _
      rw [hcomp, mapTermTarget_id]
  surjective b1 hb1 := by
    refine ⟨splitBlock r b1 false, ?_, (decodeBlockId_encode b1.id false).symm, rfl, rfl, rfl, ?_⟩
    · simp only [splitGraphForRoot, toList_toArray, List.mem_flatMap]
      exact ⟨b1, hb1, .head _⟩
    · simp only [splitBlock, ← mapTermTarget_comp]
      have hcomp : decodeBlockId ∘ (fun tid => encodeBlockId tid (mirBlockOut b1 r false)) = id := by
        funext tid; exact decodeBlockId_encode tid _
      rw [hcomp, mapTermTarget_id]

private theorem GraphCovering.of_splitGraphForRoots {Ty : Type} {lang : LoweringLanguage Ty}
    (rs : List PlaceId) {g : MIRGraph lang}
    (hWF : g.WF Terminator.successors Sigma.fst Sigma.fst) :
    ∃ proj, GraphCovering proj g (splitGraphForRoots rs g) := by
  induction rs generalizing g with
  | nil => exact ⟨id, GraphCovering.refl hWF⟩
  | cons r rest ih =>
    have h1 := GraphCovering.of_splitGraphForRoot r hWF
    obtain ⟨proj', h2⟩ := ih h1.dstWF
    exact ⟨decodeBlockId ∘ proj', h1.trans h2⟩

/-! ### 4. Elimination of ambiguous lifetime joins under `splitGraphForRoots` -/

private theorem toLogicalGraph_eq_logicalView {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    (mir : MIR lang profile) :
    (mir.logicalView (language := language)).functions =
      mir.functions.map fun (locals, graph) => (locals, toLogicalGraph graph) := by
  with_unfolding_all
    apply List.map_congr_left
    rintro ⟨fn, locals, graph⟩ _
    cases graph with
    | mk entry blocks =>
      refine Prod.ext rfl (Prod.ext rfl ?_)
      dsimp only [toLogicalGraph]
      congr 1
      apply Array.map_congr_left
      intro b _
      cases b with
      | mk id params body ending bo =>
        dsimp only
        congr 1
        · apply List.map_congr_left; intro op _; cases op with | mk o inst =>
          dsimp only
          congr 1
          cases inst with
          | bind dst src => cases src <;> rfl
          | assign dst src => cases src <;> rfl
          | op dst op args checks addrs =>
            refine congrArg (fun x => MemoryInst.op (toLogicalPlace dst) op x checks
              (addrs.map fun a => ⟨a.1, toLogicalPlace a.2⟩)) ?_
            rename_i hopMem
            clear hopMem op
            induction args with
            | nil => rfl
            | cons a rest ih =>
              exact (congrFun (congrArg Args.cons (by cases a <;> rfl)) _).trans (congrArg _ ih)
          | endLifetime _ => rfl
          | destructure src bindings => rfl
          | call callee dst args checks =>
            refine congrArg (fun x => MemoryInst.call callee
              (dst.map fun d => ⟨d.1, toLogicalPlace d.2⟩) x checks) ?_
            apply List.map_congr_left
            rintro ⟨ty, a⟩ _; cases a <;> rfl
        · cases ending with
          | jump e =>
            cases e with
            | mk t args =>
              refine congrArg (fun x => Terminator.jump ⟨t, x⟩) ?_
              apply List.map_congr_left
              rintro ⟨ty, a⟩ _; cases a <;> rfl
          | branch c y n =>
            cases y with | mk yt ya =>
            cases n with | mk nt na =>
              have hMk : ∀ (c' : MemoryRead language.operands.Ty lang.literal lang.bool)
                  {a b d e : CFGEdge (Sigma (MemoryRead language.operands.Ty lang.literal))},
                  a = b → d = e → Terminator.branch c' a d = Terminator.branch c' b e := by
                intro _ _ _ _ _ h1 h2; rw [h1, h2]
              cases c with
              | literal v =>
                refine hMk (.literal v) (congrArg (CFGEdge.mk yt) ?_) (congrArg (CFGEdge.mk nt) ?_)
                · apply List.map_congr_left; rintro ⟨_, rd⟩ _; cases rd <;> rfl
                · apply List.map_congr_left; rintro ⟨_, rd⟩ _; cases rd <;> rfl
              | copy p =>
                refine hMk (.copy (toLogicalPlace p)) (congrArg (CFGEdge.mk yt) ?_) (congrArg (CFGEdge.mk nt) ?_)
                · apply List.map_congr_left; rintro ⟨_, rd⟩ _; cases rd <;> rfl
                · apply List.map_congr_left; rintro ⟨_, rd⟩ _; cases rd <;> rfl
              | move p =>
                refine hMk (.move (toLogicalPlace p)) (congrArg (CFGEdge.mk yt) ?_) (congrArg (CFGEdge.mk nt) ?_)
                · apply List.map_congr_left; rintro ⟨_, rd⟩ _; cases rd <;> rfl
                · apply List.map_congr_left; rintro ⟨_, rd⟩ _; cases rd <;> rfl
          | ret v =>
            cases v with
            | none => rfl
            | some a => cases a with | mk ty rd => cases rd <;> rfl
          | trap _ _ => rfl
          | invalid => rfl

private theorem toLogicalGraph_wf {Ty : Type} {lang : LoweringLanguage Ty} {g : MIRGraph lang} :
    (toLogicalGraph g).WF Terminator.successors Sigma.fst Sigma.fst ↔
      g.WF Terminator.successors Sigma.fst Sigma.fst := by
  have hSucc : ∀ t : Terminator (Sigma (Read Ty lang.literal)) (Read Ty lang.literal lang.bool),
      (toLogicalTerm t).successors = t.successors.map toLogicalEdge := by
    intro t; cases t <;> rfl
  unfold CFG.WF toLogicalGraph
  simp only [Array.toList_map, List.map_map, Function.comp_def, toLogicalBlock, List.mem_map]
  constructor
  · rintro ⟨hnd, ⟨_, ⟨eb, heb, rfl⟩, heid⟩, hedges⟩
    refine ⟨hnd, ⟨eb, heb, heid⟩, ?_⟩
    intro b hb e he
    have he' : toLogicalEdge e ∈ (toLogicalTerm b.ending).successors := by
      rw [hSucc]; exact List.mem_map.mpr ⟨e, he, rfl⟩
    obtain ⟨_, ⟨next, hnext, rfl⟩, hnid, htypes⟩ := hedges _ ⟨b, hb, rfl⟩ _ he'
    refine ⟨next, hnext, hnid, ?_⟩
    simpa [toLogicalEdge, List.map_map, Function.comp_def] using htypes
  · rintro ⟨hnd, ⟨eb, heb, heid⟩, hedges⟩
    refine ⟨hnd, ⟨_, ⟨eb, heb, rfl⟩, heid⟩, ?_⟩
    rintro _ ⟨b, hb, rfl⟩ edge hedge
    rw [hSucc] at hedge
    obtain ⟨e, he, rfl⟩ := List.mem_map.mp hedge
    obtain ⟨next, hnext, hnid, htypes⟩ := hedges b hb e he
    refine ⟨_, ⟨next, hnext, rfl⟩, hnid, ?_⟩
    simpa [toLogicalEdge, List.map_map, Function.comp_def] using htypes

private def memInstChange {Ty : Type} {lang : LoweringLanguage Ty} (inst : MemoryInst lang) (r : PlaceId) : Option Bool :=
  match inst with
  | .bind dst _ | .op dst .. => if dst.logical.root = r then some true else none
  | .beginLifetime id => if id = r then some true else none
  | .endLifetime id => if id = r then some false else none
  | .call _ dst .. => if dst.any (fun p : Sigma (MemoryPlace Ty) => p.2.logical.root == r) then some true else none
  | .destructure _ bindings =>
    if bindings.any (fun b : FieldId × Sigma (MemoryPlace Ty) => b.2.2.logical.root == r) then some true else none
  | .addWithOverflow _ dst flag _ _ =>
    if dst.logical.root = r || flag.logical.root = r then some true else none
  | .assign .. => none

private theorem mirInstChange_eq_toLogical {Ty : Type} {lang : LoweringLanguage Ty}
    (inst : MIRInst lang) (r : PlaceId) :
    memInstChange (toLogicalInst inst) r = mirInstChange inst r := by
  cases inst with
  | bind _ _ | assign _ _ | op _ _ _ _ _ | endLifetime _ => rfl
  | destructure _ bindings =>
    simp only [toLogicalInst, memInstChange, mirInstChange, List.any_map]
    rfl
  | call _ dst _ _ =>
    cases dst <;> rfl

private theorem foldl_toLogical_eq_mirBlockOut {Ty : Type} {lang : LoweringLanguage Ty}
    (b : MIRBlock lang) (r : PlaceId) (inc : Bool) :
    (toLogicalBlock b).body.foldl (fun fact op => (memInstChange op.instruction r).getD fact)
      (inc || (toLogicalBlock b).params.any (fun p : Sigma (MemoryPlace Ty) => p.2.logical.root == r)) =
      mirBlockOut b r inc := by
  have hParams : (toLogicalBlock b).params.any (fun p : Sigma (MemoryPlace Ty) => p.2.logical.root == r) =
      b.params.any (fun p : Sigma (Place Ty) => p.2.root == r) := by
    simp [toLogicalBlock, toLogicalPlace, List.any_map, Function.comp_def]
  rw [hParams]
  unfold mirBlockOut toLogicalBlock
  dsimp only
  rw [List.foldl_map]
  simp only [mirInstChange_eq_toLogical]

private theorem toLogicalTerm_mapTarget {Ty : Type} {lang : LoweringLanguage Ty}
    (f : BlockId → BlockId)
    (t : Terminator (Sigma (Read Ty lang.literal)) (Read Ty lang.literal lang.bool)) :
    (toLogicalTerm (mapTermTarget f t)).successors =
      (toLogicalTerm t).successors.map (fun e => { e with target := f e.target }) := by
  cases t <;> rfl

private theorem array_find?_of_mem_nodup {P I T : Type} {blocks : Array (Block P I T)}
    (hnd : (blocks.toList.map Block.id).Nodup) {b : Block P I T}
    (hmem : b ∈ blocks.toList) :
    blocks.find? (·.id == b.id) = some b := by
  rw [← Array.find?_toList]
  have aux : ∀ (xs : List (Block P I T)), (xs.map Block.id).Nodup → b ∈ xs →
      xs.find? (·.id == b.id) = some b := by
    intro xs hnd' hm
    induction xs with
    | nil => cases hm
    | cons a rest ih =>
      obtain ⟨hnot, hrest⟩ := List.nodup_cons.mp hnd'
      rcases List.mem_cons.mp hm with rfl | htail
      · simp
      · have hne : (a.id == b.id) = false := by
          cases hbeq : (a.id == b.id) with
          | false => rfl
          | true =>
            exact (hnot (List.mem_map.mpr ⟨b, htail, (eq_of_beq hbeq).symm⟩)).elim
        simp only [List.find?_cons, hne]
        exact ih hrest htail
  exact aux blocks.toList hnd hmem

/-- Every `EntryPath` for `r` in `splitGraphForRoot r g` tracks `r`'s liveness in the
encoded block ID's parity bit. -/
private theorem entryPath_splitGraphForRoot_self {Ty : Type} {lang : LoweringLanguage Ty}
    (r : PlaceId) {g : MIRGraph lang} {bid : BlockId} {inc : Bool}
    (hp : EntryPath (toLogicalGraph (splitGraphForRoot r g)) r (bid, inc)) :
    ∃ origId, bid = encodeBlockId origId inc := by
  refine EntryPath.ind_step (fun bid inc => ∃ origId, bid = encodeBlockId origId inc)
    ⟨g.entry, rfl⟩ ?_ hp
  intro src prev edge hfind _ ih hedge
  obtain ⟨origSrc, hSrcId⟩ := ih
  have hSrcMem : src ∈ (toLogicalGraph (splitGraphForRoot r g)).blocks.toList :=
    Array.mem_toList_iff.mpr (Array.mem_of_find?_eq_some hfind)
  simp only [toLogicalGraph, splitGraphForRoot, Array.toList_map,
    List.mem_map, List.mem_flatMap] at hSrcMem
  obtain ⟨b2, ⟨b1, _, hb2Mem⟩, rfl⟩ := hSrcMem
  simp only [List.mem_cons, List.not_mem_nil, or_false] at hb2Mem
  rcases hb2Mem with rfl | rfl
  · have hprev : prev = false := (encodeBlockId_inj hSrcId).2.symm
    subst hprev
    have hsucc : (toLogicalBlock (splitBlock r b1 false)).ending.successors =
        (toLogicalTerm b1.ending).successors.map (fun e => { e with target := encodeBlockId e.target (mirBlockOut b1 r false) }) := by
      simpa only [toLogicalBlock, splitBlock] using
        toLogicalTerm_mapTarget (fun tid => encodeBlockId tid (mirBlockOut b1 r false)) b1.ending
    rw [hsucc] at hedge
    obtain ⟨e1, _, rfl⟩ := List.mem_map.mp hedge
    have hout : (toLogicalBlock (splitBlock r b1 false)).body.foldl
        (fun fact op => (memInstChange op.instruction r).getD fact)
        (false || (toLogicalBlock (splitBlock r b1 false)).params.any
          (fun p : Sigma (MemoryPlace Ty) => p.2.logical.root == r)) = mirBlockOut b1 r false :=
      foldl_toLogical_eq_mirBlockOut b1 r false
    exact ⟨e1.target, congrArg (encodeBlockId e1.target) hout.symm⟩
  · have hprev : prev = true := (encodeBlockId_inj hSrcId).2.symm
    subst hprev
    have hsucc : (toLogicalBlock (splitBlock r b1 true)).ending.successors =
        (toLogicalTerm b1.ending).successors.map (fun e => { e with target := encodeBlockId e.target (mirBlockOut b1 r true) }) := by
      simpa only [toLogicalBlock, splitBlock] using
        toLogicalTerm_mapTarget (fun tid => encodeBlockId tid (mirBlockOut b1 r true)) b1.ending
    rw [hsucc] at hedge
    obtain ⟨e1, _, rfl⟩ := List.mem_map.mp hedge
    have hout : (toLogicalBlock (splitBlock r b1 true)).body.foldl
        (fun fact op => (memInstChange op.instruction r).getD fact)
        (true || (toLogicalBlock (splitBlock r b1 true)).params.any
          (fun p : Sigma (MemoryPlace Ty) => p.2.logical.root == r)) = mirBlockOut b1 r true :=
      foldl_toLogical_eq_mirBlockOut b1 r true
    exact ⟨e1.target, congrArg (encodeBlockId e1.target) hout.symm⟩

/-- Every `EntryPath` for ANY root `q` in `splitGraphForRoot r g` projects via
`decodeBlockId` to an `EntryPath` in `g`. -/
private theorem entryPath_splitGraphForRoot_proj {Ty : Type} {lang : LoweringLanguage Ty}
    (r q : PlaceId) {g : MIRGraph lang}
    (hWF : g.WF Terminator.successors Sigma.fst Sigma.fst)
    {bid : BlockId} {inc : Bool}
    (hp : EntryPath (toLogicalGraph (splitGraphForRoot r g)) q (bid, inc)) :
    EntryPath (toLogicalGraph g) q (decodeBlockId bid, inc) := by
  have hLogWF : (toLogicalGraph g).WF Terminator.successors Sigma.fst Sigma.fst :=
    toLogicalGraph_wf.mpr hWF
  refine EntryPath.ind_step (fun bid inc => EntryPath (toLogicalGraph g) q (decodeBlockId bid, inc))
    ?_ ?_ hp
  · obtain ⟨eb, heb, heid⟩ := hLogWF.2.1
    change EntryPath (toLogicalGraph g) q (decodeBlockId (encodeBlockId g.entry false), false)
    rw [decodeBlockId_encode]
    exact EntryPath.mk_initial heb heid
  · intro src prev edge hfind _ ih hedge
    have hSrcMem : src ∈ (toLogicalGraph (splitGraphForRoot r g)).blocks.toList :=
      Array.mem_toList_iff.mpr (Array.mem_of_find?_eq_some hfind)
    simp only [toLogicalGraph, splitGraphForRoot, Array.toList_map,
      List.mem_map, List.mem_flatMap] at hSrcMem
    obtain ⟨b2, ⟨b1, hb1, hb2Mem⟩, rfl⟩ := hSrcMem
    have hLogB1 : toLogicalBlock b1 ∈ (toLogicalGraph g).blocks.toList := by
      simp only [toLogicalGraph, Array.toList_map, List.mem_map]
      exact ⟨b1, hb1, rfl⟩
    have hFindB1 : (toLogicalGraph g).blocks.find? (·.id == (toLogicalBlock b1).id) = some (toLogicalBlock b1) :=
      array_find?_of_mem_nodup hLogWF.1 hLogB1
    have hStepCase : ∀ (inc : Bool),
        EntryPath (toLogicalGraph g) q (decodeBlockId (toLogicalBlock (splitBlock r b1 inc)).id, prev) →
        edge ∈ (toLogicalBlock (splitBlock r b1 inc)).ending.successors →
        EntryPath (toLogicalGraph g) q
          (decodeBlockId edge.target,
            (toLogicalBlock (splitBlock r b1 inc)).body.foldl
              (fun fact op => (memInstChange op.instruction q).getD fact)
              (prev || (toLogicalBlock (splitBlock r b1 inc)).params.any
                (fun p : Sigma (MemoryPlace Ty) => p.2.logical.root == q))) := by
      intro inc ih' he'
      have hDecId : decodeBlockId (toLogicalBlock (splitBlock r b1 inc)).id = (toLogicalBlock b1).id :=
        decodeBlockId_encode b1.id inc
      rw [hDecId] at ih'
      have hsucc : (toLogicalBlock (splitBlock r b1 inc)).ending.successors =
          (toLogicalTerm b1.ending).successors.map
            (fun e => { e with target := encodeBlockId e.target (mirBlockOut b1 r inc) }) := by
        simpa only [toLogicalBlock, splitBlock] using
          toLogicalTerm_mapTarget (fun tid => encodeBlockId tid (mirBlockOut b1 r inc)) b1.ending
      rw [hsucc] at he'
      obtain ⟨e1, he1, rfl⟩ := List.mem_map.mp he'
      obtain ⟨dst1, hdst1, hdstId, _⟩ := hLogWF.2.2 (toLogicalBlock b1) hLogB1 e1 he1
      change EntryPath (toLogicalGraph g) q (decodeBlockId (encodeBlockId e1.target (mirBlockOut b1 r inc)), _)
      rw [decodeBlockId_encode, ← hdstId]
      exact EntryPath.mk_edge (src := toLogicalBlock b1) ih' hFindB1 he1 hdst1 hdstId
    simp only [List.mem_cons, List.not_mem_nil, or_false] at hb2Mem
    rcases hb2Mem with rfl | rfl
    · exact hStepCase false ih hedge
    · exact hStepCase true ih hedge

/-- Splitting on `r` makes `r` unambiguous at every block of `splitGraphForRoot r g`. -/
private theorem rootUnambiguousAt_splitGraphForRoot_self {Ty : Type} {lang : LoweringLanguage Ty}
    (r : PlaceId) (g : MIRGraph lang) {blk : MemoryBlock lang} :
    RootUnambiguousAt (toLogicalGraph (splitGraphForRoot r g)) blk r := by
  refine .inr (.inr ?_)
  rintro ⟨ht, hf⟩
  obtain ⟨id1, h1⟩ := entryPath_splitGraphForRoot_self r ht
  obtain ⟨id2, h2⟩ := entryPath_splitGraphForRoot_self r hf
  exact Bool.noConfusion (encodeBlockId_inj (h1.symm.trans h2)).2

/-- Splitting on `r` preserves `RootUnambiguousAt` for any root `q` that was already
unambiguous at every block of `g`. -/
private theorem rootUnambiguousAt_splitGraphForRoot_preserve {Ty : Type} {lang : LoweringLanguage Ty}
    (r q : PlaceId) {g : MIRGraph lang}
    (hWF : g.WF Terminator.successors Sigma.fst Sigma.fst)
    (hPrev : ∀ b ∈ (toLogicalGraph g).blocks.toList, RootUnambiguousAt (toLogicalGraph g) b q)
    {blk : MemoryBlock lang} (hblk : blk ∈ (toLogicalGraph (splitGraphForRoot r g)).blocks.toList) :
    RootUnambiguousAt (toLogicalGraph (splitGraphForRoot r g)) blk q := by
  simp only [toLogicalGraph, splitGraphForRoot, Array.toList_map,
    List.mem_map, List.mem_flatMap] at hblk
  obtain ⟨b2, ⟨b1, hb1, hb2Mem⟩, rfl⟩ := hblk
  have hLogB1 : toLogicalBlock b1 ∈ (toLogicalGraph g).blocks.toList := by
    simp only [toLogicalGraph, Array.toList_map, List.mem_map]
    exact ⟨b1, hb1, rfl⟩
  have hUnamb := hPrev (toLogicalBlock b1) hLogB1
  have hCase : ∀ (inc : Bool),
      RootUnambiguousAt (toLogicalGraph (splitGraphForRoot r g)) (toLogicalBlock (splitBlock r b1 inc)) q := by
    intro inc
    rcases hUnamb with hParam | hNoPred | hNotBoth
    · exact .inl hParam
    · refine .inr (.inl ?_)
      cases hAny : (toLogicalGraph (splitGraphForRoot r g)).blocks.any
          (fun b => b.ending.successors.any (·.target == (toLogicalBlock (splitBlock r b1 inc)).id)) with
      | false => rfl
      | true =>
        obtain ⟨src, hsrcMem, hedge⟩ := Array.any_eq_true'.mp hAny
        have hsrcList : src ∈ (toLogicalGraph (splitGraphForRoot r g)).blocks.toList :=
          Array.mem_toList_iff.mpr hsrcMem
        simp only [toLogicalGraph, splitGraphForRoot, Array.toList_map,
          List.mem_map, List.mem_flatMap] at hsrcList
        obtain ⟨sb2, ⟨sb1, hsb1, hsb2Mem⟩, rfl⟩ := hsrcList
        obtain ⟨e, he, hbeq⟩ := List.any_eq_true.mp hedge
        have htgtEq : e.target = (toLogicalBlock (splitBlock r b1 inc)).id := eq_of_beq hbeq
        have hSbCase : ∀ (sinc : Bool),
            e ∈ (toLogicalBlock (splitBlock r sb1 sinc)).ending.successors → False := by
          intro sinc he'
          have hsucc := toLogicalTerm_mapTarget (fun tid => encodeBlockId tid (mirBlockOut sb1 r sinc)) sb1.ending
          simp only [toLogicalBlock, splitBlock] at he'
          rw [hsucc] at he'
          obtain ⟨e0, he0, rfl⟩ := List.mem_map.mp he'
          have horigEq : e0.target = (toLogicalBlock b1).id := (encodeBlockId_inj htgtEq).1
          have hexists : (toLogicalGraph g).blocks.any
              (fun b => b.ending.successors.any (·.target == (toLogicalBlock b1).id)) = true :=
            Array.any_eq_true'.mpr ⟨toLogicalBlock sb1,
              Array.mem_toList_iff.mp (by simp only [toLogicalGraph, Array.toList_map, List.mem_map]; exact ⟨sb1, hsb1, rfl⟩),
              List.any_eq_true.mpr ⟨e0, he0, by simp [horigEq]⟩⟩
          rw [hNoPred] at hexists
          cases hexists
        simp only [List.mem_cons, List.not_mem_nil, or_false] at hsb2Mem
        rcases hsb2Mem with rfl | rfl
        · exact (hSbCase false he).elim
        · exact (hSbCase true he).elim
    · refine .inr (.inr ?_)
      rintro ⟨ht, hf⟩
      have ht' := entryPath_splitGraphForRoot_proj r q hWF ht
      have hf' := entryPath_splitGraphForRoot_proj r q hWF hf
      change EntryPath (toLogicalGraph g) q (decodeBlockId (encodeBlockId b1.id inc), true) at ht'
      change EntryPath (toLogicalGraph g) q (decodeBlockId (encodeBlockId b1.id inc), false) at hf'
      rw [decodeBlockId_encode] at ht' hf'
      exact hNotBoth ⟨ht', hf'⟩
  simp only [List.mem_cons, List.not_mem_nil, or_false] at hb2Mem
  rcases hb2Mem with rfl | rfl
  · exact hCase false
  · exact hCase true

private theorem rootUnambiguousAt_splitGraphForRoots {Ty : Type} {lang : LoweringLanguage Ty}
    (rs : List PlaceId) {g : MIRGraph lang}
    (hWF : g.WF Terminator.successors Sigma.fst Sigma.fst)
    {q : PlaceId}
    (hInit : q ∈ rs ∨ ∀ b ∈ (toLogicalGraph g).blocks.toList, RootUnambiguousAt (toLogicalGraph g) b q) :
    ∀ b ∈ (toLogicalGraph (splitGraphForRoots rs g)).blocks.toList,
      RootUnambiguousAt (toLogicalGraph (splitGraphForRoots rs g)) b q := by
  induction rs generalizing g with
  | nil =>
    rcases hInit with hm | hall
    · cases hm
    · exact hall
  | cons r rest ih =>
    have hWF' := splitGraphForRoot_wf r hWF
    apply ih hWF'
    rcases hInit with hm | hall
    · rcases List.mem_cons.mp hm with rfl | hrest
      · exact .inr fun b _ => rootUnambiguousAt_splitGraphForRoot_self q g
      · exact .inl hrest
    · exact .inr fun b hb => rootUnambiguousAt_splitGraphForRoot_preserve r q hWF hall hb

/-- Structural shape equivalence between two `MemoryGraph`s differing only in `.storage`
annotations on places (as produced by `prepareGraph`'s `classifyGraph`). -/
private structure StorageShapeEq {Ty : Type} {lang : LoweringLanguage Ty}
    (g1 g2 : MemoryGraph lang) : Prop where
  entry : g1.entry = g2.entry
  size : g1.blocks.size = g2.blocks.size
  blockAt : ∀ (i : Nat) (b1 b2 : MemoryBlock lang),
    g1.blocks[i]? = some b1 → g2.blocks[i]? = some b2 →
    b1.id = b2.id ∧
    b1.params.map (fun p => p.2.logical.root) = b2.params.map (fun p => p.2.logical.root) ∧
    b1.ending.successors.map CFGEdge.target = b2.ending.successors.map CFGEdge.target ∧
    ∀ r, b1.body.map (fun op => memInstChange op.instruction r) =
         b2.body.map (fun op => memInstChange op.instruction r)

private theorem list_any_eq_of_map_eq {α β : Type} {xs : List α} {ys : List β}
    {f : α → PlaceId} {g : β → PlaceId} {p : PlaceId → Bool}
    (h : xs.map f = ys.map g) :
    xs.any (fun a => p (f a)) = ys.any (fun b => p (g b)) := by
  induction xs generalizing ys with
  | nil => cases ys <;> simp at h ⊢
  | cons a rest ih =>
    cases ys with
    | nil => cases h
    | cons b rest' =>
      simp only [List.map_cons, List.cons.injEq] at h
      simp only [List.any_cons, h.1, ih h.2]

private theorem list_foldl_eq_of_map_eq {α β γ : Type} {xs : List α} {ys : List β}
    {f : α → γ} {g : β → γ} {step : Bool → γ → Bool} {init : Bool}
    (h : xs.map f = ys.map g) :
    xs.foldl (fun acc a => step acc (f a)) init = ys.foldl (fun acc b => step acc (g b)) init := by
  induction xs generalizing ys init with
  | nil => cases ys <;> simp at h ⊢
  | cons a rest ih =>
    cases ys with
    | nil => cases h
    | cons b rest' =>
      simp only [List.map_cons, List.cons.injEq] at h
      simp only [List.foldl_cons, h.1]
      exact ih h.2

private theorem storageShapeEq_find? {Ty : Type} {lang : LoweringLanguage Ty}
    {g1 g2 : MemoryGraph lang} (h : StorageShapeEq g1 g2) (bid : BlockId) :
    ∀ b1, g1.blocks.find? (·.id == bid) = some b1 →
      ∃ (i : Nat), ∃ b2, g1.blocks[i]? = some b1 ∧ g2.blocks[i]? = some b2 ∧
        g2.blocks.find? (·.id == bid) = some b2 := by
  intro b1 hf
  have aux : ∀ (k : Nat) (xs ys : List (MemoryBlock lang)),
      xs.length = ys.length →
      (∀ (j : Nat) (a b : MemoryBlock lang), xs[j]? = some a → ys[j]? = some b → a.id = b.id) →
      xs.find? (·.id == bid) = some b1 →
      ∃ (j : Nat), ∃ b2, xs[j]? = some b1 ∧ ys[j]? = some b2 ∧ ys.find? (·.id == bid) = some b2 := by
    intro _ xs ys
    induction xs generalizing ys with
    | nil => intro _ _ h; cases h
    | cons a rest ih =>
      cases ys with
      | nil => intro hlen; cases hlen
      | cons b rest' =>
        intro hlen hid hfind
        have hab : a.id = b.id := hid 0 a b rfl rfl
        simp only [List.find?_cons] at hfind ⊢
        rw [← hab]
        cases hcond : (a.id == bid) with
        | true =>
          rw [hcond] at hfind
          cases Option.some.inj hfind
          exact ⟨0, b, rfl, rfl, rfl⟩
        | false =>
          rw [hcond] at hfind
          obtain ⟨j, b2, h1, h2, h3⟩ := ih rest' (Nat.succ.inj hlen)
            (fun j' x y hx hy => hid (j' + 1) x y hx hy) hfind
          exact ⟨j + 1, b2, h1, h2, h3⟩
  rw [← Array.find?_toList] at hf
  obtain ⟨j, b2, h1, h2, h3⟩ := aux 0 g1.blocks.toList g2.blocks.toList
    (by simp [h.size])
    (fun j a b ha hb => (h.blockAt j a b (by simpa [Array.getElem?_toList] using ha)
      (by simpa [Array.getElem?_toList] using hb)).1)
    hf
  exact ⟨j, b2, by simpa [Array.getElem?_toList] using h1,
    by simpa [Array.getElem?_toList] using h2, by rwa [← Array.find?_toList]⟩

private theorem storageShapeEq_symm {Ty : Type} {lang : LoweringLanguage Ty}
    {g1 g2 : MemoryGraph lang} (h : StorageShapeEq g1 g2) : StorageShapeEq g2 g1 where
  entry := h.entry.symm
  size := h.size.symm
  blockAt i b2 b1 h2 h1 :=
    let ⟨a, b, c, d⟩ := h.blockAt i b1 b2 h1 h2
    ⟨a.symm, b.symm, c.symm, fun r => (d r).symm⟩

private theorem storageShapeEq_entryPath {Ty : Type} {lang : LoweringLanguage Ty}
    {g1 g2 : MemoryGraph lang} (h : StorageShapeEq g1 g2)
    (hWF2 : g2.WF Terminator.successors Sigma.fst Sigma.fst)
    {r : PlaceId} {bid : BlockId} {inc : Bool}
    (hp : EntryPath g1 r (bid, inc)) : EntryPath g2 r (bid, inc) := by
  refine EntryPath.ind_step (fun bid inc => EntryPath g2 r (bid, inc)) ?_ ?_ hp
  · obtain ⟨eb2, heb2, heid2⟩ := hWF2.2.1
    rw [h.entry]
    exact EntryPath.mk_initial heb2 heid2
  · intro src1 prev edge1 hfind1 _ ih hedge1
    obtain ⟨i, src2, hget1, hget2, hfind2⟩ := storageShapeEq_find? h src1.id src1 hfind1
    obtain ⟨hid, hparams, hsucc, hbody⟩ := h.blockAt i src1 src2 hget1 hget2
    have hTargetIn : edge1.target ∈ src2.ending.successors.map CFGEdge.target := by
      rw [← hsucc]; exact List.mem_map.mpr ⟨edge1, hedge1, rfl⟩
    obtain ⟨edge2, hedge2, htgtEq⟩ := List.mem_map.mp hTargetIn
    have hSrc2Mem : src2 ∈ g2.blocks.toList := Array.mem_toList_iff.mpr (Array.mem_of_find?_eq_some hfind2)
    obtain ⟨dst2, hdst2, hdstId, _⟩ := hWF2.2.2 src2 hSrc2Mem edge2 hedge2
    have hFindSrc2 : g2.blocks.find? (·.id == src2.id) = some src2 := by rwa [← hid]
    have hParamEq : src1.params.any (fun p : Sigma (MemoryPlace Ty) => p.2.logical.root == r) =
        src2.params.any (fun p : Sigma (MemoryPlace Ty) => p.2.logical.root == r) :=
      list_any_eq_of_map_eq (p := (· == r)) hparams
    have hFoldEq : src1.body.foldl (fun fact op => (memInstChange op.instruction r).getD fact)
        (prev || src1.params.any (fun p : Sigma (MemoryPlace Ty) => p.2.logical.root == r)) =
        src2.body.foldl (fun fact op => (memInstChange op.instruction r).getD fact)
        (prev || src2.params.any (fun p : Sigma (MemoryPlace Ty) => p.2.logical.root == r)) := by
      rw [hParamEq]
      exact list_foldl_eq_of_map_eq (step := fun acc opt => opt.getD acc) (hbody r)
    change EntryPath g2 r (edge1.target,
      src1.body.foldl (fun fact op => (memInstChange op.instruction r).getD fact)
        (prev || src1.params.any (fun p : Sigma (MemoryPlace Ty) => p.2.logical.root == r)))
    rw [← htgtEq, hFoldEq, ← hdstId]
    exact EntryPath.mk_edge (hid ▸ ih) hFindSrc2 hedge2 hdst2 hdstId

private theorem storageShapeEq_rootUnambiguousAt {Ty : Type} {lang : LoweringLanguage Ty}
    {g1 g2 : MemoryGraph lang} (h : StorageShapeEq g1 g2)
    (hWF1 : g1.WF Terminator.successors Sigma.fst Sigma.fst)
    {i : Nat} {b1 b2 : MemoryBlock lang}
    (hb1 : g1.blocks[i]? = some b1) (hb2 : g2.blocks[i]? = some b2)
    {r : PlaceId} (hU : RootUnambiguousAt g1 b1 r) :
    RootUnambiguousAt g2 b2 r := by
  obtain ⟨hid, hparams, _, _⟩ := h.blockAt i b1 b2 hb1 hb2
  rcases hU with hP | hNoEdge | hNotBoth
  · exact .inl ((list_any_eq_of_map_eq (p := (· == r)) hparams).symm.trans hP)
  · refine .inr (.inl ?_)
    cases hAny : g2.blocks.any (fun b => b.ending.successors.any (·.target == b2.id)) with
    | false => rfl
    | true =>
      obtain ⟨s2, hs2Mem, hEx⟩ := Array.any_eq_true'.mp hAny
      obtain ⟨j, hjLt, hjEq⟩ := Array.getElem_of_mem hs2Mem
      have hget2 : g2.blocks[j]? = some s2 := by rw [Array.getElem?_eq_getElem hjLt, hjEq]
      have hjLt1 : j < g1.blocks.size := h.size ▸ hjLt
      have hget1 : g1.blocks[j]? = some g1.blocks[j] := Array.getElem?_eq_getElem hjLt1
      obtain ⟨_, _, hsuccJ, _⟩ := h.blockAt j g1.blocks[j] s2 hget1 hget2
      obtain ⟨e2, he2, hbeq⟩ := List.any_eq_true.mp hEx
      have htgtIn : b1.id ∈ g1.blocks[j].ending.successors.map CFGEdge.target := by
        rw [hsuccJ]
        exact List.mem_map.mpr ⟨e2, he2, (eq_of_beq hbeq).trans hid.symm⟩
      obtain ⟨e1, he1, ht1⟩ := List.mem_map.mp htgtIn
      have hAny1 : g1.blocks.any (fun b => b.ending.successors.any (·.target == b1.id)) = true :=
        Array.any_eq_true'.mpr ⟨g1.blocks[j], Array.getElem_mem hjLt1,
          List.any_eq_true.mpr ⟨e1, he1, by simp [ht1]⟩⟩
      rw [hNoEdge] at hAny1
      cases hAny1
  · refine .inr (.inr ?_)
    rintro ⟨ht, hf⟩
    have hSym := storageShapeEq_symm h
    exact hNotBoth ⟨hid ▸ storageShapeEq_entryPath hSym hWF1 ht,
      hid ▸ storageShapeEq_entryPath hSym hWF1 hf⟩

private theorem storageShapeEq_of_prepareGraph {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {mir : MIR lang profile} {prepared : PreparedGraphs lang profile}
    (hprep : prepareGraph (language := language) mir = .ok prepared) :
    prepared.functions.length = mir.functions.length ∧
    ∀ (i : Nat) fn locals gSrc gPrep,
      mir.functions[i]? = some (fn, locals, gSrc) →
      prepared.functions[i]? = some (fn, locals, gPrep) →
      StorageShapeEq (toLogicalGraph gSrc) gPrep ∧
      ∀ bPrep ∈ gPrep.blocks.toList, ∀ op ∈ bPrep.body, ∀ root,
        definitionRoot locals op.instruction = some root →
        ∃ bSrc ∈ gSrc.blocks.toList, ∃ opSrc ∈ bSrc.body,
          mirDefinedRoot locals opSrc.instruction = some root := by
  have hPrepUnf : prepared.functions =
      List.map (fun (fn, locals, graph) =>
        (fn, locals,
          let classes := List.map (fun (r, ty) => (r, storageClass (language := language) mir fn r ty)) locals
          let cls := fun r => (AList.lookup classes r).getD .materialized
          { graph with blocks := graph.blocks.map fun block =>
              { block with
                params := block.params.map fun p => ⟨p.1, { p.2 with storage := cls p.2.logical.root }⟩
                body := block.body.map fun op =>
                  { op with
                    instruction := match op.instruction with
                      | .bind dst src => .bind { dst with storage := cls dst.logical.root } (match src with
                          | .literal v => .literal v | .copy p => .copy { p with storage := cls p.logical.root }
                          | .move p => .move { p with storage := cls p.logical.root })
                      | .assign dst src => .assign { dst with storage := cls dst.logical.root } (match src with
                          | .literal v => .literal v | .copy p => .copy { p with storage := cls p.logical.root }
                          | .move p => .move { p with storage := cls p.logical.root })
                      | .op dst op args checks addrs =>
                        .op { dst with storage := cls dst.logical.root } op
                          (args.rec .nil fun a _ ih => .cons (match a with
                            | .literal v => .literal v | .copy p => .copy { p with storage := cls p.logical.root }
                            | .move p => .move { p with storage := cls p.logical.root }) ih)
                          checks (addrs.map fun a => ⟨a.1, { a.2 with storage := cls a.2.logical.root }⟩)
                      | .addWithOverflow t dst flag lhs rhs =>
                        .addWithOverflow t { dst with storage := cls dst.logical.root }
                          { flag with storage := cls flag.logical.root }
                          (match lhs with
                            | .literal v => .literal v | .copy p => .copy { p with storage := cls p.logical.root }
                            | .move p => .move { p with storage := cls p.logical.root })
                          (match rhs with
                            | .literal v => .literal v | .copy p => .copy { p with storage := cls p.logical.root }
                            | .move p => .move { p with storage := cls p.logical.root })
                      | .beginLifetime p => .beginLifetime p
                      | .endLifetime p => .endLifetime p
                      | .destructure src bs =>
                        .destructure { src with storage := cls src.logical.root }
                          (bs.map fun (f, d) => (f, ⟨d.1, { d.2 with storage := cls d.2.logical.root }⟩))
                      | .call callee dst args checks =>
                        .call callee (dst.map fun d => ⟨d.1, { d.2 with storage := cls d.2.logical.root }⟩)
                          (args.map fun a => ⟨a.1, match a.2 with
                            | .literal v => .literal v | .copy p => .copy { p with storage := cls p.logical.root }
                            | .move p => .move { p with storage := cls p.logical.root }⟩) checks }
                ending := match block.ending with
                  | .jump e => .jump { e with arguments := e.arguments.map fun a => ⟨a.1, match a.2 with
                      | .literal v => .literal v | .copy p => .copy { p with storage := cls p.logical.root }
                      | .move p => .move { p with storage := cls p.logical.root }⟩ }
                  | .branch t y n => .branch (match t with
                      | .literal v => .literal v | .copy p => .copy { p with storage := cls p.logical.root }
                      | .move p => .move { p with storage := cls p.logical.root })
                      { y with arguments := y.arguments.map fun a => ⟨a.1, match a.2 with
                        | .literal v => .literal v | .copy p => .copy { p with storage := cls p.logical.root }
                        | .move p => .move { p with storage := cls p.logical.root }⟩ }
                      { n with arguments := n.arguments.map fun a => ⟨a.1, match a.2 with
                        | .literal v => .literal v | .copy p => .copy { p with storage := cls p.logical.root }
                        | .move p => .move { p with storage := cls p.logical.root }⟩ }
                  | .ret r => .ret (r.map fun a => ⟨a.1, match a.2 with
                      | .literal v => .literal v | .copy p => .copy { p with storage := cls p.logical.root }
                      | .move p => .move { p with storage := cls p.logical.root }⟩)
                  | .trap k o => .trap k o
                  | .invalid => .invalid } }))
        (mir.logicalView (language := language)).functions := by
    unfold prepareGraph at hprep
    simp only [bind, Except.bind] at hprep
    split at hprep
    · cases hprep
    · simp only [pure, Except.pure, Except.ok.injEq] at hprep
      subst prepared
      apply List.map_congr_left
      rintro ⟨fn, locals, graph⟩ _
      with_unfolding_all
        cases graph with
        | mk entry blocks =>
          refine Prod.ext rfl (Prod.ext rfl ?_)
          dsimp only
          apply congrArg (CFG.mk entry)
          apply Array.map_congr_left
          intro block _
          cases block with
          | mk id params body ending bo =>
            dsimp only
            congr 1
            · apply List.map_congr_left
              intro op _; cases op with | mk o inst =>
              congr 1
              cases inst with
              | bind _ src => cases src <;> rfl
              | assign _ src => cases src <;> rfl
              | op dst op args checks addrs =>
                let cls := fun r => (AList.lookup (List.map (fun (r, ty) => (r, storageClass (language := language) mir fn r ty)) locals) r).getD StorageClass.materialized
                refine congrArg (fun x => MemoryInst.op
                  { dst with storage := cls dst.logical.root } op x checks
                  (addrs.map fun a => ⟨a.1, { a.2 with storage := cls a.2.logical.root }⟩)) ?_
                rename_i hopMem
                clear hopMem op
                induction args with
                | nil => rfl
                | cons a rest ih =>
                  exact (congrFun (congrArg Args.cons (by cases a <;> rfl)) _).trans (congrArg _ ih)
              | addWithOverflow _ _ _ l r => cases l <;> cases r <;> rfl
              | beginLifetime _ => rfl
              | endLifetime _ => rfl
              | destructure _ _ => rfl
              | call callee dst args checks =>
                let cls := fun r => (AList.lookup (List.map (fun (r, ty) => (r, storageClass (language := language) mir fn r ty)) locals) r).getD StorageClass.materialized
                refine congrArg (fun x => MemoryInst.call callee
                  (dst.map fun d => ⟨d.1, { d.2 with storage := cls d.2.logical.root }⟩)
                  x checks) ?_
                apply List.map_congr_left
                rintro ⟨_, a⟩ _; cases a <;> rfl
            · cases ending with
              | jump e =>
                cases e with | mk t args =>
                refine congrArg (fun x => Terminator.jump ⟨t, x⟩) ?_
                apply List.map_congr_left; rintro ⟨_, a⟩ _; cases a <;> rfl
              | branch t y n =>
                cases y with | mk yt ya =>
                cases n with | mk nt na =>
                exact congr (congr (congrArg Terminator.branch (by cases t <;> rfl))
                  (congrArg (CFGEdge.mk yt) (List.map_congr_left fun ⟨_, rd⟩ _ => by cases rd <;> rfl)))
                  (congrArg (CFGEdge.mk nt) (List.map_congr_left fun ⟨_, rd⟩ _ => by cases rd <;> rfl))
              | ret r =>
                cases r with
                | none => rfl
                | some a => cases a with | mk _ rd => cases rd <;> rfl
              | trap _ _ => rfl
              | invalid => rfl
  rw [toLogicalGraph_eq_logicalView] at hPrepUnf
  refine ⟨by rw [hPrepUnf, AList.map, List.length_map, List.length_map], ?_⟩
  intro i fn locals gSrc gPrep hSrc hPrep
  rw [hPrepUnf, AList.map, List.getElem?_map, List.getElem?_map, hSrc] at hPrep
  simp only [Option.map_some, Option.some.injEq, Prod.mk.injEq] at hPrep
  obtain ⟨_, _, rfl⟩ := hPrep
  constructor
  · refine ⟨rfl, by simp [Array.size_map], ?_⟩
    intro j b1 b2 hj1 hj2
    rw [Array.getElem?_map, hj1] at hj2
    cases Option.some.inj hj2
    refine ⟨rfl, by simp [List.map_map], ?_, ?_⟩
    · cases hEnd : b1.ending <;> simp [hEnd, Terminator.successors]
    · intro r
      rw [List.map_map]
      apply List.map_congr_left
      intro op _
      cases op with
      | mk orig inst =>
        cases inst with
        | bind _ _ | assign _ _ | op _ _ _ _ _ | addWithOverflow _ _ _ _ _ | beginLifetime _ | endLifetime _ =>
          rfl
        | destructure _ bs =>
          simp only [Function.comp_def, memInstChange]
          rw [List.any_map]
          rfl
        | call _ dst _ _ => cases dst <;> rfl
  · intro bPrep hbPrep op hop root hdef
    simp only [Array.toList_map, List.mem_map] at hbPrep
    obtain ⟨bLog, hbLog, rfl⟩ := hbPrep
    simp only [toLogicalGraph, Array.toList_map, List.mem_map] at hbLog
    obtain ⟨bSrc, hbSrc, rfl⟩ := hbLog
    simp only [List.mem_map] at hop
    obtain ⟨opLog, hopLog, rfl⟩ := hop
    simp only [toLogicalBlock, List.mem_map] at hopLog
    obtain ⟨opSrc, hopSrc, rfl⟩ := hopLog
    refine ⟨bSrc, hbSrc, opSrc, hopSrc, ?_⟩
    cases hinst : opSrc.instruction <;> simp [toLogicalInst, toLogicalPlace, definitionRoot, mirDefinedRoot, hinst] at hdef ⊢ <;> exact hdef

/-! ### 5. Whole-MIR covering (`MIRCovering`) and constructive `LowersMemory` transport -/

/-- Function-indexed block-projection covering of `dst` onto `src`. -/
structure MIRCovering {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (proj : FnId → BlockId → BlockId) (src dst : MIR lang profile) : Prop where
  project : dst.project = src.project
  entry : dst.entry = src.entry
  signatures : dst.signatures = src.signatures
  declarations : dst.declarations = src.declarations
  mutableLocals : dst.mutableLocals = src.mutableLocals
  fnsLen : dst.functions.length = src.functions.length
  fns : ∀ (i : Nat) (e1 e2 : FnId × LocalTypes Ty × MIRGraph lang),
    src.functions[i]? = some e1 → dst.functions[i]? = some e2 →
    e2.1 = e1.1 ∧ e2.2.1 = e1.2.1 ∧ GraphCovering (proj e1.1) e1.2.2 e2.2.2

private theorem MIRCovering.inventories_eq {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    {proj : FnId → BlockId → BlockId} {src dst : MIR lang profile}
    (h : MIRCovering proj src dst) :
    List.map (fun e => (e.1, e.2.1)) dst.functions = List.map (fun e => (e.1, e.2.1)) src.functions := by
  apply List.ext_getElem?
  intro i
  rw [List.getElem?_map, List.getElem?_map]
  cases h1 : src.functions[i]? with
  | none =>
    have hge : src.functions.length ≤ i := List.getElem?_eq_none_iff.mp h1
    rw [List.getElem?_eq_none (h.fnsLen ▸ hge)]
  | some e1 =>
    have hlt : i < dst.functions.length := h.fnsLen.symm ▸ (List.getElem?_eq_some_iff.mp h1).1
    have h2 : dst.functions[i]? = some dst.functions[i] := List.getElem?_eq_getElem hlt
    rw [h2]
    obtain ⟨hf, hl, _⟩ := h.fns i e1 dst.functions[i] h1 h2
    simp only [Option.map_some, Option.some.injEq, Prod.mk.injEq]
    exact ⟨hf, hl⟩

private theorem MIRCovering.lookup_fn {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    {proj : FnId → BlockId → BlockId} {src dst : MIR lang profile}
    (h : MIRCovering proj src dst) (fn : FnId) :
    (AList.lookup src.functions fn = none ∧ AList.lookup dst.functions fn = none) ∨
    ∃ locals g1 g2,
      AList.lookup src.functions fn = some (locals, g1) ∧
      AList.lookup dst.functions fn = some (locals, g2) ∧
      GraphCovering (proj fn) g1 g2 := by
  have aux : ∀ (xs ys : AList FnId (LocalTypes Ty × MIRGraph lang)),
      ys.length = xs.length →
      (∀ (i : Nat) (e1 e2 : FnId × LocalTypes Ty × MIRGraph lang),
        xs[i]? = some e1 → ys[i]? = some e2 →
        e2.1 = e1.1 ∧ e2.2.1 = e1.2.1 ∧ GraphCovering (proj e1.1) e1.2.2 e2.2.2) →
      (AList.lookup xs fn = none ∧ AList.lookup ys fn = none) ∨
      ∃ locals g1 g2,
        AList.lookup xs fn = some (locals, g1) ∧
        AList.lookup ys fn = some (locals, g2) ∧
        GraphCovering (proj fn) g1 g2 := by
    intro xs ys
    induction xs generalizing ys with
    | nil =>
      intro hlen _
      cases ys with
      | nil => exact .inl ⟨rfl, rfl⟩
      | cons _ _ => cases hlen
    | cons e1 rest1 ih =>
      cases ys with
      | nil => intro hlen; cases hlen
      | cons e2 rest2 =>
        intro hlen hf
        obtain ⟨hfEq, hlEq, hcov⟩ := hf 0 e1 e2 rfl rfl
        simp only [AList.lookup, hfEq]
        by_cases heq : e1.1 = fn
        · subst heq
          simp only [↓reduceIte]
          exact .inr ⟨e1.2.1, e1.2.2, e2.2.2, rfl, congrArg some (Prod.ext hlEq rfl), hcov⟩
        · simp only [heq, ↓reduceIte]
          exact ih rest2 (Nat.succ.inj hlen) (fun i a b ha hb => hf (i + 1) a b ha hb)
  exact aux src.functions dst.functions h.fnsLen h.fns

private theorem MIRCovering.origins_iff {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    {proj : FnId → BlockId → BlockId} {src dst : MIR lang profile}
    (h : MIRCovering proj src dst) (o : Origin) :
    o ∈ dst.origins ↔ o ∈ src.origins := by
  unfold MIR.origins CFG.origins
  simp only [List.mem_flatMap]
  constructor
  · rintro ⟨e2, he2, b2, hb2, ho⟩
    obtain ⟨i, hi, rfl⟩ := List.getElem_of_mem he2
    have hget2 : dst.functions[i]? = some dst.functions[i] := List.getElem?_eq_getElem hi
    have hi1 : i < src.functions.length := h.fnsLen ▸ hi
    have hget1 : src.functions[i]? = some src.functions[i] := List.getElem?_eq_getElem hi1
    obtain ⟨_, _, hcov⟩ := h.fns i src.functions[i] dst.functions[i] hget1 hget2
    obtain ⟨b1, hb1, _, _, hbd, hbo, hend⟩ := hcov.forward b2 hb2
    refine ⟨src.functions[i], List.getElem_mem hi1, b1, hb1, ?_⟩
    rw [hbd, hbo, hend, mapTermTarget_origins]
    exact ho
  · rintro ⟨e1, he1, b1, hb1, ho⟩
    obtain ⟨i, hi, rfl⟩ := List.getElem_of_mem he1
    have hget1 : src.functions[i]? = some src.functions[i] := List.getElem?_eq_getElem hi
    have hi2 : i < dst.functions.length := h.fnsLen.symm ▸ hi
    have hget2 : dst.functions[i]? = some dst.functions[i] := List.getElem?_eq_getElem hi2
    obtain ⟨_, _, hcov⟩ := h.fns i src.functions[i] dst.functions[i] hget1 hget2
    obtain ⟨b2, hb2, _, _, hbd, hbo, hend⟩ := hcov.surjective b1 hb1
    refine ⟨dst.functions[i], List.getElem_mem hi2, b2, hb2, ?_⟩
    rw [hbd, hbo, hend, mapTermTarget_origins] at ho
    exact ho

private theorem MIRCovering.escapes_iff {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {proj : FnId → BlockId → BlockId} {src dst : MIR lang profile}
    (h : MIRCovering proj src dst) (fn : FnId) (place : PlaceId) :
    dst.Escapes (language := language) fn place ↔ src.Escapes (language := language) fn place := by
  unfold MIR.Escapes MIR.operations
  simp only [List.mem_flatMap]
  constructor
  · rintro ⟨op, ⟨e2, he2, b2, hb2, hop⟩, hfn, ty, path, haddr⟩
    obtain ⟨i, hi, rfl⟩ := List.getElem_of_mem he2
    have hget2 : dst.functions[i]? = some dst.functions[i] := List.getElem?_eq_getElem hi
    have hi1 : i < src.functions.length := h.fnsLen ▸ hi
    have hget1 : src.functions[i]? = some src.functions[i] := List.getElem?_eq_getElem hi1
    obtain ⟨hfEq, _, hcov⟩ := h.fns i src.functions[i] dst.functions[i] hget1 hget2
    obtain ⟨b1, hb1, _, _, hbd, _, _⟩ := hcov.forward b2 hb2
    rw [← hbd] at hop
    simp only [List.mem_filterMap] at hop
    obtain ⟨⟨idx, locOp⟩, hmemZip, hEq⟩ := hop
    refine ⟨{ op with block := b1.id }, ⟨src.functions[i], List.getElem_mem hi1, b1, hb1, ?_⟩, hfn, ty, path, haddr⟩
    refine List.mem_filterMap.mpr ⟨(idx, locOp), hmemZip, ?_⟩
    cases hinst : idx.instruction <;> simp [hinst] at hEq ⊢ <;> cases hEq <;> simp [hfEq]
  · rintro ⟨op, ⟨e1, he1, b1, hb1, hop⟩, hfn, ty, path, haddr⟩
    obtain ⟨i, hi, rfl⟩ := List.getElem_of_mem he1
    have hget1 : src.functions[i]? = some src.functions[i] := List.getElem?_eq_getElem hi
    have hi2 : i < dst.functions.length := h.fnsLen.symm ▸ hi
    have hget2 : dst.functions[i]? = some dst.functions[i] := List.getElem?_eq_getElem hi2
    obtain ⟨hfEq, _, hcov⟩ := h.fns i src.functions[i] dst.functions[i] hget1 hget2
    obtain ⟨b2, hb2, _, _, hbd, _, _⟩ := hcov.surjective b1 hb1
    rw [hbd] at hop
    simp only [List.mem_filterMap] at hop
    obtain ⟨⟨idx, locOp⟩, hmemZip, hEq⟩ := hop
    refine ⟨{ op with block := b2.id }, ⟨dst.functions[i], List.getElem_mem hi2, b2, hb2, ?_⟩, hfn, ty, path, haddr⟩
    refine List.mem_filterMap.mpr ⟨(idx, locOp), hmemZip, ?_⟩
    cases hinst : idx.instruction <;> simp [hinst] at hEq ⊢ <;> cases hEq <;> simp [hfEq]

private theorem MIRCovering.materializable {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {proj : FnId → BlockId → BlockId} {src dst : MIR lang profile}
    (h : MIRCovering proj src dst) (hMat : Materializable (language := language) src) :
    Materializable (language := language) dst := by
  intro fn locals g2 hMem2 place ty hLoc hNeed
  obtain ⟨i, hi, hEq⟩ := List.getElem_of_mem hMem2
  have hget2 : dst.functions[i]? = some (fn, locals, g2) := by rw [List.getElem?_eq_getElem hi, hEq]
  have hi1 : i < src.functions.length := h.fnsLen ▸ hi
  have hget1 : src.functions[i]? = some src.functions[i] := List.getElem?_eq_getElem hi1
  obtain ⟨hfEq, hlEq, _⟩ := h.fns i src.functions[i] (fn, locals, g2) hget1 hget2
  have hMem1 : (fn, locals, src.functions[i].2.2) ∈ src.functions := by
    have hm := List.getElem_mem hi1
    rcases he : src.functions[i] with ⟨f1, l1, g1⟩
    rw [he] at hfEq hlEq hm
    dsimp only at hfEq hlEq
    rw [← hfEq, ← hlEq] at hm
    exact hm
  have hNeed1 : (language.scalarType ty).isNone = true ∨ src.Escapes (language := language) fn place :=
    hNeed.imp id (h.escapes_iff fn place).mp
  rw [h.declarations]
  exact hMat fn locals src.functions[i].2.2 hMem1 place ty hLoc hNeed1

private theorem MIRCovering.logicalView_handoffWF {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {proj : FnId → BlockId → BlockId} {src dst : MIR lang profile}
    (h : MIRCovering proj src dst)
    (hWF : (src.logicalView (language := language)).HandoffWF) :
    (dst.logicalView (language := language)).HandoffWF := by
  have hInv : List.map (fun e => (e.1, e.2.1)) dst.functions =
      List.map (fun e => (e.1, e.2.1)) src.functions := h.inventories_eq
  have hFnKeys : List.map Prod.fst (dst.logicalView (language := language)).functions =
      List.map Prod.fst (src.logicalView (language := language)).functions := by
    simp only [MIR.logicalView, AList.map, List.map_map, Function.comp_def]
    have hc := congrArg (List.map Prod.fst) hInv
    simpa only [List.map_map, Function.comp_def] using hc
  have hStorKeys : List.map Prod.fst (dst.logicalView (language := language)).storage =
      List.map Prod.fst (src.logicalView (language := language)).storage := by
    simp only [MIR.logicalView, AList.map, List.map_map, Function.comp_def]
    have hc := congrArg (List.map Prod.fst) hInv
    simpa only [List.map_map, Function.comp_def] using hc
  have hStorEq : (dst.logicalView (language := language)).storage =
      (src.logicalView (language := language)).storage := by
    simp only [MIR.logicalView, AList.map]
    have hc := congrArg (List.map fun p : FnId × LocalTypes language.operands.Ty =>
      (p.1, List.map (fun e => (e.1, StorageClass.virtual)) p.2)) hInv
    simpa only [List.map_map, Function.comp_def] using hc
  refine ⟨⟨hFnKeys ▸ hWF.1.1, by change (List.map Prod.fst dst.signatures).Nodup; rw [h.signatures]; exact hWF.1.2.1,
    hStorKeys ▸ hWF.1.2.2.1, hWF.1.2.2.2.1, hStorEq ▸ hWF.1.2.2.2.2.1, hWF.1.2.2.2.2.2.1, ?_⟩,
    by change dst.entry ∈ List.map Prod.fst (dst.logicalView (language := language)).functions; rw [h.entry, hFnKeys]; exact hWF.2.1,
    ?_, hWF.2.2.2.1, by change DeclarationsWF dst.declarations; rw [h.declarations]; exact hWF.2.2.2.2.1, ?_⟩
  · intro fn locals g2Log hMem2
    rw [toLogicalGraph_eq_logicalView] at hMem2
    simp only [AList.map, List.mem_map] at hMem2
    obtain ⟨⟨fn', locals', g2⟩, hMemDst, hEq⟩ := hMem2
    cases hEq
    obtain ⟨i, hi, hGet2⟩ := List.getElem_of_mem hMemDst
    have hget2 : dst.functions[i]? = some (fn', locals', g2) := by rw [List.getElem?_eq_getElem hi, hGet2]
    have hi1 : i < src.functions.length := h.fnsLen ▸ hi
    have hget1 : src.functions[i]? = some src.functions[i] := List.getElem?_eq_getElem hi1
    obtain ⟨hfEq, hlEq, hcov⟩ := h.fns i src.functions[i] (fn', locals', g2) hget1 hget2
    have hMemSrcLog : (fn', locals', toLogicalGraph src.functions[i].2.2) ∈
        (src.logicalView (language := language)).functions := by
      rw [toLogicalGraph_eq_logicalView]
      simp only [AList.map, List.mem_map]
      refine ⟨src.functions[i], List.getElem_mem hi1, ?_⟩
      rcases he : src.functions[i] with ⟨f1, l1, g1⟩
      rw [he] at hfEq hlEq; exact Prod.ext hfEq.symm (Prod.ext hlEq.symm rfl)
    obtain ⟨hLocNd, _, hSigSrc⟩ := hWF.1.2.2.2.2.2.2 _ _ _ hMemSrcLog
    refine ⟨hLocNd, toLogicalGraph_wf.mpr hcov.dstWF, ?_⟩
    obtain ⟨sig, hSigLu, ⟨eb1, heb1, heb1Id, heb1Params⟩, hBlksSrc⟩ := hSigSrc
    obtain ⟨eb2, heb2, heb2Id⟩ := hcov.dstWF.2.1
    obtain ⟨b1ForEb2, hb1ForEb2, hb1Id, hb1Params, _, _, _⟩ := hcov.forward eb2 heb2
    have heb1Eq : toLogicalBlock b1ForEb2 = eb1 := by
      have h1 : (toLogicalGraph src.functions[i].2.2).blocks.find? (·.id == src.functions[i].2.2.entry) = some eb1 := by
        have hf := array_find?_of_mem_nodup (toLogicalGraph_wf.mpr hcov.srcWF).1 heb1
        rwa [heb1Id] at hf
      have h2 : (toLogicalGraph src.functions[i].2.2).blocks.find? (·.id == src.functions[i].2.2.entry) = some (toLogicalBlock b1ForEb2) := by
        have hMemLog : toLogicalBlock b1ForEb2 ∈ (toLogicalGraph src.functions[i].2.2).blocks.toList := by
          simp only [toLogicalGraph, Array.toList_map, List.mem_map]; exact ⟨b1ForEb2, hb1ForEb2, rfl⟩
        have hf := array_find?_of_mem_nodup (toLogicalGraph_wf.mpr hcov.srcWF).1 hMemLog
        have heqId : (toLogicalBlock b1ForEb2).id = src.functions[i].2.2.entry :=
          hb1Id.trans ((congrArg (proj src.functions[i].1) heb2Id).trans hcov.entry)
        rwa [heqId] at hf
      exact Option.some.inj (h2.symm.trans h1)
    refine ⟨sig, by change AList.lookup dst.signatures fn' = some sig; rw [h.signatures]; exact hSigLu,
      ⟨toLogicalBlock eb2, ?_, heb2Id, ?_⟩, ?_⟩
    · simp only [toLogicalGraph, Array.toList_map, List.mem_map]; exact ⟨eb2, heb2, rfl⟩
    · have hp : (toLogicalBlock eb2).params.map Sigma.fst = (toLogicalBlock b1ForEb2).params.map Sigma.fst := by
        simp [toLogicalBlock, hb1Params]
      rw [hp, heb1Eq]
      exact heb1Params
    · intro blk2 hblk2
      simp only [toLogicalGraph, Array.toList_map, List.mem_map] at hblk2
      obtain ⟨b2, hb2, rfl⟩ := hblk2
      obtain ⟨b1, hb1, _, _, hbd, _, hend⟩ := hcov.forward b2 hb2
      have hb1Log : toLogicalBlock b1 ∈ (toLogicalGraph src.functions[i].2.2).blocks.toList := by
        simp only [toLogicalGraph, Array.toList_map, List.mem_map]; exact ⟨b1, hb1, rfl⟩
      obtain ⟨hTerm1, hCalls1⟩ := hBlksSrc (toLogicalBlock b1) hb1Log
      constructor
      · dsimp only [toLogicalBlock] at hTerm1 ⊢
        cases hb2End : b2.ending with
        | jump _ => simp [toLogicalTerm]
        | branch _ _ _ => simp [toLogicalTerm]
        | ret _ =>
          rw [hb2End] at hend
          simp only [mapTermTarget] at hend
          rw [hend] at hTerm1
          simpa [toLogicalTerm] using hTerm1
        | trap _ _ => simp [toLogicalTerm]
        | invalid =>
          rw [hb2End] at hend
          simp only [mapTermTarget] at hend
          rw [hend] at hTerm1
          exact hTerm1
      · intro op hop
        have hBodyEq : (toLogicalBlock b2).body = (toLogicalBlock b1).body := by
          simp [toLogicalBlock, hbd]
        rw [hBodyEq] at hop
        have hc := hCalls1 op hop
        simpa [MIR.logicalView, h.signatures] using hc
  · intro o ho
    rw [logicalView_origins, h.origins_iff, ← logicalView_origins] at ho
    have hProj : (dst.logicalView (language := language)).project =
        (src.logicalView (language := language)).project := h.project
    rw [hProj]
    exact hWF.2.2.1 o ho
  · refine ⟨by change (List.map Prod.fst dst.mutableLocals).Nodup; rw [h.mutableLocals]; exact hWF.mutableLocalsWF.1, ?_⟩
    intro fn roots hm
    change (fn, roots) ∈ dst.mutableLocals at hm
    rw [h.mutableLocals] at hm
    obtain ⟨locals, _, hSrcFn, hRoots⟩ := hWF.mutableLocalsWF.2 fn roots hm
    have hPair : (fn, locals) ∈ List.map (fun e => (e.1, e.2.1)) (dst.logicalView (language := language)).functions := by
      rw [MIR.logicalView_inventories_eq, hInv, ← MIR.logicalView_inventories_eq]
      exact List.mem_map.mpr ⟨(fn, locals, _), hSrcFn, rfl⟩
    obtain ⟨⟨fn', locals', g'⟩, hMemDst, hEq⟩ := List.mem_map.mp hPair
    obtain ⟨rfl, rfl⟩ := Prod.mk.inj hEq
    exact ⟨locals', g', hMemDst, hRoots⟩

private theorem alist_lookup_of_mem_nodup {K V : Type} [DecidableEq K]
    {xs : AList K V} (hnd : (List.map Prod.fst xs).Nodup) {k : K} {v : V} (hm : (k, v) ∈ xs) :
    AList.lookup xs k = some v := by
  induction xs with
  | nil => cases hm
  | cons a rest ih =>
    obtain ⟨hnot, hrest⟩ := List.nodup_cons.mp hnd
    rcases List.mem_cons.mp hm with rfl | htail
    · simp [AList.lookup]
    · have hne : a.1 ≠ k := fun heq => hnot (List.mem_map.mpr ⟨(k, v), htail, heq.symm⟩)
      simp only [AList.lookup, hne, ↓reduceIte]
      exact ih hrest htail

private def repeatN {α : Type} (f : α → α) : Nat → α → α
  | 0, x => x
  | k + 1, x => f (repeatN f k x)

private theorem MIRCovering.of_specializeMIR {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    {mir : MIR lang profile}
    (hNd : (List.map Prod.fst mir.functions).Nodup)
    (hWF : ∀ fn locals graph, (fn, locals, graph) ∈ mir.functions →
      graph.WF Terminator.successors Sigma.fst Sigma.fst) :
    ∃ proj, MIRCovering proj mir (specializeMIR mir) := by
  let projFn : FnId → BlockId → BlockId := fun fn bid =>
    match AList.lookup mir.functions fn with
    | none => bid
    | some (locals, graph) => repeatN decodeBlockId (ambiguousRoots locals graph).length bid
  have hRepeat : ∀ (rs : List PlaceId) (g : MIRGraph lang),
      g.WF Terminator.successors Sigma.fst Sigma.fst →
      GraphCovering (repeatN decodeBlockId rs.length) g (splitGraphForRoots rs g) := by
    intro rs
    induction rs with
    | nil => intro g hg; exact GraphCovering.refl hg
    | cons r rest ih =>
      intro g hg
      have h1 := GraphCovering.of_splitGraphForRoot r hg
      have h2 := ih (splitGraphForRoot r g) h1.dstWF
      exact h1.trans h2
  refine ⟨projFn, ⟨rfl, rfl, rfl, rfl, rfl, by simp only [specializeMIR, AList.map, List.length_map], ?_⟩⟩
  intro i e1 e2 h1 h2
  simp only [specializeMIR, AList.map, List.getElem?_map, h1, Option.map_some, Option.some.injEq] at h2
  subst h2
  rcases e1 with ⟨fn, locals, graph⟩
  have hMem : (fn, locals, graph) ∈ mir.functions := List.mem_of_getElem? h1
  have hLu : AList.lookup mir.functions fn = some (locals, graph) :=
    alist_lookup_of_mem_nodup hNd hMem
  refine ⟨rfl, rfl, ?_⟩
  have hProjEq : projFn fn = repeatN decodeBlockId (ambiguousRoots locals graph).length := by
    funext bid
    simp only [projFn, hLu]
  rw [hProjEq]
  exact hRepeat (ambiguousRoots locals graph) graph (hWF fn locals graph hMem)

/-! ### 5b. `lowerGraph (specializeMIR mir)` succeeds for every `MIR.HandoffWF` -/

/-- `lowerGraph` succeeds on `specializeMIR mir` for every `MIR.HandoffWF` input:
`Materializable` guarantees `prepareGraph` succeeds, and `specializeMIR` eliminates
every ambiguous lifetime join without disturbing already-unambiguous roots. -/
theorem lowerGraph_specializeMIR_ok {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {mir : MIR lang profile}
    (hWF : mir.HandoffWF (language := language)) :
    ∃ memory, lowerGraph (language := language) (specializeMIR mir) = .ok memory := by
  obtain ⟨hLog, hMat⟩ := hWF
  have hNd : (List.map Prod.fst mir.functions).Nodup := by
    have h1 := hLog.1.1
    have hKeys : List.map Prod.fst (mir.logicalView (language := language)).functions =
        List.map Prod.fst mir.functions := by
      have hc := congrArg (List.map Prod.fst) (MIR.logicalView_inventories_eq (language := language) mir)
      simpa only [List.map_map, Function.comp_def] using hc
    rwa [hKeys] at h1
  have hGraphWF : ∀ fn locals graph, (fn, locals, graph) ∈ mir.functions →
      graph.WF Terminator.successors Sigma.fst Sigma.fst := by
    intro fn locals graph hm
    have hmLog : (fn, locals, toLogicalGraph graph) ∈ (mir.logicalView (language := language)).functions := by
      rw [toLogicalGraph_eq_logicalView]
      exact List.mem_map.mpr ⟨(fn, locals, graph), hm, rfl⟩
    exact toLogicalGraph_wf.mp (hLog.1.2.2.2.2.2.2 fn locals (toLogicalGraph graph) hmLog).2.1
  obtain ⟨proj, hCov⟩ := MIRCovering.of_specializeMIR hNd hGraphWF
  have hMat2 : Materializable (language := language) (specializeMIR mir) :=
    hCov.materializable hMat
  obtain ⟨prepared, hPrep⟩ := prepareGraph_ok_of_materializable hMat2
  obtain ⟨hLen, hShape⟩ := storageShapeEq_of_prepareGraph hPrep
  apply lowerGraph_ok_of_unambiguous hPrep
  intro fn locals gPrep hPrepMem bPrep hbPrep op hop root hDef
  obtain ⟨i, hiPrep, hEqPrep⟩ := List.getElem_of_mem hPrepMem
  have hGetPrep : prepared.functions[i]? = some (fn, locals, gPrep) := by
    rw [List.getElem?_eq_getElem hiPrep, hEqPrep]
  have hiSpec : i < (specializeMIR mir).functions.length := hLen ▸ hiPrep
  have hiOrig : i < mir.functions.length := by simpa only [specializeMIR, AList.map, List.length_map] using hiSpec
  rcases hOrigAt : mir.functions[i] with ⟨fn0, locals0, gOrig⟩
  have hGetOrig : mir.functions[i]? = some (fn0, locals0, gOrig) := by
    rw [List.getElem?_eq_getElem hiOrig, hOrigAt]
  have hGetSpec : (specializeMIR mir).functions[i]? =
      some (fn0, locals0, splitGraphForRoots (ambiguousRoots locals0 gOrig) gOrig) := by
    simp only [specializeMIR, AList.map, List.getElem?_map, hGetOrig, Option.map_some]
  have hEqSpecPair : fn0 = fn ∧ locals0 = locals := by
    -- Compare `(fn, locals)` at index `i` between `prepared.functions` and `(specializeMIR mir).functions`.
    have hPrepInv : List.map (fun e => (e.1, e.2.1)) prepared.functions =
        List.map (fun e => (e.1, e.2.1)) (specializeMIR mir).functions := by
      unfold prepareGraph at hPrep
      simp only [bind, Except.bind] at hPrep
      split at hPrep
      · cases hPrep
      · simp only [pure, Except.pure, Except.ok.injEq] at hPrep
        subst prepared
        dsimp only
        rw [List.map_map]
        have hc := MIR.logicalView_inventories_eq (language := language) (specializeMIR mir)
        simpa only [Function.comp_def] using hc
    have hAt := congrArg (fun l => l[i]?) hPrepInv
    simp only [List.getElem?_map, hGetPrep, hGetSpec, Option.map_some, Option.some.injEq, Prod.mk.injEq] at hAt
    exact ⟨hAt.1.symm, hAt.2.symm⟩
  obtain ⟨rfl, rfl⟩ := hEqSpecPair
  let gSpec := splitGraphForRoots (ambiguousRoots locals0 gOrig) gOrig
  obtain ⟨hSEq, hDefOrigSpec⟩ := hShape i fn0 locals0 gSpec gPrep hGetSpec hGetPrep
  obtain ⟨bSpec, hbSpec, opSpec, hopSpec, hMirDefSpec⟩ := hDefOrigSpec bPrep hbPrep op hop root hDef
  obtain ⟨_, _, hGCov⟩ := hCov.fns i (fn0, locals0, gOrig) (fn0, locals0, gSpec) hGetOrig hGetSpec
  obtain ⟨bOrig, hbOrig, _, _, hBodyEq, _, _⟩ := hGCov.forward bSpec hbSpec
  have hopOrig : opSpec ∈ bOrig.body := hBodyEq.symm ▸ hopSpec
  have hOrigMem : (fn0, locals0, gOrig) ∈ mir.functions := List.mem_of_getElem? hGetOrig
  have hOrigWF : gOrig.WF Terminator.successors Sigma.fst Sigma.fst := hGraphWF fn0 locals0 gOrig hOrigMem
  have hRootInLocals : root ∈ List.map Prod.fst locals0 := by
    have hSome : (AList.lookup locals0 root).isSome = true := by
      unfold mirDefinedRoot at hMirDefSpec
      cases hI : opSpec.instruction <;> rw [hI] at hMirDefSpec <;>
        simp only [Option.filter] at hMirDefSpec
      · split at hMirDefSpec
        · rename_i hCond; cases Option.some.inj hMirDefSpec; exact hCond
        · cases hMirDefSpec
      · cases hMirDefSpec
      · split at hMirDefSpec
        · rename_i hCond; cases Option.some.inj hMirDefSpec; exact hCond
        · cases hMirDefSpec
      · cases hMirDefSpec
      · cases hMirDefSpec
      · cases hMirDefSpec
    cases hLu : AList.lookup locals0 root with
    | none => rw [hLu] at hSome; cases hSome
    | some ty =>
      have auxMem : ∀ (xs : LocalTypes language.operands.Ty), AList.lookup xs root = some ty →
          root ∈ List.map Prod.fst xs := by
        intro xs hlu
        induction xs with
        | nil => cases hlu
        | cons e rest ih =>
          simp only [AList.lookup] at hlu
          by_cases he : e.1 = root
          · subst he; exact .head _
          · simp only [he, ↓reduceIte] at hlu
            exact .tail _ (ih hlu)
      exact auxMem locals0 hLu
  have hInitRoot : root ∈ ambiguousRoots locals0 gOrig ∨
      ∀ b ∈ (toLogicalGraph gOrig).blocks.toList, RootUnambiguousAt (toLogicalGraph gOrig) b root := by
    cases hAmb : hasAmbiguousJoin (toLogicalGraph gOrig) root with
    | false => exact .inr (fun b hb => rootUnambiguousAt_of_hasAmbiguousJoin_false hAmb hb)
    | true =>
      refine .inl (List.mem_filter.mpr ⟨hRootInLocals, ?_⟩)
      have hAnyDef : gOrig.blocks.any
          (fun b => b.body.any fun op => mirDefinedRoot locals0 op.instruction == some root) = true :=
        Array.any_eq_true'.mpr ⟨bOrig, Array.mem_toList_iff.mp hbOrig,
          List.any_eq_true.mpr ⟨opSpec, hopOrig, by simp [hMirDefSpec]⟩⟩
      simp only [hAnyDef, hAmb, Bool.and_self]
  have hUnambSpec : ∀ b ∈ (toLogicalGraph gSpec).blocks.toList,
      RootUnambiguousAt (toLogicalGraph gSpec) b root :=
    rootUnambiguousAt_splitGraphForRoots (ambiguousRoots locals0 gOrig) hOrigWF hInitRoot
  obtain ⟨j, hjLt, hEqBlk⟩ := Array.getElem_of_mem (Array.mem_toList_iff.mp hbPrep)
  have hGet2 : gPrep.blocks[j]? = some bPrep := by rw [Array.getElem?_eq_getElem hjLt, hEqBlk]
  have hjLt1 : j < (toLogicalGraph gSpec).blocks.size := hSEq.size.symm ▸ hjLt
  have hGet1 : (toLogicalGraph gSpec).blocks[j]? = some (toLogicalGraph gSpec).blocks[j] :=
    Array.getElem?_eq_getElem hjLt1
  have hMem1 : (toLogicalGraph gSpec).blocks[j] ∈ (toLogicalGraph gSpec).blocks.toList :=
    Array.mem_toList_iff.mpr (Array.getElem_mem hjLt1)
  exact storageShapeEq_rootUnambiguousAt hSEq (toLogicalGraph_wf.mpr hGCov.dstWF) hGet1 hGet2
    (hUnambSpec _ hMem1)

/-! ### 5c. Exact bisimulation for `MIRCovering` and constructive `LowersMemory` transport -/

private structure TablesAgree {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    (m1 m2 : Interfaces.Memory lang profile) : Prop where
  declarations : m1.declarations = m2.declarations
  project : m1.project = m2.project
  entry : m1.entry = m2.entry
  signatures : m1.signatures = m2.signatures
  layouts : m1.layouts = m2.layouts
  mutableLocals : m1.mutableLocals = m2.mutableLocals
  storage : m1.storage = m2.storage
  inventories : List.map (fun e => (e.1, e.2.1)) m1.functions =
    List.map (fun e => (e.1, e.2.1)) m2.functions

private theorem TablesAgree.symm {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {m1 m2 : Interfaces.Memory lang profile} (h : TablesAgree m1 m2) : TablesAgree m2 m1 where
  declarations := h.declarations.symm
  project := h.project.symm
  entry := h.entry.symm
  signatures := h.signatures.symm
  layouts := h.layouts.symm
  mutableLocals := h.mutableLocals.symm
  storage := h.storage.symm
  inventories := h.inventories.symm

private theorem MIRCovering.logicalView_tablesAgree {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {proj : FnId → BlockId → BlockId} {src dst : MIR lang profile}
    (h : MIRCovering proj src dst) :
    TablesAgree (src.logicalView (language := language)) (dst.logicalView (language := language)) where
  declarations := h.declarations.symm
  project := h.project.symm
  entry := h.entry.symm
  signatures := h.signatures.symm
  layouts := rfl
  mutableLocals := h.mutableLocals.symm
  storage := by
    simp only [MIR.logicalView, AList.map]
    have hc := congrArg (List.map fun p : FnId × LocalTypes language.operands.Ty =>
      (p.1, List.map (fun e => (e.1, StorageClass.virtual)) p.2)) h.inventories_eq.symm
    simpa only [List.map_map, Function.comp_def] using hc
  inventories := by
    rw [MIR.logicalView_inventories_eq, MIR.logicalView_inventories_eq]
    exact h.inventories_eq.symm

private def swapCtx {language : CoreLanguage} {profile : Profile}
    (blk : BlockId) (cs : List (MemoryContinuation language.operands.Ty language.operands.Val))
    (s : MemoryRegisters language profile) : MemoryRegisters language profile :=
  { s with
    toMemoryFrame := { s.toMemoryFrame with block := blk }
    callers := cs }

private theorem swapCtx_inv {language : CoreLanguage} {profile : Profile}
    (blk : BlockId) (cs : List (MemoryContinuation language.operands.Ty language.operands.Val))
    (s : MemoryRegisters language profile) :
    swapCtx s.block s.callers (swapCtx blk cs s) = s := by
  cases s with | mk f _ _ _ _ _ _ => cases f; rfl

private theorem swapCtx_advance {language : CoreLanguage} {profile : Profile}
    (blk : BlockId) (cs : List (MemoryContinuation language.operands.Ty language.operands.Val))
    (s : MemoryRegisters language profile) :
    (swapCtx blk cs s).advance = swapCtx blk cs s.advance := by
  cases s with | mk f _ _ _ _ _ _ => cases f; rfl

private theorem swapCtx_markWritten {language : CoreLanguage} {profile : Profile}
    (blk : BlockId) (cs : List (MemoryContinuation language.operands.Ty language.operands.Val))
    (s : MemoryRegisters language profile) {ty : language.operands.Ty}
    (place : MemoryPlace _ ty) (owners : Own.State) :
    Interfaces.Memory.markWritten (swapCtx blk cs s) place owners =
      swapCtx blk cs (Interfaces.Memory.markWritten s place owners) := by
  cases s with | mk f _ _ _ _ _ _ => cases f; rfl

private theorem TablesAgree.rootType_eq {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {m1 m2 : Interfaces.Memory lang profile} (h : TablesAgree m1 m2) (fn : FnId) (root : PlaceId) :
    m1.rootType fn root = m2.rootType fn root := by
  have hLu : (AList.lookup m1.functions fn).map Prod.fst = (AList.lookup m2.functions fn).map Prod.fst := by
    have aux : ∀ (xs ys : AList FnId (LocalTypes language.operands.Ty × MemoryGraph lang)),
        List.map (fun e => (e.1, e.2.1)) xs = List.map (fun e => (e.1, e.2.1)) ys →
        (AList.lookup xs fn).map Prod.fst = (AList.lookup ys fn).map Prod.fst := by
      intro xs ys heq
      induction xs generalizing ys with
      | nil => cases ys with | nil => rfl | cons _ _ => cases heq
      | cons a rest ih =>
        cases ys with
        | nil => cases heq
        | cons b rest' =>
          simp only [List.map_cons, List.cons.injEq, Prod.mk.injEq] at heq
          obtain ⟨⟨hf, hl⟩, htail⟩ := heq
          simp only [AList.lookup, hf]
          by_cases hfn : b.1 = fn
          · simp only [hfn, ↓reduceIte, Option.map_some, hl]
          · simp only [hfn, ↓reduceIte]; exact ih rest' htail
    exact aux m1.functions m2.functions h.inventories
  unfold Interfaces.Memory.rootType
  cases h1 : AList.lookup m1.functions fn <;> cases h2 : AList.lookup m2.functions fn <;>
    simp [h1, h2] at hLu ⊢
  rw [hLu]

private theorem TablesAgree.classAt_eq {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {m1 m2 : Interfaces.Memory lang profile} (h : TablesAgree m1 m2) (fn : FnId) (root : PlaceId) :
    m1.classAt fn root = m2.classAt fn root := by
  unfold Interfaces.Memory.classAt; rw [h.storage]

private theorem TablesAgree.layoutAt_eq {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {m1 m2 : Interfaces.Memory lang profile} (h : TablesAgree m1 m2) (fn : FnId) (root : PlaceId) :
    m1.layoutAt fn root = m2.layoutAt fn root := by
  unfold Interfaces.Memory.layoutAt; rw [h.layouts]

private theorem TablesAgree.zeroRoot_eq {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {m1 m2 : Interfaces.Memory lang profile} (h : TablesAgree m1 m2) (fn : FnId) (root : PlaceId) :
    m1.zeroRoot fn root = m2.zeroRoot fn root := by
  unfold Interfaces.Memory.zeroRoot; rw [h.layoutAt_eq]

private theorem TablesAgree.validPlace_eq {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {m1 m2 : Interfaces.Memory lang profile} (h : TablesAgree m1 m2)
    (blk : BlockId) (cs : List (MemoryContinuation language.operands.Ty language.operands.Val))
    (s : MemoryRegisters language profile) {ty : language.operands.Ty} (p : MemoryPlace _ ty) :
    m2.validPlace (swapCtx blk cs s) p = m1.validPlace s p := by
  unfold Interfaces.Memory.validPlace
  apply decide_eq_decide.mpr
  dsimp only [swapCtx]
  rw [← h.classAt_eq, ← h.rootType_eq]

private theorem TablesAgree.rangeAt_eq {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {m1 m2 : Interfaces.Memory lang profile} (h : TablesAgree m1 m2)
    (blk : BlockId) (cs : List (MemoryContinuation language.operands.Ty language.operands.Val))
    (s : MemoryRegisters language profile) {ty : language.operands.Ty} (p : Place _ ty) :
    m2.rangeAt (swapCtx blk cs s) p = m1.rangeAt s p := by
  unfold Interfaces.Memory.rangeAt
  dsimp only [swapCtx]
  rw [← h.layoutAt_eq]

private theorem TablesAgree.ownerView_eq {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {m1 m2 : Interfaces.Memory lang profile} (h : TablesAgree m1 m2)
    (blk : BlockId) (cs : List (MemoryContinuation language.operands.Ty language.operands.Val))
    (s : MemoryRegisters language profile) (r : PlaceId) (ty : language.operands.Ty) (st : Own.Status) :
    m2.ownerView (swapCtx blk cs s) r ty st = m1.ownerView s r ty st := by
  unfold Interfaces.Memory.ownerView
  dsimp only [swapCtx]
  rw [← h.mutableLocals]

private theorem TablesAgree.loadMaterialized_eq {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {m1 m2 : Interfaces.Memory lang profile} (h : TablesAgree m1 m2)
    (blk : BlockId) (cs : List (MemoryContinuation language.operands.Ty language.operands.Val))
    (s : MemoryRegisters language profile) {ty : language.operands.Ty} (p : MemoryPlace _ ty)
    (tgt : Target) (c : Nat) :
    m2.loadMaterialized (swapCtx blk cs s) p tgt c = m1.loadMaterialized s p tgt c := by
  unfold Interfaces.Memory.loadMaterialized
  rw [h.rangeAt_eq]; rfl

private theorem TablesAgree.loadBacking_eq {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {m1 m2 : Interfaces.Memory lang profile} (h : TablesAgree m1 m2)
    (blk : BlockId) (cs : List (MemoryContinuation language.operands.Ty language.operands.Val))
    (s : MemoryRegisters language profile) {ty : language.operands.Ty} (p : MemoryPlace _ ty) (c : Nat) :
    m2.loadBacking (swapCtx blk cs s) p c = m1.loadBacking s p c := by
  unfold Interfaces.Memory.loadBacking
  rw [h.validPlace_eq, h.zeroRoot_eq, h.loadMaterialized_eq]; rfl

private theorem TablesAgree.loadValue_eq {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {m1 m2 : Interfaces.Memory lang profile} (h : TablesAgree m1 m2)
    (blk : BlockId) (cs : List (MemoryContinuation language.operands.Ty language.operands.Val))
    (s : MemoryRegisters language profile) {ty : language.operands.Ty} (p : MemoryPlace _ ty) (c : Nat) :
    m2.loadValue (swapCtx blk cs s) p c = m1.loadValue s p c := by
  unfold Interfaces.Memory.loadValue
  rw [h.loadBacking_eq]; rfl

private theorem TablesAgree.storesValue_iff {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {m1 m2 : Interfaces.Memory lang profile} (h : TablesAgree m1 m2)
    (blk : BlockId) (cs : List (MemoryContinuation language.operands.Ty language.operands.Val))
    (s : MemoryRegisters language profile) {ty : language.operands.Ty} (p : MemoryPlace _ ty)
    (v : language.operands.Val) (c : Nat) :
    m2.StoresValue (swapCtx blk cs s) p v c ↔ m1.StoresValue s p v c := by
  unfold Interfaces.Memory.StoresValue
  rw [h.loadValue_eq, h.zeroRoot_eq, h.rangeAt_eq]; rfl

private theorem TablesAgree.discardRemainder_eq {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {m1 m2 : Interfaces.Memory lang profile} (h : TablesAgree m1 m2)
    (blk : BlockId) (cs : List (MemoryContinuation language.operands.Ty language.operands.Val))
    (s : MemoryRegisters language profile) {ty : language.operands.Ty} (p : MemoryPlace _ ty)
    (allowCopy : Bool) (c : Nat) :
    m2.discardRemainder (swapCtx blk cs s) p allowCopy c = m1.discardRemainder s p allowCopy c := by
  unfold Interfaces.Memory.discardRemainder
  rw [h.loadBacking_eq, h.declarations]; rfl

private theorem TablesAgree.mayInitialize_eq {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {m1 m2 : Interfaces.Memory lang profile} (h : TablesAgree m1 m2)
    (blk : BlockId) (cs : List (MemoryContinuation language.operands.Ty language.operands.Val))
    (s : MemoryRegisters language profile) {ty : language.operands.Ty} (p : MemoryPlace _ ty) (c : Nat) :
    m2.mayInitialize (swapCtx blk cs s) p c = m1.mayInitialize s p c := by
  unfold Interfaces.Memory.mayInitialize
  rw [h.discardRemainder_eq]; rfl

private theorem TablesAgree.writeOwnerView_eq {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {m1 m2 : Interfaces.Memory lang profile} (h : TablesAgree m1 m2)
    (blk : BlockId) (cs : List (MemoryContinuation language.operands.Ty language.operands.Val))
    (s : MemoryRegisters language profile) {ty : language.operands.Ty} (p : MemoryPlace _ ty) (c : Nat) :
    m2.writeOwnerView (swapCtx blk cs s) p c = m1.writeOwnerView s p c := by
  unfold Interfaces.Memory.writeOwnerView
  rw [h.discardRemainder_eq, h.ownerView_eq]; rfl

private theorem TablesAgree.authorizeWrite_eq {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {m1 m2 : Interfaces.Memory lang profile} (h : TablesAgree m1 m2)
    (blk : BlockId) (cs : List (MemoryContinuation language.operands.Ty language.operands.Val))
    (s : MemoryRegisters language profile) {ty : language.operands.Ty} (p : MemoryPlace _ ty) (c : Nat) :
    m2.authorizeWrite (swapCtx blk cs s) p c = m1.authorizeWrite s p c := by
  unfold Interfaces.Memory.authorizeWrite
  rw [h.writeOwnerView_eq, h.discardRemainder_eq]

private theorem TablesAgree.writeValue_eq {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {m1 m2 : Interfaces.Memory lang profile} (h : TablesAgree m1 m2)
    (blk : BlockId) (cs : List (MemoryContinuation language.operands.Ty language.operands.Val))
    (s : MemoryRegisters language profile) {ty : language.operands.Ty} (p : MemoryPlace _ ty)
    (v : language.operands.Val) (c : Nat) :
    m2.writeValue (swapCtx blk cs s) p v c = (m1.writeValue s p v c).map (swapCtx blk cs) := by
  unfold Interfaces.Memory.writeValue
  have hLive : (swapCtx blk cs s).live = s.live := rfl
  have hMay := h.mayInitialize_eq blk cs s p c
  have hAuth := h.authorizeWrite_eq blk cs s p c
  simp only [hLive, hMay, hAuth]
  cases hCond : (!s.live.contains p.logical.root || !m1.mayInitialize s p c) with
  | true => simp
  | false =>
    simp only [Bool.false_eq_true, ↓reduceIte, bind, Option.bind]
    cases ha : m1.authorizeWrite s p c with
    | none => rfl
    | some owners =>
      simp only [swapCtx_markWritten]
      cases hst : p.storage with
      | virtual =>
        have hV : (swapCtx blk cs (Interfaces.Memory.markWritten s p owners)).virtualValues =
            (Interfaces.Memory.markWritten s p owners).virtualValues := rfl
        rw [hV]
        cases hrep : Interfaces.Memory.replaceLocal (language := language)
            (Interfaces.Memory.markWritten s p owners).virtualValues p.logical v with
        | none => rfl
        | some tbl =>
          simp only [pure, Option.map_some]
          cases Interfaces.Memory.markWritten s p owners with | mk f _ _ _ _ _ _ => cases f; rfl
      | materialized =>
        have hz : m2.zeroRoot (swapCtx blk cs (Interfaces.Memory.markWritten s p owners)).frame p.logical.root =
            m1.zeroRoot (Interfaces.Memory.markWritten s p owners).frame p.logical.root := (h.zeroRoot_eq _ _).symm
        rw [hz]
        cases hzCond : m1.zeroRoot (Interfaces.Memory.markWritten s p owners).frame p.logical.root with
        | true =>
          simp only [↓reduceIte]
          have hZ : (swapCtx blk cs (Interfaces.Memory.markWritten s p owners)).zeroValues =
              (Interfaces.Memory.markWritten s p owners).zeroValues := rfl
          rw [hZ]
          cases hrep : Interfaces.Memory.replaceLocal (language := language)
              (Interfaces.Memory.markWritten s p owners).zeroValues p.logical v with
          | none => rfl
          | some tbl =>
            simp only [pure, Option.map_some]
            cases Interfaces.Memory.markWritten s p owners with | mk f _ _ _ _ _ _ => cases f; rfl
        | false =>
          simp only [Bool.false_eq_true, ↓reduceIte]
          rw [h.rangeAt_eq]
          cases hrg : m1.rangeAt (Interfaces.Memory.markWritten s p owners) p.logical with
          | none => rfl
          | some rg =>
            dsimp only
            cases hbr : Interfaces.Memory.byteRuns profile.memoryTarget ty v with
            | none => rfl
            | some runs =>
              dsimp only
              have hB : (swapCtx blk cs (Interfaces.Memory.markWritten s p owners)).bytes =
                  (Interfaces.Memory.markWritten s p owners).bytes := rfl
              rw [hB]
              cases hsr : Interfaces.Memory.storeRuns profile.memoryTarget c rg runs
                  (Interfaces.Memory.markWritten s p owners).bytes with
              | none => rfl
              | some bs =>
                simp only [pure, Option.map_some]
                cases Interfaces.Memory.markWritten s p owners with | mk f _ _ _ _ _ _ => cases f; rfl

private theorem TablesAgree.readValue_eq {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {m1 m2 : Interfaces.Memory lang profile} (h : TablesAgree m1 m2)
    (blk : BlockId) (cs : List (MemoryContinuation language.operands.Ty language.operands.Val))
    (s : MemoryRegisters language profile) {ty : language.operands.Ty}
    (rd : MemoryRead _ lang.literal ty) :
    m2.readValue (swapCtx blk cs s) rd =
      (m1.readValue s rd).map fun (v, next) => (v, swapCtx blk cs next) := by
  cases rd with
  | literal lit =>
    unfold Interfaces.Memory.readValue
    simp only [bind, Option.bind, pure]
    cases language.literal ty lit.encoding <;> rfl
  | copy place =>
    unfold Interfaces.Memory.readValue
    dsimp only
    rw [h.ownerView_eq, h.loadValue_eq]
    cases m1.ownerView s place.logical.root ty .owned
    rename_i ow id
    by_cases hCond : !(language.operands.isCopy ty && Own.ownerAccess ow id .shared)
    · simp [hCond]
    · simp only [hCond, Bool.false_eq_true, ↓reduceIte, bind, Option.bind, pure]
      cases m1.loadValue s place <;> rfl
  | move place =>
    unfold Interfaces.Memory.readValue
    dsimp only
    rw [h.ownerView_eq, h.loadValue_eq]
    cases m1.ownerView s place.logical.root ty .owned
    rename_i ow id
    simp only [bind, Option.bind, pure]
    cases ho : Own.move ow id with
    | none => rfl
    | some ow' =>
      cases hv : m1.loadValue s place with
      | none => rfl
      | some v =>
        by_cases hPath : !place.logical.path.isEmpty
        · simp only [hPath, ↓reduceIte, Option.map_some]
          cases s with | mk f _ _ _ _ _ _ => cases f; rfl
        · simp only [hPath, Bool.false_eq_true, ↓reduceIte, Option.map_some]
          with_unfolding_all
            cases s with | mk f _ _ _ _ _ _ => cases f; rfl

private theorem TablesAgree.readArgs_eq {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {m1 m2 : Interfaces.Memory lang profile} (h : TablesAgree m1 m2)
    (blk : BlockId) (cs : List (MemoryContinuation language.operands.Ty language.operands.Val))
    {types : List language.operands.Ty} (args : Args (MemoryRead _ lang.literal) types)
    (s : MemoryRegisters language profile) :
    m2.readArgs args (swapCtx blk cs s) =
      (m1.readArgs args s).map fun (vs, next) => (vs, swapCtx blk cs next) := by
  induction args generalizing s with
  | nil => rfl
  | cons rd rest ih =>
    simp only [Interfaces.Memory.readArgs, h.readValue_eq, bind, Option.bind]
    cases m1.readValue s rd with
    | none => rfl
    | some pair =>
      rcases pair with ⟨v, s1⟩
      simp only [Option.map_some, ih s1]
      cases m1.readArgs rest s1 with
      | none => rfl
      | some pair2 => rfl

private theorem TablesAgree.readOperands_eq {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {m1 m2 : Interfaces.Memory lang profile} (h : TablesAgree m1 m2)
    (blk : BlockId) (cs : List (MemoryContinuation language.operands.Ty language.operands.Val))
    (args : List (Sigma (MemoryRead _ lang.literal)))
    (s : MemoryRegisters language profile) :
    m2.readOperands args (swapCtx blk cs s) =
      (m1.readOperands args s).map fun (vs, next) => (vs, swapCtx blk cs next) := by
  induction args generalizing s with
  | nil => rfl
  | cons a rest ih =>
    rcases a with ⟨ty, rd⟩
    simp only [Interfaces.Memory.readOperands, h.readValue_eq, bind, Option.bind]
    cases m1.readValue s rd with
    | none => rfl
    | some pair =>
      rcases pair with ⟨v, s1⟩
      simp only [Option.map_some, ih s1]
      cases m1.readOperands rest s1 with
      | none => rfl
      | some pair2 => rfl

private theorem TablesAgree.freshLayoutsFrom_eq {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {m1 m2 : Interfaces.Memory lang profile} (h : TablesAgree m1 m2)
    (fn : FnId) (live : List PlaceId) (roots : List PlaceId) :
    m2.freshLayoutsFrom fn live roots = m1.freshLayoutsFrom fn live roots := by
  induction roots generalizing live with
  | nil => rfl
  | cons r rest ih =>
    unfold Interfaces.Memory.freshLayoutsFrom
    rw [ih, ih (r :: live), h.classAt_eq, h.layoutAt_eq]

private theorem TablesAgree.paramSpaceFits_iff {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {m1 m2 : Interfaces.Memory lang profile} (h : TablesAgree m1 m2)
    (blk : BlockId) (cs : List (MemoryContinuation language.operands.Ty language.operands.Val))
    (ps : List (Sigma (MemoryPlace language.operands.Ty)))
    (s : MemoryRegisters language profile) (c : Nat) :
    m2.ParamSpaceFits ps (swapCtx blk cs s) c ↔ m1.ParamSpaceFits ps s c := by
  unfold Interfaces.Memory.ParamSpaceFits Interfaces.Memory.freshLayouts
  change Interfaces.Memory.ReservationsFit _ _ (m2.freshLayoutsFrom s.frame s.live _) s.bytes ↔ _
  rw [h.freshLayoutsFrom_eq]

private theorem TablesAgree.transferCeiling_eq {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {m1 m2 : Interfaces.Memory lang profile} (h : TablesAgree m1 m2)
    (blk : BlockId) (cs : List (MemoryContinuation language.operands.Ty language.operands.Val))
    (s : MemoryRegisters language profile) (rs : List PlaceId) :
    m2.transferCeiling (swapCtx blk cs s) rs = m1.transferCeiling s rs := by
  unfold Interfaces.Memory.transferCeiling Interfaces.Memory.freshLayouts
  change _ + _ + ((m2.freshLayoutsFrom s.frame s.live rs).map _).sum = _
  rw [h.freshLayoutsFrom_eq]

private theorem TablesAgree.begins {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {m1 m2 : Interfaces.Memory lang profile} (h : TablesAgree m1 m2)
    (blk : BlockId) (cs : List (MemoryContinuation language.operands.Ty language.operands.Val))
    {s next : MemoryRegisters language profile} {r : PlaceId} {c : Nat}
    (hb : m1.Begins s r next c) :
    m2.Begins (swapCtx blk cs s) r (swapCtx blk cs next) c := by
  obtain ⟨hFresh, hCase⟩ := hb
  refine ⟨hFresh, ?_⟩
  rcases hCase with ⟨hV, rfl⟩ | ⟨hM, lay, hLay, ⟨hZ, rfl⟩ | ⟨hPos, sp, base, bid, bs, hAlloc, rfl⟩⟩
  · left; exact ⟨(h.classAt_eq _ _).symm ▸ hV, by cases s with | mk f _ _ _ _ _ _ => cases f; rfl⟩
  · right
    refine ⟨(h.classAt_eq _ _).symm ▸ hM, lay, (h.layoutAt_eq _ _).symm ▸ hLay, .inl ⟨hZ, ?_⟩⟩
    cases s with | mk f _ _ _ _ _ _ => cases f; rfl
  · right
    refine ⟨(h.classAt_eq _ _).symm ▸ hM, lay, (h.layoutAt_eq _ _).symm ▸ hLay,
      .inr ⟨hPos, sp, base, bid, bs, hAlloc, ?_⟩⟩
    cases s with | mk f _ _ _ _ _ _ => cases f; rfl

private theorem TablesAgree.writes {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {m1 m2 : Interfaces.Memory lang profile} (h : TablesAgree m1 m2)
    (blk : BlockId) (cs : List (MemoryContinuation language.operands.Ty language.operands.Val))
    {s next : MemoryRegisters language profile} {ty : language.operands.Ty}
    {p : MemoryPlace _ ty} {v : language.operands.Val} {c : Nat}
    (hw : m1.Writes s p v next c) :
    m2.Writes (swapCtx blk cs s) p v (swapCtx blk cs next) c := by
  obtain ⟨⟨hV, hInit⟩, hMat, hW, hSt⟩ := hw
  refine ⟨⟨(h.validPlace_eq blk cs s p).symm ▸ hV, hInit⟩, ?_, ?_, (h.storesValue_iff blk cs next p v c).mpr hSt⟩
  · intro hst; obtain ⟨lay, hLay, hZ⟩ := hMat hst
    exact ⟨lay, (h.layoutAt_eq _ _).symm ▸ hLay, hZ⟩
  · rw [h.writeValue_eq, hW]; rfl

private theorem TablesAgree.resident_eq {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {m1 m2 : Interfaces.Memory lang profile} (h : TablesAgree m1 m2)
    (blk : BlockId) (cs : List (MemoryContinuation language.operands.Ty language.operands.Val))
    (s : MemoryRegisters language profile) (r : PlaceId) (rg : Hybrid.Range) :
    m2.resident (swapCtx blk cs s) r rg = m1.resident s r rg := by
  unfold Interfaces.Memory.resident
  have hl : m2.layoutAt (swapCtx blk cs s).frame r = m1.layoutAt s.frame r := (h.layoutAt_eq _ _).symm
  have hb : (swapCtx blk cs s).bytes = s.bytes := rfl
  simp only [hl, hb]

private theorem TablesAgree.transferCheckpoint_iff {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {m1 m2 : Interfaces.Memory lang profile} (h : TablesAgree m1 m2)
    (blk : BlockId) (cs : List (MemoryContinuation language.operands.Ty language.operands.Val))
    (s : MemoryRegisters language profile) (c : Nat) :
    m2.TransferCheckpoint (swapCtx blk cs s) c ↔ m1.TransferCheckpoint s c := by
  unfold Interfaces.Memory.TransferCheckpoint
  constructor <;> rintro ⟨hr, hi⟩ <;> refine ⟨fun e he => ?_, hi⟩
  · rw [← h.resident_eq blk cs s e.1 e.2]; exact hr e he
  · rw [h.resident_eq blk cs s e.1 e.2]; exact hr e he

private theorem TablesAgree.writeParams {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {m1 m2 : Interfaces.Memory lang profile} (h : TablesAgree m1 m2)
    (blk : BlockId) (cs : List (MemoryContinuation language.operands.Ty language.operands.Val))
    {ps vs} {s next : MemoryRegisters language profile} {ex : Bool} {c : Nat}
    (hw : m1.WriteParams ps vs s next ex c) :
    m2.WriteParams ps vs (swapCtx blk cs s) (swapCtx blk cs next) ex c := by
  induction hw with
  | nil _ => exact .nil _
  | @cons ty place params value values st started stored nxt ex' c' hStart hW _ ih =>
    refine .cons ?_ (h.writes blk cs hW) ih
    change (if st.live.contains place.logical.root then _ else _)
    split
    · rename_i hIn
      simp only [hIn, ↓reduceIte] at hStart
      subst hStart; rfl
    · rename_i hNot
      simp only [hNot] at hStart
      exact h.begins blk cs hStart
  | exhausted hNoFit =>
    exact .exhausted (fun hFit => hNoFit ((h.paramSpaceFits_iff blk cs _ _ _).mp hFit))

private theorem TablesAgree.paramOutcome {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {m1 m2 : Interfaces.Memory lang profile} (h : TablesAgree m1 m2)
    (blk : BlockId) (cs : List (MemoryContinuation language.operands.Ty language.operands.Val))
    {ps vs} {s next : MemoryRegisters language profile} {ex : Bool}
    (ho : m1.ParamOutcome ps vs s next ex) :
    m2.ParamOutcome ps vs (swapCtx blk cs s) (swapCtx blk cs next) ex := by
  cases ex with
  | false => exact h.writeParams blk cs ho
  | true =>
    obtain ⟨hWP, ⟨hTC, nxt, hW2⟩, hNoFit⟩ := ho
    refine ⟨h.writeParams blk cs hWP, ?_, fun hFit => hNoFit ((h.paramSpaceFits_iff blk cs _ _ _).mp hFit)⟩
    refine ⟨?_, swapCtx blk cs nxt, ?_⟩
    · rw [h.transferCeiling_eq]; exact (h.transferCheckpoint_iff blk cs s _).mpr hTC
    · rw [h.transferCeiling_eq]; exact h.writeParams blk cs hW2

private theorem TablesAgree.endOwnerView_eq {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {m1 m2 : Interfaces.Memory lang profile} (h : TablesAgree m1 m2)
    (blk : BlockId) (cs : List (MemoryContinuation language.operands.Ty language.operands.Val))
    (s : MemoryRegisters language profile) (r : PlaceId) :
    m2.endOwnerView (swapCtx blk cs s) r = m1.endOwnerView s r := by
  unfold Interfaces.Memory.endOwnerView
  have hRt : m2.rootType (swapCtx blk cs s).frame r = m1.rootType s.frame r := (h.rootType_eq s.frame r).symm
  rw [hRt]
  cases m1.rootType s.frame r with
  | none => rfl
  | some ty =>
    simp only [Option.map_some]
    have hCls : m2.classAt (swapCtx blk cs s).frame r = m1.classAt s.frame r := (h.classAt_eq s.frame r).symm
    have hLb : ∀ st, m2.loadBacking (swapCtx blk cs s) (ty := ty) ⟨⟨r, []⟩, st⟩ =
        m1.loadBacking s (ty := ty) ⟨⟨r, []⟩, st⟩ := fun st => h.loadBacking_eq blk cs s _ _
    have hD : m2.declarations = m1.declarations := h.declarations.symm
    have hOv : ∀ st, m2.ownerView (swapCtx blk cs s) r ty st = m1.ownerView s r ty st :=
      fun st => h.ownerView_eq blk cs s r ty st
    simp only [hCls, hLb, hD, hOv]
    rfl

private theorem TablesAgree.canEnd_eq {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {m1 m2 : Interfaces.Memory lang profile} (h : TablesAgree m1 m2)
    (blk : BlockId) (cs : List (MemoryContinuation language.operands.Ty language.operands.Val))
    (s : MemoryRegisters language profile) (r : PlaceId) :
    m2.canEnd (swapCtx blk cs s) r = m1.canEnd s r := by
  unfold Interfaces.Memory.canEnd; rw [h.endOwnerView_eq]

private theorem TablesAgree.ends {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {m1 m2 : Interfaces.Memory lang profile} (h : TablesAgree m1 m2)
    (blk : BlockId) (cs : List (MemoryContinuation language.operands.Ty language.operands.Val))
    {s next : MemoryRegisters language profile} {r : PlaceId}
    (he : m1.Ends s r next) :
    m2.Ends (swapCtx blk cs s) r (swapCtx blk cs next) := by
  obtain ⟨⟨hLive, hCan⟩, hCase⟩ := he
  refine ⟨⟨hLive, (h.canEnd_eq blk cs s r).symm ▸ hCan⟩, ?_⟩
  rcases hCase with ⟨hV, rfl⟩ | ⟨hM, ⟨lay, hLay, hZ, rfl⟩ | ⟨rg, bid, bs, hLu, hProv, hFree, rfl⟩⟩
  · left; exact ⟨(h.classAt_eq _ _).symm ▸ hV, by cases s with | mk f _ _ _ _ _ _ => cases f; rfl⟩
  · right; exact ⟨(h.classAt_eq _ _).symm ▸ hM, .inl ⟨lay, (h.layoutAt_eq _ _).symm ▸ hLay, hZ,
      by cases s with | mk f _ _ _ _ _ _ => cases f; rfl⟩⟩
  · right; exact ⟨(h.classAt_eq _ _).symm ▸ hM, .inr ⟨rg, bid, bs, hLu, hProv, hFree,
      by cases s with | mk f _ _ _ _ _ _ => cases f; rfl⟩⟩

private theorem TablesAgree.endLocals {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {m1 m2 : Interfaces.Memory lang profile} (h : TablesAgree m1 m2)
    (blk : BlockId) (cs : List (MemoryContinuation language.operands.Ty language.operands.Val))
    {rs} {s next : MemoryRegisters language profile}
    (he : m1.EndLocals rs s next) :
    m2.EndLocals rs (swapCtx blk cs s) (swapCtx blk cs next) := by
  induction he with
  | nil _ => exact .nil _
  | cons h1 _ ih => exact .cons (h.ends blk cs h1) ih

private theorem TablesAgree.instructionTrap_eq {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {m1 m2 : Interfaces.Memory lang profile} (h : TablesAgree m1 m2)
    (blk : BlockId) (cs : List (MemoryContinuation language.operands.Ty language.operands.Val))
    (s : MemoryRegisters language profile) (inst : MemoryInst lang) :
    m2.instructionTrap (swapCtx blk cs s) inst = m1.instructionTrap s inst := by
  cases inst with
  | op _ _ args checks _ =>
    simp only [Interfaces.Memory.instructionTrap, h.readArgs_eq, bind, Option.bind]
    cases m1.readArgs args s <;> rfl
  | call _ _ args checks =>
    simp only [Interfaces.Memory.instructionTrap, h.readOperands_eq, bind, Option.bind]
    cases m1.readOperands args s <;> rfl
  | bind _ _ | assign _ _ | addWithOverflow _ _ _ _ _ | beginLifetime _ | endLifetime _ | destructure _ _ => rfl

private theorem swapCtx_prepareAddresses {language : CoreLanguage} {profile : Profile}
    (blk : BlockId) (cs : List (MemoryContinuation language.operands.Ty language.operands.Val))
    (s : MemoryRegisters language profile)
    (addrs : List (Sigma (MemoryPlace language.operands.Ty))) :
    Interfaces.Memory.prepareAddresses (swapCtx blk cs s) addrs =
      swapCtx blk cs (Interfaces.Memory.prepareAddresses s addrs) := by
  unfold Interfaces.Memory.prepareAddresses
  induction addrs generalizing s with
  | nil => rfl
  | cons a rest ih =>
    simp only [List.foldl_cons]
    change List.foldl _ (if (AList.lookup s.addressIds a.2.logical.root).isSome then swapCtx blk cs s else _) rest = _
    split
    · exact ih s
    · have hStep : ({ swapCtx blk cs s with
          addressIds := (a.2.logical.root, ⟨s.nextAddressId⟩) :: s.addressIds
          nextAddressId := s.nextAddressId + 1 } : MemoryRegisters language profile) =
          swapCtx blk cs { s with
            addressIds := (a.2.logical.root, ⟨s.nextAddressId⟩) :: s.addressIds
            nextAddressId := s.nextAddressId + 1 } := by
        cases s with | mk f _ _ _ _ _ _ => cases f; rfl
      change List.foldl _ ({ swapCtx blk cs s with
          addressIds := (a.2.logical.root, ⟨s.nextAddressId⟩) :: s.addressIds
          nextAddressId := s.nextAddressId + 1 } : MemoryRegisters language profile) rest = _
      rw [hStep]
      exact ih _

private theorem TablesAgree.resolveAddress_eq {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {m1 m2 : Interfaces.Memory lang profile} (h : TablesAgree m1 m2)
    (blk : BlockId) (cs : List (MemoryContinuation language.operands.Ty language.operands.Val))
    (s : MemoryRegisters language profile) (logical : Bool)
    (addr : Sigma (MemoryPlace language.operands.Ty)) :
    m2.resolveAddress (swapCtx blk cs s) logical addr = m1.resolveAddress s logical addr := by
  unfold Interfaces.Memory.resolveAddress
  have hv := h.validPlace_eq blk cs s addr.2
  have hr := h.rangeAt_eq blk cs s addr.2.logical
  have hz : m2.zeroRoot (swapCtx blk cs s).frame addr.2.logical.root =
      m1.zeroRoot s.frame addr.2.logical.root := (h.zeroRoot_eq _ _).symm
  have hl : m2.layoutAt (swapCtx blk cs s).frame addr.2.logical.root =
      m1.layoutAt s.frame addr.2.logical.root := (h.layoutAt_eq _ _).symm
  have hL : (swapCtx blk cs s).live = s.live := rfl
  have hA : (swapCtx blk cs s).addressIds = s.addressIds := rfl
  have hB : (swapCtx blk cs s).bytes = s.bytes := rfl
  have hI : Interfaces.Memory.initializedAt (swapCtx blk cs s) addr.2.logical.root addr.2.logical.path =
      Interfaces.Memory.initializedAt s addr.2.logical.root addr.2.logical.path := rfl
  simp only [hv, hr, hz, hl, hL, hA, hB, hI]

private theorem readValue_preserves_ctx {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {m : Interfaces.Memory lang profile} {s after : MemoryRegisters language profile}
    {ty : language.operands.Ty} {rd : MemoryRead _ lang.literal ty} {v : language.operands.Val}
    (h : m.readValue s rd = some (v, after)) :
    after.frame = s.frame ∧ after.block = s.block ∧ after.callers = s.callers := by
  cases rd with
  | literal lit =>
    simp only [Interfaces.Memory.readValue] at h
    cases hl : language.literal ty lit.encoding <;> simp [hl] at h
    cases h.2; exact ⟨rfl, rfl, rfl⟩
  | copy p =>
    obtain rfl := ((Interfaces.Memory.copyRead_iff m s p v after).mp h).2.2.2
    exact ⟨rfl, rfl, rfl⟩
  | move p =>
    simp only [Interfaces.Memory.readValue, bind, Option.bind] at h
    cases ho : Own.move (m.ownerView s p.logical.root ty .owned).1 (m.ownerView s p.logical.root ty .owned).2 <;>
      cases hv : m.loadValue s p <;> simp only [ho, hv] at h <;> try contradiction
    split at h <;> obtain ⟨_, rfl⟩ := Prod.mk.inj (Option.some.inj h) <;> exact ⟨rfl, rfl, rfl⟩

private theorem readArgs_preserves_ctx {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {m : Interfaces.Memory lang profile} {types : List language.operands.Ty}
    {args : Args (MemoryRead _ lang.literal) types}
    {s after : MemoryRegisters language profile} {vs : List language.operands.Val}
    (h : m.readArgs args s = some (vs, after)) :
    after.frame = s.frame ∧ after.block = s.block ∧ after.callers = s.callers := by
  induction args generalizing s vs with
  | nil => cases Option.some.inj h; exact ⟨rfl, rfl, rfl⟩
  | cons rd rest ih =>
    simp only [Interfaces.Memory.readArgs, bind, Option.bind] at h
    cases h1 : m.readValue s rd with
    | none => simp [h1] at h
    | some p1 =>
      rcases p1 with ⟨v, s1⟩
      simp only [h1] at h
      cases h2 : m.readArgs rest s1 with
      | none => simp [h2] at h
      | some p2 =>
        rcases p2 with ⟨vs', s2⟩
        simp only [h2, pure, Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨_, rfl⟩ := h
        obtain ⟨hf1, hb1, hc1⟩ := readValue_preserves_ctx h1
        obtain ⟨hf2, hb2, hc2⟩ := ih h2
        exact ⟨hf2.trans hf1, hb2.trans hb1, hc2.trans hc1⟩

private theorem readOperands_preserves_ctx {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {m : Interfaces.Memory lang profile} {args : List (Sigma (MemoryRead _ lang.literal))}
    {s after : MemoryRegisters language profile} {vs : List language.operands.Val}
    (h : m.readOperands args s = some (vs, after)) :
    after.frame = s.frame ∧ after.block = s.block ∧ after.callers = s.callers := by
  induction args generalizing s vs with
  | nil => cases Option.some.inj h; exact ⟨rfl, rfl, rfl⟩
  | cons a rest ih =>
    rcases a with ⟨ty, rd⟩
    simp only [Interfaces.Memory.readOperands, bind, Option.bind] at h
    cases h1 : m.readValue s rd with
    | none => simp [h1] at h
    | some p1 =>
      rcases p1 with ⟨v, s1⟩
      simp only [h1] at h
      cases h2 : m.readOperands rest s1 with
      | none => simp [h2] at h
      | some p2 =>
        rcases p2 with ⟨vs', s2⟩
        simp only [h2, pure, Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨_, rfl⟩ := h
        obtain ⟨hf1, hb1, hc1⟩ := readValue_preserves_ctx h1
        obtain ⟨hf2, hb2, hc2⟩ := ih h2
        exact ⟨hf2.trans hf1, hb2.trans hb1, hc2.trans hc1⟩

private theorem writeValue_preserves_ctx {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {m : Interfaces.Memory lang profile} {s next : MemoryRegisters language profile}
    {ty : language.operands.Ty} {p : MemoryPlace _ ty} {v : language.operands.Val} {c : Nat}
    (h : m.writeValue s p v c = some next) :
    next.frame = s.frame ∧ next.block = s.block ∧ next.callers = s.callers := by
  simp only [Interfaces.Memory.writeValue] at h
  split at h
  · cases h
  · simp only [bind, Option.bind] at h
    cases ha : m.authorizeWrite s p c with
    | none => simp [ha] at h
    | some ow =>
      simp only [ha] at h
      cases hst : p.storage with
      | virtual =>
        simp only [hst] at h
        cases hr : Interfaces.Memory.replaceLocal (language := language)
            (Interfaces.Memory.markWritten s p ow).virtualValues p.logical v with
        | none => simp [hr] at h
        | some tbl => simp only [hr, pure, Option.some.injEq] at h; subst h; exact ⟨rfl, rfl, rfl⟩
      | materialized =>
        simp only [hst] at h
        split at h
        · cases hr : Interfaces.Memory.replaceLocal (language := language)
              (Interfaces.Memory.markWritten s p ow).zeroValues p.logical v with
          | none => simp [hr] at h
          | some tbl => simp only [hr, pure, Option.some.injEq] at h; subst h; exact ⟨rfl, rfl, rfl⟩
        · cases hrg : m.rangeAt (Interfaces.Memory.markWritten s p ow) p.logical with
          | none => simp [hrg] at h
          | some rg =>
            simp only [hrg] at h
            cases hbr : Interfaces.Memory.byteRuns profile.memoryTarget ty v with
            | none => simp [hbr] at h
            | some runs =>
              simp only [hbr] at h
              cases hsr : Interfaces.Memory.storeRuns profile.memoryTarget c rg runs
                  (Interfaces.Memory.markWritten s p ow).bytes with
              | none => simp [hsr] at h
              | some bs => simp only [hsr, pure, Option.some.injEq] at h; subst h; exact ⟨rfl, rfl, rfl⟩

private theorem begins_preserves_ctx {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {m : Interfaces.Memory lang profile} {s next : MemoryRegisters language profile}
    {r : PlaceId} {c : Nat} (h : m.Begins s r next c) :
    next.frame = s.frame ∧ next.block = s.block ∧ next.callers = s.callers := by
  rcases h.2 with ⟨_, rfl⟩ | ⟨_, _, _, ⟨_, rfl⟩ | ⟨_, _, _, _, _, _, rfl⟩⟩ <;> exact ⟨rfl, rfl, rfl⟩

private theorem ends_preserves_ctx {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {m : Interfaces.Memory lang profile} {s next : MemoryRegisters language profile}
    {r : PlaceId} (h : m.Ends s r next) :
    next.frame = s.frame ∧ next.block = s.block ∧ next.callers = s.callers := by
  rcases h.2 with ⟨_, rfl⟩ | ⟨_, ⟨_, _, _, rfl⟩ | ⟨_, _, _, _, _, _, rfl⟩⟩ <;> exact ⟨rfl, rfl, rfl⟩

private theorem endLocals_preserves_ctx {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {m : Interfaces.Memory lang profile} {rs} {s next : MemoryRegisters language profile}
    (h : m.EndLocals rs s next) :
    next.frame = s.frame ∧ next.block = s.block ∧ next.callers = s.callers := by
  induction h with
  | nil _ => exact ⟨rfl, rfl, rfl⟩
  | cons h1 _ ih =>
    obtain ⟨hf1, hb1, hc1⟩ := ends_preserves_ctx h1
    exact ⟨ih.1.trans hf1, ih.2.1.trans hb1, ih.2.2.trans hc1⟩

private theorem writeParams_preserves_ctx {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {m : Interfaces.Memory lang profile} {ps vs} {s next : MemoryRegisters language profile}
    {ex : Bool} {c : Nat} (h : m.WriteParams ps vs s next ex c) :
    next.frame = s.frame ∧ next.block = s.block ∧ next.callers = s.callers := by
  induction h with
  | nil _ => exact ⟨rfl, rfl, rfl⟩
  | @cons _ _ _ _ _ st started _ _ _ _ hStart hW _ ih =>
    have hs : started.frame = st.frame ∧ started.block = st.block ∧ started.callers = st.callers := by
      split at hStart
      · subst hStart; exact ⟨rfl, rfl, rfl⟩
      · exact begins_preserves_ctx hStart
    have hw := writeValue_preserves_ctx hW.2.2.1
    exact ⟨ih.1.trans (hw.1.trans hs.1), ih.2.1.trans (hw.2.1.trans hs.2.1),
      ih.2.2.trans (hw.2.2.trans hs.2.2)⟩
  | exhausted _ => exact ⟨rfl, rfl, rfl⟩

private theorem paramOutcome_preserves_ctx {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {m : Interfaces.Memory lang profile} {ps vs} {s next : MemoryRegisters language profile}
    {ex : Bool} (h : m.ParamOutcome ps vs s next ex) :
    next.frame = s.frame ∧ next.block = s.block ∧ next.callers = s.callers := by
  cases ex with
  | false => exact writeParams_preserves_ctx h
  | true => exact writeParams_preserves_ctx h.1

private theorem prepareAddresses_preserves_ctx {language : CoreLanguage} {profile : Profile}
    (s : MemoryRegisters language profile) (addrs : List (Sigma (MemoryPlace language.operands.Ty))) :
    (Interfaces.Memory.prepareAddresses s addrs).frame = s.frame ∧
    (Interfaces.Memory.prepareAddresses s addrs).block = s.block ∧
    (Interfaces.Memory.prepareAddresses s addrs).callers = s.callers := by
  unfold Interfaces.Memory.prepareAddresses
  induction addrs generalizing s with
  | nil => exact ⟨rfl, rfl, rfl⟩
  | cons a rest ih =>
    simp only [List.foldl_cons]
    split
    · exact ih s
    · exact ih _

/-- Project one activation frame's block ID through `proj`. -/
def projectFrame {V : Type} (proj : FnId → BlockId → BlockId) (f : MemoryFrame V) : MemoryFrame V :=
  { f with block := proj f.frame f.block }

/-- Project every suspended caller frame's block ID through `proj`. -/
def projectCallers {Ty V : Type} (proj : FnId → BlockId → BlockId)
    (cs : List (MemoryContinuation Ty V)) : List (MemoryContinuation Ty V) :=
  cs.map fun c => { c with caller := projectFrame proj c.caller }

/-- Project a full MIR/Memory register state's active and suspended block IDs through `proj`. -/
def projectState {language : CoreLanguage} {profile : Profile}
    (proj : FnId → BlockId → BlockId) (s : MemoryRegisters language profile) :
    MemoryRegisters language profile :=
  swapCtx (proj s.frame s.block) (projectCallers proj s.callers) s

private theorem projectState_advance {language : CoreLanguage} {profile : Profile}
    (proj : FnId → BlockId → BlockId) (s : MemoryRegisters language profile) :
    (projectState proj s).advance = projectState proj s.advance := by
  cases s with | mk f _ _ _ _ _ _ => cases f; rfl

private theorem operationStore_projectState {language : CoreLanguage} {profile : Profile}
    (proj : FnId → BlockId → BlockId) (s : MemoryRegisters language profile) (logical : Bool) :
    Interfaces.Memory.operationStore (projectState proj s) logical =
      Interfaces.Memory.operationStore s logical := by
  unfold Interfaces.Memory.operationStore
  with_unfolding_all
    congr 1
    simp only [projectState, swapCtx, projectCallers, List.flatMap_map]
    rfl

private theorem swapCtx_projectState {language : CoreLanguage} {profile : Profile}
    (proj : FnId → BlockId → BlockId) (s : MemoryRegisters language profile) :
    swapCtx s.block s.callers (projectState proj s) = s :=
  swapCtx_inv _ _ s

private theorem mirBlock_id_of_find? {Ty : Type} {lang : LoweringLanguage Ty}
    {bs : Array (MIRBlock lang)} {bid : BlockId} {b : MIRBlock lang}
    (h : bs.find? (·.id == bid) = some b) : b.id = bid :=
  eq_of_beq (Array.find?_some (p := fun x : MIRBlock lang => x.id == bid) h)

private theorem projectState_swapCtx_of_ctx {language : CoreLanguage} {profile : Profile}
    (proj : FnId → BlockId → BlockId) {s2 s1 : MemoryRegisters language profile}
    (hf : s1.frame = (projectState proj s2).frame)
    (hb : s1.block = (projectState proj s2).block)
    (hc : s1.callers = (projectState proj s2).callers) :
    projectState proj (swapCtx s2.block s2.callers s1) = s1 := by
  unfold projectState
  change swapCtx (proj s1.frame s2.block) (projectCallers proj s2.callers)
    (swapCtx s2.block s2.callers s1) = s1
  have hf' : s1.frame = s2.frame := hf
  have hb' : s1.block = proj s2.frame s2.block := hb
  have hc' : s1.callers = projectCallers proj s2.callers := hc
  rw [hf', ← hb', ← hc', swapCtx_inv]

private theorem applyOperation_projectState {language : CoreLanguage} {profile : Profile}
    (proj : FnId → BlockId → BlockId) (s : MemoryRegisters language profile) (logical : Bool)
    (store : OperationStore language.operands.Val) :
    Interfaces.Memory.applyOperation (projectState proj s) logical store =
      projectState proj (Interfaces.Memory.applyOperation s logical store) := by
  unfold Interfaces.Memory.applyOperation projectState swapCtx projectCallers
  cases logical <;>
    cases s with | mk f bytes callers effs own nid ex =>
    cases f
    with_unfolding_all
      simp only [List.map_map]
      congr 1

/-- Every active and suspended frame of `s` points to an existing function and block in `mir`. -/
private def ValidFrameBlocks {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    (mir : MIR lang profile) (f : MemoryFrame language.operands.Val) : Prop :=
  ∃ locals graph blk,
    AList.lookup mir.functions f.frame = some (locals, graph) ∧
    graph.blocks.find? (·.id == f.block) = some blk

private def ValidBlocks {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    (mir : MIR lang profile) (s : MemoryRegisters language profile) : Prop :=
  ValidFrameBlocks mir s.toMemoryFrame ∧
  ∀ c ∈ s.callers, ValidFrameBlocks mir c.caller

private theorem MIRCovering.block_correspond {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {proj : FnId → BlockId → BlockId} {src dst : MIR lang profile}
    (h : MIRCovering proj src dst) {fn : FnId} {locals : LocalTypes language.operands.Ty}
    {g2 : MIRGraph lang} {b2 : MIRBlock lang}
    (hLu2 : AList.lookup dst.functions fn = some (locals, g2))
    (hFind2 : g2.blocks.find? (·.id == b2.id) = some b2) :
    ∃ g1 b1,
      AList.lookup src.functions fn = some (locals, g1) ∧
      GraphCovering (proj fn) g1 g2 ∧
      g1.blocks.find? (·.id == proj fn b2.id) = some b1 ∧
      b1.id = proj fn b2.id ∧ b1.params = b2.params ∧ b1.body = b2.body ∧
      b1.boundaryOrigins = b2.boundaryOrigins ∧
      b1.ending = mapTermTarget (proj fn) b2.ending := by
  rcases h.lookup_fn fn with ⟨_, hNone⟩ | ⟨locs, g1, g2', hLu1, hLu2', hGCov⟩
  · rw [hLu2] at hNone; cases hNone
  · cases Option.some.inj (hLu2.symm.trans hLu2')
    have hb2Mem : b2 ∈ g2.blocks.toList := Array.mem_toList_iff.mpr (Array.mem_of_find?_eq_some hFind2)
    obtain ⟨b1, hb1Mem, hId, hP, hBd, hBo, hEnd⟩ := hGCov.forward b2 hb2Mem
    have hFind1 : g1.blocks.find? (·.id == proj fn b2.id) = some b1 := by
      have hf := array_find?_of_mem_nodup hGCov.srcWF.1 hb1Mem
      rwa [hId] at hf
    exact ⟨g1, b1, hLu1, hGCov, hFind1, hId, hP, hBd, hBo, hEnd⟩

private theorem MIRCovering.logicalView_graphAt_eq {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    (mir : MIR lang profile) (fn : FnId) :
    (mir.logicalView (language := language)).graphAt fn =
      (AList.lookup mir.functions fn).map fun (_, g) => toLogicalGraph g := by
  unfold Interfaces.Memory.graphAt
  rw [toLogicalGraph_eq_logicalView]
  have aux : ∀ (xs : AList FnId (LocalTypes language.operands.Ty × MIRGraph lang)),
      (AList.lookup (AList.map (fun p => (p.1, toLogicalGraph p.2)) xs) fn).map Prod.snd =
      (AList.lookup xs fn).map fun (_, g) => toLogicalGraph g := by
    intro xs
    induction xs with
    | nil => rfl
    | cons a rest ih =>
      simp only [AList.map, List.map_cons, AList.lookup]
      by_cases he : a.1 = fn
      · simp only [he, ↓reduceIte, Option.map_some]
      · simp only [he, ↓reduceIte]; exact ih
  exact aux mir.functions

private theorem MIRCovering.logicalView_blockAt_eq {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    (mir : MIR lang profile) (s : MemoryRegisters language profile) :
    (mir.logicalView (language := language)).blockAt s =
      ((AList.lookup mir.functions s.frame).bind fun (_, g) =>
        g.blocks.find? (·.id == s.block)).map toLogicalBlock := by
  unfold Interfaces.Memory.blockAt
  rw [MIRCovering.logicalView_graphAt_eq]
  cases AList.lookup mir.functions s.frame with
  | none => rfl
  | some p =>
    rcases p with ⟨locals, g⟩
    simp only [Option.map_some, toLogicalGraph, bind, Option.bind]
    rw [← Array.find?_toList, ← Array.find?_toList, Array.toList_map]
    generalize g.blocks.toList = bs
    induction bs with
    | nil => rfl
    | cons b rest ih =>
      simp only [List.map_cons, List.find?_cons]
      have hId : (toLogicalBlock b).id = b.id := rfl
      rw [hId]
      cases b.id == s.block with
      | false => exact ih
      | true => rfl

/-- Local mirror of `Contracts.lean`'s private `entryStateFor` (definitionally equal under `with_unfolding_all`). -/
private def entryStateOf {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    (memory : Interfaces.Memory lang profile) (arity : Nat := 0) :
    MemoryRegisters language profile :=
  let graph := memory.graphAt memory.entry
  let id := (graph.map CFG.entry).getD ⟨0⟩
  let block := graph.bind (·.blocks.find? (·.id == id))
  let pc := (block.map fun block =>
    if block.params.length = arity then 0 else block.body.length + 1).getD 0
  ⟨⟨memory.entry, id, pc, [], [], [], [], [], [], []⟩, Mem.Memory.empty, [], [], {}, 0, false⟩

/-- Local mirror of `Contracts.lean`'s private `hostedResultFor` (definitionally equal under `with_unfolding_all`). -/
private def hostedResultOf {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    (memory : Interfaces.Memory lang profile) (state : MemoryRegisters language profile) :
    Option Concrete.Contracts.Observation := do
  match ← memory.terminalObservation state with
  | .returned status bytes => return .returned status bytes
  | .trapped status kind _ bytes =>
    let block ← memory.blockAt state
    let origin ← match block.body[state.instruction]? with
      | some op => some op.origin
      | none => match block.ending with
        | .trap _ origin => some origin
        | _ => none
    let site ← runtimeSite memory.project origin
    return .trapped status kind site bytes
  | .startupRefused status bytes => return .startupRefused status bytes
  | .resourceExhausted => return .resourceExhausted

/-- Local mirror of `Contracts.lean`'s private `MIR.instructionAt` (definitionally equal under `with_unfolding_all`). -/
private def mirInstructionAt {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    (mir : MIR lang profile) (state : MemoryRegisters language profile) :
    Option (LocatedOp (MIRInst lang)) := do
  let (_, graph) ← AList.lookup mir.functions state.frame
  let block ← graph.blocks.find? (·.id == state.block)
  block.body[state.instruction]?

private theorem mir_initial_iff {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    (mir : MIR lang profile) (s : MemoryRegisters language profile) :
    (Interfaces.MIR.Execution (language := language)).initial mir s ↔
      s = entryStateOf (mir.logicalView (language := language)) := by
  with_unfolding_all exact Iff.rfl

private theorem mir_result_iff {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    (mir : MIR lang profile) (s : MemoryRegisters language profile) (o : Concrete.Contracts.Observation) :
    (Interfaces.MIR.Execution (language := language)).result mir s o ↔
      hostedResultOf (mir.logicalView (language := language)) s = some o := by
  with_unfolding_all exact Iff.rfl

private theorem mir_step_iff {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    (mir : MIR lang profile) (s next : MemoryRegisters language profile) :
    (Interfaces.MIR.Execution (language := language)).step mir s next ↔
      hostedResultOf (mir.logicalView (language := language)) s = none ∧
      mir.Step (language := language) s next ∧
      TracePrefix ((Interfaces.MIR.Execution (language := language)).trace mir s)
        ((Interfaces.MIR.Execution (language := language)).trace mir next) := by
  with_unfolding_all exact Iff.rfl

private theorem mir_raw_step_iff {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    (mir : MIR lang profile) (s next : MemoryRegisters language profile) :
    mir.Step (language := language) s next ↔
      match mirInstructionAt mir s with
      | some op =>
        (mir.logicalView (language := language)).instructionTrap s (toLogicalInst op.instruction) = none ∧
        (mir.logicalView (language := language)).InstructionStep s next true (toLogicalInst op.instruction)
      | none =>
        ∃ block, (mir.logicalView (language := language)).blockAt s = some block ∧
          s.instruction = block.body.length ∧
          (mir.logicalView (language := language)).ControlStep s next block.ending := by
  with_unfolding_all
    unfold Interfaces.MIR.Step
    dsimp only
    split
    · rename_i op hAt
      have hAt' : mirInstructionAt mir s = some op := hAt
      rw [hAt']
      cases op with
      | mk orig inst =>
        dsimp only
        have hEq : ∀ {a b : MemoryInst lang}, a = b →
            (((mir.logicalView (language := language)).instructionTrap s a = none ∧
              (mir.logicalView (language := language)).InstructionStep s next true a) ↔
             ((mir.logicalView (language := language)).instructionTrap s b = none ∧
              (mir.logicalView (language := language)).InstructionStep s next true b)) := by
          intro a b heq; rw [heq]
        apply hEq
        cases inst with
        | bind _ src => cases src <;> rfl
        | assign _ src => cases src <;> rfl
        | op dst o args checks addrs =>
          refine congrArg (fun x => MemoryInst.op (toLogicalPlace dst) o x checks
            (addrs.map fun a => ⟨a.1, toLogicalPlace a.2⟩)) ?_
          clear o hAt hAt'
          induction args with
          | nil => rfl
          | cons a rest ih => exact (congrFun (congrArg Args.cons (by cases a <;> rfl)) _).trans (congrArg _ ih)
        | endLifetime _ => rfl
        | destructure _ _ => rfl
        | call callee dst args checks =>
          refine congrArg (fun x => MemoryInst.call callee (dst.map fun d => ⟨d.1, toLogicalPlace d.2⟩) x checks) ?_
          apply List.map_congr_left; rintro ⟨_, rd⟩ _; cases rd <;> rfl
    · rename_i hAt
      have hAt' : mirInstructionAt mir s = none := hAt
      rw [hAt']

private theorem MIRCovering.entryState_project {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {proj : FnId → BlockId → BlockId} {src dst : MIR lang profile}
    (h : MIRCovering proj src dst)
    (hWF1 : (src.logicalView (language := language)).HandoffWF) :
    projectState proj (entryStateOf (dst.logicalView (language := language))) =
      entryStateOf (src.logicalView (language := language)) ∧
    ValidBlocks dst (entryStateOf (dst.logicalView (language := language))) := by
  have hEntryIn : src.entry ∈ List.map Prod.fst src.functions := by
    have h1 := hWF1.2.1
    have hKeys : List.map Prod.fst (src.logicalView (language := language)).functions =
        List.map Prod.fst src.functions := by
      have hc := congrArg (List.map Prod.fst) (MIR.logicalView_inventories_eq (language := language) src)
      simpa only [List.map_map, Function.comp_def] using hc
    rwa [hKeys] at h1
  have hLu1Some : (AList.lookup src.functions src.entry).isSome = true := by
    have aux : ∀ (xs : AList FnId (LocalTypes language.operands.Ty × MIRGraph lang)),
        src.entry ∈ List.map Prod.fst xs → (AList.lookup xs src.entry).isSome = true := by
      intro xs hm
      induction xs with
      | nil => cases hm
      | cons a rest ih =>
        simp only [AList.lookup]
        by_cases he : a.1 = src.entry
        · simp [he]
        · simp only [he, ↓reduceIte]
          rcases List.mem_cons.mp hm with hHead | hTail
          · exact (he hHead.symm).elim
          · exact ih hTail
    exact aux src.functions hEntryIn
  rcases h.lookup_fn src.entry with ⟨hNone1, _⟩ | ⟨locals, g1, g2, hLu1, hLu2, hGCov⟩
  · rw [hNone1] at hLu1Some; cases hLu1Some
  · obtain ⟨eb2, heb2Mem, heb2Id⟩ := hGCov.dstWF.2.1
    have hFind2 : g2.blocks.find? (·.id == g2.entry) = some eb2 := by
      have hf := array_find?_of_mem_nodup hGCov.dstWF.1 heb2Mem
      rwa [heb2Id] at hf
    obtain ⟨b1, hb1Mem, hb1Id, hb1P, hb1Bd, _, _⟩ := hGCov.forward eb2 heb2Mem
    have hFind1 : g1.blocks.find? (·.id == g1.entry) = some b1 := by
      have hf := array_find?_of_mem_nodup hGCov.srcWF.1 hb1Mem
      have hid : b1.id = g1.entry := hb1Id.trans ((congrArg (proj src.entry) heb2Id).trans hGCov.entry)
      rwa [hid] at hf
    have hGraph1 : (src.logicalView (language := language)).graphAt src.entry = some (toLogicalGraph g1) := by
      rw [MIRCovering.logicalView_graphAt_eq, hLu1]; rfl
    have hGraph2 : (dst.logicalView (language := language)).graphAt dst.entry = some (toLogicalGraph g2) := by
      rw [MIRCovering.logicalView_graphAt_eq, h.entry, hLu2]; rfl
    have hFindLog1 : (toLogicalGraph g1).blocks.find? (·.id == g1.entry) = some (toLogicalBlock b1) := by
      have hMem : toLogicalBlock b1 ∈ (toLogicalGraph g1).blocks.toList := by
        simp only [toLogicalGraph, Array.toList_map, List.mem_map]; exact ⟨b1, hb1Mem, rfl⟩
      have hf := array_find?_of_mem_nodup (toLogicalGraph_wf.mpr hGCov.srcWF).1 hMem
      have hid : (toLogicalBlock b1).id = g1.entry := hb1Id.trans ((congrArg (proj src.entry) heb2Id).trans hGCov.entry)
      rwa [hid] at hf
    have hFindLog2 : (toLogicalGraph g2).blocks.find? (·.id == g2.entry) = some (toLogicalBlock eb2) := by
      have hMem : toLogicalBlock eb2 ∈ (toLogicalGraph g2).blocks.toList := by
        simp only [toLogicalGraph, Array.toList_map, List.mem_map]; exact ⟨eb2, heb2Mem, rfl⟩
      have hf := array_find?_of_mem_nodup (toLogicalGraph_wf.mpr hGCov.dstWF).1 hMem
      rwa [show (toLogicalBlock eb2).id = g2.entry from heb2Id] at hf
    have hGraph1' : (src.logicalView (language := language)).graphAt (src.logicalView (language := language)).entry =
        some (toLogicalGraph g1) := hGraph1
    have hGraph2' : (dst.logicalView (language := language)).graphAt (dst.logicalView (language := language)).entry =
        some (toLogicalGraph g2) := hGraph2
    have hEntry1 : (src.logicalView (language := language)).entry = src.entry := rfl
    have hEntry2 : (dst.logicalView (language := language)).entry = dst.entry := rfl
    have hPC : (if (toLogicalBlock eb2).params.length = 0 then 0 else (toLogicalBlock eb2).body.length + 1) =
        (if (toLogicalBlock b1).params.length = 0 then 0 else (toLogicalBlock b1).body.length + 1) := by
      simp only [toLogicalBlock, hb1P, hb1Bd]; rfl
    constructor
    · unfold entryStateOf
      simp only [hGraph1', hGraph2', Option.map_some, Option.getD_some, Option.bind_some,
        projectState, swapCtx, projectCallers, List.map_nil]
      dsimp only [toLogicalGraph] at hFindLog1 hFindLog2 ⊢
      rw [hFindLog1, hFindLog2]
      simp only [Option.map_some, Option.getD_some]
      rw [hEntry1, hEntry2, h.entry, hGCov.entry, hPC]
    · refine ⟨⟨locals, g2, eb2, ?_, ?_⟩, fun _ hm => nomatch hm⟩
      · unfold entryStateOf; dsimp only; rw [hEntry2, h.entry]; exact hLu2
      · unfold entryStateOf; dsimp only; rw [hGraph2']; exact hFind2

private theorem MIRCovering.terminalObservation_eq {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {proj : FnId → BlockId → BlockId} {src dst : MIR lang profile}
    (h : MIRCovering proj src dst) {s2 : MemoryRegisters language profile}
    (hvb : ValidBlocks dst s2) :
    (dst.logicalView (language := language)).terminalObservation s2 =
      (src.logicalView (language := language)).terminalObservation (projectState proj s2) := by
  have hT := h.logicalView_tablesAgree.symm
  obtain ⟨locals, g2, b2, hLu2, hFind2⟩ := hvb.1
  have hb2Id : b2.id = s2.block := mirBlock_id_of_find? hFind2
  have hFind2' : g2.blocks.find? (·.id == b2.id) = some b2 := hb2Id ▸ hFind2
  obtain ⟨g1, b1, hLu1, _, hFind1, hId1, _, hBd1, _, hEnd1⟩ := h.block_correspond hLu2 hFind2'
  have hBlk2 : (dst.logicalView (language := language)).blockAt s2 = some (toLogicalBlock b2) := by
    rw [MIRCovering.logicalView_blockAt_eq, hLu2]; simp [hFind2]
  have hBlk1 : (src.logicalView (language := language)).blockAt (projectState proj s2) = some (toLogicalBlock b1) := by
    rw [MIRCovering.logicalView_blockAt_eq]
    change ((AList.lookup src.functions s2.frame).bind fun (_, g) =>
      g.blocks.find? (·.id == proj s2.frame s2.block)).map toLogicalBlock = some (toLogicalBlock b1)
    rw [hLu1, ← hb2Id]; simp [hFind1]
  unfold Interfaces.Memory.terminalObservation
  have hEx : (projectState proj s2).resourceExhausted = s2.resourceExhausted := rfl
  have hInst : (projectState proj s2).instruction = s2.instruction := rfl
  have hBodyEq : (toLogicalBlock b1).body = (toLogicalBlock b2).body := by simp [toLogicalBlock, hBd1]
  have hProjEq : (dst.logicalView (language := language)).project = (src.logicalView (language := language)).project := h.project
  have hEntEq : (dst.logicalView (language := language)).entry = (src.logicalView (language := language)).entry := h.entry
  rw [hEx]
  split
  · rfl
  · simp only [hBlk1, hBlk2, bind, Option.bind, hBodyEq, hInst]
    cases hOp : (toLogicalBlock b2).body[s2.instruction]? with
    | some op =>
      dsimp only
      have hTrap : (src.logicalView (language := language)).instructionTrap (projectState proj s2) op.instruction =
          (dst.logicalView (language := language)).instructionTrap s2 op.instruction :=
        hT.instructionTrap_eq _ _ s2 op.instruction
      rw [hTrap, hProjEq]
    | none =>
      dsimp only
      split
      · rfl
      · dsimp only [toLogicalBlock]
        rw [hEnd1]
        cases b2.ending with
        | jump _ => rfl
        | branch _ _ _ => rfl
        | trap _ _ => dsimp only [mapTermTarget, toLogicalTerm]; rw [hProjEq]
        | invalid => rfl
        | ret opt =>
          cases opt with
          | none => rfl
          | some a =>
            rcases a with ⟨ty, rd⟩
            dsimp only [mapTermTarget, toLogicalTerm, Option.map_some]
            have hEmp : (projectState proj s2).callers.isEmpty = s2.callers.isEmpty := by
              simp [projectState, swapCtx, projectCallers]
            have hFr : (projectState proj s2).frame = s2.frame := rfl
            rw [hEmp, hFr, hEntEq]
            split
            · rfl
            · unfold projectState
              rw [hT.readValue_eq]
              cases (dst.logicalView (language := language)).readValue s2 (toLogicalRead rd) with
              | none => rfl
              | some pair =>
                rcases pair with ⟨v, after2⟩
                simp only [Option.map_some]
                have hAll : after2.live.all ((src.logicalView (language := language)).canEnd
                    (swapCtx (proj s2.frame s2.block) (projectCallers proj s2.callers) after2)) =
                    after2.live.all ((dst.logicalView (language := language)).canEnd after2) := by
                  induction after2.live with
                  | nil => rfl
                  | cons r rs ih => simp only [List.all_cons, hT.canEnd_eq _ _ after2 r, ih]
                change (if !after2.live.all _ then none else _) = (if !after2.live.all _ then none else _)
                rw [hAll]

private theorem MIRCovering.hostedResult_eq {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {proj : FnId → BlockId → BlockId} {src dst : MIR lang profile}
    (h : MIRCovering proj src dst) {s2 : MemoryRegisters language profile}
    (hvb : ValidBlocks dst s2) :
    hostedResultOf (dst.logicalView (language := language)) s2 =
      hostedResultOf (src.logicalView (language := language)) (projectState proj s2) := by
  unfold hostedResultOf
  rw [h.terminalObservation_eq hvb]
  cases (src.logicalView (language := language)).terminalObservation (projectState proj s2) with
  | none => rfl
  | some obs =>
    cases obs with
    | returned _ _ => rfl
    | startupRefused _ _ => rfl
    | resourceExhausted => rfl
    | trapped status kind span bytes =>
      obtain ⟨locals, g2, b2, hLu2, hFind2⟩ := hvb.1
      have hb2Id : b2.id = s2.block := mirBlock_id_of_find? hFind2
      obtain ⟨g1, b1, hLu1, _, hFind1, _, _, hBd1, _, hEnd1⟩ := h.block_correspond hLu2 (hb2Id ▸ hFind2)
      have hBlk2 : (dst.logicalView (language := language)).blockAt s2 = some (toLogicalBlock b2) := by
        rw [MIRCovering.logicalView_blockAt_eq, hLu2]; simp [hFind2]
      have hBlk1 : (src.logicalView (language := language)).blockAt (projectState proj s2) = some (toLogicalBlock b1) := by
        rw [MIRCovering.logicalView_blockAt_eq]
        change ((AList.lookup src.functions s2.frame).bind fun (_, g) =>
          g.blocks.find? (·.id == proj s2.frame s2.block)).map toLogicalBlock = some (toLogicalBlock b1)
        rw [hLu1, ← hb2Id]; simp [hFind1]
      have hBodyEq : (toLogicalBlock b1).body = (toLogicalBlock b2).body := by simp [toLogicalBlock, hBd1]
      have hInst : (projectState proj s2).instruction = s2.instruction := rfl
      have hProjEq : (dst.logicalView (language := language)).project = (src.logicalView (language := language)).project := h.project
      simp only [bind, Option.bind, hBlk1, hBlk2, hBodyEq, hInst, hProjEq]
      cases (toLogicalBlock b2).body[s2.instruction]? with
      | some op => rfl
      | none =>
        dsimp only [toLogicalBlock]
        rw [hEnd1]
        cases b2.ending <;> rfl

private theorem MIRCovering.trace_eq {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {proj : FnId → BlockId → BlockId} {src dst : MIR lang profile}
    (h : MIRCovering proj src dst) {s2 : MemoryRegisters language profile}
    (hvb : ValidBlocks dst s2) :
    (Interfaces.MIR.Execution (language := language)).trace dst s2 =
      (Interfaces.MIR.Execution (language := language)).trace src (projectState proj s2) := by
  have hRes := h.hostedResult_eq hvb
  with_unfolding_all
    change s2.effects.map Concrete.Contracts.ProcessEvent.effect ++
        ((hostedResultOf (dst.logicalView (language := language)) s2).map fun o =>
          o.stderr.data.toList.map Concrete.Contracts.ProcessEvent.stderr).getD [] =
      (projectState proj s2).effects.map Concrete.Contracts.ProcessEvent.effect ++
        ((hostedResultOf (src.logicalView (language := language)) (projectState proj s2)).map fun o =>
          o.stderr.data.toList.map Concrete.Contracts.ProcessEvent.stderr).getD []
    rw [hRes]; rfl

private theorem MIRCovering.instructionAt_eq {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {proj : FnId → BlockId → BlockId} {src dst : MIR lang profile}
    (h : MIRCovering proj src dst) {s2 : MemoryRegisters language profile}
    (hvb : ValidBlocks dst s2) :
    mirInstructionAt dst s2 = mirInstructionAt src (projectState proj s2) := by
  obtain ⟨locals, g2, b2, hLu2, hFind2⟩ := hvb.1
  have hb2Id : b2.id = s2.block := mirBlock_id_of_find? hFind2
  obtain ⟨g1, b1, hLu1, _, hFind1, _, _, hBd1, _, _⟩ := h.block_correspond hLu2 (hb2Id ▸ hFind2)
  unfold mirInstructionAt
  have hF : (projectState proj s2).frame = s2.frame := rfl
  have hB : (projectState proj s2).block = proj s2.frame s2.block := rfl
  have hI : (projectState proj s2).instruction = s2.instruction := rfl
  rw [hF, hB, hI, hLu1, hLu2]
  simp only [bind, Option.bind]
  rw [hFind2, ← hb2Id, hFind1]
  simp only [hBd1]

private theorem MIRCovering.call_forward {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {proj : FnId → BlockId → BlockId} {src dst : MIR lang profile}
    (h : MIRCovering proj src dst) {callee dstOpt args} {s2 next2 : MemoryRegisters language profile}
    (hvb : ValidBlocks dst s2)
    (hCall : (dst.logicalView (language := language)).Call callee dstOpt args s2 next2 false) :
    (src.logicalView (language := language)).Call callee dstOpt args
        (projectState proj s2) (projectState proj next2) false ∧
      ValidBlocks dst next2 := by
  have hT := h.logicalView_tablesAgree.symm
  obtain ⟨vals, after2, g2Log, eb2Log, hRead2, hGraph2, hFind2, hTys, hPO2⟩ := hCall
  obtain ⟨hf2, hb2, hc2⟩ := readOperands_preserves_ctx hRead2
  rw [MIRCovering.logicalView_graphAt_eq] at hGraph2
  cases hLu2 : AList.lookup dst.functions callee with
  | none => rw [hLu2] at hGraph2; cases hGraph2
  | some p2 =>
    rcases p2 with ⟨locals, g2⟩
    rw [hLu2] at hGraph2
    cases Option.some.inj hGraph2
    rcases h.lookup_fn callee with ⟨_, hNone⟩ | ⟨locs, g1, g2', hLu1, hLu2', hGCov⟩
    · rw [hLu2] at hNone; cases hNone
    · cases Option.some.inj (hLu2.symm.trans hLu2')
      obtain ⟨eb2, heb2Mem, heb2Id⟩ := hGCov.dstWF.2.1
      have hFind2Raw : g2.blocks.find? (·.id == g2.entry) = some eb2 := by
        have hf := array_find?_of_mem_nodup hGCov.dstWF.1 heb2Mem; rwa [heb2Id] at hf
      have hFind2Log : (toLogicalGraph g2).blocks.find? (·.id == g2.entry) = some (toLogicalBlock eb2) := by
        have hm : toLogicalBlock eb2 ∈ (toLogicalGraph g2).blocks.toList := by
          simp only [toLogicalGraph, Array.toList_map, List.mem_map]; exact ⟨eb2, heb2Mem, rfl⟩
        have hf := array_find?_of_mem_nodup (toLogicalGraph_wf.mpr hGCov.dstWF).1 hm
        rwa [show (toLogicalBlock eb2).id = g2.entry from heb2Id] at hf
      cases Option.some.inj (hFind2.symm.trans hFind2Log)
      obtain ⟨eb1, heb1Mem, heb1Id, heb1P, _, _, _⟩ := hGCov.forward eb2 heb2Mem
      have hFind1Log : (toLogicalGraph g1).blocks.find? (·.id == g1.entry) = some (toLogicalBlock eb1) := by
        have hm : toLogicalBlock eb1 ∈ (toLogicalGraph g1).blocks.toList := by
          simp only [toLogicalGraph, Array.toList_map, List.mem_map]; exact ⟨eb1, heb1Mem, rfl⟩
        have hf := array_find?_of_mem_nodup (toLogicalGraph_wf.mpr hGCov.srcWF).1 hm
        have hid : (toLogicalBlock eb1).id = g1.entry :=
          heb1Id.trans ((congrArg (proj callee) heb2Id).trans hGCov.entry)
        rwa [hid] at hf
      let init2 : MemoryRegisters language profile :=
        { toMemoryFrame := ⟨callee, g2.entry, 0, [], [], [], [], [], [], []⟩
          bytes := after2.bytes
          ownership := after2.ownership
          nextAddressId := after2.nextAddressId
          effects := after2.effects
          callers := ⟨after2.advance.toMemoryFrame, dstOpt⟩ :: after2.callers }
      obtain ⟨hfn2, hbn2, hcn2⟩ := paramOutcome_preserves_ctx hPO2
      have hRead1 : (src.logicalView (language := language)).readOperands args (projectState proj s2) =
          some (vals, projectState proj after2) := by
        unfold projectState
        rw [hT.readOperands_eq, hRead2, Option.map_some, ← hf2, ← hb2, ← hc2]
      have hPO1 := hT.paramOutcome (proj callee g2.entry)
        (projectCallers proj (⟨after2.advance.toMemoryFrame, dstOpt⟩ :: after2.callers)) hPO2
      dsimp only [toLogicalGraph] at hPO1
      have hInit1Eq : swapCtx (proj callee g2.entry)
          (projectCallers proj (⟨after2.advance.toMemoryFrame, dstOpt⟩ :: after2.callers)) init2 =
          ({ toMemoryFrame := ⟨callee, g1.entry, 0, [], [], [], [], [], [], []⟩
             bytes := (projectState proj after2).bytes
             ownership := (projectState proj after2).ownership
             nextAddressId := (projectState proj after2).nextAddressId
             effects := (projectState proj after2).effects
             callers := ⟨(projectState proj after2).advance.toMemoryFrame, dstOpt⟩ :: (projectState proj after2).callers } :
            MemoryRegisters language profile) := by
        cases after2 with | mk f _ _ _ _ _ _ =>
        cases f
        simp only [init2, swapCtx, projectCallers, List.map_cons, projectState, projectFrame, MemoryRegisters.advance, hGCov.entry]
      have hNext1Eq : swapCtx (proj callee g2.entry)
          (projectCallers proj (⟨after2.advance.toMemoryFrame, dstOpt⟩ :: after2.callers)) next2 =
          projectState proj next2 := by
        have hF : next2.frame = callee := hfn2
        have hB : next2.block = g2.entry := hbn2
        have hC : next2.callers = ⟨after2.advance.toMemoryFrame, dstOpt⟩ :: after2.callers := hcn2
        unfold projectState; rw [hF, hB, hC]
      change (src.logicalView (language := language)).ParamOutcome (toLogicalBlock eb2).params vals
        (swapCtx (proj callee g2.entry)
          (projectCallers proj (⟨after2.advance.toMemoryFrame, dstOpt⟩ :: after2.callers)) init2)
        (swapCtx (proj callee g2.entry)
          (projectCallers proj (⟨after2.advance.toMemoryFrame, dstOpt⟩ :: after2.callers)) next2) false at hPO1
      rw [hInit1Eq, hNext1Eq] at hPO1
      have hParamsEq : (toLogicalBlock eb1).params = (toLogicalBlock eb2).params := by
        simp [toLogicalBlock, heb1P]
      constructor
      · refine ⟨vals, projectState proj after2, toLogicalGraph g1, toLogicalBlock eb1,
          hRead1, ?_, hFind1Log, by rw [hParamsEq]; exact hTys, by rw [hParamsEq]; exact hPO1⟩
        rw [MIRCovering.logicalView_graphAt_eq, hLu1]; rfl
      · refine ⟨⟨locals, g2, eb2, ?_, ?_⟩, ?_⟩
        · rw [hfn2]; exact hLu2
        · rw [hbn2]; exact hFind2Raw
        · intro c hc
          rw [hcn2] at hc
          rcases List.mem_cons.mp hc with rfl | htail
          · obtain ⟨l0, g0, b0, hL0, hF0⟩ := hvb.1
            exact ⟨l0, g0, b0, by simpa [MemoryRegisters.advance, hf2] using hL0,
              by simpa [MemoryRegisters.advance, hb2] using hF0⟩
          · exact hvb.2 c (hc2 ▸ htail)

private theorem MIRCovering.call_backward {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {proj : FnId → BlockId → BlockId} {src dst : MIR lang profile}
    (h : MIRCovering proj src dst) {callee dstOpt args} {s2 next1 : MemoryRegisters language profile}
    (hCall : (src.logicalView (language := language)).Call callee dstOpt args
      (projectState proj s2) next1 false) :
    ∃ next2,
      (dst.logicalView (language := language)).Call callee dstOpt args s2 next2 false ∧
      projectState proj next2 = next1 := by
  have hT := h.logicalView_tablesAgree
  obtain ⟨vals, after1, g1Log, eb1Log, hRead1, hGraph1, hFind1, hTys, hPO1⟩ := hCall
  obtain ⟨hf1, hb1, hc1⟩ := readOperands_preserves_ctx hRead1
  rw [MIRCovering.logicalView_graphAt_eq] at hGraph1
  cases hLu1 : AList.lookup src.functions callee with
  | none => rw [hLu1] at hGraph1; cases hGraph1
  | some p1 =>
    rcases p1 with ⟨locals, g1⟩
    rw [hLu1] at hGraph1
    cases Option.some.inj hGraph1
    rcases h.lookup_fn callee with ⟨hNone, _⟩ | ⟨locs, g1', g2, hLu1', hLu2, hGCov⟩
    · rw [hLu1] at hNone; cases hNone
    · cases Option.some.inj (hLu1.symm.trans hLu1')
      obtain ⟨eb2, heb2Mem, heb2Id⟩ := hGCov.dstWF.2.1
      have hFind2Log : (toLogicalGraph g2).blocks.find? (·.id == g2.entry) = some (toLogicalBlock eb2) := by
        have hm : toLogicalBlock eb2 ∈ (toLogicalGraph g2).blocks.toList := by
          simp only [toLogicalGraph, Array.toList_map, List.mem_map]; exact ⟨eb2, heb2Mem, rfl⟩
        have hf := array_find?_of_mem_nodup (toLogicalGraph_wf.mpr hGCov.dstWF).1 hm
        rwa [show (toLogicalBlock eb2).id = g2.entry from heb2Id] at hf
      obtain ⟨eb1, heb1Mem, heb1Id, heb1P, _, _, _⟩ := hGCov.forward eb2 heb2Mem
      have hFind1Log : (toLogicalGraph g1).blocks.find? (·.id == g1.entry) = some (toLogicalBlock eb1) := by
        have hm : toLogicalBlock eb1 ∈ (toLogicalGraph g1).blocks.toList := by
          simp only [toLogicalGraph, Array.toList_map, List.mem_map]; exact ⟨eb1, heb1Mem, rfl⟩
        have hf := array_find?_of_mem_nodup (toLogicalGraph_wf.mpr hGCov.srcWF).1 hm
        have hid : (toLogicalBlock eb1).id = g1.entry :=
          heb1Id.trans ((congrArg (proj callee) heb2Id).trans hGCov.entry)
        rwa [hid] at hf
      cases Option.some.inj (hFind1.symm.trans hFind1Log)
      have hRead2 := hT.readOperands_eq s2.block s2.callers args (projectState proj s2)
      rw [swapCtx_projectState, hRead1, Option.map_some] at hRead2
      let after2 := swapCtx s2.block s2.callers after1
      have hAfter1Eq : projectState proj after2 = after1 :=
        projectState_swapCtx_of_ctx proj hf1 hb1 hc1
      have hPO2 := hT.paramOutcome g2.entry (⟨after2.advance.toMemoryFrame, dstOpt⟩ :: after2.callers) hPO1
      dsimp only [toLogicalGraph] at hPO2
      let next2 := swapCtx g2.entry (⟨after2.advance.toMemoryFrame, dstOpt⟩ :: after2.callers) next1
      obtain ⟨hfn1, hbn1, hcn1⟩ := paramOutcome_preserves_ctx hPO1
      have hInit2Eq : swapCtx g2.entry (⟨after2.advance.toMemoryFrame, dstOpt⟩ :: after2.callers)
          ({ toMemoryFrame := ⟨callee, g1.entry, 0, [], [], [], [], [], [], []⟩
             bytes := after1.bytes
             ownership := after1.ownership
             nextAddressId := after1.nextAddressId
             effects := after1.effects
             callers := ⟨after1.advance.toMemoryFrame, dstOpt⟩ :: after1.callers } : MemoryRegisters language profile) =
          ({ toMemoryFrame := ⟨callee, g2.entry, 0, [], [], [], [], [], [], []⟩
             bytes := after2.bytes
             ownership := after2.ownership
             nextAddressId := after2.nextAddressId
             effects := after2.effects
             callers := ⟨after2.advance.toMemoryFrame, dstOpt⟩ :: after2.callers } : MemoryRegisters language profile) := rfl
      change (dst.logicalView (language := language)).ParamOutcome (toLogicalBlock eb1).params vals
        (swapCtx g2.entry (⟨after2.advance.toMemoryFrame, dstOpt⟩ :: after2.callers)
          ({ toMemoryFrame := ⟨callee, g1.entry, 0, [], [], [], [], [], [], []⟩
             bytes := after1.bytes
             ownership := after1.ownership
             nextAddressId := after1.nextAddressId
             effects := after1.effects
             callers := ⟨after1.advance.toMemoryFrame, dstOpt⟩ :: after1.callers } : MemoryRegisters language profile))
        next2 false at hPO2
      rw [hInit2Eq] at hPO2
      have hParamsEq : (toLogicalBlock eb1).params = (toLogicalBlock eb2).params := by
        simp [toLogicalBlock, heb1P]
      refine ⟨next2, ?_, ?_⟩
      · refine ⟨vals, after2, toLogicalGraph g2, toLogicalBlock eb2, hRead2, ?_, hFind2Log,
          by rw [← hParamsEq]; exact hTys, by rw [← hParamsEq]; exact hPO2⟩
        rw [MIRCovering.logicalView_graphAt_eq, hLu2]; rfl
      · unfold projectState next2
        have hF : next1.frame = callee := hfn1
        have hB : next1.block = g1.entry := hbn1
        have hC : next1.callers = ⟨after1.advance.toMemoryFrame, dstOpt⟩ :: after1.callers := hcn1
        have hCS : projectCallers proj (⟨after2.advance.toMemoryFrame, dstOpt⟩ :: after2.callers) = next1.callers := by
          rw [hC]
          simp only [projectCallers, List.map_cons, after2]
          congr 1
          · congr 1
            have hAdv : projectFrame proj (swapCtx s2.block s2.callers after1).advance.toMemoryFrame =
                after1.advance.toMemoryFrame := by
              have : (projectState proj (swapCtx s2.block s2.callers after1)).advance.toMemoryFrame =
                  after1.advance.toMemoryFrame := congrArg (fun x => x.advance.toMemoryFrame) hAfter1Eq
              simpa only [projectState_advance, projectState, swapCtx, MemoryRegisters.advance, projectFrame] using this
            exact hAdv
          · exact hc1.symm
        change swapCtx (proj next1.frame g2.entry)
            (projectCallers proj (⟨after2.advance.toMemoryFrame, dstOpt⟩ :: after2.callers))
            (swapCtx g2.entry (⟨after2.advance.toMemoryFrame, dstOpt⟩ :: after2.callers) next1) = next1
        rw [hCS, hF, hGCov.entry, ← hB, swapCtx_inv]

private theorem MIRCovering.transfer_forward {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {proj : FnId → BlockId → BlockId} {src dst : MIR lang profile}
    (h : MIRCovering proj src dst) {edge2 : CFGEdge (Sigma (MemoryRead _ lang.literal))}
    {s2 next2 : MemoryRegisters language profile}
    (hvb : ValidBlocks dst s2)
    (hTr : (dst.logicalView (language := language)).Transfer edge2 s2 next2 false) :
    (src.logicalView (language := language)).Transfer
        { edge2 with target := proj s2.frame edge2.target }
        (projectState proj s2) (projectState proj next2) false ∧
      ValidBlocks dst next2 := by
  have hT := h.logicalView_tablesAgree.symm
  obtain ⟨vals, after2, tgt2Log, hRead2, hBlk2, hTys, hPO2⟩ := hTr
  obtain ⟨hf2, hb2, hc2⟩ := readOperands_preserves_ctx hRead2
  obtain ⟨locals, g2, b2, hLu2, _⟩ := hvb.1
  rw [MIRCovering.logicalView_blockAt_eq] at hBlk2
  change ((AList.lookup dst.functions s2.frame).bind fun (_, g) =>
    g.blocks.find? (·.id == edge2.target)).map toLogicalBlock = some tgt2Log at hBlk2
  rw [hLu2, Option.bind_some] at hBlk2
  dsimp only at hBlk2
  cases hFindTgt2 : g2.blocks.find? (·.id == edge2.target) with
  | none => rw [hFindTgt2] at hBlk2; cases hBlk2
  | some tgt2 =>
    rw [hFindTgt2] at hBlk2
    cases Option.some.inj hBlk2
    have hTgtId : tgt2.id = edge2.target := mirBlock_id_of_find? hFindTgt2
    obtain ⟨g1, tgt1, hLu1, _, hFindTgt1, _, hP1, _, _, _⟩ := h.block_correspond hLu2 (hTgtId ▸ hFindTgt2)
    have hRead1 : (src.logicalView (language := language)).readOperands edge2.arguments (projectState proj s2) =
        some (vals, projectState proj after2) := by
      unfold projectState
      rw [hT.readOperands_eq, hRead2, Option.map_some, ← hf2, ← hb2, ← hc2]
    have hBlk1 : (src.logicalView (language := language)).blockAt
        { projectState proj s2 with block := proj s2.frame edge2.target } = some (toLogicalBlock tgt1) := by
      rw [MIRCovering.logicalView_blockAt_eq]
      change ((AList.lookup src.functions s2.frame).bind fun (_, g) =>
        g.blocks.find? (·.id == proj s2.frame edge2.target)).map toLogicalBlock = some (toLogicalBlock tgt1)
      rw [hLu1, Option.bind_some]; dsimp only; rw [← hTgtId, hFindTgt1]; rfl
    obtain ⟨hfn2, hbn2, hcn2⟩ := paramOutcome_preserves_ctx hPO2
    have hPO1 := hT.paramOutcome (proj s2.frame edge2.target) (projectCallers proj s2.callers) hPO2
    have hInit1Eq : swapCtx (proj s2.frame edge2.target) (projectCallers proj s2.callers)
        ({ after2 with block := edge2.target, instruction := 0 } : MemoryRegisters language profile) =
        ({ (projectState proj after2 : MemoryRegisters language profile) with
          block := proj s2.frame edge2.target, instruction := 0 } : MemoryRegisters language profile) := by
      simp only [swapCtx, projectState, hc2]
    have hNext1Eq : swapCtx (proj s2.frame edge2.target) (projectCallers proj s2.callers) next2 =
        projectState proj next2 := by
      unfold projectState
      have hf : next2.frame = s2.frame := hfn2.trans hf2
      have hb : next2.block = edge2.target := hbn2
      have hc : next2.callers = s2.callers := hcn2.trans hc2
      rw [hf, hb, hc]
    rw [hInit1Eq, hNext1Eq] at hPO1
    have hParamsEq : (toLogicalBlock tgt1).params = (toLogicalBlock tgt2).params := by
      simp [toLogicalBlock, hP1]
    constructor
    · refine ⟨vals, projectState proj after2, toLogicalBlock tgt1, hRead1, hBlk1,
        by rw [hParamsEq]; exact hTys, by rw [hParamsEq]; exact hPO1⟩
    · refine ⟨⟨locals, g2, tgt2, (hfn2.trans hf2).symm ▸ hLu2, hbn2.symm ▸ hFindTgt2⟩,
        fun c hc => hvb.2 c ((hcn2.trans hc2) ▸ hc)⟩

private theorem MIRCovering.transfer_backward {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {proj : FnId → BlockId → BlockId} {src dst : MIR lang profile}
    (h : MIRCovering proj src dst) {edge2 : CFGEdge (Sigma (MemoryRead _ lang.literal))}
    {s2 next1 : MemoryRegisters language profile}
    {locals : LocalTypes language.operands.Ty} {g2 : MIRGraph lang} {tgt2 : MIRBlock lang}
    (hLu2 : AList.lookup dst.functions s2.frame = some (locals, g2))
    (hFindTgt2 : g2.blocks.find? (·.id == edge2.target) = some tgt2)
    (hTr : (src.logicalView (language := language)).Transfer
        { edge2 with target := proj s2.frame edge2.target }
        (projectState proj s2) next1 false) :
    ∃ next2,
      (dst.logicalView (language := language)).Transfer edge2 s2 next2 false ∧
      projectState proj next2 = next1 := by
  have hT := h.logicalView_tablesAgree
  obtain ⟨vals, after1, tgt1Log, hRead1, hBlk1, hTys, hPO1⟩ := hTr
  obtain ⟨hf1, hb1, hc1⟩ := readOperands_preserves_ctx hRead1
  have hTgtId : tgt2.id = edge2.target := mirBlock_id_of_find? hFindTgt2
  obtain ⟨g1, tgt1, hLu1, _, hFindTgt1, _, hP1, _, _, _⟩ := h.block_correspond hLu2 (hTgtId ▸ hFindTgt2)
  have hBlk1Eq : (src.logicalView (language := language)).blockAt
      { projectState proj s2 with block := proj s2.frame edge2.target } = some (toLogicalBlock tgt1) := by
    rw [MIRCovering.logicalView_blockAt_eq]
    change ((AList.lookup src.functions s2.frame).bind fun (_, g) =>
      g.blocks.find? (·.id == proj s2.frame edge2.target)).map toLogicalBlock = some (toLogicalBlock tgt1)
    rw [hLu1, Option.bind_some]; dsimp only; rw [← hTgtId, hFindTgt1]; rfl
  cases Option.some.inj (hBlk1.symm.trans hBlk1Eq)
  have hRead2 := hT.readOperands_eq s2.block s2.callers edge2.arguments (projectState proj s2)
  rw [swapCtx_projectState, hRead1, Option.map_some] at hRead2
  let after2 := swapCtx s2.block s2.callers after1
  have hBlk2 : (dst.logicalView (language := language)).blockAt { s2 with block := edge2.target } =
      some (toLogicalBlock tgt2) := by
    rw [MIRCovering.logicalView_blockAt_eq]
    change ((AList.lookup dst.functions s2.frame).bind fun (_, g) =>
      g.blocks.find? (·.id == edge2.target)).map toLogicalBlock = some (toLogicalBlock tgt2)
    rw [hLu2, Option.bind_some]; dsimp only; rw [hFindTgt2]; rfl
  have hPO2 := hT.paramOutcome edge2.target s2.callers hPO1
  let next2 := swapCtx edge2.target s2.callers next1
  obtain ⟨hfn1, hbn1, hcn1⟩ := paramOutcome_preserves_ctx hPO1
  have hInit2Eq : swapCtx edge2.target s2.callers
      ({ after1 with block := proj s2.frame edge2.target, instruction := 0 } : MemoryRegisters language profile) =
      ({ (after2 : MemoryRegisters language profile) with block := edge2.target, instruction := 0 } :
        MemoryRegisters language profile) := by
    cases after1 with | mk f _ _ _ _ _ _ => cases f; rfl
  rw [hInit2Eq] at hPO2
  have hParamsEq : (toLogicalBlock tgt1).params = (toLogicalBlock tgt2).params := by
    simp [toLogicalBlock, hP1]
  refine ⟨next2, ⟨vals, after2, toLogicalBlock tgt2, hRead2, hBlk2,
    by rw [← hParamsEq]; exact hTys, by rw [← hParamsEq]; exact hPO2⟩, ?_⟩
  unfold projectState next2
  have hF : next1.frame = s2.frame := hfn1.trans hf1
  have hB : next1.block = proj s2.frame edge2.target := hbn1
  have hC : next1.callers = projectCallers proj s2.callers := hcn1.trans hc1
  change swapCtx (proj next1.frame edge2.target) (projectCallers proj s2.callers)
      (swapCtx edge2.target s2.callers next1) = next1
  rw [hF, ← hB, ← hC, swapCtx_inv]

private theorem MIRCovering.step_forward {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {proj : FnId → BlockId → BlockId} {src dst : MIR lang profile}
    (h : MIRCovering proj src dst) {s2 next2 : MemoryRegisters language profile}
    (hvb : ValidBlocks dst s2)
    (hStep : (Interfaces.MIR.Execution (language := language)).step dst s2 next2) :
    (Interfaces.MIR.Execution (language := language)).step src
        (projectState proj s2) (projectState proj next2) ∧
      ValidBlocks dst next2 := by
  have hT := h.logicalView_tablesAgree.symm
  obtain ⟨hNoRes2, hRaw2, hTr2⟩ := (mir_step_iff dst s2 next2).mp hStep
  have hRaw1AndVB : src.Step (language := language) (projectState proj s2) (projectState proj next2) ∧
      ValidBlocks dst next2 := by
    rw [mir_raw_step_iff] at hRaw2 ⊢
    rw [← h.instructionAt_eq hvb]
    cases hAt : mirInstructionAt dst s2 with
    | some op =>
      rw [hAt] at hRaw2
      obtain ⟨hNoTrap2, hIS2⟩ := hRaw2
      have hNoTrap1 : (src.logicalView (language := language)).instructionTrap (projectState proj s2)
          (toLogicalInst op.instruction) = none := by
        unfold projectState; rw [hT.instructionTrap_eq]; exact hNoTrap2
      have hLiftInst : ∀ (inst : MemoryInst lang),
          (dst.logicalView (language := language)).InstructionStep s2 next2 true inst →
          (src.logicalView (language := language)).InstructionStep
            (projectState proj s2) (projectState proj next2) true inst ∧
          ValidBlocks dst next2 := by
        intro inst hIS
        cases inst with
        | bind d rd =>
          obtain ⟨v, after2, stored2, hRd, hPO, rfl⟩ := hIS
          simp only [Bool.true_or, ↓reduceIte] at hPO
          obtain ⟨hf1, hb1, hc1⟩ := readValue_preserves_ctx hRd
          obtain ⟨hf2, hb2, hc2⟩ := paramOutcome_preserves_ctx hPO
          have hRd1 : (src.logicalView (language := language)).readValue (projectState proj s2) rd =
              some (v, projectState proj after2) := by
            unfold projectState; rw [hT.readValue_eq, hRd, Option.map_some, ← hf1, ← hb1, ← hc1]
          have hPO1 : (src.logicalView (language := language)).ParamOutcome [⟨_, d⟩] [v]
              (projectState proj after2) (projectState proj stored2) false := by
            have hp := hT.paramOutcome (proj s2.frame s2.block) (projectCallers proj s2.callers) hPO
            unfold projectState
            rwa [hf1, hb1, hc1, hf2, hb2, hc2, hf1, hb1, hc1]
          refine ⟨⟨v, projectState proj after2, projectState proj stored2, hRd1, hPO1,
            (projectState_advance proj stored2).symm⟩, ?_⟩
          obtain ⟨l0, g0, b0, hL0, hF0⟩ := hvb.1
          exact ⟨⟨l0, g0, b0, by simpa [MemoryRegisters.advance, hf2, hf1] using hL0,
            by simpa [MemoryRegisters.advance, hb2, hb1] using hF0⟩,
            fun c hc => hvb.2 c (by simpa [MemoryRegisters.advance, hc2, hc1] using hc)⟩
        | assign d rd =>
          obtain ⟨v, after2, stored2, hRd, hW, rfl⟩ := hIS
          obtain ⟨hf1, hb1, hc1⟩ := readValue_preserves_ctx hRd
          obtain ⟨hf2, hb2, hc2⟩ := writeValue_preserves_ctx hW.2.2.1
          have hRd1 : (src.logicalView (language := language)).readValue (projectState proj s2) rd =
              some (v, projectState proj after2) := by
            unfold projectState; rw [hT.readValue_eq, hRd, Option.map_some, ← hf1, ← hb1, ← hc1]
          have hW1 : (src.logicalView (language := language)).Writes (projectState proj after2) d v
              (projectState proj stored2) := by
            have hw := hT.writes (proj s2.frame s2.block) (projectCallers proj s2.callers) hW
            unfold projectState
            rwa [hf1, hb1, hc1, hf2, hb2, hc2, hf1, hb1, hc1]
          refine ⟨⟨v, projectState proj after2, projectState proj stored2, hRd1, hW1,
            (projectState_advance proj stored2).symm⟩, ?_⟩
          obtain ⟨l0, g0, b0, hL0, hF0⟩ := hvb.1
          exact ⟨⟨l0, g0, b0, by simpa [MemoryRegisters.advance, hf2, hf1] using hL0,
            by simpa [MemoryRegisters.advance, hb2, hb1] using hF0⟩,
            fun c hc => hvb.2 c (by simpa [MemoryRegisters.advance, hc2, hc1] using hc)⟩
        | op d oper args checks addrs =>
          obtain ⟨vals, after2, ops, res, effs, chg, stored2, hRA, hFT, hRes, hOp, hChg, hPO, rfl⟩ := hIS
          simp only [Bool.true_or, ↓reduceIte] at hPO
          obtain ⟨hf1, hb1, hc1⟩ := readArgs_preserves_ctx hRA
          obtain ⟨hfP, hbP, hcP⟩ := prepareAddresses_preserves_ctx after2 addrs
          have hAOpCtx : (Interfaces.Memory.applyOperation (Interfaces.Memory.prepareAddresses after2 addrs) true chg).frame = s2.frame ∧
              (Interfaces.Memory.applyOperation (Interfaces.Memory.prepareAddresses after2 addrs) true chg).block = s2.block := by
            with_unfolding_all
              exact ⟨hfP.trans hf1, hbP.trans hb1⟩
          obtain ⟨hf3, hb3, hc3⟩ := paramOutcome_preserves_ctx hPO
          have hRA1 : (src.logicalView (language := language)).readArgs args (projectState proj s2) =
              some (vals, projectState proj after2) := by
            unfold projectState; rw [hT.readArgs_eq, hRA, Option.map_some, ← hf1, ← hb1, ← hc1]
          have hPrep1 : Interfaces.Memory.prepareAddresses (projectState proj after2) addrs =
              projectState proj (Interfaces.Memory.prepareAddresses after2 addrs) := by
            unfold projectState
            rw [swapCtx_prepareAddresses, hfP, hbP, hcP]
          have hRes1 : addrs.mapM ((src.logicalView (language := language)).resolveAddress
              (Interfaces.Memory.prepareAddresses (projectState proj after2) addrs) true) = some ops := by
            rw [hPrep1]
            have hMapEq : ∀ xs : List (Sigma (MemoryPlace language.operands.Ty)),
                xs.mapM ((src.logicalView (language := language)).resolveAddress
                  (projectState proj (Interfaces.Memory.prepareAddresses after2 addrs)) true) =
                xs.mapM ((dst.logicalView (language := language)).resolveAddress
                  (Interfaces.Memory.prepareAddresses after2 addrs) true) := by
              intro xs
              induction xs with
              | nil => rfl
              | cons a rest ih =>
                rw [List.mapM_cons, List.mapM_cons, ih]
                unfold projectState
                rw [hT.resolveAddress_eq]
            rw [hMapEq]; exact hRes
          have hStore1 : Interfaces.Memory.operationStore
              (Interfaces.Memory.prepareAddresses (projectState proj after2) addrs) true =
              Interfaces.Memory.operationStore (Interfaces.Memory.prepareAddresses after2 addrs) true := by
            rw [hPrep1, operationStore_projectState]
          have hApp1 : Interfaces.Memory.applyOperation
              (Interfaces.Memory.prepareAddresses (projectState proj after2) addrs) true chg =
              projectState proj (Interfaces.Memory.applyOperation (Interfaces.Memory.prepareAddresses after2 addrs) true chg) := by
            rw [hPrep1, applyOperation_projectState]
          have hPO1 : (src.logicalView (language := language)).ParamOutcome [⟨_, d⟩] [res]
              (Interfaces.Memory.applyOperation (Interfaces.Memory.prepareAddresses (projectState proj after2) addrs) true chg)
              (projectState proj stored2) false := by
            rw [hApp1]
            have hp := hT.paramOutcome
              (proj (Interfaces.Memory.applyOperation (Interfaces.Memory.prepareAddresses after2 addrs) true chg).frame
                (Interfaces.Memory.applyOperation (Interfaces.Memory.prepareAddresses after2 addrs) true chg).block)
              (projectCallers proj (Interfaces.Memory.applyOperation (Interfaces.Memory.prepareAddresses after2 addrs) true chg).callers)
              hPO
            have hS : swapCtx
                (proj (Interfaces.Memory.applyOperation (Interfaces.Memory.prepareAddresses after2 addrs) true chg).frame
                  (Interfaces.Memory.applyOperation (Interfaces.Memory.prepareAddresses after2 addrs) true chg).block)
                (projectCallers proj (Interfaces.Memory.applyOperation (Interfaces.Memory.prepareAddresses after2 addrs) true chg).callers)
                stored2 = projectState proj stored2 := by
              unfold projectState; rw [hf3, hb3, hc3]
            rwa [hS] at hp
          refine ⟨⟨vals, projectState proj after2, ops, res, effs, chg, projectState proj stored2,
            hRA1, hFT, hRes1, ?_, by rw [hStore1]; exact hChg, hPO1, ?_⟩, ?_⟩
          · rw [hStore1]
            change language.operation src.declarations _ _ _ _ _ _ _ _ _ _
            rw [← h.declarations]; exact hOp
          · cases stored2 with | mk f _ _ _ _ _ _ => cases f; rfl
          · obtain ⟨l0, g0, b0, hL0, hF0⟩ := hvb.1
            refine ⟨⟨l0, g0, b0, by simpa [MemoryRegisters.advance, hf3, hAOpCtx.1] using hL0,
              by simpa [MemoryRegisters.advance, hb3, hAOpCtx.2] using hF0⟩, ?_⟩
            intro c hc
            have hc' : c ∈ (Interfaces.Memory.applyOperation (Interfaces.Memory.prepareAddresses after2 addrs) true chg).callers :=
              hc3 ▸ hc
            with_unfolding_all
              simp only [Interfaces.Memory.applyOperation, List.mem_map] at hc'
              obtain ⟨c0, hc0, rfl⟩ := hc'
              obtain ⟨lc, gc, bc, hLc, hFc⟩ := hvb.2 c0 (hc1 ▸ (hcP ▸ hc0))
              exact ⟨lc, gc, bc, hLc, hFc⟩
        | addWithOverflow t d fl lhs rhs =>
          obtain ⟨hs1, hs2, hdisj, lv, afterL, rv, afterR, x, y, stored2, hL, hR, hx, hy, hPO, rfl⟩ := hIS
          simp only [Bool.true_or, ↓reduceIte] at hPO
          obtain ⟨hfL, hbL, hcL⟩ := readValue_preserves_ctx hL
          obtain ⟨hfR, hbR, hcR⟩ := readValue_preserves_ctx hR
          obtain ⟨hfS, hbS, hcS⟩ := paramOutcome_preserves_ctx hPO
          have hL1 : (src.logicalView (language := language)).readValue (projectState proj s2) lhs =
              some (lv, projectState proj afterL) := by
            unfold projectState; rw [hT.readValue_eq, hL, Option.map_some, ← hfL, ← hbL, ← hcL]
          have hR1 : (src.logicalView (language := language)).readValue (projectState proj afterL) rhs =
              some (rv, projectState proj afterR) := by
            unfold projectState; rw [hT.readValue_eq, hR, Option.map_some, ← hfR, ← hbR, ← hcR]
          have hPO1 := hT.paramOutcome (proj afterR.frame afterR.block) (projectCallers proj afterR.callers) hPO
          have hStEq : swapCtx (proj afterR.frame afterR.block) (projectCallers proj afterR.callers) stored2 =
              projectState proj stored2 := by unfold projectState; rw [hfS, hbS, hcS]
          rw [hStEq] at hPO1
          refine ⟨⟨hs1, hs2, hdisj, lv, projectState proj afterL, rv, projectState proj afterR,
            x, y, projectState proj stored2, hL1, hR1, hx, hy, hPO1, (projectState_advance proj stored2).symm⟩, ?_⟩
          obtain ⟨l0, g0, b0, hL0, hF0⟩ := hvb.1
          exact ⟨⟨l0, g0, b0, by simpa [MemoryRegisters.advance, hfS, hfR, hfL] using hL0,
            by simpa [MemoryRegisters.advance, hbS, hbR, hbL] using hF0⟩,
            fun c hc => hvb.2 c (by simpa [MemoryRegisters.advance, hcS, hcR, hcL] using hc)⟩
        | beginLifetime r =>
          obtain ⟨st2, hB, rfl⟩ := hIS
          obtain ⟨hf1, hb1, hc1⟩ := begins_preserves_ctx hB
          have hB1 := hT.begins (proj s2.frame s2.block) (projectCallers proj s2.callers) hB
          have hStEq : swapCtx (proj s2.frame s2.block) (projectCallers proj s2.callers) st2 =
              projectState proj st2 := by unfold projectState; rw [hf1, hb1, hc1]
          rw [hStEq] at hB1
          refine ⟨⟨projectState proj st2, hB1, (projectState_advance proj st2).symm⟩, ?_⟩
          obtain ⟨l0, g0, b0, hL0, hF0⟩ := hvb.1
          exact ⟨⟨l0, g0, b0, by simpa [MemoryRegisters.advance, hf1] using hL0,
            by simpa [MemoryRegisters.advance, hb1] using hF0⟩,
            fun c hc => hvb.2 c (by simpa [MemoryRegisters.advance, hc1] using hc)⟩
        | endLifetime r =>
          obtain ⟨en2, hE, rfl⟩ := hIS
          obtain ⟨hf1, hb1, hc1⟩ := ends_preserves_ctx hE
          have hE1 := hT.ends (proj s2.frame s2.block) (projectCallers proj s2.callers) hE
          have hEnEq : swapCtx (proj s2.frame s2.block) (projectCallers proj s2.callers) en2 =
              projectState proj en2 := by unfold projectState; rw [hf1, hb1, hc1]
          rw [hEnEq] at hE1
          refine ⟨⟨projectState proj en2, hE1, (projectState_advance proj en2).symm⟩, ?_⟩
          obtain ⟨l0, g0, b0, hL0, hF0⟩ := hvb.1
          exact ⟨⟨l0, g0, b0, by simpa [MemoryRegisters.advance, hf1] using hL0,
            by simpa [MemoryRegisters.advance, hb1] using hF0⟩,
            fun c hc => hvb.2 c (by simpa [MemoryRegisters.advance, hc1] using hc)⟩
        | @destructure tyD srcP bindings =>
          obtain ⟨v, after2, flds, stored2, hRd, hDF, hFlds, hPO, rfl⟩ := hIS
          obtain ⟨hf1, hb1, hc1⟩ := readValue_preserves_ctx hRd
          obtain ⟨hf2, hb2, hc2⟩ := paramOutcome_preserves_ctx hPO
          have hRd1 : (src.logicalView (language := language)).readValue (projectState proj s2)
              (if language.operands.isCopy tyD = true then .copy srcP else .move srcP) =
              some (v, projectState proj after2) := by
            unfold projectState; rw [hT.readValue_eq, hRd, Option.map_some, ← hf1, ← hb1, ← hc1]
          have hPO1 := hT.paramOutcome (proj after2.frame after2.block) (projectCallers proj after2.callers) hPO
          have hStEq : swapCtx (proj after2.frame after2.block) (projectCallers proj after2.callers) stored2 =
              projectState proj stored2 := by unfold projectState; rw [hf2, hb2, hc2]
          rw [hStEq] at hPO1
          refine ⟨⟨v, projectState proj after2, flds, projectState proj stored2, hRd1,
            by rw [hT.declarations.symm]; exact hDF, hFlds, hPO1, (projectState_advance proj stored2).symm⟩, ?_⟩
          obtain ⟨l0, g0, b0, hL0, hF0⟩ := hvb.1
          exact ⟨⟨l0, g0, b0, by simpa [MemoryRegisters.advance, hf2, hf1] using hL0,
            by simpa [MemoryRegisters.advance, hb2, hb1] using hF0⟩,
            fun c hc => hvb.2 c (by simpa [MemoryRegisters.advance, hc2, hc1] using hc)⟩
        | call callee dstOpt args checks =>
          obtain ⟨hNT, hC⟩ := hIS
          obtain ⟨hC1, hvb'⟩ := h.call_forward hvb hC
          refine ⟨⟨?_, hC1⟩, hvb'⟩
          unfold projectState; rw [hT.instructionTrap_eq]; exact hNT
      obtain ⟨hIS1, hvb'⟩ := hLiftInst _ hIS2
      exact ⟨⟨hNoTrap1, hIS1⟩, hvb'⟩
    | none =>
      rw [hAt] at hRaw2
      obtain ⟨blk2Log, hBlk2, hPC2, hCS2⟩ := hRaw2
      obtain ⟨locals, g2, b2, hLu2, hFind2⟩ := hvb.1
      have hb2Id : b2.id = s2.block := mirBlock_id_of_find? hFind2
      obtain ⟨g1, b1, hLu1, _, hFind1, _, _, hBd1, _, hEnd1⟩ := h.block_correspond hLu2 (hb2Id ▸ hFind2)
      have hBlk2Eq : (dst.logicalView (language := language)).blockAt s2 = some (toLogicalBlock b2) := by
        rw [MIRCovering.logicalView_blockAt_eq, hLu2]; simp [hFind2]
      cases Option.some.inj (hBlk2.symm.trans hBlk2Eq)
      have hBlk1Eq : (src.logicalView (language := language)).blockAt (projectState proj s2) = some (toLogicalBlock b1) := by
        rw [MIRCovering.logicalView_blockAt_eq]
        change ((AList.lookup src.functions s2.frame).bind fun (_, g) =>
          g.blocks.find? (·.id == proj s2.frame s2.block)).map toLogicalBlock = some (toLogicalBlock b1)
        rw [hLu1, ← hb2Id]; simp [hFind1]
      have hPC1 : (projectState proj s2).instruction = (toLogicalBlock b1).body.length := by
        simpa only [projectState, swapCtx, toLogicalBlock, hBd1] using hPC2
      dsimp only [toLogicalBlock] at hCS2
      cases hb2End : b2.ending with
      | trap _ _ => rw [hb2End] at hCS2; cases hCS2
      | invalid => rw [hb2End] at hCS2; cases hCS2
      | jump e2 =>
        rw [hb2End] at hCS2 hEnd1
        obtain ⟨hTr1, hvb'⟩ := h.transfer_forward hvb hCS2
        refine ⟨⟨toLogicalBlock b1, hBlk1Eq, hPC1, ?_⟩, hvb'⟩
        dsimp only [toLogicalBlock]; rw [hEnd1]; exact hTr1
      | branch c y2 n2 =>
        rw [hb2End] at hCS2 hEnd1
        obtain ⟨v, after2, fl, hRd2, hTest, hTr2'⟩ := hCS2
        obtain ⟨hf1, hb1, hc1⟩ := readValue_preserves_ctx hRd2
        have hRd1 : (src.logicalView (language := language)).readValue (projectState proj s2) (toLogicalRead c) =
            some (v, projectState proj after2) := by
          unfold projectState; rw [hT.readValue_eq, hRd2, Option.map_some, ← hf1, ← hb1, ← hc1]
        have hvbAfter : ValidBlocks dst after2 := by
          obtain ⟨l0, g0, b0, hL0, hF0⟩ := hvb.1
          exact ⟨⟨l0, g0, b0, hf1.symm ▸ hL0, hb1.symm ▸ hF0⟩, fun c hc => hvb.2 c (hc1 ▸ hc)⟩
        obtain ⟨hTr1, hvb'⟩ := h.transfer_forward hvbAfter hTr2'
        refine ⟨⟨toLogicalBlock b1, hBlk1Eq, hPC1, ?_⟩, hvb'⟩
        dsimp only [toLogicalBlock]; rw [hEnd1]
        refine ⟨v, projectState proj after2, fl, hRd1, hTest, ?_⟩
        cases fl <;> simpa only [hf1, toLogicalEdge, ↓reduceIte, Bool.false_eq_true] using hTr1
      | ret opt =>
        rw [hb2End] at hCS2 hEnd1
        obtain ⟨caller2, rest2, vals, after2, ended2, hCallers2, hRd2, hEndL2, hTys, hPO2⟩ := hCS2
        obtain ⟨hf1, hb1, hc1⟩ := readOperands_preserves_ctx hRd2
        obtain ⟨hf2, hb2, hc2⟩ := endLocals_preserves_ctx hEndL2
        obtain ⟨hfn2, hbn2, hcn2⟩ := paramOutcome_preserves_ctx hPO2
        have hRd1 : (src.logicalView (language := language)).readOperands (opt.map fun a => ⟨a.1, toLogicalRead a.2⟩).toList
            (projectState proj s2) = some (vals, projectState proj after2) := by
          unfold projectState; rw [hT.readOperands_eq, hRd2, Option.map_some, ← hf1, ← hb1, ← hc1]
        have hEndL1 := hT.endLocals (proj after2.frame after2.block) (projectCallers proj after2.callers) hEndL2
        have hEnd1Eq : swapCtx (proj after2.frame after2.block) (projectCallers proj after2.callers) ended2 =
            projectState proj ended2 := by unfold projectState; rw [hf2, hb2, hc2]
        rw [hEnd1Eq] at hEndL1
        have hPO1 := hT.paramOutcome (proj caller2.caller.frame caller2.caller.block)
          (projectCallers proj rest2) hPO2
        have hRes1Eq : swapCtx (proj caller2.caller.frame caller2.caller.block) (projectCallers proj rest2)
            ({ toMemoryFrame := caller2.caller, bytes := ended2.bytes, callers := rest2,
               effects := ended2.effects, ownership := ended2.ownership,
               nextAddressId := ended2.nextAddressId } : MemoryRegisters language profile) =
            ({ toMemoryFrame := projectFrame proj caller2.caller, bytes := (projectState proj ended2).bytes,
               callers := projectCallers proj rest2, effects := (projectState proj ended2).effects,
               ownership := (projectState proj ended2).ownership,
               nextAddressId := (projectState proj ended2).nextAddressId } : MemoryRegisters language profile) := rfl
        have hNext1Eq : swapCtx (proj caller2.caller.frame caller2.caller.block) (projectCallers proj rest2) next2 =
            projectState proj next2 := by unfold projectState; rw [hfn2, hbn2, hcn2]
        rw [hRes1Eq, hNext1Eq] at hPO1
        refine ⟨⟨toLogicalBlock b1, hBlk1Eq, hPC1, ?_⟩, ?_⟩
        · dsimp only [toLogicalBlock]; rw [hEnd1]
          refine ⟨{ caller2 with caller := projectFrame proj caller2.caller }, projectCallers proj rest2,
            vals, projectState proj after2, projectState proj ended2, ?_, hRd1, hEndL1, hTys, hPO1⟩
          change projectCallers proj s2.callers = _; rw [hCallers2]; rfl
        · refine ⟨?_, fun c hc => hvb.2 c (hCallers2 ▸ .tail _ (by rw [hcn2] at hc; exact hc))⟩
          obtain ⟨lc, gc, bc, hLc, hFc⟩ := hvb.2 caller2 (hCallers2 ▸ .head _)
          exact ⟨lc, gc, bc, by rw [hfn2]; exact hLc, by rw [hbn2]; exact hFc⟩
  obtain ⟨hRaw1, hvb'⟩ := hRaw1AndVB
  refine ⟨(mir_step_iff src (projectState proj s2) (projectState proj next2)).mpr ⟨?_, hRaw1, ?_⟩, hvb'⟩
  · rw [← h.hostedResult_eq hvb]; exact hNoRes2
  · rw [← h.trace_eq hvb, ← h.trace_eq hvb']
    exact hTr2

private theorem MIRCovering.step_backward {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {proj : FnId → BlockId → BlockId} {src dst : MIR lang profile}
    (h : MIRCovering proj src dst) {s2 next1 : MemoryRegisters language profile}
    (hvb : ValidBlocks dst s2)
    (hStep : (Interfaces.MIR.Execution (language := language)).step src (projectState proj s2) next1) :
    ∃ next2,
      (Interfaces.MIR.Execution (language := language)).step dst s2 next2 ∧
      projectState proj next2 = next1 := by
  have hT := h.logicalView_tablesAgree
  obtain ⟨hNoRes1, hRaw1, hTr1⟩ := (mir_step_iff src (projectState proj s2) next1).mp hStep
  have hRaw2Ex : ∃ next2, dst.Step (language := language) s2 next2 ∧
      projectState proj next2 = next1 ∧ ValidBlocks dst next2 := by
    rw [mir_raw_step_iff] at hRaw1
    rw [← h.instructionAt_eq hvb] at hRaw1
    cases hAt : mirInstructionAt dst s2 with
    | some op =>
      rw [hAt] at hRaw1
      obtain ⟨hNoTrap1, hIS1⟩ := hRaw1
      have hNoTrap2 : (dst.logicalView (language := language)).instructionTrap s2
          (toLogicalInst op.instruction) = none := by
        have := hT.instructionTrap_eq s2.block s2.callers (projectState proj s2)
          (toLogicalInst op.instruction)
        rwa [swapCtx_projectState, hNoTrap1] at this
      have hLiftBack : ∀ (inst : MemoryInst lang),
          (src.logicalView (language := language)).InstructionStep (projectState proj s2) next1 true inst →
          ∃ next2, (dst.logicalView (language := language)).InstructionStep s2 next2 true inst ∧
            projectState proj next2 = next1 ∧ ValidBlocks dst next2 := by
        intro inst hIS
        cases inst with
        | bind d rd =>
          obtain ⟨v, after1, stored1, hRd1, hPO1, rfl⟩ := hIS
          simp only [Bool.true_or, ↓reduceIte] at hPO1
          obtain ⟨hf1, hb1, hc1⟩ := readValue_preserves_ctx hRd1
          obtain ⟨hf2, hb2, hc2⟩ := paramOutcome_preserves_ctx hPO1
          have hRd2 := hT.readValue_eq s2.block s2.callers (projectState proj s2) rd
          rw [swapCtx_projectState, hRd1, Option.map_some] at hRd2
          have hPO2 := hT.paramOutcome s2.block s2.callers hPO1
          let stored2 := swapCtx s2.block s2.callers stored1
          refine ⟨stored2.advance, ⟨v, swapCtx s2.block s2.callers after1, stored2, hRd2, hPO2, rfl⟩, ?_, ?_⟩
          · rw [← projectState_advance]
            exact congrArg MemoryRegisters.advance (projectState_swapCtx_of_ctx proj (hf2.trans hf1) (hb2.trans hb1) (hc2.trans hc1))
          · obtain ⟨l0, g0, b0, hL0, hF0⟩ := hvb.1
            have hF : stored1.frame = s2.frame := hf2.trans hf1
            exact ⟨⟨l0, g0, b0, by simpa [stored2, swapCtx, MemoryRegisters.advance, hF] using hL0, hF0⟩,
              fun c hc => hvb.2 c hc⟩
        | assign d rd =>
          obtain ⟨v, after1, stored1, hRd1, hW1, rfl⟩ := hIS
          obtain ⟨hf1, hb1, hc1⟩ := readValue_preserves_ctx hRd1
          obtain ⟨hf2, hb2, hc2⟩ := writeValue_preserves_ctx hW1.2.2.1
          have hRd2 := hT.readValue_eq s2.block s2.callers (projectState proj s2) rd
          rw [swapCtx_projectState, hRd1, Option.map_some] at hRd2
          have hW2 := hT.writes s2.block s2.callers hW1
          let stored2 := swapCtx s2.block s2.callers stored1
          refine ⟨stored2.advance, ⟨v, swapCtx s2.block s2.callers after1, stored2, hRd2, hW2, rfl⟩, ?_, ?_⟩
          · rw [← projectState_advance]
            exact congrArg MemoryRegisters.advance (projectState_swapCtx_of_ctx proj (hf2.trans hf1) (hb2.trans hb1) (hc2.trans hc1))
          · obtain ⟨l0, g0, b0, hL0, hF0⟩ := hvb.1
            have hF : stored1.frame = s2.frame := hf2.trans hf1
            exact ⟨⟨l0, g0, b0, by simpa [stored2, swapCtx, MemoryRegisters.advance, hF] using hL0, hF0⟩,
              fun c hc => hvb.2 c hc⟩
        | op d oper args checks addrs =>
          obtain ⟨vals, after1, ops, res, effs, chg, stored1, hRA1, hFT, hRes1, hOp1, hChg1, hPO1, rfl⟩ := hIS
          simp only [Bool.true_or, ↓reduceIte] at hPO1
          obtain ⟨hf1, hb1, hc1⟩ := readArgs_preserves_ctx hRA1
          have hRA2 := hT.readArgs_eq s2.block s2.callers args (projectState proj s2)
          rw [swapCtx_projectState, hRA1, Option.map_some] at hRA2
          let after2 := swapCtx s2.block s2.callers after1
          have hAfter1Eq : projectState proj after2 = after1 :=
            projectState_swapCtx_of_ctx proj hf1 hb1 hc1
          let ready2 := Interfaces.Memory.prepareAddresses after2 addrs
          obtain ⟨hfP, hbP, hcP⟩ := prepareAddresses_preserves_ctx after2 addrs
          have hReady1Eq : projectState proj ready2 = Interfaces.Memory.prepareAddresses after1 addrs := by
            rw [← hAfter1Eq]; unfold projectState
            rw [swapCtx_prepareAddresses, hfP, hbP, hcP]
          have hRes2 : addrs.mapM ((dst.logicalView (language := language)).resolveAddress ready2 true) = some ops := by
            have hMapEq : ∀ xs : List (Sigma (MemoryPlace language.operands.Ty)),
                xs.mapM ((dst.logicalView (language := language)).resolveAddress ready2 true) =
                xs.mapM ((src.logicalView (language := language)).resolveAddress (projectState proj ready2) true) := by
              intro xs
              induction xs with
              | nil => rfl
              | cons a rest ih =>
                rw [List.mapM_cons, List.mapM_cons, ih]
                unfold projectState
                rw [hT.symm.resolveAddress_eq]
            rw [hMapEq, hReady1Eq]; exact hRes1
          have hStoreEq : Interfaces.Memory.operationStore ready2 true =
              Interfaces.Memory.operationStore (Interfaces.Memory.prepareAddresses after1 addrs) true := by
            rw [← hReady1Eq, operationStore_projectState]
          let app2 := Interfaces.Memory.applyOperation ready2 true chg
          have hApp1Eq : projectState proj app2 =
              Interfaces.Memory.applyOperation (Interfaces.Memory.prepareAddresses after1 addrs) true chg := by
            rw [← hReady1Eq, applyOperation_projectState]
          have hPO2 := hT.paramOutcome app2.block app2.callers (hApp1Eq ▸ hPO1)
          rw [swapCtx_projectState] at hPO2
          let stored2 := swapCtx app2.block app2.callers stored1
          obtain ⟨hfS, hbS, hcS⟩ := paramOutcome_preserves_ctx hPO1
          have hStored1Eq : projectState proj stored2 = stored1 :=
            projectState_swapCtx_of_ctx proj (hfS.trans (congrArg (fun x => x.frame) hApp1Eq.symm))
              (hbS.trans (congrArg (fun x => x.block) hApp1Eq.symm))
              (hcS.trans (congrArg (fun x => x.callers) hApp1Eq.symm))
          refine ⟨{ stored2.advance with effects := stored2.effects ++ effs },
            ⟨vals, after2, ops, res, effs, chg, stored2, hRA2, hFT, hRes2,
              by rw [hStoreEq]; change language.operation dst.declarations _ _ _ _ _ _ _ _ _ _; rw [h.declarations]; exact hOp1,
              by rw [hStoreEq]; exact hChg1, hPO2, rfl⟩, ?_, ?_⟩
          · have : ({ ((projectState proj stored2).advance : MemoryRegisters language profile) with
                effects := (projectState proj stored2).effects ++ effs } : MemoryRegisters language profile) =
                { stored1.advance with effects := stored1.effects ++ effs } := by rw [hStored1Eq]
            simpa only [projectState, swapCtx, MemoryRegisters.advance] using this
          · obtain ⟨l0, g0, b0, hL0, hF0⟩ := hvb.1
            have hfApp2 : app2.frame = s2.frame := by
              with_unfolding_all exact hfP.trans hf1
            have hbApp2 : app2.block = s2.block := by
              with_unfolding_all exact hbP
            refine ⟨⟨l0, g0, b0, by
                change AList.lookup dst.functions stored1.frame = some (l0, g0)
                have hfA : stored1.frame = s2.frame := hfS.trans (by rw [← hApp1Eq]; exact hfApp2)
                rw [hfA]; exact hL0,
              by change g0.blocks.find? (·.id == app2.block) = some b0; rw [hbApp2]; exact hF0⟩, ?_⟩
            intro c hc
            change c ∈ app2.callers at hc
            with_unfolding_all
              simp only [app2, Interfaces.Memory.applyOperation, List.mem_map] at hc
              obtain ⟨c0, hc0, rfl⟩ := hc
              obtain ⟨lc, gc, bc, hLc, hFc⟩ := hvb.2 c0 (hcP ▸ hc0)
              exact ⟨lc, gc, bc, hLc, hFc⟩
        | addWithOverflow t d fl lhs rhs =>
          obtain ⟨hs1, hs2, hdisj, lv, afterL1, rv, afterR1, x, y, stored1, hL1, hR1, hx, hy, hPO1, rfl⟩ := hIS
          simp only [Bool.true_or, ↓reduceIte] at hPO1
          obtain ⟨hfL, hbL, hcL⟩ := readValue_preserves_ctx hL1
          obtain ⟨hfR, hbR, hcR⟩ := readValue_preserves_ctx hR1
          obtain ⟨hfS, hbS, hcS⟩ := paramOutcome_preserves_ctx hPO1
          have hL2 := hT.readValue_eq s2.block s2.callers (projectState proj s2) lhs
          rw [swapCtx_projectState, hL1, Option.map_some] at hL2
          have hR2 := hT.readValue_eq s2.block s2.callers afterL1 rhs
          rw [hR1, Option.map_some] at hR2
          have hPO2 := hT.paramOutcome s2.block s2.callers hPO1
          let stored2 := swapCtx s2.block s2.callers stored1
          refine ⟨stored2.advance, ⟨hs1, hs2, hdisj, lv, swapCtx s2.block s2.callers afterL1,
            rv, swapCtx s2.block s2.callers afterR1, x, y, stored2, hL2, hR2, hx, hy, hPO2, rfl⟩, ?_, ?_⟩
          · rw [← projectState_advance]
            exact congrArg MemoryRegisters.advance
              (projectState_swapCtx_of_ctx proj (hfS.trans (hfR.trans hfL)) (hbS.trans (hbR.trans hbL)) (hcS.trans (hcR.trans hcL)))
          · obtain ⟨l0, g0, b0, hL0, hF0⟩ := hvb.1
            have hF : stored1.frame = s2.frame := hfS.trans (hfR.trans hfL)
            exact ⟨⟨l0, g0, b0, by simpa [stored2, swapCtx, MemoryRegisters.advance, hF] using hL0, hF0⟩,
              fun c hc => hvb.2 c hc⟩
        | beginLifetime r =>
          obtain ⟨st1, hB1, rfl⟩ := hIS
          obtain ⟨hf1, hb1, hc1⟩ := begins_preserves_ctx hB1
          have hB2 := hT.begins s2.block s2.callers hB1
          rw [swapCtx_projectState] at hB2
          let st2 := swapCtx s2.block s2.callers st1
          refine ⟨st2.advance, ⟨st2, hB2, rfl⟩, ?_, ?_⟩
          · rw [← projectState_advance]
            exact congrArg MemoryRegisters.advance (projectState_swapCtx_of_ctx proj hf1 hb1 hc1)
          · obtain ⟨l0, g0, b0, hL0, hF0⟩ := hvb.1
            have hF : st1.frame = s2.frame := hf1
            exact ⟨⟨l0, g0, b0, by simpa [st2, swapCtx, MemoryRegisters.advance, hF] using hL0, hF0⟩,
              fun c hc => hvb.2 c hc⟩
        | endLifetime r =>
          obtain ⟨en1, hE1, rfl⟩ := hIS
          obtain ⟨hf1, hb1, hc1⟩ := ends_preserves_ctx hE1
          have hE2 := hT.ends s2.block s2.callers hE1
          rw [swapCtx_projectState] at hE2
          let en2 := swapCtx s2.block s2.callers en1
          refine ⟨en2.advance, ⟨en2, hE2, rfl⟩, ?_, ?_⟩
          · rw [← projectState_advance]
            exact congrArg MemoryRegisters.advance (projectState_swapCtx_of_ctx proj hf1 hb1 hc1)
          · obtain ⟨l0, g0, b0, hL0, hF0⟩ := hvb.1
            have hF : en1.frame = s2.frame := hf1
            exact ⟨⟨l0, g0, b0, by simpa [en2, swapCtx, MemoryRegisters.advance, hF] using hL0, hF0⟩,
              fun c hc => hvb.2 c hc⟩
        | @destructure tyD srcP bindings =>
          obtain ⟨v, after1, flds, stored1, hRd1, hDF, hFlds, hPO1, rfl⟩ := hIS
          obtain ⟨hf1, hb1, hc1⟩ := readValue_preserves_ctx hRd1
          obtain ⟨hf2, hb2, hc2⟩ := paramOutcome_preserves_ctx hPO1
          have hRd2 := hT.readValue_eq s2.block s2.callers (projectState proj s2)
            (if language.operands.isCopy tyD = true then .copy srcP else .move srcP)
          rw [swapCtx_projectState, hRd1, Option.map_some] at hRd2
          have hPO2 := hT.paramOutcome s2.block s2.callers hPO1
          let stored2 := swapCtx s2.block s2.callers stored1
          refine ⟨stored2.advance, ⟨v, swapCtx s2.block s2.callers after1, flds, stored2, hRd2,
            by rw [hT.declarations.symm]; exact hDF,
            hFlds, hPO2, rfl⟩, ?_, ?_⟩
          · rw [← projectState_advance]
            exact congrArg MemoryRegisters.advance (projectState_swapCtx_of_ctx proj (hf2.trans hf1) (hb2.trans hb1) (hc2.trans hc1))
          · obtain ⟨l0, g0, b0, hL0, hF0⟩ := hvb.1
            have hF : stored1.frame = s2.frame := hf2.trans hf1
            exact ⟨⟨l0, g0, b0, by simpa [stored2, swapCtx, MemoryRegisters.advance, hF] using hL0, hF0⟩,
              fun c hc => hvb.2 c hc⟩
        | call callee dstOpt args checks =>
          obtain ⟨hNT1, hC1⟩ := hIS
          obtain ⟨next2, hC2, hProj⟩ := h.call_backward hC1
          have hNT2 : (dst.logicalView (language := language)).instructionTrap s2 (.call callee dstOpt args checks) = none := by
            have := hT.instructionTrap_eq s2.block s2.callers (projectState proj s2) (.call callee dstOpt args checks)
            rwa [swapCtx_projectState, hNT1] at this
          have hvb' : ValidBlocks dst next2 := (h.call_forward hvb hC2).2
          exact ⟨next2, ⟨hNT2, hC2⟩, hProj, hvb'⟩
      obtain ⟨next2, hIS2, hProj, hvb'⟩ := hLiftBack _ hIS1
      refine ⟨next2, (mir_raw_step_iff dst s2 next2).mpr ?_, hProj, hvb'⟩
      rw [hAt]; exact ⟨hNoTrap2, hIS2⟩
    | none =>
      rw [hAt] at hRaw1
      obtain ⟨blk1Log, hBlk1, hPC1, hCS1⟩ := hRaw1
      obtain ⟨locals, g2, b2, hLu2, hFind2⟩ := hvb.1
      have hb2Id : b2.id = s2.block := mirBlock_id_of_find? hFind2
      obtain ⟨g1, b1, hLu1, hGCov, hFind1, _, _, hBd1, _, hEnd1⟩ := h.block_correspond hLu2 (hb2Id ▸ hFind2)
      have hBlk1Eq : (src.logicalView (language := language)).blockAt (projectState proj s2) = some (toLogicalBlock b1) := by
        rw [MIRCovering.logicalView_blockAt_eq]
        change ((AList.lookup src.functions s2.frame).bind fun (_, g) =>
          g.blocks.find? (·.id == proj s2.frame s2.block)).map toLogicalBlock = some (toLogicalBlock b1)
        rw [hLu1, ← hb2Id]; simp [hFind1]
      cases Option.some.inj (hBlk1.symm.trans hBlk1Eq)
      have hBlk2Eq : (dst.logicalView (language := language)).blockAt s2 = some (toLogicalBlock b2) := by
        rw [MIRCovering.logicalView_blockAt_eq, hLu2]; simp [hFind2]
      have hPC2 : s2.instruction = (toLogicalBlock b2).body.length := by
        simpa only [projectState, swapCtx, toLogicalBlock, hBd1] using hPC1
      have hb2Mem : b2 ∈ g2.blocks.toList := Array.mem_toList_iff.mpr (Array.mem_of_find?_eq_some hFind2)
      dsimp only [toLogicalBlock] at hCS1
      rw [hEnd1] at hCS1
      cases hb2End : b2.ending with
      | trap _ _ => rw [hb2End] at hCS1; cases hCS1
      | invalid => rw [hb2End] at hCS1; cases hCS1
      | jump e2 =>
        rw [hb2End] at hCS1
        obtain ⟨tgt2, htgt2Mem, htgt2Id, _⟩ := hGCov.dstWF.2.2 b2 hb2Mem e2 (by simp [hb2End, Terminator.successors])
        have hFindTgt2 : g2.blocks.find? (·.id == e2.target) = some tgt2 := by
          have hf := array_find?_of_mem_nodup hGCov.dstWF.1 htgt2Mem; rwa [htgt2Id] at hf
        obtain ⟨next2, hTr2, hProj⟩ := h.transfer_backward (edge2 := toLogicalEdge e2) hLu2 hFindTgt2 hCS1
        have hvb' : ValidBlocks dst next2 := (h.transfer_forward hvb hTr2).2
        refine ⟨next2, (mir_raw_step_iff dst s2 next2).mpr ?_, hProj, hvb'⟩
        rw [hAt]; refine ⟨toLogicalBlock b2, hBlk2Eq, hPC2, ?_⟩
        dsimp only [toLogicalBlock]; rw [hb2End]; exact hTr2
      | branch c y2 n2 =>
        rw [hb2End] at hCS1
        obtain ⟨v, after1, fl, hRd1, hTest, hTr1⟩ := hCS1
        obtain ⟨hf1, hb1, hc1⟩ := readValue_preserves_ctx hRd1
        have hRd2 := hT.readValue_eq s2.block s2.callers (projectState proj s2) (toLogicalRead c)
        rw [swapCtx_projectState, hRd1, Option.map_some] at hRd2
        let after2 := swapCtx s2.block s2.callers after1
        have hAfter1Eq : projectState proj after2 = after1 :=
          projectState_swapCtx_of_ctx proj hf1 hb1 hc1
        have hLuAfter2 : AList.lookup dst.functions after2.frame = some (locals, g2) := by
          change AList.lookup dst.functions after1.frame = _; rw [hf1]; exact hLu2
        have hvbAfter2 : ValidBlocks dst after2 := by
          obtain ⟨l0, g0, b0, hL0, hF0⟩ := hvb.1
          exact ⟨⟨l0, g0, b0, by change AList.lookup dst.functions after1.frame = _; rw [hf1]; exact hL0, hF0⟩,
            fun c hc => hvb.2 c hc⟩
        cases fl with
        | true =>
          obtain ⟨tgt2, htgt2Mem, htgt2Id, _⟩ := hGCov.dstWF.2.2 b2 hb2Mem y2 (by simp [hb2End, Terminator.successors])
          have hFindTgt2 : g2.blocks.find? (·.id == y2.target) = some tgt2 := by
            have hf := array_find?_of_mem_nodup hGCov.dstWF.1 htgt2Mem; rwa [htgt2Id] at hf
          have hTr1' : (src.logicalView (language := language)).Transfer
              { toLogicalEdge y2 with target := proj after2.frame y2.target }
              (projectState proj after2) next1 false := by
            rw [hAfter1Eq]
            have hF : after2.frame = s2.frame := hf1
            rw [hF]; simpa [toLogicalEdge] using hTr1
          obtain ⟨next2, hTr2, hProj⟩ := h.transfer_backward hLuAfter2 hFindTgt2 hTr1'
          have hvb' : ValidBlocks dst next2 := (h.transfer_forward hvbAfter2 hTr2).2
          refine ⟨next2, (mir_raw_step_iff dst s2 next2).mpr ?_, hProj, hvb'⟩
          rw [hAt]; refine ⟨toLogicalBlock b2, hBlk2Eq, hPC2, ?_⟩
          dsimp only [toLogicalBlock]; rw [hb2End]
          exact ⟨v, after2, true, hRd2, hTest, hTr2⟩
        | false =>
          obtain ⟨tgt2, htgt2Mem, htgt2Id, _⟩ := hGCov.dstWF.2.2 b2 hb2Mem n2 (by simp [hb2End, Terminator.successors])
          have hFindTgt2 : g2.blocks.find? (·.id == n2.target) = some tgt2 := by
            have hf := array_find?_of_mem_nodup hGCov.dstWF.1 htgt2Mem; rwa [htgt2Id] at hf
          have hTr1' : (src.logicalView (language := language)).Transfer
              { toLogicalEdge n2 with target := proj after2.frame n2.target }
              (projectState proj after2) next1 false := by
            rw [hAfter1Eq]
            have hF : after2.frame = s2.frame := hf1
            rw [hF]; simpa [toLogicalEdge] using hTr1
          obtain ⟨next2, hTr2, hProj⟩ := h.transfer_backward hLuAfter2 hFindTgt2 hTr1'
          have hvb' : ValidBlocks dst next2 := (h.transfer_forward hvbAfter2 hTr2).2
          refine ⟨next2, (mir_raw_step_iff dst s2 next2).mpr ?_, hProj, hvb'⟩
          rw [hAt]; refine ⟨toLogicalBlock b2, hBlk2Eq, hPC2, ?_⟩
          dsimp only [toLogicalBlock]; rw [hb2End]
          exact ⟨v, after2, false, hRd2, hTest, hTr2⟩
      | ret opt =>
        rw [hb2End] at hCS1
        obtain ⟨caller1, rest1, vals, after1, ended1, hCallers1, hRd1, hEndL1, hTys, hPO1⟩ := hCS1
        cases hCs2 : s2.callers with
        | nil =>
          change projectCallers proj s2.callers = caller1 :: rest1 at hCallers1
          rw [hCs2] at hCallers1; cases hCallers1
        | cons caller2 rest2 =>
          change projectCallers proj s2.callers = caller1 :: rest1 at hCallers1
          rw [hCs2, projectCallers, List.map_cons, List.cons.injEq] at hCallers1
          obtain ⟨hC1Eq, hR1Eq⟩ := hCallers1
          obtain ⟨hf1, hb1, hc1⟩ := readOperands_preserves_ctx hRd1
          obtain ⟨hf2, hb2, hc2⟩ := endLocals_preserves_ctx hEndL1
          obtain ⟨hfn1, hbn1, hcn1⟩ := paramOutcome_preserves_ctx hPO1
          have hRd2 := hT.readOperands_eq s2.block s2.callers
            (opt.map fun a => ⟨a.1, toLogicalRead a.2⟩).toList (projectState proj s2)
          rw [swapCtx_projectState, hRd1, Option.map_some] at hRd2
          have hEndL2 := hT.endLocals s2.block s2.callers hEndL1
          have hPO2 := hT.paramOutcome caller2.caller.block rest2 hPO1
          let next2 := swapCtx caller2.caller.block rest2 next1
          have hRes2Eq : swapCtx caller2.caller.block rest2
              ({ toMemoryFrame := caller1.caller, bytes := ended1.bytes, callers := rest1,
                 effects := ended1.effects, ownership := ended1.ownership,
                 nextAddressId := ended1.nextAddressId } : MemoryRegisters language profile) =
              ({ toMemoryFrame := caller2.caller, bytes := (swapCtx s2.block s2.callers ended1).bytes,
                 callers := rest2, effects := (swapCtx s2.block s2.callers ended1).effects,
                 ownership := (swapCtx s2.block s2.callers ended1).ownership,
                 nextAddressId := (swapCtx s2.block s2.callers ended1).nextAddressId } :
                MemoryRegisters language profile) := by
            rw [← hC1Eq]; cases caller2 with | mk cf cd => cases cf; rfl
          rw [hRes2Eq] at hPO2
          have hDestEq : caller2.destination = caller1.destination := by rw [← hC1Eq]
          refine ⟨next2, (mir_raw_step_iff dst s2 next2).mpr ?_, ?_, ?_⟩
          · rw [hAt]; refine ⟨toLogicalBlock b2, hBlk2Eq, hPC2, ?_⟩
            dsimp only [toLogicalBlock]; rw [hb2End]
            refine ⟨caller2, rest2, vals, swapCtx s2.block s2.callers after1,
              swapCtx s2.block s2.callers ended1, hCs2, hRd2, hEndL2,
              by rw [hDestEq]; exact hTys, by rw [hDestEq]; exact hPO2⟩
          · unfold projectState next2
            have hF : next1.frame = caller2.caller.frame := hfn1.trans (by rw [← hC1Eq]; rfl)
            have hB : next1.block = proj caller2.caller.frame caller2.caller.block :=
              hbn1.trans (by rw [← hC1Eq]; rfl)
            have hC : next1.callers = projectCallers proj rest2 := hcn1.trans hR1Eq.symm
            change swapCtx (proj next1.frame caller2.caller.block) (projectCallers proj rest2)
                (swapCtx caller2.caller.block rest2 next1) = next1
            rw [hF, ← hB, ← hC, swapCtx_inv]
          · obtain ⟨lc, gc, bc, hLc, hFc⟩ := hvb.2 caller2 (hCs2 ▸ .head _)
            refine ⟨⟨lc, gc, bc, ?_, hFc⟩, fun c hc => hvb.2 c (hCs2 ▸ .tail _ hc)⟩
            change AList.lookup dst.functions next1.frame = some (lc, gc)
            have hF : next1.frame = caller2.caller.frame := hfn1.trans (by rw [← hC1Eq]; rfl)
            rw [hF]; exact hLc
  obtain ⟨next2, hRaw2, hProj, hvb'⟩ := hRaw2Ex
  refine ⟨next2, (mir_step_iff dst s2 next2).mpr ⟨?_, hRaw2, ?_⟩, hProj⟩
  · rw [h.hostedResult_eq hvb]; exact hNoRes1
  · rw [h.trace_eq hvb, h.trace_eq hvb', hProj]
    exact hTr1

private theorem MIRCovering.validBlocks_of_finite {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {proj : FnId → BlockId → BlockId} {src dst : MIR lang profile}
    (h : MIRCovering proj src dst)
    (hWF1 : (src.logicalView (language := language)).HandoffWF)
    {s2 : MemoryRegisters language profile}
    (hf : (Interfaces.MIR.Execution (language := language)).finite dst s2) :
    ValidBlocks dst s2 := by
  refine finite_invariant (Interfaces.MIR.Execution (language := language)) dst (ValidBlocks dst) ?_ ?_ hf
  · intro q hq
    have hq' := (mir_initial_iff dst q).mp hq
    subst q
    exact (h.entryState_project hWF1).2
  · intro a b hva hstep
    exact (h.step_forward hva hstep).2

private theorem MIRCovering.finite_project {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {proj : FnId → BlockId → BlockId} {src dst : MIR lang profile}
    (h : MIRCovering proj src dst)
    (hWF1 : (src.logicalView (language := language)).HandoffWF)
    {s2 : MemoryRegisters language profile}
    (hf : (Interfaces.MIR.Execution (language := language)).finite dst s2) :
    (Interfaces.MIR.Execution (language := language)).finite src (projectState proj s2) := by
  obtain ⟨start, hInit, path⟩ := hf
  have hInit' := (mir_initial_iff dst start).mp hInit
  subst start
  have hStartVB := (h.entryState_project hWF1).2
  have hStartEq := (h.entryState_project hWF1).1
  have follow : ∀ {a b}, Steps ((Interfaces.MIR.Execution (language := language)).step dst) a b →
      ValidBlocks dst a →
      Steps ((Interfaces.MIR.Execution (language := language)).step src)
        (projectState proj a) (projectState proj b) := by
    intro a b steps hva
    induction steps with
    | nil => exact .nil _
    | cons tick _ ih =>
      obtain ⟨tick1, hvb'⟩ := h.step_forward hva tick
      exact .cons tick1 (ih hvb')
  exact ⟨projectState proj (entryStateOf (dst.logicalView (language := language))),
    (mir_initial_iff src _).mpr hStartEq, follow path hStartVB⟩

private theorem MIRCovering.safe_preserved {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {proj : FnId → BlockId → BlockId} {src dst : MIR lang profile}
    (h : MIRCovering proj src dst)
    (hWF1 : (src.logicalView (language := language)).HandoffWF)
    (hSafe1 : ProgramSafe (Interfaces.MIR.Execution (language := language)) src) :
    ProgramSafe (Interfaces.MIR.Execution (language := language)) dst := by
  intro s2 hStuck2
  have hvb2 := h.validBlocks_of_finite hWF1 hStuck2.1
  refine hSafe1 (projectState proj s2) ⟨h.finite_project hWF1 hStuck2.1, ?_, ?_⟩
  · intro next1 hStep1
    obtain ⟨next2, hStep2, _⟩ := h.step_backward hvb2 hStep1
    exact hStuck2.2.1 next2 hStep2
  · intro o hRes1
    apply hStuck2.2.2 o
    exact (mir_result_iff dst s2 o).mpr ((h.hostedResult_eq hvb2).trans ((mir_result_iff src _ o).mp hRes1))

private theorem representsCheckpoint_projectState {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {memory : Interfaces.Memory lang profile}
    (proj : FnId → BlockId → BlockId) {p2 q : MemoryRegisters language profile}
    (hRep : memory.RepresentsCheckpoint p2 q) :
    memory.RepresentsCheckpoint (projectState proj p2) q := by
  obtain ⟨hAct, hLen, hCallers, hAddr, hNd, hNid, hOwn⟩ := hRep
  refine ⟨hAct, by simpa only [projectState, swapCtx, projectCallers, List.length_map] using hLen,
    ?_, hAddr, hNd, hNid, hOwn⟩
  have hZip : ∀ (P : MemoryContinuation language.operands.Ty language.operands.Val ×
        MemoryContinuation language.operands.Ty language.operands.Val → Prop)
      (hP : ∀ c1 c2, P (c1, c2) → P ({ c1 with caller := projectFrame proj c1.caller }, c2))
      (cs1 cs2 : List (MemoryContinuation language.operands.Ty language.operands.Val)),
      (∀ pair ∈ cs1.zip cs2, P pair) →
      ∀ pair ∈ (projectCallers proj cs1).zip cs2, P pair := by
    intro P hP cs1
    induction cs1 with
    | nil => intro cs2 _ pair hm; cases hm
    | cons c1 rest1 ih =>
      intro cs2 hAll pair hm
      cases cs2 with
      | nil => cases hm
      | cons c2 rest2 =>
        simp only [projectCallers, List.map_cons, List.zip_cons_cons, List.mem_cons] at hm
        rcases hm with rfl | htail
        · exact hP c1 c2 (hAll (c1, c2) (.head _))
        · exact ih rest2 (fun p hp => hAll p (.tail _ hp)) pair htail
  exact hZip _ (fun _ _ hp => hp) p2.callers q.callers hCallers

private theorem MIRCovering.promotionSound_of_dst {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {proj : FnId → BlockId → BlockId} {src dst : MIR lang profile}
    {memory : Interfaces.Memory lang profile}
    (h : MIRCovering proj src dst)
    (hProm2 : PromotionSound (language := language) dst memory) :
    PromotionSound (language := language) src memory := by
  obtain ⟨hFwd, hBwd⟩ := hProm2
  constructor
  · intro fn locals g1 hMem1 place ty hLoc
    obtain ⟨i, hi1, hEq1⟩ := List.getElem_of_mem hMem1
    have hGet1 : src.functions[i]? = some (fn, locals, g1) := by rw [List.getElem?_eq_getElem hi1, hEq1]
    have hi2 : i < dst.functions.length := h.fnsLen.symm ▸ hi1
    have hGet2 : dst.functions[i]? = some dst.functions[i] := List.getElem?_eq_getElem hi2
    obtain ⟨hfEq, hlEq, _⟩ := h.fns i (fn, locals, g1) dst.functions[i] hGet1 hGet2
    have hMem2 : (fn, locals, dst.functions[i].2.2) ∈ dst.functions := by
      have hm := List.getElem_mem hi2
      rcases he : dst.functions[i] with ⟨f2, l2, g2⟩
      rw [he] at hfEq hlEq hm; dsimp only at hfEq hlEq; rw [hfEq, hlEq] at hm; exact hm
    obtain ⟨classes, st, hLuC, hLuP, hIff⟩ := hFwd fn locals dst.functions[i].2.2 hMem2 place ty hLoc
    exact ⟨classes, st, hLuC, hLuP, hIff.trans (and_congr_right fun _ => not_congr (h.escapes_iff fn place))⟩
  · intro fn classes place hSt hV
    obtain ⟨ty, s2, g2, tgt, tg, hMem2, hLoc2, hMemT, hLocT, hSc, hNoEsc2⟩ := hBwd fn classes place hSt hV
    obtain ⟨i, hi2, hEq2⟩ := List.getElem_of_mem hMem2
    have hGet2 : dst.functions[i]? = some (fn, s2, g2) := by rw [List.getElem?_eq_getElem hi2, hEq2]
    have hi1 : i < src.functions.length := h.fnsLen ▸ hi2
    have hGet1 : src.functions[i]? = some src.functions[i] := List.getElem?_eq_getElem hi1
    obtain ⟨hfEq, hlEq, _⟩ := h.fns i src.functions[i] (fn, s2, g2) hGet1 hGet2
    have hMem1 : (fn, s2, src.functions[i].2.2) ∈ src.functions := by
      have hm := List.getElem_mem hi1
      rcases he : src.functions[i] with ⟨f1, l1, g1⟩
      rw [he] at hfEq hlEq hm; dsimp only at hfEq hlEq; rw [← hfEq, ← hlEq] at hm; exact hm
    exact ⟨ty, s2, src.functions[i].2.2, tgt, tg, hMem1, hLoc2, hMemT, hLocT, hSc,
      fun hEsc1 => hNoEsc2 ((h.escapes_iff fn place).mpr hEsc1)⟩

private theorem MIRCovering.retainsTypedRoots_of_dst {Ty : Type}
    {lang : LoweringLanguage Ty} {profile : Profile}
    {proj : FnId → BlockId → BlockId} {src dst : MIR lang profile}
    {memory : Interfaces.Memory lang profile}
    (h : MIRCovering proj src dst)
    (hRet : RetainsTypedRoots dst memory) :
    RetainsTypedRoots src memory := by
  obtain ⟨hDstNd, hMemNd, hDstLocNd, hMemLocNd, hFwd, hBwd⟩ := hRet
  have hKeysEq : List.map Prod.fst dst.functions = List.map Prod.fst src.functions := by
    have hc := congrArg (List.map Prod.fst) h.inventories_eq
    simpa only [List.map_map, Function.comp_def] using hc
  refine ⟨hKeysEq ▸ hDstNd, hMemNd, ?_, hMemLocNd, ?_, ?_⟩
  · intro fn locals g1 hMem1
    obtain ⟨i, hi1, hEq1⟩ := List.getElem_of_mem hMem1
    have hGet1 : src.functions[i]? = some (fn, locals, g1) := by rw [List.getElem?_eq_getElem hi1, hEq1]
    have hi2 : i < dst.functions.length := h.fnsLen.symm ▸ hi1
    have hGet2 : dst.functions[i]? = some dst.functions[i] := List.getElem?_eq_getElem hi2
    obtain ⟨hfEq, hlEq, _⟩ := h.fns i (fn, locals, g1) dst.functions[i] hGet1 hGet2
    have hMem2 : (fn, locals, dst.functions[i].2.2) ∈ dst.functions := by
      have hm := List.getElem_mem hi2
      rcases he : dst.functions[i] with ⟨f2, l2, g2⟩
      rw [he] at hfEq hlEq hm; dsimp only at hfEq hlEq; rw [hfEq, hlEq] at hm; exact hm
    exact hDstLocNd fn locals dst.functions[i].2.2 hMem2
  · intro fn locals g1 hMem1
    obtain ⟨i, hi1, hEq1⟩ := List.getElem_of_mem hMem1
    have hGet1 : src.functions[i]? = some (fn, locals, g1) := by rw [List.getElem?_eq_getElem hi1, hEq1]
    have hi2 : i < dst.functions.length := h.fnsLen.symm ▸ hi1
    have hGet2 : dst.functions[i]? = some dst.functions[i] := List.getElem?_eq_getElem hi2
    obtain ⟨hfEq, hlEq, _⟩ := h.fns i (fn, locals, g1) dst.functions[i] hGet1 hGet2
    have hMem2 : (fn, locals, dst.functions[i].2.2) ∈ dst.functions := by
      have hm := List.getElem_mem hi2
      rcases he : dst.functions[i] with ⟨f2, l2, g2⟩
      rw [he] at hfEq hlEq hm; dsimp only at hfEq hlEq; rw [hfEq, hlEq] at hm; exact hm
    exact hFwd fn locals dst.functions[i].2.2 hMem2
  · intro fn locals gm hMemM
    obtain ⟨srcLocs, g2, hMem2, hIff⟩ := hBwd fn locals gm hMemM
    obtain ⟨i, hi2, hEq2⟩ := List.getElem_of_mem hMem2
    have hGet2 : dst.functions[i]? = some (fn, srcLocs, g2) := by rw [List.getElem?_eq_getElem hi2, hEq2]
    have hi1 : i < src.functions.length := h.fnsLen ▸ hi2
    have hGet1 : src.functions[i]? = some src.functions[i] := List.getElem?_eq_getElem hi1
    obtain ⟨hfEq, hlEq, _⟩ := h.fns i src.functions[i] (fn, srcLocs, g2) hGet1 hGet2
    have hMem1 : (fn, srcLocs, src.functions[i].2.2) ∈ src.functions := by
      have hm := List.getElem_mem hi1
      rcases he : src.functions[i] with ⟨f1, l1, g1⟩
      rw [he] at hfEq hlEq hm; dsimp only at hfEq hlEq; rw [← hfEq, ← hlEq] at hm; exact hm
    exact ⟨srcLocs, src.functions[i].2.2, hMem1, hIff⟩

private theorem MIRCovering.compose_handoff {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {proj : FnId → BlockId → BlockId} {src dst : MIR lang profile}
    {memory : Interfaces.Memory lang profile}
    (h : MIRCovering proj src dst)
    (hWF1 : (src.logicalView (language := language)).HandoffWF)
    (ok : lowerGraph (language := language) dst = .ok memory) :
    memory.HandoffWF ∧ RetainsTypedRoots src memory ∧ memory.mutableLocals = src.mutableLocals := by
  have hWF2 := h.logicalView_handoffWF hWF1
  have hGH := lowerGraph_handoff ok hWF2
  exact ⟨hGH.handoff, h.retainsTypedRoots_of_dst hGH.retainedRoots,
    hGH.mutability.trans h.mutableLocals⟩

/-- Constructively transport `LowersMemory` across a `MIRCovering` from `dst` back to `src`
without using `Classical.choice`, combining U03's `slice_lowerGraph_lowersMemory` and
`slice_lowerGraph_correspondence` with the exact block-projection bisimulation. -/
private theorem MIRCovering.compose_lowersMemory {slice : Slice} {profile : Profile}
    {proj : FnId → BlockId → BlockId}
    {src dst : MIR (sliceLoweringLanguage slice) profile}
    {memory : Interfaces.Memory (sliceLoweringLanguage slice) profile}
    (h : MIRCovering proj src dst)
    (hWF1 : (src.logicalView (language := sliceLanguage slice)).HandoffWF)
    (ok : lowerGraph (language := sliceLanguage slice) dst = .ok memory) :
    LowersMemory (language := sliceLanguage slice) src memory := by
  have hWF2 := h.logicalView_handoffWF hWF1
  have hLow2 := slice_lowerGraph_lowersMemory ok hWF2
  obtain ⟨hProj2, hEnt2, hOrig2, hLay2, ⟨hSig2, hDecl2⟩, hProm2, hRet2, hLayWF2, hTabWF2, _, hRef2, _⟩ := hLow2
  refine ⟨hProj2.trans h.project, hEnt2.trans h.entry, ?_, hLay2,
    ⟨hSig2.trans h.signatures, hDecl2.trans h.declarations⟩,
    h.promotionSound_of_dst hProm2, h.retainsTypedRoots_of_dst hRet2, hLayWF2,
    hTabWF2, hWF1.1, ?_, ?_⟩
  · exact ⟨fun o ho => (h.origins_iff o).mp (hOrig2.1 o ho),
      fun o ho => hOrig2.2 o ((h.origins_iff o).mpr ho)⟩
  · intro hSafe1
    have hSafe2 := h.safe_preserved hWF1 hSafe1
    obtain ⟨hObs2, hDiv2, hNoStuck2⟩ := hRef2 hSafe2
    refine ⟨?_, ?_, hNoStuck2⟩
    · intro q o hFinQ hMatch
      obtain ⟨p2, hFin2, hRes2, hTr2⟩ := hObs2 q o hFinQ hMatch
      have hvb2 := h.validBlocks_of_finite hWF1 hFin2
      refine ⟨projectState proj p2, h.finite_project hWF1 hFin2, ?_, ?_⟩
      · exact (mir_result_iff src _ o).mpr ((h.hostedResult_eq hvb2).symm.trans ((mir_result_iff dst p2 o).mp hRes2))
      · rw [← h.trace_eq hvb2]; exact hTr2
    · intro runT hDivT
      obtain ⟨run2, hDiv2', hSame2⟩ := hDiv2 runT hDivT
      have hRun2Fin : ∀ i, (Interfaces.MIR.Execution (language := sliceLanguage slice)).finite dst (run2 i) := by
        intro i
        induction i with
        | zero => exact ⟨run2 0, hDiv2'.1, .nil _⟩
        | succ k ih => exact finite_next ih (hDiv2'.2 k)
      refine ⟨fun i => projectState proj (run2 i), ⟨?_, ?_⟩, ?_⟩
      · have h0 := (mir_initial_iff dst (run2 0)).mp hDiv2'.1
        exact (mir_initial_iff src _).mpr (by change projectState proj (run2 0) = _; rw [h0]; exact (h.entryState_project hWF1).1)
      · intro i
        exact (h.step_forward (h.validBlocks_of_finite hWF1 (hRun2Fin i)) (hDiv2'.2 i)).1
      · intro evs
        have hEqTr : ∀ i, (Interfaces.MIR.Execution (language := sliceLanguage slice)).trace src (projectState proj (run2 i)) =
            (Interfaces.MIR.Execution (language := sliceLanguage slice)).trace dst (run2 i) :=
          fun i => (h.trace_eq (h.validBlocks_of_finite hWF1 (hRun2Fin i))).symm
        simpa only [hEqTr] using hSame2 evs
  · intro hSafe1
    have hSafe2 := h.safe_preserved hWF1 hSafe1
    have hCorr := slice_lowerGraph_correspondence ok hWF2 hSafe2
    refine ⟨{
      related := fun p1 q => ∃ p2, ValidBlocks dst p2 ∧
        (Interfaces.MIR.Execution (language := sliceLanguage slice)).finite dst p2 ∧
        projectState proj p2 = p1 ∧ Related memory p2 q
      rank := fun _ q => stutterRank memory q
      targetInitial := fun q hq => by
        obtain ⟨p2, hp2Init, hRel⟩ := hCorr.initial q hq
        have hInitEq := (mir_initial_iff dst p2).mp hp2Init
        have hvb2 : ValidBlocks dst p2 := hInitEq ▸ (h.entryState_project hWF1).2
        refine ⟨projectState proj p2, ?_, p2, hvb2, ⟨p2, hp2Init, .nil _⟩, rfl, hRel⟩
        exact (mir_initial_iff src _).mpr (hInitEq ▸ (h.entryState_project hWF1).1)
      targetAdvance := fun p1 q q' _ hq hR tick => by
        obtain ⟨p2, hvb2, hFin2, rfl, hRel⟩ := hR
        rcases hCorr.matchStep hFin2 hq hRel tick with ⟨p2', hStep2, hRel'⟩ | ⟨hRel', hDec⟩
        · obtain ⟨hStep1, hvb2'⟩ := h.step_forward hvb2 hStep2
          exact .inl ⟨projectState proj p2, projectState proj p2', .nil _, hStep1,
            p2', hvb2', finite_next hFin2 hStep2, rfl, hRel'⟩
        · exact .inr ⟨⟨p2, hvb2, hFin2, rfl, hRel'⟩, hDec⟩
      targetFinish := fun p1 q o _ hq hR hObs => by
        obtain ⟨p2, hvb2, hFin2, rfl, hRel⟩ := hR
        have hRes2 := hCorr.finish p2 q o hFin2 hq hRel hObs
        have hRes1 : (Interfaces.MIR.Execution (language := sliceLanguage slice)).result src (projectState proj p2) o :=
          (mir_result_iff src _ o).mpr ((h.hostedResult_eq hvb2).symm.trans ((mir_result_iff dst p2 o).mp hRes2))
        refine ⟨projectState proj p2, .nil _, hRes1, ?_⟩
        rw [← h.trace_eq hvb2]
        have hDstRes := Interfaces.Memory.matchingResult_result hObs
        with_unfolding_all
          change p2.effects.map _ ++ (Option.map _ _).getD [] =
            q.effects.map _ ++ (Option.map _ _).getD []
          rw [hRes2, hDstRes, hRel.effects]
    }, ?_⟩
    rintro p1 q _ _ ⟨p2, _, _, rfl, hRel⟩
    exact ⟨representsCheckpoint_projectState proj hRel.representation, hRel.effects⟩

/-! ### 6. Production `lowerMemory` pass and universal registered validation -/

private theorem MIRCovering.of_handoffWF {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {mir : MIR lang profile}
    (hWF : mir.HandoffWF (language := language)) :
    ∃ proj, MIRCovering proj mir (specializeMIR mir) := by
  obtain ⟨hLog, _⟩ := hWF
  have hNd : (List.map Prod.fst mir.functions).Nodup := by
    have h1 := hLog.1.1
    have hKeys : List.map Prod.fst (mir.logicalView (language := language)).functions =
        List.map Prod.fst mir.functions := by
      have hc := congrArg (List.map Prod.fst) (MIR.logicalView_inventories_eq (language := language) mir)
      simpa only [List.map_map, Function.comp_def] using hc
    rwa [hKeys] at h1
  have hGraphWF : ∀ fn locals graph, (fn, locals, graph) ∈ mir.functions →
      graph.WF Terminator.successors Sigma.fst Sigma.fst := by
    intro fn locals graph hm
    have hmLog : (fn, locals, toLogicalGraph graph) ∈ (mir.logicalView (language := language)).functions := by
      rw [toLogicalGraph_eq_logicalView]
      exact List.mem_map.mpr ⟨(fn, locals, graph), hm, rfl⟩
    exact toLogicalGraph_wf.mp (hLog.1.2.2.2.2.2.2 fn locals (toLogicalGraph graph) hmLog).2.1
  exact MIRCovering.of_specializeMIR hNd hGraphWF

/-- Rule reported only when `lowerMemory` is invoked outside the `MIR.HandoffWF` domain. -/
def invalidMIRRule : RuleId :=
  ⟨``MIR.HandoffWF, 1⟩

/-- Production MIR-to-Memory lowering pass. Runs `lowerGraph` directly on unambiguous
graphs (zero CFG overhead) and falls back to `specializeMIR` when a root has an ambiguous
lifetime join so every `MIR.HandoffWF` input succeeds. -/
def lowerMemory {slice : Slice} {profile : Profile}
    (mir : MIR (sliceLoweringLanguage slice) profile) :
    Except StageFailure (Interfaces.Memory (sliceLoweringLanguage slice) profile) :=
  match lowerGraph (language := sliceLanguage slice) mir with
  | .ok memory => .ok memory
  | .error _ =>
    match lowerGraph (language := sliceLanguage slice) (specializeMIR mir) with
    | .ok memory => .ok memory
    | .error _ => .error (.invalidInput invalidMIRRule)

/-- Fast path: whenever `lowerGraph` succeeds directly, `lowerMemory` returns its exact output. -/
theorem lowerMemory_eq_lowerGraph_of_ok {slice : Slice} {profile : Profile}
    {mir : MIR (sliceLoweringLanguage slice) profile}
    {memory : Interfaces.Memory (sliceLoweringLanguage slice) profile}
    (ok : lowerGraph (language := sliceLanguage slice) mir = .ok memory) :
    lowerMemory mir = .ok memory := by
  unfold lowerMemory
  rw [ok]

/-- Totality on the frozen domain: every `MIR.HandoffWF` program lowers to `.ok memory`. -/
theorem lowerMemory_ok_of_wf {slice : Slice} {profile : Profile}
    {mir : MIR (sliceLoweringLanguage slice) profile}
    (hWF : mir.HandoffWF (language := sliceLanguage slice)) :
    ∃ memory, lowerMemory mir = .ok memory := by
  unfold lowerMemory
  cases hFast : lowerGraph (language := sliceLanguage slice) mir with
  | ok memory => exact ⟨memory, rfl⟩
  | error _ =>
    obtain ⟨memory, hSpec⟩ := lowerGraph_specializeMIR_ok hWF
    exact ⟨memory, by rw [hSpec]⟩

/-- `lowerMemory` never returns an error on any `MIR.HandoffWF` input. -/
theorem lowerMemory_error_impossible_of_wf {slice : Slice} {profile : Profile}
    {mir : MIR (sliceLoweringLanguage slice) profile} {failure : StageFailure}
    (hWF : mir.HandoffWF (language := sliceLanguage slice))
    (hErr : lowerMemory mir = .error failure) : False := by
  obtain ⟨memory, hOk⟩ := lowerMemory_ok_of_wf hWF
  rw [hOk] at hErr
  cases hErr

/-- Handoff invariants for `lowerMemory`: frozen `Memory.HandoffWF`, `RetainsTypedRoots`,
and `mutableLocals` preservation on every `MIR.HandoffWF` input. -/
theorem lowerMemory_handoff {slice : Slice} {profile : Profile}
    {mir : MIR (sliceLoweringLanguage slice) profile}
    {memory : Interfaces.Memory (sliceLoweringLanguage slice) profile}
    (hWF : mir.HandoffWF (language := sliceLanguage slice))
    (ok : lowerMemory mir = .ok memory) :
    memory.HandoffWF ∧ RetainsTypedRoots mir memory ∧ memory.mutableLocals = mir.mutableLocals := by
  unfold lowerMemory at ok
  cases hFast : lowerGraph (language := sliceLanguage slice) mir with
  | ok mFast =>
    rw [hFast] at ok
    cases Except.ok.inj ok
    have hGH := lowerGraph_handoff hFast hWF.1
    exact ⟨hGH.handoff, hGH.retainedRoots, hGH.mutability⟩
  | error _ =>
    rw [hFast] at ok
    cases hSpec : lowerGraph (language := sliceLanguage slice) (specializeMIR mir) with
    | error _ => rw [hSpec] at ok; cases ok
    | ok mSpec =>
      rw [hSpec] at ok
      cases Except.ok.inj ok
      obtain ⟨proj, hCov⟩ := MIRCovering.of_handoffWF hWF
      exact hCov.compose_handoff hWF.1 hSpec

/-- Full `LowersMemory` refinement and step-correspondence contract (`ISS-0119`, `ISS-0125`)
for every `MIR.HandoffWF` program lowered by `lowerMemory`. -/
theorem lowerMemory_lowersMemory {slice : Slice} {profile : Profile}
    {mir : MIR (sliceLoweringLanguage slice) profile}
    {memory : Interfaces.Memory (sliceLoweringLanguage slice) profile}
    (hWF : mir.HandoffWF (language := sliceLanguage slice))
    (ok : lowerMemory mir = .ok memory) :
    LowersMemory (language := sliceLanguage slice) mir memory := by
  unfold lowerMemory at ok
  cases hFast : lowerGraph (language := sliceLanguage slice) mir with
  | ok mFast =>
    rw [hFast] at ok
    cases Except.ok.inj ok
    exact slice_lowerGraph_lowersMemory hFast hWF.1
  | error _ =>
    rw [hFast] at ok
    cases hSpec : lowerGraph (language := sliceLanguage slice) (specializeMIR mir) with
    | error _ => rw [hSpec] at ok; cases ok
    | ok mSpec =>
      rw [hSpec] at ok
      cases Except.ok.inj ok
      obtain ⟨proj, hCov⟩ := MIRCovering.of_handoffWF hWF
      exact hCov.compose_lowersMemory hWF.1 hSpec

/-- Universal validation of the registered `lowerMemory` stage for any slice, any profile,
and any pass bundle wired to `Concrete.Memory.Validate.lowerMemory`. -/
theorem lowerMemory_validated {slice : Slice} {profile : Profile}
    (passes : Passes slice profile)
    (hLower : passes.lowerMemory = lowerMemory) :
    (registry passes).lowerMemory.Validated := by
  constructor
  · intro mir memory hWF hRan
    change passes.lowerMemory mir = .ok memory at hRan
    rw [hLower] at hRan
    have ⟨hMemWF, _, hMut⟩ := lowerMemory_handoff hWF hRan
    exact ⟨hMemWF, lowerMemory_lowersMemory hWF hRan, hMut⟩
  · intro mir failure hWF hRan
    change passes.lowerMemory mir = .error failure at hRan
    rw [hLower] at hRan
    exact lowerMemory_error_impossible_of_wf hWF hRan

/-- Universal `Stage.Sound` for the registered `.lowerMemory` edge. -/
theorem lowerMemory_sound {slice : Slice} {profile : Profile}
    (passes : Passes slice profile)
    (hLower : passes.lowerMemory = lowerMemory) :
    Stage.Sound passes .lowerMemory :=
  ⟨lowerMemory_validated passes hLower, fun _ _ _ _ => trivial⟩

/-- Connecting `lowerMemory` to `ISS-0125` resident handles and `RepresentsCheckpoint`:
every reachable target state of a safe lowered program has resident active and suspended
caller handles, satisfies `TransferCheckpoint`, and represents a reachable MIR state with
`ZeroUnbacked` and identical effects. -/
theorem lowerMemory_reachable_represents_resident {slice : Slice} {profile : Profile}
    {mir : MIR (sliceLoweringLanguage slice) profile}
    {memory : Interfaces.Memory (sliceLoweringLanguage slice) profile}
    (hWF : mir.HandoffWF (language := sliceLanguage slice))
    (ok : lowerMemory mir = .ok memory)
    (safe : ProgramSafe
      (Interfaces.MIR.Execution (language := sliceLanguage slice)) mir)
    (q : MemoryRegisters (sliceLanguage slice) profile)
    (reached : (Interfaces.Memory.Execution (language := sliceLanguage slice)).finite memory q) :
    ResidentHandles memory q ∧
    (∀ ceiling, profile.memoryTarget.addressCeiling ≤ ceiling →
      memory.TransferCheckpoint q ceiling) ∧
    ∃ p, (Interfaces.MIR.Execution (language := sliceLanguage slice)).finite mir p ∧
      memory.RepresentsCheckpoint p q ∧
      memory.ZeroUnbacked q ∧
      p.effects = q.effects :=
  LowersMemory.reachable_represents_resident (lowerMemory_lowersMemory hWF ok) safe q reached

/-- Connecting `lowerMemory` to U02's `ReturnAt.exhausted_iff` and suspended-caller
resident-handle preservation at every reachable return checkpoint. -/
theorem lowerMemory_reachable_return_exhausted_iff {slice : Slice} {profile : Profile}
    {mir : MIR (sliceLoweringLanguage slice) profile}
    {memory : Interfaces.Memory (sliceLoweringLanguage slice) profile}
    (hWF : mir.HandoffWF (language := sliceLanguage slice))
    (ok : lowerMemory mir = .ok memory)
    (safe : ProgramSafe
      (Interfaces.MIR.Execution (language := sliceLanguage slice)) mir)
    {q read ended : MemoryRegisters (sliceLanguage slice) profile} {operand caller rest vs}
    (reached : (Interfaces.Memory.Execution (language := sliceLanguage slice)).finite memory q)
    (ret : memory.ReturnAt (language := sliceLanguage slice) q read ended operand caller rest vs) :
    let resume := Interfaces.Memory.ReturnAt.resume caller rest ended
    ResidentHandles memory resume ∧
    (((Interfaces.Memory.Execution (language := sliceLanguage slice)).step memory q
        { q with resourceExhausted := true } ∧
      (Interfaces.Memory.Execution (language := sliceLanguage slice)).result memory
        { q with resourceExhausted := true } .resourceExhausted) ↔
      (∃ ceiling, memory.ParamsValid caller.destination.toList vs resume ceiling) ∧
        ¬ memory.ParamSpaceFits caller.destination.toList resume) :=
  LowersMemory.reachable_return_exhausted_iff (lowerMemory_lowersMemory hWF ok) safe reached ret

end Concrete.Memory.Validate
