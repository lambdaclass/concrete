import Concrete.Memory.Lowering

namespace Concrete.Memory

open Concrete.Interfaces ConcreteSpec.Core ConcreteSpec.StageInterface

variable {Ty : Type} {lang : LoweringLanguage Ty}

/-- Roots whose lifetime can start at this instruction. Block arguments, calls and
destructures already use WriteParams on both machines. Only bind and op need a prefix. -/
def definitionRoot (locals : LocalTypes Ty) (inst : MemoryInst lang) : Option PlaceId :=
  let dest := match inst with
    | .bind dst _ | .op dst .. => some dst.logical.root
    | _ => none
  -- A trapping operator does not write its result. An undeclared result cannot
  -- begin on MIR's normal path either. Keep the trap reachable in Memory.
  dest.filter fun root => (AList.lookup locals root).isSome

private def lifetimeChange (instruction : MemoryInst lang) (root : PlaceId) : Option Bool :=
  match instruction with
  | .bind dst _ | .op dst .. => if dst.logical.root = root then some true else none
  | .beginLifetime id => if id = root then some true else none
  | .endLifetime id => if id = root then some false else none
  | .call _ dst .. => if dst.any (·.2.logical.root == root) then some true else none
  | .destructure _ bindings =>
    if bindings.any (·.2.2.logical.root == root) then some true else none
  | .assign .. => none

private abbrev Entry := BlockId × Bool

private def nextEntries (graph : MemoryGraph lang) (root : PlaceId) (key : Entry) : List Entry :=
  match graph.blocks.find? (·.id == key.1) with
  | none => []
  | some block =>
    let live := key.2 || block.params.any (·.2.logical.root == root)
    let outgoing := block.body.foldl (fun fact op =>
      (lifetimeChange op.instruction root).getD fact) live
    block.ending.successors.map fun edge => (edge.target, outgoing)

/-- Follow both branch successors. For one queried root there are at most two
distinct entry facts per block, including on back edges. Each expansion removes
one fact from the remaining domain. -/
private def reachableEntries (graph : MemoryGraph lang) (root : PlaceId)
    (remaining pending : List Entry) : List Entry :=
  match pending with
  | [] => []
  | key :: rest =>
    if _h : key ∈ remaining then
      key :: reachableEntries graph root (remaining.erase key) (nextEntries graph root key ++ rest)
    else reachableEntries graph root remaining rest
termination_by (remaining.length, pending.length)
decreasing_by
  · apply Prod.Lex.left
    have := List.length_pos_of_mem _h
    rw [List.length_erase_of_mem _h]
    omega
  · exact Prod.Lex.right _ (Nat.lt_succ_self _)

/-- A definition at a join may need to start a lifetime on just some incoming
edges. Report that block and root until those edges are specialized. -/
structure LifetimeJoin where
  fn : FnId
  block : BlockId
  root : PlaceId

private def entryDomain (graph : MemoryGraph lang) : List Entry :=
  graph.blocks.toList.flatMap fun b => [(b.id, false), (b.id, true)]

private def entryLifetime (graph : MemoryGraph lang) (block : MemoryBlock lang) (root : PlaceId) :
    Option Bool := Id.run do
  if block.params.any (·.2.logical.root == root) then return some true
  -- An activation starts empty. Without a predecessor the only other way this
  -- block can acquire a live root on entry is parameter binding (handled above).
  if !graph.blocks.any (fun b => b.ending.successors.any (·.target == block.id)) then
    return some false
  let entries := reachableEntries graph root (entryDomain graph) [(graph.entry, false)]
  let live := entries.contains (block.id, true)
  if live && entries.contains (block.id, false) then return none
  return some live

/-- The worklist contains every pending fact and is closed under the actual
transfer function within its remaining domain. -/
private theorem reachableEntries_closed (graph : MemoryGraph lang) (root : PlaceId)
    (remaining pending : List Entry) :
    let reached := reachableEntries graph root remaining pending
    (∀ x ∈ pending, x ∈ remaining → x ∈ reached) ∧
    ∀ x ∈ reached, ∀ y ∈ nextEntries graph root x, y ∈ remaining → y ∈ reached := by
  fun_induction reachableEntries with
  | case1 => simp
  | case2 remaining key rest member ih =>
    obtain ⟨pending', closed⟩ := ih
    have cover {x} (hx : x ∈ remaining) (h : x ∈ nextEntries graph root key ++ rest) :
        x ∈ key :: reachableEntries graph root (remaining.erase key) (nextEntries graph root key ++ rest) := by
      by_cases eq : x = key
      · exact List.mem_cons.mpr (.inl eq)
      · exact .tail _ (pending' x h ((List.mem_erase_of_ne eq).mpr hx))
    constructor
    · intro x hx hr
      rcases List.mem_cons.mp hx with rfl | hx
      · exact .head _
      · exact cover hr (List.mem_append_right _ hx)
    · intro x hx y edge hr
      rcases List.mem_cons.mp hx with rfl | hx
      · exact cover hr (List.mem_append_left _ edge)
      · by_cases eq : y = key
        · exact List.mem_cons.mpr (.inl eq)
        · exact .tail _ (closed x hx y edge ((List.mem_erase_of_ne eq).mpr hr))
  | case3 remaining key rest absent ih =>
    exact ⟨fun x hx hr => ih.1 x (by
      rcases List.mem_cons.mp hx with rfl | h
      · exact (absent hr).elim
      · exact h) hr, ih.2⟩

/-- A syntactic path follows the same transfer used in the worklist. Keeping the
root as an index distinguishes the value on a back edge from activation entry. -/
inductive EntryPath (graph : MemoryGraph lang) (root : PlaceId) : Entry → Prop
  | initial : (graph.entry, false) ∈ entryDomain graph → EntryPath graph root (graph.entry, false)
  | edge {x y} : EntryPath graph root x → y ∈ nextEntries graph root x → y ∈ entryDomain graph →
      EntryPath graph root y

theorem entryLifetime_on_path (graph : MemoryGraph lang) (block : MemoryBlock lang)
    (root : PlaceId) {incoming selected}
    (path : EntryPath graph root (block.id, incoming))
    (chosen : entryLifetime graph block root = some selected) :
    selected = (incoming || block.params.any (·.2.logical.root == root)) := by
  let reached := reachableEntries graph root (entryDomain graph) [(graph.entry, false)]
  have covers := reachableEntries_closed graph root (entryDomain graph) [(graph.entry, false)]
  have all {key} (p : EntryPath graph root key) : key ∈ reached := by
    induction p with
    | initial mem => exact covers.1 _ (.head _) mem
    | edge _ step mem ih => exact covers.2 _ ih _ step mem
  have noIncoming {key} (p : EntryPath graph root key)
      (noEdge : graph.blocks.any (fun b => b.ending.successors.any (·.target == key.1)) = false) :
      key.2 = false := by
    cases p with
    | initial => rfl
    | edge _ edge _ =>
      unfold nextEntries at edge
      split at edge
      · contradiction
      · rename_i block found
        obtain ⟨e, he, rfl⟩ := List.mem_map.mp edge
        have existsEdge : graph.blocks.any (fun b => b.ending.successors.any (·.target == e.target)) :=
          Array.any_eq_true'.mpr ⟨block, Array.mem_of_find?_eq_some found,
            List.any_eq_true.mpr ⟨e, he, by simp⟩⟩
        rw [noEdge] at existsEdge
        contradiction
  have mem := all path
  cases param : block.params.any (·.2.logical.root == root) with
  | true => simpa [entryLifetime, param, eq_comm] using chosen
  | false =>
    cases edge : graph.blocks.any (fun b => b.ending.successors.any (·.target == block.id)) with
    | false =>
      have dead : incoming = false := noIncoming path edge
      rw [dead]
      simpa [entryLifetime, param, edge, eq_comm] using chosen
    | true =>
      change (block.id, incoming) ∈ reachableEntries graph root (entryDomain graph) [(graph.entry, false)] at mem
      have member := List.contains_iff_mem.mpr mem
      cases incoming <;>
        cases ht : (reachableEntries graph root (entryDomain graph) [(graph.entry, false)]).contains (block.id, true) <;>
        cases hf : (reachableEntries graph root (entryDomain graph) [(graph.entry, false)]).contains (block.id, false) <;>
        simp_all only [entryLifetime, Bool.false_eq_true, Bool.or_self, ite_false, Bool.not_true,
          Bool.and_self, Bool.and_false, Bool.false_and, Bool.or_false, ite_true,
          Id.run_pure, Option.some.injEq, reduceCtorEq]

/-- In a block the most recent lifetime change takes precedence over the
incoming fact. Reads, including whole-root moves, leave the lifetime open. -/
private def placeBody (locals : LocalTypes Ty) (fn : FnId) (block : BlockId) (live : PlaceId → Option Bool) :
    List (LocatedOp (MemoryInst lang)) →
      Except LifetimeJoin (List (LocatedOp (MemoryInst lang)))
  | [] => pure []
  | op :: rest => do
    let prefixOps ← match definitionRoot locals op.instruction with
      | none => pure []
      | some root =>
        match live root with
        | some false => pure [⟨op.origin, .beginLifetime root⟩]
        | some true => pure []
        | none => throw ⟨fn, block, root⟩
    let next := fun root => (lifetimeChange op.instruction root).orElse (fun _ => live root)
    return prefixOps ++ op :: (← placeBody locals fn block next rest)

private def placeGraph (locals : LocalTypes Ty) (fn : FnId) (graph : MemoryGraph lang) : Except LifetimeJoin (MemoryGraph lang) := do
  let blocks ← graph.blocks.toList.mapM fun block => do
    let body ← placeBody locals fn block.id (entryLifetime graph block) block.body
    return {block with body}
  return {graph with blocks := blocks.toArray}

/-- A prefix belongs to exactly the definition it opens. This also fixes the
source PC for the intermediate checkpoint between the begin and the write. -/
inductive BodyPlacement (locals : LocalTypes Ty) : (PlaceId → Option Bool) →
    List (LocatedOp (MemoryInst lang)) → List (LocatedOp (MemoryInst lang)) → Prop
  | nil {live} : BodyPlacement locals live [] []
  | keep {live op src dst} :
      (∀ root, definitionRoot locals op.instruction = some root → live root = some true) →
      BodyPlacement locals (fun p => (lifetimeChange op.instruction p).orElse (fun _ => live p)) src dst →
      BodyPlacement locals live (op :: src) (op :: dst)
  | start {live op src dst root} : definitionRoot locals op.instruction = some root → live root = some false →
      BodyPlacement locals (fun p => (lifetimeChange op.instruction p).orElse (fun _ => live p)) src dst →
      BodyPlacement locals live (op :: src) (⟨op.origin, .beginLifetime root⟩ :: op :: dst)

private theorem placeBody_placement (locals : LocalTypes Ty) (fn : FnId) (block : BlockId) (live : PlaceId → Option Bool)
    {src dst : List (LocatedOp (MemoryInst lang))}
    (ok : placeBody locals fn block live src = .ok dst) : BodyPlacement locals live src dst := by
  induction src generalizing dst live with
  | nil => cases ok; exact .nil
  | cons op rest ih =>
    simp only [placeBody] at ok
    generalize tail : placeBody locals fn block _ rest = result at ok
    cases hd : definitionRoot locals op.instruction with
    | none =>
      cases result <;> simp [hd, bind, Except.bind, pure, Except.pure] at ok
      subst dst
      exact .keep (by simp [hd]) (ih _ tail)
    | some root =>
      cases fact : live root with
      | none => simp [hd, fact, bind, Except.bind, throw] at ok
      | some value =>
        cases result <;> cases value <;>
          simp [hd, fact, bind, Except.bind, pure, Except.pure] at ok
        · subst dst; exact .start hd fact (ih _ tail)
        · subst dst; exact .keep (by simp [hd, fact]) (ih _ tail)

private theorem BodyPlacement.origins {locals : LocalTypes Ty} {live : PlaceId → Option Bool} {xs ys : List (LocatedOp (MemoryInst lang))}
    (placed : BodyPlacement locals live xs ys) (origin : Origin) :
    origin ∈ ys.map LocatedOp.origin ↔ origin ∈ xs.map LocatedOp.origin := by
  induction placed <;> simp_all

private theorem BodyPlacement.forall {locals : LocalTypes Ty} {live : PlaceId → Option Bool} {xs ys : List (LocatedOp (MemoryInst lang))}
    (placed : BodyPlacement locals live xs ys) (p : MemoryInst lang → Prop)
    (beginOk : ∀ root, p (.beginLifetime root)) (all : ∀ x ∈ xs, p x.instruction) :
    ∀ y ∈ ys, p y.instruction := by
  induction placed <;> simp_all

structure PlacedBlock (locals : LocalTypes Ty) (source : MemoryGraph lang) (a b : MemoryBlock lang) : Prop where
  id : a.id = b.id
  params : a.params = b.params
  ending : a.ending = b.ending
  boundary : a.boundaryOrigins = b.boundaryOrigins
  body : BodyPlacement locals (entryLifetime source a) a.body b.body

structure PlacedGraph (locals : LocalTypes Ty) (a b : MemoryGraph lang) : Prop where
  entry : a.entry = b.entry
  blocks : Traversal (PlacedBlock locals a) a.blocks.toList b.blocks.toList

private theorem placeGraph_placed {locals : LocalTypes Ty} {fn : FnId} {a b : MemoryGraph lang}
    (ok : placeGraph locals fn a = .ok b) : PlacedGraph locals a b := by
  unfold placeGraph at ok
  simp only [bind, Except.bind] at ok
  split at ok
  · contradiction
  · rename_i blocks hb
    cases ok
    have walk := traversal_mapM _ hb
    have rel (x y : MemoryBlock lang)
        (h : (do let body ← placeBody locals fn x.id (entryLifetime a x) x.body
                 return {x with body}) = Except.ok y) : PlacedBlock locals a x y := by
      cases e : placeBody locals fn x.id (entryLifetime a x) x.body <;>
        simp [e, bind, Except.bind, pure, Except.pure] at h
      subst y
      exact ⟨rfl, rfl, rfl, rfl, placeBody_placement _ _ _ _ e⟩
    refine ⟨rfl, ?_⟩
    constructor
    · intro x hx
      obtain ⟨y, hy, h⟩ := walk.forward _ hx
      exact ⟨y, hy, rel _ _ h⟩
    · intro y hy
      obtain ⟨x, hx, h⟩ := walk.backward _ hy
      exact ⟨x, hx, rel _ _ h⟩
    · intro κ left right h
      exact walk.keys _ _ fun x y xy => h x y (rel x y xy)

private theorem PlacedGraph.wf {locals : LocalTypes Ty} {a b : MemoryGraph lang} (placed : PlacedGraph locals a b)
    (wf : a.WF Terminator.successors Sigma.fst Sigma.fst) :
    b.WF Terminator.successors Sigma.fst Sigma.fst := by
  obtain ⟨ids, ⟨entry, he, eid⟩, edges⟩ := wf
  obtain ⟨eb, heb, erel⟩ := placed.blocks.forward _ he
  refine ⟨placed.blocks.keys _ _ (fun _ _ => PlacedBlock.id) ▸ ids,
    ⟨eb, heb, erel.id.symm.trans (eid.trans placed.entry)⟩, ?_⟩
  intro block hb edge mem
  obtain ⟨src, hs, rel⟩ := placed.blocks.backward _ hb
  obtain ⟨succ, hn, id, types⟩ := edges src hs _ (rel.ending ▸ mem)
  obtain ⟨next, hn', link⟩ := placed.blocks.forward _ hn
  exact ⟨next, hn', link.id.symm.trans id, link.params ▸ types⟩

private theorem PlacedGraph.signature {locals : LocalTypes Ty} {profile : Profile} {a b : MemoryGraph lang} (placed : PlacedGraph locals a b)
    {src dst : Interfaces.Memory lang profile} (sigs : dst.signatures = src.signatures)
    {fn : FnId} (valid : src.SignatureMatches fn a) : dst.SignatureMatches fn b := by
  obtain ⟨sig, found, ⟨entry, he, eid, types⟩, blocks⟩ := valid
  obtain ⟨eb, heb, erel⟩ := placed.blocks.forward _ he
  refine ⟨sig, sigs ▸ found,
    ⟨eb, heb, erel.id.symm.trans (eid.trans placed.entry), erel.params ▸ types⟩, ?_⟩
  intro block hb
  obtain ⟨source, hs, rel⟩ := placed.blocks.backward _ hb
  obtain ⟨term, calls⟩ := blocks _ hs
  constructor
  · rw [← rel.ending]; exact term
  · rw [sigs]
    exact rel.body.forall (fun inst => match inst with
      | .call callee destination args _ => ∃ target,
        AList.lookup src.signatures callee = some target ∧
        args.map Sigma.fst = target.parameters ∧ destination.map Sigma.fst = target.result
      | _ => True) (fun _ => True.intro) calls

private theorem PlacedGraph.origins {locals : LocalTypes Ty} {a b : MemoryGraph lang} (placed : PlacedGraph locals a b) (o : Origin) :
    o ∈ b.origins Terminator.origins ↔ o ∈ a.origins Terminator.origins := by
  have member {x y : MemoryBlock lang} (rel : PlacedBlock locals a x y) :
      o ∈ y.body.map LocatedOp.origin ++ y.boundaryOrigins ++ y.ending.origins ↔
        o ∈ x.body.map LocatedOp.origin ++ x.boundaryOrigins ++ x.ending.origins := by
    simp only [List.mem_append, rel.body.origins, rel.boundary, rel.ending]
  simp only [Interfaces.CFG.origins, List.mem_flatMap]
  constructor
  · rintro ⟨y, hy, mem⟩
    obtain ⟨x, hx, h⟩ := placed.blocks.backward _ hy
    exact ⟨x, hx, (member h).mp mem⟩
  · rintro ⟨x, hx, mem⟩
    obtain ⟨y, hy, h⟩ := placed.blocks.forward _ hx
    exact ⟨y, hy, (member h).mpr mem⟩

private def placeFunction (entry : FnId × LocalTypes Ty × MemoryGraph lang) :
    Except LifetimeJoin (FnId × LocalTypes Ty × MemoryGraph lang) := do
  return (entry.1, entry.2.1, ← placeGraph entry.2.1 entry.1 entry.2.2)

inductive GraphFailure (Ty : Type) where
  | layout (missing : MissingLayout Ty)
  | lifetime (join : LifetimeJoin)

variable {language : CoreLanguage} {lowering : LoweringLanguage language.operands.Ty} {profile : Profile}

/-- ISS-0119. Produce the typed Memory carrier executed by Memory.Execution.
Function and block parameters start their own lifetimes through WriteParams. -/
def lowerGraph (mir : MIR lowering profile) :
    Except (GraphFailure language.operands.Ty) (Interfaces.Memory lowering profile) := do
  let prepared ← (prepareGraph (language := language) mir).mapError GraphFailure.layout
  let functions ← (prepared.functions.mapM placeFunction).mapError GraphFailure.lifetime
  return {
    project := mir.project
    entry := mir.entry
    signatures := mir.signatures
    functions
    storage := prepared.storage
    layouts := prepared.layouts
    mutableLocals := mir.mutableLocals}

/-- Static part of the frozen contract on the actual result of lowering.
The source handoff is the input invariant of the registry edge. -/
structure GraphHandoff (mir : MIR lowering profile) (memory : Interfaces.Memory lowering profile) : Prop where
  project : memory.project = mir.project
  entry : memory.entry = mir.entry
  signatures : memory.signatures = mir.signatures
  mutability : memory.mutableLocals = mir.mutableLocals
  origins : PreservesOrigins mir.origins memory.origins
  layouts : LayoutsMatch language.layout memory
  promotion : PromotionSound (language := language) mir memory
  handoff : memory.HandoffWF
  placement : ∃ prepared, prepareGraph (language := language) mir = .ok prepared ∧
    ∀ fn locals graph, (fn, locals, graph) ∈ prepared.functions →
      ∃ placed, (fn, locals, placed) ∈ memory.functions ∧ PlacedGraph locals graph placed

theorem lowerGraph_handoff {mir : MIR lowering profile} {memory : Interfaces.Memory lowering profile}
    (ok : lowerGraph (language := language) mir = .ok memory)
    (source : mir.logicalView.HandoffWF) : GraphHandoff (language := language) mir memory := by
  unfold lowerGraph at ok
  cases prep : prepareGraph (language := language) mir with
  | error err => simp [prep, Except.mapError, bind, Except.bind] at ok
  | ok graphs =>
    cases body : graphs.functions.mapM placeFunction with
    | error err => simp [prep, body, Except.mapError, bind, Except.bind] at ok
    | ok functions =>
      simp only [prep, body, Except.mapError, bind, Except.bind, pure, Except.pure, Except.ok.injEq] at ok
      subst memory
      let memory : Interfaces.Memory lowering profile := {
        project := mir.project, entry := mir.entry, signatures := mir.signatures
        functions, layouts := graphs.layouts, storage := graphs.storage, mutableLocals := mir.mutableLocals }
      have walk := traversal_mapM placeFunction body
      have shape {x y : FnId × LocalTypes language.operands.Ty × MemoryGraph lowering}
          (h : placeFunction x = .ok y) :
          x.1 = y.1 ∧ x.2.1 = y.2.1 ∧ PlacedGraph x.2.1 x.2.2 y.2.2 := by
        unfold placeFunction at h
        cases placed : placeGraph x.2.1 x.1 x.2.2 <;> simp [placed, bind, Except.bind, pure, Except.pure] at h
        subst y
        exact ⟨rfl, rfl, placeGraph_placed placed⟩
      have ids := walk.keys Prod.fst Prod.fst fun _ _ h => (shape h).1
      have decls := walk.keys (fun e => (e.1, e.2.1)) (fun e => (e.1, e.2.1))
        fun _ _ h => Prod.ext (shape h).1 (shape h).2.1
      obtain ⟨layouts, layoutKeys, slots⟩ := prepareGraph_layouts (memory := memory)
        prep rfl rfl decls.symm source.1
      obtain ⟨keys, storageKeys, storage, valid⟩ := prepareGraph_tables prep source.1
      have retained fn locals graph (h : (fn, locals, graph) ∈ graphs.functions) :
          ∃ placed, (fn, locals, placed) ∈ memory.functions := by
        obtain ⟨⟨_, _, placed⟩, hm, rel⟩ := walk.forward _ h
        obtain ⟨rfl, rfl, _⟩ := shape rel
        exact ⟨placed, hm⟩
      have sourceLocals fn locals graph (h : (fn, locals, graph) ∈ mir.functions) :
          (List.map Prod.fst locals).Nodup := by
        have m : ∃ logical, (fn, locals, logical) ∈ mir.logicalView.functions :=
          ⟨_, List.mem_map_of_mem (f := fun (e : FnId × _ × MIRGraph lowering) => (e.1, _)) h⟩
        obtain ⟨_, hm⟩ := m
        exact (source.1.2.2.2.2.2.2 _ _ _ hm).1
      have promoted := prepareGraph_promotion (memory := memory) prep rfl retained
        (show (List.map Prod.fst mir.functions).Nodup from by
          simpa only [Interfaces.MIR.logicalView, AList.map, List.map_map, Function.comp_def] using source.1.1) sourceLocals
      have origins (o : Origin) : o ∈ memory.origins ↔ o ∈ mir.logicalView.origins := by
        rw [← prepareGraph_origins prep]
        change o ∈ functions.flatMap _ ↔ _
        simp only [List.mem_flatMap]
        constructor
        · rintro ⟨y, hy, member⟩
          obtain ⟨x, hx, rel⟩ := walk.backward _ hy
          exact ⟨x, hx, ((shape rel).2.2.origins o).mp member⟩
        · rintro ⟨x, hx, member⟩
          obtain ⟨y, hy, rel⟩ := walk.forward _ hx
          exact ⟨y, hy, ((shape rel).2.2.origins o).mpr member⟩
      have fnKeys := ids.symm.trans keys
      refine ⟨rfl, rfl, rfl, rfl, ?_, layouts, promoted, ?_, ?_⟩
      · rw [← logicalView_origins mir]
        exact ⟨fun o => (origins o).mp, fun o => (origins o).mpr⟩
      · refine ⟨⟨fnKeys ▸ source.1.1, source.1.2.1, storageKeys ▸ source.1.1,
            layoutKeys ▸ source.1.1, storage, slots, ?_⟩,
          fnKeys ▸ source.2.1, fun o h => source.2.2 o ((origins o).mp h)⟩
        intro fn locals graph hm
        obtain ⟨⟨oldFn, oldLocals, src⟩, hs, rel⟩ := walk.backward _ hm
        obtain ⟨rfl, rfl, placement⟩ := shape rel
        obtain ⟨unique, cfg, sig⟩ := valid oldFn oldLocals src hs
        exact ⟨unique, placement.wf cfg, placement.signature rfl sig⟩
      · refine ⟨graphs, prep, ?_⟩
        intro fn locals graph h
        obtain ⟨⟨_, _, placed⟩, hm, rel⟩ := walk.forward _ h
        obtain ⟨rfl, rfl, certificate⟩ := shape rel
        exact ⟨placed, hm, certificate⟩

end Concrete.Memory
