import Concrete.Memory

namespace Concrete.Memory

open Concrete.Interfaces ConcreteSpec.Core ConcreteSpec.StageInterface

section
variable {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty} {profile : Profile}

private def classifyPlace (classify : PlaceId → StorageClass) {ty : language.operands.Ty}
    (place : MemoryPlace _ ty) : MemoryPlace _ ty :=
  {place with storage := classify place.logical.root}

private def classifyRead (classify : PlaceId → StorageClass) {ty : language.operands.Ty} :
    MemoryRead _ lang.literal ty → MemoryRead _ lang.literal ty
  | .literal value => .literal value
  | .copy place => .copy (classifyPlace classify place)
  | .move place => .move (classifyPlace classify place)

private def classifyArg (classify : PlaceId → StorageClass)
    (arg : Sigma (MemoryRead language.operands.Ty lang.literal)) :
    Sigma (MemoryRead _ lang.literal) :=
  ⟨arg.1, classifyRead classify arg.2⟩

private def classifyParam (classify : PlaceId → StorageClass)
    (param : Sigma (MemoryPlace language.operands.Ty)) : Sigma (MemoryPlace language.operands.Ty) :=
  ⟨param.1, classifyPlace classify param.2⟩

private def classifyArgs (classify : PlaceId → StorageClass) : {types : List language.operands.Ty} →
    Args (MemoryRead _ lang.literal) types → Args (MemoryRead _ lang.literal) types
  | _, .nil => .nil
  | _, .cons arg rest => .cons (classifyRead classify arg) (classifyArgs classify rest)

private def classifyInst (classify : PlaceId → StorageClass) : MemoryInst lang → MemoryInst lang
  | .bind dst src => .bind (classifyPlace classify dst) (classifyRead classify src)
  | .assign dst src => .assign (classifyPlace classify dst) (classifyRead classify src)
  | .op dst op args checks addresses =>
    .op (classifyPlace classify dst) op (classifyArgs classify args) checks
      (addresses.map (classifyParam classify))
  | .beginLifetime place => .beginLifetime place
  | .endLifetime place => .endLifetime place
  | .destructure src bindings =>
    .destructure (classifyPlace classify src)
      (bindings.map fun (field, dst) => (field, classifyParam classify dst))
  | .call callee dst args checks =>
    .call callee (dst.map (classifyParam classify)) (args.map (classifyArg classify)) checks

private def classifyEdge (classify : PlaceId → StorageClass)
    (edge : CFGEdge (Sigma (MemoryRead language.operands.Ty lang.literal))) :
    CFGEdge (Sigma (MemoryRead _ lang.literal)) :=
  {edge with arguments := edge.arguments.map (classifyArg classify)}

private def classifyTerm (classify : PlaceId → StorageClass) : MemoryTerm lang → MemoryTerm lang
  | .jump next => .jump (classifyEdge classify next)
  | .branch test yes no =>
    .branch (classifyRead classify test) (classifyEdge classify yes) (classifyEdge classify no)
  | .ret result => .ret (result.map (classifyArg classify))
  | .trap kind origin => .trap kind origin
  | .invalid => .invalid

private theorem classifyTerm_origins (classify : PlaceId → StorageClass) (term : MemoryTerm lang) :
    (classifyTerm classify term).origins = term.origins := by
  cases term <;> rfl

private def classifyGraph (classes : AList PlaceId StorageClass) (graph : MemoryGraph lang) :
    MemoryGraph lang :=
  let classify := fun root => (AList.lookup classes root).getD .materialized
  {graph with blocks := graph.blocks.map fun block =>
    {block with
      params := block.params.map (classifyParam classify)
      body := block.body.map fun op => {op with instruction := classifyInst classify op.instruction}
      ending := classifyTerm classify block.ending}}

private theorem classifyGraph_origins (classes : AList PlaceId StorageClass) (graph : MemoryGraph lang) :
    (classifyGraph classes graph).origins Terminator.origins = graph.origins Terminator.origins := by
  simp only [classifyGraph, Interfaces.CFG.origins, Array.toList_map, List.flatMap_map,
    Function.comp_def, List.map_map, classifyTerm_origins]

/-- A materialized root whose type has no valid layout on the selected profile. -/
structure MissingLayout (Ty : Type) where
  fn : FnId
  root : PlaceId
  ty : Ty

private def rootLayouts (mir : MIR lang profile) (fn : FnId) (locals : LocalTypes language.operands.Ty) :
    Except (MissingLayout language.operands.Ty) (AList PlaceId ConcreteSpec.StageInterface.Layout) := do
  locals.filterMapM fun (root, ty) =>
    match storageClass (language := language) mir fn root ty with
    | .virtual => pure none
    | .materialized =>
      match language.layout mir.declarations profile.memoryTarget ty with
      | some layout => if layout.WellFormed profile.memoryTarget then pure (some (root, layout)) else throw ⟨fn, root, ty⟩
      | none => throw ⟨fn, root, ty⟩

private def classifyLocals (mir : MIR lang profile) (fn : FnId)
    (locals : LocalTypes language.operands.Ty) : AList PlaceId StorageClass :=
  List.map (fun (root, ty) => (root, storageClass (language := language) mir fn root ty)) locals

private theorem classifyLocals_keys (mir : MIR lang profile) (fn : FnId)
    (locals : LocalTypes language.operands.Ty) :
    List.map Prod.fst (classifyLocals (language := language) mir fn locals) =
      List.map Prod.fst locals := by
  simp only [classifyLocals, List.map_map]
  rfl

/-- Ordered traversal of a compiler table. Both membership directions matter:
checking just existing outputs would admit a missing layout or block. -/
structure Traversal {α β : Type} (rel : α → β → Prop) (xs : List α) (ys : List β) : Prop where
  forward : ∀ x ∈ xs, ∃ y ∈ ys, rel x y
  backward : ∀ y ∈ ys, ∃ x ∈ xs, rel x y
  keys : ∀ {κ : Type} (left : α → κ) (right : β → κ),
    (∀ x y, rel x y → left x = right y) → xs.map left = ys.map right

theorem traversal_mapM {α β ε : Type} (f : α → Except ε β) {xs ys}
    (ok : xs.mapM f = .ok ys) : Traversal (fun x y => f x = .ok y) xs ys := by
  induction xs generalizing ys with
  | nil =>
    cases ok
    exact ⟨nofun, nofun, fun _ _ _ => rfl⟩
  | cons x xs ih =>
    rw [List.mapM_cons] at ok
    cases head : f x <;> cases tail : xs.mapM f <;>
      simp only [head, tail, bind, Except.bind, pure, Except.pure, reduceCtorEq] at ok
    rename_i y ys'
    cases ok
    obtain ⟨forward, backward, keys⟩ := ih tail
    constructor
    · intro a h
      rcases List.mem_cons.mp h with rfl | h
      · exact ⟨_, .head _, head⟩
      · obtain ⟨b, hb, rel⟩ := forward _ h
        exact ⟨b, .tail _ hb, rel⟩
    · intro b h
      rcases List.mem_cons.mp h with rfl | h
      · exact ⟨_, .head _, head⟩
      · obtain ⟨a, ha, rel⟩ := backward _ h
        exact ⟨a, .tail _ ha, rel⟩
    · intro κ left right same
      simp only [List.map_cons, same _ _ head, keys left right same]

private structure RootLayoutsMatch (mir : MIR lang profile) (fn : FnId)
    (locals : LocalTypes language.operands.Ty) (slots : AList PlaceId Layout) : Prop where
  keys : (List.map Prod.fst slots).Sublist (List.map Prod.fst locals)
  complete : ∀ root ty, (root, ty) ∈ locals →
    storageClass (language := language) mir fn root ty = .materialized →
    ∃ d, (root, d) ∈ slots ∧ language.layout mir.declarations profile.memoryTarget ty = some d
  sound : ∀ root d, (root, d) ∈ slots → ∃ ty, (root, ty) ∈ locals ∧
    storageClass (language := language) mir fn root ty = .materialized ∧
    language.layout mir.declarations profile.memoryTarget ty = some d ∧ d.WellFormed profile.memoryTarget

private theorem rootLayouts_match (mir : MIR lang profile) (fn : FnId)
    {locals : LocalTypes language.operands.Ty} {slots}
    (ok : rootLayouts mir fn locals = .ok slots) :
    RootLayoutsMatch (language := language) mir fn locals slots := by
  induction locals generalizing slots with
  | nil =>
    cases ok
    exact ⟨.slnil, fun _ _ => nofun, fun _ _ => nofun⟩
  | cons pair rest ih =>
    obtain ⟨root, ty⟩ := pair
    unfold rootLayouts at ok
    rw [List.filterMapM_cons] at ok
    simp only [← rootLayouts.eq_1] at ok
    cases cls : storageClass (language := language) mir fn root ty with
    | virtual =>
      simp only [cls, pure, Except.pure, bind, Except.bind] at ok
      have tail := ih ok
      refine ⟨tail.keys.cons _, ?_, ?_⟩
      · intro p t mem c
        rcases List.mem_cons.mp mem with eq | mem
        · cases eq; rw [cls] at c; contradiction
        · exact tail.complete p t mem c
      · intro p d mem
        obtain ⟨t, ht, c, l, wf⟩ := tail.sound p d mem
        exact ⟨t, .tail _ ht, c, l, wf⟩
    | materialized =>
      cases layout : language.layout mir.declarations profile.memoryTarget ty with
      | none => simp [cls, layout, bind, Except.bind, throw] at ok
      | some d =>
        by_cases wf : d.WellFormed profile.memoryTarget
        case neg => simp [cls, layout, wf, bind, Except.bind, throw] at ok
        simp only [cls, layout, ite_eq_left wf] at ok
        cases hr : rootLayouts mir fn rest with
        | error e => simp [hr, pure, Except.pure, bind, Except.bind] at ok
        | ok ds =>
          simp only [hr, pure, Except.pure, bind, Except.bind, Except.ok.injEq] at ok
          subst slots
          have tail := ih hr
          refine ⟨tail.keys.cons_cons _, ?_, ?_⟩
          · intro p t mem c
            rcases List.mem_cons.mp mem with eq | mem
            · cases eq; exact ⟨d, .head _, layout⟩
            · obtain ⟨slot, hm, desc⟩ := tail.complete p t mem c
              exact ⟨slot, .tail _ hm, desc⟩
          · intro p slot mem
            rcases List.mem_cons.mp mem with eq | mem
            · cases eq; exact ⟨ty, .head _, cls, layout, wf⟩
            · obtain ⟨t, hm, c, desc, valid⟩ := tail.sound p slot mem
              exact ⟨t, .tail _ hm, c, desc, valid⟩

/-- Storage-annotated CFGs awaiting lifetime placement. The source MIR
still owns the project, entry and signatures at this point. -/
structure PreparedGraphs (lang : LoweringLanguage language.operands.Ty) (profile : Profile) where
  functions : AList FnId (LocalTypes language.operands.Ty × MemoryGraph lang)
  storage : AList FnId (AList PlaceId StorageClass)
  layouts : AList FnId (AList PlaceId ConcreteSpec.StageInterface.Layout)

def prepareGraph (mir : MIR lang profile) :
    Except (MissingLayout language.operands.Ty) (PreparedGraphs lang profile) := do
  let view := mir.logicalView
  let classes := classifyLocals (language := language) mir
  let layouts ← view.functions.mapM fun (fn, locals, _) => do
    return (fn, ← rootLayouts mir fn locals)
  return {
    functions := List.map (fun (fn, locals, graph) =>
      (fn, locals, classifyGraph (classes fn locals) graph)) view.functions
    storage := List.map (fun (fn, locals, _) => (fn, classes fn locals)) view.functions
    layouts}

/-- The prepared layout and storage tables are exact for any placement retaining
the ordered function/local declarations, resolving types in the source environment.
The sublist property also gives layout
key uniqueness, without trusting the layout query to deduplicate a table. -/
theorem prepareGraph_layouts {mir : MIR lang profile} {graphs : PreparedGraphs lang profile}
    {memory : Interfaces.Memory lang profile}
    (prepared : prepareGraph (language := language) mir = .ok graphs)
    (storage : memory.storage = graphs.storage) (layouts : memory.layouts = graphs.layouts)
    (decls : (List.map (fun e => (e.1, e.2.1)) memory.functions) =
      (List.map (fun e => (e.1, e.2.1)) graphs.functions))
    (source : mir.logicalView.TablesWF) :
    LayoutsMatch (profile := profile) language.layout {memory with declarations := mir.declarations} ∧
      (List.map Prod.fst memory.layouts = List.map Prod.fst mir.logicalView.functions) ∧
      (∀ fn slots, (fn, slots) ∈ memory.layouts → (List.map Prod.fst slots).Nodup) ∧
      ∀ table ∈ memory.layouts, ∀ slot ∈ table.2, slot.2.WellFormed profile.memoryTarget := by
  unfold prepareGraph at prepared
  simp only [bind, Except.bind] at prepared
  split at prepared
  · contradiction
  · rename_i tables ht
    cases prepared
    let view := mir.logicalView (language := language)
    change memory.layouts = tables at layouts
    unfold LayoutsMatch
    rw [layouts]
    have traversed := traversal_mapM _ ht
    have record {x : FnId × LocalTypes language.operands.Ty × MemoryGraph lang}
        {y : FnId × AList PlaceId Layout}
        (rel : (do return (x.1, ← rootLayouts mir x.1 x.2.1)) = (Except.ok y)) :
        x.1 = y.1 ∧ RootLayoutsMatch (language := language) mir x.1 x.2.1 y.2 := by
      cases h : rootLayouts mir x.1 x.2.1 <;> simp [h, bind, Except.bind, pure, Except.pure] at rel
      subst y
      exact ⟨rfl, rootLayouts_match mir x.1 h⟩
    have keys : List.map Prod.fst view.functions = List.map Prod.fst tables :=
      traversed.keys _ _ fun _ _ h => (record h).1
    have tablesUnique := keys ▸ source.1
    have ds : (List.map (fun e => (e.1, e.2.1)) memory.functions) =
        (List.map (fun e => (e.1, e.2.1)) view.functions) := by
      simpa only [List.map_map, Function.comp_def] using decls
    have classes {fn locals graph} (member : (fn, locals, graph) ∈ view.functions) :
        AList.lookup memory.storage fn = some (classifyLocals (language := language) mir fn locals) := by
      rw [storage]
      apply AList.lookup_of_mem
      · simp only [List.map_map]; exact source.1
      · exact List.mem_map_of_mem member
    have localClass {fn locals graph root ty} (member : (fn, locals, graph) ∈ view.functions)
        (declared : (root, ty) ∈ locals) :
        AList.lookup (classifyLocals (language := language) mir fn locals) root =
          some (storageClass (language := language) mir fn root ty) := by
      apply AList.lookup_of_mem
      · rw [classifyLocals_keys]
        exact (source.2.2.2.2.2.2 fn locals graph member).1
      · exact List.mem_map_of_mem declared
    refine ⟨⟨?_, ?_⟩, keys.symm, ?_, ?_⟩
    · intro fn locals graph member root ty declared
      have d := List.mem_map_of_mem (f := fun e => (e.1, e.2.1)) member
      rw [ds] at d
      obtain ⟨⟨_, _, src⟩, hs, eq⟩ := List.mem_map.mp d
      cases eq
      refine ⟨_, classes hs, ?_⟩
      rw [localClass hs declared]
      cases c : storageClass (language := language) mir fn root ty with
      | virtual => exact .inl rfl
      | materialized =>
        obtain ⟨⟨_, slots⟩, ht', rel⟩ := traversed.forward _ hs
        obtain ⟨eq, desc⟩ := record rel
        cases eq
        obtain ⟨layout, hm, hl⟩ := desc.complete _ _ declared c
        exact .inr ⟨rfl, slots, layout, AList.lookup_of_mem tablesUnique ht',
          AList.lookup_of_mem (desc.keys.nodup (source.2.2.2.2.2.2 _ _ _ hs).1) hm, hl⟩
    · intro fn slots root description inTable inSlots
      obtain ⟨⟨_, locals, graph⟩, hs, rel⟩ := traversed.backward _ inTable
      obtain ⟨eq, desc⟩ := record rel
      cases eq
      obtain ⟨ty, declared, classEq, layout, _⟩ := desc.sound _ _ inSlots
      have d := List.mem_map_of_mem (f := fun e => (e.1, e.2.1)) hs
      rw [← ds] at d
      obtain ⟨⟨_, _, placed⟩, member, eq⟩ := List.mem_map.mp d
      cases eq
      exact ⟨locals, placed, ty, _, member, declared, classes hs,
        (localClass hs declared).trans (congrArg some classEq), layout⟩
    · intro fn slots member
      obtain ⟨x, hs, rel⟩ := traversed.backward _ member
      exact (record rel).2.keys.nodup (source.2.2.2.2.2.2 _ _ _ hs).1

    · intro table member slot declared
      obtain ⟨x, _, rel⟩ := traversed.backward _ member
      obtain ⟨_, _, _, _, valid⟩ := (record rel).2.sound slot.1 slot.2 declared
      exact valid

theorem logicalView_origins (mir : MIR lang profile) :
    mir.logicalView.origins = mir.origins := by
  simp only [Interfaces.MIR.logicalView, Interfaces.Memory.origins, Interfaces.MIR.origins,
    AList.map, List.flatMap_map, Function.comp_def, Interfaces.CFG.origins,
    Array.toList_map, List.map_map]
  congr 1
  funext fn
  congr 1
  funext block
  cases block.ending <;> with_unfolding_all rfl

private theorem classifyGraph_wf (classes : AList PlaceId StorageClass) (graph : MemoryGraph lang)
    (wf : graph.WF Terminator.successors Sigma.fst Sigma.fst) :
    (classifyGraph classes graph).WF Terminator.successors Sigma.fst Sigma.fst := by
  let classify := fun root => (AList.lookup classes root).getD .materialized
  have edges (term : MemoryTerm lang) :
      (classifyTerm classify term).successors = term.successors.map (classifyEdge classify) := by
    cases term <;> rfl
  obtain ⟨ids, ⟨entry, he, id⟩, successors⟩ := wf
  simp only [classifyGraph, Interfaces.CFG.WF, Array.toList_map, List.map_map, List.mem_map,
    Function.comp_def]
  refine ⟨ids, ⟨_, ⟨entry, he, rfl⟩, id⟩, ?_⟩
  rintro _ ⟨block, hb, rfl⟩ edge hEdge
  rw [edges] at hEdge
  obtain ⟨e, he, rfl⟩ := List.mem_map.mp hEdge
  obtain ⟨next, hn, id, types⟩ := successors _ hb _ he
  refine ⟨_, ⟨next, hn, rfl⟩, id, ?_⟩
  simpa only [classifyEdge, List.map_map, Function.comp_def, classifyArg, classifyParam] using types

private theorem classifyGraph_signature {src dst : Interfaces.Memory lang profile}
    (signatures : dst.signatures = src.signatures) (fn : FnId)
    (classes : AList PlaceId StorageClass) (graph : MemoryGraph lang)
    (valid : src.SignatureMatches fn graph) :
    dst.SignatureMatches fn (classifyGraph classes graph) := by
  obtain ⟨sig, found, ⟨entry, he, id, params⟩, blockValid⟩ := valid
  simp only [Interfaces.Memory.SignatureMatches, classifyGraph, Array.toList_map, List.mem_map,
    signatures]
  refine ⟨sig, found, ⟨_, ⟨entry, he, rfl⟩, id, ?_⟩, ?_⟩
  · simpa only [List.map_map, Function.comp_def, classifyParam] using params
  · rintro _ ⟨block, hb, rfl⟩
    obtain ⟨term, calls⟩ := blockValid _ hb
    constructor
    · cases h : block.ending <;> simp only [h] at term <;>
        first | exact term | simpa only [classifyTerm, Option.map_map, Function.comp_def, classifyArg] using term
    · intro item member
      obtain ⟨op, hop, rfl⟩ := List.mem_map.mp member
      have call := calls _ hop
      cases h : op.instruction <;> simp only [h] at call <;>
        first | exact call | simpa only [classifyInst, List.map_map, Option.map_map, Function.comp_def, classifyArg, classifyParam] using call

/-- Classification preserves the type of every edge, return and call, including
those of unreachable blocks. -/
theorem prepareGraph_tables {mir : MIR lang profile} {graphs : PreparedGraphs lang profile}
    (prepared : prepareGraph (language := language) mir = .ok graphs)
    (source : mir.logicalView.TablesWF) :
    (List.map Prod.fst graphs.functions = List.map Prod.fst mir.logicalView.functions) ∧
    (List.map Prod.fst graphs.storage = List.map Prod.fst mir.logicalView.functions) ∧
    (∀ fn classes, (fn, classes) ∈ graphs.storage → (List.map Prod.fst classes).Nodup) ∧
    ∀ fn locals graph, (fn, locals, graph) ∈ graphs.functions →
      (List.map Prod.fst locals).Nodup ∧ graph.WF Terminator.successors Sigma.fst Sigma.fst ∧
      mir.logicalView.SignatureMatches fn graph := by
  simp only [prepareGraph, bind, Except.bind] at prepared
  split at prepared
  · contradiction
  · cases prepared
    refine ⟨by simp only [List.map_map]; rfl, by simp only [List.map_map]; rfl, ?_, ?_⟩
    · intro fn classes member
      obtain ⟨⟨_, locals, graph⟩, hs, eq⟩ := List.mem_map.mp member
      cases eq
      rw [classifyLocals_keys]
      exact (source.2.2.2.2.2.2 _ _ _ hs).1
    · intro fn locals graph member
      obtain ⟨⟨_, _, src⟩, hs, eq⟩ := List.mem_map.mp member
      cases eq
      obtain ⟨unique, wf, sig⟩ := source.2.2.2.2.2.2 _ _ _ hs
      exact ⟨unique, classifyGraph_wf _ src wf, classifyGraph_signature rfl _ _ src sig⟩

theorem prepareGraph_origins {mir : MIR lang profile} {graphs : PreparedGraphs lang profile}
    (prepared : prepareGraph (language := language) mir = .ok graphs) :
    (graphs.functions.flatMap fun (_, _, graph) => graph.origins Terminator.origins) =
      mir.logicalView.origins := by
  simp only [prepareGraph, bind, Except.bind] at prepared
  split at prepared
  · contradiction
  · simp only [pure, Except.pure, Except.ok.injEq] at prepared
    subst graphs
    simp only [Interfaces.Memory.origins, List.flatMap_map, classifyGraph_origins]

/-- Lifetime placement can change the instructions without changing the
declared locals or their classification. These are the only target fields
the promotion contract reads. -/
theorem prepareGraph_promotion {mir : MIR lang profile} {graphs : PreparedGraphs lang profile}
    {memory : Interfaces.Memory lang profile}
    (prepared : prepareGraph (language := language) mir = .ok graphs)
    (sameStorage : memory.storage = graphs.storage)
    (retainsLocals : ∀ fn locals graph, (fn, locals, graph) ∈ graphs.functions →
      ∃ placed, (fn, locals, placed) ∈ memory.functions)
    (functionsUnique : (List.map Prod.fst mir.functions).Nodup)
    (localsUnique : ∀ fn locals graph, (fn, locals, graph) ∈ mir.functions →
      (List.map Prod.fst locals).Nodup) :
    PromotionSound (language := language) mir memory := by
  simp only [prepareGraph, bind, Except.bind] at prepared
  split at prepared
  · contradiction
  · simp only [pure, Except.pure, Except.ok.injEq] at prepared
    subst graphs
    unfold PromotionSound
    rw [sameStorage]
    constructor
    · intro fn locals graph member root ty declared
      let classes := classifyLocals (language := language) mir fn locals
      let storage := storageClass (language := language) mir fn root ty
      refine ⟨classes, storage, ?_, ?_, storageClass_virtual_iff mir fn root ty⟩
      · apply AList.lookup_of_mem
        · simp only [Interfaces.MIR.logicalView, AList.map, List.map_map]
          exact functionsUnique
        · simp only [Interfaces.MIR.logicalView, AList.map, List.map_map, List.mem_map]
          exact ⟨(fn, locals, graph), member, rfl⟩
      · apply AList.lookup_of_mem
        · rw [classifyLocals_keys]
          exact localsUnique fn locals graph member
        · exact List.mem_map_of_mem declared
    · intro fn classes root table virtual
      simp only [Interfaces.MIR.logicalView, AList.map, List.map_map, List.mem_map] at table
      obtain ⟨⟨sourceFn, locals, graph⟩, member, same⟩ := table
      simp only [Function.comp_apply, Prod.mk.injEq] at same
      obtain ⟨rfl, rfl⟩ := same
      simp only [classifyLocals, List.mem_map] at virtual
      obtain ⟨⟨sourceRoot, ty⟩, declared, same⟩ := virtual
      simp only [Prod.mk.injEq] at same
      obtain ⟨rfl, classified⟩ := same
      have logicalMember : ∃ logicalGraph, (sourceFn, locals, logicalGraph) ∈ mir.logicalView.functions := by
        unfold Interfaces.MIR.logicalView
        rw [AList.map]
        exact ⟨_, List.mem_map_of_mem member⟩
      obtain ⟨logicalGraph, logicalMember⟩ := logicalMember
      obtain ⟨placed, placedMember⟩ := retainsLocals sourceFn locals
        (classifyGraph (classifyLocals mir sourceFn locals) logicalGraph)
        (List.mem_map_of_mem logicalMember)
      refine ⟨ty, locals, graph, locals, placed, member, declared, placedMember, declared,
        (storageClass_virtual_iff mir sourceFn sourceRoot ty).mp classified⟩

end

end Concrete.Memory

namespace Concrete.Memory

open Concrete.Interfaces ConcreteSpec.Core ConcreteSpec.StageInterface

private def profileControl : Profile := ⟨.lcb, .x86_64Linux, 0, 4096⟩
private def slice1 : Slice := ⟨⟨1, by decide⟩⟩

open ConcreteSpec.Core.SourcePathSpan

private def controlFn : FnId :=
  ⟨⟨⟨⟨"control"⟩, ⟨⟨"0"⟩⟩⟩, [⟨"control"⟩], by decide⟩, ⟨"main"⟩⟩

private def controlDecl : DeclId := ⟨controlFn.module, ⟨"Empty"⟩⟩

private def nominalRootControl : MIR (sliceLoweringLanguage slice1) profileControl where
  project := Project.singleton mainSourcePath (SourceText.ofBytes [52, 50])
  entry := controlFn
  signatures := [(controlFn, ⟨[], none⟩)]
  functions := [(controlFn, [(⟨0⟩, .inl (.nominal controlDecl))],
    ⟨⟨0⟩, #[⟨⟨0⟩, [], [], .ret none, []⟩]⟩)]

example : (sliceLanguage slice1).layout [] profileControl.memoryTarget
    (.inl (.nominal controlDecl)) = none := rfl

example : prepareGraph (language := sliceLanguage slice1) nominalRootControl =
    .error ⟨controlFn, ⟨0⟩, .inl (.nominal controlDecl)⟩ := by with_unfolding_all rfl

private def declaredEmpty : Declarations := [(controlDecl, ⟨controlDecl.name, false, []⟩)]

example : (prepareGraph (language := sliceLanguage slice1) (profile := profileControl)
    {nominalRootControl with declarations := declaredEmpty}).toOption.map (·.layouts) =
      some [(controlFn, [(⟨0⟩, .scalar 0 1)])] := by with_unfolding_all rfl

private abbrev controlLang := sliceLoweringLanguage slice1
private abbrev u8Ty : (sliceLanguage slice1).operands.Ty := .inl (.scalar (.int .u8))

private def controlOrigin : Origin :=
  ⟨⟨0⟩, ⟨⟨0⟩, 0, 2, 1, 1⟩, ⟨.function controlFn, ⟨[]⟩⟩⟩

/-- The typed binder fixture used on both sides of the Memory handoff. -/
def bindReturnControl (slice : Slice) : MIR (sliceLoweringLanguage slice) profileControl :=
  let root : Place _ u8Ty := ⟨⟨0⟩, []⟩
  { nominalRootControl with
    signatures := [(controlFn, ⟨[], some u8Ty⟩)]
    functions := [(controlFn, [(⟨0⟩, u8Ty)],
      ⟨⟨0⟩, #[⟨⟨0⟩, [], [⟨controlOrigin, .bind root (.literal ⟨[42]⟩)⟩],
        .ret (some ⟨u8Ty, .copy root⟩), []⟩]⟩)] }

-- Preparation retains the bind and return, but its success cannot supply a
-- program to the frozen execution or handoff contracts.
example : True := by
  fail_if_success
    have handoff : Except (MissingLayout (sliceLanguage slice1).operands.Ty)
        (Interfaces.Memory controlLang profileControl) :=
      prepareGraph (language := sliceLanguage slice1) (bindReturnControl slice1)
  trivial

example :
    let root : MemoryPlace _ u8Ty := ⟨⟨⟨0⟩, []⟩, .virtual⟩
    (prepareGraph (language := sliceLanguage slice1) (bindReturnControl slice1)).map
      (fun prepared => prepared.functions) =
      .ok [(controlFn, [(⟨0⟩, u8Ty)],
        ⟨⟨0⟩, #[⟨⟨0⟩, [], [⟨controlOrigin, .bind root (.literal ⟨[42]⟩)⟩],
          .ret (some ⟨u8Ty, .copy root⟩), []⟩]⟩)] := by
  with_unfolding_all rfl

private def layoutControl : MIR controlLang profileControl :=
  let localRoot : Place _ u8Ty := ⟨⟨0⟩, []⟩
  let addressed : Place _ u8Ty := ⟨⟨1⟩, []⟩
  let probe : MIRInst controlLang := .op (opcode := .read) localRoot () .nil []
    [⟨u8Ty, addressed⟩]
  { nominalRootControl with
    signatures := [(controlFn, ⟨[], some u8Ty⟩)]
    functions := [(controlFn, [(⟨0⟩, u8Ty), (⟨1⟩, u8Ty)],
      ⟨⟨0⟩, #[⟨⟨0⟩, [], [⟨controlOrigin, probe⟩], .ret (some ⟨u8Ty, .copy localRoot⟩), []⟩]⟩)] }

example :
    (prepareGraph (language := sliceLanguage slice1) layoutControl).map
      (fun memory => (memory.storage, memory.layouts)) =
      .ok ([(controlFn, [(⟨0⟩, .virtual), (⟨1⟩, .materialized)])],
        [(controlFn, [(⟨1⟩, ⟨1, 1, .nil⟩)])]) := by with_unfolding_all rfl

example :
    (prepareGraph (language := sliceLanguage slice1) layoutControl).map
      (fun prepared => do
        let (_, _, graph) ← prepared.functions.head?
        let block ← graph.blocks[0]?
        let op ← block.body.head?
        match op.instruction with
        | .op dst _ _ _ addresses =>
          some (dst.storage, addresses.map fun address => address.2.storage)
        | _ => none) =
      .ok (some (.virtual, [.materialized])) := by
  with_unfolding_all rfl

/-- A nominal identity missing its declaration cannot be materialized or promoted. -/
theorem nominal_root_blocks_layout
    (memory : Interfaces.Memory (sliceLoweringLanguage slice1) profileControl)
    (lowered : LowersMemory (language := sliceLanguage slice1) nominalRootControl memory)
    (graph : MemoryGraph (sliceLoweringLanguage slice1))
    (retained : (controlFn, [(⟨0⟩, .inl (.nominal controlDecl))], graph) ∈ memory.functions) :
    False := by
  obtain ⟨classes, storage, table, classification, eligible⟩ :=
    lowered.2.2.2.2.2.1.1 controlFn _ _ (by exact List.Mem.head _) ⟨0⟩ _ (List.Mem.head _)
  have materialized : storage = .materialized := by
    cases storage with
    | virtual =>
      have scalar := (eligible.mp rfl).1
      contradiction
    | materialized => rfl
  obtain ⟨other, otherTable, valid⟩ :=
    lowered.2.2.2.1.1 controlFn _ graph retained ⟨0⟩ _ (List.Mem.head _)
  rw [table] at otherTable
  cases otherTable
  rw [classification, materialized] at valid
  rcases valid with impossible | ⟨_, _, _, _, _, existsLayout⟩
  · contradiction
  · rw [lowered.2.2.2.2.1.2] at existsLayout
    contradiction

/-- Reading can consume an initialized value or a field, but only a lifetime
instruction (or parameter binding) can change the set of live roots. -/
theorem readValue_live {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    (memory : Interfaces.Memory lang profile) (state next : MemoryRegisters language profile)
    {ty : language.operands.Ty} (src : MemoryRead _ lang.literal ty) {value}
    (read : memory.readValue state src = some (value, next)) : next.live = state.live := by
  cases src with
  | literal lit =>
    simp only [Interfaces.Memory.readValue] at read
    obtain ⟨_, _, eq⟩ := Option.bind_eq_some_iff.mp read
    cases eq
    rfl
  | copy place => rw [((Interfaces.Memory.copyRead_iff ..).mp read).2.2.2]
  | move place =>
    simp only [Interfaces.Memory.readValue, bind, Option.bind] at read
    cases ho : Own.move (memory.ownerView state place.logical.root ty .owned).1
        (memory.ownerView state place.logical.root ty .owned).2 <;>
      cases hv : memory.loadValue state place <;> simp only [ho, hv] at read <;> try contradiction
    split at read <;> obtain ⟨_, rfl⟩ := Prod.mk.inj (Option.some.inj read) <;> rfl

/-- The physical bind rule requires a live destination for every RHS,
including a move from the destination itself. -/
theorem physical_bind_requires_live {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    (memory : Interfaces.Memory lang profile) (state next : MemoryRegisters language profile)
    {ty : language.operands.Ty} (dst : MemoryPlace _ ty) (src : MemoryRead _ lang.literal ty)
    (fresh : dst.logical.root ∉ state.live) :
    ¬ memory.InstructionStep state next false (.bind dst src) := by
  rintro ⟨value, afterRead, stored, read, writes, _⟩
  have live := readValue_live memory state afterRead src read
  have store := writes.2.2.1
  simp [Interfaces.Memory.writeValue, live, fresh] at store

end Concrete.Memory
