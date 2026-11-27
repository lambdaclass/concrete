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

/-- A materialized root whose type has no layout on the selected profile. -/
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
      | some layout => pure (some (root, layout))
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
      some [(controlFn, [(⟨0⟩, ⟨0, 1⟩)])] := by with_unfolding_all rfl

private abbrev controlLang := sliceLoweringLanguage slice1
private abbrev u8Ty : (sliceLanguage slice1).operands.Ty := .inl (.scalar (.int .u8))

private def controlOrigin : Origin :=
  ⟨⟨0⟩, ⟨⟨0⟩, 0, 2, 1, 1⟩, ⟨.function controlFn, ⟨[]⟩⟩⟩

private def bindReturnControl : MIR controlLang profileControl :=
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
      prepareGraph (language := sliceLanguage slice1) bindReturnControl
  trivial

example :
    let root : MemoryPlace _ u8Ty := ⟨⟨⟨0⟩, []⟩, .virtual⟩
    (prepareGraph (language := sliceLanguage slice1) bindReturnControl).map
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
        [(controlFn, [(⟨1⟩, ⟨1, 1⟩)])]) := by with_unfolding_all rfl

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

/-- The physical bind rule cannot initialize a fresh destination in one step.
The MIR bind rule uses WriteParams, which can start its lifetime. -/
theorem physical_bind_requires_live {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    (memory : Interfaces.Memory lang profile) (state next : MemoryRegisters language profile)
    {ty : language.operands.Ty} (dst : MemoryPlace _ ty) (literal : lang.literal ty)
    (fresh : dst.logical.root ∉ state.live) :
    ¬ memory.InstructionStep state next false (.bind dst (.literal literal)) := by
  rintro ⟨value, afterRead, stored, read, writes, _⟩
  have same : afterRead = state := by
    simp only [Interfaces.Memory.readValue] at read
    cases h : language.literal ty literal.encoding <;> simp [h] at read
    exact read.2.symm
  subst afterRead
  have store := writes.2.2.1
  simp [Interfaces.Memory.writeValue, fresh] at store

end Concrete.Memory
