import ConcreteChecks.StageInterfaces.BackendFixtures
import ConcreteChecks.StageInterfaces.Memory.Fields
import ConcreteChecks.StageInterfaces.Refinement

open Concrete
open Concrete.Interfaces
open ConcreteSpec Core Core.SourcePathSpan StageInterface StageGrammar

namespace StageInterfaceControls

private abbrev physicalFieldLanguage : CoreLanguage :=
  {linearProjectedLanguage with
    fieldRange := fun _ path _ =>
      if path = [splitField] then some (0, ⟨1, 1⟩)
      else if path = [secondSplitField] then some (1, ⟨1, 1⟩)
      else none}

private def physicalFields : MemoryRegisters physicalFieldLanguage testProfile :=
  {storeInitial with
    live := [storeRoot]
    initialized := [storeRoot]
    materialized := [(storeRoot, {storeRange with extent := 2})]
    bytes := ⟨[(⟨0⟩, {storeBlock with size := 2, bytes := [.init 17 none, .init 25 none]})], 1⟩}

private def physicalFieldLoads : List (LocatedOp (SSAInst testLowering)) :=
  [SSAInst.load (ty := ()) ⟨0⟩ ⟨storeRoot, [splitField]⟩,
    SSAInst.load (ty := ()) ⟨1⟩ ⟨storeRoot, [secondSplitField]⟩].map (LocatedOp.mk testOrigin)

private def physicalFieldSSA : SSA testLowering testProfile :=
  ⟨tabProject, Exec.Demo.mainF, [],
    [(Exec.Demo.mainF, [(storeRoot, ())], ⟨⟨0⟩, #[⟨⟨0⟩, [],
      physicalFieldLoads, .ret none, []⟩]⟩)],
    [(Exec.Demo.mainF, [(storeRoot, ⟨2, 1⟩)])], [], []⟩

private def physicalFieldBIR : BIR testLowering testProfile :=
  ⟨tabProject, Exec.Demo.mainF, [],
    [(Exec.Demo.mainF, [(storeRoot, ())], ⟨⟨0⟩, #[⟨⟨0⟩, [], physicalFieldLoads, .ret none, []⟩]⟩)],
    physicalFieldSSA.layouts, #[], [], []⟩

-- Both register stages consume a projected linear load through the shared
-- byte interpreter. The sibling remains readable from its separate range.
set_option maxRecDepth 4096 in
example :
    let memory := physicalFieldBIR.memoryView (language := physicalFieldLanguage)
    let first : MemoryRegisters physicalFieldLanguage testProfile :=
      {physicalFields with movedFields := [(storeRoot, [splitField])]}
    memory.readValue physicalFields (.move (ty := ()) ⟨⟨storeRoot, [splitField]⟩, .materialized⟩) =
      some (.scalar (.int .u8 17), first) ∧
    memory.readValue first (.move (ty := ()) ⟨⟨storeRoot, [splitField]⟩, .materialized⟩) = none ∧
    memory.readValue first (.move (ty := ()) ⟨⟨storeRoot, [secondSplitField]⟩, .materialized⟩) =
      some (.scalar (.int .u8 25), {first with
        movedFields := (storeRoot, [secondSplitField]) :: first.movedFields}) ∧
    ∃ next,
      (SSA.Execution (language := physicalFieldLanguage)).step physicalFieldSSA physicalFields next ∧
      (BIR.Execution (language := physicalFieldLanguage)).step physicalFieldBIR physicalFields next ∧
      next.movedFields = first.movedFields := by
  let memory := physicalFieldBIR.memoryView (language := physicalFieldLanguage)
  let first : MemoryRegisters physicalFieldLanguage testProfile :=
    {physicalFields with movedFields := [(storeRoot, [splitField])]}
  let place : MemoryPlace Unit () := ⟨⟨⟨storeRoot.id + 1⟩, []⟩, .virtual⟩
  let ready : MemoryRegisters physicalFieldLanguage testProfile :=
    {first with live := place.logical.root :: first.live}
  let stored : MemoryRegisters physicalFieldLanguage testProfile :=
    {ready with
      initialized := place.logical.root :: first.initialized
      virtualValues := [(place.logical.root, .scalar (.int .u8 17))]}
  have tick : memory.InstructionStepWith physicalFields stored.advance false true
      (.bind place (.move ⟨⟨storeRoot, [splitField]⟩, .materialized⟩)) := by
    with_unfolding_all
      refine ⟨.scalar (.int .u8 17), first, stored, rfl, ?_, rfl⟩
    apply Memory.WriteParams.cons (started := ready)
    · exact ⟨nofun, Or.inl ⟨rfl, rfl⟩⟩
    · exact ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩
    · exact .nil _
  with_unfolding_all
    exact ⟨rfl, rfl, rfl, stored.advance,
      ⟨rfl, ⟨_, rfl, tick⟩, [], rfl⟩, ⟨rfl, ⟨_, rfl, tick⟩, [], rfl⟩, rfl⟩

private def splitRegisters : List (FieldId × Sigma (SSAValue Unit)) :=
  [(splitField, ⟨(), ⟨0⟩⟩), (secondSplitField, ⟨(), ⟨1⟩⟩)]

private def splitSSA (project : Project) (dropSecond : Bool := false) : SSA testLowering testProfile :=
  ⟨project, Exec.Demo.mainF, [],
    [(Exec.Demo.mainF, [(storeRoot, ())], ⟨⟨0⟩, #[⟨⟨0⟩, [],
      [⟨testOrigin, .destructure (.materialized (ty := ()) ⟨storeRoot, []⟩)
        (if dropSecond then splitRegisters.take 1 else splitRegisters)⟩], .ret none, []⟩]⟩)],
    [(Exec.Demo.mainF, [(storeRoot, ⟨1, 1⟩)])], [], []⟩

private def splitBIR (project : Project) : BIR testLowering testProfile :=
  ⟨project, Exec.Demo.mainF, [],
    [(Exec.Demo.mainF, [(storeRoot, ())], ⟨⟨0⟩, #[⟨⟨0⟩, [],
      [⟨testOrigin, .destructure (.materialized (ty := ()) ⟨storeRoot, []⟩)
        splitRegisters⟩], .ret none, []⟩]⟩)],
    [(Exec.Demo.mainF, [(storeRoot, ⟨1, 1⟩)])], #[], [], []⟩

def splitBefore : MemoryRegisters twoFieldLanguage testProfile :=
  {backingAfter ⟨0⟩ 1 with instruction := 0}

private def splitAfter : MemoryRegisters twoFieldLanguage testProfile :=
  {splitBefore with
    instruction := 1
    live := [⟨2⟩, ⟨1⟩, storeRoot]
    initialized := [⟨2⟩, ⟨1⟩]
    virtualValues := [(⟨2⟩, .scalar (.int .u8 25)), (⟨1⟩, .scalar (.int .u8 17))]}

def fieldRegister (id : Nat) : MemoryPlace Unit () := ⟨⟨⟨id⟩, []⟩, .virtual⟩

private def splitBlock : MemoryBlock testLowering := ⟨⟨0⟩, [],
  [⟨testOrigin, .destructure storePlace
    [(splitField, ⟨(), fieldRegister 1⟩), (secondSplitField, ⟨(), fieldRegister 2⟩)]⟩], .ret none, []⟩

private def splitMemory (project : Project) : Memory testLowering testProfile :=
  ⟨project, Exec.Demo.mainF, [],
    [(Exec.Demo.mainF, [(storeRoot, ()), (⟨1⟩, ()), (⟨2⟩, ())], ⟨⟨0⟩, #[splitBlock]⟩)],
    [(Exec.Demo.mainF, [(storeRoot, ⟨1, 1⟩)])],
    [(Exec.Demo.mainF, [(storeRoot, .materialized), (⟨1⟩, .virtual), (⟨2⟩, .virtual)])],
    [(Exec.Demo.mainF, [⟨1⟩, ⟨2⟩])], []⟩

private theorem splitShape :
    Memory.DestructureFields twoFieldLanguage [] () (.other ())
      [(splitField, ()), (secondSplitField, ())] := by
  refine ⟨by decide, [splitField, secondSplitField], rfl, ?_, ?_⟩
  · intro binding member
    simp only [List.mem_cons, List.not_mem_nil, or_false] at member
    rcases member with rfl | rfl
    · exact ⟨List.mem_cons_self .., rfl⟩
    · exact ⟨List.mem_cons_of_mem _ (List.mem_cons_self ..), rfl⟩
  · exact fun _ member missing => False.elim (missing member)

set_option maxRecDepth 3072 in
set_option maxHeartbeats 1000000 in
example (project : Project) :
    (splitMemory project).Step (language := twoFieldLanguage) splitBefore splitAfter ∧
    (SSA.Execution (language := twoFieldLanguage)).step (splitSSA project) splitBefore splitAfter ∧
    (BIR.Execution (language := twoFieldLanguage)).step (splitBIR project) splitBefore splitAfter ∧
    (splitMemory project).readValue (language := twoFieldLanguage) splitAfter (.move storePlace) = none ∧
    (splitMemory project).readValue (language := twoFieldLanguage) splitAfter (.move (fieldRegister 1)) =
      some (.scalar (.int .u8 17), {splitAfter with
        initialized := [⟨2⟩]
        virtualValues := [(⟨2⟩, .scalar (.int .u8 25))]}) := by
  have tick : (splitMemory project).InstructionStepWith (language := twoFieldLanguage) splitBefore splitAfter
      false false (.destructure storePlace
        [(splitField, ⟨(), fieldRegister 1⟩), (secondSplitField, ⟨(), fieldRegister 2⟩)]) := by
    refine ⟨.other (), {splitBefore with initialized := []},
      [.scalar (.int .u8 17), .scalar (.int .u8 25)], {splitAfter with instruction := 0},
      by with_unfolding_all rfl, splitShape, by with_unfolding_all rfl, ?_, rfl⟩
    let afterRead : MemoryRegisters twoFieldLanguage testProfile := {splitBefore with initialized := []}
    let first : MemoryRegisters twoFieldLanguage testProfile := {afterRead with
      live := [⟨1⟩, storeRoot]
      initialized := [⟨1⟩]
      virtualValues := [(⟨1⟩, .scalar (.int .u8 17))]}
    apply Memory.WriteParams.cons (started := {afterRead with live := [⟨1⟩, storeRoot]}) (stored := first)
    · with_unfolding_all exact ⟨nofun, Or.inl ⟨rfl, rfl⟩⟩
    · with_unfolding_all exact ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩
    · apply Memory.WriteParams.cons (started := {first with live := [⟨2⟩, ⟨1⟩, storeRoot]})
      · with_unfolding_all exact ⟨nofun, Or.inl ⟨rfl, rfl⟩⟩
      · with_unfolding_all exact ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩
      · exact .nil _
  let block : MemoryBlock testLowering := ⟨⟨0⟩, [],
    [⟨testOrigin, .destructure storePlace
      [(splitField, ⟨(), fieldRegister 1⟩), (secondSplitField, ⟨(), fieldRegister 2⟩)]⟩], .ret none, []⟩
  refine ⟨⟨block, by with_unfolding_all rfl, by with_unfolding_all exact tick⟩, ?_, ?_, ?_, ?_⟩
  · exact ⟨by with_unfolding_all rfl, ⟨block, by with_unfolding_all rfl, by with_unfolding_all exact tick⟩,
      [], by with_unfolding_all rfl⟩
  · exact ⟨by with_unfolding_all rfl, ⟨block, by with_unfolding_all rfl, by with_unfolding_all exact tick⟩,
      [], by with_unfolding_all rfl⟩
  · with_unfolding_all rfl
  · with_unfolding_all rfl


private abbrev copySplitLanguage : CoreLanguage :=
  {twoFieldLanguage with
    operands := {twoFieldLanguage.operands with isCopy := fun _ => true}
    safety := fun _ => []}

private def copySplitGraph {T : Type} (ending : T) :
    CFG (Sigma (SSAValue Unit)) (LocatedOp (SSAInst testLowering)) T :=
  ⟨⟨0⟩, #[⟨⟨0⟩, [⟨(), ⟨0⟩⟩],
    [⟨testOrigin, .destructure (.register (ty := ()) ⟨0⟩)
      [(splitField, ⟨(), ⟨1⟩⟩), (secondSplitField, ⟨(), ⟨2⟩⟩)]⟩], ending, []⟩]⟩

private def copySplitSSA : SSA testLowering testProfile :=
  ⟨tabProject, Exec.Demo.mainF, [],
    [(Exec.Demo.mainF, [], copySplitGraph (.ret none))], [], [], []⟩

private def copySplitBIR : BIR testLowering testProfile :=
  ⟨tabProject, Exec.Demo.mainF, [],
    [(Exec.Demo.mainF, [], copySplitGraph (.ret none))], [], #[], [], []⟩

private def copySplitMIR : MIR testLowering testProfile :=
  let block : Block _ _ _ := ⟨⟨0⟩, [⟨(), ⟨storeRoot, []⟩⟩],
    [⟨testOrigin, .destructure (ty := ()) ⟨storeRoot, []⟩
      [(splitField, ⟨(), ⟨⟨1⟩, []⟩⟩), (secondSplitField, ⟨(), ⟨⟨2⟩, []⟩⟩)]⟩], .ret none, []⟩
  ⟨tabProject, Exec.Demo.mainF, [],
    [(Exec.Demo.mainF, [(storeRoot, ()), (⟨1⟩, ()), (⟨2⟩, ())], ⟨⟨0⟩, #[block]⟩)],
    [(Exec.Demo.mainF, [storeRoot, ⟨1⟩, ⟨2⟩])], []⟩

set_option maxRecDepth 4096 in
example :
    let memory := copySplitMIR.logicalView (language := copySplitLanguage)
    let before : MemoryRegisters copySplitLanguage testProfile :=
      {storeInitial with
        live := [storeRoot]
        initialized := [storeRoot]
        virtualValues := [(storeRoot, .other ())]}
    ∃ next, copySplitMIR.Step before next ∧ memory.Step before next ∧
      (SSA.Execution (language := copySplitLanguage)).step copySplitSSA before next ∧
      (BIR.Execution (language := copySplitLanguage)).step copySplitBIR before next ∧
      memory.readValue next (.copy (fieldRegister 0)) = some (.other (), next) ∧
      memory.readValue next (.copy (fieldRegister 1)) = some (.scalar (.int .u8 17), next) ∧
      memory.readValue next (.copy (fieldRegister 2)) = some (.scalar (.int .u8 25), next) := by
  intro memory before
  let first : MemoryRegisters copySplitLanguage testProfile :=
    {before with
      live := [⟨1⟩, storeRoot]
      initialized := [⟨1⟩, storeRoot]
      virtualValues := (⟨1⟩, .scalar (.int .u8 17)) :: before.virtualValues}
  let stored : MemoryRegisters copySplitLanguage testProfile :=
    {first with
      live := ⟨2⟩ :: first.live
      initialized := ⟨2⟩ :: first.initialized
      virtualValues := (⟨2⟩, .scalar (.int .u8 25)) :: first.virtualValues}
  let dests : List (FieldId × Sigma (MemoryPlace Unit)) :=
    [(splitField, ⟨(), fieldRegister 1⟩), (secondSplitField, ⟨(), fieldRegister 2⟩)]
  have tick : memory.InstructionStepWith before stored.advance false false
      (.destructure (fieldRegister 0) dests) := by
    refine ⟨.other (), before, [.scalar (.int .u8 17), .scalar (.int .u8 25)], stored,
      by with_unfolding_all rfl, ?_, by with_unfolding_all rfl, ?_, rfl⟩
    · exact ⟨splitShape.1, splitShape.2.choose, splitShape.2.choose_spec.1,
        splitShape.2.choose_spec.2.1, fun _ _ _ => ⟨(), rfl, rfl⟩⟩
    · apply Memory.WriteParams.cons (started := {before with live := first.live}) (stored := first)
      · with_unfolding_all exact ⟨nofun, Or.inl ⟨rfl, rfl⟩⟩
      · with_unfolding_all exact ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩
      · apply Memory.WriteParams.cons (started := {first with live := stored.live})
        · with_unfolding_all exact ⟨nofun, Or.inl ⟨rfl, rfl⟩⟩
        · with_unfolding_all exact ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩
        · exact .nil _
  with_unfolding_all
    exact ⟨stored.advance, ⟨rfl, tick⟩, ⟨_, rfl, tick⟩, ⟨rfl, ⟨_, rfl, tick⟩, [], rfl⟩,
      ⟨rfl, ⟨_, rfl, tick⟩, [], rfl⟩, rfl, rfl, rfl⟩

-- Each field register is consumed by its first move, even though the original
-- backing bytes remain allocated until the source root's lifetime ends.
set_option maxRecDepth 2048 in
example (project : Project) :
    let memory := splitMemory project
    let args : List (Sigma (MemoryRead Unit (Literal Unit))) :=
      [⟨(), .move (fieldRegister 1)⟩, ⟨(), .move (fieldRegister 2)⟩]
    let consumed : MemoryRegisters twoFieldLanguage testProfile :=
      {splitAfter with initialized := [], virtualValues := []}
    memory.readOperands (language := twoFieldLanguage) args splitAfter =
      some ([.scalar (.int .u8 17), .scalar (.int .u8 25)], consumed) ∧
    memory.readOperands (language := twoFieldLanguage) args consumed = none := by
  with_unfolding_all exact ⟨rfl, rfl⟩

-- A consuming split cannot omit a linear field.
set_option maxRecDepth 2048 in
example (project : Project) :
    ∀ next, ¬ (SSA.Execution (language := twoFieldLanguage)).step
      (splitSSA project true) splitBefore next := by
  intro next execution
  obtain ⟨_, ⟨block, fetch, tick⟩, _⟩ := execution
  let expected : MemoryBlock testLowering := ⟨⟨0⟩, [],
    [⟨testOrigin, .destructure storePlace [(splitField, ⟨(), fieldRegister 1⟩)]⟩], .ret none, []⟩
  have exactBlock : some expected = some block := by with_unfolding_all exact fetch
  obtain rfl := Option.some.inj exactBlock
  obtain ⟨_, _, _, _, _, shape, _, _, _⟩ := tick
  obtain ⟨_, fields, active, _, omitted⟩ := shape
  have same : some [splitField, secondSplitField] = some fields := active
  cases same
  obtain ⟨_, _, copy⟩ := omitted secondSplitField
    (List.mem_cons_of_mem _ (List.mem_cons_self ..)) (by decide)
  exact Bool.noConfusion copy

private def fullSplitBody (dropSecond : Bool := false) : List (LocatedOp (SSAInst testLowering)) :=
  let split : SSAInst testLowering := .destructure (.materialized (ty := ()) ⟨storeRoot, []⟩)
    (if dropSecond then splitRegisters.take 1 else splitRegisters)
  let sum : SSAInst testLowering := .op (opcode := .named ⟨"sumFields"⟩) (types := [(), ()]) (ty := ()) ⟨2⟩ ()
    (.cons (.value ⟨0⟩) (.cons (.value ⟨1⟩) .nil))
  [.beginLifetime storeRoot, .store (ty := ()) ⟨storeRoot, []⟩ (.literal ⟨[]⟩), split,
    .endLifetime storeRoot, sum].map (LocatedOp.mk testOrigin)

private def fullSplitReturn : Option (Sigma (SSARead Unit testLowering.literal)) :=
  some ⟨(), .value ⟨2⟩⟩

private def fullSplitBIR (project : Project) (dropSecond : Bool := false) : BIR testLowering testProfile :=
  let block : Block _ _ _ := ⟨⟨0⟩, [], fullSplitBody dropSecond, .ret fullSplitReturn, []⟩
  {(splitBIR project) with
    functions := [(Exec.Demo.mainF, [(storeRoot, ())], ⟨⟨0⟩, #[block]⟩)]}

private def fullSplitSSA (project : Project) : SSA testLowering testProfile :=
  let block : Block _ _ _ := ⟨⟨0⟩, [], fullSplitBody, .ret fullSplitReturn, []⟩
  {(splitSSA project) with
    functions := [(Exec.Demo.mainF, [(storeRoot, ())], ⟨⟨0⟩, #[block]⟩)]}

private def fullSplitMemory (project : Project) : Memory testLowering testProfile :=
  let memory := (fullSplitBIR project).memoryView (language := twoFieldLanguage)
  let body : List (MemoryInst testLowering) :=
    [.beginLifetime storeRoot, .assign storePlace (.literal ⟨[]⟩),
      .destructure storePlace [(splitField, ⟨(), fieldRegister 1⟩),
        (secondSplitField, ⟨(), fieldRegister 2⟩)],
      .endLifetime storeRoot, .beginLifetime ⟨3⟩,
      .op (opcode := .named ⟨"sumFields"⟩) (fieldRegister 3) ()
        (.cons (.move (fieldRegister 1)) (.cons (.move (fieldRegister 2)) .nil))]
  let block : MemoryBlock testLowering :=
    ⟨⟨0⟩, [], body.map (LocatedOp.mk testOrigin), .ret (some ⟨(), .move (fieldRegister 3)⟩), []⟩
  {memory with
    functions := [(Exec.Demo.mainF, [(storeRoot, ()), (⟨1⟩, ()), (⟨2⟩, ()), (⟨3⟩, ())],
      ⟨⟨0⟩, #[block]⟩)]}

-- Starting with an empty machine, BIR allocates and stores the aggregate,
-- transfers its two linear fields, frees the aggregate and consumes both
-- fields to return 42.
set_option maxRecDepth 4096 in
set_option maxHeartbeats 1600000 in
private theorem fullSplitReturns (project : Project) :
    (BIR.Execution (language := twoFieldLanguage)).observes (fullSplitBIR project)
      (.returned 42 ByteArray.empty) ∧
    (∃ start finish : MemoryRegisters twoFieldLanguage testProfile,
      (fullSplitMemory project).Initial start ∧
      Concrete.Contracts.Steps (fullSplitMemory project).Step start finish ∧
      (fullSplitMemory project).terminalObservation finish = some (.returned 42 ByteArray.empty)) := by
  let bir := fullSplitBIR project
  let exec := BIR.Execution (language := twoFieldLanguage) (lang := testLowering) (profile := testProfile)
  let start := BIR.entryState (language := twoFieldLanguage) bir
  let allocated : MemoryRegisters twoFieldLanguage testProfile := backingBefore ⟨0⟩ 1
  let bound : MemoryRegisters twoFieldLanguage testProfile := backingAfter ⟨0⟩ 1
  let split : MemoryRegisters twoFieldLanguage testProfile := {splitAfter with instruction := 3}
  let freed := (split.bytes.free ⟨0⟩).getD Mem.Memory.empty
  let released : MemoryRegisters twoFieldLanguage testProfile :=
    {split with
      instruction := 4
      bytes := freed
      live := [⟨2⟩, ⟨1⟩]
      materialized := []}
  let finished : MemoryRegisters twoFieldLanguage testProfile :=
    {released with
      instruction := 5
      live := [⟨3⟩, ⟨2⟩, ⟨1⟩]
      initialized := [⟨3⟩]
      virtualValues := [(⟨3⟩, .scalar (.int .u8 42))]}
  have allocate : exec.step bir start allocated := by
    with_unfolding_all
      refine ⟨rfl, ⟨_, rfl, {allocated with instruction := 0},
        ⟨nofun, Or.inr ⟨rfl, ⟨1, 1⟩, rfl, Or.inr ?_⟩⟩, rfl⟩, [], rfl⟩
      exact ⟨by decide, ⟨0⟩, 1, ⟨0⟩, allocated.bytes, rfl, rfl⟩
  have bind : exec.step bir allocated bound := by
    with_unfolding_all
      refine ⟨rfl, ⟨_, rfl, .other (), allocated, {bound with instruction := 1}, rfl, ?_, rfl⟩, [], rfl⟩
      refine ⟨⟨rfl, nofun⟩, fun _ => ⟨⟨1, 1⟩, rfl, ⟨nofun, nofun⟩⟩, rfl, rfl, ?_⟩
      intro _
      refine Or.inr ⟨backingRange ⟨0⟩ 1, [(0, [.init 42 none])], rfl, rfl,
        fun _ => ⟨0, [.init 42 none], List.mem_singleton_self _, nofun⟩, ?_⟩
      intro offset bytes member
      cases List.mem_singleton.mp member
      exact ⟨by decide, by simp, rfl⟩
  have moveFields : exec.step bir bound split := by
    let afterRead : MemoryRegisters twoFieldLanguage testProfile := {bound with initialized := []}
    let first : MemoryRegisters twoFieldLanguage testProfile := {afterRead with
      live := [⟨1⟩, storeRoot]
      initialized := [⟨1⟩]
      virtualValues := [(⟨1⟩, .scalar (.int .u8 17))]}
    with_unfolding_all
      refine ⟨rfl, ⟨_, rfl, .other (), afterRead,
        [.scalar (.int .u8 17), .scalar (.int .u8 25)], {split with instruction := 2},
        rfl, splitShape, rfl, ?_, rfl⟩, [], rfl⟩
    apply Memory.WriteParams.cons (started := {afterRead with live := [⟨1⟩, storeRoot]}) (stored := first)
    · with_unfolding_all exact ⟨nofun, Or.inl ⟨rfl, rfl⟩⟩
    · with_unfolding_all exact ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩
    · apply Memory.WriteParams.cons (started := {first with live := [⟨2⟩, ⟨1⟩, storeRoot]})
      · with_unfolding_all exact ⟨nofun, Or.inl ⟨rfl, rfl⟩⟩
      · with_unfolding_all exact ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩
      · exact .nil _
  have release : exec.step bir split released := by
    with_unfolding_all
      exact ⟨rfl, ⟨_, rfl, {released with instruction := 3},
        ⟨⟨by decide, rfl⟩,
          Or.inr ⟨rfl, Or.inr ⟨backingRange ⟨0⟩ 1, ⟨0⟩, freed, rfl, rfl, rfl, rfl⟩⟩⟩,
        rfl⟩, [], rfl⟩
  have add : exec.step bir released finished := by
    let read : MemoryRegisters twoFieldLanguage testProfile :=
      {released with initialized := [], virtualValues := []}
    let stored : MemoryRegisters twoFieldLanguage testProfile := {finished with instruction := 4}
    with_unfolding_all
      refine ⟨rfl, ⟨_, rfl, [.scalar (.int .u8 17), .scalar (.int .u8 25)], read,
        [], .scalar (.int .u8 42), [], Memory.operationStore read false, stored,
        rfl, rfl, rfl, ⟨rfl, rfl, rfl, rfl, rfl, rfl⟩, rfl, ?_, rfl⟩, [], rfl⟩
    apply Memory.WriteParams.cons (started := {read with live := [⟨3⟩, ⟨2⟩, ⟨1⟩]})
    · with_unfolding_all exact ⟨nofun, Or.inl ⟨rfl, rfl⟩⟩
    · with_unfolding_all exact ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩
    · exact .nil _
  constructor
  · exact ⟨finished, ⟨start, rfl, .cons allocate (.cons bind (.cons moveFields
      (.cons release (.cons add (.nil _)))))⟩, by with_unfolding_all rfl⟩
  · let memory := fullSplitMemory project
    let ready : MemoryRegisters twoFieldLanguage testProfile :=
      {released with instruction := 5, live := [⟨3⟩, ⟨2⟩, ⟨1⟩]}
    let result : MemoryRegisters twoFieldLanguage testProfile := {finished with instruction := 6}
    have starts : memory.Initial start := by
      with_unfolding_all
        exact ⟨rfl, rfl, ⟨_, rfl, rfl⟩, ⟨_, rfl, rfl⟩, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩
    have memoryAllocate : memory.Step start allocated := by
      obtain ⟨_, ⟨block, found, tick⟩, _⟩ := allocate
      have found' : some (((bir.memoryView (language := twoFieldLanguage)).blockAt start).getD splitBlock) =
          some block := by with_unfolding_all exact found
      have equal := Option.some.inj found'
      subst block
      exact ⟨_, by with_unfolding_all rfl, by with_unfolding_all exact tick⟩
    have memoryBind : memory.Step allocated bound := by
      with_unfolding_all
        refine ⟨_, rfl, .other (), allocated, {bound with instruction := 1}, rfl, ?_, rfl⟩
        refine ⟨⟨rfl, nofun⟩, fun _ => ⟨⟨1, 1⟩, rfl, ⟨nofun, nofun⟩⟩, rfl, rfl, ?_⟩
        intro _
        refine Or.inr ⟨backingRange ⟨0⟩ 1, [(0, [.init 42 none])], rfl, rfl,
          fun _ => ⟨0, [.init 42 none], List.mem_singleton_self _, nofun⟩, ?_⟩
        intro offset bytes member
        cases List.mem_singleton.mp member
        exact ⟨by decide, by simp, rfl⟩
    have memoryMoveFields : memory.Step bound split := by
      let afterRead : MemoryRegisters twoFieldLanguage testProfile := {bound with initialized := []}
      let first : MemoryRegisters twoFieldLanguage testProfile := {afterRead with
        live := [⟨1⟩, storeRoot]
        initialized := [⟨1⟩]
        virtualValues := [(⟨1⟩, .scalar (.int .u8 17))]}
      with_unfolding_all
        refine ⟨_, rfl, .other (), afterRead,
          [.scalar (.int .u8 17), .scalar (.int .u8 25)], {split with instruction := 2},
          rfl, splitShape, rfl, ?_, rfl⟩
      apply Memory.WriteParams.cons (started := {afterRead with live := [⟨1⟩, storeRoot]}) (stored := first)
      · with_unfolding_all exact ⟨nofun, Or.inl ⟨rfl, rfl⟩⟩
      · with_unfolding_all exact ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩
      · apply Memory.WriteParams.cons (started := {first with live := [⟨2⟩, ⟨1⟩, storeRoot]})
        · with_unfolding_all exact ⟨nofun, Or.inl ⟨rfl, rfl⟩⟩
        · with_unfolding_all exact ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩
        · exact .nil _
    have memoryRelease : memory.Step split released := by
      obtain ⟨_, ⟨block, found, tick⟩, _⟩ := release
      have found' : some (((bir.memoryView (language := twoFieldLanguage)).blockAt split).getD splitBlock) =
          some block := by with_unfolding_all exact found
      have equal := Option.some.inj found'
      subst block
      exact ⟨_, by with_unfolding_all rfl, by with_unfolding_all exact tick⟩
    have openResult : memory.Step released ready := by
      with_unfolding_all exact ⟨_, rfl, {ready with instruction := 4},
        ⟨nofun, Or.inl ⟨rfl, rfl⟩⟩, rfl⟩
    have sum : memory.Step ready result := by
      let read : MemoryRegisters twoFieldLanguage testProfile :=
        {ready with initialized := [], virtualValues := []}
      with_unfolding_all
        exact ⟨_, rfl, [.scalar (.int .u8 17), .scalar (.int .u8 25)], read,
          [], .scalar (.int .u8 42), [], Memory.operationStore read false,
          {result with instruction := 5}, rfl, rfl, rfl, ⟨rfl, rfl, rfl, rfl, rfl, rfl⟩, rfl,
          ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩, rfl⟩
    exact ⟨start, result, starts,
      .cons memoryAllocate (.cons memoryBind (.cons memoryMoveFields
        (.cons memoryRelease (.cons openResult (.cons sum (.nil _)))))),
      by with_unfolding_all rfl⟩

set_option maxRecDepth 2048 in
example (project : Project) :
    (SSA.Execution (language := twoFieldLanguage)).observes (fullSplitSSA project)
      (.returned 42 ByteArray.empty) := by
  with_unfolding_all exact (fullSplitReturns project).1

-- The reachable aggregate witness also satisfies the function-preservation
-- check. Removing either binding changes the decoded program.
example (project : Project) :
    BuildsBIR (language := twoFieldLanguage) (fullSplitSSA project) (fullSplitBIR project) := by
  refine ⟨rfl, ?_, ?_, ?_, by change 3 ≤ 3; decide, ⟨rfl, rfl, by with_unfolding_all rfl⟩,
    BIR.refinesOfViewEq (by with_unfolding_all rfl)⟩
  · intro fn member block included slot trap
    cases List.mem_singleton.mp member
    cases List.mem_singleton.mp included
    cases trap
  · intro fn member block included kind origin trap
    cases List.mem_singleton.mp member
    cases List.mem_singleton.mp included
    cases trap
  · with_unfolding_all exact ⟨fun _ h => h, fun _ h => h⟩

set_option maxRecDepth 2048 in
example (project : Project) :
    ¬ BuildsBIR (language := twoFieldLanguage) (fullSplitSSA project) (fullSplitBIR project true) := by
  intro builds
  have bindings := congrArg (fun program => do
    let graph ← program
    let block ← graph.blocks[0]?
    let operation ← block.body[2]?
    let .destructure _ fields := operation.instruction | none
    pure fields.length) builds.2.2.2.2.2.1.2.2
  have wrong : some 2 = some 1 := by with_unfolding_all exact bindings
  cases wrong

private def returnSSA (project : Project) : SSA testLowering testProfile :=
  let block : Block _ _ _ := ⟨⟨0⟩, [], [], .ret (some ⟨(), .literal ⟨[0]⟩⟩), []⟩
  ⟨project, Exec.Demo.mainF, [], [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[block]⟩)], [], [], []⟩

-- The substituted project leaves the reached behavior and all the old
-- handoff checks unchanged. The project equality rejects it on this edge.
set_option maxRecDepth 4096 in
example (original forged : Project) (different : forged ≠ original) :
    let ssa := returnSSA original
    let bir := birReturn forged false
    (bir.TrapSlotsWF ∧ bir.TrapsMatch ssa ∧ PreservesOrigins ssa.origins bir.origins ∧
      bir.frameSlots ≤ testProfile.maxBIRSlots ∧ BIR.EntryMatches (language := testLanguage) ssa bir ∧
      BIR.PreservesProgram (language := testLanguage) ssa bir) ∧
    ¬ BuildsBIR (language := testLanguage) ssa bir := by
  refine ⟨⟨?_, ?_, ?_, (by decide : 0 ≤ 3), ⟨rfl, rfl, by with_unfolding_all rfl⟩, ?_⟩, ?_⟩
  · intro fn member block included index trap
    cases List.mem_singleton.mp member
    cases List.mem_singleton.mp included
    cases trap
  · intro fn member block included kind origin trap
    cases List.mem_singleton.mp member
    cases List.mem_singleton.mp included
    cases trap
  · exact ⟨nofun, nofun⟩
  · refine stationaryRefines _ _ _ _ storeInitial storeInitial
      (.returned 0 ByteArray.empty) (.returned 0 ByteArray.empty)
      (fun _ => by with_unfolding_all rfl) (fun _ => by with_unfolding_all rfl) ?_ ?_ ?_ ?_ ?_
    all_goals first
      | (intro o; with_unfolding_all exact ⟨fun h => (Option.some.inj h).symm, fun h => congrArg some h.symm⟩)
      | with_unfolding_all rfl
  · exact fun built => different built.1

end StageInterfaceControls
