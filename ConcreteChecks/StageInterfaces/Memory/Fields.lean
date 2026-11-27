import ConcreteSpec.Proofs.Memory
import ConcreteChecks.StageInterfaces.Frontend.Literals
import ConcreteChecks.StageInterfaces.Memory.Fixtures

open Concrete
open Concrete.Interfaces
open ConcreteSpec Core Core.SourcePathSpan StageInterface StageGrammar

namespace StageInterfaceControls

def secondSplitField : FieldId := {splitField with name := ⟨"second"⟩}

abbrev twoFieldLanguage : CoreLanguage :=
  {erasureLanguage with
    operands := {erasureLanguage.operands with isCopy := fun _ => false}
    safety := fun _ => []
    operation := fun _ opcode _ _ values _ addresses before result effects after =>
      opcode = .named ⟨"sumFields"⟩ ∧
      values = [.scalar (.int .u8 17), .scalar (.int .u8 25)] ∧ addresses = [] ∧
      result = .scalar (.int .u8 42) ∧ effects = [] ∧ after = before
    fieldType := fun _ _ => some ()
    activeFields := fun _ _ _ => some [splitField, secondSplitField]
    field := fun _ path =>
      if path = [splitField] then some (.scalar (.int .u8 17))
      else if path = [secondSplitField] then some (.scalar (.int .u8 25)) else none}

private def partialRead (path : List FieldId) : Read Unit testLowering.literal () :=
  .move ⟨storeRoot, path⟩

private def partialMIR (sibling : List FieldId) : MIR testLowering testProfile :=
  let body : List (MIRInst testLowering) :=
    [.bind (ty := ()) ⟨storeRoot, []⟩ (.literal ⟨[]⟩),
      .bind ⟨⟨1⟩, []⟩ (partialRead [splitField]),
      .bind ⟨⟨2⟩, []⟩ (partialRead sibling),
      .endLifetime storeRoot,
      .op (opcode := .named ⟨"sumFields"⟩) (types := [(), ()]) (ty := ()) ⟨⟨3⟩, []⟩ ()
        (.cons (.move ⟨⟨1⟩, []⟩) (.cons (.move ⟨⟨2⟩, []⟩) .nil)) []]
  ⟨tabProject, Exec.Demo.mainF, [(Exec.Demo.mainF, ⟨[], some ()⟩)],
    [(Exec.Demo.mainF, [(storeRoot, ()), (⟨1⟩, ()), (⟨2⟩, ()), (⟨3⟩, ())],
      ⟨⟨0⟩, #[⟨⟨0⟩, [], body.map (LocatedOp.mk testOrigin),
        .ret (some ⟨(), .move ⟨⟨3⟩, []⟩⟩), []⟩]⟩)], [], []⟩

private def firstMoved : MemoryRegisters twoFieldLanguage testProfile :=
  {erasureBound with
    instruction := 2
    live := [⟨1⟩, storeRoot]
    initialized := [⟨1⟩, storeRoot]
    movedFields := [(storeRoot, [splitField])]
    virtualValues := [(⟨1⟩, .scalar (.int .u8 17)), (storeRoot, .other ())]}

private def bothMoved : MemoryRegisters twoFieldLanguage testProfile :=
  {firstMoved with
    instruction := 3
    live := ⟨2⟩ :: firstMoved.live
    initialized := ⟨2⟩ :: firstMoved.initialized
    movedFields := (storeRoot, [secondSplitField]) :: firstMoved.movedFields
    virtualValues := (⟨2⟩, .scalar (.int .u8 25)) :: firstMoved.virtualValues}

-- Starting at entry, each linear field moves once. The remaining sibling is
-- usable, and the aggregate can end only when both linear fields are gone.
set_option maxRecDepth 4096 in
set_option maxHeartbeats 1000000 in
example :
    let program := partialMIR [secondSplitField]
    (program.logicalView (language := twoFieldLanguage)).Initial storeInitial ∧
    Concrete.Contracts.Steps (program.Step (language := twoFieldLanguage)) storeInitial bothMoved ∧
    (program.logicalView (language := twoFieldLanguage)).canEnd firstMoved storeRoot = false ∧
    (program.logicalView (language := twoFieldLanguage)).canEnd bothMoved storeRoot = true ∧
    (∀ next, ¬ (partialMIR [splitField]).Step (language := twoFieldLanguage) firstMoved next) ∧
    (∀ next, ¬ (partialMIR []).Step (language := twoFieldLanguage) firstMoved next) := by
  refine ⟨?_, ?_, rfl, rfl, ?_, ?_⟩
  · with_unfolding_all
      exact ⟨rfl, rfl, ⟨_, rfl, rfl⟩, ⟨_, rfl, rfl⟩,
        rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩
  · refine .cons (b := erasureBound) ?_ (.cons (b := firstMoved) ?_ (.cons ?_ (.nil _)))
    · with_unfolding_all
        refine ⟨rfl, .other (), storeInitial, {erasureBound with instruction := 0}, rfl, ?_, rfl⟩
      rw [ite_eq_left (show (true || false) = true from rfl)]
      with_unfolding_all apply Memory.WriteParams.cons (language := twoFieldLanguage) (started := {storeInitial with live := [storeRoot]})
      · with_unfolding_all exact ⟨nofun, Or.inl ⟨rfl, rfl⟩⟩
      · with_unfolding_all exact ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩
      · exact .nil _
    · let afterRead : MemoryRegisters twoFieldLanguage testProfile :=
        {erasureBound with movedFields := firstMoved.movedFields}
      with_unfolding_all
        refine ⟨rfl, .scalar (.int .u8 17), afterRead, {firstMoved with instruction := 1}, rfl, ?_, rfl⟩
      rw [ite_eq_left (show (true || false) = true from rfl)]
      with_unfolding_all apply Memory.WriteParams.cons (language := twoFieldLanguage) (started := {afterRead with live := firstMoved.live})
      · with_unfolding_all exact ⟨nofun, Or.inl ⟨rfl, rfl⟩⟩
      · with_unfolding_all exact ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩
      · exact .nil _
    · let afterRead : MemoryRegisters twoFieldLanguage testProfile :=
        {firstMoved with movedFields := bothMoved.movedFields}
      with_unfolding_all
        refine ⟨rfl, .scalar (.int .u8 25), afterRead, {bothMoved with instruction := 2}, rfl, ?_, rfl⟩
      rw [ite_eq_left (show (true || false) = true from rfl)]
      with_unfolding_all apply Memory.WriteParams.cons (language := twoFieldLanguage) (started := {afterRead with live := bothMoved.live})
      · with_unfolding_all exact ⟨nofun, Or.inl ⟨rfl, rfl⟩⟩
      · with_unfolding_all exact ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩
      · exact .nil _
  · intro next tick
    obtain ⟨_, _, _, _, read, _, _⟩ := tick
    with_unfolding_all contradiction
  · intro next tick
    obtain ⟨_, _, _, _, read, _, _⟩ := tick
    with_unfolding_all contradiction

private abbrev loanFieldLanguage : CoreLanguage :=
  {earlyFixtureLanguage with
    fieldType := fun _ field =>
      if field = splitField then some (.inr false)
      else if field = secondSplitField then some (.inl (.scalar (.int .u8))) else none
    activeFields := fun _ _ _ => some [splitField, secondSplitField]
    replaceField := fun _ path value => if path = [secondSplitField] then some value else none
    field := fun _ path =>
      if path = [splitField] then some (.scalar (.int .u8 17))
      else if path = [secondSplitField] then some (.scalar (.int .u8 25)) else none}

private def loanLowering : LoweringLanguage loanFieldLanguage.operands.Ty :=
  ⟨fun _ _ _ => Unit, .inr false⟩

private def loanMemory : Memory loanLowering testProfile :=
  ⟨tabProject, Exec.Demo.mainF, [],
    [(Exec.Demo.mainF, [(storeRoot, .inr false)], ⟨⟨0⟩, #[]⟩)],
    [], [(Exec.Demo.mainF, [(storeRoot, .virtual)])], [], []⟩

private def loanRoot : Hybrid.Place := ⟨0⟩

private def moveBeforeBorrow : MemoryRegisters loanFieldLanguage testProfile :=
  { frame := Exec.Demo.mainF
    block := ⟨0⟩
    instruction := 0
    live := [storeRoot]
    initialized := [storeRoot]
    virtualValues := [(storeRoot, .scalar (.int .u8 42))]
    materialized := []
    bytes := Mem.Memory.empty
    addressIds := [(storeRoot, loanRoot)]
    nextAddressId := 1
    ownership := {
      slots := [⟨loanRoot, 0, ⟨.int .u8, false, true⟩, .owned⟩]
      frames := [⟨0, [], []⟩]
      nextPlace := 1
      nextActivation := 1 } }

private def liveFieldLoan (mode : Own.Mode := .shared) :
    Option (MemoryRegisters loanFieldLanguage testProfile × MemoryRegisters loanFieldLanguage testProfile) := do
  let (_, moved) ← loanMemory.readValue moveBeforeBorrow
    (.move (ty := .inr false) ⟨⟨storeRoot, [splitField]⟩, .virtual⟩)
  let address ← loanMemory.resolveAddress moved true
    ⟨.inl (.scalar (.int .u8)), ⟨⟨storeRoot, [secondSplitField]⟩, .virtual⟩⟩
  let borrowed ← Own.borrow moved.ownership address.provenance
    (address.fields.map Own.Projection.field) mode
  let released ← Own.endLoan borrowed ⟨0⟩
  return ({moved with ownership := borrowed}, {moved with ownership := released})

-- The only linear field moves out. A loan of the surviving Copy field
-- keeps the root alive, and releasing that loan permits the same end.
set_option maxRecDepth 4096 in
example : ∃ borrowed released, liveFieldLoan = some (borrowed, released) ∧
    (∀ next, ¬ loanMemory.Ends borrowed storeRoot next) ∧
    ∃ next, loanMemory.Ends released storeRoot next := by
  refine ⟨_, _, rfl, ?_, ?_⟩
  · intro next ended
    have h := ended.1.2
    change false = true at h
    contradiction
  · exact ⟨_, ⟨List.mem_cons_self .., rfl⟩, Or.inl ⟨rfl, rfl⟩⟩

-- Both modes exclude owner writes. Only a mutable loan excludes owner copies.
private def ownerFieldAccess (mode : Own.Mode) : Option (Bool × Bool × Bool × Bool) := do
  let (borrowed, released) ← liveFieldLoan mode
  let place : MemoryPlace loanFieldLanguage.operands.Ty (.inl (.scalar (.int .u8))) :=
    ⟨⟨storeRoot, [secondSplitField]⟩, .virtual⟩
  let write state := (loanMemory.writeValue state place (.scalar (.int .u8 25))).isSome
  let copy state := (loanMemory.readValue state (.copy place)).isSome
  return (write borrowed, copy borrowed, write released, copy released)

set_option maxRecDepth 4096 in
example : ownerFieldAccess .shared = some (false, true, true, true) ∧
    ownerFieldAccess .mutable = some (false, false, true, true) :=
  ⟨by with_unfolding_all rfl, by with_unfolding_all rfl⟩

private def partialOwnerOverwrite (mutable : Bool) : Option (Bool × Bool × Bool × Bool) := do
  let initial := {moveBeforeBorrow with ownership := {moveBeforeBorrow.ownership with
    slots := [⟨loanRoot, 0, ⟨.int .u8, false, mutable⟩, .owned⟩]}}
  let (_, moved) ← loanMemory.readValue initial
    (.move (ty := .inr false) ⟨⟨storeRoot, [splitField]⟩, .virtual⟩)
  let field : MemoryPlace loanFieldLanguage.operands.Ty (.inl (.scalar (.int .u8))) :=
    ⟨⟨storeRoot, [secondSplitField]⟩, .virtual⟩
  let root : MemoryPlace loanFieldLanguage.operands.Ty (.inr false) :=
    ⟨⟨storeRoot, []⟩, .virtual⟩
  let surviving := (loanMemory.readValue moved (.copy field)).isSome
  let direct := (loanMemory.writeValue moved field (.scalar (.int .u8 42))).isSome
  let whole := (loanMemory.writeValue moved root (.scalar (.int .u8 42))).isSome
  let realOwner := Own.ownerAccess moved.ownership loanRoot .mutable
  return (surviving, direct, whole, realOwner)

set_option maxRecDepth 4096 in
example : partialOwnerOverwrite false = some (true, false, false, false) ∧
    partialOwnerOverwrite true = some (true, true, true, true) :=
  ⟨by with_unfolding_all rfl, by with_unfolding_all rfl⟩

-- All moved descendants are cleared on replacement. A surviving linear
-- sibling still prevents an overwrite.
example :
    let memory := (partialMIR [secondSplitField]).logicalView (language := twoFieldLanguage)
    let root : MemoryPlace Unit () := ⟨⟨storeRoot, []⟩, .virtual⟩
    memory.writeValue firstMoved root (.other ()) = none ∧
    (memory.writeValue bothMoved root (.other ())).map (·.movedFields) = some [] :=
  ⟨rfl, rfl⟩

private def enumDecl : DeclId := ⟨Exec.Demo.mainF.module, ⟨"choice"⟩⟩
private def enumMoved : FieldId := ⟨.variant ⟨enumDecl, ⟨"left"⟩⟩, ⟨"linear"⟩⟩
private def enumSurviving : FieldId := ⟨.variant ⟨enumDecl, ⟨"left"⟩⟩, ⟨"copy"⟩⟩
private def enumInactive : FieldId := ⟨.variant ⟨enumDecl, ⟨"right"⟩⟩, ⟨"other"⟩⟩

private abbrev enumFieldLanguage : CoreLanguage :=
  {loanFieldLanguage with
    fieldType := fun _ field =>
      if field = enumMoved then some (.inr false)
      else if field == enumSurviving || field == enumInactive then some (.inl (.scalar (.int .u8))) else none
    activeFields := fun _ _ _ => some [enumMoved, enumSurviving]
    field := fun value path =>
      if path = [enumMoved] then some (.scalar (.int .u8 17))
      else if path = [enumSurviving] then some value else none}

private def enumState (n : Nat) : MemoryRegisters enumFieldLanguage testProfile :=
  {moveBeforeBorrow with
    virtualValues := [(storeRoot, .scalar (.int .u8 n))]
    movedFields := [(storeRoot, [enumMoved])]}

private def enumLogical : LogicalFrame enumFieldLanguage.operands.Val :=
  ⟨Exec.Demo.mainF, [storeRoot], (enumState 25).virtualValues, (enumState 25).movedFields⟩

-- The old range of the representation obligation included this initialized
-- type-valid path, although the selected variant cannot supply a value there.
example : (enumFieldLanguage.fieldType (.inr false) enumInactive).isSome = true ∧
    Memory.initializedAt (enumState 25) storeRoot [enumInactive] = true ∧
    enumFieldLanguage.field (.scalar (.int .u8 25)) [enumInactive] = none :=
  ⟨rfl, rfl, rfl⟩

-- The alternate variant's field has a type but no value. The active surviving
-- field still has to agree with the logical value.
set_option maxRecDepth 4096 in
example : loanMemory.Represents (language := enumFieldLanguage) enumLogical (enumState 25) ∧
    ¬ loanMemory.Represents (language := enumFieldLanguage) enumLogical (enumState 24) := by
  constructor
  · refine ⟨⟨⟨Mem.empty_wellFormed _, ?_, ?_⟩, rfl⟩, rfl, fun _ h => h,
      ?_, ?_, ?_, ?_, nofun, ?_, ?_, ?_, nofun, ?_⟩
    · change [storeRoot].Nodup
      simp
    · change [storeRoot].Nodup ∧ ∀ place ∈ [storeRoot], place ∈ [storeRoot]
      exact ⟨by simp, fun _ h => h⟩
    · change [storeRoot].Nodup
      simp
    · exact List.nodup_nil
    · change [storeRoot].Nodup
      simp
    · exact List.nodup_nil
    · intro place
      change place ∈ [storeRoot] ↔
        (AList.lookup ([(storeRoot, CoreValue.scalar (.int .u8 25))] :
          AList PlaceId (CoreValue DeclId)) place).isSome = true
      by_cases eq : place = storeRoot
      · subst place
        exact ⟨fun _ => rfl, fun _ => List.mem_cons_self ..⟩
      · simp [AList.lookup, eq, Ne.symm eq]
    · intro place value member
      have eq : place = storeRoot := congrArg Prod.fst (List.mem_singleton.mp member)
      cases eq
      exact List.mem_cons_self ..
    · intro place value member
      have eq : place = storeRoot := congrArg Prod.fst (List.mem_singleton.mp member)
      cases eq
      exact ⟨List.mem_cons_self .., rfl⟩
    · intro place member
      obtain rfl := List.mem_singleton.mp member
      refine ⟨_, _, .inr false, .virtual, List.mem_cons_self .., rfl, rfl, nofun, ?_⟩
      intro value found
      have eq : CoreValue.scalar (.int .u8 25) = value := Option.some.inj found
      cases eq
      intro path ty field live typed active
      change (if path = [enumMoved] then some (.scalar (.int .u8 17))
        else if path = [enumSurviving] then some (.scalar (.int .u8 25)) else none) =
        some field at active
      split at active
      · rename_i moved
        subst path
        have impossible : false = true := live
        cases impossible
      · split at active
        · rename_i surviving
          subst path
          have same : some (.inl (.scalar (.int .u8))) = some ty := typed
          obtain rfl := Option.some.inj same
          obtain rfl := Option.some.inj active
          exact ⟨_, rfl, rfl, nofun⟩
        · contradiction
  · intro h
    have fields := h.2.2.2.2.2.2.2.2.2.2.2.2 storeRoot (List.mem_cons_self ..)
    obtain ⟨locals, graph, ty, storage, mem, lookup, storageClass, _, related⟩ := fields
    obtain eq := List.mem_singleton.mp mem
    cases eq
    obtain rfl := Option.some.inj lookup
    obtain rfl := Option.some.inj storageClass
    obtain ⟨physical, rfl, loaded, _⟩ := related (.scalar (.int .u8 25)) rfl
      [enumSurviving] (.inl (.scalar (.int .u8))) (.scalar (.int .u8 25)) rfl rfl rfl
    cases loaded

end StageInterfaceControls
