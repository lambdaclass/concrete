import ConcreteChecks.StageInterfaces.Frontend.Literals
import ConcreteChecks.StageInterfaces.Memory.Fixtures

open Concrete
open Concrete.Interfaces
open ConcreteSpec Core Core.SourcePathSpan StageInterface StageGrammar

namespace StageInterfaceControls

private abbrev ownerFixtureLowering := sliceLoweringLanguage slice0
private def ownerFixtureType : earlyFixtureLanguage.operands.Ty := .scalar (.int .u8)

private def ownerFixtureMIR (consuming mutable : Bool) : MIR ownerFixtureLowering testProfile :=
  let place : Place _ ownerFixtureType := ⟨storeRoot, []⟩
  let read : Read _ ownerFixtureLowering.literal ownerFixtureType :=
    if consuming then .move place else .copy place
  let block : Block _ _ _ := ⟨⟨0⟩, [],
    [⟨testOrigin, .bind place (.literal ⟨[42]⟩)⟩], .ret (some ⟨ownerFixtureType, read⟩), []⟩
  ⟨tabProject, earlyEntry, [(earlyEntry, ⟨[], some ownerFixtureType⟩)],
    [(earlyEntry, [(storeRoot, ownerFixtureType)], ⟨⟨0⟩, #[block]⟩)],
    if mutable then [(earlyEntry, [storeRoot])] else [], []⟩

def ownerFixtureStart : MemoryRegisters earlyFixtureLanguage testProfile :=
  {frame := earlyEntry, block := ⟨0⟩, instruction := 0, virtualValues := [],
    materialized := [], live := [], initialized := [], bytes := Mem.Memory.empty}

private def ownerFixtureBound : MemoryRegisters earlyFixtureLanguage testProfile :=
  {ownerFixtureStart with
    instruction := 1
    live := [storeRoot]
    initialized := [storeRoot]
    virtualValues := [(storeRoot, .scalar (.int .u8 42))]}

set_option maxRecDepth 4096 in
example :
    (ownerFixtureMIR true false).Step ownerFixtureStart ownerFixtureBound ∧
    (ownerFixtureMIR false false).Step ownerFixtureStart ownerFixtureBound ∧
    (ownerFixtureMIR true false).terminalObservation ownerFixtureBound = none ∧
    (ownerFixtureMIR false false).terminalObservation ownerFixtureBound = some (.returned 42 ByteArray.empty) := by
  have binds (consuming : Bool) : (ownerFixtureMIR consuming false).Step ownerFixtureStart ownerFixtureBound := by
    with_unfolding_all
      refine ⟨rfl, _, ownerFixtureStart, {ownerFixtureBound with instruction := 0}, rfl, ?_, rfl⟩
      apply Memory.WriteParams.cons (started := {ownerFixtureStart with live := [storeRoot]})
      · exact ⟨nofun, Or.inl ⟨rfl, rfl⟩⟩
      · exact ⟨⟨rfl, fun _ => rfl⟩, nofun, rfl, rfl, nofun⟩
      · exact .nil _
  exact ⟨binds true, binds false, by with_unfolding_all rfl, by with_unfolding_all rfl⟩

set_option maxRecDepth 4096 in
example :
    let place : MemoryPlace _ ownerFixtureType := ⟨⟨storeRoot, []⟩, .virtual⟩
    let inst : MemoryInst ownerFixtureLowering := .assign place (.literal ⟨[25]⟩)
    let before := ownerFixtureBound
    let immutable := (ownerFixtureMIR false false).logicalView
    let mutable := (ownerFixtureMIR false true).logicalView
    (∀ next, ¬ immutable.InstructionStep before next false inst) ∧
      ∃ next, mutable.InstructionStep before next false inst ∧
        next.virtualValues = [(storeRoot, .scalar (.int .u8 25))] := by
  dsimp only
  constructor
  · rintro next ⟨value, after, stored, read, write, _⟩
    have result : some (CoreValue.scalar (.int .u8 25), ownerFixtureBound) = some (value, after) := by
      with_unfolding_all exact read
    cases Option.some.inj result
    have impossible : none = some stored := by with_unfolding_all exact write.2.2.1
    cases impossible
  · with_unfolding_all
      exact ⟨_, ⟨_, _, _, rfl, ⟨⟨rfl, fun _ => rfl⟩, nofun, rfl, rfl, nofun⟩, rfl⟩, rfl⟩

-- Check the same decisions with an explicitly instrumented owner and no loans.
set_option maxRecDepth 4096 in
example :
    let memory := (ownerFixtureMIR false false).logicalView
    let place : MemoryPlace _ ownerFixtureType := ⟨⟨storeRoot, []⟩, .virtual⟩
    let bound (mutable : Bool) : MemoryRegisters earlyFixtureLanguage testProfile :=
      {ownerFixtureBound with
        addressIds := [(storeRoot, ⟨0⟩)]
        nextAddressId := 1
        ownership :=
          {slots := [⟨⟨0⟩, 0, ⟨.int .u8, true, mutable⟩, .owned⟩]
           frames := [⟨0, [(⟨0⟩, ⟨0⟩)], []⟩]
           nextPlace := 1
           nextActivation := 1}}
    memory.readValue (bound false) (.move place) = none ∧
      (memory.readValue (bound false) (.copy place)).isSome = true ∧
      memory.writeValue (bound false) place (.scalar (.int .u8 25)) = none ∧
      (memory.writeValue (bound true) place (.scalar (.int .u8 25))).isSome = true := by
  with_unfolding_all exact ⟨rfl, rfl, rfl, rfl⟩

private def retainedOwnerCore : CoreInput earlyFixtureLanguage :=
  ⟨elaborationSource.project, early,  .straightLine (.zero
    ⟨[⟨⟨⟨storeRoot.id⟩, .atom (.lit 42)⟩, resultSpan, resultSpan, (), .unnamed none⟩], .var ⟨storeRoot.id⟩, resultSpan, resultSpan⟩) rfl⟩

private def retainedOwnerMIR (substituted : Bool) : MIR ownerFixtureLowering testProfile :=
  let dst : Place _ ownerFixtureType := ⟨storeRoot, []⟩
  let origin (site : Nat) := (AList.lookup retainedOwnerCore.machineProgram.origins ⟨site⟩).getD testOrigin
  let binding : LocatedOp (MIRInst ownerFixtureLowering) := ⟨origin 0, .bind dst (.literal ⟨[42]⟩)⟩
  let assignment : LocatedOp (MIRInst ownerFixtureLowering) := ⟨origin 0, .assign dst (.literal ⟨[42]⟩)⟩
  let block : Block _ _ _ := ⟨⟨0⟩, [], binding :: (if substituted then [assignment] else []),
    .ret (some ⟨ownerFixtureType, .copy dst⟩), [origin 1]⟩
  ⟨retainedOwnerCore.project, earlyEntry, [(earlyEntry, ⟨[], some ownerFixtureType⟩)],
    [(earlyEntry, [(storeRoot, ownerFixtureType)], ⟨⟨0⟩, #[block]⟩)],
    if substituted then [(earlyEntry, [storeRoot])] else [], []⟩

set_option maxRecDepth 4096 in
private theorem retainedOwnerAccepted :
    PreservesOwnerMutability retainedOwnerCore (retainedOwnerMIR false) := by
  refine ⟨?_, nofun⟩
  intro fn locals graph member root ty localMember
  obtain ⟨fnEq, contents⟩ := Prod.mk.inj (List.mem_singleton.mp member)
  have localsEq : locals = [(storeRoot, ownerFixtureType)] := congrArg Prod.fst contents
  subst locals
  have rootEq : root = storeRoot := congrArg Prod.fst (List.mem_singleton.mp localMember)
  rw [fnEq, rootEq]
  with_unfolding_all rfl

set_option maxRecDepth 4096 in
private theorem substitutedOwnerRefused :
    ¬ PreservesOwnerMutability retainedOwnerCore (retainedOwnerMIR true) := by
  intro h
  have incompatible := h.1 earlyEntry _ _ (List.mem_singleton_self _) storeRoot ownerFixtureType
    (List.mem_singleton_self _)
  with_unfolding_all change true = false at incompatible
  contradiction

set_option maxRecDepth 4096 in
private theorem substitutedOriginsPreserved :
    PreservesOrigins (List.map Prod.snd retainedOwnerCore.machineProgram.origins) (retainedOwnerMIR true).origins := by
  let bound := (AList.lookup retainedOwnerCore.machineProgram.origins ⟨0⟩).getD testOrigin
  let returned := (AList.lookup retainedOwnerCore.machineProgram.origins ⟨1⟩).getD testOrigin
  with_unfolding_all
    change PreservesOrigins [bound, returned] [bound, bound, returned]
  constructor
  · intro o member
    simp only [List.mem_cons, List.not_mem_nil, or_false] at member ⊢
    exact member.elim Or.inl id
  · intro o member
    simp only [List.mem_cons, List.not_mem_nil, or_false] at member ⊢
    exact member.elim Or.inl (fun h => Or.inr (Or.inr h))

set_option maxRecDepth 4096 in
private theorem substitutedOwnerAssigns :
    (retainedOwnerMIR true).Step ownerFixtureBound ownerFixtureBound.advance := by
  with_unfolding_all
    exact ⟨rfl, _, _, _, rfl, ⟨⟨rfl, fun _ => rfl⟩, nofun, rfl, rfl, nofun⟩, rfl⟩

set_option maxRecDepth 4096 in
private theorem ownerFixtureObservations (substituted : Bool) :
    let mir := retainedOwnerMIR substituted
    let done := if substituted then ownerFixtureBound.advance else ownerFixtureBound
    mir.terminalObservation done = some (.returned 42 ByteArray.empty) ∧
      (MIR.Execution (language := earlyFixtureLanguage)).trace mir done = [] := by
  cases substituted <;> with_unfolding_all exact ⟨rfl, rfl⟩

example :
    PreservesOwnerMutability retainedOwnerCore (retainedOwnerMIR false) ∧
    PreservesOrigins (List.map Prod.snd retainedOwnerCore.machineProgram.origins) (retainedOwnerMIR true).origins ∧
    (retainedOwnerMIR true).Step ownerFixtureBound ownerFixtureBound.advance ∧
    (retainedOwnerMIR true).terminalObservation ownerFixtureBound.advance =
      (retainedOwnerMIR false).terminalObservation ownerFixtureBound ∧
    (MIR.Execution (language := earlyFixtureLanguage)).trace
        (retainedOwnerMIR true) ownerFixtureBound.advance =
      (MIR.Execution (language := earlyFixtureLanguage)).trace
        (retainedOwnerMIR false) ownerFixtureBound ∧
    ¬ LowersUnchecked retainedOwnerCore (retainedOwnerMIR true) ∧
    ¬ LowersCore ⟨retainedOwnerCore, []⟩ (retainedOwnerMIR true) :=
  ⟨retainedOwnerAccepted, substitutedOriginsPreserved, substitutedOwnerAssigns,
    (ownerFixtureObservations true).1.trans (ownerFixtureObservations false).1.symm,
    (ownerFixtureObservations true).2.trans (ownerFixtureObservations false).2.symm,
    fun ⟨_, lowered⟩ => substitutedOwnerRefused lowered.2.2.2.2.1.1, fun h => substitutedOwnerRefused h.2.2.2.2.1.1⟩

end StageInterfaceControls
