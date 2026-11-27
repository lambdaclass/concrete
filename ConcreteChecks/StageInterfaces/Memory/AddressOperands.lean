import ConcreteChecks.StageInterfaces.SSABIR

open Concrete
open Concrete.Interfaces
open ConcreteSpec Core Core.SourcePathSpan StageInterface StageGrammar

namespace StageInterfaceControls

private abbrev nonzeroAddressLanguage : CoreLanguage :=
  {erasureLanguage with
    operation := fun _ _ _ _ _ _ operands before result effects after =>
      (∃ address, operands = [address] ∧ address.root = shadowRoot) ∧
      result = .scalar (.int .u8 17) ∧ effects = [] ∧ after = before}

private def nonzeroAddressBIR (project : Project) : BIR testLowering testProfile :=
  let block := {birReturnBlock false with
    body := [⟨testOrigin, .op (opcode := .named ⟨"address"⟩) (ty := ()) ⟨0⟩ () .nil
      [⟨(), ⟨shadowRoot, []⟩⟩]⟩]}
  {(birReturn project false) with
    functions := [(Exec.Demo.mainF, [(shadowRoot, ())], ⟨⟨0⟩, #[block]⟩)]
    layouts := [(Exec.Demo.mainF, [(shadowRoot, ⟨1, 1⟩)])]}

private def addressAtOne : MemoryRegisters nonzeroAddressLanguage testProfile :=
  {splitBefore with
    live := [shadowRoot]
    initialized := [shadowRoot]
    materialized := [(shadowRoot, backingRange ⟨0⟩ 1)]}

-- The operation checks the exact spec root. Renumbering materialized root 1
-- to 2 invalidates this step, even though the backing bytes are unchanged.
set_option maxRecDepth 3072 in
set_option maxHeartbeats 1000000 in
example (project : Project) :
    let exec := BIR.Execution (language := nonzeroAddressLanguage)
    ∃ next, exec.step (nonzeroAddressBIR project) addressAtOne next ∧
      next.addressIds = [(shadowRoot, ⟨0⟩)] ∧
      next.virtualValues = [(⟨2⟩, .scalar (.int .u8 17))] := by
  let addr : MemoryPlace Unit () := ⟨⟨shadowRoot, []⟩, .materialized⟩
  let dst := fieldRegister 2
  let block : MemoryBlock testLowering := ⟨⟨0⟩, [],
    [⟨testOrigin, .op (opcode := .named ⟨"address"⟩) (ty := ()) dst () .nil [] [⟨(), addr⟩]⟩],
    .ret (some ⟨(), .literal ⟨[0]⟩⟩), []⟩
  let ready : MemoryRegisters nonzeroAddressLanguage testProfile := {addressAtOne with
    addressIds := [(shadowRoot, ⟨0⟩)]
    nextAddressId := 1}
  let started : MemoryRegisters nonzeroAddressLanguage testProfile := {ready with live := [⟨2⟩, shadowRoot]}
  let stored : MemoryRegisters nonzeroAddressLanguage testProfile := {started with
    initialized := [⟨2⟩, shadowRoot]
    virtualValues := [(⟨2⟩, .scalar (.int .u8 17))]}
  let next : MemoryRegisters nonzeroAddressLanguage testProfile := {stored with instruction := 1}
  let address : OperationAddress Unit := ⟨(), shadowRoot, [], some (backingRange ⟨0⟩ 1), ⟨0⟩, .bytes⟩
  refine ⟨next, ⟨by with_unfolding_all rfl, ⟨block, by with_unfolding_all rfl, ?_⟩,
    [], by with_unfolding_all rfl⟩, rfl, rfl⟩
  refine ⟨[], addressAtOne, [address], .scalar (.int .u8 17), [], Memory.operationStore ready false,
    stored, rfl, rfl, by with_unfolding_all rfl, ⟨⟨address, rfl, rfl⟩, rfl, rfl, rfl⟩, rfl, ?_, rfl⟩
  apply Memory.WriteParams.cons (started := started) (stored := stored)
  · with_unfolding_all exact ⟨nofun, Or.inl ⟨rfl, rfl⟩⟩
  · with_unfolding_all exact ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩
  · exact .nil _

-- An undeclared physical lifetime operand cannot start the synthetic storage
-- reserved for a later register definition.
private def unknownLifetimeBIR (project : Project) : BIR testLowering testProfile :=
  let block := {birReturnBlock false with
    body := [⟨testOrigin, .beginLifetime shadowRoot⟩,
      ⟨testOrigin, .op (opcode := .named ⟨"unused"⟩) (ty := ()) ⟨0⟩ () .nil⟩]}
  {(birReturn project false) with
    functions := [(Exec.Demo.mainF, [(storeRoot, ())], ⟨⟨0⟩, #[block]⟩)]}

example (project : Project) :
    let bir := unknownLifetimeBIR project
    let view := bir.memoryView (language := testLanguage)
    view.classAt (language := testLanguage) bir.entry shadowRoot = none ∧
    ∀ next, ¬ view.Begins (language := testLanguage) (BIR.entryState bir) shadowRoot next := by
  refine ⟨by with_unfolding_all rfl, ?_⟩
  intro next begins
  rcases begins.2 with virtual | materialized
  · have impossible : none = some StorageClass.virtual := by with_unfolding_all exact virtual.1
    cases impossible
  · have impossible : none = some StorageClass.materialized := by with_unfolding_all exact materialized.1
    cases impossible

end StageInterfaceControls
