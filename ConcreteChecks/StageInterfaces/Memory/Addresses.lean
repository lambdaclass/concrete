import ConcreteChecks.StageInterfaces.Memory.Fixtures

open Concrete
open Concrete.Interfaces
open ConcreteSpec Core Core.SourcePathSpan StageInterface StageGrammar

namespace StageInterfaceControls

def abstractStore : OperationAddress Unit :=
  ⟨(), storeRoot, [], none, ⟨0⟩, .logical⟩

def emptyStoreAddress : OperationAddress Unit :=
  ⟨(), storeRoot, [], none, ⟨0⟩, .zero⟩

abbrev addressLanguage : CoreLanguage :=
  {testLanguage with
    operation := fun _ opcode _ _ args _ operands before result effects after =>
      match opcode with
      | .named operation =>
        if operation = ⟨"address"⟩ then
          ((∃ (range : Hybrid.Range), operands = [resolvedStore range] ∧
              result = CoreValue.scalar (.int .u8 range.ptr.addr)) ∨
            ((operands = [abstractStore] ∨ operands = [emptyStoreAddress]) ∧ result = CoreValue.other ())) ∧
          effects = [] ∧ after = before
        else
          operands = [] ∧ result = storeValue ∧ effects = [] ∧
          ((args = [CoreValue.other ()] ∧
            AList.lookup before.addressedValues ⟨0⟩ = some storeValue ∧
            after = {before with
              addressedValues := AList.update before.addressedValues ⟨0⟩ (fun _ => CoreValue.scalar (.int .u8 99))}) ∨
           ∃ ptr bytes, args = [CoreValue.scalar (.int .u8 ptr.addr)] ∧
             before.bytes.load testProfile.memoryTarget ptr 1 1 = some [.init 42 none] ∧
             before.bytes.store testProfile.memoryTarget ptr [.init 99 none] 1 = some bytes ∧
             after = {before with bytes := bytes})
      | _ => False
    representsValue := fun _ ranges _ logical physical =>
      match logical with
      | .scalar _ => logical = physical
      | .other () =>
        match AList.lookup ranges ⟨0⟩ with
        | some (some range) => physical = CoreValue.scalar (.int .u8 range.ptr.addr)
        | some none => physical = logical
        | none => False}

def addressResultPlace : MemoryPlace Unit () :=
  ⟨⟨shadowRoot, []⟩, .virtual⟩

def addressInstruction : MemoryInst testLowering :=
  .op (opcode := .named ⟨"address"⟩) (types := []) (ty := ()) addressResultPlace () .nil []
    [⟨(), storePlace⟩]

def indirectInstruction : MemoryInst testLowering :=
  .op (opcode := .named ⟨"indirectReadWrite"⟩) (types := [()]) addressResultPlace ()
    (.cons (.copy addressResultPlace) .nil)

def addressBlock : MemoryBlock testLowering :=
  ⟨⟨0⟩, [],
    [⟨testOrigin, .beginLifetime storeRoot⟩, ⟨testOrigin, .bind storePlace (.literal ⟨[42]⟩)⟩,
      ⟨testOrigin, .beginLifetime shadowRoot⟩, ⟨testOrigin, addressInstruction⟩,
      ⟨testOrigin, indirectInstruction⟩], .ret (some ⟨(), .copy addressResultPlace⟩), []⟩

def addressMemory (project : Project) : Memory testLowering testProfile :=
  {(storingMemory project) with
    mutableLocals := [(Exec.Demo.mainF, [storeRoot, shadowRoot])]
    signatures := [(Exec.Demo.mainF, ⟨[], some ()⟩)]
    functions := [(Exec.Demo.mainF, [(storeRoot, ()), (shadowRoot, ())], ⟨⟨0⟩, #[addressBlock]⟩)]
    storage := [(Exec.Demo.mainF, [(storeRoot, .materialized), (shadowRoot, .virtual)])]}

def addressReady (base : MemoryRegisters addressLanguage testProfile) :
    MemoryRegisters addressLanguage testProfile :=
  {base with instruction := 3, live := shadowRoot :: base.live}

def addressWritten (base : MemoryRegisters addressLanguage testProfile) (address : Nat) :
    MemoryRegisters addressLanguage testProfile :=
  {(addressReady base) with
    initialized := shadowRoot :: base.initialized
    virtualValues := [(shadowRoot, .scalar (.int .u8 address))]
    addressIds := [(storeRoot, ⟨0⟩)]
    nextAddressId := 1}

/-- The fixture operation returns the address supplied by the instruction
judgment, so storage equality cannot make two allocations interchangeable. -/
private theorem addressSteps (project : Project) :
    let memory := addressMemory project
    memory.InstructionStep (language := addressLanguage) (addressReady storeAfter)
      (addressWritten storeAfter 1).advance false addressInstruction ∧
    memory.InstructionStep (language := addressLanguage) (addressReady alternateBacking)
      (addressWritten alternateBacking 2).advance false addressInstruction ∧
    ¬ memory.InstructionStep (language := addressLanguage) (addressReady alternateBacking)
      (addressWritten alternateBacking 1).advance false addressInstruction := by
  dsimp only
  refine ⟨?_, ?_, ?_⟩
  · refine ⟨[], _, [resolvedStore storeRange], _, [], _, addressWritten storeAfter 1,
      rfl, rfl, rfl, ⟨Or.inl ⟨storeRange, rfl, rfl⟩, rfl, rfl⟩, rfl, ?_, rfl⟩
    exact ⟨⟨rfl, fun _ => rfl⟩, nofun, rfl, rfl, nofun⟩
  · refine ⟨[], _, [resolvedStore alternateRange], _, [], _, addressWritten alternateBacking 2,
      rfl, rfl, rfl, ⟨Or.inl ⟨alternateRange, rfl, rfl⟩, rfl, rfl⟩, rfl, ?_, rfl⟩
    exact ⟨⟨rfl, fun _ => rfl⟩, nofun, rfl, rfl, nofun⟩
  · rintro ⟨values, afterRead, addresses, result, effects, changed, stored,
      reads, _, resolved, operates, _, writes, next⟩
    have readEq : some ([], addressReady alternateBacking) = some (values, afterRead) := reads
    cases readEq
    have resolvedEq : some [resolvedStore alternateRange] = some addresses := resolved
    cases resolvedEq
    have meaning : ((∃ range : Hybrid.Range, [resolvedStore alternateRange] = [resolvedStore range] ∧
          result = CoreValue.scalar (.int .u8 range.ptr.addr)) ∨
        (([resolvedStore alternateRange] = [abstractStore] ∨
          [resolvedStore alternateRange] = [emptyStoreAddress]) ∧ result = CoreValue.other ())) ∧
        effects = [] ∧ changed = Memory.operationStore
          (Memory.prepareAddresses (addressReady alternateBacking) [⟨(), storePlace⟩]) false := by
      with_unfolding_all exact operates
    rcases meaning with ⟨⟨range, operands, value⟩ | ⟨impossible, _⟩, _, unchanged⟩
    · have same : alternateRange = range := by
        have sameAddress := (List.cons.inj operands).1
        exact Option.some.inj (congrArg OperationAddress.range sameAddress)
      subst range
      subst changed
      subst result
      have written : some (addressWritten alternateBacking 2) = some stored := writes.2.2.1
      cases written
      have contradictionAtRoot := congrArg
        (fun state : MemoryRegisters addressLanguage testProfile => state.virtualValues) next
      cases contradictionAtRoot
    · rcases impossible with wrong | wrong
      all_goals
        have range := congrArg OperationAddress.range (List.cons.inj wrong).1
        cases range

def logicalAddressProgram (project : Project) : MIR testLowering testProfile :=
  let address : MIRInst testLowering :=
    .op (opcode := .named ⟨"address"⟩) (ty := ()) ⟨shadowRoot, []⟩ () .nil [] [⟨(), ⟨storeRoot, []⟩⟩]
  let indirect : MIRInst testLowering :=
    .op (opcode := .named ⟨"indirectReadWrite"⟩) (types := [()]) (ty := ()) ⟨shadowRoot, []⟩ ()
      (.cons (.copy ⟨shadowRoot, []⟩) .nil) []
  ⟨project, Exec.Demo.mainF, (addressMemory project).signatures,
    [(Exec.Demo.mainF, [(storeRoot, ()), (shadowRoot, ())], ⟨⟨0⟩, #[⟨⟨0⟩, [],
      [⟨testOrigin, .bind (ty := ()) ⟨storeRoot, []⟩ (.literal ⟨[42]⟩)⟩,
        ⟨testOrigin, address⟩, ⟨testOrigin, indirect⟩], .ret (some ⟨(), .copy ⟨shadowRoot, []⟩⟩), []⟩]⟩)],
    [(Exec.Demo.mainF, [shadowRoot])], []⟩

def logicalAddressBefore : MemoryRegisters addressLanguage testProfile :=
  {storeInitial with
    instruction := 1, live := [storeRoot], initialized := [storeRoot]
    virtualValues := [(storeRoot, storeValue)]}

def logicalAddressAfter : MemoryRegisters addressLanguage testProfile :=
  {logicalAddressBefore with
    instruction := 2, live := [shadowRoot, storeRoot], initialized := [shadowRoot, storeRoot]
    virtualValues := [(shadowRoot, .other ()), (storeRoot, storeValue)]
    addressIds := [(storeRoot, ⟨0⟩)], nextAddressId := 1}

-- One logical step serves both physical placements. A pointer to the other
-- placement fails the value relation even when the pointee bytes coincide.
theorem logicalAddressStep (project : Project) :
    (logicalAddressProgram project).Step (language := addressLanguage)
      logicalAddressBefore logicalAddressAfter ∧
    addressLanguage.representsValue testProfile.memoryTarget [(⟨0⟩, some storeRange)] ()
      (CoreValue.other ()) (CoreValue.scalar (.int .u8 1)) ∧
    addressLanguage.representsValue testProfile.memoryTarget [(⟨0⟩, some alternateRange)] ()
      (CoreValue.other ()) (CoreValue.scalar (.int .u8 2)) ∧
    ¬ addressLanguage.representsValue testProfile.memoryTarget [(⟨0⟩, some alternateRange)] ()
      (CoreValue.other ()) (CoreValue.scalar (.int .u8 1)) := by
  refine ⟨?_, rfl, rfl, (show CoreValue.scalar (.int .u8 1) ≠ (CoreValue.scalar (.int .u8 2) : CoreValue Unit) from by intro h; cases h)⟩
  refine ⟨rfl, [], _, [abstractStore], CoreValue.other (), [], _, {logicalAddressAfter with instruction := 1},
    rfl, rfl, rfl, ⟨Or.inr ⟨Or.inl rfl, rfl⟩, rfl, rfl⟩, rfl, ?_, rfl⟩
  with_unfolding_all
    refine Memory.WriteParams.cons (language := addressLanguage)
      (started := {logicalAddressBefore with
        live := [shadowRoot, storeRoot]
        addressIds := [(storeRoot, ⟨0⟩)], nextAddressId := 1}) ?_ ?_ (.nil _)
  · exact ⟨by decide, Or.inl ⟨rfl, rfl⟩⟩
  · exact ⟨⟨rfl, fun _ => rfl⟩, nofun, rfl, rfl, nofun⟩

private def zeroAddressMemory (project : Project) : Memory testLowering testProfile :=
  {addressMemory project with layouts := (zeroMemory project).layouts}

private def zeroAddressReady : MemoryRegisters addressLanguage testProfile :=
  {storeInitial with
    live := [shadowRoot, storeRoot], initialized := [storeRoot], zeroValues := [(storeRoot, storeValue)]}

private def zeroAddressWritten : MemoryRegisters addressLanguage testProfile :=
  {zeroAddressReady with
    initialized := [shadowRoot, storeRoot]
    virtualValues := [(shadowRoot, CoreValue.other ())]
    addressIds := [(storeRoot, ⟨0⟩)]
    nextAddressId := 1}

example (project : Project) :
    let memory := zeroAddressMemory project
    let address : Sigma (MemoryPlace Unit) := ⟨(), storePlace⟩
    memory.resolveAddress (Memory.prepareAddresses zeroAddressReady [address]) false address =
      some emptyStoreAddress ∧
    memory.InstructionStep (language := addressLanguage) zeroAddressReady
      {zeroAddressReady with
        instruction := 1, initialized := [shadowRoot, storeRoot]
        virtualValues := [(shadowRoot, .other ())], addressIds := [(storeRoot, ⟨0⟩)], nextAddressId := 1}
      false addressInstruction := by
  refine ⟨rfl, ?_⟩
  refine ⟨[], _, [emptyStoreAddress], CoreValue.other (), [], _, zeroAddressWritten, rfl, rfl, rfl,
    ⟨Or.inr ⟨Or.inr rfl, rfl⟩, rfl, rfl⟩, rfl, ?_, rfl⟩
  exact ⟨⟨rfl, fun _ => rfl⟩, nofun, rfl, rfl, nofun⟩

private def suspendedAddress : MemoryRegisters addressLanguage testProfile :=
  {storeInitial with
    live := [shadowRoot], initialized := [shadowRoot], virtualValues := [(shadowRoot, .other ())]
    callers := [⟨logicalAddressAfter.toMemoryFrame, none⟩], nextAddressId := 1}

private def updatedSuspendedAddress : MemoryRegisters addressLanguage testProfile :=
  let caller := {logicalAddressAfter.toMemoryFrame with
    virtualValues := [(shadowRoot, .other ()), (storeRoot, .scalar (.int .u8 99))]}
  {suspendedAddress with
    instruction := 1, virtualValues := [(shadowRoot, storeValue)]
    callers := [⟨caller, none⟩]}

private def indirectWriteStore : OperationStore (CoreValue Unit) :=
  {Memory.operationStore (language := addressLanguage) suspendedAddress true with
    addressedValues := [(⟨0⟩, CoreValue.scalar (.int .u8 99))]}

-- Read an escaped value through a reference and update the suspended frame
-- that owns it. Plant a lowering that discards the indirect write.
example (project : Project) :
    let memory := (logicalAddressProgram project).logicalView (language := addressLanguage)
    memory.InstructionStep suspendedAddress updatedSuspendedAddress true indirectInstruction ∧
    ¬ memory.InstructionStep suspendedAddress
      {updatedSuspendedAddress with callers := suspendedAddress.callers} true indirectInstruction := by
  dsimp only
  constructor
  · refine ⟨[CoreValue.other ()], _, [], storeValue, [], _, {updatedSuspendedAddress with instruction := 0},
      rfl, rfl, rfl, ⟨rfl, rfl, rfl, Or.inl ⟨rfl, rfl, rfl⟩⟩, rfl, ?_, rfl⟩
    with_unfolding_all
      exact Memory.WriteParams.cons (language := addressLanguage) rfl
        ⟨⟨rfl, fun _ => rfl⟩, nofun, rfl, rfl, nofun⟩ (.nil _)
  · rintro ⟨values, afterRead, operands, result, effects, changed, stored, reads, _, addresses,
      operates, _, writes, next⟩
    have actualRead : some ([CoreValue.other ()], suspendedAddress) = some (values, afterRead) := by
      with_unfolding_all exact reads
    cases actualRead
    have resolved : some [] = some operands := addresses
    cases resolved
    have observed : ([] : List (OperationAddress Unit)) = [] ∧ result = storeValue ∧ effects = [] ∧
        (([CoreValue.other ()] = [CoreValue.other ()] ∧
          AList.lookup (Memory.operationStore suspendedAddress true).addressedValues ⟨0⟩ = some storeValue ∧
          changed = indirectWriteStore) ∨
          ∃ ptr bytes, [CoreValue.other ()] = [CoreValue.scalar (.int .u8 ptr.addr)] ∧
            suspendedAddress.bytes.load testProfile.memoryTarget ptr 1 1 = some [.init 42 none] ∧
            suspendedAddress.bytes.store testProfile.memoryTarget ptr [.init 99 none] 1 = some bytes ∧
            changed = {Memory.operationStore suspendedAddress true with bytes := bytes}) := by
      with_unfolding_all exact operates
    obtain ⟨_, rfl, rfl, meaning⟩ := observed
    rcases meaning with ⟨_, _, rfl⟩ | ⟨_, _, impossible, _⟩
    · have params : ((logicalAddressProgram project).logicalView (language := addressLanguage)).WriteParams (language := addressLanguage)
          [⟨(), addressResultPlace⟩] [storeValue] (Memory.applyOperation suspendedAddress true indirectWriteStore) stored false := by
        simp only [Bool.true_or, ite_true] at writes
        with_unfolding_all exact writes
      cases params with
      | cons same write rest =>
        cases rest
        have before : _ = Memory.applyOperation suspendedAddress true indirectWriteStore := same
        subst_vars
        have actualWrite : some {updatedSuspendedAddress with instruction := 0} = some stored := write.2.2.1
        cases actualWrite
        have callers := congrArg (fun state : MemoryRegisters addressLanguage testProfile => state.callers) next
        cases callers
    · cases impossible

private def suspendedPhysical : MemoryRegisters addressLanguage testProfile :=
  {storeInitial with
    live := [shadowRoot], initialized := [shadowRoot]
    virtualValues := [(shadowRoot, CoreValue.scalar (.int .u8 1))]
    bytes := storeAfter.bytes
    callers := [⟨(addressWritten storeAfter 1).toMemoryFrame, none⟩], nextAddressId := 1}

private def updatedSuspendedPhysical : MemoryRegisters addressLanguage testProfile :=
  {suspendedPhysical with
    virtualValues := [(shadowRoot, storeValue)]
    bytes := ⟨[(⟨0⟩, {storeBlock with bytes := [.init 99 none]})], 1⟩}

example (project : Project) :
    (addressMemory project).InstructionStep (language := addressLanguage)
      suspendedPhysical updatedSuspendedPhysical.advance false indirectInstruction ∧
    (Memory.operationStore (language := addressLanguage) updatedSuspendedAddress true).addressedValues =
      [(⟨0⟩, CoreValue.scalar (.int .u8 99))] ∧
    updatedSuspendedPhysical.bytes.load testProfile.memoryTarget storeRange.ptr 1 1 =
      some [.init 99 none] := by
  refine ⟨?_, rfl, rfl⟩
  refine ⟨[CoreValue.scalar (.int .u8 1)], _, [], storeValue, [], _, updatedSuspendedPhysical,
    rfl, rfl, rfl, ⟨rfl, rfl, rfl, Or.inr ⟨storeRange.ptr, _, rfl, rfl, rfl, rfl⟩⟩, rfl, ?_, rfl⟩
  exact ⟨⟨rfl, fun _ => rfl⟩, nofun, rfl, rfl, nofun⟩

def logicalAddressFinal : MemoryRegisters addressLanguage testProfile :=
  {logicalAddressAfter with
    instruction := 3
    virtualValues := [(shadowRoot, storeValue), (storeRoot, CoreValue.scalar (.int .u8 99))]}

def physicalAddressFinal (base : MemoryRegisters addressLanguage testProfile) (address : Nat)
    (bytes : Mem.Memory) : MemoryRegisters addressLanguage testProfile :=
  {addressWritten base address with instruction := 5, virtualValues := [(shadowRoot, storeValue)], bytes := bytes}

-- Give the wrong compiler the exact input graph and the other live allocation.
-- Its substituted result cannot be projected from a certified Memory step.
example (project : Project) (m : LoweringModels addressLanguage testLowering testProfile)
    (q next : m.memory.execution.State)
    (reachable : m.memory.execution.finite (addressMemory project) q)
    (step : m.memory.execution.step (addressMemory project) q next)
    (before : m.memoryState q = addressReady alternateBacking)
    (wrong : m.memoryState next = (addressWritten alternateBacking 1).advance) :
    ¬ MemorySimulationWitness m (logicalAddressProgram project) (addressMemory project) := by
  intro lowers
  have tick := lowers.2.2.2.2.2.2.1.2.1 q next reachable step
  rw [before, wrong] at tick
  obtain ⟨block, fetched, runs⟩ := tick
  have fetchedAddress := (addressSteps project).2.2
  have same : some _ = some block := fetched
  cases same
  exact fetchedAddress runs

end StageInterfaceControls
