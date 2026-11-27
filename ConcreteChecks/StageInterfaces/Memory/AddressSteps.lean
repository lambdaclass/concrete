import Concrete.Memory.Execution
import ConcreteChecks.StageInterfaces.Memory.Addresses

open Concrete
open Concrete.Interfaces
open ConcreteSpec Core Core.SourcePathSpan StageInterface StageGrammar

namespace StageInterfaceControls

structure AddressAllocation where
  space : AddrSpace
  base : Nat
  fits : backingFits space base

def AddressAllocation.range (allocation : AddressAllocation) : Hybrid.Range :=
  backingRange allocation.space allocation.base

def AddressAllocation.before (allocation : AddressAllocation) :
    MemoryRegisters addressLanguage testProfile :=
  backingBefore allocation.space allocation.base

def AddressAllocation.bound (allocation : AddressAllocation) :
    MemoryRegisters addressLanguage testProfile :=
  backingAfter allocation.space allocation.base

def AddressAllocation.updatedBytes (allocation : AddressAllocation) : Mem.Memory :=
  ⟨[(⟨0⟩, {Mem.candidate allocation.space allocation.base 1 1 with bytes := [.init 99 none]})], 1⟩

theorem AddressAllocation.access (allocation : AddressAllocation) (byte : Fin 256) :
    (⟨[(⟨0⟩, {Mem.candidate allocation.space allocation.base 1 1 with bytes := [.init byte none]})], 1⟩ :
      Mem.Memory).access testProfile.memoryTarget allocation.range.ptr 1 1 = some (some (⟨0⟩, 0)) := by
  simp [AddressAllocation.range, backingRange, Mem.Memory.access, Mem.Memory.block, AList.lookup,
    Mem.candidate, Nat.mod_one]

theorem AddressAllocation.accessBound (allocation : AddressAllocation) :
    allocation.bound.bytes.access testProfile.memoryTarget allocation.range.ptr 1 1 = some (some (⟨0⟩, 0)) :=
  allocation.access 42

theorem AddressAllocation.loadBound (allocation : AddressAllocation) :
    allocation.bound.bytes.load testProfile.memoryTarget allocation.range.ptr 1 1 = some [.init 42 none] := by
  unfold Mem.Memory.load
  rw [allocation.accessBound]
  rfl

theorem AddressAllocation.storeBound (allocation : AddressAllocation) :
    allocation.bound.bytes.store testProfile.memoryTarget allocation.range.ptr [.init 99 none] 1 =
      some allocation.updatedBytes := by
  unfold Mem.Memory.store
  rw [show ([Mem.MByte.init 99 none]).length = 1 from rfl, allocation.accessBound]
  rfl

theorem AddressAllocation.storeScalar (allocation : AddressAllocation) (project : Project) :
    (addressMemory project).Writes (language := addressLanguage) allocation.before storePlace storeValue
      {allocation.bound with instruction := 1} := by
  have encoded : Hybrid.encode testProfile.memoryTarget (.int .u8 42) = some [.init 42 none] := rfl
  refine ⟨⟨rfl, fun _ => rfl⟩, (fun _ => ⟨⟨1, 1, .nil⟩, rfl, ⟨nofun, nofun⟩⟩),
    ?_, ?_, fun _ => Or.inr ⟨allocation.range, [(0, [.init 42 none])], rfl, rfl, ?_, ?_⟩⟩
  · simp [Memory.writeValue, Memory.markWritten, Memory.storeRuns, Memory.atOffset, Memory.mayInitialize, Memory.authorizeWrite, Memory.writeOwnerView, Memory.ownerView, Memory.zeroRoot, Memory.rangeAt, Memory.byteRuns, Memory.layoutAt,
      addressMemory, storingMemory, storePlace, AddressAllocation.before, AddressAllocation.bound,
      backingBefore, backingAfter, backingRange, storeBefore, storeInitial, storeRoot,
      Mem.Memory.store, Mem.Memory.access, Mem.Memory.block, Mem.Memory.modify,
      AList.lookup, AList.update, Mem.candidate, Nat.mod_one, addressLanguage, storeValue, encoded] <;> rfl
  · change (do
      let bytes ← allocation.bound.bytes.load testProfile.memoryTarget allocation.range.ptr 1 1
      let value ← Hybrid.decode testProfile.memoryTarget (.int .u8) bytes
      pure (CoreValue.scalar value : CoreValue Unit)) = some storeValue
    rw [allocation.loadBound]
    rfl
  · exact fun _ => ⟨0, [.init 42 none], List.mem_cons_self .., by decide⟩
  · intro offset bytes member
    cases List.mem_singleton.mp member
    refine ⟨Nat.le_refl 1, ?_, allocation.loadBound⟩
    intro byte included
    cases List.mem_singleton.mp included
    decide

inductive AddressSourcePhase where
  | start | bound | reference | done

def AddressSourcePhase.state : AddressSourcePhase → MemoryRegisters addressLanguage testProfile
  | .start => storeInitial
  | .bound => logicalAddressBefore
  | .reference => logicalAddressAfter
  | .done => logicalAddressFinal

def AddressSourcePhase.next : AddressSourcePhase → Option AddressSourcePhase
  | .start => some .bound
  | .bound => some .reference
  | .reference => some .done
  | .done => none

inductive AddressTargetPhase where
  | start
  | allocated (allocation : AddressAllocation)
  | bound (allocation : AddressAllocation)
  | ready (allocation : AddressAllocation)
  | reference (allocation : AddressAllocation)
  | done (allocation : AddressAllocation)

def AddressTargetPhase.state : AddressTargetPhase → MemoryRegisters addressLanguage testProfile
  | .start => storeInitial
  | .allocated allocation => allocation.before
  | .bound allocation => allocation.bound
  | .ready allocation => addressReady allocation.bound
  | .reference allocation => (addressWritten allocation.bound allocation.base).advance
  | .done allocation => physicalAddressFinal allocation.bound allocation.base allocation.updatedBytes

def AddressTargetPhase.source : AddressTargetPhase → AddressSourcePhase
  | .start | .allocated _ => .start
  | .bound _ | .ready _ => .bound
  | .reference _ => .reference
  | .done _ => .done

inductive AddressTargetPhase.Tick : AddressTargetPhase → AddressTargetPhase → Prop
  | allocate (a) : Tick .start (.allocated a)
  | bind (a) : Tick (.allocated a) (.bound a)
  | beginDestination (a) : Tick (.bound a) (.ready a)
  | takeAddress (a) : Tick (.ready a) (.reference a)
  | indirect (a) : Tick (.reference a) (.done a)

private theorem physicalAddressExact (project : Project) (allocation : AddressAllocation)
    (next : MemoryRegisters addressLanguage testProfile) :
    (addressMemory project).InstructionStep (language := addressLanguage)
      (addressReady allocation.bound) next false addressInstruction ↔
    next = (AddressTargetPhase.reference allocation).state := by
  have resolved : (addressMemory project).resolveAddress
      (Memory.prepareAddresses (addressReady allocation.bound) [⟨(), storePlace⟩]) false
      ⟨(), storePlace⟩ = some (resolvedStore allocation.range) := by
    with_unfolding_all
      change (do
        let _ ← allocation.bound.bytes.access testProfile.memoryTarget allocation.range.ptr 1 1
        pure (resolvedStore allocation.range)) = _
    rw [allocation.accessBound]
    rfl
  constructor
  · rintro ⟨values, readState, addresses, result, effects, changed, stored, read, _, fetched, op, _, writes, nextEq⟩
    have readEq : some ([], addressReady allocation.bound) = some (values, readState) := read
    cases readEq
    have fetchedEq : some [resolvedStore allocation.range] = some addresses := by
      change (do
        let a ← (addressMemory project).resolveAddress
          (Memory.prepareAddresses (addressReady allocation.bound) [⟨(), storePlace⟩]) false ⟨(), storePlace⟩
        pure [a]) = some addresses at fetched
      rw [resolved] at fetched
      exact fetched
    cases fetchedEq
    have meaning : ((∃ range, [resolvedStore allocation.range] = [resolvedStore range] ∧
          result = CoreValue.scalar (.int .u8 range.ptr.addr)) ∨
        (([resolvedStore allocation.range] = [abstractStore] ∨
          [resolvedStore allocation.range] = [emptyStoreAddress]) ∧ result = CoreValue.other ())) ∧
        effects = [] ∧ changed = Memory.operationStore
          (Memory.prepareAddresses (addressReady allocation.bound) [⟨(), storePlace⟩]) false := by
      with_unfolding_all exact op
    rcases meaning with ⟨⟨range, operands, rfl⟩ | ⟨impossible, _⟩, rfl, rfl⟩
    · have same : allocation.range = range :=
        Option.some.inj (congrArg OperationAddress.range (List.cons.inj operands).1)
      subst range
      have written : some (addressWritten allocation.bound allocation.base) = some stored := writes.2.2.1
      cases written
      exact nextEq
    · rcases impossible with wrong | wrong
      all_goals
        have range := congrArg OperationAddress.range (List.cons.inj wrong).1
        cases range
  · rintro rfl
    refine ⟨[], _, [resolvedStore allocation.range], CoreValue.scalar (.int .u8 allocation.base), [], _,
      addressWritten allocation.bound allocation.base, rfl, rfl, ?_,
      ⟨Or.inl ⟨allocation.range, rfl, rfl⟩, rfl, rfl⟩, rfl, ?_, rfl⟩
    · change (do
        let a ← (addressMemory project).resolveAddress
          (Memory.prepareAddresses (addressReady allocation.bound) [⟨(), storePlace⟩]) false ⟨(), storePlace⟩
        pure [a]) = _
      rw [resolved]
      rfl
    · exact ⟨⟨rfl, fun _ => rfl⟩, nofun, rfl, rfl, nofun⟩

private theorem logicalWritesUnique (memory : Memory testLowering testProfile)
    (place : MemoryPlace Unit ()) (value : CoreValue Unit)
    (before started stored expected : MemoryRegisters addressLanguage testProfile)
    (unique : ∀ other, (if before.live.contains place.logical.root then other = before
      else memory.Begins (language := addressLanguage) before place.logical.root other) → other = started)
    (writes : memory.WriteParams (language := addressLanguage) [⟨(), place⟩]
      [value] before stored false)
    (writing : memory.writeValue (language := addressLanguage) started place value = some expected) :
    stored = expected := by
  cases writes with
  | cons begin write rest =>
    cases rest
    have starts := unique _ begin
    subst_vars
    exact Option.some.inj (write.2.2.1.symm.trans writing)


abbrev logicalAddressView (project : Project) : Memory testLowering testProfile :=
  @MIR.logicalView addressLanguage testLowering testProfile (logicalAddressProgram project)

def logicalAddressBlock : MemoryBlock testLowering :=
  let root : MemoryPlace Unit () := ⟨⟨storeRoot, []⟩, .virtual⟩
  let address : MemoryInst testLowering :=
    .op (opcode := .named ⟨"address"⟩) addressResultPlace () .nil [] [⟨(), root⟩]
  ⟨⟨0⟩, [], [⟨testOrigin, .bind root (.literal ⟨[42]⟩)⟩, ⟨testOrigin, address⟩,
    ⟨testOrigin, indirectInstruction⟩], .ret (some ⟨(), .copy addressResultPlace⟩), []⟩

theorem logicalAddressExact (project : Project) (phase : AddressSourcePhase)
    (next : MemoryRegisters addressLanguage testProfile) :
    (logicalAddressProgram project).Step (language := addressLanguage) phase.state next ↔
      ∃ q, phase.next = some q ∧ q.state = next := by
  constructor
  · intro run
    cases phase with
    | start =>
      obtain ⟨_, value, state, stored, read, write, rfl⟩ := run
      have actual : some (storeValue, storeInitial) = some (value, state) := read
      cases actual
      have params : (logicalAddressView project).WriteParams (language := addressLanguage)
          [⟨(), ⟨⟨storeRoot, []⟩, .virtual⟩⟩] [storeValue] storeInitial stored false := by
        simp only [Bool.true_or, ite_true] at write
        with_unfolding_all exact write
      have unique : ∀ state, (logicalAddressView project).Begins
          (language := addressLanguage) storeInitial storeRoot state →
          state = {storeInitial with live := [storeRoot]} := by
        intro state h
        rcases h.2 with ⟨_, eq⟩ | ⟨bad, _⟩
        · exact eq
        · have impossible : some StorageClass.virtual = some .materialized := bad
          cases impossible
      have eq := logicalWritesUnique (logicalAddressView project) ⟨⟨storeRoot, []⟩, .virtual⟩
        storeValue storeInitial {storeInitial with live := [storeRoot]} stored
        {logicalAddressBefore with instruction := 0} unique params rfl
      cases eq
      exact ⟨.bound, rfl, rfl⟩
    | bound =>
      obtain ⟨_, values, state, operands, result, effects, changed, stored, read, _, resolved, op, _, write, rfl⟩ := run
      have actual : some ([], logicalAddressBefore) = some (values, state) := read
      cases actual
      have address : some [abstractStore] = some operands := by with_unfolding_all exact resolved
      cases address
      have meaning : ((∃ range, [abstractStore] = [resolvedStore range] ∧
            result = CoreValue.scalar (.int .u8 range.ptr.addr)) ∨
          (([abstractStore] = [abstractStore] ∨ [abstractStore] = [emptyStoreAddress]) ∧ result = CoreValue.other ())) ∧
          effects = [] ∧ changed = Memory.operationStore
            (Memory.prepareAddresses logicalAddressBefore [⟨(), ⟨⟨storeRoot, []⟩, .virtual⟩⟩]) true := by
        with_unfolding_all exact op
      rcases meaning with ⟨⟨_, impossible, _⟩ | ⟨_, rfl⟩, rfl, rfl⟩
      · have range := congrArg OperationAddress.range (List.cons.inj impossible).1
        cases range
      · let ready : MemoryRegisters addressLanguage testProfile := {logicalAddressBefore with
          addressIds := [(storeRoot, ⟨0⟩)]
          nextAddressId := 1}
        have params : (logicalAddressView project).WriteParams (language := addressLanguage)
            [⟨(), addressResultPlace⟩] [CoreValue.other ()] ready stored false := by
          simp only [Bool.true_or, ite_true] at write
          with_unfolding_all exact write
        have unique : ∀ state, (logicalAddressView project).Begins
            (language := addressLanguage) ready shadowRoot state →
            state = {ready with live := [shadowRoot, storeRoot]} := by
          intro state h
          rcases h.2 with ⟨_, eq⟩ | ⟨bad, _⟩
          · exact eq
          · have impossible : some StorageClass.virtual = some .materialized := bad
            cases impossible
        have eq := logicalWritesUnique (logicalAddressView project) addressResultPlace
          (CoreValue.other ()) ready {ready with live := [shadowRoot, storeRoot]} stored
          {logicalAddressAfter with instruction := 1} unique params rfl
        cases eq
        exact ⟨.reference, rfl, rfl⟩
    | reference =>
      obtain ⟨_, values, state, operands, result, effects, changed, stored, read, _, resolved, op, _, write, rfl⟩ := run
      have actual : some ([CoreValue.other ()], logicalAddressAfter) = some (values, state) := by
        with_unfolding_all exact read
      cases actual
      have address : some [] = some operands := resolved
      cases address
      let modified : OperationStore (CoreValue Unit) :=
        {Memory.operationStore (language := addressLanguage) logicalAddressAfter true with
          addressedValues := [(⟨0⟩, CoreValue.scalar (.int .u8 99))]}
      have meaning : ([] : List (OperationAddress Unit)) = [] ∧ result = storeValue ∧ effects = [] ∧
          (([CoreValue.other ()] = [CoreValue.other ()] ∧
            AList.lookup (Memory.operationStore logicalAddressAfter true).addressedValues ⟨0⟩ = some storeValue ∧
            changed = modified) ∨
            ∃ ptr bytes, [CoreValue.other ()] = [CoreValue.scalar (.int .u8 ptr.addr)] ∧
              logicalAddressAfter.bytes.load testProfile.memoryTarget ptr 1 1 = some [.init 42 none] ∧
              logicalAddressAfter.bytes.store testProfile.memoryTarget ptr [.init 99 none] 1 = some bytes ∧
              changed = {Memory.operationStore logicalAddressAfter true with bytes := bytes}) := by
        with_unfolding_all exact op
      obtain ⟨_, rfl, rfl, meaning⟩ := meaning
      rcases meaning with ⟨_, _, rfl⟩ | ⟨_, _, impossible, _⟩
      · let ready := Memory.applyOperation (language := addressLanguage) logicalAddressAfter true modified
        have params : (logicalAddressView project).WriteParams (language := addressLanguage)
            [⟨(), addressResultPlace⟩] [storeValue] ready stored false := by
          simp only [Bool.true_or, ite_true] at write
          with_unfolding_all exact write
        have eq := logicalWritesUnique (logicalAddressView project) addressResultPlace
          storeValue ready ready stored {logicalAddressFinal with instruction := 2}
          (fun _ h => h) params rfl
        cases eq
        exact ⟨.done, rfl, rfl⟩
      · cases impossible
    | done =>
      obtain ⟨block, found, step⟩ := run
      have pulled : some logicalAddressBlock = some block := by with_unfolding_all exact found
      cases pulled
      with_unfolding_all
        change _ ∧ (logicalAddressView project).Return (language := addressLanguage)
          (some ⟨(), .copy addressResultPlace⟩) logicalAddressFinal next at step
      obtain ⟨_, caller, rest, _, _, _, callers, _⟩ := step
      change [] = caller :: rest at callers
      cases callers
  · rintro ⟨q, step, rfl⟩
    cases phase
    · have same : some AddressSourcePhase.bound = some q := step
      cases same
      with_unfolding_all
        refine ⟨rfl, storeValue, storeInitial, {logicalAddressBefore with instruction := 0}, rfl, ?_, rfl⟩
      exact Memory.WriteParams.cons (language := addressLanguage)
        (started := {storeInitial with live := [storeRoot]}) ⟨by decide, Or.inl ⟨rfl, rfl⟩⟩
        ⟨⟨rfl, fun _ => rfl⟩, nofun, rfl, rfl, nofun⟩ (.nil _)
    · have same : some AddressSourcePhase.reference = some q := step
      cases same
      exact (logicalAddressStep project).1
    · have same : some AddressSourcePhase.done = some q := step
      cases same
      refine ⟨rfl, [CoreValue.other ()], logicalAddressAfter, [], storeValue, [], _,
        {logicalAddressFinal with instruction := 2}, rfl, rfl, rfl,
        ⟨rfl, rfl, rfl, Or.inl ⟨rfl, rfl, rfl⟩⟩, rfl, ?_, rfl⟩
      with_unfolding_all
        exact Memory.WriteParams.cons (language := addressLanguage) rfl
          ⟨⟨rfl, fun _ => rfl⟩, nofun, rfl, rfl, nofun⟩ (.nil _)
    · have impossible : (none : Option AddressSourcePhase) = some q := step
      cases impossible


theorem AddressAllocation.pointerUnique (a : AddressAllocation) (ptr : Mem.Ptr)
    (base : ptr.addr = a.base)
    (load : a.bound.bytes.load testProfile.memoryTarget ptr 1 1 = some [.init 42 none]) :
    ptr = a.range.ptr := by
  obtain ⟨space, address, provenance⟩ := ptr
  change address = a.base at base
  subst address
  cases provenance with
  | none =>
    have impossible : (none : Option (List Mem.MByte)) = some [.init 42 none] := load
    cases impossible
  | some block =>
    unfold Mem.Memory.load Mem.Memory.access at load
    simp only [Nat.one_ne_zero, ite_false] at load
    -- Select the only live byte block in the allocation.
    by_cases same : (⟨0⟩ : BlockId) = block
    · subst block
      by_cases inSpace : a.space = space
      · subst space
        rfl
      · simp [AddressAllocation.bound, backingAfter, backingBefore, Mem.Memory.block,
          AList.lookup, Mem.candidate, inSpace] at load
    · simp [AddressAllocation.bound, backingAfter, backingBefore, Mem.Memory.block,
        AList.lookup, same] at load

private theorem physicalIndirectExact (project : Project) (a : AddressAllocation)
    (next : MemoryRegisters addressLanguage testProfile) :
    (addressMemory project).InstructionStep (language := addressLanguage)
      (AddressTargetPhase.reference a).state next false indirectInstruction ↔
    next = (AddressTargetPhase.done a).state := by
  let before := (AddressTargetPhase.reference a).state
  let storedState : MemoryRegisters addressLanguage testProfile :=
    {(AddressTargetPhase.done a).state with instruction := 4}
  constructor
  · rintro ⟨values, afterRead, operands, result, effects, changed, stored, reads, _, resolves, op, _, writes, ends⟩
    have read : some ([CoreValue.scalar (.int .u8 a.base)], before) = some (values, afterRead) := reads
    cases read
    have resolve : some [] = some operands := resolves
    cases resolve
    have meaning : ([] : List (OperationAddress Unit)) = [] ∧ result = storeValue ∧ effects = [] ∧
        (([CoreValue.scalar (.int .u8 a.base)] = [CoreValue.other ()] ∧
          AList.lookup (Memory.operationStore before false).addressedValues ⟨0⟩ = some storeValue ∧
          changed = {Memory.operationStore before false with
            addressedValues := AList.update (Memory.operationStore before false).addressedValues ⟨0⟩
              (fun _ => CoreValue.scalar (.int .u8 99))}) ∨
          ∃ ptr bytes, [(CoreValue.scalar (.int .u8 a.base) : CoreValue Unit)] = [CoreValue.scalar (.int .u8 ptr.addr)] ∧
            a.bound.bytes.load testProfile.memoryTarget ptr 1 1 = some [.init 42 none] ∧
            a.bound.bytes.store testProfile.memoryTarget ptr [.init 99 none] 1 = some bytes ∧
            changed = {Memory.operationStore before false with bytes := bytes}) := by
      with_unfolding_all exact op
    obtain ⟨_, rfl, rfl, meaning⟩ := meaning
    rcases meaning with ⟨impossible, _⟩ | ⟨ptr, bytes, argument, loaded, written, rfl⟩
    · cases impossible
    · have addr : ptr.addr = a.base := by
        have equal := (List.cons.inj argument).1
        injection equal with el
        injection el with _ number
        exact_mod_cast number.symm
      have actual := a.pointerUnique ptr addr loaded
      subst ptr
      have exactBytes := Option.some.inj (a.storeBound.symm.trans written)
      subst bytes
      have exactWrite : some storedState = some stored := by
        with_unfolding_all exact writes.2.2.1
      cases exactWrite
      exact ends
  · rintro rfl
    refine ⟨[CoreValue.scalar (.int .u8 a.base)], before, [], storeValue, [], _, storedState,
      rfl, rfl, rfl, ⟨rfl, rfl, rfl, Or.inr ⟨a.range.ptr, a.updatedBytes, rfl, ?_, ?_, rfl⟩⟩, rfl, ?_, rfl⟩
    · exact a.loadBound
    · exact a.storeBound
    · exact ⟨⟨rfl, fun _ => rfl⟩, nofun, rfl, rfl, nofun⟩

private theorem addressTargetNormal (project : Project) (phase : AddressTargetPhase)
    (next : MemoryRegisters addressLanguage testProfile) :
    (addressMemory project).StepOutcome (language := addressLanguage) phase.state next false ↔
      ∃ q, phase.Tick q ∧ q.state = next := by
  have block : (addressMemory project).blockAt phase.state = some addressBlock := by
    cases phase <;> rfl
  constructor
  · rintro ⟨actual, fetched, step⟩
    rw [block] at fetched
    have equal := Option.some.inj fetched
    subst actual
    cases phase with
    | start =>
      change (∃ begun, (addressMemory project).Begins (language := addressLanguage)
        storeInitial storeRoot begun ∧ next = begun.advance) at step
      obtain ⟨begun, begins, rfl⟩ := step
      rcases begins.2 with ⟨impossible, _⟩ | ⟨_, layout, found, zero | allocated⟩
      · have wrong : some StorageClass.materialized = some .virtual := impossible
        cases wrong
      · have same : some (⟨1, 1, .nil⟩ : Layout) = some layout := found
        cases same
        exact (Nat.one_ne_zero zero.1).elim
      · have same : some (⟨1, 1, .nil⟩ : Layout) = some layout := found
        cases same
        obtain ⟨_, space, base, id, bytes, alloc, rfl⟩ := allocated
        have result : Mem.Memory.empty.alloc testProfile.memoryTarget space base 1 1 = .ok (id, bytes) := alloc
        unfold Mem.Memory.alloc at result
        dsimp only at result
        split at result
        · rename_i fits
          cases result
          exact ⟨.allocated ⟨space, base, fits⟩, .allocate _, rfl⟩
        · contradiction
    | allocated a =>
      change (addressMemory project).InstructionStep (language := addressLanguage)
        a.before next false (.bind storePlace (.literal ⟨[42]⟩)) at step
      obtain ⟨value, afterRead, stored, read, write, rfl⟩ := step
      have same : some (storeValue, a.before) = some (value, afterRead) := read
      cases same
      have exactWrite := (a.storeScalar project).2.2.1.symm.trans write.2.2.1
      cases exactWrite
      exact ⟨.bound a, .bind _, rfl⟩
    | bound a =>
      change (∃ begun, (addressMemory project).Begins (language := addressLanguage)
        a.bound shadowRoot begun ∧ next = begun.advance) at step
      obtain ⟨begun, begins, rfl⟩ := step
      rcases begins.2 with ⟨_, rfl⟩ | ⟨bad, _⟩
      · exact ⟨.ready a, .beginDestination _, rfl⟩
      · have impossible : some StorageClass.virtual = some .materialized := bad
        cases impossible
    | ready a =>
      have nextIs := (physicalAddressExact project a next).1 step
      exact ⟨.reference a, .takeAddress _, nextIs.symm⟩
    | reference a =>
      have nextIs := (physicalIndirectExact project a next).1 step
      exact ⟨.done a, .indirect _, nextIs.symm⟩
    | done a =>
      change _ ∧ (addressMemory project).Return (language := addressLanguage)
        (some ⟨(), .copy addressResultPlace⟩) (AddressTargetPhase.done a).state next at step
      obtain ⟨_, caller, rest, _, _, _, callers, _⟩ := step
      change [] = caller :: rest at callers
      cases callers
  · rintro ⟨q, tick, rfl⟩
    refine ⟨addressBlock, block, ?_⟩
    cases tick with
    | allocate a =>
      refine ⟨{a.before with instruction := 0}, ⟨by decide, Or.inr ⟨rfl, ⟨1, 1, .nil⟩, rfl, Or.inr ?_⟩⟩, rfl⟩
      refine ⟨by decide, a.space, a.base, ⟨0⟩, a.before.bytes, ?_, rfl⟩
      change Mem.Memory.empty.alloc testProfile.memoryTarget a.space a.base 1 1 = _
      unfold Mem.Memory.alloc
      have fits : Mem.Memory.empty.Fits testProfile.memoryTarget (Mem.candidate a.space a.base 1 1) := a.fits
      simp only [fits, ite_true]
      rfl
    | bind a => exact ⟨storeValue, a.before, {a.bound with instruction := 1}, rfl, a.storeScalar project, rfl⟩
    | beginDestination a =>
      exact ⟨{a.bound with live := [shadowRoot, storeRoot]}, ⟨nofun, Or.inl ⟨rfl, rfl⟩⟩, rfl⟩
    | takeAddress a => exact (physicalAddressExact project a _).2 rfl
    | indirect a => exact (physicalIndirectExact project a _).2 rfl


theorem addressTargetExact (project : Project) (phase : AddressTargetPhase)
    (next : MemoryRegisters addressLanguage testProfile) :
    (addressMemory project).Step (language := addressLanguage) phase.state next ↔
      ∃ q, phase.Tick q ∧ q.state = next := by
  constructor
  · intro tick
    apply (addressTargetNormal project phase next).mp
    apply tick.normal (language := addressLanguage)
    intro partialState failed
    have block : (addressMemory project).blockAt phase.state = some addressBlock := by cases phase <;> with_unfolding_all rfl
    cases phase with
    | start =>
      have ok := (addressTargetNormal project .start (.allocated ⟨⟨0⟩, 1, by unfold backingFits; decide⟩ : AddressTargetPhase).state).mpr
        ⟨_, .allocate _, rfl⟩
      have exhausted : (addressMemory project).BeginExhausted (language := addressLanguage) storeInitial storeRoot :=
        failed.instructionAt (op := ⟨testOrigin, .beginLifetime storeRoot⟩) block (by with_unfolding_all rfl)
      obtain ⟨started, begins, _⟩ := ok.instructionAt (op := ⟨testOrigin, .beginLifetime storeRoot⟩) block (by with_unfolding_all rfl)
      exact exhausted.unavailable ⟨started, begins⟩
    | allocated a =>
      exact Memory.bind_cannot_exhaust (language := addressLanguage)
        (failed.instructionAt (op := ⟨testOrigin, .bind storePlace (.literal ⟨[42]⟩)⟩) block (by with_unfolding_all rfl))
    | bound a =>
      have bad : (addressMemory project).BeginExhausted (language := addressLanguage) a.bound shadowRoot :=
        failed.instructionAt (op := ⟨testOrigin, .beginLifetime shadowRoot⟩) block (by with_unfolding_all rfl)
      have no : some StorageClass.virtual = some .materialized := by with_unfolding_all exact bad.materialized
      contradiction
    | ready a =>
      exact Memory.op_cannot_exhaust (language := addressLanguage)
        (failed.instructionAt (op := ⟨testOrigin, addressInstruction⟩) block (by with_unfolding_all rfl))
    | reference a =>
      exact Memory.op_cannot_exhaust (language := addressLanguage)
        (failed.instructionAt (op := ⟨testOrigin, indirectInstruction⟩) block (by with_unfolding_all rfl))
    | done a =>
      obtain ⟨caller, rest, _, _, _, callers, _⟩ := failed.terminatorAt block (by with_unfolding_all rfl)
      change [] = caller :: rest at callers
      cases callers
  · rintro ⟨q, tick, rfl⟩
    have normal := (addressTargetNormal project phase q.state).mpr ⟨q, tick, rfl⟩
    cases tick <;> exact normal

end StageInterfaceControls
