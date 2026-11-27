import ConcreteChecks.StageInterfaces.Memory.Fixtures
import ConcreteChecks.StageInterfaces.Refinement

open Concrete
open Concrete.Interfaces
open ConcreteSpec Core Core.SourcePathSpan StageInterface StageGrammar

namespace StageInterfaceControls

private theorem storingWrite (project : Project) :
    (storingMemory project).Writes (language := testLanguage) storeBefore storePlace storeValue
      {storeAfter with instruction := 1} := by
  refine ⟨⟨rfl, fun _ => rfl⟩, (fun _ => ⟨⟨1, 1⟩, rfl, by decide⟩), rfl, rfl, fun _ => Or.inr ⟨storeRange, [(0, [.init 42 none])], rfl, rfl, ?_, ?_⟩⟩
  · exact fun _ => ⟨0, [.init 42 none], List.mem_cons_self .., by decide⟩
  · intro offset bytes member
    have eq := List.mem_singleton.mp member
    cases eq
    refine ⟨by decide, ?_, rfl⟩
    intro byte hb
    cases List.mem_singleton.mp hb
    decide

example (project : Project) : (storingMemory project).Step (language := testLanguage) storeBefore storeAfter :=
  ⟨_, rfl, storeValue, storeBefore, {storeAfter with instruction := 1}, rfl, storingWrite project, rfl⟩

-- Even with a truthful PC and lifetime, an erased byte write cannot satisfy the
-- registered Memory success contract. Its projection leaves the allocated byte uninitialized.
example (project : Project) (m : LoweringModels testLanguage testLowering testProfile)
    (q next : m.memory.execution.State)
    (reachable : m.memory.execution.finite (storingMemory project) q)
    (step : m.memory.execution.step (storingMemory project) q next)
    (before : m.memoryState q = storeBefore)
    (erased : m.memoryState next = {storeAfter with bytes := storeBefore.bytes}) :
    ¬ MemorySimulationWitness m (testMIR project []) (storingMemory project) := by
  intro lowers
  have exactStep := lowers.2.2.2.2.2.2.1.2.1 q next reachable step
  rw [before, erased] at exactStep
  obtain ⟨block, fetched, instruction⟩ := exactStep
  have eq : (storingMemory project).blockAt storeBefore = some block := fetched
  change some _ = some block at eq
  cases eq
  obtain ⟨value, afterRead, stored, read, write, finish⟩ := instruction
  have eqRead : some (storeValue, storeBefore) = some (value, afterRead) := read
  cases eqRead
  have eqWrite : some {storeAfter with instruction := 1} = some stored := write.2.2.1
  cases eqWrite
  have bytes := congrArg (fun state => (MemoryState.bytes state).blocks) finish
  exact (show storeBefore.bytes.blocks ≠ storeAfter.bytes.blocks from by decide) bytes

private def falseReturnModels (m : LoweringModels testLanguage testLowering testProfile) :
    LoweringModels testLanguage testLowering testProfile :=
  {m with
    mir := returnsStatus _ 42
    memory := returnsStatus _ 42
    memoryState := fun _ => storeInitial
    mirState := fun _ => storeInitial}

-- The current return instruction really yields zero.
set_option maxRecDepth 2048 in
example (project : Project) :
    (returningMemory project).terminalObservation storeInitial =
      some (.returned 0 ByteArray.empty) := by with_unfolding_all rfl

-- A recursive activation of the entry returns to its caller. A trap in that
-- activation still reports the fetched terminator's kind and lexical origin.
example (project : Project) :
    (returningMemory project).terminalObservation
      {storeInitial with callers := [⟨storeInitial.toMemoryFrame, none⟩]} = none := rfl

example (project : Project) :
    let memory : Memory testLowering testProfile := {returningMemory project with
      functions := [(Exec.Demo.mainF, [], ⟨⟨0⟩,
        #[⟨⟨0⟩, [], [], .trap .divisionByZero testOrigin, []⟩]⟩)]}
    memory.terminalObservation {storeInitial with callers := [⟨storeInitial.toMemoryFrame, none⟩]} =
      some (.trapped 134 .divisionByZero testOrigin.lexical
        (trapDiagnostic project .divisionByZero testOrigin.operation testOrigin.lexical)) := rfl

-- The two models agree on 42, but the full handoff must reject their claim:
-- the actual Memory terminator returns zero from its initial state.
example (project : Project) (m : LoweringModels testLanguage testLowering testProfile) :
    ¬ MemorySimulationWitness (falseReturnModels m) (returningMIR project) (returningMemory project) := by
  intro lowers
  have terminal := lowers.2.2.2.2.2.2.1.2.2.2 () (.returned 42 ByteArray.empty)
    ⟨(), trivial, .nil ()⟩
  have wrong := terminal.mp rfl
  have status := congrArg (Option.map fun o => match o with
    | .returned code _ => code
    | _ => 255) wrong
  exact (show some (0 : UInt8) ≠ some 42 from by decide) status


-- A structurally correct entry and return cannot carry an extra host effect.
example (project : Project) (m : LoweringModels testLanguage testLowering testProfile)
    (q : m.memory.execution.State) (reachable : m.memory.execution.finite (returningMemory project) q)
    (entry : m.memoryState q = storeInitial)
    (forged : m.memory.execution.trace (returningMemory project) q = [.effect (show testLanguage.operands.Event from (7 : Nat))]) :
    ¬ MemorySimulationWitness m (returningMIR project) (returningMemory project) := by
  intro lowers
  have trace := lowers.2.2.2.2.2.2.1.1.2 q reachable
  rw [forged, entry] at trace
  exact (show ([.effect 7] : List (Concrete.Contracts.ProcessEvent Nat)) ≠ [] from nofun) trace

private def twoCallMemory (project : Project) : Memory testLowering testProfile :=
  let caller : MemoryBlock testLowering :=
    ⟨⟨0⟩, [], [⟨testOrigin, .call callee none []⟩, ⟨testOrigin, .call callee none []⟩],
      .ret (some ⟨(), .literal ⟨[0]⟩⟩), []⟩
  let helper : MemoryBlock testLowering := ⟨⟨0⟩, [], [], .ret none, []⟩
  ⟨project, Exec.Demo.mainF, [(Exec.Demo.mainF, ⟨[], some ()⟩), (callee, ⟨[], none⟩)],
    [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[caller]⟩), (callee, [], ⟨⟨0⟩, #[helper]⟩)], [], [], [], []⟩

private def resumeAt (index : Nat) : MemoryRegisters testLanguage testProfile :=
  {storeInitial with instruction := index}

private def helperReturning (index : Nat) : MemoryRegisters testLanguage testProfile :=
  {storeInitial with frame := callee, callers := [⟨(resumeAt index).toMemoryFrame, none⟩]}

-- Both calls use the same callee PC and byte memory. Each return pops its own
-- continuation and the second return cannot resume the first call's successor.
set_option maxRecDepth 2048 in
example (project : Project) :
    let memory := twoCallMemory project
    memory.Step storeInitial (helperReturning 1) ∧
    memory.Step (helperReturning 1) (resumeAt 1) ∧
    memory.Step (resumeAt 1) (helperReturning 2) ∧
    memory.Step (helperReturning 2) (resumeAt 2) ∧
    (¬ memory.Step (helperReturning 2) (resumeAt 1)) ∧
    memory.terminalObservation (resumeAt 2) = some (.returned 0 ByteArray.empty) := by
  dsimp only
  refine ⟨?_, ?_, ?_, ?_, ?_, by with_unfolding_all rfl⟩
  · exact ⟨_, rfl, rfl, [], _, _, _, rfl, rfl, rfl, rfl, .nil _⟩
  · exact ⟨_, rfl, rfl, _, [], [], _, _, rfl, rfl, .nil _, rfl, .nil _⟩
  · exact ⟨_, rfl, rfl, [], _, _, _, rfl, rfl, rfl, rfl, .nil _⟩
  · exact ⟨_, rfl, rfl, _, [], [], _, _, rfl, rfl, .nil _, rfl, .nil _⟩
  · rintro ⟨block, fetched, instruction⟩
    have fetched : some _ = some block := fetched
    cases fetched
    obtain ⟨_, caller, rest, values, read, ended, stack, evaluated, released, _, written⟩ :=
      instruction
    have stack : _ :: [] = caller :: rest := stack
    cases stack
    have evaluated : some ([], helperReturning 2) = some (values, read) := evaluated
    cases evaluated
    cases released
    cases written

-- A materialized zero-byte value goes through begin, bind, linear move, and
-- end with empty byte memory throughout. A subsequent move has no value.
example (project : Project) :
    let memory := zeroMemory project
    let moved := {zeroBound with initialized := [], zeroValues := []}
    memory.Step (language := zeroLanguage) storeInitial zeroStarted ∧
    memory.Step (language := zeroLanguage) zeroStarted zeroBound ∧
    memory.readValue (language := zeroLanguage) zeroBound (.move storePlace) = some (.scalar .unit, moved) ∧
    memory.readValue (language := zeroLanguage) moved (.move storePlace) = none ∧
    memory.Step (language := zeroLanguage) moved {storeInitial with instruction := 3} := by
  dsimp only
  refine ⟨?_, ?_, rfl, rfl, ?_⟩
  · exact ⟨_, rfl, {zeroStarted with instruction := 0},
      ⟨by decide, Or.inr ⟨rfl, ⟨0, 1⟩, rfl, Or.inl ⟨rfl, rfl⟩⟩⟩, rfl⟩
  · refine ⟨_, rfl, _, zeroStarted, {zeroBound with instruction := 1}, rfl, ?_, rfl⟩
    exact ⟨⟨rfl, nofun⟩, (fun _ => ⟨⟨0, 1⟩, rfl, by decide⟩), rfl, rfl,
      fun _ => Or.inl rfl⟩
  · exact ⟨_, rfl, {storeInitial with instruction := 2},
      ⟨⟨by decide, rfl⟩, Or.inr ⟨rfl, Or.inl ⟨⟨0, 1⟩, rfl, rfl, rfl⟩⟩⟩, rfl⟩

-- Destructuring a linear field cannot erase it or create two copies.
example :
    Concrete.Interfaces.Memory.DestructureFields splitLanguage [] () (.scalar .unit) [(splitField, ())] ∧
    ¬ Concrete.Interfaces.Memory.DestructureFields splitLanguage [] () (.scalar .unit) [] ∧
    ¬ Concrete.Interfaces.Memory.DestructureFields splitLanguage [] () (.scalar .unit)
      [(splitField, ()), (splitField, ())] := by
  refine ⟨⟨by decide, [splitField], rfl, ?_, ?_⟩, ?_, ?_⟩
  · intro binding h
    cases List.mem_singleton.mp h
    exact ⟨List.mem_cons_self .., rfl⟩
  · intro field member absent
    exact False.elim (absent member)
  · rintro ⟨_, fields, hfields, _, omitted⟩
    have hfields : some [splitField] = some fields := hfields
    cases hfields
    obtain ⟨_, _, copy⟩ := omitted splitField (List.mem_cons_self ..) (by decide)
    contradiction
  · exact fun h => (show ¬ [splitField, splitField].Nodup from by decide) h.1

-- A fresh virtual shadow cannot stand in for the required materialized root.
example (project : Project) (m : LoweringModels zeroLanguage testLowering testProfile) :
    ¬ MemorySimulationWitness m (shadowMIR project) (shadowMemory project) := by
  intro lowers
  obtain ⟨ty, locals, _, _, _, source, member, _⟩ :=
    lowers.2.2.2.2.2.1.2 Exec.Demo.mainF _ shadowRoot
      (List.mem_cons_self ..) (List.mem_cons_of_mem _ (List.mem_cons_self ..))
  have falseKey : ((shadowRoot, ty) : PlaceId × Unit) ∉ [(storeRoot, ())] := by
    cases ty
    decide
  have eq : locals = [(storeRoot, ())] := congrArg (fun item => item.2.1) (List.mem_singleton.mp source)
  exact falseKey (eq ▸ member)

-- Even a proposed semantics that freezes the MIR projection at its entry
-- must realize the reachable binding and the logical value it initializes.
example (project : Project) (m : LoweringModels zeroLanguage testLowering testProfile)
    (p : m.mir.execution.State) (reachable : m.mir.execution.finite (shadowMIR project) p)
    (erased : ∀ q, m.mirState q = storeInitial) :
    ¬ (shadowMIR project).ExecutionExact (language := zeroLanguage) m := by
  intro exact
  let next : MemoryRegisters zeroLanguage testProfile := {zeroBound with
    instruction := 1, zeroValues := [], virtualValues := [(storeRoot, .scalar .unit)]}
  have binding : (shadowMIR project).Step (language := zeroLanguage) storeInitial next := by
    refine ⟨rfl, _, storeInitial, {next with instruction := 0}, rfl, ?_, rfl⟩
    let view := (shadowMIR project).logicalView (language := zeroLanguage)
    let place : MemoryPlace Unit () := ⟨⟨storeRoot, []⟩, .virtual⟩
    with_unfolding_all
      change view.WriteParams (language := zeroLanguage) [⟨(), place⟩]
        [.scalar .unit] storeInitial {next with instruction := 0}
    refine Memory.WriteParams.cons (language := zeroLanguage) (started := {storeInitial with live := [storeRoot]}) ?_ ?_ (.nil _)
    · exact ⟨by decide, Or.inl ⟨rfl, rfl⟩⟩
    · exact ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩
  have projected : (shadowMIR project).Step (language := zeroLanguage) (m.mirState p) next := erased p ▸ binding
  obtain ⟨q, _, hq⟩ := exact.2.2.1 p next reachable projected
  rw [erased] at hq
  exact (show (0 : Nat) ≠ 1 from by decide) (congrArg (fun state : MemoryRegisters zeroLanguage testProfile => state.instruction) hq)

-- A projected write can replace an initialized field. It cannot revive a
-- consumed root, even when the physical bytes of the moved value remain.
example (project : Project) :
    let memory := storingMemory project
    let moved := {storeAfter with initialized := []}
    memory.readValue (language := linearProjectedLanguage) storeAfter (.move storePlace) =
      some (storeValue, moved) ∧
    memory.writeValue (language := linearProjectedLanguage) moved projectedStorePlace storeValue = none ∧
    memory.writeValue (language := projectedLanguage) storeAfter projectedStorePlace storeValue =
      some storeAfter := ⟨rfl, rfl, rfl⟩

-- Both projected reads and writes execute against the initialized zero-byte
-- root. An otherwise identical uninitialized root refuses the write.
example (project : Project) :
    let memory := zeroMemory project
    memory.readValue (language := projectedZeroLanguage) zeroBound (.copy projectedStorePlace) =
      some (.scalar .unit, zeroBound) ∧
    memory.Writes (language := projectedZeroLanguage) zeroBound projectedStorePlace (.scalar .unit) zeroBound ∧
    memory.writeValue (language := projectedZeroLanguage) zeroStarted projectedStorePlace (.scalar .unit) =
      none := by
  exact ⟨rfl, ⟨⟨rfl, fun _ => rfl⟩, (fun _ => ⟨⟨0, 1⟩, rfl, by decide⟩), rfl,
    rfl, fun _ => Or.inl rfl⟩, rfl⟩

example (project : Project) (next : MemoryRegisters zeroLanguage testProfile) :
    ¬ (zeroMemory project).Ends (language := zeroLanguage) zeroBound storeRoot next := by
  intro ends
  have h : false = true := by
    with_unfolding_all exact ends.1.2
  contradiction

-- Two valid placements of the same initialized value provide different
-- pointers to the operation. Substituting the first pointer for the second
-- fails the address-operand premise even though the stored bytes agree.
example (project : Project) :
    let memory := storingMemory project
    let address := ⟨(), storePlace⟩
    memory.resolveAddress (Memory.prepareAddresses storeAfter [address]) false address = some (resolvedStore storeRange) ∧
    memory.resolveAddress (Memory.prepareAddresses alternateBacking [address]) false address = some (resolvedStore alternateRange) ∧
    memory.resolveAddress (Memory.prepareAddresses alternateBacking [address]) false address ≠ some (resolvedStore storeRange) := by
  refine ⟨rfl, rfl, ?_⟩
  intro wrong
  have pointer := congrArg (Option.bind · (fun operand : OperationAddress Unit =>
    operand.range.map (·.ptr.addr))) wrong
  exact (show some (2 : Nat) ≠ some 1 from by decide) pointer

end StageInterfaceControls
