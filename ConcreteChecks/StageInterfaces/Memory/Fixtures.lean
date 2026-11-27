import ConcreteChecks.StageInterfaces.Fixtures

open Concrete
open Concrete.Interfaces
open ConcreteSpec Core Core.SourcePathSpan StageInterface StageGrammar

namespace StageInterfaceControls

def storeRoot : PlaceId := ⟨0⟩
def storePlace : MemoryPlace Unit () := ⟨⟨storeRoot, []⟩, .materialized⟩
def storeValue : testLanguage.operands.Val := .scalar (.int .u8 42)

def storingMemory (project : Project) : Memory testLowering testProfile :=
  let block : MemoryBlock testLowering := ⟨⟨0⟩, [],
    [⟨testOrigin, .beginLifetime storeRoot⟩, ⟨testOrigin, .bind storePlace (.literal ⟨[42]⟩)⟩],
    .ret none, []⟩
  ⟨project, Exec.Demo.mainF, [], [(Exec.Demo.mainF, [(storeRoot, ())], ⟨⟨0⟩, #[block]⟩)],
    [(Exec.Demo.mainF, [(storeRoot, .scalar 1 1)])], [(Exec.Demo.mainF, [(storeRoot, .materialized)])],
    [(Exec.Demo.mainF, [storeRoot])], []⟩

def storeInitial : MemoryRegisters testLanguage testProfile :=
  ⟨⟨Exec.Demo.mainF, ⟨0⟩, 0, [], [], [], [], [], [], []⟩, Mem.Memory.empty, [], [], {}, 0, false⟩

def storeBlock : Mem.Block :=
  ⟨⟨0⟩, 1, 1, 1, true, false, [.uninit]⟩

def storeRange : Hybrid.Range :=
  ⟨⟨⟨0⟩, 1, some ⟨0⟩⟩, 1, 1⟩

def storeBefore : MemoryRegisters testLanguage testProfile :=
  {storeInitial with
    instruction := 1
    live := [storeRoot]
    materialized := [(storeRoot, storeRange)]
    bytes := ⟨[(⟨0⟩, storeBlock)], 1⟩}

def storeAfter : MemoryRegisters testLanguage testProfile :=
  {storeBefore with
    instruction := 2
    initialized := [storeRoot]
    bytes := ⟨[(⟨0⟩, {storeBlock with bytes := [.init 42 none]})], 1⟩}

example (project : Project) : (storingMemory project).Step (language := testLanguage) storeInitial storeBefore := by
  refine ⟨_, rfl, {storeBefore with instruction := 0}, ⟨by decide, Or.inr ?_⟩, rfl⟩
  exact ⟨rfl, ⟨⟨1, 1, .nil⟩, rfl, Or.inr ⟨by decide, ⟨0⟩, 1, ⟨0⟩, storeBefore.bytes, rfl, rfl⟩⟩⟩

def returningMIR (project : Project) : MIR testLowering testProfile :=
  ⟨project, Exec.Demo.mainF, [(Exec.Demo.mainF, ⟨[], some ()⟩)],
    [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[⟨⟨0⟩, [], [], .ret (some ⟨(), .literal ⟨[42]⟩⟩), []⟩]⟩)], [], []⟩

def returningMemory (project : Project) : Memory testLowering testProfile :=
  ⟨project, Exec.Demo.mainF, (returningMIR project).signatures,
    [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[⟨⟨0⟩, [], [], .ret (some ⟨(), .literal ⟨[0]⟩⟩), []⟩]⟩)],
    [], [], [], []⟩

abbrev zeroLanguage : CoreLanguage :=
  {testLanguage with
    operands := {testLanguage.operands with isCopy := fun _ => false}
    safety := fun _ => []
    layout := fun _ _ _ => some ⟨0, 1, .nil⟩
    scalarType := fun _ => none
    literal := fun _ _ => some (.scalar .unit)}

def zeroMemory (project : Project) : Memory testLowering testProfile :=
  let block : MemoryBlock testLowering := ⟨⟨0⟩, [],
    [⟨testOrigin, .beginLifetime storeRoot⟩, ⟨testOrigin, .bind storePlace (.literal ⟨[]⟩)⟩,
      ⟨testOrigin, .endLifetime storeRoot⟩], .ret none, []⟩
  {(storingMemory project) with
    functions := [(Exec.Demo.mainF, [(storeRoot, ())], ⟨⟨0⟩, #[block]⟩)]
    layouts := [(Exec.Demo.mainF, [(storeRoot, ⟨0, 1, .nil⟩)])]}

def zeroStarted : MemoryRegisters zeroLanguage testProfile :=
  {storeInitial with instruction := 1, live := [storeRoot]}

def zeroBound : MemoryRegisters zeroLanguage testProfile :=
  {zeroStarted with
    instruction := 2
    initialized := [storeRoot]
    zeroValues := [(storeRoot, .scalar .unit)]}

def splitField : FieldId := ⟨.struct ⟨Exec.Demo.mainF.module, ⟨"record"⟩⟩, ⟨"item"⟩⟩

abbrev splitLanguage : CoreLanguage :=
  {zeroLanguage with
    fieldType := fun _ _ => some ()
    activeFields := fun _ _ _ => some [splitField]}

def shadowRoot : PlaceId := ⟨1⟩

def shadowMIR (project : Project) : MIR testLowering testProfile :=
  let block : MIRGraph testLowering := ⟨⟨0⟩, #[⟨⟨0⟩, [],
    [⟨testOrigin, .bind (ty := ()) ⟨storeRoot, []⟩ (.literal ⟨[]⟩)⟩], .ret none, []⟩]⟩
  ⟨project, Exec.Demo.mainF, [], [(Exec.Demo.mainF, [(storeRoot, ())], block)], [], []⟩

def shadowMemory (project : Project) : Memory testLowering testProfile :=
  let place : MemoryPlace Unit () := ⟨⟨shadowRoot, []⟩, .virtual⟩
  let block : MemoryGraph testLowering := ⟨⟨0⟩, #[⟨⟨0⟩, [],
    [⟨testOrigin, .beginLifetime shadowRoot⟩, ⟨testOrigin, .bind place (.literal ⟨[]⟩)⟩],
    .ret none, []⟩]⟩
  {(zeroMemory project) with
    functions := [(Exec.Demo.mainF, [(storeRoot, ()), (shadowRoot, ())], block)]
    storage := [(Exec.Demo.mainF, [(storeRoot, .materialized), (shadowRoot, .virtual)])]}

def projectedStorePlace : MemoryPlace Unit () :=
  ⟨⟨storeRoot, [splitField]⟩, .materialized⟩

abbrev projectedLanguage : CoreLanguage :=
  {testLanguage with
    fieldType := fun _ _ => some ()
    layout := fun _ _ _ => some ⟨1, 1, .cons splitField 0 (.scalar 1 1) .nil⟩}

abbrev linearProjectedLanguage : CoreLanguage :=
  {projectedLanguage with
    operands := {projectedLanguage.operands with isCopy := fun _ => false}
    safety := fun _ => []}

abbrev projectedZeroLanguage : CoreLanguage :=
  {splitLanguage with
    operands := {splitLanguage.operands with isCopy := fun _ => true}
    safety := fun _ => []
    field := fun _ _ => some (.scalar .unit)
    replaceField := fun _ _ value => some value}

def alternateRange : Hybrid.Range :=
  {storeRange with ptr := {storeRange.ptr with addr := 2}}

def alternateBacking : MemoryRegisters testLanguage testProfile :=
  {storeAfter with
    materialized := [(storeRoot, alternateRange)]
    bytes := ⟨[(⟨0⟩, {storeBlock with base := 2, bytes := [.init 42 none]})], 1⟩}

def resolvedStore (range : Hybrid.Range) : OperationAddress Unit :=
  ⟨(), storeRoot, [], some range, ⟨0⟩, .bytes⟩

abbrev erasureLanguage : CoreLanguage :=
  {zeroLanguage with
    operands := testLanguage.operands
    safety := fun _ => []
    layout := fun _ _ _ => some ⟨1, 1, .nil⟩
    literal := fun _ encoding =>
      if encoding.isEmpty then some (.other ()) else some (.scalar (.int .u8 0))
    encodeOther := fun _ _ _ => some [(0, [.init 42 none])]
    decodeOther := fun _ _ bytes =>
      if bytes == [.init 42 none] then some () else none}

def immediateReturn (project : Project) : Memory testLowering testProfile :=
  {(zeroMemory project) with
    signatures := [(Exec.Demo.mainF, ⟨[], some ()⟩)]
    functions := [(Exec.Demo.mainF, [(storeRoot, ())], ⟨⟨0⟩,
      #[⟨⟨0⟩, [], [], .ret (some ⟨(), .literal ⟨[0]⟩⟩), [testOrigin]⟩]⟩)]
    layouts := [(Exec.Demo.mainF, [(storeRoot, ⟨1, 1, .nil⟩)])]}

def erasureSource (project : Project) : MIR testLowering testProfile :=
  {(shadowMIR project) with
    signatures := (immediateReturn project).signatures
    functions := [(Exec.Demo.mainF, [(storeRoot, ())], ⟨⟨0⟩,
      #[⟨⟨0⟩, [], [⟨testOrigin, .bind (ty := ()) ⟨storeRoot, []⟩ (.literal ⟨[]⟩)⟩],
        .ret (some ⟨(), .literal ⟨[0]⟩⟩), []⟩]⟩)]}

def erasureBound : MemoryRegisters erasureLanguage testProfile :=
  {storeInitial with
    instruction := 1
    live := [storeRoot]
    initialized := [storeRoot]
    virtualValues := [(storeRoot, .other ())]}

theorem noInvalidEnding {memory : Memory testLowering testProfile} (wellFormed : memory.TablesWF)
    {fn locals graph} (present : (fn, locals, graph) ∈ memory.functions)
    {block} (inGraph : block ∈ graph.blocks.toList) : block.ending ≠ .invalid := by
  intro invalid
  obtain ⟨_, _, _, signatureMatches⟩ := (wellFormed.2.2.2.2.2.2 fn locals graph present).2.2
  have result := (signatureMatches block inGraph).1
  rw [invalid] at result
  exact result

def backingFits (space : AddrSpace) (base : Nat) : Prop :=
  Mem.Memory.empty.Fits testProfile.memoryTarget (Mem.candidate space base 1 1)

def backingRange (space : AddrSpace) (base : Nat) : Hybrid.Range :=
  ⟨⟨space, base, some ⟨0⟩⟩, 1, 1⟩

def backingBefore (space : AddrSpace) (base : Nat) : MemoryRegisters erasureLanguage testProfile :=
  {storeBefore with
    materialized := [(storeRoot, backingRange space base)]
    bytes := ⟨[(⟨0⟩, Mem.candidate space base 1 1)], 1⟩}

def backingAfter (space : AddrSpace) (base : Nat) (byte : Fin 256 := 42) : MemoryRegisters erasureLanguage testProfile :=
  {backingBefore space base with
    instruction := 2, initialized := [storeRoot]
    bytes := ⟨[(⟨0⟩, {Mem.candidate space base 1 1 with bytes := [.init byte none]})], 1⟩}

end StageInterfaceControls
