import ConcreteChecks.StageInterfaces.BackendFixtures
import ConcreteChecks.StageInterfaces.Frontend.Elaboration

open Concrete
open Concrete.Interfaces
open ConcreteSpec Core Core.SourcePathSpan StageInterface StageGrammar

namespace StageInterfaceControls

abbrev returnLowering := sliceLoweringLanguage slice0

def returnType : earlyFixtureLanguage.operands.Ty := .scalar (.int .u8)

def singleBlockGraph {P I T : Type} (ending : T) (origins : List Origin) : CFG P I T :=
  ⟨⟨0⟩, #[⟨⟨0⟩, [], [], ending, origins⟩]⟩

private def lowerLiteralReturn (core : CoreInput earlyFixtureLanguage) :
    Except StageFailure (MIR returnLowering testProfile) :=
  match core.machineProgram.operations with
  | [(site, .expression (.read (.literal (.int .u8 value))))] =>
    match AList.lookup core.machineProgram.origins site with
    | some origin =>
      let graph : MIRGraph returnLowering :=
        singleBlockGraph (.ret (some ⟨returnType, .literal ⟨[UInt8.ofNat value.toNat]⟩⟩)) [origin]
      .ok ⟨core.project, core.machineProgram.entry, [(core.machineProgram.entry, ⟨[], some returnType⟩)],
        [(core.machineProgram.entry, [], graph)], [], []⟩
    | none => .error (.invalidInput rule)
  | _ => .error (.invalidInput rule)

private def memoryLiteral (memory : Memory returnLowering testProfile) :
    Except StageFailure (List UInt8 × List Origin) :=
  match AList.lookup memory.functions memory.entry with
  | some (_, ⟨_, #[block]⟩) =>
    match block.ending with
    | .ret (some ⟨_, .literal value⟩) => .ok (value.encoding, block.boundaryOrigins)
    | _ => .error (.invalidInput rule)
  | _ => .error (.invalidInput rule)

private def ssaLiteral (ssa : SSA returnLowering testProfile) :
    Except StageFailure (List UInt8 × List Origin) :=
  memoryLiteral (ssa.memoryView (language := earlyFixtureLanguage))

def literalReturnPasses : Passes slice0 testProfile :=
  { returnPasses fixtureElaborate with
    checkCore := ConcreteSpec.Admission.checkInput
    lowerCore := fun input => lowerLiteralReturn input.core
    lowerUnchecked := lowerLiteralReturn
    lowerMemory := fun mir => .ok (mir.logicalView (language := earlyFixtureLanguage))
    buildSSA := fun memory => do
      let (bytes, origins) ← memoryLiteral memory
      let graph : SSAGraph returnLowering :=
        singleBlockGraph (.ret (some ⟨returnType, .literal ⟨bytes⟩⟩)) origins
      return ⟨memory.project, memory.entry, memory.signatures, [(memory.entry, [], graph)], memory.layouts, [], memory.declarations⟩
    buildBIR := fun ssa => do
      let (bytes, origins) ← ssaLiteral ssa
      let graph : BIRGraph returnLowering :=
        singleBlockGraph (.ret (some ⟨returnType, .literal ⟨bytes⟩⟩)) origins
      return ⟨ssa.project, ssa.entry, ssa.signatures, [(ssa.entry, [], graph)], ssa.layouts, #[], [], ssa.declarations⟩
    emit := fun bir => do
      let (bytes, _) ← memoryLiteral (bir.memoryView (language := earlyFixtureLanguage))
      match bytes with
      | [status] => return lcbReturnJob (BitVec.ofNat 32 status.toNat)
      | _ => throw (.invalidInput rule) }

end StageInterfaceControls
