import ConcreteChecks.StageInterfaces.Fixtures

open Concrete
open Concrete.Interfaces
open ConcreteSpec Core Core.SourcePathSpan StageInterface StageGrammar

namespace StageInterfaceControls

def macLLVM : Profile := ⟨.llvm, .aarch64Macos, 2, 4096⟩

def lcbReturnJob (status : BitVec 32) : LCBInput testProfile :=
  let body : LeanCompilerBackend.Verified.Source.Function :=
    ⟨.word, #[], #[], #[⟨#[], #[], .ret (.literal (.word status))⟩]⟩
  ⟨body, [], #[], (Backend.Runtime.qbeDiagnostics []).toUTF8⟩

def birReturnBlock (alternate : Bool) :
    Block (Sigma (SSAValue Unit)) (LocatedOp (SSAInst testLowering))
      (BIRTerm (Sigma (SSARead Unit testLowering.literal)) (SSARead Unit testLowering.literal ())) :=
  let literal : UInt8 := if alternate then 42 else 0
  ⟨⟨0⟩, [], [], .ret (some ⟨(), .literal ⟨[literal]⟩⟩), []⟩

def birReturn (project : Project) (alternate : Bool) : BIR testLowering testProfile :=
  {(testBIR project (profile := testProfile)) with
    functions := [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[birReturnBlock alternate]⟩)]}

abbrev birExecution := BIR.Execution (language := testLanguage)
  (lang := testLowering) (profile := testProfile)

end StageInterfaceControls
