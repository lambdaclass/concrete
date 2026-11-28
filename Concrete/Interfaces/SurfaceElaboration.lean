import ConcreteSpec.StageInterface
import ConcreteSpec.Spec.Surface0
import ConcreteSpec.Spec.SliceGrammar
import Concrete.Refusal

/-! The frozen Slice 0 elaboration used by both the driver and the stage contract. -/
namespace Concrete.Elab
open ConcreteSpec.Surface0
open ConcreteSpec.StageInterface (SyntaxTree)

/-- Interpret the shared grammar's located constructor tree via the pinned
single-pass left-folding Slice 0 decoder, preserving literal spans, statement
spans, and the closing-brace span. -/
def surfaceProgram : SyntaxTree → Option Program :=
  ConcreteSpec.StageGrammar.decodeSurface0

end Concrete.Elab
