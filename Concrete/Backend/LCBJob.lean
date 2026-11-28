import LeanCompilerBackend.Verified.Job

/-!
# Pure LCB function-job leaf

Reusable symbol and `FunctionJob` constructor for the verified LCB entry path,
with no dependency on provisional `Concrete.BIR`.
-/

namespace Concrete.Backend.LCBJob

/-- The body's symbol: the runtime entry calls `concrete_body`. -/
def bodySymbol : LeanCompilerBackend.Verified.Symbol :=
  { namespaceName := "concrete", name := "concrete_body", exported := true }

/-- Build a verified LCB `FunctionJob` with the canonical body symbol, ABI and frame plans. -/
def lcbFunctionJob (f : LeanCompilerBackend.Verified.Source.Function) :
    LeanCompilerBackend.Verified.FunctionJob :=
  { code := { symbol := bodySymbol, body := f, abi := LeanCompilerBackend.Verified.planAbi f,
              layout := LeanCompilerBackend.Verified.planFrame f } }

end Concrete.Backend.LCBJob
