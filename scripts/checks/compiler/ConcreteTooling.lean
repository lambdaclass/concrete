import Lean.Meta.Basic
import Lean.PrettyPrinter
import Lean.Util.CollectAxioms
import Concrete.Premises

namespace ConcreteTooling

open Concrete.Premises

open Lean in
/-- The trusted-base report: one line per premise, then for each theorem its sorted axioms, then
    each axiom once with its type (so an axiom restated under the same name changes the report). -/
def report (thms : List Name) : CoreM (List String) := do
  let mut lines := all.map Premise.line
  let mut seen : Array Name := #[]
  for n in thms do
    let axs := (← collectAxioms n).qsort Name.lt
    let names := if axs.isEmpty then "" else " " ++ ", ".intercalate (axs.map toString).toList
    lines := lines ++ [s!"theorem {n} axioms:{names}"]
    for a in axs do
      unless seen.contains a do seen := seen.push a
  for a in seen.qsort Name.lt do
    let ty ← Meta.MetaM.run' (PrettyPrinter.ppExpr (← getConstInfo a).type)
    lines := lines ++ [s!"axiom {a} : {toString ty}"]
  return lines

end ConcreteTooling
