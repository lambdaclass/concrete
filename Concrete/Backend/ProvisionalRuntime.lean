import Concrete.BIR
import Concrete.Backend.Runtime

/-!
# Provisional BIR runtime wrappers

PROVISIONAL (slice 0). `Concrete.BIR.SiteTable`-based wrappers around the frozen
diagnostic-list runtime renderers in `Concrete.Backend.Runtime`. Retired at post-freeze
CLI cutover alongside `Concrete.BIR`.
-/

namespace Concrete.Backend.Runtime
open Concrete.BIR
open Concrete.Backend.LLVM
open ConcreteSpec (Trap)

/-- The trap's name in the trap line. -/
def trapName : Trap → String
  | .arithmeticOverflow => "overflow"

/-- The trap line for one site, without the final newline. -/
def trapLine (e : SiteEntry) : String :=
  s!"concrete: trap: {trapName e.kind} at {e.loc.path}:{decimal e.loc.line}:{decimal e.loc.col} (site {decimal e.site.id})"

/-- The decided form, checked against a literal (not against `trapLine` itself). -/
example : trapLine ⟨.arithmeticOverflow, ⟨9⟩, ⟨40, 47⟩, ⟨"src/main.con", 4, 12⟩⟩ =
    "concrete: trap: overflow at src/main.con:4:12 (site 9)" := by decide

/-- The fixed runtime entry and its byte globals as LLVM assembly items. -/
def llvmItems (tbl : SiteTable) : List Item :=
  llvmDiagnostics (tbl.map fun entry => trapLine entry ++ "\n")

/-- The runtime as standalone LLVM text. -/
def llvm (tbl : SiteTable) : String := render (llvmItems tbl)

/-- The current compiler renders its legacy site table with the same runtime template. -/
def qbe (tbl : SiteTable) : String :=
  qbeDiagnostics (tbl.map fun entry => trapLine entry ++ "\n")

end Concrete.Backend.Runtime
