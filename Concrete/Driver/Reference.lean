import ConcreteSpec.Spec.Slice1Checker
import ConcreteSpec.Spec.Observation

/-! Reference-side driver for the grant-free component. No compiler stage supplies its expected
outcomes. Q8.4–Q8.5 and R-0586. -/

namespace Concrete.Driver.Reference
open ConcreteSpec

/-- A launcher is paired with the component it was admitted against. This is the reference
pairing, with no assertion about a native image. -/
structure Component where
  program : Program
  admitted : check program = true

def Component.ofChecked (program : Program) : Option Component :=
  if h : check program = true then some ⟨program, h⟩ else none

structure Launcher where
  component : Component

structure Run where
  outcome : Outcome
  observation : Option Observe.Observation
  deriving DecidableEq, Repr

/-- The slice has no grant parameters. Its reference runner performs no host I/O.
The driver retains the original outcome so a fuel limit is distinct from a stuck execution. -/
def Launcher.run (launcher : Launcher) (fuel : Nat) : Run :=
  let outcome := ConcreteSpec.run launcher.component.program fuel
  ⟨outcome, Observe.observe true outcome⟩

end Concrete.Driver.Reference
