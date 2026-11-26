import ConcreteSpec.Spec.Authority
import ConcreteSpec.Spec.Exec
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

/-- Generic replay boundary for Core programs with an environment. A refused startup never
enters the evaluator. Admitted runs preserve events, final state and ending classification. -/
inductive Replay (ops : Core.Exec.Operands) where
  | startupRefused
  | executed (report : Core.Exec.Report ops)

def replay {ops : Core.Exec.Operands} (program : Core.Exec.Program ops)
    (startup : Option (Core.Exec.Config ops)) (oracle : Core.Exec.Oracle ops)
    (fuel : Nat) : Replay ops :=
  match startup with
  | none => .startupRefused
  | some config => .executed (Core.Exec.run program oracle fuel config)

/-- The authority evaluator's startup and trace, parameterized by the configured grant types
and the launcher-minted identities. The caller supplies a permitted program. -/
def replayAuthority (program : Authority.Prog) (_admitted : Authority.ProgOk program)
    (configured : List (Authority.GrantTy × Nat)) (fuel : Nat) :
    Except Observe.Observation Authority.Trace := do
  let some entry := Authority.Prog.find program "main" | throw .startupRefused
  unless configured.map Prod.fst == entry.grants do throw .startupRefused
  return Authority.runMain program fuel (configured.map Prod.snd)

end Concrete.Driver.Reference
