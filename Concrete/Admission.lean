import Concrete.Elab

namespace Concrete.Admission

/-- Compose structural elaboration and spec preparation for the pre-cutover driver. -/
def prepareParsed (purpose : ConcreteSpec.StageInterface.SurfacePreparation.Purpose)
    (ast : ConcreteSpec.Surface0.Program) : Except Refusal Elab.Output := do
  let prepared ← (ConcreteSpec.StageInterface.SurfacePreparation.prepareSurface0 ast purpose).mapError Refusal.source
  return Elab.LocatedOutput.erase prepared

/-- The statically known result of an addition, indexed by the Core binder for that operation. -/
structure Token where
  binding : ConcreteSpec.Var
  value : Int
  deriving DecidableEq

structure Output where
  private mk ::
  core : Elab.Output
  tokens : List Token

private def record (acc : ConcreteSpec.Ctx × List Token) (stmt : Core.Stmt) :
    ConcreteSpec.Ctx × List Token :=
  let (ctx, tokens) := acc
  let known := ConcreteSpec.knownExpr ctx stmt.e.toCore
  let tokens := match stmt.e, known with
    | .add .., some (.int .u8 value) => ⟨stmt.x, value⟩ :: tokens
    | _, _ => tokens
  ((stmt.x, { ty := .scalar (.int .u8), live := true, used := false, known }) :: ctx, tokens)

def evidence (p : Core.Program) : List Token := (p.body.foldl record ([], [])).2.reverse

private def refusal (out : Elab.Output) (reason : ConcreteSpec.Admission.Refusal) : Refusal :=
  match (ConcreteSpec.StageInterface.AdmissionView.zero out.located).render reason with
  | some cause => .source cause
  | none => .missingLocation reason

def checkWith (policy : ConcreteSpec.ArithmeticPolicy) (out : Elab.Output) :=
  (ConcreteSpec.checkProgramWith policy (Core.toCore out.program)).mapError (refusal out)

def check (out : Elab.Output) : Except Refusal Output := do
  let _ ← checkWith .static out
  return ⟨out, evidence out.program⟩

theorem check_evidence {src : Elab.Output} {out : Output} (h : check src = .ok out) :
    out.core = src ∧ out.tokens = evidence src.program := by
  cases ran : checkWith .static src <;> simp [check, ran, Bind.bind, Except.bind] at h
  cases h
  exact ⟨rfl, rfl⟩

theorem admit_checked {src : Elab.Output} {out : Output} (h : check src = .ok out) :
    Core.Checked (Core.toCore out.core.program) := by
  rw [(check_evidence h).1]
  apply (ConcreteSpec.checkProgram_iff _).1
  cases ran : ConcreteSpec.checkProgram (Core.toCore src.program) with
  | ok evidence => exact ⟨_, rfl⟩
  | error r => simp [check, checkWith, ConcreteSpec.checkProgram] at h ran
               rw [ran] at h
               cases h

theorem admit_never_traps {src : Elab.Output} {out : Output} (h : check src = .ok out)
    (fuel : Nat) (trap : ConcreteSpec.Trap) :
    Core.run (Core.toCore out.core.program) fuel ≠ .trap trap :=
  Core.slice0_never_traps (admit_checked h) fuel trap

end Concrete.Admission
