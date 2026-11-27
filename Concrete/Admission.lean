import Concrete.Elab

namespace Concrete.Admission

/-- Compose structural elaboration and spec preparation for the pre-cutover driver. -/
def prepareParsed (ast : ConcreteSpec.Surface0.Program)
    (purpose : ConcreteSpec.StageInterface.SurfacePreparation.Purpose := .admission) : Except Refusal Elab.Output := do
  let prepared ← (ConcreteSpec.StageInterface.SurfacePreparation.prepareSurface0 ast purpose).mapError fun
    | .unexpectedEntry span => .unsupported s!"expected the entry point 'main' at byte {span.start}"
    | .wildcard span => .unsupported s!"wildcard binding at byte {span.start}"
    | .fallthrough _ => .checker "reachable end of a u8 function (STMT-9)"
    | .unreachable span =>
      .checker s!"unreachable code (STMT-14) at {span.line}:{span.column}, bytes {span.start}..{span.stop}"
    | .integerSuffix span => .checker s!"integer suffix in an expected-type position at byte {span.start}"
    | .unknownLocal name span => .checker s!"unknown local '{name}' at byte {span.start}"
    | .duplicateBinder name span | .reservedBinder name span =>
      .checker s!"binding shadows '{name}' at byte {span.start}"
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

def check (out : Elab.Output) : Except Refusal Output :=
  if Core.check (Core.toCore out.program) then .ok ⟨out, evidence out.program⟩
  else .error (.checker "Core check rejected the program")

theorem admit_checked {src : Elab.Output} {out : Output} (h : check src = .ok out) :
    Core.Checked (Core.toCore out.core.program) := by
  unfold check at h
  split at h
  · cases h
    exact (Core.check_iff _).mp (by assumption)
  · contradiction

theorem check_evidence {src : Elab.Output} {out : Output} (h : check src = .ok out) :
    out.core = src ∧ out.tokens = evidence src.program := by
  unfold check at h
  split at h
  · cases h
    exact ⟨rfl, rfl⟩
  · contradiction

theorem admit_never_traps {src : Elab.Output} {out : Output} (h : check src = .ok out)
    (fuel : Nat) (trap : ConcreteSpec.Trap) :
    Core.run (Core.toCore out.core.program) fuel ≠ .trap trap :=
  Core.slice0_never_traps (admit_checked h) fuel trap

end Concrete.Admission
