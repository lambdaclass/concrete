import Lean.Elab.Command
import Concrete

/-!
Audits every declaration in the Concrete library for `scripts/checks/compiler/slice0_trusted_base.sh`.
The check functions live in Concrete.Premises and are reused unchanged by the planted controls.
-/

open Lean Elab Command Concrete.Premises in
#eval show CommandElabM Unit from do
  let env ← getEnv
  let axiomErrors ← liftCoreM (Concrete.Premises.axiomErrors env)
  let trusted := trustedDeclarations env
  let trustedErrors := trustedDeclarationErrors env
  for error in axiomErrors ++ trustedErrors do
    IO.println s!"FAIL {error}"
  IO.println s!"trusted declarations: {trusted.size}"
  unless axiomErrors.isEmpty && trustedErrors.isEmpty do
    throwError "trusted-base environment audit failed"
