import Lean.Elab.Command
import Concrete

/-!
Printer for the slice-0 trusted-base golden (`scripts/checks/slice0_trusted_base.sh`, README gate
9). PROVISIONAL: it stands in for `concrete facts` until the compiler's command line exists.

`lake env lean scripts/checks/slice0_trusted_base.lean` prints `Concrete.Premises.report` of the
headline theorems: every open premise of the registry, each theorem's axioms, and each axiom's type.
-/

open Lean Elab Command Concrete.Premises in
#eval show CommandElabM Unit from do
  for l in ← liftCoreM (report theorems) do
    IO.println l
