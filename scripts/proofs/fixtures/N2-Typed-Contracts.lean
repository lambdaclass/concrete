-- Confirmed evalArgs contracts: inferred/restated positives and concrete-result counterfeit.
namespace Confirm
open ConcreteSpec.Core ConcreteSpec.Core.Own ConcreteSpec.Core.Own.Examples
-- Infer the exact proposition of a term, without evaluating the semantic subject.
open Lean.Elab.Term in
elab "typeOf% " t:term : term => do
  inferType (← elabTerm t none)

-- A closed, inferred-type application, without restating a match.
def inferredOk : typeOf% (@evalArgs_ok_erasure erasedOps ConcreteSpec.Core.Own.Examples.types permitE [(x, 7)] [.read x, .move x] ⟨0, initial⟩) :=
  @evalArgs_ok_erasure erasedOps ConcreteSpec.Core.Own.Examples.types permitE [(x, 7)] [.read x, .move x] ⟨0, initial⟩
def inferredTrap : typeOf% (@evalArgs_trap_erasure erasedOps ConcreteSpec.Core.Own.Examples.types permitE [(x, 7)] [.move x, .fail .overflow] ⟨0, initial⟩) :=
  @evalArgs_trap_erasure erasedOps ConcreteSpec.Core.Own.Examples.types permitE [(x, 7)] [.move x, .fail .overflow] ⟨0, initial⟩

def restatedOk : (match Exec.evalArgs (instrumentOperands erasedOps ConcreteSpec.Core.Own.Examples.types permitE) [(x, 7)] ⟨0, initial⟩ [.read x, .move x] with
    | .ok (vs, t) => Exec.evalArgs erasedOps [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) [.read x, .move x] = .ok (vs, erase t)
    | _ => True) :=
  @evalArgs_ok_erasure erasedOps ConcreteSpec.Core.Own.Examples.types permitE [(x, 7)] [.read x, .move x] ⟨0, initial⟩


-- Negative: same match shape, but the success contract discards the returned store.
def changedStoreOk : (match Exec.evalArgs (instrumentOperands erasedOps ConcreteSpec.Core.Own.Examples.types permitE) [(x, 7)] ⟨0, initial⟩ [.read x, .move x] with
    | .ok (vs, t) => Exec.evalArgs erasedOps [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) [.read x, .move x] = .ok (vs, 0)
    | _ => True) :=
  @evalArgs_ok_erasure erasedOps ConcreteSpec.Core.Own.Examples.types permitE [(x, 7)] [.read x, .move x] ⟨0, initial⟩


def restatedTrap : (match Exec.evalArgs (instrumentOperands erasedOps ConcreteSpec.Core.Own.Examples.types permitE) [(x, 7)] ⟨0, initial⟩ [.move x, .fail .overflow] with
    | .trap kd => Exec.evalArgs erasedOps [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) [.move x, .fail .overflow] = .trap kd
    | _ => True) :=
  @evalArgs_trap_erasure erasedOps ConcreteSpec.Core.Own.Examples.types permitE [(x, 7)] [.move x, .fail .overflow] ⟨0, initial⟩



def unselectedRestatedOk : (match Exec.evalArgs (instrumentOperands erasedOps ConcreteSpec.Core.Own.Examples.types permitE) [(x, 7)] ⟨0, initial⟩ [.fail .overflow] with
    | .ok (vs, t) => Exec.evalArgs erasedOps [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) [.fail .overflow] = .ok (vs, erase t)
    | _ => True) :=
  @evalArgs_ok_erasure erasedOps ConcreteSpec.Core.Own.Examples.types permitE [(x, 7)] [.fail .overflow] ⟨0, initial⟩



def unselectedChangedStoreOk : (match Exec.evalArgs (instrumentOperands erasedOps ConcreteSpec.Core.Own.Examples.types permitE) [(x, 7)] ⟨0, initial⟩ [.fail .overflow] with
    | .ok (vs, t) => Exec.evalArgs erasedOps [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) [.fail .overflow] = .ok (vs, 0)
    | _ => True) :=
  @evalArgs_ok_erasure erasedOps ConcreteSpec.Core.Own.Examples.types permitE [(x, 7)] [.fail .overflow] ⟨0, initial⟩



end Confirm
