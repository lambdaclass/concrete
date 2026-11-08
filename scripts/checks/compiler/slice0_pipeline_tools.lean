import Concrete

/-!
Prints the tool basenames allowed by Concrete.Premises.components. Setting
CONCRETE_TRUSTED_BASE_WITHOUT to a component id makes the removed-entry control use the same registry.
-/

#eval show IO Unit from do
  let without ← IO.getEnv "CONCRETE_TRUSTED_BASE_WITHOUT"
  let components : List Concrete.Premises.Component :=
    Concrete.Premises.components.filter fun c => some c.id != without
  for name in Concrete.Premises.pipelineMatches components do
    IO.println name
