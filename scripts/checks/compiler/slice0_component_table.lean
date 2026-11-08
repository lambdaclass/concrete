import Concrete

/-!
Prints the rows of Concrete/README.md's trusted-component table from
`Concrete.Premises.components`, for `scripts/checks/compiler/slice0_trusted_base.sh`.
-/

#eval do
  for c in Concrete.Premises.components do
    IO.println c.tableRow
