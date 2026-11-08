import Concrete

/-!
Prints the rows of Concrete/README.md's open-premise table from the registry
(`Concrete.Premises.all`, `Premise.tableRow`), for `scripts/checks/slice0_trusted_base.sh`. PROVISIONAL.
-/

#eval do
  for p in Concrete.Premises.all do
    IO.println p.tableRow
