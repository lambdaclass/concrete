Add a Lean-only specification landing inventory to the Spec build job. Reconcile the
original 51 pieces and recorded additions, and report theorem qualification separately.
Check named declarations, authored Laws, and root reachability, and link pending work
to its owning records.

Naming 943 anonymous examples as theorems gives the inventory stable declarations
to check. The authored statements and proofs are byte-identical. Lean elaborates
named theorems under their declaration names, which also changes the names and
population of elaborator-generated auxiliary theorems. The current axiom inventory
includes those auxiliaries. Regenerating it with `scripts/proofs/axioms.sh --update`
adds 943 authored entries and 160 generated entries and removes 24 generated entries.
