### The specification, laid out for reading (A2c)

`ConcreteSpec/Spec.lean` is now the one normative file of row Q9.15 with the rules and their
examples only; the proofs moved to `ConcreteSpec/Proofs/` (one file per part), and
`ConcreteSpec/README.md` is the reading guide and the list of what is not yet formalized. Every
theorem keeps its name and statement (97, same axioms). The slice-1 examples are stated as
`check p = true` / `check p = false`, which `check_iff` makes exactly `Checked p` / `¬ Checked p`.
`autoImplicit` is off for `ConcreteSpec`. Still PROPOSED until the owner freezes Core.
