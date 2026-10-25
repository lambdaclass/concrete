### Verified-Pipeline Design Examples Are Machine-Checked (2026-10-20)

`proofs/DesignExamples` holds the reviewed design examples that
[VERIFIED_PIPELINE.md](docs/compiler/VERIFIED_PIPELINE.md) asks for before Core's interfaces
freeze. Each is a small Lean model, a few hundred lines that import nothing from the compiler,
built by `lake build` and CI:

- linear move and destroy (`Linear.lean`, `LinearSound.lean`): for every program the model
  checker accepts, no stuck state, each resource destroyed exactly once at normal and `return`
  exits and at most once on `abort`, and an erasure theorem;
- borrows and reborrowing (`Borrow.lean`): an exclusive borrow passed down to callees, exclusive
  and shared reborrows, and the rejected programs shown to dangle or get stuck;
- structured cleanup (`Cleanup.lean`): LIFO `defer` on every exit, and a static lowering that
  agrees with the scoped meaning on every example;
- typed-value representation in byte and provenance memory (`Repr.lean`).

Every theorem is fully proved; `Axioms.lean` pins each theorem's axiom footprint (at most
`propext`, `Classical.choice` and `Quot.sound`). These are design models, not proofs about the
compiler. The [README](proofs/DesignExamples/README.md) lists the design questions they forced,
each with a recommendation (owner R-0511).
