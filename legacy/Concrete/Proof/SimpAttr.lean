import Lean

/-! # Scoped simp set for generated proof tables

`register_simp_attr` needs `import Lean` and must live in a module that is
IMPORTED by its users (it runs at `initialize` time), so it gets its own file
rather than pulling all of Lean into `Concrete/Proof/Proof.lean`.
-/

/-- Simp set for GENERATED proof-table lookup lemmas.

    Deliberately a scoped attribute rather than `@[simp]`. A generated table
    emits one lookup lemma PER ENTRY, and putting those in the default simp set
    means every `simp` anywhere in the project pays for — and can be rewritten
    by — machine-generated table facts it never asked about. That is a global
    cost imposed by a local artifact.

    Proofs that want them ask: `simp [proofTable]`. -/
register_simp_attr proofTable
