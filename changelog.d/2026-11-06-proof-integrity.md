### Proof-integrity checks for the specification

Four required CI jobs now check ConcreteSpec's proofs, each a script under `scripts/proofs/` with its
own negative controls:

- **Proof axioms** (`axioms.sh`): every theorem constant, private and generated ones included, has
  transitive axioms among propext, Quot.sound and Classical.choice, and the per-theorem inventory
  equals the golden list `scripts/proofs/axioms.golden` (regenerated with `--update` in the commit that changes it).
- **Proof trust ban** (`trust_ban.sh`): no constant of any kind depends on `sorryAx`,
  `Lean.ofReduceBool`/`ofReduceNat`, `Lean.trustCompiler` or a generated native axiom; the library
  declares no axiom; every `example` (which Lean checks and then discards) depends only on the
  standard axioms, checked by building a copy of the library in which each example is a named
  declaration; and, as defence in depth, the library builds with warnings as errors and the sources
  (lexed by `scripts/proofs/lean_lex.py`, the one Lean lexer, also used by the owner view) never
  spell `sorry`, `native_decide` or the like outside comments, set no option, run no meta code,
  define no syntax and import nothing outside the library.
- **Kernel replay** (`kernel_replay.sh`): `leanchecker --fresh ConcreteSpec` replays the whole
  closure into an empty kernel environment, and `leanchecker ConcreteSpec` module by module; a
  declaration forged with `debug.skipKernelTC` is the control.
- **External re-check** (`external.sh`): the theorems exported with lean4export and re-checked by
  con-leche (`--verified --jobs=1`) and nanoda at pinned revisions. Every exported declaration (the
  theorems and every definition, inductive type and Lean core declaration they depend on) must equal
  Lean's own, so a stream cannot keep a statement while changing what it means; corrupted exports (a
  swapped proof, a cut stream, a missing record, a replaced definition body) are the controls. It runs in the new `nix develop .#extcheck` shell, which adds cargo for nanoda.

The 19 retired-job rows of `required-checks-transition.tsv` are removed: main no longer requires
those jobs, so `train.sh` reported each as stale.
