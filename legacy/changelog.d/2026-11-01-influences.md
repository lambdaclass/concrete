### Influences and references for the new compiler (2026-11-01)

`docs/project/INFLUENCES.md` gains references for Hylo, Austral and CompCert, records Concrete's
authority deviation from Austral, and adds the verification and compiler influences of the 2026-11-01
research: Aeneas, Effekt, Scala's System Capless, Linear Core, Owlang, CompCertO, CakeML and Pancake,
Verus, WebAssembly SpecTec, Vermillion, Lean's `Std.Do`/`mvcgen`/`grind`, equality saturation and Axon.
ROADMAP's proof order folds them in: functional models simplified by second-class references (their
exactness a theorem to prove), contracts planned to be discharged with Lean's own tools instead of a
project VC generator, a generated LL(1) parser with its remaining obligations named, Capless as a
metatheory reference, the Owlang safety split, a later e-graph optimizer with per-result proofs, and
kernel-checked agent proofs. The README links the list.
