-- Root of the `DesignExamples` Lake library: machine-checked design examples for the
-- verified-compiler plan (docs/compiler/VERIFIED_PIPELINE.md, "Early semantic risk
-- retirement"; ROADMAP row 3, R-0511). Self-contained: nothing here imports the compiler.
-- `Axioms` pins the axiom footprint of every theorem, so a `sorry` or a new axiom fails
-- the build. See proofs/DesignExamples/README.md.
import DesignExamples.Linear
import DesignExamples.Borrow
import DesignExamples.Cleanup
import DesignExamples.Repr
import DesignExamples.Axioms
