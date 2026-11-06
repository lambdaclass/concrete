### Long-term plan changes, no hidden traps and bounded execution (2026-11-06)

`docs/decisions.md` records the owner's five long-term changes: integration before the Core freeze
(stage A2i, R-0571), a bounded LCB pilot (LP0, R-0572), certificate-discharged compiler-execution
trust (explicit execution assumptions wherever a certificate leaves a gap), one modular spec library with a browsable owner view, and adversarial workloads from the
first slices (R-0573). It also records the owner's direction that an accepted program has no hidden
traps (every run-time failure proven impossible at compile time or handled in the code) and that the
compiler reports which functions are bounded; the detailed rules are pending a reviewed proposal and
an owner decision (R-0574), and the recorded trap rules stand until then. `docs/PHILOSOPHY.md` gains
rule 18 and a boundedness sentence in rule 16; the README states the direction and a table of the
languages Concrete learns from.
