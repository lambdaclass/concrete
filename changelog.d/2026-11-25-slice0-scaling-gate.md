Check the scaling of each Slice-0 compiler stage with paired in-process measurements.
The CI report records both input sizes, elapsed times and ratios, with controls for
linear and quadratic growth (R-0550).

Index elaborator names and admission entries, and build LCB numbering and predecessor
indexes once per program. The admission verdict and evidence agree with the list-based
checker and evidence pass, and the indexed LCB adapter agrees with its list model.
MIR indexes source sites and admission tokens by binder. BIR validation indexes
register scopes and predecessors, checks register uniqueness with indexed membership,
and uses an array for branch targets. The Memory lowering borrows the retained MIR input,
and LLVM printing indexes incoming edges and their arguments. Lean checks the equivalence
of the MIR, Memory, BIR, LLVM, and LCB replacements with their list-based definitions.
The elaborator proves lookup agreement. Checked SSA lowering transfers live values and
uses the existing translation validator to check its output.
