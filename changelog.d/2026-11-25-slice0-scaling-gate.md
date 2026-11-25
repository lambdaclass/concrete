Check the scaling of each Slice-0 compiler stage with paired in-process measurements.
The CI report records both input sizes, elapsed times and ratios, with controls for
linear and quadratic growth (R-0550).

Index elaborator names and admission entries, and build LCB numbering and predecessor
indexes once per program. The admission verdict and evidence agree with the list-based
checker and evidence pass, and the indexed LCB adapter agrees with its list model.
MIR indexes source sites and admission tokens by binder. BIR validation indexes
register scopes and predecessors, checks register uniqueness with indexed membership,
and uses an array for branch targets. Lean checks the equality of each optimized pass
with its list-based definition before using the optimized implementation in compiled code.
