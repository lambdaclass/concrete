Check the scaling of each Slice-0 compiler stage with paired in-process measurements.
The CI report records both input sizes, elapsed times and ratios, with controls for
linear and quadratic growth (R-0550).

Index elaborator names and admission entries, and build LCB numbering and predecessor
indexes once per program. The admission verdict and evidence agree with the list-based
checker and evidence pass, and the indexed LCB adapter agrees with its list model.
MIR and SSA/BIR remain subject to the same scaling limit and still block the gate.
