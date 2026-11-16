### Slice-0 lexer token components (2026-11-08)

Stacked on the unlanded `wip/slice0-lexer-ws` branch at `e474d8f0`.

- Adds declarative judgments and total scanners for ASCII case-sensitive identifiers, string bodies with the six decided escapes, table-parametric longest match, and decimal integers with leading-zero refusal.
- Proves soundness, completeness and uniqueness, with kernel-checked accepted and refused examples. String-body raw-scalar handling remains an explicit parameter; no keyword, builtin, concrete operator, suffix or other undecided token rule is introduced.
