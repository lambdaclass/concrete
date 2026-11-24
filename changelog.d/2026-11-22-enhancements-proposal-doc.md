### Add language and compiler enhancements proposal document

Add [LANGUAGE_AND_COMPILER_ENHANCEMENTS.md](docs/proposals/LANGUAGE_AND_COMPILER_ENHANCEMENTS.md),
recording the detailed design proposals, philosophy evaluations, and required owner questions for
proposed tasks `R-0711`–`R-0727` and extensions to existing compiler, proof, and tooling rows.

- Covers language/systems-model proposals (`&out T`, `ByteSafe` + explicit-endian wire formats,
  erased `Witness<P>`, const generics + pure compile-time data eval, allocator ergonomics,
  field-disjoint borrows + slice split, scoped projections, disarmable `defer`/`errdefer`, surface
  literals/scalars/const arrays/zero-heap `args`, `MappedFile` + shader headers, `f16`/`bf16` +
  fixed-width SIMD), compiler/prover/tooling proposals (verified optimizer hints, deterministic
  bitmask/modulo/shift transfer rules, `concrete sim`, automatic `#[variant]` iteration/depth
  bounds and composed step bounds, safe-subset `concrete extract-lean`, translation-validated
  passes/e-graphs, direct LCB machine code + in-Lean binary stack bounds, two-level Merkle hashing,
  `concrete diff`/audit dossier, proof-guided diagnostics/`concrete trace`, and Verified
  MAP-Elites), plus pre-Slice-0/pre-Slice-3 compiler/spec fixes and the proof/CI velocity overhaul.
- Decides no language rule and alters no roadmap table rows; scheduling `R-0711`–`R-0727` and
  updating cross-links in [ROADMAP.md](ROADMAP.md) and [docs/decisions.md](docs/decisions.md)
  follow in the companion roadmap-tasks branch.
