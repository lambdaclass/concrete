### Schedule language and compiler enhancements proposal

Add [LANGUAGE_AND_COMPILER_ENHANCEMENTS.md](docs/proposals/LANGUAGE_AND_COMPILER_ENHANCEMENTS.md)
and schedule its scope in [ROADMAP.md](ROADMAP.md). Extend existing roadmap owners for automatic
`#[variant]` step bounds (`R-0574`), certified SSA/register-allocation validators and
proof-producing e-graphs (`R-0583`, `R-0299`), proof-guided refusal diagnostics and
`Obligation_*.lean` stubs (`R-0593`, `R-0599`, `R-0177`), `concrete trace` over the hybrid Core
evaluator (`R-0586`), two-level declaration Merkle hashing (`R-0556`, `R-0149`), slice-8
pure-functional Lean extraction (`R-0495`), direct LCB machine code and in-Lean binary stack-bound
validation (`R-0517`, `R-0505`), and earlier `concrete diff` assurance diffing (`R-0201`).
Add proposed tasks `R-0711`–`R-0718` for verified optimizer hints, deterministic bitmask/modulo/shift
checker rules, `&out T`, zero-`unsafe` wire formats (`ByteSafe` and explicit-endian scalars),
erased proof witnesses (`Witness<P>`), const generics and pure compile-time data evaluation,
`concrete sim` deterministic simulation testing, and scoped scratch arenas with inferred brands
and O(1) pure-memory arena reset. All language rows remain explicitly owner-gated and decide no
language rule.
