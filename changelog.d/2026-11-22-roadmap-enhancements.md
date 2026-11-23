### Schedule language and compiler enhancements proposal

Add [LANGUAGE_AND_COMPILER_ENHANCEMENTS.md](docs/proposals/LANGUAGE_AND_COMPILER_ENHANCEMENTS.md)
and schedule its full scope in [ROADMAP.md](ROADMAP.md). Extend existing roadmap owners for automatic
`#[variant]` step bounds (`R-0574`), certified SSA/register-allocation validators and
proof-producing e-graphs (`R-0583`, `R-0299`), proof-guided refusal diagnostics and
`Obligation_*.lean` stubs (`R-0593`, `R-0599`, `R-0177`), `concrete trace` over the hybrid Core
evaluator (`R-0586`), two-level declaration Merkle hashing (`R-0556`, `R-0149`), proof-surface
pure-functional Lean extraction (`R-0495`), direct LCB machine code and in-Lean binary stack-bound
validation (`R-0517`, `R-0505`), and earlier `concrete diff` assurance diffing, `Concrete.toml`
explicit composable `[policy]` clauses (no opaque `"strict"` mode; `concrete init --strict` emits
explicit lines, [decision](docs/decisions.md#decided-2026-11-22-explicit-policy-clauses)) with
per-function step/complexity budgets and ratchets (no source cost syntax; `R-0576`, `R-0201`), plus
the one-command `concrete build --audit` user audit dossier (`R-0201`, `R-0353`).
Add proposed tasks `R-0711`–`R-0727` for verified optimizer hints, deterministic bitmask/modulo/shift
checker rules, `&out T`, zero-`unsafe` wire formats (`ByteSafe` and explicit-endian scalars),
erased proof witnesses (`Witness<P>`), const generics and pure compile-time data evaluation,
`concrete sim` deterministic simulation testing (first `Fs`/Entropy leg pulled to 0.1),
scoped scratch arenas with inferred brands, O(1) pure-memory arena reset and brand-preserving loops,
field-disjoint borrows and `borrow mut xs split at mid`, Hylo-style scoped projection functions,
disarmable `defer`/`errdefer` and cleanup/destroy alignment, `Surface0` literal revisions, `const`
arrays, systems/crypto scalars + `volatile_zero` and zero-heap `args`, pre-Slice-0/pre-Slice-3
compiler and spec correctness/scaling fixes (`LLVM`, `Lex`, `LL1`, `BIR`, `LCB`, `Runtime`,
`Hybrid`/`Ident`/`Exec`), the proof/CI velocity overhaul (`ConcreteSpecEvidence` split,
zero-TCB tactics, `axioms.golden`/`landing_inventory` cleanup, CI build-once/attestation reuse, and
proportion rule), read-only `MappedFile` (`mmap`) linear grants + layout-verified C/Metal shader
struct headers, `f16`/`bf16` storage/conversions + explicit fixed-width SIMD vectors, and Phase-E
Verified MAP-Elites kernel and schedule superoptimization. All language/grant rows
(`R-0713`–`R-0716`, `R-0718`–`R-0722`, `R-0725`–`R-0726`) and policy items in `R-0724` remain
explicitly owner-gated and decide no language rule.
