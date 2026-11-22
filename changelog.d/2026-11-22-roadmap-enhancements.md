### Schedule language and compiler enhancements proposal and record 2026-11-22 roadmap reordering

Record the [2026-11-22 roadmap reordering decision](docs/decisions.md#decided-2026-11-22-roadmap-reordering)
and update [ROADMAP.md](ROADMAP.md) accordingly: unblock Slices 0–5 on an unfrozen Core with
just-in-time `C1–C10` contracts and move the `A2c` Core freeze to 0.1 (Slice 11); split `B0` into
`B0a` (working spine) and `B0b` (LCB/ASan/fuzzer in parallel with Slices 2–4); move `LP0` (`R-0572`)
after Slice 5; defer heavy meta-gates (`R-0702`, `R-0699`, `R-0577`) and JSON schema freeze
(`R-0178`) from Slice-0 per-PR blockers to nightly / 0.1; reorder Phase C (Slices 6–11) so structs,
enums/`Result`/`checked_*`, borrows, arrays, modules/`const`, generics and traits precede the proof
surface and authority; reorder Phase D (Slices 12–21) so containers, raw pointers and FFI/RocksDB
precede function-value contracts; move DWARF (`R-0587`) and the formatter after 0.1; and pause new
Phase T governance/meta-tooling unless a real bug hits `main` while running `R-0724` to cut existing
proof and CI drag.

Add [LANGUAGE_AND_COMPILER_ENHANCEMENTS.md](docs/proposals/LANGUAGE_AND_COMPILER_ENHANCEMENTS.md)
and schedule its full scope in [ROADMAP.md](ROADMAP.md). Extend existing roadmap owners for automatic
`#[variant]` step bounds (`R-0574`), certified SSA/register-allocation validators and
proof-producing e-graphs (`R-0583`, `R-0299`), proof-guided refusal diagnostics and
`Obligation_*.lean` stubs (`R-0593`, `R-0599`, `R-0177`), `concrete trace` over the hybrid Core
evaluator (`R-0586`), two-level declaration Merkle hashing (`R-0556`, `R-0149`), proof-surface
pure-functional Lean extraction (`R-0495`), direct LCB machine code and in-Lean binary stack-bound
validation (`R-0517`, `R-0505`), and earlier `concrete diff` assurance diffing plus the one-command
`concrete build --audit` user audit dossier (`R-0201`, `R-0353`).
Add proposed tasks `R-0711`–`R-0724` for verified optimizer hints, deterministic bitmask/modulo/shift
checker rules, `&out T`, zero-`unsafe` wire formats (`ByteSafe` and explicit-endian scalars),
erased proof witnesses (`Witness<P>`), const generics and pure compile-time data evaluation,
`concrete sim` deterministic simulation testing (first `Fs`/Time/Entropy leg pulled to 0.1),
scoped scratch arenas with inferred brands, O(1) pure-memory arena reset and brand-preserving loops,
field-disjoint borrows and `borrow mut xs split at mid`, Hylo-style scoped projection functions,
disarmable `defer`/`errdefer` and cleanup/destroy alignment, `Surface0` literal revisions, `const`
arrays, systems/crypto scalars + `volatile_zero` and zero-heap `args`, pre-Slice-0/pre-Slice-3
compiler and spec correctness/scaling fixes (`LLVM`, `Lex`, `LL1`, `BIR`, `LCB`, `Runtime`,
`Hybrid`/`Ident`/`Exec`), and the proof/CI velocity overhaul (`ConcreteSpecEvidence` split,
zero-TCB tactics, `axioms.golden`/`landing_inventory` cleanup, CI build-once/attestation reuse, and
proportion rule). All language rows (`R-0713`–`R-0716`, `R-0718`–`R-0722`) and policy items in
`R-0724` remain explicitly owner-gated and decide no language rule.
