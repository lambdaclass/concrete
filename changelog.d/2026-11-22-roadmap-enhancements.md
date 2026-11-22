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
Phase T meta-tooling unless a real bug hits `main`.

Add [LANGUAGE_AND_COMPILER_ENHANCEMENTS.md](docs/proposals/LANGUAGE_AND_COMPILER_ENHANCEMENTS.md)
and schedule its scope in [ROADMAP.md](ROADMAP.md). Extend existing roadmap owners for automatic
`#[variant]` step bounds (`R-0574`), certified SSA/register-allocation validators and
proof-producing e-graphs (`R-0583`, `R-0299`), proof-guided refusal diagnostics and
`Obligation_*.lean` stubs (`R-0593`, `R-0599`, `R-0177`), `concrete trace` over the hybrid Core
evaluator (`R-0586`), two-level declaration Merkle hashing (`R-0556`, `R-0149`), proof-surface
pure-functional Lean extraction (`R-0495`), direct LCB machine code and in-Lean binary stack-bound
validation (`R-0517`, `R-0505`), and earlier `concrete diff` assurance diffing (`R-0201`).
Add proposed tasks `R-0711`–`R-0718` for verified optimizer hints, deterministic bitmask/modulo/shift
checker rules, `&out T`, zero-`unsafe` wire formats (`ByteSafe` and explicit-endian scalars),
erased proof witnesses (`Witness<P>`), const generics and pure compile-time data evaluation,
`concrete sim` deterministic simulation testing (first `Fs`/Time/Entropy leg pulled to 0.1), and
scoped scratch arenas with inferred brands and O(1) pure-memory arena reset. All language rows
(`R-0713`–`R-0716`, `R-0718`) remain explicitly owner-gated and decide no language rule.
