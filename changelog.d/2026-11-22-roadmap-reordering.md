### Record 2026-11-22 roadmap reordering, LLM inference showcase, and explicit `[policy]` clauses

Record the [2026-11-22 roadmap reordering](docs/decisions.md#decided-2026-11-22-roadmap-reordering),
[local LLM inference showcase / float-order](docs/decisions.md#decided-2026-11-22-llm-inference-showcase-and-float-order),
and [explicit composable `[policy]` clauses](docs/decisions.md#decided-2026-11-22-explicit-policy-clauses)
owner decisions in [docs/decisions.md](docs/decisions.md) and update [ROADMAP.md](ROADMAP.md):

- Keep strict **spec-first per vertical slice** discipline while unblocking Slices 0–5 on an
  unfrozen Core with incremental `C1–C10` foundation integration (completing all `C1–C10` contracts
  and cross-part interaction proofs in `ConcreteSpec` before Core freezes at `A2c` / Slice 11,
  separate from later Slice-12/16 compiler/surface increments).
- Split `B0` into `B0a` (LLVM working spine + Lean oracle + per-lowering proofs) and `B0b`
  (LCB/ASan/fuzzer after `B0a` in parallel with early slices through Slices 2–4 under an explicit
  early-slice staging rule); move `LP0` (`R-0572`) after Slice 5; defer heavy meta-gates (`R-0702`,
  `R-0699`, `R-0577`) and JSON schema freeze (`R-0178`) from per-PR Slice-0 blockers to nightly / 0.1.
- Reorder Phase C (Slices 6–11) so structs, enums/`Result`/`checked_*`, borrows, arrays,
  modules/`const`, generics and traits precede the proof surface and authority (`file-copy`, `grep`,
  0.1 at Slice 11); reorder Phase D (Slices 12–21) so allocation, containers, raw pointers,
  FFI/RocksDB and floats (`f32`/`f64` at Slice 18, adopting the `concrete-llm` CPU & Concrete+Metal
  local inference showcase) precede function-value contracts (Slice 19); move DWARF (`R-0587`) and
  the formatter after 0.1; pause new Phase T governance/meta-tooling (`R-0692`, `R-0693`, remaining
  `R-0710` items) unless a real bug hits `main`.
- Update `R-0576` and `R-0201` for explicit composable `[policy]` clauses (no opaque `"strict"` mode
  word; `concrete init --strict` may emit explicit lines as a template) and `Concrete.toml`
  per-function step/complexity budgets and ratchets from Slice 10 (with no source cost syntax),
  splitting `R-0178` and `R-0201` increment dependencies cleanly.
- Add a top **Quick start: Now, Next, and Owner Decision Batches** orientation summary to
  [ROADMAP.md](ROADMAP.md).
