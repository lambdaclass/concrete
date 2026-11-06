### Core statement, facts report and query, outside readers, predictable policy, reproducible builds (2026-11-06)

`docs/decisions.md` records six owner decisions:
- **The core statement:** Concrete is a fully proven systems programming language with no garbage collector, where you can know everything a program does. It now opens README.md and PHILOSOPHY.md.
- **The per-function facts report and query:** a slice-0 deliverable, new task R-0575 in B0. From slice 0 the compiler answers what a function may do, with evidence.
- **The outside readers for R-0559:** the owner, agy, Codex, and Kimi when available, with the models reading first.
- **The predictable policy:** an optional module or project policy (no allocation, no blocking, no foreign calls, plus termination and stack bound where demanded), refused at compile time where it is on. New task R-0576, in slice 1.
- **Reproducible builds:** checked in CI from slice 1. New task R-0577, moved up from phase G.
- **The three no-hidden-traps spellings are decided:**
  - `checked_*` operations returning `Result<T, ArithmeticError>`;
  - `#[proof(path)]` for linking Lean theorems, with roles derived from the theorem's statement;
  - `#[may_not_terminate("reason")]`, which may also prefix an indirect call. This extends OD82 with one LL(1) attribute-prefix production.
