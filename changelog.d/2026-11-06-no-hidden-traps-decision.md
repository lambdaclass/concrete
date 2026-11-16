### No hidden traps and bounded execution decided (2026-11-06)

`docs/decisions.md` turns the 2026-11-06 direction into decided rules: accepted safe code has no
implicit trap; every operation that could fail carries checked evidence (a decidable octagon checker
proved sound in Lean, contracts with closed evidence, Lean theorems; no SMT) or a written failure
branch. Every function has termination evidence with its premises, or a report naming the
explicitly permitted unresolved obligations, each permitted at its source loop, recursive cycle or
boundary, never silently unknown. Per-function execution facts (termination,
recursion depth, stack, abort, allocation, blocking, foreign calls) are reported, `bounded` is
derived, and module or project policies may require them. The revised rows carry revision notes;
`PHILOSOPHY.md` rules 16 and 18, `README.md` and R-0574 follow. Three spellings stay with the owner.
The reviewed proposal and the termination analysis are archived in
`docs/archive/no-hidden-traps-2026-11-06/`.
