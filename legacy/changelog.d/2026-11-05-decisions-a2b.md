### Owner decisions of 2026-11-05 made during A2b

- **docs/spec/decisions.md**: two owner answers given while formalizing Core (A2b). Q6.2 and R4 F12
  are revised: known values propagate field by field through immutable struct construction and
  destructuring, so `let S { a } = S { a: 200u8 }; a + 100` is refused. Q9.1 is confirmed: `copy`
  only for Copy types, `move` only for non-Copy types. Also recorded: the owner-approved oracle
  release `oracle-legacy-compiler-final` (asset digest included) and the owner's confirmation that
  the website goes down at the move.
