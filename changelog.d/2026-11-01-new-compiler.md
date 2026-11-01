### A new compiler is written from the Lean spec (2026-11-01)

The owner decided that the current compiler is not migrated: a new compiler is written from the Lean
definition of the language, proof-first, on a `ConcreteSpec` library and a new lowering chain. The
current compiler is to be frozen and tagged `legacy-compiler-final`, used as a differential oracle (and by the
site's browser compiler until replaced), and deleted from main once the first slice works end to end. The first deliverable is the target Core
proposal for the owner. The plan is at the top of `ROADMAP.md`.
