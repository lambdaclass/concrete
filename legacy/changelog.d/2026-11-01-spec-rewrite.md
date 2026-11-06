### The specification is rewritten around one formal source (2026-11-01)

The owner decided to rewrite the spec around a small Core, with every surface feature defined as its
rewrite into Core, and to make one Lean definition of Core the source that generates the spec's formal
parts, the reference interpreter, the conformance oracle and the proof definitions, as WebAssembly's
SpecTec does. Spec and implementation then cannot drift apart. It is the most important work in row 0s;
the plan is at the top of `ROADMAP.md`, and the decision is in `docs/project/ROADMAP_DECISIONS.md`.
