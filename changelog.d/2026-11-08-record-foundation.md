### Owner decisions: the Lean foundation first, contracts C1–C10 (2026-11-07, 2026-11-08)

- `docs/decisions.md` records the owner's answers verbatim. "ok to option b then for freeze the
  foundation>?" (2026-11-07): the shared foundation is written and frozen in the Lean spec before
  slice 1 builds on it, and each later slice adds its rules on top without rewriting it; this
  answers the spec plan's scheduling question S1. "ok all recommended" (2026-11-08): every question
  of the foundation proposal takes its recommendation, with the revisions of Q3.7, Q3.9, Q3.10 and
  OD126 (target profiles), Q1.7 (release-context erasure only with a proof) and Q7.1, Q7.10 and
  Q9.7 (lawful extensible registries) noted at each row, byte columns at the trap line and
  `Secret<T>`'s layout at the information-flow row.
- The approved proposal is `docs/proposals/FOUNDATION.md`, the plan for the spec work from PLAN
  step 3 on; the Lean spec stays the only home of the rules.
- `ROADMAP.md` gains R-0607 to R-0616, one per contract, each with its home in the spec and its
  gate; A2i and the freeze (R-0567) include them.
