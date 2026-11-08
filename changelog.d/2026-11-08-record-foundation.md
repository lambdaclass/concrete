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
- `docs/proposals/SPEC_UNIFICATION.md`, the spec's step plan, is reordered for the foundation: steps
  3–19 write C1–C10 in dependency order on top of `Ident` and `Rules` (target and types, scalars,
  layout and values, primitive contracts, syntax and front end, admission; then observations, the
  hybrid store, the step relation, the evaluator, borrows, brands and authority, leakage, frames and
  evidence with erasure), with every old feature fragment ported there; steps 20–23 close and
  freeze the foundation; the slices follow on top of it from step 24.

### Owner decisions: eleven plan and process decisions (2026-11-08)

- "ok go ahead with all the decisions": UBSan leaves the slice-0 gates (it checks nothing on LLVM IR
  input; ASan stays); the BIR → LCB-IR adapter theorem is complete (an error produces no artifact,
  success implies correctness); the landing scripts shrink (R-0617); Claude's quota goes to the
  critical path; the lexer and LL(1) parser start now (R-0620); one writer for `docs/decisions.md`
  and `ROADMAP.md`; review rounds are capped by risk, 2, 3 and 5 (R-0618); one clones folder
  (R-0619); a no-libc runtime around slices 24–25 (R-0621); a report-only compile-speed budget
  (R-0550); and the ISS-0014 F7 termination wording, applied in `docs/decisions.md`, `README.md`
  and `docs/PHILOSOPHY.md` (ISS-0014 closed). The process rules are in `AGENTS.md`.
