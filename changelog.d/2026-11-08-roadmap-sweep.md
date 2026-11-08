### Test gates, spec and process improvements, and the question sweep (2026-11-08)

`docs/decisions.md` records two owner decisions:
- **Four test gates** ("do this with kimi, codex and agy and review it" and "we should have ci or
  gates for scaling, determinism, comparison in the spec and the compiler"): the seeded
  spec-vs-compiler comparison, one test manifest and runner, scaling and determinism, and the
  maintained trusted-base list. The back-end-first comparison and its fixed full-path hook were
  approved with "ok wuith this but we have to add it to the roadmap or a slice" and "ok let's make
  sure the roadmap has all this and the slices" (R-0632–R-0635). Showcase sketches before the
  freeze were not included in that approval.
- **Six spec and process improvements** ("yes to 6 things"): law-vacuity controls, a
  property-checked axioms list, continuous foundation integration, frozen process tooling and
  independent landings, bounded watchers for background jobs, and a Codex+Kimi+agy round before
  every owner question (R-0636–R-0641).

ROADMAP also records the ISS-0019 review-provenance and ISS-0020 sign-off verifier repairs
(R-0642, R-0643), the draft slice-1 brief and its owner-question gate (R-0644), the proposed std
coverage placements (R-0645), and the CLI question audit's placements (R-0646): C9 identities before
the schema freeze, B0 provenance and printers, slice-8 proof exports and binding, construct-local
facts views, the B0/B1 debug bundle and build record, and six questions awaiting three-model review.
Every slice grows the comparison, manifest and trusted-base list; slice 1 requires the full
spec-vs-native comparison.

The foundation state for C1 (including its lifecycle change), C2, C4 and C6 is: reviewed;
reopened by the 2026-11-08 vacuity audit, one fix round each (C1, C6, C2, C4) on the integrated
spec-core. C7 was accepted before the vacuity rule and is audited next. The C1 lifecycle change's
owner-confirmation question stays open, and the A2c freeze gate is unchanged.
The lexer inventory and its six spelling blockers join R-0620 and R-0625. R-0600 stays in both
rows while its slice is an owner question. Slice 26 leaves NaN semantics open. A new open
owner-question register points to the foundation, std, slice-1, lexer, R0600 and NaN sources;
recommendations remain labelled as recommendations.

Review fixes on 2026-11-08 align the conversion/output revisions, writer and push rules, landing services, roadmap owners and revision notes, and record ISS-0020 as filed and fixed on `wip/signoff-reject`.
