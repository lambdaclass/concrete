# N2 round 3

This is a writer handoff, not an independent review or a claim of full-head PASS.
No ConcreteSpec, ROADMAP, or read-only C10 files were changed. No IDs were allocated,
no language decision was taken, no history was rewritten, and nothing was pushed.

For the coordinator's independent reviewers: judge by the best long-term design;
never cut corners for speed, diff size, review rounds, or a CI budget. Preserve
full checks in the scheduled tier if a genuinely cheaper PR tier is needed.

## Historical fixture correction

The [fixture manifest](fixtures/provenance.json) records exact commit IDs, file
hashes, original line intervals and excerpt hashes. `vacuity_fixtures.py` verifies
all ten excerpts against git objects. Its optional `--reports DIRECTORY` requires
exact diagnostic codes at the original retained example identities and matching
`N2 SOURCE-FILE` hashes, not merely a nonzero audit exit.

- C9 old: a97e361f1, Ident:1252 instance and :1257 planted. Partial application;
  both instance-application and planted-proposition must reject it.
- C9 corrected: 3e22e6417, Ident:1265 tag / :1266 example, :1272 tag / :1273
  example. Fully closed application; both evidence checks must pass.
- **C6 correction:** at 6477c65a7, Observe2:427 declares trapLine_injective with
  six arguments. ObserveEvidence:142–166 contains two full six-argument instances
  and their closed planted examples. These must not fail for partial application.
  The carry's partial instance is at **a97e361f1**, Observe2:379–397, and is
  preserved separately as c6-old. There is no partial trapLine instance in
  Observe2 at 6477c65a7. Do not introduce a false FAIL to satisfy that stale premise.

C9's corrected collectKeysIn_sound was reported only as missing-instance because
Ident:1266 was rejected as instance-false-discharge. The law-name report loses the
relationship between that primary diagnostic and its cascade. Instance failures
now include law/case identities, and paired planted evidence explicitly fails
planted-proposition when its required closed instance is rejected.

Both historical C9 reports have the same missing-provenance declaration:
`_private.ConcreteSpec.Proofs.Ident.0.PSigma.casesOn._arg_pusher`. Lean's
`Lean/Elab/PreDefinition/WF/Unfold.lean`, mkMatchArgPusher, realizes a private child
of a matcher/auxiliary recursor; PSigma.casesOn is registered auxiliary recursor
provenance. The checker previously tested only matcher parents. It now also checks
registered auxiliary recursors and recursor parents; it does not exempt suffixes.

## Review leads, independently reproduced

The invalid agy citations were not used as evidence. Focused throwaway controls
were also run against the original checker at 6b40e334.

| Lead | Verdict and evidence |
| --- | --- |
| F2 | **CONFIRMED.** `forall-alias-full-application` incorrectly fails instance-application on 6b40e334, passes after consuming only the declaration's literal telescope. Closed ¬/≠ controls also pass. |
| F3 | **CONFIRMED.** `decide-hypothesis-positive` incorrectly fails instance-false-discharge on 6b40e334. The scan now applies supplied arguments to helper bodies and reduces the selected computation before inspecting it; generic unreachable branches are not supplied discharges. Existing False/Empty/PEmpty/nomatch controls must still reject. |
| F4 | **CONFIRMED.** Both supplied C9 reports name the same generated recursor child. The explicit provenance repair is described above; missing genuinely unauthenticated metadata remains INCOMPLETE. |
| F5 | **CONFIRMED.** `recursive-law-positive` fails projection-law on 6b40e334 and passes with receiver-aware recognition. A projection of recursive computation is distinct from projecting a supplied premise. Ordinary projection controls remain rejecting. |
| F6 | **CONFIRMED.** On 6b40e334 the correct multiple-subject case fails planted-proposition while the sibling-erasure defect passes; revised controls reverse both verdicts. Historical re-elaboration pending. counterexample mutated through sibling subjects without protecting their identities, unlike the instance comparison. All declared subjects are now mutation boundaries and remain protected in planted comparison. `multiple-subjects-positive` and `sibling-subject-erased` exercise both sides. |
| F7, root/open | **CONFIRMED.** `root-and-open-ambiguous` is wrongly accepted by 6b40e334. Use Lean's resolver with source namespace/open context before the private-only fallback. |
| F7, multi-component namespace | **NOT REPRODUCED.** `namespace-components` passes on both original and revised checkers. Existing nested open-in, renaming, hiding and section-boundary controls also pass. No speculative namespace-stack rewrite was made. |

The [131-row historical C9 audit](audits/C9-3e22e6417-round3.tsv) preserves every
FAIL/INCOMPLETE from NS1 with source location, disposition and reason. These are
baseline dispositions; the table explicitly marks historical re-elaboration as
pending, and must not be mistaken for the requested post-fix server report.
Nineteen backticked local variables/expressions violate the final declaration-name
carrier convention; one subject is the inductive Exec.Step rather than a mutable
definition; 35 law/case/subject combinations have no planted tag. Those source tag
and evidence gaps go to the spec writer, without changing the normative spec here.
The other rows are primary gate false positives or their rejected-instance
cascades. Confirm the revised remainder against a fresh server report before
claiming that all residual diagnostics have been classified.

## CI identity and tiers

`gates.json` is the executable registry (none existed in the starting tree).
The required Proof vacuity job executes its PR tier, currently real N2 controls
plus explicitly NOT MIGRATED source coverage. Cumulative migrated source scopes
belong in that registry. The scheduled daily tier executes the full unscoped audit.
The workflow checker plants the former axioms.sh substitution and requires exactly
`FAIL registry-required-invocation proof-vacuity`. The required job cannot silently
run the unrelated axiom gate. The normal workflow registry and YAML checks pass.

## Cost and required server follow-up

The old driver compiled the **entire requested scope into one audit.lean process**;
it did not isolate C1 modules or individual laws. The NS1 timeout alone cannot
identify the expensive law. Repeated source-context parsing and resolution were
uncached; revised code parses each source module once, caches source text and
per-declaration contexts/resolutions, and imports a freshly compiled consumer for
real audits as well as controls. No persistent or stale spec oleans are used.
A private progress trace now survives an audit timeout and names the last 20
stage/declaration boundaries. This makes a remaining slow obligation identifiable.
The existing 120-second per-process bound and 240-second retry remain enforced;
no proof budget or structural check was relaxed.

**NOT YET VERIFIED:** full-head completion within that bound on concrete-ci-2.
Do not promote based on these local controls alone. If caching is insufficient,
the long-term design is an explicit law-to-witness schema (owner row **R-0691**):
build one canonical evidence index over the shared environment population, assign
every authored declaration to deterministic bounded audit shards, retain all
cross-module instance/planted relationships, aggregate only complete shard results,
and fail closed on any missing shard. R-0219 continues to own N2. The coordinator
must record that scope under R-0691; this writer does not edit ROADMAP or allocate IDs.
No sampling, skipped law, or weaker daily/PR interpretation is acceptable.

Requested server runs, with each exact checker commit recorded:

1. Archive a97e361f1 and 3e22e6417; audit Proofs.Ident and Proofs.Prim together.
2. Audit a97e361f1 Proofs.Observe2 (old partial case); audit 6477c65a7 with
   Proofs.Observe2 **and Proofs.ObserveEvidence** (current closed cases). Include
   the evidence module explicitly; otherwise scope selection excludes its examples.
3. Save those reports as c9-old.txt, c9-new.txt, c6-old.txt, c6-current.txt and run
   `python3 scripts/proofs/vacuity_fixtures.py --reports DIRECTORY`.
4. Rerun c1 ccca4abaa with the exact NS1 scopes and capture the progress tail on
   timeout. Also rerun c2 9cb0ef7fe; phase-2 interpretation stays void until the
   historical discrimination checks pass.
5. Run the unscoped full-head daily invocation on the intended promotion candidate,
   measure wall time, and only then establish the server completion bound. A
   timeout is INCOMPLETE and blocks promotion; the writer has not certified cost.

## Local validation record

- d33d5e491: six focused discharge/application regressions PASS, followed by four
  closed-alias and dependent-mutant controls PASS, then five targeted regressions
  PASS. These runs validated the identical working-tree source committed there.
- bd5eff806: workflow_yaml **3 PASS**, gates_registry exact substitution control
  **PASS**, doc_links **12 PASS**, issues **5 PASS**.
- 7ae83335d: cached-context/provenance/multiple-subject selection **8 PASS**;
  original resolver/projection selections **12 PASS** and **7 PASS** before the
  later source cache (identical resolver/projection logic). The final cache run
  includes the ordinary positive, missing/generated provenance, open-in,
  namespace-components, decide and both sibling-subject controls.
- a3eda3a13: ten byte-for-byte historical fixture provenance checks **PASS**;
  paired-partial/provenance/decide/multiple-subject selection **5 PASS** on the
  identical committed working tree. Full historical source re-elaboration and
  report-diagnostic verification remain server work, not certified locally.

A broader controls run was launched before the final source-text cache and
universe substitution refinement; its result must be labelled with that earlier
checker state, rather than attributed to the final checker SHA.

Final complete focused controls were rerun with checker/control code exactly at
**a3eda3a13** (documentation-only changes were present): **100 PASS, 0 FAIL**.
The [verbatim run log](audits/N2-round3-controls.txt) includes the paired partial
control and both multiple-subject controls. The earlier pre-final-cache run was
**99 PASS, 0 FAIL** and is not used as the final code's certification.
After the handoff documentation commit **223cb22c1**, doc_links was rerun:
**12 PASS**, checking 1121 links and 893 anchors. No production checker or control
code changed during the final complete control run. Workflow_yaml, gates_registry,
fixture provenance and issues passed on a3eda3a13; their production inputs are
unchanged in the later documentation commits.

The writer's remaining unverified deliverables are the fresh historical-head
reports/discrimination assertion and the concrete-ci-2 full-head completion bound.
The exact coordinator runs above are required before promotion. No post-fix C9
remainder or full-head PASS is claimed by this local validation record.
