# N2 round 4 writer handoff

This is writer validation, not an independent review or a full-head PASS.
For the coordinator's reviewers: **do the best long-term thing rather than cut
corners**; preserve the structural checks and exact planted reasons, regardless
of speed, diff size or review rounds.

## Findings

**FIXED — C9 nested discharge false positive (F3).** The exact closed application
at 3e22e6417 Ident:1266 and its planted proposition at :1273 pass the isolated
`vacuity_fixtures.py --focused-c9` control. This imports the unchanged historical
sources, selects the law and two evidence declarations from the shared population,
and executes both normal evidence checks. Its sole allowed failure is the law's
missing sibling-subject collectKeysIn mutant (the exact supplied planted example
mutates pathAtIn). Neither evidence declaration may have any failure/incomplete.
The same control with checker d5331fa56 reproduces instance-false-discharge,
unpaired-planted, planted-proposition and missing-instance exactly.

The deeper cause was traversal of generic well-founded recursion proof machinery
as if its hypothetical branches were actual supplied discharges. Closed helpers
are specialized to actual arguments; open helper bodies are scanned generically
once per declaration with every actual proof/empty-type argument still inspected.
Only a checked refutation of a binder's type excludes its hypothetical body.
Actual inhabitants of refuted types are rejected. A visited expression graph
bounds repeated work, and resource errors still propagate as INCOMPLETE. There is
no decide/name/shape allowlist or proof-nesting cutoff. Exact negative controls
include False, absurd, nomatch, Empty and PEmpty, plus new generic-helper controls
for each empty type. The initial all-transparency-only commit was insufficient;
7ee94b683 supersedes it with the complete structural repair.

**FIXED — C2 projection exhaustion.** On the exact ConstOrderIndependent proof at
9cb0ef7fe4f23ac13745717e2e4854410cbf36d1, checker d5331fa56 exhausts 64 visits
and needs **81** to complete with a larger bound. The revised complete proof DAG
traversal needs **49** distinct visits, so memoization alone fixes this instance
within the original bound. `vacuity_fixtures.py --focused-c2` checks that exact
proof at 64 and 4,096 and requires a complete nonprojection result at both limits.
All constructor proof fields remain visited; no field/law is sampled or skipped.
Production declares 4,096 distinct-expression visits per law and reports
`NOTE projection-cost`; low-budget controls still require exact INCOMPLETE
projection-budget. A 96-wrapper pair completes at 4,096 and is INCOMPLETE at 64.

**FIXED — fixture observations cannot be silently overwritten or treated as
complete when truncated.** `--replay NEW_DIRECTORY` freshly elaborates all four
historical cases and then asserts exact retained identities/diagnostic codes.
The output directory must be new. Report validation rejects missing terminal
verdicts and INCOMPLETE diagnostics; the planted truncated-report control adds
exactly `FAIL report-incomplete c9-new`.

## Before/after evidence and remaining server work

The supplied `/home/unbalancedparen/projects/gates-briefs/NS2-reports` was read-only.
Both before and after this repair, `--reports` returns **1 with exactly two
fixture-diagnostic lines**, both C9-new, because those reports still describe
checker d5331fa56. The new isolated exact C9 control passes; it does not rewrite
or retroactively certify the archived reports. The ten excerpt provenance checks
pass byte-for-byte. C9-old/C6-old partial application rejection and C6-current
non-partial evidence remain enforced by the unchanged exact fixture expectations.

The coordinator must run on concrete-ci-2 at the final checker SHA:

1. `nix develop -c python3 scripts/proofs/vacuity_fixtures.py --replay NEW_DIRECTORY`
   (bound the whole campaign externally). This archives a97e361f1 for C9-old
   Ident+Prim and C6-old Observe2, 3e22e6417 for C9-new Ident+Prim, and 6477c65a7
   for C6-current Observe2+ObserveEvidence. The tool then performs the exact
   provenance and diagnostic assertions; **it must pass before phase 2**.
2. Re-audit c1 ccca4abaa and c2 9cb0ef7fe with the same 17 proof-module scopes as
   their NS2 reports: Authority, Basics, Borrow, Brand, Checker, Cleanup, Const,
   Examples, Exec, Hybrid, Memory, Observe2, Observe, Rep, Scalar, Slice0, Slice1.
   Save the new checker SHA, source hashes, elapsed times and projection-cost
   notes. Confirm C2 has no projection-budget; other failures remain findings.
3. Run the complete daily audit on the intended promotion head and measure its
   completion bound. No whole-module/head completion or timing claim is made
   from the isolated local controls. c1/c2 phase-2 interpretations remain void
   until the fresh server fixture campaign passes.

R-0219 owns N2; the previously recorded complete environment-sharding design
under coordinator-owned R-0691 remains the long-term fallback if full-head cost
requires it. Do not weaken or sample the gate. No IDs were allocated, no OD was
taken, and ConcreteSpec/, ROADMAP.md, other clones and other branches were untouched.
No review tools or pushes were run.

## Validation and commits

All checks ran bounded in `nix develop`:

- Complete N2 controls at 96f6a8e57 (consumer identical to 7ee94b683): **108 PASS,
  0 FAIL**; [captured output](audits/N2-round4-controls.txt). This includes every
  previous negative control and the 96-wrapper completion/exhaustion pair.
- Final 11a05ea55 delta: **13 PASS, 0 FAIL**, covering every affected discharge
  control and the three added hypothetical-domain positives. These results cover
  all 111 current controls across the complete run plus the affected delta; there
  was no redundant full rerun after the isolated empty-domain ordering change.
- Exact C9 evidence control: **PASS** at 7ee94b683 source and again at 11a05ea55;
  both evidence checks clean, sole exact missing-sibling-subject law diagnostic.
- Exact C2 isolated projection control: **PASS** at 96f6a8e57 source, 49 visits at
  both limits. Baseline d5331fa56: INCOMPLETE at 64, complete in 81 visits at 4,096.
- Historical provenance: **10 PASS** at 11a05ea55; supplied NS2 reports: expected
  **rc=1, exactly two C9-new fixture-diagnostic lines**, before and after.
- Planted incomplete-report control: **PASS**, adds exactly
  `FAIL report-incomplete c9-new` to those two unchanged baseline diagnostics.
- workflow_yaml: **3 PASS**; gates_registry and its exact substitution diagnostic
  control: **PASS**; doc_links: **12 PASS**; issues: **5 PASS**. These were repeated
  after the production changes. The final documentation check at 907ad188f also
  passed **12/12**, checking 1,123 links and 893 anchors.

The final empty-domain ordering adjustment at 11a05ea55 passed all 13 affected
controls, including three new positive hypothetical-domain cases and all ten
real-discharge checks. It recognizes empty inductive types before asking for a
Decidable instance, so PEmpty in Prop works without a name-specific exception. Earlier
intermediate code at e7221c7d3 passed all 105 controls, but that result predates the
final discharge repair and is not used as final-code certification.

- 43d05c258 — reduce nested closed discharge proofs semantically (initial slice).
- ccf134781 — cache the complete projection DAG and declare bounds/costs.
- e7221c7d3 — require complete historical fixture reports.
- 7ee94b683 — certify hypothetical discharge contexts and traverse helper graphs.
- 96f6a8e57 — pin exact historical C2 projection completion.
- 11a05ea55 — distinguish hypothetical empty domains from actual supplied impossibilities.
