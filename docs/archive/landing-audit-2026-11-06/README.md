# Landings that skipped the review leg, 2026-11-06 (audit)

Status: audit record (ISS-0012). History is not rewritten; every result here is bound to an exact SHA.

On 2026-11-06 three decision-recording trains landed after `train.sh check --no-review`. The automated
review leg refused decision recording (ISS-0011). Its `check` recorded a pass without a review note, and
`land` read no review record (ISS-0009). Each train had a separate manual Codex review, written down
only in its local train notes. The landing machinery now fails closed (`train.sh` review provenance,
landed at 4de277fe; dispatch CI run 42255918182, main run 42256339823, both green). This record checks, for each train,
whether that manual review covered the exact landed diff. It adds a retrospective review of each one.

The per-train logs are copies of the local landing logs (`<git-common-dir>/concrete-landing/log/`):
[docs-2026-11-06.tsv](docs-2026-11-06.tsv), [nht-decision.tsv](nht-decision.tsv),
[facts-decision.tsv](facts-decision.tsv) and, for the fix, [flm.tsv](flm.tsv).

## Contemporaneous review: did it cover the exact landed diff?

| train | landed diff (base..head) | CI (dispatch / main) | manual Codex review | covers the landed diff |
| --- | --- | --- | --- | --- |
| docs-2026-11-06 | 28889fbd3a..ac8e50404b | 42156910326 / 42157227191 green | round 1 NOT YET (3 findings), round 2 ACCEPT | yes: the reviewed `BATCH.diff` has the same content as the landed diff (blob-id abbreviation only), and round 1's files plus round 2's `DELTA.diff` equal the final files byte for byte. Round 1 focused on the new part; the first two commits were stated to replay a diff accepted earlier on wip/long-term-2026-11-06 (bf47b5cae, b172171ca), not re-verified here |
| nht-decision | ac8e50404b..d20e3ab333 | 42166696652 / 42167006263 green | round 1 NOT YET (2 findings), round 2 ACCEPT | yes: the reviewed `DIFF.patch` matches the landed diff on its 5 files; the other 3 landed files (`docs/archive/no-hidden-traps-2026-11-06/`) were given to the reviewer as sources and are byte-identical to what landed |
| facts-decision | 70e222d947..ab537833d7 | 42229695498 / 42230637757 green | rounds 1-3 NOT YET, round 4 ACCEPT (whole diff read) | yes: the round-4 `CHANGE.diff` has the same content as the landed diff, and its 5 files are byte-identical to ab537833d7. The automated leg's one finding (the OD82 extension) was owner-approved: "ok agreed with the 3 decisions" (2026-11-06) covered the three spellings, including the `#[may_not_terminate]` expression-prefix placement |

Gaps at landing time, for all three: no review note was written (verify.sh had nothing to read), and no
second reviewer of the exact diff was recorded (decided-project-review requires one when available, or
a record of its absence).

## Retrospective reviews (2026-11-06, after landing)

Codex ran through `scripts/review/review.sh` from main 4de277fe (the prompt with the recording rule), on
detached checkouts of each landed head, with the prompt marked RETROSPECTIVE. The notes are on the
landed heads under `refs/notes/concrete-review`. They are retrospective evidence and did not gate the
landings. Kimi ran as a read-only second reviewer (`kimi -p`) over each exact landed diff and tree.

| train | Codex (note on head) | Kimi |
| --- | --- | --- |
| docs-2026-11-06 | **findings** at ac8e50404b (16:53:53Z): `docs/decisions.md:440`, the long-term-changes row gave no quoted owner answer | **findings** (6): the same provenance gap; and five unconfirmed points (rule detail recorded beside "left to R-0574"; README obligations O7/O9 without a referent; A2i/A2c acceptance not naming R-0574; ROADMAP:13 phase-E wording against the LP0 pilot; the changelog's certificate wording) |
| nht-decision | clean at d20e3ab333 (16:54:12Z, 3 pieces) | **findings** (2, unconfirmed): README.md:10 drops the "safe code" qualifier; "termination is known beforehand" overstates what a d-site permission establishes |
| facts-decision | clean at ab537833d7 (16:54:32Z) | clean |

## Remediation

- The provenance gap is fixed on this branch: the long-term-changes row now quotes the owner's answer.
  On 2026-11-06 the owner asked "is this the best long term design". Asked whether to accept all five
  changes, the owner answered "1" (option 1: accept all five).
- Kimi's other findings are unconfirmed. Each needs Codex confirmation, and any wording change to a
  recorded decision goes to the owner. They are tracked in ISS-0014, and nothing here changes those
  rows.
- Structural fix: ISS-0009 and ISS-0011, on main at 4de277fe. `check` fails without a reviewer.
  `land` requires a verified clean review note at the head, or on every member at its exact SHA. The
  review prompt permits recording an owner-approved decision and checks its provenance.
