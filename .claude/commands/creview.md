---
description: Review this branch through scripts/review/review.sh, with delta rounds and the policy cap
argument-hint: "[base ref, default origin/main]"
---

Review this branch against $ARGUMENTS (`origin/main` by default).

Rules and the required reviewers: [AGENTS.md](../../AGENTS.md#rules).
The procedure, options, policy caps and verdict meanings are in [REVIEW.md](../../docs/REVIEW.md).

1. Commit the candidate and note its exact SHA. Run the applicable checks first.
2. Run `scripts/review/review.sh --base <base> --head <sha>`. It owns diff selection,
   delta rounds, notes and the review record; do not substitute an unrecorded manual review.
3. Address findings with a fix or a supported rejection reason. Commit fixes, then run the next
   round over the delta. Follow the risk cap in AGENTS.md and REVIEW.md; the script's current
   default is not permission to exceed that policy.
4. Obtain the other reviewers required by AGENTS.md, retaining their exact candidate SHA and
   verdicts; record unavailable reviewers explicitly.
5. Verify the note with `scripts/review/verify.sh <sha>`. Follow REVIEW.md for cap handling;
   never report findings, blocked or missing evidence as clean.

Report the reviewed SHA, checks, each reviewer's verdict, and any remaining findings.
