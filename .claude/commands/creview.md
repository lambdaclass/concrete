---
description: Codex review of this branch through scripts/review/review.sh (max 5 rounds, delta-only after the first)
argument-hint: "[base ref, default origin/main]"
---

Review this branch against: $ARGUMENTS (`origin/main` when empty; written `<base>` below).

Rules: [AGENTS.md](../../AGENTS.md) ("Review").

Use `scripts/review/review.sh`; read its usage header before the first call.
It owns the prompt, round count and delta between rounds. Its shared runner uses
an exact-SHA private archive and owner-authorized `--sandbox danger-full-access`,
requires repository-read evidence, and preserves the exact prompt and raw output.
Do not invoke Codex directly against a live checkout. See [REVIEW.md](../../docs/REVIEW.md).

The prompt is neutral: it states what the change claims and asks Codex to refute it. Do not tell
Codex what you believe is correct.

1. Commit everything first; review a commit, not a dirty tree. Note the reviewed SHA.
2. **Round 1** over the whole diff from the merge base. For each finding: fix it (a commit per
   round), or answer it with the reason it does not hold, citing the code or a command you ran.
   Verify a claim before accepting or rejecting it; Codex is sometimes wrong, and so is prose.
3. **Rounds 2 to 5** see only the delta since the last reviewed SHA, plus the open findings.
4. Stop when a round reports no remaining findings, or after round 5. Never exceed 5 rounds; list
   whatever is still open as follow-ups in the report and, when it matters, in the record or roadmap.
5. The runner keeps `GIT_OPTIONAL_LOCKS=0` as defense in depth. Its archive has no
   Git index; the caller's index.lock is no longer exposed (bug 209).

Report: the rounds run, each round's findings with the fix commit or the rejection reason, the
final verdict, and the head SHA reviewed.
