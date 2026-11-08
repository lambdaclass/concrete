---
description: independent review of this branch through scripts/review/review.sh (max 5 rounds, delta-only after the first)
argument-hint: "[base ref, default origin/main]"
---

Review this branch against: $ARGUMENTS (`origin/main` when empty; written `<base>` below).

Rules: [AGENTS.md](../../AGENTS.md) ("Review"). State who wrote the branch and choose a different
reviewer: Codex-written work goes to Kimi or agy, never Codex.

**Dependency.** `scripts/review/review.sh` is being built on its own branch. Read its usage header
before the first call and follow it; it owns the prompt, reviewer invocation, round count and delta
between rounds. Until it is on your base, run the equivalent by hand with an independent reviewer
over `git diff <base>...HEAD` in round 1 and `git diff <sha reviewed last round>..HEAD` afterward,
with the prompt and diff supplied together and stdin otherwise closed.

The prompt is neutral: it states what the change claims and asks the reviewer to refute it. Do not
tell the reviewer what you believe is correct.

1. Commit everything first; review a commit, not a dirty tree. Note the reviewed SHA.
2. **Round 1** over the whole diff from the merge base. For each finding: fix it (a commit per
   round), or answer it with the reason it does not hold, citing the code or a command you ran.
   Verify a claim before accepting or rejecting it; reviewers are sometimes wrong, and so is prose.
3. **Rounds 2 to 5** see only the delta since the last reviewed SHA, plus the open findings.
4. Stop when a round reports no remaining findings, or after round 5. Never exceed 5 rounds; list
   whatever is still open as follow-ups in the report and, when it matters, in the record or roadmap.
5. Always run the reviewer with `GIT_OPTIONAL_LOCKS=0` (bug 209): a sandboxed `git status` otherwise
   takes the worktree's `index.lock`, and a command stopped mid-refresh leaves it behind. If a lock
   is still found, remove it only when no process owns it.

Report: the rounds run, each round's findings with the fix commit or the rejection reason, the
final verdict, and the head SHA reviewed.
