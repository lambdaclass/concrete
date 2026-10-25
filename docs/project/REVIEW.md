# Review: one procedure for every agent

Status: stable reference (R-0544)

Every branch is reviewed by `scripts/review/review.sh` before it lands. The
script picks the diff, splits it, runs the reviewer, records the verdict as a
git note on the reviewed commit, and enforces the round cap.
`scripts/review/verify.sh` validates that note. The landing tools in `scripts/land/`
([LANDING.md](LANDING.md)) read its verdict through `scripts/land/notes.sh`: a branch is
ready to land only when its head carries a clean review note, and the landing bot carries
a clean verdict across a conflict-free rebase.

## Why one script

Measured on 2026-10-22:

- Reviews took 8 to 34 rounds per branch, and every round re-reviewed the whole
  growing diff. Most of each round's cost was re-reading code that an earlier
  round had already accepted.
- The reviewer's content filter refused whole rounds on ownership and memory
  diffs. The same diffs went through once the prompt described the work neutrally
  ("a compiler ownership-checker review") and the diff was sent in smaller pieces.

Each agent had been solving both problems by hand, differently. The script does
it the same way every time, and its note is the record the landing bot checks.

## Usage

```sh
scripts/review/review.sh [--base <ref>] [--max-rounds 5] [--pieces auto|N] [--prompt-extra "..."]
scripts/review/verify.sh [--fetch] <commit>
```

One invocation runs **one round** on `HEAD`. Commit fixes between rounds; the
uncommitted tree is never reviewed, and the script warns when it is dirty.

| option | meaning |
| --- | --- |
| `--base <ref>` | base of round 1. Default: the merge-base of `HEAD` with `origin/main`. |
| `--max-rounds N` | the round cap (default 5). |
| `--pieces auto\|N` | `auto` splits at 60000 bytes per piece (`REVIEW_PIECE_BYTES`); `N` aims for N pieces. File groups are never merged, so a diff touching more groups than N still gets one piece per group. |
| `--prompt-extra "..."` | text appended to every prompt, such as what the branch is for. |

| exit | meaning |
| --- | --- |
| 0 | clean |
| 1 | findings: fix, commit, run again |
| 2 | usage or environment error; nothing recorded |
| 3 | round cap reached; split the branch or escalate (open findings are printed) |
| 4 | nothing to review: `HEAD` was already reviewed with findings, or the diff is empty |
| 5 | blocked: a piece could not be reviewed, even after the retry |
| 6 | recorded locally, but the notes ref could not be pushed |

## What each round reviews

- **Round 1** reviews the full diff from the base to `HEAD`.
- **Round N > 1** reviews only the delta since the last reviewed commit. The
  script finds that commit by walking the branch (`base..HEAD`) for the newest
  note under `refs/notes/concrete-review`. The prompt includes the previous
  round's findings and asks the reviewer to list each one that is still open.
- A `blocked` round does not count as having reviewed its diff, so the next
  round starts from the last `clean` or `findings` round, or from the base.
  Running again on a blocked `HEAD` is allowed and counts as a round. A round
  with both findings and a blocked piece is `blocked` for this reason; its
  findings are still recorded and carried into the next prompt, together with
  those of the last fully reviewed round.
- A `HEAD` that is already `clean` exits 0 without calling the reviewer.

Rounds are counted from the notes, so the cap holds across invocations,
worktrees and agents. Rebasing creates new commits without notes, which
restarts the count; say so when you hand the branch over.

## Splitting and the neutral prompt

The diff is split by file group first, in this order: compiler sources
(`Concrete/`, `std/`, `proofs/`, `grammar/`, top-level Lean and Lake files),
tests (`tests/`, `lean_tests/`, `examples/`, `bench/`, `scripts/tests/`),
scripts (everything else), docs (`docs/`, `paper/`, `site/`, `research/`,
`*.md`). Within a group, files are packed into pieces at or under the size
limit. A file larger than the limit is cut at hunk boundaries, and a hunk larger
than the limit is cut into parts with renumbered `@@` headers, so each piece is a
well-formed diff. `scripts/review/split_diff.py` does the splitting.

The prompt describes the repository as a compiler and asks for correctness
findings: wrong behaviour, logic errors, regressions, unhandled cases, tests that
do not check what they claim, and docs that contradict the code. It runs
`codex exec --sandbox read-only` from the repository root, with the piece on
stdin, so the reviewer can open other files for context but cannot change them.
It runs with `GIT_OPTIONAL_LOCKS=0`: the sandbox does not stop git from taking the
worktree's `index.lock` for an index refresh, and a reviewer command stopped
mid-refresh left that lock behind (bug 209).

The reply must end with a line that is exactly `VERDICT: clean` or
`VERDICT: findings`. Markdown emphasis around it is tolerated; anything after it
is not. A reply without that line is treated as no answer, never as clean.

**Refusals.** A piece that comes back refused, with no verdict line, or with a
reviewer error is retried **once**: the piece is halved and each half is sent
with a more neutral prompt that leaves out the project framing. If any half
fails again, the piece is `blocked` and the script moves on. It does not loop.
Refusal text is matched only when the reply has no verdict line, so a real
review that discusses content filters still parses.

## The note

After each round the script writes this note on `HEAD` under
`refs/notes/concrete-review`, and pushes the ref to `origin`:

```
verdict: clean|findings|blocked
rounds: N
reviewer: codex
utc: 2026-10-22T14:03:11Z
base: <40-hex sha>
pieces: K
```

**The format is an interface.** The landing bot parses it, and `verify.sh`
rejects any note that lacks one of these six keys, repeats a key, or carries a
key other than these and the landing pipeline's two optional ones
(`carried-from: <sha>`, which the bot writes when it carries a clean verdict
across a conflict-free rebase, and `order: N`). Keys are matched by name, not
position. Change the format only together with both.

- `rounds` is this round's number, which is also the number of rounds run on the
  branch so far.
- `base` is the branch base (the merge-base), not the start of the delta.
- `pieces` counts the pieces before any refusal retry.

When the verdict is not clean, the findings text goes in a second note under
`refs/notes/concrete-review-findings`, which is pushed too. It is how the next
round, possibly run by another agent, gets the previous findings. The full raw
output of every round is written to `.review/<sha>-<round>.md`, which git
ignores.

A non-fast-forward rejection (another agent pushed notes in the meantime) is
handled by fetching the remote notes, merging them (`git notes merge -s ours`: on
a conflict for the same commit the local, newer note wins), and retrying, up to
three attempts. Any other push failure stops at once with exit 6. The script also
fetches and merges the remote notes before each round, so it counts rounds that
were run elsewhere.

The pre-push hook (`.githooks/pre-push`) runs no gates for a push that updates
only these two notes refs: a notes commit holds one blob per annotated commit and
no source tree. A push that also carries a branch takes that branch's tier.
Until this hook reaches `main`, a hook installed from an older `main` still runs
the gates on a note push, and the push fails outside `nix develop`.

## The cap

At most `--max-rounds` rounds (default 5). When the last allowed round ends with
findings or blocked pieces, the script records the note, prints the open
findings, and exits 3. Asking for another round exits 3 without calling the
reviewer. Do not work around the cap: split the branch into smaller branches or
escalate to a human reviewer, and pass on the open findings.

## verify.sh

```sh
scripts/review/verify.sh <commit>          # exit 0 clean, 1 findings/blocked, 2 none or malformed
scripts/review/verify.sh --fetch <commit>  # fetch the notes ref from origin first
```

It judges the exact commit. A clean note on an ancestor says nothing about the
commits after it, so a branch that gains a commit after its clean round needs
another round.

## Tests

`scripts/tests/check_review_tool.sh` runs the scripts against throwaway
repositories, with a bare repository as `origin` and a fake `codex` that answers
according to markers in the diff. It needs no API key. It covers delta selection,
splitting, the refusal retry, the cap, note writing and pushing (including the
non-fast-forward merge), verdict parsing, and `verify.sh`.
