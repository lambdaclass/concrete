# Review: one procedure for every agent

Status: stable reference (R-0544)

Every branch is reviewed by `scripts/review/review.sh` before it lands. The
script picks the diff, splits it, runs the reviewer, records the verdict as a
git note on the reviewed commit, and enforces the round cap.
`scripts/review/verify.sh` validates that note. The landing tools in `scripts/land/`
([LANDING.md](LANDING.md)) read its verdict through `scripts/land/notes.sh`: a branch is
ready to land only when its head carries a clean review note.

## Why one script

Measured on 2026-10-22:

- Reviews took 8 to 34 rounds per branch, and every round re-reviewed the whole
  growing diff. Most of each round's cost was re-reading code that an earlier
  round had already accepted.
- The reviewer's content filter refused whole rounds on ownership and memory
  diffs. The same diffs went through once the prompt described the work neutrally
  ("a compiler ownership-checker review") and the diff was sent in smaller pieces.

Each agent had been solving both problems by hand, differently. The script does
it the same way every time, and its note is the record the landing train checks.

## Usage

```sh
scripts/review/review.sh [--base <ref>] [--head <ref>] [--max-rounds 5] [--pieces auto|N] [--prompt-extra "..."]
scripts/review/verify.sh [--fetch] <commit>
```

One invocation runs **one round** on `HEAD`. Commit fixes between rounds; the
uncommitted tree is never reviewed, and the script warns when it is dirty.

| option | meaning |
| --- | --- |
| `--base <ref>` | base of round 1. Default: the merge-base of `HEAD` with `origin/main`. |
| `--head <ref>` | the commit the caller means to review; it must be the checkout's HEAD, or nothing runs (exit 2). `train.sh` and `ready.sh` pass it. |
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
do not check what they claim, and docs that contradict the code. The script
extracts `HEAD` with `git archive` into a temporary directory and runs
`codex exec --skip-git-repo-check -s danger-full-access` there, never in the
live clone. The reviewer can read the complete tree at the reviewed commit but
has no checkout, refs or notes of its own. The diff piece is included in the
prompt and Codex's stdin is closed. Each attempt is bounded by `REVIEW_TIMEOUT`
(default 1500 seconds, with one longer retry). `GIT_OPTIONAL_LOCKS=0` remains set
so a reviewer command cannot leave an `index.lock` behind (bug 209).

The reply's penultimate line must be exactly `REPOSITORY: readable` or
`REPOSITORY: unreadable`, and its last line exactly `VERDICT: clean` or
`VERDICT: findings`. Markdown emphasis around them is tolerated; anything after
the verdict is not. `readable` means the reviewer successfully opened repository
files for context. A verdict without exactly one readable marker is `blocked`,
never clean. Known repository-access failures (a sandbox that could not start or
initialize, `bwrap: Operation not permitted`, failed repository reads, or an
explicit unreadable marker) are `blocked` even when the reply says clean.

**Refusals.** A piece that comes back refused, with no verdict line, or with a
reviewer error is retried **once**: the piece is halved and each half is sent
with a more neutral prompt that leaves out the project framing. If any half
fails again, the piece is `blocked` and the script moves on. It does not loop.
Refusal text is matched only when the reply has no verdict line, so a real
review that discusses content filters still parses.

## Owner decisions: choosing versus recording

Both prompts ask whether the patch picks a side on an owner decision: choosing, revising,
encoding, implementing, extending or reverting what an undecided question covers, whether one of
the language decisions OD1 – OD157 ([decisions.md](decisions.md#owner-decisions)) or an important
project question. That is a finding, and the patch does not land: only the owner decides.

Recording a decision the owner explicitly approved, under the
[recording rule](decisions.md#decided-project-recording), is not picking a side, and the review
permits it. For such a patch the reviewer checks four things, and a failure of any is a finding:

- **provenance**: each recorded answer carries the owner's answer, quoted with its date, or the
  quoted owner rule that decides the question under the recording rule;
- **faithful transcription**: the recorded text says what that answer says, no more and no less;
- **scope**: the patch changes only what that answer covers;
- **nothing unapproved**: nothing else is marked decided, and no undecided question is chosen or
  revised.

A review that refuses such a patch is answered in the next round (a quoted answer, a narrower
change), never by skipping the review.

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
repository: readable             # clean notes only
```

**The format is an interface.** `verify.sh` parses it, and the train's review
leg reads it through `verify.sh`, which rejects any note that lacks one of the
six common keys, repeats a key, or carries any other key. Keys are matched by
name, not position. A clean note must also carry `repository: readable`; a
legacy clean note without it is well formed but exits 1, not clean, so old
diff-only reviews are not landing provenance. Running `review.sh` on such a
`HEAD` performs a new round rather than short-circuiting. Change the format only
together with `verify.sh`.

- `rounds` is this round's number, which is also the number of rounds run on the
  branch so far.
- `base` is the branch base (the merge-base), not the start of the delta.
- `pieces` counts the pieces before any refusal retry.
- `repository` records that every piece's reviewer could read the archived tree.

When the verdict is not clean, the findings text goes in a second note under
`refs/notes/concrete-review-findings`, which is pushed too. It is how the next
round, possibly run by another agent, gets the previous findings. The full raw
output of every round is written to `.review/<sha>-<round>.md`, which git
ignores. Every prompt sent, including its diff piece, is saved byte for byte as
`.review/<sha>-<round>.prompt.<invocation>.txt`; the corresponding raw-output
section records that path and the prompt's sha256.

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

The owner may then approve one exact commit (R-0618), after Codex, Kimi and agy have all reviewed
that commit. The cap is the class of the branch's diff against its base, by path, the highest class
present winning: docs and roadmap 2 rounds, scripts 3, compiler and spec 5 (an unclassified path
counts as compiler and spec); it counts the rounds used, whatever their verdicts. The approval is
recorded, from the owner's own words only, with `scripts/land/signoff.sh record <sha> --base <base>
--rounds-at <the review.sh-noted commit> --date --quote`, plus `--codex-request <request> --codex
<output>` and the corresponding kimi and agy options. For each reviewer `r`, the recorder stores the
verbatim request as `r-request.txt` after prepending `reviewed: <sha>`, and the output separately as
`r.txt`. The request body must name that commit by an unambiguous prefix, and only the output's last
verdict line counts; it must be `VERDICT: ACCEPT`. Recording refuses a record that does not qualify.
Push the resulting `refs/owner-signoff/<sha>` to both remotes. The landing train accepts it as that
commit's review provenance ([LANDING.md](LANDING.md)); it approves no other commit.

## verify.sh

```sh
scripts/review/verify.sh <commit>          # 0 readable clean; 1 findings/blocked/legacy clean; 2 none/malformed
scripts/review/verify.sh --fetch <commit>  # fetch the notes ref from origin first
```

It judges the exact commit. A clean note on an ancestor says nothing about the
commits after it, so a branch that gains a commit after its clean round needs
another round. A legacy clean note without `repository: readable` exits 1 with
a message naming the missing key.

## Tests

`scripts/checks/review_tool.sh` runs the scripts against throwaway
repositories, with a bare repository as `origin` and a fake `codex` that answers
according to markers in the diff. It needs no API key or network. It covers delta
selection, splitting, the refusal retry, the cap, note writing and pushing
(including the non-fast-forward merge), verdict parsing, archived-tree access,
prompt persistence and hashes, blocked repository-access failures, and
`verify.sh`'s refusal of legacy clean notes.
