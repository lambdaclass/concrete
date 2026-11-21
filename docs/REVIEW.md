# Review: one procedure for every agent

Status: stable reference (R-0544)

Every branch is reviewed by `scripts/review/review.sh` before it lands. The
script picks the diff, splits it, runs the reviewer, records the verdict as a
git note on the reviewed commit, and enforces the round cap.
`scripts/review/verify.sh` validates that note. The landing tools in `scripts/land/`
([LANDING.md](LANDING.md)) use that verifier, including for readiness and sign-off
evidence; see the [range contract](#verifysh).

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
scripts/review/verify.sh [--fetch] [--notes-ref <ref>] [--required-base <commit>] <commit>
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
| 6 | recorded locally, but the notes could not be published |

## What each round reviews

- **Round 1** reviews the full diff from the base to `HEAD`.
- **Round N > 1** can review just the delta from a clean review whose evidence
  covers the current base. The script finds it by walking the branch
  (`base..HEAD`) and [verifying](#verifysh) the notes it finds. Open findings,
  insufficient coverage, or a new base beyond the previous clean commit
  require the full `base..HEAD` diff. The prompt includes unresolved findings
  and asks the reviewer to list each one that is still open.
- A `blocked` round leaves the last qualifying clean review as the possible
  delta start. Running on a blocked `HEAD` again counts as another round. The
  next prompt includes its findings and any earlier unresolved findings.
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
`refs/notes/concrete-review-v2`, and pushes the ref to `origin`:

```
verdict: clean|findings|blocked
rounds: N
reviewer: codex
utc: 2026-10-22T14:03:11Z
base: <40-hex sha>
pieces: K
diff-start: <40-hex sha>
```

**The format is an interface.** `verify.sh` parses it, and the train's review
leg reads it through `verify.sh`. It validates the required fields and rejects
repeated or unknown keys. Change the format only together with `verify.sh`.

- `rounds` counts the rounds run on the branch, and `pieces` counts the pieces
  before any refusal retry.
- `base` is the branch base. `diff-start` equals `base` for a full review.
- A delta also records `predecessor-note` with the exact verdict-note blob ID
  for the commit at `diff-start`.
- A findings or blocked verdict also records `findings-note`, the Git blob ID
  of the findings text. A blank line separates the header from that text
  in the same note. The verifier checks the binding before the evidence is used.

The writer builds the whole record on a private notes ref, verifies it, and
updates the local review ref only if its tip still matches the one used for
intake. Before publication it validates new and changed local records and pushes
the validated notes commit. An interrupted write leaves the previously recorded
verdict and findings together. Raw output goes to `.review/<sha>-<round>.md`,
which Git ignores.

After a non-fast-forward rejection the writer fetches and merges the remote
notes, with up to three push attempts. Concurrent changes to distinct notes can
merge. Conflicting revisions to the same note require reconciliation. Any other
push failure stops at once with exit 6. The writer also syncs before intake
to count rounds run elsewhere.

The pre-push hook (`.githooks/pre-push`) takes the notes tier for review-only
pushes. A push that also carries a branch takes that branch's tier.
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
verdict line counts; it must be `VERDICT: ACCEPT` or `VERDICT: REJECT`. A completed REJECT is
recorded and reported, not a refusal of the owner's sign-off; an output with no verdict line does
not qualify. Recording refuses a record that does not qualify.
Push the resulting `refs/owner-signoff/<sha>` to origin. The landing train accepts it as that
commit's review provenance ([LANDING.md](LANDING.md)); it approves no other commit.

## verify.sh

```sh
scripts/review/verify.sh [--notes-ref <ref>] --required-base <base> <commit> # exit 0 clean, 1 findings/blocked, 2 invalid
scripts/review/verify.sh --fetch --required-base <base> <commit> # use the published snapshot
```

It judges the exact commit. A clean note on an ancestor says nothing about the
commits after it, so a branch that gains a commit after its clean round needs
another round. Landing supplies the recorded train base, sign-off supplies its diff
base for `rounds-at`, readiness supplies its selected base (`notes.sh ready`
defaults to `origin/main`), and incremental review supplies its current review
base. With `--required-base`, the note's base must be an ancestor-or-equal of the
required commit. The verifier also requires the note base to be an ancestor of
the reviewed commit or that commit itself. Incremental review retains the
maximum round count across valid notes in the branch history, including merge
parents. Uncovered insufficient-range evidence
triggers a full review without resetting rounds or the cap. A covering descendant
review supersedes earlier findings and narrow ancestor notes.
Verification walks each delta back to a full review. Each predecessor must be a
strict ancestor with a clean verdict, the recorded note blob, and coverage of
its successor's base. `--notes-ref <ref>` selects the notes ref, which defaults
to `refs/notes/concrete-review-v2`. With `--fetch`, the verifier reads the
published snapshot at `refs/notes/remote/concrete-review-v2`, which readiness
and the landing train also use.

Existing notes under `refs/notes/concrete-review` are eligible for round
accounting. `verify.sh --metadata-only --required-base <base> <commit>` validates
their schema and scope for exact-SHA owner sign-off. It returns success for
valid metadata with any verdict and establishes no review coverage. In a fresh
clone, fetch the published round metadata with `verify.sh --fetch --metadata-only`
and pass `--notes-ref refs/notes/remote/concrete-review-v2` to `signoff.sh record`.
Unresolved
legacy findings still require text under `refs/notes/concrete-review-findings`.
New evidence takes precedence at each commit. Older writers can update their
legacy refs while the new verifier uses only the new-format evidence for clean
landing. A branch with only legacy evidence needs a full review within its
remaining rounds or an exact-SHA owner sign-off once it reaches its cap.

## Tests

`scripts/checks/review_tool.sh` runs the scripts against throwaway
repositories, with a bare repository as `origin` and a fake `codex` that answers
according to markers in the diff. It needs no API key. It covers delta selection,
splitting, the refusal retry, the cap, note writing and pushing (including the
non-fast-forward merge), verdict parsing, and `verify.sh`.
