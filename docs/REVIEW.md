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
scripts/review/verify.sh [--fetch] [--notes-ref <ref>] [--required-base <commit>] [--round-history] <commit>
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
  Only notes satisfying the [range contract](#verifysh) can advance the delta
  start or skip an already-clean HEAD. Otherwise the round reviews the full
  `base..HEAD` diff. If the last reviewed commit does not descend from the current base (for
  example, after merging newer main changes), the round reviews the full
  `base..HEAD` diff instead. This excludes changes imported from the base and
  includes merge resolutions. It preserves the round count and cap; the prompt
  identifies the patch as the complete branch change.
- A `blocked` round does not count as having reviewed its diff, so the next
  round starts from the last `clean` or `findings` round, or from the base.
  Running again on a blocked `HEAD` is allowed and counts as a round. A round
  with both findings and a blocked piece is `blocked` for this reason; its
  findings are still recorded and carried into the next prompt, together with
  those of the last fully reviewed round.
- A `HEAD` with a qualifying `clean` note exits 0 without calling the reviewer.

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
`scripts/review/invoke.sh`, the single Codex review runner, with the piece on
stdin. Each invocation extracts a fresh `git archive` of the exact reviewed SHA
outside the caller’s worktree, verifies paths, blob identities, executable bits
and symlink targets, and removes the copy after the reviewer and its descendants
exit. Running reviews requires Python 3.9 or newer and Linux with procfs and
child subreaping to clean up processes in separate sessions. The driver checks
the Python version and host facilities before starting a round. Reading review
notes with `verify.sh` remains available on other hosts. The driver resolves
the external reviewer executable to an absolute path from the invoking directory
before entering the repository root.

The review scripts require an absolute directory in every PATH entry. The
shell entry points require `/bin/bash` and reject empty entries,
`.`, and other relative entries before running any external command. This
keeps command lookup independent of the working directory and avoids silently
turning the caller’s checkout into a source of executables. An explicitly
configured relative reviewer filename still resolves from the invoking
directory. The supervisor checks the exact PATH it will pass to the reviewer
before launching it in the archive, so an `/usr/bin/env bash` or
`/usr/bin/env node` shebang also searches only absolute directories.

Diff, archive and verification all disable Git object replacement. Review
patches also disable external diff and text conversion drivers. Absolute
symlinks, symlink chains that leave the archive, hardlinks, special files,
unsupported gitlinks and archive attributes that omit or rewrite content fail
closed before the reviewer starts. The archive verifier also rejects any
committed path with a `.codex` or `.agents` component. Codex discovers project
configuration in `.codex` and repository skills and plugins in `.agents`.
The runner disables automatic project instructions with
`project_doc_max_bytes=0` so files such as `AGENTS.md` remain available as
review material. See the
[Codex configuration reference](https://developers.openai.com/codex/config-reference)
and [plugin documentation](https://developers.openai.com/plugins/build/plugins).

Codex runs in the copy with
`--sandbox danger-full-access --skip-git-repo-check --json`; the prompt requires
read-only review. This disposable copy keeps ordinary reviewer writes away from
the caller, but is not a security boundary for arbitrary code with host access.
The owner authorized unsandboxed Codex on 2026-11-02, "run codex without sandbox",
and 2026-11-08, "you can use codex without sandbox it's okay".
`GIT_OPTIONAL_LOCKS=0` remains defense in depth; the copy has no Git index, retiring
the caller's bug-209 index.lock hazard independently of that setting.

The reviewer inherits only PATH, HOME, CODEX_HOME, TERM, LANG, LC_ALL, SSL_CERT_FILE,
SSL_CERT_DIR and LEAN_NUM_THREADS when set. Its TMPDIR is private to the invocation. The runner sets a Git discovery ceiling
around the copy and disables object replacement in the reviewer environment.
The supervisor stays outside the archive and starts the absolute reviewer
executable directly with this environment and the archive as its working directory.

Every completed review must include a successful command event for the exact
read probe supplied in the prompt. The probe invokes a runner-selected absolute
Python executable and computes SHA-256 over a fresh random nonce followed by
the archived file bytes. The nonce file lives inside the private copy, and the
runner holds the expected digest in memory.
Missing evidence, malformed events, startup errors and execution failures exit
2 before recording a note. Policy refusals go through the retry below.
The exact prompt, final output, raw JSON event stream and stderr are saved beside
the round report under `.review/<sha>-<round>.<random>/` as `invocation-<n>.*`.
Each invocation also retains its exact stdin in the corresponding `.patch.diff` file.
The report also includes runner stderr.

The reply must end with a line that is exactly `VERDICT: clean` or
`VERDICT: findings`. Markdown emphasis around it is tolerated; anything after it
is not. A reply without that line is treated as no answer, never as clean.

**Retry.** The runner retries a policy refusal, including a structured CLI
refusal or a plain refusal before the read probe. It also retries a reply that supplied read
evidence but omitted a valid final verdict. In either case it halves the piece
and sends each half once with a more neutral prompt that leaves out the project
framing. If a half is refused or still lacks a verdict, the piece becomes
`blocked`. Discussion of content filters in a valid review is ordinary review
text.

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
read-evidence: v1
patch-coverage: v2
head: <40-hex sha>
patch-from: <40-hex sha>
patch-sha256: <64-hex digest>
```

**The format is an interface.** `verify.sh` parses it, and the train's review
leg reads it through `verify.sh`, which requires each current key exactly once
and rejects unknown keys. Keys may appear in any order. Change the format only
together with `verify.sh`.

- `rounds` is this round's number, which is also the number of rounds run on the
  branch so far.
- `base` is the branch base (the merge-base), not the start of the delta.
- `pieces` counts the pieces before any refusal retry.
- `read-evidence: v1` identifies the runner protocol that checked every piece
  before accepting its verdict. A blocked note records the protocol and cannot
  establish coverage.

- `patch-coverage: v2` identifies the patch-generation, splitting and range-binding
  protocol. `head` must match the annotated commit. `patch-from` names the actual
  start of the full patch or round delta. `patch-sha256` hashes the exact Git
  patch before splitting. The runner checks each piece's bytes before sending
  it to Codex. The publisher and verifier recompute the range digest from Git.
  Advance this version when earlier patch inputs may have omitted or altered changes.

Historical notes with older or missing protocol versions still count toward the round cap, including
for owner sign-off, and can carry forward open findings. Current versions of both
fields are required for coverage and landing provenance. The next round reviews
the full branch diff unless a newer qualifying review already covers it.

When the verdict is not clean, the findings text goes in a second note under
`refs/notes/concrete-review-findings`, which is pushed too. It is how the next
round, possibly run by another agent, gets the previous findings. The full raw
output of every round is written to `report.md` in its private run directory,
along with `range.diff` and the note payload when the round completes. The
driver creates each run directory atomically and Git ignores it.

A lock in the common Git directory serializes reviews of the same head across
linked worktrees, from round allocation through note publication. A second
lock protects updates to the shared notes refs while reviewers for different
heads can work in parallel.

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
verdict line counts; it must be `VERDICT: ACCEPT` or `VERDICT: REJECT`. A completed REJECT is
recorded and reported, not a refusal of the owner's sign-off; an output with no verdict line does
not qualify. Recording refuses a record that does not qualify.
Push the resulting `refs/owner-signoff/<sha>` to origin. The landing train accepts it as that
commit's review provenance ([LANDING.md](LANDING.md)); it approves no other commit.

## verify.sh

```sh
scripts/review/verify.sh [--notes-ref <ref>] --required-base <base> <commit> # exit 0 clean, 1 findings/blocked, 2 invalid
scripts/review/verify.sh --fetch [--notes-ref <ref>] --required-base <base> <commit> # fetch origin notes first
scripts/review/verify.sh --round-history --required-base <base> <commit> # include historical notes for round accounting
```

It judges the exact commit. A clean note on an ancestor says nothing about the
commits after it, so a branch that gains a commit after its clean round needs
another round. All consumers use this verifier for schema and range validation:
landing supplies the recorded train base, sign-off supplies its diff base for
`rounds-at`, readiness supplies its selected base (`notes.sh ready` defaults to
`origin/main`), and incremental review supplies its current review base.
Sign-off and incremental review use `--round-history` when counting rounds. Standalone sign-off,
readiness and notes helpers disable Git object replacement as the train does.
With `--required-base`, the note's base must be an ancestor-or-equal
of the required commit; an unknown or insufficient base exits 2. This applies to
clean, findings and blocked evidence alike. Incremental review ignores malformed
notes and retains the maximum round count across all schema-valid notes in the
branch history, including merge parents. Uncovered insufficient-range evidence
triggers a full review without resetting rounds or the cap. A covering descendant
review supersedes earlier findings and narrow ancestor notes.
Without this option, verification judges the note without a required range.
`--notes-ref <ref>` explicitly selects the notes ref (default `refs/notes/concrete-review`);
readiness uses the fetched `refs/notes/remote/concrete-review` snapshot.

## Tests

`scripts/checks/review_tool.sh` runs the scripts against throwaway
repositories, with a bare repository as `origin` and a fake `codex` that answers
according to markers in the diff. It needs no API key. It covers delta selection,
splitting, the refusal retry, the cap, note writing and pushing (including the
non-fast-forward merge), verdict parsing, and `verify.sh`.
