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
scripts/review/review.sh [--writer codex|kimi|agy|claude[,...]] [--reviewer codex|kimi|agy] [--base <ref>] [--head <ref>] [--max-rounds 5] [--pieces auto|N] [--prompt-extra "..."]
scripts/review/verify.sh [--fetch] [--required-base <ref>] <commit>
```

One invocation runs **one round** on `HEAD`. Commit fixes between rounds; the
uncommitted tree is never reviewed, and the script warns when it is dirty.
Writer provenance is derived from the commits in `base..HEAD` by
`scripts/review/writer_provenance.py` (`refs/notes/concrete-writer` plus commit
stamps; unstamped human or pre-harness commits are explicitly attributed to
`claude`). `--writer` is optional and, when passed, checked as an assertion that
must match the derived writer set. `--reviewer` defaults to Codex. The writer
never reviews their own work: reviewer equal to any writer of `base..HEAD` is a
usage error (exit 2). In particular, a branch with any Codex-written commit must
be reviewed by Kimi or agy, never Codex.

| option | meaning |
| --- | --- |
| `--writer <model[,...]>` | optional assertion: who wrote the branch (`codex`, `kimi`, `agy`, `claude`, or a comma-separated list across rounds); must match derived provenance. |
| `--reviewer <model>` | independent reviewer: `codex` (default), `kimi` or `agy`; it must not be any writer of `base..HEAD`. |
| `--base <ref>` | base of round 1. Default: the merge-base of `HEAD` with `origin/main`; must be an ancestor of `HEAD`. |
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
- **Round N > 1** reviews only the delta since the last reviewed commit whose
  note is well-formed, carries valid independent writer provenance, and whose
  recorded `base` is an ancestor-or-equal of the current round's `BASE_SHA` (so a
  delta review never widens base coverage across an unreviewed `A..B` range). The
  script finds that commit by walking the branch (`base..HEAD`) for the newest
  qualifying note under `refs/notes/concrete-review`. The prompt includes the
  previous round's findings and asks the reviewer to list each one that is still
  open.
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
extracts `HEAD` with `git archive` into a temporary directory and runs the chosen
reviewer there, never in the live clone:

- Codex: `codex exec --skip-git-repo-check -s danger-full-access` with `-C` naming the archive.
- Kimi: `kimi -p "<prompt>"` with the archive as its working directory, never `--yolo` or `--auto`.
- agy: `agy --dangerously-skip-permissions -p "<prompt>"` with the archive as its working directory.
  That permission bypass exists only inside this disposable archive.

The reviewer can read the complete tree at the reviewed commit but has no
checkout, refs or notes of its own. The extracted archive is marked read-only
(`chmod -R a-w`) and hashed before and after every reviewer call; if a reviewer
modifies the archived tree, the piece is `blocked` and the archive is restored
from `HEAD` before the next piece runs. The diff piece is included in the prompt
and every reviewer's stdin is closed. Each attempt is bounded by `bounded_run`
with `REVIEW_TIMEOUT` (default 1500 seconds, with one longer retry). A double
timeout under `bounded_run` (exit 124 or 137) is recorded as `blocked` at once
without a halving retry. `GIT_OPTIONAL_LOCKS=0` remains set so a reviewer
command cannot leave an `index.lock` behind (bug 209).

### Shared reviewer-output contract (`scripts/review/output_contract.py`)

Every automated reviewer output — branch reviews (`scripts/review/review.sh`),
owner sign-off reviewer outputs (`scripts/land/signoff.sh`), and review-evidence
records — is validated by `scripts/review/output_contract.py`:

1. At least one `READ: <path>` line naming a clean relative file path that
   exists as a file (blob) in the reviewed commit's tree must appear before the
   final two lines.
2. The penultimate non-empty line must be `REPOSITORY: readable`. Any
   `REPOSITORY: unreadable` line anywhere in the output, any `REPOSITORY:`
   directive before the penultimate line, or any known sandbox/repository
   failure text (`sandbox could not start`, `bwrap: Operation not permitted`,
   `repository reads failed`, `could not read the repository`,
   `repository verification failed`) immediately marks the output `blocked`.
3. The last non-empty line must be `VERDICT: <value>` (where `<value>` is
   `clean` or `findings` for `review.sh`, and `ACCEPT` or `REJECT` for
   `signoff.sh`), with nothing after it. Markdown emphasis around directive
   lines is tolerated; trailing text after the verdict is not.

**Refusals.** A piece that comes back refused, with no verdict line, or with a
non-timeout reviewer error is retried **once**: the piece is halved and each
half is sent with a more neutral prompt that leaves out the project framing. If
any half fails again, the piece is `blocked` and the script moves on. It does
not loop. Refusal text is matched only when the reply has no verdict line, so a
real review that discusses content filters still parses.

## Writer provenance and the writer log (`scripts/review/writer_provenance.py`)

Writer identity comes from the writer harness, not from a caller flag or a
commit trailer alone:

- Each harness-written commit is stamped with author
  `<Model> (writer) <model@writers.concrete>` (while the committer remains the
  project owner's identity), a `Writer: <model>` trailer, and for `agy` an
  `Agy-Diff-SHA256: <64-hex>` trailer recording the sha256 of the diff
  transported from its throwaway archive copy.
- Each stamped commit carries a matching writer-log entry under
  `refs/notes/concrete-writer` (and exportable as a self-contained multi-stanza
  git blob via `writer_provenance.py export-log --write-blob` so review-evidence
  records can embed it unchanged):
  ```
  model: codex|kimi|agy|claude
  round: <positive integer>
  branch: <branch-name>
  brief-sha256: <64-hex>
  start-sha: <40-hex>
  end-sha: <40-hex>
  commit: <40-hex>
  agy-diff-sha256: <64-hex>        # present iff model == agy
  ```
- `scripts/review/writer_provenance.py` derives and cross-checks the ordered
  unique writer set of `base..HEAD`. Any commit carrying a partial or mismatched
  stamp (such as a `Writer:` trailer with no matching writer-log entry, a writer
  author without trailer/log, or a mismatched `agy-diff-sha256`) is refused.
- Unstamped non-merge commits (human or pre-harness commits with no writer
  author, no writer trailer, and no writer-log entry) have an explicit,
  documented treatment: they are attributed to `claude` and never silently
  treated as written by nobody.
- A branch written by multiple models across rounds records all of them in order
  (for example `writer: codex,agy` or `writer: claude,codex`); the reviewer must
  be none of them.

## Permanent owner rule

Both the standard and neutral prompts carry this rule permanently. The reviewer
must report every shortcut, containment patch or corner cut as a finding and
name the long-term fix:

> OWNER RULE (2026-11-08, verbatim: "ALWAYS tell the reviewrs to do the best long term thing rather than cuitting corners"): judge and fix by the best long-term design. Never cut corners for speed, a smaller diff or fewer rounds. As a reviewer, report any shortcut, containment patch or corner cut as a real finding and say what the long-term fix is. As a writer, implement the structural fix, not a containment patch; if staging is truly unavoidable, record the long-term design and name its roadmap row.

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
reviewer: codex|kimi|agy
writer: codex|kimi|agy|claude[,...]
utc: 2026-10-22T14:03:11Z
base: <40-hex sha>
pieces: K
repository: readable             # clean notes only
```

**The format is an interface.** `verify.sh` parses it, and the train's review
leg, `ready.sh`, and `notes.sh ready` read it through `verify.sh`. The original
six common keys remain mandatory; new notes add `writer`. A missing writer makes
a note legacy: it stays readable for round counting (`verify.sh` exits 1, not 2)
but is not clean provenance. Any repeated or unknown key is malformed (exit 2).
Keys are matched by name, not position. A clean note must carry
`repository: readable`, a `base` that is an ancestor-or-equal of the reviewed
commit, and a `writer` list that matches `writer_provenance.py` over `base..HEAD`
and excludes `reviewer`; legacy, self-reviewed, provenance-mismatched and
non-ancestor-base clean notes exit 1. Running `review.sh` on such a `HEAD`
performs a new round rather than short-circuiting. Change the format only
together with `verify.sh`.

### Schema transition

1. **Bootstrap of `wip/review-access`:** While `main` still carries the 6-key
   `verify.sh` (`verdict`, `rounds`, `reviewer`, `utc`, `base`, `pieces`),
   `wip/review-access` itself is reviewed on `concrete-ci-2` with full
   repository access and lands on a 6-key clean note written by `main`'s
   `review.sh` (which `main`'s `verify.sh` accepts).
2. **After `wip/review-access` lands on `main`:** `main`'s `verify.sh` is the
   new verifier. Every pre-existing 6-key clean note (lacking
   `repository: readable` and `writer:`) and any self-reviewed clean note stops
   qualifying as clean provenance (`verify.sh` exits 1, while still counting its
   rounds for `review.sh` and `signoff.sh`).
3. **Re-reviewing an open branch:** On any open branch whose head carries a
   legacy 6-key clean note, run `scripts/review/review.sh [--reviewer <model>]`
   at the branch head; `review.sh` treats the legacy note as `legacy-clean`,
   increments the round counter, reviews the full `base..HEAD` diff in the
   read-only archive, and writes a new 8-key note.

The `writer:` key and its `codex|kimi|agy|claude` vocabulary deliberately match
the key owner sign-off records use, so all provenance names the branch writer in
the same way.

- `rounds` is this round's number, which is also the number of rounds run on the
  branch so far.
- `reviewer` and `writer` record the independent review relationship.
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
only the review and writer notes refs (`refs/notes/concrete-review`,
`refs/notes/concrete-review-findings`, `refs/notes/concrete-writer`): a notes
commit holds one blob per annotated commit and no source tree. A push that also
carries a branch takes that branch's tier.
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
`r.txt`. The request body must name that commit by an unambiguous prefix, and the output must pass
`scripts/review/output_contract.py` (at least one valid `READ: <path>` from `<sha>`'s tree,
`REPOSITORY: readable` as the penultimate non-empty line, and `VERDICT: ACCEPT` or `VERDICT: REJECT`
as the last non-empty line). A completed REJECT is recorded and reported, not a refusal of the
owner's sign-off; an output with no verdict line or failing the output contract does not qualify.
Recording refuses a record that does not qualify.
Push the resulting `refs/owner-signoff/<sha>` to both remotes. The landing train accepts it as that
commit's review provenance ([LANDING.md](LANDING.md)); it approves no other commit.

## verify.sh

```sh
scripts/review/verify.sh <commit>                              # 0 clean provenance; 1 not clean; 2 none/malformed
scripts/review/verify.sh --fetch <commit>                     # fetch the notes ref from origin first
scripts/review/verify.sh --required-base <base> <commit>      # also require review coverage from base
```

It judges the exact commit. A clean note on an ancestor says nothing about the
commits after it, so a branch that gains a commit after its clean round needs
another round. A legacy clean note without `repository: readable` or `writer`,
and a clean note whose reviewer equals its writer, exits 1.

With `--required-base BASE`, a clean note qualifies only when its `base` is an
ancestor of `BASE`, or `BASE` itself. Thus the note's reviewed range covers
`BASE..commit`. For history A→B→C and required base A, a clean note on C based
at unreviewed B is refused; one based at A passes. The landing train passes its
recorded base for both head and member notes. Owner sign-off's `rounds-at` check
deliberately omits this option: it counts rounds used, not landing coverage.

## Tests

`scripts/checks/review_tool.sh` runs the scripts against throwaway
repositories, with a bare repository as `origin` and fake Codex, Kimi and agy
binaries. It needs no API key or network. It covers delta selection, splitting,
the refusal retry, the cap, note writing and pushing (including the
non-fast-forward merge), verdict parsing, archived-tree access for every
reviewer, independent reviewer/writer provenance and its refusal cases, both
prompts' permanent owner rule, prompt persistence and hashes, blocked
repository-access failures, legacy clean-note refusal, and required-base
coverage. `scripts/checks/landing_script.sh` repeats the A→B→C reproducer
through the landing train for both head and member notes.
