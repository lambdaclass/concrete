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
scripts/review/verify.sh [--fetch] [--required-base <ref>] [--fields] [--brief-file <file>] [--output-file <file>] [--evidence-dir <dir>] <commit>
```

One invocation runs **one round** on `HEAD`. Commit fixes between rounds; the
uncommitted tree is never reviewed, and the script warns when it is dirty.
Writer provenance is derived from the commits in `base..HEAD` by
`scripts/review/writer_provenance.py` (`refs/notes/concrete-writer` plus commit
stamps; unstamped authored commits have an unknown writer and are refused unless
backfilled in `refs/notes/concrete-writer`, and `claude` is a writer only when
explicitly logged). `--writer` is optional and, when passed, checked as an assertion that
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

### Shared reviewer-output collector (`scripts/review/collect_output.sh`) and contract (`scripts/review/output_contract.py`)

The reader's input is strictly the reviewer's final message, never a stdout/stderr
session log (coordinator, 2026-11-08). In particular, `codex exec` writes a
session transcript ending with a `"tokens used"` trailer to stdout while writing
the reviewer's final message to `-o <file>`. Neither the collector nor the
reader strips or tolerates trailers. Instead, every tool that collects reviewer
output (`scripts/review/review.sh`, `scripts/land/signoff.sh`, and any other
caller) invokes `collect_reviewer_output` from
[collect_output.sh](../scripts/review/collect_output.sh):

- **Codex:** `codex exec --skip-git-repo-check -s danger-full-access --ephemeral --color never -C <tree> -o <out-file> "<prompt>" ><log-file> 2>&1 </dev/null` — the reader receives `<out-file>` (`-o`), never `<log-file>`.
- **agy:** `(cd <tree> && agy --dangerously-skip-permissions -p "<prompt>") ><out-file> 2><log-file> </dev/null` — the final response on stdout goes to `<out-file>`, with stderr kept separate in `<log-file>`.
- **Kimi:** `(cd <tree> && kimi -p "<prompt>") ><out-file> 2><log-file> </dev/null` — stdout goes to `<out-file>` with stderr kept separate in `<log-file>`.
  *Recorded finding (2026-11-08):* `kimi --help` (`kimi-code` 2.4.1) offers only
  `--output-format text` and `--output-format stream-json` for `-p` and has no
  `-o` / `--output-last-message` or final-message-only flag (`text` renders every
  assistant step through `PromptBlockWriter` on stdout, and `stream-json` emits
  a JSONL event stream). Per project policy, neither `collect_output.sh` nor
  `output_contract.py` parses or strips logs.
- **Shared archive verification, validation and structural check:** `collect_output.sh`
  provides `prepare_review_archive`, `verify_review_archive`,
  `collect_reviewer_output` and `validate_reviewer_output`, shared by both
  `review.sh` and `signoff.sh`. `prepare_review_archive` extracts the reviewed
  commit's read-only archive (`chmod -R a-w`) and records its manifest before
  the reviewer runs; `validate_reviewer_output` verifies the archive manifest
  afterward (refusing any modification as `blocked` and restoring the archive)
  and invokes `output_contract.py` with `--rc`, `--incomplete` (when
  `bounded_is_incomplete`), `--log <log-file>`, and `<out-file>`.
  `collect_reviewer_output` and `validate_reviewer_output` refuse identical
  `<out-file>` and `<log-file>` paths and never copy or fall back from
  `<log-file>` to `<out-file>`. `scripts/review/collect_output.sh --check-structure`
  verifies structurally that the collector and its callers (`review.sh`,
  `signoff.sh`) use `prepare_review_archive`, `collect_reviewer_output` and
  `validate_reviewer_output` and feed the reader only `<out-file>` and never
  `<log-file>`.

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

4. Every source location and identifier must satisfy the
   [citation contract](../scripts/review/README.md). Tracked paths (including
   extensionless files and dotfiles) define filenames. Exact paths take precedence;
   basename lookup excludes frozen legacy copies. Absolute file URLs must remain
   beneath the verified archive root. Named anchors check the declaration inside
   the cited range and namespace components in that file. Identifiers resolve only
   from the reviewed tree, the removed diff side, an explicit ABSENT report or a
   pinned external name; export helper files cannot authorize them. Positive
   verdicts require at least one resolving location. All citation failures are
   collected with status `invalid`, before final-message framing checks.

`invalid` describes unusable reviewer evidence, not a verdict. It cannot authorize
landing or advance the next delta base. Consumers fail closed, and rejected
sign-off output is removed. Repository failures, process errors and timeouts
retain their precedence over citation validation.

**Refusals.** A piece that comes back refused, with no verdict line, or with a
non-timeout reviewer error is retried **once**: the piece is halved and each
half is sent with a more neutral prompt that leaves out the project framing. If
any half fails again, the piece is `blocked` and the script moves on. It does
not loop. Refusal text is matched only when the reply has no verdict line, so a
real review that discusses content filters still parses.

## Writer provenance and the writer log (`scripts/review/writer_provenance.py`)

Writer identity comes from the writer harness or an explicit audited backfill in
`refs/notes/concrete-writer`, never from a caller flag, a commit trailer alone,
a self-declared claim, or a default attribution on unstamped commits:

- **SSH-signed entries and [allowed_writer_log_signers](../scripts/review/allowed_writer_log_signers).**
  Every writer-log stanza (`source: harness` and `source: backfill`, including
  corrections) carries `entry-id: <64-hex>` (the sha256 of the canonical
  unsigned key-value lines), `signer: <principal>`, and a detached SSH signature
  block (`-----BEGIN SSH SIGNATURE----- ... -----END SSH SIGNATURE-----`
  produced by `ssh-keygen -Y sign -n git`), verified at read time by
  `ssh-keygen -Y find-principals` and `ssh-keygen -Y verify -n git` against
  [allowed_writer_log_signers](../scripts/review/allowed_writer_log_signers).
  `scripts/review/allowed_writer_log_signers` ships tracked in the repository
  with the owner-approved orchestrator signing key (approved by the project
  owner on 2026-11-08, item 7):
  - Authorized principal: `orchestrator@concrete`
  - Public key (`namespaces="git"`): `ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDH7y2/5tTyKta6QRChKOmMVE3rOy0ajU/kaC2X8VOKF`
  - Fingerprint: `SHA256:vx5cqlK2RywhQLRwaOCplg2BeBCnEBiZnxF8wn/CSLU`
  If `allowed_writer_log_signers` holds no active key lines, every entry is
  unverified and provenance over any range that requires a writer-log entry
  fails closed.
  - *Operator signing instructions on the orchestrator host:*
    1. Keep the dedicated Ed25519 private key on the orchestrator host outside
       any agent worktree or archive (`~/.ssh/concrete_writer_log_ed25519`).
    2. Configure the orchestrator environment (or git config in the orchestrator
       clone) to sign writer-log entries with that key:
       ```sh
       export REVIEW_WRITER_SIGNING_KEY="$HOME/.ssh/concrete_writer_log_ed25519"
       export REVIEW_WRITER_SIGNER="orchestrator@concrete"
       # or in git config:
       git config --global gpg.format ssh
       git config --global user.signingkey ~/.ssh/concrete_writer_log_ed25519
       ```
    3. To rotate or add a key after explicit owner approval, record the line in
       `scripts/review/allowed_writer_log_signers` as
       `<principal> namespaces="git" <keytype> <base64-key>`:
       ```sh
       printf 'orchestrator@concrete namespaces="git" %s\n' \
         "$(awk '{print $1, $2}' ~/.ssh/concrete_writer_log_ed25519.pub)" \
         >> scripts/review/allowed_writer_log_signers
       ```
    4. Stamp (`stamp-commit`) or backfill (`backfill`) the branch's writer-log
       entries so each stanza is signed by the authorized key, and verify with
       `python3 -I scripts/review/writer_provenance.py report --base <base> --head <head>`.
- **Append-only log and signed corrections.** Each commit has one authoritative
  writer history on its own `refs/notes/concrete-writer` note (a note on a
  commit may contain stanzas for that commit only, and exported logs passed via
  `--log-object` are verified against each commit's authoritative note history so
  conflicting or truncated copies are refused). The writer log is append-only: an
  existing entry is never replaced or removed. `backfill` refuses to overwrite
  an existing entry; a correction is a new signed stanza appended to the commit's
  note via `backfill --supersedes <entry-id> --reason "<why>"` naming the latest
  `entry-id` on that commit and stating why. The reader verifies every stanza in
  the chain, reports the full history (`history: <sha>:<old>@r<N>[<id>]->...`),
  and uses the latest valid entry for `writers:`. Any model attested as writer
  in an earlier superseded signed entry remains disqualified from reviewing that
  commit (so relabelling an unstamped Kimi commit can never make Kimi eligible
  to review it). `.githooks/pre-push` (`validate-notes`) requires every nonzero
  `--old-notes-rev` to resolve locally and be compared (distinguishing an absent
  object from the all-zero creation sentinel), checking that no existing commit
  note was deleted and that existing stanzas on every changed commit remain an
  unmodified prefix of the new stanzas, refusing any update that rewrites or
  drops an existing entry.
- **Harness-stamped commits (`source: harness`).** Each harness-written commit
  (`scripts/review/writer_provenance.py stamp-commit`) is stamped with author
  `<Model> (writer) <model@writers.concrete>` (while the committer remains the
  project owner's identity), a `Writer: <model>` trailer, and for `agy` an
  `Agy-Diff-SHA256: <64-hex>` trailer recording the sha256 of the diff
  transported from its throwaway archive copy, plus a matching SSH-signed
  writer-log entry under `refs/notes/concrete-writer`:
  ```
  model: codex|kimi|agy|claude
  round: <positive integer>
  branch: <branch-name>
  brief-sha256: <64-hex>
  start-sha: <40-hex>
  end-sha: <40-hex>
  commit: <40-hex>
  source: harness
  agy-diff-sha256: <64-hex>        # present iff model == agy
  entry-id: <64-hex>
  signer: <principal>
  -----BEGIN SSH SIGNATURE-----
  ...
  -----END SSH SIGNATURE-----
  ```
- **Unstamped commits and `backfill` (`source: backfill`).** An unstamped
  commit has an **unknown** writer and is never attributed to `claude` by
  default; review provenance over a range containing one is refused unless
  `refs/notes/concrete-writer` holds an explicit signed backfilled entry for
  that commit (`scripts/review/writer_provenance.py backfill`). `claude` is a
  writer only when explicitly logged. `backfill` requires `--brief-file <path>`
  and `--output-file <path>`, computing `brief-sha256` and `output-sha256` from
  those files (and verifying `--brief-sha256` / `--output-sha256` if also
  passed). A backfilled entry records:
  ```
  model: codex|kimi|agy|claude
  round: <positive integer>
  branch: <branch-name>
  brief-sha256: <64-hex>
  output-sha256: <64-hex>
  start-sha: <40-hex>
  end-sha: <40-hex>
  commit: <40-hex>
  agy-diff-sha256: <64-hex>        # optional when model == agy
  source: backfill
  evidence: <non-empty single-line provenance description>
  supersedes: <64-hex>             # present iff appending a correction
  reason: <non-empty single-line>  # present iff appending a correction
  entry-id: <64-hex>
  signer: <principal>
  -----BEGIN SSH SIGNATURE-----
  ...
  -----END SSH SIGNATURE-----
  ```
  `backfill` refuses any commit that already carries a harness stamp, and can
  never attribute a commit to a model that is also the `reviewer` on that
  commit's (or `end-sha`'s) review note unless an earlier signed entry already
  did so. At verification time, when the operator supplies `--brief-file`,
  `--output-file`, or `--evidence-dir`, the reader checks the files' sha256
  digests against `brief-sha256` and `output-sha256` (reporting
  `backfill-evidence: verified` when all backfilled commits' files are checked,
  and `backfill-evidence: evidence not re-checked` otherwise).
- **Merge commits.** A merge commit's resolution is authored work when its tree
  differs from git's conflict-free automatic merge of its two parents
  (`git merge-tree --write-tree`); a merge with no resolution adds no writer.
  The harness stamps resolved merges with `Writer: agy` and
  `Agy-Diff-SHA256: <64-hex>` (or a resolved merge may be backfilled); an
  unstamped merge commit whose tree differs from git's automatic merge has an
  unknown writer and is refused.
- **Reader and reporting.** `scripts/review/writer_provenance.py` (`derive`,
  `report`, `verify`, `export-log`) verifies every entry's `entry-id` and SSH
  signature against `scripts/review/allowed_writer_log_signers`, derives and
  cross-checks the ordered unique writer set of `base..HEAD`, distinguishes
  harness entries from backfilled ones, reports superseded correction chains,
  and reports whether backfill evidence files were re-checked. Any commit
  carrying a partial or mismatched stamp (such as a `Writer:` trailer with no
  matching writer-log entry, a writer author without trailer/log, or a
  mismatched `agy-diff-sha256`) is refused. A branch written by multiple models
  across rounds records all of them in order (for example `writer: kimi,agy` or
  `writer: codex,agy`); the reviewer must be none of them (including any model
  attested in an earlier superseded entry).
- **Trust boundary of `refs/notes/concrete-writer`.**
  - *Who may write entries:* Only an authorized signer whose public key is in
    `scripts/review/allowed_writer_log_signers` — specifically (1) the automated
    writer harness via `writer_provenance.py stamp-commit` when creating a
    writer commit or resolved merge (`source: harness`), and (2) the project
    owner or orchestrator via `writer_provenance.py backfill` when recording
    audited provenance or an append-only correction from the session's brief and
    raw writer output log (`source: backfill`). No agent or reviewer holds the
    signing key or hand-edits `refs/notes/concrete-writer`.
  - *What entries attest:* Each signed stanza attests that `commit` (in round
    range `start-sha..end-sha` on `branch`, round `round`) was authored by
    `model` under the brief with digest `brief-sha256`. A `source: harness`
    stanza attests that the commit was created by the harness with matching
    writer author, `Writer: <model>` trailer, owner committer, and (for `agy`)
    `Agy-Diff-SHA256`. A `source: backfill` stanza attests that the unstamped
    commit's writer was verified against the brief file (`brief-sha256`), the
    writer's raw output log (`output-sha256`), and the cited `evidence`.
  - *Hook enforcement:* `.githooks/pre-push` semantically and cryptographically
    validates every push to `refs/notes/concrete-writer` through
    `scripts/review/writer_provenance.py validate-notes` before the review-notes
    tier accepts it, and refuses deleting the ref or rewriting/dropping any
    existing writer-log entry.

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
leg, `ready.sh`, and `notes.sh ready` read it through `verify.sh`. All seven
common keys (`verdict`, `rounds`, `reviewer`, `writer`, `utc`, `base`, `pieces`)
are mandatory on every note; any missing, repeated or unknown key is malformed
(exit 2). Keys are matched by name, not position. A clean note must also carry
`repository: readable`, a `base` that is an ancestor-or-equal of the reviewed
commit, and a `writer` list that matches `writer_provenance.py` over `base..HEAD`
and excludes `reviewer`; clean notes missing `repository: readable` or failing
self-review, writer-provenance or base-coverage checks exit 1. Running
`review.sh` on such a `HEAD` performs a new round rather than short-circuiting.
Change the format only together with `verify.sh`.

> OWNER DECISION, item 5 (2026-11-08, verbatim: "item 5 no old records stayh valid, i want the new record. i don't care about the past. let's make the best long term thing"): no format versioning and no grandfathering. `verify.sh`, `signoff.sh`, `writer_provenance.py`, and `output_contract.py` accept only the current schemas and fail closed on any record or output lacking the required fields and `READ:`/`REPOSITORY: readable`/`VERDICT:` contract. Past landed commits on `main` are not re-judged: validation runs only when recording a note or sign-off and when landing the commit concerned.

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
commit holds one blob per annotated commit and no source tree, and any push to
`refs/notes/concrete-writer` is semantically validated in the hook by
`scripts/review/writer_provenance.py validate-notes` ( deletions of the ref or of
existing entries are refused). A push that also carries a branch takes that
branch's tier.
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
counts as compiler and spec); it counts the rounds used, whatever their verdicts. Collect each
reviewer's final message against the commit's read-only archive with `scripts/land/signoff.sh collect
<sha> --reviewer <codex|kimi|agy> --request <request> --out <output>` (which uses the shared
collector in `scripts/review/collect_output.sh` so Codex writes via `-o <output>` rather than its
stdout log). The approval is recorded, from the owner's own words only, with `scripts/land/signoff.sh
record <sha> --base <base> --rounds-at <the review.sh-noted commit> --date --quote`, plus
`--codex-request <request> --codex <output>` and the corresponding kimi and agy options. For each
reviewer `r`, the recorder stores the verbatim request as `r-request.txt` after prepending
`reviewed: <sha>`, and the output separately as `r.txt`. The request body must name that commit by an
unambiguous prefix, and the output must pass `scripts/review/output_contract.py` (at least one valid
`READ: <path>` from `<sha>`'s tree, `REPOSITORY: readable` as the penultimate non-empty line, and
`VERDICT: ACCEPT` or `VERDICT: REJECT` as the last non-empty line). A completed REJECT is recorded
and reported, not a refusal of the owner's sign-off; an output with no verdict line or failing the
output contract does not qualify. Recording refuses a record that does not qualify.
Push the resulting `refs/owner-signoff/<sha>` to both remotes. The landing train accepts it as that
commit's review provenance ([LANDING.md](LANDING.md)); it approves no other commit.

## verify.sh

```sh
scripts/review/verify.sh <commit>                              # 0 clean provenance; 1 not clean; 2 none/malformed
scripts/review/verify.sh --fetch <commit>                     # fetch the notes ref from origin first
scripts/review/verify.sh --required-base <base> <commit>      # also require review coverage from base
scripts/review/verify.sh --fields <commit>                    # 0 well-formed note fields; 2 none/malformed
scripts/review/verify.sh --evidence-dir <dir> <commit>        # re-check backfill brief/output files
```

It judges the exact commit. A clean note on an ancestor says nothing about the
commits after it, so a branch that gains a commit after its clean round needs
another round. A clean note without `repository: readable`, or a clean note
whose reviewer equals its writer, exits 1; a note missing `writer` or any other
required field is malformed and exits 2.

With `--fields`, `verify.sh` validates the note's schema and prints its
structured `key: value` fields (`commit`, `verdict`, `rounds`, `reviewer`,
`writer`, `utc`, `base`, `pieces`, `repository`) with exit 0 on any well-formed
note, independent of clean-provenance eligibility (including `findings`,
`blocked`, and provenance-invalid `clean` notes), exiting 2 only when no note
exists or the note is malformed. `signoff.sh` and `review.sh` read note fields
through `verify.sh --fields` rather than scraping `verify.sh`'s human summary,
so valid cap evidence on a `findings`, `blocked` or provenance-invalid `clean`
note is preserved.

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
non-fast-forward merge), verdict parsing, the single reviewer-output collector
and its structural check, archived-tree access for every reviewer, independent
reviewer/writer provenance and its refusal cases, SSH signature verification
against `allowed_writer_log_signers` (including fail-closed behaviour when no
key is configured), append-only writer-log corrections and relabelling defence,
backfill evidence verification, `verify.sh --fields`, both prompts' permanent
owner rule, prompt persistence and hashes, blocked repository-access failures,
and required-base coverage. `scripts/checks/landing_script.sh` repeats the A→B→C
reproducer through the landing train for both head and member notes and tests
`signoff.sh` cap counting and historical sign-off non-re-verification.

### Citation resolution (ISS-0040)

Citation contract: use repository-relative path:N[-M][,N[-M]...] or path#LN-LM, optionally as Markdown links. En-dash ranges are accepted. Absolute file:// links must be beneath the exact checkout or verified archive root. Bare basenames must be unique and cannot implicitly select legacy/. Basename labels resolve through an immediately following exact link with the same numeric starting line. A ``[`name`](path#LA-LB)`` link asserts that the final name component occurs inside those lines and namespace components occur in that file. Every citation must resolve; invented citations make the output invalid. Backticked dotted, underscore or camelCase identifiers must resolve component by component in the reviewed tree, removed side of the reviewed diff, or the pinned external-name list. Identifier authority excludes legacy/, ISSUES.tsv, scripts/review/fixtures/, scripts/review/test_*.py and the external-name registry itself. Report a missing identifier with a standalone ABSENT: name line. For proposed fixes in FINDINGS/REJECT use `proposed:newName`. Plain words and hash ranges are not identifiers. Positive CLEAN/clean/ACCEPT requires READ: paths actually read and at least one resolving file:line citation supporting the conclusion. ALWAYS do the best long term thing rather than cutting corners. A bare `proposed:` is grammar documentation, not a proposal; any nonempty proposed suffix remains forbidden for positive verdicts. To cite a name only as quoted text, use [`text:name`](path#LA-LB); the exact name must occur within that range. Text links never establish identifier authority or support a positive verdict. Bare names still claim tree authority even near fixture citations. Declared harness files may be READ but are never evidence. Links outside the reviewed roots are invalid.

The exact identifier lexicon, external-name policy and fixture provenance are specified in [the contract README](../scripts/review/README.md). Unresolved citations return `invalid`, with every unresolved citation in the reason. This is distinct from repository access failure (`blocked`) and never counts as a verdict. Unknown statuses fail closed.
