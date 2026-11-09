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
  to review it). Validation of writer-history states in `writer_provenance.py`
  is structured as two separate predicates required in order by every reader,
  writer, validator, and checkpoint ratchet:
  1. **State validity (`validate_writer_history_state`):** a pure function of
     the writer-history state alone (notes tree layout, every note blob's syntax,
     every stanza's `entry-id` and SSH signature against authorized signers in
     `allowed_writer_log_signers`, single-commit note authority, and commit-level
     semantic rules for every annotated commit present in the repository),
     computed in full on every read and never derived from, cached by, or skipped
     because of an old ref, checkpoint, blob identity, or option (in-process
     memoization is keyed only by content hash of a blob and signers file already
     validated in the current process).
  2. **Continuity (`verify_writer_history_continuity`):** a separate predicate
     verifying that the state is a fast-forward descendant and append-only stanza
     prefix extension of `--old-notes-rev`, every ancestor commit in the notes
     ref's history, and every companion checkpoint (`REVIEW_WRITER_NOTES_REF`,
     `refs/notes/concrete-writer`, `refs/notes/remote/concrete-writer`, and any
     caller-supplied `--old-notes-rev`).
  `.githooks/pre-push` (`validate-notes`) runs unconditionally on every push
  before any `CONCRETE_SKIP_GATES` bypass, refuses deleting
  `refs/notes/concrete-writer`, requires every nonzero `--old-notes-rev` to
  resolve locally, and enforces state validity followed by continuity across all
  notes (including notes unchanged from `--old-notes-rev`). `verify.sh --fetch`,
  `scripts/land/notes.sh` (`fetch`, `get writer`, `ready`), and
  `scripts/land/train.sh` share `writer_provenance.py fetch-notes`, which
  requires state validity first and continuity second before advancing any
  checkpoint ref or returning a writer-notes state, never deletes a checkpoint
  ref, and refuses a missing remote or local ref whenever any prior writer
  history was recorded. `scripts/land/notes.sh put` refuses `writer` notes
  (`stamp-commit`, `backfill`, `fetch-notes`, `sync-notes`, and `publish-notes`
  in `writer_provenance.py` are the only writer-note ref APIs).

### Writer-history continuity: one predicate

Rounds 10–13 exposed a single structural defect class: writer-history continuity was split across readers and validators that each inspected a different projection of the repository state. Specifically, on the round-13 tree (`fb8425686`):

- **Range filtering inside the continuity predicate (`scripts/review/writer_provenance.py:1105`, `1966`, `2098-2099`, `2126-2127`, `2177`, `2190`):** `derive_range_provenance` (`scripts/review/writer_provenance.py:1105`) calls `read_validated_writer_notes(repo, allowed_signers_path, target_commits=set(commits))`, which forwards `target_commits` to `verify_writer_history_continuity` (`scripts/review/writer_provenance.py:2186-2191`). Inside `verify_writer_history_continuity`, the `git log --raw` walk skips every annotated commit `c` not in `target_commits` (`if target_commits is not None and c not in target_commits: continue` at `scripts/review/writer_provenance.py:2098-2099` and `2126-2127`), and the direct tree comparison (`scripts/review/writer_provenance.py:1981-2017`) runs only when an explicit `old_notes_rev` is passed. Deleting or rolling back a valid note on a historical commit outside `base..head` therefore passes `derive`, `report`, `verify`, and `export-log` over `base..head`.
- **Companion-checkpoint ancestry checked only for an explicit `old_notes_rev` (`scripts/review/writer_provenance.py:2026-2049`, `2164-2171`):** `verify_writer_history_continuity` collects companion checkpoints (`WRITER_NOTES_REF`, `refs/notes/concrete-writer`, `refs/notes/remote/concrete-writer`, and `extra_revs`) into `rev_roots` (`scripts/review/writer_provenance.py:2026-2037`), but checks `is_ancestor(repo, old_sha, notes_sha)` only when `old_notes_rev` is explicitly supplied and nonzero (`scripts/review/writer_provenance.py:2164-2171`). When `old_notes_rev` is omitted (`validate_notes_ref` at `scripts/review/writer_provenance.py:2195-2216`, `read_validated_writer_notes` at `2174-2192`) or is the creation sentinel `0000...` (`.githooks/pre-push:70`), a parentless root notes commit or a divergent notes commit with identical valid note blobs passes `git log --raw` blob-prefix checks without descending from the companion checkpoints by commit ancestry.
- **Check-to-use ref re-reads (`scripts/review/writer_provenance.py:920`, `1105-1118`, `1409-1476`, `1545-1630`, `2352-2355`, `2403-2406`, `2474`, `2504`, `2610`; `scripts/land/notes.sh:63-71`):** `derive_range_provenance` (`scripts/review/writer_provenance.py:1105`) discards the dictionary returned by `read_validated_writer_notes` and re-reads `WRITER_NOTES_REF` per commit via `inspect_commit` (`scripts/review/writer_provenance.py:1116-1118` -> `920` -> `get_commit_note_history` at `738-747` -> `read_optional_note` at `677-689`), while `notes.sh get writer` (`scripts/land/notes.sh:63-71`) validates `$ref` with `fetch-notes --no-fetch` (`63`) and then re-resolves `$ref` via `git for-each-ref` and `git notes --ref="$ref" show` (`69-71`).

#### 1. The single invariant over the complete writer-notes state

Let $N \in \text{Commit} \cup \{\bot\}$ be the candidate writer-notes commit (or $\bot$ when the candidate ref does not exist) in `repo`, and let the prior checkpoint set $\mathcal{P}$ be the deduplicated set of all nonzero 40-hex commit SHAs resolved from:
1. every caller-supplied `old_notes_rev` / `old_notes_revs` (`--old-notes-rev`),
2. any caller-supplied `target_ref` (`--target-ref`),
3. the active `WRITER_NOTES_REF` (`REVIEW_WRITER_NOTES_REF`, defaulting to `refs/notes/concrete-writer` at `scripts/review/writer_provenance.py:122`),
4. `refs/notes/concrete-writer`, and
5. `refs/notes/remote/concrete-writer`.

The complete writer-notes state satisfies continuity if and only if **all three** clauses below hold over the **entire** repository state, with no commit-range filter (`target_commits`) anywhere in the predicate:

1. **Complete candidate validity (signatures, principals, keys, and commit semantics):**
   - **Full local object and history availability:** `repo` is not a shallow repository (`git rev-parse --is-shallow-repository` is `false`), no element of `old_notes_revs` is an empty string (`""`), and every nonzero revision in $\{N\} \cup \mathcal{P}$ resolves locally to a commit object (`commit_exists` / `resolve_commit` at `scripts/review/writer_provenance.py:243-249`, `274-295`).
   - **Non-deletion of the ref:** If $N = \bot$, then $\mathcal{P} = \emptyset$. Whenever $\mathcal{P} \neq \emptyset$, $N = \bot$ is rejected.
   - **Whole-tree cryptographic and semantic validity:** If $N \neq \bot$, the notes tree `list_notes_tree(repo, N)` (`scripts/review/writer_provenance.py:1638-1669`) is non-empty and maps only 40-hex commit paths (`100644 blob`) to non-empty blobs. For **every** annotated commit $c \in \text{dom}(N)$ across the entire notes tree:
     - `allowed_writer_log_signers` (`resolve_allowed_signers` and `check_allowed_signers_has_keys` at `scripts/review/writer_provenance.py:170-204`) exists and contains at least one active `<principal> namespaces="git" <keytype> <base64-key>` line.
     - Every stanza in $N[c]$ (`parse_and_verify_note_blob` at `scripts/review/writer_provenance.py:704-735`, `parse_multi_stanza_log` at `623-658`, `parse_log_stanza` at `459-613`, `verify_stanza_signature` at `414-456`) has valid key-value syntax, canonical key ordering, an `entry-id` equal to the SHA-256 of its canonical unsigned body, a `signer` principal matching the key's authorized principal in `allowed_writer_log_signers` (`ssh-keygen -Y find-principals`), and a valid OpenSSH signature in namespace `"git"` (`ssh-keygen -Y verify -n git`).
     - Single-commit note authority holds: every stanza in $N[c]$ has `commit == c`, stanza 1 has no `supersedes` or `reason`, and each stanza $k > 1$ has `supersedes` equal to stanza $k-1$'s `entry-id` with a non-empty single-line `reason`.
     - Commit-level semantics hold for $c$ (`validate_writer_history_state` at `scripts/review/writer_provenance.py:1852-1954`): commit $c$ must exist in `repo` (unless `allow_missing_targets=True` is explicitly passed by `validate-notes --allow-missing-targets`), its committer is the owner's identity (never `(writer)` or `@writers.concrete`), `start-sha` is a strict ancestor of $c$, $c$ is an ancestor-or-equal of `end-sha`, `end-sha` is an ancestor-or-equal of the latest stanza's `end-sha`, and `source: harness` vs `source: backfill` matches $c$'s author and `Writer:` / `Agy-Diff-SHA256:` trailers.
2. **No deletion or rollback of any existing entry (complete tree and complete ancestry):**
   - **Direct checkpoint tree inclusion and stanza-prefix extension:** For **every** checkpoint $P \in \mathcal{P}$ and **every** annotated commit $c \in \text{dom}(P)$:
     - $c \in \text{dom}(N)$ (no commit note present in any checkpoint $P \in \mathcal{P}$ may be absent from $N$, whether $c$ is inside or outside any caller's `base..head` range).
     - $\text{stanzas}(P[c]) = \text{stanzas}(N[c])[0 : |\text{stanzas}(P[c])|]$ (the raw stanza list of $P[c]$ is an exact prefix of the raw stanza list of $N[c]$; no stanza may be dropped, modified, replaced in place, or rolled back).
   - **Full ancestor-graph inclusion and stanza-prefix extension:** Across `git log --raw --root -m --no-abbrev` of $\{N\} \cup \mathcal{P}$ (`scripts/review/writer_provenance.py:2047-2162`), for **every** ancestor notes commit $A$ and **every** annotated commit $c$ touched in $A$ (with **no** `target_commits` filter) whose note blob $B$ in $A$ is a valid signed note (`_blob_has_valid_signed_note(c, B, eff_signers)` at `scripts/review/writer_provenance.py:1682-1691`), $c \in \text{dom}(N)$ and $\text{stanzas}(B)$ is an exact prefix of $\text{stanzas}(N[c])$.
3. **Descent from every companion checkpoint by commit ancestry, never by content equality:**
   - For **every** checkpoint $P \in \mathcal{P}$, $P$ must be a git commit ancestor-or-equal of $N$ (`is_ancestor(repo, P, N)` via `git merge-base --is-ancestor P N` at `scripts/review/writer_provenance.py:298-305`):
     $$\forall P \in \mathcal{P},\quad P \preceq_{\text{commit}} N$$
   - Tree equality ($N\text{^{tree}} == P\text{^{tree}}$) or identical valid note blobs **never** substitute for commit ancestry: an orphan/root notes commit or a sibling/divergent notes commit carrying identical blobs fails $P \preceq_{\text{commit}} N$ and is rejected.

#### 2. The one function that owns the predicate and how every caller changes

`verify_writer_history_continuity` (`scripts/review/writer_provenance.py:1960-2171`) is the **single** function that owns this complete invariant:

```python
def verify_writer_history_continuity(
    repo: str,
    notes_rev: str | None,
    old_notes_revs: list[str] | None = None,
    check_refs: list[str] | None = None,
    allowed_signers_path: str | None = None,
    allow_missing_targets: bool = False,
) -> tuple[str | None, dict[str, list[dict[str, str]]]]:
```

`verify_writer_history_continuity` has **no `target_commits` parameter**. On every call it:
1. Checks `git rev-parse --is-shallow-repository` and fails closed if `"true"`.
2. Validates `old_notes_revs` (refusing any `""` entry and any nonzero SHA not present as a commit in `repo`) and collects all prior checkpoints $\mathcal{P}$ via `collect_prior_writer_checkpoints` (`scripts/review/writer_provenance.py:2219-2252`) over `old_notes_revs` and `check_refs` (always including `WRITER_NOTES_REF`, `refs/notes/concrete-writer`, and `refs/notes/remote/concrete-writer`).
3. If `notes_rev` is `None`: returns `(None, {})` when $\mathcal{P} = \emptyset$, or raises `ProvenanceError` (`local <ref> is missing, but prior writer-log history was recorded at ...`) when $\mathcal{P} \neq \emptyset$.
4. Resolves `notes_sha = resolve_commit(repo, notes_rev, "notes-rev")` and calls `validate_writer_history_state(repo, notes_sha, allowed_signers_path=allowed_signers_path, allow_missing_targets=allow_missing_targets)` (`scripts/review/writer_provenance.py:1763-1957`) to validate the complete candidate state (where `validate_writer_history_state` requires every annotated commit $c \in \text{dom}(N)$ to exist in `parsed_commits` unless `allow_missing_targets=True`).
5. For **every** checkpoint $P \in \mathcal{P}$:
   - Compares `list_notes_tree(repo, P)` against `new_entries` (`scripts/review/writer_provenance.py:1993-2017`) across all commits in $P$, rejecting any deleted note or non-prefix stanza change before any ancestry or log check.
   - Enforces commit ancestry `is_ancestor(repo, P, notes_sha)` (`scripts/review/writer_provenance.py:2167-2171`), rejecting any non-ancestor checkpoint even when its blobs are identical to `notes_sha`.
6. Walks the full commit graph `git log --raw --root -m --no-abbrev --format=COMMIT %H` of `[notes_sha, *P]` (`scripts/review/writer_provenance.py:2047-2162`) over **all** annotated commits (with lines `2098-2099` and `2126-2127` deleted) so any historical note deletion or stanza rewrite in the ancestor graph is rejected.
7. Returns `(notes_sha, validated_state)`, where `validated_state` is the complete validated `{commit_sha: [stanza_dict, ...]}` snapshot at `notes_sha`.

Every current caller in the repository changes as follows to call `verify_writer_history_continuity` on the full state before selecting any range, and to consume the returned `(notes_sha, validated_state)` snapshot rather than re-reading mutable refs:

| Caller (`file:line`) | Role | Current behaviour on `fb8425686` | How it changes |
|---|---|---|---|
| `verify_writer_history_continuity` (`scripts/review/writer_provenance.py:1960-2171`) | Owner of the continuity predicate | Accepts `old_notes_rev: str \| None` (`1963`), `extra_revs` (`1964`), and `target_commits: set[str] \| None` (`1966`); skips commits not in `target_commits` at `2098-2099` and `2126-2127`; checks `old_entries` vs `new_entries` (`1993-2017`) and `is_ancestor` (`2164-2171`) only for `old_notes_rev`; does not call `validate_writer_history_state` itself; returns `None`. | Drops `target_commits` parameter and range filters (`2098-2099`, `2126-2127`); rejects shallow repos; runs `validate_writer_history_state` (`1763-1957`) on `notes_sha`; enforces full-tree prefix continuity and `is_ancestor(repo, P, notes_sha)` for **every** checkpoint $P \in \mathcal{P}$ (both `old_notes_revs` and companion refs `2026-2037`); walks full `git log --raw` history without filtering; returns `(notes_sha, validated_state)`. |
| `validate_writer_history_state` (`scripts/review/writer_provenance.py:1763-1957`) | Pure candidate-state validator (called as step 1 inside `verify_writer_history_continuity`) | Validates tree layout, note blobs, `entry-id`, SSH signatures/principals, and commit-level rules, but silently skips commit-level checks when `annotated_sha not in parsed_commits` (`1853-1854`). | Accepts `allow_missing_targets: bool = False`; when `allow_missing_targets` is `False` (the default for all readers, mutators, `fetch-notes`, `sync-notes`, and `.githooks/pre-push`), raises `ProvenanceError` if any annotated commit $c \in \text{dom}(N)$ is missing from `parsed_commits` (`1853-1854`). Called unconditionally by `verify_writer_history_continuity`. |
| `read_validated_writer_notes` (`scripts/review/writer_provenance.py:2174-2192`) | Local active-ref reader helper | Accepts `target_commits` (`2177`), calls `validate_writer_history_state` (`2185`) and `verify_writer_history_continuity(..., target_commits=target_commits)` (`2186-2191`), and returns `state` without `cur_sha`. | Removes `target_commits` parameter; delegates directly to `verify_writer_history_continuity(repo, resolve_optional_ref(repo, WRITER_NOTES_REF), allowed_signers_path=allowed_signers_path)` and returns `(cur_sha, validated_state)`. |
| `validate_notes_ref` (`scripts/review/writer_provenance.py:2195-2216`) and `cmd_validate_notes` (`2619-2632`) | Full candidate validator (`validate-notes` CLI) | `validate_notes_ref` discards `allow_missing_targets` (`2204`), calls `validate_writer_history_state` (`2205-2209`), and calls `verify_writer_history_continuity` with `old_notes_rev=old_notes_rev` (`2210-2215`), leaving companion checkpoints unchecked for `is_ancestor` when `old_notes_rev` is `None` or `ZERO40`. | Calls `_, validated_state = verify_writer_history_continuity(repo, notes_rev, old_notes_revs=[old_notes_rev] if old_notes_rev is not None else None, allowed_signers_path=allowed_signers_path, allow_missing_targets=allow_missing_targets)` and returns `len(validated_state)`. |
| `derive_range_provenance` (`scripts/review/writer_provenance.py:1076-1170`) | Range provenance reader for `base..head` | Calls `read_validated_writer_notes(repo, allowed_signers_path, target_commits=set(commits))` at `1105` (filtering continuity to `commits` and discarding the returned state), then loops `inspect_commit(repo, c, head_sha, None, eff_signers)` (`1115-1118`), which at `920` calls `get_commit_note_history` (`738-747`) -> `read_optional_note` (`677-689`) to re-read `WRITER_NOTES_REF` from git per commit. | Calls `_, validated_state = verify_writer_history_continuity(repo, resolve_optional_ref(repo, WRITER_NOTES_REF), allowed_signers_path=allowed_signers_path)` on the **complete** state before iterating over `commits`, and passes `validated_state` into `inspect_commit(repo, c, head_sha, validated_state, eff_signers)` so `inspect_commit` (`917-920`) reads only the validated in-memory snapshot and never re-reads `WRITER_NOTES_REF` from git. |
| `inspect_commit` (`scripts/review/writer_provenance.py:877-970`) and `get_commit_note_history` (`738-747`) | Per-commit stamp vs note inspector | When `external_log is None`, `inspect_commit` (`920`) calls `get_commit_note_history(repo, sha, allowed_signers_path)` which reads `WRITER_NOTES_REF` directly from git (`743`) without continuity validation. | `get_commit_note_history` is removed (or restricted to call `verify_writer_history_continuity` first); `inspect_commit` receives the already-validated `validated_state: dict[str, list[dict[str, str]]]` from `verify_writer_history_continuity` (or single-entry dict in `stamp-commit`/`backfill` pre-check) and looks up `validated_state.get(sha)` at `917-920`. |
| `cmd_derive` (`scripts/review/writer_provenance.py:1267-1295`), `cmd_report` (`1298-1314`), `cmd_verify` (`1317-1346`), `cmd_export_log` (`1349-1380`) | Range reader CLI subcommands (`derive`, `report`, `verify`, `export-log`) | Call `derive_range_provenance` (`1269`, `1300`, `1326`, `1351`), inheriting the `target_commits` range filter at `1105` and the missing companion `is_ancestor` check at `2164-2171`. | Call `derive_range_provenance`, which now runs `verify_writer_history_continuity` over the complete writer-notes state and all companion checkpoints before projecting onto `base..head`. |
| `cmd_stamp_commit` (`scripts/review/writer_provenance.py:1383-1481`) | Harness writer-note mutator (`stamp-commit`) | Calls `read_validated_writer_notes(..., target_commits=None)` (`1409`), amends `HEAD` (`1434-1441`), re-reads `read_optional_note(args.repo, WRITER_NOTES_REF, new_sha)` (`1446`), and mutates `WRITER_NOTES_REF` in place with `git notes --ref={WRITER_NOTES_REF} add` (`1473-1476`) without a compare-and-swap (CAS) check on the prior ref SHA. | Calls `old_notes_sha, validated_state = verify_writer_history_continuity(args.repo, resolve_optional_ref(args.repo, WRITER_NOTES_REF), allowed_signers_path=args.allowed_signers)` (`1409`), checks `new_sha not in validated_state` from the snapshot, builds the new notes commit on a temporary ref from `old_notes_sha`, validates it with `verify_writer_history_continuity(..., old_notes_revs=[old_notes_sha or ZERO40])`, and atomically updates `WRITER_NOTES_REF` via `git update-ref WRITER_NOTES_REF new_notes_sha (old_notes_sha or ZERO40)`. |
| `cmd_backfill` (`scripts/review/writer_provenance.py:1484-1635`) | Audited backfill and correction mutator (`backfill`, `correct`) | Calls `read_validated_writer_notes(..., target_commits=None)` (`1545`), then re-reads `read_optional_note(args.repo, WRITER_NOTES_REF, commit_sha)` (`1547`), and overwrites the note via `git notes --ref={WRITER_NOTES_REF} add -f` (`1627-1630`) without a CAS check on `WRITER_NOTES_REF`. | Calls `old_notes_sha, validated_state = verify_writer_history_continuity(args.repo, resolve_optional_ref(args.repo, WRITER_NOTES_REF), allowed_signers_path=args.allowed_signers)` (`1545`), takes `existing_history = validated_state.get(commit_sha, [])` from the validated snapshot, builds the new notes commit on a temporary ref from `old_notes_sha`, validates it with `verify_writer_history_continuity(..., old_notes_revs=[old_notes_sha or ZERO40])`, and atomically CAS-updates `WRITER_NOTES_REF` from `old_notes_sha or ZERO40` to `new_notes_sha`. |
| `fetch_notes_ref` (`scripts/review/writer_provenance.py:2255-2410`) and `cmd_fetch_notes` (`2635-2655`) | Remote/local checkpoint validator and ratchet (`fetch-notes`) | Runs `validate_writer_history_state` (`2295`, `2331`, `2381`) and loops `verify_writer_history_continuity` over `priors` (`2302`, `2338`, `2388`) or `ZERO40` (`2346`, `2396`), then runs `git update-ref` (`2310-2319`, `2352-2355`, `2403-2406`) without expected-old-SHA CAS arguments. | Calls `verify_writer_history_continuity(repo, cand_sha, old_notes_revs=priors or [ZERO40], check_refs=check_refs, allowed_signers_path=allowed_signers_path)` once in the ratchet (`2293-2309`), in the `no_fetch` branch (`2331-2351`), and in the remote fetch branch (`2381-2401`), and passes the initial resolved ref SHAs to `git update-ref <ref> <new_sha> <expected_old_sha>` so concurrent ref replacement between check and update is refused. |
| `sync_notes_refs` (`scripts/review/writer_provenance.py:2412-2616`), `cmd_sync_notes` (`2658-2671`), `cmd_publish_notes` (`2674-2701`) | Append-only notes synchronizer and publisher (`sync-notes`, `publish-notes`) | Splits validation across fast-forward/ancestor branches calling `validate_writer_history_state` (`2454`, `2457`) and `validate_notes_ref` (`2460-2529`) and a divergent merge branch calling `verify_writer_history_continuity` (`2539-2544`, `2598-2609`), then runs `git update-ref` (`2474`, `2504`, `2610`) without CAS old-SHA arguments. | Validates candidate inputs and the resulting `target_sha` (`remote_sha`, `local_sha`, or two-parent `merged_sha` created with `-p local_sha -p remote_sha` at `2582-2594`) through `verify_writer_history_continuity(repo, target_sha, old_notes_revs=[r for r in (local_sha, remote_sha, prev_remote_sha) if r], allowed_signers_path=allowed_signers_path)`, and atomically CAS-updates `WRITER_NOTES_REF` and `refs/notes/remote/concrete-writer` with their expected old SHAs. |
| `.githooks/pre-push` (`.githooks/pre-push:65-78`) | Push-time notes gate | Runs `writer_provenance.py validate-notes --repo "$ROOT_DIR" --notes-rev "$localsha" --old-notes-rev "${_remotesha:-}"` (`70`) and refuses deletion (`75-77`) before `CONCRETE_SKIP_GATES` (`108-111`). When `_remotesha` is `0000...`, companion checkpoints in the local clone were not checked for `is_ancestor` (`writer_provenance.py:2164-2171`). | Invokes `validate-notes` at `.githooks/pre-push:70`, which now executes the unified `verify_writer_history_continuity` over the full state and enforces `is_ancestor` and full-tree prefix continuity against both `--old-notes-rev` and every local companion checkpoint even when `_remotesha` is `0000...`. |
| `scripts/review/review.sh` (`scripts/review/review.sh:159`, `211`, `223`, `263`) | Independent branch reviewer | Calls `writer_provenance.py sync-notes` (`159`, invoked at `184` and `211`), `writer_provenance.py derive` (`223`), and `writer_provenance.py verify` (`263`). | All three calls (`159`, `223`, `263`) execute `verify_writer_history_continuity` on the complete writer-notes state before selecting `BASE_SHA..HEAD_SHA` or `nb..c`, aborting `review.sh` before any reviewer binary runs if any note in the repository was deleted, rolled back, or divorced from a checkpoint. |
| `scripts/review/verify.sh` (`scripts/review/verify.sh:72`, `182`) | Review-note verifier | Under `--fetch`, calls `writer_provenance.py fetch-notes` (`72`); for clean notes, calls `writer_provenance.py verify --base "$(field base)" --head "$SHA"` (`182`). | Both `fetch-notes` (`72`) and `verify` (`182`) call `verify_writer_history_continuity` on the complete writer-notes state before inspecting `base..SHA`. |
| `scripts/land/notes.sh` (`scripts/land/notes.sh:42`, `63-71`, `150-152`) and `scripts/land/ready.sh` (`96-98`) | Readiness and note reader (`notes.sh fetch`, `get writer`, `ready`) | `fetch_kind` (`42`) calls `fetch-notes`; `ready` (`150-152`, called by `ready.sh:98`) calls `verify.sh`; `get_note` (`63-71`) runs `wout="$(python3 -B -I "$WRITER_PROV" fetch-notes --no-fetch --target-ref "$ref" --no-update-local 2>&1)"` (`63`) and then re-resolves `ref_sha="$(git for-each-ref ... "$ref")"` (`69`) and runs `git notes --ref="$ref" show "$2"` (`71`). | `get_note` (`scripts/land/notes.sh:63-71`) binds its read to the exact validated 40-hex commit SHA printed to `wout` by `fetch-notes --no-fetch` (`[ -z "$wout" ] && return 0; git notes --ref="$wout" show "$2"`), eliminating the check-to-use re-resolution of `$ref` at `69-71`. `fetch` (`42`) and `ready` (`150-152`) inherit the unified predicate. |
| `scripts/land/signoff.sh` (`scripts/land/signoff.sh:191`) | Owner sign-off recorder and verifier (`signoff.sh record`, `verify`) | Calls `writer_provenance.py derive --repo . --base "$base" --head "$sha"` (`191`) inside `check()` (`148-229`). | Through `derive` (`191`), validates the complete writer-notes state and checkpoint ancestry via `verify_writer_history_continuity` before checking `base..sha`. |
| `scripts/land/train.sh` (`scripts/land/train.sh:338`, `559`, `570`, `585`, `586`) | Landing train (`train.sh build`, `check`, `land`) | `cmd_build` (`338`) and `review_provenance` (`559`, called from `review_leg` at `605`, `609` and `cmd_land` at `1113`) call `writer_provenance.py fetch-notes ... ${prev_wnotes:+--old-notes-rev "$prev_wnotes"}` and then `verify.sh` (`570`, `585`) and `signoff.sh verify` (`586`). | Every train entry point (`338`, `559`, `570`, `585`, `586`) enforces `verify_writer_history_continuity` over the complete state and all checkpoints (including the train's persisted `writer_notes` checkpoint at `162-169`, `335`, `558`). |

#### 3. Continuity-loss channels, planted controls, and exact diagnostics

Every channel by which a writer-notes state can lose validity or continuity is rejected by `verify_writer_history_continuity` and pinned by a planted control in `scripts/checks/review_tool.sh` and `scripts/checks/landing_ready.sh`:

| Channel | How the state loses continuity | Planted control that must reject it | Exact diagnostic |
|---|---|---|---|
| **1. Note deletion inside the range (`base..head`)** | A valid signed note on a commit $c \in \text{commits}(\text{base}..\text{head})$ present in a prior checkpoint $P \in \mathcal{P}$ or ancestor notes commit $A$ is removed in candidate $N$. | Create valid signed notes on $c_1, c_2$, record the notes tip at `refs/notes/remote/concrete-writer` (and test as parent commit $A$ with no companion ref), remove the note on $c_2$, and invoke `derive`, `report`, `verify`, `export-log` over `c1..c2`, `validate-notes`, `fetch-notes`, `verify.sh`, `notes.sh`, `train.sh`, and `.githooks/pre-push` (with and without `CONCRETE_SKIP_GATES=1`). | Against checkpoint $P \in \mathcal{P}$: `writer-notes push deletes existing writer-log entry for commit(s): <c[:10]>`<br>Against ancestor notes commit $A$: `writer-notes push deletes existing writer-log entry for commit(s): <c[:10]> (present in writer-notes commit <A[:10]>)` |
| **2. Note deletion outside the range (`base..head`)** *(Round-13 HIGH finding)* | A valid signed note on a commit $c_{\text{out}} \notin \text{commits}(\text{base}..\text{head})$ is removed in candidate $N$, while all notes on commits inside `base..head` remain valid. | Reproduce the Round-13 HIGH scenario: create two valid signed commit notes on $c_1$ and $c_2$, record the notes tip as the companion checkpoint (and in a second sub-case as a parent notes commit with no companion ref), delete the note on $c_1$, and run `derive`, `report`, `verify`, `export-log`, `review.sh`, `verify.sh`, and `signoff.sh verify` on the range covering only $c_2$ (`c1..c2`), plus `validate-notes`, `fetch-notes`, and `notes.sh get writer`. Include a negative-control mutation re-inserting `target_commits` filtering to prove the control fails if range filtering returns. | Against checkpoint $P \in \mathcal{P}$: `writer-notes push deletes existing writer-log entry for commit(s): <c1[:10]>`<br>Against ancestor notes commit $A$: `writer-notes push deletes existing writer-log entry for commit(s): <c1[:10]> (present in writer-notes commit <A[:10]>)` |
| **3. Correction rollback (inside or outside the range, or re-signing in place)** | A commit note with a valid 2-stanza correction chain ($s_1 \to s_2$) is rolled back to its 1-stanza prefix $s_1$ (by rewinding the ref to $N_1$, committing a rollback child $N_3$ on top of $N_2$, or replacing the note with a freshly signed 1-stanza note $s_1'$), on a commit inside or outside `base..head`, with or without a matching 1-stanza `--log-object`. | Build 1-stanza $N_1$ and 2-stanza $N_2$ (`claude -> kimi` / `kimi -> codex`) on both $c_{\text{out}} \notin \text{base}..\text{head}$ and $c_{\text{in}} \in \text{base}..\text{head}$; test rewinding to $N_1$ (with $N_2$ at companion checkpoint), child rollback $N_3$, and re-signed replacement $N_3'$ across `derive`, `report`, `verify` (with and without `--log-object`), `export-log`, `stamp-commit`, `backfill`, `correct`, `validate-notes`, `fetch-notes`, `sync-notes`, `publish-notes`, `review.sh`, `verify.sh`, `notes.sh`, `train.sh`, and `.githooks/pre-push`. | Against checkpoint $P \in \mathcal{P}$: `writer-notes push rewrites or drops existing writer-log entry on <c[:10]> (the writer log is append-only; corrections must append a signed superseding entry)`<br>Against ancestor notes commit $A$: `writer-notes push rewrites or drops existing writer-log entry on <c[:10]> from writer-notes commit <A[:10]> (the writer log is append-only; corrections must append a signed superseding entry)` |
| **4. Unrelated root with identical blobs** *(Round-13 MEDIUM finding)* | Candidate $N_{\text{root}}$ is a parentless notes commit (`git commit-tree <P^{tree}>`) carrying the exact same tree and valid signed note blobs as checkpoint $P \in \mathcal{P}$, but does not descend from $P$ by commit ancestry. | Reproduce the Round-13 MEDIUM scenario: create two valid signed commit notes at $P$, record $P$ as the companion checkpoint (`refs/notes/remote/concrete-writer`), create parentless $N_{\text{root}}$ with $P$'s exact tree, point `refs/notes/concrete-writer` at $N_{\text{root}}$, and test `validate-notes` (with no `--old-notes-rev` and with `--old-notes-rev 0000...`), `derive`, `report`, `verify`, `export-log`, `stamp-commit`, `backfill`, `fetch-notes --no-fetch`, `fetch-notes` (from remote), `notes.sh get writer`, `verify.sh`, `train.sh`, and `.githooks/pre-push`. Include a negative-control mutation replacing `is_ancestor` with tree equality to prove the control fails if content equality is accepted. | `writer-notes push is non-fast-forward (<P[:10]> is not an ancestor of <N_root[:10]>): refs/notes/concrete-writer history is append-only and may not be rewound or rewritten` |
| **5. Divergent history (with identical or superset valid blobs)** *(Round-13 MEDIUM finding)* | From common notes ancestor $P_0$, checkpoint $P_1$ and candidate $N_2$ diverge as sibling commits ($P_1 \not\preceq_{\text{commit}} N_2$) where $N_2$ has the exact same valid note blobs as $P_1$ (or a strict superset of valid note blobs) without having $P_1$ as an ancestor. | Create common ancestor $P_0$ (note on $c_1$), child checkpoint $P_1$ (adding note on $c_2$) at `refs/notes/remote/concrete-writer`, and sibling commit $N_2$ (parent $P_0$, identical tree $P_1\text{^{tree}}$ in sub-case A and superset tree adding note on $c_3$ in sub-case B) at `refs/notes/concrete-writer`. Verify rejection across `validate-notes`, `derive`, `report`, `verify`, `export-log`, `stamp-commit`, `backfill`, `fetch-notes`, `notes.sh get writer`, `verify.sh`, `train.sh`, and `.githooks/pre-push` (and verify that `sync-notes` resolves non-conflicting divergence only by creating a two-parent merge commit `-p local_sha -p remote_sha` that descends from both $P_1$ and $N_2$). | On non-descendant candidate $N_2$: `writer-notes push is non-fast-forward (<P1[:10]> is not an ancestor of <N2[:10]>): refs/notes/concrete-writer history is append-only and may not be rewound or rewritten`<br>On conflicting stanza lists in `sync-notes`: `writer-notes sync conflict on <c[:10]> between <local_sha[:10]> and <remote_sha[:10]> (rewrites or drops existing writer-log entry)` |
| **6. Missing or unfetched checkpoint (or deleted local/remote ref)** | `--old-notes-rev` is empty (`""`) or names a nonzero SHA not present in the local object store, or the local/remote writer-notes ref is missing/deleted after prior history was recorded at a checkpoint $P \in \mathcal{P}$. | (a) Pass `--old-notes-rev ""` and unfetched nonzero SHA `deadbeef...` to `validate-notes`, `fetch-notes`, and `.githooks/pre-push`; (b) delete local `refs/notes/concrete-writer` while companion checkpoint $P$ exists and run all readers/mutators; (c) delete remote `refs/notes/concrete-writer` on `origin` (and then replay older $P_0$) across `fetch-notes`, `verify.sh --fetch`, `notes.sh fetch writer`, and `train.sh build`/`check`/`land`. | Empty `--old-notes-rev`: `old-notes-rev is empty (expected 40-hex creation sentinel 0000... or an existing notes commit)`<br>Unfetched nonzero SHA: `old-notes-rev '<rev>' is not available locally (every nonzero old notes SHA must resolve and be compared)`<br>Deleted local ref: `local <ref> is missing, but prior writer-log history was recorded at <prior_desc> (refusing deleted or missing local writer-notes ref)`<br>Deleted remote ref: `remote <remote> has no <remote_ref>, but prior writer-log history was recorded at <prior_desc> (refusing deleted or missing remote writer-notes ref)` |
| **7. Environment override (`REVIEW_WRITER_NOTES_REF` / `REVIEW_WRITER_ALLOWED_SIGNERS`)** | `REVIEW_WRITER_NOTES_REF` points at a distinct ref (`refs/notes/custom-writer-override`) holding a rolled-back, out-of-range-deleted, or orphan-root notes commit while standard refs (`refs/notes/concrete-writer`, `refs/notes/remote/concrete-writer`) hold $P$, or `REVIEW_WRITER_ALLOWED_SIGNERS` points at an empty/unauthorized signers file. | Set `REVIEW_WRITER_NOTES_REF=refs/notes/custom-writer-override` to (a) rolled-back $N_1$, (b) out-of-range note deletion, and (c) orphan root with identical blobs while standard companion refs hold $N_2$, testing `derive`, `report`, `verify`, `export-log`, `validate-notes`, and `fetch-notes --no-fetch`, plus the negative-control mutation removing companion refs from `check_refs`. Test empty and unlisted `REVIEW_WRITER_ALLOWED_SIGNERS`. | Override with out-of-range deletion: `writer-notes push deletes existing writer-log entry for commit(s): <c[:10]>`<br>Override with rollback: `writer-notes push rewrites or drops existing writer-log entry on <c[:10]> (the writer log is append-only; corrections must append a signed superseding entry)`<br>Override with orphan root: `writer-notes push is non-fast-forward (<N2[:10]> is not an ancestor of <N_root[:10]>): refs/notes/concrete-writer history is append-only and may not be rewound or rewritten`<br>Empty signers: `<source_desc>: unverified writer-log entry for commit <c[:10]>: no authorized signers configured in <path> (every entry is unverified until the orchestrator's public key is added after owner approval; see docs/REVIEW.md)` |
| **8. Shallow fetch / shallow repository** | `.git/shallow` is present (`git rev-parse --is-shallow-repository` is `true`), truncating the commit or notes ancestry graph behind a shallow boundary so earlier notes commits or parent links are omitted. | Create a shallow clone (`git clone --depth 1`) or plant `.git/shallow` in a test repository with writer notes, and verify that `derive`, `report`, `verify`, `export-log`, `stamp-commit`, `backfill`, `validate-notes`, `fetch-notes`, and `sync-notes` all fail closed in `verify_writer_history_continuity` before trusting `git log` or `merge-base --is-ancestor`. | `repository is shallow (git rev-parse --is-shallow-repository is true): writer-notes continuity and ancestry require full commit history` |
| **9. Refs replaced between check and use (TOCTOU)** | A ref (`refs/notes/concrete-writer` or `refs/notes/remote/concrete-writer`) is replaced after `verify_writer_history_continuity` / `fetch-notes --no-fetch` validates $N_{\text{good}}$ and before a reader reads per-commit notes or a mutator updates the ref. | (a) In `derive_range_provenance` and `notes.sh get writer`, use a git wrapper that flips the mutable ref to an invalid/rolled-back commit immediately after the validation call and verify that the reader still reads from the pinned `validated_state` / 40-hex `$wout` SHA (or rejects); (b) in `stamp-commit`, `backfill`, `fetch-notes`, and `sync-notes`, flip the ref between validation and `git update-ref <ref> <new_sha> <expected_old_sha>` and verify that the CAS update fails closed without overwriting the concurrent ref change. | On CAS ref-update mismatch: `git update-ref <ref> <new_sha> <expected_old_sha> failed: fatal: cannot lock ref '<ref>': is at <actual_sha> but expected <expected_old_sha>` |

#### 4. Staged implementation plan

Each step below fits comfortably within a single reviewed round (one diff piece in `scripts/review/review.sh`) and leaves all root checks green:

1. **Step 1 (Round 15): Shared continuity predicate (`verify_writer_history_continuity`) in `scripts/review/writer_provenance.py` and core continuity controls in `scripts/checks/review_tool.sh`.**
   - Make `verify_writer_history_continuity` (`scripts/review/writer_provenance.py:1960-2171`) the single predicate owning:
     1. shallow-repository rejection (`git rev-parse --is-shallow-repository`),
     2. complete candidate state validity via `validate_writer_history_state` (`scripts/review/writer_provenance.py:1763-1957`, enforcing commit existence for all annotated commits unless `allow_missing_targets=True`),
     3. full-tree `list_notes_tree` inclusion and stanza-prefix continuity against **every** checkpoint $P \in \mathcal{P}$ and across the complete `git log --raw` ancestor graph (removing `target_commits` from `verify_writer_history_continuity` at `1966`, `2098-2099`, `2126-2127`, from `read_validated_writer_notes` at `2177`, `2190`, and from `derive_range_provenance` at `1105`), and
     4. commit ancestry `is_ancestor(repo, P, notes_sha)` for **every** checkpoint $P \in \mathcal{P}$ (`2026-2037`, `2164-2171`), never substituting content or tree equality for ancestry.
   - Return `(notes_sha, validated_state)` from `verify_writer_history_continuity` and pass `validated_state` directly into `inspect_commit` in `derive_range_provenance` (`scripts/review/writer_provenance.py:1105-1118`, `917-920`) so range derivation never re-reads `WRITER_NOTES_REF` from git after validation.
   - Add the planted controls and negative-control mutations in `scripts/checks/review_tool.sh` for:
     - note deletion outside `base..head` (Round-13 HIGH reproduction + negative mutation control re-introducing `target_commits`),
     - correction rollback outside `base..head`,
     - unrelated root notes commit with identical valid blobs (Round-13 MEDIUM reproduction + negative mutation control replacing `is_ancestor` with tree equality),
     - divergent notes history with identical and superset valid blobs, and
     - shallow repository rejection.
2. **Step 2 (Round 16): Atomic check-to-use binding across mutators, `fetch-notes`, `sync-notes`, and `scripts/land/notes.sh`, plus landing/train controls.**
   - Update `cmd_stamp_commit` (`scripts/review/writer_provenance.py:1409-1476`), `cmd_backfill` (`1545-1630`), `fetch_notes_ref` (`2255-2410`), and `sync_notes_refs` (`2412-2616`) to consume `(notes_sha, validated_state)` directly from `verify_writer_history_continuity` without re-reading `WRITER_NOTES_REF` and to update refs atomically via compare-and-swap (`git update-ref <ref> <new_sha> <expected_old_sha>`).
   - Update `get_note` in `scripts/land/notes.sh:59-83` to read the note from the exact validated 40-hex commit SHA returned by `writer_provenance.py fetch-notes --no-fetch` (`$wout`) rather than re-resolving `refs/notes/remote/concrete-writer` at `scripts/land/notes.sh:69-71`.
   - Add the check-to-use (TOCTOU) reader/mutator controls and end-to-end `notes.sh`, `verify.sh`, and `train.sh` controls for out-of-range deletion/rollback, unrelated-root, and divergent-history states in `scripts/checks/review_tool.sh`, `scripts/checks/landing_ready.sh`, and `scripts/checks/landing_script.sh`.
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
handled by fetching the remote notes, merging them (`git notes merge -s ours` for
review and findings notes; `writer_provenance.py sync-notes` for
`refs/notes/concrete-writer`, which verifies append-only continuity and refuses
dropping any remote writer-log entry), and retrying, up to
three attempts. Any other push failure stops at once with exit 6. The script also
fetches and synchronizes the remote notes before each round, so it counts rounds that
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
