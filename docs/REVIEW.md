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

Rounds 10–14 exposed a single structural defect class: writer-history continuity was split across readers, validators, mutators, and publishers that each inspected a different projection of the repository state. Specifically, on the round-14 tree (`5db55e04f` / code state `fb8425686`):

- **Range filtering inside the continuity predicate (`scripts/review/writer_provenance.py:1105`, `1966`, `2098-2099`, `2126-2127`, `2177`, `2190`):** `derive_range_provenance` (`scripts/review/writer_provenance.py:1105`) calls `read_validated_writer_notes(repo, allowed_signers_path, target_commits=set(commits))`, which forwards `target_commits` to `verify_writer_history_continuity` (`scripts/review/writer_provenance.py:2186-2191`). Inside `verify_writer_history_continuity`, the `git log --raw` walk skips every annotated commit `c` not in `target_commits` (`if target_commits is not None and c not in target_commits: continue` at `scripts/review/writer_provenance.py:2098-2099` and `2126-2127`), and the direct tree comparison (`scripts/review/writer_provenance.py:1981-2017`) runs only when an explicit `old_notes_rev` is passed. Deleting or rolling back a valid note on a historical commit outside `base..head` therefore passes `derive`, `report`, `verify`, and `export-log` over `base..head`.
- **Companion-checkpoint ancestry checked only for an explicit `old_notes_rev` (`scripts/review/writer_provenance.py:2026-2049`, `2164-2171`):** `verify_writer_history_continuity` collects companion checkpoints (`WRITER_NOTES_REF`, `refs/notes/concrete-writer`, `refs/notes/remote/concrete-writer`, and `extra_revs`) into `rev_roots` (`scripts/review/writer_provenance.py:2026-2037`), but checks `is_ancestor(repo, old_sha, notes_sha)` only when `old_notes_rev` is explicitly supplied and nonzero (`scripts/review/writer_provenance.py:2164-2171`). When `old_notes_rev` is omitted (`validate_notes_ref` at `scripts/review/writer_provenance.py:2195-2216`, `read_validated_writer_notes` at `2174-2192`) or is the creation sentinel `0000...` (`.githooks/pre-push:70`), a parentless root notes commit or a divergent notes commit with identical valid note blobs passes `git log --raw` blob-prefix checks without descending from the companion checkpoints by commit ancestry.
- **Premature pair-wise continuity rejection on sibling tips before divergent merge construction (`scripts/review/writer_provenance.py:2453-2457`, `2534-2544`):** When `local_sha` and `remote_sha` diverge as sibling tips from a common ancestor $N_0$ (`local_sha` adding a note on $c_{\text{local}}$ and `remote_sha` adding a note on $c_{\text{remote}}$), `sync_notes_refs` (`scripts/review/writer_provenance.py:2542`) calls `verify_writer_history_continuity(repo, remote_sha, old_notes_rev=prev_remote_sha, extra_revs=[local_sha])` *before* constructing the two-parent merge candidate (`2546-2595`). Because `local_sha` is included in `extra_revs` (`2035-2037`), `git log --raw` walks `local_sha`, sees $c_{\text{local}}$ absent from `remote_sha`'s tree (`new_entries`), and raises `writer-notes push deletes existing writer-log entry for commit(s): <c_local[:10]>` (`2154-2156`) before `merged_sha` (`-p local_sha -p remote_sha`) can ever be built. Likewise, running the full checkpoint-ancestry predicate on unmerged sibling inputs would reject `local_sha` and `remote_sha` because neither descends from the other before merge construction.
- **Check-to-use ref re-reads, unpinned note reads, and non-transactional checkpoint publication (`scripts/review/writer_provenance.py:677-689`, `738-747`, `920`, `1105-1118`, `1409-1476`, `1545-1630`, `2310-2319`, `2352-2355`, `2403-2406`, `2474-2476`, `2487`, `2504-2506`, `2531`, `2610-2612`, `2685-2696`; `scripts/review/review.sh:166-188`; `scripts/land/notes.sh:63-75`):** `derive_range_provenance` (`scripts/review/writer_provenance.py:1105`) discards the dictionary returned by `read_validated_writer_notes` and re-reads `WRITER_NOTES_REF` per commit via `inspect_commit` (`1116-1118` -> `920` -> `get_commit_note_history` at `738-747` -> `read_optional_note` at `677-689`); `read_optional_note` itself resolves `notes_ref` (`680`) and then re-reads `notes_ref` by mutable ref name in `git notes --ref={notes_ref} show` (`682`). In `scripts/land/notes.sh:63-75`, `get_note` validates `$ref` with `fetch-notes --no-fetch` (`63`) and then re-resolves `$ref` via `git for-each-ref` and `git notes --ref="$ref" show` (`69-71`)—and passing a bare 40-hex commit SHA `$wout` to `git notes --ref="$wout" show` fails because `git notes --ref=<val>` interprets `<val>` as the ref name `refs/notes/<val>`, returning `"error: no note found for object ..."`, which `scripts/land/notes.sh:71-75` treats as an absent note (`return 0` with empty stdout). On the publication side, `push_notes` in `scripts/review/review.sh:166-188` (called at `533`) pushes `$WRITER_NOTES_REF` by mutable ref name (`170-172`), then re-reads `$(git rev-parse "$WRITER_NOTES_REF")` after pushing and unconditionally overwrites `refs/notes/remote/concrete-writer` (`173-175`), and calls `sync_notes` on push failure (`184`); meanwhile `cmd_publish_notes` (`scripts/review/writer_provenance.py:2674-2701`) pushes `f"{sha}:{WRITER_NOTES_REF}"` (`2687`) without `--force-with-lease` on the remote ref and updates `refs/notes/remote/concrete-writer` at `2696` without compare-and-swap (CAS), and `fetch_notes_ref` (`2310-2319`, `2352-2355`, `2403-2406`) and `sync_notes_refs` (`2474-2476`, `2487`, `2504-2506`, `2531`, `2610-2612`) update both local and companion refs without CAS.
- **Uncanonical git object and history view (`scripts/review/writer_provenance.py:143-155`, `298-305`, `1434-1441`, `1704-1709`, `2047-2055`, `2572-2578`, `2686-2692`):** `git_run` (`scripts/review/writer_provenance.py:143-155`) and the four direct `subprocess.run(["git", "-C", ...])` callers (`cmd_stamp_commit` at `1434-1441`, `batch_cat_objects` at `1704-1709`, `sync_notes_refs` at `2572-2578`, and `cmd_publish_notes` at `2686-2692`) do not pass `--no-replace-objects` (`GIT_NO_REPLACE_OBJECTS=1`), do not disable `core.commitGraph`, do not scrub ambient git environment variables (`GIT_REPLACE_REF_BASE`, `GIT_GRAFT_FILE`, `GIT_SHALLOW_FILE`, `GIT_DIR`, `GIT_COMMON_DIR`, `GIT_OBJECT_DIRECTORY`, `GIT_ALTERNATE_OBJECT_DIRECTORIES`, `GIT_INDEX_FILE`, `GIT_NAMESPACE`), and do not check for `.git/info/grafts` or `refs/replace/*`. A planted `refs/replace/<orphan>` ref or `.git/info/grafts` entry can spoof `git merge-base --is-ancestor`, `git log --raw`, and blob/tree reads so an orphan or rewritten notes commit appears to descend from every checkpoint $P \in \mathcal{P}$.

#### 1. The single invariant over the complete writer-notes state

Let $N \in \text{Commit} \cup \{\bot\}$ be the authoritative candidate writer-notes commit (or $\bot$ when the candidate ref does not exist) in `repo`, and let the prior checkpoint set $\mathcal{P}$ be the deduplicated set of all nonzero 40-hex commit SHAs resolved from:
1. every caller-supplied `old_notes_rev` / `old_notes_revs` (`--old-notes-rev`),
2. any caller-supplied `target_ref` (`--target-ref`),
3. the active `WRITER_NOTES_REF` (`REVIEW_WRITER_NOTES_REF`, defaulting to `refs/notes/concrete-writer` at `scripts/review/writer_provenance.py:122`),
4. `refs/notes/concrete-writer`, and
5. `refs/notes/remote/concrete-writer`.

**One checkpoint snapshot, including absence.** At operation entry, capture a single `CheckpointSnapshot` mapping every participating local ref name to its exact commit SHA or `ABSENT`: the active override, standard writer ref, remote companion ref, caller `target_ref`, and every named old/check ref (deduplicate names, not just SHAs). Pin caller revisions to immutable SHAs once; keep their labels and creation sentinels separately. Derive $\mathcal{P}$ solely from this snapshot and explicit immutable revisions. Never independently re-resolve refs during validation, candidate construction, or installation. Capture the map with one ref enumeration; atomically verify the entire map with an `update-ref --stdin` verification-only transaction before accepting it as a checkpoint snapshot; on mismatch discard it and retry from capture. This gives a checkpoint set verified at one transaction boundary even if enumeration raced. Missing refs remain participants, not omissions.

Every subsequent predicate call for this operation receives the same snapshot explicitly (including isolated-input calls, which use an explicitly restricted checkpoint view). A read-only operation validates and returns the pinned snapshot; it does not promise that mutable refs remain current after return. An installer additionally validates and commits against that same snapshot under the transaction contract below. Retries capture a fresh complete snapshot and revalidate; they never refresh only one expected SHA.

The complete writer-notes state satisfies continuity if and only if **all three** clauses below hold over the **entire** repository state under the canonical, unadulterated git object graph, with no commit-range filter (`target_commits`) anywhere in the predicate:

1. **Canonical repository view and complete candidate validity (signatures, principals, keys, and commit semantics):**
   - **Canonical, unadulterated object and history view:** Every git command executed by `writer_provenance.py` runs with `--no-replace-objects` and `-c core.commitGraph=false` under a scrubbed environment (`GIT_NO_REPLACE_OBJECTS=1`, `GIT_OPTIONAL_LOCKS=0`, and all `GIT_REPLACE_REF_BASE`, `GIT_GRAFT_FILE`, `GIT_SHALLOW_FILE`, `GIT_DIR`, `GIT_WORK_TREE`, `GIT_COMMON_DIR`, `GIT_OBJECT_DIRECTORY`, `GIT_ALTERNATE_OBJECT_DIRECTORIES`, `GIT_INDEX_FILE`, `GIT_NAMESPACE` removed). Furthermore, `repo` must not be shallow (`git rev-parse --is-shallow-repository` is `false` and no `<git-dir>/shallow` or `<git-common-dir>/shallow` file exists), must have no grafts file (`<git-dir>/info/grafts` and `<git-common-dir>/info/grafts` do not exist), and must have no replacement refs (`refs/replace/*` under `git for-each-ref refs/replace` is empty). No element of `old_notes_revs` may be an empty string (`""`), and every nonzero revision in $\{N\} \cup \mathcal{P}$ must resolve locally to a commit object (`commit_exists` / `resolve_commit` at `scripts/review/writer_provenance.py:243-249`, `274-295`).
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
   - For **every** checkpoint $P \in \mathcal{P}$, $P$ must be a git commit ancestor-or-equal of $N$ (`is_ancestor(repo, P, N)` via `git --no-replace-objects merge-base --is-ancestor P N` at `scripts/review/writer_provenance.py:298-305`):
     $$\forall P \in \mathcal{P},\quad P \preceq_{\text{commit}} N$$
   - Tree equality ($N\text{^{tree}} == P\text{^{tree}}$) or identical valid note blobs **never** substitute for commit ancestry: an orphan/root notes commit or a sibling/divergent notes commit carrying identical blobs fails $P \preceq_{\text{commit}} N$ and is rejected.
   - **Untrusted merge/mutation inputs versus the authoritative candidate $N$:** When a mutator (`stamp-commit`, `backfill`, `correct`) or synchronizer (`sync-notes`, `publish-notes`) constructs a new notes commit, the pre-merge or pre-mutation inputs (`old_local_sha` and `remote_sha`) are validated for pure state validity (`validate_writer_history_state`) and isolated self-ancestry (`check_refs=[]`, plus `old_notes_revs=[prev_remote_sha]` on `remote_sha`), the candidate notes commit $N$ (for divergent sibling tips, the two-parent merge commit `merged_sha` with parents `-p old_local_sha -p remote_sha`) is constructed privately in the git object store without touching any ref, and the single complete predicate `verify_writer_history_continuity` is then enforced on the authoritative candidate $N$ against **all** pinned checkpoints in $\mathcal{P}$ before any ref is updated.

#### 2. The one function that owns the predicate and how every caller changes

`verify_writer_history_continuity` (`scripts/review/writer_provenance.py:1960-2171`) is the **single** function that owns this complete invariant:

```python
def verify_writer_history_continuity(
    repo: str,
    notes_rev: str | None,
    checkpoint_snapshot: CheckpointSnapshot,
    old_notes_revs: list[str] | None = None,
    allowed_signers_path: str | None = None,
    allow_missing_targets: bool = False,
) -> tuple[str | None, dict[str, list[dict[str, str]]]]:
```

`verify_writer_history_continuity` has **no `target_commits` parameter**. On every call it:
1. Calls `verify_canonical_repo_view(repo)` at operation entry (not before each Git subprocess) to reject shallow repositories (`--is-shallow-repository` or `shallow` file), grafted history (`info/grafts`), and replacement refs (`refs/replace/*`), with all git subprocesses in `writer_provenance.py` running via `git --no-replace-objects -c core.commitGraph=false` under a scrubbed environment (`GIT_NO_REPLACE_OBJECTS=1`, `GIT_OPTIONAL_LOCKS=0`).
2. Consumes the caller-captured, atomically verified `checkpoint_snapshot` without ref resolution; validates `old_notes_revs` (refusing any `""` entry and any nonzero SHA not present as a commit in `repo`) and derives all prior checkpoints $\mathcal{P}$ via `collect_prior_writer_checkpoints` (`scripts/review/writer_provenance.py:2219-2252`) from the snapshot and pinned explicit old revisions. The capture helper supplies the default companion names plus caller additions; no public caller may remove default companions. `check_refs=[]` below denotes only the restricted snapshot view for untrusted input validation, never authoritative acceptance.
3. If `notes_rev` is `None`: returns `(None, {})` when $\mathcal{P} = \emptyset$, or raises `ProvenanceError` (`local <ref> is missing, but prior writer-log history was recorded at ...`) when $\mathcal{P} \neq \emptyset$.
4. Takes `notes_rev` only as an immutable candidate SHA (or the pinned candidate value from the snapshot), checks its commit type without re-resolving a mutable name, and calls `validate_writer_history_state(repo, notes_sha, allowed_signers_path=allowed_signers_path, allow_missing_targets=allow_missing_targets)` (`scripts/review/writer_provenance.py:1763-1957`) to validate the complete candidate state (where `validate_writer_history_state` requires every annotated commit $c \in \text{dom}(N)$ to exist in `parsed_commits` unless `allow_missing_targets=True`).
5. For **every** checkpoint $P \in \mathcal{P}$:
   - Compares `list_notes_tree(repo, P)` against `new_entries` (`scripts/review/writer_provenance.py:1993-2017`) across all commits in $P$, rejecting any deleted note or non-prefix stanza change before any ancestry or log check.
   - Enforces commit ancestry `is_ancestor(repo, P, notes_sha)` (`scripts/review/writer_provenance.py:2167-2171`), rejecting any non-ancestor checkpoint even when its blobs are identical to `notes_sha`.
6. Walks the full commit graph `git log --raw --root -m --no-abbrev --format=COMMIT %H` of `[notes_sha, *P]` (`scripts/review/writer_provenance.py:2047-2162`) over **all** annotated commits (with lines `2098-2099` and `2126-2127` deleted) so any historical note deletion or stanza rewrite in the ancestor graph is rejected.
7. Returns `(notes_sha, validated_state)`, where `validated_state` is the complete validated `{commit_sha: [stanza_dict, ...]}` snapshot at `notes_sha`.

**Installation transaction contract (applies to every installer below).** `cas_update_refs(snapshot, updates)` emits one `git update-ref --stdin` transaction with `start`, one operation per participating ref, `prepare`, and `commit`. Updated refs use `update <ref> <new> <snapshot-old-or-zero>` (or `create` for expected absence); unchanged refs use `verify <ref> <snapshot-old-or-zero>`. `verify <ref> 0000...` asserts continued absence. Never reduce the transaction to the write set. Include aliases under distinct names even when their SHAs agree. Any mismatch aborts the entire local transaction without overwriting concurrent work. Even no-op/ratchet branches verify the full set. Temporary fetch refs are unique operation-owned transport inputs, not a replacement for snapshot checkpoints.

**clarified in round 21.** Checkpoint participants are never symbolic refs. The canonical runner drives `git update-ref --no-deref --stdin` interactively: after `prepare: ok`, every participating name (including unchanged refs and expected absence) is locked as itself; while those locks are held, inspect each name for symbolic status and its expected value, then commit only if all checks succeed, otherwise abort (`symbolic checkpoint ref is forbidden: <ref>` for a symbolic participant). A symbolic replacement before prepare is rejected under lock; concurrent symbolic or direct replacement while prepared cannot acquire the lock. This contract is tested against files (including packed refs) and reftable backends; unsupported backends are refused explicitly.

For publication, no local Git transaction can lock a remote ref. Push the immutable candidate with an exact remote lease (including expected absence), then acknowledge it locally with a complete-snapshot transaction. The post-push validation and acknowledgement share one newly captured and atomically verified complete snapshot; validate the published candidate against it, then verify every participating ref while updating the companion. If a companion advanced incompatibly, fail acknowledgement and report that the remote push succeeded but local acknowledgement failed; never overwrite the companion or claim publication was rolled back. A retry starts with a fresh snapshot and remote lease. This contract also covers unchanged standard refs under an active override.

**Slice A runner boundary.** Each CLI command creates a fresh, private `_VerifiedRepo` handle inside its existing error handling and passes it through all helpers; the caller's namespace is unchanged. The handle verifies the canonical repository view once and carries that verification through the operation, without a repository-keyed cache. Standalone calls with plain paths verify independently. Git's `rev-parse --local-env-vars` owns the scrub vocabulary, supplemented by the view variables and the entire `GIT_CONFIG_*` family. Only `GIT_CONFIG_GLOBAL` and `GIT_CONFIG_NOSYSTEM` survive that family scrub because `git_sandbox` uses them to isolate gate fixtures from user/system configuration. The cached vocabulary contains no repository state. The checkpoint capture/transaction work in later slices will bind a fresh handle to each snapshot boundary, including the new post-push acknowledgement operation; slice A does not implement or change those predicates or transactions.

The pseudocode calls in the inventory omit the explicit snapshot argument for readability: all calls within an operation use its one snapshot, not independently collected defaults. Per-row expected-SHA examples below are write operands only; **every row also verifies every unchanged participating ref and expected absence under this contract**.

Every current caller and helper in the repository changes as follows to call `verify_writer_history_continuity` on the full state before selecting any range, to read notes from the validated immutable commit/tree rather than mutable ref names, and to update local and companion checkpoint refs transactionally via atomic compare-and-swap (`git update-ref --stdin`):

| Caller (`file:line`) | Role | Current behaviour on `5db55e04f` | How it changes |
|---|---|---|---|
| `git_run` (`scripts/review/writer_provenance.py:143-155`) and direct `subprocess.run` callers (`1434-1441`, `1704-1709`, `2572-2578`, `2686-2692`) | Git command runner and object/history readers/writers | `git_run` (`143-155`) runs `["git", "-C", repo, *args]` with ambient `os.environ` (`GIT_OPTIONAL_LOCKS=0` only), and `cmd_stamp_commit` (`1434-1441`), `batch_cat_objects` (`1704-1709`), `sync_notes_refs` (`2572-2578`), and `cmd_publish_notes` (`2686-2692`) invoke `subprocess.run(["git", "-C", ...])` directly without `--no-replace-objects`, `core.commitGraph=false`, or environment scrubbing. | All git invocations route through a canonical runner that prepends `["git", "--no-replace-objects", "-c", "core.commitGraph=false", "-C", repo, *args]` and uses `canonical_git_env()` (`GIT_NO_REPLACE_OBJECTS=1`, `GIT_OPTIONAL_LOCKS=0`, and strips `GIT_REPLACE_REF_BASE`, `GIT_GRAFT_FILE`, `GIT_SHALLOW_FILE`, `GIT_DIR`, `GIT_WORK_TREE`, `GIT_COMMON_DIR`, `GIT_OBJECT_DIRECTORY`, `GIT_ALTERNATE_OBJECT_DIRECTORIES`, `GIT_INDEX_FILE`, `GIT_NAMESPACE`). `verify_canonical_repo_view(repo)` rejects shallow repos, `info/grafts`, and `refs/replace/*`. |
| `verify_writer_history_continuity` (`scripts/review/writer_provenance.py:1960-2171`) | Owner of the continuity predicate | Accepts `old_notes_rev: str \| None` (`1963`), `extra_revs` (`1964`), and `target_commits: set[str] \| None` (`1966`); skips commits not in `target_commits` at `2098-2099` and `2126-2127`; checks `old_entries` vs `new_entries` (`1993-2017`) and `is_ancestor` (`2164-2171`) only for `old_notes_rev`; does not call `validate_writer_history_state` itself; returns `None`. | Drops `target_commits` parameter and range filters (`2098-2099`, `2126-2127`); calls `verify_canonical_repo_view(repo)`; runs `validate_writer_history_state` (`1763-1957`) on `notes_sha`; enforces full-tree prefix continuity and `is_ancestor(repo, P, notes_sha)` for **every** checkpoint $P \in \mathcal{P}$ (both `old_notes_revs` and companion refs `2026-2037`); walks full `git log --raw` history without filtering; returns `(notes_sha, validated_state)`. |
| `validate_writer_history_state` (`scripts/review/writer_provenance.py:1763-1957`) | Pure candidate-state validator (called inside `verify_writer_history_continuity` and for pre-merge input validation) | Validates tree layout, note blobs, `entry-id`, SSH signatures/principals, and commit-level rules, but silently skips commit-level checks when `annotated_sha not in parsed_commits` (`1853-1854`). | Accepts `allow_missing_targets: bool = False`; when `allow_missing_targets` is `False` (the default for all readers, mutators, `fetch-notes`, `sync-notes`, `publish-notes`, and `.githooks/pre-push`), raises `ProvenanceError` if any annotated commit $c \in \text{dom}(N)$ is missing from `parsed_commits` (`1853-1854`). Called unconditionally by `verify_writer_history_continuity`. |
| `read_validated_writer_notes` (`scripts/review/writer_provenance.py:2174-2192`) | Local active-ref reader helper | Accepts `target_commits` (`2177`), calls `validate_writer_history_state` (`2185`) and `verify_writer_history_continuity(..., target_commits=target_commits)` (`2186-2191`), and returns `state` without `cur_sha`. | Removes `target_commits` parameter; delegates directly to `verify_writer_history_continuity(repo, snapshot.active_sha, allowed_signers_path=allowed_signers_path)` and returns `(cur_sha, validated_state)`. |
| `validate_notes_ref` (`scripts/review/writer_provenance.py:2195-2216`) and `cmd_validate_notes` (`2619-2632`) | Full candidate validator (`validate-notes` CLI) | `validate_notes_ref` discards `allow_missing_targets` (`2204`), calls `validate_writer_history_state` (`2205-2209`), and calls `verify_writer_history_continuity` with `old_notes_rev=old_notes_rev` (`2210-2215`), leaving companion checkpoints unchecked for `is_ancestor` when `old_notes_rev` is `None` or `ZERO40`. | Calls `_, validated_state = verify_writer_history_continuity(repo, notes_rev, old_notes_revs=[old_notes_rev] if old_notes_rev is not None else None, allowed_signers_path=allowed_signers_path, allow_missing_targets=allow_missing_targets)` and returns `len(validated_state)`. |
| `read_optional_note` (`scripts/review/writer_provenance.py:677-689`), `read_note_from_commit`, and `cmd_show_note` (`show-note` CLI) | Pinned note blob reader from a validated notes commit | `read_optional_note` (`677-689`, called at `743`, `1211`, `1446`, `1547`) calls `resolve_optional_ref(repo, notes_ref)` (`680`) and then re-reads `notes_ref` by mutable ref name via `git notes --ref={notes_ref} show {sha}` (`682`). | Writer-note uses of `read_optional_note` are replaced by `read_note_from_commit(repo, notes_commit_sha, target_commit_sha)`, which reads the note blob directly from the immutable notes commit tree (`list_notes_tree` / `git cat-file blob <blob_sha>`) without invoking `git notes --ref`. A new `show-note` CLI subcommand (`writer_provenance.py show-note --repo <repo> --target-ref <ref> --commit <sha> [--key <key>]`) validates `<ref>` via `verify_writer_history_continuity` (using the pinned read-only checkpoint snapshot) and returns the raw note text or `<key>` field from the in-memory `validated_state` / pinned `notes_sha` in a single invocation. |
| `derive_range_provenance` (`scripts/review/writer_provenance.py:1076-1170`) | Range provenance reader for `base..head` | Calls `read_validated_writer_notes(repo, allowed_signers_path, target_commits=set(commits))` at `1105` (filtering continuity to `commits` and discarding the returned state), then loops `inspect_commit(repo, c, head_sha, None, eff_signers)` (`1115-1118`), which at `920` calls `get_commit_note_history` (`738-747`) -> `read_optional_note` (`677-689`) to re-read `WRITER_NOTES_REF` from git per commit. | Calls `_, validated_state = verify_writer_history_continuity(repo, snapshot.active_sha, allowed_signers_path=allowed_signers_path)` on the **complete** state before iterating over `commits`, and passes `validated_state` into `inspect_commit(repo, c, head_sha, validated_state, eff_signers)` so `inspect_commit` (`917-920`) reads only the validated in-memory snapshot and never re-reads `WRITER_NOTES_REF` from git. |
| `inspect_commit` (`scripts/review/writer_provenance.py:877-970`) and `get_commit_note_history` (`738-747`) | Per-commit stamp vs note inspector | When `external_log is None`, `inspect_commit` (`920`) calls `get_commit_note_history(repo, sha, allowed_signers_path)` which reads `WRITER_NOTES_REF` directly from git (`743`) without continuity validation. | `get_commit_note_history` is removed; `inspect_commit` receives the already-validated `validated_state: dict[str, list[dict[str, str]]]` from `verify_writer_history_continuity` (or single-entry dict in `stamp-commit`/`backfill` pre-check) and looks up `validated_state.get(sha)` at `917-920`. |
| `load_log_object` (`scripts/review/writer_provenance.py:661-674`) | Supplemental exported-history reader | Reads a file or Git blob and parses signed stanzas; active at `1151-1152`. | Preserve file/blob loading and parse/signature failures. Read a file once or resolve a blob once through the canonical runner. Exports are supplemental evidence only; they never supply the authoritative state, checkpoint set, or continuity result. |
| `verify_history_against_authority` (`scripts/review/writer_provenance.py:750-780`) and range export checks (`1154-1167`) | Supplemental-history validator | Rejects missing, truncated, conflicting, unauthoritative and extra exported histories. | Compare exports exclusively against histories projected from the shared predicate's immutable authoritative snapshot. Preserve all five rejection paths, for both file and blob exports; matching exports cannot excuse invalid authoritative state outside the range. |
| `list_notes_tree`, `batch_cat_objects`, `_parse_raw_commit_object` (`1638-1669`, `1697-1760`); `parse_and_verify_note_blob`, `parse_multi_stanza_log`, `parse_log_stanza`, `verify_stanza_signature`, `check_allowed_signers_has_keys` (`704-735`, `623-658`, `459-613`, `414-456`, `182-204`) | Structural, object, cryptographic and chain readers/validators | Enforce tree layout, object framing, note authority, signed stanza syntax and chains, authorized principals and keys. | Remain mandatory beneath complete state validation; caches are keyed by immutable content and signer content. `_blob_has_valid_signed_note` (`1682-1691`) classifies historical blobs only; its caught parse errors must never suppress candidate-state rejection. |
| `validate_entry_against_commit`, `is_clean_auto_merge` (`784-874`, `308-323`); `parse_evidence_specs`, `verify_backfill_evidence_for_entries`, `sha256_file` (`973-1073`, `157-167`); `check_reviewer_against_history`, review-note readers (`1173-1225`) | Range semantics, evidence and independence checks | Validate stamps, ancestry, evidence hashes, reviewer overlap and existing review notes. | Retain these additional checks after authoritative whole-state validation. Evidence and review notes cannot replace writer authority. Review-note reads remain distinct from writer-note reads when removing `read_optional_note`; preserve that reader's non-writer use at `1211`. |
| `collect_prior_writer_checkpoints`, `resolve_optional_ref`, `resolve_commit`, `commit_exists`, `is_ancestor` (`2219-2252`, `251-272`, `243-249`, `274-295`, `298-305`) | Checkpoint and graph helpers | Resolve mutable refs and test objects/ancestry. | Capture named refs only in the snapshot helper; subsequent graph/object checks consume pinned SHAs in the canonical view. Preserve errors on non-commit or unavailable objects and distinguish absence from lookup failure. |
| `cmd_derive` (`scripts/review/writer_provenance.py:1267-1295`), `cmd_report` (`1298-1314`), `cmd_verify` (`1317-1346`), `cmd_export_log` (`1349-1380`) | Range reader CLI subcommands (`derive`, `report`, `verify`, `export-log`) | Call `derive_range_provenance` (`1269`, `1300`, `1326`, `1351`), inheriting the `target_commits` range filter at `1105` and the missing companion `is_ancestor` check at `2164-2171`. | Call `derive_range_provenance`, which now runs `verify_writer_history_continuity` over the complete writer-notes state and all companion checkpoints before projecting onto `base..head`. |
| `cmd_stamp_commit` (`scripts/review/writer_provenance.py:1383-1481`) | Harness writer-note mutator (`stamp-commit`) | Calls `read_validated_writer_notes(..., target_commits=None)` (`1409`), amends `HEAD` via direct `subprocess.run` (`1434-1441`), re-reads `read_optional_note(args.repo, WRITER_NOTES_REF, new_sha)` (`1446`), and mutates `WRITER_NOTES_REF` in place with `git notes --ref={WRITER_NOTES_REF} add` (`1473-1476`) without a compare-and-swap (CAS) check on the prior ref SHA. | Calls `old_notes_sha, validated_state = verify_writer_history_continuity(args.repo, snapshot.active_sha, allowed_signers_path=args.allowed_signers)` (`1409`), checks `new_sha not in validated_state` from the snapshot, builds `new_notes_sha` privately in the object store (`git hash-object -w` + `git mktree` + `git commit-tree` with parent `old_notes_sha`) without touching `WRITER_NOTES_REF`, validates `new_notes_sha` with `verify_writer_history_continuity(..., old_notes_revs=[old_notes_sha or ZERO40])`, and atomically CAS-updates `WRITER_NOTES_REF` via `cas_update_refs` (`git update-ref --stdin` expecting `old_notes_sha or ZERO40`). |
| `cmd_backfill` (`scripts/review/writer_provenance.py:1484-1635`) | Audited backfill and correction mutator (`backfill`, `correct`) | Calls `read_validated_writer_notes(..., target_commits=None)` (`1545`), then re-reads `read_optional_note(args.repo, WRITER_NOTES_REF, commit_sha)` (`1547`), and overwrites the note via `git notes --ref={WRITER_NOTES_REF} add -f` (`1627-1630`) without a CAS check on `WRITER_NOTES_REF`. | Calls `old_notes_sha, validated_state = verify_writer_history_continuity(args.repo, snapshot.active_sha, allowed_signers_path=args.allowed_signers)` (`1545`), takes `existing_history = validated_state.get(commit_sha, [])` from the validated snapshot, builds `new_notes_sha` privately in the object store (`hash-object` + `mktree` + `commit-tree` with parent `old_notes_sha`), validates `new_notes_sha` with `verify_writer_history_continuity(..., old_notes_revs=[old_notes_sha or ZERO40])`, and atomically CAS-updates `WRITER_NOTES_REF` from `old_notes_sha or ZERO40` to `new_notes_sha`. |
| `fetch_notes_ref` (`scripts/review/writer_provenance.py:2255-2410`) and `cmd_fetch_notes` (`2635-2655`) | Remote/local checkpoint validator and ratchet (`fetch-notes`) | Runs `validate_writer_history_state` (`2295`, `2331`, `2381`) and loops `verify_writer_history_continuity` over `priors` (`2302`, `2338`, `2388`) or `ZERO40` (`2346`, `2396`), then re-reads refs in the monotone ratchet (`2310-2319`) and runs `git update-ref` (`2311`, `2313`, `2319`, `2352-2355`, `2403-2406`) without expected-old-SHA CAS arguments. | Pins the initial SHAs of `eff_target_ref`, `refs/notes/remote/concrete-writer`, `refs/notes/concrete-writer`, and `WRITER_NOTES_REF` at entry; calls `verify_writer_history_continuity(repo, cand_sha, old_notes_revs=priors or [ZERO40], checkpoint_snapshot=snapshot, allowed_signers_path=allowed_signers_path)` once in the ratchet (`2293-2309`), in `no_fetch` (`2331-2351`), and in the remote fetch branch (`2381-2401`); and updates modified refs and verifies every unchanged participating ref (including absence) in one atomic `git update-ref --stdin` transaction (`cas_update_refs`) using their pinned expected old SHAs so any concurrent ref mutation between check and update fails closed. |
| `sync_notes_refs` (`scripts/review/writer_provenance.py:2412-2616`) and `cmd_sync_notes` (`2658-2671`) | Append-only notes synchronizer and divergent-history merger (`sync-notes`) | Calls `verify_writer_history_continuity(repo, remote_sha, old_notes_rev=prev_remote_sha, extra_revs=[local_sha])` at `2542` *before* building the merge commit (`2546-2595`), which rejects valid divergent sibling tips (`local_sha` and `remote_sha`) because `remote_sha` lacks `local_sha`'s newly added notes (`2154-2156`); uses a fixed temporary ref `refs/notes/writer-sync-fetch-tmp` (`2420`); and updates `WRITER_NOTES_REF` and `refs/notes/remote/concrete-writer` (`2474-2476`, `2487`, `2504-2506`, `2531`, `2610-2612`) without CAS. | Takes `old_local_sha` and `prev_remote_sha` from the single entry snapshot and fetches remote into a unique temporary ref (`refs/notes/writer-sync-fetch-tmp-{pid}-{token}`). Distinguishes untrusted merge inputs from the authoritative candidate: validates `old_local_sha` and `remote_sha` individually for state validity and isolated self-ancestry (`verify_writer_history_continuity(repo, old_local_sha, check_refs=[])` and `verify_writer_history_continuity(repo, remote_sha, old_notes_revs=[prev_remote_sha] if prev_remote_sha else None, check_refs=[])`); when neither is an ancestor of the other, privately constructs the two-parent merge commit `merged_sha` (`git mktree` + `git commit-tree <tree_sha> -p <old_local_sha> -p <remote_sha>` at `2571-2594`, enforcing stanza-prefix compatibility on shared commits at `2554-2569`); then validates the resulting `target_sha` (`remote_sha`, `old_local_sha`, or `merged_sha`) through the single full predicate `verify_writer_history_continuity(repo, target_sha, old_notes_revs=[r for r in (old_local_sha, remote_sha, prev_remote_sha) if r], allowed_signers_path=allowed_signers_path)` against **every** checkpoint in $\mathcal{P}$; and finally updates `WRITER_NOTES_REF` (to `target_sha`, expecting `old_local_sha or ZERO40`) and, when `remote_sha` is already at `target_sha` (`remote_sha == target_sha`), `refs/notes/remote/concrete-writer` (to `remote_sha`, expecting `prev_remote_sha or ZERO40`) in one atomic `cas_update_refs` transaction (`git update-ref --stdin`). When `target_sha != remote_sha`, `sync_notes_refs` advances `refs/notes/remote/concrete-writer` only to the validated `remote_sha` (if `remote_sha` is non-None), leaving `target_sha` advancement of the remote checkpoint to `publish_notes_ref` after the push succeeds. |
| `publish_notes_ref` and `cmd_publish_notes` (`scripts/review/writer_provenance.py:2674-2701`) | Single transactional writer-notes publisher (`publish-notes`) | Calls `sha = sync_notes_refs(...)` (`2676`), pushes `f"{sha}:{WRITER_NOTES_REF}"` (`2687`) without `--force-with-lease` on the remote ref, and runs `git update-ref refs/notes/remote/concrete-writer {sha}` (`2696`) without CAS or re-verifying local ref continuity. | Becomes the **single** writer-notes publication API across the repository. On each attempt (up to 3 bounded retries on remote lease contention): (1) runs `sync_notes_refs` to obtain the pinned `(target_sha, remote_sha, prev_remote_sha)` and atomically CAS-update `WRITER_NOTES_REF` to `target_sha`; (2) pushes explicit `f"{target_sha}:{WRITER_NOTES_REF}"` (never a mutable ref name) with `--force-with-lease={WRITER_NOTES_REF}:{remote_sha or ''}` so the remote rejects the push if `origin`'s `refs/notes/concrete-writer` moved since step 1; (3) after the push succeeds, captures and atomically verifies the complete acknowledgement snapshot, validates `target_sha` against its pinned checkpoints plus the original immutable publication checkpoints, then updates the companion from **that snapshot's** old SHA to `target_sha` and verifies every other participating ref, including unchanged active/standard refs and expected absence, in one transaction. A changed active ref is not overwritten. Incompatible changes before validation reject the candidate; changes after validation abort the transaction. Report remote success and local acknowledgement failure separately under the installation contract above. |
| `.githooks/pre-push` (`.githooks/pre-push:65-78`) | Push-time notes gate | Runs `writer_provenance.py validate-notes --repo "$ROOT_DIR" --notes-rev "$localsha" --old-notes-rev "${_remotesha:-}"` (`70`) and refuses deletion (`75-77`) before `CONCRETE_SKIP_GATES` (`108-111`). When `_remotesha` is `0000...`, companion checkpoints in the local clone were not checked for `is_ancestor` (`writer_provenance.py:2164-2171`). | Invokes `validate-notes` at `.githooks/pre-push:70`, which now executes the unified `verify_writer_history_continuity` over the full state and enforces `is_ancestor` and full-tree prefix continuity against both `--old-notes-rev` and every local companion checkpoint even when `_remotesha` is `0000...`. |
| `scripts/review/review.sh` (`scripts/review/review.sh:158-188`, `211`, `223`, `263`, `533`) | Independent branch reviewer and post-review notes publisher (`sync_notes`, `push_notes`) | Calls `writer_provenance.py sync-notes` (`159`, invoked at `184` and `211`), `writer_provenance.py derive` (`223`), and `writer_provenance.py verify` (`263`). In `push_notes` (`166-188`, called at `533`), adds `$WRITER_NOTES_REF` to `refs` (`170`), pushes `${refs[@]}` by mutable ref name (`172`), then re-reads `$(git rev-parse "$WRITER_NOTES_REF")` and overwrites `refs/notes/remote/concrete-writer` without CAS or validation (`173-175`), and calls `sync_notes` on push failure (`184`). | `sync_notes` (`159`), `derive` (`223`), and `verify` (`263`) execute `verify_writer_history_continuity` on the complete writer-notes state before selecting `BASE_SHA..HEAD_SHA` or `nb..c`. In `push_notes` (`166-188`), `$WRITER_NOTES_REF` is removed from `refs` (`170`) and lines `173-175` (`git update-ref refs/notes/remote/concrete-writer "$(git rev-parse "$WRITER_NOTES_REF")"`) are deleted: `push_notes` pushes `refs/notes/reviews` independently and delegates writer-notes synchronization, lease-checked publication, and transactional CAS checkpoint update exclusively to `python3 -B -I "$WRITER_PROV" publish-notes --repo "$REPO_ROOT" --remote "$REMOTE"`. |
| `scripts/review/verify.sh` (`scripts/review/verify.sh:72`, `182`) | Review-note verifier | Under `--fetch`, calls `writer_provenance.py fetch-notes` (`72`); for clean notes, calls `writer_provenance.py verify --base "$(field base)" --head "$SHA"` (`182`). | Both `fetch-notes` (`72`) and `verify` (`182`) call `verify_writer_history_continuity` on the complete writer-notes state before inspecting `base..SHA`. |
| `scripts/land/notes.sh` (`scripts/land/notes.sh:42`, `59-83`, `150-152`) and `scripts/land/ready.sh` (`96-98`) | Readiness and note reader (`notes.sh fetch`, `get writer`, `ready`) | `fetch_kind` (`42`) calls `fetch-notes`; `ready` (`150-152`, called by `ready.sh:98`) calls `verify.sh`; `get_note` (`63-75`) runs `wout="$(python3 -B -I "$WRITER_PROV" fetch-notes --no-fetch --target-ref "$ref" --no-update-local 2>&1)"` (`63`), then re-resolves `ref_sha="$(git for-each-ref ... "$ref")"` (`69`) and runs `git notes --ref="$ref" show "$2"` (`71`), treating `"no note found for object"` as `return 0` (`72-75`). | `get_note` (`scripts/land/notes.sh:59-83`) delegates `writer` note reads to a single Python invocation `python3 -B -I "$WRITER_PROV" show-note --repo . --target-ref "$ref" --commit "$2"`, which validates `$ref` via `verify_writer_history_continuity` and reads commit `$2`'s note blob directly from the validated immutable notes tree (`validated_state` / `read_note_from_commit`), eliminating both the TOCTOU re-resolution of `$ref` at `69-71` and the `git notes --ref` SHA-as-ref-name failure. `fetch` (`42`) and `ready` (`150-152`) inherit the unified predicate. |
| `scripts/land/signoff.sh` (`scripts/land/signoff.sh:191`) | Owner sign-off recorder and verifier (`signoff.sh record`, `verify`) | Calls `writer_provenance.py derive --repo . --base "$base" --head "$sha"` (`191`) inside `check()` (`148-229`). | Through `derive` (`191`), validates the complete writer-notes state and checkpoint ancestry via `verify_writer_history_continuity` before checking `base..sha`. |
| `scripts/land/train.sh` (`scripts/land/train.sh:338`, `559`, `570`, `585`, `586`) | Landing train (`train.sh build`, `check`, `land`) | `cmd_build` (`338`) and `review_provenance` (`559`, called from `review_leg` at `605`, `609` and `cmd_land` at `1113`) call `writer_provenance.py fetch-notes ... ${prev_wnotes:+--old-notes-rev "$prev_wnotes"}` and then `verify.sh` (`570`, `585`) and `signoff.sh verify` (`586`). | Every train entry point (`338`, `559`, `570`, `585`, `586`) enforces `verify_writer_history_continuity` over the complete state and all checkpoints (including the train's persisted `writer_notes` checkpoint at `162-169`, `335`, `558`). |

#### 3. Candidate-validity and continuity-loss channels, planted controls, and exact diagnostics

Every channel by which a writer-notes state can lose validity or continuity is rejected by `verify_writer_history_continuity` (or by atomic CAS ref updates) and pinned by planted positive and negative controls in `scripts/checks/review_tool.sh`, `scripts/checks/landing_ready.sh`, and `scripts/checks/landing_script.sh`:

| Channel | How the state loses continuity | Planted control that must reject it | Exact diagnostic |
|---|---|---|---|
| **1. Note deletion inside the range (`base..head`)** | A valid signed note on a commit $c \in \text{commits}(\text{base}..\text{head})$ present in a prior checkpoint $P \in \mathcal{P}$ or ancestor notes commit $A$ is removed in candidate $N$. | Create valid signed notes on $c_1, c_2$, record the notes tip at `refs/notes/remote/concrete-writer` (and test as parent commit $A$ with no companion ref), remove the note on $c_2$, and invoke `derive`, `report`, `verify`, `export-log`, `show-note` over `c1..c2`, `validate-notes`, `fetch-notes`, `verify.sh`, `notes.sh`, `train.sh`, and `.githooks/pre-push` (with and without `CONCRETE_SKIP_GATES=1`). | Against checkpoint $P \in \mathcal{P}$: `writer-notes push deletes existing writer-log entry for commit(s): <c[:10]>`<br>Against ancestor notes commit $A$: `writer-notes push deletes existing writer-log entry for commit(s): <c[:10]> (present in writer-notes commit <A[:10]>)` |
| **2. Note deletion outside the range (`base..head`)** *(Round-13 HIGH finding)* | A valid signed note on a commit $c_{\text{out}} \notin \text{commits}(\text{base}..\text{head})$ is removed in candidate $N$, while all notes on commits inside `base..head` remain valid. | Reproduce the Round-13 HIGH scenario: create two valid signed commit notes on $c_1$ and $c_2$, record the notes tip as the companion checkpoint (and in a second sub-case as a parent notes commit with no companion ref), delete the note on $c_1$, and run `derive`, `report`, `verify`, `export-log`, `show-note`, `review.sh`, `verify.sh`, and `signoff.sh verify` on the range covering only $c_2$ (`c1..c2`), plus `validate-notes`, `fetch-notes`, and `notes.sh get writer`. Include a negative-control mutation re-inserting `target_commits` filtering to prove the control fails if range filtering returns. | Against checkpoint $P \in \mathcal{P}$: `writer-notes push deletes existing writer-log entry for commit(s): <c1[:10]>`<br>Against ancestor notes commit $A$: `writer-notes push deletes existing writer-log entry for commit(s): <c1[:10]> (present in writer-notes commit <A[:10]>)` |
| **3. Correction rollback (inside or outside the range, or re-signing in place)** | A commit note with a valid 2-stanza correction chain ($s_1 \to s_2$) is rolled back to its 1-stanza prefix $s_1$ (by rewinding the ref to $N_1$, committing a rollback child $N_3$ on top of $N_2$, or replacing the note with a freshly signed 1-stanza note $s_1'$), on a commit inside or outside `base..head`, with or without a matching 1-stanza `--log-object`. | Build 1-stanza $N_1$ and 2-stanza $N_2$ (`claude -> kimi` / `kimi -> codex`) on both $c_{\text{out}} \notin \text{base}..\text{head}$ and $c_{\text{in}} \in \text{base}..\text{head}$; test rewinding to $N_1$ (with $N_2$ at companion checkpoint), child rollback $N_3$, and re-signed replacement $N_3'$ across `derive`, `report`, `verify` (with and without `--log-object`), `export-log`, `show-note`, `stamp-commit`, `backfill`, `correct`, `validate-notes`, `fetch-notes`, `sync-notes`, `publish-notes`, `review.sh`, `verify.sh`, `notes.sh`, `train.sh`, and `.githooks/pre-push`. | Against checkpoint $P \in \mathcal{P}$: `writer-notes push rewrites or drops existing writer-log entry on <c[:10]> (the writer log is append-only; corrections must append a signed superseding entry)`<br>Against ancestor notes commit $A$: `writer-notes push rewrites or drops existing writer-log entry on <c[:10]> from writer-notes commit <A[:10]> (the writer log is append-only; corrections must append a signed superseding entry)` |
| **4. Unrelated root with identical blobs** *(Round-13 MEDIUM finding)* | Candidate $N_{\text{root}}$ is a parentless notes commit (`git commit-tree <P^{tree}>`) carrying the exact same tree and valid signed note blobs as checkpoint $P \in \mathcal{P}$, but does not descend from $P$ by commit ancestry. | Reproduce the Round-13 MEDIUM scenario: create two valid signed commit notes at $P$, record $P$ as the companion checkpoint (`refs/notes/remote/concrete-writer`), create parentless $N_{\text{root}}$ with $P$'s exact tree, point `refs/notes/concrete-writer` at $N_{\text{root}}$, and test `validate-notes` (with no `--old-notes-rev` and with `--old-notes-rev 0000...`), `derive`, `report`, `verify`, `export-log`, `show-note`, `stamp-commit`, `backfill`, `fetch-notes --no-fetch`, `fetch-notes` (from remote), `notes.sh get writer`, `verify.sh`, `train.sh`, and `.githooks/pre-push`. Include a negative-control mutation replacing `is_ancestor` with tree equality to prove the control fails if content equality is accepted. | `writer-notes push is non-fast-forward (<P[:10]> is not an ancestor of <N_root[:10]>): refs/notes/concrete-writer history is append-only and may not be rewound or rewritten` |
| **5. Divergent history (unmerged rejection vs two-parent merge resolution)** *(Round-13 MEDIUM finding & Round-14 Lead 2)* | From common notes ancestor $P_0$, checkpoint $P_1$ and candidate $N_2$ diverge as sibling commits ($P_1 \not\preceq_{\text{commit}} N_2$). Any reader or validator given unmerged $N_2$ while $P_1 \in \mathcal{P}$ must reject $N_2$ (even if $N_2$ has identical or superset valid blobs); conversely, `sync-notes` and `publish-notes` given valid non-conflicting sibling tips $P_1$ (adding note on $c_2$) and $N_2$ (adding note on $c_3$, or extending $c_1$'s stanza chain) must privately construct the two-parent merge commit $M$ (`-p N_2 -p P_1`), verify $M$ through `verify_writer_history_continuity` against $\{P_0, P_1, N_2\}$, and advance the ref to $M$. | (a) Negative control on unmerged sibling $N_2$: create common ancestor $P_0$ (note on $c_1$), child checkpoint $P_1$ (adding note on $c_2$) at `refs/notes/remote/concrete-writer`, and sibling commit $N_2$ (parent $P_0$, identical tree $P_1\text{^{tree}}$ in sub-case A and superset tree adding notes on $c_2, c_3$ in sub-case B) at `refs/notes/concrete-writer`, and verify rejection across `validate-notes`, `derive`, `report`, `verify`, `export-log`, `show-note`, `stamp-commit`, `backfill`, `fetch-notes`, `notes.sh get writer`, `verify.sh`, `train.sh`, and `.githooks/pre-push`. (b) Positive divergent-merge control: with local at $N_2$ (note on $c_{\text{local}}$) and remote `origin` at sibling $P_1$ (note on $c_{\text{remote}}$) diverging from $P_0$, run `sync-notes` and `publish-notes` and verify that they succeed, produce a two-parent merge commit $M$ descending from both $N_2$ and $P_1$ containing all notes from $\{P_0, P_1, N_2\}$, and pass `derive`, `verify`, and `notes.sh get writer` on both $c_{\text{local}}$ and $c_{\text{remote}}$. (c) Conflicting divergence control: when $N_2$ and $P_1$ carry incompatible stanza chains on the same commit $c$, verify `sync-notes` fails closed. | On unmerged sibling candidate $N_2$: `writer-notes push is non-fast-forward (<P1[:10]> is not an ancestor of <N2[:10]>): refs/notes/concrete-writer history is append-only and may not be rewound or rewritten`<br>On conflicting stanza lists in `sync-notes`: `writer-notes sync conflict on <c[:10]> between <local_sha[:10]> and <remote_sha[:10]> (rewrites or drops existing writer-log entry)` |
| **6. Missing or unfetched checkpoint (or deleted local/remote ref)** | `--old-notes-rev` is empty (`""`) or names a nonzero SHA not present in the local object store, or the local/remote writer-notes ref is missing/deleted after prior history was recorded at a checkpoint $P \in \mathcal{P}$. | (a) Pass `--old-notes-rev ""` and unfetched nonzero SHA `deadbeef...` to `validate-notes`, `fetch-notes`, and `.githooks/pre-push`; (b) delete local `refs/notes/concrete-writer` while companion checkpoint $P$ exists and run all readers/mutators; (c) delete remote `refs/notes/concrete-writer` on `origin` (and then replay older $P_0$) across `fetch-notes`, `verify.sh --fetch`, `notes.sh fetch writer`, and `train.sh build`/`check`/`land`. | Empty `--old-notes-rev`: `old-notes-rev is empty (expected 40-hex creation sentinel 0000... or an existing notes commit)`<br>Unfetched nonzero SHA: `old-notes-rev '<rev>' is not available locally (every nonzero old notes SHA must resolve and be compared)`<br>Deleted local ref: `local <ref> is missing, but prior writer-log history was recorded at <prior_desc> (refusing deleted or missing local writer-notes ref)`<br>Deleted remote ref: `remote <remote> has no <remote_ref>, but prior writer-log history was recorded at <prior_desc> (refusing deleted or missing remote writer-notes ref)` |
| **7. Environment override (`REVIEW_WRITER_NOTES_REF` / `REVIEW_WRITER_ALLOWED_SIGNERS`)** | `REVIEW_WRITER_NOTES_REF` points at a distinct ref (`refs/notes/custom-writer-override`) holding a rolled-back, out-of-range-deleted, or orphan-root notes commit while standard refs (`refs/notes/concrete-writer`, `refs/notes/remote/concrete-writer`) hold $P$, or `REVIEW_WRITER_ALLOWED_SIGNERS` points at an empty/unauthorized signers file. | Set `REVIEW_WRITER_NOTES_REF=refs/notes/custom-writer-override` to (a) rolled-back $N_1$, (b) out-of-range note deletion, and (c) orphan root with identical blobs while standard companion refs hold $N_2$, testing `derive`, `report`, `verify`, `export-log`, `show-note`, `validate-notes`, and `fetch-notes --no-fetch`, plus the negative-control mutation removing companion refs from `check_refs`. Test empty and unlisted `REVIEW_WRITER_ALLOWED_SIGNERS`. | Override with out-of-range deletion: `writer-notes push deletes existing writer-log entry for commit(s): <c[:10]>`<br>Override with rollback: `writer-notes push rewrites or drops existing writer-log entry on <c[:10]> (the writer log is append-only; corrections must append a signed superseding entry)`<br>Override with orphan root: `writer-notes push is non-fast-forward (<N2[:10]> is not an ancestor of <N_root[:10]>): refs/notes/concrete-writer history is append-only and may not be rewound or rewritten`<br>Empty signers: `<source_desc>: unverified writer-log entry for commit <c[:10]>: no authorized signers configured in <path> (every entry is unverified until the orchestrator's public key is added after owner approval; see docs/REVIEW.md)` |
| **8. Shallow repository, grafted history, or git replacement refs (`refs/replace/*`, `.git/info/grafts`)** *(Round-14 Lead 4)* | (a) `.git/shallow` is present (`git rev-parse --is-shallow-repository` is `true` or `GIT_SHALLOW_FILE` is set), truncating history; (b) `.git/info/grafts` (or `GIT_GRAFT_FILE`) fabricates a parent link so an orphan/rewritten notes commit $N_{\text{orphan}}$ appears to descend from checkpoint $P$; (c) `refs/replace/<N_orphan>` (or `GIT_REPLACE_REF_BASE`) replaces $N_{\text{orphan}}$ with a valid descendant $N_{\text{good}}$ when git commands run without `--no-replace-objects`. | Plant (a) `.git/shallow` / shallow clone, (b) `.git/info/grafts` grafting checkpoint $P$ as parent of an orphan notes commit $N_{\text{orphan}}$ (and via `GIT_GRAFT_FILE`), and (c) `git update-ref refs/replace/$N_orphan $N_good` (and via `GIT_REPLACE_REF_BASE`) while `refs/notes/concrete-writer` points at $N_{\text{orphan}}$ and `refs/notes/remote/concrete-writer` points at $P$. Verify that `derive`, `report`, `verify`, `export-log`, `show-note`, `stamp-commit`, `backfill`, `validate-notes`, `fetch-notes`, `sync-notes`, and `publish-notes` all reject the repository state (and that `--no-replace-objects` is enforced on every git invocation). | Shallow repo: `repository is shallow (git rev-parse --is-shallow-repository is true): writer-notes continuity and ancestry require full commit history`<br>Grafted history: `repository has grafted history (<grafts_path>): writer-notes continuity and ancestry forbid git grafts`<br>Replacement refs: `repository has git replacement refs (refs/replace/<sha>): writer-notes continuity and ancestry forbid replacement objects` |
| **9. Refs replaced between check and use (TOCTOU) or during publication (`push_notes` / `publish-notes`), or unpinned `git notes --ref` read** *(Round-14 Leads 1 & 3)* | (a) A mutable ref (`refs/notes/concrete-writer` or `refs/notes/remote/concrete-writer`) is replaced after validation and before a reader reads per-commit notes or before a mutator/publisher updates local and companion refs; (b) `review.sh`'s `push_notes` (`172-175`) re-reads `$(git rev-parse "$WRITER_NOTES_REF")` after pushing and overwrites `refs/notes/remote/concrete-writer` with an unvalidated or concurrently modified ref; (c) a reader passes a 40-hex commit SHA to `git notes --ref="$sha" show`, which looks up `refs/notes/<sha>` and silently treats `"no note found for object"` as an absent note (`notes.sh:71-75`). | (a) Positive pinned-read control: run `notes.sh get writer "$c"` and `writer_provenance.py show-note --repo . --target-ref refs/notes/remote/concrete-writer --commit "$c"` and assert that the exact multi-line signed note stanza (and `--key model`) is returned non-empty; then use a git/hook wrapper that flips the mutable ref to an invalid/rolled-back commit mid-flight and verify that `derive` and `notes.sh get writer` still return the exact validated note or fail closed. (b) Concurrent mutation and publication CAS controls: flip `refs/notes/concrete-writer`, `refs/notes/remote/concrete-writer`, or `origin`'s `refs/notes/concrete-writer` mid-flight during `stamp-commit`, `backfill`, `fetch-notes`, `sync-notes`, `publish-notes`, and `review.sh` (`push_notes`) and verify that `git push --force-with-lease` / `git update-ref --stdin` (`cas_update_refs`) rejects the stale update without overwriting the concurrent change or corrupting `refs/notes/remote/concrete-writer`. | On CAS ref-update mismatch (`cas_update_refs` via `git update-ref --stdin`): `concurrent writer-notes ref update detected during <op>: fatal: cannot lock ref '<ref>': is at <actual_sha> but expected <expected_old_sha>`<br>On remote lease rejection after bounded retries in `publish-notes`: `writer-notes publish failed after concurrent remote updates on <remote>:<ref>` |
| **10. Invalid tree or object added without continuity loss** | Extend a valid checkpoint with all existing entries intact, adding a `100755` blob, a non-40-hex path, duplicate fanout paths for one SHA, an empty note, or a missing/wrong-type blob; separately test an empty initial tree. | Plant each independently with raw tree/commit objects; use a missing object only in a disposable copy. Preserve checkpoint ancestry and existing stanza prefixes. | `entry '<path>' has mode/type 100755 blob, expected 100644 blob`; `path '<path>' is not a 40-hex commit note`; `duplicate note path for <sha>`; `<ref> on <c> is empty`; `missing note blob <blob> for <c>`; `has an empty notes tree`. |
| **11. Invalid signed stanza or chain added without continuity loss** | Add a note on a new commit, or append a bad stanza without changing the existing prefix: malformed/duplicate keys, invalid fields/order, changed entry-id, missing/corrupt signature, unauthorized key/principal, wrong annotated commit, initial correction metadata, missing reason or wrong supersedes. | Plant each failure separately; re-sign syntactically valid semantic/chain failures so signature rejection cannot mask them. Include a valid multi-stanza positive control. | `duplicate key '<key>'`; `entry-id mismatch on commit <c>`; `missing entry-id, signer, or SSH signature`; `invalid SSH signature on writer-log entry`; `signing key is not authorized`; `signer '<signer>' does not match authorized principal(s)`; `records commit <other>, not <c>`; `correction entry requires non-empty single-line reason`; `supersedes <id>, which is not the latest entry <id>`. Other field/order/initial-chain cases assert their specific parser diagnostics (`459-658`), not a generic nonzero exit. |
| **12. Signed but semantically invalid new note** | Preserve every old note and ancestry while adding a correctly signed note for a missing commit, writer committer identity, invalid start/end ancestry, harness/backfill mismatch, mismatched author/model/trailers, or invalid agy diff trailer. | Independently plant every rule in invariant clause 1 and `validate_writer_history_state:1852-1954`; use a valid signature and chain. Test missing targets with default rejection and the explicit `validate-notes --allow-missing-targets` exception only; the exception must not disable other semantics or continuity. | Proposed missing-target diagnostic: `writer-notes <N>: annotated commit <c> is not available locally`; existing diagnostics include `committer '<name> <email>' must be the owner's identity, not a writer identity`, `start-sha <start> is not a strict ancestor`, `is not an ancestor-or-equal of its writer-log end-sha`, `carries a harness commit stamp and cannot use a backfilled writer-log entry`, and `writer-log model '<model>' does not match commit stamp '<model>'`. |
| **13. Export substitution as supplemental evidence** | A valid authority has a correction chain; file/blob exports omit it, truncate it, conflict, invent history for an unauthoritative commit, or add entries outside the range. | Preserve `load_log_object` parse/signature rejection and all `verify_history_against_authority` paths; exercise both file and blob forms, plus matching-export success. Matching exports with an invalid out-of-range authoritative note still fail the shared predicate first. | `exported writer-log is missing history`; `has truncated history`; `conflicts with authoritative writer history`; `contains history for commit <c>, which has no authoritative note`; `writer-log contains entries for commits not in <base>..<head>`. |

**Coverage across the inventory, not just selected commands.** Channels 10–12 each include a newly invalid note outside the requested range while all requested notes remain valid. Run the planted matrix through every authoritative reader/validator in section 2: Python helper-level calls (including state, continuity, tree/parser/signature and range validators), all reader CLIs, mutators, fetch/no-fetch, sync, publication, pre-push and shell consumers. Shell review/publication controls use disposable local remotes and stub reviewer output, never an actual reviewer. For mutators and publishers assert rejection before ref changes or push; for readers assert no provenance/note output is accepted. Supplemental validators are exercised with malformed exports as well as matching exports against invalid authority. Preserve range evidence, independence, missing/truncated/conflicting export and object-reader rejection controls; these have their own obligations and are not alternate continuity authorities. A helper inventory control maps each named helper/consumer to its cases; missing coverage fails rather than silently skipping a command.

**Complete-set race control (channel 9 extension).** With an override active, pause each installer after candidate validation; advance the unchanged standard writer ref to an incompatible sibling. The write ref's expected SHA still matches, but the unchanged ref's `verify` must abort the entire transaction. Repeat for every unchanged companion/target ref and for absent-to-created, present-to-deleted, and same-SHA/different-name companions, including no-op fetch/ratchet branches. Assert all intended updates remain uninstalled and the concurrent change survives. Repeat between snapshot enumeration and verification-only acceptance, and between post-push validation and acknowledgement; the latter reports successful remote push plus failed local acknowledgement. A negative mutation dropping unchanged/absence verifies must make these controls fail.

#### 4. Staged implementation plan

Round 16 changes this design note only, under the owner-recorded third-design-round exception. Implementation follows in dependency order, with independent review per slice; these are scope boundaries, not promised round numbers. No slice may claim the full invariant until slice C and its required caller changes land together. This follows [PHILOSOPHY.md](PHILOSOPHY.md)'s single owner and explicit trust-boundary rules.

| Slice | Bounded implementation and required controls | Dependency / review boundary |
|---|---|---|
| A. Canonical runner | Route the five Git invocation sites through the canonical runner/environment; add canonical-view checks and shallow/graft/replacement controls (channel 8). | No predicate/API, merge, publication or CLI changes. Review only the runner and its call-site routing. |
| B. Snapshot and transaction mechanism | Introduce `CheckpointSnapshot`, capture plus verification-only acceptance, and complete-set `cas_update_refs`; unit controls for unchanged refs, aliases and expected absence. | Depends on A. Infrastructure only, no authority-policy change; test helper transactions in disposable repositories. |
| B1. Private note construction | Extract private tree/commit construction for stamp, backfill and correction; preserve current validation policy while installing through B's complete transaction. | Depends on B. Mutation mechanics and race controls only; no predicate strengthening. |
| B2. Merge preparation | Extract isolated input validation and private sibling-merge construction from sync, preserving prefix conflict rules; move fetch/ratchet installation to B. | Depends on B1. Sync/fetch mechanics with positive sibling merge, conflict and complete-set race controls; no new CLI or range policy. |
| B3. Snapshot plumbing | Pass snapshot/state through existing Python readers, validators and publisher; preserve export rejection and separate review-note uses. Replace per-commit writer-ref rereads with the returned pinned state under the existing validation policy. | Depends on B2. Mechanical caller interfaces and pinned-read controls; no strengthened continuity claim. |
| C. Complete predicate and mandatory callers | Strengthen the shared predicate (whole-state validity, complete tree/history, all-checkpoint ancestry, no range filter), return the pinned state, and adapt **all existing Python callers** in the same slice: range readers consume it, validators preserve exports, mutators build private candidates and install with B, fetch/ratchet paths use B, and sync separates sibling inputs from its private merge candidate and installs with B. Existing Python publisher must use the snapshot contract too. | Depends on B3. Private construction, transaction adoption and snapshot plumbing have already been reviewed; C changes the predicate and only its required semantic caller adaptations. This is the inseparable semantic slice: no temporary bypass, weakened predicate, or broken divergent-sync intermediate. Excludes new CLI, shell migrations and publication consolidation. Its review examines the predicate plus necessary adapters only. Controls cover channels 1–7 and 10–13 at Python boundaries, valid sibling merge and unchanged-companion races for each existing installer. Review the remaining semantic diff as one unit; never land a stronger predicate against incompatible callers. Further mechanical extraction, if required by the actual diff, precedes C and cannot relax its obligations. |
| D. Pinned note interface | Add `read_note_from_commit` / `show-note`; migrate `notes.sh get writer` to it; retain review-note reading separately. | Depends on C. Exact multiline and key-return positives, invalid out-of-range note rejection, and pinned-read races; no publisher changes. |
| E. Single publication lifecycle | Consolidate synchronization, immutable leased push, bounded retries and complete-snapshot acknowledgement in `publish-notes`; migrate `review.sh push_notes` to that API in the same slice, removing its writer-ref push/update path. | Depends on C. Remote lease/absence controls and post-push unchanged-companion races, with explicit partial-success reporting. Local disposable remotes only. |
| F. Consumer coverage | Complete the shell integration matrix for `verify.sh`, readiness, signoff, train and pre-push; audit every inventory row against channels 1–13 and retained evidence/independence checks. | Depends on D and E. Consumer controls only; no new predicate semantics. Any uncovered implementation defect returns to its owning slice rather than being hidden in this coverage slice. |

Each slice carries its own impact-selected checks and exact-SHA result. Preparatory slices make no claim to repair continuity. C is larger than a helper refactor because the predicate and its callers form one semantic unit; its explicit scope exclusions and advance extraction option replace the unsupported claim that all earlier work fits comfortably in one review. The implementation coordinator must assess its actual diff before requesting review and split preparatory mechanics further if needed, preserving that semantic unit.

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
