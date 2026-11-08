- Reviewer outputs now parse through one shared contract reader, `scripts/review/output_contract.py`
  (ISS-0021): at least one `READ: <path>` line naming a file in the reviewed commit's tree,
  `REPOSITORY: readable` as the penultimate non-empty line (any `REPOSITORY: unreadable` or sandbox
  failure text blocks immediately), and `VERDICT: <value>` as the last non-empty line. `review.sh`
   runs Codex, Kimi (`kimi -p`, never `--yolo` or `--auto`) and agy (`--dangerously-skip-permissions`
  only inside the archive) in a read-only (`chmod -R a-w`) temporary `git archive` copy checked for
  immutability after each piece, treats `bounded_run` double-timeouts as `blocked` without halving
  retry, and writes `repository: readable` to clean notes. `scripts/land/signoff.sh` validates each
  sign-off reviewer output through the same contract reader.
- Writer provenance is derived and cross-checked from the branch commits and the SSH-signed,
  append-only git-object writer log (`scripts/review/writer_provenance.py`,
  `refs/notes/concrete-writer`): every writer-log stanza (`source: harness` and `source: backfill`,
  including corrections) carries `entry-id`, `signer`, and a detached SSH signature verified against
  `scripts/review/allowed_writer_log_signers` (shipped with no key configured so provenance fails
  closed until the orchestrator's public key is added after owner approval); harness-written commits
  carry author `<Model> (writer) <model@writers.concrete>` (committer remains the owner), a
  `Writer: <model>` trailer (plus `Agy-Diff-SHA256:` for `agy`), and a matching `source: harness`
  writer-log entry per commit; a `Writer:` trailer alone is refused; unstamped non-merge commits and
  unstamped merge commits whose tree differs from git's conflict-free automatic merge have an
  unknown writer and are refused unless backfilled via `writer_provenance.py backfill` (`source:
  backfill`, `--brief-file`, `--output-file`, `output-sha256`, and `evidence`); `claude` is a writer
  only when explicitly logged; clean automatic merges add no writer; the writer log is append-only
  (corrections append a signed entry with `supersedes` and `reason`, full history is reported, any
  model attested in an earlier superseded entry remains disqualified from reviewing that commit,
  `backfill` refuses attributing a commit to its review note's reviewer unless an earlier signed
  entry already did so, and `.githooks/pre-push` `validate-notes` refuses any update that rewrites or
  drops an existing entry); readers re-check brief and output files when provided (reporting
  `"evidence not re-checked"` otherwise); and multi-writer branches record all writers in order and
  refuse any of them as reviewer. `--writer` on `review.sh` and `LAND_REVIEW_WRITER` are optional
  assertions, and `train.sh`, `review.sh`, and `verify.sh` fail closed when fetching
  `refs/notes/concrete-writer` fails while clearing any stale local ref when the remote has no such
  ref.
- `scripts/review/verify.sh --fields` exposes validated note fields (`commit`, `verdict`, `rounds`,
  `reviewer`, `writer`, `utc`, `base`, `pieces`, `repository`) independent of clean-provenance
  eligibility, and `scripts/land/signoff.sh` (and `review.sh`) reads note fields through it so
  legacy and provenance-invalid clean notes at the round cap remain valid cap evidence.
- Every review prompt, including its supplied diff piece, is saved byte for byte under `.review/`
  and its sha256 is recorded in the round's raw output (ISS-0022).
- Review provenance and delta selection enforce base coverage (ISS-0019). `verify.sh` requires
  `base:` to be an ancestor-or-equal of the reviewed commit and, with `--required-base`, an
  ancestor-or-equal of the required base; `review.sh` delta selection requires a prior note's base
  to cover `BASE_SHA` so a delta round never widens base coverage across an unreviewed `A..B` range;
  and `train.sh`, `ready.sh` and `notes.sh ready` all validate review notes through `verify.sh`.
- Standard and neutral review prompts permanently carry the owner's 2026-11-08 rule to choose the
  best long-term design and report shortcuts with their structural fixes.
