- Reviewer outputs are collected as the reviewer's final message (never a stdout/stderr log) through
  one shared collector and validation module (`scripts/review/collect_output.sh`:
  `prepare_review_archive`, `verify_review_archive`, `collect_reviewer_output`, and
  `validate_reviewer_output`, checked structurally by `collect_output.sh --check-structure`) and
  parsed through one strict contract reader, `scripts/review/output_contract.py` (ISS-0021): at least
  one `READ: <path>` line naming a file in the reviewed commit's tree, `REPOSITORY: readable` as the
  penultimate non-empty line (any `REPOSITORY: unreadable` or sandbox failure text in output or
  `--log` blocks immediately), and `VERDICT: <value>` as the last non-empty line. `review.sh` and
  `signoff.sh collect` share `prepare_review_archive`, `collect_reviewer_output` and
  `validate_reviewer_output` to run Codex (`codex exec -o <out>`), Kimi (`kimi -p`, never `--yolo` or
  `--auto`; `kimi --help` offers only `--output-format text|stream-json` and has no `-o` /
  final-message-only flag) and agy (`--dangerously-skip-permissions -p` with stderr kept separate) in
  a read-only (`chmod -R a-w`) temporary `git archive` copy verified against its before/after
  manifest after each reviewer run, evaluate process outcome (`--rc`, `--incomplete` on
  `bounded_run` double-timeout) and diagnostic log (`--log`), and write `repository: readable` to
  clean notes. `scripts/land/signoff.sh` validates each sign-off reviewer output through the same
  strict contract reader with no legacy-format grandfathering (owner item 5, 2026-11-08), while past
  landed commits are not re-judged.
- Writer provenance is derived and cross-checked from the branch commits and the SSH-signed,
  append-only git-object writer log (`scripts/review/writer_provenance.py`,
  `refs/notes/concrete-writer`): each commit has one authoritative writer history on its own
  `refs/notes/concrete-writer` note (refusing stanzas for other commits in a commit's note and
  verifying exported `--log-object` logs against the authoritative per-commit histories to reject
  conflicting or truncated copies); every writer-log stanza (`source: harness` and `source:
  backfill`, including corrections) carries `entry-id`, `signer`, and a detached SSH signature
  verified against `scripts/review/allowed_writer_log_signers` (configured with the owner-approved
  `orchestrator@concrete` Ed25519 public key `SHA256:vx5cqlK2RywhQLRwaOCplg2BeBCnEBiZnxF8wn/CSLU`,
  approved 2026-11-08, item 7; fails closed if no active key is present); harness-written commits
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
  entry already did so, and `.githooks/pre-push` `validate-notes` requires every nonzero
  `--old-notes-rev` to resolve locally and be compared while refusing any update that deletes,
  rewrites or drops an existing entry and leaving unchanged historical notes untouched); readers
  re-check brief and output files when provided (reporting `"evidence not re-checked"` otherwise);
  and multi-writer branches record all writers in order and refuse any of them as reviewer. `--writer`
  on `review.sh` and `LAND_REVIEW_WRITER` are optional assertions, and `train.sh`, `review.sh`, and
  `verify.sh` fail closed when fetching `refs/notes/concrete-writer` fails while clearing any stale
  local ref when the remote has no such ref.
- `scripts/review/verify.sh --fields` exposes validated note fields (`commit`, `verdict`, `rounds`,
  `reviewer`, `writer`, `utc`, `base`, `pieces`, `repository`) independent of clean-provenance
  eligibility, and `scripts/land/signoff.sh` (and `review.sh`) reads note fields through it so
  `findings`, `blocked`, and provenance-invalid clean notes at the round cap remain valid cap
  evidence without grandfathering old 6-key notes.
- Every review prompt, including its supplied diff piece, is saved byte for byte under `.review/`
  and its sha256 is recorded in the round's raw output (ISS-0022).
- Review provenance and delta selection enforce base coverage (ISS-0019). `verify.sh` requires
  `base:` to be an ancestor-or-equal of the reviewed commit and, with `--required-base`, an
  ancestor-or-equal of the required base; `review.sh` delta selection requires a prior note's base
  to cover `BASE_SHA` so a delta round never widens base coverage across an unreviewed `A..B` range;
  and `train.sh`, `ready.sh` and `notes.sh ready` all validate review notes through `verify.sh`.
- Standard and neutral review prompts permanently carry the owner's 2026-11-08 rule to choose the
  best long-term design and report shortcuts with their structural fixes.
