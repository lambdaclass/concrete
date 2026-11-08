- Reviewer outputs now parse through one shared contract reader, `scripts/review/output_contract.py`
  (ISS-0021): at least one `READ: <path>` line naming a file in the reviewed commit's tree,
  `REPOSITORY: readable` as the penultimate non-empty line (any `REPOSITORY: unreadable` or sandbox
  failure text blocks immediately), and `VERDICT: <value>` as the last non-empty line. `review.sh`
   runs Codex, Kimi (`kimi -p`, never `--yolo` or `--auto`) and agy (`--dangerously-skip-permissions`
  only inside the archive) in a read-only (`chmod -R a-w`) temporary `git archive` copy checked for
  immutability after each piece, treats `bounded_run` double-timeouts as `blocked` without halving
  retry, and writes `repository: readable` to clean notes. `scripts/land/signoff.sh` validates each
  sign-off reviewer output through the same contract reader.
- Writer provenance is derived and cross-checked from the branch commits and the git-object writer
  log (`scripts/review/writer_provenance.py`, `refs/notes/concrete-writer`): harness-written commits
  carry author `<Model> (writer) <model@writers.concrete>` (committer remains the owner), a
  `Writer: <model>` trailer (plus `Agy-Diff-SHA256:` for `agy`), and a matching writer-log entry per
  commit; a `Writer:` trailer alone is refused; unstamped human/pre-harness commits are explicitly
  attributed to `claude`; multi-writer branches record all writers in order and refuse any of them
  as reviewer. `--writer` on `review.sh` and `LAND_REVIEW_WRITER` are now optional assertions.
- Every review prompt, including its supplied diff piece, is saved byte for byte under `.review/`
  and its sha256 is recorded in the round's raw output (ISS-0022).
- Review provenance and delta selection enforce base coverage (ISS-0019). `verify.sh` requires
  `base:` to be an ancestor-or-equal of the reviewed commit and, with `--required-base`, an
  ancestor-or-equal of the required base; `review.sh` delta selection requires a prior note's base
  to cover `BASE_SHA` so a delta round never widens base coverage across an unreviewed `A..B` range;
  and `train.sh`, `ready.sh` and `notes.sh ready` all validate review notes through `verify.sh`.
- Standard and neutral review prompts permanently carry the owner's 2026-11-08 rule to choose the
  best long-term design and report shortcuts with their structural fixes.
