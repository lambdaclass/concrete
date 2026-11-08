- Review provenance now requires evidence that Codex could read the repository (ISS-0021).
  `review.sh` runs Codex in a temporary archive of the reviewed commit, blocks missing readability
  markers and known sandbox failures, and writes `repository: readable` to clean notes. `verify.sh`
  and the landing train no longer accept legacy diff-only clean notes; running `review.sh` again
  replaces them with a reviewed result.
- Every Codex review prompt, including its supplied diff piece, is saved byte for byte under
  `.review/` and its sha256 is recorded in the round's raw output (ISS-0022).
- Review provenance now covers the landing train's full diff (ISS-0019). `verify.sh
  --required-base` accepts a clean note only when its review base is an ancestor-or-equal of the
  required base, and the train applies that rule to both head and member notes.
- Reviews name an independent reviewer and writer. `review.sh` supports Codex, Kimi and agy in the
  same disposable archive, requires `--writer`, refuses self-review, and records `reviewer:` and
  `writer:` in notes; legacy and self-reviewed clean notes are not clean provenance.
- Standard and neutral review prompts permanently carry the owner's 2026-11-08 rule to choose the
  best long-term design and report shortcuts with their structural fixes.
