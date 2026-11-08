- Review provenance now requires evidence that Codex could read the repository (ISS-0021).
  `review.sh` runs Codex in a temporary archive of the reviewed commit, blocks missing readability
  markers and known sandbox failures, and writes `repository: readable` to clean notes. `verify.sh`
  and the landing train no longer accept legacy diff-only clean notes; running `review.sh` again
  replaces them with a reviewed result.
- Every Codex review prompt, including its supplied diff piece, is saved byte for byte under
  `.review/` and its sha256 is recorded in the round's raw output (ISS-0022).
