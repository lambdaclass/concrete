#!/usr/bin/env bash
# THE ORACLE CHECK ON A GITHUB-HOSTED RUNNER (ISS-0005, P13; ROADMAP R-0564).
#
#   ci_check.sh    fetch the published oracle (fetch.sh: download, artifact sha256, install
#                  manifest, binary hash), then compile and run scripts/oracle/smoke.con through it
#                  (run.sh) and require its exit code, 42
#
# This is what proves that a runner other than concrete-ci-2 gets a verified oracle from the release
# alone. Exit status: 0 passed; 3 INCOMPLETE (no published artifact, a failed download, a digest or
# manifest mismatch, an install that fails verification: the oracle could not be trusted, so nothing
# was checked); 1 FAILED (the verified oracle ran and the smoke program did not exit 42). Both 1 and 3
# fail the CI job, so a required oracle check is never reported as passed without a verified oracle.
# A compile failure is FAILED whatever its status: run.sh announces it with "oracle: compile failed"
# on stderr, so a compiler (or nested nix) that happened to exit 42 is never taken for the program.
# It needs only bash and nix and sources nothing outside scripts/oracle, which it is retained with
# (RETENTION.tsv); CI runs it in the gate shell (check_gate_hygiene.sh GUARD_EXEMPT says why).
set -uo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
incomplete() { echo "oracle-check: INCOMPLETE: $*" >&2; exit 3; }
bash "$HERE/fetch.sh"; rc=$?
[ "$rc" = 0 ] || incomplete "fetch.sh exited $rc: the oracle is not installed and verified"
ERR="$(mktemp)" || exit 2; trap 'rm -f "$ERR"' EXIT
bash "$HERE/run.sh" "$HERE/smoke.con" 2>"$ERR"; rc=$?
cat "$ERR" >&2
[ "$rc" = 125 ] && incomplete "run.sh refused the install (it failed verification)"
if grep -q '^oracle: compile failed' "$ERR"; then
  echo "oracle-check: FAILED: the verified oracle did not compile smoke.con (status $rc)" >&2; exit 1
fi
[ "$rc" = 42 ] || { echo "oracle-check: FAILED: smoke.con exited $rc through the oracle, expected 42" >&2; exit 1; }
echo "oracle-check: passed: the published oracle fetched, verified, and ran smoke.con to exit 42"
