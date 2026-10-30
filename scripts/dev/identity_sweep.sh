#!/usr/bin/env bash
# Byte-identity of two compiler binaries' outputs: the evidence a behaviour-preserving refactor owes.
#
#   scripts/dev/identity_sweep.sh <base-binary> <new-binary> <out-dir> [--emit|--report|--all]
#
#   --emit    --emit-core, --emit-ssa, --emit-llvm, --check and --interp over every example and
#             every tests/programs/*.con
#   --report  every --report kind over every example and every 8th test program
#   --all     both (the default)
#
# Outputs are normalised before comparison for what legitimately differs between two builds of one
# source: the build id, timestamps, the addresses in a crash backtrace line (`bin(+0x..) [0x..]`,
# and nothing else: LLVM text carries float constants as hexadecimal) and the binary's own path.
# Anything else that differs is printed, and the exit status is 1. A sweep that did not run every
# comparison on both binaries is an error (exit 2), never "identical".
#
# A sweep is a heavy run (R-0543): it takes one of the machine-wide heavy slots
# (scripts/tests/lib/heavy_slots.sh) and waits for one, so sweeps and suites share the host's cap.
# JOBS defaults to the slot's fair share.
set -uo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
usage() { sed -n '2,10p' "$0" >&2; exit 2; }
{ [ $# -eq 3 ] || [ $# -eq 4 ]; } || usage
WHAT="${4:---all}"
case "$WHAT" in --emit|--report|--all) ;; *) usage ;; esac
[ -x "$1" ] && [ -x "$2" ] || { echo "identity_sweep: both binaries must be executable" >&2; exit 2; }
BASE="$(cd "$(dirname "$1")" && pwd)/$(basename "$1")"
NEW="$(cd "$(dirname "$2")" && pwd)/$(basename "$2")"
OUT="$3"
for tool in timeout xargs sed diff; do
  command -v "$tool" >/dev/null || { echo "identity_sweep: '$tool' is required (run inside nix develop)" >&2; exit 2; }
done
source "$ROOT/scripts/tests/lib/heavy_slots.sh"
heavy_slot_acquire "identity_sweep $WHAT" || exit $?
trap heavy_slot_release EXIT
JOBS="${JOBS:-$(heavy_jobs_default)}"
case "$JOBS" in ''|*[!0-9]*|0) echo "identity_sweep: JOBS must be a positive integer, got '$JOBS'" >&2; exit 2 ;; esac
cd "$ROOT"

KINDS="alloc arithmetic artifact-fuzz assumptions attestation-join audit authority backend-contracts body-bytes bool-kernel bridge-check caps check-proofs consistency contracts core-semantics-diff diagnostic-codes diagnostics-json effects eligibility extraction fingerprints generated-implementations impl-manifest interface layout lean-stubs lowering-agreement migration migration-manifest model-diff mono multi-kernel obligation-ledger obligations package-identity proof proof-bundle proof-deps proof-diagnostics proof-status receipts recursion schema solver-cert stack-depth subject-facts term-ir traceability trust-edges unsafe vcgen-diff vcs verify"
MODES="--emit-core --emit-ssa --emit-llvm --check --interp"

run_one() {  # $1 tag, $2 binary, $3 input, $4 flag..., output keyed by input and flags
  local tag="$1" bin="$2" src="$3"; shift 3
  local key; key="$(echo "$src" | tr '/' '_')$(echo "$*" | tr ' ' '_')"
  ( cd "$(dirname "$src")" && timeout 60 "$bin" "$(basename "$src")" "$@" < /dev/null ) > "$OUT/$tag/$key.out" 2>&1
  echo "rc=$?" >> "$OUT/$tag/$key.out"
}
export -f run_one; export OUT

rm -rf "$OUT"; mkdir -p "$OUT/base" "$OUT/new"
: > "$OUT/jobs.txt"
if [ "$WHAT" != "--report" ]; then
  { ls examples/*/src/main.con examples/*/main.con 2>/dev/null; ls tests/programs/*.con; } \
    | while read -r f; do for m in $MODES; do echo "$f $m"; done; done >> "$OUT/jobs.txt"
fi
if [ "$WHAT" != "--emit" ]; then
  { ls examples/*/src/main.con examples/*/main.con 2>/dev/null; ls tests/programs/*.con | awk 'NR % 8 == 0'; } \
    | while read -r f; do for k in $KINDS; do echo "$f --report $k"; done; done >> "$OUT/jobs.txt"
fi
want="$(wc -l < "$OUT/jobs.txt" | tr -d ' ')"
[ "$want" -gt 0 ] || { echo "identity_sweep: no comparisons were scheduled" >&2; exit 2; }
for tag in base new; do
  bin="$BASE"; [ "$tag" = new ] && bin="$NEW"
  xargs -P "$JOBS" -L 1 bash -c 'run_one '"$tag"' '"$bin"' "$@"' _ < "$OUT/jobs.txt" \
    || { echo "identity_sweep: running the $tag binary failed (xargs exit $?)" >&2; exit 2; }
  got="$(find "$OUT/$tag" -name '*.out' | wc -l | tr -d ' ')"
  [ "$got" = "$want" ] || { echo "identity_sweep: $tag produced $got of $want outputs" >&2; exit 2; }
done
find "$OUT/base" "$OUT/new" -name '*.out' -print0 | xargs -0 sed -i.bak -E \
  -e 's/\(build:[0-9a-f]+\)/(build:X)/g' \
  -e 's/"compiler": *"[^"]*"/"compiler": "X"/g' \
  -e 's/"timestamp": *"[^"]*"/"timestamp": "T"/g' \
  -e 's/[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9:.]+Z?/TS/g' \
  -e 's/\(\+0x[0-9a-f]+\) \[0x[0-9a-f]+\]/(+ADDR) [ADDR]/g' \
  -e "s#$BASE#BIN#g; s#$NEW#BIN#g"
find "$OUT/base" "$OUT/new" -name '*.bak' -delete
n="$(find "$OUT/base" -name '*.out' | wc -l | tr -d ' ')"
if diff -rq "$OUT/base" "$OUT/new" > "$OUT/diff.txt"; then
  echo "IDENTITY-SWEEP ($WHAT): $n outputs byte-identical after normalisation"
else
  echo "IDENTITY-SWEEP ($WHAT): $(wc -l < "$OUT/diff.txt" | tr -d ' ') of $n outputs differ:"
  head -10 "$OUT/diff.txt"
  exit 1
fi
