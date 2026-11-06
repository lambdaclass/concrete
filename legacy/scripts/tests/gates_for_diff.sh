#!/usr/bin/env bash
# WHICH GATES CAN THIS DIFF BREAK?
#
# CI runs 204 `check_*.sh` gates. Nothing mapped a change to the gates it could break, so
# the practical procedure was "know which ones to run", and that procedure fails exactly
# when a change reaches further than its author expected. R-0483 is the worked example: a
# stdlib rewrite broke, in four separate CI rounds, the stdlib surface manifest, the
# shadow-body extraction coupling, six mutation-family anchors, and the ByteView gate.
# Each was a DIFFERENT derived artifact of one change, and each round fixed only what CI
# had just named.
#
# The mutation-anchor one is why this exists rather than being a convenience. Renaming a
# symbol left six anchors pointing at text that no longer occurred, and an anchor that
# does not match does not fail loudly — the mutation becomes inert, applies nothing, and
# the gate it exists to prove silently stops proving it while the campaign still counts
# the family. A gate that quietly stops testing is worse than a gate that goes red.
#
# HOW THE MAPPING IS DERIVED. Gates name the files they check — `check_byte_view.sh`
# contains `std/src/numeric.con`, `check_stdlib_manifest.sh` contains `std/src/`. So the
# map is read out of the gates themselves rather than hand-maintained, which means it
# cannot drift out of date the way a curated list would. Gate-to-gate references are
# included for free and matter: `check_mutation_anchors.sh` names
# `check_gate_mutation_coverage.sh`, so editing the driver implicates the anchor gate.
#
# WHAT THIS IS NOT. It is not a proof of completeness, and treating it as one would
# repeat the mistake R-0483 just removed from `ByteView` — a check that catches some
# cases reads at the call site like one that catches all of them. A gate can be broken by
# a change it never names: it may compile a whole corpus, or assert a count derived from
# everything. The ALWAYS set below covers the derived-artifact gates for that reason, and
# the output says plainly what the mapping does and does not establish. When in doubt,
# run the job.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

BASE=""
RUN=0
for arg in "$@"; do
  case "$arg" in
    --run)    RUN=1 ;;
    --base=*) BASE="${arg#--base=}" ;;
    -h|--help)
      echo "usage: gates_for_diff.sh [--run] [--base=REF]"
      echo "  no --base: uncommitted changes (staged + unstaged + untracked)"
      echo "  --base=REF: everything since REF, plus uncommitted"
      echo "  --run: execute the selected gates instead of only listing them"
      exit 0 ;;
    *) echo "unknown argument: $arg" >&2; exit 2 ;;
  esac
done

# ---- changed files ----------------------------------------------------------
{
  git diff --name-only
  git diff --name-only --cached
  git ls-files --others --exclude-standard
  [ -n "$BASE" ] && git diff --name-only "$BASE"...HEAD
} 2>/dev/null | sed '/^$/d' | LC_ALL=C sort -u > /tmp/gfd_changed.$$
GFD_BOUNDED="$(mktemp -d)"
trap 'rm -f /tmp/gfd_changed.$$ /tmp/gfd_sel.$$; rm -rf "$GFD_BOUNDED"' EXIT
BOUNDED_PARENT="$GFD_BOUNDED"; source scripts/tests/lib/bounded.sh

nchanged=$(wc -l < /tmp/gfd_changed.$$)
if [ "$nchanged" -eq 0 ]; then
  echo "no changed files — nothing to select"
  exit 0
fi

echo "=== $nchanged changed file(s) ==="
awk 'NR<=12' /tmp/gfd_changed.$$ | sed 's/^/  /'
[ "$nchanged" -gt 12 ] && echo "  ... and $((nchanged - 12)) more"
echo

# ---- selection ---------------------------------------------------------------
# The selector lives in `lib/gates_for_diff.py` so it can be exercised directly by
# `check_gates_for_diff.sh` against a known diff. A mapping nothing tests is a mapping
# that quietly stops mapping, which is the failure mode this whole tool exists to stop.
python3 scripts/tests/lib/gates_for_diff.py < /tmp/gfd_changed.$$ > /tmp/gfd_sel.$$

nsel=$(wc -l < /tmp/gfd_sel.$$)
echo "=== $nsel gate(s) that name what changed, or derive from it ==="
while IFS=$'\t' read -r g why; do
  printf '  %-42s %s\n' "$g" "$why"
done < /tmp/gfd_sel.$$

echo
echo "This is a MAPPING, not a proof of completeness. A gate can be broken by a change it"
echo "never names — it may compile a corpus, or assert a count derived from everything."
echo "Treat an empty or short list as 'no obvious target', never as 'nothing can break'."

[ "$RUN" -eq 0 ] && exit 0

echo
echo "=== running $nsel gate(s) ==="
pass=0; fail=0; failed=""
while IFS=$'\t' read -r g why; do
  case "$why" in
    *"[not auto-run:"*)
      reason="${why##*\[not auto-run: }"; reason="${reason%]}"
      printf '  %-42s skipped (%s)\n' "$g" "$reason"
      continue ;;
  esac
  printf '  %-42s ' "$g"
  # `$g` may carry flags (e.g. `run_tests.sh --trust-gate`), so it is not quoted as one word.
  if bounded_run -r 1800 1800 -- bash scripts/tests/$g >/tmp/gfd_out.$$ 2>&1; then
    echo "ok"; pass=$((pass+1))
  else
    echo "FAIL"; fail=$((fail+1)); failed="$failed $g"
    grep -iE "^  FAIL|FAIL " /tmp/gfd_out.$$ | awk 'NR<=3' | sed 's/^/        /'
  fi
  rm -f /tmp/gfd_out.$$
done < /tmp/gfd_sel.$$

echo
echo "GATES-FOR-DIFF: PASS=$pass FAIL=$fail"
[ -n "$failed" ] && echo "failed:$failed"
[ "$fail" -eq 0 ]
