#!/usr/bin/env bash
# THE MINIMAL ISSUE RECORD IS WELL-FORMED (P12).
#
# ISSUES.tsv holds spec, proof, launcher, CI and migration defects from day one, before the bug
# ledger returns with slice 1. A record nobody can parse or sort is not a record, so this gate holds
# it to its header: the exact column list; ids ISS-NNNN, unique and increasing; ISO dates; the area,
# severity and status vocabularies; no empty field (evidence, owner and the closure condition are
# what make an issue actionable). Each rule has a negative control against a mutated copy.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR" || exit 2
[ -f ISSUES.tsv ] || { echo "FATAL: ISSUES.tsv missing" >&2; exit 2; }
TMP="$(mktemp -d)" || exit 2
trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
problems(){ python3 -I scripts/ids/issues.py "$1"; }

echo "=== ISSUES.tsv ==="
rc=0; out="$(problems ISSUES.tsv)" || rc=$?
[ "$rc" = 0 ] && [ -z "$out" ] && ok "ISSUES.tsv is well-formed ($(grep -c '^ISS-' ISSUES.tsv) issues, $(grep -c $'\topen$' ISSUES.tsv) open)" \
  || { no "ISSUES.tsv has problems"; echo "$out" | sed 's/^/      /'; }

echo "=== negative controls ==="
mut(){ # label awk-program expected-text
  awk -F'\t' -v OFS='\t' "$2" ISSUES.tsv > "$TMP/m.tsv"
  if cmp -s ISSUES.tsv "$TMP/m.tsv"; then no "$1: the mutation changed nothing"; return; fi
  local verdict rc=0
  verdict="$(problems "$TMP/m.tsv")" || rc=$?
  if [ "$rc" != 1 ]; then no "$1: validator exit $rc (expected 1)"; return; fi
  case "$verdict" in *"$3"*) ok "$1 is refused" ;; *) no "$1 passed" ;; esac
}
mut "an unknown area" '$1 ~ /^ISS-/ && !d {$4="compilr"; d=1} {print}' "area 'compilr'"
mut "an unknown status" '$1 ~ /^ISS-/ && !d {$9="wontfix"; d=1} {print}' "status 'wontfix'"
mut "an empty closure condition" '$1 ~ /^ISS-/ && !d {$8=""; d=1} {print}' "non-empty"
mut "a repeated id" '{print} $1 ~ /^ISS-/ && !d {print; d=1}' "not greater than the previous id"

echo
echo "check_issues: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
