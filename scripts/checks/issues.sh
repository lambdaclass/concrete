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
problems(){ python3 -I -c '
import re, sys
errors = []
def problem(message): errors.append(message)
HDR = ["id", "title", "date", "area", "evidence", "owner", "severity", "closure_condition", "status"]
AREA = {"spec", "proof", "compiler", "launcher", "ci", "migration"}
SEV = {"high", "medium", "low"}; STATUS = {"open", "closed"}
lines = [l.rstrip("\n") for l in open(sys.argv[1]) if not l.startswith("#") and l.strip()]
if not lines or lines[0].split("\t") != HDR:
    problem("header is not: " + "\t".join(HDR)); print("\n".join(errors)); sys.exit(1)
last = 0
for n, l in enumerate(lines[1:], 2):
    f = l.split("\t")
    if len(f) != len(HDR) or any(not x.strip() for x in f):
        problem(f"row {n}: needs {len(HDR)} non-empty tab-separated fields"); continue
    m = re.fullmatch(r"ISS-(\d{4})", f[0])
    if not m: problem(f"row {n}: id {f[0]!r} is not ISS-NNNN")
    elif int(m.group(1)) <= last: problem(f"row {n}: id {f[0]} is not greater than the previous id")
    else: last = int(m.group(1))
    if not re.fullmatch(r"\d{4}-\d{2}-\d{2}", f[2]): problem(f"row {n}: date {f[2]!r} is not YYYY-MM-DD")
    if f[3] not in AREA: problem(f"row {n}: area {f[3]!r} is not one of {sorted(AREA)}")
    if f[6] not in SEV: problem(f"row {n}: severity {f[6]!r} is not one of {sorted(SEV)}")
    if f[8] not in STATUS: problem(f"row {n}: status {f[8]!r} is not one of {sorted(STATUS)}")

print("\n".join(errors), end="\n" if errors else "")
sys.exit(1 if errors else 0)
' "$1"; }

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
