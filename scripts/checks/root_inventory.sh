#!/usr/bin/env bash
# THE ROOT HOLDS ONLY THE DAY-ONE SET (ROADMAP R-0558). Every tracked top-level entry, and every
# first-level entry under docs/ and scripts/, must be listed in scripts/checks/root_inventory.txt,
# and every listed entry must exist. A negative control runs the same judgment on a synthetic
# listing first.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
LIST=scripts/checks/root_inventory.txt
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# judge LIST: reads tracked paths on stdin, prints one problem per line.
judge() {
  python3 -I -c '
import sys
want = {l.strip() for l in open(sys.argv[1]) if l.strip() and not l.startswith("#")}
have = set()
for p in (l.rstrip("\n") for l in sys.stdin):
    if not p: continue
    parts = p.split("/")
    have.add(parts[0])
    if parts[0] in ("docs", "scripts") and len(parts) > 1:
        have.add(parts[0] + "/" + parts[1])
for e in sorted(have - want): print("not in the day-one set: " + e)
for e in sorted(want - have): print("listed but missing: " + e)
' "$1"
}

echo "=== negative controls ==="
TMP="$(mktemp -d)" || exit 2
trap 'rm -rf "$TMP"' EXIT
printf 'a\nb\ndocs\ndocs/x.md\n' > "$TMP/list"
[ -z "$(printf 'a\nb/c\ndocs/x.md\n' | judge "$TMP/list")" ] && ok "control: exactly the listed set passes" || no "control: listed set refused"
[ -n "$(printf 'a\nb/c\ndocs/x.md\nstray\n' | judge "$TMP/list")" ] && ok "control: an unlisted top-level entry is refused" || no "control: stray accepted"
[ -n "$(printf 'a\nb/c\ndocs/x.md\ndocs/y.md\n' | judge "$TMP/list")" ] && ok "control: an unlisted docs/ entry is refused" || no "control: docs stray accepted"
[ -n "$(printf 'a\ndocs/x.md\n' | judge "$TMP/list")" ] && ok "control: a missing listed entry is refused" || no "control: missing accepted"

echo "=== the tracked root against $LIST ==="
[ -f "$LIST" ] || { no "$LIST is missing"; echo "root_inventory: PASS=$PASS FAIL=$FAIL"; exit 1; }
out="$(git ls-files --cached | judge "$LIST")"
[ -z "$out" ] && ok "the root holds exactly the day-one set" || { no "the root differs from the day-one set:"; echo "$out" | sed 's/^/      /'; }
echo "root_inventory: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
