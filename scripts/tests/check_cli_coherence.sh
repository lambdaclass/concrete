#!/usr/bin/env bash
# Phase 6E #6: CLI coherence gate — the exit criterion before Phase 7 stdlib.
#
# Locks: (a) help works from ANY directory (no project), never throws;
# (b) daily aliases are byte-identical to their legacy flag spellings;
# (c) legacy spellings keep working; (d) invalid invocations produce a
# structured usage/error, never an uncaught exception; (e) the help page
# names every public command group.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first" >&2; exit 2; }
TMP=$(mktemp -d); trap 'rm -rf "$TMP"' EXIT
cd "$TMP"   # deliberately: no Concrete.toml, no project, not the repo
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

echo "=== (a) help is context-free and never throws ==="
for inv in "--help" "help" "-h" "test --help" "fmt --help" "prove --help" "reduce --help" "trace --help"; do
  out="$("$C" $inv 2>&1)"; rc=$?
  if [ $rc -eq 0 ] && ! grep -qi <<<"$out" "uncaught exception"; then
    ok "concrete $inv (exit 0, no exception)"
  else no "concrete $inv (rc=$rc)"; fi
done

echo "=== (b) aliases byte-identical to legacy spellings ==="
printf 'mod m { fn main() -> Int { return 7; } }' > p.con
if diff <("$C" report caps p.con 2>&1) <("$C" p.con --report caps 2>&1) >/dev/null; then
  ok "report <kind> <file> == <file> --report <kind>"; else no "report alias diverges"; fi
if diff <("$C" trace p.con --json 2>&1) <("$C" p.con --trace-pipeline 2>&1) >/dev/null; then
  ok "trace <file> --json == <file> --trace-pipeline"; else no "trace alias diverges"; fi

echo "=== (c) legacy spellings still work ==="
"$C" p.con --report caps >/dev/null 2>&1 && ok "legacy <file> --report" || no "legacy --report broke"
"$C" p.con --emit-core   >/dev/null 2>&1 && ok "legacy <file> --emit-core" || no "legacy --emit-core broke"
"$C" p.con --trace-pipeline >/dev/null 2>&1 && ok "legacy --trace-pipeline" || no "legacy --trace-pipeline broke"

echo "=== (d) invalid invocations: structured error, no exception ==="
for inv in "nonexistent-cmd" "report" "trace" "p.con --report bogus-kind"; do
  out="$("$C" $inv 2>&1 || true)"
  if grep -qi <<<"$out" "uncaught exception"; then no "concrete $inv threw"
  else ok "concrete $inv -> structured error"; fi
done

echo "=== (e) help names every public command group ==="
H="$("$C" --help 2>&1)"
for grp in DAILY "REPORTS & EVIDENCE" DEBUGGING "INTERNALS" build run test fmt prove reduce debug-bundle; do
  grep -q <<<"$H" "$grp" && ok "help mentions $grp" || no "help missing $grp"
done

echo
echo "CLI-COHERENCE: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
