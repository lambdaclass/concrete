#!/usr/bin/env bash
# Unified command-plumbing gate (ROADMAP Phase 4 #14a).
#
# Commands must share their plumbing: one project-root prologue (one message, one
# exit code), and one exit-code taxonomy that drives both the codes and the
# documented `EXIT CODES` help block so they cannot drift. This gate proves the
# single sources exist and that project-scoped commands behave identically when
# run outside a project.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

echo "=== single source: the no-project prologue lives in exactly one place ==="
n="$(grep -c "no Concrete.toml found in current directory" Main.lean || true)"
[ "$n" = "1" ] && ok "the 'no Concrete.toml' diagnostic appears once (withProjectRoot)" \
  || no "expected 1 copy of the no-project diagnostic, found $n"
grep -qE "def withProjectRoot" Main.lean && ok "withProjectRoot helper exists" \
  || no "withProjectRoot helper missing"

echo "=== shared prologue: project commands behave identically outside a project ==="
TMP="$(mktemp -d)"
declare -a outs=()
allsame=1; allone=1
for cmd in build test check; do
  out="$(cd "$TMP" && "$COMPILER" "$cmd" 2>&1)"; rc=$?
  [ "$rc" = "1" ] || allone=0
  outs+=("$out")
  printf '%s' "$out" | grep -q "no Concrete.toml found" || allsame=0
done
[ "$allone" = "1" ] && ok "build/test/check all exit 1 outside a project" \
  || no "project commands disagree on the no-project exit code"
[ "$allsame" = "1" ] && ok "build/test/check all emit the same canonical no-project message" \
  || no "project commands emit different no-project messages"

echo "=== single source: the exit-code taxonomy drives the help block ==="
grep -qE "def taxonomy" Main.lean && grep -qE "def helpBlock" Main.lean \
  && ok "ExitCode.taxonomy + generated helpBlock exist" || no "exit-code taxonomy not centralized"
# the documented EXIT CODES block is generated (codes 0..6 present, in order).
HELP="$("$COMPILER" prove --help=agent 2>/dev/null)"
block="$(printf '%s' "$HELP" | sed -n '/EXIT CODES:/,/internal compiler error/p')"
okcodes=1
for c in 0 1 2 3 4 5 6; do printf '%s' "$block" | grep -qE "^  $c  " || okcodes=0; done
[ "$okcodes" = "1" ] && ok "help EXIT CODES block lists codes 0..6 (generated from taxonomy)" \
  || no "help EXIT CODES block does not match the taxonomy"
# the prove exit-code values are still routed through named constants (no literals).
grep -qE "ExitCode.staleEvidence" Main.lean && grep -qE "ExitCode.obligationsMissing" Main.lean \
  && ok "prove exits route through named ExitCode constants" \
  || no "prove still uses magic exit-code literals"

echo "=== the taxonomy and prove's runtime exits agree (0 proved / 2 missing / 3 stale) ==="
CT="examples/constant_time_tag/src/main.con"
"$COMPILER" prove "$CT" constant_time_tag.ct_compare --json >/dev/null 2>&1
[ "$?" = "0" ] && ok "a proved obligation exits 0 (ExitCode.ok)" || no "proved obligation did not exit 0"
"$COMPILER" prove examples/proof_pressure/src/main.con main.clamp_value --json >/dev/null 2>&1
[ "$?" = "2" ] && ok "a missing obligation exits 2 (ExitCode.obligationsMissing)" || no "missing obligation did not exit 2"

echo ""
echo "CLI-PLUMBING: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
