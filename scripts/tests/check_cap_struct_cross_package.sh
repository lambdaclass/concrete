#!/usr/bin/env bash
# R-0484: A STRUCT'S CAPABILITY PARAMETERS HOLD ACROSS A PACKAGE BOUNDARY.
#
# Bug 071 showed a dependency's declared capabilities being ignored by its consumers.
# This gate asserts the same does not happen to capability parameters on structs:
# `sinklib` declares `Sink<cap C>`, a capability-generic helper and a method on
# `impl<cap C>`; a consumer package imports them.
#
# The specific code is asserted, not merely failure: if capability arguments were not
# normalized after the package merge, `Sink<Console>` in a consumer would be read as a
# TYPE argument and rejected with E0114 — a refusal, but for the wrong reason, and one
# that would hide the authority check this gate exists to see.
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
CC="$ROOT_DIR/.lake/build/bin/concrete"
FIX="$ROOT_DIR/tests/regressions/cap_struct_cross_package"

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

[ -x "$CC" ] || { echo "FATAL: compiler not built at $CC" >&2; exit 2; }
if command -v timeout >/dev/null 2>&1; then TO="timeout 300"; else TO=""; echo "  warn 'timeout' not found — running without a hang watchdog"; fi
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

echo "=== an imported Sink<cap C> works across the boundary (positive control) ==="
if (cd "$FIX/use_ok" && $TO "$CC" build . -o "$TMP/use_ok" >"$TMP/use_ok.log" 2>&1); then
  "$TMP/use_ok"; rc=$?
  [ "$rc" -eq 42 ] && ok "use_ok builds and returns 42 (helper + method across the boundary)" \
                   || no "use_ok returned $rc, expected 42"
else
  no "use_ok does not build"; sed 's/^/       /' "$TMP/use_ok.log" | head -6
fi

echo "=== authority does not leak across the boundary ==="
for case in leak_refused method_leak_refused; do
  out="$(cd "$FIX/$case" && $TO "$CC" build . -o "$TMP/$case" 2>&1)"
  if printf '%s' "$out" | grep -q 'E0240'; then
    ok "$case is refused with E0240 (missing Console)"
  elif printf '%s' "$out" | grep -q 'E0114'; then
    no "$case is refused with E0114 — Sink<Console> was read as a TYPE: capability arguments were not normalized after the package merge"
  else
    no "$case was not refused with E0240: $(printf '%s' "$out" | head -2 | tr '\n' ' ' | cut -c1-200)"
  fi
done

echo
echo "CAP-STRUCT-CROSS-PACKAGE: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
