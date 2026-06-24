#!/usr/bin/env bash
# Interpreter-vs-compiled differential gate (ROADMAP Phase 4 #44f / precursor to
# #18). Runs every tests/codegen/*.con program through BOTH the source-level
# interpreter (`--interp`) and the compiled binary, and asserts they agree.
#
# This is the mechanized successor to the manual codegen sweep: the interpreter
# is an independent oracle, so a divergence means one side miscompiles/
# misinterprets — exactly the class that hid C4/C5/C6 from the
# compile/reject-only suite.
#
# Three honest exclusion lists (never silent):
#   EXPECTED_DIVERGE — the interpreter uses unbounded Int; the compiled binary
#     uses fixed-width values. CASTS (`as`) truncate/sign-extend, so they
#     legitimately differ (interp keeps the unbounded value). (The deeper #18
#     fix gives the interpreter a fixed-width mode so these agree; until then
#     they are documented.)
#   EXPECTED_TRAP — checked arithmetic (ROADMAP #10): overflow is a defined
#     abort on BOTH sides, so neither produces a value. interp must exit nonzero
#     AND the compiled binary must abort. (Pre-#10 these wrapped and were listed
#     EXPECTED_DIVERGE; now they agree by both trapping.)
#   INTERP_UNSUPPORTED — shapes the interpreter cannot run yet (e.g. function
#     pointers). Tracked so coverage gaps are visible, not hidden.
# The gate FAILS on any divergence or unsupported case OUTSIDE these lists.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

# unbounded-Int vs fixed-width casts: legitimately differ (no trap).
EXPECTED_DIVERGE=" cast_signext cast_truncate "
# checked-arithmetic overflow: both sides trap (no value on either side).
EXPECTED_TRAP=" i32_wrap i64_mul_overflow u32_wrap "
# interpreter cannot execute these shapes yet.
INTERP_UNSUPPORTED=" fn_pointer "

in_list() { case "$2" in *" $1 "*) return 0;; *) return 1;; esac }

echo "=== interpreter vs compiled agreement over tests/codegen/ ==="
for f in tests/codegen/*.con; do
  name="$(basename "$f" .con)"
  ci="$("$COMPILER" "$f" --interp 2>/dev/null | tail -1)"
  "$COMPILER" "$f" --interp >/dev/null 2>&1; ie=$?
  if "$COMPILER" "$f" -o "$TMP/d" >/dev/null 2>&1; then
    cc="$("$TMP/d" 2>/dev/null)"; ce=$?; [ -z "$cc" ] && cc="$ce"
  else cc="CFAIL"; ce=99; fi
  if in_list "$name" "$EXPECTED_TRAP"; then
    if [ "$ie" -ne 0 ] && [ "$ce" -ne 0 ]; then
      ok "$name traps on both sides as documented (interp exit=$ie, compiled exit=$ce — checked-arith overflow)"
    else
      no "$name listed EXPECTED_TRAP but interp exit=$ie compiled exit=$ce — overflow no longer traps on both sides; investigate"
    fi
    continue
  fi
  if in_list "$name" "$INTERP_UNSUPPORTED"; then
    [ -z "$ci" ] && echo "  skip $name (interp-unsupported, documented)" \
      || no "$name listed INTERP_UNSUPPORTED but the interpreter ran it ('$ci') — remove from the list"
    continue
  fi
  if in_list "$name" "$EXPECTED_DIVERGE"; then
    if [ -n "$ci" ] && [ -n "$cc" ] && [ "$ci" != "$cc" ]; then
      ok "$name diverges as documented (interp=$ci compiled=$cc; unbounded vs fixed-width)"
    else
      no "$name listed EXPECTED_DIVERGE but interp=$ci compiled=$cc — no longer the documented divergence; remove from the list"
    fi
    continue
  fi
  if [ -z "$ci" ]; then
    no "$name: interpreter produced nothing (new unsupported shape?) compiled=$cc"
  elif [ "$ci" = "$cc" ]; then
    ok "$name: interp == compiled ($ci)"
  else
    no "$name: DIVERGENCE interp='$ci' compiled='$cc' — one side is wrong"
  fi
done

echo ""
echo "CODEGEN-DIFFERENTIAL: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
