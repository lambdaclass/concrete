#!/usr/bin/env bash
# Diagnostics-quality gate (ROADMAP Phase 5 #4).
#
# #4's bar is "every diagnostic has a source span, reason, and next action." The
# SPAN is the floor — a location-less error is the worst failure — so this gate
# asserts that representative diagnostics from each pass (parser, resolver, type,
# linearity, capability) carry a source span in --diagnostics-json. It also pins
# the capability diagnostic as the exemplar that already has reason + next-action
# (hint), the target shape the other passes are being enriched toward.
#
# NOTE: reason/next-action are NOT yet populated on most codes (parser/resolver/
# type carry only a span today) — that enrichment is the remaining #4 content
# work, tracked in ROADMAP #4. This gate locks the span floor (and the E0208
# linearity fix that gave it a span) so it cannot regress.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
F="$TMP/m.con"

# field(code-label, source, jq-field): prints Y/- for whether diagnostics[0].<field> is set
diag_field(){ printf '%s' "$2" > "$F"
  "$COMPILER" "$F" --diagnostics-json 2>/dev/null | python3 -c "
import json,sys
try: d=json.load(sys.stdin)
except: print('ERR'); sys.exit()
ds=d.get('diagnostics',[])
if not ds: print('NONE'); sys.exit()
x=ds[0]; f='$3'
if f=='span':
    sp=x.get('span') or {}; print('Y' if sp.get('line',0) else '-')
else:
    v=x.get(f); print('Y' if v not in (None,'',[],{}) else '-')
"
}

echo "=== every probed pass's diagnostic carries a source span ==="
declare -A SRC=(
  [parser]='mod m { fn f() -> i64 { return 1 } }'
  [resolver]='mod m { fn f() -> i64 { return zzz; } }'
  [type]='mod m { fn f() -> i64 { return true; } }'
  [linearity]='mod m { struct S{x:i64} fn f() { let s:S=S{x:1}; let a=s; let b=s; } }'
  [capability]='mod m { fn f() { println("x"); } }'
)
for pass in parser resolver type linearity capability; do
  [ "$(diag_field "$pass" "${SRC[$pass]}" span)" = "Y" ] \
    && ok "$pass diagnostic has a source span" \
    || no "$pass diagnostic is location-less (no span)"
done

echo "=== capability diagnostic is the reason+next-action exemplar ==="
r="$(diag_field cap "${SRC[capability]}" reason)"
h="$(diag_field cap "${SRC[capability]}" hint)"
{ [ "$r" = "Y" ] && [ "$h" = "Y" ]; } \
  && ok "capability diagnostic carries reason + next-action (hint)" \
  || no "capability diagnostic lost its reason/next-action (reason=$r hint=$h)"

echo ""
echo "DIAGNOSTICS-QUALITY: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
