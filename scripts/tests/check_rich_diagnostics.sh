#!/usr/bin/env bash
# Rich-diagnostics gate (ROADMAP Phase 4 #4 / #11).
#
# Diagnostics are structured records rendered to BOTH human text and JSON from the
# SAME record (`Diagnostic.render` and `Diagnostic.toJson`), so the two outputs
# cannot drift. This gate asserts, per diagnostic family, that the human and JSON
# renderings agree on code / severity / pass / span, then pins rich fields as
# producers opt in.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# check_family <label> <fixture> <expected-code> <expected-pass>: the human and
# JSON renderings of the fixture's first diagnostic must agree on code / severity /
# pass / span line — proving both came from the same structured record.
check_family(){ local label="$1" F="$2" wantCode="$3" wantPass="$4"
  local JSON HUMAN jcode jsev jpass jline
  JSON="$("$COMPILER" "$F" --diagnostics-json 2>/dev/null)"
  HUMAN="$("$COMPILER" "$F" --report caps 2>&1)"
  # span may be null (function-level diagnostics, e.g. capability errors).
  read -r jcode jsev jpass jline < <(printf '%s' "$JSON" | python3 -c "
import json,sys
d=json.load(sys.stdin); x=d['diagnostics'][0]
print(x['code'], x['severity'], x['pass'], (x['span'] or {}).get('line',''))" 2>/dev/null)
  local spanOk=1
  if [ -n "$jline" ]; then printf '%s' "$HUMAN" | grep -qE ":${jline}:" || spanOk=0; fi
  if [ "$jcode" = "$wantCode" ] && [ "$jpass" = "$wantPass" ] && [ "$jsev" = "error" ] \
     && [ "$spanOk" = "1" ] \
     && printf '%s' "$HUMAN" | grep -qF "($jcode)" \
     && printf '%s' "$HUMAN" | grep -qE "${jsev}\[${jpass}\]"; then
    ok "$label: human and JSON agree ($jcode/$jsev/$jpass${jline:+ @ line $jline})"
  else
    no "$label: human/JSON disagree (json=$jcode/$jsev/$jpass@${jline:-none} want $wantCode/$wantPass)"
  fi; }

echo "=== diagnostics render human + JSON from the SAME record, per family ==="
check_family "parser"   "examples/diagnostics_rich/parser_error.con" "E0001" "parse"
check_family "resolver" "examples/diagnostics_rich/unknown_name.con" "E0101" "resolve"
check_family "type"     "examples/diagnostics_rich/type_mismatch.con" "E0220" "check"
check_family "ownership" "examples/diagnostics_rich/use_after_move.con" "E0205" "check"
check_family "capability" "examples/diagnostics_rich/missing_capability.con" "E0520" "core-check"

echo "=== rich fields: expected-vs-actual agree in human and JSON (type family) ==="
TM="examples/diagnostics_rich/type_mismatch.con"
TMJ="$("$COMPILER" "$TM" --diagnostics-json 2>/dev/null)"
TMH="$("$COMPILER" "$TM" --report caps 2>&1)"
read -r jexp jact < <(printf '%s' "$TMJ" | python3 -c "import json,sys;x=json.load(sys.stdin)['diagnostics'][0];print(x['expected'],x['actual'])" 2>/dev/null)
if [ "$jexp" = "i32" ] && [ "$jact" = "bool" ] \
   && printf '%s' "$TMH" | grep -qE "expected: i32" \
   && printf '%s' "$TMH" | grep -qE "found: +bool"; then
  ok "type mismatch: expected/actual structured + shown in human and JSON (i32 vs bool)"
else
  no "type mismatch expected/actual mismatch (json exp=$jexp act=$jact)"
fi

echo "=== rich fields: related spans agree in human and JSON (ownership family) ==="
UAM="examples/diagnostics_rich/use_after_move.con"
UAMJ="$("$COMPILER" "$UAM" --diagnostics-json 2>/dev/null)"
UAMH="$("$COMPILER" "$UAM" --report caps 2>&1)"
read -r relCount relLine relNote < <(printf '%s' "$UAMJ" | python3 -c "
import json,sys
x=json.load(sys.stdin)['diagnostics'][0]
r=x['related']
if r:
  print(len(r), r[0]['span']['line'], r[0]['note'])
else:
  print(0, '', '')" 2>/dev/null)
if [ "$relCount" = "1" ] && [ "$relLine" = "9" ] \
   && printf '%s' "$relNote" | grep -qF "moved here" \
   && printf '%s' "$UAMH" | grep -qF "related (9:" \
   && printf '%s' "$UAMH" | grep -qF "moved here"; then
  ok "use-after-move: related move site structured + shown in human and JSON"
else
  no "use-after-move related span mismatch (json count=$relCount line=$relLine note=$relNote)"
fi

echo "=== rich fields: evidence + reason agree in human and JSON (capability family) ==="
MC="examples/diagnostics_rich/missing_capability.con"
MCJ="$("$COMPILER" "$MC" --diagnostics-json 2>/dev/null)"
MCH="$("$COMPILER" "$MC" --report caps 2>&1)"
read -r jcode jreq jhas jreason < <(printf '%s' "$MCJ" | python3 -c "
import json,sys
x=json.load(sys.stdin)['diagnostics'][0]
ev={e['key']:e['value'] for e in x['evidence']}
print(x['code'], ev.get('requires',''), ev.get('caller_has',''), 'yes' if x['reason'] else 'no')" 2>/dev/null)
if [ "$jcode" = "E0520" ] && [ "$jreq" = "Alloc" ] && [ "$jhas" = "(none)" ] && [ "$jreason" = "yes" ] \
   && printf '%s' "$MCH" | grep -qE "requires: Alloc" \
   && printf '%s' "$MCH" | grep -qE "caller_has: \(none\)" \
   && printf '%s' "$MCH" | grep -qE "reason: capabilities are part of"; then
  ok "capability: evidence (requires/caller_has) + reason structured + shown in human and JSON"
else
  no "capability evidence/reason mismatch (json code=$jcode req=$jreq has=$jhas reason=$jreason)"
fi

echo "=== JSON envelope is well-formed and versioned ==="
ENV_J="$("$COMPILER" examples/diagnostics_rich/parser_error.con --diagnostics-json 2>/dev/null)"
printf '%s' "$ENV_J" | python3 -c "import json,sys;d=json.load(sys.stdin);sys.exit(0 if d['schema_kind']=='diagnostics' and d['schema_version']>=1 and d['count']==1 else 1)" \
  && ok "diagnostics JSON has schema_kind/version/count" || no "diagnostics JSON envelope malformed"

echo "=== a well-formed file yields zero diagnostics (no false errors) ==="
CLEAN_J="$("$COMPILER" examples/hmac_sha256/src/main.con --diagnostics-json 2>/dev/null)"
printf '%s' "$CLEAN_J" | python3 -c "import json,sys;d=json.load(sys.stdin);sys.exit(0 if d['count']==0 else 1)" \
  && ok "clean file → empty diagnostics" || no "clean file produced diagnostics"

echo ""
echo "RICH-DIAGNOSTICS: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
