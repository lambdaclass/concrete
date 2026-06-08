#!/usr/bin/env bash
# Rich-diagnostics gate (ROADMAP Phase 4 #4 / #11).
#
# Diagnostics are structured records rendered to BOTH human text and JSON from the
# SAME record (`Diagnostic.render` and `Diagnostic.toJson`), so the two outputs
# cannot drift. This gate asserts, per diagnostic family, that the human and JSON
# renderings agree on code / severity / pass / span. Families are added one per
# commit; this commit covers the PARSER family.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

echo "=== parser family: human and JSON agree (same Diagnostic record) ==="
F="examples/diagnostics_rich/parser_error.con"
JSON="$("$COMPILER" "$F" --diagnostics-json 2>/dev/null)"
HUMAN="$("$COMPILER" "$F" --report caps 2>&1)"

# pull the structured fields from the JSON record
read -r jcode jsev jpass jline < <(printf '%s' "$JSON" | python3 -c "
import json,sys
d=json.load(sys.stdin)
x=d['diagnostics'][0]
print(x['code'], x['severity'], x['pass'], x['span']['line'])")

[ "$jcode" = "E0001" ] && ok "JSON: parser error code E0001" || no "JSON parser code=$jcode"
[ "$jsev" = "error" ] && ok "JSON: severity error" || no "JSON severity=$jsev"
[ "$jpass" = "parse" ] && ok "JSON: pass=parse" || no "JSON pass=$jpass"

# the human rendering must carry the SAME code, severity, and span line
printf '%s' "$HUMAN" | grep -qF "($jcode)" && ok "human shows the same code ($jcode)" || no "human missing code $jcode"
printf '%s' "$HUMAN" | grep -qE "${jsev}\[${jpass}\]" && ok "human shows the same severity[pass] ($jsev[$jpass])" || no "human severity/pass mismatch"
printf '%s' "$HUMAN" | grep -qE ":${jline}:" && ok "human shows the same span line ($jline)" || no "human span line mismatch"

echo "=== JSON envelope is well-formed and versioned ==="
printf '%s' "$JSON" | python3 -c "import json,sys;d=json.load(sys.stdin);sys.exit(0 if d['schema_kind']=='diagnostics' and d['schema_version']>=1 and d['count']==1 else 1)" \
  && ok "diagnostics JSON has schema_kind/version/count" || no "diagnostics JSON envelope malformed"

echo "=== a well-formed file yields zero diagnostics (no false errors) ==="
"$COMPILER" examples/hmac_sha256/src/main.con --diagnostics-json 2>/dev/null \
  | python3 -c "import json,sys;d=json.load(sys.stdin);sys.exit(0 if d['count']==0 else 1)" \
  && ok "clean file → empty diagnostics" || no "clean file produced diagnostics"

echo ""
echo "RICH-DIAGNOSTICS: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
