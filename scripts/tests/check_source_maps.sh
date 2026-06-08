#!/usr/bin/env bash
# Source-map preservation gate (ROADMAP Phase 4 #13a).
#
# Source locations must survive lowering boundaries. Before #13, the AST→Core
# (elaboration) boundary dropped all spans, so every Core-level (core-check)
# diagnostic was location-less. Now `CFnDef.declSpan` carries the function
# declaration span across that boundary and core-check diagnostics point at the
# offending function in source. This gate proves the span survives and is the
# RIGHT line (the function's declaration), and pins the plumbing.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

FIX="examples/source_maps/capability_at_line.con"
# the line where the offending function is declared (computed, not hard-coded).
FNLINE="$(grep -n "fn allocates" "$FIX" | head -1 | cut -d: -f1)"

echo "=== source location survives the AST→Core boundary into core-check diagnostics ==="
J="$("$COMPILER" "$FIX" --diagnostics-json 2>/dev/null)"
read -r code pass line < <(printf '%s' "$J" | python3 -c "
import json,sys
x=json.load(sys.stdin)['diagnostics'][0]
sp=x['span'] or {}
print(x['code'], x['pass'], sp.get('line','null'))" 2>/dev/null)
[ "$code" = "E0520" ] && [ "$pass" = "core-check" ] && ok "the capability violation is a core-check diagnostic (E0520)" \
  || no "expected E0520/core-check, got $code/$pass"
[ "$line" != "null" ] && [ -n "$line" ] && ok "the core-check diagnostic carries a span (was null before #13)" \
  || no "core-check diagnostic still location-less (span=$line)"
[ "$line" = "$FNLINE" ] && ok "the span points at the function declaration (line $FNLINE)" \
  || no "span line $line does not match the function declaration line $FNLINE"

echo "=== the human render shows the file:line for the core-check diagnostic ==="
H="$("$COMPILER" "$FIX" --report caps 2>&1)"
printf '%s' "$H" | grep -qE "capability_at_line\.con:${FNLINE}:" \
  && ok "human output prefixes the diagnostic with file:${FNLINE}" \
  || no "human output missing the source location"

echo "=== plumbing is pinned: declSpan is carried Core-side and populated by Elab ==="
grep -qE "declSpan : Option Span" Concrete/Core.lean \
  && ok "CFnDef carries declSpan" || no "CFnDef.declSpan missing"
grep -qE "declSpan := some f\.span" Concrete/Elab.lean \
  && ok "Elab populates declSpan from the source FnDef span" || no "Elab does not populate declSpan"
grep -qE "currentFnSpan := f\.declSpan" Concrete/CoreCheck.lean \
  && ok "core-check threads declSpan as the diagnostic span" || no "core-check does not use declSpan"

echo "=== obligations from project code cite their source location ==="
# Regression lock: every obligation generated for THIS file's functions must
# carry a real (file, line) — file non-empty and line > 0 — so audit/proof
# artifacts point at source. (Dependency/stdlib-internal obligation locations
# are a separate, deeper item — see ROADMAP #13 — and are not asserted here.)
OBF="examples/source_maps/obligation_located.con"
read -r total located < <("$COMPILER" "$OBF" --report obligation-ledger --json 2>/dev/null | python3 -c "
import json,sys
d=json.load(sys.stdin)
obs=d.get('obligations',[])
loc=[o for o in obs if (o.get('loc') or {}).get('line',0)>0 and (o.get('loc') or {}).get('file','')]
print(len(obs), len(loc))" 2>/dev/null)
[ -n "$total" ] && [ "$total" -ge 1 ] && [ "$total" = "$located" ] \
  && ok "all $total project obligations carry a source (file, line)" \
  || no "project obligations missing source locations ($located of $total located)"

echo "=== source location survives Core→SSA into the emitted backend artifact (#13b) ==="
# declSpan is carried CFnDef → (mono) → SFnDef → SSA dump, so the backend artifact
# names the source line of each function it lowers.
OBLINE="$(grep -n "fn divide" "$OBF" | head -1 | cut -d: -f1)"
SSA="$("$COMPILER" "$OBF" --emit-ssa 2>/dev/null)"
printf '%s' "$SSA" | grep -qE "^; source: divide @ line ${OBLINE}\$" \
  && ok "the SSA dump names divide's source line ($OBLINE)" \
  || no "SSA dump missing source-line provenance for divide"
# pin the Core→SSA plumbing.
grep -qE "declSpan : Option Span" Concrete/SSA.lean \
  && ok "SFnDef carries declSpan" || no "SFnDef.declSpan missing"
grep -qE "declSpan := f\.declSpan" Concrete/Lower.lean \
  && ok "lowerFn carries declSpan Core→SSA" || no "lowerFn drops declSpan"

echo "=== a clean file still yields zero diagnostics (no spurious spans) ==="
"$COMPILER" examples/hmac_sha256/src/main.con --diagnostics-json 2>/dev/null \
  | python3 -c "import json,sys;d=json.load(sys.stdin);sys.exit(0 if d['count']==0 else 1)" \
  && ok "clean file → no diagnostics" || no "clean file produced diagnostics"

echo ""
echo "SOURCE-MAPS: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
