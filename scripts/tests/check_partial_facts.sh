#!/usr/bin/env bash
# Error-tolerant partial-facts gate (ROADMAP Phase 4 #12a).
#
# A failing pass must not erase diagnostics from passes that can still run: a bad
# reference in one function should not hide a type error in another. The tolerant
# driver (`runFrontendDiagnostics`) returns ONLY diagnostics plus a `partial` flag
# and is structurally incapable of producing a `ValidatedCore`, so it can never feed
# codegen / proof / policy as if it were complete. This gate proves the tolerance,
# the cascade suppression, the `partial` labelling, and — critically — the safety
# boundary (codegen still bails and emits nothing on a file the tolerant path
# reports on).

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

FIX="examples/partial_facts/resolve_error_hides_nothing.con"

echo "=== cross-pass tolerance: a resolve error does not erase unrelated diagnostics ==="
J="$("$COMPILER" "$FIX" --diagnostics-json 2>/dev/null)"
read -r hasResolve hasType hasCascade isPartial cnt < <(printf '%s' "$J" | python3 -c "
import json,sys
d=json.load(sys.stdin)
codes=[(x['code'],x['pass']) for x in d['diagnostics']]
print('yes' if ('E0101','resolve') in codes else 'no',
      'yes' if ('E0220','check') in codes else 'no',
      'yes' if any(c=='E0263' for c,_ in codes) else 'no',
      'yes' if d['partial'] else 'no',
      d['count'])" 2>/dev/null)
[ "$hasResolve" = "yes" ] && ok "the resolve error (E0101) is reported" || no "resolve error missing"
[ "$hasType" = "yes" ] && ok "the UNRELATED type error (E0220) survives the resolve failure" \
  || no "type error erased by resolve failure (the bug #12a fixes)"

echo "=== cascade suppression: the check echo of the resolve error is dropped ==="
[ "$hasCascade" = "no" ] && ok "no E0263 echo (check's re-report at the resolve span is suppressed)" \
  || no "cascade E0263 leaked through"
[ "$cnt" = "2" ] && ok "exactly the two root diagnostics, no duplicates (count=2)" \
  || no "unexpected diagnostic count $cnt (want 2)"

echo "=== partial labelling: incomplete runs are flagged, complete runs are not ==="
[ "$isPartial" = "yes" ] && ok "resolve-error run is labelled partial=true" || no "partial flag not set"
CLEAN="$("$COMPILER" examples/hmac_sha256/src/main.con --diagnostics-json 2>/dev/null)"
printf '%s' "$CLEAN" | python3 -c "import json,sys;d=json.load(sys.stdin);sys.exit(0 if d['partial']==False and d['count']==0 else 1)" \
  && ok "a clean file is partial=false with zero diagnostics" || no "clean file mislabelled"

echo "=== capability/core-check diagnostics still available on the tolerant path ==="
CAP="$("$COMPILER" examples/diagnostics_rich/missing_capability.con --diagnostics-json 2>/dev/null)"
printf '%s' "$CAP" | python3 -c "import json,sys;d=json.load(sys.stdin);sys.exit(0 if any(x['code']=='E0520' for x in d['diagnostics']) and d['partial']==False else 1)" \
  && ok "core-check (E0520) diagnostics preserved, partial=false (full chain ran)" \
  || no "capability diagnostics lost on tolerant path"

echo "=== SAFETY: partial facts never feed codegen — build still bails, emits nothing ==="
TMPO="$(mktemp -d)/out.o"
"$COMPILER" "$FIX" -o "$TMPO" --emit-llvm >/dev/null 2>&1; rc=$?
[ "$rc" != "0" ] && ok "codegen on the resolve-error file fails (exit $rc)" || no "codegen did NOT fail (leak!)"
[ ! -f "$TMPO" ] && ok "no object emitted for the failing file" || no "object emitted despite resolve failure (LEAK)"

echo "=== STRUCTURAL: the tolerant driver yields only Diagnostics, never ValidatedCore ==="
# runFrontendDiagnostics returns (Diagnostics × Bool) — it cannot construct the
# proof/codegen artifact. Pin the signature so a refactor cannot widen it.
# the def exists and its declared return type is (Diagnostics × Bool) — the
# signature spans multiple lines, so match the return-type line on its own.
if grep -q "def runFrontendDiagnostics" Concrete/Pipeline.lean \
   && grep -qF ": IO (Diagnostics × Bool)" Concrete/Pipeline.lean; then
  ok "runFrontendDiagnostics : IO (Diagnostics × Bool) (no ValidatedCore)"
else
  no "tolerant driver signature changed — re-audit the safety boundary"
fi
# and it is wired only into the diagnostics surface, not build/run/test.
nuse="$(grep -c "runFrontendDiagnostics" Main.lean || true)"
[ "$nuse" = "1" ] && ok "exactly one consumer in Main (the --diagnostics-json surface)" \
  || no "expected one tolerant-driver consumer, found $nuse"

echo ""
echo "PARTIAL-FACTS: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
