#!/usr/bin/env bash
# 13t gate (the report/assertion tail): the three error buckets of
# docs/ERROR_CONVENTIONS.md are REPORT-VISIBLE on the fixture — one public API
# per bucket, each classification shown by a compiler surface, not prose.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
C=".lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first" >&2; exit 2; }
F="examples/error_conventions/src/main.con"
TMP=$(mktemp -d); trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

echo "=== bucket 1 RECOVERABLE: parse_digit -> Result; ignoring it REJECTS ==="
grep -q "parse_digit(c: i32) -> Result<i32, ParseErr>" "$F" \
  && ok "parse_digit returns Result (recoverable by signature)" || no "fixture signature drifted"
cat > "$TMP/ign.con" <<'CON'
mod m {
    pub enum Copy ParseErr { NotADigit {} }
    fn parse_digit(c: i32) -> Result<i32, ParseErr> {
        if c >= 48 { if c <= 57 { return Result::<i32, ParseErr>::Ok { value: c - 48 }; } }
        return Result::<i32, ParseErr>::Err { error: ParseErr::NotADigit {} };
    }
    fn main() -> Int { parse_digit(53); return 0; }
}
CON
out="$("$C" "$TMP/ign.con" 2>&1 || true)"
printf '%s' "$out" | grep -q "E0286" \
  && ok "ignored Result rejected (E0286 — the compile error IS the surface)" \
  || no "ignored Result not rejected"

echo "=== bucket 2 FATAL: sum_all's traps countable in telemetry ==="
ts="$("$C" "$F" --emit-trace-json 2>/dev/null | grep -oE '"trap_sites":[0-9]+' | grep -oE '[0-9]+')"
[ -n "$ts" ] && [ "$ts" -ge 1 ] && ok "trap_sites visible in telemetry ($ts)" || no "trap_sites missing"

echo "=== bucket 3 POLICY-GATED: read_config's File named by --report caps ==="
"$C" "$F" --report caps 2>&1 | grep -qE "read_config *: *File" \
  && ok "caps report names read_config: File" || no "caps report missing the File classification"

echo
echo "ERROR-CONVENTIONS: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
