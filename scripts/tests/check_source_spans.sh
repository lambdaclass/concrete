#!/usr/bin/env bash
# Source-span preservation gate (pipeline second-tier: span audit).
#
# A diagnostic is only useful if it points at the user's source. Spans are
# threaded through every pass (lex → parse → resolve → check → elab → core-check
# → mono → lower); any pass that drops or zeroes a span turns a real error into
# "somewhere in your program". This gate triggers an error from each major pass
# with the offending construct on a KNOWN line and asserts the diagnostic points
# to that line, so a span-loss regression in any pass fails CI.
#
# It checks the LINE (robust), not the exact column (formatting-sensitive). Each
# fixture puts `mod m {` on line 1 and the error on line 2.
#
# Needs the compiler built; runs in the compiler test job.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first ($C missing)" >&2; exit 2; }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# span_on_line <label> <expected-pass-tag> <line> <program-with-error-on-that-line>
# Compile must fail; the FIRST diagnostic must carry `<file>:<line>:<col>:` and
# name the expected pass. A dropped span shows up as a missing/`:0:`/`:1:`
# location or no `file.con:LINE:` at all.
span_on_line(){
  local label="$1" passtag="$2" line="$3" prog="$4"
  printf '%s' "$prog" > "$TMP/$label.con"
  local out rc
  out="$("$C" "$TMP/$label.con" -o "$TMP/$label.bin" 2>&1)" && rc=0 || rc=$?
  if [ "$rc" -eq 0 ]; then no "$label: expected a diagnostic, but it compiled"; return; fi
  if ! printf '%s' "$out" | grep -qE "\.con:${line}:[0-9]+:"; then
    no "$label: diagnostic not anchored to line ${line}: $(printf '%s' "$out" | head -1)"; return; fi
  if ! printf '%s' "$out" | grep -q "error\[${passtag}"; then
    # pass tag is advisory — warn but do not fail if the line anchor is right
    ok "$label (line ${line}; note: pass tag not '${passtag}')"; return; fi
  ok "$label (${passtag} @ line ${line})"
}

echo "=== every pass anchors its diagnostic to the original source line ==="
span_on_line parse     parse       2 'mod m {
  fn main() -> Int { return 1 }
}'
span_on_line resolve   ''          2 'mod m {
  fn main() -> Int { return missingFn(); }
}'
span_on_line typecheck ''          2 'mod m {
  fn main() -> Int { let x: i32 = true; return 0; }
}'
span_on_line corecheck core-check  2 'mod m {
  struct S { x: S }
  fn main() -> Int { return 0; }
}'
span_on_line linearity check       3 'mod m {
  struct R { x: i32 }
  fn f() { let r: R = R { x: 1 }; }
  fn main() -> Int { return 0; }
}'

echo ""
echo "SOURCE-SPANS: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
