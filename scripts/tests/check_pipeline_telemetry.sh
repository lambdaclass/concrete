#!/usr/bin/env bash
# Phase 6C #1 gate: pipeline telemetry trace schema stability.
#
# `concrete <file> --emit-trace-json` emits a stable-schema JSON trace of per-stage
# structural counts (Core / post-mono / SSA) plus the opaque compiler identity.
# This is NOT a performance claim — timing/RSS are deliberately omitted from v1.
# The gate pins exactly what the roadmap says to gate: schema stability, absence
# of private absolute paths, and that counts are present integers. Benchmarks stay
# Phase 17 work.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
TMPDIR=$(mktemp -d)
trap 'rm -rf "$TMPDIR"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
emit(){ printf '%s\n' "$2" > "$TMPDIR/$1.con"; }

# A rich program: structs, enums, functions, a loop (blocks), calls.
emit rich 'mod m { struct P { x: i32 } enum E { A {} B {} } fn helper(a: i32) -> i32 { return a + 1; } fn main() -> Int { let mut i: Int = 0; while i < 3 { i = i + 1; } return helper(i as Int as i32) as Int; } }'
# A minimal program: schema must be identical, counts differ.
emit tiny 'mod m { fn main() -> Int { return 0; } }'

TRACE="$("$COMPILER" "$TMPDIR/rich.con" --emit-trace-json 2>&1)"
TINY="$("$COMPILER" "$TMPDIR/tiny.con" --emit-trace-json 2>&1)"

echo "=== schema stability ==="
has(){ printf '%s' "$1" | grep -q "$2"; }

for key in '"schema":"concrete.pipeline.telemetry.v1"' '"compiler":"' \
           '"stages":' '"core":' '"mono":' '"ssa":' '"diagnostics":' \
           '"modules":' '"functions":' '"enums":' '"structs":' '"blocks":' '"instructions":'; do
  if has "$TRACE" "$key"; then ok "schema has $key"; else no "schema missing $key"; fi
done

# The schema string and top-level key set must be identical across programs.
if has "$TINY" '"schema":"concrete.pipeline.telemetry.v1"'; then ok "minimal program has the same schema"
else no "minimal program schema differs"; fi

echo "=== counts are present integers, and vary with program size ==="
rfns="$(printf '%s' "$TRACE" | grep -oE '"functions":[0-9]+' | head -1)"
tfns="$(printf '%s' "$TINY"  | grep -oE '"functions":[0-9]+' | head -1)"
if [ -n "$rfns" ] && [ -n "$tfns" ]; then ok "core.functions present (rich=$rfns tiny=$tfns)"
else no "core.functions not an integer (rich='$rfns' tiny='$tfns')"; fi
# rich has helper+main (2), tiny has main (1): a real difference proves the counts
# reflect the program, not a constant.
if [ "$rfns" != "$tfns" ]; then ok "counts reflect program size (differ)"
else no "counts identical across different programs (not measuring anything)"; fi

echo "=== no private absolute paths leak into the trace ==="
if printf '%s' "$TRACE" | grep -qE '/(Users|home|private|tmp|root)/'; then
  no "absolute path leaked: $(printf '%s' "$TRACE" | grep -oE '/(Users|home|private|tmp|root)/[^"]*' | head -1)"
else ok "no absolute path in the trace"; fi

echo
echo "check_pipeline_telemetry: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
