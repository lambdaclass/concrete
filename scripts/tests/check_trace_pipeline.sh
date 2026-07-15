#!/usr/bin/env bash
export CONCRETE_ECHO_RESULT=1  # MAIN_EXIT_MODEL stage 1: legacy echoed-result mode until fixtures migrate (stage 2 deletes this)
# Phase 6C #3 gate: `concrete <file> --trace-pipeline` per-stage trace.
#
# The trace runs the pipeline stage by stage and, for a rejected program, names
# the FIRST stage that rejected the relevant fact; for an accepted program every
# stage is `ok`. This gate pins: a clean program reports accepted with all stages
# ok; failing programs at DIFFERENT stages report the correct firstFailingStage +
# diagnostic code; and the trace is stable-schema with no private absolute paths.

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
has(){ printf '%s' "$1" | grep -q "$2"; }

# names_stage <label> <name> <outcome> <firstFailingStage-json> [code]
names_stage(){ local label="$1" T="$("$COMPILER" "$TMPDIR/$2.con" --trace-pipeline 2>&1)"
  local want_outcome="$3" want_fail="$4" want_code="${5:-}"
  local okk=1
  has "$T" "\"outcome\":\"$want_outcome\"" || okk=0
  has "$T" "\"firstFailingStage\":$want_fail" || okk=0
  [ -n "$want_code" ] && { has "$T" "\"code\":\"$want_code\"" || okk=0; }
  if [ "$okk" -eq 1 ]; then ok "$label"
  else no "$label — got: $(printf '%s' "$T" | grep -oE '"outcome":"[^"]*","firstFailingStage":[^,]*' )"; fi; }

echo "=== accepted program: all stages ok ==="
emit clean 'mod m { fn main() -> Int { return 2 + 3; } }'
names_stage "clean program is accepted, no failing stage" clean "accepted" "null"
CLEAN="$("$COMPILER" "$TMPDIR/clean.con" --trace-pipeline 2>&1)"
for st in parse resolve check elaborate coreCheck monomorphize lower; do
  if has "$CLEAN" "\"stage\":\"$st\",\"status\":\"ok\""; then ok "clean: stage $st ok"; else no "clean: stage $st missing/not ok"; fi
done

echo "=== rejected programs: trace names the FIRST failing stage ==="
# Parse error (`fn name!` is dead grammar) → fails at parse.
emit eparse 'mod m { fn foo!() -> Int { return 1; } fn main() -> Int { return 0; } }'
names_stage "parse error → firstFailingStage=parse" eparse "rejected" "\"parse\""

# Name resolution error (call to an undeclared function) → fails at resolve.
emit eresolve 'mod m { fn main() -> Int { return nonexistent_fn(1); } }'
names_stage "unknown function → firstFailingStage=resolve" eresolve "rejected" "\"resolve\""

# Type/ownership error (use-after-move) → fails at check, code E0205.
emit echeck 'mod m { struct Box { v: i32 } fn sink(b: Box) -> i32 { let Box { v } = b; return v; } fn main() -> Int { let b: Box = Box { v: 3 }; let x: i32 = sink(b); let y: i32 = sink(b); return (x + y) as Int; } }'
names_stage "use-after-move → firstFailingStage=check (E0205)" echeck "rejected" "\"check\"" "E0205"

echo "=== schema stability + no private paths ==="
if has "$CLEAN" '"schema":"concrete.pipeline.trace.v1"'; then ok "stable schema tag"; else no "missing schema tag"; fi
if printf '%s' "$CLEAN" | grep -qE '/(Users|home|private|tmp|root)/'; then
  no "absolute path leaked into trace"
else ok "no absolute path in trace"; fi

echo
echo "check_trace_pipeline: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
