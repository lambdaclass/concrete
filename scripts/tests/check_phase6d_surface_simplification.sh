#!/usr/bin/env bash
# Phase 6D #6: the surface-simplification validation artifact.
#
# Proves the phase reduced syntax WITHOUT weakening Phase 6B semantics:
#   - LL(1) stays green on the simplified grammar
#   - removed forms are rejected with structured migration hints
#     (#1 fn name!, #2 value while…else, #3 postfix p->field, #4 postfix ?)
#   - the migrated statement-form / dot-form fixtures behave identically
#     (loop-control + heap corpus rows)
#   - ownership/linearity and interp-vs-compiled agreement gates stay green
#   - unsafe/raw authority stays visible via capabilities, not syntax

set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
# Per-run scratch (R-0546): a fixed /tmp path collides with a concurrent run of this gate.
GATE_TMP="$(mktemp -d)"; trap 'rm -rf "$GATE_TMP"' EXIT
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/fresh.sh"
require_fresh_binary || exit 1
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
source "$ROOT_DIR/scripts/tests/lib/selfprint.sh"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first" >&2; exit 2; }
PASS=0; FAIL=0
run(){ local l="$1"; shift; if "$@" >"$GATE_TMP/6d_sub.log" 2>&1; then echo "  ok   $l"; PASS=$((PASS+1)); else echo "  FAIL $l"; tail -3 "$GATE_TMP/6d_sub.log" | sed 's/^/       /'; FAIL=$((FAIL+1)); fi; }
rejects(){ local l="$1" f="$2" pat="$3" out
  out="$("$COMPILER" "$f" 2>&1 || true)"
  if grep -q <<<"$out" "$pat"; then echo "  ok   $l"; PASS=$((PASS+1));
  else echo "  FAIL $l"; FAIL=$((FAIL+1)); fi; }

echo "=== grammar stays LL(1) ==="
run "LL(1) checker" bash scripts/tests/check_grammar_ll1.sh

echo "=== removed forms reject with migration hints ==="
rejects "#1 fn name! is dead grammar"        tests/programs/error_fn_bang_removed.con    "E0001"
rejects "#2 value while…else removed + hint" tests/programs/error_while_expr_removed.con "while is a statement, not an expression"
rejects "#3 postfix -> removed + hint"       tests/programs/error_arrow_not_heap.con     "postfix \`->\` was removed"
rejects "#4 postfix ? removed + hint"        tests/programs/error_postfix_question.con   "postfix \`?\` was removed"

if rg -n '\.try_|\| try_|tryProp' Concrete >"$GATE_TMP/6d_try_surface.log"; then
  echo "  FAIL #4 propagation node survived below the parser"
  sed -n '1,8p' "$GATE_TMP/6d_try_surface.log" | sed 's/^/       /'
  FAIL=$((FAIL+1))
else
  echo "  ok   #4 no postfix-propagation node exists in AST/Core/evidence"
  PASS=$((PASS+1))
fi

echo "=== migrated forms behave (loop results + heap dot corpus) ==="
run "loop-control gate (stmt-form results)"  bash scripts/tests/check_loop_control.sh
p(){ gate_selfprint_wrap "$1" "$GATE_TMP/6d_w.con" && "$COMPILER" "$GATE_TMP/6d_w.con" -o "$GATE_TMP/6d.bin" >/dev/null 2>&1 && "$GATE_TMP/6d.bin"; }
[ "$(p tests/programs/loop_break_result_basic.con)" = "5" ]  && { echo "  ok   loop_break_result_basic => 5"; PASS=$((PASS+1)); } || { echo "  FAIL loop_break_result_basic"; FAIL=$((FAIL+1)); }
[ "$(p tests/programs/heap_dot_access.con)" = "1" ]          && { echo "  ok   heap_dot_access => 1"; PASS=$((PASS+1)); } || { echo "  FAIL heap_dot_access"; FAIL=$((FAIL+1)); }
[ "$(p tests/programs/heap_arrow.con)" = "20" ]              && { echo "  ok   heap corpus (migrated) => 20"; PASS=$((PASS+1)); } || { echo "  FAIL heap corpus"; FAIL=$((FAIL+1)); }

echo "=== 6B semantics unweakened (ownership + agreement) ==="
run "linear-conservation gate"  bash scripts/tests/check_linear_conservation.sh
run "ownership-judgment gate"   bash scripts/tests/check_ownership_judgment.sh
run "trailing-value blocks"     bash scripts/tests/check_trailing_value_blocks.sh

echo "=== unsafe stays visible via capability, not syntax ==="
printf 'mod m { fn bad(p: *mut i32) -> i32 { return *p; } fn main() -> Int { return 0; } }' > "$GATE_TMP/6d_raw.con"
rejects "raw deref without Unsafe still rejected (E052x)" "$GATE_TMP/6d_raw.con" "E052"

echo
echo "PHASE-6D-SURFACE: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
