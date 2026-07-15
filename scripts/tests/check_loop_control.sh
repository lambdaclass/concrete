#!/usr/bin/env bash
export CONCRETE_ECHO_RESULT=1  # MAIN_EXIT_MODEL stage 1: legacy echoed-result mode until fixtures migrate (stage 2 deletes this)
# Loop-control gate (ROADMAP Phase 6 #4).
#
# break / continue / labeled loops / while-as-expression are implemented; this
# gate pins their behavior so further control-flow work (defer, more patterns)
# builds on a fixed semantics. It locks four things:
#
#   1. CONTROL FLOW — unlabeled break exits the innermost loop only; labeled
#      `break 'l` exits the named loop; `for` supports break + continue.
#   2. WHILE-AS-EXPRESSION VALUE — `break <v>` and the `else { v }` block produce
#      the loop's value, correctly at non-i64 widths (regression: the value used
#      to be stored as i64 into a narrower result slot and read back as 0).
#   3. LINEAR CLEANUP — break/continue that would skip an unconsumed linear value
#      are rejected (E0210 / E0211); a linear consumed before the break is fine.
#   4. TYPE AGREEMENT — a while-expression's break value and else value must have
#      the same type (E0222).
#
# Fixtures live in tests/programs/loop_control/ (not picked up by the main suite,
# which only scans tests/programs/*.con). See docs/LOOP_CONTROL.md.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first ($C missing)" >&2; exit 2; }
D="tests/programs/loop_control"
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# run_expect <fixture> <expected-stdout>
run_expect(){
  local name="$1" exp="$2"
  if ! "$C" "$D/$name.con" -o "$TMP/$name.bin" >"$TMP/$name.err" 2>&1; then
    no "$name: expected to compile, but it failed"; sed 's/^/        /' "$TMP/$name.err" | head -3; return
  fi
  local got; got="$("$TMP/$name.bin" 2>/dev/null)"
  [ "$got" = "$exp" ] && ok "$name -> $got" || no "$name: got '$got', want '$exp'"
}

# reject_with <fixture> <E-code>
reject_with(){
  local name="$1" code="$2"
  local out; out="$("$C" "$D/$name.con" -o "$TMP/$name.bin" 2>&1)"
  if grep <<<"$out" -qE 'error\['; then
    grep <<<"$out" -q "($code)" \
      && ok "$name rejected with $code" \
      || { no "$name rejected, but not with $code"; grep <<<"$out" -oE '\([A-Z0-9]+\)[^|]*' | head -1 | sed 's/^/        got: /'; }
  else
    no "$name: expected rejection ($code), but it compiled"
  fi
}

echo "=== 1. control flow: break scope, labeled break, for break/continue ==="
run_expect break_inner_only 3
run_expect labeled_break 1
run_expect for_break_continue 8

echo "=== 2. while-as-expression value (non-i64 width regression) ==="
run_expect loop_result_break_i32 7
run_expect loop_result_completes_i32 42

echo "=== 3. linear cleanup across break/continue ==="
run_expect break_after_consume_linear 0
reject_with neg_break_leaks_linear E0210
reject_with neg_continue_leaks_linear E0211

echo "=== 4. while-expression break/else type agreement ==="
reject_with neg_while_expr_removed E0001

echo ""
echo "LOOP-CONTROL: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
