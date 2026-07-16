#!/usr/bin/env bash
# Codegen execution-oracle regression gate.
#
# Compile-run-assert-value fixtures over the codegen surface, built from the
# 2026-06-10 adversarial codegen sweep. Most of the existing suite checks
# "does it compile" / "is it rejected" / "right diagnostic" and is blind to
# MISCOMPILES (wrong runtime value) — the class that produced H3 (mono name
# collision), C5 (nested place writes), and C6 (struct mixed-width layout).
# This gate runs each program and asserts the printed result, so that class of
# bug cannot silently return. Each fixture is `tests/codegen/<name>.con` with
# the expected value encoded here.
#
# (Lightweight precursor to the full interpreter-vs-compiled differential
# harness, ROADMAP Phase 4 #18.)

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
source "$ROOT_DIR/scripts/tests/lib/selfprint.sh"
cd "$ROOT_DIR"
COMPILER="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

# expect <name> <expected>  — compiles tests/codegen/<name>.con, runs, asserts.
expect() {
  local name="$1" exp="$2" src="tests/codegen/$1.con"
  [ -f "$src" ] || { no "$name: missing fixture $src"; return; }
  gate_selfprint_wrap "$src" "$TMP/$name.w.con"
  if "$COMPILER" "$TMP/$name.w.con" -o "$TMP/$name" >/dev/null 2>&1; then
    local out rc val; out="$("$TMP/$name" 2>/dev/null)"; rc=$?; val="$out"; [ -z "$val" ] && val="$rc"
    [ "$val" = "$exp" ] && ok "$name = $exp" || no "$name: got '$val' expect $exp"
  else
    no "$name failed to compile"
  fi
}

# expect_trap <name>  — compiles + runs, asserts the binary ABORTS on checked
# arithmetic overflow (ROADMAP #10: ordinary `+ - * / % << >>` trap on overflow/
# UB). A trap is a defined abort, not undefined behavior: exit code is nonzero
# (SIGABRT → 134) with no value on stdout.
expect_trap() {
  local name="$1" src="tests/codegen/$1.con"
  [ -f "$src" ] || { no "$name: missing fixture $src"; return; }
  if "$COMPILER" "$src" -o "$TMP/$name" >/dev/null 2>&1; then
    local out rc; out="$("$TMP/$name" 2>/dev/null)"; rc=$?
    if [ -z "$out" ] && [ "$rc" -ne 0 ]; then ok "$name traps (checked overflow, exit $rc)"
    else no "$name: expected a checked-overflow trap, got out='$out' exit=$rc"; fi
  else
    no "$name failed to compile"
  fi
}

echo "=== codegen execution oracles ==="
expect cast_truncate 44
expect cast_signext 255
# Checked arithmetic (ROADMAP #10): these overflowing ops now TRAP, they no
# longer wrap. (`wrapping_*` is the explicit opt-in for modular arithmetic.)
expect_trap i32_wrap
expect_trap u32_wrap
expect_trap i64_mul_overflow
expect shift_or 19
expect neg_div_mod_identity -7
expect short_circuit_and 7
expect short_circuit_or 1
expect precedence 18
expect comparisons 111111
expect recursion_fac 120
expect mutual_recursion 11
expect while_sum 55
expect match_payload 73
expect nested_enum 77
expect enum_struct_payload 42
expect enum_mixed_payload 1005
expect fn_pointer 42
expect return_struct 78
expect return_array 60
expect recursion_struct 155
expect struct_array_byval 106
expect chained_field 99
expect array_2d_read 3
expect mixed_width_struct 51200
expect bool_field_between 30
expect nested_field_write 7709
expect array_elem_field 53
expect two_d_write 99
expect mod_nested_generic 42
expect mod_two_instantiations 18
expect mod_triple_nested 55

echo ""
echo "CODEGEN-EXECUTION: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
