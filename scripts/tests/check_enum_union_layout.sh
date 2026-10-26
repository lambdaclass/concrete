#!/usr/bin/env bash
# Enum union layout gate (audit 2026-07-16).
#
# The canonical builtin Option/Result union is shared by ALL instantiations of
# the program, so it must cover the worst ALIGNMENT-AWARE footprint
# (`end = alignUp 4 align + size`), not merely the worst tySize; and every
# alloca of an enum whose payloads need 8-byte alignment must carry an
# explicit `align 8` (the `{ i32, [N x i8] }` union declaration only carries
# 4-byte alignment, while payload loads/stores assume natural alignment).
# The byte-array storage itself is deliberate: aggregate load/store of a
# partially-initialized union is poison-safe only at byte granularity.
#
# Locks the class: footprint coverage, alloca alignment, no align bloat for
# align-1-only programs, and (bug 121) that LLVM's layout of every emitted
# struct/enum type equals the compiler's (leg 5, scripts/tests/lib/layout_probe.py).

set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/fresh.sh"
require_fresh_binary || exit 1
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first ($C missing)" >&2; exit 2; }
# RUN THE COMPILED ARTIFACT, not a JIT interpretation of the IR.
#
# This leg used `lli`, and `lli` from LLVM 21.1.8 cannot run this program at all: the ORC path dies
# with `Symbol "orc_rt_alt_UnwindInfoManager_register" not found in bootstrap symbols map`,
# `--jit-kind=mcjit` reports no available targets, and `--force-interpreter` aborts with
# `Cannot load value of type %enum.Option` — which is precisely the aggregate this fixture exists to
# exercise. The emitted IR was fine throughout; the runner was not.
#
# Compiling with `clang` and executing the result gives 42007 immediately, and is a better test on
# its own terms: it exercises the artifact the compiler actually ships rather than an interpretation
# of the IR, and the interpreter's inability to load an aggregate says nothing about the layout.
# `lli` is kept as a fallback for environments without clang.
CC="$(command -v clang || command -v cc || true)"
LLI="$(command -v lli || true)"
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

FIX="$ROOT_DIR/tests/programs/regress_enum_canonical_align.con"
"$C" "$FIX" --emit-llvm > "$TMP/fix.ll" 2>"$TMP/fix.err" || { no "fixture compiles ($(head -1 "$TMP/fix.err"))"; echo "ENUM-UNION-LAYOUT: PASS=$PASS FAIL=$FAIL"; exit 1; }

# 1. Footprint coverage: worst end is Option<[i64;3]> at 8+24=32 → [28 x i8], and the
#    zero-length [0 x i64] member gives the union the payload's 8-byte alignment (bug 121).
if grep -q '%enum.Option = type { \[0 x i64\], i32, \[28 x i8\] }' "$TMP/fix.ll"; then
  ok "union covers worst footprint ({ [0 x i64], i32, [28 x i8] })"
else
  no "union footprint (want { [0 x i64], i32, [28 x i8] }, got: $(grep '%enum.Option = type' "$TMP/fix.ll"))"
fi

# 2. Every %enum.Option alloca carries align 8 (payloads need it here).
total=$(grep -c 'alloca %enum.Option' "$TMP/fix.ll" || true)
aligned=$(grep -c 'alloca %enum.Option, align 8' "$TMP/fix.ll" || true)
if [ "$total" -gt 0 ] && [ "$total" = "$aligned" ]; then
  ok "all $total enum allocas carry align 8"
else
  no "alloca alignment ($aligned of $total enum allocas have align 8)"
fi

# 3. Behavioral: reads back the high-alignment payload correctly.
if [ -n "$CC" ]; then
  if "$CC" -o "$TMP/fix.exe" "$TMP/fix.ll" 2>"$TMP/cc.err"; then
    out="$("$TMP/fix.exe" 2>/dev/null || true)"
    if [ "$out" = "42007" ]; then ok "compiled fixture runs: $out"; else no "compiled fixture runs (want 42007, got '$out')"; fi
  else
    no "the emitted IR did not compile: $(head -1 "$TMP/cc.err")"
  fi
elif [ -n "$LLI" ]; then
  out="$("$LLI" "$TMP/fix.ll" 2>/dev/null || true)"
  if [ "$out" = "42007" ]; then ok "fixture runs under lli: $out"; else no "fixture runs (want 42007, got '$out')"; fi
else
  # NOT A SKIP. Without a compiler or a JIT the behavioural property is UNVERIFIED, and a gate that
  # prints "skip" and still reports FAIL=0 says the layout was checked when only its shape was. The
  # two legs above read the emitted IR; nothing above this line observes the program computing the
  # right answer.
  no "no clang/cc and no lli — the behavioural leg did NOT run, so the payload read-back is unverified"
fi

# 4. Align-1-only program: no align attribute, byte-exact old shape.
cat > "$TMP/lo.con" <<'EOF'
enum Option<T> {
    Some { value: T },
    None
}

fn main() with(Console) -> Int {
    let a: [u8; 24] = [7; 24];
    let arr: Option<[u8; 24]> = Option::Some { value: a };
    match arr {
        Option::Some { value } => { print_int(value[0] as Int); },
        Option::None {} => { print_int(-2); },
    }
    return 0;
}
EOF
"$C" "$TMP/lo.con" --emit-llvm > "$TMP/lo.ll" 2>"$TMP/lo.err" || { no "align-1 fixture compiles"; echo "ENUM-UNION-LAYOUT: PASS=$PASS FAIL=$FAIL"; exit 1; }
if grep -q '%enum.Option = type { i32, \[24 x i8\] }' "$TMP/lo.ll" \
   && ! grep -q 'alloca %enum.Option, align' "$TMP/lo.ll"; then
  ok "align-1-only: byte-exact union, no align attribute"
else
  no "align-1-only shape ($(grep '%enum.Option = type' "$TMP/lo.ll"); $(grep 'alloca %enum.Option' "$TMP/lo.ll" | head -1))"
fi

# 5. LLVM agrees with the compiler's layout model (bug 121). Field access is a byte GEP at the
#    compiler's offset; aggregate loads, stores and allocas use LLVM's layout of the emitted type.
#    For every struct and enum `--report layout` lists, a compiled probe asks LLVM itself (GEP
#    arithmetic under the module's datalayout) for size, alignment and each field offset. Before
#    bug 121's fix this found 99 disagreements in tests/programs; conlog's was a String pointer
#    copied into padding and freed (SIGSEGV on Linux). The remaining ones are bug 122 (the
#    canonical builtin Option/Result union), listed below so a NEW disagreement fails and a FIXED
#    one fails too until the ledger is updated.
KNOWN_122="tests/programs/let_else.con Result size
tests/programs/let_else.con Result align
tests/programs/match_expr_linear.con Result size
tests/programs/regress_229_user_option_does_not_hide_builtin.con Option size
tests/programs/regress_229_user_option_does_not_hide_builtin.con Option align
tests/programs/test_defer_result_nested.con Result size
tests/programs/test_defer_result_nested.con Result align
tests/programs/wildcard_pattern.con Result size
tests/programs/wildcard_pattern.con Result align
tests/regressions/bug122_builtin_enum_canonical_layout/repro.con Rec size
tests/regressions/bug122_builtin_enum_canonical_layout/repro.con Rec field#1 offset
tests/regressions/bug122_builtin_enum_canonical_layout/repro.con Rec field#2 offset"
# Programs in the corpus the compiler rejects by design (run_err / rejected-report fixtures in
# run_tests.sh). Pinned exactly: a program that newly fails to report or emit is lost coverage
# and fails the leg, and one listed here that now compiles must be removed.
KNOWN_REJECTED="tests/programs/adversarial_linear_array.con
tests/programs/adversarial_linear_borrow_and_move.con
tests/programs/adversarial_linear_branch_consume.con
tests/programs/adversarial_linear_double_use.con
tests/programs/adversarial_linear_leak.con
tests/programs/adversarial_neg_borrow_outlives_owner.con
tests/programs/adversarial_neg_double_move_branch.con
tests/programs/adversarial_neg_enum_variant_type_mismatch.con
tests/programs/adversarial_neg_move_loop_body.con
tests/programs/adversarial_neg_private_struct_field.con
tests/programs/bug_int_match_disagree.con
tests/programs/pressure_err_branch_leak.con
tests/programs/pressure_err_defer_then_move.con
tests/programs/pressure_err_destroy_then_use.con
tests/programs/pressure_err_linear_no_destroy.con
tests/programs/regress_093_shadowing_locals_consumed.con
tests/programs/regress_094_shadow_state_by_identity.con
tests/programs/regress_094_shadowing_binders.con
tests/programs/report_interface_check.con"
# Valid programs the compiler still refuses because of an OPEN bug, each named beside it. They are
# not rejected by design: pinned the same way, so the fix of the bug fails this leg until the entry
# is removed (and the program is probed like any other). None at present (bug 227's
# regress_217_enclosing_type_not_in_scope left with its fix, 2026-10-25).
OPEN_BUG_REJECTED=""
# Annotated layouts the probe must reach (their report headers carry #[repr(C)] / #[packed]).
MUST_PROBE="tests/programs/regress_bug121_enum_field_layout.con
tests/programs/repr_packed.con
tests/programs/repr_c_basic.con"
if ! command -v python3 >/dev/null 2>&1 || [ -z "$CC" ]; then
  no "layout agreement: needs python3 and clang/cc — the LLVM-vs-compiler leg did NOT run"
else
  mkdir -p "$TMP/probe"
  probe_files=$(grep -l '^[[:space:]]*\(pub \)\?\(struct\|enum\) ' tests/programs/*.con | grep -v '/error_')
  # shellcheck disable=SC2086
  python3 -I "$ROOT_DIR/scripts/tests/lib/layout_probe.py" "$C" "$CC" "$TMP/probe" $probe_files \
    tests/programs/regress_bug121_enum_field_layout.con \
    tests/regressions/bug122_builtin_enum_canonical_layout/repro.con > "$TMP/probe.out" 2>&1
  n_ok=$(grep -c '^ok ' "$TMP/probe.out" || true)
  grep '^MISMATCH ' "$TMP/probe.out" | sed -E 's/^MISMATCH //; s/ compiler=[0-9]+ llvm=[0-9]+$//' | sort -u > "$TMP/mis.txt"
  printf '%s\n' "$KNOWN_122" | sort -u > "$TMP/known.txt"
  new_mis=$(comm -23 "$TMP/mis.txt" "$TMP/known.txt")
  gone=$(comm -13 "$TMP/mis.txt" "$TMP/known.txt")
  failed=$(grep '^PROBE-FAILED' "$TMP/probe.out" || true)
  grep '^REJECTED ' "$TMP/probe.out" | awk '{print $2}' | sort -u > "$TMP/rej.txt"
  printf '%s\n' "$KNOWN_REJECTED" "$OPEN_BUG_REJECTED" | sed '/^$/d' | sort -u > "$TMP/known_rej.txt"
  rej_diff=$(diff "$TMP/known_rej.txt" "$TMP/rej.txt" || true)
  unprobed=""
  while IFS= read -r f; do
    grep -q "^ok $f " "$TMP/probe.out" || unprobed="$unprobed $f"
  done <<< "$MUST_PROBE"
  if [ -n "$failed" ]; then no "layout agreement: probe failures:"; printf '%s\n' "$failed" | awk 'NR<=5'
  elif [ -n "$rej_diff" ]; then no "layout agreement: the rejected programs differ from KNOWN_REJECTED (< pinned, > now):"; printf '%s\n' "$rej_diff" | awk 'NR<=10'
  elif [ -n "$unprobed" ]; then no "layout agreement: not probed clean:$unprobed"
  elif [ -n "$new_mis" ]; then no "layout agreement: LLVM and the compiler disagree:"; grep -F -f <(printf '%s\n' "$new_mis") "$TMP/probe.out" | awk 'NR<=10'
  elif [ -n "$gone" ]; then no "layout agreement: bug 122 entries no longer disagree — fixed? update docs/bugs/122 and KNOWN_122:"; printf '%s\n' "$gone"
  elif [ "$n_ok" -lt 200 ]; then no "layout agreement: only $n_ok programs probed clean (expected >= 200) — the corpus or the report format changed"
  else ok "layout agreement: LLVM matches the compiler on $n_ok programs (bug 122's $(wc -l < "$TMP/known.txt" | tr -d ' ') known disagreements pinned)"
  fi
fi

echo "ENUM-UNION-LAYOUT: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
