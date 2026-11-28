#!/usr/bin/env bash
# Dual-route native process-run harness (LLVM + LCB, checked + unchecked).
#
# Exercises bound native builds and independent pinned replay verification
# (scripts/checks/lib/slice0_native.sh) across both backends (two-module LLVM
# component.ll + launcher.ll and LCB lcb_body.s + lcb_rt.ssa) and both routes
# (checked discharged admission and unchecked residual-checked lowering):
#   1. Exit-42 execution on checked (CLI + bound/verify) and unchecked routes.
#   2. Two-site overflow fixtures: checked-route refusal and unchecked-route
#      trap-134 execution with exact manifest stderr bytes at site 0 and site 1.
#   3. Link-order reversal controls (LLVM and LCB, exit 42 and trap 134),
#      asserting distinct executable bytes and rejection by pinned replay.
#   4. Object-substitution (silent link-time, on-disk during bound, post-bound),
#      staged-input-mutation, extra-file, inventory-order, and judge controls.
#
# Run inside `nix develop` from the repository root (with LEAN_NUM_THREADS=2).
set -euo pipefail
# shellcheck source=scripts/checks/lib/gate_env.sh
source "$(dirname "${BASH_SOURCE[0]}")/../lib/gate_env.sh" || exit 2
root=$(cd -- "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd -P)
cd -- "$root"

work=$(mktemp -d)
trap 'rc=$?; bounded_summary || true
  if [ "$rc" = 0 ] && grep -Eqs "^(incomplete|runner-failed)" "${BOUNDED_LOG:-/nonexistent}"; then
    echo "INCOMPLETE: a bounded run timed out twice or lost its runner; not a pass"; rc=3
  fi
  rm -rf "${work:?}"; exit "$rc"' EXIT

BOUNDED_PARENT="$work"
# shellcheck source=scripts/checks/lib/bounded.sh
source "$root/scripts/checks/lib/bounded.sh"
[ "$BOUNDED_HAVE_WATCHDOG" = 1 ] || { echo "dual_route_harness: python3 is required" >&2; exit 2; }
gate_limits "${SLICE0_GATE_MEM_GB:-64}" 40000

# shellcheck source=scripts/checks/lib/slice0_dual_route.sh
source "$root/scripts/checks/lib/slice0_dual_route.sh"
dual_route_init "$root" "$work"

manifest=${CONCRETE_TEST_MANIFEST:-"$root/tests/manifest.tsv"}
pass=0
fail=0
ok() { echo "  ok   $1"; pass=$((pass + 1)); }
no() { echo "  FAIL $1"; fail=$((fail + 1)); }

# Ensure the Lean library, public CLI, and LCB tool are built.
dual_route_bounded 1800 lake build Concrete concrete lean-compiler-backend/lcb \
  >"$work/lake-build.log" 2>&1 || {
  tail -n 200 "$work/lake-build.log" >&2
  echo "dual_route_harness: lake build failed" >&2
  exit 1
}

# Materialize exact manifest expectations (status + raw stderr bytes) once.
mkdir -p "$work/expect"
for test_id in source_answer source_two_site_first_trap source_two_site_second_trap; do
  dual_route_materialize_expect "$test_id" "$work/expect/$test_id" "$manifest" \
    >"$work/expect/$test_id.row"
done

# Emit checked and unchecked bound inputs in a single Lean startup.
# The two-site overflow fixtures must refuse on the checked route (exit 1).
batch_spec="$work/emit-batch.tsv"
cat >"$batch_spec" <<EOF
checked	tests/slice0/answer.con	$root/tests/slice0/answer.con	$work/stage/answer-checked	$DUAL_ROUTE_TARGET	both	0
unchecked	tests/slice0/answer.con	$root/tests/slice0/answer.con	$work/stage/answer-unchecked	$DUAL_ROUTE_TARGET	both	0
checked	tests/slice0/two_site_first_trap.con	$root/tests/slice0/two_site_first_trap.con	$work/stage/first-checked	$DUAL_ROUTE_TARGET	both	1
checked	tests/slice0/two_site_second_trap.con	$root/tests/slice0/two_site_second_trap.con	$work/stage/second-checked	$DUAL_ROUTE_TARGET	both	1
unchecked	tests/slice0/two_site_first_trap.con	$root/tests/slice0/two_site_first_trap.con	$work/stage/first-unchecked	$DUAL_ROUTE_TARGET	both	0
unchecked	tests/slice0/two_site_second_trap.con	$root/tests/slice0/two_site_second_trap.con	$work/stage/second-unchecked	$DUAL_ROUTE_TARGET	both	0
EOF
if dual_route_emit_batch "$batch_spec" >"$work/emit-batch.stdout" 2>"$work/emit-batch.stderr" &&
    [ "$(grep -c '^CORE-CHECK: ' "$work/emit-batch.stderr")" = 2 ]; then
  ok "emit batch: checked exit-42, checked two-site CORE-CHECK refusals, and unchecked dual-route inputs"
else
  cat "$work/emit-batch.stderr" >&2
  no "emit batch failed or did not produce both CORE-CHECK refusals"
  echo "dual_route_harness: PASS=$pass FAIL=$fail"
  exit 1
fi
[ ! -e "$work/stage/first-checked" ] && [ ! -e "$work/stage/second-checked" ] &&
  ok "checked emission created no staging directory for refused two-site fixtures" ||
  no "checked emission left output for refused two-site fixtures"

# --- 1. Checked + Unchecked exit-42 matrix (LLVM + LCB) ---
run_and_check_stage() {
  local label=$1
  local stage=$2
  local expect_dir=$3
  local obs_dir=$4
  local backend
  mkdir -p "$obs_dir"
  if ! dual_route_build_and_verify "$stage" both "$obs_dir/build"; then
    no "$label: bound build or pinned replay verification failed"
    return 1
  fi
  for backend in llvm lcb; do
    dual_route_observe "$stage/$backend.exe" "$obs_dir/$backend"
    if dual_route_judge "$expect_dir" "$obs_dir/$backend"; then
      ok "$label [$backend]: verified replay and status $(cat "$obs_dir/$backend.status")"
    else
      no "$label [$backend]: got status $(cat "$obs_dir/$backend.status"), expected $(cat "$expect_dir/status")"
    fi
  done
}

# Public CLI checked route for answer.con (both backends, plus replay verify).
cli_answer="$work/cli/answer"
mkdir -p "$work/cli"
if dual_route_bounded "$DUAL_ROUTE_NATIVE_SECS" \
    "$DUAL_ROUTE_CLI" build tests/slice0/answer.con -o "$cli_answer" \
    >"$work/cli/answer.stdout" 2>"$work/cli/answer.stderr"; then
  if has_match -Fx 'language-rules version 0.1' <"$work/cli/answer.stdout" &&
      [ ! -s "$work/cli/answer.stderr" ] &&
      dual_route_check_inventory "$cli_answer" both &&
      dual_route_verify_build "$cli_answer" both "$work/cli/answer.verify.log"; then
    for backend in llvm lcb; do
      dual_route_observe "$cli_answer/$backend.exe" "$work/cli/answer-$backend"
      if dual_route_judge "$work/expect/source_answer" "$work/cli/answer-$backend"; then
        ok "CLI checked answer [$backend]: verified replay and exit 42"
      else
        no "CLI checked answer [$backend]: observation mismatch"
      fi
    done
  else
    cat "$work/cli/answer.verify.log" >&2 || true
    no "CLI checked answer: version, inventory, or pinned replay failed"
  fi
else
  cat "$work/cli/answer.stderr" >&2
  no "CLI checked answer: concrete build failed"
fi

# Confirm checked route discharged the addition while unchecked retained site 0.
if ! has_match -F 'llvm.uadd.with.overflow' <"$work/stage/answer-checked/component.ll" &&
    ! has_match -F '@.site0' <"$work/stage/answer-checked/launcher.ll" &&
    ! has_match -F '$site0' <"$work/stage/answer-checked/lcb_rt.ssa" &&
    has_match -F '@llvm.uadd.with.overflow.i8' <"$work/stage/answer-unchecked/component.ll" &&
    has_match -F '@.site0' <"$work/stage/answer-unchecked/launcher.ll" &&
    has_match -F '$site0' <"$work/stage/answer-unchecked/lcb_rt.ssa" &&
    ! cmp -s "$work/stage/answer-checked/component.ll" "$work/stage/answer-unchecked/component.ll"; then
  ok "route separation: checked answer discharges overflow check; unchecked retains site 0"
else
  no "route separation: checked and unchecked answer IR did not separate as expected"
fi

run_and_check_stage "checked answer" "$work/stage/answer-checked" \
  "$work/expect/source_answer" "$work/obs/answer-checked"
run_and_check_stage "unchecked answer" "$work/stage/answer-unchecked" \
  "$work/expect/source_answer" "$work/obs/answer-unchecked"

# --- 2. Two-site overflow fixtures: checked refusal + unchecked trap-134 ---
check_cli_checked_refusal() {
  local label=$1
  local rel_path=$2
  local out_dir="$work/cli/$label-out"
  local rc=0
  dual_route_bounded "$DUAL_ROUTE_NATIVE_SECS" \
    "$DUAL_ROUTE_CLI" build "$rel_path" -o "$out_dir" \
    >"$work/cli/$label.stdout" 2>"$work/cli/$label.stderr" || rc=$?
  if [ "$rc" = 1 ] && [ ! -s "$work/cli/$label.stdout" ] &&
      has_match -E '^CORE-CHECK: ' <"$work/cli/$label.stderr" &&
      [ ! -e "$out_dir" ] && [ ! -L "$out_dir" ]; then
    ok "CLI checked $label: refused with CORE-CHECK (exit 1, no output)"
  else
    no "CLI checked $label: expected exit 1 CORE-CHECK without output, got status $rc"
    cat "$work/cli/$label.stderr" >&2
  fi
}

check_cli_checked_refusal two-site-first tests/slice0/two_site_first_trap.con
check_cli_checked_refusal two-site-second tests/slice0/two_site_second_trap.con

for pair in \
    "first-unchecked|source_two_site_first_trap|two-site first trap (site 0)" \
    "second-unchecked|source_two_site_second_trap|two-site second trap (site 1)"; do
  IFS='|' read -r stage_key test_id label <<<"$pair"
  stage="$work/stage/$stage_key"
  if has_match -F '@.site0' <"$stage/launcher.ll" &&
      has_match -F '@.site1' <"$stage/launcher.ll" &&
      ! has_match -F '@.site2' <"$stage/launcher.ll" &&
      has_match -F '$site0' <"$stage/lcb_rt.ssa" &&
      has_match -F '$site1' <"$stage/lcb_rt.ssa" &&
      ! has_match -F '$site2' <"$stage/lcb_rt.ssa"; then
    ok "$label: both launchers carry exactly 2 trap sites"
  else
    no "$label: launcher trap table does not have exactly 2 sites"
  fi
  run_and_check_stage "unchecked $label" "$stage" \
    "$work/expect/$test_id" "$work/obs/$stage_key"
done

# Cross-check that site 0 and site 1 observations do not accept each other.
if ! dual_route_judge "$work/expect/source_two_site_second_trap" "$work/obs/first-unchecked/llvm" &&
    ! dual_route_judge "$work/expect/source_two_site_first_trap" "$work/obs/second-unchecked/lcb"; then
  ok "two-site distinction: site 0 and site 1 exact stderr bytes are mutually rejected"
else
  no "two-site distinction: site 0 and site 1 expectations were not distinct"
fi

# --- 3. Link-order reversal controls (LLVM + LCB, exit 42 and trap 134) ---
copy_stage_inputs() {
  local src=$1
  local dst=$2
  local backend=$3
  mkdir -p "$dst"
  if [ "$backend" != lcb ]; then
    cp -- "$src/component.ll" "$src/launcher.ll" "$dst/"
  fi
  if [ "$backend" != llvm ]; then
    cp -- "$src/lcb_body.s" "$src/lcb_rt.ssa" "$dst/"
  fi
}

link_order_control() {
  local label=$1
  local canonical_stage=$2
  local backend=$3
  local expect_dir=$4
  local mode="reverse-$backend-link-order"
  local ctl_dir="$work/ctl/link-order-$label"
  local stage="$ctl_dir/stage"
  local wrapper="$ctl_dir/clang-wrapper"
  local marker="$ctl_dir/triggered"
  local verify_rc=0
  mkdir -p "$ctl_dir"
  copy_stage_inputs "$canonical_stage" "$stage" "$backend"
  dual_route_make_clang_wrapper "$wrapper" "$mode" "$marker"
  if ! dual_route_bound_build "$stage" "$backend" "$ctl_dir/bound.log" "$wrapper"; then
    cat "$ctl_dir/bound.log" >&2
    no "link-order control ($label): reversed-order bound build failed unexpectedly"
    return
  fi
  if [ ! -s "$marker" ]; then
    no "link-order control ($label): clang wrapper never intercepted the link step"
    return
  fi
  if cmp -s "$canonical_stage/$backend.exe" "$stage/$backend.exe"; then
    no "link-order control ($label): reversed link order produced an identical executable"
    return
  fi
  dual_route_observe "$stage/$backend.exe" "$ctl_dir/run"
  if ! dual_route_judge "$expect_dir" "$ctl_dir/run"; then
    no "link-order control ($label): reversed-order binary did not preserve runtime observation"
    return
  fi
  dual_route_verify_build "$stage" "$backend" "$ctl_dir/verify.log" || verify_rc=$?
  if [ "$verify_rc" -ne 0 ] &&
      has_match -F "$backend retained build differs from pinned replay" <"$ctl_dir/verify.log"; then
    ok "link-order control ($label): distinct binary caught by pinned replay"
  else
    cat "$ctl_dir/verify.log" >&2
    no "link-order control ($label): pinned replay did not reject reversed link order (rc=$verify_rc)"
  fi
}

link_order_control answer-llvm "$work/stage/answer-unchecked" llvm "$work/expect/source_answer"
link_order_control answer-lcb "$work/stage/answer-unchecked" lcb "$work/expect/source_answer"
link_order_control trap-llvm "$work/stage/second-unchecked" llvm "$work/expect/source_two_site_second_trap"
link_order_control trap-lcb "$work/stage/second-unchecked" lcb "$work/expect/source_two_site_second_trap"

# --- 4. Object-substitution, input-mutation, and inventory controls ---
# 4a. Silent link-time object substitution: on-disk .o stays intact, so bound
#     and its self-hash succeed, while BOTH pinned replay AND exact process
#     observation catch the planted wrong-site / wrong-status behavior.
silent_object_sub_control() {
  local label=$1
  local target_stage=$2
  local replacement_stage=$3
  local backend=$4
  local mode=$5
  local expect_dir=$6
  local planted_status=$7
  local planted_stderr=$8
  local ctl_dir="$work/ctl/silent-sub-$label"
  local stage="$ctl_dir/stage"
  local wrapper="$ctl_dir/clang-wrapper"
  local marker="$ctl_dir/triggered"
  local verify_rc=0
  local planted_ok=0
  mkdir -p "$ctl_dir"
  copy_stage_inputs "$target_stage" "$stage" "$backend"
  dual_route_make_clang_wrapper "$wrapper" "$mode" "$marker" "$replacement_stage"
  if ! dual_route_bound_build "$stage" "$backend" "$ctl_dir/bound.log" "$wrapper"; then
    cat "$ctl_dir/bound.log" >&2
    no "silent object substitution ($label): bound build failed unexpectedly"
    return
  fi
  if [ ! -s "$marker" ]; then
    no "silent object substitution ($label): wrapper did not trigger"
    return
  fi
  if ! (cd -- "$stage" && sha256sum --quiet --strict -c "$backend.link.sha256"); then
    no "silent object substitution ($label): self-hash failed before replay"
    return
  fi
  dual_route_verify_build "$stage" "$backend" "$ctl_dir/verify.log" || verify_rc=$?
  dual_route_observe "$stage/$backend.exe" "$ctl_dir/run"
  if [ "$(cat "$ctl_dir/run.status")" = "$planted_status" ] && [ ! -s "$ctl_dir/run.stdout" ]; then
    if [ -z "$planted_stderr" ]; then
      [ ! -s "$ctl_dir/run.stderr" ] && planted_ok=1
    elif printf '%s\n' "$planted_stderr" | cmp -s - "$ctl_dir/run.stderr"; then
      planted_ok=1
    fi
  fi
  if [ "$verify_rc" -ne 0 ] &&
      has_match -F "$backend retained build differs from pinned replay" <"$ctl_dir/verify.log" &&
      [ "$planted_ok" = 1 ] &&
      ! dual_route_judge "$expect_dir" "$ctl_dir/run"; then
    ok "silent object substitution ($label): rejected by both pinned replay and exact stderr/status judge"
  else
    cat "$ctl_dir/verify.log" >&2
    no "silent object substitution ($label): replay rc=$verify_rc, planted_ok=$planted_ok, status=$(cat "$ctl_dir/run.status")"
  fi
}

swapped_site0_line='concrete: trap: overflow at tests/slice0/two_site_second_trap.con:2:21 (site 0)'
silent_object_sub_control llvm-component-site0-into-site1 \
  "$work/stage/second-unchecked" "$work/stage/first-unchecked" \
  llvm link-substitute-llvm-component "$work/expect/source_two_site_second_trap" \
  134 "$swapped_site0_line"
silent_object_sub_control llvm-launcher-site1-into-site0 \
  "$work/stage/first-unchecked" "$work/stage/second-unchecked" \
  llvm link-substitute-llvm-launcher "$work/expect/source_two_site_first_trap" \
  134 "$swapped_site0_line"
silent_object_sub_control lcb-body-site0-into-site1 \
  "$work/stage/second-unchecked" "$work/stage/first-unchecked" \
  lcb link-substitute-lcb-body "$work/expect/source_two_site_second_trap" \
  134 "$swapped_site0_line"
silent_object_sub_control lcb-rt-site1-into-site0 \
  "$work/stage/first-unchecked" "$work/stage/second-unchecked" \
  lcb link-substitute-lcb-rt "$work/expect/source_two_site_first_trap" \
  134 "$swapped_site0_line"
silent_object_sub_control llvm-component-exit42-into-trap \
  "$work/stage/first-unchecked" "$work/stage/answer-unchecked" \
  llvm link-substitute-llvm-component "$work/expect/source_two_site_first_trap" \
  42 ""
silent_object_sub_control lcb-body-exit42-into-trap \
  "$work/stage/first-unchecked" "$work/stage/answer-unchecked" \
  lcb link-substitute-lcb-body "$work/expect/source_two_site_first_trap" \
  42 ""

# 4b. On-disk object substitution and staged-input mutation during bound build.
bound_fault_control() {
  local label=$1
  local target_stage=$2
  local replacement_stage=$3
  local backend=$4
  local mode=$5
  local expected_msg=$6
  local ctl_dir="$work/ctl/bound-fault-$label"
  local stage="$ctl_dir/stage"
  local wrapper="$ctl_dir/clang-wrapper"
  local marker="$ctl_dir/triggered"
  local rc=0
  mkdir -p "$ctl_dir"
  copy_stage_inputs "$target_stage" "$stage" "$backend"
  dual_route_make_clang_wrapper "$wrapper" "$mode" "$marker" "$replacement_stage"
  dual_route_bound_build "$stage" "$backend" "$ctl_dir/bound.log" "$wrapper" || rc=$?
  if [ -s "$marker" ] && [ "$rc" -ne 0 ] && has_match -F "$expected_msg" <"$ctl_dir/bound.log"; then
    ok "bound fault control ($label): rejected with '$expected_msg'"
  else
    cat "$ctl_dir/bound.log" >&2
    no "bound fault control ($label): expected '$expected_msg' (rc=$rc, triggered=$(test -s "$marker" && echo yes || echo no))"
  fi
}

bound_fault_control llvm-overwrite-component-obj \
  "$work/stage/answer-unchecked" "$work/stage/first-unchecked" \
  llvm overwrite-llvm-component-obj 'component.o: FAILED'
bound_fault_control lcb-overwrite-body-obj \
  "$work/stage/answer-unchecked" "$work/stage/first-unchecked" \
  lcb overwrite-lcb-body-obj 'lcb_body.o: FAILED'
bound_fault_control llvm-mutate-launcher-ll \
  "$work/stage/answer-unchecked" "" \
  llvm mutate-llvm-launcher-ll 'launcher.ll: FAILED'
bound_fault_control lcb-mutate-body-s \
  "$work/stage/answer-unchecked" "" \
  lcb mutate-lcb-body-s 'lcb_body.s: FAILED'

# 4c. Post-bound object substitution, staged input tampering, extra file, and
#     inventory-order tampering before verify.
post_bound_control() {
  local label=$1
  local src_stage=$2
  local backend=$3
  local expected_msg=$4
  local ctl_dir="$work/ctl/post-$label"
  local stage="$ctl_dir/stage"
  local rc=0
  mkdir -p "$ctl_dir"
  cp -R -- "$src_stage" "$stage"
  case "$label" in
    llvm-launcher-obj)
      cp -- "$work/stage/second-unchecked/launcher.o" "$stage/launcher.o"
      dual_route_verify_build "$stage" "$backend" "$ctl_dir/out.log" || rc=$?
      ;;
    lcb-rt-obj)
      cp -- "$work/stage/second-unchecked/lcb_rt.o" "$stage/lcb_rt.o"
      dual_route_verify_build "$stage" "$backend" "$ctl_dir/out.log" || rc=$?
      ;;
    llvm-input-check)
      dual_route_snapshot_inputs "$stage" "$backend" "$ctl_dir/snap"
      printf '\n; tampered\n' >>"$stage/launcher.ll"
      dual_route_check_inputs "$stage" "$ctl_dir/snap" >"$ctl_dir/out.log" 2>&1 || rc=$?
      ;;
    lcb-input-check)
      dual_route_snapshot_inputs "$stage" "$backend" "$ctl_dir/snap"
      printf '\n' >>"$stage/lcb_body.s"
      dual_route_check_inputs "$stage" "$ctl_dir/snap" >"$ctl_dir/out.log" 2>&1 || rc=$?
      ;;
    extra-file)
      printf 'stray\n' >"$stage/extra.o"
      dual_route_verify_build "$stage" "$backend" "$ctl_dir/out.log" || rc=$?
      ;;
    inventory-order)
      awk 'NR==3{a=$0;next} NR==4{print; print a; next} {print}' "$stage/llvm.link.sha256" >"$stage/llvm.link.sha256.tmp"
      mv -- "$stage/llvm.link.sha256.tmp" "$stage/llvm.link.sha256"
      dual_route_check_inventory "$stage" "$backend" >"$ctl_dir/out.log" 2>&1 || rc=$?
      ;;
    lcb-inventory-order)
      awk 'NR==3{a=$0;next} NR==5{print; print a; next} {print}' "$stage/lcb.link.sha256" >"$stage/lcb.link.sha256.tmp"
      mv -- "$stage/lcb.link.sha256.tmp" "$stage/lcb.link.sha256"
      dual_route_check_inventory "$stage" "$backend" >"$ctl_dir/out.log" 2>&1 || rc=$?
      ;;
  esac
  if [ "$rc" -ne 0 ] && has_match -F "$expected_msg" <"$ctl_dir/out.log"; then
    ok "post-bound control ($label): rejected with '$expected_msg'"
  else
    cat "$ctl_dir/out.log" >&2
    no "post-bound control ($label): expected '$expected_msg' (rc=$rc)"
  fi
}

post_bound_control llvm-launcher-obj "$work/stage/first-unchecked" both 'launcher.o: FAILED'
post_bound_control lcb-rt-obj "$work/stage/first-unchecked" both 'lcb_rt.o: FAILED'
post_bound_control llvm-input-check "$work/stage/first-unchecked" llvm 'staged link input changed during native build: launcher.ll'
post_bound_control lcb-input-check "$work/stage/first-unchecked" lcb 'staged link input changed during native build: lcb_body.s'
post_bound_control extra-file "$work/stage/first-unchecked" both 'Only in '
post_bound_control inventory-order "$work/stage/first-unchecked" llvm 'unexpected llvm.link.sha256 inventory order'
post_bound_control lcb-inventory-order "$work/stage/first-unchecked" lcb 'unexpected lcb.link.sha256 inventory order'

# --- 5. Observation judge negative controls ---
judge_ctl="$work/ctl/judge"
mkdir -p "$judge_ctl"
forge_obs() {
  local name=$1
  local status=$2
  local err_fmt=$3
  local out_text=$4
  local prefix="$judge_ctl/obs-$name"
  printf '%s\n' "$status" >"$prefix.status"
  printf "$err_fmt" >"$prefix.stderr"
  printf '%s' "$out_text" >"$prefix.stdout"
  echo "$prefix"
}

exact_trap_line='concrete: trap: overflow at tests/slice0/two_site_first_trap.con:2:21 (site 0)'
if dual_route_judge "$work/expect/source_two_site_first_trap" \
    "$(forge_obs exact 134 "$exact_trap_line\n" '')"; then
  ok "judge accepts exact trap-134 stderr observation"
else
  no "judge rejected exact trap-134 stderr observation"
fi

for spec in \
    "wrong-status|0|$exact_trap_line\n|" \
    "no-newline|134|$exact_trap_line|" \
    "extra-newline|134|$exact_trap_line\n\n|" \
    "wrong-site|134|concrete: trap: overflow at tests/slice0/two_site_first_trap.con:2:21 (site 1)\n|" \
    "stray-stdout|134|$exact_trap_line\n|42" \
    "incomplete|incomplete||" \
    "runner-failed|runner-failed||"; do
  IFS='|' read -r cname cstatus cerr cout <<<"$spec"
  if dual_route_judge "$work/expect/source_two_site_first_trap" \
      "$(forge_obs "$cname" "$cstatus" "$cerr" "$cout")"; then
    no "judge accepted forged observation: $cname"
  else
    ok "judge rejects forged observation: $cname"
  fi
done

printf 'runner-failed\n' >"$judge_ctl/status"
: >"$judge_ctl/stderr"
if dual_route_judge "$judge_ctl" "$(forge_obs rf-match runner-failed '' '')"; then
  no "judge accepted runner-failed even when expected matched"
else
  ok "judge rejects runner-failed even when expected matches"
fi

echo "dual_route_harness: PASS=$pass FAIL=$fail"
[ "$fail" -eq 0 ]
