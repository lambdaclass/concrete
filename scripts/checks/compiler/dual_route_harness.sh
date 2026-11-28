#!/usr/bin/env bash
# Dual-route native process-run harness (LLVM + LCB, checked + unchecked).
#
# Exercises bound native builds and independent pinned replay verification
# (scripts/checks/lib/slice0_native.sh) across both backends (two-module LLVM
# component.ll + launcher.ll and LCB lcb_body.s + lcb_rt.ssa) and both routes
# (checked discharged admission and unchecked residual-checked lowering):
#   1. Exit-42 execution on checked (CLI + bound/verify) and unchecked routes,
#      verifying that the checked CLI publication contains no unchecked lowering
#      (derived from Admission.check's own output) and matches checked inputs.
#   2. Two-site overflow fixtures: checked-route refusal and unchecked-route
#      normal exit-134 execution with exact manifest stderr bytes at site 0 and
#      site 1, resolving every source path and project root from the selected
#      manifest row.
#   3. Link-order reversal controls (LLVM and LCB, exit 42 and trap 134),
#      asserting distinct executable bytes and rejection by pinned replay.
#   4. Object-substitution (silent link-time, on-disk during bound, post-bound),
#      staged-input-mutation, extra-file, and inventory-order controls.
#   5. Termination-reason and judge controls (including a native SIGABRT
#      executable and an abort()-mutated trap launcher).
#   6. Checked-publication negative controls (reviewer's mutation: unchecked
#      lowering behind checked admission, both combined and per-backend).
#   7. Manifest source-resolution negative controls (20 + 21 copied manifest,
#      and outside-manifest / symlink / traversal / non-con refusals).
#
# Run inside `nix develop` from the repository root (with LEAN_NUM_THREADS=2).
set -euo pipefail
# shellcheck source=scripts/checks/lib/gate_env.sh
source "$(dirname "${BASH_SOURCE[0]}")/../lib/gate_env.sh" || exit 2
caller_pwd=$(pwd -P)
root=$(cd -- "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd -P)

manifest=${CONCRETE_TEST_MANIFEST:-"$root/tests/manifest.tsv"}
[[ "$manifest" = /* ]] || manifest="$caller_pwd/$manifest"
if [ -n "${CONCRETE_DUAL_ROUTE_CLI:-}" ] && [[ "$CONCRETE_DUAL_ROUTE_CLI" != /* ]]; then
  CONCRETE_DUAL_ROUTE_CLI="$caller_pwd/$CONCRETE_DUAL_ROUTE_CLI"
fi
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

pass=0
fail=0
ok() { echo "  ok   $1"; pass=$((pass + 1)); }
no() { echo "  FAIL $1"; fail=$((fail + 1)); }

# Resolve every program path, project root, and expectation from the same
# selected manifest row before compiling anything, refusing sources outside it.
mkdir -p "$work/expect"
for row_spec in \
    "source_answer|exit" \
    "source_two_site_first_trap|trap" \
    "source_two_site_second_trap|trap"; do
  IFS='|' read -r test_id expect_kind <<<"$row_spec"
  if ! dual_route_load_con_row "$test_id" "$work/expect/$test_id" "$manifest" "$expect_kind"; then
    echo "dual_route_harness: refused manifest row '$test_id' in $manifest" >&2
    exit 1
  fi
done

answer_rel=$(cat "$work/expect/source_answer/rel_path")
answer_src=$(cat "$work/expect/source_answer/source_path")
answer_root=$(cat "$work/expect/source_answer/project_root")
manifest_tests_dir=$(cat "$work/expect/source_answer/tests_dir")

first_rel=$(cat "$work/expect/source_two_site_first_trap/rel_path")
first_src=$(cat "$work/expect/source_two_site_first_trap/source_path")
first_root=$(cat "$work/expect/source_two_site_first_trap/project_root")

second_rel=$(cat "$work/expect/source_two_site_second_trap/rel_path")
second_src=$(cat "$work/expect/source_two_site_second_trap/source_path")
second_root=$(cat "$work/expect/source_two_site_second_trap/project_root")

if [ "$first_root" != "$answer_root" ] || [ "$second_root" != "$answer_root" ]; then
  echo "dual_route_harness: selected manifest rows resolved to inconsistent project roots" >&2
  exit 1
fi
ok "manifest rows: resolved con: sources and expectations inside $manifest_tests_dir"

# Ensure the Lean library, public CLI, and LCB tool are built.
dual_route_bounded 1800 lake build Concrete concrete lean-compiler-backend/lcb \
  >"$work/lake-build.log" 2>&1 || {
  tail -n 200 "$work/lake-build.log" >&2
  echo "dual_route_harness: lake build failed" >&2
  exit 1
}

# Emit checked and unchecked bound inputs in a single Lean startup from the
# manifest-resolved source paths.
batch_spec="$work/emit-batch.tsv"
cat >"$batch_spec" <<EOF
checked	$answer_rel	$answer_src	$work/stage/answer-checked	$DUAL_ROUTE_TARGET	both	0
unchecked	$answer_rel	$answer_src	$work/stage/answer-unchecked	$DUAL_ROUTE_TARGET	both	0
checked	$first_rel	$first_src	$work/stage/first-checked	$DUAL_ROUTE_TARGET	both	1
checked	$second_rel	$second_src	$work/stage/second-checked	$DUAL_ROUTE_TARGET	both	1
unchecked	$first_rel	$first_src	$work/stage/first-unchecked	$DUAL_ROUTE_TARGET	both	0
unchecked	$second_rel	$second_src	$work/stage/second-unchecked	$DUAL_ROUTE_TARGET	both	0
EOF
if dual_route_emit_batch "$batch_spec" >"$work/emit-batch.stdout" 2>"$work/emit-batch.stderr" &&
    [ "$(grep -c '^CORE-CHECK: ' "$work/emit-batch.stderr")" = 2 ]; then
  ok "emit batch: checked answer, checked two-site CORE-CHECK refusals, and unchecked dual-route inputs"
else
  cat "$work/emit-batch.stderr" >&2
  no "emit batch failed or did not produce both CORE-CHECK refusals"
  echo "dual_route_harness: PASS=$pass FAIL=$fail"
  exit 1
fi
[ ! -e "$work/stage/first-checked" ] && [ ! -e "$work/stage/second-checked" ] &&
  ok "checked emission created no staging directory for refused two-site fixtures" ||
  no "checked emission left output for refused two-site fixtures"

# --- 1. Checked + Unchecked exit matrix (LLVM + LCB) ---
format_term() {
  local prefix=$1
  printf 'reason=%s status=%s signal=%s' \
    "$(cat "$prefix.reason" 2>/dev/null || echo missing)" \
    "$(cat "$prefix.status" 2>/dev/null || echo missing)" \
    "$(cat "$prefix.signal" 2>/dev/null || echo missing)"
}

format_expect() {
  local dir=$1
  printf 'reason=%s status=%s signal=%s' \
    "$(cat "$dir/reason" 2>/dev/null || echo missing)" \
    "$(cat "$dir/status" 2>/dev/null || echo missing)" \
    "$(cat "$dir/signal" 2>/dev/null || echo missing)"
}

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
      ok "$label [$backend]: verified replay and normal exit $(cat "$obs_dir/$backend.status")"
    else
      no "$label [$backend]: got $(format_term "$obs_dir/$backend"), expected $(format_expect "$expect_dir")"
    fi
  done
}

# Public CLI checked route for source_answer from the selected manifest's project root.
cli_answer="$work/cli/answer"
mkdir -p "$work/cli"
if (cd -- "$answer_root" &&
    dual_route_bounded "$DUAL_ROUTE_NATIVE_SECS" \
      "$DUAL_ROUTE_CLI" build "$answer_rel" -o "$cli_answer") \
    >"$work/cli/answer.stdout" 2>"$work/cli/answer.stderr"; then
  if has_match -Fx 'language-rules version 0.1' <"$work/cli/answer.stdout" &&
      [ ! -s "$work/cli/answer.stderr" ] &&
      dual_route_check_inventory "$cli_answer" both &&
      dual_route_verify_build "$cli_answer" both "$work/cli/answer.verify.log"; then
    for backend in llvm lcb; do
      dual_route_observe "$cli_answer/$backend.exe" "$work/cli/answer-$backend"
      if dual_route_judge "$work/expect/source_answer" "$work/cli/answer-$backend"; then
        ok "CLI checked answer [$backend]: verified replay and normal exit $(cat "$work/cli/answer-$backend.status")"
      else
        no "CLI checked answer [$backend]: got $(format_term "$work/cli/answer-$backend"), expected $(format_expect "$work/expect/source_answer")"
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

# Verify what the public CLI actually published (and the checked/unchecked stages)
# directly from Admission.check's output, plus derive the two-site trap line and
# SIGABRT control LLVM module in a single Lean startup.
ops_spec="$work/verify-ops.tsv"
cat >"$ops_spec" <<EOF
verify-separation	$answer_rel	$answer_src	$cli_answer	$work/stage/answer-unchecked	$DUAL_ROUTE_TARGET
verify-checked	$answer_rel	$answer_src	$work/stage/answer-checked	$DUAL_ROUTE_TARGET	both
verify-unchecked	$first_rel	$first_src	$work/stage/first-unchecked	$DUAL_ROUTE_TARGET	both
verify-unchecked	$second_rel	$second_src	$work/stage/second-unchecked	$DUAL_ROUTE_TARGET	both
site-line	$second_rel	$second_src	0	$work/second-site0.line
emit-sigabrt	$work/expect/source_two_site_first_trap/stderr	$work/ctl/sigabrt-exe
EOF
if [ -d "$cli_answer" ] &&
    dual_route_ops_batch "$ops_spec" >"$work/verify-ops.stdout" 2>"$work/verify-ops.stderr"; then
  ok "CLI checked publication: admitted discharge verified on all four published inputs (no unchecked lowering) and separated from unchecked route"
  ok "two-site unchecked stages: both LLVM and LCB carry exact admission-derived trap-site tables"
else
  cat "$work/verify-ops.stderr" >&2 || true
  no "CLI checked publication or stage discharge verification failed"
fi

run_and_check_stage "checked answer" "$work/stage/answer-checked" \
  "$work/expect/source_answer" "$work/obs/answer-checked"
run_and_check_stage "unchecked answer" "$work/stage/answer-unchecked" \
  "$work/expect/source_answer" "$work/obs/answer-unchecked"

# --- 2. Two-site overflow fixtures: checked refusal + unchecked trap-134 ---
check_cli_checked_refusal() {
  local label=$1
  local project_root=$2
  local rel_path=$3
  local out_dir="$work/cli/$label-out"
  local rc=0
  (cd -- "$project_root" &&
    dual_route_bounded "$DUAL_ROUTE_NATIVE_SECS" \
      "$DUAL_ROUTE_CLI" build "$rel_path" -o "$out_dir") \
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

check_cli_checked_refusal two-site-first "$first_root" "$first_rel"
check_cli_checked_refusal two-site-second "$second_root" "$second_rel"

for pair in \
    "first-unchecked|source_two_site_first_trap|two-site first trap (site 0)" \
    "second-unchecked|source_two_site_second_trap|two-site second trap (site 1)"; do
  IFS='|' read -r stage_key test_id label <<<"$pair"
  stage="$work/stage/$stage_key"
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

if [ "${CONCRETE_DUAL_ROUTE_SKIP_CONTROLS:-0}" = 1 ]; then
  echo "dual_route_harness: PASS=$pass FAIL=$fail"
  [ "$fail" -eq 0 ]
  exit $?
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
  if [ "$(cat "$ctl_dir/run.reason")" = exit ] &&
      [ "$(cat "$ctl_dir/run.status")" = "$planted_status" ] &&
      [ "$(cat "$ctl_dir/run.signal")" = none ] &&
      [ ! -s "$ctl_dir/run.stdout" ]; then
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
    no "silent object substitution ($label): replay rc=$verify_rc, planted_ok=$planted_ok, $(format_term "$ctl_dir/run")"
  fi
}

swapped_site0_line=$(cat "$work/second-site0.line")
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

# --- 5. Termination-reason and observation judge negative controls ---
sigabrt_num=$(python3 -I -c 'import signal; print(int(signal.SIGABRT))')

# 5a. Native SIGABRT executable: writes the exact site-0 diagnostic and calls abort().
sigabrt_dir="$work/ctl/sigabrt-exe"
if dual_route_bounded "$DUAL_ROUTE_NATIVE_SECS" \
    bash "$DUAL_ROUTE_NATIVE" "$sigabrt_dir" - llvm >"$sigabrt_dir/build.log" 2>&1; then
  dual_route_observe "$sigabrt_dir/llvm.exe" "$sigabrt_dir/run"
  if [ "$(cat "$sigabrt_dir/run.reason")" = signal ] &&
      [ "$(cat "$sigabrt_dir/run.status")" = none ] &&
      [ "$(cat "$sigabrt_dir/run.signal")" = "$sigabrt_num" ] &&
      cmp -s "$work/expect/source_two_site_first_trap/stderr" "$sigabrt_dir/run.stderr" &&
      [ ! -s "$sigabrt_dir/run.stdout" ] &&
      ! dual_route_judge "$work/expect/source_two_site_first_trap" "$sigabrt_dir/run"; then
    ok "SIGABRT control (standalone executable): observed signal $sigabrt_num (status=none, not 134) and rejected by trap judge"
  else
    no "SIGABRT control (standalone executable): $(format_term "$sigabrt_dir/run")"
  fi
else
  cat "$sigabrt_dir/build.log" >&2
  no "SIGABRT control (standalone executable): native build failed"
fi

# 5b. SIGABRT-mutated trap launcher: replaces `_exit(134)` with `abort()` in
#     launcher.ll, passes bound+verify replay, emits exact site-0 stderr, and
#     must still be rejected by the trap judge.
sigabrt_stage="$work/ctl/sigabrt-launcher/stage"
mkdir -p "$work/ctl/sigabrt-launcher"
copy_stage_inputs "$work/stage/first-unchecked" "$sigabrt_stage" llvm
sed 's/call void @_exit(i32 134)/call void @abort()/g' \
  "$work/stage/first-unchecked/launcher.ll" >"$sigabrt_stage/launcher.ll"
if ! cmp -s "$work/stage/first-unchecked/launcher.ll" "$sigabrt_stage/launcher.ll" &&
    dual_route_build_and_verify "$sigabrt_stage" llvm "$work/ctl/sigabrt-launcher/build"; then
  dual_route_observe "$sigabrt_stage/llvm.exe" "$work/ctl/sigabrt-launcher/run"
  if [ "$(cat "$work/ctl/sigabrt-launcher/run.reason")" = signal ] &&
      [ "$(cat "$work/ctl/sigabrt-launcher/run.status")" = none ] &&
      [ "$(cat "$work/ctl/sigabrt-launcher/run.signal")" = "$sigabrt_num" ] &&
      cmp -s "$work/expect/source_two_site_first_trap/stderr" "$work/ctl/sigabrt-launcher/run.stderr" &&
      [ ! -s "$work/ctl/sigabrt-launcher/run.stdout" ] &&
      ! dual_route_judge "$work/expect/source_two_site_first_trap" "$work/ctl/sigabrt-launcher/run"; then
    ok "SIGABRT control (aborting trap launcher): observed signal $sigabrt_num and rejected despite passing replay"
  else
    no "SIGABRT control (aborting trap launcher): $(format_term "$work/ctl/sigabrt-launcher/run")"
  fi
else
  no "SIGABRT control (aborting trap launcher): mutated launcher build/verify setup failed"
fi

# 5c. Forged observation judge negative controls.
judge_ctl="$work/ctl/judge"
mkdir -p "$judge_ctl"
forge_obs() {
  local name=$1
  local reason=$2
  local status=$3
  local sig=$4
  local err_fmt=$5
  local out_text=$6
  local prefix="$judge_ctl/obs-$name"
  printf '%s\n' "$reason" >"$prefix.reason"
  printf '%s\n' "$status" >"$prefix.status"
  printf '%s\n' "$sig" >"$prefix.signal"
  printf "$err_fmt" >"$prefix.stderr"
  printf '%s' "$out_text" >"$prefix.stdout"
  echo "$prefix"
}

exact_trap_line=$(tr -d '\n' <"$work/expect/source_two_site_first_trap/stderr")
wrong_site_line=${exact_trap_line//(site 0)/(site 1)}
if dual_route_judge "$work/expect/source_two_site_first_trap" \
    "$(forge_obs exact exit 134 none "$exact_trap_line\n" '')"; then
  ok "judge accepts exact normal-exit trap-134 stderr observation"
else
  no "judge rejected exact normal-exit trap-134 stderr observation"
fi

for spec in \
    "wrong-status|exit|0|none|$exact_trap_line\n|" \
    "signal-sigabrt|signal|none|$sigabrt_num|$exact_trap_line\n|" \
    "inconsistent-signal-status|signal|134|$sigabrt_num|$exact_trap_line\n|" \
    "inconsistent-exit-signal|exit|134|$sigabrt_num|$exact_trap_line\n|" \
    "status-out-of-range|exit|256|none|$exact_trap_line\n|" \
    "no-newline|exit|134|none|$exact_trap_line|" \
    "extra-newline|exit|134|none|$exact_trap_line\n\n|" \
    "wrong-site|exit|134|none|$wrong_site_line\n|" \
    "stray-stdout|exit|134|none|$exact_trap_line\n|42" \
    "incomplete|incomplete|none|none||" \
    "runner-failed|runner-failed|none|none||"; do
  IFS='|' read -r cname creason cstatus csig cerr cout <<<"$spec"
  if dual_route_judge "$work/expect/source_two_site_first_trap" \
      "$(forge_obs "$cname" "$creason" "$cstatus" "$csig" "$cerr" "$cout")"; then
    no "judge accepted forged observation: $cname"
  else
    ok "judge rejects forged observation: $cname"
  fi
done

printf 'runner-failed\n' >"$judge_ctl/reason"
printf 'none\n' >"$judge_ctl/status"
printf 'none\n' >"$judge_ctl/signal"
: >"$judge_ctl/stderr"
if dual_route_judge "$judge_ctl" "$(forge_obs rf-match runner-failed none none '' '')"; then
  no "judge accepted runner-failed even when expected matched"
else
  ok "judge rejects runner-failed even when expected matches"
fi

# --- 6. Checked-publication negative controls (unchecked lowering behind admission) ---
# 6a. Reviewer's exact mutation: keep checked admission in the CLI, feed
#     unchecked lowering to both backends after admission succeeds, build and
#     verify with real slice0_native.sh, and confirm the full harness FAILS.
mut_cli_dir="$work/ctl/mutated-cli-both"
mkdir -p "$mut_cli_dir"
mut_cli="$mut_cli_dir/concrete-unchecked-after-admission"
{
  printf '#!/usr/bin/env bash\n'
  printf 'set -euo pipefail\n'
  printf 'ROOT=%q\n' "$root"
  printf 'EMITTER=%q\n' "$DUAL_ROUTE_EMITTER_LEAN"
  printf 'NATIVE=%q\n' "$DUAL_ROUTE_NATIVE"
  printf 'LCB=%q\n' "$DUAL_ROUTE_LCB"
  printf 'TARGET=%q\n' "$DUAL_ROUTE_TARGET"
  cat <<'MUTCLI'
[ "$#" = 4 ] && [ "$1" = build ] && [ "$3" = -o ] || { echo "USAGE: mutated cli" >&2; exit 3; }
rel_path=$2
out_dir=$4
src_path="$PWD/$rel_path"
staging=$(mktemp -d "$(dirname -- "$out_dir")/.mut-stage.XXXXXXXX")
trap 'rm -rf -- "$staging"' EXIT
rc=0
(cd -- "$ROOT" && lake env lean --run "$EMITTER" \
  emit-unchecked-after-admission both "$rel_path" "$src_path" "$staging" "$TARGET") || rc=$?
if [ "$rc" -ne 0 ]; then
  exit "$rc"
fi
bash "$NATIVE" bound "$staging" "$LCB" >/dev/null
bash "$NATIVE" verify "$staging" "$LCB" >/dev/null
rm -rf -- "$out_dir"
mv -- "$staging" "$out_dir"
printf '%s\n' 'language-rules version 0.1'
MUTCLI
} >"$mut_cli"
chmod +x "$mut_cli"

answer_expect_status=$(cat "$work/expect/source_answer/status")
mut_rc=0
CONCRETE_DUAL_ROUTE_CLI="$mut_cli" \
  CONCRETE_TEST_MANIFEST="$manifest" \
  CONCRETE_DUAL_ROUTE_SKIP_CONTROLS=1 \
  dual_route_bounded "$DUAL_ROUTE_NATIVE_SECS" \
  bash "$root/scripts/checks/compiler/dual_route_harness.sh" \
  >"$mut_cli_dir/harness.stdout" 2>"$mut_cli_dir/harness.stderr" || mut_rc=$?
if [ "$mut_rc" -ne 0 ] &&
    has_match -F "ok   CLI checked answer [llvm]: verified replay and normal exit $answer_expect_status" <"$mut_cli_dir/harness.stdout" &&
    has_match -F "ok   CLI checked answer [lcb]: verified replay and normal exit $answer_expect_status" <"$mut_cli_dir/harness.stdout" &&
    has_match -F 'ok   CLI checked two-site-first: refused with CORE-CHECK (exit 1, no output)' <"$mut_cli_dir/harness.stdout" &&
    has_match -F 'ok   CLI checked two-site-second: refused with CORE-CHECK (exit 1, no output)' <"$mut_cli_dir/harness.stdout" &&
    has_match -F 'FAIL CLI checked publication or stage discharge verification failed' <"$mut_cli_dir/harness.stdout" &&
    has_match -F 'verify-checked: published component.ll has 1 overflow intrinsic call(s), expected 0' <"$mut_cli_dir/harness.stderr" &&
    has_match -F 'verify-checked: published launcher.ll has 1 trap site(s), expected 0' <"$mut_cli_dir/harness.stderr" &&
    has_match -F 'verify-checked: published lcb_rt.ssa has 1 trap site(s), expected 0' <"$mut_cli_dir/harness.stderr" &&
    has_match -F 'verify-checked: published lcb_body.s differs from independently emitted checked LCB body' <"$mut_cli_dir/harness.stderr"; then
  ok "checked-publication control (unchecked lowering behind checked admission): full harness fails on both LLVM and LCB despite passing admission, replay, and exit $answer_expect_status"
else
  cat "$mut_cli_dir/harness.stdout" >&2 || true
  cat "$mut_cli_dir/harness.stderr" >&2 || true
  no "checked-publication control (unchecked lowering behind checked admission): unexpected result (rc=$mut_rc)"
fi

# 6b & 6c. Single-backend unchecked lowering behind checked admission (LLVM-only and LCB-only).
for mode_spec in \
    "llvm|verify-checked: published component.ll has 1 overflow intrinsic call(s), expected 0" \
    "lcb|verify-checked: published lcb_rt.ssa has 1 trap site(s), expected 0"; do
  IFS='|' read -r mut_mode expected_diag <<<"$mode_spec"
  mode_dir="$work/ctl/mutated-pub-$mut_mode"
  mkdir -p "$mode_dir/pub"
  if dual_route_emit_unchecked_after_admission "$mut_mode" "$answer_rel" "$answer_src" "$mode_dir/pub" \
      >"$mode_dir/emit.log" 2>&1; then
    vrc=0
    dual_route_verify_checked "$answer_rel" "$answer_src" "$mode_dir/pub" both \
      >"$mode_dir/verify.stdout" 2>"$mode_dir/verify.stderr" || vrc=$?
    if [ "$vrc" -ne 0 ] && has_match -F "$expected_diag" <"$mode_dir/verify.stderr"; then
      ok "checked-publication control ($mut_mode-only unchecked behind admission): rejected with '$expected_diag'"
    else
      cat "$mode_dir/verify.stderr" >&2 || true
      no "checked-publication control ($mut_mode-only unchecked behind admission): expected '$expected_diag' (rc=$vrc)"
    fi
  else
    cat "$mode_dir/emit.log" >&2 || true
    no "checked-publication control ($mut_mode-only): failed to emit mutated publication"
  fi
done

# --- 7. Manifest source-resolution and outside-source negative controls ---
# 7a. 20 + 21 control: copy the manifest's tests/, edit answer.con to 20 + 21
#     while keeping expectation exit 42 -, and confirm the full harness FAILS.
ctl_20_21="$work/ctl/manifest-20-plus-21"
mkdir -p "$ctl_20_21/project"
cp -R -- "$manifest_tests_dir" "$ctl_20_21/project/tests"
printf 'fn main() -> u8 { return 20 + 21; }\n' >"$ctl_20_21/project/$answer_rel"
if ! cmp -s "$answer_src" "$ctl_20_21/project/$answer_rel"; then
  rc_20_21=0
  CONCRETE_TEST_MANIFEST="$ctl_20_21/project/tests/manifest.tsv" \
    CONCRETE_DUAL_ROUTE_SKIP_CONTROLS=1 \
    dual_route_bounded "$DUAL_ROUTE_NATIVE_SECS" \
    bash "$root/scripts/checks/compiler/dual_route_harness.sh" \
    >"$ctl_20_21/harness.stdout" 2>"$ctl_20_21/harness.stderr" || rc_20_21=$?
  if [ "$rc_20_21" -ne 0 ] &&
      has_match -F "FAIL CLI checked answer [llvm]: got reason=exit status=41 signal=none, expected reason=exit status=$answer_expect_status signal=none" <"$ctl_20_21/harness.stdout" &&
      has_match -F "FAIL CLI checked answer [lcb]: got reason=exit status=41 signal=none, expected reason=exit status=$answer_expect_status signal=none" <"$ctl_20_21/harness.stdout" &&
      has_match -F "FAIL checked answer [llvm]: got reason=exit status=41 signal=none, expected reason=exit status=$answer_expect_status signal=none" <"$ctl_20_21/harness.stdout" &&
      has_match -F "FAIL unchecked answer [lcb]: got reason=exit status=41 signal=none, expected reason=exit status=$answer_expect_status signal=none" <"$ctl_20_21/harness.stdout"; then
    ok "manifest control (20 + 21 in copied manifest): full harness runs copied source (status 41) and fails expectation $answer_expect_status on both backends"
  else
    cat "$ctl_20_21/harness.stdout" >&2 || true
    cat "$ctl_20_21/harness.stderr" >&2 || true
    no "manifest control (20 + 21 in copied manifest): unexpected result (rc=$rc_20_21)"
  fi
else
  no "manifest control (20 + 21): mutation did not change $answer_rel"
fi

# 7b-7f. Refuse any manifest row whose source is outside the selected manifest.
manifest_refusal_control() {
  local label=$1
  local target_manifest=$2
  local expected_err=$3
  local log_dir="$work/ctl/manifest-refuse-$label"
  local mrc=0
  mkdir -p "$log_dir"
  CONCRETE_TEST_MANIFEST="$target_manifest" \
    CONCRETE_DUAL_ROUTE_SKIP_CONTROLS=1 \
    dual_route_bounded 60 \
    bash "$root/scripts/checks/compiler/dual_route_harness.sh" \
    >"$log_dir/harness.stdout" 2>"$log_dir/harness.stderr" || mrc=$?
  if [ "$mrc" -ne 0 ] && has_match -F "$expected_err" <"$log_dir/harness.stderr"; then
    ok "manifest refusal control ($label): refused with '$expected_err'"
  else
    cat "$log_dir/harness.stdout" >&2 || true
    cat "$log_dir/harness.stderr" >&2 || true
    no "manifest refusal control ($label): expected '$expected_err' (rc=$mrc)"
  fi
}

# 7b. Symlinked source file pointing outside copied manifest to original checkout.
ctl_sym_src="$work/ctl/manifest-symlink-src"
mkdir -p "$ctl_sym_src/project"
cp -R -- "$manifest_tests_dir" "$ctl_sym_src/project/tests"
rm -f -- "$ctl_sym_src/project/$answer_rel"
ln -s -- "$answer_src" "$ctl_sym_src/project/$answer_rel"
manifest_refusal_control symlink-source-outside "$ctl_sym_src/project/tests/manifest.tsv" \
  'must not be or traverse a symlink'

# 7c. Path traversal (`..`) escaping tests/ to an outside .con file.
ctl_trav="$work/ctl/manifest-traversal"
mkdir -p "$ctl_trav/project"
cp -R -- "$manifest_tests_dir" "$ctl_trav/project/tests"
cp -- "$answer_src" "$ctl_trav/project/outside.con"
awk -F '\t' -v OFS='\t' '$1 == "source_answer" { $2 = "con:tests/../outside.con" } { print }' \
  "$ctl_trav/project/tests/manifest.tsv" >"$ctl_trav/project/tests/manifest.tsv.new"
mv -- "$ctl_trav/project/tests/manifest.tsv.new" "$ctl_trav/project/tests/manifest.tsv"
manifest_refusal_control traversal-outside "$ctl_trav/project/tests/manifest.tsv" \
  'must be a normalized tests/*.con path'

# 7d. Symlinked tests/ directory pointing to original checkout's tests/.
ctl_sym_dir="$work/ctl/manifest-symlink-dir"
mkdir -p "$ctl_sym_dir/project"
ln -s -- "$manifest_tests_dir" "$ctl_sym_dir/project/tests"
manifest_refusal_control symlink-tests-dir "$ctl_sym_dir/project/tests/manifest.tsv" \
  'must not be a symlink'

# 7e. Manifest residing in a directory not named tests/.
ctl_wrong_dir="$work/ctl/manifest-wrong-dir"
mkdir -p "$ctl_wrong_dir/project"
cp -R -- "$manifest_tests_dir" "$ctl_wrong_dir/project/not_tests"
manifest_refusal_control non-tests-dir "$ctl_wrong_dir/project/not_tests/manifest.tsv" \
  "must be named 'tests'"

# 7f. Non-con: row (bir:) substituted for source_answer.
ctl_non_con="$work/ctl/manifest-non-con"
mkdir -p "$ctl_non_con/project"
cp -R -- "$manifest_tests_dir" "$ctl_non_con/project/tests"
awk -F '\t' -v OFS='\t' '$1 == "answer" { next } $1 == "source_answer" { $2 = "bir:answer" } { print }' \
  "$ctl_non_con/project/tests/manifest.tsv" >"$ctl_non_con/project/tests/manifest.tsv.new"
mv -- "$ctl_non_con/project/tests/manifest.tsv.new" "$ctl_non_con/project/tests/manifest.tsv"
manifest_refusal_control non-con-row "$ctl_non_con/project/tests/manifest.tsv" \
  "must be a con: source"

echo "dual_route_harness: PASS=$pass FAIL=$fail"
[ "$fail" -eq 0 ]
