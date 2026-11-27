#!/usr/bin/env bash
# Manifest-driven slice-0 test gate (Concrete/README.md gates 4 and 6).
# Hand-written BIR and accepted or unchecked source go through the LLVM path (clang) and the LCB path: the
# adapted body assembled by LCB in process (no text on the proved path), linked with the runtime
# entry, which LCB's command line assembles from QBE text. The manifest's bir: inventory must equal
# the emitter's list in both directions. CONCRETE_TEST_MANIFEST selects another manifest (controls).
# Each executable runs under bounded_run; its observation is its exit status (or "incomplete"),
# the exact bytes on descriptor 2, and descriptor 1, which must be empty. All three must equal the
# manifest expectation checked by the emitter (which writes the expected status and stderr bytes).
#
# Planted controls exercise typed observation mismatches, manifest inventory and exact byte
# comparison. The two native runtime controls exercise divergence and invalid result words.
set -euo pipefail
# shellcheck source=scripts/checks/lib/slice0_common.sh
source scripts/checks/lib/slice0_common.sh
# shellcheck source=scripts/checks/lib/has_match.sh
source scripts/checks/lib/has_match.sh

# The trap row exercises exactly the expression masked by the reserved-binder refusal.
fixture_dir=$(dirname "$test_manifest")
sed 's/\bOption\b/number/g' "$fixture_dir/slice0/reserved_binder_trap.con" >"$work/legal-binder-trap.con"
diff -u "$work/legal-binder-trap.con" "$fixture_dir/slice0/legal_binder_trap.con"

# Lake builds each control's imports from its header. For executable controls,
# Lean also needs the file as the argument to --run.
# Emit-only runs validate fixture expectations without building native executables.
if [[ ${CONCRETE_TEST_EMIT_ONLY:-0} != 1 ]]; then
  build rule-registry "$work/rule-registry.log" bounded 1800 \
    lake lean scripts/checks/compiler/rule_registry.lean
  echo "ok   slice-0 rule references and layout controls"
  build local-binders "$work/local-binders.log" bounded 120 \
    lake lean scripts/checks/compiler/local_binders.lean -- --run scripts/checks/compiler/local_binders.lean
  echo "ok   builtin type and trait binder refusals through lexing, parsing and elaboration"
  # Exercise one golden in independent process environments, with distinct cwd and TMPDIR.
  encoding_control="$root/scripts/checks/compiler/artifact_encoding.lean"
  for encoding_env in utc pacific; do
    encoding_dir="$work/artifact-$encoding_env"
    mkdir -p "$encoding_dir/tmp"
    if [[ $encoding_env == utc ]]; then
      encoding_tz=UTC0
    else
      encoding_tz=PST8
    fi
    build "artifact encoding ($encoding_env)" "$encoding_dir/vectors" bounded 120 \
      env "TMPDIR=$encoding_dir/tmp" "TZ=$encoding_tz" bash -c \
      'cd "$1" && exec lake -d "$2" lean "$3" -- --run "$3"' _ \
      "$encoding_dir" "$root" "$encoding_control"
  done
  test -s "$work/artifact-utc/vectors"
  cmp "$work/artifact-utc/vectors" "$work/artifact-pacific/vectors"
  echo "ok   canonical artifact bytes and SHA-256 goldens in two environments, payload substitutions"
  bash scripts/checks/compiler/f31_census.sh
  # Lake reuses each topic's checked olean. Allow for a cold native runner.
  stage_controls=(python3 -I "$root/scripts/checks/compiler/stage_controls.py")
  build stage-interface-controls "$work/stage-interface-controls.log" bounded 360 \
    "${stage_controls[@]}"
  # Use the real configuration for both the developer build and this CI gate.
  probe="$work/stage-interface-plant"
  mkdir -p "$probe/ConcreteChecks/StageInterfaces/Frontend" "$probe/.lake"
  cp lakefile.toml lake-manifest.json lean-toolchain "$probe/"
  ln -s "$root/.lake/packages" "$probe/.lake/packages"
  (
    cd "$probe"
    touch Concrete.lean ConcreteSpec.lean
    root_module=ConcreteChecks/StageInterfaces.lean
    good='example : (0 : Nat) = 0 := rfl'
    echo "$good" >"$root_module"
    build stage-interface-plant-baseline "$work/plant-baseline.log" bounded 60 "${stage_controls[@]}"
    reject_false_control() {
      local module=$1
      local rc=0
      shift
      bounded 60 "$@" >"$work/plant.log" 2>&1 || rc=$?
      if [[ $rc != 1 ]] || ! has_match -F "$module:1:" <"$work/plant.log" ||
          ! has_match -F '0 = 1' <"$work/plant.log"; then
        cat "$work/plant.log" >&2
        echo "FAIL $*: expected a false equality in $module" >&2
        exit 1
      fi
    }
    # Change the cached root, then add fresh, unimported sources at two depths.
    for module in "$root_module" ConcreteChecks/StageInterfaces/Frontend/Planted.lean ConcreteChecks/Unlisted.lean; do
      echo 'example : (0 : Nat) = 1 := rfl' >"$module"
      reject_false_control "$module" lake build
      reject_false_control "$module" "${stage_controls[@]}"
      echo "$good" >"$module"
    done
    # Restore the old glob after caching all three successes. Even with valid
    # oleans present, the inventory must identify both files the target skips.
    build stage-interface-plant-repaired "$work/plant-repaired.log" bounded 60 "${stage_controls[@]}"
    sed -i 's/"ConcreteChecks\.+"/"ConcreteChecks.StageInterfaces.+"/' lakefile.toml
    rc=0
    bounded 60 "${stage_controls[@]}" >"$work/plant.log" 2>&1 || rc=$?
    if [[ $rc != 1 ]] || ! has_match -Fx "missing from ConcreteChecks.StageInterfaces: $root_module" <"$work/plant.log" ||
        ! has_match -Fx 'missing from ConcreteChecks.StageInterfaces: ConcreteChecks/Unlisted.lean' <"$work/plant.log"; then
      cat "$work/plant.log" >&2
      echo "FAIL stage-interface inventory: expected the old glob to omit the root and sibling" >&2
      exit 1
    fi
  )
  echo "ok   default and native builds reject false stage controls in the root, a new nested file and a sibling"
  echo "ok   stage-interface inventory detects omissions with a warm cache"
  build stage-end-to-end-audit "$work/stage-end-to-end-audit.log" bounded 60 \
    lake lean scripts/checks/compiler/stage_end_to_end_audit.lean -- -DwarningAsError=true -DautoImplicit=false
  missing_import="$work/missing_import.lean"
  printf '%s\n' 'import ConcreteSpec.NoSuchControlModule' >"$missing_import"
  missing_rc=0
  bounded 60 lake lean "$missing_import" >"$work/missing_import.log" 2>&1 || missing_rc=$?
  if [[ $missing_rc != 1 ]] ||
      ! has_match -F "bad import 'ConcreteSpec.NoSuchControlModule'" <"$work/missing_import.log"; then
    cat "$work/missing_import.log" >&2
    echo "FAIL stage interface controls: expected a missing import diagnostic" >&2
    exit 1
  fi
  echo "ok   Lean control rejects a nonexistent import"
  echo "ok   stage interface controls and end-to-end semantic parameter audit"
  build slice1-driver-controls "$work/slice1-driver-controls.log" bounded 120 \
    lake lean scripts/checks/compiler/slice1_driver.lean -- --run scripts/checks/compiler/slice1_driver.lean "$work/slice1-link"
  for case_name in answer trap; do
    case_dir="$work/slice1-link/$case_name"
    build "split LLVM $case_name" "$case_dir/build.log" bounded "$native_secs" \
      bash "$root/scripts/checks/lib/slice0_native.sh" bound "$case_dir" - llvm
    observe "$case_dir/llvm.exe" "$case_dir/native" 10 20
    test ! -s "$case_dir/native.stdout"
    if [ "$case_name" = answer ]; then
      test "$(cat "$case_dir/native.status")" = 42
      test ! -s "$case_dir/native.stderr"
    else
      test "$(cat "$case_dir/native.status")" = 134
      printf '%s\n' 'concrete: trap: overflow at tests/slice0/sum.con:3:12 (site 0)' |
        cmp -s - "$case_dir/native.stderr"
    fi
  done
  echo "ok   slice 1 driver controls"
  bounded 1800 lake lean scripts/checks/compiler/span_scaling.lean -- --run scripts/checks/compiler/span_scaling.lean
  build lexer-digit-limit "$work/lexer-digit-limit.log" bounded 120 \
    lake lean scripts/checks/compiler/lexer_digit_limit.lean -- --run scripts/checks/compiler/lexer_digit_limit.lean
  echo "ok   lexer digit limit controls"
  build lexer-stream "$work/lexer-stream.log" bounded 120 \
    lake lean scripts/checks/compiler/lexer_stream.lean -- --run scripts/checks/compiler/lexer_stream.lean
  echo "ok   lexer stream controls"
  build llvm-mutation-controls "$work/llvm-mutation-controls.log" bounded 120 \
    lake env python3 -B scripts/checks/compiler/llvm_mutation_controls.py
  cat "$work/llvm-mutation-controls.log"
  build memory-exhaustion-mutation "$work/memory-exhaustion.log" bounded 300 \
    python3 -B scripts/checks/compiler/memory_exhaustion_mutation.py
  cat "$work/memory-exhaustion.log"
fi

fail=0
count=0
examples=0
# check NAME DIR SECS RETRY: build both backends in DIR, run them and judge them.
check() {
  local name=$1
  local d=$2
  local backend
  build "$name" "$d/build.log" bounded "$native_secs" bash "$root/scripts/checks/lib/slice0_native.sh" "$d" "$lcb"
  for backend in llvm lcb; do
    observe "$d/$backend.exe" "$d/$backend" "$3" "$4"
    count=$((count + 1))
    if judge "$d" "$d/$backend"; then
      echo "ok   $name [$backend]: status $(cat "$d/$backend.status")"
    else
      echo "FAIL $name [$backend]: status $(cat "$d/$backend.status") (expected $(cat "$d/status"))," \
        "stderr $(od -An -c "$d/$backend.stderr" | tr -s ' ' | head -c 300)," \
        "stdout $(wc -c <"$d/$backend.stdout") bytes"
      fail=1
    fi
  done
}

# Check a diagnostic through the public CLI, including its process status.
check_cli_diagnostic() {
  local label=$1
  local output=$2
  local expectation=$3
  shift 3
  local version_report=
  if [ "$1" = --reports-version ]; then
    version_report=$'language-rules version 0.1\n'
    shift
  fi
  local status code message
  local rc=0
  read -r _ status code message <<<"$expectation"
  bounded "$native_secs" "$@" >"$output.stdout" 2>"$output.stderr" || rc=$?
  count=$((count + 1))
  local -a expected=(has_match -E "^$code: .+$")
  if [ -n "$message" ]; then
    expected=(cmp -s "$output.expected" -)
    printf '%s: %s\n' "$code" "$message" >"$output.expected"
  fi
  if [ "$rc" = "$status" ] && printf '%s' "$version_report" | cmp -s - "$output.stdout" &&
      "${expected[@]}" <"$output.stderr"; then
    echo "ok   $label: status $rc ($code)"
  else
    echo "FAIL $label: expected status $status and $code, got status $rc"
    cat "$output.stderr"
    fail=1
  fi
}

# Redirect inside the watchdog so the failing descriptor belongs to the CLI.
check_cli_output_failure() {
  local label=$1
  local fd=$2
  shift 2
  local rc=0
  local diagnostic="$work/ctl/$label.stderr"
  bounded "$native_secs" bash -c '
    fd=$1
    shift
    if [ -c /dev/full ]; then
      exec 3>/dev/full
    else
      exec 3</dev/null
    fi
    if [ "$fd" = 1 ]; then
      exec "$@" 1>&3 3>&-
    else
      exec "$@" 2>&3 3>&-
    fi
  ' _ "$fd" "$@" >"$work/ctl/$label.stdout" 2>"$diagnostic" || rc=$?
  count=$((count + 1))
  if [ "$rc" = 2 ] && {
    if [ "$fd" = 1 ]; then
      has_match -E '^TOOL-FAILURE: incomplete \(write stdout: .+\)$' <"$diagnostic"
    else
      [ ! -s "$diagnostic" ]
    fi
  }; then
    echo "ok   $label: status $rc"
  else
    echo "FAIL $label: expected output failure on fd $fd with status 2, got status $rc"
    cat "$diagnostic"
    fail=1
  fi
}

cli_bin="$root/.lake/build/bin/concrete"
cli_project=$(cd "$(dirname "$test_manifest")/.." && pwd)
project_cli=(bash -c 'cd "$1" && shift && exec "$@"' _ "$cli_project" "$cli_bin")

# Mirror the Lake binary layout when a control needs installation-local fault injection.
private_cli_installation() {
  local installation=$1
  local lib="$installation/scripts/checks/lib"
  mkdir -p "$installation/.lake/build/bin" "$lib"
  cp "$cli_bin" "$installation/.lake/build/bin/concrete"
  ln -s "$root/.lake/packages" "$installation/.lake/packages"
  cp "$root/scripts/checks/lib/"slice0_{write.py,publish.py,native.sh} "$lib/"
}

check_cli_case() {
  local name=$1
  local program=$2
  local d=$3
  local expect=$4
  local source=tests/slice0/answer.con
  local backend=lcb
  local -a cli=("${project_cli[@]}")
  mkdir -p "$d"
  case "$program" in
    cli:invalid-backend) backend=invalid ;;
    cli:invalid-path) source="./$source" ;;
    cli:unknown-target|cli:native-helper-failure|cli:build-input-write-failure)
      local project="$d/project"
      local installation="$d/installation"
      local helper="$installation/scripts/checks/lib/slice0_native.sh"
      mkdir -p "$project/tests/slice0"
      cp "$root/$source" "$project/$source"
      private_cli_installation "$installation"
      cli=(bash -c 'cd "$1" && shift && exec "$@"' _ "$project" "$installation/.lake/build/bin/concrete")
      if [ "$program" = cli:unknown-target ]; then
        printf '%s\n' '[ "$1" = target ] || exit 99' 'echo invalid-target' >"$helper"
      elif [ "$program" = cli:build-input-write-failure ]; then
        backend=llvm
        mkdir -p "$d/out"
        local device=/dev/full
        if [ ! -c "$device" ]; then device=/dev/null; fi
        ln -s "$device" "$d/out/component.ll"
        printf '%s\n' ': > helper-invoked' >"$helper"
      else
        printf '%s\n' "if [ \"\$1\" = target ]; then echo '$target'; exit; fi" \
          'printf partial >"$2/lcb.exe"' 'echo "native helper control" >&2' 'exit 17' >"$helper"
      fi
      ;;
    *) echo "FAIL $name: unknown CLI case $program" >&2; fail=1; return ;;
  esac
  check_cli_diagnostic "$name" "$d/cli" "$expect" "${cli[@]}" \
    build "$source" -o "$d/out" --backend "$backend"
  if [ "$program" = cli:native-helper-failure ] && [ -e "$d/out" ]; then
    echo "FAIL $name: failed native build left files in the output directory"
    fail=1
  fi
  if [ "$program" = cli:build-input-write-failure ]; then
    if [ ! -L "$d/out/component.ll" ] || [ "$(readlink "$d/out/component.ll")" != "$device" ]; then
      echo "FAIL $name: build changed the destination symlink"
      fail=1
    fi

    # Force an actual staged file write to fail. Capture the CLI's streams in
    # pipes so the file-size limit applies to build files and not the diagnostic.
    check_cli_diagnostic "$name (staged write)" "$d/staged" "$expect" python3 -c '
import resource
import signal
import subprocess
import sys

def limit_file_size():
    signal.signal(signal.SIGXFSZ, signal.SIG_IGN)
    resource.setrlimit(resource.RLIMIT_FSIZE, (0, 0))

result = subprocess.run(sys.argv[1:], preexec_fn=limit_file_size, capture_output=True)
sys.stdout.buffer.write(result.stdout)
sys.stderr.buffer.write(result.stderr)
sys.exit(result.returncode)
' "${cli[@]}" build "$source" -o "$d/staged-out" --backend "$backend"
    if [ -e "$d/project/helper-invoked" ]; then
      echo "FAIL $name: native build helper ran after a build output failure"
      fail=1
    fi
    if ! has_match -E '^TOOL-FAILURE: incomplete \(write .*/component\.ll: checked writer exited' <"$d/staged.stderr" ||
        [ -e "$d/staged-out" ] ||
        [ -n "$(find "$d" -maxdepth 1 -name '.concrete-build.*' -print)" ]; then
      echo "FAIL $name: staged write failure did not identify the failed input and clean up"
      fail=1
    fi
  fi
}

# The public status and exact bytes are pinned independently of Refusal rendering.
cli_resource_refused() {
  local dir=$1
  [ "$2" = 2 ] && [ ! -s "$dir/lcb.stdout" ] &&
    [ ! -e "$dir/lcb" ] && [ ! -L "$dir/lcb" ] &&
    cmp -s "$3" "$dir/lcb.stderr"
}

# The backend-specific expectation exercises the public driver and its native build policy.
check_backend_limit() {
  local path=${1#con:}
  local d=$2
  local expect=$3
  local status resource bound measured
  local project
  local -a cli
  project=$cli_project
  if [ "$project" != "$root" ]; then
    mkdir -p "$d/project/$(dirname "$path")"
    cp -- "$project/$path" "$d/project/$path"
    project="$d/project"
  fi
  cli=(bash -c 'cd "$1" && shift && exec "$@"' _ "$project" "$cli_bin")
  read -r _ _ status _ _ resource bound measured <<<"$expect"
  mkdir -p "$d/expected"
  printf 'BACKEND-LIMIT: incomplete (LCB backend: %s: %s exceeds limit %s)\n' \
    "$resource" "$measured" "$bound" >"$d/lcb-expected.stderr"
  printf '%s\n' "$status" >"$d/expected/status"
  : >"$d/expected/stderr"
  build "$path [llvm]" "$d/llvm-build.log" bounded "$native_secs" "${cli[@]}" \
    build "$path" -o "$d/llvm" --backend llvm
  observe "$d/llvm/llvm.exe" "$d/llvm-run" 10 20
  count=$((count + 1))
  if judge "$d/expected" "$d/llvm-run"; then
    echo "ok   $path [llvm]: status $status"
  else
    echo "FAIL $path [llvm]: expected status $status and empty streams"
    fail=1
  fi

  local rc=0
  bounded "$native_secs" "${cli[@]}" \
    build "$path" -o "$d/lcb" --backend lcb \
    >"$d/lcb.stdout" 2>"$d/lcb.stderr" || rc=$?
  count=$((count + 1))
  if cli_resource_refused "$d" "$rc" "$d/lcb-expected.stderr"; then
    echo "ok   $path [lcb]: $(cat "$d/lcb.stderr")"
  else
    echo "FAIL $path [lcb]: expected LCB CLI refusal without published output, got status $rc"
    cat "$d/lcb.stderr"
    fail=1
  fi
  if [ "${CONCRETE_TEST_MANIFEST_SKIP_CONTROLS:-0}" != 1 ]; then
    local control="$d/cli-wrong-code"
    mkdir -p "$control"
    cp "$d/lcb.stdout" "$control/lcb.stdout"
    sed 's/^BACKEND-LIMIT:/CORE-CHECK:/' "$d/lcb.stderr" >"$control/lcb.stderr"
    if [ "$rc" = 2 ] && ! cmp -s "$d/lcb.stderr" "$control/lcb.stderr" &&
        ! cli_resource_refused "$control" "$rc" "$d/lcb-expected.stderr"; then
      echo "ok   CLI control: wrong public resource code rejected"
    else
      echo "FAIL CLI control: wrong public resource code was not rejected"
      fail=1
    fi

    control="$d/cli-wrong-refusal"
    mkdir -p "$control"
    rc=0
    bounded "$native_secs" "${cli[@]}" build "./$path" -o "$control/lcb" --backend lcb \
      >"$control/lcb.stdout" 2>"$control/lcb.stderr" || rc=$?
    if [ "$rc" = 3 ] && [ ! -s "$control/lcb.stdout" ] && [ ! -e "$control/lcb" ] &&
        has_match -Fx 'USAGE: expected a project-relative .con path' <"$control/lcb.stderr" &&
        ! cli_resource_refused "$control" "$rc" "$d/lcb-expected.stderr"; then
      echo "ok   CLI control: unrelated refusal rejected"
    else
      echo "FAIL CLI control: expected rejection of an unrelated usage refusal"
      fail=1
    fi
  fi
}

rows=$(manifest_rows)

mutate_field() {
  awk -F '\t' -v OFS='\t' -v id="$2" -v index_to_change="$3" -v value="$4" '
    $1 == id {
      n = split($3, fields, " ")
      fields[index_to_change] = value
      $3 = fields[1]
      for (i = 2; i <= n; i++) $3 = $3 " " fields[i]
    }
    {print}
  ' "$1" >"$1.new"
  mv "$1.new" "$1"
}
missing_row() { awk -F '\t' '$1 != "answer"' "$1" >"$1.new"; mv "$1.new" "$1"; }
nonexistent_program() { printf 'no_such_program\tbir:no_such_program\texit 0 -\tslice0\n' >>"$1"; }

manifest_control() {
  local label=$1
  local expected_status=$2
  local mutation=$3
  local row_id=$4
  local inventory_message=${5:-}
  shift 5
  local selected program expect
  local dir="$work/ctl/$label"
  local copy
  local rc=0
  mkdir -p "$dir"
  cp -R "$(dirname "$test_manifest")" "$dir/tests"
  copy="$dir/tests/$(basename "$test_manifest")"
  "$mutation" "$copy" "$row_id" "$@"
  if cmp -s "$test_manifest" "$copy"; then
    echo "FAIL manifest control $label: mutation changed nothing" >&2; fail=1; return
  fi
  if [ -n "$inventory_message" ]; then
    (test_manifest=$copy; manifest_rows) >"$work/ctl/$label.log" 2>&1 || rc=$?
  else
    selected=$(bounded 30 python3 -I "$root/scripts/checks/lib/test_manifest.py" rows "$copy" |
      awk -F '\t' -v id="$row_id" '$1 == id') || { fail=1; return; }
    if [ -z "$selected" ]; then
      echo "FAIL manifest control $label: missing mutated row $row_id" >&2
      fail=1
      return
    fi
    IFS=$'\t' read -r _ program expect <<<"$selected"
    emit_slice0_row "$copy" "$program" "$dir/out" "$target" "$expect" \
      >"$work/ctl/$label.log" 2>&1 || rc=$?
  fi
  if [ "$rc" = "$expected_status" ] &&
      { [ -z "$inventory_message" ] || has_match -Fx "$inventory_message" <"$work/ctl/$label.log"; }; then
    echo "ok   manifest control $label: refused for the planted reason"
  else
    echo "FAIL manifest control $label: expected status $expected_status, got $rc" >&2
    cat "$work/ctl/$label.log" >&2
    fail=1
  fi
}
mkdir -p "$work/ctl"
if [ "${CONCRETE_TEST_MANIFEST_SKIP_CONTROLS:-0}" != 1 ]; then
  manifest_control wrong-expectation 10 mutate_field answer "" 2 41
  manifest_control wrong-bir-exit-stderr 16 mutate_field answer "" 3 0a
  manifest_control wrong-source-exit-stderr 16 mutate_field source_answer "" 3 0a
  manifest_control wrong-source-exit-status 10 mutate_field source_answer "" 2 41
  for id in overflow source_trap; do
    manifest_control "$id-wrong-trap-site" 11 mutate_field "$id" "" 3 2
    manifest_control "$id-wrong-trap-kind" 12 mutate_field "$id" "" 2 division_by_zero
    manifest_control "$id-wrong-trap-stderr" 16 mutate_field "$id" "" 4 0a
  done
  manifest_control missing-row 1 missing_row "" "slice0: program without a manifest row: answer"
  manifest_control nonexistent-program 1 nonexistent_program "" "slice0: manifest row names no program: no_such_program"
  manifest_control wrong-source-refusal 13 mutate_field source_syntax "" 3 checker
  manifest_control wrong-rule-id 13 mutate_field source_after_return "" 4 STMT-15@1
  manifest_control wrong-rule-revision 13 mutate_field source_after_return "" 4 STMT-14@2
  manifest_control wrong-primary-origin 14 mutate_field source_after_return "" 5 18:27:1:19
  manifest_control wrong-related-origin 14 mutate_field source_after_return "" 6 anchor=18:27:1:19
  manifest_control wrong-resource-name 15 mutate_field source_lcb_value_limit "" 6 blocks
  manifest_control wrong-resource-limit 15 mutate_field source_lcb_value_limit "" 7 489
  manifest_control wrong-resource-measurement 15 mutate_field source_lcb_value_limit "" 8 1925

  while IFS=$'\t' read -r name _ expect; do
    case "$name" in site0|site1) ;; *) continue ;; esac
    read -r -a trap_expectation <<<"$expect"
    rc=0
    "${emitter[@]}" control-reordered-traps "$name" "$work/ctl/reordered-$name" "$target" \
      "${trap_expectation[@]}" || rc=$?
    if [ "$rc" = 11 ]; then
      echo "ok   $name: reordered trap table rejected"
    else
      echo "FAIL $name: reordered trap table returned $rc, expected 11"
      fail=1
    fi
  done <<<"$rows"

  rc=0
  "${emitter[@]}" control-wrong-compiler-origin tests/slice0/after_return.con \
    "$fixture_dir/slice0/after_return.con" || rc=$?
  if [ "$rc" = 18 ]; then
    echo "ok   compiler control: wrong production origin rejected"
  else
    echo "FAIL compiler control: expected wrong production origin status 18, got $rc"
    fail=1
  fi
  rc=0
  "${emitter[@]}" control-unsupported-lcb-profile || rc=$?
  if [ "$rc" = 17 ]; then
    echo "ok   unsupported LCB profile is distinct from a resource mismatch"
  else
    echo "FAIL unsupported LCB profile: expected status 17, got $rc"
    fail=1
  fi
  rc=0
  "${emitter[@]}" emit answer "$work/ctl/unsupported" unavailable-target exit 42 - || rc=$?
  if [ "$rc" = 17 ]; then
    echo "ok   unsupported target is distinct from schema errors and observation mismatches"
  else
    echo "FAIL unsupported target: expected status 17, got $rc"
    fail=1
  fi
fi

if [ "${CONCRETE_TEST_EMIT_ONLY:-0}" != 1 ]; then
  build "concrete driver" "$work/cli-build.log" bounded 1800 lake build concrete
  project_dir="$work/project-control"
  mkdir -p "$project_dir"
  printf 'fn main() -> u8 { return 42; }\n' >"$project_dir/answer.con"
  cli=(bash -c 'cd "$1" && shift && exec "$@"' _ "$project_dir" "$root/.lake/build/bin/concrete")
  for manifest_case in schema malformed depth size encoding directory symlink; do
    project_file="$project_dir/Concrete.toml"
    case "$manifest_case" in
      schema)
        printf '[policy]\n[assumptions]\n' >"$project_file"
        expected='Concrete.toml requires an approved manifest schema' ;;
      malformed)
        printf 'a = "open\n' >"$project_file"
        expected='Concrete.toml is malformed' ;;
      depth)
        printf '[a.b.c.d.e.f.g.h.i.j.k.l.m.n.o.p]\nx = true\n' >"$project_file"
        expected='Concrete.toml exceeds the nesting limit of 16' ;;
      size)
        printf '%65537s' '' >"$project_file"
        expected='Concrete.toml exceeds 65536 bytes' ;;
      encoding)
        printf '\377' >"$project_file"
        expected='Concrete.toml must be UTF-8' ;;
      directory)
        mkdir "$project_file"
        expected='cannot read Concrete.toml (expected a regular file)' ;;
      symlink)
        printf '[policy]\n' >"$project_dir/other.toml"
        ln -s other.toml "$project_file"
        expected='cannot read Concrete.toml (expected a regular file)' ;;
    esac
    rule=
    case "$manifest_case" in
      malformed) rule=SyntaxAccepted ;;
      depth) rule=DepthAccepted ;;
      size) rule=SizeAccepted ;;
      encoding) rule=EncodingAccepted ;;
    esac
    if [ -n "$rule" ]; then expected+=" [rule ConcreteSpec.Project.Manifest.$rule@1]"; fi
    case "$manifest_case" in
      directory|symlink) code=READ ;;
      malformed) code=SYNTAX ;;
      *) code=${manifest_case^^} ;;
    esac
    rc=0
    bounded "$native_secs" "${cli[@]}" build answer.con -o output/ --backend llvm \
      >"$project_dir/stdout" 2>"$project_dir/stderr" || rc=$?
    test "$rc" = 1
    test ! -e "$project_dir/output"
    test ! -s "$project_dir/stdout"
    has_match -Fx "MANIFEST-$code: $expected" <"$project_dir/stderr"
    if [ "$manifest_case" = directory ]; then rmdir "$project_file"; else rm -- "$project_file"; fi
  done
  echo "ok   malformed, over-limit, non-file and pending-schema inputs refuse before output"
  check_bound_build() {
    local dest=$1
    shift
    build "standalone bound launcher ($dest)" "$project_dir/$dest.log" bounded "$native_secs" "${cli[@]}" \
      build answer.con -o "$dest/" "$@"
    has_match -Fx 'language-rules version 0.1' <"$project_dir/$dest.log"
  }
  check_bound_image() {
    local dest=$1
    local backend=$2
    (cd "$project_dir/$dest" && sha256sum -c "$backend.link.sha256")
    has_match -Fx 'Runtime loader and system libraries are unbound environment premises.' \
      <"$project_dir/$dest/runtime-premises.txt"
    observe "$project_dir/$dest/$backend.exe" "$project_dir/native" 10 20
    test "$(cat "$project_dir/native.status")" = 42
    test ! -s "$project_dir/native.stdout"
    test ! -s "$project_dir/native.stderr"
  }
  check_bound_build output
  check_bound_image output llvm
  check_bound_image output lcb
  mkdir -p "$project_dir/scripts/checks/lib" "$project_dir/.lake/packages/lean-compiler-backend/.lake/build/bin"
  for local_tool in scripts/checks/lib/slice0_native.sh .lake/packages/lean-compiler-backend/.lake/build/bin/lcb; do
    printf '#!/usr/bin/env bash\ntouch project-tool-ran\nexit 99\n' >"$project_dir/$local_tool"
    chmod +x "$project_dir/$local_tool"
  done
  check_bound_build lcb-only --backend lcb
  check_bound_image lcb-only lcb
  check_bound_build llvm-only --backend llvm
  check_bound_image llvm-only llvm
  test ! -e "$project_dir/project-tool-ran"

  cat >"$project_dir/clang-control" <<'EOF'
#!/usr/bin/env bash
set -eu
for arg in "$@"; do
  case "$CONTROL:$arg" in
    fail:launcher.ll|lcb-fail:lcb_rt.s) exit 79 ;;
    substitute-ir:launcher.ll) cp -- "$REPLACEMENT/launcher.ll" launcher.ll ;;
    substitute-object:launcher.ll) cp -- "$REPLACEMENT/component.o" component.o ;;
    substitute-llvm-image:llvm.exe) cp -- "$REPLACEMENT/llvm.exe" llvm.exe; exit 0 ;;
    substitute-lcb-image:lcb.exe) cp -- "$REPLACEMENT/llvm.exe" lcb.exe; exit 0 ;;
    substitute-lcb-object:lcb.exe) "$REAL_CLANG" "$@"; cp -- lcb_rt.o lcb_body.o; exit 0 ;;
    late-llvm-input:lcb.exe) cp -- "$REPLACEMENT/launcher.ll" launcher.ll ;;
    late-lcb-input:launcher.ll) printf '\n' >>lcb_body.s ;;
    late-inventory:lcb.exe) rm -- llvm.link.sha256 ;;
  esac
done
exec "$REAL_CLANG" "$@"
EOF
  # The native helper clears the tool environment. Bake control parameters into its
  # private wrapper rather than relying on ambient flags for the real invocation.
  for control in fail lcb-fail substitute-ir substitute-object substitute-llvm-image substitute-lcb-image \
      substitute-lcb-object late-llvm-input late-lcb-input late-inventory; do
    wrapper="$project_dir/clang-$control"
    {
      printf '#!/usr/bin/env bash\nCONTROL=%q\nREPLACEMENT=%q\nREAL_CLANG=%q\n' \
        "$control" "$work/slice1-link/trap" "$CONCRETE_SLICE0_CLANG"
      tail -n +2 "$project_dir/clang-control"
    } >"$wrapper"
    chmod +x "$wrapper"
    case "$control" in
      lcb-fail|substitute-lcb-*) backend_args=(--backend lcb) ;;
      late-*) backend_args=() ;;
      *) backend_args=(--backend llvm) ;;
    esac
    rc=0
    CONCRETE_SLICE0_CLANG="$wrapper" bounded "$native_secs" "${cli[@]}" \
      build answer.con -o "$control/" "${backend_args[@]}" \
      >"$project_dir/$control.log" 2>&1 || rc=$?
    test ! -e "$project_dir/$control"
    if [[ "$control" = *fail ]]; then
      test "$rc" = 2
      has_match -F "native build helper exited with status 79" <"$project_dir/$control.log"
    else
      test "$rc" = 2
      case "$control" in
        substitute-ir) has_match -F 'launcher.ll: FAILED' <"$project_dir/$control.log" ;;
        substitute-object) has_match -F 'component.o: FAILED' <"$project_dir/$control.log" ;;
        substitute-lcb-object) has_match -F 'lcb_body.o: FAILED' <"$project_dir/$control.log" ;;
        substitute-llvm-image|late-inventory)
          has_match -F 'llvm retained build differs from pinned replay' <"$project_dir/$control.log" ;;
        substitute-lcb-image)
          has_match -F 'lcb retained build differs from pinned replay' <"$project_dir/$control.log" ;;
        late-llvm-input)
          has_match -F 'staged link input changed during native build: launcher.ll' <"$project_dir/$control.log" ;;
        late-lcb-input)
          has_match -F 'staged link input changed during native build: lcb_body.s' <"$project_dir/$control.log" ;;
      esac
    fi
  done
  echo "ok   standalone version, native split build, failure and substitution before publication"
fi
while IFS=$'\t' read -r name program expect; do
  d="$work/ex/$name"
  if [[ "$program" == cli:* ]]; then
    if [ "${CONCRETE_TEST_EMIT_ONLY:-0}" != 1 ]; then
      check_cli_case "$name" "$program" "$d" "$expect"
      examples=$((examples + 1))
    fi
    continue
  fi
  emit_slice0_row "$test_manifest" "$program" "$d" "$target" "$expect"
  if [ "${CONCRETE_TEST_EMIT_ONLY:-0}" = 1 ]; then continue; fi
  case "$expect" in
    "llvm "*) check_backend_limit "$program" "$d" "$expect" ;;
    "refuse "*|"pending-refusal "*)
      mkdir -p "$d"
      read -r form status kind _ <<<"$expect"
      case "$form $kind" in
        "pending-refusal syntax") code=SYNTAX ;;
        "pending-refusal unsupported_form") code=SLICE-0 ;;
        *) code=CORE-CHECK ;;
      esac
      check_cli_diagnostic "$name" "$d/cli" "refuse $status $code" "${project_cli[@]}" \
        build "${program#con:}" -o "$d/out"
      if [ -e "$d/out" ] || [ -L "$d/out" ]; then
        echo "FAIL $name: refused build created an output path"
        fail=1
      fi
      ;;
    *) check "$name" "$d" 10 20 ;;
  esac
  examples=$((examples + 1))
done <<<"$rows"
if [ "${CONCRETE_TEST_EMIT_ONLY:-0}" = 1 ]; then exit "$fail"; fi
help_rc=0
bounded "$native_secs" "$cli_bin" help >"$work/ctl/help.stdout" 2>"$work/ctl/help.stderr" || help_rc=$?
count=$((count + 1))
printf '%s\n' 'usage: concrete build SOURCE.con -o OUTPUT-DIRECTORY [--backend llvm|lcb] | concrete help' \
  >"$work/ctl/help.expected"
if [ "$help_rc" = 0 ] && [ ! -s "$work/ctl/help.stderr" ] &&
    cmp -s "$work/ctl/help.expected" "$work/ctl/help.stdout"; then
  echo "ok   CLI help: status 0"
else
  echo "FAIL CLI help: expected status 0, usage on stdout, and empty stderr" >&2
  fail=1
fi
check_cli_output_failure cli-help-stdout 1 "$cli_bin" help
check_cli_output_failure cli-usage-stderr 2 "$cli_bin" invalid-command

# A successful helper emits enough stdout to fail during the write even when buffered.
helper_project="$work/ctl/output-project"
helper_install="$work/ctl/output-installation"
mkdir -p "$helper_project/tests/slice0"
cp "$root/tests/slice0/answer.con" "$helper_project/tests/slice0/"
private_cli_installation "$helper_install"
publish_lib="$helper_install/scripts/checks/lib"
publish_cli=(bash -c 'cd "$1" && shift && exec "$@"' _ "$helper_project" "$helper_install/.lake/build/bin/concrete")
printf '%s\n' 'head -c 65536 /dev/zero | tr "\000" x' >"$publish_lib/slice0_native.sh"
check_cli_output_failure cli-helper-stdout 1 "${publish_cli[@]}" \
  build tests/slice0/answer.con -o "$helper_project/out" --backend llvm
cp "$root/scripts/checks/lib/slice0_native.sh" "$publish_lib/"

# The real helper and pinned replay are quiet. Fail the CLI's own version write.
first_out="$helper_project/first-output"
check_cli_output_failure cli-version-stdout-first 1 "${publish_cli[@]}" \
  build tests/slice0/answer.con -o "$first_out" --backend llvm
if [ -e "$first_out" ]; then
  echo "FAIL CLI version write: failed first build installed a generation"
  fail=1
fi

# Rebuild over a known generation. Inject publication faults through a private installation.
publish_out="$helper_project/out"
publish_build=(build tests/slice0/answer.con -o "$publish_out" --backend llvm)
mkdir -p "$work/ctl/previous" "$publish_out"
for artifact in component.ll component.o launcher.ll launcher.o llvm.link.sha256 llvm.exe runtime-premises.txt lcb.exe; do
  printf 'old %s\n' "$artifact" >"$work/ctl/previous/$artifact"
done
cp "$work/ctl/previous/"* "$publish_out/"

check_cli_output_failure cli-version-stdout-rebuild 1 "${publish_cli[@]}" "${publish_build[@]}"
if ! diff -r "$work/ctl/previous" "$publish_out"; then
  echo "FAIL CLI version write: failed rebuild replaced the previous generation"
  fail=1
fi

mv "$publish_lib/slice0_publish.py" "$publish_lib/publisher.py"
cat >"$publish_lib/slice0_publish.py" <<'PYTHON'
import errno
import os
from pathlib import Path
import sys

import publisher

fault = os.environ.get("PUBLICATION_CONTROL")
if fault == "second-move":
    rename = Path.rename

    def block_second_move(source, destination):
        if source.name == "component.o":
            first = destination.parent / "component.ll"
            assert first.stat().st_size > 0
            assert not (source.parent / first.name).exists()
            destination.unlink()
            destination.mkdir()
            try:
                return rename(source, destination)
            except OSError as error:
                assert error.errno in (errno.EISDIR, errno.ENOTDIR)
                raise OSError(error.errno, "second artifact rename rejected directory target", destination) from error
        return rename(source, destination)

    Path.rename = block_second_move
elif fault == "ambiguous-alias":
    original_stat = os.stat
    output = Path(sys.argv[2])
    identity = output.stat()

    def stat_output(path, *, dir_fd=None, follow_symlinks=True):
        if dir_fd is not None and path == "component.o" and os.path.samestat(os.fstat(dir_fd), identity):
            path = "LLVM.old"
        return original_stat(path, dir_fd=dir_fd, follow_symlinks=follow_symlinks)

    os.stat = stat_output
elif fault in ("lookup-mismatch", "lookup-unknown") and sys.argv[1] == "publish":
    output = Path(sys.argv[2])
    identity = output.stat()

    def lookup_rules(fd, *_):
        is_output = os.path.samestat(os.fstat(fd), identity)
        if is_output and fault == "lookup-unknown":
            raise OSError(errno.EIO, "injected lookup probe failure")
        return "simulated", is_output

    publisher.linux_lookup_rules = lookup_rules
    publisher.macos_lookup_rules = lookup_rules
elif fault == "lookup-unknown-new":
    def unknown_lookup(fd, *_):
        raise OSError(errno.EIO, "injected first-build lookup probe failure")

    publisher.linux_lookup_rules = unknown_lookup
    publisher.macos_lookup_rules = unknown_lookup

    def unexpected_staging(*args, **kwargs):
        raise AssertionError("allocated staging before the lookup probe")

    publisher.tempfile.mkdtemp = unexpected_staging
elif fault == "exchange":
    def fail_exchange(parent_fd, candidate, output):
        fd = os.open(candidate, os.O_RDONLY | os.O_DIRECTORY, dir_fd=parent_fd)
        try:
            assert set(os.listdir(fd)) == {
                "component.ll", "component.o", "launcher.ll", "launcher.o",
                "llvm.link.sha256", "llvm.exe", "runtime-premises.txt", "lcb.exe",
            }
        finally:
            os.close(fd)
        raise OSError(errno.EIO, "injected directory exchange failure")

    publisher.exchange = fail_exchange

try:
    publisher.main(sys.argv[1:])
except (OSError, ValueError) as error:
    sys.exit(str(error))
PYTHON
for fault in second-move exchange lookup-mismatch lookup-unknown; do
  case "$fault" in
    second-move) detail='second artifact rename rejected directory target' ;;
    exchange) detail='injected directory exchange failure' ;;
    lookup-mismatch) detail='name-lookup rules differ' ;;
    lookup-unknown) detail='injected lookup probe failure' ;;
  esac
  log="$work/ctl/publication-$fault"
  check_cli_diagnostic "CLI publication failure ($fault)" "$log" "refuse 2 TOOL-FAILURE" --reports-version \
    env PUBLICATION_CONTROL="$fault" "${publish_cli[@]}" "${publish_build[@]}"
  if ! diff -r "$work/ctl/previous" "$publish_out" ||
      ! has_match -E '^TOOL-FAILURE: incomplete \(publish build outputs: ' <"$log.stderr" ||
      ! has_match -F "$detail" <"$log.stderr"; then
    echo "FAIL $fault: publication failure did not preserve the previous generation"
    fail=1
  fi
done

mv "$publish_out/component.o" "$publish_out/LLVM.old"
ln "$publish_out/LLVM.old" "$publish_out/keep"
log="$work/ctl/publication-ambiguous-alias"
check_cli_diagnostic "CLI publication failure (ambiguous alias)" "$log" "refuse 2 TOOL-FAILURE" --reports-version \
  env PUBLICATION_CONTROL=ambiguous-alias "${publish_cli[@]}" "${publish_build[@]}"
if ! cmp -s "$work/ctl/previous/component.o" "$publish_out/keep" ||
    ! [ "$publish_out/LLVM.old" -ef "$publish_out/keep" ] ||
    ! has_match -F 'cannot identify the build output entry: component.o' <"$log.stderr"; then
  echo "FAIL ambiguous alias: expected an unchanged hard link and an identification failure"
  fail=1
fi
rm "$publish_out/keep"
mv "$publish_out/LLVM.old" "$publish_out/component.o"
if ! diff -r "$work/ctl/previous" "$publish_out"; then
  echo "FAIL ambiguous alias: publication failure did not preserve the previous generation"
  fail=1
fi

new_output="$helper_project/new-parent/nested/out"
log="$work/ctl/publication-lookup-unknown-new"
cp "$publish_lib/slice0_native.sh" "$work/ctl/native-success.sh"
printf '%s\n' 'touch native-ran-before-lookup' 'exit 99' >"$publish_lib/slice0_native.sh"
check_cli_diagnostic "CLI first build with unknown lookup rules" "$log" "refuse 2 TOOL-FAILURE" \
  env PUBLICATION_CONTROL=lookup-unknown-new "${publish_cli[@]}" \
  build tests/slice0/answer.con -o "$new_output" --backend llvm
if [ -e "$helper_project/new-parent" ] ||
    [ -e "$helper_project/native-ran-before-lookup" ] ||
    ! has_match -F 'injected first-build lookup probe failure' <"$log.stderr"; then
  echo "FAIL first build with unknown lookup rules: expected refusal before staging and native build"
  fail=1
fi
mv "$work/ctl/native-success.sh" "$publish_lib/slice0_native.sh"

mv "$publish_lib/publisher.py" "$publish_lib/slice0_publish.py"
build "CLI atomic rebuild" "$work/ctl/publication-success.log" bounded "$native_secs" \
  "${publish_cli[@]}" "${publish_build[@]}"
if cmp -s "$work/ctl/previous/component.ll" "$publish_out/component.ll" ||
    ! (cd "$publish_out" && sha256sum --quiet --strict -c llvm.link.sha256) ||
    [ ! -x "$publish_out/llvm.exe" ] ||
    ! cmp -s "$work/ctl/previous/lcb.exe" "$publish_out/lcb.exe" ||
    [ -n "$(find "$helper_project" -maxdepth 1 -name '.concrete-build.*' -print)" ]; then
  echo "FAIL CLI atomic rebuild: expected the new LLVM set and the previous LCB executable"
  fail=1
else
  echo "ok   CLI atomic rebuild preserves the unselected backend"
fi

mkdir -p "$helper_project/target/nested" "$helper_project/target/out"
ln -s target/nested "$helper_project/link"
printf '%s\n' requested-old >"$helper_project/target/out/component.o"
cp "$publish_out/component.o" "$work/ctl/expected-component.o"
printf '%s\n' sibling-old >"$work/ctl/sibling-component.o"
cp "$work/ctl/sibling-component.o" "$publish_out/component.o"
build "CLI output through ancestor symlink and dot-dot" "$work/ctl/publication-dot-dot.log" bounded "$native_secs" \
  "${publish_cli[@]}" build tests/slice0/answer.con -o "$helper_project/link/../out" --backend llvm
if ! cmp -s "$work/ctl/sibling-component.o" "$publish_out/component.o" ||
    ! cmp -s "$helper_project/target/out/component.o" "$work/ctl/expected-component.o" ||
    ! (cd "$helper_project/target/out" && sha256sum --quiet --strict -c llvm.link.sha256); then
  echo "FAIL CLI symlink and dot-dot: expected only the requested output to be replaced"
  fail=1
else
  echo "ok   CLI symlink and dot-dot publish to the requested output"
fi

build "CLI first build with missing ancestors" "$work/ctl/publication-first-build.log" bounded "$native_secs" \
  "${publish_cli[@]}" build tests/slice0/answer.con -o "$new_output" --backend llvm
if ! (cd "$new_output" && sha256sum --quiet --strict -c llvm.link.sha256) ||
    [ ! -x "$new_output/llvm.exe" ] ||
    [ ! -s "$new_output/component.ll" ]; then
  echo "FAIL CLI first build: expected the full LLVM generation under the new ancestors"
  fail=1
else
  echo "ok   CLI first build publishes the full generation with its missing ancestors"
fi

build "publication case aliases" "$work/ctl/publication-case.log" bounded 30 \
  python3 -B scripts/checks/compiler/slice0_publication_controls.py
cat "$work/ctl/publication-case.log"

cp -R "$work/ctl/previous" "$work/ctl/symlink-target-before"
ln -s "$work/ctl/previous" "$helper_project/linked-out"
check_cli_diagnostic "CLI symlink output directory" "$work/ctl/linked-out" "refuse 2 TOOL-FAILURE" \
  "${publish_cli[@]}" build tests/slice0/answer.con -o "$helper_project/linked-out/." --backend llvm
if [ ! -L "$helper_project/linked-out" ] ||
    ! diff -r "$work/ctl/previous" "$work/ctl/symlink-target-before" ||
    ! has_match -F 'build output directory must be a real directory:' <"$work/ctl/linked-out.stderr"; then
  echo "FAIL CLI symlink output directory: expected rejection before staging"
  fail=1
fi

"${emitter[@]}" backend-profile
"${emitter[@]}" loop "$work/ctl/loop" "$target"
expected_timeout check control-loop "$work/ctl/loop" 1 1
"${emitter[@]}" invalid "$work/ctl/invalid" "$target"
check control-invalid-word "$work/ctl/invalid" 10 20

# The comparison's own negative controls: each forged observation must be rejected.
e="$work/ctl/judge"; mkdir -p "$e"
echo 134 >"$e/status"; printf 'concrete: trap: x\n' >"$e/stderr"
forge() { # forge NAME STATUS STDERR-PRINTF STDOUT
  local o="$work/ctl/forged-$1"
  echo "$2" >"$o.status"; printf "$3" >"$o.stderr"; printf '%s' "$4" >"$o.stdout"
  echo "$o"
}
judge "$e" "$(forge same 134 'concrete: trap: x\n' '')" || { echo "FAIL judge rejects an exact match"; fail=1; }
for c in "no-newline|134|concrete: trap: x|" "extra-newline|134|concrete: trap: x\n\n|" \
         "wrong-status|1|concrete: trap: x\n|" "stray-stdout|134|concrete: trap: x\n|42" \
         "incomplete|incomplete||" "runner-failed|runner-failed||"; do
  IFS='|' read -r n s err out <<<"$c"
  if judge "$e" "$(forge "$n" "$s" "$err" "$out")"; then
    echo "FAIL judge accepts a forged observation: $n"; fail=1
  else
    echo "ok   judge rejects $n"
  fi
done

# Isolate the intentional watchdog failure from the gate's own bounded log.
(
  BOUNDED_LOG="$work/ctl/failed-watchdog.log"
  BOUNDED_EXEC="$work/ctl/nonexistent-watchdog.py"
  observe "$work/ctl/invalid/llvm.exe" "$work/ctl/failed-watchdog" 1 1
  [ "$(cat "$work/ctl/failed-watchdog.status")" = runner-failed ] &&
    ! judge "$work/ctl/loop" "$work/ctl/failed-watchdog"
) && echo "ok   observe distinguishes a failed watchdog from timeout" ||
  { echo "FAIL observe: watchdog failure was not classified separately"; fail=1; }

for unavailable in runner-failed unsupported-profile; do
  echo "$unavailable" >"$e/status"
  : >"$e/stderr"
  if judge "$e" "$(forge "$unavailable" "$unavailable" '' '')"; then
    echo "FAIL judge accepts $unavailable as an observation"
    fail=1
  else
    echo "ok   judge rejects $unavailable even when the expected status matches"
  fi
done

[ "$examples" -gt 0 ] || { echo "tests: no examples ran" >&2; exit 1; }
echo "tests: $examples examples, $count backend runs (controls included)"
exit "$fail"
