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
# Controls: wrong exit and trap expectations (both site and kind) must fail in the emitter; a
# missing row and a nonexistent program must fail opposite inventory directions; a wrong source
# refusal must fail in the source runner. Child controls are emit-only. A body that never returns must
# be observed as incomplete on both backends; a body returning a word that encodes nothing must reach
# the runtime's refusal; and the comparison itself must reject a missing or extra newline, a wrong
# status with the right stderr, and stray stdout. Run inside the nix devshell, from the repo root.
set -euo pipefail
# shellcheck source=scripts/checks/lib/slice0_common.sh
source scripts/checks/lib/slice0_common.sh
# shellcheck source=scripts/checks/lib/has_match.sh
source scripts/checks/lib/has_match.sh

# The emit-only manifest controls exercise only manifest validation.
if [[ ${CONCRETE_TEST_EMIT_ONLY:-0} != 1 ]]; then
  bash scripts/checks/compiler/f31_census.sh
  build slice1-driver "$work/slice1-driver-build.log" bounded 120 \
    lake build ConcreteSpec.Spec.Rules ConcreteSpec.Spec.Examples
  build slice1-driver-controls "$work/slice1-driver-controls.log" bounded 120 \
    lake env lean --run scripts/checks/compiler/slice1_driver.lean "$work/slice1-link"
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
  build llvm-rendering "$work/llvm-rendering.log" bounded 120 \
    lake env lean scripts/checks/compiler/llvm_rendering.lean
  echo "ok   LLVM rendering controls"
  build llvm-mutation-controls "$work/llvm-mutation-controls.log" bounded 120 \
    python3 -B scripts/checks/compiler/llvm_mutation_controls.py
  cat "$work/llvm-mutation-controls.log"
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

# The backend-specific expectation exercises the public driver and its native build policy.
check_backend_limit() {
  local path=${1#con:}
  local d=$2
  local expect=$3
  local status rule
  local project
  local -a cli
  project=$(cd "$(dirname "$test_manifest")/.." && pwd)
  if [ "$project" != "$root" ]; then
    mkdir -p "$d/project/$(dirname "$path")"
    cp -- "$project/$path" "$d/project/$path"
    project="$d/project"
  fi
  cli=(bash -c 'cd "$1" && shift && exec "$@"' _ "$project" "$root/.lake/build/bin/concrete")
  read -r _ _ status _ _ rule <<<"$expect"
  mkdir -p "$d/expected"
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
  if [ "$rc" = 1 ] && [ ! -s "$d/lcb.stdout" ] && [ ! -e "$d/lcb" ] &&
      has_match -E "^$rule: LCB backend resource refusal: [^:]+: [0-9]+ exceeds limit [0-9]+$" <"$d/lcb.stderr"; then
    echo "ok   $path [lcb]: $(cat "$d/lcb.stderr")"
  else
    echo "FAIL $path [lcb]: expected a $rule backend resource refusal, got status $rc"
    cat "$d/lcb.stderr"
    fail=1
  fi
}

rows=$(manifest_rows)

wrong_expectation() { awk -F '\t' -v OFS='\t' '$1 == "answer" {$3 = "exit 41"} {print}' "$1" >"$1.new"; mv "$1.new" "$1"; }
wrong_trap_expectation() { awk -F '\t' -v OFS='\t' '$1 == "overflow" {$3 = "trap arithmeticOverflow site 1"} {print}' "$1" >"$1.new"; mv "$1.new" "$1"; }
wrong_trap_kind() { awk -F '\t' -v OFS='\t' '$1 == "overflow" {$3 = "trap divisionByZero site 0"} {print}' "$1" >"$1.new"; mv "$1.new" "$1"; }
missing_row() { awk -F '\t' '$1 != "answer"' "$1" >"$1.new"; mv "$1.new" "$1"; }
nonexistent_program() { printf 'no_such_program\tbir:no_such_program\texit 0\tslice0\n' >>"$1"; }
wrong_source_refusal() {
  mkdir -p "$(dirname "$1")/slice0"
  printf 'fn main() -> u8 { return 42 }\n' >"$(dirname "$1")/slice0/wrong_refusal.con"
  awk -F '\t' '{print} $1 == "answer" {print "wrong_source_refusal\tcon:tests/slice0/wrong_refusal.con\trefuse CORE-CHECK\tslice0"}' "$1" >"$1.new"
  mv "$1.new" "$1"
}
manifest_control() { # manifest_control LABEL EXPECTED-MESSAGE MUTATION-FUNCTION
  local label=$1
  local expected=$2
  local mutation=$3
  local dir="$work/ctl/$1"
  local copy
  local rc=0
  mkdir -p "$dir"
  cp -R "$(dirname "$test_manifest")" "$dir/tests"
  copy="$dir/tests/$(basename "$test_manifest")"
  "$mutation" "$copy"
  if cmp -s "$test_manifest" "$copy"; then
    echo "FAIL manifest control $label: mutation changed nothing" >&2; fail=1; return
  fi
  CONCRETE_TEST_MANIFEST="$copy" CONCRETE_TEST_MANIFEST_SKIP_CONTROLS=1 CONCRETE_TEST_EMIT_ONLY=1 \
    bash "$root/scripts/checks/compiler/tests.sh" >"$work/ctl/$label.log" 2>&1 || rc=$?
  if [ "$rc" -ne 0 ] && has_match -F "$expected" <"$work/ctl/$label.log"; then
    echo "ok   manifest control $label: refused for the planted reason"
  else
    echo "FAIL manifest control $label: not refused as '$expected'" >&2
    cat "$work/ctl/$label.log" >&2
    fail=1
  fi
}
mkdir -p "$work/ctl"
if [ "${CONCRETE_TEST_MANIFEST_SKIP_CONTROLS:-0}" != 1 ]; then
  manifest_control wrong-expectation "answer: expectation mismatch: BIR run gives" wrong_expectation
  manifest_control wrong-trap-expectation "overflow: expectation mismatch: BIR run gives" wrong_trap_expectation
  manifest_control wrong-trap-kind "overflow: expectation mismatch: trap site 0 has kind ConcreteSpec.Trap.arithmeticOverflow, expected divisionByZero" wrong_trap_kind
  manifest_control missing-row "program without a manifest row: answer" missing_row
  manifest_control nonexistent-program "manifest row names no program: no_such_program" nonexistent_program
  manifest_control wrong-source-refusal "expected refusal CORE-CHECK, got SYNTAX" wrong_source_refusal
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
      test "$rc" = 79
    else
      test "$rc" = 1
      case "$control" in
        substitute-ir) has_match -F 'launcher.ll: FAILED' <"$project_dir/$control.log" ;;
        substitute-object) has_match -F 'component.o: FAILED' <"$project_dir/$control.log" ;;
        substitute-lcb-object) has_match -F 'lcb_body.o: FAILED' <"$project_dir/$control.log" ;;
        substitute-llvm-image|late-inventory)
          has_match -F 'llvm retained build differs from pinned replay' <"$project_dir/$control.log" ;;
        substitute-lcb-image)
          has_match -F 'lcb retained build differs from pinned replay' <"$project_dir/$control.log" ;;
        late-llvm-input)
          has_match -F 'LLVM link inputs changed during native build' <"$project_dir/$control.log" ;;
        late-lcb-input)
          has_match -F 'LCB link inputs changed during native build' <"$project_dir/$control.log" ;;
      esac
    fi
  done
  echo "ok   standalone version, native split build, failure and substitution before publication"
fi
while IFS=$'\t' read -r name program expect; do
  d="$work/ex/$name"
  if [[ "$expect" == "llvm "* ]]; then
    if [ "${CONCRETE_TEST_EMIT_ONLY:-0}" != 1 ]; then
      check_backend_limit "$program" "$d" "$expect"
      examples=$((examples + 1))
    fi
    continue
  fi
  emit_slice0_row "$test_manifest" "$program" "$d" "$target" "$expect"
  # Emit-only controls visit every row without a native build.
  if [ "${CONCRETE_TEST_EMIT_ONLY:-0}" = 1 ] || [[ "$expect" == "refuse "* ]]; then continue; fi
  check "$name" "$d" 10 20
  examples=$((examples + 1))
done <<<"$rows"
if [ "${CONCRETE_TEST_EMIT_ONLY:-0}" = 1 ]; then exit "$fail"; fi
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
         "incomplete|incomplete||"; do
  IFS='|' read -r n s err out <<<"$c"
  if judge "$e" "$(forge "$n" "$s" "$err" "$out")"; then
    echo "FAIL judge accepts a forged observation: $n"; fail=1
  else
    echo "ok   judge rejects $n"
  fi
done

[ "$examples" -gt 0 ] || { echo "tests: no examples ran" >&2; exit 1; }
echo "tests: $examples examples, $count backend runs (controls included)"
exit "$fail"
