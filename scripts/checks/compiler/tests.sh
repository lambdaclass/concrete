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
  build stage-interfaces "$work/stage-interfaces.log" bounded 120 \
    lake build Concrete.Interfaces.Pipeline
  build stage-interface-controls "$work/stage-interface-controls.log" bounded 120 \
    lake env lean -DwarningAsError=true -DautoImplicit=false scripts/checks/compiler/stage_interfaces.lean
  echo "ok   stage interface controls"
  # The scaling fixture uses the project admission proof, outside Concrete's build closure.
  build span-proof-deps "$work/span-proof-deps.log" bounded 1800 lake build ConcreteSpec.Proofs.Ident
  bounded 120 lake env lean --run scripts/checks/compiler/span_scaling.lean
  build llvm-mutation-controls "$work/llvm-mutation-controls.log" bounded 120 \
    lake env python3 -B scripts/checks/compiler/llvm_mutation_controls.py
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

# Check a diagnostic through the public CLI, including its process status.
check_cli_diagnostic() {
  local label=$1
  local output=$2
  local expectation=$3
  shift 3
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
  if [ "$rc" = "$status" ] && [ ! -s "$output.stdout" ] &&
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
      # Substitute the native helper only in a private project. The CLI still runs
      # its production frontend, LCB admission, and helper invocation.
      local project="$d/project"
      local helper="$project/scripts/checks/lib/slice0_native.sh"
      mkdir -p "$project/tests/slice0" "$(dirname "$helper")"
      cp "$root/$source" "$project/$source"
      cp "$root/scripts/checks/lib/slice0_write.py" "$root/scripts/checks/lib/slice0_publish.py" "$(dirname "$helper")/"
      ln -s "$root/.lake" "$project/.lake"
      cli=(bash -c 'cd "$1" && shift && exec "$@"' _ "$project" "$cli_bin")
      if [ "$program" = cli:unknown-target ]; then
        printf '%s\n' '[ "$1" = target ] || exit 99' 'echo invalid-target' >"$helper"
      elif [ "$program" = cli:build-input-write-failure ]; then
        backend=llvm
        mkdir -p "$d/out"
        local device=/dev/full
        if [ ! -c "$device" ]; then device=/dev/null; fi
        ln -s "$device" "$d/out/llvm.ll"
        printf '%s\n' ': > helper-invoked' >"$helper"
      else
        printf '%s\n' "if [ \"\$1\" = target ]; then echo '$target'; exit; fi" \
          'printf partial >"$1/lcb.exe"' 'echo "native helper control" >&2' 'exit 17' >"$helper"
      fi
      ;;
    *) echo "FAIL $name: unknown CLI case $program" >&2; fail=1; return ;;
  esac
  check_cli_diagnostic "$name" "$d/cli" "$expect" "${cli[@]}" \
    build "$source" -o "$d/out" --backend "$backend"
  if [ "$program" = cli:native-helper-failure ] && [ -n "$(find "$d/out" -mindepth 1 -print)" ]; then
    echo "FAIL $name: failed native build left files in the output directory"
    fail=1
  fi
  if [ "$program" = cli:build-input-write-failure ]; then
    if [ ! -L "$d/out/llvm.ll" ] || [ "$(readlink "$d/out/llvm.ll")" != "$device" ]; then
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
    if ! has_match -E '^TOOL-FAILURE: incomplete \(write .*/llvm\.ll: checked writer exited' <"$d/staged.stderr" ||
        [ ! -d "$d/staged-out" ] || [ -n "$(find "$d/staged-out" -mindepth 1 -print)" ]; then
      echo "FAIL $name: staged write failure did not identify the failed input and clean up"
      fail=1
    fi
  fi
}

# The backend-specific expectation exercises the public driver and its native build policy.
check_backend_limit() {
  local path=${1#con:}
  local d=$2
  local expect=$3
  local status diagnostic
  local project
  local -a cli
  project=$cli_project
  if [ "$project" != "$root" ]; then
    mkdir -p "$d/project/$(dirname "$path")"
    cp -- "$project/$path" "$d/project/$path"
    ln -s "$root/scripts" "$d/project/scripts"
    ln -s "$root/.lake" "$d/project/.lake"
    project="$d/project"
  fi
  cli=(bash -c 'cd "$1" && shift && exec "$@"' _ "$project" "$cli_bin")
  read -r _ _ status _ diagnostic <<<"$expect"
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

  check_cli_diagnostic "$path [lcb]" "$d/lcb" "$diagnostic" "${cli[@]}" \
    build "$path" -o "$d/lcb" --backend lcb
  if [ -e "$d/lcb" ]; then
    echo "FAIL $path [lcb]: incomplete build created an output directory"
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
  awk -F '\t' '{print} $1 == "answer" {print "wrong_source_refusal\tcon:tests/slice0/wrong_refusal.con\trefuse 1 CORE-CHECK\tslice0"}' "$1" >"$1.new"
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
  manifest_control wrong-source-refusal "expected refusal CORE-CHECK (status 1), got SYNTAX (status 1)" wrong_source_refusal
fi

if [ "${CONCRETE_TEST_EMIT_ONLY:-0}" != 1 ]; then
  build "concrete driver" "$work/cli-build.log" bounded 1800 lake build concrete
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
  if [[ "$expect" == "llvm "* ]]; then
    if [ "${CONCRETE_TEST_EMIT_ONLY:-0}" != 1 ]; then
      check_backend_limit "$program" "$d" "$expect"
      examples=$((examples + 1))
    fi
    continue
  fi
  emit_slice0_row "$test_manifest" "$program" "$d" "$target" "$expect"
  # Emit-only controls visit every row without a native build.
  if [ "${CONCRETE_TEST_EMIT_ONLY:-0}" = 1 ]; then continue; fi
  if [[ "$expect" == "refuse "* ]]; then
    mkdir -p "$d"
    # The emitter checks optional exact stage messages, including syntax message formatting.
    read -r _ status code _ <<<"$expect"
    check_cli_diagnostic "$name" "$d/cli" "refuse $status $code" "${project_cli[@]}" \
      build "${program#con:}" -o "$d/out"
    if [ -e "$d/out" ]; then
      echo "FAIL $name: refused build created an output directory"
      fail=1
    fi
  else
    check "$name" "$d" 10 20
  fi
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
mkdir -p "$helper_project/tests/slice0" "$helper_project/scripts/checks/lib"
cp "$root/tests/slice0/answer.con" "$helper_project/tests/slice0/"
cp "$root/scripts/checks/lib/slice0_write.py" "$root/scripts/checks/lib/slice0_publish.py" "$helper_project/scripts/checks/lib/"
printf '%s\n' 'head -c 65536 /dev/zero | tr "\000" x' >"$helper_project/scripts/checks/lib/slice0_native.sh"
check_cli_output_failure cli-helper-stdout 1 bash -c 'cd "$1" && shift && exec "$@"' \
  _ "$helper_project" "$cli_bin" build tests/slice0/answer.con -o "$helper_project/out" --backend llvm

# Rebuild over a known generation. Faults are injected only in this private
# project's copy of the publication helper, through the production CLI.
publish_lib="$helper_project/scripts/checks/lib"
publish_out="$helper_project/out"
publish_cli=(bash -c 'cd "$1" && shift && exec "$@"' _ "$helper_project" "$cli_bin")
publish_build=(build tests/slice0/answer.con -o "$publish_out" --backend llvm)
printf '%s\n' 'printf new-object >"$1/llvm.o"' 'printf new-executable >"$1/llvm.exe"' \
  >"$publish_lib/slice0_native.sh"
mkdir -p "$work/ctl/previous" "$publish_out"
for artifact in llvm.ll llvm.o llvm.exe lcb.exe; do
  printf 'old %s\n' "$artifact" >"$work/ctl/previous/$artifact"
done
cp "$work/ctl/previous/"* "$publish_out/"

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
        if source.name == "llvm.o":
            first = destination.parent / "llvm.ll"
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
        if dir_fd is not None and path == "llvm.o" and os.path.samestat(os.fstat(dir_fd), identity):
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
elif fault == "exchange":
    def fail_exchange(parent_fd, candidate, output):
        fd = os.open(candidate, os.O_RDONLY | os.O_DIRECTORY, dir_fd=parent_fd)
        try:
            assert set(os.listdir(fd)) == {"llvm.ll", "llvm.o", "llvm.exe", "lcb.exe"}
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
  check_cli_diagnostic "CLI publication failure ($fault)" "$log" "refuse 2 TOOL-FAILURE" \
    env PUBLICATION_CONTROL="$fault" "${publish_cli[@]}" "${publish_build[@]}"
  if ! diff -r "$work/ctl/previous" "$publish_out" ||
      ! has_match -E '^TOOL-FAILURE: incomplete \(publish build outputs: ' <"$log.stderr" ||
      ! has_match -F "$detail" <"$log.stderr"; then
    echo "FAIL $fault: publication failure did not preserve the previous generation"
    fail=1
  fi
done

mv "$publish_out/llvm.o" "$publish_out/LLVM.old"
ln "$publish_out/LLVM.old" "$publish_out/keep"
log="$work/ctl/publication-ambiguous-alias"
check_cli_diagnostic "CLI publication failure (ambiguous alias)" "$log" "refuse 2 TOOL-FAILURE" \
  env PUBLICATION_CONTROL=ambiguous-alias "${publish_cli[@]}" "${publish_build[@]}"
if ! cmp -s "$work/ctl/previous/llvm.o" "$publish_out/keep" ||
    ! [ "$publish_out/LLVM.old" -ef "$publish_out/keep" ] ||
    ! has_match -F 'cannot identify the build output entry: llvm.o' <"$log.stderr"; then
  echo "FAIL ambiguous alias: expected an unchanged hard link and an identification failure"
  fail=1
fi
rm "$publish_out/keep"
mv "$publish_out/LLVM.old" "$publish_out/llvm.o"
if ! diff -r "$work/ctl/previous" "$publish_out"; then
  echo "FAIL ambiguous alias: publication failure did not preserve the previous generation"
  fail=1
fi

mv "$publish_lib/publisher.py" "$publish_lib/slice0_publish.py"
build "CLI atomic rebuild" "$work/ctl/publication-success.log" bounded "$native_secs" \
  "${publish_cli[@]}" "${publish_build[@]}"
if cmp -s "$work/ctl/previous/llvm.ll" "$publish_out/llvm.ll" ||
    [ "$(cat "$publish_out/llvm.o")" != new-object ] ||
    [ "$(cat "$publish_out/llvm.exe")" != new-executable ] ||
    ! cmp -s "$work/ctl/previous/lcb.exe" "$publish_out/lcb.exe" ||
    [ -n "$(find "$helper_project" -maxdepth 1 -name '.concrete-build.*' -print)" ]; then
  echo "FAIL CLI atomic rebuild: expected the new LLVM set and the previous LCB executable"
  fail=1
else
  echo "ok   CLI atomic rebuild preserves the unselected backend"
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
