#!/usr/bin/env bash
# Per-test determinism gate, the one-compiler-build complement of gate 8. Every bir: manifest row is
# emitted and built natively twice from different absolute output paths, working directories, HOMEs,
# TMPDIRs and environment assignment orders; every emitted and native artifact must be byte-identical.
# A second emit in the same Lean process checks in-process determinism. Gate 8 instead rebuilds the
# compiler and LCB independently from two source copies and also compares cross-runner manifests.
# Controls: a planted CWD in llvm.ll, planted monotonic time in lcb_body.s, and a missing artifact
# must each fail for that named reason; a clean double run must pass. Plants require
# --allow-planted-failure in slice0_bir.lean.
#
# usage: determinism.sh. Run inside nix develop. DETERMINISM_EMIT_ONLY=1 is the local smoke mode:
# it builds only Concrete and omits native compilation (the normal gate always builds it and LCB).
set -euo pipefail
root=$(cd "$(dirname "$0")/../../.." && pwd)
cd "$root"
# shellcheck source=scripts/checks/lib/limits.sh
source scripts/checks/lib/limits.sh
gate_limits 64 40000
work=$(mktemp -d)
trap 'rc=$?; bounded_summary || true
  if [ "$rc" = 0 ] && grep -Eqs "^(incomplete|runner-failed)" "${BOUNDED_LOG:-/nonexistent}"; then
    echo "INCOMPLETE: a bounded run timed out twice; not a pass"; rc=3
  fi
  rm -rf "${work:?}"; exit "$rc"' EXIT
BOUNDED_PARENT="$work"
# shellcheck source=scripts/checks/lib/bounded.sh
source scripts/checks/lib/bounded.sh
[ "$BOUNDED_HAVE_WATCHDOG" = 1 ] || { echo "determinism: python3 is required for bounded runs" >&2; exit 2; }
bounded() {
  local secs=$1 rc=0
  shift
  bounded_run -i /dev/null "$secs" -- "$@" || rc=$?
  if [ "$rc" != 0 ] && bounded_is_incomplete "$rc"; then
    echo "determinism: INCOMPLETE (timed out twice): $*" >&2
  fi
  return "$rc"
}
emit_only=${DETERMINISM_EMIT_ONLY:-0}
case "$emit_only" in 0|1) ;; *) echo "determinism: DETERMINISM_EMIT_ONLY must be 0 or 1" >&2; exit 2 ;; esac
if [ "$emit_only" = 1 ]; then
  bounded 1800 lake build Concrete >"$work/lake-build.log" 2>&1 ||
    { tail -n 100 "$work/lake-build.log" >&2; exit 1; }
else
  bounded 1800 lake build Concrete lean-compiler-backend/lcb >"$work/lake-build.log" 2>&1 ||
    { tail -n 100 "$work/lake-build.log" >&2; exit 1; }
fi
lcb=$root/.lake/packages/lean-compiler-backend/.lake/build/bin/lcb
case "$(uname -s)-$(uname -m)" in
  Linux-x86_64) target=x86_64-linux ;;
  Linux-aarch64) target=aarch64-linux ;;
  Darwin-arm64) target=aarch64-macos ;;
  *) echo "determinism: unsupported host $(uname -s)-$(uname -m)" >&2; exit 2 ;;
esac
lake_cmd=$(command -v lake)
driver=$root/scripts/checks/compiler/slice0_bir.lean
manifest=${CONCRETE_TEST_MANIFEST:-tests/manifest.tsv}
bounded 30 python3 -I "$root/scripts/checks/lib/test_manifest.py" rows --kind bir "$manifest" >"$work/rows"
[ -s "$work/rows" ] || { echo "determinism: no bir tests" >&2; exit 1; }
awk -F '\t' '{print $1}' "$work/rows" >"$work/names"
awk -F '\t' '{sub(/^bir:/, "", $2); print $2}' "$work/rows" | LC_ALL=C sort >"$work/manifest-programs"
bounded 120 "$lake_cmd" --dir "$root" env lean --run "$driver" list | LC_ALL=C sort >"$work/emitter-programs"
if [ "$emit_only" = 0 ]; then
  cmp -s "$work/manifest-programs" "$work/emitter-programs" ||
    { echo "determinism: test manifest inventory differs from emitter" >&2; exit 1; }
fi
printf 'one compiler build; per-test determinism\n' >"$work/identity"
emitted=bir.txt,llvm.ll,lcb_job.txt,lcb_source.txt,lcb_body.ssa,lcb_body.s,lcb_rt.ssa,status,stderr
all=$emitted,lcb_rt.s,llvm.o,lcb_body.o,lcb_rt.o,llvm.exe,lcb.exe
compare=(python3 -I "$root/scripts/checks/lib/repro_compare.py" --label slice0_determinism)

a=$work/first/output-with-one-absolute-path
b=$work/second/a-different-and-deliberately-longer-absolute-output-path
for side in "$a" "$b"; do
  mkdir -p "$side/out" "$side/home" "$side/tmp"
  cp "$work/names" "$side/names"
done

emit_one() { # emit_one SIDE CWD NAME PROGRAM EXPECT [PLANT]
  local side=$1 cwd=$2 name=$3 program=$4 expect=$5 plant=${6:-}
  local out=$side/out/$name
  local -a expectation plant_args envs
  read -r -a expectation <<<"$expect"
  plant_args=()
  [ -z "$plant" ] || plant_args=(--allow-planted-failure --plant "$plant")
  if [ "$side" = "$a" ]; then
    envs=("HOME=$side/home" "TMPDIR=$side/tmp" LC_ALL=C TZ=UTC SOURCE_DATE_EPOCH=1 ZERO_AR_DATE=1)
  else
    envs=(ZERO_AR_DATE=1 SOURCE_DATE_EPOCH=1 TZ=UTC LC_ALL=C "TMPDIR=$side/tmp" "HOME=$side/home")
  fi
  (cd "$cwd" && bounded 120 env "${envs[@]}" "$lake_cmd" --dir "$root" env lean --run "$driver" emit \
    "${program#bir:}" "$out" "$target" "${expectation[@]}" ${plant_args[@]+"${plant_args[@]}"})
}

native_one() { # native_one SIDE NAME
  local side=$1 name=$2 out=$side/out/$name log=$side/$name.native.log
  if ! (cd "$side" && bounded 300 env "TMPDIR=$side/tmp" "HOME=$side/home" ZERO_AR_DATE=1 \
      SOURCE_DATE_EPOCH=1 bash "$root/scripts/checks/lib/slice0_native.sh" "$out" "$lcb") >"$log" 2>&1; then
    cat "$log" >&2
    echo "FAIL slice0_determinism: $name: native build failed" >&2
    exit 1
  fi
}

first_name= first_program= first_expect=
while IFS=$'\t' read -r name program expect; do
  if [ -z "$first_name" ]; then first_name=$name first_program=$program first_expect=$expect; fi
  emit_one "$a" "$root" "$name" "$program" "$expect"
  emit_one "$b" "$root/scripts" "$name" "$program" "$expect"
  if [ "$emit_only" = 0 ]; then
    native_one "$a" "$name"
    native_one "$b" "$name"
  fi

  same=$work/same/$name
  mkdir -p "$same/a/out/$name" "$same/b/out/$name"
  printf '%s\n' "$name" >"$same/a/names"
  cp "$same/a/names" "$same/b/names"
  read -r -a expectation <<<"$expect"
  bounded 120 "$lake_cmd" --dir "$root" env lean --run "$driver" emit-twice "${program#bir:}" \
    "$same/a/out/$name" "$same/b/out/$name" "$target" "${expectation[@]}"
  "${compare[@]}" --stages "$emitted" "$same/a" "$same/b" "$work/identity"
done <"$work/rows"

stages=$all
[ "$emit_only" = 0 ] || stages=$emitted
"${compare[@]}" --stages "$stages" "$a" "$b" "$work/identity"
echo 'ok slice0_determinism: clean double run'

control_compare() { # control_compare LABEL ARTIFACT SIDE_A SIDE_B STAGES
  local label=$1 artifact=$2 ca=$3 cb=$4 selected=$5 rc=0
  local log=$work/control-$label.log
  "${compare[@]}" --stages "$selected" "$ca" "$cb" "$work/identity" >"$log" 2>&1 || rc=$?
  if [ "$rc" = 0 ] || ! grep -qF "$first_name: $artifact:" "$log"; then
    echo "FAIL slice0_determinism control $label: comparator did not name $artifact" >&2
    cat "$log" >&2
    exit 1
  fi
  cat "$log"
  echo "ok slice0_determinism control $label: refused for the planted reason"
}
for plant in nondeterministic monotonic; do
  c=$work/control-$plant
  for side in ca cb; do
    mkdir -p "$c/$side/out" "$c/$side/home" "$c/$side/tmp"
    printf '%s\n' "$first_name" >"$c/$side/names"
  done
  # Reuse emit_one's environment variations by making the control paths the two known sides.
  old_a=$a old_b=$b
  a=$c/ca b=$c/cb
  emit_one "$a" "$root" "$first_name" "$first_program" "$first_expect" "$plant"
  # The CWD plant needs distinct CWDs; the time plant is deliberately run from the same CWD.
  cwd=$root
  [ "$plant" != nondeterministic ] || cwd=$root/scripts
  emit_one "$b" "$cwd" "$first_name" "$first_program" "$first_expect" "$plant"
  a=$old_a b=$old_b
  artifact=llvm.ll
  [ "$plant" != monotonic ] || artifact=lcb_body.s
  control_compare "$plant" "$artifact" "$c/ca" "$c/cb" "$emitted"
done

c=$work/control-missing
mkdir -p "$c"
cp -R "$a" "$c/a"
cp -R "$a" "$c/b"
rm "$c/b/out/$first_name/lcb_body.s"
control_compare missing-artifact lcb_body.s "$c/a" "$c/b" "$stages"

rc=0
read -r -a first_expectation <<<"$first_expect"
(cd "$root" && bounded 120 "$lake_cmd" --dir "$root" env lean --run "$driver" emit "${first_program#bir:}" \
  "$work/refused" "$target" "${first_expectation[@]}" --plant nondeterministic) >"$work/refusal.log" 2>&1 || rc=$?
if [ "$rc" != 2 ] || ! grep -qF 'plant without --allow-planted-failure' "$work/refusal.log"; then
  echo 'FAIL slice0_determinism control refusal: plant did not require the explicit flag' >&2
  cat "$work/refusal.log" >&2
  exit 1
fi
echo 'ok slice0_determinism control refusal: explicit flag required'
echo 'PASS slice0_determinism'
