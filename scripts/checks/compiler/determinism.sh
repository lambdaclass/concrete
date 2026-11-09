#!/usr/bin/env bash
# Per-test determinism gate, the one-compiler-build complement of gate 8. Every bir: manifest row is
# emitted and built natively twice from different absolute output paths, working directories, HOMEs,
# TMPDIRs, locales (LC_ALL/LANG: C vs an available alternate UTF-8/POSIX locale), POSIX timezones
# (TZ=UTC0 vs TZ=UTC-5:30), process umasks (022 vs 077) and opposite insertion orders of the same
# explicit env -i keys. Top-level out/ entries and recursive per-program inventories must match, and
# every emitted/native file is compared byte for byte in pipeline stage order so the earliest
# differing stage is always reported first; required files also catch absence on both sides.
# A second emit in the same Lean process checks in-process determinism. Gate 8 instead rebuilds the
# compiler and LCB independently from two source copies and also compares cross-runner manifests.
#
# Unsupported perturbations recorded explicitly:
# - Non-UTF-8 single-byte locales (e.g. ISO-8859-1) are unsupported because Lean's runtime and the
#   manifest's UTF-8 trap-site paths (src/ñandú "x"/lib.con) require a UTF-8 or C/POSIX locale.
# - Named Olson zoneinfo timezones (e.g. America/New_York) under env -i without TZDIR fall back to
#   UTC on Nix Linux, so self-contained POSIX offset strings (UTC0 vs UTC-5:30) are used instead.
# - slice0_native.sh normalizes its own child environment with env -i (LC_ALL=C, TZ=UTC), so the
#   native step varies paths, cwd, TMPDIR and umask while the emitter step varies the full set.
#
# Controls: a planted CWD in llvm.ll (hash mismatch on llvm.ll before downstream llvm.o/llvm.exe),
# planted monotonic time in lcb_body.s (hash mismatch on lcb_body.s before downstream lcb_body.o/lcb.exe),
# a missing required file, an extra per-program file, an unexpected top-level file in out/, and an
# unexpected program directory in out/ must each fail with status 1 and the exact diagnostic class;
# plants without --allow-planted-failure must be refused with status 2; a clean double run must pass.
#
# usage: determinism.sh. Run inside nix develop. DETERMINISM_EMIT_ONLY=1 is the local smoke mode:
# it builds only Concrete and omits native compilation (the normal gate always builds it and LCB).
set -euo pipefail
root=$(cd "$(dirname "$0")/../../.." && pwd)
cd "$root"
# shellcheck source=scripts/checks/lib/limits.sh
source scripts/checks/lib/limits.sh
gate_limits 64 40000
# Shared C lifecycle; the scheduler owns orchestration and outcome interpretation.
# Snapshot external identity only at this entry point; campaign.sh receives explicit inputs.
campaign_outcome_helper=${SCHEDULED_OUTCOME_HELPER:-}
campaign_tier=${SELECTED_TIER:-qualification}
campaign_attempt=${SCHEDULED_ATTEMPT_ID:-local}
campaign_repository=${GITHUB_REPOSITORY:-local}
campaign_runner_environment=${RUNNER_ENVIRONMENT:-local}
campaign_runner_name=${RUNNER_NAME:-local}
source scripts/checks/compiler/campaign.sh

report=${1:-$work/report}
head_sha=$(git rev-parse HEAD)
expected_sha=${SELECTED_SHA:-$head_sha}
[[ "$expected_sha" =~ ^[0-9a-f]{40}$ ]] && [ "$head_sha" = "$expected_sha" ] ||
  { echo 'FAIL slice0_determinism: selected SHA does not match HEAD' >&2; exit 1; }
campaign_identity "$report" "$expected_sha"
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
# Resolve Lake's executable and absolute module search path once; Lake is never in the
# perturbed child launch path. Keep the clean environment explicit.
lean_cmd=$(bounded 30 "$lake_cmd" env which lean)
lean_path=$(bounded 30 "$lake_cmd" env printenv LEAN_PATH)
driver=$root/scripts/checks/compiler/slice0_bir.lean
printf 'def main : IO Unit := do IO.println (← IO.currentDir)\n' >"$work/cwd.lean"
for cwd in "$root" "$root/scripts"; do
  actual=$(cd "$cwd" && bounded 30 env -i "PATH=$PATH" "LEAN_PATH=$lean_path" "$lean_cmd" --run "$work/cwd.lean")
  [ "$actual" = "$cwd" ] || { echo "FAIL slice0_determinism: child cwd $actual != $cwd" >&2; exit 1; }
done
[ "$root" != "$root/scripts" ] || exit 1
echo "ok slice0_determinism: effective Lean child CWDs distinct: $root / $root/scripts"
manifest=${CONCRETE_TEST_MANIFEST:-tests/manifest.tsv}
bounded 30 python3 -I "$root/scripts/checks/lib/test_manifest.py" rows --kind bir "$manifest" >"$work/rows"
[ -s "$work/rows" ] || { echo "determinism: no bir tests" >&2; exit 1; }
awk -F '\t' '{print $1}' "$work/rows" >"$work/names"
awk -F '\t' '{sub(/^bir:/, "", $2); print $2}' "$work/rows" | LC_ALL=C sort >"$work/manifest-programs"
bounded 120 "$lake_cmd" --dir "$root" env lean --run "$driver" list | LC_ALL=C sort >"$work/emitter-programs"
cmp -s "$work/manifest-programs" "$work/emitter-programs" ||
  { echo "determinism: test manifest inventory differs from emitter" >&2; exit 1; }
printf 'one compiler build; per-test determinism\n' >"$work/identity"
emitted=bir.txt,llvm.ll,lcb_job.txt,lcb_source.txt,lcb_body.ssa,lcb_body.s,lcb_rt.ssa,status,stderr
all=$emitted,lcb_rt.s,llvm.o,lcb_body.o,lcb_rt.o,llvm.exe,lcb.exe
compare=(python3 -I "$root/scripts/checks/lib/repro_compare.py" --inventory --label slice0_determinism)

locale_a=C
locale_b=
unsupported_locales=()
for candidate in en_US.UTF-8 C.UTF-8 POSIX; do
  if env -i PATH="$PATH" LC_ALL="$candidate" LANG="$candidate" locale >/dev/null 2>"$work/locale.err" &&
      [ ! -s "$work/locale.err" ]; then
    if [ -z "$locale_b" ]; then
      locale_b=$candidate
    fi
  else
    unsupported_locales+=("$candidate")
  fi
done
[ -n "$locale_b" ] || locale_b=C
tz_a=UTC0
tz_b=UTC-5:30
umask_a=022
umask_b=077
echo "info slice0_determinism: perturbations: locale=$locale_a/$locale_b tz=$tz_a/$tz_b umask=$umask_a/$umask_b; unsupported: non-UTF-8 locales (UTF-8 site paths), TZDIR-dependent Olson names under env -i${unsupported_locales:+ (${unsupported_locales[*]})}"

a=$work/first/output-with-one-absolute-path
b=$work/second/a-different-and-deliberately-longer-absolute-output-path
for side in "$a" "$b"; do
  mkdir -p "$side/out" "$side/home" "$side/tmp"
  cp "$work/names" "$side/names"
done

emit_one() { # emit_one SIDE CWD NAME PROGRAM EXPECT [PLANT]
  local side=$1 cwd=$2 name=$3 program=$4 expect=$5 plant=${6:-}
  local out=$side/out/$name mask
  local -a expectation plant_args envs
  read -r -a expectation <<<"$expect"
  plant_args=()
  [ -z "$plant" ] || plant_args=(--allow-planted-failure --plant "$plant")
  if [ "$side" = "$a" ]; then
    mask=$umask_a
    envs=("PATH=$PATH" "HOME=$side/home" "TMPDIR=$side/tmp" "LC_ALL=$locale_a" "LANG=$locale_a"
      "LEAN_PATH=$lean_path" "TZ=$tz_a" SOURCE_DATE_EPOCH=1 ZERO_AR_DATE=1)
  else
    mask=$umask_b
    envs=("LEAN_PATH=$lean_path" ZERO_AR_DATE=1 SOURCE_DATE_EPOCH=1 "TZ=$tz_b" "LANG=$locale_b" "LC_ALL=$locale_b"
      "TMPDIR=$side/tmp" "HOME=$side/home" "PATH=$PATH")
  fi
  (umask "$mask" && cd "$cwd" && bounded 120 env -i "${envs[@]}" "$lean_cmd" --run "$driver" emit \
    "${program#bir:}" "$out" "$target" "${expectation[@]}" ${plant_args[@]+"${plant_args[@]}"})
}

native_one() { # native_one SIDE NAME
  local side=$1
  local name=$2
  local out=$side/out/$name
  local log=$side/$name.native.log
  local mask
  local -a envs
  if [ "$side" = "$a" ]; then
    mask=$umask_a
    envs=("PATH=$PATH" "HOME=$side/home" "TMPDIR=$side/tmp" "LC_ALL=$locale_a" "LANG=$locale_a"
      "LEAN_PATH=$lean_path" "TZ=$tz_a" SOURCE_DATE_EPOCH=1 ZERO_AR_DATE=1 "CONCRETE_SLICE0_CLANG=$CONCRETE_SLICE0_CLANG")
  else
    mask=$umask_b
    envs=("CONCRETE_SLICE0_CLANG=$CONCRETE_SLICE0_CLANG" ZERO_AR_DATE=1 SOURCE_DATE_EPOCH=1 "TZ=$tz_b"
      "LANG=$locale_b" "LC_ALL=$locale_b" "TMPDIR=$side/tmp" "HOME=$side/home" "PATH=$PATH")
  fi
  if ! (umask "$mask" && cd "$side" && bounded 300 env -i "${envs[@]}" \
      bash "$root/scripts/checks/lib/slice0_native.sh" "$out" "$lcb") >"$log" 2>&1; then
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
  "${compare[@]}" --required "$emitted" "$same/a" "$same/b" "$work/identity"
done <"$work/rows"

required=$all
[ "$emit_only" = 0 ] || required=$emitted
"${compare[@]}" --required "$required" "$a" "$b" "$work/identity" "$work/manifest.txt"
awk -v first="$first_name/" '
  /^[0-9a-f]{64}  / {
    is_first = (index($2, first) == 1)
    if (seen_other && is_first) exit 1
    if (!is_first) seen_other = 1
    if (is_first) count++
  }
  END { if (count < 2 || !seen_other) exit 1 }
' "$work/manifest.txt" || { echo "FAIL slice0_determinism: manifest is not in example-major order" >&2; exit 1; }
echo 'ok slice0_determinism: clean double run'

control_compare() { # control_compare LABEL EXPECTED_REGEX SIDE_A SIDE_B REQUIRED
  local label=$1 pattern=$2 ca=$3 cb=$4 selected=$5 rc=0
  local log=$work/control-$label.log
  "${compare[@]}" --required "$selected" "$ca" "$cb" "$work/identity" >"$log" 2>&1 || rc=$?
  if [ "$rc" != 1 ] || [ "$(grep -c '^FAIL ' "$log")" != 1 ] || ! grep -Eq "$pattern" "$log"; then
    echo "FAIL slice0_determinism control $label: comparator status $rc did not match exact diagnostic '$pattern'" >&2
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
  # Reuse emit_one/native_one environment variations by making the control paths the two known sides.
  old_a=$a old_b=$b
  a=$c/ca b=$c/cb
  emit_one "$a" "$root" "$first_name" "$first_program" "$first_expect" "$plant"
  # The CWD plant needs distinct CWDs; the time plant is deliberately run from the same CWD.
  cwd=$root
  [ "$plant" != nondeterministic ] || cwd=$root/scripts
  emit_one "$b" "$cwd" "$first_name" "$first_program" "$first_expect" "$plant"
  if [ "$emit_only" = 0 ]; then
    native_one "$a" "$first_name"
    native_one "$b" "$first_name"
  fi
  a=$old_a b=$old_b
  artifact=llvm.ll
  [ "$plant" != monotonic ] || artifact=lcb_body.s
  control_compare "$plant" \
    "^FAIL slice0_determinism: ${first_name}: ${artifact//./\\.}: [0-9a-f]{64} != [0-9a-f]{64}$" \
    "$c/ca" "$c/cb" "$required"
done

c=$work/control-missing
mkdir -p "$c"
cp -R "$a" "$c/a"
cp -R "$a" "$c/b"
rm "$c/b/out/$first_name/lcb_body.s"
control_compare missing-artifact \
  "^FAIL slice0_determinism: ${first_name}: lcb_body\\.s: missing \\(.*/out/${first_name}/lcb_body\\.s\\)$" \
  "$c/a" "$c/b" "$required"

c=$work/control-extra
mkdir -p "$c"
cp -R "$a" "$c/a"
cp -R "$a" "$c/b"
touch "$c/b/out/$first_name/unexpected.extra"
control_compare extra-artifact \
  "^FAIL slice0_determinism: ${first_name}: unexpected\\.extra: inventory differs \\(only in second side\\)$" \
  "$c/a" "$c/b" "$required"

c=$work/control-extra-top-file
mkdir -p "$c"
cp -R "$a" "$c/a"
cp -R "$a" "$c/b"
touch "$c/b/out/unexpected.top"
control_compare extra-top-file \
  "^FAIL slice0_determinism: unexpected\\.top: unexpected top-level file in out/ \\(second side\\)$" \
  "$c/a" "$c/b" "$required"

c=$work/control-extra-program-dir
mkdir -p "$c"
cp -R "$a" "$c/a"
cp -R "$a" "$c/b"
mkdir "$c/b/out/unexpected_prog"
control_compare extra-program-dir \
  "^FAIL slice0_determinism: unexpected_prog: unexpected program directory in out/ \\(second side\\)$" \
  "$c/a" "$c/b" "$required"

# Early registered bytes take precedence over every later inventory defect together.
c=$work/control-combined
mkdir -p "$c"
cp -R "$a" "$c/a"
cp -R "$a" "$c/b"
printf '\ncombined control\n' >>"$c/b/out/$first_name/bir.txt"
mkdir "$c/b/out/$first_name/later-directory" "$c/b/out/unexpected_prog"
touch "$c/b/out/unexpected.top"
control_compare combined \
  "^FAIL slice0_determinism: ${first_name}: bir\\.txt: [0-9a-f]{64} != [0-9a-f]{64}$" \
  "$c/a" "$c/b" "$required"

read -r -a first_expectation <<<"$first_expect"
for plant in nondeterministic monotonic; do
  rc=0
  (cd "$root" && CAMPAIGN_EXPECT_FAILURE=1 bounded 120 "$lake_cmd" --dir "$root" env lean --run "$driver" emit "${first_program#bir:}" \
    "$work/refused-$plant" "$target" "${first_expectation[@]}" --plant "$plant") >"$work/refusal-$plant.log" 2>&1 || rc=$?
  if [ "$rc" != 2 ] || ! grep -qxF 'invalid expectation, or a plant without --allow-planted-failure' "$work/refusal-$plant.log"; then
    echo "FAIL slice0_determinism control refusal ($plant): plant did not require the explicit flag" >&2
    cat "$work/refusal-$plant.log" >&2
    exit 1
  fi
done
echo 'ok slice0_determinism control refusal: explicit flag required'
cp "$work/manifest.txt" "$report/manifest.txt"
echo 'PASS slice0_determinism'
