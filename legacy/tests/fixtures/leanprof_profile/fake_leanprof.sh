#!/usr/bin/env bash
# A stand-in for leanprof in check_leanprof_profile.sh: it runs the program the way leanprof would
# (in --cwd, with its arguments) and writes a synthetic report (synth.py) where --json says.
#   FAKE_LP_VARIANT  synth.py variant (default base)     FAKE_LP_SEED  seed (default 1)
#   FAKE_LP_FAIL     a substring of the --json path: exit 1 without a report for that workload
#   FAKE_LP_HANG     a substring of the --json path: sleep past any budget for that workload
#   FAKE_LP_LOG      append "MODE|cwd|program args" per run
set -uo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
[ "${1:-}" = --version ] && { echo "fake-leanprof 0"; exit 0; }
mode="$1"; shift
json="" cwd="." rep=1
case "$mode" in sample|run) ;; *) echo "fake leanprof: unknown command $mode" >&2; exit 2 ;; esac
[ "$mode" = run ] && mode=auto
while [ $# -gt 0 ] && [ "$1" != -- ]; do
  case "$1" in
    --json) json="$2"; shift 2 ;;
    --cwd) cwd="$2"; shift 2 ;;
    --rate|--top|--out) shift 2 ;;
    --auto|--no-deps) shift ;;
    *) exe="$1"; shift ;;
  esac
done
shift
[ -n "$json" ] || { echo "fake leanprof: no --json" >&2; exit 2; }
[ -z "${FAKE_LP_HANG:-}" ] || [[ "$json" != *"$FAKE_LP_HANG"* ]] || sleep 600
[ -z "${FAKE_LP_FAIL:-}" ] || [[ "$json" != *"$FAKE_LP_FAIL"* ]] || { echo "fake leanprof: planted failure" >&2; exit 1; }
[ -z "${FAKE_LP_LOG:-}" ] || echo "$mode|$cwd|$exe $*" >> "$FAKE_LP_LOG"
if [ "$mode" = sample ]; then ( cd "$cwd" && "$exe" "$@" ) || exit $?; fi
[[ "$json" =~ -([0-9]+)\.json$ ]] && rep="${BASH_REMATCH[1]}"
python3 "$HERE/synth.py" --mode "$mode" --variant "${FAKE_LP_VARIANT:-base}" --seed "${FAKE_LP_SEED:-1}" --rep "$rep" "$json"
