#!/usr/bin/env bash
# PROFILE THE COMPILER WITH LEANPROF ON FIXED WORKLOADS, AND SAY WHERE IT GOT SLOWER (R-0551).
#
# The instruction-count tracker (scripts/perf/, docs/project/PERF_TRACKING.md) says THAT a commit
# made the compiler slower; this says WHERE: the time per compiler stage and the top hot spots of
# each workload, compared with a previous profile. It is portable: it needs a built compiler, a
# leanprof binary (github.com/unbalancedparentheses/leanprof) and, in sample mode, samply. The
# scheduled job that runs it at every main commit, keeps the history and publishes the summary is
# host provisioning and lives outside this repository (the owner's dotfiles, servers/).
#
#   leanprof_profile.sh profile --leanprof BIN --out DIR [options]
#   leanprof_profile.sh compare BASE.json HEAD.json [--out DIR] [threshold options]
#   leanprof_profile.sh gen-corpus N
#
# profile options
#   --source DIR         the compiler source tree profiled, and where lox and std come from
#                        (default: this checkout). The scheduled job runs a read-only harness
#                        checkout against a separate clone at the commit it profiles.
#   --compiler BIN       the compiler to profile (default: SOURCE/.lake/build/bin/concrete)
#   --mode auto|sample   auto (default): `leanprof run --auto`, exact call counts, times through
#                        probes; rebuilds SOURCE's compiler with probes for each workload (leanprof
#                        puts the normal build back), ignores --compiler. Works at any
#                        perf_event_paranoid, so it is the CI host's mode. sample: `leanprof
#                        sample`, sampled CPU per declaration, no probes; needs samply and perf
#                        events (perf_event_paranoid <= 1 or CAP_PERFMON).
#   --workloads LIST     comma-separated, default lox,std,scaling:
#                          lox      `concrete build --emit-llvm` in a copy of examples/lox
#                          std      `concrete build --emit-llvm` in a copy of std/
#                          scaling  `concrete FILE --emit-llvm` for each FILE of the scaling corpus
#   --scaling-corpus DIR the *.con files of R-0550's generated corpus, one workload each
#                        (scaling:<stem>). Without it, one program from `gen-corpus`
#                        (--scaling-size, default 5000) until that corpus lands.
#   --reps N             sample mode: repetitions per workload, the profile is their median (3)
#   --rate HZ            sample mode: sampling rate (2000)
#   --top N              hot spots kept per workload (15)
#   --cores LIST         run leanprof under `taskset -c LIST` (the host fixes the clock)
#   --budget SECS        bounded_run budget per leanprof run (sample 300, auto 1200)
#   --baseline FILE      a previous profile.json: also write comparison.json/.md
#   --sha SHA            the commit profiled (default: this checkout's HEAD)
# compare options: --grow-pts --grow-ratio --new-pts --new-base-max (defaults: THRESHOLDS in
#   leanprof_profile.py, recorded in every comparison).
#
# OUTPUT (DIR): profile.json (schema concrete-leanprof-profile/1), summary.md, raw/ (leanprof's
# reports), and with a baseline comparison.json and comparison.md (summary with the findings).
# EXIT: 0 no finding; 1 a finding against the baseline; 2 usage, environment or unreadable input;
#   3 a workload failed or was incomplete (timed out twice under bounded_run: no observation,
#   never a pass) — the profile is still written, with that workload's status.
#
# `profile` runs inside the nix devshell (`nix develop`: lake, clang and the toolchain the
# compiler was built with); it refuses to start outside it. compare and gen-corpus need python3
# only. Gate: scripts/tests/check_leanprof_profile.sh (synthetic profiles and a fake leanprof).
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
PY="$ROOT_DIR/scripts/perf/leanprof_profile.py"
die() { echo "leanprof_profile: $*" >&2; exit 2; }
log() { echo "[leanprof_profile] $*" >&2; }

cmd="${1:-}"; [ $# -gt 0 ] && shift
case "$cmd" in
  gen-corpus) exec python3 "$PY" gen-corpus "$@" ;;
  compare)
    [ $# -ge 2 ] || die "usage: leanprof_profile.sh compare BASE.json HEAD.json [--out DIR] [thresholds]"
    base="$1" head="$2"; shift 2
    out=""; th=()
    while [ $# -gt 0 ]; do
      case "$1" in
        --out) [ $# -ge 2 ] || die "--out needs a directory"; out="$2"; shift 2 ;;
        --grow-pts|--grow-ratio|--new-pts|--new-base-max) [ $# -ge 2 ] || die "$1 needs a value"; th+=("$1" "$2"); shift 2 ;;
        *) die "compare: unknown option $1" ;;
      esac
    done
    if [ -n "$out" ]; then
      mkdir -p "$out" || die "cannot create $out"
      exec python3 "$PY" compare "$base" "$head" --json "$out/comparison.json" --md "$out/comparison.md" "${th[@]}"
    fi
    exec python3 "$PY" compare "$base" "$head" "${th[@]}" ;;
  profile) ;;
  *) die "usage: leanprof_profile.sh profile|compare|gen-corpus ... (header of $0)" ;;
esac

LEANPROF="" OUT="" SRC="$ROOT_DIR" COMPILER="" MODE=auto
WORKLOADS="lox,std,scaling" CORPUS="" SCALING_SIZE=5000 REPS=3 RATE=2000 TOP=15 CORES="" BUDGET=""
BASELINE="" SHA=""
nat() { [[ "$2" =~ ^[1-9][0-9]*$ ]] || die "$1 must be a positive integer (got '$2')"; }
while [ $# -gt 0 ]; do
  [ $# -ge 2 ] || die "$1 needs a value"
  case "$1" in
    --leanprof) LEANPROF="${2:-}"; shift 2 ;;
    --out) OUT="${2:-}"; shift 2 ;;
    --source) SRC="${2:-}"; shift 2 ;;
    --compiler) COMPILER="${2:-}"; shift 2 ;;
    --mode) MODE="${2:-}"; shift 2 ;;
    --workloads) WORKLOADS="${2:-}"; shift 2 ;;
    --scaling-corpus) CORPUS="${2:-}"; shift 2 ;;
    --scaling-size) nat "$1" "${2:-}"; SCALING_SIZE="$2"; shift 2 ;;
    --reps) nat "$1" "${2:-}"; REPS="$2"; shift 2 ;;
    --rate) nat "$1" "${2:-}"; RATE="$2"; shift 2 ;;
    --top) nat "$1" "${2:-}"; TOP="$2"; shift 2 ;;
    --cores) CORES="${2:-}"; shift 2 ;;
    --budget) nat "$1" "${2:-}"; BUDGET="$2"; shift 2 ;;
    --baseline) BASELINE="${2:-}"; shift 2 ;;
    --sha) SHA="${2:-}"; shift 2 ;;
    *) die "profile: unknown option $1" ;;
  esac
done
[ -n "$LEANPROF" ] && [ -x "$LEANPROF" ] || die "--leanprof BIN: an executable leanprof is required"
[ -n "$OUT" ] || die "--out DIR is required"
[ -d "$SRC/Concrete" ] && { [ -f "$SRC/lakefile.toml" ] || [ -f "$SRC/lakefile.lean" ]; } || die "--source $SRC is not a concrete source tree"
SRC="$(cd "$SRC" && pwd)"
[ -n "$COMPILER" ] || COMPILER="$SRC/.lake/build/bin/concrete"
case "$MODE" in sample|auto) ;; *) die "--mode must be sample or auto (got '$MODE')" ;; esac
[ "$MODE" = auto ] || [ -x "$COMPILER" ] || die "compiler $COMPILER is not executable (build it, or pass --compiler)"
[ -z "$BASELINE" ] || [ -f "$BASELINE" ] || die "baseline $BASELINE not found"
[ -z "$CORPUS" ] || [ -d "$CORPUS" ] || die "scaling corpus $CORPUS is not a directory"
# The devshell, not an inherited IN_NIX_SHELL (scripts/land/lib.sh, land_in_nix): both the marker
# and the shell's lake.
[ -n "${IN_NIX_SHELL:-}" ] && command -v lake >/dev/null 2>&1 \
  || die "profile runs inside the nix devshell: nix develop $SRC --command $0 profile ..."
TASKSET=()
if [ -n "$CORES" ]; then
  command -v taskset >/dev/null 2>&1 || die "--cores needs taskset"
  TASKSET=(taskset -c "$CORES")
fi
[ "$MODE" = sample ] || REPS=1
[ -n "$BUDGET" ] || { [ "$MODE" = sample ] && BUDGET=300 || BUDGET=1200; }
[ -n "$SHA" ] || SHA="$(git -C "$SRC" rev-parse HEAD 2>/dev/null || echo unknown)"

mkdir -p "$OUT/raw" || die "cannot create $OUT"
OUT="$(cd "$OUT" && pwd)"
WORK="$(mktemp -d)" || die "mktemp failed"
trap 'rm -rf "$WORK"' EXIT
BOUNDED_PARENT="$WORK"
# shellcheck source=scripts/tests/lib/bounded.sh
source "$ROOT_DIR/scripts/tests/lib/bounded.sh"
export PYTHONDONTWRITEBYTECODE=1

# One workload: NAME, the directory it runs in, then the compiler's arguments.
# SAFE: the current workload's file stem, numbered so that two names that sanitize alike
# (scaling:a+b, scaling:a_b) never share raw reports or a summary.
WL_FILES=(); STATUS=0; WL_N=0; SAFE=""
next_safe() { WL_N=$((WL_N + 1)); SAFE="$(printf '%02d' "$WL_N")_${1//[^A-Za-z0-9_.-]/_}"; }
profile_one() {
  local name="$1" dir="$2"; shift 2
  local safe="$SAFE" reports=() r rc lp
  for r in $(seq 1 "$REPS"); do
    local json="$OUT/raw/$safe-$r.json"
    rm -f "$json"
    if [ "$MODE" = sample ]; then
      lp=("${TASKSET[@]}" "$LEANPROF" sample --rate "$RATE" --top "$TOP" --out "$WORK/$safe-$r.samply.json"
          --json "$json" --cwd "$dir" "$COMPILER" -- "$@")
    else
      lp=("${TASKSET[@]}" "$LEANPROF" run --auto --no-deps --json "$json" --cwd "$dir" concrete -- "$@")
    fi
    # leanprof runs from the compiler's source tree: that is where it places declarations.
    # The compiler's own output (LLVM IR on stdout) and leanprof's report text go to the log.
    ( cd "$SRC" && bounded_run "$BUDGET" -- "${lp[@]}" ) > "$WORK/$safe-$r.log" 2>&1
    rc=$?
    if bounded_is_incomplete "$rc"; then
      log "$name: INCOMPLETE (leanprof timed out twice at ${BUDGET}s)"
      wl_status "$name" incomplete "leanprof timed out twice (budget ${BUDGET}s, then the retry)"
      return
    fi
    if [ "$rc" -ne 0 ] || ! python3 "$PY" validate "$json" 2> "$WORK/$safe.err"; then
      log "$name: FAILED (leanprof exit $rc)"; tail -5 "$WORK/$safe-$r.log" >&2; cat "$WORK/$safe.err" >&2 2>/dev/null
      wl_status "$name" failed "leanprof exit $rc: $(tail -1 "$WORK/$safe.err" 2>/dev/null || true)"
      return
    fi
    reports+=("$json")
  done
  if python3 "$PY" summarize --workload "$name" --mode "$MODE" --top "$TOP" "${reports[@]}" > "$WORK/wl-$safe.json"; then
    WL_FILES+=("$WORK/wl-$safe.json"); log "$name: ok"
  else
    wl_status "$name" failed "the reports could not be summarized"
  fi
}
wl_status() {  # NAME STATUS DETAIL: a workload without an observation
  local f="$WORK/wl-$SAFE.json"
  python3 -c 'import json,sys; json.dump({"name": sys.argv[1], "status": sys.argv[2], "detail": sys.argv[3], "stages": [], "hotspots": []}, open(sys.argv[4], "w"))' \
    "$1" "$2" "$3" "$f"
  WL_FILES+=("$f"); STATUS=3
}
copy_pkg() {  # SRC NAME -> a private copy, so the build writes nothing into the checkout
  rm -rf "${WORK:?}/$2"; cp -R "$1" "$WORK/$2" && echo "$WORK/$2"
}

IFS=',' read -r -a WL <<< "$WORKLOADS"
for w in "${WL[@]}"; do
  case "$w" in
    lox|std)
      next_safe "$w"
      src="$SRC/std"; [ "$w" = std ] || src="$SRC/examples/lox"
      if d="$(copy_pkg "$src" "$w")"; then profile_one "$w" "$d" build --emit-llvm
      else log "$w: FAILED (cannot copy $src)"; wl_status "$w" failed "cannot copy $src"; fi ;;
    scaling)
      mkdir -p "$WORK/scaling"
      if [ -n "$CORPUS" ]; then
        found=0
        for f in "$CORPUS"/*.con; do
          [ -f "$f" ] || continue; found=1
          cp "$f" "$WORK/scaling/"; next_safe "scaling:$(basename "$f" .con)"; profile_one "scaling:$(basename "$f" .con)" "$WORK/scaling" "$(basename "$f")" --emit-llvm
        done
        [ "$found" = 1 ] || die "scaling corpus $CORPUS has no .con files"
      else
        python3 "$PY" gen-corpus "$SCALING_SIZE" > "$WORK/scaling/scaling.con" || die "gen-corpus failed"
        next_safe scaling; profile_one scaling "$WORK/scaling" scaling.con --emit-llvm
      fi ;;
    *) die "unknown workload '$w' (lox, std, scaling)" ;;
  esac
done

# leanprof has no --version: its git commit when it runs from a checkout, else the binary's hash.
LPV="$(git -C "$(dirname "$LEANPROF")" rev-parse --short=12 HEAD 2>/dev/null)"
[ -z "$LPV" ] || LPV="git:$LPV"
[ -n "$LPV" ] || LPV="sha256:$(python3 -c 'import hashlib,sys; print(hashlib.sha256(open(sys.argv[1],"rb").read()).hexdigest()[:16])' "$LEANPROF")"
CV="unknown"; [ "$MODE" = auto ] || CV="$("$COMPILER" --version 2>/dev/null | head -1)"
python3 - "$WORK/meta.json" "$SHA" "$MODE" "$REPS" "$RATE" "$LPV" "$CV" "$SCALING_SIZE" "$CORPUS" "$CORES" <<'PY'
import json, socket, sys, time
o, sha, mode, reps, rate, lpv, cv, size, corpus, cores = sys.argv[1:]
json.dump({"sha": sha, "mode": mode, "reps": int(reps), "rate": int(rate), "leanprof": lpv,
           "compiler": cv, "machine": socket.gethostname().split(".")[0],
           "date": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
           "scaling": {"corpus": corpus or None, "size": None if corpus else int(size)},
           "cores": cores or None}, open(o, "w"))
PY
[ "${#WL_FILES[@]}" -gt 0 ] || die "no workload was profiled"
python3 "$PY" assemble --meta "$WORK/meta.json" --out "$OUT/profile.json" "${WL_FILES[@]}" || die "assemble failed"
python3 "$PY" render "$OUT/profile.json" > "$OUT/summary.md" || die "render failed"
bounded_summary >&2
if [ -n "$BASELINE" ]; then
  python3 "$PY" compare "$BASELINE" "$OUT/profile.json" --json "$OUT/comparison.json" --md "$OUT/comparison.md"
  crc=$?
  [ "$crc" -le 1 ] || exit 2
  [ "$STATUS" -ne 0 ] || STATUS="$crc"
fi
log "profile in $OUT (status $STATUS)"
exit "$STATUS"
