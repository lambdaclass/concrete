#!/usr/bin/env bash
# THE LANDING WORKFLOW ACCEPTANCE TEST (ROADMAP R-0563 on the new roadmap): does a fresh clone still build, hook, run CI
# at the exact SHA and get as far as a landing, and do the refusals still refuse?
#
# Run it before the legacy/ move (on the current tree) and again on the moved tree, before that move
# lands. It drives the real scripts/land/train.sh from a FRESH clone, so nothing in this checkout,
# its state directory or its hooks configuration can make it pass. It never lands: the only `land`
# it runs is `land --dry-run`, and it refuses to start when LAND_* overrides point at other remotes.
#
#   acceptance.sh [--ref REF] [--name NAME] [--skip LEG]... [--run-id ID] [--keep]
#
#   --ref REF      what to accept, as a train member (default origin/main; a wip/ branch works)
#   --name NAME    train name (default accept-<short sha of REF>)
#   --skip LEG     skip one leg; a skipped leg makes the verdict `incomplete`, never `accepted`
#   --run-id ID    reuse a completed CI run of this exact train head instead of dispatching one
#   --keep         keep the clone
#
# Legs, in order (each stops the run when it fails, except the negative controls, which all run):
#   clone    git clone of origin, both remotes configured, hooks installed (core.hooksPath)
#   build    ACCEPT_BUILD_CMD in the clone (default: make build)
#   train    train.sh build <name> <ref>
#   check    train.sh check <name> --no-review (the hook's impact selection over base..head)
#   ci       train.sh ci <name>: push ci/<name>, dispatch CI, record the run of the exact head
#   watch    train.sh watch <name>: the run's head SHA, landing tier and required-checks.txt
#   dryland  train.sh --dry-run land <name>: every land check, nothing pushed
#   negative the same watch and dry-run land against the same run with its verdict tampered (a gh
#            wrapper edits the JSON): a required job removed, a required job skipped, a required
#            job cancelled, the run's head SHA changed. Each must be refused by BOTH commands.
#
# Output: one line per leg (passed | failed | skipped) and a verdict: accepted (every leg passed),
# incomplete (none failed, some skipped), or failed. Results are bound to the train's head SHA and
# appended to <state>/acceptance.tsv. Exit 0 only for `accepted`.
set -uo pipefail

SELF_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REF=origin/main NAME="" RUN_ID="" KEEP=0
declare -A SKIP=()
while [ $# -gt 0 ]; do
  case "$1" in
    --ref) REF="$2"; shift ;;
    --name) NAME="$2"; shift ;;
    --skip) SKIP["$2"]=1; shift ;;
    --run-id) RUN_ID="$2"; shift ;;
    --keep) KEEP=1 ;;
    -h|--help) sed -n '2,34p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
    *) echo "acceptance: unknown argument $1" >&2; exit 2 ;;
  esac; shift
done
for v in LAND_REMOTE LAND_MIRROR LAND_REPO LAND_STATE_DIR LAND_GH; do
  [ -z "${!v+x}" ] || { echo "acceptance: refusing to run with $v set (the test uses the real defaults)" >&2; exit 2; }
done

SRC_REPO="$(git rev-parse --show-toplevel 2>/dev/null)" || { echo "acceptance: run inside a clone" >&2; exit 2; }
ORIGIN_URL="$(git -C "$SRC_REPO" remote get-url origin)"
MIRROR_URL="$(git -C "$SRC_REPO" remote get-url lambdaclass 2>/dev/null || true)"
WORK="$(mktemp -d "${TMPDIR:-/tmp}/acceptance.XXXXXX")"
[ "$KEEP" = 1 ] || trap 'rm -rf "$WORK"' EXIT
C="$WORK/clone"
declare -a ROWS=()
FAILED=0 SKIPPED=0 HEAD=""
row() { ROWS+=("$1"$'\t'"$2"$'\t'"$3"); printf '  %-8s %-8s %s\n' "$1" "$2" "$3"; }
leg() { # name cmd... — runs unless skipped; a failure ends the run (the negative leg has its own loop)
  local n="$1"; shift
  if [ -n "${SKIP[$n]:-}" ]; then row "$n" skipped "--skip"; SKIPPED=1; return 0; fi
  local log="$WORK/$n.log"
  if "$@" > "$log" 2>&1; then row "$n" passed "$(tail -n 1 "$log" | cut -c1-100)"; return 0; fi
  row "$n" failed "see $log"; tail -n 30 "$log" | sed 's/^/      /'; FAILED=1; finish
}
finish() {
  local verdict=accepted
  [ "$SKIPPED" = 1 ] && verdict=incomplete
  [ "$FAILED" = 1 ] && verdict=failed
  local rec="${ACCEPT_RECORD:-$SRC_REPO/.git/concrete-landing/acceptance.tsv}"
  mkdir -p "$(dirname "$rec")"
  for r in "${ROWS[@]}"; do printf '%s\t%s\t%s\t%s\n' "$(date -u +%Y-%m-%dT%H:%M:%SZ)" "${HEAD:-?}" "$NAME" "$r"; done >> "$rec"
  printf '%s\t%s\t%s\tverdict\t%s\t\n' "$(date -u +%Y-%m-%dT%H:%M:%SZ)" "${HEAD:-?}" "$NAME" "$verdict" >> "$rec"
  local kept=""; [ "$KEEP" = 1 ] && kept="; clone $C"
  echo "acceptance: $verdict at ${HEAD:-?} (train $NAME; record $rec$kept)"
  [ "$verdict" = accepted ]; exit $?
}

TRAIN() { ( cd "$C" && bash scripts/land/train.sh "$@" ); }

do_clone() {
  git clone --quiet "$ORIGIN_URL" "$C" && cd "$C" || return 1
  [ -z "$MIRROR_URL" ] || git remote add lambdaclass "$MIRROR_URL"
  git config core.hooksPath .githooks
  git fetch --quiet origin '+refs/heads/wip/*:refs/remotes/origin/wip/*' || true
  git rev-parse --verify --quiet "$REF^{commit}" >/dev/null || { echo "no ref $REF in the fresh clone"; return 1; }
  [ -f scripts/land/train.sh ] || { echo "no scripts/land/train.sh"; return 1; }
  echo "cloned $ORIGIN_URL; $REF = $(git rev-parse "$REF")"
}
do_build() { cd "$C" && bash -c "${ACCEPT_BUILD_CMD:-make build}"; }
do_train() { TRAIN build "$NAME" "$REF" && HEAD="$(git -C "$C" rev-parse HEAD)" && echo "train head $HEAD"; }
do_check() { TRAIN check "$NAME" --no-review --no-remote; }
do_ci() {
  if [ -n "$RUN_ID" ]; then
    local f; f="$(cd "$C" && cd "$(git rev-parse --git-common-dir)" && pwd)/concrete-landing/$NAME/train.env"
    grep -v '^run_id=' "$f" > "$f.tmp"; echo "run_id=$RUN_ID" >> "$f.tmp"; mv "$f.tmp" "$f"
    echo "reusing run $RUN_ID (watch checks that it ran $HEAD)"
  else
    TRAIN ci "$NAME"
  fi
}
do_watch() { TRAIN watch "$NAME" --interval 60; }
do_dryland() { TRAIN --dry-run land "$NAME"; }

# The tampering gh: real `gh`, except that `run view --json ...` output is edited per $ACCEPT_TAMPER.
make_tamper_gh() {
  cat > "$WORK/tamper-gh" <<'GH'
#!/usr/bin/env bash
if [ "$1 $2" != "run view" ]; then exec gh "$@"; fi
gh "$@" | python3 -I -c '
import json, sys, os
mode = os.environ["ACCEPT_TAMPER"]; req = os.environ["ACCEPT_REQ"]
d = json.load(sys.stdin)
jobs = d.get("jobs")
if mode == "sha" and "headSha" in d:
    d["headSha"] = "0" * 40
if jobs is not None:
    if mode == "drop":
        d["jobs"] = [j for j in jobs if j.get("name") != req]
    elif mode in ("skipped", "cancelled"):
        for j in jobs:
            if j.get("name") == req: j["conclusion"] = mode
json.dump(d, sys.stdout)'
GH
  chmod +x "$WORK/tamper-gh"
}
do_negative() {
  if [ -n "${SKIP[negative]:-}" ]; then row negative skipped "--skip"; SKIPPED=1; return 0; fi
  make_tamper_gh
  local req; req="$(sed 's/#.*//; s/^[[:space:]]*//; s/[[:space:]]*$//' "$C/required-checks.txt" 2>/dev/null | sed '/^$/d' | head -1)"
  if [ -z "$req" ]; then row negative failed "no required-checks.txt in the candidate"; FAILED=1; return 0; fi
  local mode cmd out rc
  for mode in drop skipped cancelled sha; do
    for cmd in watch land; do
      if [ "$cmd" = watch ]; then
        out="$(cd "$C" && ACCEPT_TAMPER=$mode ACCEPT_REQ="$req" LAND_GH="$WORK/tamper-gh" bash scripts/land/train.sh watch "$NAME" --interval 1 --timeout 1 2>&1)"; rc=$?
      else
        out="$(cd "$C" && ACCEPT_TAMPER=$mode ACCEPT_REQ="$req" LAND_GH="$WORK/tamper-gh" bash scripts/land/train.sh --dry-run land "$NAME" 2>&1)"; rc=$?
      fi
      if [ "$rc" != 0 ]; then
        local why; why="$(printf '%s\n' "$out" | awk '/train:|required|missing|not the train/ && !n {print substr($0,1,80); n=1}')"
        row "neg:$mode" passed "$cmd refused ($why)"
      else
        row "neg:$mode" failed "$cmd ACCEPTED a tampered run ($req)"; FAILED=1
      fi
    done
  done
}

echo "acceptance: $REF from $ORIGIN_URL"
leg clone do_clone
NAME="${NAME:-accept-$(git -C "$C" rev-parse --short=10 "$REF")}"
leg build do_build
leg train do_train
leg check do_check
leg ci do_ci
leg watch do_watch
leg dryland do_dryland
do_negative
finish
