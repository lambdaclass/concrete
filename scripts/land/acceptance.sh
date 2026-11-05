#!/usr/bin/env bash
# THE LANDING WORKFLOW ACCEPTANCE TEST (ROADMAP R-0563 on the new roadmap): does a fresh clone still build, hook, run CI
# at the exact SHA and get as far as a landing, and do the refusals still refuse?
#
# Run it before the legacy/ move (on the current tree) and again on the moved tree, before that move
# lands. It drives the real scripts/land/train.sh from a FRESH clone, so nothing in this checkout,
# its state directory or its hooks configuration can make it pass.
#
# IT NEVER LANDS, and that does not rest on the script under test honouring --dry-run. Three
# independent guards: a `git` shim first on PATH for every leg refuses any push whose destination is
# main (or --all, --mirror, a bare HEAD), whatever flags the candidate's scripts pass, --no-verify
# included; the clone's hooks path is a pre-push guard that refuses the same before it runs the real
# hook; and both remotes' main is read before and after, a move to the train head being a failure.
# The train head's publishing scripts are also refused if they run `git push ... --no-verify`. It refuses to start with any LAND_* or CONCRETE_SKIP_* variable set, since those
# redirect remotes, swap the hook or skip the gates.
#
#   acceptance.sh --ref REF [--name NAME] [--skip LEG]... [--run-id ID] [--keep]
#
#   --ref REF      what to accept, as a train member: a ref not yet on main (a wip/ branch); required,
#                  because train.sh refuses a member main already contains
#   --name NAME    train name (default accept-<short sha of REF>)
#   --skip LEG     skip one leg; a skipped leg makes the verdict `incomplete`, never `accepted`
#   --run-id ID    reuse a completed CI run of this exact train head instead of dispatching one
#   --keep         keep the clone
#
# Legs, in order (each stops the run when it fails, except the negative controls, which all run):
#   clone    git clone of origin, both remotes configured, the guarded hooks path installed
#   train    train.sh build <name> <ref>; the train head is the SHA every later result is bound to
#   build    ACCEPT_BUILD_CMD in the clone, at the train head (default: make build)
#   check    train.sh check <name> --no-review (the hook's impact selection over base..head)
#   ci       train.sh ci <name>: push ci/<name>, dispatch CI, record the run of the exact head
#   watch    train.sh watch <name>: the run's head SHA, landing tier and required-checks.txt
#   dryland  train.sh --dry-run land <name>: every land check, nothing pushed
#   negative controls, each of which must FAIL, for its own reason:
#     neg:build  a commit on the train head that breaks the build (ACCEPT_BREAK_BUILD, a shell
#                snippet run in a scratch worktree): ACCEPT_BUILD_CMD must fail, naming the file
#     neg:hook   a commit on the train head that adds a job name to required-checks.txt that no
#                workflow has: the candidate's own pre-push hook, fed that range, must fail
#     neg:<m>    the same watch and dry-run land against the same run with its verdict tampered (a
#                gh wrapper edits the JSON): a required job removed (drop), skipped, cancelled or
#                failed, and the run's head SHA changed (sha). Each must be refused by BOTH commands
#                with that mode's own diagnostic, and the wrapper must record that it changed the
#                verdict; a refusal for any other reason (gh failing, a moved main) fails the control.
#   Before the tampered controls, the real run is read once more: its head SHA must be the train head.
#
# Output: one line per leg (passed | failed | skipped | incomplete) and a verdict: accepted (every
# leg passed), incomplete (none failed, some skipped, or main moved before the dry-run land so
# train.sh refused the stale candidate; re-run), or failed. Results are bound to the train's head SHA and
# appended to <state>/acceptance.tsv. Exit 0 only for `accepted`.
set -uo pipefail

SELF_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REF="" NAME="" RUN_ID="" KEEP=0
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
[ -n "$REF" ] || { echo "acceptance: --ref is required (a ref not yet on main)" >&2; exit 2; }
bad_env="$(compgen -e | grep -E '^(LAND_|CONCRETE_SKIP_)' || true)"
[ -z "$bad_env" ] || { echo "acceptance: refusing to run with $(echo $bad_env) set (remote, hook or gate overrides)" >&2; exit 2; }

SRC_REPO="$(git rev-parse --show-toplevel 2>/dev/null)" || { echo "acceptance: run inside a clone" >&2; exit 2; }
ORIGIN_URL="$(git -C "$SRC_REPO" remote get-url origin)"
MIRROR_URL="$(git -C "$SRC_REPO" remote get-url lambdaclass 2>/dev/null || true)"
WORK="$(mktemp -d "${TMPDIR:-/tmp}/acceptance.XXXXXX")"
[ "$KEEP" = 1 ] || trap 'rm -rf "$WORK"' EXIT
C="$WORK/clone"
declare -a ROWS=()
FAILED=0 SKIPPED=0 HEAD=""
MAIN_BEFORE=""
row() { ROWS+=("$1"$'\t'"$2"$'\t'"$3"); printf '  %-8s %-8s %s\n' "$1" "$2" "$3"; }
leg() { # name cmd... — runs unless skipped; a failure ends the run (the negative leg has its own loop)
  local n="$1"; shift
  if [ -n "${SKIP[$n]:-}" ]; then row "$n" skipped "--skip"; SKIPPED=1; return 0; fi
  local log="$WORK/$n.log"
  if "$@" > "$log" 2>&1; then row "$n" passed "$(tail -n 1 "$log" | cut -c1-100)"; return 0; fi
  if [ "$n" = dryland ] && grep -q "origin/main moved from" "$log"; then
    # Another landing moved main under the run: train.sh refused correctly, but the run proves
    # nothing about this candidate's dry-run land or the tamper controls. Incomplete, never passed.
    row "$n" incomplete "main moved during the run; train.sh refused the stale candidate (see $log)"; SKIPPED=1; finish
  fi
  row "$n" failed "see $log"; tail -n 30 "$log" | sed 's/^/      /'; FAILED=1; finish
}
main_heads() { local r; for r in origin lambdaclass; do git -C "$C" ls-remote "$r" refs/heads/main 2>/dev/null | cut -f1; done | tr '\n' ' '; }
finish() {
  if [ -n "$MAIN_BEFORE" ]; then
    local after; after="$(main_heads)"
    if [ "$after" != "$MAIN_BEFORE" ]; then
      case " $after " in
        *" ${HEAD:-none} "*) row main failed "main moved to the train head: the test LANDED"; FAILED=1 ;;
        *) row main note "main moved during the run (not to the train head): $MAIN_BEFORE-> $after" ;;
      esac
    fi
  fi
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

# The git shim: every leg runs with it first on PATH. It refuses a push that could move main, and
# passes everything else to the real git.
make_git_shim() {
  local real; real="$(command -v git)"
  mkdir -p "$WORK/bin"
  cat > "$WORK/bin/git" <<SHIM
#!/usr/bin/env bash
seen=0
for a in "\$@"; do
  if [ "\$seen" = 0 ]; then [ "\$a" = push ] && seen=1; continue; fi
  case "\$a" in
    --all|--mirror|HEAD|main|refs/heads/main|*:main|*:refs/heads/main|+*:main|+*:refs/heads/main)
      echo "acceptance git shim: refusing push argument '\$a' (could move main)" >&2; exit 1 ;;
  esac
done
exec "$real" "\$@"
SHIM
  chmod +x "$WORK/bin/git"
}
# The guard hooks path: pre-push refuses any main update, then runs the real hook on the same input.
make_guard() {
  mkdir -p "$WORK/guard-hooks"
  cat > "$WORK/guard-hooks/pre-push" <<GUARD
#!/usr/bin/env bash
in="\$(cat)"
while read -r _l _ls rref _rs; do
  case "\$rref" in refs/heads/main|main) echo "acceptance guard: refusing a push to \$rref on \$1" >&2; exit 1 ;; esac
done <<< "\$in"
[ -x "$C/.githooks/pre-push" ] || [ -f "$C/.githooks/pre-push" ] || exit 0
printf '%s\n' "\$in" | bash "$C/.githooks/pre-push" "\$@"
GUARD
  chmod +x "$WORK/guard-hooks/pre-push"
}
do_clone() {
  git clone --quiet "$ORIGIN_URL" "$C" && cd "$C" || return 1
  [ -z "$MIRROR_URL" ] || git remote add lambdaclass "$MIRROR_URL"
  make_guard
  git config core.hooksPath "$WORK/guard-hooks"
  MAIN_BEFORE="$(main_heads)"
  git fetch --quiet origin '+refs/heads/wip/*:refs/remotes/origin/wip/*' || true
  git rev-parse --verify --quiet "$REF^{commit}" >/dev/null || { echo "no ref $REF in the fresh clone"; return 1; }
  [ -f scripts/land/train.sh ] || { echo "no scripts/land/train.sh"; return 1; }
  echo "cloned $ORIGIN_URL; $REF = $(git rev-parse "$REF")"
}
do_build() { # at the train head: the build result belongs to the SHA every other leg tests
  [ "$(git -C "$C" rev-parse HEAD)" = "$HEAD" ] || { echo "the clone is not at the train head"; return 1; }
  cd "$C" && bash -c "${ACCEPT_BUILD_CMD:-make build}" && echo "built $HEAD"
}
do_train() {
  TRAIN build "$NAME" "$REF" && HEAD="$(git -C "$C" rev-parse HEAD)" || return 1
  # The candidate's own publishing scripts, at the train head, may not skip hooks on a push.
  local hits; hits="$(cd "$C" && grep -rnE -- 'git[^#]*push[^#]*--no-verify|--no-verify[^#]*push' scripts/land scripts/push-both.sh 2>/dev/null \
    | grep -v '^scripts/land/acceptance.sh:' | grep -vE '^[^:]+:[0-9]+:[[:space:]]*#' || true)"
  [ -z "$hits" ] || { echo "the train head's landing scripts push with --no-verify:"; echo "$hits"; return 1; }
  echo "train head $HEAD"
}
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
# Each edit it actually makes is appended to $ACCEPT_TAMPER_LOG, so a control can prove its mutation.
make_tamper_gh() {
  cat > "$WORK/tamper-gh" <<'GH'
#!/usr/bin/env bash
if [ "$1 $2" != "run view" ]; then exec gh "$@"; fi
gh "$@" | python3 -I -c '
import json, sys, os
mode = os.environ["ACCEPT_TAMPER"]; req = os.environ["ACCEPT_REQ"]
d = json.load(sys.stdin)
jobs = d.get("jobs")
done = []
if mode == "sha" and "headSha" in d:
    d["headSha"] = "0" * 40; done.append("headSha")
if jobs is not None:
    if mode == "drop":
        d["jobs"] = [j for j in jobs if j.get("name") != req]
        if len(d["jobs"]) != len(jobs): done.append("dropped " + req)
    elif mode in ("skipped", "cancelled", "failure"):
        for j in jobs:
            if j.get("name") == req and j.get("conclusion") != mode: j["conclusion"] = mode; done.append(mode + " " + req)
if done:
    open(os.environ["ACCEPT_TAMPER_LOG"], "a").write(mode + ": " + ", ".join(done) + "\n")
json.dump(d, sys.stdout)'
GH
  chmod +x "$WORK/tamper-gh"
}
# A scratch worktree at the train head with one breaking commit; prints its path, or nothing.
neg_tree() { # name snippet
  local d="$WORK/neg-$1"
  git -C "$C" worktree add -q --detach "$d" "$HEAD" >/dev/null 2>&1 || return 1
  ( cd "$d" && bash -c "$2" && git add -A && git -c user.name=acceptance -c user.email=acceptance@invalid commit -qm "acceptance: broken on purpose ($1)" ) >/dev/null 2>&1 || return 1
  echo "$d"
}
do_neg_build() {
  if [ -n "${SKIP[build]:-}" ]; then row neg:build skipped "the build leg is skipped"; SKIPPED=1; return 0; fi
  local snip="${ACCEPT_BREAK_BUILD:-f=ConcreteSpec.lean; [ -f \$f ] || f=Main.lean; printf '\ntheorem acceptance_broken : False := rfl\n' >> \$f; echo \$f > .acceptance-broken-file}"
  local d out f
  d="$(neg_tree build "$snip")" || { row neg:build failed "could not make the broken commit"; FAILED=1; return 0; }
  f="$(git -C "$d" show HEAD:.acceptance-broken-file 2>/dev/null)"
  out="$(cd "$d" && bash -c "${ACCEPT_BUILD_CMD:-make build}" 2>&1)"
  if [ $? = 0 ]; then row neg:build failed "a broken build passed"; FAILED=1
  elif [ -n "$f" ] && ! printf '%s\n' "$out" | awk -v w="$f" 'index($0, w) {x=1} END {exit !x}'; then
    row neg:build failed "the build failed, but not on the broken file $f"; FAILED=1
  else row neg:build passed "build refused the broken commit${f:+ ($f)}"; fi
  git -C "$C" worktree remove --force "$d" >/dev/null 2>&1 || true
}
do_neg_hook() {
  local d out base
  d="$(neg_tree hook "printf 'Acceptance: no such job\n' >> required-checks.txt")" || { row neg:hook failed "could not make the broken commit"; FAILED=1; return 0; }
  [ -f "$d/.githooks/pre-push" ] || { row neg:hook failed "the candidate has no .githooks/pre-push"; FAILED=1; return 0; }
  base="$(git -C "$C" rev-parse "$HEAD")"
  out="$(cd "$d" && printf 'refs/heads/acceptance-neg %s refs/heads/ci/acceptance-neg %s\n' "$(git rev-parse HEAD)" "$base" \
         | bash .githooks/pre-push origin "$ORIGIN_URL" 2>&1)"
  if [ $? = 0 ]; then row neg:hook failed "the hook passed a required-checks.txt naming a job no workflow has"; FAILED=1
  elif ! printf '%s\n' "$out" | awk 'index($0, "landing_script") {x=1} END {exit !x}'; then
    row neg:hook failed "the hook failed, but not in the landing-script check: $(printf '%s\n' "$out" | tail -n 1 | cut -c1-80)"; FAILED=1
  else row neg:hook passed "the hook refused (landing-script check)"; fi
  git -C "$C" worktree remove --force "$d" >/dev/null 2>&1 || true
}
do_negative() {
  if [ -n "${SKIP[negative]:-}" ]; then row negative skipped "--skip"; SKIPPED=1; return 0; fi
  local rid rsha
  rid="$(cd "$C" && sed -n 's/^run_id=//p' "$(git rev-parse --git-common-dir)/concrete-landing/$NAME/train.env" | tail -1)"
  rsha="$(gh run view "$rid" -R "${ACCEPT_REPO:-unbalancedparentheses/concrete2}" --json headSha 2>/dev/null | python3 -I -c 'import json,sys; print(json.load(sys.stdin).get("headSha",""))' 2>/dev/null)"
  if [ "$rsha" != "$HEAD" ]; then row neg:run failed "run ${rid:-?} is at ${rsha:-?}, not the train head $HEAD: the controls would test another SHA"; FAILED=1; return 0; fi
  row neg:run passed "run $rid is at the train head ${HEAD:0:10}"
  make_tamper_gh
  local req; req="$(sed 's/#.*//; s/^[[:space:]]*//; s/[[:space:]]*$//' "$C/required-checks.txt" 2>/dev/null | sed '/^$/d' | head -1)"
  if [ -z "$req" ]; then row negative failed "no required-checks.txt in the candidate"; FAILED=1; return 0; fi
  local mode cmd out rc want tlog
  for mode in drop skipped cancelled failure sha; do
    for cmd in watch land; do
      tlog="$WORK/tamper-$mode-$cmd.log"; : > "$tlog"
      case "$mode:$cmd" in
        drop:*) want="missing: $req" ;;
        skipped:*) want="skipped: $req" ;;
        cancelled:*) want="cancelled: $req" ;;
        failure:*) want="failure: $req" ;;
        sha:watch) want="says nothing about this train" ;;
        sha:land) want="not the train HEAD" ;;
      esac
      if [ "$cmd" = watch ]; then
        out="$(cd "$C" && ACCEPT_TAMPER=$mode ACCEPT_REQ="$req" ACCEPT_TAMPER_LOG="$tlog" LAND_GH="$WORK/tamper-gh" bash scripts/land/train.sh watch "$NAME" --interval 1 --timeout 1 2>&1)"; rc=$?
      else
        out="$(cd "$C" && ACCEPT_TAMPER=$mode ACCEPT_REQ="$req" ACCEPT_TAMPER_LOG="$tlog" LAND_GH="$WORK/tamper-gh" bash scripts/land/train.sh --dry-run land "$NAME" 2>&1)"; rc=$?
      fi
      if [ "$rc" = 0 ]; then
        row "neg:$mode" failed "$cmd ACCEPTED a tampered run ($req)"; FAILED=1
      elif [ ! -s "$tlog" ]; then
        row "neg:$mode" failed "$cmd refused, but the wrapper changed nothing: no evidence of the mutation"; FAILED=1
      elif ! printf '%s\n' "$out" | awk -v w="$want" 'index($0, w) {f=1} END {exit !f}'; then
        row "neg:$mode" failed "$cmd refused for another reason (expected '$want'): $(printf '%s\n' "$out" | tail -n 1 | cut -c1-80)"; FAILED=1
      else
        row "neg:$mode" passed "$cmd refused: $want"
      fi
    done
  done
}

make_git_shim
export PATH="$WORK/bin:$PATH"
echo "acceptance: $REF from $ORIGIN_URL"
leg clone do_clone
NAME="${NAME:-accept-$(git -C "$C" rev-parse --short=10 "$REF")}"
leg train do_train
leg build do_build
leg check do_check
leg ci do_ci
leg watch do_watch
leg dryland do_dryland
do_neg_build
do_neg_hook
do_negative
finish
