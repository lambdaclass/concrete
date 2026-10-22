#!/usr/bin/env bash
# Landing bot (R-0539): lands ready branches without a human in the critical path.
# docs/project/LANDING.md, "The landing bot", is the manual; this header is the contract.
#
#   bot.sh [--dry-run] [tick]   one pass: resume, auto-rebase, collect, train, land (the timer's job)
#   bot.sh [--dry-run] rebase   the auto-rebase pass only
#   bot.sh list                 the ready branches in landing order, with each one's order key
#   bot.sh status               bot state and the last events
#   bot.sh reset                clear a `blocked` state after a human has looked at it
#
# READY: a branch on the primary remote named <BOT_BRANCH_PREFIX>* (default wip/, never wip/land-*)
# whose head commit is not in main, carries `verdict: clean` in refs/notes/concrete-review AND
# `gates: pass` in refs/notes/concrete-gates (scripts/land/notes.sh), and was not ejected at that SHA.
#
# ORDER: every explicit `order: N` (either note) first, by N; then language-rule branches (they touch
# Concrete/Check/ or Concrete/Elab/ AND add an E-code main does not have); then everything
# else. Ties: smaller diff first, then name.
#
# SAFETY: one instance (a kernel flock, via flock(1) or Python fcntl); state in a file, so a restart resumes a
# train that is in CI instead of dispatching again; `touch ~/.concrete-bot-paused` stops it before
# the next stage; --dry-run changes nothing; main is pushed only by train.sh land (fast-forward,
# never forced) to the primary and the mirror; a remote named `fork` is refused; every git
# operation is followed by an in-progress check, and `git rebase` is never piped.
#
# ENV: BOT_STATE_DIR (default <git-common-dir>/concrete-landing/bot), BOT_PAUSE_FILE
# (~/.concrete-bot-paused), BOT_BRANCH_PREFIX (wip/), BOT_ACTIVE_SECONDS (600: younger heads are
# not rebased), BOT_MAX_TRAINS (4 per tick), BOT_WATCH_INTERVAL (60), BOT_CHECK_ARGS (extra
# `train.sh check` arguments; the bot always passes --no-review, members were reviewed at their
# heads, unless BOT_TRAIN_REVIEW=1), BOT_NO_REBASE=1, BOT_NOTIFY (default scripts/land/notify.sh,
# called as `notify.sh <event> <message>` when present), BOT_NOW (test seam: epoch seconds).
# LAND_* variables pass through to train.sh.
set -euo pipefail
SELF_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=scripts/land/lib.sh
. "$SELF_DIR/lib.sh"
DRY=0
if [ "${1:-}" = "--dry-run" ]; then DRY=1; shift; fi
SUB="${1:-tick}"

die() { echo "bot: $*" >&2; exit 1; }
say() { echo "bot: $*"; }
ROOT="$(git rev-parse --show-toplevel 2>/dev/null)" || die "not inside a git work tree"
cd "$ROOT"
COMMON="$(cd "$(git rev-parse --git-common-dir)" && pwd)"
LAND_ROOT="${LAND_STATE_DIR:-$COMMON/concrete-landing}"
LAND_LOGS="${LAND_LOG_DIR:-$LAND_ROOT/log}"
STATE_DIR="${BOT_STATE_DIR:-$LAND_ROOT/bot}"
STATE="$STATE_DIR/bot.env"
EVENTS="$STATE_DIR/events.tsv"
TRAIN="${BOT_TRAIN:-$SELF_DIR/train.sh}"
NOTES="$SELF_DIR/notes.sh"
IMPACT="$SELF_DIR/impact_gates.sh"
NOTIFY="${BOT_NOTIFY:-$SELF_DIR/notify.sh}"
REMOTE="${LAND_REMOTE:-origin}"
MIRROR="${LAND_MIRROR-lambdaclass}"
PREFIX="${BOT_BRANCH_PREFIX:-wip/}"; PREFIX="${PREFIX%/}/"
PAUSE="${BOT_PAUSE_FILE:-$HOME/.concrete-bot-paused}"
ACTIVE="${BOT_ACTIVE_SECONDS:-600}"
MAX_TRAINS="${BOT_MAX_TRAINS:-4}"
WATCH_INTERVAL="${BOT_WATCH_INTERVAL:-60}"
CHECK_ARGS=()
[ "${BOT_TRAIN_REVIEW:-0}" = 1 ] || CHECK_ARGS+=(--no-review)
# shellcheck disable=SC2206
[ -z "${BOT_CHECK_ARGS:-}" ] || CHECK_ARGS+=(${BOT_CHECK_ARGS})
now() { echo "${BOT_NOW:-$(date +%s)}"; }
TRAIN_SEQ=0

[ "$REMOTE" != fork ] || die "refusing the remote named 'fork' as primary"
[ "$MIRROR" != fork ] || die "refusing the remote named 'fork' as mirror"

# --- state, events, notification -----------------------------------------------------------------

st_get() { [ -f "$STATE" ] || return 0; sed -n "s/^$1=//p" "$STATE" | tail -n 1; }
st_set() {
  [ "$DRY" = 1 ] && return 0
  mkdir -p "$STATE_DIR"; touch "$STATE"
  grep -v "^$1=" "$STATE" > "$STATE.tmp" || true
  printf '%s=%s\n' "$1" "$2" >> "$STATE.tmp"
  mv "$STATE.tmp" "$STATE"
}

event() { # kind message — logged, printed, and handed to notify.sh when it exists
  local kind="$1" msg="$2"
  say "[$kind] $msg"
  [ "$DRY" = 1 ] && return 0
  mkdir -p "$STATE_DIR"
  printf '%s\t%s\t%s\n' "$(date -u +%Y-%m-%dT%H:%M:%SZ)" "$kind" "$(printf '%s' "$msg" | tr '\t\n' '  ')" >> "$EVENTS"
  if [ -f "$NOTIFY" ]; then
    if command -v timeout >/dev/null 2>&1; then timeout 60 bash "$NOTIFY" "$kind" "$msg" || true
    else bash "$NOTIFY" "$kind" "$msg" || true; fi
  fi
}

paused() { [ -e "$PAUSE" ]; }

# --- single instance -----------------------------------------------------------------------------
# A KERNEL lock on both platforms: this shell opens $LOCK.flock on fd 9 and has flock(2) taken on
# that open file description (by flock(1), or by Python's fcntl.flock where flock(1) is missing, as
# on macOS). The lock belongs to the description, so it lasts while this process (or a child that
# inherited fd 9) lives and is released by the kernel on any exit: nothing stale is ever left.
# A lock DIRECTORY or symlink at $LOCK is the earlier format: refused while its pid lives, removed
# once it is dead.
LOCK="$STATE_DIR/lock"
alive() { [ -n "$1" ] && kill -0 "$1" 2>/dev/null; }
acquire_lock() {
  mkdir -p "$STATE_DIR"
  if [ -e "$LOCK" ] || [ -L "$LOCK" ]; then
    local old; old="$(cat "$LOCK/pid" 2>/dev/null || readlink "$LOCK" 2>/dev/null || true)"
    if alive "$old"; then say "an instance of the earlier lock format (pid $old) holds $LOCK; nothing to do"; exit 0; fi
    rm -rf "$LOCK"
  fi
  exec 9>"$LOCK.flock"
  local got=0
  if command -v flock >/dev/null 2>&1; then flock -n 9 && got=1
  else python3 -I -c 'import fcntl, sys
try: fcntl.flock(9, fcntl.LOCK_EX | fcntl.LOCK_NB)
except OSError: sys.exit(1)' && got=1; fi
  [ "$got" = 1 ] || { say "another instance holds $LOCK.flock; nothing to do"; exit 0; }
  echo "$$" > "$LOCK.pid"
}

# --- remote view ---------------------------------------------------------------------------------

sync_remote() {
  git fetch -q --prune "$REMOTE"
  land_assert_no_op fetch
  bash "$NOTES" fetch
}

# "branch sha" per ready branch.
ready_branches() {
  local main b sha
  main="$(git rev-parse "$REMOTE/main")"
  git for-each-ref --format='%(refname:lstrip=3) %(objectname)' "refs/remotes/$REMOTE/${PREFIX%/}" \
  | while read -r b sha; do
      case "$b" in "${PREFIX}land-"*|"$PREFIX") continue ;; esac
      if git merge-base --is-ancestor "$sha" "$main"; then continue; fi
      case " $(st_get ejected) " in *" $b@$sha "*) continue ;; esac
      bash "$NOTES" ready "$sha" || continue
      echo "$b $sha"
    done
}

# "category<TAB>rank<TAB>reason" for one ready head. Categories sort first, so every explicit order
# precedes every rule change, whatever its value: 0 explicit (rank = N), 1 language rule, 2 ordinary.
order_key() { # branch sha
  local sha="$2" o mb files codes c new=""
  o="$(bash "$NOTES" get review "$sha" order)"; [ -n "$o" ] || o="$(bash "$NOTES" get gates "$sha" order)"
  # Decimal, at most 9 digits; 10# keeps a leading zero ("08") from reading as octal.
  case "$o" in ''|*[!0-9]*) ;; *)
    if [ "${#o}" -le 9 ]; then printf '0\t%s\texplicit order: %s\n' "$((10#$o))" "$o"; return 0; fi ;;
  esac
  mb="$(git merge-base "$REMOTE/main" "$sha")"
  files="$(git diff --name-only "$mb" "$sha" -- Concrete/Check Concrete/Elab | tr '\n' ' ')"
  if [ -n "$files" ]; then
    codes="$(git diff -U0 "$mb" "$sha" -- Concrete | awk '/^\+\+\+/{next} /^\+/{ s=$0; while (match(s, /E[0-9][0-9][0-9][0-9]/)) { print substr(s, RSTART, RLENGTH); s=substr(s, RSTART+RLENGTH) } }' | sort -u)"
    for c in $codes; do
      if ! git grep -q -w "$c" "$REMOTE/main" -- Concrete; then new="$new $c"; fi
    done
    if [ -n "$new" ]; then printf '1\t0\tlanguage rule: new%s in %s\n' "$new" "${files% }"; return 0; fi
  fi
  printf '2\t0\tordinary\n'
}

# "key size branch sha reason", sorted: the landing order. key = <category>.<rank, 9 digits>.
ordered_ready() {
  local b sha k cat rank reason size
  ready_branches | while read -r b sha; do
    k="$(order_key "$b" "$sha")"
    IFS=$'\t' read -r cat rank reason <<< "$k"
    size="$(git diff --numstat "$(git merge-base "$REMOTE/main" "$sha")" "$sha" | awk '{n+=$1+$2} END{print n+0}')"
    printf '%s.%09d %08d %s %s %s\n' "$cat" "$rank" "$size" "$b" "$sha" "$reason"
  done | sort -k1,1 -k2,2 -k3,3
}

# --- auto-rebase ---------------------------------------------------------------------------------

to_epoch() { python3 -I -c 'import sys, datetime
try: print(int(datetime.datetime.strptime(sys.argv[1], "%Y-%m-%dT%H:%M:%SZ").replace(tzinfo=datetime.timezone.utc).timestamp()))
except Exception: print(0)' "$1"; }

# The latest sign of activity on a branch: its head's commit time, the time this clone saw the head
# arrive (the remote-tracking reflog), and the notes' utc. An agent pushing, reviewing or gating
# moves one of them.
last_activity() { # branch sha
  local t=0 x u
  x="$(git log -1 --format=%ct "$2")"; [ "$x" -gt "$t" ] && t="$x"
  x="$(git reflog show --date=unix -n 1 --format='%gd' "refs/remotes/$REMOTE/$1" 2>/dev/null | sed -n 's/.*@{\([0-9]*\)}.*/\1/p')"
  [ -n "$x" ] && [ "$x" -gt "$t" ] && t="$x"
  for u in "$(bash "$NOTES" get review "$2" utc)" "$(bash "$NOTES" get gates "$2" utc)"; do
    [ -n "$u" ] || continue
    x="$(to_epoch "$u")"; [ "$x" -gt "$t" ] && t="$x"
  done
  echo "$t"
}

rebase_in_scratch() { # branch sha main log — runs inside the scratch worktree
  local b="$1" sha="$2" main="$3" log="$4" rc=0 files new sel
  git rebase "$main" > "$log" 2>&1 || rc=$?   # never piped: a piped rebase hides its stop
  local stopped=0 p
  for p in rebase-merge rebase-apply MERGE_HEAD CHERRY_PICK_HEAD; do [ -e "$(git rev-parse --git-path "$p")" ] && stopped=1; done
  if [ "$rc" != 0 ] || [ "$stopped" = 1 ]; then
    files="$(git diff --name-only --diff-filter=U | tr '\n' ' ')"
    git rebase --abort >> "$log" 2>&1 || true
    land_assert_no_op "rebase --abort"
    event rebase-conflict "$b (${sha:0:10}) does not rebase onto main ${main:0:10}; conflicts in: ${files:-(rebase exit $rc, see $log)}"
    return 1
  fi
  land_assert_no_op rebase
  if [ -f scripts/gen/refresh_generated.sh ]; then
    land_in_nix bash scripts/gen/refresh_generated.sh >> "$log" 2>&1 \
      || { event rebase-failed "$b: refresh_generated.sh refused after rebasing (log $log)"; return 1; }
    if [ -n "$(git status --porcelain --untracked-files=no)" ]; then
      git add -u; git commit -q -m "regenerate generated views after rebasing onto main ${main:0:10}"
      land_assert_no_op "regeneration commit"
    fi
  fi
  new="$(git rev-parse HEAD)"
  bash "$IMPACT" --base "$main" --head "$new" --src-ref "refs/heads/$b" --dst-ref "refs/heads/ci/rebase-check" \
    --report "$log.selected" >> "$log" 2>&1 \
    || { event rebase-failed "$b: impact gates failed on the rebased head ${new:0:10}; branch left as is (log $log)"; return 1; }
  if paused; then event paused "$b: rebased and gated as ${new:0:10}, NOT pushed (kill switch)"; return 2; fi
  # The lease: if the agent pushed in the meantime, the push is refused and the branch is left alone.
  git push -q --force-with-lease="refs/heads/$b:$sha" "$REMOTE" "$new:refs/heads/$b" >> "$log" 2>&1 \
    || { event rebase-skipped "$b moved while it was being rebased; left alone"; return 1; }
  land_assert_no_op push
  sel="$(sed -n 's/^selected: //p' "$log.selected" 2>/dev/null || true)"
  bash "$NOTES" put review "$new" verdict=clean rounds="$(bash "$NOTES" get review "$sha" rounds)" \
    reviewer="$(bash "$NOTES" get review "$sha" reviewer)" carried-from="$sha"
  bash "$NOTES" put gates "$new" gates=pass selected="${sel:-unknown}" runner=landing-bot rebased-onto="$main"
  event rebased "$b ${sha:0:10} -> ${new:0:10} onto main ${main:0:10}"
}

rebase_pass() {
  local main b sha age key slug wt rrc
  main="$(git rev-parse "$REMOTE/main")"
  while read -r b sha; do
    [ -n "$b" ] || continue
    if paused; then say "rebase: paused; stopping the pass"; break; fi
    case "$b" in "$PREFIX"*) ;; *) say "rebase: $b is outside $PREFIX; never rewritten"; continue ;; esac
    if git merge-base --is-ancestor "$main" "$sha"; then continue; fi
    slug="$(printf '%s' "$b" | tr -c 'A-Za-z0-9._-' '_')"
    key="$sha@$main"
    [ "$(st_get "rebased_$slug")" = "$key" ] && continue
    age=$(( $(now) - $(last_activity "$b" "$sha") ))
    if [ "$age" -lt "$ACTIVE" ]; then say "rebase: skip $b: last activity ${age}s ago (< ${ACTIVE}s; an agent may be pushing)"; continue; fi
    if [ "$DRY" = 1 ]; then say "(dry-run) would rebase $b (${sha:0:10}) onto main ${main:0:10}"; continue; fi
    wt="$STATE_DIR/scratch/$slug"
    mkdir -p "$STATE_DIR/scratch"
    git worktree remove --force "$wt" >/dev/null 2>&1 || rm -rf "$wt"
    git worktree prune
    git worktree add -q --detach "$wt" "$sha"
    if [ -d "$ROOT/.lake" ] && [ ! -e "$wt/.lake" ]; then cp -a "$ROOT/.lake" "$wt/.lake"; fi
    rrc=0; ( cd "$wt" && rebase_in_scratch "$b" "$sha" "$main" "$wt.log" ) || rrc=$?
    git worktree remove --force "$wt" >/dev/null 2>&1 || rm -rf "$wt"
    git worktree prune
    land_assert_no_op "scratch cleanup"
    # A pass stopped by the kill switch (exit 2) is not a try: the pair is retried after a resume.
    [ "$rrc" = 2 ] || st_set "rebased_$slug" "$key"
  done <<< "$(ready_branches)"
}

# --- trains --------------------------------------------------------------------------------------

train_get() { sed -n "s/^$2=//p" "$LAND_ROOT/$1/train.env" 2>/dev/null | tail -n 1; }
train_set() { # train key value — the same file and format as train.sh's state_set
  local f="$LAND_ROOT/$1/train.env"
  grep -v "^$2=" "$f" > "$f.tmp" || true
  printf '%s=%s\n' "$2" "$3" >> "$f.tmp"
  mv "$f.tmp" "$f"
}
last_result() { awk -F'\t' -v s="$2" '$3==s{r=$8} END{print r}' "$LAND_LOGS/$1.tsv" 2>/dev/null; }

eject() { # branch sha reason
  st_set ejected "$(st_get ejected) $1@$2"
  bash "$NOTES" put gates "$2" gates=fail reason="ejected by the landing bot: $3" runner=landing-bot || true
  event ejected "$1 (${2:0:10}): $3"
}

# Exit 10: retry with the rest. 20: stop this tick, nothing lost. 30: blocked, a human must look.
# Bisection checks the kill switch between its steps (LAND_PAUSE_FILE), so a pause stops it too.
bisect_and_eject() { # train mode(--ci|test command) why
  local name="$1" mode="$2" why="$3" members n out culprit ref sha verdict
  members="$(train_get "$name" members)"
  n="$(printf '%s\n' "${members//,/$'\n'}" | grep -c . || true)"
  if [ "$n" -le 1 ]; then
    ref="${members%@*}"; sha="${members##*@}"; verdict=red
  else
    if paused; then event paused "train $name is red ($why); bisection waits for the kill switch to clear"; return 20; fi
    event bisecting "train $name is red ($why); bisecting $n members"
    if [ "$mode" = --ci ]; then out="$(LAND_PAUSE_FILE="$PAUSE" bash "$TRAIN" bisect "$name" --ci 2>&1)" || true
    else out="$(LAND_PAUSE_FILE="$PAUSE" bash "$TRAIN" bisect "$name" --test "$mode" 2>&1)" || true; fi
    printf '%s\n' "$out" > "$STATE_DIR/bisect-$name.log"
    culprit="$(printf '%s\n' "$out" | sed -n 's/^train: culprit: \([^ ]*\) (\([0-9a-f]*\)) — alone it is \(.*\)$/\1 \2 \3/p' | tail -n 1)"
    if [ -z "$culprit" ] && paused; then event paused "bisection of $name stopped by the kill switch"; return 20; fi
    [ -n "$culprit" ] || { st_set phase blocked; event blocked "bisect of $name gave no culprit (log $STATE_DIR/bisect-$name.log)"; return 30; }
    read -r ref sha verdict <<< "$culprit"
  fi
  local b="${ref#refs/bot/}"
  if [ "$verdict" = green ]; then eject "$b" "$sha" "train $name red ($why); green alone, so an interaction with another member"
  else eject "$b" "$sha" "train $name red ($why); red in bisection"; fi
  return 10
}

# A step that ended a train leaves the phase idle, except a block, which waits for `bot.sh reset`.
settle() { [ "$1" = 30 ] || st_set phase idle; return "$1"; }

watch_and_land() { # train
  local name="$1" rc=0 res
  bash "$TRAIN" watch "$name" --interval "$WATCH_INTERVAL" > "$STATE_DIR/watch-$name.log" 2>&1 || rc=$?
  if [ "$rc" != 0 ]; then
    res="$(last_result "$name" watch)"
    if [ "$res" = failed ]; then
      # A paused bisection keeps the train in CI: the next tick re-reads the red run and resumes.
      rc=0; bisect_and_eject "$name" --ci "CI landing tier failed" || rc=$?
      [ "$rc" = 20 ] && return 20
      settle "$rc"; return "$rc"
    fi
    event watch-incomplete "train $name: no verdict yet (${res:-unknown}); will look again next tick"
    return 20
  fi
  if paused; then event paused "train $name is green; landing waits for the kill switch to clear"; return 20; fi
  git checkout -q "train/$name"; land_assert_no_op checkout
  rc=0; bash "$TRAIN" land "$name" > "$STATE_DIR/land-$name.log" 2>&1 || rc=$?
  cat "$STATE_DIR/land-$name.log"
  case "$rc" in
    0) st_set phase idle
       event landed "train $name at $(train_get "$name" head | cut -c1-10): $(train_get "$name" members | sed 's,refs/bot/,,g; s/@\([0-9a-f]\{10\}\)[0-9a-f]*/@\1/g')"
       return 0 ;;
    4) st_set phase idle; event main-moved "train $name: main moved before landing; rebuilding"; return 10 ;;
    *) st_set phase blocked; event blocked "train $name: land failed (exit $rc; log $STATE_DIR/land-$name.log)"; return 30 ;;
  esac
}

# After a crash or a failure between dispatch and recording the run id, find the run for this exact
# train head instead of dispatching a second one. Prints the run id (or nothing: GitHub answered and
# has no such run). Exit 2 when the lookup itself failed (network, auth, bad JSON): that is no answer.
reconcile_run() { # train
  local name="$1" id head out
  id="$(train_get "$name" run_id)"
  if [ -n "$id" ]; then echo "$id"; return 0; fi
  head="$(train_get "$name" head)"
  [ -n "$head" ] || return 0
  out="$("${LAND_GH:-gh}" run list -R "${LAND_REPO:-unbalancedparentheses/concrete2}" --workflow CI --branch "ci/$name" \
      --event workflow_dispatch -L 20 --json databaseId,headSha 2>/dev/null)" || return 2
  printf '%s' "$out" | python3 -I -c '
import json, sys
try: runs = json.load(sys.stdin)
except Exception: sys.exit(2)
if not isinstance(runs, list): sys.exit(2)
m = [r for r in runs if r.get("headSha") == sys.argv[1]]
print(m[0]["databaseId"] if m else "")' "$head" || return 2
}

run_train() { # "branch sha"... (landing order)
  TRAIN_SEQ=$((TRAIN_SEQ+1))
  local name; name="bot-$(date -u +%Y%m%d-%H%M%S)-$TRAIN_SEQ"
  local refs=() b sha line rc out desc=""
  for line in "$@"; do
    read -r b sha <<< "$line"
    git update-ref "refs/bot/$b" "$sha"
    refs+=("refs/bot/$b"); desc="$desc $b@${sha:0:10}"
  done
  st_set train "$name"; st_set phase build
  event train "building $name from$desc"
  rc=0; out="$(bash "$TRAIN" build "$name" --force "${refs[@]}" 2>&1)" || rc=$?
  printf '%s\n' "$out" > "$STATE_DIR/build-$name.log"
  if [ "$rc" = 3 ]; then
    local who; who="$(printf '%s\n' "$out" | sed -n 's/^train: STOPPED — \([^ ]*\) conflicts.*/\1/p' | head -n 1)"
    sha="$(git rev-parse "$who" 2>/dev/null || true)"
    [ -n "$who" ] && [ -n "$sha" ] || { st_set phase blocked; event blocked "build of $name stopped without naming a member"; return 30; }
    eject "${who#refs/bot/}" "$sha" "conflicts with the members ordered before it: $(printf '%s\n' "$out" | sed -n 's/^    //p' | tr '\n' ' ')"
    st_set phase idle; return 10
  elif [ "$rc" != 0 ]; then
    [ -n "$(train_get "$name" members)" ] || { st_set phase blocked; event blocked "build of $name failed before recording members (log $STATE_DIR/build-$name.log)"; return 30; }
    rc=0; bisect_and_eject "$name" true "build failed" || rc=$?
    settle "$rc"; return "$rc"
  fi
  if paused; then st_set phase idle; event paused "train $name built; not checked (kill switch)"; return 20; fi
  st_set phase check
  rc=0; bash "$TRAIN" check "$name" "${CHECK_ARGS[@]}" > "$STATE_DIR/check-$name.log" 2>&1 || rc=$?
  if [ "$rc" != 0 ]; then
    local t
    t="bash $(printf '%q' "$TRAIN") check \"\$(git symbolic-ref --short HEAD | sed 's,^train/,,')\" ${CHECK_ARGS[*]}"
    rc=0; bisect_and_eject "$name" "$t" "check failed" || rc=$?
    settle "$rc"; return "$rc"
  fi
  if paused; then st_set phase idle; event paused "train $name checked; CI not dispatched (kill switch)"; return 20; fi
  # `dispatching` is persisted BEFORE the dispatch: a restart (or a failure to find the run) then
  # reconciles with GitHub instead of dispatching the same candidate twice.
  st_set dispatch_epoch "$(now)"; st_set phase dispatching
  rc=0; bash "$TRAIN" ci "$name" > "$STATE_DIR/ci-$name.log" 2>&1 || rc=$?
  if [ "$rc" != 0 ]; then event ci-failed "train $name: push, dispatch or run lookup failed; next tick reconciles (log $STATE_DIR/ci-$name.log)"; return 20; fi
  st_set phase ci
  watch_and_land "$name"
}

# --- subcommands ---------------------------------------------------------------------------------

cmd_tick() {
  if paused; then say "paused ($PAUSE exists); doing nothing"; return 0; fi
  [ "$DRY" = 1 ] || acquire_lock
  land_assert_no_op start
  [ -z "$(git status --porcelain --untracked-files=no)" ] || die "the bot's clone has uncommitted changes; it must be a dedicated clone"
  sync_remote
  local phase train rc
  phase="$(st_get phase)"; train="$(st_get train)"
  case "$phase" in
    blocked) event blocked-still "train ${train:-?} needs a human (bot.sh status; then bot.sh reset)"; return 0 ;;
    ci)
      if [ "$DRY" = 1 ]; then say "(dry-run) would resume watching train $train"; else
        event resume "train $train is in CI; watching it instead of dispatching again"
        rc=0; watch_and_land "$train" || rc=$?
        case "$rc" in 0|10) ;; *) return 0 ;; esac
      fi ;;
    dispatching)
      local id="" lrc=0 since
      [ "$DRY" = 1 ] || id="$(reconcile_run "$train")" || lrc=$?
      since="$(st_get dispatch_epoch)"; since=$(( $(now) - ${since:-0} ))
      if [ "$DRY" = 1 ]; then say "(dry-run) would reconcile the dispatch of train $train"
      elif [ "$lrc" != 0 ]; then
        event dispatch-unknown "train $train: the CI run lookup failed; keeping it as dispatching and asking again next tick"; return 0
      elif [ -z "$id" ] && [ "$since" -lt "${BOT_DISPATCH_GRACE:-900}" ]; then
        event dispatch-unknown "train $train: no run visible yet ${since}s after its dispatch; asking again next tick"; return 0
      elif [ -n "$id" ]; then
        train_set "$train" run_id "$id"; st_set phase ci
        event resume "train $train was dispatched as run $id before the restart; watching it"
        rc=0; watch_and_land "$train" || rc=$?
        case "$rc" in 0|10) ;; *) return 0 ;; esac
      else
        event resume "train $train has no CI run at its head ${since}s after its dispatch; discarding it and starting over"; st_set phase idle
      fi ;;
    build|check) event resume "train $train was interrupted at $phase; discarding it and starting over"; st_set phase idle ;;
  esac
  if [ "${BOT_NO_REBASE:-0}" != 1 ]; then rebase_pass; sync_remote; fi
  local n=0 ready lines
  while [ "$n" -lt "$MAX_TRAINS" ]; do
    if paused; then say "paused; stopping before the next train"; break; fi
    ready="$(ordered_ready)"
    [ -n "$ready" ] || { say "no ready branches"; break; }
    lines=()
    while read -r _k _s b sha _r; do lines+=("$b $sha"); done <<< "$ready"
    if [ "$DRY" = 1 ]; then
      say "(dry-run) would build one train, in this order:"; printf '%s\n' "$ready" | sed 's/^/    /'
      bash "$TRAIN" --dry-run build bot-dry-run $(printf '%s\n' "$ready" | awk '{print $4}') || true
      break
    fi
    n=$((n+1))
    rc=0; run_train "${lines[@]}" || rc=$?
    case "$rc" in 0|10) ;; *) break ;; esac
  done
  if [ "$DRY" = 0 ]; then git checkout -q --detach "$REMOTE/main"; land_assert_no_op "final checkout"; fi
}

case "$SUB" in
  tick) cmd_tick ;;
  rebase)
    if paused; then say "paused"; exit 0; fi
    [ "$DRY" = 1 ] || acquire_lock
    sync_remote; rebase_pass ;;
  list) sync_remote; ordered_ready ;;
  status)
    echo "state ($STATE):"; sed 's/^/    /' "$STATE" 2>/dev/null || echo "    (none)"
    paused && echo "PAUSED: $PAUSE exists"
    echo "last events:"; tail -n 20 "$EVENTS" 2>/dev/null | sed 's/^/    /' || true ;;
  reset) st_set phase idle; event reset "blocked state cleared by hand" ;;
  *) sed -n '2,10p' "$0" | sed 's/^# \{0,1\}//'; exit 2 ;;
esac
