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
# ORDER: an explicit `order: N` in either note; else language-rule branches (they touch
# Concrete/Check/ or Concrete/Elab/ AND add an E-code main does not have) get 100; everything else
# 500. Ties: smaller diff first, then name.
#
# SAFETY: one instance (lock dir with a live-pid check); state in a file, so a restart resumes a
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
# A lock directory holding the owner's pid. A lock whose owner is dead is taken over, under a second
# directory so that two starters cannot both take it over.
LOCK="$STATE_DIR/lock"
release_lock() { rm -rf "$LOCK"; }
acquire_lock() {
  mkdir -p "$STATE_DIR"
  if ! mkdir "$LOCK" 2>/dev/null; then
    local pid; pid="$(cat "$LOCK/pid" 2>/dev/null || true)"
    if [ -z "$pid" ] || kill -0 "$pid" 2>/dev/null; then
      say "another instance (pid ${pid:-starting}) holds $LOCK; nothing to do"; exit 0
    fi
    mkdir "$LOCK.takeover" 2>/dev/null || { say "another instance is taking over $LOCK"; exit 0; }
    pid="$(cat "$LOCK/pid" 2>/dev/null || true)"
    if [ -n "$pid" ] && kill -0 "$pid" 2>/dev/null; then rmdir "$LOCK.takeover"; say "lock is live (pid $pid)"; exit 0; fi
    rm -rf "$LOCK"; mkdir "$LOCK"; rmdir "$LOCK.takeover"
    event stale-lock "took over the lock of dead pid ${pid:-?}"
  fi
  echo "$$" > "$LOCK/pid"
  trap release_lock EXIT
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

# "key<TAB>reason" for one ready head.
order_key() { # branch sha
  local sha="$2" o mb files codes c new=""
  o="$(bash "$NOTES" get review "$sha" order)"; [ -n "$o" ] || o="$(bash "$NOTES" get gates "$sha" order)"
  case "$o" in ''|*[!0-9]*) ;; *) printf '%s\texplicit order: %s\n' "$o" "$o"; return 0 ;; esac
  mb="$(git merge-base "$REMOTE/main" "$sha")"
  files="$(git diff --name-only "$mb" "$sha" -- Concrete/Check Concrete/Elab | tr '\n' ' ')"
  if [ -n "$files" ]; then
    codes="$(git diff -U0 "$mb" "$sha" -- Concrete | awk '/^\+\+\+/{next} /^\+/{ s=$0; while (match(s, /E[0-9][0-9][0-9][0-9]/)) { print substr(s, RSTART, RLENGTH); s=substr(s, RSTART+RLENGTH) } }' | sort -u)"
    for c in $codes; do
      if ! git grep -q -w "$c" "$REMOTE/main" -- Concrete; then new="$new $c"; fi
    done
    if [ -n "$new" ]; then printf '100\tlanguage rule: new%s in %s\n' "$new" "${files% }"; return 0; fi
  fi
  printf '500\tordinary\n'
}

# "key size branch sha reason", sorted: the landing order.
ordered_ready() {
  local b sha k size
  ready_branches | while read -r b sha; do
    k="$(order_key "$b" "$sha")"
    size="$(git diff --numstat "$(git merge-base "$REMOTE/main" "$sha")" "$sha" | awk '{n+=$1+$2} END{print n+0}')"
    printf '%05d %08d %s %s %s\n' "${k%%$'\t'*}" "$size" "$b" "$sha" "${k#*$'\t'}"
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
  local main b sha age key slug wt
  main="$(git rev-parse "$REMOTE/main")"
  while read -r b sha; do
    [ -n "$b" ] || continue
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
    ( cd "$wt" && rebase_in_scratch "$b" "$sha" "$main" "$wt.log" ) || true
    git worktree remove --force "$wt" >/dev/null 2>&1 || rm -rf "$wt"
    git worktree prune
    land_assert_no_op "scratch cleanup"
    st_set "rebased_$slug" "$key"
  done <<< "$(ready_branches)"
}

# --- trains --------------------------------------------------------------------------------------

train_get() { sed -n "s/^$2=//p" "$LAND_ROOT/$1/train.env" 2>/dev/null | tail -n 1; }
last_result() { awk -F'\t' -v s="$2" '$3==s{r=$8} END{print r}' "$LAND_LOGS/$1.tsv" 2>/dev/null; }

eject() { # branch sha reason
  st_set ejected "$(st_get ejected) $1@$2"
  bash "$NOTES" put gates "$2" gates=fail reason="ejected by the landing bot: $3" runner=landing-bot || true
  event ejected "$1 (${2:0:10}): $3"
}

# Exit 10: retry with the rest. 20: stop this tick, nothing lost. 30: blocked, a human must look.
bisect_and_eject() { # train mode(--ci|test command) why
  local name="$1" mode="$2" why="$3" members n out culprit ref sha verdict
  members="$(train_get "$name" members)"
  n="$(printf '%s\n' "${members//,/$'\n'}" | grep -c . || true)"
  if [ "$n" -le 1 ]; then
    ref="${members%@*}"; sha="${members##*@}"; verdict=red
  else
    event bisecting "train $name is red ($why); bisecting $n members"
    if [ "$mode" = --ci ]; then out="$(bash "$TRAIN" bisect "$name" --ci 2>&1)" || true
    else out="$(bash "$TRAIN" bisect "$name" --test "$mode" 2>&1)" || true; fi
    printf '%s\n' "$out" > "$STATE_DIR/bisect-$name.log"
    culprit="$(printf '%s\n' "$out" | sed -n 's/^train: culprit: \([^ ]*\) (\([0-9a-f]*\)) — alone it is \(.*\)$/\1 \2 \3/p' | tail -n 1)"
    [ -n "$culprit" ] || { event blocked "bisect of $name gave no culprit (log $STATE_DIR/bisect-$name.log)"; st_set phase blocked; return 30; }
    read -r ref sha verdict <<< "$culprit"
  fi
  local b="${ref#refs/bot/}"
  if [ "$verdict" = green ]; then eject "$b" "$sha" "train $name red ($why); green alone, so an interaction with another member"
  else eject "$b" "$sha" "train $name red ($why); red in bisection"; fi
  return 10
}

watch_and_land() { # train
  local name="$1" rc=0 res
  bash "$TRAIN" watch "$name" --interval "$WATCH_INTERVAL" > "$STATE_DIR/watch-$name.log" 2>&1 || rc=$?
  if [ "$rc" != 0 ]; then
    res="$(last_result "$name" watch)"
    if [ "$res" = failed ]; then
      bisect_and_eject "$name" --ci "CI landing tier failed"; return $?
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
    bisect_and_eject "$name" true "build failed"; rc=$?; st_set phase idle; return "$rc"
  fi
  st_set phase check
  rc=0; bash "$TRAIN" check "$name" "${CHECK_ARGS[@]}" > "$STATE_DIR/check-$name.log" 2>&1 || rc=$?
  if [ "$rc" != 0 ]; then
    local t
    t="bash $(printf '%q' "$TRAIN") check \"\$(git symbolic-ref --short HEAD | sed 's,^train/,,')\" ${CHECK_ARGS[*]}"
    bisect_and_eject "$name" "$t" "check failed"; rc=$?; st_set phase idle; return "$rc"
  fi
  if paused; then st_set phase idle; event paused "train $name checked; CI not dispatched (kill switch)"; return 20; fi
  rc=0; bash "$TRAIN" ci "$name" > "$STATE_DIR/ci-$name.log" 2>&1 || rc=$?
  if [ "$rc" != 0 ]; then st_set phase idle; event ci-failed "train $name: could not push or dispatch CI (log $STATE_DIR/ci-$name.log)"; return 20; fi
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
