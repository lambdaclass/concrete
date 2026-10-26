#!/usr/bin/env bash
# THE LANDING BOT LANDS ONLY READY HEADS, IN RULE-FIRST ORDER, AND STOPS WHEN TOLD (R-0539).
#
# scripts/land/bot.sh takes the orchestrator out of the landing loop: it collects ready branches,
# builds one train, checks it, runs CI, lands it on both remotes, bisects and ejects a red member,
# and rebases ready branches onto a moved main. Each leg below drives the real bot (and the real
# train.sh under it) against a THROWAWAY repository with bare-repo remotes, a fake `gh` whose CI is
# red exactly when the candidate contains a file named CIRED, and a fake pre-push hook that fails
# exactly when the checkout contains a file named BAD. Nothing here touches this repository.
#
#   ready      only a head with `verdict: clean` AND `gates: pass` notes is ready; a stale note, a
#              findings verdict, a failed gate, wip/land-* and branches outside wip/ are not
#   ordering   explicit `order:` first, then language-rule branches (Check/Elab + a new E-code),
#              then the rest; a Check/Elab edit with no new E-code is not a rule change
#   dry-run    prints the train and changes nothing
#   kill switch, lock   a pause file or a live lock holder stops the tick; a dead holder's lock is
#              taken over
#   train      the tick lands every ready branch in order on origin AND lambdaclass; fork untouched
#   bisect     a red check or a red CI ejects exactly the culprit, publishes `gates: fail` at its
#              SHA, notifies, and lands the rest
#   resume     a restart while a train is in CI watches it instead of dispatching again
#   rebase     heads with recent activity are skipped; a quiet head is rebased in a scratch worktree,
#              re-gated, pushed with refreshed notes; a conflict is notified with its files and
#              leaves the branch and the clone untouched; a tried pair is not retried
#   main       no script under scripts/land force-pushes main
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
BOT="$ROOT_DIR/scripts/land/bot.sh"
NOTES="$ROOT_DIR/scripts/land/notes.sh"
[ -f "$BOT" ] && [ -f "$NOTES" ] || { echo "FATAL: scripts/land/bot.sh or notes.sh missing" >&2; exit 2; }
TMP="$(mktemp -d)" || { echo "FATAL: could not create a sandbox directory" >&2; exit 2; }
[ -n "$TMP" ] && [ -d "$TMP" ] || { echo "FATAL: sandbox directory is not usable" >&2; exit 2; }
trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
has(){ case "$1" in *"$2"*) return 0 ;; esac; return 1; }

unset GIT_DIR GIT_WORK_TREE GIT_INDEX_FILE GIT_COMMON_DIR GIT_OBJECT_DIRECTORY
export GIT_CONFIG_GLOBAL="$TMP/gitconfig" GIT_CONFIG_NOSYSTEM=1
cat > "$GIT_CONFIG_GLOBAL" <<EOF
[user]
	name = bot gate
	email = gate@example.invalid
[init]
	defaultBranch = main
[core]
	hooksPath = $TMP/nohooks
EOF
mkdir -p "$TMP/nohooks" "$TMP/gh"
export LAND_DUP_POLL=1 LAND_DUP_POLL_SLEEP=0 LAND_PUSH_RETRY_SLEEP=0
export LAND_NO_NIX=1 LAND_STATE_DIR="$TMP/landstate" LAND_GH="$TMP/fake-gh" LAND_REPO=test/repo
export LAND_HOOK="$TMP/hook" LAND_REMOTE_RUNNER="$TMP/fake-runner" FAKE_GH_DIR="$TMP/gh" ORIGIN_GIT="$TMP/origin.git"
export BOT_PAUSE_FILE="$TMP/paused" BOT_NOTIFY="$TMP/notify.sh" BOT_WATCH_INTERVAL=1 BOT_NO_REBASE=1
unset BOT_NOW BOT_STATE_DIR BOT_CHECK_ARGS LAND_REVIEW_CMD

cat > "$TMP/fake-gh" <<'GH'
#!/usr/bin/env bash
# CI is red exactly when the dispatched commit contains a file named CIRED.
echo "$*" >> "$FAKE_GH_DIR/calls.log"
case "$1 $2" in
  "workflow run")
    ref=""; while [ $# -gt 0 ]; do [ "$1" = --ref ] && ref="$2"; shift; done
    n=$(( $(cat "$FAKE_GH_DIR/counter" 2>/dev/null || echo 0) + 1 )); echo "$n" > "$FAKE_GH_DIR/counter"
    git --git-dir="$ORIGIN_GIT" rev-parse "refs/heads/$ref" > "$FAKE_GH_DIR/run.$n"
    [ -f "$FAKE_GH_DIR/pause-on-dispatch" ] && touch "$BOT_PAUSE_FILE"
    exit 0 ;;
  "run list")
    [ -f "$FAKE_GH_DIR/list-fails" ] && { echo "fake gh: network down" >&2; exit 1; }
    [ -f "$FAKE_GH_DIR/list-empty" ] && { echo '[]'; exit 0; }
    n="$(cat "$FAKE_GH_DIR/counter")"
    printf '[{"databaseId": %s, "headSha": "%s", "createdAt": "9999-01-01T00:00:00Z"}]' "$n" "$(cat "$FAKE_GH_DIR/run.$n")" ;;
  "run view")
    sha="$(cat "$FAKE_GH_DIR/run.$3")"; c=success
    git --git-dir="$ORIGIN_GIT" cat-file -e "$sha:CIRED" 2>/dev/null && c=failure
    printf '{"status":"completed","conclusion":"%s","headSha":"%s","jobs":[{"name":"Build","conclusion":"%s","databaseId":%s}]}' "$c" "$sha" "$c" "$3" ;;
  "api "*) echo "fake log tail" ;;
  *) echo "fake gh: unexpected $*" >&2; exit 1 ;;
esac
GH
cat > "$TMP/hook" <<'EOF'
#!/usr/bin/env bash
cat > /dev/null
if [ -f "$FAKE_GH_DIR/pause-countdown" ]; then
  n=$(( $(cat "$FAKE_GH_DIR/pause-countdown") - 1 )); echo "$n" > "$FAKE_GH_DIR/pause-countdown"
  [ "$n" -le 0 ] && touch "$BOT_PAUSE_FILE"
fi
if [ -f "$FAKE_GH_DIR/pause-in-culprit-probe" ]; then
  case "$(git symbolic-ref -q --short HEAD)" in *-bc) touch "$BOT_PAUSE_FILE" ;; esac
fi
[ ! -e BAD ]
EOF
cat > "$TMP/notify.sh" <<'EOF'
#!/usr/bin/env bash
# The real notify.sh signature: <info|success|warn|error> <title> <body>; the bot's title is
# "landing bot: <event>". The log keeps <event>|<body> so the assertions read the event.
[ "$#" -eq 3 ] || { echo "notify.sh called with $# arguments" >> "$FAKE_GH_DIR/notify.log"; exit 2; }
case "$1" in info|success|warn|error) ;; *) echo "bad level $1" >> "$FAKE_GH_DIR/notify.log"; exit 2 ;; esac
ev="${2#landing bot: }"
printf '%s|%s\n' "$ev" "$3" >> "$FAKE_GH_DIR/notify.log"
[ "$ev" = bisecting ] && [ -f "$FAKE_GH_DIR/pause-on-bisect" ] && touch "$BOT_PAUSE_FILE"
exit 0
EOF
cat > "$TMP/train-nobisect.sh" <<EOF
#!/usr/bin/env bash
[ "\$1" = bisect ] && { echo "bisect crashed before naming anyone"; exit 1; }
exec bash "$ROOT_DIR/scripts/land/train.sh" "\$@"
EOF
cat > "$TMP/fake-runner" <<'EOF'
#!/usr/bin/env bash
# A remote runner: fetches like concrete-remote (a ref on origin, or a SHA) and reports the SHA.
echo "$1" >> "$FAKE_GH_DIR/runner.log"
sha="$(git --git-dir="$ORIGIN_GIT" rev-parse --verify -q "refs/heads/$1" || git --git-dir="$ORIGIN_GIT" rev-parse --verify -q "$1^{commit}")" || { echo "no such ref $1"; exit 1; }
echo "[concrete-remote] ref=$1 sha=$sha"
EOF
chmod +x "$TMP/fake-gh" "$TMP/hook" "$TMP/notify.sh" "$TMP/train-nobisect.sh" "$TMP/fake-runner"

for r in origin lambdaclass fork; do git init -q --bare "$TMP/$r.git"; done
A="$TMP/agent"; W="$TMP/bot"
git init -q "$A"; cd "$A" || exit 2
for r in origin lambdaclass fork; do git remote add "$r" "$TMP/$r.git"; done
mkdir -p Concrete/Check Concrete/Elab src
printf 'def e := "E0001 existing"\n' > Concrete/Check/Check.lean
printf 'def x := 1\n' > Concrete/Elab/Elab.lean
printf 'x\n' > conflict.txt
git add -A && git commit -qm base
for r in origin lambdaclass fork; do git push -q "$r" main; done
BASE="$(git rev-parse HEAD)"
git clone -q "$TMP/origin.git" "$W"
( cd "$W" && git remote add lambdaclass "$TMP/lambdaclass.git" && git remote add fork "$TMP/fork.git" )

# mk <branch> <commit-message> <file> <content> [from]: one commit on <from> (default main), pushed.
mk(){ ( cd "$A" && git fetch -q origin && git checkout -q -B "$1" "${5:-origin/main}" && mkdir -p "$(dirname "$3")" \
        && printf '%s\n' "$4" > "$3" && git add -A && git commit -qm "$2" && git push -q -f origin "HEAD:refs/heads/$1" ); }
sha_of(){ git --git-dir="$TMP/origin.git" rev-parse "refs/heads/$1"; }
mark(){ # branch [review-verdict] [gates] [extra review key=value]
  local s; s="$(sha_of "$1")"
  ( cd "$A" && git fetch -q origin \
    && bash "$NOTES" put review "$s" verdict="${2:-clean}" rounds=1 reviewer=codex ${4:+"$4"} >/dev/null \
    && bash "$NOTES" put gates "$s" gates="${3:-pass}" selected=3 >/dev/null ) || no "mark $1"
}
bot(){ ( cd "$W" && bash "$BOT" "$@" ) 2>&1; }
main_sha(){ git --git-dir="$TMP/$1.git" rev-parse refs/heads/main; }
land_refs(){ git --git-dir="$TMP/origin.git" for-each-ref --format='%(refname)' 'refs/heads/wip/land-*'; }
in_main(){ git --git-dir="$TMP/origin.git" merge-base --is-ancestor "$1" refs/heads/main; }

echo "=== ready: both notes, at the exact head ==="
mk wip/a "a" src/a.txt a; mark wip/a
mk wip/nogates "nogates" src/ng.txt ng
( cd "$A" && bash "$NOTES" put review "$(sha_of wip/nogates)" verdict=clean rounds=1 reviewer=codex >/dev/null )
mk wip/findings "findings" src/f.txt f; mark wip/findings findings
mk wip/gatefail "gatefail" src/gf.txt gf; mark wip/gatefail clean fail
mk wip/stale "stale 1" src/s.txt s1; mark wip/stale
mk wip/stale "stale 2" src/s.txt s2 wip/stale
mk wip/land-x "land-x" src/lx.txt lx; mark wip/land-x
mk other/x "other" src/ox.txt ox; mark other/x
mk wip/rule "rule" Concrete/Check/Check.lean 'def e := "E0001 existing"
def f := "E0999 new rule"'; mark wip/rule
mk wip/checkonly "checkonly" Concrete/Elab/Elab.lean 'def x := "E0001 reused"'; mark wip/checkonly
mk wip/explicit "explicit" src/big.txt "$(seq 1 50)"; mark wip/explicit clean pass order=5
mk wip/late "late" src/late.txt late; mark wip/late clean pass order=0800
out="$(bot list)"; rc=$?
names="$(printf '%s\n' "$out" | awk '{print $3}' | tr '\n' ' ')"
[ "$rc" = 0 ] && ok "list runs" || { no "list exit $rc"; echo "$out" | sed 's/^/      /'; }
has " $names" " wip/a " && ok "a head with clean review and passing gates is ready" || no "wip/a not ready: $names"
for b in wip/nogates wip/findings wip/gatefail wip/stale wip/land-x other/x; do
  has " $names" " $b " && no "$b listed as ready" || ok "$b is not ready"
done
# The wip/land-x fixture has served; every wip/land-* ref after this point is the remote leg's.
git --git-dir="$TMP/origin.git" update-ref -d refs/heads/wip/land-x

echo "=== ordering ==="
[ "$names" = "wip/explicit wip/late wip/rule wip/a wip/checkonly " ] && ok "explicit orders (5, then 0800 read as decimal 800) before the rule change, then the rest by size" \
  || { no "order: '$names'"; echo "$out" | sed 's/^/      /'; }
has "$out" "language rule: new E0999" && ok "the rule branch is recognized by its new E-code" || no "rule reason missing"
has "$(printf '%s\n' "$out" | grep wip/checkonly)" "ordinary" && ok "a Check/Elab edit reusing an existing E-code is not a rule change" || no "checkonly misclassified"

echo "=== dry-run, kill switch, lock ==="
out="$(bot --dry-run tick)"; rc=$?
[ "$rc" = 0 ] && has "$out" "would build one train" && [ "$(main_sha origin)" = "$BASE" ] && [ ! -e "$LAND_STATE_DIR/bot/events.tsv" ] \
  && ok "dry-run prints the train and changes nothing" || { no "dry-run: exit $rc"; echo "$out" | sed 's/^/      /'; }
touch "$BOT_PAUSE_FILE"
out="$(bot tick)"; rc=$?
[ "$rc" = 0 ] && has "$out" "paused" && [ "$(main_sha origin)" = "$BASE" ] && ok "the kill switch stops the tick" || { no "pause: exit $rc"; echo "$out" | sed 's/^/      /'; }
acks(){ awk -F'\t' '$2=="paused"' "$LAND_STATE_DIR/bot/events.tsv" 2>/dev/null | grep -c . || true; }
[ "$(acks)" = 1 ] && has "$(cat "$TMP/gh/notify.log")" "paused|kill switch seen" && ok "the pause is acknowledged in the event log (and notified)" || no "no paused event (acks=$(acks))"
bot tick >/dev/null
[ "$(acks)" = 1 ] && ok "a second paused tick does not acknowledge the same pause again" || no "duplicate acknowledgment (acks=$(acks))"
rm -f "$BOT_PAUSE_FILE"
# The holder takes the same kernel lock the bot takes (flock(2) on an fd it keeps) and lets go on a
# signal file; it never hands the fd to a child it would have to kill.
mkdir -p "$LAND_STATE_DIR/bot"; L="$LAND_STATE_DIR/bot/lock"
( exec 9>"$L.flock"; python3 -I -c 'import fcntl; fcntl.flock(9, fcntl.LOCK_EX)'; touch "$TMP/held"
  while [ ! -e "$TMP/release" ]; do sleep 0.2; done ) & fl=$!
while [ ! -e "$TMP/held" ]; do sleep 0.1; done
out="$(bot tick)"; rc=$?
[ "$rc" = 0 ] && has "$out" "another instance holds" && [ "$(main_sha origin)" = "$BASE" ] && ok "a live holder of the kernel lock stops a second instance" || { no "kernel lock: exit $rc"; echo "$out" | sed 's/^/      /'; }
touch "$TMP/release"; wait "$fl" 2>/dev/null

git --git-dir="$TMP/origin.git" update-ref refs/heads/wip/land-control "$BASE"
[ -n "$(land_refs)" ] && ok "CONTROL: the leaked-ref probe sees a planted wip/land-* ref" || no "the leaked-ref probe is blind"
git --git-dir="$TMP/origin.git" update-ref -d refs/heads/wip/land-control

echo "=== train: the tick lands every ready branch, in order, on both remotes ==="
out="$(bot tick)"; rc=$?
[ "$rc" = 0 ] && ok "once the holder lets go, the next tick takes the lock and runs" || { no "tick after release: exit $rc"; echo "$out" | sed 's/^/      /'; }
all=1; for b in wip/explicit wip/late wip/rule wip/a wip/checkonly; do in_main "$(sha_of "$b")" || all=0; done
[ "$all" = 1 ] && ok "all five ready branches are in origin/main" || { no "a ready branch did not land"; echo "$out" | sed 's/^/      /'; }
[ "$(main_sha origin)" = "$(main_sha lambdaclass)" ] && ok "lambdaclass/main equals origin/main" || no "mirror differs"
[ "$(main_sha fork)" = "$BASE" ] && ok "fork untouched" || no "fork was pushed"
for b in wip/nogates wip/findings wip/gatefail wip/stale; do in_main "$(sha_of "$b")" && no "$b landed"; done
merges="$(git --git-dir="$TMP/origin.git" log --first-parent --reverse --format=%s "$BASE..refs/heads/main" | sed -n 's/.*merge refs\/bot\/\([^ ]*\) .*/\1/p' | tr '\n' ' ')"
[ "$merges" = "wip/explicit wip/late wip/rule wip/a wip/checkonly " ] && ok "the train merged members in landing order" || no "merge order: '$merges'"
rl="$(cat "$TMP/gh/runner.log" 2>/dev/null)"
has "$rl" "wip/land-bot-" && [ "$(printf '%s\n' "$rl" | sort | uniq -d)" = "" ] \
  && [ -z "$(land_refs)" ] \
  && ok "the remote leg ran on a per-run wip/land-* ref, and the ref is gone afterwards" || { no "remote leg refs: '$rl', left: '$(land_refs)'"; cat "$LAND_STATE_DIR"/bot-*/check-*/remote.log 2>/dev/null | grep 'remote:' | sed 's/^/      /'; }
has "$(cat "$TMP/gh/notify.log" 2>/dev/null)" "landed|train bot-" && ok "notify.sh was called with the landing" || no "no landing notification"
has "$(cat "$LAND_STATE_DIR/bot/bot.env")" "phase=idle" && ok "state is idle after landing" || no "state not idle"

echo "=== bisect: a red check ejects the culprit and the rest lands ==="
mk wip/g1 g1 src/g1.txt g1; mark wip/g1
mk wip/bad bad BAD bad; mark wip/bad
mk wip/g2 g2 src/g2.txt g2; mark wip/g2
BADSHA="$(sha_of wip/bad)"
out="$(bot tick)"; rc=$?
in_main "$(sha_of wip/g1)" && in_main "$(sha_of wip/g2)" && ok "the good members landed" || { no "good members missing"; echo "$out" | sed 's/^/      /'; }
in_main "$BADSHA" && no "the red member landed" || ok "the red member did not land"
has "$out" "[ejected] wip/bad (${BADSHA:0:10})" && ok "the culprit is ejected by name" || no "no eject event for wip/bad"
[ "$(cd "$W" && bash "$NOTES" get gates "$BADSHA" gates)" = fail ] && ok "gates: fail published at the culprit's SHA" || no "culprit's gates note not failed"
has "$(cat "$TMP/gh/notify.log")" "ejected|wip/bad" && ok "the ejection was notified" || no "no eject notification"
out="$(bot list)"; has "$out" "wip/bad" && no "the ejected head is still ready" || ok "the ejected head is no longer ready"

echo "=== bisect: a red CI run ejects the culprit ==="
mk wip/cired cired CIRED red; mark wip/cired
mk wip/g3 g3 src/g3.txt g3; mark wip/g3
out="$(bot tick)"; rc=$?
in_main "$(sha_of wip/g3)" && ! in_main "$(sha_of wip/cired)" && has "$out" "[ejected] wip/cired" \
  && ok "CI-red member ejected, the rest landed" || { no "CI-red bisect"; echo "$out" | sed 's/^/      /'; }

echo "=== kill switch reaches bisection; a block waits for a human ==="
mk wip/kb kb BAD kb; mark wip/kb
mk wip/kg kg src/kg.txt kg; mark wip/kg
KB="$(sha_of wip/kb)"
touch "$TMP/gh/pause-on-bisect"
out="$(bot tick)"; rc=$?
rm -f "$TMP/gh/pause-on-bisect"
has "$out" "stopped by the kill switch" && ! has "$out" "[ejected]" && ! has "$(cat "$LAND_STATE_DIR/bot/bot.env")" "phase=blocked" && ! in_main "$(sha_of wip/kg)" \
  && ok "a pause during bisection stops it: nothing ejected, nothing landed, not blocked" || { no "pause in bisect"; echo "$out" | sed 's/^/      /'; }
rm -f "$BOT_PAUSE_FILE"
out="$(BOT_TRAIN="$TMP/train-nobisect.sh" bot tick)"; rc=$?
has "$out" "gave no culprit" && has "$(cat "$LAND_STATE_DIR/bot/bot.env")" "phase=blocked" && ok "a bisection that names no culprit blocks the bot" || { no "block"; echo "$out" | sed 's/^/      /'; }
out="$(bot tick)"; rc=$?
has "$out" "blocked-still" && has "$(cat "$LAND_STATE_DIR/bot/bot.env")" "phase=blocked" && ! in_main "$(sha_of wip/kg)" \
  && ok "the next tick keeps the block (no automatic retry)" || { no "block overwritten"; echo "$out" | sed 's/^/      /'; }
bot reset >/dev/null
out="$(bot tick)"; rc=$?
in_main "$(sha_of wip/kg)" && ! in_main "$KB" && has "$out" "[ejected] wip/kb" && ok "after reset the culprit is ejected and the rest lands" || { no "after reset"; echo "$out" | sed 's/^/      /'; }

echo "=== resume: a restart while the train is in CI does not dispatch again ==="
mk wip/r r src/r.txt r; mark wip/r
touch "$TMP/gh/pause-on-dispatch"
before="$(grep -c 'workflow run' "$TMP/gh/calls.log")"
out="$(bot tick)"; rc=$?
rm -f "$TMP/gh/pause-on-dispatch"
has "$(cat "$LAND_STATE_DIR/bot/bot.env")" "phase=ci" && ! in_main "$(sha_of wip/r)" && has "$out" "landing waits" \
  && ok "the kill switch between CI and land holds the train in CI" || { no "pause before land"; echo "$out" | sed 's/^/      /'; }
rm -f "$BOT_PAUSE_FILE"
out="$(bot tick)"; rc=$?
after="$(grep -c 'workflow run' "$TMP/gh/calls.log")"
has "$out" "watching it instead of dispatching again" && in_main "$(sha_of wip/r)" && [ $((after - before)) = 1 ] \
  && ok "the restarted bot resumed the run and landed with one dispatch" || { no "resume: dispatches $((after-before))"; echo "$out" | sed 's/^/      /'; }

echo "=== kill switch inside a bisection probe ==="
mk wip/pb pb BAD pb; mark wip/pb
mk wip/pg pg src/pg.txt pg; mark wip/pg
echo 2 > "$TMP/gh/pause-countdown"   # 1: the train's check; 2: the last halving probe's check
out="$(bot tick)"; rc=$?
rm -f "$TMP/gh/pause-countdown"
blog="$(cat "$LAND_STATE_DIR"/bot/bisect-*.log 2>/dev/null)"
has "$out" "stopped by the kill switch" && has "$blog" "no culprit attributed" && ! has "$out" "[ejected]" \
  && ! has "$(cat "$LAND_STATE_DIR/bot/bot.env")" "phase=blocked" && ok "a pause during the halving probe stops the bisection; nothing attributed" \
  || { no "pause before culprit probe"; echo "$out" | sed 's/^/      /'; }
rm -f "$BOT_PAUSE_FILE" "$LAND_STATE_DIR"/bot/bisect-*.log
out="$(bot tick)"
in_main "$(sha_of wip/pg)" && has "$out" "[ejected] wip/pb" && ok "after the pause, the same train is bisected and the rest lands" || { no "after probe pause"; echo "$out" | sed 's/^/      /'; }
mk wip/pc pc CIRED pc; mark wip/pc
mk wip/ph ph src/ph.txt ph; mark wip/ph
before="$(grep -c 'workflow run' "$TMP/gh/calls.log")"
echo 2 > "$TMP/gh/pause-countdown"   # 1: the train's check; 2: the first --ci probe's check
out="$(bot tick)"; rc=$?
rm -f "$TMP/gh/pause-countdown"
after="$(grep -c 'workflow run' "$TMP/gh/calls.log")"
[ $((after - before)) = 1 ] && has "$(cat "$LAND_STATE_DIR/bot/bot.env")" "phase=ci" && ! has "$out" "[ejected]" \
  && ok "a pause during a --ci probe's check dispatches nothing more; the red train stays in CI" || { no "--ci probe pause: dispatches $((after-before))"; echo "$out" | sed 's/^/      /'; }
rm -f "$BOT_PAUSE_FILE"
out="$(bot tick)"
in_main "$(sha_of wip/ph)" && ! in_main "$(sha_of wip/pc)" && has "$out" "[ejected] wip/pc" && ok "resumed: the red run is re-read, bisected, the culprit ejected" || { no "after --ci pause"; echo "$out" | sed 's/^/      /'; }

mk wip/qb qb BAD qb; mark wip/qb
mk wip/qg qg src/qg.txt qg; mark wip/qg
rm -f "$LAND_STATE_DIR"/bot/bisect-*.log
touch "$TMP/gh/pause-in-culprit-probe"   # the hook pulls the switch while checking the culprit alone (red)
out="$(bot tick)"; rc=$?
rm -f "$TMP/gh/pause-in-culprit-probe"
blog="$(cat "$LAND_STATE_DIR"/bot/bisect-*.log 2>/dev/null)"
has "$out" "stopped by the kill switch" && ! has "$out" "[ejected]" && ! has "$blog" "train: culprit:" && has "$blog" "while probing refs/bot/wip/qb alone" \
  && ok "a pause raised DURING the red culprit probe attributes nothing" || { no "pause in culprit probe"; echo "$out" | sed 's/^/      /'; echo "$blog" | sed 's/^/      /'; }
rm -f "$BOT_PAUSE_FILE" "$LAND_STATE_DIR"/bot/bisect-*.log
out="$(bot tick)"
in_main "$(sha_of wip/qg)" && has "$out" "[ejected] wip/qb" && ok "after the pause the culprit is ejected and the rest lands" || { no "after culprit-probe pause"; echo "$out" | sed 's/^/      /'; }

echo "=== remote leg: a same-SHA ref of another run is never adopted ==="
mk wip/cx cx src/cx.txt cx; mark wip/cx
( cd "$W" && git fetch -q origin && bash "$ROOT_DIR/scripts/land/train.sh" build cx-t --no-fetch origin/wip/cx >/dev/null 2>&1 \
  && git push -q origin "HEAD:refs/heads/wip/land-collide" )
CXH="$(cd "$W" && git rev-parse HEAD)"
: > "$TMP/gh/runner.log"
out="$(cd "$W" && LAND_REMOTE_REF_FIRST=wip/land-collide bash "$ROOT_DIR/scripts/land/train.sh" check cx-t --no-review 2>&1)"; rc=$?
rlog="$(cat "$LAND_STATE_DIR"/cx-t/check-*/remote.log 2>/dev/null)"
[ "$rc" = 0 ] && has "$rlog" "wip/land-collide was not created by this run" && ! has "$(cat "$TMP/gh/runner.log")" "wip/land-collide" \
  && [ "$(git --git-dir="$TMP/origin.git" rev-parse refs/heads/wip/land-collide)" = "$CXH" ] && [ "$(land_refs)" = "refs/heads/wip/land-collide" ] \
  && ok "an existing ref at the same SHA is refused, a fresh name is used, and only the run's own ref is deleted" || { no "collision: exit $rc, refs '$(land_refs)'"; echo "$rlog" | sed 's/^/      /'; }
git --git-dir="$TMP/origin.git" update-ref -d refs/heads/wip/land-collide
( cd "$W" && git checkout -q --detach origin/main )

echo "=== dispatch: a restart between dispatch and recording the run reconciles, never re-dispatches ==="
mk wip/d1 d1 src/d1.txt d1; mark wip/d1
touch "$TMP/gh/pause-on-dispatch"
out="$(bot tick)"
rm -f "$TMP/gh/pause-on-dispatch" "$BOT_PAUSE_FILE"
T="$(sed -n 's/^train=//p' "$LAND_STATE_DIR/bot/bot.env" | tail -n 1)"
sed -i.bak 's/^phase=ci$/phase=dispatching/' "$LAND_STATE_DIR/bot/bot.env"
sed -i.bak '/^run_id=/d' "$LAND_STATE_DIR/$T/train.env"
before="$(grep -c 'workflow run' "$TMP/gh/calls.log")"
touch "$TMP/gh/list-fails"
out="$(bot tick)"; rc=$?
rm -f "$TMP/gh/list-fails"
has "$out" "lookup failed" && has "$(cat "$LAND_STATE_DIR/bot/bot.env")" "phase=dispatching" && ok "a failed run lookup is no answer: the train stays dispatching" || { no "lookup failure"; echo "$out" | sed 's/^/      /'; }
touch "$TMP/gh/list-empty"
out="$(bot tick)"; rc=$?
rm -f "$TMP/gh/list-empty"
has "$out" "no run visible yet" && has "$(cat "$LAND_STATE_DIR/bot/bot.env")" "phase=dispatching" && ok "an empty answer inside the grace period waits instead of re-dispatching" || { no "empty lookup"; echo "$out" | sed 's/^/      /'; }
out="$(bot tick)"; rc=$?
after="$(grep -c 'workflow run' "$TMP/gh/calls.log")"
has "$out" "before the restart; watching it" && in_main "$(sha_of wip/d1)" && [ "$after" = "$before" ] \
  && ok "the run was found by its head SHA and landed with no second dispatch" || { no "reconcile: dispatches $((after-before))"; echo "$out" | sed 's/^/      /'; }

echo "=== auto-rebase: skip rules, scratch worktree, refreshed notes, conflicts ==="
mk wip/r1 r1 src/r1.txt r1; mark wip/r1
mk wip/rc rc conflict.txt rc; mark wip/rc
R1="$(sha_of wip/r1)"; RC="$(sha_of wip/rc)"
( cd "$A" && git fetch -q origin && git checkout -q -B mainmove origin/main && printf 'main\n' > conflict.txt \
  && git commit -qam "main moves" && git push -q origin HEAD:main )
NEWMAIN="$(main_sha origin)"
unset BOT_NO_REBASE
out="$(bot rebase)"; rc=$?
[ "$rc" = 0 ] && has "$out" "skip wip/r1: last activity" && has "$out" "skip wip/rc: last activity" && [ "$(sha_of wip/r1)" = "$R1" ] \
  && ok "heads with activity in the last 10 minutes are not rebased" || { no "active skip: exit $rc"; echo "$out" | sed 's/^/      /'; }
export BOT_NOW=$(( $(date +%s) + 3600 ))
out="$(bot rebase)"; rc=$?
N1="$(sha_of wip/r1)"
[ "$N1" != "$R1" ] && git --git-dir="$TMP/origin.git" merge-base --is-ancestor "$NEWMAIN" "$N1" && has "$out" "[rebased] wip/r1" \
  && ok "a quiet ready head is rebased onto the new main and pushed" || { no "rebase r1"; echo "$out" | sed 's/^/      /'; }
rv="$(cd "$W" && bash "$NOTES" get review "$N1")"
has "$rv" "verdict: clean" && has "$rv" "carried-from: $R1" && [ "$(cd "$W" && bash "$NOTES" get gates "$N1" gates)" = pass ] \
  && ok "the rebased head carries the review verdict (naming the reviewed SHA) and a fresh gates note" || no "notes on rebased head: $rv"
has "$out" "[rebase-conflict] wip/rc" && has "$out" "conflicts in: conflict.txt" && [ "$(sha_of wip/rc)" = "$RC" ] \
  && ok "a real conflict is notified with its files and the branch is left alone" || { no "rebase conflict"; echo "$out" | sed 's/^/      /'; }
has "$(cat "$TMP/gh/notify.log")" "rebase-conflict|wip/rc" && ok "the conflict was notified" || no "no conflict notification"
inprog=0; for p in rebase-merge rebase-apply MERGE_HEAD; do [ -e "$W/.git/$p" ] && inprog=1; done
[ "$inprog" = 0 ] && [ "$(cd "$W" && git worktree list | wc -l | tr -d ' ')" = 1 ] && [ -z "$(cd "$W" && git status --porcelain --untracked-files=no)" ] \
  && ok "no rebase left in progress, scratch worktrees removed, clone clean" || no "rebase left state behind"
out="$(bot rebase)"
has "$out" "rebase-conflict" && no "the same conflict was retried" || ok "a tried (head, main) pair is not retried"
unset BOT_NOW

echo "=== ready.sh: legs in parallel; any failed leg (the review included) publishes gates: fail ==="
mk wip/rv rv src/rv.txt rv; mark wip/rv
RV="$(sha_of wip/rv)"
out="$(cd "$A" && LAND_REVIEW_CMD=false bash "$ROOT_DIR/scripts/land/ready.sh" wip/rv 2>&1)"; rc=$?
g="$(cd "$A" && bash "$NOTES" get gates "$RV")"
[ "$rc" != 0 ] && has "$g" "gates: fail" && has "$g" "reason: failed legs: review" && has "$out" "run one after another" \
  && ok "a failed review leg revokes readiness even over an older clean verdict" || { no "ready.sh review failure: exit $rc, note '$g'"; echo "$out" | sed 's/^/      /'; }
has "$(bot list)" "wip/rv" && no "wip/rv still listed as ready" || ok "the bot no longer sees wip/rv as ready"
mk wip/rv2 rv2 src/rv2.txt rv2; mark wip/rv2 clean fail
out="$(cd "$A" && LAND_REVIEW_CMD=true bash "$ROOT_DIR/scripts/land/ready.sh" wip/rv2 2>&1)"; rc=$?
[ "$rc" = 0 ] && [ "$(cd "$A" && bash "$NOTES" get gates "$(sha_of wip/rv2)" gates)" = pass ] && has "$(bot list)" "wip/rv2" \
  && ok "passing legs publish gates: pass and the head becomes ready" || { no "ready.sh pass: exit $rc"; echo "$out" | sed 's/^/      /'; }

echo "=== main is never force-pushed ==="
bad="$(grep -nE 'git push[^#]*(--force|[[:space:]]-f[[:space:]])[^#]*refs/heads/main' "$ROOT_DIR"/scripts/land/*.sh || true)"
[ -z "$bad" ] && ok "no forced push of refs/heads/main in scripts/land" || { no "forced push of main:"; echo "$bad" | sed 's/^/      /'; }

echo
echo "check_landing_bot: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
