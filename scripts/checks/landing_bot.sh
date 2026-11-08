#!/usr/bin/env bash
# THE LANDING BOT LANDS ONLY READY HEADS, IN RULE-FIRST ORDER, AND STOPS WHEN TOLD (R-0539).
#
# scripts/land/bot.sh takes the orchestrator out of the landing loop: it collects ready branches,
# builds one train, checks it, runs CI, lands it on main, bisects and ejects a red member,
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
#   train      the tick lands every ready branch in order on origin; fork untouched
#   bisect     a red check or a red CI ejects exactly the culprit, publishes `gates: fail` at its
#              SHA, notifies, and lands the rest
#   resume     a restart while a train is in CI watches it instead of dispatching again
#   rebase     heads with recent activity are skipped; a quiet head is rebased in a scratch clone,
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

source "$ROOT_DIR/scripts/checks/lib/git_sandbox.sh" || exit 2
git_sandbox "$TMP/gitconfig" || exit 2
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
# train.sh runs only with the repository's pre-push hook enabled (ISS-0015): it asks the hook git
# would run who it is (its single argument --concrete-identify). The sandbox's global hooksPath holds this
# stand-in, which runs no gate and answers as the repository's .githooks/pre-push would: a test
# fixture impersonating the hook on purpose.
IDENTIFY='[ "$#" = 1 ] && [ "$1" = --concrete-identify ] && { echo "concrete-pre-push $(cd "$(git rev-parse --show-toplevel)" && pwd -P)/.githooks/pre-push"; exit 0; }'
standin_hook() {
  printf '#!/usr/bin/env bash\n%s\nexit 0\n' "$IDENTIFY" > "$TMP/nohooks/pre-push"
  chmod +x "$TMP/nohooks/pre-push"
}
standin_hook
export LAND_PUSH_RETRY_SLEEP=0
export LAND_NO_NIX=1 LAND_STATE_DIR="$TMP/landstate" LAND_GH="$TMP/fake-gh" LAND_REPO=test/repo
export LAND_HOOK="$TMP/hook" LAND_REMOTE_RUNNER="$TMP/fake-runner" FAKE_GH_DIR="$TMP/gh" ORIGIN_GIT="$TMP/origin.git"
export BOT_PAUSE_FILE="$TMP/paused" BOT_NOTIFY="$TMP/notify.sh" BOT_WATCH_INTERVAL=1 BOT_NO_REBASE=1
unset BOT_NOW BOT_STATE_DIR BOT_CHECK_ARGS
# train.sh check never skips the review leg silently, and a bisection probe runs it. Every member here
# is marked reviewed clean (mark), so the leg passes on the members' provenance without calling the
# reviewer; a reviewer this fails stands for one that is never needed.
export LAND_REVIEW_CMD=false

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

for r in origin fork; do git init -q --bare "$TMP/$r.git"; done
A="$TMP/agent"; W="$TMP/bot"
git init -q "$A"; cd "$A" || exit 2
for r in origin fork; do git remote add "$r" "$TMP/$r.git"; done
mkdir -p Concrete/Check Concrete/Elab src
printf 'def e := "E0001 existing"\n' > Concrete/Check/Check.lean
printf 'def x := 1\n' > Concrete/Elab/Elab.lean
printf 'x\n' > conflict.txt
printf 'Build\n' > required-checks.txt  # train.sh refuses a landing with no required-checks.txt
git add -A && git commit -qm base
for r in origin fork; do git push -q "$r" main; done
BASE="$(git rev-parse HEAD)"
git clone -q "$TMP/origin.git" "$W"
( cd "$W" && git remote add fork "$TMP/fork.git" )

# mk <branch> <commit-message> <file> <content> [from]: one commit on <from> (default main), pushed.
mk(){ ( cd "$A" && git fetch -q origin && git checkout -q -B "$1" "${5:-origin/main}" && mkdir -p "$(dirname "$3")" \
        && printf '%s\n' "$4" > "$3" && git add -A && git commit -qm "$2" && git push -q -f origin "HEAD:refs/heads/$1" ); }
sha_of(){ git --git-dir="$TMP/origin.git" rev-parse "refs/heads/$1"; }
mark(){ # branch [review-verdict] [gates] [extra review key=value]
  local s; s="$(sha_of "$1")"
  ( cd "$A" && git fetch -q origin \
    && bash "$NOTES" put review "$s" verdict="${2:-clean}" rounds=1 reviewer=codex base="$s" pieces=1 ${4:+"$4"} >/dev/null \
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

echo "=== train: the tick lands every ready branch, in order, on main ==="
out="$(bot tick)"; rc=$?
[ "$rc" = 0 ] && ok "once the holder lets go, the next tick takes the lock and runs" || { no "tick after release: exit $rc"; echo "$out" | sed 's/^/      /'; }
all=1; for b in wip/explicit wip/late wip/rule wip/a wip/checkonly; do in_main "$(sha_of "$b")" || all=0; done
[ "$all" = 1 ] && ok "all five ready branches are in origin/main" || { no "a ready branch did not land"; echo "$out" | sed 's/^/      /'; }
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

echo "=== auto-rebase: skip rules, scratch clone, refreshed notes, conflicts ==="
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
# The scratch is a clone, so its notes reach this clone through the remote, as they reach anyone.
rv="$(cd "$W" && bash "$NOTES" fetch >/dev/null && bash "$NOTES" get review "$N1")"
has "$rv" "verdict: clean" && has "$rv" "carried-from: $R1" && [ "$(cd "$W" && bash "$NOTES" get gates "$N1" gates)" = pass ] \
  && ok "the rebased head carries the review verdict (naming the reviewed SHA) and a fresh gates note" || no "notes on rebased head: $rv"
has "$out" "[rebase-conflict] wip/rc" && has "$out" "conflicts in: conflict.txt" && [ "$(sha_of wip/rc)" = "$RC" ] \
  && ok "a real conflict is notified with its files and the branch is left alone" || { no "rebase conflict"; echo "$out" | sed 's/^/      /'; }
has "$(cat "$TMP/gh/notify.log")" "rebase-conflict|wip/rc" && ok "the conflict was notified" || no "no conflict notification"
inprog=0; for p in rebase-merge rebase-apply MERGE_HEAD; do [ -e "$W/.git/$p" ] && inprog=1; done
[ "$inprog" = 0 ] && [ "$(cd "$W" && git worktree list | wc -l | tr -d ' ')" = 1 ] && [ -z "$(cd "$W" && git status --porcelain --untracked-files=no)" ] \
  && ok "no rebase left in progress, no worktree, scratch clones removed, clone clean" || no "rebase left state behind"
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

echo "=== the bot refuses a remote that is not the one repository ==="
( cd "$W" && git remote add gh-stale https://github.com/unbalancedparentheses/concrete2.git )
heads_before="$(git --git-dir="$ORIGIN_GIT" for-each-ref --format='%(refname) %(objectname)')"
# A git shim logs every network operation and the fake gh every call: a refused pass logs neither.
mkdir -p "$TMP/netshim"
printf '#!/bin/sh\nfor a in "$@"; do case "$a" in fetch|push|ls-remote|clone) case " $* " in *" --get-url "*) ;; *) echo "$*" >> "%s" ;; esac; break ;; esac; done\nexec "%s" "$@"\n' "$TMP/netshim.log" "$(command -v git)" > "$TMP/netshim/git"
chmod +x "$TMP/netshim/git"
for sub in tick rebase list; do
  : > "$TMP/netshim.log"; g0="$(cat "$FAKE_GH_DIR/calls.log" 2>/dev/null | wc -l)"
  out="$(PATH="$TMP/netshim:$PATH" LAND_REMOTE=gh-stale bot "$sub")"; rc=$?
  [ "$rc" != 0 ] && has "$out" "points at another repository" && [ ! -s "$TMP/netshim.log" ] && [ "$(cat "$FAKE_GH_DIR/calls.log" 2>/dev/null | wc -l)" = "$g0" ] \
    && ok "bot $sub refuses a stale GitHub remote before any fetch, push or gh call" || { no "bot $sub accepted a stale remote: exit $rc"; echo "$out" | sed 's/^/      /'; sed 's/^/      net: /' "$TMP/netshim.log"; }
done
out="$(LAND_REPO=unbalancedparentheses/concrete2 LAND_REMOTE=gh-stale bot rebase)"; rc=$?
[ "$rc" != 0 ] && has "$out" "points at another repository" && ok "bot rebase refuses an old origin even with an old LAND_REPO" || no "bot rebase accepted old origin + old LAND_REPO: exit $rc"
[ "$(git --git-dir="$ORIGIN_GIT" for-each-ref --format='%(refname) %(objectname)')" = "$heads_before" ] \
  && ok "no branch or note moved on a refused pass" || no "a refused bot pass moved refs on origin"
( cd "$W" && git remote remove gh-stale )

echo "=== the installer migrates a stale clone and its launcher checks the remote before fetching ==="
# The real install-bot.sh, in a sandboxed HOME, with a local origin holding scripts/land and stubs for
# ssh, gh and systemctl (nothing here reaches the network or the user manager).
BOUNDED_PARENT="$TMP"; source "$ROOT_DIR/scripts/checks/lib/bounded.sh" || exit 2
IB="$TMP/ib"; mkdir -p "$IB/home/.config" "$IB/stub" "$IB/src/scripts/checks/lib"
for c in ssh gh systemctl; do printf '#!/bin/sh\nexit 1\n' > "$IB/stub/$c"; chmod +x "$IB/stub/$c"; done
# ssh -G connects nowhere and only reports the configuration: the stub answers it as an ssh with no
# configuration would (every host is itself), and refuses everything else.
printf '#!/bin/sh\n[ "$1" = -G ] || exit 1\nfor h; do :; done\necho "hostname $h"\n' > "$IB/stub/ssh"
cp -r "$ROOT_DIR/scripts/land" "$IB/src/scripts/"; cp "$ROOT_DIR/scripts/checks/lib/git_sandbox.sh" "$IB/src/scripts/checks/lib/"
( cd "$IB/src" && git init -q && git add -A && git commit -qm src ) && git clone -q --bare "$IB/src" "$IB/origin.git"
printf 'LAND_REPO=unbalancedparentheses/concrete2\n' > "$IB/home/.config/concrete-landing-bot.env"
chmod 600 "$IB/home/.config/concrete-landing-bot.env"   # it may hold secrets: migration keeps the mode
ib(){ HOME="$IB/home" PATH="$IB/stub:$PATH" INSTALL_ORIGIN_URL="$IB/origin.git" bash "$ROOT_DIR/scripts/land/install-bot.sh" 2>&1; }
out="$(ib)"; rc=$?
[ "$rc" = 0 ] && grep -qx 'LAND_REPO=lambdaclass/concrete' "$IB/home/.config/concrete-landing-bot.env" \
  && ! grep -q 'concrete2' "$IB/home/.config/concrete-landing-bot.env" \
  && [ "$(ls -l "$IB/home/.config/concrete-landing-bot.env" | cut -c1-10)" = "-rw-------" ] \
  && ok "the installer migrates an old LAND_REPO line in an existing environment file, keeping its mode (0600)" || { no "installer env migration: exit $rc"; echo "$out" | sed 's/^/      /'; }
# A stale line beside a canonical one is migrated too, in either order (systemd takes the last).
for lines in 'LAND_REPO=lambdaclass/concrete\nLAND_REPO=unbalancedparentheses/concrete2\n' 'LAND_REPO=unbalancedparentheses/concrete2\nLAND_REPO=lambdaclass/concrete\n'; do
  printf "$lines" > "$IB/home/.config/concrete-landing-bot.env"
  out="$(ib)"; rc=$?
  [ "$rc" = 0 ] && [ "$(grep -c '^LAND_REPO=' "$IB/home/.config/concrete-landing-bot.env")" = 2 ] \
    && [ -z "$(grep '^LAND_REPO=' "$IB/home/.config/concrete-landing-bot.env" | grep -Fvx 'LAND_REPO=lambdaclass/concrete')" ] \
    && ok "the installer migrates a stale LAND_REPO line beside a canonical one ($(printf "$lines" | head -n 1 | cut -d/ -f2) first)" || { no "installer duplicate LAND_REPO: exit $rc"; echo "$out" | sed 's/^/      /'; }
done
# A fresh install logs its origin redacted (the clone itself is refused by a git that does no network).
mkdir -p "$IB/home2" "$IB/nonet"
printf '#!/bin/sh\n[ "$1" = clone ] && { echo "git shim: no network" >&2; exit 1; }\nexec "%s" "$@"\n' "$(command -v git)" > "$IB/nonet/git"; chmod +x "$IB/nonet/git"
for c in ssh gh systemctl; do cp "$IB/stub/$c" "$IB/nonet/$c"; done
out="$(HOME="$IB/home2" PATH="$IB/nonet:$PATH" INSTALL_ORIGIN_URL='https://x-access-token:pre@SENTINEL-SECRET@github.com/lambdaclass/concrete.git' bash "$ROOT_DIR/scripts/land/install-bot.sh" 2>&1)"; rc=$?
[ "$rc" != 0 ] && has "$out" "cloning https://***@github.com/lambdaclass/concrete.git" && ! has "$out" "SENTINEL" \
  && ok "a fresh install logs its origin with the credentials redacted" || { no "fresh install credential output: exit $rc"; echo "$out" | sed 's/^/      /'; }
BC="$IB/home/concrete-bot"
git -C "$BC" remote set-url origin https://github.com/unbalancedparentheses/concrete2.git
git -C "$BC" config --add remote.origin.url https://github.com/unbalancedparentheses/concrete2.git
git -C "$BC" remote set-url --push origin https://github.com/unbalancedparentheses/concrete2.git
out="$(ib)"; rc=$?
[ "$rc" = 0 ] && [ "$(git -C "$BC" config --get-all remote.origin.url)" = "$IB/origin.git" ] && ! git -C "$BC" config --get remote.origin.pushurl >/dev/null \
  && ok "the installer repoints every origin url and drops a stale push URL" || { no "installer repoint: exit $rc, urls '$(git -C "$BC" config --get-all remote.origin.url | tr '\n' ' ')'"; echo "$out" | sed 's/^/      /'; }
# A fresh install checks its URL before it clones: a foreign origin URL (also one that
# url.*.insteadOf rewrites, as git clone would, to another repository) is refused with no clone and
# no fetch. (An existing clone is repointed, as above, and then guarded before it fetches.)
printf '#!/bin/sh\nfor a in "$@"; do case "$a" in fetch|push|ls-remote|clone) case " $* " in *" --get-url "*) ;; *) echo "$*" >> "%s" ;; esac; break ;; esac; done\nexec "%s" "$@"\n' "$IB/inst.log" "$(command -v git)" > "$IB/nonet/git"; chmod +x "$IB/nonet/git"
for env in "INSTALL_ORIGIN_URL=https://github.com/someone/else.git" "INSTALL_ORIGIN_URL=https://github.com/unbalancedparentheses/concrete2.git" \
           "GIT_CONFIG_COUNT=1 GIT_CONFIG_KEY_0=url.https://github.com/someone/else.git.insteadOf GIT_CONFIG_VALUE_0=https://github.com/lambdaclass/concrete.git INSTALL_ORIGIN_URL=https://github.com/lambdaclass/concrete.git"; do
  rm -rf "$IB/home3"; mkdir -p "$IB/home3"; : > "$IB/inst.log"
  out="$(env HOME="$IB/home3" PATH="$IB/nonet:$PATH" $env bash "$ROOT_DIR/scripts/land/install-bot.sh" 2>&1)"; rc=$?
  [ "$rc" != 0 ] && has "$out" "refusing" && has "$out" "not one of the project's repositories" && [ ! -s "$IB/inst.log" ] && [ ! -e "$IB/home3/concrete-bot" ] \
    && ok "a fresh install refuses a foreign URL before cloning ($env)" || { no "installer foreign URL ($env): exit $rc"; echo "$out" | sed 's/^/      /'; sed 's/^/      net: /' "$IB/inst.log"; }
done
# A git shim logs every fetch: a refused launcher must log none.
printf '#!/bin/sh\nfor a in "$@"; do case "$a" in fetch|push|ls-remote) case " $* " in *" --get-url "*) ;; *) echo "$*" >> "%s" ;; esac; break ;; esac; done\nexec "%s" "$@"\n' "$IB/git.log" "$(command -v git)" > "$IB/stub/git"
chmod +x "$IB/stub/git"
for stale in https://github.com/unbalancedparentheses/concrete2.git git@github-old:unbalancedparentheses/concrete2.git; do
  git -C "$BC" remote set-url origin "$stale"
  : > "$IB/git.log"
  out="$(bounded_run -i /dev/null 60 -- env HOME="$IB/home" PATH="$IB/stub:$PATH" "$IB/home/.local/libexec/concrete-landing-bot" status 2>&1)"; rc=$?
  [ "$rc" != 0 ] && has "$out" "refusing to fetch" && has "$out" "points at another repository" && [ ! -s "$IB/git.log" ] \
    && ok "the launcher refuses origin=$stale before its first fetch" || { no "launcher with origin=$stale: exit $rc"; echo "$out" | sed 's/^/      /'; }
done
git -C "$BC" remote set-url origin "$IB/origin.git"
: > "$IB/git.log"
out="$(bounded_run -i /dev/null 60 -- env HOME="$IB/home" PATH="$IB/stub:$PATH" "$IB/home/.local/libexec/concrete-landing-bot" status 2>&1)"; rc=$?
[ "$rc" = 0 ] && has "$out" "state (" && grep -q fetch "$IB/git.log" && ok "the launcher fetches and runs the bot when origin is a local repository (the shim sees the fetch)" || { no "launcher with a good origin: exit $rc"; echo "$out" | sed 's/^/      /'; }

echo "=== main is never force-pushed ==="
bad="$(grep -nE 'git push[^#]*(--force|[[:space:]]-f[[:space:]])[^#]*refs/heads/main' "$ROOT_DIR"/scripts/land/*.sh || true)"
[ -z "$bad" ] && ok "no forced push of refs/heads/main in scripts/land" || { no "forced push of main:"; echo "$bad" | sed 's/^/      /'; }

echo
echo "check_landing_bot: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
