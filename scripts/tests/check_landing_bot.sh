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
export LAND_NO_NIX=1 LAND_STATE_DIR="$TMP/landstate" LAND_GH="$TMP/fake-gh" LAND_REPO=test/repo
export LAND_HOOK="$TMP/hook" LAND_REMOTE_RUNNER="" FAKE_GH_DIR="$TMP/gh" ORIGIN_GIT="$TMP/origin.git"
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
[ ! -e BAD ]
EOF
cat > "$TMP/notify.sh" <<'EOF'
#!/usr/bin/env bash
printf '%s|%s\n' "$1" "$2" >> "$FAKE_GH_DIR/notify.log"
EOF
chmod +x "$TMP/fake-gh" "$TMP/hook" "$TMP/notify.sh"

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
out="$(bot list)"; rc=$?
names="$(printf '%s\n' "$out" | awk '{print $3}' | tr '\n' ' ')"
[ "$rc" = 0 ] && ok "list runs" || { no "list exit $rc"; echo "$out" | sed 's/^/      /'; }
has " $names" " wip/a " && ok "a head with clean review and passing gates is ready" || no "wip/a not ready: $names"
for b in wip/nogates wip/findings wip/gatefail wip/stale wip/land-x other/x; do
  has " $names" " $b " && no "$b listed as ready" || ok "$b is not ready"
done

echo "=== ordering ==="
[ "$names" = "wip/explicit wip/rule wip/a wip/checkonly " ] && ok "explicit order, then the rule change, then the rest by size" \
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
rm -f "$BOT_PAUSE_FILE"
mkdir -p "$LAND_STATE_DIR/bot/lock"; sleep 60 & live=$!; echo "$live" > "$LAND_STATE_DIR/bot/lock/pid"
out="$(bot tick)"; rc=$?
[ "$rc" = 0 ] && has "$out" "another instance (pid $live)" && [ "$(main_sha origin)" = "$BASE" ] && ok "a live lock holder stops a second instance" || { no "live lock: exit $rc"; echo "$out" | sed 's/^/      /'; }
kill "$live" 2>/dev/null; wait "$live" 2>/dev/null
sh -c 'exit 0' & dead=$!; wait "$dead"
echo "$dead" > "$LAND_STATE_DIR/bot/lock/pid"

echo "=== train: the tick lands every ready branch, in order, on both remotes ==="
out="$(bot tick)"; rc=$?
[ "$rc" = 0 ] && has "$out" "[stale-lock] took over the lock of dead pid $dead" && ok "a dead holder's lock is taken over" || { no "stale lock: exit $rc"; echo "$out" | sed 's/^/      /'; }
[ ! -d "$LAND_STATE_DIR/bot/lock" ] && ok "the lock is released at exit" || no "lock left behind"
all=1; for b in wip/explicit wip/rule wip/a wip/checkonly; do in_main "$(sha_of "$b")" || all=0; done
[ "$all" = 1 ] && ok "all four ready branches are in origin/main" || { no "a ready branch did not land"; echo "$out" | sed 's/^/      /'; }
[ "$(main_sha origin)" = "$(main_sha lambdaclass)" ] && ok "lambdaclass/main equals origin/main" || no "mirror differs"
[ "$(main_sha fork)" = "$BASE" ] && ok "fork untouched" || no "fork was pushed"
for b in wip/nogates wip/findings wip/gatefail wip/stale; do in_main "$(sha_of "$b")" && no "$b landed"; done
merges="$(git --git-dir="$TMP/origin.git" log --first-parent --reverse --format=%s "$BASE..refs/heads/main" | sed -n 's/.*merge refs\/bot\/\([^ ]*\) .*/\1/p' | tr '\n' ' ')"
[ "$merges" = "wip/explicit wip/rule wip/a wip/checkonly " ] && ok "the train merged members in landing order" || no "merge order: '$merges'"
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

echo "=== main is never force-pushed ==="
bad="$(grep -nE 'git push[^#]*(--force|[[:space:]]-f[[:space:]])[^#]*refs/heads/main' "$ROOT_DIR"/scripts/land/*.sh || true)"
[ -z "$bad" ] && ok "no forced push of refs/heads/main in scripts/land" || { no "forced push of main:"; echo "$bad" | sed 's/^/      /'; }

echo
echo "check_landing_bot: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
