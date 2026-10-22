#!/usr/bin/env bash
# THE WORK QUEUE SCHEDULES BY PRIORITY, NEVER OVERLAPS AREAS, HONOURS ITS CAPS AND RELEASES WHAT STALLS.
#
# scripts/queue/ turns the open bugs, the ROADMAP (throughput track first), fuzz-farm findings and
# tasks/*.md into queue.json and starts headless Claude agents on it (docs/project/WORK_QUEUE.md).
# Every leg below drives the real scripts against a THROWAWAY repository (a bare remote, the queue's
# own clone with its push guard) and stub `claude` and `cswap` binaries, so nothing here can reach
# this repository, its remotes, a Claude account or CI.
#
#   sources     open/closed bug records, severity tiers (the ledger's `silent:` line wins), the
#               throughput table before the bug tiers, explicit tasks, the roadmap rows, then loud
#               bugs; dispositions; dependencies from the table and **Depends on:**; cycles marked;
#               fuzz refusals and promoted findings dropped; a malformed task file exits 2
#   areas       a task whose area a running task holds is skipped and a later disjoint one starts
#   caps        the concurrency cap; the agent-hours and USD daily budgets stop new starts
#   rotation    a usage limit releases the task uncharged and restarts it on another slot; a limited
#               slot is not used again within the hour; tool output mentioning a limit is not one
#   lifecycle   heartbeat from wip pushes; clean+pass notes make a task done, unblock its dependents
#               and retire its worktree; no push for the stuck window kills and releases it
#   kill switch the pause file stops new starts; `kill` in it also stops running agents
#   guard       the queue clone refuses pushes to main; a clone without the guard starts nothing
#   install     units are written with KillMode=process and the timer is NOT enabled
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
QD="$ROOT_DIR/scripts/queue"
for f in build_queue.py dispatch.py dispatch.sh run_agent.sh push_guard.sh install-queue.sh; do
  [ -f "$QD/$f" ] || { echo "FATAL: scripts/queue/$f missing" >&2; exit 2; }
done
TMP="$(mktemp -d)" || { echo "FATAL: could not create a sandbox directory" >&2; exit 2; }
[ -n "$TMP" ] && [ -d "$TMP" ] || { echo "FATAL: sandbox directory is not usable" >&2; exit 2; }
cleanup() {
  # Stop any stub agent a failing leg left behind (each runs in its own session).
  python3 - "$TMP" <<'EOF' 2>/dev/null || true
import glob, json, os, signal, sys
for p in glob.glob(os.path.join(sys.argv[1], "*", "home", "state.json")):
    for t in json.load(open(p))["tasks"].values():
        if t.get("pid"):
            try: os.killpg(t["pid"], signal.SIGKILL)
            except OSError: pass
EOF
  rm -rf "$TMP"
}
trap cleanup EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

unset GIT_DIR GIT_WORK_TREE GIT_INDEX_FILE GIT_COMMON_DIR GIT_OBJECT_DIRECTORY
export GIT_CONFIG_GLOBAL="$TMP/gitconfig" GIT_CONFIG_NOSYSTEM=1
cat > "$GIT_CONFIG_GLOBAL" <<EOF
[user]
	name = queue gate
	email = gate@example.invalid
[init]
	defaultBranch = main
[core]
	hooksPath = $TMP/nohooks
EOF
mkdir -p "$TMP/nohooks"
unset QUEUE_MAX_AGENTS QUEUE_MAX_AGENT_HOURS QUEUE_MAX_DAILY_USD QUEUE_MAX_DAILY_TOKENS QUEUE_SLOTS \
  QUEUE_STUCK_MINUTES QUEUE_MAX_ATTEMPTS QUEUE_NOTIFY QUEUE_STATUS_JSON QUEUE_FUZZ_DIR QUEUE_NO_FETCH \
  QUEUE_SOURCE_ROOT QUEUE_TASK_MAX_USD QUEUE_MODEL QUEUE_SLOT_COOLDOWN QUEUE_REQUIRE_GUARD QUEUE_ENV_FILE
T0=1800000000

# ------------------------------------------------------------------ fixture sources
SRC="$TMP/src"
mkdir -p "$SRC/docs/bugs" "$SRC/Concrete/Backend" "$SRC/tasks" "$TMP/fuzz"
for m in Lower Elab Interp Resolve Mono; do echo "-- $m" > "$SRC/Concrete/$m.lean"; done
echo "-- EmitSSA" > "$SRC/Concrete/Backend/EmitSSA.lean"
bug() { # num status severity owner-text [extra]
  printf '# Bug %s — fixture bug %s\n\n**Status:** %s\n**Severity:** %s\n\n%s## Structural cause\n\nclass 13.\n\n## Structural owner\n\n%s\n' \
    "$1" "$1" "$2" "$3" "${5:-}" "$4" > "$SRC/docs/bugs/$1_fixture.md"
}
bug 201 OPEN "silent wrong code." "R-0501 (the Lower pass)."
bug 202 OPEN "silent wrong code in safe code." "R-0502, in Lower."
bug 203 OPEN "compiler crash (panic in Elab)." "R-0492 for Elab."
bug 204 OPEN "loud failure: a valid program is rejected." "R-0499 in Resolve."
bug 205 FIXED "silent wrong code." "R-0501 in Lower."
bug 206 "CONTAINED (2026-10-01)" "loud failure at link time." "R-0510 in Interp." \
  $'<!-- bug-ledger\nsummary: fixture\ncoverage: fixture\nsilent: yes\n-->\n\n'
cat > "$SRC/docs/bugs/README.md" <<'EOF'
Status: reference
EOF
cat > "$SRC/ROADMAP.md" <<'EOF'
# Roadmap

## Development throughput

| order | item | owner | exit criterion | depends on |
|---|---|---|---|---|
| 1 | Measurement: stage timestamps | R-0547 | recorded | — |
| 2 | Landing trains: one CI per train | R-0539 | three trains | 1 |

## The current execution queue

| order | work | exit before advancing |
|---|---|---|
| 0 | **Soundness repairs (R-0500)** | close it |
| 1 | **Place MIR (R-0501, R-0600, R-0601)** | shadow paths |

### Phase 1 exits

Not a row: R-0999.

## Area

### Task R-0539

**Objective:** trains.

### Task R-0500

**Objective:** written rules in `Mono`.

### Task R-0600

**Depends on:** R-0601 (the other half).

### Task R-0601

**Depends on:** R-0600. **Blocks:** nothing.

### Task R-0501

**Depends on:** nothing. **Blocks:** R-0600.
EOF
cat > "$SRC/tasks/R-0501.md" <<'EOF'
---
id: R-0501
disposition: in-flight
branch: wip/place-mir
---
EOF
cat > "$SRC/tasks/docs-sweep.md" <<'EOF'
---
title: Sweep the glossary
areas: docs/GLOSSARY.md
---
Update docs/GLOSSARY.md for the new terms.
EOF
cat > "$TMP/fuzz/f1.json" <<'EOF'
{"id": "f1", "class": "silent", "summary": "miscompile in EmitSSA", "areas": ["mod:EmitSSA"]}
EOF
echo '{"id": "f2", "class": "refusal", "summary": "valid program refused"}' > "$TMP/fuzz/f2.json"
echo '{"id": "f3", "class": "crash", "summary": "panic", "bug": 207}' > "$TMP/fuzz/f3.json"
echo '{"id": "f4", "class": "crash", "summary": "segfault in Mono"}' > "$TMP/fuzz/f4.json"

# ------------------------------------------------------------------ leg: sources and priority
echo "=== sources and priority ==="
if python3 "$QD/build_queue.py" --root "$SRC" --fuzz-dir "$TMP/fuzz" > "$TMP/q.json" 2> "$TMP/q.err"; then
  ok "build_queue.py reads the fixture sources"
else
  no "build_queue.py failed: $(cat "$TMP/q.err")"
fi
order="$(python3 -c 'import json,sys; print(" ".join(t["id"] for t in json.load(open(sys.argv[1]))["tasks"]))' "$TMP/q.json" 2>/dev/null)"
want="R-0547 R-0539 bug-201 bug-202 bug-206 fuzz-f1 bug-203 fuzz-f4 task-docs-sweep R-0500 R-0501 R-0600 R-0601 bug-204"
[ "$order" = "$want" ] && ok "priority: throughput, silent (bugs then fuzz), crash, explicit, roadmap, loud" \
  || no "priority order: got '$order', want '$want'"
qget() { python3 - "$TMP/q.json" "$@" <<'EOF'
import json, sys
q = {t["id"]: t for t in json.load(open(sys.argv[1]))["tasks"]}
t = q.get(sys.argv[2], {})
v = t.get(sys.argv[3], "")
print(" ".join(v) if isinstance(v, list) else v)
EOF
}
[ "$(qget bug-206 tier)" = silent ] && ok "a ledger 'silent: yes' outranks the severity prose" || no "bug 206 tier: $(qget bug-206 tier)"
[ "$(qget bug-203 tier)" = crash ] && ok "a crash severity is the crash tier" || no "bug 203 tier: $(qget bug-203 tier)"
case " $order " in *" bug-205 "*|*" fuzz-f2 "*|*" fuzz-f3 "*|*" R-0999 "*) no "a fixed bug, a refusal, a promoted finding or a non-row R-id was queued" ;;
  *) ok "fixed bugs, refusals, promoted findings and R-ids outside the rows are not tasks" ;; esac
[ "$(qget R-0539 depends)" = R-0547 ] && ok "the throughput table's 'depends on' column" || no "R-0539 depends: $(qget R-0539 depends)"
[ "$(qget R-0501 depends)" = "" ] && ok "'Depends on: nothing. **Blocks:** R-0600' adds no dependency" || no "R-0501 depends: $(qget R-0501 depends)"
[ "$(qget R-0600 cycle)" = True ] && [ "$(qget R-0601 cycle)" = True ] && ok "a dependency cycle is marked" || no "cycle not marked"
[ "$(qget R-0501 disposition)" = in-flight ] && ok "tasks/R-0501.md marks the sourced task in flight" || no "disposition not applied"
[ "$(qget bug-201 areas)" = "mod:Lower rule:R-0501" ] && ok "bug areas come from the structural owner" || no "bug-201 areas: $(qget bug-201 areas)"
[ "$(qget R-0500 areas)" = "mod:Mono rule:R-0500" ] && ok "roadmap areas: the task's rule and the modules its body names" || no "R-0500 areas: $(qget R-0500 areas)"
mkdir -p "$TMP/bad/tasks"; printf -- '---\nid: x\nsize: 3\n---\nbody\n' > "$TMP/bad/tasks/x.md"
rc=0; python3 "$QD/build_queue.py" --root "$TMP/bad" > /dev/null 2>&1 || rc=$?
[ "$rc" = 2 ] && ok "an unknown task-file key exits 2" || no "malformed task file exit $rc (want 2)"

# ------------------------------------------------------------------ the sandbox repository and stubs
git init -q --bare "$TMP/remote.git"
git clone -q "$TMP/remote.git" "$TMP/seed" 2>/dev/null
cp -r "$SRC/." "$TMP/seed/"
git -C "$TMP/seed" add -A && git -C "$TMP/seed" commit -qm fixture && git -C "$TMP/seed" push -q origin main 2>/dev/null
mkdir -p "$TMP/bin"
cat > "$TMP/bin/cswap" <<'EOF'
#!/usr/bin/env bash
# Stub cswap: `cswap run <slot> -- cmd...` runs cmd as that slot.
[ "$1" = run ] && [ "$3" = -- ] || { echo "stub cswap: unexpected $*" >&2; exit 64; }
echo "$2" >> "$STUB/cswap.log"
slot="$2"; shift 3
CSWAP_SLOT="$slot" exec "$@"
EOF
cat > "$TMP/bin/claude" <<'EOF'
#!/usr/bin/env bash
# Stub claude: behaviour per task from $STUB/behave/<task> (else $STUB/behave/default).
printf '%s slot=%s %s\n' "$QUEUE_TASK_ID" "$CSWAP_SLOT" "$*" >> "$STUB/calls.log"
b="$(cat "$STUB/behave/$QUEUE_TASK_ID" 2>/dev/null || cat "$STUB/behave/default")"
push() {
  git commit -q --allow-empty -m "wip $QUEUE_TASK_ID $(date +%s%N)" && git push -q origin "HEAD:refs/heads/$QUEUE_WIP_REF" 2>>"$STUB/push.err"
  touch "$STUB/pushed.$QUEUE_TASK_ID"
}
result() { printf '{"type":"result","is_error":%s,"result":"%s","total_cost_usd":%s,"usage":{"input_tokens":1000,"output_tokens":200,"cache_read_input_tokens":0}}\n' "$1" "$2" "$3"; }
case "$b" in
  limit-on:*)
    if [ "$CSWAP_SLOT" = "${b#limit-on:}" ]; then result true "Claude AI usage limit reached|1800003600" 0; exit 1; fi
    push; result false done 0.5 ;;
  noisy-fail)
    echo '{"type":"user","message":"tool output: usage limit reached in the docs"}'; result true "build failed" 0.1; exit 1 ;;
  hang) push; sleep 600 ;;
  silent) sleep 600 ;;
  ready)
    push
    git notes --ref=refs/notes/concrete-review add -f -m "verdict: clean" HEAD
    git notes --ref=refs/notes/concrete-gates add -f -m "gates: pass" HEAD
    git push -q origin refs/notes/concrete-review refs/notes/concrete-gates 2>>"$STUB/push.err"
    result false done 0.5 ;;
  cost:*) push; result false done "${b#cost:}" ;;
  *) push; result false done 0.5 ;;
esac
EOF
chmod +x "$TMP/bin/cswap" "$TMP/bin/claude"

# scenario <name>: a fresh queue clone (installed by install-queue.sh), state dir and stub dir.
scenario() {
  S="$TMP/$1"; mkdir -p "$S/stub/behave" "$S/xdg"
  echo push > "$S/stub/behave/default"
  HOME="$S" XDG_CONFIG_HOME="$S/xdg/config" XDG_DATA_HOME="$S/xdg/data" QUEUE_REPO="$S/repo" \
    QUEUE_NO_SYSTEMCTL=1 bash "$QD/install-queue.sh" "$TMP/remote.git" > "$S/install.log" 2>&1 \
    || no "$1: install-queue.sh failed: $(tail -3 "$S/install.log")"
  export STUB="$S/stub" QUEUE_HOME="$S/home" QUEUE_REPO="$S/repo" QUEUE_PAUSE_FILE="$S/paused" \
    QUEUE_CLAUDE="$TMP/bin/claude" QUEUE_CSWAP="$TMP/bin/cswap" QUEUE_ENV_FILE="$S/none" \
    QUEUE_FUZZ_DIR="" QUEUE_MAX_AGENTS=3 QUEUE_SLOTS="1 2 3 4" QUEUE_MAX_ATTEMPTS=2
  unset QUEUE_MAX_AGENT_HOURS QUEUE_MAX_DAILY_USD QUEUE_STUCK_MINUTES QUEUE_REQUIRE_GUARD
}
tick() { # tick <epoch> — one dispatcher pass at a pinned clock
  QUEUE_NOW="$1" bash "$QD/dispatch.sh" tick > "$S/tick.out" 2> "$S/tick.err" || no "tick at $1 failed: $(tail -3 "$S/tick.err")"
}
st() { python3 - "$QUEUE_HOME/state.json" "$@" <<'EOF'
import json, sys
s = json.load(open(sys.argv[1]))
t = s["tasks"].get(sys.argv[2], {})
print(t.get(sys.argv[3], ""))
EOF
}
running_ids() { python3 -c 'import json,sys; s=json.load(open(sys.argv[1])); print(" ".join(sorted(k for k,v in s["tasks"].items() if v.get("status")==sys.argv[2])))' "$QUEUE_HOME/state.json" "${1:-running}"; }
events() { grep -c "\"event\": \"$1\"" "$QUEUE_HOME/events.log" 2>/dev/null || true; }
wait_exit() { # wait_exit <task> — until its current run wrote its exit file
  local d i; d="$(st "$1" run_dir)"
  for i in $(seq 1 150); do [ -f "$d/exit" ] && return 0; sleep 0.2; done
  no "timed out waiting for $1's agent to exit"; return 1
}
wait_pushed() { local i; for i in $(seq 1 150); do [ -f "$STUB/pushed.$1" ] && return 0; sleep 0.2; done; no "timed out waiting for $1's wip push"; return 1; }
dead() { python3 -c 'import os,sys
try: os.kill(int(sys.argv[1]),0)
except OSError: sys.exit(0)
sys.exit(1)' "$1"; }

# ------------------------------------------------------------------ leg: guard and install
echo "=== push guard and install ==="
scenario guard
[ "$(git -C "$QUEUE_REPO" config core.hooksPath)" = "$S/xdg/data/concrete-queue/hooks" ] && ok "install points the clone's hooks at the guard" || no "hooksPath: $(git -C "$QUEUE_REPO" config core.hooksPath)"
grep -q '^KillMode=process' "$S/xdg/config/systemd/user/concrete-queue.service" && ok "the service keeps detached agents alive (KillMode=process)" || no "KillMode=process missing"
[ -f "$S/xdg/config/systemd/user/concrete-queue.timer" ] && [ ! -e "$S/xdg/config/systemd/user/timers.target.wants/concrete-queue.timer" ] \
  && ok "the timer is written and NOT enabled" || no "timer missing or enabled"
grep -q 'systemctl --user enable --now concrete-queue.timer' "$S/install.log" && ok "install prints the enable command" || no "no enable command printed"
main0="$(git -C "$TMP/remote.git" rev-parse main)"
git -C "$QUEUE_REPO" commit -q --allow-empty -m "try main"
if git -C "$QUEUE_REPO" push -q origin HEAD:refs/heads/main 2>/dev/null; then no "the guard let a push to main through"; else ok "the guard refuses a push to main"; fi
git -C "$QUEUE_REPO" push -q origin HEAD:refs/heads/ci/x 2>/dev/null && no "the guard let a push to ci/* through" || ok "the guard refuses a push to ci/*"
git -C "$QUEUE_REPO" push -q origin HEAD:refs/heads/wip/queue/try 2>/dev/null && ok "the guard allows wip/*" || no "the guard refused wip/*"
[ "$(git -C "$TMP/remote.git" rev-parse main)" = "$main0" ] && ok "remote main unchanged" || no "remote main moved"
git -C "$QUEUE_REPO" config --unset core.hooksPath
tick "$T0"
[ -z "$(running_ids)" ] && [ "$(events push_guard_missing)" = 1 ] && ok "no guard, no agents (and one event)" || no "started without the guard: $(running_ids)"

# ------------------------------------------------------------------ leg: areas and concurrency
echo "=== area conflicts and the concurrency cap ==="
scenario areas
echo hang > "$STUB/behave/default"
QUEUE_MAX_AGENTS=5 tick "$T0"
r="$(running_ids)"
[ "$r" = "R-0547 bug-201 bug-203 bug-206 task-docs-sweep" ] && ok "bug-202 (shares Lower with bug-201) skipped, disjoint later tasks started" \
  || no "started: '$r'"
for id in $r; do wait_pushed "$id"; done   # the agents start asynchronously
grep -q -- "-p /fix-bug 201 --permission-mode dontAsk --settings" "$STUB/calls.log" && ok "the agent gets its playbook command, dontAsk and the settings file" || no "claude call: $(grep bug-201 "$STUB/calls.log")"
grep -q 'Bash(git push \* main)' "$(st bug-201 run_dir)/settings.json" && ok "the settings deny pushes to main" || no "settings.json lacks the main-push deny"
[ "$(sort -u "$STUB/cswap.log" | tr '\n' ' ')" = "1 2 3 4 " ] && ok "agents are spread across the four slots" || no "slots used: $(tr '\n' ' ' < "$STUB/cswap.log")"
QUEUE_MAX_AGENTS=5 tick "$T0"
[ "$(running_ids)" = "$r" ] && ok "a second tick starts nothing new (areas still held)" || no "second tick changed the running set: $(running_ids)"
echo kill > "$QUEUE_PAUSE_FILE"; tick "$T0"; rm -f "$QUEUE_PAUSE_FILE"

scenario cap
echo hang > "$STUB/behave/default"
QUEUE_MAX_AGENTS=2 tick "$T0"
[ "$(running_ids)" = "R-0547 bug-201" ] && ok "the concurrency cap (2) holds on the first tick" || no "cap: '$(running_ids)'"
QUEUE_MAX_AGENTS=2 tick "$((T0+60))"
[ "$(running_ids | wc -w | tr -d ' ')" = 2 ] && ok "and on the next" || no "cap broken: '$(running_ids)'"

# ------------------------------------------------------------------ leg: kill switch (continues 'cap')
echo "=== kill switch ==="
pid="$(st R-0547 pid)"
: > "$QUEUE_PAUSE_FILE"
QUEUE_MAX_AGENTS=4 tick "$((T0+120))"
[ "$(running_ids | wc -w | tr -d ' ')" = 2 ] && [ "$(events paused)" = 1 ] && ok "paused: running agents continue, nothing new starts" || no "pause: '$(running_ids)'"
echo kill > "$QUEUE_PAUSE_FILE"
QUEUE_MAX_AGENTS=4 tick "$((T0+180))"
sleep 0.5
[ -z "$(running_ids)" ] && dead "$pid" && [ "$(st R-0547 status)" = pending ] && [ "$(st R-0547 attempts)" = 0 ] \
  && ok "kill: agents stopped, tasks released uncharged" || no "kill: running='$(running_ids)' status=$(st R-0547 status)"
rm -f "$QUEUE_PAUSE_FILE"
QUEUE_MAX_AGENTS=1 tick "$((T0+240))"
[ "$(running_ids)" = R-0547 ] && [ "$(events paused_cleared)" = 1 ] && ok "removing the switch resumes dispatch" || no "resume: '$(running_ids)'"
echo kill > "$QUEUE_PAUSE_FILE"; tick "$((T0+300))"; rm -f "$QUEUE_PAUSE_FILE"

# ------------------------------------------------------------------ leg: budgets
echo "=== budgets ==="
scenario hours
echo hang > "$STUB/behave/default"
export QUEUE_STUCK_MINUTES=1000
QUEUE_MAX_AGENTS=1 QUEUE_MAX_AGENT_HOURS=1 tick "$T0"
QUEUE_MAX_AGENTS=2 QUEUE_MAX_AGENT_HOURS=1 tick "$((T0+5400))"
[ "$(running_ids)" = R-0547 ] && [ "$(events budget_exhausted)" = 1 ] && ok "1.5 agent-hours used of 1: no new start" || no "hours: '$(running_ids)' events=$(events budget_exhausted)"
python3 -c 'import json,sys; s=json.load(open(sys.argv[1])); b=s["budget"]; sys.exit(0 if b["agent_hours"]==1.5 and b["max_agent_hours"]==1 else 1)' "$QUEUE_HOME/status.json" \
  && ok "status.json reports the budget used" || no "status.json budget: $(cat "$QUEUE_HOME/status.json")"
echo kill > "$QUEUE_PAUSE_FILE"; tick "$((T0+5500))"; rm -f "$QUEUE_PAUSE_FILE"
unset QUEUE_STUCK_MINUTES

scenario usd
echo cost:2.5 > "$STUB/behave/default"
QUEUE_MAX_AGENTS=1 QUEUE_MAX_DAILY_USD=2 tick "$T0"
wait_exit R-0547
QUEUE_MAX_AGENTS=1 QUEUE_MAX_DAILY_USD=2 tick "$((T0+60))"
[ "$(st R-0547 status)" = exited ] && [ -z "$(running_ids)" ] && [ "$(events budget_exhausted)" = 1 ] \
  && ok "\$2.50 spent of \$2: no new start" || no "usd: status=$(st R-0547 status) running='$(running_ids)'"
grep -q '"usd": 2.5' "$QUEUE_HOME/status.json" && grep -q '"tokens": 1200' "$QUEUE_HOME/status.json" \
  && ok "cost and tokens are read from the run's result event" || no "usd/tokens not recorded"

# ------------------------------------------------------------------ leg: usage-limit rotation
echo "=== usage-limit rotation ==="
scenario rotate
echo limit-on:1 > "$STUB/behave/R-0547"
QUEUE_MAX_AGENTS=1 tick "$T0"
[ "$(st R-0547 slot)" = 1 ] && ok "first run on slot 1" || no "first slot $(st R-0547 slot)"
wait_exit R-0547
QUEUE_MAX_AGENTS=1 tick "$((T0+60))"
[ "$(events usage_limit)" = 1 ] && [ "$(st R-0547 slot)" = 2 ] && [ "$(st R-0547 attempts)" = 0 ] \
  && ok "a usage limit restarts the task on slot 2, uncharged" || no "rotation: slot=$(st R-0547 slot) attempts=$(st R-0547 attempts) limits=$(events usage_limit)"
grep -q '"cooling_until"' "$QUEUE_HOME/status.json" && ok "status.json shows slot 1 cooling" || no "no cooling slot in status.json"

scenario oneslot
echo limit-on:1 > "$STUB/behave/R-0547"
export QUEUE_SLOTS=1
QUEUE_MAX_AGENTS=1 tick "$T0"; wait_exit R-0547
QUEUE_MAX_AGENTS=1 tick "$((T0+60))"
[ -z "$(running_ids)" ] && [ "$(events no_slot_available)" = 1 ] && ok "the only slot is limited: nothing starts" || no "oneslot: '$(running_ids)'"
QUEUE_MAX_AGENTS=1 tick "$((T0+1800))"
[ -z "$(running_ids)" ] && ok "still nothing after 30 minutes" || no "slot reused within the hour"
QUEUE_MAX_AGENTS=1 tick "$((T0+3700))"
[ "$(running_ids)" = R-0547 ] && ok "the slot is retried after an hour" || no "not retried after the hour: '$(running_ids)'"
wait_exit R-0547
unset QUEUE_SLOTS

scenario noisy
echo noisy-fail > "$STUB/behave/R-0547"
QUEUE_MAX_AGENTS=1 tick "$T0"; wait_exit R-0547
QUEUE_MAX_AGENTS=0 tick "$((T0+60))"
[ "$(events usage_limit)" = 0 ] && [ "$(st R-0547 attempts)" = 1 ] && [ "$(events released)" = 1 ] \
  && ok "limit text inside tool output is an ordinary failure (charged, released)" || no "noisy: limits=$(events usage_limit) attempts=$(st R-0547 attempts)"

# ------------------------------------------------------------------ leg: lifecycle
echo "=== heartbeat, done, stuck ==="
scenario done
echo ready > "$STUB/behave/R-0547"
QUEUE_MAX_AGENTS=1 tick "$T0"
wt="$(st R-0547 worktree)"
wait_exit R-0547
QUEUE_MAX_AGENTS=1 tick "$((T0+60))"
[ "$(st R-0547 status)" = done ] && [ "$(events heartbeat)" -ge 1 ] && ok "clean review + passing gates notes on the wip head: done" || no "done: status=$(st R-0547 status)"
[ "$(running_ids)" = R-0539 ] && ok "its dependent (R-0539) starts once it is done" || no "dependent: '$(running_ids)'"
QUEUE_MAX_AGENTS=1 tick "$((T0+120))"
[ ! -d "$wt" ] && [ "$(events worktree_removed)" = 1 ] && ok "a done task's published worktree is removed" || no "worktree $wt still present"
[ "$(git -C "$TMP/remote.git" rev-parse main)" = "$main0" ] && ok "main untouched by the whole lifecycle" || no "main moved"

scenario stuck
echo hang > "$STUB/behave/default"
export QUEUE_MAX_ATTEMPTS=1
QUEUE_MAX_AGENTS=1 tick "$T0"
pid="$(st R-0547 pid)"; wait_pushed R-0547
QUEUE_MAX_AGENTS=1 tick "$((T0+600))"
[ "$(st R-0547 last_push)" = "$((T0+600))" ] || [ "$(st R-0547 last_push)" = "$((T0+600)).0" ] && ok "a wip push is a heartbeat" || no "last_push=$(st R-0547 last_push)"
QUEUE_MAX_AGENTS=1 tick "$((T0+600+3500))"
[ "$(st R-0547 status)" = running ] && ok "58 minutes without a push: still running" || no "released early: $(st R-0547 status)"
QUEUE_MAX_AGENTS=1 tick "$((T0+600+3700))"
sleep 0.5
[ "$(events stuck)" = 1 ] && [ "$(st R-0547 status)" = abandoned ] && dead "$pid" \
  && ok "61 minutes without a push: killed, released, abandoned at the attempt cap" || no "stuck: status=$(st R-0547 status) events=$(events stuck)"
unset QUEUE_MAX_ATTEMPTS

echo ""
echo "check_work_queue: $PASS passed, $FAIL failed"
[ "$FAIL" -eq 0 ] || exit 1
