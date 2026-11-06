#!/usr/bin/env bash
# THE STATUS PAGE TELLS THE TRUTH, PUBLISHES NO SECRET, AND NOTIFIES ONCE (docs/project/STATUS_PAGE.md).
#
# scripts/status/status.py replaces hand-written status reports, so a wrong page is a wrong report:
# this gate drives it against a THROWAWAY repository (bare-repo origin and mirror, review/gate notes,
# bug records, a landing log) and a fake `gh`, and checks what the page says. Nothing here touches
# this repository, its remotes, GitHub or the network.
#
#   generation  main mismatch flagged; in-flight ci/* run with its failing job; branch ahead count
#               and review note; landing medians against the targets; open bugs; runners offline
#   fail-soft   with gh failing outright, every section says "unavailable" and the page still renders
#   sanitizer   the public variant carries no IP, token, webhook, e-mail, home path or private host
#               that the full variant does carry, and keeps ordinary timestamps intact
#   html        doctype, viewport, meta refresh, dark mode, no external asset, balanced tags
#   notify      usage errors; stdout fallback; dry-run redacts the topic and sends nothing; a short
#               topic is refused; duplicates inside the window are dropped, outside it are sent; a
#               failed delivery falls back to stdout and is not recorded; the env file is not sourced
#   install     writes localhost-only units and enables nothing without --enable
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
STATUS="$ROOT_DIR/scripts/status/status.py"
NOTIFY="$ROOT_DIR/scripts/land/notify.sh"
INSTALL="$ROOT_DIR/scripts/status/install.sh"
for f in "$STATUS" "$NOTIFY" "$INSTALL"; do [ -f "$f" ] || { echo "FATAL: $f missing" >&2; exit 2; }; done
TMP="$(mktemp -d)" || { echo "FATAL: could not create a sandbox directory" >&2; exit 2; }
[ -n "$TMP" ] && [ -d "$TMP" ] || { echo "FATAL: sandbox directory is not usable" >&2; exit 2; }
trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
check(){ if eval "$2"; then ok "$1"; else no "$1"; fi; }

unset GIT_DIR GIT_WORK_TREE GIT_INDEX_FILE GIT_COMMON_DIR GIT_OBJECT_DIRECTORY
export GIT_CONFIG_GLOBAL="$TMP/gitconfig" GIT_CONFIG_NOSYSTEM=1
printf '[user]\n\tname = status gate\n\temail = gate@example.invalid\n[init]\n\tdefaultBranch = main\n[core]\n\thooksPath = %s/nohooks\n' "$TMP" > "$GIT_CONFIG_GLOBAL"
mkdir -p "$TMP/nohooks"

# Secrets planted in the fixture. The full page must carry them (so their absence from the public
# page is the sanitizer's doing), the public page must not.
SECRET_IP="62.210.216.169"
SECRET_IP6="2001:bc8:701:54:d604:e6ff:feda:35d0"
SECRET_TOKEN="ghp_ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
SECRET_HOOK="https://hooks.slack.com/services/T000/B000/XXXXXXXXXXXXXXXX"
SECRET_MAIL="someone@private.example.org"
SECRET_HOME="/home/dev/runs/r1"
SECRET_HOST="concrete-ci-1-gha1"

echo "=== fixture repository ==="
git init -q --bare "$TMP/origin.git"; git init -q --bare "$TMP/mirror.git"
git init -q "$TMP/repo" && cd "$TMP/repo" || exit 2
mkdir -p docs/bugs
printf '# Bug 001: open thing\n\n**Status:** OPEN\n' > docs/bugs/001_open_thing.md
printf '# Bug 002: fixed thing\n\n**Status:** Fixed (2026-01-01)\n' > docs/bugs/002_fixed_thing.md
printf '# Bug 003: contained thing\n\nStatus: CONTAINED by a gate\n' > docs/bugs/003_contained.md
git add -A && git commit -qm base
git remote add origin "$TMP/origin.git"; git remote add lambdaclass "$TMP/mirror.git"
git push -q origin main; git push -q lambdaclass main
git checkout -qb wip/a; echo a > a.txt; git add a.txt; git commit -qm "wip a: one commit ahead"
git notes --ref=concrete-review add -m "codex: approve. reviewer at $SECRET_IP mailed $SECRET_MAIL token=$SECRET_TOKEN" HEAD
git notes --ref=concrete-gates add -m "fast surface: passed (log $SECRET_HOME)" HEAD
git push -q origin wip/a
git checkout -q main; git checkout -qb ready/b; echo b > b.txt; git add b.txt; git commit -qm "ready b"; git push -q origin ready/b
git checkout -q main; echo m > m.txt; git add m.txt; git commit -qm "main moves"; git push -q origin main   # mirror lags: mismatch
git fetch -q origin
cd "$ROOT_DIR" || exit 2
MAIN_SHA="$(git -C "$TMP/repo" rev-parse origin/main)"; WIPA="$(git -C "$TMP/repo" rev-parse origin/wip/a)"

mkdir -p "$TMP/log"
{ printf 'utc\ttrain\tstage\tbase\thead\tmembers\trun_id\tresult\tduration_s\tnote\n'
  printf '2026-10-22T10:00:00Z\tt1\tbuild\tb\th\twip/a@1,wip/b@2\t-\tpassed\t30\t2 member(s)\n'
  printf '2026-10-22T10:05:00Z\tt1\tci\tb\th\twip/a@1,wip/b@2\t9\tdispatched\t10\tci/t1\n'
  printf '2026-10-22T10:35:00Z\tt1\twatch\tb\th\twip/a@1,wip/b@2\t9\tpassed\t1800\trun conclusion success\n'
  printf '2026-10-22T11:00:00Z\tt1\tland\tb\th\twip/a@1,wip/b@2\t9\tpassed\t20\torigin + lambdaclass via %s\n' "$SECRET_HOST"
} > "$TMP/log/t1.tsv"
{ printf 'utc\ttrain\tstage\tbase\thead\tmembers\trun_id\tresult\tduration_s\tnote\n'
  printf '2026-10-22T12:00:00Z\tt2\tbuild\tb\th\twip/c@3\t-\tpassed\t30\t1 member(s)\n'
  printf '2026-10-22T12:01:00Z\tt2\tci\tb\th\twip/c@3\t10\tdispatched\t10\tci/t2\n'
  printf '2026-10-22T13:00:00Z\tt2\twatch\tb\th\twip/c@3\t10\tfailed\t3540\trun conclusion failure\n'
} > "$TMP/log/t2.tsv"

mkdir -p "$TMP/gh"
cat > "$TMP/fake-gh" <<GH
#!/usr/bin/env bash
case "\$*" in
  *"-R test/mirror --branch main"*) echo '[{"databaseId":2,"status":"completed","conclusion":"failure","headSha":"$MAIN_SHA","createdAt":"2026-10-22T09:00:00Z","updatedAt":"2026-10-22T09:30:00Z","url":"https://github.com/test/mirror/actions/runs/2","workflowName":"CI","event":"push"}]' ;;
  *"--branch main"*) echo '[{"databaseId":1,"status":"completed","conclusion":"success","headSha":"$MAIN_SHA","createdAt":"2026-10-22T09:00:00Z","updatedAt":"2026-10-22T09:30:00Z","url":"https://github.com/test/repo/actions/runs/1","workflowName":"CI","event":"push"}]' ;;
  "run list"*) echo '[{"databaseId":7,"status":"in_progress","conclusion":"","headSha":"abc","headBranch":"ci/t3","createdAt":"2026-10-22T13:00:00Z","updatedAt":"2026-10-22T13:10:00Z","url":"https://github.com/test/repo/actions/runs/7","workflowName":"CI","event":"workflow_dispatch","displayTitle":"train t3"},{"databaseId":8,"status":"completed","conclusion":"success","headSha":"$WIPA","headBranch":"wip/a","createdAt":"2026-10-22T12:00:00Z","updatedAt":"2026-10-22T12:02:00Z","url":"https://github.com/test/repo/actions/runs/8","workflowName":"Branch health","event":"push","displayTitle":"wip a"}]' ;;
  "run view"*) echo '{"jobs":[{"name":"Build","status":"completed","conclusion":"success"},{"name":"Unit tests","status":"completed","conclusion":"failure"},{"name":"Gates","status":"in_progress","conclusion":""}]}' ;;
  "api repos/"*"/actions/runners") echo '{"total_count":2,"runners":[{"name":"$SECRET_HOST","status":"online","busy":true},{"name":"concrete-ci-1-gha2","status":"offline","busy":false}]}' ;;
  *) echo "fake gh: unexpected \$*" >&2; exit 1 ;;
esac
GH
chmod +x "$TMP/fake-gh"

run_status() { # extra env as NAME=VALUE args, then -- status.py args
  local envs=(); while [ "$1" != "--" ]; do envs+=("$1"); shift; done; shift
  env STATUS_ROOT="$TMP/repo" STATUS_GH="$TMP/fake-gh" STATUS_REPO=test/repo STATUS_MIRROR_REPO=test/mirror \
      LAND_LOG_DIR="$TMP/log" STATUS_NOW=1792675200 STATUS_LOAD_CMD="echo 1.50 2.00 3.25 1/100 999" \
      "${envs[@]}" python3 -I "$STATUS" "$@"
}

echo "=== generation from fixture data ==="
if run_status -- --out "$TMP/full" --public-out "$TMP/public" > "$TMP/gen.log" 2>&1; then ok "status.py exits 0"; else no "status.py exits 0"; cat "$TMP/gen.log"; fi
J="$TMP/full/status.json"
q() { python3 -I -c "import json,sys; d=json.load(open(sys.argv[1])); print($2)" "$1"; }
check "status.json parses"                         '[ "$(q "$J" "type(d).__name__")" = dict ]'
check "main mismatch between origin and mirror is flagged" '[ "$(q "$J" "d[\"main\"][\"in_sync\"]")" = False ] && q "$J" "d[\"main\"][\"flags\"]" | has_match MISMATCH'
check "a red mirror CI is flagged too"             'q "$J" "d[\"main\"][\"flags\"]" | has_match "mirror main CI failure"'
check "main CI result is read for origin"          '[ "$(q "$J" "d[\"main\"][\"remotes\"][0][\"ci\"][\"conclusion\"]")" = success ]'
check "one ci/* run in flight"                     '[ "$(q "$J" "d[\"inflight\"][\"active\"]")" = 1 ]'
check "its failing job is named"                   '[ "$(q "$J" "\",\".join(d[\"inflight\"][\"runs\"][0][\"failing\"])")" = "Unit tests" ]'
check "elapsed time comes from STATUS_NOW"         '[ "$(q "$J" "d[\"inflight\"][\"runs\"][0][\"elapsed_s\"]")" -gt 0 ]'
check "Branch health runs are not reported as ci/* runs" '[ "$(q "$J" "len(d[\"inflight\"][\"runs\"])")" = 1 ]'
check "wip/a is one ahead and one behind main"     '[ "$(q "$J" "[(b[\"ahead\"],b[\"behind\"]) for b in d[\"branches\"][\"branches\"] if b[\"name\"]==\"wip/a\"][0]")" = "(1, 1)" ]'
check "wip/a carries its review and gate notes"    'q "$J" "[b for b in d[\"branches\"][\"branches\"] if b[\"name\"]==\"wip/a\"][0]" | has_match "codex: approve" && q "$J" "[b[\"gates\"] for b in d[\"branches\"][\"branches\"] if b[\"name\"]==\"wip/a\"][0]" | has_match "fast surface"'
check "wip/a shows its branch-health run at tip"   '[ "$(q "$J" "[b[\"health\"][\"at_tip\"] for b in d[\"branches\"][\"branches\"] if b[\"name\"]==\"wip/a\"][0]")" = True ]'
check "ready/b is listed"                          'q "$J" "[b[\"name\"] for b in d[\"branches\"][\"branches\"]]" | has_match ready/b'
check "landing: two trains, one landed"            '[ "$(q "$J" "(d[\"landing\"][\"trains_logged\"], d[\"landing\"][\"landed\"])")" = "(2, 1)" ]'
check "landing: time to land 1h00m, on target"     '[ "$(q "$J" "(d[\"landing\"][\"median_time_to_land_s\"], d[\"landing\"][\"land_on_target\"])")" = "(3600, True)" ]'
check "landing: CI median 44m30s, over the 40m target" '[ "$(q "$J" "(d[\"landing\"][\"median_ci_wall_s\"], d[\"landing\"][\"ci_on_target\"])")" = "(2670.0, False)" ]'
check "landing: newest train first"                '[ "$(q "$J" "d[\"landing\"][\"trains\"][0][\"train\"]")" = t2 ]'
check "bugs: OPEN and CONTAINED are open, Fixed is not" '[ "$(q "$J" "\",\".join(sorted(b[\"bug\"] for b in d[\"bugs\"][\"open\"]))")" = "001,003" ]'
check "server: load from STATUS_LOAD_CMD"          '[ "$(q "$J" "d[\"server\"][\"load\"]")" = "[1.5, 2.0, 3.25]" ]'
check "server: one of two runners online"          '[ "$(q "$J" "(d[\"server\"][\"runners_online\"], d[\"server\"][\"runners_total\"])")" = "(1, 2)" ]'
check "html headline asks for attention"           'grep -q "attention" "$TMP/full/status.html" && grep -q "runner(s) offline" "$TMP/full/status.html"'

echo "=== fail-soft: gh unavailable ==="
if run_status STATUS_GH=false -- --out "$TMP/nogh" > "$TMP/nogh.log" 2>&1; then ok "status.py still exits 0 without gh"; else no "status.py still exits 0 without gh"; cat "$TMP/nogh.log"; fi
check "the page says what is unavailable"          'grep -q "unavailable" "$TMP/nogh/status.html"'
check "git-only sections still render"             'q "$TMP/nogh/status.json" "d[\"branches\"][\"branches\"][0][\"name\"]" | has_match /'

echo "=== sanitizer: the public variant ==="
for s in "$SECRET_IP" "$SECRET_TOKEN" "$SECRET_MAIL" "$SECRET_HOME" "$SECRET_HOST"; do
  check "full page carries the planted ${s:0:12}..." 'grep -qF -- "$s" "$TMP/full/status.json"'
  check "public page strips ${s:0:12}..."           '! grep -qF -- "$s" "$TMP/public/status.json" && ! grep -qF -- "$s" "$TMP/public/status.html"'
done
check "public page names no private host at all"   '! grep -q "concrete-ci-" "$TMP/public/status.json"'
check "public page keeps ISO timestamps"           'grep -q "2026-10-22T13:00:00Z" "$TMP/public/status.json"'
check "public page is marked as public"            'grep -q "public view" "$TMP/public/status.html"'
SAN="$(env STATUS_PRIVATE_HOSTS=build-box.internal python3 -I -c '
import importlib.util,sys
spec=importlib.util.spec_from_file_location("s",sys.argv[1]); m=importlib.util.module_from_spec(spec); spec.loader.exec_module(m)
print(m.sanitize_text(sys.argv[2]))' "$STATUS" "ip $SECRET_IP6 hook $SECRET_HOOK xoxb-1234567890-abcdef pw: hunter2 on build-box.internal at 15:41:15 v2.338.1 ntfy.sh/concrete-secret-topic NTFY_TOKEN=opaquecred1 AWS_SECRET_ACCESS_KEY=opaquecred2 {\"password\": \"opaquecred3\"} password=\"two opaquecred4 words\" {\"secret\": \"pre\\\"opaquecred5\"} password=\"pre\\
opaquecred6\" tokenizer: keeps this")"
for s in "$SECRET_IP6" "$SECRET_HOOK" "xoxb-" "hunter2" "build-box.internal" "concrete-secret-topic" opaquecred1 opaquecred2 opaquecred3 opaquecred4 opaquecred5 opaquecred6; do
  check "sanitize_text strips ${s:0:14}..." '! printf "%s" "$SAN" | has_match -F -- "$s"'
done
check "sanitize_text keeps clock times and versions" 'printf "%s" "$SAN" | has_match "15:41:15 v2.338.1"'
check "sanitize_text keeps a word that merely contains a key name" 'printf "%s" "$SAN" | has_match "tokenizer: keeps this"'
# Free-form text is withheld, not scrubbed: no pattern list can vouch for arbitrary prose.
check "public page withholds note text but keeps its presence" '! grep -q "codex: approve" "$TMP/public/status.json" && ! grep -q "fast surface" "$TMP/public/status.json" && grep -q "present (withheld" "$TMP/public/status.json"'
check "public page withholds landing notes"        '! grep -q "run conclusion" "$TMP/public/status.json"'

echo "=== html validity basics ==="
for page in "$TMP/full/status.html" "$TMP/public/status.html" "$TMP/nogh/status.html"; do
  n="${page#"$TMP"/}"
  check "$n: doctype, charset, viewport"           'head -c 300 "$page" | has_match -i "<!doctype html>" && grep -q "charset=\"utf-8\"" "$page" && grep -q "name=\"viewport\"" "$page"'
  check "$n: meta refresh"                         'grep -q "http-equiv=\"refresh\" content=\"120\"" "$page"'
  check "$n: dark mode"                            'grep -q "prefers-color-scheme:dark" "$page"'
  check "$n: no external asset"                    '! grep -qiE "<(script|img|iframe)[^>]* src=|<link |@import|url\(" "$page"'
  check "$n: tags balance"                         'python3 -I -c "
import sys
from html.parser import HTMLParser
VOID={\"meta\",\"br\",\"img\",\"hr\",\"input\",\"link\"}
class P(HTMLParser):
    def __init__(s): super().__init__(); s.st=[]; s.bad=0
    def handle_starttag(s,t,a):
        if t not in VOID: s.st.append(t)
    def handle_endtag(s,t):
        if t in VOID: return
        if not s.st or s.st.pop()!=t: s.bad+=1
p=P(); p.feed(open(sys.argv[1]).read()); sys.exit(1 if p.bad or p.st else 0)" "$page"'
done

echo "=== notify.sh ==="
export CONCRETE_NOTIFY_ENV="$TMP/notify.env" NOTIFY_STATE_DIR="$TMP/nstate" NOTIFY_CURL="$TMP/fake-curl" NOTIFY_NOW=1000
# The fake curl records one line per call: its argv, the -K config it was handed (and that file's
# mode), and the body read from stdin.
cat > "$TMP/fake-curl" <<'C'
#!/usr/bin/env bash
cfg=""; prev=""; for a in "$@"; do [ "$prev" = -K ] && cfg="$a"; prev="$a"; done
mode="$(stat -c %a "$cfg" 2>/dev/null || stat -f %Lp "$cfg" 2>/dev/null)"
printf 'ARGV %s\n' "$*" >> "$FAKE_CURL_ARGV"
printf 'CALL mode=%s cfg=%s body=%s\n' "$mode" "$(tr '\n' ' ' < "$cfg")" "$(cat | tr '\n' ' ')" >> "$FAKE_CURL_LOG"
exit "${FAKE_CURL_RC:-0}"
C
chmod +x "$TMP/fake-curl"; export FAKE_CURL_LOG="$TMP/curl.log" FAKE_CURL_ARGV="$TMP/curl.argv"; : > "$FAKE_CURL_LOG"; : > "$FAKE_CURL_ARGV"
calls() { wc -l < "$FAKE_CURL_LOG" | tr -d " "; }
bash "$NOTIFY" info onlytwo > /dev/null 2>&1; rc=$?
check "missing argument is a usage error (exit 2)" '[ "$rc" = 2 ]'
bash "$NOTIFY" loud t b > /dev/null 2>&1; rc=$?
check "unknown level is a usage error (exit 2)"    '[ "$rc" = 2 ]'
out="$(bash "$NOTIFY" info "no channel" "body one" 2>/dev/null)"; rc=$?
check "no env file: stdout fallback, exit 0"       '[ "$rc" = 0 ] && [ "$out" = "[info] no channel: body one" ]'
TOPIC="concrete-$(printf 'x%.0s' $(seq 1 30))"
printf 'NTFY_TOPIC=%s\nNTFY_TOKEN=tk_secret_token_123\nSLACK_WEBHOOK_URL=%s\nNOTIFY_DEDUP_SECONDS=600\nPWN=$(touch %s/pwned)\n$(touch %s/pwned2)\n' \
  "$TOPIC" "$SECRET_HOOK" "$TMP" "$TMP" > "$TMP/notify.env"; chmod 600 "$TMP/notify.env"
# The dry run uses EXACTLY the arguments of the real send below: if a dry run recorded a dedup stamp,
# that send would be suppressed and the next check would fail.
out="$(bash "$NOTIFY" --dry-run error "land t1" "train t1 is red" 2>/dev/null)"
check "dry-run shows the ntfy and slack messages"  'printf "%s" "$out" | has_match "dry-run ntfy: POST https://ntfy.sh/<topic> priority=urgent" && printf "%s" "$out" | has_match "dry-run slack: POST <webhook>"'
check "dry-run redacts the topic and webhook"      '! printf "%s" "$out" | has_match -F -- "$TOPIC" && ! printf "%s" "$out" | has_match -F hooks.slack.com'
check "dry-run sends nothing"                      '[ "$(calls)" = 0 ]'
check "the env file is parsed, never sourced"      '[ ! -e "$TMP/pwned" ] && [ ! -e "$TMP/pwned2" ]'
bash "$NOTIFY" error "land t1" "train t1 is red" > /dev/null 2>&1; rc=$?
check "a real send after the dry run posts to ntfy and slack" '[ "$rc" = 0 ] && [ "$(calls)" = 2 ] && grep -q "Priority: urgent" "$FAKE_CURL_ARGV" && grep -qF "url = \"https://ntfy.sh/$TOPIC\"" "$FAKE_CURL_LOG" && grep -qF "url = \"$SECRET_HOOK\"" "$FAKE_CURL_LOG"'
check "the ntfy token goes only to ntfy"           '[ "$(grep -c "Bearer tk_secret_token_123" "$FAKE_CURL_LOG")" = 1 ] && grep "Bearer" "$FAKE_CURL_LOG" | has_match -F "ntfy.sh"'
check "no topic, token or webhook on any argv"     '! grep -qF -- "$TOPIC" "$FAKE_CURL_ARGV" && ! grep -q "tk_secret" "$FAKE_CURL_ARGV" && ! grep -q "hooks.slack" "$FAKE_CURL_ARGV"'
check "the curl config file is mode 600"           '! grep -v "^CALL mode=600 " "$FAKE_CURL_LOG" | has_match .'
err="$(NOTIFY_NOW=1300 bash "$NOTIFY" error "land t1" "train t1 is red" 2>&1 >/dev/null)"
check "a duplicate inside the window is dropped"   '[ "$(calls)" = 2 ] && printf "%s" "$err" | has_match "suppressed duplicate"'
NOTIFY_NOW=1300 bash "$NOTIFY" error "land t1" "train t1 is GREEN now" > /dev/null 2>&1
check "a different body is not a duplicate"        '[ "$(calls)" = 4 ]'
NOTIFY_NOW=1700 bash "$NOTIFY" error "land t1" "train t1 is red" > /dev/null 2>&1
check "the same message after the window is sent"  '[ "$(calls)" = 6 ]'
bash "$NOTIFY" info "file" "@$TMP/notify.env" > /dev/null 2>&1
check "a body starting with @ is sent as text, never as a file" 'grep -qF "body=@$TMP/notify.env" "$FAKE_CURL_LOG" && ! grep -q "body=NTFY_TOPIC" "$FAKE_CURL_LOG"'
out="$(FAKE_CURL_RC=7 bash "$NOTIFY" info "down" "network down" 2>/dev/null)"; rc=$?
check "failed delivery falls back to stdout, exit 0" '[ "$rc" = 0 ] && [ "$out" = "[info] down: network down" ]'
FAKE_CURL_RC=0 bash "$NOTIFY" info "down" "network down" > /dev/null 2>&1
check "a failed delivery is not recorded as sent"  '[ "$(grep -c "network down" "$FAKE_CURL_LOG")" = 4 ]'
printf 'NTFY_TOPIC=short\n' > "$TMP/notify.env"
out="$(bash "$NOTIFY" info "t" "short topic" 2>"$TMP/short.err")"
check "a short (guessable) topic is refused"       'grep -q "NTFY_TOPIC refused" "$TMP/short.err" && [ "$out" = "[info] t: short topic" ] && ! grep -q "short topic" "$FAKE_CURL_LOG"'
printf 'SLACK_WEBHOOK_URL=https://hooks.slack.com/x" -o ./evil\n' > "$TMP/notify.env"
out="$(bash "$NOTIFY" info "q" "quote injection" 2>"$TMP/inj.err")"
check "a webhook that could break the curl config is refused" 'grep -q "does not pass to curl" "$TMP/inj.err" && [ "$out" = "[info] q: quote injection" ]'

echo "=== install.sh ==="
HOME="$TMP/home" STATUS_REPO_URL="$TMP/origin.git" XDG_CONFIG_HOME="$TMP/home/.config" bash "$INSTALL" > "$TMP/install.log" 2>&1; rc=$?
U="$TMP/home/.config/systemd/user"
check "install exits 0 and enables nothing"        '[ "$rc" = 0 ] && grep -q "NOT enabled" "$TMP/install.log"'
check "units written: service, timer, http"        '[ -f "$U/concrete-status.service" ] && [ -f "$U/concrete-status.timer" ] && [ -f "$U/concrete-status-http.service" ]'
check "the timer fires every 2 minutes"            'grep -q "^OnUnitActiveSec=2min" "$U/concrete-status.timer"'
check "the http server binds localhost only"       'grep -q -- "--bind 127.0.0.1" "$U/concrete-status-http.service" && ! grep -q "0\.0\.0\.0" "$U/concrete-status-http.service"'
check "the generator runs from a stable copy"      '[ -x "$TMP/home/concrete-status/bin/status.py" ] && grep -q "concrete-status/bin/status.py" "$U/concrete-status.service"'
HOME="$TMP/home" STATUS_REPO_URL="$TMP/origin.git" XDG_CONFIG_HOME="$TMP/home/.config" bash "$INSTALL" --log-dir "$TMP/a dir
ExecStartPre=/bin/false" > "$TMP/install2.log" 2>&1; rc=$?
check "a path that would inject a unit directive is refused" '[ "$rc" = 2 ] && grep -q "unsafe in a unit file" "$TMP/install2.log" && ! grep -q "bin/false" "$U/concrete-status.service"'

echo ""
echo "status page: $PASS passed, $FAIL failed"
[ "$FAIL" -eq 0 ] || exit 1
