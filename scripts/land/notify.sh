#!/usr/bin/env bash
# Push a notification about landing work: scripts/land/notify.sh [--dry-run] <level> <title> <body>
#
#   level   info | success | warn | error   (sets the ntfy priority and tags, and the Slack emoji)
#
# Channels come from ~/.config/concrete/notify.env (CONCRETE_NOTIFY_ENV overrides the path); see
# scripts/land/notify.env.example. The file is parsed as KEY=VALUE lines, never sourced:
#
#   NTFY_TOPIC         ntfy topic (https://ntfy.sh, no account). It MUST be a long random string:
#                      an ntfy topic is public, so anyone who guesses it can read every message and
#                      post fake ones. Topics shorter than 24 characters are refused.
#   NTFY_SERVER        default https://ntfy.sh
#   NTFY_TOKEN         optional access token (a self-hosted or reserved topic)
#   SLACK_WEBHOOK_URL  optional Slack incoming webhook
#   NOTIFY_DEDUP_SECONDS  an identical (level, title, body) inside this window is dropped; default 900
#
# With no channel configured, or when every channel fails, the message goes to stdout. Delivery
# failures are reported on stderr but the exit status is still 0, so a notification can never fail
# a landing. Exit 2 is a usage error only. --dry-run (or NOTIFY_DRY_RUN=1) prints what each channel
# would receive, with the topic and webhook redacted, and sends nothing; it honours the dedup window
# but records nothing in it.
#
# Test seams: NOTIFY_CURL (curl binary), NOTIFY_STATE_DIR (dedup state), NOTIFY_NOW (epoch).
set -uo pipefail

DRY="${NOTIFY_DRY_RUN:-0}"
if [ "${1:-}" = "--dry-run" ]; then DRY=1; shift; fi
usage() { echo "usage: notify.sh [--dry-run] <info|success|warn|error> <title> <body>" >&2; exit 2; }
[ "$#" -eq 3 ] || usage
LEVEL="$1" TITLE="$2" BODY="$3"
case "$LEVEL" in info|success|warn|error) ;; *) usage ;; esac

ENV_FILE="${CONCRETE_NOTIFY_ENV:-$HOME/.config/concrete/notify.env}"
CURL="${NOTIFY_CURL:-curl}"
STATE_DIR="${NOTIFY_STATE_DIR:-${XDG_STATE_HOME:-$HOME/.local/state}/concrete/notify}"
NOW="${NOTIFY_NOW:-$(date +%s)}"

NTFY_TOPIC="" NTFY_SERVER="https://ntfy.sh" NTFY_TOKEN="" SLACK_WEBHOOK_URL="" NOTIFY_DEDUP_SECONDS=900
if [ -f "$ENV_FILE" ]; then
  # A world- or group-readable file leaks the topic to other users on a shared host.
  perm="$(stat -c %a "$ENV_FILE" 2>/dev/null || stat -f %Lp "$ENV_FILE" 2>/dev/null || echo 600)"
  case "$perm" in *00) ;; *) echo "notify: warning: $ENV_FILE is mode $perm; chmod 600 it" >&2 ;; esac
  while IFS= read -r line || [ -n "$line" ]; do
    line="${line%$'\r'}"
    case "$line" in ''|'#'*) continue ;; esac
    key="${line%%=*}"; val="${line#*=}"
    key="${key//[[:space:]]/}"
    val="${val#"${val%%[![:space:]]*}"}"; val="${val%"${val##*[![:space:]]}"}"
    case "$val" in \"*\") val="${val:1:${#val}-2}" ;; \'*\') val="${val:1:${#val}-2}" ;; esac
    case "$key" in
      NTFY_TOPIC|NTFY_SERVER|NTFY_TOKEN|SLACK_WEBHOOK_URL|NOTIFY_DEDUP_SECONDS) printf -v "$key" '%s' "$val" ;;
      *) echo "notify: ignoring unknown key '$key' in $ENV_FILE" >&2 ;;
    esac
  done < "$ENV_FILE"
fi
case "$NOTIFY_DEDUP_SECONDS" in ''|*[!0-9]*) NOTIFY_DEDUP_SECONDS=900 ;; esac

# --- dedup: same level+title+body within the window is dropped ------------------------------------
hash_of() { if command -v sha256sum >/dev/null 2>&1; then sha256sum; else shasum -a 256; fi | cut -c1-32; }
KEY="$(printf '%s\0%s\0%s' "$LEVEL" "$TITLE" "$BODY" | hash_of)"
mkdir -p "$STATE_DIR" 2>/dev/null && chmod 700 "$STATE_DIR" 2>/dev/null
STAMP="$STATE_DIR/$KEY"
if [ -f "$STAMP" ]; then
  last="$(cat "$STAMP" 2>/dev/null)"; case "$last" in ''|*[!0-9]*) last=0 ;; esac
  if [ $(( NOW - last )) -lt "$NOTIFY_DEDUP_SECONDS" ]; then
    echo "notify: suppressed duplicate ($LEVEL) '$TITLE' (sent $(( NOW - last ))s ago, window ${NOTIFY_DEDUP_SECONDS}s)" >&2
    exit 0
  fi
fi
# Old stamps are pruned so the directory does not grow without bound.
find "$STATE_DIR" -type f -mmin +1440 -delete 2>/dev/null || true

case "$LEVEL" in
  info)    PRIO=default; TAGS=information_source; EMOJI=":information_source:" ;;
  success) PRIO=default; TAGS=white_check_mark;   EMOJI=":white_check_mark:" ;;
  warn)    PRIO=high;    TAGS=warning;            EMOJI=":warning:" ;;
  error)   PRIO=urgent;  TAGS=rotating_light;     EMOJI=":rotating_light:" ;;
esac

json_str() { python3 -I -c 'import json,sys; print(json.dumps(sys.argv[1]))' "$1"; }
sent=0 tried=0

# post <secret-url> [header...]; body on stdin. The URL (it carries the topic or the webhook secret)
# and any Authorization header go in a mode-600 curl config file, so no secret is on a command line
# (visible in ps) and nothing is passed as `@file` text: the body is read from stdin (`@-`), so a body
# that starts with `@` cannot make curl upload a local file.
post() {
  local url="$1" cfg rc; shift
  cfg="$(umask 077; mktemp)" || return 1
  printf 'url = "%s"\n' "$url" > "$cfg"
  [ -n "$NTFY_TOKEN" ] && [ "$url" != "$SLACK_WEBHOOK_URL" ] && printf 'header = "Authorization: Bearer %s"\n' "$NTFY_TOKEN" >> "$cfg"
  "$CURL" -fsS -m 15 "$@" --data-binary @- -K "$cfg" >/dev/null; rc=$?
  rm -f "$cfg"; return "$rc"
}
# A value written into the curl config must not be able to close its quotes or add a line.
safe_cfg() { [[ "$1" =~ ^[A-Za-z0-9_.:/?=\&%+~-]+$ ]]; }

if [ -n "$NTFY_TOPIC" ]; then
  if ! [[ "$NTFY_TOPIC" =~ ^[A-Za-z0-9_-]{24,64}$ ]]; then
    echo "notify: NTFY_TOPIC refused: use 24-64 random characters [A-Za-z0-9_-] (a short topic is guessable, and anyone who guesses it can read and post)" >&2
  elif ! safe_cfg "$NTFY_SERVER" || { [ -n "$NTFY_TOKEN" ] && ! safe_cfg "$NTFY_TOKEN"; }; then
    echo "notify: NTFY_SERVER or NTFY_TOKEN contains characters notify.sh does not pass to curl" >&2
  else
    tried=$((tried+1))
    # Header values cannot carry newlines; the body can.
    t1="$(printf '%s' "$TITLE" | tr '\r\n' '  ')"
    if [ "$DRY" = 1 ]; then
      echo "dry-run ntfy: POST ${NTFY_SERVER%/}/<topic> priority=$PRIO tags=$TAGS title=$t1"
      echo "dry-run ntfy body: $BODY"
      sent=$((sent+1))
    elif printf '%s' "$BODY" | post "${NTFY_SERVER%/}/$NTFY_TOPIC" -H "Title: $t1" -H "Priority: $PRIO" -H "Tags: $TAGS"; then
      sent=$((sent+1))
    else
      echo "notify: ntfy delivery failed" >&2
    fi
  fi
fi

if [ -n "$SLACK_WEBHOOK_URL" ]; then
  if ! safe_cfg "$SLACK_WEBHOOK_URL"; then
    echo "notify: SLACK_WEBHOOK_URL contains characters notify.sh does not pass to curl" >&2
  else
    tried=$((tried+1))
    payload="{\"text\": $(json_str "$EMOJI *$TITLE*"$'\n'"$BODY")}"
    if [ "$DRY" = 1 ]; then
      echo "dry-run slack: POST <webhook> $payload"
      sent=$((sent+1))
    elif printf '%s' "$payload" | post "$SLACK_WEBHOOK_URL" -H 'Content-Type: application/json'; then
      sent=$((sent+1))
    else
      echo "notify: slack delivery failed" >&2
    fi
  fi
fi

if [ "$sent" -eq 0 ]; then
  [ "$tried" -gt 0 ] && echo "notify: no channel delivered; falling back to stdout" >&2
  printf '[%s] %s: %s\n' "$LEVEL" "$TITLE" "$BODY"
fi
# A dry run records nothing, so the real send that follows it is not suppressed; neither does a
# message that every configured channel failed to deliver, so a retry is not swallowed as a duplicate.
if [ "$DRY" != 1 ] && { [ "$sent" -gt 0 ] || [ "$tried" -eq 0 ]; }; then
  printf '%s\n' "$NOW" > "$STAMP" 2>/dev/null || true
fi
exit 0
