#!/usr/bin/env bash
# AGENT JOBS HAVE ONE ATTEMPT AND A BOUNDED WATCHER.
#
# CI has no systemd user session. These controls put systemd-run and systemctl
# stubs on PATH. The stubs reproduce dollar/percent command expansion, runtime
# kills, --collect unloading, result queries and collection, and user-bus errors.
# Controls cover exact argv bytes, the status/inactive race, retained timeout
# results, exclusive job directories, eval-safe WAIT output, normal statuses 0
# and 3, lost units (125), deadline overruns (124), query failures (2), resource
# properties, logs, one attempt, and input/environment refusals. Every launcher
# invocation is bounded. AGENT_LAUNCHER selects a mutant for mutation testing.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR" || exit 2
LAUNCH="${AGENT_LAUNCHER:-$ROOT_DIR/scripts/agent/launch.sh}"
[ -f "$LAUNCH" ] || { echo "FATAL: $LAUNCH missing" >&2; exit 2; }
TMP="$(mktemp -d)" || exit 2
STUB_STATE_DIR="$TMP/systemd"
STUB_RELEASE_DIR="$TMP/release"
mkdir -p "$STUB_STATE_DIR" "$STUB_RELEASE_DIR" "$TMP/bin" "$TMP/empty" "$TMP/work"
export STUB_STATE_DIR STUB_RELEASE_DIR
export AGENT_JOBS_DIR="$TMP/jobs" AGENT_WAIT_INTERVAL=1 AGENT_WAIT_GRACE=1
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

cleanup() {
  local d
  if [ -d "$STUB_STATE_DIR" ]; then
    for d in "$STUB_STATE_DIR"/*; do
      [ -d "$d" ] && touch "$STUB_RELEASE_DIR/${d##*/}"
    done
  fi
  rm -rf "$TMP"
}
trap cleanup EXIT

cat > "$TMP/bin/systemd-run" <<'STUB'
#!/usr/bin/env bash
set -u
raw="$STUB_STATE_DIR/args.$$"
printf '%s\n' "$@" > "$raw" || exit 2
unit="" workdir="$PWD" job_path="$PATH" job_home="$HOME" job_dir="" launch_self=""
runtime=30 collect=0
while [ "$#" -gt 0 ]; do
  case "$1" in
    --user) shift ;;
    --collect) collect=1; shift ;;
    --unit=*) unit="${1#--unit=}"; shift ;;
    --property=WorkingDirectory=*) workdir="${1#--property=WorkingDirectory=}"; shift ;;
    --property=RuntimeMaxSec=*) runtime="${1#--property=RuntimeMaxSec=}"; shift ;;
    --property=*) shift ;;
    --setenv=PATH=*) job_path="${1#--setenv=PATH=}"; shift ;;
    --setenv=HOME=*) job_home="${1#--setenv=HOME=}"; shift ;;
    --setenv=AGENT_JOB_DIR=*) job_dir="${1#--setenv=AGENT_JOB_DIR=}"; shift ;;
    --setenv=AGENT_LAUNCH_SELF=*) launch_self="${1#--setenv=AGENT_LAUNCH_SELF=}"; shift ;;
    --setenv=*) shift ;;
    *) break ;;
  esac
done
[ -n "$unit" ] && [ "$#" -gt 0 ] || { rm -f "$raw"; exit 2; }
state="$STUB_STATE_DIR/$unit"
mkdir -p "$state" || exit 2
mv "$raw" "$state/args" || exit 2
printf '%s\n' "$job_dir" > "$state/jobdir"
: > "$state/active"
(
  export PATH="$job_path" HOME="$job_home" STUB_UNIT="$unit"
  export AGENT_JOB_DIR="$job_dir" AGENT_LAUNCH_SELF="$launch_self"
  expand_arg() {
    local rest="$1" out="" prefix="" name=""
    while [[ "$rest" == *[\$%]* ]]; do
      prefix="${rest%%[\$%]*}"
      out+="$prefix"
      rest="${rest:${#prefix}}"
      case "$rest" in
        \$\$*) out+='$'; rest="${rest:2}" ;;
        %%*) out+='%'; rest="${rest:2}" ;;
        %n*) out+="$unit"; rest="${rest:2}" ;;
        \$\{*)
          name="${rest:2}"
          if [[ "$name" == *'}'* ]]; then
            prefix="${name%%\}*}"
            if [[ "$prefix" =~ ^[A-Za-z_][A-Za-z0-9_]*$ ]]; then
              out+="${!prefix-}"; rest="${name:${#prefix}+1}"
            else out+='$'; rest="${rest:1}"; fi
          else out+='$'; rest="${rest:1}"; fi ;;
        \$[A-Za-z_]*)
          name="${rest:1}"; prefix="${name%%[^A-Za-z0-9_]*}"
          out+="${!prefix-}"; rest="${rest:${#prefix}+1}" ;;
        \$[0-9]*)
          name="${rest:1}"; prefix="${name%%[^0-9]*}"
          rest="${rest:${#prefix}+1}" ;;
        \$*) out+='$'; rest="${rest:1}" ;;
        %*) out+='%'; rest="${rest:1}" ;;
      esac
    done
    printf '%s' "$out$rest"
  }
  expanded=()
  for arg in "$@"; do expanded+=("$(expand_arg "$arg")"); done
  rc=1
  if cd "$workdir"; then
    "${expanded[@]}" & command_pid=$!
    (
      sleep "$runtime"
      if kill -0 "$command_pid" 2>/dev/null; then
        : > "$state/timed-out"
        kill -TERM "$command_pid" 2>/dev/null || :
      fi
    ) & watchdog_pid=$!
    wait "$command_pid"; rc=$?
    kill "$watchdog_pid" 2>/dev/null || :
    wait "$watchdog_pid" 2>/dev/null || :
  fi
  printf '%s\n' "$rc" > "$state/exit"
  if [ -f "$state/timed-out" ]; then result=timeout
  elif [ "$rc" -eq 0 ]; then result=success
  else result=exit-code; fi
  printf '%s\n' "$result" > "$state/result"
  rm -f "$state/active"
  if [ "$collect" -eq 1 ]; then rm -f "$state/result"; : > "$state/collected"; fi
  exit "$rc"
) &
printf '%s\n' "$!" > "$state/pid"
exit 0
STUB
cat > "$TMP/bin/systemctl" <<'STUB'
#!/usr/bin/env bash
set -u
[ "${1:-}" = --user ] || exit 2
action="${2:-}"; unit="${3:-}"
state="$STUB_STATE_DIR/$unit"
case "$action" in
  is-active)
    if [ -f "$state/no-bus" ]; then
      echo 'Failed to connect to bus: No medium found' >&2
      exit 1
    fi
    [ ! -f "$state/no-bus-empty" ] || exit 1
    if [ -f "$state/finish-on-is-active" ]; then
      jobdir="$(cat "$state/jobdir")"
      tmp="$jobdir/.status.systemd"
      cp "$state/finish-on-is-active" "$tmp" && mv "$tmp" "$jobdir/status"
      rm -f "$state/active"
    fi
    if [ -f "$state/active" ]; then echo active; exit 0; fi
    echo inactive; exit 1 ;;
  show)
    [ -r "$state/result" ] && cat "$state/result"
    exit 0 ;;
  reset-failed)
    : > "$state/reset-failed"
    [ -f "$state/active" ] || rm -f "$state/result"
    exit 0 ;;
  *) exit 2 ;;
esac
STUB
chmod +x "$TMP/bin/systemd-run" "$TMP/bin/systemctl"
export PATH="$TMP/bin:$PATH"
BOUNDED_PARENT="$TMP"
source "$ROOT_DIR/scripts/checks/lib/bounded.sh" || exit 2

OUT="" ERR="" RC=0
capture() { # budget command...; leave exact streams and status in OUT, ERR, RC
  local budget="$1"
  shift
  OUT="$(bounded_run "$budget" -- "$@" 2>"$TMP/stderr")"
  RC=$?
  ERR="$(cat "$TMP/stderr")"
  if bounded_is_incomplete "$RC"; then no "INCOMPLETE: $*"; fi
}
expect_rc() { [ "$RC" -eq "$1" ] && ok "$2" || { no "$2: expected exit $1, got $RC"; printf '%s\n' "$OUT" "$ERR" | sed 's/^/      /'; }; }
expect_out() { [ "$OUT" = "$1" ] && ok "$2" || { no "$2: unexpected stdout"; printf 'expected:\n%s\nactual:\n%s\n' "$1" "$OUT" | sed 's/^/      /'; }; }
file_line() { # file exact-line label
  if has_match -F -x -- "$2" < "$1"; then ok "$3"; else no "$3 (missing exact line: $2)"; fi
}
parse_start() {
  UNIT="$(printf '%s\n' "$OUT" | sed -n 's/^UNIT=//p')"
  JOBDIR="$(printf '%s\n' "$OUT" | sed -n 's/^DIR=//p')"
  WAIT="$(printf '%s\n' "$OUT" | sed -n 's/^WAIT=//p')"
  EXPECTED="$(printf 'UNIT=%s\nDIR=%s\nWAIT=bash %q wait %q' "$UNIT" "$JOBDIR" "$LAUNCH" "$JOBDIR")"
}
release_job() { # release a stub-blocked command and observe its wrapper finishing
  local n=0
  touch "$STUB_RELEASE_DIR/$UNIT"
  while { [ ! -f "$JOBDIR/status" ] || [ -f "$STUB_STATE_DIR/$UNIT/active" ]; } && [ "$n" -lt 50 ]; do
    sleep 0.1; n=$((n+1))
  done
  [ -f "$JOBDIR/status" ] && [ ! -f "$STUB_STATE_DIR/$UNIT/active" ] \
    && ok "$1 cleaned up" || no "$1 did not finish after its release"
}

echo "=== start: exact output, properties and one job directory ==="
capture 10 bash -n "$LAUNCH"
expect_rc 0 "launch.sh parses"
expect_out "" "bash -n has no output"
capture 10 bash -c 'cd "$1" && bash "$2" start writer 7 -- bash -c '\''printf "cwd=%s\n" "$PWD"; i=1; while [ "$i" -le 45 ]; do printf "line-%s\n" "$i"; i=$((i+1)); done'\''' _ "$TMP/work" "$LAUNCH"
expect_rc 0 "a successful start"
parse_start
expect_out "$EXPECTED" "start prints exactly UNIT, DIR and absolute WAIT"
[ -n "$UNIT" ] && [ -n "$JOBDIR" ] && [ "$(cat "$JOBDIR/unit")" = "$UNIT" ] \
  && ok "unit file records the unique unit" || no "unit file is missing or wrong"
case "$(cat "$JOBDIR/deadline")" in ''|*[!0-9]*) no "deadline is not an epoch second" ;; *) ok "deadline is an epoch second" ;; esac
has_match -F 'line-%s' < "$JOBDIR/cmd" && ok "cmd records the command line" || no "cmd does not record the command"
ARGS="$STUB_STATE_DIR/$UNIT/args"
file_line "$ARGS" --user "systemd-run uses the user manager"
if has_match -F -x -- --collect < "$ARGS"; then no "--collect must not unload the result"; else ok "--collect is absent"; fi
file_line "$ARGS" "--unit=$UNIT" "the sanitised unique unit is passed"
file_line "$ARGS" --property=MemoryMax=8G "MemoryMax defaults to 8G"
file_line "$ARGS" --property=TasksMax=1024 "TasksMax defaults to 1024"
file_line "$ARGS" --property=RuntimeMaxSec=7 "RuntimeMaxSec is SECS"
file_line "$ARGS" "--property=WorkingDirectory=$TMP/work" "WorkingDirectory is the caller's directory"
file_line "$ARGS" "--setenv=PATH=$PATH" "PATH is passed"
file_line "$ARGS" "--setenv=HOME=$HOME" "HOME is passed"

echo "=== wait: normal status and the last 40 log lines ==="
capture 15 bash "$LAUNCH" wait "$JOBDIR"
expect_rc 0 "wait returns 0 for a successful job"
EXPECTED="JOB $UNIT ENDED status=0"
i=6; while [ "$i" -le 45 ]; do EXPECTED="$EXPECTED
line-$i"; i=$((i+1)); done
expect_out "$EXPECTED" "success prints its state and exactly log lines 6-45"
[ "$(sed -n '1p' "$JOBDIR/out.log")" = "cwd=$TMP/work" ] \
  && ok "the wrapped command ran in the caller's directory" || no "the wrapped command ran elsewhere"
[ "$(wc -l < "$JOBDIR/out.log" | tr -d ' ')" -eq 46 ] \
  && ok "out.log retains the full stdout and stderr" || no "out.log is incomplete"
[ -z "$(find "$JOBDIR" -name '.status.*' -print)" ] \
  && ok "status was moved into place without a temporary file left" || no "status temporary file remains"
capture 10 bash "$LAUNCH" status "$JOBDIR"
expect_rc 0 "status returns the successful job's status"
expect_out "JOB $UNIT ENDED status=0" "status prints one line and does not wait or print the log"

echo "=== a non-zero command runs once and returns its status ==="
capture 10 bash "$LAUNCH" start --mem 2G --tasks 17 fails 5 -- bash -c 'echo attempt >> "$AGENT_JOBS_DIR/attempts"; echo failed; exit 3'
expect_rc 0 "start of the exit-3 job"
parse_start
expect_out "$EXPECTED" "custom start also prints exactly three lines"
file_line "$STUB_STATE_DIR/$UNIT/args" --property=MemoryMax=2G "--mem overrides MemoryMax"
file_line "$STUB_STATE_DIR/$UNIT/args" --property=TasksMax=17 "--tasks overrides TasksMax"
capture 15 bash "$LAUNCH" wait "$JOBDIR"
expect_rc 3 "wait returns the command's exit 3"
expect_out "JOB $UNIT ENDED status=3
failed" "exit-3 output is its state and log"
[ "$(wc -l < "$AGENT_JOBS_DIR/attempts" | tr -d ' ')" -eq 1 ] \
  && ok "a failed writer has exactly one attempt" || no "the failed writer was retried"
capture 10 bash "$LAUNCH" status "$JOBDIR"
expect_rc 3 "status also returns exit 3"
expect_out "JOB $UNIT ENDED status=3" "status of exit 3 is one line"

echo "=== a unit lost without status is 125 ==="
capture 10 bash "$LAUNCH" start vanished 30 -- bash -c 'while [ ! -e "$STUB_RELEASE_DIR/$STUB_UNIT" ]; do sleep 0.1; done'
expect_rc 0 "start of the lost-unit job"
parse_start
rm -f "$STUB_STATE_DIR/$UNIT/active"
printf 'killed\n' > "$STUB_STATE_DIR/$UNIT/result"
capture 10 bash "$LAUNCH" wait "$JOBDIR"
expect_rc 125 "a lost unit returns 125"
expect_out "JOB $UNIT ENDED without a status (killed, timed out or OOM): killed" "lost-unit output includes systemctl's result"
release_job "the lost-unit fixture"

echo "=== still active past deadline plus grace is 124 ==="
capture 10 bash "$LAUNCH" start overrun 30 -- bash -c 'while [ ! -e "$STUB_RELEASE_DIR/$STUB_UNIT" ]; do sleep 0.1; done'
expect_rc 0 "start of the overrun job"
parse_start
capture 10 bash "$LAUNCH" status "$JOBDIR"
expect_rc 0 "status of a running job is a successful observation"
expect_out "JOB $UNIT RUNNING" "status does not wait for a running job"
printf '1\n' > "$JOBDIR/deadline"
capture 10 bash "$LAUNCH" wait "$JOBDIR"
expect_rc 124 "deadline plus grace returns 124"
expect_out "JOB $UNIT STILL RUNNING past its deadline" "overrun output is INCOMPLETE, never a pass"
release_job "the overrun fixture"

echo "=== argv survives systemd dollar and percent expansion ==="
literal='prefix ${HOME} $1 %n and a space'
capture 10 bash "$LAUNCH" start literal 10 -- bash -c 'printf %s "$1"' _ "$literal"
expect_rc 0 "start of the literal-argv job"
parse_start
capture 15 bash "$LAUNCH" wait "$JOBDIR"
expect_rc 0 "literal-argv job exits normally"
expect_out "JOB $UNIT ENDED status=0
$literal" 'an argument containing ${HOME}, $1, %n and a space reaches the job byte for byte'
[ "$(cat "$JOBDIR/out.log")" = "$literal" ] && ok "out.log preserves the exact literal bytes" || no "out.log corrupted the literal argument"

echo "=== timeout result is read before explicit collection ==="
capture 10 bash "$LAUNCH" start timeout-result 1 -- bash -c 'while [ ! -e "$STUB_RELEASE_DIR/$STUB_UNIT" ]; do sleep 0.1; done'
expect_rc 0 "start of the timed-out job"
parse_start
capture 15 bash "$LAUNCH" wait "$JOBDIR"
expect_rc 125 "a systemd timeout without status returns 125"
expect_out "JOB $UNIT ENDED without a status (killed, timed out or OOM): timeout" "125 reports the retained timeout result, not unknown"
[ "$(cat "$JOBDIR/result")" = timeout ] && ok "the result is cached in the job directory" || no "the timeout result was not cached"
[ -f "$STUB_STATE_DIR/$UNIT/reset-failed" ] && ok "reset-failed collects only after the result was read" || no "the unit was not explicitly collected"

echo "=== status appearing at the inactive observation wins the TOCTOU race ==="
capture 10 bash "$LAUNCH" start race 30 -- bash -c 'while [ ! -e "$STUB_RELEASE_DIR/$STUB_UNIT" ]; do sleep 0.1; done'
expect_rc 0 "start of the status-race job"
parse_start
printf '0\n' > "$STUB_STATE_DIR/$UNIT/finish-on-is-active"
capture 10 bash "$LAUNCH" wait "$JOBDIR"
expect_rc 0 "a status written before is-active returns is not declared lost"
expect_out "JOB $UNIT ENDED status=0" "inactive triggers a second status-file check"
release_job "the status-race fixture"

echo "=== job directories are reserved exclusively ==="
capture 10 bash "$LAUNCH" start --dir "$TMP/exclusive" exclusive 30 -- bash -c 'while [ ! -e "$STUB_RELEASE_DIR/$STUB_UNIT" ]; do sleep 0.1; done'
expect_rc 0 "start in a new explicit job directory"
parse_start
before="$(cd "$JOBDIR" && sha256sum unit deadline cmd argv)"
capture 10 bash "$LAUNCH" start --dir "$TMP/exclusive" duplicate 30 -- true
expect_rc 2 "a second start on the same directory is refused"
expect_out "" "duplicate directory refusal has no stdout"
has_match -F 'job directory already exists' <<<"$ERR" && ok "duplicate directory has a clear error" || no "duplicate directory error is unclear: $ERR"
after="$(cd "$JOBDIR" && sha256sum unit deadline cmd argv)"
[ "$after" = "$before" ] && ok "the refused start leaves the first job's files untouched" || no "the refused start changed the first job's files"
release_job "the exclusive-directory fixture"

echo "=== the printed WAIT value is safe to eval ==="
special="$TMP/job ; touch pwned ; #"
capture 10 bash "$LAUNCH" start --dir "$special" quoted 10 -- true
expect_rc 0 "start in a directory with spaces and shell metacharacters"
parse_start
expect_out "$EXPECTED" "both WAIT paths are shell-quoted with printf %q"
capture 15 bash -c 'cd "$1" && eval "$2"' _ "$TMP/work" "$WAIT"
expect_rc 0 "eval of the printed WAIT value waits on the right job"
expect_out "JOB $UNIT ENDED status=0" "eval prints the intended job result"
[ ! -e "$TMP/work/pwned" ] && ok "eval executes nothing else" || no "eval injected the pwned command"

echo "=== systemd query failures are not lost units ==="
capture 10 bash "$LAUNCH" start no-bus 30 -- bash -c 'while [ ! -e "$STUB_RELEASE_DIR/$STUB_UNIT" ]; do sleep 0.1; done'
expect_rc 0 "start of the no-user-bus job"
parse_start
: > "$STUB_STATE_DIR/$UNIT/no-bus"
capture 10 bash "$LAUNCH" wait "$JOBDIR"
expect_rc 2 "a user-bus query error returns 2, not 125"
expect_out "" "a user-bus query error has no job result on stdout"
has_match -F 'cannot query systemd' <<<"$ERR" && has_match -F 'Failed to connect to bus' <<<"$ERR" \
  && ok "the query error includes systemctl's bus error" || no "the query error is unclear: $ERR"
rm -f "$STUB_STATE_DIR/$UNIT/no-bus"
: > "$STUB_STATE_DIR/$UNIT/no-bus-empty"
capture 10 bash "$LAUNCH" wait "$JOBDIR"
expect_rc 2 "a silent user-bus query failure also returns 2"
has_match -F 'no output' <<<"$ERR" && ok "a silent query failure is identified" || no "silent query failure is unclear: $ERR"
release_job "the no-user-bus fixture"

echo "=== invalid input and unavailable systemd are exit 2 ==="
capture 10 bash "$LAUNCH" start bad/name 1 -- true
expect_rc 2 "an invalid NAME is refused"
expect_out "" "invalid NAME has no stdout"
has_match -F 'NAME must' <<<"$ERR" && ok "invalid NAME explains the error on stderr" || no "invalid NAME has no clear stderr"
capture 10 bash "$LAUNCH" start valid 0 -- true
expect_rc 2 "zero SECS is refused"
has_match -F 'SECS must be a positive integer' <<<"$ERR" && ok "zero SECS explains the error" || no "zero SECS has no clear stderr"
capture 10 bash "$LAUNCH" start valid nope -- true
expect_rc 2 "non-numeric SECS is refused"
has_match -F 'SECS must be a positive integer' <<<"$ERR" && ok "non-numeric SECS explains the error" || no "non-numeric SECS has no clear stderr"
capture 10 env PATH="$TMP/empty" /bin/bash "$LAUNCH" start valid 1 -- true
expect_rc 2 "missing systemd-run is refused"
expect_out "" "missing systemd-run has no stdout"
[ "$ERR" = 'launch.sh: systemd-run --user is unavailable (systemd-run not found)' ] \
  && ok "missing systemd-run has the exact clear error" || no "unexpected missing-systemd-run stderr: $ERR"

echo
bounded_gate_status || FAIL=$((FAIL+1))
echo "check_agent_launch: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
