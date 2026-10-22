#!/usr/bin/env bash
# Run one queue agent to completion (started detached by dispatch.py; never run by hand).
#
#   run_agent.sh <run-dir> <worktree> <cmd...>
#
# The agent's whole output (stream-json events and the CLI's own stderr) goes to <run-dir>/agent.log;
# its exit status goes to <run-dir>/exit LAST, so a tick that sees the exit file also sees the full
# log. The dispatcher reads both: the exit status, the usage-limit text, the cost and the tokens.
set -uo pipefail
run_dir="$1"; wt="$2"; shift 2
# Started with a new session, so this shell's pid is the agent's process-group id: a dispatcher
# that died before recording it can still stop the whole agent from this file.
echo "$$" > "$run_dir/pid"
# Wait for the dispatcher's go-ahead, written only after this launch is in state.json; a launch the
# dispatcher did not record (it died) or cancelled exits 98 without starting the agent.
for _ in $(seq 1 "${QUEUE_GO_WAIT:-600}"); do
  [ -e "$run_dir/cancel" ] && break
  [ -e "$run_dir/go" ] && break
  sleep 0.1
done
if [ ! -e "$run_dir/go" ] || [ -e "$run_dir/cancel" ]; then
  echo 98 > "$run_dir/exit.tmp" && mv "$run_dir/exit.tmp" "$run_dir/exit"; exit 98
fi
cd "$wt" || { echo 97 > "$run_dir/exit"; exit 97; }
date -u +%Y-%m-%dT%H:%M:%SZ > "$run_dir/started"
"$@" > "$run_dir/agent.log" 2>&1 < /dev/null
rc=$?
echo "$rc" > "$run_dir/exit.tmp" && mv "$run_dir/exit.tmp" "$run_dir/exit"
exit "$rc"
