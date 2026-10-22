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
cd "$wt" || { echo 97 > "$run_dir/exit"; exit 97; }
date -u +%Y-%m-%dT%H:%M:%SZ > "$run_dir/started"
"$@" > "$run_dir/agent.log" 2>&1 < /dev/null
rc=$?
echo "$rc" > "$run_dir/exit.tmp" && mv "$run_dir/exit.tmp" "$run_dir/exit"
exit "$rc"
