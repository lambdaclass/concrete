#!/usr/bin/env bash
# The work-queue dispatcher's entry point (docs/project/WORK_QUEUE.md). The systemd timer that
# install-queue.sh writes runs `dispatch.sh tick` every few minutes as user `agent`.
#
#   dispatch.sh tick | status | release <task>
#
# Configuration: ~/.config/concrete-queue/env (KEY=value lines, sourced) and the environment; the
# knobs are listed in dispatch.py. The kill switch is ~/.concrete-queue-paused: while it exists no
# agent starts; when its first line is `kill`, running agents are stopped and their tasks released.
set -euo pipefail
CONF="${QUEUE_ENV_FILE:-$HOME/.config/concrete-queue/env}"
if [ -f "$CONF" ]; then
  set -a
  # shellcheck disable=SC1090
  . "$CONF"
  set +a
fi
# Agents and gates need the user's tools (claude, cswap, nix, node) on a timer's minimal PATH.
export PATH="$HOME/.local/bin:$HOME/.npm-global/bin:$HOME/.local/opt/node/bin:/nix/var/nix/profiles/default/bin:$PATH"
unset GIT_DIR GIT_WORK_TREE GIT_INDEX_FILE GIT_COMMON_DIR
exec python3 "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/dispatch.py" "$@"
