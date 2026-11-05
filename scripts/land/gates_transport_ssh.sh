#!/usr/bin/env bash
# The default transport of scripts/land/gates_remote.sh:
#
#   gates_transport_ssh.sh <host> <bundle> <ref> <command>
#
# Copies the bundle to <host> (any ssh destination, e.g. an ~/.ssh/config alias), brings the host's
# gate clone up to date with its origin (so base, which is on main, is there), and pipes THIS
# checkout's scripts/land/run-at-ref.sh through ssh with RUN_AT_REF_SOURCE=<the copied bundle>. Its
# output is run-at-ref.sh's. The host needs its gate clone (RUN_AT_REF_REPO, default ~/concrete2) and
# nix; it needs no GitHub credentials and no copy of the landing scripts.
#
# Env: LAND_GATES_SSH (default `ssh -o BatchMode=yes -o ServerAliveInterval=20 -o ServerAliveCountMax=120`),
#      LAND_GATES_SCP (default `scp -q -o BatchMode=yes`).
set -uo pipefail
[ $# = 4 ] || { echo "usage: gates_transport_ssh.sh <host> <bundle> <ref> <command>" >&2; exit 2; }
host="$1" bundle="$2" ref="$3" cmd="$4"
SELF_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
read -r -a SSH <<< "${LAND_GATES_SSH:-ssh -o BatchMode=yes -o ServerAliveInterval=20 -o ServerAliveCountMax=120}"
read -r -a SCP <<< "${LAND_GATES_SCP:-scp -q -o BatchMode=yes}"
remote_bundle="land-remote-$(basename "$bundle" .bundle)-$$-$RANDOM.bundle"
"${SCP[@]}" "$bundle" "$host:$remote_bundle" || { echo "transport: copying the bundle to $host failed" >&2; exit 255; }
q() { printf '%q' "$1"; }
rc=0
"${SSH[@]}" "$host" "git -C \"\${RUN_AT_REF_REPO:-\$HOME/concrete2}\" fetch -q origin \
  && RUN_AT_REF_SOURCE=\"\$HOME/$remote_bundle\" bash -s -- $(q "$ref") $(q "$cmd"); rc=\$?; rm -f \"\$HOME/$remote_bundle\"; exit \$rc" \
  < "$SELF_DIR/run-at-ref.sh" || rc=$?
exit "$rc"
