#!/usr/bin/env bash
# The default transport of scripts/land/gates_remote.sh:
#
#   gates_transport_ssh.sh <host> <bundle> <ref> <command>
#
# Copies ONE directory to <host> (any ssh destination, e.g. an ~/.ssh/config alias): the bundle, this
# checkout's scripts/land/run-at-ref.sh, and a bash script that brings the host's gate clone up to
# date with its origin (so base, which is on main, is there) and runs run-at-ref.sh with
# RUN_AT_REF_SOURCE=<the bundle>, <ref> and <command>. The ref and command are quoted for bash HERE,
# into that file, so the host's login shell only ever sees `bash land-remote-<id>/run.sh`, whatever
# the command contains. Output is run-at-ref.sh's. The host needs its gate clone (RUN_AT_REF_REPO,
# default ~/concrete2) and nix; it needs no GitHub credentials and no copy of the landing scripts.
#
# Env: LAND_GATES_SSH (default `ssh -o BatchMode=yes -o ServerAliveInterval=20 -o ServerAliveCountMax=120`),
#      LAND_GATES_SCP (default `scp -q -r -o BatchMode=yes`; must copy a directory).
set -uo pipefail
[ $# = 4 ] || { echo "usage: gates_transport_ssh.sh <host> <bundle> <ref> <command>" >&2; exit 2; }
host="$1" bundle="$2" ref="$3" cmd="$4"
SELF_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
read -r -a SSH <<< "${LAND_GATES_SSH:-ssh -o BatchMode=yes -o ServerAliveInterval=20 -o ServerAliveCountMax=120}"
read -r -a SCP <<< "${LAND_GATES_SCP:-scp -q -r -o BatchMode=yes}"
id="land-remote-$(date -u +%Y%m%dT%H%M%S)-$$-$RANDOM"
tmp="$(mktemp -d)" || { echo "transport: cannot create a temp directory" >&2; exit 255; }
trap 'rm -rf "$tmp"' EXIT
mkdir "$tmp/$id"
cp "$bundle" "$tmp/$id/c.bundle" && cp "$SELF_DIR/run-at-ref.sh" "$tmp/$id/run-at-ref.sh" \
  || { echo "transport: cannot stage the bundle" >&2; exit 255; }
{
  echo '#!/usr/bin/env bash'
  echo 'dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"'
  echo 'trap '\''rm -rf "$dir"'\'' EXIT'
  echo 'git -C "${RUN_AT_REF_REPO:-$HOME/concrete2}" fetch -q origin || { echo "transport: fetch in the gate clone failed" >&2; exit 255; }'
  printf 'RUN_AT_REF_SOURCE="$dir/c.bundle" bash "$dir/run-at-ref.sh" %q %q\n' "$ref" "$cmd"
} > "$tmp/$id/run.sh"
"${SCP[@]}" "$tmp/$id" "$host:$id" || { echo "transport: copying to $host failed" >&2; exit 255; }
rc=0
"${SSH[@]}" "$host" "bash $id/run.sh" < /dev/null || rc=$?
exit "$rc"
