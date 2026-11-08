#!/usr/bin/env bash
# The default transport of scripts/land/gates_remote.sh:
#
#   gates_transport_ssh.sh <host> <bundle> <ref> <command>
#
# Copies ONE directory to <host> (any ssh destination, e.g. an ~/.ssh/config alias): the bundle, this
# checkout's scripts/land/run-at-ref.sh with the sandbox helper it sources (scripts/checks/lib/
# git_sandbox.sh, staged at the same relative paths), and a bash script that brings the host's gate clone up to
# date with its origin (so base, which is on main, is there) and runs run-at-ref.sh with
# RUN_AT_REF_SOURCE=<the bundle>, <ref> and <command>. The ref and command are quoted for bash HERE,
# into that file, so the host's login shell only ever sees `bash land-remote-<id>/run.sh`, whatever
# the command contains. Output is run-at-ref.sh's. The login must not be root. The host needs its gate clone (RUN_AT_REF_REPO,
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
mkdir -p "$tmp/$id/scripts/land" "$tmp/$id/scripts/checks/lib"
cp "$bundle" "$tmp/$id/c.bundle" && cp "$SELF_DIR/run-at-ref.sh" "$tmp/$id/scripts/land/run-at-ref.sh" \
  && cp "$SELF_DIR/../checks/lib/git_sandbox.sh" "$tmp/$id/scripts/checks/lib/git_sandbox.sh" \
  || { echo "transport: cannot stage the bundle" >&2; exit 255; }
{
  echo '#!/usr/bin/env bash'
  echo 'dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"'
  echo 'trap '\''rm -rf "$dir"'\'' EXIT'
  echo 'gc="${RUN_AT_REF_REPO:-$HOME/concrete2}"'
  # Who the ssh destination logged in as, checked ON THE HOST before anything is fetched: gates never
  # run as root, an unreadable identity is not "not root", and a login without its gate clone names
  # the account and path. (2026-11-08: a train run from an account whose `concrete-ci-2` alias is
  # the root admin login looked for /root/concrete2 and reported only "fetch in the gate clone
  # failed".) Each is a transport failure: INCOMPLETE, never a pass.
  echo 'uid="$(id -u 2>/dev/null)"; who="$(id -un 2>/dev/null)@$(hostname 2>/dev/null)"'
  echo 'case "$uid" in ""|*[!0-9]*) echo "transport: cannot read the gate host login (id -u gave \"$uid\")" >&2; exit 255 ;; esac'
  echo '[ "$uid" != 0 ] || { echo "transport: the gate host login is $who (root); gates run as the gate user (dev on the CI hosts), never root: check the ssh destination User" >&2; exit 255; }'
  # A gate clone is a work tree whose top level is $gc (a clone, or a worktree whose .git is a file).
  echo 'top="$(git -C "$gc" rev-parse --show-toplevel 2>/dev/null)"'
  echo '[ -n "$top" ] && [ "$(cd "$gc" && pwd -P)" = "$(cd "$top" && pwd -P)" ] || { echo "transport: no gate clone at $gc for $who (RUN_AT_REF_REPO); is the ssh destination the gate user?" >&2; exit 255; }'
  echo 'git -C "$gc" fetch -q origin || { echo "transport: fetch in the gate clone failed" >&2; exit 255; }'
  printf 'RUN_AT_REF_SOURCE="$dir/c.bundle" bash "$dir/scripts/land/run-at-ref.sh" %q %q\n' "$ref" "$cmd"
} > "$tmp/$id/run.sh"
"${SCP[@]}" "$tmp/$id" "$host:$id" || { echo "transport: copying to $host failed" >&2; exit 255; }
rc=0
"${SSH[@]}" "$host" "bash $id/run.sh" < /dev/null || rc=$?
exit "$rc"
