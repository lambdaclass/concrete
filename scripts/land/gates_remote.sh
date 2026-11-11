#!/usr/bin/env bash
# THE IMPACT GATES ON THE GATE HOST. `train.sh check`'s impact leg, run on the CI server (concrete-ci-2)
# instead of the machine that pushes: credentials stay on the pushing machine, the compute moves.
#
#   gates_remote.sh --host H --base <sha> --head <sha> [--src-ref R] [--dst-ref R]
#
# 1. Bundles head and its complete history (a temporary ref, refs/land-remote/<id>, at head) and hands it to the
#    TRANSPORT, which runs scripts/land/run-at-ref.sh on the host: a fresh clone of exactly head,
#    where scripts/land/impact_gates.sh runs the same hook with the same input line the `ci` push will
#    present. The CONCRETE_* variables of this environment are forwarded, so the selection and the
#    record's environment component are the push's.
# 2. Reads the log. The run counts only when run-at-ref printed exactly one start and one exit line
#    and both name HEAD's SHA. A run at any other SHA, a lost connection, an unreachable host or a log
#    without the exit line is NO RESULT.
# 3. On a pass, keeps the log and writes the remote attestation (scripts/checks/lib/prepush_record.sh)
#    so the `ci` push's hook can reuse this run instead of running the gates on this machine. A pass
#    whose host wrote no record still passes the leg, and says that the push will run the gates here.
#
# Exit: 0 passed at head; 1 the gates FAILED at head; 3 INCOMPLETE (no result about head). The
# caller treats 3 as blocking; it never counts as a pass.
#
# Transport: LAND_GATES_TRANSPORT, called as `<transport> <host> <bundle> <ref> <command>`, must
# print run-at-ref.sh's output. The default, gates_transport_ssh.sh, copies the bundle to the host
# and pipes this checkout's run-at-ref.sh through ssh. The landing gate passes a fake.
set -uo pipefail
SELF_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
incomplete() { echo "gates-remote: INCOMPLETE: $*" >&2; exit 3; }
host="" base="" head="" src="refs/heads/impact" dst="refs/heads/ci/impact"
while [ $# -gt 0 ]; do
  case "$1" in
    --host) host="$2"; shift ;;
    --base) base="$2"; shift ;;
    --head) head="$2"; shift ;;
    --src-ref) src="$2"; shift ;;
    --dst-ref) dst="$2"; shift ;;
    *) incomplete "unknown argument $1" ;;
  esac; shift
done
[ -n "$host" ] && [ -n "$base" ] && [ -n "$head" ] || incomplete "--host, --base and --head are required"
case "$dst" in refs/heads/wip/*) incomplete "--dst-ref $dst is in the hook's wip tier, which runs no gates" ;; esac
ROOT="$(git rev-parse --show-toplevel)" || incomplete "not in a git checkout"
cd "$ROOT" || incomplete "cannot enter $ROOT"
head="$(git rev-parse --verify "$head^{commit}" 2>/dev/null)" || incomplete "--head is not a commit"
base="$(git rev-parse --verify "$base^{commit}" 2>/dev/null)" || incomplete "--base is not a commit"
[ "$(git rev-parse HEAD)" = "$head" ] || incomplete "the checkout is not at --head ${head:0:10}"
# The record library of the checkout this script belongs to (the train checkout, in practice).
# shellcheck source=scripts/checks/lib/prepush_record.sh
source "$SELF_DIR/../checks/lib/prepush_record.sh" 2>/dev/null && ts_require 2>/dev/null \
  || incomplete "scripts/checks/lib/prepush_record.sh did not load"
TRANSPORT="${LAND_GATES_TRANSPORT:-$SELF_DIR/gates_transport_ssh.sh}"
[ -x "$TRANSPORT" ] || [ -f "$TRANSPORT" ] || incomplete "no transport at $TRANSPORT"

TMPD="$(mktemp -d)" || incomplete "cannot create a temp directory"
id="$(date -u +%Y%m%dT%H%M%S)-$$-$RANDOM"
ref="refs/land-remote/$id"
cleanup() { git update-ref -d "$ref" 2>/dev/null || true; rm -rf "$TMPD"; }
trap cleanup EXIT
# This run replaces any earlier attestation: whatever it ends in, no older result stands beside it.
rm -f "$(ppr_remote_path "$ROOT")" "$(ppr_remote_log_path "$ROOT")" 2>/dev/null || true
git update-ref "$ref" "$head" || incomplete "cannot create $ref"
# No receiver inventory is negotiated by this transport. Include all ancestors rather than
# assuming its clone has base (or every parent of a merged candidate).
git bundle create "$TMPD/candidate.bundle" "$ref" >/dev/null 2>&1 \
  || incomplete "git bundle create failed"

# The command run in the host's clone. It first clears the gate bypass and the hook's re-entry
# bookkeeping, whatever the host's environment holds: a hook that skips its gates exits 0, and a
# skipped run must never read as a pass. Then this environment's CONCRETE_* variables are forwarded:
# NUL-delimited (a value may hold newlines), names validated, values quoted with %q.
fwd=""
while IFS= read -r -d '' kv; do
  k="${kv%%=*}"; v="${kv#*=}"
  [[ "$k" =~ ^CONCRETE_[A-Za-z0-9_]+$ ]] || continue
  case "$k" in CONCRETE_SKIP_GATES|CONCRETE_GATES_SHELL|CONCRETE_PREPUSH_REENTERED) continue ;; esac
  fwd="$fwd export $k=$(printf '%q' "$v");"
done < <(env -0 | LC_ALL=C sort -z)
q() { printf '%q' "$1"; }
cmd="unset CONCRETE_SKIP_GATES CONCRETE_PREPUSH_REENTERED;$fwd bash scripts/land/impact_gates.sh --base $(q "$base") --head $(q "$head") --src-ref $(q "$src") --dst-ref $(q "$dst")"
cmd="$cmd && { r=\"\$(git rev-parse --git-path concrete-prepush-record)\"; if [ -f \"\$r\" ]; then echo $(q "$PPR_REMOTE_BEGIN"); cat \"\$r\"; echo $(q "$PPR_REMOTE_END"); else echo '=== concrete-remote-record-none'; fi; }"

LOG="$TMPD/remote.log"
echo "gates-remote: running the impact gates for ${base:0:10}..${head:0:10} on $host"
rc=0
"$TRANSPORT" "$host" "$TMPD/candidate.bundle" "$ref" "$cmd" > "$LOG" 2>&1 || rc=$?
cat "$LOG"

# The verdict comes from the log, bound to the SHA, never from the transport's exit status alone.
starts="$(grep -c '^\[concrete-remote\] ref=' "$LOG")"
exits="$(grep -c '^\[concrete-remote\] exit=' "$LOG")"
[ "$starts" = 1 ] && [ "$exits" = 1 ] \
  || incomplete "the host's log has $starts start and $exits exit line(s) (transport exit $rc); no result about ${head:0:10}"
grep -qE "^\[concrete-remote\] ref=[^ ]+ sha=$head( |\$)" "$LOG" \
  || incomplete "the host ran another commit than ${head:0:10}: $(grep '^\[concrete-remote\] ref=' "$LOG")"
grep -qE "^\[concrete-remote\] exit=[0-9]+ .*sha=$head\$" "$LOG" \
  || incomplete "the host's exit line does not name ${head:0:10}: $(grep '^\[concrete-remote\] exit=' "$LOG")"
if ! grep -qE "^\[concrete-remote\] exit=0 " "$LOG"; then
  echo "gates-remote: the impact gates FAILED on $host at ${head:0:10}" >&2
  exit 1
fi
if ppr_remote_attest "$ROOT" "$LOG" "$host" "$head"; then
  echo "gates-remote: passed on $host at ${head:0:10}; attestation $(ppr_remote_path "$ROOT") (the ci push reuses it)"
else
  echo "gates-remote: passed on $host at ${head:0:10}, but no attestation ($PPR_REASON): the ci push will run the gates on this machine"
fi
echo "impact: gates passed for ${base:0:10}..${head:0:10} on $host"
exit 0
