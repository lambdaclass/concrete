#!/usr/bin/env bash
# concrete-queue push guard: the pre-push hook of the queue's clone (install-queue.sh points
# core.hooksPath at a copy of this file; dispatch.py refuses to start agents without it).
#
# Queue agents publish only work in progress and readiness notes. Any other target is refused:
#   allowed   refs/heads/wip/*            (a task's heartbeat and its candidate)
#             refs/notes/concrete-review, refs/notes/concrete-gates   (readiness notes)
#   refused   main, ci/*, every other ref, and any remote named `fork`
# Landing is the landing bot's job, from its own clone. After the guard, the repository's own
# pre-push hook (.githooks/pre-push: its wip tier) runs with the same input.
set -uo pipefail
remote="${1:-}"
if [ "$remote" = fork ]; then
  echo "queue push guard: refusing the remote named 'fork'" >&2; exit 1
fi
input="$(cat)"
while read -r _lref _lsha rref _rsha; do
  [ -n "${rref:-}" ] || continue
  case "$rref" in
    refs/heads/wip/*|refs/notes/concrete-review|refs/notes/concrete-gates) ;;
    *) echo "queue push guard: refusing to push $rref (agents push refs/heads/wip/* and readiness notes only)" >&2
       exit 1 ;;
  esac
done <<< "$input"
top="$(git rev-parse --show-toplevel 2>/dev/null || true)"
if [ -n "$top" ] && [ -x "$top/.githooks/pre-push" ]; then
  printf '%s\n' "$input" | "$top/.githooks/pre-push" "$@"
  exit $?
fi
exit 0
