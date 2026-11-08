#!/usr/bin/env bash
# ISS-0030: cheap PR regressions and a real weekly scaling/stack-depth tier.
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/../../.."
case "${1:-}" in
  "") args=() ;;
  --full) args=(--full) ;;
  *) echo "lcb_numbering: expected no argument or --full" >&2; exit 2 ;;
esac
[ "$#" -le 1 ] || { echo "lcb_numbering: too many arguments" >&2; exit 2; }
work=$(mktemp -d)
BOUNDED_PARENT="$work"
source scripts/checks/lib/bounded.sh
finish() {
  local rc=$?
  bounded_summary || true
  if grep -Eq '^(incomplete|runner-failed)' "$BOUNDED_LOG"; then rc=3; fi
  rm -rf "$work"
  exit "$rc"
}
trap finish EXIT
[ "$BOUNDED_HAVE_WATCHDOG" = 1 ] || { echo "lcb_numbering: python3 watchdog required" >&2; exit 2; }
printf 'ISS-0030 sha=%s tier=%s\n' "$(git rev-parse HEAD)" "${1:-pr}"
bounded_run 600 -- lake build Concrete.Backend.LCB Concrete.Backend.LLVM
bounded_run 120 -- lake env lean --run scripts/checks/compiler/lcb_numbering_cases.lean "${args[@]}"
