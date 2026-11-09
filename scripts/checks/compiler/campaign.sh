# Gate C lifecycle and nested outcomes. Orchestration stays in the shared scheduler.
work=$(mktemp -d)
BOUNDED_PARENT=$work
source "$root/scripts/checks/lib/bounded.sh"
CAMPAIGN_INCOMPLETE=0
campaign_outcome() {
  [ "$1" != INCOMPLETE ] || CAMPAIGN_INCOMPLETE=1
  if [ -n "${SCHEDULED_OUTCOME_HELPER:-}" ]; then
    python3 -I "$SCHEDULED_OUTCOME_HELPER" "$1" "$2"
  fi
}
campaign_finish() {
  local rc=$?
  trap - EXIT
  bounded_summary || true
  if [ "$CAMPAIGN_INCOMPLETE" = 1 ] || grep -Eqs '^(incomplete|runner-failed)' "$BOUNDED_LOG"; then
    campaign_outcome INCOMPLETE 'Gate C nested watchdog incomplete'
    rc=125
  elif [ "$rc" != 0 ] && [ "${CAMPAIGN_EXPECT_FAILURE:-0}" != 1 ]; then
    campaign_outcome FAIL "Gate C exited $rc"
  fi
  if [ -n "${report:-}" ] && [ -d "$report" ]; then
    mkdir -p "$report/diagnostics"
    # Preserve raw observations and setup/control logs even on a failed campaign.
    for file in "$work"/*.log "$work"/*.tsv "$work"/manifest.txt; do
      [ ! -f "$file" ] || cp "$file" "$report/diagnostics/"
    done
  fi
  if declare -F heavy_slot_release >/dev/null; then heavy_slot_release; fi
  rm -rf "${work:?}"
  exit "$rc"
}
trap campaign_finish EXIT
[ "$BOUNDED_HAVE_WATCHDOG" = 1 ] || { echo 'Gate C requires python3 bounded runs' >&2; exit 2; }
bounded() {
  local secs=$1 rc=0
  shift
  bounded_run -i /dev/null "$secs" -- "$@" || rc=$?
  if bounded_is_incomplete "$rc"; then
    campaign_outcome INCOMPLETE "Gate C timed out: $*"
    echo "INCOMPLETE Gate C: timed out twice: $*" >&2
    exit 125
  elif [ "$rc" != 0 ] && [ "${CAMPAIGN_EXPECT_FAILURE:-0}" != 1 ]; then
    campaign_outcome FAIL "Gate C command exited $rc: $*"
  fi
  return "$rc"
}
campaign_identity() {
  mkdir -p "$1"
  printf '%s\n' "$2" >"$1/sha.txt"
  printf 'sha=%s\ntier=%s\nattempt=%s\nrepository=%s\nrunner_environment=%s\nrunner_name=%s\n' \
    "$2" "${SELECTED_TIER:-qualification}" "${SCHEDULED_ATTEMPT_ID:-local}" \
    "${GITHUB_REPOSITORY:-local}" "${RUNNER_ENVIRONMENT:-local}" "${RUNNER_NAME:-local}" >"$1/provenance.txt"
}
