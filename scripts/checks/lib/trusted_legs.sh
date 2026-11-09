# Independent legs retain errexit internally; waiting collects every result.
# Do not call the leg in an if/|| context: Bash would disable its errexit.
trusted_legs_status=0
trusted_leg() {
  local name=$1 pid rc=0
  shift
  ( trap - EXIT; set -euo pipefail; "$@" ) &
  pid=$!
  wait "$pid" || rc=$?
  case "$rc" in
    0) echo "PASS trusted-base leg: $name" ;;
    2|124|137) echo "INCOMPLETE trusted-base leg: $name (exit $rc)"
       trusted_legs_status=1 ;;
    *) echo "FAIL trusted-base leg: $name (exit $rc)"
       trusted_legs_status=1 ;;
  esac
}
