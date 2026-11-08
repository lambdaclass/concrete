set -euo pipefail
work=$(mktemp -d -p "$1")
bounded_summary() { echo "summary"; }
BOUNDED_LOG=$work/log
eval "$(sed -n '/^trap .rc=/,/EXIT$/p' /home/unbalancedparen/projects/slice0/scripts/checks/lib/slice0_common.sh)"
[ "$2" = inc ] && echo "incomplete x" > "$BOUNDED_LOG"
[ "$2" = fail ] && false
echo body-done
