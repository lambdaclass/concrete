#!/usr/bin/env bash
# Mark a pushed branch ready to land (docs/project/LANDING.md, "Readiness").
#
#   ready.sh [--dry-run] [--no-review] [--no-remote] [--base <ref>] <branch>
#
# Run in a clean checkout at the branch head, after pushing it (origin/<branch> must be HEAD). Three
# independent legs run AT THE SAME TIME, and every one is collected before the verdict:
#   review  LAND_REVIEW_CMD or scripts/review/review.sh, with --base --head (it writes the review note)
#   impact  scripts/land/impact_gates.sh over merge-base(<base>, HEAD)..HEAD
#   remote  the full gate set via LAND_REMOTE_RUNNER (default ~/bin/concrete-remote) at <branch>
# Then the gates note is published at HEAD: `gates: pass` only when impact and remote (if run)
# passed. Exit 0 iff the head is ready (review note clean, gates pass). The wall time and the
# sequential sum of the legs go to <git-common-dir>/concrete-landing/ready.tsv.
set -euo pipefail
SELF_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=scripts/land/lib.sh
. "$SELF_DIR/lib.sh"
die() { echo "ready: $*" >&2; exit 1; }
DRY=0 review=1 remote=1 baseref="" branch=""
while [ $# -gt 0 ]; do
  case "$1" in
    --dry-run) DRY=1 ;;
    --no-review) review=0 ;;
    --no-remote) remote=0 ;;
    --base) baseref="$2"; shift ;;
    -*) die "unknown option $1" ;;
    *) branch="$1" ;;
  esac; shift
done
[ -n "$branch" ] || die "name the pushed branch (e.g. wip/my-fix)"
cd "$(git rev-parse --show-toplevel)"
REMOTE="${LAND_REMOTE:-origin}"
[ "$REMOTE" != fork ] || die "refusing the remote named 'fork'"
[ -z "$(git status --porcelain --untracked-files=no)" ] || die "the work tree has uncommitted changes"
land_assert_no_op start
git fetch -q "$REMOTE"
head="$(git rev-parse HEAD)"
pushed="$(git rev-parse -q --verify "refs/remotes/$REMOTE/$branch" || true)"
[ "$pushed" = "$head" ] || die "$REMOTE/$branch is ${pushed:0:10}, not HEAD ${head:0:10}; push first (readiness is bound to a published SHA)"
base="$(git merge-base "${baseref:-$REMOTE/main}" "$head")"

rcmd="${LAND_REVIEW_CMD:-}"
if [ -z "$rcmd" ] && [ -f scripts/review/review.sh ]; then rcmd="bash scripts/review/review.sh"; fi
[ -n "$rcmd" ] || review=0
runner="${LAND_REMOTE_RUNNER-$HOME/bin/concrete-remote}"
{ [ -n "$runner" ] && [ -x "$runner" ]; } || remote=0
full="${LAND_FULL_GATES_CMD:-lake build && bash scripts/tests/run_ci_gates_local.sh}"

if [ "$DRY" = 1 ]; then
  echo "ready (dry-run): ${base:0:10}..${head:0:10} of $branch, in parallel:"
  echo "    impact: $SELF_DIR/impact_gates.sh --base $base --head $head"
  [ "$remote" = 1 ] && echo "    remote: $runner $branch '$full'"
  [ "$review" = 1 ] && echo "    review: $rcmd --base $base --head $head"
  echo "    then: notes.sh put gates $head gates=<pass|fail> selected=<n>"
  exit 0
fi

COMMON="$(cd "$(git rev-parse --git-common-dir)" && pwd)"
dir="$COMMON/concrete-landing/ready/${head:0:12}"; mkdir -p "$dir"
remote_full() { # the run must report this exact SHA
  local rc=0 out
  out="$("$runner" "$branch" "$full" 2>&1)" || rc=$?
  printf '%s\n' "$out"
  case "$out" in *"sha=$head"*) ;; *) echo "remote: the runner did not report running ${head:0:10}"; return 1 ;; esac
  return "$rc"
}
t0="$(date +%s)"
legs_start impact "$dir/impact.log" bash "$SELF_DIR/impact_gates.sh" --base "$base" --head "$head" \
  --src-ref "refs/heads/$branch" --dst-ref "refs/heads/ready-check" --report "$dir/selected"
[ "$remote" = 1 ] && legs_start remote "$dir/remote.log" remote_full
[ "$review" = 1 ] && legs_start review "$dir/review.log" bash -c "$rcmd --base $base --head $head"
legs_wait
wall=$(( $(date +%s) - t0 ))
for leg in $LEGS_FAILED; do echo "ready: $leg leg FAILED; tail of $dir/$leg.log:"; tail -n 30 "$dir/$leg.log" || true; done

gates=pass
case " $LEGS_FAILED " in *" impact "*|*" remote "*) gates=fail ;; esac
selected="$(sed -n 's/^selected: //p' "$dir/selected" 2>/dev/null || true)"
bash "$SELF_DIR/notes.sh" put gates "$head" gates="$gates" selected="${selected:-unknown}" \
  remote="$([ "$remote" = 1 ] && { case " $LEGS_FAILED " in *" remote "*) echo fail ;; *) echo pass ;; esac; } || echo skipped)" \
  runner=ready.sh
bash "$SELF_DIR/notes.sh" fetch
verdict="$(bash "$SELF_DIR/notes.sh" get review "$head" verdict)"

printf '%s\t%s\t%s\t%s\t%s\t%s\n' "$(date -u +%Y-%m-%dT%H:%M:%SZ)" "$branch" "$head" "$LEGS_REPORT" "$wall" "$LEGS_SUM" \
  >> "$COMMON/concrete-landing/ready.tsv"
echo "ready: legs $LEGS_REPORT"
echo "ready: wall ${wall}s; run one after another they take ${LEGS_SUM}s (saved $(( LEGS_SUM - wall ))s)"
echo "ready: ${head:0:10} gates=$gates review=${verdict:-none}"
[ "$gates" = pass ] && [ "$verdict" = clean ] && [ -z "$LEGS_FAILED" ]
