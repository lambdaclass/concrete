#!/usr/bin/env bash
# Landing trains (ROADMAP R-0539): integrate every ready branch into ONE candidate, validate it once,
# and land it on both remotes. Stage timestamps go to the landing log (R-0547). docs/project/LANDING.md
# is the manual; this header is the contract.
#
#   train.sh [--dry-run] build  <name> [--force] [--no-fetch] <ref>...
#   train.sh [--dry-run] check  <name> [--no-remote]
#   train.sh [--dry-run] ci     <name> [--skip-check]
#   train.sh             watch  <name> [--interval S] [--timeout S]
#   train.sh [--dry-run] land   <name> [--no-mirror] [--keep-main-ci]
#   train.sh             bisect <name> (--test '<cmd>' | --ci)
#   train.sh             status <name>
#
# SAFETY RULES, each written after a mistake made by hand on 2026-10-22:
#   * after EVERY git operation the script checks that no merge, rebase, cherry-pick or revert is in
#     progress. A rebase piped to `tail` kept going and a commit was made mid-rebase; here a stopped
#     operation is a stop, never a warning.
#   * only generated views and append-only ledgers are resolved mechanically
#     (scripts/land/resolve_conflicts.py); any other conflict aborts the merge and names the files.
#   * a train lands only when the CI run's head SHA is the local HEAD, every landing-tier job
#     succeeded, and origin/main is still the train's base. A moved main means rebuild and re-run:
#     a result is bound to the exact candidate that ran.
#   * pushes name the exact SHA, never `HEAD` (push-both.sh: HEAD can move during a wait), and a
#     remote named `fork` is refused outright.
#   * (2026-10-25) a push's exit status is not its result: GitHub drops the connection during a long
#     hook, so a push can fail after the remote moved, or before. The train's push to ci/<name> and
#     both pushes of `land` are followed by an `ls-remote` of the ref, and one that did not move it
#     is retried (LAND_PUSH_ATTEMPTS). The ci/<name> reset to the base skips the hook; not retried.
#
# ENVIRONMENT (test seams and overrides): LAND_REPO (owner/name for gh, default
# unbalancedparentheses/concrete2), LAND_GH (gh binary), LAND_REMOTE (primary, default origin),
# LAND_MIRROR (default lambdaclass, required: a missing remote is refused; empty or --no-mirror skips),
# LAND_STATE_DIR (default <git common dir>/concrete-landing), LAND_LOG_DIR (default
# $LAND_STATE_DIR/log), LAND_NO_NIX=1 (never re-enter `nix develop`), LAND_CAMPAIGN_JOBS (regex of
# campaign-tier job names), LAND_HOOK (pre-push hook path, default .githooks/pre-push),
# LAND_PUSH_ATTEMPTS (default 3) and LAND_PUSH_RETRY_SLEEP (seconds, default 15) for verified pushes,
# LAND_KEEP_MAIN_CI=1 (same as land --keep-main-ci), LAND_DUP_POLL (default 6) and
# LAND_DUP_POLL_SLEEP (seconds, default 5) for finding main's duplicate run.
set -euo pipefail

SELF_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
RESOLVER="$SELF_DIR/resolve_conflicts.py"
# shellcheck source=scripts/land/lib.sh
. "$SELF_DIR/lib.sh"
DRY=0
if [ "${1:-}" = "--dry-run" ]; then DRY=1; shift; fi

die() { echo "train: $*" >&2; exit 1; }
note() { echo "train: $*"; }

ROOT="$(git rev-parse --show-toplevel 2>/dev/null)" || die "not inside a git work tree"
cd "$ROOT"
COMMON="$(cd "$(git rev-parse --git-common-dir)" && pwd)"
STATE_ROOT="${LAND_STATE_DIR:-$COMMON/concrete-landing}"
LOG_DIR="${LAND_LOG_DIR:-$STATE_ROOT/log}"
REPO="${LAND_REPO:-unbalancedparentheses/concrete2}"
GH="${LAND_GH:-gh}"
REMOTE="${LAND_REMOTE:-origin}"
MIRROR="${LAND_MIRROR-lambdaclass}"
HOOK="${LAND_HOOK:-.githooks/pre-push}"
CAMPAIGN_RE="${LAND_CAMPAIGN_JOBS:-^(Gate mutation|Weekly clean-checkout proof replay|Nightly differential fuzz)}"

# --- invariants ----------------------------------------------------------------------------------

# No git operation may be left half-done. Called after every git command that can stop.
assert_no_op() {
  local what="$1" p
  for p in MERGE_HEAD CHERRY_PICK_HEAD REVERT_HEAD rebase-merge rebase-apply; do
    if [ -e "$(git rev-parse --git-path "$p")" ]; then
      die "after $what: a git operation is still in progress ($p exists). Stopping; resolve or abort it by hand."
    fi
  done
}

assert_clean() {
  if [ -n "$(git status --porcelain --untracked-files=no)" ]; then
    git status --short --untracked-files=no >&2
    die "the work tree has uncommitted changes to tracked files; commit or move them first"
  fi
}

refuse_fork() {
  case "$1" in
    fork) die "refusing to push to the remote named 'fork' (it is a divergent repository)" ;;
  esac
}

valid_name() {
  case "$1" in
    ""|*[!A-Za-z0-9._-]*|.*|*..*) die "train name '$1' must be [A-Za-z0-9._-]+ and not start with '.'" ;;
  esac
}

# --- state and log -------------------------------------------------------------------------------
# State and log live in the COMMON git dir: every worktree sees the same trains, nothing is committed,
# and no per-train file can conflict on a rebase (LANDING.md, "Where the record lives").

state_file() { echo "$STATE_ROOT/$1/train.env"; }

state_get() { # name key
  local f; f="$(state_file "$1")"
  [ -f "$f" ] || die "no train '$1' (build it first: train.sh build $1 <ref>...)"
  sed -n "s/^$2=//p" "$f" | tail -1
}

state_set() { # name key value
  local f; f="$(state_file "$1")"
  mkdir -p "$(dirname "$f")"
  touch "$f"
  grep -v "^$2=" "$f" > "$f.tmp" || true
  printf '%s=%s\n' "$2" "$3" >> "$f.tmp"
  mv "$f.tmp" "$f"
}

STAGE_T0=0
stage_start() { STAGE_T0="$(date +%s)"; }

# utc, train, stage, base, head, members (ref@sha, comma-joined), run id, result, duration_s, note
log_row() { # name stage result note
  [ "$DRY" = 1 ] && return 0
  local name="$1" stage="$2" result="$3" notes="${4:-}" now dur base head members run
  now="$(date +%s)"; dur=$(( now - STAGE_T0 ))
  base="$(state_get_soft "$name" base)"; head="$(state_get_soft "$name" head)"
  members="$(state_get_soft "$name" members)"; run="$(state_get_soft "$name" run_id)"
  mkdir -p "$LOG_DIR"
  local f="$LOG_DIR/$name.tsv"
  [ -f "$f" ] || printf 'utc\ttrain\tstage\tbase\thead\tmembers\trun_id\tresult\tduration_s\tnote\n' > "$f"
  printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
    "$(date -u +%Y-%m-%dT%H:%M:%SZ)" "$name" "$stage" "${base:--}" "${head:--}" "${members:--}" \
    "${run:--}" "$result" "$dur" "$(printf '%s' "${notes:--}" | tr '\t\n' '  ')" >> "$f"
}

state_get_soft() {
  local f; f="$(state_file "$1")"
  [ -f "$f" ] || return 0
  sed -n "s/^$2=//p" "$f" | tail -1
}

# --- toolchain -----------------------------------------------------------------------------------
# The pre-push hook builds the compiler, so pushes and checks run inside the dev shell.
in_nix() { land_in_nix "$@"; }   # scripts/land/lib.sh

run_or_print() { # cmd... — executes, or prints under --dry-run
  if [ "$DRY" = 1 ]; then printf 'train (dry-run): would run:'; printf ' %q' "$@"; echo; return 0; fi
  "$@"
}

remote_main() { git ls-remote "$REMOTE" refs/heads/main | cut -f1; }

# A push whose result is read from the remote, not from git's exit status. GitHub drops the
# connection during a long pre-push hook: the push can fail AFTER the remote ref moved (then it is
# done) or before (then it is retried). Each attempt is a plain push of the exact SHA, so a retry
# to main is still a fast-forward or a refusal, and the hook's retry reuses the validation record
# of the same range. A ref that moved to anything else stops at once. Prints nothing under --dry-run
# except the command.
push_verified() { # remote sha ref [env assignments...]
  local remote="$1" sha="$2" ref="$3"; shift 3
  if [ "$DRY" = 1 ]; then run_or_print env "$@" git push "$remote" "$sha:refs/heads/$ref"; return 0; fi
  local before now attempt=1 max="${LAND_PUSH_ATTEMPTS:-3}" rc
  before="$(git ls-remote "$remote" "refs/heads/$ref" | cut -f1)"
  while :; do
    rc=0
    ( for kv in "$@"; do export "${kv?}"; done; in_nix git push "$remote" "$sha:refs/heads/$ref" ) || rc=$?
    assert_no_op "push to $remote"
    now="$(git ls-remote "$remote" "refs/heads/$ref" | cut -f1)"
    if [ "$now" = "$sha" ]; then
      [ "$rc" = 0 ] || note "push to $remote $ref exited $rc, but $remote/$ref is at ${sha:0:10}: the connection dropped after the update"
      return 0
    fi
    [ "$now" = "$before" ] || die "push to $remote $ref: the ref moved to ${now:0:10}, neither its old value nor ${sha:0:10}; not retrying"
    [ "$attempt" -lt "$max" ] || die "push to $remote $ref failed $attempt time(s) (last exit $rc); $remote/$ref is still ${before:0:10}"
    note "push to $remote $ref exited $rc and the ref did not move; retrying ($attempt of $max)"
    attempt=$((attempt + 1)); sleep "${LAND_PUSH_RETRY_SLEEP:-15}"
  done
}

# After a landing, the push to main starts a CI run at the same SHA that the train's dispatched run
# already validated (same workflow, same commit; the push event runs no job the dispatch skips).
# It is cancelled so a landing costs one CI run, not two; the train's run stays main's record for
# that SHA. Only an unfinished push-event run whose headSha is exactly the landed head is touched.
# Not finding one, or failing to cancel it, is a note: the landing is already complete.
cancel_duplicate_main_run() { # head validating-run-id
  local head="$1" vid="$2" ids="" i
  for i in $(seq 1 "${LAND_DUP_POLL:-6}"); do
    ids="$("$GH" run list -R "$REPO" --workflow CI --branch main --event push -L 20 \
             --json databaseId,headSha,status 2>/dev/null | python3 -I -c '
import json, sys
try:
    runs = json.load(sys.stdin)
except ValueError:
    runs = []
print(" ".join(str(r["databaseId"]) for r in runs
               if r.get("headSha") == sys.argv[1] and r.get("status") != "completed"
               and str(r.get("databaseId")) != sys.argv[2]))' "$head" "$vid")" || ids=""
    [ -n "$ids" ] && break
    sleep "${LAND_DUP_POLL_SLEEP:-5}"
  done
  if [ -z "$ids" ]; then
    note "no unfinished push run of CI on main at ${head:0:10} found; nothing to cancel"
    return 0
  fi
  local id done_ids=""
  for id in $ids; do
    if "$GH" run cancel "$id" -R "$REPO" >/dev/null 2>&1; then done_ids="${done_ids:+$done_ids }$id"
    else note "could not cancel main's run $id (left running)"; fi
  done
  [ -n "$done_ids" ] && note "cancelled main's duplicate CI run(s) $done_ids at ${head:0:10}; run $vid validated that SHA"
  log_row "$LAND_NAME" land-dedupe "$([ -n "$done_ids" ] && echo passed || echo failed)" "cancelled: ${done_ids:-none}; validated by run $vid"
  return 0
}

# The train's state must describe the checkout: same branch, same HEAD, clean.
require_train_checkout() { # name
  local name="$1" head
  head="$(state_get "$name" head)"
  [ "$(git symbolic-ref -q --short HEAD || true)" = "train/$name" ] \
    || die "check out train/$name first (state is bound to that branch)"
  [ "$(git rev-parse HEAD)" = "$head" ] \
    || die "HEAD $(git rev-parse --short HEAD) is not the built train ${head:0:10}; rebuild (build --force) rather than editing a train"
  assert_clean
  assert_no_op "state check"
}

# --- build ---------------------------------------------------------------------------------------

# The generators regenerate() runs on the current tree, colon-separated. The resolver may take one
# side of a generated view only when that view's generator is in this plan (LAND_REGEN_PLAN).
regen_plan() {
  local plan="" g
  if [ -f scripts/gen/refresh_generated.sh ]; then
    plan="scripts/gen/refresh_generated.sh"
    # refresh_generated.sh runs bug_ledger.py always, and the other three when present.
    for g in scripts/gen/bug_ledger.py scripts/gen/bug_dashboard.py scripts/gen/candidate_table.py scripts/gen/build_identity.sh; do
      [ -f "$g" ] && plan="$plan:$g"
    done
  else
    for g in scripts/gen/bug_dashboard.py scripts/gen/candidate_table.py scripts/gen/build_identity.sh; do
      [ -f "$g" ] && plan="${plan:+$plan:}$g"
    done
  fi
  echo "$plan"
}

regenerate() { # each generator's failure is the build's failure
  if [ -f scripts/gen/refresh_generated.sh ]; then
    bash scripts/gen/refresh_generated.sh || return 1
  else
    if [ -f scripts/gen/bug_dashboard.py ]; then python3 -I scripts/gen/bug_dashboard.py --write || return 1; fi
    if [ -f scripts/gen/candidate_table.py ]; then python3 -I scripts/gen/candidate_table.py --write || return 1; fi
    if [ -f scripts/gen/build_identity.sh ]; then bash scripts/gen/build_identity.sh || return 1; fi
  fi
  return 0
}

markers_in() { # paths... — prints the files that still hold conflict markers
  local f
  for f in "$@"; do
    [ -f "$f" ] || continue
    if grep -qE '^(<{7}|>{7}|\|{7})( |$)|^={7}$' "$f"; then echo "$f"; fi
  done
  return 0
}

cmd_build() {
  local name="" force=0 fetch=1 refs=()
  while [ $# -gt 0 ]; do
    case "$1" in
      --force) force=1 ;;
      --no-fetch) fetch=0 ;;
      -*) die "build: unknown option $1" ;;
      *) if [ -z "$name" ]; then name="$1"; else refs+=("$1"); fi ;;
    esac
    shift
  done
  valid_name "$name"
  [ "${#refs[@]}" -ge 1 ] || die "build: name at least one ref to merge"
  stage_start
  assert_no_op "start"
  assert_clean
  if [ "$fetch" = 1 ]; then git fetch -q "$REMOTE"; assert_no_op "fetch"; fi
  local base; base="$(git rev-parse --verify "$REMOTE/main^{commit}")"

  local r sha members="" shas=()
  for r in "${refs[@]}"; do
    sha="$(git rev-parse --verify -q "$r^{commit}")" || die "build: '$r' does not name a commit"
    if git merge-base --is-ancestor "$sha" "$base"; then
      note "skip $r (${sha:0:10}): already contained in $REMOTE/main"
      continue
    fi
    shas+=("$r@$sha"); members="${members:+$members,}$r@$sha"
  done
  [ "${#shas[@]}" -ge 1 ] || die "build: every ref is already in $REMOTE/main; nothing to land"

  if [ "$DRY" = 1 ]; then
    note "(dry-run) would create train/$name at ${base:0:10} ($REMOTE/main) and merge, in order:"
    printf '    %s\n' "${shas[@]}"
    return 0
  fi

  if git show-ref -q --verify "refs/heads/train/$name"; then
    [ "$force" = 1 ] || die "build: train/$name exists; pass --force to rebuild it from $REMOTE/main"
  fi
  local prev; prev="$(git symbolic-ref -q --short HEAD || git rev-parse HEAD)"
  git checkout -q -B "train/$name" "$base"
  assert_no_op "checkout"
  rm -f "$(state_file "$name")"
  state_set "$name" base "$base"
  state_set "$name" members "$members"
  state_set "$name" prev "$prev"

  local m ref msg rc conflicted plan used_plan=""
  for m in "${shas[@]}"; do
    ref="${m%@*}"; sha="${m##*@}"
    msg="train/$name: merge $ref (${sha:0:10})"
    note "merging $ref (${sha:0:10})"
    rc=0; git merge -q --no-ff --no-edit -m "$msg" "$sha" || rc=$?
    if [ "$rc" -ne 0 ]; then
      if [ ! -e "$(git rev-parse --git-path MERGE_HEAD)" ]; then
        assert_no_op "merge of $ref"
        die "build: merging $ref failed before any conflict (exit $rc; see above). Nothing was committed."
      fi
      conflicted="$(git diff --name-only --diff-filter=U)"
      note "conflicts in $ref:"; printf '    %s\n' $conflicted
      plan="$(regen_plan)"; used_plan="${used_plan:+$used_plan:}$plan"
      # shellcheck disable=SC2086
      if ! LAND_REGEN_PLAN="$plan" python3 -I "$RESOLVER" resolve $conflicted; then
        git merge --abort
        assert_no_op "merge --abort"
        log_row "$name" build conflict "real conflict merging $ref: $(printf '%s ' $conflicted)"
        echo "train: STOPPED — $ref conflicts with the train outside the generated views and ledgers." >&2
        echo "train: the merge was aborted; train/$name holds every member merged before $ref." >&2
        echo "train: rebase $ref onto $REMOTE/main (or the other member), or drop it from the train." >&2
        exit 3
      fi
      [ -z "$(git diff --name-only --diff-filter=U)" ] || die "build: unmerged paths remain after resolution"
      local left; left="$(markers_in $conflicted)"
      [ -z "$left" ] || { git merge --abort; die "build: conflict markers remain in: $left"; }
      git commit -q --no-edit
    fi
    assert_no_op "merge of $ref"
    if [ -f scripts/tests/audit_bug_corpus.sh ]; then
      python3 -I "$RESOLVER" corpus-keys scripts/tests/audit_bug_corpus.sh \
        || { log_row "$name" build refused "duplicate corpus key after $ref"; \
             die "build: audit_bug_corpus.sh has duplicate keys after merging $ref (above); fix the member, not the train"; }
    fi
  done

  # A view resolved by "take one side, regenerate" must really be regenerated: every generator the
  # resolver relied on is still in the plan that runs now.
  local final_plan g; final_plan="$(regen_plan)"
  for g in ${used_plan//:/ }; do
    case ":$final_plan:" in *":$g:"*) ;; *) die "build: $g resolved a conflict but is gone from the final tree; the view cannot be regenerated" ;; esac
  done
  note "regenerating generated views once on the train (${final_plan:-no generators})"
  if ! regenerate; then
    git checkout -q -- .
    log_row "$name" build refused "a generator refused"
    die "build: a generator refused (above); the train is left at its last merge and is NOT built"
  fi
  if [ -n "$(git status --porcelain --untracked-files=no)" ]; then
    local changed; changed="$(git status --porcelain --untracked-files=no | cut -c4-)"
    local left; left="$(markers_in $changed)"
    [ -z "$left" ] || die "build: conflict markers remain after regeneration in: $left"
    git add -u
    git commit -q -m "train/$name: regenerate generated views ($(printf '%s ' $changed | sed 's/ $//'))"
    assert_no_op "regeneration commit"
  fi
  local tracked_markers
  tracked_markers="$(git grep -lE '^(<{7}|>{7})( |$)' HEAD -- . 2>/dev/null || true)"
  [ -z "$tracked_markers" ] || die "build: committed conflict markers in: $tracked_markers"

  state_set "$name" head "$(git rev-parse HEAD)"
  state_set "$name" built_utc "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
  log_row "$name" build passed "${#shas[@]} member(s)"
  note "train/$name built at $(git rev-parse --short=10 HEAD) on base ${base:0:10}:"
  printf '    %s\n' "${shas[@]}"
  note "next: train.sh check $name"
}

# --- check ---------------------------------------------------------------------------------------

# Remote full gate set (R-0541). LAND_REMOTE_RUNNER is `<runner> <ref> <command>`: the laptop's
# ~/bin/concrete-remote (the default), or scripts/land/run-at-ref.sh on the server; empty disables.
# The runner must print `sha=<sha>` for the commit it ran; a run of any other SHA is no result.
# With LAND_REMOTE_PUSH=1 (default) the head is first published as wip/land-<name>-<sha>-<pid>-<n>
# (the hook's backup tier) so a runner that fetches from the remote can see it, and deleted
# afterwards; the
# server's local runner fetches from this clone instead (LAND_REMOTE_PUSH=0, ref = the SHA).
remote_runner() {
  local r="${LAND_REMOTE_RUNNER-$HOME/bin/concrete-remote}"
  if [ -n "$r" ] && [ -x "$r" ]; then echo "$r"; fi
  return 0
}

remote_leg() { # name head
  local name="$1" head="$2" runner ref
  runner="$(remote_runner)"
  ref="$head"
  if [ "${LAND_REMOTE_PUSH:-1}" = 1 ]; then
    refuse_fork "$REMOTE"
    # One ref per run, created only if absent (no --force), so concurrent checks never share one.
    # Created only if ABSENT. An empty lease is not enough: git reports a ref that already holds
    # this SHA as "up to date" (success) before it looks at the lease, so ownership is taken only
    # from --porcelain's `*` (new ref) line; `=` is another run's ref and gets a fresh name.
    local try ok=0 out
    for try in 1 2 3; do
      ref="${LAND_REMOTE_REF_FIRST:-wip/land-$name-${head:0:12}-$$-$RANDOM$RANDOM}"; LAND_REMOTE_REF_FIRST=""
      out="$(git push --porcelain --force-with-lease="refs/heads/$ref:" "$REMOTE" "$head:refs/heads/$ref" 2>&1)" || true
      case $'\n'"$out"$'\n' in *$'\n*\t'"$head:refs/heads/$ref"$'\t'*) ok=1; break ;; esac
      echo "remote: refs/heads/$ref was not created by this run (${out//$'\n'/ | }); trying another name"
    done
    [ "$ok" = 1 ] || return 1
    # Removed even when the leg is interrupted. A deletion publishes nothing, so it skips the gates
    # (the hook's wip tier exempts only non-deletions), and the lease removes only this run's ref.
    trap 'CONCRETE_SKIP_GATES=1 git push -q --force-with-lease="refs/heads/'"$ref"':'"$head"'" "'"$REMOTE"'" ":refs/heads/'"$ref"'" 2>/dev/null || true' EXIT
  fi
  local rc=0 out
  out="$("$runner" "$ref" "${LAND_FULL_GATES_CMD:-lake build && bash scripts/tests/run_ci_gates_local.sh}" 2>&1)" || rc=$?
  printf '%s\n' "$out"
  if [ "${LAND_REMOTE_PUSH:-1}" = 1 ]; then
    # The normal-path deletion, logged; the EXIT trap above stays as the backstop for interruptions.
    local dout
    # The backstop is disarmed BEFORE this one attempt, which is never retried: a push that fails
    # may still have deleted the ref, and a later run may have re-created the name at the same SHA,
    # where a retry's lease would match. An unconfirmed deletion is reported and left alone.
    trap - EXIT
    if dout="$(CONCRETE_SKIP_GATES=1 git push --force-with-lease="refs/heads/$ref:$head" "$REMOTE" ":refs/heads/$ref" 2>&1)"; then
      echo "remote: deleted refs/heads/$ref"
    else
      echo "remote: deletion of refs/heads/$ref NOT confirmed; left for a human (not retried): ${dout//$'\n'/ | }"
    fi
  fi
  case "$out" in *"sha=$head"*) ;; *) echo "remote: the runner did not report running ${head:0:10}; no result"; return 1 ;; esac
  return "$rc"
}

# Codex review leg: LAND_REVIEW_CMD, else scripts/review/review.sh, called with --base --head. Its
# exit status is the verdict (0 = clean); a published review note saying `findings` at head also fails.
review_cmd() {
  if [ -n "${LAND_REVIEW_CMD:-}" ]; then echo "$LAND_REVIEW_CMD"
  elif [ -f scripts/review/review.sh ]; then echo "bash scripts/review/review.sh"; fi
  return 0
}

review_leg() { # base head
  local cmd; cmd="$(review_cmd)"
  bash -c "$cmd --base $1 --head $2" || return $?
  bash "$SELF_DIR/notes.sh" fetch >/dev/null 2>&1 || true
  [ "$(bash "$SELF_DIR/notes.sh" get review "$2" verdict 2>/dev/null)" != findings ]
}

cmd_check() {
  local name="" remote=1 review=1
  while [ $# -gt 0 ]; do
    case "$1" in
      --no-remote) remote=0 ;;
      --no-review) review=0 ;;
      -*) die "check: unknown option $1" ;;
      *) name="$1" ;;
    esac; shift
  done
  valid_name "$name"
  require_train_checkout "$name"
  stage_start
  local base head; base="$(state_get "$name" base)"; head="$(git rev-parse HEAD)"
  # A new check withdraws the previous one: a failing re-run must not leave an old pass standing.
  [ "$DRY" = 1 ] || state_set "$name" checked ""
  if [ "$remote" = 1 ] && [ -z "$(remote_runner)" ]; then remote=0; fi
  if [ "$review" = 1 ] && [ -z "$(review_cmd)" ]; then review=0; fi

  # The legs are independent, so they run AT THE SAME TIME and are all collected (LANDING.md,
  # "Pipelining"): the impact selection over exactly the range the `ci` push will present
  # (base..head on ci/<name>, so that push reuses the hook's record), the remote full set, and the
  # Codex review. Any failing leg fails the check.
  if [ "$DRY" = 1 ]; then
    note "(dry-run) would run in parallel: impact gates (hook line 'refs/heads/train/$name $head refs/heads/ci/$name $base')"
    if [ "$remote" = 1 ]; then note "(dry-run)   + remote full gates: $(remote_runner) <ref of ${head:0:10}> '${LAND_FULL_GATES_CMD:-lake build && bash scripts/tests/run_ci_gates_local.sh}'"; fi
    if [ "$review" = 1 ]; then note "(dry-run)   + review: $(review_cmd) --base $base --head $head"; fi
    return 0
  fi
  local dir="$STATE_ROOT/$name/check-${head:0:12}"; mkdir -p "$dir"
  local t0; t0="$(date +%s)"
  legs_start impact "$dir/impact.log" bash "$SELF_DIR/impact_gates.sh" --base "$base" --head "$head" \
    --src-ref "refs/heads/train/$name" --dst-ref "refs/heads/ci/$name"
  if [ "$remote" = 1 ]; then legs_start remote "$dir/remote.log" remote_leg "$name" "$head"; fi
  if [ "$review" = 1 ]; then legs_start review "$dir/review.log" review_leg "$base" "$head"; fi
  legs_wait
  local wall=$(( $(date +%s) - t0 )) leg
  cat "$dir/impact.log"
  for leg in $LEGS_FAILED; do
    [ "$leg" = impact ] && continue
    note "$leg leg FAILED at ${head:0:10}; tail of $dir/$leg.log:"; tail -n 40 "$dir/$leg.log" || true
  done
  note "legs: $LEGS_REPORT; wall ${wall}s, sequential ${LEGS_SUM}s"
  log_row "$name" check-legs "$([ -z "$LEGS_FAILED" ] && echo passed || echo failed)" "$LEGS_REPORT wall=${wall}s sum=${LEGS_SUM}s"
  if [ -z "$LEGS_FAILED" ]; then
    state_set "$name" checked "$head"
    log_row "$name" check passed "impact$([ "$remote" = 1 ] && echo ' + remote')$([ "$review" = 1 ] && echo ' + review')"
    note "check passed at ${head:0:10}; next: train.sh ci $name"
  else
    log_row "$name" check failed "legs failed: $LEGS_FAILED"
    die "check FAILED at ${head:0:10} ($LEGS_FAILED); fix the member that broke it (bisect: train.sh bisect $name --test '<failing gate>')"
  fi
}

# --- ci ------------------------------------------------------------------------------------------

cmd_ci() {
  local name="" skip_check=0
  while [ $# -gt 0 ]; do
    case "$1" in
      --skip-check) skip_check=1 ;;
      -*) die "ci: unknown option $1" ;;
      *) name="$1" ;;
    esac; shift
  done
  valid_name "$name"
  require_train_checkout "$name"
  refuse_fork "$REMOTE"
  stage_start
  local base head; base="$(state_get "$name" base)"; head="$(git rev-parse HEAD)"
  if [ "$(state_get_soft "$name" checked)" != "$head" ]; then
    [ "$skip_check" = 1 ] || die "ci: no passing check recorded at ${head:0:10}; run train.sh check $name (or --skip-check, recorded as skipped)"
    log_row "$name" check skipped "--skip-check"
  fi
  local branch="ci/$name"
  # Two pushes. First ci/<name> is reset to the BASE, which is $REMOTE/main's own commit: publishing
  # an already-published main commit to a ci/ ref validates nothing new, so it skips the hook. Then the
  # train is pushed on top with the hook running normally: its range is base..head, the same range
  # `check` validated, so the hook reuses that record instead of re-selecting from the root commit
  # (a new branch has no remote base, and the hook would diff the whole tree).
  git merge-base --is-ancestor "$base" "$(remote_main)" 2>/dev/null \
    || die "ci: the train's base ${base:0:10} is not on $REMOTE/main; rebuild the train"
  run_or_print env CONCRETE_SKIP_GATES=1 git push -q --force "$REMOTE" "$base:refs/heads/$branch"
  push_verified "$REMOTE" "$head" "$branch"
  if [ "$DRY" = 1 ]; then
    note "(dry-run) would dispatch: $GH workflow run CI -R $REPO --ref $branch"
    return 0
  fi
  assert_no_op "push"
  [ "$(git ls-remote "$REMOTE" "refs/heads/$branch" | cut -f1)" = "$head" ] \
    || die "ci: $REMOTE/$branch is not at ${head:0:10} after the push"
  local t0; t0="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
  "$GH" workflow run CI -R "$REPO" --ref "$branch"
  local id="" i
  for i in $(seq 1 30); do
    id="$("$GH" run list -R "$REPO" --workflow CI --branch "$branch" --event workflow_dispatch -L 10 \
           --json databaseId,headSha,createdAt | python3 -I -c '
import json, sys
runs = [r for r in json.load(sys.stdin) if r.get("headSha") == sys.argv[1] and r.get("createdAt", "") >= sys.argv[2]]
print(runs[0]["databaseId"] if runs else "")' "$head" "$t0")" || true
    [ -n "$id" ] && break
    sleep 4
  done
  [ -n "$id" ] || { log_row "$name" ci incomplete "dispatched, run id not found"; die "ci: dispatched, but no run for ${head:0:10} appeared; find it with: $GH run list -R $REPO --branch $branch"; }
  state_set "$name" run_id "$id"
  state_set "$name" ci_utc "$t0"
  log_row "$name" ci dispatched "$branch"
  note "CI run $id for ${head:0:10}: https://github.com/$REPO/actions/runs/$id"
  note "next: train.sh watch $name"
}

# --- watch ---------------------------------------------------------------------------------------

# gh's own -q needs its embedded jq; parsing here keeps a fake gh (the gate's) to plain JSON.
run_field() { # run-id fields... — prints the fields of the run, space-separated, one line
  local id="$1"; shift
  local fields; fields="$(IFS=,; echo "$*")"
  "$GH" run view "$id" -R "$REPO" --json "$fields" | python3 -I -c '
import json, sys
d = json.load(sys.stdin)
print(" ".join(str(d.get(k) or "-") for k in sys.argv[1:]))' "$@"
}

# Prints "<tier>\t<conclusion>\t<job id>\t<name>" per job of the run.
run_jobs() { # run-id
  "$GH" run view "$1" -R "$REPO" --json jobs | python3 -I -c '
import json, re, sys
rx = re.compile(sys.argv[1])
for j in json.load(sys.stdin).get("jobs", []):
    name = j.get("name", "")
    tier = "campaign" if rx.search(name) else "landing"
    print("\t".join([tier, j.get("conclusion") or "pending", str(j.get("databaseId", "")), name]))' "$CAMPAIGN_RE"
}

cmd_watch() {
  local name="" interval=60 timeout=$((4*3600))
  while [ $# -gt 0 ]; do
    case "$1" in
      --interval) interval="$2"; shift ;;
      --timeout) timeout="$2"; shift ;;
      -*) die "watch: unknown option $1" ;;
      *) name="$1" ;;
    esac; shift
  done
  valid_name "$name"
  stage_start
  local id; id="$(state_get "$name" run_id)"
  [ -n "$id" ] || die "watch: no CI run recorded for $name (train.sh ci $name)"
  local status="" concl="" sha="" waited=0
  while :; do
    read -r status concl sha <<< "$(run_field "$id" status conclusion headSha)" \
      || die "watch: cannot read run $id"
    [ -n "$status" ] || die "watch: cannot read run $id"
    [ "$status" = completed ] && break
    [ "$waited" -ge "$timeout" ] && { log_row "$name" watch incomplete "timeout after ${waited}s, status $status"; die "watch: run $id still $status after ${waited}s (a timeout is not a result)"; }
    note "run $id: $status ($(run_jobs "$id" | cut -f2 | sort | uniq -c | tr -s ' \n' ' '))"
    sleep "$interval"; waited=$((waited + interval))
  done
  local jobs; jobs="$(run_jobs "$id")"
  local bad_landing bad_campaign
  bad_landing="$(printf '%s\n' "$jobs" | awk -F'\t' '$1=="landing" && $2!="success"')"
  bad_campaign="$(printf '%s\n' "$jobs" | awk -F'\t' '$1=="campaign" && $2!="success" && $2!="skipped"')"
  # The verdict must describe THIS train: its exact head, with a non-empty landing tier. Otherwise
  # it is no verdict (bisect --ci consumes this exit status).
  local head; head="$(state_get_soft "$name" head)"
  if [ "$sha" != "$head" ]; then
    log_row "$name" watch refused "run $id at $sha"
    echo "train: run $id ran ${sha:0:10}, not the train head ${head:0:10}; it says nothing about this train" >&2
    return 1
  fi
  # awk reads all of its input: `| grep -q` under pipefail can SIGPIPE the writer and flip the test.
  if [ "$(printf '%s\n' "$jobs" | awk -F'\t' '$1=="landing"{n++} END{print n+0}')" = 0 ]; then
    log_row "$name" watch refused "no landing-tier jobs"
    echo "train: run $id has no landing-tier jobs; an empty set is not green" >&2
    return 1
  fi
  if [ -n "$bad_landing$bad_campaign" ]; then
    note "jobs that did not succeed:"
    printf '%s\n' "$bad_landing" "$bad_campaign" | sed '/^$/d' | awk -F'\t' '{printf "    [%s] %-10s %s\n", $1, $2, $4}'
    local line jid jname jc
    while IFS=$'\t' read -r _t jc jid jname; do
      [ -n "$jid" ] || continue
      case "$jc" in failure|timed_out) ;; *) continue ;; esac
      echo "----- log tail: $jname (job $jid) -----"
      "$GH" api "repos/$REPO/actions/jobs/$jid/logs" 2>/dev/null | tail -n 40 || echo "(log unavailable)"
    done <<< "$(printf '%s\n' "$bad_landing" "$bad_campaign")"
  fi
  if [ -z "$bad_landing" ] && [ -n "$bad_campaign" ]; then
    log_row "$name" watch landing-green "only the campaign tier failed (run conclusion $concl)"
    note "ONLY THE CAMPAIGN TIER FAILED: every landing-tier job succeeded. The campaign gates"
    note "qualification (stage c), not merges; train.sh land $name may proceed. Record the campaign failure."
    return 0
  fi
  if [ -z "$bad_landing" ]; then
    log_row "$name" watch passed "run conclusion $concl"
    note "run $id green at ${sha:0:10}; next: train.sh land $name"
    return 0
  fi
  # failed only when a landing job failed; otherwise (cancelled, skipped, pending) it is no verdict.
  local res=incomplete
  if [ "$(printf '%s\n' "$bad_landing" | awk -F'\t' '$2=="failure"||$2=="timed_out"{n++} END{print n+0}')" != 0 ]; then res=failed; fi
  log_row "$name" watch "$res" "run conclusion $concl"
  echo "train: landing tier is NOT green ($res). Bisect: train.sh bisect $name --test '<failing gate>'" >&2
  return 1
}

# --- land ----------------------------------------------------------------------------------------

cmd_land() {
  local name="" mirror_on=1 keep_ci="${LAND_KEEP_MAIN_CI:-0}"
  while [ $# -gt 0 ]; do
    case "$1" in
      --no-mirror) mirror_on=0 ;;
      --keep-main-ci) keep_ci=1 ;;
      -*) die "land: unknown option $1" ;;
      *) name="$1" ;;
    esac; shift
  done
  valid_name "$name"
  require_train_checkout "$name"
  refuse_fork "$REMOTE"
  stage_start
  local base head id; base="$(state_get "$name" base)"; head="$(git rev-parse HEAD)"
  id="$(state_get "$name" run_id)"
  [ -n "$id" ] || die "land: no CI run recorded for $name"

  # Serialize with push-both.sh BEFORE reading the remotes: every check below must still hold when
  # the push happens. The pushes also carry leases on the values read here, so a publisher that does
  # not take this lock still cannot slip a commit in between.
  local lock="$COMMON/concrete-push-both.lock"
  if [ "$DRY" = 0 ]; then
    mkdir "$lock" 2>/dev/null || die "land: another publish holds $lock"
    printf 'pid=%s worktree=%s train=%s\n' "$$" "$ROOT" "$name" > "$lock/owner"
    trap 'rm -f "'"$lock"'/owner"; rmdir "'"$lock"'" 2>/dev/null || true' EXIT
  fi

  local status sha
  read -r status sha <<< "$(run_field "$id" status headSha)" || die "land: cannot read run $id"
  [ "$sha" = "$head" ] || { log_row "$name" land refused "run $id at $sha"; die "land: CI run $id ran ${sha:0:10}, not the train HEAD ${head:0:10}"; }
  [ "$status" = completed ] || die "land: CI run $id is $status, not completed"
  local jobs bad n
  jobs="$(run_jobs "$id")"
  n="$(printf '%s\n' "$jobs" | awk -F'\t' '$1=="landing"' | grep -c . || true)"
  [ "$n" -gt 0 ] || die "land: run $id has no landing-tier jobs; refusing to read an empty set as green"
  bad="$(printf '%s\n' "$jobs" | awk -F'\t' '$1=="landing" && $2!="success" {print $2": "$4}')"
  if [ -n "$bad" ]; then
    log_row "$name" land refused "landing tier not green"
    printf '    %s\n' "$bad" >&2
    die "land: landing-tier jobs above did not succeed in run $id"
  fi

  local now_main; now_main="$(remote_main)"
  if [ "$now_main" != "$base" ]; then
    log_row "$name" land refused "main moved to $now_main"
    echo "train: $REMOTE/main moved from ${base:0:10} to ${now_main:0:10} since the train was built." >&2
    echo "train: the CI result describes a candidate that no longer integrates main. Rebuild and re-run:" >&2
    echo "train:   train.sh build $name --force $(state_get "$name" members | tr ',' '\n' | sed 's/@.*//' | tr '\n' ' ')" >&2
    exit 4
  fi
  git merge-base --is-ancestor "$base" "$head" || die "land: the train does not descend from its base"

  local mirror="" mmain=""
  # Both remotes are required (owner's decision, 2026-10-22): a missing mirror remote is a refusal
  # before anything is pushed, not a silent origin-only landing. --no-mirror is the explicit opt-out.
  if [ "$mirror_on" = 1 ] && [ -n "$MIRROR" ]; then
    refuse_fork "$MIRROR"
    git remote get-url "$MIRROR" >/dev/null 2>&1 \
      || die "land: mirror remote '$MIRROR' is not configured; both remotes are required (add it, or pass --no-mirror deliberately)"
    mirror="$MIRROR"
    mmain="$(git ls-remote "$mirror" refs/heads/main | cut -f1)"
    if [ -n "$mmain" ] && ! git merge-base --is-ancestor "$mmain" "$head" 2>/dev/null; then
      die "land: $mirror/main (${mmain:0:10}) is not an ancestor of the train; reconcile the mirror first (nothing pushed)"
    fi
  fi

  # The hook sees base..head again (the same range `check` validated) and reuses that record. main
  # is NEVER force-pushed, not even with a lease: a plain push is a fast-forward or a refusal, so a
  # publisher that moved main without taking the lock makes this push fail instead of being undone.
  # (Head descends from base; if main moved to anything head does not contain, git refuses.)
  push_verified "$REMOTE" "$head" main
  if [ "$DRY" = 0 ]; then
    [ "$(remote_main)" = "$head" ] || die "land: $REMOTE/main is not at ${head:0:10} after the push"
  fi
  if [ -n "$mirror" ]; then
    # The mirror receives exactly the commit the primary's hook just accepted (push-both.sh policy).
    ( push_verified "$mirror" "$head" main CONCRETE_SKIP_GATES=1 ) || {
      log_row "$name" land failed "mirror $mirror not at head"
      die "land: $REMOTE landed, but $mirror/main is not at ${head:0:10}"
    }
  fi
  if [ "$DRY" = 1 ]; then
    [ "$keep_ci" = 1 ] || note "(dry-run) would cancel main's duplicate push run of CI at ${head:0:10} (run $id validated it)"
    return 0
  fi
  # Both remotes, read back once more: the landing is complete only when each is at the head.
  [ "$(remote_main)" = "$head" ] || die "land: $REMOTE/main is not at ${head:0:10}"
  if [ -n "$mirror" ] && [ "$(git ls-remote "$mirror" refs/heads/main | cut -f1)" != "$head" ]; then
    die "land: $mirror/main is not at ${head:0:10}"
  fi
  state_set "$name" landed_utc "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
  log_row "$name" land passed "$REMOTE${mirror:+ + $mirror}"
  note "landed ${head:0:10} on $REMOTE/main${mirror:+ and $mirror/main}. Members (result: CI run $id at ${head:0:10}):"
  state_get "$name" members | tr ',' '\n' | sed 's/^/    /'
  if [ "$keep_ci" = 0 ]; then LAND_NAME="$name" cancel_duplicate_main_run "$head" "$id"; fi
}

# --- bisect --------------------------------------------------------------------------------------
# Halve the member list until one member remains. Each subset is its own train (<name>-bN) and its
# result is recorded against that train's SHA. Attribution assumes one culprit; an interaction
# between members shows up as a culprit that is green alone, and the report says to check it.

# The landing bot's kill switch (LAND_PAUSE_FILE) is read before and after every operation of every
# probe (each --ci probe checks and dispatches a run). A probe that sees it answers `paused`, never
# green or red. The switch is a file that stays until the bot has acknowledged it: one created and
# removed inside a single operation is not seen.
bisect_paused() { [ -n "${LAND_PAUSE_FILE:-}" ] && [ -e "$LAND_PAUSE_FILE" ]; }

subset_red() { # sub-name test-cmd|"" members...
  local sub="$1" test="$2"; shift 2
  local refs=() m
  for m in "$@"; do refs+=("${m%@*}"); done
  # Members are pinned to their recorded SHAs via the temporary refs below, so a moved branch tip
  # cannot change what is bisected.
  local pinned=() i=0
  for m in "$@"; do
    git update-ref "refs/landing-bisect/$sub/$i" "${m##*@}"
    pinned+=("refs/landing-bisect/$sub/$i"); i=$((i+1))
  done
  if bisect_paused; then echo paused; return 0; fi
  local brc=0
  "$SELF_DIR/train.sh" build "$sub" --force --no-fetch "${pinned[@]}" >/dev/null || brc=$?
  if bisect_paused; then echo paused; return 0; fi
  if [ "$brc" != 0 ]; then echo build-failed; return 0; fi
  local rc=0
  # The switch is read again after EVERY operation, whatever it returned: a probe that ran while
  # the switch was pulled gives no verdict, so nothing is attributed from it.
  if [ -n "$test" ]; then
    bash -c "$test" >/dev/null 2>&1 || rc=$?
    if bisect_paused; then echo paused; return 0; fi
  else
    "$SELF_DIR/train.sh" check "$sub" --no-remote >/dev/null 2>&1 || rc=$?
    if bisect_paused; then echo paused; return 0; fi
    if [ "$rc" = 0 ]; then
      "$SELF_DIR/train.sh" ci "$sub" >/dev/null 2>&1 || rc=$?
      if bisect_paused; then echo paused; return 0; fi
    fi
    if [ "$rc" = 0 ]; then
      "$SELF_DIR/train.sh" watch "$sub" >/dev/null 2>&1 || rc=$?
      if bisect_paused; then echo paused; return 0; fi
    fi
  fi
  STAGE_T0="$(date +%s)"
  log_row "$sub" bisect-step "$([ "$rc" = 0 ] && echo passed || echo failed)" "members: $(printf '%s ' "${refs[@]}")"
  [ "$rc" = 0 ] && echo green || echo red
}

cmd_bisect() {
  local name="" test="" useci=0
  while [ $# -gt 0 ]; do
    case "$1" in
      --test) test="$2"; shift ;;
      --ci) useci=1 ;;
      -*) die "bisect: unknown option $1" ;;
      *) name="$1" ;;
    esac; shift
  done
  valid_name "$name"
  [ -n "$test" ] || [ "$useci" = 1 ] || die "bisect: pass --test '<cmd that fails on red>' or --ci"
  assert_clean
  stage_start
  local members; members="$(state_get "$name" members)"
  local list=() m; IFS=',' read -r -a list <<< "$members"
  [ "${#list[@]}" -ge 2 ] || die "bisect: train $name has fewer than two members"
  local step=0 half verdict
  while [ "${#list[@]}" -gt 1 ]; do
    step=$((step+1)); half=$(( ${#list[@]} / 2 ))
    local first=("${list[@]:0:$half}") second=("${list[@]:$half}")
    verdict="$(subset_red "$name-b$step" "$test" "${first[@]}")"
    note "step $step: first half [$(printf '%s ' "${first[@]%@*}")] -> $verdict"
    case "$verdict" in
      paused) log_row "$name" bisect incomplete "paused ($LAND_PAUSE_FILE) at step $step"; die "bisect: paused by $LAND_PAUSE_FILE at step $step; no culprit attributed" ;;
      red|build-failed) list=("${first[@]}") ;;
      green) list=("${second[@]}") ;;
    esac
  done
  local culprit="${list[0]}"
  verdict="$(subset_red "$name-bc" "$test" "$culprit")"
  if [ "$verdict" = paused ]; then
    log_row "$name" bisect incomplete "paused ($LAND_PAUSE_FILE) during the culprit probe"
    die "bisect: paused by $LAND_PAUSE_FILE while probing ${culprit%@*} alone; no culprit attributed"
  fi
  if bisect_paused; then
    log_row "$name" bisect incomplete "paused ($LAND_PAUSE_FILE) before attribution"
    die "bisect: paused by $LAND_PAUSE_FILE before attribution; no culprit attributed"
  fi
  local rest=() ; for m in $(printf '%s\n' "${members//,/ }"); do [ "$m" = "$culprit" ] || rest+=("${m%@*}"); done
  STAGE_T0="$(date +%s)"
  state_set "$name" bisect_culprit "$culprit"
  log_row "$name" bisect "$([ "$verdict" = green ] && echo inconclusive || echo attributed)" "culprit $culprit alone: $verdict"
  git checkout -q "train/$name" 2>/dev/null || true
  note "culprit: ${culprit%@*} (${culprit##*@}) — alone it is $verdict"
  [ "$verdict" = green ] && note "it is green ALONE: the failure is an interaction between members; check it by hand"
  note "land the rest:  train.sh build $name --force $(printf '%s ' "${rest[@]}")"
}

cmd_status() {
  local name="$1"; valid_name "$name"
  cat "$(state_file "$name")" 2>/dev/null || die "no train '$name'"
  [ -f "$LOG_DIR/$name.tsv" ] && column -t -s $'\t' "$LOG_DIR/$name.tsv" 2>/dev/null || true
}

usage() { sed -n '2,13p' "$0" | sed 's/^# \{0,1\}//'; exit 2; }

sub="${1:-}"; [ $# -gt 0 ] && shift
case "$sub" in
  build) cmd_build "$@" ;;
  check) cmd_check "$@" ;;
  ci) cmd_ci "$@" ;;
  watch) cmd_watch "$@" ;;
  land) cmd_land "$@" ;;
  bisect) cmd_bisect "$@" ;;
  status) cmd_status "$@" ;;
  *) usage ;;
esac
