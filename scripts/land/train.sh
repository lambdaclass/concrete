#!/usr/bin/env bash
# Landing trains (ROADMAP R-0539): integrate every ready branch into ONE candidate, validate it once,
# and land it on main. Stage timestamps go to the landing log (R-0547). docs/LANDING.md
# is the manual; this header is the contract.
#
#   train.sh [--dry-run] build  <name> [--force] [--no-fetch] <ref>...
#   train.sh [--dry-run] check  <name> [--no-remote] [--no-review] [--local-gates]
#   train.sh [--dry-run] ci     <name> [--skip-check]
#   train.sh             watch  <name> [--interval S] [--timeout S]
#   train.sh [--dry-run] land   <name>
#   train.sh             bisect <name> (--test '<cmd>' | --ci)
#   train.sh             status <name>
#   train.sh             record <head> --base <sha> [--exclude-run <id>]   (main's CI, R-0570)
#
# SAFETY RULES, each written after a mistake made by hand on 2026-10-22:
#   * after EVERY git operation the script checks that no merge, rebase, cherry-pick or revert is in
#     progress. A rebase piped to `tail` kept going and a commit was made mid-rebase; here a stopped
#     operation is a stop, never a warning.
#   * only generated views and append-only ledgers are resolved mechanically
#     (scripts/land/resolve_conflicts.py); any other conflict aborts the merge and names the files.
#   * (2026-11-06, ISS-0015) a train runs only from a FULL clone whose pre-push hook is the
#     repository's (core.hooksPath .githooks, or a wrapper that runs it): a linked worktree hands its
#     hook GIT_DIR, which once turned a test check into writes on a real clone and GitHub (where_ok;
#     `status` and `record` push nothing and run anywhere).
#   * (2026-11-06) review fails closed: `check` with no review command fails rather than skipping the
#     leg, and a train lands only with review provenance (review_provenance: a verified clean review
#     note at its head, or, when the head has none, on every member at its exact SHA).
#   * a train lands only when the CI run's head SHA is the local HEAD, every landing-tier job
#     succeeded, every required job (scripts/land/required_checks.py: the candidate's list, plus the
#     base's unless an explicit transition row replaces or retires it) is present and succeeded, and
#     origin/main is still the train's base. A moved main means rebuild and re-run:
#     a result is bound to the exact candidate that ran.
#   * (2026-11-05) a candidate that changes legacy/ or the root's top-level entries (the legacy/ move)
#     lands only with an accepted acceptance rehearsal of that exact candidate (same train-head tree,
#     same base) in <state>/rehearsals.tsv; see is_move_candidate and require_rehearsal. A dry run
#     refused only by that gate exits 5.
#   * pushes name the exact SHA, never `HEAD` (push-both.sh: HEAD can move during a wait), and a
#     remote named `fork` is refused outright.
#   * (2026-11-06, the owner's decision: one repository, lambdaclass/concrete) there is no mirror.
#     $REMOTE must be the repository gh reads ($REPO) whenever it is a GitHub URL: a clone still
#     pointing at the archived unbalancedparentheses/concrete2 is refused before anything is pushed.
#   * (2026-10-25) a push's exit status is not its result: GitHub drops the connection during a long
#     hook, so a push can fail after the remote moved, or before. The train's push to ci/<name> and
#     the push of `land` are followed by an `ls-remote` of the ref, and one that did not move it
#     is retried (LAND_PUSH_ATTEMPTS). The ci/<name> reset to the base skips the hook; not retried.
#
# ENVIRONMENT (test seams and overrides): LAND_REPO (owner/name for gh, default
# lambdaclass/concrete), LAND_GH (gh binary), LAND_REMOTE (the repository's remote, default origin),
# LAND_STATE_DIR (default <git common dir>/concrete-landing), LAND_LOG_DIR (default
# $LAND_STATE_DIR/log), LAND_NO_NIX=1 (never re-enter `nix develop`), LAND_CAMPAIGN_JOBS (regex of
# campaign-tier job names), LAND_HOOK (pre-push hook path, default .githooks/pre-push),
# LAND_PUSH_ATTEMPTS (default 3) and LAND_PUSH_RETRY_SLEEP (seconds, default 15) for verified pushes,
# LAND_REQUIRED_CHECKS_FILE (default required-checks.txt), LAND_REQUIRED_TRANSITION_FILE (default
# required-checks-transition.tsv), LAND_GATES_HOST (the
# gate host of `check`'s impact leg, else `git config land.gatesHost`; see gates_host below) and
# LAND_GATES_TRANSPORT (scripts/land/gates_remote.sh's transport).
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

# Where a train may run (ISS-0015). Only from a FULL clone: in a linked worktree git hands its hooks
# GIT_DIR=<repo>/.git/worktrees/<name>, which is how the 2026-11-06 hook run wrote test notes into a
# real clone and onto GitHub; a long hook in a worktree also drops the push. And only with the
# repository's pre-push hook enabled: train.sh pushes ci/* and main, and with the hook off (no
# core.hooksPath, or one pointing elsewhere) those pushes would run no gate. The hook git runs is
# `git rev-parse --git-path hooks/pre-push` (core.hooksPath honoured). It is asked who it is: run
# with the single argument --concrete-identify and no input, the repository's .githooks/pre-push prints
# `concrete-pre-push <its real path>` and runs nothing, and so does a wrapper that runs it
# (acceptance.sh's guard hook). Any other answer, or none, is refused; a hook that merely mentions
# the file is not enough. `status` and `record` push nothing (record runs in CI's Landing record
# job, in a checkout with no hook enabled), so they run anywhere.
where_ok() {
  local gd hook want got
  gd="$(cd "$(git rev-parse --git-dir)" && pwd -P)"
  [ "$gd" = "$(cd "$COMMON" && pwd -P)" ] \
    || die "refusing to run in a linked worktree ($ROOT; its git dir is $gd): run train.sh from a full clone, never a worktree"
  hook="$(git rev-parse --git-path hooks/pre-push)"
  case "$hook" in /*) ;; *) hook="$ROOT/$hook" ;; esac
  [ -f "$hook" ] && [ -x "$hook" ] \
    || die "refusing to run: this clone has no enabled pre-push hook ($hook is missing or not executable); enable the repository's: git config core.hooksPath .githooks"
  want="concrete-pre-push $(cd "$ROOT" && pwd -P)/.githooks/pre-push"
  got="$("$hook" --concrete-identify < /dev/null 2>/dev/null | tail -n 1 || true)"   # a failing hook is an answer too
  [ "$got" = "$want" ] \
    || die "refusing to run: the enabled pre-push hook $hook is not the repository's .githooks/pre-push and does not run it (asked who it is, it answered '${got}'); enable it: git config core.hooksPath .githooks"
}
STATE_ROOT="${LAND_STATE_DIR:-$COMMON/concrete-landing}"
LOG_DIR="${LAND_LOG_DIR:-$STATE_ROOT/log}"
REPO="${LAND_REPO:-$LAND_CANONICAL_REPO}"
GH="${LAND_GH:-gh}"
REMOTE="${LAND_REMOTE:-origin}"
HOOK="${LAND_HOOK:-.githooks/pre-push}"
# Must name exactly the workflow's campaign jobs (an `if:` on `inputs.campaign`, and the jobs that `needs:`
# one): check_landing_script.sh derives that set from lean_action_ci.yml and fails on any difference.
# The root has no campaign tier yet; a campaign job, when one returns, is named "Campaign: ...".
CAMPAIGN_RE="${LAND_CAMPAIGN_JOBS:-^Campaign: }"

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

# The remote is the one repository and gh reads it (since 2026-11-06): every fetch and push URL that
# is GitHub names LAND_CANONICAL_REPO, and so does $REPO (land_remote_bad, lib.sh). Every command
# that can push or read GitHub (build, check, ci, watch, land, record) checks it before anything
# else. In CI a refused record is no record, so every job runs: the guard can only add checks there.
require_remote_repo() {
  local why
  why="$(land_remote_bad "$REMOTE" "$REPO")" || die "$why"
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
  require_remote_repo
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
  out="$("$runner" "$ref" "${LAND_FULL_GATES_CMD:-lake build && bash scripts/checks/run.sh}" 2>&1)" || rc=$?
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

# The gate host (heavy legs default to concrete-ci-2, docs/LANDING.md "Gate host"):
# LAND_GATES_HOST if set (empty = none), else `git config land.gatesHost`. With a gate host the impact
# leg runs there through scripts/land/gates_remote.sh, for the exact head SHA, and its pass is
# attested so the `ci` push's hook reuses it. An unreachable host is INCOMPLETE and fails the check;
# running the gates on this machine instead takes the explicit `check --local-gates`.
gates_host() {
  if [ -n "${LAND_GATES_HOST+x}" ]; then echo "$LAND_GATES_HOST"
  else git config --get land.gatesHost 2>/dev/null || true; fi
  return 0
}

# The review command: LAND_REVIEW_CMD, else scripts/review/review.sh, called with --base --head.
# Empty when there is neither, which `check` refuses (it never skips the leg silently).
review_cmd() {
  if [ -n "${LAND_REVIEW_CMD:-}" ]; then echo "$LAND_REVIEW_CMD"
  elif [ -f scripts/review/review.sh ]; then echo "bash scripts/review/review.sh"; fi
  return 0
}

# REVIEW PROVENANCE (fail closed; docs/LANDING.md, "check" and "land"). A train is reviewed when its
# head carries a clean review note, or when its head carries no review note and every member carries
# one at its exact recorded SHA (each member was reviewed at its own head: the landing bot's trains,
# and a train rebuilt from reviewed branches). Every note is judged by scripts/review/verify.sh, on
# $REMOTE's notes (fetched first, replacing the local copy, as verify.sh --fetch does): an absent,
# malformed, blocked or findings note qualifies nothing, and a head note that is not clean is not
# overridden by its members. Prints what qualified; on failure prints why and returns 1.
VERIFY="$SELF_DIR/../review/verify.sh"
review_provenance() { # name head
  local name="$1" head="$2" out m n=0 bad=""
  [ -f "$VERIFY" ] || { echo "no review verifier ($VERIFY is missing)"; return 1; }
  git fetch -q "$REMOTE" "+refs/notes/concrete-review:refs/notes/concrete-review" 2>/dev/null \
    || { echo "could not fetch refs/notes/concrete-review from $REMOTE"; return 1; }
  if out="$(bash "$VERIFY" "$head" 2>&1)"; then echo "the head ${head:0:10} carries a clean review note"; return 0; fi
  if git notes --ref=refs/notes/concrete-review show "$head" >/dev/null 2>&1; then
    echo "the head's review note does not qualify: ${out//$'\n'/ }"; return 1
  fi
  for m in $(state_get_soft "$name" members | tr ',' ' '); do
    n=$((n + 1))
    out="$(bash "$VERIFY" "${m##*@}" 2>&1)" || bad="$bad; member ${m%@*}: ${out//$'\n'/ }"
  done
  [ "$n" -gt 0 ] || { echo "the head ${head:0:10} has no review note and the train records no members"; return 1; }
  [ -z "$bad" ] || { echo "the head ${head:0:10} has no review note, and not every member has a clean one$bad"; return 1; }
  echo "the head has no review note; every member ($n) carries a clean one at its exact SHA"
}

# Codex review leg. It passes without running the reviewer when the train already has review
# provenance. Otherwise it runs the review command; a non-zero exit fails, and so does a zero exit
# that leaves no provenance (no note, or a malformed, blocked or findings note at the head).
review_leg() { # name base head
  local why
  if why="$(review_provenance "$1" "$3")"; then echo "review: $why; not reviewed again"; return 0; fi
  echo "review: $why; running the reviewer"
  bash -c "$(review_cmd) --base $2 --head $3" || return $?
  why="$(review_provenance "$1" "$3")" || { echo "review: the reviewer exited 0, but $why"; return 1; }
  echo "review: $why"
}

cmd_check() {
  local name="" remote=1 review=1 local_gates=0
  while [ $# -gt 0 ]; do
    case "$1" in
      --no-remote) remote=0 ;;
      --no-review) review=0 ;;
      --local-gates) local_gates=1 ;;
      -*) die "check: unknown option $1" ;;
      *) name="$1" ;;
    esac; shift
  done
  valid_name "$name"
  require_remote_repo
  require_train_checkout "$name"
  stage_start
  local base head; base="$(state_get "$name" base)"; head="$(git rev-parse HEAD)"
  # A new check withdraws the previous one: a failing re-run must not leave an old pass standing.
  [ "$DRY" = 1 ] || state_set "$name" checked ""
  if [ "$remote" = 1 ] && [ -z "$(remote_runner)" ]; then remote=0; fi
  if [ "$review" = 1 ] && [ -z "$(review_cmd)" ]; then
    log_row "$name" check failed "no review command"
    die "check: no review command (LAND_REVIEW_CMD is unset and scripts/review/review.sh is missing); the review leg is required and is never skipped silently. Restore the reviewer, or pass --no-review explicitly (land still requires review provenance)"
  fi
  local ghost=""; [ "$local_gates" = 1 ] || ghost="$(gates_host)"
  local impact_cmd=(bash "$SELF_DIR/impact_gates.sh") impact_where="here"
  if [ -n "$ghost" ]; then impact_cmd=(bash "$SELF_DIR/gates_remote.sh" --host "$ghost"); impact_where="on $ghost"
  elif [ "$local_gates" = 0 ]; then note "no gate host configured (LAND_GATES_HOST / git config land.gatesHost): the impact gates run on this machine"; fi
  # The attestation describes one run; a new check withdraws the previous one before it starts.
  [ "$DRY" = 1 ] || rm -f "$(git rev-parse --git-path concrete-prepush-remote-record)" \
    "$(git rev-parse --git-path concrete-prepush-remote-record).log"

  # The legs are independent, so they run AT THE SAME TIME and are all collected (LANDING.md,
  # "Pipelining"): the impact selection over exactly the range the `ci` push will present
  # (base..head on ci/<name>, so that push reuses the hook's record), the remote full set, and the
  # Codex review. Any failing leg fails the check.
  if [ "$DRY" = 1 ]; then
    note "(dry-run) would run in parallel: impact gates $impact_where (hook line 'refs/heads/train/$name $head refs/heads/ci/$name $base')"
    if [ "$remote" = 1 ]; then note "(dry-run)   + remote full gates: $(remote_runner) <ref of ${head:0:10}> '${LAND_FULL_GATES_CMD:-lake build && bash scripts/checks/run.sh}'"; fi
    if [ "$review" = 1 ]; then note "(dry-run)   + review: the train's review provenance, else $(review_cmd) --base $base --head $head"; fi
    return 0
  fi
  local dir="$STATE_ROOT/$name/check-${head:0:12}"; mkdir -p "$dir"
  local t0; t0="$(date +%s)"
  legs_start impact "$dir/impact.log" "${impact_cmd[@]}" --base "$base" --head "$head" \
    --src-ref "refs/heads/train/$name" --dst-ref "refs/heads/ci/$name"
  if [ "$remote" = 1 ]; then legs_start remote "$dir/remote.log" remote_leg "$name" "$head"; fi
  if [ "$review" = 1 ]; then legs_start review "$dir/review.log" review_leg "$name" "$base" "$head"; fi
  legs_wait
  local wall=$(( $(date +%s) - t0 )) leg
  cat "$dir/impact.log"
  for leg in $LEGS_FAILED; do
    [ "$leg" = impact ] && continue
    note "$leg leg FAILED at ${head:0:10}; tail of $dir/$leg.log:"; tail -n 40 "$dir/$leg.log" || true
  done
  note "legs: $LEGS_REPORT; wall ${wall}s, sequential ${LEGS_SUM}s"
  # gates_remote.sh exit 3: no result about head (host unreachable, wrong SHA, lost log). It blocks
  # like a failure and is recorded as INCOMPLETE, never as a pass or as a gate failure.
  local impact_rc=0; [ -f "$dir/impact.log.rc" ] && read -r impact_rc _ < "$dir/impact.log.rc"
  local verdict; verdict="$([ -z "$LEGS_FAILED" ] && echo passed || echo failed)"
  if [ -n "$ghost" ] && [ "$impact_rc" = 3 ]; then verdict=incomplete; fi
  log_row "$name" check-legs "$verdict" "$LEGS_REPORT wall=${wall}s sum=${LEGS_SUM}s"
  if [ -z "$LEGS_FAILED" ]; then
    state_set "$name" checked "$head"
    log_row "$name" check passed "impact $impact_where$([ "$remote" = 1 ] && echo ' + remote')$([ "$review" = 1 ] && echo ' + review')"
    note "check passed at ${head:0:10}; next: train.sh ci $name"
  elif [ "$verdict" = incomplete ]; then
    log_row "$name" check incomplete "impact gates on $ghost gave no result; legs failed: $LEGS_FAILED"
    die "check INCOMPLETE at ${head:0:10}: the gate host $ghost gave no result about it (see $dir/impact.log). Re-run when it is reachable, or run the gates here with: train.sh check $name --local-gates"
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
  require_remote_repo
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
    note "(dry-run) would dispatch: $GH workflow run CI -R $REPO --ref $branch -f base=$base"
    return 0
  fi
  assert_no_op "push"
  [ "$(git ls-remote "$REMOTE" "refs/heads/$branch" | cut -f1)" = "$head" ] \
    || die "ci: $REMOTE/$branch is not at ${head:0:10} after the push"
  local t0; t0="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
  # base: the run's base-sensitive checks compare with the train's base, and its title records it
  # (main's Landing record accepts only a run of the base main had before the landing, R-0570).
  "$GH" workflow run CI -R "$REPO" --ref "$branch" -f base="$base"
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

# Required checks: required-checks.txt (one exact CI job name per line) and, for a candidate that
# changes the set, required-checks-transition.tsv. scripts/land/required_checks.py is the one owner
# of the rule (its header): the candidate's list plus every base requirement, except one the
# candidate drops through an explicit transition row (replaced by jobs it requires, or retired with
# a reason, reported as retired and never as passed). A required job absent from the run, or whose
# conclusion is not success, is no verdict. GitHub branch protection is not configured for main, so
# these files are the record of what is required.
REQUIRED_FILE="${LAND_REQUIRED_CHECKS_FILE:-required-checks.txt}"
TRANSITION_FILE="${LAND_REQUIRED_TRANSITION_FILE:-required-checks-transition.tsv}"
# Prints required_checks.py's findings for base, head and the run's jobs.
required_report() { # base head jobs
  local d; d="$(mktemp -d)" || return 1
  git show "$1:$REQUIRED_FILE" > "$d/base" 2>/dev/null || : > "$d/base"
  git show "$2:$REQUIRED_FILE" > "$d/cand" 2>/dev/null || : > "$d/cand"
  git show "$2:$TRANSITION_FILE" > "$d/trans" 2>/dev/null || : > "$d/trans"
  printf '%s\n' "$3" > "$d/jobs"
  python3 -I "$SELF_DIR/required_checks.py" "$d/base" "$d/cand" "$d/trans" "$d/jobs"; local rc=$?
  rm -rf "$d"; return "$rc"
}
# Prints the unmet lines (anything but a note); empty when the requirements are met.
unmet_required() { # base head jobs
  local r; r="$(required_report "$@")" || { echo "invalid: required_checks.py failed"; return 0; }
  printf '%s\n' "$r" | awk '/./ && !/^(retired|replaced|stale): /'
}
required_notes() { # base head jobs
  required_report "$@" 2>/dev/null | awk '/^(retired|replaced|stale): /' | sed 's/^/train: requirement /' || true
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
  require_remote_repo
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
  local base unmet; base="$(state_get_soft "$name" base)"
  required_notes "$base" "$head" "$jobs"
  unmet="$(unmet_required "$base" "$head" "$jobs")"
  if [ -n "$unmet" ]; then
    log_row "$name" watch incomplete "required checks unmet"
    echo "train: run $id does not satisfy $REQUIRED_FILE; a missing, skipped or cancelled required job is no verdict:" >&2
    printf '    %s\n' "$unmet" >&2
    [ -z "$bad_landing" ] && return 1
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

# --- record: main's CI run names the run that qualified its exact SHA (R-0570) ---------------------
# A landing pushes main to a SHA that the train's CI run (on ci/<name>) already validated. The push
# starts CI on main at that SHA; its first job, "Landing record", runs this. When the record holds,
# every other landing-tier job of main's run is skipped, so main's run concludes success with a
# summary naming the qualifying run, instead of running everything twice or showing "cancelled".
# When it does not hold, nothing is skipped: main's run runs every job itself. So main is green
# only on a result obtained AT ITS EXACT SHA, by this workflow, under land's own rule:
#   * the NEWEST completed workflow_dispatch run of .github/workflows/lean_action_ci.yml in $REPO
#     (the train's repository, the one CI runs in) whose head SHA is exactly <head> and whose title says it ran at base <base>
#     (`<ref> at base <sha>`, written by the workflow's run-name from the `base` input `train.sh ci`
#     passes), other than the asking run. The base binds the base-sensitive checks (LL1_BASE,
#     TOTALITY_BASE, CONFORMANCE_BASE, SPEC_BASE) to the very base main had before the push, as a
#     push run of main would; an older green run never outvotes a later one of the same SHA and base;
#   * its landing tier is non-empty and every landing-tier job succeeded;
#   * every required job (required_checks.py on <base> and <head>, transition rows included) is
#     present and succeeded; <base> (the value main had before the push) must be in this clone and
#     <head> must be strictly ahead of it on GitHub (compare status "ahead").
# Any other finding, an unreadable API included, is no record (exit 1), never a pass. A record is
# never carried from one SHA to another. Prints `qualifying_run=<id>` on success.
cmd_record() {
  local head="" base="" exclude=""
  while [ $# -gt 0 ]; do
    case "$1" in
      --base) base="${2:-}"; shift ;;
      --exclude-run) exclude="${2:-}"; shift ;;
      -*) die "record: unknown option $1" ;;
      *) head="$1" ;;
    esac; shift
  done
  require_remote_repo
  [[ "$head" =~ ^[0-9a-f]{40}$ ]] || die "record: <head> must be a full 40-hex SHA, not '$head'"
  [[ "$base" =~ ^[0-9a-f]{40}$ ]] && [ "$base" != 0000000000000000000000000000000000000000 ] \
    || die "record: --base must be the full SHA main had before the push, not '$base'"
  case "$exclude" in ""|*[!0-9]*) [ -z "$exclude" ] || die "record: --exclude-run must be a run id" ;; esac
  git cat-file -e "$base^{commit}" 2>/dev/null || die "record: base ${base:0:10} is not in this clone (fetch it); its required-checks.txt binds the record"
  git cat-file -e "$head^{commit}" 2>/dev/null || die "record: head ${head:0:10} is not in this clone"
  local cmp
  cmp="$("$GH" api "repos/$REPO/compare/$base...$head" 2>/dev/null | python3 -I -c '
import json, sys
try: print(json.load(sys.stdin).get("status", "-"))
except ValueError: print("-")')" || cmp="-"
  [ "$cmp" = ahead ] || die "record: ${head:0:10} is not strictly ahead of ${base:0:10} on $REPO (compare: $cmp); no record"
  local runs
  runs="$("$GH" api "repos/$REPO/actions/runs?head_sha=$head&per_page=100" 2>/dev/null | python3 -I -c '
import json, sys
head, base, exclude = sys.argv[1], sys.argv[2], sys.argv[3]
try: runs = json.load(sys.stdin).get("workflow_runs", [])
except ValueError: sys.exit(0)
ok = [r for r in runs
      if r.get("head_sha") == head and r.get("path") == ".github/workflows/lean_action_ci.yml"
      and r.get("event") == "workflow_dispatch" and r.get("status") == "completed"
      and str(r.get("display_title", "")).endswith(" at base " + base)
      and str(r.get("id")) != exclude]
for r in sorted(ok, key=lambda r: r.get("id", 0), reverse=True): print(r["id"])' "$head" "$base" "$exclude")" || runs=""
  [ -n "$runs" ] || die "record: no completed CI dispatch of $REPO at ${head:0:10} at base ${base:0:10}; no record"
  # The newest such run decides: an older green run never outvotes a later one.
  local id sha jobs n bad unmet
  id="${runs%%$'\n'*}"
  read -r sha <<< "$(run_field "$id" headSha)" || die "record: cannot read run $id"
  [ "$sha" = "$head" ] || die "record: run $id ran ${sha:0:10}, not ${head:0:10}; no record"
  jobs="$(run_jobs "$id")" || die "record: cannot read the jobs of run $id"
  n="$(printf '%s\n' "$jobs" | awk -F'\t' '$1=="landing"{n++} END{print n+0}')"
  [ "$n" -gt 0 ] || die "record: run $id has no landing-tier jobs; an empty set is not green"
  bad="$(printf '%s\n' "$jobs" | awk -F'\t' '$1=="landing" && $2!="success" {print "    "$2": "$4}')"
  [ -z "$bad" ] || die "record: run $id, the newest at ${head:0:10}, has landing-tier jobs that did not succeed:
$bad"
  unmet="$(unmet_required "$base" "$head" "$jobs")"
  [ -z "$unmet" ] || die "record: run $id does not satisfy $REQUIRED_FILE on ${base:0:10}..${head:0:10}:
$(printf '%s\n' "$unmet" | sed 's/^/    /')"
  note "record: run $id passed every landing-tier and required job at exactly ${head:0:10} (base ${base:0:10})"
  echo "qualifying_run=$id"
}

# --- land ----------------------------------------------------------------------------------------

# THE MOVE GATE (R-0558, R-0563). A candidate that moves the old tree into legacy/ lands only when
# $STATE_ROOT/rehearsals.tsv holds an `accepted` row for it, written by scripts/land/acceptance.sh.
# Which candidates count as a move is decided from base..head, not from names: one that changes
# anything under legacy/, whose head has a `legacy` entry that is not a directory, or that changes the
# SET OF TOP-LEVEL ENTRIES scripts/checks/root_inventory.txt lists (an entry added or removed, or the
# file itself added or removed). The inventory's comments, its order and its nested docs/... and
# scripts/... entries do not count: adding a folder under docs/ is upkeep of the root, which
# scripts/checks/root_inventory.sh enforces on every push, not a move. So a move staged across several
# candidates (a scaffold first, the compiler later) needs a rehearsal at every step, and so does any
# later edit under legacy/ (B3 deletes it), which is rare by design.
# The candidate is identified by its train head's TREE and its base: the acceptance run builds its own
# train, whose merge commit differs from this one's, but the same base and members give the same
# tree, and a tree is exactly what lands. A different tree, a different base or any other verdict is
# no rehearsal of this candidate. Columns (tab-separated, `#` lines are comments):
#   utc  verdict  train_head  train_head_tree  base  ref  name
# A row that is not exactly that (7 fields, verdict accepted|failed|incomplete, 40-hex ids) is a
# corrupt record and refuses the land outright rather than being skipped.
# A real land refuses (exit 1). A dry run that reaches this gate has passed every other land check;
# without the record it refuses with exit 5 and says so, which is what acceptance.sh's own dry-run
# land leg expects of a move candidate (it writes the record only after that leg).
# TRUST BOUNDARY: the gate lives in this script, so it holds only while the train.sh that runs the
# land has it. A candidate that edits train.sh changes the land path for every later land; review
# must keep require_rehearsal on it. Nothing here defends against a reviewed change that removes it.
REHEARSALS="$STATE_ROOT/rehearsals.tsv"
# The top-level entries <commit>'s root inventory lists, sorted; nothing when it has none. Parsed as
# scripts/checks/root_inventory.sh parses it: a line starting with `#` is a comment, the rest is
# stripped and a blank line skipped. An entry with a `/` is nested (docs/..., scripts/...).
root_entries() { # commit
  git show "$1:scripts/checks/root_inventory.txt" 2>/dev/null \
    | awk '/^#/ { next } { gsub(/^[[:space:]]+|[[:space:]]+$/, "") } $0 != "" && !/\// { print }' \
    | LC_ALL=C sort -u
  return 0
}
is_move_candidate() { # base head
  local t
  t="$(git cat-file -t "$2:legacy" 2>/dev/null || true)"
  [ -z "$t" ] || [ "$t" = tree ] || return 0
  [ -z "$(git diff --name-only "$1" "$2" -- legacy)" ] || return 0
  [ "$(root_entries "$1")" != "$(root_entries "$2")" ]
}
require_rehearsal() { # name base head
  local name="$1" base="$2" head="$3" tree why=""
  is_move_candidate "$base" "$head" || return 0
  tree="$(git rev-parse "$head^{tree}")"
  if [ ! -s "$REHEARSALS" ]; then
    why="no rehearsal record ($REHEARSALS does not exist or is empty)"
  else
    local bad; bad="$(awk -F'\t' '!/^#/ && !(NF==7 && $2 ~ /^(accepted|failed|incomplete)$/ && $3 ~ /^[0-9a-f]{40}$/ && $4 ~ /^[0-9a-f]{40}$/ && $5 ~ /^[0-9a-f]{40}$/) {print NR; exit}' "$REHEARSALS")"
    [ -z "$bad" ] || die "land: $REHEARSALS line $bad is not a rehearsal row (utc verdict head tree base ref name); fix or remove the record"
    local mine; mine="$(awk -F'\t' -v t="$tree" -v b="$base" '!/^#/ && $4==t && $5==b' "$REHEARSALS")"
    if printf '%s\n' "$mine" | awk -F'\t' '$2=="accepted" {f=1} END {exit !f}'; then
      note "move candidate: acceptance accepted tree ${tree:0:10} on base ${base:0:10} ($(printf '%s\n' "$mine" | awk -F'\t' '$2=="accepted"' | tail -1 | cut -f1,3,6 | tr '\t' ' '))"
      return 0
    elif [ -n "$mine" ]; then
      why="the rehearsal of this candidate was $(printf '%s\n' "$mine" | tail -1 | cut -f2), not accepted"
    else
      why="no rehearsal of this candidate (the record holds other candidates only)"
    fi
  fi
  log_row "$name" land refused "move without an accepted rehearsal"
  local msg="land: ${head:0:10} is a legacy/ move candidate (it changes legacy/ or the root's top-level entries), and there is no accepted rehearsal for its tree ${tree:0:10} on base ${base:0:10}: $why. Run scripts/land/acceptance.sh --ref <member> in this clone first."
  if [ "$DRY" = 1 ]; then
    echo "$MOVE_GATE_MARKER" >&2
    echo "train: $msg" >&2
    exit 5
  fi
  die "$msg"
}

cmd_land() {
  local name=""
  while [ $# -gt 0 ]; do
    case "$1" in
      -*) die "land: unknown option $1" ;;
      *) name="$1" ;;
    esac; shift
  done
  valid_name "$name"
  require_train_checkout "$name"
  refuse_fork "$REMOTE"
  require_remote_repo
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

  required_notes "$base" "$head" "$jobs"
  local unmet; unmet="$(unmet_required "$base" "$head" "$jobs")"
  if [ -n "$unmet" ]; then
    log_row "$name" land refused "required checks unmet"
    printf '    %s\n' "$unmet" >&2
    die "land: run $id does not satisfy $REQUIRED_FILE"
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

  # Review provenance, read now and not trusted from `check`: a check run with --no-review, or a
  # reviewer that wrote no clean note, leaves none, and nothing lands without it.
  local prov
  if ! prov="$(review_provenance "$name" "$head")"; then
    log_row "$name" land refused "no review provenance"
    die "land: ${head:0:10} has no review provenance: $prov. Review the train head (scripts/review/review.sh --head ${head:0:10}) or its members, then land."
  fi
  note "review provenance: $prov"

  # The legacy/ move (R-0558) lands only after the acceptance test accepted this exact candidate:
  # last of all, so a dry run that reaches it has passed every other land check.
  require_rehearsal "$name" "$base" "$head"

  # The hook sees base..head again (the same range `check` validated) and reuses that record. main
  # is NEVER force-pushed, not even with a lease: a plain push is a fast-forward or a refusal, so a
  # publisher that moved main without taking the lock makes this push fail instead of being undone.
  # (Head descends from base; if main moved to anything head does not contain, git refuses.)
  push_verified "$REMOTE" "$head" main
  if [ "$DRY" = 0 ]; then
    [ "$(remote_main)" = "$head" ] || die "land: $REMOTE/main is not at ${head:0:10} after the push"
  fi
  [ "$DRY" = 1 ] && return 0
  # Read back once more: the landing is complete only when main is at the head.
  [ "$(remote_main)" = "$head" ] || die "land: $REMOTE/main is not at ${head:0:10}"
  state_set "$name" landed_utc "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
  log_row "$name" land passed "$REMOTE"
  note "landed ${head:0:10} on $REMOTE/main. Members (result: CI run $id at ${head:0:10}):"
  state_get "$name" members | tr ',' '\n' | sed 's/^/    /'
  note "main's own CI run at ${head:0:10} records run $id in its Landing record job (train.sh record) and"
  note "skips the jobs that run already passed; confirm: gh run list --workflow CI --branch main -R $REPO"
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

usage() { sed -n '2,14p' "$0" | sed 's/^# \{0,1\}//'; exit 2; }

sub="${1:-}"; [ $# -gt 0 ] && shift
case "$sub" in status|record|""|-h|--help) ;; *) where_ok ;; esac
case "$sub" in
  build) cmd_build "$@" ;;
  check) cmd_check "$@" ;;
  ci) cmd_ci "$@" ;;
  watch) cmd_watch "$@" ;;
  land) cmd_land "$@" ;;
  bisect) cmd_bisect "$@" ;;
  status) cmd_status "$@" ;;
  record) cmd_record "$@" ;;
  *) usage ;;
esac
