#!/usr/bin/env bash
# Landing trains (ROADMAP R-0539): integrate every ready branch into ONE candidate, validate it once,
# and land it on both remotes. Stage timestamps go to the landing log (R-0547). docs/project/LANDING.md
# is the manual; this header is the contract.
#
#   train.sh [--dry-run] build  <name> [--force] [--no-fetch] <ref>...
#   train.sh [--dry-run] check  <name> [--no-remote]
#   train.sh [--dry-run] ci     <name> [--skip-check]
#   train.sh             watch  <name> [--interval S] [--timeout S]
#   train.sh [--dry-run] land   <name> [--no-mirror]
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
#
# ENVIRONMENT (test seams and overrides): LAND_REPO (owner/name for gh, default
# unbalancedparentheses/concrete2), LAND_GH (gh binary), LAND_REMOTE (primary, default origin),
# LAND_MIRROR (default lambdaclass, pushed only when that remote exists; empty disables),
# LAND_STATE_DIR (default <git common dir>/concrete-landing), LAND_LOG_DIR (default
# $LAND_STATE_DIR/log), LAND_NO_NIX=1 (never re-enter `nix develop`), LAND_CAMPAIGN_JOBS (regex of
# campaign-tier job names), LAND_HOOK (pre-push hook path, default .githooks/pre-push).
set -euo pipefail

SELF_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
RESOLVER="$SELF_DIR/resolve_conflicts.py"
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
in_nix() { # cmd...
  if [ "${LAND_NO_NIX:-0}" = 1 ] || [ -n "${IN_NIX_SHELL:-}" ] || ! command -v nix >/dev/null 2>&1 \
     || [ ! -f "$ROOT/flake.nix" ]; then
    "$@"
  else
    nix develop "$ROOT" --command "$@"
  fi
}

run_or_print() { # cmd... — executes, or prints under --dry-run
  if [ "$DRY" = 1 ]; then printf 'train (dry-run): would run:'; printf ' %q' "$@"; echo; return 0; fi
  "$@"
}

remote_main() { git ls-remote "$REMOTE" refs/heads/main | cut -f1; }

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

regenerate() {
  if [ -f scripts/gen/refresh_generated.sh ]; then
    bash scripts/gen/refresh_generated.sh
  else
    [ -f scripts/gen/bug_dashboard.py ] && python3 -I scripts/gen/bug_dashboard.py --write
    [ -f scripts/gen/candidate_table.py ] && python3 -I scripts/gen/candidate_table.py --write
    [ -f scripts/gen/build_identity.sh ] && bash scripts/gen/build_identity.sh
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

  local m ref msg rc conflicted
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
      # shellcheck disable=SC2086
      if ! python3 -I "$RESOLVER" resolve $conflicted; then
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

  note "regenerating generated views once on the train"
  regenerate || { git checkout -q -- . ; die "build: a generator refused (above); the train is left at its last merge"; }
  if [ -n "$(git status --porcelain --untracked-files=no)" ]; then
    local changed; changed="$(git status --porcelain --untracked-files=no | cut -c4-)"
    local left; left="$(markers_in $changed)"
    [ -z "$left" ] || die "build: conflict markers remain after regeneration in: $left"
    git add -u
    git commit -q -m "train/$name: regenerate generated views ($(printf '%s ' $changed | sed 's/ $//'))"
    assert_no_op "regeneration commit"
  fi
  local tracked_markers
  tracked_markers="$(git grep -lE '^(<{7}|>{7})( |$)' HEAD -- . 2>/dev/null | head -5 || true)"
  [ -z "$tracked_markers" ] || die "build: committed conflict markers in: $tracked_markers"

  state_set "$name" head "$(git rev-parse HEAD)"
  state_set "$name" built_utc "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
  log_row "$name" build passed "${#shas[@]} member(s)"
  note "train/$name built at $(git rev-parse --short=10 HEAD) on base ${base:0:10}:"
  printf '    %s\n' "${shas[@]}"
  note "next: train.sh check $name"
}

# --- check ---------------------------------------------------------------------------------------

cmd_check() {
  local name="" remote=1
  while [ $# -gt 0 ]; do
    case "$1" in
      --no-remote) remote=0 ;;
      -*) die "check: unknown option $1" ;;
      *) name="$1" ;;
    esac; shift
  done
  valid_name "$name"
  require_train_checkout "$name"
  stage_start
  local base head; base="$(state_get "$name" base)"; head="$(git rev-parse HEAD)"

  # Remote full set (R-0541) runs alongside the local selection when the server tool exists.
  local rpid="" rlog=""
  if [ "$remote" = 1 ] && [ -x "$HOME/bin/concrete-remote" ]; then
    if [ "$DRY" = 1 ]; then
      note "(dry-run) would run: $HOME/bin/concrete-remote full-gates $head train/$name"
    else
      rlog="$STATE_ROOT/$name/remote-$head.log"
      note "remote full gate set: $HOME/bin/concrete-remote full-gates $head (log $rlog)"
      "$HOME/bin/concrete-remote" full-gates "$head" "train/$name" > "$rlog" 2>&1 &
      rpid=$!
    fi
  fi

  # The hook's own selection (fast surface gates + touched areas) over exactly the range the `ci`
  # push will present (base..head on ci/<name>), so that push can reuse this validation record.
  local line="refs/heads/train/$name $head refs/heads/ci/$name $base"
  local rc=0
  if [ "$DRY" = 1 ]; then
    note "(dry-run) would run the pre-push selection: echo '$line' | bash $HOOK $REMOTE"
  else
    [ -f "$HOOK" ] || die "check: no pre-push hook at $HOOK"
    printf '%s\n' "$line" | in_nix bash "$HOOK" "$REMOTE" "$(git remote get-url "$REMOTE")" || rc=$?
  fi

  # Impact selection (R-0513), when present: one gate command per line on stdout.
  if [ "$rc" = 0 ] && [ -f scripts/tests/select_gates.sh ] && ! grep -q select_gates "$HOOK" 2>/dev/null; then
    if [ "$DRY" = 1 ]; then
      note "(dry-run) would run the gates printed by: bash scripts/tests/select_gates.sh --base $base --head $head"
    else
      local sel; sel="$(bash scripts/tests/select_gates.sh --base "$base" --head "$head")" || rc=$?
      if [ "$rc" = 0 ] && [ -n "$sel" ]; then
        local args=() c
        while IFS= read -r c; do [ -n "$c" ] && args+=(--first "$c"); done <<< "$sel"
        in_nix bash scripts/tests/run_ci_gates_local.sh "${args[@]}" || rc=$?
      fi
    fi
  fi

  if [ -n "$rpid" ]; then
    local rrc=0; wait "$rpid" || rrc=$?
    if [ "$rrc" = 0 ]; then note "remote full gate set passed at ${head:0:10}"
    else note "remote full gate set FAILED at ${head:0:10} (exit $rrc); tail of $rlog:"; tail -n 40 "$rlog" || true; rc=1; fi
    log_row "$name" check-remote "$([ "$rrc" = 0 ] && echo passed || echo failed)" "concrete-remote exit $rrc"
  fi
  [ "$DRY" = 1 ] && return 0
  if [ "$rc" = 0 ]; then
    state_set "$name" checked "$head"
    log_row "$name" check passed "pre-push selection$([ -f scripts/tests/select_gates.sh ] && echo ' + select_gates')"
    note "check passed at ${head:0:10}; next: train.sh ci $name"
  else
    log_row "$name" check failed "exit $rc"
    die "check FAILED at ${head:0:10}; fix the member that broke it (bisect: train.sh bisect $name --test '<failing gate>')"
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
  run_or_print in_nix git push "$REMOTE" "$head:refs/heads/$branch"
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
  local head; head="$(state_get_soft "$name" head)"
  [ "$sha" = "$head" ] || note "WARNING: run $id is at ${sha:0:10}, not the train head ${head:0:10}"
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
  if printf '%s\n' "$bad_landing" | cut -f2 | grep -qE '^(failure|timed_out)$'; then res=failed; fi
  log_row "$name" watch "$res" "run conclusion $concl"
  echo "train: landing tier is NOT green ($res). Bisect: train.sh bisect $name --test '<failing gate>'" >&2
  return 1
}

# --- land ----------------------------------------------------------------------------------------

cmd_land() {
  local name="" mirror_on=1
  while [ $# -gt 0 ]; do
    case "$1" in
      --no-mirror) mirror_on=0 ;;
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

  local mirror=""
  if [ "$mirror_on" = 1 ] && [ -n "$MIRROR" ] && git remote get-url "$MIRROR" >/dev/null 2>&1; then
    refuse_fork "$MIRROR"
    mirror="$MIRROR"
    local mmain; mmain="$(git ls-remote "$mirror" refs/heads/main | cut -f1)"
    if [ -n "$mmain" ] && ! git merge-base --is-ancestor "$mmain" "$head" 2>/dev/null; then
      die "land: $mirror/main (${mmain:0:10}) is not an ancestor of the train; reconcile the mirror first (nothing pushed)"
    fi
  fi

  # Serialize with push-both.sh: remotes are shared by every worktree.
  local lock="$COMMON/concrete-push-both.lock"
  if [ "$DRY" = 0 ]; then
    mkdir "$lock" 2>/dev/null || die "land: another publish holds $lock"
    printf 'pid=%s worktree=%s train=%s\n' "$$" "$ROOT" "$name" > "$lock/owner"
    trap 'rm -f "'"$lock"'/owner"; rmdir "'"$lock"'" 2>/dev/null || true' EXIT
  fi
  # The hook sees base..head again (the same range `check` validated) and reuses that record.
  run_or_print in_nix git push "$REMOTE" "$head:refs/heads/main"
  if [ "$DRY" = 0 ]; then
    assert_no_op "push to $REMOTE"
    [ "$(remote_main)" = "$head" ] || die "land: $REMOTE/main is not at ${head:0:10} after the push"
  fi
  if [ -n "$mirror" ]; then
    # The mirror receives exactly the commit the primary's hook just accepted (push-both.sh policy).
    run_or_print env CONCRETE_SKIP_GATES=1 git push "$mirror" "$head:refs/heads/main"
    if [ "$DRY" = 0 ] && [ "$(git ls-remote "$mirror" refs/heads/main | cut -f1)" != "$head" ]; then
      log_row "$name" land failed "mirror $mirror not at head"
      die "land: $REMOTE landed, but $mirror/main is not at ${head:0:10}"
    fi
  fi
  [ "$DRY" = 1 ] && return 0
  state_set "$name" landed_utc "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
  log_row "$name" land passed "$REMOTE${mirror:+ + $mirror}"
  note "landed ${head:0:10} on $REMOTE/main${mirror:+ and $mirror/main}. Members (result: CI run $id at ${head:0:10}):"
  state_get "$name" members | tr ',' '\n' | sed 's/^/    /'
}

# --- bisect --------------------------------------------------------------------------------------
# Halve the member list until one member remains. Each subset is its own train (<name>-bN) and its
# result is recorded against that train's SHA. Attribution assumes one culprit; an interaction
# between members shows up as a culprit that is green alone, and the report says to check it.

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
  "$SELF_DIR/train.sh" build "$sub" --force --no-fetch "${pinned[@]}" >/dev/null || { echo build-failed; return 0; }
  local rc=0
  if [ -n "$test" ]; then
    bash -c "$test" >/dev/null 2>&1 || rc=$?
  else
    "$SELF_DIR/train.sh" check "$sub" --no-remote >/dev/null 2>&1 && "$SELF_DIR/train.sh" ci "$sub" >/dev/null 2>&1 \
      && "$SELF_DIR/train.sh" watch "$sub" >/dev/null 2>&1 || rc=$?
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
      red|build-failed) list=("${first[@]}") ;;
      green) list=("${second[@]}") ;;
    esac
  done
  local culprit="${list[0]}"
  verdict="$(subset_red "$name-bc" "$test" "$culprit")"
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
