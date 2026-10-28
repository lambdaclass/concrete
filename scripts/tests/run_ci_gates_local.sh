#!/usr/bin/env bash
# Run every gate command the CI workflow runs, locally (ROADMAP #34a).
#
# The fast suite is NOT the CI gate set: golden baselines, proof gates, and
# per-feature check_*.sh gates run only in CI, and twice (2026-06, 2026-07)
# CI stayed red for 14-15 pushes while local fast-suite green was treated as
# done. This script extracts the gate commands FROM the workflow file itself
# (so the list cannot drift) and runs them sequentially, printing only
# failures. Run it before pushing anything that touches the compiler or the
# gates; a full pass takes a while (proof/oracle gates are slow) — that is
# still ~3x faster than one CI round-trip.
#
# Extraction contract: a workflow line invoking
#   [VAR=value ...] [bash|python3|sh] [./]scripts/....(sh|py) [args...]
# is a gate; the full argument list is preserved (fuzz seeds/counts,
# --trust-gate, the grammar path), as are VAR=value env prefixes.
# EXCLUDED on purpose: check_gate_mutation_coverage.sh — a nightly-only gate
# that mutates compiler source files to verify gate coverage; running it as
# a pre-push step would dirty the working tree it is meant to protect.
#
# Usage: scripts/tests/run_ci_gates_local.sh [filter-substring]
#        scripts/tests/run_ci_gates_local.sh --job "<CI job name substring>"
#        scripts/tests/run_ci_gates_local.sh [--plan] [--no-wait] [--first CMD]... [--job J]... [--filter S]...
#        scripts/tests/run_ci_gates_local.sh [--plan] [--no-wait] --impact <base-ref> | --impact-files <paths-file>
#   --no-wait    a heavy run (see R-0543 below) exits 75 instead of waiting when every slot is busy.
#
# DEFERRED TO CI (bug 315). A gate that cannot make part of its verdict on this host exits 77
# (GATE_DEFERRED_RC); that is DEFERRED, counted apart from passed and failed, only for a gate that
# scripts/tests/lib/host_tools.sh declares (HOST_DEFERRED_TABLE, judged_on) and that CI runs as an
# unconditional step; from any other gate exit 77 is a failure. A deferral leaves the run's exit 0 (the
# pre-push hook accepts it) but PARTIAL: deferred_to_ci(...) blocks completed=1 like a host skip, and
# .ci-gates-summary records deferred= and deferred_gates=. CONCRETE_GATES_DEFERRED_OUT=<file>, when
# set, receives the deferred commands one per line (the hook writes them into its record).
#
# SELECTORS COMPOSE INTO ONE UNIQUE LIST. `--job`, `--filter` (or a bare positional substring) and
# `--first` may each be repeated; the union is resolved ONCE, deduplicated by the normalized command
# text, and every selected command runs exactly once. The pre-push hook used to call this runner once
# per job and once per area, and the selections overlap: a Concrete/Proof change ran 79 gate
# invocations of which only 67 were distinct (scripts/ci/proof_gate.sh twice, among others).
#   --first CMD  runs CMD sequentially BEFORE the rest, in the order given, and a failure there stops
#                the run before anything else launches — the fail-fast ordering the hook had when it
#                ran the fast surface checklist first. CMD need not be a workflow command, but its
#                script must exist.
#   --plan       prints the resolved list (one command per line, execution order) and exits. It takes
#                no lock, writes no summary and runs nothing.
# Every --job/--filter must select at least one command on its own; an empty selector is refused,
# exactly as a single empty filter always was.
#
# `--job` selects every gate declared inside one CI JOB, instead of guessing at
# gate names. Filtering by name substring is itself a fact restated where it can
# drift: the workflow already says which gates belong to which job, and a
# name filter re-derives that by convention. `proof` selects 6 gates, but the
# "Proof evidence gate" job runs 40 — including `check_operational_vc_auto_discharge.sh`,
# whose NAME contains no "proof". R-0442 changed a Proof-layer API, the hook ran
# the 6, and the 40th broke a Lean fixture that only CI typechecks.

set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
WORKFLOW=".github/workflows/lean_action_ci.yml"

# A RUN IS ONE PROCESS GROUP, AND THE GROUP IS WHAT A SIGNAL KILLS (bug 244).
#
# Killing this runner used to kill this runner: its backgrounded gates, the compilers they started and
# the mutation harnesses copying the tree were reparented to init and kept working, and the repository
# lock then refused every later run — correctly, since a dead creator proves nothing about its
# children — until a human deleted it. So a real run re-executes itself under
# scripts/tests/lib/gate_session.py, which makes it the whole of a new session's process group: a
# SIGINT, SIGTERM or SIGHUP to the pid the caller started terminates every descendant, the launcher
# sweeps the group until the kernel reports it empty, and the lock (which records the group) is
# reclaimed on exactly that proof. --plan runs nothing and takes no lock, so it needs no session.
#
# NEVER A SECOND SESSION. A runner started inside a gate session (a gate exercising this runner) stays
# in the enclosing group, or the outer run's sweep could no longer reach it. Without python3 there is
# no session and a killed run needs the manual recovery it always did; that is said, not hidden.
if [ -z "${CONCRETE_GATE_SESSION:-}" ]; then
  _ci_plan_only=0
  for _a in "$@"; do [ "$_a" = "--plan" ] && _ci_plan_only=1; done
  if [ "$_ci_plan_only" = "0" ]; then
    if command -v python3 >/dev/null 2>&1 && [ -f "$ROOT_DIR/scripts/tests/lib/gate_session.py" ]; then
      exec python3 "$ROOT_DIR/scripts/tests/lib/gate_session.py" -- \
        bash "$ROOT_DIR/scripts/tests/run_ci_gates_local.sh" "$@"
    fi
    echo "warning: no python3 or gate_session.py — running without a gate session; a killed run" >&2
    echo "         will leave its children running and the lock held until recovered by hand." >&2
  fi
fi
# The deferral list is this runner's output, not its gates' input: a gate that runs a runner of its
# own (check_gate_lock.sh does) must not write this run's list (bug 315).
DEFERRED_OUT="${CONCRETE_GATES_DEFERRED_OUT:-}"; unset CONCRETE_GATES_DEFERRED_OUT
# THIS RUNNER OWNS THE SESSION when the session's leader is its parent and it is in that group. Only
# the owner signals the group: a nested runner that did would kill the run enclosing it.
CI_SESSION_OWNER=0
if [ -n "${CONCRETE_GATE_SESSION:-}" ] && [ "${CONCRETE_GATE_SESSION}" = "$PPID" ] \
   && [ "$(ps -o pgid= -p $$ 2>/dev/null | tr -d ' ')" = "$CONCRETE_GATE_SESSION" ]; then
  CI_SESSION_OWNER=1
fi

# GATES RUN IN A CLEAN GIT ENVIRONMENT. Git exports GIT_DIR (and friends) into hooks, so a
# gate run from pre-push inherited them, and every `git -C <other repo>` inside a gate read
# THIS repository instead: check_external_checker.sh compared lean4export's pinned SHA with
# the pushed HEAD and reported "could not build lean4export" only under the hook (CI and a
# hand run were green). Clearing the repository-local variables here makes a gate see the
# same git that CI and a hand run see; git rediscovers this repository from the cwd.
unset $(git rev-parse --local-env-vars)

# ARGUMENTS ARE PARSED BEFORE THE LOCK, BUT JUDGED AFTER IT. Parsing is pure; an error found here is
# only reported once the lock is held (so the invalidation record below is written by the holder),
# except in --plan mode, which never takes the lock.
ARG_ERR=""
PLAN=0
HEAVY_NO_WAIT="${CONCRETE_HEAVY_NO_WAIT:-0}"
JOBSEL=(); FILTERS=(); FIRST_CMDS=(); IMPACT=()
while [ $# -gt 0 ]; do
  case "$1" in
    # `--closure` IS DOCUMENTED but was never parsed: it fell through as a filter substring, matched
    # no gate, and produced a partial result that looked like a deliberate subset.
    --closure) CONCRETE_CLOSURE_RUN=1; shift ;;
    --plan)    PLAN=1; shift ;;
    # --impact BASE / --impact-files FILE: the commands scripts/tests/select_gates.sh derives from
    # what changed since merge-base(BASE, HEAD), or from a list of changed paths (R-0513).
    --impact|--impact-files)
      if [ -z "${2:-}" ]; then ARG_ERR="$1 needs an argument"; break; fi
      if [ "$1" = "--impact" ]; then IMPACT=("$2"); else IMPACT=(--files "$2"); fi
      shift 2 ;;
    --no-wait) HEAVY_NO_WAIT=1; shift ;;   # R-0543: fail fast (exit 75) when every heavy slot is busy
    --job|--filter|--first)
      if [ -z "${2:-}" ]; then ARG_ERR="$1 needs an argument"; break; fi
      case "$1" in
        --job)    JOBSEL+=("$2") ;;
        --filter) FILTERS+=("$2") ;;
        --first)  FIRST_CMDS+=("$2") ;;
      esac
      shift 2 ;;
    --*) ARG_ERR="unknown option $1"; break ;;
    *)   FILTERS+=("$1"); shift ;;   # the legacy positional filter substring
  esac
done
SELECTING=0
[ "${#JOBSEL[@]}${#FILTERS[@]}${#FIRST_CMDS[@]}${#IMPACT[@]}" = "0000" ] || SELECTING=1

# THE ONE EXTRACTOR. Strip any trailing REDIRECTION from the extracted command. The character
# class stops at `&`, so a workflow line ending `... --trust-gate 2>&1 | tee
# /tmp/trust-gate.log` was captured as `... --trust-gate 2>` — a redirect with no
# target. `eval` then died of a parse error and the gate was counted FAIL, every
# run, on a clean tree. So the trust gate was never actually EXECUTED locally
# while appearing in the failure list: the loudest possible way to be silent.
# Output is discarded by the runner anyway, so redirections are noise here.
_extract_cmds() { # workflow text file -> gate commands, raw spelling, sorted unique
  grep -oE '([A-Z_][A-Z0-9_]*=[^ ;|&]+[[:space:]]+)*((bash|python3|sh)[[:space:]]+)?(\./)?scripts/[^ ;|&]*\.(sh|py)[^;|&]*' "$1" \
    | grep -v -E 'check_gate_mutation_coverage\.sh|aggregate_mutation_shards\.py|scripts/dev/seed-lake\.sh|check_checker_pin_freshness\.sh --tier campaign' \
    | sed 's/[[:space:]][0-9]*[<>].*$//' \
    | sed 's/[[:space:]]*$//' \
    | sort -u
}
# The spelling the runner EXECUTES: `bash ./scripts/x.sh` and `bash scripts/x.sh` are one gate.
# Deduplication is on this form, so two spellings of one gate can never both run.
_norm_cmd() { local c="$1"; printf '%s' "${c/bash .\//bash }"; }

# PINNED, not floored. A floor of 100 against 208 accepted the silent loss of 108 commands while
# still producing a closure artifact, because DISCOVERED is derived from whatever survived.
# The pin describes the WHOLE workflow, so it applies only when the whole workflow was read. With
# `--job` the text is deliberately narrowed to one job first, and demanding all 208 commands from that
# subset made the documented flag unusable (fail-closed, but unusable).
# The count history (213 -> 273) is in git history; since the ledger migration the
# pin is the inventory file below, and a gate's line is added in the commit that adds the gate.
EXPECTED_GATE_COMMANDS=0
# THE INVENTORY REPLACES THE COUNT once scripts/tests/ci_gate_inventory.txt exists (the ledger migration,
# scripts/gen/migrate_ledgers.sh, writes it). One extracted command per line, sorted: a branch that
# adds a gate adds its line, two branches adding different gates touch different lines instead of
# both rewriting one number, and the check compares CONTENT, so a lost gate cannot hide behind an
# added one with the same total (ROADMAP_HISTORY "Gate inventory checked in CI, by content").
GATE_INVENTORY="scripts/tests/ci_gate_inventory.txt"
if [ -f "$GATE_INVENTORY" ]; then
  EXPECTED_GATE_COMMANDS="$(grep -c . "$GATE_INVENTORY" || true)"
fi

# RESOLVE THE SELECTION — sets CMDS (raw inventory), SELECTED (normalized, unique, execution order),
# FIRST_N (how many leading SELECTED entries are the sequential first stage) and OUTSIDE (selected
# commands that are not workflow commands). On failure sets RESOLVE_ERR (a refusal token) and
# RESOLVE_MSG; nothing is executed or written here.
resolve_selection() {
  RESOLVE_ERR=""; RESOLVE_MSG=""; SELECTED=(); FIRST_N=0; OUTSIDE=0
  mapfile -t CMDS < <(_extract_cmds "$WORKFLOW" 2>/dev/null)
  # THE INVENTORY, WHEN PRESENT, IS CHECKED FIRST AND BY CONTENT: it names what was lost or added,
  # where the count below can only say that the total moved. A near-empty inventory is refused as
  # vacuous, exactly like a near-empty extraction.
  if [ -f "$GATE_INVENTORY" ]; then
    if [ "$EXPECTED_GATE_COMMANDS" -lt 100 ]; then
      RESOLVE_ERR="gate_inventory_vacuous(inventory:$EXPECTED_GATE_COMMANDS)"
      RESOLVE_MSG="$GATE_INVENTORY lists only $EXPECTED_GATE_COMMANDS gate commands; refusing a vacuous inventory."
      return 1
    fi
    local inv_diff
    inv_diff="$(diff <(LC_ALL=C sort -u "$GATE_INVENTORY") <(printf '%s\n' "${CMDS[@]}" | LC_ALL=C sort -u) || true)"
    if [ -n "$inv_diff" ]; then
      RESOLVE_ERR="gate_inventory_mismatch"
      RESOLVE_MSG="the workflow's gate commands differ from $GATE_INVENTORY (< inventory only, > workflow only):
$inv_diff
       If the workflow legitimately changed, update $GATE_INVENTORY in the SAME commit
       (one command per line, LC_ALL=C sorted). Refusing to report a verdict."
      return 1
    fi
  fi
  # VACUITY FLOOR ON THE INVENTORY ITSELF. `mapfile` does not fail when its process substitution
  # produces nothing, so a missing workflow file or extractor drift yields CMDS=() — after which every
  # loop below does nothing, DISCOVERED == EXECUTED == 0 holds, and closure mode writes `completed=1`
  # with `PASS=0 FAIL=0`. PINNED, not floored: see EXPECTED_GATE_COMMANDS below.
  # THE PIN NOW APPLIES TO EVERY RUN. `--job` used to read only that job's text and so could not
  # check it; selection is now resolved against the whole workflow, and a job's commands must be a
  # subset of it.
  if [ "${#CMDS[@]}" != "$EXPECTED_GATE_COMMANDS" ]; then
    RESOLVE_ERR="gate_inventory_vacuous(${#CMDS[@]})"
    RESOLVE_MSG="gate extraction produced ${#CMDS[@]} commands from $WORKFLOW, pinned at $EXPECTED_GATE_COMMANDS.
       If the workflow legitimately changed, update EXPECTED_GATE_COMMANDS in the SAME commit.
       That is extractor failure, not an empty gate set. Refusing to report a verdict."
    return 1
  fi
  # Every extracted command must be SYNTACTICALLY VALID before anything is run. A truncated command
  # otherwise reports as a failing gate, which reads as "the tree is red" rather than "the extractor
  # is broken" — and those demand opposite responses.
  local cmd bad=""
  for cmd in "${CMDS[@]}"; do
    bash -n -c "$cmd" 2>/dev/null || bad="$bad
         $cmd"
  done
  if [ -n "$bad" ]; then
    RESOLVE_ERR="gate_extraction_invalid"
    RESOLVE_MSG="gate extraction produced unrunnable commands — fix the extractor,
       not the gates. A truncated command cannot be told from a red gate:$bad"
    return 1
  fi
  local -A inv=() seen=()
  local n
  for cmd in "${CMDS[@]}"; do
    n="$(_norm_cmd "$cmd")"
    if [ -n "${inv[$n]:-}" ]; then
      RESOLVE_ERR="gate_inventory_duplicate_spelling"
      RESOLVE_MSG="the workflow spells one gate two ways ($n); a full run would execute it twice."
      return 1
    fi
    inv[$n]=1
  done
  if [ "$SELECTING" = "0" ]; then
    for cmd in "${CMDS[@]}"; do SELECTED+=("$(_norm_cmd "$cmd")"); done
    return 0
  fi
  _sel_add() { if [ -z "${seen[$1]:-}" ]; then seen[$1]=1; SELECTED+=("$1"); return 0; fi; return 1; }
  local script
  for cmd in "${FIRST_CMDS[@]}"; do
    n="$(_norm_cmd "$cmd")"
    if ! bash -n -c "$n" 2>/dev/null; then
      RESOLVE_ERR="first_command_invalid"; RESOLVE_MSG="--first command is not valid shell: $n"; return 1
    fi
    if [ -z "${inv[$n]:-}" ]; then
      # Outside the workflow inventory (the docs-only checks are). It still has to name a script that
      # exists: a missing script would otherwise "fail" for a reason that says nothing about the tree.
      script="$(printf '%s' "$n" | grep -oE '(\./)?scripts/[^ ;|&]*\.(sh|py)' | head -1)"
      if [ -z "$script" ] || [ ! -f "$script" ]; then
        RESOLVE_ERR="first_command_missing_script"; RESOLVE_MSG="--first command names no existing script: $n"; return 1
      fi
      _sel_add "$n" && OUTSIDE=$((OUTSIDE+1))
    else
      _sel_add "$n" || true
    fi
  done
  FIRST_N="${#SELECTED[@]}"
  local j f t got
  for j in "${JOBSEL[@]}"; do
    # Job headers sit at two-space indent under `jobs:`; a name: line at that depth starts a new block.
    t="$(mktemp)" || { RESOLVE_ERR="tmp_unavailable"; RESOLVE_MSG="mktemp failed"; return 1; }
    awk -v job="$j" '
      /^  [a-zA-Z0-9_-]+:[[:space:]]*$/ { inblock=0 }
      /^    name:/ { inblock = (index($0, job) > 0) }
      inblock { print }
    ' "$WORKFLOW" > "$t"
    if ! [ -s "$t" ]; then rm -f "$t"
      RESOLVE_ERR="no_ci_job($j)"; RESOLVE_MSG="no CI job matching '$j'"; return 1
    fi
    got=0
    while IFS= read -r cmd; do
      [ -n "$cmd" ] || continue
      n="$(_norm_cmd "$cmd")"
      if [ -z "${inv[$n]:-}" ]; then rm -f "$t"
        RESOLVE_ERR="job_command_outside_inventory"; RESOLVE_MSG="job '$j' yields '$n', which the whole-workflow extraction does not"; return 1
      fi
      got=$((got+1)); _sel_add "$n" || true
    done < <(_extract_cmds "$t")
    rm -f "$t"
    if [ "$got" -eq 0 ]; then
      RESOLVE_ERR="job_selected_nothing($j)"; RESOLVE_MSG="--job '$j' yielded no gate commands."; return 1
    fi
  done
  for f in "${FILTERS[@]}"; do
    # A SUBSET OF ZERO IS NOT A SUBSET — per selector. A typo'd filter used to skip every command and
    # report `PARTIAL PASS=0`; with several selectors, one empty one would hide behind the others.
    got=0
    for cmd in "${CMDS[@]}"; do
      n="$(_norm_cmd "$cmd")"
      [[ "$n" == *"$f"* ]] || continue
      got=$((got+1)); _sel_add "$n" || true
    done
    if [ "$got" -eq 0 ]; then
      RESOLVE_ERR="filter_selected_nothing($f)"; RESOLVE_MSG="filter '$f' matches no gate command."; return 1
    fi
  done
  # IMPACT SELECTION. Unlike a filter it may legitimately select nothing (an inert docs change); the
  # selector's own failure is a refusal, never an empty selection. Every command it names must be in
  # the inventory, so a selector/extractor disagreement cannot run something CI does not.
  if [ "${#IMPACT[@]}" -gt 0 ]; then
    local imp
    if ! imp="$(bash "$ROOT_DIR/scripts/tests/select_gates.sh" "${IMPACT[@]}")"; then
      RESOLVE_ERR="impact_selection_failed"; RESOLVE_MSG="scripts/tests/select_gates.sh ${IMPACT[*]} failed"; return 1
    fi
    while IFS= read -r cmd; do
      [ -n "$cmd" ] || continue
      if [ -z "${inv[$cmd]:-}" ]; then
        RESOLVE_ERR="impact_command_outside_inventory"; RESOLVE_MSG="select_gates.sh named '$cmd', which is not a workflow command"; return 1
      fi
      _sel_add "$cmd" || true
    done <<< "$imp"
  fi
  return 0
}

# --plan: print the resolved selection and stop. No lock, no summary, no build, no gate.
if [ "$PLAN" = "1" ]; then
  [ -z "$ARG_ERR" ] || { echo "error: $ARG_ERR" >&2; exit 2; }
  resolve_selection || { echo "error: $RESOLVE_MSG" >&2; exit 2; }
  printf '%s\n' "${SELECTED[@]}"
  exit 0
fi

# THE WHOLE PASS HOLDS ONE LOCK. Individual gates take it too, but only those that call
# `require_fresh_binary`; a grep-only gate does not, so without this a stray run could interleave
# between this runner's children. Acquiring it here also makes the runner itself refuse to start
# while anything else is working the tree — which is the failure that made an entire 187-gate pass
# reconnaissance-only rather than a verdict.
# CHECKED. An unchecked `source` of a missing or truncated library left `_gate_lock_acquire`
# undefined; the `||` branch below then exited WITHOUT invalidating, so an older `completed=1` stayed
# readable after an attempted run. Loading the library is the only step permitted before the lock,
# because the lock is what makes writing the artifact legitimate.
# The same unavoidable gap as the campaign, stated rather than hidden: if the locking library will
# not load there is no lock to take, so nothing may be written, so an earlier record survives. Saying
# so turns a silent stale artifact into a reported one.
_ci_fresh_fail() {
  echo "FATAL: $1" >&2
  echo "       The repository lock could not be taken, so this run wrote NOTHING." >&2
  echo "       .ci-gates-summary may still hold an EARLIER run's result — including completed=1." >&2
  echo "       Do not read it as describing this attempt." >&2
  exit 2
}
# THIS RUNNER MANAGES THE LOCK ITSELF, and must say so before the library can arm anything.
#
# Its policy is fail-closed on interruption: gates run as backgrounded children, so a TERM delivered
# only to this runner leaves them building, and releasing would hand the repository to a new run
# while they work. The library's automatic release — correct for an ordinary gate, which otherwise
# strands the lock — composes itself into the `trap 'RUN_INTERRUPTED=1' INT TERM HUP` below and
# would release on exactly the signal this runner refuses to release on.
_GATE_LOCK_SELF_MANAGED=1
source "$ROOT_DIR/scripts/tests/lib/fresh.sh" 2>/dev/null \
  || _ci_fresh_fail "could not load scripts/tests/lib/fresh.sh — refusing to run unlocked."
command -v _gate_lock_acquire >/dev/null 2>&1 \
  || _ci_fresh_fail "fresh.sh loaded but _gate_lock_acquire is missing."
# INSIDE A GATE SESSION THIS RUNNER NEVER RELEASES THE LOCK, on any path (bug 244). Every exit — a
# refusal after a failed build as much as the end of a pass — may leave a process behind that is
# still working the tree, and the lock is what keeps the next run off it. The session launcher sweeps
# the group and then reclaims the lock on the kernel's word that the group is empty.
if [ "$CI_SESSION_OWNER" = "1" ]; then
  _gate_lock_release() { return 0; }
fi

# INVALIDATE THE COMPLETION ARTIFACT BEFORE ANYTHING CAN FAIL.
#
# The only write was at the very END of the pass, so every early exit — lock unavailable, missing
# binary, bad arguments, no commands extracted — left the PREVIOUS pass's artifact untouched and
# readable. Found 2026-08-21 with the repository in exactly that state: `.ci-gates-summary` said
# `completed=1 head=a839d519` while HEAD was `1b615650`, so an attempted-and-refused run presented a
# different commit's completion as its own. A stale completion record is worse than none, because it
# answers a question about a run that never happened.
CI_ARTIFACT="$ROOT_DIR/.ci-gates-summary"
ci_write_summary() { # completed refusals ; atomic — a truncate-in-place write can be interrupted
  # mktemp, not a predictable "$file.$$.tmp" a symlink could be planted at.
  local tmp
  tmp="$(mktemp "$CI_ARTIFACT.XXXXXX" 2>/dev/null)" || { echo "error: could not create a temp artifact" >&2; return 1; }
  if ! { echo "completed=$1"
         echo "mode=$([ "${CLOSURE:-0}" = 1 ] && echo closure || echo ordinary)"
         echo "jobs=${JOBS:-?}"
         echo "discovered=${DISCOVERED:-0}"
         echo "executed=${EXECUTED:-0}"
         echo "passed=${PASS:-0}"
         echo "failed=${FAIL:-0}"
         echo "head=${START_HEAD:-$(git -C "$ROOT_DIR" rev-parse HEAD 2>/dev/null || echo no-head)}"
         echo "compiler_identity=${START_COMPILER:-unknown}"
         # RECORDED, not merely compared. With only `head=`, a completed=1 artifact could not be told
         # apart from one describing an UNCOMMITTED tree: a stable dirty change outside the
         # mutation-target set passes the equality checks and never appears in the record.
         echo "tracked_sha=${START_TRACKED:-unknown}"
         echo "untracked_sha=${START_UNTRACKED:-unknown}"
         # Stated in the record, so completed=1 cannot be read as covering what this runner cannot run.
         echo "excludes=check_gate_mutation_coverage.sh${EXCLUSIONS:-}"
         echo "host_skipped=${HOST_SKIPPED_N:-0}"
         echo "deferred=${DEFERRED:-0}"
         [ -z "${DEFERRED_GATES:-}" ] || echo "deferred_gates=${DEFERRED_GATES}"
         [ -n "$2" ] && echo "refusals=$2"
         true
       } > "$tmp" 2>/dev/null; then
    echo "error: could not write $CI_ARTIFACT" >&2; rm -f "$tmp" 2>/dev/null; return 1
  fi
  mv -f "$tmp" "$CI_ARTIFACT" 2>/dev/null || {
    echo "error: could not install $CI_ARTIFACT" >&2; rm -f "$tmp" 2>/dev/null; return 1; }
  return 0
}
# THE LOCK COMES FIRST. Invalidating before acquiring let a second invocation overwrite a LIVE
# run's `completed=1` with `completed=0` and then be refused by the lock — the first run still
# printing success while its authority record had already been replaced. Nothing is written on the
# lock-unavailable path: the artifact belongs to whoever holds the lock.
_gate_lock_acquire || {
  echo "error: another run holds the repository lock." >&2
  echo "       This run wrote NOTHING. .ci-gates-summary may still hold an EARLIER run's result —" >&2
  echo "       including completed=1. Do not read it as describing this attempt." >&2
  exit 2; }
# If the invalidation cannot be WRITTEN after the lock is held, DELETE the old record instead: an
# absent artifact reads as "no completion", while a surviving one describes an earlier run as if it
# were this attempt.
# PRIOR FAILURE LOGS ARE CLEARED ONCE, IMMEDIATELY AFTER THE LOCK IS TAKEN.
#
# Two wrong places, tried in that order. Clearing them only on selected exits left post-lock
# bad-argument, inventory and treestate refusals with a previous run's diagnostics on disk and no
# run/head binding. Clearing them "at the top" was WORSE: it ran BEFORE lock acquisition, so a second
# cooperative invocation deleted the LIVE holder's failure logs — the retained evidence for its failed
# gates — and then refused to run. Shared state may only be touched by whoever holds the lock, which
# makes this the one correct point: after acquisition, before any refusal taken while holding it.
rm -rf "$ROOT_DIR/.ci-gate-failures" 2>/dev/null || true

if ! ci_write_summary 0 " run_did_not_reach_reconciliation"; then
  if rm -f "$CI_ARTIFACT" 2>/dev/null && [ ! -e "$CI_ARTIFACT" ]; then
    echo "warning: could not write the invalidation record, so the previous one was DELETED." >&2
  else
    echo "FATAL: could not discredit the previous $CI_ARTIFACT." >&2
  fi
  _gate_lock_release; exit 2
fi
# FRESHNESS, NOT MERE EXISTENCE. This checked only that the binary was executable. Gates that call
# `require_fresh_binary` rebuild for themselves, but the ones that do not — the first extracted
# command, scripts/ci/proof_gate.sh, among them — ran against whatever was on disk, so a closure
# run could record the current HEAD while its early verdicts described an older compiler.
# MACHINE-WIDE LOAD CONTROL (R-0543, scripts/tests/lib/heavy_slots.sh). A full pass, or a selection
# larger than CONCRETE_HEAVY_MIN_GATES (default 8; the pre-push hook's job selections are), takes one
# of CONCRETE_HEAVY_SLOTS machine-wide slots BEFORE the rebuild below, because a fresh worktree's
# build is itself heavy. Small selections and single gates never wait. Taken after the repository
# lock, so a waiting run holds only its own worktree. Released at the end; a killed run's slot is
# reclaimed by the next waiter (its pid is dead).
HEAVY_RUN=0
if source "$ROOT_DIR/scripts/tests/lib/heavy_slots.sh" 2>/dev/null && [ -z "$ARG_ERR" ] && resolve_selection 2>/dev/null; then
  if [ "$SELECTING" = "0" ] || [ "${#SELECTED[@]}" -gt "${CONCRETE_HEAVY_MIN_GATES:-8}" ]; then HEAVY_RUN=1; fi
fi
if [ "$HEAVY_RUN" = "1" ]; then
  _hargs=(); [ "$HEAVY_NO_WAIT" = "1" ] && _hargs=(--no-wait)
  heavy_slot_acquire "run_ci_gates_local.sh (${#SELECTED[@]} gates)" ${_hargs[@]+"${_hargs[@]}"}; _hrc=$?
  if [ "$_hrc" -ne 0 ]; then
    ci_write_summary 0 " heavy_slot_unavailable"; _gate_lock_release
    exit "$_hrc"
  fi
fi
if ! require_fresh_binary ".lake/build/bin/concrete"; then
  ci_write_summary 0 " compiler_not_fresh"
  echo "error: refusing to run the gate set against an unverified compiler." >&2
  _gate_lock_release
  exit 2
fi
# The nightly fuzz steps derive SEED from the date in CI; the extracted fuzz
# commands reference "$SEED", which would trip `set -u` here. Mirror CI.
SEED="${SEED:-$(date -u +%Y%m%d)}"
# The acceptance step writes its evidence under $RUNNER_TEMP on CI. Here it gets a per-run directory,
# removed at the end, so two passes on one host never share it (R-0546).
if [ -z "${RUNNER_TEMP:-}" ]; then RUNNER_TEMP="$(mktemp -d)"; _CI_RUNNER_TEMP_OWNED=1; fi
export RUNNER_TEMP

# Gates are independent processes (each uses its own mktemp -d), so they
# parallelize cleanly, and PARALLEL IS THE DEFAULT: on a multicore box this cuts
# a full pass from ~20min to a few, and CI itself fans out (ten jobs all declare
# `needs: build`), so a sequential local pass is slower than the thing it is
# meant to pre-empt.
#
# What makes a parallel default safe is the sequential RE-RUN of any failure
# below: a contention flake self-corrects, so the verdict does not get noisier as
# JOBS rises. What it costs is interleaved output ordering — failures still print,
# but not in list order. `JOBS=1` restores sequential streaming when a run needs
# to be read live.
#
# The cap lives HERE and nowhere else. The pre-push hook used to compute its own
# `min(cores, 8)` and pass it in, which is the same fact stated in two places —
# the defect class this tree keeps paying for. The hook now just calls the script.
CORES="$( (sysctl -n hw.ncpu 2>/dev/null || nproc 2>/dev/null) || echo 4 )"
# Leave two cores for the OS and the editor; cap at 8 because the heavy gates
# each spawn the compiler over many .con files and contention past that buys
# little while making flakes (and their re-runs) more likely.
# Captured BEFORE defaulting, so closure mode can tell "the operator asked for JOBS=4" from "nobody
# said anything and the default happens to be 4". Refusing the first and silently fixing the second
# is the correct pair of behaviours; without this they are indistinguishable.
JOBS_EXPLICIT=0
[ -n "${JOBS:-}" ] && JOBS_EXPLICIT=1
JOBS_DEFAULT=$(( CORES - 2 ))
[ "$JOBS_DEFAULT" -lt 1 ] && JOBS_DEFAULT=1
[ "$JOBS_DEFAULT" -gt 8 ] && JOBS_DEFAULT=8
# R-0543: with more than one machine-wide heavy slot the machine is shared, so the default becomes
# this run's fair share of the free cores (heavy_jobs_default), not a fixed 8.
if command -v heavy_jobs_default >/dev/null 2>&1 && [ "$(heavy_slots_n)" -gt 1 ]; then
  JOBS_DEFAULT="$(heavy_jobs_default)"
fi
# A HOST DEFAULT (R-0546). concrete-remote sets CONCRETE_GATE_JOBS_DEFAULT on the CI server, where the
# measured best width (docs/project/GATE_PROFILE.md) is far above the laptop rule. It is a DEFAULT,
# not an explicit JOBS: an explicit JOBS still wins, and closure mode still forces JOBS=1 without
# refusing, because the operator did not ask for this width.
case "${CONCRETE_GATE_JOBS_DEFAULT:-}" in
  ""|*[!0-9]*|0) ;;
  *) JOBS_DEFAULT="$CONCRETE_GATE_JOBS_DEFAULT" ;;
esac
JOBS="${JOBS:-$JOBS_DEFAULT}"

# ---------------------------------------------------------------------------
# CLOSURE MODE. `CONCRETE_CLOSURE_RUN=1` (or --closure) makes this run capable of producing a
# completion record. It forces JOBS=1 and refuses anything else.
#
# Why it must be explicit rather than the default: JOBS_DEFAULT is cores-2, so "run the complete gate
# set serially" ran four gates wide for 38 minutes and would have printed an ordinary-looking
# summary. The repository lock did not prevent it and could not — the lock serializes RUNS against
# each other and is deliberately re-entrant within a run, so this runner's own fan-out passes
# straight through it. Verifying that the lock refuses a second run is not verifying serialism.
#
# A parallel pass may still run and report. It simply cannot produce `completed=1`.
CLOSURE=0
[ "${CONCRETE_CLOSURE_RUN:-0}" = "1" ] && CLOSURE=1
if [ "$CLOSURE" = "1" ]; then
  # REFUSE rather than silently override: quietly ignoring an explicit JOBS= is its own defect, and
  # the operator would be told nothing about why their flag did not apply.
  if [ -n "${JOBS+x}" ] && [ "${JOBS_EXPLICIT:-0}" = "1" ] && [ "$JOBS" != "1" ]; then
    echo "error: closure mode requires JOBS=1, but JOBS=$JOBS was given explicitly." >&2
    echo "       Re-run without JOBS, or with JOBS=1." >&2
    _gate_lock_release
    exit 2
  fi
  JOBS=1
fi
export SEED

# RESOLVE THE SELECTION (see resolve_selection above) under the lock, so a refusal is recorded by
# the holder. Bad arguments are reported here for the same reason.
if [ -n "$ARG_ERR" ]; then
  echo "error: $ARG_ERR" >&2
  ci_write_summary 0 " bad_arguments"; _gate_lock_release; exit 2
fi
if ! resolve_selection; then
  echo "error: $RESOLVE_MSG" >&2
  ci_write_summary 0 " $RESOLVE_ERR"; _gate_lock_release; exit 2
fi

# ---------------------------------------------------------------------------
# START-STATE CAPTURE for completion integrity.
#
# `completed=1` is a claim that this run measured the tree it says it measured. Nothing established
# that before: a run that mutated a source and died, or that ran while someone else edited, produced
# a summary indistinguishable from a clean one. Two contaminations in one day motivated this — a
# 38-minute parallel pass, and a SIGKILL that left Concrete/Elab/Elab.lean holding `declSpan := none`
# while the harness had already exited.
#
# Captured here and re-captured at the end. Any difference names a DIMENSION and suppresses
# completion; the refusal is diagnostic because "completed=0" with no reason is the same failure
# shape as a gate that dies silently.
# TREE STATE COMES FROM THE SHARED LIBRARY. These were a SECOND implementation of the campaign's
# snapshots, and the two drifted: when the campaign's were corrected to hash tracked CONTENT (rather
# than `git status` text, which does not change when an already-modified file is edited again) and to
# see STAGED mutations, this copy kept the original defects. One fact, one producer.
# shellcheck source=scripts/tests/lib/treestate.sh
source "$ROOT_DIR/scripts/tests/lib/treestate.sh" 2>/dev/null || true
ts_require || { ci_write_summary 0 " treestate_library_unavailable"; _gate_lock_release; exit 2; }
snapshot_head()      { ts_head "$ROOT_DIR"; }
snapshot_tracked()   { ts_tracked "$ROOT_DIR"; }
snapshot_untracked() { ts_untracked "$ROOT_DIR"; }
snapshot_gates()     { printf '%s\n' "${CMDS[@]}" | LC_ALL=C sort | cksum | cut -d' ' -f1; }
snapshot_compiler()  { grep -oE 'buildIdentity : String := "[0-9a-f]+"' Concrete/BuildIdentity.lean 2>/dev/null | grep -oE '[0-9a-f]{32}' | head -1 || echo "none"; }
# Mutation targets specifically: the files any registered mutation names. A leftover mutation is the
# one contamination that no trap can prevent, so it is checked by name rather than folded into the
# tracked-tree hash — the diagnosis "a mutation target was not restored" is far more actionable than
# "the tree changed".
snapshot_muttargets() {
  # shellcheck disable=SC2046,SC2086
  # EVERY TARGET EITHER HARNESS NAMES, not just Concrete/*.lean and Main.lean. The in-place harness
  # also mutates `std/src/*.con`, and the campaign registers `scripts/gen/*.sh`; a stranded mutation in
  # those paths appeared in NEITHER cleanliness check, and start/end tracked hashes stay equal for a
  # mutation that was already there when the run began.
  ts_dirty_files "$ROOT_DIR" $( { grep -hoE '"(Concrete|Main|std|scripts|examples|proofs)[A-Za-z0-9/._-]*\.(lean|con|sh)"' \
      scripts/tests/test_mutation.sh scripts/tests/check_gate_mutation_coverage.sh 2>/dev/null \
      | tr -d '"' | LC_ALL=C sort -u; } )
}
START_HEAD="$(snapshot_head)"
START_TRACKED="$(snapshot_tracked)"
START_UNTRACKED="$(snapshot_untracked)"
START_GATES="$(snapshot_gates)"
START_COMPILER="$(snapshot_compiler)"
START_MUT="$(snapshot_muttargets)"
# INITIALISED TO 0, with the trap as the ONLY writer. This was seeded to 1 as a "has not finished
# yet" sentinel and cleared by an unconditional reset just before reconciliation — which is exactly
# what made `run_interrupted` inert. Removing that reset without re-seeding this made the dimension
# fire on EVERY run instead, so the flag now means only what its name says: a signal arrived.
RUN_INTERRUPTED=0
REACHED_END=0
# AN INTERRUPTION STOPS THE WHOLE RUN, not just this shell (bug 244). The session owner signals its
# group, so a signal sent to this process alone still reaches every gate; the guard keeps the signal
# it sends itself from re-entering. Nothing new launches once the flag is set (see the loops below).
_ci_on_signal() {
  [ "$RUN_INTERRUPTED" = "1" ] && return 0
  RUN_INTERRUPTED=1
  [ "$CI_SESSION_OWNER" = "1" ] && kill -TERM -- "-$CONCRETE_GATE_SESSION" 2>/dev/null
  return 0
}
trap '_ci_on_signal' INT TERM HUP

# HOST TOOLS. A workflow command can need a tool only the CI image has (the Examples job runs
# check_example_outputs.sh under EXAMPLE_VALGRIND=1, and macOS has no valgrind). The gate refuses to
# run without it, which is right for CI; here that refusal would read as a red tree. Such a command is
# NOT run and NOT passed: it is listed by name as host-skipped, and the skip is a completion-blocking
# note (host_tool_missing below), so the run reports PARTIAL and can never write completed=1 — the
# same standing as a deliberate subset. The table is scripts/tests/lib/host_tools.sh.
source "$ROOT_DIR/scripts/tests/lib/host_tools.sh" 2>/dev/null \
  && command -v host_tool_missing >/dev/null 2>&1 \
  || { echo "error: could not load scripts/tests/lib/host_tools.sh" >&2
       ci_write_summary 0 " host_tools_library_unavailable"; _gate_lock_release; exit 2; }
HOST_SKIPPED=(); HOST_SKIP_TOOLS=""
RUN_LIST=(); RUN_FIRST_N=0
for (( _i=0; _i<${#SELECTED[@]}; _i++ )); do
  _c="${SELECTED[$_i]}"
  if _miss="$(host_tool_missing "$_c")"; then
    HOST_SKIPPED+=("$_c")
    echo "  SKIP $_c (host lacks: $_miss — CI runs it; this run cannot be a full pass)"
    for _t in $_miss; do
      case ",$HOST_SKIP_TOOLS," in *",$_t,"*) ;; *) HOST_SKIP_TOOLS="$HOST_SKIP_TOOLS${HOST_SKIP_TOOLS:+,}$_t" ;; esac
    done
  else
    RUN_LIST+=("$_c")
    [ "$_i" -lt "$FIRST_N" ] && RUN_FIRST_N=$((RUN_FIRST_N+1))
  fi
done
HOST_SKIPPED_N="${#HOST_SKIPPED[@]}"

PASS=0; FAIL=0; FAILED=""
# DEFERRED (bug 315): a declared gate that exited GATE_DEFERRED_RC; neither passed nor failed.
DEFERRED=0; DEFERRED_GATES=""; DEFERRED_LIST=""; DEFERRED_NL=""
_defer() { # cmd -> records a deferral
  DEFERRED=$((DEFERRED+1)); DEFERRED_LIST="$DEFERRED_LIST\n  DEFERRED $1"
  DEFERRED_GATES="$DEFERRED_GATES${DEFERRED_GATES:+; }$1"; DEFERRED_NL="$DEFERRED_NL$1"$'\n'
}

# A FAILING GATE'S OUTPUT IS KEPT. Discarding it means a failure in a 40-minute pass leaves nothing
# to diagnose but the gate's name — and a gate that passes standalone but fails inside the pass is
# then undiagnosable by construction, because reproducing it means re-running the whole thing. That
# happened to check_clean_checkout on the first closure-candidate run.
#
# Only FAILING gates are kept, so a green pass writes nothing: the point is a diagnosis when one is
# needed, not a transcript nobody reads.
FAILLOG_DIR="$ROOT_DIR/.ci-gate-failures"
rm -rf "$FAILLOG_DIR"; mkdir -p "$FAILLOG_DIR"
faillog_name(){ printf '%s' "$1" | tr -c 'A-Za-z0-9_.-' '_' | cut -c1-80; }

# PER-GATE TIMINGS (R-0546). One TSV row per gate execution: start and end (epoch seconds, sub-second
# where bash has EPOCHREALTIME), duration, stage and verdict. Stage is `first`, `seq`, `pool` or
# `recheck`; a pool FAIL followed by a recheck PASS is the contention flake the summary names, and is
# visible here rather than only as a count. Written for every run, green or red.
GATE_TIMINGS="$ROOT_DIR/.ci-gate-timings.tsv"
_now() { printf '%s' "${EPOCHREALTIME:-$(date +%s)}"; }
_timing_row() { # start end stage verdict cmd
  awk -v s="$1" -v e="$2" -v st="$3" -v v="$4" -v c="$5" \
    'BEGIN{ printf "%.2f\t%.2f\t%.2f\t%s\t%s\t%s\n", s, e, e-s, st, v, c }' >> "$GATE_TIMINGS"
}
printf 'start\tend\tsecs\tstage\tverdict\tcmd\n' > "$GATE_TIMINGS"

# ONE SEQUENTIAL VERDICT, used by the first stage, JOBS=1 and the parallel re-run alike.
# GATE OUTPUT IS WRITTEN OUTSIDE THE CHECKOUT and moved into $FAILLOG_DIR only on failure (R-0546).
# Creating and deleting a log per gate inside the checkout churned the tree that other gates copy
# (`cp -a` of the checkout in the mutation driver refused when a file vanished under it).
GATE_OUT_TMP="$(mktemp -d)"
run_seq() { # cmd [stage] -> 0 pass / 1 fail; output kept only on failure
  local _out="$FAILLOG_DIR/$(faillog_name "$1").log" _t0 _st="${2:-seq}" _tmp
  _tmp="$GATE_OUT_TMP/seq.$RANDOM$RANDOM.log"
  _t0="$(_now)"
  # BACKGROUNDED AND WAITED FOR, so a signal is acted on now: bash defers a trap until a
  # foreground command returns, and a gate can run for many minutes. After an interruption the gate is
  # not waited for — it may ignore SIGTERM, and the session launcher escalates — and it counts as FAIL.
  local _p _rc
  if [ "$RUN_INTERRUPTED" = "1" ]; then
    _timing_row "$_t0" "$(_now)" "$_st" FAIL "$1"
    FAIL=$((FAIL+1)); FAILED="$FAILED\n  FAIL $1 (not run: the run was interrupted)"; return 1
  fi
  eval "$1" >"$_tmp" 2>&1 &
  _p=$!
  wait "$_p"; _rc=$?
  while [ "$RUN_INTERRUPTED" != "1" ] && kill -0 "$_p" 2>/dev/null; do wait "$_p"; _rc=$?; done
  [ "$RUN_INTERRUPTED" = "1" ] && _rc=1
  if [ "$_rc" = "0" ]; then
    PASS=$((PASS+1)); rm -f "$_tmp"; _timing_row "$_t0" "$(_now)" "$_st" PASS "$1"; return 0
  fi
  local _why=""
  if [ "$_rc" = "$GATE_DEFERRED_RC" ]; then
    # Kept, like a failure's: what this host did not judge is worth reading.
    if _why="$(gate_deferral "$1" "$WORKFLOW")"; then
      mv -f "$_tmp" "${_out%.log}.deferred.log" 2>/dev/null || cp -f "$_tmp" "${_out%.log}.deferred.log" 2>/dev/null
      _timing_row "$_t0" "$(_now)" "$_st" DEFERRED "$1"; _defer "$1"
      echo "  DEFERRED $1 (not judged on this host; CI decides: ${_out#$ROOT_DIR/} -> .deferred.log)"
      return 0
    fi
  fi
  mv -f "$_tmp" "$_out" 2>/dev/null || cp -f "$_tmp" "$_out" 2>/dev/null
  _timing_row "$_t0" "$(_now)" "$_st" FAIL "$1"
  FAIL=$((FAIL+1)); FAILED="$FAILED\n  FAIL $1${_why:+ ($_why)}"; echo "  FAIL $1 (output: ${_out#$ROOT_DIR/})${_why:+ -- $_why}"
  return 1
}

# THE FIRST STAGE runs sequentially, in order, before anything else launches, and a failure there
# stops the run: the hook's fast surface checklist used to run first and fail the push in seconds,
# and folding it into one pool must not turn that into a failure reported after minutes of other gates.
STOPPED_EARLY=0
for (( _i=0; _i<RUN_FIRST_N; _i++ )); do
  [ "$RUN_INTERRUPTED" = "1" ] && { STOPPED_EARLY=1; break; }
  run_seq "${RUN_LIST[$_i]}" first || { STOPPED_EARLY=1; break; }
done
REST=("${RUN_LIST[@]:$RUN_FIRST_N}")
[ "$STOPPED_EARLY" = "1" ] && REST=()

# LONGEST FIRST (R-0546). The pool's wall time is bounded below by its longest gate, and a 6-minute
# gate launched last finishes 6 minutes after everything else. scripts/tests/gate_weights.tsv holds
# measured seconds per command (from .ci-gate-timings.tsv, see docs/project/GATE_PROFILE.md); the
# pool starts the heaviest first and keeps the workflow order among the rest. Ordering only: a stale
# or missing weight changes when a gate starts, never whether it runs or what it concludes.
GATE_WEIGHTS="$ROOT_DIR/scripts/tests/gate_weights.tsv"
if [ "$JOBS" -gt 1 ] && [ -f "$GATE_WEIGHTS" ] && [ "${#REST[@]}" -gt 1 ]; then
  _ordered=()
  while IFS= read -r _line; do _ordered+=("${_line#*$'\t'*$'\t'}"); done < <(
    for (( _i=0; _i<${#REST[@]}; _i++ )); do printf '%s\t%s\t%s\n' "$_i" "$_i" "${REST[$_i]}"; done \
      | awk -F'\t' 'NR==FNR { if ($0 !~ /^#/ && NF >= 2) w[$2] = $1; next }
                    { c = $0; sub(/^[^\t]*\t[^\t]*\t/, "", c); printf "%d\t%d\t%s\n", (c in w ? w[c] : 0), $1, c }' \
          "$GATE_WEIGHTS" - \
      | LC_ALL=C sort -t$'\t' -k1,1nr -k2,2n)
  # Same multiset of commands or no reordering at all: ordering must never add, drop or alter one.
  if [ "${#_ordered[@]}" -eq "${#REST[@]}" ] \
     && [ "$(printf '%s\n' "${_ordered[@]}" | LC_ALL=C sort | cksum)" = "$(printf '%s\n' "${REST[@]}" | LC_ALL=C sort | cksum)" ]; then
    REST=("${_ordered[@]}")
  else
    echo "warning: gate_weights ordering produced a different command list; keeping workflow order" >&2
  fi
fi

if [ "$JOBS" -le 1 ]; then
  for cmd in "${REST[@]}"; do
    [ "$RUN_INTERRUPTED" = "1" ] && break
    run_seq "$cmd" || true
  done
else
  # Parallel pool: each gate writes "OK"/"FAIL <cmd>" to its own result file.
  RES=$(mktemp -d); trap 'rm -rf "$RES"' EXIT
  idx=0
  for cmd in "${REST[@]}"; do
    # throttle to JOBS concurrent
    while [ "$RUN_INTERRUPTED" != "1" ] && [ "$(jobs -rp | wc -l)" -ge "$JOBS" ]; do wait -n 2>/dev/null || true; done
    [ "$RUN_INTERRUPTED" = "1" ] && break
    idx=$((idx+1))
    # The pool's own output is kept on failure too (<name>.pool.log): a gate that fails only under
    # load is a defect in the gate, and the sequential recheck's output cannot show why it failed.
    ( _t0="$(_now)"; _pl="$GATE_OUT_TMP/pool.$idx.log"
      eval "$cmd" >"$_pl" 2>&1; _prc=$?
      if [ "$_prc" = 0 ]; then rm -f "$_pl"; _timing_row "$_t0" "$(_now)" pool PASS "$cmd"; echo "OK" > "$RES/$idx"
      elif [ "$_prc" = "$GATE_DEFERRED_RC" ]; then
        # Judged in the parent (gate_deferral): a deferral, or a failure from an undeclared gate.
        mv -f "$_pl" "$FAILLOG_DIR/$(faillog_name "$cmd").deferred.log" 2>/dev/null
        _timing_row "$_t0" "$(_now)" pool DEFERRED "$cmd"; echo "DEFER $cmd" > "$RES/$idx"
      else mv -f "$_pl" "$FAILLOG_DIR/$(faillog_name "$cmd").pool.log" 2>/dev/null
        _timing_row "$_t0" "$(_now)" pool FAIL "$cmd"; echo "FAIL $cmd" > "$RES/$idx"; fi ) &
  done
  LAUNCHED=$idx
  wait
  # The heavy gates spawn the compiler over many .con files; running JOBS of
  # them at once can OOM/CPU-starve and produce a spurious failure (a clean
  # tree then reads as red — the exact "crying wolf" that erodes trust in
  # this pre-push check). So a parallel FAIL is only a *candidate*: re-run it
  # once, sequentially and unthrottled, and believe that verdict. Genuine
  # failures still fail; contention flakes self-correct.
  # EVERY LAUNCHED CHILD MUST HAVE LEFT A WELL-FORMED RESULT.
  #
  # Three ways this loop turned lost results into a pass. A child killed before writing left NO file
  # and was silently skipped by the glob. A child killed mid-write left an EMPTY file: `grep -q ^OK$`
  # failed, `sed` produced an empty command, and `eval ""` SUCCEEDS — so the missing result was
  # counted as PASS. And for filtered runs the discovered-vs-executed check is deliberately suppressed
  # as deliberate scope, so neither loss showed up there either. ENOSPC or an interrupted child could
  # therefore produce a green partial run out of results that never existed.
  RECHECKED=""
  RESULTS_SEEN=0
  for f in "$RES"/*; do
    [ -f "$f" ] || continue
    RESULTS_SEEN=$((RESULTS_SEEN+1))
    if grep -q '^OK$' "$f"; then PASS=$((PASS+1)); continue; fi
    if grep -q '^DEFER ' "$f"; then
      cmd="$(sed 's/^DEFER //' "$f")"
      if _why="$(gate_deferral "$cmd" "$WORKFLOW")"; then
        _defer "$cmd"; echo "  DEFERRED $cmd (not judged on this host; CI decides)"
      else
        mv -f "$FAILLOG_DIR/$(faillog_name "$cmd").deferred.log" "$FAILLOG_DIR/$(faillog_name "$cmd").pool.log" 2>/dev/null
        FAIL=$((FAIL+1)); FAILED="$FAILED\n  FAIL $cmd ($_why)"; echo "  FAIL $cmd -- $_why"
      fi
      continue
    fi
    if ! grep -q '^FAIL ' "$f"; then
      FAIL=$((FAIL+1))
      FAILED="$FAILED\n  FAIL <result file $f is empty or malformed — the child died before recording>"
      echo "  FAIL <lost result: child $f left no usable verdict>"
      continue
    fi
    cmd="$(sed 's/^FAIL //' "$f")"
    # The re-run's output is KEPT on failure, as in sequential mode. Parallel is the default, and it
    # discarded both runs' output — so the hook's fast surface checklist, whose own report names the
    # failing constituent, failed with nothing but its name.
    if run_seq "$cmd" recheck; then
      RECHECKED="$RECHECKED\n  (contention flake, passed on sequential re-run) $cmd"
    fi
  done
  [ -n "$RECHECKED" ] && echo -e "Recovered under sequential re-run:$RECHECKED"
  # A child that vanished entirely leaves no file for the loop above to judge.
  if [ "${RESULTS_SEEN:-0}" -ne "${LAUNCHED:-0}" ]; then
    FAIL=$((FAIL+1))
    FAILED="$FAILED\n  FAIL <${LAUNCHED:-0} gates launched but only ${RESULTS_SEEN:-0} results recorded>"
    echo "  FAIL <${LAUNCHED:-0} launched, ${RESULTS_SEEN:-0} results — some child left nothing behind>"
  fi
fi

# REACHED_END replaces an unconditional `RUN_INTERRUPTED=0` that sat exactly here, immediately before
# reconciliation, clearing whatever the INT/TERM/HUP trap had recorded — so `run_interrupted` could
# never appear in a completed run. bash defers a trap until the running foreground command returns,
# and a gate can run for minutes, so a signal arriving mid-gate was routinely forgotten.
REACHED_END=1

# NOTHING THE GATES STARTED MAY OUTLIVE THEM UNACCOUNTED (bug 244). Inside a gate session every
# process this pass started is a member of the session's group — except a campaign group registered
# in the lock, which is swept too. Anything still alive here was left behind by a gate (or, after an
# interruption, has not stopped yet): it is terminated (SIGTERM, the grace period, SIGKILL) BEFORE the
# tree is reconciled, the summary written and the heavy slot released, and it blocks completion,
# because a process still working the tree after its gate "passed" makes the verdict describe a tree
# that kept changing. Listing excludes this shell, the session leader and the `ps` doing the listing;
# zombies are not processes.
_ci_session_members() { # file -> member pids, one per line; nonzero when ps could not answer
  ps -A -o pid=,ppid=,pgid=,stat=,comm= > "$1.ps" 2>/dev/null || return 1
  awk -v g="$CONCRETE_GATE_SESSION" -v me="$$" '
    $3 == g && $1 != me && $1 != g && $4 !~ /^Z/ && !($2 == me && $5 ~ /(^|\/)ps$/) { print $1 }
  ' "$1.ps" > "$1"
  local d="${CONCRETE_GATE_LOCK#*:}" e
  [ -d "$d/groups" ] || return 0
  for e in "$d"/groups/*; do
    case "${e##*/}" in ''|*[!0-9]*) continue ;; esac
    kill -0 -- "-${e##*/}" 2>/dev/null && echo "-${e##*/}" >> "$1"
  done
  return 0
}
CI_SURVIVORS=0
if [ "$CI_SESSION_OWNER" = "1" ]; then
  _sw="$GATE_OUT_TMP/session.members"
  if ! _ci_session_members "$_sw"; then
    CI_SURVIVORS="unknown"; CI_KEEP_HEAVY_SLOT=1
  elif [ -s "$_sw" ]; then
    CI_SURVIVORS="$(grep -c . "$_sw")"
    _grace="${CONCRETE_GATE_SESSION_GRACE:-10}"; _grace="${_grace%.*}"; [ -n "$_grace" ] || _grace=10
    # EVERY POLL RE-LISTS AND RE-SIGNALS: a member that forked after the previous signal is sent one
    # too. SIGTERM for the grace period, then SIGKILL for 5 s; emptiness must be CONFIRMED before the
    # summary and the heavy slot are handed on.
    _empty=0
    for _phase in "TERM:$((_grace*5))" "KILL:25"; do
      _psig="${_phase%%:*}"; _pn="${_phase#*:}"
      for (( _i=0; _i<_pn; _i++ )); do
        # shellcheck disable=SC2046
        [ -s "$_sw" ] && kill -"$_psig" -- $(cat "$_sw") 2>/dev/null
        sleep 0.2
        if _ci_session_members "$_sw" && [ ! -s "$_sw" ]; then _empty=1; break 2; fi
      done
    done
    if [ "$_empty" != "1" ]; then
      CI_SURVIVORS="${CI_SURVIVORS}_unterminated"
      CI_KEEP_HEAVY_SLOT=1
      echo "  WARNING processes of this run are STILL ALIVE after SIGKILL: $(tr '\n' ' ' < "$_sw")" >&2
      echo "          the lock stays held until they are gone, and this run does not release its heavy slot" >&2
    fi
    [ "$RUN_INTERRUPTED" = "1" ] \
      || echo "  WARNING $CI_SURVIVORS process(es) outlived their gates and were terminated; this run cannot be a closure run" >&2
  fi
fi

# ---------------------------------------------------------------------------
# COMPLETION INTEGRITY. Each dimension that moved is NAMED, so a suppressed completion says which
# thing changed rather than only that something did.
REFUSALS=""
# jobs_not_serial is a MODE fact, not tree corruption — it belongs with the deliberate-scope notes
# below. Parallel is this runner's documented DEFAULT (JOBS_DEFAULT = cores-2), so classifying it as
# an integrity refusal made the ordinary `make` invocation at Makefile:50 exit nonzero on a completely
# clean pass. It still cannot produce completed=1: interleaved gates share one .lake tree, and that is
# exactly why closure mode forces JOBS=1.
MODE_NOTES=""
[ "$JOBS" = "1" ] || MODE_NOTES=" jobs_not_serial(jobs=$JOBS)"
# Survivors (bug 244; swept above) block completed=1 in every mode: the tree may have changed after
# the gate that left them reported. They are a completion fact, not a gate failure: the gates' own
# verdicts stand, and the warning above names how many there were.
[ "$CI_SURVIVORS" = "0" ] || MODE_NOTES="$MODE_NOTES processes_outlived_gates($CI_SURVIVORS)"
# The in-band marker must REFUSE, not be compared: two identical markers satisfy equality exactly
# like two identical digests. The campaign rejected it and this producer did not.
case "$START_HEAD$START_TRACKED$START_UNTRACKED" in
  *TREESTATE-UNAVAILABLE*) REFUSALS="$REFUSALS tree_state_unavailable_at_start" ;;
esac
case "$(snapshot_head)$(snapshot_tracked)$(snapshot_untracked)" in
  *TREESTATE-UNAVAILABLE*) REFUSALS="$REFUSALS tree_state_unavailable_at_end" ;;
esac
# EMPTY IS NOT A DIGEST — the campaign rejects this and the runner did not, so the same
# empty-equals-empty hole stayed open in one of the two producers of the same fact.
for _v in "$START_HEAD" "$START_TRACKED" "$START_UNTRACKED"; do
  [ -n "$_v" ] || { REFUSALS="$REFUSALS tree_state_empty_at_start"; break; }
done
for _v in "$(snapshot_head)" "$(snapshot_tracked)" "$(snapshot_untracked)"; do
  [ -n "$_v" ] || { REFUSALS="$REFUSALS tree_state_empty_at_end"; break; }
done
[ "$(snapshot_head)"      = "$START_HEAD" ]      || REFUSALS="$REFUSALS head_changed($START_HEAD->$(snapshot_head))"
[ "$(snapshot_tracked)"   = "$START_TRACKED" ]   || REFUSALS="$REFUSALS tracked_tree_changed($START_TRACKED->$(snapshot_tracked))"
[ "$(snapshot_untracked)" = "$START_UNTRACKED" ] || REFUSALS="$REFUSALS untracked_tree_changed($START_UNTRACKED->$(snapshot_untracked))"
[ "$(snapshot_gates)"     = "$START_GATES" ]     || REFUSALS="$REFUSALS gate_inventory_changed($START_GATES->$(snapshot_gates))"
[ "$(snapshot_compiler)"  = "$START_COMPILER" ]  || REFUSALS="$REFUSALS compiler_identity_changed($START_COMPILER->$(snapshot_compiler))"
# MUTATION TARGETS ARE JUDGED CLEAN, NOT UNCHANGED — and the difference is the whole point.
# Comparing start against end asks "did a mutation appear during this run", which is silent about a
# mutation that was ALREADY THERE when the run began. That is exactly the state a SIGKILLed harness
# leaves behind, and the next run would then measure a mutated compiler and report a clean summary,
# because nothing changed while it watched. Verified by planting `declSpan := none` before a run:
# under the change-comparison it went undetected.
END_MUT="$(snapshot_muttargets)"
[ -z "$START_MUT" ] || REFUSALS="$REFUSALS mutation_target_dirty_at_start($START_MUT)"
[ -z "$END_MUT" ]   || REFUSALS="$REFUSALS mutation_target_not_restored($END_MUT)"
[ "$RUN_INTERRUPTED" = "0" ]                     || REFUSALS="$REFUSALS run_interrupted"
[ "${REACHED_END:-0}" = "1" ]                    || REFUSALS="$REFUSALS did_not_reach_end"
# The freshness override makes gates run against a binary nobody verified. Successful gate output is
# discarded, so the warning it prints does not survive, and `compiler_identity` below is a hash of
# SOURCE text — it can positively name a build identity the binary was never shown to contain. A run
# under that override cannot be a completion record.
[ "${CONCRETE_ALLOW_UNVERIFIED_BINARY:-0}" != "1" ] || REFUSALS="$REFUSALS compiler_freshness_unverified"
# `none` is what snapshot_compiler emits when it cannot read the build identity at all. It was
# captured, compared against itself, and accepted — so a run that could not identify the compiler
# it tested could still be a completion record.
[ "$START_COMPILER" != "none" ]                  || REFUSALS="$REFUSALS compiler_identity_unknown"

# STEP-LEVEL `env:` IS NOT CAPTURED, and that is now stated rather than hidden by `sort -u`.
#
# The extractor reads only a step's `run:` text, so two steps invoking the SAME script with DIFFERENT
# step-level environments collapse to one command. The workflow does exactly that: a scheduled step
# sets `MULTI_KERNEL_MUTATE: "1"` for check_multi_kernel.sh, which is a semantically different gate
# run. `DISCOVERED == EXECUTED` therefore cannot mean "the workflow's gate population ran".
#
# Those steps are NOT executed here on purpose — that one mutates tracked source, which a local
# pre-push pass must not do, the same reason check_gate_mutation_coverage.sh is excluded. So the gap
# is counted and named, and it blocks `completed=1` rather than being silently absorbed.
UNCAPTURED_ENV_STEPS="$(awk '
  /^[[:space:]]+env:[[:space:]]*$/ { inenv=1; n=0; next }
  inenv && /^[[:space:]]+run:.*scripts\// { print; inenv=0; next }
  inenv { n++; if (n > 6) inenv=0 }
' "$WORKFLOW" 2>/dev/null | grep -c . || true)"
# A PERMANENT, NAMED EXCLUSION — not a refusal. Making this a refusal meant no run on the current
# workflow could EVER produce completed=1, because the workflow legitimately contains such a step and
# this runner legitimately must not execute it (it mutates tracked source, the same reason
# check_gate_mutation_coverage.sh is excluded). A control that can never be satisfied stops being a
# control and starts being an outage. Recorded in the artifact so every consumer sees the exclusion,
# and `completed=1` therefore means "the complete LOCALLY RUNNABLE population passed".
EXCLUSIONS=""
[ "${UNCAPTURED_ENV_STEPS:-0}" -eq 0 ] \
  || EXCLUSIONS=" workflow_step_env_uncaptured($UNCAPTURED_ENV_STEPS)"

DISCOVERED="${#CMDS[@]}"
EXECUTED=$(( PASS + FAIL + DEFERRED ))
# SCOPE IS NOT INTEGRITY — the same distinction the campaign needed. A `--job`/filter run deliberately
# executes a subset, and calling that `discovered_not_executed` classified the operator's own request
# as corruption: every successful filtered run printed REFUSED and exited nonzero. A filtered run
# still cannot produce `completed=1`, because it is not the complete gate set.
SCOPE_NOTES=""
SELECTED_N="${#SELECTED[@]}"
if [ "$SELECTING" = "1" ]; then
  # A SUBSET OF ZERO IS NOT A SUBSET. Any nonempty filter was accepted as deliberate scope, so a typo
  # skipped all 208 commands and the run printed `PARTIAL PASS=0 FAIL=0` and exited ZERO — a vacuous
  # success with no positive control that the filter selected even one live gate. (Each selector is
  # now also checked on its own in resolve_selection.)
  # A host-skipped command was selected and is accounted for by name, so it is not "nothing".
  if [ $(( EXECUTED + HOST_SKIPPED_N )) -eq 0 ]; then
    REFUSALS="$REFUSALS filter_selected_nothing(${JOBSEL[*]:-}${FILTERS[*]:-})"
  fi
  # EVERY SELECTED COMMAND RAN, OR THE RUN SAYS WHY NOT. A selection is a promise about what was
  # executed; the full-run check below makes the same promise for the whole inventory.
  if [ "$STOPPED_EARLY" = "1" ]; then
    REFUSALS="$REFUSALS stopped_after_first_stage_failure($((SELECTED_N - EXECUTED - HOST_SKIPPED_N)) not run)"
  elif [ $(( EXECUTED + HOST_SKIPPED_N )) != "$SELECTED_N" ]; then
    REFUSALS="$REFUSALS selected_not_executed($SELECTED_N selected, $EXECUTED executed, $HOST_SKIPPED_N host-skipped)"
  fi
  SCOPE_NOTES=" deliberate_subset($EXECUTED of $DISCOVERED)"
  [ "${OUTSIDE:-0}" -eq 0 ] || SCOPE_NOTES="$SCOPE_NOTES outside_inventory($OUTSIDE)"
elif [ "$DISCOVERED" != $(( EXECUTED + HOST_SKIPPED_N )) ]; then
  REFUSALS="$REFUSALS discovered_not_executed($DISCOVERED discovered, $EXECUTED executed, $HOST_SKIPPED_N host-skipped)"
fi
# Host-skipped commands are named, not run: like a deliberate subset they keep the run PARTIAL and
# block completed=1 in every mode, closure included; unlike a refusal they say nothing about the tree.
HOST_NOTES=""
[ "$HOST_SKIPPED_N" -eq 0 ] || HOST_NOTES=" host_tool_missing($HOST_SKIP_TOOLS: $HOST_SKIPPED_N not run)"
# A deferral (bug 315) is a skip: the gate ran, but part of its verdict is made on CI. It keeps the run
# PARTIAL and blocks completed=1 in every mode, like a host skip, and says nothing about the tree.
DEFER_NOTES=""
[ "$DEFERRED" -eq 0 ] || DEFER_NOTES=" deferred_to_ci($DEFERRED not judged here)"
HOST_NOTES="$HOST_NOTES$DEFER_NOTES"
ALL_REFUSALS="$REFUSALS$SCOPE_NOTES$MODE_NOTES$HOST_NOTES"

COMPLETED=0
# FAILURES BLOCK COMPLETION. This tested only refusals and closure mode, so a run with red gates
# wrote `completed=1 failed=N` into the authority artifact and THEN printed REFUSED and exited
# nonzero — the artifact, the summary line and the exit status all disagreeing about the same run.
# The artifact is the thing other tooling reads, so it was the worst of the three to get wrong.
COMPLETED=0
if [ -z "$ALL_REFUSALS" ] && [ "$CLOSURE" = "1" ] && [ "$FAIL" -eq 0 ]; then COMPLETED=1; fi

CI_ARTIFACT_OK=1; ci_write_summary "$COMPLETED" "$ALL_REFUSALS" || CI_ARTIFACT_OK=0
# The deferred commands for the pre-push hook's record (bug 315), part of the run's durable output.
if [ -n "$DEFERRED_OUT" ]; then
  # Written whole or not at all: the hook reads an absent file as "the runner said nothing".
  _dtmp="$(mktemp "$DEFERRED_OUT.XXXXXX" 2>/dev/null)" \
    && printf '%s' "$DEFERRED_NL" > "$_dtmp" \
    && mv -f "$_dtmp" "$DEFERRED_OUT" \
    || { rm -f "${_dtmp:-}" 2>/dev/null; echo "error: could not write $DEFERRED_OUT" >&2; CI_ARTIFACT_OK=0; }
fi

# THE SUMMARY LINE COMES AFTER RECONCILIATION, and says REFUSED when completion was refused. It used
# to print `CI-GATES-LOCAL: PASS=.. FAIL=0` BEFORE this block, so a head/tree/compiler change — or a
# failed artifact write — produced a clean PASS line followed by a nonzero exit. That is the same
# summary-vs-exit disagreement that let check_clean_checkout report PASS=11 FAIL=0 while exiting 1,
# stranding 17 assertions for two days.
# The process succeeds when the verdicts are sound and durably recorded. Deliberate scope does not
# enter this: a filtered run that passed is a pass.
RUN_OK=1
[ "$FAIL" -eq 0 ]           || RUN_OK=0
[ -z "$REFUSALS" ]          || RUN_OK=0
[ "$CI_ARTIFACT_OK" = "1" ] || RUN_OK=0

echo
if [ "$RUN_OK" = "1" ] && [ -z "$SCOPE_NOTES$MODE_NOTES$HOST_NOTES" ]; then
  echo "CI-GATES-LOCAL: PASS=$PASS FAIL=$FAIL (JOBS=$JOBS)"
elif [ "$RUN_OK" = "1" ]; then
  echo "CI-GATES-LOCAL: PARTIAL PASS=$PASS FAIL=$FAIL$([ "$DEFERRED" -eq 0 ] || echo " DEFERRED=$DEFERRED") (JOBS=$JOBS) —$SCOPE_NOTES$MODE_NOTES$HOST_NOTES"
else
  echo "CI-GATES-LOCAL: REFUSED PASS=$PASS FAIL=$FAIL (JOBS=$JOBS)"
fi
[ -n "$FAILED" ] && echo -e "Failures:$FAILED"
if [ "$HOST_SKIPPED_N" -gt 0 ]; then
  echo "Not run — this host lacks a tool CI has ($HOST_SKIP_TOOLS); they did NOT pass here:"
  printf '  SKIP %s\n' "${HOST_SKIPPED[@]}"
fi
[ "$DEFERRED" -eq 0 ] || echo -e "Deferred to CI — not judged on this host (scripts/tests/lib/host_tools.sh); they did NOT pass here:$DEFERRED_LIST"

if [ -n "$REFUSALS" ]; then
  echo
  echo "COMPLETION REFUSED:$REFUSALS"
  echo "  This run's summary describes a tree that changed under it, or a run that was not serial."
  echo "  It is reconnaissance, not closure evidence."
elif [ "$CLOSURE" != "1" ]; then
  echo
  echo "NOT A CLOSURE RUN (mode=ordinary). Set CONCRETE_CLOSURE_RUN=1 to produce completed=1."
elif [ -n "$HOST_NOTES" ]; then
  echo
  echo "CLOSURE NOT REACHED:$HOST_NOTES — run on a host that has the tool (CI does) for completed=1."
fi
if [ "$CI_ARTIFACT_OK" = "1" ]; then
  echo "summary written to $ROOT_DIR/.ci-gates-summary (completed=$COMPLETED)"
else
  echo "error: the completion artifact could NOT be written — this run has no completion record" >&2
fi

# RELEASE THE LOCK. This runner acquired it and never released it on ANY path, including normal
# completion — so a finished pass left `.gate.lock` behind and the next gate refused with "REPOSITORY
# BUSY" until someone deleted it by hand. Release is creator-only, so this is a no-op when the lock
# was inherited from an outer run.
# NOT RELEASED AFTER AN INTERRUPTION. Gates run as backgrounded children; a TERM delivered only to
# this runner sets the flag and `wait` returns early, so releasing here would hand the repository to a
# new run while those children were still building — exactly the child-liveness risk `fresh.sh`
# refuses to take when reclaiming a lock. Fail closed: keep the lock. Inside a gate session the lock
# records the session's group, and the launcher reclaims it once the kernel reports that group empty
# (bug 244); outside one, only the operator can.
if [ "${RUN_INTERRUPTED:-0}" = "1" ]; then
  echo "" >&2
  echo "INTERRUPTED: keeping the repository lock, because gate children may still be running." >&2
  if [ "$CI_SESSION_OWNER" = "1" ]; then
    echo "  It is reclaimed automatically once no process of gate session $CONCRETE_GATE_SESSION remains." >&2
  else
    echo "  Verify nothing is working the tree, then: rm -rf $ROOT_DIR/.gate.lock" >&2
  fi
else
  # A no-op inside a gate session (see where fresh.sh is loaded): the launcher reclaims after its sweep.
  _gate_lock_release
fi
# Not while processes of this run are still alive (bug 244): the slot is reclaimed from a dead holder.
[ "$HEAVY_RUN" = "1" ] && [ "${CI_KEEP_HEAVY_SLOT:-0}" != "1" ] && heavy_slot_release
[ "${_CI_RUNNER_TEMP_OWNED:-0}" = "1" ] && rm -rf "$RUNNER_TEMP"
rm -rf "${GATE_OUT_TMP:-}"

# The artifact write is part of the verdict: a pass with no durable record is not closure evidence.
[ "$RUN_OK" = "1" ]
