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
#        scripts/tests/run_ci_gates_local.sh [--plan] [--first CMD]... [--job J]... [--filter S]...
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
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
WORKFLOW=".github/workflows/lean_action_ci.yml"

# ARGUMENTS ARE PARSED BEFORE THE LOCK, BUT JUDGED AFTER IT. Parsing is pure; an error found here is
# only reported once the lock is held (so the invalidation record below is written by the holder),
# except in --plan mode, which never takes the lock.
ARG_ERR=""
PLAN=0
JOBSEL=(); FILTERS=(); FIRST_CMDS=()
while [ $# -gt 0 ]; do
  case "$1" in
    # `--closure` IS DOCUMENTED but was never parsed: it fell through as a filter substring, matched
    # no gate, and produced a partial result that looked like a deliberate subset.
    --closure) CONCRETE_CLOSURE_RUN=1; shift ;;
    --plan)    PLAN=1; shift ;;
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
[ "${#JOBSEL[@]}${#FILTERS[@]}${#FIRST_CMDS[@]}" = "000" ] || SELECTING=1

# THE ONE EXTRACTOR. Strip any trailing REDIRECTION from the extracted command. The character
# class stops at `&`, so a workflow line ending `... --trust-gate 2>&1 | tee
# /tmp/trust-gate.log` was captured as `... --trust-gate 2>` — a redirect with no
# target. `eval` then died of a parse error and the gate was counted FAIL, every
# run, on a clean tree. So the trust gate was never actually EXECUTED locally
# while appearing in the failure list: the loudest possible way to be silent.
# Output is discarded by the runner anyway, so redirections are noise here.
_extract_cmds() { # workflow text file -> gate commands, raw spelling, sorted unique
  grep -oE '([A-Z_][A-Z0-9_]*=[^ ;|&]+[[:space:]]+)*((bash|python3|sh)[[:space:]]+)?(\./)?scripts/[^ ;|&]*\.(sh|py)[^;|&]*' "$1" \
    | grep -v -E 'check_gate_mutation_coverage\.sh|aggregate_mutation_shards\.py|checker_acceptance_evidence\.sh' \
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
# 213 -> 224: four gates added by R-0483/R-0484 and the diff-to-gate mapping
# (check_view_lifetime, check_effect_opacity, check_gates_for_diff, and the
# attestation reference-order gate). Updated here because the pin is what makes
# "the whole workflow was read" checkable; leaving it stale turns a growth signal
# into a permanent refusal.
# 224 -> 229: the bug-071 capability gates — check_cross_package_caps,
# check_cap_sibling_module, check_trusted_method, check_cap_inference,
# check_absence_not_fact. The pin went stale for four commits because this runner is
# not itself a CI gate, so nothing noticed; GitHub ran the five new gates the whole
# time. Worth stating plainly: a pin that only a local tool enforces detects drift
# whenever someone runs it, not when the drift happens.
# 229 -> 230: check_spawn_exit.sh (bug 072). The pin went stale again in the commit
# that added the gate, and the pre-push hook — which does run this — refused the push.
# 230 -> 231: check_cap_struct_cross_package.sh (R-0484), pinned in the same commit.
# 231 -> 232: check_foreign_assumptions_report.sh (R-0484 R10), pinned in the same commit.
# 232 -> 233: check_assumption_summary.sh (R-0484 R10 assumption summaries), pinned in the same commit.
# 233 -> 234: check_proof_admission.sh (R-0484 R10 proof admission over the summary), pinned in the same commit.
# 234 -> 235: check_trusted_boundaries.sh (R-0484 R10 named trusted boundaries), pinned in the same commit.
# 235 -> 236: check_descriptor_coverage.sh (R-0484 R10/R5 descriptor coverage), pinned in the same commit.
# 236 -> 237: check_capability_explanations.sh (R-0484 R10 cross-package explanations), pinned in the same commit.
# 237 -> 238: check_bug077_contained.sh (bug 077 kept separately owned under R-0484 closure), pinned in the same commit.
# 238 -> 239: check_divergence_detection.sh (bug 078; the divergence-detection family's gate), pinned in the same commit.
# 239 -> 240: check_mutation_shard_aggregate.sh (sharded mutation campaign), pinned in the same commit.
#   scripts/tools/checker_acceptance_evidence.sh (R-0208) is excluded too: it PRODUCES evidence for a
#   CI-measured checker build (an artifact) and needs $RUNNER_TEMP. Its one judgement, the claim
#   inventory replay, runs locally as check_claim_inventory.sh.
#   The aggregator itself (aggregate_mutation_shards.py) is excluded above with the campaign driver:
#   it reads artifacts of a sharded dispatch and has nothing to judge in a local pass.
# 240 -> 241: check_prepush_record.sh (the pre-push validation-record bindings), pinned in the same commit.
# 241 -> 242: check_checker_identity.sh (R-0208 slice 1a checker identity), pinned at integration.
# 242 -> 243: check_kernel_recheck.sh (R-0208 slice 1b fresh kernel re-check), pinned in the same commit.
# 243 -> 244: check_checker_policy.sh (R-0208 checker advisory policy), pinned in the same commit.
# 244 -> 245: check_lean14576_reproducer.sh (R-0208 fire drill reproducer), pinned in the same commit.
# 245 -> 246: check_identity_rows.sh (R-0482 frozen identity rows), pinned in the same commit.
# 248 -> 249: check_subject_binding.sh (bug 079 subject binding), pinned in the same commit.
# 249 -> 250: check_model_diff.sh (bug 080 model vs runtime), pinned in the same commit.
# 250 (unchanged): check_roadmap_coverage.sh (2026-10-19 roadmap reconciliation) runs inside
#   check_docs_drift.sh, exactly as check_roadmap_linear.sh does, so it adds no workflow command.
# 250 -> 251: check_claim_inventory.sh (R-0208 claim inventory, the local twin of the acceptance step), pinned in the same commit.
# 251 -> 252: check_qualification_record.sh (stage (c) is one verdict over every required job; the
#   mutation aggregate is one component), pinned in the same commit. The record tool itself,
#   scripts/ci/qualification_record.py, reads CI run records and is not a workflow command.
# 252 -> 253: check_example_outputs.sh (every runnable example's stdout + exit code, native and
#   interpreter), which replaces the inline "Run compiled examples (smoke test)" step; that step ran no
#   scripts/ command, so removing it changes nothing here. Pinned in the same commit.
# 253 -> 254: check_stack_budget.sh (bug 065 stack budget fails closed), pinned at integration.
EXPECTED_GATE_COMMANDS=254

# RESOLVE THE SELECTION — sets CMDS (raw inventory), SELECTED (normalized, unique, execution order),
# FIRST_N (how many leading SELECTED entries are the sequential first stage) and OUTSIDE (selected
# commands that are not workflow commands). On failure sets RESOLVE_ERR (a refusal token) and
# RESOLVE_MSG; nothing is executed or written here.
resolve_selection() {
  RESOLVE_ERR=""; RESOLVE_MSG=""; SELECTED=(); FIRST_N=0; OUTSIDE=0
  mapfile -t CMDS < <(_extract_cmds "$WORKFLOW" 2>/dev/null)
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
if ! require_fresh_binary ".lake/build/bin/concrete"; then
  ci_write_summary 0 " compiler_not_fresh"
  echo "error: refusing to run the gate set against an unverified compiler." >&2
  _gate_lock_release
  exit 2
fi
# The nightly fuzz steps derive SEED from the date in CI; the extracted fuzz
# commands reference "$SEED", which would trip `set -u` here. Mirror CI.
SEED="${SEED:-$(date -u +%Y%m%d)}"

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
trap 'RUN_INTERRUPTED=1' INT TERM HUP

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

# ONE SEQUENTIAL VERDICT, used by the first stage, JOBS=1 and the parallel re-run alike.
run_seq() { # cmd -> 0 pass / 1 fail; output kept only on failure
  local _out="$FAILLOG_DIR/$(faillog_name "$1").log"
  if eval "$1" >"$_out" 2>&1; then PASS=$((PASS+1)); rm -f "$_out"; return 0; fi
  FAIL=$((FAIL+1)); FAILED="$FAILED\n  FAIL $1"; echo "  FAIL $1 (output: ${_out#$ROOT_DIR/})"
  return 1
}

# THE FIRST STAGE runs sequentially, in order, before anything else launches, and a failure there
# stops the run: the hook's fast surface checklist used to run first and fail the push in seconds,
# and folding it into one pool must not turn that into a failure reported after minutes of other gates.
STOPPED_EARLY=0
for (( _i=0; _i<RUN_FIRST_N; _i++ )); do
  run_seq "${RUN_LIST[$_i]}" || { STOPPED_EARLY=1; break; }
done
REST=("${RUN_LIST[@]:$RUN_FIRST_N}")
[ "$STOPPED_EARLY" = "1" ] && REST=()

if [ "$JOBS" -le 1 ]; then
  for cmd in "${REST[@]}"; do
    run_seq "$cmd" || true
  done
else
  # Parallel pool: each gate writes "OK"/"FAIL <cmd>" to its own result file.
  RES=$(mktemp -d); trap 'rm -rf "$RES"' EXIT
  idx=0
  for cmd in "${REST[@]}"; do
    # throttle to JOBS concurrent
    while [ "$(jobs -rp | wc -l)" -ge "$JOBS" ]; do wait -n 2>/dev/null || true; done
    idx=$((idx+1))
    ( if eval "$cmd" >/dev/null 2>&1; then echo "OK" > "$RES/$idx"
      else echo "FAIL $cmd" > "$RES/$idx"; fi ) &
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
    if run_seq "$cmd"; then
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
EXECUTED=$(( PASS + FAIL ))
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
ALL_REFUSALS="$REFUSALS$SCOPE_NOTES$MODE_NOTES$HOST_NOTES"

COMPLETED=0
# FAILURES BLOCK COMPLETION. This tested only refusals and closure mode, so a run with red gates
# wrote `completed=1 failed=N` into the authority artifact and THEN printed REFUSED and exited
# nonzero — the artifact, the summary line and the exit status all disagreeing about the same run.
# The artifact is the thing other tooling reads, so it was the worst of the three to get wrong.
COMPLETED=0
if [ -z "$ALL_REFUSALS" ] && [ "$CLOSURE" = "1" ] && [ "$FAIL" -eq 0 ]; then COMPLETED=1; fi

CI_ARTIFACT_OK=1; ci_write_summary "$COMPLETED" "$ALL_REFUSALS" || CI_ARTIFACT_OK=0

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
  echo "CI-GATES-LOCAL: PARTIAL PASS=$PASS FAIL=$FAIL (JOBS=$JOBS) —$SCOPE_NOTES$MODE_NOTES$HOST_NOTES"
else
  echo "CI-GATES-LOCAL: REFUSED PASS=$PASS FAIL=$FAIL (JOBS=$JOBS)"
fi
[ -n "$FAILED" ] && echo -e "Failures:$FAILED"
if [ "$HOST_SKIPPED_N" -gt 0 ]; then
  echo "Not run — this host lacks a tool CI has ($HOST_SKIP_TOOLS); they did NOT pass here:"
  printf '  SKIP %s\n' "${HOST_SKIPPED[@]}"
fi

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
# refuses to take when reclaiming a lock. Fail closed: keep the lock and tell the operator how to
# recover once they have confirmed nothing is running.
if [ "${RUN_INTERRUPTED:-0}" = "1" ]; then
  echo "" >&2
  echo "INTERRUPTED: keeping the repository lock, because gate children may still be running." >&2
  echo "  Verify nothing is working the tree, then: rm -rf $ROOT_DIR/.gate.lock" >&2
else
  _gate_lock_release
fi

# The artifact write is part of the verdict: a pass with no durable record is not closure evidence.
[ "$RUN_OK" = "1" ]
