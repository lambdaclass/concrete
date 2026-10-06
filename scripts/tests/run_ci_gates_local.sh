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
FILTER="${1:-}"
JOB=""
# `--closure` IS DOCUMENTED but was never parsed: it fell through as a filter substring, matched no
# gate, and produced a partial result that looked like a deliberate subset. A documented flag that
# silently means something else is worse than an unknown one.
if [ "$FILTER" = "--closure" ]; then
  CONCRETE_CLOSURE_RUN=1; FILTER="${2:-}"
fi
if [ "$FILTER" = "--job" ]; then
  JOB="${2:-}"; FILTER=""
  [ -n "$JOB" ] || { echo "error: --job needs a CI job name substring" >&2; _gate_lock_release; exit 2; }
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

# When --job is given, narrow the workflow text to that job's block first, so
# the gate list comes from the same declaration CI uses.
SRC="$WORKFLOW"
if [ -n "$JOB" ]; then
  SRC="$(mktemp)"; trap 'rm -f "$SRC"' EXIT
  awk -v job="$JOB" '
    # job headers sit at two-space indent under `jobs:`; a name: line at that
    # depth starts a new job block.
    /^  [a-zA-Z0-9_-]+:[[:space:]]*$/ { inblock=0 }
    /^    name:/ { inblock = (index($0, job) > 0) }
    inblock { print }
  ' "$WORKFLOW" > "$SRC"
  if ! [ -s "$SRC" ]; then
    echo "error: no CI job matching '$JOB'" >&2; _gate_lock_release; exit 2
  fi
fi

# Extract the gate command list once (same filter as before).
# Strip any trailing REDIRECTION from the extracted command. The character
# class stops at `&`, so a workflow line ending `... --trust-gate 2>&1 | tee
# /tmp/trust-gate.log` was captured as `... --trust-gate 2>` — a redirect with no
# target. `eval` then died of a parse error and the gate was counted FAIL, every
# run, on a clean tree. So the trust gate was never actually EXECUTED locally
# while appearing in the failure list: the loudest possible way to be silent.
# Output is discarded by the runner anyway, so redirections are noise here.
mapfile -t CMDS < <(grep -oE '([A-Z_][A-Z0-9_]*=[^ ;|&]+[[:space:]]+)*((bash|python3|sh)[[:space:]]+)?(\./)?scripts/[^ ;|&]*\.(sh|py)[^;|&]*' "$SRC" \
         | grep -v 'check_gate_mutation_coverage\.sh' \
         | sed 's/[[:space:]][0-9]*[<>].*$//' \
         | sed 's/[[:space:]]*$//' \
         | sort -u)

# Every extracted command must be SYNTACTICALLY VALID before anything is run. A
# truncated command otherwise reports as a failing gate, which reads as "the tree
# is red" rather than "the extractor is broken" — and those demand opposite
# responses. Checked up front so the whole list is trustworthy before a ~20min
# pass, not discovered gate by gate.
# VACUITY FLOOR ON THE INVENTORY ITSELF. `mapfile` does not fail when its process substitution
# produces nothing, so a missing workflow file or extractor drift yields CMDS=() — after which every
# loop below does nothing, DISCOVERED == EXECUTED == 0 holds, and closure mode writes `completed=1`
# with `PASS=0 FAIL=0`. An authoritative record that the entire gate set passed, having run none of
# it. The floor is far below the current 208 so it catches collapse, not growth.
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
EXPECTED_GATE_COMMANDS=237
if [ -n "$JOB" ]; then
  [ "${#CMDS[@]}" -ge 1 ] || { echo "error: --job '$JOB' yielded no gate commands." >&2
    ci_write_summary 0 " job_selected_nothing"; _gate_lock_release; exit 2; }
elif [ "${#CMDS[@]}" != "$EXPECTED_GATE_COMMANDS" ]; then
  echo "error: gate extraction produced ${#CMDS[@]} commands from $WORKFLOW, pinned at $EXPECTED_GATE_COMMANDS." >&2
  echo "       If the workflow legitimately changed, update EXPECTED_GATE_COMMANDS in the SAME commit." >&2
  echo "       That is extractor failure, not an empty gate set. Refusing to report a verdict." >&2
  ci_write_summary 0 " gate_inventory_vacuous(${#CMDS[@]})"
  _gate_lock_release
  exit 2
fi

bad=0
for cmd in "${CMDS[@]}"; do
  if ! bash -n -c "$cmd" 2>/dev/null; then
    echo "error: extracted gate command is not valid shell: $cmd" >&2
    bad=1
  fi
done
if [ "$bad" -ne 0 ]; then
  echo "error: gate extraction produced unrunnable commands — fix the extractor," >&2
  echo "       not the gates. A truncated command cannot be told from a red gate." >&2
  ci_write_summary 0 " gate_extraction_invalid"
  _gate_lock_release
  exit 2
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

if [ "$JOBS" -le 1 ]; then
  for cmd in "${CMDS[@]}"; do
    cmd="${cmd/bash .\//bash }"
    if [ -n "$FILTER" ] && [[ "$cmd" != *"$FILTER"* ]]; then continue; fi
    _out="$FAILLOG_DIR/$(faillog_name "$cmd").log"
    if eval "$cmd" >"$_out" 2>&1; then PASS=$((PASS+1)); rm -f "$_out"
    else FAIL=$((FAIL+1)); FAILED="$FAILED\n  FAIL $cmd"; echo "  FAIL $cmd (output: ${_out#$ROOT_DIR/})"; fi
  done
else
  # Parallel pool: each gate writes "OK"/"FAIL <cmd>" to its own result file.
  RES=$(mktemp -d); trap 'rm -rf "$RES"' EXIT
  idx=0
  for cmd in "${CMDS[@]}"; do
    cmd="${cmd/bash .\//bash }"
    if [ -n "$FILTER" ] && [[ "$cmd" != *"$FILTER"* ]]; then continue; fi
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
    if eval "$cmd" >/dev/null 2>&1; then
      PASS=$((PASS+1)); RECHECKED="$RECHECKED\n  (contention flake, passed on sequential re-run) $cmd"
    else
      FAIL=$((FAIL+1)); FAILED="$FAILED\n  FAIL $cmd"; echo "  FAIL $cmd"
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
if [ -n "$FILTER" ] || [ -n "$JOB" ]; then
  # A SUBSET OF ZERO IS NOT A SUBSET. Any nonempty filter was accepted as deliberate scope, so a typo
  # skipped all 208 commands and the run printed `PARTIAL PASS=0 FAIL=0` and exited ZERO — a vacuous
  # success with no positive control that the filter selected even one live gate.
  if [ "$EXECUTED" -eq 0 ]; then
    REFUSALS="$REFUSALS filter_selected_nothing($FILTER$JOB)"
  fi
  SCOPE_NOTES=" deliberate_subset($EXECUTED of $DISCOVERED)"
elif [ "$DISCOVERED" != "$EXECUTED" ]; then
  REFUSALS="$REFUSALS discovered_not_executed($DISCOVERED discovered, $EXECUTED executed)"
fi
ALL_REFUSALS="$REFUSALS$SCOPE_NOTES$MODE_NOTES"

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
if [ "$RUN_OK" = "1" ] && [ -z "$SCOPE_NOTES$MODE_NOTES" ]; then
  echo "CI-GATES-LOCAL: PASS=$PASS FAIL=$FAIL (JOBS=$JOBS)"
elif [ "$RUN_OK" = "1" ]; then
  echo "CI-GATES-LOCAL: PARTIAL PASS=$PASS FAIL=$FAIL (JOBS=$JOBS) —$SCOPE_NOTES$MODE_NOTES"
else
  echo "CI-GATES-LOCAL: REFUSED PASS=$PASS FAIL=$FAIL (JOBS=$JOBS)"
fi
[ -n "$FAILED" ] && echo -e "Failures:$FAILED"

if [ -n "$REFUSALS" ]; then
  echo
  echo "COMPLETION REFUSED:$REFUSALS"
  echo "  This run's summary describes a tree that changed under it, or a run that was not serial."
  echo "  It is reconnaissance, not closure evidence."
elif [ "$CLOSURE" != "1" ]; then
  echo
  echo "NOT A CLOSURE RUN (mode=ordinary). Set CONCRETE_CLOSURE_RUN=1 to produce completed=1."
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
