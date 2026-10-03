#!/usr/bin/env bash
# Phase 6C #5: gate mutation-testing — prove the pipeline gates are load-bearing.
#
# For each rule FAMILY, apply a one-line source mutation that disables the rule
# and prove the mutation cannot survive. A mutation that survives is a SURVIVOR = a
# decorative gate = failure.
#
# TWO KINDS OF EVIDENCE, stated precisely because the code enforces both and this header used to
# promise only the first:
#
#   (1) the family's NAMED GATE goes red — that gate is shown load-bearing for that rule; or
#   (2) the mutation fails to BUILD — the defect is unrepresentable, which is a stronger result than a
#       gate catching it, but leaves that family's gate unproven, so it must be DECLARED per family.
#
# `gates_proven=<n>/<N>` reports (1) on every summary line and in the artifact, and the two counts must
# add up to the whole corpus or the run refuses (`families_unaccounted`). So a completed run does NOT
# claim every gate was exercised — it claims every family produced one of these two kinds of evidence
# and says how many of each. `gates_proven=78/81` with three declared build kills is a complete result,
# not a shortfall; and a PASS line never means "all 81 gates went red", which is why the ratio is
# always printed beside it.
#
# HEAVY / NIGHTLY: the behavioral families need a `lake build` per mutation (~1-3
# min each), so this is not a per-commit gate. Grep-only families (constructor
# coverage, source-maps) need no rebuild. Run one family with FAMILY=<n>, or all
# (default). Restore is via `git checkout --` (assumes a clean tree for the
# mutated files); a final clean rebuild is done at the end.
#
# Most families' gates are OUTSIDE run_tests.sh --fast, so we invoke each gate directly.
#
# COVERAGE, stated because this harness is the thing that stops gates being decorative and
# its own coverage was never written down (2026-08-04 sweep):
#
#   180 gate scripts in scripts/tests/
#    55 guard a SOUNDNESS claim — breaking the rule would let the compiler assert something
#       false about a program (a missing trap, a false `proved`, a laundered axiom)
#    18 of those 55 are the GATE FIELD of a family here
#    37 of those 55 have NONE
#
# Count coverage from the GATE FIELD of `add` lines, never by grepping this file for gate
# names. Two gates read as covered for a while because their names appear only in prose:
# `check_multi_kernel.sh` (mentioned in a note about dirty-tree refusal) and
# `check_totality_judgment.sh` (mentioned in the survivor note below, which says explicitly
# that it does NOT cover its rule). The name-grep figure was 20; the real figure is 18, and
# the inherited "11 of 180" was inflated the same way.
#
# `--coverage` prints the real numbers so nobody has to grep. Third instance today of a
# metric measuring a proxy instead of the thing, and this file exists to prevent that class.
#
# Those figures are RECOMPUTED, not derived by arithmetic. A first version of this header
# said 17/38, reached by adding the four families of that pass to a previous count — wrong,
# because six of the covered gates (source-maps, copy-judgment, corecheck-boundary,
# diagnostics-quality, mono-name-collision, constructor-coverage) are not soundness gates at
# all, so "covered" and "covered AND soundness" are different sets. Same defect class this
# file exists to catch: a number restated instead of measured.
#
# The 42 are not known to be decorative. They are UNMEASURED, which is a different claim
# from safe, and the distinction is the whole reason this file exists: for a week, every
# defect found landed in the unmeasured set while the suite was green.
#
# Selection rule for extending this list: a gate qualifies if breaking the rule it guards
# would let the compiler assert something FALSE. Gates over formatting, naming, docs hygiene
# or CI plumbing are out of scope — they matter, but their failure mode is noise rather than
# a wrong claim.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
# HASHED BEFORE ANYTHING ELSE RUNS. The snapshot below makes the driver immutable from the point it is
# taken — but the lock acquisition and artifact invalidation execute from the LIVE file first, and bash
# reads a script incrementally. A save during that preamble means one version supplied those controls
# and another supplied the campaign body, while `executed_driver_sha` recorded only the latter. This
# cannot be prevented (bash has already read those bytes); it can be DETECTED, by hashing the file
# before the preamble and comparing at snapshot time.
_PREAMBLE_SHA="$( { sha256sum "${BASH_SOURCE[0]}" 2>/dev/null || shasum -a 256 "${BASH_SOURCE[0]}"; } | cut -c1-32 )"
LAKE="${LAKE:-lake}"
RUN_T0=$(date +%s)

# PER-PHASE TIMING, so the next optimisation is chosen from measurement rather than from my
# assumption that builds dominate. The 898d9a7b campaign took ~20 hours for 81 families and nothing
# in it recorded WHERE that time went.
#
# These two wrappers are also the single invocation site for "build the workspace" and "run a gate in
# the workspace". Those were spelled out at five and five places respectively, which is the
# duplicated-producer shape this harness keeps paying for — a timing wrapper is a poor reason to add a
# sixth copy, and a good reason to remove the other nine.
declare -A PHASE_SECS
declare -A FAMILY_SECS
_sw_now(){ date +%s; }
_timed_build(){ # logfile
  local _t0 rc=0; _t0=$(_sw_now)
  ( cd "$WORK" && "$LAKE" build ) >"$1" 2>&1 || rc=$?
  local d=$(( $(_sw_now) - _t0 ))
  PHASE_SECS[build]=$(( ${PHASE_SECS[build]:-0} + d )); FAMILY_SECS[build]=$(( ${FAMILY_SECS[build]:-0} + d ))
  return $rc
}
_timed_gate(){ # gate-path logfile
  local _t0 rc=0; _t0=$(_sw_now)
  ( cd "$WORK" && bash "$1" ) >"$2" 2>&1 || rc=$?
  local d=$(( $(_sw_now) - _t0 ))
  PHASE_SECS[gate]=$(( ${PHASE_SECS[gate]:-0} + d )); FAMILY_SECS[gate]=$(( ${FAMILY_SECS[gate]:-0} + d ))
  return $rc
}

# IS THIS A PARTIAL SELECTION? Decided ONCE, here, before anything derives from it.
#
# This was previously inferred twice from different evidence: the child set CAMPAIGN_MODE from ONLY,
# and the supervisor chose its publication target from ${FAMILY}. Adding FAMILY_ID gave the two
# inferences different inputs — ONLY was resolved late, so the child had already recorded
# mode=campaign, and FAMILY was unset, so the supervisor published a ONE-FAMILY result over the
# authoritative FULL-campaign artifact. Measured, on this repository: a record reading
# `mode=campaign discovered=85 selected=1` replaced the full record.
#
# A sample must never be able to destroy evidence about the whole corpus, so the question is asked
# once and both roles read the same answer.
CAMPAIGN_PARTIAL=0
{ [ -z "${FAMILY:-}" ] && [ -z "${FAMILY_ID:-}" ]; } || CAMPAIGN_PARTIAL=1
export CONCRETE_MUT_PARTIAL="$CAMPAIGN_PARTIAL"
ONLY="${FAMILY:-}"
# A single-family probe and a full campaign make different claims. Only a campaign can ever qualify;
# a probe must be able to SUCCEED on a sound selected result without ever storing or printing campaign
# qualification. Conflating the two would break the registered FAMILY=n consumers, including
# check_phase6c_observability.sh, which chains two of them with `&&`.
CAMPAIGN_MODE=campaign; [ "$CAMPAIGN_PARTIAL" = "0" ] || CAMPAIGN_MODE=single

# ---------------------------------------------------------------------------
# IMMUTABLE DRIVER SNAPSHOT.
#
# Bash reads a script INCREMENTALLY, by byte offset. Editing this file while a campaign runs
# therefore changes the campaign — it does not merely change the next one. Measured 2026-08-20: a
# 78-family run completed all 78 verdicts and then died with `unexpected EOF while looking for
# matching '` because the file had grown underneath the interpreter, and it produced no summary at
# all. That failure was LUCKY. A syntactically valid edit would have silently altered which
# mutations later families applied, with nothing to expose it. "The arrays are already loaded" is not
# protection, and I asserted that it was.
#
# So the driver re-execs itself from a copy under /tmp and runs from there. Repo edits then cannot
# reach the running interpreter at all — the failure mode is removed rather than detected. Same
# reasoning as mutating a disposable copy of the tree instead of the tree.
# ONE VALIDATED REMOVER for the snapshot directory, used by every cleanup path. This is an `rm -rf`
# of a value that arrives from the ENVIRONMENT: a caller, or a stale export, naming an unrelated
# directory would have it deleted. Three separate paths performed that removal and only one of them
# checked anything. A bare prefix test is not enough either — `/tmp/concrete-mut.x/../../home` has the
# prefix — so the path must contain no `..` component and must still look like our own.
_rm_snapdir() {
  local d="${CONCRETE_MUT_SNAPDIR:-}" owner
  [ -n "$d" ] || return 0
  case "$d" in
    *..*) echo "warning: refusing to remove CONCRETE_MUT_SNAPDIR='$d' — contains '..'" >&2; return 1 ;;
  esac
  # The namespace root is normalized the same way the snapshot directory is when created (`cd && pwd`):
  # macOS's TMPDIR ends in `/`, so the raw prefix `.../T//concrete-mut.` never matched the normalized
  # directory and every snapshot was refused cleanup and leaked.
  local tmp_root
  tmp_root="$(cd "${TMPDIR:-/tmp}" 2>/dev/null && pwd)" || tmp_root="${TMPDIR:-/tmp}"
  case "$d" in
    "$tmp_root/concrete-mut."*) ;;
    *) echo "warning: refusing to remove CONCRETE_MUT_SNAPDIR='$d' — not a campaign-created path" >&2; return 1 ;;
  esac
  # OURS, not merely in the namespace. A prefix test alone let a STALE exported CONCRETE_MUT_SNAPDIR —
  # or one supplied by a caller alongside CONCRETE_MUT_SNAPSHOT — name a directory belonging to a
  # DIFFERENT, possibly live, campaign in the same namespace, and this would delete it. The marker file
  # must be present and must record THIS process as owner.
  [ -f "$d/.concrete-mutation-workspace" ] || {
    echo "warning: refusing to remove '$d' — no campaign marker file" >&2; return 1; }
  owner="$(sed -n 's/^owner_pid=//p' "$d/.concrete-mutation-workspace" 2>/dev/null | head -1)"
  [ "$owner" = "$$" ] || {
    echo "warning: refusing to remove '$d' — owned by pid ${owner:-?}, not $$" >&2; return 1; }
  rm -rf "$d" 2>/dev/null || true
  return 0
}

# ---------------------------------------------------------------------------
# THE LOCK AND THE INVALIDATION COME BEFORE ANYTHING THAT CAN FAIL — including the driver snapshot.
#
# I previously claimed this ordering was in place and it was NOT: the snapshot was created first, so a
# full disk or an unwritable TMPDIR aborted the run while an older `completed=1` stayed readable. The
# claim was written into the checkpoint without the corresponding code, which is exactly the kind of
# unverified assertion this whole exercise exists to catch.
#
# Acquiring here is safe across the `exec` below: the token is exported and `exec` keeps the SAME pid,
# so the re-exec'd phase inherits a token whose owner is itself and passes validation trivially.
# CAMPAIGN_HELD_LOCK is exported for the same reason — the post-exec phase must know it owns the lock.
# READ-ONLY MODES TAKE NO LOCK AND WRITE NO ARTIFACT. `--coverage` is a documented reporting command
# that greps this file and exits; treating it as a campaign start meant it acquired the repository
# lock, overwrote `.mutation-campaign-summary` with `completed=0` — destroying the authoritative
# record — and then exited through a trap that only removes the snapshot, never releasing the lock. A
# reporting command must not be able to do any of that.
# ONE LIST OF QUERY MODES, CONSULTED BY BOTH DECISIONS. Whether a mode takes the repository lock and
# whether it is supervised are the same question — "does this invocation run a campaign?" — and they
# were answered by two separately maintained enumerations of `--coverage`. Adding a third reporting
# mode to one and not the other yields a query that forks a supervisor, or one that takes the lock.
_mut_query_mode() { case "${1:-}" in --coverage|--spec) return 0 ;; *) return 1 ;; esac; }
_MUT_READ_ONLY_MODE=0
[ "${ANCHORS_ONLY:-0}" = "1" ] && _MUT_READ_ONLY_MODE=1
_mut_query_mode "${1:-}" && _MUT_READ_ONLY_MODE=1

# `CONCRETE_MUT_SNAPSHOT` MEANS "I AM THE SNAPSHOT", AND THAT IS CHECKABLE.
#
# Every step below — lock acquisition, invalidating the previous record, taking the snapshot — is
# skipped when this variable is set, because a legitimate snapshot has already done all of it. But
# the variable was TRUSTED: supplying it together with CONCRETE_MUT_ROOT skipped the lock and the
# invalidation while still reaching the supervisor and publishing authoritatively.
#
# A real snapshot was re-exec'd AS the file the variable names, so `$0` and the variable agree by
# construction. An inherited or forged value does not survive that comparison — and neither does
# a stray export left in a shell or in CI.
# AND A REAL SNAPSHOT IS NEVER THE REPOSITORY'S OWN COPY OF THIS FILE.
#
# Comparing `$0` with the variable alone compares two caller-controlled strings, and pointing the
# variable AT the repository driver satisfies it — MEASURED: invoked by absolute path, the claim
# passed and the run proceeded with no lock and no invalidation. A legitimate snapshot is a copy
# taken into a freshly created temporary directory and re-exec'd from there, so it is outside the
# repository by construction and inside the snapdir the driver made. Both are checked. This is not
# a security boundary — anyone who controls the environment controls everything — it is a guard
# against an inherited or mistaken value, which is what actually happens.
if [ -n "${CONCRETE_MUT_SNAPSHOT:-}" ]; then
  # `$0` IS RESOLVED BEFORE IT IS COMPARED WITH ABSOLUTE PATHS. Invoked as
  # `scripts/tests/check_gate_mutation_coverage.sh` it is RELATIVE, so the `"$ROOT_DIR"/*` test
  # never matched and the ordinary invocation walked straight past the check written to catch it.
  # A comparison between paths of different kinds is not a comparison.
  _self_abs="$(cd "$(dirname "$0")" 2>/dev/null && pwd)/$(basename "$0")"
  # A CANDIDATE ROOT COUNTS ONLY IF IT IS A REPOSITORY. `ROOT_DIR` here is still derived from this
  # file's location, two levels up — for a real snapshot that is the temp directory's grandparent,
  # not a repository. On Linux (`/tmp/concrete-mut.X/driver.sh`) it is `/`, whose pattern `//*`
  # matches nothing, so the check passed by accident; on macOS (`/var/folders/../T/concrete-mut.X/`)
  # it is a real ancestor of the snapshot and EVERY legitimate snapshot was refused as "inside the
  # repository". The launcher-named root is checked too, so the guard does not depend on which
  # candidate an inherited value happens to supply.
  for _cand_root in "$ROOT_DIR" "${CONCRETE_MUT_ROOT:-}"; do
    [ -n "$_cand_root" ] && [ -f "$_cand_root/scripts/tests/lib/fresh.sh" ] || continue
    case "$_self_abs" in
      "$_cand_root"/*)
        echo "FATAL: CONCRETE_MUT_SNAPSHOT is set but this file is inside the repository." >&2
        echo "       A snapshot is a copy taken OUTSIDE the tree; the repository's own driver is" >&2
        echo "       never one. Honouring it would skip the lock and the invalidation." >&2
        exit 2 ;;
    esac
  done
  case "$_self_abs" in
    "${CONCRETE_MUT_SNAPDIR:-/nonexistent}"/*) ;;
    *) echo "FATAL: CONCRETE_MUT_SNAPSHOT is set but this file is not inside the snapshot" >&2
       echo "       directory the driver created (${CONCRETE_MUT_SNAPDIR:-<unset>}). Refusing." >&2
       exit 2 ;;
  esac
fi
if [ -n "${CONCRETE_MUT_SNAPSHOT:-}" ] && [ "$0" != "${CONCRETE_MUT_SNAPSHOT}" ]; then
  echo "FATAL: CONCRETE_MUT_SNAPSHOT is set but this process is not that snapshot." >&2
  echo "       running: $0" >&2
  echo "       claimed: ${CONCRETE_MUT_SNAPSHOT}" >&2
  echo "       Honouring it would skip the repository lock and the invalidation of the previous" >&2
  echo "       record, and still publish authoritatively. Refusing." >&2
  exit 2
fi
# A GIT WORKTREE IS REFUSED HERE, BEFORE THE LOCK, THE SWEEP AND THE COPY — not at the end of them.
# The isolated workspace is a `cp -a` of this checkout, and per-family restore runs git inside it.
# In a worktree `.git` is a FILE naming the shared git dir, so the copy would share HEAD and index
# with the real worktree: a restore inside the copy could rewrite the checkout it was meant to
# protect. The same refusal used to fire only after the workspace was built, buried under lock and
# sweep output, and a golden check failed 4/7 twice before the cause was visible. Full worktree
# support (separating checkout-local paths from shared git state) is a separate change.
# NARROWED to the worktree case (`.git` is a FILE). The first version refused any checkout whose
# `.git` was not a directory, which also refused check_campaign_supervisor.sh's sandbox — a copy
# with no `.git` at all — before the injected failure that control exists to observe. A missing
# `.git` is still refused by the backstop after the copy.
if [ "$_MUT_READ_ONLY_MODE" = "0" ] && [ -z "${CONCRETE_MUT_SNAPSHOT:-}" ] && [ -f "$ROOT_DIR/.git" ]; then
  _wt_gitdir="$(git -C "$ROOT_DIR" rev-parse --git-dir 2>/dev/null || echo '?')"
  _wt_common="$(git -C "$ROOT_DIR" rev-parse --git-common-dir 2>/dev/null || echo '?')"
  echo "error: the mutation campaign cannot run in a git worktree (.git here is a file naming a shared git dir)." >&2
  echo "       checkout: $ROOT_DIR" >&2
  echo "       git dir:  $_wt_gitdir   shared git dir: $_wt_common" >&2
  echo "       Run it in a clone:  git clone --local \"$ROOT_DIR\" <dir> && cd <dir>" >&2
  echo "       This run wrote NOTHING. .mutation-campaign-summary may still hold an EARLIER" >&2
  echo "       run's result. Do not read it as describing this attempt." >&2
  exit 2
fi
if [ "$_MUT_READ_ONLY_MODE" = "0" ] && [ -z "${CONCRETE_MUT_SNAPSHOT:-}" ]; then
  # shellcheck source=scripts/tests/lib/fresh.sh
  # THE ONE GAP THAT CANNOT BE CLOSED BY ORDERING, so it is stated instead of papered over.
  #
  # Two rules are in tension. Nothing may write the artifact without holding the lock (otherwise a
  # second invocation overwrites a LIVE run's record and is then refused by the lock). And every
  # failed attempt should discredit the previous record. When the failure IS "the locking library will
  # not load", both cannot hold: there is no way to take the lock, so there is no way to write
  # legitimately. Refusing without writing is the safe half of that trade, and the message says so
  # explicitly — a stale record the operator has been TOLD about is a different thing from one they
  # have not.
  _fresh_fail() {
    echo "FATAL: $1" >&2
    echo "       The repository lock could not be taken, so this run wrote NOTHING." >&2
    echo "       .mutation-campaign-summary may still hold an EARLIER run's result — including" >&2
    echo "       completed=1. Do not read it as describing this attempt." >&2
    exit 2
  }
  # THIS DRIVER MANAGES THE LOCK ITSELF, and says so before the library can arm anything.
  #
  # The supervisor decides release by `supervisor_must_hold_lock`: a campaign group that is not
  # proven empty must KEEP the lock, so the next run cannot start while work may still be writing
  # the tree. The library's automatic release — correct for every ordinary gate, which otherwise
  # strands the lock — would compose itself into this driver's EXIT trap and reverse that decision.
  _GATE_LOCK_SELF_MANAGED=1
  . "$ROOT_DIR/scripts/tests/lib/fresh.sh" 2>/dev/null \
    || _fresh_fail "could not load scripts/tests/lib/fresh.sh — refusing to run unlocked."
  command -v _gate_lock_acquire >/dev/null 2>&1 \
    || _fresh_fail "fresh.sh loaded but _gate_lock_acquire is missing."
  _gate_lock_acquire || {
    # NOTHING IS WRITTEN on this path: the artifact belongs to whoever holds the lock. Contention is
    # the COMMON case of that, and it was the one case that said nothing about the consequence — so an
    # operator saw "requires exclusive access", then read an artifact describing an earlier run.
    echo "error: the mutation campaign requires exclusive access to the repository." >&2
    echo "       This run wrote NOTHING. .mutation-campaign-summary may still hold an EARLIER" >&2
    echo "       run's result — including completed=1. Do not read it as describing this attempt." >&2
    exit 2
  }
  export CAMPAIGN_HELD_LOCK=1
  # RELEASED FROM THE MOMENT IT IS HELD.
  #
  # The supervisor's cleanup trap is installed much later, just before the child is spawned. Every
  # failure between here and there — the decision library not loading, treestate not loading,
  # ts_require unsatisfied — exited holding the lock, and the next run would then refuse to start
  # against a repository where nothing was running. Naming those three sites individually would fix
  # the three that exist today; a trap fixes the ones added tomorrow. No child exists yet on any of
  # these paths, so releasing is unconditionally correct here. The richer trap replaces this one.
  trap '_gate_lock_release 2>/dev/null || true' EXIT
  # THE SAME PARTIAL-NESS FACT AS EVERY OTHER SITE. This keyed on ONLY, which FAMILY_ID does not
  # resolve until the inventory is built — so an identity-selected run invalidated the FULL record
  # at startup, before doing any work. That is the same defect as the publication target, in the one
  # place that runs FIRST.
  _early_target="$ROOT_DIR/.mutation-campaign-summary"
  [ "${CAMPAIGN_PARTIAL:-0}" = "0" ] || _early_target="$_early_target.partial"
  if ! { _early="$(mktemp "$_early_target.XXXXXX" 2>/dev/null)" \
         && printf 'completed=0\nrefusals= run_started_and_did_not_finish\n' > "$_early" \
         && mv -f "$_early" "$_early_target"; }; then
    rm -f "${_early:-}" 2>/dev/null || true
    # WRITING IS NOT THE ONLY WAY TO DISCREDIT A RECORD. If the atomic write fails AFTER the lock is
    # held, the old artifact would otherwise survive an attempted run — so remove it outright. By this
    # harness's own doctrine an ABSENT artifact is a refusal and never a success, so deletion is the
    # safe fallback; only a failure to delete is unrecoverable.
    if rm -f "$_early_target" 2>/dev/null && [ ! -e "$_early_target" ]; then
      echo "warning: could not write the invalidation record, so the previous one was DELETED." >&2
      echo "         An absent artifact reads as 'no completion', which is correct here." >&2
      _early_deleted=1
    fi
    [ "${_early_deleted:-0}" = "1" ] || \
    echo "FATAL: could not discredit the previous campaign record at $_early_target." >&2
    echo "       Refusing to run: an old completed=1 would stay readable as this run's result." >&2
    _gate_lock_release
    exit 2
  fi
fi

if [ -z "${CONCRETE_MUT_SNAPSHOT:-}" ]; then
  # Same namespace as the workspace below, and marked, so a later run's sweep reclaims it: the EXIT
  # trap that removes this cannot fire on SIGKILL. Measured 2026-08-21: nine of these were stranded.
  # CHECKED. An unchecked `mktemp -d` leaves the variable EMPTY on failure, after which `_snap`
  # becomes "/driver.sh" and the workspace below becomes "/repo" — so an ordinary disk-full or
  # unwritable-TMPDIR condition turns into writes at the filesystem root.
  _snap_dir="$(mktemp -d "${TMPDIR:-/tmp}/concrete-mut.$$.XXXXXX")" || {
    echo "FATAL: could not create a temp directory for the driver snapshot" >&2
    _gate_lock_release 2>/dev/null || true; exit 2; }
  [ -n "$_snap_dir" ] && [ -d "$_snap_dir" ] || {
    echo "FATAL: temp directory for the driver snapshot is not usable" >&2
    _gate_lock_release 2>/dev/null || true; exit 2; }
  # NORMALIZED THE SAME WAY THE SNAPSHOT WILL LOCATE ITSELF (`cd && pwd`). macOS's `TMPDIR` ends in
  # `/`, so `mktemp` returned `.../T//concrete-mut.X`, while the snapshot's self-check resolves to
  # `.../T/concrete-mut.X` — two spellings of one directory, compared as strings, and every
  # legitimate snapshot was refused as "not inside the snapshot directory".
  _snap_dir="$(cd "$_snap_dir" && pwd)" || {
    echo "FATAL: temp directory for the driver snapshot cannot be resolved" >&2
    _gate_lock_release 2>/dev/null || true; exit 2; }
  printf 'owner_pid=%s\nstarted_head=driver-snapshot\n' "$$" > "$_snap_dir/.concrete-mutation-workspace"
  _snap="$_snap_dir/driver.sh"
  cp "${BASH_SOURCE[0]}" "$_snap" || {
    echo "FATAL: could not snapshot the campaign driver" >&2
    _gate_lock_release 2>/dev/null || true; exit 2; }
  # ROOT_DIR IS PASSED EXPLICITLY. After `exec`, `BASH_SOURCE[0]` is the snapshot under /tmp, so
  # deriving the repository root from it lands in the wrong tree and every mutation target reads as
  # "file no longer exists" — measured on the first attempt, 78/78 spurious failures.
  # HASH THE COPY, NOT THE ORIGINAL. Hashing `BASH_SOURCE[0]` here reads the LIVE repository file
  # again, after the `cp` above — so an edit landing between the two produced a snapshot of the OLD
  # bytes labelled with the NEW hash, and end reconciliation then compared the live file against that
  # new hash and found them equal. The only value that cannot lie about what is executing is the
  # digest of the bytes that were actually copied and are about to be exec'd.
  CONCRETE_MUT_SNAPSHOT="$_snap" \
  CONCRETE_MUT_ROOT="$ROOT_DIR" \
  CONCRETE_MUT_PREAMBLE_SHA="$_PREAMBLE_SHA" \
  CONCRETE_MUT_DRIVER_SHA="$(sha256sum "$_snap" 2>/dev/null | cut -c1-32 \
                             || shasum -a 256 "$_snap" | cut -c1-32)" \
  CONCRETE_MUT_SNAPDIR="$_snap_dir" \
    exec bash "$_snap" "$@"
fi
# From here on we are the snapshot, running against the repository the launcher named.
ROOT_DIR="${CONCRETE_MUT_ROOT:?snapshot launched without a repository root}"
# The lock was acquired before the snapshot and survives the `exec` (same pid, exported token). This
# phase only needs the library so it can RELEASE, and must not acquire again.
if [ "${CAMPAIGN_HELD_LOCK:-0}" = "1" ]; then
  . "$ROOT_DIR/scripts/tests/lib/fresh.sh" 2>/dev/null || {
    echo "FATAL: could not load fresh.sh in the snapshot phase — cannot release the lock." >&2; exit 2; }
fi
# AND IT RELEASES THE LOCK, BECAUSE `exec` CLEARED THE TRAP THAT DID.
#
# The lock is taken before the snapshot and survives the re-exec — but traps do not. So the
# releasing trap installed at acquisition was gone by the time this image started, and every failure
# between here and the supervisor's own trap exited HOLDING the lock: the decision library not
# loading, treestate not loading, ts_require unsatisfied. I had already 'fixed' this once and
# confirmed it with a control that looked for the wrong lock filename — the control passed, the lock
# was stranded every time, and the next run refused to start against a repository where nothing was
# running.
#
# The role is not decided at install time but IS decided when this fires, and the child must never
# release: the supervisor holds the tree until the artifact is installed.
trap '_rm_snapdir; [ "${CONCRETE_MUT_ROLE:-supervisor}" = "child" ] || _gate_lock_release 2>/dev/null || true' EXIT
cd "$ROOT_DIR"

# =================================================================================================
# SUPERVISOR BOUNDARY.
#
# A process cannot safely publish a verdict about its own exit. Reconciliation and artifact
# installation used to happen INSIDE the run, before the EXIT trap performed its final dirty-target
# check — so a target changing after the last reconciliation let the script print and durably store
# `qualified=1` and THEN exit nonzero from the trap. A failed process left a passing artifact behind.
# No additional trap fixes that; the publisher has to outlive the thing it is judging.
#
# So the run is split. The CHILD executes the campaign and may only write a CANDIDATE record. The
# SUPERVISOR observes the child's exit, re-reconciles the tree ITSELF, and is the only role that may
# install the authoritative artifact or write qualified=1. Both roles are the same immutable
# snapshot, so this adds a process boundary without adding a second implementation.
#
# The supervisor's reconciliation is INDEPENDENT, not a re-read of what the child claimed: it
# captures tree state before spawning and again after reaping, so a child that lied or died mid-write
# is caught by an observer that never trusted it.
#
# The lock is acquired by the launcher and survives the exec by pid identity; the child inherits the
# exported token and is a verified descendant. Only the supervisor releases it, so the tree stays
# held until the artifact is installed.
# THE PINNED INVENTORY, HOISTED ABOVE THE DISPATCH. The supervisor needs it to judge whether a
# candidate's population describes the CORPUS rather than merely agreeing with itself, and it exits
# inside its own branch long before the inventory is built — so a constant defined next to that
# build is invisible to the role that audits it.
EXPECTED_FAMILIES=95

# LOADED FOR BOTH ROLES, BEFORE THE DISPATCH. The child computes the evidence root and the
# supervisor recomputes it; they must use the SAME function or two implementations could each agree
# with themselves and disagree with each other. Loading here also freezes the rules for this run —
# bash binds functions at source time — and the library is tracked, so a mid-run edit moves
# ts_tracked and is refused.
. "$ROOT_DIR/scripts/tests/lib/campaign_supervise.sh" 2>/dev/null \
  || { echo "FATAL: cannot load the campaign decision library" >&2; exit 2; }

CONCRETE_MUT_ROLE="${CONCRETE_MUT_ROLE:-supervisor}"
# THERE ARE EXACTLY TWO ROLES, AND AN UNRECOGNISED ONE IS NOT A THIRD.
#
# Supervision was selected by `= supervisor` and the child path by `= child`, so ANY other value fell
# through both: the run proceeded with no supervisor at all, and write_summary — which redirects to
# the candidate file only for the child — targeted the AUTHORITATIVE artifact directly. An inherited
# or mistyped CONCRETE_MUT_ROLE was therefore a way to publish qualified=1 with the entire supervisor
# boundary bypassed. It is an enumeration; anything else is a refusal.
case "$CONCRETE_MUT_ROLE" in
  supervisor|child) ;;
  *) echo "FATAL: CONCRETE_MUT_ROLE='$CONCRETE_MUT_ROLE' is not a role." >&2
     echo "       Only 'supervisor' and 'child' exist. Any other value would run this campaign with" >&2
     echo "       no supervisor and publish straight to the authoritative artifact." >&2
     exit 2 ;;
esac

# Anchor and coverage modes publish nothing and are not campaigns; supervising them would add a fork
# and a second set of failure modes for no verdict.
if [ "${ANCHORS_ONLY:-0}" != "1" ] && [ "${CONCRETE_MUT_ROLE}" = "supervisor" ] \
   && ! _mut_query_mode "${1:-}"; then
  # The supervisor forks BEFORE the campaign preamble, so it loads the tree-state library itself
  # rather than relying on the child's later sourcing. One producer, used by both roles.
  . "$ROOT_DIR/scripts/tests/lib/treestate.sh" 2>/dev/null \
    || { echo "FATAL: supervisor cannot load treestate.sh — it would have nothing to reconcile with" >&2; exit 2; }
  ts_require || { echo "FATAL: supervisor tree-state producers unavailable" >&2; exit 2; }
  # LOADED BEFORE ANYTHING IS MEASURED OR SPAWNED. Sourcing the decision library AFTER the final
  # reconciliation left a window in which an edit to it was never remeasured: the supervisor would
  # have judged this run with rules loaded after it finished looking. Bash binds functions at source
  # time, so loading here freezes the rules, and the library is TRACKED, so an edit during the run
  # also moves ts_tracked and is refused.

  _sup_head0="$(ts_head "$ROOT_DIR" 2>/dev/null)"
  _sup_tracked0="$(ts_tracked "$ROOT_DIR" 2>/dev/null)"
  _sup_untracked0="$(ts_untracked "$ROOT_DIR" 2>/dev/null)"

  # ONE OWNER FOR THE RUN ID, COMPUTED BEFORE THE CHILD EXISTS. The child used to derive it from
  # START_HEAD, which is established a thousand lines after the supervisor has already spawned and
  # bound its launcher status to it — `set -u` caught that immediately as an unbound variable. The
  # supervisor computes it here from tree state it already holds and exports it, so both roles name
  # the same run by construction rather than by two derivations agreeing.
  RUN_ID="$(printf '%.12s' "${_sup_head0:-nohead}")-$(date +%Y%m%dT%H%M%S)-$$-$(basename "$(mktemp -u XXXXXX)")"
  export CONCRETE_MUT_RUN_ID="$RUN_ID"

  # THE SUPERVISOR OWNS THE LOCK, SO IT RELEASES IT ON EVERY EXIT. Acquisition happens before the
  # exec, and the supervisor previously released only along its normal paths — so a supervisor that
  # died first (measured: an unbound variable at dispatch) left the lock held by a dead pid, and the
  # next campaign refused. That refusal is correct, since this harness deliberately never reclaims
  # from a dead pid, but requiring a human for every crash is a poor trade when the owner can simply
  # release on the way out. Release is creator-checked, so releasing twice is safe.
  # The trap consults supervisor_must_hold_lock on the OBSERVED group state rather than releasing
  # unconditionally. A flag set at the refusal site would be a second copy of that rule, and the trap
  # can fire on paths that never reach the refusal — where the state is still not `empty`, and where
  # an unset state must therefore hold rather than release.
  _sup_exit_cleanup() {
    _rm_snapdir
    if supervisor_must_hold_lock "${_group_state:-no_child_launched}"; then
      echo "REFUSING TO RELEASE THE REPOSITORY LOCK: campaign work may still be running." >&2
      echo "  Inspect pgid ${_child_pgid:-unknown}; remove the lock only once nothing remains." >&2
      return 0
    fi
    _gate_lock_release 2>/dev/null || true
  }
  # AND FROM HERE THE TRAP IS THE ONLY RELEASE SITE.
  #
  # Every path below used to call _gate_lock_release itself before exiting. The refusal path did it
  # too — so the supervisor printed "campaign work may still be running", released the lock, and only
  # then reported that it was retaining it. Adding the hold rule to the trap while leaving eight
  # explicit releases in front of it produced exactly the shape this harness exists to catch: a
  # protection that reads as present and never runs. There is one release, it is here, and it asks
  # supervisor_must_hold_lock first.
  trap '_sup_exit_cleanup' EXIT

  # THE CHILD RUNS IN ITS OWN SESSION, so "is any campaign work still running" is answered by the
  # kernel — does the child's process group still have members — rather than by matching command
  # lines or walking /proc. The supervisor and every ancestor are outside that group by
  # construction, so there is no exclusion list to get wrong, and a worker that rewrites its argv is
  # still a member. Built on portable POSIX mechanisms so the two platforms are INTENDED to agree,
  # with CI on both as the confirmation; /proc is Linux-only
  # and this repository has already taken a macOS outage from depending on it.
  _launch_report="$(mktemp "${TMPDIR:-/tmp}/mutlaunch.XXXXXX")" || {
    echo "FATAL: supervisor cannot stage the launch report" >&2; exit 2; }
  # THE STATUS IS BOUND TO THIS RUN. Without a run id a report left by an earlier launch is
  # indistinguishable from this one's, and the supervisor would publish against a stale answer.
  CONCRETE_MUT_ROLE=child python3 "$ROOT_DIR/scripts/tests/lib/run_campaign_child.py" \
    --report "$_launch_report" --run-id "$RUN_ID" -- bash "$0" "$@"
  _launch_rc=$?
  # FROM THIS MOMENT A CHILD HAS EXISTED, SO THE LOCK IS HELD UNTIL PROVEN OTHERWISE.
  #
  # Moving the report guard ahead of the field extraction stopped a REJECTED report populating
  # `_group_state` — and created the opposite fail-open, because unset maps to `no_child_launched`,
  # which means RELEASE. The comment written alongside that change claimed the trap would hold. It
  # did not, and the claim was false the moment it was written. `no_child_launched` is only true
  # BEFORE the launcher runs; after it the honest state is "a child ran and I do not yet know
  # whether its group is empty", which the enumeration answers by holding.
  _group_state="launched_state_unknown"
  # THE DRIVER NAMES ITSELF, rather than letting the body infer the inventory from $0 — which was
  # correct only for as long as the body was inside this file.
  CAMPAIGN_DRIVER="$0"

  # THE RECONCILIATION AND PUBLICATION BODY LIVES IN THE DECISION LIBRARY, so that a gate can run
  # these exact lines and a mutation neutering a refusal has something to turn red. It was inline
  # here, unreachable by any gate: the decisions were well controlled, the asking was not.
  supervisor_reconcile_and_publish
fi

# THE CHILD DOES NOT RELEASE THE LOCK. The supervisor holds the tree until the artifact is installed;
# a child that released here would let another run start against a tree whose verdict is still being
# decided.
if [ "$CONCRETE_MUT_ROLE" = "child" ]; then
  _gate_lock_release() { return 0; }
fi

# `--coverage`: report which soundness gates have a control, computed from the GATE FIELD of
# the `add` lines below — not from a grep for gate names, which counts prose.
if [ "${1:-}" = "--coverage" ]; then
  SELF="$0"
  TARGETED="$(grep -E '^add "' "$SELF" | awk '{print $4}' | tr -d '"' | sort -u)"
  sound=0; cov=0; missing=""
  for f in scripts/tests/check_*.sh; do
    b="$(basename "$f")"
    k="$(grep -ciE 'unsound|must never|false green|no false|trap|proved|axiom|forge|launder' "$f")"
    [ "$k" -ge 4 ] || continue
    sound=$((sound+1))
    if printf '%s\n' "$TARGETED" | grep -qx "$b"; then cov=$((cov+1)); else missing="$missing $b"; fi
  done
  echo "soundness gates: $sound"
  echo "  with a negative control: $cov"
  echo "  without: $((sound-cov))"
  echo "uncovered:"; printf '%s\n' $missing | sed 's/^/  /'
  exit 0
fi

# family i: NAME FILE GATE NEEDS_BUILD ; OLD/NEW written to temp files per family.
NAME=();  FILE=();  GATE=();  BUILD=()
OLD=();   NEW=()
add(){ NAME+=("$1"); FILE+=("$2"); GATE+=("$3"); BUILD+=("$4"); OLD+=("$5"); NEW+=("$6"); }

# family_spec_digest <name> <file> <gate> <build> <old> <new> -> digest of the EXPERIMENT
#
# Length-prefixed, so no field can be forged by embedding a separator in another. This is what a
# caller pins when it means "the mutation that did X", as opposed to "whatever is currently called
# X" — a family may be renamed without changing what it tests, and may keep its name while becoming
# a different experiment entirely. Only the second of those should invalidate a pinned reference.
# THE NAME IS DELIBERATELY NOT A COMPONENT. It was, which made the paragraph above false: with the
# label inside the digest, renaming a family necessarily changed its spec, so the two identifiers
# moved together and there was no way to express "same experiment, new name" — the exact distinction
# they exist to draw. The first argument is still accepted and ignored, so every call site reads as
# the full family and the omission is stated once, here.
family_spec_digest() {
  local LC_ALL=C
  printf '%d:%s,%d:%s,%d:%s,%d:%s,%d:%s' \
    "${#2}" "$2" "${#3}" "$3" "${#4}" "$4" "${#5}" "$5" "${#6}" "$6" \
    | { sha256sum 2>/dev/null || shasum -a 256; } | cut -d' ' -f1
}

add "corecheck-unsafe-op" "Concrete/Check/CoreCheck.lean" "check_corecheck_boundary.sh" yes \
  'addCCError (.missingCapability "*raw_ptr" "Unsafe" "")' \
  'pure ()'
add "copy-predicate" "Concrete/Check/Layout.lean" "check_copy_judgment.sh" yes \
  '| some (isC, _, _) => isC' \
  '| some (_, _, _) => true'
add "checked-arith-trap" "Concrete/Backend/EmitSSA.lean" "check_checked_arith.sh" yes \
  $'      let mnem := if ssaIsSignedInt operandTy then "sadd" else "uadd"\n      emitStructured s (.call (some dst) iTy (.global (checkedCallName mnem operandTy)) [(iTy, lOp), (iTy, rOp)])' \
  $'      let _mnem := if ssaIsSignedInt operandTy then "sadd" else "uadd"\n      emitStructured s (.binOp dst .add iTy lOp rOp)'
add "capability-requirement" "Concrete/Check/CoreCheck.lean" "check_capability_judgment.sh" yes \
  'if !capD.satisfied then' \
  'if false then'
add "walker-constructor" "Concrete/Check/CoreCheck.lean" "check_constructor_coverage.sh" no \
  '| .intLit _ _ | .floatLit _ _ | .boolLit _ | .strLit _ | .charLit _ => pure ()' \
  '| .intLit _ _ | .floatLit _ _ | .boolLit _ | .strLit _ | _ => pure ()'
add "source-span-stamping" "Concrete/Elab/Elab.lean" "check_source_maps.sh" no \
  'declSpan := some f.span' \
  'declSpan := none'
# Re-anchored for R-0484: the suffix also carries capability arguments (`capPart`). The
# experiment is unchanged — erase what distinguishes the type arguments, so distinct
# instantiations share a name. Every binding stays used: an unused one fails the build, and
# a mutant that does not build proves nothing about the gate.
add "mono-name-hygiene" "Concrete/IR/Mono.lean" "check_mono_name_collision.sh" yes \
  '    n ++ "_T_" ++ "_".intercalate (args.map tyToSuffix) ++ capPart ++ "_E"' \
  '    n ++ "_T_" ++ "_".intercalate (args.map fun _ => "X") ++ capPart ++ "_E"'
add "diagnostic-quality" "Concrete/Check/CoreCheck.lean" "check_diagnostics_quality.sh" yes \
  '| .insufficientCapabilities _ required _ => some s!"add '"'"'with({required})'"'"' to the calling function, or wrap the call in a trusted function"' \
  '| .insufficientCapabilities _ _ _ => none'
add "fact-invalidation" "Concrete/Report/ReportObligations.lean" "check_scoped_collector.sh" yes \
  '| _ => dropStaleHyps scope (assignedScalarsS s)' \
  '| _ => scope'
add "report-schema-row" "Concrete/Report/Report.lean" "check_vc_schema.sh" yes \
  '("kind", .str v.kind),' \
  '("knd", .str v.kind),'

# ---------------------------------------------------------------------------
# 2026-08-04: the R-0460..R-0465 gates. Registered because they were NOT here, and
# every failure of the past week landed in exactly the uncovered set: a gate that
# passed while the thing it guarded was broken. 11 of 180 gates had negative
# controls; the newest and most load-bearing had none.
#
# Each mutation below was verified by hand when the gate was written. Verified by
# hand means verified once, by whoever remembered. These entries make it repeat.
# ---------------------------------------------------------------------------

# H23's composition: drop the cap and a proved obligation resting on an unproved
# invariant reads proved again. This is the defect, restored exactly.
add "hypothesis-cap" "Main.lean" "check_known_wrong_corpus.sh" yes \
  'return Report.capOnHypothesisDebt (Report.dischargeVCs vcs omegaProved (bvCallKeys ++ bvOvfKeys))' \
  'return Report.dischargeVCs vcs omegaProved (bvCallKeys ++ bvOvfKeys)'

# H24's insufficiency: drop the quotient condition and a division reports proved
# while trapping on signed MIN / -1. Also breaks trapConditions_sufficient, so this
# one is expected to fail the BUILD as well as the gate — recorded because a
# mutation that cannot compile is a stronger result than a gate going red.
add "trap-quotient-condition" "Concrete/Semantics/IntArith.lean" "check_known_wrong_corpus.sh" yes \
  '| .div | .mod => [.divisorNonZero, .quotientInRange]' \
  '| .div | .mod => [.divisorNonZero]'

# The multi-kernel firewall: let attestation act on an unproved VC and a badge can be
# minted for something no kernel proved.
add "attestation-precondition" "Concrete/Report/Report.lean" "check_discharge_adapters.sh" yes \
  $'    actsOn := fun s => s == "proved_by_kernel_decision" || s == "proved_by_lean"\n                       || s == "proved_by_lean_replay",' \
  $'    actsOn := fun _ => true,'

# The independence coordinate: collapse Isabelle into CIC and the badge overstates
# foundational independence — two CIC kernels reported as two foundations.
add "kernel-foundation" "Concrete/Report/Evidence.lean" "check_evidence_algebra.sh" yes \
  '| "isabelle" => .hol' \
  '| "isabelle" => .cic'

# The reference evaluator's division convention: swap truncating for Lean's `/` and
# every lowering-agreement check is validated against the wrong reference at
# negative operands.
# Re-pointed 2026-08-04: the reference evaluator moved into the term IR when `evalIntEnv`
# stopped being a `partial def`, so the division convention now lives in `TermIR.evalInt`.
# The mutation is meaningful — Lean's `/` on Int is FLOORED (`-7 / 2 = -4`) while `.tdiv`
# truncates (`-3`), so swapping them silently re-points the reference every rendering is
# validated against, at exactly the inputs positive tests never reach.
#
# Expect "killed by build": the convention is now pinned by `rfl` examples, and for a
# compile-time lock a build failure IS the correct kill — which this harness can finally say,
# as of the invalid-mutation split above.
add "reference-division" "Concrete/Semantics/TermIR.lean" "check_vc_bridge_register.sh" yes \
  '| .tdiv => if b == 0 then none else some (IntArith.tdiv a b)' \
  '| .tdiv => if b == 0 then none else some (a / b)'

# The artifact fuzzer's claim classification: treat every function as claimed and a
# trap in an admittedly-unproved function is reported as a Register A counterexample —
# a false alarm, which is how a fuzzer gets switched off.
add "fuzz-claim-classes" "Concrete/Report/ReportObligations.lean" "check_artifact_fuzz.sh" yes \
  'artifactFuzzDriverFor (cases.filter (fun c => c.claim != "unproved")) "artifact_fuzz_all"' \
  'artifactFuzzDriverFor cases "artifact_fuzz_all"'

# ---------------------------------------------------------------------------
# 2026-08-04, second pass: a sweep of the SOUNDNESS gates that had no control.
#
# Selection rule, so the next person can extend it the same way: a gate qualifies
# if breaking the rule it guards would let the compiler assert something FALSE
# about a program — a missing trap, a false `proved`, a laundered axiom. Gates over
# formatting, naming, docs hygiene or CI plumbing are out of scope; they matter, but
# their failure mode is noise, not a wrong claim.
# ---------------------------------------------------------------------------

# Bug 053, restored exactly: make every unary op read as non-side-effecting and a
# DISCARDED integer negation is deleted, taking the documented MIN trap with it. The
# compiled program exits 0 where the interpreter aborts.
add "trap-preservation-unary" "Concrete/IR/SSACleanup.lean" "check_trap_inventory.sh" yes \
  'if !(IntArith.unaryOpCanTrap op ty) then false' \
  'if true then false'

# NO FALSE GREEN, the obligation red-team's first clause: make the constant-divisor
# verdict unconditionally true and `x / 0` at compile time reads proved.
add "const-divisor-verdict" "Concrete/Report/ReportObligations.lean" "check_obligation_redteam.sh" yes \
  '| some k => (some (decide (k ≠ 0)), none)' \
  '| some k => (some (decide (k == k)), none)'

# The same false-green in the bounds family: a constant index outside the array
# reads proved.
add "const-bounds-verdict" "Concrete/Report/ReportObligations.lean" "check_obligation_redteam.sh" yes \
  '| some k => (some (decide (0 ≤ k) && decide (k < (Int.ofNat n))), none)' \
  '| some k => (some (decide (k == k)), none)'

# The FOLD path, which is what check_int_arith_semantics.sh actually guards:
# interpret == fold-then-interpret == compiled. Make a trapping constant operation fold
# to a value and the trap disappears under constant folding — the interpreter aborts
# while the folded/compiled program returns 0.
#
# Chosen after a first attempt failed for an INSTRUCTIVE reason. Removing the div
# overflow trap from `evalIntBinOp` does not compile: `div_obligation_necessary`
# (R-0460) rejects it. That is stronger protection than a gate — but it meant the
# family proved the THEOREM was load-bearing while claiming to prove the gate was, and
# a control that validates something other than its stated subject is exactly the
# false-green this harness exists to prevent. `foldIntBinOp` is covered by no theorem,
# so a mutation there reaches the gate.
add "fold-drops-trap" "Concrete/Semantics/IntArith.lean" "check_int_arith_semantics.sh" yes \
  '| .trap _    => some none' \
  '| .trap _    => some (some 0)'

# ---------------------------------------------------------------------------
# 2026-08-04, third pass: the remaining soundness gates, batch 1.
# Each mutation removes a RUNTIME CHECK or a trust boundary, so the compiled program
# does something the compiler said it would not.
# ---------------------------------------------------------------------------

# H8's check: stop emitting the array bounds check and an out-of-range index reads
# and writes past the array instead of aborting. The obligation layer is untouched,
# so this is the pure lowering failure — proved obligation, unchecked artifact.
# INFLATE the length rather than deleting the call or replacing the length outright. Two
# earlier attempts died on a lint, not on the check: `pure ()` left `idxI64` unused, and a
# constant length left `len` unused, and this project treats an unused binding as a build
# error. A mutation that cannot compile proves nothing about the gate (see fold-drops-trap).
# `len + 999999999` keeps both bindings live and makes the check pass for every real index.
add "bounds-check-emission" "Concrete/IR/Lower.lean" "check_array_bounds.sh" yes \
  '[idxI64, .intConst (Int.ofNat len) .int] .unit)' \
  '[idxI64, .intConst (Int.ofNat len + 999999999) .int] .unit)'

# ---------------------------------------------------------------------------
# 2026-08-04, batch 2 of the remaining soundness gates.
# ---------------------------------------------------------------------------

# Checked integer NEGATION: make `-x` compute `0 - 0` so it neither traps at MIN nor
# returns the right value. Bug 053's sibling — that one deleted a discarded negation,
# this one keeps it and makes it wrong. Operands changed rather than the call replaced,
# because dropping the call leaves `mnem` unused and this project errors on that.
add "checked-negation" "Concrete/Backend/EmitSSA.lean" "check_arith_redteam.sh" yes \
  '[(iTy, .intLit 0), (iTy, valOp)]' \
  '[(iTy, .intLit 0), (iTy, .intLit 0)]'

# Proof FRESHNESS (bugs 059/060): make staleness detection always answer "fresh", so a
# proof whose subject has changed underneath it keeps reading proved. The stored digest
# is still there and still compared — the comparison just always agrees, which is the
# shape a real staleness bug takes.
# RE-ANCHORED 2026-08-20. The comparison moved into `specIsStale` when the three private copies of
# it were lifted into one definition during the V2 activation, and the old anchor named the
# body-only form that no longer exists. Same intent: staleness can never fire, so an edited body
# keeps reporting proved.
add "proof-staleness" "Concrete/Proof/ProofCore.lean" "check_proof_freshness.sh" yes \
  $'  | some h => storedFreshness h subjectDigestV2 == StoredFreshness.moved\n  | none   => a.expectedFp != currentFp' \
  $'  | some h => storedFreshness h subjectDigestV2 == StoredFreshness.current && h != h\n  | none   => a.expectedFp != a.expectedFp && currentFp == currentFp'

# H2's check: route float→int through a raw `fptosi` instead of the checked helper.
# LLVM says raw fptosi is POISON on NaN/±inf/out-of-range, so this is undefined
# behaviour reachable from safe source.
add "checked-float-cast" "Concrete/Backend/EmitSSA.lean" "check_float_cast.sh" yes \
  'emitStructured s (.call (some dst) dstLLTy (.global (checkedF2ICallName srcTy targetTy)) [(srcLLTy, valOp)])' \
  'emitStructured s (.cast dst .fptosi srcLLTy valOp dstLLTy)'

# Wrapping arithmetic must NOT trap: make `wrapping_add` emit the CHECKED add and a
# documented-wrapping operation aborts instead of wrapping. The inverse direction of
# the checked-arith family — that one removes a trap, this one adds one where the
# language promises none.
add "wrapping-stays-unchecked" "Concrete/Backend/EmitSSA.lean" "check_wrapping_arith.sh" yes \
  '| .wrappingAdd => emitStructured s (.binOp dst .add iTy lOp rOp)' \
  '| .wrappingAdd => emitStructured s (.call (some dst) iTy (.global (checkedCallName "sadd" operandTy)) [(iTy, lOp), (iTy, rOp)])'

# ---------------------------------------------------------------------------
# 2026-08-04, batch 3. APPENDED at the end deliberately: family numbers are
# positional, and inserting mid-list earlier today renumbered everything below and
# sent a validation run at two already-covered families.
# ---------------------------------------------------------------------------

# A SURVIVOR that turned into a finding about the GATE, not about the rule.
#
# Making `blockDiverges` always answer false — so code after a `return` reads as
# reachable — leaves `check_totality_judgment.sh` GREEN. That gate's header names
# `blockDiverges` among the single-sourced facts it covers, but what it actually tests is
# arithmetic traps and interp/compiled value agreement; no fixture in it exercises
# divergence detection. Its scope is narrower than its name and header claim, and that is
# worth knowing on its own.
#
# The RULE is guarded, though, and decisively: measured with the mutation applied,
# `make test` goes from 1702/0 to **1315 passed / 76 failed** — E0213 linear-variable
# errors, because divergence detection is what allows an `if` without `else` whose
# then-branch returns. So this family targets `run_tests.sh`, the check that actually
# kills it, rather than the gate whose name suggested it should.
#
# Recorded this way because the first instinct — delete the family, or leave it aimed at a
# gate it does not exercise — would have converted a measurement into either silence or a
# false green.
add "divergence-detection" "Concrete/Check/CheckHelpers.lean" "run_tests.sh" yes \
  $'partial def blockDiverges (stmts : List Stmt) : Bool :=\n  match stmts.getLast? with\n  | none => false\n  | some s => stmtDiverges s' \
  $'partial def blockDiverges (stmts : List Stmt) : Bool :=\n  match stmts.getLast? with\n  | none => false\n  | some _ => false'

# Check and Elab must route integer-literal typing through the ONE shared judgment.
# Make Check DISCARD a hint Elab still honours and the E0228/E0715 bug class returns:
# two passes inferring a source type independently and disagreeing. Phrased to keep
# `hintR` live — dropping it outright leaves it unused, which this project treats as a
# build error, and a mutation that cannot compile tests nothing.
add "shared-type-judgment" "Concrete/Check/Check.lean" "check_type_agreement.sh" yes \
  'let d := TypeJudgment.intLitDecision hintR' \
  'let d := TypeJudgment.intLitDecision (if hintR.isSome then none else hintR)'

# R-0455 Register B row 1. Making the transformation a NO-OP still preserves meaning — the
# soundness theorem holds — so the thing that must fail is the non-vacuity lock. It does, at
# BUILD time, because those locks are `rfl` examples: for a compile-time lock, "killed by
# build" is the correct mechanism rather than the weak one, unlike a mutation that dies on an
# unrelated lint. This family records that the vacuity guard, not just the soundness proof,
# is load-bearing.
add "transform-has-effect" "Concrete/Semantics/TermIR.lean" "check_transform_register.sh" yes \
  $'    | .bin .tmod l r =>\n      let l\' := elimTmod l\n      let r\' := elimTmod r\n      .bin .sub l\' (.bin .mul r\' (.bin .tdiv l\' r\'))' \
  $'    | .bin .tmod l r => .bin .tmod (elimTmod l) (elimTmod r)'

# R-0004 slice 5. The subject digest binds the STRUCTURAL body; before 2026-08-09 it bound the
# legacy Core-statement hash, which is what bugs 059/060 are filed against. This mutation
# restores the old behaviour in the direction that matters — the body stops contributing to the
# subject at all — and `check_proof_freshness.sh` must go red on its SHADOW(body) leg.
#
# It is the RIGHT mutation for this gate because it is indistinguishable from the pre-change
# state at every other surface: the digest still exists, still refuses incomplete facts, still
# moves on signature and contract edits. Only "a body edit moves the subject" is lost, and if
# the gate does not notice, then binding the structural body bought nothing.
# RE-ANCHORED 2026-08-20 (second attempt). The subject digest was restructured to build from
# `implementationPreimage`, so the previous anchor named a `subjectV2:`/`bodyBytesV2` construction
# that no longer appears anywhere in this file. The mutation still removes the BODY's contribution —
# it reduces the preimage to a single boolean — while keeping `complete` live, because a mutation
# that merely drops a binding fails on a lint and is scored INVALID rather than killed.
# RENAMED FROM `subject-binds-body`, because that is not what it establishes.
#
# The mutation reduces the whole preimage, so EVERY stored fingerprint changes — and
# check_proof_freshness.sh:107 requires an untouched fixture to stay `proved`, which fails on that
# alone. The gate therefore goes red via the stored-fingerprint assertion, NOT via the body-binding
# assertion at :226 that the old name claimed. Any mutation of the digest SCHEME has the same problem,
# so this family cannot isolate body-binding at all; renaming it to the claim it actually supports is
# the honest repair. The body-binding assertion remains unproven by mutation — recorded as a coverage
# gap rather than quietly credited here.
add "subject-digest-binds-preimage" "Concrete/Proof/ProofCore.lean" "check_proof_freshness.sh" yes \
  $'      some (shortHash (implementationPreimage facts complete\n              ++ "|spec:" ++ specPart ++ "|scope:" ++ scopePart))' \
  $'      some (shortHash (facts.canonical ++ toString (implementationPreimage facts complete).isEmpty\n              ++ "|spec:" ++ specPart ++ "|scope:" ++ scopePart))'

# R-0004 slice 6, blocker (c) containment. `rowJustifies` refuses a classification whose theorem
# SHAPE cannot support it, and `classifiedEdgeOf` downgrades such a row to `unclassified`. The
# probes for that use SYNTHETIC rows, which proves the function rejects — not that the consumer
# acts on it for a real row in the checked-in table.
#
# This mutation relabels a real row `body` -> `contract` while leaving `quantifies = false`. The
# row stays structurally valid: well-formed digests, one table, no duplicates. Only the
# correspondence condition fails, so nothing but `rowJustifies` can catch it — which is exactly
# what makes it the right mutation for this control.
# RE-ANCHORED 2026-08-20. Two things had moved: the row gained a trailing `specs` component when
# classification rows began carrying the spec constants a theorem is about, and the table digest
# changed with the corpus. Anchoring on a row LITERAL means this family needs re-anchoring whenever
# either moves — the alternative is matching loosely enough to keep applying to a row it no longer
# describes, which is worse. Same intent: the edge is reclassified `body` -> `contract`, so the
# justification no longer matches what the theorem actually establishes.
add "classification-justifies" "Concrete/Proof/ClassificationTable.lean" "check_dependency_edges.sh" yes \
  $'  ("Concrete.Proof.parse_byte_correct", "body", "7bcec2d7871f93204b26e2bf83d5acf1", [("Concrete.Proof.proofFns", "e41b73d684a263ed7a2f8cfebdc34727")], false, ["Concrete.Proof.parseByteExpr"]),' \
  $'  ("Concrete.Proof.parse_byte_correct", "contract", "7bcec2d7871f93204b26e2bf83d5acf1", [("Concrete.Proof.proofFns", "e41b73d684a263ed7a2f8cfebdc34727")], false, ["Concrete.Proof.parseByteExpr"]),'

# R-0004 slice 6. `tableEntryEvidence` RECOMPUTES the canonical body digest from `PFnDef.body` and
# refuses unless the stored provenance agrees. Before that, the stored digest was copied and every
# downstream check compared metadata to metadata, so this state bound successfully:
#
#     body replaced, `identity` retained, `sourceBodyDigest` retained
#
# The mutation restores exactly the old behaviour — the comparison still runs, its verdict is just
# discarded — so the table entry is trusted again while everything else about the join is
# untouched. Nothing but the recompute can catch it, which is what makes it the right control:
# schema and scope checks still pass, identities are unique, and the manifest join still agrees.
# RETARGETED 2026-08-13 when `tableEntryEvidence` moved from `Option` to a named-refusal `Except`.
# The harness FAILED rather than passing when its OLD text vanished, which is the behaviour that
# makes a stale control detectable instead of a quiet always-green.
# SPLIT IN TWO, 2026-08-21, because the single anchor matched BOTH body-provenance sites and was
# therefore inert: the applier refuses an ambiguous anchor, and the anchor gate had been accepting it
# because its `check` mode returned success before testing ambiguity. Each site is a separate
# authority path — one recomputes the digest of an ATTESTED MODEL's body, the other of a CALLEE's —
# and picking either one arbitrarily would have left the other unproven while the count still read 78.
add "entry-body-recomputed-attested-model" "Concrete/Proof/DependencyEdge.lean" "check_dependency_edges.sh" yes \
  $'          let recomputed := Concrete.sourceBodyDigestV1Of model.body\n          if stored.value != recomputed then' \
  $'          let recomputed := Concrete.sourceBodyDigestV1Of model.body\n          if false then'
add "entry-body-recomputed-callee" "Concrete/Proof/DependencyEdge.lean" "check_dependency_edges.sh" yes \
  $'          let recomputed := Concrete.sourceBodyDigestV1Of d.body\n          if stored.value != recomputed then' \
  $'          let recomputed := Concrete.sourceBodyDigestV1Of d.body\n          if false then'

# R-0004 slice 6, manifest provenance. `CompleteImplementation.of?` refuses facts describing a
# DIFFERENT callable than the one claimed, which is what stops a manifest row pairing one callable's
# identity with another's signature, contracts and capabilities.
#
# SCOPE OF THIS CONTROL, stated because the neighbouring gap is easy to conflate with it: this covers
# the facts/callable mispairing ONLY. A `body` or `extracted` belonging to another entry is NOT
# covered and has deliberately no mutation, because neither `CompleteEvidenceBodyV2` nor `PExpr`
# carries identity, so such a mutation would SURVIVE — and a surviving control recorded as coverage
# is worse than an absent one. See the named gap in ROADMAP.md R-0004 item 2.
add "implementation-facts-match-callable" "Concrete/Proof/ImplementationIdentity.lean" "check_dependency_edges.sh" yes \
  $'  if facts.id != callable then none' \
  $'  if false then none'

# R-0004 slice 6, manifest completeness. `ManifestResult.usable?` compares the rows against the
# STORED denominator (`expected`), which is what stops a producer that drops entries from returning a
# smaller manifest that looks complete. The mutation discards that comparison, restoring exactly the
# `filterMap` behaviour: rows are then trusted to be their own denominator.
#
# The refusals check is left intact by this mutation, so the probe that kills it is the one whose
# refusal list is EMPTY and whose rows are a strict subset of `expected` — i.e. it can only be caught
# by the stored-denominator comparison, not by any other condition.
add "manifest-rows-match-expected" "Concrete/Proof/DependencyEdge.lean" "check_dependency_edges.sh" yes \
  $'  else if r.impls.map (\u00b7.callable) != r.expected then none' \
  $'  else if false then none'

# R-0004 slice 6, the laundering itself. The producer records a NAMED refusal for an entry it cannot
# build a row for; the mutation drops that entry instead, which is exactly what `filterMap` did.
#
# This is the control for the REAL-CORPUS gate, not a synthetic one, and that is the point: under the
# mutation the corpus reports expected=63 rows=63 refused=0 usable=yes for every file — a complete
# manifest, with the incompleteness gone from the accounting rather than fixed. Only a gate that
# stores the denominator can see the difference, which is what check_impl_manifest.sh does.
add "manifest-refusal-recorded" "Concrete/Proof/ProofCore.lean" "check_impl_manifest.sh" yes \
  $'          | none => refuse .extractedMissing' \
  $'          | none => acc'

# R-0004 slice 6. THE DISPATCH IS SECURITY-RELEVANT INVENTORY. It maps a hand-back table name to the
# `FnTable` the compiler links, and per-edge correspondence rests on it: if an entry is removed or
# misrouted, the tables a theorem names stop resolving, its edges lose their witnesses, and the
# subject must fall to `missing` rather than corresponding on a table it never read.
#
# The mutation MISROUTES one entry to `none` — the shape a stale dispatch actually takes when a
# table is renamed and the list is not updated. It is caught by the real-corpus assertion
# (6/11 subjects fully correspond), not by a synthetic probe, so this covers the production path.
add "dispatch-entry-routes" "Concrete/Proof/TableResolve.lean" "check_dependency_edges.sh" yes \
  $'  | "Concrete.Proof.cryptoFns"         => some cryptoFns' \
  $'  | "Concrete.Proof.cryptoFns"         => none'

# R-0004 slice 6. Surplus must be RETAINED, not dropped. The review named this explicitly:
# "dropping surplus handling through filterMap must be mutation-killed". The mutation empties the
# surplus set while leaving every other set intact, which is exactly what a `filterMap` that
# discarded unmatched witnesses would do — the join still reports matched/missing/ambiguous and
# looks healthy.
add "correspondence-surplus-retained" "Concrete/Proof/Correspondence.lean" "check_dependency_edges.sh" yes \
  $'  let surplus := ours.filter (fun w => !(i.requestedEdges.any (fun r => witnessTargets r w)))' \
  $'  let surplus := []'

# R-0004 slice 6. External table material is GENERATOR-ASSERTED: no body exists to recompute it
# against, so the entry-derived digest recorded beside it is its ONLY independent verification.
# The mutation changes the stored digest alone, leaving the entry list intact — the shape a stale
# hand-back row takes after a table changes and the digest is regenerated but the rows are not, or
# vice versa. If the comparison is not load-bearing this passes unnoticed and external evidence is
# accepted on trust with a decorative digest beside it.
add "external-table-digest-checked" "Concrete/Proof/ClassificationTable.lean" "check_dependency_edges.sh" yes \
  $'  ("Examples.ProofPatterns.Proofs.combineFns", "1393cec60470308d80326ce29c170734", [("calls", "dbl", "b78225e71dcabeba3282cf29cdc93ef5"), ("calls", "inc", "547e67b5f2b072131034d8cec278c032")])' \
  $'  ("Examples.ProofPatterns.Proofs.combineFns", "00000000000000000000000000000000", [("calls", "dbl", "b78225e71dcabeba3282cf29cdc93ef5"), ("calls", "inc", "547e67b5f2b072131034d8cec278c032")])'

# R-0004 slice 6. MISROUTING a table name to ANOTHER TABLE'S VALUE — distinct from the
# routed-to-`none` case already covered. Routing to `none` makes the table unresolvable and the
# subject's edges fall to `missing`; routing to a WRONG table resolves successfully and answers
# membership from the wrong material, which is the more dangerous shape because nothing looks
# absent. `cryptoFns` is pointed at `elfFns`: both resolve, both have entries, and the callees
# simply are not the ones the theorem bound.
add "dispatch-routes-to-correct-table" "Concrete/Proof/TableResolve.lean" "check_dependency_edges.sh" yes \
  $'  | "Concrete.Proof.cryptoFns"         => some cryptoFns' \
  $'  | "Concrete.Proof.cryptoFns"         => some elfFns'

# R-0004 slice 6. RELABELLING asserted evidence as checked. The digest agreeing does not mean a
# body was ever read, and this mutation makes external material claim it was — the single most
# consequential lie this subsystem could tell, because every downstream consumer treats
# `compilerLinked` as recomputed-from-source.
add "external-stays-asserted" "Concrete/Proof/TableResolve.lean" "check_dependency_edges.sh" yes \
  $'      else .ok (.generatorAsserted, rows)' \
  $'      else .ok (.compilerLinked, rows)'

# R-0004 slice 6. IDENTITY RETENTION FOR EXCLUDED CALLEES. A trusted helper is excluded from the
# proof entries but is still a real callable carrying a scoped identity. Resolving callee names only
# against `entries` reported it as unresolved, which turned a `trusted` edge into a `missing` one and
# cost the subject its correspondence. Caught by the REAL-CORPUS correspondence assertion, not by a
# synthetic probe.
#
# RETARGETED 2026-08-20 to `scopedOf`, which is where the identity is actually RETAINED. The previous
# version mutated `labelOf`, and that was mis-targeted: `labelOf` is consulted only AFTER `scopedOf`
# has already refused, purely to label the unscoped diagnostic. Removing its excluded fallback
# changes presentation, not the closure, so the mutation SURVIVED — and survived correctly. It was
# not evidence that excluded identity is redundant; it was evidence that the mutation was pointed at
# the wrong mechanism. Scored as mis-targeted/equivalent, never as an authority gap.
#
# The pair this belongs to, both load-bearing and independent:
#   * `scopedOf` retains the trusted callee's scoped identity        <- THIS family
#   * `dependencyNodesOf` gives that identity a leaf node            <- root-leaf-only-trusted-exclusions
add "excluded-identity-retained" "Concrete/Proof/ProofCore.lean" "check_dependency_edges.sh" yes \
  $'      match pc.excluded.find? (fun x => x.qualName == qn) with\n      | some x => x.definitionIdentity\n      | none   => .error (.legacyNameOnly qn)' \
  $'      .error (.legacyNameOnly qn)'

# R-0004 attestation provenance. DRIFT SELECTION: the classifier matches a fixture's own header
# (`// … DRIFTED variant`) rather than its filename, so a renamed drift fixture stays classified. The
# mutation breaks the match, which lets a DRIFTED implementation be attested — the hazard that
# actually occurred when `proofFns` references resolved to `main_drifted.con`.
add "manifest-drift-classified" "scripts/gen/attestation_manifest.sh" "check_attestation_manifest.sh" no \
  $'DRIFT="$(grep -rlE \'^// .*DRIFTED variant\' examples --include=\'*.con\' 2>/dev/null | sort)"' \
  $'DRIFT="$(grep -rlE \'^// .*NO_SUCH_MARKER\' examples --include=\'*.con\' 2>/dev/null | sort)"'

# R-0004 attestation provenance. PACKAGE COLLAPSE: `contentRoot` binds module CONTENT. The mutation
# reverts it to module NAMES ONLY, which is the state that collapsed `composition` and
# `composition_trusted_helper` — two different programs — into one package identity.
# The mutation makes CONTENT contribute nothing while keeping every binding live. A first version
# deleted `srcPart` from the concatenation, which left it unused and failed the build on a lint — the
# harness correctly called that INVALID, because a mutation that cannot compile tests nothing.
add "package-identity-binds-content" "Concrete/Proof/DefinitionIdentity.lean" "check_attestation_manifest.sh" yes \
  $'    let srcPart := srcs.foldl (fun a d => a ++ "|S" ++ d) ""\n    PackageIdentity.synthetic (Concrete.shortHash ("pkgSyntheticV1:" ++ mods ++ srcPart))' \
  $'    let srcPart := srcs.foldl (fun a _ => a) ""\n    PackageIdentity.synthetic (Concrete.shortHash ("pkgSyntheticV1:" ++ mods ++ srcPart))'

# R-0004 attestation provenance. OMISSION OF THE SOURCE-LINKED POPULATION. The manifest scans fixtures
# carrying `#[proof_by]`/`#[ensures_proof]`; dropping that population leaves only whatever the other
# path reaches. Killed by the exact row/attestation denominators, which is the point of pinning them.
add "manifest-source-population" "scripts/gen/attestation_manifest.sh" "check_attestation_manifest.sh" no \
  $'done < <(grep -rlE \'#\\[(proof_by|ensures_proof)\\(\' examples --include=\'*.con\' 2>/dev/null | sort)' \
  $'done < <(grep -rlE \'#\\[(proof_by)\\(\' examples --include=\'*.con\' 2>/dev/null | grep -v ensures | sort)'

# R-0004 attestation provenance. DISCARDED SURPLUS. A table attested but referenced by no
# classification row authorises material nothing asked for. The mutation drops the check, so surplus
# stops being reported — the shape where a manifest quietly authorises more than the corpus requests.
add "manifest-surplus-refused" "scripts/gen/attestation_manifest.sh" "check_attestation_manifest.sh" no \
  $'  printf \'%s\\n\' "$KNOWN_TABLES" | grep -qx "$t" || { refuse "table \'$t\' is attested but referenced by no classification row (surplus)"; SURPLUS=$((SURPLUS+1)); }' \
  $'  printf \'%s\\n\' "$KNOWN_TABLES" | grep -qx "$t" || true'

# R-0004 attestation provenance. SILENT FILTERING of subjects that have no identity or no table. Both
# counts must stay EXPLICIT: a subject dropped from the denominator is indistinguishable from one that
# never existed, which is the laundering the typed reconciliation exists to prevent. The mutation
# stops counting untabled subjects, so the reconciliation no longer balances.
add "manifest-no-silent-filtering" "scripts/gen/attestation_manifest.sh" "check_attestation_manifest.sh" no \
  $'  if [ "$tables" = "-" ] || [ -z "$tables" ]; then UNTABLED=$((UNTABLED+1)); continue; fi' \
  $'  if [ "$tables" = "-" ] || [ -z "$tables" ]; then continue; fi'

# R-0004 attestation provenance. THE DUPLICATE PATH IS LIVE. Deleting the duplicate check could not
# kill — duplicates are 0 in this corpus, so removing a check with no live case is undetectable, the
# same vacuity that retargeted the surplus control. So the mutation makes the emitter record each
# mapping TWICE, which must be caught as a duplicate. A duplicate matters because it means one input
# row was counted twice, and every denominator built on it is then wrong.
add "manifest-duplicate-path-live" "scripts/gen/attestation_manifest.sh" "check_attestation_manifest.sh" no \
  $'    printf \'%s\\t%s\\t%s\\t%s\\t%s\\t%s\\n\' "$t" "$pkg" "$mod" "$decl" "$impl" "$src" >> "$TMP/pairs.tsv"' \
  $'    printf \'%s\\t%s\\t%s\\t%s\\t%s\\t%s\\n\' "$t" "$pkg" "$mod" "$decl" "$impl" "$src" >> "$TMP/pairs.tsv"; printf \'%s\\t%s\\t%s\\t%s\\t%s\\t%s\\n\' "$t" "$pkg" "$mod" "$decl" "$impl" "$src" >> "$TMP/pairs.tsv"'

# R-0004 attestation provenance. SAME-NAME CROSS-PACKAGE SUBSTITUTION. The conflict key is
# (table, package, module, decl). Dropping PACKAGE from it conflates same-named declarations across
# different packages — precisely the `main.validate_header` collision that motivated scoped identity —
# so distinct implementations in distinct packages register as one key with several implementations.
# Retargeted to the awk grouping. The first version altered a `cut` field set and SURVIVED, because
# the old grep-based detection went silently inert when the key changed — it found nothing and
# reported no conflicts. That survival was a finding about the CHECK, and the check was rewritten to
# group over the whole key instead of reconstructing and grepping it.
add "manifest-key-includes-package" "scripts/gen/attestation_manifest.sh" "check_attestation_manifest.sh" no \
  $'CONFLICTS="$(awk -F\'\\t\' \'{ key = $1 FS $2 FS $3 FS $4; if (!(key in seen)) { seen[key] = $5 }' \
  $'CONFLICTS="$(awk -F\'\\t\' \'{ key = $1 FS $3 FS $4; if (!(key in seen)) { seen[key] = $5 }'

# R-0004 package 2. PARTIAL CONVERSION THAT READS AS A FINISHED ONE. A table site selects generated
# references by hand, so it can select FEWER than the manifest offers — and nothing about the result
# looks unfinished: the table reports `attested`, `scopedEntryEvidence` returns rows, and the
# definitions nobody selected are simply not described. The mutation drops one of `elfFns`'s five
# attestations, which must be caught by the per-table reconciliation (manifest rows = attested +
# NAMED exclusions), not by a count that only has to be nonzero.
# ANCHORS THAT QUOTE GENERATED SYMBOL NAMES need re-anchoring whenever package identity moves,
# because the symbol name embeds it. All six of these went inert at once when the identity cascade
# renamed 38 references, and nothing objected — there was no anchor-integrity check over this corpus
# until ANCHORS_ONLY existed. That coupling is a cost of scoping DefinitionIdentity by package
# CONTENT, and it is one of the concrete arguments for the five-way identity separation: under
# PackageScopeIdentity these names would not move when an unrelated source did.
# ============================================================================
# BUG 071 — a capability declared in a DEPENDENCY must bind on its caller.
#
# Both mutations restore a form of "I have no signature for this name, so it requires
# nothing", which is the defect: a function declaring nothing called `std.env.get` and
# the program printed `$HOME`. Each must be KILLED by check_cross_package_caps.sh.
#
# The first severs the transport — Elab stops putting the requirement on the module, so
# CoreCheck has nothing to consult and falls through to the old `none` branch. The second
# keeps the transport and empties the payload, which is the subtler shape: everything
# still looks wired, and every imported call requires nothing. A gate that only checked
# "is importedFnCaps mentioned" would survive the second.
# Anchored at the CONSUMPTION point, not the production point. Emptying Elab's side
# (`importedFnCaps := importedFnCaps` -> `:= []`) leaves the `let` binding unused, which
# Lean rejects outright — the mutant does not build, and a mutant that does not build
# proves nothing about the gate. Severing it here builds cleanly and is the same defect:
# the module carries the requirements and the checker ignores them.
add "cross-package-caps-transport" "Concrete/Check/CoreCheck.lean" "check_cross_package_caps.sh" yes \
  $'    importedCaps := m.importedFnCaps' \
  $'    importedCaps := []'

# Re-anchored when bug 071 stage 2 removed the cross-package `Unsafe` exception: the line
# was `return some (dropCrossPackageUnsafe caps)` and is now `return some caps`. The
# EXPERIMENT is unchanged — keep the transport wired and empty the payload, so every
# imported call requires nothing while everything still looks connected.
add "cross-package-caps-empty-not-unknown" "Concrete/Check/CoreCheck.lean" "check_cross_package_caps.sh" yes \
  $'    match env.importedCaps.lookup name with\n    | some caps => return some caps' \
  $'    match env.importedCaps.lookup name with\n    | some _ => return some CapSet.empty'

add "attestation-conversion-complete" "Concrete/Proof/Proof.lean" "check_attestation_manifest.sh" yes \
  $'    , AttestedPFnDef.of checkDataFn       GeneratedAttestations.elfFns_0b4c54cf_check_data\n    , AttestedPFnDef.of checkMagicFn      GeneratedAttestations.elfFns_0b4c54cf_check_magic' \
  $'    , AttestedPFnDef.of checkMagicFn      GeneratedAttestations.elfFns_0b4c54cf_check_magic'

# ...and the same mutation PER CONVERTED TABLE, because the reconciliation is per table: a version
# that only reconciled the table someone happened to mutate would leave every other conversion
# unmeasured. `elfFns` above has a named exclusion, so its arithmetic is rows = attested + 1;
# `fixedCapacityFns` has none, so it is the control for the simple case rows = attested.
add "attestation-conversion-complete-fixedcapacity" "Concrete/Proof/Proof.lean" "check_attestation_manifest.sh" yes \
  $'    , AttestedPFnDef.of ringNewFn       GeneratedAttestations.fixedCapacityFns_44a81ba0_ring_new\n    , AttestedPFnDef.of ringPushFn      GeneratedAttestations.fixedCapacityFns_44a81ba0_ring_push' \
  $'    , AttestedPFnDef.of ringPushFn      GeneratedAttestations.fixedCapacityFns_44a81ba0_ring_push'

# R-0004 package 2, and the same family for the table whose manifest rows are FEWER than its entries.
# `parseValidateFns` has 8 entries and 3 rows, so a reader could mistake a dropped attestation for
# the known subject/callee shortfall. The reconciliation is against the MANIFEST, not the entry
# count, so dropping one still fails: 3 rows, 2 attested, 0 named exclusions.
# RE-ANCHORED 2026-08-20 (second attempt). The previous anchor paired `validateHeaderFieldsFn` with
# `validateVersionFn`, which are FOUR lines apart in the attested list — each line was present, so a
# line-wise check saw a match while an exact substring could never apply. Adjacent lines only.
add "attestation-conversion-complete-parsevalidate" "Concrete/Proof/Proof.lean" "check_attestation_manifest.sh" yes \
  $'    , AttestedPFnDef.of validateHeaderFieldsFn GeneratedAttestations.parseValidateFns_5b60e3b6_validate_header_fields\n    , AttestedPFnDef.of validateMsgTypeFn      GeneratedAttestations.parseValidateFns_5b60e3b6_validate_msg_type' \
  $'    , AttestedPFnDef.of validateMsgTypeFn      GeneratedAttestations.parseValidateFns_5b60e3b6_validate_msg_type'

# R-0004 package 2. A DRIFTED IMPLEMENTATION ATTESTED. This is the exclusion that is NOT a
# judgement call: `evidence_classes/stale_proof` links the same theorem while its body starts `diff`
# at 1, and the compiler reports SPEC DRIFT for it. The mutation selects that manifest row anyway —
# which is what a conversion driven mechanically off the manifest would do, since the manifest's
# header-grep drift classifier does not exclude it. Killed by the drift check, which re-derives the
# verdict from the compiler rather than from the fixture's prose.
#
# It SWAPS a legitimate reference for the drifted one rather than adding it, deliberately: adding
# would also break the row/attestation arithmetic, so the kill could come from the reconciliation
# and the drift check would never be exercised. With the swap the counts still reconcile (3 rows =
# 2 attested + 1 declared exclusion) and only the drift check can catch it — and only because it
# reads what the table site BOUND, not what the exclusion list DECLARED.
add "attestation-never-binds-drifted-impl" "Concrete/Proof/Proof.lean" "check_attestation_manifest.sh" yes \
  $'    , AttestedPFnDef.of ctCompareFn GeneratedAttestations.ctTagFns_404dc2c1_ct_compare ]' \
  $'    , AttestedPFnDef.of ctCompareFn GeneratedAttestations.ctTagFns_13c8e415_ct_compare ]'

# R-0004 package 2. A DEPENDENCY REFERENCE LEFT UNBOUND. The subject/dependency split only matters if
# an unbound reference is caught, and the earlier reconciliation could not see one: it compared
# attestations against SUBJECT rows, so dropping a dependency binding left the counts agreeing. The
# reconciliation now runs over the distinct reference set for each table, so a model the table could
# describe exactly and does not is a failure.
add "attestation-dependency-reference-bound" "Concrete/Proof/Proof.lean" "check_attestation_manifest.sh" yes \
  $'    [ AttestedPFnDef.of computeChecksumFn      GeneratedAttestations.parseValidateFns_5b60e3b6_compute_checksum\n    , AttestedPFnDef.of parseHeaderFn          GeneratedAttestations.parseValidateFns_5b60e3b6_parse_header' \
  $'    [ AttestedPFnDef.of parseHeaderFn          GeneratedAttestations.parseValidateFns_5b60e3b6_parse_header'

# R-0004 package 2. THE ENTRANCE ASSERTION MUST BE ABLE TO SAY NO. It exists to be red until the flip
# is safe, and a completion gate that cannot fail is worse than none — it converts an unchecked
# assumption into a green check. The mutation returns one table to the unattested state, which is
# precisely the condition the flip must not proceed over.
#
# THE FIRST VERSION OF THIS MUTATION SURVIVED, and the survival was a finding about the mutation
# rather than about the gate: it appended `[] |>.withAttestations` before the real list, so the table
# was attested with nothing and then attested again with everything — a no-op. The harness reported
# SURVIVED, which is the honest answer for a mutation that changed no behaviour. It now empties the
# attestation list outright, which is the actual unattested state.
add "atomic-flip-entrance-refuses-pending" "Concrete/Proof/Proof.lean" "check_atomic_flip_entrance.sh" yes \
  $'    [ AttestedPFnDef.of fcTagFn         GeneratedAttestations.fixedCapacityFns_44a81ba0_compute_tag\n    , AttestedPFnDef.of ringContainsFn  GeneratedAttestations.fixedCapacityFns_44a81ba0_ring_contains\n    , AttestedPFnDef.of ringNewFn       GeneratedAttestations.fixedCapacityFns_44a81ba0_ring_new\n    , AttestedPFnDef.of ringPushFn      GeneratedAttestations.fixedCapacityFns_44a81ba0_ring_push ]' \
  $'    []'

# ...and it must also refuse a table whose bound references are not LOAD-BEARING. Binding an
# attestation the scoped lookup cannot answer with would satisfy "pending is zero" while carrying
# nothing: the mutation attests a model the table does not hold, which `scopedEntryEvidence` refuses
# as `attestedModelNotInTable` — so the membership no longer equals the bound count.
add "atomic-flip-entrance-refuses-inert-binding" "Concrete/Proof/Proof.lean" "check_atomic_flip_entrance.sh" yes \
  $'    [ AttestedPFnDef.of checkClassFn      GeneratedAttestations.elfFns_0b4c54cf_check_class' \
  $'    [ AttestedPFnDef.of checkNonceFn      GeneratedAttestations.elfFns_0b4c54cf_check_class'


# ---------------------------------------------------------------------------
# R-0004 package 2: THE SCOPED JOIN'S FIVE ATTACK CLASSES. The flip replaced a name-keyed evidence
# join with an identity-keyed one; each mutation below removes one component of that identity or one
# of the join's refusals, and each must be caught by a control that exists on the real corpus where
# one exists, and synthetically where the corpus has no case.
# ---------------------------------------------------------------------------

# COLLISION: two programs declaring the same functions. Dropping the PACKAGE component makes
# `main_drifted`'s edges match `elfFns` again — the exact substitution the flip closed, measured on
# a real fixture rather than argued.
add "scoped-identity-compares-package" "Concrete/Proof/DefinitionIdentity.lean" "check_dependency_edges.sh" yes \
  $'  a.packageIdentity == b.packageIdentity\n    && a.moduleIdentity == b.moduleIdentity' \
  $'  a.moduleIdentity == b.moduleIdentity'

# SUBSTITUTION: the same declaration in the same package with a DIFFERENT body. The corpus has no
# such pair — that would be one program compiled twice — so the control is synthetic, and dropping
# the implementation component must still be caught.
add "scoped-identity-compares-implementation" "Concrete/Proof/DefinitionIdentity.lean" "check_dependency_edges.sh" yes \
  $'    && a.declarationIdentity == b.declarationIdentity\n    && a.implementationIdentity == b.implementationIdentity' \
  $'    && a.declarationIdentity == b.declarationIdentity'

# OMISSION: an edge the compiler HAS whose callee cannot be keyed. Dropping it before the join leaves
# every result set empty while the closure covers less than was asked — fail-open, and invisible
# without a control, since the corpus currently has no unkeyable edge.
add "scoped-join-carries-unkeyable-edges" "Concrete/Proof/Correspondence.lean" "check_dependency_edges.sh" yes \
  $'  let unscopedRefusals := i.unscopedEdges.map (fun (c, k, w) => WitnessRefusal.unscopedCallee c k w)' \
  $'  let unscopedRefusals : List WitnessRefusal := []'

# MISATTACHMENT: a theorem whose table does not hold the callee's definition. Making membership
# answer `true` regardless restores `proof_pressure`'s false correspondence AND `main_drifted`'s.
#
# THE FIRST VERSION OF THIS MUTATION SURVIVED, and the survival was a finding about the CORPUS: it
# made an UNREADABLE table justify its edges, and every table this corpus names is now readable, so
# the branch it flipped has no live case. That is why the unreadable-table refusal is asserted
# directly by a probe instead — a branch with no corpus case needs a control, not a mutation that
# silently tests nothing.
add "scoped-join-membership-answers-identity" "Concrete/Proof/DependencyEdge.lean" "check_dependency_edges.sh" yes \
  $'def scopedEvidenceContains (rows : List ScopedEntryEvidence) (d : DefinitionIdentity) : Bool :=\n  rows.any (fun r => r.definition.sameDefinition d)' \
  $'def scopedEvidenceContains (rows : List ScopedEntryEvidence) (d : DefinitionIdentity) : Bool :=\n  let _ := d\n  !rows.isEmpty'


# AUTHORITY: a friendly verdict must survive its dependency justification. The mutation makes the
# authority pass accept every subject, which is the state the corpus was in before it was wired —
# and `main_drifted`'s cross-program closure reports `proved` again.
add "authority-consumes-correspondence" "Concrete/Proof/ProofCore.lean" "check_dependency_edges.sh" yes \
  $'      if i.requestedEdges.isEmpty && i.unscopedEdges.isEmpty then true\n      else (Proof.correspond i).usable i.requestedEdges.length' \
  $'      if i.requestedEdges.isEmpty && i.unscopedEdges.isEmpty then true\n      else true'


# R-0004 package 2. THE LEAF-BOUNDARY RULE. Only a TRUSTED exclusion may be a dependency-node leaf:
# an ineligible callee is unprovable, and a closure over it must refuse rather than serialize. The
# mutation removes the filter, so every excluded definition becomes a leaf again — killed by the
# probe that CALLS `dependencyNodesOf` with one trusted and one ineligible exclusion and reads the
# node set. An earlier probe asserted two facts about `isCurrentForDependents` instead and never
# touched the node builder, so this mutation would have survived it.
add "root-leaf-only-trusted-exclusions" "Concrete/Proof/ProofCore.lean" "check_dependency_edges.sh" yes \
  $'    if !x.eligibility.isTrusted then none else' \
  $'    if false then none else'


# R-0004 package 2. THE PROVED-ROOTS INVARIANT MUST BE LOAD-BEARING. It is expected to hold vacuously
# on this corpus — every subject whose root refuses is already not `proved` — so the only way to know
# it works is to remove it and watch a control go red. The mutation drops it from the violation list
# that `selfCheck` returns, which is exactly how it would be lost in a refactor.
#
# NEUTRALIZED, NOT DELETED. Deleting the operand fails to compile (the record literals take their
# expected type from this concatenation) and deleting the binding fails an unused-binding lint — the
# harness called both INVALID, correctly, since a mutation that cannot compile tests nothing.
# `.take 0` keeps every binding live and every type inferable while making the invariant report
# nothing, which is precisely the behaviour a silent regression would have.
# ---------------------------------------------------------------------------
# ADMISSION MUST NOT RE-COUPLE TO MAINTENANCE (R-0484, 2026-09-26). Two mutations, one per
# direction of the recoupling that kept the effect-opacity rule inert for a release.

# 1. REPLAY TARGETS CHOSEN BY ADMISSION. This is the original defect exactly: obligation
# status derived from a predicate merely NAMED `eligible` that was really `admissible`, so
# refusing admission also withdrew the obligation and silently removed the function from
# replay and drift coverage. An effect reclassification must never delete evidence from
# maintenance. Killed by `check_effect_opacity.sh` (drift coverage falls 11 -> 8) and by
# `check_purecore_proofs.sh`, whose pending claims stop being asked about.
add "admission-does-not-select-replay-targets" "Concrete/Proof/ProofCore.lean" "check_effect_opacity.sh" yes \
  $'    let status := deriveObligationStatus e.eligibility.eligible\n        e.eligibility.isTrusted extracted specDrifted e.spec e.fingerprint e.subjectDigest' \
  $'    let status := deriveObligationStatus e.eligibility.admissible\n        e.eligibility.isTrusted extracted specDrifted e.spec e.fingerprint e.subjectDigest'

# 2. ADMISSION NOT RENDERED ON `proved`. The first implementation appended the admission
# line inside each status arm and skipped exactly one — `proved` — which is the single
# state where an unreported refusal reads as a program-level guarantee rather than a
# pending item. Restricting the wrapper to the non-proved states reproduces it. Killed by
# `check_effect_opacity.sh`'s dispatch-site assertion.
add "admission-rendered-on-proved-too" "Concrete/Report/Report.lean" "check_effect_opacity.sh" yes \
  $'  renderProofStatusBody e sourceMap ++ admissionLine e' \
  $'  renderProofStatusBody e sourceMap ++ (match e.state with | ProofState.proved => "" | _ => admissionLine e)'

add "proved-roots-invariant-reported" "Concrete/Proof/ProofCore.lean" "check_dependency_edges.sh" yes \
  $'  oblKnown ++ oblStatus ++ provedRoots ++ provedExtracted ++ provedFp ++ staleFp' \
  $'  oblKnown ++ oblStatus ++ (provedRoots.take 0) ++ provedExtracted ++ provedFp ++ staleFp'


# R-0004 package 2. TRUST MUST TRAVEL THE WHOLE CLOSURE. The mutation narrows `trustedOf` from the
# transitive closure to DIRECT callees, which is the shape that looks right and silently drops every
# boundary more than one hop away. Killed by `composition_deep_trust`, where `outer` reaches the
# trusted leaf through `middle` and mentions no trusted function itself.
add "trust-propagates-transitively" "Concrete/Proof/ProofCore.lean" "check_dependency_edges.sh" yes \
  $'    (reachableFrom self).filter fun c =>\n      c != self && (match statusOf c with\n                    | some .trusted => true\n                    | _ => false)' \
  $'    (directCalleesOf self).filter fun c =>\n      c != self && (match statusOf c with\n                    | some .trusted => true\n                    | _ => false)'

# ---------------------------------------------------------------------------
# R-0004 package 3: the kernel-replay producer. Each family below targets a boundary where the
# producer's answer stops being usable as evidence and becomes merely output.

# THE FLAG THAT WAS ALREADY DEAD ONCE. The inline version computed `generalFailure` AFTER filling
# `failed` with every target, so it was unreachably false and a file that did not compile was
# reported as every theorem individually "not found". Pinning it: if the whole-file failure stops
# being detected, every target silently becomes an individual verdict again.
# Written as an OPERAND change rather than a deletion: `let generalFailure := false` cannot compile,
# because it strands `named` on an unused-binding lint, and a mutation that cannot build is INVALID
# rather than killed. Inverting the exit-code test reproduces the old broken behaviour directly — a
# file that failed to compile is judged theorem by theorem, and since it named none of them, every
# target reads as accepted.
add "replay-general-failure-detected" "Concrete/Proof/Replay.lean" "check_replay_producer.sh" yes \
  'let generalFailure := result.exitCode != 0 && named.isEmpty' \
  'let generalFailure := result.exitCode == 0 && named.isEmpty'

# AN EMPTY REQUEST MUST REFUSE. `List.all` is true over an empty list, so dropping this guard hands
# a minting authority a vacuous "everything was accepted" — the single most dangerous answer this
# producer can give.
add "replay-empty-request-refused" "Concrete/Proof/Replay.lean" "check_replay_producer.sh" yes \
  'if req.targets.isEmpty then throw .noTargets' \
  'if false then throw .noTargets'

# ACCEPTED-BUT-UNBOUND IS NOT ACCEPTED. Collapsing the two is exactly the summing that once read
# "11 verified" on a program with 11 unbound claims.
add "replay-unbound-not-accepted" "Concrete/Proof/Replay.lean" "check_replay_producer.sh" yes \
  $'      else match t.binding with\n        | .unbound => .acceptedUnbound\n        | .bound   => .accepted' \
  $'      else match t.binding with\n        | .unbound => .accepted\n        | .bound   => .accepted'

# WHAT MINTING MAY ACCEPT. `fullyBound` is the predicate a receipt authority asks: every target
# accepted AND bound. Widening it to admit `acceptedUnbound` is the exact laundering this producer
# exists to prevent — a claim with no stored subject digest would become mintable.
#
# NOT A FAMILY, recorded so its absence is not mistaken for an oversight: dropping the
# `!r.generalFailure` conjunct from `allAccepted` SURVIVES, and correctly. A general failure assigns
# every target `notAttempted`, which already fails the `accepted || acceptedUnbound` test, so the
# conjunct is defence-in-depth against a future change in verdict assignment rather than a live
# boundary. It cannot be mutation-killed today and a family claiming otherwise would be false.
add "replay-mintable-requires-bound" "Concrete/Proof/Replay.lean" "check_replay_producer.sh" yes \
  '  !r.generalFailure && r.checks.all (·.verdict == .accepted)' \
  '  !r.generalFailure && r.checks.all (fun c => c.verdict == .accepted || c.verdict == .acceptedUnbound)'

# AN UNNAMED TARGET WOULD REPORT AS ACCEPTED WITHOUT BEING CHECKED: the empty needle matches the
# transcript everywhere. A false pass is the worst failure mode available to this producer.
add "replay-unnamed-target-refused" "Concrete/Proof/Replay.lean" "check_replay_producer.sh" yes \
  'if t.subject.trimAscii.isEmpty || t.theoremName.trimAscii.isEmpty then' \
  'if false then'

# ---------------------------------------------------------------------------
# R-0004 package 3: minting authority. `unchecked facts -> receipt` is prevented by a construction
# chain, not by a runtime check, so most of that property is enforced at compile time and cannot be
# mutation-tested at all — a mutation opening the chain simply stops building, and a build failure
# IS the correct kill for a compile-time lock. What CAN be mutated is the extraction step: the
# conditions under which a completed run is allowed to witness one theorem's acceptance.

# IGNORING A REPLAY FAILURE. Dropping the verdict test hands out a token for a REJECTED theorem —
# the receipt would record that the kernel accepted a proof it had just refused.
add "mint-token-requires-acceptance" "Concrete/Proof/Replay.lean" "check_replay_producer.sh" yes \
  'if c.verdict != .accepted then throw (.notAccepted theoremName c.verdict)' \
  'if false then throw (.notAccepted theoremName c.verdict)'

# TREATING AN INTERRUPTED REPLAY AS SUCCESS. Under a general failure no verdict means anything;
# without this guard a file that never compiled would mint for every theorem it named.
add "mint-token-refuses-interrupted-run" "Concrete/Proof/Replay.lean" "check_replay_producer.sh" yes \
  'if r.generalFailure then throw .underGeneralFailure' \
  'if false then throw .underGeneralFailure'

# SILENCE READ AS ACCEPTANCE. A theorem absent from the run has no verdict, and defaulting an
# absent check to acceptance is how an unreplayed artifact acquires a receipt.
add "mint-token-refuses-unreplayed" "Concrete/Proof/Replay.lean" "check_replay_producer.sh" yes \
  $'  let some c := r.checks.find? (·.target.theoremName == theoremName)\n    | throw (.notReplayed theoremName)' \
  $'  let c := (r.checks.find? (·.target.theoremName == theoremName)).getD\n    { target := { subject := "", theoremName := theoremName, kind := .refinement\n                , origin := .hardcoded, binding := .bound }, verdict := .accepted }'

# LOCATION-DEPENDENT EVIDENCE. A workspace resolved from the caller's directory may replay and may
# not mint: a receipt must be re-checkable from the artifact alone.
add "mint-token-refuses-fallback-workspace" "Concrete/Proof/Replay.lean" "check_replay_producer.sh" yes \
  $'  if !r.environment.workspaceFromInput then\n    throw (.fallbackWorkspace r.environment.workspace)' \
  $'  if false then\n    throw (.fallbackWorkspace r.environment.workspace)'

# THE ARTIFACT IS THE REPLAYED THEOREM, not something the caller supplies. Sourcing it from the
# material instead of the token restores exactly the hole the token was introduced to close.
add "mint-artifact-comes-from-token" "Concrete/Proof/Receipt.lean" "check_dependency_edges.sh" yes \
  '  , theoremArtifact := sr.theoremName' \
  '  , theoremArtifact := m.dependencyRoot'

# R-0004 package 3: production receipt issuance.

# THE GATE THAT CAUGHT A REAL BUG THE DAY ISSUANCE LANDED. Without it, `elf_header_drifted` — a
# DIFFERENT program sharing every declaration name with `elf_header` — received four receipts, two
# for `stale` claims whose bodies had changed since their proofs were linked. The token says the
# kernel accepted a THEOREM; it says nothing about whether that theorem still proves THIS body.
# SPLIT IN TWO for the same reason as entry-body-recomputed above: this text appears in BOTH
# `issueFor` (which mints a receipt) and `freshFactsFor` (which produces fresh facts). Both must
# refuse a subject that is not proved, and a single ambiguous anchor tested neither.
#
# THE TWO HALVES NAME DIFFERENT GATES, and getting that wrong makes the kill meaningless. My first
# version pointed both at check_receipt_issuance.sh, but that gate only drives `--report receipts`,
# whose producer is `issueFor`. `freshFactsFor` is reached ONLY from `storedDispositionFor`
# (Issue.lean:315), which is receipt CONSUMPTION — so an issuance-gate kill for the freshFactsFor
# mutations would have been evidence about a path the mutation never touched.
add "issue-requires-proved-status" "Concrete/Proof/Issue.lean" "check_receipt_issuance.sh" yes \
  $'  if obl.status != .proved then throw (.notProved subject obl.status.canonical)\n  if res.environment.importDigests.isEmpty then throw (.noImportClosure subject)' \
  $'  if false then throw (.notProved subject obl.status.canonical)\n  if res.environment.importDigests.isEmpty then throw (.noImportClosure subject)'
add "freshfacts-requires-proved-status" "Concrete/Proof/Issue.lean" "check_receipt_consumption.sh" yes \
  $'  if obl.status != .proved then throw (.notProved subject obl.status.canonical)\n  let row ← (validatedRowOf thm).mapError (IssueRefusal.classification thm)' \
  $'  if false then throw (.notProved subject obl.status.canonical)\n  let row ← (validatedRowOf thm).mapError (IssueRefusal.classification thm)'

# TRUST MUST NOT BE DROPPED ON THE WAY INTO THE RECEIPT. Issuing with no boundaries named while the
# closure crosses one turns a qualified claim into an unqualified one — the exact laundering the
# qualification exists to prevent.
# Written as an operand change: emptying the binding outright strands `trustedDepsOf` on an
# unused-binding lint, and a mutation that cannot build is INVALID rather than killed.
# SPLIT IN TWO — `issueFor` and `freshFactsFor` each read the trusted boundaries from the same
# producer, and each must carry them. One ambiguous anchor covered neither.
add "issue-carries-trusted-boundaries" "Concrete/Proof/Issue.lean" "check_receipt_issuance.sh" yes \
  $'  let boundaries := trustedDepsOf subject\n  -- The root\'s own trust verdict and the named boundaries must AGREE.' \
  $'  let boundaries := (trustedDepsOf subject).take 0\n  -- The root\'s own trust verdict and the named boundaries must AGREE.'
add "freshfacts-carries-trusted-boundaries" "Concrete/Proof/Issue.lean" "check_receipt_consumption.sh" yes \
  $'  let boundaries := trustedDepsOf subject\n  if rootMat.carriesTrust != !boundaries.isEmpty then throw (.materialRefused subject)' \
  $'  let boundaries := (trustedDepsOf subject).take 0\n  if rootMat.carriesTrust != !boundaries.isEmpty then throw (.materialRefused subject)'

# R-0004 package 3: receipt storage and consumption.

# THE BUG THE CORPUS CAUGHT AND THE UNIT PROBE MISSED. `s!"{n}"` renders a `Name` in display form,
# so a component containing dots came out as «Concrete.Proof.elfFns» and decoded back WITH the
# guillemets — every receipt disagreed with itself the instant it was written. The round-trip probe
# used table `A`, too simple to expose it.
add "receipt-encode-name-unescaped" "Concrete/Proof/Receipt.lean" "check_receipt_consumption.sh" yes \
  '    ++ (r.tableBindings.map fun (n, d) => s!"table {n.toString false} {d}")' \
  '    ++ (r.tableBindings.map fun (n, d) => s!"table {n} {d}")'

# PARTIAL DECODING IS THE ATTACK. A decoder that accepts a record with a missing field produces a
# receipt whose defaulted value compares equal to another default, so two unrelated claims agree.
add "receipt-decode-refuses-partial" "Concrete/Proof/Receipt.lean" "check_receipt_consumption.sh" yes \
  $'    | none => Except.error (ReceiptDecodeRefusal.missingField n)\n    | some v => if v.isEmpty then Except.error (.malformedField n v) else Except.ok v' \
  $'    | none => Except.ok ""\n    | some v => if v.isEmpty then Except.error (.malformedField n v) else Except.ok v'

# SKIPPING AN UNKNOWN KEY makes a newer format — or a smuggled field — look like a clean parse.
add "receipt-decode-refuses-unknown-key" "Concrete/Proof/Receipt.lean" "check_receipt_consumption.sh" yes \
  '      else throw (.unknownKey key)' \
  '      else pure ()'

# SCHEMA IS CHECKED BEFORE CONTENT, so an older envelope reads as a different format rather than as
# a program that moved.
add "receipt-decode-checks-schema-first" "Concrete/Proof/Receipt.lean" "check_receipt_consumption.sh" yes \
  '  if schema != receiptSchemaVersion then throw (.schemaUnreadable schema)' \
  '  if false then throw (.schemaUnreadable schema)'

# THE REPORT HANDLERS MUST RETURN. attestation-join, generated-implementations and impl-manifest
# each printed their report and then fell through to the "Unknown report type" branch, exiting 1 on
# correct output. Nothing asserted a report's exit code, so it went unnoticed until a harness started
# rejecting nonzero exits. Removing the restored `return 0` reproduces the defect exactly, and
# check_cli_contract.sh must turn red for it.
add "report-handler-returns" "Main.lean" "check_cli_contract.sh" yes \
$'      IO.println (Report.generatedImplementationsReport (pc := pc))\n      return 0' \
$'      IO.println (Report.generatedImplementationsReport (pc := pc))'

# ONLY A SUPERVISOR MAY PUBLISH QUALIFICATION, and its refusals must be load-bearing rather than
# decorative. Disabling the child-exit refusal makes the supervisor accept a child that died — the
# exact case the boundary exists for, since reconciliation used to happen inside the run and a
# process cannot judge its own exit. check_campaign_supervisor.sh must turn red for it.
add "supervisor-child-exit-refusal" "scripts/tests/lib/campaign_supervise.sh" "check_campaign_supervisor.sh" no \
$'  [ "$rc" = "0" ] || out="$out child_exit($rc)"' \
$'  [ "$rc" = "0" ] || out="$out"'

# H-5: THE SUPERVISOR'S WIRING, NOT ONLY ITS DECISIONS.
#
# Each of these keeps the decision function intact and severs the ONE line that consults it. That is
# the failure these exist to catch: a helper suite at 169/0 over decisions the production path had
# quietly stopped asking. Every one was measured pristine-green then mutated-red on a disposable
# copy, failing the control that names its own call site, before being written down here.

# A DECISION NOTHING CONSULTS IS NOT A CONTROL. This keeps the call to candidate_provenance and
# DISCARDS its answer, so every helper-level control stays green while the supervisor stops acting
# on identity refusals entirely — the exact shape that made H-5 necessary. Measured: it also takes
# the cross-field workspace comparison with it, since both refusals travel this one call site.
add "supervisor-asks-provenance" "scripts/tests/lib/campaign_supervise.sh" "check_campaign_supervisor.sh" no \
  '_sup_refusals="$_sup_refusals$(candidate_provenance "$_cand" \' \
  '_wiring_discarded="$(candidate_provenance "$_cand" \'

# THE CANDIDATE MUST BE THIS RUN'S. Discarding the binding lets a stale candidate from an earlier run
# answer for a child that just exited cleanly.
add "supervisor-asks-run-binding" "scripts/tests/lib/campaign_supervise.sh" "check_campaign_supervisor.sh" no \
  '_sup_refusals="$_sup_refusals$(candidate_run_binding "$_cand" "$RUN_ID" "$_sup_head1")"' \
  '_wiring_discarded="$(candidate_run_binding "$_cand" "$RUN_ID" "$_sup_head1")"'

# INCOHERENCE MUST REACH THE REFUSALS. The check still runs and its result is still computed; it just
# stops being appended, so a record that contradicts itself publishes.
add "supervisor-asks-incoherence" "scripts/tests/lib/campaign_supervise.sh" "check_campaign_supervisor.sh" no \
  '[ -z "$_cand_incoh" ] || _sup_refusals="$_sup_refusals candidate_incoherent($_cand_incoh)"' \
  '[ -z "$_cand_incoh" ] || _wiring_discarded="candidate_incoherent($_cand_incoh)"'

# PUBLICATION WHILE WORK SURVIVES. Short-circuiting the guard lets the supervisor install an
# authoritative record while a process that can still modify the evidence is running.
add "supervisor-asks-group-state" "scripts/tests/lib/campaign_supervise.sh" "check_campaign_supervisor.sh" no \
  'if ! group_state_permits_publication "$_group_state"; then' \
  'if false && ! group_state_permits_publication "$_group_state"; then'

# THE LAUNCHER'S ANSWER MUST BE READ. The decoder still runs; its verdict is thrown away, so a report
# naming a different run is accepted as this run's.
add "supervisor-asks-launch-report" "scripts/tests/lib/campaign_supervise.sh" "check_campaign_supervisor.sh" no \
  '_launch_bad="$(decode_launch_report "$_launch_report" "$RUN_ID" "$_launch_rc")"' \
  '_launch_bad=""; _wiring_discarded="$(decode_launch_report "$_launch_report" "$RUN_ID" "$_launch_rc")"'

# EVIDENCE CHANGED AFTER THE CENSUS MUST REFUSE. The digests are still computed and compared; the
# refusal is simply not recorded, so post-census tampering publishes.
add "supervisor-asks-evidence-root" "scripts/tests/lib/campaign_supervise.sh" "check_campaign_supervisor.sh" no \
  '_sup_refusals="$_sup_refusals evidence_changed_after_census($_cand_root->$_sup_root)"' \
  '_wiring_discarded="evidence_changed_after_census($_cand_root->$_sup_root)"'

# THE ROOT CONJUNCT MUST BE CONSUMED, NOT MERELY CALLED. This mutation keeps the call to
# dependencyRootMaterial and DISCARDS its result, so every lexical control — including the pinned
# call-site count — still sees two live call sites while `rooted e` goes inert. Measured before the
# behavioural probes existed: the count stayed 2 and saw nothing. check_dependency_edges.sh must turn
# red for it, which is what makes those probes evidence of consumption rather than of presence.
add "authority-consumes-root" "Concrete/Proof/ProofCore.lean" "check_dependency_edges.sh" yes \
$'    | .ok d    => (Proof.dependencyRootMaterial (dependencyNodesOf pc graph) d).toOption.isSome' \
$'    | .ok d    => let _ := (Proof.dependencyRootMaterial (dependencyNodesOf pc graph) d).toOption.isSome; true'

# A MISSING PROBE MUST NOT COUNT AS A PASS. This turns the batch's missing-result refusal into an
# `ok`, which is precisely the silent-shrink the batching work had to rule out: a probe that never
# reported would be indistinguishable from one that passed. Its declared gate is
# check_mint_batch_accounting.sh, which is the only thing that exercises that path.
add "mint-missing-result-refusal" "scripts/tests/check_dependency_edges.sh" "check_mint_missing_result.sh" no \
$'      mint_no "${MINT_LABEL[$i]} — MISSING from group \'$grp\' (no <<P$id>> result)"' \
$'      mint_ok "${MINT_LABEL[$i]} — MISSING from group \'$grp\' (no <<P$id>> result)"'

N=${#NAME[@]}

# NO NAME MAY APPEAR TWICE, WHETHER OR NOT ANYONE SELECTS BY IT. Selection took the first match and
# said nothing, so a duplicated family name silently made one of the two unreachable by identity and
# made every evidence record keyed by that name ambiguous. Identity that is not unique is not
# identity; this is checked over the whole inventory rather than only on the selection path.
# THE UNDERSCORE PREFIX IS RESERVED, AND THAT IS ENFORCED HERE RATHER THAN ASSUMED.
# The supervisor skips evidence directories beginning with `_` because the run-level baseline
# transcript lives in `_baseline` and belongs to the run, not to any family. If a family could be
# named `_something`, that skip would silently exempt it from the evidence reconciliation — the
# exclusion would become a hole. It is a hole only if the reservation is unchecked.
_reserved_names="$(printf '%s\n' "${NAME[@]}" | grep '^_' || true)"
if [ -n "$_reserved_names" ]; then
  echo "FATAL: family names may not begin with '_' (reserved for run-level evidence):" >&2
  printf '       %s\n' $_reserved_names >&2
  _gate_lock_release; exit 2
fi

_dupe_names="$(printf '%s\n' "${NAME[@]}" | LC_ALL=C sort | uniq -d)"
if [ -n "$_dupe_names" ]; then
  echo "FATAL: the family inventory contains duplicate names:" >&2
  printf '       %s\n' $_dupe_names >&2
  echo "       Records and FAMILY_ID selection are both keyed by name and cannot disambiguate them." >&2
  _gate_lock_release; exit 2
fi

# `--spec <FAMILY_ID>`: print the mutation-spec digest of one family and exit.
#
# Callers that pin a spec need its value, and the only sound way to obtain one is to ask the producer
# that will later check it. Deriving the digest a second time by hand — in a gate, a script or a
# commit message — is how a pin comes to describe something the driver never computes. This runs
# before the workspace copy, so it costs nothing.
if [ "${1:-}" = "--spec" ]; then
  [ -n "${2:-}" ] || { echo "usage: --spec <FAMILY_ID>" >&2; exit 2; }
  for (( _i=0; _i<N; _i++ )); do
    [ "${NAME[$_i]}" = "$2" ] || continue
    # THE PRODUCER'S STATUS AND ITS OUTPUT ARE BOTH CHECKED. `exit 0` sat directly beneath the
    # call, so with no sha256 implementation — or a hasher that succeeded and printed nothing —
    # this printed an empty line and reported success. A caller pinning that value would pin the
    # empty string, and the pin would then match any family whose digest also failed to compute.
    _spec_out="$(family_spec_digest "${NAME[$_i]}" "${FILE[$_i]}" "${GATE[$_i]}" \
                                    "${BUILD[$_i]}" "${OLD[$_i]}" "${NEW[$_i]}")" || {
      echo "FATAL: the mutation-spec digest producer failed for '$2'" >&2; exit 2; }
    # A DIGEST IS SIXTY-FOUR HEX CHARACTERS AND NOTHING ELSE. The first version of this check was
    # `[0-9a-f][0-9a-f]*`, which is "two hex characters followed by ANYTHING" — it accepted
    # `abNOT_A_DIGEST`, and `af/../../x`, and any truncation. Length and alphabet are both checked.
    case "${#_spec_out}:$_spec_out" in
      64:*[!0-9a-f]*) echo "FATAL: the spec digest for '$2' is not hex: '$_spec_out'" >&2; exit 2 ;;
      64:*) printf '%s\n' "$_spec_out"; exit 0 ;;
      *) echo "FATAL: the spec digest for '$2' is ${#_spec_out} characters, not 64: '$_spec_out'" >&2
         exit 2 ;;
    esac
  done
  echo "no family named '$2' in this inventory of $N" >&2; exit 2
fi


# VACUITY FLOOR, APPLIED TO EVERY MODE. `N` comes straight from the inventory array, so an inventory
# that lost its entries — a renamed array, a truncated edit, a botched merge — would run zero
# families, satisfy `VERDICTS == EXPECTED_RUN` as 0 == 0, and report `PASS=0 FAIL=0` with
# `completed=1`: a campaign that proved nothing, recorded as an authoritative success. An earlier
# version of this floor guarded only ANCHORS_ONLY, which left the mode that actually issues completion
# records unprotected. The floor sits far below the current 81 so it catches collapse, not growth.
# THE COUNT IS PINNED, NOT FLOORED. A floor of 50 accepted the deletion of 31 of the 81 families
# while still reporting a "full" campaign: `EXPECTED_RUN` is derived from whatever `N` happens to be,
# so a reduced corpus passes as complete. Retiring a mutation is a deliberate act and must be recorded
# in the same commit as the removal, exactly like the identity freezes.
if [ "$N" != "$EXPECTED_FAMILIES" ]; then
  echo "FATAL: the mutation inventory holds $N families, pinned at $EXPECTED_FAMILIES." >&2
  echo "       Mutations are the evidence that gates are load-bearing, so losing some silently" >&2
  echo "       withdraws that evidence. If this change is intended, update EXPECTED_FAMILIES in the" >&2
  echo "       SAME commit and say in the message which families moved and why." >&2
  echo "       The 'add' inventory was probably renamed or truncated. Refusing to report a verdict." >&2
  # INVALIDATE HERE TOO. This exit happens BEFORE the artifact producer is defined, so without this a
  # prior `completed=1` stayed readable while the run that should have discredited it refused. Written
  # inline and atomically for the same reason the producer is: a truncate-in-place can be interrupted.
  if [ "${ANCHORS_ONLY:-0}" != "1" ]; then
    # THE SAME TARGET THE RUN OWNS. Hard-coding the full artifact here meant a `FAMILY=n` invocation
    # against a temporarily changed inventory destroyed the authoritative full-campaign record — the
    # identical defect fixed on the normal early path, reintroduced by a second producer.
    _vac_target="$ROOT_DIR/.mutation-campaign-summary"
    [ "${CAMPAIGN_PARTIAL:-0}" = "0" ] || _vac_target="$_vac_target.partial"
    _t="$(mktemp "$_vac_target.XXXXXX" 2>/dev/null)" \
      && printf 'completed=0\nfamilies_declared=%s\nrefusals= mutation_inventory_pin_violated(%s)\n' "$N" "$N" > "$_t" \
      && mv -f "$_t" "$_vac_target"
  fi
  _gate_lock_release 2>/dev/null || true
  exit 2
fi
PASS=0; FAIL=0

# ---------------------------------------------------------------------------
# START-STATE CAPTURE for campaign integrity.
#
# A campaign's verdicts are only about the tree it measured. The interrupted run of 2026-08-20
# printed 78 verdicts and stopped without a summary, and nothing distinguished that from a finished
# run except a human noticing the missing line. These dimensions are recorded now, recomputed at the
# end, and any difference NAMES itself and suppresses the completion record.
# DELEGATED TO THE SHARED LIBRARY, so the supervisor computes these with the SAME code the child
# does. They used to be defined only here, which is why the supervisor could not check them and
# published the child's word for which driver and inventory had run.
_d(){ ts_digest; }
snap_driver()    { ts_driver_digest "$ROOT_DIR"; }
snap_inventory() { ts_inventory_digest "$ROOT_DIR"; }
snap_head()      { ts_head "$ROOT_DIR"; }
# TREE STATE COMES FROM THE SHARED LIBRARY, not from a second implementation here. The runner had its
# own copy of these and the two disagreed after each was fixed separately; see lib/treestate.sh.
# shellcheck source=scripts/tests/lib/treestate.sh
# PROVE IT LOADED. Sourcing failure is not caught by `set -uo pipefail`, and there is no `set -e`, so
# a missing library would leave every ts_* call as "command not found" — and start/end comparisons of
# two empty strings ACCEPT. This is a live risk for a file that is new in this change: omit it from
# the commit and the campaign reconciles nothing while reporting completed=1.
. "$ROOT_DIR/scripts/tests/lib/treestate.sh" 2>/dev/null || true
ts_require || { _gate_lock_release 2>/dev/null || true; exit 2; }
snap_tracked()   { ts_tracked "$ROOT_DIR"; }
snap_untracked() { ts_untracked "$ROOT_DIR"; }
# Mutation targets are judged CLEAN, not merely unchanged: a mutation already present when the run
# STARTS is invisible to a start-vs-end comparison, and that is exactly what a killed harness leaves.
snap_dirty_targets() {
  # shellcheck disable=SC2086
  ts_dirty_files "$ROOT_DIR" $(printf '%s\n' "${FILE[@]}" | LC_ALL=C sort -u)
}
START_DRIVER="$(snap_driver)";    START_INV="$(snap_inventory)"
START_HEAD="$(snap_head)";        START_TRACKED="$(snap_tracked)"
START_UNTRACKED="$(snap_untracked)"; START_DIRTY="$(snap_dirty_targets)"
FAMILIES_RUN=0

# THE SHA OF THE BYTES ACTUALLY EXECUTED. The launcher hashes the driver before `exec`ing the
# snapshot and passes it in; before this it computed that value and NOTHING READ IT, while the
# artifact's `driver_sha` was a hash of the LIVE repository file taken later. An edit landing between
# snapshot creation and that later capture produced a run executing old bytes while the artifact
# recorded the new ones, with no mismatch anywhere. The executed bytes are the authority.
EXECUTED_DRIVER_SHA="${CONCRETE_MUT_DRIVER_SHA:-unknown}"
# The preamble that took the lock and discredited the old artifact must be the SAME file the snapshot
# captured. If it is not, two versions of this driver contributed to one run.
PREAMBLE_DRIVER_SHA="${CONCRETE_MUT_PREAMBLE_SHA:-unknown}"

# ONE producer of the completion artifact, used by every exit path. Two producers would let one
# path emit a shape the consumer does not check.
#   write_summary <completed> <refusals>   -> nonzero if the artifact could not be written
CAMPAIGN_ARTIFACT="$ROOT_DIR/.mutation-campaign-summary"
write_summary() {
  # THE ANCHOR GATE MUST NEVER TOUCH THE CAMPAIGN RECORD. `check_mutation_anchors.sh` invokes this
  # driver with ANCHORS_ONLY=1, and it is registered in CI, so with an unguarded invalidation a cheap
  # anchor check OVERWROTE an authoritative `completed=1` campaign artifact with a record for a
  # campaign that was never attempted. An anchor check is not a campaign and has nothing to say here.
  if [ "${ANCHORS_ONLY:-0}" = "1" ]; then return 0; fi
  # A PARTIAL RUN WRITES ITS OWN FILE. `FAMILY=n` is a deliberate sample — and the registered
  # phase6c gate runs two of them — but each one overwrote the authoritative full-campaign record
  # with a one-family partial. The full record is evidence about all 81 families and a sample cannot
  # be allowed to destroy it; the sample still gets a durable artifact, under its own name.
  local target="$CAMPAIGN_ARTIFACT"
  [ "${CAMPAIGN_PARTIAL:-0}" = "0" ] || target="$CAMPAIGN_ARTIFACT.partial"
  # PUBLISHED FOR THE CALLER, because `target` is a local and the reporting below runs outside this
  # function. Reading a function-local from its caller is unbound under `set -u`, which killed a run
  # whose work had already completed — the child died after its verdict and the supervisor correctly
  # refused on child_exit(1).
  WROTE_ARTIFACT_PATH=""
  # UNDER SUPERVISION THE CHILD MAY ONLY PROPOSE. The authoritative name is installed by the
  # supervisor after it has observed this process exit and re-reconciled the tree.
  [ "${CONCRETE_MUT_ROLE:-supervisor}" != "child" ] || target="$CAMPAIGN_ARTIFACT.candidate"
  # ATOMIC. A truncate-in-place write can be interrupted, leaving a half-written artifact that parses
  # as a valid record with missing fields. Write beside it, then rename.
  # mktemp, not a predictable "$file.$$.tmp": a pre-existing symlink at a guessable path would
  # have the redirection below write THROUGH it and then be installed as the authority artifact.
  local tmp
  tmp="$(mktemp "$target.XXXXXX" 2>/dev/null)" || { echo "error: could not create a temp artifact" >&2; return 1; }
  if ! { echo "completed=$1"
         # MODE IS PART OF THE RECORD. A single-family probe and a full campaign are different claims,
         # and only one of them can ever qualify. Stating it means a partial record cannot be read as
         # a campaign one.
         echo "mode=${CAMPAIGN_MODE:-campaign}"
         echo "discovered=${N:-0}"
         echo "selected=${EXPECTED_RUN:-${N:-0}}"
         echo "executed=${FAMILIES_RUN:-0}"
         echo "reported=$(( ${PASS:-0} + ${FAIL:-0} ))"
         echo "killed=${PASS:-0}"
         echo "invalid=${INVALID:-0}"
         echo "survived=${SURVIVED_N:-0}"
         echo "could_not_apply=${COULD_NOT_APPLY:-0}"
         echo "integrity_ok=${INTEGRITY_OK:-0}"
         # Measured, not estimated. `secs_other` is whatever is left after builds and gate runs —
         # mutation apply, restores, digests, snapshot bookkeeping — so the three numbers account for
         # the whole run and an unexplained remainder cannot hide.
         echo "secs_total=${RUN_SECS:-0}"
         echo "secs_copy=${PHASE_SECS[copy]:-0}"
         echo "secs_build=${PHASE_SECS[build]:-0}"
         echo "secs_gate=${PHASE_SECS[gate]:-0}"
         echo "secs_other=$(( ${RUN_SECS:-0} - ${PHASE_SECS[copy]:-0} - ${PHASE_SECS[build]:-0} - ${PHASE_SECS[gate]:-0} ))"
         echo "qualified=${QUALIFIED:-0}"
         # Retained for continuity with older records; `reported` is the field to read.
         echo "families_declared=${N:-0}"
         echo "families_run=${FAMILIES_RUN:-0}"
         # Split, because they are different strengths of evidence: only killed_by_gate shows the
         # family's named gate is load-bearing. A build kill skips the gate entirely.
         echo "killed_by_gate=${KILLED_BY_GATE:-0}"
         echo "killed_by_build=${KILLED_BY_BUILD:-0}"
         # The positive control's own result. A campaign whose baseline was not fully green has
         # families that could not be judged, and the artifact must say so rather than leaving a
         # reader to infer it from the kill count.
         echo "baseline_gates_green=${BASELINE_GREEN:-0}/${BASELINE_TOTAL:-0}"
         # The contract figure: how many families turned their NAMED gate red. Build kills are real
         # results but leave their gate unexercised, so they are excluded here on purpose.
         echo "gates_proven=${KILLED_BY_GATE:-0}/${N:-0}"
         echo "failed=${FAIL:-0}"
         # Where the per-family transcripts are, and how many exist. A reader can open them.
         echo "evidence_written=${EVIDENCE_WRITTEN:-0}"
         echo "evidence_root=${EVIDENCE_ROOT:-unknown}"
         echo "families_digest=${FAMILIES_DIGEST:-unknown}"
         echo "evidence_dir=.mutation-evidence/${RUN_ID:-unknown}"
         echo "run_id=${RUN_ID:-unknown}"
         echo "head=$START_HEAD"
         # Both, because they answer different questions: what ran, and what the repository holds now.
         echo "executed_driver_sha=$EXECUTED_DRIVER_SHA"
         echo "preamble_driver_sha=$PREAMBLE_DRIVER_SHA"
         echo "repo_driver_sha=$START_DRIVER"
         echo "inventory_sha=$START_INV"
         # Recorded, not merely compared. `head=` alone let a completed=1 artifact describe an
         # uncommitted tree while appearing keyed to a commit.
         echo "tracked_sha=$START_TRACKED"
         # The digest of the tree actually TESTED (the disposable copy), not only of the repository
         # around it.
         echo "workspace_tracked_sha=${WORK_TRACKED:-unknown}"
         echo "workspace_head=${WORK_HEAD:-unknown}"
         # Both measured AFTER the baseline: these name the workspace the families were actually
         # mutated in, which is not the workspace that was copied.
         echo "workspace_untracked_sha=${WORK_UNTRACKED:-unknown}"
         # RECOMPUTED AFTER THE BASELINE, not at copy time. Baseline gates call `require_fresh_binary`,
         # which BUILDS — so the compiler captured before them is not the one families were judged
         # against. And a run starting with NO compiler had `absent == absent`, passed the copy check,
         # built one during the baseline, and then recorded `absent` as the identity of the compiler
         # that produced every verdict.
         # NAMED FOR WHAT IT IS. Calling this "the compiler actually tested" was incoherent for a
         # campaign: every family that needs a build produces its OWN mutated binary, so 81 families
         # test up to 81 different compilers and no single field can name them. What this digest does
         # identify — and what is worth recording — is the PRISTINE compiler the baseline established,
         # which is the common starting point every family was mutated away from.
         echo "baseline_compiler_sha=${TESTED_BIN:-unknown}"
         echo "compilers_tested=per-family-rebuilds"
         echo "untracked_sha=$START_UNTRACKED"
         # ALWAYS EMITTED, even when empty. `refusals` is a mandatory schema key, so omitting it on
         # the happy path meant a CLEAN full campaign published a record the decoder then rejected as
         # missing_key(refusals) — the one run that must succeed was the one that could not.
         echo "refusals=$2"
       } > "$tmp" 2>/dev/null; then
    echo "error: could not write the campaign artifact at $tmp" >&2; rm -f "$tmp" 2>/dev/null; return 1
  fi
  # CHECKED. There is no `set -e` here, so an unchecked write failure would be followed by
  # "summary written" and could still exit zero.
  mv -f "$tmp" "$target" 2>/dev/null || {
    echo "error: could not install the campaign artifact" >&2; rm -f "$tmp" 2>/dev/null; return 1; }
  # RECORDED INSIDE THE FUNCTION, where `target` is in scope. The reporting below runs in the
  # caller, and reading a function-local from there is unbound under `set -u` — which killed a run
  # after its verdict was already complete.
  WROTE_ARTIFACT_PATH="${target#$ROOT_DIR/}"
  return 0
}
# INVALIDATE FIRST. Measured 2026-08-21: a run refused by the dirty-target guard exited before
# reconciliation and left the PREVIOUS run's artifact in place, so reading the artifact after a
# refused run reported the earlier run's verdicts. A stale completed=1 is worse than no artifact:
# it answers a question about a run that never happened. Every early exit and every signal now
# leaves an artifact that says completed=0.
write_summary 0 " run_did_not_reach_reconciliation" || { _gate_lock_release; exit 2; }
# REACHED_END is the live signal, replacing an unconditional `CAMPAIGN_INTERRUPTED=0` reset that sat
# just before reconciliation and cleared whatever the signal trap had recorded — so `run_interrupted`
# could never appear in a completed reconciliation. A deferred signal (bash defers a trap until the
# running foreground command returns, which for `lake build` can be minutes) was silently forgotten.
CAMPAIGN_INTERRUPTED=0
REACHED_END=0
trap 'CAMPAIGN_INTERRUPTED=1' INT TERM HUP

# ---------------------------------------------------------------------------
# ANCHORS_ONLY=1 — verify every family's OLD text still exists in its FILE, and exit. No mutation,
# no build, no gate run; seconds rather than hours.
#
# This exists because these 78 families had NO anchor-integrity check at all. `check_mutation_anchors.sh`
# covers `test_mutation.sh`'s 77 anchors and nothing else, so a family here whose target text moved
# became silently inert — the mutation cannot be applied, its gate is never proven load-bearing, and
# only a full multi-hour campaign would say so. Three had gone dead exactly that way
# (proof-staleness, subject-binds-body, classification-justifies), all three guarding R-0004
# authority boundaries, all three broken by refactors that no gate objected to.
#
# It reuses THIS FILE'S arrays rather than re-parsing them elsewhere. A second parser for the `add`
# format would be a second producer of "what the anchors are", and would drift from the harness it
# is supposed to describe — which is the defect class this repository keeps paying for.
# ---------------------------------------------------------------------------
# ONE DEFINITION OF "DOES THIS ANCHOR MATCH", used by both the applier and the anchor gate.
#
# There were two. `apply()` did an exact substring test in python; the ANCHORS_ONLY gate used
# `grep -qF`, and `grep -F` with a MULTI-LINE pattern is an ALTERNATION, not a sequence — it matches
# when any single line matches. So the gate reported "all 78 anchors match" while three multi-line
# anchors could not be applied at all, which is precisely the inert-mutation state it exists to
# detect. A checker weaker than the thing it checks converts a real failure into a green, and two
# implementations of one question is the defect class this repository keeps paying for.
#
# Defined HERE, above both consumers: ANCHORS_ONLY runs and exits long before the old `apply()`
# definition was reached, so a function defined further down would not exist yet.
mut_anchor(){ # mode(check|apply) file oldfile [newfile]
  MUT_MODE="$1" python3 - "$2" "$3" "${4:-}" <<'PYANCHOR'
import os, sys
mode = os.environ["MUT_MODE"]
f, ofl, nfl = sys.argv[1], sys.argv[2], sys.argv[3]
src = open(f).read(); old = open(ofl).read()
n = src.count(old)
if n < 1:
    sys.stderr.write("anchor not found in %s\n" % f); sys.exit(1)
# AMBIGUITY IS TESTED IN BOTH MODES, and it must be. An earlier version returned success from
# `check` for any n >= 1 and only rejected n > 1 in `apply`, which made the "shared" applier still
# WEAKER in check mode than in apply mode: ANCHORS_ONLY reported an anchor as matching that the
# campaign then could not apply. That is the same defect shape as the `grep -F` alternation this
# routine replaced — a checker weaker than the thing it checks — reintroduced one level down.
# Exactly one occurrence is required: more than one means the anchor does not identify a unique
# site, and mutating an arbitrary one of them tests something nobody chose.
if n > 1:
    sys.stderr.write("anchor is AMBIGUOUS in %s: %d occurrences\n" % (f, n)); sys.exit(2)
if mode == "check":
    sys.exit(0)
new = open(nfl).read()
open(f, "w").write(src.replace(old, new, 1))
PYANCHOR
}

if [ "${ANCHORS_ONLY:-0}" = "1" ]; then
  TMP_ANCHOR_OLD="$(mktemp)"
  trap 'rm -f "$TMP_ANCHOR_OLD"; _rm_snapdir' EXIT
  echo "=== mutation anchor integrity: $N families ==="
  # Uses `mut_anchor check` — the SAME routine the applier uses, not a second implementation.
  stale=0
  # ARITHMETIC LOOP, NOT `seq`. With `seq` absent the command substitution yields an EMPTY word list,
  # the loop body never runs, `stale` stays 0, and this printed "all 81 anchors still match" — a
  # vacuous green produced by a missing coreutil rather than by any property of the corpus.
  for (( i=0; i<N; i++ )); do
    f="${FILE[$i]}"
    if [ ! -f "$f" ]; then
      echo "  FAIL ${NAME[$i]}: file $f no longer exists"; stale=$((stale+1)); continue
    fi
    printf '%s\n' "${OLD[$i]}" > "$TMP_ANCHOR_OLD"
    perl -i -pe 'chomp if eof' "$TMP_ANCHOR_OLD"
    # THE TWO FAILURE MODES ARE REPORTED SEPARATELY. They need different repairs: a missing anchor
    # has drifted from the source and must be re-anchored, while an AMBIGUOUS one matches several
    # sites and must be widened until it identifies the one site intended. Collapsing both into
    # "anchor not found" sent me re-anchoring three mutations whose text was present all along.
    mut_anchor check "$f" "$TMP_ANCHOR_OLD" 2>/dev/null; rc=$?
    case "$rc" in
      0) ;;
      2) echo "  FAIL ${NAME[$i]}: anchor is AMBIGUOUS in $f (matches more than one site) — the campaign"
         echo "       CANNOT apply it, so this mutation is INERT and ${GATE[$i]} is unproven"
         stale=$((stale+1)) ;;
      *) echo "  FAIL ${NAME[$i]}: anchor not found in $f — this mutation is INERT and ${GATE[$i]} is unproven"
         stale=$((stale+1)) ;;
    esac
  done
  if [ "$stale" -eq 0 ]; then
    echo "  ok   all $N anchors still match their targets"
  fi
  echo "GATE-MUTATION-ANCHORS: PASS=$(( stale == 0 ? 1 : 0 )) FAIL=$stale (of $N families)"
  [ "$stale" -eq 0 ]
  exit $?
fi

# PRECONDITION: every file this run will mutate must be clean.
#
# Restore is `git checkout --`, which discards whatever is in the working tree. If a file
# is already modified when we start — by a previous run that died, or by someone's
# in-progress edit — this script silently destroys that work, and worse, a mutation
# stranded by an earlier abort is treated as pristine source. The later families then run
# their gates against a doubly-mutated tree and their KILLED/SURVIVED verdicts are wrong
# without saying so.
#
# Observed 2026-07-31: a `pkill` of this script left Concrete/Check/Layout.lean mutated,
# because the EXIT trap does not fire on signal-based termination (now also trapped
# below). Recovery depended on someone running `git status`, not on the tooling.
# check_multi_kernel.sh already refuses to run dirty for exactly this reason; this is the
# same guard.
# START_DIRTY is the single producer of this fact (snap_dirty_targets, above). This guard used to
# recompute it with its own loop; two producers of "which targets are dirty" can disagree, and the
# reconciliation dimension below was unreachable because this guard exits first.
if [ -n "$START_DIRTY" ]; then
  write_summary 0 " mutation_target_dirty_at_start($START_DIRTY)"
  echo "error: refusing to run — these files have uncommitted changes and would be" >&2
  echo "       DESTROYED by the restore step: $START_DIRTY" >&2
  echo "       Commit, stash, or 'git checkout --' them first. If a previous run was" >&2
  echo "       killed, verify the diff is yours before discarding it." >&2
  # RELEASE ON THE REFUSAL PATH. The campaign acquires the lock above but its cleanup trap is not
  # installed until the workspace exists, so every early exit between the two leaked the lock and the
  # next gate refused with "REPOSITORY BUSY" until someone removed it by hand.
  _gate_lock_release
  exit 2
fi

# ---------------------------------------------------------------------------
# DISPOSABLE ISOLATION. Mutations are applied to a COPY of the repository, never to the working tree.
#
# Everything below used to mutate in place and restore afterwards. That is recoverable from an
# ordinary exit and from INT/TERM/HUP, and recoverable from nothing else: `kill -9` runs no trap, and
# on 2026-08-19 exactly that left Concrete/Elab/Elab.lean holding `declSpan := none` while this
# harness had already exited. A subsequent gate then measured a compiler nobody wrote.
#
# With the mutation confined to a copy, a killed run leaves an orphaned directory under /tmp — which
# is garbage, not corruption. The failure mode is bounded by construction rather than by remembering
# to trap one more signal.
#
# CHEAP because the copy is made ONCE per run and the filesystem is copy-on-write: 627 MB of `.lake`
# copies in ~0.05s here. `.lake` comes along so the build in the copy is INCREMENTAL — only the
# mutated file and its dependents recompile, which is the same work the in-place version did. `.git`
# comes along so a family can restore its target between mutations without reaching outside.
# NAMESPACED, and swept on start. SIGKILL cannot be trapped, so a killed campaign always leaks its
# workspace; measured 2026-08-21, two killed runs left 1.4 GB in /tmp because the 696 MB repo copy
# lives inside this directory. A plain `mktemp -d` name cannot be swept safely — it is
# indistinguishable from every other program's temp dir — so the workspace carries the owning PID in
# its name and a marker naming itself, and only dirs matching BOTH whose owner is gone are removed.
_MUT_TMP_PREFIX="${TMPDIR:-/tmp}/concrete-mut"
_swept=0; _kept=0; _stale=""
for _d in "$_MUT_TMP_PREFIX".*; do
  [ -d "$_d" ] || continue
  [ -f "$_d/.concrete-mutation-workspace" ] || { _kept=$((_kept+1)); continue; }
  _opid="$(sed -n 's/^owner_pid=//p' "$_d/.concrete-mutation-workspace" 2>/dev/null | head -1)"
  case "$_opid" in ''|*[!0-9]*) _kept=$((_kept+1)); continue;; esac
  # A DEAD OWNER IS NOT ENOUGH — this is the same mistake `fresh.sh` refuses to make about the
  # repository lock, and it would be worse here because the remedy is `rm -rf` rather than a refusal.
  # The recorded pid is the campaign SHELL. Its `lake`/gate descendants outlive it when it is killed,
  # and they are still building inside this directory. Deleting it underneath them destroys live work
  # and produces a verdict about an artifact that stopped existing halfway through.
  #
  # So both must hold: the owner is gone AND nothing is currently working in the directory. Liveness
  # is established by looking for any process whose cwd or open files sit under it. If that cannot be
  # determined on this platform, the directory is KEPT — an unswept 700 MB copy costs disk, a wrongly
  # swept one costs a live campaign.
  if kill -0 "$_opid" 2>/dev/null; then _kept=$((_kept+1)); continue; fi
  # NO AUTOMATIC RECLAIM — the same policy `fresh.sh` applies to the lock, and for a stronger reason:
  # the remedy here is `rm -rf` of a 700 MB tree rather than a refusal.
  #
  # Two successive attempts to make automatic deletion safe were both wrong. Checking only the owner
  # PID ignored the `lake` descendants that outlive a killed campaign shell. Adding a "did the /proc
  # scan see anything" witness was INERT, because the scanning process always resolves its own cwd, so
  # the witness was satisfied whether or not any other process was inspectable. There is also an
  # unavoidable gap between scanning and deleting during which a descendant can enter the directory.
  #
  # So the default is to REPORT abandoned workspaces and let a human remove them. Disk is cheap;
  # deleting a live campaign's workspace produces a verdict about an artifact that stopped existing
  # halfway through. CONCRETE_MUT_SWEEP=1 opts in explicitly for an operator who has just checked.
  if [ "${CONCRETE_MUT_SWEEP:-0}" != "1" ]; then
    _stale="$_stale $_d"; _kept=$((_kept+1)); continue
  fi
  _busy=unknown
  if [ -d /proc ]; then
    # SEEN-ANYTHING is tracked separately from FOUND-ANYTHING. Initialising to `no` and ignoring every
    # failed readlink meant a restricted /proc, or a permissions failure, inspected nothing, stayed
    # `no`, and licensed `rm -rf` of a directory a descendant might still be using.
    _busy=no; _seen=0
    # CWD AND OPEN DESCRIPTORS. Scanning only `cwd` missed the case that matters most: a `lake`
    # process whose working directory is elsewhere but which holds an open .olean under this
    # workspace. Deleting it then destroys a live build's inputs mid-write.
    for _c in /proc/[0-9]*/cwd /proc/[0-9]*/fd/*; do
      _t="$(readlink "$_c" 2>/dev/null)" || continue
      _seen=$((_seen+1))
      case "$_t" in "$_d"|"$_d"/*) _busy=yes; break;; esac
    done
    # Our own process has a cwd, so a scan that resolved NOTHING did not work.
    [ "$_busy" = "yes" ] || [ "$_seen" -gt 0 ] || _busy=unknown
  elif command -v lsof >/dev/null 2>&1; then
    # `lsof +D` exits nonzero BOTH when nothing is open under the path and when it could not look
    # (permissions, a vanished mount). Treating every nonzero as "not busy" turns an inspection
    # ERROR into a licence to delete, so only an explicit empty result counts as not-busy.
    if _lsof_out="$(lsof -t +D "$_d" 2>/dev/null)"; then
      if [ -n "$_lsof_out" ]; then _busy=yes; else _busy=no; fi
    else
      _busy=unknown
    fi
  fi
  if [ "$_busy" = "no" ]; then rm -rf "$_d" && _swept=$((_swept+1))
  else _kept=$((_kept+1)); fi
done
# Reported, not silent: "no output" must not be the only evidence that a sweep happened.
echo "workspace sweep: reclaimed $_swept abandoned workspace(s), left $_kept in place"
if [ -n "${_stale:-}" ]; then
  echo "  ABANDONED workspaces found (owner gone). NOT deleted automatically — verify nothing is"
  echo "  working in them, then remove explicitly, or re-run with CONCRETE_MUT_SWEEP=1:"
  for _d in $_stale; do echo "      rm -rf $_d   ($(du -sh "$_d" 2>/dev/null | cut -f1))"; done
fi
# CHECKED, for the same reason as the snapshot directory above: an empty $TMP makes WORK="/repo".
TMP=$(mktemp -d "$_MUT_TMP_PREFIX.$$.XXXXXX") || {
  echo "FATAL: could not create the campaign workspace directory" >&2; _gate_lock_release; exit 2; }
[ -n "$TMP" ] && [ -d "$TMP" ] || {
  echo "FATAL: campaign workspace directory is not usable" >&2; _gate_lock_release; exit 2; }
WORK="$TMP/repo"
mkdir -p "$WORK" || { echo "FATAL: could not create $WORK" >&2; _gate_lock_release; exit 2; }
printf 'owner_pid=%s\nstarted_head=%s\n' "$$" "$START_HEAD" > "$TMP/.concrete-mutation-workspace"
_copy_t0=$(date +%s)
cp -a "$ROOT_DIR/." "$WORK/" 2>/dev/null || { echo "error: could not create the isolated workspace" >&2; _gate_lock_release; exit 2; }
PHASE_SECS[copy]=$(( $(date +%s) - _copy_t0 ))
[ -d "$WORK/.git" ] || { echo "error: isolated workspace has no .git; per-family restore would not work" >&2; _gate_lock_release; exit 2; }

# THE ACTIVATED WORKSPACE IS BOUND TO THE RECORDED STATE.
#
# Start state is captured, and the `cp -a` happens later. The lock stops other GATES, not an editor or
# a `git` command — so a tracked edit landing between capture and copy, and reverted before
# reconciliation, was copied into the workspace, TESTED, and then invisible to the start/end
# comparison. A textbook ABA: the campaign would report verdicts about bytes different from the ones
# its own artifact names. Digesting the COPY closes it, because the copy is what gets tested.
# ALL THREE DIMENSIONS, because `ts_tracked` hashes `git diff HEAD` — and every clean checkout has
# the SAME empty-diff digest. Comparing only that left two ordinary ABA paths open: HEAD moving between
# capture and copy (the new clean checkout copies to an identical diff digest) and an untracked file
# appearing, being copied, influencing the build, and vanishing before reconciliation.
# THE COMPILER BINARY IS BOUND TOO. `cp -a` copies IGNORED content — `.lake`, dependency trees,
# generated binaries — and those are build inputs for every gate that runs here, while
# `ts_untracked` deliberately excludes ignored paths. So a build or package update landing between
# capture and copy changed what was TESTED while all three tracked/untracked/head digests still
# matched. Hashing the whole 627 MB `.lake` tree per run is not affordable; the compiler binary is the
# input that actually decides gate verdicts, so it is bound explicitly and the residual is stated
# rather than implied.
_bin_sha() { [ -f "$1/.lake/build/bin/concrete" ] \
  && { sha256sum "$1/.lake/build/bin/concrete" 2>/dev/null || shasum -a 256 "$1/.lake/build/bin/concrete"; } | cut -c1-32 \
  || echo "absent"; }
WORK_TRACKED="$(ts_tracked "$WORK")"
WORK_HEAD="$(ts_head "$WORK")"
WORK_UNTRACKED="$(ts_untracked "$WORK")"
WORK_BIN="$(_bin_sha "$WORK")"
ROOT_BIN="$(_bin_sha "$ROOT_DIR")"
if [ "$WORK_BIN" != "$ROOT_BIN" ]; then
  echo "error: the workspace compiler binary does not match the repository's." >&2
  echo "       repository: $ROOT_BIN" >&2
  echo "       workspace:  $WORK_BIN" >&2
  echo "       Something rebuilt between the snapshot and the copy, so gate verdicts would describe" >&2
  echo "       a different compiler from the one this run records. Refusing." >&2
  write_summary 0 " workspace_binary_diverged"
  rm -rf "$TMP"; _gate_lock_release; exit 2
fi
if [ "$WORK_TRACKED" != "$START_TRACKED" ] \
   || [ "$WORK_HEAD" != "$START_HEAD" ] \
   || [ "$WORK_UNTRACKED" != "$START_UNTRACKED" ]; then
  echo "error: the workspace copy does not match the recorded repository state." >&2
  echo "       recorded: head=$START_HEAD tracked=$START_TRACKED untracked=$START_UNTRACKED" >&2
  echo "       copied:   head=$WORK_HEAD tracked=$WORK_TRACKED untracked=$WORK_UNTRACKED" >&2
  echo "       Something wrote to the tree between the snapshot and the copy, so any verdict would" >&2
  echo "       describe different bytes from those this run records. Refusing." >&2
  write_summary 0 " workspace_copy_diverged"
  rm -rf "$TMP"
  _gate_lock_release
  exit 2
fi
# INT/TERM as well as EXIT: plain EXIT did not restore the tree when this script was
# killed (observed 2026-07-31, Layout.lean left mutated). The handler re-raises so the
# exit status still reflects the signal.
#
# KNOWN LIMIT, measured rather than assumed: bash defers a trap until the current
# foreground command returns, so a TERM delivered while `lake build` is running does not
# take effect for however long that build has left — verified by sending TERM to a run
# mid-build and watching it continue. Restoration is therefore reliable but NOT prompt.
# If you must stop a run immediately, kill the build too, then check `git status` before
# doing anything else; the dirty-tree guard above will refuse the next run rather than
# silently treating a stranded mutation as pristine source, which is the failure this
# pair of mechanisms exists to prevent.
# RESTORATION IS VERIFIED, not assumed. `git checkout --` can fail — a read-only file, a full disk,
# an index lock held by something else — and it reports that on stderr which this discards. A restore
# that ran is not a restore that worked, and the difference is a mutated compiler source left in the
# tree while the harness exits 0.
#
# HUP is trapped alongside INT/TERM: a run started over ssh or in a terminal that closes gets HUP,
# not TERM, and the 2026-07-31 incident that motivated the INT/TERM traps would have recurred
# unchanged for that one signal.
#
# NOTHING HERE SURVIVES SIGKILL, and it is worth being plain about that rather than implying
# otherwise: `kill -9` runs no trap, and on 2026-08-19 exactly that left Concrete/Elab/Elab.lean
# holding `declSpan := none`. The dirty-tree guard above catches it on the NEXT run, and the gate
# runner's start/end reconciliation catches it in the SAME run. In-place mutation cannot do better
# than that; only isolating the mutation into a disposable worktree can, which is tracked separately.
MUT_FILES_SORTED="$(printf "%s\n" "${FILE[@]}" | sort -u)"
# TREE_HASH_START was computed here and never read by anything — a dead producer of an apparent
# integrity fact, which reads as protection when scanning the file. The live checks are
# `snap_dirty_targets` (start guard and end reconciliation) and `dispose_all`'s verification that the
# real tree was never touched; both are compared against something.

# DISPOSE, don't repair. The working tree is never mutated now, so there is nothing to restore —
# only a copy to throw away. The verification below therefore asserts something stronger than "the
# restore worked": it asserts the real tree was never touched in the first place, which is a claim
# the previous design could not make at all.
dispose_all(){
  rm -rf "$TMP"
  # The driver snapshot is removed HERE too. Its own EXIT trap (set before the re-exec) is overwritten
  # by this trap and by the ANCHORS_ONLY trap, so on a normal run nothing removed it and every
  # invocation — including every cheap anchor check — left one behind.
  _rm_snapdir
  # Release the lock this campaign took, if it took one. Only the creator's release does anything.
  [ "${CAMPAIGN_HELD_LOCK:-0}" = "1" ] && _gate_lock_release
  # THE SHARED PRODUCER, not a private `git diff`. This last-line check — the one that asserts the
  # real tree was never touched — used the index-relative form, so a STAGED late edit to a mutation
  # target passed it while the campaign's own guard would have caught the same change.
  local touched
  # shellcheck disable=SC2086
  touched="$(ts_dirty_files "$ROOT_DIR" $MUT_FILES_SORTED)"
  if [ -n "$touched" ]; then
    echo "" >&2
    echo "WORKING TREE WAS MUTATED:$touched" >&2
    echo "  Mutations are supposed to be confined to a disposable copy. If these files differ, the" >&2
    echo "  isolation was bypassed — inspect with 'git diff' before running anything else." >&2
    # EXIT EXPLICITLY. `return 1` from a function called in an EXIT trap does NOT change the process
    # status, so a run that detected a mutated real tree still exited 0 and reported success. Calling
    # `exit` inside the trap does set it.
    exit 1
  fi
  return 0
}
trap 'dispose_all' EXIT
trap 'dispose_all; trap - INT TERM HUP; kill -s INT $$' INT
trap 'dispose_all; trap - INT TERM HUP; kill -s TERM $$' TERM
trap 'dispose_all; trap - INT TERM HUP; kill -s HUP $$' HUP

apply(){ mut_anchor apply "$1" "$2" "$3"; }

# RESTORE IS VERIFIED, NOT ATTEMPTED. Every per-family restore used to be
# `git -C "$WORK" checkout -- "$file" 2>/dev/null` with its status discarded. A restore that fails
# leaves the previous family's mutation ACTIVE in the workspace, so every later family runs its gate
# against an accumulating pile of mutations and its KILLED/SURVIVED verdict describes a tree nobody
# chose. `mutation_target_not_restored` cannot see this: it inspects the real repository, not $WORK.
# A workspace that will not restore invalidates the rest of the campaign, so this is fatal, not a
# warning.
WORK_DIRTY=0
restore_work(){ # file
  if ! git -C "$WORK" checkout -- "$1" 2>"$TMP/rerr"; then
    echo "  FATAL: could not restore $1 in the workspace ($(tr '\n' ' ' < "$TMP/rerr"))" >&2
    echo "         Every later family would run against this mutation. Aborting the campaign." >&2
    WORK_DIRTY=1; return 1
  fi
  # `git checkout` can report success while leaving the file modified if the pathspec did not match.
  if ! git -C "$WORK" diff --quiet -- "$1" 2>/dev/null; then
    echo "  FATAL: $1 is still modified after a successful-looking restore. Aborting." >&2
    WORK_DIRTY=1; return 1
  fi
  return 0
}

# THE CLEAN-GATE POSITIVE CONTROL — the pairing that stops "reject all" from passing.
#
# The campaign ran ONLY the mutated gate and read every nonzero exit as a kill. Nothing established
# that the gate PASSES on an unmutated workspace, so a gate that was broken for any unrelated reason
# — a missing fixture, a stale lock left by an earlier gate, a pre-existing failure, a build lock —
# was credited as mutation evidence. Measured 2026-08-21: run directly without an inherited
# repository lock, check_corecheck_boundary.sh exits 0 and LEAVES $WORK/.gate.lock behind (fresh.sh
# installs no release), after which check_copy_judgment.sh exits 1 with "REPOSITORY BUSY" — a
# manufactured kill that names the right gate for entirely the wrong reason.
#
# MEASURED ONCE, UP FRONT, ON A PRISTINE WORKSPACE — not lazily per family.
#
# The lazy version was unsound and the reason is subtle: `restore_work` restores the SOURCE file, but
# the `.lake` outputs built from the previous family's mutation stay behind. A gate first encountered
# midway through the campaign would therefore take its "clean" baseline against compiled artifacts
# built from a mutation — and some gates never rebuild, so nothing would correct it
# (`check_dependency_edges.sh:63` drives `lake env lean` directly and does not call
# `require_fresh_binary`). The baseline has to be taken while the workspace is genuinely untouched,
# which is only true before the first mutation is applied.
#
# Cost: one run per DISTINCT gate — 33 for the current 81 families, not 81.
declare -A CLEAN_GATE
# DERIVED FROM EACH GATE'S OWN CLEAN RUN, not from a static list. 209 of 214 gates in this tree emit a
# `NAME: PASS=n FAIL=m` verdict line, so "no verdict line" is strong evidence a failure was
# infrastructural rather than a gate disagreeing — but only for a gate that emits one in the first
# place. Measuring it per gate during the pristine baseline makes the requirement a fact about that
# gate instead of an assumption about all of them.
# WHAT THE GATE PRINTS LAST, captured from its own pristine run.
#
# The first version of this control accepted any `PASS=n`/`FAIL=n` ANYWHERE in the log, which
# authenticated a NESTED producer: `run_tests.sh` prints an inner `ORACLE: PASS=… FAIL=…` early and
# only later reaches its own `passed:`/`failed:` summary, so an unrelated failure after that oracle
# output still looked like a completed gate run. The question is not "does the log contain something
# verdict-shaped" but "did this gate reach the end it reaches when it succeeds" — so the control keys
# on the last non-empty line's first token, measured per gate on pristine source.
declare -A CLEAN_GATE_VERDICT
declare -A CLEAN_GATE_TAIL
# THE LAST LINE WITH ITS NUMBERS NORMALISED, not its first token.
#
# Keying on the first token was inert for a real corpus gate: `run_tests.sh`'s final token is
# `summary`, and EVERY premature exit prints "no summary was produced" — which contains `summary`. So
# the control accepted exactly the runs it existed to reject. Comparing the whole last line fails for
# the opposite reason (the counts in it change under mutation), so digits are normalised to `#` and the
# SHAPE of the line is what must match.
_tail_shape() { awk 'NF {last=$0} END {gsub(/[0-9]+/, "#", last); print last}' "$1" 2>/dev/null; }
# ONE PREDICATE FOR "IS THIS RED A GATE VERDICT", used by BOTH red legs.
#
# The first leg grew these checks one review round at a time; the confirming leg was written later and
# had none of them, so the confirmation was weaker than the thing it confirmed. Two implementations of
# one question again — the defect class this whole arc keeps rediscovering — so there is now one.
_RED_LEG_WHY=""
_red_leg_is_gate_evidence() { # log rc gate
  _RED_LEG_WHY=""
  if [ "$2" -eq "$_DIED_EARLY_RC" ]; then
    _RED_LEG_WHY="exited $_DIED_EARLY_RC, the documented died-early code"; return 1
  fi
  if grep -q 'GATE-PRECONDITION-FAILED:' "$1" 2>/dev/null; then
    _RED_LEG_WHY="$(grep -m1 -o 'GATE-PRECONDITION-FAILED:.*' "$1")"; return 1
  fi
  if [ "${CLEAN_GATE_VERDICT[$3]:-no}" = "yes" ] && ! _reached_own_end "$1" "$3"; then
    _RED_LEG_WHY="never reached the end it reaches on pristine source"; return 1
  fi
  return 0
}

_reached_own_end() { # log gate
  local want="${CLEAN_GATE_TAIL[$2]:-}"
  [ -n "$want" ] || return 0            # nothing measured: cannot require anything
  [ "$(_tail_shape "$1")" = "$want" ]
}
# EXIT 97 IS "DIED EARLY", BY CONTRACT. `run_tests.sh` documents 97 as a code no assertion produces,
# emitted by an EXIT trap when its summary file was never written, precisely so "died early" cannot be
# read as "failed". Any gate exiting 97 therefore did not reach a verdict.
_DIED_EARLY_RC=97
# THE PRISTINE TREE MUST BUILD BEFORE ANY FAMILY RUNS.
#
# Some gates deliberately perform no build (check_vc_bridge_register.sh, check_transform_register.sh),
# so for their families the FIRST build of the run is the MUTATED one — and its failure is accepted as
# a kill. With `reference-division` and `transform-has-effect` on the declared build-kill allowlist, an
# already-unbuildable pristine workspace would produce KILLED, PARTIAL PASS and exit 0 without anything
# having been tested: the same shape as a CI job that is green because it never ran.
# VALIDATED, because these producers signal failure IN BAND and return status 0. Re-measuring without
# checking would write a `TREESTATE-UNAVAILABLE:*` marker — or an empty string — straight into the
# authority artifact and still permit `completed=1`: an ordinary mktemp, sort, git or hashing failure
# during the remeasurement would then certify a workspace nobody measured. The start/end reconciliation
# validates only the START_* values; these are separate ones.
_require_measured() { # value label
  case "${1:-}" in
    *TREESTATE-UNAVAILABLE*|"")
      echo "error: post-baseline workspace measurement failed ($2): '${1:-<empty>}'." >&2
      echo "       Refusing: the artifact would name a workspace that was never measured." >&2
      write_summary 0 " workspace_remeasure_failed($2)"
      rm -rf "$TMP"; _gate_lock_release; exit 2 ;;
  esac
}

baseline_pristine_build(){
  printf "baseline: pristine workspace builds ... "
  # THE PRISTINE BUILD IS RUN-LEVEL EVIDENCE. Three families declare a BUILD route, and their kills
  # only mean anything because the UNMUTATED tree built first. That transcript was discarded.
  if _timed_build "$TMP/pristine_build.log"; then
    keep_baseline_log "_pristine_build" "$TMP/pristine_build.log"
    echo "ok"; return 0
  fi
  echo "FAILED"
  echo "error: the UNMUTATED workspace does not build, so every build-kill verdict would be" >&2
  echo "       meaningless — a mutation cannot be blamed for a failure that predates it." >&2
  echo "       see $TMP/pristine_build.log" >&2
  return 1
}

baseline_all_gates(){
  local g total=0 red=0
  local -a uniq=()
  # Distinct gates, in inventory order.
  for g in "${GATE[@]}"; do
    [ -z "${CLEAN_GATE[$g]:-}" ] || continue
    CLEAN_GATE[$g]=pending; uniq+=("$g")
  done
  total=${#uniq[@]}
  echo "=== clean-gate baseline: $total distinct gates on the pristine workspace ==="
  for g in "${uniq[@]}"; do
    if _timed_gate "scripts/tests/$g" "$TMP/clean.log"; then
      CLEAN_GATE[$g]=yes
      _note_freshness_taint "$TMP/clean.log"
      # THE POSITIVE CONTROL'S OWN TRANSCRIPT, kept per gate at RUN level. $TMP/clean.log is reused
      # by every gate in this loop, so by the end it holds only the LAST one — and it used to be
      # copied into all 81 family records as though it were each family's evidence. It belongs to
      # the run: it is what makes a later red meaningful, and discarding it left every kill resting
      # on a green baseline nobody could re-read.
      keep_baseline_log "$g" "$TMP/clean.log"
      CLEAN_GATE_TAIL[$g]="$(_tail_shape "$TMP/clean.log")"
      if [ -n "${CLEAN_GATE_TAIL[$g]}" ]; then CLEAN_GATE_VERDICT[$g]=yes; else CLEAN_GATE_VERDICT[$g]=no; fi
    else
      CLEAN_GATE[$g]=no; red=$((red+1))
      # KEPT ON THE RED PATH TOO. This transcript is the reason every family naming this gate is
      # reported INVALID; retaining it only when the baseline is GREEN would discard exactly the
      # diagnosis a reader needs.
      keep_baseline_log "$g" "$TMP/clean.log"
      echo "  RED ON CLEAN: $g — every family naming it will be reported INVALID, not killed"
    fi
  done
  # Reported, never silent: a baseline that measured nothing must not look like a baseline that passed.
  echo "  baseline: $((total-red))/$total gates green on the unmutated workspace"
  BASELINE_GREEN=$((total-red)); BASELINE_TOTAL=$total; BASELINE_RED=$red
}
gate_clean_ok(){ # gate-path (bare filename) — was this gate green on the PRISTINE workspace?
  [ "${CLEAN_GATE[$1]:-no}" = "yes" ]
}

# Attribution counters. A build kill is a real and strong result, but it is NOT evidence that the
# family's named gate is load-bearing, because the gate is skipped once the build already failed.
KILLED_BY_GATE=0; KILLED_BY_BUILD=0
UNDECLARED_BUILD_KILLS=""
# DISPOSITIONS ARE COUNTED SEPARATELY. `FAIL` lumped three different outcomes together — a gate that
# was already red, a mutation that could not be applied, and a mutation the gate FAILED TO CATCH — so
# the artifact could not say which. They mean different things and need different work: a survivor is
# a coverage gap in the gate, an invalid is a broken experiment, and a could-not-apply is a stale
# anchor. The 81-family run on 898d9a7b reported `failed=8` and left which-was-which to be recovered
# from the log.
INVALID=0; SURVIVED_N=0; COULD_NOT_APPLY=0
FRESHNESS_UNVERIFIED=0
_note_freshness_taint() { grep -q 'GATE-FRESHNESS-UNVERIFIED' "$1" 2>/dev/null && FRESHNESS_UNVERIFIED=1; return 0; }

# A BUILD KILL MUST BE DECLARED FOR THAT FAMILY.
#
# Counting build kills separately was not enough: they still entered PASS, still counted in `killed=`,
# and still permitted `completed=1` — so the gate could report full coverage while a family's named
# gate had never run. For these three the type system (or a proof) rejecting the mutation IS the
# intended outcome, and each says so in its own comment; the mutation is unrepresentable, which is
# stronger than a gate going red. For any OTHER family, a build kill means the mutation broke the
# build instead of exercising the rule, and the run must say so rather than bank it as coverage.
# Evidence accounting. A record that could not be written is a REFUSAL, not a silent gap: the run
# would otherwise claim red/green/red for a family whose transcript nobody can read.
EVIDENCE_WRITTEN=0
EVIDENCE_FAILED=""
# RUN IDENTITY, NOT JUST HEAD. Keying evidence on the SHA alone let a later single-family run
# overwrite a family that an older full-campaign summary still points at — same HEAD, different run,
# possibly different driver or working state. Each run publishes under its own directory and names
# that directory in its summary, so a summary and its evidence cannot drift apart.
# HEAD + second + pid can repeat across hosts sharing a checkout, and the run directory is supposed
# to be unique. A random component removes the coincidence; `mktemp -u` is used because it draws from
# the same entropy the rest of this script relies on.
# INHERITED FROM THE SUPERVISOR when there is one, so the id the launcher status is bound to and the
# id the evidence is filed under cannot diverge. The fallback exists for the modes that run without a
# supervisor at all (anchor and coverage checks), which publish nothing.
RUN_ID="${CONCRETE_MUT_RUN_ID:-${START_HEAD:0:12}-$(date +%Y%m%dT%H%M%S)-$$-$(basename "$(mktemp -u XXXXXX)")}"
EVIDENCE_DIR="$ROOT_DIR/.mutation-evidence/$RUN_ID"

# The per-family transcript files. Named once: publication copies exactly these, and the family
# preamble clears exactly these.
# clean.log is deliberately NOT here: it is written by the BASELINE loop, not by any family, and
# including it is what let the last baseline gate's transcript be copied into every family record.
# The baseline keeps its own per-gate transcripts under _baseline/.
EV_LOGS="build.log gate.log confirm_clean.log confirm_red.log confirm_build.log confirm_build2.log aerr aerr2"

# ONE PRODUCER, because the baseline is implemented TWICE — once for the campaign sweep and once for
# single-family selection — and adding retention to only one of them is exactly the drift this
# repository keeps being bitten by. I did precisely that: the campaign path kept its transcript and
# the single-family path did not, and the single-family run I used to verify the change could not
# see the difference.
keep_baseline_log() { # gate-name source-log
  # A SILENT COPY FAILURE LEAVES THE POSITIVE CONTROL UNEVIDENCED. `|| true` here meant a full disk
  # or an unwritable path produced a campaign whose baseline transcript simply was not there, with
  # nothing saying so — and every kill in that campaign rests on that baseline having been green.
  if ! mkdir -p "$EVIDENCE_DIR/_baseline" 2>/dev/null \
     || ! cp "$2" "$EVIDENCE_DIR/_baseline/$1.log" 2>/dev/null \
     || [ ! -s "$EVIDENCE_DIR/_baseline/$1.log" ]; then
    BASELINE_EVIDENCE_FAILED="${BASELINE_EVIDENCE_FAILED:-} $1"
  fi
  return 0
}

# PUBLISH ONE FAMILY'S RECORD. Called at EVERY exit from run_one, including the early ones: a family
# that never reached its verdict is exactly the one whose transcript a reader needs, and those paths
# used to delete it. Arguments are explicit so an early return cannot publish another family's
# leftover variables.
publish_evidence() { # nm index file gate killed invalid note [disposition]
  local nm="$1" idx="$2" file="$3" gate="$4" killed="$5" invalid="$6" note="$7" disp="${8:-}"
  # THE STORED INDEX MUST REPRODUCE THE RUN. The array index is ZERO-based and FAMILY= is ONE-based,
  # so a record for FAMILY=73 said index=72 and anyone using it to re-run selected a DIFFERENT
  # experiment. The selector is stored as the value you actually type.
  local selector=$(( idx + 1 ))
  # DISPOSITION IS EXPLICIT, not inferred from two booleans. A could-not-apply was written as
  # invalid=1 while the summary deliberately counts it under could_not_apply, so the record and the
  # artifact disagreed about what happened.
  if [ -z "$disp" ]; then
    if [ "$killed" = "1" ]; then disp=killed
    elif [ "$invalid" = "1" ]; then disp=invalid
    else disp=survived; fi
  fi
  local stage final l ok=1
  final="$EVIDENCE_DIR/$nm"
  # mktemp, not a predictable name: `mkdir -p .staging.$nm.$$` silently REUSED a stranded directory
  # from an interrupted run, so a new record could inherit old files.
  mkdir -p "$EVIDENCE_DIR" 2>/dev/null || { EVIDENCE_FAILED="$EVIDENCE_FAILED $nm"; return 0; }
  stage="$(mktemp -d "$EVIDENCE_DIR/.staging.XXXXXX" 2>/dev/null)" \
    || { EVIDENCE_FAILED="$EVIDENCE_FAILED $nm"; return 0; }
  for l in $EV_LOGS; do
    if [ -f "$TMP/$l" ]; then cp "$TMP/$l" "$stage/$l" 2>/dev/null || ok=0; fi
  done
  printf 'family=%s\nselector=FAMILY=%s\narray_index=%s\nfile=%s\ngate=%s\ndisposition=%s\nkilled=%s\ninvalid=%s\nbuild_required=%s\nexpected_route=%s\nhead=%s\nrun_id=%s\nverdict=%s\n' \
    "$nm" "$selector" "$idx" "$file" "$gate" "$disp" "$killed" "$invalid" "${BUILD[$idx]:-unknown}" \
    "$(if build_kill_declared "$nm"; then echo build; else echo gate; fi)" \
    "$START_HEAD" "$RUN_ID" "$note" > "$stage/verdict.txt" 2>/dev/null || ok=0
  # VALIDATE BEFORE PUBLISHING. Every cp and the verdict write used to be `|| true`, and
  # evidence_written incremented on the rename alone — so an EMPTY directory counted as evidence.
  [ -s "$stage/verdict.txt" ] || ok=0
  if [ "$ok" != "1" ]; then
    rm -rf "$stage" 2>/dev/null; EVIDENCE_FAILED="$EVIDENCE_FAILED $nm"; return 0
  fi
  # AN EXISTING RECORD IS A COLLISION, NOT SOMETHING TO REPLACE. The run directory is unique per run
  # and each family publishes once, so finding a record already there means two writers are using one
  # identity — and the previous move-aside dance had a window in which NO final record existed, so an
  # interruption left only a hidden `.previous`. Refusing removes both the window and the ambiguity.
  if [ -e "$final" ]; then
    rm -rf "$stage" 2>/dev/null
    EVIDENCE_FAILED="$EVIDENCE_FAILED $nm(duplicate-record)"
    return 0
  fi
  # ONE rename, into a name nothing occupies. Two renames are not atomic even on one filesystem.
  if mv "$stage" "$final" 2>/dev/null && [ -s "$final/verdict.txt" ]; then
    EVIDENCE_WRITTEN=$((EVIDENCE_WRITTEN + 1))
  else
    rm -rf "$stage" 2>/dev/null
    EVIDENCE_FAILED="$EVIDENCE_FAILED $nm"
  fi
  return 0
}
# DECLARED, each with the lock that rejects it. Until 2026-10-03 none of these could be
# attributed (the header-layout bug above), so the list was never exercised by a build kill:
#   attestation-precondition — `rfl` locks at Report.lean (multiKernelAdapter.admits on
#     unproven/planned VCs is false) refuse `actsOn := fun _ => true` at compile time.
#   kernel-foundation        — `rfl` locks at Evidence.lean (foundationSummary with isabelle
#     is (2, "CIC×HOL")) refuse collapsing Isabelle into CIC at compile time.
# A compile-time lock rejecting the mutation is the strongest kill; it also means the named
# gate never ran for that family, which is why each declaration is deliberate, not inferred.
EXPECT_BUILD_KILL=" trap-quotient-condition reference-division transform-has-effect attestation-precondition kernel-foundation "
build_kill_declared(){ case "$EXPECT_BUILD_KILL" in *" $1 "*) return 0;; *) return 1;; esac; }

run_one(){
  local i="$1"
  [ "$WORK_DIRTY" = "0" ] || return 0
  FAMILIES_RUN=$((FAMILIES_RUN + 1))
  local nm="${NAME[$i]}" file="${FILE[$i]}" gate="scripts/tests/${GATE[$i]}" needs="${BUILD[$i]}"
  # CLEAR THE PREVIOUS FAMILY'S TRANSCRIPT. Every family reuses the campaign-wide $TMP and these logs
  # were never removed, so publication copied whatever happened to be there: a BUILD=no family
  # inherited the previous family's build.log, and clean.log was the last baseline gate's transcript
  # copied into every record. A single-family run cannot expose that; a campaign silently would.
  for _l in $EV_LOGS; do rm -f "$TMP/$_l" 2>/dev/null; done
  printf '%s\n' "${OLD[$i]}" > "$TMP/old"; printf '%s\n' "${NEW[$i]}" > "$TMP/new"
  # strip the trailing newline the printf added (match raw substring)
  perl -i -pe 'chomp if eof' "$TMP/old" "$TMP/new"
  echo "--- family $((i+1))/$N: $nm ($file -> ${GATE[$i]}) ---"
  FAMILY_SECS[build]=0; FAMILY_SECS[gate]=0; _fam_t0=$(_sw_now)
  # THE POSITIVE CONTROL was measured before this campaign mutated anything. A gate that is already
  # red proves nothing by going red again.
  if ! gate_clean_ok "${GATE[$i]}"; then
    echo "  FAIL $nm: INVALID — ${GATE[$i]} does not pass on the UNMUTATED workspace, so its failure"
    echo "       under mutation is not evidence. Fix the gate (or its fixtures/lock) first."
    FAIL=$((FAIL+1)); INVALID=$((INVALID+1))
    publish_evidence "$nm" "$i" "$file" "${GATE[$i]}" 0 1 "INVALID — gate not green on the unmutated workspace"
    return
  fi
  if ! apply "$WORK/$file" "$TMP/old" "$TMP/new" 2>"$TMP/aerr"; then
    echo "  FAIL $nm: could not apply mutation ($(cat "$TMP/aerr"))"
    FAIL=$((FAIL+1)); COULD_NOT_APPLY=$((COULD_NOT_APPLY+1))
    publish_evidence "$nm" "$i" "$file" "${GATE[$i]}" 0 0 "could not apply mutation ($(cat "$TMP/aerr" 2>/dev/null))" could_not_apply
    restore_work "$file"
    return
  fi
  local killed=0 note="" invalid=0
  if [ "$needs" = yes ]; then
    if ! _timed_build "$TMP/build.log"; then
      # A build failure is NOT automatically a kill. Distinguish two very different things:
      #
      #   * the type system (or a proof) REJECTED the mutation — a real and strong result,
      #     stronger than a gate going red, because the defect is unrepresentable; versus
      #   * the mutation is simply BROKEN — most often an unused binding, which this project
      #     treats as an error, so replacing `emit (...)` with `pure ()` or dropping a use of
      #     `len`/`mnem`/`hintR` fails to compile for a reason that has nothing to do with the
      #     rule under test.
      #
      # The second case reported KILLED four times this week. Each time the family passed
      # while testing NOTHING, and once the gate silently ran the previous binary. A harness
      # that cannot tell "unrepresentable" from "my patch is malformed" manufactures exactly
      # the false green it exists to prevent.
      # ORDER MATTERS, and getting it wrong is easy: a genuine rejection often emits a lint
      # warning ALONGSIDE the real error (removing the div range check breaks
      # `div_obligation_necessary` AND leaves a simp argument unused). Checking for the lint
      # first therefore misclassifies real kills as invalid — which my first version did,
      # caught by self-testing both directions instead of only the one I was fixing.
      #
      # So: look for a GENUINE error first. Only a failure that is lint-and-nothing-else is
      # an invalid mutation.
      # ATTRIBUTED WITHIN THE MUTATED FILE'S OWN DIAGNOSTIC BLOCK.
      #
      # This was two INDEPENDENT whole-log searches — "is there a genuine error anywhere" and, inside
      # it, "is there an error header naming this file anywhere". Both can be satisfied by different
      # diagnostics: a harmless lint on the target file plus an unrelated file's type mismatch scored
      # as a kill for this mutation. The two facts must come from the SAME diagnostic.
      #
      # A Lean diagnostic is a `path:line:col: error:` header followed by an indented message, so the
      # reason legitimately appears on later lines. This keeps the blocks whose header names the
      # mutated file — header until the next header — and searches only inside them.
      if awk -v f="$file" '
           { h = $0; hdr = 0
             # Two header layouts. `path:line:col: error: msg` is the Lean layout; Lake prints
             # `error: path:line:col: msg`. Recognising only the first meant no build kill was
             # ever attributed, and every proof-rejected mutation scored INVALID "for an
             # unrecognised reason" (four campaign families, measured 2026-10-03).
             if (h ~ /^(error|warning|info): [^ ]+:[0-9]+:[0-9]+: /) { sub(/^(error|warning|info): /, "", h); hdr = 1 }
             else if (h ~ /^[^ ].*:[0-9]+:[0-9]+: (error|warning)/) hdr = 1
             if (hdr) { sub(/^(\.\/)+/, "", h); inblk = (index(h, f ":") == 1) } }
           inblk { print }
         ' "$TMP/build.log" \
           | grep -qE "unsolved goals|[Tt]ype mismatch|Unknown identifier|Unknown constant|\
failed to synthesize|Missing cases|declaration uses 'sorry'"; then
        killed=1
        note="(killed by build — the type system or a proof rejected the mutation in $file)"
      elif grep -qE "unused variable|This simp argument is unused|unused binding" "$TMP/build.log"; then
        invalid=1
        note="(INVALID mutation — build failed on an unused-binding lint, not on the rule; \
rewrite it so every binding stays live, e.g. change an operand rather than deleting a call)"
      else
        invalid=1
        note="(INVALID mutation — build failed for an unrecognised reason; inspect the log \
rather than counting it as a kill)"
      fi
    fi
  fi
  if [ "$invalid" -eq 1 ]; then
    echo "  FAIL $nm $note"; FAIL=$((FAIL+1)); INVALID=$((INVALID+1))
    # EVERY EXIT PUBLISHES ITS RECORD, INCLUDING THIS ONE.
    #
    # This path returned without calling publish_evidence, so a CORRECTLY detected invalid mutation
    # lost its build transcript — the one artifact anyone would need to diagnose it — and then
    # appeared to the supervisor census as a family that ran and left no evidence. A full campaign
    # containing a single invalid family would have been refused for missing evidence rather than
    # reported as having found an invalid experiment.
    publish_evidence "$nm" "$i" "$file" "${GATE[$i]}" "$killed" "$invalid" "$note"
    restore_work "$file"
    return
  fi
  if [ "$killed" -eq 0 ]; then
    _gate_rc=0
    _timed_gate "$gate" "$TMP/gate.log" || _gate_rc=$?
    if [ "$_gate_rc" -eq 0 ]; then
      note="(SURVIVED — gate stayed green)"
    else
      # THE GATE MAY HAVE FAILED BECAUSE IT BUILT. Many gates call `require_fresh_binary`, which runs
      # `lake build`, so a family declared BUILD=no can still die of a compile error — and the
      # diagnostic classifier above only runs for BUILD=yes. Every such death was blindly credited as
      # "killed by <gate>" although the gate's assertions need never have executed. The gate log is
      # therefore checked for a compile failure attributable to the mutated file before the kill is
      # attributed to the gate.
      # THE GATE'S FAILURE MUST BE AUTHENTICATED BEFORE IT COUNTS AS RULE EVIDENCE.
      #
      # Three ways a nonzero exit can mean something other than "this rule is load-bearing":
      #
      #  1. The gate refused to START. Gates that call `require_fresh_binary` exit before running any
      #     assertion when the build fails, the binary is missing, there is no toolchain, or the
      #     repository is busy — and the build output is filtered to `^error` lines, so the compiler
      #     diagnostic naming the mutated file never appears in this log. A previous attempt to detect
      #     that by scanning for the diagnostic was INERT for exactly this reason. `fresh.sh` now emits
      #     a stable `GATE-PRECONDITION-FAILED:` marker instead.
      #  2. The gate built the mutated source itself and the compile failed — detectable as a
      #     diagnostic block naming the mutated file.
      #  3. Infrastructure: ENOSPC, a missing gate script, a shell syntax error, an exhausted inode
      #     table. Any of these produced a nonzero exit and was scored as a rule kill. The check is a
      #     positive one: a gate that produced a verdict line on PRISTINE source must produce one here
      #     too, otherwise it did not get far enough to disagree with anything.
      # THE SHARED PREDICATE IS CALLED HERE TOO. I claimed both legs used it and only the confirming
      # leg did — this one still carried its own inline copy, and the two had ALREADY diverged (only
      # this one classified attributable in-gate build failures). One question, one implementation.
      #
      # A precondition failure is never rule evidence, DECLARED or not: a declaration says "the type
      # system rejecting this mutation is the intended outcome", not "any infrastructure failure will
      # do".
      # ORDER: the SPECIFIC evidence first. An in-gate compile failure attributable to the mutated file
      # is a real (build) kill, and it typically also fails the generic predicate — because a gate that
      # died compiling never reaches its usual end. Testing the predicate first would therefore reclass
      # legitimate build kills as INVALID. Fail-closed either way, but wrong, and it would push the
      # operator to declare families that do not need declaring.
      if awk -v f="$file" '
           { h = $0; hdr = 0
             # Two header layouts. `path:line:col: error: msg` is the Lean layout; Lake prints
             # `error: path:line:col: msg`. Recognising only the first meant no build kill was
             # ever attributed, and every proof-rejected mutation scored INVALID "for an
             # unrecognised reason" (four campaign families, measured 2026-10-03).
             if (h ~ /^(error|warning|info): [^ ]+:[0-9]+:[0-9]+: /) { sub(/^(error|warning|info): /, "", h); hdr = 1 }
             else if (h ~ /^[^ ].*:[0-9]+:[0-9]+: (error|warning)/) hdr = 1
             if (hdr) { sub(/^(\.\/)+/, "", h); inblk = (index(h, f ":") == 1) } }
           inblk { print }
         ' "$TMP/gate.log" \
           | grep -qE "unsolved goals|[Tt]ype mismatch|Unknown identifier|Unknown constant|\
failed to synthesize|Missing cases|declaration uses 'sorry'"; then
        killed=1
        note="(killed by build INSIDE ${GATE[$i]} — the gate's own assertions did not run)"
        KILLED_BY_BUILD=$((KILLED_BY_BUILD+1))
        build_kill_declared "$nm" || UNDECLARED_BUILD_KILLS="$UNDECLARED_BUILD_KILLS $nm"
      elif ! _red_leg_is_gate_evidence "$TMP/gate.log" "$_gate_rc" "${GATE[$i]}"; then
        # The SHARED predicate — died-early, precondition failure, or never reached its own end.
        invalid=1
        note="(INVALID — ${GATE[$i]} did not produce a gate verdict: $_RED_LEG_WHY)"
      else
        # COMPLETION IS NOT CAUSATION — the kill is CONFIRMED against the same gate on restored source.
        #
        # Reaching the expected final-line shape proves the gate ran to a verdict; it says nothing
        # about WHY the verdict was red. Gates can turn an unrelated failure into an ordinary failed
        # assertion and still finish normally — `check_dependency_edges.sh` swallows a `lake env lean`
        # failure with `|| true` and reports it as a `no`, then prints its usual summary — so a
        # transient probe, tool or disk failure after the baseline had the right shape and was scored
        # as a mutation kill.
        #
        # The paired control is adjacent rather than up front: restore the file, rebuild if this family
        # needs one, and run the SAME gate again. Red with the mutation and green without it, measured
        # minutes apart, is causal evidence. Still red without it means the gate is red for reasons of
        # its own and this family proves nothing. The cost is one extra gate run (and one rebuild for
        # BUILD=yes families) per killed family, which is the right price for the difference between
        # "went red" and "went red BECAUSE of this".
        # RED, GREEN, RED — the failure must REPRODUCE, not merely coincide.
        #
        # One red-with and one green-without is not causal evidence: a one-shot transient during the
        # mutated run, gone by the restored run, produces exactly that pattern. And the green step must
        # itself be authenticated, or an unrelated early death during it would read as "still red".
        # So the sequence is: restore (must go GREEN and reach its own end), re-apply, and run again
        # (must go RED again). A transient would have to fire twice, in the mutated runs only.
        _confirm_ok=1
        if ! restore_work "$file"; then
          invalid=1; note="(INVALID — could not restore $file to confirm the kill)"; _confirm_ok=0
        fi
        if [ "$_confirm_ok" = "1" ] && [ "$needs" = yes ]; then
          _timed_build "$TMP/confirm_build.log" || _confirm_ok=0
        fi
        if [ "$_confirm_ok" = "1" ]; then
          _clean_rc=0
          _timed_gate "$gate" "$TMP/confirm_clean.log" || _clean_rc=$?
          if [ "$_clean_rc" -ne 0 ] || ! _reached_own_end "$TMP/confirm_clean.log" "${GATE[$i]}"; then
            invalid=1
            note="(INVALID — ${GATE[$i]} did not go cleanly green after restoring $file, so its \
failure under mutation is not attributable to the mutation)"
            _confirm_ok=0
          fi
        fi
        if [ "$_confirm_ok" = "1" ]; then
          if ! apply "$WORK/$file" "$TMP/old" "$TMP/new" 2>"$TMP/aerr2"; then
            invalid=1; note="(INVALID — could not re-apply the mutation to reproduce the kill)"; _confirm_ok=0
          fi
        fi
        if [ "$_confirm_ok" = "1" ] && [ "$needs" = yes ]; then
          # NOT `|| true`. Ignoring this rebuild meant the second red leg could be produced by a
          # FAILED BUILD rather than by the gate — and the leg below then accepted any nonzero exit
          # without the exit-97, precondition, or end-shape checks the FIRST red leg gets. The
          # confirmation was weaker than the thing it was confirming.
          _timed_build "$TMP/confirm_build2.log" || {
            invalid=1
            note="(INVALID — the mutated tree did not rebuild for the confirming red leg, so the \
kill could not be reproduced under the same conditions)"
            _confirm_ok=0
          }
        fi
        if [ "$_confirm_ok" = "1" ]; then
          _red2_rc=0
          _timed_gate "$gate" "$TMP/confirm_red.log" || _red2_rc=$?
          if [ "$_red2_rc" -eq 0 ]; then
            invalid=1
            note="(INVALID — ${GATE[$i]} was GREEN when the mutation was re-applied, so the earlier \
red was not reproducible and is not rule evidence)"
          elif ! _red_leg_is_gate_evidence "$TMP/confirm_red.log" "$_red2_rc" "${GATE[$i]}"; then
            # The SAME authentication the first red leg gets: a reproduced failure must be the gate
            # disagreeing, not a precondition failure, an early death, or a run that never finished.
            invalid=1
            note="(INVALID — the confirming red leg of ${GATE[$i]} was not a gate verdict: \
$_RED_LEG_WHY)"
          else
            killed=1
            note="(killed by ${GATE[$i]}; reproduced red/green/red)"
            KILLED_BY_GATE=$((KILLED_BY_GATE+1))
          fi
        fi
      fi
    fi
  else
    # Killed by the build before the gate ran. Counted separately: the family's named gate was never
    # exercised, so this result must not be cited as evidence that THAT gate is load-bearing.
    KILLED_BY_BUILD=$((KILLED_BY_BUILD+1))
    if ! build_kill_declared "$nm"; then
      UNDECLARED_BUILD_KILLS="$UNDECLARED_BUILD_KILLS $nm"
      note="$note (UNDECLARED build kill — ${GATE[$i]} never ran, so it is NOT shown load-bearing)"
    fi
  fi
  if ! restore_work "$file"; then
    publish_evidence "$nm" "$i" "$file" "${GATE[$i]}" "${killed:-0}" 1 "restore FAILED after the experiment — workspace state is not trustworthy"
    return 1
  fi
  # Where this family's wall-clock went, so the aggregate below can be trusted and a single slow
  # family is visible rather than averaged away.
  printf '  [t] family=%ds build=%ds gate=%ds\n' \
    "$(( $(_sw_now) - ${_fam_t0:-0} ))" "${FAMILY_SECS[build]:-0}" "${FAMILY_SECS[gate]:-0}"
  if [ "$killed" -eq 1 ]; then
    echo "  ok   $nm KILLED $note"; PASS=$((PASS+1))
  else
    echo "  FAIL $nm $note"; FAIL=$((FAIL+1))
    # A SURVIVOR AND AN INVALID ARE NOT THE SAME FINDING. Survived = the gate ran, reached its verdict,
    # and stayed green: a real coverage gap. Invalid = the experiment did not establish anything.
    if [ "$invalid" -eq 1 ]; then INVALID=$((INVALID+1)); else SURVIVED_N=$((SURVIVED_N+1)); fi
  fi
  # The record is published here for the families that reached a verdict, and at every early
  # return above for those that did not. One producer, called from every exit.
  publish_evidence "$nm" "$i" "$file" "${GATE[$i]}" "$killed" "$invalid" "$note"
}

echo "=== gate mutation coverage: $N families ==="
# The baseline is taken here — after the workspace copy, before any mutation. For a single-family run
# only that family's gate needs a baseline, so the 33-gate sweep is skipped in favour of the one.
# VALIDATED BEFORE INDEXING. `FAMILY=0` computes index -1, which bash resolves to the LAST element of
# an indexed array — so the run executed family 81 while reporting `single_family_selected(0)`, and
# exited zero. A non-numeric value would index 0 after arithmetic coercion.
# SELECTION BY STABLE IDENTITY, NOT BY POSITION.
#
# An ordinal names a slot, not an experiment: inserting a family renumbers everything after it, so a
# command, a golden baseline or a recorded verdict that said FAMILY=85 silently begins selecting a
# DIFFERENT mutation. The evidence records already carry `selector=FAMILY=n` for reproduction, and
# that reproduction is only sound while the inventory is unchanged — which is exactly the assumption
# that fails when someone adds a family.
#
# FAMILY_ID names the family, and FAMILY_SPEC optionally pins the digest of the mutation it must
# still be — file, gate, build requirement, and the exact old/new text. A family renamed keeps its
# spec; a family whose MUTATION changed no longer answers to the old digest. The ordinal form is
# still accepted, because a human reading a list wants to type a number, but it is resolved against
# the same identity and reported alongside it.
if [ -n "${FAMILY_ID:-}" ]; then
  _fid_idx=""
  for (( _i=0; _i<N; _i++ )); do
    [ "${NAME[$_i]}" = "$FAMILY_ID" ] || continue
    _fid_idx="$_i"; break
  done
  if [ -z "$_fid_idx" ]; then
    echo "FATAL: FAMILY_ID='$FAMILY_ID' names no family in this inventory of $N." >&2
    echo "       Selection is by identity: a renamed or removed family must be named, not guessed." >&2
    _gate_lock_release; exit 2
  fi
  if [ -n "${FAMILY_SPEC:-}" ]; then
    _fid_spec="$(family_spec_digest "${NAME[$_fid_idx]}" "${FILE[$_fid_idx]}" "${GATE[$_fid_idx]}" \
                                    "${BUILD[$_fid_idx]}" "${OLD[$_fid_idx]}" "${NEW[$_fid_idx]}")"
    if [ "$_fid_spec" != "$FAMILY_SPEC" ]; then
      echo "FATAL: FAMILY_ID='$FAMILY_ID' resolves, but its mutation spec is $_fid_spec, not the pinned $FAMILY_SPEC." >&2
      echo "       The family kept its name and changed what it does; a caller pinning the spec is" >&2
      echo "       asking for the experiment, not the label. Update the pin deliberately." >&2
      _gate_lock_release; exit 2
    fi
  fi
  ONLY=$(( _fid_idx + 1 ))
  echo "selected by identity: $FAMILY_ID (ordinal $ONLY of $N)"
fi
if [ -n "$ONLY" ]; then
  case "$ONLY" in
    ''|*[!0-9]*) echo "FATAL: FAMILY must be a positive integer, got '$ONLY'." >&2; _gate_lock_release; exit 2 ;;
  esac
  if [ "$ONLY" -lt 1 ] || [ "$ONLY" -gt "$N" ]; then
    echo "FATAL: FAMILY=$ONLY is out of range 1..$N." >&2; _gate_lock_release; exit 2
  fi
  _only_gate="${GATE[$((ONLY-1))]}"
  echo "=== clean-gate baseline: 1 gate (single-family selection) ==="
  # The single-family path needs the pristine build too: its one gate may do no build at all, in which
  # case the first build of the run would be the mutated one.
  baseline_pristine_build || { write_summary 0 " pristine_build_failed"; rm -rf "$TMP"; _gate_lock_release; exit 2; }
  BASELINE_TOTAL=1
  if _timed_gate "scripts/tests/$_only_gate" "$TMP/clean.log"; then
    CLEAN_GATE[$_only_gate]=yes; BASELINE_GREEN=1; BASELINE_RED=0
    keep_baseline_log "$_only_gate" "$TMP/clean.log"
    _note_freshness_taint "$TMP/clean.log"
    CLEAN_GATE_TAIL[$_only_gate]="$(_tail_shape "$TMP/clean.log")"
    if [ -n "${CLEAN_GATE_TAIL[$_only_gate]}" ]; then CLEAN_GATE_VERDICT[$_only_gate]=yes
    else CLEAN_GATE_VERDICT[$_only_gate]=no; fi
    echo "  baseline: 1/1 gates green on the unmutated workspace"
  else
    CLEAN_GATE[$_only_gate]=no; BASELINE_GREEN=0; BASELINE_RED=1
    # A RED baseline transcript is the MOST important one to keep: every family on that gate is
    # reported INVALID because of it.
    keep_baseline_log "$_only_gate" "$TMP/clean.log"
    echo "  RED ON CLEAN: $_only_gate — this family will be reported INVALID, not killed"
    echo "  baseline: 0/1 gates green on the unmutated workspace"
  fi
  # The single-family path needs the same post-baseline compiler identity as the full run: its one
  # baseline gate can build too, so the compiler captured at copy time is not necessarily the one this
  # family was judged against.
  TESTED_BIN="$(_bin_sha "$WORK")"
  if [ "$TESTED_BIN" = "absent" ]; then
    echo "error: no compiler in the workspace after the baseline — nothing could have been tested." >&2
    write_summary 0 " workspace_compiler_absent"
    rm -rf "$TMP"; _gate_lock_release; exit 2
  fi
  # Re-measured after the baseline for the same reason as the full path: the one baseline gate may
  # delete untracked paths in the copy.
  WORK_UNTRACKED="$(ts_untracked "$WORK")"; _require_measured "$WORK_UNTRACKED" untracked
  WORK_TRACKED="$(ts_tracked "$WORK")";       _require_measured "$WORK_TRACKED" tracked
  run_one "$((ONLY-1))"
else
  baseline_pristine_build || { write_summary 0 " pristine_build_failed"; rm -rf "$TMP"; _gate_lock_release; exit 2; }
  baseline_all_gates
  # The compiler the families are judged against exists only now: the baseline may have built it.
  TESTED_BIN="$(_bin_sha "$WORK")"
  if [ "$TESTED_BIN" = "absent" ]; then
    echo "error: no compiler in the workspace after the baseline — nothing could have been tested." >&2
    write_summary 0 " workspace_compiler_absent"
    rm -rf "$TMP"; _gate_lock_release; exit 2
  fi
  # RE-MEASURED AFTER THE BASELINE, because baseline gates CHANGE the workspace they run in.
  #
  # This is live, not hypothetical: `check_receipt_consumption.sh` removes its probe file, and
  # `run_tests.sh` deletes every extensionless file directly under `tests/programs`. Those paths are
  # not generally ignored, so untracked bytes present at copy time can be gone before the first
  # mutation runs — while the artifact still named them as the workspace's untracked state. Start/end
  # reconciliation cannot see this: it inspects the REAL repository, not the copy.
  WORK_UNTRACKED="$(ts_untracked "$WORK")"; _require_measured "$WORK_UNTRACKED" untracked
  WORK_TRACKED="$(ts_tracked "$WORK")";       _require_measured "$WORK_TRACKED" tracked
  # Arithmetic loop, not `seq`, for the same reason as the anchor loop above.
  for (( i=0; i<N; i++ )); do run_one "$i"; done
fi

# leave a clean binary behind
# NO REBUILD IS OWED FOR MUTATION REPAIR. Mutations are applied in a disposable copy, so the real
# `.lake` never held objects compiled from mutated source — which is what the in-place design needed a
# rebuild to undo.
#
# BUT THIS IS STILL A REAL BUILD IN THE REAL TREE, and saying otherwise would overclaim: `lake build`
# below runs against $ROOT_DIR and may write to the real `.lake` (relink, refreshed oleans). It
# touches no tracked SOURCE, and that is the isolation claim — not that the campaign leaves no trace
# at all. The check is a genuine one and needs a genuine build: it asserts the real build artifacts
# still correspond to the real sources, so if it fails, isolation was bypassed somewhere.
if "$LAKE" build >/dev/null 2>&1; then
  echo "--- working tree still builds clean (isolation left it untouched) ---"
else
  echo "  FAIL the working tree does not build after an isolated mutation run — isolation was bypassed"
  FAIL=$((FAIL+1))
fi

REACHED_END=1
RUN_SECS=$(( $(date +%s) - RUN_T0 ))

# ---------------------------------------------------------------------------
# CAMPAIGN INTEGRITY. Each dimension that moved is NAMED, because "completed=0" with no reason is
# the same failure shape as a run that dies quietly.
#
# RECONCILED BEFORE THE SUMMARY LINE IS PRINTED. This block used to run AFTER
# `GATE-MUTATION-COVERAGE: PASS=.. FAIL=..`, so a refused campaign printed a clean PASS line and then
# exited nonzero — the exact summary-vs-exit disagreement that hid 17 stranded assertions in
# check_clean_checkout for two days. Consumers that scrape the summary line must not be able to read
# PASS off a run whose completion was refused.
REFUSALS=""
# COMPARED AGAINST THE EXECUTED BYTES, not against a second hash of the live file taken at start.
if [ "$EXECUTED_DRIVER_SHA" = "unknown" ]; then
  REFUSALS="$REFUSALS driver_snapshot_unverifiable"
elif [ "$(snap_driver)" != "$EXECUTED_DRIVER_SHA" ]; then
  REFUSALS="$REFUSALS driver_changed"
fi
# An in-band unavailability marker must REFUSE, not be compared. Two identical markers would
# otherwise satisfy the equality checks below exactly like two identical digests.
case "$START_HEAD$START_TRACKED$START_UNTRACKED" in
  *TREESTATE-UNAVAILABLE*) REFUSALS="$REFUSALS tree_state_unavailable_at_start" ;;
esac
# EMPTY IS NOT A DIGEST. Rejecting only the literal marker still let two EMPTY values compare equal
# and authorise completion — the exact empty-equals-empty path the marker exists to close, reached by
# a different route (a failed hasher, a partial pipeline).
for _v in "$START_HEAD" "$START_TRACKED" "$START_UNTRACKED"; do
  [ -n "$_v" ] || { REFUSALS="$REFUSALS tree_state_empty_at_start"; break; }
done
for _v in "$(snap_head)" "$(snap_tracked)" "$(snap_untracked)"; do
  [ -n "$_v" ] || { REFUSALS="$REFUSALS tree_state_empty_at_end"; break; }
done
case "$(snap_head)$(snap_tracked)$(snap_untracked)" in
  *TREESTATE-UNAVAILABLE*) REFUSALS="$REFUSALS tree_state_unavailable_at_end" ;;
esac
[ "$(snap_inventory)" = "$START_INV" ]       || REFUSALS="$REFUSALS mutation_inventory_changed"
[ "$(snap_head)"      = "$START_HEAD" ]      || REFUSALS="$REFUSALS head_changed"
[ "$(snap_tracked)"   = "$START_TRACKED" ]   || REFUSALS="$REFUSALS tracked_tree_changed"
[ "$(snap_untracked)" = "$START_UNTRACKED" ] || REFUSALS="$REFUSALS untracked_tree_changed"
# mutation_target_dirty_at_start is NOT re-checked here: the start guard above already refused and
# exited, so this point is only reachable with START_DIRTY empty. A check that cannot fail is not a
# check, and leaving it here would overstate how many dimensions this block actually covers.
END_DIRTY="$(snap_dirty_targets)"
[ -z "$END_DIRTY" ]                          || REFUSALS="$REFUSALS mutation_target_not_restored($END_DIRTY)"
[ "$CAMPAIGN_INTERRUPTED" = "0" ]            || REFUSALS="$REFUSALS run_interrupted"
[ "$REACHED_END" = "1" ]                     || REFUSALS="$REFUSALS did_not_reach_end"
[ "${WORK_DIRTY:-0}" = "0" ]                 || REFUSALS="$REFUSALS workspace_restore_failed"
# A family that died in the build without declaring that as its intended outcome leaves its named gate
# unproven, and the campaign's whole purpose is proving those gates load-bearing.
[ -z "$UNDECLARED_BUILD_KILLS" ]             || REFUSALS="$REFUSALS undeclared_build_kills($UNDECLARED_BUILD_KILLS )"
# A gate that ran against an unverified binary taints the whole campaign: its verdict describes an
# artifact nobody established corresponds to the source. `CONCRETE_ALLOW_UNVERIFIED_BINARY=1` only
# warned, which is indistinguishable from verified evidence to anything reading logs or artifacts.
[ "${FRESHNESS_UNVERIFIED:-0}" = "0" ]       || REFUSALS="$REFUSALS compiler_freshness_unverified"
[ "$PREAMBLE_DRIVER_SHA" = "$EXECUTED_DRIVER_SHA" ] \
  || REFUSALS="$REFUSALS driver_changed_during_preamble($PREAMBLE_DRIVER_SHA->$EXECUTED_DRIVER_SHA)"
# EVERY FAMILY MUST BE ACCOUNTED FOR, and a bare PASS may not paper over a gap. This file's stated
# purpose is that each family's SPECIFIC gate goes red. A DECLARED build kill is a different and
# legitimate kind of evidence — the mutation is unrepresentable — but it leaves that family's gate
# unproven, so the two must add up: gate-proven families plus declared build kills must equal the
# whole corpus. Anything else means some family produced neither kind of evidence while the run still
# reported PASS.
if [ -z "$ONLY" ]; then
  _accounted=$(( ${KILLED_BY_GATE:-0} + ${KILLED_BY_BUILD:-0} ))
  [ "$_accounted" = "$N" ] \
    || REFUSALS="$REFUSALS families_unaccounted(gate=${KILLED_BY_GATE:-0} build=${KILLED_BY_BUILD:-0} of $N)"
fi
# A partial campaign is not a campaign. FAMILY=<n> selects one deliberately and cannot complete.
EXPECTED_RUN="$N"; [ -n "$ONLY" ] && EXPECTED_RUN=1
# THE DENOMINATOR IS VERDICTS, NOT VISITS. `FAMILIES_RUN` is incremented unconditionally at the top of
# `run_one` and the loop is `seq 0 $((N-1))`, so `FAMILIES_RUN == N` was ARITHMETICALLY guaranteed and
# the check could not fail — an inert control, the same shape as the dirty-target copy removed above.
# `PASS + FAIL` is not guaranteed: it holds only because every one of `run_one`'s exit paths records a
# verdict. A `return` added later without one silently shrinks the denominator, which is precisely the
# "discovered but not executed" failure this dimension exists to catch. FAMILIES_RUN stays in the
# artifact as reported data, not as an assertion.
VERDICTS=$(( PASS + FAIL ))
# Every SELECTED unit reported a verdict. This is what `completed` asserts — not that they all passed.
# EVERY REPORTED FAMILY MUST HAVE LEFT A RECORD. `reported` counts verdicts printed; this counts
# transcripts a reader can actually open. If they disagree, the run claims more than it can show.
[ -z "$EVIDENCE_FAILED" ] || REFUSALS="$REFUSALS evidence_unwritable($(echo $EVIDENCE_FAILED | tr ' ' ','))"
[ "$EVIDENCE_WRITTEN" = "$(( ${PASS:-0} + ${FAIL:-0} ))" ] \
  || REFUSALS="$REFUSALS evidence_missing($EVIDENCE_WRITTEN/$(( ${PASS:-0} + ${FAIL:-0} )))"
# EXACT SET RECONCILIATION OVER FAMILY IDS, NOT A COUNT.
#
# Counting records reconciles a number: an unknown directory can stand in for a missing expected
# family and the total still agrees. What must reconcile is the SET — the families this run SELECTED
# against the families that left a record — with missing, unexpected and duplicate each named. The
# counts are then DERIVED from that reconciled set rather than compared to it.
#
# The record contract is checked per family, not once: a record must name its own directory, a
# gate-route kill must carry the transcripts that show red/green/red (gate.log, confirm_clean.log,
# confirm_red.log), a build-route kill must carry its build transcript, and a family that needed no
# build must not carry one — that last being the shape cross-family log contamination takes.
# THE EXPECTED SET COMES FROM THE SELECTION RULE ITSELF, so it cannot drift from what ran: a single
# family selects one id, a campaign selects the whole inventory.
_ev_expected=""
if [ -n "${ONLY:-}" ]; then
  _ev_expected="${NAME[$((ONLY-1))]}
"
else
  for (( _i=0; _i<N; _i++ )); do _ev_expected="$_ev_expected${NAME[$_i]}
"; done
fi
_ev_expected="$(printf '%s' "$_ev_expected" | LC_ALL=C sort)"

_ev_found=""
_census_bad=""
if [ -d "$EVIDENCE_DIR" ]; then
  for _d in "$EVIDENCE_DIR"/*/; do
    _n="$(basename "$_d")"; [ "$_n" != "_baseline" ] || continue
    [ -e "$_d" ] || continue
    _ev_found="$_ev_found$_n
"
    if [ ! -s "$_d/verdict.txt" ]; then
      _census_bad="$_census_bad $_n(no-verdict)"; continue
    fi
    # THE RECORD MUST NAME ITS OWN DIRECTORY: a record written under another family's name is what a
    # swapped or duplicated publication looks like, and a count cannot see it.
    _rfam="$(sed -n 's/^family=//p' "$_d/verdict.txt" | head -1)"
    [ "$_rfam" = "$_n" ] || _census_bad="$_census_bad $_n(record-names-${_rfam:-nothing})"
    _rdisp="$(sed -n 's/^disposition=//p' "$_d/verdict.txt" | head -1)"
    _rroute="$(sed -n 's/^expected_route=//p' "$_d/verdict.txt" | head -1)"
    _rbuild="$(sed -n 's/^build_required=//p' "$_d/verdict.txt" | head -1)"
    if [ "$_rdisp" = "killed" ] && [ "$_rroute" = "gate" ]; then
      # RED/GREEN/RED IS THREE TRANSCRIPTS. Requiring only gate.log lets a kill be published with no
      # evidence that the gate went green again on restored source, which is the leg that makes the
      # red attributable to the mutation rather than to the workspace.
      for _need in gate.log confirm_clean.log confirm_red.log; do
        [ -s "$_d/$_need" ] || _census_bad="$_census_bad $_n(gate-kill-without-$_need)"
      done
    fi
    if [ "$_rdisp" = "killed" ] && [ "$_rroute" = "build" ] && [ ! -s "$_d/build.log" ]; then
      _census_bad="$_census_bad $_n(build-kill-without-build-log)"
    fi
    if [ "$_rbuild" = "no" ] && [ -e "$_d/build.log" ]; then
      _census_bad="$_census_bad $_n(build.log-in-a-no-build-family)"
    fi
  done
fi
_ev_found="$(printf '%s' "$_ev_found" | LC_ALL=C sort)"

# MISSING, UNEXPECTED AND DUPLICATE, EACH BY NAME.
_ev_missing="$(comm -23 <(printf '%s\n' "$_ev_expected") <(printf '%s\n' "$_ev_found" | LC_ALL=C sort -u) | grep -c . || true)"
[ "$_ev_missing" = "0" ] || REFUSALS="$REFUSALS evidence_missing_families($(comm -23 <(printf '%s\n' "$_ev_expected") <(printf '%s\n' "$_ev_found" | LC_ALL=C sort -u) | tr '\n' ',' ))"
_ev_unexpected="$(comm -13 <(printf '%s\n' "$_ev_expected") <(printf '%s\n' "$_ev_found" | LC_ALL=C sort -u) | grep -c . || true)"
[ "$_ev_unexpected" = "0" ] || REFUSALS="$REFUSALS evidence_unexpected_families($(comm -13 <(printf '%s\n' "$_ev_expected") <(printf '%s\n' "$_ev_found" | LC_ALL=C sort -u) | tr '\n' ',' ))"
_ev_dupes="$(printf '%s\n' "$_ev_found" | LC_ALL=C sort | uniq -d | tr '\n' ',')"
[ -z "$_ev_dupes" ] || REFUSALS="$REFUSALS evidence_duplicate_families($_ev_dupes)"

# COUNTS DERIVED FROM THE RECONCILED SET, not compared against it.
_census="$(printf '%s\n' "$_ev_found" | grep -c . || true)"
[ -z "$_census_bad" ] || REFUSALS="$REFUSALS evidence_census_bad($_census_bad )"
# RESTORED. keep_baseline_log records BASELINE_EVIDENCE_FAILED, and the refusal that consumed it was
# dropped when this census block was rewritten — so a silently unwritable baseline transcript went
# back to being invisible, which is the exact defect that check was added to close.
[ -z "${BASELINE_EVIDENCE_FAILED:-}" ] \
  || REFUSALS="$REFUSALS baseline_evidence_unwritable($(echo ${BASELINE_EVIDENCE_FAILED} | tr ' ' ','))"
[ "$_census" = "$EVIDENCE_WRITTEN" ] \
  || REFUSALS="$REFUSALS evidence_census_disagrees(on-disk=$_census counter=$EVIDENCE_WRITTEN)"

# THE EVIDENCE ROOT: a digest over canonical (family id, record digest) pairs. The supervisor
# revalidates this after the child exits, so validating one set of bytes and publishing a different,
# later-mutated set is no longer possible.
EVIDENCE_ROOT="$(evidence_root_digest "$EVIDENCE_DIR")"
# THE SET THIS RUN ACTUALLY HELD, digested from the inventory it built.
FAMILIES_DIGEST="$(family_set_digest "$(printf '%s\n' "${NAME[@]}")")"
REPORTED_ALL=0; [ "$VERDICTS" = "$EXPECTED_RUN" ] && REPORTED_ALL=1
[ "$VERDICTS" = "$EXPECTED_RUN" ]            || REFUSALS="$REFUSALS verdicts_missing($VERDICTS/$EXPECTED_RUN)"

# SCOPE IS NOT INTEGRITY, and conflating them broke a working gate. Everything above says "these
# verdicts cannot be trusted" — the tree moved, the driver moved, a restore failed, a signal arrived.
# `FAMILY=<n>` says something completely different: the operator deliberately asked for ONE family.
# That run must not claim `completed=1` (it is not a campaign), but its verdict is perfectly good, and
# forcing it to exit nonzero broke `check_phase6c_observability.sh:35`, which chains
# `FAMILY=5 ... && FAMILY=6 ...` as a cheap sample and expects success. A deliberate selection is not
# a corrupted run, so it suppresses completion WITHOUT failing the process.
SCOPE_NOTES=""
[ -z "$ONLY" ] || SCOPE_NOTES=" single_family_selected($ONLY)"
ALL_REFUSALS="$REFUSALS$SCOPE_NOTES"

# COMPLETED AND QUALIFIED ARE DIFFERENT FACTS, and conflating them recreated exactly the
# missing-summary ambiguity this harness exists to remove.
#
#   completed = this run reached reconciliation and every SELECTED unit reported a verdict.
#               It says the record is trustworthy, NOT that the corpus is discharged.
#   qualified = a full campaign, complete, integrity intact, and every family met its declared causal
#               route. It says the corpus IS discharged.
#
# The 81-family run on 898d9a7b executed and reported all 81 families and still said `completed=0`,
# because `FAIL -eq 0` was folded into completion. A complete report of eight unresolved claims is a
# GOOD artifact; calling it incomplete discards the distinction between "did not finish" and "finished
# and found problems". That was Codex round-12 finding 1 resolved on the wrong axis: the finding (a
# closure run could write completed=1 with failed gates) was real, and the fix should have been two
# fields rather than one stricter field.
INTEGRITY_OK=1; [ -z "$REFUSALS" ] || INTEGRITY_OK=0

COMPLETED=0
[ "$REACHED_END" = "1" ] && [ "${REPORTED_ALL:-0}" = "1" ] && COMPLETED=1

QUALIFIED=0
if [ "$COMPLETED" = "1" ] && [ "$INTEGRITY_OK" = "1" ] \
   && [ "${CAMPAIGN_MODE:-campaign}" = "campaign" ] \
   && [ "${PASS:-0}" = "${N:-0}" ] \
   && [ "${INVALID:-0}" -eq 0 ] && [ "${SURVIVED_N:-0}" -eq 0 ] && [ "${COULD_NOT_APPLY:-0}" -eq 0 ]; then
  QUALIFIED=1
fi
ARTIFACT_OK=1; write_summary "$COMPLETED" "$ALL_REFUSALS" || ARTIFACT_OK=0

# The process succeeds when the verdicts are sound and durably recorded: integrity intact, no failing
# family, artifact written. Deliberate scope does not enter this.
# EXIT FOLLOWS `qualified` ONLY IN CAMPAIGN MODE.
#
# A campaign's job is to discharge the corpus, so its exit tracks qualification. A single-family probe's
# job is to report one sound result, so it exits on THAT — otherwise every legitimate FAMILY=n consumer
# breaks. Neither mode may exit 0 without a durable record.
RUN_OK=1
[ "$ARTIFACT_OK" = "1" ] || RUN_OK=0
[ -z "$REFUSALS" ]       || RUN_OK=0
if [ "$CAMPAIGN_MODE" = "campaign" ]; then
  [ "$QUALIFIED" = "1" ] || RUN_OK=0
else
  # Sound selected result: it reported, and it was not an invalid experiment or a survivor.
  [ "$REPORTED_ALL" = "1" ] || RUN_OK=0
  [ "${INVALID:-0}" -eq 0 ] && [ "${SURVIVED_N:-0}" -eq 0 ] && [ "${COULD_NOT_APPLY:-0}" -eq 0 ] || RUN_OK=0
fi

echo
# GATE COVERAGE IS REPORTED SEPARATELY FROM KILL COUNT.
#
# This gate's stated purpose is that each family's SPECIFIC gate goes red. A build kill does not
# establish that — the named gate is skipped entirely — so counting it into a single PASS number let a
# run claim the contract was met for families whose gate never executed. Both numbers are now on the
# line and in the artifact: `killed` is how many mutations died, `gates_proven` is how many did so by
# turning their named gate red, which is the only figure that speaks to the contract.
GATES_PROVEN="${KILLED_BY_GATE:-0}/$N"
DISPO="killed=$PASS invalid=${INVALID:-0} survived=${SURVIVED_N:-0} could_not_apply=${COULD_NOT_APPLY:-0}"
if [ "$CAMPAIGN_MODE" = "campaign" ] && [ "$QUALIFIED" = "1" ]; then
  # THE CHILD DOES NOT DECLARE QUALIFICATION. IT PROPOSES IT.
  #
  # This printed "QUALIFIED" before the supervisor had run one of its independent checks — and
  # it is the line a human reads in a transcript. When the supervisor then refused and exited
  # nonzero, the transcript still said QUALIFIED: the summary and the exit status disagreed
  # about the one word that matters. Qualification is the supervisor's word.
  echo "GATE-MUTATION-COVERAGE: QUALIFIABLE completed=1 $DISPO (of $N) gates_proven=$GATES_PROVEN"
  echo "  (child verdict — only the supervisor's published artifact can say QUALIFIED)"
elif [ "$CAMPAIGN_MODE" = "campaign" ] && [ "$COMPLETED" = "1" ]; then
  # COMPLETE BUT NOT QUALIFYING is the honest shape of a run that finished and found problems. Saying
  # only "REFUSED" would hide that the report itself is trustworthy and every family was accounted for.
  echo "GATE-MUTATION-COVERAGE: COMPLETE-NOT-QUALIFIED completed=1 qualified=0 $DISPO (of $N) gates_proven=$GATES_PROVEN"
  [ -n "$REFUSALS" ] && echo "  integrity refusals:$REFUSALS"
elif [ "$RUN_OK" = "1" ]; then
  # Sound but deliberately partial: say so on the summary line rather than printing a bare PASS that
  # a consumer could read as a completed campaign.
  echo "GATE-MUTATION-COVERAGE: PARTIAL completed=$COMPLETED qualified=0 $DISPO (of $N) —$SCOPE_NOTES"
else
  # The summary line itself carries the refusal, so scraping it cannot yield a false PASS.
  echo "GATE-MUTATION-COVERAGE: REFUSED completed=$COMPLETED qualified=0 $DISPO (of $N) gates_proven=$GATES_PROVEN"
  echo "CAMPAIGN INTEGRITY REFUSED:$REFUSALS"
  echo "  These verdicts describe a tree that moved under the run, or a run with no durable record."
fi
if [ "$ARTIFACT_OK" = "1" ]; then
  if [ "${ANCHORS_ONLY:-0}" != "1" ]; then
    # NAMES THE FILE IT ACTUALLY WROTE. A partial run writes .partial and deliberately leaves the full
    # record alone, but this line always said ".mutation-campaign-summary" — so following it led a
    # reader to an OLDER full-campaign record and let it be read as this run's result.
    echo "summary written to ${WROTE_ARTIFACT_PATH:-<unrecorded>} (completed=$COMPLETED)"
    [ "${CAMPAIGN_PARTIAL:-0}" = "0" ] \
      || echo "  the full-campaign record .mutation-campaign-summary was NOT touched by this partial run"
  fi
else
  echo "error: the campaign artifact could NOT be written — this run has no completion record" >&2
fi

[ "$RUN_OK" = "1" ]
