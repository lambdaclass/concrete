#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

# Mutation testing for the Concrete compiler.
# Applies targeted source mutations, rebuilds, and checks if the test suite catches them.
# A surviving mutation = a test gap.
#
# Usage:
#   bash scripts/tests/test_mutation.sh              # run all mutations
#   bash scripts/tests/test_mutation.sh --list       # list mutations without running
#   bash scripts/tests/test_mutation.sh --check-patterns  # assert none would SKIP
#   bash scripts/tests/test_mutation.sh --mutation N # run only mutation N

# Resolve `lake`: PATH first (the nix devshell puts it there, as does elan's
# shim), then elan's default location. A hardcoded elan path reported every
# mutation as "KILLED (build)" inside `nix develop`, because a missing toolchain
# is indistinguishable from an ill-typed mutation once the build has failed —
# a harness that cannot build must not be able to claim kills.
LAKE="${LAKE:-$(command -v lake || true)}"
[ -n "$LAKE" ] || LAKE="$HOME/.elan/bin/lake"
if ! "$LAKE" --version >/dev/null 2>&1; then
  echo "error: no working 'lake' found (tried \$LAKE, PATH, ~/.elan/bin/lake)." >&2
  echo "hint: run inside the devshell, e.g. 'nix develop --command bash scripts/tests/test_mutation.sh'" >&2
  exit 2
fi

# EXCLUSIVE LOCK. This harness mutates source IN PLACE and restores from
# `<file>.mutbak`. Two concurrent runs in one worktree therefore clobber each
# other's backups: on 2026-07-30 a second run overwrote the first's .mutbak,
# leaving `keys.length == keys.length -- MUTATION` committed-adjacent in
# Concrete/Proof/Proof.lean, one run reporting "mv: cannot stat …mutbak" and
# another "SKIPPED (pattern not found)". Test machinery that can corrupt the tree
# it is testing must refuse to run twice, not rely on the operator remembering.
# KNOWN LIMIT, measured 2026-07-30: a TERM/INT arriving while a FOREGROUND CHILD
# runs (`lake build`, which dominates each mutation) does not restore promptly —
# bash defers the trap until the child exits, so the tree stays mutated for the
# rest of that build. Traps cannot fix this; only not touching the developer's
# tree can. The real answer is to run mutations in a DISPOSABLE WORKTREE, which is
# tracked and not yet built. Until then: the lock stops concurrent corruption, the
# hash postcondition refuses to exit quietly on an inexact restore, and a killed
# run must be followed by
#   grep -rn -- "-- MUTATION" Concrete && git checkout -- <files>
# THE REPOSITORY LOCK, TOO — not just this harness's own.
#
# `.mutation.lock` stops two mutation runs colliding, but it says nothing about the shared `.lake`
# tree and binary. This harness invokes named gates, and a gate that calls `require_fresh_binary`
# ACQUIRES `.gate.lock` and never releases it (the library deliberately installs no EXIT trap), so
# the SECOND named gate in a run refused with "REPOSITORY BUSY" — and this harness recorded that
# nonzero exit as `KILLED (gate)`. That is the manufactured-kill defect fixed in
# check_gate_mutation_coverage.sh on 2026-08-21, left live here because the two harnesses hold
# different locks. Acquiring the repository lock here makes every gate this harness invokes re-entrant
# by inheritance, so none of them creates a lock to leave behind.
#
# Held for the whole run and released explicitly on the cleanup path below, alongside .mutation.lock.
# READ-ONLY MODES TAKE NO LOCKS. `--list` and `--check-patterns` touch no files, and
# `check_mutation_anchors.sh` calls the latter as a cheap gate; making them contend for the mutation
# lock would turn a one-second read into a source of spurious "another mutation run holds" refusals.
# LAST OPTION WINS, exactly as the real parser below decides. Scanning for "any read-only flag"
# disagreed with it: `--check-patterns --mutation 1` skipped both locks and then entered MUTATING
# mode, editing real compiler source with no lock held at all.
MUT_READ_ONLY=0
for _a in "$@"; do
  case "$_a" in
    --list|--check-patterns) MUT_READ_ONLY=1 ;;
    --mutation|--all|-*) MUT_READ_ONLY=0 ;;
  esac
done

MUT_HELD_GATE_LOCK=0
if [ "$MUT_READ_ONLY" = "0" ]; then
  source "$ROOT_DIR/scripts/tests/lib/fresh.sh"
  _gate_lock_acquire || {
    echo "error: this harness mutates real compiler source and needs exclusive repository access." >&2
    exit 2
  }
  MUT_HELD_GATE_LOCK=1
fi

LOCK_DIR="$ROOT_DIR/.mutation.lock"
MUT_HELD_MUT_LOCK=0
if [ "$MUT_READ_ONLY" = "1" ]; then
  :
elif ! mkdir "$LOCK_DIR" 2>/dev/null; then
  [ "${MUT_HELD_GATE_LOCK:-0}" = "1" ] && _gate_lock_release
  echo "error: another mutation run holds $LOCK_DIR" >&2
  echo "       this harness edits source in place; concurrent runs corrupt it." >&2
  echo "       if no run is active, remove the directory and check for stray" >&2
  echo "       '-- MUTATION' markers: grep -rn 'MUTATION' Concrete/ --include='*.lean'" >&2
  exit 2
else
  # Recorded so cleanup releases only a lock THIS run created. Without this the flag stayed 0 and
  # cleanup never removed the lock at all, wedging every later run — the opposite failure.
  MUT_HELD_MUT_LOCK=1
fi
# Backups live in a unique temp dir, not beside the source: a `<file>.mutbak`
# sitting in the tree is itself a way to leave state behind, and two runs racing
# on the same path is what corrupted Proof.lean.
MUT_BACKUP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/concrete-mutation.XXXXXX")" || {
  echo "error: could not create the mutation backup directory" >&2; exit 2; }
[ -n "$MUT_BACKUP_DIR" ] && [ -d "$MUT_BACKUP_DIR" ] || {
  echo "error: mutation backup directory is not usable" >&2; exit 2; }
# EVIDENCE LOGS ARE PER-RUN, NOT GLOBAL. Build and gate output went to fixed /tmp/mutation_*.log
# paths, and the classifier below READS those paths to decide whether a kill is genuine. Two runs in
# separate worktrees — each correctly holding its own locks, since the locks are per-repository — would
# truncate and interleave the same file, letting one mutation be credited with the other run's
# diagnostic. The backup directory was already unique; the evidence-bearing logs were not.
# A SIBLING DIRECTORY, NOT A CHILD OF THE BACKUP TREE.
#
# The backup tree MIRRORS THE REPOSITORY LAYOUT, and cleanup walks every file in it treating the
# relative path as the target to restore. Putting logs inside it therefore made
# `$MUT_BACKUP_DIR/logs/build.log` look like a backup of `$ROOT_DIR/logs/build.log` — so if such a
# file existed in the repository, cleanup would overwrite it with a build log. That is a path outside
# the mutation-target set, so no preflight covered it. Introduced by my own round-5 fix for the
# global-log race; the log directory has to be somewhere cleanup does not interpret.
MUT_LOG_DIR="$(mktemp -d "${TMPDIR:-/tmp}/concrete-mutation-logs.XXXXXX")" || {
  echo "error: could not create the mutation log directory" >&2; exit 2; }
[ -n "$MUT_LOG_DIR" ] && [ -d "$MUT_LOG_DIR" ] || {
  echo "error: mutation log directory is not usable" >&2; exit 2; }

# Hashes of every target file, captured BEFORE any mutation. Restoration is
# verified against these, so "restored" means byte-identical rather than "the
# restore command ran".
declare -A MUT_HASH0=()
# Hash of the MUTATED content this harness wrote, per file. The difference
# between this and what is on disk at restore time is a THIRD PARTY's edit.
declare -A MUT_HASH_APPLIED=()
# Set when a restore was REFUSED because another writer owned the file. The run
# must then fail: a refused restore leaves the tree in a state nobody chose.
MUT_CONCURRENT=0
hash_of() { shasum -a 256 "$1" 2>/dev/null | cut -d' ' -f1; }

# Families for which a BUILD kill is the intended outcome, by description. Empty until a full run
# enumerates them: an undeclared build kill is reported as an ERROR rather than banked as coverage.
MUT_EXPECT_BUILD_KILL=""
build_kill_declared() { case " $MUT_EXPECT_BUILD_KILL " in *" $1 "*) return 0;; *) return 1;; esac; }

# A SIGNAL MUST NOT BE ABLE TO PRODUCE EXIT 0.
#
# This handler exits with the status it captured on entry, and bash DEFERS a signal until the running
# foreground child returns — so a TERM arriving during a successful build gives `$?` = 0 and the
# interrupted, INCOMPLETE run exits ZERO. An exit-code consumer reads that as a completed successful
# campaign. The signal is recorded by the trap itself, so the handler can tell the two apart.
MUT_SIGNALLED=0
cleanup_lock() {
  local rc=$?
  if [ "${MUT_SIGNALLED:-0}" = "1" ] && [ "$rc" -eq 0 ]; then
    rc=130   # terminated by a signal — never a verdict
  fi
  # The lock is released at the END, after restoration. Releasing it first let a
  # second run start while this one still had a mutation applied — the exact race
  # the lock exists to prevent.
  # POSTCONDITION, not a warning. A mutation left applied is worse than a failed
  # run: the next build, gate or commit silently uses it, and a green result then
  # describes mutated code. This must make the harness FAIL.
  local bad=0
  # RESTORE FIRST, then verify. Detecting a stray mutation and exiting leaves the
  # tree modified, which is the failure this trap exists to prevent — a signalled
  # run must not be able to leave the developer's source semantically changed.
  # Backups live in $MUT_BACKUP_DIR, so this works even on INT/TERM mid-mutation.
  if [ -d "$MUT_BACKUP_DIR" ]; then
    # The backup dir mirrors the repo layout, so the relative path IS the target —
    # no guessing, no flattening to invert.
    while IFS= read -r bak; do
      local rel="${bak#$MUT_BACKUP_DIR/}"
      # Skip rescued copies of a third party's work — they are evidence, not backups.
      # `.staging` files are half-written backups, never authoritative originals — see apply_mutation.
      case "$rel" in CONCURRENT-EDIT/*) continue ;; *.staging) continue ;; esac
      if [ -f "$ROOT_DIR/$rel" ]; then
        # Same non-clobber rule as restore_mutation: on INT/TERM we must not
        # "restore" over an edit that was never ours.
        local now applied
        now="$(hash_of "$ROOT_DIR/$rel")"
        applied="${MUT_HASH_APPLIED[$rel]:-}"
        # Same rule as restore_mutation, and it must not depend on having installed anything: the
        # on-disk content is either ours or the pristine original, or it is someone else's.
        if [ "$now" != "${MUT_HASH0[$rel]:-}" ] \
           && { [ -z "$applied" ] || [ "$now" != "$applied" ]; }; then
          mkdir -p "$(dirname "$MUT_BACKUP_DIR/CONCURRENT-EDIT/$rel")"
          cp "$ROOT_DIR/$rel" "$MUT_BACKUP_DIR/CONCURRENT-EDIT/$rel"
          echo "  REFUSED to restore $rel — changed by another writer; theirs kept" >&2
          bad=1
          continue
        fi
        # STAGED RENAME, like restore_mutation — and verified. A plain `cp` leaves the target partial
        # while copying, so an interrupted signal cleanup produced a truncated compiler source; and it
        # overwrote a save landing between the ownership check above and the copy. The signal path had
        # the exact defect the normal restore path was repaired for, which is what happens when the
        # same operation is written twice.
        # OWNERSHIP RE-CHECKED IMMEDIATELY BEFORE THE RENAME. The check above happens, then the copy
        # and hash take time, and only then does the rename land — so a cooperative save during that
        # staging work was silently overwritten. The ordinary restore path already re-checks; the
        # signal path did not, which is the same operation written twice and diverging again.
        if cp "$bak" "$ROOT_DIR/$rel.mutsig.$$" \
           && [ "$(hash_of "$ROOT_DIR/$rel.mutsig.$$")" = "$(hash_of "$bak")" ] \
           && [ "$(hash_of "$ROOT_DIR/$rel")" = "$now" ] \
           && mv -f "$ROOT_DIR/$rel.mutsig.$$" "$ROOT_DIR/$rel"; then
          echo "  restored $rel from backup" >&2
          MUT_RESTORED_ANY=1
        else
          rm -f "$ROOT_DIR/$rel.mutsig.$$" 2>/dev/null || true
          echo "  FAILED to restore $rel — backup KEPT at $bak; restore it by hand" >&2
          # KEPT means kept: retention was conditioned only on MUT_CONCURRENT, so any OTHER restore
          # failure fell through to `rm -rf "$MUT_BACKUP_DIR"` and destroyed the recovery copy this
          # message had just promised.
          MUT_KEEP_BACKUP=1
          bad=1
        fi
      fi
    done < <(find "$MUT_BACKUP_DIR" -type f 2>/dev/null)
  fi
  # THE BINARY MUST FOLLOW THE SOURCE ON THIS PATH TOO. The signal handler restored source and never
  # rebuilt, so a Ctrl-C — deferred by bash until the running build finishes, and reachable right after
  # a mutated confirming rebuild — left pristine source beside a compiler and oleans built FROM the
  # mutation. That is the stale-artifact state the normal restore rebuilds to prevent, and anything run
  # afterwards would measure the mutated compiler while `git status` looked clean.
  if [ "${MUT_RESTORED_ANY:-0}" = "1" ] && [ -n "${LAKE:-}" ]; then
    echo "  rebuilding after restore (the binary must match the restored source)..." >&2
    "$LAKE" build >/dev/null 2>&1 \
      || echo "  WARNING: rebuild after signal-restore FAILED — .lake may still hold a mutated binary" >&2
  fi
  if [ "$MUT_CONCURRENT" != 0 ]; then
    echo "" >&2
    echo "FATAL: a restore was refused because another writer held a target file." >&2
    echo "       The tree is in a state neither party chose — reconcile by hand." >&2
    bad=1
  fi
  local stray
  stray="$(grep -rn -- "-- MUTATION" "$ROOT_DIR"/Concrete "$ROOT_DIR"/std 2>/dev/null || true)"
  if [ -n "$stray" ]; then
    echo "" >&2
    echo "FATAL: a mutation survived restoration:" >&2
    printf '%s\n' "$stray" >&2
    bad=1
  fi
  # Exact restoration, per target file.
  for f in "${!MUT_HASH0[@]}"; do
    local now; now="$(hash_of "$ROOT_DIR/$f")"
    if [ "$now" != "${MUT_HASH0[$f]}" ]; then
      echo "" >&2
      echo "FATAL: $f was not restored exactly" >&2
      echo "  before: ${MUT_HASH0[$f]}" >&2
      echo "  after : $now" >&2
      bad=1
    fi
  done
  # KEEP the backup dir when a concurrent edit was refused. It holds both the
  # rescued foreign version and our original, which is exactly what reconciling
  # needs — and the refusal message names that path. A first version of this fix
  # printed the path and then deleted it two lines later.
  if [ "$MUT_CONCURRENT" != 0 ]; then
    echo "" >&2
    echo "  PRESERVED for reconciliation: $MUT_BACKUP_DIR" >&2
    echo "    CONCURRENT-EDIT/<path>  the other writer's version" >&2
    echo "    <path>                  the original this harness backed up" >&2
  else
    if [ "${MUT_KEEP_BACKUP:-0}" = "1" ]; then
      echo "  PRESERVED for recovery: $MUT_BACKUP_DIR" >&2
    else
      rm -rf "$MUT_BACKUP_DIR" 2>/dev/null || true
    fi
  fi
  # ONLY IF THIS RUN CREATED IT. A read-only invocation takes no lock, yet cleanup removed
  # $LOCK_DIR unconditionally — so a documented `--check-patterns` run deleted the lock of an ACTIVE
  # mutating run, and the next mutating run would then start alongside it.
  # The log directory is a SIBLING of the backup tree (see its creation), so it is removed here and
  # never walked by the restore loop above.
  [ -n "${MUT_LOG_DIR:-}" ] && case "$MUT_LOG_DIR" in
    "${TMPDIR:-/tmp}/concrete-mutation-logs."*) rm -rf "$MUT_LOG_DIR" ;;
  esac
  [ "${MUT_HELD_MUT_LOCK:-0}" = "1" ] && rmdir "$LOCK_DIR" 2>/dev/null || true
  # The repository lock is released alongside it. Creator-only, so this is a no-op when the lock was
  # inherited from an outer runner.
  [ "${MUT_HELD_GATE_LOCK:-0}" = "1" ] && _gate_lock_release
  if [ "$bad" = 1 ]; then
    echo "" >&2
    echo "restore the tree before building, gating or committing:" >&2
    echo "  git diff -- Concrete std   # then git checkout -- <files>" >&2
    exit 3
  fi
  exit $rc
}
trap cleanup_lock EXIT
trap 'MUT_SIGNALLED=1; cleanup_lock' INT TERM

KILLED=0
SURVIVED=0
ERRORS=0
# ERRORS is the total; it splits two ways, and both are failures of the RUN, not results about a gate.
#   INVALID — the mutation is not a valid experiment: its anchor is gone, it trips a lint instead of
#             the rule, or it dies in the build without declaring that a build kill is its verdict.
#   INFRA   — no verdict could be reached: a gate red on pristine source, died early, a precondition
#             failed, a restore or rebuild failed, or the build failed for a reason not attributable
#             to the mutation (a missing toolchain looks exactly like this).
# Neither may be read as a kill. The fix differs, which is why they are counted apart.
INVALID=0
INFRA=0
TOTAL=0

# ============================================================
# Mutation definitions — parallel arrays
# ============================================================

MUT_FILE=()
MUT_OLD=()
MUT_NEW=()
MUT_DESC=()
# Optional per-mutation gate. The fast suite is the default killer, but a defect
# whose guard is a `check_*.sh` gate would SURVIVE that suite and be reported as
# a test gap — the harness would be measuring the wrong thing and saying so
# confidently. Naming the gate turns "this gate is load-bearing" into a claim
# the harness actually checks. Set it with `gate_for_last`, which indexes off
# the current array length so it cannot drift out of alignment.
MUT_GATE=()
gate_for_last() { MUT_GATE[$(( ${#MUT_FILE[@]} - 1 ))]="$1"; }

# 1. Layout: i32/u32/f32 size 4 → 8  (tySize)
MUT_FILE+=("Concrete/Check/Layout.lean")
MUT_OLD+=("  | .i32 | .u32 | .float32 => 4
  | .i16 | .u16 => 2
  | .i8 | .u8 | .char | .bool => 1
  | .unit => 0")
MUT_NEW+=("  | .i32 | .u32 | .float32 => 8
  | .i16 | .u16 => 2
  | .i8 | .u8 | .char | .bool => 1
  | .unit => 0")
MUT_DESC+=("Layout: tySize i32/u32/f32 4 → 8")

# 2. Layout: i32/u32/f32 alignment 4 → 1  (tyAlign)
MUT_FILE+=("Concrete/Check/Layout.lean")
MUT_OLD+=("partial def tyAlign (ctx : Ctx) : Ty → Nat
  | .int | .uint | .float64 => 8
  | .i32 | .u32 | .float32 => 4")
MUT_NEW+=("partial def tyAlign (ctx : Ctx) : Ty → Nat
  | .int | .uint | .float64 => 8
  | .i32 | .u32 | .float32 => 1")
MUT_DESC+=("Layout: tyAlign i32/u32/f32 4 → 1")

# 3. Layout: unit size 0 → 4
MUT_FILE+=("Concrete/Check/Layout.lean")
MUT_OLD+=("  | .unit => 0
  | .string => Builtin.stringSize")
MUT_NEW+=("  | .unit => 4
  | .string => Builtin.stringSize")
MUT_DESC+=("Layout: tySize unit 0 → 4")

# 4. Layout: string size 24 → 16
MUT_FILE+=("Concrete/Check/Layout.lean")
MUT_OLD+=("def stringSize : Nat := 24")
MUT_NEW+=("def stringSize : Nat := 16")
MUT_DESC+=("Layout: string size 24 → 16")

# 5. Layout: string not pass-by-ptr
MUT_FILE+=("Concrete/Check/Layout.lean")
MUT_OLD+=("def isPassByPtr (ctx : Ctx) (ty : Ty) : Bool :=
  match ty with
  | .string => true")
MUT_NEW+=("def isPassByPtr (ctx : Ctx) (ty : Ty) : Bool :=
  match ty with
  | .string => false")
MUT_DESC+=("Layout: isPassByPtr string → false")

# 6. Layout: isFFISafe rejects integers
MUT_FILE+=("Concrete/Check/Layout.lean")
MUT_OLD+=("def isFFISafe (ctx : Ctx) (ty : Ty) : Bool :=
  match ty with
  | .int | .uint | .i8 | .i16 | .i32 | .u8 | .u16 | .u32 => true")
MUT_NEW+=("def isFFISafe (ctx : Ctx) (ty : Ty) : Bool :=
  match ty with
  | .int | .uint | .i8 | .i16 | .i32 | .u8 | .u16 | .u32 => false")
MUT_DESC+=("Layout: isFFISafe rejects integers")

# 7. Shared: floats not numeric
MUT_FILE+=("Concrete/Resolve/Shared.lean")
MUT_OLD+=("def isNumeric : Ty → Bool
  | .int | .uint | .i8 | .i16 | .i32 | .u8 | .u16 | .u32 => true
  | .float64 | .float32 => true")
MUT_NEW+=("def isNumeric : Ty → Bool
  | .int | .uint | .i8 | .i16 | .i32 | .u8 | .u16 | .u32 => true
  | .float64 | .float32 => false")
MUT_DESC+=("Shared: isNumeric rejects floats")

# 8. Shared: i32 not integer
MUT_FILE+=("Concrete/Resolve/Shared.lean")
MUT_OLD+=("def isInteger : Ty → Bool
  | .int | .uint | .i8 | .i16 | .i32 | .u8 | .u16 | .u32 => true")
MUT_NEW+=("def isInteger : Ty → Bool
  | .int | .uint | .i8 | .i16 | .u8 | .u16 | .u32 => true")
MUT_DESC+=("Shared: isInteger excludes i32")

# 9. Check: disable use-after-move detection
MUT_FILE+=("Concrete/Check/Check.lean")
MUT_OLD+=("        if !info.isCopy && info.state == .consumed then
          -- secondary span: where the value was moved (Phase 4 #11).")
MUT_NEW+=("        if false then -- MUTATION: use-after-move disabled
          -- secondary span: where the value was moved (Phase 4 #11).")
MUT_DESC+=("Check: disable use-after-move")

# 10. Check: disable loop-depth linearity check (enforcement lives in CheckHelpers)
MUT_FILE+=("Concrete/Check/CheckHelpers.lean")
MUT_OLD+=("      if info.loopDepth + breakDepthExempt < env.loopDepth && !env.inFnExitingBranch
          && env.rebindingVar != some name then
        throwCheck (.cannotConsumeLinearInLoop name) span")
MUT_NEW+=("      if false && (info.loopDepth + breakDepthExempt < env.loopDepth && !env.inFnExitingBranch
          && env.rebindingVar != some name) then -- MUTATION: loop-depth disabled
        throwCheck (.cannotConsumeLinearInLoop name) span")
MUT_DESC+=("Check: disable loop-depth linearity")

# 11. Check: disable scope-exit unconsumed check (enforcement lives in CheckHelpers)
MUT_FILE+=("Concrete/Check/CheckHelpers.lean")
MUT_OLD+=("      if !info.isCopy && info.state != .consumed && info.state != .reserved then")
MUT_NEW+=("      if false then -- MUTATION: scope check disabled")
MUT_DESC+=("Check: disable scope-exit linearity")

# 12. CoreCheck: disable match exhaustiveness
MUT_FILE+=("Concrete/Check/CoreCheck.lean")
MUT_OLD+=("            if !seenVariants.contains vn then
              addCCError (.matchMissingVariant name vn)")
MUT_NEW+=("            if !seenVariants.contains vn then
              pure () -- MUTATION: exhaustiveness disabled")
MUT_DESC+=("CoreCheck: disable match exhaustiveness")

# 13. CoreCheck: disable capability discipline
MUT_FILE+=("Concrete/Check/CoreCheck.lean")
MUT_OLD+=("      if !capD.satisfied then
        addCCError (.insufficientCapabilities fn (capSetToString capD.required) (capSetToString capD.callerHas))")
MUT_NEW+=("      if false then -- MUTATION: capability check disabled
        addCCError (.insufficientCapabilities fn (capSetToString capD.required) (capSetToString capD.callerHas))")
MUT_DESC+=("CoreCheck: disable capability check")

# 14. CoreCheck: allow break outside loop
MUT_FILE+=("Concrete/Check/CoreCheck.lean")
MUT_OLD+=("    if !env.inLoop then
      addCCError .breakOutsideLoop")
MUT_NEW+=("    if false then -- MUTATION: break check disabled
      addCCError .breakOutsideLoop")
MUT_DESC+=("CoreCheck: allow break outside loop")

# 15. Lower: arrayIndex GEP uses .int instead of elem type
MUT_FILE+=("Concrete/IR/Lower.lean")
MUT_OLD+=("    emit (.gep gepDst aVal [iVal] ty)
    let loadDst ← freshReg
    emit (.load loadDst (.reg gepDst ty) ty)")
MUT_NEW+=("    emit (.gep gepDst aVal [iVal] .int)
    let loadDst ← freshReg
    emit (.load loadDst (.reg gepDst .int) .int)")
MUT_DESC+=("Lower: arrayIndex GEP uses .int")

# 16. EmitSSA: isReprCStruct always false
MUT_FILE+=("Concrete/Backend/EmitSSA.lean")
MUT_OLD+=("private def isReprCStruct (s : EmitSSAState) : Ty → Bool
  | .named name => (Layout.lookupStruct (layoutCtxOf s) name).any (·.isReprC)
  | _ => false")
MUT_NEW+=("private def isReprCStruct (_s : EmitSSAState) : Ty → Bool
  | _ => false")
MUT_DESC+=("EmitSSA: isReprCStruct always false")

# 17. SSAVerify: disable aggregate phi check
MUT_FILE+=("Concrete/IR/SSAVerify.lean")
MUT_OLD+=("      let ctx := if isAggregateType ty then
        addSSAError ctx (.aggregatePhi b.label dst (reprStr ty))
      else ctx")
MUT_NEW+=("      let ctx := if false then -- MUTATION: agg phi disabled
        addSSAError ctx (.aggregatePhi b.label dst (reprStr ty))
      else ctx")
MUT_DESC+=("SSAVerify: disable aggregate phi check")

# 18. SSAVerify: disable phi missing-predecessor check
MUT_FILE+=("Concrete/IR/SSAVerify.lean")
MUT_OLD+=("        if phiLabels.contains p then ctx
        else addSSAError ctx (.phiMissingPredecessor b.label p)")
MUT_NEW+=("        if phiLabels.contains p then ctx
        else ctx -- MUTATION: phi pred check disabled")
MUT_DESC+=("SSAVerify: disable phi predecessor check")

# 19. Mono: no user generic enum is specialized (R-0001 / bug 051)
# Treating every generic enum as a builtin removes BOTH halves of the fix at
# once: no per-instantiation declaration is created, and the residual E0808
# containment goes vacuous because its name list is empty. That is precisely the
# pre-fix state where instantiations of different size share one declaration, so
# a surviving mutation would mean nothing pins the layout.
MUT_FILE+=("Concrete/IR/Mono.lean")
MUT_OLD+=("  let isBuiltin (ed : CEnumDef) : Bool :=
    ed.builtinId.isSome || ed.name == optionEnumName || ed.name == resultEnumName")
MUT_NEW+=("  let isBuiltin (_ed : CEnumDef) : Bool := true -- MUTATION: enum mono disabled")
MUT_DESC+=("Mono: user generic enums not specialized (bug 051)")

# 20. Mono: enums recognized as generic but left out of the specialization map
# The complement of #19: detection stays ON while specialization is skipped, so
# the residual E0808 containment is armed and MUST fire. Correct programs never
# reach E0808, so this mutation is the only thing proving that path is live
# rather than dead code — without it, deleting the backstop would go unnoticed
# until something else regressed.
MUT_FILE+=("Concrete/IR/Mono.lean")
MUT_OLD+=("    if allStructs.any (fun sd => sd.name == name) || allEnums.any (fun ed => ed.name == name)
    then some (name, args, monoTypeName name args)")
MUT_NEW+=("    if allStructs.any (fun sd => sd.name == name) -- MUTATION: enums unmapped
    then some (name, args, monoTypeName name args)")
MUT_DESC+=("Mono: generic enums detected but unmapped (E0808 backstop)")

# 21. Elab: emit an indirect call as a DIRECT one (R-0002 / bug 050)
# Restores the pre-fix Core shape — a call through a fn-typed local becomes
# indistinguishable from a direct call, so Mono resolves the binding name against
# the global fn map again and a same-named generic hijacks the call. This is the
# mutation the roadmap asks for: "routes indirect calls through direct-name
# resolution". It must be caught by the fn-pointer fixtures, not merely by
# something downstream noticing an undefined symbol.
MUT_FILE+=("Concrete/Elab/Elab.lean")
MUT_OLD+=("    return ElaboratedExprV2.mk (CExpr.call (.indirect fnName) [] cArgs retTy)")
MUT_NEW+=("    return ElaboratedExprV2.mk (CExpr.call (.direct fnName) [] cArgs retTy) -- MUTATION: indirect call resolved by name")
MUT_DESC+=("Elab: indirect call emitted as direct (bug 050)")

# 22. std map: forget the remembered tombstone (R-0003 / bug 047)
# Insert stops at the first free-or-tombstone slot again, so a key living past a
# tombstone gets a second live slot. Independent of #23 and #24: this one is the
# duplication invariant only.
MUT_FILE+=("std/src/map.con")
MUT_OLD+=("                if flag == 2 {
                    if !have_tomb {
                        have_tomb = true;
                        tomb_idx = idx;
                    }
                }")
MUT_NEW+=("                if flag == 2 {
                    // MUTATION: write into the first tombstone immediately
                    let key_ptr: *mut K = self.keys + idx;
                    *key_ptr = key;
                    let val_ptr: *mut V = self.values + idx;
                    *val_ptr = value;
                    *flag_ptr = 1;
                    self.len = self.len + 1;
                    self.tombstones = self.tombstones - 1;
                    return Option::<V>::None;
                }")
MUT_DESC+=("std map: insert reuses the first tombstone (bug 047)")

# 23. std map: unbounded lookup probe (R-0003 / bug 048, half 1)
# The probe no longer stops after `cap` slots, so a missing-key lookup in a table
# with no empty slot wraps forever. Caught as a TIMEOUT, which is why the gate
# runs every leg under a watchdog.
MUT_FILE+=("std/src/map.con")
MUT_OLD+=("            while probes < self.cap {
                let flag_ptr: *mut u8 = self.flags + idx;
                let flag: u8 = *flag_ptr;

                if flag == 0 {
                    // Empty — the probe chain ends here, so the key is absent.
                    return Option::<u64>::None;
                }")
MUT_NEW+=("            while true { // MUTATION: probe bound removed
                let flag_ptr: *mut u8 = self.flags + idx;
                let flag: u8 = *flag_ptr;

                if flag == 0 {
                    // Empty — the probe chain ends here, so the key is absent.
                    return Option::<u64>::None;
                }")
MUT_DESC+=("std map: lookup probe unbounded (bug 048)")

# 24. std map: occupancy ignores tombstones (R-0003 / bug 048, half 2)
# The load factor counts only live entries again, so tombstones accumulate until
# the table has no empty slot. With #23's bound still in place the lookup returns
# rather than hanging, so what this must break is the VALUE invariants — proving
# the two halves of 048 are gated separately.
MUT_FILE+=("std/src/map.con")
MUT_OLD+=("            if (self.len + self.tombstones) * 4 >= self.cap * 3 {")
MUT_NEW+=("            if self.len * 4 >= self.cap * 3 { // MUTATION: tombstones uncounted")
MUT_DESC+=("std map: load factor ignores tombstones (bug 048)")

# 25. DCE: unary ops are always removable again (R-0005 / bug 053)
# Restores the pre-fix state — `.unaryOp` falls through to the catch-all meaning
# "harmless" — so a discarded checked negation at MIN is deleted and the trap
# silently disappears. The roadmap asks for a mutation omitting a trapping unary
# constructor; this is that, and it must be caught by the differential (compiled
# vs interpreter), not merely by a value check.
MUT_FILE+=("Concrete/IR/SSACleanup.lean")
MUT_OLD+=("  | .unaryOp _ op operand ty =>
    if !(IntArith.unaryOpCanTrap op ty) then false")
MUT_NEW+=("  | .unaryOp _ op operand ty =>
    if true then false -- MUTATION: unary trap inventory ignored")
MUT_DESC+=("DCE: discarded trapping unary ops removable again (bug 053)")
gate_for_last "scripts/tests/check_trap_inventory.sh"

# 26. IntArith: the trap inventory's answer is inverted (R-0005 / bug 053)
# Mutates the SINGLE SOURCE rather than a consumer. If the centralisation is
# real, poisoning the inventory must break behaviour in the consumers — a
# consumer that still passes is deriving the answer locally, and the
# single-source claim is false.
#
# Inverted rather than constant-`false` on purpose: `| .neg => false` leaves
# `ty` unused, so Lean's linter rejects the file and the mutation is INVALID —
# it never reaches anything that could see the semantics. A mutation killed by the wrong mechanism is a mutation that
# never ran.
MUT_FILE+=("Concrete/Semantics/IntArith.lean")
MUT_OLD+=("  | .neg => isIntTy ty
  | .bitnot | .not_ => false")
MUT_NEW+=("  | .neg => !(isIntTy ty) -- MUTATION: inventory answer inverted
  | .bitnot | .not_ => false")
MUT_DESC+=("IntArith: unary trap inventory inverted (bug 053)")
gate_for_last "scripts/tests/check_trap_inventory.sh"

# 27. IntArith: checked negation wraps instead of trapping (R-0005 / bug 053)
# The other half of the inventory: `unaryOpCanTrap` says WHETHER, this says
# WHAT. Wrapping at MIN is the plausible-looking wrong answer, and it must be
# visible on the interpreter path — the differential is what makes a silent
# semantic drift observable rather than merely a missing abort.
MUT_FILE+=("Concrete/Semantics/IntArith.lean")
MUT_OLD+=("    | none   => .trap \"arithmetic overflow (checked negation)\"")
MUT_NEW+=("    | none   => .value (maskWidth ty (-n)) ty -- MUTATION: wrap, do not trap")
MUT_DESC+=("IntArith: checked negation wraps at MIN instead of trapping (bug 053)")
gate_for_last "scripts/tests/check_trap_inventory.sh"

# 28. SSACleanup: the indirect call target is not substituted (R-0436 / bug 056)
# Restores the pre-fix behaviour at the exact spot: the callee operand is left
# alone while every other operand is rewritten. Folding a fn-pointer phi then
# leaves a call through a value nothing defines.
MUT_FILE+=("Concrete/IR/SSACleanup.lean")
MUT_OLD+=("      | .indirect target => .indirect (r target)")
MUT_NEW+=("      | .indirect target => .indirect target -- MUTATION: callee not substituted")
MUT_DESC+=("SSACleanup: indirect call target escapes substitution (bug 056)")
gate_for_last "scripts/tests/check_fnptr_values.sh"

# 29. SSAVerify: the indirect call target is not a use (R-0436 / bug 056)
# This is the state the String callee forced — the verifier could not see a call
# target at all, so a call through an undefined register passed verification and
# was caught by llvm-as instead. DCE may also delete the producing instruction.
MUT_FILE+=("Concrete/IR/SSAVerify.lean")
MUT_OLD+=("    | .indirect target => svalRegs target ++ argRegs")
MUT_NEW+=("    | .indirect _ => argRegs -- MUTATION: callee is not a use")
MUT_DESC+=("SSAVerify: indirect call target invisible to use-checking (bug 056)")
gate_for_last "scripts/tests/check_fnptr_values.sh"

# 30. EmitSSA: a function reference stops resolving to a global (R-0436)
# Devirtualization is decided HERE, not in Lower: `.indirect (.fnRef f)` and
# `.direct f` both reach `svalToOperand` and both emit `call @f`, so removing
# Lower's `.direct` conversion leaves the emitted IR byte-identical (measured)
# and is NOT a behaviour change worth mutating. Lower's conversion still matters
# for passes that key on a direct callee — `checkCallArity` only validates
# those — but it cannot be what the direct-call assertions detect.
# Making `.fnRef` emit a register instead is the real inverse: correctness legs
# keep passing (the call still reaches the right function via a load) while the
# common case silently becomes an indirect call.
MUT_FILE+=("Concrete/Backend/EmitSSA.lean")
MUT_OLD+=("    .global resolved")
MUT_NEW+=("    .reg resolved -- MUTATION: fn reference emitted as a register")
MUT_DESC+=("EmitSSA: fn reference no longer resolves to a global (R-0436)")
gate_for_last "scripts/tests/check_fnptr_values.sh"

# 31. ProofCore: an applied parameter extracts as a definition call (R-0442 / 061)
# Restores bug 061 exactly: `.applyVar` collapses back into `.call`, so a
# parameter named `f` and a definition named `f` become the same node and the
# evaluator resolves the parameter through the global function table.
MUT_FILE+=("Concrete/Proof/ProofCore.lean")
MUT_OLD+=("    some (.applyVar binding pargs)
  | .structLit name _ fields _ => do")
MUT_NEW+=("    some (.call binding pargs) -- MUTATION: parameter application as a definition call
  | .structLit name _ fields _ => do")
MUT_DESC+=("ProofCore: applied parameter extracts as a global call (bug 061)")
gate_for_last "scripts/tests/check_proofcore_callable_identity.sh"

# 32. Proof: an applied local resolves in the GLOBAL namespace (R-0442 / 061)
# The other half. Extraction stays correct but `eval` looks the binding up among
# definitions, so a global `f` answers an application of a parameter `f` — the
# soundness hazard, in the evaluator rather than the extractor.
MUT_FILE+=("Concrete/Proof/Proof.lean")
MUT_OLD+=("    match fns.callables binding with")
MUT_NEW+=("    match fns.globals binding with -- MUTATION: local resolved as a global")
MUT_DESC+=("Proof: eval resolves an applied local through globals (bug 061)")
gate_for_last "scripts/tests/check_proofcore_callable_identity.sh"

# 33. Proof: the representative callback goes back into the global namespace
# The state R-0442 found: the HOF specs' callback bound as a DEFINITION.
# Measured outcome, better than expected: this and #32 are killed by the Lean
# KERNEL, not by the gate — the three map theorems reduce to `⊢ False` because
# `.applyVar f` is stuck when `f` lives in the wrong namespace. So the proofs
# themselves are load-bearing evidence for the separation; the gate's structural
# assertions are a second, independent line rather than the only one.
# (A build failure is INVALID when a LINTER rejects the file; it is the strongest
# possible signal when the kernel rejects the theorem — a declared build kill.)
MUT_FILE+=("Concrete/Proof/Proof.lean")
MUT_OLD+=("  FnTable.withCallables (fun _ => none) pureCoreCallables")
MUT_NEW+=("  FnTable.withCallables pureCoreCallables (fun _ => none) -- MUTATION: callback as a global")
MUT_DESC+=("Proof: representative callback bound as a global (bug 061)")
gate_for_last "scripts/tests/check_proofcore_callable_identity.sh"

# 34. ProofCore: dependency containment removed entirely (R-0004 slice 3 / 062)
# Restores the pre-slice-3 state: notCurrentDeps is still computed and recorded,
# and still has no effect on the status — which is exactly what bug 062 was.
MUT_FILE+=("Concrete/Proof/ProofCore.lean")
MUT_OLD+=("    | some .proved => if (notCurrentOf n).isEmpty then .proved else .depsNotCurrent")
MUT_NEW+=("    | some .proved => .proved -- MUTATION: containment has no effect")
MUT_DESC+=("ProofCore: a non-current dependency no longer downgrades (bug 062)")
gate_for_last "scripts/tests/check_proof_freshness.sh"

# 35. ProofCore: containment stops at ONE hop (R-0004 slice 3 / 062)
# The subtler half. The direct dependent is still contained, so a gate that only
# checked one hop would pass; only the two-hop leg can see this.
MUT_FILE+=("Concrete/Proof/ProofCore.lean")
# Mutated INSIDE the walk so every binding stays used: replacing the call site
# left `reachableFrom` unused and Lean's linter rejected the file, which is a
# kill that says nothing about whether a test can see one-hop-only behaviour.
MUT_OLD+=("        else go fuel (rest ++ directCalleesOf n) (n :: seen)")
MUT_NEW+=("        else go fuel rest (n :: seen) -- MUTATION: frontier never expands (one hop)")
MUT_DESC+=("ProofCore: containment does not traverse the closure (bug 062 transitive)")
gate_for_last "scripts/tests/check_proof_freshness.sh"

# 36. ProofCore: a stale dependency counts as current (R-0004 slice 3)
# Mutates the single-source policy rather than a consumer. If the policy really
# is single-source, poisoning it must break containment everywhere at once.
MUT_FILE+=("Concrete/Proof/ProofCore.lean")
# Both arms replaced together: adding `.stale` to the first line alone leaves it
# overlapping the second, which Lean rejects structurally rather than any test
# catching the semantics.
# RE-ANCHORED, and the anchor now spans the interleaved comments because that is what the arms
# actually look like. The false arm has grown twice — `needsRecheck` and `correspondenceUnjustified` —
# and each time the anchor stopped matching, leaving the single-source currency policy with no live
# mutation. An anchor that must be updated when the vocabulary grows is the correct trade here: the
# alternative is a looser match that keeps applying to a rule it no longer describes.
MUT_OLD+=("  | .proved | .trusted => true
  -- \`needsRecheck\` is NOT current: the stored digest answers an older, weaker
  -- question, so nothing downstream may treat this claim as established.
  | .stale | .missing | .blocked | .ineligible | .unbound | .needsRecheck
  -- A claim whose own dependency justification was never established is NOT current for anyone
  -- else's: propagating it would let an unjustified closure become the foundation of a second one.
  | .depsNotCurrent | .correspondenceUnjustified => false")
MUT_NEW+=("  | .proved | .trusted | .stale => true -- MUTATION: stale counts as current
  | .missing | .blocked | .ineligible | .unbound | .needsRecheck
  | .depsNotCurrent | .correspondenceUnjustified => false")
MUT_DESC+=("ProofCore: trap inventory of dependency currency admits stale (slice 3)")
gate_for_last "scripts/tests/check_proof_freshness.sh"

# 37. FnTable: entry order leaks into the root (R-0004 step 3)
# Canonical ordering is what makes the root a function of CONTENT. Sorting by
# insertion order instead makes two identical tables hash differently, so a
# receipt would depend on how the generator happened to emit entries.
MUT_FILE+=("Concrete/Proof/Proof.lean")
# Retargeted: the root no longer sorts (qsort does not kernel-reduce), so
# canonical order is now an ASSERTED property. Accepting any order is the defect.
MUT_OLD+=("  (keys.zip (keys.drop 1)).all fun (a, b) => a < b")
MUT_NEW+=("  (keys.zip (keys.drop 1)).all fun (a, b) => a <= b -- MUTATION: order not strict")
MUT_DESC+=("FnTable: entry order only non-decreasing, not strict (R-0004 step 3)")
gate_for_last "scripts/tests/check_callable_identity.sh"

# 38. FnTable: duplicate identities accepted (R-0004 step 3)
# Two entries claiming one identity means the table disagrees with itself.
# Accepting it makes lookup arbitrary and the root insertion-order dependent.
MUT_FILE+=("Concrete/Proof/Proof.lean")
MUT_OLD+=("  keys.length != keys.eraseDups.length")
MUT_NEW+=("  keys.length != keys.length -- MUTATION: duplicate identities accepted")
MUT_DESC+=("FnTable: duplicate CallableIds no longer rejected (R-0004 step 3)")
gate_for_last "scripts/tests/check_callable_identity.sh"

# 39. FnTable: the body/params are dropped from the root (R-0004 step 3)
# A root over identities alone cannot detect an altered body — the caller would
# keep a `current` verdict across a real semantic change, which is the whole
# class R-0004 exists to close.
MUT_FILE+=("Concrete/Proof/Proof.lean")
# The INNER per-param prefix is what makes the param list injective. Removing the
# outer one only changes formatting: each param is already self-delimiting, so
# two distinct tables still get distinct roots and there is nothing to catch — a
# first draft of this mutation SURVIVED for exactly that reason, correctly.
# Dropping the inner prefix is the real defect: ["a","b"] and ["a,b"] both render
# "a,b", so two different signatures collide on one root.
MUT_OLD+=("      let ps := String.intercalate \",\" (d.params.map fun p => lp \"p\" p)")
MUT_NEW+=("      let ps := String.intercalate \",\" d.params -- MUTATION: params not self-delimiting")
MUT_DESC+=("FnTable: param list not injective in the root (step 3)")
gate_for_last "scripts/tests/check_callable_identity.sh"

# 40. FnTable: the operational key index leaves the root (R-0004 step 3)
# Calls still select entries by STRING. Dropping the key index from the root
# means a receipt does not commit to the name->identity mapping it was produced
# under, so renaming a displayName silently keeps the old root.
MUT_FILE+=("Concrete/Proof/Proof.lean")
MUT_OLD+=("    some (s!\"tblv{t.schemaVersion}:\" ++ lp \"E\" (String.join parts) ++ lp \"K\" (String.join idx))")
MUT_NEW+=("    some (s!\"tblv{t.schemaVersion}:\" ++ lp \"E\" (String.join parts) ++ lp \"K\" (String.join (idx.take 0))) -- MUTATION: key index dropped")
MUT_DESC+=("FnTable: root omits the string-key index (R-0004 step 3)")
gate_for_last "scripts/tests/check_callable_identity.sh"

# 41. FnTable: one key reaching two entries is allowed (R-0004 step 3)
# While calls select by name, an ambiguous key means a call picks arbitrarily.
MUT_FILE+=("Concrete/Proof/Proof.lean")
MUT_OLD+=("  keys.length == keys.eraseDups.length")
MUT_NEW+=("  keys.length == keys.length -- MUTATION: ambiguous keys accepted")
MUT_DESC+=("FnTable: ambiguous string-key index accepted (R-0004 step 3)")
gate_for_last "scripts/tests/check_callable_identity.sh"

# 42. lookupById matches on the display NAME (R-0004 step 3)
# The mismatched-lookup-key case: resolving by name rather than identity is
# exactly the keyed identity the finite table exists to remove, and it makes a
# same-named callable in another module answer for this one.
MUT_FILE+=("Concrete/Proof/Proof.lean")
MUT_OLD+=("    | .semantic cid => cid == id")
MUT_NEW+=("    | .semantic cid => cid.declName == id.declName -- MUTATION: lookup by NAME, ignoring module and arity")
MUT_DESC+=("FnTable: lookupById resolves by display name, not identity (step 3)")
gate_for_last "scripts/tests/check_callable_identity.sh"

# 43. FnTable: the root stops binding entry BODIES (R-0004 step 3)
# The original defect, kept as a permanent mutation: with bodies unbound, two
# tables with identical identities and parameters but different bodies had EQUAL
# roots while evaluating to 1 and 999. A root blind to behaviour cannot back a
# receipt, and the nine-table migration would have moved proofs onto it.
MUT_FILE+=("Concrete/Proof/Proof.lean")
MUT_OLD+=("      lp \"i\" d.identityKey ++ lp \"P\" ps ++ lp \"B\" (pexprCanonical d.body) ++ lp \"S\" sd")
MUT_NEW+=("      lp \"i\" d.identityKey ++ lp \"P\" ps ++ lp \"S\" sd -- MUTATION: root blind to bodies")
MUT_DESC+=("FnTable: root omits entry bodies (R-0004 step 3)")
gate_for_last "scripts/tests/check_callable_identity.sh"

# 44. FnTable: a type-erased generic identity is accepted as complete
# One entry standing in for every monomorphization, when the monomorphizations
# disagree: extracted arithmetic is width-free, so a kernel-true proof over `Int`
# is FALSE of an `i8` instance where 100 + 100 wraps. This is the fail-closed
# direction, so removing the check must not be silent.
MUT_FILE+=("Concrete/Proof/Proof.lean")
MUT_OLD+=("    | some id => id.isComplete")
MUT_NEW+=("    | some id => id.isComplete || true -- MUTATION: erased generics accepted")
MUT_DESC+=("FnTable: incomplete (type-erased) identities accepted")
gate_for_last "scripts/tests/check_callable_identity.sh"

# 45. Generator: a lookup lemma for the FIRST entry only
# Missing lemmas make an entry unreachable to the kernel while the table still
# looks complete — a proof about that identity cannot be used, and nothing says
# so. The incorrect/missing-lemma class.
MUT_FILE+=("Concrete/Report/Report.lean")
MUT_OLD+=("  let lookupLemmas := extracted.map fun e =>")
MUT_NEW+=("  let lookupLemmas := (extracted.take 1).map fun e => -- MUTATION: one lemma only")
MUT_DESC+=("Generator: lookup lemma emitted for one entry only")
gate_for_last "scripts/tests/check_callable_identity.sh"

# 46. Generator: an entry is dropped from the table but keeps its lemma
# The entry-deletion class. The table shrinks while the lemmas still claim the
# missing entry is reachable, so `rfl` on that lookup no longer holds.
MUT_FILE+=("Concrete/Report/Report.lean")
MUT_OLD+=("\", \".intercalate entryNames}]")
MUT_NEW+=("\", \".intercalate entryNames.dropLast}]")
MUT_DESC+=("Generator: an entry is dropped but keeps its lookup lemma")
gate_for_last "scripts/tests/check_callable_identity.sh"

# 47. Generator: the body digest is computed over a CONSTANT, not the body
# A digest blind to the body cannot detect that a generated table's body literal
# drifted from what the compiler extracts — which is its whole job. The field was
# representable but never emitted for a while; this keeps "emitted" from decaying
# back into "emitted, but meaningless".
# RE-ANCHORED. The computation moved to `Concrete/Proof/BodyIdentity.lean` in bba323ee (the
# Digest/BodyIdentity split) and this anchor stayed pointing at `Report.lean`, so the mutation could
# not be applied and the digest below has had NO live mutation coverage since. That is the failure
# mode check_mutation_anchors.sh exists to report: an unapplied mutation is not a passing mutation,
# it is an absent one, and it silently withdraws the evidence that the gate is load-bearing.
MUT_FILE+=("Concrete/Proof/BodyIdentity.lean")
MUT_OLD+=("  shortHash (Proof.pexprCanonical (normalizePExpr pe))")
MUT_NEW+=("  shortHash (toString (Proof.pexprCanonical (normalizePExpr pe)).length) -- MUTATION: digest sees only the body LENGTH")
MUT_DESC+=("Generator: body digest does not depend on the body")
gate_for_last "scripts/tests/check_callable_identity.sh"

# 48. Generator: identities are PERMUTED among entries (R-0004 step 4)
# The ID-swap class. Deliberately a rotation rather than a duplication, because
# every cheap check survives a permutation: the SET of identities is unchanged, so
# "the expected identities are present" passes; both identities remain distinct, so
# duplicate detection passes; and the Id definition and the lookup lemma name the
# same symbol, so the generated file stays internally consistent and the kernel has
# nothing to object to. Only CORRESPONDENCE catches it — the identity must sit on
# the body that belongs to it — which is why the correspondence legs exist and why
# this mutation is the thing that proves they are load-bearing.
MUT_FILE+=("Concrete/Report/Report.lean")
MUT_OLD+=("    let idLit := match e.callableId with")
MUT_NEW+=("    let rotIdx := (((extracted.findIdx? (·.qualName == e.qualName)).getD 0) + 1) % extracted.length
    let idLit := match (extracted[rotIdx]?).bind (·.callableId) with -- MUTATION: identities rotated among entries")
MUT_DESC+=("Generator: identities permuted among entries (ID swap)")
gate_for_last "scripts/tests/check_callable_identity.sh"

# 49. The shadowing rule ignores IMPORTED enums (bug 065's original divergence)
# Elab consulted `imports.enums` and Check did not, so an imported user `Result`
# was visible to one pass and not the other. Now that the rule has one owner, this
# mutation reinstates exactly that asymmetry — a structural check would not notice,
# because the code is still centralised.
MUT_FILE+=("Concrete/Resolve/BuiltinEnums.lean")
MUT_OLD+=("  moduleEnums.any (fun ed => ed.name == resultEnumName)
    || importedEnums.any (fun ed => ed.name == resultEnumName)")
MUT_NEW+=("  moduleEnums.any (fun ed => ed.name == resultEnumName) -- MUTATION: imported enums ignored")
MUT_DESC+=("BuiltinEnums: shadowing rule ignores imported enums (bug 065)")
gate_for_last "scripts/tests/check_builtin_enum_owner.sh"

# 50. Import alias becomes the type's DEFINITION identity (R-0004 V2 input)
# Bug 064 requires `.name` to become the importer-visible alias, but TypeId must
# continue to name the defining declaration. This mutation makes `Coord` a new
# type identity instead of another spelling of `lib.Point`.
MUT_FILE+=("Concrete/Resolve/FileSummary.lean")
MUT_OLD+=("            .ok { acc with structs := acc.structs ++ [{ sd with name := localName }],")
MUT_NEW+=("            .ok { acc with structs := acc.structs ++ [{ sd with name := localName, definitionName := localName }], -- MUTATION: alias becomes identity")
MUT_DESC+=("TypeId: import alias replaces definition-site name")
gate_for_last "scripts/tests/check_type_identity.sh"

# 51. Nested type identity drops its parent module (R-0004 V2 input)
# `a.sub.Point` and `b.sub.Point` must not become the same `sub.Point`.
MUT_FILE+=("Concrete/Resolve/FileSummary.lean")
MUT_OLD+=("      let childPath := if thisDefinitionPath.isEmpty then sub.name
                       else thisDefinitionPath ++ \".\" ++ sub.name")
MUT_NEW+=("      let childPath := sub.name -- MUTATION: enclosing definition path dropped")
MUT_DESC+=("TypeId: nested identity drops enclosing module")
gate_for_last "scripts/tests/check_type_identity.sh"

# 52. Elab resolves a field but silently drops it from the V2 input.
#
# SPLIT IN TWO AND WIDENED, 2026-08-21. The original anchor matched BOTH `recordFieldUse` call sites
# (Elab.lean:685 in field-access elaboration and :1762 in the field-owner resolution), and the harness
# applies mutations with `content.replace(old, new, 1)` — the FIRST match. So this mutation silently
# tested only site A while claiming to test "the" resolved-field path, and the anchor gate accepted it
# because that gate asked only whether the text was PRESENT, never whether it was UNIQUE. Both sites
# feed evidence inputs, so each gets its own mutation; the extra adjacent line makes each unique.
MUT_FILE+=("Concrete/Elab/Elab.lean")
# Re-anchored for R-0484: the field's type is now instantiated at the object's capability
# arguments, so the line after `recordFieldUse` changed. The experiment is unchanged —
# drop the recorded field use and keep everything else.
MUT_OLD+=("      | some f =>
        recordFieldUse sd field
        -- R-0484: the field's type at this instantiation (same rule as Check).")
MUT_NEW+=("      | some f =>
        pure () -- MUTATION: resolved field omitted from evidence input
        -- R-0484: the field's type at this instantiation (same rule as Check).")
MUT_DESC+=("Elab V2 input: resolved field use omitted (field access)")
gate_for_last "scripts/tests/check_type_identity.sh"

# 52b. The same omission in the field-OWNER resolution path.
MUT_FILE+=("Concrete/Elab/Elab.lean")
MUT_OLD+=("      | some f =>
        recordFieldUse sd field
        let mapping := sd.typeParams.zip tArgs")
MUT_NEW+=("      | some f =>
        pure () -- MUTATION: resolved field omitted from evidence input
        let mapping := sd.typeParams.zip tArgs")
MUT_DESC+=("Elab V2 input: resolved field use omitted (field owner resolution)")
gate_for_last "scripts/tests/check_type_identity.sh"

# 53. A normally resolved declaration with missing provenance reads as covered.
MUT_FILE+=("Concrete/Elab/Elab.lean")
MUT_OLD+=("private def markBodyIdentityUncovered : ElabM Unit := do
  let env ← getEnv
  setEnv { env with bodyIdentityCovered := false }")
MUT_NEW+=("private def markBodyIdentityUncovered : ElabM Unit :=
  pure () -- MUTATION: missing identity silently treated as covered")
MUT_DESC+=("Elab V2 input: missing identity fails open")
gate_for_last "scripts/tests/check_type_identity.sh"

# 54. A local reference is resolved but emits no node. The binder vanishes from the
# body, which UNDER-APPROXIMATES the subject — worse than uncovered, because the
# subject then looks complete.
# Repointed when the accumulator was deleted: dropping the node now means dropping the
# STRUCTURAL one, so the mutation replaces the builder call with a gap.
MUT_FILE+=("Concrete/Elab/Elab.lean")
MUT_OLD+=("(Proof.evBinderRef env.bodyScope name)")
MUT_NEW+=("(Proof.evUnhandledExpr \"local\") -- MUTATION: local reference emits no V2 node")
MUT_DESC+=("Elab V2 input: local binder references silently dropped")
gate_for_last "scripts/tests/check_binder_refs.sh"

# 55. Frames open EAGERLY. Every positional leg still passes; only the empty-scope
# property breaks, so this is the mutation that proves that leg is load-bearing.
MUT_FILE+=("Concrete/Elab/Elab.lean")
MUT_OLD+=("  setEnv { env with bodyScope := extendFrame env.bodyScope env.pendingFrame name
                    pendingFrame := false }")
MUT_NEW+=("  setEnv { env with bodyScope := extendFrame env.bodyScope true name
                    pendingFrame := false } -- MUTATION: eager frames")
MUT_DESC+=("Elab V2 input: binder frames open eagerly, not on first binder")
gate_for_last "scripts/tests/check_binder_refs.sh"

# 56. The relative position is emitted with its components SWAPPED. A wrong
# semantic position looks resolved, which is the confidently-wrong-identity failure
# this task exists to remove.
# Repointed when the accumulator was deleted: the position is now minted once, by the
# structural builder, so that is where the mutation has to bite.
MUT_FILE+=("Concrete/Proof/EvidenceBuild.lean")
MUT_OLD+=("  | some (out, idx) => .binderRef out idx")
MUT_NEW+=("  | some (out, idx) => .binderRef idx out -- MUTATION: swapped")
MUT_DESC+=("evBinderRef: binder position components swapped")
gate_for_last "scripts/tests/check_binder_refs.sh"

# 57. Repeated uses are DEDUPLICATED. Occurrence count and order are semantic: a
# body reading a binder twice is not the same body as one reading it once.
# Repointed likewise: deduplication would now happen in the DERIVATION, not in an
# accumulator that no longer exists.
MUT_FILE+=("Concrete/Proof/IdentityUseBytes.lean")
MUT_OLD+=("  b.statements.flatMap stmtFlatUses")
MUT_NEW+=("  (b.statements.flatMap stmtFlatUses).eraseDups -- MUTATION: drop repetitions")
MUT_DESC+=("flatUsesOf: repeated identity uses collapsed")
gate_for_last "scripts/tests/check_binder_refs.sh"

# The desugared for-loop. Its evidence mirrors the LOWERING (init; while cond { body; step }),
# so each part must be present or the evidence describes a loop the program does not run.
#
# `stepEv.drop 1` rather than `[]`: dropping the binding outright leaves it unused, Lean's
# linter rejects the file, and the mutation is INVALID — the compiler noticing, not the
# gate.
MUT_FILE+=("Concrete/Elab/Elab.lean")
MUT_OLD+=("(cBodyEv.map (·.evidence) ++ stepEv)")
MUT_NEW+=("(cBodyEv.map (·.evidence) ++ stepEv.drop 1) -- MUTATION: step omitted from the loop body")
MUT_DESC+=("for-loop evidence: step omitted")
gate_for_last "scripts/tests/check_shadow_body_v2.sh"

MUT_FILE+=("Concrete/Elab/Elab.lean")
MUT_OLD+=("      initEv := [cInitEv.evidence]")
MUT_NEW+=("      initEv := [] -- MUTATION: init omitted from the block")
MUT_DESC+=("for-loop evidence: init omitted")
gate_for_last "scripts/tests/check_shadow_body_v2.sh"

# The loop FRAME, not the node. Without the push a `break` in a for-loop body has no
# enclosing loop to resolve against.
MUT_FILE+=("Concrete/Elab/Elab.lean")
MUT_OLD+=("    setEnv { outer with loopFrames := label :: outer.loopFrames }
    let cBodyEv ← elabStmtsEv body
    let cBody := cBodyEv.flatMap (·.core)
    let stepRes ← match step with")
MUT_NEW+=("    let cBodyEv ← elabStmtsEv body
    let cBody := cBodyEv.flatMap (·.core)
    let stepRes ← match step with -- MUTATION: no loop frame for the for-loop body")
MUT_DESC+=("for-loop: body elaborated with no loop frame")
gate_for_last "scripts/tests/check_shadow_body_v2.sh"

# The 2a assembly pass. Each of these was a gap that discarded information the site had
# already computed, so each mutation restores the loss it used to have.
MUT_FILE+=("Concrete/Elab/Elab.lean")
MUT_OLD+=("      | some owner => Proof.evField { owner := owner, field := field } cObjPlaceEv")
MUT_NEW+=("      | some _ => Proof.evField { owner := TypeId.user \"\" \"\", field := field } cObjPlaceEv -- MUTATION: place owner constant")
MUT_DESC+=("field place: owning type replaced by a constant")
gate_for_last "scripts/tests/check_shadow_body_v2.sh"

MUT_FILE+=("Concrete/Elab/Elab.lean")
MUT_OLD+=("      | some owner => Proof.evField { owner := owner, field := field } cObjPlaceEv")
MUT_NEW+=("      | some owner => Proof.evField { owner := owner, field := \"\" } cObjPlaceEv -- MUTATION: place field name blanked")
MUT_DESC+=("field place: written field name blanked")
gate_for_last "scripts/tests/check_shadow_body_v2.sh"

MUT_FILE+=("Concrete/Elab/Elab.lean")
MUT_OLD+=("(Proof.evCall (CallableId.ofIntrinsic \"discard\") [cArgEv.evidence])")
MUT_NEW+=("(Proof.evCall (CallableId.ofIntrinsic \"discard\") []) -- MUTATION: discarded expression dropped")
MUT_DESC+=("discard(): argument dropped from the intrinsic call")
gate_for_last "scripts/tests/check_shadow_body_v2.sh"

MUT_FILE+=("Concrete/Elab/Elab.lean")
MUT_OLD+=("        let mut argEvs : List Proof.EvidenceExprV2 := [selfEv]")
MUT_NEW+=("        let mut argEvs : List Proof.EvidenceExprV2 := [selfEv].drop 1 -- MUTATION: receiver dropped")
MUT_DESC+=("method call: self receiver dropped from the argument list")
gate_for_last "scripts/tests/check_shadow_body_v2.sh"

# Parenthesised so it PARSES. Written `(cElseEv.map (·.evidence)).drop 1` without the outer
# parens it is a syntax error, and the build fails unattributably — the compiler
# noticing, not the gate.
MUT_FILE+=("Concrete/Elab/Elab.lean")
MUT_OLD+=("        (cThenEv.map (·.evidence)) (cElseEv.map (·.evidence)))")
MUT_NEW+=("        (cThenEv.map (·.evidence)) ((cElseEv.map (·.evidence)).drop 1)) -- MUTATION: else branch dropped")
MUT_DESC+=("if-expression: else branch dropped")
gate_for_last "scripts/tests/check_shadow_body_v2.sh"

# The EvidenceTypeRef vocabulary. Its whole reason for existing is that it is nominal by
# IDENTITY and alpha-invariant in type variables, so those are the two things to break.
MUT_FILE+=("Concrete/Proof/EvidenceBuild.lean")
MUT_OLD+=("        | some id => .nominal id")
MUT_NEW+=("        | some _ => .nominal (TypeId.user \"\" n) -- MUTATION: nominal type by spelling")
MUT_DESC+=("evTypeRef: nominal type keyed on spelling, not identity")
gate_for_last "scripts/tests/check_shadow_body_v2.sh"

MUT_FILE+=("Concrete/Proof/EvidenceBuild.lean")
MUT_OLD+=("  | .typeVar n =>
      match binders.findIdx? (· == n) with
      | some i => .typeVarAt i")
MUT_NEW+=("  | .typeVar n =>
      match binders.findIdx? (· == n) with
      | some _ => .typeVarAt n.length -- MUTATION: type variable by spelling")
MUT_DESC+=("evTypeRef: type variable keyed on spelling, not binder position")
gate_for_last "scripts/tests/check_shadow_body_v2.sh"

# `.drop 9` rather than `[]`: an empty list leaves `elemEvs` unused, the linter rejects the
# file, and the mutation is INVALID instead of exercising the gate.
MUT_FILE+=("Concrete/Elab/Elab.lean")
MUT_OLD+=("        (Proof.evArrayLit elemRef elemEvs)")
MUT_NEW+=("        (Proof.evArrayLit elemRef (elemEvs.drop 9)) -- MUTATION: elements dropped")
MUT_DESC+=("array literal: element evidence dropped")
gate_for_last "scripts/tests/check_shadow_body_v2.sh"

# THE FAIL-OPEN. Discarding the type field makes typeRefGaps unreachable, so a body with
# an unresolvable type validates as COMPLETE. Silent, because it is not a type error.
MUT_FILE+=("Concrete/Proof/EvidenceTree.lean")
MUT_OLD+=("  | .arrayLit t els => typeRefGaps t ++ els.flatMap exprGaps")
MUT_NEW+=("  | .arrayLit _ els => els.flatMap exprGaps -- MUTATION: gaps inside a type swallowed")
MUT_DESC+=("evidence gaps: a gap inside a TYPE does not reach the subject")
gate_for_last "scripts/tests/check_shadow_body_v2.sh"

# Bug 068. A ghost binding is erased; hardcoding the flag makes the two lets share bytes
# again, which is the collision the flag exists to remove.
MUT_FILE+=("Concrete/Elab/Elab.lean")
MUT_OLD+=("(Proof.EvidenceStmtV2.letBind true declRef cValEv.evidence)")
MUT_NEW+=("(Proof.EvidenceStmtV2.letBind false declRef cValEv.evidence) -- MUTATION: ghost erasure invisible")
MUT_DESC+=("ghost let: erasure not recorded in the binding node")
gate_for_last "scripts/tests/check_shadow_body_v2.sh"

# Proof-only predicates. assert is DISCHARGED and assume is RELIED UPON, so conflating the
# two node kinds is the mutation that matters most here — it also blinds the assumption
# axis, which is what keeps a claim over an assuming body from being reported unqualified.
MUT_FILE+=("Concrete/Elab/Elab.lean")
MUT_OLD+=("(Proof.EvidenceStmtV2.assumeStmt (← proofPredicateEv pred))")
MUT_NEW+=("(Proof.EvidenceStmtV2.assertStmt (← proofPredicateEv pred)) -- MUTATION: assume encoded as assert")
MUT_DESC+=("assume encoded as assert (collides, and empties the assumption axis)")
gate_for_last "scripts/tests/check_shadow_body_v2.sh"

# Discards the elaborated predicate while keeping the binding used — returning a constant
# with `r` still bound would leave it unused, and the mutation would be INVALID.
MUT_FILE+=("Concrete/Elab/Elab.lean")
MUT_OLD+=("  | (.ok r, after) =>
    setEnv { saved with freshBinder := after.freshBinder }
    pure r.evidence")
MUT_NEW+=("  | (.ok _, after) =>
    setEnv { saved with freshBinder := after.freshBinder }
    pure (Proof.evBoolLit true) -- MUTATION: predicate content discarded")
MUT_DESC+=("proof predicate: elaborated evidence replaced by a constant")
gate_for_last "scripts/tests/check_shadow_body_v2.sh"

# An imported impl method must be identified by its DEFINING module. Erasing that makes
# a.P_get and b.P_get one identity — an import laundering one type's method into another.
# Written as an `if` rather than `""` so `defModule` stays referenced; both the bare
# constant and `.take 0` leave it unused, which makes the mutation INVALID.
MUT_FILE+=("Concrete/Elab/Elab.lean")
MUT_OLD+=("          some (localKey, CallableId.ofUser defModule declName sig.typeParams.length)")
MUT_NEW+=("          some (localKey, CallableId.ofUser (if defModule == \"\" then \"\" else \"\") declName sig.typeParams.length) -- MUTATION: defining module erased")
MUT_DESC+=("imported impl method: identity loses its defining module")
gate_for_last "scripts/tests/check_shadow_body_v2.sh"

# 58. An unclassified node constructor. Adding one to BodyIdentityUse must be
# REJECTED, and this mutation adds a fallback arm at the same time so the exhaustive
# match still compiles — the case a type-checker alone cannot catch. The reflective
# constructor-count check is what must object.
MUT_FILE+=("Concrete/Proof/IdentityUseBytes.lean")
MUT_OLD+=("def allNodeTags : List String := [\"b\", \"t\", \"f\", \"v\"]")
MUT_NEW+=("def allNodeTags : List String := [\"b\", \"t\", \"f\"] -- MUTATION: a node kind left unclassified")
MUT_DESC+=("V2 serializer: a node kind is left out of the tag inventory")
gate_for_last "scripts/tests/check_identity_use_bytes.sh"

# 59. The serializer emits nodes without their count, so a truncated stream reads as
# a shorter complete one.
MUT_FILE+=("Concrete/Proof/IdentityUseBytes.lean")
MUT_OLD+=("    some (\"identityUsesV1:n\" ++ toString inputs.uses.length ++ \":\" ++ body)")
MUT_NEW+=("    some (\"identityUsesV1:\" ++ body) -- MUTATION: no node count")
MUT_DESC+=("V2 serializer: node count omitted from the stream")
gate_for_last "scripts/tests/check_identity_use_bytes.sh"

# 60. An UNCOVERED body is serialized anyway, presenting a partial body as complete —
# worse than no bytes, because it looks authoritative.
MUT_FILE+=("Concrete/Proof/IdentityUseBytes.lean")
MUT_OLD+=("  if !inputs.covered then none")
MUT_NEW+=("  if false then none -- MUTATION: serialize an uncovered body as complete")
MUT_DESC+=("V2 serializer: uncovered body serialized as complete")
gate_for_last "scripts/tests/check_identity_use_bytes.sh"

# 61. Length prefixes dropped, so the concatenated stream stops being injective.
MUT_FILE+=("Concrete/Proof/IdentityUseBytes.lean")
MUT_OLD+=("      let p := s!\"{out}:{idx}\"
      \"b\" ++ toString p.length ++ \":\" ++ p")
MUT_NEW+=("      let p := s!\"{out}{idx}\"
      \"b\" ++ p -- MUTATION: no length prefix, no separator")
MUT_DESC+=("V2 serializer: binder encoding loses its length prefix")
gate_for_last "scripts/tests/check_identity_use_bytes.sh"

# 79–82. R-0484 R10 assumption summaries (check_assumption_summary.sh). Two ways to DROP an
# assumption — one a function reaches directly, one it inherits through a callee — and two ways
# to report coverage as falsely COMPLETE: losing a function's own indirect calls, and losing
# the ones it inherits. Each guard is disabled rather than its line deleted, so every binding
# stays live and the mutant builds.
MUT_FILE+=("Concrete/Report/AssumptionSummary.lean")
MUT_OLD+=("        if !(rs.any (·.key == key)) then")
MUT_NEW+=("        if false && !(rs.any (·.key == key)) then -- MUTATION: direct binding hit dropped")
MUT_DESC+=("assumption summary: a directly reached binding is dropped")
gate_for_last "scripts/tests/check_assumption_summary.sh"

MUT_FILE+=("Concrete/Report/AssumptionSummary.lean")
MUT_OLD+=("            if !keys.contains r.key then keys := keys.push r.key")
MUT_NEW+=("            if false && !keys.contains r.key then keys := keys.push r.key -- MUTATION: inherited assumption dropped")
MUT_DESC+=("assumption summary: an assumption inherited through a callee is dropped")
gate_for_last "scripts/tests/check_assumption_summary.sh"

MUT_FILE+=("Concrete/Report/AssumptionSummary.lean")
MUT_OLD+=("    ownGaps := ownGaps.set! i ((nd.indirect.toArray.map fun b => { site := nd.fn, siteKey := nd.fnKey, binding := b }) ++ unloadedGaps)")
MUT_NEW+=("    ownGaps := ownGaps.set! i ((nd.indirect.toArray.filter fun _ => false).map fun b => { site := nd.fn, siteKey := nd.fnKey, binding := b }) -- MUTATION: own unresolved edges ignored")
MUT_DESC+=("assumption summary: a function's own indirect call no longer makes it incomplete")
gate_for_last "scripts/tests/check_assumption_summary.sh"

MUT_FILE+=("Concrete/Report/AssumptionSummary.lean")
MUT_OLD+=("            if !gs.contains g then gs := gs.push g")
MUT_NEW+=("            if false && !gs.contains g then gs := gs.push g -- MUTATION: inherited gap dropped")
MUT_DESC+=("assumption summary: an inherited indirect-call gap is dropped (falsely complete)")
gate_for_last "scripts/tests/check_assumption_summary.sh"

# 83. Propagation INSIDE a recursive group: members that reach a binding only through another
# member of the same group lose it. The ping/pong fixture rows catch it.
MUT_FILE+=("Concrete/Report/AssumptionSummary.lean")
MUT_OLD+=("      while changed do")
MUT_NEW+=("      while false && changed do -- MUTATION: no propagation within a recursive group")
MUT_DESC+=("assumption summary: a recursive group's members do not share what the group reaches")
gate_for_last "scripts/tests/check_assumption_summary.sh"

# 84–85. R10 conclusion qualification: a capability conclusion stops naming the foreign bindings
# it rests on, and "pure" stops requiring complete coverage. The cross-surface section of
# check_assumption_summary.sh (caps text vs diagnostics-json vs assumptions JSON) catches both.
MUT_FILE+=("Concrete/Report/AssumptionSummary.lean")
MUT_OLD+=("  if bs.isEmpty then s!\"{cap} (declared); reached foreign assumptions: none\"")
MUT_NEW+=("  if true || bs.isEmpty then s!\"{cap} (declared); reached foreign assumptions: none\" -- MUTATION: bindings dropped")
MUT_DESC+=("qualification: a capability conclusion no longer names the bindings it rests on")
gate_for_last "scripts/tests/check_assumption_summary.sh"

MUT_FILE+=("Concrete/Report/AssumptionSummary.lean")
MUT_OLD+=("  declared.isEmpty && s.complete && (t.foreignFacts s).isEmpty")
MUT_NEW+=("  declared.isEmpty && (s.complete || true) && (t.foreignFacts s).isEmpty -- MUTATION: purity past a gap")
MUT_DESC+=("qualification: effect-free is claimed past an unresolved call")
gate_for_last "scripts/tests/check_assumption_summary.sh"

NUM_MUTATIONS=${#MUT_FILE[@]}
# PINNED, not self-denominating. Every downstream count derives from this, so deleting families
# silently shrank the population a "full" run reported on. Retiring a mutation withdraws the evidence
# that some gate is load-bearing and must be a recorded decision.
EXPECTED_MUTATIONS=85
if [ "$NUM_MUTATIONS" != "$EXPECTED_MUTATIONS" ]; then
  echo "FATAL: the mutation inventory holds $NUM_MUTATIONS families, pinned at $EXPECTED_MUTATIONS." >&2
  echo "       If this change is intended, update EXPECTED_MUTATIONS in the SAME commit and say" >&2
  echo "       in the message which families moved and why." >&2
  exit 2
fi

# ============================================================
# Argument parsing
# ============================================================

MODE="run"
SINGLE_IDX=0

while [[ $# -gt 0 ]]; do
  case "$1" in
    --list)
      MODE="list"
      shift
      ;;
    --mutation)
      MODE="single"
      SINGLE_IDX="$2"
      shift 2
      ;;
    --check-patterns)
      MODE="check-patterns"
      shift
      ;;
    *)
      echo "Unknown option: $1"
      echo "Usage: bash scripts/tests/test_mutation.sh [--list] [--mutation N]"
      exit 1
      ;;
  esac
done

# ============================================================
# Pattern-freshness mode
# ============================================================
# A mutation whose MUT_OLD no longer occurs in its file reports
# "SKIPPED (pattern not found in file)" — it stops testing anything while still
# LOOKING like part of the suite. Refactoring the compiler silently retires
# mutations this way, and a suite that has quietly stopped covering a property
# is worse than a missing one, because the summary line still counts it.
#
# This iterates the REAL arrays the harness applies, so it cannot drift from
# them the way a separate parser of this file would (a re-parsing check
# mis-handled multi-line MUT_OLD entries and reported 17 false stalenesses).
# It touches no files and runs in about a second, so it is a cheap gate.
if [[ "$MODE" == "check-patterns" ]]; then
  echo "=== Mutation pattern freshness ($NUM_MUTATIONS mutations) ==="
  stale=0
  for (( i=0; i<NUM_MUTATIONS; i++ )); do
    f="${MUT_FILE[$i]}"
    if [[ ! -f "$f" ]]; then
      printf "  STALE  [%2d] missing file %s\n" "$((i+1))" "$f"
      stale=$((stale + 1)); continue
    fi
    # Count exact literal occurrences; awk avoids regex interpretation of the
    # pattern (these contain ., |, =>, ( and would misbehave under grep).
    n=$(awk -v pat="${MUT_OLD[$i]}" '
      BEGIN { RS="\0"; n=0 }
      { s=$0; l=length(pat); if (l==0) { print 0; exit }
        p=1
        while ((k=index(substr(s,p),pat)) > 0) { n++; p=p+k+l-1 }
        print n }' "$f")
    if [[ "$n" != "1" ]]; then
      printf "  STALE  [%2d] %s occurs %s time(s) in %s\n      %s\n" \
        "$((i+1))" "MUT_OLD" "$n" "$f" "${MUT_DESC[$i]}"
      stale=$((stale + 1))
    fi
  done
  if [[ $stale -gt 0 ]]; then
    echo ""
    echo "FAIL: $stale mutation(s) would SKIP rather than test."
    echo "A mutation must match its target EXACTLY ONCE: zero means the code moved,"
    echo "more than one means the harness would patch an unintended site too."
    exit 1
  fi
  echo "PASS: all $NUM_MUTATIONS mutation patterns match their target exactly once"
  exit 0
fi

# ============================================================
# List mode
# ============================================================

if [[ "$MODE" == "list" ]]; then
  echo "=== Mutation List ($NUM_MUTATIONS mutations) ==="
  for (( i=0; i<NUM_MUTATIONS; i++ )); do
    idx=$((i + 1))
    printf "  [%2d/%d] %-30s %s\n" "$idx" "$NUM_MUTATIONS" "${MUT_FILE[$i]}:" "${MUT_DESC[$i]}"
  done
  exit 0
fi

# ============================================================
# Apply / restore mutation using exact string replacement
# ============================================================

# CLEAN BASELINE for every kill-evidence producer, measured BEFORE ANY MUTATION IS APPLIED.
#
# A gate that is already red proves nothing by going red again, and this harness had no such control
# at all: it read every nonzero exit as a kill. The first attempt at a fix was worse than useless —
# it called the baseline from inside `run_mutation`, AFTER the mutation had been applied and built, so
# a mutation that correctly turned its gate red was labelled "red on clean" and a mutation that left
# the gate green poisoned the cache for every later family. A baseline measured after the mutation is
# not a baseline.
#
# There are TWO producers of kill evidence — the named gate and the fast suite — and both need a
# control. Only the named gate had one; a fast suite already red for an unrelated reason manufactured
# kills exactly as a red gate did.
declare -A MUT_CLEAN_GATE
# The campaign's end-of-run control, which this harness lacked entirely: a gate that reached a verdict
# on pristine source must reach one under mutation too, or its nonzero exit is infrastructural rather
# than rule evidence. Digits are normalised so the SHAPE of the final line is what must match, since
# its counts move under mutation.
declare -A MUT_CLEAN_TAIL
_mut_tail_shape() { awk 'NF {last=$0} END {gsub(/[0-9]+/, "#", last); print last}' "$1" 2>/dev/null; }
MUT_CLEAN_FAST="unknown"
MUT_FRESHNESS_TAINT=0
baseline_clean_producers() {
  local g total=0 red=0
  local -a uniq=()
  for (( _b=0; _b<NUM_MUTATIONS; _b++ )); do
    g="${MUT_GATE[$_b]:-}"
    [[ -n "$g" ]] || continue
    [[ -z "${MUT_CLEAN_GATE[$g]:-}" ]] || continue
    MUT_CLEAN_GATE[$g]=pending; uniq+=("$g")
  done
  printf "baseline: fast suite on pristine source ... "
  if bash scripts/tests/run_tests.sh --fast > "$MUT_LOG_DIR/fast_clean.log" 2>&1; then
    MUT_CLEAN_FAST=yes
    # Its pristine end shape, so a mutated run that dies before reaching it is not read as a kill —
    # the same control the named gates get. Without this, SIGKILL, exit 126/127 or a startup failure
    # manufactured `KILLED (fast suite)`.
    MUT_CLEAN_FAST_TAIL="$(_mut_tail_shape "$MUT_LOG_DIR/fast_clean.log")"
    echo "green"
  else
    MUT_CLEAN_FAST=no; echo "RED — fast-suite kills are not evidence in this run"
  fi
  total=${#uniq[@]}
  echo "baseline: $total distinct named gate(s) on pristine source"
  for g in "${uniq[@]}"; do
    if bash "$g" > "$MUT_LOG_DIR/gate_clean.log" 2>&1; then
      MUT_CLEAN_GATE[$g]=yes
      MUT_CLEAN_TAIL[$g]="$(_mut_tail_shape "$MUT_LOG_DIR/gate_clean.log")"
    else
      MUT_CLEAN_GATE[$g]=no; red=$((red+1))
      echo "  RED ON CLEAN: $g — families naming it will be reported ERROR, not killed" >&2
    fi
  done
  echo "  baseline: $((total-red))/$total named gates green on pristine source"
}
gate_clean_ok() { # gate-path — was it green on PRISTINE source?
  [[ "${MUT_CLEAN_GATE[$1]:-no}" == "yes" ]]
}

apply_mutation() {
  local idx=$1
  local file="${MUT_FILE[$idx]}"
  local old="${MUT_OLD[$idx]}"
  local new="${MUT_NEW[$idx]}"

  # Backup into the unique temp dir, NOT beside the source. A `<file>.mutbak` in
  # the tree is itself state left behind — one was staged into a commit before
  # being caught — and two runs racing on that path is what corrupted
  # Proof.lean. The key flattens the path so nested files cannot collide.
  # Mirror the path INSIDE the backup dir rather than flattening it. `tr / _`
  # collides (`a/b_c` and `a_b/c` both become `a_b_c`) and is not invertible, so
  # the restore loop had to guess which file a backup belonged to.
  # THE BACKUP IS VERIFIED BEFORE THE FILE IS TOUCHED. Both of these were unchecked, and `set -e`
  # does NOT help: this function is invoked as `if ! apply_mutation ...`, and bash suppresses errexit
  # for the whole function body in that context. So a full disk or an unwritable TMPDIR let the copy
  # fail and the python below mutate real compiler source with NO backup in existence. Byte-compared,
  # not merely attempted: a partial copy is worse than none, because it looks like a backup.
  if ! mkdir -p "$MUT_BACKUP_DIR/$(dirname "$file")"; then
    echo "  FATAL: could not create the backup directory for $file — refusing to mutate." >&2
    return 2
  fi
  # STAGED UNDER A NON-AUTHORITATIVE NAME, THEN ACTIVATED BY RENAME.
  #
  # Copying straight to the path cleanup treats as authoritative meant an interrupt DURING the copy
  # left a PARTIAL file there — and the signal trap then walks every such file, stages it, verifies it
  # only against itself, and renames it over the original. A terminal interrupt reaching both bash and
  # `cp` could therefore replace tracked compiler source with a truncated copy, and cleanup deletes
  # the backup directory afterwards. A partial backup must never be reachable under the name that
  # means "this is the original".
  local bak_stage="$MUT_BACKUP_DIR/$file.staging"
  if ! cp "$file" "$bak_stage"; then
    echo "  FATAL: could not back up $file — refusing to mutate." >&2
    rm -f "$bak_stage"
    return 2
  fi
  if [ "$(hash_of "$file")" != "$(hash_of "$bak_stage")" ]; then
    echo "  FATAL: backup of $file does not match the original — refusing to mutate." >&2
    rm -f "$bak_stage"
    return 2
  fi
  if ! mv -f "$bak_stage" "$MUT_BACKUP_DIR/$file"; then
    echo "  FATAL: could not activate the backup of $file — refusing to mutate." >&2
    rm -f "$bak_stage"
    return 2
  fi

  # WRITTEN TO A SIBLING TEMP FILE, HASHED, THEN INSTALLED BY RENAME.
  #
  # The previous form wrote the target in place and then re-read it to record what had been written.
  # Between those two steps a concurrent editor's save would be hashed as OUR mutation, after which
  # the restore below would overwrite it as if it were ours — the exact loss the recorded hash exists
  # to prevent, one step earlier. Writing a sibling and renaming makes installation atomic and lets
  # the recorded hash describe exactly the bytes installed, because it is computed before they are.
  local tmp_out="$file.mutwrite.$$"
  python3 -c "
import sys
src = sys.argv[1]
dst = sys.argv[2]
old = sys.argv[3]
new = sys.argv[4]
with open(src, 'r') as f:
    content = f.read()
# EXACTLY ONE OCCURRENCE, ENFORCED HERE — not only in --check-patterns.
#
# This tested presence and then used replace(..., 1), so an ambiguous anchor mutated whichever site
# came first and produced a perfectly green run testing something nobody chose. The freshness mode
# already required uniqueness, so the two disagreed and the WEAKER one was the code that actually
# mutates. A checker stricter than its applier is the same defect as an applier stricter than its
# checker: the authority has to be the thing that acts.
# SINGLE QUOTES ONLY inside this payload: it is passed via `python3 -c "..."`, so a double quote
# here TERMINATES the bash string and python receives truncated source. That is why the surrounding
# code uses 'r'/'w' rather than "r"/"w".
n = content.count(old)
if n < 1:
    sys.stderr.write('anchor not found\n')
    sys.exit(1)
if n > 1:
    sys.stderr.write('anchor is AMBIGUOUS: %d occurrences\n' % n)
    sys.exit(3)
content = content.replace(old, new, 1)
with open(dst, 'w') as f:
    f.write(content)
" "$file" "$tmp_out" "$old" "$new"
  local rc=$?
  if [ "$rc" -ne 0 ]; then
    rm -f "$tmp_out"
    return $rc
  fi
  local want; want="$(hash_of "$tmp_out")"
  if [ -z "$want" ]; then
    echo "  FATAL: could not hash the mutated content for $file — refusing to install it." >&2
    rm -f "$tmp_out"
    return 2
  fi
  # OWNERSHIP RE-CHECKED IMMEDIATELY BEFORE INSTALLING. Backing up and then renaming over the target
  # without re-reading it meant a user save landing after the backup was folded into
  # MUT_HASH_APPLIED — and then erased by the restore as though the harness had written it. This is
  # the same class as the restore-side check, and it was missing on the apply side entirely.
  if [ "$(hash_of "$file")" != "$(hash_of "$MUT_BACKUP_DIR/$file")" ]; then
    echo "  FATAL: $file changed between backup and mutation — refusing to overwrite another writer." >&2
    echo "         Backup: $MUT_BACKUP_DIR/$file" >&2
    rm -f "$tmp_out"
    MUT_CONCURRENT=1
    return 1
  fi
  # RECORDED BEFORE ACTIVATION, AND RE-CHECKED IMMEDIATELY BEFORE IT — WHICH NARROWS THE RACE, IT DOES
  # NOT CLOSE IT. A save landing between this final hash read and the `mv -f` below is still
  # overwritten, and is then indistinguishable from content this harness installed. Every check/rename
  # pair in this file has that residual, including the normal restore, which says so at its own site.
  # Closing it needs a lock the editors here do not participate in; the remedy is operational — run
  # mutation campaigns in a dedicated worktree (`scripts/worktree-new.sh`), not the tree you are
  # editing. I am recording the bound rather than claiming a fix.
  #
  # Two gaps here. The ownership check happened well above and the rename below, so a save landing
  # between them was overwritten — and the later restore then saw the installed mutation, judged it
  # ours, and restored the older backup over the save. And `MUT_HASH_APPLIED` was recorded only AFTER
  # the rename, so a signal in that window made cleanup misclassify the harness's own mutation as a
  # concurrent edit and strand it. Recording first is safe: if the rename never happens the file still
  # holds pristine bytes, which the restore guard already recognises.
  MUT_HASH_APPLIED["$file"]="$want"
  if [ "$(hash_of "$file")" != "$(hash_of "$MUT_BACKUP_DIR/$file")" ]; then
    echo "  FATAL: $file changed just before the mutation was installed — refusing to overwrite." >&2
    rm -f "$tmp_out"
    MUT_CONCURRENT=1
    return 1
  fi
  # Same directory, so this is a rename rather than a copy: no window in which the target is partial.
  if ! mv -f "$tmp_out" "$file"; then
    echo "  FATAL: could not install the mutation into $file." >&2
    rm -f "$tmp_out"
    return 2
  fi
  return 0
}

restore_mutation() {
  local idx=$1
  local file="${MUT_FILE[$idx]}"
  local bak="$MUT_BACKUP_DIR/$file"
  if [ -f "$bak" ]; then
    # DO NOT clobber someone else's work. If the file on disk is neither what we
    # wrote nor the original, a concurrent writer edited it while the mutation was
    # applied, and copying the backup over it DESTROYS that edit — then the
    # postcondition check compares against the PRE-mutation hash and reports
    # "restored exactly", confirming the loss. That happened: an edit to
    # ProofCore.lean vanished mid-session and the file dropped out of git status.
    local now; now="$(hash_of "$file")"
    local applied="${MUT_HASH_APPLIED[$file]:-}"
    # THE GUARD MUST NOT DEPEND ON HAVING INSTALLED SOMETHING.
    #
    # It required `applied` to be non-empty, so when the apply path DETECTED a concurrent edit and
    # refused to install, `applied` stayed unset — and this guard let the restore proceed and rename
    # the backup straight over the edit it had just protected. A fix that detects the loss and then
    # causes it is worse than no fix. The correct rule needs no knowledge of what we installed: the
    # on-disk content must be either what we wrote or the pristine original; anything else belongs to
    # someone else.
    if [ "$now" != "${MUT_HASH0[$file]:-}" ] \
       && { [ -z "$applied" ] || [ "$now" != "$applied" ]; }; then
      local rescue="$MUT_BACKUP_DIR/CONCURRENT-EDIT/$file"
      mkdir -p "$(dirname "$rescue")"; cp "$file" "$rescue"
      echo "" >&2
      echo "  FATAL: $file changed while a mutation was applied." >&2
      echo "         On-disk content is neither our mutation nor the original, so" >&2
      echo "         another writer edited it. Refusing to overwrite." >&2
      echo "         Their version: $rescue" >&2
      echo "         Our backup   : $bak" >&2
      echo "         Reconcile by hand. This harness needs exclusive use of the" >&2
      echo "         worktree; use a separate one (scripts/worktree-new.sh)." >&2
      MUT_CONCURRENT=1
      return 1
    fi
    # THE COPY IS VERIFIED BEFORE THE BACKUP IS DELETED. This was `cp` then an unconditional
    # `rm -f "$bak"`, so a failed or PARTIAL copy — full disk, read-only file, interrupted write —
    # destroyed the only copy of the original and left real compiler source mutated or truncated. No
    # later hash check can recover bytes that no longer exist anywhere. The backup is removed only
    # once the restored file is byte-identical to it.
    # STAGED BESIDE THE TARGET, VERIFIED, THEN RENAMED — and the ownership check above is repeated
    # immediately before the rename. A plain `cp` over the target leaves it partial while copying, so
    # an interrupted restore produced a truncated compiler source; and the gap between the ownership
    # check and the overwrite was a window in which a concurrent save was destroyed. The rename is
    # atomic, so the target is either the mutation or the original and never something in between.
    #
    # RESIDUAL, stated rather than implied: a save landing between the final re-check and the rename
    # is still lost. Closing that completely needs file locking the editors here do not participate
    # in. The window is now a single re-hash plus a rename rather than a whole file copy.
    local tmp_in="$file.mutrestore.$$"
    if ! cp "$bak" "$tmp_in"; then
      echo "" >&2
      echo "  FATAL: could not stage the restore of $file." >&2
      echo "         The backup is PRESERVED at: $bak" >&2
      rm -f "$tmp_in"
      ERRORS=$((ERRORS + 1)); INFRA=$((INFRA + 1)); RESTORE_BUILD_FAILED=1
      return 1
    fi
    if [ "$(hash_of "$tmp_in")" != "$(hash_of "$bak")" ]; then
      echo "" >&2
      echo "  FATAL: staged restore of $file does not match the backup — the copy was partial." >&2
      echo "         The backup is PRESERVED at: $bak" >&2
      rm -f "$tmp_in"
      ERRORS=$((ERRORS + 1)); INFRA=$((INFRA + 1)); RESTORE_BUILD_FAILED=1
      return 1
    fi
    local recheck; recheck="$(hash_of "$file")"
    if [ "$recheck" != "$now" ]; then
      echo "" >&2
      echo "  FATAL: $file changed again while the restore was being staged. Refusing to overwrite." >&2
      echo "         The backup is PRESERVED at: $bak" >&2
      rm -f "$tmp_in"
      MUT_CONCURRENT=1
      return 1
    fi
    if ! mv -f "$tmp_in" "$file"; then
      echo "" >&2
      echo "  FATAL: could not install the restored $file. Backup PRESERVED at: $bak" >&2
      rm -f "$tmp_in"
      ERRORS=$((ERRORS + 1)); INFRA=$((INFRA + 1)); RESTORE_BUILD_FAILED=1
      return 1
    fi
    rm -f "$bak"
  else
    # FATAL, not a warning. A warning returned zero and the run continued with real source possibly
    # still mutated.
    echo "  FATAL: no backup for $file — cannot restore. The file may still be mutated." >&2
    ERRORS=$((ERRORS + 1)); INFRA=$((INFRA + 1))
    RESTORE_BUILD_FAILED=1
  fi
  # Rebuild so the tree's BINARY matches its restored SOURCE. Restoring only the
  # source leaves `.lake/build/bin/concrete` built from the mutation, and anything
  # run afterwards — a gate, a probe, another script — silently measures the
  # mutated compiler while the source looks clean. That is a trap this harness
  # has sprung on its callers more than once, and it costs more than the rebuild.
  # A WARNING IS NOT A CONTROL. This printed to stderr and returned ZERO, so the run continued with
  # `.lake` holding a binary built from the mutation — and every later family's verdict, plus anything
  # the operator ran afterwards, measured that binary while the SOURCE reconciled as clean. The
  # harness could finish with no survivors and no errors on exactly that state. Source hashes agreeing
  # is not the same as the build artifacts agreeing, and this is the gap between them.
  if ! $LAKE build > "$MUT_LOG_DIR/restore_build.log" 2>&1; then
    echo "" >&2
    echo "  FATAL: rebuild after restore FAILED — .lake now holds a binary built from a mutation." >&2
    echo "         Every later verdict would describe that binary while the source looks clean." >&2
    echo "         See $MUT_LOG_DIR/restore_build.log, then rebuild before running anything else." >&2
    ERRORS=$((ERRORS + 1)); INFRA=$((INFRA + 1))
    RESTORE_BUILD_FAILED=1
    return 1
  fi
  return 0
}

# ============================================================
# Run a single mutation
# ============================================================

# CONFIRMATION HELPERS — red / green / red, without disturbing the backup.
#
# `restore_mutation` is the END-OF-FAMILY operation: it consumes the backup. Confirmation happens in
# the MIDDLE of a family, so it swaps content directly and leaves the backup for the normal restore.
CONFIRM_WHY=""
# EVERY SWAP CHECKS WHAT IT IS OVERWRITING.
#
# The first version renamed over the target unconditionally. That is worse than the narrow
# check-to-rename window documented elsewhere: these helpers hold PRISTINE source across a full
# rebuild and gate run — many minutes — and then overwrite it with the mutation. A save landing
# anywhere in that interval was destroyed silently, and the final restore saw the expected mutation
# bytes and could not tell anything had been lost.
#
# So each swap states what it expects to find and refuses if the file is something else. The caller
# treats that refusal as fatal and keeps the user's bytes.
_confirm_swap() { # file  from-path  expected-hash-or-empty
  local now
  if [ -n "${3:-}" ]; then
    now="$(hash_of "$1")"
    if [ "$now" != "$3" ]; then
      CONFIRM_WHY="$1 was modified by another writer during confirmation — refusing to overwrite it"
      MUT_CONCURRENT=1
      return 1
    fi
  fi
  # STAGED FIRST, THEN RE-CHECKED IMMEDIATELY BEFORE THE RENAME. Checking on entry and renaming after
  # the copy leaves a much longer gap — but this is still a narrowing, not a closure: a save landing
  # between the check and the `mv -f` is overwritten. Same residual as every other check/rename pair
  # here; same operational remedy (a dedicated worktree).
  cp "$2" "$1.confirmswap.$$" || return 1
  if [ -n "${3:-}" ] && [ "$(hash_of "$1")" != "$3" ]; then
    rm -f "$1.confirmswap.$$"
    CONFIRM_WHY="$1 changed while the swap was being staged — refusing to overwrite it"
    MUT_CONCURRENT=1
    return 1
  fi
  mv -f "$1.confirmswap.$$" "$1"
}
# THE BINARY MUST FOLLOW THE SOURCE. This harness mutates IN PLACE and its own restore path already
# rebuilds for exactly this reason: source-only restoration leaves `.lake` holding a binary built from
# the mutation, so anything run afterwards measures the mutated compiler while the source looks clean.
# The confirmation swaps source three times, so it has to rebuild after each swap or its "green" leg
# tests a source/binary pair that was never built together.
_confirm_rebuild() { # label
  $LAKE build > "$MUT_LOG_DIR/confirm_build_$1.log" 2>&1
}
# THE CONFIRMING RED LEG IS AUTHENTICATED, exactly like the first one. Accepting any nonzero here was
# the same defect as the campaign's unauthenticated confirming leg — a confirming exit 97 or
# precondition failure would have become a KILL.
_confirm_red_ok() { # log rc expected-tail-shape
  [ "$2" -ne 0 ] || return 1
  [ "$2" -ne 97 ] || { CONFIRM_WHY="confirming run exited 97 (died early), not a verdict"; return 1; }
  if grep -q 'GATE-PRECONDITION-FAILED:' "$1" 2>/dev/null; then
    CONFIRM_WHY="confirming run hit a precondition failure, so its assertions never ran"; return 1
  fi
  # THE END-SHAPE CHECK TOO. Without it this accepted exit 126/127, a shell failure, or any premature
  # non-97 exit — so the confirming leg was still weaker than the first leg it claimed to match.
  if [ -n "${3:-}" ] && [ "$(_mut_tail_shape "$1")" != "$3" ]; then
    CONFIRM_WHY="confirming run never reached the end it reaches on pristine source"; return 1
  fi
  return 0
}
confirm_gate_kill() { # idx gate
  local idx="$1" gate="$2"
  local file="${MUT_FILE[$idx]}" bak="$MUT_BACKUP_DIR/${MUT_FILE[$idx]}"
  CONFIRM_WHY=""
  [ -f "$bak" ] || { CONFIRM_WHY="no backup available to confirm against"; return 1; }
  # THE EXPECTED HASH COMES FROM `MUT_HASH_APPLIED`, the record of what this harness INSTALLED — not
  # from whatever is on disk now. Deriving it from disk BLESSED a concurrent save as "the expected
  # mutation" and then overwrote it; and if a signal arrived during the pristine leg, cleanup saw
  # pristine bytes, did not flag concurrency, and deleted the log holding the only copy of the user's
  # version. Checking against what we wrote is the entire point of having recorded it.
  local mut_hash pristine_hash
  mut_hash="${MUT_HASH_APPLIED[$file]:-}"
  [ -n "$mut_hash" ] || { CONFIRM_WHY="no recorded applied-hash for $file; refusing to confirm"; return 1; }
  pristine_hash="$(hash_of "$bak")"
  if [ "$(hash_of "$file")" != "$mut_hash" ]; then
    CONFIRM_WHY="$file is not the content this harness installed — another writer changed it"
    MUT_CONCURRENT=1
    return 1
  fi
  cp "$file" "$MUT_LOG_DIR/mutated.keep" || { CONFIRM_WHY="could not stash the mutated content"; return 1; }
  _confirm_swap "$file" "$bak" "$mut_hash" || return 1
  _confirm_rebuild green || { _confirm_swap "$file" "$MUT_LOG_DIR/mutated.keep" "$pristine_hash"
    CONFIRM_WHY="restored source did not rebuild, so the green leg could not be measured"; return 1; }
  local rc=0
  bash "$gate" > "$MUT_LOG_DIR/gate_confirm.log" 2>&1 || rc=$?
  if [ "$rc" -ne 0 ] \
     || { [ -n "${MUT_CLEAN_TAIL[$gate]:-}" ] \
          && [ "$(_mut_tail_shape "$MUT_LOG_DIR/gate_confirm.log")" != "${MUT_CLEAN_TAIL[$gate]}" ]; }; then
    _confirm_swap "$file" "$MUT_LOG_DIR/mutated.keep" "$pristine_hash" || return 1
    _confirm_rebuild restore >/dev/null 2>&1 || true
    CONFIRM_WHY="still red after restoring the source — not attributable to the mutation"
    return 1
  fi
  # second red leg: the file must still hold the PRISTINE bytes we just installed
  _confirm_swap "$file" "$MUT_LOG_DIR/mutated.keep" "$pristine_hash" || return 1
  _confirm_rebuild red2 || { CONFIRM_WHY="re-applied mutation did not rebuild for the second red leg"; return 1; }
  rc=0
  bash "$gate" > "$MUT_LOG_DIR/gate_confirm2.log" 2>&1 || rc=$?
  if [ "$rc" -eq 0 ]; then
    CONFIRM_WHY="green when the mutation was re-applied — the earlier red was not reproducible"
    return 1
  fi
  _confirm_red_ok "$MUT_LOG_DIR/gate_confirm2.log" "$rc" "${MUT_CLEAN_TAIL[$gate]:-}" || return 1
  return 0
}
confirm_fast_kill() { # idx
  local idx="$1"
  local file="${MUT_FILE[$idx]}" bak="$MUT_BACKUP_DIR/${MUT_FILE[$idx]}"
  CONFIRM_WHY=""
  [ -f "$bak" ] || { CONFIRM_WHY="no backup available to confirm against"; return 1; }
  # Same rule as the named-gate helper: the expected bytes are the ones we installed.
  local mut_hash pristine_hash
  mut_hash="${MUT_HASH_APPLIED[$file]:-}"
  [ -n "$mut_hash" ] || { CONFIRM_WHY="no recorded applied-hash for $file; refusing to confirm"; return 1; }
  pristine_hash="$(hash_of "$bak")"
  if [ "$(hash_of "$file")" != "$mut_hash" ]; then
    CONFIRM_WHY="$file is not the content this harness installed — another writer changed it"
    MUT_CONCURRENT=1
    return 1
  fi
  cp "$file" "$MUT_LOG_DIR/mutated.keep" || { CONFIRM_WHY="could not stash the mutated content"; return 1; }
  _confirm_swap "$file" "$bak" "$mut_hash" || return 1
  _confirm_rebuild fastgreen || { _confirm_swap "$file" "$MUT_LOG_DIR/mutated.keep" "$pristine_hash"
    CONFIRM_WHY="restored source did not rebuild, so the green leg could not be measured"; return 1; }
  local rc=0
  bash scripts/tests/run_tests.sh --fast > "$MUT_LOG_DIR/fast_confirm.log" 2>&1 || rc=$?
  if [ "$rc" -ne 0 ]; then
    _confirm_swap "$file" "$MUT_LOG_DIR/mutated.keep" "$pristine_hash" || return 1
    _confirm_rebuild fastrestore >/dev/null 2>&1 || true
    CONFIRM_WHY="fast suite still red after restoring the source"
    return 1
  fi
  _confirm_swap "$file" "$MUT_LOG_DIR/mutated.keep" "$pristine_hash" || return 1
  _confirm_rebuild fastred2 || { CONFIRM_WHY="re-applied mutation did not rebuild for the second red leg"; return 1; }
  rc=0
  bash scripts/tests/run_tests.sh --fast > "$MUT_LOG_DIR/fast_confirm2.log" 2>&1 || rc=$?
  [ "$rc" -ne 0 ] || { CONFIRM_WHY="fast suite green when the mutation was re-applied"; return 1; }
  _confirm_red_ok "$MUT_LOG_DIR/fast_confirm2.log" "$rc" "${MUT_CLEAN_FAST_TAIL:-}" || return 1
  return 0
}

run_mutation() {
  local num=$1       # 1-based index for display
  local idx=$((num - 1))  # 0-based index for arrays

  printf "[%2d/%d] %-30s %-45s ... " "$num" "$NUM_MUTATIONS" "${MUT_FILE[$idx]}:" "${MUT_DESC[$idx]}"

  # Apply mutation
  if ! apply_mutation "$idx"; then
    echo "SKIPPED (pattern not found in file)"
    ERRORS=$((ERRORS + 1)); INVALID=$((INVALID + 1))
    TOTAL=$((TOTAL + 1))
    # Restore if a backup was created. This guard tested `<file>.mutbak` for a
    # while after backups MOVED to $MUT_BACKUP_DIR, so it was never true and a
    # skipped mutation restored nothing — the source stayed mutated and the next
    # mutation backed up the ALREADY-MUTATED file. Test the real backup path.
    [[ -f "$MUT_BACKUP_DIR/${MUT_FILE[$idx]}" ]] && restore_mutation "$idx"
    return
  fi

  local result=""

  # Try to build
  if $LAKE build > "$MUT_LOG_DIR/build.log" 2>&1; then
    # Build succeeded — run tests
    local gate="${MUT_GATE[$idx]:-}"
    # THE NAMED GATE IS ALWAYS CONSULTED, and it is consulted against a CLEAN baseline.
    #
    # Two defects lived in the old ordering. First, a failing fast suite short-circuited to KILLED and
    # the named gate never ran — so a family recorded as killed established nothing about the gate it
    # names, which is the whole point of naming one. Second, nothing ever showed the named gate GREEN
    # on unmutated source, so any gate that was red for an unrelated reason (a missing fixture, a
    # leaked lock, a pre-existing failure) was credited as mutation evidence. That is the same
    # false-green the campaign harness was repaired for on 2026-08-21; it was left live here.
    local gate_verdict="none"
    if [[ -n "$gate" ]]; then
      if gate_clean_ok "$gate"; then
        local grc=0
        bash "$gate" > "$MUT_LOG_DIR/gate.log" 2>&1 || grc=$?
        # THE SAME AUTHENTICATION THE CAMPAIGN APPLIES. This harness accepted ANY nonzero exit as a
        # gate kill: it checked neither the precondition marker nor whether the gate reached a verdict,
        # so a transient build failure inside the gate, a missing toolchain, repository contention, or
        # an early death all counted as "the rule is load-bearing". Two producers of kill evidence and
        # only one of them authenticated it.
        if [[ $grc -eq 0 ]]; then
          gate_verdict="green"
        elif [[ $grc -eq 97 ]]; then
          gate_verdict="died-early"
        elif grep -q 'GATE-PRECONDITION-FAILED:' "$MUT_LOG_DIR/gate.log" 2>/dev/null; then
          gate_verdict="precondition"
        elif [[ -n "${MUT_CLEAN_TAIL[$gate]:-}" ]] \
             && [[ "$(_mut_tail_shape "$MUT_LOG_DIR/gate.log")" != "${MUT_CLEAN_TAIL[$gate]}" ]]; then
          gate_verdict="no-verdict"
        else
          gate_verdict="red"
        fi
        grep -q 'GATE-FRESHNESS-UNVERIFIED' "$MUT_LOG_DIR/gate.log" 2>/dev/null && MUT_FRESHNESS_TAINT=1
      else
        gate_verdict="invalid-baseline"
      fi
    fi
    if [[ "$gate_verdict" == "invalid-baseline" ]]; then
      result="ERROR (gate red on clean source — not evidence)"
      ERRORS=$((ERRORS + 1)); INFRA=$((INFRA + 1))
    elif [[ "$gate_verdict" == "died-early" ]]; then
      result="ERROR ($gate exited 97, its documented died-early code — no verdict, not evidence)"
      ERRORS=$((ERRORS + 1)); INFRA=$((INFRA + 1))
    elif [[ "$gate_verdict" == "precondition" ]]; then
      result="ERROR (a precondition of $gate failed — its assertions never ran, not evidence)"
      ERRORS=$((ERRORS + 1)); INFRA=$((INFRA + 1))
    elif [[ "$gate_verdict" == "no-verdict" ]]; then
      result="ERROR ($gate never reached the end it reaches on pristine source — no verdict, not evidence)"
      ERRORS=$((ERRORS + 1)); INFRA=$((INFRA + 1))
    elif [[ "$gate_verdict" == "red" ]]; then
      # CONFIRMED AGAINST RESTORED SOURCE, as the campaign does. Without this, a one-off unrelated
      # gate failure was valid evidence in this harness while the campaign rejected it — two producers
      # of the same kind of evidence with different standards, which is how this whole arc started.
      # NON-DESTRUCTIVE. Calling `restore_mutation` here DELETED the backup, and the common tail
      # then restored again, found none, and aborted the whole run — so this "fix" made a genuine
      # named-gate kill impossible to accept. The confirmation swaps content directly and leaves the
      # backup exactly where the normal restore expects it.
      if confirm_gate_kill "$idx" "$gate"; then
        result="KILLED (gate, confirmed green once restored)"
        KILLED=$((KILLED + 1))
      else
        result="ERROR ($gate: $CONFIRM_WHY)"
        ERRORS=$((ERRORS + 1)); INFRA=$((INFRA + 1))
      fi
    elif [[ "$MUT_CLEAN_FAST" != "yes" ]]; then
      # The fast suite is not a usable producer in this run, so it cannot supply a kill.
      result="ERROR (fast suite red on pristine source — not evidence)"
      ERRORS=$((ERRORS + 1)); INFRA=$((INFRA + 1))
    else
      # THE FAST SUITE IS AUTHENTICATED THE SAME WAY, and written as a plain sequence rather than a
      # compound condition — the first version of this was a single `elif` with nested `&&`/`||` and
      # an inline assignment, which is unreadable enough to be its own defect.
      local fast_rc=0
      bash scripts/tests/run_tests.sh --fast > "$MUT_LOG_DIR/test.log" 2>&1 || fast_rc=$?
      grep -q 'GATE-FRESHNESS-UNVERIFIED' "$MUT_LOG_DIR/test.log" 2>/dev/null && MUT_FRESHNESS_TAINT=1
      if [[ $fast_rc -eq 0 ]]; then
        result="SURVIVED"
        SURVIVED=$((SURVIVED + 1))
      elif [[ $fast_rc -eq 97 ]]; then
        # Its own documented contract: 97 means the summary file was never written, so no verdict was
        # reached. A code no assertion produces cannot be read as an assertion failing.
        result="ERROR (fast suite exited 97 — died early, no verdict, not evidence)"
        ERRORS=$((ERRORS + 1)); INFRA=$((INFRA + 1))
      elif grep -q 'GATE-PRECONDITION-FAILED:' "$MUT_LOG_DIR/test.log" 2>/dev/null; then
        result="ERROR (a precondition of the fast suite failed — its assertions never ran)"
        ERRORS=$((ERRORS + 1)); INFRA=$((INFRA + 1))
      elif [[ -n "${MUT_CLEAN_FAST_TAIL:-}" ]] \
           && [[ "$(_mut_tail_shape "$MUT_LOG_DIR/test.log")" != "$MUT_CLEAN_FAST_TAIL" ]]; then
        result="ERROR (fast suite never reached the end it reaches on pristine source — no verdict)"
        ERRORS=$((ERRORS + 1)); INFRA=$((INFRA + 1))
      else
        # The fast suite caught it but the named gate did not. Reported as such: a real kill, but not
        # evidence that the named gate is load-bearing.
        # CONFIRMED like a named-gate kill: a one-off failure during the mutated run only, with no
        # reproduction, is not evidence. The fast suite was the last producer still accepting one
        # observation.
        if confirm_fast_kill "$idx"; then
          result="KILLED (fast suite, confirmed; named gate stayed green)"
          KILLED=$((KILLED + 1))
        else
          result="ERROR (fast suite: $CONFIRM_WHY)"
          ERRORS=$((ERRORS + 1)); INFRA=$((INFRA + 1))
        fi
      fi
    fi
  else
    # BUILD FAILED. That is not automatically a kill: the mutation may simply be malformed. The
    # diagnostic has to be attributable to the mutated file, and a failure that is only an
    # unused-binding lint is a broken mutation rather than a type-system rejection. Same distinction
    # the campaign harness makes, which this harness lacked entirely.
    local mfile="${MUT_FILE[$idx]}" base
    base="$(basename "$mfile" .lean)"
    # ONE LINE MUST CARRY BOTH FACTS. Two independent greps over the whole log accepted a generic
    # error from one file and the mutated basename from an unrelated line — so a mutation could be
    # credited with somebody else's diagnostic. Lean emits `path:line:col: error: ...`, so the
    # attribution is a single line naming the mutated file AND carrying the error.
    # THE TARGET FILE'S OWN DIAGNOSTIC BLOCK. A Lean diagnostic is a `path:line:col: error:` header
    # followed by an indented message, so the reason can legitimately sit on a later line — but
    # `grep -A2` also swept in whatever came next, letting an UNRELATED file's diagnostic supply the
    # reason. This keeps only the blocks whose header names the mutated file (header to next header).
    if awk -v f="$mfile" '
         { h = $0; hdr = 0
           # Two header layouts. `path:line:col: error: msg` is the Lean layout; Lake prints
           # `error: path:line:col: msg`. Recognising only the first meant no build kill was
           # ever attributed, and every proof-rejected mutation scored INVALID "for an
           # unrecognised reason" (four campaign families, measured 2026-10-03).
           if (h ~ /^(error|warning|info): [^ ]+:[0-9]+:[0-9]+: /) { sub(/^(error|warning|info): /, "", h); hdr = 1 }
           else if (h ~ /^[^ ].*:[0-9]+:[0-9]+: (error|warning)/) hdr = 1
           if (hdr) { sub(/^(\.\/)+/, "", h); inblk = (index(h, f ":") == 1) } }
         inblk { print }
       ' "$MUT_LOG_DIR/build.log" \
         | grep -qE "unsolved goals|[Tt]ype mismatch|Unknown identifier|Unknown constant|failed to synthesize|Missing cases|declaration uses 'sorry'"; then
      # DECLARED PER FAMILY, exactly as the campaign requires. An attributable type error proves the
      # mutation is unrepresentable, which is a real result — but it also means the family's NAMED
      # GATE never ran, so it is not evidence that the gate is load-bearing. Without a declaration
      # this harness credited every such family as killed and could exit successfully having exercised
      # no gate at all.
      #
      # The allowlist starts EMPTY on purpose: the next full run enumerates exactly which families die
      # in the build, and each is then declared deliberately with a reason. Guessing the list here
      # would be the same unverified assertion this whole exercise exists to remove.
      if build_kill_declared "${MUT_DESC[$idx]}"; then
        result="KILLED (build, declared)"
        KILLED=$((KILLED + 1))
      else
        result="ERROR (UNDECLARED build kill — ${MUT_GATE[$idx]:-no gate} never ran, so it is not shown load-bearing)"
        ERRORS=$((ERRORS + 1)); INVALID=$((INVALID + 1))
      fi
    elif grep -qE "unused variable|This simp argument is unused|unused binding" "$MUT_LOG_DIR/build.log"; then
      result="ERROR (invalid mutation — unused-binding lint, not the rule)"
      ERRORS=$((ERRORS + 1)); INVALID=$((INVALID + 1))
    else
      result="ERROR (build failed unattributably — inspect $MUT_LOG_DIR/build.log)"
      ERRORS=$((ERRORS + 1)); INFRA=$((INFRA + 1))
    fi
  fi

  # Restore original. A failed restore-build is fatal to the REST of the run, because every later
  # family would be judged against a binary built from this mutation.
  restore_mutation "$idx" || true
  TOTAL=$((TOTAL + 1))
  if [ "${RESTORE_BUILD_FAILED:-0}" = "1" ]; then
    echo "$result"
    echo "  ABORTING: refusing to run further families against a mutated binary." >&2
    return 1
  fi

  if [[ "$result" == "SURVIVED" ]]; then
    echo "$result  <-- TEST GAP"
  else
    echo "$result"
  fi
}

# ============================================================
# Main
# ============================================================

echo "=== Mutation Testing ($NUM_MUTATIONS mutations) ==="
echo ""

# Preflight: no target file may be dirty. A mutation applied on top of
# uncommitted work cannot be distinguished from that work on restore, and the
# hash postcondition would then compare against an already-modified baseline.
# Only the files this RUN will touch. Checking the whole target set would refuse
# a single-mutation run because some unrelated target happens to be dirty, which
# makes the guard obstructive rather than protective — and an obstructive guard
# gets bypassed.
if [ "$MODE" = "single" ]; then
  MUT_TARGETS=("${MUT_FILE[$((SINGLE_IDX - 1))]}")
else
  mapfile -t MUT_TARGETS < <(printf '%s\n' "${MUT_FILE[@]}" | sort -u)
fi
for f in "${MUT_TARGETS[@]}"; do
  # `--quiet` alone misses STAGED changes: a file that is `git add`-ed but not
  # committed reads as clean to `git diff`, so a mutation could be applied on top
  # of staged work and restored over it. Check the index too.
  if ! git -C "$ROOT_DIR" diff --quiet -- "$f" 2>/dev/null \
     || ! git -C "$ROOT_DIR" diff --quiet --cached -- "$f" 2>/dev/null; then
    echo "error: target file has uncommitted changes: $f" >&2
    echo "       this harness edits targets in place; commit or stash first." >&2
    exit 2
  fi
  MUT_HASH0["$f"]="$(hash_of "$ROOT_DIR/$f")"
done

# Preflight: the PRISTINE tree must build. Otherwise every mutation fails the build
# and the run tests nothing — the same shape as a CI job that is green because it never
# ran. (This once scored as "KILLED (build)" and read as perfect coverage.)
printf "preflight: pristine tree builds ... "
if $LAKE build > /tmp/mutation_preflight.log 2>&1; then
  echo "ok"
else
  echo "FAILED"
  echo "error: the unmutated tree does not build, so kill/survive verdicts would be meaningless." >&2
  echo "       see /tmp/mutation_preflight.log" >&2
  exit 2
fi
# Measured HERE: pristine source, nothing mutated yet, nothing built from a mutation.
baseline_clean_producers
echo ""

if [[ "$MODE" == "single" ]]; then
  if [[ "$SINGLE_IDX" -lt 1 || "$SINGLE_IDX" -gt "$NUM_MUTATIONS" ]]; then
    echo "Error: mutation index must be between 1 and $NUM_MUTATIONS"
    exit 1
  fi
  run_mutation "$SINGLE_IDX"
else
  for (( i=1; i<=NUM_MUTATIONS; i++ )); do
    run_mutation "$i"
  done
fi

echo ""
if [ "${MUT_FRESHNESS_TAINT:-0}" = "1" ]; then
  echo ""
  echo "FRESHNESS UNVERIFIED: at least one gate ran against a binary nobody verified against source." >&2
  echo "  Every verdict below describes that binary. This run is not evidence." >&2
  ERRORS=$((ERRORS + 1)); INFRA=$((INFRA + 1))
fi
echo "=== Results: $KILLED killed, $SURVIVED survived, $ERRORS errors [$INVALID invalid, $INFRA infrastructure] ($TOTAL total) ==="

if [[ "$SURVIVED" -gt 0 ]]; then
  echo ""
  echo "WARNING: $SURVIVED mutation(s) survived — these represent test gaps."
  exit 1
fi

# A SKIPPED mutation is lost coverage, not a neutral event: its anchor no longer exists in
# the source, so it exercises nothing while still being counted in the suite. This used to
# increment ERRORS and exit 0, which meant the harness reported success over a mutation
# that had quietly stopped testing anything. Measured 2026-08-06: 4 of 77 anchors were
# stale, three of them found only because someone happened to be editing nearby.
if [[ "$ERRORS" -gt 0 ]]; then
  echo ""
  echo "FAILED: $ERRORS mutation(s) produced no verdict about any gate."
  if [[ "$INVALID" -gt 0 ]]; then
    echo "  $INVALID invalid: the mutation itself is not a valid experiment (drifted anchor, lint,"
    echo "    or an undeclared build kill). Re-anchor or rewrite it on the current code shape, so"
    echo "    it builds and reaches its gate; do not delete it, and do not leave it skipped."
  fi
  if [[ "$INFRA" -gt 0 ]]; then
    echo "  $INFRA infrastructure: no verdict could be reached (see the per-mutation lines above)."
    echo "    Fix the environment or the gate on pristine source, then re-run; the mutation may be fine."
  fi
  exit 1
fi
