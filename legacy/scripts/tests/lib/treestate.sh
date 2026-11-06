#!/usr/bin/env bash
# CANONICAL TREE-STATE SNAPSHOTS — one producer, used by every start/end reconciliation.
#
# WHY THIS FILE EXISTS. Two long-running entry points reconcile the repository across their run:
# `check_gate_mutation_coverage.sh` (the mutation campaign) and `run_ci_gates_local.sh` (the gate
# runner). Each had grown its OWN implementation of "what does the tree look like", and the two
# disagreed — so a defect found and fixed in one silently survived in the other. Measured on
# 2026-08-21: the campaign's dirty-target test was repaired to see staged changes while the runner's
# was still blind to them, and the campaign's tree digests were moved to content while the runner's
# still hashed status text. Two producers of one fact is the defect class this repository keeps
# paying for, and a reconciliation that can be right in one caller and wrong in another is not a
# reconciliation.
#
# EVERY FUNCTION TAKES THE ROOT EXPLICITLY. The campaign runs from a snapshot under /tmp and must
# reconcile the REPOSITORY, not its own location, so deriving a root from `BASH_SOURCE` here would
# reintroduce the bug that produced 78 spurious "file no longer exists" failures.

# THE HASHER IS RESOLVED ONCE AND ITS ABSENCE IS FATAL. This assumed one of the two tools existed;
# with neither, every digest was empty output from a failed command, and two empty values compare
# equal — the same fail-open shape as a missing library.
_TS_HASHER=""
if command -v sha256sum >/dev/null 2>&1; then _TS_HASHER="sha256sum"
elif command -v shasum >/dev/null 2>&1; then _TS_HASHER="shasum -a 256"
fi
_ts_digest() {
  [ -n "$_TS_HASHER" ] || { echo "TREESTATE-UNAVAILABLE:no-hasher"; return 0; }
  $_TS_HASHER | cut -c1-32
}

# EVERY PRODUCER REFUSES RATHER THAN RETURNING AN EMPTY ANSWER.
#
# Callers compare a start value against an end value, so ANY failure that yields the same empty string
# twice reconciles as "nothing changed". Measured 2026-08-21: with this library absent, `ts_tracked`
# is "command not found", both captures are empty, `[ "$A" = "$B" ]` holds, and the caller issues
# `completed=1` having verified nothing. A digest of no input is a real digest of the empty string and
# is indistinguishable from a digest of a clean tree, so the failure has to be signalled in-band.
_ts_fail() { echo "TREESTATE-UNAVAILABLE:$1"; }

ts_head() { # root
  local h
  h="$(git -C "$1" rev-parse HEAD 2>/dev/null)" || { _ts_fail "head"; return 0; }
  # "no-head" was returned as an accepted sentinel on BOTH sides of the comparison, so a repository
  # git could not read reconciled as unchanged.
  [ -n "$h" ] || { _ts_fail "head-empty"; return 0; }
  printf '%s' "$h"
}

# THE CALLER MUST PROVE THE LIBRARY LOADED. Sourcing is not checked by `set -u`, and neither caller
# uses `set -e`, so a missing or truncated library degrades silently into the empty-equals-empty
# path above. Callers invoke this immediately after sourcing.
# ONE DIGEST PRODUCER, USED BY BOTH ROLES.
#
# The child had its own `_d` and its own driver/inventory digests, and the supervisor had no way to
# compute either — so it published the child's word for what driver and inventory had run. Two
# producers of one fact is the defect class this repository keeps finding; here there would not even
# have been two, because the supervisor had none and simply trusted.
ts_digest() { # stdin -> 32 hex characters
  if command -v sha256sum >/dev/null 2>&1; then sha256sum | cut -c1-32
  else shasum -a 256 | cut -c1-32; fi
}
ts_driver_digest() { # root -> digest of the campaign driver AS IT SITS IN THAT TREE
  cat "$1/scripts/tests/check_gate_mutation_coverage.sh" 2>/dev/null | ts_digest
}
ts_inventory_digest() { # root -> digest of the driver's `add` and `expect_red` inventory lines
  # `expect_red` is inventory too: it decides which families a run reports PENDING instead of judging,
  # so changing one changes what the campaign is evidence about, exactly as an `add` line does.
  grep -h -E '^(add|expect_red) "' "$1/scripts/tests/check_gate_mutation_coverage.sh" 2>/dev/null | ts_digest
}

ts_available() { echo "treestate-v1"; }
ts_require() { # called by consumers right after sourcing
  local v fn
  v="$(ts_available 2>/dev/null)" || v=""
  if [ "$v" != "treestate-v1" ]; then
    echo "FATAL: scripts/tests/lib/treestate.sh did not load — tree-state reconciliation would" >&2
    echo "       compare empty against empty and accept it. Refusing to run." >&2
    return 1
  fi
  # EVERY PRODUCER, not just the version marker. Checking one symbol proved only that SOMETHING
  # loaded: a file truncated immediately after this function would pass while ts_tracked,
  # ts_untracked and ts_dirty_files were all absent — and absent producers are exactly the
  # empty-equals-empty path this guard exists to stop.
    # THE DIGEST PRODUCERS ARE REQUIRED TOO. Without them a missing `ts_inventory_digest` made the
    # child's start and end inventory snapshots BOTH empty, their comparison succeed, the published
    # `inventory_sha=` satisfy the freeform schema, and the supervisor skip its own check — a hashing
    # failure silently disabling the verification it exists to perform.
  for fn in ts_head ts_tracked ts_untracked ts_dirty_files ts_digest ts_driver_digest ts_inventory_digest; do
    if ! command -v "$fn" >/dev/null 2>&1; then
      echo "FATAL: treestate.sh loaded but '$fn' is missing — the library is truncated." >&2
      return 1
    fi
  done
  return 0
}

# TRACKED CONTENT, not status text. `git status --porcelain` names files and status letters; editing
# an already-modified file leaves its porcelain line byte-identical, so a tree that moved under the
# run reconciled as unmoved. `git diff HEAD` carries the actual patch text, and it covers the INDEX
# as well as the worktree — a staged change is exactly as real as an unstaged one.
ts_tracked() { # root
  # THE GIT FAILURE IS SIGNALLED, not digested. Piping failed output into a hash produces a real
  # digest of the empty string, which is indistinguishable from a clean tree and compares equal to
  # itself at both ends of the run.
  # `--binary`. Plain `git diff` renders a changed binary as "Binary files ... differ" plus an
  # `index <abbrev>..<abbrev>` line, so content only reaches the digest through a 7-character
  # abbreviated blob hash. (Measured: that line DOES change with content, so the reported
  # 'identical diff text' case did not reproduce — but relying on an abbreviated hash for
  # collision resistance is weaker than carrying the bytes, and this tree has tracked binary
  # fixtures.) `--binary` emits the actual delta.
  #
  # VIA A TEMP FILE, NEVER A SHELL VARIABLE.
  #
  # MEASURED, 2026-08-21: capturing this diff into a variable and piping it returned an EMPTY digest,
  # with `sha256sum` and `cut` both failing "Argument list too long", at 144 KB of diff in this
  # repository. The empty digest and the E2BIG are observations; the CAUSE is not established —
  # `allexport` is off, and an unexported variable is not placed in `execve`'s argv or environment, so
  # the obvious explanation does not hold. I am not going to assert a mechanism I have not shown.
  #
  # What matters is the consequence and the remedy. Consequence: before the empty-value refusal
  # existed, an empty digest at both ends reconciled as an unchanged tree — a fail-open reached by
  # ordinary repository size rather than by any error. Remedy: the data never needs to be in a
  # variable, so it goes through a file and the question does not arise.
  local raw_f
  raw_f="$(mktemp)" || { _ts_fail "tracked-tmp"; return 0; }
  if ! git -C "$1" diff HEAD --binary > "$raw_f" 2>/dev/null; then
    rm -f "$raw_f"; _ts_fail "tracked"; return 0
  fi
  _ts_digest < "$raw_f"
  rm -f "$raw_f"
}

# UNTRACKED CONTENT AND NAMES, with boundaries. Hashing names alone missed edits; hashing
# concatenated bytes alone missed renames and let identical bytes be redistributed among files
# invisibly. Each file contributes its PATH, its LENGTH and its content digest, so neither dimension
# can be changed without moving the result.
# NUL-DELIMITED THROUGHOUT. Converting git's `-z` output back to newlines with `tr` undid the whole
# point of asking for NUL: a filename CONTAINING a newline was split into two entries, and could
# disappear from the digest entirely. Records are read with `read -d ''` and never pass through a
# newline-delimited stage.
ts_untracked() { # root
  local root="$1" u
  # ONE INVOCATION, VIA A TEMP FILE, AND ITS STATUS IS CHECKED. Probing with one `git status` and
  # then hashing the output of a SECOND, unchecked one meant the probe could succeed while the command
  # feeding the digest failed. The obvious repair — capturing the output in a variable — is WRONG here
  # and was measured to be: bash command substitution silently DROPS NUL bytes, so capturing
  # `--untracked-files=all -z` output made every record vanish and the function returned the digest of
  # empty input, i.e. reported a clean tree while an untracked file existed. NUL-delimited data has to
  # stay in a file or a pipe.
  local raw_f
  raw_f="$(mktemp)" || { _ts_fail "untracked-tmp"; return 0; }
  if ! git -C "$root" status --porcelain --untracked-files=all -z > "$raw_f" 2>/dev/null; then
    rm -f "$raw_f"; _ts_fail "untracked"; return 0
  fi
  { while IFS= read -r -d '' entry; do
          case "$entry" in "?? "*) u="${entry#?? }" ;; *) continue ;; esac
          [ -n "$u" ] || continue
          # TYPE IS PART OF IDENTITY. `[ -f ]` alone recorded a symlink's TARGET contents as if they
          # were the file's, and dropped broken symlinks entirely — both changes that must move this
          # digest. Directories cannot appear here because `--untracked-files=all` lists files.
          if [ -L "$root/$u" ]; then
            # AN UNRESOLVABLE LINK IS A FAILURE, NOT DATA. `|| echo '?'` turned "cannot read this" into
            # an ordinary record, so two different unreadable links produced the same accepted digest.
            _lt="$(readlink "$root/$u" 2>/dev/null)" \
              || { echo "TREESTATE-UNAVAILABLE:readlink"; continue; }
            printf 'L|%s|%s\n' "$u" "$_lt"
          elif [ -f "$root/$u" ]; then
            # UNREADABLE IS ITS OWN RECORD. `wc` fell back to 0 and the digest to empty, so an
            # unreadable file contributed a well-formed-looking record and the outer digest looked
            # normal — content nobody could read reconciled as a known quantity.
            if [ -r "$root/$u" ]; then
              # COMPUTED BEFORE PRINTING, so a failing hasher or `wc` cannot hide inside a successful
              # `printf`. An embedded command substitution does not fail the surrounding pipeline, so
              # PIPESTATUS could not see it, and an unreadable same-length file hashed identically at
              # both ends of the run.
              _sz="$(wc -c < "$root/$u" 2>/dev/null)" || { echo "TREESTATE-UNAVAILABLE:wc"; continue; }
              _dg="$(_ts_digest < "$root/$u")" || { echo "TREESTATE-UNAVAILABLE:digest"; continue; }
              case "$_dg" in *TREESTATE-UNAVAILABLE*) echo "$_dg"; continue ;; esac
              printf 'F|%s|%s|%s\n' "$u" "$_sz" "$_dg"
            else
              printf 'U|%s|unreadable\n' "$u"
            fi
          else
            printf 'O|%s\n' "$u"
          fi
        done < "$raw_f" | LC_ALL=C sort > "$raw_f.recs"; }
  # CAPTURED AT THE PIPELINE, not several commands later. The previous version saved PIPESTATUS after a
  # `grep` and a `_ts_digest` had already run, so it described THOSE — in practice a successful
  # function call — and never the `while | sort` pipeline it was meant to check. If `sort` or its
  # redirection failed, an empty or partial record file was hashed into an ordinary-looking digest.
  local _recst=("${PIPESTATUS[@]}") _s
  for _s in "${_recst[@]}"; do
    [ "$_s" = "0" ] || { rm -f "$raw_f" "$raw_f.recs"; _ts_fail "untracked-pipeline"; return 0; }
  done
  # A marker anywhere in the records means an inner producer could not read what it was asked to read;
  # digesting the rest would return an ordinary-looking value for a tree that was never measured.
  if grep -q 'TREESTATE-UNAVAILABLE' "$raw_f.recs" 2>/dev/null; then
    rm -f "$raw_f" "$raw_f.recs"; _ts_fail "untracked-record"; return 0
  fi
  _ts_digest < "$raw_f.recs"
  # THE PIPELINE'S STATUS IS CHECKED. If `sort` is missing or fails, the hasher downstream still emits
  # a perfectly valid digest — of EMPTY INPUT — and the trailing `rm` then masked the nonzero status.
  # That digest is neither empty nor the unavailability marker, so both callers accepted it and two
  # identical false digests could authorise `completed=1` with the untracked tree never measured.
  local _st=("${PIPESTATUS[@]}")   # the final digest only; the record pipeline was checked above
  rm -f "$raw_f" "$raw_f.recs"
  local _s
  for _s in "${_st[@]}"; do
    [ "$_s" = "0" ] || { _ts_fail "untracked-pipeline"; return 0; }
  done
}

# NAMED FILES THAT DIFFER FROM HEAD. Used for mutation targets, where the actionable diagnosis is
# "this specific file was not restored" rather than "the tree changed".
#
# `git diff HEAD`, NOT `git diff`. Plain `git diff` compares the worktree against the INDEX, so a
# mutation that had been `git add`ed read as CLEAN — measured 2026-08-21 by staging a mutated
# Concrete/Elab/Elab.lean and watching `git diff --quiet` report no change. A staged mutation is what
# a killed run plus a reflexive `git add` leaves behind.
ts_dirty_files() { # root file...
  local root="$1"; shift
  local f
  for f in "$@"; do
    [ -n "$f" ] || continue
    # A MISSING OR RETYPED TARGET IS THE WORST CASE, NOT AN EXEMPTION. This skipped anything that was
    # not currently a regular file, so a mutation target that had been DELETED — or replaced by a
    # symlink — was absent from both the start and end lists and reconciled as clean. Verified
    # 2026-08-21 by removing Concrete/Elab/Elab.lean: the check reported nothing. `git diff HEAD`
    # already reports a deletion, so the file only has to exist in HEAD to be judged.
    if [ -L "$root/$f" ]; then printf '%s(symlink) ' "$f"; continue; fi
    if [ ! -e "$root/$f" ]; then
      if git -C "$root" cat-file -e "HEAD:$f" 2>/dev/null; then printf '%s(deleted) ' "$f"; fi
      continue
    fi
    git -C "$root" diff HEAD --quiet -- "$f" 2>/dev/null || printf '%s ' "$f"
  done
}
