#!/usr/bin/env bash
# PRE-PUSH VALIDATION RECORD — lets a second push of an already-validated candidate skip re-running
# the identical local gate selection, and nothing more.
#
# The same SHA is pushed to more than one remote. The hook used to re-run the whole selection for the
# second push, minutes of work whose answer could not differ. A record may stand in for that run ONLY
# when every input the run depended on is provably the same, so a record is a set of BOUND
# COMPONENTS, and reuse is equality on every one of them:
#
#   head, pushed     the exact commit, and every ref being pushed must point at it. The gates measure
#                    the CHECKOUT, so a push of anything other than HEAD is never recorded.
#   tracked          the tracked tree must be CLEAN (no staged or unstaged change), so HEAD names the
#                    tree that was measured; the digest is stored as well.
#   untracked        digest of every untracked, non-ignored file (names, sizes, contents).
#   toolchain_file   lean-toolchain contents;
#   lean, lake       the binaries actually on PATH and their --version output. An unidentifiable
#                    toolchain disables records rather than binding "unknown".
#   checker_build    the toolchain the gates actually ran, BY CONTENT: `lean --print-prefix`, then the
#                    bytes of bin/lean, bin/lake and every native library in lib/lean. A path and a
#                    --version string do not identify a build (an elan proxy path is shared by every
#                    toolchain, and a rebuilt toolchain can report the same version), so a record
#                    without libleanshared located is not written.
#   cc               the C compiler on PATH (path and --version); the codegen gates link with it.
#   binary           digest of .lake/build/bin/concrete, the compiler the gates ran.
#   gate_defs        digest of every file under scripts/ and .githooks/, the workflow directory, the
#                    Makefile and the build/toolchain manifests — tracked or untracked. The SHA and the
#                    clean-tree requirement already pin these; the explicit digest keeps the binding
#                    visible and catches an untracked gate script that the SHA cannot see.
#   selection        digest of the resolved gate list (run_ci_gates_local.sh --plan output).
#   base             digest of the changed-file list the selection was computed from. A second remote
#                    whose push range differs computes a different list and does not reuse, even when
#                    the selection would happen to coincide.
#   seed             the fuzz seed, when a selected command reads $SEED (the runner derives it from
#                    the UTC date).
#   env              digest of every CONCRETE_* environment variable.
#
# A record is written only after a fully successful run, atomically, with `complete=1` as its LAST
# line; a missing, truncated, failed or foreign record simply does not match. A run in which a gate's
# verdict was DEFERRED TO CI (bug 315; scripts/tests/lib/host_tools.sh) is recorded as
# `status=pass-deferred` with one `deferred_gate=` line per gate, never as `status=pass`: the hook
# accepted it, and the record says which verdicts it did not make. It lives in the git
# directory (`git rev-parse --git-path`), per worktree and never committed.
#
# Every function takes the repository root explicitly and reports why it refused in PPR_REASON.

PPR_FORMAT="concrete-prepush-record-v1"

_ppr_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=scripts/tests/lib/treestate.sh
source "$_ppr_dir/treestate.sh" 2>/dev/null || true

ppr_available() { echo "$PPR_FORMAT"; }

ppr_path() { # root -> absolute record path inside that worktree's git dir
  local p
  p="$(git -C "$1" rev-parse --git-path concrete-prepush-record 2>/dev/null)" || return 1
  [ -n "$p" ] || return 1
  case "$p" in /*) ;; *) p="$1/$p" ;; esac
  printf '%s\n' "$p"
}

_ppr_toolver() { # tool -> "path|version" or nothing
  local t="$1" path ver
  path="$(command -v "$t" 2>/dev/null)" || return 1
  [ -n "$path" ] || return 1
  ver="$("$t" --version 2>/dev/null | tr '\n' ' ' | sed 's/[[:space:]]*$//')" || return 1
  [ -n "$ver" ] || return 1
  printf '%s|%s' "$path" "$ver"
}

_ppr_checker_build() { # -> digest of the toolchain's executables and native libraries, or nothing
  local pfx f rows=""
  pfx="$(lean --print-prefix 2>/dev/null)" || return 1
  [ -n "$pfx" ] && [ -f "$pfx/bin/lean" ] || return 1
  ls "$pfx"/lib/lean/libleanshared.* >/dev/null 2>&1 || return 1
  for f in "$pfx/bin/lean" "$pfx/bin/lake" "$pfx"/lib/lean/*.so "$pfx"/lib/lean/*.so.* \
           "$pfx"/lib/lean/*.dylib "$pfx"/lib/lean/*.dll; do
    [ -f "$f" ] || continue
    rows="$rows${f#"$pfx"/}:$(ts_digest < "$f")
"
  done
  [ -n "$rows" ] || return 1
  printf '%s' "$rows" | ts_digest
}

_ppr_gate_defs() { # root -> digest
  local root="$1" list hashes
  list="$(mktemp)" || return 1
  hashes="$(mktemp)" || { rm -f "$list"; return 1; }
  if ! git -C "$root" ls-files -co --exclude-standard -- scripts .githooks .github/workflows \
         Makefile lean-toolchain lakefile.toml lakefile.lean lake-manifest.json flake.nix flake.lock \
         2>/dev/null | LC_ALL=C sort -u > "$list"; then
    rm -f "$list" "$hashes"; return 1
  fi
  [ -s "$list" ] || { rm -f "$list" "$hashes"; return 1; }
  if ! (cd "$root" && git hash-object --stdin-paths < "$list" > "$hashes" 2>/dev/null); then
    rm -f "$list" "$hashes"; return 1
  fi
  [ "$(wc -l < "$list")" = "$(wc -l < "$hashes")" ] || { rm -f "$list" "$hashes"; return 1; }
  paste "$list" "$hashes" | ts_digest
  rm -f "$list" "$hashes"
}

# ppr_components ROOT PUSHED_SHAS CHANGED_FILE PLAN_FILE OUT_FILE
#   PUSHED_SHAS: whitespace-separated local SHAs of the refs being pushed.
# Writes `key=value` lines to OUT_FILE. Returns 1 with PPR_REASON when this push cannot be recorded
# or matched at all — in which case the caller runs the gates and writes no record.
ppr_components() {
  local root="$1" pushed="$2" changed_f="$3" plan_f="$4" out="$5"
  PPR_REASON=""
  command -v ts_require >/dev/null 2>&1 && ts_require 2>/dev/null \
    || { PPR_REASON="treestate_library_unavailable"; return 1; }
  [ "${CONCRETE_ALLOW_UNVERIFIED_BINARY:-0}" != "1" ] \
    || { PPR_REASON="unverified_binary_override"; return 1; }
  [ -s "$plan_f" ] || { PPR_REASON="empty_selection"; return 1; }
  [ -f "$changed_f" ] || { PPR_REASON="no_changed_list"; return 1; }
  local head s sorted
  head="$(ts_head "$root")"
  case "$head" in ""|*TREESTATE-UNAVAILABLE*) PPR_REASON="head_unreadable"; return 1 ;; esac
  sorted="$(printf '%s\n' $pushed | sed '/^$/d' | LC_ALL=C sort -u | tr '\n' ' ' | sed 's/ $//')"
  [ -n "$sorted" ] || { PPR_REASON="no_pushed_sha"; return 1; }
  for s in $sorted; do
    [ "$s" = "$head" ] || { PPR_REASON="pushed_sha_not_head(${s:0:12})"; return 1; }
  done
  if ! git -C "$root" diff --quiet HEAD -- 2>/dev/null || ! git -C "$root" diff --cached --quiet 2>/dev/null; then
    PPR_REASON="tracked_tree_dirty"; return 1
  fi
  local tracked untracked tcfile lean lake ckb cc bin gates sel base seed envd
  tracked="$(ts_tracked "$root")"
  untracked="$(ts_untracked "$root")"
  tcfile="$(tr '\n' ' ' < "$root/lean-toolchain" 2>/dev/null | sed 's/[[:space:]]*$//')"
  [ -n "$tcfile" ] || { PPR_REASON="lean_toolchain_unreadable"; return 1; }
  lean="$(_ppr_toolver lean)" || { PPR_REASON="toolchain_unidentified(lean)"; return 1; }
  lake="$(_ppr_toolver lake)" || { PPR_REASON="toolchain_unidentified(lake)"; return 1; }
  ckb="$(_ppr_checker_build)" || { PPR_REASON="checker_build_unidentified"; return 1; }
  cc="$(_ppr_toolver cc)" || { PPR_REASON="toolchain_unidentified(cc)"; return 1; }
  [ -f "$root/.lake/build/bin/concrete" ] || { PPR_REASON="no_compiler_binary"; return 1; }
  bin="$(ts_digest < "$root/.lake/build/bin/concrete")"
  gates="$(_ppr_gate_defs "$root")" || { PPR_REASON="gate_defs_unreadable"; return 1; }
  sel="$(ts_digest < "$plan_f")"
  base="$(LC_ALL=C sort -u "$changed_f" | sed '/^$/d' | ts_digest)"
  if grep -q 'SEED' "$plan_f"; then seed="${SEED:-$(date -u +%Y%m%d)}"; else seed="unused"; fi
  envd="$(env | grep '^CONCRETE_' | grep -v '^CONCRETE_SKIP_GATES=' | LC_ALL=C sort | ts_digest)"
  local kv k v
  for kv in "tracked=$tracked" "untracked=$untracked" "checker_build=$ckb" "binary=$bin" "gate_defs=$gates" \
            "selection=$sel" "base=$base" "env=$envd"; do
    k="${kv%%=*}"; v="${kv#*=}"
    case "$v" in ""|*TREESTATE-UNAVAILABLE*) PPR_REASON="component_unavailable($k)"; return 1 ;; esac
  done
  {
    echo "format=$PPR_FORMAT"
    echo "head=$head"
    echo "pushed=$sorted"
    echo "tracked=$tracked"
    echo "untracked=$untracked"
    echo "toolchain_file=$tcfile"
    echo "lean=$lean"
    echo "lake=$lake"
    echo "checker_build=$ckb"
    echo "cc=$cc"
    echo "binary=$bin"
    echo "gate_defs=$gates"
    echo "selection=$sel"
    echo "selection_count=$(grep -c . "$plan_f")"
    echo "base=$base"
    echo "seed=$seed"
    echo "env=$envd"
  } > "$out" || { PPR_REASON="components_unwritable"; return 1; }
  return 0
}

# ppr_check RECORD COMPONENTS_FILE -> 0 when the record matches every component.
# On mismatch PPR_REASON names every dimension that differs.
ppr_check() {
  local rec="$1" comp="$2" line k v rv diff=""
  PPR_REASON=""
  [ -f "$rec" ] || { PPR_REASON="no_record"; return 1; }
  [ -s "$comp" ] || { PPR_REASON="no_components"; return 1; }
  # COMPLETE, PASSED, OURS — or nothing. `complete=1` is written last, so a truncated write fails here.
  [ "$(tail -n 1 "$rec")" = "complete=1" ] || { PPR_REASON="record_incomplete"; return 1; }
  grep -qxE "status=(pass|pass-deferred)" "$rec" || { PPR_REASON="record_not_a_pass"; return 1; }
  # pass-deferred names what it deferred, and pass defers nothing: either mismatch is not a record.
  if grep -qx "status=pass-deferred" "$rec"; then
    grep -q '^deferred_gate=.' "$rec" || { PPR_REASON="record_deferral_unnamed"; return 1; }
  elif grep -q '^deferred_gate=' "$rec"; then
    PPR_REASON="record_pass_with_deferral"; return 1
  fi
  grep -qx "format=$PPR_FORMAT" "$rec" || { PPR_REASON="record_format_mismatch"; return 1; }
  while IFS= read -r line; do
    k="${line%%=*}"; v="${line#*=}"
    [ -n "$k" ] || continue
    # Exactly one line per key; a duplicated key cannot be resolved and is not a match.
    [ "$(grep -c "^$k=" "$rec")" = "1" ] || { diff="$diff $k"; continue; }
    rv="$(grep "^$k=" "$rec" | head -1)"; rv="${rv#*=}"
    [ "$rv" = "$v" ] || diff="$diff $k"
  done < "$comp"
  if [ -n "$diff" ]; then PPR_REASON="changed:${diff# }"; return 1; fi
  return 0
}

# ppr_write RECORD COMPONENTS_FILE [DEFERRED_FILE] -> atomic write; status and complete=1 come from
# here only. DEFERRED_FILE lists the commands the run deferred to CI, one per line: any line makes the
# status pass-deferred and is recorded as deferred_gate=<command>.
ppr_write() {
  local rec="$1" comp="$2" deferred="${3:-}" tmp
  [ -s "$comp" ] || return 1
  tmp="$(mktemp "$rec.XXXXXX" 2>/dev/null)" || return 1
  if ! { cat "$comp"
         if [ -n "$deferred" ] && [ -s "$deferred" ]; then
           echo "status=pass-deferred"
           sed '/^$/d; s/^/deferred_gate=/' "$deferred"
         else
           echo "status=pass"
         fi
         echo "written_at=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
         echo "complete=1"; } > "$tmp" 2>/dev/null; then
    rm -f "$tmp"; return 1
  fi
  mv -f "$tmp" "$rec" 2>/dev/null || { rm -f "$tmp"; return 1; }
}

# ppr_invalidate RECORD -> remove; a run that starts no longer vouches for the previous record.
ppr_invalidate() { rm -f "$1" 2>/dev/null; [ ! -e "$1" ]; }
