# Shared helpers for scripts/land/*.sh, scripts/push-both.sh and .githooks/pre-push (sourced,
# never run). docs/LANDING.md is the manual.
#
#   LAND_CANONICAL_REPO                   the one canonical repository ("lambdaclass/concrete")
#   LAND_CANONICAL_REMOTE                 the one canonical remote name ("origin")
#   land_normalize_url url                normalize a remote URL to its repository identity
#   land_canonical_target caller          expected identity (or sandbox test seam)
#   land_check_remote caller [remote]     verify remote's resolved fetch/push URLs match canonical
#   land_check_destination caller         verify env selectors and origin's resolved URLs
#   land_check_hook_destination c r u     verify .githooks/pre-push's ($1, $2) when pushing main
#   land_in_nix cmd...                    run cmd inside `nix develop` unless already there (LAND_NO_NIX=1 never)
#   land_assert_no_op what                die if a merge, rebase, cherry-pick or revert is in progress
#   legs_start name log cmd...            run cmd in the background as a named leg, output to log
#   legs_wait                             wait for every leg; sets LEGS_FAILED, LEGS_SUM, LEGS_REPORT
#   MOVE_GATE_MARKER                      the line train.sh's dry-run land prints when only the move gate refused
#   dryland_only_move_gate rc out         0 iff a dry-run land exited 5 with MOVE_GATE_MARKER as a whole line
#
# A LEG is one independent validation (impact gates, remote full gates, Codex review). Legs of one
# candidate run at the same time and are all collected: one failing leg fails the whole, and no
# leg is abandoned while another is still running. LEGS_SUM is the sequential cost (the sum of the
# legs' durations); the caller compares it with the wall time to record what pipelining saved.

# Canonical publication and CI-evidence repository identity (one owner: scripts/land/lib.sh).
# Every publishing tool (train.sh, push-both.sh, ready.sh, notes.sh, impact_gates.sh, acceptance.sh)
# and .githooks/pre-push sources this file and validates destinations and CI selectors here.
LAND_CANONICAL_REPO="lambdaclass/concrete"
LAND_CANONICAL_REMOTE="origin"
LAND_LIB_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd -P)"

# Normalize a remote URL to its canonical repository identity:
#   - GitHub HTTPS or SSH URLs (https://[user@]github.com/owner/repo[.git][/],
#     ssh://[user@]github.com[:port]/owner/repo[.git][/], [user@]github.com:owner/repo[.git][/])
#     normalize case-insensitively to `github.com/<owner>/<repo>`.
#   - An existing local bare git repository (/abs/path or file:///abs/path) normalizes to
#     `local:<real-abs-path>`.
#   - Anything else normalizes to `raw:<url>`, which never matches a canonical target.
land_normalize_url() {
  local u="${1:-}" p="" owner="" repo="" lc
  case "$u" in
    file:///*) p="${u#file://}" ;;
    /*) p="$u" ;;
  esac
  if [ -n "$p" ] && [ -d "$p" ] && [ "$(git --git-dir="$p" rev-parse --is-bare-repository 2>/dev/null)" = true ]; then
    echo "local:$(cd "$p" && pwd -P)"
    return 0
  fi
  lc="$(printf '%s' "$u" | tr '[:upper:]' '[:lower:]')"
  while [ "${lc%/}" != "$lc" ]; do lc="${lc%/}"; done
  if [[ "$lc" =~ ^https://([a-z0-9_.:-]+@)?github\.com/([^/]+)/([^/]+)$ ]]; then
    owner="${BASH_REMATCH[2]}"; repo="${BASH_REMATCH[3]}"
  elif [[ "$lc" =~ ^ssh://([a-z0-9_.-]+@)?github\.com(:[0-9]+)?/([^/]+)/([^/]+)$ ]]; then
    owner="${BASH_REMATCH[3]}"; repo="${BASH_REMATCH[4]}"
  elif [[ "$lc" =~ ^([a-z0-9_.-]+@)?github\.com:([^/:][^/]*)/([^/]+)$ ]]; then
    owner="${BASH_REMATCH[2]}"; repo="${BASH_REMATCH[3]}"
  fi
  if [ -n "$owner" ] && [ -n "$repo" ]; then
    repo="${repo%.git}"
    if [ -n "$repo" ] && [ "$owner" != "." ] && [ "$owner" != ".." ] && [ "$repo" != "." ] && [ "$repo" != ".." ]; then
      echo "github.com/$owner/$repo"
      return 0
    fi
  fi
  echo "raw:$u"
}

# Prints the expected normalized destination identity (`github.com/lambdaclass/concrete` in
# production). A local bare repository (`local:<real-path>`) is accepted ONLY through the test seam
# CONCRETE_TEST_CANONICAL_DIR when ALL of the following hold simultaneously:
#   1. git_sandbox config isolation is active: GIT_CONFIG_NOSYSTEM=1 and GIT_CONFIG_GLOBAL is an
#      existing file whose directory ($gcfg_dir, the mktemp sandbox root) is inside ${TMPDIR:-/tmp}/.
#   2. The current git worktree (`git rev-parse --show-toplevel`) resolves inside that sandbox
#      directory $gcfg_dir/ (and therefore inside ${TMPDIR:-/tmp}/, never the owning checkout).
#   3. CONCRETE_TEST_CANONICAL_DIR is an absolute filesystem path (no URL scheme) to an existing bare
#      git repository whose real path is inside $gcfg_dir/ (and ${TMPDIR:-/tmp}/).
# If CONCRETE_TEST_CANONICAL_DIR is set (even to empty) and any condition above fails, this function
# refuses and fails closed. Thus the seam can never be activated in a production checkout and can
# never point to a network remote or GitHub repository.
land_canonical_target() {
  local caller="${1:-land}"
  if [ -n "${CONCRETE_TEST_CANONICAL_DIR+x}" ]; then
    local cdir="$CONCRETE_TEST_CANONICAL_DIR" tmp_real top gcfg_dir cdir_real
    tmp_real="$(cd "${TMPDIR:-/tmp}" 2>/dev/null && pwd -P)" || {
      echo "$caller: refusing CONCRETE_TEST_CANONICAL_DIR='$cdir': TMPDIR '${TMPDIR:-/tmp}' does not exist" >&2
      return 1
    }
    if [ -z "$cdir" ]; then
      echo "$caller: refusing empty CONCRETE_TEST_CANONICAL_DIR test seam" >&2
      return 1
    fi
    case "$cdir" in
      *://*|*[[:space:]]*)
        echo "$caller: refusing CONCRETE_TEST_CANONICAL_DIR='$cdir': test seam accepts only a local bare repository path inside $tmp_real, never a URL" >&2
        return 1 ;;
      /*) ;;
      *)
        echo "$caller: refusing CONCRETE_TEST_CANONICAL_DIR='$cdir': must be an absolute path inside $tmp_real" >&2
        return 1 ;;
    esac
    if [ "${GIT_CONFIG_NOSYSTEM:-0}" != "1" ] || [ -z "${GIT_CONFIG_GLOBAL:-}" ] || [ ! -f "${GIT_CONFIG_GLOBAL:-}" ]; then
      echo "$caller: refusing CONCRETE_TEST_CANONICAL_DIR='$cdir' outside git_sandbox config isolation (GIT_CONFIG_NOSYSTEM=1 and GIT_CONFIG_GLOBAL required)" >&2
      return 1
    fi
    gcfg_dir="$(cd "$(dirname "$GIT_CONFIG_GLOBAL")" 2>/dev/null && pwd -P)" || gcfg_dir=""
    case "$gcfg_dir/" in
      "$tmp_real/"*) ;;
      *)
        echo "$caller: refusing CONCRETE_TEST_CANONICAL_DIR='$cdir': GIT_CONFIG_GLOBAL='$GIT_CONFIG_GLOBAL' is not inside $tmp_real" >&2
        return 1 ;;
    esac
    top="$(cd "$(git rev-parse --show-toplevel 2>/dev/null)" 2>/dev/null && pwd -P)" || top=""
    if [ -z "$top" ] || [ "$top" = "$LAND_LIB_ROOT" ]; then
      case "$top/" in
        "$gcfg_dir/"*) ;;
        *)
          echo "$caller: refusing CONCRETE_TEST_CANONICAL_DIR='$cdir' in repository checkout '${top:-unknown}' (test seam is disabled in the owning checkout)" >&2
          return 1 ;;
      esac
    fi
    case "$top/" in
      "$gcfg_dir/"*) ;;
      *)
        echo "$caller: refusing CONCRETE_TEST_CANONICAL_DIR='$cdir': worktree '$top' is not inside $gcfg_dir (test seam is disabled in the owning checkout)" >&2
        return 1 ;;
    esac
    if [ ! -d "$cdir" ] || [ "$(git --git-dir="$cdir" rev-parse --is-bare-repository 2>/dev/null)" != true ]; then
      echo "$caller: refusing CONCRETE_TEST_CANONICAL_DIR='$cdir': not an existing bare git repository" >&2
      return 1
    fi
    cdir_real="$(cd "$cdir" 2>/dev/null && pwd -P)" || cdir_real=""
    case "$cdir_real/" in
      "$gcfg_dir/"*) ;;
      *)
        echo "$caller: refusing CONCRETE_TEST_CANONICAL_DIR='$cdir' ($cdir_real): bare repository is not inside $gcfg_dir" >&2
        return 1 ;;
    esac
    echo "local:$cdir_real"
    return 0
  fi
  echo "github.com/$LAND_CANONICAL_REPO"
}

# Verify that remote $2 (must be $LAND_CANONICAL_REMOTE, i.e. `origin`) exists, has a single fetch
# URL and a single push URL, that its resolved fetch and push URLs agree, and that both resolve
# (via git's own pushurl, insteadOf and pushInsteadOf resolution) to the canonical repository identity.
land_check_remote() {
  local caller="${1:-land}" remote="${2:-$LAND_CANONICAL_REMOTE}" target
  target="$(land_canonical_target "$caller")" || return 1
  if [ "$remote" = "fork" ]; then
    echo "$caller: refusing the remote named 'fork' (a divergent repository)" >&2
    return 1
  fi
  if [ "$remote" != "$LAND_CANONICAL_REMOTE" ]; then
    local o_url
    o_url="$(git remote get-url --push "$remote" 2>/dev/null || true)"
    echo "$caller: refusing remote '$remote'${o_url:+ ($o_url)}: publication must use remote '$LAND_CANONICAL_REMOTE' ($LAND_CANONICAL_REPO)" >&2
    return 1
  fi
  local raw_fetch raw_push res_fetch res_push
  raw_fetch="$(git config --get-all "remote.$remote.url" 2>/dev/null || true)"
  if [ -z "$raw_fetch" ]; then
    echo "$caller: remote '$remote' is not configured (must point to $LAND_CANONICAL_REPO)" >&2
    return 1
  fi
  raw_push="$(git config --get-all "remote.$remote.pushurl" 2>/dev/null || true)"
  res_fetch="$(git remote get-url --all "$remote" 2>/dev/null || true)"
  res_push="$(git remote get-url --push --all "$remote" 2>/dev/null || true)"
  if [ -z "$res_fetch" ] || [ -z "$res_push" ]; then
    echo "$caller: could not resolve URLs for remote '$remote'" >&2
    return 1
  fi
  local n_raw_fetch n_raw_push n_res_fetch n_res_push
  n_raw_fetch="$(printf '%s\n' "$raw_fetch" | grep -c . || true)"
  n_raw_push="$( [ -n "$raw_push" ] && printf '%s\n' "$raw_push" | grep -c . || echo 0 )"
  n_res_fetch="$(printf '%s\n' "$res_fetch" | grep -c . || true)"
  n_res_push="$(printf '%s\n' "$res_push" | grep -c . || true)"
  if [ "$n_raw_push" -gt 1 ] || [ "$n_res_push" -gt 1 ]; then
    echo "$caller: refusing remote '$remote' with multiple push URLs ($(printf '%s\n' "$res_push" | tr '\n' ' ' | sed 's/ $//')); exactly one push destination ($LAND_CANONICAL_REPO) is allowed" >&2
    return 1
  fi
  if [ "$n_raw_fetch" -gt 1 ] || [ "$n_res_fetch" -gt 1 ]; then
    echo "$caller: refusing remote '$remote' with multiple fetch URLs ($(printf '%s\n' "$res_fetch" | tr '\n' ' ' | sed 's/ $//')); exactly one repository ($LAND_CANONICAL_REPO) is allowed" >&2
    return 1
  fi
  local norm_fetch norm_push raw_eff_push
  norm_fetch="$(land_normalize_url "$res_fetch")"
  norm_push="$(land_normalize_url "$res_push")"
  raw_eff_push="${raw_push:-$raw_fetch}"
  if [ "$res_push" != "$raw_eff_push" ] && [ "$norm_push" != "$target" ]; then
    echo "$caller: refusing remote '$remote': push URL '$raw_eff_push' rewrites to '$res_push' ($norm_push; fetch URL '$res_fetch'), not the canonical repository $LAND_CANONICAL_REPO ($target)" >&2
    return 1
  fi
  if [ "$res_fetch" != "$raw_fetch" ] && [ "$norm_fetch" != "$target" ]; then
    echo "$caller: refusing remote '$remote': fetch URL '$raw_fetch' rewrites to '$res_fetch' ($norm_fetch; push URL '$res_push'), not the canonical repository $LAND_CANONICAL_REPO ($target)" >&2
    return 1
  fi
  if [ "$norm_fetch" != "$norm_push" ]; then
    echo "$caller: refusing remote '$remote': fetch URL '$res_fetch' ($norm_fetch) and push URL '$res_push' ($norm_push) disagree (both must resolve to $LAND_CANONICAL_REPO)" >&2
    return 1
  fi
  if [ "$norm_push" != "$target" ]; then
    echo "$caller: refusing remote '$remote': push URL '$res_push' ($norm_push) is not the canonical repository $LAND_CANONICAL_REPO ($target)" >&2
    return 1
  fi
  return 0
}

# Validate all repository and remote selector environment variables and the configured remote.
land_check_destination() {
  local caller="${1:-land}"
  land_canonical_target "$caller" >/dev/null || return 1
  local var val rurl
  for var in LAND_REPO ACCEPT_REPO GH_REPO; do
    if [ -n "${!var+x}" ]; then
      val="${!var}"
      if [ "$val" != "$LAND_CANONICAL_REPO" ]; then
        echo "$caller: refusing repository override $var='${val}' (must be $LAND_CANONICAL_REPO)" >&2
        return 1
      fi
    fi
  done
  for var in LAND_REMOTE PUSH_REMOTE; do
    if [ -n "${!var+x}" ]; then
      val="${!var}"
      if [ "$val" = "fork" ]; then
        echo "$caller: refusing the remote named 'fork' ($var='fork'; it is a divergent repository)" >&2
        return 1
      fi
      if [ "$val" != "$LAND_CANONICAL_REMOTE" ]; then
        rurl="$(git remote get-url --push "$val" 2>/dev/null || true)"
        echo "$caller: refusing destination override $var='${val}'${rurl:+ ($rurl)} (must use remote '$LAND_CANONICAL_REMOTE' -> $LAND_CANONICAL_REPO)" >&2
        return 1
      fi
    fi
  done
  if [ "$caller" = "push-both" ] && [ -n "${BRANCH+x}" ] && [ "$BRANCH" != "main" ]; then
    echo "$caller: refusing branch override BRANCH='${BRANCH}' (must be 'main')" >&2
    return 1
  fi
  land_check_remote "$caller" "$LAND_CANONICAL_REMOTE"
}

# Validate the destination arguments git passes to .githooks/pre-push ($1 = remote name or URL,
# $2 = git's resolved destination URL) when refs/heads/main is being pushed.
land_check_hook_destination() {
  local caller="${1:-pre-push}" remote_arg="${2:-}" url_arg="${3:-}" target
  target="$(land_canonical_target "$caller")" || return 1
  if [ "$remote_arg" = "fork" ]; then
    echo "$caller: refusing to push main to the remote named 'fork'${url_arg:+ ($url_arg)}" >&2
    return 1
  fi
  if [ -z "$url_arg" ]; then
    echo "$caller: refusing to push main without a destination URL (remote='${remote_arg}')" >&2
    return 1
  fi
  local norm_url
  norm_url="$(land_normalize_url "$url_arg")"
  if [ "$norm_url" != "$target" ]; then
    echo "$caller: refusing to push main to '$url_arg' ($norm_url): destination is not the canonical repository $LAND_CANONICAL_REPO ($target)" >&2
    return 1
  fi
  if [ "$remote_arg" != "$LAND_CANONICAL_REMOTE" ]; then
    echo "$caller: refusing to push main via '$remote_arg' ('$url_arg'): main may only be pushed via remote '$LAND_CANONICAL_REMOTE' ($LAND_CANONICAL_REPO)" >&2
    return 1
  fi
  land_check_remote "$caller" "$LAND_CANONICAL_REMOTE" || return 1
  return 0
}

# "Already in the dev shell" means the gate shell's marker (CONCRETE_GATES_SHELL=1, which every gate
# requires, scripts/checks/lib/gate_env.sh) AND a toolchain: an inherited IN_NIX_SHELL from some other
# shell (lean on PATH, no lake) made the hook refuse to build (2026-10-22), and any other nix shell
# would now leave every gate refusing.
land_in_nix() {
  if [ "${LAND_NO_NIX:-0}" = 1 ] || { [ "${CONCRETE_GATES_SHELL:-}" = 1 ] && command -v lake >/dev/null 2>&1; } \
     || ! command -v nix >/dev/null 2>&1 \
     || [ ! -f "$(git rev-parse --show-toplevel)/flake.nix" ]; then
    "$@"
  else
    nix develop "$(git rev-parse --show-toplevel)" --command "$@"
  fi
}

land_assert_no_op() {
  local what="$1" p
  for p in MERGE_HEAD CHERRY_PICK_HEAD REVERT_HEAD rebase-merge rebase-apply; do
    if [ -e "$(git rev-parse --git-path "$p")" ]; then
      echo "land: after $what: a git operation is still in progress ($p exists); stopping" >&2
      exit 1
    fi
  done
}

LEG_NAMES=(); LEG_PIDS=(); LEG_LOGS=()
legs_start() { # name log cmd...
  local n="$1" log="$2"; shift 2
  rm -f "$log.rc"
  (
    s="$(date +%s)"; rc=0
    "$@" > "$log" 2>&1 || rc=$?
    echo "$rc $(( $(date +%s) - s ))" > "$log.rc"
    exit "$rc"
  ) &
  LEG_NAMES+=("$n"); LEG_PIDS+=("$!"); LEG_LOGS+=("$log")
}

legs_wait() {
  LEGS_FAILED=""; LEGS_SUM=0; LEGS_REPORT=""
  local i rc d
  for i in "${!LEG_PIDS[@]}"; do
    wait "${LEG_PIDS[$i]}" 2>/dev/null || true
    rc=255; d=0
    [ -f "${LEG_LOGS[$i]}.rc" ] && read -r rc d < "${LEG_LOGS[$i]}.rc"
    LEGS_SUM=$(( LEGS_SUM + d ))
    LEGS_REPORT="${LEGS_REPORT:+$LEGS_REPORT }${LEG_NAMES[$i]}=$([ "$rc" = 0 ] && echo pass || echo "fail($rc)")/${d}s"
    [ "$rc" = 0 ] || LEGS_FAILED="${LEGS_FAILED:+$LEGS_FAILED }${LEG_NAMES[$i]}"
  done
  LEG_NAMES=(); LEG_PIDS=(); LEG_LOGS=()
}

# The move gate's dry-run diagnostic (train.sh require_rehearsal; acceptance.sh's dry-run land leg).
# One definition, so the two cannot drift apart; matched as a whole line, never as a substring.
MOVE_GATE_MARKER="train (dry-run): every other land check passed; refused only by the move gate."
dryland_only_move_gate() { # rc out
  [ "$1" = 5 ] || return 1
  local l
  while IFS= read -r l; do [ "$l" = "$MOVE_GATE_MARKER" ] && return 0; done <<< "$2"
  return 1
}
