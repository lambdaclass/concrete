# Shared helpers for scripts/land/*.sh (sourced, never run). docs/LANDING.md is the manual.
#
#   land_in_nix cmd...          run cmd inside `nix develop` unless already there (LAND_NO_NIX=1 never)
#   land_assert_no_op what      die if a merge, rebase, cherry-pick or revert is in progress
#   legs_start name log cmd...  run cmd in the background as a named leg, output to log
#   legs_wait                   wait for every leg; sets LEGS_FAILED, LEGS_SUM, LEGS_REPORT
#   MOVE_GATE_MARKER            the line train.sh's dry-run land prints when only the move gate refused
#   dryland_only_move_gate rc out   0 iff a dry-run land exited 5 with MOVE_GATE_MARKER as a whole line
#   LAND_CANONICAL_REPO         the one GitHub repository (owner/name); not overridable
#   land_url_target url             "<host> <owner/name>" of a network URL; returns 1 for a local one
#   land_remote_bad remote repo     print why <remote> or <repo> is not LAND_CANONICAL_REPO and return 1, else 0
#
# A LEG is one independent validation (impact gates, remote full gates, Codex review). Legs of one
# candidate run at the same time and are all collected: one failing leg fails the whole, and no
# leg is abandoned while another is still running. LEGS_SUM is the sequential cost (the sum of the
# legs' durations); the caller compares it with the wall time to record what pipelining saved.

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

# The one repository since 2026-11-06 (docs/decisions.md#decided-project-repository). It is a constant,
# not LAND_REPO: a stale clone that still has origin=concrete2 and an old LAND_REPO=concrete2 in its
# environment would otherwise pass a check that only compares the two.
LAND_CANONICAL_REPO=lambdaclass/concrete
# Every fetch URL and every push URL of <remote> (git pushes to remote.<r>.pushurl when one is set,
# and to each of several; get-url applies url.*.insteadOf) that is a NETWORK URL must be
# github.com/LAND_CANONICAL_REPO: host github.com (an ssh host alias is resolved with `ssh -G`, which
# reads the ssh config and connects nowhere: git@github-lambdaclass:lambdaclass/concrete.git, used on
# the servers, is github.com) and owner/name LAND_CANONICAL_REPO. Another host serving that path, a
# scheme git does not use for GitHub, and the archived unbalancedparentheses/concrete2 under any
# host or alias are refused. When the remote has a network URL, <repo>, the repository gh reads,
# must be LAND_CANONICAL_REPO as well. A local path or file:// URL (the test fixtures' bare
# repositories) passes and leaves <repo> free for a fake gh. Prints why, returns 1.
#
# land_url_target url: returns 1 for a local URL (a path, or file://). For a network URL returns 0
# and prints "<host> <owner/name>", both lowercased, the host without user, port or trailing dot and
# resolved through the ssh config for ssh transports; "? ?" for a scheme other than
# http(s)/ssh/git/git+ssh/ssh+git or a host that is not a plain name.
land_ssh_host() { # host -> the HostName the ssh config gives it (itself without one, or without ssh)
  local h
  h="$(ssh -G "$1" 2>/dev/null | awk '$1 == "hostname" { print $2; exit }')" || h=""
  printf '%s\n' "${h:-$1}"
}
land_url_target() {
  local u="$1" scheme="" auth p h
  case "$u" in
    [Ff][Ii][Ll][Ee]://*) return 1 ;;
    *://*) scheme="$(printf '%s' "${u%%://*}" | tr 'A-Z' 'a-z')"; p="${u#*://}"
           case "$p" in */*) auth="${p%%/*}"; p="${p#*/}" ;; *) auth="$p"; p="" ;; esac ;;
    *:*) case "${u%%:*}" in */*) return 1 ;; esac
         scheme=scp; auth="${u%%:*}"; p="${u#*:}" ;;   # scp-like [user@]host:path (git's rule)
    *) return 1 ;;
  esac
  h="${auth##*@}"
  case "$scheme" in scp) ;; *) h="${h%%:*}" ;; esac
  h="$(printf '%s' "$h" | tr 'A-Z' 'a-z')"
  case "$h" in ""|-*|*[!a-z0-9.-]*) echo "? ?"; return 0 ;; esac
  case "$scheme" in
    scp|ssh|git+ssh|ssh+git) h="$(land_ssh_host "$h" | tr 'A-Z' 'a-z')" ;;
    https|http|git) ;;
    *) echo "? ?"; return 0 ;;
  esac
  h="${h%.}"
  while :; do case "$p" in /*) p="${p#/}" ;; *) break ;; esac; done
  p="${p%/}"; p="${p%.git}"
  printf '%s %s\n' "$h" "$(printf '%s' "$p" | tr 'A-Z' 'a-z')"
}
land_remote_bad() { # remote repo
  local r="$1" want u target urls net=0
  want="github.com $(printf '%s' "$LAND_CANONICAL_REPO" | tr 'A-Z' 'a-z')"
  urls="$(git remote get-url --all "$r" 2>/dev/null)" || { echo "the remote '$r' is not configured"; return 1; }
  urls="$urls"$'\n'"$(git remote get-url --push --all "$r" 2>/dev/null)"
  while IFS= read -r u; do
    [ -n "$u" ] || continue
    target="$(land_url_target "$u")" || continue
    net=1
    if [ "$target" != "$want" ]; then
      echo "$r uses $u, not github.com/$LAND_CANONICAL_REPO: this clone points at another repository. Repoint it: git remote set-url $r https://github.com/$LAND_CANONICAL_REPO.git (and drop any other url or pushurl of $r)"
      return 1
    fi
  done <<< "$urls"
  if [ "$net" = 1 ] && [ "$(printf '%s' "$2" | tr 'A-Z' 'a-z')" != "$(printf '%s' "$LAND_CANONICAL_REPO" | tr 'A-Z' 'a-z')" ]; then
    echo "LAND_REPO is '$2' but $r is github.com/$LAND_CANONICAL_REPO: gh would read another repository. Set LAND_REPO=$LAND_CANONICAL_REPO or unset it"
    return 1
  fi
  return 0
}
