# Shared helpers for scripts/land/*.sh (sourced, never run). docs/LANDING.md is the manual.
#
#   land_in_nix cmd...          run cmd inside `nix develop` unless already there (LAND_NO_NIX=1 never)
#   land_assert_no_op what      die if a merge, rebase, cherry-pick or revert is in progress
#   legs_start name log cmd...  run cmd in the background as a named leg, output to log
#   legs_wait                   wait for every leg; sets LEGS_FAILED, LEGS_SUM, LEGS_REPORT
#   MOVE_GATE_MARKER            the line train.sh's dry-run land prints when only the move gate refused
#   dryland_only_move_gate rc out   0 iff a dry-run land exited 5 with MOVE_GATE_MARKER as a whole line
#   LAND_REPOSITORIES           the project's GitHub repositories (owner/name), a constant
#   land_url_target url             "<host> <owner/name>" of a network URL; returns 1 for a local one
#   land_url_bad url                print why <url> is outside LAND_REPOSITORIES and return 1 (local: 0)
#   land_url_redact url             the URL with a scheme URL's userinfo replaced by ***
#   land_remote_bad remote [repo]   print why <remote> is not one of LAND_REPOSITORIES (or not <repo>), return 1
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

# THE PROJECT'S REPOSITORIES (docs/decisions.md#decided-project-repository). Every fetch and push of
# the landing tooling, and every push the pre-push hook sees, goes to one of these and nowhere else.
# A constant, not an environment variable: nothing a run inherits can widen it. Both are listed while
# the migration to lambdaclass/concrete is under way; the one-repository change removes concrete2.
LAND_REPOSITORIES="lambdaclass/concrete unbalancedparentheses/concrete2"
land_known_repo() { # owner/name (lowercase) -> 0 iff it is one of LAND_REPOSITORIES
  local k
  for k in $LAND_REPOSITORIES; do [ "$1" = "$(printf '%s' "$k" | tr 'A-Z' 'a-z')" ] && return 0; done
  return 1
}
# land_url_target and the ssh alias resolution it uses. A NETWORK URL must be github.com/<one of
# LAND_REPOSITORIES>: host github.com (an ssh host alias is resolved with `ssh -G`, with the URL's
# user and port, which reads the ssh config and connects nowhere: git@github-lambdaclass:... on the
# servers is github.com), owner/name one of the list. Another host serving that path, a scheme git
# does not use for GitHub and any other repository are refused. A local path or file:// URL (the test
# fixtures' bare repositories) passes.
#
# land_url_target url: returns 1 for a local URL (a path, or file://). For a network URL returns 0
# and prints "<host> <owner/name>", both lowercased, the host without user, port or trailing dot and
# resolved through the ssh config for ssh transports; "? ?" for a scheme other than
# http(s)/ssh/git/git+ssh/ssh+git or a host, user or port that is not plain.
# land_ssh_host host [user] [port]: the HostName ssh resolves <host> to, asking the ssh command git
# itself would run here (git's order, connect.c: GIT_SSH_COMMAND, else core.sshCommand, else
# GIT_SSH, else ssh) with -G, which reads the configuration and connects nowhere; the URL's user and port are
# passed as git's ssh would, so a `Match user` or one on the port resolves as for the real
# connection, and so does an override such as GIT_SSH_COMMAND='ssh -o HostName=...'. An override
# that is not ssh (a wrapper program, plink, ...) cannot be asked: returns 1 and the URL is
# refused. Without ssh installed, the host is itself.
land_ssh_host() {
  local cmd prog h args=()
  [ -z "${2:-}" ] || args+=(-l "$2")
  [ -z "${3:-}" ] || args+=(-p "$3")
  if [ -n "${GIT_SSH_COMMAND:-}" ]; then cmd="$GIT_SSH_COMMAND"
  elif cmd="$(git config --get core.sshCommand 2>/dev/null)" && [ -n "$cmd" ]; then :
  elif [ -n "${GIT_SSH:-}" ]; then
    [ "${GIT_SSH##*/}" = ssh ] || return 1
    h="$("$GIT_SSH" -G ${args[@]+"${args[@]}"} "$1" 2>/dev/null | awk '$1 == "hostname" { print $2; exit }')" || h=""
    printf '%s\n' "${h:-$1}"; return 0
  else cmd=ssh
  fi
  prog="${cmd%%[[:space:]]*}"; prog="${prog##*/}"
  [ "$prog" = ssh ] || return 1
  # As git runs it: the command through the shell, with git's arguments appended.
  h="$(sh -c "$cmd -G \"\$@\"" land-ssh-G ${args[@]+"${args[@]}"} "$1" 2>/dev/null | awk '$1 == "hostname" { print $2; exit }')" || h=""
  printf '%s\n' "${h:-$1}"
}
land_url_target() {
  local u="$1" scheme="" auth p h user="" port=""
  case "$u" in
    [Ff][Ii][Ll][Ee]://*) return 1 ;;
    *://*) scheme="$(printf '%s' "${u%%://*}" | tr 'A-Z' 'a-z')"; p="${u#*://}"
           case "$p" in */*) auth="${p%%/*}"; p="${p#*/}" ;; *) auth="$p"; p="" ;; esac ;;
    *:*) case "${u%%:*}" in */*) return 1 ;; esac
         scheme=scp; auth="${u%%:*}"; p="${u#*:}" ;;   # scp-like [user@]host:path (git's rule)
    *) return 1 ;;
  esac
  h="${auth##*@}"
  case "$auth" in *@*) user="${auth%@*}"; user="${user%%:*}" ;; esac
  case "$scheme" in scp) ;; *) case "$h" in *:*) port="${h#*:}"; h="${h%%:*}" ;; esac ;; esac
  h="$(printf '%s' "$h" | tr 'A-Z' 'a-z')"
  # A plain name (ssh aliases may carry an underscore); the RESOLVED name is compared, not this one.
  case "$h" in ""|-*|*[!a-z0-9._-]*) echo "? ?"; return 0 ;; esac
  case "$port" in *[!0-9]*) echo "? ?"; return 0 ;; esac
  case "$scheme" in
    scp|ssh|git+ssh|ssh+git)
      # Only an ssh user reaches ssh -G, so only it must be plain (http userinfo may be percent-encoded
      # and is never passed on).
      case "$user" in -*|*[!A-Za-z0-9._-]*) echo "? ?"; return 0 ;; esac
      h="$(land_ssh_host "$h" "$user" "$port")" || { echo "? ?"; return 0; }
      h="$(printf '%s' "$h" | tr 'A-Z' 'a-z')" ;;
    https|http|git) ;;
    *) echo "? ?"; return 0 ;;
  esac
  h="${h%.}"
  while :; do case "$p" in /*) p="${p#/}" ;; *) break ;; esac; done
  p="${p%/}"; p="${p%.git}"
  printf '%s %s\n' "$h" "$(printf '%s' "$p" | tr 'A-Z' 'a-z')"
}
# land_url_redact url: the URL with any userinfo of a scheme URL (https://user:TOKEN@host) replaced
# by "***", for messages; an scp-like user@ (no secret) is kept.
land_url_redact() {
  case "$1" in
    *://*@*) local rest="${1#*://}" auth path=""
             auth="${rest%%/*}"; case "$rest" in */*) path="/${rest#*/}" ;; esac
             # through the LAST @ of the authority: userinfo may itself contain @
             case "$auth" in *@*) printf '%s://***@%s%s\n' "${1%%://*}" "${auth##*@}" "$path"; return 0 ;; esac ;;
  esac
  printf '%s\n' "$1"
}
# land_url_bad url: print why <url> is not a URL of one of the project's repositories and return 1;
# 0 for such a URL and for a local one. The URL is first expanded as git would (url.*.insteadOf,
# through `git ls-remote --get-url`, which connects nowhere), as `git remote get-url` does for a remote.
land_url_bad() {
  local target u
  u="$(git ls-remote --get-url "$1" 2>/dev/null)" && [ -n "$u" ] || u="$1"
  target="$(land_url_target "$u")" || return 0
  if [ "${target%% *}" != github.com ] || ! land_known_repo "${target#* }"; then
    echo "$(land_url_redact "$1") is not one of the project's repositories ($LAND_REPOSITORIES on github.com): it points at another repository"
    return 1
  fi
  return 0
}
# land_remote_bad remote [repo]: every fetch and push URL of <remote> (git pushes to
# remote.<r>.pushurl when one is set, and to each of several; get-url applies url.*.insteadOf) that
# is a network URL must be github.com/<one of LAND_REPOSITORIES>, all of them the SAME repository
# (a fetch URL at one and a push URL at the other is refused), and, with <repo> (the repository gh
# reads), that repository. A remote with only local URLs passes and leaves <repo> free for a fake gh.
# Prints why and returns 1.
land_remote_bad() { # remote [repo]
  local r="$1" want="" u target urls host path first=""
  [ -z "${2:-}" ] || want="$(printf '%s' "$2" | tr 'A-Z' 'a-z')"
  urls="$(git remote get-url --all "$r" 2>/dev/null)" || { echo "the remote '$r' is not configured"; return 1; }
  urls="$urls"$'\n'"$(git remote get-url --push --all "$r" 2>/dev/null)"
  while IFS= read -r u; do
    [ -n "$u" ] || continue
    target="$(land_url_target "$u")" || continue
    host="${target%% *}"; path="${target#* }"
    if [ "$host" != github.com ] || ! land_known_repo "$path"; then
      echo "$r uses $(land_url_redact "$u"), which is not one of the project's repositories ($LAND_REPOSITORIES on github.com): this clone points at another repository. Repoint it: git remote set-url $r https://github.com/lambdaclass/concrete.git (and drop any other url or pushurl of $r)"
      return 1
    fi
    if [ -n "$first" ] && [ "$path" != "$first" ]; then
      echo "$r fetches or pushes both $first and $path: this clone points at another repository for some of its URLs. Give $r one repository (drop the other url or pushurl)"
      return 1
    fi
    first="$path"
  done <<< "$urls"
  if [ -n "$first" ] && [ -n "$want" ] && [ "$first" != "$want" ]; then
    echo "LAND_REPO is '$2' but $r is github.com/$first: gh would read another repository. Set LAND_REPO to $r's repository, or repoint $r"
    return 1
  fi
  return 0
}
dryland_only_move_gate() { # rc out
  [ "$1" = 5 ] || return 1
  local l
  while IFS= read -r l; do [ "$l" = "$MOVE_GATE_MARKER" ] && return 0; done <<< "$2"
  return 1
}
