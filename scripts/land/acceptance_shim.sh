# Sourced by scripts/land/acceptance.sh, and by scripts/checks/landing_script.sh for its controls.
#
#   make_git_shim BIN-DIR REAL-GIT PROTECTED-URL...
#
# writes BIN-DIR/git: a git that refuses a push which could move main on a protected repository and
# passes everything else to REAL-GIT. The PROTECTED-URLs are the clone's real remotes (origin and the
# mirror). A local one, in any spelling git accepts (path, file://, file://localhost, ~, relative to
# the caller's directory), is resolved to its repository identity
# (the absolute, symlink-free git common directory) when the shim is made; both its spelling and its
# identity are baked into the shim, so nothing a leg sets later can change them. A protected local
# URL that names no repository is an error: the shim is not made.
#
# The root checks the train's impact leg runs (landing_bot.sh, landing_script.sh, review_tool.sh)
# push main to throwaway local fixture repositories; refusing those made every acceptance run fail
# (ISS-0017). A destination is therefore unprotected only when every URL it resolves to is an
# absolute local path (or file:// URL) naming an existing repository whose identity is none of the
# protected ones. Everything else counts as protected and fails closed: no repository argument (the
# default remote), a URL that is relative, remote (https, ssh) or names no repository, a protected
# spelling or identity (a remote name, the URL, file://, a non-bare repository's .git, a symlink, a
# second push URL, --repo), and any url.*.insteadOf or pushInsteadOf rewrite in effect.
#
# A push "could move main" when it names --all, --branches, --mirror or --prune, no refspec, ":", a
# glob, or a refspec whose destination (or lone source) is main, refs/heads/main, any */main, HEAD or
# @. Push options come from a fixed list, spelled in full; an abbreviated, bundled or unknown one is
# refused, and so is submodule recursion (by option or push.recurseSubmodules), whose remotes the shim
# does not judge.
#
# The subcommand is found the way git finds it (after the global options; an unknown one is refused),
# and an alias is expanded
# the way git would run it: a builtin or a git-<name> command is never an alias; any other alias is
# expanded and judged again. A shell alias ("!...") is refused: git runs it with its own exec path
# first on PATH, so the git inside it would not be this shim. So is a quoted alias, which the shim
# does not split as git does, and one nested past 10 levels.
# The shim guards git as found on PATH; a script that runs the real git by its absolute path is
# stopped by acceptance's other two guards (the pre-push guard and the before/after read of main).

# repo_identity REAL-GIT ABSOLUTE-PATH: the absolute git common directory of the repository git would
# open at this path, or nothing (no such path, not absolute, not a repository).
repo_identity() {
  local real="$1" u="$2" d
  case "$u" in /*) ;; *) return 0 ;; esac
  [ -e "$u" ] || return 0
  d="$(env -u GIT_DIR -u GIT_WORK_TREE -u GIT_COMMON_DIR -u GIT_INDEX_FILE -u GIT_OBJECT_DIRECTORY \
        "$real" -C "$u" rev-parse --path-format=absolute --git-common-dir 2>/dev/null)" || return 0
  [ -n "$d" ] && (cd "$d" 2>/dev/null && pwd -P)
}

# local_url_path URL [dest]: the absolute path git would open for a local URL, as git spells them (an
# absolute path, file:///p, file://localhost/p, ~/p, ~user/p, or a path relative to $PWD); nothing for
# a remote URL (scheme://, or host:path with no slash before the colon); exit 1 when it is local but
# cannot be resolved (file://otherhost/, an unknown ~user) and, with "dest", when it is relative (the
# shim does not know which directory git will resolve it from).
local_url_path() {
  local u="$1" mode="${2:-}" user home
  case "$u" in
    /*) printf '%s\n' "$u"; return 0 ;;
    file://localhost/*) u="/${u#file://localhost/}" ;;
    file:///*) u="${u#file://}" ;;
    file://*) return 1 ;;
  esac
  [[ "$u" =~ ^[A-Za-z][A-Za-z0-9+.-]*:// ]] && return 0
  case "$u" in
    '~'|'~/'*) u="$HOME${u#\~}" ;;
    '~'*) user="${u%%/*}"; user="${user#\~}"; home="$(getent passwd "$user" | cut -d: -f6)"
          [ -n "$home" ] || return 1
          case "$u" in */*) u="$home/${u#*/}" ;; *) u="$home" ;; esac ;;
  esac
  case "$u" in
    /*) ;;
    *) case "${u%%/*}" in *:*) return 0 ;; esac; [ "$mode" = dest ] && return 1; u="$PWD/$u" ;;
  esac
  printf '%s\n' "$u"
}

make_git_shim() {
  local dir="$1" real="$2"; shift 2
  local p raw="" ids="" id abs
  for p in "$@"; do
    [ -n "$p" ] || continue
    raw+="$(printf '%q ' "$p")"
    abs="$(local_url_path "$p")" || { echo "make_git_shim: cannot resolve the protected local URL $p" >&2; return 1; }
    [ -n "$abs" ] || continue
    id="$(repo_identity "$real" "$abs")"
    [ -n "$id" ] || { echo "make_git_shim: the protected local URL $p names no repository" >&2; return 1; }
    ids+="$(printf '%q ' "$id")"
  done
  mkdir -p "$dir"
  {
    printf '#!/usr/bin/env bash\n'
    printf 'REAL=%q\n' "$real"
    printf 'PROTECTED_RAW=(%s)\n' "$raw"
    printf 'PROTECTED_ID=(%s)\n' "$ids"
    printf 'BUILTINS=%q\n' " $("$real" --list-cmds=builtins 2>/dev/null | tr '\n' ' ')"
    declare -f repo_identity local_url_path
    cat <<'SHIM'
refuse() { echo "acceptance git shim: refusing $1 (git $ARGS)" >&2; exit 1; }
ARGS="$*"; args=("$@"); pre=(); i=0
# Global options, as git reads them; the first other argument is the subcommand. An option the
# shim does not know is refused: it might take a value the shim would read as the subcommand.
while [ "$i" -lt "${#args[@]}" ]; do
  a="${args[$i]}"
  case "$a" in
    -C|-c|--git-dir|--work-tree|--namespace|--config-env|--attr-source)
      pre+=("$a" "${args[$((i+1))]:-}"); i=$((i+2)) ;;
    --git-dir=*|--work-tree=*|--namespace=*|--config-env=*|--attr-source=*|--exec-path|--exec-path=*|\
    --super-prefix=*|--list-cmds=*|-p|--paginate|-P|--no-pager|--bare|--no-replace-objects|--no-lazy-fetch|\
    --no-optional-locks|--no-advice|--literal-pathspecs|--glob-pathspecs|--noglob-pathspecs|--icase-pathspecs|\
    --html-path|--man-path|--info-path|-v|--version|-h|--help)
      pre+=("$a"); i=$((i+1)) ;;
    -*) refuse "an unknown global option $a" ;;
    *) break ;;
  esac
done
[ "$i" -lt "${#args[@]}" ] || exec "$REAL" "$@"
sub="${args[$i]}"; rest=("${args[@]:$((i+1))}")
if [ "$sub" != push ]; then
  case "$BUILTINS" in *" $sub "*) exec "$REAL" "$@" ;; esac
  command -v "git-$sub" >/dev/null 2>&1 && exec "$REAL" "$@"
  [ -x "$("$REAL" --exec-path 2>/dev/null)/git-$sub" ] && exec "$REAL" "$@"
  al="$("$REAL" "${pre[@]}" config --get "alias.$sub" 2>/dev/null)" || exec "$REAL" "$@"
  case "$al" in '!'*) refuse "a shell alias '$sub' (git runs it with its own exec path first, so its git is not this shim)" ;; esac
  case "$al" in *[\'\"\\]*) refuse "a quoted alias '$sub' (the shim cannot split it as git does)" ;; esac
  depth="${ACCEPT_SHIM_DEPTH:-0}"; case "$depth" in ''|*[!0-9]*) depth=10 ;; esac
  [ "$depth" -lt 10 ] || refuse "an alias nested past 10 levels"
  read -r -a words <<< "$al"
  ACCEPT_SHIM_DEPTH=$((depth+1)) exec "$0" "${pre[@]}" "${words[@]}" "${rest[@]}"
fi
# As git reads push: the first positional argument is the repository; --repo only stands in for a
# missing one; the remaining positionals are refspecs. Options come from a fixed list, spelled in
# full: git accepts abbreviations (--mir for --mirror) and bundled short flags, and an option the
# shim misreads could hide a repository or a refspec, so anything else is refused.
repo=""; optrepo=""; have_repo=0; specs=(); flagall=0; j=0; dashdash=0
positional() { if [ "$have_repo" = 0 ]; then repo="$1"; have_repo=1; else specs+=("$1"); fi; }
submod() { case "$1" in no|check) ;; *) refuse "a push with submodule recursion ($1): the submodules' remotes are not judged" ;; esac; }
while [ "$j" -lt "${#rest[@]}" ]; do
  a="${rest[$j]}"
  if [ "$dashdash" = 1 ]; then positional "$a"; j=$((j+1)); continue; fi
  case "$a" in
    --) dashdash=1 ;;
    --all|--branches|--mirror|--prune) flagall=1 ;;
    --repo=*) optrepo="${a#--repo=}" ;;
    --repo) j=$((j+1)); optrepo="${rest[$j]:-}" ;;
    -o|--push-option|--receive-pack|--exec) j=$((j+1)) ;;
    --push-option=*|--receive-pack=*|--exec=*|--force-with-lease=*|--signed=*) ;;
    --recurse-submodules=*) submod "${a#--recurse-submodules=}" ;;
    --recurse-submodules) j=$((j+1)); submod "${rest[$j]:-}" ;;
    --no-recurse-submodules) ;;
    -4|-6|--ipv4|--ipv6|-n|--dry-run|--porcelain|-q|--quiet|-v|--verbose|--progress|--no-progress|\
    -f|--force|--no-force|--force-with-lease|--no-force-with-lease|--force-if-includes|--no-force-if-includes|\
    --atomic|--no-atomic|--thin|--no-thin|--verify|--no-verify|-u|--set-upstream|--no-set-upstream|\
    --tags|--no-tags|--follow-tags|--no-follow-tags|--signed|--no-signed|-d|--delete) ;;
    -*) refuse "an unknown or abbreviated push option $a" ;;
    *) positional "$a" ;;
  esac
  j=$((j+1))
done
case "$("$REAL" "${pre[@]}" config --get push.recurseSubmodules 2>/dev/null)" in
  ''|no|false|check) ;; *) refuse "a push while push.recurseSubmodules is set: the submodules' remotes are not judged" ;;
esac
# With push.recurseSubmodules unset, submodule.recurse=true means on-demand recursion.
case "$("$REAL" "${pre[@]}" config --type=bool --get submodule.recurse 2>/dev/null)" in
  true) [ -n "$("$REAL" "${pre[@]}" config --get push.recurseSubmodules 2>/dev/null)" ] \
          || refuse "a push while submodule.recurse is true: the submodules' remotes are not judged" ;;
esac
[ "$have_repo" = 1 ] || repo="$optrepo"
moves_main=$flagall
[ "${#specs[@]}" = 0 ] && moves_main=1
for s in "${specs[@]}"; do
  t="${s#+}"; d="${t##*:}"; [ -n "$d" ] || d="${t%%:*}"
  case "$t" in *'*'*|:) moves_main=1 ;; esac
  case "$d" in main|*/main|HEAD|@) moves_main=1 ;; esac
done
if [ "$moves_main" = 0 ]; then
  # A push that names no main can still move it through a symbolic ref on the destination
  # (refs/heads/alias -> refs/heads/main). On a protected local repository, a destination that is a
  # symbolic ref is refused. (The real remotes are GitHub repositories, which have no branch symrefs.)
  [ -n "$repo" ] || exec "$REAL" "$@"
  urls="$("$REAL" "${pre[@]}" remote get-url --push --all "$repo" 2>/dev/null)" || urls="$repo"
  while IFS= read -r u; do
    abs="$(local_url_path "$u" dest 2>/dev/null)" || continue
    id="$(repo_identity "$REAL" "$abs")"; [ -n "$id" ] || continue
    for p in "${PROTECTED_ID[@]}"; do
      [ "$id" = "$p" ] || continue
      for s in "${specs[@]}"; do
        t="${s#+}"; d="${t##*:}"; [ -n "$d" ] || d="${t%%:*}"
        case "$d" in refs/*) ;; *) d="refs/heads/$d" ;; esac
        if env -u GIT_DIR -u GIT_WORK_TREE -u GIT_COMMON_DIR "$REAL" --git-dir="$id" symbolic-ref -q "$d" >/dev/null 2>&1; then
          refuse "a push to $d, a symbolic ref on the protected repository $id"
        fi
      done
    done
  done <<< "$urls"
  exec "$REAL" "$@"
fi
[ -n "$repo" ] || refuse "a push to the default remote that could move main"
[ -z "$("$REAL" "${pre[@]}" config --get-regexp '^url\..*\.(push)?insteadof$' 2>/dev/null)" ] \
  || refuse "a push that could move main while a url insteadOf rewrite is in effect"
urls="$("$REAL" "${pre[@]}" remote get-url --push --all "$repo" 2>/dev/null)" || urls="$repo"
[ -n "$urls" ] || urls="$repo"
while IFS= read -r u; do
  for p in "${PROTECTED_RAW[@]}"; do [ "$u" = "$p" ] && refuse "a push to $repo, a protected remote, that could move main"; done
  abs="$(local_url_path "$u" dest)" || refuse "a push to $repo that could move main: $u is relative or cannot be resolved"
  [ -n "$abs" ] || refuse "a push to $repo that could move main: $u is a remote URL"
  id="$(repo_identity "$REAL" "$abs")"
  [ -n "$id" ] || refuse "a push to $repo that could move main: $u names no local repository"
  for p in "${PROTECTED_ID[@]}"; do [ "$id" = "$p" ] && refuse "a push to $repo ($id, a protected repository) that could move main"; done
done <<< "$urls"
exec "$REAL" "$@"
SHIM
  } > "$dir/git"
  chmod +x "$dir/git"
}
