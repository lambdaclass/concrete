# Sourced by scripts/land/acceptance.sh, and by scripts/checks/landing_script.sh for its controls.
#
#   make_git_shim BIN-DIR REAL-GIT PROTECTED-URL...
#
# writes BIN-DIR/git: a git that refuses a push which could move main on a protected repository and
# passes everything else to REAL-GIT. The PROTECTED-URLs are the clone's real remotes (origin and the
# mirror). A local one is resolved, relative to the caller's directory, to its repository identity
# (the absolute, symlink-free git common directory) when the shim is made; both its spelling and its
# identity are baked into the shim, so nothing a leg sets later can change them.
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
# A push "could move main" when it names --all or --mirror, no refspec, ":", a glob, or a refspec whose
# destination (or lone source) is main, refs/heads/main, any */main, HEAD or @.
#
# The subcommand is found the way git finds it (after the global options), and an alias is expanded
# the way git would run it: a builtin or a git-<name> command is never an alias; any other alias is
# expanded and judged again. A shell alias ("!...") is refused: git runs it with its own exec path
# first on PATH, so the git inside it would not be this shim. So is a quoted alias, which the shim
# does not split as git does, and one nested past 10 levels.
# The shim guards git as found on PATH; a script that runs the real git by its absolute path is
# stopped by acceptance's other two guards (the pre-push guard and the before/after read of main).

# repo_identity PATH-OR-FILE-URL: the absolute git common directory of the repository git would open
# for this local push destination, or nothing (no such path, relative, not a repository).
repo_identity() {
  local real="$1" u="${2#file://}" d
  case "$u" in /*) ;; *) return 0 ;; esac
  [ -e "$u" ] || return 0
  d="$(env -u GIT_DIR -u GIT_WORK_TREE -u GIT_COMMON_DIR -u GIT_INDEX_FILE -u GIT_OBJECT_DIRECTORY \
        "$real" -C "$u" rev-parse --path-format=absolute --git-common-dir 2>/dev/null)" || return 0
  [ -n "$d" ] && (cd "$d" 2>/dev/null && pwd -P)
}

make_git_shim() {
  local dir="$1" real="$2"; shift 2
  local p raw="" ids="" id abs
  for p in "$@"; do
    [ -n "$p" ] || continue
    raw+="$(printf '%q ' "$p")"
    abs="${p#file://}"
    case "$abs" in /*|*://*|*@*:*) ;; *) abs="$PWD/$abs" ;; esac
    id="$(repo_identity "$real" "$abs")"; [ -n "$id" ] && ids+="$(printf '%q ' "$id")"
  done
  mkdir -p "$dir"
  {
    printf '#!/usr/bin/env bash\n'
    printf 'REAL=%q\n' "$real"
    printf 'PROTECTED_RAW=(%s)\n' "$raw"
    printf 'PROTECTED_ID=(%s)\n' "$ids"
    printf 'BUILTINS=%q\n' " $("$real" --list-cmds=builtins 2>/dev/null | tr '\n' ' ')"
    declare -f repo_identity
    cat <<'SHIM'
refuse() { echo "acceptance git shim: refusing $1 (git $ARGS)" >&2; exit 1; }
ARGS="$*"; args=("$@"); pre=(); i=0
# Global options, as git reads them; the first other argument is the subcommand.
while [ "$i" -lt "${#args[@]}" ]; do
  a="${args[$i]}"
  case "$a" in
    -C|-c|--git-dir|--work-tree|--namespace) pre+=("$a" "${args[$((i+1))]:-}"); i=$((i+2)) ;;
    -*) pre+=("$a"); i=$((i+1)) ;;
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
# missing one; the remaining positionals are refspecs.
repo=""; optrepo=""; have_repo=0; specs=(); flagall=0; j=0
while [ "$j" -lt "${#rest[@]}" ]; do
  a="${rest[$j]}"
  case "$a" in
    --all|--mirror) flagall=1 ;;
    --repo=*) optrepo="${a#--repo=}" ;;
    --repo|-o|--push-option|--receive-pack|--exec) j=$((j+1)); [ "$a" = --repo ] && optrepo="${rest[$j]:-}" ;;
    --|-*) ;;
    *) if [ "$have_repo" = 0 ]; then repo="$a"; have_repo=1; else specs+=("$a"); fi ;;
  esac
  j=$((j+1))
done
[ "$have_repo" = 1 ] || repo="$optrepo"
moves_main=$flagall
[ "${#specs[@]}" = 0 ] && moves_main=1
for s in "${specs[@]}"; do
  t="${s#+}"; d="${t##*:}"; [ -n "$d" ] || d="${t%%:*}"
  case "$t" in *'*'*|:) moves_main=1 ;; esac
  case "$d" in main|*/main|HEAD|@) moves_main=1 ;; esac
done
[ "$moves_main" = 1 ] || exec "$REAL" "$@"
[ -n "$repo" ] || refuse "a push to the default remote that could move main"
[ -z "$("$REAL" "${pre[@]}" config --get-regexp '^url\..*\.(push)?insteadof$' 2>/dev/null)" ] \
  || refuse "a push that could move main while a url insteadOf rewrite is in effect"
urls="$("$REAL" "${pre[@]}" remote get-url --push --all "$repo" 2>/dev/null)" || urls="$repo"
[ -n "$urls" ] || urls="$repo"
while IFS= read -r u; do
  for p in "${PROTECTED_RAW[@]}"; do [ "$u" = "$p" ] && refuse "a push to $repo, a protected remote, that could move main"; done
  id="$(repo_identity "$REAL" "$u")"
  [ -n "$id" ] || refuse "a push to $repo that could move main: $u is not an absolute path to a local repository"
  for p in "${PROTECTED_ID[@]}"; do [ "$id" = "$p" ] && refuse "a push to $repo ($id, a protected repository) that could move main"; done
done <<< "$urls"
exec "$REAL" "$@"
SHIM
  } > "$dir/git"
  chmod +x "$dir/git"
}
