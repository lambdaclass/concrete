# Sourced by scripts/land/acceptance.sh, and by scripts/checks/landing_script.sh for its controls.
#
#   make_git_shim BIN-DIR REAL-GIT PROTECTED-URL...
#
# writes BIN-DIR/git: a git that refuses a push which could move main on a protected repository and
# passes everything else to REAL-GIT. The PROTECTED-URLs are the clone's real remotes (origin and the
# mirror); they are baked into the shim, so nothing a leg sets in its environment can change them.
#
# The root checks the train's impact leg runs (landing_bot.sh, landing_script.sh, review_tool.sh)
# push main to throwaway local fixture repositories; refusing those made every acceptance run fail
# (ISS-0017). A destination is therefore unprotected only when every URL it resolves to is an
# existing local path that is none of the protected ones. Everything else is protected and fails
# closed: no repository argument (the default remote), a remote or raw URL that is not an existing
# local path (https, ssh, a relative path the shim cannot resolve), any url.*.insteadOf or
# pushInsteadOf rewrite in effect, and a protected URL however it is spelled (a remote name, the URL
# itself, a file:// URL, --repo=, a second push URL).
#
# A push "could move main" when it names --all or --mirror, no refspec, ":", a glob, or a refspec whose
# destination (or lone source) is main, refs/heads/main, any */main, HEAD or @.
make_git_shim() {
  local dir="$1" real="$2"; shift 2
  local p prot=""
  for p in "$@"; do [ -n "$p" ] && prot+="$(printf '%q ' "$p")"; done
  mkdir -p "$dir"
  {
    printf '#!/usr/bin/env bash\n'
    printf 'REAL=%q\n' "$real"
    printf 'PROTECTED=(%s)\n' "$prot"
    cat <<'SHIM'
pre=(); i=0; args=("$@")
while [ "$i" -lt "${#args[@]}" ] && [ "${args[$i]}" != push ]; do
  pre+=("${args[$i]}")
  case "${args[$i]}" in -C|-c|--git-dir|--work-tree|--namespace) i=$((i+1)); pre+=("${args[$i]:-}") ;; esac
  i=$((i+1))
done
[ "$i" -lt "${#args[@]}" ] || exec "$REAL" "$@"   # not a push
# As git reads it: the first positional argument is the repository; --repo only stands in for a
# missing one; the remaining positionals are refspecs.
repo=""; optrepo=""; have_repo=0; specs=(); flagall=0; i=$((i+1))
while [ "$i" -lt "${#args[@]}" ]; do
  a="${args[$i]}"
  case "$a" in
    --all|--mirror) flagall=1 ;;
    --repo=*) optrepo="${a#--repo=}" ;;
    --repo|-o|--push-option|--receive-pack|--exec) i=$((i+1)); [ "$a" = --repo ] && optrepo="${args[$i]:-}" ;;
    --) ;;
    -*) ;;
    *) if [ "$have_repo" = 0 ]; then repo="$a"; have_repo=1; else specs+=("$a"); fi ;;
  esac
  i=$((i+1))
done
[ "$have_repo" = 1 ] || repo="$optrepo"
# No refspec (the remote's configured push refspecs or push.default decide), ":" (every matching
# branch) and a glob all could move main; so does a destination, or a lone source, that names main
# (main, refs/heads/main, any */main) or HEAD (HEAD, @).
moves_main=$flagall
[ "${#specs[@]}" = 0 ] && moves_main=1
for s in "${specs[@]}"; do
  t="${s#+}"; d="${t##*:}"; [ -n "$d" ] || d="${t%%:*}"
  case "$t" in *'*'*|:) moves_main=1 ;; esac
  case "$d" in main|*/main|HEAD|@) moves_main=1 ;; esac
done
[ "$moves_main" = 1 ] || exec "$REAL" "$@"
canon() { # a URL as a canonical local path, or nothing when it is not an existing local path
  local u="${1#file://}"
  case "$u" in /*) [ -e "$u" ] && (cd "$u" 2>/dev/null && pwd -P) ;; esac
}
protected=0
if [ -z "$repo" ]; then
  protected=1
elif [ -n "$("$REAL" "${pre[@]}" config --get-regexp '^url\..*\.(push)?insteadof$' 2>/dev/null)" ]; then
  protected=1
else
  urls="$("$REAL" "${pre[@]}" remote get-url --push --all "$repo" 2>/dev/null)" || urls="$repo"
  [ -n "$urls" ] || urls="$repo"
  while IFS= read -r u; do
    c="$(canon "$u")"
    [ -n "$c" ] || { protected=1; break; }
    for p in "${PROTECTED[@]}"; do
      [ "$u" = "$p" ] && { protected=1; break; }
      pc="$(canon "$p")"; [ -n "$pc" ] && [ "$c" = "$pc" ] && { protected=1; break; }
    done
    [ "$protected" = 1 ] && break
  done <<< "$urls"
fi
if [ "$protected" = 1 ]; then
  echo "acceptance git shim: refusing a push to ${repo:-the default remote} that could move main (git $*)" >&2
  exit 1
fi
exec "$REAL" "$@"
SHIM
  } > "$dir/git"
  chmod +x "$dir/git"
}
