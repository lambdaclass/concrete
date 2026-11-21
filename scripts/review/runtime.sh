# Require one absolute directory per colon-delimited PATH entry.
review_require_absolute_path() {
  local rest="$1"
  local entry
  while :; do
    entry=${rest%%:*}
    case "$entry" in
      /*) ;;
      *)
        printf 'review: PATH entries must be absolute directories (invalid entry: %q)\n' "$entry" >&2
        return 1
        ;;
    esac
    case "$rest" in
      *:*) rest=${rest#*:} ;;
      *) break ;;
    esac
  done
  export PATH
}

# Resolve from the invoking directory, ignoring shell functions and builtins.
review_executable() {
  local executable
  case "$1" in
    */*) executable="$1" ;;
    *) executable="$(type -P -- "$1")" || return 1 ;;
  esac
  case "$executable" in /*) ;; *) executable="$PWD/$executable" ;; esac
  [ -f "$executable" ] && [ -x "$executable" ] || return 1
  printf '%s\n' "$executable"
}

# Anchor CODEX_HOME in the caller's directory and exclude the live checkout.
# Pass the original caller directory when preparing the environment after cd.
review_prepare_codex_home() {
  [ "${CODEX_HOME+x}" = x ] || return 0
  if [ -z "$CODEX_HOME" ]; then
    echo "review: CODEX_HOME requires a nonempty path" >&2
    return 1
  fi
  local resolved
  resolved="$(python3 -I -c '
from pathlib import Path
import sys

try:
    root = Path(sys.argv[1]).resolve()
    home = (Path(sys.argv[3]) / sys.argv[2]).resolve()
except (OSError, RuntimeError) as exc:
    sys.exit(f"review: cannot resolve CODEX_HOME or repository root: {exc}")
if home == root or root in home.parents:
    sys.exit(f"review: CODEX_HOME must be outside the repository root ({root}): {home}")
# A final slash preserves any trailing newlines through shell command substitution.
print(home, end="/")
' "$1" "$CODEX_HOME" "${2:-$PWD}")" || return 1
  export CODEX_HOME="${resolved%/}"
}

# Anchor caller-supplied paths before the driver enters the repository or the
# runner enters the archive. Keep ".." intact for paths that cross symlinks.
review_resolve_paths() {
  local variable rest entry resolved
  for variable in HOME TMPDIR SSL_CERT_FILE SSL_CERT_DIR; do
    [ "${!variable+x}" = x ] || continue
    rest=${!variable}
    resolved=
    while :; do
      entry=$rest
      if [ "$variable" = SSL_CERT_DIR ]; then entry=${rest%%:*}; fi
      case "$entry" in
        /*) ;;
        "")
          printf 'review: %s requires nonempty paths\n' "$variable" >&2
          return 1
          ;;
        *) entry="$PWD/$entry" ;;
      esac
      resolved="${resolved:+$resolved:}$entry"
      if [ "$variable" != SSL_CERT_DIR ] || [[ "$rest" != *:* ]]; then break; fi
      rest=${rest#*:}
    done
    export "$variable=$resolved"
  done
}
