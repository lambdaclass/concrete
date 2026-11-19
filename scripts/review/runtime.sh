# Require one absolute directory per colon-delimited PATH entry.
review_require_absolute_path() {
  local rest="$1" entry
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

# Anchor caller-supplied paths before the driver enters the repository or the
# runner enters the archive. Keep ".." intact for paths that cross symlinks.
review_resolve_paths() {
  local variable rest entry resolved
  for variable in HOME CODEX_HOME TMPDIR SSL_CERT_FILE SSL_CERT_DIR; do
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
