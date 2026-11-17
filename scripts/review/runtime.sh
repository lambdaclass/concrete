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
