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
