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

review_lock_dir() {
  local common
  common="$(git rev-parse --path-format=absolute --git-common-dir)" || return 2
  mkdir -p "$common/concrete-review-locks" || return 2
  printf '%s\n' "$common/concrete-review-locks"
}

# flock(2) is available through Python on Linux and macOS. The driver owns the
# open file description, so cancellation releases it without unlinking a lock.
review_lock() {
  "$PYTHON" -I -c '
import fcntl
import sys

operation = {
    "lock": fcntl.LOCK_EX,
    "nonblocking": fcntl.LOCK_EX | fcntl.LOCK_NB,
    "unlock": fcntl.LOCK_UN,
}[sys.argv[2]]
try:
    fcntl.flock(int(sys.argv[1]), operation)
except BlockingIOError:
    sys.exit(1)
' "$1" "${2:-lock}"
}

# Compare original objects in a bare repository with no worktree, index,
# attribute source or inherited Git configuration. The empty template rules
# out injected info/attributes. Name binary changes without sending payloads.
review_range_patch() (
  set -e
  local script_dir="$1"
  local bare="$2"
  local from="$3"
  local head="$4"
  local object_format object_directory
  local object_alternates="${GIT_ALTERNATE_OBJECT_DIRECTORIES-}"
  object_format="$(git rev-parse --show-object-format)" || exit 2
  object_directory="$(git rev-parse --path-format=absolute --git-path objects)" || exit 2
  source "$script_dir/../checks/lib/git_sandbox.sh" || exit 2
  # This subshell deliberately uses a new config even inside a fixture sandbox.
  unset _CONCRETE_GIT_SANDBOX_CONFIG
  git_sandbox /dev/null || exit 2
  unset GIT_ATTR_SOURCE
  export GIT_ATTR_NOSYSTEM=1 LC_ALL=C
  git init -q --bare --template= --object-format="$object_format" "$bare" || exit 2
  # Keep the current directory for relative alternate object paths.
  GIT_OBJECT_DIRECTORY="$object_directory" GIT_ALTERNATE_OBJECT_DIRECTORIES="$object_alternates" \
    git --git-dir="$bare" -c core.attributesFile=/dev/null \
      diff --no-color --no-ext-diff --no-textconv -M "$from" "$head"
)

review_patch_digest() {
  "$1" -I -c '
import hashlib
import sys

digest = hashlib.sha256()
for block in iter(lambda: sys.stdin.buffer.read(1024 * 1024), b""):
    digest.update(block)
print(digest.hexdigest())
'
}

review_range_digest() (
  set -o pipefail
  local scratch
  scratch="$(mktemp -d)" || exit 2
  trap 'rm -rf "$scratch"' EXIT
  review_range_patch "$1" "$scratch/diff.git" "$3" "$4" | review_patch_digest "$2"
)
