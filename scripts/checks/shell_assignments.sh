#!/usr/bin/env bash
# Bash expands the right-hand sides of a local declaration before assigning any of them.
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
source "$ROOT_DIR/scripts/checks/lib/git_sandbox.sh" || exit 2
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

check() {
  python3 -I "$ROOT_DIR/scripts/checks/lib/shell_assignments.py"
}

(
  git_sandbox "$tmp/gitconfig"
  cd "$tmp"
  git init -q
  mkdir -p scripts/nested .githooks
  cat > split <<'CONTROL'
fixture() {
  local name="$1"
  local path="${name}.con"
  printf '%s\n' "$path"
}
CONTROL
  cat >> split <<'CONTROL'
literal() {
  local path='${name}.con' name="$1"
  printf '%s\n' "$path" "$name"
}
CONTROL
  cat > dependent <<'CONTROL'
fixture() {
  local name="$1" \
    path="${name}.con"
  printf '%s\n' "$path"
}
CONTROL
  library=scripts/nested/library.sh
  entry=.githooks/entry
  cp split "$library"
  printf '#!/usr/bin/env -S bash -e\n' > "$entry"
  cat split >> "$entry"
  git add -- "$library" "$entry"
  cp dependent scripts/untracked.sh
  check

  expect_failure() {
    local expected="$1" status="${2:-1}" out rc=0
    out="$(check 2>&1)" || rc=$?
    if [ "$rc" -ne "$status" ] || [[ "$out" != *"$expected"* ]]; then
      printf 'shell_assignments: expected %s (exit %s, got %s)\n%s\n' "$expected" "$status" "$rc" "$out" >&2
      exit 1
    fi
  }

  cp dependent "$library"
  SHELLCHECK_OPTS=--exclude=SC2318 expect_failure '[SC2318]'
  printf '\nif true; then\n  true\n' >> "$library"
  expect_failure "$library: shfmt:" 2
  for initializer in '"${name}.con"' '"${prefix:-$name}"' '"$((name + 1))"'; do
    printf 'fixture() { local path=%s name="$1"; printf "%%s\\n" "$path" "$name"; }\n' "$initializer" > "$library"
    expect_failure 'path expands name in the same declaration'
  done
  cp split "$library"

  printf '#!/usr/bin/env -S bash -e\n#\tshellcheck disable="SC2000-SC3000"\n' > "$entry"
  cat dependent >> "$entry"
  expect_failure '[SC2318]'
  printf '#!/usr/bin/env -S bash -e\n# shellcheck disable=all\n' > "$entry"
  cat dependent >> "$entry"
  expect_failure '[SC2318]'
  printf '#!/usr/bin/env -S bash -e\n' > "$entry"
  cat split >> "$entry"
  check
)

cd "$ROOT_DIR"
check
