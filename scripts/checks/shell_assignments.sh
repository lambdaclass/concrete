#!/usr/bin/env bash
# Require one name per declaration when an initializer uses expansion.
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
  cat > passing <<'CONTROL'
fixture() {
  local name="$1"
  local path="${name}.con"
  local part="${input:offset:length}"
  local result="$(name=child; printf '%s' "$name")"
  local arithmetic="$((name + 1))"
  local -a items=("$name" "$(printf '%s' "$path")")
  local plain='literal' quoted='${name}.con' escaped=\$name bare
  declare -x -- single="$name"
  typeset -'x' quoted_option="$name"
  local - option_state="$name"
  export "$name" PATH
  export "$name=literal" PATH
  export "${name:-fallback=literal}" PATH
}
CONTROL
  cat > failing <<'CONTROL'
fixture() {
  local name="$1" \
    path="${name}.con"
}
CONTROL
  library=scripts/nested/library.sh
  entry=.githooks/entry
  cp passing "$library"
  printf '#!/usr/bin/env -S bash -e\n' > "$entry"
  cat passing >> "$entry"
  git add -- "$library" "$entry"
  cp failing scripts/untracked.sh
  check

  expect_failure() {
    local expected="$1"
    local status="${2:-1}"
    local out
    local rc=0
    out="$(check 2>&1)" || rc=$?
    if [ "$rc" -ne "$status" ] || [[ "$out" != *"$expected"* ]]; then
      printf 'shell_assignments: expected %s (exit %s, got %s)\n%s\n' "$expected" "$status" "$rc" "$out" >&2
      exit 1
    fi
  }

  diagnostic='expansion in a multi-name declaration'
  cp failing "$library"
  expect_failure "$library:2:3: $diagnostic"
  printf '\nif true; then\n  true\n' >> "$library"
  expect_failure "$library: shfmt:" 2

  while IFS= read -r declaration; do
    printf 'fixture() { %s; }\n' "$declaration" > "$library"
    expect_failure "$diagnostic"
  done <<'CONTROL'
local part="${input:offset}" offset=1
local part="${input:0:length}" length=1
local result="$(name=child; printf '%s' "$name")" name=outer
local result="$(printf '%s' "$name")" name=outer
declare -i first="$((value + 1))" second
typeset first="${unrelated}" second=literal
readonly first="`printf literal`" second=literal
export -- first=literal second="$unrelated"
local -a first=("$unrelated") second
local "first=$unrelated" second
export "$name=$value" PATH
CONTROL
  cp passing "$library"

  printf '#!/usr/bin/env -S bash -e\n# shellcheck disable=all\n' > "$entry"
  cat failing >> "$entry"
  expect_failure "$entry:4:3: $diagnostic"
  printf '#!/usr/bin/env -S bash -e\n' > "$entry"
  cat passing >> "$entry"
  check
)

cd "$ROOT_DIR"
check
