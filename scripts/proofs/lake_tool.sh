# Sourced by external.sh and the toolchain-selection regression control.
source "$(dirname "${BASH_SOURCE[0]}")/../checks/lib/git_sandbox.sh" || return 2
git_sandbox || return 2
# Verify the revision's declaration even on a cache hit; never rewrite lean-toolchain.
verify_toolchain() {
  local dir=$1 rev=$2 tc=$3 declared worktree
  declared=$(git -C "$dir" show "$rev:lean-toolchain") || return 1
  worktree=$(cat "$dir/lean-toolchain") || return 1
  [ "$declared" = "$tc" ] && [ "$worktree" = "$tc" ] || {
    echo "error: toolchain mismatch at $rev: pin=$tc declared=$declared worktree=$worktree" >&2
    return 1
  }
}
build_lake_tool() {
  local name=$1 url=$2 rev=$3 tc=$4 target=$5
  local dir="$TOOLS/$name-$rev-$(printf '%s' "$tc" | tr -c 'A-Za-z0-9._-' '_')"
  if [ ! -f "$dir/.built" ]; then
    local work="$dir.partial.$$"
    rm -rf "$work"
    git clone -q "$url" "$work" >&2 && git -C "$work" checkout -q "$rev" >&2 \
      || { rm -rf "$work"; return 1; }
    verify_toolchain "$work" "$rev" "$tc" \
      || { rm -rf "$work"; return 1; }
    local elan="${CONCRETE_EXT_ELAN:-elan}"
    command -v "$elan" >/dev/null || { echo "error: external Lean tools need elan" >&2; rm -rf "$work"; return 1; }
    publish "$dir" env -u ELAN_TOOLCHAIN -u LEAN_PATH ELAN_HOME="$TOOLS/elan" "$elan" run --install "$tc" lake build "$target" \
      || { rm -rf "$work"; return 1; }
  fi
  [ "$(git -C "$dir" rev-parse HEAD)" = "$rev" ] && verify_toolchain "$dir" "$rev" "$tc" || return 1
  echo "$dir"
}
