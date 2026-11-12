# Sourced by external.sh: self-contained tool recipes; callers hold the cache lock.
source "$(dirname "${BASH_SOURCE[0]}")/../checks/lib/git_sandbox.sh" || exit 2
git_sandbox || exit 2
# All recipe content (including the nanoda patch) participates in every tree key.
recipe_identity() { sha256sum "${BASH_SOURCE[0]}" | cut -d' ' -f1; }
elan_identity() {
  local elan=$1 resolved version
  resolved=$(realpath "$elan") || return 1
  version=$("$elan" --version) || return 1
  printf '%s\n%s\n' "$resolved" "$version" | sha256sum | cut -d' ' -f1
}
publish() {
  local dir=$1 work="$1.partial.$$"; shift
  (cd "$work" && "$@" >&2) && touch "$work/.built" && rm -rf "$dir" && mv "$work" "$dir"
}
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
  local tools=$1 name=$2 url=$3 rev=$4 tc=$5 target=$6
  local elan identity recipe
  elan=$(command -v "${CONCRETE_EXT_ELAN:-elan}") || return 1
  identity=$(elan_identity "$elan") || return 1
  recipe=$(recipe_identity) || return 1
  local dir="$tools/$name-$rev-$identity-$recipe-$(printf '%s' "$tc" | tr -c 'A-Za-z0-9._-' '_')"
  if [ ! -f "$dir/.built" ]; then
    local work="$dir.partial.$$"
    rm -rf "$work"
    git clone -q "$url" "$work" >&2 && git -C "$work" checkout -q "$rev" >&2 \
      || { rm -rf "$work"; return 1; }
    verify_toolchain "$work" "$rev" "$tc" \
      || { rm -rf "$work"; return 1; }
    publish "$dir" env -u ELAN_TOOLCHAIN -u LEAN_PATH ELAN_HOME="$tools/elan-$identity" "$elan" run --install "$tc" lake build "$target" \
      || { rm -rf "$work"; return 1; }
  fi
  [ "$(git -C "$dir" rev-parse HEAD)" = "$rev" ] && verify_toolchain "$dir" "$rev" "$tc" || return 1
  echo "$dir"
}

build_cargo_tool() {
  local tools=$1 url=$2 rev=$3 cargo rustc identity recipe dir work
  cargo=$(command -v cargo) || return 1
  rustc=$(command -v rustc) || return 1
  local version cargo_path rustc_path
  cargo_path=$(realpath "$cargo") && rustc_path=$(realpath "$rustc") || return 1
  # Invoke the selected paths: rustup proxies depend on their invocation basename.
  version=$("$rustc" -vV) || return 1
  identity=$(printf '%s\n%s\n%s\n' "$cargo_path" "$rustc_path" "$version" | sha256sum | cut -d' ' -f1)
  recipe=$(recipe_identity) || return 1
  dir="$tools/nanoda-$rev-$identity-$recipe"
  if [ ! -f "$dir/.built" ]; then
    work="$dir.partial.$$"
    rm -rf "$work"
    # Raise the thread stack from 16 MiB to 256 MiB, as the Lean Kernel Arena does.
    git clone -q "$url" "$work" >&2 && git -C "$work" checkout -q "$rev" >&2 \
      && grep -qF 'STACK_SIZE: usize = 16_777_216;' "$work/src/lib.rs" \
      && sed -i.orig 's/STACK_SIZE: usize = 16_777_216;/STACK_SIZE: usize = 268_435_456;/' "$work/src/lib.rs" \
      && publish "$dir" env RUSTC="$rustc" "$cargo" build --release -q \
      || { rm -rf "$work"; return 1; }
  fi
  [ -x "$dir/target/release/nanoda_bin" ] && [ "$(git -C "$dir" rev-parse HEAD)" = "$rev" ] || return 1
  echo "$dir"
}
