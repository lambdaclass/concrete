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
  # Preserve the proxy name, but make invocation independent of publish's cwd.
  elan=$(realpath -s "$elan") || return 1
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
  local tools=$1 url=$2 rev=$3 cargo rustc identity recipe dir work source home ancestor
  local -a cargo_env=(env -i "PATH=$PATH" "HOME=$HOME")
  # Rustup proxies need their selected installation, but no ambient Cargo/Rust overrides.
  [ -z "${RUSTUP_HOME+x}" ] || cargo_env+=("RUSTUP_HOME=$RUSTUP_HOME")
  [ -z "${RUSTUP_TOOLCHAIN+x}" ] || cargo_env+=("RUSTUP_TOOLCHAIN=$RUSTUP_TOOLCHAIN")
  # Keep the pinned checkout locally: a cache hit verifies HEAD without network access.
  source="$tools/source-nanoda-$rev"
  if [ ! -d "$source" ]; then
    work=$(mktemp -d "$tools/nanoda-source.XXXXXX") || return 1
    git clone -q "$url" "$work" >&2 && git -C "$work" checkout -q "$rev" >&2 \
      && mv "$work" "$source" || { rm -rf "$work"; return 1; }
  fi
  [ "$(git -C "$source" rev-parse HEAD)" = "$rev" ] && [ -z "$(git -C "$source" status --porcelain)" ] || { echo "error: cached nanoda source revision or cleanliness mismatch: $source" >&2; return 1; }
  work=$source
  local version cargo_path rustc_path
  cargo=$(cd "$work" && command -v cargo) && rustc=$(cd "$work" && command -v rustc) \
    && cargo_path=$(cd "$work" && realpath "$cargo") \
    && rustc_path=$(cd "$work" && realpath "$rustc") \
    && version=$(cd "$work" && "$rustc" -vV) \
    && recipe=$(recipe_identity) || return 1
  # Keep proxy invocation names (cargo/rustc), rather than invoking their rustup realpaths.
  identity=$(printf '%s\n%s\n%s\n' "$cargo_path" "$rustc_path" "$version" | sha256sum | cut -d' ' -f1)
  dir="$tools/nanoda-$rev-$identity-$recipe"
  if [ ! -f "$dir/.built" ]; then
    # Build outside HOME so the usual ~/.cargo configuration is not an ancestor.
    work=$(mktemp -d /tmp/concrete-nanoda.XXXXXX) || return 1
    cp -a "$source/." "$work/" || { rm -rf "$work"; return 1; }
    # Cargo searches every ancestor, independently of CARGO_HOME. Fail closed.
    ancestor=$(realpath "$work") || { rm -rf "$work"; return 1; }
    while :; do
      if [ -e "$ancestor/.cargo/config" ] || [ -e "$ancestor/.cargo/config.toml" ]; then
        echo "error: refusing nanoda build: Cargo configuration in $ancestor/.cargo" >&2
        rm -rf "$work"; return 1
      fi
      [ "$ancestor" != / ] || break
      ancestor=$(dirname "$ancestor")
    done
    # Re-measure in the actual build directory, where rustup selects its compiler.
    [ "$(cd "$work" && "$rustc" -vV)" = "$version" ] || { echo "error: Rust measured outside build directory or compiler identity changed" >&2; rm -rf "$work"; return 1; }
    home=$(mktemp -d) || { rm -rf "$work"; return 1; }
    # Retain downloads by builder identity; no ambient Cargo home configuration is read.
    mkdir -p "$tools/registry-$identity"
    ln -s "$tools/registry-$identity" "$home/registry"
    # Raise the thread stack from 16 MiB to 256 MiB, as the Lean Kernel Arena does.
    grep -qF 'STACK_SIZE: usize = 16_777_216;' "$work/src/lib.rs" \
      && sed -i.orig 's/STACK_SIZE: usize = 16_777_216;/STACK_SIZE: usize = 268_435_456;/' "$work/src/lib.rs" \
      && (cd "$work" && "${cargo_env[@]}" CARGO_HOME="$home" RUSTC="$rustc" "$cargo" build --release --locked -q >&2) \
      && printf '%s\n' "$rev" > "$work/.source-rev" && touch "$work/.built" && rm -rf "$dir" && mv "$work" "$dir.partial.$$" && mv "$dir.partial.$$" "$dir" \
      || { rm -rf "$work" "$dir.partial.$$" "$home"; return 1; }
    rm -rf "$home"
  fi
  [ -x "$dir/target/release/nanoda_bin" ] && [ "$(cat "$dir/.source-rev")" = "$rev" ] && [ "$(git -C "$dir" rev-parse HEAD)" = "$rev" ] || return 1
  echo "$dir"
}
