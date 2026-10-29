#!/usr/bin/env bash
# Build the site's browser compiler from this checkout: site/static/compiler/concrete.mjs,
# concrete.wasm and manifest.json. These are build outputs, not sources: they are gitignored, and
# the site deployment (.github/workflows/book.yml) runs this script before Zola. A Zola build
# without them still works; the explorer then reports that the compiler is unavailable.
#
# Steps (each skipped when its pinned output is already in the cache):
#   1. the Lean source checkout matching lean-toolchain, with scripts/browser/lean-runtime.patch;
#   2. Emscripten $EMSDK_VERSION;
#   3. the single-threaded Lean runtime built with Emscripten (leanrt);
#   4. `lake build concrete-browser-native` (the generated C of the compiler);
#   5. scripts/browser/build-wasm.py, then the native/Wasm parity, loader and UI tests.
#
# Environment: BROWSER_CACHE (default .cache/browser), JOBS (default 4).
# Requires: the pinned Lean toolchain (elan or nix), git, cmake, python3, node.
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT"
EMSDK_VERSION=4.0.23
JOBS="${JOBS:-4}"
CACHE="${BROWSER_CACHE:-$ROOT/.cache/browser}"
LEAN_TAG="$(sed 's/.*://' lean-toolchain)"            # e.g. v4.34.1
PATCH="$ROOT/scripts/browser/lean-runtime.patch"
PATCH_ID="$(sha256sum "$PATCH" | cut -c1-12)"
LEAN_SRC="$CACHE/lean-$LEAN_TAG-$PATCH_ID"
EMSDK="$CACHE/emsdk-$EMSDK_VERSION"
RUNTIME="$CACHE/runtime-$LEAN_TAG-$EMSDK_VERSION-$PATCH_ID"
mkdir -p "$CACHE"

step() { echo "== $*"; }

if [ ! -f "$LEAN_SRC/.patched" ]; then
  step "Lean source $LEAN_TAG with the runtime patch"
  rm -rf "$LEAN_SRC"
  git clone -q --depth 1 --branch "$LEAN_TAG" https://github.com/leanprover/lean4 "$LEAN_SRC"
  git -C "$LEAN_SRC" apply --unidiff-zero "$PATCH"
  touch "$LEAN_SRC/.patched"
fi

if [ ! -f "$EMSDK/.activated" ]; then
  step "Emscripten $EMSDK_VERSION"
  rm -rf "$EMSDK"
  git clone -q https://github.com/emscripten-core/emsdk "$EMSDK"
  "$EMSDK/emsdk" install "$EMSDK_VERSION"
  "$EMSDK/emsdk" activate "$EMSDK_VERSION"
  touch "$EMSDK/.activated"
fi

if [ ! -f "$RUNTIME/lib/lean/libleanrt.a" ]; then
  step "Lean runtime (leanrt) for Emscripten"
  rm -rf "$RUNTIME"
  "$EMSDK/upstream/emscripten/emcmake" cmake \
    -S "$LEAN_SRC/src" -B "$RUNTIME" \
    -DSTAGE=0 -DCMAKE_BUILD_TYPE=Release -DUSE_GMP=OFF -DUSE_MIMALLOC=OFF \
    -DMULTI_THREAD=OFF -DAUTO_THREAD_FINALIZATION=OFF \
    -DLEAN_VERSION_PATCH=1 -DUSE_GITHASH=OFF
  cmake --build "$RUNTIME" --target leanrt -j "$JOBS"
fi

step "native browser root (generated C)"
lake build concrete-browser-native

step "Wasm cross-compilation"
python3 scripts/browser/build-wasm.py --lean-source "$LEAN_SRC" --runtime "$RUNTIME" \
  --emsdk "$EMSDK" --jobs "$JOBS"

step "native/Wasm parity, loader and UI tests"
node scripts/browser/test-parity.mjs
node scripts/browser/test-loader.mjs
node scripts/browser/test-ui.mjs
echo "browser compiler built: site/static/compiler/{concrete.mjs,concrete.wasm,manifest.json}"
