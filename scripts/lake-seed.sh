#!/usr/bin/env bash
# Seed this checkout's empty `.lake` from another checkout's build, then let `lake build` verify it
# (R-0546). A new worktree otherwise builds all 242 Lean jobs from scratch: measured 52 s wall /
# 255 s CPU on an 18-core laptop at 25eb35fd, against 0.2 s to clone the seed plus 0.6 s for the
# no-op `lake build` that checks it.
#
# WHY A SEED CANNOT GO STALE: Lake decides what to rebuild from CONTENT HASHES recorded in each
# artifact's .trace file, not from mtimes. Measured: a seed built at 25eb35fd was copied into a
# checkout of 8f40d45b and every seeded file was touched to be NEWER than the sources; `lake build`
# still rebuilt the 146 modules whose sources differ, and the resulting binary embeds 8f40d45b's
# BuildIdentity (ee30a7dc...), not the seed's (5a0afcdd...). So a seed from any commit, or from a
# tree with uncommitted edits, only saves work; it never substitutes a stale artifact. The
# `lake build` below is therefore mandatory, not an optimisation, and this script always runs it.
#
# The copy is copy-on-write where the filesystem supports it (APFS `cp -c`, Linux `--reflink`), so
# the seed takes no extra disk until a file is rebuilt, and the source checkout is never written.
#
# Usage: (in the checkout to seed) bash scripts/lake-seed.sh [SOURCE_CHECKOUT]
#   SOURCE_CHECKOUT defaults to the repository's primary checkout (the parent of the common git dir).
#   Refuses when this checkout already has a .lake (it never overwrites a build).
set -uo pipefail
# The checkout to seed is the one the caller stands in (worktree-new.sh runs this script from the
# parent checkout's copy inside the new worktree, whose base may predate the script).
ROOT_DIR="$(git rev-parse --show-toplevel 2>/dev/null)" || { echo "lake-seed: not inside a checkout" >&2; exit 2; }
cd "$ROOT_DIR"

SRC="${1:-}"
if [ -z "$SRC" ]; then
  common="$(git rev-parse --path-format=absolute --git-common-dir 2>/dev/null)" || common=""
  [ -n "$common" ] && SRC="$(dirname "$common")"
fi
[ -n "$SRC" ] && [ -d "$SRC/.lake/build" ] || { echo "lake-seed: no seed build at ${SRC:-<none>}/.lake/build" >&2; exit 2; }
SRC="$(cd "$SRC" && pwd)"
[ "$SRC" != "$ROOT_DIR" ] || { echo "lake-seed: the seed is this checkout" >&2; exit 2; }
if [ -e "$ROOT_DIR/.lake" ]; then
  echo "lake-seed: $ROOT_DIR/.lake already exists; refusing to overwrite a build." >&2
  exit 2
fi
# The toolchain must match, or every artifact is rebuilt anyway (still correct, just no gain).
if ! cmp -s "$SRC/lean-toolchain" "$ROOT_DIR/lean-toolchain"; then
  echo "lake-seed: warning: lean-toolchain differs from the seed's; expect a full rebuild." >&2
fi

TMP_LAKE="$ROOT_DIR/.lake.seed.$$"
t0=$(date +%s)
if cp -Rc "$SRC/.lake" "$TMP_LAKE" 2>/dev/null \
   || cp -R --reflink=auto "$SRC/.lake" "$TMP_LAKE" 2>/dev/null \
   || cp -R "$SRC/.lake" "$TMP_LAKE"; then
  mv "$TMP_LAKE" "$ROOT_DIR/.lake"
else
  rm -rf "$TMP_LAKE"; echo "lake-seed: copy failed" >&2; exit 1
fi
echo "lake-seed: seeded .lake from $SRC in $(( $(date +%s) - t0 ))s; verifying with lake build"
# Verification is the build. Lake rebuilds every artifact whose recorded input hashes differ.
if command -v lake >/dev/null 2>&1; then
  lake build
elif command -v nix >/dev/null 2>&1; then
  nix develop -c lake build
else
  echo "lake-seed: no lake on PATH; run 'lake build' before using the binary." >&2; exit 1
fi
