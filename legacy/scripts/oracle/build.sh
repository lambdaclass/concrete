#!/usr/bin/env bash
# Rebuild the legacy oracle (ROADMAP R-0564) from its tag and check it against scripts/oracle/ORACLE.lock.
#
# The oracle is the old compiler, frozen at the tag the lock names. It is built from a FRESH clone of
# a public remote, never from this checkout or from legacy/, so nothing on main and no particular
# host (concrete-ci-2 caches it, but is not required) can change what it is. Each input is checked
# before the build: the tag resolves to the locked commit, the Lean toolchain and flake.lock are the
# locked ones, and the committed build identity is fresh. The binary's sha256 is then compared with
# the lock. A mismatch is a failure: either the build is not reproducible or something changed.
#
#   build.sh            rebuild, verify every locked value, pack the artifact (scripts/oracle/lib.sh),
#                       verify its sha256 too, and install it into $ORACLE_HOME/<commit>/
#   build.sh --record   rebuild and print the measured values in lock syntax (no verification of
#                       binary_sha256 or artifact_sha256); used once to write the lock
#
# The rebuild is the VERIFICATION of the published artifact (fetch.sh is how runners get it): both
# hashes reproduced from a fresh clone of the tag mean the release asset is what the tag builds.
#
# ORACLE_SOURCE   clone source (default: the lock's url, then mirror_url); a path or bundle also works
# ORACLE_HOME     install root (default ~/oracle); the install is <commit>/ (the unpacked artifact)
#                 and <commit>.BUILD_INFO beside it
# ORACLE_KEEP=1   keep a failed build's directory for inspection
# ORACLE_ARTIFACT_OUT=PATH  also copy the packed artifact there (for publish.sh)
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=scripts/oracle/lib.sh
. "$HERE/lib.sh"
LOCK="$ORACLE_LOCK"
RECORD=0
[ "${1:-}" = "--record" ] && { RECORD=1; shift; }
[ $# -eq 0 ] || { echo "usage: build.sh [--record]" >&2; exit 2; }
[ -f "$LOCK" ] || { echo "oracle: $LOCK missing" >&2; exit 2; }
die() { echo "oracle: $*" >&2; exit 1; }

TAG="$(lock tag)"; COMMIT="$(lock commit)"
[ -n "$TAG" ] && [ ${#COMMIT} = 40 ] || die "lock has no tag or a short commit"
HOME_DIR="$(oracle_home)"
WORK="$(mktemp -d "${TMPDIR:-/tmp}/oracle-build.XXXXXX")"
cleanup() { if [ "${ORACLE_KEEP:-0}" = 1 ]; then echo "oracle: kept $WORK" >&2; else rm -rf "$WORK"; fi; }
trap cleanup EXIT
SRC="$WORK/src"
NIX=(nix --extra-experimental-features "nix-command flakes")

# 1. Source: a fresh clone; the tag must resolve to the locked commit.
cloned=0
for url in ${ORACLE_SOURCE:-} "$(lock url)" "$(lock mirror_url)"; do
  [ -n "$url" ] || continue
  rm -rf "$SRC"
  if git clone --quiet --no-checkout "$url" "$SRC" 2>/dev/null \
     && git -C "$SRC" fetch --quiet origin "refs/tags/$TAG:refs/tags/$TAG" 2>/dev/null; then
    cloned=1; echo "oracle: cloned $url"; break
  fi
done
[ "$cloned" = 1 ] || die "could not clone any source with tag $TAG"
got="$(git -C "$SRC" rev-parse "refs/tags/$TAG^{commit}")"
[ "$got" = "$COMMIT" ] || die "tag $TAG is $got, the lock says $COMMIT (a moved tag is never followed)"
git -C "$SRC" -c advice.detachedHead=false checkout --quiet --detach "$COMMIT"

# 2. Inputs the lock pins.
check() { # key measured
  local want; want="$(lock "$1")"
  [ "$RECORD" = 1 ] && { echo "$1=$2"; return 0; }
  [ "$want" = "$2" ] || die "$1 is '$2', the lock says '$want'"
}
check lean_toolchain "$(tr -d '\n' < "$SRC/lean-toolchain")"
check flake_lock_sha256 "$(sha256sum "$SRC/flake.lock" | cut -d' ' -f1)"
for t in tests examples std; do check "corpus_${t}_tree" "$(git -C "$SRC" rev-parse "$COMMIT:$t")"; done

# 3. The committed build identity must already describe these sources (the binary embeds it).
( cd "$SRC" && bash scripts/gen/build_identity.sh >/dev/null )
[ -z "$(git -C "$SRC" status --porcelain)" ] || { git -C "$SRC" status --short >&2; die "the tag's committed build identity is stale"; }

# 4. Build inside the tag's own default dev shell (its flake.lock pins clang and Lean).
echo "oracle: building $TAG ($COMMIT) in $SRC"
t0=$(date +%s)
( cd "$SRC" && XDG_CACHE_HOME="$WORK/cache" "${NIX[@]}" develop "$SRC" --command lake build concrete ) > "$WORK/build.log" 2>&1 \
  || { tail -40 "$WORK/build.log" >&2; die "lake build failed"; }
BIN="$SRC/.lake/build/bin/concrete"
[ -x "$BIN" ] || die "no binary at .lake/build/bin/concrete"
toolver() { ( cd "$SRC" && XDG_CACHE_HOME="$WORK/cache" "${NIX[@]}" develop "$SRC" --command bash -c "$1" ) 2>/dev/null | head -1; }
LEANV="$(toolver 'lean --version')"; CLANGV="$(toolver 'clang --version')"
check lean_version "$LEANV"
check clang_version "$CLANGV"
SHA="$(sha256sum "$BIN" | cut -d' ' -f1)"
[ "$RECORD" = 1 ] || check binary_sha256 "$SHA"

# 5. Pack the artifact from the verified checkout: the binary, std/ exactly as committed (git
#    archive, so nothing untracked rides along) and the dev shell's flake files.
STAGE="$WORK/stage"; mkdir -p "$STAGE/bin"
cp "$BIN" "$STAGE/bin/concrete"
git -C "$SRC" archive "$COMMIT" std | tar -x -C "$STAGE"
mkdir -p "$STAGE/shell"
git -C "$SRC" archive "$COMMIT" flake.nix flake.lock lean-toolchain | tar -x -C "$STAGE/shell"
TB="$WORK/$(oracle_artifact_name)"
oracle_pack "$STAGE" "$TB"
ASHA="$(sha256sum "$TB" | cut -d' ' -f1)"
MSHA="$(sha256sum "$STAGE/MANIFEST.sha256" | cut -d' ' -f1)"
if [ "$RECORD" = 1 ]; then
  echo "binary_sha256=$SHA"; echo "manifest_sha256=$MSHA"; echo "artifact_sha256=$ASHA"; echo "# build seconds: $(( $(date +%s) - t0 ))"
  [ -z "${ORACLE_ARTIFACT_OUT:-}" ] || cp "$TB" "$ORACLE_ARTIFACT_OUT"
  exit 0
fi
check manifest_sha256 "$MSHA"
check artifact_sha256 "$ASHA"
[ -z "${ORACLE_ARTIFACT_OUT:-}" ] || cp "$TB" "$ORACLE_ARTIFACT_OUT"

# 6. Install through the same path a fetched artifact takes.
DEST="$HOME_DIR/$COMMIT"; mkdir -p "$HOME_DIR"
why="$(oracle_install "$TB" "$DEST")" || die "install: $why"
printf 'tag=%s\ncommit=%s\nbinary_sha256=%s\nartifact_sha256=%s\nsource=rebuilt from the tag\nbuilt_utc=%s\nhost=%s\nbuild_seconds=%s\n' \
  "$TAG" "$COMMIT" "$SHA" "$ASHA" "$(date -u +%Y-%m-%dT%H:%M:%SZ)" "$(hostname)" "$(( $(date +%s) - t0 ))" > "$HOME_DIR/$COMMIT.BUILD_INFO"
echo "oracle: verified binary $SHA and artifact $ASHA, installed at $DEST"
