#!/usr/bin/env bash
# Publish the oracle artifact as a GitHub release asset (P13; owner approval required before the
# first run: it creates a public release). Authenticated by the caller's `gh` login.
#
#   publish.sh <artifact.tar.gz>
#
# Refuses unless the file's sha256 is the lock's artifact_sha256 (so only an artifact build.sh
# reproduced can be published) and its name is the lock's asset name. The release named by the lock's
# release_tag on the lock's release_repo is created with its tag AT THE LOCK'S commit (--target), never
# at the default branch's head; if the release already exists, its tag must resolve to that commit or
# publishing is refused. The asset is uploaded unless the release already holds it. Either way the
# published asset must download back to artifact_sha256. The lock's artifact_url is the result;
# commit it with the lock. Rerunning on a published release is safe (it only re-verifies).
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=scripts/oracle/lib.sh
. "$HERE/lib.sh"
die() { echo "publish: $*" >&2; exit 1; }
[ $# = 1 ] && [ -f "$1" ] || { echo "usage: publish.sh <artifact.tar.gz>" >&2; exit 2; }
WANT="$(lock artifact_sha256)"; GOT="$(sha256sum "$1" | cut -d' ' -f1)"
[ ${#WANT} = 64 ] || die "the lock has no artifact_sha256"
[ "$GOT" = "$WANT" ] || die "$1 is $GOT, the lock says $WANT"
REPO="$(lock release_repo)"; RTAG="$(lock release_tag)"; NAME="$(oracle_artifact_name)"
COMMIT="$(lock commit)"
[ -n "$REPO" ] && [ -n "$RTAG" ] || die "the lock has no release_repo or release_tag"
[[ "$COMMIT" =~ ^[0-9a-f]{40}$ ]] || die "the lock's commit '$COMMIT' is not a full commit id"
[ "$(basename "$1")" = "$NAME" ] || die "the asset must be named $NAME"
if gh release view "$RTAG" -R "$REPO" >/dev/null 2>&1; then
  # The release exists: its tag must already name the locked commit.
  AT="$(gh api "repos/$REPO/commits/tags/$RTAG" --jq .sha 2>/dev/null)" || die "cannot resolve tag $RTAG on $REPO"
  [ "$AT" = "$COMMIT" ] || die "release $RTAG on $REPO is at commit $AT, the lock says $COMMIT"
else
  gh release create "$RTAG" -R "$REPO" --target "$COMMIT" --title "Legacy oracle $(lock tag)" \
    --notes "The frozen legacy compiler (tag $(lock tag), commit $COMMIT) as the differential oracle. sha256 $WANT. Rebuild and verify: scripts/oracle/build.sh."
  AT="$(gh api "repos/$REPO/commits/tags/$RTAG" --jq .sha 2>/dev/null)" || die "cannot resolve the new tag $RTAG on $REPO"
  [ "$AT" = "$COMMIT" ] || die "the new release $RTAG on $REPO is at commit $AT, not $COMMIT"
fi
# The asset names, one per line; matched as a whole line (no pipe into a short-circuiting grep).
ASSETS="$(gh release view "$RTAG" -R "$REPO" --json assets --jq '.assets[].name')" || die "cannot list the assets of $RTAG on $REPO"
if case $'\n'"$ASSETS"$'\n' in *$'\n'"$NAME"$'\n'*) true ;; *) false ;; esac; then
  echo "publish: $RTAG already holds $NAME; verifying it"
else
  gh release upload "$RTAG" "$1" -R "$REPO"
fi
URL="https://github.com/$REPO/releases/download/$RTAG/$NAME"
TMPF="$(mktemp)"; trap 'rm -f "$TMPF"' EXIT
curl -fsSL --proto '=https' -o "$TMPF" "$URL" || die "cannot download $URL"
[ "$(sha256sum "$TMPF" | cut -d' ' -f1)" = "$WANT" ] || die "the published asset does not download to $WANT"
echo "publish: $URL verified; set artifact_url=$URL in ORACLE.lock"
