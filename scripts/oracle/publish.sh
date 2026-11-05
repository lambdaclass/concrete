#!/usr/bin/env bash
# Publish the oracle artifact as a GitHub release asset (P13; owner approval required before the
# first run: it creates a public release). Authenticated by the caller's `gh` login.
#
#   publish.sh <artifact.tar.gz>
#
# Refuses unless the file's sha256 is the lock's artifact_sha256 (so only an artifact build.sh
# reproduced can be published), then uploads it to the release named by the lock's release_tag on
# the lock's release_repo, and checks that the published asset downloads back to the same digest.
# The lock's artifact_url is the result; commit it with the lock.
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=scripts/oracle/lib.sh
. "$HERE/lib.sh"
[ $# = 1 ] && [ -f "$1" ] || { echo "usage: publish.sh <artifact.tar.gz>" >&2; exit 2; }
WANT="$(lock artifact_sha256)"; GOT="$(sha256sum "$1" | cut -d' ' -f1)"
[ "$GOT" = "$WANT" ] || { echo "publish: $1 is $GOT, the lock says $WANT" >&2; exit 1; }
REPO="$(lock release_repo)"; RTAG="$(lock release_tag)"; NAME="$(oracle_artifact_name)"
[ "$(basename "$1")" = "$NAME" ] || { echo "publish: the asset must be named $NAME" >&2; exit 1; }
gh release view "$RTAG" -R "$REPO" >/dev/null 2>&1 \
  || gh release create "$RTAG" -R "$REPO" --target "$(lock commit)" --title "Legacy oracle $(lock tag)" \
       --notes "The frozen legacy compiler (tag $(lock tag), commit $(lock commit)) as the differential oracle. sha256 $WANT. Rebuild and verify: scripts/oracle/build.sh."
gh release upload "$RTAG" "$1" -R "$REPO"
URL="https://github.com/$REPO/releases/download/$RTAG/$NAME"
TMPF="$(mktemp)"; trap 'rm -f "$TMPF"' EXIT
curl -fsSL --proto '=https' -o "$TMPF" "$URL"
[ "$(sha256sum "$TMPF" | cut -d' ' -f1)" = "$WANT" ] || { echo "publish: the published asset does not download to $WANT" >&2; exit 1; }
echo "publish: $URL verified; set artifact_url=$URL in ORACLE.lock"
