#!/usr/bin/env bash
# Fetch the published oracle artifact and install it, verified (ROADMAP R-0564; P13).
#
#   fetch.sh    download artifact_url from the lock, check artifact_sha256, unpack into
#               $ORACLE_HOME/<commit>/ and verify the install (manifest, nothing added, binary hash)
#
# This is how a runner other than concrete-ci-2 (a GitHub-hosted fallback runner) gets the oracle.
# The digest in the lock is what authenticates the download: the bytes are trusted because their
# sha256 is the one the reviewed lock names, whatever served them. build.sh is the independent
# check that those bytes are what the tag builds.
#
# Exit status: 0 installed and verified; 3 INCOMPLETE (no published artifact, download failed, or
# verification failed). A caller reports 3 as incomplete, which blocks landing when the oracle check
# is required; it never reports it as passed and never falls back to an unverified binary.
set -uo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=scripts/oracle/lib.sh
. "$HERE/lib.sh"
incomplete() { echo "oracle: INCOMPLETE: $*" >&2; exit 3; }
URL="$(lock artifact_url)"; WANT="$(lock artifact_sha256)"
case "$URL" in https://*) ;; *) incomplete "no published artifact (artifact_url is '${URL:-empty}')";; esac
[ ${#WANT} = 64 ] || incomplete "the lock has no artifact_sha256"
D="$(oracle_home)/$(lock commit)"
if oracle_verify_install "$D" >/dev/null 2>&1; then echo "oracle: already installed and verified at $D"; exit 0; fi
mkdir -p "$(oracle_home)" || incomplete "cannot create $(oracle_home)"
TMPF="$(mktemp "$(oracle_home)/fetch.XXXXXX")" || incomplete "no temporary file"
trap 'rm -f "$TMPF"' EXIT
curl -fsSL --proto '=https' --tlsv1.2 --retry 3 -o "$TMPF" "$URL" || incomplete "download of $URL failed"
why="$(oracle_install "$TMPF" "$D")" || incomplete "$why"
echo "oracle: fetched $URL, verified $WANT, installed at $D"
