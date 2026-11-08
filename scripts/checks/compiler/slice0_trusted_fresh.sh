#!/usr/bin/env bash
# Scheduled gate 9: a private checkout with no compiler/dependency build products.
# PRs always run the real small fresh Lake compile/link control; this leg builds
# the entire Concrete/LCB pipeline from sources, with its FIRST build traced.
set -euo pipefail
export LC_ALL=C LAKE_ARTIFACT_CACHE=false
root=$(pwd)
work=$(mktemp -d)
trap 'rm -rf "${work:?}"' EXIT
source scripts/checks/lib/bounded.sh
# Hooks export repository-local Git variables; clear them before creating the clone.
source scripts/checks/lib/git_sandbox.sh
git_sandbox
bounded_run 120 -- git clone --shared --no-hardlinks "$root" "$work/source"
cd "$work/source"
export LAKE_CACHE_DIR="$work/cache"
# No .lake artifacts are copied: the initial gate build fetches pinned sources
# and traces every compilation/linking child, including dependency acquisition.
bounded_run 7200 -- bash scripts/checks/compiler/slice0_trusted_base.sh
