#!/usr/bin/env bash
# gx-run.sh NAME SHA BRANCH CMD: fresh clone of wip/BRANCH at SHA in ~/gx/NAME, warm .lake from
# ~/gates-test (copied, never shared), then CMD inside nix develop. Ends with a GX-DONE line.
set -uo pipefail
name=$1 sha=$2 branch=$3 cmd=$4
T=$HOME/gx/$name
rm -rf "${T:?}"; mkdir -p "$HOME/gx"
git clone -q --no-checkout --reference "$HOME/concrete2" https://github.com/unbalancedparentheses/concrete2.git "$T" || { echo "GX-DONE rc=clone-failed"; exit 1; }
cd "$T"
git fetch -q origin "wip/$branch" || { echo "GX-DONE rc=fetch-failed"; exit 1; }
git checkout -q "$sha" || { echo "GX-DONE rc=checkout-failed"; exit 1; }
echo "head=$(git rev-parse HEAD) host=$(hostname) start=$(date -u +%FT%TZ)"
if [ -d "$HOME/gates-test/.lake/packages" ]; then mkdir -p .lake && cp -a "$HOME/gates-test/.lake/packages" .lake/ ; fi
ulimit -u 40000
nix develop --command bash -c "$cmd"
rc=$?
echo "GX-DONE rc=$rc end=$(date -u +%FT%TZ)"
