#!/usr/bin/env bash
# Fresh clone at SHA (GitHub wip/slice0 + ~/repro.bundle); run the backends and repro gates,
# one after the other, each bounded. Usage: gates-run.sh SHA
set -euo pipefail
sha=$1; T=$HOME/gates-test
rm -rf "${T:?}"
git clone -q --no-checkout --shared ~/slice0 "$T"
cd "$T"
git fetch -q https://github.com/unbalancedparentheses/concrete2.git wip/slice0 || true
git fetch -q ~/repro.bundle "$sha"
git checkout -q "$sha"
echo "head=$(git rev-parse HEAD)"
ulimit -S -v 67108864; ulimit -u 40000
echo "== backends"; nix develop --command timeout -k 60 2400 bash scripts/checks/slice0_backends.sh 2>&1 | tail -3; echo "backends_rc=${PIPESTATUS[0]}"
echo "== repro"; nix develop --command timeout -k 60 5400 bash scripts/checks/slice0_repro.sh "$T/manifest.txt" 2>&1 | tail -6; echo "repro_rc=${PIPESTATUS[0]}"
echo "== artifacts"; nix develop --command timeout -k 60 3000 bash scripts/checks/slice0_artifacts.sh "$T/report" > "$T/art.log" 2>&1; echo "artifacts_rc=$?"; grep -c "^ok" "$T/art.log"; grep FAIL "$T/art.log" | head -20
