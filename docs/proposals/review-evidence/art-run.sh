#!/usr/bin/env bash
# Run the artifact gate then the backends gate in ~/slice0 (synced working tree), bounded.
cd ~/slice0
rm -rf "${HOME:?}/art-report"; mkdir -p ~/art-report
nix develop --command bash -c '
ulimit -u 40000
timeout -k 60 3000 bash scripts/checks/slice0_artifacts.sh ~/art-report > ~/art.log 2>&1; echo "artifacts_rc=$?" >> ~/art.log
timeout -k 60 2400 bash scripts/checks/slice0_backends.sh > ~/be.log 2>&1; echo "backends_rc=$?" >> ~/be.log'
echo "ok lines: $(grep -c '^ok' ~/art.log)"; grep -v '^ok' ~/art.log | head -60; tail -2 ~/be.log
