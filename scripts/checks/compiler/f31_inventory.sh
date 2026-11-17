#!/usr/bin/env bash
# F31 migration qualification in the CI spec job: build both libraries, run the census tools
# and reject live-inventory drift.
# Retire this gate, the tools and the summary together when F31 closes.
set -euo pipefail
source scripts/checks/lib/gate_env.sh || exit 2
source scripts/checks/lib/bounded.sh
bounded_run 1200 -- lake build ConcreteSpec Concrete
bounded_run 1200 -- python3 -I scripts/spec/f31_inventory.py
