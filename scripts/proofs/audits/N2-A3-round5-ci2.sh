#!/usr/bin/env bash
# Round 5: selected fields, recursor alternatives, typed subject carriers and dependent domains.
# Owner questions (including propext/question 8) remain pending.
# Run from the coordinator clone on concrete-ci-2 after pushing the final SHA.
set -euo pipefail
bash scripts/checks/lib/bounded.sh 960 nix develop --command \
  bash scripts/checks/lib/bounded.sh 900 bash -c '
    set -euo pipefail
    git rev-parse HEAD
    test "$(git rev-parse HEAD:scripts/proofs/vacuity.lean)" = 59b6faed3d5dd998848ce738d817a11a74729d4c
    scratch=$(mktemp -d)
    trap '\''rm -rf "$scratch"'\'' EXIT
    git archive --format=tar 6efdce6136752d51fc74265d61278d90937f24cd > "$scratch/source.tar"
    mkdir "$scratch/source"
    tar -xf "$scratch/source.tar" -C "$scratch/source"
    set +e
    python3 scripts/proofs/vacuity_driver.py --source "$scratch/source" --scope ConcreteSpec.Proofs.Grant,ConcreteSpec.Proofs.Own
    rc=$?
    printf "N2 CI2 EXIT %s\n" "$rc"
    exit "$rc"
  '
