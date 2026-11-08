#!/usr/bin/env bash
# Sync the slice0 worktree's working files to concrete-ci-2:~/slice0b and run a command there in the nix shell.
# usage: ci2build.sh "<command>"
set -euo pipefail
cd /home/unbalancedparen/projects/slice0
tar czf - --exclude=.lake --exclude=.git Concrete Concrete.lean ConcreteSpec ConcreteSpec.lean lakefile.toml lake-manifest.json lean-toolchain flake.nix flake.lock scripts tests 2>/dev/null \
  | asagent "ssh concrete-ci-2 'cd ~/slice0b && tar xzf - && nix develop -c bash -lc \"$1\"'"
