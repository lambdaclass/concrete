#!/usr/bin/env bash
# The gate environment guard. Every gate sources this first:
#
#   source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
#
# Gates run in three places: the dev shell, Linux CI runners and the macOS CI runner. When each took
# coreutils, sed, grep, python and the rest from whatever host it landed on, a gate could pass on one
# and fail on another for reasons unrelated to what it tests: macOS has no `timeout`, and BSD `sed -i`,
# `stat`, `date` and `readlink` take other flags. flake.nix now defines that userland once (the
# `gates` shell), CI runs every step inside it, and the dev shells include it. Each of those shells
# exports CONCRETE_GATES_SHELL=1; a gate started anywhere else refuses here, before it can produce a
# verdict about the wrong environment.
#
# Use `nix develop .#gates` (no Lean; what CI uses, with elan's Lean on PATH) or `nix develop` (the
# dev shell, with its own Lean). An entry point that must start outside both, like the pre-push
# hook, re-executes itself inside `nix develop` before it runs a gate.
#
# check_gate_hygiene.sh proves every gate sources this file.

if [ "${CONCRETE_GATES_SHELL:-}" != 1 ]; then
  echo "error: ${0##*/} refuses to run outside the gate shell: run inside nix develop .#gates (CI) or nix develop" >&2
  exit 2
fi

# Every gate matches text with has_match, never a pipe into grep -q (bug 283).
source "$(dirname "${BASH_SOURCE[0]}")/has_match.sh" || exit 2
