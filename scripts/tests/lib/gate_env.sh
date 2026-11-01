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

# THE PER-PROCESS MEMORY CAP every gate runs under.
#
# On 2026-10-26 one compiler run grew to 260 GB and exhausted the dev server: a pre-fix compiler
# (bug 286, whose lowering memory grows with the square of nesting depth) on a 10^5-deep probe. So
# every gate sets a SOFT address-space limit (RLIMIT_AS, `ulimit -S -v`) that each process it starts
# inherits, and a runaway compiler, interpreter or clang run stops with an out-of-memory error
# instead of taking the machine down. The compiler peaks under 1 GB on the 10^5-element scaling
# shapes, and valgrind memcheck runs under the default.
#
#   CONCRETE_MEM_CAP_GB=N   the soft cap in GiB, a decimal integer 1 - 4096 (default 32)
#   CONCRETE_MEM_CAP_GB=0   no soft cap for this gate (the soft limit is raised to the hard limit)
#
# Each gate sets the soft limit to its own value when it sources this file, so a gate that needs
# more (a measurement, a tool that reserves address space) sets the variable before it sources. The
# hard limit is the caller's policy and always holds: a cap above it is refused with a warning, and
# the soft limit is left at the hard limit. Linux only, by policy: elsewhere nothing is set.
# A limit on address space is not a limit on resident memory, and it is per process; concurrent
# processes are bounded by the host's cgroup limits (the dev server's user slice), not by this.

mem_cap_apply() {
  local gb="${CONCRETE_MEM_CAP_GB:-32}" hard want
  [ "$(uname -s)" = Linux ] || return 0
  if ! [[ "$gb" =~ ^(0|[1-9][0-9]{0,3})$ ]] || [ "$gb" -gt 4096 ]; then
    echo "error: CONCRETE_MEM_CAP_GB must be a decimal integer 0 - 4096 (got '$gb')" >&2
    return 2
  fi
  if ! hard="$(ulimit -H -v 2>/dev/null)" || ! [[ "$hard" =~ ^(unlimited|[0-9]+)$ ]]; then
    echo "warn: cannot read the address-space limit; no memory cap set" >&2
    return 0
  fi
  if [ "$gb" = 0 ]; then want="$hard"; else want=$((gb * 1024 * 1024)); fi
  if [ "$hard" != unlimited ] && [ "$want" != unlimited ] && [ "$want" -gt "$hard" ]; then
    echo "warn: memory cap ${gb} GiB is above the hard limit (${hard} KiB); keeping the hard limit" >&2
    want="$hard"
  fi
  ulimit -S -v "$want" 2>/dev/null || echo "warn: could not set the memory cap (${want} KiB); this gate runs without it" >&2
  export CONCRETE_MEM_CAP_GB="$gb"
}
mem_cap_apply || exit 2

# Every gate matches text with has_match, never a pipe into grep -q (bug 283).
source "$(dirname "${BASH_SOURCE[0]}")/has_match.sh" || exit 2
