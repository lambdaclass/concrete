#!/usr/bin/env bash
# Resource limits for a gate and everything it starts (sourced; the limits are inherited).
#
#   gate_limits MEM_GB NPROC
#     Caps the address space of each process at MEM_GB GiB (Linux; Darwin does not enforce
#     RLIMIT_AS, so there the CI job's memory is the bound) and the number of processes of this
#     user at NPROC, so a runaway fork loop stops at the cap instead of taking the machine. Lowering
#     a limit is always allowed; a limit already below the requested one is kept. Lake reserves
#     far more address space than it uses (a cold `lake build` fails to create threads under
#     32 GiB), so gates pass 64 here and bound the executables under test tightly, through
#     lib/limited_exec.sh. The address-space cap is the SOFT limit: it binds every process, and
#     only limited_exec.sh 0 (a sanitizer build, whose shadow reservation needs terabytes of
#     address space; slice0_artifacts.sh) raises it back to the hard limit for one executable.
#
# Per-command time bounds are separate: bounded_run (lib/bounded.sh) for executables under test,
# `timeout -k` for builds.
gate_limits() {
  local mem_kb=$(($1 * 1024 * 1024)) nproc=$2 cur
  if [ "$(uname -s)" = Linux ]; then
    cur=$(ulimit -S -v)
    if [ "$cur" = unlimited ] || [ "$cur" -gt "$mem_kb" ]; then ulimit -S -v "$mem_kb"; fi
  fi
  cur=$(ulimit -u)
  if [ "$cur" = unlimited ] || [ "$cur" -gt "$nproc" ]; then ulimit -u "$nproc"; fi
}
