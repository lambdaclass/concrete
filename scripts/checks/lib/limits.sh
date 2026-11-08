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
#     lib/limited_exec.sh. MEM_GB 0 sets no address-space cap for the gate itself: only for a gate
#     whose executables under test need an unbounded reservation (a sanitizer build's shadow
#     memory; slice0_artifacts.sh), which then caps every other command it starts through
#     lib/limited_exec.sh; it raises an inherited soft cap to the hard limit and fails (exit 2)
#     unless that leaves the address space unlimited. The caps are hard limits: no descendant can
#     raise them.
#
# Per-command time bounds are separate: bounded_run (lib/bounded.sh).
gate_limits() {
  local mem_kb=$(($1 * 1024 * 1024)) nproc=$2 cur
  if [ "$(uname -s)" = Linux ] && [ "$mem_kb" -gt 0 ]; then
    cur=$(ulimit -v)
    if [ "$cur" = unlimited ] || [ "$cur" -gt "$mem_kb" ]; then ulimit -v "$mem_kb"; fi
  elif [ "$(uname -s)" = Linux ]; then
    ulimit -S -v "$(ulimit -H -v)"
    [ "$(ulimit -S -v)" = unlimited ] ||
      { echo "gate_limits: an unlimited address space is required, the hard limit is $(ulimit -H -v) KiB" >&2; exit 2; }
  fi
  cur=$(ulimit -u)
  if [ "$cur" = unlimited ] || [ "$cur" -gt "$nproc" ]; then ulimit -u "$nproc"; fi
}
