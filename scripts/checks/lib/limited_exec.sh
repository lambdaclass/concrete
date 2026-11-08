#!/usr/bin/env bash
# limited_exec.sh MEM_MB CMD [ARGS...]: run CMD with its address space capped at MEM_MB MiB (Linux;
# Darwin does not enforce RLIMIT_AS) and a core size of 0. For executables under test, inside
# bounded_run, which bounds their time and kills their process group; the process count is
# bounded by the gate's lib/limits.sh. Once exec succeeds, CMD's status and streams pass through
# unchanged; an executable that needs more than the cap fails differently, which the gate reports
# as a mismatch (fail closed), never as a pass. MEM_MB 0 lifts the gate's soft address-space cap
# to the hard limit (lib/limits.sh): a sanitizer build reserves terabytes of shadow address space,
# so its memory is bounded by the sanitizer's own hard_rss_limit_mb and the job's memory instead
# (slice0_artifacts.sh).
set -eu
mem_kb=$(($1 * 1024)); shift
if [ "$(uname -s)" = Linux ]; then
  if [ "$mem_kb" -gt 0 ]; then ulimit -v "$mem_kb"; else ulimit -S -v "$(ulimit -H -v)"; fi
fi
ulimit -c 0
exec "$@"
