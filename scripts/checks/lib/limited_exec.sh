#!/usr/bin/env bash
# limited_exec.sh MEM_MB CMD [ARGS...]: run CMD with its address space capped at MEM_MB MiB (Linux;
# Darwin does not enforce RLIMIT_AS) and a core size of 0. For executables under test, inside
# bounded_run, which bounds their time and kills their process group; the process count is
# bounded by the gate's lib/limits.sh. Once exec succeeds, CMD's status and streams pass through
# unchanged; an executable that needs more than the cap fails differently, which the gate reports
# as a mismatch (fail closed), never as a pass. MEM_MB 0 sets no address-space cap, for a sanitizer
# build under a gate that set none (lib/limits.sh); its memory is then bounded by the sanitizer's
# own hard_rss_limit_mb and the job's memory (slice0_artifacts.sh).
set -eu
mem_kb=$(($1 * 1024)); shift
if [ "$(uname -s)" = Linux ] && [ "$mem_kb" -gt 0 ]; then ulimit -v "$mem_kb"; fi
ulimit -c 0
exec "$@"
