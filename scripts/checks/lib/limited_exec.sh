#!/usr/bin/env bash
# limited_exec.sh MEM_MB CMD [ARGS...]: run CMD with its address space capped at MEM_MB MiB (Linux;
# Darwin does not enforce RLIMIT_AS) and a core size of 0. For executables under test, inside
# bounded_run, which bounds their time and kills their process group; the process count is
# bounded by the gate's lib/limits.sh. exec keeps CMD's status and streams as they are.
set -eu
mem_kb=$(($1 * 1024)); shift
if [ "$(uname -s)" = Linux ]; then ulimit -v "$mem_kb"; fi
ulimit -c 0
exec "$@"
