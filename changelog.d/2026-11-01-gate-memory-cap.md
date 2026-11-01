### Every gate runs under a per-process memory cap (2026-11-01)

On 2026-10-26 a probe run of a pre-fix compiler (bug 286, whose lowering memory grows with the square of
nesting depth) reached 260 GB and exhausted the dev server. `scripts/tests/lib/gate_env.sh`, which every
gate sources, now sets, on Linux, a soft 32 GiB per-process
address-space cap (`CONCRETE_MEM_CAP_GB`, 0 for none, the caller's hard limit always holding), so a
runaway compile stops with an out-of-memory error. Measured: the pre-fix compiler at depth 16000 stops
after 4 s under a 4 GiB cap; the current compiler compiles the 10^5-deep program natively, and valgrind
memcheck runs, under the default.
