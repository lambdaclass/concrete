# Compiler Pipeline Performance Fixtures

This directory is reserved for Phase 4 compiler-speed experiments.

Fixtures here must pair measurement with correctness. A change belongs here
only when it tests compiler latency, memory use, cache reuse, incremental
invalidation, parallel determinism, lazy artifact emission, or a measured
data-layout/table experiment.

Rules:

- Every fixture must have a stable command and a committed expected result.
- Cache and incremental fixtures must include stale-input negative cases.
- Parallel fixtures must compare serial and parallel outputs byte-for-byte.
- Lazy-artifact fixtures must prove artifacts are absent unless requested.
- Data-layout fixtures must prove public output parity before using speed or
  memory results as evidence.
- This directory is not for ordinary language examples or proof examples.
