# Compiler Pipeline Performance Fixtures

This directory is reserved for Phase 6C observability/scaling fixtures and the
conditional Phase 8.5 incremental-driver edit corpus. Phase 17 later promotes
selected measurements into release budgets.

Fixtures here must pair measurement with correctness. A change belongs here
only when it tests compiler latency, memory use, cache reuse, incremental
invalidation, parallel determinism, lazy artifact emission, or a measured
data-layout/table experiment.

Rules:

- Every fixture must have a stable command and a committed expected result.
- Cache and incremental fixtures must include stale-input negative cases.
- Phase 6C `incremental_shadow/` fixtures store manifests/digests only and must
  recompute fresh output; they may not reuse compiler artifacts.
- Phase 8.5 `incremental_driver/` fixtures must compare
  `--incremental=off|on|verify`, assert executed query sets, and treat cache bytes
  as untrusted input.
- Parallel fixtures must compare serial and parallel outputs byte-for-byte.
- Codegen-unit/object fixtures must cover target/toolchain invalidation,
  corrupted/substituted objects, and deterministic relinking.
- Lazy-artifact fixtures must prove artifacts are absent unless requested.
- Data-layout fixtures must prove public output parity before using speed or
  memory results as evidence.
- This directory is not for ordinary language examples or proof examples.
