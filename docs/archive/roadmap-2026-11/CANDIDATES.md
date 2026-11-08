Status: historical snapshot; schedules no work and is not a current readiness record.

# Historical candidates, 2026-11-02

Generated from [`docs/project/candidates.tsv`](../../../legacy/docs/project/candidates.tsv) by
`scripts/gen/candidate_table.py --write`; `check_docs_drift.sh` fails when they differ.


| candidate | bugs | deliverable | branch | candidate SHA | exits | stage (a) | stage (b) | stage (c) | unlocks | remaining acceptance step |
|---|---|---|---|---|---|---|---|---|---|---|
| rocksdb-slice1 | — | RocksDB slice 1 groundwork (R-0285) | `rocksdb-slice1` | `094e51bf3258` | blocked by 0b (incl. bug 102), 2i, 2q | not run | not run | not run | RocksDB slice 1 acceptance, then the four-adapter checkpoint | 2026-11-02: paused until its constructs are supported by the new compiler. landed on main at 094e51bf but not accepted: exits 0b (bug 102), 2i and 2q; G1 (declaring the native library, so it builds with no manual flags) closed by `[native]` ([NATIVE_LIBRARIES.md](../../../legacy/docs/platform/NATIVE_LIBRARIES.md)); G3 closed (bug 103 fixed on main by `0c507fb9`, verified 2026-10-21) |
| v1-design-examples | — | V1 design examples (R-0511, row 3) | `v1-design-examples` | `03d2edec1d21` | none (isolated proof development) | not run | not run | not run | V1a's Core interfaces freeze | 2026-11-02: research input for phase A, not an accepted compiler deliverable. landed on main at 03d2edec but not accepted: review of the design questions, then stages (b) and (c) |
