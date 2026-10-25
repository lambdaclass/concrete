### Compiler Performance Tracking By Retired Instructions (2026-10-22)

`scripts/perf/` measures every new main commit with a versioned benchmark suite and keeps the
records on the `perf-data` branch, which is never merged into main. The primary metric is retired
user-space instructions (`perf stat -e instructions:u`), because no machine is ever quiet and
instruction counts do not move with load: measured at load 35, they varied by at most 0.002 %
between runs. Peak RSS is the memory metric, and wall time is recorded but never alarmed on. An
alarm needs a value above the baseline's noise band plus a 1 % threshold and an absolute floor
(about +1.5 % for instructions), and `perf_bisect.py` then names the culprit commit. A run that
cannot count instructions falls back to wall time and is marked not comparable. The suite includes
synthetic stress programs, whose growth exponents are computed, and the ten most expensive passing
CI gates. [PERF_TRACKING.md](docs/project/PERF_TRACKING.md) describes the engine and what the
server needs; `check_perf_tracking.sh` (CI) gates the decision logic.
