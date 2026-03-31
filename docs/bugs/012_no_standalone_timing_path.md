# Bug 012: No Easy Timing Path For Standalone Benchmark Programs

**Status:** Fixed
**Discovered:** 2026-03-15
**Discovered in:** `examples/mal/main.con`

## Situation

| Layer | timing available? |
|-------|-------------------|
| Compiler builtin | **No** |
| Stdlib `std.time` | **Yes** — `Instant::now`, `elapsed`, `unix_timestamp`, `sleep` |
| Standalone `.con` files | **No easy path** — `import std.time` requires project/package setup |

## The Real Gap

Timing and benchmarking are blocked in standalone real programs for the same reason printing is awkward there: the usable API lives in stdlib/project setup rather than in an always-available surface.

For Phase H comparative workloads, this means:

- benchmark code cannot easily self-measure when compiled as a standalone `.con` file
- users fall back to external shell timing or ad hoc harnesses
- per-benchmark timing inside the program is harder than it should be

## What Is NOT the Problem

- Concrete does have a timing API in `std.time`
- project-based code with stdlib access can use it
- this is not "Concrete has no clock"

## Impact

- standalone benchmark examples cannot easily report their own timings
- Phase H comparison work must currently time programs externally
- the standalone/program split becomes visible in one more basic workflow

## Fix

Added `clock_monotonic_ns() -> Int` as a compiler builtin (Option A).
Returns nanoseconds from the monotonic clock via `clock_gettime(CLOCK_MONOTONIC)`.
Requires `Clock` capability.

```con
let t1: Int = clock_monotonic_ns();
// ... work ...
let t2: Int = clock_monotonic_ns();
let elapsed_ns: Int = t2 - t1;
```
