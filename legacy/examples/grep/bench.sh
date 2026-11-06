#!/usr/bin/env bash
#
# bench.sh -- benchmark Concrete cgrep vs C cgrep_c
#
# Steps:
#   1. Generate a 100k-line test file with mixed text (deterministic; not tracked)
#   2. Compile the C version with -O2
#   3. Build the Concrete version with `concrete build`
#   4. Run both on the same search pattern 5 times each
#   5. Report average wall-clock times
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR"

TESTFILE="bench_testdata.txt"
C_SRC="bench_c.c"
C_BIN="cgrep_c"
CON_BIN="cgrep"
PATTERN="error"
RUNS=5

# ── 1. Generate test file ────────────────────────────────────────────
# The input is generated, not tracked (it is about 6 MB): gen_bench_testdata.py with a fixed seed
# writes byte-identical data on every machine, pinned by its SHA-256 below. A file left by an older,
# unseeded generator does not match and is regenerated, so every run measures the same bytes.
TESTFILE_SHA256="54d077a08cabb3dd17fd5d00c0a0682abbb9697aa115f08a4d7b1aea3d2cf366"
sha256_of() { if command -v sha256sum >/dev/null 2>&1; then sha256sum "$1" | cut -d' ' -f1; else shasum -a 256 "$1" | cut -d' ' -f1; fi; }
echo "==> Generating $TESTFILE (100000 lines, fixed seed)..."
if [ -f "$TESTFILE" ] && [ "$(sha256_of "$TESTFILE")" = "$TESTFILE_SHA256" ]; then
    echo "    (already exists and matches, skipping)"
else
    python3 gen_bench_testdata.py "$TESTFILE"
    got="$(sha256_of "$TESTFILE")"
    if [ "$got" != "$TESTFILE_SHA256" ]; then
        echo "    ERROR: generated $TESTFILE has SHA-256 $got, expected $TESTFILE_SHA256" >&2
        echo "    (the generator is no longer deterministic here; fix it before comparing timings)" >&2
        exit 1
    fi
    echo "    done."
fi

# ── 2. Compile C version ─────────────────────────────────────────────
echo "==> Compiling C version ($C_SRC -> $C_BIN) with -O2..."
cc -O2 -o "$C_BIN" "$C_SRC"
echo "    done."

# ── 3. Build Concrete version ────────────────────────────────────────
echo "==> Building Concrete version..."
CONCRETE_COMPILER="$(cd "$SCRIPT_DIR/../.." && pwd)/.lake/build/bin/concrete"
(cd "$SCRIPT_DIR" && "$CONCRETE_COMPILER" build 2>&1) || {
    echo "    WARNING: concrete build failed; skipping Concrete benchmarks."
    CON_BIN=""
}
echo "    done."

# ── Helper: average time over N runs ─────────────────────────────────
#   Usage: bench_avg <label> <command...>
#   Prints each run's time and the average.
bench_avg() {
    local label="$1"; shift
    echo ""
    echo "--- $label ($RUNS runs, pattern='$PATTERN') ---"
    local total=0
    for i in $(seq 1 $RUNS); do
        # Use bash built-in TIMEFORMAT; redirect program stdout to /dev/null
        local t
        t=$( { TIMEFORMAT='%R'; time "$@" > /dev/null; } 2>&1 )
        printf "  run %d: %s s\n" "$i" "$t"
        total=$(echo "$total + $t" | bc)
    done
    local avg
    avg=$(echo "scale=4; $total / $RUNS" | bc)
    printf "  avg:   %s s\n" "$avg"
}

# ── 4 & 5. Run benchmarks ────────────────────────────────────────────
echo ""
echo "==========================================="
echo " Benchmark: plain search (pattern='$PATTERN')"
echo "==========================================="

bench_avg "C (cgrep_c)" ./"$C_BIN" "$PATTERN" "$TESTFILE"

if [ -n "$CON_BIN" ] && [ -x "$CON_BIN" ]; then
    bench_avg "Concrete (cgrep)" ./"$CON_BIN" "$PATTERN" "$TESTFILE"
else
    echo ""
    echo "--- Concrete (cgrep): SKIPPED (binary not available) ---"
fi

echo ""
echo "==========================================="
echo " Benchmark: case-insensitive + line numbers"
echo "==========================================="

bench_avg "C (cgrep_c) -i -n" ./"$C_BIN" -i -n "$PATTERN" "$TESTFILE"

if [ -n "$CON_BIN" ] && [ -x "$CON_BIN" ]; then
    bench_avg "Concrete (cgrep) -i -n" ./"$CON_BIN" -i -n "$PATTERN" "$TESTFILE"
else
    echo ""
    echo "--- Concrete (cgrep) -i -n: SKIPPED ---"
fi

echo ""
echo "==========================================="
echo " Benchmark: count only (-c)"
echo "==========================================="

bench_avg "C (cgrep_c) -c" ./"$C_BIN" -c "$PATTERN" "$TESTFILE"

if [ -n "$CON_BIN" ] && [ -x "$CON_BIN" ]; then
    bench_avg "Concrete (cgrep) -c" ./"$CON_BIN" -c "$PATTERN" "$TESTFILE"
else
    echo ""
    echo "--- Concrete (cgrep) -c: SKIPPED ---"
fi

echo ""
echo "==> Benchmark complete."
