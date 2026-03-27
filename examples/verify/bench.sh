#!/usr/bin/env bash
#
# bench.sh — benchmark Concrete conhash vs C conhash_c
#
# Generates a 10MB test file, compiles both versions, runs each 5 times,
# and reports average wall-clock times.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR"

CONCRETE_COMPILER="$(cd "$SCRIPT_DIR/../.." && pwd)/.lake/build/bin/concrete"
RUNS=5
TEST_FILE="/tmp/conhash_bench_10mb.bin"
TEST_SIZE=$((10 * 1024 * 1024))

# ---- Step 1: Generate test file ----
echo "==> Generating ${TEST_SIZE}-byte test file..."
dd if=/dev/urandom of="$TEST_FILE" bs=1048576 count=10 2>/dev/null
echo "    $TEST_FILE ($(wc -c < "$TEST_FILE" | tr -d ' ') bytes)"

# Get reference hash
EXPECTED_HASH="$(shasum -a 256 "$TEST_FILE" | cut -d' ' -f1)"
echo "    SHA-256: $EXPECTED_HASH"
echo

# ---- Step 2: Compile C version ----
echo "==> Compiling C version (bench_c.c -> conhash_c) with -O2..."
cc -O2 -o conhash_c bench_c.c
echo "    done."
echo

# ---- Step 3: Build Concrete version ----
echo "==> Building Concrete version..."
if [ -x "$CONCRETE_COMPILER" ]; then
    (cd "$SCRIPT_DIR" && "$CONCRETE_COMPILER" build -o conhash 2>&1) || true
    echo "    done."
else
    echo "    WARNING: Concrete compiler not found at $CONCRETE_COMPILER"
    echo "    Using existing conhash binary (if present)."
fi
echo

# ---- Timing helper ----
# Uses bash built-in EPOCHREALTIME (bash 5+) or falls back to perl/date.
get_ns() {
    if command -v gdate >/dev/null 2>&1; then
        gdate +%s%N
    elif date +%s%N | grep -qv N; then
        date +%s%N
    else
        perl -MTime::HiRes=time -e 'printf "%d\n", time()*1e9'
    fi
}

run_timed() {
    local binary="$1"
    local file="$2"
    local t0 t1
    t0="$(get_ns)"
    "$binary" "$file" >/dev/null 2>&1
    t1="$(get_ns)"
    echo $(( (t1 - t0) / 1000000 ))
}

# ---- Step 4: Benchmark C version ----
echo "==> Benchmarking C version ($RUNS runs)..."
c_total=0
for i in $(seq 1 $RUNS); do
    ms="$(run_timed ./conhash_c "$TEST_FILE")"
    printf "    run %d: %d ms\n" "$i" "$ms"
    c_total=$((c_total + ms))
done
c_avg=$((c_total / RUNS))
echo "    C average: ${c_avg} ms"
echo

# ---- Step 5: Benchmark Concrete version ----
if [ -x ./conhash ]; then
    echo "==> Benchmarking Concrete version ($RUNS runs)..."
    con_total=0
    for i in $(seq 1 $RUNS); do
        ms="$(run_timed ./conhash "$TEST_FILE")"
        printf "    run %d: %d ms\n" "$i" "$ms"
        con_total=$((con_total + ms))
    done
    con_avg=$((con_total / RUNS))
    echo "    Concrete average: ${con_avg} ms"
    echo
else
    echo "==> Skipping Concrete benchmark (no conhash binary found)."
    con_avg="N/A"
    echo
fi

# ---- Step 6: Summary ----
echo "========================================="
echo "  RESULTS (10 MB file, $RUNS runs each)"
echo "========================================="
printf "  C (O2):    %s ms avg\n" "$c_avg"
printf "  Concrete:  %s ms avg\n" "$con_avg"
if [ "$con_avg" != "N/A" ] && [ "$c_avg" -gt 0 ]; then
    ratio=$(echo "scale=1; $con_avg / $c_avg" | bc 2>/dev/null || echo "?")
    printf "  Ratio:     %sx (Concrete / C)\n" "$ratio"
fi
echo "========================================="

# ---- Cleanup ----
rm -f "$TEST_FILE"
