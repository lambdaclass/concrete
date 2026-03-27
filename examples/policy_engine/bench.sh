#!/bin/bash
# Benchmark: Concrete policy engine vs C policy engine
# Compiles both, runs each 5 times, reports average wall-clock time.
set -e

DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$DIR/../.." && pwd)"
ITERS=5

# ---- Build ----

echo "=== Building ==="

# C version
echo "  C: compiling with -O2 ..."
cc -O2 -o /tmp/policy_bench_c "$DIR/bench_c.c"

# Concrete version: emit LLVM IR then compile with clang -O2
echo "  Concrete: compiling via LLVM IR with -O2 ..."
cd "$ROOT"
~/.elan/bin/lake env concrete "$DIR/main.con" --emit-llvm > /tmp/policy_con.ll 2>&1
clang /tmp/policy_con.ll -o /tmp/policy_bench_con -O2 -Wno-override-module 2>/dev/null

echo ""

# ---- Helpers ----

# run_n <label> <binary> <n>
# Runs <binary> n times, prints each wall-clock time, prints average.
run_n() {
    local label="$1"
    local bin="$2"
    local n="$3"
    local total=0

    echo "--- $label ($n runs) ---"
    for i in $(seq 1 "$n"); do
        # Use bash built-in TIMEFORMAT; capture only real time in seconds
        local t
        t=$( { TIMEFORMAT='%R'; time "$bin" > /dev/null; } 2>&1 )
        echo "  run $i: ${t}s"
        total=$(echo "$total + $t" | bc)
    done
    local avg
    avg=$(echo "scale=6; $total / $n" | bc)
    echo "  avg: ${avg}s"
    echo ""
}

# ---- Run ----

echo "================================================================"
echo "  Benchmark: RBAC Policy Engine — Concrete vs C"
echo "================================================================"
echo ""

run_n "C -O2"       /tmp/policy_bench_c   "$ITERS"
run_n "Concrete -O2" /tmp/policy_bench_con "$ITERS"
