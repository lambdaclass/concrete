#!/bin/bash
# Benchmark comparison: Concrete MAL vs Python MAL vs Python native vs C native
set -e
DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$DIR/../.." && pwd)"
# Private scratch per run: a fixed /tmp name is owned by whoever ran first on a shared host (bug 191).
BENCH_TMP="$(mktemp -d)"
trap 'rm -rf "$BENCH_TMP"' EXIT

echo "Building..."
cd "$ROOT"
sed 's/fn bench_mode() -> bool { return false; }/fn bench_mode() -> bool { return true; }/' examples/mal/main.con > "$BENCH_TMP/mal_bench.con"
grep -q 'fn bench_mode() -> bool { return true; }' "$BENCH_TMP/mal_bench.con"
~/.elan/bin/lake env concrete "$BENCH_TMP/mal_bench.con" --emit-llvm > "$BENCH_TMP/mal.ll" 2>&1
clang "$BENCH_TMP/mal.ll" -o "$BENCH_TMP/mal_O2" -O2 -Wno-override-module 2>/dev/null
clang -O2 "$DIR/bench_c_native.c" -o "$BENCH_TMP/bench_c" 2>/dev/null

echo ""
echo "================================================================"
echo "  Benchmark: MAL (Make A Lisp) — Concrete vs Python vs C"
echo "================================================================"
echo ""
echo "Tests: fib(30), sum-loop(10000), ack(3,4), sum-closures(1000),"
echo "       compose-n(100)"
echo ""

echo "--- C native -O2 (baseline) ---"
"$BENCH_TMP/bench_c"
echo ""

echo "--- Python native ---"
python3 "$DIR/bench_python_native.py"
echo ""

echo "--- Python MAL interpreter ---"
python3 "$DIR/bench_python_mal.py"
echo ""

echo "--- Concrete MAL -O2 ---"
echo "(all benchmarks combined)"
time "$BENCH_TMP/mal_O2"
echo ""
