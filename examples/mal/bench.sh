#!/bin/bash
# Benchmark comparison: Concrete MAL vs Python MAL vs Python native vs C native
set -e
DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$DIR/../.." && pwd)"

echo "Building..."
cd "$ROOT"
~/.elan/bin/lake env concrete examples/mal/main.con --emit-llvm > /tmp/mal.ll 2>&1
clang /tmp/mal.ll -o /tmp/mal_O2 -O2 -Wno-override-module 2>/dev/null
clang -O2 "$DIR/bench_c_native.c" -o /tmp/bench_c 2>/dev/null

echo ""
echo "================================================================"
echo "  Benchmark: MAL (Make A Lisp) — Concrete vs Python vs C"
echo "================================================================"
echo ""
echo "Tests: fib(30), sum-loop(10000), ack(3,4), sum-closures(1000),"
echo "       compose-n(100)"
echo ""

echo "--- C native -O2 (baseline) ---"
/tmp/bench_c
echo ""

echo "--- Python native ---"
python3 "$DIR/bench_python_native.py"
echo ""

echo "--- Python MAL interpreter ---"
python3 "$DIR/bench_python_mal.py"
echo ""

echo "--- Concrete MAL -O2 ---"
echo "(all benchmarks combined)"
time /tmp/mal_O2
echo ""
