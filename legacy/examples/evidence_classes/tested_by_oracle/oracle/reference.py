#!/usr/bin/env python3
# Independent reference for demo.clamp, used as the differential oracle.
# Prints one case per line:  x | lo | hi | expected
# All values are in [0, 200] so the expected result fits a process exit code.
import random, sys
N = 200
seed = int(sys.argv[1]) if len(sys.argv) > 1 else 0
random.seed(seed)

def clamp(x, lo, hi):
    return lo if x < lo else (hi if x > hi else x)

for _ in range(N):
    lo = random.randint(0, 100)
    hi = lo + random.randint(0, 100)
    x = random.randint(0, 200)
    print(f"{x} | {lo} | {hi} | {clamp(x, lo, hi)}")
