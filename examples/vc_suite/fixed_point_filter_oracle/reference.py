#!/usr/bin/env python3
# Differential oracle reference for vc_suite/fixed_point_filter.
# Emits seeded (sample, coeff) cases as `sample | coeff | expected` where the
# expected value is the Q-format scale product. Bounds match the #[requires];
# within them sample*coeff fits i32, so the compiled scale must agree exactly.
import random, sys
seed = int(sys.argv[1]) if len(sys.argv) > 1 else 0
random.seed(seed)
# deterministic edges + seeded random over the contract's ranges.
edges = [(0, 0), (30000, 32767), (-30000, 32767), (30000, -32768), (-30000, -32768), (1, -1)]
cases = edges + [(random.randint(-30000, 30000), random.randint(-32768, 32767)) for _ in range(44)]
for s, c in cases:
    print(f"{s} | {c} | {s * c}")
