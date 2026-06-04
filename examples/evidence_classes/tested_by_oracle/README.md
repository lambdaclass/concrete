# tested_by_oracle

**Class:** evidence from a differential oracle — the compiled program agrees
with an independent reference implementation across a vector set. This is a
`tested` (not `proved`) evidence class: it raises confidence, it does not
kernel-verify. Disagreement is real signal (the source, the reference, or the
spec is wrong).

`demo.clamp` is checked against `oracle/reference.py` (an independent Python
re-implementation) across 200 seeded random cases:

```sh
bash examples/evidence_classes/tested_by_oracle/oracle/run_oracle.sh 0
# → ORACLE (tested_by_oracle, seed=0): PASS=200  FAIL=0  TOTAL=200
```

The runner generates a tiny Concrete driver per case (calls `clamp` with literal
inputs), compiles it natively, and compares the printed result to the reference.
This is the same shape as the flagship oracles (e.g. `constant_time_tag`),
shrunk to one clean function. It requires the native toolchain (`llvm-as` +
a C compiler), so it runs on demand rather than in the fast snapshot gate.
