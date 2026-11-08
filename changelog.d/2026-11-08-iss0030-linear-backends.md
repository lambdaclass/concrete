### ISS-0030: linear prepared numbering and predecessor tables (2026-11-08)

The LCB adapter and LLVM printer now prepare their register-numbering and incoming-edge tables once
per program. The closed prefix-sum definitions remain as references. Kernel proofs establish the
generated ids and offsets, first-definition-wins register lookup, Boolean validation, incoming-edge
order, `adaptFast p = adapt p`, `emit p = emitSpec p`, and `LLVM.printFast p = LLVM.print p`. The
headline theorem statements and their axiom sets are unchanged; the trusted-base golden and native
repro manifests were not edited.

Measured with the ISS-0030 family (2k+1 blocks, one parameter and four instructions per body block),
using a linear structural digest and the minimum of three runs. Times are milliseconds; `—` is the
baseline LCB k=1000 run omitted because its measured cubic growth projects about 15 hours.

| stage | k=1 before / after | k=10 before / after | k=100 before / after | k=1000 before / after |
|---|---:|---:|---:|---:|
| `LCB.emit` | 0.394 / 0.144 | 69.929 / 1.339 | 53944 / 13.528 | — / 141.896 |
| `LLVM.module` | 0.203 / 0.213 | 0.776 / 0.683 | 18.621 / 5.727 | 1437.992 / 60.883 |

The LCB 10→100 exponent fell from 2.887 to 1.004, and its optimized 100→1000 exponent is 1.021.
LLVM's exponents fell from 1.380 and 1.888 to 0.923 and 1.027. All before/after structural digests
matched. `lake build Concrete` passed with warnings as errors; native gates remain for the CI server.
