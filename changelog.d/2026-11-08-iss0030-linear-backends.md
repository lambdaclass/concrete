### ISS-0030: linear prepared numbering and predecessor tables (2026-11-08)

`LCB.adapt`, `LCB.emit` and `LLVM.print` now prepare their register-numbering and incoming-edge
tables once per program (`Concrete/Backend/LCB.lean`, `Concrete/Backend/LLVM.lean`,
`Concrete/Backend/Runtime.lean`). The closed prefix-sum and list-filtering definitions remain as the
reference specifications `LCB.adaptSpec`, `LCB.emitSpec` and `LLVM.printSpec`, connected to the
production entry points by the kernel-checked theorems `Proof.adapt_eq_spec : adapt p = adaptSpec p`,
`Proof.emit_eq_spec : emit p = emitSpec p` and `LLVM.print_eq_spec : LLVM.print p = LLVM.printSpec p`.
The four headline theorem statements (`Proof.adapter_correct`, `Proof.emit_correct`,
`Proof.artifact_of_emit_error`, `Proof.artifact_correct`) and their axiom sets (`propext`,
`Classical.choice`, `Quot.sound`) are unchanged and govern the production entry points directly;
`tests/golden/slice0_trusted_base.txt` is unchanged.

Register lookup/uniqueness (`regTable`, `regCheck`) and predecessor grouping (`incomingBuckets`,
`LLVM.edgeBuckets`) use `Std.HashMap`, so storage is proportional to definitions and edges rather
than the maximum numeric register name or branch target; kernel proofs (`regTable_lookup`,
`uniqueRegs_eq`) preserve first-definition-wins lookup and duplicate detection even on malformed
inputs. Each incoming edge's argument list is converted to `Array Operand` once and each bucket is
reversed once at finalization, so `adaptPhiFast` and `LLVM.phisFast` index arrays in $O(1)$ per phi
arm. Production numbering (`numberingFastFrom`, `instNumberingFast`, `Runtime.positions`,
`Runtime.indexedFrom`) and table construction are tail-recursive with $O(1)$ stack frames. Operand,
instruction, terminator, phi and block lowering share single implementations (`idOfWith`,
`operandWith`, `instWith`, `termWith`, `adaptPhiWith`, `adaptBlockWith`, `LLVM.phiText`)
parameterized by their queries.

Deterministic functional regressions (sparse/large register names, first-definition-wins lookup,
exact refusal diagnostics for duplicate/undefined registers, short phi payloads, short second
conditional arms and out-of-profile types, conditional-arm order/multiplicity, huge branch targets,
`adapt = adaptSpec` / `emit = emitSpec` / `LLVM.print = LLVM.printSpec` equivalence, and a
non-timed stack-depth check on `sparse` and `deepBlocks`) run in
`scripts/checks/compiler/slice0_bir.lean` (`numbering`, invoked by
`scripts/checks/compiler/slice0_backends.sh`). The scaling families (`wide_phi`,
`sparse_registers`, `dense_registers`, `many_blocks`, and the `100000`-entry depth probes) are
recorded in `Concrete/README.md` for Gate C (`scripts/checks/compiler/scaling.lean`).

Measured on the ISS-0030 reproducer family ($2k+1$ blocks, one parameter and four instructions per
body block) with a linear structural digest (milliseconds, minimum of three runs; `—` is the
baseline `LCB.emit` $k=1000$ run omitted because cubic growth projects ~15 hours):

| stage | k=1 before / after | k=10 before / after | k=100 before / after | k=1000 before / after | 10→100 before / after | 100→1000 after |
|---|---:|---:|---:|---:|---:|---:|
| `LCB.emit` | 0.394 / 0.144 | 69.929 / 1.339 | 53944 / 13.528 | — / 141.896 | 2.887 / 1.004 | 1.021 |
| `LLVM.module` | 0.203 / 0.213 | 0.776 / 0.683 | 18.621 / 5.727 | 1437.992 / 60.883 | 1.380 / 0.923 | 1.027 |

Measured across the four stress families (milliseconds per operation, minimum average of three
sampling windows $\ge 10\text{ ms}$, including a linear structural output digest; on `concrete-ci-2`
at `ef3fed56`, all backend exponents are $\le 1.075$):

| Family | Stage | n=10 ms | n=100 ms | n=1000 ms | 100→1000 exponent |
|---|---|---:|---:|---:|---:|
| `wide_phi` | `lcb_emit` | 0.197646 | 1.290937 | 11.345203 | 0.944 |
| `wide_phi` | `llvm_module` | 0.036779 | 0.190733 | 1.749864 | 0.963 |
| `sparse_registers` | `lcb_emit` | 0.453178 | 4.574019 | 44.876917 | 0.992 |
| `sparse_registers` | `llvm_module` | 0.020336 | 0.118637 | 1.098416 | 0.967 |
| `dense_registers` | `lcb_emit` | 0.453059 | 4.615693 | 45.264394 | 0.992 |
| `dense_registers` | `llvm_module` | 0.020237 | 0.119599 | 1.126592 | 0.974 |
| `many_blocks` | `lcb_emit` | 0.071261 | 0.635718 | 6.228520 | 0.991 |
| `many_blocks` | `llvm_module` | 0.027513 | 0.210669 | 2.069792 | 0.992 |
