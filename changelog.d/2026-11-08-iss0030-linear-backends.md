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

Landing dependency: this branch lands **together with Gate C**, through Gate C's merge of this
branch (`wip/gate-scaling-determinism`), never alone. [R-0550](../ROADMAP.md) owns backend scaling.
Gate C must provide meaningful cheap PR coverage plus full scheduled scaling assertions and
100000-entry depth probes on `concrete-ci-2`, as recorded in `Concrete/README.md`. ISS-0030
remains open until Gate C's families pass there against the integrated candidate's exact SHA.

Local stress-family measurements below are from commit
`1103b54c4f9e5e60605ce96c0d1e7576b82e5657`, on the Linux shared development host
(hostname was not retained in the original record; these are not server measurements).
Method: `nix develop --command lake env lean --run scripts/checks/compiler/lcb_numbering_cases.lean`,
interpreter execution at sizes 10/100/1000, milliseconds per operation, minimum average of three
sampling windows of at least 10 ms, including a linear structural output digest. Exponents use
100→1000. The older reproducer before/after table is omitted because its exact measured revisions
and host were not recorded; it cannot supply reproducible benchmark provenance.

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

Server qualification, separately from the local table (coordinator's `gates-briefs/RESULTS.md`):

- Host `concrete-ci-2`, commit `ee2d82528fe84b2da2d69eec9756e168e29a1ecd`: build,
  trusted base, backends, artifacts, repro and root checks passed. A throwaway integration with
  Gate C `1f5457c9` used its full scaling driver, base size 1 (sizes 1/10/100)
  and a 10 ms resolution floor (the sampling-window/repetition details were not retained in the
  qualification record). Backend 10→100 exponents were at most 1.08;
  the scaling gate failed the resolution floor. The throwaway merge SHA was not retained,
  so this is evidence associated with those inputs, not qualification of an exact merged candidate.
- Host `concrete-ci-2`, commit `ef3fed56ab78f1adb04778d099091c6c57c8d2fe`: build,
  trusted base, backends, artifacts, repro and root checks passed; `lcb_numbering.sh --full`
  passed (stress-family sizes 100/1000/10000, three sampling windows of at least 50 ms,
  and 100000-entry depth probes). The separate throwaway integration with Gate C `1f5457c9`
  again had backend exponents at most 1.08 but failed its resolution floor at base size 1;
  its merge SHA was not retained. Neither floor failure is a passing scaling qualification.
- Host `concrete-ci-2`, exact commit `cea8af594b287a6b8442005db122cd08d0669755`, fresh
  clone: build, trusted base, backends (including moved numbering regressions), artifacts, repro
  and root checks (11/11) passed. This does not qualify Gate C's pending family additions.

The coordinator must run the integrated Gate C families on `concrete-ci-2` and record the exact
merged SHA, sizes, sampling method and results before closing ISS-0030.
