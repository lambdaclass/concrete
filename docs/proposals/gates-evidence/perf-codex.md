Confirmed from source. No files changed, builds run, or network used.

1. **LCB.emit / adapt is cubic on this family.** Let \(n\) be program size, counting blocks, instructions, parameters, and operand occurrences; use unit-cost arithmetic.

   - [`blockBase`, line 61](Concrete/Backend/LCB.lean:61) traverses preceding blocks and their instructions through `blockSize` (line 59). [`instBase`, line 63](Concrete/Backend/LCB.lean:63) similarly rescans an instruction prefix.
   - [`ids`, lines 75–79](Concrete/Backend/LCB.lean:75) computes these prefixes for every definition. On `scaled k`, there are Θ(k) definitions and bounded block sizes, so constructing `ids p` costs Θ(k²).
   - [`regId`, line 103](Concrete/Backend/LCB.lean:103) constructs `ids p` anew for each lookup, then performs a linear list lookup. Nothing here memoizes the table across calls.
   - `idOf` → `operand` → `inst` invokes this path for instruction operands (lines 105–147); terminators and phi arms also invoke it (lines 159–172). `scaled k` has Θ(k) register uses. Thus **`adapt`, and consequently `emit`, perform Θ(k³) work** on this family. The observed exponent 2.89 fits that mechanism.

   [`numberingOk`, lines 95–99](Concrete/Backend/LCB.lean:95) adds quadratic work: list `Nodup`, intermediate-to-slot membership tests, and prefix-based construction of IDs/intermediates. It also places `valueCount p` inside per-element predicates. Separately, `adaptInst` repeats prefix calculations, and `adaptPhi` scans all blocks for incoming edges. These matter after eliminating the cubic term.

2. **LLVM.module has a source-level quadratic cause.**

   [`print`, lines 87–90](Concrete/Backend/LLVM.lean:87) calls `phis p j b` for each block. [`phis`, lines 64–69](Concrete/Backend/LLVM.lean:64) constructs `edges p` and filters the entire edge list for target `j`. [`edges`, lines 57–62](Concrete/Backend/LLVM.lean:57) scans all blocks.

   `scaled k` has Θ(k) blocks, Θ(k) edges, and Θ(k) parameter-bearing blocks. Even restricting attention to those blocks, this is **Θ(k²)** work. It remains quadratic if edge construction were shared: each target still filters the whole list.

   The finite-range exponent 1.52 does not establish an asymptotic exponent of 1.5. A mixture of quadratic edge processing, output construction, and fixed overhead explains a value between 1 and 2; source inspection cannot apportion the measured milliseconds. There is no need to attribute the superlinearity solely to interpreter noise. Intrinsic declarations have a bounded set of `(BinOp, IntTy)` combinations, so `eraseDups` is not an unbounded quadratic explanation here.

3. **Structural cause and smallest proof-preserving fix.**

   The design fact is that **closed specification functions also serve directly as executable queries**, repeatedly deriving global information from the entire program. Neither numbering nor predecessor information has a shared, prepared representation.

   The smallest fix removing the cubic term is to prepare the numbering table once per adaptation and pass it through operand translation, proving its lookup equals `regId p r`. Merely sharing the existing list leaves quadratic construction and quadratic aggregate lookups.

   A fuller repair should:

   - Build numbering with running block/instruction offsets; prove the generated list equals `ids p` and offsets equal `blockBase` / `instBase`.
   - Build a lookup map preserving first-definition behavior, and prove lookup equality with `regId`, including malformed inputs.
   - Replace quadratic validation with an equivalent checked implementation, preserving acceptance and refusal behavior.
   - Group incoming edges by target once, preserving predecessor order and both conditional edges. Use that in both backends.

   Keep the existing definitions and proofs as the reference specification. Prove `adaptFast p = adapt p` and corresponding emit equality; then [`adapter_correct`](Concrete/Backend/LCB.lean:1993) and [`emit_correct`](Concrete/Backend/LCB.lean:2023) transfer by rewriting. Existing numbering lemmas remain intact. This requires no language decision.

4. **This is a performance defect, not a semantic correctness defect.** Repeated computation produces the same IDs, checks, and phi arms; it does not change generated behavior or invalidate those proofs. Excessive time or memory can prevent compilation operationally. Any optimization must preserve validation and ordering rather than bypass them.

VERDICT: CONFIRMED