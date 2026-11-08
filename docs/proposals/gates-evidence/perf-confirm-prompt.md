You are confirming (or refuting) a suspected performance bug in the Concrete compiler, in a throwaway export of main (ea212942) in the current directory. Do not modify files, do not build, no network; reason from the source.

Measurement (fede-256gb, `lake env lean --run PROBE_scaling.lean measure 1 3 x86_64-linux`; PROBE_scaling.lean builds `scaled k`, a WF BIR program with 2k+1 blocks and 4 instructions per body block, k = 1, 10, 100, and times each stage, minimum of 3 interleaved repetitions; the emit result is forced by a linear structural digest, not by repr):
  LCB.emit:     k=1 0.18 ms, k=10 27.9 ms, k=100 21584 ms   (10x->100x exponent 2.89)
  LLVM.module:  k=1 0.11 ms, k=10 0.49 ms, k=100 16.1 ms    (exponent 1.52)
  LCB.assemble: k=1 0.60 ms, k=10 4.46 ms, k=100 52.5 ms    (exponent 1.07)

Suspected cause: in Concrete/Backend/LCB.lean the numbering is a closed function of position evaluated naively: `blockBase p j` sums the sizes of the first j blocks; `ids p` builds every register's id calling `blockBase` per entry (O(n^2)); `regId p r := (ids p).lookup r` recomputes `ids p` on every operand lookup, so `adapt` is O(n^3); `numberingOk` adds List.Nodup checks (O(n^2)) over `ids p`. LLVM.module's 1.5 is unexplained.

Tasks: (1) confirm or refute that LCB.emit / adapt is superlinear (state the complexity with the exact functions and lines responsible); (2) find the cause of LLVM.module's superlinearity in Concrete/Backend/LLVM.lean (or say it is noise/interpreter artifact, with reasons); (3) state the structural cause (the design fact) and the smallest fix that keeps the proofs (`adapter_correct`, `emit_correct` and the lemmas over `blockBase`/`instBase`/`regId`) intact, e.g. computing the table once and proving it equal to the closed functions; (4) is this a correctness issue anywhere (it should not be)? Last line exactly: VERDICT: CONFIRMED or VERDICT: REFUTED.
