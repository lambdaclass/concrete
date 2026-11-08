# Brief: make the LCB adapter and the LLVM printer linear (ISS-0030) — branch lcb-numbering

You work in the full clone /home/unbalancedparen/projects/gates-lcb-numbering, branch
`lcb-numbering`, based on main ea212942 (slice 0 landed). Its first commit (4eabaffa) files ISS-0030
in ISSUES.tsv; read that row first — it has the measurements, the reproducer and the cause. Then
read AGENTS.md (repository root), CLAUDE.md, Concrete/README.md (pipeline, "The `adapter_correct`
proof, in layers", gates), Concrete/Backend/LCB.lean and Concrete/Backend/LLVM.lean.

Do NOT push, do NOT touch git remotes, do NOT run `git rebase/reset/clean/stash`. Commit on the
branch in small commits ("ISS-0030: ..."). Every command bounded with `timeout`. You may run
`timeout 3600 lake build Concrete` (this is a proof-heavy build; run it at most a few times, and
never two builds at once). Never run the native gates (scripts/checks/compiler/*.sh); I run them on
the CI server. Scratch only under mktemp.

## The defect (confirmed by Codex from source)

- Concrete/Backend/LCB.lean: `regId p r := (ids p).lookup r` (line ~103) rebuilds `ids p` (lines
  ~75-79, which computes `blockBase p j` and `instBase b i`, prefix sums, per entry: Θ(n²)) on every
  operand lookup, so `adapt` and `emit` are Θ(n³). `numberingOk` (~95-99) adds quadratic List.Nodup
  and membership checks; `adaptInst` repeats prefix sums; `adaptPhi` scans every block for incoming
  edges per phi (`incoming`).
- Concrete/Backend/LLVM.lean: `print` (~87-90) calls `phis p j b` per block, which rebuilds `edges p`
  (~57-62) and filters it for target j: Θ(n²).
- Measured: LCB.emit 27.9 ms at k=10 and 21.6 s at k=100 (exponent 2.89); LLVM.module exponent 1.52.
- Structural cause: closed specification functions (numbering and predecessor relations as
  functions of position, chosen for the proofs) also serve directly as executable queries.

## The fix (decided; do not change the approach)

Keep the existing definitions and every existing theorem as the REFERENCE. Add a prepared
representation computed once per program and prove it equal to the reference, so that
`Proof.adapter_correct`, `Proof.emit_correct`, `Proof.artifact_correct`,
`Proof.artifact_of_emit_error` and every other theorem keep their EXACT statements (same names,
same hypotheses, same conclusions):

1. Numbering: build the id table once with running block/instruction offsets (a single pass), and
   a lookup structure (e.g. `Std.HashMap Reg Nat` or an array indexed by register when registers are
   dense — choose what you can prove cleanly in core Lean; no Mathlib) whose lookup is proved equal
   to `regId p r` for every `r`, including the malformed-input cases `adapt` refuses (first
   definition wins, exactly as `List.lookup`). Prove the generated list equals `ids p` and the
   offsets equal `blockBase`/`instBase`.
2. Validation: replace `numberingOk`'s quadratic checks in the executable path with an equivalent
   linear (or n log n) check proved to return the same Bool as `numberingOk p`, so acceptance and
   refusal are unchanged.
3. Incoming edges: group edges by target once (preserving predecessor order and both conditional
   arms, exactly the order `incoming p j` gives), proved equal to `incoming p j` for every j. Use it
   in the LCB adapter's phis and in the LLVM printer's phis (`LLVM.phis`), proved equal to the
   current printed text (the printer is unverified, P_print, but its output must not change: prove
   `printFast b = print b`, or at least the module text equality, so gate outputs stay byte-identical
   and the trusted-base golden and repro manifests are unchanged).
4. Define the fast functions (`adaptFast`/`emitFast` or replace the bodies of `adapt`/`emit` with
   the fast code), prove `adaptFast p = adapt p` (and the emit equality), and make the executable
   entry points (`LCB.emit`, `LLVM.module`, whatever slice0_bir.lean and the gates call) use the
   fast code. If you replace the bodies, the reference definitions keep existing under a clear name
   (e.g. `adaptSpec`) and the proofs are re-pointed by rewriting with the equality — the theorem
   statements visible in the golden (tests/golden/slice0_trusted_base.txt) must not change; if any
   statement must change, stop and say why instead.
5. Axioms: only propext, Classical.choice, Quot.sound; no sorry, no native_decide, no partial,
   unsafe, implemented_by, extern or opaque (the trust ban and gate 9 check this).
6. Evidence: measure with the reproducer (`lake env lean --run` a copy of
   scripts/checks/compiler/scaling.lean... it is on another branch: instead write a small probe under
   mktemp that times LLVM.module and LCB.emit on a scaled family of k = 1, 10, 100, 1000 blocks with
   a linear structural digest) before and after; report the numbers and exponents in your final
   message and in a changelog.d fragment. Target: exponent ≤ 1.2 for both at 10→100 and 100→1000.
7. Add the ISS-0030 closure evidence to nothing yet (I close the issue after the server gates).

## Constraints

- No language/spec decisions; ConcreteSpec/ untouched.
- Update Concrete/README.md where it describes the numbering (layer 3) only if the design text
  becomes wrong; keep its style.
- Final message: commits, build result (warnings are errors), axioms of the headline theorems
  (unchanged), before/after timings, open questions.
