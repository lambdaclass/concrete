**PROPOSED, awaiting owner decision (2026-11-06); reviewed by Codex, agy, Kimi.**

# Pipeline review: merged findings (Codex, agy/Gemini, Kimi), 2026-11-06

Snapshot: origin/main 70e222d94, origin/wip/spec-root 342850ebb, origin/wip/slice0 b986be822 (git archive
copies in this folder: codex/, agy/, kimi/, repo/). Brief: BRIEF.md. Raw outputs: codex.out, agy.out, kimi.out.
All three ran to completion (exit 0). No reviewer was denied a permission; agy and Kimi used read tools only.

Paths below: `slice0/` = `_branches/slice0/`, `spec/` = `_branches/spec-root/ConcreteSpec/`, `VP` =
`legacy/docs/compiler/VERIFIED_PIPELINE.md`. I checked every claim kept here against those files.

## Verdicts

- Codex: **BEST-WITH-CHANGES**. The direction is right; representation, evidence and relational
  preservation need explicit boundaries from slice 0.
- agy: **BEST-WITH-CHANGES**. Needs both backends and source spans in slice 0, an explicit Memory CFG,
  post-Mono re-checking, and modular certificates in place of whole-compiler `decide`.
- Kimi: **BEST-WITH-CHANGES**. Needs Memory CFG, cross-stage provenance, explicit security preservation
  and a complete BIR target contract.

## Merged ranked list

1. **Restore a Memory CFG stage (layout/ABI lowering) between Place MIR and BIR.** Raised by all three.
   *Problem:* the planned chain goes Place MIR → BIR (slice0/Concrete/README.md, "The pipeline"). The
   earlier plan gives Memory CFG its own job: one `Layout` authority, offsets, alignment, loads and stores,
   CompCert block–offset memory (VP "IR chain" l.162; "Passes" l.211; S11 l.775). Without it, layout and
   provenance spread into SSA construction and both printers. spec/Spec/Rep.lean already defines the byte
   encoding that it would consume. It can be the identity in slice 0. *Cost:* L. **Internal.**

2. **Write BIR as a complete backend contract, and take stack bytes from the final artifact.** Raised by
   all three. *Problem:* BIR is the right shared boundary only once it fixes integer semantics, layout and
   endianness, alignment, zero-sized values, internal and foreign calling conventions, external events,
   trap realization (kind plus site), and a frame-accounting handoff. BIR slot counts cannot give stack
   bytes, because spills, outgoing arguments, helpers and startup all sit below BIR. The slice-0 facts
   table already says this ("Stack exhaustion… notEstablished… BIR slot count… never as bytes"). Stack
   bounds need allocated frames, the call graph and the launcher's reservation (decisions.md
   no-hidden-traps, Q14). *Cost:* L. **Internal.**

3. **Make CheckedCore the single admission point, re-check after Mono, and carry evidence downward.**
   Raised by all three. *Problem:* the pipeline does not say where trap admission, termination and fact
   derivation run, or how they survive Mono, MIR and BIR. The proposal:
   - Run admission (octagon evidence, closed contracts, termination permissions) on Core.
   - Re-validate after Mono: substitution and indirect-target resolution can expose new constants and
     cycles. Row Q6.3 already decides "parametric checking plus substitution-time rechecking".
   - Let Place MIR only expand residual checks, never admit.
   - Let MIR and BIR carry operation-specific discharged-evidence tokens, so that no pass removes a check
     without evidence (VP l.190–192, PASS-9).

   The spec still has the pre-decision rule: `knownViolates`-only refusal, `add100` accepted
   (spec/Spec/Examples.lean l.317–328), and `RangeOk` accepting an unknown bound (spec/Spec/Borrow.lean
   l.343–346). Codex and Kimi cite this. It is already disclosed in Borrow.lean l.215–216 and tracked in
   R-0574/A2i, so it is a known gap, not a new finding. *Cost:* L. **Internal** (the decision is made;
   the three pending spellings stay with the owner).

4. **Replace whole-compiler `by decide` certificates with per-pass certificates.** Raised by all three.
   *Problem:* the slice-0 certificate is `theorem cert : compile ⟨bytes⟩ = .ok ⟨bir⟩ := by decide`
   (slice0 README, "Per-compilation evidence"). That makes the kernel re-run the whole compiler by
   reduction. It works for five-line programs and will not scale. The fix composes universal pass theorems
   with small witnesses that are linear in program size and checked per edge. Keeping `decide` for slice 0
   is fine (Codex). *Cost:* M. **Internal.**

5. **Carry typed source identity end to end, as one evidence graph.** Raised by all three. *Problem:* Core
   has no source positions; open obligations O1 and O4 in spec/README.md l.106–115 cover them, and trap
   diagnostics need them. The proposal:
   - Mint typed `SiteId`/span/origin edges at lex and parse.
   - Preserve them through generated temporaries, specialization instances and cleanup down to BIR traps
     and machine-site maps.
   - Render diagnostics, JSON, facts and policy from one derivation.

   Kimi and Codex also note that `derive_sound` covers only `established` facts (slice0 README l.170).
   Conditional, refuted and coverage statuses need soundness theorems too, and so does final-artifact
   stack evidence. `Fact.Holds` is typed over `CheckedCore` only. agy and Codex note that ROADMAP F still
   places the audit/query surface late (R-0201, ROADMAP l.354), against "from the start". *Cost:* S–M.
   **Internal**, except that the policy-file syntax and CLI spellings are **owner decisions** (the slice-0
   README already lists the CLI as open).

6. **Give information flow and constant time a separate obligation, preserved through every stage.**
   Raised by all three. *Problem:* the brief calls this CRUCIAL, but the ideas sweep lists it as DEFERRED
   (_IDEAS_SWEEP.md #11). Ordinary refinement does not preserve noninterference or constant time; VP
   "Proofs attach to Core" says so. decisions.md item 14 (l.895) already makes information-flow security a
   separate claim with its own assumptions. The proposal:
   - Define the observer and the secret/public labels.
   - Analyze dependencies on Core and Mono, and carry secret tags through MIR and BIR.
   - Require a machine-level leakage model on LCB.
   - Treat LLVM results for these claims as unverified.

   *Cost:* L. **Owner decision** for the labels, the observer and any surface marker (agy suggests a
   `ct_select` primitive). The analysis and preservation are **internal**.

7. **Choose direct proof or a validator per pass.** Raised by all three, with disagreements (below).
   - *Agreed:* prove the stable, structural passes directly: the generic table-driven LL(1) parser, which
     discharges P_parse once, sequencing, match, cleanup, representation lowering, and BIR → LCB-IR.
     Validate the search-heavy or changing passes with proved checkers: Mono specialization, SSA
     construction and destruction, optimizations, register allocation, scheduling and linking. This
     matches VP "Passes" (l.207–216).
   - *Correction, Kimi:* the lexer's slice-0 obligation (total, plus re-printing the bytes) is not a
     source-correspondence theorem. Prove it against an independent byte/token judgment.

   *Cost:* M–L. **Internal.**

8. **Record the both-backends-in-slice-0 decision and remove the stale text.** Raised by all three.
   *Problem:* slice0/Concrete/README.md "Not in slice 0" still lists "The LCB path (LP0)". On main,
   ROADMAP LP0/R-0572 (l.173, l.300) and decisions.md (proof-infrastructure "Backend order";
   long-term-2026-11-06 "Bound the LLVM hole") still put the LCB pilot after B1. agy adds that the premises
   should split into `native_llvm` and `native_lcb`, which matches the decided separate assumption
   inventories. *Cost:* S. **Owner decision** (already made; it needs recording under the recording rule).

9. **Compare more than exit codes between the two backends.** Raised by Kimi; Codex agrees ("later
   comparisons must include traces, refusal/trap diagnostics and external state"). *Problem:* status 134
   cannot tell a trap from `return 134`. spec/Spec/Observation.lean l.80–82 proves the statuses equal and
   the observations different. The fix compares full observations: status, fd-2 trap line, startup
   refusal, and later the event traces. *Cost:* S. **Internal.**

10. **Shrink the trusted base.** Raised by all three. The cheapest steps:
    - **Lean compiler and runtime:** kernel-checked per-compilation certificates (R-0521/R-0552), with
      con-leche/nanoda replay.
    - **LLVM:** keep it the labelled-unverified profile, with all authoritative native claims through LCB.
      Do not try to verify clang.
    - **Assembler, linker, loader:** validate final-image bytes and relocations against LCB's ISA model
      (R-0505).
    - **Launcher:** make it a minimal direct-syscall launcher written in Concrete and compiled through the
      verified path.
    - **OS and hardware:** narrow named contracts.
    - agy adds that the launcher should enforce `main`'s grant table with seccomp/landlock, Capsicum or
      pledge/unveil, as defense in depth (this does not remove trust). Owner, 2026-11-06: OS-level sandboxing (seccomp/landlock, Capsicum, pledge/unveil) is outside the compiler's scope; users may apply it themselves.

    *Cost:* M per item. **Internal**.

11. **Integrate the spec fragments before the compiler targets Core.** Raised by agy and Codex. Kimi
    touches it through O7–O9. *Problem:* spec/README.md says "The seven parts are separate Core fragments
    … unifying them into one Core is A3 work (R-0560)". `Slice0.toCore` targets the slice-1 fragment. A2i
    (decisions.md long-term-2026-11-06) already requires integration before the freeze. The review only
    stresses that slice 0's authoritative claim must target the integrated Core. *Cost:* L, already
    planned. **Internal.**

12. **Keep the root lean.** Raised by agy and Kimi. *Problem:* slice 0 adds four root entries: `Concrete`,
    `Concrete.lean`, `conformance`, `scripts/compiler` (slice0 README, "Files"). agy suggests putting
    compiler gates under the existing `scripts/checks/` and placing conformance under the spec or one
    `conformance/` owner. Kimi suggests deriving the stage, fact and target views from the Lean registries
    rather than hand-written docs, and keeping review sweeps and `_branches` out of the permanent root.
    *Cost:* S. **Internal.**

## Disagreements

- **HIR.** agy: collapse HIR into the resolved AST unless desugaring needs a separate tree. Kimi: HIR and
  Mono are not redundant; Mono is simply absent when there are no generics. Codex: HIR is useful only as
  the resolved surface and must not duplicate the checker.
- **Resolve/Elab proof strategy.** agy: translation validation, with name-binding and ANF witnesses checked
  against `Surface.Elab`. Codex and Kimi: prove resolution and elaboration correspondence directly. Codex
  adds that Core acceptance alone cannot show that source meaning survived elaboration.
- **SSA.** Kimi and agy: validate it, as VP says. Codex: direct SSA proofs are reasonable at first;
  validation wins as the algorithms evolve. The slice-0 design proves `ssa_correct` directly.
- **Constant time in BIR.** agy wants BIR to carry `ct_select` and secret-typed values, with LLVM barriers.
  Codex and Kimi say BIR alone cannot establish constant time: it needs a final-machine leakage model, and
  LLVM stays unverified for it.
- **Weight on the facts surface.** agy wants to move the facts query and policy file (R-0201) into B0/B1
  now. Codex wants the typed evidence graph in B0 but leaves the policy surface to the owner.

## Notes from verification

- Every row and line citation kept above was checked against the snapshot. Two were softened: the stale
  `add100`/`RangeOk` admission is already disclosed and tracked (R-0574), so it is not a new finding; the
  information-flow claim is already separated by decisions.md item 14.
- On main, "both backends in slice 0" is recorded nowhere: the ROADMAP, the decisions and the slice-0
  README all still say LCB comes after B1.
