### The Verified-Compiler Plan: Vertical Slices, A Frozen Legacy Evidence Layer, And LCB As The Native Backend (2026-10-19, roadmap only)

[VERIFIED_PIPELINE.md](docs/compiler/VERIFIED_PIPELINE.md) (plan v3, R-0491 – R-0509, later
extended) retargets the structural refactor at an end-to-end verified compiler. Core is the
language definition, with an executable semantics; references are strictly second-class; the
authoritative checker runs on Core; Place MIR, a Memory CFG and a block-argument IR each get a
semantics; and LLVM becomes a thin unverified printer. Part 1 builds the proof-ready pipeline and
Part 2 proves it, and the plan states, per bug class, what "no bugs" means at the end.

The plan is delivered as proved vertical slices rather than layer by layer: V1 (scalars, control,
calls, checked arithmetic, an output event) is to be carried through every stage to native AArch64
with its own compiler-correctness theorem, as a `verified` profile beside the existing compiler
(R-0511), and V2 – V7 grow it. The current evidence layer, where bugs 079 – 101 lived, receives
soundness, compatibility and security maintenance only, with no new semantic claim mechanisms, and
is to be retired claim by claim onto compiler-generated Core statements
(R-0512). The native AArch64 code generation of the `verified` profile is to be the separately
developed [Lean Compiler Backend](https://github.com/unbalancedparentheses/lean-compiler-backend), a
Lean 4 port of QBE 1.3, used as a pinned dependency; Concrete builds no machine backend of its own
(R-0517, coordinated with R-0505). These were planning changes; the full verified pipeline remains
planned, though some constituent slices have since landed (R-0492, R-0497).
