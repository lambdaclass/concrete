# A reproducible, independently checkable compiler backend in Lean 4

Status: research proposal, 2026-10-18. This proposes changes to the sequencing of
the standalone `lean-compiler-backend` project; it does not change that project's
roadmap or Concrete's implementation. The backend is implemented in Lean 4 and
accepts a general-purpose IR. It is not specifically a compiler for Lean programs.

## Recommendation

Keep full QBE 1.3 compatibility as the first complete product milestone. Establish
function isolation, explicit semantics, typed lowering boundaries and reproducible
artifacts while completing that port. These foundations are expensive to retrofit.
Grow proofs alongside the implementation, with an explicitly limited certified
subset until the remaining paths are covered.

The central interfaces should be pure operations:

```lean
-- Architectural sketch, not a declaration of existing repository APIs.
compileFunction : FunctionJob → Except CompileError FunctionArtifact
verifyFunction :
  FunctionJob → FunctionArtifact → Except ValidationError CheckedArtifact
```

Filesystem access, scheduling, cache lookup and external tools belong in the
driver. `CheckedArtifact` must record precisely which properties were established.
Initially that may mean structural validity and artifact consistency; semantic
certification requires the relevant proved checker or proof evidence. A successful
verification result must never imply broader coverage than the checker provides.

A job includes every input that can influence its result. A function artifact
contains symbolic references rather than final addresses. Module planning resolves
dependencies before compilation; deterministic assembly and linking resolve layout
afterward. This makes the same unit useful for parallel compilation, caching,
proofs, regression tests and debugging.

There is no credible basis for promising a universal 1,000× improvement. The
high-value improvement is making failures local, repeatable and checkable, while
avoiding repeated work. Compile-time and generated-code gains must be measured.

## What to borrow

The observations below describe upstream ideas; the final column is our proposal.
They are not claims that upstream projects already provide our complete contract.

| Project | Relevant evidence | Adopt or adapt |
|---|---|---|
| QBE | A compact backend with a small SSA language and practical C ABI support. [Project and IL](https://c9x.me/compile/) | Preserve its manageable scope, compatibility corpus and baseline implementation. Use it as a differential oracle on defined inputs. |
| CompCert | Explicit intermediate semantics, simulation proofs, separate compilation, and validation of allocation results. [Documentation](https://compcert.org/doc/index.html) | Make semantic contracts and composition the architecture's organizing principle. Prove small transformations; use proved validators for suitable complex heuristics. |
| CompCertSSA | A verified SSA middle end, including construction and destruction. [Project](https://compcertssa.gitlabpages.inria.fr/) | Study the proof obligations for SSA transitions and dominance instead of assuming SSA makes proofs automatic. |
| Cranelift | Machine IR, declarative ISLE lowering and an external register allocator. [ISLE](https://github.com/bytecodealliance/wasmtime/tree/main/cranelift/isle), [machine backend](https://github.com/bytecodealliance/wasmtime/blob/main/cranelift/codegen/src/machinst/mod.rs) | Explicit target constraints and small inspectable lowering rules. Use symbolic allocation checking as inspiration, then prove our checker's soundness. |
| Zig | Function code generation and target-specific MIR are visible in the pinned source. [Official source distributions](https://ziglang.org/download/index.json) | A function produces its own code and relocation data. Strengthen the boundary to an immutable dependency snapshot; Zig's organization alone is not evidence of hermetic or bit-identical compilation. |
| MLIR | Isolated operations, scoped passes, legality-driven conversion, interfaces, block arguments and reproducers. [Pass management](https://mlir.llvm.org/docs/PassManagement/) | Borrow these contracts in a small Lean implementation. Avoid making a general dialect framework a prerequisite for the port. |
| LLVM | Explicit machine IR fixtures and target legalization. [MIR](https://llvm.org/docs/MIRLangRef.html), [GlobalISel legality](https://llvm.org/docs/GlobalISel/Legalizer.html) | Serialize intermediate stages and test individual passes. Make unsupported target operations explicit. |
| MIR, by Vladimir Makarov | A lightweight IR, interpreter and JIT pipeline. [Repository](https://github.com/vnmakarov/mir) | Keep an executable semantic reference and an inexpensive baseline lowering. A JIT is a separate future product decision. This is distinct from LLVM MIR and Rust MIR. |
| IR, by Dmitry Stogov | Graph-based JIT infrastructure and declarative folding rules. [Repository](https://github.com/dstogov/ir) | A compact rule library with explicit conditions. Adopting a sea-of-nodes representation is not necessary to obtain that benefit. |
| libFirm | Graph-based SSA, analyses, optimizations and graph inspection. [Features](https://libfirm.github.io/Features.html) | Strong invariant checks and useful stage dumps. Evaluate individual analyses rather than inheriting the entire graph architecture. |
| CakeML | A verified compiler extending to concrete machine code. [Project](https://cakeml.org/) | Use its end-to-end scope as a reference for eventually closing the encoding gap. An assembly-level theorem does not cover emitted bytes. |
| Alive2 | Translation validation for LLVM transformations. [Repository](https://github.com/AliveToolkit/alive2) | Check transformations at precise boundaries. It cannot directly validate our custom IR or native output. |
| lean-mlir | Lean infrastructure for SSA and verified rewrites. [Repository](https://github.com/opencompl/lean-mlir) | Run a bounded reuse experiment before choosing a dependency: one arithmetic rewrite, one conditional rewrite and a representative control-flow example. |
| Sail | Executable ISA specifications, including RISC-V. [RISC-V model](https://github.com/riscv/sail-riscv) | Evaluate a target-semantics connection for RV64 later. It does not automatically establish the AArch64 implementation's correctness. |

The original size comparisons used different source scopes and excluded different
dependencies. They should not decide architecture. Record pinned revisions and a
uniform counting methodology before presenting a fresh size ranking.

## The function independence contract

`FunctionJob` should contain:

- Validated function IR and a stable symbol identity.
- Target architecture, explicitly selected CPU features, ABI, data layout and
  relocation/code model.
- The exact imported declarations and semantic contracts the function uses:
  signatures, layouts, symbol linkage, runtime helpers and relevant constants.
- Compilation options, a versioned pass recipe and compiler identity.
- Any optional inline bodies, specialization facts or profile inputs actually
  consumed by this compilation.

All references must be resolved before entering the worker. Workers cannot look
up arbitrary module state. An imported function's implementation changing does
not invalidate callers unless they consumed its body or a derived fact. Its ABI
changing does invalidate them. Recursive calls use stable declaration identities;
they do not require recursively hashing every callee body.

Use local dense IDs internally and stable names at boundaries. Module insertion
order, pointer addresses, global fresh-name counters, hash-table iteration,
worker scheduling, host CPU detection, working directory, clock and random state
must not influence code generation. Local mutation inside a pure Lean interface
is compatible with this design; uncontrolled ambient inputs are the problem.

The acceptance criterion is stronger than rebuilding a whole program twice:
changing an unrelated function must leave this function's canonically serialized
artifact byte-for-byte unchanged, under the same declared compilation configuration.

Define a versioned canonical serialization with specified ordering and integer
encoding. Do not hash `Repr` output. A digest indexes a cache; it is not a proof
that two inputs are equal. Validate cached records and bind any correctness
evidence to the actual input and output. Separate job, code and evidence identities
so an unrelated proof packaging change need not alter generated code.

An artifact initially contains typed assembly, local labels, symbolic relocations,
owned constants, alignment, frame information, imports and source mappings.
Later it can contain bytes. Compilation independence means independence of that
artifact: final addresses and linked bytes can change when other functions move.
The reproducible final-build contract additionally requires a deterministic
assembler/linker and normalized metadata. Debug paths need an explicit mapping.

Do not promise history-independent release binaries from a linker that preserves
holes or prior placement for fast incremental updates. A deterministic release
layout can be a separate mode. Constant pooling and identical-code folding must
also respect observable address identity.

## A small set of typed stages

```mermaid
flowchart LR
  P[Module planner] --> J[Frozen function job]
  J --> S[Checked SSA]
  S --> O[Optimized SSA]
  O --> M[Selected machine IR]
  M --> L[Located machine IR]
  L --> A[Final typed assembly]
  A --> F[Function artifact]
  F --> B[Deterministic module assembly and linking]
```

Use closed Lean datatypes, arrays and dense indices, with extrinsic well-formedness
predicates and checked constructors. Avoid requiring large dependent proofs in
every ordinary IR manipulation. Maintain a clear mathematical interpretation of
the efficient representation.

| Stage | Required boundary invariant |
|---|---|
| Checked SSA | Types, dominance, terminators and edge arguments are valid; imported references are resolved. |
| Optimized SSA | The same structural invariants, plus justified transformation conditions. |
| Selected machine IR | Legal instruction forms; virtual registers, fixed-register operands, clobbers, calling conventions and abstract stack slots are explicit. |
| Located machine IR | Physical locations and spill operations are assigned; edge copies preserve values; constraints and clobbers are respected. |
| Final typed assembly | Stack layout, alignment, prologue/epilogue and concrete offsets are resolved; unsupported pseudo-instructions are gone. |
| Artifact | Symbol references, local labels, relocations and metadata satisfy an explicit format contract. |

Keep the current stack-slot implementation as a baseline path while introducing
machine IR. This gives allocation work an independent comparison point. Translate
QBE phi nodes to internal block arguments at a checked boundary if that simplifies
the representation, and account for simultaneous edge assignments in the proof.

## MLIR ideas worth copying

**Pass isolation.** MLIR constrains passes from inspecting sibling operations and
using shared mutable state. Our function workers should go further: even readable
ancestor information must arrive in the frozen job. `IsolatedFromAbove` is not by
itself a complete dependency or reproducibility guarantee.
[Pass restrictions](https://mlir.llvm.org/docs/PassManagement/)

**Legality at every lowering boundary.** Each stage should specify what is legal
and reject residual unsupported operations. This resembles full dialect conversion,
rather than accepting a partially lowered program and hoping a later emitter
handles it. [Dialect conversion](https://mlir.llvm.org/docs/DialectConversion/)

**Effects and speculation are separate.** An operation that does not write memory
can still trap, diverge or otherwise be unsafe to speculate. Attach conservative
effect and speculation facts with semantic justification before enabling movement
or deletion. [Side effects and speculation](https://mlir.llvm.org/docs/Rationale/SideEffectsAndSpeculation/)

**Block arguments and operation descriptions.** Block arguments make incoming
values explicit at control-flow edges. Small declarative descriptions can generate
builders, printers, structural validators and instruction constraints. Keep the
semantic meaning independently auditable: generating an emitter and checker from
the same incorrect table does not establish ISA correctness.
[Blocks](https://mlir.llvm.org/docs/LangRef/#blocks),
[operation definitions](https://mlir.llvm.org/docs/DefiningDialects/Operations/)

**Reproducers and explicit pass recipes.** Serialize a failing function job, stage,
options and pass sequence. Analyses are tied to an IR revision and invalidated
unless their preservation is established. Treat correctness-critical analysis
results as proved conservative facts or validated certificates.

**Bounded rewriting.** Use deterministic priorities and work budgets. Correctness
must not depend on reaching a particular optimization normal form. Canonical
serialization is a separate requirement from MLIR-style canonicalization, which
is best effort. [Canonicalization](https://mlir.llvm.org/docs/Canonicalization/)

Defer a generic dialect plugin system, C++ MLIR dependency, tensor/polyhedral
passes, bufferization, GPU lowering and a general transform language until a
consumer demonstrates a need. A fixed, versioned pass recipe is sufficient now.

## CompCert should determine the correctness structure

Define source and target semantics before optimizing their translation. Use
small-step semantics for the specification, including machine-register aliases,
clobbers and stack accesses at the appropriate target stages. Use
fixed-width integers, explicit floating-point behavior, a specified memory model,
external-call events and precise treatment of traps, undefined behavior and
divergence. An interpreter with finite fuel is useful for testing; fuel exhaustion
is not a semantic account of divergence.
The interpreter should report execution-budget exhaustion as evaluation incomplete,
distinct from successful termination, a modeled trap or undefined behavior.

CompCert's block-and-offset memory model is a useful starting point, but adopting
it requires checking compatibility with QBE operations, ABI interactions and
pointer/integer conversions. C semantics cannot simply be substituted for QBE's
contract. [CompCert memory](https://compcert.org/doc/html/compcert.common.Memory.html)

Specify a simulation or refinement relation for each stage, including states,
memory and observable events. State precisely the assumptions under which defined
source behavior is preserved. Per-function theorems must quantify over compatible
external environments; composition must establish that the linked implementations
satisfy those contracts. Independent compilation alone does not prove safe linking.
[Compiler composition](https://compcert.org/doc/html/compcert.driver.Compiler.html),
[separate compilation](https://compcert.org/doc/html/compcert.common.Linking.html)

Direct proofs are attractive for local arithmetic rewrites, simple lowering and
frame transformations. For register allocation, permit an untrusted heuristic to
propose a result and require a proved checker to validate value flow through
moves, spills, reloads, joins and calls, as well as register aliases and constraints. Merely
checking that live ranges do not conflict is insufficient.
[CompCert allocation](https://compcert.org/doc/html/compcert.backend.Allocation.html),
[regalloc2's practical checker](https://github.com/bytecodealliance/regalloc2/blob/main/src/checker.rs)

Do not build a universal semantic-equivalence checker as a prerequisite. Choose
restricted transformations and certificate formats with tractable soundness
arguments. Offline SMT or synthesis tools may suggest rules; accept a rule only
with the required checked proof and side conditions. Solver scheduling should not
be a hidden input to ordinary production compilation.

Report verification scope explicitly:

- Structural validation establishes well-formedness, not semantic preservation.
- Determinism establishes repeatability, not correctness.
- A pass theorem must cover the implementation and input domain actually used.
- An assembly theorem leaves rendering, assembly, relocation and linking gaps.
- Native execution of a proved checker introduces a different trust boundary from
  replaying the relevant proof in the Lean kernel.

Record covered stages and assumptions with evidence tied to the precise artifact.
A self-reported receipt is not a proof. Audit axioms, `sorry`, unsafe execution and
external dependencies. Keep assemblers and linkers named in the trusted boundary
until their contributions are covered. Decoding generated bytes independently is
useful testing but does not by itself prove instruction semantics.

## What the current implementation changes about sequencing

The reviewed sibling repository already has a pure `AArch64.emitFunction`, a
restricted integer/control-flow implementation, validation and an executable
interpreter. It emits assembly strings and assigns stack slots to values. Its
small selection theorem relates abstract arithmetic specifications; it does not
prove complete frame, control-flow, rendered-assembly or machine-byte behavior.

The current roadmap puts precise semantics and most proof work in Phase 2, with
machine-IR improvements and reproducible function artifacts in Phase 3. Move their
foundations forward. This is a proposed sequencing change, not a claim that those
roadmap items have been implemented or approved in the sibling repository.

| Stage | Deliverable | Exit gate |
|---|---|---|
| 0 — Boundary and contracts | Frozen jobs, versioned canonical codecs, typed artifact skeleton, deterministic IDs, initial semantic contracts and lean-mlir reuse experiment. | The current subset compiles identically alone, reordered and under different worker schedules. No undeclared dependency reads. |
| 1 — Typed baseline | Route existing integer/CFG stack lowering through typed machine IR and final assembly. | Regression parity; structural validators; a scoped preservation result covering edge copies, frame behavior and control flow in the chosen target model. |
| 2 — Memory and interfaces | Memory/data, symbols, calls, floating point, variadics and ABI contracts, expanding target coverage incrementally. | Defined-behavior differential tests and ABI fixtures pass; semantics and certification gaps are recorded per feature. |
| 3 — SSA and optimizations | QBE SSA construction, analyses and optimization behavior; effects and a small proved rewrite library. | Every enabled optimization has a documented contract, meaningful tests and explicit proof/validation status. |
| 4 — Allocation | Liveness, spills, constraints and allocation proposals with value-flow validation. | Adversarial fixtures exercise joins, calls and spills; checker soundness covers the certified subset; failures are explicit. |
| 5 — Complete compatible release | Finish inventoried QBE 1.3 syntax, passes, targets and ABI support. | Full upstream suite and coverage accounting, differential fuzzing, pinned performance measurements; tested and certified coverage are reported separately. |
| 6 — Incremental execution | Parallel workers and content-addressed cache over the established job boundary. | Cold/warm and scheduling matrices yield identical artifacts; dependency changes invalidate exactly the required jobs; corrupt records are rejected. |
| 7 — Concrete encoding | Direct encoding and relocation/object support, beginning with the baseline target. | Encoding checks and target-semantics connection cover the claimed subset; assembler/linker assumptions remain explicit where applicable. |
| 8 — Composed certification | Open-function, lowering and artifact/linking results composed for a declared domain. | Exact supported-domain theorem and audited trust inventory; a standalone verification story for delivered artifacts. |
| 9 — Measured extensions | Consumer adapters, selected optimizations, richer debug information and optional specialization. | Workload-specific benefits and preserved correctness/reproducibility contracts. |

These are acceptance milestones, not a strict serial schedule. Cache work can
start after stage 0 and a stable artifact format; encoding can start after typed
target lowering stabilizes. Composition planning starts immediately, with its
covered domain growing throughout. Full QBE compatibility remains the first
complete product release; narrower verified milestones may precede it.

In existing roadmap terms: pull V01 and the isolation/serialization part of E03
forward; introduce the necessary typed-stage portion of E01 during P05; grow
V02–V05 alongside P05–P12. Defer the broader optimization and consumer work rather
than making it a prerequisite for compatibility.

## Qualification and measurement

Keep replayable test artifacts containing canonical input, target configuration,
compiler/toolchain identities, pass recipe, seed and observed mismatch. Add an
isolated pass runner and stage dump format before developing complex optimizations.

Proposed driver interfaces are:

```text
lcb replay function.job --stop-after allocation
lcb verify function.job function.artifact
```

A serialized job should suffice to reproduce compilation without the original
frontend, project directory or cache, using the identified compiler and target
configuration. Replay invokes the same pure pipeline as ordinary compilation;
verification reports its exact coverage and remaining trusted assumptions.

The reproducibility matrix should cover function order, unrelated edits, thread
count, scheduling, fresh process, cache state and normalized source paths. Test
that ABI/layout/inline-body changes invalidate relevant jobs while unrelated
implementation changes do not. Distinguish deterministic errors from operational
failures such as out-of-memory termination.

Compare the interpreter, baseline lowering, optimized lowering, pinned QBE and
independent reference implementations on their common defined domain. Exercise
ABI behavior with native fixtures. Mutation tests should demonstrate that checkers
reject broken edge copies, clobbers, spills, frame offsets and evidence bindings.
Tests support engineering confidence; they do not replace universal proofs.

Measure compile time, peak memory, validation time, generated-code size, execution
time and the fraction of work reused after representative edits. Publish workload,
hardware, flags, toolchain and uncertainty. Keep runtime helpers explicit: writing
the compiler in Lean does not imply linking Lean's runtime into generated programs.

## Community discussion research: evidence status

Reddit, Lobsters and Hacker News searches have located the discussions below.
Their substantive comments have not all been read and verified. They are a reading
list, not corroboration for architectural or performance claims. Dates, software
versions, firsthand experience and benchmark methodology need checking before any
conclusion is promoted into the design.

| Project | Discussion leads | Questions to investigate |
|---|---|---|
| QBE | [HN 1.3](https://news.ycombinator.com/item?id=48373442), [Lobsters](https://lobste.rs/s/j1sadh/qbe_compiler_backend), [Reddit](https://www.reddit.com/r/Compilers/comments/1vna7lp/anyone_using_qbe/) | Integration effort, ABI surprises, debug information, real workload code quality. |
| MIR | [HN](https://news.ycombinator.com/item?id=22098399), [Lobsters](https://lobste.rs/s/zaevwg/mir_lightweight_jit_compiler_based_on_mir), [Reddit](https://www.reddit.com/r/Compilers/comments/p32k1c/) | Interpreter/JIT usefulness, portability, benchmark comparability. |
| Stogov IR | [HN QBE discussion mentioning alternatives](https://news.ycombinator.com/item?id=48373442) | Dedicated discussions remain to be located; avoid treating passing comparisons as controlled measurements. |
| libFirm | [HN](https://news.ycombinator.com/item?id=10921976), [Lobsters](https://lobste.rs/s/ijhl0w/libfirm_compiler_backend), [Reddit](https://www.reddit.com/r/ProgrammingLanguages/comments/1e8qwis/) | Graph representation costs, optimization settings, maintenance and embedding experience. |
| CompCert | [HN testing discussion](https://news.ycombinator.com/item?id=36018300), [Lobsters](https://lobste.rs/s/qlrh1u/runtime_error_formally_verified), [Reddit](https://www.reddit.com/r/Compilers/comments/1h5tzv6/) | Which failures concern specifications, verified transformations or unverified tool boundaries? |
| Cranelift | [HN](https://news.ycombinator.com/item?id=39742692), [Lobsters broader backend comparison](https://lobste.rs/s/ftk8xs/10_20x_faster_llvm_o0_back_end), [Reddit](https://www.reddit.com/r/rust/comments/10h4gd5/) | Compile-time versus code-quality tradeoffs, ISLE experience and integration costs. |
| Zig | [HN incremental internals](https://news.ycombinator.com/item?id=49085666), [Lobsters rewrite experience](https://lobste.rs/s/axdfjx/how_our_rust_zig_rewrite_is_going), [Reddit incremental compilation](https://www.reddit.com/r/Compilers/comments/1v951kq/inside_zigs_incremental_compilation_mlugg/) | Dependency granularity, shared state, incremental linking and clean-build reproducibility. |
| MLIR | [HN](https://news.ycombinator.com/item?id=37157451), [Reddit learning experience](https://www.reddit.com/r/Compilers/comments/1pwabrz/starting_with_mlir_seems_impossible/) | Framework costs, learning curve and which contracts remain valuable without the framework. Lobsters coverage remains incomplete. |
| LLVM | [HN integration discussion](https://news.ycombinator.com/item?id=36538344), [Lobsters backend comparison](https://lobste.rs/s/ftk8xs/10_20x_faster_llvm_o0_back_end) | Build/API burden versus optimization and target coverage. Dedicated Reddit coverage remains incomplete. |

Community coverage of CakeML, Alive2, Sail and lean-mlir also remains incomplete.
No conclusion in this proposal depends on an unverified social comment. A future
revision should distinguish maintainer explanations, firsthand reports and
speculation, and cross-check design-relevant claims with primary sources.

## Immediate implementation slice

Start with the existing integer subset: freeze a function job, serialize it,
introduce typed target instructions around the stack baseline, and prove one
complete restricted path through control flow and frame handling. Add replay and
reproducibility tests at that boundary. This is small enough to review and exposes
the interfaces that memory, calls and allocation will need before those features
make the design difficult to change.
