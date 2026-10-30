# Bend And Roc: Evidence, Performance, Authority And Execution

Status: Research — broad considerations remain undecided; five narrow roadmap
clarifications were approved on 2026-10-25. No implementation is claimed here.

Date: 2026-10-24. Bend source inspected at
[`295e996574573e3a88827b1cf7045b437333c8f0`](https://github.com/bendlang/bend/tree/295e996574573e3a88827b1cf7045b437333c8f0).
Concrete HEAD during review:
[`25b99d1aa2ea54eb05b52a567c4c021859e5f48a`](https://github.com/unbalancedparentheses/concrete2/tree/25b99d1aa2ea54eb05b52a567c4c021859e5f48a).
The working tree contained unrelated changes; this was not a clean-build qualification.
Method: source/documentation review and analysis of downloaded video captions.
No comparative compiler, runtime, or GPU benchmarks were independently reproduced.

This note preserves the full Bend/Roc discussion, including corrections to initial
interpretations. The [roadmap](../../ROADMAP.md#the-current-execution-queue) remains
the only execution queue. Its final [open research questions](../../docs/project/ROADMAP_TASKS.md#open-research-questions-bend-and-roc)
record uncertainties while the owner is deciding, not planned features or tasks.
The 2026-10-25 review led to narrow amendments under [R-0215](../../docs/project/ROADMAP_TASKS.md#task-r-0215),
[R-0285](../../docs/project/ROADMAP_TASKS.md#task-r-0285), [R-0150](../../docs/project/ROADMAP_TASKS.md#task-r-0150),
[R-0486](../../docs/project/ROADMAP_TASKS.md#task-r-0486) and [R-0416](../../docs/project/ROADMAP_TASKS.md#task-r-0416).
Those amendments clarify existing work; they neither reorder the queue nor adopt
the rest of this register. Further decisions remain subject to Concrete's design
policy and workload evidence.

## 1. Concrete's Philosophy Is The Admission Filter

The governing references are [Identity](../../docs/project/PROJECT.md#identity),
[Why Concrete](../../docs/project/PROJECT.md#why-concrete),
[Design Policy](../../docs/project/PROJECT.md#design-policy),
[Decisions](../../docs/project/PROJECT.md#language-decisions),
[Anti-Features](../../docs/project/ANTI_FEATURES.md),
[Value Model](../../docs/archive/design-records/VALUE_MODEL.md), and
[Evidence Architecture](../../docs/verification/EVIDENCE_ARCHITECTURE.md).

Every possible adaptation below is conditional on these constraints:

- Concrete remains a systems language with explicit control flow, allocation,
  authority, cleanup, FFI, and trust boundaries; not a new proof assistant.
- Linear ownership, scoped second-class references, and explicit mutation remain
  the default. No implicit reference counting, tracing GC, copy-on-write, hidden
  destruction, or hidden allocation is proposed.
- Borrowed values do not acquire escaping lifetimes to accommodate an optimization.
  Scoped callbacks, owned values/views, and explicit raw-pointer boundaries remain
  the vocabulary.
- No hidden executor, automatic CPU/GPU migration, implicit parallelization, or
  new async execution semantics follows from the Bend comparison.
- Capabilities express allowed external authority. They do not establish purity,
  determinism, termination, absence of mutation, confinement of filesystem paths,
  or honesty of foreign implementations.
- Proof, compiler enforcement, testing, runtime checks, and trusted assumptions
  remain distinct. Source/model correspondence and generated-code correctness
  cannot be inferred from a theorem name or from the compiler being written in Lean.
- Reuse existing contracts, reports, task/receipt identities, and replay paths.
  No second evidence ledger, policy schema, or editor-only proof status.
- Optimize only while preserving specified arithmetic, traps, cleanup, ownership,
  and authority. Profiles must not silently change source semantics.

## 2. Sources And Scope

### Bend

- [Repository and README](https://github.com/bendlang/bend/tree/295e996574573e3a88827b1cf7045b437333c8f0): this is Bend 2, not the earlier Bend/HVM implementation.
- [BendAI](https://bend-lang.com/bendai): product claims/status inspected on the review date.
- [Guide](https://github.com/bendlang/bend/blob/295e996574573e3a88827b1cf7045b437333c8f0/guide/GUIDE.md), [CLI](https://github.com/bendlang/bend/blob/295e996574573e3a88827b1cf7045b437333c8f0/bend2/main.ts), and [agent instructions](https://github.com/bendlang/bend/blob/295e996574573e3a88827b1cf7045b437333c8f0/AGENTS.md).
- [Type checker](https://github.com/bendlang/bend/blob/295e996574573e3a88827b1cf7045b437333c8f0/bend2/bend.ts), [kernel translation](https://github.com/bendlang/bend/blob/295e996574573e3a88827b1cf7045b437333c8f0/bend2/safe.ts), and [Lean-written BendTT kernel/formalization](https://github.com/bendlang/bend/blob/295e996574573e3a88827b1cf7045b437333c8f0/bend2/bendtt.lean).
- [BendTT paper source](https://github.com/bendlang/bend/blob/295e996574573e3a88827b1cf7045b437333c8f0/bend2/docs/BendTT/main.typ) and [PDF](https://github.com/bendlang/bend/blob/295e996574573e3a88827b1cf7045b437333c8f0/paper/BendTT.pdf).
- [Compiler/runtime](https://github.com/bendlang/bend/blob/295e996574573e3a88827b1cf7045b437333c8f0/bend2/comp.ts), [BendRT paper source](https://github.com/bendlang/bend/blob/295e996574573e3a88827b1cf7045b437333c8f0/bend2/docs/BendRT/main.typ), [PDF](https://github.com/bendlang/bend/blob/295e996574573e3a88827b1cf7045b437333c8f0/paper/BendRT.pdf), and [GPU guide](https://github.com/bendlang/bend/blob/295e996574573e3a88827b1cf7045b437333c8f0/guide/SHADERS.md).
- [Checker benchmarks](https://github.com/bendlang/bend/tree/295e996574573e3a88827b1cf7045b437333c8f0/bench/checker), [runtime benchmarks](https://github.com/bendlang/bend/tree/295e996574573e3a88827b1cf7045b437333c8f0/bench/runtime), [measurement script](https://github.com/bendlang/bend/blob/295e996574573e3a88827b1cf7045b437333c8f0/bend2/docs/gen_pins.ts), and [performance gate](https://github.com/bendlang/bend/blob/295e996574573e3a88827b1cf7045b437333c8f0/gates/perf.ts).
- [Independent-check comparison gate](https://github.com/bendlang/bend/blob/295e996574573e3a88827b1cf7045b437333c8f0/gates/safe.ts), [repository/token-budget gate](https://github.com/bendlang/bend/blob/295e996574573e3a88827b1cf7045b437333c8f0/gates/repo.ts), and [proof evaluation corpus](https://github.com/bendlang/bend/tree/295e996574573e3a88827b1cf7045b437333c8f0/evals).
- [Invariant game demo](https://github.com/bendlang/bend/tree/295e996574573e3a88827b1cf7045b437333c8f0/demos/app_win_is_bug_2d) and [Apache-2.0 license](https://github.com/bendlang/bend/blob/295e996574573e3a88827b1cf7045b437333c8f0/LICENSE). Repository code and proprietary BendAI technology are different reuse questions; any actual copying needs a per-file notice/license review.

### Roc, Lean And Koka

- Richard Feldman, [How close can pure FP get to C?](https://rtfeldman.com/fp-vs-c), 2026-10-22.
- [Why Lean is faster than Rust](https://kim-em.github.io/blog/2026-7-24-why-lean-is-faster-than-rust/), 2026-07-24.
- Richard Feldman, [How Our Rust-to-Zig Rewrite is Going](https://rtfeldman.com/rust-to-zig), 2026-07-15.
- Roc [platforms](https://www.roc-lang.org/docs/main/langref/platforms/), [lists](https://www.roc-lang.org/docs/main/langref/lists/), [modules](https://www.roc-lang.org/docs/main/langref/modules/), [functional programming](https://www.roc-lang.org/functional), and [FAQ](https://www.roc-lang.org/faq). These are mutable documentation, accessed on the review date, not pinned implementations.
- [Perceus: Garbage Free Reference Counting with Reuse](https://www.microsoft.com/en-us/research/wp-content/uploads/2020/11/perceus-tr-v1.pdf).
- [FP2: Fully In-Place Functional Programming](https://www.microsoft.com/en-us/research/wp-content/uploads/2023/05/fbip.pdf) and [research overview](https://www.microsoft.com/en-us/research/blog/fp2-fully-in-place-functional-programming-provides-memory-reuse-for-pure-functional-programs/).
- Richard Feldman, [Outperforming Imperative with Pure Functional Languages](https://www.youtube.com/watch?v=vzfy4EKwG_Y), Strange Loop, uploaded 2021-10-20; duration 34:55. One requested video was analyzed. Videos embedded/linked in the articles were not separately watched or transcribed.

The original-English automatic captions were downloaded using `yt-dlp`, after an
initial English-track request received HTTP 429. Automatic captions sometimes
spell Roc as “rock” and misrecognize technical names. The retained local review
files were `/private/tmp/roc-concrete-review/vzfy4EKwG_Y.en-orig.vtt`,
`vzfy4EKwG_Y.info.json`, and `transcript.txt`. Those temporary paths are not durable
repository dependencies. The timestamped public links below preserve access;
the full third-party transcript is not vendored.

## 3. Bend's Requirements, Contracts And Proof Workflow

The [guide](https://github.com/bendlang/bend/blob/295e996574573e3a88827b1cf7045b437333c8f0/guide/GUIDE.md)
puts formal propositions in `LAWS.bend` and their proofs in `PROOF.bend`. Comments
explain intent; propositions are code. The [CLI import check](https://github.com/bendlang/bend/blob/295e996574573e3a88827b1cf7045b437333c8f0/bend2/main.ts#L1025-L1028)
requires `PROOF.bend` to import a sibling `LAWS.bend` only when that sibling exists.
Removing the laws file bypasses this particular check; other program checks still
apply. The convention does not preserve a frozen set of requirements or prevent
weakening them.

Concrete contracts usually attach pre/postconditions to a function. A law can
name a property spanning calls or traces. This is an organizational distinction,
not an intrinsic strength ordering: a contract on a replay function can express a
trace property too. For example, preserving total balance in one successful
transfer differs from preserving it across every valid transfer sequence; the
latter needs composition and appropriate failure-state assumptions.

The [game's laws](https://github.com/bendlang/bend/blob/295e996574573e3a88827b1cf7045b437333c8f0/demos/app_win_is_bug_2d/LAWS.bend)
quantify over move sequences, and its [proof file](https://github.com/bendlang/bend/blob/295e996574573e3a88827b1cf7045b437333c8f0/demos/app_win_is_bug_2d/PROOF.bend)
establishes invariants. A valid proof still requires the human to have stated the
right requirement and the implementation/model connection to be justified.

Possible Concrete considerations, using [Proof Workflow](../../docs/verification/PROOF_WORKFLOW.md)
and [Proof Patterns](../../examples/proof_patterns/README.md):

- A project-level inventory of required claims, checked by one discoverable entry
  point. Deleting, weakening, making unsupported, or leaving stale a required claim
  must not silently produce success. Receipt validity and policy acceptance remain separate.
- An agent repair mode that fixes implementation/proofs against fixed requirements;
  specification changes are a separate reviewable action, enforced outside agent prose.
- A version-matched installed guide with working examples, common diagnostics,
  supported evidence, and exact validation commands. The existing command catalog
  should own machine discovery; no stale parallel feature list.
- Reuse R-0486's planned bounded component and R-0215's mutation controls for a
  demonstration: invalid change, visible rejection, repair and scoped replay.
  The protocol-state-machine flagship is planned, not an available example.
  Its existing prerequisites still apply; do not create a fourth flagship.
- Difficulty-graded proof/repair tasks inspired by conservation, undo, protocol
  equivalence, and resource certificates in Bend's eval corpus. Hide reference
  solutions from evaluated agents; include cases where refusal is correct.
- A bounded proof-repair loop with exact obligations, candidate attempts, diagnostics,
  replay, time/token/cost accounting, and checks for claim weakening. Concrete's
  [synthesis document](../../docs/verification/PROOF_SYNTHESIS.md) is a design target,
  not evidence that a complete service exists.
- Comparison reports that account for agreement, unsafe assumptions, unsupported
  cases, mismatches, timeouts, and infrastructure errors. Never drop failed cases.

[BendAI](https://bend-lang.com/bendai) described its current product as a thin
harness around public models and its specialized models/SupGen as future work.
That is not a demonstrated proprietary prover to transplant or a reason to build
a paid service. Consider the measurable repair workflow independently of marketing.

## 4. What Bend's Checking-Speed Comparison Actually Measures

The [recorded M4 Max checker table](https://github.com/bendlang/bend/blob/295e996574573e3a88827b1cf7045b437333c8f0/bench/checker/_pin_/apple_m4_max.txt)
is stamped 2026-09-09 at `f655a39d`; its Isabelle column was retained from an older
run. Values below are upstream observations, not this review's measurements:

| Fixture | Bend | Lean | Agda | Rocq |
|---|---:|---:|---:|---:|
| `defs_12800` | 0.295 s | 36.177 s | >300 s | 5.985 s |
| `generics_3200` | 0.384 s | 19.214 s | >300 s | 6.038 s |
| `proofs_3200` | 0.834 s | >300 s | >300 s | 9.526 s |
| `trees_400` | 0.606 s | 1.335 s | 3.266 s | 1.339 s |

Rocq's proof-fixture result is a useful counterexample to treating the Lean/Agda
timeouts as a property of proof assistants generally. These are still different
pipelines and fixture encodings, not isolated measurements of language choice.

The [measurement script](https://github.com/bendlang/bend/blob/295e996574573e3a88827b1cf7045b437333c8f0/bend2/docs/gen_pins.ts)
runs source checking, not optimized native compilation. Bend's invocation omits
`--verdict`, so translation and independent BendTT kernel checking are excluded.
Its cold comparison uses a 300-second limit. The separate recurring performance
gate takes the minimum of three checker runs; these procedures must not be conflated.
`compute_1600` appears in the recurring Bend gate but has no Lean counterpart in
the inspected comparison corpus. A timeout is a lower bound, not an exact ratio.

The [proof fixture](https://github.com/bendlang/bend/tree/295e996574573e3a88827b1cf7045b437333c8f0/bench/checker/proofs_3200)
uses induction/simplification tactics in Lean and explicit proof terms in Bend.
Therefore these are user-facing pipeline comparisons, not identical terms checked
by isolated kernels. Exact causes of the observed ratios remain unprofiled.

The [BendTT design](https://github.com/bendlang/bend/blob/295e996574573e3a88827b1cf7045b437333c8f0/bend2/docs/BendTT/main.typ)
uses bidirectional checking, restricted variable matches, no kernel unification,
and no universe hierarchy. It separates typing from a live-code affinity/descent
check. Types may diverge; conversion has a budget. Its `Type : Type` design must
be understood together with its live/dead restrictions, not transplanted alone.
These choices simplify machinery at an expressiveness/ergonomics cost; they do
not establish that arbitrary dependent checking is linear-time. Bend lacks the
general inference and proof-search conveniences found in larger proof systems.

For Concrete, consider phase-specific timings: load/resolve, check, obligation
construction, Lean elaboration/tactics, kernel replay, evidence accounting,
emission, native compilation, and cold/warm incremental edits. Small explicit
generated obligations and reusable lemmas may avoid rediscovering known structure.
Investigate actual hot paths before replacing representations, adding caches, or
changing proof generation. Lean-hosted compilation, Lean proof elaboration, and
execution of emitted Concrete binaries are three different costs.

The independent [BendTT checker](https://github.com/bendlang/bend/blob/295e996574573e3a88827b1cf7045b437333c8f0/bend2/bendtt.lean)
is itself written in Lean. A faster front-end checker is not an argument against
Lean as Concrete's host or evidence foundation. Bend's translation/parser and
native backends have separate trust boundaries; a proved calculus is not a proved
end-to-end compiler.

## 5. Bend Runtime And GPU Findings

The [runtime design](https://github.com/bendlang/bend/blob/295e996574573e3a88827b1cf7045b437333c8f0/bend2/docs/BendRT/main.typ)
combines erasure, packed values, sharing/borrowing analysis, selective reference
counting, explicit continuations, and native optimization. Forks are explicit
parallel bindings; `f!(x)` requests device execution. Calls lower to segments;
tasks hold arguments and continuations, while joins collect results and count
unfinished children. The scheduler alternates frontier growth with sequential
work within each lane, avoiding unlimited tiny-task creation. It has no work
stealing and relies on balanced work. CPU and GPU computation do not overlap in
this design. The runtime is unverified; the paper's measured GPU lane is Metal,
not CUDA.

The [compiler source](https://github.com/bendlang/bend/blob/295e996574573e3a88827b1cf7045b437333c8f0/bend2/comp.ts)
contains Metal and CUDA `bend_dev` entries. A device dispatch loop executes
generated segments with explicit stacks rather than ordinary recursive device
calls. Child tasks write distinct result slots; an atomic countdown makes the
join runnable. GPU-reachable code is compiled into a device artifact; host code
loads or rebuilds it when needed. This is substantial machinery, not a consequence
of merely having linear types. Runtime failure uses a shared error protocol so
the host can report device errors.

The [GPU guide](https://github.com/bendlang/bend/blob/295e996574573e3a88827b1cf7045b437333c8f0/guide/SHADERS.md)
describes a 128-by-128 Metal lane grid, balanced fork trees ending in flat loops,
Apple shared memory, and CUDA managed-memory migration. The implementation/paper
also describe device-dependent CUDA sizing, so 16,384 lanes is not universal
hardware occupancy. Small tasks, divergence, uneven work, and page transfers can
erase gains. Without a GPU the marked computation can run on the CPU. Neither
fallback behavior nor a shared heap is automatically appropriate for Concrete.

The [runtime results](https://github.com/bendlang/bend/blob/295e996574573e3a88827b1cf7045b437333c8f0/bench/runtime/_pin_/apple_m4_max.txt)
include near-C sequential cases and substantial losses, plus cases where parallel
CPU execution beats GPU execution. Their dated pin is 2026-09-17 at `d0db7b3e`.
Comparing a parallel Bend run to a sequential C twin does not establish superior
sequential code generation. Native C/Metal/CUDA compilation is a separate cost;
fast source checking does not promise fast native builds.

### Why Concrete Or Rust Could Do This Too

Three ingredients are separate: safe independent work, device-compatible reachable
code, and device execution/memory machinery. Rust ownership/borrowing and Concrete
ownership/exclusive access both help with the first. Neither supplies the other
two automatically. Bend integrates all three in a restricted execution model.
The earlier implication that this is possible in Bend/Concrete but not Rust was
incorrect. Neither a capability-free function nor an owned wrapper alone proves
device suitability or uniqueness of all reachable memory.

### Conditional GPU Investigation For Concrete

Relevant foundations are [Execution Model](../../docs/platform/EXECUTION_MODEL.md),
[SSA](../../Concrete/IR/SSA.lean), [runtime builtins](../../Concrete/Backend/EmitBuiltins.lean),
and [concurrency research](../proof-evidence/concurrency-formal-model.md).
The inspected builtins use host allocation/abort services; a device backend needs
its own supported operation and runtime boundary.

A workload alone is insufficient: [R-0399](../../docs/project/ROADMAP_BACKLOG.md#task-r-0399)
keeps concurrency design-only until its v1/formal-model and pressure-test
prerequisites are met. Synchronous host completion still permits device-side
concurrency. Only after those prerequisites and a forcing workload, consider:

1. One backend and a restricted integer array map/reduce workload, with an ordinary
   explicit host API and synchronous completion first. No new `!` syntax is implied.
2. Preallocated owned device buffers, explicit transfer, layout/alignment checks,
   and no outstanding host access conflicting with device writes. Do not return
   escaping safe references to implement device views.
3. Transitive device eligibility: every reachable call, indirect target, operation,
   type, allocation, and foreign dependency must be supported or rejected.
4. Explicit target address spaces, ABI, numeric semantics, stack/resource limits,
   failure reporting, and partially written-output behavior. Device overflow,
   bounds, and allocation failures must preserve the declared contract.
5. Backend/toolchain/runtime artifact identity in evidence and caches; CPU/device
   differential tests remain tests, not proofs of the scheduler or binary.
6. End-to-end timing including compilation when relevant, transfer, launch,
   synchronization, memory consumption, and CPU baselines. Measure cold and warm
   runs separately and include irregular-work cases.
7. Explicit policy for unavailable devices and fallback, rather than silently
   changing resource behavior. No hidden executor or automatic migration.

General recursive tasks, a managed shared heap, work scheduling, and an allocator
would be later independent decisions, not prerequisites for this experiment.

## 6. Roc: Authority, FFI And Supply-Chain Boundaries

Roc's [platform architecture](https://www.roc-lang.org/docs/main/langref/platforms/)
puts I/O/native primitives and allocation behind one application-selected platform.
Packages can use that platform's exposed API, but cannot independently invent an
arbitrary foreign escape. The compiler/runtime/platform remain trusted. This is
not “Roc has no foreign integration.”

Concrete intentionally exposes [FFI and trusted boundaries](../../docs/platform/FFI.md).
It checks declared capabilities at calls; it cannot prove arbitrary native code
obeys them. Rejecting known effectful C symbols mislabeled with no effects does
not solve dishonest declarations for unknown symbols. `trusted` absorbs unsafe
implementation obligations, not declared external authority, and remains an
assumption rather than a proof.

The comparison suggests a possible application-controlled restriction on the
entire dependency closure: no new package-owned unsafe/trusted/native escape
outside a selected foundation. This is a policy option, not removal of systems
FFI, a mandatory Roc-style single platform, or a claim that Concrete already
provides this restriction. Merely forbidding the `Unsafe` capability at the call
site misses trusted wrappers. Unresolved edges must yield unknown/refusal.

Keep these claims separate:

| Claim | Additional questions |
|---|---|
| No external I/O authority | Are all call targets and foreign assumptions accounted for? |
| No additional native/unsafe trust | Can dependencies introduce trusted wrappers, native artifacts, constructors, or loading? |
| No caller-visible mutation | Are mutable parameters and reachable state excluded or explicitly modeled? |
| Deterministic behavior | Are targets, floating-point rules, foreign behavior, and ambient inputs controlled? |
| Termination/resource bounds | Are divergence, allocation, and adversarial workloads bounded? |
| Correct result | Is the relevant functional property actually established? |

A malicious pure dependency can still return a value that causes its caller to
make a bad decision, crash, or consume resources. Restricting direct authority does
not make data trustworthy or prevent compiler/runtime exploits. Resource isolation
and semantic validation are separate concerns.

### Build-Time Authority Is A Separate Boundary

The [package design](../packages-tooling/package-manager-design.md) rejects arbitrary
build hooks in its MVP, while [planned native interop](../../docs/platform/FFI_GLUE.md)
integrates foreign build tools. An application writing no scripts does not imply
that upstream build code never executes. Declarative metadata can still select
an executable upstream recipe.

Possible questions for later review: which build steps execute dependency code;
which files, credentials, environment variables, subprocesses, and network resources
they can access; which native components the application accepts; and what changes
on upgrade. Enforced isolation is stronger than a manifest merely listing inputs.
Hashes/provenance establish identity, not harmlessness. Read-only semantic analysis
should not fetch dependencies or execute their build steps. Any policy would need
hostile transitive fixtures, including trusted wrappers and build hooks, with a
positive case for the explicitly accepted foundation.

## 7. Roc's Opportunistic Mutation And Concrete's Ownership

Roc's [lists documentation](https://www.roc-lang.org/docs/main/langref/lists/)
describes updating unique storage in place and copying when the old value remains
shared. Retaining an old version can therefore change cost substantially.
[Perceus](https://www.microsoft.com/en-us/research/wp-content/uploads/2020/11/perceus-tr-v1.pdf)
connects precise reference counting with storage reuse; [FP2](https://www.microsoft.com/en-us/research/wp-content/uploads/2023/05/fbip.pdf)
explores stronger in-place guarantees under explicit conditions. This is useful
prior art, not a proposal to import reference counting into Concrete.

Concrete's [value model](../../docs/archive/design-records/VALUE_MODEL.md) already permits explicit
in-place mutation through owned values/exclusive scoped access. A possible API
comparison is borrowed transformation versus consuming transformation versus
explicit mutation. [Vec.map](../../std/src/vec.con) currently borrows input and
allocates a new vector; [Bytes](../../std/src/bytes.con) already exposes mutation
and capacity operations. That behavior is legitimate, not a bug to silently change.

Potential experiments:

- A same-type transform through `&mut Vec<T>`, or a consuming operation returning
  the owned vector, with allocation/capability behavior visible.
- Filtering/compaction and repeated transformations that preserve capacity.
- Preallocation where output size is known, measured against repeated growth.
- Scoped callback specialization when the target is known, preserving callback
  capabilities and non-escape rules.
- Allocation counts, bytes copied, peak live memory, and growth counts alongside
  wall time; representation changes must preserve semantics and cleanup.

Arbitrary `T -> U` buffer reuse needs size/alignment, element destruction, partial
initialization, and failure reasoning. Begin any investigation with an ordinary
same-type operation; do not smuggle in implicit cloning, copy-on-write, or changes
to borrow lifetimes. Any backend alias/noalias annotation needs the exact LLVM
contract and a sound derivation; source ownership is not a blanket license to add it.

## 8. The Video: Complete Topic Map And Concrete Implications

These are summaries of the 2021 talk, not claims about the current Roc compiler.
The requested video's captions were actually retrieved and read. Timestamp links
identify the discussion; the Concrete interpretations in the final column are ours.

| Segment | Talk topic | Possible Concrete consideration |
|---|---|---|
| [00:06–03:23](https://www.youtube.com/watch?v=vzfy4EKwG_Y&t=6s) | Elm-inspired usability; Ruby-to-Haskell service speedup attributed to overhead, not paradigm magic. | Evaluate real workloads and developer workflow together; never rank languages from one port. |
| [03:24–05:03](https://www.youtube.com/watch?v=vzfy4EKwG_Y&t=204s) | Execution models, LLVM optimization, and boxing costs. | Inspect representation and emitted code before introducing machinery. |
| [05:04–07:54](https://www.youtube.com/watch?v=vzfy4EKwG_Y&t=304s) | Allocation strategies, counting optimizations, Perceus, and optional arenas. | Preserve explicit ownership; investigate allocation traffic and workload-specific arenas only when measured. |
| [08:00–11:43](https://www.youtube.com/watch?v=vzfy4EKwG_Y&t=480s) | Managed effects, unboxed tasks/closures, defunctionalization, direct calls. | Keep capability-polymorphic callbacks cheap without importing Task syntax or a hidden executor. |
| [11:44–14:29](https://www.youtube.com/watch?v=vzfy4EKwG_Y&t=704s) | Mutation avoids copying; immutable snapshots can save rendering work. | Measure total work; do not erase snapshots or sharing that enable useful incremental reuse. |
| [14:30–21:14](https://www.youtube.com/watch?v=vzfy4EKwG_Y&t=870s) | Quicksort, unsafe alternatives, FFI, and benchmark sensitivity. | Compare equivalent algorithms, representations, checks, and output semantics. |
| [21:15–24:37](https://www.youtube.com/watch?v=vzfy4EKwG_Y&t=1275s) | Unobservable mutation and runtime uniqueness. | Prefer explicit consuming/mutating APIs over adding hidden reference counting. |
| [24:38–26:34](https://www.youtube.com/watch?v=vzfy4EKwG_Y&t=1478s) | Static uniqueness; brittle earlier analysis; Morphic-based replacement. | Investigate stable optimizations and missed-opportunity diagnostics; this is historical implementation evidence. |
| [26:35–30:49](https://www.youtube.com/watch?v=vzfy4EKwG_Y&t=1595s) | Quicksort combines unboxing, reuse, LLVM, and tail-call elimination. | Benchmark components; test stack behavior and cleanup before promising tail-call optimization. |
| [30:50–34:35](https://www.youtube.com/watch?v=vzfy4EKwG_Y&t=1850s) | Distinguishes baseline overhead, suitable workloads, and optimization; scaling caveats. | Require workload evidence rather than copying a language philosophy wholesale. |

Additional interpretation retained from the discussion:

- Managed effects need not impose a heap allocation and indirect call per operation.
  Concrete can investigate equivalent benefits through its existing function values,
  explicit contexts, known targets, and capability model. The old talk's Task
  explanation is not today's Roc effect syntax and is not proposed Concrete syntax.
- Immutability is not intrinsically a performance loss: avoiding repeated downstream
  work can outweigh snapshot costs. Compiler caches still require complete identities
  and invalidation; pointer identity alone is not evidence identity.
- Tail-call elimination has applicability conditions, including pending cleanup.
  Concrete's ordinary loops already provide an explicit alternative; no new recursive
  programming obligation follows from the quicksort example.
- Treat the talk's simplified descriptions of other languages and its small benchmark
  as illustrations. Safe local mutation can preserve a pure external interface;
  mutation itself is not a soundness failure. Bounds-check removal needs its own proof.
- Reference counting can still have expensive destruction cascades; absence of a
  tracing collector is not a general latency guarantee. Arenas also impose lifetime
  and peak-memory tradeoffs, so they remain workload-driven research.

## 9. The Compression Articles And Proof-Guided Optimization

[How close can pure FP get to C?](https://rtfeldman.com/fp-vs-c) reports median
roc-deflate slowdowns against libdeflate of 2.6% on M1 and 8.9% on Ryzen across
its tested compression cases. It describes avoiding sharing, hoisting checks,
compile-time tables, and deliberate wrapping arithmetic. These are author-reported
results on a particular corpus/hardware and non-streaming API, not a general
functional-versus-C ranking. The article contrasts memory safety, enforced purity,
restricted FFI/build scripts, and intended cross-target determinism with raw speed.

Possible Concrete investigations retain those distinctions: remove bounds/overflow
checks only when justified; use wrapping operations only when that is the intended
arithmetic; and consider compile-time tables only within the existing bounded,
explicit compile-time design, not arbitrary package-time execution. Cost and
semantics must remain visible. The article's incident examples were not independently
audited in this review and are not needed to justify the threat model.

[Why Lean is faster than Rust](https://kim-em.github.io/blog/2026-7-24-why-lean-is-faster-than-rust/)
describes proof-constrained AI tuning of lean-zip against miniz_oxide. It explicitly
notes higher memory use, slower decompression, native `@[extern]` trust gaps,
and faster competing implementations. Its title is not a claim that all Lean
programs beat Rust. Proofs support the specified properties; they do not establish
side-channel freedom or resource bounds automatically.

For a possible Concrete experiment, use an existing workload and freeze the
specification, optimize, replay evidence, run independent differential/conformance
tests, and measure runtime plus memory. A round-trip property alone does not prove
compatibility with every external implementation, compression quality, adversarial
resource behavior, or binary correctness. Keep task identity bound to the exact
subject/spec/dependency environment. Candidate search may be untrusted; accepted
evidence must replay without the model. Do not weaken assumptions to win a benchmark.

## 10. The Rust-To-Zig Article And Compiler Engineering

[The rewrite report](https://rtfeldman.com/rust-to-zig) discusses a substantial
architectural rewrite, lambda-set specialization, development hot loading, native
deployment, cross-compilation, allocation-free parsing patterns, build latency,
and compact indexed caches. It distinguishes memory corruption in generated code
from errors in the compiler process and describes arena/allocator preferences.
Its before/after timings and bug counts compare different codebases, versions,
exposure, and architectures; they do not isolate implementation language effects.

Concrete implications to consider, not adoption decisions:

- Preserve known callable targets and avoid unnecessary boxing where existing
  language semantics permit it. Do not import global inference or an entire
  lambda-set system just to copy Roc's implementation.
- Separate fast development feedback from optimized deployment costs. Hot loading
  is interesting background, not a proposed requirement or a reason to alter
  Concrete's explicit execution model.
- Investigate compact indexed/array-oriented representations only on measured hot
  paths. An index requires bounds and owner identity; integers are not automatically
  safe just because they replace pointers.
- A direct-load cache still needs schema/version, byte validation, target/endian/layout,
  provenance, and invalidation rules. “Zero parse” is not permission to trust cached bytes.
- Compiler implementation safety does not imply emitted-program correctness. This
  reinforces the existing [emitted-code validation research](../compiler/sparkle-emitted-code-validation.md).
- Allocator/arena control can fit explicit resource APIs, but does not justify
  hidden per-request allocation policies or implicit destruction.
- Retain deterministic cross-target build identities and output checks. Do not
  assume that a deterministic language specification proves compiler reproducibility.

No Lean-to-Zig/Rust/Concrete compiler rewrite is proposed. In particular, the
article's advocacy about unsafe Rust, Zig modes, or aggregate CVE counts is not
adopted as a comparative safety theorem.

## 11. Complete Consideration Register

This is an unordered inventory, not an implementation queue. “Possible evidence”
means evidence for a decision, not an additional release gate. The limited approved
amendments are identified at the top of this note; all broader questions remain open.

| ID | Consideration | Possible evidence / boundary | Existing context |
|---|---|---|---|
| C01 | Version-matched installed guide and agent onboarding | A non-author completes an existing example using installed guidance | [R-0397](../../docs/project/ROADMAP_TASKS.md#task-r-0397), [developer tooling](../packages-tooling/developer-tooling.md) |
| C02 | Required-claim inventory and one project verification entry | Missing/stale/deleted/weakened/unsupported claims stay visible; policy remains separate | [Evidence Architecture](../../docs/verification/EVIDENCE_ARCHITECTURE.md) |
| C03 | Fixed-spec agent repair workflow | Unauthorized specification/assumption changes are rejected | [Proof Synthesis](../../docs/verification/PROOF_SYNTHESIS.md) |
| C04 | Rejected-bug demo | Planned R-0486 component shows failure, repair and scoped replay after prerequisites | [current queue](../../ROADMAP.md#the-current-execution-queue) |
| C05 | Graded proof and repair corpus | Hidden solutions, refusal tasks, conservation/undo/protocol cases | [Proof Patterns](../../examples/proof_patterns/README.md) |
| C06 | Bounded synthesis/repair harness | Attempts, costs, failures, exact tasks, independent replay | [Proof Authoring And Automation](../../docs/project/ROADMAP_TASKS.md#area-proof-authoring-and-automation) |
| C07 | Complete independent-check accounting | Every claim classified, including timeout and infrastructure failure | [R-0353](../../docs/project/ROADMAP_TASKS.md#task-r-0353) |
| C08 | Compiler/checker/replay phase budgets | Cold/warm/incremental measurements; explicit proof terms versus tactic costs | [existing telemetry gate](../../scripts/tests/check_pipeline_telemetry.sh), [R-0150](../../docs/project/ROADMAP_TASKS.md#task-r-0150) |
| C09 | Small explicit generated obligations and reusable proof patterns | Lower authoring/replay cost with identical statements and trust | [ProofKit](../../Concrete/ProofKit.lean) |
| C10 | Compact guides and complexity discipline | Documentation stays useful without imposing arbitrary monolithic token caps | [Bend repo gate](https://github.com/bendlang/bend/blob/295e996574573e3a88827b1cf7045b437333c8f0/gates/repo.ts) |
| C11 | Transitive restricted dependency policy | Reject unapproved native/trusted/unsafe additions; accepted foundation explicit | [R-0440](../../docs/project/ROADMAP_TASKS.md#task-r-0440), [trusted-code policy](../language/trusted-code-policy.md) |
| C12 | Build-time authority controls | Dependency build execution is identified and isolated, including upgrades | [package design](../packages-tooling/package-manager-design.md), [FFI integration](../../docs/platform/FFI_GLUE.md) |
| C13 | Separate authority, mutation, purity, determinism, totality | No false inference from an empty capability set | [R-0487](../../docs/project/ROADMAP_TASKS.md#task-r-0487), [R-0488](../../docs/project/ROADMAP_TASKS.md#task-r-0488) |
| C14 | Same-type consuming/mutating collection transforms | Capacity reuse without hidden copy/allocation/cleanup changes | [Vec](../../std/src/vec.con), [Value Model](../../docs/archive/design-records/VALUE_MODEL.md) |
| C15 | Allocation/copy/peak-memory regression measurements | Detect extra growth, retained sharing, and abstraction overhead | [R-0416](../../docs/project/ROADMAP_TASKS.md#task-r-0416) |
| C16 | Ownership-aware lowering and callable specialization | Sound alias facts, scoped borrows, direct calls; no blanket noalias | [SSA](../../Concrete/IR/SSA.lean), [callable model](../../docs/archive/design-records/CALLABLE_VALUES_AND_CAPABILITIES.md) |
| C17 | Check hoisting, constant tables, intentional wrapping | Same bounds/trap/arithmetic semantics before and after | [Arithmetic Policy](../../docs/archive/design-records/ARITHMETIC_POLICY.md) |
| C18 | Unboxing, tail-call analysis, allocation reuse | Measure applicability and cleanup/stack behavior; ordinary loops remain available | [Execution Model](../../docs/platform/EXECUTION_MODEL.md) |
| C19 | Immutable snapshots and incremental reuse | Saved downstream work exceeds copying; exact cache identities retained | [compiler research](../compiler/COMPILER_PIPELINE_RESEARCH.md) |
| C20 | Proof-preserving optimization experiment | Fixed spec, replay, differential tests, runtime and memory measurements | [AI-assisted optimization](ai-assisted-optimization.md), [R-0482](../../docs/project/ROADMAP_TASKS.md#task-r-0482) |
| C21 | Emitted-code validation | Distinguish host compiler safety from native behavior | [R-0299](../../docs/project/ROADMAP_TASKS.md#task-r-0299), [R-0300](../../docs/project/ROADMAP_TASKS.md#task-r-0300) |
| C22 | Compact indexed data and validated caches | Profile first; owner/bounds/schema/target/corruption controls | [compiler research](../compiler/COMPILER_PIPELINE_RESEARCH.md) |
| C23 | Explicit allocator/arena experiments | Workload-specific lifetime and peak-memory benefit, no hidden policy | [R-0404](../../docs/project/ROADMAP_BACKLOG.md#task-r-0404) |
| C24 | Restricted GPU feasibility experiment | One array workload, explicit buffers/transfers, synchronous completion | [Execution Model](../../docs/platform/EXECUTION_MODEL.md), [R-0399](../../docs/project/ROADMAP_BACKLOG.md#task-r-0399) |
| C25 | Device eligibility, runtime/ABI/failure/evidence boundaries | Transitive rejection, target identity, output failure state, CPU/device tests | [SSA](../../Concrete/IR/SSA.lean), [backend builtins](../../Concrete/Backend/EmitBuiltins.lean) |
| C26 | Honest performance comparisons | Equivalent work/checks, cold/warm costs, transfer/launch costs, adverse cases | [comparative workloads](../workloads/comparative-program-suite.md) |
| C27 | Reproducible development/deployment artifacts | Separate build latency from runtime speed; no implicit hot-loading commitment | [package design](../packages-tooling/package-manager-design.md) |

## 12. Ideas Not Adopted By This Review

No new `law` syntax, file-name-based security guarantee, replacement kernel,
universe-free dependent type theory, compulsory pure-functional surface,
implicit reference counting/copy-on-write, single mandatory platform, hidden
effect runtime, or automatic GPU offload is proposed. Bend's affine dependent
calculus and GPU task cube remain research references. General recursive device
execution and shared managed heaps are substantially larger than an array-kernel
experiment. Rust can support the same broad GPU approach; ownership is a useful
foundation, not an exclusive capability of Bend or Concrete.

No performance headline establishes universal language superiority. A successful
proof does not prove the intended informal requirement, the compiler/backend,
foreign code, or all resource behavior. Bend's source-checking benchmark omits
independent verdict replay; Roc's 2021 video is not current compiler documentation;
BendAI's future features are not shipped evidence. No benchmark or advertised
security incident was reproduced here.

If any further consideration is promoted, identify its existing normative owner,
produce a forcing workload and bounded experiment, record the decision, and only
then update the active roadmap. This research record alone changes no language guarantee or implementation
priority; the five approved task clarifications are owned by the roadmap.


## 13. Review Corrections And Implementation Boundaries (2026-10-25)

A read-only Claude review prompted the corrections above. The review inspected
repository documentation/code and the pinned Bend clone; it did not execute the
proposed mutation experiment or independently recheck the Roc articles/video.
The findings are recorded here with durable code/owner links rather than depending
on a temporary review transcript.

- Requirement identity must be held by the experiment's verifier outside the
  candidate's writable scope. Re-pinning a changed contract is not preservation
  of its earlier requirement. The bounded experiment rejects identity changes;
  semantic implication/weakening analysis is a separate problem.
- Whether transitions to ineligible/trusted code evade a required-claim policy
  remains an untested hypothesis. [R-0215](../../docs/project/ROADMAP_TASKS.md#task-r-0215) owns the
  adversarial cases, including an unchanged-requirement positive control.
- Replaying without the model/network separates accepted evidence from candidate
  search. It does not establish checker implementation independence. The
  [evidence architecture](../../docs/verification/EVIDENCE_ARCHITECTURE.md#independence)
  distinguishes same-kernel replay, independent implementations and other axes.
  Source/model correspondence is another claim, not a consequence of either replay.
- Existing [pipeline telemetry](../../Concrete/Pipeline/Pipeline.lean) and its
  [gate](../../scripts/tests/check_pipeline_telemetry.sh) already cover front-end
  stages. The performance-fixture directory linked in the original review was a
  README scaffold, not an implemented benchmark corpus. R-0150 owns the added
  feedback-cost attribution; R-0416 owns workload-scoped performance evidence.
- Collection reuse is conditional on measured demand and instrumentation. Existing
  [Vec](../../std/src/vec.con) mutation and its trusted implementation must be
  considered before introducing APIs; trusted-code and cleanup costs are part of
  the comparison. This review does not establish a workload bottleneck.
- Build-time authority belongs with [R-0285](../../docs/project/ROADMAP_TASKS.md#task-r-0285)'s
  executing foreign-toolchain integration. Recording authority is not sandboxing;
  unsupported isolation claims must remain explicit trust assumptions.
- GPU feasibility remains behind R-0399, including device-side concurrency even
  for synchronous host APIs. No workload overrides that existing gate.

The seven final roadmap question groups cover C01–C27 without promoting the
register wholesale. They preserve the distinction between approved narrow task
amendments, future experiments and features that remain excluded by Concrete's
philosophy.
