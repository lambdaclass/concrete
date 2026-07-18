# Concrete Roadmap

This document is the active execution plan. It answers one question: **what
should happen next, in what order?** Historical phase detail lives in
[CHANGELOG.md](CHANGELOG.md); this file keeps only active work, future-relevant
constraints, and deferred tails with a real pull trigger.

## How To Read This Roadmap

### Active Work vs Historical Record

- **Active roadmap:** future work and pull-gated deferred work that can still
  affect Phase 7+ execution.
- **Historical record:** completed Phase 1-6 item bodies, implementation notes,
  and detailed gate transcripts live in [CHANGELOG.md](CHANGELOG.md) and the
  linked design docs.
- **Done items:** do not remain here as task text. Keep only a dependency or
  invariant that still constrains future work; put the completion record in
  the changelog.

### Definition Of Done

A roadmap item is done only when its behavior is protected by checked evidence:
positive fixtures, negative fixtures, regression fixtures, and a red-team or
mutation-style guard for the failure mode that would be most damaging. If a task
cannot yet have that gate, the item must say why and name the trigger that makes
it testable.

**When a fix addresses a bug CLASS, "done" means a gate that catches every
instance of the class, not only the reported one.** This project has repeatedly
fixed one instance — a decimal parser while `hex`/`bin`/`oct` still trapped, one
snapshot while sibling ledgers still drifted, one fixture main while the corpus
still failed — and had the siblings resurface a CI run later. The fix that holds
is a class-level gate (a grep/lint over the shape, or a corpus check), not a
per-instance patch.

**Large semantic migrations land in a worktree/branch and merge green — never
incrementally on `main`.** A change to a cross-cutting contract (e.g. the
main-return / exit-code model across the whole fixture corpus) done commit-by-
commit on `main` has produced multi-commit red streaks; isolate it, finish it to
green, and merge once.

### Pull-Gated Work

Deferred work stays in the phase where its trigger lives. Do not build machinery
because the roadmap can imagine it; build it when a workload, proof, failing gate,
or public API forces it.

### Where Execution Starts

Execution starts at the first `Task R-NNNN` heading and advances by file
position. The `R-NNNN` value is immutable identity, not priority: existing IDs
are never renumbered or reused, and a newly confirmed urgent defect receives the
next unused ID before being inserted at its first honest execution position.
There are no phase-local queues, priority overlays, or separate stop-the-line
lists. Phase headings are milestone labels only.
Phases 1–6E and completed Phase 7 foundations/workloads 1–8 are historical and
live in [CHANGELOG.md](CHANGELOG.md). Phase 7.5's QBE backend is specified but
has not started.

## Inherited Scheduling Constraints From Earlier Phases

These are not a second task queue. They constrain the stable-ID task
that owns each trigger; they remain here so moving completed phase bodies to the
changelog does not erase a still-relevant condition:

- **6C V1 hardening:** add remaining telemetry fields, pass hashes, and
  incremental-shadow manifest/edit coverage when a workload needs them.
- **ProofCore partial-def reduction:** moved to Phase 14, pulled only by
  preservation proofs.
- **CompilerDB / interned fact store:** moved to Phase 8.5, pulled by the real
  incremental driver and relational fact consumers.
- **Consume-then-break linear restoration:** after value `while...else` removal,
  consume-then-break on an outer linear remains rejected; add a narrow exemption
  only if a workload needs that control-flow shape.
- **Qualified names/import aliases, FFI surface, target-profile roots, target
  constants, lint/vet, doc, bench, trace/debug, eval/inspect, LSP:** future
  usability/tooling work should be pulled by Phase 7+ workloads or the later
  phase that owns the corresponding user surface.
- **Build-output convention:** move generated binaries/IR out of source dirs when
  the next build-UX touch makes it worthwhile.

## Cross-Cutting Decisions

### Global Sequence And Evidence Discipline

The roadmap has one authoritative sequence: headings named `Task R-NNNN`, read
in file order without restarting at phase boundaries. Stable IDs identify work;
they do not encode position. Phase headings are milestone labels, while
specifications, contracts, and decision indexes are non-executable context.
There are no parallel tracks. Completed work moves to
[CHANGELOG.md](CHANGELOG.md) under a `### Completed Task R-NNNN` heading so the
gate reserves that identity permanently; conditional work occupies the file
position where its trigger can first make it executable.

Every user-facing claim should have a replay command, checked report, gate, or
proof. Unsupported cases must fail closed and loud. A feature that passes only
because no one tried to break its gate is not done.

### No Semantically Dark Constructs

Every construct is `proved`, `enforced`, `reported`, `assumed`, or `trusted`.
Authority, allocation, trust, runtime failure, byte/text/path boundaries, and
proof class must remain visible in source or audit output. Do not hide them
behind inference-heavy abstractions, implicit conversions, ambient lookup,
metaprogramming, or broad convenience APIs.

### Capability And Authority Visibility

Capabilities are part of the language's audit story. Imports do not grant
authority. Trusted/Unsafe internals may exist, but public wrappers must expose
the domain capability (`File`, `Env`, `Time`, `Network`, `Process`, `Console`,
`Alloc`, `Unsafe`, etc.) in signatures and reports.

### Allocation Visibility

`with(Alloc)` is allocation authority; allocator values name allocator identity.
The Phase 7 allocator decision is two-tier: tier-1 defaults remain simple,
while allocator-specific `*_in` APIs use explicit allocator values when they are
pulled. Do not add an ambient implicit allocator.

### Scoped References, Not User-Managed Lifetimes

Safe references are second-class scoped access paths, not ordinary returned or
stored values. They may flow down into calls, callbacks, and borrow blocks, but
safe APIs should not return `&T` / `&mut T` or hide lifetime relationships in
data structures. This is the Hylo/Val-style value-semantics choice Concrete
adopts: users get in-place mutation, while the compiler manages the short-lived
borrow reasoning locally instead of exposing a Rust-style lifetime language.

API consequence: prefer operation APIs, owned values/views, handles/indices, or
scoped callbacks such as `with_value` / `with_value_mut`. Returning safe
references stays deeply deferred and evidence-gated; raw pointers are the
trusted/Unsafe escape hatch when low-level code must return an address.

### Compiler Pipeline Spine

Phase 6B established semantic truth and validation records; Phase 6C made the
pipeline observable/replayable; Phase 7.5 adds a real user-selectable QBE
backend whose required duties include independent differential validation
without upgrading evidence claims; Phase 8 validates the bet with real
examples and performs the scheduled architecture migrations; Phase 8.5 turns
the shadow graph into a persistent incremental driver
only after that validation. Later proof/backend/tooling work must not bypass
this order: semantic facts first, observability second, independent executable
cross-checking third, external validation fourth, incremental reuse fifth, and
independently checked evidence after that.

### Future Compiler-Pipeline Refactor Contract

The pipeline works, but several bug classes came from boundaries represented by
convention rather than types: raw module lists reused after different validation
stages, ordered first-match lookup tables standing in for scoped identity,
reports rescanning artifacts, duplicated CLI compile/link paths, and backend
emitters rediscovering program facts. Refactor these incrementally as real
consumers pull them—Phase 7.5 is the first forcing consumer—not as a flag-day
rewrite.

#### A. Make Successful Phase Boundaries Type-Safe

Introduce thin opaque wrappers whose constructors live with the pass that earns
the invariant. The intended ladder is:

```text
SourceInputs
  -> ParsedProgram
  -> ResolvedProgram
  -> CheckedProgram
  -> ElaboratedCore
  -> CanonicalCore
  -> MonomorphizedCore
  -> LoweredSSA
  -> ValidatedSSA
  -> CleanedSSA
  -> CodegenInput
```

Lean-like API sketch (the implementation should enforce opacity with module
boundaries/private constructors or an equivalent Lean mechanism; names may
adapt to the existing pipeline, but invariant ownership must not):

```lean
structure LoweredSSA where
  modules : List SModule

structure ValidatedSSA where
  modules : List SModule

structure CleanedSSA where
  modules : List SModule

def lower (mono : MonomorphizedCore) : Except Diagnostics LoweredSSA
def verifySSA (ssa : LoweredSSA) : Except Diagnostics ValidatedSSA
def cleanupSSA (ssa : ValidatedSSA) : Except Diagnostics CleanedSSA
def prepareCodegen (ssa : CleanedSSA) : Except Diagnostics CodegenInput
```

The wrappers are not proof theater: APIs that require verified SSA accept only
`ValidatedSSA`; emitters accept only `CodegenInput`; test helpers that
deliberately inspect invalid/raw SSA name that escape hatch explicitly. Do not add
`unsafeCastValidated`, public constructors, or ubiquitous `.modules` extraction
that immediately erases the boundary.

Example failure prevented: a new CLI path may no longer call cleanup/emission
on raw `SModule` output while silently skipping `SSAVerify`. The code should
fail to typecheck before a runtime gate is needed.

Gate: `scripts/tests/check_pipeline_types.sh` scans public signatures and runs
negative compile fixtures proving raw/lowered SSA cannot reach cleanup or
codegen, cleaned SSA cannot be fabricated outside its owning module, and the
normal file/build/test/debug paths traverse the complete typed ladder.

#### B. Give Passes One Observable Contract

Keep `Except Diagnostics` for fail-fast transformations where appropriate, but
standardize the metadata every pass can publish instead of rebuilding it in
CLI/report/debug code:

```lean
structure PassTrace where
  passName       : String
  inputHash      : String
  outputHash     : String
  elapsedMicros  : Nat
  counters       : List (String × Nat)
  changed        : Bool

structure PassFacts where
  schemaVersion  : Nat
  invariantNames : List String
  artifactKind   : String

structure PassSuccess (α : Type) where
  output : α
  facts  : PassFacts
  trace  : PassTrace
```

This does not require every pass to accumulate recoverable diagnostics or use
one giant generic monad. The contract is the observable success envelope:
diagnostics remain structured on failure; output, facts, trace, counters, and
fingerprints come from the pass once on success.

Before/after example:

```text
BEFORE: `--report verify`, debug-bundle, and `--trace-pipeline` each infer
        whether SSA verification ran by inspecting different output.
AFTER:  `verifySSA` publishes one `PassSuccess ValidatedSSA`; all three views
        render its versioned facts/trace without rerunning verification.
```

Gate: snapshot the pass sequence, schema versions, input/output hash chaining,
and counters for one success and one failure at every phase. Mutation tests must
make the trace/report/debug views fail together when a pass is skipped or its
artifact hash is swapped.

#### C. Derive Program Facts Once, With Stable Identity

Add one immutable, typed program-fact index before codegen. It should include:

```lean
structure FunctionId where
  moduleId : ModuleId
  localId  : Nat

structure CodegenFacts where
  functions       : Std.HashMap FunctionId FunctionFacts
  symbolOwners    : Std.HashMap LinkSymbol FunctionId
  moduleAliases   : Std.HashMap ModuleId AliasScope
  layouts         : Std.HashMap TypeId LayoutFacts
  strings         : Std.HashMap StringId StringFacts
  builtins        : Std.HashMap BuiltinId BuiltinCodegenFacts
  entryPoint      : Option FunctionId
  testEntries     : Array FunctionId
```

Use stable IDs for semantic identity and strings only for display/link symbols.
Ordered lists may preserve deterministic presentation order, but semantic lookup
must not mean “first equal bare name in program order.” Module-local aliases are
resolved in their owning scope before any program-wide fallback.

Bug-class examples this closes:

- bug 039: `std.env.get` cannot be rebound to `std.args.get` because both have
  distinct `FunctionId`s and aliases are keyed by importing `ModuleId`;
- duplicate basenames cannot collide merely because their generated strings
  match before qualification;
- builtin recognition uses `BuiltinId`, not a raw function name that user code
  can shadow;
- entry/test discovery is computed once and cannot differ between LLVM, QBE,
  `concrete test`, reports, and debug bundles.

Gate: generated programs permute module order, repeat local names/import aliases,
and create basename/link-symbol pressure. All permutations must produce the
same fact index hash, semantic observations, and deterministic emitted symbol
set. Inject duplicate semantic IDs and duplicate link symbols to prove the
builder rejects them with a diagnostic rather than selecting one.

#### D. Transform Once; Render Many Views

Compiler transformations own canonical artifacts and facts. Reports, audit,
queries, LSP, agent JSON, and debug bundles are views over those artifacts—not
partial compiler passes hidden behind output commands.

```text
canonical pass artifact
  + versioned facts
  + stable source identities
        |- diagnostics renderer
        |- `--report ...`
        |- audit JSON
        |- query API
        |- debug bundle
        `- LSP/agent views
```

Concrete example: layout is computed once into `LayoutFacts { size, alignment,
fieldOffsets, enumTag, payloadOffset, passMode }`. LLVM prints LLVM types from
it, QBE prints QBE aggregate declarations from it, `--report layout` renders it,
and the ABI gate serializes it. None independently walks `Ty` to rediscover
offsets.

Another example: proof/obligation status is read from the canonical obligation
ledger; `audit`, reports, diagnostics JSON, and editor output must not derive a
second status by scanning proof files.

Gate: an import-firewall/source-scan rejects report/query modules importing
transformation internals that are not declared artifact APIs. For selected
facts, mutate the canonical artifact and prove every view changes together;
mutate a renderer and prove compiler semantics do not change.

#### E. Centralize Semantic Legalization Ownership

Add a machine-readable table covering every Core/SSA operation and answering:

1. Where are source types and failure semantics fixed?
2. Where is the operation canonicalized?
3. Does SSA already express exact runtime semantics?
4. Is target-neutral legalization required?
5. What remains backend-specific instruction selection?
6. Which interpreter/external/mutation oracle covers it?

Example rows:

| Family | Exact semantics owned by | Shared legalization | Backend responsibility |
| --- | --- | --- | --- |
| checked `i32` add | integer-semantics facts | overflow result + trap edge | LLVM intrinsic/CFG; QBE explicit CFG/helper |
| signed right shift | typed Core/SSA signedness | normalized count policy | LLVM `ashr`; QBE signed word/long sequence |
| float-to-int cast | cast policy | finite/range guard + trap class | target conversion after guard |
| enum payload copy | layout + SSA copy intent | size/alignment/payload facts | independent LLVM/QBE memory operations |
| indirect call | resolved function type/caps | canonical signature/callee identity | target ABI call sequence |

Do not “single-source” backend instruction sequences: that would correlate LLVM
and QBE failures and weaken the oracle. Single-source language semantics and
facts; independently realize them in each backend.

Gate: generate the table from `SInst`, terminator, `Ty`, builtin, and trap
constructors. A new constructor fails CI until it names its semantic owner,
legalization class, interpreter status, LLVM/QBE status, and at least one test.

#### F. Finish Panic-To-Diagnostic Hygiene By Boundary

Sequence note: land 0d's split (LLVM spellings out of `Concrete/Check/Layout.lean`
into a backend-neutral `LayoutFacts` home) BEFORE converting the Layout panics —
converting them in the old location churns the same functions twice (review
2026-07-18).

The Layout conversion is the first slice, not the whole audit. Inventory and
remove compiler-owned panics/unreachable assumptions in:

- resolver and symbol/alias lookup;
- monomorphization specialization lookup/merge;
- Core canonicalization and CoreCheck environment lookup;
- lowering place/type/function lookup;
- SSA verification and cleanup rewrite assumptions;
- layout and aggregate field/tag/payload lookup;
- LLVM/QBE emission and builtin/runtime manifests;
- report/query handling of unknown/new constructors.

Internal invariant failures should report:

```text
error[internal:E9xxx]: codegen requested layout for unknown TypeId 41
  phase: prepare-codegen
  invariant: CODEGEN-TYPE-KNOWN
  function: app.parse_config
  source: src/main.con:27:14
  artifact: <debug-bundle path>
```

Do not turn impossible states into ordinary user errors. Classify them as
internal diagnostics, retain relevant artifacts, fail the command, and keep CI
leak gates rejecting `panic`, raw Lean exceptions, LLVM/QBE parser errors, or
host stack traces as the first diagnosis.

Gate: one controlled invariant-break fixture per boundary plus a static panic
inventory with an allowlist containing only process-fatal bootstrap/runtime
cases justified by owner and removal condition.

#### G. Make SSA Cleanup A Named, Testable Rule Pipeline

Replace opaque fixpoint behavior with an ordered registry of small rules while
preserving the existing semantics:

```lean
structure CleanupRule where
  name       : String
  apply      : SFunction -> CleanupResult
  invariants : List String

structure CleanupResult where
  function : SFunction
  changed  : Bool
  rewrites : Nat
```

Example rule sequence (adapt to the actual implementation): trivial-copy
forwarding, dead pure instruction removal, constant branch simplification,
unreachable-block removal, phi simplification, block merge, strength reduction.
Rules that require a fixpoint declare it; the driver records iteration and
rewrite counts and enforces a termination/fuel bound as an internal diagnostic.

Each rule needs:

- a minimal positive before/after golden;
- a negative fixture where it must not fire;
- verifier-before/verifier-after checks;
- interpreter/native observation preservation where executable;
- a mutation demonstrating the gate catches an unsound rewrite;
- deterministic output under block/function/module order perturbation.

Do not begin by rewriting all of `SSACleanup.lean`. Extract one existing rule at
a time with byte-identical cleaned SSA over the full corpus, then delete the old
branch in the same slice.

#### H. Unify All Compilation Entry Points

File compile, project build, `concrete run`, `concrete test`, emit-only modes,
and debug-bundle capture should configure one driver:

```text
prepare inputs
  -> frontend
  -> canonical Core
  -> monomorphize
  -> lower
  -> verify SSA
  -> cleanup SSA
  -> prepare codegen
  -> emit selected backend
  -> validate backend artifact
  -> assemble/codegen
  -> link
  -> optionally execute
```

Illustrative configuration:

```lean
structure CompileRequest where
  mode          : CompileMode       -- file | project | test | run | emitOnly
  backend       : BackendKind       -- llvm | qbe
  target        : TargetSpec
  optimization  : OptimizationMode
  retain        : ArtifactPolicy
  testFilter    : Option TestFilter
```

Modes may stop at a named boundary or substitute the test entry wrapper; they
must not reproduce the passes. `--emit-ssa` stops after cleaned SSA,
`--emit-qbe` stops after QBE validation, normal build links, and `run` executes
the linked artifact. Debug-bundle capture subscribes to artifacts already
produced by the driver rather than rerunning the pipeline.

Gate: a matrix covers file/project × normal/test/run/emit × LLVM/QBE-supported
subset. It verifies identical frontend/SSA hashes for equivalent inputs,
consistent diagnostics, artifact retention on each injected boundary failure,
and absence of duplicated direct emitter/tool invocations outside the driver.

#### I. Preserve Stable Source And Transformation Identity

Carry identity needed for diagnostics/evidence without attaching large source
objects to every instruction:

```lean
structure OriginId where
  sourceFile : SourceFileId
  nodeId     : SyntaxNodeId

structure TransformOrigin where
  primary    : OriginId
  family     : SemanticFamilyId
  parents    : SmallArray OriginId
```

Core/SSA operations that can trap, create obligations, cross capabilities,
materialize memory, or become backend calls retain an origin. Cleanup rewrites
preserve the primary origin and record combined parents where necessary. LLVM
and QBE runtime checks/debug bundles report the same source construct and
semantic family.

Example: a checked addition lowered into compare + branch + trap must keep one
`checked_add` family ID and source span through cleanup and both backends; a QBE
mismatch bundle should not point only to a generated temporary or `.qbe` line.

Gate: source-span/identity goldens for direct instructions, inlined helpers,
phi/block merges, aggregate copies, generated runtime checks, and
monomorphized functions. Moving a source file must not change semantic IDs that
are defined as position-independent; editing the owning construct must change
its freshness/fingerprint where required.

#### J. Generate Consumer Coverage From IR Constructors

Maintain a compiler-generated matrix, not a manually curated checklist:

```text
constructor | Check | CoreCheck | Interp | Lower | Verify | Cleanup
            | LLVM | QBE | reports | source identity | tests
```

“Handled” must distinguish `implemented`, `intentionally_noop`,
`rejected_before_here`, `runtime_helper`, `pending`, and `not_applicable`.
Catch-all pattern matches do not count as coverage unless the constructor is
explicitly classified.

Example: adding a new `SInst.atomicRmw` in the future should immediately fail
the matrix because interpreter semantics, capability/memory-model facts,
cleanup side-effect classification, LLVM, QBE, reports, and tests have no rows.
It must not silently inherit `_ => unchanged` in cleanup and `_ => panic` in an
emitter.

Gate: constructor extraction is fail-closed and mutation-tested by adding a
synthetic constructor in a fixture copy or removing one coverage row. Publish
the matrix through agent features/debug bundles so CI and external tooling use
the same inventory.

#### Scheduling Map And Definition Of Done

The long-term contract is scheduled, but it is not one prerequisite chain and
must not delay the Phase 7 exit. Implement each item as a separately green,
runnable slice in these phase-owned sequences:

- **Phase 7.5 backend sequence:** extract target-neutral `LayoutFacts`; define
   the typed deterministic QBE fragment model; establish the minimal immutable
   `CodegenInput`/backend-neutral driver required by the first `main -> 42`
   vertical slice; then grow the real QBE backend and generated capability
   matrix. Convert Layout panics after the ownership split. An LLVM fragment
   migration is optional and remains bug-027 profile/pull-gated—building typed
   QBE fragments does not itself fix LLVM's concatenation cost.
- **Phase 8 architecture sequence:** introduce stable semantic identities;
   build immutable indexed program facts; add thin typed pass-boundary wrappers;
   replace the temporary manifest scanner with compiler-derived interface
   facts under a comparison gate; then generate the complete consumer/coverage
   matrix from those facts. Alpha-renaming remains the completed fix for bug
   045; IDs prevent future name-as-identity failures rather than relabeling that
   fix as temporary.
- **Incremental consumers:** move reports/debug bundles to canonical pass
   artifacts, carry source/semantic identity into runtime checks and backend
   bundles, extract SSA cleanup rules one at a time, and finish remaining
   panic-to-internal-diagnostic boundaries as their owning facts stabilize.
- **Phase 15:** add translation validation and only then consider owning a
   target MIR/native encoder beyond the real QBE backend.

For every refactor slice:

- name the bug class or immediate consumer;
- preserve byte-identical LLVM/Core/SSA artifacts by default;
- if bytes change, explain the exact delta and prove behavior/evidence parity;
- run verifier-before/after and the relevant full/differential/fuzz gates;
- add a mutation or negative fixture that fails without the new invariant;
- delete the superseded path in the same slice or give it a dated retirement
  gate;
- record compile-time/memory impact and reject unbounded regressions;
- keep unrelated feature work out of the refactor commit.

The refactor program is complete when all production entry points traverse the
typed ladder and unified driver; semantic lookup uses stable identity; canonical
facts feed every view/backend; every constructor has fail-closed consumer
coverage; cleanup rules are named and mutation-sensitive; compiler-owned
invariant failures are structured diagnostics; LLVM remains green; and the QBE
backend consumes the same immutable codegen input without importing LLVM logic
while retaining its independent-oracle test duty.

Explicit non-goals: no wholesale rewrite, no universal pass monad, no dynamic
backend plugin framework, no new source language, no duplicate ZIR/AIR layer,
no target MIR/register allocator/object writer before a direct-native forcing
case, no report-specific compiler pipeline, and no claim that typed wrappers or
backend agreement constitute a proof of compiler correctness.

### Incremental / Certificate Trust Split

Cache hits are optimizations, not proofs, and cannot upgrade evidence classes.
Phase 8.5 starts as conservative cache reuse over successful artifacts. Phase 14
adds an independent Core certificate checker for named predicates; Phase 15
extends the statement only through supported backend-translation slices. Parser,
resolver, unproved source-to-Core correspondence, LLVM, linking, runtime, OS,
and hardware remain trusted until separately checked.

### External-Validation Gate

The back half of the roadmap depends on the bet that evidence-carrying source is
worth the discipline. Phase 8 supplies the first serious external-facing
workloads; the external-validation gate decides whether the project proceeds
with Phase 8.5 and the larger Phase 11-19 investment as planned, narrows the
bet, or redesigns the discipline.

### Early Risk Probes

Four verdict-deciding risks get narrow early probes:

- **External credibility:** deterministic `diff-caps` style authority-diff
  artifact before the full Phase 8 trial.
- **Proof cost:** Phase 9 proof-effort telemetry on the first real proof slice.
- **Backend soundness:** narrow Phase 15 translation-validation slice over a
  small supported subset.
- **Pipeline scale:** Phase 6C/Phase 8 shadow invalidation transcript before real
  artifact reuse.

### Floats And Proof Profiles

`f32`/`f64` are usable runtime types but outside `ProvableV1` unless a future
explicit float profile is selected. Float proofs require named IEEE semantics,
backend fast-math restrictions, and an honest trusted/checked primitive layer.

### Embedded / Freestanding Direction

Embedded and freestanding support remain later-phase work. Inline asm is
trusted/Unsafe; MMIO/volatile access needs explicit capabilities; `Device` is
reserved for real freestanding/MMIO code and should not be added before a
consumer exists.

### Target-Conditional Code

Prefer target/profile-selected source roots and modules in `Concrete.toml` over
in-source conditionals. If narrow `cfg` attributes are ever admitted, they must
be audit-visible target/profile facts, not hidden preprocessor state.

### Native Debug Info

DWARF/source-mapped crash traces matter once Concrete emits real binaries. This
is backend/tooling work owned by Phase 15+, not Phase 7 stdlib work.

### Contract / Spec Trust

Specs and contracts carry evidence classes too. The system must avoid vacuous
proofs, impossible preconditions, partial ghost/spec functions, and generic
proofs whose scope is unclear. Proof evidence is relative to the toolchain that
checked it and must detect drift.

### Stdlib Direction

The stdlib should be small, explicit, and auditable: Gleam/Roc-sized coherence,
Hare-style restrained systems modules, Zig-style allocator/authority
explicitness, Rust-style `Option`/`Result` and layering where useful, Ada/SPARK
evidence discipline, Go-style `Reader`/`Writer` simplicity, and Odin-style
pragmatic breadth only after workload pressure.

## Architecture Decision Index (Non-Executable Context)

This preserves the conclusions of the 2026-07-18 18-item architecture review.
It is not a task list and does not define execution order; every open conclusion
is implemented by one of the stable-ID tasks below. The unifying
rule: **`pub` is the ONLY visibility keyword — everything is module-private by
default, `pub` exports a declaration/field/variant, and exporting a type does
NOT export its representation.** No `private`/`sealed`/`opaque`/`transparent`.
Every decision below is carried by a numbered task; these topic labels do not
create ordering or an alternate queue.

### Stdlib Boundary Decisions
- **Construction rights:** enum-variant privacy is a separate scheduled slice:
   it must define
   construction, matching, imports, exhaustiveness with inaccessible variants,
   and migration diagnostics. Syntax (D2): keep `pub newtype N = u32` (NO
   parentheses); variant marker is `pub Ok { value: T }`. `pub` stays the only
   visibility word.
- **Borrow versus ownership:** split `BytesRaw` into `Slice<u8>` (borrow) and
   `BytesParts` (ownership transfer); a borrowed view and owned allocation parts
   must not share an ambiguous destruction contract. Inventory
   producers/consumers first, then split with destruction-counter tests.
- **One IO spine:** Files/streams/sockets/buffers on shared Reader/Writer;
   keep parser-oriented `read_all -> Bytes`, ADD `read_to_end -> Result<Bytes,_>`
   for error-preserving completion. Migrate vertically through workloads.
- **Boundary validation:** validate UTF-8 ONLY where an API promises text
    (CORRECTION: network/process expose `Bytes` by DEFAULT; validate only in
    explicit text adapters). args/env already validated.
- **Checked C-string boundary:** `to_cstr -> Result<CString, CStringError>`;
    interior NUL must not silently truncate. Land it in vertical slices:
    add checked conversion + CStringError, keep a narrow internal `_unchecked`,
    migrate all FFI sites, test interior-NUL/empty/alloc-fail/exactly-once,
    then remove/privatize the infallible public conversion.
- **Hosted adapters absorb unsafe:** callers request domain caps, not `Unsafe`;
    raw-pointer APIs stay private/explicitly-unsafe. Unblocked now that manifest
    facts come from the repaired balanced scanner. This is the
    raw-ptr⇒Unsafe gate (the hosted-adapter rule).

### Linearity Decisions
- **Centralized linear conservation:** one checker enforces exactly-once across
   branches/matches/returns/loops/callbacks/destructuring; consuming
   Option/Result combinators in Task R-0012 use this machinery, with no special-case rules
   (verified). Add an assertion fixture.
- **Trusted is not implicit linear copy:** local/binding copies are rejected (verified
   E0205; pin permanently), but raw-pointer dereference can still duplicate a
   non-Copy owned value: bug 046's `*(p : *mut K)` shape compiles today. Schedule
   a checker slice that rejects dereference-as-owned-duplication unless code uses
   an explicit audited ownership-transfer primitive such as `read_owned`; test
   both paths rather than claiming trusted linearity already holds generally.

### Backend-Separation Decisions
- **Backend-neutral LayoutFacts** (size/align/offsets/tags/ABI/legal scalar),
    separate from LLVM spelling — the opening slice; QBE is the forcing consumer.
- **Typed emitter fragments** (structured defs/blocks/instrs/terminators, rendered
    once); LLVM and QBE share rendering + neutral facts, NOT instruction
    selection. CORRECTION: this does NOT fix bug 027 in the LLVM emitter — that
    happens only if LLVM later adopts the fragment model (profile/pull-gated).
- **Structured diagnostics only** (panics→diagnostics) — the Layout panic
    conversion, reordered behind the LayoutFacts split (do the split once).
    Phase 7.5 order: LayoutFacts → QBE fragment model → QBE backend → Layout
    panic-to-diagnostic → optional LLVM fragment migration (only when pulled).

### Compiler-Architecture Decisions

Each migration is independently runnable, never a compiler-wide rewrite; the
compiler stays runnable and tested after every slice.
- **Stable semantic IDs:** alpha-renaming remains the demonstrated binder fix,
   while the larger 039–055 audit family justifies retiring name-as-identity:
   import/alias capture (039, 044, 055), binder/type/import tables keyed by text
   or first-match position (040–042, 045), indirect-call hijacking (050),
   generic-enum declaration/layout identity failure (051), and non-injective
   monomorphized names/layout lookup (054). Their narrow fixes remain necessary;
   stable IDs prevent the family from recurring at the next boundary. Bug 054's
   immediate collision/layout fix lands in Task R-0007; Phase 8 retires semantic
   identity-by-name across the pipeline.
- **Compiler-derived interface facts:** supersede the repaired manifest scanner.
   Keep the scanner until Task R-0117; the compiler artifact becomes
   authoritative, compared against the scanner during migration before deletion.
- **Typed pass boundaries:** ParsedProgram → … → VerifiedSSAProgram.
- **Immutable indexed program facts:** modules/defs/layouts/builtin identities/
    target) consumed by passes instead of re-scanning / first-match lookup.
- **Self-enforcing coverage matrix:** derive it from compiler facts.

Their execution order is expressed only by Tasks R-0114–R-0118: stable IDs → immutable
indexed facts → typed pass boundaries → compiler-derived interfaces → generated
coverage matrix.

### Slice Decision
- **Slices as the safe buffer interface:** `&Slice<u8>`/`&mut MutSlice<u8>` on
   hosted read/write; raw-pointer forms private) — the `&Bytes` net-I/O pull
   (currently 2nd ask).


The corresponding local/CI gate work is scheduled as a stable-ID task
below; this index carries no executable work.

## Phase 7: Standard Library And Core APIs

Goal: build the small standard library people need before real workloads,
packages, editor tooling, freestanding targets, and release work can be honest.

Done when: a normal C/Rust-style Concrete program can use documented core APIs
for errors, bytes/text/path, collections, formatting/parsing, I/O capabilities,
tests, and oracle helpers, with every public stdlib item carrying an evidence
class and authority/allocation story.

**Excellence exit contract.** Phase 7 is judged on *honesty, not breadth* — the
one stdlib where every public API's authority, allocation, failure, ownership,
and evidence are visible and gated, over a proven pure core. Breadth is copied
only after that holds. Four properties gate the phase, each backed by a check,
not prose:

- **Capability-complete.** Every hosted operation carries its exact domain
   capability — `fs`→`File`, `env`→`Env`, `time`→`Time`, `process`→`Process`,
   `net`→`Network`, `console`/`io`→`Console`. `trusted` internals may use
   `Unsafe`, but the public wrapper must re-expose the real cap (a `trusted`
   extern requires *no* capability, so authority collapses into `Unsafe` unless
   re-added by hand). The stdlib manifest gate fails if a hosted op is behind
   `Unsafe` alone.
- **One IO spine.** All output targets a single `io.Writer` (fn-pointer handle,
   authority at acquisition, no `dyn`), and there is one `File` type: `fmt`,
   files, console, `std.test` output, logs/progress, and future sockets are all
   `Writer` producers/consumers. No second sink, no duplicate `Writer`/`File`.
- **Linearity-real.** Collections destroy live non-`Copy` elements on
   `drop`/`clear`/`remove`/overwrite (not merely reclaim the buffer); no hidden
   `Clone`, no implicit move-out from indexed containers; deterministic map/set
   traversal. Until non-`Copy` element drop is wired, the limitation is recorded,
   not silent.
- **Self-enforcing + selectively proven.** The five-fact manifest (allocate / consume /
   fail / capability / proof-class), derived from compiler facts and gated
   (the stdlib manifest gate); the strict error rule (`Option` for absence, `Result` for
   recoverable domain/environment failure, trap/abort for invariants / OOM /
   bounds / arithmetic, explicit cross-module wrapping, no hidden `?` conversion
   web); and the stable, workload-pulled pure core (`option`/`result`, selected
   `bytes`/`numeric` helpers, and checked conversion boundaries) carrying Lean
   evidence where the API has stopped moving. Do not try to prove all stdlib
   APIs before workloads validate their usefulness; every API needs an evidence
   class, but only stable central pure APIs should be upgraded to `proved`.

These enforce a three-layer shape: a **pure core** (no capabilities, no
allocation unless explicit), an **alloc layer** (`with(Alloc)`, later explicit
allocator values), and a **hosted authority layer** (every API names its domain
cap). Iteration is internal only (`for_each`/`fold`/`_ctx`, optional
`Continue | Break`, no iterator trait or lazy adapter tower). Hold breadth —
broad crypto/compression/networking/threading and framework-style APIs — until
the four properties hold and a workload pulls it; the pure core is the priority,
not the periphery.

### Phase 7A: Confirmed Stdlib Boundary Corrections

This is a specification set for verified drift in the
stdlib that already exists. It is not a coordinated parity migration and does
not block unrelated workload-pulled Phase 7 slices. Only confirmed contract
violations and the narrow infrastructure needed to prevent their recurrence
are immediate; broader API shapes below remain owned by the workload that
demonstrates the need.

Canonical rule:

> A public ordinary stdlib API exposes typed values, ownership, its exact
> domain capability, allocation, and recoverable failure. Raw pointers,
> platform sentinels, C strings, ABI structs, and `Unsafe` stop at a narrow
> trusted adapter boundary. Explicitly raw modules and `_unchecked` functions
> are the visible exception.

Execution rule: land each confirmed correction as its own green vertical slice
with a discriminating fixture, mutation check, manifest/API-snapshot update,
and migrated consumers. Do not build Command/Rng/SystemTime/digest abstractions,
unify every handle, or redesign every error type merely for symmetry. Deferred
notes become work only when their named workload or failing gate pulls them.

### Global Task Sequence

Three implementation audits on 2026-07-18 reproduced nine numbered defects in
previously dark stdlib trusted bodies, Mono/Elab/SSACleanup boundaries, and the
reducer, plus a four-part proof-freshness class. The surface-gate task comes
first because it protects those fixes; the defects immediately follow it in the
one global file-order sequence.

### Task R-0001

**Objective:** Fix bug 051 — user generic-enum memory corruption User-defined generic enums retain one name/declaration while Lower uses instantiation-specific payload offsets, so mixed layouts can write beyond the emitted aggregate.

   Immediate containment: reject every unsupported user generic-enum
   instantiation before Lower/codegen with a stable diagnostic. Root fix:
   monomorphize enums injectively per canonical type arguments (preferred), or
   prove one program-wide union representation sound for every instantiation.
   Gate at least two differently sized/aligned payload instantiations, nested
   enums, arrays/structs, cross-module use, interpreter/LLVM/QBE where supported,
   layout write-within-declaration verification, and a mutation removing the
   containment/layout check.
### Task R-0002

**Objective:** Fix bug 050 — indirect function-pointer call hijack A local callable value whose text matches a generic/global function is rewritten by Mono into a direct specialization. Add a distinct resolved Core/Mono call form whose callee is a value identity, never a string looked up in the global fn/alias maps; preserve it through Lower, SSA, interpreter, LLVM, QBE, reports, and source identities. Gate the `pick` 42-vs-21 wrong-code witness, std.io local callback names such as `f`, generic/non-generic collisions, renamed imports, and a mutation that routes indirect calls through direct-name resolution.

### Task R-0003

**Objective:** Fix bugs 047 + 048 — one HashMap probe/occupancy invariant slice Insertion must remember the first tombstone but continue to a live equal key or a bounded chain end; lookup must inspect at most `cap` slots; load policy tracks occupied slots (`live + tombstones`) and rehashes/cleans tombstones before no empty slot can wedge a miss. Maintain explicit `len`, tombstone/occupied, and capacity invariants through insert/remove/clear/grow. Gate constant-hash overwrite-after-tombstone (`len == 2`, one remove eliminates the key), the zero-empty missing lookup under a watchdog, 10k+ churn, full-table misses,

   grow/clear/reuse, Copy and linear key/value destruction, interpreter/native
   agreement, a reference-map oracle, and independent mutations of remembered-
   tombstone, bounded-probe, and occupancy accounting logic.
### Task R-0004

**Objective:** Fix proof-subject freshness and fail closed Before preserving any `proved` claim, treat the following as one evidence- integrity defect class and file individually numbered ledger entries/reproducers before implementation:

   - a source `#[proof_by]` without `#[proof_fingerprint]` currently compares a
     synthesized current fingerprint with itself and can remain proved forever;
   - `bodyFingerprint` hashes statements but omits parameters, return type,
     generics/bounds, capabilities, and other signature facts;
   - `#[requires]`/`#[ensures]` and selected spec/proof identity are outside the
     body hash; and
   - stale dependency propagation considers direct callees rather than the
     transitive proof dependency closure.

   Immediate containment: an in-source proof link without a stored, validated
   proof-subject digest is `missing`/`unbound` (or `needs_recheck`), never
   `proved`; release/verified profiles fail closed. Replace body-only freshness
   with a versioned canonical `ProofSubjectDigest` covering qualified semantic
   identity, full typed signature and generic constraints, capabilities,
   normalized body, requires/ensures/invariants, selected spec and theorem
   identity, extraction/schema version, and dependency root. Compute dependency
   freshness transitively using a deterministic SCC/Merkle root so recursion is
   finite and a deep callee edit stales every dependent claim without making
   source location/alpha-renaming noise semantic. Gate each omitted component,
   missing/malformed digest, false postcondition, `i32 -> u32` signature edit,
   capability/generic edit, theorem/spec swap, direct and multi-hop dependency
   drift, recursive SCCs, comments/formatting/alpha-renaming stability, and a
   mutation proving no old body-only path can emit `proved`.
### Task R-0005

**Objective:** Fix bug 053 — DCE deletes checked integer negation Centralize a generated operation-semantics/trap inventory consumed by folding, DCE, interpreter, LLVM, QBE, fuzz generation, and the capability matrix. Until then, mark integer `.unaryOp .neg` side-effecting unless it is proved non-trapping;

   float neg remains pure. Gate discarded MIN negation for every fixed/native
   signed width, non-MIN removal where valid, optimized/unoptimized/interpreter
   agreement, and mutations omitting every trapping unary/binary constructor.
Bug 048's observable hang is closed only by task 3; a timeout merely detects the
defect and is not the fix. Keep its denial-of-service/watchdog observation
separate from bug 047's corruption observation inside the shared gate.

### Task R-0006

**Objective:** Fix bug 052 — array element destruction becomes a no-op Immediate containment: reject `T: Destroy` for arrays/unnamed element types whose drop glue cannot be resolved; never synthesize an empty destroy function from “name lookup missed.” Root fix: structural, type-directed drop-glue identity for arrays and nested aggregates, independent of `tyName`. Gate destructor counters for arrays of linear values in Vec/Deque/heap containers, nesting, partial construction/failure, clear/remove/drop, exactly-once behavior, and a mutation replacing structural glue with the old no-op fallback.

### Task R-0007

**Objective:** Fix bug 054 — non-injective monomorphized names First fail closed on every generated/user symbol or type-name collision. Then separate semantic `TypeId`/`FunctionId` from link/display symbols and use an injective, versioned mangling encoding that user identifiers cannot forge. Layout lookup is by identity and missing fields diagnose instead of returning a past-end offset. Gate ambiguous type-argument boundaries, user names resembling specializations, module/basename pressure, deterministic symbols, and collision mutations.

### Task R-0008

**Objective:** Fix bug 055 — sibling renamed import emits an undefined callee Resolve an import once to canonical definition identity and carry that identity through Mono/codegen; do not repair only one alias-string orientation. Gate plain and generic sibling imports, qualified/unqualified and renamed forms, duplicate basenames, module-order permutations, and undefined-symbol failure injection. This is a rejected-valid-program bug unless a wrong-code witness appears.

### Task R-0009

**Objective:** Fix bug 049 — vacuous `reduce --predicate crash` Remove the predicate from help/CLI immediately unless the same slice evaluates candidates in an isolated subprocess with bounded time/memory/output and preserves the original crash boundary/class. Parse success is never a crash predicate.

    The reducer must verify the final candidate still satisfies the predicate,
    report stage/error-class drift, and say explicitly when a multi-module
    input has no supported reductions. Gate a healthy program (must not reduce
    as crash), signal/tool/compiler/runtime crash classes, timeout, changed
    failure class, empty candidate, and reducer self-failure.

### Task R-0010

**Objective:** Make the bug-corpus truth gate honest Replace the current skip-based audit summary with an explicit per-bug state:


```text
fixed_with_regression
open_with_reproducer
documented_no_fixture
missing
```

Only `fixed_with_regression` contributes to “regression coverage.” An open bug
requires a checked-in minimal reproducer plus exact expected bad observation,
watchdog/resource limits where needed, owner/priority/containment, and replay
command; it remains red or explicitly quarantined and can never be summarized
as covered/fixed. Mutation-sensitive class gates replace individual reproducers
when the fix lands. The reverse inventory fails on undocumented `bug_*.con`
files and on numbered bug docs absent from the state table.

**Publication discipline.** A milestone is not “pushed everywhere” until the
same intended tip and bug/roadmap artifacts are present on every declared
remote and required CI conclusions are recorded. Remote parity is process
state, not a compiler bug; report it separately and do not let a green origin
implicitly describe a lagging mirror.

### Task R-0011

**Objective:** Finish construction rights with private-by-default enum variants Struct-field privacy and direct-newtype construction are historical milestones, recorded in the changelog and `docs/CONSTRUCTION_RIGHTS.md`. Finish the same one-keyword model for the still-open construction paths: `pub` remains the only visibility word; exporting a type never implicitly exports its variants or raw representation.


- `pub enum T` exports the type name. A variant is externally constructible and
  matchable only when marked `pub`, for example `pub Ok { value: T }`. The
  marker governs the whole variant payload in v1; do not add payload-field
  visibility or a second `private`/`sealed`/`opaque` vocabulary.
- Preserve the existing newtype spelling (`pub newtype N = u32`) and make raw
  payload projection/unwrap obey the same defining-module rule as construction.
  Public checked constructors and accessors are the ordinary cross-module path.
- Representation privilege follows the exact defining module. External impls,
  parent/child/sibling modules, `trusted`, `Unsafe`, direct/renamed/wildcard
  imports, and umbrella re-exports do not widen variant or constructor access.
- Define exhaustiveness before migration: a client unable to see all variants
  must use a wildcard, and diagnostics must not reveal inaccessible payload
  structure or synthesize private patterns.
- Public interfaces may not transitively leak a module-private type through a
  public variant, callback, generic argument, capability signature, alias, or
  nested composite. ABI/layout knowledge remains separate from source access.
- Compiler-generated Copy/Destroy/equality/formatting/serialization/layout,
  manifests, debug views, and backend glue may inspect representation without
  exposing it. Visibility changes must update interface hashes, API snapshots,
  manifests, dependency invalidation, and docs.
- Mechanically mark intentionally public stdlib variants, audit invariant and
  state-machine enums rather than exporting them reflexively, and apply the
  same result in Resolve, Check, Elab, projects, interpreter, LLVM, and QBE
  artifacts.

Acceptance: positive public construction/matching and negative private
construction/matching fixtures through direct, renamed, wildcard, and re-export
paths; wildcard/exhaustiveness and stable diagnostic/span fixtures; external-
impl and `trusted` non-bypass tests; interface-leak tests; interpreter/compiled
parity; API/manifest snapshots; and mutations that accidentally export a
variant or omit one enforcement path.

### Task R-0012

**Objective:** Generalize workload-pulled Option/Result combinators for linear payloads This is an ergonomics/ownership blocker, not a current soundness defect, but workloads already pay the manual-match tax. Do not merely delete Copy bounds: prove arm-wise conservation and explicitly dispose of every unused owned default/error.


- Reuse the checker's existing branch-join, divergence, return-path, and linear-
  conservation machinery. Combinators are ordinary functions and must not gain
  callback- or Option/Result-specific ownership exceptions.

- Support at least `Option<String>.map/and_then` and
  `Result<String,E>.map/and_then/map_err` where each present payload is consumed
  or passed through exactly once.
- Design `unwrap_or`, `ok`, `err`, and `ok_or` separately because the unused
  default or opposite payload may require `Destroy`; do not hide disposal
  behind Clone, leaks, or broad Copy bounds.
- Gate transformed, passed-through, short-circuit, callback-failure where
  expressible, and unused-payload paths with destructor counters; add negative
  fixtures for leak/double-consume callbacks and open match arms.
- Require interpreter/LLVM agreement, proof fingerprint/spec updates, and
  representative kernel proofs to remain alpha-invariant after generalization.

### Task R-0013

**Objective:** Close confirmed boundary correctness holes

1. Validate UTF-8 in `std.fs.read_to_string`, just as `env::get`, argv, and
   `Bytes::to_string` do. Prefer `Result<String, ReadTextError>` with distinct
   filesystem and `InvalidUtf8` cases. A file containing invalid bytes must
   remain available through `read_file -> Bytes` and must never become a
   contract-violating `String`.
2. Replace unchecked public C-string conversion at hosted boundaries with an
   owning checked `CStr`/`CString` adapter that rejects interior NUL and always
   terminates. Translate `InteriorNul` into `FsError`, `EnvError`, `NetError`,
   or `ProcessError` at the owning boundary. Keep raw conversion private or
   explicitly `_unchecked`. Gate all 13 C-string FFI consumers with empty,
   exact-capacity, embedded-NUL, invalid-UTF-8 where relevant, and normal cases.
3. Audit every external return value before constructing a typed success, but
   promote a site only with a concrete failure witness or a normative contract
   that already requires propagation:
   `fseek`/`ftell`/`fread`/`fwrite`/`fclose`/`fflush`, `clock_gettime`,
   `nanosleep`, socket operations, environment mutation, and process calls.
   Negative sizes must never cast to huge allocation sizes; zeroed FFI storage
   must never be returned as a fabricated `Instant` or address after failure.
4. Correct two already-ratified hosted contracts: `env.set`/`env.unset` return a
   typed environmental failure instead of unconditional Unit, and `args::get`
   no longer maps both out-of-range and invalid OS bytes to the valid empty
   String. Preserve a simple UTF-8 accessor; add a raw-bytes argv API only when
   a workload actually needs non-UTF-8 arguments.
5. Make `random_range(lo, hi)` enforce its documented half-open-range
   precondition by trapping when `lo >= hi`; the current `hi == lo -> lo`
   special case contradicts the bucket-2 invariant policy. Pin equality,
   reversed bounds, ordinary ranges, and overflow edges. A recoverable
   `checked_range`/`try_range` remains a separate workload-pulled API, as does
   any explicit Rng/system-entropy redesign.

Gate: `scripts/tests/check_stdlib_boundary_correctness.sh`, mutation-tested at
each discarded-error/unchecked-conversion site. Add a source inventory that
fails when a hosted module introduces unchecked `String` construction from
external bytes or an ignored classified FFI result without an explicit
justification row.

Boundary rule: validate UTF-8 only when an API promises text. File, process,
environment, argument, and network byte surfaces preserve `Bytes`; explicit
String/Text adapters validate and return the declared absence/error shape.
Never force arbitrary process output, argv bytes, paths, or network payloads
through UTF-8 merely to make adjacent APIs symmetric.

`std.io.read_all` is explicitly not a confirmed hole. Its workload-2 parser
contract intentionally ends the drain on a Reader error and returns partial
bytes so the parser reports a short/invalid payload. Preserve that behavior and
its documentation. Revisit only when a socket-to-Reader workload demonstrates
that EOF, partial data, and mid-stream transport failure need a different API;
then choose an explicit split such as fallible `read_to_end` versus a named
partial-read result rather than retroactively calling the existing contract a
bug.

### Task R-0014

**Objective:** Put Unsafe behind domain-capability wrappers and close trusted dereference duplication Split hosted modules into ordinary public wrappers and private trusted/raw adapters:


```text
public typed API: with(File | Env | Time | Network | Process | Console | Random)
        -> private trusted adapter: with(Unsafe), pointers, ABI/platform values
        -> std.libc/raw platform declarations
```

Ordinary `File::open`, `env::get`, `Instant::now`, `TcpStream::connect`,
`TcpListener::accept`, `Child::wait`, and random acquisition must not require
callers to add `Unsafe`. Allocation remains visible when the returned operation
allocates. Keep `std.alloc`, `std.ptr`, raw buffer constructors, FFI helpers,
and explicitly named `_raw`/`_unchecked` operations capability-visible and out
of the ordinary prelude.

Building on the repaired manifest derivation, extend the stdlib manifest with
`public_safety = safe | raw | unchecked`
and an owning trusted-boundary id. Start by migrating `std.net`, where compiled
workloads already exercise the boundary and `std.io`/`std.fmt` demonstrate that
trusted adapters can absorb `Unsafe` today. Ratchet the remaining reviewed
legacy rows with an owner and workload trigger; fail CI on any new ordinary
hosted API that exposes `Unsafe`, a raw pointer, or a platform sentinel. Raw
allowlist entries require a narrow purpose and no safer claimed surface. Audit
output shows both the public domain capability and the underlying trusted
assumption.

`trusted` may authorize raw implementation operations, but it is not intended
to suspend linearity. The implemented guarantee is currently narrower: E0205
(or its stable successor) rejects ordinary local/binding duplication inside
trusted functions/impls, while bug 046 proved that dereferencing
`*(p : *mut K)` can still duplicate a non-Copy owned value. Pin the working
local-copy path, and schedule a checker slice for the dereference path. Its
negative fixture must include the bug-046 shape; taking ownership through a raw
pointer is accepted only through an explicit audited primitive such as
`read_owned` whose contract consumes/invalidates the source ownership. Add
mutations for both local-copy and dereference paths. Until that slice lands,
reports describe trusted pointer ownership as an open assumption rather than an
enforced linearity property.

The raw-pointer rule is semantic, not decorative: a public operation
that accepts caller-controlled memory must take a provenance-safe Slice, be
private, or be explicitly raw/`_unchecked` with `Unsafe`. Inventory and migrate
`Vec::get_mut`, String/Text raw constructors, numeric cursor/writer `from_raw`,
IO `read`/`write_raw`, test sink pointers, and similar signatures. Conversely,
do not make callers request `Unsafe` merely because HashMap/HashSet or another
trusted implementation manipulates its own private storage; normalize the
current set/map capability mismatch according to public authority.

### Task R-0015

**Objective:** Converge File and Reader/Writer when its recorded trigger fires Direction only; not a Phase 7A closure condition. Pull this when a real socket, file, or buffered-IO workload otherwise needs a second implementation of short-read/write, close, or ownership behavior.


Workload 8 executed the public listener/stream path but left borrowed net-buffer
overloads at two independent asks, so it does not by itself authorize a full
socket Reader/Writer adapter or File/TextFile migration. Preserve the existing
parser-oriented `read_all` contract; a future socket adapter must state whether
mid-stream transport failure needs fallible `read_to_end`, an explicit partial
result, or a distinct operation.

Remove the overlapping `std.fs.File` versus `std.io.TextFile` split. Choose one
linear `File` handle and adapt it into the existing `Reader`/`Writer` contracts:

```concrete
File::open(path: &Path, options: OpenOptions) with(File)
  -> Result<File, FsError>
File::reader(self) -> Reader
File::writer(self) -> Writer
read_file(path: &Path) with(File, Alloc) -> Result<Bytes, FsError>
read_to_string(path: &Path) with(File, Alloc) -> Result<String, ReadTextError>
write_file(path: &Path, data: Slice<u8>) with(File) -> Result<Unit, FsError>
```

All reads distinguish `Ok(0)` EOF from `Err`; writes expose short writes;
flush/seek/tell/close return typed results. Define `SeekFrom` rather than public
integer `whence`, and never encode EOF/error as `-1`. Formatting, files,
console, fixed buffers, tests, and sockets reuse the same read-all/write-all and
fault-simulation logic. When a bidirectional resource creates adapters, use
borrowed adapters or one explicit ownership split so two independently owned
handles cannot close the same resource.

Gate: `scripts/tests/check_io_spine.sh` inventories exactly one public File,
Reader, and Writer contract; runs file/fixed-buffer/console/socket adapters
through identical short-read, short-write, EOF, flush-failure, close-failure,
and exactly-once-close schedules; and forbids a new public sink/source loop.
The forcing inventory must retain the specific legacy discrepancies rather
than hiding them under “unify IO”: TextFile write/flush/close discard
`fwrite`/`fflush`/`fclose` results; `read_byte` uses `-1` for EOF or failure;
`read_line` cannot distinguish EOF from an empty line; and console stdout/stderr
writers do not apply the same external-write result policy. Fix each when its
vertical consumer moves to the common spine.

### Task R-0016

**Objective:** Add provenance-safe scoped Slice access Promote the currently exempt `std.slice` surface because existing public raw accessors already need a safe counterpart and the compiled-coverage exemption is concrete evidence. V1 deliberately uses scoped access rather than escapable slice returns:


```concrete
Vec::with_slice<R, cap C>(&self, f: fn(&Slice<T>) with(C) -> R) with(C) -> R
Vec::with_mut_slice<R, cap C>(&mut self, f: fn(&mut MutSlice<T>) with(C) -> R) with(C) -> R
Bytes::with_slice<R, cap C>(&self, f: fn(&Slice<u8>) with(C) -> R) with(C) -> R
Bytes::with_mut_slice<R, cap C>(&mut self, f: fn(&mut MutSlice<u8>) with(C) -> R) with(C) -> R
```

`Slice`/`MutSlice` are private-field, non-Copy view tokens; the callback receives
only a second-class reference to the token, not an owned pointer pair it can
return. This adds no user-visible lifetime or region syntax. The callback scope
is the lifetime: the existing reference-escape rules keep the view inside the
call while the owner remains borrowed. Explicit non-goal:
do not add Rust-style lifetime annotations, escapable borrows, or a region
language for this slice.

An ordinary `as_slice(&self) -> Slice<T>`/`as_mut_slice` return remains private,
trusted-internal, or explicitly `_unchecked` until a workload demonstrates that
scoped callbacks are insufficient. A non-Copy linear Slice alone is not a
solution: exactly-once consumption does not stop the owner from being dropped
while the view exists without a checkout/give-back protocol. A future escapable
slice therefore requires a separately reviewed owner/region mechanism or an
explicitly documented unsafe/hazard contract; it is not smuggled into S3.

Convert `Vec::get_mut -> *mut T` into the existing scoped `with_at_mut` ordinary
path; if the pointer escape remains for trusted internals, name it
`get_mut_ptr_unchecked`. Raw pointer+length Slice constructors remain private or
`from_raw_unchecked`. Preserve owner provenance through the existing scoped-
reference rules—do not make slices forgeable or escapable Copy pointer pairs
without an owning borrow contract. Reader/Writer/File/TcpStream/numeric/crypto
migration is workload-pulled after these scoped constructors exist, not an all-
at-once mandate.

Gate: convert the compiled-coverage `std.slice` exemption into positive and
negative fixtures; prove the scoped callback cannot return/store/capture the
slice, the owner cannot be freed/reallocated/mutated incompatibly during the
callback, checked access rejects OOB, mutation is visible only through
`MutSlice`, and the ordinary Vec/Bytes access path no longer returns a raw
pointer or escapable Slice. Track remaining pointer-length hosted signatures in
the manifest rather than forcing an unpulled whole-stdlib migration in this
slice.

### Task R-0017

**Objective:** Normalize hosted failure shapes as their recorded triggers fire The normative direction below follows `ERROR_CONVENTIONS.md`, but each signature change is pulled by a workload or a failing manifest leg. It is not a demand to migrate every hosted module in one campaign.


Converge domain surfaces on explicit errors:

- `net`: `read/write/write_all/read_all/close -> Result`; `Ok(0)` alone means
  EOF; add at least `ListenFailed`, `ReadFailed`, `WriteFailed`, and
  `CloseFailed`; retain portable categories while optionally attaching reviewed
  platform detail.
- `fs/io`: eliminate raw counts/status sentinels and discarded flush/close
  failures; distinguish open, metadata/seek, read, write, flush, close, invalid
  path/C-string, and invalid text boundaries where a caller can react.
- `env`: `set`/`unset -> Result<Unit, EnvError>`; provide a representation that
  distinguishes absent from malformed. Keep invalid-value fallback policy in
  the workload, not irreversibly inside the only stdlib accessor.
- `args`: stop mapping both out-of-range and invalid OS bytes to `""`. Make the
  UTF-8 accessor optional/fallible and add a raw-bytes accessor only when a
  workload needs non-UTF-8 argv.
- `time`: `Instant::now`, `sleep`, and wall-clock acquisition surface platform
  failure; validate normalized nonnegative Duration values; keep monotonic
  `Instant` distinct from wall-clock `SystemTime`.
- `process`: typed spawn/wait/kill/close outcomes; do not expose argv pointer
  arrays or raw errno/signal conventions as the primary portable API.
- `rand`: define `random_range(lo, hi)` over the half-open range `[lo, hi)` and
  trap when `lo >= hi`, because invalid bounds are a bucket-2 caller-invariant
  failure under `ERROR_CONVENTIONS.md`. If a workload needs recoverable range
  validation, add a separately named `checked_range`/`try_range` returning
  `Result`/`Option`; never soften the existing API with a mode flag. Separately,
  distinguish deterministic generator state from hosted OS entropy and
  document modulo-bias policy when those shapes are pulled.

Use `Option` only for ordinary absence, `Result` for recoverable malformed or
environmental failure, and trap for documented invariants/OOM. Add
`scripts/tests/check_stdlib_error_contracts.sh` with one success, absence/EOF,
recoverable error, cleanup error, and invalid-input leg per hosted module.

### Task R-0018

**Objective:** Stabilize boundary type roles under pull discipline Freeze and gate these roles:


```text
Bytes / Slice<u8>       owned / borrowed arbitrary bytes
String / Text           owned / borrowed valid UTF-8
PathBuf / Path          owned / borrowed OS path bytes
CString / CStr          owned / borrowed checked FFI string
```

Filesystem APIs accept `Path`, not UTF-8 `String`; display conversion is
fallible. Keep operations semantically distinct and do not inflate pull counts:
`index_of(byte, from)`, future `find_subslice(needle, from)`, and future
`starts_with_at(offset, expected)` are different APIs. If a workload needs
recoverable endian bounds, add checked readers and name trapping forms
`_unchecked`; do not churn them without that consumer. `fmt::write_char` is
correct under Concrete's current byte-sized `char` contract and is not a stdlib
defect. Revisit its name/encoding only if the language character model changes.
Raw views and ownership-taking raw-parts APIs remain explicitly trusted and
document their lifetime/allocation handoff.

`BytesRaw` currently names both a non-owning result of `raw_view` and the
ownership handoff from `into_raw_parts`; audit this immediately because borrowed
and owning values cannot share an ambiguous destruction contract. Split the
owning form into `BytesParts` (pointer/length/capacity plus explicit allocator/
handoff rules) and keep the borrowed form provenance-bound, unless a narrower
design proves the same distinction. Do not collapse Slice, Text, ByteView,
ByteCursor, and the text parser Cursor merely to reduce the type count: record
their owner, validation, mutability, position, and destruction roles, then let
parser workloads decide whether ByteView is a refined Slice and whether cursors
share implementation. Move binary cursor APIs out of `numeric` only with that
consumer-backed module decision.

Gate: a generated conversion matrix covers valid/invalid UTF-8, arbitrary path
bytes, embedded NUL, empty buffers, multibyte scalars, split code points, and
owned/borrowed round trips. The API snapshot fails if a checked boundary loses
its fallibility or an unchecked operation loses the suffix.

### Task R-0019

**Objective:** Add collection ergonomics as recorded workloads pull them Preserve the strong existing decisions: scoped callback access instead of escaping references, `Destroy` for live non-Copy contents, capability- polymorphic traversal, and ascending traversal only for ordered containers.

Then:

- expose slice views first; remove ordinary raw element pointers;
- make allocation timing consistent or documented (`HashMap::new` currently
  allocates while `OrderedMap::new` is lazy);
- keep HashMap/HashSet storage unsafety internal, so callers need `Alloc` but
  not `Unsafe`;
- gate tombstone/full-table probing, duplicate replacement, non-Copy key/value
  destruction, clear/reuse, and growth under adversarial hashes;
- define BitSet's logical-length versus capacity contract for set/unset/test,
  union/intersection, out-of-range bits, and clear/reuse; keep storage `Unsafe`
  internal and make allocation the only ordinary caller-visible effect;
- add `Vec` insert/stable-remove/retain/sort/search and an ownership-aware map
  entry/upsert API only when independent workloads reach the pull threshold;
- keep unordered traversal explicitly unspecified and ordered traversal
  ascending; do not promise insertion order accidentally; and
- do not add iterator traits, lazy adapters, or reference-returning entry APIs
  until the reference model can enforce them.

Keep a semantic naming/coherence ledger rather than a mechanical rename sweep:
process name stuttering; duplicate BitSet verbs (`set/unset/test` versus
`insert/remove/contains`); BitSet logical length versus population count;
unchecked endian readers; empty `with()`; bool versus three-way comparator
contracts; and insert return shapes. Rename only after documenting whether the
difference carries ownership, ordering, replacement, or failure information—
for example HashMap returning a displaced value is not equivalent to Set
reporting whether membership changed.

### Task R-0020

**Objective:** Add remaining module shapes as recorded workloads pull them The following are design candidates, not scheduled Phase 7A migrations. Each requires independent workload evidence or a confirmed contract failure before implementation.


1. Replace public `spawn(cmd, *const *const u8)` with a typed `Command` builder
   over `Path`/owned arguments. Put fork and Unix signals in an explicitly Unix
   or raw module; generic `std.process` must not imply portability it lacks.
2. Store Duration canonically and define overflow/negative policy. Remove
   hard-coded clock ids from generic code; target facts choose the platform
   implementation. Add deterministic clock/failure adapters for tests.
3. If a workload pulls it, introduce explicit deterministic `Rng` state and
   reserve `with(Random)` for hosted entropy. Gate reproducibility, range
   bounds, invariant-trap behavior for `lo >= hi`, any separately named
   recoverable range API, and bias claims. Never describe libc `rand` as
   cryptographic.
4. Keep scalar parsing's `Option` surface; use positional `ParseError` results
   when cursor/structured-parser workloads need diagnostics. Do not create an
   implicit conversion web between module errors.
5. Keep Writer-first formatting and allocation-visible String wrappers. Test
   all Writer failures; do not use `unwrap_or` to erase a formatting sink error.
6. Make test assertion messages borrowed/static where possible, add typed
   byte/String/path assertions, and keep exit status/stdout/stderr distinct.
7. Replace `sha256::hash_raw(*mut u8) -> Vec<u32>` with a borrowed byte input
   and nominal/fixed 32-byte digest once fixed-array ergonomics support it; input
   is not mutable. Apply the same Slice input policy to checksum/base64/hex.
   Streaming crypto/encoding remains workload-pulled.
8. Replace `std.cli`'s untyped `u64` flag ids and unchecked lookups with nominal
   declaration handles, reject duplicate declarations, keep unknown/missing/
   malformed/arity errors distinct, and define repeated-flag policy. Argument
   acquisition must carry its authority consistently (`count` and `get` may not
   disagree), while pure token parsing remains capability-free.
9. A3 owns the already-pulled Option/Result consuming-combinator slice. Any
   additional convenience combinator remains pull-gated; extend `std.test`
   helpers alongside A3 so linear Option/Result values can be asserted without
   becoming unconsumable.
10. Audit pure scalar modules (`ascii`, `math`, `parse`, `numeric`, `hash`) for
    totality and naming: invalid clamp/range bounds, `abs(MIN)`, float domain/
    NaN behavior, cursor bounds, integer overflow, endian width, and hash
    stability must be documented as checked result, trap, or defined value and
    pinned by edge vectors. Avoid generic APIs whose ordering/arithmetic
    constraints are only implicit in the body.
11. Keep the remaining workload-gap ledger evidence-counted: `String::cmp` at
    its recorded ask count; `Vec::with_capacity`; SHA/hex borrowed-Bytes input;
    stdin Reader; and safe process argv construction. Promote only at the pull
    threshold or on a confirmed contract defect. Do not combine them into a
    polish release merely because neighboring modules already have symmetric
    names.

### Task R-0021

**Objective:** Complete the mechanical manifest and fail-closed coverage gate Extend `concrete std snapshot --json` and the compiled-coverage inventory so every public symbol records: canonical signature, safety class, capabilities, allocation, ownership/consumption, success/absence/error/trap behavior, platform availability, evidence class, examples, and trusted-boundary id.

Generate rather than hand-count the inventory.

The inventory is fail-closed over every `std/src/*.con` module, including the
low-level `alloc`, `ptr`, and `libc` modules and umbrella `lib`. A module may be
classified pure, alloc-backed, hosted, raw/unsafe, umbrella, or intentionally
surface-less, but none may disappear because the focused migration examples do
not mention it. Adding a module or public symbol without a compiled behavioral
fixture or reasoned exemption, API snapshot entry, and evidence/authority/
failure classification fails CI. This extends the existing five-fact manifest
and compiled-coverage machinery; do not create a parallel review database.

### Phase 7 Completion Contract

The final Phase 7 task may close only when:

- Tasks R-0001–R-0010 are fixed with committed
  load-bearing regression/class gate, the bug-corpus report distinguishes open
  reproducers from fixed coverage, and declared remotes contain the intended
  milestone artifacts before publication is claimed;
- private-by-default enum variants and raw newtype payload access complete the
  one-keyword construction-rights model without weakening the shipped struct-field gate;
- A3 supports the workload-pulled linear combinators with exactly-once
  conservation and updated proof evidence;
- `fs::read_to_string` rejects invalid UTF-8 without losing the Bytes path;
- every current String-to-C consumer rejects interior NUL through one checked
  adapter and the unchecked primitive is explicitly named/private;
- the public-safety rule is documented and mechanically gated, `std.net` no
  longer makes ordinary callers request `Unsafe`, and every remaining legacy
  exception is a ratcheted manifest row rather than an invisible exemption;
- Vec/Bytes expose provenance-preserving Slice/MutSlice constructors,
  `Vec::get_mut` is scoped or explicitly `_unchecked`, and the compiled
  `std.slice` exemption is gone;
- the API snapshot, compiled behavioral inventory, docs/examples, authority/
  allocation/failure manifest, and evidence report agree;
- borrowed Bytes views and owning raw-parts handoff are different typed
  contracts with mutation-tested destruction/provenance behavior; and
- the documented `io::read_all` partial-parser contract remains pinned rather
  than being silently changed under the cleanup.

S2 and S4–S7 are trigger records, not Phase 7A closure criteria. Move their work
into the earliest honest global task position only with the named workload,
failing gate, or already-ratified contract violation. Do not wait for broad File/process/time/
random/crypto redesign or JSON/DNS/compression/threading breadth to close this
correction slice.

### Task R-0023

**Objective:** Retire the remaining entry-point tails Deliberately decoupled from the completed exit-model migration: retire Int-main in a later surface pass by narrowing the entry signature to `fn main() -> u8 | Unit`; workload-gate any `main -> Result<Unit, E>` form and nominal error rendering to stderr.


### Task R-0024

**Objective:** Finish compiler hygiene and known-defect tails Each item lands as its own evidence-ranked slice with the battery:

   - Layout panic hygiene: convert `Concrete/Check/Layout.lean` `panic!`
     paths (unknown struct/named type in fieldOffset/tySize) into
     structured internal-error diagnostics — bug 035's fix made them
     unreachable for well-formed programs, which is exactly when a panic
     should become a diagnostic. SCOPED (2026-07-17): ~69 call sites
     (tyAlign 21, tySize 27, isPassByPtr 12, fieldOffset 8, tyToLLVM 2
     incl. EmitSSA), all cascading through pure code — no small slice;
     it is an Except-threading grind that must land green in ONE piece,
     with the full battery as backstop. Dedicated fresh session.
   - Known-defects ledger (each owns a slice and battery when its stated trigger
     fires):
     These are historical/pull-gated tails inside this task, not another source
     of priority; do not select one while an earlier-numbered task lacks
     containment.
     1. Bug 039 residual (docs/bugs/039): two SUBMODULES of one top-level
        module importing different same-named fns still share a flat
        alias list (collectAllLinkerAliases). Pull-gated: fix when a
        workload or fixture hits it.
     2. `std.slice` has no public constructor — user code cannot obtain a
        provenance-backed `Slice` at all (coverage-gate exemption records it).
        Phase 7A S3 owns the narrow fix because existing Vec/Bytes raw access
        already needs a safe counterpart; convert the exemption into positive
        and negative compiled fixtures. Migration of IO/fs/net/numeric
        signatures remains workload-pulled after the constructor exists.
     3. Bug 027 (EmitSSA O(n^2) rendering) — open perf item, not
        correctness; profile-driven fix when compile times matter.
     4. Differential fuzzer/oracle residuals: heap/alloc/IO shapes still
        compiled-only, trap class not compared interp-vs-compiled, no
        float support in interp comparisons.
     5. Check-side divergence/capability classifiers still match raw Core names
        after intrinsic identity was fixed in Elab/Lower. The remaining
        direction is conservative misclassification only; pull with Task R-0274's
        typing-truth work or a failing user-shadowing fixture.
     6. Non-finite float literals leak an external tool error (audit
        2026-07-16): a literal with a ≥309-digit integer part lexes to +inf,
        and EmitLLVM prints `double inf`, which llvm-as rejects — an
        external-tool diagnostic reaching the user instead of a compiler one.
        Fix fail-closed: reject non-finite literals at lex time with a
        spanned diagnostic (or emit the LLVM hex-float form); add a corpus
        leg to check_float_literals.sh.
     7. Silent defaults sweep (audit 2026-07-16; adjacent to §F's panic
        inventory but distinct — these fabricate rather than crash):
        `sizeof` with no type args defaults to `Ty.int` (Lower),
        `vec_pop`'s element type defaults to `.placeholder` (Elab), an
        unknown enum's payload offset defaults to 8 (Lower). Each becomes a
        structured internal-error diagnostic with a sweep proving no current
        program reaches them.
     8. Bug-043 class rule is prose-only (docs/bugs/043): nothing stops a
        new FFI site handing a raw `.ptr` to a NUL-delimited C API again.
        Add a std-hardening-style grep/lint leg over trusted FFI sites
        (no bare `.ptr` argument to a C string function; ptr+len sites
        exempt).
     9. printf|grep -q pipefail flake class (10b9a776 fixed 123 sites, 43
        files): add a grep-lint to gate-hygiene so the pattern cannot be
        reintroduced.
### Task R-0025

**Objective:** CLI/env/process helpers for real tools (stdlib APIs, not compiler CLI).

### Task R-0026

**Objective:** Finish trap/debug UX and verified-profile/ proof-obligation UX.

### Task R-0027

**Objective:** Extend the shipped pure-core proof arc over actual `Option`/`Result`, `Bytes`/slice, numeric checked helpers, and checked text/path conversions against their documented contracts.

### Task R-0028

**Objective:** Proof-facing formal stdlib models (`formal_vec`, `formal_map`, `formal_set`, `bigint`, lemma helpers) once contracts need them.

### Task R-0029

**Objective:** Broad compression/crypto/networking/threading only after workload demand.

The comparison-language bias for remaining breadth is deliberate: copy module
categories from Hare/Zig/Odin/Go, not their API volume. The near-term systems
set is `cli`, `fmt`/`parse`, `io` buffering and memory sinks/sources,
`fs`/`path`/`temp`/`dirs`, `log`, `sort`/`search`, `uri`, `json`, checksums,
and narrow shell helpers (`shlex`/`glob`/`fnmatch`) when a workload pulls them.
Keep broad crypto, compression/archive formats, OS debug formats, DNS/Unix
sockets, full HTTP, threads, SIMD, and platform databases out of the core until
there is a validation workload and an evidence story.

Phase 6E owns the **compiler** command surface (`concrete build/run/test/fmt`,
help, reports, trace/debug aliases, and compatibility). Phase 7's CLI work is
the **stdlib** side: APIs that Concrete programs use to parse their own command
lines (`std.cli`), read process arguments (`std.args`), and build real tools.
Do not duplicate compiler-command cleanup here.

### Task R-0030

**Objective:** Deterministic capability-fault simulation (pull-gated; the *dynamic* complement to H3). H3's manifest gate proves an IO error *can* be surfaced (static shape); this proves it *is* surfaced under real failure (dynamic).

  Because every effect flows through an explicit capability/handle value — e.g.
  `Writer`'s `write_fn: fn(...) -> Result<u64, IoError>` — faults are injected by
  swapping the backend at the seam, with no whole-world simulator; the capability
  model makes fault injection cheap by construction, which is why Concrete's
  version is far smaller than Turso-style DST. Deterministic failure schedules
  drive properties static analysis cannot reach: `write_all` never `Ok` after a
  short/failed write; close-fail-after-flush is reported; allocation #N fails;
  `Reader` treats `Ok(0)` as EOF only at the right boundary. Do NOT re-litigate
  already-static properties (capability erasure = manifest gate; exit-status ≠
  stdout = MAIN_EXIT_MODEL). Proof stays separate — the pure core's first line;
  simulation covers only the effectful failure interleavings proofs do not, and
  upgrades no evidence class to `proved`. Pull-gated: pulled when H3 lands or an IO
  bug appears; run against `base64_cli`/`png_chunks`/workload 3, starting at the
  `Writer`/`File` seam and expanding only as each capability (`Alloc`, `Time`)
  gains an injectable backend. Design record:
  [`docs/DETERMINISTIC_SIMULATION.md`](docs/DETERMINISTIC_SIMULATION.md), which
  refines the tier-J+ "deterministic simulation backend" earmark in
  `docs/EXECUTION_MODEL.md`. Gate: `scripts/tests/check_effect_simulation.sh`
  replays a fixed fault schedule and asserts each effect-boundary property.

### Task R-0031

**Objective:** Build the remaining collection APIs across `std.vec`, `std.map`, `std.set`, `std.ordered_map`, `std.ordered_set`, `std.deque`, `std.heap`, `std.bitset`, and `std.slice`: fixed arrays/slices, `Vec<T>`, maps, sets, buffers, parser cursors, and capacity-aware helpers. Add the ordinary APIs C/Rust users expect: `contains`, `remove`, `iter` for maps and sets;

`insert`, `remove`, `retain`, `sort`, `search` for vectors and slices;
stable ordering helpers for ordered collections. Each API must state whether
it requires `with(Alloc)`, whether it can fail, and which runtime
obligations it creates.

Every new collection API must preserve the H18 ownership contract documented
in [CHANGELOG.md](CHANGELOG.md) and `docs/RUNTIME_COLLECTIONS.md`: a
collection owns its live elements until they are explicitly moved out.
`clear`, `drop`, overwrite,
set/replacement, and compaction paths destroy displaced live elements;
`pop`, `remove`, and `swap_remove` transfer ownership to the caller.
Disposal stays forced-explicit (`x.drop()` / `defer x.drop()`), never silent
scope-end auto-drop. `get -> Option<T>` remains `T: Copy`; non-`Copy` indexed
access remains scoped callback / borrow / explicit move-out, with no returned
mutable references. The remaining pull-gated v1 fence is associated
destructor-capability machinery, pulled by the first infallible
non-`Alloc` destructor, and raw `*_unchecked` overwrite escapes remain
documented trusted escapes.

Traversal must be **deterministic but not ordered**:
`for_each`/`fold` over `std.map`/`std.set` need a fixed hasher and no per-run
random seed (so the oracle/differential story is stable given the same
operations), but the visitation order is deliberately **not** a defined key
order — `HashMap`/`HashSet` are permanently unordered (a map is unordered in
the proof model, so this costs the evidence story nothing). A defined key
order is `OrderedMap`/`OrderedSet`'s job; their `for_each`/`fold` APIs have
landed and must remain ascending-key traversal. Do not add insertion-order
tracking to the hash collections.
- 1a. Keep `Clone` and indexed move/swap as **workload-gated value-model
  research**, separate from H1 and not assumed inevitable. If admitted,
  `Clone` is explicit semantic duplication (capability-visible, usually
  `with(Alloc)`, audit-visible, and not silently proof-eligible), not a
  convenience patch for borrowed reads. The sibling move-out primitive for
  indexed containers is `swap(i, new) -> V`, which transfers ownership out
  while preserving the linear one-value-per-slot invariant. Build either only
  after a real workload repeatedly needs owned duplication or indexed
  ownership-out and the existing `Copy` / move / borrow APIs are the wrong
  fit.
- 1b. Design arena/index safety before any arena-backed structure becomes a
  flagship or stable stdlib API. Array-backed linked structures use indices,
  and a stale index into a removed/reused slot is a logic-level dangling
  pointer that ordinary ownership does not see. V1 answer should compare
  typed index newtypes (`NodeId`, not `u64`), generation-counted slots,
  arena validity invariants, and proof obligations that inject newtype
  invariants as scoped hypotheses. Public stdlib arenas remain workload-
  gated: do not add a generic `Arena<T>` until a parser/graph/runtime
  workload needs it, and never expose raw integer indices as the stable API.
  Any admitted arena-backed container must preserve linear destruction,
  allocator visibility, and stale-index rejection or explicit trust. Add
  `docs/ARENA_INDEX_SAFETY.md`, `examples/arena_indices/{stale_index,generation_checked,typed_node_id}/`,
  and `scripts/tests/check_arena_index_safety.sh`; the gate must prove stale
  index reuse is rejected, trapped, or explicitly trusted, never silently
  treated as ordinary memory-safe access.
- 1c. Decide explicit-dictionary coherence before binary collection
  operations harden. `HashMap<K, V>` values built with different `hash_fn` /
  `eq_fn`, or ordered maps/sets built with different comparators, have the
  same type but incompatible semantics; `union`, `merge`, `intersection`,
  equality, and bulk transfer across them can silently produce nonsense.
  Choose one v1 rule: canonical registered dictionary per key type,
  dictionary/brand carried in the collection type or value, debug/runtime
  identity checks on binary operations, or a documented sharp edge that
  forbids binary ops across explicitly-different dictionaries. Add
  `docs/COLLECTION_COHERENCE.md`,
  `examples/collection_coherence/{same_dictionary_merge,different_eq_rejected,different_cmp_rejected}/`,
  and `scripts/tests/check_collection_coherence.sh`; the gate must show the
  chosen verdict for incompatible dictionaries and prove the API cannot
  silently combine them.
- 1d. Future collection APIs must preserve the `with_value_mut` / `modify`
  `E0293`
  container-not-in-context guard: the callback's `&mut ctx` may not alias the
  `&mut self` container. Gated in `check_callable_values.sh`.
- 1e. Keep scalar `from(param)` returned references deeply deferred and
  evidence-gated. If ever added, they stay flat and scalar: no `Option`,
  `Result`, structs, arrays, containers, callback contexts, or generic
  wrappers. This is a research escape valve only if real workloads prove
  operation APIs, owned views, and scoped callbacks insufficient.
- 1f. Pull narrow const generics forward only when fixed-capacity stdlib APIs
  need reusable capacities. Follow `docs/CONST_GENERICS_V1.md`; implementation
  is deferred until a real Phase 7 consumer
  appears (`BoundedVec<T, N>`, `RingBuffer<T, N>`, `PacketBuf<N>`, fixed hash
  table, parser scratch buffer, freestanding reusable buffer, or a
  capacity-indexed proof API). When triggered, implement the staged V1 from
  that doc and wire `scripts/tests/check_const_generics_v1.sh` to prove
  distinct capacities specialize separately, layout is capacity-specific,
  runtime-safety obligations name the instantiated size, and unsupported
  comptime/reflection/runtime-bound forms are rejected.
- 1g. Research **allocator-as-value** before allocator-backed collections,
  arenas, or freestanding APIs harden. Keep the distinction sharp:
  `with(Alloc)` is permission to allocate; an allocator value names which
  allocator/arena/pool is used. Do not replace capabilities with allocator
  parameters, and do not add an ambient implicit allocator. The research
  note should compare Zig-style explicit allocators, arena/test allocators,
  embedded/freestanding pools, hot-reload/plugin ownership, allocation
  failure policy, and audit output that records both the capability and the
  allocator identity/strategy. If admitted, every allocator-taking API must
  keep allocation authority visible in function headers and reports. This
  item should move ahead of allocator-heavy collection stabilization if the
  validation project needs arenas, test allocators, reload-safe allocation,
  or freestanding pools.
### Task R-0032

**Objective:** Build internal-iteration and builder APIs in proposed `std.iter` and `std.builder` after the collection shape is known. This is NOT Rust's external-iterator / adapter-tower model: `research/stdlib/iterators.md` resolved the v1 design as per-container internal traversal (`for_each`, `fold`, context-threaded callbacks, and optional early-exit via an explicit `Continue | Break` tag), with no iterator trait, no lazy adapter chain, and no cursor/lifetime model. Add known-length reporting and reverse traversal (`rev_fold`/`rev_for_each` — today every backwards walk is a manual index-decrement loop; extend `docs/ITERATION_PROTOCOL.md` when these land),

plus byte/text builders and tree/buffer builders inspired by Gleam's
`BytesTree` and `StringTree`. Do not hide allocation; builder APIs either carry
`with(Alloc)` or operate over fixed buffers.
### Task R-0033

**Objective:** Complete the Hare/Zig-shaped IO adapter layer on top of the one `std.io.Reader` / `std.io.Writer` spine: buffered reader/writer helpers (`bufio` shape), fixed-memory readers/writers (`memio` shape), byte-counting and tee/discard adapters, and small copy/drain helpers. These are adapters, not a second IO abstraction. Every adapter must preserve the underlying capability story, keep allocation explicit (`with(Alloc)` or caller-provided buffers), and be covered by `std.test` sink/source oracles.

### Task R-0034

**Objective:** Build numeric helper APIs in `std.numeric`, `std.math`, and `std.mem`:

checked/wrapping/saturating arithmetic helpers,
narrowing/conversion helpers, endian conversions, byte/word packing, and
evidence classes for each helper.
### Task R-0035

**Objective:** Build sorting and searching primitives in proposed `std.sort` and `std.search`: comparison conventions, stable/unstable sort decision, binary search, min/max helpers, and evidence/oracle tests over edge cases.

### Task R-0036

**Objective:** Build hashing, checksums, and deterministic random helpers in `std.hash`, proposed `std.checksum`, and `std.rand`: stable hash APIs for maps/sets, non-cryptographic checksums, seeded deterministic RNG for tests/oracles, and a clear split from cryptographic randomness. Any OS entropy source is hosted-only and capability-visible.

### Task R-0037

**Objective:** Build constant-time helper APIs in proposed `std.ct` only for narrow, auditable cases: equality/compare over fixed-size bytes, no secret-dependent branches or early exits, source-shape audit evidence, and clear machine-level timing assumptions. This belongs to the narrow security surface, not broad crypto.

### Task R-0038

**Objective:** Build time and duration helpers in `std.time`: monotonic versus wall-clock distinction, timestamp formatting/parsing if admitted, timeout helpers, and explicit hosted authority for reading the clock.

### Task R-0039

**Objective:** Build formatting and parsing helpers in `std.fmt` and `std.parse`:

 integer/text formatting, simple
 scanners, structured parse results, error-set reports, and oracle-friendly
 output conventions.
### Task R-0040

**Objective:** Build a reusable scanner/parser core in `std.parse` over `std.bytes.Bytes` and `std.text.Text`: `peek`, `advance`, `take_while`, `consume`, span/position tracking, error reporting, and no hidden allocation unless the API carries `with(Alloc)`.

### Task R-0041

**Objective:** Extend `std.base64` only when a workload pulls streaming encode/decode; preserve canonical padding-bit strictness and the args -> bytes/text -> parse/errors -> Writer oracle path.

### Task R-0042

**Objective:** Add `std.uri` parsing/formatting after the byte/text/path split is stable:

 component accessors, percent encoding/decoding, normalization policy, and
 clear distinction between syntax validation and network authority.
### Task R-0043

**Objective:** Add `std.json` as the first structured data module: tokenization, string/number handling, error spans, bounded recursion policy, optional DOM-like representation only if the allocation story is explicit, and oracle tests against a reference implementation.

### Task R-0044

**Objective:** Add a small typed decoding layer in proposed `std.decode` after `std.json`: dynamic value decoding, field access, error paths, and examples comparable to Gleam's `dynamic/decode`, without broad reflection or hidden runtime typing.

### Task R-0045

**Objective:** Add binary serialization helpers in proposed `std.bin`: endian-aware reading/writing, fixed-width integers, length-prefixed fields only with explicit bounds, byte-span diagnostics, and no hidden allocation.

### Task R-0046

**Objective:** Add semantic-version and config-format helpers in proposed `std.semver` and `std.config` if package/build work starts depending on them:

 `SemVer`, INI/TOML-style scanner, and manifest parsing support. Prefer a
 small Hare-like `format/ini`-class module before any broad configuration
 framework; keep TOML/YAML/package manifests workload-gated. These are
 stdlib/package-boundary helpers, not general metaprogramming.
### Task R-0047

**Objective:** Add command-line parser helpers in proposed `std.cli`: flags, positional arguments, usage text, typed parse errors, no ambient environment access except through `std.args`, and examples that keep authority visible. This is for user programs. The `concrete` compiler's own command taxonomy and help behavior are Phase 6E, and `std.cli` should learn from that surface without coupling to compiler internals.

### Task R-0048

**Objective:** Add narrow shell/path helper modules only as tool workloads demand them:

  `std.shlex` for shell-word splitting/quoting, `std.glob` / `std.fnmatch`
  for file-pattern matching, and no `wordexp`-style command/variable
  expansion in the core. These APIs are byte/path aware, carry no hidden
  filesystem authority, and must keep expansion separate from matching so a
  pure parser cannot accidentally become a hosted operation.
### Task R-0049

**Objective:** Add simple logging/diagnostic output APIs in proposed `std.log`: levels, writers, formatting integration, capability requirements, and policy for release builds. Keep this small; it is not a tracing framework.

### Task R-0050

**Objective:** Add progress/status output helpers for CLI tools in proposed `std.progress` only after `examples/daily/word_count` and `examples/base64_cli` need visible progress output. V1 surface: `ProgressWriter`, `quiet`, `verbose`, `set_total`, `advance`, `finish`, terminal detection through an explicit console handle, and no ambient terminal authority. Wire `scripts/tests/check_stdlib_progress.sh` when the module is admitted.

### Task R-0051

**Objective:** Build capability-scoped console, file, network, process, and time APIs in `std.io`, `std.fs`, `std.env`, `std.args`, `std.process`, `std.net`, and `std.time`. Authority must be visible in function types and audit reports; no API may smuggle ambient authority through a convenience wrapper. New output/input APIs target the `std.io.Reader` / `std.io.Writer` spine; a separate public `std.writer` sink surface must not reappear.

 Add an explicit **boundary-module pattern**, borrowing the useful part of
 Elm's ports without copying Elm's web-application architecture: authority
 enters through a small named module, the wrapper exposes a narrow safe API,
 and the audit report names the capability, trusted/FFI boundary,
 assumptions, allocation behavior, and evidence class. Concrete examples:
 `std.fs.boundary` wraps ambient filesystem entry points behind directory
 handles; `std.net.boundary` wraps socket creation before pure parsers see
 bytes; `std.libc.boundary` wraps extern calls before safe code sees owned
 values. Add `docs/stdlib/STDLIB_BOUNDARY_MODULES.md`,
 `examples/stdlib_recipes/{fs_boundary,net_boundary,ffi_boundary}/`, and
 `scripts/tests/check_stdlib_boundary_modules.sh`; the gate must prove that
 the public safe wrapper surface has narrower authority than the underlying
 trusted/extern implementation and that the audit transcript shows both
 sides of the boundary.
### Task R-0052

**Objective:** Build handle-relative filesystem APIs in `std.fs` as the preferred file/path shape: directory/file handles carry authority; operations are relative to handles for `open`, `create`, `read`, `write`, `metadata`, `remove`, `rename`, and `list`. Ambient absolute-path helpers must be hosted-only convenience wrappers with explicit authority. Temp-file, symlink, path-normalization, and TOCTOU behavior must appear in `docs/stdlib/STDLIB_GUIDE.md` and `scripts/tests/check_stdlib_fs.sh`.

### Task R-0053

**Objective:** Add small `std.temp` and `std.dirs` helpers after the handle-relative filesystem shape is gated. `std.temp` owns temporary files/directories with explicit cleanup (`drop`/`defer`, no implicit scope-end delete) and visible `with(File)` authority; `std.dirs` exposes only conservative hosted directory discovery needed by tools. Do not add XDG/platform policy sprawl until package/build workloads pull it.

### Task R-0054

**Objective:** Build handle-based network surface in `std.net` only as far as the validation workloads require: address parsing, socket handle wrappers, HTTP header parsing as a pure parser first, and no full HTTP client/server until package/workload evidence demands it.

### Task R-0055

**Objective:** Build stdlib test/oracle helpers in `std.test`: expected failures, capability-scoped fixtures, temp directories, oracle vector runners, interpreter-vs-compiled helpers, and report snapshots. Stdlib tests must also serve as runnable API documentation: every public stdlib type/function gets a tiny positive usage test and, where meaningful, a negative or edge case. Public docs should link to or quote those tests rather than carrying stale hand-written examples. This is the Zig stdlib lesson adapted to Concrete: usage examples should be executable fixtures, not prose that can rot. Add a gate that fails when a new public stdlib symbol lacks a test/doc

 example, when a referenced example no longer compiles, or when a doc claims
 an evidence class/capability/allocation behavior that the test/report does
 not produce.
### Task R-0056

**Objective:** Add a public stdlib API snapshot/diff immediately after the first real workload (`base64_cli`) and before broad URI/JSON/CLI/log/progress breadth.


  Deliverable: `concrete std snapshot --json` (or an equivalent test helper)
  and `scripts/tests/check_stdlib_api_snapshot.sh`.

  Snapshot fields: module path, public names, signatures, `Copy`/linear
  requirements, capabilities, allocation behavior, trap/recoverable-failure
  profile, evidence class, deprecation status, and linked tests/docs.

  Done when adding/removing a public function, changing a signature, widening
  a capability, changing allocation/trap behavior, or changing an evidence
  class fails the gate until the snapshot and docs are updated. This is the
  stdlib-local version of Phase 10's proof/capability diff and Phase 18's
  package API artifacts; it prevents accidental drift while the library is
  still small.
### Task R-0057

**Objective:** Define stdlib error-handling conventions: when APIs return `Result`, `Option`, panic/abort, or require a policy gate; how ignored-result diagnostics apply; and how accumulating error sets are reported. Split recoverable domain/environment failures from fatal invariant failures:

 `Result`/`Option` for user, file, network, parse, and domain errors that a
 caller can handle; abort/trap for OOM, bounds, arithmetic traps, impossible
 invariants, and explicitly unrecoverable runtime failures. Audit output
 should identify which public APIs can recover, which can trap, and which
 require a policy gate. Do not add a second Zig-style error-union mechanism;
 improve the `Result`/`Option` surface instead. Define how a module error
 composes into a caller's error across a boundary (`FsError` -> `ServiceError`)
 WITHOUT Rust's `From`/`Into` trait web: an explicit conversion function or a
 small wrapping variant named at the call site, not an implicit trait
 conversion. State the pattern once so error-heavy code does not each invent
 its own.
### Task R-0058

**Objective:** Define the canonical **consume / destroy / handoff** conventions for linear code. The guide must distinguish explicit cleanup (`destroy(x)` or the type's consuming `.drop()`/Destroy verb), ownership transfer by by-value call, ownership transfer by return, destructuring into owned fields, `defer` as explicit scheduled cleanup, and the forbidden cases (bare non-`Copy` statement, `_` over non-`Copy`, `let _`, non-`Copy` sub-place projection by value). This is documentation plus examples and diagnostics, not automatic `Drop`: no hidden cleanup and no hidden control flow. Include a short **Copy and Linear Values** guide: when to mark a type

 `Copy`, why `Option<i32>` is `Copy` but `Option<String>` is linear, how
 value-mode reads move non-`Copy` bindings by default, why `_` ignores only
 `Copy`, how params are owned locals unless they are borrows, and the
 ordinary consume paths (`destroy`, handoff, return, destructure, `defer`).
 This guide is the user-facing explanation of the conditional-`Copy` and
 mode-based-checker refactors; it should be validated by small runnable
 examples and linked from README/site docs.
### Task R-0059

**Objective:** Define stdlib evidence classes per public API: `proved`, `enforced`, `reported`, `tested_by_oracle`, `assumed`, or `trusted`. The evidence class must appear in docs and audit artifacts, not just implementation comments.

### Task R-0060

**Objective:** Add proof-facing formal stdlib models only when a proof or contract workload pulls them. These are not runtime containers: `std.formal_vec`, `std.formal_map`, and `std.formal_set` model mathematical sequences, maps, and sets for contracts, loop invariants, and Lean obligations; `std.bigint` models unbounded mathematical integers for specs before any runtime big-number API exists; `std.rational` stays deferred until exact-ratio specs need it. Each formal module must state its erasure/runtime story, its evidence class, the Lean artifact it lowers to, and the refinement relation to runtime containers such as `Vec`, `HashMap`, `OrderedMap`, and `HashSet`.

### Task R-0061

**Objective:** Add a **selective shipped pure-core stdlib proof arc**, distinct from the pull-gated formal-container work in 24a. This proves stable stdlib code users actually call, not separate mathematical containers, and it is not a mandate to prove all 414+ public API rows before the API surface has been validated by workloads. Scope v1 narrowly:

 `Option`/`Result` helpers (`map`, `and_then`, `map_err`, identity /
 composition where expressible), numeric checked helpers (success/failure
 agreement with documented overflow, divide-by-zero, and narrowing
 behavior), `Bytes`/slice pure helpers (`view`, `cmp`, checked `get`/`set`,
 raw-data preservation, bounds behavior), and checked text/path conversions
 (valid UTF-8 boundary, raw bytes preserved, unchecked constructors named
 `_unchecked`). Non-goals: hosted APIs, allocation behavior, filesystem,
 network, process, clock, console effects, broad collection proofs, and the
 `formal_vec`/`formal_map`/`formal_set` model-container layer. Treat tests,
 oracle vectors, manifest facts, mutation gates, and capability/allocation
 gates as the right evidence for unstable or broad APIs; upgrade an API to
 `proved` only after it is small, pure, central, and workload-pulled. Done
 when a small Lean proof suite is CI-gated, each covered API's manifest/report
 row distinguishes `proved` from `tested`/`enforced`, and a negative fixture
 proves the report cannot keep `proved` after a body/spec drift. This is the
 "proved pure core" part of the Phase 7 excellence contract; do not let it be
 deferred behind breadth modules such as JSON, CLI, or networking, but also
 do not turn it into an exhaustive stdlib-proof sweep before API ergonomics
 have been validated.
### Task R-0062

**Objective:** Add stdlib authority/allocation/runtime-obligation gates so core helpers cannot silently widen capabilities, allocation behavior, trusted assumptions, or runtime-risk obligations.

### Task R-0063

**Objective:** Add sanitizer/runtime-instrumentation hooks as debug and validation surfaces, not proof evidence. Inspired by Odin's sanitizer-facing base surface, Concrete should expose named hooks for bounds, overflow, use-after-free-like trusted/FFI probes where applicable, allocator checks, and generated-code validation. Audit output must classify this as `runtime_checked`, `tested`, or `instrumented`, never `proved`.

### Task R-0064

**Objective:** Split hosted versus freestanding-ready stdlib modules at the API level:

 no-alloc/no-OS core modules, allocator-backed modules, hosted OS modules,
 and modules that are explicitly unavailable under freestanding profiles.
 The freestanding target implementation still lands in Phase 16.
### Task R-0065

**Objective:** Record deliberately deferred stdlib families so they do not disappear from planning: compression/archive formats, broad crypto beyond the narrow hash/HMAC/constant-time story, PEM/ASN.1/X509, MIME helpers beyond the first workload, full HTTP client/server, DNS/Unix-socket breadth, dynamic libraries, OS debug formats (`ELF`, DWARF, image parsing), atomics, threads, SIMD, target/ABI databases, and platform-specific C/POSIX wrappers. Each stays package-later, backend-later, freestanding-later, or research-later until a workload forces it.

### Task R-0066

**Objective:** Add stdlib docs and examples for C/Rust users:

 `docs/stdlib/STDLIB_GUIDE.md` plus `examples/stdlib_recipes/bytes_text`,
 `path_fs`, `result_errors`, `vec_map`, `parser_cursor`, `json_scan`,
 `base64_cli`, `uri_parse`, `checksum`, `deterministic_rand`, `time_log`,
 and `capability_io`. Each recipe must show the exact `std.*` imports,
 capability set, allocation behavior, and expected audit line.
 Add one blessed systems-program shape, inspired by Elm's success at
 teaching a single architecture but adapted to Concrete's domain:
 **read/acquire -> parse -> validate -> transform -> emit/release**. This is
 not a framework and not hidden control flow; it is an example/documentation
 convention for ordinary tools and protocol handlers. The guide must show
 which stages are pure, which carry capabilities, where `Result` flows, where
 runtime obligations attach, and which report proves each claim. Add
 `examples/stdlib_recipes/pipeline_shape/` with a tiny byte-oriented CLI and
 a no-alloc parser variant; both must include the source, expected
 `--report caps`, `--report contracts`, and `--report profile` snippets.
 Include a small **stdlib style-coherence pass** in this item: each module
 must choose and document when an operation is a method versus a free
 function. Do not mix `s.len()`, `string_length(s)`, and
 `string_push_char(&mut s, c)` arbitrarily in the same public surface. The
 rule should serve explicitness and capability visibility: methods are fine
 for operations whose receiver clearly owns/borrows the authority; free
 functions are fine for cross-type operations or functions whose capability
 story would be clearer outside a receiver. Add examples that teach the
 chosen style instead of preserving accidental historical names.
### Task R-0067

**Objective:** Add a stdlib compatibility/oracle corpus under `examples/stdlib_compat/`: `fmt_parse_vectors`, `bytes_text_vectors`, `path_vectors`, `collection_vectors`, `base64_vectors`, `uri_vectors`, `json_vectors`, `semver_vectors`, `sort_search_vectors`, `checksum_vectors`, `rand_vectors`, and `cli_io_vectors`. Wire it with `scripts/tests/check_stdlib_compat.sh`; every vector must declare exactly one mode in `manifest.toml`: `oracle_python`, `oracle_system_tool`, `interp_vs_compiled`, `audit_only`, or `negative_expected_failure`.

### Task R-0068

**Objective:** Add real stdlib workload checks before Phase 8 relies on the library.

 Start with `examples/base64_cli`; it is the first
 workload, not just one item in the set. Then broaden to
 `json_validator`, `ini_parser`, `checksum_cli`, `http_headers`,
 `path_normalizer`, and `lru_cache` or `ring_buffer`. Wire them with
 `scripts/tests/check_stdlib_workloads.sh`. Each workload must build, run,
 compare interpreter-vs-compiled output, and report authority/allocation/
 evidence classes. The oracle requirement is explicit per workload:
 `base64_cli` against Python `base64`, `json_validator` against Python
 `json`, `ini_parser` against a checked-in Python reference,
 `checksum_cli` against Python/hashlib or a checked-in reference,
 `http_headers` against checked-in vectors, `path_normalizer` against
 checked-in platform-specific vectors, and `lru_cache`/`ring_buffer` against
 a checked-in reference model.
### Task R-0069

**Objective:** Add a stdlib sentinel/arithmetic audit before broadening hosted APIs. The checked-arithmetic flip exposed syscall/sentinel-style code that relied on silent wrap (`-1 as unsigned` followed by `+ 1`, size/error sentinels, bit-packed flags, checksum/hash arithmetic). Add `scripts/tests/check_stdlib_sentinel_arithmetic.sh` or fold the checks into `check_stdlib_workloads.sh`: every intentional modular/sentinel operation in `std.fs`, `std.process`, `std.net`, `std.io`, hash/checksum code, and binary parsers must use explicit `wrapping_*`/bit operations with a comment or a report-visible classification; every non-intentional trap must be fixed as a

 real bug, not papered over. The gate should include at least one non-
 arithmetic workload path (text/path/UTF-8, fs/process/env capabilities,
 maps/iterators, formatter/parser round-trip, or proof/report views) so the
 post-arithmetic validation does not overfit to numeric examples.
### Task R-0070

**Objective:** Add the Phase 7 validation project:

 `examples/stdlib_client/` plus `scripts/tests/check_phase6_stdlib.sh`.
 The client must use `std.option`, `std.result`, `std.bytes`, `std.text`,
 `std.path`, `std.vec`, `std.map`, `std.fs`, `std.io`, `std.fmt`,
 `std.parse`, either `std.json` or `std.base64`, deterministic RNG or
 checksums, sanitizer/runtime-instrumentation hooks where available, and
 `std.test`. CI must build, run, test, audit authority/allocation/evidence
 classes, and compare interpreter-vs-compiled behavior.

### Task R-0071

**Objective:** Close Phase 7 against the Phase 7 Completion Contract, move every fully completed task body to CHANGELOG, leave only future-relevant invariants in this document, and then continue directly with Task R-0072.


## Phase 7.5: Usable QBE Backend And Independent Validation

Goal: add QBE as a real, user-selectable native backend for ordinary Concrete
programs and use its independent implementation to find shared lowering,
interpreter, ABI/layout, and LLVM-emission bugs before the Phase 8 flagship
workloads deepen the public claims. Differential validation is a required duty
of the backend, not its sole purpose. QBE need not replace LLVM or prove that
native execution preserves source semantics to be useful and genuinely usable.

Done when: a QBE backend consumes verified/cleaned Concrete SSA directly,
supports a documented useful subset with no silent LLVM fallback, and works
through the normal `build`, `run`, `test`, project, artifact-retention, and
diagnostic workflows—not only a special differential-test harness. At least one
Phase 8-class program must be buildable and runnable by a user with
`--backend qbe`. CI must also classify interpreter/LLVM/QBE disagreements over
an edge-heavy corpus by stdout, stderr, exit status, and failure class. The
audit/evidence surface labels QBE results `tested` or `backend_trusted`; this
phase cannot upgrade a source/Core proof to native-code evidence. Release-grade
target breadth, translation certificates, full debug information, optimization
policy, and long-running platform qualification remain owned by Phase 15.

### Product Contract: A Backend, Not A Test Adapter

- `--backend qbe` is accepted anywhere `--backend llvm` is accepted: single
  file compilation, project builds, `run`, `test`, examples, retained
  artifacts, debug bundles, and reproducible replay commands.
- A successful QBE build produces a normal native executable with an explicit
  target/toolchain/link manifest. Users do not need to invoke the differential
  harness or understand QBE IL to run it.
- Supported programs are defined by a generated capability matrix. Unsupported
  operations, ABI shapes, targets, or builtins receive a stable diagnostic
  before partial code generation; they never fall back to LLVM silently.
- QBE owns real backend concerns: instruction selection, QBE IL legality,
  runtime-helper selection, object/link inputs, entry and test wrappers,
  platform ABI realization, artifact naming, diagnostics, and toolchain facts.
- The backend ships initially as `experimental`, which describes compatibility
  and release guarantees—not a toy implementation. Its documented supported
  subset must work end to end and remain protected by ordinary user-workflow
  tests in addition to differential tests.

### Preflight Refactors: Make The Backend Boundary Real

These are enabling refactors, not a new optimizer or a premature second middle
end. Zig's useful lesson is the stable analyzed input shared by every backend;
Concrete already has that candidate in verified/cleaned SSA. Make the boundary
explicit before duplicating LLVM-era assumptions in QBE.

Opening dependency order: provision and pin the QBE toolchain on eligible CI
hosts; extract backend-neutral `LayoutFacts`; define the typed QBE IL/fragment
model; introduce only the `CodegenInput` and driver surface needed to run the
first vertical slice; then expand QBE coverage. The numbered preflight inventory
below records all obligations, not permission to complete every abstraction
before `main -> 42`. Layout panic-to-diagnostic conversion follows the layout
split. Existing LLVM emission moves to fragments only when a shared consumer or
profiled bug 027 justifies it.

The correspondence is architectural, not literal:

| Zig role | Concrete role in Phase 7.5 |
| --- | --- |
| AST / ZIR | parsed syntax / checked Core |
| AIR | verified and cleaned Concrete SSA |
| isolated codegen-job input | immutable `CodegenInput` + `CodegenFacts` |
| LLVM backend | structured LLVM emitter + existing toolchain |
| self-hosted native backend | QBE emitter + pinned QBE toolchain |
| target MIR / encoder | deliberately delegated to QBE; not built in Phase 7.5 |
| linker job | backend-neutral Concrete driver invoking the declared linker |

Hard boundary: do not rename Concrete IRs to imitate Zig, introduce ZIR/AIR as
extra layers, or build target MIR/register allocation/object writers merely for
architectural symmetry. Copy the separation of responsibilities, immutable
job shape, coverage discipline, and gradual backend enablement. QBE provides a
useful native compilation path while letting Concrete defer target MIR,
register allocation, and object writing until owning those layers is justified.

Execution discipline: build this phase as thin executable vertical slices, not
as a backend-framework rewrite followed by a distant first program. Every slice
must end with an eligible fixture running through interpreter, LLVM, and QBE;
the same commit adds its capability row, differential observation, negative or
mutation case, and retained failure artifacts. A refactor-only slice is allowed
only when an existing LLVM behavior/IR parity gate consumes it immediately and
the next QBE slice names it as a prerequisite. Do not land unused interfaces,
generic plugin machinery, speculative target abstractions, or a second
representation merely because a future backend might need them.

First executable milestone (before broad preflight completion): compile
`fn main() -> Int { return 42; }` through cleaned SSA into typed QBE IL, validate
it, invoke the pinned QBE toolchain, assemble/link it, observe clean stdout and
exit status 42, and compare that structured observation with interpreter and
LLVM. `--emit-qbe`, `--backend qbe`, unavailable-tool diagnostics, no-fallback
behavior, and a debug bundle containing source/Core/SSA/QBE IL/tool facts must
all work for this one program. Then expand in this order unless a failing
backend or differential case forces a different dependency: branches/phi -> direct calls -> integer widths
and casts -> checked arithmetic/traps -> stack memory -> f32/f64 arithmetic,
comparisons, and checked conversions -> globals/strings -> aggregates/enums ->
indirect/extern calls -> builtins -> test mode. Each step must stay runnable;
no "all operations implemented, testing later" batch.

LLVM-refactor rule: moving existing code behind `CodegenInput`, layout facts,
the runtime manifest, or the backend driver must initially produce
byte-identical LLVM IR for the gated corpus. If byte identity is impossible or
undesirable, the slice must state the exact textual change, prove equal native
observations in debug/release modes, run LLVM validation, and add a regression
for the changed invariant. QBE progress cannot be used to waive LLVM evidence.

### Task R-0072

**Objective:** Provision QBE as an explicit toolchain dependency. Pin a supported release and checksum in Nix/CI, verify the installed binary rather than accepting an arbitrary `PATH` match, document source/bootstrap and system-package paths, and make local discovery available through `concrete doctor` / agent feature JSON. Tool download is never an implicit compilation side effect.

CI must cover a clean checkout and reject an unsupported QBE version with a
clear diagnostic; updating the pin requires the quick matrix plus scheduled
corpus and an attached toolchain-diff record. Check in a host/target
capability matrix discovered from the pinned binary and verified by compile/
assemble/link probes, including `x86_64-linux -> amd64_sysv` and each macOS
runner's real Apple target. Cross targets may be emit/assemble-only when no
declared runner exists; executable rows require a named host or emulator.
Do not infer support from an OS or CPU-family label alone.

### Task R-0073

**Objective:** Introduce an immutable `CodegenInput` wrapper around cleaned `ValidatedSSA`, plus `CodegenFacts` derived once per program: target-neutral type/layout facts, function signatures, symbol identities, module-local aliases, entry/test metadata, string/global inventory, builtin identities, runtime-helper requirements, and source-span mappings. This is not a new IR and must not rewrite instructions. Both LLVM and QBE consume the identical value; a hash of its canonical form appears in backend artifacts and differential bundles. Eliminate emitter-owned rescans that can disagree by program/module order (the bug-039 class).

### Task R-0074

**Objective:** Replace `Pipeline.emit : SSAProgram -> String` with a statically dispatched backend driver returning a structured `CodegenArtifact`:

 `{backend, target, codegen_input_hash, primary_text, auxiliary_files,
 runtime_helpers, link_inputs, diagnostics, toolchain_facts}`. Use a closed
 `BackendKind` sum (`llvm | qbe`) and ordinary Lean functions, not a dynamic
 plugin system or effectful callback framework. Keep pure emission separate
 from external validation, assembly, and linking.
### Task R-0075

**Objective:** Consolidate the duplicated file/build/test compilation paths in `Main.lean` behind one backend-neutral driver:

 `prepare -> emit -> verify -> assemble/codegen -> link`. Backend-specific
 code supplies artifact extension, verifier/tool invocation, target flags,
 and link inputs; CLI code owns diagnostics, artifact retention, and process
 policy once. Add failure-injection tests at every boundary so a QBE error
 cannot take a less structured path than an LLVM error.
### Task R-0076

**Objective:** Split target-neutral layout computation from LLVM rendering. Rename or move `tyToLLVM`, LLVM type-definition construction, target triple/data-layout text, and LLVM ABI spellings out of `Concrete/Check/Layout.lean`; retain one checked `LayoutFacts` source for size, alignment, field offsets, enum tag/payload placement, pass-by-pointer decisions, and canonical builtin aggregates. LLVM and QBE render those facts independently. No emitter may recompute layout from syntax or host assumptions.

 Sequencing dependency: perform this ownership split before the queued
 Layout panic-to-diagnostic conversion, then convert the panic paths in the
 target-neutral `LayoutFacts` owner and LLVM-specific renderer where they
 actually remain. Gate the split with byte-identical LLVM IR first so the
 hygiene slice does not move and rewrite the same functions twice.
### Task R-0077

**Objective:** Extract a versioned `RuntimeABI`/`BuiltinCodegenManifest` from `EmitSSA.lean`: symbol names, signatures, ownership/capability class, failure/trap behavior, required helper, and backend support status. Keep builtin semantic behavior detection in the existing interpreter/compiled gate; this manifest owns codegen availability and linkage, not a second signature or semantic truth source. A new builtin must fail closed until both backend rows are classified.

### Task R-0078

**Objective:** Make backend emission function-local and deterministic where possible.

 Each function job receives immutable `CodegenInput`/target facts and returns
 a function fragment plus declarations/helper requirements; it may not
 mutate program-wide alias, type, string, or symbol state. Merge fragments in
 canonical symbol/module order and deduplicate declarations through typed
 keys. Start sequentially, add a serial-versus-parallel byte-identity gate,
 and only then allow parallel codegen. This follows Zig's successful isolated
 codegen-job shape without pulling in its linker architecture.
### Task R-0079

**Objective:** Define a legalization responsibility table before QBE instruction coverage:

 for every `SInst`/terminator, state whether Concrete SSA already expresses
 the exact semantics, a shared target-neutral expansion is required, or the
 backend must select an independent sequence. Shared legalization is allowed
 only for language semantics that must be identical (checked arithmetic,
 normalized shift counts, trap classification, aggregate copy intent);
 instruction selection and target realization remain independent so the
 oracle does not correlate backend bugs. Every legalization carries source
 span and semantic-family identity into emitted artifacts.
### Task R-0080

**Objective:** Add a generated backend capability matrix keyed by the constructors of `SInst`, `STerm`, `Ty`, ABI shape, builtin, target, and mode (normal/test/debug/release). Coverage derives from compiler constructors, not a hand-maintained prose list, so adding an IR operation makes the gate fail until LLVM/QBE support is declared and tested. Expose the matrix in `concrete agent features --json`, debug bundles, and the Phase 7.5 capstone.

### Task R-0081

**Objective:** Preserve oracle independence with a review/firewall gate. QBE modules may import SSA, target-neutral layout/codegen facts, diagnostics, and semantic identities, but not LLVM AST/printer/emitter modules. LLVM and QBE may share runtime helpers only when the manifest discloses the correlation. Add an import-direction test and a source scan preventing calls from one emitter into the other.


### Architecture And Independence Rules

### Task R-0082

**Objective:** Preserve one frontend and one lowering spine:

`source -> Core -> Mono -> ValidatedSSA -> SSACleanup`, followed by sibling
LLVM and QBE emitters. QBE must consume `SModule` / `SInst` (or a minimal
target-neutral view over them) and must never translate emitted LLVM IR or
reuse LLVM instruction-selection text. Share semantic facts such as integer
width/signedness, size/alignment, symbol identity, builtin identity, and
entry-point policy; keep instruction selection, aggregate materialization,
ABI spelling, wrappers, globals, and runtime-helper emission independent.
### Task R-0083

**Objective:** Add an explicit backend selection surface:

`--backend llvm|qbe`, `--emit-qbe`, and the corresponding project-build
setting. LLVM remains the default throughout Phase 7.5. Reject an unknown or
unavailable backend with a structured diagnostic that names the missing
tool/target; never fall back to LLVM after the user selects QBE.
### Task R-0084

**Objective:** Add `Concrete/Backend/QBE.lean` for a small typed QBE IL AST, `EmitQBE.lean` for the pure text printer, and an SSA-to-QBE translation module separate from `EmitSSA.lean`. Do not build QBE text through ad-hoc concatenation inside CLI code. Pin the supported QBE release/tool identity in CI and record it in debug/audit output.

### Task R-0085

**Objective:** Keep layout facts backend-neutral. Split LLVM spelling out of `Concrete/Check/Layout.lean` where necessary, but do not create a second size/alignment calculator. Add a gate proving LLVM and QBE consume the same Concrete layout decisions while independently realizing them.


### Staged Implementation

### Task R-0086

**Objective:** Land a scalar smoke slice first: integer and boolean constants, arithmetic, comparisons, casts, direct calls, returns, blocks, branches, phi/copy lowering, stack slots, loads/stores, globals, string data, and the stage-2 main wrapper. Map Concrete `i8`/`i16`/`i32`/`bool` temporaries deliberately onto QBE word operations with explicit truncation and signed/unsigned extension; map `i64`/native pointers to long and `f32`/`f64` to single/double.

Gate negative, wide, shift, division/remainder, overflow, NaN, signed-zero,
and exit-code edge cases rather than accepting scalar happy paths alone.
### Task R-0087

**Objective:** Give floating point its own executable slice rather than treating the `f32`/`f64` type mapping as coverage. Add arithmetic; ordered/unordered comparisons; NaN, infinity, signed-zero, subnormal, and precision-change vectors; int-to-float conversions; and checked float-to-int conversions.

 Concrete's finite/range/trap policy must be emitted as explicit guards and
 runtime trap edges before QBE conversion instructions—raw QBE `stosi`,
 `stoui`, `dtosi`, or `dtoui` behavior is not Concrete's checked-cast
 semantics. Until this slice lands, every float family is an explicit
 `qbe_pending` capability row rather than silently absent from the subset.
### Task R-0088

**Objective:** Add memory and aggregate support: fixed arrays, structs, packed/explicitly aligned layouts, enums, canonical `Option`/`Result`, strings, vectors, nested places, byte-union payloads, aggregate copies, and small/large aggregate calls/returns. QBE aggregates are memory/ABI objects rather than general SSA temporaries, so make materialization explicit and test tag/payload offsets, padding, alignment, and partially initialized payload storage.

### Task R-0089

**Objective:** Add calls and platform boundaries: indirect calls, externs, variadics used by the runtime, argc/argv accessors, symbol visibility, linker aliases, C ABI scalar/aggregate classification, and Linux/macOS target selection for the architectures QBE and Concrete both declare supported. Unsupported target, calling convention, TLS, or ABI shapes must fail before emission with a stable diagnostic.

The ABI matrix must cover 1–8-byte and 9–16-byte aggregates, mixed
integer/float classes, register exhaustion with stack fallback, aggregates
larger than two eightbytes (`MEMORY` class), unaligned/opaque aggregates,
and caller-provided storage for memory-class returns. Do not encode a false
“greater than 16 bytes is unsupported” rule: QBE's supported ABI paths must
be tested; only a shape that the pinned target/toolchain actually cannot
realize receives a named `qbe_pending` row or runtime-mediated boundary.
### Task R-0090

**Objective:** Port the compiler-emitted builtin/runtime surface needed by the validation corpus. Prefer a small shared C runtime for allocation, IO, traps, and other platform services when that keeps semantics explicit; keep pure scalar builtins in the emitter when independence is more valuable. Extend the builtin-semantics inventory so every builtin is one of `qbe_equal`, `qbe_runtime_helper`, or `qbe_pending`; adding a builtin without an explicit class fails CI.

### Task R-0091

**Objective:** Add QBE test-mode entry generation so `concrete test` can execute the same discovered tests and module filter under both native backends. Preserve the test runner's result accounting and output exactly; do not special-case a smaller, silently different QBE test semantics.


### Differential Validation And Failure Localization

### Task R-0092

**Objective:** Add `scripts/tests/check_qbe_backend.sh` and a reusable differential driver that runs each fixture through the source interpreter, LLVM, and QBE and compares a structured observation:

 `{compile_class, stdout_bytes, stderr_bytes, exit_status, runtime_class}`.
 Normalize only declared platform noise; never normalize program output,
 trap kind, or exit status merely to make paths agree.
### Task R-0093

**Objective:** Classify every mismatch rather than printing a generic wrong-code failure:

 - interpreter == QBE != LLVM: `llvm_backend_mismatch`;
 - interpreter == LLVM != QBE: `qbe_backend_mismatch`;
 - LLVM == QBE != interpreter: `interpreter_or_shared_lowering_mismatch`;
 - all three differ: `semantic_split`;
 - both compiled paths agree with an external/hand oracle but the
   interpreter differs: `interpreter_mismatch`;
 - both compiled paths agree with each other and disagree with the external/
   hand oracle: `shared_lowering_or_spec_mismatch`.
 Save sources, Core, cleaned SSA, LLVM IR, QBE IL, tool versions, commands,
 observations, and classification in the wrong-code/debug bundle.
### Task R-0094

**Objective:** Require an independent expected result for cases capable of exposing a shared lowering error: a hand-authored expectation, checked-in reference model, system-tool/Python oracle, or metamorphic property. Two backends agreeing is corroboration, not correctness, because both consume the same lowered SSA.

### Task R-0095

**Objective:** Build the validation corpus by semantic family, not file count: every `SInst`, terminator, scalar width, signedness-sensitive operation, aggregate/layout class, direct/indirect call shape, builtin class, trap, and main/test wrapper path needs a manifest row. Seed it with the existing arithmetic, cast, enum-union-layout, nested-place, callable-value, fuzz, workload, wrong-code, and exit-model corpora. Add mutation checks proving the differential suite notices at least one injected fault in lowering, LLVM emission, QBE emission, interpreter semantics, layout, and builtin behavior.

### Task R-0096

**Objective:** Run a deterministic quick QBE matrix on every CI change and a larger rotating fuzz/differential matrix on schedule. QBE-unavailable CI is a hard configuration failure for the QBE job, not a skip. Keep LLVM-only release jobs intact until Phase 15 graduates QBE; report QBE flakes separately and retain their artifacts.


### Phase Boundary And Graduation

### Task R-0097

**Objective:** Phase 7.5 may close with a declared subset, but that subset must be honest:

 `concrete agent features --json`, help, audit output, and documentation list
 supported targets/features and `qbe_pending` cases. A program outside the
 subset receives a diagnostic; per-function or per-instruction fallback to
 LLVM is forbidden.
### Task R-0098

**Objective:** Exercise the completed subset immediately in Phase 8: every new flagship workload that stays inside it runs interpreter/LLVM/QBE differential checks;

 exclusions require a named missing feature and roadmap owner. Record bugs
 found by the backend/differential suite in the bug corpus with the disagreement classification
 that exposed them.
### Task R-0099

**Objective:** Graduation to a supported release backend occurs only in Phase 15: migrate QBE from the direct validation path onto `ValidatedBackendIR`, pass the full target/toolchain and C ABI matrices, integrate debug/source maps and incremental artifacts, attach translation-validation status, define optimization/tool-version policy, and pass release/platform soak gates.

 Until then `--backend qbe` is explicitly experimental and its native result
 remains backend-trusted/test evidence.
### Task R-0100

**Objective:** After the QBE backend is stable, evaluate a complementary target rather than starting one in parallel. WebAssembly is the provisional next backend, governed by Phase 15.25's entrance criteria and narrow Wasm32/WASI scope.

 This is not an implementation commitment until those criteria pass.
 Cranelift remains the alternative if a forcing workload instead requires
 fast native/JIT compilation or a broader production backend. C emission,
 libgccjit, MLIR, and direct machine code remain comparison options, not
 parallel commitments. No additional execution path starts without a bug
 class or workload that LLVM plus QBE cannot pressure effectively.
### Task R-0101

**Objective:** Define and gate QBE-IL legality independently of execution. Add a pure verifier for Concrete's typed QBE AST (definition-before-use where QBE requires it, unique temporaries/blocks/symbols, legal base/extended type positions, phi predecessor agreement, terminator completeness, entry-block restrictions, call/return ABI agreement, aggregate definitions before use, alignment validity, and escaped identifier/string syntax), then run the pinned `qbe` parser as a second validation layer. An emitter failure must be a structured Concrete diagnostic with the `.qbe` artifact retained; raw QBE/assembler/linker errors must not be the first user-facing diagnosis for

 a compiler-owned invalid artifact.
### Task R-0102

**Objective:** Write a QBE semantic-gap ledger before broad coverage. For every Concrete SSA operation, record the QBE instruction sequence and the assumptions that make it equivalent: signed division overflow and divide-by-zero, shift counts, float-to-int out-of-range behavior, NaNs and signed zero, sub-word truncation/extension, uninitialized bytes, invalid pointers, alignment, aliasing, aggregate padding, unreachable paths, traps, and integer/pointer conversions. Where LLVM uses poison/undef, flags, or intrinsics that have no direct QBE analogue, lower to explicit checks/helpers or mark the operation pending; never inherit accidental host behavior as Concrete semantics.

### Task R-0103

**Objective:** Make symbol and module composition a first-class gate. Test duplicate basenames, import aliases, colliding private names, generic specializations, extern/defined-symbol collisions, linker aliases, string and type-name escaping, multi-module declaration deduplication, visibility, and deterministic ordering. Compile both one combined QBE unit and, once supported, separate units linked together; their observations and exported symbol sets must agree.

### Task R-0104

**Objective:** Add deterministic-emission and reproducibility checks: identical cleaned SSA plus an identical target/toolchain configuration must produce byte-identical QBE IL, assembly where the pinned toolchain permits it, link manifests, and backend observation records. Debug paths, temporary paths, hash-map order, host locale, parallel test scheduling, and environment variables must not perturb semantic artifacts. Record the QBE version, target, assembler, C compiler/linker, runtime-helper hash, and flags in the build/debug bundle and cache key.

### Task R-0105

**Objective:** Add optimization-sensitive triangulation. For every eligible fixture, compare interpreter, LLVM at the supported debug/release optimization levels, and QBE followed by the supported assembler/linker configurations.

 Classify within-backend disagreement separately as
 `llvm_optimization_mismatch` or `qbe_toolchain_mismatch`; retain pre/post
 optimization artifacts where available. Include sanitizer/instrumented LLVM
 runs as corroborating evidence, while keeping sanitizer output out of the
 program-output comparison channel.
### Task R-0106

**Objective:** Extend fuzzing and reduction for the new path. Add backend-aware predicates for QBE compile crash/rejection, QBE runtime mismatch, three-way semantic split, LLVM-only mismatch, shared-compiled mismatch, optimization mismatch, and nondeterministic QBE emission. Reducers must preserve the mismatch classification and relevant target/tool versions, save the smallest reproducer plus replay command, and prove by mutation tests that a reduced case is not accepted merely because one backend stopped compiling.

### Task R-0107

**Objective:** Separate compiler defects from toolchain defects operationally. The bundle and CI report must identify the first failing boundary:

 `ssa_to_qbe`, `qbe_verify`, `qbe_codegen`, `assemble`, `link`, `runtime`, or
 `observation_compare`; capture exit code/signal/timeout/resource-limit and a
 bounded stderr excerpt for each external process. Add time, output-size,
 memory, and recursion limits so malformed generated IL or fuzz cases cannot
 hang the suite, and distinguish timeout/OOM/tool crash from semantic
 disagreement.
### Task R-0108

**Objective:** Establish performance guardrails without making QBE win benchmarks. Track compiler wall time, peak memory where available, emitted IL/assembly/object size, final binary size, and runtime for a small stable corpus against LLVM.

 Phase 7.5 fails on unbounded or accidental regressions, not on a fixed
 QBE-versus-LLVM speed ratio. Performance results are benchmark facts only;
 they cannot choose a backend silently or weaken semantic/evidence gates.
### Task R-0109

**Objective:** Define runtime-helper ownership and versioning. Shared C helpers must have a narrow, documented ABI; checked-in source; deterministic build; symbol namespace; target/feature manifest; sanitizer tests; and interpreter/LLVM/ QBE semantic vectors. Decide explicitly which helpers are common semantic infrastructure and which remain independently emitted oracle logic. A helper change invalidates both backend cache keys and reruns every builtin/ ABI consumer; sharing a helper must be disclosed when it reduces oracle independence.

### Task R-0110

**Objective:** Add release-surface and compatibility discipline even while experimental:

 reserve backend names and project-manifest schema, define `.qbe` artifact
 naming without colliding with Concrete's own SSA dumps, keep LLVM as the
 stable default, and snapshot help/JSON/diagnostic output. Removing or
 changing the experimental backend requires a roadmap/changelog decision,
 but no package may claim generic native support from a QBE-only test. Audit
 bundles must state the exact backend used; examples and documentation must
 not show QBE-derived native evidence as LLVM-derived or backend-independent.
### Task R-0111

**Objective:** Add a Phase 7.5 capstone artifact:

 `examples/qbe_backend_validation/` plus
 `scripts/tests/check_phase7_5_qbe.sh`. It must exercise every supported
 semantic-family manifest row; demonstrate each disagreement classifier
 with controlled mutations; validate deterministic emission, clean-checkout
 tool discovery, reducer replay, debug-bundle completeness, unsupported-
 feature diagnostics, no-fallback behavior, target/ABI facts, and quick-CI
 wiring; and publish a machine-readable coverage/pending inventory. Closure
 requires zero unexplained mismatches, successful normal `build`/`run`/`test`
 use on the declared subset, and either a recorded real defect found by the
 differential suite or successful injected-fault detection for every owned
 fault class. All pending rows need an owner and Phase 8/15 disposition.
### Task R-0112

**Objective:** Add a refactor-retirement gate. Once LLVM runs through `CodegenInput`, the structured backend driver, and the extracted layout/runtime facts with byte/behavior parity, delete the old direct `Pipeline.emit -> String` path, duplicated CLI compile/link branches, and emitter-local program-wide fact discovery. Do not carry two LLVM pipelines through the QBE implementation;

 the compatibility window must have a named removal commit and mutation-
 sensitive parity gate.
### Task R-0113

**Objective:** Keep a Phase 7.5 decision log tied to implementation evidence. For every vertical slice record: the supported semantic families, newly shared facts, deliberately independent logic, LLVM textual changes (if any), three-way result counts, mutations caught, performance deltas, and remaining blocker.

 If the first three executable slices show that `CodegenInput`, the typed QBE
 AST, or the driver boundary creates duplication without improving failure
 localization, revise or delete that abstraction before expanding coverage;
 the roadmap is a hypothesis, not permission to preserve a bad framework.

## Phase 8: Flagship Depth And Examples

Goal: produce examples that outside systems engineers find impressive, not only
internally coherent.

Done when: the showcase set includes a serious security/crypto or protocol
example with proof/evidence strong enough to anchor the public pitch.

Maintain the graduated showcase, deepen theorem coverage where it strengthens
public claims, and add examples only when they force a named surface or claim.

This phase is also the external-credibility probe for the compiler/evidence
pipeline: Phase 6B's `diff-caps` artifact gives the first narrow reviewable
delta, and this phase turns that style of replayable evidence into a non-author
example or workload transcript before the larger package/release machinery
depends on it.

### Incremental Architecture Track

Execute the following migrations after the QBE boundary has supplied concrete
consumers. Each is an independently green vertical slice, never a compiler-wide
rewrite; production entry points remain runnable and LLVM output is byte-stable
unless the task records and gates an intentional delta.

### Task R-0114

**Objective:** Introduce stable semantic identities at definition boundaries.
Give modules, definitions, fields, variants, binders, monomorphized instances,
blocks, values, runtime checks, and proof subjects stable typed identities where
text names or list position still act as identity. Treat bugs 039–045 and
050/051/054/055 as the forcing family: alias capture, first-match binder/type
tables, indirect-call hijacking, generic-enum layout identity, non-injective
monomorphized names, and renamed-import callee identity. Alpha-renaming remains
the completed local fix for bug 045; this task eliminates the recurring
architectural precondition without relabeling narrow fixes as temporary.


Acceptance: IDs originate once at the owning boundary, survive rename/re-export
and semantics-preserving moves as specified, change when their semantic owner
changes, appear in diagnostics/debug/proof artifacts, and are mutation-tested
against first-match/name-collision regressions. Migrate one consumer at a time
and delete each superseded text-keyed lookup in the same slice.

### Task R-0115

**Objective:** Build one immutable indexed ProgramFacts store Derive module/definition indexes, signatures, layouts, builtin identities, aliases, target/runtime facts, call relationships, and source/semantic identity once per program. Passes query typed keys rather than rescanning lists or building private first-match tables. Facts are immutable within a compilation;

transformations produce a new version with an explicit invalidation boundary.

Acceptance: compare old and new derivations over the corpus before cutover,
fail on duplicate/missing keys, record deterministic hashes and construction
cost, demonstrate linear-time builders, and gate that two consumers cannot
derive conflicting values for the same fact.

### Task R-0116

**Objective:** Make successful compiler pass boundaries type-safe Introduce thin opaque results such as `ParsedProgram`, `ResolvedProgram`, `CheckedProgram`, `ElaboratedCore`, `CanonicalCore`, `MonomorphizedCore`, `LoweredSSA`, `ValidatedSSA`, `CleanedSSA`, and `CodegenInput`. Constructors live with the pass that earns each invariant; reports, backends, tests, and projects cannot call a later consumer with an earlier-stage value.


Acceptance: migrate one production edge per green slice, preserve diagnostics
and artifacts, add compile-time negative fixtures for illegal handoffs, expose
no unwrap/backdoor constructor, and delete the parallel raw-list entry point as
each boundary lands. Do not introduce a universal pass monad.

### Task R-0117

**Objective:** Replace the temporary stdlib scanner with compiler-derived interface facts Emit canonical public symbol/signature, visibility/construction rights, capabilities, allocation, ownership, failure, trust, platform, and evidence facts from the compiler's resolved/checked interface artifact. Compare them against the repaired balanced scanner until every row agrees, then delete the scanner rather than retaining two authorities.


Acceptance: nested generics/function pointers, externs, trusted impls,
overloads/renames/re-exports, private representation, duplicate names, and all
public std modules are covered; mutations of each fact class fail closed; API
snapshots, docs, audit output, incremental keys, and backend manifests consume
the same artifact.

### Task R-0118

**Objective:** Generate a self-enforcing IR/pass/consumer coverage matrix Generate, from compiler constructors and interface facts, the matrix spanning Check, CoreCheck, Interp, Lower, Verify, Cleanup, LLVM, QBE, reports, source identity, obligations/proofs, and tests. Every constructor is explicitly classified as implemented, intentionally no-op, rejected-before-here, runtime-helper, pending, or not-applicable; catch-all matches do not count.


Acceptance: adding a synthetic constructor or deleting any consumer row makes
the gate fail; every row links to its implementation and discriminating test;
the matrix is published through debug/agent artifacts; and the former manual
inventories are deleted or reduced to generated snapshots.


### Task R-0119

**Objective:** Maintain the five graduated flagships and keep their evidence bundles green:

`parse_validate`, `crypto_verify`, `fixed_capacity`, `constant_time_tag`,
and `hmac_sha256`.
### Task R-0120

**Objective:** Add stretch theorem for `fixed_capacity`: multi-iteration ring invariant or stronger push/search property.

### Task R-0121

**Objective:** Add stretch theorem for `parse_validate`: success-path / failure-completeness theorem once proof ergonomics support it.

### Task R-0122

**Objective:** Audit the next stronger real-crypto candidate only if it forces a new public claim: Ed25519 verification subset, AEAD, or a post-quantum primitive.

### Task R-0123

**Objective:** Add only the ProofCore surface that candidate forces: shifts, bitand, u32 compound loops, rotations, byte-to-word packing, and multi-round invariants.

### Task R-0124

**Objective:** Keep `hmac_sha256` as the regression anchor for exact-extraction, spec-drift-tied refinement: source perturbations must make the registered proof stale, and ProofKit refactors must keep the 11 proof checks green.

### Task R-0125

**Objective:** Keep the paper, website, README, and showcase manifest aligned with HMAC's actual claim: exact extracted source refines an independent SHA-256/HMAC spec under named assumptions and trusted backend boundaries.

### Task R-0126

**Objective:** Use HMAC-derived proof patterns only after they move into ProofKit or an explicit example guide; do not let future flagships copy private `Sha256Refine` scaffolding as hidden infrastructure.

### Task R-0127

**Objective:** Graduate one runtime-error-obligation flagship: parser/protocol example with no OOB/div-zero/overflow obligations discharged.

### Task R-0128

**Objective:** Graduate one authority/capability flagship: a privilege-separated tool whose trusted core cannot touch files/network/processes except through named wrappers.

### Task R-0129

**Objective:** Graduate one FFI-wrapper flagship: trusted C boundary, safe pure core, explicit assumptions, layout/ABI evidence.

### Task R-0130

**Objective:** Graduate one ownership-heavy resource flagship: explicit cleanup, borrow-heavy APIs, no leaks/double-use, and evidence explaining why.

### Task R-0131

**Objective:** Keep the curated showcase balanced: parser/protocol, bounded state, crypto/security, authority, FFI/trust, ownership-heavy.

### Task R-0132

**Objective:** Add a Unix-tool/protocol compatibility flagship that demonstrates bugs memory safety alone does not catch: byte-preserving I/O, path/OS-string handling, handle-relative filesystem authority, exit-code compatibility, error behavior compatibility, ignored-result diagnostics, and oracle tests against a reference implementation.

### Task R-0133

**Objective:** Add a thin end-to-end credibility slice before the larger workload ladder, so skeptical users can replay one compelling artifact before the full Phase 5/6/12/13 surface is complete. Target:

 `examples/credibility_slice/packet_window/` or an equivalent no-alloc
 protocol parser/verifier (WebSocket frame decoder, TLS record-header parser,
 DNS packet parser, or HTTP/1 header parser). The slice must demonstrate the
 actual production-readiness claim, not a toy proof: fixed buffers, `Bytes`,
 `ByteView`, arrays, `Result`, pattern matching, explicit capabilities,
 value-model accessors / scoped borrows, no returned references, and bounded
 allocation or no allocation. It must include positive and negative corpus
 fixtures, fuzz/property-style generated cases with persisted
 counterexamples, an oracle or interpreter-vs-compiled check, one
 runtime-safety obligation, one source contract, one Lean-checked proof, one
 `proved_by_kernel_decision` discharge, and one audit/release bundle. Wire
 `scripts/tests/check_credibility_slice.sh`; the gate must compile and run
 the example, replay the proof/evidence checks, emit `--report contracts`,
 `--report obligation-ledger`, `--report caps`, `--report layout`, and
 `--report fingerprints`, persist any counterexample as a regression, and
 produce a README transcript that a non-author can run without understanding
 compiler internals. This is intentionally a vertical slice, not a new
 parallel track; it exists to validate the bet before the later 10k-line
 workload ladder. Do not start this slice until the immediate Phase 6
 blockers that directly affect parser/buffer code are either done or
 explicitly deferred with examples: the historical Phase 6 array-literal
 element-inference item, match guards / OR patterns / match-on-reference item,
 and `defer`/cleanup item if the chosen slice owns resources.
### Task R-0134

**Objective:** Add a graduated real-workload ladder. The goal is to make sure Concrete builds real things that can be checked against references, not only tiny proof demos. Each workload must name the surface or public claim it forces;

 otherwise it does not belong in this phase. Do not jump straight to multiple
 10k-line ports before the Phase 5 core slab, Phase 7 stdlib, and daily
 workflow can support them; that would mostly test missing ergonomics.
 Each accepted workload and port tier must leave behind a checked gap
 report (`WORKLOAD_REPORT.md` or manifest fields) naming: missing stdlib
 APIs, painful syntax, ownership/copy friction, error-handling friction,
 proof/contract friction, runtime trap/debug friction, capability/authority
 surprises, cold and representative edit/rebuild latency, predicted Phase 6C
 invalidation fanout, docs/tooling gaps, and which later roadmap item should absorb
 each lesson. A workload is not complete if it only builds; it must either
 feed the linear roadmap or prove that no new item is needed.
 Sequence:
 - **Main compiler repo:** keep tiny proof patterns
   (`examples/proof_patterns/`), evidence-class examples, small real programs
   that gate the compiler, and showcase flagships here. These protect
   compiler/proof correctness and should stay close to the tests.
 - **Medium in-repo real programs after the Phase 5 core slab and Phase 7
   stdlib:** build exactly these first six examples under
   `examples/workloads_medium/`: `mini_toml`, `http_headers`, `tar_index`,
   `bytecode_vm`, `lru_cache`, and `checksum_cli`. Wire
   `scripts/tests/check_medium_workloads.sh`. `mini_toml` forces
   bytes/text/path diagnostics and parse errors; `http_headers` forces
   ignored-result diagnostics and byte-preserving parsing; `tar_index`
   forces path/OS-string boundaries, archive offsets, overflow/cast
   obligations; `bytecode_vm` forces modules, dispatch, bounded loops;
   `lru_cache` forces collections, ownership, frame facts; `checksum_cli`
   forces project model, `concrete test`, and oracle comparison. Tier exits
   only when at least two medium programs build, run, pass
   interpreter-vs-compiled checks, carry full evidence/trust classification,
   and are covered by runtime-obligation audit.
   At least two of these must follow the Phase 7 pipeline recipe
   (`read/acquire -> parse -> validate -> transform -> emit/release`) and
   include a short transcript showing where capabilities enter and where the
   pure core starts. This borrows the teaching value of Elm's one clear app
   shape and Roc's host/application split, while keeping Concrete's
   authority/evidence boundaries explicit.
 - **Separate workload repo later** (`concrete-workloads`): use it for
   larger ports, 2k-10k line
   programs, compatibility suites, external-user programs, benchmark
   workloads, and reference/oracle data that would bloat the compiler repo.
   Do not create this repo until modules/imports, `Concrete.toml`,
   `concrete test`, bytes/text/path, collections, stdlib core APIs, and basic
   diagnostics are usable. The first external-user workload in this repo is the
   external-validation-gate trial from the cross-cutting checkpoint. The repo
   must pin a Concrete compiler/toolchain version in
   `concrete-workloads/Concrete.lock`, include `workloads.toml` with
   `name`, `size_lines`, `source_language`, `oracle`, `evidence_class`, and
   `required_concrete_version`, and be validated by release CI through
   `scripts/tests/check_external_workloads_repo.sh` so it does not silently
   rot as the language changes.
 - **Ported compatibility examples after the daily workflow is stable:**
   `wc`/`cat`/`sha256sum`-style tools, QOI image decoder, base64 library,
   INI parser, tiny glob/regex matcher, arena allocator demo, MMIO-mock
   driver, or protocol codec. Annotate forcing surfaces: glob/regex must
   stay bounded or explicitly outside `PredictableV1`; arena allocator demo
   waits for allocation-profile work that needs it; MMIO-mock driver waits
   for the Phase 16 `with(Device)`/MMIO evidence decision or explicitly
   pulls that decision forward.
 - **10k-line stress ports** only after daily workflow is stable enough that
   the port tests Concrete rather than fighting missing basics. First
   accepted port set: `inih`, `cJSON`, `qoi`, `tiny_regex`, and `tiny_tar`.
   Each port must live under `concrete-workloads/ports/<name>/` and include
   `PORT.toml` with `upstream_url`, `upstream_commit`, `license`,
   `ported_subset`, `line_count`, `unsupported_features`, `oracle_command`,
   `known_trust_boundaries`, and `lessons_for_concrete`. Wire
   `scripts/tests/check_10k_ports.sh`. Tier exits only when at least one
   large port has a pinned reference/oracle suite, interpreter-vs-compiled
   differential coverage, runtime-obligation audit, and release-CI replay.
 Each workload must have a check story: oracle/reference comparison,
 interpreter-vs-compiled differential tests, runtime-obligation audit, and
 explicit evidence/trust classification for what is proved, tested, assumed,
 or trusted.
### Task R-0135

**Objective:** Do not run broad examples cleanup/polish sweeps. Clean examples opportunistically when a roadmap task touches them. Improve examples only when they serve proof-link migration, `concrete prove` authoring, external validation, or a release-facing tutorial. Add an example-refresh checkpoint at every phase closure, and after every two substantial Phase-6/7 usability increments. The checkpoint is not a broad rewrite; it is a small, gate-backed audit that asks whether graduated examples and release-facing docs still teach the current language. It must remove stale "deferred" language for newly landed features; update examples that should

 now use the preferred surface (`if let` / `while let`, range patterns,
 future guards/OR patterns, value-model collection access, `ByteView`,
 explicit capabilities); keep legacy examples only when they intentionally
 demonstrate the old/low-level form; and record any skipped update with a
 reason. Add `scripts/tests/check_example_refresh.sh` once the first refresh
 has enough concrete assertions; until then, each phase closure commit must
 name the examples/docs it checked and why no refresh was needed. The goal is
 to prevent tutorial/showcase drift without turning every feature into a
 repo-wide churn pass.
### Task R-0136

**Objective:** Upgrade the constant-time flagship from `reported` to `enforced` with a secret-dependent-flow checker. Today `constant_time_tag` reports a constant-time source shape and leaves machine timing `assumed`; add a source/IR information-flow pass that rejects secret-tagged values reaching branch conditions, loop bounds, or array indices, so the discipline becomes a compiler-enforced structural property, not a reported shape. This needs no hardware/timing model and must not claim machine-level timing: it produces `enforced` for the source-flow property only, with machine timing still named `assumed`. Mark secrets with an explicit annotation (e.g. `#[secret]`); the

 checker reports `enforced` or a counterexample flow path back to source. Add
 `examples/secret_flow/` with a clean constant-time case and negatives for a
 secret-dependent branch, a secret-dependent index, and a secret-dependent
 loop bound. Wire `scripts/tests/check_secret_flow.sh`; the gate must reject
 every negative and must never present source-flow enforcement as timing
 proof.
### Task R-0137

**Objective:** Add the Phase 8 validation artifact: a showcase/workload dashboard that proves every flagship and graduated workload has a check story, evidence bundle, oracle or reference when appropriate, interpreter-vs-compiled coverage, property-test/counterexample-regression coverage where relevant, runtime-obligation audit, trust/assumption classification, and release-CI replay. The first external-user workload in this dashboard is the external-validation-gate trial. Also publish representative cold pipeline timings and Phase 6C shadow invalidation traces for no-op, private-body, public-interface, proof/contract, policy, and target edits; those traces are

 the workload-derived input to Phase 8.5, not a cache implementation here.
 If the external-validation trial finds manual proof authoring too costly,
 this artifact must also run the exact narrow Task R-0167 synthesis probe
 selected by the gate, preserve its transcript and review-cost measurements,
 and record the final GO/NO decision consumed by the Phase 8.5 trigger. It
 may not silently broaden that probe into the general synthesis roadmap.

## Phase 8.5 / 8B: Incremental Compiler Driver And Artifact Reuse

Goal: make unchanged compiler, proof, report, and backend work reusable without
creating a second semantic pipeline or letting stale evidence remain green.

Trigger: start implementation only after the Phase 8 external-validation gate
returns **GO**, Phase 6B has stable identities/dependency/invalidation contracts
and structured validation records, Phase 6C's shadow edit corpus is green, and
at least one medium Phase 8 workload is large enough to choose granularity from
measurements rather than toy programs.

**Conditional phase rule.** The roadmap remains linear. After Phase 8, a GO
verdict executes this phase before Phase 9. A NO verdict must produce the
required decision record and mark Phase 8.5 explicitly
`rejected-by-validation` or `deferred-by-decision` before Phase 9 proceeds; this
slot may not remain silently blocked while later work advances. A later reversal
requires a new forcing workload and recorded decision.

Prerequisites: Phase 6B determinism, one composed pipeline, stable fact IDs,
dependency graph, pass locality/invalidation, and validation-record contract;
Phase 6C telemetry/replay/shadow manifests; Phase 8 workload edit traces. The
general engine must land before Phase 9's proof cache so proof reuse does not
become a second private database.

Non-goals for V1: no per-token/per-expression query graph, no Datalog or
user-facing rule language, no public long-term compatibility promise for the
opaque internal cache encoding, no remote cache, and no evidence upgrade from a
cache hit or hash match. Start serial and conservatively module/function-grained;
refine granularity and add parallelism only after measurements and equivalence
gates justify them.

Done when: clean, incremental-off, incremental-on, and verify/dual-run modes
produce the same facts, diagnostics, obligations, reports, Core/SSA/LLVM
behavior, and native result; the edit matrix proves conservative bounded
invalidation; no-op and leaf edits reuse semantic and object artifacts; cache
corruption/schema/tool drift recompute or fail closed; and no cached partial or
stale artifact can fabricate a validated boundary or stronger evidence class.

Timing of query-shaping (a deliberate decision, not an omission): the
query/`CompilerSession` form below is built HERE, as part of this
validation-gated driver — it is NOT pulled forward as a pre-GO "query monad
seam." Because this whole engine is gated on the external-validation GO,
threading a query monad through the pipeline earlier would be speculative
insurance against a build that may never happen, plus churn against the
still-moving Phase 6B fact graph. The bounded call-site conversion from the
batch/eager pipeline into queries is accepted as part of this build, post-GO.
Do not re-open a "do the seam early" debate: the seam's only payoff is avoiding
that conversion, and that payoff exists only if the bet validates.

### Task R-0138

**Objective:** Freeze every build revision into an immutable `ProjectInputSnapshot` before evaluating queries.

The snapshot owns canonical project/file/module discovery and exact file
bytes; manifest/lock/policy/profile/target; stdlib and dependency roots;
configured cwd/path normalization, symlink and case policy; SDK/sysroot and
runtime/startup inputs; allowlisted environment variables; and compiler,
checker, solver, clang/LLVM, linker, and other tool identities. Watch events
only trigger resnapshot/content hashing. Query evaluation may perform no
ambient filesystem/environment/tool discovery outside declared input queries,
and an edit during a build belongs to the next revision rather than mutating
the active snapshot.

Give source-bearing artifacts two identities where justified: an exact-source
revision digest for bytes/spans/diagnostics/provenance, and a canonical
semantic AST/interface digest for semantics-preserving reuse. Reusing a
semantic result across whitespace/comment/span edits requires a checked
semantic-hash equivalence rule plus a refreshed revision/span map; span-bearing
output always depends on the exact revision. Gate changed environment/std
path, SDK/sysroot/tool binary, symlink/case alias, add/delete/rename, and
edit-during-build snapshots.

### Task R-0139

**Objective:** Define one typed query/artifact contract and one `CompilerSession` driver.

Add a closed `QueryKey` family and an explicit typed `CompilerDB` fact/edge
store owned by the session. Split every result into (a) a canonical hashed
payload + semantic dependency manifest and (b) non-hashed operational/view
metadata. Only normalized semantic fields, schema, subject identity, and
declared dependency digests enter `output_digest`/the dependency root.
Timings, hit/miss state, cache path/location, absolute/local paths, rendered
replay commands, redaction/display choices, and other operational metadata do
not. Exact-revision diagnostics/provenance use their own revision-bound
digest.

Keep orthogonal status dimensions: the existing proof/evidence class;
`pipeline_validation_status` (`unvalidated | compiler_validated`);
independent `certificate_status`; and `replay_status`. Cache/integrity/
certificate states may not become new proof-evidence classes. The result
envelope also carries dependency keys/digests, producer validation-record id,
fact/diagnostic ids, provenance roots, cacheability, and replay-plan identity.
Required initial families:

- project input: snapshot, loaded-file/module graph, manifest/dependencies;
- file: source/lex/parse/interface-summary/body;
- module: resolve, desugar, check, elaborate, Core canonicalize, CoreCheck;
- function or explicit SCC: calls, ownership/capabilities/effects, ProofCore,
  obligations, discharge inputs, and typed report facts;
- monomorphized instance: stable definition id + canonical type arguments,
  monomorphization and post-Mono verification;
- codegen unit: lower, raw-SSA verify, SSA cleanup, post-cleanup verify, LLVM
  emission/object from `ValidatedSSAUnit` in V1;
- project: module graph, reachability, policy aggregation, link plan, and
  release/bundle view.

If several production stages remain one composite query, its contract must
name every enclosed stage and verifier so `runFrontend`, desugar,
canonicalization, cleanup, or post-pass verification cannot disappear from
the incremental path. Phase 15 schema-bumps codegen inputs from validated SSA
to `ValidatedBackendIR`; Phase 8.5 does not depend on future BackendIR.

Expose `--incremental=off|on|verify` (or equivalent internal modes). Strict,
tolerant, audit, proof, test, report, and codegen operations are demand sets
over the same stage functions, never alternate pipelines. Formatting,
redaction, proof-tool, target, profile, and policy inputs invalidate only
query families that declare them. The eager `off` mode remains the oracle
and debugging path.

Add a query-effect firewall: filesystem reads/discovery, environment
variables, locale/timezone/time, tool/solver discovery and identity, target
data, policy, and external process results enter computation only through
declared input/query APIs. Dynamic dependency recording cannot see ambient
inputs read behind the query engine, so bypassing the firewall is a cache
correctness bug and fails a mutation gate.

### Task R-0140

**Objective:** Add deterministic indexed compiler data and linear-time builders before persisting today's hot whole-program scans.

Introduce measured `ProgramIndex`/`OrderedIndex`-style structures for modules,
symbols, types, functions, layouts, obligations, stable-id lookup, reverse
imports/calls, and monomorphized users. Maps/intern tables may serve lookup;
canonically ordered arrays/lists own iteration, serialization, and output so
hash-table order never leaks. Use dense arrays where stable ids permit them
and builders/reverse accumulation for large strings and collections.
Index-heavy compiler artifacts must carry table/arena identity in the type
or validated artifact schema (`FunctionId`, `TypeId`, `BlockId`,
`ObligationId`, not raw integers). Serialized artifact bytes are hostile
until decoded and validated: cross-table IDs, stale generation IDs where
slots can be reused, and out-of-range IDs must be rejected before they become
semantic facts or cache hits.

This is profiler-driven, not a blanket ban on `List`: replace hot repeated
`find?`/`contains`, `acc ++ [x]`, growing-string append, and repeated global
scans selected by Phase 6C telemetry and bug 027's corpus. Add duplicate-key
and index-finalization verifiers, plus mutations that reintroduce a
quadratic builder and that swap two incompatible typed IDs; both must fail
their gates.

### Task R-0141

**Objective:** Make reusable frontend artifact boundaries operational.

Add or validate durable `ParsedFile`, split `InterfaceSummary` and body
summary, `ResolvedModule`, checked module/program artifact, elaborated module,
and validated Core-module boundaries. Check may no longer return only `Unit`
if downstream reuse needs its ownership/capability/value-flow decisions; it
must produce the durable facts or typed artifact that Elab and later queries
consume. Initially keep imported body digests in the conservative dependency
envelope anywhere resolution, trait/method lookup, Check, or Elab can still
inspect bodies. A module may depend on imported **interface-only** digests—and
therefore survive a dependency-private body edit—only after the interface
completeness gate proves every consumer uses the split artifact rather than
a body side channel. Public API identity and implementation-evidence identity
remain separate. Even then, the unchanged interface permits reuse only for
importer frontend/interface queries: referenced generic bodies,
call/effect/proof facts, monomorphized instances, implementation evidence,
and codegen still depend on the applicable body digest. Gate both an unused
nongeneric private-body edit whose importer frontend remains a hit and a
referenced generic-body edit that invalidates the affected mono/object path.

Strict and tolerant/partial artifacts use different result types. A partial,
recovered, cancelled, or internal-error artifact cannot satisfy strict
codegen, proof, policy, package, or release queries. Introduce only boundaries
demonstrated useful by the workload; `CheckedModule` is a candidate name,
not permission to add a redundant typed tree.

### Task R-0142

**Objective:** Implement the serial in-memory query engine and conservative reverse-dependency invalidation.

Record every actual dependency read while evaluating a query and maintain
reverse edges. Initial recomputation units are file, module, function/SCC,
mono instance, codegen unit, and project as listed above; do not create a
query per expression merely because an id exists. Call/import/recursive
cycles are explicit SCC or fixed-point query families, not accidental query
recursion.

V1 uses a **complete conservative dependency envelope**. A result may be
reused only when that whole envelope is unchanged. A digest authenticates
declared inputs; it cannot detect an omitted semantic dependency. Missing a
reuse opportunity is acceptable; stale reuse is not. Narrow an envelope only
after a gate or theorem demonstrates completeness for that query family.

### Task R-0143

**Objective:** Define deterministic internal codecs before persisting an artifact family.

 Each cacheable type needs a compiler-versioned canonical encoder/decoder,
 explicit ordering and normalization, `decode(encode(x))` semantic equality,
 canonical re-encoding, malformed-input/size-limit fuzzing, and a round-trip
 boundary verifier. Summary hashes from Phase 6C are not a serialization
 format. Internal codecs may invalidate wholesale across compiler versions;
 Phase 18 separately owns stable public package encodings.

### Task R-0144

**Objective:** Add an opaque compiler-versioned local content-addressed store under `.build/concrete-cache/` (or an equivalent project-local path).

Separate the memo/action table from the content store:
`QueryKey + canonical input/dependency fingerprint -> ResultManifest +
output digest`, then `output digest -> canonical artifact bytes`. Keys include
compiler build/internal schema, semantic input/dependency digests,
invariant-set version, target/data layout, build/test profile, policy only
where consumed, and relevant proof/backend/tool identity. Add
atomic writes, locking, checksums, interrupted-write recovery, size/GC policy,
`concrete cache status --json`, and `concrete clean --cache --dry-run`.

Serialized cache entries are untrusted bytes for decoding/integrity purposes.
Loading must follow an explicit `UntrustedCached* ->
UntrustedDecodedArtifact` API after schema/digest/dependency/subject checks;
those checks yield only `cache_integrity_checked` bytes bound to the
**original** producer validation record; they cannot reconstruct
`ValidatedCore`, `ValidatedSSA`, a proof result, or a release fact. Before
Phase 14/15, rerun the current
semantic boundary verifier over every loaded strict Core/SSA artifact before
constructing its in-process validated token and fresh producer record. If no verifier can reconstruct
a boundary, that artifact is not safely cacheable for strict codegen, proof,
policy, package, or release use. This still trusts the compiler's verifier—it
is not independent certification.

Treat the project-local store as inside the same user's integrity boundary:
hashes/checksums detect accidental corruption and substitution relative to a
trusted action mapping, not a malicious same-user rewrite of both mapping and
result. Unknown schema/compiler/invariant versions, truncated content,
conflicting ids, or mismatched dependencies are quarantined and treated as
ordinary cache misses whenever source inputs allow recomputation. Fail only
in an explicit artifact-only/offline operation where recomputation is
impossible. A coordinated malicious rewrite of action mapping, result,
manifest, and internal hashes is outside the local-cache threat model until
an authenticated expected action/root exists; tests must not misdescribe
internal consistency checks as detecting it.

### Task R-0145

**Objective:** Turn proof, obligation, and report work into queries rather than cached rendered strings.

Cache per-function ProofCore extraction, call/SCC classification,
stable normalized obligations, typed proof inputs, and typed evidence/report
rows whose schemas already exist. Project
reports are deterministic views/joins over those typed facts. A cache hit
preserves the original proof/evidence class and replay provenance; pipeline
validation remains a separate dimension. It never turns `tested`,
`solver_trusted`, `assumed`, or `trusted` into proof. Lean, ProofKit, solver,
synthesis-model, toolchain, spec, theorem, dependency-fact, and policy
identities affect only the query families that consume them.

Do not restore `proved_by_lean`, `proved_by_kernel_decision`, or
`kernel_replayed` from an untrusted cached verdict alone. Cache proof inputs,
normalized obligations, generated artifacts, exact proof-workspace/import
dependency digests, and replay plans; replay the
kernel before emitting the current strong status unless a separately
authenticated replay receipt is explicitly admitted by policy and named in
the TCB. Solver/LLM results retain their weaker class and replay provenance.

Phase 8.5 supplies the query namespace and store; Task R-0155 owns the
shared proof-artifact schema and activation of reusable Lean/solver verdicts,
rather than creating a second `.build/concrete-proof-cache/` database. Cache
only successful strict non-verdict artifacts first. Negative/tolerant
diagnostics, filesystem discovery, environment-dependent operations, solver
results, and cancelled work need explicit cache contracts before reuse.

### Task R-0146

**Objective:** Add stable codegen units, an object cache, and deterministic relinking.

First define `ToolchainIdentityV1` / `BackendInvocationV1`: exact clang/linker
executable digests (not version strings alone), SDK/sysroot, allowlisted
environment, target/data layout and ABI, optimization/debug/sanitizer flags,
runtime/startup objects, and every effective tool flag. These inputs come
from `ProjectInputSnapshot` and participate in action keys.

Then add a deterministic `ProgramCodegenPlan` / global ABI-symbol index that
owns reachability, symbol naming/collisions, whole-program signature/layout
facts, builtin/helper/type universe, global/string ownership, externs,
monomorphized-instance ownership, unit imports/exports, and common-versus-test
partitioning. The current whole-program Mono/SSA/emitter decisions must become
explicit plan inputs before a module can emit safely in isolation. V1 unit
input is `ValidatedSSAUnit`; Phase 15 migrates it to validated BackendIR.
Start with one unit per source module plus stable runtime/test-harness units
only if the partitionability gate passes. Later subdivision uses deterministic
stable-id buckets or explicit mono SCCs, never traversal-order chunks.

The object action key includes validated-SSA unit + codegen-plan digest,
backend schema, and `BackendInvocationV1`. The link action key includes
canonically ordered object digests plus linker/startup/runtime identities.
Cache/materialize the final linked artifact by link key if a retained-output
warm no-op is expected to skip the linker; otherwise a missing final artifact
may relink while still reusing every object.

Gate isolated unit emission, duplicate/orphan mono ownership, missing/
colliding symbols, common/test partitioning, target/toolchain invalidation,
corrupt local objects/action mappings, and requested-output materialization.
A leaf edit recompiles only its measured affected units plus link; a retained
warm no-op invokes neither clang nor linker. Phase 15 later adds independent
BackendIR relation checking. Before a native/object validator, an object hit
remains backend/compiler-trusted and its hash proves identity only relative
to the trusted local action mapping—not that LLVM produced correct code.

### Task R-0147

**Objective:** Add a long-lived service/watch surface over the same session, then deterministic independent-query scheduling.

The initial engine is serial. Once serial reuse is correct, allow independent
file/module/codegen-unit queries to execute in parallel with cancellation and
bounded resources. Serial and parallel **Concrete-owned canonical**
artifacts/facts must be byte- and schema-equivalent; external LLVM/object
nondeterminism follows the explicit Phase 15 policy and must not change
compiler facts or native behavior. Cancellation or a worker crash must not
commit partial cache state. Canonically sort diagnostics/events whose order
is otherwise scheduling-dependent. The internal service exists to preserve a
session across CLI or scripted edits; Phase 19's LSP must wrap this exact
session rather than a batch `runFrontend` or editor-only cache.

### Task R-0148

**Objective:** Add cache/query observability and a conservative rollout.

Extend pipeline events with `executed | memory_hit | disk_hit | invalidated |
uncacheable`, reason, dependency ids, bytes, time, validation-record id, and
the separate evidence/pipeline-validation/certificate/replay statuses. Roll
out `shadow -> opt-in -> CI verify/dual-run -> default-on`
only after each stage's corpus is green. `verify` recomputes selected hits and
compares canonical results; any false hit disables that query family and
persists a minimized regression. Audit/why/diff output may explain reuse but
may not cite reuse as correctness evidence.

### Task R-0149

**Objective:** Add the Phase 8.5 validation artifact.

 Create `examples/incremental_driver/`,
 `tests/perf/compiler_pipeline/incremental_driver/edits.json`,
 `scripts/tests/check_incremental_driver.sh`,
 `scripts/tests/check_codegen_object_cache.sh`,
 `scripts/tests/check_incremental_failure_recovery.sh`, and the umbrella
 `scripts/tests/check_phase8_5_incremental.sh`.

 Cover no-op; whitespace/comment; private leaf body; public
 signature/type/capability/contract; dependency private body versus
 interface; generic body/new instantiation; proof link/theorem/spec; policy;
 target/profile; file add/delete/rename; syntax/type error then repair;
 cancellation; cache truncation/corruption; schema/compiler/checker/tool
 drift; coordinated action-map/artifact/dependency rewrite (recorded as
 outside the unauthenticated local-cache threat model, never falsely
 "detected"); object
 substitution; ambient-input firewall bypass; and serial/parallel modes. Compare
 incremental-off/on/verify facts, diagnostics, obligations, reports,
 Concrete-owned canonical Core/SSA/LLVM and manifests byte-for-byte; compare
 normalized external-object metadata and native behavior unless the pinned
 toolchain is separately proved reproducible. Assert query execution sets,
 named unaffected sentinel modules/functions/objects that must remain hits,
 affected outputs that must change, and maximum query-family execution counts
 per edit—not only wall time or the tautology "declared dependencies rebuilt."
 On the Phase 8 large workload, a warm no-op must execute zero **producer
 recomputation** beyond required decode/integrity/boundary-validation queries,
 and a leaf edit must rebuild only declared reverse
 dependencies within the stated count bound. Publish observed/baseline-derived
 no-op and leaf-edit speedups (20x/5x are investigation targets, not phase
 gates or public claims); Phase 17 owns platform-specific release performance
 budgets.

## Phase 9: Proof Authoring And Automation

Goal: make flagship proofs a repeatable engineering workflow, not a collection
of one-off `simp` scripts.

Done when: new flagship proofs can start from useful generated stubs, standard
lemmas, and actionable failure diagnostics.

This phase owns the proof-cost probe from the roadmap spine: before assuming the
proof discipline scales, measure manual proof effort on real flagships, then
measure review effort for the LLM-guided synthesis loop if manual authoring is
too expensive.

### Task R-0150

**Objective:** Instrument the flagships with PROOF-EFFORT TELEMETRY before investing in automation, so the external-validation gate's "was the proof discipline worth the cost?" question has data instead of anecdotes: per proved function, record Lean proof lines, tactic depth, solver/`bv_decide` time, and the source complexity it covers (loops, branches, width casts). Publish a small baseline table for hmac_sha256/constant_time_tag and refresh it per flagship. Cheap to collect now; impossible to reconstruct later.

### Task R-0151

**Objective:** Move example Lean proofs physically next to their Concrete examples. Target layout:

`examples/<name>/src/main.con`, `examples/<name>/proofs/Proofs.lean`,
`examples/<name>/snapshot/...`. Configure Lake/module discovery so these
files are importable by the proof checker (for example as
`Examples.<Name>.Proofs` or an equivalent stable namespace), update
`Concrete.lean`/proof umbrellas so `check-proofs` can see them, retarget
source `#[proof_by]` / `#[ensures_proof]` links, and update
`concrete prove --emit-lean` / `--workspace` to prefer the
`examples/<name>/proofs/` destination. Update the proof-namespace guard to
reject new example proof modules under `Concrete/Examples/`. Pilot on one
small example, then migrate all current example proof modules. Keep registered
spec PExprs in `Concrete.Proof` until the later `ProofCore` /
`SpecRegistry` split.
### Task R-0152

**Objective:** Add proof minimization: `concrete prove --minimize <obligation_id>` emits the smallest source / ProofCore / Lean slice needed to reproduce a failed obligation. Output directory:

 `.build/prove/<function>/<obligation_id>/minimized/` with `source.con`,
 `proofcore.lean`, `replay.lean`, `context.json`, and `README.md`. Wire
 `scripts/tests/check_prove_minimize.sh` with one loop VC, one failed
 postcondition, one stale proof, and one SMT counterexample. The minimized
 artifact must reproduce the same status and stable id without unrelated
 functions.
### Task R-0153

**Objective:** Define and document stable theorem naming conventions in tool output:

 `<fn>_refines_spec`, `<fn>_<obligation>_proved`,
 `<fn>_loop_<name>_preserves`, and
 `<fn>_call_<callee>_discharges_requires`. `concrete prove` should suggest
 these names instead of leaving agents to invent them.
### Task R-0154

**Objective:** Add CI gates for the agent-facing proof surfaces: snapshot representative `--json` output, validate schema versioning, ensure generated Lean stubs parse/check up to the intended placeholder boundary, assert replay JSON reports the same statuses as human replay, and assert proof-check JSON maps a failing Lean proof back to the intended obligation id.

### Task R-0155

**Objective:** Define one proof artifact schema shared by obligations, minimization, `--why`, generated stubs, synthesis attempts, repair plans, stale-proof reports, proof-cache entries, and replay bundles.


 Deliverable: `docs/PROOF_ARTIFACT_SCHEMA.md` plus a schema-versioned JSON
 fixture directory. Required common fields: `schema_version`,
 `obligation_id`, source fingerprint, source span, policy id, evidence
 class, replay command, tool versions, assumptions/trust deltas, and final
 status.

 Done when `--emit-lean`, `--minimize`, `--why`, `--synthesize`,
 `--repair-plan`, proof-cache status, and stale-proof reports all emit this
 shared envelope, and a gate fails if any command invents a private JSON
 dialect.
### Task R-0156

**Objective:** Add human docs only after the binary path exists:

 `docs/AGENT_PROOF_AUTHORING.md` and an optional repo-root `AGENTS.md`
 should summarize the binary workflow and point to the ProofKit guide, but
 they must not be the source of truth for agents using only an installed
 binary.
### Task R-0157

**Objective:** Keep AI/agent assurance guidance aligned with the implemented proof surface.

 `docs/SPARK_CLASS_ASSURANCE.md` is the current design-target guide for
 agents: it tells Claude/Codex-style tools which assurance annotations are
 implemented today, which are future-only, and which replay commands must
 validate a claim. When loop invariants, frame/dependency contracts,
 ghost/spec code, or package evidence land, update this guide in the same
 commit as the feature and add at least one agent-facing example of the
 intended suggestion pattern.
### Task R-0158

**Objective:** Add MCP only after the CLI/JSON/stub/workspace surfaces are stable. The MCP server should wrap the binary rather than duplicate logic, exposing resources such as `concrete://prove/<fn>/obligations`, `concrete://proofkit/lemmas`, and `concrete://examples/evidence-classes`, plus tools for `prove_json`, `show_obligation`, `emit_lean`, `check`, `replay`, and `check_proofs`.

### Task R-0159

**Objective:** Build reusable proof lemmas for arrays: lookup, update, length, in-bounds, OOB stuck behavior.

### Task R-0160

**Objective:** Build reusable lemmas for loop-carried state and `while_step`.

### Task R-0161

**Objective:** Build reusable lemmas for BitVec operations used by flagships.

### Task R-0162

**Objective:** Build reusable lemmas for structs, fields, enum construction, match, Result, Option, and bounded-buffer invariants.

### Task R-0163

**Objective:** Upgrade generated proof stubs for real shapes: arrays, structs, enums, fixed buffers, Result/Option, loops, source contracts, and refinement composition. Stubs should emit spec target, `PExpr` body, FnTable skeleton, expected theorem statement, common imports/tactics, and TODO blocks for loop invariants. These items enrich what `--emit-lean` produces; they do not introduce a second stub generator. Provide a friendly alias such as `concrete prove <file> <fn> --stub` only if it is the same artifact path and schema as `--emit-lean`, not a second proof surface.

### Task R-0164

**Objective:** Add generated composition scaffolds: FnTable entries, call lemmas, callee refinement dependencies, and composed theorem skeletons.

### Task R-0165

**Objective:** Add generated loop-invariant templates for common proof shapes:

 counter loop over array writes, copy loop, fold loop, multi-store loop,
 offset loop, and block-processing loop.
### Task R-0166

**Objective:** Improve failed-proof diagnostics after `--json`, failed artifacts, and `--minimize` exist: classify common failures into actionable categories such as missing callee theorem, stale source link, missing table entry, failed arithmetic bridge, insufficient frame fact, and spec/extraction mismatch. Add `concrete prove <file> <fn> --why <obligation_id>` (or an equivalent `--show-obligation --why` form) to explain why the obligation exists, which source span generated it, which facts are in scope, what evidence classes are allowed, and why automation did not close it.

 Diagnostics should point to the already-generated artifact or next action
 instead of introducing another parallel proof surface. Where a failed proof,
 failed contract, or solver counterexample has concrete inputs or a finite
 witness, emit a runnable `.con` counterexample fixture plus a replay command.
 This is the Dafny/SPARK/F* lesson in Concrete's terms: proof failure should
 feel like debugging a minimized program, not reading a wall of theorem-state
 text. The fixture is evidence of failure only; it must never upgrade a claim
 to proof.
### Task R-0167

**Objective:** Add **LLM-guided proof synthesis, kernel-verified** as a first-class proof-authoring workflow, not as prose-only AI help. Command shape:

 `concrete prove --synthesize <module.fn>` (or an equivalent subcommand)
 runs a closed loop: Concrete emits exact obligations; an LLM proposes loop
 invariants, helper lemmas, or Lean proof scripts; Lean's kernel /
 kernel-backed decision procedures check the artifact; failed attempts are
 minimized and fed back into the next attempt; and only replayed
 kernel-checked artifacts may upgrade evidence. LLM output is never
 evidence by itself. Reports must record model/tool identity, attempt count,
 generated assumptions, rejected assumptions, introduced trusted boundaries,
 final evidence class, replay command, and whether the final artifact works
 without the LLM. Design target:
 `docs/PROOF_SYNTHESIS.md`.

 Product model: the human reviews the spec, assumptions, and evidence class;
 the kernel checks the generated proof artifact. Do not design this as
 "trust the generated proof" or "the LLM decides correctness." If the spec
 is weak, vacuous, or wrong, synthesis has only made the wrong theorem cheap;
 Phase 10/11 spec-adequacy, vacuity, and mutation gates are therefore part of
 this feature's safety story.

 Deliverable: a replay bundle, not an LLM transcript. The bundle must contain
 the obligation, generated candidate artifacts, accepted proof/invariant/
 lemma artifacts, rejected attempts with reasons, policy decisions, and a
 `concrete replay` command that validates the final evidence without network
 access or model access.

 Gates: synthesize a loop invariant for a small loop proof; synthesize a
 missing helper lemma; repair a stale proof after source drift; reject a
 plausible but false proof; reject any attempt that introduces a new
 assumption, trusted fact, extern/Unsafe dependency, or solver-trusted fact
 without policy approval; and produce a replay bundle that verifies without
 the LLM or network. Add red-team fixtures where the agent proposes a proof
 of the wrong theorem, a vacuous spec, an unapproved assumption, and a proof
 script that typechecks only after weakening the claim.

 Agent discoverability is part of the feature, not documentation garnish.
 The installed binary must expose a machine-readable feature/command catalog
 (for example `concrete agent features --json` or `concrete help --json`,
 extending the Phase 6E command catalog rather than inventing a second one)
 that tells LLM tools which proof commands exist, what artifacts they emit,
 what evidence classes mean, which replay command validates a result, and
 which actions are forbidden without policy approval. The same facts should
 be reachable through any later MCP server, but the CLI/JSON catalog is the
 source of truth so agents can discover the workflow without reading the
 repository docs.
### Task R-0168

**Objective:** Add proof-result caching once proof artifacts and fingerprints are stable.

 This item is active only when Phase 8.5 completed after a GO verdict; if
 Phase 8.5 was rejected/deferred, proof replay remains uncached rather than
 growing a private workaround. Implement caching as Phase 8.5 query families in the one
 `.build/concrete-cache/` content-addressed store, not as a separate proof
 database. Cache key: compiler/query schema, toolchain version, source and
 typed-Core fingerprints, spec/proof link, obligation id, dependency-fact
 fingerprints, ProofKit version, proof/solver/synthesis engine version, and
 policy mode where it affects the verdict. Stale or dependency-mismatched
 entries become `needs_recheck`, never green. An LLM-synthesized proof is not
 cacheable as evidence until Lean replays it, and a hit preserves the
 recorded evidence class rather than upgrading it.
 Expose
 `concrete prove --cache-status --json`. Wire
 `scripts/tests/check_proof_cache.sh`; the gate must prove cache hits do not
 mask stale source, stale theorem names, changed policies, or changed solver
 trust settings, and that `--incremental=off|on|verify` produces identical
 proof statuses and replay artifacts.
### Task R-0169

**Objective:** Add simple auto-discharge for structural obligations that do not need human proof search. V1 shapes: reflexive field projection, tuple/struct constructor-destructor round trips, enum tag preservation, fixed-array literal length, direct call wrapper, and source-contract metadata erasure.

 Command surface: `concrete prove --auto <function> --json`, reporting
 `auto_closed`, `needs_lean`, or `not_supported` per obligation. Wire
 `scripts/tests/check_structural_auto_discharge.sh`; auto-discharge may only
 emit `proved_by_kernel_decision` or a linked Lean theorem when the kernel
 actually checks the generated proof.
### Task R-0170

**Objective:** Add operational VC auto-discharge as the next automation tier after structural auto-discharge. Today `linear` and `bitvector` obligations route to `omega` / `bv_decide`, while `operational` and `refinement` obligations route to Lean proof links; that leaves ordinary `#[ensures]` bodies and loop operational-preservation steps dependent on hand-written bridge theorems.


 The completed forcing probe and 4/6 result are recorded in
 [CHANGELOG.md](CHANGELOG.md). The active V1 gaps are generated
 `Int`/`Nat`/`BitVec` shift-amount normalization before `bv_decide` and
 automatic guard splitting; keep one positive fixture and one expected
 boundary failure until the implementation lands.

 V1 core: symbolically execute a narrow Core/ProofCore fragment, unfold the
 generated eval and named spec, split conjunctions and guards mechanically,
 normalize `Int`/`Nat`/`BitVec` casts, route arithmetic leaves to `omega`,
 route bitvector leaves to `bv_decide`, and report surviving leaves as
 `needs_lean` / `not_supported` rather than false green. Defer whole SHA
 compression rounds, message schedules, heap/alias-heavy code, effectful code,
 and induction-heavy specs until separate evidence shows they fit. Design
 note: `research/proof-evidence/operational-vc-auto-discharge.md`.
### Task R-0171

**Objective:** Add an automation trust-upgrade firewall for every proof automation path.

 Any auto-discharge, generated proof, solver route, cache hit, replay helper,
 agent suggestion, or future MCP proof tool must emit a replayable artifact,
 name the engine that closed the fact, preserve the evidence class
 (`proved_by_kernel_decision`, `proved_by_lean`, `solver_trusted`,
 `assumed`, `runtime_checked`, `needs_lean`, `not_supported`, etc.), and
 include a negative gate proving unsupported / stale / effectful / trusted /
 out-of-fragment obligations do not become green. This is the guardrail that
 prevents proof automation from becoming a hidden trust upgrade. Wire the
 first version into the structural and operational auto-discharge gates, then
 extend it to proof-result caching, agent-facing proof suggestions, and any
 external solver path.
### Task R-0172

**Objective:** Add a small verified/spec-checked standard proof library for common predicates and formal stdlib models: sorted, bounded, no-duplicates, fixed-length, prefix, checksum, constant-time source shape, `formal_vec`, `formal_map`, `formal_set`, spec-only `bigint`, and the first reusable lemma families over those models. Treat these as **formal shadow models** for real containers: user proofs reason about the mathematical model, while collection implementations prove refinement facts from `Vec`, `HashMap`, `OrderedMap`, and `HashSet` to the corresponding formal model.


 Deliverable: one end-to-end refinement slice, for example
 `HashMap<K,V>` operation facts lowering to `formal_map`, with replayable
 Lean artifacts and a user proof that consumes only the formal model facts.

 Done when the user proof does not mention buckets/tombstones/capacity, the
 collection implementation proves the bridge facts, the report names the
 refinement evidence class, and a deliberately false refinement is rejected by
 the checker/kernel. This is the tractability unlock for proving real
 programs that use containers.
### Task R-0173

**Objective:** Add bounded quantified specs for collections, not arbitrary open-ended logic. V1 syntax should cover only finite, source-visible domains:

 `forall i in 0..n { P(i) }`, `exists i in 0..n { P(i) }`, and library
 predicates that lower to those bounded forms (`all_bytes_valid`, `bounded`,
 `sorted`, `no_duplicates`, `prefix`, `fixed_length`). These specs must
 lower into ProofCore with explicit bounds, source spans, and generated
 theorem shapes; they may be proved by Lean, by kernel decision procedures
 only for decidable finite fragments, or reported `needs_lean` / `blocked`
 when the fragment is outside automation. Do not add general quantifier
 syntax until a bounded collection fragment has examples and gates. Add
 `examples/quantified_specs/` with `bounded_array`, `all_bytes_valid`,
 `sorted_prefix`, and one rejected unbounded quantifier. Wire
 `scripts/tests/check_quantified_specs.sh`; the gate must prove quantified
 claims are never erased into a vague postcondition and never reported as
 proved unless the generated finite theorem actually checks.
### Task R-0174

**Objective:** Add AI-assisted proof repair only after artifacts, statuses, and replay are stable enough to validate suggestions mechanically. The binary surface is `concrete prove --repair-plan <obligation_id> --json`; it emits no edited source, only candidate next actions with required checks. Required JSON fields: `obligation_id`, `failure_class`, `minimal_artifact`, `suggested_lemma`, `suggested_imports`, `candidate_tactic`, `validation_command`, and `risk`. Wire `scripts/tests/check_proof_repair_plan.sh` with missing theorem, stale proof, missing frame fact, arithmetic bridge failure, and spec mismatch.

 No repair suggestion may change a proof status until `--check` or
 `check-proofs` verifies it. A convenience spelling such as
 `concrete prove <file> <fn> --repair` may exist only as an alias for the
 same repair-plan artifact; it must not edit source by default and must never
 upgrade evidence without replay.
### Task R-0175

**Objective:** **Frame inference (the proof-scaling cliff).** Every loop/state proof must establish not just what an iteration *changes* but what it *preserves* — the frame problem (Smallfoot 2006; later Infer; separation logic's frame rule:

"a proof mentioning only its footprint preserves everything else"). Today
this is handled *cheaply* and *implicitly*: mutation is functional
(`List.set` / `Env.bind`), loop invariants are total index-predicates
(`fun j => if j < m then word j else 0`), and the frame is discharged ONCE as
the generic `set_in_counter_map` lemma and applied O(1)/iteration via
`eval_while_count` — so `block_to_words` / `schedule` / `compress` pay no
per-cell frame cost. This holds only while updates stay single-cell and
arrays stay non-aliasing. It will NOT scale to scattered/multi-cell updates
per iteration, multiple aliasing mutable arrays, invariants that are not
index-predicates, or a future flat mutable-heap / pointer model. Before any
of those land, design a frame-like annotation or inference pass into
ProofCore (separation-logic-style footprints, or a `#[frame]`/`modifies`
clause that auto-derives preservation) so frame conditions never become the
majority of proof work. Gate: do not build it until a second update shape
actually forces it (per the operating rules) — the current functional-list
model gets framing for free.
### Task R-0176

**Objective:** Deferred architecture refactor: split the current `Concrete.Proof` layering so registered example specs can move without a cycle, but do not let this block the active frontier unless spec ownership or proof authoring starts depending on it. Target shape: `Concrete.ProofCore` owns `PExpr`, `PVal`, evaluation, `FnTable`, and source-independent semantics; `Concrete.SpecRegistry` owns the spec-drift table and imports whichever example spec modules it registers;

`Concrete.Proof` becomes the generic proof-theorem / compatibility umbrella.
Only after this split should registered example SPEC PExprs move from
`Concrete.Proof.*Expr` into `Concrete.Examples.<Ex>.Proofs` or sibling
`Specs` modules. Preserve the spec-drift tie throughout. Add
`scripts/tests/check_proof_layering_split.sh`; the gate must prove example
proof theorems and example spec PExprs live under the example namespace,
`Concrete.SpecRegistry` still drives spec-drift, `Concrete.ProofCore` has no
example-owned theorem/spec definitions, `check-proofs` can reach moved
modules through the umbrella import, and changing a moved example body still
reports stale/spec-drift rather than silently accepting the old proof.
### Task R-0177

**Objective:** Add the Phase 9 validation artifact: a proof-authoring project that exercises `--json`, `--show-obligation`, `--emit-lean`, `--emit-artifacts`, `--workspace`, `--check`, `--nearest-lemmas`, `--minimize`, and source-linked proof attachment across straight-line, array update, loop copy, fold, composition, bounded quantified specs, ghost, stale, missing, partial, and repair cases. The gate must typecheck generated stubs, reject any `proof-registry.json`, and verify that failing Lean proofs map back to stable obligation ids. If Phase 8.5 completed, it must also exercise proof-query memory/disk hits, stale dependency invalidation, proof-tool/policy drift,

corrupt-entry recovery, and cache-off/on/verify equivalence without treating
a hit as proof evidence. If Phase 8.5 was rejected/deferred, the validation
artifact runs eagerly and does not create a private proof cache.

## Phase 10: Audit Commands And Review Artifacts

Goal: let a reviewer answer "what can this program do, what is proved, what is
assumed, and what changed?" without reading compiler internals.

Done when: `concrete audit`, semantic diff, and an artifact viewer cover the
five graduated flagships and one package-scale example.

### Task R-0178

**Objective:** Stabilize machine-readable fact schemas for proof status, obligations, effects, capabilities, assumptions, policies, snapshots, showcase metadata, runtime traps, synthesis attempts, stdlib evidence, and package evidence.

Keep one shared evidence-class enum and one shared fact vocabulary across
reports; no report kind may invent private status strings for `proved`,
`reported`, `trusted`, `assumed`, `runtime_checked`, `tested_by_oracle`,
`observed_only`, or `stale`. This is the phase where the global **no second
truth source** rule becomes mechanical: README/site docs, LSP, MCP, agent
JSON, release bundles, artifact viewers, and package reports must wrap these
compiler facts or generated snapshots, not restate evidence status by hand.
Add a drift fixture proving a stale hand-written claim is caught or marked
explicitly non-authoritative.
Keep pipeline validation, independent-certificate, and kernel-replay status
in separate shared dimensions from that evidence-class enum. In particular,
`compiler_validated`, `certificate_structurally_checked`, a named checked
relation, and `kernel_replayed` answer different questions and may not be
collapsed into or used to upgrade a proof/evidence class.
### Task R-0179

**Objective:** Add `concrete audit`: one human-readable plus machine-readable bundle covering authority, trust, allocation, proof status, obligations, assumptions, policy, snapshots, backend/target assumptions, replay, and the proof-story matrix specialized to the audited program.

### Task R-0180

**Objective:** Add `concrete explain <function>`: capabilities, proof status, assumptions, obligations, trusted callees, evidence level, and why each status is what it is.

### Task R-0181

**Objective:** Add `concrete why <capability>`: explain why a function needs `File`, `Network`, `Alloc`, `Unsafe`, etc., including transitive call chains.

### Task R-0182

**Objective:** Add `concrete diff old new`: authority/proof/trust/runtime-obligation diff.

This is also the first **proof/capability diff for code review** surface.

Output must include human text and JSON rows for: added/removed capability,
capability widening/narrowing, new `trusted`/`Unsafe`/extern boundary,
stale/missing/downgraded proof, new runtime trap site, allocation authority
change, changed assumption, and changed stdlib evidence class.

Done when the same JSON drives CI, a GitHub-comment/golden fixture, editor
display, and agent consumption. This is Concrete's review differentiator:
evidence changes live where code review happens, not in a separate proof
report nobody opens. A PR artifact should look like a compact evidence
changelog: `+ File capability`, `+ trusted function`, `- Unsafe removed`,
`verify_packet proof became stale`, `bounds obligation discharged`,
`dependency evidence downgraded`. It must preserve separate evidence classes
and must not collapse the review into one green badge.
### Task R-0183

**Objective:** Add semantic trust diff gates: capability widening, allocation change, trusted boundary addition, stale proof, weakened/missing obligation, assumption widening, runtime-obligation change, and stdlib evidence-class drift. Add a red-team fixture proving the diff cannot emit a false-clean summary when a capability/trust/proof fact changed.

### Task R-0184

**Objective:** Add early **capability budget files** before full package facts exist.

 Deliverable: a project-local policy file (for example
 `concrete.policy.toml`) with a minimal V1 surface:
 `forbidden_caps = ["Network", "Unsafe"]`, `allowed_caps = ["Alloc"]`, and
 optional `forbid_trusted = true`.

 Done when `concrete check` / `concrete audit` fails a fixture that widens
 beyond the budget, reports the exact function/import that caused the
 widening, and reuses the same fact vocabulary as Phase 18 import fact
 constraints so the policy graduates cleanly to dependency/package
 boundaries.
### Task R-0185

**Objective:** Add `concrete audit --json`: machine-readable audit output for CI, dashboards, editor tooling, and release bundles.

### Task R-0186

**Objective:** Add a **verified profile** command/policy surface that makes "formally verifiable code" operational without overclaiming. Candidate spellings:

 `concrete check --profile verified` and/or
 `concrete audit --profile verified`. The profile rejects or fails the
 policy when a selected target contains stale proofs, unresolved proof
 obligations, unapproved `assumed` facts, unapproved `solver_trusted` facts,
 unreviewed `trusted`/extern/`Unsafe` boundaries, unchecked runtime-safety
 obligations, open known holes, or missing replay commands. It must preserve
 evidence classes rather than upgrading them: `tested_by_oracle` remains
 testing, `solver_trusted` remains trusted unless separately replayed in
 Lean, and runtime checks remain runtime checks. Add a gate with one clean
 verified fixture and negatives for stale proof, assumption, SMT-only claim,
 unchecked unsafe, and unresolved runtime obligation.
### Task R-0187

**Objective:** Add an artifact viewer CLI/TUI over facts, obligations, proofs, assumptions, release bundles, and diffs. It may show a compact dashboard, but it must never collapse different evidence classes into one fake green badge: `proved`, `runtime_checked`, `tested_by_oracle`, `assumed`, and `trusted` stay distinct on screen and in JSON.

### Task R-0188

**Objective:** Ensure every release bundle includes an evidence replay command.

Deliverable: `concrete replay <bundle>` rebuilds and rechecks the selected
tests, gates, proofs, doc snippets, report snapshots, and release facts. It
emits a summary of memory-safety enforcement, runtime checks, capabilities,
proofs, trusted boundaries, assumptions, and unsupported facts. It must work
without network access, without an LLM, and without private source paths.

Done when the replay gate covers: clean bundle replays green, stale proof
fails, missing gate fails, changed source fails, schema mismatch fails, and
an LLM-generated proof bundle replays using only checked artifacts.
### Task R-0189

**Objective:** Make `tested_by_oracle` evidence structured and diffable:

 - add an oracle manifest naming reference, seeds, case count, input model,
   comparison mode, and coverage kind;
 - split cases into boundary, known-vector, random, adversarial, and
   regression buckets;
 - report case counts, seeds, reference identity, comparison mode, and
   `not_proof` evidence level in audit output;
 - save failing cases as reproducible fixtures;
 - optionally cross-check reference, interpreter, and compiled binary;
 - support metamorphic tests where no complete reference exists;
 - flag oracle evidence weakening in `concrete diff` when cases, seeds,
   reference, comparison mode, or boundary coverage shrink.
### Task R-0190

**Objective:** Add property-based contract testing as a cheap counterexample finder, not proof. Command surface: `concrete test --contracts --property --json` generates inputs satisfying `#[requires]`, executes the function, checks `#[ensures]`, runtime obligations, and selected `assert` facts, then shrinks failures to a minimal source-level witness. Evidence class:

 `tested_by_property`, always below proof and below solver evidence. Required
 report fields: function, contract id, seed, generator profile, case count,
 shrunk witness, failing postcondition/obligation id, replay command, and
 whether the witness was persisted as a regression. Add
 `examples/property_contracts/` with `clamp`, `bounded_index`,
 `checksum_range`, one precondition-filtered generator, and one deliberately
 false postcondition. Wire `scripts/tests/check_property_contracts.sh`; the
 gate must prove property testing finds and shrinks the false claim without
 ever producing a `proved_*` status.
### Task R-0191

**Objective:** Add counterexample-to-regression persistence for obligation witnesses from SMT, property tests, oracle failures, fuzzers, differential mismatches, runtime traps, proof failures, and future fuzzed contracts. Command surface: `concrete counterexample save <obligation_id> --out tests/counterexamples/<name>.con` plus JSON mode. The saved fixture must include source inputs, expected failing obligation id, expected status (`counterexample`, `tested_by_property_failure`, `oracle_failure`, etc.), replay command, and the original tool provenance. Wire `scripts/tests/check_counterexample_regressions.sh` with one SMT overflow

 witness, one property-test contract witness, one oracle mismatch, one
 fuzzer/differential mismatch, one runtime-trap witness, and one proof
 failure/minimized obligation witness. The
 gate must fail if a future refactor turns the same counterexample into
 `proved_*` without changing the checked fixture expectation.
### Task R-0192

**Objective:** Add **observed contract inference from tests** as an explicit non-proof on-ramp to specs.


  Deliverable: `concrete infer-contracts <target> --from-tests --json`
  proposes candidate preconditions, postconditions, or invariants from
  passing tests/property cases. Every output fact is marked
  `observed_only` / `suggested_contract`, never `proved`.

  Reports must show sample size, generator profile, counterexamples tried,
  surviving mutants if any, and the command needed to promote the suggestion
  into a checked contract/proof obligation. Gates: infer a useful range
  postcondition from tests; reject or mark weak a vacuous suggestion; prove a
  mutated implementation can invalidate the observed claim; and ensure no
  inferred contract upgrades evidence without a separate proof or
  policy-approved assumption.
### Task R-0193

**Objective:** Add spec provenance and adequacy facts to audit/release bundles: spec name, source standard or paper, independent reference if any, test-vector set, reviewer, review date, assumptions, and evidence class (`spec_trusted`, `spec_reviewed`, `tested_by_oracle`, or future `spec_refines_standard`). Do not let a source-to-spec proof imply the spec itself is adequate.

### Task R-0194

**Objective:** Add evidence-level monotonicity checks to audit/diff output.

### Task R-0195

**Objective:** Add one AI-audit demo where an agent answers authority/proof/trust questions using compiler facts rather than source guesses.

### Task R-0196

**Objective:** Add review checklists generated from facts: what changed, what widened, what became trusted, what lost proof, what gained assumptions, and which obligations remain open.

### Task R-0197

**Objective:** Add artifact redaction/stability rules so release bundles can be shared publicly without leaking local paths, secrets, or machine-specific noise.

### Task R-0198

**Objective:** Keep audit, contracts, obligations, assumptions, policies, manifests, and proof-status output on one shared vocabulary. Do not let each artifact grow its own mini-language for the same evidence classes.

### Task R-0199

**Objective:** Keep public-facing docs and website copy grounded in the same evidence vocabulary. Use `docs/WHY_CONCRETE.md` as the source for a C/Rust-oriented "why this exists" page: small systems code, explicit authority, visible evidence classes, spec-drift-tied proofs, named trust boundaries, and what Concrete deliberately avoids. The website should show the end goal and the current honest status, not catchy slogans or one-badge proof claims.

### Task R-0200

**Objective:** [historical origin: closed Phase 4 item 42] Add compiler self-audit: `concrete audit

 --compiler` renders the `CompilerLedger` / `ObligationCore` as a reviewable
 bundle (ledger-from-ledger), proving the compiler's own facts are
 audit-visible through the same surface user programs use. When Phase 8.5
 completed, it must consume its typed query/fact results and show cache/query
 provenance separately from evidence. Otherwise it consumes the eager shared
 pipeline facts. Neither branch may recompute a private audit-only compiler
 model, create a private cache, or present a cache hit as validation.
### Task R-0201

**Objective:** Add the Phase 10 validation artifact: one package-scale audit bundle fixture with human and JSON output, semantic diff before/after a change, artifact viewer smoke test, oracle manifest, property-test manifest, persisted counterexample regression, spec-provenance facts, redaction check, replay command, and a README showing how a reviewer answers authority, proof, trust, assumption, and runtime-obligation questions without reading compiler internals. When Phase 8.5 exists, cold, warm, and cache-off audit/diff/why output must be identical after normalizing timing/cache-observability fields, and corrupt/stale query artifacts must recompute or fail closed; otherwise

 the same fixture runs eagerly with no private caching.

## Phase 11: Proof Status And Trust Gates

Goal: make every green proof/evidence status precise, traceable, and hard to
misread.

Done when: all existing production proof specs are directly and transitively
FnTable-complete, proof dependencies and provenance are visible, assumptions
and trust boundaries have lifecycle reports, and weaker evidence cannot appear
under a stronger badge.

### Task R-0202

**Objective:** Add transitive FnTable completeness: walk registered spec call graphs, not only direct call sites, and fail or flag missing callees before theorem authors hit confusing `none` evaluations.

### Task R-0203

**Objective:** Add proof dependency tracking: if proof/spec for `f` depends on `g`, drift in `g` must affect `f`'s proof/evidence status or surface an explicit dependency warning.

### Task R-0204

**Objective:** Add per-obligation proof/evidence status. Function-level status is only a summary; padding, block fold, digest serialization, final composition, contract clauses, runtime obligations, oracle checks, and assumptions each carry their own evidence class.

### Task R-0205

**Objective:** Keep oracle-tested evidence separate from Lean/spec refinement. Oracles are implementation sanity and regression evidence, not proof completion.

### Task R-0206

**Objective:** Add proof debugging output for failed/stale proofs: extracted spec, current fingerprint, registered fingerprint, expected theorem shape, missing callee facts, likely missing lemma class.

### Task R-0207

**Objective:** Add evidence provenance to proof/evidence facts: source file/span, compiler commit, theorem name, spec name, policy file, assumption file, tool version, and replay command where available.

### Task R-0208

**Objective:** Add tool-version drift checks: proof/evidence facts record the Lean version, Concrete compiler commit, ProofKit hash, extraction version, decision procedure version, and solver version where relevant. A toolchain bump marks affected evidence `needs_recheck` until replayed; old green badges are never silently reused across a proof-tool upgrade.

### Task R-0209

**Objective:** Add evidence monotonicity checks: a refactor cannot silently present a weaker claim as if it were still stronger (`proved` cannot degrade to `reported` while retaining the same badge/summary).

### Task R-0210

**Objective:** Add assumption lifecycle checks: every assumption has an owner, scope, rationale, review date, affected claims, and a diff gate when it widens.

### Task R-0211

**Objective:** Add a trust-boundary inventory report: all `trusted`, `Unsafe`, extern, backend, runtime, and target assumptions in one machine-readable list.


V1 deliverable: `concrete audit --trust-boundaries --json` shows the trusted
surface, the facts each boundary supports, what calls it reaches, and which
claims depend on it. This is an honest report, not an auto-refactorer.

A later "trusted-boundary shrinker" may suggest wrappers or proof targets,
but suggestions must be advisory until a replayed proof/audit diff validates
the smaller boundary.
### Task R-0212

**Objective:** Add spec-adequacy gates: release policy can require reviewed spec provenance for selected claims, forbid unreviewed specs in graduated flagships, and show when a theorem is `proved_by_lean` against a `spec_trusted` or unreviewed spec.

### Task R-0213

**Objective:** Add vacuity gates to proof status: `proved` summaries must be downgraded or blocked when the proof depends on an unsatisfiable precondition, contradictory assumptions, unreachable code path, or invariant `false`.

### Task R-0214

**Objective:** Add solver portfolio and cross-solver agreement as a strictly separate evidence class, never as kernel evidence. External SMT V1 may start with Z3 only, but the trust-gate roadmap must define how to run `z3`, `cvc5`, and `bitwuzla` where the fragment applies. Result classes:

 `solver_trusted` for one trusted solver, `solver_cross_checked` when two or
 more independent solvers agree on the same `unsat` result for the same
 normalized query, `solver_disagreement` when they differ, and
 `solver_unavailable` / `solver_timeout` / `solver_unknown` for non-proofs.
 Agreement must record solver names, versions, logic, query hash, timeout,
 and replay commands. A cross-checked solver result is stronger than a
 single solver but still below `proved_by_lean` and
 `proved_by_kernel_decision`. Add `examples/solver_portfolio/` with one
 QF_NIA query, one bitvector query, one unsupported-fragment case, one fake
 disagreement wrapper, and one missing-solver case. Wire
 `scripts/tests/check_solver_portfolio.sh`; the gate must prove no external
 solver result can overwrite kernel evidence and that disagreement blocks
 release claims unless explicitly assumed.
### Task R-0215

**Objective:** Add spec/proof mutation testing to prove evidence is load-bearing. Command surface: `concrete mutate-evidence --target <example> --json` creates controlled mutants: change a function body under a proof link, weaken or delete an `#[ensures]` clause, strengthen an impossible `#[requires]`, remove a loop invariant, alter a spec PExpr/table entry, change a theorem name, and perturb a trusted assumption. Expected outcomes must be explicit:

 stale, missing, vacuous, partial, failed proof, widened trust, or unchanged
 only when the mutation is semantically irrelevant and justified. Wire
 `scripts/tests/check_evidence_mutation.sh` over `hmac_sha256`,
 `constant_time_tag`, `proof_patterns`, and one contract-negative example.
 The gate must fail if a mutated proof/spec still reports the original green
 evidence class without an allowed explanation. This is evidence about the
 evidence: proofs must constrain the implementation, not merely decorate it.
### Task R-0216

**Objective:** Add proof-corpus migration across toolchain upgrades, the active half of the §7 drift story. Detection marks evidence `needs_recheck`; migration must turn a bumped corpus green again without hand-walking every obligation. Command surface: `concrete prove --recheck-corpus [--json]` re-runs every linked proof/evidence check under the current toolchain and triages each into `still_proved`, `replayed_clean`, `broke_needs_repair`, or `unavailable_dependency`; `concrete prove --recheck <obligation_id>` does one.

 Pin the external Lean proof-library surface the corpus depends on (Lean
 stdlib / Batteries / Mathlib lemmas and tactics actually cited) in a checked
 `proofs/lean-deps.lock` with versions, so a renamed or relocated upstream
 lemma is reported as `unavailable_dependency`, never a silent break. Wire
 `scripts/tests/check_proof_corpus_migration.sh`: it must prove the flagship
 corpus (`hmac_sha256`, `constant_time_tag`) re-greens under a simulated
 toolchain bump, that a removed/renamed pinned lemma surfaces as
 `unavailable_dependency`, and that no `needs_recheck` obligation can reach a
 green badge without an actual kernel re-check.
### Task R-0217

**Objective:** Add an axiom-inventory and clean-checkout proof replay gate. Every `proved_by_lean` / `proved_by_kernel_decision` fact must record the Lean axioms its theorem transitively depends on (the `#print axioms` walk):

 `propext`, `Classical.choice`, and `Quot.sound` are the expected baseline;
 any `sorryAx`, `ofReduceBool`/`native_decide`, or user-declared axiom must
 downgrade the evidence class or surface as an explicit trusted-boundary
 fact in the §10 inventory — never sit silently under a kernel-evidence
 badge. Pair it with clean-checkout replay: the proof corpus must re-green
 from a fresh checkout in a clean temp directory, proving no green badge
 depends on stale `.lake` build artifacts, uncommitted files, or local
 machine state. Wire `scripts/tests/check_axiom_inventory.sh` (red-team
 case: a spec theorem patched to use `sorry` or a new `axiom` must lose its
 kernel-evidence class) and
 `scripts/tests/check_clean_checkout_replay.sh` (the flagship corpus —
 `hmac_sha256`, `constant_time_tag` — must replay green from a pristine
 copy, and a deliberately corrupted source-vs-artifact mismatch must fail
 loudly, not reuse the stale artifact). Seed Phase 8.5's local cache with a
 stale proof result, wrong dependency root, truncated entry, and valid entry
 from another compiler/toolchain; clean-checkout replay must reject/recompute
 all four before any evidence becomes green.
### Task R-0218

**Objective:** [historical origin: closed Phase 3 items 15/16] Convert `--report proof-status` and `concrete prove`'s obligation facts from consistency-gated recompute to literal `ObligationCore`-ledger views — the last Phase 3 surfaces that recompute (sound today because consistency-gated). They are the proof-status / trust surfaces, so they belong in this phase.

### Task R-0219

**Objective:** Add the Phase 11 validation artifact: a trust-gate pressure project that includes transitive proof dependencies, stale dependency propagation, tool-version drift, proof-corpus migration across a simulated toolchain bump, assumption widening, spec-adequacy policy, vacuity downgrade, solver portfolio / disagreement handling, evidence mutation testing, axiom inventory and clean-checkout replay, weaker-evidence monotonicity, and a release gate proving each status cannot be silently presented as stronger evidence. Include cache-off/on/verify parity and stale/corrupt-cache negatives so the trust gates cover the operational path

 users actually run.

## Phase 12: Provable And Predictable Subsets

Goal: give users a named small subset they can rely on for serious
proof/evidence work.

Done when: the subset family has public names, allowed constructs, rejected
constructs, arithmetic-site policy, runtime-error policy, and compatibility
promises.

### Task R-0220

**Objective:** Define `PredictableV1`: no allocation unless bounded, no FFI unless trusted and assumed, no unbounded loops/recursion, explicit failure-path policy.

### Task R-0221

**Objective:** Freeze arithmetic-site semantics for subset claims. This task is reconciled with historical Phase 6 item 10 and `docs/ARITHMETIC_POLICY.md`: build/profile names are policy bundles, not arithmetic modes, and the same source expression must not mean wrap in one profile and checked in another. Ordinary `+ - *` are checked in every profile; intentional modular arithmetic is written as `wrapping_*`; intentional clamping is written as `saturating_*`. Subset reports must classify arithmetic sites as `checked`, `proved`, `runtime- checked`, `explicit-wrapping`, or `explicit-saturating`; they must never infer an ambient arithmetic mode from `debug`, `release`, `predictable`, or

`proof`.
### Task R-0222

**Objective:** Carry arithmetic-site facts into diagnostics, reports, assumptions, proof obligations, release bundles, and backend contracts. A theorem about checked arithmetic is not a theorem about modular arithmetic; a function using `wrapping_*` is either proved against the proof model's explicit modular operator (today only `wrapping_add@u32 → addw 32`, as SHA-256 uses) or, for the not-yet-modeled forms (`wrapping_sub`/`wrapping_mul`, other widths, saturating), outside the default proof subset. Add `docs/ARITHMETIC_SITE_EVIDENCE.md`, `examples/arithmetic_site_evidence/{checked,wrapping,saturating,profile_invariant}/`,

and `scripts/tests/check_arithmetic_site_evidence.sh`; the gate must prove
profile-invariance (the same expression has the same semantics under
different build profiles), explicit classification of every arithmetic site,
and rejection/downgrade of proof claims that confuse checked and wrapping
semantics.
### Task R-0223

**Objective:** Define a first runtime failure model: abort, assertion failure, OOM, stack overflow, `defer`/cleanup, impossible branches, and what each does to proof/resource claims.

### Task R-0224

**Objective:** Define source-level stack-depth versus backend/target stack claims.

### Task R-0225

**Objective:** Define source-level constant-time profile v1:

no secret-dependent branch, no secret-dependent memory index, fixed loop
bounds, explicit backend timing assumptions.
### Task R-0226

**Objective:** Define secret/data-sensitivity labels for future security work:

`public`, `secret`, `timing-sensitive`.
### Task R-0227

**Objective:** Define source-level memory-safety claims precisely: what linearity, borrows, cleanup, trusted code, raw pointers, and FFI do and do not guarantee.

### Task R-0228

**Objective:** Define the **Unsafe island** rule for unchecked operations. Any operation that bypasses a safe runtime check, borrow/linearity rule, raw-pointer restriction, layout proof, or FFI ownership boundary must be exposed through a deliberately small surface requiring `with(Unsafe)` or an explicitly named `trusted` boundary. Examples include future `get_unchecked`, raw pointer place writes, unchecked casts/conversions, inline asm, and unchecked FFI wrappers. There is no global unsafe mode and no silent trusted wrapper that hides authority: audit output must show the safe wrapper, the underlying trusted/Unsafe operation, and the assumption being

 accepted.

 Deliverable: `docs/UNSAFE_ISLAND.md` plus fixtures for `get_unchecked`-style
 access, raw-pointer place writes, unchecked casts, inline asm placeholder,
 and FFI wrapper assumptions. Add a red-team gate proving safe code cannot
 reach the unchecked operation without the capability or trusted boundary,
 and proving audit output shows both the safe wrapper and the underlying
 trusted/Unsafe operation.
### Task R-0229

**Objective:** Decide the proof class for references and borrows. A function using `&` or `&mut` must be classified as one of: value-only/borrow-free, proved over read-only references, proved over mutable references with explicit frame/modifies obligations, or enforced-only and outside `ProvableV1`. Do not let borrow-using code appear proof-eligible through a value-only ProofCore model.

### Task R-0230

**Objective:** Define the v1 threat model: adversary, trusted base, proof scope, backend scope, side-channel scope, dependency scope, and what remains out of model.

### Task R-0231

**Objective:** Add negative examples for every `ProvableV1` and `PredictableV1` exclusion.

### Task R-0232

**Objective:** Update `CLAIMS_TODAY.md`, README, showcase docs, and release bundles to use the frozen subset names consistently.

### Task R-0233

**Objective:** Close the unprofiled-float proof hole before any float proof claim:

 float-typed params/returns/locals/literals/ops are excluded from ProofCore
 extraction unless an explicit float profile is active. Audit output must
 say `float semantics: unprofiled` and `proof eligibility: excluded` rather
 than reporting a float operation as extracted through integer `PBinOp.add`.
### Task R-0234

**Objective:** Define `ProvableFloatV1` as a separate, narrow proof profile:

 IEEE-754 binary32/binary64, round-to-nearest-even, no fast-math, no
 reassociation, no implicit FMA contraction, no ambient rounding-mode
 mutation, and explicit NaN/infinity/subnormal/signed-zero policy.
### Task R-0235

**Objective:** Add ProofCore support for profiled floats only after Task R-0233 is closed:

 `PVal.float32/64`, float `PBinOp` cases carrying width and rounding
 (`fadd`/`fsub`/`fmul`/`fdiv`/`feq`/`flt`/`fle`), interpreter agreement,
 and backend/audit checks that prove/report `fast_math: forbidden`.
### Task R-0236

**Objective:** Classify the first float semantics layer honestly. Until Concrete imports or proves a checked IEEE-754 semantics library, primitive float operations are `float_semantics_trusted`; proofs over profiled float code are refinements to an explicit bit-level IEEE spec under that named trusted primitive layer, not `proved_by_lean` from first principles.

### Task R-0237

**Objective:** Add one small `ProvableFloatV1` flagship only after the profile exists:

 a fixed-order `f32`/`f64` kernel such as clamp/normalize, tiny FIR/IIR, PID,
 or dot product. Prove exact IEEE behavior first; real-valued epsilon-bound
 refinement is a later layer.
### Task R-0238

**Objective:** Add the Phase 12 validation artifact: a profile matrix project covering `PredictableV1`, `ProvableV1`, unprofiled-float exclusion, profiled-float admission, borrow/reference proof-class decisions, constant-time source shape, stack/runtime-failure assumptions, and negative examples for every exclusion. The gate must prove reports never call excluded code proof eligible.

### Task R-0239

**Objective:** Define the SPARK-class contract layer in Concrete's vocabulary, not Ada's:

 frame/read/write contracts, dependency-flow contracts, ghost/spec purity,
 and proof classes for each fact. Candidate surface may include
 `#[reads(...)]`, `#[writes(...)]`, `#[modifies(...)]`, and
 `#[depends(out <- in1, in2)]`, but syntax is not frozen until a flagship or
 external workload pulls it. The design must explain how these facts differ
 from capabilities: capabilities name external authority; frame/dependency
 contracts name memory/state/data influence. Add
 `docs/SPARK_CLASS_ASSURANCE.md` updates, examples with one parser/buffer
 loop and one policy/data-flow function, and a gate proving facts are
 reported as proved/enforced/reported/assumed/trusted rather than prose-only.
### Task R-0240

**Objective:** Define a development-only expectation policy, borrowing Roc's useful lightweight `expect` idea but fitting Concrete's evidence model. The goal is a clear place for "this should hold during tests/dev" that cannot be mistaken for proof, runtime-safety evidence, or a release assumption.

 Candidate surface may be an attribute, a test helper, or a restricted
 `expect` form, but the semantics must be decided before syntax: it is
 allowed in tests/examples and development profiles; it produces
 `tested`/`dev_checked` evidence only; it is excluded from `ProvableV1`
 unless translated into an explicit contract/obligation; and release bundles
 must either omit it, downgrade it, or list it as non-release evidence. Add
 `docs/EXPECTATION_POLICY.md`,
 `examples/expectation_policy/{dev_expect,test_expect,release_reject,contract_replacement}/`,
 and `scripts/tests/check_expectation_policy.sh`; the gate must prove that a
 passing expectation is never reported as `proved` and that a release/high-
 integrity profile cannot silently rely on it.

## Phase 13: Runtime Safety Obligations

Goal: generate SPARK-like obligations for boring runtime failures instead of
relying only on examples and prose.

Done when: parser/security examples can show obligations for bounds, div/mod
zero, overflow profile, casts, and loop bounds with statuses
`proved`, `enforced`, `assumed`, `missing`, or `blocked`.

This phase is the **absence of runtime errors** story, stated honestly. A bounds
check, overflow trap, cast check, or loop-bound assertion may be enforced at
runtime, proved statically, assumed by policy, or still missing; those are
different evidence classes and reports must not collapse them. The SPARK/Ada
lesson to keep is the framing: runtime safety is not just "the program probably
doesn't crash"; it is a set of named obligations with source spans, replay
commands, and visible proof/enforcement status.

### Task R-0241

**Objective:** Define stable obligation schema v1: id, kind, source span, function, expression, dependencies, evidence status, discharging theorem/check/ assumption, and replay command.

### Task R-0242

**Objective:** Define the user-level error model: `Result`, `Option`, assertion failure, abort/panic, recoverable errors, test failures, and how error flow interacts with capabilities, proofs, runtime obligations, and audit output.

### Task R-0243

**Objective:** Generate narrowing/invalid-cast obligations.

### Task R-0244

**Objective:** Generate loop bound and variant obligations for bounded loops.

### Task R-0245

**Objective:** Define policy gates for `#[overflow_checked]`: release profiles may require overflow obligations for selected functions/packages, while ordinary examples remain quiet unless they opt in. Reports must distinguish `overflow_checked`, `overflow checking not requested`, and explicit wrapping or saturating arithmetic.

### Task R-0246

**Objective:** Generate obligations for panic/abort/assert-as-denial-of-service risks:

unchecked indexing, unwrap-like operations, explicit abort paths, failed
assertions, and profile-dependent panic behavior.
### Task R-0247

**Objective:** Generate byte/text/path boundary obligations: invalid UTF-8, lossy conversion, OS-string conversion, path normalization assumptions, and rejected implicit conversions.

- 7a. Implement explicit enum discriminant values for FFI/protocol enums
  (`enum Op { Get = 0x01, Set = 0x02 }`). These are currently REJECTED at
  parse time (E0001, "explicit enum discriminant values are not supported
  yet") because the parser previously parsed and silently DISCARDED them,
  assigning positional tags regardless — a semantically dark construct that
  would corrupt any FFI/serialization enum. Implementing means: honor the
  written value in the variant's tag/repr, reject duplicate discriminant
  values (a collision is a bug, not a silent alias — Rust rejects it),
  surface the chosen tag in the layout/ABI report, and tie it to the
  `repr(C)` boundary so a C consumer and a round-trip decoder agree. Until
  then the rejection stands (regression-locked by
  `tests/programs/error_enum_explicit_discriminant.con`). Add
  `examples/enum_discriminants/{protocol_ops,duplicate_rejected}/` and a
  gate proving values are honored at the repr boundary and duplicates are
  rejected.
### Task R-0248

**Objective:** Generate stack/recursion obligations where the profile claims boundedness.

### Task R-0249

**Objective:** Report runtime-error obligations in human and JSON forms.

### Task R-0250

**Objective:** Add policy gates that can require selected runtime-error obligations to be proved/enforced before graduation.

### Task R-0251

**Objective:** Add a runtime-error regression corpus: invalid cast, loop-bound violation, lossy byte/text conversion, ignored fallible result, unwrap-like failure, panic/abort profile mismatch, and release-policy rejection for missing `#[overflow_checked]` evidence where required.

### Task R-0252

**Objective:** Add a runtime-error-obligation flagship requirement: one graduated example must demonstrate no OOB/div-zero/overflow under a named profile.

### Task R-0253

**Objective:** Add high-quality diagnostics for obligation failures: violated obligation, source expression, required evidence, current status, and next action.

### Task R-0254

**Objective:** Add obligation suppression only through explicit assumptions or policy waivers, never comments or hidden allowlists.

### Task R-0255

**Objective:** Prove or validate obligation-generation soundness for the first obligation kinds through the compiler soundness bridge.

### Task R-0256

**Objective:** Add automatic invariant inference / abstract interpretation as an annotation-reduction pass, not as trusted proof. V1 analysis is finite and auditable: interval facts, simple relational facts (`i <= n`, `i < len`, `0 <= i`), monotone loop counters, constant loop bounds, simple affine equalities/inequalities, and fixed-array length facts. It may synthesize candidate loop invariants and scoped facts for bounds, div/mod-zero, overflow, cast, and loop-variant obligations. Every inferred fact must be emitted in the obligation ledger with source span, analysis name, abstract domain, dependencies, and a replay command, then independently discharged

 by `omega`, `bv_decide`, or Lean before receiving
 `proved_by_kernel_decision` / `proved_by_lean`. Unchecked inference is not
 evidence. Add status detail `inferred_candidate` for facts proposed by the
 analysis but not yet checked. Add `examples/inferred_invariants/` with
 `array_sum_no_oob`, `copy_loop_bounds`, `ring_index_mod`, `overflow_counter`,
 and negative cases for non-affine updates, alias-sensitive updates, and
 widened bounds. Wire `scripts/tests/check_invariant_inference.sh`; the gate
 must prove inferred facts reduce required user annotations without creating
 false green obligations.
### Task R-0257

**Objective:** Connect runtime-safety obligations to loop, frame, and dependency facts.

 Bounds/cast/overflow obligations should be dischargeable from explicit loop
 invariants, generated invariant candidates, and future `reads`/`writes`/
 `modifies` facts without duplicating the ledger. The report must show which
 invariant or frame fact discharged each obligation, and missing facts must
 produce actionable diagnostics rather than generic "unproven" output.
### Task R-0258

**Objective:** Add newtype/type invariants as scoped obligation hypotheses after they are checked at construction boundaries. This connects validated wrappers to the proof/runtime-safety pipeline: a type such as `#[invariant(self.0 > 0 && self.0 < 65536)] newtype Port = u16` must have the invariant proved/enforced at every constructor or `try_new` admission point, then injected as a named hypothesis for obligations in any function receiving a `Port`. No invariant may enter scope from a raw cast, trusted constructor, FFI boundary, stale proof, or unchecked assumption without a ledger entry saying so. Add `docs/NEWTYPE_INVARIANT_OBLIGATIONS.md`,

 `examples/newtype_invariants/{port,nonzero_len,bounded_index,ffi_trusted}/`,
 and `scripts/tests/check_newtype_invariant_obligations.sh`; the gate must
 prove constructor checks create reusable hypotheses, invalid constructors
 are rejected or return `Result`, raw/trusted paths do not silently inject
 facts, and obligation reports name the type invariant source.
### Task R-0259

**Objective:** Add the Phase 13 validation artifact: a runtime-safety corpus covering bounds, div/mod-zero, overflow, casts, panic/abort/assert, byte/text/path boundaries, stack/recursion, inferred invariant candidates, newtype invariant hypotheses, arithmetic-site evidence mismatches, and obligation suppression. Each case must show one of `proved`, `enforced`, `assumed`, `missing`, or `blocked`, include a negative variant, and run through policy gates plus human/JSON reports.


## Phase 14: Compiler Soundness Bridge

Goal: move the flagship-used `Core -> ProofCore` rules from "extracts to the
expected ProofCore shape" toward source-semantics agreement and checked
trust-gate correctness.

Prior-art peer group for this phase: CompCert for proved compiler passes,
CakeML for verified bootstrap discipline, Cogent for linear systems code with
refinement proof artifacts, F*/Low* for obligation-to-SMT-to-extraction
workflow, and Alive2 for SMT-checked LLVM IR equivalence/optimization
validation. Keep these as reference points for the shape of evidence, not as
marketing claims that Concrete already matches them.

Done when: each flagship-used ProofCore construct is classified as
shape-preserved, eval/source-semantics-preserved, or still trusted; proof-report
facts agree with compiler state; a small independent checker re-derives the
named `CoreCertificateV1` predicate from canonical artifacts rather than
trusting producer summaries; and the remaining trusted base is machine-readable.

**Endgame (north star): the compiler becomes a mechanically-verified artifact,
out of the trusted base.** The per-rule discharge below (R-01..R-21), the
`CoreCertificateV1` independent checker, and Task R-0299 translation validation
are not the finish line — they are the incremental ladder toward proving each
pass semantics-preserving in Lean against the interpreter reference, so the
compiler leaves the TCB entirely. Concrete is uniquely positioned for this: it
is Lean-hosted, so the compiler's correctness proof and users' program proofs
share ONE kernel — collapsing "trust the prover" and "trust the compiler" into a
single trusted base no other verified-compiler effort (CompCert, CakeML) has.
The 6B fact-centralized IR and interpreter-as-oracle are the runway: each pass
is a clean function over committed facts with a reference semantics to prove
against, so passes should be built PROVABLE-FIRST, not merely consistent. This
names the endpoint so intermediate design choices bend toward it; it does not
schedule a multi-year proof now, and it must not starve the near-term or the
external trial. The complementary rewrite-passes-in-Concrete route is Task R-0413; the shorter path is proving the existing Lean-hosted passes here.

### Task R-0260

**Objective:** Add a compiler-soundness rule-status dashboard over R-01..R-21:

`shape-preserved`, `eval-preserved`, `source-preserved`, `trusted`, or
`blocked`, with theorem names and source links for each status.
### Task R-0261

**Objective:** Build source semantics for the provable subset as needed by rule discharge, not as a speculative full-language semantics.

### Task R-0262

**Objective:** Upgrade extraction-only rules to three-view preservation where practical:

source Core evaluation, extracted PExpr evaluation, and extraction theorem
agree.
### Task R-0263

**Objective:** Prioritize the hard eval/source rules in dependency order: direct calls, structs/fields, enums/match, arrays, casts, arraySet, flat bounded `while`, and `while_step`.

### Task R-0264

**Objective:** Prove selected proof-report facts agree with compiler state: `proved`, `stale`, `blocked`, `missing`, `ineligible`, `trusted`.

### Task R-0265

**Objective:** Prove or mechanically validate trust-gate correctness: body fingerprint determinism, spec-drift completeness, proof attachment lookup, FnTable completeness, eligibility classification.

### Task R-0266

**Objective:** Record a machine-readable trusted computing base for proof/evidence claims:

Lean kernel, compiler modules, backend/toolchain, runtime/OS/hardware,
trusted/extern code, and the external Lean proof-library surface the corpus
cites (Lean stdlib / Batteries / Mathlib lemmas and tactics). Kernel-checked
library lemmas do not widen the trusted base, but they are a pinned
version/availability dependency (see Phase 11 proof-corpus migration); the TCB
record must name them so a proof's replay surface stays reproducible.
### Task R-0267

**Objective:** Prove selected checker/report agreement for authority and purity facts used by proof eligibility, so a function cannot be called proof-eligible while secretly requiring capabilities.

### Task R-0268

**Objective:** Decide whether deeper source-semantics proofs require a normalized Core layer. Add the layer only if the direct rule proofs show repeated semantic duplication across at least two forcing examples.

### Task R-0269

**Objective:** Automate dependency-ordered spec/table generation. Function specs and PExpr bodies should be collected/generated in dependency order together with FnTable completeness and call dependencies, so proof authors do not hand-relocate definitions above tables like `shaFns`.

### Task R-0270

**Objective:** Prove or mechanically validate spec/ghost totality reporting: every contract-referenced `spec fn` or ghost computation is either backed by Lean termination, accepted by a Concrete totality check, or rejected with a `totality_obligation_missing` status. A contract may not depend on a partial or non-terminating spec expression silently.

### Task R-0271

**Objective:** Define proof preservation across monomorphization and capability-polymorphic instantiation. The compiler must report whether a theorem proves a specific generated instance (`proved_for_instance`) or a generic body (`proved_generic`), and it must prevent one instance proof from being presented as proof for every future instantiation.

### Task R-0272

**Objective:** [historical origin: Phase 4 items 44f/44g] Finish the remaining compiler-correctness hardening:

 - extend the differential generator with string, heap/allocation,
   linear-value, and effectful-I/O shapes while preserving the rule that an
   E07xx/panic on a generated well-typed program is a compiler bug;
 - rotate seeds in a nightly CI campaign and automatically minimize
   failures rather than relying only on the fixed presubmit seeds; and
 - repair the dead-path ref-return lowering case where a reference-typed
   return materialized from a ref identifier or `&place` emits a spurious
   extra load. This path is unreachable from source while reference returns
   are rejected, so its regression fixtures must separately preserve the
   accepted trusted raw-pointer (`*const`/`*mut`) return cases.
### Task R-0273

**Objective:** Add a semantic-darkness audit and red-team gate. The goal is to catch the checked-arithmetic class of bug before it repeats: a construct looks ordinary in source, but its real behavior depends on width, profile, target, allocation, authority, runtime checks, or an outdated proof/interpreter model. Add `docs/SEMANTIC_DARKNESS_AUDIT.md` and `scripts/tests/check_semantic_darkness.sh`; wire the gate into CI or the Phase 14 validation artifact. The audit must cover:

 - **source-semantics matrix** for each core operation: parser/AST,
   checker rule, Core/Elab representation, interpreter behavior, SSA
   cleanup/optimization, LLVM lowering, ProofCore extraction, reports, and
   stdlib examples must agree on the same meaning;
 - **primitive boundary tests** over arithmetic, casts, shifts, div/mod,
   literals, references, arrays, byte/text/path boundaries, runtime aborts,
   target assumptions, and trusted/Unsafe escape hatches;
 - **stdlib authority/allocation audit**: every stdlib API that allocates,
   touches host state, uses trusted/Unsafe internals, depends on target/OS
   behavior, or relies on sentinel/wrapping/truncating behavior must expose
   that fact in source or report output;
 - **proof/runtime consistency fixtures**: interpreter-vs-compiled,
   optimized-vs-unoptimized where available, report-vs-runtime, and
   ProofCore-vs-runtime behavior must agree or be explicitly classified as
   trusted/unsupported;
 - **old-assumption grep**: scan code, tests, docs, and snapshots for stale
   terms such as `wrap`, `wrapping`, `overflow`, `truncate`, `expected
   diverge`, `unbounded Int`, `addw`, `mod 2`, `unsafe`, `trusted`,
   `temporary`, and `current lowering`; every hit must be either current,
   linked to a roadmap item, or removed.
 The first version should seed fixtures from the known high-risk families:
 checked arithmetic / explicit wrapping, float→int cast overflow (H2),
 lossy casts, div/mod-zero, shifts, returned-reference rejection, ByteView
 wrong-buffer guards, HMAC/SHA-256 modular arithmetic, and capability-bearing
 stdlib calls.
 Prior art and comparison targets: CompCert/CakeML/Cogent/F*/Low* for
 source-semantics and proof/evidence pipelines, and Alive2 for backend IR
 equivalence checking. See
 `research/compiler/pipeline-lessons-2026-07.md`.
### Task R-0274

**Objective:** **Preserve the one source of typing truth through proofs.** Every 2026-07 front-end bug was two passes holding different opinions about the same program: Check typed literals from the hint while Elab typed them from the sibling operand;

 `typesCompatible` was lenient where SSA-verify was strict (the E0715 class);
 Elab carried a private int-vs-fixed-width re-elaboration Check knew nothing
 about; std skipped Check entirely while Elab/CoreCheck ran (H12). Shared
 predicates (`binOpOperandsAgree`) and LANGUAGE_INVARIANTS rule 19 gates DETECT
 drift; the architecture still INVITES it — three semi-independent semantic
 judgments over one program. Endgame: one shared source type judgment feeds
 both Check's type-dependent checks and Elab's existing typed Core
 (`CExpr.ty`) stamping, deleting Elab's private re-inference and CoreCheck's
 overlapping rules down to genuine Core-shape validation.

 The completed Phase 6B / 6.5 `TypeJudgment` work is where the architecture
 lands, not this phase: Core `CExpr`
 is already the typed IR, and the type-axis work is to centralize the source
 type judgment so Check and Elab stamp/read the same answer. The Phase 8.5
 `CompilerDB` owns relational, provenance, evidence, dependency, and query
 facts pulled by real consumers. This task proves and
 preserves that architecture. The work here is to delete remaining
 overlapping typing judgments, prove passes preserve typed Core meaning, and
 ensure proof extraction / reports / backend validation consume typed Core
 plus relational/evidence facts rather than reconstructing source-level
 judgments.

 Source identity remains useful for DB-owned relational facts, but it is not
 the type carrier and should not be built just to fix source typing. Typed
 Core is the type carrier. For migrated type families, Check and Elab must
 call the shared judgment. For migrated relational/evidence families, DB
 coverage checks apply to the Phase 8.5 `CompilerDB`. Missing DB facts or
 conflicting DB facts are compiler errors. Unmigrated families may still be
 absent only while their migration flag says so.

 The preservation split is explicit: typed Core is primary for node-local
 type facts; the Phase 8.5 `CompilerDB` is primary for relational facts such as E0293
 container-not-in-context, borrow conflicts, call-site/parameter pass
 agreement, capability/proof dependencies, provenance, evidence, and
 invalidation. A proof obligation may reference both, but it must not create a
 third spelling of the same fact. This is the proof-facing form of the Phase
 6.5 placement rule.

 The boundary chain should be unrepresentable-by-construction:
 `ResolvedProgram -> DesugaredProgram -> Check -> Elab -> CoreProgram ->
 CoreCheck -> ValidatedCore -> Mono ->
 ValidatedMonoCore -> Lower -> SSA -> SSAVerify -> ValidatedSSA`. If a
 downstream stage wants a source type, local value-flow mode, capability
 requirement, pass-agreement decision, or resolved callee, it consumes the
 owning representation: `CExpr.ty` / typed Core for type facts,
 Phase 8.5 `CompilerDB` edges/evidence for relational facts. It may validate
 shape; it may not create a second source-level judgment. Done when Elab's
 re-inference code is deleted, not merely bypassed, and regressions prove the
 literal/defaulting, mixed-width, std-bypass, conditional-Copy, and
 capability/type-drift classes cannot be expressed through the typed
 boundary. Later identity/DB refinement remains available for edge facts, but it
 is not on the critical path for type drift.
### Task R-0275

**Objective:** Add an independent Core-certificate checker for a narrow, explicit validated-Core predicate.


 Define a versioned canonical `CoreCertificateV1` and a small separate
 `concrete-cert` library/executable target. Enforce a dependency firewall: it
 may import the canonical artifact/schema decoder, hash/id datatypes,
 **independent declarative specifications and shared data types**, and checker
 theorems; it may
 not import Parser, Check, Elab, Mono, Lower, Report, CLI, project loader, the
 Phase 8.5 scheduler/cache, producer executable judgments such as
 `TypeJudgment`/Layout/fingerprint helpers, or producer verifier
 implementations. Publish its
 source/binary hash, import inventory, `partial`/unsafe/axiom inventory,
 rule-set version, and size so "small checker" remains measurable.

 V1 independently derives its verdict from the canonical Core artifact and
 small specifications. It checks at least: canonical/schema validity;
 artifact, subject, predecessor, and dependency-root binding; unique stable
 identities and referenced-node/provenance existence; constructor-local type
 consistency; direct-call arity and argument/result agreement; absence of
 unresolved placeholders/type variables where the Core boundary forbids
 them; direct-call capability containment; proof-target/body-fingerprint and
 obligation dependency binding; and the Phase 6B validation-chain shape.
 Proof-target/fingerprint/obligation/provenance checks establish identity and
 binding only; they do not establish extraction correctness, theorem/spec
 adequacy, or source correspondence. It
 must not merely compare a producer-emitted fact table with a second
 producer-emitted summary.

 Ownership/borrow/value-flow claims enter V1 only where the artifact carries
 enough independently checkable decisions and path edges to validate them.
 Checking that an ownership field exists is not ownership-checker soundness.
 Extend the predicate one rule family at a time with a mutation fixture. Give
 every predicate an explicit checker-proof status:
 `checker_trusted` for executable checking without a soundness theorem,
 `checker_soundness_kernel_proved` only when Lean proves a theorem such as
 `checkCoreCertificate a c = true -> CoreCertificateV1.Valid a c`, and
 `kernel_replayed` only for the artifact-specific theorem. An unproved checker
 may not appear under the same badge as a proved checker.

 Emit `certificate_structurally_checked`, `certificate_mismatch`, or
 `certificate_unsupported`; never `certified` or `proved_by_lean` merely
 because the structural checker passed. An artifact-specific preservation
 theorem actually replayed by Lean may separately emit `kernel_replayed`.
 State the remaining TCB precisely: V1 does **not** prove parsing,
 resolution, Check/source-to-Core correspondence, ownership/capability
 soundness beyond named local rules, spec adequacy, ProofCore extraction
 beyond discharged Phase 14 rules, native-code behavior, or dependency
 completeness beyond the conservative Phase 8.5 envelope. Its own TCB still
 includes adequacy of the named predicate, canonical decoder, hash algorithm
 and collision assumption, checker implementation unless covered by the
 stated soundness theorem, Lean compiler/runtime and host execution when
 running a compiled checker, and every imported trusted/axiomatic primitive.
 `kernel_replayed` is reserved for an artifact-specific theorem actually
 checked by the kernel; a theorem about the checker does not make execution
 of the compiled checker kernel-only.

 Bind the checker to the Phase 8.5 query DAG: an **independent consumer** may
 distrust cached canonical bytes and the producing compiler for the named V1
 predicate only after that consumer reruns the checker. A receipt rendered by
 the producer remains producer output until external replay. A verification
 receipt names artifact digest,
 dependency root, checker/rule-set version, checked and unsupported
 predicates, theorem/replay identity where any, and exact trust boundary.

 Gate altered expression types, wrong call signatures/results, missing
 capabilities, unresolved type variables, duplicate ids, missing provenance,
 stale dependency roots, artifact/certificate swapping, fabricated proof
 links, mismatched obligation subjects, malformed/noncanonical encodings,
 unsupported constructs mislabeled checked, and a producer record that
 agrees with itself about a false artifact fact. Break a producer judgment/
 layout/fingerprint helper and prove the checker still rejects the bad
 artifact without importing that helper. Wire
 `scripts/tests/check_core_certificates.sh` and a checker import-firewall gate.
### Task R-0276

**Objective:** Reduce ProofCore partial-def opacity only where proof preservation needs it.

 (Rehomed from Phase 6B: its trigger is here, not there.) ProofCore still
 contains many `partial def` walkers/wrappers, which are opaque to the kernel.
 Do not chase a full rewrite. Add non-partial wrappers or structural recursion
 ONLY for the constructs pulled by the R-rule preservation proofs above, so a
 lifted rule can be reasoned about structurally. Gate each lifted rule with one
 theorem that would fail if the wrapper still delegated to an opaque
 partial-def shape.

### Task R-0277

**Objective:** Add the Phase 14 validation artifact: a compiler-soundness dashboard with one witness program per shipped ProofCore construct, one status per R-rule, replay commands for proved/mechanically-validated facts, and regressions proving report facts (`proved`, `stale`, `blocked`, `missing`, `ineligible`, `trusted`) agree with compiler state. Include the `CoreCertificateV1` predicate/rule-set version, checker binary/source hash, soundness theorem names, independent receipt per artifact, mutation corpus, cache-off/on receipt parity, and a machine-readable list of every boundary V1 still leaves producer/compiler-trusted.


## Phase 15: Backend, Target, And Stdlib Contracts

Goal: make backend/toolchain/stdlib assumptions explicit, and state exactly
where source-level proof stops.

Done when: SSA, target/toolchain, optimization, ABI/layout, stdlib evidence,
and Phase 8.5 incremental build contracts are explicit enough for release
evidence; the supported SSA-to-BackendIR translation slice is independently
replayable; and every boundary after that slice remains explicitly trusted.

### Task R-0278

**Objective:** Stabilize SSA as the only backend contract.

### Task R-0279

**Objective:** Document target/toolchain model: triple, data layout, linker, runtime/startup, libc expectation, clang/llc boundary, sanitizer/coverage hooks.

### Task R-0280

**Objective:** Define optimization policy: allowed optimizations, evidence preservation, debug/release behavior, report/codegen validation. Follow the QBE-style "small auditable backend contract first" principle: prefer a stable, inspectable subset with explicit assumptions over early attempts to match LLVM-level optimization coverage.

### Task R-0281

**Objective:** Add native compiled-program debugging support: DWARF/source-map emission, source-mapped backtraces for runtime failures, debug-vs-release behavior, optimized-code caveats, and diagnostics that distinguish source-level runtime checks from trusted backend/OS crashes. Runtime traps inserted by Concrete (bounds, arithmetic, cast/profile checks, failed runtime obligations) should carry enough source span and check-class information that a real application can debug them without reverse-engineering the abort site. This is not proof evidence; it is usability for enforced/runtime- checked facts.

### Task R-0282

**Objective:** Extend Phase 8.5 clean-build versus incremental-build equivalence across backend variants: facts, obligations, diagnostics, reports, BackendIR, codegen units/objects, link manifests, and native behavior must agree under target, ABI/layout, optimization, sanitizer, debug, and toolchain changes.

### Task R-0283

**Objective:** Add ABI/layout round-trip checks: C headers/stubs, offsets, size, alignment, calling conventions.

### Task R-0284

**Objective:** Design and, if pulled by a workload, implement first-class alignment facts.

Start from `research/language/ALIGNMENT_FACTS.md`: compare Zig, Rust, Odin, C, C++, and
SPARK/Ada; distinguish type/layout alignment from object/place alignment and
pointer/reference/slice alignment facts; decide whether Concrete uses
declaration attributes, fact/contract syntax, type-like pointer qualifiers, or
a staged mix. No implementation should start until a forcing workload exists
(SIMD/autovectorized slices, C ABI over-aligned buffers, freestanding DMA/MMIO,
allocator contracts, or an optimization/audit gap). If implemented, gates must
prove stronger alignment satisfies weaker requirements, offset/field/slice
projections conservatively weaken facts, runtime checked refinement records the
fact, unchecked assumptions are `unsafe`/trusted and reported, and backend IR
alignment metadata is emitted only from Concrete facts.
### Task R-0285

**Objective:** Add C/ABI glue validation: generated headers, imported declarations, host stubs, symbol names, calling conventions, ownership transfer, capability labels, and trust assumptions must round-trip through at least one C harness and one Concrete caller.

### Task R-0286

**Objective:** Add outbound FFI / embeddability as a first-class backend/ABI contract.

 Concrete's most likely early adoption path is not replacing a whole
 process; it is exporting a small verified parser, protocol checker,
 constant-time helper, or evidence-bearing subsystem into an existing
 C/C++/Rust/Zig/Go host. Today the FFI story is mostly inbound
 (Concrete calling libc or trusted host functions). The roadmap must also
 cover Concrete code callable from outside.

 V1 deliverables:
 - `#[export]` or equivalent stable export annotation with explicit symbol
   naming / no-mangle policy;
 - `staticlib` and/or `sharedlib` output mode, with the supported target
   triples named;
 - generated C header for the exported ABI subset;
 - exported-function ABI classification: scalar, pointer, small/large
   aggregate, enum/result, string/bytes/path, ownership transfer, and
   unsupported forms;
 - generated boundary docs that expose capabilities, allocation authority,
   failure mode, trusted/Unsafe dependencies, and evidence class for each
   export;
 - evidence bundle next to the library artifact, so a host build can audit
   what the embedded Concrete component claims.

 Hard rule: exported functions must not hide Concrete's thesis at the C
 boundary. If an export allocates, needs a capability, can fail, depends on a
 trusted wrapper, or has only tested/runtime-checked evidence, the generated
 header/docs/report must say so. Gate: one C harness calls a pure exported
 parser/checker, one harness calls an exported fallible API, and one negative
 fixture proves an unsupported export shape fails with a diagnostic rather
 than silently choosing an ABI.
### Task R-0287

**Objective:** Add a C ABI classification suite, inspired by QBE's explicit ABI contract:

scalar parameters and returns, small and large structs, arrays, nested
aggregates, alignment/padding, by-value versus by-reference lowering,
varargs policy, calling convention labels, symbol names, and ownership/
capability/trust annotations. Wire `scripts/tests/check_c_abi_matrix.sh`
with both generated C callers and Concrete callers.
### Task R-0288

**Objective:** Add `BackendIR` as a structured backend contract between `ValidatedSSA` and LLVM/native emission. This is useful even if Concrete never ships its own native backend: it gives the compiler a typed, inspectable place to preserve runtime checks, trap/source-span facts, layout/ABI decisions, target constants, trusted/runtime helper calls, and capability/trust labels before they disappear into LLVM text or target code. The intended ladder is `ValidatedSSA -> BackendIR -> ValidatedBackendIR -> EmitLLVM` first; later emitters (native, C, WASM, QBE-style) may consume the same `ValidatedBackendIR` only after the contract is stable.


V1 should cover a narrow subset: integer arithmetic, bools, branches,
direct calls, returns, checked runtime traps, small loads/stores, source-map
annotations, and helper calls. Add `concrete inspect --backend-ir`,
`--emit-backend-ir`, `concrete verify-ir --pass backend-ir`, golden
backend-IR fixtures, and round-trip checks that source maps, target
constants, runtime checks, and capability/trust labels survive lowering.
Gate old path vs BackendIR-mediated LLVM where both exist:
`interp == old compiled == BackendIR->LLVM compiled`, then retire the old
direct SSA->LLVM path once parity is established.
### Task R-0289

**Objective:** Add sanitizer-backed generated-code validation for trusted/FFI/layout/ pointer-heavy examples, plus the stdlib sanitizer/runtime hooks from Phase

 11. Sanitizer findings are `runtime_checked` / `tested` evidence, not proof.
### Task R-0290

**Objective:** Add backend/codegen differential validation where executable oracles exist.

### Task R-0291

**Objective:** Add compiler self-leak/resource soak harness for long-running workflows.

### Task R-0292

**Objective:** Harden stdlib stability and evidence policy from Phase 7: which stdlib functions are trusted, proved, enforced, allocation-free, capability-free, or assumption carriers.

### Task R-0293

**Objective:** Define stdlib contracts for allocators, I/O handles, directory/file/path handles, byte/text/path conversion APIs, and fallible return discipline.

 Each public stdlib function must state allocation behavior, OS authority,
 failure mode, trusted platform assumptions, and evidence class. This item
 also owns the release-facing unsafe/trusted boundary UX: a safe wrapper may
 narrow authority, but reports must still show the underlying trusted,
 extern, raw-pointer, or `with(Unsafe)` operation and the assumption it
 depends on.
### Task R-0294

**Objective:** Add stdlib evidence gates so core helpers cannot silently widen authority, allocation, proof assumptions, or runtime-error obligations.

### Task R-0295

**Objective:** Evaluate a normalized mid-level IR only when traceability/backend-contract reports expose a concrete gap.

### Task R-0296

**Objective:** Graduate the Phase 7.5 QBE backend from experimental to release-supported only after evidence attachment, optimization policy, backend-IR verification, and backend trust boundaries are trustworthy. Phase 7.5 deliberately emits directly from validated/cleaned SSA to maximize independence and find bugs early; that is the initial usable backend architecture, not yet the release architecture.

 Production QBE support must consume `ValidatedBackendIR`, emit the same source maps and
 target assumptions as other supported backends, pass the full C ABI and
 target/toolchain matrices for its declared subset, integrate Phase 8.5
 incremental artifacts, and report exactly which claims are translation-
 validated versus backend/target-trusted. Keep WASM, Cranelift, and any
 additional backend deferred until a forcing workload or uncovered bug class
 justifies another independent path.
### Task R-0297

**Objective:** Introduce target-specific MIR only for a direct native backend forced after QBE, never as a prerequisite for QBE itself. The staged shape is `ValidatedBackendIR -> <target> MIR -> encoded object fragment`, with MIR owning instruction selection, physical-register constraints, calling- convention realization, relocations, and debug locations. Reuse the Phase

  7.5 immutable per-function job/deterministic merge contract, but keep MIR
  architecture-specific rather than creating a lowest-common-denominator
  universal machine IR. Require behavior coverage and target/ABI matrices
  comparable to the LLVM/QBE gates before any native backend becomes a
  default for any build mode.
### Task R-0298

**Objective:** Define backend-default policy explicitly if multiple production backends graduate: defaults are a versioned `(target, build_mode) -> backend` table, visible in help/audit/build receipts and overridable by an explicit flag. Changing a default requires clean-build/incremental equivalence, behavior and optimization matrices, performance data, a rollback switch, and release notes. Backend availability must never change source-language semantics, proof status, diagnostics before codegen, or package resolution.

### Task R-0299

**Objective:** Add translation validation for codegen as the path out of a fully trusted backend. V1 should validate a narrow `ValidatedSSA -> BackendIR -> ValidatedBackendIR` subset per compile:

 integer arithmetic, fixed arrays, structs, direct calls, branches, bounded
 loops, runtime checks, capability calls, and source-map annotations. The
 validator compares checked Core / typed IR / SSA facts against BackendIR
 facts and reports one of: `translation_compiler_validated_v1`, `translation_trusted`,
 `translation_blocked`, or `translation_mismatch`. This is not a promise to
 prove the whole LLVM/native stack; it is a per-artifact check that the
 Concrete lowering into the backend contract preserves the facts Concrete
 claims. Add `examples/translation_validation/` with straight-line,
 branch/loop, struct/array, runtime-check, and deliberate mismatch fixtures.
 This is the roadmap's early backend-soundness probe: start with the small
 subset as soon as SSA/backend facts are stable enough, rather than waiting
 for a full native-backend story before learning whether Concrete's evidence
 can cross the backend boundary.
 Prior art: CompCert for proved compilation/pass-correctness discipline and
 Alive2 for SMT-checked LLVM IR equivalence/optimization validation. The
 concrete threat model here is the class differential fuzzing has already
 exposed: source/Core facts can be true while a backend lowering silently
 emits a different native artifact unless that boundary has its own check.
 Wire `scripts/tests/check_translation_validation.sh`; the gate must prove a
 Lean-proved/Core-level claim cannot be presented as native-code evidence
 unless the backend artifact is either translation-validated or explicitly
 backend-trusted in the audit bundle.
### Task R-0300

**Objective:** Make the Phase 15 translation-validation slice independently replayable.

 Extend the Phase 14 `concrete-cert` target with a versioned canonical
 `BackendTranslationCertificateV1` over hash-bound
 `ValidatedSSA -> BackendIR -> ValidatedBackendIR` artifacts. The checker
 validates the explicit input/output relation; it does not trust
 producer-emitted `preserved` flags or compare two summaries produced by the
 same lowering pass. Its operator, trap, intrinsic/helper, ABI-layout, and
 target specifications must be independent declarative definitions; it may
 not reuse Lower/Emit/Layout decision helpers whose common-mode bug it is
 intended to catch.

 V1 covers integer width/signedness and operator selection, bools, direct
 calls and signatures, returns, branches, fixed-layout structs/arrays,
 required arithmetic/bounds/runtime traps, layout/target constants,
 source-map identities, helper/intrinsic calls, and capability/trust labels
 for the subset admitted by Task R-0299. V1 admission is whole-function: if an
 unsupported operation can influence control flow, memory, calls/arguments,
 or the return value, that entire function is `translation_trusted` or
 `translation_blocked`. Region-level validation is forbidden until explicit
 compositional boundaries and a composition theorem exist. Track call-closure
 separately: a caller may satisfy the local V1 lowering relation while a
 reachable callee remains backend-trusted, so that is not end-to-end validated
 function behavior.

 Bind Phase 8.5 codegen-unit manifests and object/link identities into the
 dependency root with explicit edge status. The independent V1 **relation**
 check stops at `ValidatedBackendIR`: `BackendIR -> object` and
 `object -> binary` are `identity_bound_only` / `backend_trusted`, not
 semantically checked. LLVM optimization, LLVM text-to-object
 translation, object writer, linker, ABI/runtime behavior, and native
 execution remain trusted/tested unless a later validator explicitly covers
 them. Object and binary hashes prove identity, not semantics.

 Mutate signed versus unsigned division, integer width, required overflow or
 bounds trap, branch target, field offset/layout, direct-call target/signature,
 capability/trust label, source-map id, target/profile, codegen-unit owner,
 object/link manifest, and certificate/build binding. Include an unsupported
 instruction mislabeled as validated. Independent replay must reject each
 checked-relation mutation without calling the producer backend. Also combine
 BackendIR A with object B and consistently rehash a fresh internal graph: in
 the absence of an externally expected root the checker must report an
 unvalidated backend edge, not claim semantic mismatch detection. Wire
 `scripts/tests/check_backend_translation_certificates.sh`.
### Task R-0301

**Objective:** Add the Phase 15 validation artifact: a backend/std-lib contract project with ABI/layout C round trips, C/ABI glue generation/import checks, the C ABI classification matrix, alignment-fact fixtures if Task R-0284 is pulled into implementation, backend-IR emission/verifier checks, producer translation-validation checks, independent `BackendTranslationCertificateV1` replay and mutations, sanitizer runs, compiled-oracle differential tests, native debug/source-map smoke tests, clean-vs-incremental fact equivalence, cached-object substitution negatives, and stdlib

 authority/allocation/evidence gates. The artifact must show the exact point
 at which independent checking stops and backend/toolchain trust begins.
### Task R-0302

**Objective:** Pin the supported LLVM/clang version and gate version drift. The backend today targets whatever `clang` defaults to on the host (`docs/EXECUTION_MODEL.md`), yet the TCB already conditions backend correctness on "LLVM version" — so a floating toolchain leaves the trusted boundary a moving, un-audited target and makes native output non-reproducible across hosts (`docs/DETERMINISM.md` already disclaims binary reproducibility for exactly this reason). Declare one supported LLVM version (or a narrow tested range) as part of the backend contract, and make an upgrade a deliberate, gated event: on a version bump the wrong-code corpus, the backend-contract gate, and IR golden tests must re-pass before the new version is

 declared supported. This is the cheap control that turns the moving boundary into
 a fixed one; the structured `BackendIR` contract (Task R-0288) and a future hand-written
 LLVM bitcode emitter (the Zig/Roc decoupling technique) are the heavier long-term
 options, pulled only if version pinning proves insufficient. Wire the check into
 `scripts/tests/check_backend_contracts.sh`: the compiler refuses or warns on an
 unsupported LLVM version, and the drift corpus re-runs on a bump.
 - 20a. LLVM bitcode emitter — the heavy decoupling rung (pull-gated, deliberately
   NOT its own phase). *What it is:* emit LLVM **bitcode** (LLVM IR's stable
   serialized binary format) from a hand-written serializer that never links or
   calls the churning LLVM C++ API. Zig's self-hosted compiler is the reference
   implementation — it writes bitcode in Zig and hands it to whatever LLVM is
   present, making it immune to the C++-API breakage that costs most frontends an
   upgrade tax; Roc reused Zig's bitcode writer wholesale in its 2026 Rust→Zig
   rewrite. Concrete today emits *textual* LLVM IR to `clang`
   (`Concrete/Backend/EmitLLVM.lean`), which already avoids C++-API linkage but
   stays coupled to textual-IR *syntax* drift across LLVM versions (the
   opaque-pointer migration was one such event). A bitcode writer would remove
   that coupling and enable `clang`-free, bit-identical cross-host emission.
   *Why an item and not a phase, and why deferred:* (1) it is a sub-component of the
   backend contract that gates nothing downstream; (2) it is *less* proof-relevant
   than translation validation (Task R-0299) — it buys decoupling/reproducibility,
   not correctness — so elevating it above Task R-0299 would invert priorities for
   a proof-carrying compiler; (3) the cheap version pin (Task R-0302) already fixes the moving
   trusted boundary, and LLVM churn has not been shown intolerable (one migration
   eaten so far), so a phase-sized commitment now would be speculative scheduling.
   *Pull-trigger* — promote to scheduled work when EITHER a freestanding / no-`clang`
   target is pursued (Phase 16), where self-contained emission stops being optional,
   OR LLVM version churn recurs intolerably despite Task R-0302's pin. Same ladder as
   `BackendIR` (Task R-0288): `ValidatedSSA → BackendIR → ValidatedBackendIR → EmitLLVM`
   today; bitcode / own-backend later. Prior art: Zig (hand-written bitcode writer,
   self-hosted backend), Roc (reused it in the Zig rewrite).
### Task R-0303

**Objective:** Compiler-writing / verified-self-host language prerequisites (pull-gated research). Features a future proof-preserving self-host (CakeML-style, Phase 14) or a compiler-in-Concrete workload would need, surfaced by what makes ML/OCaml and Zig good compiler hosts. None is scheduled; each is pulled by a real workload and must preserve the linear / no-GC / visible-layout thesis — the OCaml GC path is deliberately not taken; this task's non-GC arena and typed-index subitems plus the layout path are.

 - 21a. Guaranteed tail-call optimization: a tail call lowers to a loop, so
   recursive tree-walks (the dominant compiler shape) run in constant stack. Today
   recursion is a stack-overflow risk the predictable profile avoids; guaranteed
   TCO makes tail recursion predictable-profile-safe rather than banned. Pure
   codegen guarantee — no ownership/capability/proof interaction. Gate: a deep
   tail-form fixture runs in O(1) stack; non-tail recursion is diagnosed, not
   silently stack-unsafe.
 - 21b. Bit-level data layout: arbitrary-width integers (`uN`/`iN`) and packed
   structs/enums with defined bit layout — compact IR nodes, discriminant/flag
   packing. Fits the visible-systems-layout thesis directly. Gate: a packed
   struct's size and field offsets equal the declared bit layout; an over-wide
   store into a `uN` field is rejected or wraps per the declared width, not
   silently. (SOA containers and `std.bigint` are stdlib-level, pulled the same
   way; nested patterns is tracked in `docs/NESTED_PATTERNS.md`.)
### Task R-0304

**Objective:** Keep inbound FFI, outbound FFI, and unsafe-profile work connected but distinct. Inbound FFI says how Concrete safely calls host code; outbound FFI says how host code safely calls Concrete exports; Phase 15.5 says how the trusted/raw-pointer implementation islands remain auditable. Release-facing docs must not collapse these into a vague "FFI supported" claim. Each side needs its own examples, ABI restrictions, ownership-transfer rules, capability/evidence report, and red-team negatives.


## Provisional Phase 15.25: WebAssembly Portability And Sandbox Backend

Status: provisional. This phase records the preferred backend after QBE; it
does not authorize WebAssembly implementation in parallel with Phase 7.5 or
before the entrance criteria below pass.

Goal: add an independently lowered, portable, sandbox-oriented backend that
pressures Concrete's memory, control-flow, capability, layout, and trap
semantics differently from the native LLVM and QBE paths.

Entrance criteria:

- QBE's documented subset works through ordinary `build`, `run`, and `test`
  workflows and its backend boundary, `CodegenInput`, capability matrix,
  diagnostics, and artifact/replay model are stable.
- Phase 15 has established `ValidatedBackendIR`, translation-validation status,
  and explicit target/toolchain/runtime contracts that WebAssembly can consume
  without inventing a parallel middle end.
- At least one named portability, sandboxing, deployment, or validation
  workload demonstrates value that LLVM plus QBE do not provide.
- A short design record confirms Wasm remains preferable to Cranelift for that
  workload. If fast native/JIT compilation is the actual requirement, schedule
  Cranelift instead and leave this phase provisional.

Initial scope:

### Task R-0305

**Objective:** Target Wasm32 with a pinned WebAssembly toolchain and one documented WASI profile. Browser integration, the component model, Wasm GC, Wasm64, threads, SIMD, and exception handling are explicitly later work unless a forcing workload promotes one of them.

### Task R-0306

**Objective:** Lower `ValidatedBackendIR` into a small typed Concrete WebAssembly AST, run a pure Concrete verifier, emit deterministic `.wasm`, and validate it with the pinned external validator. Do not emit ad-hoc WAT from CLI code and do not route WebAssembly through LLVM.

### Task R-0307

**Objective:** Reuse canonical semantic, layout, builtin, trap, source-identity, and capability facts, while keeping WebAssembly instruction selection and control-flow restructuring independent from LLVM and QBE lowering.

### Task R-0308

**Objective:** Deliver thin vertical slices in this order: `main -> 42`; direct calls and returns; branches/loops; integer operations and checked traps; linear-memory loads/stores; basic aggregates; runtime imports. Each slice ends in an executable differential fixture.

### Task R-0309

**Objective:** Map external authority deliberately onto an explicit WASI import manifest.

Missing capabilities and unsupported operations fail before partial code
generation with stable `wasm_pending` diagnostics; there is no LLVM/QBE or
per-function fallback.
### Task R-0310

**Objective:** Integrate `--backend wasm`, `--emit-wasm`, projects, retained artifacts, debug bundles, replay commands, caching, `doctor`, feature JSON, audit output, and ordinary `run`/`test` workflows through the backend-neutral driver.

### Task R-0311

**Objective:** Compare interpreter, LLVM, QBE, and WebAssembly observations for every eligible fixture: stdout, stderr, exit status, and normalized trap/failure class. Add Wasm-specific cases for structured control flow, linear-memory bounds, alignment, integer-width conversion, imports, and deterministic module composition.


Done when: the declared Wasm32/WASI subset builds and runs at least one Phase
8-class workload through normal user workflows; its capability matrix is
complete and fail-closed; emitted modules and manifests are deterministic and
independently validated; the four-way differential suite covers the supported
subset; and audit/evidence output labels WebAssembly results honestly as
`tested` or `backend_trusted`, never as proof that native or source semantics
are preserved.

## Phase 15.5: Unsafe Concrete And Trusted Systems Profile

Goal: make deliberately unsafe Concrete code boring to write, easy to audit, and
hard to misuse. This phase borrows the useful Zig lesson: low-level programs
need excellent tools for pointers, slices, allocators, debug allocators, and
runtime instrumentation; the answer is not a global unsafe mode or Rust-style
lifetime algebra, but small explicit unsafe islands with visible authority and
replayable checks.

Done when: every raw-pointer, unchecked, trusted, allocator, and FFI escape hatch
has a named safe wrapper pattern, audit/report output shows the underlying
assumption, debug/profile instrumentation can catch representative misuse, and a
validation project proves safe code cannot reach these operations accidentally.

This phase does **not** weaken Concrete's philosophy. Safe Concrete remains
linear, capability-visible, second-class-reference, and no-hidden-cleanup.
Unsafe Concrete is a deliberately marked profile for the code that must cross
those boundaries: runtimes, allocators, FFI, VM/interpreter internals,
freestanding/MMIO, sanitizer probes, and backend/ABI glue. Its evidence class is
`trusted`, `runtime_checked`, `tested_by_oracle`, or `assumed` unless a later
proof actually discharges the exact claim.

Reference languages and reading:
- Zig for pointer/slice taxonomy, allocator-passing, arena/fixed-buffer/debug
  allocators, and the "prefer slices over raw many-item pointers when length is
  known" rule:
  `https://ziglang.org/documentation/master/` and
  `https://zig.guide/standard-library/allocators/`.
- Hare for checked slices by default, unbounded arrays as an explicit escape
  hatch, small Unix-shaped wrappers, and honest docs about what remains manual:
  `https://harelang.org/blog/2022-06-21-safety-features/` and
  `https://harelang.org/blog/2021-02-09-hare-advances-on-c/`.
- Odin for tracking allocators, sanitizer/runtime-instrumentation pragmatism,
  foreign-system ergonomics, layout/endian control, and fine-grained
  bounds-check controls:
  `https://odin-lang.org/docs/overview/` and
  `https://odin-lang.org/docs/faq/`.
- Rust/Rustonomicon as cautionary input: unsafe reference/aliasing validity is
  subtle, pervasive unsafe can become harder to audit than raw pointer code, and
  Concrete should avoid silently upgrading raw pointers into ordinary safe
  references:
  `https://dev-doc.rust-lang.org/beta/nomicon/what-unsafe-does.html` and
  `https://doc.rust-lang.org/stable/reference/behavior-considered-undefined.html`.
- Counterpoint reading, to keep the claims honest:
  `https://rustmagazine.org/issue-3/is-zig-safer-than-unsafe-rust`.

Borrow the techniques, not the philosophy. Concrete must not adopt hidden
authority, implicit allocation, implicit cleanup, lifetime syntax, trait-object
unsafe abstractions, ambient context, or a broad "unsafe mode." Debug allocators,
sanitizers, and runtime checks are `runtime_checked` / `tested` evidence, never
proof. Unsafe ergonomics are allowed only when they make the trusted assumption
more legible in source and reports.

### Task R-0312

**Objective:** Freeze the unsafe-surface taxonomy:

- safe references remain second-class and may not be returned;
- raw single pointers, many-item pointers, fixed-size array pointers, and
  slices are distinct source/report concepts;
- nullable pointers are explicit (`Option<Ptr>`-style), not the default;
- every raw pointer/slice carries mutability, provenance/trust class,
  alignment fact where known, and optional length/bounds facts where known.
Add `docs/UNSAFE_CONCRETE.md` and make it point back to Phase 12's
`UNSAFE_ISLAND.md`, not duplicate it.
The design note must include a comparison table mapping Zig's `*T`, `[*]T`,
`[*c]T`, `?*T`, slices, fixed array pointers, Hare unbounded arrays/slices,
and Odin pointers/slices onto Concrete's chosen surface. Every row must name
whether bounds/null/alignment/provenance are statically known, runtime-
checked, trusted, or unavailable.
### Task R-0313

**Objective:** Add safe-wrapper recipes for the common unsafe shapes: checked slice from raw pointer + length, bounds-checked indexed access, non-null construction, alignment-refined pointer construction, FFI-owned handle wrappers, allocator-backed buffers, and raw-place writes. Each recipe must show the public safe wrapper, the private trusted/Unsafe operation, the accepted assumption, and the report line that exposes it.

### Task R-0314

**Objective:** Add a debug allocator / checked allocator profile inspired by Zig's development allocators: allocation/free stack identity where available, double-free detection, use-after-free-like sentinel checks where applicable, leak reporting for tests, red-zone or canary checks where practical, and allocator-name reporting in audit output. This is `runtime_checked`, not proof evidence. It must work with Concrete's allocator-as-value direction:

`with(Alloc)` grants authority; allocator values name strategy/identity.
Include the Odin tracking-allocator lesson, but reject Odin's implicit
`context` as hidden authority: Concrete allocator identity is an explicit
value or report fact, and allocation permission remains visible as
`with(Alloc)`.
### Task R-0315

**Objective:** Add pointer/slice runtime-instrumentation hooks for unsafe code: null checks, bounds checks for trusted slice views, alignment checks, provenance/trust labels, lifetime/owner debugging where the runtime can track it, and explicit opt-out for trusted performance paths. A checked profile may trap; a release profile may erase checks only when reports still say which checks became assumptions.

Include sanitizer opt-out attributes only as named unsafe/trusted facts
(`no_sanitize_address`-style behavior must appear in audit output), never as
comments or unreported compiler flags.
### Task R-0316

**Objective:** Add an unsafe-aliasing policy that avoids Rust's hidden reference-UB trap:

safe references keep Concrete's ordinary borrow/linearity rules; raw pointers
do not silently become safe references. Any operation that temporarily
upgrades raw memory to a safe borrow must state its exclusivity, lifetime, and
frame assumptions in the wrapper contract/report. No "trust me, this raw
pointer is an ordinary `&mut` now" shortcut.
### Task R-0317

**Objective:** Add allocator-strategy examples: global allocator, arena/bump allocator, fixed-buffer allocator, debug allocator, and test allocator. Every example must show allocation authority, allocator identity, cleanup path, and failure behavior. Do not add ambient allocation.

### Task R-0318

**Objective:** Add VM/interpreter-style pressure test as the validation workload:

a small bytecode stack, value array, call-frame/upvalue-like structure, or
mark/sweep-shaped toy heap that deliberately needs raw pointers/slices. The
workload should compare the safe-indexed version and the unsafe-pointer
version, then prove the unsafe version's assumptions are visible in reports
and checked in debug/profile gates.
The pressure test should deliberately exercise the cases from the Zig/Rust
unsafe-code debate: stack-top pointer movement, array/slice access, closure
or upvalue-like indirection, allocator-triggered collection/checking, and a
path where a safe-index/handle version is simpler but measurably different
from the unsafe pointer version. The output is not a performance claim; it is
an ergonomics/evidence stress test.
### Task R-0319

**Objective:** Add FFI/trusted-boundary red-team tests: wrong length, stale pointer, null pointer, alignment mismatch, double free, missing capability, hidden allocation, and wrapper that forgets to report the underlying trusted call.

Safe code must fail to reach the operation; unsafe code must appear in
`concrete audit --trust-boundaries`.
### Task R-0320

**Objective:** Add syntax/ergonomics only after the taxonomy and gates exist. Candidate sugar may include pointer-field access or checked slice construction helpers, but it must not reintroduce removed `->` syntax as a general language form and must not blur raw pointers with safe references. Ergonomics serve audit, not the other way around.

Candidate ergonomics must be rejected if they make a raw pointer look like a
safe reference, hide a bounds/null/alignment assumption, or make an unchecked
operation harder to grep.
### Task R-0321

**Objective:** Add the Phase 15.5 validation artifact:

 `examples/unsafe_concrete_vm/` plus
 `scripts/tests/check_unsafe_concrete.sh`. The gate must run the workload in
 normal and checked profiles, compare interpreter-vs-compiled behavior where
 applicable, prove audit output lists every trusted/Unsafe/allocator/FFI
 assumption, and include at least one mutation per unsafe class that the
 checked profile or audit gate catches.

## Phase 16: Freestanding And Embedded Target

Goal: make Concrete's hosted-vs-freestanding boundary explicit enough for
embedded, kernel, and audit-critical targets without destabilizing the hosted
language.

Prerequisite: the hosted stdlib/runtime boundary, allocator story, target model,
and daily workflow in Phases 8, 9, and 12 must be stable. Freestanding is not
a second backend escape hatch; it is a target profile with fewer assumptions.

Trigger: start this phase when at least one serious workload needs no libc,
explicit allocator/runtime setup, or embedded/kernel-style startup, and the
hosted evidence pipeline can already explain what will be removed.

Done when: a freestanding Concrete program can build under a named target
profile, with no ambient hosted stdlib assumptions, explicit allocator/startup
choices, and an audit report naming every remaining target/runtime assumption.

### Task R-0322

**Objective:** Define `hosted` versus `freestanding` target profiles: libc, startup, allocator, panic/abort behavior, stack assumptions, I/O availability, floating-point assumptions, and supported capabilities.

### Task R-0323

**Objective:** Split stdlib modules by target profile: core no-alloc/no-OS modules, allocator-backed modules, hosted OS modules, and explicitly unavailable modules.

### Task R-0324

**Objective:** Define freestanding capability policy: no ambient `File`, `Network`, `Console`, or `Process`; target-specific capabilities must be declared and audited.

### Task R-0325

**Objective:** Define hardware-access primitives and evidence classes before device-driver examples: `volatile`/MMIO operations require explicit `with(Mmio)` or `with(Device)` capability, inline assembly is `trusted` and requires `with(Unsafe)`, and interrupt handlers are an effectful/trusted boundary with explicit target assumptions.

### Task R-0326

**Objective:** Add explicit allocator/runtime hooks for freestanding builds, including ownership of allocation failure behavior and cleanup expectations.

### Task R-0327

**Objective:** Add linker/startup configuration: entry symbol, no-main mode, target triple, data layout, linker script hooks, and section/layout assumptions.

### Task R-0328

**Objective:** Add freestanding diagnostics: reject hosted APIs, hidden allocation, libc calls, unsupported target features, and unavailable capabilities.

### Task R-0329

**Objective:** Add one freestanding example: bounded parser, small checksum/hash kernel, or fixed-capacity state machine with no allocation and no hosted I/O.

### Task R-0330

**Objective:** Add one embedded-style audit bundle naming all remaining target assumptions:

stack, interrupt model if any, allocator/runtime hooks, endian/layout, and
backend/toolchain boundary.
### Task R-0331

**Objective:** Keep production freestanding WASM, QBE, and additional backend targets deferred until freestanding profiles prove the current LLVM path is not enough. This does not defer Phase 7.5's hosted experimental QBE differential oracle: that path validates cleaned SSA/codegen under hosted toolchains and carries only `tested`/`backend_trusted` evidence. Any QBE freestanding target, runtime/startup contract, or release claim still graduates here and through Phase 15's backend/target contract.

### Task R-0332

**Objective:** Add the Phase 16 validation artifact: one freestanding demo project plus an MMIO/device-profile mock audit bundle. The demo must build with no hosted APIs, name allocator/startup/linker assumptions, reject hidden libc or allocation, and report `with(Device)`/`with(Mmio)`/`with(Unsafe)` evidence classes without pretending hardware behavior is proved.


## Phase 17: Public Release Bar

Goal: make Concrete understandable and usable by someone who did not build the
compiler.

Done when: a fresh user can install Concrete, run a proof-bearing example,
inspect its audit bundle, independently verify its evidence root offline, and
understand the claim matrix and remaining trust boundary in under ten minutes.

**Language graduation is a gate, not a date.** Concrete can graduate individual
examples before the language graduates. The language reaches **alpha** only
after the source-contract/proof-link path is usable outside flagship hero work;
it reaches **beta/release** only after ordinary project workflow and external
validation are in place.

**Alpha bar (language can be presented as a usable experimental language):**
- At least one non-author writes, proves, or contract-annotates a useful
  Concrete program and reports that ProofKit + contracts + `concrete prove`
  were worth the discipline.
- Source contracts are primary for at least one flagship, with source-linked
  proofs and no JSON dependency for that flagship's main proof surface.
- `concrete prove` guides a user through contract -> obligation -> Lean/omega/
  `bv_decide` evidence -> source link -> audit.
- Small evidence examples exist for every public claim class:
  `proved_by_lean`, `proved_by_kernel_decision`, `tested_by_oracle`,
  `assumed`, `trusted`, `partial`, and `stale`.
- Core language surface blockers are settled or explicitly deferred with
  examples: pattern cleanup, bytes/text/path, iteration, collections,
  ignored-result diagnostics, and capability polymorphism.
- Runtime safety obligations have defined evidence classes for bounds,
  div-zero, overflow, casts, panic/abort, and unchecked conversion behavior.
- README, website, papers, examples, and roadmap agree and do not outclaim
  audit evidence.

**Beta/release bar (language can be released to users beyond the thesis
audience):**
- `concrete fmt`, `concrete test`, and a minimal project model (`Concrete.toml`)
  exist.
- In-source proof links are the only proof path; `proof-registry.json` support
  must remain absent.
- Release bundles have stable schemas, replay commands, assumption/trust
  reports, and proof-link provenance.
- An installed independent `concrete-cert verify-bundle` path can validate a
  release evidence root offline from untrusted bundle bytes, report the precise
  Core/backend predicates it checked, replay kernel evidence where claimed, and
  keep every unsupported/native boundary visibly trusted.
- A verified-profile policy gate exists and is used by every release-facing
  proof/audit claim; public examples cannot bypass it with stale proofs,
  unapproved assumptions, or hidden trusted/Unsafe boundaries.
- The stdlib/runtime boundary is stable enough for daily examples.
- At least one external user completes the first-user tutorial and a useful
  audit/proof workflow without compiler-author intervention.

### Task R-0333

**Objective:** Define first public release criteria: supported subset, required examples, required diagnostics, proof workflow, stdlib/project UX, evidence/policy/ tooling story.

### Task R-0334

**Objective:** Add the first-user teaching path as a release-bar artifact, not marketing copy. Deliverable: `docs/LEARN_CONCRETE.md` plus a checked tutorial transcript that covers install/build/run/test, `Copy` vs linear values, consume/destroy/handoff, capabilities in function headers, `Result` / ignored-result diagnostics, stdlib basics, one runtime trap, one audit report, and the five evidence classes. The tutorial must use runnable snippets covered by the doc-snippet gate and must not claim proof where the compiler reports only testing, runtime checking, assumption, or trust.

### Task R-0335

**Objective:** Publish a versioned authoritative language reference before any public release claim. The tutorial/book is not the reference. The current normative material is scattered across grammar, value-flow, execution, invariants, and evidence docs; release users need one versioned document whose sections define the language surface, not merely explain it.


 Deliverable: `docs/LANGUAGE_REFERENCE_V0.md` (or generated equivalent)
 covering syntax, lexical forms, type/value model, Copy vs linear values,
 explicit destroy/defer, capabilities, references and borrow blocks,
 statements/expressions, modules/imports/visibility, generics/monomorphization,
 trusted/unsafe/raw pointers, runtime checks/traps, main/exit semantics,
 stdlib boundary expectations, and evidence taxonomy. Every section links to
 its machine-checked gate, diagnostic, or "not yet supported" rejection.
 Gate: `scripts/tests/check_language_reference.sh` verifies every construct
 named in the grammar has a reference section, every anti-feature/rejected
 construct has a diagnostic or policy citation, and README/site/book links
 point to this reference as the normative source.
### Task R-0336

**Objective:** Publish a public claim matrix: what Concrete proves, enforces, reports, assumes, and trusts.

### Task R-0337

**Objective:** Add release claim freeze: README, `CLAIMS_TODAY.md`, roadmap, showcase manifest, and release bundles must agree.

### Task R-0338

**Objective:** Add compatibility policy for proof artifacts and fact schemas.

### Task R-0339

**Objective:** Add compatibility policy for generated contract/VC obligation IDs:

obligation IDs should stay stable across harmless formatting and local
refactors, and any unavoidable churn should be reported as artifact churn,
not hidden under ordinary proof drift.
### Task R-0340

**Objective:** Add public API compatibility checking before release:

`concrete api-diff old/ new/ --json` compares exported modules, function
signatures, capabilities, allocation behavior, evidence classes, stdlib API,
package interface artifacts, and generated docs. This is the Swift
API-digester lesson adapted to Concrete: API drift is a reportable fact,
not a release-note afterthought.
### Task R-0341

**Objective:** Define release/showcase evidence policy by class:

`proved_by_lean` and `proved_by_kernel_decision` are strong evidence;
`tested_by_oracle` is supporting evidence; `proved_by_smt` /
`solver_trusted` require explicit policy approval; `open` and unreviewed
`assumed` are forbidden for release claims. The policy must be executable by
the Phase 10 verified-profile command, not just written in prose.
### Task R-0342

**Objective:** Add public examples policy: public-facing examples, website copy, README snippets, paper examples, and showcase manifests must not outclaim their proof status. Active candidates can be shown as active work, but cannot be presented as proved or graduated until their bars land.

### Task R-0343

**Objective:** Add public security/soundness disclosure policy: compiler/proof pipeline bugs are security-relevant.

- 9a. Publish `SECURITY.md`: how to report a compiler/proof-system bug
  privately, response-time commitment, advisory format, and severity
  classes — a soundness bug that lets a false claim go green
  (checker hole, extraction bug, stale-evidence upgrade) is treated as
  highest severity even when nothing crashes. The policy must name scope:
  compiler crashes on valid input, silent miscompiles, verifier/proof
  unsoundness, stale-proof upgrades, hidden capability/allocation/trusted
  boundaries, package-evidence forgery, and stdlib/runtime safety holes are
  all security-relevant for Concrete. Advisories enumerate affected claims by
  reading the obligation ledger of affected releases, not by prose
  recollection. Gate: `scripts/tests/check_security_policy.sh` verifies
  `SECURITY.md` exists, is linked from README/site/release bundle docs, and
  names the supported versions/reporting channel before any public release
  target can pass.
- 9b. Define the proof-revocation procedure BEFORE the first release that
  carries proof claims: when a shipped theorem/evidence class is
  invalidated (proof bug, axiom-inventory violation, solver unsoundness,
  spec error), there must be a mechanical path that (1) republishes the
  release manifest with the affected facts downgraded to their honest
  class (`open` / `needs_recheck` / `trusted`), (2) emits an advisory
  listing every downgraded claim id, and (3) makes the old bundle
  fail verification loudly rather than silently coexist. Red-team gate:
  `scripts/tests/check_proof_revocation.sh` simulates revoking one
  flagship theorem (e.g. remove it from the axiom-inventory allowlist or
  break its fingerprint) and must show the release bundle flips to
  failing with the downgraded claim named — no false green on a revoked
  proof. Task R-0353's independent verifier must reject the old evidence
  root and any cached receipt that still names the revoked theorem.
### Task R-0344

**Objective:** Add a certification-style assurance bundle profile before any SPARK-class comparison claim. The bundle must include the claim matrix, authority/capability report, obligation ledger, proof status, runtime-safety status, assumptions, trusted boundaries, package/dependency evidence, toolchain versions, replay commands, and any SPARK-class flow/frame facts.

 It must also include an agent-readable summary naming which annotations were
 checked and which were only suggested or future-only. This is a release
 artifact over existing facts, not a second evidence system.
### Task R-0345

**Objective:** Publish `THREAT_MODEL.md` and keep it linked from README, release bundles, showcase manifests, and assumptions docs.

### Task R-0346

**Objective:** Add first-user workflow CI: install compiler, create/run one example, inspect one audit bundle without repo-local assumptions.

### Task R-0347

**Objective:** Improve onboarding, tutorial, and docs around `proved` / `enforced` / `reported` / `assumed` / `trusted`.

### Task R-0348

**Objective:** Resolve string interpolation / formatting as an explicit release-facing decision, not an orphaned gap. V1 rejects string interpolation syntax, format macros, and trait-object-style `Display` because they hide allocation, dispatch, and formatting authority. The supported path is `std.fmt` over `std.io.Writer`, with any string-returning helper carrying `with(Alloc)` and any future convenience helper pulled by workloads.


  Required cleanup: remove or close the stale `LANGUAGE_GAPS.md` and
  `docs/bugs/README.md` entries that call interpolation a blocker unless a
  concrete workload reopens the decision. If a future workload proves the
  Writer path insufficient, add a new design note that preserves the same
  evidence: visible allocation, no macros, no hidden dynamic dispatch, and a
  gate proving fixed-buffer formatting remains allocation-free.
### Task R-0349

**Objective:** Refresh or archive stale gap/backlog documents before release. Documents such as `docs/LANGUAGE_GAPS.md`, root-level idea piles, and old stdlib reviews may be useful historically, but they cannot sit next to the roadmap making claims that contradict closed known holes or current semantics. Gate: `scripts/tests/check_gap_docs_current.sh` rejects stale "true blocker" / "largest gap" claims unless they map to an open roadmap item or known-hole id; otherwise the material moves to CHANGELOG/archive with a date and status.

### Task R-0350

**Objective:** Add positioning page against Rust, Zig, Lean, SPARK/Ada, Austral, Hylo, Cogent, Dafny, F*, Why3.

### Task R-0351

**Objective:** Add migration/adoption playbook: what C/Rust/Zig code moves first, how to wrap libraries honestly, what stays outside Concrete.

### Task R-0352

**Objective:** Add release/install distribution matrix: host triples, checksums/signing, install paths, supported/deferred channels.

### Task R-0353

**Objective:** Define the canonical release-evidence DAG and offline independent bundle verifier before packaging or benchmarking it.


 Move the common public/verifier codecs here: versioned canonical encoders
 for source/lock inputs, typed-Core artifacts and Phase 14 receipts,
 obligations/proof bundles, SSA/BackendIR artifacts and Phase 15 receipts,
 codegen-unit/object/link manifests, binary identity, policy/profile,
 assumptions/trust facts, checker/toolchain identities, and proof replay
 payloads. Task R-0381 later wraps these in package interface/body formats;
 it does not first define encoders required by this release gate.

 Every edge declares its relation and status (`identity_bound_only`,
 `producer_validated`, or a named independently checked relation), and the
 graph schema names mandatory edges. The bundle exposes one computed evidence
 root, but self-consistency is not authenticity: verification requires an
 externally supplied pinned expected root or a policy-accepted signature/
 builder attestation over that root. A root/signature authenticates identity
 and provenance, not semantic correctness.

 Define `concrete-cert verify-bundle <bundle> --expected-root <root>` plus a
 `concrete verify-bundle` alias only if it invokes the same checker. It runs
 without repository/network state, treats bundle bytes as hostile, recomputes
 canonical nodes/edges/root, reruns Phase 14/15 certificate predicates, and
 validates checker/rule-set/toolchain/target/profile/policy binding. Output
 separate dimensions: `internally_consistent`, `root_matches_expected`,
 `provenance_authenticated`, `core_certificate`, `backend_translation`,
 `proof_replay`, `policy`, `assumptions`, and `remaining_trust`—never one
 ambiguous `verified` badge.

 The schema can reject a missing mandatory edge; the Merkle graph cannot
 discover an arbitrary omitted semantic dependency unless completeness is
 separately proved. Proof/Core and SSA/BackendIR substitutions may fail their
 independently checked relations. Object/binary substitutions fail only the
 expected-root/provenance binding; `BackendIR -> object -> binary` remains
 backend-trusted. A fresh internally consistent root over different native
 bytes is not a semantic mismatch.

 Specify a self-contained offline proof-replay payload and runtime: theorem
 sources or checked proof objects, generated workspace/manifest, exact Lean/
 ProofKit dependencies, kernel/tool versions and hashes, and licenses. If a
 platform bundle cannot provide that replay surface, report
 `proof_replay=unavailable`; it may not satisfy the beta/release bar for a
 claim advertised as offline kernel-replayable.

 Gate expected-root mismatch, accepted/bad signature, proof from build A with
 Core from B, SSA/BackendIR substitution, object/binary substitution,
 schema-required edge deletion, declared dependency-root mismatch, revoked
 proof, unsupported relation mislabeled checked, malformed cycles/duplicate
 ids, and missing checker/theorem identity. A byte mutation must produce a
 different root and fail against the pinned expected root (or lose accepted
 authentication), not necessarily fail internal-consistency parsing. Wire
 `scripts/tests/check_verify_bundle.sh`.
### Task R-0354

**Objective:** Add concrete distribution UX:

 `concrete --version --json`, `scripts/release/build_dist.sh`,
 `dist/concrete-<version>-<target>.tar.gz`, `dist/SHA256SUMS`,
 `dist/SIGNATURES` if signing is enabled, `INSTALL.md`,
 `RELEASE_NOTES.md`, and uninstall/upgrade notes. Nix/Homebrew or similar
 channels may be added only if they can be kept reproducible and
 version-pinned. The archive must ship the independently built
 `concrete-cert` verifier, expose `concrete-cert --version --json`, record its
 source/binary/rule-set hashes separately from the producer compiler, and let
 `verify-bundle` run without repository state or network access. It must also
 ship Task R-0353's pinned proof-replay runtime/payload dependencies—or report
 `proof_replay=unavailable` and fail any release profile that requires offline
 kernel replay. Include every shipped Lean/ProofKit/runtime component in
 checksums, the supply-chain lock, and `THIRD_PARTY.md`.
### Task R-0355

**Objective:** Add release performance budgets:

 `scripts/tests/check_release_performance.sh` must measure compiler startup,
 cold small-project/stdlib builds, Phase 8.5 warm no-op, private-leaf and
 public-interface edits, proof-query/object-cache hits, relink latency,
 `concrete test`, audit/report generation, proof-check latency, and
 independent bundle-verification latency against
 `release/perf-baseline.json`. Done when
 release CI blocks unexplained regressions and prints the regressed command.
### Task R-0356

**Objective:** Add reproducible release artifact hashes:

 `concrete release --manifest --json` must record source tree hash,
 compiler commit/version, schema versions, target triple, build profile,
 dependency lock hash, stdlib hash, canonical query/artifact dependency root,
 Phase 14/15 checker and rule-set identities, typed-Core/BackendIR/codegen-unit
 roots, emitted object/binary identity, release-bundle evidence root, and
 replay command. V1 does not support old release manifests; schema
 mismatches fail loudly with a regeneration command. Wire
 `scripts/tests/check_reproducible_release_hash.sh`; the gate must build the
 same release artifact twice from a clean tree and compare all hashes.
### Task R-0357

**Objective:** Add a toolchain/dependency supply-chain lock and license inventory before any release claim:

 - The Lean toolchain (`lean-toolchain`), every Lake dependency
   (`lake-manifest.json` pinned revisions), the SMT solver binary
   (name + version + hash, per the SMT replay metadata), and clang/LLVM
   versions are part of the evidence chain — a claim is only as
   trustworthy as the unpinned link. Task R-0356's release manifest must
   record all of them; drift between the lock and the build environment
   fails the release gate, mirroring the Task R-0216
   `proofs/lean-deps.lock` recheck trigger.
 - Publish `docs/THIRD_PARTY.md`: every shipped dependency (Lean
   toolchain, Lake packages, solver, runtime libs linked into emitted
   binaries) with license and role; release claims are blocked while any
   shipped dependency's license is unreviewed or incompatible.
 - Red-team gate: `scripts/tests/check_supply_chain_lock.sh` must fail
   when (a) `lake-manifest.json` and the recorded release manifest
   disagree on any pinned revision, (b) a dependency is present in the
   build but absent from `THIRD_PARTY.md`, or (c) the solver version
   recorded in replay metadata differs from the one in the lock.
### Task R-0358

**Objective:** Add the language/stdlib/artifact deprecation policy (moved up from Phase 18 — users of a released language need the policy BEFORE the release, not after): what stability the first release promises for syntax, stdlib API, proof/fact schemas, and obligation ids; how deprecations are announced (diagnostic with a replacement, minimum deprecation window measured in releases); and what may never break silently (anything that would flip an evidence class without `needs_recheck`). The Phase 19 migration tooling (`concrete migrate`) implements this policy; the policy itself is a release-bar document. Gate: Task R-0340's `concrete api-diff` must classify every

 public-surface change as `compatible`, `deprecated(window)`, or
 `breaking`, and the release gate fails on `breaking` without a recorded
 policy exception.
### Task R-0359

**Objective:** Add the external-contributor and public-platform surface before release.

 Write `CONTRIBUTING.md` as an operational checklist, not generic etiquette:
 how to add a language feature, which compiler modules usually change, which
 gates to run, which docs must be updated, how to add positive/negative/
 adversarial fixtures, and how to update
 `scripts/tests/example_manifest.txt` in the same commit as any new
 `examples/` `.con` file (this broke CI twice on 2026-06-09/10). Add
 `scripts/tests/check_contributing_contract.sh`; the gate must prove every
 example project has a manifest entry, every roadmap-required gate is named
 in either `Makefile` or CI, and contributor docs mention source changes,
 tests, docs, roadmap/changelog, claims, and release-bundle impacts. Publish
 a public platform-support statement (`docs/PLATFORM_SUPPORT.md`) naming
 Tier 1/Tier 2/unsupported hosts and targets, including Linux/macOS
 x86_64/aarch64 and Windows status. Windows must not remain ambient: either
 it is a named supported target with CI and hosted-capability semantics, or
 it is explicitly "not v1" with a pull trigger (external adopter, workload,
 or release requirement) and a list of blocked surfaces: paths, process,
 env, console, file locking, line endings, and toolchain/LLVM availability.
 Add performance-claim discipline: public copy may not imply "fast systems
 language" unless `concrete bench` and release performance gates publish
 replayable numbers; otherwise docs must say performance claims are not made
 yet.
### Task R-0360

**Objective:** Ship the first narrow public release only after the above are green.

### Task R-0361

**Objective:** [relocated from closed Phase 4] Artifact and docs stability hardening:

 schema-version rejection gates (refuse to silently misread an artifact whose
 schema version differs from the compiler's), source-location privacy /
 redaction modes for emitted diagnostics and artifacts (historical Phase 4 item 38), and
 docs-drift SEMANTIC checks beyond the artifact-existence gate
 (`check_docs_drift.sh`) — e.g. `Status:`/`Verified:` metadata and stale-claim
 marker detection. (Found not mechanically robust as a default during historical item 44
 work; they belong here, gated for the release bar.)
### Task R-0362

**Objective:** Add the Phase 17 validation artifact:

 `scripts/tests/check_release_candidate.sh` installs the dist archive into a
 clean temp prefix on every supported host, runs `concrete --version --json`,
 builds one example, runs one proof/audit workflow, verifies checksums and
 signatures if enabled, checks `release/perf-baseline.json`, runs
 `concrete api-diff` against the previous public interface snapshot, and
 confirms the bundle contains the claim matrix, threat model, public
 examples policy, replay commands, schemas, assumptions/trust reports,
 Task R-0343's `SECURITY.md`, the supply-chain lock and `THIRD_PARTY.md` from
 Task R-0357, the deprecation policy and clean `api-diff` classification from
 Task R-0358, and
 the tutorial transcript from someone who did not build the compiler. It
 must also run Task R-0343's proof-revocation drill against the candidate
 bundle, verify the evidence DAG offline through Task R-0353 from the installed
 independent checker, and run tamper cases for source/Core/proof/BackendIR/
 object/binary/dependency-edge substitution. The report must distinguish
 Core/proof or SSA/BackendIR relation failures from object/binary failures
 against the expected root/authentication; native semantic edges remain
 explicitly backend-trusted. Release replay begins with a
 clean cache or `--incremental=off`; a local cache cannot be an undeclared
 release input.

## Phase 18: Packages And Dependency Evidence

Goal: let package users inspect proof, trust, capability, and assumption facts
before adopting a dependency.

Done when: packages have manifests, lockfiles, package-aware facts, trust
policies, provenance, independently checked interface/evidence receipts,
Phase 8.5-aware reuse/invalidation, and registry protocol.

Prerequisite: Phase 8.5 completed. If it was rejected/deferred, Phase 18 may
start only after an explicit roadmap amendment replaces these artifact/query
dependencies; package work may not grow a private cache/driver as a workaround.

### Task R-0363

**Objective:** Expand package artifacts only after reports, policies, assumptions, interface artifacts, and CI gates prove what packages must carry. The first package artifact refactor must define exact files:

`Concrete.package.json` (manifest summary), `Concrete.lock`,
`.concrete/interfaces/<module>.json`, `.concrete/facts/<module>.json`,
`.concrete/evidence/<module>.json`, `.concrete/certificates/<module>.json`,
optional `.concrete/verifier-inputs/<module>.car`, and
`.concrete/docs/<module>.json`.
Each artifact must name compiler version, schema version, package id,
source hash, interface/body hash, dependency/evidence root, Phase 14/15
checker/rule-set identities and receipts where applicable, authority budget,
assumptions, proof/evidence summaries, and replay commands. Add
`scripts/tests/check_package_artifacts.sh`; the gate must prove package
consumers read these artifacts rather than source-private side channels.
This phase is the main 20k+ LOC scale boundary: large Concrete programs need
package/workspace boundaries, import hygiene, visibility/API stability,
versioning, generated docs, and evidence summaries before the language can
claim to support multi-team codebases rather than only single-repo examples.
### Task R-0364

**Objective:** Design and parse package manifest.

### Task R-0365

**Objective:** Add version constraints, dependency resolution, and lockfile.

### Task R-0366

**Objective:** Add workspace and multi-package support.

### Task R-0367

**Objective:** Add package-aware test selection.

### Task R-0368

**Objective:** Split interface artifacts from body artifacts at package/workspace scale.

Promote the Phase 8.5 internal split and stable ids into versioned public
package artifacts; do not expose or rename the opaque internal cache format
as a package protocol.
Interface artifacts expose public names, types, capabilities, contracts,
allocation/effect summaries, deprecation/version facts, and evidence classes.
Body artifacts contain implementation fingerprints, private obligations,
proof links, emitted IR hashes, and private diagnostics. A dependent package
may compile against interface artifacts without seeing private bodies, but
audit/release bundles must still show when a public claim depends on a
private body proof, trusted boundary, or assumption.
### Task R-0369

**Objective:** Define the public package codecs and `PackageInterfaceCertificateV1` before consuming certificates.

 Canonically encode the package manifest, public interface/API payload,
 body/evidence root, optional verifier input, and certificate/receipt
 envelope. Keep `api_identity` separate from `implementation_evidence_root`:
 a private body edit may preserve exported names/types while invalidating
 capabilities/effects inferred from bodies, proof/trust/allocation/runtime
 summaries, mono/codegen facts, or other implementation evidence.

 `PackageInterfaceCertificateV1` checks package/schema/root binding and,
 where a canonical checked-Core/body verifier input is shipped, derives the
 supported exported facts from that hash-bound artifact plus Phase 14/15
 receipts. It may not validate a summary against a self-authored summary.
 When private verifier input is withheld, public API shape can remain
 structurally checked, but body-derived capability/trust/allocation/evidence
 summaries stay `compiler_validated` with the producer trusted unless backed
 by a separately replayable proof. The package report must expose that split.

 Gate canonical round trips, old schema, interface/body-root swapping,
 private-body edit with stable API identity but changed evidence root, and a
 coordinated tamper that changes interface summary + matching certificate
 while retaining the old body root. Wire
 `scripts/tests/check_package_interface_certificate_schema.sh`.
### Task R-0370

**Objective:** Add proof-aware package artifacts: facts, obligations, proof status, trusted assumptions, policy declarations, package-boundary evidence summaries.

Required statuses: `proved_by_lean`, `proved_by_kernel_decision`,
`solver_trusted`, `tested_by_oracle`, `enforced`, `assumed`, `trusted`,
`partial`, `stale`, `vacuous`, `missing`, and `ineligible`. The artifact
must record whether evidence is package-local, inherited from a dependency,
or trusted through a boundary.
### Task R-0371

**Objective:** Verify dependency interface certificates before consuming package facts or reusable artifacts.

 A content hash establishes identity, not semantic validity. Run Task R-0369's
 dedicated package-interface checker and, only over embedded canonical
 verifier inputs of the right type, the Phase 14 Core / applicable Phase 15
 translation checker. Cache the independent receipt by package root +
 checker/rule-set version and bind it into the importing project's Phase 8.5
 dependency graph. Invalid, stale, unsupported-schema, or unsupported-
 predicate certificates trigger a source rebuild when source and policy
 permit it, or a loud policy failure; they never silently become trusted
 facts. Source rebuild first produces `compiler_validated` facts and becomes
 independently checked only after the applicable checker reruns.

 Import authority/evidence constraints may consume independently checked
 interface facts or facts explicitly labeled `compiler_validated` with the
 producing compiler still trusted; they may not erase that distinction. Gate
 a valid package, tampered interface fact, certificate from another
 package/build, interface+certificate consistently changed against an old
 body root, stale private body with unchanged public API but changed evidence
 root, changed public interface, old checker version, withheld verifier input,
 unsupported predicate, proof revocation, and source-rebuild fallback. Wire
 `scripts/tests/check_package_certificates.sh`.
### Task R-0372

**Objective:** Add module/package authority budgets after package graphs are real, and make imports fact-checked boundaries. Imports do not grant capabilities; they declare and constrain facts about the imported interface. The first concrete fact class is authority: support source-level or manifest-level constraints such as `import std.parse requires(no File, no Network, no Unsafe)` and package-wide budgets such as `allowed = ["Alloc"]`. A dependency capability widening must be a build/audit diff and, when constrained, a build failure until explicitly accepted. The design must leave room for the same import mechanism to constrain allocation (`no Alloc` / bounded allocation), trust

(`no trusted`, `no extern`, `no Unsafe`), runtime-failure profile, platform
(`hosted` / `freestanding` / `posix`), arithmetic-site policy, determinism,
constant-time / secret-flow claims, and supply-chain facts such as source
verification or license.
This is the package/dependency analogue of Elm ports and Elm package
discipline, adapted to Concrete rather than web apps: code may import a
dependency's values, but it does not inherit ambient authority, hidden host
effects, or private assumptions. The imported interface artifact is the
boundary contract. Roc's platform/host separation is also a useful warning:
host authority must be explicit at the boundary, never a package-local
convenience that disappears from reports.
Security threat model: if a dependency is compromised or upgraded to do more
than it used to do, the importer must be able to fail closed. Examples:
`import json as j requires(no File, no Network, no Unsafe, no trusted)`;
`import hmac.compute requires(proved_by_lean, no Unsafe)`; package-wide
`allowed_caps = ["Alloc"]`; or `requires(freestanding, deterministic,
license = "MIT OR Apache-2.0")`. The checked fact set—read through item
Task R-0371's independently-checked-or-explicitly-trusted boundary—must include at
least:
public capability set, allocation authority/profile, `trusted`/extern/FFI/
`Unsafe` use, assumption set, evidence class floor, proof staleness/vacuity,
runtime-failure policy, arithmetic-site classification, hosted/freestanding/platform facts,
dependency provenance/source hash, license status, and supply-chain lock
identity. A change in any constrained fact is not a warning hidden in prose:
it is either a build failure or an explicit audit diff requiring acceptance.
Write
`research/packages-tooling/import-fact-constraints.md`, then add
`docs/IMPORT_FACT_CONSTRAINTS.md`,
`examples/package_authority_imports/{rejects_network_widening,accepts_file_without_granting_file,rejects_unsafe_dependency,rejects_alloc_widening,rejects_hosted_dependency,rejects_trusted_widening,rejects_evidence_downgrade,rejects_new_assumption,rejects_license_drift}/`,
and `scripts/tests/check_import_fact_constraints.sh`; the gate must prove
imports read summaries from interface artifacts, do not grant authority to the
importer, compose with package-level budgets, and reject capability/allocation/
trust/platform/evidence/assumption/supply-chain fact drift until explicitly
accepted.
The first report view should be deliberately small and agent-readable:
`concrete audit dependency <pkg>` or the package-audit bundle must show
`public capabilities`, `allocation`, `trusted/Unsafe/extern`, `assumptions`,
`minimum evidence`, `platform`, `license`, and `source hash` in a stable
table. This report is the thing an AI assistant, reviewer, or CI policy reads
before accepting a dependency update.
### Task R-0373

**Objective:** Add dependency trust policy: trust widening across boundaries, review and inheritance.

### Task R-0374

**Objective:** Add package-level assumption inheritance: dependency assumptions must be visible to dependents and release bundles.

### Task R-0375

**Objective:** Add package provenance and publishing model.

### Task R-0376

**Objective:** Add package registry server protocol and trust model.

### Task R-0377

**Objective:** Add API docs publishing for packages and stdlib:

 `concrete doc --format json`, `concrete doc --format html`,
 `docs/api/std/<version>/`, and package docs under
 `docs/api/packages/<name>/<version>/`. Generated docs must carry version,
 module path, capabilities, allocation behavior, evidence class,
 deprecation status, and source links where available. Published docs must
 be reproducible from the package artifact.
### Task R-0378

**Objective:** Add package documentation hosting/export format only after `concrete doc` and package artifacts are stable: static HTML, JSON docs, versioned stdlib docs, package docs, release-note links, and `scripts/tests/check_docs_publish.sh` to prove generated docs are reproducible.

### Task R-0379

**Objective:** Design evidence-typed imports before the package fact schema freezes. A dependent package should be able to demand an evidence floor at the import boundary, for example `import hmac.compute requires(proved_by_lean)` or a manifest-level equivalent for all imports from a dependency. The compiler must check the requirement against the dependency's interface/evidence artifacts and fail the import if the evidence is missing, stale, vacuous, downgraded, inherited only through an unaccepted assumption, or weaker than the importing package's policy. This is a package-boundary trust-chain feature, not a local proof feature: it must define how evidence classes are

 ordered or deliberately non-ordered, how `solver_trusted`/assumed/trusted
 dependencies are named, how proof revocation invalidates dependents, and
 how release bundles explain the imported requirement. Write
 `research/packages/evidence-typed-imports.md` before implementing the
 surface, and keep it consistent with import authority constraints from
 Task R-0372 (e.g. `import hmac.compute requires(proved_by_lean, no Unsafe)`).
 Then add `docs/EVIDENCE_TYPED_IMPORTS.md`,
 `examples/package_evidence_imports/{requires_lean,allows_solver_trusted,rejects_stale,rejects_vacuous}/`,
 and `scripts/tests/check_evidence_typed_imports.sh`; the gate must prove
 dependency evidence is read from package artifacts, not source-private side
 channels, and that an evidence downgrade breaks the importing package.
### Task R-0380

**Objective:** Add package-level SPARK-class assurance summaries once frame/dependency contracts exist. Interface artifacts should expose public contract facts, read/write/modifies summaries, dependency-flow summaries, ghost/spec assumptions, capability requirements, trusted boundaries, and evidence class per public function. Package consumers and agents must be able to ask "what may this dependency read, write, depend on, assume, trust, or prove?" without inspecting private bodies.

### Task R-0381

**Objective:** Extend content-addressing beyond proof fingerprints for package/evidence artifacts.


 Extend Phase 8.5's opaque internal artifact roots into stable, versioned
 **public package** encodings rather than implementing a second hashing/cache
 system. Reuse the canonical typed-Core, obligation/proof, BackendIR,
 certificate, and release-node encoders already required by Phases 9/14/15
 and Task R-0353. This item owns package interface/body/evidence/docs
 wrappers, public schema compatibility, and package-root composition—not the
 first encoding of common verifier inputs.

 The hard part is not hashing; it is deterministic canonical serialization.
 Before package hashes are authoritative, each package wrapper must define a
 stable, versioned, pointer-free canonical encoding: no map iteration order,
 temp paths, generated-name nondeterminism, host-dependent formatting, or
 schema-ambiguous fields. Borrow the Zig InternPool lesson here: artifacts
 that need stable identity must serialize from explicit IDs and normalized
 structure, not from incidental in-memory shape.

 Done when package interface/body/evidence/docs wrappers have versioned
 canonical encoders over the earlier common nodes; identical semantic package
 content hashes identically across repeated runs; a deliberately
 nondeterministic wrapper is caught by a gate; and package cache/dependency
 keys use these roots instead of source paths or human names.
### Task R-0382

**Objective:** Add optional shared/remote artifact reuse only after the local Phase 8.5 store and independent package verification are mature.

 Define an authenticated transport/protocol for content-addressed package,
 proof, and codegen artifacts; the server is an untrusted blob store, not a
 compiler authority. V1 permits only fetch-by-digest where that expected
 digest is already authorized by a lockfile, pinned release root, or
 policy-trusted builder attestation. An untrusted remote action-key-to-digest
 mapping is never an authority for semantic reuse. Every download must pass
 size/schema/digest and dependency-root checks, the applicable Phase 14/15
 certificate checker, target/profile/toolchain binding, and package policy
 before entering the local store.

 Remote object, link, and binary outputs may not enter a strict build merely
 because their hashes are internally consistent: the Phase 14/15 independent
 relation stops at BackendIR. Regenerate native outputs locally. A separately
 opt-in trusted-builder mode may accept signed native outputs only as
 `builder_attested` / `backend_trusted`; its signature authenticates producer
 provenance, not object semantics.
 Upload only deterministic canonical artifacts with explicit privacy/
 redaction policy; never upload source, diagnostics, proof attempts, or
 credentials implicitly. A signature authenticates provenance, not
 correctness.

 This item is optional for the first package release and must not complicate
 local builds. Gate hostile/corrupt server responses, replayed old roots,
 cross-target objects, revoked proofs, concurrent upload, offline fallback,
 and local recomputation parity. Include coordinated poisoning where a
 server rewrites the action mapping, artifact, manifest, and every internal
 hash consistently; it must still fail without an independently authorized
 expected digest. Record CI/team speedups, but keep correctness independent
 of network/cache availability.
### Task R-0383

**Objective:** Add the Phase 18 validation artifact: a multi-package workspace project with dependency resolution, lockfile, package-aware tests, interface/body artifact split, dependency trust policy, assumption inheritance, authority budgets, provenance, independently checked dependency certificates, evidence-typed imports, published docs, downstream reuse after a dependency private-body edit, invalidation after a public-interface edit, and release-bundle evidence for every dependency. Include remote-cache hostile input/offline fallback if Task R-0382 is implemented. Wire it as `examples/package_workspace/` plus

 `scripts/tests/check_phase18_packages.sh`.

## Phase 19: Editor And Human Tooling

Goal: make evidence visible where developers work.

Done when: editor/LSP/tooling exposes the same facts as CI and command-line
reports through the Phase 8.5 session/query graph, with bounded edit
invalidation and certificate freshness, without inventing a second truth source.

Prerequisite: Phase 8.5 completed. If it was rejected/deferred, Phase 19 needs a
recorded replacement architecture before starting; an editor-only fact database
or cache remains forbidden.

### Task R-0384

**Objective:** Add artifact viewer integration for proof/evidence facts.

### Task R-0385

**Objective:** Add compiler-as-service / LSP entrypoints after diagnostics and facts are structured. The service must host the Phase 8.5 `CompilerSession`; it may not wrap batch `runFrontend` as its normal edit path.

### Task R-0386

**Objective:** Reuse the incremental query/certificate graph as the only editor fact engine.

 LSP/editor requests use the same typed query keys, dependency edges, local
 store, invalidation rules, validation records, independently checked package
 receipts, and evidence views as CLI/CI. Tag each response with project
 revision, subject/dependency root, completeness (`strict | partial`),
 freshness, checker/rule-set version where applicable, and cache/query
 provenance kept separate from evidence status. Cancelled or tolerant
 partial computations cannot enter complete caches or answer proof/release
 queries.

 Gate a scripted edit sequence covering comment/span-only, private body,
 public signature/capability/contract, target/profile, policy, proof, and
 dependency-certificate changes. Assert the bounded Phase 8.5 query execution
 set as well as CLI/LSP fact equality; an editor-only database, stale hover,
 or batch whole-project rebuild for a private leaf edit fails the gate.
### Task R-0387

**Objective:** Add hover/type info for capability status, proof status, predictable status, assumptions, obligations, and trusted boundaries.

### Task R-0388

**Objective:** Add obligation navigation: jump from source contract/index/mod/loop to the generated obligation and discharging theorem.

### Task R-0389

**Objective:** Add refactor support that preserves or updates facts/proofs where possible.

### Task R-0390

**Objective:** Add dependency audit UI for capability, allocation, FFI, trust, evidence, predictability, proof-obligation drift.

The UI must expose the same boundary facts as Task R-0372, not a prose
summary: public capabilities, allocation authority, trusted/Unsafe/extern
use, assumptions, evidence floor, platform, license, and source hash. This
is explicitly for humans and AI agents reviewing imports: an editor hover or
command palette action should answer "what can this dependency do, what does
it assume, and what evidence does it carry?" without reading private bodies.
### Task R-0391

**Objective:** Add backwards-compatibility regression corpus once public users exist.

### Task R-0392

**Objective:** Add migration/deprecation tooling after the policy exists (Task R-0358):

`concrete migrate --check`, `concrete migrate --apply`, diagnostics for
deprecated syntax/APIs, suggested replacements, edition/version notes where
needed, and mechanical rewrites only for transformations that preserve
evidence facts or explicitly mark them `needs_recheck`. The tool must read
`concrete api-diff --json`, `concrete doc --format json`, and the final
obligation ledger rather than scraping text.
### Task R-0393

**Objective:** Add API-docs/editor integration: LSP go-to-docs for stdlib/package APIs, evidence/capability/deprecation badges in hover, and diagnostic links to `concrete doc` output. The LSP payload must reuse the same doc/evidence JSON as the CLI.

### Task R-0394

**Objective:** Add a playground or local web runner only after the release subset is stable: `concrete playground --local` or a static hosted runner with preloaded examples, no hidden claims, visible evidence class, audit output, and clear sandbox/timeout/resource assumptions. This is a teaching surface, not a second compiler pipeline.

### Task R-0395

**Objective:** [historical origin: closed Phase 4 item 11 tail] Route obligation / proof / policy facts into the structured `Diagnostic` record / `--diagnostics-json` channel so LSP/editor and CI-JSON consumers see them (array-bounds, solver-policy, vacuous-contract, stale-proof, …) — without duplicating the `ObligationCore`/report model. Includes interpreter structured diagnostics (historical Phase 4 item 18a). Pull when a real consumer (LSP / CI JSON parser) needs machine-readable obligation diagnostics; see LANGUAGE_GAPS for the frontend-vs-obligation diagnostic split.

### Task R-0396

**Objective:** Add editor and agent diagnostics for SPARK-class assurance facts after the facts exist: failed loop invariants, weak variants, missing frame facts, over-broad `writes`, unsatisfied `depends`, ghost/spec partiality, package evidence downgrades, and runtime-safety obligations that need a frame or invariant. The LSP/JSON payload must reuse the obligation/evidence ledger and point agents to the validation command; no editor-only proof status.

 Also surface development-only expectations from Task R-0240 as
 `dev_checked` / `tested` facts, never as proof. Editor and agent prompts
 should suggest "promote this to a contract/obligation" when release or
 high-integrity policy requires stronger evidence.
### Task R-0397

**Objective:** Add installed-binary feature discovery for agents and editor tooling.

  Deliverable: extend the Phase 6E `concrete help --json` / command catalog
  with `concrete agent features --json` only if agent-specific fields need a
  separate view. The catalog must describe supported
  commands, accepted inputs, emitted artifacts, schema versions, evidence
  classes, replay commands, policy gates, forbidden actions, and examples of
  valid next steps.

  Rule: this catalog is the source of truth that MCP, LSP, docs, and LLM
  prompts wrap; they must not duplicate a stale list of Concrete features.
  Done when golden JSON snapshots exist and one agent workflow discovers
  proof synthesis, replay, capability diffs, and counterexample saving from
  the installed binary alone.
### Task R-0398

**Objective:** Add the Phase 19 validation artifact:

`scripts/tests/check_phase19_editor.sh` runs a scripted LSP/editor session
or golden transcript over one real project, proving hover, diagnostics,
obligation navigation, proof/evidence facts, dependency audit UI, refactor
behavior, docs integration, deprecation diagnostics, and playground output
match CLI facts rather than inventing a second truth source. It also checks
revision/freshness tags, cancellation safety, minimal incremental
recomputation, independently checked dependency receipt changes, and
cache-off/on equivalence.

## Phase 20: Concurrency And Research-Gated Extensions

Goal: keep speculative ideas gated until Concrete's proof/evidence foundation
can contain them honestly.

Done when: each research idea is either pulled into an earlier phase by a
forcing example, explicitly deferred, or rejected.

### Task R-0399

**Objective:** Keep concurrency design-only until the v1 surface is frozen:

capability lattice, scopes, spawn/join, linear handles, bounded channels,
result flow, ownership transfer, rejected forms, and report schema. This
includes the memory-model question Concrete must not answer accidentally:
atomics, synchronization, shared mutable state, data-race freedom,
capability-gated thread authority, and proof/evidence classes for concurrent
code all remain research-gated until a formal model and pressure tests exist.
Async syntax remains rejected for v1 per `docs/ANTI_FEATURES.md`; the
positive research direction lives here and in `docs/EXECUTION_MODEL.md`:
explicit concurrency primitives, visible effects, linear handles, and
bounded scheduling/failure evidence rather than hidden async lowering or a
second control-flow semantics.
### Task R-0400

**Objective:** Build concurrency pressure-test sketches and expected reports before implementation.

### Task R-0401

**Objective:** Mechanize the v1 concurrency formal model before claiming safety.

### Task R-0402

**Objective:** Implement OS threads/scopes/channels only after the model and reports are stable.

### Task R-0403

**Objective:** Research typestate only if a current state-machine/protocol example needs it.

### Task R-0404

**Objective:** Research arena allocation after bounded-capacity/allocation-profile work exposes a concrete gap.

### Task R-0405

**Objective:** Research exact WCET/cache/pipeline behavior only with a target/hardware model.

### Task R-0406

**Objective:** Research binary-format DSLs only if packet/ELF examples show repeated parser boilerplate.

### Task R-0407

**Objective:** Research hardware capability mapping after source-level capabilities and package policies are stable.

### Task R-0408

**Objective:** Broaden the proof-relevant interpreter toward Miri-style UB checking only if the proof-subset interpreter proves valuable.

### Task R-0409

**Objective:** Investigate a sized/indexed ProofCore evaluator only if the current fuel-indexed evaluator remains repeated proof debt after HMAC and at least one other substantial loop/composition proof. This is ProofCore v2 research, not a migration commitment; see [research/proof-evidence/SIZED_EVALUATOR_INVESTIGATION.md](research/proof-evidence/SIZED_EVALUATOR_INVESTIGATION.md).

### Task R-0410

**Objective:** Research persistent equality/rewrite state after backend contracts, semantic diff, and proof/evidence pipeline are stronger.

### Task R-0411

**Objective:** Do not adopt row effects for v1. The default design stays object-capability and audit-visible: authority should be obvious in source, not hidden behind abstract effect inference. Revisit only as a research note if explicit capabilities create a proven, repeated blocker in real programs after the stdlib and concurrency pressure tests exist.

### Task R-0412

**Objective:** Research a generational-reference-style dynamic fallback only if a forcing workload proves static linearity plus second-class references are too restrictive. The target use case is not ordinary ownership; it is a narrow escape for liveness/provenance facts around observers, back-references, callback-heavy object graphs, or host/FFI handles that Concrete cannot prove statically without unacceptable surface complexity. If adopted, the runtime check discharges an obligation as `checked_dynamically`, never as `proved`, and audit/report output must distinguish it from static ownership proof.


 Do not pull this forward without a pressure test and a decision record.
 The research pass must compare Vale-style generational references and
 region/frozen-scope ideas, explicitly reject any vague "zero-cost safety"
 claim, and prove the fallback does not become a hidden borrow checker,
 hidden GC, implicit Drop, or a way to weaken the linear default.
### Task R-0413

**Objective:** Research dogfooding Concrete's evidence model onto the compiler's own TCB.

 This is the logical endpoint of "evidence-carrying artifact applied to the
 compiler": rewrite or mirror selected compiler passes/checkers in Concrete,
 attach contracts, and use Concrete's own linearity/capability/proof pipeline
 to reduce the trusted base of the toolchain itself. This is NOT a near-term
 migration commitment and must not block Phase 7-19 work; it is a research
 bet to keep visible so the project does not stop at verifying user programs
 while leaving the compiler as an opaque trust-me artifact. This rewrite-in-Concrete route is complementary to the Phase 14 verified-compiler endgame; the primary route there is proving the existing Lean-hosted passes semantics-preserving, not re-implementing them.

 This is distinct from the Phase 14/15 independent certificate checker. That
 checker re-derives narrow predicates over compiler artifacts in a small Lean
 target; this research item asks whether selected compiler passes should be
 implemented or mirrored in Concrete itself. Do not delay the independent
 checker while waiting for self-hosting evidence.

 First pressure-test only: choose one narrow pass or checker helper with a
 small state space (for example a value-flow checker slice, a type-policy
 predicate, a report fact transformer, or a simple SSA cleanup rule), specify
 its input/output contract, and compare the Concrete implementation against
 the Lean/compiler implementation on a generated corpus. Done when the
 decision record states whether the approach actually shrinks the TCB, merely
 duplicates it, or creates a second source of truth. Pull forward only if a
 real pass can be checked without weakening the no-second-truth-source rule.
### Task R-0414

**Objective:** Research a Datalog-style / stratified relational **rule layer** only when the shipped Phase 8.5 `CompilerDB` has real relational consumers that typed queries/maps cannot express cleanly.


 `CompilerDB` facts — pass-agreement edges, borrow conflicts, E0293
 container exclusions, provenance edges, dependency/invalidation, package
 evidence, and proof/evidence queries — are naturally relational. A
 Datalog-like or stratified-facts model is a good design reference for that
 layer: derived facts are explicit, dependency edges are queryable, cycles
 and invalidation have a discipline, and audit/report views can ask
 relational questions without reparsing prose.

 Phase 8.5 already owns the typed acyclic query/cache driver, scheduling, and
 invalidation; do not reopen or replace it here. What remains deferred is a
 general Datalog/stratified derivation layer or user-visible query language.
 The trigger is a real `CompilerDB` relation family whose rules are awkward
 as ordinary typed Lean functions/maps/joins. The first investigation should
 compare a small stratified internal rule layer against those ordinary typed
 functions, reuse Phase 8.5 dependency/certificate machinery, and keep the
 user language unchanged: no user-facing logic language, no implicit effects,
 and no hidden second truth source.

 Done when a design note demonstrates one concrete relation family
 (for example provenance invalidation or package evidence queries), includes
 an acyclicity/stratification story, shows replayable derived-fact output,
 and rejects a cyclic or stale derived fact. Until then, this remains a
 future rule-layer reference, not unfinished incremental-compiler work.
### Task R-0415

**Objective:** Add the Phase 20 validation artifact: one pressure-test sketch, expected report, and decision record for every research-gated extension (concurrency, atomics/memory model, typestate, arena allocation, WCET, binary-format DSLs, hardware capability mapping, Miri-style interpreter, sized evaluator, persistent rewrite state, row effects, generational dynamic fallback, compiler self-verification, and Datalog-style relational facts). No research item graduates unless its forcing example, report shape, evidence class, and rejection or pull-forward criteria are recorded.
