# Roadmap Language Scope (archived, pending absorption into the specification)

Status: archive — **pending absorption into the language specification, `docs/language/SPEC.md`** (being written). These
sections stated language and standard-library design rules inside ROADMAP.md. A roadmap answers only
*what happens next, in what order*, so they moved here verbatim on 2026-10-28 (only heading levels and
relative links were adjusted). Until the specification absorbs them, each keeps the authority it had in
the roadmap; once a section is absorbed, it is replaced here by a link to its specification entry.
Nothing here schedules work, and nothing here answers an [owner decision](../../ROADMAP.md#owner-decisions)
(OD1 – OD22): where a section and an open owner decision overlap, the owner decision governs.

## From "Product north star"

### Language baseline and bounded verification milestone

**Direction ratified 2026-09-15; implementation and verification remain incomplete.**
Concrete's systems-language shape is provisionally settled. The next phase implements
and validates that shape, while completing the contract semantics needed for one useful,
compositional verification subset.

Linear ownership, scoped references, explicit capabilities and cleanup, exhaustive
`match` plus explicit error returns, static or explicitly indirect dispatch,
monomorphization, no GC and no unwinding are the baseline. This is a planning constraint,
not a claim of soundness or a public compatibility freeze. Safety repairs, consistent
diagnostics and library API corrections remain admissible. New ordinary-language syntax
or abstractions require a concrete workload demonstrating why the existing model is
insufficient, plus a stated semantic rule and verification boundary. Postfix `?` remains
excluded. Broader language expressiveness is not the next milestone.

The remaining design work has existing owners:

| area | owner | decision or completion required |
|---|---|---|
| Safe library abstractions | R-0483, with R-0485 for review clarity | Make views, cursors, text and resource APIs preserve the existing ownership rules; distinguish reusable coordinates from access tied to an encapsulated owner, and preserve validated content against mutation. This is primarily library/API work, not a presumption that a new lifetime system is needed. |
| Authority and effects | R-0484 | **Decided 2026-09-29/30:** a header's `with(...)` is the complete list of the function's external authority. Handles carry their capability in their type (`Writer<C>`) and using one requires `with(C)`; `trusted` absorbs only `Unsafe`. Implemented; closed 2026-10-08 (see the task). |
| Compositional contract semantics | R-0473/R-0474/R-0477, the proof-authoring area and the existing VC bridge tasks | Complete typed contracts, binding/substitution, call composition, narrow mutation/frame semantics and checked totality. This is the largest remaining language-design area; R-0486 supplies the forcing workload. |
| Tuples and destructuring | R-0515 coordinates the decision record | Before language-surface freeze, document whether named records remain the supported product type or a workload justifies tuples. Include a multi-value-return example, ownership/cleanup implications, and acceptance or rejection examples. A deliberate deferral with a reopening trigger closes the decision; implementation is not presumed. |

**First bounded verification milestone — planned, not shipped:** one sequential
parser/state-machine component, drawn from the already selected flagships, with contracts
over scalars, records and fixed arrays. R-0486 owns the workload; the existing typed-contract,
two-state, totality and VC tasks own the semantics. Graduation requires:

1. A stated admitted subset with bounded iteration and explicit failure behavior;
   contract-callable helpers have a checked logical interpretation and required totality.
   An empty `with(...)` is not sufficient for that: it still permits mutation through
   `&mut` parameters, so contract evaluation needs its own purity and admissibility rule
   (recorded 2026-09-30 by R-0484's design, `HANDLE_CAPABILITIES.md` §1).
2. Calls establish preconditions and use exported postconditions without callee inlining;
   `old` and frame/`modifies` conditions describe the admitted record/array mutations.
3. Bounds, arithmetic traps, assertions and call obligations have explicit evidence or
   gaps. Unsupported constructs refuse or are classified visibly; runtime checks and
   trusted FFI/backend boundaries do not become proofs by association.
4. A caller consumes the component's interface without inspecting private bodies;
   contract-preserving implementation changes preserve caller evidence after the required
   implementation recheck, while contract/authority/trust changes produce precise diffs.
5. The migrated component has functioning safety regression controls and measured proof
   effort, review friction and runtime costs. Its results determine subsequent feature work.
6. A non-author changes the component or one dependency, sees which guarantees the
   change affects, repairs the affected obligations, and independently checks the
   resulting evidence within the admitted scope. R-0486 retains the before/change/repair
   artifacts; R-0150 measures effort from the first baseline, and R-0182/R-0183 supply
   the initial authority/assumption/contract-evidence diff. Compiler-author intervention
   is recorded as an unresolved usability limitation, not counted as independent success.

Every feature contributing to this milestone includes actionable failure feedback in
its exit: source location, relevant call/dependency path, unmet permission or obligation,
and a valid next action (or an explicit unsupported boundary). R-0137/R-0466 own the
diagnostic criteria; consumers reuse the same facts rather than inventing new verdicts.

General heap proofs, concurrency, relational verification and general resource-bound proofs
are not prerequisites for this milestone. Their existing research and later-phase owners
remain in place. Fixed-capacity enforcement and resource reporting must state their actual
evidence strength. This milestone narrows the first useful verification target; it does not
waive the current queue's evidence-integrity prerequisites or the broader release gates.

### Design-review priorities (ratified 2026-09-15)

R-0483 (sound, usable zero-copy parsing) closed 2026-10-09 with owner-bound views, and
R-0484's authority semantics closed 2026-10-08; both are in the changelog. R-0485 improves review clarity within the existing
error model. R-0486 carries the same workload into the typed-contract and external-user
rows above; it is not an additional public flagship or a parallel execution queue.
The ByteCursor defect and Text/ByteView lifetime, identity and content-validity
classification are recorded under R-0483 with ten retained fixtures. The additive-change
experiment and raw-constructor inspection are also recorded there. The remaining
questions concern safe migration of the affected attestation references and the actual
proof/evidence path for the Writer helper: its reporting and eligibility misclassification
is now measured under R-0484. Repairs and target guarantees are not yet shipped.

- **Keep explicit error propagation.** Exhaustive `match` plus explicit `return`
  remains the model. Postfix `?` stays rejected; this review does not reopen it.
  Improve diagnostics, `defer` consistency and library APIs within that constraint.
- **Prioritize semantic correspondence and composition over backend breadth.**
  R-0473/R-0474/R-0477 own typed contracts, resolved lexical identities, capture-safe
  substitution and call composition. R-0450/R-0454–R-0456/R-0460 own the single typed
  obligation pipeline and H19 bridge work. Complete a useful fragment with explicit
  trap semantics, call preconditions and unsupported coverage before expanding
  proof backends. Agreement among kernels does not establish source correspondence.
- **Double down on the compiler as an audit tool.** Authority paths, resource and
  failure summaries, named trust boundaries, contract-preserving change detection,
  and independently degradable evidence remain the product. Existing evidence schema
  owners supply these facts; new renderers must not invent another source of truth.
- **Retain the restricted systems core and evaluate its costs on workloads.** No GC,
  no unwinding, explicit indirect calls, scoped references, monomorphization and
  explicit cleanup remain the baseline. No closures or macros are proposed here.
  Explain those exclusions through semantic complexity, allocation visibility and
  analysis cost: finite callable enumeration alone does not require either ban.
  Evaluate explicit function-plus-context callbacks on real composition patterns;
  record awkward workarounds before considering any separately justified change.

## From "Cross-Cutting Decisions"

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
The stdlib area allocator decision is two-tier: tier-1 defaults remain simple,
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

> Moved to [ROADMAP_HISTORY.md](ROADMAP_HISTORY.md#compiler-pipeline-spine) (historical record; nothing in it schedules work).

## From "Cross-Cutting Decisions" (continued)

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

> Moved to [ROADMAP_HISTORY.md](ROADMAP_HISTORY.md#native-debug-info) (historical record; nothing in it schedules work).

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

## From the Architecture Decision Index (continued)

The index's backend-separation and compiler-architecture decisions are in
[ROADMAP_DIRECTION.md](../project/ROADMAP_DIRECTION.md#architecture-decision-index-backend-and-compiler-architecture).

### Slice Decision
- **Slices as the safe buffer interface:** `&Slice<u8>`/`&mut MutSlice<u8>` on
   hosted read/write; raw-pointer forms private) — the `&Bytes` net-I/O pull
   (currently 2nd ask).


The corresponding local/CI gate work is scheduled as a stable-ID task
below; this index carries no executable work.
