# Stage interfaces

The frozen declarations are [spec carriers](../../ConcreteSpec/StageInterface.lean),
[compiler carriers](../../Concrete/Interfaces/Carriers.lean),
[grammar](../../ConcreteSpec/StageGrammar.lean),
[behavior refinement](../../Concrete/Interfaces/Behavior.lean), [edge propositions](../../Concrete/Interfaces/Contracts.lean),
[reference elaboration](../../Concrete/Interfaces/SurfaceElaboration.lean),
[executable signatures and registry](../../Concrete/Interfaces/Pipeline.lean), and the
[backend observation adapter](../../Concrete/Backend/Observation.lean) and the
[backend runtime renderer](../../Concrete/Backend/Runtime.lean).
The root check compares their exact SHA-256 digests with the [recorded manifest](../../scripts/checks/lib/stage_interfaces.sha256). Changing a frozen declaration takes a new interface round. The canonical A-normal syntax also lives in the frozen inventory. The data modules can be imported by both a pass and its caller without importing the pass implementation.

## Source → Tokens

Fix (f) owns the lossless scanner. `Source` carries the admitted project, entry-file ID and active slice. `TokenStream` retains typed decoded tokens, including string bytes, byte literals and label names, plus each exact spelling and the bytes and spans of leading and EOF trivia. The spec reconstructs ordered, contiguous pieces from the retained bytes and checks their file and coordinates. `Lexes` compares the result with the whole entry file and establishes the parser-view invariant. It applies the proved lossless lexical reference to the reconstructed bytes and compares each decoded token and trivia boundary. Malformed input and resource limits have separate typed refusals. Migrate `Lex.lex` and its correctness proof to the retained stream and prove agreement on its parser view.

## Tokens → AST

The registry selects the grammar and verification inputs from the slice. The closed bootstrap grammar covers the standalone literal-return fixture. Its checked table has a fixed word program and proposal, with kernel proofs of acceptance and exact row identity. Fix (f) replaces the bootstrap data with the full shared grammar and its certificate-producing verifier. The AST is indexed by the start symbol. First-order actions construct the shared syntax tree from retained token payloads and coordinates. The verifier runs a closed word machine over proposal memory and certifies the exact dense prediction table used by the parser. Its word-operation and memory budgets use a fixed multiplier of grammar positions times the exact number of lookahead columns. The boundary validates file IDs, ranges and exact starting coordinates in one pass over project bytes and tokens. Invalid streams receive a typed refusal before grammar execution. The parser executes the frozen cursor machine with indexed lookups and first-order typed constructor actions. It counts visits and action constructors on the same execution that returns the result. A stage proof must show that execution completes within its linear token budget, including on refusal. The full declarative and predictive derivations check each production and token's slice eligibility, including radix and suffix. The unsupported refusal names the least slice admitting the full stream and the span that raised the requirement. The registered output and elaborator input share `Parsed.ElaborationDomain`. It requires a successful run of the fixed parser on a coordinate-valid stream and a defined reference elaboration result. Every successful handoff must satisfy `AdmissionInput.Ready` in the issuing project. This checks Core locations and arguments for a prepared program and the diagnostic location for a source refusal. Replace `Parse.parse`, the separate refusal grammar, token rewriting and handwritten body derivations.

## AST → Core → admitted Core

### ISS-0118, interface revision round 1

The registered elaborator returns an `AdmissionInput`. Its `.pending` case carries the parsed, located surface program and its project. Slice 0 elaboration can refuse an unsupported entry or wildcard. Its success theorem preserves the complete input, including unresolved names, suffixes and unreachable statements. `SliceElaboration` binds that program to the exact parser tree. Changing a literal while retaining its source coordinates fails the registered elaboration contract.

The checked and unchecked entries both call `AdmissionInput.prepare`. This spec function owns body reachability, binder availability, suffix checks, name resolution, and their precedence. It produces a located Core fragment or a typed source diagnostic. A source refusal must equal the result on the supplied program, including its rule and spans. Surface syntax and refusal causes are indexed by slice. A later slice can introduce its own source facts and rules. The unchecked route uses the single `.unpreparable` outcome for a failed preparation and checks independent static typing on successful preparation. The active Slice 0 binder test and its diagnostic reference `Slice0Rules.AvailableBinder`, a reference to the existing `Surface0.BinderLegal` rule at empty scope. ISS-0121 records the discrepancy with `UnreservedLocal` for the owner's token ballot.

The Core payload stores the canonical located syntax of an early straight-line fragment or a general integrated machine program. From the first constructor, `AdmissionView.program` supplies the shared checker syntax and `AdmissionView.execution` builds the executable body, declarations and origins. This avoids a reverse translation that would lose declaration spans. The view locates every statement position and the terminator, and records each named binding's declaration separately from its RHS. `Represents` requires equality to the exact payload. `decode_iff` and `AdmissionView.unique` establish that decoding succeeds exactly for this relation and that one input determines every position and diagnostic span. The fragment covers Slices 0 and 1. General machine payloads in these slices receive `.outsideFragment`; Slice 2 and later receive `.unsupportedSlice` until their CFG admission judgment is supplied. A later slice can judge the integrated program directly.

Fix (d) supplies the location and provenance rules. Semantic identities are spec `Var`, `PlaceId` and declaration IDs. An `Origin` contains a run-local operation ID, file-scoped lexical span and persistent evidence key. A file span resolves against exact bytes in the admitted project and agrees with the spec span judgment. A backend table index is a separate type, allocated only at BIR formation. R-0709 is represented by the project admission invariant.

Fix (c)'s `Admission.checkInput` runs the shared traversal on the decoded input. The dependent batch certifies acceptance and independent typing. A Core refusal requires non-acceptance, the exact typed result of that traversal and its canonical rendering. For example, an out-of-range Slice 0 return uses `LiteralTyped` at the return operand. An unused or live linear local has the terminator as primary and its declaration as a related span. Structural decoding, missing-location and evidence-encoding failures have explicit execution outcomes. Only justified language diagnostics satisfy the registered refusal contract on its domain.

The batch encoder validates an ordered subset of issued discharges, including the empty subset, and maps each statement to its operation, stable key and ordered safety operands. `Admission.DischargeTransfer` is the remaining (c)/(e) proof that the issued straight-line safety fact holds at every matching reachable integrated machine state. At cutover (c) must prove that all well-formed fragment inputs render and encode, consume discharges once at Core → MIR, and retire the pre-cutover compiler token path. Fix (e) extends the integrated instance and proves its execution correspondence. Fix (a) must validate and select the actual checked and unchecked production pass bundles for each backend. The registered literal-return control already runs `Admission.checkInput` on the checked route.

## Admitted Core → MIR

Fix (c) owns evidence validation and consumption and the empty-evidence entry. Fix (b) owns the typed MIR instruction semantics. Fix (d) owns operation origin transport. `MIR` carries typed places, copy/move reads and explicit lifetime and control transfers. `LowersCore` ties removed checks to exact validated discharges and checks every emitted occurrence against the source condition inventory, indexed operator or exact callee, and ordered typed operand identities. Core calls and primitives take atomic typed reads that cannot trap on operand evaluation. A checked argument computation runs in its own Core operation before the call, with its own origin and result local. MIR checks its discharge at that operation and the call reads the result local. A residual condition is part of the same MIR instruction as the operation it guards and traps at its origin on failure. CFG edges enter blocks, so they cannot enter an instruction after its checks. Each supplied discharge matches exactly one emitted occurrence. An unconditional abort cannot supply a discharge condition. `LowersUnchecked` uses the same carrier and checks with no admission discharges. The registered typing stage checks the unchanged Core input against the independent static typing judgment before passing it to unchecked lowering. Typing rejects out-of-range literals and does not require the absence of defined execution traps. Both preserve origins and compare the fixed Core and MIR executions on the exact input and emitted graph. Retained owners keep their fresh Core binding number as their MIR root number within the defining function. Both lowering contracts match each declared root’s mutability to its Core descriptor and require a source binding for every mutable root. Synthetic MIR roots use fresh numbers and are immutable. Block control boundaries carry origins for returns and other operations that emit no instruction. The boundary metadata cannot satisfy a required MIR check or replace a trap terminator. Replace the linear `MIR.Program` and its token-bearing operators.

## MIR → Memory

Fix (b) owns the CFG substrate and direct lowering to `Memory`. Both graphs use the same
block and terminator families and check the types of block arguments. The integrated spec
determines layouts, literal and operation values, field projections and aggregate codecs.
Core literal operands have a profile-independent byte encoding. MIR/Memory operand reads call `CoreLanguage.literal`. Materialized scalar storage reads use the selected target's `Hybrid.decode`. For the Core bytes `fe ff`, an i16 literal is −2 under either byte order. Reading the same two storage bytes as big-endian i16 yields −257.

Core execution, MIR, Memory, SSA and BIR carry the same qualified declaration environment. Independent typing establishes the spec's `DeclsOk`; each lowering contract preserves the exact declarations. Layout, active fields and operation semantics consult that environment. A declared empty linear nominal has size zero and alignment one, the identity for maximum field alignment under foundation C3. It has a logical value and ownership lifetime even without a backing block. Materialized unknown or unsupported types produce `MissingLayout`. Fix (b) can use the declaration at the normal layout, construction, transfer and destructure entry points when proving the MIR-to-Memory simulation and promotion for eligible roots. The controls execute construction and consuming destructure, then try to read the consumed value.

Each reachable
projection must execute the instruction fetched at its program counter. Calls push the
caller’s resume point, destination and suspended locals. Return evaluates the callee operand,
ends its frame’s lifetimes and writes the result in the restored caller. Hosted observations
use the fetched return operand or trap kind and origin. The trace records spec operation
effects and the fetched trap’s diagnostic bytes.

The logical MIR evaluator holds roots as logical values. Both IRs retain residual operation
and call checks and evaluate them before performing the instruction. For a safe MIR input, the simulation
relates reachable activations, including suspended callers, to virtual values or live
backing ranges and initialized bytes. Target steps and terminal matches extend a related source execution, checking
representation at each intermediate source state. The source path can follow the
allocation decisions already made by the target. A target step that makes no
source progress decreases a natural-valued rank. Complete behavior refinement
independently requires a matching source execution for any target divergence.

Address operands name typed places explicitly. Physical execution resolves a live,
initialized materialized place through its recorded range and passes that range, byte
memory and ownership state to the spec operation transition. Both executions assign the
same fresh logical provenance on first address use during a lifetime and keep it across
calls. MIR uses the logical provenance and field path. The Memory simulation matches
abstract ownership and checks stored-value correspondence using the live backing ranges
of all related activations. Operations receive escaped logical values by provenance and
apply indirect writes to the owning activation, including suspended callers. Physical
operations use byte memory for nonempty roots. A zero-byte address carries its provenance
and field path without requiring a byte block. Zero-byte roots keep a logical value for
direct and indirect reads and writes. Each activation tracks the moved subtrees
of its initialized roots. A projected move leaves disjoint fields available and
blocks reads of the moved subtree or an ancestor containing it. An exact field
write can fill a moved subtree or replace its parent once the remaining active fields can be discarded. Coverage compares only present fields of the logical variant. Ending a partially moved root checks that all
remaining active fields can be discarded and that every loan of the root has ended. A direct owner write requires the same whole-root loan exclusion. A direct Copy read permits shared loans and excludes mutable loans. MIR and Memory use the same masks,
and the representation checkpoint compares initialized subtrees across frames.
Before any slice introduces borrows, close ISS-0116. Establish each owner's
spec ownership slot at binding with its declared mutability and prove in Lean
that every shared-interpreter step preserves equality with, or refinement of,
the `Own` spec state. The required control reaches the first mutable borrow
through normal execution and checks that it succeeds for a mutable `u8` owner
and is refused for an otherwise identical immutable owner. Slice 0 has no
borrows.

Replace the erase-to-MIR `Memory.lower` and `Memory.run` proof path.

## Memory → SSA

Fix (b) supplies the addressable/virtual partition and Fix (a) registers the validated SSA lowering. `SSA` has typed value IDs and parallel block arguments. Addressable places and layout remain explicit. SSA promotion records the eliminated virtual binding origins at control boundaries. `BuildsSSA` requires preservation of origins, layouts and observations against the
exact Memory and SSA semantics. A linear projected load consumes that field in the shared interpreter for SSA and BIR. A destructure reads a whole materialized or promoted root, copying Copy aggregates
and moving linear aggregates. It simultaneously defines the typed field registers and requires
each active linear field to have a distinct binding. Split the SSA construction
currently inside `SSA.lower` from its BIR output. The registered MIR, Memory and SSA handoffs require an entry in the function table and project-bound origins. Memory table validity excludes invalid terminators.

MIR, Memory, SSA and BIR carry mutable root declarations. The shared
interpreter uses them, or the existing owner slot's checked descriptor when
an identity already has a slot. The interpreter projects its selected field
into the owner slot used for permission while retaining the original root
for loan exclusion. Copy reads, consuming reads, writes and lifetime ends
use the corresponding `Own` decisions. Storage initialization and field
liveness distinguish a fully moved destination from a partially moved destination
with surviving fields. Replacing a live remainder requires mutable owner access
and proof that its remaining fields can be discarded. The registered handoffs
preserve the declared mutable roots from MIR through BIR. The SSA and BIR
views also allow Copy replacement in their own synthetic register slots so
loop iterations can define a register again.

## SSA → BIR

Fix (a) owns the validated handoff and profile slot check. BIR formation preserves the decoded entry graph and refines the linked execution from that entry. Both stages run the same interpreter on their decoded graphs. Unreachable helper bodies can differ. Fix (d) owns the trap table and its origin proof. `BIR` indexes traps through `TrapIndex`; `BuildsBIR` requires valid lookup, origin preservation, behavior refinement against the graph’s execution rules and the resolved profile's BIR slot ceiling. The count is the largest function's number of BIR definitions, including block parameters,
destructure bindings and call destinations. The shared SSA/BIR interpreter reserves
register storage above the physical place IDs declared or named by lifetime instructions in each
function, preserving the
identity passed to spec address operations. An unresolved trap slot has neither a
successor nor a terminal result, including inside a callee. The resolved platform supplies the ISA, byte order and spec memory layout for either backend. Dominance-scoped validation can strengthen the invariant on the same graph before slice 3. Retire SSA's positional site creation and BIR's duplicate diagnostic identity and scalar vocabulary at cutover.

The handoff preserves the exact project used to resolve source locations and
produce runtime diagnostics, including sites in unreachable blocks. Both backend entries require a duplicate-free BIR function table, a present entry function, project-bound trap sites and well-formed tables in the decoded Memory view.

## BIR → LLVM

Fix (a) registers the exact emitter and the named LLVM premise. `LLVMInput` carries a target triple, data layout, structured LLVM items and the exact trap table. `LLVMMetadataMatches` checks the target fields and trap table. The trusted-base entry
`P_print` covers the meaning of the exact emitted LLVM module. The renderer serializes the checked header and the structured body into the actual toolchain bytes. The legacy printer inventory and inverse claim retire in the A11 cutover. Full-image stack, toolchain and launch requirements stay in the theorem's native artifact boundary.

## BIR → LCB

Fix (a) registers the guarded adapter and profile preparation. `LCBInput`
carries the hosted body, auxiliary jobs keyed by BIR function identity, runtime
bytes and the exact trap table. The verified handoff validates the emitted
jobs, checks symbol uniqueness, and runs the entry job in the pinned LCB
semantics. A job-admission refusal identifies a validation error on the exact candidate produced by the emitter. The candidate must still satisfy the encoding contract. The runtime projection uses the emitted diagnostic table to
distinguish returning from trapping. The LCB return word reserves codes 0
through 255 for normal exits. The LCB adapter refuses more than
`2^32 - 256` trap entries before it invokes the emitter. This capacity
belongs to the hosted ABI at the LCB edge. BIR table validity is independent
of that limit. The guard also rejects calls in the entry
graph before invoking the emitter when their blocks are reachable by following
CFG edges from the entry block. Multi-job execution requires
a pinned linked semantics and a proof of behavior preservation. The
[issue ledger](../../ISSUES.tsv) tracks that work under ISS-0106.

## Correctness notion

An execution starts in a program's designated initial configuration and follows
small steps. A program is safe when no reachable state lacks both a successor
and a terminal result. A terminal result records return, trap or startup
refusal, with exact stderr and diagnostic sites where applicable. A language
trap is a defined outcome. Interpreter failure to take a step or report a
terminal result is going wrong.

`BehaviorRefinement` states safe-source backward inclusion directly. If the
source is safe, every target termination has a reachable source termination
with the same result and finite trace, every infinite target path has an
infinite source path with the same visible trace, and the target has no
going-wrong behavior. The source can have additional results or infinite paths.
A target can consequently make an early allocation choice or select the
terminating path of a source that can also diverge.

This is the conclusion of CompCert's
[`backward_simulation_same_safe_behavior`](https://compcert.org/doc/html/compcert.common.Behaviors.html).
Equality of finite trace prefixes on infinite paths accounts for both silent
divergence and infinitely many events without constraining the timing of
internal steps. Refinement is stated on the behaviors directly, so it requires
neither an evaluator fuel bound nor a forward-to-backward simulation theorem.
Evaluator fuel remains an adequacy obligation.

Correctness is whole-program observable behavior from the designated entry.
The CFG interpreters resolve calls through the full function environment.
Changing a helper that is unreachable from the entry is unobservable.
Changing a reachable helper can change the program's behavior and must be
rejected at the edge that introduced the change.

Core, MIR, Memory, SSA, BIR and the verified LCB job use repository-defined
execution on the exact carriers. Core → MIR runs the emitted graph against
the integrated Core execution. The pinned LCB semantics executes individual
jobs, including hosted traps through the runtime projection. Programs that
need calls across the job boundary receive a typed refusal before emission.

The registry binds each proposition to the function it executes. Its stage invariants and refusal rules come from repository definitions. Registry construction selects the slice-indexed grammar, verification inputs, Core semantics and lowering language. `Stage.Sound` selects a registered edge. `Pipeline.endToEnd` composes the registered chain selected by the profile and checked or unchecked mode. It takes executable pass implementations and validation evidence for that chain. The compiler gate follows the definitions in the theorem statement and rejects a planted quantified model. Lowering from a well-formed input is total through SSA. The pre-BIR frame budget is measured on SSA register definitions, and emitter refusals must match the backend profile or the LCB linking guard.
`Pipeline.endToEnd` takes the validated pipeline and its trusted obligations as separate hypotheses.
The guarded `registeredLCBEndToEnd` theorem takes only validation, readiness and a successful run.
`Stage.Sound` includes the named behavior-preservation obligations on the
exact successful invocations, separately from structural checks and refusals.
The trusted base for the frozen interfaces contains the following obligations.

| Premise | Scope |
| --- | --- |
| `P_print` | Behaviors of the exact rendered LLVM module bytes under the fixed opaque `llvmBehaviors` interpretation refine the originating BIR program. |

The revised D5 asks for a reachable whole-program witness and a rejected
wrong-compiler mutant on each verified edge. Existing edge proofs count toward
that requirement. Trusted edges are exempt. Any control on the LLVM behavior
claim is necessarily conditional on `P_print` and the opaque `llvmBehaviors`
interpretation. Native-image and launch assumptions remain at the production
theorem boundary.

## Writer ownership

Use separate implementation modules so parallel proof and implementation work can proceed against the frozen signatures. These paths reserve ownership, including their eventual in-place retirements. Existing cross-cutting gates and root imports are integrated by (a) after the implementation branches are ready.

| Fix | Owned implementation paths and retirements |
| --- | --- |
| (e) | `Concrete/IntegratedCore.lean`, `Concrete/Core.lean`, `Concrete/Elab.lean`, `ConcreteSpec/IntegratedCore.lean`, `ConcreteSpec/Spec/Slice0.lean`, `ConcreteSpec/Spec/Slice1Syntax.lean`, `ConcreteSpec/Spec/Slice1Semantics.lean`, `ConcreteSpec/Spec/Scalar.lean`, `ConcreteSpec/Proofs/Slice0.lean`, `ConcreteSpec/Proofs/Slice1.lean`, `ConcreteSpec/Proofs/Scalar.lean`, `scripts/checks/compiler/f31_census.sh` |
| (d) | `Concrete/Origins.lean`, `ConcreteSpec/Spec/Ident.lean`, `ConcreteSpec/Proofs/Ident.lean` |
| (c) | `ConcreteSpec/Admission.lean`, `Concrete/Admission.lean`, `Concrete/LowerAdmitted.lean`, `ConcreteSpec/Spec/Slice1Checker.lean`, `ConcreteSpec/Spec/Slice1Ownership.lean`, `ConcreteSpec/Proofs/Checker.lean` |
| (b) | `Concrete/CFG.lean`, `Concrete/MIR.lean`, `Concrete/Memory.lean`, `Concrete/SSA.lean`, `ConcreteSpec/Layout.lean`, `ConcreteSpec/Spec/Memory.lean`, `ConcreteSpec/Spec/Hybrid.lean`, `ConcreteSpec/Proofs/Memory.lean`, `ConcreteSpec/Proofs/Hybrid.lean` |
| (a) | `Concrete/BIR.lean`, `Concrete/BIR/`, `Concrete/Stages.lean`, `Concrete/Contracts.lean`, `Concrete/Theorem.lean`, `Concrete/Certificate.lean`, `Concrete/Premises.lean`, `Main.lean`, `Concrete/Backend/` except `Observation.lean` and `Runtime.lean`, root imports, proof inventories and cross-stage checks and fixtures |
| (f) | `Concrete/Lex.lean`, `Concrete/Parse.lean`, `Concrete/LL1.lean`, `ConcreteSpec/Spec/LL1.lean`, `ConcreteSpec/Spec/Surface0.lean`, `ConcreteSpec/Surface1.lean`, `ConcreteSpec/Proofs/Surface0.lean`, `ConcreteSpec/Proofs/Surface1.lean`, `ConcreteSpec/Proofs/LL1.lean` |

Fix (a)'s production cutover owns the update to the provisional trusted-base
registry and the removal of test-local model scaffolding after its remaining
controls exercise the registered contracts directly.

No end-to-end correctness claim is made for the compiler before fix (a)
proves that the production pass bundle satisfies the registered validation
premise and applies the registered end-to-end theorems to that bundle. The
cutover requires kernel-checked proofs using only standard axioms, recorded
in the axiom inventory, for both checked and unchecked chains. Validation
covers each stage's full input domain, including intermediate inputs that the
current source grammar cannot produce.

- **(f) and (e), parser and elaborator.** For each registered slice, a
  successful parse must supply a tree in the fixed elaboration domain.
  The grammar and parser must establish this domain, and production
  elaboration must respect it and retain project-bound locations. The
  counterexamples were a non-tree start value, a malformed surface tree
  and an out-of-bounds leaf span on an otherwise decodable return.
- **(a), unchecked typing.** Prove that the registered typing handoff
  refuses an ill-typed Core input such as a return of `256` at type
  `u8` and passes well-typed Core unchanged, including programs whose
  execution traps.
- **(b), lowering and SSA.** Establish the registered handoff invariants
  through SSA, including a designated entry, project-bound origins
  and exclusion of invalid terminators. A malformed terminator cannot
  enter BIR formation through the validated SSA handoff. The shared
  execution must also enforce whole-root loan exclusion for owner
  writes and for owner copies under a mutable loan.
- **(a), BIR formation and LCB emission.** Accept well-formed SSA within
  the frame-slot budget, including a one-block self-loop with zero frame
  slots, and preserve its entry graph. Establish unique BIR function IDs,
  the presence of the designated entry, valid trap slots and
  project-bound trap-site origins. The LCB emitter must establish the
  registered encoding and job-validation obligations for that domain.
  Refusals must satisfy the precise resource, profile, linking or encoded
  job-validation predicates of their respective stages.

The permanent compiler gate checks `Pipeline.Corresponds` for `return 42`
on both registered LCB routes and observes exit 42 with empty stderr in
the fixed LCB semantics. The checked route uses the spec checker and
refuses `return 256` for `u8`. Boundary controls cover the zero-slot
self-loop, the first oversized trap table and malformed syntax. A planted
lowering refusal produces no artifact, and an elaborator that changes the
literal to `43` fails validation.

When an atomic retirement spans owners, each writer prepares the replacement for its own files. The integration writer applies the coordinated deletion and updates the trusted-base inventories and cross-stage fixtures with the cutover. Frozen paths and the digest manifest belong to the interface round. Fixes (e) and (f) provide production definitions in their owned modules before the integration writer replaces the bootstrap instance bodies at cutover.

ISS-0113 tracks D5 controls on the fixed slice
instances for checked and unchecked Core lowering, Memory lowering, SSA, BIR
and LCB. Each edge needs a reachable program and a rejected substitution at the
registered boundary.


ISS-0114 tracks indexed type-family
inspection in the semantic-parameter audit. ISS-0115 tracks aligning
intermediate slice domains with production backend coverage before cutover.
