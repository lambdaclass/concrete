# Stage interfaces

The frozen declarations are [spec carriers](../../ConcreteSpec/StageInterface.lean),
[compiler carriers](../../Concrete/Interfaces/Carriers.lean),
[grammar](../../ConcreteSpec/StageGrammar.lean),
[behavior refinement](../../Concrete/Interfaces/Behavior.lean), [edge propositions](../../Concrete/Interfaces/Contracts.lean),
[reference elaboration](../../Concrete/Interfaces/SurfaceElaboration.lean),
[executable signatures and registry](../../Concrete/Interfaces/Pipeline.lean), and the
[backend observation adapter](../../Concrete/Backend/Observation.lean) and the
[backend runtime renderer](../../Concrete/Backend/Runtime.lean).
The root check compares their exact SHA-256 digests with the [recorded manifest](../../scripts/checks/lib/stage_interfaces.sha256). Changing a frozen declaration takes a new interface round. The data modules can be imported by both a pass and its caller without importing the pass implementation.

## Source → Tokens

Fix (f) owns the lossless scanner. `Source` carries the admitted project, entry-file ID and active slice. `TokenStream` retains typed decoded tokens, including string bytes, byte literals and label names, plus each exact spelling and the bytes and spans of leading and EOF trivia. The spec reconstructs ordered, contiguous pieces from the retained bytes and checks their file and coordinates. `Lexes` compares the result with the whole entry file and establishes the parser-view invariant. Malformed input and resource limits have separate typed refusals. Migrate `Lex.lex` and its correctness proof to the retained stream and prove agreement on its parser view.

## Tokens → AST

Fix (f) supplies the grammar, table-verification program and correspondence proofs. The AST is indexed by the start symbol. The verifier runs a closed word machine over proposal memory and certifies the exact dense prediction table used by the parser. Its word-operation and memory budgets use a fixed multiplier of grammar positions times the exact number of lookahead columns. The boundary validates file IDs, ranges and exact starting coordinates in one pass over project bytes and tokens. Invalid streams receive a typed refusal before grammar execution. The parser executes the frozen cursor machine with indexed lookups and first-order typed constructor actions. It counts visits and action constructors on the same execution that returns the result. A stage proof must show that execution completes within its linear token budget, including on refusal. The full declarative and predictive derivations check each production and token's slice eligibility, including radix and suffix. The unsupported refusal names the least slice admitting the full stream and the span that raised the requirement. Replace `Parse.parse`, the separate refusal grammar, token rewriting and handwritten body derivations.

## AST → Core → admitted Core

Fix (e) supplies the integrated Exec operands over Hybrid and the ownership instrumentation. The early fragment uses the shared scalar vocabulary and nominal declaration IDs. Elaboration preserves the admitted project and slice and establishes exact source correspondence and valid locations. The fixed judgment embeds the existing Slice 0 A-normal elaboration into the integrated carrier, pairing each operation with its retained span and assigning the single standalone entry its canonical internal identity. Fix (e) implements against this judgment. Later surface fragments extend the repository judgment when their elaboration is specified. The Core operation inventory requires an origin for each executable operation, including explicit abort. The location contract also quantifies over reachable Exec traps independently of table membership. Replace `Elab.Output`'s slice carrier and `Core.toCore` with `CoreInput`. Retire duplicate integer and trap types, wrapping and bit conversions, fixed-fuel outcomes and F31 with their consumers.

Fix (d) supplies the location and provenance rules. Semantic identities are spec `Var`, `PlaceId` and declaration IDs. An `Origin` contains a run-local operation ID, file-scoped lexical span and persistent evidence key. A file span resolves against exact bytes in the admitted project and agrees with the spec span judgment. A backend table index is a separate type, allocated only at BIR formation. R-0709 is represented by the project admission invariant.

Fix (c) supplies the single ConcreteSpec traversal. `Admitted` carries the exact Core input with its discharges grouped by operation and keyed by obligation within that group. Acceptance correspondence, independent static typing and discharge safety on reachable operation configurations are distinct proof obligations. The Core payload selects the entry and supplies the initial configuration through `Exec.enter`. The active operation follows from Exec control and frames. Safety excludes the exact discharged trap and requires a transition at every reachable matching configuration. A failed check yields `CheckerRefusal`; elaboration yields only elaboration refusals. Replace compiler-side `Admission.check`, `Admission.Token` and duplicate checker logic. The registry uses the exact spec checker through `check`.

## Admitted Core → MIR

Fix (c) owns evidence validation and consumption and the empty-evidence entry. Fix (b) owns the typed MIR instruction semantics. Fix (d) owns operation origin transport. `MIR` carries typed places, copy/move reads and explicit lifetime and control transfers. `LowersCore` ties removed checks to exact validated discharges and checks every emitted occurrence against the source condition inventory, indexed operator or exact callee, and ordered typed operand identities. Core calls and primitives take atomic typed reads that cannot trap on operand evaluation. A checked argument computation runs in its own Core operation before the call, with its own origin and result local. MIR checks its discharge at that operation and the call reads the result local. A residual condition is part of the same MIR instruction as the operation it guards and traps at its origin on failure. CFG edges enter blocks, so they cannot enter an instruction after its checks. Each supplied discharge matches exactly one emitted occurrence. An unconditional abort cannot supply a discharge condition. `LowersUnchecked` uses the same carrier and checks with no admission discharges. Both preserve origins and compare the fixed Core and MIR executions on the exact input and emitted graph. Block control boundaries carry origins for returns and other operations that emit no instruction. The boundary metadata cannot satisfy a required MIR check or replace a trap terminator. Replace the linear `MIR.Program` and its token-bearing operators.

## MIR → Memory

Fix (b) owns the CFG substrate and direct lowering to `Memory`. Both graphs use the same
block and terminator families and check the types of block arguments. The integrated spec
determines layouts, literal and operation values, field projections and aggregate codecs.
Integer literals and materialized integer operands use the spec decoder. Before registering
a big-endian profile, fixes (e) and (b) must pin the target-independent Core literal
encoding separately from target-endian materialized bytes. Each reachable
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
write can fill a moved subtree. Ending a partially moved root checks that all
remaining active fields can be discarded. MIR and Memory use the same masks,
and the representation checkpoint compares initialized subtrees across frames.
Replace the erase-to-MIR `Memory.lower` and `Memory.run` proof path.

## Memory → SSA

Fix (b) supplies the addressable/virtual partition and Fix (a) registers the validated SSA lowering. `SSA` has typed value IDs and parallel block arguments. Addressable places and layout remain explicit. SSA promotion records the eliminated virtual binding origins at control boundaries. `BuildsSSA` requires preservation of origins, layouts and observations against the
exact Memory and SSA semantics. A linear projected load consumes that field in the shared interpreter for SSA and BIR. A destructure consumes a whole materialized or
promoted root. It simultaneously defines the typed field registers and requires
each active linear field to have a distinct binding. Split the SSA construction
currently inside `SSA.lower` from its BIR output.

## SSA → BIR

Fix (a) owns the validated handoff and profile slot check. BIR formation preserves the decoded entry graph and refines the linked execution from that entry. Both stages run the same interpreter on their decoded graphs. Unreachable helper bodies can differ. Fix (d) owns the trap table and its origin proof. `BIR` indexes traps through `TrapIndex`; `BuildsBIR` requires valid lookup, origin preservation, behavior refinement against the graph’s execution rules and the resolved profile's BIR slot ceiling. The count is the largest function's number of BIR definitions, including block parameters,
destructure bindings and call destinations. The shared SSA/BIR interpreter reserves
register storage above the physical place IDs declared or named by lifetime instructions in each
function, preserving the
identity passed to spec address operations. An unresolved trap slot has neither a
successor nor a terminal result, including inside a callee. The resolved platform supplies the ISA, byte order and spec memory layout for either backend. Dominance-scoped validation can strengthen the invariant on the same graph before slice 3. Retire SSA's positional site creation and BIR's duplicate diagnostic identity and scalar vocabulary at cutover.

The handoff preserves the exact project used to resolve source locations and
produce runtime diagnostics, including sites in unreachable blocks.

## BIR → LLVM

Fix (a) registers the exact emitter and the named LLVM premise. `LLVMInput` carries a target triple, data layout, structured LLVM items and the exact trap table. `LLVMMetadataMatches` checks the target fields and trap table. The trusted-base entry
`P_print` covers the meaning of the exact emitted LLVM module. The renderer serializes the checked header and the structured body into the actual toolchain bytes. The legacy printer inventory and inverse claim retire in the A11 cutover. Full-image stack, toolchain and launch requirements stay in the theorem's native artifact boundary.

## BIR → LCB

Fix (a) registers the guarded adapter and profile preparation. `LCBInput`
carries the hosted body, auxiliary jobs keyed by BIR function identity, runtime
bytes and the exact trap table. The verified handoff validates the emitted
jobs, checks symbol uniqueness, and runs the entry job in the pinned LCB
semantics. The runtime projection uses the emitted diagnostic table to
distinguish returning from trapping. The guard rejects calls in the entry
graph before invoking the emitter when their blocks are reachable by following
CFG edges from the entry block. Multi-job execution requires
a pinned linked semantics and a proof of behavior preservation. The
[issue ledger](../../ISSUES.tsv) tracks that work under ISS-0105.

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

The registry binds each proposition to the function it executes.
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

When an atomic retirement spans owners, each writer prepares the replacement for its own files. The integration writer applies the coordinated deletion and updates the trusted-base inventories and cross-stage fixtures with the cutover. Frozen paths and the digest manifest belong to the interface round.
