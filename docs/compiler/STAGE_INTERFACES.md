# Stage interfaces

The frozen declarations are [spec carriers](../../ConcreteSpec/StageInterface.lean),
[compiler carriers](../../Concrete/Interfaces/Carriers.lean),
[grammar](../../ConcreteSpec/StageGrammar.lean),
[behavior refinement](../../Concrete/Interfaces/Behavior.lean), [edge propositions](../../Concrete/Interfaces/Contracts.lean) and
[executable signatures and registry](../../Concrete/Interfaces/Pipeline.lean).
The root check compares their exact SHA-256 digests with the [recorded manifest](../../scripts/checks/lib/stage_interfaces.sha256). Changing a frozen declaration takes a new interface round. The data modules can be imported by both a pass and its caller without importing the pass implementation.

## Source → Tokens

Fix (f) owns the lossless scanner. `Source` carries the admitted project, entry-file ID and active slice. `TokenStream` retains typed decoded tokens, including string bytes, byte literals and label names, plus the leading and EOF trivia ranges. `Lexes` relates the exact project bytes to the token stream and requires exact reconstruction and establishes the spec-owned parser-view invariant. Malformed input and resource limits have separate typed refusals. Migrate `Lex.lex` and its correctness proof to the retained stream and prove agreement on its parser view.

## Tokens → AST

Fix (f) supplies the grammar, table-verification program and correspondence proofs. The AST is indexed by the start symbol. The verifier runs a closed word machine over proposal memory and certifies the exact dense prediction table used by the parser. Its word-operation and memory budgets use a fixed multiplier of grammar positions times the exact number of lookahead columns. The boundary validates file IDs, ranges and exact starting coordinates in one pass over project bytes and tokens. Invalid streams receive a typed refusal before grammar execution. The parser executes the frozen cursor machine with indexed lookups and first-order typed constructor actions. It counts visits and action constructors on the same execution that returns the result. A stage proof must show that execution completes within its linear token budget, including on refusal. The full declarative and predictive derivations check each production and token's slice eligibility, including radix and suffix. The unsupported refusal names the least slice admitting the full stream and the span that raised the requirement. Replace `Parse.parse`, the separate refusal grammar, token rewriting and handwritten body derivations.

## AST → Core → admitted Core

Fix (e) supplies the integrated Exec operands over Hybrid and the ownership instrumentation. The early fragment uses the shared scalar vocabulary and nominal declaration IDs. Elaboration preserves the admitted project and slice and establishes exact source correspondence and valid locations. The Core operation inventory requires an origin for each executable operation, including explicit abort. The location contract also quantifies over reachable Exec traps independently of table membership. Replace `Elab.Output`'s slice carrier and `Core.toCore` with `CoreInput`. Retire duplicate integer and trap types, wrapping and bit conversions, fixed-fuel outcomes and F31 with their consumers.

Fix (d) supplies the location and provenance rules. Semantic identities are spec `Var`, `PlaceId` and declaration IDs. An `Origin` contains a run-local operation ID, file-scoped lexical span and persistent evidence key. A file span resolves against exact bytes in the admitted project and agrees with the spec span judgment. A backend table index is a separate type, allocated only at BIR formation. R-0709 is represented by the project admission invariant.

Fix (c) supplies the single ConcreteSpec traversal. `Admitted` carries the exact Core input with its discharges grouped by operation and keyed by obligation within that group. Acceptance correspondence, independent static typing and discharge safety on reachable operation configurations are distinct proof obligations. The Core payload selects the entry and supplies the initial configuration through `Exec.enter`. The active operation follows from Exec control and frames. Safety excludes the exact discharged trap and requires a transition at every reachable matching configuration. A failed check yields `CheckerRefusal`; elaboration yields only elaboration refusals. Replace compiler-side `Admission.check`, `Admission.Token` and duplicate checker logic. The registry uses the exact spec checker through `check`.

## Admitted Core → MIR

Fix (c) owns evidence validation and consumption and the empty-evidence entry. Fix (b) owns the typed MIR instruction semantics. Fix (d) owns operation origin transport. `MIR` carries typed places, copy/move reads and explicit lifetime and control transfers. `LowersCore` ties removed checks to exact validated discharges and checks every emitted occurrence against the source condition inventory, indexed operator or exact callee, and ordered typed operand identities. Core calls and primitives take atomic typed reads that cannot trap on operand evaluation. A checked argument computation runs in its own Core operation before the call, with its own origin and result local. MIR checks its discharge at that operation and the call reads the result local. A residual condition is part of the same MIR instruction as the operation it guards and traps at its origin on failure. CFG edges enter blocks, so they cannot enter an instruction after its checks. Each supplied discharge matches exactly one emitted occurrence. An unconditional abort cannot supply a discharge condition. `LowersUnchecked` uses the same carrier and checks with no admission discharges. Both preserve origins and terminal observations. Block control boundaries carry origins for returns and other operations that emit no instruction. The boundary metadata cannot satisfy a required MIR check or replace a trap terminator. Replace the linear `MIR.Program` and its token-bearing operators.

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
and call checks and evaluate them before performing the instruction. The simulation
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
direct and indirect reads and writes. A linear move consumes root
initialization, and field writes require an initialized root. Replace the erase-to-MIR `Memory.lower`
and `Memory.run` proof path.

## Memory → SSA

Fix (b) supplies the addressable/virtual partition and Fix (a) registers the validated SSA lowering. `SSA` has typed value IDs and parallel block arguments. Addressable places and layout remain explicit. SSA promotion records the eliminated virtual binding origins at control boundaries. `BuildsSSA` requires preservation of origins, layouts and observations against the
exact Memory and SSA semantics. A destructure consumes a whole materialized or
promoted root. It simultaneously defines the typed field registers and requires
each active linear field to have a distinct binding. Split the SSA construction
currently inside `SSA.lower` from its BIR output.

## SSA → BIR

Fix (a) owns the validated handoff and profile slot check. BIR formation preserves the decoded linked program, including helpers unreachable from the hosted entry. Both stages run the same interpreter on that program, so the decoded-program check implies backward refinement. Fix (d) owns the trap table and its origin proof. `BIR` indexes traps through `TrapIndex`; `BuildsBIR` requires valid lookup, origin preservation, simulation against the graph’s execution rules and the resolved profile's BIR slot ceiling. The count is the largest function's number of BIR definitions, including block parameters,
destructure bindings and call destinations. The shared SSA/BIR interpreter reserves
register storage above the physical place IDs declared or named by lifetime instructions in each
function, preserving the
identity passed to spec address operations. An unresolved trap slot has neither a
successor nor a terminal result, including inside a callee. The resolved platform supplies the ISA, byte order and spec memory layout for either backend. Dominance-scoped validation can strengthen the invariant on the same graph before slice 3. Retire SSA's positional site creation and BIR's duplicate diagnostic identity and scalar vocabulary at cutover.

## BIR → LLVM

Fix (a) registers the exact emitter and the named LLVM premise. `LLVMInput` carries a target triple, data layout, structured LLVM items and the exact trap table. `LLVMMetadataMatches` checks the target fields and trap table. The trusted-base entry
`P_print` covers the meaning of the exact emitted LLVM module. The renderer serializes the checked header and the structured body into the actual toolchain bytes. The legacy printer inventory and inverse claim retire in the A11 cutover. Full-image stack, toolchain and launch requirements stay in the theorem's native artifact boundary.

## BIR → LCB

Fix (a) registers the exact adapter and profile preparation. `LCBInput` carries
the hosted body, auxiliary jobs keyed by BIR function identity, runtime bytes and
the exact trap table. `EmitsLCB` requires one job for each BIR function, validates
each exact job and requires unique emitted symbols. The hosted body runs through
the runtime ABI with the emitted diagnostic table. Each auxiliary invocation resolves the exact job by function ID and runs it
with every argument tuple in its validated integer ABI and compared with the
corresponding BIR function running in the full BIR function environment. The
auxiliary return comparison uses the full ABI value. The pinned LCB verified
profile admits leaf jobs. The dependency also ships a separate module evaluator
that resolves calls, with fixed execution budgets. That evaluator consumes a
different IR from the verified job format. [LP0](../../ROADMAP.md#vertical-slices)
tracks imported call steps, frames, canonical ABI value encoding and the
multi-frame trap ABI for a profile that emits inter-job calls. The proved source adapter and native lowering
premises remain separate.

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

The CFG interpreters resolve calls through the full function environment.
Hosted behaviors start at the entry and therefore cannot distinguish changes
to an unreachable definition. The SSA to BIR handoff checks all decoded graphs
in addition to hosted behaviors. The current LCB pin supplies individual
function semantics, so linked inter-job execution remains a dependency of the
full-program LCB contract.

The registry binds each proposition to the function it actually executes.
`Pipeline.Sound` checks the registered success and refusal obligations and
adjacent invariants. LLVM has a structural handoff check and a named trusted
assumption, `P_print`. Native-image and launch assumptions remain at the theorem
boundary.

## Writer ownership

Use separate implementation modules so parallel proof and implementation work can proceed against the frozen signatures. These paths reserve ownership, including their eventual in-place retirements. Existing cross-cutting gates and root imports are integrated by (a) after the implementation branches are ready.

| Fix | Owned implementation paths and retirements |
| --- | --- |
| (e) | `Concrete/IntegratedCore.lean`, `Concrete/Core.lean`, `Concrete/Elab.lean`, `ConcreteSpec/IntegratedCore.lean`, `ConcreteSpec/Spec/Slice0.lean`, `ConcreteSpec/Spec/Slice1Syntax.lean`, `ConcreteSpec/Spec/Slice1Semantics.lean`, `ConcreteSpec/Spec/Scalar.lean`, `ConcreteSpec/Proofs/Slice0.lean`, `ConcreteSpec/Proofs/Slice1.lean`, `ConcreteSpec/Proofs/Scalar.lean`, `scripts/checks/compiler/f31_census.sh` |
| (d) | `Concrete/Origins.lean`, `ConcreteSpec/Spec/Ident.lean`, `ConcreteSpec/Proofs/Ident.lean`, `Concrete/Backend/Observation.lean` |
| (c) | `ConcreteSpec/Admission.lean`, `Concrete/Admission.lean`, `Concrete/LowerAdmitted.lean`, `ConcreteSpec/Spec/Slice1Checker.lean`, `ConcreteSpec/Spec/Slice1Ownership.lean`, `ConcreteSpec/Proofs/Checker.lean` |
| (b) | `Concrete/CFG.lean`, `Concrete/MIR.lean`, `Concrete/Memory.lean`, `Concrete/SSA.lean`, `ConcreteSpec/Layout.lean`, `ConcreteSpec/Spec/Memory.lean`, `ConcreteSpec/Spec/Hybrid.lean`, `ConcreteSpec/Proofs/Memory.lean`, `ConcreteSpec/Proofs/Hybrid.lean` |
| (a) | `Concrete/BIR.lean`, `Concrete/BIR/`, `Concrete/Stages.lean`, `Concrete/Contracts.lean`, `Concrete/Theorem.lean`, `Concrete/Certificate.lean`, `Concrete/Premises.lean`, `Main.lean`, `Concrete/Backend/` except `Observation.lean`, root imports, proof inventories and cross-stage checks and fixtures |
| (f) | `Concrete/Lex.lean`, `Concrete/Parse.lean`, `Concrete/LL1.lean`, `ConcreteSpec/Spec/LL1.lean`, `ConcreteSpec/Spec/Surface0.lean`, `ConcreteSpec/Surface1.lean`, `ConcreteSpec/Proofs/Surface0.lean`, `ConcreteSpec/Proofs/Surface1.lean`, `ConcreteSpec/Proofs/LL1.lean` |

When an atomic retirement spans owners, each writer prepares the replacement for its own files. The integration writer applies the coordinated deletion and updates the trusted-base inventories and cross-stage fixtures with the cutover. Frozen paths and the digest manifest belong to the interface round.
