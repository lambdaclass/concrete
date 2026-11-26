# Stage interfaces

The frozen declarations are [spec carriers](../../ConcreteSpec/StageInterface.lean),
[compiler carriers](../../Concrete/Interfaces/Carriers.lean),
[grammar](../../ConcreteSpec/StageGrammar.lean),
[edge propositions](../../Concrete/Interfaces/Contracts.lean) and
[executable signatures and registry](../../Concrete/Interfaces/Pipeline.lean).
The root check compares their exact SHA-256 digests with the [recorded manifest](../../scripts/checks/lib/stage_interfaces.sha256). Changing a frozen declaration takes a new interface round. The data modules can be imported by both a pass and its caller without importing the pass implementation.

## Source → Tokens

Fix (f) owns the lossless scanner. `Source` carries the admitted project, entry-file ID and active slice. `TokenStream` retains decoded token payloads and the leading and EOF trivia ranges. `Lexes` relates the exact project bytes to the token stream and requires exact reconstruction. Malformed input and resource limits have separate typed refusals. Migrate `Lex.lex` and its correctness proof to the retained stream and prove agreement on its parser view.

## Tokens → AST

Fix (f) owns `ParserView → Except ParseRefusal (Parsed Ast)`. The AST is the typed start action result of one slice-tagged grammar. `CheckedTable` binds a lookahead to the exact production index. `Derives` covers all consumed tokens, production tags and action results. The parser reports a malformed stream or the required slice, with the positive derivation and the step bound separate from table verification cost. Replace `Parse.parse`, the refusal grammar, token rewriting and the separate handwritten body derivations.

## AST → Core → admitted Core

Fix (e) supplies the integrated Exec operands over Hybrid and the ownership instrumentation. The early fragment uses the shared scalar vocabulary and nominal declaration IDs. Elaboration preserves the admitted project and slice and establishes exact source correspondence and valid locations. Replace `Elab.Output`'s slice carrier and `Core.toCore` with `CoreInput`. Retire duplicate integer and trap types, wrapping and bit conversions, fixed-fuel outcomes and F31 with their consumers.

Fix (d) supplies the location and provenance rules. Semantic identities are spec `Var`, `PlaceId` and declaration IDs. An `Origin` contains a run-local operation ID, file-scoped lexical span and persistent evidence key. A file span resolves against exact bytes in the admitted project and agrees with the spec span judgment. A backend table index is a separate type, allocated only at BIR formation. R-0709 is represented by the project admission invariant.

Fix (c) supplies the single ConcreteSpec traversal. `Admitted` carries the exact Core input with its discharges grouped by operation and keyed by obligation within that group. Acceptance correspondence, independent static typing and discharge safety on reachable operation configurations are distinct proof obligations. A failed check yields `CheckerRefusal`; elaboration yields only elaboration refusals. Replace compiler-side `Admission.check`, `Admission.Token` and duplicate checker logic. The registry uses the exact spec checker through `check`.

## Admitted Core → MIR

Fix (c) owns evidence validation and consumption and the empty-evidence entry. Fix (b) owns the typed MIR instruction semantics. Fix (d) owns operation origin transport. `MIR` carries typed places, copy/move reads and explicit lifetime and control transfers. `LowersCore` ties removed checks to exact validated discharges and requires residual guard coverage. `LowersUnchecked` uses the same carrier and checks with no admission discharges. Both preserve origins and terminal observations. Replace the linear `MIR.Program` and its token-bearing operators.

## MIR → Memory

Fix (b) owns the CFG substrate and direct lowering to `Memory`. Both graphs use the same block and terminator families and typed block argument agreement. Memory carries target-derived layouts and has its own transition state for virtual values, live logical locations, materialized ranges and spec byte memory. `LowersMemory` relates reachable states and requires well-formed bytes, promotion eligibility and layout correspondence on the resolved target. Zero-byte values have logical lifetime. Replace the erase-to-MIR `Memory.lower` and `Memory.run` proof path.

## Memory → SSA

Fix (b) supplies the addressable/virtual partition and Fix (a) registers the validated SSA lowering. `SSA` has typed value IDs and parallel block arguments. Addressable places and layout remain explicit. `BuildsSSA` requires preservation of origins, layouts and observations. Split the SSA construction currently inside `SSA.lower` from its BIR output.

## SSA → BIR

Fix (a) owns the validated handoff and profile slot check. Fix (d) owns the trap table and its origin proof. `BIR` indexes traps through `TrapIndex`; `BuildsBIR` requires valid lookup, origin preservation, behavioral simulation and the resolved profile's BIR slot ceiling. Dominance-scoped validation can strengthen the invariant on the same graph before slice 3. Retire SSA's positional site creation and BIR's duplicate diagnostic identity and scalar vocabulary at cutover.

## BIR → LLVM

Fix (a) registers the exact emitter and the named LLVM premise. `LLVMInput` carries the full module bytes and the exact trap table. `EmitsLLVM` relates those bytes to the BIR and projected behavior. The legacy printer inventory and inverse claim retire in the A11 cutover. Full-image stack, toolchain and launch requirements stay in the theorem's native artifact boundary.

## BIR → LCB

Fix (a) registers the exact adapter and profile preparation. `LCBInput` carries the actual source function, additional functions for the later call profile, runtime input bytes and the same trap table. `EmitsLCB` requires exact handoff and BIR refinement on adapter success. The proved source adapter and native lowering premises remain separate. The result-word encoding belongs in the backend/runtime projection.

## Common proof boundary

Executable registry entries own the function and the proof graph uses that function's result. `Pipeline.Sound` requires edge soundness and adjacent invariant implication for the registered fold. The production and unchecked folds differ at admission. Semantic edges use fuel-free simulation over the theorem's transition system, with a nonempty source path or a decreasing stutter rank, finite-prefix safety, progress and divergence trace matching. The precise terminal projection distinguishes return, trap and startup refusal, including exact stderr and file-scoped lexical sites. Evaluator fuel appears separately in adequacy. Existence of an admitted observation and native-image assumptions remain theorem-level obligations.

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
