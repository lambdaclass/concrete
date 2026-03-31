# Compiler Pass Contracts

This document describes each pass in the Concrete compiler pipeline, its signature, preconditions, postconditions, error conditions, and the invariant it establishes.

For the safety model enforced by these passes, see [SAFETY.md](SAFETY.md).

## Pipeline Overview

```
Source Text
    │
    ▼
  Parse ─── String → ParsedProgram
    │
    ├─→ BuildSummary ── ParsedProgram → SummaryTable
    │
    ▼
  Resolve ── ParsedProgram × SummaryTable → ResolvedProgram
    │
    ▼
  Check ─── ResolvedProgram × SummaryTable → Unit
    │
    ▼
  Elab ──── ResolvedProgram × SummaryTable → ElaboratedProgram
    │         (includes CoreCanonicalize)
    ▼
  CoreCheck ── ElaboratedProgram → ValidatedCore
    │
    ├─→ ProofCore extraction (Concrete/ProofCore.lean) — pure fragment for Lean proofs
    ├─→ Proof semantics (Concrete/Proof.lean) — formal evaluation + first theorems
    │
    ▼
  Mono ──── ValidatedCore → MonomorphizedProgram
    │
    ▼
  Lower ─── MonomorphizedProgram → SSAProgram
    │         (includes SSAVerify pre-cleanup, SSACleanup, SSAVerify post-cleanup)
    ▼
  EmitSSA ── SSAProgram → String (LLVM IR)
    │
    ▼
  clang ─── executable
```

Each arrow represents a typed artifact boundary defined in `Concrete/Pipeline.lean`. The pipeline is data-flow-driven: each pass consumes the previous pass's named artifact type. `ValidatedCore` is the proof-oriented boundary — only `Pipeline.coreCheck` can construct it, and `Pipeline.monomorphize` requires it as input.

### Proof-Oriented Pipeline Summary

For Lean-side proofs of selected Concrete functions, the intended proof boundary sits
after `CoreCheck` and before `Mono`.

Keep as-is:

- `CoreCheck` remains the semantic authority
- validated Core remains the main proof boundary
- `Mono` stays after the proof boundary
- SSA stays backend-only territory, mainly for compiler-preservation proofs
- explicit `Unsafe` / `trusted` / FFI boundaries stay visible in the language and reports

Done:

- `ValidatedCore` is a named, explicit pipeline artifact (`Concrete/Pipeline.lean`)
- `ProofCore` extracts the pure, proof-eligible fragment from validated Core (`Concrete/ProofCore.lean`)
- `Concrete/Proof.lean` defines evaluation semantics for a pure Core fragment and proves properties (abs, max, clamp correctness, literal evaluation, conditional reduction, arithmetic)
- `Pipeline.coreCheck` is the only constructor for `ValidatedCore`; `Pipeline.monomorphize` takes `ValidatedCore`

Still change:

- preserve source-to-Core traceability (span tracking from source through to Core)
- add selected-function export support for Lean-facing proof workflows
- extend proof fragment to cover structs, enums, match, and function calls with recursion
- keep proof scopes staged explicitly: pure first, then effects/resources/capabilities, then runtime/concurrency

Fine for now:

- no separate verification compiler
- no proof pass inserted into ordinary compilation
- no surface-AST proof target
- no backend or MLIR layer in the proof story yet

Main caution:

- `ProofCore` must not become a second semantic authority

---

## 1. Parse

**Signature:** `Pipeline.parse : String → Except Diagnostics ParsedProgram`

**Preconditions:**
- Input is a UTF-8 source string.

**Postconditions:**
- All tokens consumed. AST is syntactically well-formed.
- Module hierarchy (submodules) preserved.
- Capability aliases (`cap IO = File + Console;`) parsed and expanded: all `CapSet`s in function defs, impl blocks, trait sigs, trait impls, and function pointer types have alias names replaced with their constituent capabilities.
- No semantic validation performed.

**Error conditions:**
- Unexpected token, unterminated string, mismatched brackets/braces.
- Nested generic ambiguity handled (`>>` split into `> >`).
- Unknown capability name in `cap` alias definition.

**Invariant established:** Syntactically valid AST with all tokens consumed. Capability aliases fully expanded.

---

## 2. Resolve

**Signature:** `Pipeline.resolve : ParsedProgram → SummaryTable → Except Diagnostics ResolvedProgram`

Resolve is strictly a shallow/interface validation pass. It operates on `FileSummary` artifacts and surface AST — it never inspects function bodies for declaration-level information and does not perform any post-elaboration semantic checks.

**Preconditions:**
- Syntactically valid AST from Parse with module files resolved.
- `FileSummary` artifacts built for all modules (via `buildSummaryTable`).

**Postconditions:**
- All name references validated: function calls, struct/enum literals, static method calls, function references, variable identifiers.
- Deep type validation: all type names in annotations, parameters, return types, generics, refs, arrays, and function pointer types are known.
- `Self` only used inside impl/trait-impl blocks.
- Import validation: imported modules exist, imported symbols are public.
- Submodule definitions registered in global scope.

**What Resolve does NOT check:**
- **Instance method calls (`.methodCall`)** are skipped entirely. The method name depends on the receiver type, which is only known after type checking. This is an intentional boundary — method resolution requires type information that only Check can provide.
- **Trait impl completeness** — CoreCheck owns this (validates all required methods provided and signatures match, operating on Core IR after elaboration).
- **Trait impl signature compatibility** — CoreCheck owns this too.
- **Type correctness** — Resolve only checks that names exist, not that types are used correctly.
- **FFI safety, repr validation, Copy/Destroy rules** — all declaration-level semantic checks that can be stated on Core IR belong in CoreCheck.

**Error conditions** (all errors use the structured `ResolveError` inductive with span-bearing `Diagnostic` output via `Diagnostic.render`):
- Unknown function, struct type, enum, enum variant, static method, function reference.
- Unknown type name in any type position.
- `Self` outside impl block.
- Import referencing unknown module or non-public symbol.

**Invariant established:** All name references resolve to known definitions. Types are named validly. Imports reference existing public symbols.

---

## 3. Check

**Signature:** `checkProgram : List ResolvedModule → Except Diagnostics Unit`

**Preconditions:**
- `ResolvedProgram` from Resolve (proves name resolution happened).
- `SummaryTable` for import resolution.

**Postconditions:**
- Types are consistent across expressions, statements, and function signatures.
- Linearity discipline enforced: linear variables consumed exactly once, cannot be reassigned.
- Capability discipline validated: callers possess required capabilities.
- `defer` and borrow blocks are well-formed.
- Cross-module imports resolved via export tables.

**Error conditions** (all errors use the structured `CheckError` inductive, rendered to identical strings via `CheckError.message`):
- Type mismatch, undeclared variable/function/type.
- Linear variable double-consumed or left unconsumed.
- Missing capability for effect-requiring calls.
- Invalid borrow nesting, consuming borrowed references.

**Error accumulation:** Check accumulates errors at two granularities:
- Across functions/modules: `checkModule` runs each function independently, collecting all per-function errors.
- Within function bodies: `checkStmts` catches per-statement errors, restores the type environment on failure, and adds placeholder types for failed let-declarations to prevent cascading "undeclared variable" errors. All accumulated diagnostics are thrown at the end.

**Capability error hints:** Capability-related errors include actionable `hint:` text suggesting `with(Cap)` additions or trusted wrapper alternatives.

**Invariant established:** Types consistent, linearity valid, capabilities valid, FFI-safe types at extern boundaries. All names resolve within module scopes.

**FFI safety validation:**
- `#[repr(C)]` structs cannot have type parameters.
- All fields of `#[repr(C)]` structs must be FFI-safe types.
- All `extern fn` parameters and return types must be FFI-safe.
- FFI-safe types: integer types, float types, Bool, Char, `()`, raw pointers (`*mut T`, `*const T`), and `#[repr(C)]` structs.

**Unsafe boundary validation:**
- Dereferencing raw pointers (`*ptr` on `*mut T` or `*const T`) requires `Unsafe` capability.
- Assigning through raw pointers (`*ptr = val` on `*mut T`) requires `Unsafe` capability.
- Pointer-involving casts require `Unsafe` capability: pointer-to-pointer, pointer-to-integer, integer-to-pointer, array-to-pointer, pointer-to-reference.
- Reference-to-pointer casts (`&x as *const T`, `&mut x as *mut T`) are safe and do not require `Unsafe`, since they preserve compiler-known provenance.

---

## 4. Elab (Elaboration)

**Signature:** `elabProgram : List ResolvedModule → Except Diagnostics (List CModule)`

**Preconditions:**
- `ResolvedProgram` from Resolve (same artifact as Check consumed).
- Check has passed (types and linearity validated).

**Postconditions:**
- Surface AST desugared to Core IR (`CModule`/`CExpr`/`CStmt`).
- All expressions carry full type annotations.
- Method calls desugared to plain function calls.
- Arrow expressions desugared.
- `for` loops desugared to `while` with iterators.
- Trait method resolution applied.
- Cross-module import resolution via export tables.

**Error conditions** (all errors use the structured `ElabError` inductive, rendered to identical strings via `ElabError.message`):
- Unresolved trait method, unknown type in elaboration context.
- Import resolution failures.

**Error accumulation:** Elab accumulates errors at two granularities:
- Across functions/modules: `elabModule` runs each function independently, collecting all per-function errors.
- Within function bodies: `elabStmts` catches per-statement errors, skips failed statements from the output, and adds placeholder types for failed let-declarations. All accumulated diagnostics are thrown at the end.

**Invariant established:** Fully type-annotated Core IR. Methods/arrows/for desugared. Generic type parameters still present.

---

## 5. CoreCanonicalize

**Signature:** `canonicalizeProgram : List CModule → List CModule`

**Preconditions:**
- Valid Core IR from Elab.

**Postconditions:**
- Types normalized: `Ty.generic "Heap" [t]` → `Ty.heap t`, etc.
- Match arms sorted: specific constructors before wildcards/variables.
- Struct literal fields reordered to match definition order.
- Function signatures and expressions normalized.
- Submodules recursively canonicalized (trait defs, trait impls, types in submodule declarations are all normalized).

**Error conditions:**
- None (pure transformation, always succeeds).

**Invariant established:** Types normalized, match arms sorted, struct fields in definition order. Applies uniformly across top-level modules and all nested submodules.

---

## 6. CoreCheck

**Signature:** `coreCheckProgram : List CModule → Except String Unit`

CoreCheck is the post-elaboration semantic authority. It owns all legality rules that can be stated on Core IR, including both function-body validation and declaration-level checks. It recursively processes submodules, so declaration checks apply uniformly to inline `mod X { ... }` blocks.

**Preconditions:**
- Canonicalized Core IR.

**Postconditions:**
- Capability discipline re-validated at Core level: caller capSet ⊇ callee capSet.
- Operand types match operators (numeric ops on numeric types, etc.).
- Core-level capability requirements are enforced for lowered operations, builtins, and extern calls.
- Return statements agree with the elaborated function return type.
- Match structure is validated after elaboration, including wrong-enum arms, duplicate variants, and variant field-count agreement.
- Match expressions cover all enum variants (or have wildcard).
- `break`/`continue` only inside loops.
- Declaration-level checks validated across all modules and submodules:
  - Copy/Destroy conflict detection.
  - Copy struct fields must be Copy types.
  - `#[repr(C)]` validation (no generics, FFI-safe fields).
  - `#[repr(packed)]`/`#[repr(align(N))]` conflict and power-of-two checks.
  - Extern fn parameter and return type FFI safety.
  - Builtin trait (`Destroy`) redeclaration prevention.
  - Reserved function name detection.
  - Trait impl completeness: all required methods provided, return type signatures match trait definition.
  - Unknown trait detection.

**Error conditions** (all errors use the structured `CoreCheckError` inductive, rendered to identical strings via `CoreCheckError.message`):
- Insufficient capabilities for callee.
- Type mismatch on operator arguments.
- Missing capability for a Core-level builtin or operation.
- Return type mismatch.
- Incomplete match coverage.
- Wrong enum variant used in match.
- Duplicate match arm.
- Wrong field count for enum variant arm.
- `break`/`continue` outside loop context.
- Copy/Destroy conflict, non-Copy field in Copy struct.
- `#[repr(C)]` with generics, non-FFI-safe fields.
- `#[repr(packed)]` + `#[repr(align)]` conflict, non-power-of-two alignment.
- Non-FFI-safe extern fn parameters/return types.
- Builtin trait redeclared, unknown trait, missing trait method, trait method signature mismatch.
- Reserved function name.

**Invariant established:** Capabilities valid in Core IR, operand types match, return types agree, match structure/coverage valid, declaration-level trait/FFI/repr rules satisfied. All checks apply uniformly across top-level modules and nested submodules.

**Proof-boundary note:** for eventual Lean-side proofs of selected Concrete functions, the output of `CoreCheck` is the right semantic boundary. A future proof-oriented artifact should attach here, before monomorphization and lowering.

---

## 7. Mono (Monomorphization)

**Signature:** `monoProgram : List CModule → Except String (List CModule)`

**Preconditions:**
- CoreCheck has passed.

**Postconditions:**
- No type variables remain in any function body.
- All generic function calls instantiated with concrete types.
- Monomorphized function copies appended to module (e.g., `fn_for_i32_bool`).
- Original generic functions retained but unreferenced by concrete code.

**Error conditions:**
- Unbounded polymorphic recursion (infinite specialization).

**Invariant established:** All generics fully instantiated. No type variables in emitted code.

**Proof-boundary note:** user-program proofs should generally attach before this pass unless the proof target is specifically about a concrete monomorphized instantiation. The primary semantic proof boundary remains validated Core after `CoreCheck`.

---

## 8. Lower

**Signature:** `lowerModule : CModule → SModule`

**Preconditions:**
- Monomorphized Core IR (no type variables).

**Postconditions:**
- Structured control flow (if/else, while, for, match) converted to basic blocks with branches.
- SSA form: each variable assignment produces a unique register.
- Phi nodes inserted at control flow merge points.
- String literals extracted as globals, deduplicated by value across all functions, with globally unique names (`str.0`, `str.1`, ...). Per-function references are renamed to match the global canonical names.
- Borrowed string literals (`&"literal"`) produce `strConstRef` values instead of `strConst`, signaling that EmitSSA should reference the global constant directly without heap allocation.
- Generic functions filtered out (only concrete/monomorphized lowered).
- Every block has exactly one terminator (`br`, `condBr`, `ret`, `unreachable`).

**Error conditions:**
- None at the API level (pure transformation). Internal `LowerM` may fail on unknown types/struct lookups.

**Invariant established:** SSA form with explicit blocks, branches, phi nodes. Control flow fully linearized.

---

## 9. SSAVerify

**Signature:** `ssaVerifyProgram : List SModule → Except String Unit`

**Runs:** Both before and after SSACleanup (see `Pipeline.lower`). The pre-cleanup run validates Lower's output; the post-cleanup run mechanically ensures cleanup transformations preserved all SSA invariants.

**Preconditions:**
- SSA IR from Lower (pre-cleanup run) or SSACleanup (post-cleanup run).

**Postconditions:**
- Every block has exactly one terminator (structurally enforced by `SBlock.term : STerm`).
- All register uses dominated by their definitions.
- Phi node incoming values come from correct predecessor blocks.
- All branch targets reference existing block labels.
- Phi nodes have entries for all predecessor blocks.
- No duplicate register definitions within a block.

**Error conditions** (all errors use the structured `SSAVerifyError` inductive, rendered to identical strings via `SSAVerifyError.message`):
- Use-before-def (register used without dominating definition).
- Branch to non-existent label.
- Phi missing entry for a predecessor.
- Duplicate register definition.
- Block with no terminator or multiple terminators.

**Invariant established:** Dominance correct, branch targets valid, phi coverage complete, no duplicate defs.

---

## 10. SSACleanup

**Signature:** `ssaCleanupProgram : List SModule → List SModule`

**Preconditions:**
- SSAVerify has passed.

**Postconditions:**
- Dead (unreachable) blocks eliminated.
- Trivial phi nodes (single incoming value) replaced with direct register use.
- Empty blocks (containing only an unconditional branch) folded by redirecting predecessors.
- Semantics preserved.

**Error conditions:**
- None (pure transformation, always succeeds).

**Invariant established:** Dead blocks, trivial phis, and empty blocks removed. SSA form maintained.

---

## 11. EmitSSA

**Signature:** `emitSSAProgram : List SModule → String`

**Preconditions:**
- Cleaned-up SSA IR (all modules verified and optimized).

**Postconditions:**
- Valid LLVM IR text emitted with target triple and datalayout.
- Struct and enum type definitions generated.
- String literal globals emitted.
- All user functions emitted with correct LLVM signatures.
- Builtin functions (Vec, HashMap, String ops) included.
- `main` wrapper generated for entry point.
- External declarations (malloc, free, printf, etc.) emitted.
- **Extern fn ABI flattening:** `#[repr(C)]` struct parameters in extern fn calls are flattened to integer registers per the platform C ABI (ARM64: ≤8 bytes → i64, 9-16 bytes → two i64s, >16 bytes → ptr). Return values ≤8 bytes similarly flattened. This is an exception to the internal pass-by-ptr rule and matches clang's calling convention.

**Error conditions:**
- None at the API level (pure string generation).

**Invariant established:** Well-formed LLVM IR text suitable for clang compilation.

---

## SSA Backend Contract

The SSA pipeline (Lower → SSAVerify → SSACleanup → EmitSSA) is a closed backend contract.  This section consolidates what each stage guarantees, what each stage may assume, and the overall invariant chain.

### What SSAVerify guarantees (postconditions)

After SSAVerify passes, all downstream passes may assume:

1. **Dominance:** Every non-phi register use is dominated by its definition.
2. **Phi correctness:** Every phi node has entries for exactly its predecessor set.  Phi operands are defined in or dominate their source blocks.
3. **No aggregate phis:** Phi nodes carry only scalar types (no struct/enum types).
4. **Branch safety:** All branch targets reference existing block labels.
5. **Unique defs:** No register is defined twice in the same block.
6. **Call arity:** Function argument count matches parameter count.
7. **Return coverage:** Non-void functions return values; void functions do not.
8. **Type consistency:** Binary operation operands are type-compatible.

### What SSACleanup guarantees (postconditions)

After SSACleanup, EmitSSA may additionally assume:

1. **No dead blocks:** All blocks are reachable from the entry block.
2. **No trivial phis:** No phi with a single incoming value or all-identical incoming values.
3. **No empty blocks:** No blocks whose only instruction is an unconditional branch.
4. **No dead instructions:** No unused register definitions (except side-effecting ops).
5. **Constant folding:** Integer arithmetic on constants is evaluated at compile time.
6. **Strength reduction:** Multiply/divide by powers of 2 converted to shifts.
7. **Store-load forwarding:** Block-local value forwarding (invalidated by calls/memcpy).
8. **Fixpoint convergence:** The pass iterates to a fixpoint — re-running cleanup does not change the output.

### What EmitSSA assumes (preconditions)

EmitSSA is a pure translation; it does not validate its input.  It assumes:

1. All SSAVerify invariants hold (dominance, phi correctness, branch safety).
2. All SSACleanup optimizations have been applied (no dead blocks, no trivial phis).
3. Aggregate types are passed/returned by pointer (the ABI contract from Lower). Exception: extern fn calls with `#[repr(C)]` struct args use ABI-flattened integer registers (see postconditions above).
4. String literals are deduplicated globally (from Lower).
5. Vec operations carry correct element-size metadata.

### Invariant chain

```
Lower: Core → SSA with explicit control flow, phi insertion, ABI pass-by-ptr
  ↓
SSAVerify (pre-cleanup): validates dominance, phi coverage, type consistency, branch safety
  ↓ (all structural invariants now hold)
SSACleanup: removes dead code, folds constants, forwards stores, converges
  ↓
SSAVerify (post-cleanup): re-validates all invariants after cleanup transformations
  ↓ (no dead blocks, no trivial phis, constants folded, invariants mechanically re-checked)
EmitSSA: 1:1 translation to LLVM IR text — no validation, just emission
```

---

## Artifact Flow

The compiler produces explicit, stable artifacts at each stage. Since the introduction of `Concrete/Pipeline.lean`, each boundary is a named artifact type with a composable runner function:

```
Source Text
    │
    ▼
  Pipeline.parse ─── String → ParsedProgram (List Module)
    │
    ▼
  Pipeline.resolveFiles ─── IO, reads sub-module files → ParsedProgram
    │
    ▼
  Pipeline.buildSummary ─── ParsedProgram → SummaryTable (List (String × FileSummary))
    │
    ├─→ FileSummary: declaration-level cross-file interface
    │     (function sigs, extern sigs, impl method sigs, structs, enums,
    │      traits, constants, type aliases, newtypes, imports, public names)
    │
    ├─→ ResolvedImports: per-module import artifact
    │     (imported functions, structs, enums, impl blocks, trait impls,
    │      impl method sigs — all resolved from FileSummary)
    │
    ▼
  Pipeline.resolve ─── ParsedProgram × SummaryTable → ResolvedProgram (List ResolvedModule)
    │
    ▼
  Pipeline.check ─── ResolvedProgram × SummaryTable → Unit
    │
    ▼
  Pipeline.elaborate ─── ResolvedProgram × SummaryTable → ElaboratedProgram (List CModule)
    │                     (elab + canonicalize, no validation)
    ▼
  Pipeline.coreCheck ─── ElaboratedProgram → ValidatedCore (List CModule)
    │                     (validates Core IR; only way to construct ValidatedCore)
    ├──→ extractProofCore ─── ValidatedCore → ProofCore (pure fragment for Lean proofs)
    ▼
  Pipeline.monomorphize ─── ValidatedCore → MonomorphizedProgram (List CModule)
    │
    ▼
  Pipeline.lower ─── MonomorphizedProgram → SSAProgram (List SModule)
    │                  (includes lower + verify + cleanup)
    ▼
  Pipeline.emit ─── SSAProgram → String (LLVM IR)
    │
    ▼
  clang → executable
```

`Pipeline.runFrontend` composes the shared prefix (parse → resolveFiles → buildSummary → resolve → check → elaborate → coreCheck) used by all CLI entry points.

`FileSummary` is the single cross-file interface artifact for the current frontend architecture phase. All passes consume signatures and type declarations from it rather than rebuilding their own views from raw ASTs. `ResolvedImports` is the single import artifact consumed by Check and Elab — it is built once from the summary table and shared, not rebuilt ad hoc in each pass.

`Layout` is the single source of truth for type sizes, alignment, field offsets, pass-by-pointer decisions, `Ty` → LLVM type mappings, LLVM type definition generation (`structTypeDef`, `enumTypeDefs`, `builtinTypeDefs`), and FFI-safety checks (`isFFISafe`). Both EmitSSA and CoreCheck delegate to Layout rather than maintaining their own layout or type-emission logic.

`Report` is the current audit/inspection surface over the pipeline:
- `--report interface` consumes `FileSummary` — public API surface (functions, types, traits)
- `--report caps` consumes canonicalized Core — capability requirements with "why" traces showing which callees contribute each cap
- `--report unsafe` consumes canonicalized Core — trust boundary analysis (trusted fn/impl/extern, Unsafe capability holders, raw pointer signatures, what trusted functions wrap)
- `--report layout` consumes canonicalized Core — struct/enum sizes, alignment, field offsets, packed/repr(C) annotations, enum tag/payload layout
- `--report alloc` consumes canonicalized Core — allocation/cleanup summaries (vec_new/alloc sites, defer free patterns, returned-alloc warnings)
- `--report mono` compares pre- and post-monomorphization Core modules — generic function count, specialization instances

These reports are intended as compiler-facing audit outputs, not as a second semantic pipeline. All 8 modes are regression-tested with 59 semantic assertions in `run_tests.sh`.

- `--report authority` consumes canonicalized Core — for each capability, lists which functions require it with transitive call chain traces showing how the capability is introduced (BFS through the call graph)
- `--report proof` consumes canonicalized Core — ProofCore eligibility analysis: marks each function as eligible (pure, no trusted/extern/pointer ops) or excluded with specific reasons

### Next Report Modes

The following report mode is explicitly deferred — named here so its scope is defined before implementation starts:

| Mode | Purpose | Pipeline stage | Status |
|------|---------|---------------|--------|
| `--report high-integrity` | High-integrity profile summary: which functions could run in a no-alloc/no-panic/bounded-stack environment. | Post-Mono | Profile defined in `docs/SAFETY.md`; compiler enforcement not yet implemented |

These are audit-oriented modes — they answer questions about what the program does, not what it should do. No mode should become a second semantic authority; all should consume validated artifacts from the existing pipeline.

**Note:** `FileSummary` and `ResolvedImports` currently carry full impl/trait-impl blocks with method bodies (not just signatures). Check and Elab need these to type-check and elaborate imported method implementations. Splitting into interface-only and body portions is a future incremental-compilation concern, not a current blocker.

---

## Cross-Pass Invariants

| Property | Established by | Relied upon by |
|----------|---------------|----------------|
| Syntactic validity | Parse | All subsequent passes |
| Capability aliases expanded | Parse | Check, Elab, CoreCheck (see only concrete cap names) |
| FileSummary (declaration-level interface) | buildSummaryTable | Resolve, Check, Elab |
| ResolvedImports (per-module import artifact) | resolveImportsFromTable | Check, Elab |
| Name resolution, import validity | Resolve | Check, Elab (names exist, imports valid) |
| Type consistency | Check | Elab, CoreCheck |
| Linearity | Check | (enforced at surface level) |
| Capabilities | Check (cap-polymorphic calls), CoreCheck | EmitSSA (no runtime checks) |
| Return type agreement after elaboration | CoreCheck | Lower, EmitSSA |
| Match shape and coverage after elaboration | CoreCheck | Lower |
| Declaration-level legality (trait/FFI/repr) | CoreCheck | Lower, EmitSSA |
| Full type annotations | Elab | Mono, Lower, EmitSSA |
| No type variables | Mono | Lower, EmitSSA |
| SSA form / dominance | Lower, SSAVerify | SSACleanup, EmitSSA |
| No dead blocks | SSACleanup | EmitSSA |
