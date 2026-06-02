# Safety Model

Status: stable reference

This document is the central reference for Concrete's safety model. It defines the organizing principles, explains how the pieces fit together, and points to the detailed references for each subsystem.

For ownership and linearity, see [VALUE_MODEL.md](VALUE_MODEL.md).
For the current safe-memory guarantee boundary, see [MEMORY_GUARANTEES.md](MEMORY_GUARANTEES.md).
For named profiles (`safe`, `predictable`, `provable`, `high-integrity`), see [PROFILES.md](PROFILES.md).
For the short public claim surface, see [CLAIMS_TODAY.md](CLAIMS_TODAY.md).
For explicit trusted-computing-base accounting, see [TRUSTED_COMPUTING_BASE.md](TRUSTED_COMPUTING_BASE.md).
For effect/trust proof boundaries, see [EFFECT_PROOF_BOUNDARIES.md](EFFECT_PROOF_BOUNDARIES.md).
For the precise public guarantee statement, see [GUARANTEE_STATEMENT.md](GUARANTEE_STATEMENT.md).
For the proof-claim taxonomy, see [CLAIM_TAXONOMY.md](CLAIM_TAXONOMY.md).
For FFI, trusted boundaries, and capability aliases, see [FFI.md](FFI.md).
For diagnostics and error recovery, see [DIAGNOSTICS.md](DIAGNOSTICS.md).
For pass contracts and report modes, see [PASSES.md](PASSES.md).
For the execution model and runtime boundary, see [EXECUTION_MODEL.md](EXECUTION_MODEL.md).
For the stdlib module inventory and capability mapping, see [STDLIB.md](STDLIB.md).

## The Three-Way Split

Concrete's safety model is organized around a three-way split:

| Mechanism | What it covers | Visibility | Example |
|-----------|---------------|------------|---------|
| **Capabilities** | Semantic effects visible to callers | In function signatures | `with(File, Alloc)` |
| **`trusted`** | Containment of pointer-level implementation unsafety behind safe APIs | At declaration site only; callers see a safe signature | `trusted impl Vec<T> { ... }` |
| **`with(Unsafe)`** | Authority to cross foreign boundaries (FFI, transmute) | In function signatures, even inside trusted code | `extern fn read(...)` requires `with(Unsafe)` |

This split applies uniformly across compiler builtins, stdlib internals, and user code. No layer is silently exempt.

### Capabilities

Capabilities are named, compile-time effects declared in function signatures:

```
fn serve() with(Network, Alloc) -> Int { ... }
```

The nine capabilities are: `File`, `Network`, `Clock`, `Env`, `Random`, `Process`, `Console`, `Alloc`, `Unsafe`.

A function can only call functions whose capabilities are a subset of its own. This is checked at both the surface level (Check) and the Core IR level (CoreCheck). Capability errors include actionable hints suggesting `with(Cap)` additions or trusted wrapper alternatives.

**Capability aliases** reduce signature repetition:

```
cap IO = File + Console;
fn log(msg: &String) with(IO) { ... }
```

Aliases expand at parse time. The rest of the compiler sees only concrete capability names. See [FFI.md](FFI.md) for details.

**`Std` macro**: `with(Std)` expands to all standard capabilities except `Unsafe`.

### Trusted Boundaries

`trusted fn`, `trusted impl`, and `trusted extern fn` contain pointer-level implementation techniques behind safe APIs:

```
trusted impl Vec<T> {
    pub fn push(self: &mut Vec<T>, value: T) with(Alloc, Unsafe) { ... }
}
```

**What `trusted` is:** An audited containment boundary for pointer-level implementation techniques. It means "this declaration uses implementation techniques ordinary safe code cannot use directly, and that usage is intentionally concentrated and review-relevant."

**What `trusted` permits** (and nothing else):
- Pointer arithmetic (`*mut T + offset`)
- Raw pointer dereference (`*p`)
- Raw pointer assignment (`*p = value`)
- Pointer-involving casts (except ref→ptr, which is always safe)

**What `trusted` does not do:**
- Does **not** suppress capabilities — callers still need the declared capabilities
- Does **not** permit `extern fn` calls without `with(Unsafe)` — foreign boundaries stay explicit
- Does **not** relax linearity rules — linear values follow the same ownership rules everywhere
- Does **not** hide any caller-visible semantic fact
- `trusted extern fn` is a separate, narrower mechanism for audited pure foreign bindings (e.g., `sqrt`)

**Syntactic surfaces** (intentionally narrow):
- `trusted fn` — standalone function
- `trusted impl` — all methods in the impl block
- `trusted extern fn` — audited foreign binding

The stdlib demonstrates this pattern throughout: `trusted impl Vec<T>`, `trusted impl HashMap<K, V>`, `trusted impl TextFile`, `trusted fn print/println`. See [FFI.md](FFI.md) for wrapper patterns.

### `with(Unsafe)`

`with(Unsafe)` gates the explicit low-level boundary:

- calling `extern fn` (but not `trusted extern fn`)
- raw pointer dereference and assignment (outside `trusted` code)
- pointer-involving casts (outside `trusted` code)

The audit story is simple: `grep with(Unsafe)` finds the boundary.

## Linearity and Ownership

Concrete uses linear types by default for structs and enums. This provides compile-time resource safety without a garbage collector:

- Linear values must be consumed exactly once
- Branches must agree on consumption
- `defer` schedules cleanup that runs when the enclosing scope exits
- `borrow(x) { ... }` creates scoped references without consuming
- `Heap<T>` is linear — must be freed, cannot be silently dropped

Primitives, `&T`, raw pointers, and function pointers are Copy. See [VALUE_MODEL.md](VALUE_MODEL.md) for the full value category table.

### Current Claim Boundary

Concrete already enforces a real ownership/borrow discipline in the checker:

- linear ownership
- borrow conflict checking
- borrow escape checking
- explicit cleanup scheduling
- trusted/unsafe separation without relaxing linearity

What is not yet fully closed as a public language claim is the final centralized semantics for all reference/aliasing edge cases and the proof-facing statement that turns those checker rules into one explicit safe-memory guarantee. That remaining closure work is tracked in the roadmap's memory/reference-model item.

## Audit Reports

Concrete's compiler produces eight report modes for audit and inspection:

| Mode | What it shows |
|------|--------------|
| `--report caps` | Per-function capabilities with "why" traces showing which callees contribute each cap |
| `--report unsafe` | Trust boundary analysis: `trusted fn/impl/extern`, `Unsafe` holders, raw pointer signatures, what trusted functions wrap |
| `--report authority` | Per-capability function lists with transitive BFS call-chain traces |
| `--report proof` | ProofCore eligibility: which functions are proof-eligible, which are excluded and why |
| `--report layout` | Struct/enum sizes, alignment, field offsets, packed/repr(C) annotations |
| `--report alloc` | Allocation/cleanup summaries: alloc sites, defer free patterns, leak warnings |
| `--report interface` | Public API surface: functions, types, traits |
| `--report mono` | Generic function count and monomorphization specializations |

These are audit-oriented outputs, not a second semantic pipeline. All modes consume validated compiler artifacts from the existing pipeline. See [PASSES.md](PASSES.md) for details and the 59 regression assertions.

## Error Model

Concrete uses structured, per-pass error types with source spans:

- `ResolveError` — name resolution failures
- `CheckError` — type mismatches, linearity violations, capability violations
- `ElabError` — elaboration failures
- `CoreCheckError` — post-elaboration legality violations
- `SSAVerifyError` — backend invariant violations

Errors accumulate at two granularities:
- Across functions/modules: each function is checked independently
- Within function bodies: per-statement recovery catches independent errors without cascading

Capability-related errors include actionable `hint:` text suggesting specific fixes. See [DIAGNOSTICS.md](DIAGNOSTICS.md) for the full error model.

## Proof Boundary

The proof boundary sits after CoreCheck and before Mono, materialized as the `ValidatedCore` artifact type. At this point the program is:

- fully elaborated (no surface sugar)
- normalized by CoreCanonicalize
- validated by CoreCheck (capabilities, types, match coverage, declaration legality)

`ProofCore` (`Concrete/ProofCore.lean`) extracts the pure, proof-eligible fragment from `ValidatedCore`:

**Eligible:** Pure functions (empty capability set, not trusted, no extern calls), safe algebraic data types (no repr(C)/packed).

**Excluded:** Functions with capabilities, trusted/unsafe functions, extern functions, entry points.

`Concrete/Proof.lean` defines formal evaluation semantics for the pure Core fragment and proves properties (abs, max, clamp correctness, structural lemmas, conditional reduction, arithmetic). See [PROVABLE_SUBSET.md](PROVABLE_SUBSET.md) for the full definition of the proof-eligible subset, and [ARCHITECTURE.md](ARCHITECTURE.md) for the proof architecture.

## High-Integrity Profile Direction

Concrete is working toward a clearly defined high-integrity profile for critical code. The profile is the same language under stricter, explicit restrictions — not a separate language or a large contract system.

### What it restricts

| Restriction | Meaning |
|------------|---------|
| No `Unsafe` | No `with(Unsafe)`, or only through approved `trusted extern fn` wrappers |
| No unrestricted FFI | No `extern fn` calls outside approved wrappers |
| No dynamic allocation | Or only bounded allocation (`no_alloc`, `bounded_alloc`) |
| No ambient authority growth | Capabilities declared and budgeted, not accumulated |
| Analyzable concurrency | Structured concurrency only, no unrestricted thread spawning |
| Stronger evidence | Reports, traceability, and eventually proof artifacts required |

### What it requires from the compiler

1. **Profile-recognized restrictions** — actual compiler checks, not just style guidance
2. **Profile-aware reports** — why code qualifies, where it crosses the stricter rules
3. **Project/package visibility** — profile membership visible at module or package scope
4. **A clear relation to the proof story** — ProofCore eligibility aligns naturally with profile restrictions

### Relationship to existing features

The high-integrity profile is a cross-phase synthesis, not a single feature:

- **Capabilities** gate what authority code may use — profile restricts which caps are allowed
- **`trusted`** contains implementation unsafety — profile may restrict `trusted` to approved wrappers only
- **Linearity** ensures resource safety — profile strengthens this by requiring explicit cleanup evidence
- **ProofCore** extracts the provable fragment — profile code should maximize proof eligibility
- **Reports** make boundaries visible — profile reports show what meets and fails the stricter rules

### What it is not

- Not a second language
- Not a giant contract system
- Not a justification for uncontrolled feature growth plus "safe mode" later
- Not a vague marketing term without compiler and report consequences

The profile direction is documented in detail in [../research/language/high-integrity-profile.md](../research/language/high-integrity-profile.md).

## How The Pieces Fit Together

```
Capabilities ─── what authority code may use (compile-time, signature-visible)
     │
     ├── cap aliases reduce repetition (cap IO = File + Console)
     ├── Std macro expands to all standard caps except Unsafe
     ├── --report caps shows per-function capability requirements
     └── --report authority shows transitive capability chains

Trusted ──────── containment of pointer-level unsafety behind safe APIs
     │
     ├── trusted fn / trusted impl / trusted extern fn
     ├── callers still see the declared capabilities; only trusted extern fn avoids with(Unsafe)
     ├── --report unsafe shows trust boundaries
     └── wrapper patterns documented in docs/FFI.md

with(Unsafe) ─── explicit foreign boundary gate
     │
     ├── required for extern fn calls (even inside trusted code)
     ├── required for raw pointer ops (outside trusted code)
     ├── grep with(Unsafe) finds the boundary
     └── --report unsafe shows where and why

Linearity ────── compile-time resource safety
     │
     ├── structs/enums linear by default, opt-in Copy
     ├── must consume exactly once
     ├── defer for cleanup
     └── borrow blocks for scoped references

ProofCore ────── proof-eligible subset of validated Core
     │
     ├── pure functions only (no caps, no trusted, no extern)
     ├── safe algebraic data types
     ├── --report proof shows eligibility
     └── Concrete/Proof.lean has formal semantics + theorems

High-Integrity ── future profile: same language, stricter rules
     │
     ├── restricted Unsafe/FFI/allocation/authority
     ├── profile-aware compiler checks and reports
     └── maximizes proof eligibility
```

## Cross-References

| Topic | Primary doc | Also mentioned in |
|-------|------------|-------------------|
| Capabilities | This document, [FFI.md](FFI.md) | [PASSES.md](PASSES.md), [STDLIB.md](STDLIB.md) |
| Trusted boundaries | [FFI.md](FFI.md) | This document, [STDLIB.md](STDLIB.md) |
| Capability aliases | [FFI.md](FFI.md) | [PASSES.md](PASSES.md) |
| Linearity / ownership | [VALUE_MODEL.md](VALUE_MODEL.md) | This document |
| Error model | [DIAGNOSTICS.md](DIAGNOSTICS.md) | [PASSES.md](PASSES.md) |
| Report modes | [PASSES.md](PASSES.md) | This document |
| Proof boundary | [ARCHITECTURE.md](ARCHITECTURE.md) | This document, [PASSES.md](PASSES.md) |
| Execution model | [EXECUTION_MODEL.md](EXECUTION_MODEL.md) | [STDLIB.md](STDLIB.md) |
| High-integrity profile | This document | [IDENTITY.md](IDENTITY.md), [EXECUTION_MODEL.md](EXECUTION_MODEL.md) |
| ABI / FFI safety | [ABI.md](ABI.md) | [FFI.md](FFI.md), [VALUE_MODEL.md](VALUE_MODEL.md) |
