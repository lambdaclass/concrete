# Safety Model

Moved from docs/language: the overview of the safety model, its claims, audit reports and the high-integrity profile direction.

The rules are in [docs/spec/trusted.md](../spec/trusted.md) (TRUST-D1, TRUST-1 to TRUST-13) and
[docs/spec/ffi.md](../spec/ffi.md) (FFI-1 to FFI-9); this document is the design record of the
safety model and keeps its claims, audit reports and the high-integrity profile direction.

Status: stable reference

This document is the central reference for Concrete's safety model. It defines the organizing principles, explains how the pieces fit together, and points to the detailed references for each subsystem.

For ownership and linearity, see [VALUE_MODEL.md](../archive/design-records/VALUE_MODEL.md).
For the current safe-memory guarantee boundary, see [MEMORY_GUARANTEES.md](MEMORY_GUARANTEES.md).
For named profiles (`safe`, `predictable`, `provable`, `high-integrity`), see [PROFILES.md](../platform/PROFILES.md).
For the short public claim surface, see [CLAIMS_TODAY.md](CLAIMS_TODAY.md).
For explicit trusted-computing-base accounting, see [TRUSTED_COMPUTING_BASE.md](TRUSTED_COMPUTING_BASE.md).
For effect/trust proof boundaries, see [EFFECT_PROOF_BOUNDARIES.md](EFFECT_PROOF_BOUNDARIES.md).
For the precise public guarantee statement, see [GUARANTEE_STATEMENT.md](GUARANTEE_STATEMENT.md).
For the proof-claim taxonomy, see [CLAIM_TAXONOMY.md](CLAIM_TAXONOMY.md).
For FFI, trusted boundaries, and capability aliases, see [FFI.md](../platform/FFI.md).
For diagnostics and error recovery, see [DIAGNOSTICS.md](../compiler/DIAGNOSTICS.md).
For pass contracts and report modes, see [PASSES.md](../compiler/PASSES.md).
For the execution model and runtime boundary, see [EXECUTION_MODEL.md](../platform/EXECUTION_MODEL.md).
For the stdlib module inventory and capability mapping, see [STDLIB.md](../stdlib/STDLIB.md).

## The Three-Way Split

Concrete's safety model is organized around a three-way split:

| Mechanism | What it covers | Visibility | Example |
|-----------|---------------|------------|---------|
| **Capabilities** | Semantic effects visible to callers | In function signatures | `with(File, Alloc)` |
| **`trusted`** | Containment of pointer-level implementation unsafety behind safe APIs | At declaration site only; callers see a safe signature | `impl Vec<T> { pub trusted fn push(..) { ... } }` |
| **`with(Unsafe)`** | The memory-safety obligation of crossing a foreign boundary (FFI, transmute) | In function signatures; a `trusted` body absorbs it (R-0484) | `extern fn read(...) with(Console)` requires `with(Unsafe, Console)` outside trusted code |

This split applies uniformly across compiler builtins, stdlib internals, and user code. No layer is silently exempt.

### Capabilities

Capabilities are named, compile-time effects declared in function signatures:

```
fn serve() with(Network, Alloc) -> Int { ... }
```

What each capability means is [TRUST-D1](../spec/trusted.md#TRUST-D1) (`Random` is external
entropy; a seeded PRNG needs none), and capability sets, aliases (`cap IO = File + Console;`) and
`Std` are [TYPE-D1](../spec/types.md#TYPE-D1); the nine capability names and the `Std` alias are
listed in [TRUST-4](../spec/trusted.md#TRUST-4)'s rationale, with the reserved `Device`, `Thread`
and `Signal`. The calling rule, at Check and at CoreCheck, is
[TRUST-4](../spec/trusted.md#TRUST-4), and a trait impl method's capabilities are
[TRAIT-9](../spec/traits.md#TRAIT-9). The heap builtins need `Alloc`
([MEM-7](../spec/memory.md#MEM-7)). See [FFI.md](../platform/FFI.md) for capability aliases in
wrappers.

### Trusted Boundaries

`trusted fn` and `trusted extern fn` contain pointer-level implementation techniques behind safe APIs. A method is trusted when it declares `trusted fn` itself:

```
impl Vec<T> {
    pub trusted fn push(&mut self, value: T) with(Alloc) { ... }
}
```

**What `trusted` is:** An audited containment boundary for pointer-level implementation techniques. It means "this declaration uses implementation techniques ordinary safe code cannot use directly, and that usage is intentionally concentrated and review-relevant."

**What `trusted` permits and does not do** is [TRUST-5](../spec/trusted.md#TRUST-5): raw-pointer
operations and plain `extern fn` calls without declaring `Unsafe` (since R-0484), and nothing else.
It does not suppress capabilities, those an `extern fn` declares included
([FFI-3](../spec/ffi.md#FFI-3)), and it relaxes no ownership, linearity, borrow or scope-exit rule
([TRUST-12](../spec/trusted.md#TRUST-12)), except two ownership exceptions main allows in trusted code, destroying a value through a reference and `vec_free` on a vector whose elements are not Copy, whose future is [OPEN-113](../spec/decisions.md#OPEN-113). Pointer arithmetic is `ptr::offset`, and `as` never
converts between a pointer and an integer ([TRUST-1](../spec/trusted.md#TRUST-1), pending R-0502;
main still accepts `p + n` on a pointer). Converting a reference to a raw pointer needs neither
`trusted` nor `Unsafe` ([TRUST-13](../spec/trusted.md#TRUST-13); its `as` spelling is OPEN-98).
What a `trusted extern fn` asserts is [FFI-9](../spec/ffi.md#FFI-9).

**Syntactic surfaces** (intentionally narrow): `trusted fn` (a function or a method, each declaring
it itself) and `trusted extern fn` (an audited foreign binding). There is no block-level trust:
`trusted impl` is refused ([TRUST-5](../spec/trusted.md#TRUST-5), bug 189). The stdlib demonstrates
the pattern throughout: `Vec<T>`'s and `HashMap<K, V>`'s raw-storage methods, `TextFile`'s I/O
methods, `trusted fn print/println`. See [FFI.md](../platform/FFI.md) for wrapper patterns.

### Unsafe-marked modules and global state (decided 2026-10-29: OD7 and OD8)

The owner decided both on 2026-10-29. A `trusted mod` marker puts the trusted base in the source and
never confers trust by itself ([TRUST-6](../spec/trusted.md#TRUST-6), pending). Global state follows
Austral's capability model: a program has no mutable global state and a constant holds no token
([TRUST-8](../spec/trusted.md#TRUST-8)), global resources are linear tokens descending from the root
value `main` receives, with hardware and runtime singletons minted only by listed `trusted fn`
wrappers ([TRUST-9](../spec/trusted.md#TRUST-9)), and handlers and callbacks hold only the tokens
they are given ([TRUST-10](../spec/trusted.md#TRUST-10)); all pending R-0557. The proposal of
2026-10-23 (declared global cells in marked modules, [TIGHTENING_DECISIONS D8 and
D10](../project/TIGHTENING_DECISIONS.md#46-decisions-of-2026-10-23-second-set)) was rejected for global
state. The compiler's emitted globals (`__concrete_argc`, `__concrete_argv`) and std.env's environment
mutation are inventoried as trusted base.

### `with(Unsafe)`

`with(Unsafe)` gates raw-pointer operations and plain `extern fn` calls outside a `trusted` body
([TRUST-4](../spec/trusted.md#TRUST-4), [TRUST-5](../spec/trusted.md#TRUST-5)); a binding's declared
effects are required everywhere ([FFI-3](../spec/ffi.md#FFI-3)). The audit story is simple:
`grep with(Unsafe)` finds the boundary.

## Linearity and Ownership

Structs and enums are linear unless declared `Copy`
([OWN-D1](../spec/ownership.md#OWN-D1)). The rules are in the ownership chapter: a linear value is
consumed exactly once ([OWN-1](../spec/ownership.md#OWN-1), [OWN-2](../spec/ownership.md#OWN-2)),
branches agree ([OWN-3](../spec/ownership.md#OWN-3)), `defer` registers cleanup
([OWN-12](../spec/ownership.md#OWN-12)), borrow blocks scope references
([REF-D3](../spec/references.md#REF-D3)), and a `Heap<T>` owns its cell
([MEM-5](../spec/memory.md#MEM-5)).

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

These are audit-oriented outputs, not a second semantic pipeline. All modes consume validated compiler artifacts from the existing pipeline. See [PASSES.md](../compiler/PASSES.md) for details and the 59 regression assertions.

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

Capability-related errors include actionable `hint:` text suggesting specific fixes. See [DIAGNOSTICS.md](../compiler/DIAGNOSTICS.md) for the full error model.

## Proof Boundary

The proof boundary sits after CoreCheck and before Mono, materialized as the `ValidatedCore` artifact type. At this point the program is:

- fully elaborated (no surface sugar)
- normalized by CoreCanonicalize
- validated by CoreCheck (capabilities, types, match coverage, declaration legality)

`ProofCore` (`Concrete/Proof/ProofCore.lean`) extracts the authority-free,
proof-eligible fragment from `ValidatedCore`:

**Eligible:** Authority-free functions with no trusted/extern boundary whose
entire body is admitted by the named ProofCore version. An empty capability set
is an authority claim, not purity ([TRUST-4](../spec/trusted.md#TRUST-4)): it implies neither
termination nor absence of traps.

**Excluded:** Functions with capabilities, trusted/unsafe functions, extern functions, entry points.

`Concrete/Proof/Proof.lean` defines formal evaluation semantics for the admitted
authority-free Core fragment. See [PROVABLE_V1.md](PROVABLE_V1.md) for the
canonical admitted surface, [PROVABLE_SUBSET.md](PROVABLE_SUBSET.md) for its
architecture, and [ARCHITECTURE.md](../compiler/ARCHITECTURE.md) for the proof pipeline.

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

The profile direction is documented in detail in [../research/language/high-integrity-profile.md](../../research/language/high-integrity-profile.md).

## How The Pieces Fit Together

```
Capabilities ─── what authority code may use (compile-time, signature-visible)
     │
     ├── cap aliases reduce repetition (cap IO = File + Console)
     ├── Std capability alias expands to hosted standard caps except Unsafe today
     │   (reserved Device authority also stays outside Std if/when added)
     ├── --report caps shows per-function capability requirements
     └── --report authority shows transitive capability chains

Trusted ──────── containment of pointer-level unsafety behind safe APIs
     │
     ├── trusted fn (functions and methods) / trusted extern fn
     ├── callers still see the declared capabilities; only trusted extern fn avoids with(Unsafe)
     ├── --report unsafe shows trust boundaries
     └── wrapper patterns documented in docs/platform/FFI.md

with(Unsafe) ─── explicit foreign boundary gate (TRUST-4)
     │
     ├── required for extern fn calls outside a trusted body (TRUST-5)
     ├── required for raw pointer ops outside trusted code
     ├── grep with(Unsafe) finds the boundary
     └── --report unsafe shows where and why

Linearity ────── compile-time resource safety (OWN-D1, OWN-2)
     │
     ├── structs/enums linear by default, opt-in Copy
     ├── consumed exactly once
     ├── defer for cleanup (OWN-12)
     └── borrow blocks for scoped references (REF-D3)

ProofCore ────── proof-eligible subset of validated Core
     │
     ├── extractable: no declared capability, not trusted, no extern call
     ├── admitted as effect-free only with a complete assumption summary
     │   that reaches no foreign binding (no capability is NOT purity)
     ├── safe algebraic data types
     ├── --report proof shows eligibility
     └── Concrete/Proof/Proof.lean has formal semantics + theorems

High-Integrity ── future profile: same language, stricter rules
     │
     ├── restricted Unsafe/FFI/allocation/authority
     ├── profile-aware compiler checks and reports
     └── maximizes proof eligibility
```

## Cross-References

| Topic | Primary doc | Also mentioned in |
|-------|------------|-------------------|
| Capabilities | This document, [FFI.md](../platform/FFI.md) | [PASSES.md](../compiler/PASSES.md), [STDLIB.md](../stdlib/STDLIB.md) |
| Trusted boundaries | [FFI.md](../platform/FFI.md) | This document, [STDLIB.md](../stdlib/STDLIB.md) |
| Capability aliases | [FFI.md](../platform/FFI.md) | [PASSES.md](../compiler/PASSES.md) |
| Linearity / ownership | [VALUE_MODEL.md](../archive/design-records/VALUE_MODEL.md) | This document |
| Error model | [DIAGNOSTICS.md](../compiler/DIAGNOSTICS.md) | [PASSES.md](../compiler/PASSES.md) |
| Report modes | [PASSES.md](../compiler/PASSES.md) | This document |
| Proof boundary | [ARCHITECTURE.md](../compiler/ARCHITECTURE.md) | This document, [PASSES.md](../compiler/PASSES.md) |
| Execution model | [EXECUTION_MODEL.md](../platform/EXECUTION_MODEL.md) | [STDLIB.md](../stdlib/STDLIB.md) |
| High-integrity profile | This document | [PROJECT.md, Identity](../project/PROJECT.md#identity), [EXECUTION_MODEL.md](../platform/EXECUTION_MODEL.md) |
| ABI / FFI safety | [ABI.md](../platform/ABI.md) | [FFI.md](../platform/FFI.md), [VALUE_MODEL.md](../archive/design-records/VALUE_MODEL.md) |
