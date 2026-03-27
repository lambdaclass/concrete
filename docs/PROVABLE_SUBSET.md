# Provable Subset

Status: standing reference

This document defines the clearly analyzable, proof-eligible subset of Concrete. It specifies what is included, what is excluded, why, and how the subset connects to the proof pipeline.

For the safety model, see [SAFETY.md](SAFETY.md). For the proof architecture, see [ARCHITECTURE.md](ARCHITECTURE.md). For the formalization roadmap, see [../research/proof-evidence/formalization-breakdown.md](../research/proof-evidence/formalization-breakdown.md). For language decisions that shape this subset, see [DECISIONS.md](DECISIONS.md).

## Definition

The provable subset is the fragment of Concrete programs that can be formally reasoned about in Lean 4. It currently has two related boundaries:

1. **ProofCore** (`Concrete/ProofCore.lean`) — the actual compiler extractor over validated Core IR
2. **`--report proof`** (`Concrete/Report.lean`) — a stricter report that approximates which functions are currently good proof targets
3. **Proof** (`Concrete/Proof.lean`) — formal evaluation semantics and proved properties over the proof fragment

ProofCore does not define its own semantics. It is a filter, not a rival IR. The semantic authority remains CoreCheck; ProofCore identifies which validated Core declarations the Lean proof infrastructure can reason about.

The important distinction is:

- **ProofCore extraction** is the real compiler boundary today.
- **`--report proof`** is a stricter operational heuristic that also flags extern calls and raw-pointer operations in function bodies.

This document describes both, and labels them explicitly.

## What Is Included

### Functions

#### Current ProofCore extractor

A function is proof-eligible when all of the following hold:

| Criterion | Check | Rationale |
|-----------|-------|-----------|
| Empty capability set | `f.capSet.isEmpty` | No side effects — pure computation only |
| Not trusted | `!f.isTrusted` | No pointer-level implementation techniques |
| Not an entry point | `!f.isEntryPoint` | Entry points have runtime obligations (exit code, setup) |
| No trusted impl origin | `f.trustedImplOrigin.isNone` | Not inherited from a trusted impl block |

A ProofCore-eligible function is intended to be a pure transformation: it takes values, computes, and returns a value. The current extractor enforces the signature-level and declaration-level conditions above. It does not yet inspect function bodies for extern calls or raw-pointer operations.

#### Stricter `--report proof` heuristic

The `--report proof` mode applies the same base conditions and additionally excludes functions that:

- call extern functions
- perform raw pointer operations in their bodies

That makes the report a better approximation of today's practical proof targets than the current extractor alone.

### Types

| Declaration | Eligible when | Rationale |
|-------------|--------------|-----------|
| Struct | Not `repr(C)`, not `packed`, no `reprAlign` | No FFI layout constraints — pure algebraic structure |
| Enum | No `builtinId` | Not a compiler-intercepted builtin — ordinary algebraic data |
| Trait definition | Always included | Provides context for method resolution (not yet verified) |

### Expressions (formalized in Proof.lean)

The current formal semantics covers:

| Construct | Status |
|-----------|--------|
| Integer literals and arithmetic (add, sub, mul) | Formalized |
| Boolean literals and logical operators | Formalized |
| Comparison operators (eq, ne, lt, le, gt, ge) | Formalized |
| Let bindings | Formalized |
| If/then/else | Formalized |
| Non-recursive function calls | Formalized |
| Structs and field access | Not yet formalized |
| Enums and match | Not yet formalized |
| While loops | Not yet formalized |
| Recursive functions | Not yet formalized (uses fuel for termination) |
| String/char operations | Not yet formalized |

## What Is Excluded

### Always excluded by the current ProofCore extractor

| Declaration | Why |
|-------------|-----|
| Functions with capabilities | Side effects make formal reasoning about return values insufficient — the function's meaning includes its effects |
| Trusted functions | Contain pointer-level techniques that are outside the formal model |
| Entry points | Runtime setup/teardown obligations, not pure computation |
| `repr(C)` / packed structs | Layout is dictated by FFI constraints, not algebraic structure |
| Builtin-overridden enums | Compiler-intercepted semantics, not user-defined algebraic data |

### Additionally excluded by `--report proof`

| Construct | Why |
|-----------|-----|
| Functions that call extern functions | Their behavior depends on external code not modeled in Proof.lean |
| Functions with raw pointer operations in their bodies | These are a stronger practical signal that the function is outside today's proof-oriented fragment |

### Not yet modeled (future work)

| Construct | Blocking issue |
|-----------|---------------|
| Structs and enums in proof semantics | Needs PExpr extensions for constructors, field access, match |
| Recursive functions | Needs termination discipline beyond fuel |
| While loops with break/continue | Needs loop semantics formalization |
| String/char operations | Needs string model |
| Heap operations | Fundamentally effectful — may never enter the pure subset |
| Raw pointer operations | Same — trusted/unsafe territory |

## Pipeline Position

```
Source → Parse → Resolve → Check → Elab → CoreCanonicalize → CoreCheck → [ValidatedCore]
                                                                              │
                                                                    extractProofCore
                                                                              │
                                                                        [ProofCore]
                                                                              │
                                                                     Proof.lean semantics
                                                                              │
                                                                      Lean 4 theorems
```

The proof boundary sits after CoreCheck and before Mono. At this point the program is:

- Fully elaborated (no surface sugar)
- Normalized by CoreCanonicalize
- Validated by CoreCheck (capabilities, types, match coverage, declaration legality)

ProofCore extraction happens at the `ValidatedCore` stage, not after monomorphization. This means proof-eligible functions are proved over their pre-monomorphization form. Generic functions that are proof-eligible in principle would need to be proved per-instantiation after mono — this is future work.

## Proved Properties (Current)

`Concrete/Proof.lean` contains 17 theorems:

**Concrete function correctness:**
- `abs_positive`, `abs_negative`, `abs_zero` — abs function returns correct values
- `max_right`, `max_left`, `max_self` — max function returns correct values
- `clamp_in_range`, `clamp_below`, `clamp_above` — clamp function returns correct values

**Structural lemmas:**
- `eval_lit` — integer literals evaluate to themselves
- `eval_bool_lit` — boolean literals evaluate to themselves
- `eval_var_bound` — variable lookup succeeds when bound
- `eval_if_true`, `eval_if_false` — conditional reduction
- `eval_add_lits`, `eval_sub_lits`, `eval_mul_lits` — arithmetic on literals

All proofs use either `native_decide` (kernel reduction) or `simp` (structural simplification).

## Tooling

### `--report proof`

Shows which functions in a program are likely practical proof targets and why excluded functions are excluded:

```
$ concrete program.con --report proof
=== Proof Eligibility Report ===

module main:
  ✓ pure_add
  ✓ factorial
  ✗ read_file  (requires capabilities: File)
  ✗ main  (requires capabilities: Alloc, Console)
  ✗ unsafe_cast  (trusted boundary)

Totals: 5 functions, 2 eligible for ProofCore, 3 excluded
```

Exclusion reasons are specific: "requires capabilities: X", "trusted boundary", "calls extern: ...", "raw pointer operations".

## Relationship to Other Subsets

### ProofCore vs High-Integrity Profile

The **provable subset** (ProofCore) and the **high-integrity profile** (see [SAFETY.md](SAFETY.md)) are related but different:

| | Provable subset | High-integrity profile |
|---|---|---|
| **Purpose** | Formal verification in Lean | Stricter runtime restrictions for critical code |
| **Scope** | Pure functions only | Entire programs under tighter rules |
| **Capabilities** | None allowed | Restricted set (no Unsafe, bounded allocation) |
| **Trusted** | Excluded | Allowed but restricted to approved wrappers |
| **FFI** | Excluded | Allowed through approved `trusted extern fn` only |
| **Enforcement** | ProofCore extraction filter (with stricter `--report proof` heuristic today) | Future compiler mode/checks |

The provable subset is strictly narrower than the high-integrity profile. Every proof-eligible function would also pass high-integrity restrictions, but many high-integrity functions (those with restricted capabilities or approved trusted wrappers) are not proof-eligible.

### ProofCore vs the rest of the program

ProofCore is not a wall. It is a window.

Programs are not expected to be entirely proof-eligible. The typical pattern is:

- Most of the program uses capabilities, trusted wrappers, and FFI normally
- Selected pure functions (algorithms, validators, transformers) are proof-eligible
- The `--report proof` tool shows what is and is not eligible
- Proofs target the eligible fragment; the rest is covered by testing, reports, and audits

## Design Constraints

The provable subset's shape follows directly from Concrete's permanent language decisions:

1. **No closures / no trait objects** → all dispatch in proof-eligible code is statically resolved. No hidden function pointers or vtables to reason about.
2. **No inference-heavy abstraction** → types are explicit. The proof model does not need to reconstruct inferred types.
3. **Trusted = pointer containment only** → the boundary between provable and non-provable code is clean. Trusted functions are excluded because they use pointer techniques, not because of vague "unsafety."
4. **Capabilities in signatures** → the eligibility check is trivial: `capSet.isEmpty`. No need to analyze function bodies for hidden effects.
5. **Phase separation** → ProofCore extracts from a well-defined pipeline stage (ValidatedCore) with known invariants. It does not depend on information from later passes.

## Future Direction

Tracked in [../research/proof-evidence/formalization-breakdown.md](../research/proof-evidence/formalization-breakdown.md). The main expansion axes are:

1. **Richer proof semantics** — structs, enums, match, recursion in `Proof.lean`
2. **Source-to-Core traceability** — connecting proof results back to user-visible source locations
3. **Language guarantee proofs** — proving that capability checking is sound, linearity is enforced, trusted boundaries are honest
4. **Compiler preservation proofs** — proving that Core→SSA lowering preserves the semantics of proof-eligible functions
5. **User-facing proof workflow** — exporting proof subjects, documenting proof artifacts, tying proofs to `--report proof` output

Each level builds on the previous. The current state (Level 1: formal semantics for a small pure fragment) is the foundation. See the formalization breakdown for the full roadmap.
