# Language Semantics vs Proof Semantics Boundary

Status: canonical reference — defines exactly where the proof model matches the language, where it is intentionally narrower, and how users should read "proved" relative to ordinary language execution.

For the public guarantee tiers, see [GUARANTEE_STATEMENT.md](GUARANTEE_STATEMENT.md).
For effect/trust proof boundaries, see [EFFECT_PROOF_BOUNDARIES.md](EFFECT_PROOF_BOUNDARIES.md).
For the provable subset definition, see [PROVABLE_SUBSET.md](PROVABLE_SUBSET.md).

---

## 1. Two Semantic Models

Concrete has two semantic models, and they are not the same:

**Language semantics** — the meaning of a Concrete program as defined by the checker (`Check.lean`), elaboration (`Elab.lean`), Core IR (`Core.lean`), lowering (`Lower.lean`), and LLVM codegen (`EmitSSA.lean`). This is what runs on the machine.

**Proof semantics** — the meaning of a proof-eligible function as defined by `PExpr` evaluation in `Proof.lean`. This is what Lean theorems reason about.

The proof model is a strict subset of the language model. Every construct in PExpr has a corresponding construct in CExpr, but not every CExpr construct has a PExpr equivalent. Where the two models overlap, they are intended to agree — but there is one known semantic gap (integer representation) and several constructs where the proof model simply has nothing to say.

---

## 2. Where They Are Identical

For the following constructs, language semantics and proof semantics agree exactly:

| Construct | Language behavior | Proof behavior | Agreement |
|-----------|------------------|----------------|-----------|
| Boolean literals | `true`/`false` values | `PVal.bool true`/`PVal.bool false` | Exact |
| Boolean equality/inequality | `==`, `!=` on bools | `PBinOp.eq`, `PBinOp.ne` on `PVal.bool` | Exact |
| Comparison operators | `<`, `<=`, `>`, `>=`, `==`, `!=` on integers | Same operators on `PVal.int` | Exact (within representable range) |
| Let bindings | Immutable local binding, scoped | `PExpr.letIn` with `Env.bind` | Exact |
| If/then/else | Branch on boolean condition | `PExpr.ifThenElse` with `PVal.bool` | Exact |
| Function calls | By-value argument passing, fresh scope | `PExpr.call` with `FnTable` lookup, `Env.empty` per call | Exact (for pure, non-recursive calls) |
| Variable lookup | Identifier resolves to bound value | `env name` returns `Option PVal` | Exact |

These constructs are safe to reason about in Lean with the expectation that the proof result matches runtime behavior.

---

## 3. Where They Diverge: The Integer Gap

**This is the most important semantic divergence.**

| Property | Language semantics | Proof semantics |
|----------|-------------------|-----------------|
| Integer type | Fixed-width (64-bit `Int`, 32-bit `i32`, etc.) | Lean's unbounded `Int` (arbitrary precision) |
| Overflow behavior | Wraps silently (two's-complement) | No overflow — arithmetic is exact |
| Division/modulo | Supported (`/`, `%`) | **Not modeled** (not in `PBinOp`) |

**What this means:**

A Lean theorem proving `abs(x) ≥ 0` holds for all mathematical integers, but the compiled program uses 64-bit integers where `abs(Int.MIN)` overflows to `Int.MIN` (which is negative). The proof is true in the proof model but not true for all runtime inputs.

**How users should read this:**

Proofs in Concrete's current proof model are valid for inputs within the representable range of the target integer type. They do not cover overflow behavior. This is an honest gap, documented here, not a hidden assumption.

**Future direction:** Two options exist:
1. Add overflow preconditions to proof obligations (prove `f(x) = y` only when inputs are in range)
2. Model fixed-width arithmetic in PExpr (wrap semantics in the proof model)

Neither is implemented. Until one is, proofs carry an implicit assumption that integer operations do not overflow.

---

## 4. Where the Proof Model Is Intentionally Narrower

The proof model does not attempt to cover the full language. These constructs exist in the language but have no proof-model equivalent:

### Constructs with no PExpr representation

| Language construct | CExpr node | Why excluded from proofs |
|-------------------|------------|------------------------|
| Float literals and arithmetic | `.floatLit` | No formal float model |
| String and character literals | `.strLit`, `.charLit` | No formal string model |
| Struct/enum construction | `.structLit`, `.enumLit` | Algebraic data not yet formalized |
| Field access | `.fieldAccess` | Depends on struct formalization |
| Pattern matching | `.match_` | Depends on enum formalization |
| Borrow/borrow_mut | `.borrow`, `.borrowMut` | Memory/reference model not in proof semantics |
| Dereference | `.deref` | Memory/reference model not in proof semantics |
| Array literals and indexing | `.arrayLit`, `.arrayIndex` | No formal array model |
| Type casts | `.cast` | No formal cast model |
| Function references | `.fnRef` | No first-class function model |
| Try expressions | `.try_` | Error propagation not modeled |
| Heap allocation | `.allocCall` | Effectful; may never enter proof subset |
| While loops | `.whileExpr` | Loop semantics not formalized |
| Unary operators | `.unaryOp` | Negation, bitwise NOT not modeled |
| Division, modulo | `BinOp.div`, `BinOp.mod` | Partial operations (division by zero) not modeled |
| Bitwise operators | `BinOp.bitAnd`, etc. | Bit-level semantics not modeled |
| Logical AND/OR | `BinOp.and`, `BinOp.or` | Short-circuit semantics not modeled |

### Statements with no proof representation

| Language statement | Why excluded |
|-------------------|-------------|
| Assignment (`x = val`) | Mutation not modeled |
| Field/deref/array assignment | Mutation not modeled |
| While loops | Loop semantics not formalized |
| Break/continue | Control flow not modeled |
| Defer | Cleanup semantics not modeled |
| Borrow regions | Reference semantics not modeled |

### Effectful code

Any function with capabilities (`with(File, Alloc, ...)`) is excluded at the eligibility gate before extraction is attempted. The proof model has no effect system — it reasons about pure return values only.

### Trusted code

Any function marked `trusted` or originating from a `trusted impl` block is excluded at the eligibility gate. The proof model has no raw-pointer semantics.

---

## 5. How Users Should Read "Proved"

When a function carries a proof, this is what it means:

**"The function's PExpr representation, evaluated with Lean's unbounded integer arithmetic and pure functional semantics, satisfies the stated theorem."**

Specifically:

1. **The proof is about PExpr, not about the compiled binary.** The theorem is proved over the `eval` function in `Proof.lean`, which interprets PExpr in a pure functional model. It does not reason about LLVM IR, machine code, or runtime behavior directly.

2. **The proof assumes no integer overflow.** PExpr uses Lean's `Int` (arbitrary precision). The compiled code uses fixed-width integers. The proof holds for inputs within the representable range.

3. **The proof covers the function in isolation.** Proofs are per-function. A proof of `f` says nothing about callers of `f` or about the composition `f(g(x))` unless both `f` and `g` are independently proved and the composition is explicitly proved.

4. **The proof assumes the function table is correct.** If `f` calls `g`, the proof assumes `g`'s PExpr representation is in the `FnTable` and that `g` behaves as its PExpr says. There is no verified linking step between function proofs.

5. **Stale detection is mechanical.** If the function body changes, its fingerprint changes, and the proof is marked stale. The compiler does not re-verify the proof — it reports staleness and revokes the "proved" evidence level.

### What "proved" does NOT mean

- It does not mean the compiled binary is correct for all inputs (overflow gap).
- It does not mean the function is safe (safety is a checker property, not a proof property).
- It does not mean the compiler itself is correct (no compiler-preservation proof exists).
- It does not mean the proof is complete (the proof covers the stated property, not all possible properties).

---

## 6. The Extraction Bridge

The bridge between language semantics and proof semantics is `cExprToPExpr` in `ProofCore.lean` (lines 635–668). This function translates Core IR expressions (`CExpr`) into proof expressions (`PExpr`).

### What the bridge preserves

- **Structure.** The PExpr tree mirrors the CExpr tree for supported constructs. A `CExpr.binOp .add lhs rhs` becomes `PExpr.binOp .add (translate lhs) (translate rhs)`.
- **Names.** Variable names and function names are preserved verbatim.
- **Control flow.** If/then/else and let-binding structure is preserved.

### What the bridge discards

- **Type information.** CExpr carries `Ty` on most nodes; PExpr does not. The proof model is untyped — `PVal.int` and `PVal.bool` are the only value kinds.
- **Type arguments.** Generic instantiation information is dropped (proofs are pre-monomorphization).
- **Spans.** Source location information is not preserved in PExpr.

### What the bridge cannot represent

Any CExpr node not listed in the supported set returns `none`, causing the entire function to fail extraction. This is all-or-nothing: a function is either fully extractable or not extractable at all. There is no partial extraction.

---

## 7. Backend and Target Assumptions

The proof model sits above the backend entirely. Between the proof model and actual execution, these unverified steps exist:

```
PExpr (proof model)
  │
  ├── assumed equivalent to ──→ CExpr (Core IR)
  │                               │
  │                          Lower.lean
  │                               │
  │                          SSA IR
  │                               │
  │                          EmitSSA.lean
  │                               │
  │                          LLVM IR
  │                               │
  │                          LLVM optimizer + codegen
  │                               │
  │                          Machine code
  │                               │
  │                          OS / hardware
  └── proof says nothing about any of these steps
```

Each step introduces assumptions:

| Step | Assumption |
|------|------------|
| CExpr → PExpr | `cExprToPExpr` is structure-preserving (not formally verified) |
| CExpr → SSA | `Lower.lean` preserves semantics (not formally verified) |
| SSA → LLVM IR | `EmitSSA.lean` generates correct LLVM IR (not formally verified) |
| LLVM IR → machine code | LLVM optimizer and codegen are correct |
| Machine code → behavior | OS, hardware, and runtime behave as expected |

A compiler-preservation proof (CExpr → SSA or SSA → LLVM IR) would strengthen the chain. This is future work tracked in the roadmap.

---

## 8. Summary: Reading the Boundary

| Question | Answer |
|----------|--------|
| Does "proved" mean the binary is correct? | No. It means the PExpr model satisfies the theorem. |
| Does "proved" cover overflow? | No. PExpr uses unbounded integers. |
| Does "proved" mean the function is safe? | No. Safety is checker-enforced, not proof-backed. |
| Can I trust a proof of a pure function? | Yes, for inputs within the integer range, assuming the extraction bridge is correct. |
| What happens when I change the function? | The fingerprint changes, the proof becomes stale, and the evidence level drops to "enforced". |
| Can proofs compose across functions? | Not yet. Proofs are per-function. |
| What constructs can be proved? | Integer/boolean arithmetic, comparisons, let bindings, if/then/else, non-recursive calls. |
| What about structs, enums, loops? | Not yet formalized. Functions using these constructs cannot be extracted. |
| Where does the proof model end? | At the PExpr → CExpr bridge. Everything below (lowering, codegen, runtime) is unverified. |
