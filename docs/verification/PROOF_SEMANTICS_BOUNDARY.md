# Language Semantics vs Proof Semantics Boundary

Status: canonical reference — defines where the proof model matches the
language, where it is intentionally narrower, and how users should read
`proved` relative to ordinary execution.

For the public guarantee tiers, see
[GUARANTEE_STATEMENT.md](GUARANTEE_STATEMENT.md). For the admitted proof
surface, see [PROVABLE_V1.md](PROVABLE_V1.md). For the per-construct evidence
inventory, see [PROOF_STORY_MATRIX.md](PROOF_STORY_MATRIX.md).

---

## 1. Two Semantic Models

Concrete has two related semantic models:

- **Language semantics** is the behavior accepted by the checker and carried
  through validated Core, lowering, SSA, and a backend to execution.
- **Proof semantics** is the behavior of extracted `PExpr` terms under the
  evaluator in `Concrete/Proof/Proof.lean`.

Proof semantics covers a named subset of the language. It is not a second
definition of the whole language and it is not a semantics-preservation proof
for the compiler. A construct may be accepted by Concrete while being outside
`ProvableV1`, and a theorem about a `PExpr` still depends on the unverified
extraction and compilation bridges described below.

### Identity is not the bridge

The target architecture gives implementations and models distinct
`DefinitionIdentity` and `ModelIdentity` values. An exact generated
implementation reference prevents a proof attachment from silently moving to
another package, declaration, or implementation revision. It does not prove
that the PExpr/model denotes the implementation's behavior.

Accordingly, Concrete retains two independent claims:

```text
implementation/model correspondence
logical validity of a proposition over the model
```

Correspondence requires its own relation (`refines`, `equivalent`,
`abstracts`, and so on), scope, evidence, and disposition. A kernel theorem
over PExpr cannot fill that field. Conversely, a correspondence result does
not prove every proposition stated over the model.

## 2. Established Overlap

The extractor and evaluator currently represent the shapes forced by the
proof corpus, including:

- integer and Boolean literals, variables, comparisons, and selected
  arithmetic and bitwise operations;
- `let`, conditionals, and supported early-return forms;
- non-recursive direct calls when the proof function table is complete;
- selected structs, enums, field access, and pattern matching;
- fixed arrays, reads, and functional updates;
- bounded loop/state encodings used by the graduated examples.

This is an implemented correspondence, backed by extraction tests and Lean
theorems over the evaluator. It is not a proof that every compiler stage
preserves those terms exactly. `PROVABLE_V1.md` is the compatibility contract;
`PROOF_STORY_MATRIX.md` records the evidence and open obligation for each
language family.

Indirect application needs special care. Runtime Core now distinguishes a
direct symbol call from a call through a local callable value. ProofCore can
represent an opaque application, which is necessary for higher-order
theorems, but bug 061 shows that its current term can still conflate a local
parameter application with a global call bearing the same spelling. R-0442
owns that semantic-identity fix. Until it lands, callback proofs are
representative evidence, not a blanket callable-identity theorem.

## 3. Fixed-Width Arithmetic Boundary

| Property | Language execution | Proof semantics |
|---|---|---|
| Ordinary integers | Fixed-width values | Lean `Int` for width-agnostic operations |
| Ordinary overflow | Checked terminal failure | Mathematical arithmetic does not overflow |
| Explicit wrapping | Modular at the named width | Modeled only for admitted width/operation pairs |
| Division/modulo | Checked for zero and signed overflow | Modeled for admitted signedness/width pairs; invalid cases evaluate to `none` |

A theorem over ordinary mathematical addition does not prove that a
fixed-width execution returns normally for every mathematical input. It says
that, when execution stays within the represented operation's domain, the
result satisfies the theorem. If checked execution leaves that domain, it
terminates rather than silently producing a wrapped result.

Where ProofCore records a fixed-width operation explicitly—currently the
width/operation pairs listed in `PROVABLE_V1.md`—the theorem is about that
fixed-width behavior. Arithmetic-dependent claims must therefore name either
their range/no-failure assumptions or the explicit modular operation they
cover. See [ARITHMETIC_POLICY.md](../language/ARITHMETIC_POLICY.md).

## 4. Intentionally Excluded Surface

The canonical admitted surface is `PROVABLE_V1.md`; this document does not
maintain a second constructor-by-constructor allowlist.

Current exclusions include:

- floats, strings, characters, and text APIs;
- allocation, heap-owning values, capabilities, FFI, trusted code, and raw
  pointer operations;
- references, borrows, and alias-sensitive memory semantics;
- `defer` and cleanup behavior;
- recursion and arbitrary unmodeled control flow or mutation;
- layout-sensitive FFI values;
- generic obligations that do not identify an explicit proof target;
- operation widths and failure forms not admitted by `ProvableV1`.

Selected structs, enums, arrays, matches, casts, loops, and functional state
are already admitted. Their presence alone is not a reason to call a function
unprovable.

Extraction is all-or-nothing for a function. If any node is unsupported, the
function does not acquire a proof term by silently omitting that node.

## 5. How to Read `proved`

`proved` means:

> The linked Lean theorem kernel-checks for the registered proof expression
> and coverage class, and the attachment passes the integrity checks
> implemented by this compiler version.

That sentence has important boundaries:

1. **It is a theorem about `PExpr`, not the generated binary.** There is no
   compiler-preservation theorem from validated Core through machine code.
2. **Arithmetic scope must be explicit.** Width-agnostic `Int` operations and
   fixed-width trapping execution are not interchangeable; explicit width
   operations carry their recorded semantics.
3. **Coverage is part of the claim.** A point theorem, one-direction theorem,
   invariant, and full contract are all kernel-checked but prove different
   things.
4. **Composition is not automatic.** The proof function table makes direct
   callees available to evaluation; it does not turn separate function
   theorems into a whole-program theorem. R-0443’s planned authority-path
   certificate is a separately scoped graph predicate, not automatic
   composition of functional theorems.
5. **Freshness is currently partial.** The shipped R-0004 containment refuses
   to report an in-source proof link as proved when no stored fingerprint
   exists. A stored fingerprint still covers the extracted body rather than a
   versioned digest of the full proof subject: signatures, declared types,
   contracts, and transitive dependency roots remain open under bugs 059/060
   and R-0004. The target keeps the semantic subject distinct from its evidence:
   theorem/toolchain/workspace identity and kernel replay live in a receipt
   bound to the subject and dependency root.
6. **Replay has an execution context.** `--report check-proofs` must resolve
   the same Lean workspace and theorem environment in every supported working
   directory. R-0004 owns the current context-sensitive lookup defect.

`proved` does not mean that the checker, extractor, compiler, LLVM, linker,
runtime, OS, or hardware has been proved correct. Those dependencies remain
reported as trust or assumptions.

## 6. The Extraction Bridge

`cExprToPExpr` in `Concrete/Proof/ProofCore.lean` translates supported
validated Core expressions into proof expressions.

The bridge preserves the supported expression structure, normalized callable
form, and the type/width facts represented by the target `PExpr` constructor.
It discards source spans and any language information for which ProofCore has
no semantic representation.

Two identity limitations are explicit:

- proof extraction occurs before whole-program monomorphization, so generic
  source identity and per-instantiation proof identity are not yet a general
  theorem (R-0271);
- callable application identity is incomplete as described by bug 061 and
  R-0442.

Names are display material and lookup keys in parts of the current proof
model; they are not a substitute for binding identity. Principle 12 requires
the bridge to preserve semantic identity even when source names collide or
are alpha-renamed.

## 7. Backend and Target Assumptions

The proof model sits above the backends:

```text
PExpr theorem
    │  unverified extraction correspondence
Validated Core
    │  unverified lowering
SSA
    │  unverified backend emission
LLVM/QBE IR
    │  external optimizer, code generator, linker
machine execution
```

Differential tests, validators, mutation gates, and artifact inspection make
this chain auditable and catch divergences. They do not turn it into a formal
compiler-correctness proof.

| Boundary | Current status |
|---|---|
| Validated Core → PExpr | Implemented and tested; not formally verified |
| Validated Core → SSA | Checked by stage invariants and tests; not formally verified |
| SSA → backend IR | Differential/artifact tested; not formally verified |
| Backend IR → machine code | Trusted external toolchain |
| Execution environment | Target, runtime, OS, and hardware assumptions |

## 8. Summary

| Question | Answer |
|---|---|
| Does `proved` mean the binary is correct? | No. It means the linked theorem holds in ProofCore under its recorded coverage and assumptions. |
| Does it cover overflow? | Only when the theorem names the relevant range/no-failure condition or an admitted fixed-width operation. Ordinary runtime overflow terminates. |
| Does it prove safety? | No. Ownership, capabilities, and related safety properties are checker-enforced evidence. |
| What constructs can be proved? | The named `ProvableV1` surface, including selected aggregates, arrays, matches, loops, and functional state. |
| Do proofs compose automatically? | No. Cross-function and whole-program composition require explicit theorems and dependency integrity. |
| Does a matching fingerprint cover every semantic dependency? | Not yet. Missing fingerprints fail closed, while signatures, contracts, and transitive dependencies remain R-0004 work. |
| Where does the proof claim end? | At the PExpr theorem plus its attachment. The extraction and compiler chain remain explicit trust boundaries. |

## Failure assumptions of proof-backed code

Moved from docs/spec/statements.md ("Abort and failure") (absorbed into the spec's statements chapter, "Abort
and failure"), whose language half is now there.

### What proved code may assume

1. **No capability-driven abort or OOM**: proof-eligible functions are
   authority-free and non-allocating. Checked arithmetic/bounds traps are a
   separate semantic obligation.
2. **No trusted hardware UB from safe operations**: proof-eligible functions
   exclude raw-pointer/FFI behavior; admitted checked operations may still abort
   outside the theorem's modeled domain.
3. **Normal control flow only**: proved functions return normally or via explicit `Result` error — no hidden exit paths
4. **Defer runs if present**: deferred expressions in proved functions always execute (no abort path exists)

### What proved code may NOT assume

1. **Trap-free fixed-width execution**: ordinary arithmetic traps when the
   machine-width result is invalid, while many ProofCore theorems use unbounded
   integers. The theorem needs an explicit range/no-overflow obligation to claim
   normal return for all runtime inputs.
2. **Static array bounds**: safe indexing traps rather than causing UB, but a
   functional theorem does not by itself prove the trap unreachable.
3. **Termination beyond the modeled evaluator/variant**: selected bounded loops
   are admitted, but proof status alone is not a general language termination
   theorem.
4. **No binary correspondence**: the proof is over PExpr (source-level IR), not compiled LLVM IR. Backend transformations are not formally verified.

### Gap summary for proved code

| Property | Proof model | Runtime reality | Gap |
|----------|------------|-----------------|-----|
| Integers | Mostly unbounded, with selected width-aware operators | Fixed-width checked or explicitly wrapping/saturating | Normal-return claim needs matching range/width semantics |
| Array access | Functional get/set for admitted forms | Runtime bounds trap | Trap-unreachability is a separate obligation |
| Control flow | Selected bounded loops and functional state forms | Compiled with LLVM optimizations | Backend not verified |
| Failure | Not modeled | abort/signal/UB possible in theory | Proved functions avoid all sources in practice |
