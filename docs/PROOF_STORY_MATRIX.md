# Proof Story Matrix

Status: stable reference

Concrete's long-term goal is not that every program is pure, or that every
construct fits into `ProvableV1`. The goal is that no language construct is
semantically dark.

A construct is not semantically dark when the project can answer:

1. what claim class applies to it,
2. what evidence supports that claim,
3. what remains assumed or trusted,
4. what would have to change for the claim to become stronger.

This matrix is the language-level inventory. `ProvableV1` is the subset of rows
that currently have a value-semantics proof story. Other rows may be enforced,
reported, assumed, or trusted instead.

## Claim Classes

| Class | Meaning |
|---|---|
| `proved` | A Lean theorem checks against an extracted model and current fingerprint. |
| `enforced` | The compiler rejects violations before code generation. |
| `reported` | The compiler reports the fact, but does not make it a hard error. |
| `assumed` | The claim is explicit in assumptions/policy/release evidence. |
| `trusted` | The claim depends on compiler, backend, runtime, target, Lean kernel, or foreign code. |
| `open` | The construct has a named gap and must not be presented as covered. |

## Matrix

| Construct / surface | Current story | Evidence today | Named gap / next strengthening |
|---|---|---|---|
| Integer and boolean literals | proved | R-01/R-02 soundness rules; PExpr eval theorems | deeper compiler-wide preservation remains Phase 8 work |
| Variables | proved | R-03 soundness rule | none for current ProofCore surface |
| Arithmetic and comparisons | proved for admitted `PBinOp` cases | R-04/R-05; proof-status and flagship specs | richer arithmetic profiles still need `PredictableV1`/runtime obligations |
| Width-tagged mod / bitxor / bitor / bitand / shr / shl / addw | proved at extraction shape; trusted for backend equivalence | R-16/R-17/R-21/R-22/R-23/R-24/R-25/R-26 register entries; BitVec eval rules | BitVec-vs-LLVM semantic agreement; more widths/ops by forcing example |
| Let bindings | proved | R-06; PExpr eval theorem pattern | none for current shape |
| If/then/else and early-return fall-through | proved at extraction/eval shape | R-07; parse_validate proofs | fuller source semantics for all if forms |
| Direct non-recursive calls | proved at extraction/eval shape when FnTable-complete | R-08; G-05 direct FnTable checks | transitive FnTable and proof dependency tracking |
| Struct literals and field access | proved at extraction/eval shape | R-09/R-10; parse_validate/fixed_capacity proofs | richer layout-sensitive structs remain excluded |
| Enum literals and match | proved at extraction/eval shape | R-11/R-12; parse_validate proofs | per-arm semantic preservation and larger pattern surface |
| Casts | proved for current identity/widening proof model | R-14; fixed_capacity specs | narrowing/invalid-cast obligations |
| Fixed array literals and reads | proved at extraction/eval shape | R-13/R-15; fixed_capacity/constant_time_tag | bounds obligations for source-level runtime safety |
| Functional array update | proved at extraction/eval shape | R-19; fixed_capacity ring_push theorem | bounds obligations and richer mutation shapes |
| Bounded flat-assignment while loops | proved at extraction/eval shape | R-18; compute_tag/checksum extraction | loop bound/variant obligations |
| Rich loop bodies via `while_step` | proved at extraction shape; partially proved at eval shape | R-20; fixed_capacity ring_contains theorem | multi-iteration invariants and source-semantics preservation |
| Linear ownership | enforced | checker rejects use-after-move, double-use, leaks, branch disagreement | formal checker soundness is open |
| Borrows and `&mut` exclusivity | enforced | borrow-block checker, frozen-owner rules | proof model for references/borrows is open |
| Capabilities in signatures | enforced | caller must declare superset of callee capabilities | effectful proof judgments are open |
| Allocation | reported/enforced by policy, not proved | `--report alloc`, policies, assumptions | `PredictableV1` allocation budget and obligations |
| Stack depth | reported | `--report stack-depth` | source-vs-target stack contract and hard gates |
| Runtime failures: OOB/div-zero/overflow/cast | open/reported | scattered diagnostics and proof-model `none` | Phase 3 runtime safety obligations |
| Source contracts | open | none yet | Phase 4 attributes and obligation generation |
| Strings/text | open | interpreter coverage exists for literals; ProofCore excludes text APIs | string/text proof model |
| Heap-owning values | open/assumed by profile | ownership checker enforces linear use; ProofCore excludes heap state | resource semantics and allocation obligations |
| Raw pointers / `Unsafe` | trusted | visible `Unsafe` capability and trust reports | wrapper-specific assumptions; no general proof story |
| FFI bodies | trusted/assumed | extern/trusted boundary reports, assumption files | ABI/layout and wrapper contracts |
| `trusted fn` / trusted impl | trusted/assumed | `--report unsafe`, assumptions, policy gates | smaller wrappers and stronger audit evidence |
| Backend lowering and LLVM/toolchain | trusted | verify gates, wrong-code corpus, bundles | Phase 8/9 compiler/backend soundness work |
| Machine-level constant time | assumed | constant_time_tag assumption files and manifest limits | source-level constant-time profile, backend timing assumptions |
| Concurrency / async / threads / channels | open | research notes only | Phase 13 design, simulation, and evidence model |
| Packages/dependencies | open | single-repo evidence today | Phase 11 dependency evidence |

## How To Use The Matrix

Every new language feature, ProofCore rule, runtime profile, contract form, or
flagship-forced construct must update this matrix.

The rule is simple:

- if the construct is proved, link the theorem or obligation register entry;
- if it is enforced, name the checker or gate;
- if it is reported, name the report;
- if it is assumed, name the assumption artifact;
- if it is trusted, name the trusted boundary;
- if it is open, name the gap and keep the claim out of release material.

A construct with no row is a documentation bug. A release claim that depends on
an `open` row is a release bug.

## Relationship To Audit

The future `concrete audit` command should render this matrix specialized to a
program:

- list every construct the program uses,
- map each construct to its claim class,
- link the exact proof, check, report, assumption, or trusted boundary,
- show any rows that remain open,
- fail policy if a release profile disallows those open rows.

That is the practical form of "no semantically dark constructs": not an essay,
but a per-program fact table.

## Relationship To ProvableV1

`ProvableV1` is the first named subset where selected rows in this matrix have
enough proof/evidence to support Lean-backed user-code theorems.

Future subsets should not replace this matrix. They should move rows from
`open` to `reported`, from `reported` to `enforced`, from `assumed` to `proved`,
or from coarse `trusted` to smaller explicit assumptions.
