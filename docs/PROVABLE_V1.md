# ProvableV1

Status: v1 contract

`ProvableV1` is the first named subset of Concrete where the project is willing
to say: this function can carry Lean-backed evidence, and the compiler has a
defined workflow for keeping that evidence tied to the source.

This is not a whole-language guarantee. It is a deliberately small, auditable
profile for selected pure functions.

## One-Sentence Contract

A `ProvableV1` function is a pure, non-trusted, non-entry function whose
validated Core body extracts to ProofCore, whose registered spec matches the
current source extraction, whose callees are available in the proof function
table, and whose theorem coverage is explicitly classified.

## Claim Classes

Every claim about a `ProvableV1` function must use the project taxonomy:

| Claim | Meaning in `ProvableV1` |
|---|---|
| `proved` | A Lean theorem checks against the registered spec and current body fingerprint. |
| `enforced` | The compiler rejects violations, such as capability widening or ownership errors. |
| `reported` | The compiler surfaces a fact, but does not make it a hard error. |
| `assumed` | The claim depends on an explicit assumption file or trusted boundary. |
| `trusted` | The claim depends on compiler, backend, runtime, target, Lean kernel, or foreign code. |

Do not collapse these. A function can be proof-eligible but unproved. A proved
PExpr theorem is not a binary-correctness theorem.

## Function Eligibility

A function is inside `ProvableV1` only if all of these hold:

| Gate | Requirement |
|---|---|
| Purity | The function has no capabilities. |
| Trust | The function is not `trusted`, not in a trusted impl, and does not cross FFI. |
| Entry point | The function is not an entry point. |
| Calls | Direct calls only; registered proof specs must be FnTable-complete. |
| Recursion | No recursion. |
| Allocation | No heap allocation and no `Alloc` capability. |
| Blocking effects | No `File`, `Network`, `Process`, `Console`, `Clock`, `Random`, or `Env` capability. |
| Raw operations | No raw pointer operations and no `Unsafe` capability. |
| Body | Every body construct must be in the supported ProofCore surface below. |
| Registry | Any proved claim must have `body_fingerprint`, `spec`, `proof`, and `coverage`. |

## Supported Types

`ProvableV1` supports the types needed by the four graduated flagships:

| Type form | Status |
|---|---|
| `Int` / integer-like scalar values | Supported in the PExpr value model. |
| `Bool` | Supported. |
| Fixed-width integer operations | Supported where the extractor records width and signedness. Current widths: `i32`, `u32`, `u8` for the operations already forced by flagships. |
| Struct values | Supported as algebraic values. |
| Enum values | Supported as algebraic tagged values. |
| Fixed arrays | Supported as ProofCore array values for reads, literals, and functional updates. |

Excluded from `ProvableV1`:

- strings and text APIs
- heap-owning values
- raw pointers
- references and borrow semantics
- function pointers, closures, trait objects
- generic proof obligations not monomorphized into an explicit proof target
- layout-sensitive `repr(C)` / packed / FFI values

## Supported Expressions

The current `ProvableV1` ProofCore expression surface is:

| Construct | Status |
|---|---|
| integer and boolean literals | Supported. |
| variables | Supported. |
| arithmetic and comparisons | Supported for the operations admitted by `PBinOp`. |
| `mod` | Supported for the width/signedness cases recorded in ProofCore. |
| bitxor / bitor | Supported for currently forced widths: `i32`, `u32`, `u8` as applicable. |
| bitand | Supported at `u32` (unsigned view), forced by HMAC-SHA256's `Ch`/`Maj`. Other widths are append-only follow-ups. |
| shr (logical right shift) | Supported at `u32` (unsigned view, models LLVM `lshr`), forced by HMAC-SHA256's `sigma` functions and `rotr`. Other widths / arithmetic `ashr` are append-only follow-ups. |
| let bindings | Supported. |
| if/then/else and early-return fall-through shape | Supported. |
| non-recursive direct calls | Supported when FnTable-complete. |
| struct literals and field access | Supported. |
| enum literals and match | Supported for the extracted pattern forms. |
| casts | Supported for the current identity/widening proof model; narrowing remains outside the stable claim. |
| array literals | Supported. |
| array reads | Supported; out of bounds evaluates to `none` in the proof model. |
| array functional update | Supported via `arraySet`; out of bounds evaluates to `none`. |
| bounded flat-assignment while loops | Supported through `while_` with fuel in the proof evaluator and boundedness checked by profile tooling. |
| richer loop body with Cont/Break | Supported through `while_step` and the `LoopStep` enum encoding. |

Excluded from `ProvableV1` until explicitly added:

- `shl` (left shift) — forced next by HMAC-SHA256's `rotr` (the last blocked helper)
- shifts at widths other than `u32`; arithmetic right shift (`ashr`)
- bitand at widths other than `u32`
- rotations
- multi-word arithmetic (`u32` wrapping `add` not yet width-tagged)
- arbitrary mutation outside the modeled state forms
- arbitrary loop invariants
- recursive functions
- exceptions or panic-like recovery semantics
- string/char operations
- allocation, FFI, raw pointer operations, or effectful calls

## Statements And State

`ProvableV1` is not restricted to purely scalar expressions. It includes a
small functional state model:

- local `let` bindings become `letIn`
- loop-carried variable updates become environment rebinding
- array assignment becomes `arraySet` plus rebinding of the array name
- richer loop exits use `LoopStep::Cont` / `LoopStep::Break`

This is the state model used by `fixed_capacity`. It is intentionally
functional: mutation is modeled by producing a new value, not by aliasing heap
state.

## Runtime And Failure Model

The proof evaluator returns `some value` or `none`.

In `ProvableV1`, `none` means the proof model got stuck:

- out-of-bounds array read or update
- missing callee in the proof FnTable
- unsupported value shape for an operation
- insufficient fuel
- unsupported extraction shape

The current V1 contract does not yet generate a separate runtime-error
obligation for every possible stuck case. That is the job of the runtime
obligation phase. Until then, theorems must either avoid stuck cases by
construction or state the needed hypotheses explicitly.

## Proof Attachment Requirements

A proved `ProvableV1` claim requires all of these:

1. The source function is proof-eligible.
2. `cExprToPExpr` extracts the current body.
3. The proof registry has a matching `body_fingerprint`.
4. The registered spec normalizes to the same PExpr as the source extraction.
5. The registered proof name resolves to a Lean theorem built by `make build`.
6. The registered FnTable resolves every direct call in the spec.
7. The theorem has a `coverage` classification:
   `point`, `one_direction`, `iff`, `invariant`, `runtime_error`, or
   `full_contract`.

The coverage classification is load-bearing. A `point` proof and an `iff`
theorem are both kernel-checked, but they are not equally strong claims.

## Current Examples

| Example | `ProvableV1` role |
|---|---|
| `parse_validate` | Parser/validator proof surface, structs/enums, arrays, match, checksum loop. |
| `crypto_verify` | Toy proof-scaffolding example; explicitly not real crypto. |
| `fixed_capacity` | Bounded mutable state, array update, while_step, composition over state. |
| `constant_time_tag` | Narrow real-crypto-adjacent byte-array comparison, u8 bitwise ops, honest timing assumption. |

These examples are not proof-complete. Their proof coverage is classified in
`--report proof-status` and in `tests/showcase/manifest.toml`.

## Non-Claims

`ProvableV1` does not claim:

- the whole compiler is proved correct
- generated binaries preserve every PExpr theorem
- LLVM, clang, linker, libc, OS, or hardware behavior is verified
- machine-level constant-time behavior is proved
- all proof-eligible functions are proved
- all proved functions have full functional specifications
- source contracts exist yet
- runtime-error obligations are systematically generated yet
- borrow/reference semantics are in the proof model

The current proof bridge has started narrowing the trusted compiler gap, but
`ProvableV1` is still a source/ProofCore-level claim under explicit trusted
components.

## Compatibility Rule

`ProvableV1` can grow, but it must not silently change meaning.

Adding a construct to `ProvableV1` requires:

1. a forcing example or obligation,
2. a ProofCore extraction rule,
3. an entry in `docs/PROOF_OBLIGATIONS_REGISTER.md`,
4. tests or theorems that exercise the new rule,
5. a clear statement of the claim class,
6. negative examples if the construct has excluded forms,
7. updated docs and release evidence.

Removing or weakening a supported construct is a compatibility break and must
show up in the roadmap, changelog, and release notes.

## Direction

`ProvableV1` is not the destination.  The long-term goal is for
the provable subset to expand toward the whole language: every
construct that does not violate the proof-evidence preconditions
(purity boundary, capability gating, FFI honesty, no hidden
state) should eventually be a candidate.  Each expansion follows
the Compatibility Rule above — forcing example, ProofCore rule,
register entry, theorems, claim class, negative examples if
needed, doc updates.

Some constructs may stay outside indefinitely: FFI bodies, raw
pointer arithmetic, unrestrained loops, dynamic dispatch on
non-monomorphized targets.  That is a property of those
constructs' semantics, not a limitation of the provable-subset
machinery.

`ProvableV1` is the smallest subset that meets the criteria
today and is wired into the flagship corpus.  Future subsets
(`ProvableV2`, etc.) will be additive and named.  A subset
narrowing is a compatibility break.

For the whole-language version of this principle, see
[PROOF_STORY_MATRIX.md](PROOF_STORY_MATRIX.md): every construct should be
proved, enforced, reported, assumed, trusted, or explicitly open.

## Relation To Other Documents

- `docs/PROOF_STORY_MATRIX.md` is the per-construct proof/evidence inventory.
- `docs/PROVABLE_SUBSET.md` is the broader background document.
- `docs/PROOF_OBLIGATIONS_REGISTER.md` records per-construct soundness debt.
- `docs/PROOF_STATE_MODEL.md` defines the functional mutation model.
- `docs/CLAIM_TAXONOMY.md` defines the claim vocabulary.
- `docs/THREAT_MODEL.md` explains what the evidence pipeline catches.
- `docs/CLAIMS_TODAY.md` is the public claim summary.
