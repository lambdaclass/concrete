# User-Facing Proof Contract

Status: canonical reference — defines what a proof attachment means, what a
user may rely on, and what the `proved` report state does and does not promise.

For the claim vocabulary, see
[CLAIM_TAXONOMY.md](CLAIM_TAXONOMY.md). For the semantic boundary, see
[PROOF_SEMANTICS_BOUNDARY.md](PROOF_SEMANTICS_BOUNDARY.md). For the admitted
surface, see [PROVABLE_V1.md](PROVABLE_V1.md).

---

## 1. The Artifact

A supportable Lean-backed claim has:

1. a Lean theorem about a named `PExpr` specification;
2. a proof attachment that names the function, specification, theorem, and
   coverage class;
3. a stored digest for the semantic proof subject;
4. successful attachment validation; and
5. a replay receipt from successful Lean-kernel checking.

In-source attachments use `#[spec]`, `#[proof_by]`,
`#[proof_coverage]`, and `#[proof_fingerprint]`. Registry-backed project
entries carry the equivalent stored data.

These roles must not be collapsed:

- `--report proof-status` establishes that an attachment is eligible,
  extractable, linked, and fresh under the compiler's current integrity model;
- `--report check-proofs` establishes that the named theorem resolves and
  kernel-checks in the selected Lean workspace;
- the coverage class states how much the theorem proves.

The current implementation stores a body fingerprint and emits a replay
verdict rather than the complete artifacts in items 3 and 5. R-0004's target
model separates them:

- `ProofSubjectDigest` identifies the semantic subject: qualified function
  identity, typed signature and constraints, capabilities, normalized typed
  body, contracts, selected normalized specification, claim scope/coverage,
  and extraction/schema version;
- the dependency root identifies the subject's transitive proof dependencies;
- `ProofEvidenceReceipt` binds that subject and dependency root to the theorem
  identity and artifact digest, compiler/Lean/ProofKit identities,
  workspace/import closure, replay command, and kernel result.

The theorem and toolchain are evidence about a semantic subject, not components
that redefine the subject.

### 1.1 Typed dependency edges

The dependency root is not a flat list of callees. Each edge carries a KIND, and
the kind is DERIVED from what the theorem actually uses — never declared by the
author, because a declaration can assert a relationship the proof does not have:

| edge | the caller relies on | invalidated by |
| --- | --- | --- |
| `contract` | the callee's proved contract | that contract, or the callee's receipt, changing |
| `body` | the exact callee implementation | the callee's body / type / semantic digest changing |
| `trusted` | a declared trust boundary | the boundary changing; the trust also PROPAGATES |
| `missing` | nothing validated | always: the caller is `deps_not_current` |

The discriminator between `contract` and `body` is already visible in the
theorem's TYPE, and both shapes exist in the tree today:

- **contract** — `Concrete.ProofKit.unary_call` is universally quantified over
  `fns : FnTable` and takes the callee's behaviour as a HYPOTHESIS
  (`href : ∀ Y f, eval fns … body = some (specf Y)`). It holds for ANY table
  satisfying that contract, so an implementation change preserving `specf`
  leaves the theorem applicable and must not stale the caller.
- **body** — `Examples.ProofPatterns.Proofs.combine_correct` names the CONCRETE
  `combineFns` and unfolds `incFn`/`incExpr`/`dblFn`/`dblExpr`. It is a
  statement about those exact bodies; change `inc` and the theorem is about a
  different program.

Two honest proof styles therefore fall out of the edges rather than being
selected. A **modular** proof carries `contract` edges; a **closed-subject**
proof carries `body` edges and needs no individual proof links on its helpers.

`closed_subject` MUST NOT be available until the receipt actually carries the
transitive dependency root and replay verifies it. Until then the default is
modular and the system fails closed: a caller whose callees have neither links
nor bound bodies is `deps_not_current`.

### 1.2 How a `body` edge is resolved

A `body` edge names the callee's exact implementation, so resolving it must never
go through pretty-printed text, source names, absolute paths, or positional table
indices. Any of those is a guess, and a guessed dependency root is worse than no
root: it is confidently wrong.

The process is:

1. structurally reify the referenced `FnTable` constant;
2. require each relevant entry to carry a compiler-generated semantic
   `CallableId`;
3. resolve that ID through a generated proof-subject index;
4. compare the proof-model body digest against the extracted source-subject
   digest;
5. record the table digest, entry ID, callable ID and subject digest in the
   receipt.

Failures are classified precisely, because "unknown" and "known-stale" are
different facts and deserve different verdicts:

| outcome | verdict |
| --- | --- |
| no mapping | `missing` |
| multiple or colliding mappings | integrity error |
| identity matches, body digest differs | `stale` |
| exact mapping, digests agree | a current `body` edge |

**Never under-approximate.** Where table accesses are statically known, record
the exact subset; where they are dynamic, bind the ENTIRE table root. Recording
only the statically-visible subset of a dynamically-accessed table mints a
receipt that omits real dependencies — a confident `current` that is wrong.

**Migration order.** `PFnDef` currently carries `name : String` and no semantic
identity, and every FnTable in the proof corpus is hand-written. Enforcing the
classification before generated, ID-carrying tables exist would therefore report
`missing` for essentially every table-naming theorem and contain the whole
corpus. The order is: `CallableId` on `PFnDef` → compiler-generated tables →
migrate the hand-written tables → then enforce. Contract edges are unaffected
throughout, since they never name a table.

### 1.3 Workspace locator vs workspace identity

Locating a workspace and identifying one are different jobs. The locator policy
is: explicit `--workspace` → the input's workspace → the caller's workspace →
error. That is a convenience ordering and may use paths.

A durable receipt must NOT use the absolute workspace path as identity. It
carries the logical subject/callable ID, the workspace/import-closure digest,
manifest and lock digests, relevant module-content digests, proof/toolchain/
schema versions, and the workspace-selection origin as informational metadata
only.

For `verified`/`release` issuance, a caller-workspace fallback may produce a
receipt ONLY after proving the input corresponds to a subject in that closure.
Otherwise it may report locally but must not mint durable evidence.

A `trusted` edge never disappears into the caller. A proof reaching one is
recorded as `proved_by_lean_modulo_trusted`, never unqualified `proved_by_lean`
— otherwise trust is laundered through the caller, and a reader sees a
kernel-checked claim without being told part of the chain was never proved.

Only an EXPLICIT typed `trusted` edge authorizes a boundary. The call graph may
identify candidates for diagnostics, but it may not authorize: the current
implementation derives `trustedDeps` from the call graph, which is a conservative
stand-in to be replaced by the derived edge.

## 2. The `proved` State

When `proof-status` reports `proved`, it means:

- the function passed the proof-eligibility gates;
- its body extracted to ProofCore;
- its attachment names a non-empty spec and theorem;
- its stored fingerprint matches the current extracted body; and
- any registered spec-drift comparison passed.

The human report describes this as “linked + fingerprint-fresh” and points to
`--report check-proofs` for kernel replay. A CI claim of
`proved_by_lean` therefore gates both the attachment state and the replay
result; the status label alone is not a substitute for replay.

The theorem's coverage is independently reported as `point`,
`one_direction`, `iff`, `invariant`, `runtime_error`, or `full_contract`.
A point theorem is not silently promoted to a full contract.

## 3. Integrity States

| State | Meaning | Repair |
|---|---|---|
| `proved` | Attachment is eligible, extractable, linked, and fresh | Run kernel replay; keep the gate |
| `unbound` | A source proof link has no stored proof subject | Re-verify, then record `#[proof_fingerprint]` |
| `stale` | A stored fingerprint no longer matches | Update the theorem/attachment or revert the source change |
| `needs_recheck` *(R-0004 target)* | The subject/evidence schema or producer context changed without evidence that the program fact is false | Replay under the current schema/toolchain; do not copy hashes manually |
| `missing` | An eligible function has no attachment | Add a proof or accept the missing state |
| `blocked` | Extraction cannot represent the body | Use admitted constructs or extend ProofCore |
| `ineligible` | Capabilities, trust, entry status, or another gate excludes it | Change the program/policy or accept the boundary |
| `invalid`/diagnostic failure | Attachment or kernel replay failed | Repair the named integrity or theorem error |

R-0004's first containment introduced `unbound`: a
`#[proof_by]` link without `#[proof_fingerprint]` is never reported
`proved` and is not mislabeled `stale`. There was never a stored subject
against which to detect change.

`needs_recheck` is distinct from `stale`. `stale` means the semantic subject
changed. `needs_recheck` means existing evidence cannot be reused under the
current schema or producer context; the claim may still be true, but it is not
green until replay succeeds. Current `proof-status` does not yet emit this
target state.

## 4. What Users May Rely On

After both a fresh attachment and successful kernel replay, users may rely on:

- the named Lean theorem being accepted by the configured Lean kernel;
- the theorem applying to the registered `PExpr` specification;
- the recorded coverage class describing the theorem shape;
- body drift detected by the current fingerprint algorithm;
- machine-readable status and diagnostic facts suitable for CI.

The flagship gates record exact replay counts rather than inferring proof
existence from prose. Replay must run from a context that resolves the intended
workspace; R-0004 tracks a current directory-sensitive
`theorem_lookup` defect.

## 5. Current Freshness Boundary

The current stored fingerprint is a structural digest of the extracted
function body. It is deliberately insensitive to comments and whitespace.
Changes to represented statements, control flow, operations, literals, or call
forms change it.

It is not yet a digest of the complete semantic proof subject:

- signature and declared-type changes can be missed (bug 059);
- source contracts and attributes can be missed (bug 060);
- changes in direct or transitive callees do not invalidate the caller;
- toolchain/workspace identity is not bound to a persistent replay receipt;
- generic instantiation identity is not generally recorded.

R-0004 replaces this partial body hash with a versioned
`ProofSubjectDigest`, transitive dependency roots, and replay receipts. The
subject digest deliberately excludes theorem and toolchain identity; the
receipt binds those evidence facts to the subject. Until then, `proved` must be
read with these limitations, not as “the exact complete program fact is
pinned.”

## 6. What `proved` Does Not Mean

It does not mean:

- the generated binary is formally proved equivalent to the `PExpr`;
- ordinary fixed-width arithmetic cannot terminate on overflow;
- the function is proved safe—ownership and capability safety are separate,
  checker-enforced evidence;
- every property of the function is proved;
- separately proved functions compose automatically;
- the checker, extractor, compiler, LLVM/QBE, linker, runtime, OS, or hardware
  is proved correct;
- machine-level timing, termination, allocation bounds, or determinism is
  proved unless the theorem and assumptions say so.

The proof model uses mathematical integers for width-agnostic operations and
explicit fixed-width semantics only for the admitted operations listed in
`PROVABLE_V1.md`. Runtime ordinary arithmetic is checked and terminates on
overflow; it does not silently wrap.

## 7. Dependencies, Trust, and FFI

Proof-eligible functions are capability-free, non-trusted, non-entry
functions. Allocation, FFI, raw pointers, and effectful calls remain outside
`ProvableV1`.

A direct call represented in the proof function table lets the evaluator
execute the callee's proof expression. It does not automatically prove a
separate theorem about the composition, and the caller's current body
fingerprint does not include the callee body's digest. That dependency gap is
explicit R-0004 work.

The theorem also sits above unverified bridges:

| Boundary | Evidence |
|---|---|
| Lean theorem over `PExpr` | Kernel replay |
| Validated Core → `PExpr` | Implemented, tested, not formally verified |
| Validated Core → SSA | Validators/tests, not formally verified |
| SSA → backend IR | Differential/artifact tests, not formally verified |
| Backend IR → execution | Trusted toolchain and target assumptions |

## 8. Compatibility and CI

Fingerprint values and `PExpr` normalization may change across compiler
versions. Under R-0004's target model, a legacy schema or producer-context
change becomes `needs_recheck`, while a changed semantic subject becomes
`stale`. Either case requires replay, not blind hash replacement:

1. run `--report proof-status`;
2. run `--report check-proofs` from the repository/workspace context;
3. inspect any changed specification or extraction;
4. update a fingerprint only after the theorem has been re-verified;
5. gate the expected coverage and replay counts.

Human-readable report text may evolve. CI should consume the structured facts
and the explicit replay result, while still treating a schema change as a
review event.

## 9. Decision Table

| Question | Answer |
|---|---|
| Is the named theorem kernel-checked? | Only after successful `--report check-proofs`; `proof-status` alone reports link/freshness state. |
| Is an un-fingerprinted source link proved? | No. It is `unbound` and fails closed. |
| Does a fresh body hash cover signatures, contracts, and callees? | Not yet; R-0004 owns the full subject digest. |
| Does the theorem establish binary equivalence? | No. The compiler and backend bridges remain trusted/tested boundaries. |
| Do two proved functions automatically yield a composition theorem? | No. Composition must be explicit. |
| Can CI rely on the evidence? | Yes, when it gates structured attachment state, coverage, kernel replay, and the named assumptions together. |
