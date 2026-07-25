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
