# R-0208 closure record — tool-version drift, checker identity, advisory policy, Lean #14576 fire drill

Status: closure record. Each recorded criterion is listed with the evidence that meets it and the
gate that keeps it true. Closure is stated against these criteria only; what stays open is listed at
the end, not hidden. Native-trust receipts are a DEFERRED feature, withheld, and are not a closure
criterion (user decision, 2026-10-12).

## The fire drill, in one paragraph

Lean issue [#14576](https://github.com/leanprover/lean4/issues/14576): the kernel accepted
wrong-structure projections, so an adversarial metaprogram could add checked declarations that prove
`False` while `#print axioms` reports none. The upstream reproducer is retained verbatim
(`scripts/tests/fixtures/lean14576/Bug.lean`, sha256-pinned) and run in isolation — fresh directory,
emptied environment, the exact checker executable — by `check_lean14576_reproducer.sh`. Measured
2026-10-12 on macOS aarch64: **lean 4.28.0 (the checker this repository pinned) ACCEPTS it**
(`'bad'` and `'boom'` "do not depend on any axioms"); **lean 4.34.1 REJECTS it** in the kernel
(`(kernel) invalid projection`). The gate fails on 4.28.0 and passes on 4.34.1.

This removes the assurance the affected checker provided. It does **not** show that any theorem in
this repository is false, and no report says so.

## Criteria → evidence

| Recorded criterion | Evidence | Kept true by |
|---|---|---|
| Evidence records the Lean version and build, the compiler, and the proof library | receiptV2 binds `toolchain` = hash(compiler identity, checker identity) and carries both in the clear; the checker identity is version, commit, platform and a digest of the images the replay process actually loaded; `imports` binds the proof library (ProofKit included) by source content | `check_checker_identity.sh`, `check_receipt_consumption.sh` |
| A toolchain bump marks evidence needing re-check; old badges never silently reused | a receipt established under another checker is not current, and the report names what moved: an old 4.28.0 receipt "differs in: toolchain; still matching: subject, edge, tables, root, artifact, trust, workspace, imports" | `check_checker_policy.sh` |
| Machine-readable advisory register keyed by implementation, exact version/build, advisory id; statuses accepted/affected/revoked/unknown | `Concrete/Proof/CheckerPolicy.lean` (policy v3): `lean4#14576` affects exactly `4.28.0`, replacement `4.34.1` (verified by the reproducer); accepted entries are MEASURED BUILDS (checker, version, commit, platform, build digest) | `check_checker_policy.sh` |
| Policy refuses or downgrades affected, revoked or unknown checkers; a register change re-evaluates | decisions are computed at read time beside currency and never written into receipts; revoked > affected > accepted; unknown is never accepted | `check_checker_policy.sh` (synthetic transitions) |
| Who controls policy; missing, altered or rolled-back policy fails closed | the register is compiled into the consumer and its identity is named in every decision; a compiled `policyVersion` below `scripts/tests/fixtures/checker_policy_minimum` is refused. **Limited:** reverting both together is not refused | `check_checker_policy.sh` |
| Checker identity binds the executable and build that actually ran | the replay process reports itself (nonce-correlated) and the consumer hashes its loaded images; Nix and elan load byte-identical images on macOS | `check_checker_identity.sh` |
| Trust basis is fresh consumer replay; replay includes an actual kernel re-check | issuance and consumption both re-check each theorem's workspace constant closure through `Lean.Kernel.Environment.replay` in the identified process; axioms allow-listed to `propext`/`Classical.choice`/`Quot.sound` | `check_kernel_recheck.sh` |
| Retain the exact upstream exploit and an isolated affected toolchain; never run it through the ambient environment | above | `check_lean14576_reproducer.sh` |
| Inventory every Lean-backed claim before upgrading; replay the same denominator after | `docs/verification/claim-inventory-v4.28.0.tsv` (frozen before the upgrade) and `…-v4.34.1.tsv`: 35 sources, 71 claims, every row identical | `scripts/tools/claim_inventory.sh`; `checker_acceptance_evidence.sh` replays it per measured build |
| Classify every proof, build and golden change; no unexplained regeneration | upgrade commit `ca7852e1` classifies each source adaptation (none changes a statement); classification table `b3fec1b8`: 43 rows keep edge, tables, trust and specs, only proof-term digests move (rendered types identical across versions) | `check_classification_freshness.sh` |
| Old checker-bound validity downgrades while subject, correspondence and dependency facts stay visible | the "differs in / still matching" report line | `check_checker_policy.sh` |
| An old receipt cannot regain acceptance by editing a version string, advisory entry or report output | editing the readable checker or version → `identity_not_bound`; a fully consistent forgery is accepted 0 of 5 without the consumer's re-check; the register is compiled, not read from the store | `check_checker_policy.sh` |
| Only a fresh replay under an accepted checker restores acceptance | ACCEPTED NOW = accepted measured build ∧ this run's kernel re-check (`Proof.acceptedNow`, tested on synthetic inputs on every platform) | `check_checker_policy.sh` |
| Publish the exact affected claims and the remaining trust | below | this document |

## Affected claims

Every receipt established under lean 4.28.0 rests on a checker named by `lean4#14576`. In the frozen
inventory that is **31 minted receipts** across 16 sources: `crypto_verify` 4, `elf_header` 5,
`elf_header_drifted` 2, `fixed_capacity` 4, `parse_validate` 1, `contract_negatives/weakened_postcondition`
1, and `proof_patterns` 14 (`array_update` 1, `composition` 3, `composition_trusted_helper` 2,
`composition_unlinked_helper` 1, `fold` 1, `ghost` 2, `loop_copy` 1, `stale_missing_partial` 1,
`straight_line` 1, `workspace` 1). Each is re-established only by a fresh kernel re-check under an
accepted 4.34.1 build; the replay on 4.34.1 re-checks all 31 and every row is unchanged.

## Remaining trust, stated

- **Toolchain core** (`Init`/`Std`/`Lean` `.olean` data) is not replayed; it is part of the checker
  build named by its identity. A `--fresh` replay of the core measured 88 s per closure.
- **OS libraries** are named in the identity, not hashed. On 4.34.1 the release links GMP and libuv
  statically, so only system libraries remain named.
- **Correlation, not authentication**: the checker's self-report is nonce-correlated; code running
  inside the checker could print the same lines. Files are hashed after the run (a time-of-check gap).
- **Native trust**: the HMAC claims rest on `bv_decide` (`._native.bv_decide.ax_*` on 4.34.1). They are
  reported `native_trust` and withheld; native-trust receipts are deferred, and when built must never
  satisfy a kernel-only policy.
- **Rollback**: limited, as above.
- **Linux**: no Linux build is accepted until its CI-measured digest has self-contained evidence
  (`scripts/tools/checker_acceptance_evidence.sh`: identity, the hostile controls re-run with logs and
  tied to that identity, axiom inventory, inventory replay). Until registered it reads `unknown`.
