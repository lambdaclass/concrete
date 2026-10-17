# Bug 079 — a receipt was minted for a body the linked theorem says nothing about

**Status:** FIXED (branch `bug079-spec-binding`, from published main `b87152ba`)
**Found:** 2026-10-17, while building R-0482 P1's negative controls. A cross-package fixture's claim
reached "proved" once its closure could root, and nothing about its theorem was ever about its body.
**Severity:** a false proof, carried into the evidence surface. On `b87152ba`, in a project inside the
repository:

```
#[spec(Concrete.Proof.checkNonceExpr)]
#[proof_by(Examples.CryptoVerify.Proofs.check_nonce_correct)]
#[proof_coverage(iff)]
#[proof_fingerprint("v2:9c35413fd77272f9bc5933c510f2c5bf")]
fn always_seven(x: Int) -> Int { return 7; }
```

`--report proof-status` said `proved [iff]`, `--report receipts` said `1 issued`, and the stored
receipt read back as `kernel-replayed, receipt current`. The store is kept as
`scripts/tests/fixtures/bug079/always_seven.b87152ba.store`.

## Mechanism

A claim names a theorem (`#[proof_by]`) and a specification (`#[spec]`). Three checks stood between
that and a receipt, and none compared the specification with the body:

- the **fingerprint** says the body did not change since it was pinned — and an author pins it;
- **issuance** required the theorem's statement to mention the claimed specification's name — and
  the author chooses that name;
- the **drift check** compared the extracted body with a specification, but looked the
  specification up in `Concrete.Proof.specs` by the FUNCTION'S SOURCE NAME. A function whose name had
  no entry was never compared, and the report said only `spec: NOT drift-covered`.

So any function could borrow any accepted theorem by naming that theorem's specification. Two further
shapes of the same defect, both measured:

- a drift-covered function whose `#[spec]` and `#[proof_by]` were changed together was compared with
  its OLD specification, the one keyed by its name, and passed;
- membership in the theorem's function table did not help: an attested subject linked to a theorem
  about a different body was proved, because membership says an identity was attested, not that the
  theorem is about that identity's body.

Ten proved claims in the corpus were not drift-covered when found. Audited individually (below).

## Fixed

ONE binding result, `subjectBindingOf` (`Concrete/Proof/ProofCore.lean`), governs the proved status,
receipt issuance and receipt consumption — issuance and consumption both require `proved`, and
`proved` requires the binding. A claim binds only when:

1. the theorem's statement evaluates a specification (a closed `PExpr` term);
2. the claimed `#[spec]` is the head of one of those terms (a hardcoded link, which carries no spec
   name, may use the theorem's specification only when there is exactly one);
3. the extracted body's canonical digest (`sourceBodyDigestV1Of`) equals that term's value's;
4. the specification declares its parameters — the `PFnDef` or `SpecDecl` whose body it is, exactly
   one list — and they are the function's parameters, in order.

The specification VALUES are computed where the theorem lives: `scripts/gen/classifications.lean`
evaluates each closed specification term of each linked theorem's statement and emits its digest and
declared parameters into `specBindingTable` (`Concrete/Proof/ClassificationTable.lean`), which the
classification freshness gate re-derives. Most specifications live under `proofs/`, which the compiler
cannot import. std's own links are now in the generator's input too.

A body that differs from the specification the theorem evaluates is `stale`, rendered as "the linked
theorem is about a different body". Any other failure to bind is the new status `spec_unbound`
(diagnostic `E0813`), which is not current for dependents and fails `require-proofs`.
`Concrete.Proof.specs` and `specFor` are removed; nothing compares by source name any more.

Specifications with no `PFnDef` gained a `SpecDecl` naming their parameters: the six ProofPatterns
specs, the nine std specs and the two hmac specs.

## Audit of the ten

| claim | finding | status |
|---|---|---|
| `elf_header` `check_magic`, `check_class`, `check_data`, `check_version`, `validate_header` | bound to their own specifications, parameters exact | proved |
| `elf_header_drifted` `check_class`, `check_data` | bound (bodies identical to the specifications) | proved |
| `thesis_demo` `parse_byte`, `check_length` (hardcoded) | bound through the theorem's one specification; parameters from its `PFnDef` | proved |
| `loop_invariant.count_up` | `count_up_loop_preserves` is about a list of loop assignments and evaluates no specification; nothing ties that list to the source loop | spec_unbound |

Elsewhere in the corpus the binding refused four claims that were proved before: `count_up` (above);
`contract_negatives/fabricated_proof` and `proof_patterns/repair`, which link theorems that do not
exist (the first was a disclosed false green, now closed); and `proof_pressure`'s `check_nonce`,
whose `#[spec(check_nonce.spec)]` names no specification — it was "drift-checked" against
`checkNonceExpr` only because its source name was a key in the old table. Repairing that fixture's
attribute moves the frozen R-0482 identity rows, so it is left refused and recorded here.

## Controls

`scripts/tests/check_subject_binding.sh` (12 legs, in a scratch copy of the repository):

- a positive control that still mints and reads back current;
- `always_seven`: not proved, named as a different body, no receipt;
- the `b87152ba` receipt for `always_seven` is rejected as `claim_not_proved`;
- `crypto_verify.check_nonce` re-linked to `check_class_correct` with its fingerprint re-pinned;
- `elf_header.check_class`, attested in `elfFns`, re-linked to `check_data_correct`;
- `compute_tag`'s body under permuted parameters;
- `count_up`, whose theorem evaluates no specification.

Mutation families `subject-binding-compares-body`, `subject-binding-compares-params` and
`subject-binding-governs-proved` each kill against the gate.

## Limits

The binding establishes that the claim's body and parameters ARE a specification the theorem's
statement evaluates. It does not establish what the theorem says about that specification — whether
the statement is the claimed coverage (`iff`, `one_direction`, …) — which remains the author's
annotation, and it does not establish that `PExpr` semantics match compiled code (H19).
