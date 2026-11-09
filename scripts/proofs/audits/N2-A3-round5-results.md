# N2 amendment 3 — round 5 final receipts
All requested focused checks passed. No whole-scope census, review tool, push, rebase or force operation was run by the writer.
Checker code: `0053d556f` (same consumer bytes throughout the final replay). Consumer blob: `59b6faed3d5dd998848ce738d817a11a74729d4c`; SHA-256: `a9804eb0909e63b129a4a95e8e5054c1b7edf78075ff02c248cabcb06321ef69`. Typed-contract source: `6efdce6136752d51fc74265d61278d90937f24cd`; other cases are the checked synthetic or byte-exact historical fixtures.
## Confirmed review findings
All four N17a findings were reproduced at `2dcdaff775bdb4bf69a1be19f3609e544e2db7a0` before repair. Full outcomes and audit extensions are in [the reproduction record](N2-A3-round5-reproduction.md).
| Reproducer | Before | Final exact outcome |
| --- | --- | --- |
| Subtype proof projection | N2 PASS | `FAIL projection-law ConcreteSpecControl.growth` |
| Packed And / Nat.le.refl selected field | N2 PASS | `FAIL projection-law ConcreteSpecControl.growth` |
| Bool.casesOn true h.1 h.1 | N2 PASS | `FAIL projection-law ConcreteSpecControl.growth` |
| Counterfeit empty-list Ok (store and wildcard) | checkInstance PASS | `FAIL instance-application Confirm.counterfeitEmptyOk law=ConcreteSpec.Core.Own.evalArgs_ok_erasure case=typed` |
| Counterfeit empty-list Trap | checkInstance PASS | `FAIL instance-application Confirm.counterfeitTrapBranchEmpty law=ConcreteSpec.Core.Own.evalArgs_trap_erasure case=typed` |
| Dependent Subtype contract | INCOMPLETE, mismatched computation/result domains | N2 PASS |

Iff and direct Exists repackaging already rejected in the baseline; they were retained as regression controls. The Nat.casesOn shorthand needed an explicit constant motive to elaborate; the Bool reproducer needed no change.
## Final checks
| Check | Result |
| --- | --- |
| vacuity_controls.py through vacuity_driver.main --controls | 134 assertions PASS |
| vacuity_reporting.py through the driver | exact named faults, watchdog/journal recovery and replay rejection PASS |
| vacuity_budget.py | 13 primary budget checks, DAG/dependency/refutation and identity checks PASS |
| vacuity_round5.py controls | selected controls and certificates PASS |
| vacuity_certificates.py | 5 PASS |
| vacuity_inline_match.py / vacuity_typed_contracts.py | 2 inline controls and all 13 typed contracts PASS |
| vacuity_fixtures.py | 10 byte-for-byte provenance checks PASS |
| vacuity_body_access.py | PASS |
| vacuity_cache_identity.py | 25 assertions and 4 exact planted stale-cache controls PASS |
| vacuity_amendment3_round5.py | 39 exact assertions: 27 expected projection failures, 10 genuine PASS, 2 exact unavailable-carrier INCOMPLETE |
| vacuity_protected_results.py | 9 controls PASS, including fieldless subjects, dependent domains and three metadata comparisons |
| def/theorem/opaque differential | all three exactly `FAIL projection-law ConcreteSpecControl.growth`; unavailable proof body exactly INCOMPLETE |

Each transcript is `N2-A3-round5-<check>.txt` and carries the consumer blob. The driver automatically runs reporting controls after the 134 core assertions. Planted consumer measurements are identified separately from ordinary consumers in the JSONL.
## Every expanded projection/carrier control
| Control | Exit | Exact diagnostic / result | Projection H / W / P |
| --- | ---: | --- | --- |
| subtype-proj | 1 | `FAIL projection-law ConcreteSpecControl.growth` | 9 / 0 / 4 |
| packed-and-refl | 1 | `FAIL projection-law ConcreteSpecControl.growth` | 9 / 0 / 4 |
| pprod-proj | 1 | `FAIL projection-law ConcreteSpecControl.growth` | 9 / 0 / 4 |
| psigma-proj | 1 | `FAIL projection-law ConcreteSpecControl.growth` | 9 / 0 / 4 |
| cases-bool | 1 | `FAIL projection-law ConcreteSpecControl.growth` | 13 / 0 / 6 |
| cases-nat | 1 | `FAIL projection-law ConcreteSpecControl.growth` | 14 / 0 / 6 |
| matcher | 1 | `FAIL projection-law ConcreteSpecControl.growth` | 45 / 0 / 24 |
| nested-matcher | 1 | `FAIL projection-law ConcreteSpecControl.growth` | 86 / 0 / 46 |
| mixed-minors | 1 | `FAIL projection-law ConcreteSpecControl.growth` | 47 / 0 / 26 |
| recursor-function | 1 | `FAIL projection-law ConcreteSpecControl.growth` | 43 / 0 / 23 |
| match-carrier | 1 | `FAIL projection-law ConcreteSpecControl.growth` | 41 / 0 / 25 |
| psum-carrier | 1 | `FAIL projection-law ConcreteSpecControl.growth` | 115 / 0 / 62 |
| nested-exists-field | 1 | `FAIL projection-law ConcreteSpecControl.growth` | 150 / 0 / 76 |
| minor-function-application | 1 | `FAIL projection-law ConcreteSpecControl.growth` | 135 / 0 / 66 |
| recursive-ih-transfer | 1 | `FAIL projection-law ConcreteSpecControl.growth` | 37 / 0 / 15 |
| cast-let | 1 | `FAIL projection-law ConcreteSpecControl.growth` | 19 / 0 / 9 |
| iff-field | 1 | `FAIL projection-law ConcreteSpecControl.growth` | 9 / 0 / 4 |
| exists-carrier | 1 | `FAIL projection-law ConcreteSpecControl.growth` | 22 / 0 / 10 |
| sigma-carrier | 1 | `FAIL projection-law ConcreteSpecControl.growth` | 9 / 0 / 6 |
| def-data-projection | 1 | `FAIL projection-law ConcreteSpecControl.growth` | 11 / 0 / 6 |
| def-data-elimination | 1 | `FAIL projection-law ConcreteSpecControl.growth` | 122 / 0 / 69 |
| opaque-data-projection | 1 | `FAIL projection-law ConcreteSpecControl.growth` | 11 / 0 / 6 |
| opaque-data-elimination | 1 | `FAIL projection-law ConcreteSpecControl.growth` | 122 / 0 / 69 |
| unavailable-carrier | 2 | `DETAIL analysis-error ConcreteSpecControl.growth stage=projection law=ConcreteSpecControl.growth case=none projection-body-unavailable ConcreteSpecControl.unavailableCarrier` | 7 / 0 / 3 |
| unavailable-sum | 2 | `DETAIL analysis-error ConcreteSpecControl.growth stage=projection law=ConcreteSpecControl.growth case=none projection-body-unavailable ConcreteSpecControl.unavailableCarrier` | 19 / 0 / 10 |
| function-carrier | 1 | `FAIL projection-law ConcreteSpecControl.growth` | 5 / 0 / 2 |
| nested-carrier | 1 | `FAIL projection-law ConcreteSpecControl.growth` | 5 / 0 / 3 |
| independent-selected | 0 | `N2 PASS` | 15 / 0 / 7 |
| match-independent-field | 0 | `N2 PASS` | 45 / 0 / 28 |
| minor-independent-origin | 0 | `N2 PASS` | 118 / 0 / 64 |
| psum-independent-origin | 0 | `N2 PASS` | 117 / 0 / 65 |
| nested-exists-independent-field | 0 | `N2 PASS` | 157 / 0 / 83 |
| minor-function-independent-argument | 0 | `N2 PASS` | 142 / 0 / 72 |
| recursive-independent-base | 0 | `N2 PASS` | 36 / 0 / 15 |
| two-ih-projection | 1 | `FAIL projection-law ConcreteSpecControl.growth` | 186 / 0 / 99 |
| two-ih-independent | 0 | `N2 PASS` | 83 / 0 / 38 |
| cast-field-1 | 1 | `FAIL projection-law ConcreteSpecControl.growth` | 18 / 0 / 12 |
| cast-field-2 | 0 | `N2 PASS` | 29 / 0 / 20 |
| dependent | 0 | `N2 PASS` | 12 / 0 / 6 |

## Typed contracts and per-analysis cost
Bounds remain heartbeats 2,000,000, work 20,000 and projection 4,096; the synthetic controls retain their existing lower bounds. Driver watchdog defaults remain 120 seconds with the existing retry. No budget or wall policy was chosen.
| Contract | Result | Before H / W | Final H / W |
| --- | --- | --- | --- |
| Confirm.inferredOk | PASS | 4301 / 5584 | 4279 / 5354 |
| Confirm.restatedOk | PASS | 4367 / 6577 | 4342 / 6256 |
| Confirm.inferredTrap | PASS | 4260 / 5102 | 4239 / 4898 |
| Confirm.restatedTrap | PASS | 4316 / 5837 | 4294 / 5568 |
| Confirm.changedStoreOk | FAIL | 4371 / 6585 | 4345 / 6266 |
| Confirm.unselectedRestatedOk | PASS | 4362 / 6529 | 4338 / 6213 |
| Confirm.unselectedChangedStoreOk | FAIL | 4366 / 6537 | 4340 / 6223 |
| Confirm.emptyOk | PASS | 4350 / 6461 | 4332 / 6152 |
| Confirm.emptyTrap | PASS | 4299 / 5721 | 4284 / 5464 |
| Confirm.counterfeitEmptyOk | FAIL | 4351 / 6469 | 4334 / 6162 |
| Confirm.counterfeitEmptyStore | FAIL | 4350 / 6469 | 4334 / 6162 |
| Confirm.counterfeitEmptyWildcard | FAIL | 4350 / 6461 | 4332 / 6152 |
| Confirm.counterfeitTrapBranchEmpty | FAIL | 4291 / 5655 | 4275 / 5398 |

[Final measurements](N2-A3-round5-final-measurements.tsv) contain 783 labeled per-analysis vectors; [changed measurements](N2-A3-round5-changed-measurements.tsv) contain all 61 comparable baseline/final changes, including status changes. [Baseline vectors](N2-A3-round5-baseline-measurements.tsv) pin the original checker. [Driver measurements](N2-A3-round5-driver-measurements.jsonl) contain every driver-run analysis result, fixture hash, verdict and ordinary/planted consumer identity. Raw transcripts include the protected-result vectors and every new control diagnostic. No full-scope cost is claimed.
## Commits and invariants
- `a9878b073` — fix(proofs): follow selected projection fields through data carriers
- `ef69e976a` — fix(proofs): inspect all reachable recursor alternatives with provenance
- `7bcab63ff` — fix(proofs): distinguish subject carriers from typed computation results
- `6fc5c526a` — fix(proofs): transform dependent contracts once with shared typed placeholders
- `2a7934620` — fix(proofs): preserve nested field demands through premise carriers
- `1e25579d3` — fix(proofs): exclude type constructors from projection evidence
- `625bab0d9` — fix(proofs): track lazy constructor-field provenance across recursor branches
- `1800c4f1b` — fix(proofs): carry nested demands and share strict constructor reduction
- `8a789aa12` — fix(proofs): validate recursive sources and preserve cast demands
- `5770ea8db` — fix(proofs): seal subject carriers with typed local identities
- `8ae89aa46` — test(proofs): pin round 5 server audit and baseline measurements
- `0053d556f` — fix(proofs): reuse strong recursive invariants through carrier demands

The `certificateDependency` through `end Discharge` region remains byte-identical to `2dcdaff77`: SHA-256 `474a74f351984565847d5239a86707b389970bf1eba33d2b45c4486c1ec11fd4`. The consumer contains no propext-specific code. No ConcreteSpec, ROADMAP or owner-decision file was edited. Structural causes and owner R-0219 are recorded in scripts/proofs/README.md.

Pending: per-analysis interpretation, whole-scope wall policy, the Law/Planted hidden-contradiction contract C, and `propext: pending owner question 8`. The six real-spec names from N17d remain recorded in README.md and N2-A3-accounting.md. Full branch traversal newly exposes the same pending primitive in synthetic `ConcreteSpec.Repro.find_absent`; its exact INCOMPLETE is required while len_pos and sum_zero pass under their unchanged 64-visit limits.
## Coordinator command on concrete-ci-2
After the coordinator pushes the final branch, run from its ci-2 clone:

```sh
bash scripts/proofs/audits/N2-A3-round5-ci2.sh
```

The script checks the final consumer blob, archives the pinned historical source, and uses the existing watchdog and outer bounds. It was syntax-checked locally and was not executed locally. Reviews of record remain with the coordinator using a different model; reviewers must do the best long-term thing rather than cut corners.
