# Round 5 reproductions before repair

Consumer: `2dcdaff775bdb4bf69a1be19f3609e544e2db7a0`, blob
`0dc43c1028dc684f04a83851ffac52432dc511a9`. All four N17a findings
were reproduced under `nix develop`, using bounded single-fixture drivers before
the corresponding implementation was changed. No review was run by the writer.

| Probe | Original outcome | Projection visits |
| --- | --- | ---: |
| subtype-proj | exit 0, N2 PASS (confirmed bypass) | 4 |
| packed-and-refl | exit 0, N2 PASS (confirmed bypass) | 7 |
| pprod-proj | exit 0, N2 PASS | 4 |
| psigma-proj | exit 0, N2 PASS | 4 |
| sigma-carrier | exit 0, N2 PASS | 4 |
| cases-bool | exit 0, N2 PASS (confirmed bypass) | 4 |
| cases-nat (explicit constant motive) | exit 0, N2 PASS | 4 |
| matcher | exit 0, N2 PASS | 5 |
| nested-matcher | exit 0, N2 PASS | 5 |
| cast-let | exit 0, N2 PASS | 8 |
| iff-field | exit 1, FAIL projection-law ConcreteSpecControl.growth | 9 |
| exists-carrier | exit 1, FAIL projection-law ConcreteSpecControl.growth | 14 |

The Iff and Exists probes do not reproduce a bypass: the original unary
eliminator traversal already follows those specific proofs. They are retained
as regression controls, not counted as newly confirmed defects. The review's
`Nat.casesOn n h.1 (fun _ => h.1)` shorthand requires an explicit constant motive
in this dependent fixture; without it Lean rejects the fixture before analysis.
The Bool reproducer needs no such adjustment and confirms finding 2 directly.

The two exact empty-list counterfeits (`counterfeitEmptyOk`, changing both the
success store and wildcard, and `counterfeitTrapBranchEmpty`, replacing the trap
branch by False) both reported `result=PASS` with zero findings. The isolated
store-only and wildcard-only variants also passed. The original seven controls
retained exactly their two expected `FAIL instance-application` findings.

The genuine dependent contract (`∃ x : {v : Nat // v ≤ S n}, x.1 < S x.1`)
reported exit 2, `INCOMPLETE analysis-budget-or-error` at stage instance,
`reason=analysis-error`, with `Application type mismatch` between
`{ v // v ≤ computation 2 }` and `{ v // v ≤ result }` in `x.val`.
The dependent planted proof is constructive (`Nat.lt_irrefl`), because `decide`
cannot synthesize a decision procedure for that unbounded existential.
Instance measurement: heartbeats 33/2000000, work 302/20000; projection visits 6.

Projection source and assertions are in `vacuity_amendment3_round5.py`;
empty-list contracts are in `fixtures/N2-Typed-Contracts.lean` and use the
pinned single-contract harness `vacuity_typed_contracts.py`.
