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

## Additional carrier audit confirmations

- Premise functions returning Subtype and nested Sigma/PLift fields initially
  reported `projection-carrier-unavailable Subtype 1` and
  `projection-carrier-unavailable PLift 0`. The selected-demand repair now
  requires exact `FAIL projection-law` for both.
- A PSum computed from a data branch and an independently proved proof branch
  incorrectly reported `FAIL projection-law` under the whole-major provenance
  implementation (`1e25579d3`, 115 projection visits). Per-constructor field
  origins now distinguish it from the paired projected PSum.
- At `625bab0d9` (consumer `4b8764ee47a87e4c30df0714198cf2a0aff6ce37`),
  selecting `.1` from an And stored inside an Exists produced by a Bool matcher
  incorrectly returned N2 PASS (44 projection visits, 91 heartbeats). The
  preserved residual-demand repair rejects `.1` and keeps the independently
  proved `.2` PASS. Function-field controls additionally check that actual
  arguments survive the eliminator alias.
- The existing type-constructor/cache-identity probe exposed unsupported `List`
  traversal when replacing the isProof guard. Type-valued functions are now
  recognized structurally. All planted cache mutations retain exact stale-cache
  outcomes, including the expanded context/mode/origin diagnostics.

The final regression script has strict assertions in every group; its earlier
reproduction-only baseline mode is not part of the gate interface. Baseline
per-analysis measurements are in `N2-A3-round5-baseline-measurements.tsv`.

## Recursive and cast audit confirmations

At `1800c4f1b` (consumer `066a20466efa77ab6f59a88931851271a3da27fb`):

- A Nat recursor with an identity base function and a step applying its IH to
  `h.1` incorrectly passed (8 projection visits). Kernel-rule recursive sources
  now preserve that actual argument and require exact `FAIL projection-law`.
- Selecting the independent second proof from an Eq.ndrec-cast PProd incorrectly
  failed (22 projection visits) because the K-shaped shortcut inspected the
  entire carrier. The shortcut now applies the residual demand. The projected
  first field fails exactly; the independent second field passes.

The universal recursive-independence invariant generalizes the input major and
checks all proof-bearing fields and function outputs. Paired independent-base
and two-IH controls ensure that recursion is not rejected merely because it
uses an IH. Failed or unfinished checks cannot certify a negative answer, and
all validation consumes the same original budget.

## Metadata and fieldless-subject audit confirmations

At `8a789aa12` (consumer `4a4697bdc63dc1def7a13977e479079f0686fdcc`),
a bounded low-level computation-protection probe reported:

```
METADATA forged=false equal=false
METADATA forged=true equal=true
EMPTY carrier equal=true
```

The forged probe used the same known metadata name as the carrier recognizer
around an ordinary constant-returning subject-consuming operation. A plain
name is not an unforgeable carrier identity. The empty-record probe used
`protectedEq` and admitted a concrete-result counterfeit because reconstruction
created no semantic fields. Both are fixed by a typed projection wrapper with
a fresh local expression seal; no metadata name or flag grants authority. The
seal also supplies the missing fieldless-subject dependence. Permanent controls
require both metadata forgeries to compare false and the genuine/counterfeit
fieldless contracts to compare true/false respectively.
