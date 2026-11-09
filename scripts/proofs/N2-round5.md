# N2 round 5 writer handoff — staging, not promotion-ready

For the coordinator's independent reviewers: **do the best long-term thing rather
than cut corners**. Preserve exact negative reasons and actual argument provenance;
do not accept argument erasure, dependency sampling, or a budget increase as the
repair for the remaining recursive-proof expansion.

## Findings and status

1. **FIXED — reproduction and independent cause confirmation.** The complete
external Init-only repro is retained byte-for-byte in `fixtures/F3b-Repro.lean`.
The README's driver was run against a throwaway source directory containing that
file, its original root import, and ROADMAP.md. Elaboration uses cached Lean and
local files, with no Lake/network dependency fetch. The instrumented baseline
550fab85e reports the two instance-false-discharge failures at :19 and :52 and
four consequent planted failures. Own trace: `False.rec … (f₂ t₁)` under the
hypothetical isTrue/isFalse branches and `List.decidableBAll.match_1` under local
branch binders. The additional context trace identifies `decide (a=b)=true` with
`¬a=b` inside `of_decide_eq_true`; neither binder alone is refuted.

The exact c1 ccca4abaa Exec and c2 9cb0ef7fe Const sources were freshly archived
from local git. Own baseline probes reproduce all three named helper findings:
logSegment derives False from a hypothetical exec=done equation;
boundary_of_performed eliminates a nil/cons contradiction under a local plan
matching equality; acyclic_of_dependencyOrder derives False from the jointly
incompatible CycleChain and DependencyOrder assumptions. The current isolated
helper-body probes require falseDischarge=false for every helper. These are
isolated real-source checks, not complete c1/c2 audits.

2. **FIXED — reported discharge boundary, with a completion blocker below.**
Actual arguments are substituted even for open helper applications. A checked
context refutation, including several binders jointly, can certify a hypothetical
branch unreachable. Open intermediate contradictions are followed through helper
proofs to their suppliers. A closed refuted inhabitant or an opaque supplied
producer of one is rejected. No library names are trusted and no nesting cutoff
was added. Memoization includes the local context, preventing an unreachable use
from masking a later actual supplied use.

3. **FIXED — focused controls.** The complete external repro must return N2 PASS
with no FAIL/INCOMPLETE findings. Every previous negative discharge form retains
its exact instance-false-discharge expectation. New controls cover joint supplied
contradictions both closed and under a Nat binder, a reachable impossible helper
branch after a safe application of the same helper, a data-dependent supplied
impossibility, an unreachable closed supplier, and context-sensitive cache reuse.
The joint negatives follow directly from the convention: they really discharge
the actual premise through False.elim, unlike unsupplied branch binders. No OD or
new convention interpretation was needed.

4. **NOT FIXED — historical completion requirement.** Projection analysis,
projection bounds, report completeness, replay implementation and all historical
fixture expectations are unchanged. However actual-argument discharge traversal
now exceeds the unchanged 2,000,000-heartbeat bound in the exact historical C9
control. It correctly reports INCOMPLETE; the expected clean C9 evidence cannot
yet be certified. This blocks promotion and a claim that --replay still passes.
No whole-module/head completion claim is made from the helper probes.

Own progress tracing shows repeatedly expanded Nat.le_trans/Nat.le.below
machinery under growing hypothetical contexts. An attempted alpha-closure cache
incorrectly conflated a lambda with its opened body: the exact generic-supplier
controls caught that false pass. That optimization is not shipped. Preserving the
lambda/body distinction restored the negatives but not C9 completion. The shipped
cache uses exact expressions and explicit local-context identities.

The owning README records the remaining structural design: recursive proof
summaries that retain formal supplier provenance and data-dependent branch
constraints, instantiated at every application, with a shared bounded worklist
and unfinished recursive components reported INCOMPLETE. R-0219 owns that work;
the coordinator can allocate a specific implementation row. No ISS/R IDs were
allocated. R-0691 environment sharding does not fix expansion inside one proof.

## Focused validation

All commands are externally bounded; Lean checks use nix develop. No full local
suite, heavy build, review tool, push, history rewrite, other-branch edit,
ConcreteSpec edit or ROADMAP edit was performed.

- `vacuity_round5.py controls`: see `audits/N2-round5-controls-final.txt`.
- `vacuity_round5.py c1|c2 --baseline`: exact true verdicts for all three helpers;
  own diagnostics in `audits/N2-round5-c1-baseline.txt` and
  `audits/N2-round5-c2-baseline.txt`.
- `vacuity_round5.py c1|c2`: final isolated false verdicts in the corresponding
  `audits/N2-round5-c1-helpers-final.txt` and `N2-round5-c2-helpers-final.txt`.
- `vacuity_fixtures.py`: PASS, 10 byte-for-byte historical excerpts.
- `vacuity_fixtures.py --focused-c2`: PASS, 49 visits at limits 64 and 4,096.
- `vacuity_fixtures.py --focused-c9`: FAIL, INCOMPLETE heartbeat exhaustion;
  this is an unresolved final-code blocker, not an accepted expected PASS.
- `scripts/checks/doc_links.sh`: PASS, 12 controls, 1,123 links, 893 anchors.
- `git diff --check`: PASS.

## Required coordinator server runs

Run on concrete-ci-2 against the final writer commit, record its exact SHA,
source hashes, projection costs, elapsed time and terminal verdicts, and use
fresh report directories. Keep every existing fixture expectation unchanged.

1. Run the full N2 controls remotely (the writer ran only the focused discharge
selection): `nix develop -c python3 scripts/proofs/vacuity_driver.py --controls`.
2. Run `nix develop -c python3 scripts/proofs/vacuity_fixtures.py --replay NEW_DIR`.
This is currently blocked by the known C9 completion regression; do not treat
missing diagnostics in an incomplete report as clean evidence. Fix recursive
completion and require replay PASS before interpreting c1/c2 promotion results.
3. After replay passes, archive exact c1 ccca4abaa and c2 9cb0ef7fe sources and
run `vacuity_driver.py --source ARCHIVE_DIR` once for each, with all seventeen
`--scope ConcreteSpec.Proofs.MODULE` arguments below. Run sequentially under the
server heavy-slot policy and an external bound. These are whole-scope audits,
not the isolated helper probes:

```
Authority Basics Borrow Brand Checker Cleanup Const Examples Exec Hybrid Memory
Observe2 Observe Rep Scalar Slice0 Slice1
```

The target is **0 c1 / 0 c2 instance-false-discharge findings**, down from 11/5,
with no INCOMPLETE and all scopes complete. The focused semantic controls support
that boundary; the writer has not established these full-run outcomes. Other
real findings (including approvals, classifications and planted witnesses) remain
findings, so zero false-discharge findings does not imply whole-report N2 PASS.
If the current checker exhausts, record the actual INCOMPLETE and stop promotion.
