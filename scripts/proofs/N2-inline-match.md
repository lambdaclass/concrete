# N2 inline-match Instance comparison: implementation defect

Owner: R-0219. Independent reviewers: **do the best long-term thing rather than
cut corners**. The coordinator runs reviews of record; this writer ran none.
Amendment 2 and its pending owner trust-policy answer are untouched.

## Classification and structural cause

**Implementation defect**, not a new design or trust rule. The accepted contract
in `N2-discharge-solver.md:18–22` already requires the full law application's type
to match its instantiated statement by protected definitional equality. The
owning README requires surrounding contracts to normalize while the semantic
subjects remain abstract. No new rule for admitting an Instance is needed.

At the baseline f85c2151ecfa35e916f9ee96714997d090438ed5,
`scripts/proofs/vacuity.lean:887–909` (`withProtected`) correctly abstracts the
subject. But `protectedEq` at **912–913** passes the default `isDefEq` directly,
leaving Lean's smart-unfolding heuristic enabled. The Instance loop at
**1137** reports the resulting false comparison as `FAIL instance-application`.
This is the one owner of protected comparison, also used by Planted checking.

In the minimal reproducer, the law's polymorphic nested-pattern matcher is
`growth.match_1 Nat ...`, while the example's matcher is specialized at Nat and
has a different head and argument vector. Both are definitionally equal contract
implementations. After replacing `S` by a fresh local, their discriminant cannot
compute. Lean 4.34.1 `Lean/Meta/WHNF.lean:910–913` deliberately refuses to unfold
matcher definitions under smart unfolding. `Lean/Meta/ExprDefEq.lean:1486–1489`
explains that their abbreviation annotation assists the kernel but smart
unfolding leaves them folded. Ordinary delta conversion exposes the same
recursor contract without evaluating the protected operation.

A temporary, uncommitted instrumentation probe observed:

- Raw instantiated statements: `isDefEq = true`.
- Protected statements: default `isDefEq = false`.
- Protected statements even at transparency `.all`: `false`.
- Protected statements with `smartUnfolding = false`: `true`.

The fix at `vacuity.lean:912–917` selects ordinary definitional unfolding only
inside the protected conversion callback. Subject/dependency abstraction still
happens first. Both Instance and Planted comparisons use this same function.
There is no matcher/name/trust list, added normalization pass, increased
transparency, proof-dependency admission, budget change, or spec/OD choice.
Unfinished conversion still follows the existing named INCOMPLETE path.

## Minimal reproducer and exact controls

[The Lean fixture](fixtures/N2-Inline-Match-Repro.lean) contains one polymorphic
law with an inline nested match, one genuine full Instance, and its valid paired
Planted witness. It needs no real spec files or axioms. It abstracts the pattern
at `ConcreteSpec/Proofs/Own.lean:4224` in source commit
6efdce6136752d51fc74265d61278d90937f24cd. That object was fetched directly into
this clone after it was absent from the prescribed concrete2 clone; nothing was
checked out, merged, or changed in that source.

The exact baseline finding is:

```text
FAIL instance-application ConcreteSpec.Repro.«⟪example⟫ConcreteSpec.lean:16» law=ConcreteSpec.Repro.growth case=two
```

The [baseline transcript](audits/N2-inline-match-baseline.txt) records the full
finding set, including the consequent pairing/missing-instance failures. The
baseline experiment asserts that exact set and exit 1, rather than counting any
failure as reproduction.

The normal control inventory now includes:

- `inline-match-positive`: the entire fixture must return 0 with no FAIL or
  INCOMPLETE, including its Planted comparison.
- `inline-match-changed-branch`: retains the valid pair and adds an example
  claiming the named law with the branch `y = y` instead of `y = 2`. Concrete
  evaluation hides the changed predicate, so Lean accepts the proof; with the
  operation protected this is genuinely a different contract. Require exit 1
  and exactly the retained added example's `FAIL instance-application`, including
  its original file/line identity, `law=ConcreteSpec.Repro.growth` and
  `case=changed-branch`. Missing evidence or an unrelated error cannot substitute.

Run just these two light controls (private elaboration, unchanged analysis limits):

```sh
bash scripts/checks/lib/bounded.sh 180 nix develop --command python3 -u scripts/proofs/vacuity_inline_match.py
```

## Validation

Focused results on the validated tree (base f85c2151ecfa35e916f9ee96714997d090438ed5;
consumer SHA-256 `0f47f7dec65531c7f5bbbaab94b0015926aa10a43178626edcc261e7669d5ee4`):

- [Discharge transcript](audits/N2-inline-match-existing-controls.txt): **32 existing
  discharge controls PASS**, including F3b, plus **5 certificate probes PASS**.
- [Comparison transcript](audits/N2-inline-match-comparison-controls.txt): **12 PASS**:
  the ordinary existing positive, nine other existing comparison controls, and
  the two new inline-match controls. In particular, the 32 discharge fixtures
  plus the ordinary positive give **33 existing gate fixtures passing**; the two
  new controls pass as well. No existing fixture was removed or weakened.
- [Focused inline-match transcript](audits/N2-inline-match-controls.txt): both new
  controls PASS, with the negative's complete diagnostic checked verbatim.
- Issue-ledger focused check: **5 PASS**, Python syntax and `git diff --check` PASS.

Count distinction: the solver note's **33-control completion target** includes
its *planned* `certificate-global-supplied-negative` Instance wrapper. That
wrapper is not implemented in the existing inventory. This slice does not claim
completion of that planned solver target or implement its blocked trust-policy
work; the existing executable discharge selection has 32 fixtures, not 33.

No full suite, real-source whole-scope
replay, review tooling, push, history rewrite, ROADMAP edit or ConcreteSpec edit.
The five historical C5 sites have been read, but this light slice does not claim
that the full C5 scope has been replayed; the coordinator owns that server check.
