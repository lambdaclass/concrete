# N8a baseline reproduction: slice 2a remains incomplete

Owner: R-0219. For the coordinator’s independent reviewers: **do the best
long-term thing rather than cut corners**. No review tooling was run here.

This record measures the implementation at
`6fe93229364ce94f27ade8c8e9f953d2d01a7197`, after reading its
[N2 inline-match diagnosis](N2-inline-match.md), the entire N8a review, and the
fixed phase-B exit criterion. Consumer SHA-256:
`0f47f7dec65531c7f5bbbaab94b0015926aa10a43178626edcc261e7669d5ee4`.
It is a defect reproduction, not a repaired solver,
new admission rule, containment patch, or phase-B completion claim.

## Reproductions

All seven direct observations check their exact outcome strings. Each has a fresh
original 20,000-work allowance and a 2,000,000-heartbeat bound; the allowance is
never increased. Work includes graph construction and dependency validation.

| Observation | Exact terminal reason | Work | Terms | Nodes | Templates |
| --- | --- | ---: | ---: | ---: | ---: |
| bad-chain (`bad1 → bad0`, leaf `fun _ => impossible`) | admitted (defect) | 22 | 9 | 7 | 3 |
| useImpossibleLet, applied to permitted hypothetical Empty input | admitted (defect) | 69 | 32 | 29 | 8 |
| directApp (`suppliedByBool true`) | rejected supplier=suppliedByBool | 137 | 70 | 63 | 15 |
| letApp, same actual supplied contradiction | unavailable-certificate-transfer suppliedByBool (defect) | 223 | 107 | 99 | 20 |
| cert01 (`¬ (0 = 1) := by decide`) | false-discharge-budget | 20000 | 8974 | 8183 | 52 |
| joint-simp, permitted `p`, `hp : p`, `hn : ¬p` | unavailable-certificate-transfer propext | 125 | 52 | 46 | 10 |
| F3b-simp, permitted `y`, `decide (y = 0) = true`, `¬ (y = 0)` | false-discharge-budget | 20000 | 8954 | 8163 | 52 |

Every incomplete string also checks exactly
`INCOMPLETE analysis-budget-or-error Baseline.instance stage=certificate-dependencies law=Baseline.law case=<label>`.
All seven have zero query and component counts. These are direct certificate
measurements, not whole-scope replay or projection measurements.

F1 and F2 are reproduced by the admitted poisoned chain, admitted
useImpossibleLet, and differing directApp/letApp outcomes. F3’s actual hypothetical
context is explicitly supplied to validateCertificate; it still produces the
named propext INCOMPLETE. Source inspection confirms constant applications stamp
`.supplied ... 0 id`; the dependency traversal constructs neither `.substitution`
nor `.field`. F4’s basic decide and F3b budget failures reproduce exactly. The
walker reads neither graph.nodes nor graph.templates; its parallel pending and
completed tables use expressions opened with fresh locals.

F5 has two independent private mutations: deleting the pending-cycle rejection,
and replacing the historical 80-step chain’s leaf by `fun _ => impossible`.
**All five historical certificate probes still pass under each mutation.** No
tracked consumer is modified. This is evidence that those controls are vacuous,
not an acceptable result for replacement controls. The “origin” comparison is
also visibly producer-term equality at the terminal supplier, rather than an
assertion about an entry argument. No real cycle control has been implemented.

## Historical probe costs

| Probe | Work | Terms | Nodes | Templates |
| --- | ---: | ---: | ---: | ---: |
| origin | 37 | 11 | 11 | 6 |
| non-circular | 37 | 11 | 11 | 6 |
| library-decide-matcher | 2517 | 1141 | 1079 | 38 |
| dependency-chain-baseline | 338 | 88 | 86 | 82 |
| shared-budget | 20000 | 10 | 8 | 6 |

Counts are identical for the baseline and both private mutations. All have zero
queries and components. Shared-budget includes the 19,970 work units already
spent by its containing Instance: only 30 remain for the certificate.

## Required design and unresolved boundary

The long-term R-0219 fix is one Graph/Work traversal over once-compiled closed
templates, with typed actual-input substitutions and origins across closures,
let and projections. Lambda bodies and forall codomains must be checked;
structural exclusions arise only at actual permitted empty-inductive elimination.
The parallel certificateDependency walker must be deleted, rather than repaired
alongside a second evaluator. Replacement controls need a genuine pending-evidence
cycle, a poison-sensitive dependency leaf, real decide and F3b completion, and
entry-argument origin assertions, each checking its exact planted diagnostic.
**None of that replacement is implemented by this reproduction commit.**

The existing [amendment-2 boundary](N2-discharge-solver.md#design-amendment-2-primitive-axiom-applications--owner-decision-required)
remains pending. The focused boundary probe still checks exactly:

```
INCOMPLETE analysis-budget-or-error Boundary.instance stage=certificate-dependencies law=Boundary.law case=finite-collection-simp-positive unavailable-certificate-transfer propext
```

Its source is `∀ x : Nat, x ∈ [2] → x = 2 := by simp`. It reaches propext through
congruence and equality transports. No origin-only exemption or bodyless-axiom
admission has been implemented. The current walker cannot determine whether a
particular hypothetical-derived primitive result needs no axiom-content evidence:
it does not transfer actual origins at all. The joint-simp reproduction therefore
does **not** prove that correct origin transfer still needs a new trust rule.
A hypothetical-only, undemanded propext result must be reported separately to the
coordinator if established by the replacement evaluator; it must not be turned
into a general primitive trust policy.

No owner decision is made. The boundary reproduction must not be used to claim
that the independent graph/origin repairs are impossible or completed. Their
implementation remains outstanding, as do the genuine replacement controls.

## Focused validation

The unchanged selection reports **32 discharge controls PASS, 0 FAIL**, including
the whole retained F3b fixture, followed by **5 historical certificate probes PASS**.
The latter five are not evidence of the missing slice-2a properties above.
Offline `nix develop --command` is available; direct reproductions and historical
mutation probes ran within that environment. Every child Lean invocation is
bounded by the existing command wrapper, and each outer command is bounded.
No full suite, heavy replay, review, push, force, history rewrite, ROADMAP change,
ConcreteSpec change or other-branch change was performed. C9-new and C6-current
whole-scope costs were not measured: they remain coordinator server obligations.
The fixed phase-B exit criterion and every budget are unchanged.

Reproduction commands (run each under `bash scripts/checks/lib/bounded.sh 180`):

```
nix develop --offline --command python3 -u scripts/proofs/vacuity_n8a_repro.py
nix develop --offline --command python3 -u scripts/proofs/vacuity_n8a_repro.py --historical
nix develop --offline --command python3 -u scripts/proofs/vacuity_n8a_repro.py --remove-cycle-check
nix develop --offline --command python3 -u scripts/proofs/vacuity_n8a_repro.py --poison-leaf
```
