Spec worker (a43b714) views, 2026-11-08 (not a decision):
1 carrier: Lean attributes resolved in env (rename-safe, names resolve, share population with axioms.sh/rules.lean); doc tags only as human mirror.
2 planted: negation not enough (top audit defect: Planted falsifies premise, C10 r4 receipt-8, C9 r3 trapLine). Structural check: mutant instantiates law's hypotheses, conclusion fails; may need law stated over a parameter (op/rule arg) so planted supplies a named mutant; gate checks shape, reviewers judge rest.
3 instance: closed terms for every hypothesis; flag hyps discharged by absurd/False.elim/nomatch.
4 exemptions: in source + generated ledger printed by gate; Helper private or suffixed; approved by contract's reviewer round.
5 one population with axioms.sh and rules.sh. Obligations listed in gate report.
