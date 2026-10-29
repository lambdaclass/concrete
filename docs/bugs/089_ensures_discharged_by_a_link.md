# Bug 089 — an `#[ensures]` was discharged by a link, not by a checked theorem

<!-- bug-ledger
summary: `#[ensures_proof(No.Such.Theorem)]` made a false `#[ensures]` read `proved_by_lean` on contracts, vcs, ledger and audit with no Lean run, and `check-proofs` printed ✓ for any type-checking theorem; one verdict (`ensuresVerdictOf`) now decides every surface, and no link or coverage label discharges a postcondition until Exact property acceptance exists
coverage: an #[ensures] read proved_by_lean on contracts/vcs/obligation-ledger/audit whenever a link named a theorem (even No.Such.Theorem, or only #[proof_coverage(iff)]), check-proofs printed a check mark for any theorem that type-checked, and a rejected ensures theorem exited 1 under 0 failed while its receipt minted. One verdict (ensuresVerdictOf) decides every surface; nothing discharges a postcondition until Exact property acceptance. Regression vehicle is a gate: check_contract_negatives.sh section bug 089 runs tests/regressions/bug089_ensures_proof (four fixtures x every surface, check-proofs summaries/exit, receipts); see docs/bugs/089 (2026-10-18)
class: 9
silent: yes
ruling: Outside the spec: tooling — an #[ensures] was reported discharged by a link with no checked theorem
-->

**Status:** FIXED (branch `bug089-ensures-proof`, on top of the unpublished bug-080 head `00efbaf4`)
**Found:** 2026-10-18 by red-team review (`rt_ens_only`, `rt_ens`, `rt_ens_bogus`, `rt_ens_noens`).
**Severity:** a false proof on every evidence surface. This program:

```con
#[ensures_proof(No.Such.Theorem)]
#[ensures(result == 42)]
fn f(x: Int) -> Int { return x; }
```

read `proved_by_lean` on `--report contracts`, `vcs`, `obligation-ledger` and `audit`. No Lean ran,
the theorem does not exist, and the postcondition is false.

## Mechanism

Four surfaces each decided an `#[ensures]` from the registry entry alone, and none asked whether a
theorem was checked or what it said:

- `contractsReport.discharge` (ReportObligations) returned `proved_by_lean` whenever `ensuresProof`
  was set — `(full iff)` when `#[proof_coverage(iff)]` was also present.
- `collectVCs` (Report; read by `vcs`, `obligation-ledger` and `audit`) made the postcondition
  `proved_by_lean` whenever ANY theorem was linked and coverage was not `one_direction`. A plain
  `#[proof_by]` with `#[proof_coverage(iff)]` and no `#[ensures_proof]` was enough, so `vcs` said
  proved while `contracts` said missing.
- `#[proof_coverage(one_direction)]` granted "partial — one direction proved_by_lean" on the same
  basis. `weakened_postcondition` used it with `add_three_correct`, which concludes the body's value
  and nothing about `result > x`.
- `check-proofs` printed `✓ … ensures discharged by T` whenever `T`'s name did not appear in the
  Lean transcript — a substring test that passes for any theorem that type-checks, about anything.
  Its JSON hardcoded the obligation status `"proved"`. A rejected ensures theorem exited 1 under a
  summary reading `1 verified, 0 failed`, and `--report receipts` (which replayed only refinement
  targets) minted the claim's receipt current.

## Fixed

One verdict, `Report.ensuresVerdictOf` (ReportObligations), decides every `#[ensures]`, and every
surface consumes it: the contracts renderer, `collectVCs` (and through it the ledger and audit),
`concrete prove`, and the `check-proofs` ensures section. An `#[ensures]` is discharged only when the
linked theorem is kernel-accepted, is bound to this body (the bug-079 subject binding), AND concludes
the clause. No checked matcher for the third condition exists — that is ROADMAP **Exact property
acceptance** — so today no link discharges a postcondition. A linked clause reads `unproven — NOT
discharged` (status `unproven`, the link kept as a dependency), an unlinked one `missing`. A
`#[proof_coverage]` label grants nothing. A compile-time `example` pins that no verdict maps to
`proved_by_lean` or `partial`.

`check-proofs` never prints ✓ for an ensures link. A type-checking theorem prints `? … type-checks,
but is NOT a discharge` with its subject-binding result (`subjectBindingOf`) and the missing-matcher
reason. A rejected one counts in `N failed`, so the summary, the JSON summary and the exit code agree.
JSON ensures checks carry `status` `failed`/`not_discharged`/`env_failure` and `"discharged": false`.
`--report receipts` replays `#[ensures_proof]` theorems too, and withholds a claim's receipt when its
ensures theorem was rejected (`replay_does_not_witness`).

Goldens that changed, each to the honest status: `constant_time_tag` (`proved_by_lean (full iff)` →
`unproven`), `evidence_classes/partial_contract` and `contract_negatives/weakened_postcondition`
(`partial` → `unproven`). `hmac_sha256.ch` is `unproven` in `vcs`/`contracts`. `loop_invariant`'s
contracts snapshot was already stale at `00efbaf4` (that commit stopped a coverage label discharging
a loop step) and is refreshed here.

## Prevention

`check_contract_negatives.sh`, section "bug 089", runs the four repros
(`tests/regressions/bug089_ensures_proof/{ens_missing_theorem, ens_unrelated_theorem,
ens_missing_with_proof_by, ens_coverage_only}`). For each, contracts, audit, `vcs --json` and
`obligation-ledger --json` must all read `unproven`. With Lean available, `check-proofs` must print no
✓ or "discharged by" for the link; its text summary, JSON summary and both exit codes must agree on
the failure count; and receipts must be withheld for the rejected theorem but issued for the control
whose ensures theorem replays. A positive control requires the genuine refinement claim beside the
bogus link to stay `proved_by_lean`, so the legs cannot pass by refusing everything.
`check_replay_producer.sh` now requires a refused ensures theorem to be counted as failed.

## Limits

The fix withholds a verdict; it does not supply one. Genuine postcondition proofs (`ct_compare`'s
two directions, `ch_refines`) read `unproven` until Exact property acceptance builds the expected
proposition from the clause and checks the theorem's type against it. A stored receipt minted before
this fix is not re-judged against ensures replays: consumption runs no kernel.
