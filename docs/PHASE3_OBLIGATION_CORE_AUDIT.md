# Phase 3 (ObligationCore Consolidation) — Status Audit

Status: audit
Date: 2026-06-15
Scope: ROADMAP Phase 3 items #1–#19, audited against the code and gate suite.

## Purpose

Phase 3's goal is **one typed obligation ledger** (`ObligationCore`) that every
proof / contract / runtime-safety / assertion / SMT / policy / audit / report /
prove surface reads from, so no two surfaces can classify the same fact
differently. This document records the *actual* state of that migration, because
the ROADMAP Phase 3 prose has drifted from the code (see "Stale ROADMAP findings"
below) — a spec-vs-code dual-truth-source that is itself the class of problem
Phase 3 exists to kill.

## Status legend

- **DONE** — a literal hub consumer (reads/renders the unified record), or a
  structural fact that is true and gate-locked.
- **GATED-INTERIM** — still computes obligations on its own path, but a
  *consistency gate* holds its output to the ledger (same ids / statuses /
  totals). Sound today; not yet a literal view.
- **OPEN** — a parallel discharge+render path with **no** ledger-consistency
  gate. Can silently drift from the hub.
- **GAP** — a referenced gate or artifact that does not exist.

## TL;DR

The hub is real and is already the single truth source for **policy**,
**`--report vcs`**, **`--report obligation-ledger`**, **obligation JSON**, and the
**audit VC summary**. The records are unified (`abbrev VC := Report.Obligation`,
one `structure Obligation`), and `ofVC` is lossless.

`--report contracts` joined them as a literal hub view on 2026-06-16 (see below).
The two surfaces that still recompute (both consistency-gated, sound today):

| Surface | State | Why |
|---|---|---|
| `--report proof-status` | GATED-INTERIM | recomputes, but `check_obligation_report_views.sh` pins its totals to the ledger's `#prooflink` projection |
| `concrete prove` (obligation facts) | GATED-INTERIM | reconstructs, but `check_obligation_prove_views.sh` pins its ids+statuses to the ledger |
| `--report contracts` (`renderContracts`) | DONE (literal hub view) | as of 2026-06-16 reads `computeVCsDischarged` (the one ledger) and slices omega/bv proved-key sets by engine — no private discharge. Byte-identical (snapshots unchanged); locked by `check_contracts_ledger_parity.sh` + a source guard in `check_obligation_single_truth_source.sh`. Building the gate also fixed a real `*_at_callsite` vocabulary drift in `renderCallSites`. |

UPDATE (2026-06-16): `renderContracts` was refactored to consume the ledger
(#18e); the only report surface still recomputing is `--report proof-status`
(consistency-gated). The remaining literal-view work is the proof-status
renderer.

### Live evidence of the gap (now resolved)

`examples/evidence_classes/runtime_checked/snapshot/contracts.txt` was **drifted**
(`check_snapshots.sh` FAIL=1): the snapshot reported `loop @ line 85`/`100` while
the live `--report contracts` reported `line 91`/`106`. The source was edited on
2026-06-11 (the H2 commit) and the contracts snapshot was last refreshed
2026-06-06, so it had been stale for days — a pre-existing `--full` failure,
unrelated to codegen work. Refreshed 2026-06-15 alongside the parity-gate work.
It was a small but real instance of the Phase 3 thesis: contracts regenerates
from source on its own path, and nothing kept it
synced to anything except a manually-updated self-snapshot.

## Item-by-item (#1–#19)

| # | Item | Status | Evidence (code / gate) |
|---|---|---|---|
| 1 | `ObligationCore` schema v1 | DONE | `Report.Obligation` (`Report.lean:4123`); `kindVocabulary` (`ObligationCore.lean:34`) |
| 2 | Single status/evidence vocabulary | DONE | `statusVocabulary` (`ObligationCore.lean:25`); enforced by report-views gate |
| 3 | One scoped context collector | DONE | shared `hyps` threading + `loopHypsAt`/`dropStaleHyps`/`assignedScalarsS` (`Report.lean:~890–1107`); single-truth gate forbids per-family `scoped*S` collectors |
| 4 | Call-site preconditions → ledger | DONE | `callPrecondGoals` folded into `collectVCs`/`dischargeVCs` (`Main.lean:520–531`) |
| 5 | Array-bounds → ledger | DONE | `boundsObligations`/`boundsGoals` in the discharge set |
| 6 | Div/mod nonzero → ledger | DONE | `divObligations`/`divGoals`; non-negative-dividend guard preserved |
| 7 | Opt-in overflow → ledger | DONE | `overflowGoals` + bv/SMT routing; SMT only touches kernel-unproved VCs (redteam firewall) |
| 8 | `assert` / `assume` / vacuity → ledger | DONE | kinds `assert`/`assume`/`vacuity` in `kindVocabulary`; policy reads them *from the ledger* via `vacuousFunctions`/`assumeFunctions` (`ObligationCore.lean:147/154`, `Main.lean:661–662`) |
| 9 | Loop obligations O1–O5 → ledger | DONE | `loopVCGoals` in the discharge set; split status preserved |
| 10 | `#[requires/ensures/invariant/variant]` clauses → ledger | DONE | contract-clause diagnostics ride in as VCs (`ObligationCore.lean:88–91`) |
| 11 | Proof-link freshness / fingerprint / spec-drift → ledger | DONE | `ofProofStatus` + `proofLinkLedger` (`ObligationCore.lean:108`); single-truth gate checks it |
| 12 | Unified obligation expression lowering | DONE (gated) | `check_obligation_lowering.sh` (present, in Makefile) |
| 13 | Backend discharge adapters + per-adapter class gate | DONE | typed `DischargeAdapter` + `DischargeAdapter.fold` rejecting foreign classes (`Report.lean:4380+`); KERNEL-CHECKED by compile-time `example`s; behavioral gate `check_discharge_adapters.sh` (Makefile `test-discharge-adapters`). *(Corrected 2026-06-15: the gate exists under `check_discharge_adapters.sh`, not the `check_obligation_discharge_adapters.sh` name #13's text specified — the initial audit missed it by searching the literal name.)* |
| 14 | Policies consume the ledger | DONE | `computePolicyQuals` reads `vacuousFunctions`/`assumeFunctions`/`solverTrustedIds` from `ledger` (`Main.lean:659–664`); `check_obligation_policy_views.sh` |
| 15 | Reports consume the ledger | PARTIAL | `--report vcs`/`obligation-ledger`/JSON/audit/**contracts** = literal views (`--report contracts` refactored 2026-06-16, `Main.lean:677+`); `--report proof-status` = GATED-INTERIM (the remaining literal-view work) |
| 16 | `concrete prove` consumes the ledger | GATED-INTERIM | `proveReport`/`proveReportJson` (`Main.lean:974/976`) reconstruct; `check_obligation_prove_views.sh` pins to ledger |
| 17 | Migration parity gate per family | DONE | report-views + prove-views + redteam + snapshot gates serve as the parity corpus |
| 18a | Widen record to VC superset, `ofVC` lossless | DONE | `Report.Obligation` carries solver/replay/view fields (`Report.lean:4140–4149`); `ofVC` enriches only (`ObligationCore.lean:82`, proof at `:96`) |
| 18b | `--report vcs` renders from hub, byte-identical | DONE | `Report.vcsJson dvcs`; single-truth gate asserts it |
| 18c | Audit VC summary + JSON consume hub | DONE | `vcAuditSummary auditVCs`; report-views gate |
| 18d | Fold proof-status in; `Report.VC` → alias | DONE | `abbrev VC := Obligation` (`Report.lean:4153`); `check_obligation_single_truth_source.sh` |
| 18e | Delete hub-bypass shims | DONE | the last shim — `renderContracts`'s private per-family discharge — removed 2026-06-16; it now reads `computeVCsDischarged`. Source-guarded in `check_obligation_single_truth_source.sh` |
| 18f | Negative source guard | DONE | `check_obligation_single_truth_source.sh` (in Makefile + the no-duplicate-walkers gate) |
| 19 | Phase 3 validation artifact | DONE | `examples/obligation_core_probe/` + `check_phase3_obligation_core.sh` (present, in Makefile) |

## Gate inventory

Present and wired into the Makefile (`obligation-*` / `test-*` targets, lines
~167–222): `check_obligation_core`, `check_scoped_collector`,
`check_call_site_migration`, `check_bounds_migration`, `check_div_migration`,
`check_overflow_migration`, `check_assume_migration`, `check_loop_migration`,
`check_contract_clause_migration`, `check_proof_link_migration`,
`check_obligation_lowering`, **`check_discharge_adapters`** (#13),
`check_obligation_report_views`, `check_no_duplicate_obligation_walkers`,
`check_obligation_single_truth_source`, `check_obligation_policy_views`,
`check_obligation_prove_views`, `check_phase3_obligation_core`,
`check_obligation_redteam`.

**No gate is missing.** (An earlier draft of this audit claimed #13's gate was
absent; it is present as `check_discharge_adapters.sh` — the literal-name search
missed it. Per-family migration gates `check_*_migration.sh` also exist for #4–#11.)

## Current gate baseline (2026-06-15)

The obligation gates are green on their *structural* assertions. Two
pre-existing, orthogonal failures are present (both unrelated to obligation
architecture and to current codegen work):

- `check_snapshots.sh`: `contracts` snapshot drift (line-number shift only; see
  "Live evidence of the gap" above) — stale since the 2026-06-11 source edit.
- `test_prove_cli.sh`: `exit stale=3 — exit 0, want 3` — a stale-proof *exit
  code* check, part of the in-flight JSON→in-source proof-link / fingerprint
  migration, not a prove-vs-ledger drift. The prove **ledger-consistency**
  assertions pass (PROVE-CLI PASS=67).

Neither blocks the structural conclusions below.

## Stale ROADMAP findings (to reconcile)

1. **Item #18 prose is wrong now.** It says "`ObligationCore` is today a lossy
   projection of `Report.VC` (it drops `smtHash`/`smtQuery`/`solver`/
   `dischargeMode`/`leanReplay`…) … a leaf, not the hub." The records are
   unified and `ofVC` is lossless — 18a–18d are done. The sub-steps should be
   marked DONE and the "lossy leaf" framing removed.
2. **The risk note (ROADMAP lines ~256–266) is partly stale.** It claims
   "assert, vacuity, and assume obligations are still computed by report-side
   walkers outside the ledger." They are now *in* the ledger and policy reads
   them from it (#8 DONE). The accurate residual risk is narrower:
   `--report contracts` recomputes the families on a path with no
   ledger-consistency gate.
3. **#13's gate filename in the ROADMAP is wrong (not the gate).** The text
   references `check_obligation_discharge_adapters.sh`, but the gate exists and
   passes as `check_discharge_adapters.sh` (Makefile `test-discharge-adapters`),
   backed by a kernel-checked `DischargeAdapter` model. Reconciled 2026-06-15:
   #13 marked DONE with the filename caveat noted.

## Prioritized remaining work

1. **`--report contracts` literal-view refactor (#15 / #18e)** — DONE 2026-06-16.
   `renderContracts` now reads `computeVCsDischarged` and slices the proved-key
   sets by engine; byte-identical, locked by `check_contracts_ledger_parity.sh`
   + the single-truth source guard. (The parity gate, landed 2026-06-15, also
   fixed a real call-site vocabulary drift.)
2. **Reconcile the ROADMAP Phase 3 prose** (done 2026-06-15/16) so the spec stops
   being a second truth source about the truth-source migration. (#13 was found
   already done — gate `check_discharge_adapters.sh`; no new gate needed.)
3. **Remaining:** convert `--report proof-status` and `concrete prove` from
   GATED-INTERIM to literal hub views once their presentation fields live in the
   record (lower priority — both are consistency-gated, so sound today). This is
   now the only open Phase 3 literal-view work.
