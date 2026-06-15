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

The remaining live parallel paths are the three presentation-rich / interactive
surfaces:

| Surface | State | Why |
|---|---|---|
| `--report proof-status` | GATED-INTERIM | recomputes, but `check_obligation_report_views.sh` pins its totals to the ledger's `#prooflink` projection |
| `concrete prove` (obligation facts) | GATED-INTERIM | reconstructs, but `check_obligation_prove_views.sh` pins its ids+statuses to the ledger |
| **`--report contracts`** (`renderContracts`) | **OPEN** | full independent walk+discharge (`vacuityGoals`/`assertGoals`/`boundsGoals`/`divGoals`/`overflowGoals` → its own `kernelDischargeLoopVCs`); **no ledger-consistency gate** (an output snapshot exists but pins contracts to *itself*, not the hub — and is currently stale: see below) |

So the single highest-value remaining Phase 3 increment is **making
`--report contracts` a ledger consumer** (or at minimum adding a ledger-parity
gate), because it is the only obligation surface that can disagree with the hub
with nothing to catch it.

### Live evidence of the gap (current, pre-existing)

`examples/evidence_classes/runtime_checked/snapshot/contracts.txt` is **drifted**
(`check_snapshots.sh` FAIL=1): the snapshot reports `loop @ line 85`/`100` while
the live `--report contracts` reports `line 91`/`106`. The source was edited on
2026-06-11 (the H2 commit) and the contracts snapshot was last refreshed
2026-06-06, so it has been stale for days — a pre-existing `--full` failure,
unrelated to current codegen work. It is a small but real instance of the Phase 3
thesis: contracts regenerates from source on its own path, and nothing keeps it
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
| 13 | Backend discharge adapters + per-adapter class gate | PARTIAL / **GAP** | adapter *firewall* property covered by `check_obligation_redteam.sh` (solver↛kernel, kernel↛solver); but the dedicated **`check_obligation_discharge_adapters.sh` is MISSING** |
| 14 | Policies consume the ledger | DONE | `computePolicyQuals` reads `vacuousFunctions`/`assumeFunctions`/`solverTrustedIds` from `ledger` (`Main.lean:659–664`); `check_obligation_policy_views.sh` |
| 15 | Reports consume the ledger | PARTIAL | `--report vcs`/`obligation-ledger`/JSON/audit = literal views (`Main.lean:997–1006`); `--report proof-status` = GATED-INTERIM; **`--report contracts` = OPEN** (`renderContracts`, `Main.lean:669–694`) |
| 16 | `concrete prove` consumes the ledger | GATED-INTERIM | `proveReport`/`proveReportJson` (`Main.lean:974/976`) reconstruct; `check_obligation_prove_views.sh` pins to ledger |
| 17 | Migration parity gate per family | DONE | report-views + prove-views + redteam + snapshot gates serve as the parity corpus |
| 18a | Widen record to VC superset, `ofVC` lossless | DONE | `Report.Obligation` carries solver/replay/view fields (`Report.lean:4140–4149`); `ofVC` enriches only (`ObligationCore.lean:82`, proof at `:96`) |
| 18b | `--report vcs` renders from hub, byte-identical | DONE | `Report.vcsJson dvcs`; single-truth gate asserts it |
| 18c | Audit VC summary + JSON consume hub | DONE | `vcAuditSummary auditVCs`; report-views gate |
| 18d | Fold proof-status in; `Report.VC` → alias | DONE | `abbrev VC := Obligation` (`Report.lean:4153`); `check_obligation_single_truth_source.sh` |
| 18e | Delete hub-bypass shims | PARTIAL | `renderContracts` is still a hub-bypassing parallel discharge path (the remaining shim) |
| 18f | Negative source guard | DONE | `check_obligation_single_truth_source.sh` (in Makefile + the no-duplicate-walkers gate) |
| 19 | Phase 3 validation artifact | DONE | `examples/obligation_core_probe/` + `check_phase3_obligation_core.sh` (present, in Makefile) |

## Gate inventory

Present and wired into the Makefile (`obligation-*` targets, lines ~168–222):
`check_obligation_core`, `check_obligation_lowering`, `check_obligation_report_views`,
`check_no_duplicate_obligation_walkers`, `check_obligation_single_truth_source`,
`check_obligation_policy_views`, `check_obligation_prove_views`,
`check_phase3_obligation_core`, `check_obligation_redteam`.

**Missing:** `check_obligation_discharge_adapters.sh` (ROADMAP #13). The
soundness property it would assert (no adapter emits a stronger evidence class
than its tier) is *partially* covered by the redteam firewall, but there is no
dedicated per-adapter gate.

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
3. **#13's gate is referenced as if it exists.** It does not; either write
   `check_obligation_discharge_adapters.sh` or fold its assertions into redteam
   explicitly and update the ROADMAP.

## Prioritized remaining work

1. **Close the `--report contracts` truth-source gap (#15 / #18e).** Either make
   `renderContracts` render from the ledger, or — if its presentation-rich
   contract-clause fields don't yet live in the record — first widen the record
   with those fields, then convert. Minimum acceptable interim: add a
   ledger-parity gate (the proof-status pattern in `check_obligation_report_views.sh`)
   so contracts cannot silently drift. **Highest value: it is the only OPEN
   surface.**
2. **Add `check_obligation_discharge_adapters.sh` (#13)** or formally retire it
   into redteam, and reconcile the ROADMAP.
3. **Reconcile the ROADMAP Phase 3 prose** (items above) so the spec stops being
   a second truth source about the truth-source migration.
4. Convert `--report proof-status` and `concrete prove` from GATED-INTERIM to
   literal hub views once their presentation fields live in the record (lower
   priority — both are consistency-gated, so sound today).
