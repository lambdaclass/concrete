# Roadmap History

Status: archive — historical execution records moved out of [ROADMAP.md](../../ROADMAP.md).
Source: `ROADMAP.md` at commit `a159c489` (2026-10-19), moved verbatim; only heading levels and relative
links were adjusted for this location.

Nothing here schedules work, states current status or overrides the
[current execution queue](../../ROADMAP.md#the-current-execution-queue). `ACTIVE`, `OPEN`,
`PENDING`, "remaining" and "next" describe the date each record was written. The 2026-10-19
[reconciliation](../project/ROADMAP_RECONCILIATION.md) mapped every live item in these sections to an
owning task in the roadmap (its prose inventory cites these sections by their line numbers at
`7b134645`); an item it recorded as done or historical is closed here.

Sections, in their original order:

- [Current Execution State (2026-08-31)](#current-execution-state-2026-08-31) — from top-level section
- [Rationale notes: the prover-neutral arc (R-0004, R-0450, R-0448, R-0454–R-0456, R-0458–R-0464, R-0467, R-0468)](#rationale-notes-the-prover-neutral-arc-r-0004-r-0450-r-0448-r-0454r-0456-r-0458r-0464-r-0467-r-0468) — from subsection of "Product north star: compositional, transferable evidence"
- [Historical status board (2026-08-08) — superseded](#historical-status-board-2026-08-08--superseded) — from top-level section
- [Branch `spike/non-arithmetic-multi-kernel` (depends on `spike/multi-prover-evidence`)](#branch-spikenon-arithmetic-multi-kernel-depends-on-spikemulti-prover-evidence) — from top-level section
- [PRE-MERGE STATE of `spike/multi-prover-evidence` (recorded 2026-08-05)](#pre-merge-state-of-spikemulti-prover-evidence-recorded-2026-08-05) — from top-level section
- [Outstanding work recorded 2026-08-04](#outstanding-work-recorded-2026-08-04) — from top-level section
- [Compiler Pipeline Spine](#compiler-pipeline-spine) — from subsection of "Cross-Cutting Decisions"
- [External-Validation Gate](#external-validation-gate) — from subsection of "Cross-Cutting Decisions"
- [Native Debug Info](#native-debug-info) — from subsection of "Cross-Cutting Decisions"
- [Global Task Sequence](#global-task-sequence) — from subsection of Phase 7 (Standard Library And Core APIs)
- [Strict post-R-0004 critical path](#strict-post-r-0004-critical-path) — from paragraph under "Post-R-0004 milestones and longer-term research" (Phase 7)
- [Historical record and detailed slice traceability](#historical-record-and-detailed-slice-traceability) — from R-0004 historical material under "Post-R-0004 milestones and longer-term research" (Phase 7)
- [Binder frames: position and meaning are separate obligations](#binder-frames-position-and-meaning-are-separate-obligations) — from R-0004 historical material under "Post-R-0004 milestones and longer-term research" (Phase 7)
- [Multi-kernel: where the ladder actually stands (2026-08-08)](#multi-kernel-where-the-ladder-actually-stands-2026-08-08) — from subsection of Phase 7 (Standard Library And Core APIs)
- [R-0480: physical documentation migration (2026-08-14)](#r-0480-physical-documentation-migration-2026-08-14) — from the body of R-0480
- [R-0473: the embedded R-0004 Slice-6 record (2026-08-07 – 2026-09-02)](#r-0473-the-embedded-r-0004-slice-6-record-2026-08-07--2026-09-02) — from the body of R-0473
- [R-0475: exposure audit and shipped snapshot fix (2026-08-07)](#r-0475-exposure-audit-and-shipped-snapshot-fix-2026-08-07) — from the body of R-0475

## Current Execution State (2026-08-31)

### Post-R-0004 mutation qualification — QUALIFIED 2026-10-11 at `670ec85a`

**Qualified:** dispatched run
[38104755470](https://github.com/unbalancedparentheses/concrete2/actions/runs/38104755470) on the
published `670ec85a`, 24 shards with a fail-closed aggregate: 95 discovered = selected = executed =
reported = killed, 0 invalid, 0 survived, 0 could-not-apply, every clean-tree baseline green,
`completed=1`, `integrity_ok=1`, `qualified=1`. The aggregator re-run locally over the downloaded
shard artifacts reproduced the CI verdict exactly. Slowest shard 92m; 14.1 runner-hours, 8.3 of them
in the 21 families gated by `check_dependency_edges.sh`. The history below is kept as the record of
how the campaign got there; R-0208 has since closed (2026-10-13, `2b570c5a`).

**Re-qualification for the next integration (2026-10-11).** The batch integrating R-0208 slice 1a
adds seven families (inventory 102) and changes `check_dependency_edges.sh`, the gate of 21
families: its probes now run in 18 Lean processes instead of 244 and each compiler result is
computed once per run, 305 s → 34 s locally with the same 317 assertions (`0a7002b0`; six of its
families hand-verified red). The same batch makes the pre-push hook run one deduplicated gate
list and reuse a validation record bound to the exact tree, toolchain bytes, C compiler, gate
definitions, selection and changed files. `qualified=1` above describes `670ec85a` only; the
integrated commit needs its own 102-family campaign. Shard balancing by measured timings is
deferred: it is not built or tested, and qualification does not wait for it.

R-0004 remains **closed** by the protected 205/205 serial run recorded below and in
[CHANGELOG.md](../../CHANGELOG.md). The current work is a stronger post-closure question: can every
registered evidence-boundary mutation be shown to cause its intended production gate to fail for
the mutation itself, rather than because a build, precondition, runner, or summary failed?

The hardened harness is shipped at `898d9a7b`. It now requires a green pristine baseline, a
pristine build, exact and unique anchor application, authenticated gate completion, and a causal
restore/rebuild/reapply red-green-red confirmation. Mutations run in disposable workspaces.

The complete diagnostic campaign against `898d9a7b` ran and reported all 81 families. Its preserved
record is `mutation-campaign-summary.898d9a7b`, with the 173-line trace retained as
`campaign_full.898d9a7b.log`:

```text
73 killed (all causally confirmed red -> green -> red)
 6 invalid experiments
 2 survived
 0 could_not_apply
```

This is a trustworthy **diagnosis**, not qualification. The two survivors are
`freshfacts-requires-proved-status` and `freshfacts-carries-trusted-boundaries`; the receipt-
consumption gate has no live trusted-boundary receipt and cannot yet distinguish the intended fresh-
fact refusal from a material mismatch. The invalid experiments are `trap-quotient-condition`,
`attestation-precondition`, `kernel-foundation`, `reference-division`, `divergence-detection`, and
`transform-has-effect`. They establish nothing until retained logs support an attributable route or
the mutation is rewritten/reclassified honestly. **Update 2026-10-11:** the first complete sharded campaign
(`1f2fe0f1`, run 38097946883) killed all six with 0 invalid (five as declared build kills, `divergence-detection` by
its own gate); the two survivors
remained, and `964073e7` closes both with a live trusted-boundary receipt and a status-only stale claim.

The historical artifact says `completed=0` even though all 81 units reported, because its schema
made zero failed families a condition of completion. It also names `head_changed`—the control
checkout moved from `898d9a7b` to the documentation-only `89a2bcb1` while the immutable disposable
workspace remained bound to `898d9a7b`—and
`families_unaccounted(gate=73 build=0 of 81)`. Neither refusal is rewritten after the fact. Under the
correct semantics this run is `completed=1`, `qualified=0`; the old artifact remains the historical
record of the old schema.

**Schema separation shipped at `98dee5e3`.** The artifact now records `mode`, `discovered`,
`selected`, `executed`, `reported`, `killed`, `invalid`, `survived`, `could_not_apply`,
`integrity_ok`, `completed`, and `qualified` separately. A single-family probe can complete and exit
according to its selected result but can never claim campaign qualification. Live controls cover a
killed single family and a surviving one with retained failure logs. Before the next full campaign,
the pure reconciliation matrix must also demonstrate the campaign-positive branch and the
complete-but-unqualified, integrity-refused, and interrupted branches without spending hours to
reach them.

**Performance is part of this checkpoint, without weakening causality.** Before another full run,
record per-family time spent copying, building, running gates and confirming. Replace redundant
confirmation rebuilds only with content-bound snapshots that pair complete source and build state;
never swap `.lake` alone. Reject stale, corrupt, wrong-mutation and source/artifact-mismatched
snapshots. Then benchmark isolated workers (starting at 1, 2 and 4), with separate source/build/temp/
process state and serial/parallel disposition agreement. A fast affected-family mode may be
non-qualifying; the authoritative mode retains full red-green-red confirmation.

The checkpoint exits only when one clean pushed HEAD reports all of the following separately:

- `completed=1`: the driver reached reconciliation and every selected family reported;
- discovered = selected = executed = reported = killed = 95 (the live inventory; 91 when this list was written);
- `invalid=0`, `survived=0`, `could_not_apply=0`, and `integrity_ok=1`;
- `qualified=1`: all 95 families were killed by their intended attributable gate/build outcome;
- the artifact and human summary agree, and the repository is clean afterward.

Only then does implementation run R-0208's required toolchain/revocation drill and move to R-0482's
full-row identity freeze and five-way identity separation. A product-authority defect found by the
campaign is fixed before moving on; a harness defect does not retroactively become an R-0004 product
defect, and an interrupted run is never evidence.


#### Harness-hardening checkpoint — green under registered gates, explicitly unqualified

This checkpoint pushes a harness whose registered gates are green and whose known limitations are
recorded here. **It claims nothing about qualification.** There is no qualified 91-family campaign at
this HEAD, the receipt schema is not frozen, F7 and F9 are not complete, and the post-R-0004
qualification programme is not finished.

Six adversarial review rounds were run against this work. Roughly half of each round's findings were
defects introduced while repairing the previous round, and three separate times a repair produced a
gate that REFUSED CORRECT RUNS — the evidence-set check, a `refusals` predicate, and a `gates_proven`
check that would have rejected the qualifying campaign this programme exists to produce. Each was
found by running, not by reading. The review policy going forward is bounded: design review, one
adversarial implementation review, one confirmation review, then push on green gates with no known
authority-breaking finding, carrying further hardening as tracked work.

The following findings are known, reproducible, and deliberately not fixed in this checkpoint. Each
is named with the authority it affects.

| Finding | Authority impact |
| --- | --- |
| **H-1 Signal during launch releases the lock.** The holding state is set when the foreground launcher RETURNS. A supervisor signalled while waiting exits with the state unset, which maps to `no_child_launched` and releases. | Concurrency, not verdict content. A second run could start while a campaign group survives. Cannot produce a false `qualified=1`; can produce two writers. |
| **H-2 A clean full-campaign exit is not cross-checked against `qualified=1`.** `candidate_incoherent` accepts a `mode=campaign` record with `qualified=0`, the supervisor checks only `completed` and `integrity_ok`, publishes `qualified=0`, and exits 0. | Under-claiming only. The published record is honest; the exit status does not distinguish "campaign ran and did not qualify" from "campaign ran and qualified". Blocks nothing that could over-claim. |
| **H-3 CLOSED. Nine identity fields were child-controlled; seven are now supervisor-observed and two are declared non-authoritative.** The four driver/inventory digests are compared against `ts_driver_digest`/`ts_inventory_digest` computed by the supervisor (OBSERVED); the three workspace fields must agree with the observed repository digests (CROSS-FIELD). `baseline_compiler_sha` and `compilers_tested` **cannot** be observed — that compiler existed only inside a disposable workspace the supervisor never enters and which is deleted before reconciliation — so they are declared NON-AUTHORITATIVE in `CAMPAIGN_FIELDS_NON_AUTHORITATIVE` and support no qualification or provenance claim. | **Resolved.** A well-formed child-reported digest does not prove which compiler ran, so it is labelled rather than trusted. All 45 published fields now carry a declared authority status and the partition is asserted exact, so a new field cannot be added without classifying it. A control asserts qualification does not move when the two non-authoritative fields do; mutations making qualification consult one, adding an unclassified field, and silently demoting a third field to shape-only were each confirmed to turn the gate red. |
| **H-4 Golden freshness has a residual TOCTOU.** The run id is read from the live shared artifact and the bytes are copied afterwards; the copy's own `run_id` is never re-checked. | Test-infrastructure only. Detects the stale-artifact case that actually occurred; a concurrent replacement inside the window is not excluded. No authority over campaign verdicts. |
| **H-5 CLOSED. Production wiring is now mutation-covered.** The reconciliation body was inline in the driver where no gate could reach it; it is now `supervisor_reconcile_and_publish` in the decision library, driven by thirteen controls that run the real publication path against a sandbox. Six families sever one call site each — provenance, run-binding, incoherence, group-state, launcher report, evidence root — leaving the decision function intact. | **Resolved.** Each family was measured pristine-green then mutated-red on a disposable copy, red on the control naming its own call site. Building the harness also found the body locating its inventory through `$0`, which was correct only while the code lived in the driver; the driver now names itself through `CAMPAIGN_DRIVER` and unset is a refusal. The inventory is **91 families**, not 85. |
| **H-6 Mint accounting proves co-occurrence, not causation.** Each case requires a nonzero exit AND a named failure line, but never establishes that the dependency-edge gate is green WITHOUT its self-test hook. An unrelated permanent failure could supply the red exit. | Test-infrastructure. Mitigated in practice by `DEPENDENCY-EDGES` passing 317/0 in the same session, which is that missing positive control run separately rather than inside the gate. |
| **H-7 `refusals` mixes two kinds of value.** It is published as `$REFUSALS$SCOPE_NOTES` — integrity refusals concatenated with scope annotations — so no predicate over the field alone can separate them. | Currently sound, by argument that must be preserved: the one authority consumer is inside `candidate_incoherent`'s qualification branch, qualification requires `mode=campaign`, and `mode=campaign` holds exactly when `ONLY` is unset, which is exactly when `SCOPE_NOTES` is empty. So the consumer only ever reads a pure integrity value. **Split the field before the first `qualified=1` campaign and before schema conformance/freeze**, because that argument depends on an invariant no gate currently enforces. |
| **H-9 Lock ownership is not comparable across PID namespaces.** A campaign run inside a sandbox with `--unshare-pid` records its namespace-local pid — observed here as `pid=2` — and the host reclaim logic then tests pid 2, which is `kthreadd` and always alive. | **One-writer guarantee.** A lock stranded by a sandboxed run can never be reclaimed, because its owner always looks alive; and the dead-creator reclaim path is defeated in the other direction. Observed live, not hypothesised: a concurrent sandboxed campaign held `.gate.lock` with `pid=2 cmd=check_gate_mutation_coverage.sh`. Record the namespace identity, or a start-time-qualified owner token, alongside the pid. |

### R-0004 — CLOSED (2026-08-20)

Closed on a protected serial run, not on a claim. The authorizing artifact is
`.ci-gates-summary` from a run at `a839d519`:

```
completed=1  mode=closure  jobs=1
discovered=205  executed=205  passed=205  failed=0
```

with no `refusals=` line, a clean working tree, and `HEAD == origin/main`. Every integrity
dimension held: serial execution, unchanged HEAD, unchanged tracked and untracked state,
unchanged gate inventory, unchanged compiler identity, mutation targets clean at both ends, and
no interruption.

#### What "closed" means here

The friendly verdict `proved` now requires a current exact subject, a complete dependency root,
explicit trust and assumptions, and a receipt that only a successful kernel replay can mint.
`ProofEvidenceReceipt` has no total constructor: the chain `replay → ReplayResult →
SuccessfulReplay.of? → mint` makes "unchecked facts became a receipt" fail to typecheck rather
than fail a test.

#### Final denominators

| Population | Value |
|---|---|
| gates discovered = executed | 205 / 205, zero failures |
| corpus census | 35 proved, 0 dependency-closure-unjustified |
| stored proof links | 55 v2 / 10 v1 of 65, across `examples/`, `tests/` and `std/` |
| V1 body-fingerprint golden | 77 extracted, unmoved under V2 |
| compiler-source inventory | 96 files (95 Lean + 2 build files − 1 named exclusion) |
| registered mutation families at closure | 78 (historical denominator; the post-closure registry now declares 81) |
| subject accounting | 86/48 frozen, tracked separately from dependency accounting |

The 10 v1 links are pinned exactly rather than required to be zero: their v2 subject is
INCOMPLETE because the structural producer refuses on intrinsic casts, so no digest is minted
and there is nothing to migrate to. That is a remaining producer gap, recorded so it cannot grow
silently.

#### Bugs closed by the V2 activation

**059** (the subject omitted declared types) and **060** (contracts sat outside it) are FIXED and
verified behaviourally: a whole-signature `i32 -> u32` change and an added `#[ensures]` both stale
the claim. Both legs were tripwires asserting the WRONG verdict on purpose while the defects were
open; both fired, and both are inverted rather than deleted, so a reversion is caught by the
assertion that proved the fix.

#### The independent theorem-retargeting finding

`proof_pressure.validate_header` carried
`#[proof_by(Examples.ElfHeader.Proofs.validate_header_correct)]` — a theorem about `elf_header`'s
`validate_header`, which checks ELF magic/class/data/version bytes, attached to a function that
checks a nonce and calls `check_nonce`. They shared a qualified name and nothing else, and the
link passed every name-based check for months. Correspondence caught it: `elfFns` holds no
`check_nonce` model, so the edge had no justification, and the scoped join then showed the refusal
was about identity rather than luck.

Repaired by DELETING the claim, not by attaching a different theorem. No theorem proves that body,
and substituting the nearest plausible one is the defect being repaired. The population is
genuinely one smaller: stored links 44 → 43, and every pinned denominator that counted it was
corrected rather than left to drift.

#### Non-blocking limitations, recorded rather than resolved

**1. PackageIdentity over-binds, at two levels.** It contains the complete dependency closure,
which dependency roots already model, and it scopes `DefinitionIdentity` by package CONTENT.
Both were demonstrated, not theorised:

- migrating two lines in `std/src/base64.con` moved the identity of packages that never mention
  std, because it is auto-injected everywhere. `GeneratedAttestations.lean` went 168 lines stale
  in one step, three subjects silently dropped from `proved` to `dependency closure unjustified`,
  and 38 references had to be remapped — with no implementation digest changing anywhere.
  MEASURED by `check_identity_overbinding.sh`: 7 of 21 package identities move. An earlier
  version of this record said "every package identity", which overstated it; one package is
  unaffected.
- deleting a wrong `#[proof_by]` from one function renamed the attestation symbol of a DIFFERENT
  function in the same package, whose module, declaration and implementation digest were all
  unchanged.

Fail-safe rather than unsound: the failure mode is demotion and symbol renaming, never a proof
credited to a body it does not describe. The redesign — PackageScopeIdentity, PackageArtifactIdentity,
ResolutionContextIdentity, DefinitionIdentity, claim dependency root — is the first post-R-0004
slice.

**2. Post-build executable bytes are unclaimed.** The compiler identity is computed at build time
over an explicit 96-file source inventory. A binary PATCHED AFTER BUILD keeps the constant its
sources produced. This identifies the BUILD, not the bytes. The executable digest that would bind
the bytes was removed because it was not portable — neither `/proc/self/exe` nor GNU `sha256sum`
exists on macOS, and receipt issuance refused there outright. A portable streaming digest or a
signed build manifest is post-R-0004 provenance work and is not claimed today.

#### What the closure run does NOT cover

The protected 205-gate run did not include the multi-hour mutation campaign. Its closure claim is
therefore the production-gate and adversarial boundary above, not “every registered mutation was
causally killed.” The registry contained 78 families at closure and contains 85 at the current
post-closure checkpoint.

Subsequent review showed that anchor presence, a red exit, and a completion line were each too weak
on their own: multi-line anchors could match only one line; a mutation could fail to apply or merely
break the build; a gate could be red before mutation; source restoration could leave a mutated
binary; and an empty result could be counted as success. The harness at `898d9a7b` now requires the
stronger causal controls described in the current execution state.

No **qualifying** current-inventory result (95 families) exists yet. The complete 81-family diagnostic
run at `898d9a7b` reported 73 causal kills, 6 invalid experiments, 2 survivors and zero
could-not-apply results. Its immutable
workspace stayed bound to that SHA, so the family dispositions are diagnostic evidence; the control
checkout's concurrent move to `89a2bcb1`, the eight unresolved families, and the old schema's
completion/qualification conflation correctly prevent it from discharging this checkpoint. The
preserved historical artifact is not rewritten to use the schema shipped later at `98dee5e3`.

### R-0004 evidence producer — landed state (2026-08-05)

> **[historical] Reconciled 2026-10-19:** R-0004 closed 2026-08-20; this is its implementation record. Its live remainders are mapped to owning tasks in the
> [reconciliation record](../project/ROADMAP_RECONCILIATION.md); nothing in it schedules work.

Canonical inventory of what exists, so the next session does not re-derive it. The
constructor-by-constructor classification lives in `docs/verification/EVIDENCE_PRODUCER_MATRIX.md`
(gated against the AST, so it cannot silently omit a form).

**Producer.** `elabExprEv` / `elabStmtEv` / `elabStmtsEv` / `elabCallEv` are ONE producer of
Core and evidence. All 104 semantic child edges thread evidence; zero Core-only projections
remain inside the mutual block, asserted by `check_binder_refs`. Callee identity is an
OUTPUT of the resolution that selects runtime behaviour — one owner, installed at ElabEnv
construction so there is no fail-open window.

**Vocabulary now real, not gaps.** Literals with semantic width, binder references as
relative lexical positions, constants, fn references, operators, calls with arguments in
evaluation order, field access, struct literals, enum/variant literals, match with arm
patterns and guards, arrays, index, deref, borrow, cast, try, defer, assert, assume.

**Loop frames.** `ElabEnv.loopFrames` tracks enclosing loops innermost-first;
`break`/`continue` carry a RELATIVE depth, never a label spelling, and both failure modes
(no enclosing loop, label matching none) refuse rather than defaulting to 0. The frame
covers the loop BODY only — a `break` in a while condition is not inside that loop.

**Structural serializer.** `bodyBytesV2` accepts only `CompleteEvidenceBodyV2`, so a gap
reason cannot reach digest bytes — a type error, not a rule. Distinct programs now produce
distinct bytes: operators, nesting, argument order, field selection, variant selection and
arm order all discriminate. The flat identity-use serializer could do none of that.

**A latent bug worth remembering.** `ElaboratedExprV2` was declared in the Proof layer where
`CExpr` is not in scope, so Lean AUTO-BOUND `CExpr` as an implicit type variable: the
structure was silently polymorphic over a made-up type with its field in `Prop`. It sat in
published, CI-green code because NOTHING HAD EVER CONSTRUCTED IT — the gates exercised the
tree in isolation. A type nobody builds is a type nobody has checked. Fixed by moving the
pairing types to the Elab layer, which is also the correct layering, and gated both ways.

**Remaining, in order.** Each is an entry in `check_convergence_inventory.sh`:

1. ~~Delete the independent `bodyIdentityUses` accumulator.~~ Done. `bodyIdentityInputs.uses`
   is now `flatUsesOf` of the structural evidence body, so there is one producer.

   Worth recording that this was not a relocation. The criterion was CONTAINMENT, not
   equality, and the derived view turned out to be strictly richer: it records an assignment
   TARGET as a use of a binder (`i = i + 1` and `j = i + 1` had the same flat view before)
   and a match pattern's FIELD identity (renaming `v` in the enum declaration moved nothing).
   Two gates pinned the old lossy numbers and were updated to assert the recovered
   identities — `check_type_identity` 7 → 10 and `check_binder_refs` 4 → 5 — and three
   mutation entries were repointed from the accumulator to the structural builder, each
   re-verified as KILLED (gate) rather than KILLED (build).

   What survives the deletion is the COVERAGE axis, which is a different fact: an unresolved
   identity contributes nothing to the use list, so a body missing an identity and a body
   that never mentioned one are indistinguishable there. Only the explicit mark separates
   them, and that is what makes the subject fail closed.
2. ~~Switch shadow mode to `bodyBytesV2`.~~ Done — as an ADDITIONAL line, not a
   replacement. `--report subject-facts` prints both `shadow identityUses` and
   `shadow bodyV2`, because they answer different questions: the flat digest merges
   `p + 1`, `p * 2` and `p - 9`, the structural one separates them, and dropping the flat
   line would remove the comparison the shadow period exists for. Both stay
   non-authoritative; the AUTHORITATIVE comparison is still V1-frozen (deliberately, until
   step 7 — the V2 digest exists and is observed in shadow; see the headline status above), and
   `check_shadow_body_v2.sh` asserts `bodyBytesV2` reaches nothing but the report.

   First measurement of the real corpus: **292 of 432 subjects digest, 0 absent** (316 after 2a wired the for-loop). Every
   subject has a structural body threaded, so what remains is the producer declining to
   describe constructs, not the body failing to arrive. The refusals name the construct
   rather than the gap code, which turns them into the work list:

   | count | construct |
   |---|---|
   | 54 | desugared for-loop |
   | 43 | array literal: element `TypeId` not minted here |
   | 32 | assert: predicate not elaborated |
   | 20 | callee not resolvable to a `CallableId` |
   | 11 | cast: target `TypeId` not minted here |
   | 7 | assume: predicate not elaborated |
   | 6 | self argument |
   | 4 | if-expression: statement evidence not wired |
   | 4 | `discard()`: desugared to a no-op if |
   | 2 | field place: `FieldId` not minted here |

   The 292 is a ratchet floor, not a target. A drop means a construct that used to be
   described stopped being described.

2a. Close those refusals, heaviest first.

   **Done: the desugared for-loop** (54 refusals). Its evidence mirrors the LOWERING —
   `for (init; cond; step) { body }` becomes a `block` holding the init and a `loop` over
   `body ++ step` — because describing the surface form instead would record a loop whose
   body omits code the program runs. Every part was already elaborated at the site; only
   the evidence was discarded.

   Wired together with a defect it was hiding: `while_` pushes `loopFrames` around its body
   so `break`/`continue` resolve to a relative loop depth, and `forLoop` never did. That was
   invisible while the construct was a gap, and wiring the node alone would have converted a
   hidden defect into an `unresolvedLoopTarget` on every `break` in a for-loop.

   **A gap node discards its subtree**, which changes how the table above must be read.
   Closing 54 refusals raised coverage by 24, not 54: the other 30 bodies contained a second
   unhandled construct the for-loop gap had been hiding, and the cast row rose 11 → 23 for
   exactly that reason. The per-construct counts are a LOWER BOUND on remaining work, never
   a partition of it. Corpus now **316 of 432**.

   **Done: the type vocabulary** (array literal 43 + cast 23). DECIDED and built:
   `EvidenceTypeRef`, structural over primitives and composites AND nominal-by-identity.

   `TypeId` names only nominal types and cannot express `Int` or `[Int; 3]`; `tyCanonical`
   and `boundTyCanonical` cover primitives but render a named type by its SOURCE SPELLING
   (`.named n => n`), which is exactly what `TypeId` exists to prevent — reusing either
   would have put a spelling into versioned bytes. So: primitives enumerated (not free
   tags, so a new primitive is a compile error rather than an unclassified byte), nominal
   types by `TypeId`, and TYPE VARIABLES BY BINDER POSITION on the same rule as
   `binderRef` — renaming `T` to `Zed` leaves the digest identical, gated.

   At that historical checkpoint this applied to four positions that named an arbitrary type —
   `cast`, `arrayLit`, the since-removed postfix-propagation node, and `letBind`. The current tree
   has no propagation node. `structLit` and `structPat` keep `TypeId`, because those genuinely name
   a nominal type.

   FUNCTION TYPES are deliberately still a gap: encoding one means encoding a capability
   SET whose variables are identified against a different binder list, which is a second
   identity question and should be decided on its own. Zero occurrences in the corpus.

   Fixed a FAIL-OPEN found while wiring this: the gap traversal discarded the type field
   with `_`, so `typeRefGaps` was unreachable and a body with an unresolvable type
   validated as COMPLETE. Silent, because it is not a type error. Now gated by a fn-typed
   probe that must be refused.

   Corpus **376 of 432**, with no residual `unresolvedType` gaps anywhere.

   **Done: the assembly rows** (field place, `self` receiver, `discard()`, if-expression).
   Each discarded information the site had already computed:

   - the field ASSIGNMENT place had no `FieldId`, so every field write in the corpus was
     described identically. It now carries owner and field name, and tracks the heap-shell
     deref, because writing through a dereference is not the same write.
   - the METHOD RECEIVER was a gap in the named-type branch while the trait-bound branch
     beside it already threaded `cObjEv.evidence`. One match now yields both the Core
     receiver and its evidence, so the borrow cannot drift from what is recorded.
   - `discard(e)` is described as the intrinsic call it is, WITH its argument.
   - the IF-EXPRESSION got its own constructor and byte tag, separate from the statement
     `branch`. One value-bearing, one not; they must not share an encoding.

   Corpus **323 of 432** at this point.

   (`assert` was briefly listed as an assembly row here. It is not: its predicate is
   deliberately unelaborated because it may legally read ghost bindings, which
   `elabExprEv` rejects.)

   **Done: proof predicates** (39). `assert`/`assume` are now elaborated for EVIDENCE ONLY
   — the Core is discarded, because a proof-only construct must not generate code — under
   two constraints. It cannot fail the compilation: nothing elaborated these before, so a
   throw would turn a program that used to build into one that does not, and a predicate
   reading a `ghost let` becomes a typed gap instead (gated with exactly that probe). And
   it cannot leak state: the environment is restored, with `freshBinder` carried forward
   deliberately so Core names stay unique even when the expression that minted them was
   discarded.

   The ASSUMPTION AXIS now sees an assume. Before this it counted zero for the only
   construct that feeds it — vacuous, though fail-closed, since the gap refused the
   subject anyway.

   **Bug 068, found while scoping the predicate work and not by any gate.** A `ghost let`
   and a runtime `let` produced IDENTICAL body evidence. A ghost binding is ERASED before
   Core — its initializer never runs and cannot trap — so those are different programs, and
   a proof over one stayed valid-looking after a change to the other. `EvidenceStmtV2.letBind`
   now carries `isGhost`, on the same principle as `exprStmt`'s `isValue`: the distinction
   that decides whether code RUNS belongs in the bytes. Gated and mutation-killed.

   **Done: imported impl methods** (18 of the last 21). `importedCallIds` searched only a
   module's FUNCTIONS, so a method on an imported type — `w.write_str()` where `Writer`
   comes from `std.io` — had no `CallableId`. Not a resolution defect: the table never
   held the entries. Keyed by local name, identified by defining module, so `a.P_get` and
   `b.P_get` stay distinct (gated, mutation-verified). The alias half of that split is
   unreachable today: Check rejects a method call on an aliased imported type (E0264),
   before and after this change alike.

   Corpus **421 of 432**.

   **The 11 remaining are three distinct causes**, each named at its own site instead of
   sharing one message that read as a lookup failure everywhere:

   - **5 incomplete callee identity** — a generic callable whose type arguments are not
     recorded. `isComplete` refuses it, and should: an identity missing its type arguments
     looks resolved and is not.
   - **4 method calls** whose mangled name still has no `CallableId`.
   - **2 trait methods on a type parameter** — NOT a lookup failure. Before
     monomorphization, `a.area()` where `a : T` denotes no single function; Core carries a
     placeholder that Mono rewrites. Describing it needs a `TraitId`, and `TraitDef` holds
     only a name and an optional `BuiltinTraitId` — there is no trait identity to name it
     with. Minting one mirrors `TypeId` and is its own slice.

   Naming the sites produced that breakdown and immediately disproved my guess: I had
   assumed pre-monomorphization polymorphism dominated, and it was 2 of 21.

   **Also latent, recorded at the site rather than filed:** `alloc`/`free` drop their
   arguments, so `alloc(1)` and `alloc(2)` share evidence. Unreachable today — they require
   the `Alloc` capability and capability-bearing functions are excluded from the provable
   subset — but it becomes a live collision the moment that exclusion is relaxed.
3. Dependency binding. Three shadow axes now exist, deliberately separate because they
   answer different questions and collapsing any two would let one imply another:
   `bodyV2` (does the subject DESCRIBE the body), `deps` (is what it reaches BOUND), and
   `assumes` (what would a claim be RESTING ON).

   **Done: `ConstId` → initializer digest.** A `constRef` named a constant and said nothing
   about its value, so changing `const LIMIT: Int = 10` to `= 99` moved neither the body
   digest nor the subject digest — a proof stayed valid-looking after the constant it
   depends on changed. `shadow deps` digests the initializer encodings of exactly the
   constants a body reaches, ordered by identity so statement reordering cannot move it.
   Note the corpus barely exercises this: 431 of 432 subjects reference no constant,
   because only two example files declare one. Constructed probes, not corpus coverage,
   are what test this axis.

   **Done: the assumption axis.** `SubjectQualificationV2` and `stmtAssumptions` had ZERO
   consumers. They could not have worked before the predicate work landed either: until
   `assert`/`assume` predicates were elaborated, an assume emitted a gap and the axis
   counted zero for the only construct that feeds it — vacuous rather than wrong, but it
   would have reported "nothing assumed" the moment the gap closed, which is the unsafe
   direction. 7 of 432 subjects are qualified, including `contract_negatives/assume_taint`.

   **SHADOW IS NOT A STAGING DETAIL, IT IS THE SEQUENCING CONSTRAINT.** `subjectDigest` is
   `proofSubjectDigestV2` — not the frozen V1 body fingerprint — and it already drives
   freshness verdicts. Binding anything new into it before the step-5 migration moves
   existing subject digests and strands every stored proof link. So each axis lands as a
   report line reaching no verdict, and the gates assert BOTH halves: the new material must
   move with what it describes, AND the subject digest must not. A change satisfying only
   the first half is the right fix shipped at the wrong time.

3a. **Remaining: the DepNode graph and the deterministic root.**

   `DependencyRoot` is built and has ZERO PRODUCERS — `DepNode`, typed edges, fail-closed
   error cases, validated material, trust propagation, none of it called. That is the third
   instance of one shape this arc: `SubjectCompletenessV2.of`, `SubjectQualificationV2` and
   now `DependencyRoot` were all designed, landed, and never wired. Worth treating as a
   review question rather than a coincidence — design work that ships without a consumer
   looks complete and is not.

   **Edges must come from the EVIDENCE BODY, not `buildCallGraph`.** The landed containment
   closure walks a graph keyed on qualified NAME STRINGS; `DepNode`'s own comment calls that
   "the defect class R-0004 exists to close appearing inside the dependency material
   itself". The evidence body already carries a `CallableId` on every `call` and `fnRef`
   node, minted by the resolver, so deriving edges from it is identity-native and keeps one
   producer instead of re-deriving a graph from names beside the one that exists.

   Measured before assuming the worst: two modules each declaring `P::get` give `a.P_get`
   and `b.P_get`, so qualNames carry the module path and the weakness is about RENAME
   STABILITY, not live conflation. Latent, not exploitable today.

   Decisions taken in building the edge set, recorded because each could reasonably have
   gone the other way:

   - `fnRef` IS an edge. Taking a function as a value is a dependency on it — behaviour
     changes when that function's body changes, whether it is called here or handed
     elsewhere. Omitting it would make higher-order programs look dependency-free.
   - Self-edges are KEPT. A recursive function does depend on itself; whether that counts
     as self-blame is POLICY (the containment pass excludes `self`), and baking policy into
     the material hides it from consumers that need the real graph.
   - Ordered by canonical identity, so the edge set is a function of the body alone.

   Still to do: the edge KIND derivation (contract / body / trusted / missing, driven by
   callee proof status), assumption propagation through the closure the way `carriesTrust`
   propagates trust, and table reachability binding the whole canonical table rather than a
   guessed subset.

   **Scouted 2026-08-08, so the next person does not re-derive it.** The whole-table
   requirement is HALF built, and the two halves are easy to confuse:

   * *Identity* over the whole table exists. `FnTable.allIdentified` requires every entry to
     carry an identity — "evidence requires identity for the WHOLE table, not most of it" —
     and one legacy entry disqualifies the table from minting a receipt. `canonicalEntries`
     and `hasDuplicateIds` are there too. This half is done and fail-closed.
   * *Reachability* binding **LANDED 2026-08-08** (`tableValueDigest` + `EdgeEvidence.tableDigests`
     + `tablesFullyBound`, gated 6 ways in `check_dependency_edges.sh`, 36 -> 42). It digests the
     table constant's DEFINING VALUE — whole by construction, since there is no subset to select
     from — and REFUSES for a valueless or unavailable constant rather than digesting a part.
     The CONSUMER side landed with it: `Concrete/Proof/Receipt.lean` defines
     `ProofEvidenceReceipt`, whose `tableBindings` is `List (Name × String)` — **not** `Option
     String` — so an unbound table has no representation inside a receipt at all, and `mint?`
     is the only constructor — the structure constructor is PRIVATE and `Inhabited` is not
     derived. **Both were missing when this first landed (`477a1726`), so the
     "unrepresentable" claim was false for two commits**: a caller could assemble a receipt
     with an empty subject and the CURRENT schema version, and `default` produced one with an
     empty schema. The gate demonstrated the bypass while asserting the invariant held, because
     its legs tested the smart constructor rather than the claim. Fixed with hostile controls
     that assert direct construction and `default` do not COMPILE.

     Two value-level holes closed at the same time, both found by the same review:
     `tablesFullyBound` checked equal lengths and all-present, which admitted `tables := [X]`
     with digests for `[Y]` — a receipt claiming Y while the theorem depended on X, since equal
     counts of unrelated things is not correspondence; and `some ""` minted where `none`
     refused, the same hole as an empty environment identity. Original finding, kept because it is why the item read as one thing:
     `tableDigest`/`tableReach`/`tableBytes` return nothing across the tree. So a proof over
     a body that indexes a table is currently bound to the table's IDENTITY discipline but
     not to its CONTENTS, and a change to an entry the body does not appear to touch moves
     no dependency material.

   The missing piece is specifically (a) which tables a body reaches, then (b) a digest over
   each reached table's FULL `canonicalEntries` — not the entries a static index suggests,
   because a dynamic index can reach any of them and binding the apparent subset is the
   under-approximation this item is named after. `shadowDepsLine` is the model to follow: it
   already REFUSES rather than digesting a partial constant set, which is the same
   fail-closed shape this needs. The root itself is slice 6 — the landed closure says so: "Slice 6
   replaces this with a deterministic SCC/Merkle root; until then a conservative closure is
   what the roadmap asks for."
4. Freshness integration — closes bugs 059/060. First point where a mistake costs a wrong
   proof status rather than rework.
5. The 44-fingerprint migration and kernel replay.
6. Receipts.
7. The ninth proof table (blocked on mutable-borrow extraction).

**Decisions this arc queued, in the order they bind.** None is a matter of taste; each puts
something into versioned bytes or leaves a construct undescribable.

- **Struct-literal initializer evaluation order — blocking before step 5.** Declaration
  order today, source order for every other positional construct. It was believed to be
  refused pending a decision; the refusal was a doc comment and an uncalled function, so
  declaration order has been in the shadow bytes all along. Harmless while shadow, wrong
  the moment the migration makes those bytes authoritative. `check_convergence_inventory`
  now enforces that ordering rather than trusting prose.
- **`TraitId` — blocks the last 2 producer refusals.** A trait method on a type parameter
  denotes no single function before monomorphization, and `TraitDef` carries only a name
  and an optional `BuiltinTraitId`. Minting a trait identity mirrors `TypeId`.
- **Function-type capability-set identity.** Encoding a `fn` type means encoding a
  capability SET whose variables resolve against a different binder list. Zero corpus
  occurrences, so it can wait — but it is why `evTypeRef` refuses function types rather
  than guessing.

### R-0004 state as of 2026-08-07 (`d156a7f6`, CI green)

> **[historical] Reconciled 2026-10-19:** a dated R-0004 snapshot superseded by its closure. Its live remainders are mapped to owning tasks in the
> [reconciliation record](../project/ROADMAP_RECONCILIATION.md); nothing in it schedules work.

Written for someone picking this up cold. `d156a7f6` is the first complete CI verdict since
`b133b04c`; everything between was cancelled by superseding pushes, so this is the point to
trust.

**Where it is.** Slice 4 of 7. Steps 1, 2 and 2a are done — producer coverage **421 of 432
subjects**, 0 absent. Step 3 is done as far as the compiler can take it.

**Four shadow axes exist, and they are deliberately four**, because each answers a question
the others cannot and collapsing any two lets one imply another:

| line | question |
|---|---|
| `shadow bodyV2` | does the subject DESCRIBE the body? |
| `shadow deps` | is what it reaches BOUND to a meaning? |
| `shadow assumes` / `inherited` | what would a claim be RESTING ON, locally and through callees? |
| `shadow edges` / `edgeKinds` | what does it REACH, and how? |

All four are SHADOW, and that is a sequencing constraint rather than a staging convenience:
`subjectDigest` is `proofSubjectDigestV2`, not the frozen V1 body fingerprint, and it
already drives freshness verdicts. Anything reaching it before the step-5 migration moves
existing subject digests and strands every stored proof link. Each gate asserts BOTH halves
— the new material must move with what it describes, AND the subject digest must not.

**The 11 producer refusals that remain** are three distinct causes, each named at its site:
5 incomplete callee identity (a generic callable whose type arguments are unrecorded), 4
method calls whose mangled name has no `CallableId`, 2 trait methods on a type parameter.
None blocks the completion gate, which accepts "fail-closed uncovered".

**BOTH OWED DECISIONS ARE NOW MADE (2026-08-08).**

1. **Struct-literal initializer evaluation order → SOURCE ORDER.** Initializers evaluate
   left-to-right as written; `P { y: f(), x: g() }` runs `f()` before `g()`. Today `Elab` walks
   `sd.fields` and looks each initializer up by name, so declaration order wins and source order
   is discarded before Core exists — this changes that.

   The deciding argument is sequencing, not taste. Step 5 migrates the shadow bytes and makes them
   authoritative; changing evaluation order AFTER that migration means paying the fingerprint
   rebuild twice. Whichever order was chosen, it had to be chosen before step 5 — and if there was
   any chance of wanting source order later, doing it now is strictly cheaper.

   On the merits: written order and run order differing is a permanent readability trap, and it is
   observable exactly where it hurts most — when an initializer traps or has effects, which is
   precisely what the obligation system reasons about. Matching the order a reader sees is worth
   an afternoon and one rebuild.

2. **[historical decision at the time] Slices 7–8 → HOLD R-0004 OPEN** until mutable-borrow extraction and adversarial validation
   land. The ninth proof table needs the former, and it lives outside R-0004. The decision then was
   to keep R-0004 open rather than close with a named gap. The cost was a long-running task in flight on work it
   does not own; the reason to accept that is that "done, with an asterisk" reads as done to
   everyone who did not write the
   asterisk.

**Original framing, kept because it records what was blocking and why:**

1. **Struct-literal initializer evaluation order — blocks step 5.** Declaration order is
   already in the shadow bytes; the migration is what makes them authoritative. Either
   ratify declaration order in the language reference and gate it, or change `Elab` to
   source order and rebuild. Each is roughly an afternoon. It is a language-semantics call.
2. **Slice 7 and the ninth proof table.** That table is blocked on mutable-borrow
   extraction, which lives outside R-0004. Ship slice 7 without it, or hold R-0004 open.

`TraitId` is NOT a blocker despite appearing in the refusal list — see above.

**A structural constraint on slice 6, worth knowing before designing its root.** The
deterministic dependency root CANNOT be completed purely in the compiler.
`DependencyEdges.classifyTheorem` decides contract-vs-body by reading a linked theorem's
ELABORATED TYPE and runs in `MetaM`, which exists only while Lean is elaborating. The
compiler can honestly produce `trusted`, `missing` and `unclassified`; the remaining split
has to come from the Lean side. This is not a plumbing gap that more threading would close.

### Gate and tooling health (found while building the above, not by any gate)

**Instruments added 2026-08-07, each in response to a defect this arc produced.**

- `check_wired_definitions.sh` — a ratchet on evidence-layer definitions with no consumer
  outside their own file. THREE pieces shipped designed-landed-never-wired
  (`SubjectCompletenessV2.of`, `SubjectQualificationV2`, `DependencyRoot`) and every one was
  found by accident. Here that is not dead weight, it is A FACT NOBODY CHECKS: the axis
  exists, the subject looks like it has one, and no verdict consults it. A ratchet rather
  than a prohibition because same-file-only use is often legitimate (mutual-recursion
  members, field types), and a gate that flagged those would be argued with until disabled.
  Baseline 23; the direction is what matters. Verified to fire.

  Its first version measured ITSELF — its header names the definitions it counts, so they
  read as consumed by its own prose. Excluded, and the baseline moved 21 → 23 once it
  stopped.

- Mutation-coverage floors per evidence gate, inside `check_mutation_anchors.sh`. "Every
  leg is mutation-verified" is a discipline nobody can enforce by reading, and it was
  applied inconsistently here: several legs passed for the wrong reason and three gates
  could not fail at all. The floor cannot prove a specific leg is covered; it stops a gate
  accumulating assertions while its mutation count stays flat.

- `scripts/tests/lib/subject_facts.sh` — ONE reader for `--report subject-facts`, selecting
  by callable identity. Every gate had its own awk, and selection went wrong twice in one
  day because `head -1`/`tail -1` pick by POSITION while subjects are not emitted in
  declaration order — a probe's helper got compared instead of its subject. Until the
  report emits something machine-readable this is the seam: one parser to fix when the
  format moves, rather than one per gate.

  **The report has become the machine API by accident.** Five `shadow *` lines are now
  scraped by gates. The shared reader contains the damage; a structured emission is the
  real fix and is not done.

- `push-both` no longer calls a CANCELLED run red. It tested `conclusion != "success"` and
  printed "origin is red. Stop the line." Two cancellations on 2026-08-06 were
  environmental — one run superseded by a later push, one killed by a GitHub outage
  resolving action downloads — and in both cases nothing was compiled. No verdict is not a
  failure; it still exits non-zero, but it now says re-run instead of debug.

**Freshness guard adopted across the R-0004 gates (2026-08-07).** The
`spike/multi-kernel-theories` merge brought `scripts/tests/lib/fresh.sh`, which 145 gates
already sourced — but none of the seven evidence gates did, because they were written on
main before the guard existed and its sweep never saw them. They are the gates where the
trap actually bit: twice in one session I restored a mutated source, re-ran a gate without
rebuilding, and read the previous binary's verdict as the current one. Once that looked
like a real failure and was nearly written up as one.

Its mechanism is worth knowing before anyone "improves" it: it BUILDS rather than compares
timestamps. An earlier mtime version had to be abandoned because `lake build` completes
without relinking when output is unchanged, so a touched source looks newer than a binary
that is in fact current — the guard fired permanently and its own advice could not clear
it. Delegating to lake costs ~130ms and makes a stale reading impossible rather than merely
reported.

**CI ran on `main` only, so branches were never checked (fixed 2026-08-07).**
`.github/workflows/lean_action_ci.yml` triggered on `push: branches: [main]`, and branches
here are pushed directly rather than through PRs. `spike/multi-kernel-theories` therefore
had ZERO CI runs across its entire life and reached merge carrying FOUR independent gate
failures — `check_roadmap_linear` (four task headings breaking the bare-ID convention, so
eight references read as dangling), `check_div_migration`, `check_contract_clause_migration`,
and `check_hole_status_consistency` (a tripwire's conditional message scanned as a status
claim). All four were found days later by a pre-push run on the MERGE, by someone who had
not written them, alongside five examples with no manifest entry.

`.github/workflows/branch_health.yml` now runs the FAST SURFACE GATES on any non-main push
— 14 seconds after a build, sharing CI's build cache key. Of the four, `check_docs_drift`
(which calls `check_roadmap_linear`) is in that set and would have fired on the branch's
first push. Full CI per branch was rejected deliberately: 70+ minutes is a cost that gets
switched off, and a check people disable is worse than one that never existed. It is
explicitly NOT a merge gate and says so in its header — a check that overclaims is one
people learn to ignore.

UNTESTED AS OF `fb87cc7c`: every push that day went to main, so the workflow has never
fired. Its first real exercise is the next branch push.

**Prefer asserting a PROPERTY over a COUNT wherever a property is expressible.** Three
gates failed this session because behaviour legitimately IMPROVED — div VCs 2→4 (safety
obligations added), uncovered subjects 1→0 (contract type-checking closed the cause), typed
uses 7→10→12 (the derived view got richer). Each needed a human to judge
regression-versus-improvement, which is the ratchet working but is also friction that
invites re-baselining without reading.

Where a property exists it survives the improvement and still catches the regression. The
worked example is `signed_div`: its count was re-baselined, but a leg was added requiring
its VC kinds to be exactly `{div_nonzero, div_quotient_in_range}`, so a VALUE VC appearing
there fails no matter how many safety VCs get added later.

Related discipline, from a near-miss the same day: **read the actual obligation, not the
comment above it.** Four proved VCs appeared on a negative control whose header said its
division "is NOT lowered to a VC", and it looked exactly like a soundness regression. It
was not — the conclusions were `2 ≠ 0` and `¬(x = MIN ∧ 2 = -1)`, both semantics-
independent, and the value lowering was still absent. A header describes what was true when
it was written; new obligation kinds appearing under an old comment are outside its scope,
not violating it.

**Pre-push gate runtime is growing and should be watched.** The evidence gates compile the
example corpus repeatedly; a publish now spends ~25-30 minutes in pre-push gates before it
even reaches CI, and the worktree cannot be edited during that window. The response so far
is to BATCH several slices per publish rather than thin the gate set — the gates are what
caught the real problems in this arc, so the cost is the wait, not the checking. If it
keeps growing, split the corpus-wide RATCHET (which needs the whole corpus) from the
property legs (which need three files), and run only the latter pre-push.

**The mutation harness could lose coverage silently, in two independent ways.** It is what
establishes that the other gates are load-bearing, so an inert mutation does not merely
skip a test — it withdraws the evidence that some gate does any work.

1. A stale anchor printed `SKIPPED (pattern not found in file)`, incremented `ERRORS`, and
   the harness still **exited 0**; only `SURVIVED > 0` failed it. Fixed: `ERRORS > 0` now
   fails.
2. `test_mutation.sh` is not run in CI at all — it rebuilds the compiler twice per
   mutation — so nothing was checking. Fixed by splitting off the cheap half:
   `check_mutation_anchors.sh` verifies every `MUT_OLD` still exists in its `MUT_FILE`.
   Pure string presence, no builds, seconds, every push. It cannot say a mutation is
   killed; it says whether the mutation can still be applied at all, which is the property
   that was being lost.

Measured 2026-08-06: **4 of 77 anchors were stale.** Three rotted when the V2 producer
cutover turned `return .call ...` into `return ElaboratedExprV2.mk (CExpr.call ...)`; the
fourth when R-0455 replaced a literal Rocq proof template with an interpolated one. All
four were found by hand, three of them only because someone happened to be editing nearby.

**`push-both` reports a cancelled CI run as red.** It tests `conclusion != "success"` and
prints *"origin is red. Stop the line."* Two runs today were CANCELLED — one superseded by
a later push (the workflow sets `cancel-in-progress` for pushes on a ref), one killed by a
GitHub outage resolving action downloads — and both read as breakage. A cancelled run is
NO VERDICT and should say so: the correct response is to re-run, not to investigate a
failure that did not happen.

**Two latent evidence gaps, recorded at their sites and tracked here so they are not
rediscovered:**

- `alloc`/`free` drop their arguments, so `alloc(1)` and `alloc(2)` share evidence.
  Unreachable today because they require the `Alloc` capability and capability-bearing
  functions are excluded from the provable subset; a live collision the moment that is
  relaxed.
- **E0264**: a method call on an aliased imported type does not compile (`no method 'get'
  on type 'Q'`). Pre-existing and unrelated to the evidence work, but it is why the
  alias half of the imported-method identity split is written correctly and never
  exercised.

**Still unmeasured:** whether `lookupVar`'s raw binder position is stable across parameters,
lets, loops, patterns and temporary scopes. Nothing depends on it — relative frame positions
are used instead — and it must not enter canonical bytes until measured.

**Found while landing the R-0484 checkpoint (2026-10-03).** Four separate items, not one
batch: each changes a different trust boundary, so each lands and is verified on its own.

1. **Mutation scoring: split invalid from infrastructure failure.** Verified first: both
   harnesses already refuse to count a mutant that does not build as a semantic kill. The
   supervised campaign scores it INVALID; `test_mutation.sh` credits a build failure only
   when the family declares it, and scores an unused-binding lint as an invalid ERROR.
   The remaining gap closed 2026-10-03: `test_mutation.sh` now counts every ERROR as
   INVALID (drifted anchor, lint, undeclared build kill) or INFRA (no verdict reachable),
   reports both, and names the fix for each; comments no longer describe the retired
   `KILLED (build)` scoring. Verified by a deliberately drifted anchor reporting
   `[1 invalid, 0 infrastructure]`. Anchor matching stays a structural check;
   compilation and causal failure stay with campaign qualification.

   **Found by the first dispatched full campaign (2026-10-03), identical on main and on the
   R-0484 branch.** Six families scored INVALID. Four were a harness defect: build-kill
   attribution recognised only the `path:line:col: error` header, and Lake prints
   `error: path:line:col:`, so no build kill was ever attributed. Fixed in both harnesses;
   `trap-quotient-condition`, `attestation-precondition`, `kernel-foundation` and
   `transform-has-effect` now score KILLED by build. A build kill qualifies only through
   its NAMED check: each declared family carries the exact diagnostic its compile-time
   `rfl` lock produces, and a build failure without that diagnostic in the mutated file's
   own blocks is INVALID (negative control: a wrong declared check scores INVALID).
   `reference-division` qualifies the same way — its lint was incidental; the declared
   `-7 tdiv 2 = some (-3)` lock fires. One remains an open qualification gap:
   `divergence-detection` (under the mutant `run_tests.sh` never reaches the end it
   reaches on pristine source — find out whether that is a hang the suite should report
   or a mutant that breaks the harness). The campaign job also exceeds
   GitHub's 6-hour limit on a dispatched run, so the full campaign has no CI home today;
   it runs only on the mirror's schedule, which is pinned to an old commit.

   **Incomplete qualification, recorded at the R-0484 checkpoint merge (2026-10-04).** The
   checkpoint merges under an approved exception: the full campaign is not a merge blocker
   because it cannot finish within the job limit and behaves identically on main. The
   exception required every other CI job to pass on the merged revision, targeted
   qualification of every changed family on that revision, and a passing campaign golden.
   It does not close R-0484 and does not certify the campaign. Still open:
   - the 6-hour timeout: no complete campaign run exists for the merged revision;
   - campaign survivors `freshfacts-requires-proved-status` and
     `freshfacts-carries-trusted-boundaries` (diagnosis above);
   - `test_mutation.sh` mutation 52, "Elab V2 input: resolved field use omitted (field
     access)" in `Concrete/Elab/Elab.lean`, survives on main and on the branch alike —
     re-anchored for R-0484 with the experiment unchanged, not newly weakened;
   - `divergence-detection` remains INVALID (above).
   **Update 2026-10-11:** the sharded campaign on `1f2fe0f1` completed within the job limit
   (slowest shard 109m), with `divergence-detection` KILLED; the two survivors are closed at
   `964073e7`. Qualification still needs a clean run on the published revision.
2. **Worktree support for the mutation harness.** It assumes `.git` is a directory and
   reports `isolated workspace has no .git` only after lock and cleanup output, so a golden
   check failed 4/7 twice before the cause was visible. Discover paths with
   `git rev-parse --git-dir`/`--git-common-dir`/`--show-toplevel`, separate
   checkout-local state from shared Git state, and test the repository lock across two
   worktrees. The early refusal landed 2026-10-03: a campaign run in a worktree now stops
   in milliseconds, before the lock, sweep or copy, naming both git dirs and the clone
   command (the copied `.git` file would have shared HEAD and index with the real
   worktree). Full support remains.
3. **Gate inventory checked in CI, by content.** `EXPECTED_GATE_COMMANDS` went stale four
   times because only local tools enforce it, and a count of 232 can still hide a
   duplicate alongside an omitted gate. Check the extracted command inventory against a
   committed list in the Branch health workflow, so drift fails on push.
4. **Std identity refresh, with safeguards.** A std edit needs `attestation_refs.sh`, a
   content-keyed migration of renamed references, `refresh_classifications.sh` and
   `build_identity.sh`, in order, by hand. Automate it so that it emits a reviewable
   migration diff, refuses ambiguous content matches, and runs the freshness gates
   afterwards. It must never bless changed evidence or rewrite a golden to make a
   failure go away.

Also: a full-CI dispatch-and-watch script for branches, so "broad migrations merge green"
is the easy path. It pins the commit it validates and reports failure, cancellation and
success as three distinct outcomes. And `knownEffectfulForeignSymbols` should have one
machine-readable authoritative source with generated documentation; moving the list
beside prose would add no guarantee.

### Multi-prover merge: what it took, and what it left open (2026-08-05)

> **[historical] Reconciled 2026-10-19:** a dated merge record. Its live remainders are mapped to owning tasks in the
> [reconciliation record](../project/ROADMAP_RECONCILIATION.md); nothing in it schedules work.

Merged the spike pinned at `80be5368` into main after the quarantine review. The merge
itself was textually CLEAN — zero conflicts across a 105-commit divergence — but five
distinct problems had to be fixed first, none of them visible from the conflict count.

| # | problem | resolution |
|---|---|---|
| 1 | stackdepth expected 3 bounded/3 recursive | the spike FIXED a fail-open; the test asserted the defect. Corrected the counts. |
| 2 | VC `kind`/`status` whitelists asserted a v1 set | extended to measured reality: `div_quotient_in_range`, `shift_amount_in_range`, `ineligible` |
| 3 | three sites expected `solver_error` | never produced anywhere; all report `unproven`. Safety asserted harder, diagnostic TRACKED |
| 4 | `assume_taint` contract snapshot drift | the spike added contracts to std's `rotr`; regenerated (1 of 14 actually changed) |
| 5 | fixed_capacity expected 29 bounds | the spike emits 5 MORE bounds obligations (fail-open closed); zero proofs lost |
| 6 | six report snapshots drifted | the spike modified the hmac fixture and std/sha256; regenerated (exactly 6 changed) |

**Three of the five were fail-opens the spike CLOSED**, with the gate asserting the old
permissive behaviour: transitive stack-depth admission, bounds obligations on five
previously-unchecked array accesses, and std contracts on `rotr`. In each case the correct
action was to update the expectation, and in each case a lazier reading ("the spike broke a
gate") would have concluded the opposite.

**A merge being conflict-free says nothing about it being correct.** Every one of these
was found by RUNNING gates, and three of them were gates asserting behaviour the spike had
deliberately changed for the better.

**The convergence inventory** (`scripts/tests/check_convergence_inventory.sh`, 12 entries,
CI-registered) makes every remaining gap executable. Each entry asserts the gap's CURRENT
state, so closing one FAILS the gate and forces a deliberate inventory update. That is the
machine-checked tracking this merge was conditioned on.

### Merge blocker RESOLVED: the spike fixed a fail-open, the test asserted the defect

> **[historical] Reconciled 2026-10-19:** a resolved merge blocker. Its live remainders are mapped to owning tasks in the
> [reconciliation record](../project/ROADMAP_RECONCILIATION.md); nothing in it schedules work.

The pin `80be5368` failed one trust-gate case on its own (1500/1), identically on the
quarantine merge:

    FAIL stackdepth: mixed should have 3 bounded, 3 recursive

Diagnosed rather than skipped. `adversarial_stackdepth_mixed_recursion.con` has six
functions; `main` calls `recurse(5)` and `mutual_a(3)`, so **main's stack usage IS
unbounded**. Pre-spike it was counted BOUNDED because only DIRECT recursion was admitted —
a fail-open. The spike's transitive recursion admission classifies it correctly, moving the
totals 3 bounded/3 recursive → 2 bounded/4 recursive.

So the expectation encoded the defect, and the spike is more correct than main was. Fixed
by correcting the expected counts with that reason recorded inline, NOT by skip-listing:
trust gate now 1501 passed / 0 failed, oracle 70/0.

Worth stating plainly: main has been fail-open on transitive stack depth, and merging the
spike fixes that.

### Multi-prover work that remains AFTER R-0004

> **[historical] Reconciled 2026-10-19:** a dated list; `solver_error` is R-0520, kernel-set binding is R-0440, proposition identity is R-0454. Its live remainders are mapped to owning tasks in the
> [reconciliation record](../project/ROADMAP_RECONCILIATION.md); nothing in it schedules work.

Recorded so the dependency is explicit rather than discovered later. The spike's own comment
in `ObligationCore.lean` states that `proved_by_two_kernels` / `proved_by_multi_kernel`
attest CHECKER diversity, not agreement on one proposition, because *"there is no
subject-digest cross-check — that is future work"*. That cross-check IS R-0004's subject
digest, so these statuses cannot become authoritative until R-0004 lands.

1. **Subject-digest cross-check across kernels.** Each per-prover renderer re-spells the
   obligation with nothing verifying they spell the SAME proposition. Until the subject
   digest is authoritative, N kernels accepting N lowerings is not N kernels agreeing.
2. **Graduate the statuses.** `proved_by_rocq`, `proved_by_isabelle`,
   `proved_by_two_kernels`, `proved_by_multi_kernel` stay EXPERIMENTAL and
   non-authoritative: they must not drive badges or release policy while the receipt and
   status model is unfinished.
3. **`kernel_disagreement` handling** — currently caps the claim and fails the two-kernel
   release gate. Its interaction with V2 freshness (a disagreement on a STALE subject is a
   different fact from one on a fresh subject) is undefined.
4. **Receipts must bind the kernel set.** A receipt naming a subject digest and dependency
   root must also record WHICH kernels attested and at what versions, or replay cannot
   reproduce the verdict.
5. **`solver_error` is never produced.** Absent solver, garbage solver and the negatives
   fixture all report `unproven`. Fail-closed, so nothing unsound — but a consumer cannot
   distinguish an environment defect from a proof-difficulty statement. Three gate sites
   track it; restoring their strict assertions is the close condition.
6. **Re-pin, do not track a moving head.** Later spike deltas integrate as new pinned
   epochs against a green pin, with the convergence inventory updated each time.

> **RECONCILIATION NOTE (merge of spike/multi-prover-evidence @ 80be5368).**> **RECONCILIATION NOTE (merge of spike/multi-prover-evidence @ 80be5368).** This section
> is dated 2026-08-01 and is STALE for R-0004: it predates the evidence-producer work of
> 2026-08-04/05. Git auto-merged the two roadmaps without a textual conflict, so nothing
> flagged the staleness — it is recorded here rather than silently inherited.
>
> Superseded for R-0004 by the later dated material in this file. Landed since this
> section was written:
>
> - `elabExprEv`/`elabStmtEv`/`elabCallEv` — ONE producer of Core and evidence, with all
>   104 child edges threaded and zero Core-only projections inside the mutual block.
> - Callee identity as an OUTPUT of resolution (single owner, installed at ElabEnv
>   construction so there is no fail-open window).
> - The typed evidence tree, structurally collected per function, with `matchExpr` added
>   because Concrete's `match` is an expression.
> - A structural serializer: distinct programs now produce distinct body bytes — operators,
>   nesting, argument order, field selection, variant selection and arm order all
>   discriminate, which the flat identity-use serializer could not do.
> - Bug 066 fixed: the interpreter discarded match-guard effects, silently corrupting the
>   differential oracle for every guarded match.
>
> Still open, and NOT completed by this merge: the independent `bodyIdentityUses`
> accumulator (deletion unblocked, tree proven to lose nothing), shadow mode on structural
> bytes, dependency binding, freshness integration, the 44-fingerprint migration, receipts,
> and the struct-literal initializer-order language decision.
>
> Multi-prover results arriving with this merge are EXPERIMENTAL and non-authoritative;
> they must not drive `proved_by_*`, badges, or release policy until R-0004's receipt and
> status model is finished.


Not a second task queue — a map from *what is broken* to *who owns it* to *how you know
when it is fixed*. Added because the information existed only as prose spread across
KNOWN_HOLES, the gate scripts and commit messages, which is how the H23 status
contradiction survived. [docs/verification/KNOWN_HOLES.md](../verification/KNOWN_HOLES.md) remains the authority
on hole status; this table is a reference to it and
`scripts/tests/check_hole_status_consistency.sh` fails if the two disagree.

## Rationale notes: the prover-neutral arc (R-0004, R-0450, R-0448, R-0454–R-0456, R-0458–R-0464, R-0467, R-0468)

**Not an ordering.** The numbered order in this note was argued from the 2026-07-31 audit of
`spike/multi-prover-evidence` and is superseded by [the current execution
queue](../../ROADMAP.md#the-current-execution-queue), which is the only ordering in this file. The
per-task reasoning is retained because these tasks are entangled enough that picking one
without reading the others has produced rework; it moves to `docs/archive/` with the
other historical material.

0. **R-0004's remaining slices come first, and are not optional to this arc.** R-0454
   states in its own body that it belongs immediately after them, and slice 4 (the receipt
   envelope with typed dependency edges) is what R-0454's digest attaches to. Slices 1–3 are
   closed; Slice 5's subject/manifest foundation is complete but not production-activated;
   Slices 4 and 6 are active; 7–8 remain. Use R-0004's dated canonical block for exact state.

   **Slice 3 is also the template for R-0461, which is why R-0004 leads rather than
   merely precedes.** It closed bug 062 by making a not-current dependency downgrade its
   dependent, transitively, with which statuses count decided in ONE place
   (`ObligationStatus.isCurrentForDependents`). H23 is that identical defect one dimension
   over: bug 062 was "dependency statuses are computed and never consulted to downgrade
   the caller"; H23 is "hypothesis statuses are attached and never consulted to downgrade
   the conclusion". The project found this class, fixed it once, and shipped the second
   dimension without it.

   **What R-0461 actually did with this instruction (2026-08-03), since it qualifies the
   advice for R-0464.** It reused slice 3's *shape* and could not reuse its *code*:
   `ObligationStatus.isCurrentForDependents` ranges over function qualNames (proof links),
   while hypothesis debt ranges over VC ids (`fn@line#O2`). The two are disjoint key spaces,
   so `capOnHypothesisDebt` is a separate VC-level composition that mirrors the discipline —
   compute the weakest input, cap the output, decide "counts as proved" in one place — rather
   than calling into it. The stated risk stands and is now real: two predicates decide what
   counts as proved, and if they diverge that divergence is the next bug. Unifying them is
   the honest content of a future slice; pretending one call site covered both would have
   been the wrong fix.

1. ~~**R-0461, because the arc is currently decorating a defect.**~~ **DONE 2026-08-03.**
   H23 was: an obligation may assume a loop invariant whose preservation VC is unproven, and
   nothing composes the two statuses — so a guaranteed out-of-bounds access reported
   `proved_by_multi_kernel (3: lean, rocq, isabelle)` and `require-two-kernels` built it.

   Closed in three parts, and the estimate of "Register C already proves the composition, so
   this is wiring" was **wrong in an instructive way**: the algebra was indeed ready, and the
   cap that used it was the cheapest third. The other two thirds were the ones this arc keeps
   underestimating — deriving real provenance (`loopInvariantDebt`), and making the resulting
   status *enforced* (`E0617`) rather than merely displayed, on *every* surface that renders
   it. Full account in `docs/verification/KNOWN_HOLES.md` under H23, and the transferable method is in the
   scope-limits section above.

1b. **R-0464 immediately after, on the same grounds. Also precedes the R-0466–R-0469
   block.** H24: obligation generation restates `IntArith`'s trap conditions and states
   them weaker, so a division reports proved and aborts on signed `MIN / -1`, and shifts
   generate no obligation at all. Reproduced in `examples/trap_semantics_gap/`. Paired
   with R-0461 because both are reproduced holes with failing fixtures already written,
   and because a reader who fixes only the first still ships a compiler that reports
   `proved` for an operation that traps. R-0461 having landed, **H24 is the only reproduced
   unsoundness left**, and R-0461's three-part shape is the checklist: derive the fact,
   compose it into the status on every surface, then make a policy fail on it.

   **Why this one outranks the block while the rest of the arc does not.** The block's
   argument is that continuous measurement of the language surface beats further agreement
   between checkers of a model — and that argument is right about *arc work*. H24 is not
   arc work. It is a live unsoundness in what the compiler already claims: a `proved`
   division that aborts, and a shift family that claims nothing at all. Measurement of a
   surface that reports `proved` for a trapping operation measures the wrong thing until
   this is fixed. Everything from 1c down stays behind the block, because vocabulary,
   digests and IRs are exactly the "more arc work" the block was sequenced ahead of.

1c. **R-0458, which R-0461 forced and then shipped without.** A claim capped
   by an outstanding hypothesis is *proved conditional on invariant@6* — real evidence
   naming exactly one remaining VC, which the current vocabulary cannot express. That is
   `strength` and `independence` as separate coordinates, and R-0461 is their first real
   consumer rather than a hypothetical one. Note the part already landed (the four-field
   `independent_of`, per-kernel receipts, display-only composite) so this is scoped to the
   remainder; see its body.
2. **R-0454 (neutral digest), because its window is closing.** Digests are
   *stored*. No `subjectDigest` field exists in `Concrete/` yet, so migration cost is
   zero today and rises with every artifact written. It also closes the substantive
   gap in R-0448: kernels currently agree on an obligation *id*, so "two kernels
   agreed" means "two kernels each closed something filed under the same key".
   Alpha-normalize before hashing — the third encoding invariant, added from the R-0464
   cross-check, without which R-0464 invalidates every stored digest.
3. **R-0467 and R-0468 here, not later, because everything above is unverified on the
   merge commit without them.** The multi-kernel job runs on dispatch, schedule or a
   label, so a regression in R-0461's or R-0464's work merges green; and the scheduled
   jobs are pinned to `lambdaclass/concrete`, so in this repository the nightly can never
   fire. Both are hours, not weeks, and they are what stops the rest of this list from
   being verified by hand each time. Sequenced after the two hole fixes only because
   there is no point automating the verification of work not yet done.
4. **Point the existing agreement technique at `exprToSmt` and Lean's own rendering**
   (recorded under R-0450). Cheap, needs no IR, and it tells the IR work what the
   fragment actually has to preserve. The SMT path matters most: its verdict enters
   the TCB as `solver_trusted`, and no kernel re-derives it.
5. **R-0462 (fuzz the compiled binary against the safety claims).** Placed above the IR
   and the register rows deliberately: it is the highest fault-finding value per unit of
   work on this list, it would have caught H24 in seconds, and it is the only item here
   that tests a claim against the ARTIFACT rather than against another model of it. Every
   static surface reported success on `examples/trap_semantics_gap/`.
6. **R-0455 (term IR + Register B), absorbing the four drivers that landed ahead of
   it.** Every further kernel added before this multiplies the thing being removed.
7. **R-0460 (Register A rows).** One discharged row moves the ceiling further than a
   third kernel does — see the priority argument recorded in that task.
8. **R-0459 before R-0448's credibility gate**, because that gate is currently
   unsatisfiable rather than merely un-run: every obligation family is arithmetic, so
   flagship code produces nothing to badge.
9. **R-0463 (probe whether a Farkas witness extracts from `micromega`).** Timeboxed, and
   scheduled late only because its result changes strategy rather than unblocking work: a
   positive result deletes the case for a second kernel at the linear tier, which is where
   multi-kernel evidence is currently deployed. Do it before any further prover, never
   after.
10. **R-0448 (graduate the arc) once the above hold**, and not before — its own merge bar
   already names H23 as blocking.
11. **R-0456 and R-0449 last, and only on their own merits** — meta-theory in a second
   host, and realization — both explicitly de-prioritised or pull-gated.

The governing principle, measured rather than assumed: kernel diversity is a
*portability* property for auditors, not a bug-finding strategy. Zero real defects on
this arc came from kernel agreement; the faults came from differential tests, and H23 —
the worst of them — was found by writing a wrong program and reading the report, while
every kernel-side surface reported success. Sequence accordingly, and note that R-0462
(fuzz the compiled binary against the safety claims) would have caught H23 in seconds.

The same principle applied one level out, added 2026-07-31: this arc strengthens claims
the language already makes, and nothing on it widens what the language can claim or makes
a rejection legible to the model that has to act on it. R-0459 is the first of those and
is already sequenced above; **R-0466, R-0471, R-0470 and R-0469 are the rest and sit ahead
of this arc by file position**, on the argument that a continuous empirical measurement of
the language surface outranks further agreement between checkers of a model — the same
trade R-0462 represents against R-0460. None is a substitute for the arc; all are cheaper,
and all fail loudly in places the arc reports success.

**Two exceptions, and only two: R-0461 (done 2026-08-03) and R-0464 precede this block.** Both own
reproduced unsoundness — a bounds obligation reported `proved_by_multi_kernel` on a
program that aborts, and a division reported `proved` that aborts on signed `MIN / -1`.
The argument above holds against *arc work*; it does not hold against a compiler that
currently reports `proved` for operations that trap, because measuring a surface in that
state measures the wrong thing. Everything else in the arc — vocabulary (R-0458), digests
(R-0454), CI reachability (R-0467/R-0468), the IR (R-0455), register rows (R-0460) — stays
behind this block, which is precisely the "more arc work" it was sequenced ahead of.

That block is four tasks, so state what earns each position rather than leaving the
count to look like drift. R-0466 is the measurement. R-0471 is hours of work the
measurement needs in order to have anything to move, and it pulls three defects out
from behind R-0137's Phase 8 dashboard. R-0470 closes a capability-inference path that
manufactures an empty capability set and is caught today only by a type mismatch —
authority visibility is a pillar claim, so it is worth closing while still a false
rejection. R-0469 is the least urgent and says so in its own body; it is here because
its trigger is unpredictable and it must precede any stack-budget work. Two of the four
(R-0470, R-0469) are ledger bugs that had no roadmap owner at all, which is the actual
defect being corrected: an unreferenced ledger bug is invisible to a sequence that
advances by file position.

**R-0461 came before all four, and file position said otherwise — kept as the worked
example of this override.** R-0461 landed 2026-08-03 and H23 is closed; R-0464 now holds
the position this paragraph argued for, on the same criterion. R-0461 sat at a much later
file position than these tasks purely because it was allocated after them. Item 0 above
already placed it ahead of the arc; it was ahead of this block too, on the one criterion
that separates them: **it was the only item in either group where a shipped claim was
false.** A guaranteed out-of-bounds access reported
`proved_by_multi_kernel (3: lean, rocq, isabelle)` and the binary aborted. Against that,
R-0466 is a measurement, R-0471 is legibility, R-0470 is a false *rejection* (no wrong
code), and R-0469 is latent — no budgeted project is recursive today. A false `proved`
outranks all four, and the block was inserted without saying so, which is the gap this
paragraph closes rather than a change of plan.

The general rule this instance illustrates, since it will recur every time an urgent
defect gets a late ID: file position is the default order, not an argument. Where a
later-positioned task fixes a live false claim and an earlier-positioned one does not,
the false claim goes first, and the override is written here rather than left for the
next reader to rediscover.

## Historical status board (2026-08-08) — superseded

This dated board is retained for rationale only. It is not a queue and does not override the current
execution state or current execution queue near the top of this roadmap.

### Merged to main 2026-08-08

The contract-input hardening series is on `main` (`e656b0af`), verified on the merged tree rather
than on either side alone:

* **corpus re-measured after the merge** — 1249 discovered, 962 clean, **2 newly rejected**, 263
  pre-existing other errors (effect UNMEASURED, checking stops at the first error), 19 parse
  errors, 3 timeouts. Both rejections are negative fixtures that exist to demonstrate exactly the
  defect they trip. Main's 73 commits added no program that contract validation breaks. The
  defensible claim is "no unexpected rejection among the 964 files that REACH contract checking",
  not "the corpus is compatible";
* gates on the 19 files both sides changed are green: phase1-contracts 32/0, contract-negatives
  57/0, gate-vacuity 3/0, loop-control 28/0, bounds-migration 8/0, subject-facts 70/0,
  type-identity 20/0, vc-schema 23/0, subject-coverage 7/0, smt-negatives 9/0, smt-path 17/0;
* **`check_bool_kernel.sh` is 62/0/0 under `nix develop .#provers`, not 42/0/8.** The 8 were only
  `coqc`/`isabelle` missing from the shell. Quoting the bare number understates the multi-kernel
  evidence by hiding every Rocq and Isabelle assertion — it was reported that way for most of a
  session before anyone ran the provers shell.



One place to look. Two entries below were open earlier the same day and are now closed; they are
kept with their outcome because the *pattern* they belong to is the last item here.

### Closed 2026-08-07 (`spike/multi-kernel-theories`)

- ~~**`#[requires]`/`#[ensures]` are not validated anywhere.**~~ **DONE, and named honestly.**
  An unbound name reached the HYPOTHESES of every obligation in the function, including ones
  reported `proved_by_kernel_decision`. Fails closed (unprovable at the call site) but surfaced in
  the wrong function. Now rejected at check time, extended to loop invariants and variants —
  where an unbound name was universally quantified into the initiation obligation, turning a
  missing binding into an arbitrary theorem variable. **Called name-and-sort validation, not type
  checking**: H27 lists six things still accepted, each measured and each pinned by a gate written
  to flip when the gap closes. See R-0473.
- ~~**Refinement substitution can capture.**~~ **CONTAINED, not fixed.** `substExpr` replaces by
  name; a binder in a spec body has the binding substituted away. No unsound evidence escaped —
  and the reason is the finding: the corrupted obligation reported `outside the fragment` only
  because `renderTerm` has no `.ifExpr` case. **Renderer incompleteness is not a valid
  precondition for transformation soundness.** Binder-bearing bodies and contracts naming a
  shadowed parameter are now rejected, both temporary and both saying so. H25, H27, R-0474.
- ~~**Gates run whatever binary is on disk.**~~ **DONE, 145 gates.** Four times in one session a
  branch switch without a rebuild produced plausible evidence about a compiler that no longer
  existed — once a shipped feature looked unimplemented, once three "parse errors" were nearly
  written up as a finding. The first fix compared mtimes and measurement killed it: `lake build`
  succeeds *without relinking* when output is unchanged. Delegating freshness to lake costs 130ms.

### Open — added 2026-08-07

0. ~~**H26 blocks merging.**~~ **RESOLVED 2026-08-07.** The fixture is one of 3 of 21 that carry a
   `Concrete.toml`, so it builds as a project against the stdlib; `a3ba761b` added contracts to
   `std/src/sha256.con` after the golden was written. Diff verified as 94 additions, 0 removals,
   nothing touching the fixture's own module, then regenerated. The structural coupling — a
   negative fixture's golden depending on every stdlib contract — is R-0475.
0b. **285 of 1249 corpus files are UNMEASURED** for contract validation (263 masked by an earlier
   error, 19 parse failures, 3 timeouts), because checking stops at the first error. Re-measured
   on merged main 2026-08-08. The defensible claim is "no unexpected rejection among the 964 that
   reached it" — not "the corpus is compatible". A diagnostic-accumulation mode closes it (R-0473).

### Closed today

- ~~**Spec bodies are not type-checked.**~~ **DONE.** A `spec fn … = expr` body is now checked for
  free variables, for calling only other spec fns, AND for SORT (boolean vs integer). Gated in
  three directions, including that the rule *constrains* rather than forbids — a spec calling
  another spec still passes. **Limit stated in the code:** it is a sort check, not type inference,
  so a width mismatch (`i32` vs `i64`) is still not caught. `#[requires]`/`#[ensures]` were not
  validated anywhere either, so there was no machinery to reuse — **superseded 2026-08-07**: they
  are now name-and-sort validated (see the entry above), reusing this sort check.
- ~~**No negative control on the axiom inventory.**~~ **DONE for 2 of ~10 gates.**
  `check_axiom_inventory` now builds a theorem that genuinely depends on `sorryAx` and requires
  the classifier to flag it — mutation-tested: allowlisting `sorryAx` produces 2 failures, where
  before that one-word edit would have left the gate green while every incomplete proof passed.

### Open — highest leverage first

1. **The VC generator.** Four hand-written walkers replaced by a WP calculus.
   Design: `docs/verification/VC_GENERATOR_DESIGN.md`. Attacks the layer that produced every discovery defect
   this month, subsumes H19 if run over Core, and makes Register A statable as one theorem.
2. **`old` / two-state postconditions.** Not a feature among features — **a ceiling on what can
   be written at all.** Without it no specification can describe MUTATION, so the corpus's only
   real contracts being refinements of *pure* helpers is not what authors chose, it is the only
   thing the language permits. That evidence sat in plain sight for most of this work and was read
   as a coincidence. See the Why3 parity table.
3. **~8 remaining gates without negative controls.** The week's tally: six assertions fired on
   non-regressions, two silently tested nothing, one skipped its own section when the build broke,
   one reported a working fix as broken via `pipefail`. Green counts overstate what is pinned, and
   capability stacked on unverified gates is the less defensible order.
4. **Rung 4's tier (bitwise).** Fully unblocked — model bridge proved against `BitVec 32`, shared
   type confirmed in all three kernels. Only renderers, collection and a gate remain. **The one
   that unlocks real corpus coverage**, since `hmac_sha256`'s meaningful property is bitwise.
5. **`#[decreases]` (rung 8).** Converts recursion from *not eligible* to provable, and its own
   termination obligation is linear arithmetic that existing machinery discharges. **Cost, to be
   accepted deliberately:** the lowering-agreement check degrades from decidable to sampled.
6. **Register A: 0 of 5 discharged, 4 half.** Half means the row is still *trusted*. This is the
   stated goal of the prover-neutral arc and it has not moved.
7. **H19 — the Core→obligation bridge is unproven.** Obligations come from the surface AST;
   nothing proves they match what is compiled. Subsumed by item 1 if the generator runs over Core.

### Open — smaller, each with its reason

- `MIN` negation and float→int casts trap at runtime with **no obligation kind** (two rule-table
  rows under item 1)
- corpus coverage of the non-arithmetic tiers is **zero, deliberately** — the tautology guard
  prevents faking it, and rung 4 is the honest route
- rung 3 (nonlinear) is **2-of-3 kernels**: Lean has no general nonlinear procedure and Mathlib is
  not a dependency. A dependency decision, not a technical gap
- unannotated `let`s contribute no type, so their shift/overflow obligations are dropped
- the FFI closure carries bare names alongside qualified ones and can over-flag (refuses rather
  than admits — the safe direction)
- stack depth is reported, **not gated**
- 5 `partial def`s remain in the report layer (traversal drivers, not discovery)
- Register B rows 2–3; the Isabelle builder collapse; R-0462 widening

### The pattern worth naming, and the one process change

Four times this month a capability was added and under-checked: `partial` keywords documented as
unavoidable for weeks; `spec fn` bodies parsed but unchecked; then checked for names but not
types; then a gate enrolled whose file-granular limit was only found by mutating it. **Every one
was caught by USING the thing, never by reading it.**

If one process change survives all of the above:

> **Write the rejection test before the acceptance test.**

Every hole listed here would have been caught at the moment it was opened.

## Branch `spike/non-arithmetic-multi-kernel` (depends on `spike/multi-prover-evidence`)

> **[historical] Reconciled 2026-10-19:** a merged spike's record. Its live remainders are mapped to owning tasks in the
> [reconciliation record](../project/ROADMAP_RECONCILIATION.md); nothing in it schedules work.

### Landed: rung 2 — boolean postconditions in all three kernels

`--report bool-kernel`, gate `check_bool_kernel.sh` (14/0/0 with real provers). Obligations from
`#[ensures(result == <boolexpr>)]` on a single-`return` body, closed by `destruct; reflexivity`
(Rocq), `auto` (Isabelle/HOL), `decide` (Lean) — three *different* procedures, none arithmetic.

Two properties the arithmetic tier does not have:

- **agreement is EXHAUSTIVE**, not sampled: `2^n` assignments, all checked, so "this lowering
  means the same proposition" is decided. Required making `evalBool` handle `.var` at all.
- **the Rocq proofs are constructive**, `Print Assumptions` = "Closed under the global context".
  Concrete `bool` → Rocq `bool`, NOT `Prop`: over `Prop` De Morgan needs classical logic, so the
  lowering would put an axiom into an attestation. The kernels agree on `bool` and would not on
  `Prop` — a portability fact only a non-arithmetic obligation surfaces.

### The complexity ladder, and what each rung needs

Obligations here are **closed, quantifier-free formulas per function**, lowered to text and closed
by one tactic call. That shape is what makes rungs 1–7 reachable and rung 8 a redesign.

| Rung | Needs | Status |
|---|---|---|
| 1. Linear arithmetic | `omega`/`lia`/`presburger` | done (base branch) |
| 2. Propositional | boolean sorts, case analysis | **done, this branch** |
| 3. Nonlinear arithmetic | `nia`/`polyrith` | partial: `rocqNiaLowering`; H21 blocks SMT replay |
| 4. Bitvectors | `bv_decide`, Rocq/Isabelle Word libs | Lean only; H20 — native checker |
| 5. Uninterpreted functions (EUF) | quantified function variables | **done, this branch** |
| 6. Algebraic datatypes | datatype *declarations* per prover | **done, this branch** |
| 7. Arrays | total `index -> value` model | **done, this branch** |
| 8. Induction over recursion | measure + induction scripts | see below |
| 9. Full functional correctness | 6+7+8 | needs 8 |

### Landed: rung 5 — uninterpreted functions (EUF)

A `spec fn` is body-less, pure and **erased**, so it cannot appear in executable code at all
(`E0101`) — uninterpreted symbols live only in contracts. These obligations are therefore
`requires -> ensures`, not a substituted body, which is also how the rest of the pipeline states
a VC.

**The symbol is a quantified function variable, never a declaration.** `Parameter f : Z -> Z` or a
Lean `axiom` would enter the trusted base and `Print Assumptions` would report it. Quantifying
proves the goal for EVERY interpretation and stays axiom-free — all four Rocq lemmas report
"Closed under the global context", gated.

**What degrades, and this is the first rung where it does.** The boolean tier's agreement check is
exhaustive; EUF has infinitely many interpretations, so there is no table to enumerate. What
replaces it is **propositional abstraction** — each distinct comparison becomes a boolean atom and
the goal is checked as a propositional tautology. Sound in ONE direction only: a tautology implies
validity; a falsifying assignment implies **nothing**, because the abstraction forgets that `f` is
a function. `m = t -> f m = f t` is EUF-valid and abstraction-falsifiable.

An earlier version of this returned a third verdict — "FALSE, no kernel should close it" — which
labelled **congruence**, the defining property of an uninterpreted function, as something no kernel
should prove. There are only two honest verdicts here: tautology (valid), or inconclusive. Gated,
because it is an easy mistake to reintroduce.

Three per-kernel facts the implementation had to discover by running the generated scripts:

- **Rocq needs decidable case analysis, not just `congruence`.** `~(A /\ B) -> ~A \/ ~B` is
  classically valid and Rocq's `tauto` is intuitionistic. Importing `Classical` would close it and
  put an axiom in the attestation; `destruct (Z.eq_dec …)` closes it constructively because `Z`
  equality is decidable.
- **Lean needs the same**, for the same reason, and needs NAMED binders: `intros` produces
  inaccessible names (`f✝`) so the atoms cannot be referenced in `by_cases` afterwards.
- **Lean's proofs are not strictly axiom-free**: `#print axioms` reports `propext` (from
  `simp_all`), while Rocq reports nothing. `propext` is a standard Lean axiom, not classical
  choice — but the two kernels are not equally clean here, and saying they are would be wrong.

**Next rungs, in order:** 6+7 together (datatypes + arrays). The most complex thing reachable WITHOUT rung 8 is those combined in
one obligation — a structural property over a datatype with an array field and an uninterpreted
spec function, quantifier-free, case-analysed in three kernels. Real instances already in the
repo: `crypto_verify`'s `verify_tag(msg, tag) ↔ compute_tag(msg) = tag` with `compute_tag`
uninterpreted (EUF + booleans), and `elf_header`'s `validate_header` (struct fields + array
indexing).

### Rungs 3, 4, 6, 7 — measured feasibility (probed 2026-08-05, real tools)

Each was probed against the actual toolchain rather than assumed. Two turn out to be blocked in
ways worth knowing before anyone budgets for them.

#### Rung 3 — nonlinear arithmetic: **2 of 3 kernels, and that is a ceiling**

| kernel | result |
|---|---|
| Rocq | `nia` closes `0 ≤ a → 0 ≤ b → 0 ≤ a*b`, **axiom-free**; refuses `0 ≤ a → 0 ≤ a*b` with "Cannot find witness" — teeth confirmed |
| Isabelle | `by (simp add: mult_nonneg_nonneg)` closes it |
| Lean | **no general nonlinear procedure.** `nlinarith` is an unknown tactic, and Mathlib is **not a dependency of this project at all** |

Lean core can only apply a hand-picked lemma per shape (`Int.mul_nonneg` for that one goal), which
does not generalise. So a nonlinear tier can attest at most **two** independent kernels unless
Mathlib is added — and adding Mathlib is a large dependency decision, not a tactic choice.

Combined with H21 (nonlinear SMT results cannot be certificate-replayed), nonlinear is the weakest
rung on evidence quality overall: fewest kernels AND no certificate path.

#### Rung 4 — bitvectors: **the three kernels do not share a type**

| kernel | result |
|---|---|
| Lean | `bv_decide` works with `import Std.Tactic.BVDecide`. H20: its checker runs as native code |
| Rocq | stdlib has only `Z`-level bitwise (`Z.land`, axiom-free). **No fixed-width word type.** A faithful 8-bit model needs an external library (bbv / coq-bits — absent) or `Z`-with-modulus |
| Isabelle | `HOL-Library.Word` **loads fine** (session must be `= "HOL-Library"`, not `HOL`), but builds in ~1:27 wall / 8:09 CPU — too slow for a per-run gate without a prebuilt heap. Two attempts at the word bitwise-operator spelling failed with `Type unification failed: clash of "_ ⇒ _" and "_ word"`; that is a syntax question, not a capability one, and was left unresolved rather than guessed at |

This is the one rung where the kernels would not be proving the same thing. Modelling Rocq's side
as `Z`-with-modulus makes the three lowerings *different theories*, so the tier would need a proof
that the model is equivalent to fixed-width semantics — a Register-B-style transformation
obligation, not merely a lowering. **That equivalence proof is the actual work of rung 4**, and
`bv_decide`'s native checker (H20) caps the evidence quality of the Lean column regardless.

#### Rung 4 — probed properly (2026-08-06): the MODEL BRIDGE exists, the shared type does not

Two separable questions, and they came out differently.

**1. The model bridge is real and cheap — Lean proves it.** The worry was that a `Z`/`Nat` model of
`u32` bitwise operations might not correspond to fixed-width semantics, making a three-kernel proof
about the model say nothing about the program. Lean settles it directly against its native
`BitVec 32`:

```
theorem and_model (x y : BitVec 32) : (x &&& y).toNat = x.toNat &&& y.toNat := by simp
theorem or_model  (x y : BitVec 32) : (x ||| y).toNat = x.toNat ||| y.toNat := by simp
theorem xor_model (x y : BitVec 32) : (x ^^^ y).toNat = x.toNat ^^^ y.toNat := by simp
theorem and_range (x y : BitVec 32) : (x.toNat &&& y.toNat) < 2 ^ 32 := …
```

All four prove; `#print axioms` reports only `propext` and `Quot.sound`. So the chain
*property-over-naturals* + *model-equivalence* ⟹ *property-for-u32* is available, and the second
half is proved ONCE per operation rather than per obligation. This is the part I expected to be
the hard piece of rung 4, and it is not.

**2. The shared type is the actual blocker, measured across four attempts.** `u32` is unsigned, so
naturals are the natural common model:

| kernel | type | result |
|---|---|---|
| Lean | `Nat` | `x ^^^ x = 0`, `x &&& 0 = 0` — **prove** |
| Rocq | `N` | `N.lxor_nilpotent`, `N.land_0_r` — **prove, axiom-free** |
| Isabelle | `nat` | **unresolved.** `x AND 0 = 0` fails with `Clash of types "_ ⇒ _" and "nat"` |

Neither `Int` (Lean) nor `int` (Isabelle) has bitwise operations at all, so integers are not the
common type either. Four spellings of Isabelle's bitwise operators were tried across `8 word` and
`nat` — all failed with a type-inference clash suggesting `AND`/`XOR` resolve to lattice operators
rather than `Bit_Operations`. That is a *syntax/import* question I could not settle, not a
capability claim about Isabelle: its `HOL-Library.Word` session loads fine.

**SOLVED (same day), by looking it up instead of guessing a fifth time.** `AND`/`OR`/`XOR` are the
right names, but Isabelle keeps the notation in a BUNDLE — `Bit_Operations.thy` line 4186:
`bundle bit_operations_syntax`. Without opening it, `AND` resolves to a lattice operator, which is
exactly the `Clash of types "_ ⇒ _"` all four attempts produced. One line fixes it:

```
theory BwThy
imports Main
begin
unbundle bit_operations_syntax          (* <- this *)
lemma xor_self: "ALL x::nat. (x XOR x) = 0"  by simp
lemma and_zero: "ALL x::nat. (x AND 0) = 0"  by simp
end
```

Worth recording as a method note, not just a fact: four guesses cost more than one `grep` of the
distribution's own `.thy` source, which found the bundle immediately.

**So rung 4's blockers are all cleared.** The shared type exists in every kernel:

| kernel | type | positive | negative control |
|---|---|---|---|
| Lean | `Nat` | `x ^^^ x = 0`, `x &&& 0 = 0` | `x &&& y = x` REFUSED |
| Rocq | `N` | axiom-free via `N.lxor_nilpotent`, `N.land_0_r` | — |
| Isabelle | `nat` (unbundled) | both by `simp` | `x AND y = x` REFUSED |

Plus the model bridge above, proved in Lean against `BitVec 32`. **What remains is only the tier
itself** — renderers for `Nat`/`N`/`nat`, obligation collection for `u32` bitwise contracts, and
the gate — i.e. the same shape as the five tiers already landed, with no unknowns left in it.

#### Rungs 6 + 7 — **DONE 2026-08-05.** The ceiling is reached

Struct declarations are now emitted alongside the goal (`Record` / `record` / `structure`) and
arrays are modelled as total `index -> value` functions. `--report bool-kernel`, gated at 38/0/0
with real Rocq 9.0.1 and Isabelle2025-2.

**Four theories, three kernels, no arithmetic and no induction** — booleans, EUF, datatypes and
arrays in one report, with a negative control per tier refused by every kernel. That combination
is what the ladder called the ceiling of this architecture.

Two things that made it cheaper than expected:

- **Arrays needed no theory.** The bounds family already proves every index in range, so a
  fixed-size array is a *total* function and there is no partiality story — usually the hard part
  of an array encoding. Rung 7 was a renderer case, not a theory.
- **Rocq's `Record` adds nothing to the trusted base**: all four goals still report
  "Closed under the global context", gated.

The one genuine three-way syntax split is **field projection**: Rocq and Isabelle put the field
first (`magic h`), Lean puts it last (`h.magic`). Everything else about the proposition is shared.

##### Original assessment, kept for the reasoning

- `CoreExtract` emits Gallina `Definition`s but **zero** `Inductive`/`Record` declarations
  (grepped). Nothing currently tells any prover that a Concrete struct or enum exists.
- Needs: per-kernel declaration emission (`Inductive` / `datatype` / `inductive`), and the
  constructor injectivity + disjointness facts made available to the tactics.
- **Arrays are easier than they look here:** Concrete's arrays are fixed-size and the bounds family
  already proves every index in range, so a total `index → value` function model is sound — there
  is no partiality story to build, which is usually the hard part of array theories.
- This remains the stated ceiling: rungs 5 + 6 + 7 in one obligation (EUF + datatypes + arrays,
  quantifier-free, case-analysed in three kernels).

**Order followed:** 6 + 7 landed (see above — the ceiling). Rungs 3 and 4 are deliberately NOT
implemented, and the reasons are different:

- **Rung 3 was not built because a 2-of-3 tier would misrepresent the branch's claim.** The whole
  point of these tiers is that N *independent* kernels agree; a nonlinear tier with Lean absent is
  materially weaker, and labelling it "multi-kernel" would overstate it. It is cheap to add if
  2-of-3 is acceptable — the Rocq and Isabelle tactics are both verified working above — but that
  is a call about what the evidence is allowed to claim, not a technical gap.
- **Rung 4 was not built because the three kernels do not share a type**, so the tier would owe a
  proof that a `Z`-with-modulus model is equivalent to fixed-width semantics. That is a
  Register-B-style transformation obligation and the actual content of the rung; the lowering is
  the easy half. `bv_decide`'s native checker (H20) also caps the Lean column's evidence quality
  regardless of how the model question is settled.

### CAPABILITY vs COVERAGE — the rungs have zero instances in the existing corpus

Measured after landing rungs 2/5/6/7, and it corrects two claims I had written into this document
without checking them.

**`crypto_verify` and `elf_header` are NOT instances of anything.** I named them as the real
instances of the ceiling. They have **zero** `#[requires]`/`#[ensures]` clauses — nothing for any
tier to prove. The claim was made from the shape of the code, not from its contracts.

**`hmac_sha256` has 6 contract clauses, and none of them are reachable by these tiers either.**
(My first count said 14 — a grep for `#[requires|#[ensures` that also matched `#[ensures_proof(`.
The AST-derived count is 6. Both of my grep-based numbers in this section were wrong, in opposite
directions, which is why the report now derives the number from the AST instead.) They
look like exactly what rung 5 should handle:

```
spec fn ch_spec(x: u32, y: u32, z: u32) -> u32;
#[ensures(result == ch_spec(x, y, z))]
#[ensures_proof(Examples.HmacSha256.Proofs.ch_refines)]
fn ch(x: u32, y: u32, z: u32) -> u32 { return (x & y) ^ ((x ^ 0xFFFFFFFF) & z); }
```

But this is a **refinement** obligation, not an EUF one, and the difference is decisive: with
`ch_spec` uninterpreted it is **unprovable** — nothing licenses the claim that an opaque function
equals that expression. The repo discharges it by linking to a registered Lean proof where the
spec HAS a definition (`Concrete.Proof.chExpr`). So excluding `result` clauses from the EUF tier
was correct, and it is also why the tier finds nothing here.

**What this means, stated plainly:** rungs 2, 5, 6 and 7 are real, verified CAPABILITY —
38 gate assertions across three kernels with negative controls each — and they have **no coverage
of the existing corpus**. Every instance is a purpose-built demo. The rungs are what the pipeline
*could* prove non-arithmetically, not what it currently does.

**The blocker was structural — and is now FIXED at the language level rather than bridged.**
Concrete's `spec fn` was always body-less, so a spec's meaning lived in Lean and never in Concrete
source. The obvious fix was a Lean-definition → Gallina/HOL bridge: translate a Lean definition
into two more provers. That would have been large, fragile (it means parsing Lean), and would have
left the spec language permanently Lean-shaped.

**A `spec fn` may now carry a definitional body instead:**

```
spec fn double_spec(x: i32) -> i32 = x + x;
```

A refinement obligation is then an equation between two expressions of *this* language
(`2 * x = x + x`), which every kernel can already see — no bridge, no Lean parsing. Parser change is
additive: the body-less form still parses identically and keeps its uninterpreted meaning, so
nothing existing shifts (`make test` unchanged at 1703/0).

Gated in all three kernels, with the negative control: a body that does NOT refine its spec is
refused by each, so portability did not make refinement lax. `--report bool-kernel` also names
refinement obligations whose spec is still body-less, so the residual gap stays visible.

**Converting the existing corpus would MANUFACTURE coverage, not gain it — checked, not assumed.**
`hmac_sha256`'s registered Lean spec for `ch` is:

```
chExpr = (x AND y) XOR ((x XOR 0xFFFFFFFF) AND z)      -- the spec
         (x & y)   ^   ((x ^ 0xFFFFFFFF)   & z)        -- the function body
```

Character for character the same expression. Giving that spec a definitional body would make its
refinement obligation `body = body`: trivially true, provable by every kernel, and proving
**nothing** — while the coverage number rose. So the tier now DETECTS a spec that restates its
implementation, flags it, and excludes it from the count. A metric that goes up when you copy the
body into the spec is worse than one that reads zero, and this was one edit away from happening.

The repo already knows the distinction, which is the useful signal: `ch` carries BOTH
`#[ensures_proof(ch_refines)]` — the mechanical spec-equals-body step — and
`#[proof_by(ch_selects_high)]`, the semantic property that actually says what `Ch` does. The
meaningful obligation is the second one.

**So genuine coverage of `hmac_sha256` needs rung 4.** `ch_selects_high` is bitwise reasoning over
`u32`, and rung 4 is exactly the rung where the three kernels do not share a type (Rocq has no
fixed-width word). The chain is: real corpus coverage → semantic bitwise properties → rung 4 →
the `Z`-with-modulus model-equivalence proof. That equivalence obligation is the honest prerequisite,
and it is Register-B-shaped rather than a lowering.

**Where the new mechanism does apply:** specs whose meaning is genuinely independent of the
implementation, which is what a spec is for. The demo shows both — real refinements that all three
kernels prove, and a tautological one that is caught.

**Cheapest path to coverage of existing code:** add contract-to-contract
clauses (`requires -> ensures` over fields, arrays and opaque symbols) to a flagship example. Those
are exactly what the tiers prove, and `elf_header`'s header-validation invariants are naturally of
that shape — but they would be contracts written FOR the tier, which is coverage of a kind worth
labelling as such.

### Lifting the recursion ban — how, and what it costs

The ban does **two unrelated jobs**, and separating them is the whole design:

1. **execution predictability** — bounded stack, what `--check predictable` is for;
2. **proof tractability** — `assessEligibility` also excludes recursive functions.

For (2) termination is not optional: **a non-terminating function cannot be defined in Gallina at
all**, and Lean needs `termination_by`/structural. Isabelle admits partiality but every theorem
then carries domain conditions. So the ban is forced by the target logics, not chosen.

The mechanism is standard — an author-supplied measure:

```
#[decreases(n)]
fn count(n: Int) -> Int { if n <= 0 { return 0; } return count(n - 1); }
```

**The key observation: the termination obligation is LINEAR ARITHMETIC** — `n > 0 → n - 1 < n` —
so `omega`/`lia`/`presburger` close it today. Termination becomes another obligation family
alongside overflow and bounds, reusing the multi-kernel machinery for the hard part. Extraction
then emits `Fixpoint`/`Program Fixpoint`, `function … termination`, `termination_by`.

For (1), a proved measure gives termination but **not a stack bound**; bounding the measure's
initial value at every call site (`n ≤ 64`) bounds depth. Also arithmetic, also dischargeable.

Work items, roughly in order: `#[decreases]` syntax → termination obligation family → per-kernel
extraction of recursive definitions → induction-capable proof scripts (`render` already returns a
whole source file, so it can emit `induction n; simp` as easily as `lia`).

**The one real cost, and it should be a deliberate decision rather than a side effect:** the
agreement check degrades. For an inductive property over unbounded inputs you cannot decide that
a lowering means the same proposition — only sample it. The multi-kernel claim rests on that check,
so rung 8 trades *faithfulness of the lowering* for *strength of the verification*:

| | verification | lowering faithfulness |
|---|---|---|
| decidable goals (rungs 1–7) | weaker — no induction | **complete** (booleans: exhaustive) |
| inductive goals (rung 8) | **stronger** | sampled — undecidable |

**Stays refused regardless:** recursion through a function pointer cannot be measured locally, and
`--check predictable` now rejects it. Mutual recursion needs a combined measure across the cycle.
`--report stack-depth` would need the measure's bound, and if that bound is symbolic the byte
figure becomes conditional — worth care, since a false byte count was removed from it this week.

## PRE-MERGE STATE of `spike/multi-prover-evidence` (recorded 2026-08-05)

> **[historical] Reconciled 2026-10-19:** a pre-merge snapshot. Its live remainders are mapped to owning tasks in the
> [reconciliation record](../project/ROADMAP_RECONCILIATION.md); nothing in it schedules work.

Written for a merge performed on a different machine. Everything below is either a
**behaviour change a reviewer should expect**, or **work not done**.

### Behaviour changes that reclassify existing code

These change reports and profile admission. None is a type error; nothing stops compiling. Each
was measured against the corpus before landing, and the measured number is given.

| Change | Effect | Measured impact |
|---|---|---|
| Loop classifier is fail-closed | a loop is `bounded` only if a condition variable is stepped toward the bound by a constant | **zero** — all 31 previously-`bounded` loops use `i = i + 1` |
| Indirect calls refused by the profile | a body calling through a fn pointer cannot be certified acyclic | 1 test golden |
| FFI admission is transitive | reaching an extern through another function crosses FFI | 3 examples (`http`, `integrity`, `verify`), all true positives |
| Recursion admission is transitive | a caller of a recursive function is not predictable | **14 functions in 10 examples**, all true positives; 4 goldens |
| Stack-depth unboundedness propagates | no byte-exact bound where recursion cannot be ruled out | `Max stack bound` is now `0 bytes (none)` on affected programs |
| Bounds/shift sizing is per-scope | shadowed arrays and variables are sized from the binding in effect | 4 goldens; **two certified-false claims removed** |
| Proof-surface wording | "passes the predictable profile" → "is eligible for proof" | 1 test golden, 4 book transcripts |

**The two that were unsound, for the changelog:** a shadowed array gave `a[10] → 10 < 16` on a
4-element array (kernel-**proved**), and a shadowed variable gave `x << 40 → 40 < 64` on an `i8`
(kernel-**proved**). Both are now refuted. Memory safety was never affected — codegen
bounds-checks and traps regardless — but the proof REPORT was false.

### Not done — ranked

1. **Div/shift/overflow do not name their gaps.** Bounds prints
   `ARRAY ACCESSES OUTSIDE the bounds fragment`; the other three families drop an obligation
   silently when a type cannot be resolved. `#[overflow_checked]` on a function with an
   unannotated `let` yields `(no VCs generated)` and no explanation. Cheap: the reporting hook
   exists, this is the last inconsistency in that cluster.
2. **~10 soundness gates have no negative controls.** This session's tally: **six** of my own
   gate assertions fired on non-regressions, **two** silently tested nothing (one interrogated a
   stale binary, one skipped its whole section when the build broke). Green counts overstate what
   is pinned. Highest-value non-feature work on this list.
3. **Register A: 0 of 5 rows discharged, 4 half.** A half-discharged row is still a *trusted*
   row. This is the branch's stated goal and it did not move this week.
4. **H19 — the Core→obligation bridge is unproven.** Obligations are computed from the surface
   AST; nothing proves they correspond to what is compiled. This week's defect class one layer
   down. A project, not a task.
5. **Register B rows 2 and 3.** Row 2 (`eliminate_div_mod`) needs `DecidableEq` on `Term` plus
   signed `|r| < |b|` with no Mathlib. Row 3 is model-theoretic, not a longer row 2.
6. **A non-arithmetic multi-prover example** (requested 2026-08-05). Every kernel-agreement
   demo today is linear integer arithmetic. A datatype/structure example across Lean + Rocq +
   Isabelle would exercise the one-printer-many-tools claim where it is weakest. Note the known
   ceiling: SMT datatype goals are provable but **not** Alethe-certifiable, so such an example
   would be kernel-proved rather than certificate-replayed.

### Not done — smaller, each with a reason

- **`MIN` negation and float→int cast** trap at runtime but have no obligation kind at all.
- **Unannotated `let`s contribute no type**, so shift/overflow obligations for them are dropped.
  Deliberate: inferring a type here risks disagreeing with the checker, and a wrong type is the
  defect that was just fixed. Needs the checker's real types threaded in.
- **The FFI closure carries bare names alongside qualified ones**, because the call graph resolves
  callees and the effects report does not. A same-named function in another module can be
  over-flagged (which refuses rather than admits — the safe side). Resolving callees properly
  removes it.
- **`proofReport`'s extraction eligibility is still direct**, not transitive. Different question
  (can this body reach Gallina), and changing it would alter which proofs are attempted.
- **Stack depth is reported, not gated** — `PREDICTABLE_BOUNDARIES.md` says so explicitly.
- **5 `partial def`s** remain in the report layer (traversal drivers, not discovery).
- **Isabelle builder collapse** needs the batch/single split.
- **R-0462 widening**: capability holders, struct params, non-integer returns.
- **One unclassified `dropped by both`** in `fixed_capacity`.

### Known holes still open (from `docs/verification/KNOWN_HOLES.md`)

- **H19 — the Core→obligation bridge is unproven.** Listed above; repeated here because it is the
  largest single gap in the trust story.
- **H20 — `bv_decide`'s certificate check runs as native code.** The bitvector decision procedure
  is trusted at the level of its checker binary, not the Lean kernel.
- **H21 — nonlinear SMT results cannot be certificate-replayed** (upstream limitation). A
  nonlinear goal closed by an SMT solver stays `solver_trusted`; only linear goals reach
  `proved_by_lean_replay`.

Related ceiling, measured and documented rather than assumed: **SMT datatype goals are provable
but not Alethe-certifiable.** cvc5 proves a ground datatype goal and cannot emit a replayable
certificate for it, so non-arithmetic obligations are kernel-proved rather than
certificate-replayed. This is what a non-arithmetic multi-prover example (item 6 above) would run
into, and it is a property of the tooling, not of this repo.

### Register status, plainly

- **Register A** (obligation sufficiency): 0 of 5 discharged, 4 half-discharged. Half means the
  row is still *trusted*.
- **Register B** (transformation soundness): 1 of 3 rows discharged (`eliminate_tmod`).
- **Register C** (evidence composition): **DISCHARGED by compile-time rows** — C1-C4 are theorems
  in `Concrete/Report/Evidence.lean` ("a green build is the proof"), with 59 structural/wiring
  assertions in `check_evidence_algebra.sh` on top, no `sorry`/`admit`/`native_decide`. Earlier
  text here said it was "gated rather than expressed as a row
  table; no per-row discharge tracking exists for it yet, which is itself a gap.
- **Discovery completeness** (added 2026-08-05): 4 of 4 runtime-safety families. This register did
  not exist before and answers the question the other three structurally cannot — whether an
  obligation is generated at all.

### The composition test is opt-in

`check_multi_kernel.sh`'s final section — a disagreeing lowering must not earn the two-kernel
badge or pass the release gate — is **skipped unless `MULTI_KERNEL_MUTATE=1`**, because it mutates
tracked source and needs two rebuilds. It therefore does NOT run in the normal 78/78 pass. Any
claim that composition is gated should say "when that variable is set".

### Writing gates that actually bite — checklist earned the hard way

Recorded because this session produced eight concrete failures of gate quality, and the pattern
is consistent enough to be a rule rather than an anecdote.

1. **Assert the property, not the code.** Six assertions of mine fired on non-regressions. Every
   one quoted something syntactic — an argument list, a call site verbatim, a mechanism ("the
   access is NAMED") — and broke when the code improved. Twice the *same* assertion broke twice.
2. **A check that greps one error class treats every other failure as success.** A sweep reported
   GREEN from a loop whose exit condition looked only for termination errors, so
   `cannot mix partial and non-partial definitions` read as a pass.
3. **Never interrogate a binary you did not just build.** An end-to-end assertion passed while the
   fix was reverted, because the mutated source failed to compile and `lake` left the old
   executable in place. An mtime comparison is also wrong — `lake` does not relink on a cache hit.
   Build inside the gate and fail if the build fails.
4. **A section guarded on a temp dir skips itself when the build breaks.** Guard on failure, not on
   the presence of a variable an unbuildable tree never sets.
5. **Mutations must COMPILE to count.** A mutation rejected by the unused-variable lint looks
   killed and tests nothing. Classify those INVALID; do not report them as kills.
6. **Distinguish an equivalent mutant from a survivor.** Making a declaration visible to its own
   statement is unobservable (a `let` initialiser cannot index the name being declared) — that is
   INVALID, not a gate weakness.
7. **A completeness theorem needs three things**, or it is true and worthless: a satisfiable
   antecedent (non-vacuity), a negative case (the conclusion discriminates), and its boundary
   pinned so `∀ e, P e → Q e` is not read as global completeness when `P` is narrow.
8. **A false number is worse than a missing one**, because it is quotable. `--report stack-depth`
   stated `Max stack bound: 32 bytes` for arbitrarily deep recursion.
9. **`set -o pipefail` breaks `failing_cmd | grep -q`.** The pipeline reports failure even when
   grep MATCHES, because the producer exited non-zero — and a compiler rejecting a bad program
   exits non-zero by design. Two assertions checking that a bad program IS rejected therefore
   reported a working fix as broken. Capture output into a variable first, then match it.
10. **A metric that cannot move is decorative — and one that can be gamed is worse.** A coverage
   count still read "0 reached" after a new tier started covering obligations, because it had not
   been updated. Separately, a coverage number that RISES when you copy the implementation into
   the spec is worse than one that honestly reads zero: assert that the number moves, and exclude
   tautologies from it.

### Merge mechanics

- Branch was **191 commits ahead of `main`, 1 behind** at the time of writing.
- **The `lli` failure is environmental, not ours.** LLVM 21.1.8 cannot JIT on some hosts; 49
  tests fail locally on an affected machine. `run_tests.sh` probes `lli` functionally and falls
  back to clang. If the merging machine shows ~49 failures in that shape, check this first.
- Two of this week's clusters are **independent of the multi-prover spike** — the
  bounds/shift/overflow sizing fixes and the predictable-profile fixes. If the whole branch is
  too large to land at once, those are separable and carry the unsoundness fixes.

## Outstanding work recorded 2026-08-04

> **[historical] Reconciled 2026-10-19:** a dated backlog whose items were completed, merged into owners, or promoted to tasks. Its live remainders are mapped to owning tasks in the
> [reconciliation record](../project/ROADMAP_RECONCILIATION.md); nothing in it schedules work.

Everything below was found or deferred during the R-0460..R-0465 arc and is filed here so it
is not carried in someone's head. Each entry says what is *measured* rather than what is
hoped, because several of these exist precisely because a number was restated instead of
recomputed.

### The `predictable` profile audit (2026-08-05) — three fail-open gates

Same bug class as the obligation layer, in the checker analyses that back
`--check predictable`. All were **fail-open**: an analysis that could not establish a property
reported that the property held.

- **Bounded iteration admitted non-terminating loops.** The rule asked "is the condition a
  comparison?" and "is the step list non-empty?" and tied them to nothing, so
  `for (let mut i = 0; i < n; z = z + 1)` and `for (…; i < n; i = i - 1)` were both `bounded`,
  the module PASSED, and the report said `0 unbounded loops`. Now a variable in the condition
  must be stepped toward the bound by a constant; everything else is refused. Zero corpus
  change — all 31 previously-`bounded` loops use the `i = i + 1` idiom.
- **An indirect call could claim no-recursion.** The call graph has no edge for `f(x)`, so
  recursion through a function pointer was invisible: `ping` calling `apply(ping, …)` reported
  `recursion: none` and passed. Now refused.
- **`--report stack-depth` stated a byte-exact bound for unbounded recursion** — `32 bytes` for
  the above, and recursive callees were filtered out of every chain so a caller of an unbounded
  function also kept a finite bound. A false NUMBER is worse than a missing one. Unboundedness
  now propagates to callers.

- **The remaining two gates, audited 2026-08-05.** **Alloc and blocking are sound** — the
  compiler already refuses `E0520: requires Alloc but caller has (none)`, so capabilities are
  transitive by enforcement, and a fn-pointer type carries its capset (passing an `with(Alloc)`
  function as a pure `fn(i32) -> i32` is an `E0220` type error), so a combinator cannot launder
  one. Nothing to fix. **FFI was the odd one out**: no capability behind it, only a direct-callee
  check, so `f` calling `g` calling an extern reported `ffi: no` and `evidence: enforced`. Now
  closed over the call graph, which makes FFI agree with the two gates that already were.
  Reclassified three real examples (`http`, `integrity`, `verify`) and all were true positives —
  in `http` the chain is `send_string` → `handle_client` → `main`.

**Why the obligation work could not have found these:** they are LIVENESS claims. Every proof
obligation rules out a bad *event*; a program that hangs or overflows the stack performs no bad
event, so no obligation and no register could notice. This is the first defect cluster this
week that no amount of work on the obligation layer would have reached.

- **Recursion admission is now TRANSITIVE — DONE 2026-08-05**, on an explicit decision rather
  than as a side effect. `main` calling a directly recursive `fib` was `enforced` because the
  label came from each body in isolation; predictable execution is a property of running the
  function, so it now closes over the call graph like FFI, alloc and blocking already did.
  Recursion was the last gate asking "is it written here?" instead of "can you reach it?".
  **Measured:** 14 functions across 10 examples moved `enforced` → `reported`, all true
  positives; four goldens updated. Nothing stops compiling — it is an admission/label change,
  and module-level `--check predictable` already failed these programs.
  **A consumer was silently left behind, twice:** of seven call sites, the FFI closure reached
  only four — missing the `Check/Policy.lean` path that REJECTS builds. `profileClosures` now
  returns both closed sets together so one cannot be taken without the other.
  **Still direct, deliberately:** extraction eligibility (`proofExclusionReasons`) answers a
  different question and would change which proofs are attempted.

**Superseded note — the profile is not transitive — `caller` calling a directly
recursive `recurses` is still `enforced`, though module-level `--check predictable` does fail.
Making admission transitive is a semantics change that would reclassify existing code, so it is
a decision to take explicitly rather than as a side effect. The stack-depth report, which makes
a numeric rather than a label claim, IS transitive as of this change.

### From R-0455 (term IR + Register B) — slices 1–2 landed, four things remain

- **Register B row 2, `eliminate_div_mod` (fresh-variable form).** Two concrete blockers, in
  `docs/compiler/TRANSFORM_REGISTER.md`: it needs hand-written `DecidableEq` on `Term` (Lean cannot
  derive it through the nested `List Term` in `sym`), and its real content is the magnitude
  bound `|r| < |b|`, which needs `Int.natAbs` reasoning with **no Mathlib in this repo**.
  Recorded there: comparing terms by their `repr` STRINGS was tried and is the wrong answer.
  Also recorded: the identity `q*b + r = a` alone is true for *any* `q` and constrains
  nothing, so a row adding only it would be sound and useless.
- ~~The remaining `partial def`s in the report layer~~ **PARTLY DONE 2026-08-05.** Of 17,
  four (`exprIntTy`, `exprIntervalMax`, `cartesianEnvs`, `cartesianEnvsPer`) recovered
  structural recursion by deleting the keyword. The rest recurse under `List.flatMap` over a
  nested-inductive list, which the termination checker cannot see through; `collectShiftsE`
  was converted with `attach` + `decreasing_by` and the pattern generalises.
  **Two corrections to earlier claims, both found by testing rather than reasoning:**
  a first sweep reported "green" from a loop whose exit condition grepped only for
  *termination* errors, so a `cannot mix partial and non-partial definitions` failure read as
  success — `partial` is all-or-nothing inside a `mutual` block, so the four S/E walker pairs
  cannot be split. And a well-founded definition does **not** become `rfl`-reducible; the
  kernel will not unfold `WellFounded.fix`. It becomes reasoning-accessible (equation lemmas,
  recursion principle), which is a different and smaller gain than the seven structural ones.
- **NEW: obligation discovery completeness.** `collectShiftsE_complete` proves a present shift
  is always found — the first statement about whether an obligation gets *generated* at all,
  which Registers A/B/C are all structurally unable to ask. **Its antecedent is narrow**
  (`hasShift` misses a shift under a call argument, which the walker does traverse) and that
  gap is pinned by a build-enforced example so the theorem is not over-read.
  **DONE 2026-08-05 for all four families**: `collectDivisorsE`, `collectIndexUsesE` and
  `collectArithE` followed (each `mutual` block totalised as a unit, since `partial` is
  all-or-nothing inside one). 17 report-layer `partial def`s are now 6, none on discovery.
- **A real bug found by writing the predicate, not by testing.** `(a)[i]` generated NO
  array-bounds obligation — absent from `--report vcs`, not merely unproven — while `a[i]`
  did, because discovery matched a bare `.ident`. Fixed via `arrayRootName`. The suite was
  1702/0 before AND after: no fixture had ever used a parenthesised array access, so the
  regression guard is end-to-end rather than a `rfl` lock. Memory safety was never affected
  (codegen bounds-checks every access); the missing thing was the proof, and any sign of its
  absence. `b.data[i]` recorded nothing either — **CLOSED 2026-08-05**: discovery now records the
  array EXPRESSION rather than a name, and `arrayAccessOf` resolves the length either from the
  scoped size map or by following the field path with the new `placeTy`. `b.data[i]` yields
  `0 ≤ i ∧ i < 16`; `b.data[99]` is REFUTED — an out-of-bounds access the proof layer could not
  see before. Nested `m[i][j]` now records both levels, and `hasIndex` lost its narrow
  antecedent, so bounds discovery completeness covers ALL accesses. It became cheap only after
  `ScopeDecls` threaded types; the wider-peel shortcut was refused because it would have
  produced an obligation about the wrong array.
- **The same defect was in TWO more families, via `varTyMap` — DONE 2026-08-05.** That was the
  identical construct for TYPES: one flat, per-function, name-keyed map. **shift** produced a
  WRONG obligation — `x << 40` on an `i8` took width 64 from a shadowed `x : i64`, generating
  `0 ≤ 40 ∧ 40 < 64`, reported **proved**: a certified 40-bit shift of an 8-bit value. Now
  `40 < 8`, refuted. **overflow** produced a MISSING obligation — shadowing broke type
  resolution, so an addition inside an explicitly `#[overflow_checked]` function had no VC and
  no mention of one. Now covered at the `i8` range. Fixed by threading types and array lengths
  together as one `ScopeDecls` record through `scopedWalkSized{S,B}`. `varTyMap`,
  `arraySizeMap` and `collectLetTys` are deleted — no flat name-keyed map remains in this
  layer. A single mutation reversing shadowing precedence now fails 8 gate assertions across
  all three families, because they share one environment.
  **Deliberately not done:** unannotated `let`s contribute no type — inferring one could
  disagree with the checker, and a wrong type is the defect being fixed. Array lengths are the
  exception, since `let a = [0; 16]` fixes the length with no inference.
- **A WRONG obligation, found by the same question and more serious than the missing ones.**
  `boundsObligations` resolved an array's length by NAME with `find?` — first match wins — so a
  shadowed array produced a bound from the wrong binding: `a[10]` on a 4-element array
  generated `0 ≤ 10 ∧ 10 < 16`, which the kernel PROVED. The report read `2 proved, 0
  outstanding` for a program that traps. Every other bug in this cluster was a missing claim;
  this was a false claim, certified. Fixed by refusing to answer when a name has conflicting
  sizes, then **DONE properly 2026-08-05**: sizes are threaded per SCOPE through
  `scopedWalkSized{S,B}`, next to the hypothesis scope that walker already carried. The
  shadowed access now yields `10 < 4` and is reported as a **counterexample** — the compiler
  detects an out-of-bounds access it previously certified as proved. Declarations shadow
  outward, do not escape their block, and a `for`-init binding reaches condition/step/body.
  Generation and gap-reporting share one walk so nothing falls between them.
  **Two lessons from mutating it:** the refusal-era gate assertions FAILED on the correct
  implementation because they pinned the mechanism ("refuses to answer") rather than the
  property (the right bound) — rewritten. And dropping `for`-init bindings survived both the
  gate and all 1702 tests, since no fixture declares an array in a for-init; now covered.
- **Two more silent bounds gaps, closed.** `let a = [0; 16]` (unannotated — the size sits in
  the initialiser, but only annotations were read) and arrays declared inside an
  `if`/`while`/`for` body (only the flat top-level statement list was scanned).
- **The silence is now reported.** `--report vcs` prints `ARRAY ACCESSES OUTSIDE the bounds
  fragment`, naming every access that reached no obligation, including on the zero-VC path.
  `make test` stayed 1702/0 through ALL of these fixes — the corpus contained no parenthesised
  access, no shadowed array, no unannotated array literal and no nested declaration, which is
  why every regression guard here is end-to-end rather than a `rfl` lock.
- **Register B row 3, `eliminate_algebraic`.** Different in kind — requires the
  axiomatization be conservative over the datatype theory, which is model-theoretic rather
  than a rewriting argument. Not a longer row 2.
- **Absorb the four drivers into the IR** (`rocqLowering`, `isabelleLowering`,
  `rocqNiaLowering`, `isabelleSmtReplayLowering`) with `tactics` as an ordered, budgeted
  driver FIELD. Measured cost of not doing it: `rocqNiaLowering` is a whole cloned
  `ProverLowering` record existing to change one word in a proof script.
- **Collapse N printers to one.** The print step is validated, not provable (the target
  syntax is not ours), so its cost is per-printer and permanent. `exprToSmt` already is that
  printer for the arithmetic tier and is validated as of 2026-08-03.
- ~~Model casts in the IR~~ **DONE 2026-08-04.** Carried as a wrap at the target width
  matching `Interp.evalCast`/`IntArith.wrapToType`, not as identity; unknown-width targets
  still rejected. This unblocked R-0455's headline defect, which previously could not be
  demonstrated at all because every division-carrying obligation also carried a cast and was
  dropped by both layers. Measured: the IR now recovers such an obligation, and
  `examples/` contains none, so the corpus number is 0 for a reason about the corpus — a
  constructed fixture in the gate exercises the capability.
  **Remaining:** one `dropped by both` in `fixed_capacity`, unclassified.
- ~~`exprToProver` is also a `partial def`~~ **DONE 2026-08-04.** It never needed to be —
  nor did `exprToSmt`, `exprToLeanProp`, `arithToBVW`, `exprHasNonlinMul`, `collectIntLits`
  or `conjuncts`. All seven recurse only on direct subterms and were marked `partial` by
  habit; removing the keyword was the entire fix, and it makes the SMT printer, both prover
  lowerings and the bv renderer kernel-reducible. The drop the IR exists to repair is now
  pinned by `rfl` instead of only counted at runtime.
  **What this does NOT change:** a provable printer still does not prove the TARGET syntax
  means what we think — that needs the target's semantics, which are not ours. Print fidelity
  stays validated rather than proved; what improved is that the printer's own behaviour is
  now provable.
- ~~`evalIntEnv` is a `partial def`~~ **DONE 2026-08-04.** `evalIntEnv`/`evalBoolEnv` are now
  thin wrappers over `ofExpr` + `TermIR.eval*`, both structural, so the yardstick every
  lowering-agreement check measures against is kernel-reducible and its behaviour is pinned by
  `rfl` — including the truncating-division convention, which `check_vc_bridge_register.sh`
  previously could only GREP for and now asserts as real locks. Two behaviour changes, both
  gains: casts now have a reference value (previously `none`, so the instance was skipped),
  and `geq`/`gt`/`neq` are canonicalised rather than special-cased. Verified identical on the
  provers: multi-kernel 78/78.
  **Remaining in this area:** the report layer still has ~17 `partial def`s (walkers and
  collectors). They are not on the trust path, but the same question applies to each: is the
  keyword load-bearing or habitual? Seven of seven checked so far were habitual.

### From R-0462 (artifact fuzz) — works, covers almost nothing

- **Measured coverage: 4 of 356 own functions across `examples/`.** The machinery is proven
  able to find real traps (three fixtures, exit 134) but its useful coverage is ~1%. Widening
  it — capability-holding functions, struct parameters, non-integer returns — is where the
  fault-finding value actually is. This pass built the tool, not the coverage.
- **Zero `claimed` functions in `examples/`**, so the soundness half of the gate is currently
  vacuous and says so. That is a *consequence* of R-0461/R-0464 having closed H23/H24, not a
  defect, but it means a future clean result only means something because of the separate
  mechanism assertion.

### From R-0460 (Register A) — 0 of 5 fully discharged, 4 of 5 half

- **`callSiteObligations` is the remaining row and is different in kind:** its content is
  substitution correctness — that the report layer's syntactic substitution of actuals for
  formals means what the interpreter's binding of formals to values means. That is a bridge
  property (H19), not a short proof.
- **Every discharged row is only half done.** The *semantics* half is proved and proved tight;
  the *lowering* half — that the emitted obligation denotes the condition — is H19 and no
  theorem in the repo touches it. A half-discharged row is still a TRUSTED row.

### From the gate-control sweep — 18 of 55 soundness gates

- **37 soundness gates have no negative control.** Run
  `check_gate_mutation_coverage.sh --coverage` for the list; do NOT grep the harness for gate
  names, which counts prose and inflated the figure to 20.
- **8 of the 37 are migration-shaped** (`check_*_migration.sh`): they assert a refactor
  preserved behaviour and largely re-test rules the family gates already control. Deprioritise
  explicitly rather than padding the count.
- **~10 are genuinely distinct** and worth doing: `check_axiom_inventory`,
  `check_bv_certificates`, `check_smt_redteam`, `check_smt_replay`, `check_evidence_corpus`,
  `check_contract_negatives`, `check_codegen_differential`, `check_codegen_execution`,
  `check_differential_positions`, `check_subject_facts`.
- **`check_totality_judgment.sh`'s stated scope is wrong.** Its header names `blockDiverges`
  among the facts it covers; no fixture in it exercises divergence detection. The rule IS
  guarded — with divergence detection disabled, `make test` goes 1702/0 → 1315/76 — but by
  the main suite, not by that gate. Fixing the gate means giving it a divergence fixture.
- **Measured rate, for planning:** a family touching `EmitSSA`, `Lower` or `ProofCore`
  rebuilds most of the compiler and then runs a gate that compiles and executes programs —
  10–25 min each, and roughly a third of mutations need rework before they test their stated
  subject. The remaining 37 are 8–15 hours of machine time.

### Environmental, not ours

- **`lli` 21.1.8 cannot JIT this compiler's IR** (`orc_rt_alt_UnwindInfoManager_register` not
  in its bootstrap symbols map). Verified against `origin/main` in a clean worktree:
  byte-identical `.ll`, identical failure. `run_tests.sh` now probes `lli` for FUNCTION and
  falls back to native clang, which turned 49 silently-skipped tests back into real coverage
  (1653/49 → 1702/0 on an affected machine).

## Compiler Pipeline Spine

> **[historical] Reconciled 2026-10-19:** its QBE-first ordering is superseded by the current queue, R-0511/R-0514/R-0515 and R-0517 (the Lean Compiler Backend is the QBE path). Its live remainders are mapped to owning tasks in the
> [reconciliation record](../project/ROADMAP_RECONCILIATION.md); nothing in it schedules work.

Phase 6B established semantic truth and validation records; Phase 6C made the
pipeline observable/replayable; Phase 7.5 adds a real user-selectable QBE
backend whose required duties include independent differential validation
without upgrading evidence claims; Phase 8 validates the bet with real
examples, seeds comparable generated-code performance measurements, and
performs the scheduled architecture migrations; Phase 8.5 turns
the shadow graph into a persistent incremental driver
only after that validation. Later proof/backend/tooling work must not bypass
this order: semantic facts first, observability second, independent executable
cross-checking third, external validation fourth, incremental reuse fifth, and
independently checked evidence after that. Phase 15.75 owns optimization and
performance claims only after backend contracts and measurements exist; Phase
17.5 owns release stewardship only after the first public release.

## External-Validation Gate

> **[historical] Reconciled 2026-10-19:** as a global blocker this is superseded by R-0515's continuing pilots; measured product decisions remain. Its live remainders are mapped to owning tasks in the
> [reconciliation record](../project/ROADMAP_RECONCILIATION.md); nothing in it schedules work.

The back half of the roadmap depends on the bet that evidence-carrying source is
worth the discipline. Phase 8 supplies the first serious external-facing
workloads; the external-validation gate decides whether the project proceeds
with Phase 8.5 and the larger Phase 11-19 investment as planned, narrows the
bet, or redesigns the discipline.

## Native Debug Info

> **[historical] Reconciled 2026-10-19:** the "Phase 15+ only" delay is superseded by R-0515's V1b checkpoint and R-0281. Its live remainders are mapped to owning tasks in the
> [reconciliation record](../project/ROADMAP_RECONCILIATION.md); nothing in it schedules work.

DWARF/source-mapped crash traces matter once Concrete emits real binaries. This
is backend/tooling work owned by Phase 15+, not Phase 7 stdlib work.

## Global Task Sequence

> **[historical] Reconciled 2026-10-19:** this competing R-0004-first sequence is superseded by the current execution queue; its task obligations remain with their owners. Its live remainders are mapped to owning tasks in the
> [reconciliation record](../project/ROADMAP_RECONCILIATION.md); nothing in it schedules work.

Three implementation audits on 2026-07-18 reproduced nine numbered defects in
previously dark stdlib trusted bodies, Mono/Elab/SSACleanup boundaries, and the
reducer, plus a four-part proof-freshness class. The unbound-proof containment,
R-0005 trap inventory, and R-0442 callable-identity foundation have landed.
R-0004 is now the frontier: every later proof multiplier depends on its subject
digests, typed dependency roots, and replay receipts meaning what they claim.

The immediate sequence is normative:

1. R-0004 completes evidence integrity.
2. R-0450 may land the common obligation IR immediately afterward only if its
   existing implementation is merge-ready and does not mint authoritative
   evidence; otherwise it yields rather than blocking the defect queue.
3. R-0006, R-0007, R-0008, and R-0434 close the remaining structural
   destruction, semantic-symbol/import identity, and aggregate-layout
   verification defects.
4. R-0009, R-0010, and R-0439 make reduction, bug-corpus truth, and unlanded
   work mechanically honest.
5. R-0435 performs the ratified no-shadowing language migration only after the
   correctness queue above.
6. R-0440 supplies the evidence dimensions required by R-0448; R-0448 then
   graduates multi-kernel evidence only after R-0004 and R-0450 satisfy its
   merge bar.
7. The existing sequence resumes with R-0437, R-0447, and the remaining
   workload-pulled tasks.

This preserves one narrow exception for already-active semantic-unification
work without letting proof-product expansion repeatedly displace known defects.
Proof automation remains behind honest semantics and the external-user trial.

## Strict post-R-0004 critical path

> **[historical] Reconciled 2026-10-19:** this older sequence is superseded by the current execution queue. Its live remainders are mapped to owning tasks in the
> [reconciliation record](../project/ROADMAP_RECONCILIATION.md); nothing in it schedules work.


post-closure mutation qualification
→ R-0208 toolchain/revocation fire drill
→ R-0482 five-way identity separation and atomic migration
→ compiler-provenance stabilization
→ canonical production `VerificationTask`
→ R-0353 independent consumer and schema conformance
→ receipt v1 freeze
→ typed contracts and exact contract identity
→ `old`/`modifies`/frames and ghost state
→ narrow mutable-state extraction and ninth-table conversion
→ `#[decreases]` and the spec standard library
→ external-user workflow, flagships and ergonomics
→ broader package-evidence composition and typed policy.

## Historical record and detailed slice traceability

The remaining material in R-0004 preserves investigations, rejected hypotheses, corrections,
commit-level measurements and the original eight-slice plan. It is intentionally not the current
execution dashboard. When historical prose conflicts with the canonical map above, update or
archive the prose; never create another competing "authoritative current state" block.

> **[historical] The following Package 3 sequencing and risk estimates were written before the
> Concrete-owned task/attempt/session/receipt/policy/cache model and the current replay-minting seam
> were ratified. They are preserved for context and do not describe current work.** Current status,
> ordering and closure are owned only by the Package 3 block above.

> **HEADLINE STATUS — 2026-08-09 (`902ab601`, audited against code and executable gates). This
> is a historical snapshot and is superseded by the canonical trust-boundary map above.**
> Prose further down was written while the work was in flight and is kept for its reasoning, not
> its status. In particular, wording describing the subject digest as "still V1-frozen" is about
> the AUTHORITATIVE comparison — which is deliberately unflipped until step 7 — and must not be
> read as "the V2 digest does not exist". It does.
>
> **Historical estimate only: roughly 70–75% of the architecture was implemented at this
> checkpoint, while only 3 of 8 slices were formally closed.** The distinction is
> load-bearing: Concrete can compute the intended subject, dependency material, receipt helper
> values and exact migration inventory, but production `proved` still uses the legacy
> fingerprint/spec path. Producing correct material is not the same as making every friendly
> verdict depend on it.
>
> | slice | state |
> |---|---|
> | 1-3 | **complete** |
> | 4 | environment identities DONE (2026-08-09); 8 of 9 proof tables migrated, ninth externally blocked. **Receipt status, qualified:** the receipt core can mint structurally valid envelopes from supplied facts; **no production replay-backed receipt is issued or authoritative** |
> | 5 | **subject foundation complete and V2 refrozen** — semantic identity, typed signature, generics/capabilities, contracts, structural V2 body, selected specification and claim scope are bound; semantic edits move it, alpha-renaming and declaration-order changes do not. Exact compiler-produced migration inventory is **CLOSED: 44/44 links, 0 unowned, 0 links without subjects, 0 nondeterministic keys, 33 distinct callable owners**. V2 remains shadow-only until Slice 7 replays and migrates the corpus |
prerequisite that failed — cross-compilation determinism — now holds, so `v2:` is the first
ACCEPTED semantic subject format rather than a number spent on a shadow draft.

*Revocation history, kept because the reason matters more than the outcome:* the freeze declared
earlier that day was revoked when the same callable proved to digest differently in two projects.
No V2 digest had become authoritative, persisted, or receipt-bound, so this was failed
pre-release acceptance rather than migration from a published schema — which is why the encoding
was repaired in place instead of minting a V3 that would have become the first real format merely
because V2 briefly existed in shadow.

**Root cause and repair.** `elabFn` did not reset the binder frame per function, so the lexical
scope ACCUMULATED across declarations in a module and a parameter's resolved index depended on
what was elaborated before it. `calls.inc(x)` — one parameter — encoded as `r3:0.0` in
`proof_patterns/composition` and `r3:0.1` in `composition_trusted_helper`, from identical source.
`x` cannot be at index 1 of its own parameter frame.

Found by printing the canonical BYTES rather than their hash (`--report body-bytes`, added for
this and kept): a hash says only THAT two things differ, and the bytes said WHERE, in one
character. Three prior hypotheses — a trust leak, the operator encoding, and the serializers —
were each eliminated by inspection first; the serializer elimination mattered most, because
"normalize identities in the encoder" was the obvious fix and would have moved bytes while
leaving the producer defect underneath.

The reset lives at the START of `elabFn` rather than as teardown after each function: an early
return or error path skips a teardown, and a frame outliving its owner is the entire defect class.

*(An interim note here read "schema shape frozen, values not — subject production is not yet
canonical". That was true for part of 2026-08-09 and is now superseded: production IS canonical
and V2 is refrozen. Removed rather than left standing, because a stale caveat about
non-determinism is exactly the sentence someone would quote to justify not trusting the digest.)*

**SCHEMA FROZEN** (2026-08-09): `check_subject_facts.sh` pins the digest of a fixed input, so any change to the composition fails the gate. The correct response is never to update the constant — it is to bump `subjectV2:` to `subjectV3:`, so stored values read as a DIFFERENT SCHEMA (`needs_recheck`) rather than comparing unequal and reading as `stale`. Verified by mutation: adding a component moved the value and the gate fired. Slice 5's subject work is COMPLETE; what remains in the slice is the migration, which is step 7 |
> | 6 | **CLOSED 2026-08-16** — one ProofCore producer feeds ProofCore/report dependency-root measurement, both keyed on `DefinitionIdentity`. Roots are CONSUMED by production verdicts through the composed authority pass, with invariant `PROVED-ROOTS` asserting no `proved` subject fails to root. Measured: 13 subject roots refuse, none of them proved — the conjunct is consumed and currently redundant, and the redundancy is pinned. (This row read "active / shadow, 62/64 subjects root" until 2026-08-16; that figure predates the identity migration, the fixture-package split and the authority pass.) The closed correspondence join and all six refusal classes are production-fed; external table evidence, trust propagation, production-consumer mutations and verdict integration are closed. |
> | 7 | ~~receipt issuance and corpus migration **not started**~~ — HISTORICAL. Issuance, storage and consumption shipped 2026-08-16; V2 migration and activation 2026-08-17 |
> | 8 | adversarial validation of the authoritative pipeline — **shipped 24/0 after the independent non-author packet**; R-0004 subsequently closed |
>
> **The remaining work**, in critical-path order:
>
> 1. **Finish slice 4** — ~~never-under-approximate dependency access~~ **DONE 2026-08-08**
>    (`tableValueDigest`, whole-by-construction, fail-closed); the validated receipt CORE TYPE is
>    **DONE** (`Concrete/Proof/Receipt.lean`: private constructor, no `Inhabited`, correspondence,
>    duplicate and empty-value refusals, normalized bindings, edge-aware currency and one typed
>    schema-first disposition). Do not shorten this to "the receipt is done": no
>    production consumer, serialization, replay result or complete envelope exists. REMAINS:
>    *produce* the deterministic workspace,
>    imports and toolchain identities — the envelope requires them and refuses empty ones, but
>    **DONE 2026-08-09** (`toolchainIdOf`, `workspaceIdOf`, `importsIdOf` in
>    `Concrete/Proof/Receipt.lean`, 5 gate legs); and migrate the ninth proof table (blocked by mutable-borrow
>    extraction in ProofCore, outside R-0004).
>
>    **Receipt fields still unbound**, listed so the envelope's coverage is not overstated: the
>    deterministic dependency ROOT (slice 6), theorem identity and theorem-artifact digest, the
>    replay command/protocol and its result (slice 7 — a receipt minted without a replay is a
>    claim about a proof nobody ran), and trust/assumption qualification. The envelope binds
>    schema, subject, edge kind, and canonically-ordered table identities+digests today.
>
>    **Ordering is NORMALIZED and says so** (sorted by table identity in `mint?`). Traversal
>    order is deterministic but not meaningful, and a receipt comparing unequal because a walker
>    visited two constants differently would report drift that did not happen. Sorting hides
>    order and NOT a swap — exchanging two tables' digests changes which name carries which
>    value, gated in both directions.
> 2. **[done 2026-09-02] Clean the closed-manifest gate before building on it.**
>    `check_migration_manifest.sh` now has one equality-based closure section. The obsolete duplicate
>    35-row ratchet was removed, its subject parser follows the live V2 report, and both migration
>    gates consume the shared 43-link population pin. The registered gate is 12/0: 43/43 links,
>    52/52 subjects accounted, 0 unowned, 0 without subjects and 0 nondeterministic keys.
> 3. **Integrate slice 6** — feed the typed `DepNode` graph into the deterministic dependency-root
>    builder; combine compiler-produced edges with Lean-side theorem classification; handle direct,
>    transitive and recursive dependencies; bind dynamic table access to the WHOLE table rather
>    than a guessed subset; make ProofCore and Report consume the root; propagate trust and
>    assumptions into the final evidence status. **Highest-risk remaining step. Slice 6 is NOT
>    complete merely because `DependencyRoot` computes a root:** it exits only when ProofCore,
>    Report and status composition consume validated roots and no old name-keyed/advisory path can
>    emit `proved`.
> 4. **Complete the receipt and production environment binding.** The helper type still lacks the
>    dependency root, theorem/artifact identity, replay protocol/result, trust/assumption
>    qualification, canonical serialization, validated decoding, storage and a production status
>    consumer. `toolchainIdOf`, `workspaceIdOf` and `importsIdOf` exist, but production must derive
>    their inputs from the actual compiler, workspace/module closure, imported content and Lean
>    toolchain rather than accepting fixture strings. Register every weakening in the central
>    mutation harness and require the production consumer—not a helper probe—to kill it.
> 5. **Complete slice 7** — replay every repository proof; issue receipts only after successful
>    kernel replay; produce a dry-run migration manifest for every stored proof link; make V2
>    fingerprints authoritative; convert old schemas to `needs_recheck`; publish the first
>    eligibility-denominated proof-coverage baseline; verify clean-machine and
>    repository-root/project-root reproducibility.
> 6. **Complete the ninth table** after mutable-borrow ProofCore extraction lands: extract
>    `decode_header`, generate `proofFnsExt`'s remaining three entries, and replace its tripwire
>    with positive generated-evaluation/kernel-replay correspondence.
> 7. **Complete reproducibility and coverage** — repository-root/project-root and clean-machine
>    replay parity; deterministic roots/receipts; cache off/on/corrupt-cache honesty; and the first
>    eligibility-denominated coverage baseline with stable discharge routes and blockers.
> 8. **Complete slice 8** — red-team the migrated production path with independent hostile
>    fixtures, implementation mutations, and an end-to-end adversarial project. The attack goal
>    is to obtain a friendly `proved` verdict from evidence that is stale, incomplete,
>    context-dependent, misbound, unreplayed, dependency-incomplete, trust-widened, schema-confused,
>    or copied from another project. Every successful attack becomes a permanent regression;
>    every weakening mutation must be killed by the production consumer, not a helper probe.
>
> **Migration denominator — one number, one owner.** The executable
> `check_v1_fingerprint_golden.sh` inventory is authoritative today: **44 stored legacy links over
> 77 extracted functions in the eight migration examples**. A raw repository grep currently sees
> 67 annotations because it includes stdlib, adversarial/negative fixtures, tests and a drifted
> source copy. The earlier headline number 53 has no executable denominator and is retired. If
> the migration corpus changes, change the generated manifest and its gate in the same commit;
> never update prose alone.
>
> **Receipt graduation requirements.** The current type binds schema, subject, edge kind and
> normalized table identity/digest pairs plus caller-supplied environment strings, and exposes one
> edge-aware, schema-first typed disposition. The final
> receipt additionally binds the deterministic dependency root, theorem identity and theorem
> artifact digest, replay command/protocol and successful result, trust/assumption qualification,
> and versioned canonical serialization with validated decoding. Produce toolchain/workspace/import
> identities rather than accepting display paths. The production consumer must use the one typed
> disposition (`current` / `needs_recheck` / `not_current`) rather than reconstructing its own
> Boolean sequence.
>
> **Receipt mutation bar.** Behavioural probes are not enough. Register mutations that re-open
> the constructor or `Inhabited`, drop empty-subject or table-correspondence checks, duplicate or
> omit a binding, ignore edge kind or dependency root, drop each environment identity, accept a
> receipt without successful replay, or skip schema comparison. Each must be killed by the
> production consumer gate, not only by a helper-level probe.
>
> **Estimate, stated so nobody plans against a wrong one.** To "architecture implemented in
> shadow": about 2 major packages. To "authoritative and honestly closed": about 4, plus extensive
> mutation gates, a dedicated adversarial-validation package, plus the mutable-borrow prerequisite.
> In calendar terms this was estimated as several focused weeks, not days — slice 6,
> receipt/corpus migration, and the final hostile pass dominated. **At the time, closure was held
> pending mutable-borrow extraction or Slice 8**, by the decision recorded below. The ninth table was
> later made explicitly fail-closed and removed from the closure list; Slice 8 shipped before closure.
>
> No decision is outstanding. Both owed decisions were made 2026-08-08 (struct-literal evaluation
> order → source order, implemented; slice 7 → hold open). The `MetaM` constraint in steps 4 and 6
> is the only remaining part that is not ordinary compiler work: `classifyTheorem` reads a
> theorem's elaborated type and lives where Lean is elaborating, so the compiler can honestly
> produce `trusted`/`missing`/`unclassified` and the contract-vs-body split has to come from the
> Lean side. That needs a design for the hand-back, not more threading.

> **Audit instrumentation note.** The hostile receipt gate's ``"`default` ..."`` label executed
> shell command substitution and printed `default: command not found`. The companion fix quotes the
> label safely and widens `check_gate_vacuity.sh` from `echo`-only matching to every double-quoted
> shell string, ratcheting the 17 pre-existing files. Still required: enroll receipt changes in the
> mutation harness; the dependency/receipt result contains behavioural controls, not registered
> production-consumer weakening mutations.

Treat the following as one evidence-integrity defect class:

- a source `#[proof_by]` without `#[proof_fingerprint]` currently compares a
  synthesized current fingerprint with itself and can remain proved forever
  (bug 058);
- `bodyFingerprint` hashes statements but omits parameters, return type,
  generics/bounds, capabilities, and other signature facts (bug 059);
- `#[requires]`/`#[ensures]` and selected spec/proof identity are outside the
  body hash (bug 060); and
- dependency review does not affect the caller's verdict at any distance:
  `staleDeps` is filled only after statuses are derived, is never consulted to
  downgrade the caller, and is not propagated transitively. Check in the
  reported multi-function witness and control (provisionally called bug 062);
  the number becomes stable only when its document and executable control land.

Land this task in eight explicit slices. **This is the original slice traceability
table, not the current execution dashboard; the closure record and current execution state at the
top of the roadmap own current status.** `ACTIVE` means useful implementation
has landed while the slice's user-visible outcome remains incomplete.

| slice | outcome | current state | what remains before the slice exits |
| --- | --- | --- | --- |
| 1 | executable witnesses | **LANDED** | Preserve the positive controls and convert each defect tripwire when its owning slice closes. |
| 2 | missing-fingerprint containment | **LANDED** | Preserve the fail-closed controls through the V2 migration. |
| 3 | dependency containment | **LANDED** | Preserve the conservative downgrade and its controls through replay-backed V2 activation. |
| 4 | replay/table foundation and receipt-envelope plumbing | **CLOSED for R-0004** — typed replay, replay-only minting, storage and production consumption shipped; the ninth table remains fail-closed follow-up | Preserve the shipped authority path; convert the ninth table only after the narrow mutable-state model exists. |
| 5 | complete semantic `ProofSubjectDigest` | **CLOSED 2026-08-17 — V2 ACTIVATED, not shadow.** The live freshness verdict compares the V2 subject digest; 44 stored fingerprints = 43 plan rows + 1 unreachable, and the corpus census is unchanged before and after | Activation is done. `make test-v2-migration-plan` (11/0) now asserts COMPLETION and is the regression leg if a v1 value reappears |
| 6 | deterministic transitive dependency material/root | **CLOSED 2026-08-16 — the correspondence join is keyed on `DefinitionIdentity`, fed from the real corpus and CONSUMED by production verdicts; roots are consumed with `PROVED-ROOTS`; trust propagates transitively and is disclosed at every status** | Preserve the production controls and pinned root redundancy through receipt activation; no old name-keyed/advisory route may return. |
| 7 | receipt issuance, honest corpus migration, and coverage baseline | **CLOSED for R-0004** — production minting/consumption, V2 activation, clean-checkout replay and the 35/91 baseline shipped | Preserve the production controls; cache tests remain a future `ProofCache` graduation condition. |
| 8 | adversarial validation of the authoritative evidence path | **CLOSED 24/0** — permanent attacks plus the independent theorem-retargeting packet shipped | Every future evidence-schema change must continue to run the permanent hostile suite. |

**[historical] Completion estimate as of 2026-08-09.** Roughly 70–75% of the architecture existed, but only
slices 1–3 are formally closed and the authoritative evidence path is nearer 50–60%. Remaining
work is integration-heavy: clean the duplicated manifest gate, integrate dependency roots,
complete and consume replay receipts, migrate the corpus, finish the externally blocked ninth
table, prove reproducibility/publish coverage, then adversarially validate the authoritative
result. Slice 6, receipt/corpus migration and Slice 8 are the highest-risk work. This is several focused weeks of work rather than a final
cleanup pass, and the external mutable-borrow extraction prerequisite prevents
formal closure even if slices 4–6 otherwise finish.

**[historical] Former critical path as of 2026-08-09.** Remove the duplicate manifest-gate block → integrate deterministic dependency
roots in shadow → make a valid root mandatory in production status composition and kill the
omission mutation → complete/serialize/decode the receipt and derive real environment facts →
replay and migrate all 44 links → land mutable-borrow extraction and migrate the ninth table →
publish reproducibility/coverage → execute Slice 8. Do not
turn any shadow fact into a friendly authoritative verdict out of order.

## Binder frames: position and meaning are separate obligations

A contract may name binders the value environment never holds. `ghost let` is the
case in the corpus: proof-only, erased before Core, referenced only from contracts.
Ghost binders therefore form their OWN frame, collected from the AST in source
order and rendered `h:<i>`.

Two properties must hold at once, and each is repository-gated in
`scripts/tests/check_subject_coverage.sh`:

- Renaming a ghost binder must NOT move the subject digest. Otherwise `h:<i>` is a
  source name with extra steps.
- Changing a ghost's INITIALIZER MUST move it. `#[invariant(i <= bound)]` with
  `ghost let bound = 8` is a different claim than with `= 9`. Encoding only the
  position made those byte-identical — measured, then closed by encoding each
  initializer as `h<i>=<expr>` in the scope of the params and the ghosts before it.

**Source order, not elaboration order.** Raw `env.vars` positions shift across
branch and loop elaboration, so an index derived from them would move a digest when
nothing semantic changed. The AST's order is fixed by the program text.

**The loop-invariant asymmetry, recorded because it cost a wrong first fix.**
Threading the ghost frame through `requires`/`ensures` alone changes nothing. Ghosts
are named from LOOP INVARIANTS — that is the purpose of `ghost let`, to snapshot a
bound so the invariant can refer to it — and the loop encoder reconstructs its
binder list from ASSIGNMENTS. That approximation finds an induction variable and
structurally cannot find a ghost, which is exactly why `plain`'s `i <= 4` was
covered while `with_ghost`'s `i <= n` was not. Any future binder frame must be
threaded into the loop encoder too, not only the contract encoder.

This table is the only top-level progress numbering for R-0004. Slice 4 has a
normative internal build order because its representation migration must be
incremental; that internal order does not duplicate slices 5-7. `ACTIVE` means
implementation has landed but the slice's user-visible outcome is not complete;
it must not be reported as done from the presence of its intermediate data.

1. **Executable witnesses first.** This slice has landed:
   `scripts/tests/check_proof_freshness.sh` drives copies of the real
   `examples/loop_invariant` and `examples/crypto_verify` projects — real proof
   links with real stored fingerprints, because a synthetic fixture with a
   hand-written fingerprint would prove only that string comparison works. Bug
   058's containment is asserted positively; 059, 060 and both halves of 062 are
   TRIPWIRE legs that pass because the defect is present and FAIL when it is
   fixed, which is the signal to convert them and close the document. Every gap
   leg is paired with a control, so "still proved" cannot be an artifact of the
   harness never re-reading the file, and `edit` reports fixture drift as a
   failure rather than a silent no-op. Bug 062's number is now stable. Bug 061
   was a separate ProofCore identity/model defect, completed immediately before
   this task by R-0442; do not recreate its old ambiguous call representation
   inside the freshness implementation.
2. **Immediate containment.** An in-source proof link without a stored,
   validated proof-subject digest is `missing`/`unbound` (or
   `needs_recheck`), never `proved`; release/verified profiles fail closed.
   This containment has landed; preserve its executable controls.
3. **Dependency containment.** This slice has landed. A reachable dependency
   that is not current downgrades its dependent to the new
   `ObligationStatus.depsNotCurrent`, at one hop and transitively, closing bug
   062. Which statuses count is decided in one place
   (`ObligationStatus.isCurrentForDependents`: `proved` and `trusted` are
   current, everything else is not); `verified`/`release` fail closed on it and
   the ledger gives it its own kind rather than `proved_by_lean`. The closure is
   a worklist bounded by the node count, so recursion terminates rather than
   diverging, and one pass is a fixpoint because reachability is transitive.
   `staleDeps` was renamed `notCurrentDeps` — its meaning widened past its name.
   Measured effect: only `examples/parse_validate` changes, where two claims
   reach `missing` callees. Slice 6 replaces this conservative closure with the
   deterministic typed dependency material; this containment does not wait for
   it.
4. **Replay foundation and receipt envelope.** The envelope must carry TYPED
   DEPENDENCY EDGES, derived from what the theorem actually uses — not a
   manually selected mode, and not merely the call graph:

   | edge | the caller relies on | invalidated by |
   | --- | --- | --- |
   | `contract` | the callee's proved contract | that contract, or the callee's proof receipt, changing |
   | `body` | the exact callee implementation | the callee's body / type / semantic digest changing |
   | `trusted` | a declared trust boundary | the boundary changing; and the trust PROPAGATES |
   | `missing` | nothing validated | always: the caller is `depsNotCurrent` |

   Two honest proof styles then fall out of the edges rather than being chosen:
   a **modular** proof depends on callee *contracts*, so an implementation change
   that preserves the contract does not stale its callers; a **closed-subject**
   proof binds exact transitive *bodies*, so its helpers need no individual proof
   links but any relevant body change stales the caller. Deriving the edge kind
   from the theorem is what makes the distinction honest — a mode flag would let
   an author assert a relationship the proof does not have.

   Cycles are represented through versioned canonical transitive dependency
   material (slice 6), not by special-casing recursion at each consumer. The
   current design hashes the validated reachability closure as a set. It
   deliberately does not claim Merkle-DAG incremental sharing; revisit that
   representation only if corpus scale makes the simpler witness expensive.

   **Ownership.** The edge vocabulary and the theorem-expression metaprogram
   belong under Proof extraction, NOT in ProofKit, examples, reports or the CLI:

   ```
   Concrete/Proof/Core/DependencyEdge.lean       -- typed-edge vocabulary
   Concrete/Proof/Extract/DependencyEdges.lean   -- theorem-expression metaprogram
   Concrete/Proof/Receipt/                       -- serializes extracted edges
   Concrete/Proof/Replay/                        -- revalidates them
   ```

   If that directory split is premature (it depends on R-0114-R-0118, per
   [[repo-reorganization-plan]]), land it as a flat `Concrete/Proof` module with
   this declared future owner recorded in the module header.

   **Build order (normative).** `PFnDef` carries `name : String` and no semantic
   identity, and all nine FnTables in the proof corpus are hand-written.
   Enforcing `body`-edge classification before generated, ID-carrying tables
   exist would classify essentially every table-naming theorem as `missing` and
   contain the whole corpus — far larger than slice 3's two claims, and the
   pressure would then be to weaken the rule rather than finish the migration.

   1. **`CallableId` in the compiler's semantic-identity layer.** LANDED, in
      `Concrete/Resolve/CallableId.lean` — the resolve layer, not Proof, because
      the proof machinery is a CONSUMER of identity and an identity minted by its
      consumer is that consumer's opinion. Explicit `user`/`builtin`/`intrinsic`/
      `extern` namespaces; `defModule`/`declName` from the DEFINITION site so an
      imported alias preserves identity; `typeArgs` so specializations differ;
      schema-versioned rendering. No `String → CallableId` parser exists, by
      design — a parser is what would let a consumer rebuild identity from text.
      Carries no span, path, alias or binder field, so alpha-renaming cannot move
      it. Gated by `check_callable_identity.sh` (33 checks, one per acceptance
      criterion).

      This needed its own total `tyCanonical`: `Resolve.Shared.tyName` renders
      `.generic n args` as just `n` and answers `""` for refs, arrays, pointers
      and fn types, so reusing it would have collapsed exactly the
      monomorphization distinctions the identity exists to make. Capability sets
      go through `CapSet.normalize`, so union order cannot produce two identities
      for one callable.
   2. **Identity on `PFnDef`** — LANDED, as a TYPED distinction rather than an
      `Option` + `isSome` test. `PFnIdentity` is `legacy | semantic id`, and the
      evidence path takes `IdentifiedPFnDef` (which has a `CallableId`, not an
      optional one), reachable only through `PFnDef.identified?`. With an
      Option, every consumer wanting to mint evidence has to remember to check,
      and forgetting silently upgrades a legacy entry; here passing a legacy
      entry to the receipt path is UNREPRESENTABLE. `name` became `displayName`
      and is excluded from identity and from every digest.

      Measured while doing it: `PFnDef.name` is WRITE-ONLY — set at 26
      construction sites and projected at none. It never identified anything;
      lookup goes through the table's own string key, which is keyed identity,
      not semantic identity, and is exactly what step 3 removes. The field stays
      only so existing literals elaborate during the migration.

      Sixteen HMAC table entries were built POSITIONALLY (`⟨"rotr", […], expr⟩`)
      and broke on the new field — the same position-as-identity hazard. All
      converted to named fields and gated.
   3. **Canonical finite tables.** The TYPE has landed; the nine tables migrate
      in step 5. `FnTable` gained `schemaVersion : Nat` and
      `entries : Array PFnDef`, alongside the legacy `globals` function so
      migration is incremental rather than a flag day — adding them broke no
      existing table or proof, since both default.

      A `String → Option PFnDef` cannot be evidence: a Lean function cannot be
      enumerated, hashed or ordered, so there is nothing to digest. That is why
      this is not an ergonomic refactor — the finite table is what makes a root
      exist. Empty `entries` marks a legacy table: it evaluates, and it has no
      root, so it mints nothing.

      Shipped with the properties the root depends on, each gated:
      canonical ordering by rendered `CallableId` (insertion order cannot change
      a root); duplicate identities are an INTEGRITY ERROR rather than
      last-writer-wins (silently picking one would make the root depend on
      insertion order); one unidentified entry disqualifies the whole table
      (evidence needs identity for all of it, not most); the schema version is
      inside the root; and `lookupById` resolves by identity, never by name or
      position — a same-named entry in another module is not found.

      **Calls still select by an operational STRING key.** `PExpr.call "f"`
      does not make that key semantic identity. The evidence-bearing entry
      therefore separates three facts: `callableId` is semantic identity,
      `operationalKey` is the key used by evaluation, and `displayName` is
      diagnostic rendering only. A generated Lean declaration symbol is an
      emitter detail and is none of those three. `FnTable.keyIndex` binds the
      exact `operationalKey -> CallableId` mapping INTO the root. Ambiguous or
      unresolved operational keys make `isEvidenceBearing = false`; omitting a
      generated assertion or dispatch arm is not fail-closed if the typed table
      can still return a root.

      The root is LENGTH-PREFIXED and tagged, not delimiter-joined: `a;b` and
      `a` + `;b` are different entry lists that a plain join renders identically,
      which would let two distinct tables share a root.

      Step 4 supplies the per-entry scoped lookup lemmas that preserve proof
      behaviour. Every proof currently does
      `simp [XFns_globals, XFnsGlobals, …]`, relying on a projection lemma plus
      the equation lemmas of a pattern-matching function. Under `Array` those
      equations do not exist, so the lemmas must be GENERATED with the table
      and tagged `@[proofTable]`, not hand-written or registered globally with
      `@[simp]`. The bar is the same KERNEL THEOREM RESULTS, not identical proof
      terms or delta-unfolding behaviour.
   4. **Generate tables from compiler facts** — LANDED at `d2dd1eb3`, with the
      unsupported-specialization boundary below. Resolved `CallableId`s are
      carried through `ProofCoreEntry` and `ExtractionEntry`; neither generator
      splits `qualName` or reconstructs identity from rendered text. Both current
      emission surfaces (`--report lean-stubs` and `prove --emit-lean`)
      participate in one generated surface inventory, so a third cannot appear
      without joining the matrix.

      The target evidence-bearing entry is explicit:

      ```text
      callableId          semantic identity from resolved/checked/mono facts
      operationalKey      exact key used by PExpr.call and evaluation
      displayName         diagnostics only; excluded from semantic identity
      params              canonical parameter names/order used by ProofCore
      body                canonical PExpr over the complete constructor set
      sourceBodyDigestV1  body_only; receipt_eligible = false
      ```

      `sourceBodyDigestV1` is a migration-comparison fact, not the final subject
      digest. Its schema and `body_only` scope are inside the table root, and no
      code path may promote it into a receipt. Specializations receive
      `CallableId.typeArgs` from Mono facts; type erasure or reconstruction from
      a display name is an integrity failure.

      - ~~provenance from a report entry~~ FIXED: `ProofCoreEntry.callableId` is
        minted where the resolved module path and the checked declaration name are
        FACTS, and carried through `ExtractionEntry` to the generator, which now
        reads it. The generator no longer splits `qualName` — that had re-derived
        identity from a rendering, one layer away from the facts. Sorting uses the
        same carried `render`, so emission order and asserted order come from one
        function. Specializations still need `typeArgs` populated from Mono;
      - ~~no source-subject digest per entry~~ FIXED: both generators emit
        `sourceBodyDigestV1` per entry, computed as
        `shortHash (pexprCanonical body)` — the truncated SHA-256 the in-source
        `#[proof_fingerprint]` already uses, over a length-prefixed tagged
        encoding, so the digest inherits that injectivity. Deliberately NOT the
        legacy `bodyFingerprint`: that hashes Core statements and is the
        proof-freshness fingerprint bugs 058-060 are filed against, so reusing its
        VALUE would put one string in two roles; a gate leg asserts the two never
        coincide. `scope = body_only` and `receipt_eligible = false` are kept, and
        both are inside the root, so a body-only digest cannot collide with a
        future complete digest of the same body. It catches a generated table
        whose body literal drifted from what the compiler extracts (hand-edited or
        stale file), and drift moves the root. It does NOT catch a consistent
        generator bug — a wrong body plus a digest over that same wrong body
        agrees with itself — which is why it is a step-5 comparison key and not
        evidence;
      - ~~generated artifacts still go to stdout/caller-chosen paths~~ RESOLVED,
        with the naming decided rather than assumed. The canonical home for
        generated proof artifacts written to disk is `.build/prove/`, which
        already exists (`prove --emit-artifacts`, `--workspace`) and is
        gitignored; a second `.build/proofs/` was the obvious move and is the
        wrong one, because two homes for one concept is the drift this tree keeps
        paying for. Report surfaces keep printing to STDOUT: `--report lean-stubs`
        is a report, 15 call sites read it there, and a report that silently
        writes a file is a surprise — not a default worth breaking them for. The
        risk actually worth gating is different and arrives with step 5: the nine
        tracked `Proofs.lean` files are HAND-WRITTEN today and hold the nine
        hand-written tables, so once those become generated, a generated artifact
        committed as though it were source erases the distinction between what a
        human asserted and what a tool emitted — which every provenance claim
        rests on. Gated: no tracked file declares the generated namespace or is an
        emit-lean stub (anchored to real declarations, so the generator's own
        string literals do not match);
      - ~~globally-polluting simp lemmas~~ FIXED: generated lemmas are tagged
        `@[proofTable]`, a scoped set declared in `Concrete/Proof/SimpAttr.lean`
        (its own module because `register_simp_attr` needs `import Lean` and must
        be imported by its users). Proofs opt in with `simp [proofTable]`. Gated
        both ways: the scoped tag must be present AND no generated lemma may
        carry `@[simp]`;
      - lemma names are `<entry>` -based and not collision-proof;
      - "exactly one lemma per entry" and "identical inputs produce
        byte-identical modules and roots" are not asserted;
      - builtins/intrinsics/externs/specializations are representable, and the
        TYPE is now gated against conflating them (all four namespaces render
        distinctly for one `declName`; a builtin `len` and a user `len` are
        different identities; `specialize` cannot disagree with its base about
        namespace or defining module; a user and a builtin entry sharing a
        `declName` coexist when their operational keys differ, and are refused in
        non-canonical order). But NO GENERATED TABLE exercises them, and none can
        yet: nothing in the compiler mints `ofBuiltin`/`ofIntrinsic`/`ofExtern`/
        `specialize`, because extraction covers user functions only and callees
        are still selected by string. Entries for non-user callables arrive with
        callee identity, which is step 6/7. A TRIPWIRE leg asserts the absence of
        any producer, so it fails the moment one appears and this coverage stops
        being owed silently;
      - ~~no mutations yet for deleting an entry, swapping an ID/body, duplicating
        an ID/key, or emitting an incorrect lemma~~ FIXED: mutations cover
        deleting an entry (46), duplicating an ID (38) or a key (41), emitting an
        incorrect lemma (45), unbinding bodies from the root (43), accepting an
        incomplete identity (44), a body digest that does not depend on the body
        (47), and PERMUTING identities among entries (48).

        The swap is the instructive one. A permutation survives every cheap
        check: the SET of identities is unchanged, so "the expected identities are
        present" passes; both remain distinct, so duplicate detection passes; and
        the Id definition and the lookup lemma name the same symbol, so the file
        stays internally consistent and the kernel objects to nothing. Only
        CORRESPONDENCE catches it — the identity must sit on the body that belongs
        to it — so that check now exists, fixture-scoped over the three same-named
        functions whose bodies differ (+10/+20/+12), with a guard asserting those
        bodies stay distinct or the instrument silently stops working. This is the
        first concrete instance of the source-correspondence check step 4 owes in
        general, and the one thing `isEvidenceBearing := by decide` structurally
        cannot supply;
      - ~~generator coverage tests today's TWO emission surfaces by name~~
        FIXED: the number of `renderCallableId` references is pinned, so a third
        emission surface fails the gate until its author extends the coverage to
        it. Both surfaces are additionally driven over the committed same-name and
        many-instantiations fixtures, with determinism, path-independence and
        one-lemma-per-entry asserted, and positive controls so the "exactly one
        error" legs cannot pass vacuously.
      - ~~provenance from a report entry~~ FIXED: `ProofCoreEntry.callableId` is
        minted where the resolved module path and the checked declaration name are
        FACTS, and carried through `ExtractionEntry` to the generator, which now
        reads it. The generator no longer splits `qualName` — that had re-derived
        identity from a rendering, one layer away from the facts. Sorting uses the
        same carried `render`, so emission order and asserted order come from one
        function. Specializations still need `typeArgs` populated from Mono;
      - ~~no source-subject digest per entry~~ FIXED: both generators emit
        `sourceBodyDigestV1` per entry, computed as
        `shortHash (pexprCanonical body)` — the truncated SHA-256 the in-source
        `#[proof_fingerprint]` already uses, over a length-prefixed tagged
        encoding, so the digest inherits that injectivity. Deliberately NOT the
        legacy `bodyFingerprint`: that hashes Core statements and is the
        proof-freshness fingerprint bugs 058-060 are filed against, so reusing its
        VALUE would put one string in two roles; a gate leg asserts the two never
        coincide. `scope = body_only` and `receipt_eligible = false` are kept, and
        both are inside the root, so a body-only digest cannot collide with a
        future complete digest of the same body. It catches a generated table
        whose body literal drifted from what the compiler extracts (hand-edited or
        stale file), and drift moves the root. It does NOT catch a consistent
        generator bug — a wrong body plus a digest over that same wrong body
        agrees with itself — which is why it is a step-5 comparison key and not
        evidence;
      - generated artifacts still go to stdout/caller-chosen paths, not
        `.build/proofs/`;
      - ~~globally-polluting simp lemmas~~ FIXED: generated lemmas are tagged
        `@[proofTable]`, a scoped set declared in `Concrete/Proof/SimpAttr.lean`
        (its own module because `register_simp_attr` needs `import Lean` and must
        be imported by its users). Proofs opt in with `simp [proofTable]`. Gated
        both ways: the scoped tag must be present AND no generated lemma may
        carry `@[simp]`;
      - lemma names are `<entry>` -based and not collision-proof;
      - "exactly one lemma per entry" and "identical inputs produce
        byte-identical modules and roots" are not asserted;
      - builtins/intrinsics/externs/specializations are representable, and the
        TYPE is now gated against conflating them (all four namespaces render
        distinctly for one `declName`; a builtin `len` and a user `len` are
        different identities; `specialize` cannot disagree with its base about
        namespace or defining module; a user and a builtin entry sharing a
        `declName` coexist when their operational keys differ, and are refused in
        non-canonical order). But NO GENERATED TABLE exercises them, and none can
        yet: nothing in the compiler mints `ofBuiltin`/`ofIntrinsic`/`ofExtern`/
        `specialize`, because extraction covers user functions only and callees
        are still selected by string. Entries for non-user callables arrive with
        callee identity, which is step 6/7. A TRIPWIRE leg asserts the absence of
        any producer, so it fails the moment one appears and this coverage stops
        being owed silently;
      - mutations now cover deleting an entry (46), duplicating an ID (38) or a
        key (41), emitting an incorrect lemma (45), unbinding bodies from the root
        (43), accepting an incomplete identity (44) and a body digest that does
        not depend on the body (47). Still missing: SWAPPING an ID between two
        entries, which duplicate-detection does not catch because both identities
        remain present and distinct;
      - ~~generator coverage tests today's TWO emission surfaces by name~~
        FIXED: the number of `renderCallableId` references is pinned, so a third
        emission surface fails the gate until its author extends the coverage to
        it. Both surfaces are additionally driven over the committed same-name and
        many-instantiations fixtures, with determinism, path-independence and
        one-lemma-per-entry asserted, and positive controls so the "exactly one
        error" legs cannot pass vacuously.

      Both generators also emit `sourceBodyDigestV1 := shortHash
      (pexprCanonical body)`, using a length-prefixed tagged encoding rather
      than the incomplete legacy `bodyFingerprint`. Its `body_only` scope and
      `receipt_eligible = false` marker are inside the root. It detects a stale
      or hand-edited generated body but deliberately proves no more: a
      consistently wrong generator can emit the wrong body and a matching
      digest. This is a step-5 comparison key, never authoritative evidence.

      Generated lookup lemmas use the scoped `@[proofTable]` set, not global
      `@[simp]`. Determinism, path independence, one lemma per entry, the two
      current emission surfaces, same-name modules, many instantiations, and
      namespace distinctions have executable coverage. User functions are the
      only generated callable class today; builtin/intrinsic/extern entries and
      positive specialization identities remain blocked until later extraction
      and Mono facts supply them. A tripwire makes that unsupported boundary
      explicit. Mutations cover deletion (46), duplicate identity/key (38/41),
      an incorrect lemma (45), a root unbound from bodies (43), incomplete
      identity (44), a body digest independent of its body (47), and permuting
      distinct identities among entries (48). Independent identity/body
      correspondence legs catch the same permutation without relying on the
      table's internally consistent root. Generated artifacts still use
      stdout/caller-selected paths rather than R-0447's `.build/proofs/` home,
      and lemma names still need a collision-proof scheme.

      **Exit contract.** A generated table is evidence-bearing only when all of
      these hold:

      - entries are strictly ordered by rendered semantic identity, with
        duplicate IDs and duplicate/ambiguous operational keys rejected;
      - the root length-prefixes and version-tags every field, includes each
        canonical body, and binds the exact operational-key dispatch mapping;
      - canonical encoders are total, structural, exhaustive over constructors,
        and independent of `Repr`, pretty-printing, filesystem paths, comments,
        source spans, and Lean formatting;
      - builtins, intrinsics, externs, user functions, and monomorphized
        specializations retain distinct namespaces;
      - every entry has exactly one collision-proof, scoped `@[proofTable]`
        lookup lemma proving its exact key/ID/index relationship, while no
        generated lemma joins global `simp`;
      - identical compiler facts produce byte-identical modules and roots from
        different working directories and output paths;
      - deleting an entry, swapping an ID or body, duplicating an ID/key,
        corrupting dispatch, or emitting an incorrect lemma is killed by a
        well-typed mutation or committed adversarial fixture; and
      - generated artifacts follow R-0447's `.build/proofs/` ownership rule
        rather than becoming canonical source beside the program.

      `example : fns.isEvidenceBearing := by decide` proves INTERNAL
      CONSISTENCY only. It cannot prove source correspondence: a generator could
      emit a wrong ID, body, and matching lemma consistently. An independent
      comparison against resolved, checked, monomorphized compiler facts is
      therefore part of this step's exit gate.

      The `d2dd1eb3` slice landed the generator portion with semantic versus
      operational identity separated, bodies and dispatch bound into roots,
      explicit `PBinOp` canonicalization, `sourceBodyDigestV1`, scoped lookup
      lemmas, same-name module fixtures, deterministic/path-independent output,
      and mutation coverage. It refuses type-erased generic entries rather than
      emitting a false identity; positive specialized entries remain blocked
      until Mono supplies `typeArgs`. R-0447 owns moving generated output under
      `.build/proofs/`.
   5. **Migrate the nine**, one at a time, with dual comparison: same theorem
      results, same evaluation behaviour, stable generated root, kernel replay,
      and no new hand-written entries.

      **State: 8 of 9 migrated, 1 BLOCKED — 42 entries migrated of 45
      inventoried.** The three-entry `proofFnsExt` is the difference; it is
      blocked, not partially done, and must not be counted as either.

      `ctTagFns` 1, `elfFns` 5, `cryptoFns` 4, `parseValidateFns` 8,
      `fixedCapacityFns` 4, `combineFns` 2, `shaFns` 16, `proofFns` 2 = 42.
      Every committed spec was verified to hash to the compiler's digest UNDER
      NORMALIZATION before adoption; no spec was edited to make a digest agree.
      Kernel replay unchanged across all eight examples (31 theorems, 0 failures).
      Gated by `check_table_migration.sh` (declared-key resolution with an
      undeclared-key control, identity match, digest match, dispatch agreement,
      root presence, kernel replay, and an inventory leg).

      **The ninth, and its precise prerequisite.** `proofFnsExt` is `proofFns`'
      two entries plus `decode_header` — `packet.decode_header`, which the
      compiler reports `eligible (extraction failed) — unsupported: mutable
      borrow`. With no generated body there is nothing to compare the committed
      spec against, and adopting an identity and digest anyway would assert a
      correspondence nobody checked. Partial migration is unavailable by design:
      one unidentified entry disqualifies the whole table (`allIdentified`).
      UNBLOCKS ON mutable-borrow support in ProofCore extraction. A tripwire
      asserts it is still unextractable, so that day fails the gate.
   6. **Enable typed classification** (§1.1-1.2). Classify the ELABORATED
      theorem type, never source text: an `FnTable` represented by a bound
      theorem variable is a `contract` dependency (the theorem holds for any
      table), while a named table constant or an inline expression whose type
      returns `FnTable` is a `body` dependency. A 2026-07-31 metaprogram spike
      measured 271 corpus theorems that mention `FnTable`: 113 contract, 158
      body, and zero mixed. LANDED as `Concrete/Proof/DependencyEdge.lean`
      (vocabulary) and `DependencyEdges.lean` (classifier), gated by
      `check_dependency_edges.sh`; the shipped classifier measures **113 contract
      / 166 body / 0 ambiguous**.

      **Reconciling 158 -> 166.** The spike's count used the narrow "constant
      whose TYPE is `FnTable`" test — the very one flagged below as failing open.
      Widening it to "returns `FnTable` after any number of arguments" moved
      exactly the eight table-producing/metatheory cases named as that latent
      boundary, 158 + 8 = 166. They are structural metatheory about `FnTable`
      itself, so `body` OVER-approximates them deliberately: a needless recheck,
      never a false claim. The two numbers describe the same corpus under the
      unsafe and the safe test respectively. The basic discriminator is therefore small and
      unambiguous on the live corpus. Preserve the hostile control: checking
      only constants *of* type `FnTable` misses inline constructors/combinators
      and fails open. Four table-producing constants and eight current
      FnTable-metatheory lemmas exercise that latent boundary; any unresolved or
      mixed future program claim is `missing`, not guessed.
   7. **Enforce never-under-approximate** — static access binds the exact
      reachable entries; dynamic access binds the entire canonical root; if
      exact coverage cannot be established the answer is `missing`, never a
      guessed subset.
   8. **Define receipt-envelope plumbing only.** Establish the versioned data
      structure, deterministic workspace/import/toolchain identities, and replay
      serialization without issuing authoritative receipts. Slice 5 owns the
      semantic `ProofSubjectDigest`; slice 6 owns the dependency root; slice 7
      is the first point allowed to combine them into a receipt.

   Contract edges are unaffected throughout, since they never name a table.

   The `contract`/`body` discriminator is visible in the theorem's TYPE, and both
   shapes already exist in the tree — see `docs/verification/PROOF_CONTRACT.md` §1.1-1.3 for
   the normative rules, including how a `body` edge is resolved and why the
   receipt's workspace field is a closure digest rather than a path.
   `ProofKit.unary_call` quantifies over `fns : FnTable` and takes the callee's
   behaviour as a hypothesis (contract); `combine_correct` names the concrete
   `combineFns` and unfolds the helper bodies (body). Deriving the edge kind
   therefore means reading the theorem's type, which `check-proofs` already
   obtains via `#check` — the open design question is how to parse it robustly
   rather than whether the signal exists.

   Until typed edges exist, slice 3's conservative containment stands and the
   system fails closed: a caller whose callees have no links is `depsNotCurrent`.
   Adding per-helper proof links (as `examples/proof_patterns/composition` now
   does) is a TEMPORARY repair to keep the canonical example honest — it is not
   the permanent model, and proof-link bureaucracy must not become the price of
   composition. Once typed edges land, migrate that example to demonstrate the
   modular and closed-subject styles side by side.

   A trusted boundary counts as current for traversal, but its trust propagates
   into the caller's evidence assumptions: a proof reaching one records
   `proved_by_lean_modulo_trusted`, never unqualified `proved_by_lean`
   (`composition_trusted_helper`). The receipt must carry the same distinction.

   The workspace-resolution half has LANDED: `--report check-proofs` resolves the
   Lake workspace by walking up from the INPUT rather than using the process
   working directory. It previously invoked `lake` with no `cwd`, so the verdict
   depended on where the caller stood — the same file by absolute path gave
   "3 verified, 0 failed" from the repo root and "0 verified, 3 failed" from
   /tmp, blaming each theorem with `theorem_lookup` when the theorems were fine
   and the workspace was simply never found. An input with no workspace above it
   now says so and exits non-zero. An input that has NO workspace but is replayed
   by a caller inside one falls back to the caller's workspace — resolving only
   from the input was too strict and made 12 real kernel-verified std proofs
   report as unreachable, because the gate copies sources to a temp dir. The
   report names which workspace it used and whether it came from the input or
   the working directory, so a fallback verdict is never anonymous. Gated in
   `check_proof_freshness.sh` (identical verdict from two directories; the
   fallback case; the missing-workspace case named, unblamed and fail-closed).

   NOTE for the receipt: the human report prints the absolute workspace path,
   which is useful for auditing but is NOT a deterministic identity — it is
   machine-specific. The receipt's `workspace` field must be a digest of the
   workspace/import closure, not a path.

   **Current remainder after `477a1726`/`ebc6f1c7`/`b99a8f99`.** The validated receipt core
   type exists and closes construction, empty-value, correspondence, duplicate and ordering
   bypasses. It is not the complete envelope and has no production consumer. Still required:
   deterministic workspace/import/toolchain identity producers; dependency root, theorem/artifact,
   replay-result and qualification fields; and canonical serialization plus validated decoding.
   The edge-aware, schema-first comparison disposition now exists; the remaining obligation is to
   make the production status consumer use it rather than reconstructing comparison logic. This
   slice may establish replay plumbing, but it cannot upgrade a legacy body hash to a complete
   proof claim.
5. **Semantic subject digest.** Replace body-only freshness with a versioned
   canonical `ProofSubjectDigest` covering qualified semantic identity, full
   typed signature and generic constraints, capabilities, normalized typed
   body, requires/ensures/invariants, the normalized selected specification and
   claim scope/coverage, and extraction/schema version.

   **Where it can be computed, established 2026-07-31 and load-bearing for how
   this slice is built.** `CFnDef` does NOT carry `requires`/`ensures`: contracts
   are erased at the AST→Core boundary, and `ProofCore` extraction runs over Core.
   The Report layer still has the AST (it already looks functions up that way for
   obligation TODOs), so today only Report can see signature-plus-contract facts
   together. Three options, and the choice is not obvious:

   * compute the digest in Report, where all the facts happen to be reachable —
     cheapest, but puts an evidence-bearing computation in the presentation layer,
     the same inversion that put `CallableId` in the resolve layer rather than in
     Proof;
   * carry contracts across the AST→Core boundary on `CFnDef` — the digest is then
     computable where extraction happens, at the cost of widening Core with data
     no codegen path consumes;
   * mint the digest in `ProofCore` from a facts record threaded in alongside the
     Core function, keeping Core narrow and the digest out of Report.

   The third looks right by the same argument that placed `CallableId` — a digest
   minted by its consumer is that consumer's opinion — but it needs the facts
   record designed. Do not start by picking the cheap option; `sourceBodyDigestV1`
   is already emitted from Report and that is defensible only because it is
   explicitly NOT receipt-eligible.

   **STATE 2026-08-08, corrected by the end-to-end audit.** The DECLARATION-FACTS portion is
   done: `proofSubjectDigestV2` covers identity/signature/generics/capabilities/contracts, returns
   `none` for incomplete facts, and is minted per entry as `ProofCoreEntry.subjectDigest`.
   `needsRecheck` is threaded through every surface (`3936709e`). The FINAL semantic subject is
   not done: the function still combines those facts with the legacy `bodyFingerprint`, while
   the structural typed V2 body remains shadow-only; selected specification identity and claim
   scope are also absent. Dependency root and qualification belong to the receipt/evidence
   composition rather than being silently implied by this digest.

   **What remains is one sequencing decision, and it is NOT a coding question.** Switching
   `deriveObligationStatus` to compare the v2 digest makes the authoritative migration corpus's
   **44** stored `#[proof_fingerprint]` values non-comparable at once — they are v1, computed
   from the body hash. The earlier number 53 had no executable denominator and is retired; the
   V1 golden and dry-run manifest own this count. Measure the currently-`proved` subset from that
   same manifest rather than preserving the earlier prose count 19 without a matching inventory.

   That verdict is CORRECT: those links were never checked against a signature or a contract, so
   `proved` overstates what was verified. But issuing it is the *migration*, and migration is
   step 7 ("issue receipts and migrate honestly"), explicitly gated on the FINAL digest. Doing it
   here would either strand 19 proofs mid-slice or invite re-recording fingerprints against a
   digest that step 6 may still move.

   **So the comparison lands in SHADOW, like the other four axes, and for the same reason.** Emit
   what the v2 comparison WOULD say alongside the current verdict; assert both halves (the shadow
   verdict moves when signature or contracts move, AND the live verdict does not); flip it in
   step 7 together with replaying and re-recording the 44 manifest rows as `v2:`-prefixed. The `v2:` prefix is the
   discriminator — a stored value without it is v1 by construction, which is what makes
   `needsRecheck` decidable rather than guessed.

   Closing bugs 059 and 060 means wiring the digest into the FRESHNESS COMPARISON,
   not merely emitting it. Their tripwires in `check_proof_freshness.sh` still pass
   — a whole-signature `i32 -> u32` change and a TRUE-vs-FALSE `#[ensures]` are
   both still indistinguishable to the stored fingerprint — and they are the
   signal that this slice is done. The theorem and
   toolchain are evidence about that subject; their identities belong in the
   receipt, not in the semantic subject digest.
6. **Dependency root.** Compute freshness transitively from deterministic,
   validated reachability-closure material so recursion is finite and a deep
   callee edit stales every dependent claim without making source location or
   alpha-renaming noise semantic. Cycle members share the same reachable
   closure but retain distinct roots because each root also binds its own
   subject. This is intentionally simpler to audit than a Merkle-DAG-on-SCC at
   the current corpus size; incremental sharing is a measured future
   optimization, not part of R-0004's soundness claim.
**Historical state note (2026-08-01; superseded).** This point recorded slices
1–3 done, slice 4 at 8-of-9 tables, a standalone dependency root with no
consumer, and no slice-7 migration. The old summary said slice 5 still lacked a
semantic body component; that is no longer current: `proofSubjectDigestV2` now
exists and is minted per entry. Use the authoritative table at the start of
R-0004 for current status. This note remains only to preserve the investigation
sequence behind the V2 body design below.

**V2 body representation — design constraints, before any code.** The facts are
already alpha-invariant (parameters render as types by position); it is
`bodyFingerprint` alone that makes a capture-avoiding rename visible. The fix is
NOT another name-based normalization heuristic layered on top. It needs:

* ONE typed binder environment covering parameters, `let` bindings, match
  patterns and loop-carried values — not per-construct reconstruction. The
  loop-scope approximation currently in `ContractFacts.ofResolved`, which infers
  binders from a loop's assignments, is exactly the shape to avoid: it misses a
  read-only outer local and can treat an assignment target as a declaration.
* Semantic identities for direct calls, so a call in a body is not a name.
* Binders rendered by RELATIVE POSITION, which is what makes a capture-avoiding
  rename invisible.

`bodyFingerprint` must be preserved BYTE-FOR-BYTE as the V1 migration input.
Every stored `#[proof_fingerprint]` was computed under it, so changing it in
place would report the corpus stale and invite the backfill that is forbidden
until kernel replay issues a trustworthy receipt.

**Core does not carry the identities V2 needs — verified 2026-08-01, and this is
what decides the order.** A traversal over `CStmt`/`CExpr` cannot recover them,
so it would either serialize names again or fail closed across most of the
corpus:

* `CExpr.ident (name : String) (ty : Ty)` — one constructor for BOTH locals and
  constants, so a traversal cannot tell a binder reference from a constant
  reference, which is the distinction the whole encoding turns on;
* `Callee.direct (name : String)` — a call target is a name, not a `CallableId`;
* labels are `Option String`, and pattern binders and loop-carried bindings are
  textual too.

So the typed inputs come FIRST, produced in Elab where the information still
exists, either by enriching these constructors or by carrying a parallel
evidence-body representation alongside Core. Writing the canonicalizer before
them would produce exactly the temporary name-normalization layer the identity
work then has to delete.

**Sequence.** (1) introduce the typed inputs — `ConstId`, binder-relative
identities, and `CallableId` for direct references — or a parallel evidence-body
representation carrying them from Elab; (2) capture elaborated lexical scope;
(3) build an EXHAUSTIVE, alpha-invariant `ProofBodyCanonicalV2` over that typed
representation; (4) feed it into the subject digest and integrate dependency
freshness; (5) receipts last.

**Typed identity is required for EVERY non-binder semantic reference, not only
direct calls.** Constants, function references, types, fields and enum variants
all enter canonical bytes, and each is currently a spelling. Covering only calls
would produce an encoder that is alpha-invariant — renaming a local changes
nothing — while still treating DECLARATION spellings as identity, which is the
same defect wearing the opposite disguise: invariant where it should be, and
name-keyed exactly where identity matters.

So the rule for V2 is:

> No raw source, import-alias, or display spelling is used as identity. Bound
> references encode relative binder positions; declaration references encode
> versioned semantic IDs, whose canonical serialization may include
> definition-site names. Literal strings remain ordinary program data.

An earlier phrasing here — "a name may appear only as a binder rendered by
relative position" — was self-contradictory in both directions: a binder rendered
by position carries NO name, and a semantic ID's canonical serialization DOES
carry a definition-site name (`CallableId.render` is `v1:user:mod.decl`). The
distinction that matters is not whether a name appears in the bytes, but whether
a spelling is being used AS IDENTITY.

**FIRST TASK IS A BOUNDED PROBE, not the V2 implementation.** Whether the typed
inputs can land incrementally or force a flag-day across Core is unknown, and it
is the difference between roughly four and roughly eight sessions of remaining
work. It is cheap to find out relative to committing to either path, so find out
first:

1. Land the V1 byte-for-byte golden (below) — the probe needs a tripwire proving
   it changed nothing before it starts changing things.
2. Inventory every consumer of `CExpr.ident`, `Callee.direct`, labels and
   binders. The count and their shape decide whether step 4 is even possible.
3. Spike ONE typed reference end to end: Elab -> Core -> ProofCore.
4. Measure whether compatibility fields with defaults permit an incremental
   migration, or whether every consumer must change together.
5. Require Core/SSA/runtime artifacts to be UNCHANGED — a proof-bookkeeping
   change that moves codegen output has escaped its remit.
6. Discard the spike, or record the proven migration sequence, before writing any
   V2 code. A spike that is silently promoted to the implementation is how the
   measurement gets skipped.

**PROBE RESULT (steps 1-2 and 4, measured 2026-08-01).** The parallel
representation wins, and not marginally — the measurement decides it:

* Modifying `CExpr.ident` to carry typed identity breaks every pattern match on
  it. 17 files touch `CExpr`; ~47 `.ident` sites, of which **16 sit on
  codegen/runtime paths** — Lower 7, Mono 5, Interp 3, CoreCanonicalize 1.
  Adding a field to an inductive CONSTRUCTOR (unlike a structure field with a
  default) breaks all matches at once, so compatibility defaults do NOT permit an
  incremental migration here. It is a flag day.
* Worse, it is a flag day across exactly the paths step 5 forbids disturbing: a
  proof-bookkeeping change that edits Lower/Mono/Interp puts Core/SSA/runtime
  artifacts at risk, and "unchanged" would then have to be established by testing
  rather than by construction.
* A PARALLEL evidence-body representation, built in Elab and consumed only by
  proof code, touches none of them. The existing `CheckedDeclFacts` record is the
  precedent and the proof of shape: it reaches SubjectFacts, DependencyRoot,
  ProofCore, Core and Elab — and Lower, Mono and Interp appear nowhere in that
  list. Artifacts are unchanged BY CONSTRUCTION.

So: build V2 over a parallel typed evidence body produced in Elab. Do not widen
`CExpr`. Step 3's spike is unnecessary for the decision — the constructor-vs-
structure distinction settles it without one — and step 5's requirement is met
structurally rather than by measurement.

The remaining open question is narrower and belongs to the implementation, not
the probe: which typed identities the parallel body needs for constants, function
references, types, fields and enum variants, and where each is still available at
the point Elab builds it.

**SPIKE RESULT (step 3, measured 2026-08-01) — the V2 input surface is now
known, not assumed.**

| identity kind | reachable where Elab builds the parallel body? |
| --- | --- |
| constants | LOCAL ones YES, proven end to end and then REVERTED — `m.constants` is in scope; a distinct `ConstId` resolves and `LIMIT` yielded a covered subject. Scope of the proof: LOCAL constants only, with ambiguity checked against local functions and specs — NOT against the imported / builtin / intrinsic / extern universe. |
| fields / enum variants | UNKNOWN — `cStructs` and `allEnums` being in scope proves the DECLARATIONS are available, NOT that a use site's identity is. The missing fact is which declaration a particular access resolved to, which aliases and repeated field names make non-obvious. Still needs its own measurement. |
| direct calls | YES — already resolved via `CallableId` |
| local binders | NOT at that point — `elabFn : FnDef → ElabM CFnDef` returns only the Core function, so the scope it computed is gone |

**The binder-scope change is smaller than it looks, for two measured reasons.**
`elabFn` has exactly ONE caller. And `addVar` PREPENDS to `env.vars` without ever
popping, so the environment accumulates monotonically through a function rather
than behaving as a lexically-scoped stack — the value at the end of `elabFn`
already contains every binder the function introduced. Shadowing, which would
otherwise make a flat list ambiguous, is already resolved upstream: Elab
alpha-renames shadowing binders to Core-unique names (`value` -> `value.b7`, the
bug-045 fix), so names are unique within a function by construction.

So surfacing binder scope is: change `elabFn`'s return to carry the accumulated
environment, update its one caller. What still needs checking before relying on
it is whether the accumulated list is sufficient for RELATIVE POSITION — it gives
the set of binders, not their nesting — and whether alpha-renaming covers `let`
and loop binders as well as match binders.

`ConstId` is deliberately not a `CallableId`: a constant has no parameters,
capabilities or arity, and sharing the type is what let a constant borrow a
same-named function's identity. Ambiguity — a name that is both — resolves to
neither.

**THE SPIKE WAS REVERTED, per the probe contract.** Its code was minimal and
worked, which is exactly why it had to go: it had quietly become production —
`ConstId` in the resolve layer, a resolver threaded through `SubjectFacts`, gate
legs rewritten — while still labelled disposable. The contract in step 6 says a
spike is discarded or explicitly promoted, because a spike silently promoted is
how the measurement gets skipped. Promotion would have required final identity
ownership, imported/submodule constant support, classified stderr and real
alias/collision controls; none of that was done, so it was discarded and the
FINDING kept. V1 golden still 77/77 after the revert.

**What the spike did NOT establish, recorded so the next one is aimed correctly:**
there is still no parallel typed evidence body — the spike fed the EXISTING
subject-digest path; no local binder was carried through any new representation;
and field/variant USE-SITE identity remains unmeasured.

**The architectural lesson is sharper than "produce it in Elab".** The parallel
evidence body must be produced DURING ELABORATION, where use-site resolution and
lexical binding are known — not reconstructed afterwards from `m.constants`,
`cStructs` or textual expressions. Reconstruction is what the reverted spike did,
and it is why it could only answer for local constants.

**NEXT BOUNDED SPIKE — and how to run it so it stays a spike.** On a DISPOSABLE
branch or worktree, whose implementation commit is NEVER merged; only
measurements and the proven implementation sequence come back. The last one was
minimal and worked, and that is exactly how it ended up in the production tree
while still labelled disposable.

Produce a tiny parallel evidence EXPRESSION during elaboration carrying:

* one binder reference by BINDING POSITION;
* one resolved constant identity;
* one resolved field or variant identity;
* one construct that RESOLVES NORMALLY but whose EVIDENCE MAPPING is
  unavailable. Not an invalid source reference — that fails during ordinary
  resolution and never reaches evidence's fail-closed path, so it would test the
  compiler's error handling instead of the property in question.

The V1 77/77 golden is the immovable boundary throughout.

**SPIKE 2 RESULT — AMENDED. The correct heading is "lookup sites located;
identity provenance remains", NOT "input surface fully characterised".**

The amendment matters because the original conflated two different things:
DECLARATION SELECTION (which declaration shape matched) with DEFINITION-SITE
IDENTITY (a stable name for that declaration across imports and aliases).
Verified: `StructDef` and `EnumDef` carry only `name : String` — no defining
module, no origin — and imported definitions are copied into `ResolvedImports`
WITHOUT provenance. So Elab knows which declaration matched and cannot yet say
which module defined it. Two same-spelled types in different modules are
indistinguishable at that point, which is precisely the property an identity has
to provide.

| input | availability at the USE SITE, during elaboration |
| --- | --- |
| field/variant declaration SELECTION | AVAILABLE — `lookupStruct` / `lookupEnum` return the matching declaration at the use site. |
| LOCAL field/variant identity | probably constructible from the current module plus the declaration; not yet demonstrated. |
| IMPORTED field/variant identity | UNAVAILABLE. Needs definition-site provenance — a `TypeId` carried through imports, and then `FieldId` / `VariantId` derived from it. This is the real gap. |
| binder position | MECHANICALLY retrievable — `lookupVar` finds the entry in `env.vars` and discards which one matched. But whether a raw `env.vars` position is the correct STABLE LEXICAL identity is untested across parameters, lets, loops, patterns and temporary scopes; a monotonically accumulating list is not obviously a lexical numbering. |
| constant identity | AVAILABLE (spike 1), for local constants; imported / builtin / intrinsic / extern still unmeasured. |

"No new resolution machinery" was too strong. The LOOKUP ALGORITHM exists and is
already run at every use site; what does not exist is SEMANTIC PROVENANCE carried
through imports. Some of the V2 input surface is recoverable by not discarding
what the elaborator computes; the imported-type case additionally requires a
`TypeId` to be introduced and threaded.

**SPIKE 3 RESULT — SCOPED. The correct heading is "ONE provenance insertion point
located, and an aliased-type Elab gap found", not "TypeId has an exact insertion
point". The generalization outran the measurement again, and this time the
falsifying fixture had not been RUN at all — only code was inspected.**

**BUG FOUND by finally executing it (file it before any TypeId work).** An
imported struct behind an ALIAS does not resolve as a struct in Elab:

```
mod lib { pub struct Copy Point { x: Int, y: Int } }
mod app { import lib.{ Point as Coord };
          fn f(c: Coord) -> Int { return c.x; } }     -- E0254 non-struct type
```

The CONTROL is what makes this conclusive: the same import WITHOUT the alias
gives `E0298 field 'x' is private to its defining module` — a different and
sensible error, so the type resolves fine unaliased. The alias alone breaks type
resolution. Cause, from the code: Resolve registers `localName` for the aliased
type while `resolveImports` appends the original `sd`/`ed` unchanged, so
`StructDef.name` stays `origName` and `lookupStruct(alias)` finds nothing.

This fails during ordinary elaboration, LONG before any evidence path — so it
blocks the imported field/variant measurement outright and must be fixed first.

**COMPLETE TYPE-INGRESS INVENTORY (measured 2026-08-02).** There are FIVE paths,
not one, and they do not share the same facts — which is why "the copy site" was
the wrong model and why `TypeId` needs a rule per path:

| # | path | site | provenance available |
| --- | --- | --- | --- |
| 1 | local declarations | `FileSummary` 199-200 | `m.name` |
| 2 | explicit import (aliasable) | `FileSummary` 296, 302 | `summary.name`, `origName`, `localName` |
| 3 | transitive closure | `FileSummary` 386, 398 | `summary.name` (loop binds `for summary in importedModules`); no alias exists on this path, types arrive under their original name |
| 4 | sibling / submodule injection | `Elab` 1763-4 | carries an already-resolved `subImports` set — provenance must survive from wherever those entries were built, i.e. it INHERITS from 1-3 rather than having its own |
| 5 | compiler-built enums | `Elab` 1632 | NONE — `builtinOptionEnum` / `builtinResultEnum` are synthesized and have no defining module at all |

So four of five can produce a `TypeId` from facts already in scope. Path 5 cannot,
and needs a NAMESPACE rather than a module — exactly as `CallableId` needed
`.builtin` / `.intrinsic` beside `.user`. That is a missing case in the design,
not missing data.

Path 4 is the subtle one: it does not introduce provenance, it PROPAGATES it, so
it is correct only if 1-3 record theirs. A design that fixed the explicit-import
path alone would leave sibling-injected types carrying whatever path 3 dropped.

`ResolvedImports.structs : List StructDef` / `.enums : List EnumDef` store bare
declarations with no origin, which is why imported field/variant identity is
unavailable. But at the COPY SITE in `FileSummary.lean` both required facts are
in scope and simply discarded:

* `summary.name` — `FileSummary` carries the DEFINING MODULE of the imported
  declaration;
* `origName = sym.name` — the DEFINITION-SITE name, captured before aliasing
  (`localName = sym.effectiveName` is the alias).

The copy is `acc.structs ++ [sd]`, which drops both. At THIS path, introducing
`TypeId` is recording `(summary.name, origName)` at the point they are already
known — the
same shape as every other defect in this task: information present at the point
of use, discarded before the point of record. It is NOT a new resolution
algorithm; the alias-versus-definition distinction is already computed here.

**And the constructor-versus-parallel question is now MEASURED, with the opposite
answer to `CExpr` — which is why the analogy had to be tested rather than
reused.** `StructDef` and `EnumDef` are STRUCTURES, not inductives. Adding
`definedIn : Option String := none` to both compiled with ZERO errors across 23
files and 44 construction sites: a structure field with a default is backward
compatible, whereas adding a field to an inductive CONSTRUCTOR (the `CExpr` case)
breaks every match at once.

So provenance belongs ON the declarations, not in a parallel `ResolvedImports`
list. Had the `CExpr` conclusion been carried over by analogy, the design would
have taken the harder path for a reason that does not apply here.

The implementation must preserve THREE distinct facts: the local lookup
spelling (`name`), definition module (`definedIn`), and definition-site spelling
(`definitionName`). Bug 064 deliberately changes `name` to an import alias; using
`(definedIn, name)` would therefore mint a new identity for the alias. `TypeId`
is derived only from `(definedIn, definitionName)`, then `FieldId` / `VariantId`
from that owner. Nested summaries carry their full definition path, while
transport paths preserve an already-minted identity rather than reminting it.

**TYPE-IDENTITY FOUNDATION LANDED.** `TypeId` has typed user/builtin constructors;
the builtin constructor accepts `BuiltinEnumId`, never display text. Parsed or
manually built declarations have no identity until their summary stamps both
definition-site components, and accessors fail closed on absence. The gate runs
the real summary/import pipeline for local, aliased, transitive, nested and
builtin cases. This supplies the V2 input; it does not yet create the parallel
evidence body or close freshness.

**ELAB USE-SITE WIRING LANDED.** Field accesses, struct literals/updates,
field assignments, enum construction and resolved enum-pattern arms now append
typed `TypeId` / `FieldId` / `VariantId` values to a parallel per-function V2
input record while the elaborator still holds the resolved declaration. If
ordinary language resolution succeeds but the declaration has no evidence
identity, that record becomes `covered = false`; the use is never omitted.
Branch-scope restoration preserves the append-only record. These inputs are
deliberately not part of `CheckedDeclFacts.canonical` yet: they are one typed
surface of the eventual exhaustive `ProofBodyCanonicalV2`, not a partial body
digest that may mint freshness. Raw `env.vars` positions were measured to shift
across branches and loops, so they are forbidden as canonical binder identity;
the general proof-point-scoped push/pop binder stack remains absent.

**CURRENT COVERAGE RATCHET (measured 2026-08-04).**
`scripts/tests/check_subject_coverage.sh` fails on both a rise and an unexplained
drop, so improvement requires the cause inventory to change in the same commit.
The examples corpus currently has 417 covered declarations and exactly 3
uncovered declarations: 2 caused by module constants in one program (the open
`ConstId` work), plus 1 deliberate negative that must remain uncovered. The two
previously uncovered ghost programs are now covered by a ghost-specific binder
frame, and renaming a ghost binder is gated as digest-invariant. That closes the
measured ghost cause; it does NOT stand in for the general scoped lexical binder
stack that `ProofBodyCanonicalV2` still needs across parameters, lets, patterns,
branches, loops and temporary scopes. `ConstId` should deliberately move this
inventory from 3 to 1; authoritative activation requires zero unexplained
uncovered proof subjects, while retaining the named negative control.

**V2 producer-completeness and semantic-binding gates.** R-0004 owns an
immediate, production-path gate with the following acceptance contract; R-0438
later generalizes the same inventory machinery across all claims and consumers:

- every proof-relevant source/evidence constructor produces an exhaustive typed
  V2 node or explicitly makes the enclosing subject `covered = false`; adding a
  constructor or emission surface fails CI until it is classified;
- the inventory is generated or reflection-backed from the real producer and
  evidence datatypes, not a deny-list or grep over a nearby file, and contains a
  mutation that omits one constructor to prove the gate is load-bearing;
- changing a referenced constant initializer or ghost initializer moves the
  subject digest, while capture-avoiding ghost-binder renaming and importing the
  same constant through an alias do not; and
- each assertion exercises the ProofCore path that mints the digest. A helper,
  report renderer, diagnostic string, or synthetic empty result cannot stand in
  for the production consumer.

**Authoritative V2 activation gate.** Before V2 changes any user-visible proof
status, run it in shadow mode over the complete proof corpus and publish a
per-subject comparison of V1 status, V2 completeness/digest, dependency
material, and replay readiness. Activation requires every required subject to
be complete, every V1/V2 disagreement to have a stable classification, and the
unexplained count to be exactly zero. The deliberate uncovered negative stays a
control, not a claim to migrate. Shadow mode may collect facts and receipts but
may not emit a friendlier verdict than V1. R-0215/R-0217/R-0219 own the later
general mutation, clean-replay and end-to-end validation suites; this is the
R-0004 forcing gate that must exist before those downstream tasks.

**That last point is measured, not assumed.** `m.name` is in scope at the LOCAL
population site (`FileSummary.lean`, where the summary is built with
`name := m.name` two lines above `structs := m.structs`), exactly as
`summary.name` is at the import copy site. So both paths can record the defining
module, and the field should be `String`, not `Option String`.

An `Option` that is `none` locally and `some m` when imported would give one
declaration TWO identities depending on which side of an import it was observed
from — the same declaration, two spellings. That is the defect class this whole
task exists to remove, and it would have been introduced by the convenient
default rather than by any missing information.

**PREVIOUS NEXT-SPIKE NOTE (now answered by the above):** a field or variant
IMPORTED from another module,
preferably behind an alias and alongside a same-spelled local type. That is the
case that shows exactly where definition-site `TypeId` must enter the pipeline —
and it cannot be answered by a local-only example, which is how the previous
spike overreached.

**Still unmeasured:** constant identity for imported/builtin/intrinsic/extern
references, and the fail-closed path for a construct that resolves normally but
has no evidence mapping.

**V1 stays frozen byte-for-byte, with a golden proving it.** The existing
`#[proof_fingerprint]` corpus is the migration input; a test must show those
bytes are unchanged, so "V1 is untouched" is checked rather than intended.

7. **Issue receipts and migrate honestly.** Only after the final subject digest
   and dependency root exist may successful kernel replay issue a receipt. It
   binds the subject digest and dependency root to theorem identity plus theorem
   artifact digest, compiler/Lean/ProofKit and schema identities, workspace and
   import closure, replay command, and result. A legacy fingerprint or receipt
   schema becomes `needs_recheck`, not `stale`: the evidence format changed, not
   necessarily the program. Backfill the repository corpus only from successful
   kernel replay; never copy newly computed hashes into source as a substitute
   for verification. R-0216 owns the general corpus-migration command after this
   one-time R-0004 migration. As a migration output, publish the first
   proof-coverage baseline consumed by R-0150: use the existing eligibility
   report as the denominator and classify every eligible obligation by
   discharge route (`structural`, `omega`, `bv_decide`, linked Lean, or
   unsupported), including stable blocker reasons. This measures the automation
   opportunity; it does not make auto-discharge a prerequisite for replaying
   existing Lean artifacts.

   Before switching freshness authority, check in a DRY-RUN MIGRATION MANIFEST
   with one row for every stored legacy proof link. The measured 2026-08-04
   baseline is 44 stored `#[proof_fingerprint]` values; the separate V1 golden
   covers 77 extracted functions across all eight examples and must remain
   byte-for-byte stable. Each of the 44 rows records stable subject/claim
   identity, legacy schema/value, V2 completeness and blocker, replay result,
   intended final status, and issued receipt (when successful). Missing,
   duplicate, silently skipped, or hash-copied rows fail the migration. If the
   stored corpus changes before activation, update the manifest baseline and
   explain the delta rather than weakening the exact inventory.

8. **Red-team the authoritative pipeline before declaring R-0004 complete.** This is a
   completion slice, not optional post-closure hardening. Assume the attacker is trying to make
   Concrete emit friendly `proved` using evidence that is stale, incomplete, context-dependent,
   attached to the wrong callable or claim, missing a direct/transitive/recursive dependency,
   trust- or assumption-widened, unreplayed, schema-confused, partially migrated, corrupted, or
   copied across workspaces. Exercise three layers: production-consumer weakening mutations;
   permanent adversarial fixtures for every discovered failure; and one hostile end-to-end
   multi-module/project corpus combining subject, dependency, receipt, migration, cache and
   environment attacks. At least one attack packet must be designed by a non-author. Every case
   must downgrade or fail explicitly; none may retain a friendly verdict. Slice 8 starts only
   after Slice 7 creates the production path it attacks, and every successful attack is fixed and
   retained before closure.

Keep the axes explicit: subject freshness, dependency freshness, kernel replay,
producer/toolchain identity, and coverage are separate facts under R-0440. A
friendly `proved_by_lean` claim requires a current subject, a current dependency
root, and a valid replay receipt; none of those facts alone upgrades the others.
The same HMAC input currently reports 11 verified / 0 failed from the repository
root but 0 / 11 `theorem_lookup` failures and `Toolchain: unknown` from
`examples/hmac_sha256/`; the replay-foundation slice must eliminate that
cwd-dependent verdict before any receipt is accepted.

Gate each omitted component, missing/malformed digest, false postcondition,
`i32 -> u32` signature edit, capability/generic edit, theorem/spec/receipt swap,
direct and multi-hop dependency drift (including missing, unbound, and failed
callees), recursive SCCs, comments/formatting/alpha-renaming stability,
repository-root/project-root invocation parity, legacy-schema
`needs_recheck`, theorem/toolchain/workspace receipt mismatch, and a mutation
proving no old body-only or advisory-`staleDeps` path can emit
`proved_by_lean`. Keep the R-0002 proof-extraction CI repair and its reverified
std fingerprints in their already-landed commit; do not mix that repair with
these new evidence gates.

For the receipt specifically, register weakening mutations for constructor privacy and absence of
`Inhabited`, empty subject/environment identities, table identity correspondence and duplicates,
edge kind, dependency root, theorem/artifact identity, successful replay, qualification and schema
comparison. A helper-level `mint?` probe is necessary and insufficient: the mutation counts only
when the production status consumer fails.

**R-0004 completion gate.** This task closes only when all of the following are
true together:

- bugs 058, 059, 060, and 062 are closed by positive, mutation-sensitive gates;
- no hand-written evidence-bearing `FnTable` remains, and every declared and
  missing operational key agrees between generated evaluation and kernel replay;
- semantic edits to identity, signature, types, generics, capabilities, body,
  contracts, selected specification, or claim scope move the subject digest,
  while comments, formatting, source paths, spans, and capture-avoiding alpha
  renaming do not;
- every proof-relevant producer/evidence constructor is exhaustively classified
  as a typed V2 node or fail-closed uncovered, and a mutation proves an omitted
  constructor or emission surface is detected on the production ProofCore path;
- direct, transitive, and recursive dependencies produce the correct typed
  edges and deterministic validated dependency material/root, with dynamic
  access binding the whole canonical table rather than a guessed subset;
- contract dependence is either bound to the exact callee contract and imported hypotheses through
  a live production case (same contract/body change stays current; contract change does not), or is
  explicitly unsupported and owned by R-0473/R-0474; the conservative implementation-bound form
  cannot silently be described as precise contract composition;
- every friendly `proved_by_lean` claim has a current complete subject, a
  current dependency root, and a successful replay receipt with theorem,
  toolchain, schema, and workspace/import-closure identity;
- one typed theorem-replay producer returns success/refusal data rather than printing, and successful
  replay plus a complete canonical task is the only authority that can mint a receipt;
- the receipt has versioned canonical serialization and validated decoding, compares its edge kind
  and every bound field through one typed disposition, and cannot be directly constructed,
  defaulted, decoded or deserialized into a partial current-looking value;
- current proof status composes freshly recomputed source/correspondence/dependency/advisory facts
  with the immutable receipt and policy; a stored receipt or cache entry alone never produces a
  current verdict;
- legacy body fingerprints and old receipt schemas become `needs_recheck` and
  cannot upgrade themselves by copying a newly computed hash;
- shadow-mode comparison has zero unexplained V1/V2 disagreements, every
  required subject is complete, and the exact legacy-migration manifest accounts
  for every stored proof link before V2 becomes authoritative;
- repository proofs are replayed into receipts or remain honestly classified as
  missing, unbound, stale, dependency-not-current, failed, or trusted;
- root/project-directory invocation and clean-machine replay agree, generated
  artifacts are deterministic, and every weakening mutation above is killed;
- every authoritative run produces a typed completion artifact; an interrupted producer or absent
  summary is a distinct failure rather than empty output interpreted as success;
- the first eligibility-denominated proof-coverage baseline is published with
  stable discharge-route and blocker classifications; and
- Slice 8's independent red-team packet, production-consumer mutation suite, permanent hostile
  fixtures and end-to-end adversarial project all pass, with no stale, incomplete, misbound,
  dependency-incomplete, unreplayed, trust-widened, contract-widened, vacuous, schema/policy-confused,
  cache-derived or cross-project evidence able to produce a friendly `proved` verdict.

**[historical] Completion-gate audit as of 2026-08-16, before production replay and closure.** This
snapshot explains the work that remained on that date; it is not current status.

| completion condition | audited state |
|---|---|
| bugs 058/059/060/062 | **PARTIAL** — 058 contained, 062 closed; 059/060 still leave the live V1 verdict `proved` while only shadow V2 moves |
| generated evidence-bearing tables | **PARTIAL** — 8/9 migrated, 42/45 entries; `proofFnsExt` blocked on mutable-borrow extraction; final bar still forbids hand-written evidence-bearing tables |
| final semantic subject and invariance | **FOUNDATION COMPLETE, ACTIVATION PENDING** — deterministic refrozen V2 binds declaration facts, structural body, selected spec and claim scope; semantic/invariance controls pass. It remains shadow-only until replay-backed Slice 7 migration |
| exhaustive producer coverage | **STRONG SHADOW FOUNDATION** — 452 subjects threaded, 441 covered, 0 absent, 11 named fail-closed refusals; authoritative subject still does not consume these bytes |
| typed deterministic dependencies | **CLOSED 2026-08-16** — ProofCore and reports share one node/root producer keyed on `DefinitionIdentity`; correspondence and computable roots are consumed by production verdicts; 13 roots refuse and none belongs to a proved subject. `PROVED-ROOTS` now constructs a proved obligation with an unkeyable edge through real `selfCheck`, and its `.take 0` production mutation is killed. |
| monotone trust/assumption propagation | **CLOSED 2026-08-16** — `composition_deep_trust` proves a two-hop `outer -> middle -> leaf` boundary reaches the root even though `outer` does not name it; narrowing to direct callees is mutation-killed; disclosure occurs at every status rather than only under `proved`. |
| contract-witness precision | **DEFERRED 2026-08-16 to the typed-contract milestone, and REMOVED from R-0004's closure list.** Contract/body witness kinds cannot substitute for each other; a contract request conservatively binds exact implementation identity, which is strictly stronger than the contract question, so nothing is unsound — it is simply not contract composition. Deferred on this roadmap's own dependency order, not on the absence of a corpus case: evidence-affecting language features are frozen until Slice 8 closes, the typed contract substrate lands after R-0004, and binding evidence to today's untyped `requires`/`ensures` encoding would freeze an incomplete internal representation, which the same order refuses. **Tripwire:** `check_dependency_edges.sh` measures the corpus at 0 contract dependency edges (45 body, 1 trusted) and goes RED if one appears, so the deferral cannot go stale silently. |
| friendly claims require valid receipts | **NOT STARTED IN PRODUCTION** — the private structural core now binds dependency root, theorem artifact and trust boundaries in addition to subject/table/environment material, and every field participates in currency. No production-derived successful replay mints, stores or authorizes a receipt; no status consumes one. |
| legacy/schema migration | **MANIFEST CLOSED, REPLAY/MIGRATION NOT STARTED** — `needs_recheck` vocabulary and exact 44-row input exist; no replay-backed V2 rewrite or authoritative activation yet |
| exact migration manifest | **CLOSED 44/44 at this historical snapshot** — compiler emitted one row per source link; 0 unowned, 0 without subject, 0 nondeterministic. **Post-closure cleanup completed 2026-09-02:** the duplicated obsolete ratchet was removed; after deletion of the misattached claim the live registered gate is 12/0 at 43/43. |
| repository replay into receipts | **NOT STARTED** |
| reproducibility and weakening mutations | **PARTIAL** — project/path-independent material is 5/0 with content-mutation non-vacuity; repository-root/project-root, fresh checkout, cache and production receipt replay remain absent. |
| run-completion integrity | **CLOSED 2026-08-16** — the fast suite writes a machine-readable summary with `completed=1`; death before the summary emits a banner and exits 97, distinct from assertion failures, so an absent summary cannot look green. |
| eligibility-denominated coverage baseline | **CLOSED 2026-08-16: 35/91** — every remainder is named and reconciled; the baseline explicitly does not claim kernel replay coverage. |
| adversarial validation of authoritative path (Slice 8) | **HISTORICAL SNAPSHOT: NOT STARTED** — later shipped 24/0 before closure |

This table was the acceptance view on 2026-08-16. It is retained only to explain the path to the
later closure record; its partial states and percentages are not current.

**[historical] Proof-feature dependency fence.** Until that completion gate held, no task
may graduate a new automatically discharged, multi-kernel, certificate-backed,
or otherwise friendly `proved` claim as authoritative. Spikes may run and
representation/lowering infrastructure may land, but their evidence remains
experimental and non-authoritative. R-0450 may land its versioned common IR
without issuing receipts; R-0440 consumes the receipt dimensions; R-0448 is
blocked by R-0004, R-0450, and the required R-0440 fields; and R-0169/R-0170 may
not promote automated verdicts into authoritative claims before this gate.

## Multi-kernel: where the ladder actually stands (2026-08-08)

> **[historical] Reconciled 2026-10-19:** a dated snapshot; the ladder is an evidence vector, not a schedule (R-0440/R-0448). Its live remainders are mapped to owning tasks in the
> [reconciliation record](../project/ROADMAP_RECONCILIATION.md); nothing in it schedules work.

Consolidated because the answer was previously spread across six places in this file, and
"what can the multi-kernel path prove today?" could not be answered without reading all of them.

**Implemented, each with a demo emitting real Lean + Rocq + Isabelle:**

| tier | rung | demo |
|---|---|---|
| boolean postconditions | 2 | `examples/bool_kernel_demo` |
| EUF — uninterpreted spec functions | 5 | `examples/euf_kernel_demo` |
| datatypes + arrays — struct fields, array reads | 6+7 | `examples/datatype_kernel_demo` |
| refinement against a defined spec | — | `examples/refine_kernel_demo` |

`check_bool_kernel.sh` is **62/0/0 under `nix develop .#provers`**. Bare it reports 42/0/8, where
the 8 are absent `coqc`/`isabelle` — quoting that number hides every Rocq and Isabelle assertion
and understates the evidence. Always state which shell produced the figure.

**Missing, in the order they bite:**

1. **The refinement tier is narrower than it looks, and that is H25.** Its substitution is by NAME,
   so binder-bearing spec bodies are REJECTED to prevent capture. A spec body containing a `let`,
   a match, or a conditional binding cannot use this tier at all. Extending the tier before R-0474
   means building on a substitution scheduled for replacement.
2. **`old(...)` is a ceiling, not a feature.** Without it no specification can describe MUTATION,
   so every tier above proves things about pure functions only. That the corpus's real contracts
   are all refinements of pure helpers is not what authors chose — it is the only thing the
   language permits.
3. **rung 3 (nonlinear)** — 2-of-3 kernels at best; Lean has no general nonlinear procedure and
   Mathlib is not available here. H21, upstream.
4. **rung 4 (bitvectors)** — H20, `bv_decide`'s certificate check runs as native code.
5. **rung 8 (induction / `#[decreases]`)** — nothing exists; it is what converts recursion from
   *not eligible* to provable.
6. **rung 9 (full functional correctness)** — nothing exists.

Rungs 8 and 9 are where "prove every kind of code" actually lives. Everything shipped so far is
below them.

**Register C IS discharged, by compile-time rows — an earlier claim here that it "has no row
table" was wrong and is corrected.** C1-C4 live in `Concrete/Report/Evidence.lean` as theorems
(C1 assumption-set union, C2 proved-iff-every-part-proved, C3 non-empty assumptions never present
as a proof class — H23 as a theorem, C4 discharge is the only shrinking operation), with 59
structural assertions in `check_evidence_algebra.sh` and no `sorry`/`admit`/`native_decide`.

The error is worth recording because of how it was made: I grepped `docs/verification/VC_BRIDGE_REGISTER.md`,
found no Register C rows, and reported absence. Register C's rows were never in that file — they
are in the module they constrain, which is the better place for them. **A failed search is not
evidence of absence**, and this is the same mistake pattern the rest of this document warns about,
committed while documenting it.

Register A is the register that remains open in the semantic sense: **0 of 5 families fully
discharged, 4 of 5 half-discharged**. Register B has 1 of 3 rows. C is not pending in the way A
is, and grouping the three as equally-open understates what has been paid for.

## R-0480: physical documentation migration (2026-08-14)

_Moved from the body of [R-0480](../project/ROADMAP_TASKS.md#task-r-0480); the task keeps its objective, current status, exit and a summary of this record._

**Physical migration landed 2026-08-14.** The worktree reached a clean integration boundary, so
the stable documents moved into `language`, `compiler`, `verification`, `platform`, and `project`
families; the existing book/bugs/notes/stdlib trees remained intact. Repository references were
rewritten in the same change, the last root-level research note moved under `research/meta/`, and
`check_doc_links.py` now checks local Markdown destinations on every docs-drift run. Phase A's full
lifecycle-header inventory and Phase B's one-owner/duplicate-current-status controls remain open;
physical neatness is not completion of the authority audit.

## R-0473: the embedded R-0004 Slice-6 record (2026-08-07 – 2026-09-02)

_Moved from the body of [R-0473](../project/ROADMAP_TASKS.md#task-r-0473); the task keeps its objective, current status, exit and a summary of this record._

**STEP 1 CLOSED (2026-08-09).** `check_migration_manifest.sh` reports **44/44 links accounted,
0 unowned, 0 non-deterministic, 0 without a subject** — the closure gate is CLOSED and
dependency-root integration may resume. Both blockers are resolved, and neither was what it first
appeared to be:

1. ~~*8 of 44 stored links have no manifest row.*~~ They were never missing — they were **merged**.
   Rows keyed by callable alone collapsed across files, and `elf_header` ships both `main.con` and
   `main_drifted.con` defining the same callable ids, each with its own stored fingerprint. The
   fix was the field the row schema specified from the start and the gate omitted: **source
   location**. A link key without it cannot distinguish a duplicate from two distinct links that
   happen to name the same callable. Now an equality assertion, not a ratchet.
2. ~~*The subject digest is not a function of the subject alone.*~~ Root cause: `elabFn` did not
   reset the binder frame per function, so lexical scope accumulated across declarations and a
   parameter's resolved index depended on what was elaborated before it. Fixed at the start of
   `elabFn`; the check is an equality assertion.

**Closure gate** (`check_migration_manifest.sh`, separate from the ratchets): required state is
0 missing, 0 unowned, 0 nondeterministic, 0 without subject, 44/44 accounted. It currently reports
**CLOSED**. Preserve this as equality: a ratchet and a completion gate answer different questions,
and a future regression must reopen closure rather than remain green above a lowered floor. The
script cleanup below removes the obsolete duplicate ratchet block; it does not weaken these values.

**The exact migration manifest — authoritative input to Slice 7, CLOSED 2026-08-09.** The compiler
now produces exactly one row per stored source link:

```
source-location | callable-id | stored-v1 | selected-spec-id | current-v2 | disposition
```

The closure gate established 44/44 rows, 0 unowned, 0 links without subjects and 0
nondeterministic keys, covering 33 distinct callable owners. Source location is repository-relative
migration metadata only; it is absent from subject and receipt identity. **Post-closure cleanup
completed 2026-09-02:** the duplicate exact-join/obsolete 35-row ratchet was removed, the parser was
updated to the live V2 subject report, and the live 43-link denominator is shared with the V1 golden
gate rather than restated.

**Slice 6 starting sequence, in order.** Recorded because the order is what keeps a partial
consumer away from the `proved` decision path:

1. ~~implement the Lean-side `MetaM` classification hand-back~~ **DONE 2026-08-09**
   (`classifyOrUnclassified`, `classifyAll`, `renderClassification` — one row per request, an
   unknown name yields `unclassified` rather than a dropped row, serialization sorted so caller
   order cannot enter the bytes);
2. ~~produce complete typed edge material~~ **DONE** — the `unclassified` edge kind exists, with
   completeness proved by `DependencyEdge.mem_all` rather than asserted by a length check;
3. ~~let incomplete/unclassified material REFUSE roots~~ **DONE** — refused at BOTH doors: the
   root via `isCurrentForDependents` (exhaustive, no catch-all, so a future kind is non-current
   by construction) and `mint?` independently, rather than trusting a caller to consult the root
   first. `mergeClassifications` refuses five distinct ways — unanswered, duplicate, unknown,
   still-unclassified, classified-missing — each its own constructor because the repairs differ;
4. ~~wire validated roots into ProofCore and reports **in shadow**~~ **DONE 2026-08-09**.
   `ProofCore.dependencyNodesOf` builds nodes over SEMANTIC IDENTITIES from ProofCore's own
   entries and call graph — one producer, so both consumers read the same nodes rather than each
   rebuilding. `--report subject-facts` carries a sixth axis, `shadow depRoot`. Names are resolved
   to `CallableId` at the boundary and never enter a node; the name-keyed `CallGraph` is thereby
   subordinated rather than propagated.

   **Current: 62 of 64 subjects root, 2 REFUSE** (was 53/11 at first wiring, then 57/7 after the
   hand-back ran, then 62/2 once the generated table covered BOTH proof-attachment paths — the
   investigation narrative is in the commit history; this line is the state) — every refusal naming the
   non-current edge that caused it. That is the expected honest outcome: the compiler cannot mint
   `contract`/`body` without the hand-back, so those edges are `unclassified` and the root refuses
   them. Coverage recovers by classifying more dependencies, never by weakening the root. A gate
   asserts BOTH directions — a leaf must produce a root (or the integration would be
   indistinguishable from not running, and "0 roots" would read as success) and a subject reaching
   an unclassified dependency must refuse — plus that no root reaches `deriveObligationStatus`;
5. ~~compare root determinism and dependency-change sensitivity~~ **DONE 2026-08-09**. Both
   halves gated: discovery ARTEFACTS do not move the root (edge order, node-list order,
   duplicated edges) and dependency SEMANTICS do (a deep callee edit, an edge-KIND change — so
   typing the edges is load-bearing rather than decorative). Trust propagates from a DEEP
   dependency and an untrusted closure does not acquire qualification, which is monotonicity in
   both directions: non-monotone trust would let a claim be laundered clean by adding a
   dependency. A root that moves on order is unusable as a stored value; one that does not move
   on content binds nothing;
6. connect the root to status composition — **BLOCKED on a precondition the sequence did not
   state, found 2026-08-09 before wiring anything.** Every one of the 11 current refusals is an
   `unclassified` edge, and `unclassified` means *we have not asked the Lean side yet*. Gating
   `proved` on the root today would report `depsNotCurrent` for subjects whose dependencies are
   perfectly fine — a claim about OUR unfinished toolchain rendered as a claim about the USER's
   program.

   The invariant says `unclassified` must never enter a CURRENT root, and it does not: the root
   refuses it. It does not follow that a refusal caused by an unrun classifier should degrade a
   verdict. `missing` degrading a verdict is honest — something really is unvalidated.
   `unclassified` degrading one is the compiler blaming a program for the compiler's own state.

   **Hand-back RUN 2026-08-09 and wired** (`scripts/gen/classifications.lean` ->
   `Concrete/Proof/ClassificationTable.lean`, consumed by `dependencyNodesOf`). Refusals dropped
   **11 -> 7** and roots rose 53 -> 57: the classifier answers `body` for every theorem it sees,
   so real classifications do land.

   **The remaining 7 expose a gap in the GENERATOR, not in the classifier.** `main.verify_message`
   is `proved` AND its root refuses, which is exactly the case that must not exist before step 6.
   Its theorem is absent from the table because the generator enumerates
   `Proof.provedFunctions` — the hardcoded list — and misses proofs linked from SOURCE via
   `#[proof_by]`/the registry. The table is therefore complete for one of the two ways a proof
   can be attached.

   **Fixed 2026-08-09** — `scripts/gen/refresh_classifications.sh` unions `provedFunctions` with
   source-linked `#[proof_by]`/`#[ensures_proof]` names (43 rows). Refusals 7 -> 2.

   **THREE FURTHER BLOCKERS before step 6 connects**, from review, all real:

   a. ~~`dependencyNodesOf` silently dropped unresolved callees~~ **FIXED** — it was a
      `filterMap`, so a callee whose identity could not be resolved vanished and the root covered
      LESS than the actual closure while looking complete. Now every call-graph edge is
      represented; an unresolved one becomes an edge to an identity with no node, which the root
      refuses by name. A dependency you cannot resolve is not a dependency you do not have.
   b. **HALF CLOSED.** The table carries the theorem artifact digest, freshness is a MANDATORY
      gate (`make test-classification-freshness`, ~1s), and the gate rediscovers the
      theorem-name inventory from source rather than trusting the generator's own list. ~~Still
      open: the digest does not participate in classification.~~ **CLOSED 2026-08-09** —
      classification routes through `validatedRowOf`, which refuses a row whose digest is absent,
      empty, a placeholder, or not 32 hex chars, and `ValidatedRow`'s constructor is private so a
      caller cannot assemble one around a failed row. Verified end to end: corrupting a digest in
      the table turns that theorem's edge from `body` to `unclassified`.

      **What consumer validation cannot establish**, stated so it is not read as more: the
      compiler cannot recompute a theorem's artifact digest (that needs `MetaM` and the proof
      modules), so the consumer checks STRUCTURE, not correspondence to the live theorem.
      Correspondence stays `check_classification_freshness.sh`'s job, now mandatory. The two
      answer different questions and neither substitutes for the other.

      STILL OPEN: table identities, table digests and quantifies-over-table are discarded — the
      row carries which THEOREM, not yet which DEPENDENCIES. Original finding: Before production it must preserve enough to prove *this
      classification belongs to this exact theorem and applies to these exact dependencies*;
      otherwise a stale or misattached `body`/`contract` label stays structurally valid. OPEN.
   c. **CRITICAL PATH. Correspondence is per-EDGE, not per-theorem.** A partial shape check
      landed (`rowJustifies`: `contract` requires quantification, `body` requires a bound table)
      and it is NECESSARY-not-sufficient — quantification alone does not establish that the
      relevant contract hypothesis exists for a given callee, so an edge passing it may still
      under-bind. It catches the gross failure only.

      The right shape, per review, replaces `classifiedEdgeOf theorem` with something closer to
      `validatedEdgesOf theorem`, returning dependency-specific evidence:

      ```
      EdgeJustification { callee; kind; table?; tableDigest?; witness }
      ```

      The merge must require EXACTLY ONE justified result per compiler-produced call edge and
      refuse: missing or duplicate edges; classifications for unknown callees; callable or
      table-identity mismatches; a `body` edge whose implementation is absent from the bound
      table; a `contract` edge without a corresponding quantified lookup/hypothesis; leftover
      caller-level classifications applied by default; unresolved or ambiguous table entries.

      Conservative rules: `body` only when the bound table contains the exact callee
      implementation (whole-table digest retained for dynamic access); `contract` only when the
      theorem type establishes the relevant contract hypothesis for that callee; `trusted` only
      from the compiler's declared boundary; anything not PROVEN to correspond is `unclassified`
      and refuses the root.

      **PREREQUISITE LANDED 2026-08-09** (`tableEntryEvidence`, `entryEvidenceContains`). Table
      digests cannot prove membership — a digest answers "did this table change", not "does it
      CONTAIN that callee", and one had been standing in for the other. Entry evidence reads what
      `PFnDef` already carries (`identity : PFnIdentity`, `sourceBodyDigest`) rather than
      inventing a parallel record.

      **The field holds the V1 SOURCE-BODY digest, not the V2 subject**, and is now named
      `sourceBodyDigestV1` to say so. It was briefly called `subjectDigest` and documented as
      "which subject it came from" — reinterpreting a V1 body digest as the frozen V2 subject,
      under which a signature or contract change would leave entry evidence looking current and
      the eventual per-edge join would be exact over the WRONG subject identity.

      **RESOLVED 2026-08-11, and the first attempt was keyed on the wrong identity.** Table
      entries bind an IMPLEMENTATION digest, not a proof subject: the frozen V2 subject includes
      selected specification and claim scope, one callable can carry several proof links, so
      `CallableId -> ProofSubjectDigestV2` is not a function and a join on it is ill-defined. An
      entry describes an implementation and does not change when a different spec is pointed at
      it.

      `implementationPreimage` / `implementationDigest` extract that component from the existing
      V2 construction; the subject hashes the PREIMAGE rather than nesting the hash, so every V2
      value stays byte-identical and the schema freeze passes unchanged — which is the proof the
      refactor added and removed nothing. The implementation hash is domain-separated
      (`implementationV1:`) so the two identities cannot collide over the same inputs.

      `bindEntryImplementations` joins against a CLOSED `ImplementationManifest` and refuses the
      whole list when any entry is absent. The earlier version took a
      `CallableId -> Option String` CALLBACK, so any caller could mint a bound entry from any
      non-empty string — the tests passed `"v2:abc"` and demonstrated exactly that. A private
      constructor guarding a caller-supplied value is not a guard.

      `implementationManifestOf` is the authoritative producer, computing each digest from
      complete canonical facts; an entry with incomplete facts or a refused body is ABSENT rather
      than defaulted, so the fail-closed chain runs end to end: undescribable implementation -> no
      manifest row -> no binding -> no `body` justification -> root refuses.

      **STEP 2 IS NOT CLOSED. Three open items, the first an unsoundness:**

      1a. ~~**`tableEntryEvidence` trusts the stored digest.**~~ **FIXED 2026-08-11.**
         It now RECOMPUTES the canonical digest from `PFnDef.body` and refuses the whole table
         unless the stored provenance agrees, with schema and scope checked first (a digest under
         another schema is a different formula, so it is refused rather than compared). The
         acceptance mutation — replace the body, keep the identity AND the stored digest — is
         killed and registered as mutation family `entry-body-recomputed`.

         `TableEntryEvidence.sourceBodyDigestV1` is now `String`, not `Option String`: an entry
         with no provenance, and one whose provenance was never checked against a body, both lose
         their representation.

         **Two things measured rather than assumed**, because either would have made it vacuous:
         the 24 hand-authored `sourceBodyDigest` literals in `Proof.lean` AGREE with the recompute,
         so the producer matches the real corpus instead of refusing all of it; and the positive
         controls yield evidence, so the refusals do not hold merely because nothing can.

         The enabling refactor took SEVEN declarations, not the five predicted — `stripAlpha` and
         `byteToHex` surfaced only by rebuilding after each step — and landed in a new
         `BodyIdentity` module (plus a general `Digest`) rather than `Proof.lean`, because `ProofCore` is `namespace
         Concrete` while `Proof.lean` is `namespace Concrete.Proof` and moving there would have
         renamed every call site.

      1. ~~**The join matches `CallableId` ALONE.**~~ **FIXED 2026-08-11.** Manifest rows carry
         `(callable, authoritative source-body digest, implementation digest)`, and binding now
         requires the entry's stored body digest to be PRESENT and EQUAL to the authoritative one.
         The comparable value turned out to be reachable after all: `ProofCoreEntry.extracted`
         holds the `PExpr`, so `shortHash (pexprCanonical (normalizePExpr pe))` computes the same
         value Report's `sourceBodyDigestV1` does. Three acceptance legs: a STALE body digest
         refuses, a MISSING one refuses (absence is not agreement), a MATCHING one binds.

         **TWO LIMITS, both explicit.** (i) The digest formula is now DUPLICATED — Report computes
         it and so does `implementationManifestOf`. One canonical producer in a neutral module
         imported by both is the durable shape; two copies can drift, and the whole point of the
         comparison is that both sides mean the same thing. (ii) Matching V1 source-body digests
         establishes ARTIFACT binding and freshness, **not semantic equivalence** between the
         source body and the `PFnDef`. That remains the source-to-proof-model faithfulness
         boundary, and this repair must not be read as a claim that two representations have
         proved-equal semantics.

      1b. **Superseded description of the original hole:** `bindEntryImplementations` attaches the
         manifest's implementation digest without checking that the entry's stored body is the one
         the manifest describes, and `TableEntryEvidence.sourceBodyDigestV1` is ignored — a test
         with `sourceBodyDigestV1 := none` binds successfully, which demonstrates it. So a stale
         or substituted table entry carrying the right identity ACQUIRES the current authoritative
         digest. That is the misattachment blocker (c) exists to kill.

         It cannot be fixed where the producer lives, for a concrete reason: the manifest digest
         is built from `bodyBytesV2`, the entry field holds `sourceBodyDigestV1`, and `ProofCore`
         records NO source-body digest at all — `renderSourceBodyDigest` is in Report. There is
         nothing in the producer's reach to compare against. Closing it needs the authoritative
         body component carried where the manifest is built; binding then requires identities
         equal AND the entry's body digest PRESENT AND equal to that component.

         **The acceptance mutation:** replace a table entry's body while preserving its
         `CallableId`. Binding must refuse. Today it succeeds.

      2. **`ofRows` validates FORMAT only and is public** — thirty-two zeros mints a manifest.

         **NOT actually blocked — measured 2026-08-11, and the earlier "cannot be made private
         while the producer is cross-module" was wrong.** The fix does not need a correspondence
         module importing ProofCore. It needs the digest COMPUTATION below `DependencyEdge`, and
         everything it depends on already is:

         - `implementationPreimage`/`implementationDigest` need only `Proof.CheckedDeclFacts`,
           `Proof.CompleteEvidenceBodyV2`, `Proof.bodyBytesV2` and `shortHash`;
         - those live in `SubjectFacts`, `EvidenceTree`/`IdentityUseBytes`, and (since the
           `BodyIdentity`/`Digest` extraction) below `DependencyEdge`;
         - **none of `SubjectFacts`, `EvidenceTree` or `IdentityUseBytes` mentions
           `DependencyEdge`** — verified by grep, so `DependencyEdge` can import that chain with
           no cycle.

         **The shape that closes it**, rather than merely narrowing it: move those two into a
         module `DependencyEdge` imports, then replace the caller-supplied-digest entry point with
         one taking `(CallableId × CheckedDeclFacts × CompleteEvidenceBodyV2)` and computing both
         digests itself. A caller then has nowhere to put invented hex — the guard becomes the
         type rather than a format check, which is the same move that fixed `TableEntryEvidence`.
         `implementationManifestOf` can stay in `ProofCore`, since only the computation has to
         move down, not the builder that walks a `ProofCore`.

         Keep the format validation as well: it still rejects a malformed digest from the
         authoritative producer, which is a different failure from a forged one.

         **The shape, refined 2026-08-11.** Do NOT expose a replacement constructor taking bare
         `CheckedDeclFacts` and `CompleteEvidenceBodyV2`: those prove completeness, but a caller
         could pair one callable's facts or body with a different `CallableId`. Take a closed
         record instead, so the three cannot be mixed independently:

         ```lean
         structure CompleteImplementation where
           private mk ::
           callable : CallableId
           facts    : Proof.CheckedDeclFacts
           body     : Proof.CompleteEvidenceBodyV2
         ```

         then `ImplementationManifest.ofImplementations : List CompleteImplementation -> Option
         ImplementationManifest`, computing both digests internally. Raw
         `(CallableId x String x String)` construction becomes private or test-only.

         **What the smart constructor can and CANNOT check — measured, so no control is claimed
         that cannot be delivered.** `implementationIdentityOf` now sits below `DependencyEdge`
         (landed 2026-08-11), so the constructor can live there and verify:

         - `facts.id == callable` — checkable, `CheckedDeclFacts.id : CallableId`;
         - facts complete — checkable;
         - the evidence body validates — checkable.

         **"Both originated from the same `ProofCoreEntry`" is NOT checkable there**, twice over.
         `ProofCoreEntry` lives in `ProofCore`, ABOVE this module, so a `private mk` in the low
         module cannot be reached by a producer that walks entries — the constructor has to be a
         validating function taking the three parts. And `CompleteEvidenceBodyV2` carries only
         `val.statements`, **no identity**, so pairing entry A's BODY with entry B's facts and id
         passes every check above and silently yields a wrong digest. A mutation for that swap
         would SURVIVE, and must not be registered as if it were killed.

         Closing it properly needs the body to carry its owning `CallableId`, which is a V2 schema
         change and must not be done casually while the freeze holds. Until then this is a NAMED
         REFUSAL, not a covered case, and the id/facts pairing mutation (which the `facts.id ==
         callable` check does kill) must not be described as covering it.

      2a. **Manifest completeness must be self-denominating** — `implementationManifestOf` uses
         `filterMap`, which launders an incomplete production into a smaller apparently-complete
         manifest. Prefer a result carrying NAMED refusals:

         ```lean
         structure ManifestResult where
           expected : List CallableId
           rows     : List ManifestRow
           refusals : List (CallableId x ManifestRefusal)
         ```

         A manifest usable for correspondence exists only when: expected identities = row
         identities, `refusals = []`, and no duplicates. The denominator is then stored rather
         than inferred from the numerator, which is the defect `filterMap` has.

      3. **`implementationManifestOf` uses `filterMap`**, so the manifest is neither complete nor
         self-denominating. The downstream join refuses a missing callable, which is fail-closed
         for that use, but the manifest must not be called complete without either retaining
         named refusal rows or storing the expected eligible identity set and proving every
         identity received exactly one digest.

      Body, capability and generic sensitivity controls are also still owed — only signature and
      contract sensitivity are demonstrated.

      DUPLICATE IDENTITIES REFUSED: a table holding one callable twice cannot say which
      implementation a static lookup selects, and membership answering "at least one" would let a
      `body` edge be justified by an entry dispatch never reaches.

      ALL-OR-NOTHING: one legacy entry refuses evidence for the whole table, because a partial
      membership list answers "is this callee present" with "not in the part I could read" —
      indistinguishable from "absent", and absence is what justifies refusing an edge. Membership
      is decided by IDENTITY, never by display name, gated with a same-named callable from
      another module. Whole-table digests remain necessary and are not replaced: a dynamic index
      can reach any entry, so entry evidence answers the statically-known case and cannot narrow
      the dynamic one.

      **Acceptance is stronger than 64/64:** every actual call edge has exactly one validated
      justification, and removing, swapping, or weakening any justification makes the
      corresponding root refuse. Only once that mutation gate holds — and `calls.combine` closes
      for the right reason — is connecting roots to verdict composition defensible.

      Original finding: A `contract` theorem does not thereby establish that every call-graph edge is
      covered by an appropriate contract hypothesis — over-approximating is safe, but `contract`
      could UNDER-bind implementation dependencies unless the theorem's hypothesis structure is
      matched to the emitted edges. OPEN.

   And the precondition itself is still unmet by one case: `calls.combine` is `proved` with a
   refusing root. Step 6 connects only when that count is zero.

   **So step 6 waits for the hand-back to RUN over the real proof corpus**, producing genuine
   `contract`/`body` classifications, not merely for the merge machinery to exist (steps 1-3,
   done). Once real classifications land, the remaining refusals will be `missing` — and those
   SHOULD gate `proved`. The step-4 measurement (53 of 64 rooting, 11 refusing, all
   `unclassified`) is the baseline that says when this precondition is met: it is met when the
   refusal REASONS change, not when the count drops;
7. kill the mutation that omits the root while attempting `proved`;
8. delete or permanently subordinate the old advisory/name-keyed path.

**Mass refusal at the first honest integration is expected EVIDENCE, not a regression to hide.**
Coverage recovers only by classifying more dependencies — never by weakening root completeness.
That sentence is here because the pressure at step 3 will be to relax the refusal so the numbers
look better, and relaxing it converts a fail-closed root into an advisory one, which is the whole
defect class R-0004 exists to close.

**Slice 6 acceptance boundary (recorded 2026-08-09, before starting).** Integration is the
highest-risk remaining step, so its done-condition is written first rather than discovered:

* ProofCore AND reports consume the SAME validated root — two consumers building their own is how
  a second, weaker answer appears;
* direct, transitive and recursive dependencies included;
* dynamic table access binds the WHOLE table (`tableValueDigest` already does this, and
  `tablesFullyBound` is the predicate a root must respect);
* missing, unclassified or unbindable edges FAIL CLOSED — `dependencyRootMaterial` already
  refuses these, and integration must not add a path around it;
* trust and assumptions propagate MONOTONICALLY;
* dependency ORDER and duplicate discovery do not change the root; any dependency SEMANTIC change
  does; comments, paths, import order and equivalent traversal order do not;
* no legacy `staleDeps`, name-keyed, or advisory path can issue `proved`;
* a production-consumer MUTATION proves that omitting the root prevents a friendly verdict —
  without it, "integrated" means the code is called, not that it decides anything.

**Known shape of the first integration, from reading `DepNode`.** Nodes carry
`(id : CallableId, digest : Option String, edges : List (DependencyEdge × CallableId))`. The
compiler can honestly mint `trusted` and `missing` edges; the `contract` vs `body` split needs
`classifyTheorem`, which runs in `MetaM` on the Lean side. Since `dependencyRootMaterial` fails
closed on a `missing` edge, the first honest integration will REFUSE most roots — that is the
correct outcome and must not be softened into an advisory value to make coverage look better.

**Boundary between the three digests, recorded 2026-08-09 so the subject does not accrete.** It
was tempting to fold dependency material and trust qualification into `ProofSubjectDigest`; they
belong elsewhere, and keeping them out is what lets the subject be *frozen* independently:

* **subject digest** — semantic identity, typed signature, structural V2 body, contracts,
  selected specification, claim scope. What the proof is ABOUT.
* **dependency root** — transitive dependency material, computed FROM finished subject digests.
  Depends on the subject, so it cannot be inside it.
* **receipt** — dependency root, trust and assumptions, theorem/replay/environment identities.
  What the evidence RESTS ON, and the only layer that qualifies a claim.

Order that follows from it: structural body (done) -> selected-spec identity -> claim scope ->
freeze/version the subject schema -> integrate dependency roots -> environment identities and
complete receipts -> replay and migrate. Receipts must not be minted nor fingerprints migrated
before the freeze.

## R-0475: exposure audit and shipped snapshot fix (2026-08-07)

_Moved from the body of [R-0475](../project/ROADMAP_TASKS.md#task-r-0475); the task keeps its objective, current status, exit and a summary of this record._

**Exposure audited 2026-08-07, and it is exactly one snapshot.** The audit is recorded because
"the other project-mode fixtures probably have this too" was the natural assumption and it was
wrong:

* 49 examples carry a `Concrete.toml` and therefore build against the stdlib;
* `scripts/tests/phase1_snapshots` is the only golden directory, and of its 13 snapshotted
  fixtures exactly **one** — `assume_taint` — is project-mode. The other 12 are single-file and
  cannot see stdlib contracts at all;
* the other 2 project-mode contract fixtures (`assume_scope_adversarial`, `obligation_redteam`)
  are used only by gates making targeted assertions, with zero references to `cmp`/`diff`/golden
  files, so a stdlib addition cannot perturb them. All four such gates pass (8/0, 21/0, 9/0, 16/0);
* the 3 remaining golden-comparing gates that touch project-mode examples (`check_concrete_fmt`,
  `check_httpget_differential`, `check_tcpserve`) compare *program* output — formatted source,
  HTTP bytes, a server response body — not a report, so contracts are not in their basis.

So the coupling had one instance, it is the one H26 found, and the attribution diagnostic added in
`1c310a07` covers the case where a future fixture gains a `Concrete.toml`.

**Durable fix landed** (`31710d4e`): the contracts comparison is scoped to modules the fixture
defines, so a project-mode golden cannot depend on the stdlib. Comparison only — the report is
unchanged, so nothing a user sees is lost. `assume_taint`'s golden went 153 -> 32 lines with zero
stdlib entries. The invariant ("a project-mode golden contains no entry from a module it does not
define") is asserted on every run rather than registered as a mutation, because that harness
proves a gate goes RED when a rule is disabled and this property is a gate staying GREEN under a
change that is none of its business. Verified both ways: the H26 mutation no longer moves the
golden, and appending a `sha256.` entry makes the assertion fail.
