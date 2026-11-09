# ISS-0049: one fuel-indexed driver

Design only; verified against tracked files at `3b4d01d6fee3910436eb7b63e49847f1e734a3d3`. No implementation is authorized by this note. The architectural criterion is one authoritative fact with checked views (docs/PHILOSOPHY.md:68-73).

Non-vacuity convention: each proposed law is a schema quantified over the named executable/predicate subject, with that subject occurring in its conclusion. Instantiate it with the intended subject and with a mutant of exactly the same type, at identical inputs and identical premises. A negative control must falsify the conclusion, never manufacture an inconsistent premise or assume the desired output. Closed cases below are proposed proof obligations, not claims of executed Lean checks. Preservation of an existing theorem means its full quantified proposition, including refusal and hostile-input cases, survives; proof scripts may change.

## Pair and decision: VIEW

ConcreteSpec/Spec/Exec.lean:842-866 defines Report and runFrom; ConcreteSpec/Spec/Prim.lean:407-432 defines Ghost.SegmentReport and drive. Both dispatch finality, zero budget, step, oracle, contract and continuation identically. Ghost alone retains residual config and fuel. That metadata is needed by runStmts/runPieces; ordinary Report is a real abstraction hiding composition state.

Exec owns a `DriverReport O` with events, ending, config, remaining, and a single recursive `driveFrom` with the existing Ghost.drive signature after namespace changes. Move the authoritative implementation there. Define `DriverReport.report` by projecting events/ending. Keep Exec.Report unchanged; `runFrom P ρ n h c := (driveFrom P ρ n h c).report`. Keep Ghost.SegmentReport as an abbreviation of DriverReport and Ghost.drive as an abbreviation of driveFrom for API compatibility. There is no second recursion. Ordinary and ghost report views have distinct purposes: public execution observation versus resumable composition.

## Laws and discriminators

D's exact type is `(P : Exec.Program O) → Exec.Oracle O → Nat → List O.Event → Exec.Config O → Exec.DriverReport O`. Give D a single characteristic equation equal to the complete dispatch at Prim:414-432, with recursive calls to D. Subjects D occur on both sides. This equation must include all four fields, not just events/ending. Prove uniqueness by fuel induction (recursive calls consume one fuel); it fixes behavior on every branch without an enduring reference driver.

Closed test fixtures use Exec.Demo.ops, a program containing no declarations for final/silent fixtures and explicit primitive fixtures for requests; use Demo's actual program/statement constructors when implementing. Required inputs and same-type mutants for the characteristic equation:

| Closed input | Required result | Mutant of D, same inputs |
|---|---|---|
| final normal config, store .init, fuel 0, hist [] | final normal .init, same config, remaining 0 | test budget before finality |
| same final config, fuel 3 | remaining 3 | zero unused fuel |
| nonfinal skip config, fuel 0 | outOfFuel, unchanged config/hist | step at zero fuel |
| skip config, fuel 1 | silent step to final, remaining 0 | charge two fuel or retain pre-step config |
| pending log request, fuel 2, oracle always none | noAnswer, unchanged config, remaining 2 | classify absent answer as stuck |
| request with a concrete forbidden answer | badAnswer, no appended event, fuel 2 | bypass allows |
| allowed answer, continuation none | stuck at original config, fuel 2 | consume fuel on rejected continuation |
| allowed answer emitting 7, continuation final | hist [3] becomes [3,7], residual fuel 1 | omit/reverse/duplicate event |
| internal allowed answer with event none, continuation final | hist [3] unchanged, fuel 1 | fabricate event |
| nonfinal configuration with step none | stuck, unchanged config, full budget | classify stuck as noAnswer |

Exact closed fixtures (open Exec and Exec.Demo): `P : Program ops := ⟨[]⟩`, `cFinal : Config ops := ⟨.done, [], .init, []⟩`, `cSkip := ⟨.exec .skip, [], .init, []⟩`, `cLog := ⟨.exec (.prim none .log [.lit 7] s), [], .init, []⟩`, `cBump := ⟨.exec (.prim none .bump [] s), [], .init, []⟩`, `cStuck := ⟨.exec (.assign x (.lit 0) s), [], .init, []⟩`. Use `env` for successful answers, `fun _ _ _ _ _ => none` for missing answers and `fun _ _ _ _ st => some ⟨some 99, .ok 0, st⟩` for a bad log answer. For the absent continuation use the already closed `Observe.refusedRequest`, `Observe.refusingBindOps`, `P := ⟨[]⟩` and Demo.env (ConcreteSpec/Spec/Observe.lean:814-828): its allowed log response is refused by lifecycle binding. No synthetic operand language is needed. The log and bump instructions have no destination, so successful responses continue to done. These fixtures cover the table above at fuel 2 and initial hist [3]; final/skip fixtures use the listed budgets. Mutants change the specified branch only, retain D's complete dependent signature, and return correctly typed records. Reachability is checked by reducing step on these records, not assumed from a label.

Projection laws: `∀ P ρ n h c, runFrom P ρ n h c = (D P ρ n h c).report` and `Ghost.drive P ρ n h c = D P ρ n h c`. Subjects runFrom/Ghost.drive/D appear in conclusions. Closed final config at fuel 3 refutes a same-signature Ghost.drive mutant zeroing remaining; closed emitting request refutes a runFrom mutant dropping events. A stuck/noAnswer classifier mutant also breaks ordinary projection. Neither law assumes a particular resulting report.

Retain path law `∀ P ρ n h c, ∃ tr, (D P ρ n h c).events = h ++ tr ∧ Exec.Steps P c tr (D P ρ n h c).config`. Closed skip with fuel 1; same-type mutant that reports a different unreachable config is rejected. This law alone does not establish ending/fuel equality, which is why the characteristic equation is required. Closed event case above refutes history reversal while keeping the program/oracle fixed.

## Existing strength and order

Exec.runFrom, run, runFn keep their types and propositions exactly; only unfolding proofs change. Keep drive_steps_result (Prim proofs:299-304), runStmts_of_not_normal, runStmts_terminal, ghost_erasure_run and ghost_erasure_ending verbatim, including residual config/fuel equalities. All runFrom erasure, adequacy, physical-output, terminal, refusal and observation theorems in the inventory retain complete statements. Prove characteristic equations for projected runFrom matching its old equations before migrating proofs; use these equations instead of relying on definitional recursion. Do not add constraints on oracles, programs, answers or well-formed inputs.

Order: DriverReport/driveFrom in Spec/Exec; projection and characteristic equations in Proofs/Exec; Ghost aliases in Spec/Prim; residual path proof in Proofs/Prim derived from the Exec driver; then all enumerated downstream proofs. Keep the old algorithm only in the transient working diff to compare all branches, delete it before committing implementation. No independent production reference interpreter remains.

## QUESTIONS

No execution-policy question is decided. If equality of old dispatch results fails, stop and present the concrete discrepancy to the owner rather than selecting an ending, refusal state or identity policy. ISS-0046 and ISS-0048 remain outside scope.

## Verified user and affected declaration inventory

Repository-wide tracked-text search at the SHA above, excluding historical legacy/, task material, issue rows and proposal notes. Includes declarations, qualified and unqualified references, proof unfolding, examples, comments and documentation; broad matches intentionally retain dependent users rather than losing namespace-local calls. No production user is excluded by a hand-picked file list. External Lean library internals are not repository users.

Search expression: `\b(runFrom|SegmentReport|drive|runStmts|runPieces|Report)\b`. Reproduce with `git grep -n -E` over the tracked nonhistorical sources (the expression uses standard word boundaries).

Every matching use (path:line):

```text
.claude/codex-review-prompt.md:5: YOU ARE A REVIEWER. Do not modify any file. Report findings only.
.claude/commands/creview.md:33: Report: the rounds run, each round's findings with the fix commit or the rejection reason, the
.claude/commands/cstatus.md:2: description: Report where this worktree, its branch, its CI and this machine stand (read-only)
.claude/commands/cstatus.md:5: Report status. Read-only: change nothing, kill nothing.
.claude/commands/cstatus.md:22: Report in this order: the candidate being validated (branch, SHA, checks and CI state), independent
.claude/commands/rebase.md:27: Report: old and new head SHAs, the conflicts and how each was resolved, the checks re-run.
.claude/commands/roadmap-item.md:31: Report: the slice delivered, what remains of the task, the head SHA, the checks and their results,
ConcreteSpec/Proofs/Adequacy.lean:246: (runFromQ P ρ₀.toQueryOracle fuel events issued c).toReport = runFrom P ρ₀ fuel events c := by
ConcreteSpec/Proofs/Adequacy.lean:251: cases h : c.final? <;> simp [runFromQ, runFrom, h, QueryReport.toReport]
ConcreteSpec/Proofs/Adequacy.lean:255: | some f => simp [runFromQ, runFrom, hf, QueryReport.toReport]
ConcreteSpec/Proofs/Adequacy.lean:258: | none => simp [runFromQ, runFrom, hf, hs, QueryReport.toReport]
ConcreteSpec/Proofs/Adequacy.lean:261: | next c' => simpa [runFromQ, runFrom, hf, hs] using ih events issued c'
ConcreteSpec/Proofs/Adequacy.lean:266: simp [runFromQ, runFrom, hf, hs, answer, Oracle.toQueryOracle, Query.issue, ha,
ConcreteSpec/Proofs/Adequacy.lean:271: | false => simp [runFromQ, runFrom, hf, hs, answer, Oracle.toQueryOracle, Query.issue,
ConcreteSpec/Proofs/Adequacy.lean:275: | none => simp [runFromQ, runFrom, hf, hs, answer, Oracle.toQueryOracle, Query.issue,
ConcreteSpec/Proofs/Adequacy.lean:278: simpa [runFromQ, runFrom, hf, hs, answer, Oracle.toQueryOracle, Query.issue, ha,
ConcreteSpec/Proofs/Adequacy.lean:617: **Soundness for C1's `runFrom`.** Query erasure preserves the finite execution and its ending
ConcreteSpec/Proofs/Adequacy.lean:621: (runFrom P ρ₀ fuel events c).events = pos.events ∧
ConcreteSpec/Proofs/Adequacy.lean:622: Conclusion P ρ₀.toQueryOracle (runFrom P ρ₀ fuel events c).ending pos := by
ConcreteSpec/Proofs/Adequacy.lean:626: have he' : (runFrom P ρ₀ fuel events c).events =
ConcreteSpec/Proofs/Adequacy.lean:628: simpa [QueryReport.toReport] using congrArg Report.events hreport.symm
ConcreteSpec/Proofs/Adequacy.lean:629: have hend : (runFrom P ρ₀ fuel events c).ending =
ConcreteSpec/Proofs/Adequacy.lean:631: simpa [QueryReport.toReport] using congrArg Report.ending hreport.symm
ConcreteSpec/Proofs/Adequacy.lean:636: /-- Helper (computation). Every report extends the event history supplied to `runFrom`. (approved:
ConcreteSpec/Proofs/Adequacy.lean:639: ∃ tr, (runFrom P ρ₀ fuel events c).events = events ++ tr := by
ConcreteSpec/Proofs/Adequacy.lean:665: **Completeness for C1's `runFrom`.** Every finite execution from an empty query history to a
ConcreteSpec/Proofs/Adequacy.lean:670: runFrom P ρ₀ n events c = ⟨pos'.events, .final f pos'.config.store⟩ := by
ConcreteSpec/Proofs/Adequacy.lean:676: /-- Law (subject `runFrom`).
ConcreteSpec/Proofs/Adequacy.lean:680: (∀ c', (runFrom P ρ₀ n events c).ending ≠ .outOfFuel c') →
ConcreteSpec/Proofs/Adequacy.lean:681: runFrom P ρ₀ m events c = runFrom P ρ₀ n events c := by
ConcreteSpec/Proofs/Adequacy.lean:687: | some f => cases m <;> simp [runFrom, hf]
ConcreteSpec/Proofs/Adequacy.lean:688: | none => exact (hnot c (by simp [runFrom, hf])).elim
ConcreteSpec/Proofs/Adequacy.lean:696: | some f => simp [runFrom, hf]
ConcreteSpec/Proofs/Adequacy.lean:699: | none => simp [runFrom, hf, hs]
ConcreteSpec/Proofs/Adequacy.lean:703: have hnot' : ∀ c₀, (runFrom P ρ₀ n events c').ending ≠ .outOfFuel c₀ := by
ConcreteSpec/Proofs/Adequacy.lean:704: simpa [runFrom, hf, hs] using hnot
ConcreteSpec/Proofs/Adequacy.lean:705: simpa [runFrom, hf, hs] using ih m hle' events c' hnot'
ConcreteSpec/Proofs/Adequacy.lean:708: | none => simp [runFrom, hf, hs, ha]
ConcreteSpec/Proofs/Adequacy.lean:711: | false => simp [runFrom, hf, hs, ha, hallow]
ConcreteSpec/Proofs/Adequacy.lean:714: | none => simp [runFrom, hf, hs, ha, hallow, hk]
ConcreteSpec/Proofs/Adequacy.lean:717: (runFrom P ρ₀ n (events ++ a.event.toList) c').ending ≠ .outOfFuel c₀ := by
ConcreteSpec/Proofs/Adequacy.lean:718: simpa [runFrom, hf, hs, ha, hallow, hk] using hnot
ConcreteSpec/Proofs/Adequacy.lean:719: simpa [runFrom, hf, hs, ha, hallow, hk] using
ConcreteSpec/Proofs/Adequacy.lean:729: runFrom P ρ₀ m events c = ⟨pos'.events, .final f pos'.config.store⟩ := by
ConcreteSpec/Proofs/Adequacy.lean:731: have hnot : ∀ c', (runFrom P ρ₀ n events c).ending ≠ .outOfFuel c' := by
ConcreteSpec/Proofs/Adequacy.lean:737: /-- Law (subject `runFrom`).
ConcreteSpec/Proofs/Adequacy.lean:741: (runFrom P ρ₀ n events c).events <+: (runFrom P ρ₀ m events c).events := by
ConcreteSpec/Proofs/Adequacy.lean:748: have h0 : (runFrom P ρ₀ 0 events c).events = events := by
ConcreteSpec/Proofs/Adequacy.lean:749: cases hf : c.final? <;> simp [runFrom, hf]
ConcreteSpec/Proofs/Adequacy.lean:759: | some f => simp [runFrom, hf]
ConcreteSpec/Proofs/Adequacy.lean:762: | none => simp [runFrom, hf, hs]
ConcreteSpec/Proofs/Adequacy.lean:765: | next c' => simpa [runFrom, hf, hs] using ih m hle' events c'
ConcreteSpec/Proofs/Adequacy.lean:768: | none => simp [runFrom, hf, hs, ha]
ConcreteSpec/Proofs/Adequacy.lean:771: | false => simp [runFrom, hf, hs, ha, hallow]
ConcreteSpec/Proofs/Adequacy.lean:774: | none => simp [runFrom, hf, hs, ha, hallow, hk]
ConcreteSpec/Proofs/Adequacy.lean:775: | some c' => simpa [runFrom, hf, hs, ha, hallow, hk] using
ConcreteSpec/Proofs/Adequacy.lean:781: (runFrom P ρ₀ n events c).events <+: (runFrom P ρ₀ (n + 1) events c).events :=
ConcreteSpec/Proofs/Adequacy.lean:788: {f : Final O.Val} {finalStore : O.Store} (h : (runFrom P ρ₀ fuel events c).ending = .final f finalStore) :
ConcreteSpec/Proofs/Adequacy.lean:790: (runFrom P ρ₀ fuel events c).events = events ++ tr := by
ConcreteSpec/Proofs/Adequacy.lean:1592: change (runFrom P ρ₀ fuel [] c).ending = .final f finalStore at h
ConcreteSpec/Proofs/Adequacy.lean:1608: change (runFrom P ρ₀ fuel [] c).ending = .noAnswer blocked at h
ConcreteSpec/Proofs/Adequacy.lean:1619: change (runFrom P ρ₀ fuel [] c).ending = .noAnswer blocked at h
ConcreteSpec/Proofs/Adequacy.lean:1626: exact (congrArg Report.ending hm).trans h
ConcreteSpec/Proofs/Adequacy.lean:1643: exact (congrArg Report.ending hmono).trans hfinal
ConcreteSpec/Proofs/Adequacy.lean:2800: runFrom prog observationEnv 1 [] terminalConfig :=
ConcreteSpec/Proofs/Adequacy.lean:2806: runFrom prog observationEnv 1 [] terminalConfig := by
ConcreteSpec/Proofs/Adequacy.lean:2808: have he := congrArg Report.events h
ConcreteSpec/Proofs/Adequacy.lean:2906: (c : Config O) : Report O :=
ConcreteSpec/Proofs/Adequacy.lean:2937: /-- A fuel-sensitive evaluator defect that agrees with `runFrom` up to fuel `1`, but drops
ConcreteSpec/Proofs/Adequacy.lean:2940: (events : List O.Event) (c : Config O) : Report O :=
ConcreteSpec/Proofs/Adequacy.lean:2941: if fuel ≤ 1 then runFrom P ρ fuel events c
ConcreteSpec/Proofs/Adequacy.lean:2942: else ⟨[], (runFrom P ρ fuel events c).ending⟩
ConcreteSpec/Proofs/Adequacy.lean:2945: example : runFrom prog observationEnv 2 [] terminalConfig = runFrom prog observationEnv 1 [] terminalConfig :=
ConcreteSpec/Proofs/Adequacy.lean:2950: subject `runFrom`, mutant `extraFuelDropRunFrom`). -/
ConcreteSpec/Proofs/Adequacy.lean:2959: have he := congrArg Report.events h
ConcreteSpec/Proofs/Adequacy.lean:2963: example : (runFrom prog observationEnv 1 [] terminalConfig).events <+:
ConcreteSpec/Proofs/Adequacy.lean:2964: (runFrom prog observationEnv 2 [] terminalConfig).events :=
ConcreteSpec/Proofs/Adequacy.lean:2968: subject `runFrom`, mutant `extraFuelDropRunFrom`). -/
ConcreteSpec/Proofs/Adequacy.lean:3078: (c : Config O) : Report O :=
ConcreteSpec/Proofs/Exec.lean:2047: runFrom P ρ 0 hist' c' ≠ ⟨hist, .stuck c⟩ := by
ConcreteSpec/Proofs/Exec.lean:2049: cases hf : c'.final? <;> simp [runFrom, hf] at h
ConcreteSpec/Proofs/Exec.lean:2051: /-- Law (subject `ConcreteSpec.Core.Exec.runFrom`). At a primitive request with an allowed
ConcreteSpec/Proofs/Exec.lean:2060: runFrom P ρ 1 hist c = ⟨hist, .stuck c⟩ ↔ cont a = none := by
ConcreteSpec/Proofs/Exec.lean:2063: simp [runFrom, hf, hr, ho, ha, hk]
ConcreteSpec/Proofs/Exec.lean:2067: simp only [runFrom, hf, hr, ho, ha, ↓reduceIte, hk] at h
ConcreteSpec/Proofs/Exec.lean:2071: /-- Law (subject `ConcreteSpec.Core.Exec.runFrom`). A one-step run from a primitive call with a
ConcreteSpec/Proofs/Exec.lean:2081: runFrom P ρ 1 hist ⟨.exec (.prim (some x) p args site), L, st, k⟩ =
ConcreteSpec/Proofs/Exec.lean:2970: (st : O.Store) : Option (Report O) :=
ConcreteSpec/Proofs/Exec.lean:2976: (_ : O.Store) : Option (Report O) :=
ConcreteSpec/Proofs/Exec.lean:3295: /-- Planted mutant of `runFrom`: commits an allowed answer's event and finishes even when
ConcreteSpec/Proofs/Exec.lean:3298: (c : Config O) : Report O :=
ConcreteSpec/Proofs/Exec.lean:3305: | some (.next c') => runFrom P ρ n hist c'
ConcreteSpec/Proofs/Exec.lean:3312: | some c' => runFrom P ρ n (hist ++ a.event.toList) c'
ConcreteSpec/Proofs/Exec.lean:3317: /-- Planted mutant of `runFrom`: always reports `.stuck c` without executing the continuation. -/
ConcreteSpec/Proofs/Exec.lean:3319: (c : Config O) : Report O :=
ConcreteSpec/Proofs/Exec.lean:3324: example : runFrom prog Demo.env 1 [9] ⟨.exec (.prim (some x) .log [.lit 1] s), [], .init, []⟩ =
ConcreteSpec/Proofs/Exec.lean:3331: /-- Planted (law `ConcreteSpec.Core.Exec.continuation_refused`, case `refused`, subject `ConcreteSpec.Core.Exec.runFrom`, mutant `ConcreteSpec.Core.Exec.Refusal.bypassRunFrom`).
ConcreteSpec/Proofs/Exec.lean:3343: have hev := congrArg Report.events hrep
ConcreteSpec/Proofs/Exec.lean:3349: example : runFrom Demo.prog Demo.env 1 [9] ⟨.exec (.prim (some x) .log [.lit 1] s), [], .init, []⟩ =
ConcreteSpec/Proofs/Exec.lean:3356: /-- Planted (law `ConcreteSpec.Core.Exec.continuation_refused`, case `accepted`, subject `ConcreteSpec.Core.Exec.runFrom`, mutant `ConcreteSpec.Core.Exec.Refusal.stuckRunFrom`).
ConcreteSpec/Proofs/Exec.lean:3372: example : runFrom prog Demo.env 1 [9] ⟨.exec (.prim (some x) .log [.lit 1] s), [], .init, []⟩ =
ConcreteSpec/Proofs/Exec.lean:3378: /-- Planted (law `ConcreteSpec.Core.Exec.prim_bind_refused`, case `refused`, subject `ConcreteSpec.Core.Exec.runFrom`, mutant `ConcreteSpec.Core.Exec.Refusal.bypassRunFrom`).
ConcreteSpec/Proofs/Exec.lean:3389: have hev := congrArg Report.events hrep
ConcreteSpec/Proofs/Exec.lean:3395: example : runFrom Demo.prog Demo.env 1 [9] ⟨.exec (.prim (some x) .log [.lit 1] s), [], .init, []⟩ =
ConcreteSpec/Proofs/Exec.lean:3401: /-- Planted (law `ConcreteSpec.Core.Exec.prim_bind_refused`, case `accepted`, subject `ConcreteSpec.Core.Exec.runFrom`, mutant `ConcreteSpec.Core.Exec.Refusal.stuckRunFrom`).
ConcreteSpec/Proofs/Ident.lean:27: /-- Representation lemma. The renderer has no root, traversal spelling, drive introducer or
ConcreteSpec/Proofs/Leak.lean:1319: /-- Report for an empty function run under `noFootprintAllocator`: zero local request, net, and
ConcreteSpec/Proofs/Observe2.lean:286: empty, dot, traversal, absolute, drive, empty-component, preserved backslash and control-character
ConcreteSpec/Proofs/Observe2.lean:927: /-- Closed drive refusal input and exact typed diagnostic. -/
ConcreteSpec/Proofs/Observe2.lean:932: Refused: malformed drive-prefixed path. -/
ConcreteSpec/Proofs/Observe2.lean:1658: Projection distributes over the C1 event-history append performed by `ConcreteSpec.Core.Exec.runFrom`. -/
ConcreteSpec/Proofs/Observe2.lean:1663: /-- Law (subject `ConcreteSpec.Core.Exec.runFrom`).
ConcreteSpec/Proofs/Observe2.lean:1671: CompleteTrace C input (A.project (Exec.runFrom P ρ fuel hist c).events) ∧
ConcreteSpec/Proofs/Observe2.lean:1673: (Exec.runFrom P ρ fuel hist c).ending = .noAnswer c' →
ConcreteSpec/Proofs/Observe2.lean:1677: C.mayBlock input (A.project (Exec.runFrom P ρ fuel hist c).events ++ [.request q]) q := by
ConcreteSpec/Proofs/Observe2.lean:1683: | none => simp [Exec.runFrom, final, valid]
ConcreteSpec/Proofs/Observe2.lean:1684: | some value => simp [Exec.runFrom, final, valid]
ConcreteSpec/Proofs/Observe2.lean:1688: | some value => simp [Exec.runFrom, final, valid]
ConcreteSpec/Proofs/Observe2.lean:1691: | none => simp [Exec.runFrom, final, next, valid]
ConcreteSpec/Proofs/Observe2.lean:1694: | next c' => simpa [Exec.runFrom, final, next] using ih hist c' valid
ConcreteSpec/Proofs/Observe2.lean:1699: · simpa [Exec.runFrom, final, next, answer] using valid
ConcreteSpec/Proofs/Observe2.lean:1701: simp only [Exec.runFrom, final, next, answer, Exec.Ending.noAnswer.injEq] at ending
ConcreteSpec/Proofs/Observe2.lean:1705: by simpa [Exec.runFrom, final, next, answer] using mayBlock⟩
ConcreteSpec/Proofs/Observe2.lean:1709: | none => simp [Exec.runFrom, final, next, answer, allowed, continued, valid]
ConcreteSpec/Proofs/Observe2.lean:1723: simpa [Exec.runFrom, final, next, answer, allowed, continued] using
ConcreteSpec/Proofs/Observe2.lean:1725: · simp [Exec.runFrom, final, next, answer, allowed, valid]
ConcreteSpec/Proofs/Observe2.lean:1739: (ending : (Exec.runFrom P ρ fuel hist c).ending = .noAnswer c')
ConcreteSpec/Proofs/Observe2.lean:1743: (A.project (Exec.runFrom P ρ fuel hist c).events ++ [.request q]) ∧
ConcreteSpec/Proofs/Observe2.lean:1744: C.mayBlock input (A.project (Exec.runFrom P ρ fuel hist c).events ++ [.request q]) q := by
ConcreteSpec/Proofs/Observe2.lean:1761: ExecAdapter.BlockingPermitted P A C input (Exec.runFrom P ρ fuel hist c) := by
ConcreteSpec/Proofs/Observe2.lean:1767: operational `ConcreteSpec.Core.Exec.runFrom`/`reportObservation` law. (approved: REV-C6-agy-285cf57db)
ConcreteSpec/Proofs/Observe2.lean:1787: (_fuel : Nat) (hist : List O.Event) (c : Exec.Config O) : Exec.Report O :=
ConcreteSpec/Proofs/Observe2.lean:1854: def answeredReport : Exec.Report ops := Exec.run program answeringOracle 1 (requesting 3)
ConcreteSpec/Proofs/Observe2.lean:1855: def blockedReport : Exec.Report ops := Exec.run program blockingOracle 1 (requesting 3)
ConcreteSpec/Proofs/ObserveEnvironment.lean:85: Exec.runFrom (O := Environment.operands S) ⟨[]⟩ (E.oracle input) 4 hist
ConcreteSpec/Proofs/ObserveEnvironment.lean:90: (Exec.runFrom (O := Environment.operands S) ⟨[]⟩ (E.oracle input) 4 hist
ConcreteSpec/Proofs/ObserveEnvironment.lean:96: (Exec.runFrom (O := Environment.operands S) ⟨[]⟩ (E.oracle input) 4 hist
ConcreteSpec/Proofs/ObserveEnvironment.lean:101: (Exec.runFrom (O := Environment.operands S) ⟨[]⟩ (E.oracle input) 4 hist
ConcreteSpec/Proofs/ObserveEnvironment.lean:108: (Exec.runFrom (O := Environment.operands S) ⟨[]⟩ (E.oracle input) 4 hist
ConcreteSpec/Proofs/ObserveEnvironment.lean:117: have hrun : Exec.runFrom (O := Environment.operands S) ⟨[]⟩ (E.oracle input) 4 hist
ConcreteSpec/Proofs/ObserveEnvironment.lean:121: conv => lhs; unfold Exec.runFrom
ConcreteSpec/Proofs/ObserveEnvironment.lean:123: conv => lhs; unfold Exec.runFrom
ConcreteSpec/Proofs/ObserveEnvironment.lean:127: conv => lhs; unfold Exec.runFrom
ConcreteSpec/Proofs/ObserveEnvironment.lean:129: conv => lhs; unfold Exec.runFrom
ConcreteSpec/Proofs/ObserveEnvironment.lean:131: unfold Exec.runFrom
ConcreteSpec/Proofs/ObserveEnvironment.lean:141: have hrun : Exec.runFrom (O := Environment.operands S) ⟨[]⟩ (E.oracle input) 4 hist
ConcreteSpec/Proofs/ObserveEnvironment.lean:145: conv => lhs; unfold Exec.runFrom
ConcreteSpec/Proofs/ObserveEnvironment.lean:147: conv => lhs; unfold Exec.runFrom
ConcreteSpec/Proofs/ObserveEnvironment.lean:254: Exec.runFrom (O := Environment.operands Demo.signature) ⟨[]⟩ (Demo.envHandler.oracle ()) 4 []
ConcreteSpec/Proofs/ObserveEnvironment.lean:259: (Exec.runFrom (O := Environment.operands Demo.signature) ⟨[]⟩ (Demo.envHandler.oracle ()) 4 []
ConcreteSpec/Proofs/ObserveEnvironment.lean:265: (Exec.runFrom (O := Environment.operands Demo.signature) ⟨[]⟩ (Demo.envHandler.oracle ()) 4 []
ConcreteSpec/Proofs/ObserveEnvironment.lean:270: (Exec.runFrom (O := Environment.operands Demo.signature) ⟨[]⟩ (Demo.envHandler.oracle ()) 4 []
ConcreteSpec/Proofs/ObserveEnvironment.lean:283: Exec.runFrom (O := Environment.operands Demo.signature) ⟨[]⟩ (dropReturnValueOracle Demo.envHandler ()) 4 []
ConcreteSpec/Proofs/ObserveEnvironment.lean:288: (Exec.runFrom (O := Environment.operands Demo.signature) ⟨[]⟩ (dropReturnValueOracle Demo.envHandler ()) 4 []
ConcreteSpec/Proofs/ObserveEnvironment.lean:294: (Exec.runFrom (O := Environment.operands Demo.signature) ⟨[]⟩ (dropReturnValueOracle Demo.envHandler ()) 4 []
ConcreteSpec/Proofs/ObserveEnvironment.lean:299: (Exec.runFrom (O := Environment.operands Demo.signature) ⟨[]⟩ (dropReturnValueOracle Demo.envHandler ()) 4 []
ConcreteSpec/Proofs/ObserveEnvironment.lean:307: have hend := congrArg Exec.Report.ending hrun
ConcreteSpec/Proofs/ObserveEnvironment.lean:402: Exec.runFrom (O := Environment.operands Demo.signature) ⟨[]⟩ (handler.oracle ()) 4 prior
ConcreteSpec/Proofs/ObserveEnvironment.lean:407: (Exec.runFrom (O := Environment.operands Demo.signature) ⟨[]⟩ (handler.oracle ()) 4 prior
ConcreteSpec/Proofs/ObserveEnvironment.lean:413: (Exec.runFrom (O := Environment.operands Demo.signature) ⟨[]⟩ (handler.oracle ()) 4 prior
ConcreteSpec/Proofs/ObserveEnvironment.lean:418: (Exec.runFrom (O := Environment.operands Demo.signature) ⟨[]⟩ (handler.oracle ()) 4 prior
ConcreteSpec/Proofs/ObserveEnvironment.lean:430: Exec.runFrom (O := Environment.operands Demo.signature) ⟨[]⟩ (forgetHistoryOracle handler ()) 4 prior
ConcreteSpec/Proofs/ObserveEnvironment.lean:435: (Exec.runFrom (O := Environment.operands Demo.signature) ⟨[]⟩ (forgetHistoryOracle handler ()) 4 prior
ConcreteSpec/Proofs/ObserveEnvironment.lean:441: (Exec.runFrom (O := Environment.operands Demo.signature) ⟨[]⟩ (forgetHistoryOracle handler ()) 4 prior
ConcreteSpec/Proofs/ObserveEnvironment.lean:446: (Exec.runFrom (O := Environment.operands Demo.signature) ⟨[]⟩ (forgetHistoryOracle handler ()) 4 prior
ConcreteSpec/Proofs/ObserveEnvironment.lean:454: have responses := congrArg (fun r : Exec.Report (Environment.operands Demo.signature) =>
ConcreteSpec/Proofs/ObserveEnvironment.lean:516: Exec.runFrom (O := Environment.operands signature) ⟨[]⟩ (envHandler.oracle ()) 4 []
ConcreteSpec/Proofs/ObserveEnvironment.lean:521: (Exec.runFrom (O := Environment.operands signature) ⟨[]⟩ (envHandler.oracle ()) 4 []
ConcreteSpec/Proofs/ObserveEnvironment.lean:527: (Exec.runFrom (O := Environment.operands signature) ⟨[]⟩ (envHandler.oracle ()) 4 []
ConcreteSpec/Proofs/ObserveEnvironment.lean:532: (Exec.runFrom (O := Environment.operands signature) ⟨[]⟩ (envHandler.oracle ()) 4 []
ConcreteSpec/Proofs/ObserveEnvironment.lean:545: Exec.runFrom (O := Environment.operands signature) ⟨[]⟩ (noReplyOracle envHandler ()) 4 []
ConcreteSpec/Proofs/ObserveEnvironment.lean:550: (Exec.runFrom (O := Environment.operands signature) ⟨[]⟩ (noReplyOracle envHandler ()) 4 []
ConcreteSpec/Proofs/ObserveEnvironment.lean:556: (Exec.runFrom (O := Environment.operands signature) ⟨[]⟩ (noReplyOracle envHandler ()) 4 []
ConcreteSpec/Proofs/ObserveEnvironment.lean:561: (Exec.runFrom (O := Environment.operands signature) ⟨[]⟩ (noReplyOracle envHandler ()) 4 []
ConcreteSpec/Proofs/ObserveEnvironment.lean:569: have ending := congrArg Exec.Report.ending hrun
ConcreteSpec/Proofs/ObserveEnvironment.lean:665: Exec.runFrom (O := Environment.operands Demo.signature) ⟨[]⟩ (blockingHandler.oracle ()) 4 []
ConcreteSpec/Proofs/ObserveEnvironment.lean:670: (Exec.runFrom (O := Environment.operands Demo.signature) ⟨[]⟩ (blockingHandler.oracle ()) 4 []
ConcreteSpec/Proofs/ObserveEnvironment.lean:676: (Exec.runFrom (O := Environment.operands Demo.signature) ⟨[]⟩ (blockingHandler.oracle ()) 4 []
ConcreteSpec/Proofs/ObserveEnvironment.lean:681: (Exec.runFrom (O := Environment.operands Demo.signature) ⟨[]⟩ (blockingHandler.oracle ()) 4 []
ConcreteSpec/Proofs/ObserveEnvironment.lean:694: Exec.runFrom (O := Environment.operands Demo.signature) ⟨[]⟩ (fabricateReplyOnBlockOracle blockingHandler ()) 4 []
ConcreteSpec/Proofs/ObserveEnvironment.lean:699: (Exec.runFrom (O := Environment.operands Demo.signature) ⟨[]⟩ (fabricateReplyOnBlockOracle blockingHandler ()) 4 []
ConcreteSpec/Proofs/ObserveEnvironment.lean:705: (Exec.runFrom (O := Environment.operands Demo.signature) ⟨[]⟩ (fabricateReplyOnBlockOracle blockingHandler ()) 4 []
ConcreteSpec/Proofs/ObserveEnvironment.lean:710: (Exec.runFrom (O := Environment.operands Demo.signature) ⟨[]⟩ (fabricateReplyOnBlockOracle blockingHandler ()) 4 []
ConcreteSpec/Proofs/ObserveOutput.lean:6: over `Exec.runFrom`, `ExecAdapter.project`, and `reportObservation`, parameterized by the operand
ConcreteSpec/Proofs/ObserveOutput.lean:8: `Exec.Ending O` of `Exec.runFrom`. `OutputDemo` supplies their closed `Instance` and `Planted`
ConcreteSpec/Proofs/ObserveOutput.lean:41: (Exec.runFrom P ρ fuel hist c).events (Exec.runFrom P ρ fuel hist c).ending := by
ConcreteSpec/Proofs/ObserveOutput.lean:44: unfold Exec.runFrom
ConcreteSpec/Proofs/ObserveOutput.lean:49: unfold Exec.runFrom
ConcreteSpec/Proofs/ObserveOutput.lean:129: (M.runFrom P fuel hist c).events (M.runFrom P fuel hist c).ending ∧
ConcreteSpec/Proofs/ObserveOutput.lean:130: publicOutputs (A.project (M.runFrom P fuel hist c).events) =
ConcreteSpec/Proofs/ObserveOutput.lean:140: apply no_erasing_correspondence A (events := (M.runFrom P fuel hist c).events)
ConcreteSpec/Proofs/ObserveOutput.lean:163: (permitted : ExecAdapter.BlockingPermitted P A C input (M.runFrom P fuel hist c))
ConcreteSpec/Proofs/ObserveOutput.lean:165: (ending : (M.runFrom P fuel hist c).ending = .final (.trapped kind site) store)
ConcreteSpec/Proofs/ObserveOutput.lean:168: reportObservation P A C input (M.runFrom P fuel hist c) permitted =
ConcreteSpec/Proofs/ObserveOutput.lean:169: some (.terminal ⟨A.project (M.runFrom P fuel hist c).events,
ConcreteSpec/Proofs/ObserveOutput.lean:172: (M.runFrom P fuel hist c).events (.final (.trapped kind site) store) ∧
ConcreteSpec/Proofs/ObserveOutput.lean:173: publicOutputs (A.project (M.runFrom P fuel hist c).events) =
ConcreteSpec/Proofs/ObserveOutput.lean:186: (r : Exec.Report O) (permitted : ExecAdapter.BlockingPermitted P A C input r) :
ConcreteSpec/Proofs/ObserveOutput.lean:280: Exec.runFrom program Exec.Demo.env (3 * values.length + 1) history (initial values) =
ConcreteSpec/Proofs/ObserveOutput.lean:285: unfold Exec.runFrom
ConcreteSpec/Proofs/ObserveOutput.lean:290: conv => lhs; unfold Exec.runFrom
ConcreteSpec/Proofs/ObserveOutput.lean:292: conv => lhs; unfold Exec.runFrom
ConcreteSpec/Proofs/ObserveOutput.lean:295: conv => lhs; unfold Exec.runFrom
ConcreteSpec/Proofs/ObserveOutput.lean:296: change Exec.runFrom program Exec.Demo.env (3 * ns.length + 1) (history ++ [n]) (initial ns) = _
ConcreteSpec/Proofs/ObserveOutput.lean:305: (Exec.runFrom program Exec.Demo.env (3 * values.length + 1) history (initial values)) := by
ConcreteSpec/Proofs/ObserveOutput.lean:314: outputs (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events ending) :
ConcreteSpec/Proofs/ObserveOutput.lean:324: outputs (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events
ConcreteSpec/Proofs/ObserveOutput.lean:325: (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).ending ∧
ConcreteSpec/Proofs/ObserveOutput.lean:326: publicOutputs (adapter.project (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events) =
ConcreteSpec/Proofs/ObserveOutput.lean:335: outputs (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events
ConcreteSpec/Proofs/ObserveOutput.lean:336: (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).ending ∧
ConcreteSpec/Proofs/ObserveOutput.lean:337: publicOutputs (eraseProjection adapter (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events) =
ConcreteSpec/Proofs/ObserveOutput.lean:348: outputs (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events
ConcreteSpec/Proofs/ObserveOutput.lean:349: (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).ending ∧
ConcreteSpec/Proofs/ObserveOutput.lean:350: publicOutputs (reverseProjection adapter (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events) =
ConcreteSpec/Proofs/ObserveOutput.lean:362: (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])) (permitted [3, 4] [-2, 11]) =
ConcreteSpec/Proofs/ObserveOutput.lean:363: some (.terminal ⟨adapter.project (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events,
ConcreteSpec/Proofs/ObserveOutput.lean:366: outputs (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events
ConcreteSpec/Proofs/ObserveOutput.lean:368: publicOutputs (adapter.project (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events) =
ConcreteSpec/Proofs/ObserveOutput.lean:379: (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])) ∧
ConcreteSpec/Proofs/ObserveOutput.lean:380: (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).ending =
ConcreteSpec/Proofs/ObserveOutput.lean:384: (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])) (permitted [3, 4] [-2, 11]) =
ConcreteSpec/Proofs/ObserveOutput.lean:385: some (.terminal ⟨adapter.project (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events,
ConcreteSpec/Proofs/ObserveOutput.lean:388: outputs (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events
ConcreteSpec/Proofs/ObserveOutput.lean:390: publicOutputs (adapter.project (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events) =
ConcreteSpec/Proofs/ObserveOutput.lean:405: (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])) ∧
ConcreteSpec/Proofs/ObserveOutput.lean:406: (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).ending =
ConcreteSpec/Proofs/ObserveOutput.lean:410: (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])) (permitted [3, 4] [-2, 11]) =
ConcreteSpec/Proofs/ObserveOutput.lean:411: some (.terminal ⟨eraseProjection adapter (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events,
ConcreteSpec/Proofs/ObserveOutput.lean:414: outputs (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events
ConcreteSpec/Proofs/ObserveOutput.lean:416: publicOutputs (eraseProjection adapter (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events) =
ConcreteSpec/Proofs/ObserveReport.lean:19: (C : OperationContract S) (input : C.Input) (r : Exec.Report O) :
ConcreteSpec/Proofs/ObserveReport.lean:49: ReportTraceValid P A C input (Exec.runFrom P ρ fuel hist c)
ConcreteSpec/Proofs/ObserveReport.lean:50: (reportObservation P A C input (Exec.runFrom P ρ fuel hist c)
ConcreteSpec/Proofs/ObserveReport.lean:53: generalize ending : (Exec.runFrom P ρ fuel hist c).ending = e
ConcreteSpec/Proofs/ObserveReport.lean:84: (ending : (Exec.runFrom P ρ fuel hist c).ending = .noAnswer pendingConfig)
ConcreteSpec/Proofs/ObserveReport.lean:87: reportObservation P A C input (Exec.runFrom P ρ fuel hist c)
ConcreteSpec/Proofs/ObserveReport.lean:89: some (.blocked (A.project (Exec.runFrom P ρ fuel hist c).events ++ [.request q]) q) ∧
ConcreteSpec/Proofs/ObserveReport.lean:91: (A.project (Exec.runFrom P ρ fuel hist c).events ++ [.request q]) ∧
ConcreteSpec/Proofs/ObserveReport.lean:92: C.mayBlock input (A.project (Exec.runFrom P ρ fuel hist c).events ++ [.request q]) q := by
ConcreteSpec/Proofs/ObserveReport.lean:101: (r : Exec.Report O) (permitted : ExecAdapter.BlockingPermitted P A C input r) :
ConcreteSpec/Proofs/ObserveReport.lean:111: (r : Exec.Report O) (permitted : ExecAdapter.BlockingPermitted P A C input r) :
ConcreteSpec/Proofs/ObserveReport.lean:121: (_r : Exec.Report O) (_permitted : ExecAdapter.BlockingPermitted _P _A C _input _r) :
ConcreteSpec/Proofs/ObserveReport.lean:131: (ending : (Exec.runFrom P ρ fuel hist c).ending = .outOfFuel c') :
ConcreteSpec/Proofs/ObserveReport.lean:135: unfold Exec.runFrom at ending
ConcreteSpec/Proofs/ObserveReport.lean:143: unfold Exec.runFrom at ending
ConcreteSpec/Proofs/ObserveReport.lean:179: (ending : (Exec.runFrom P ρ fuel hist c).ending = .outOfFuel c') :
ConcreteSpec/Proofs/ObserveReport.lean:181: CompleteTrace C input (A.project (Exec.runFrom P ρ fuel hist c).events) ∧
ConcreteSpec/Proofs/ObserveReport.lean:182: reportObservation P A C input (Exec.runFrom P ρ fuel hist c)
ConcreteSpec/Proofs/ObserveReport.lean:188: /-- Fault: fabricates `.finiteDivergence` when `runFrom` runs out of fuel, confusing budget
ConcreteSpec/Proofs/ObserveReport.lean:192: (r : Exec.Report O) (permitted : ExecAdapter.BlockingPermitted P A C input r) :
ConcreteSpec/Proofs/ObserveReport.lean:198: /-- Fault: fabricates a normal `.exited 0` terminal observation when `runFrom` runs out of fuel. -/
ConcreteSpec/Proofs/ObserveReport.lean:201: (r : Exec.Report O) (permitted : ExecAdapter.BlockingPermitted P A C input r) :
ConcreteSpec/Proofs/ObserveReport.lean:257: (ending : (Exec.runFrom P ρ fuel hist c).ending = .final (.trapped kind site) store)
ConcreteSpec/Proofs/ObserveReport.lean:259: reportObservation P A C input (Exec.runFrom P ρ fuel hist c)
ConcreteSpec/Proofs/ObserveReport.lean:261: some (.terminal ⟨A.project (Exec.runFrom P ρ fuel hist c).events,
ConcreteSpec/Proofs/ObserveReport.lean:276: (r : Exec.Report O) (permitted : ExecAdapter.BlockingPermitted P A C input r) :
ConcreteSpec/Proofs/ObserveReport.lean:298: (ending : (Exec.runFrom P ρ fuel hist c).ending = .final (.trapped kind site) store)
ConcreteSpec/Proofs/ObserveReport.lean:299: (permitted : ExecAdapter.BlockingPermitted P A C input (Exec.runFrom P ρ fuel hist c)) :
ConcreteSpec/Proofs/ObserveReport.lean:301: reportObservation P A C input (Exec.runFrom P ρ fuel hist c) permitted =
ConcreteSpec/Proofs/ObserveReport.lean:302: some (.terminal ⟨A.project (Exec.runFrom P ρ fuel hist c).events,
ConcreteSpec/Proofs/ObserveReport.lean:323: (r : Exec.Report O) (permitted : ExecAdapter.BlockingPermitted P A C input r) :
ConcreteSpec/Proofs/ObserveReport.lean:334: (r : Exec.Report O) (permitted : ExecAdapter.BlockingPermitted P A C input r) :
ConcreteSpec/Proofs/ObserveReport.lean:345: (r : Exec.Report O) (permitted : ExecAdapter.BlockingPermitted P A C input r) :
ConcreteSpec/Proofs/ObserveReport.lean:365: (adapter.project (Exec.runFrom program blockingOracle 1 [1] (requesting 3)).events) ∧
ConcreteSpec/Proofs/ObserveReport.lean:367: (Exec.runFrom program blockingOracle 1 [1] (requesting 3)).ending = .noAnswer c' →
ConcreteSpec/Proofs/ObserveReport.lean:372: (adapter.project (Exec.runFrom program blockingOracle 1 [1] (requesting 3)).events ++
ConcreteSpec/Proofs/ObserveReport.lean:378: subject `ConcreteSpec.Core.Exec.runFrom`, mutant `ConcreteSpec.Core.Observe.completedAsPendingRun`). -/
ConcreteSpec/Proofs/ObserveReport.lean:398: (Exec.runFrom program blockingOracle 1 [1] (requesting 3))
ConcreteSpec/Proofs/ObserveReport.lean:400: (Exec.runFrom program blockingOracle 1 [1] (requesting 3))
ConcreteSpec/Proofs/ObserveReport.lean:411: (Exec.runFrom program blockingOracle 1 [1] (requesting 3))
ConcreteSpec/Proofs/ObserveReport.lean:413: (Exec.runFrom program blockingOracle 1 [1] (requesting 3))
ConcreteSpec/Proofs/ObserveReport.lean:429: (Exec.runFrom program blockingOracle 1 [1] (requesting 3))
ConcreteSpec/Proofs/ObserveReport.lean:431: (Exec.runFrom program blockingOracle 1 [1] (requesting 3))
ConcreteSpec/Proofs/ObserveReport.lean:458: (Exec.runFrom program blockingOracle 1 [1] (requesting 3))
ConcreteSpec/Proofs/ObserveReport.lean:462: (Exec.runFrom program blockingOracle 1 [1] (requesting 3)).events ++ [request 3]) (query 3)) ∧
ConcreteSpec/Proofs/ObserveReport.lean:464: (adapter.project (Exec.runFrom program blockingOracle 1 [1] (requesting 3)).events ++ [request 3]) ∧
ConcreteSpec/Proofs/ObserveReport.lean:466: (adapter.project (Exec.runFrom program blockingOracle 1 [1] (requesting 3)).events ++ [request 3])
ConcreteSpec/Proofs/ObserveReport.lean:475: (Exec.runFrom program blockingOracle 1 [1] (requesting 3)).ending = .noAnswer (requesting 3) ∧
ConcreteSpec/Proofs/ObserveReport.lean:479: (Exec.runFrom program blockingOracle 1 [1] (requesting 3))
ConcreteSpec/Proofs/ObserveReport.lean:483: (Exec.runFrom program blockingOracle 1 [1] (requesting 3)).events ++ [request 3]) (query 3)) ∧
ConcreteSpec/Proofs/ObserveReport.lean:485: (adapter.project (Exec.runFrom program blockingOracle 1 [1] (requesting 3)).events ++ [request 3]) ∧
ConcreteSpec/Proofs/ObserveReport.lean:487: (adapter.project (Exec.runFrom program blockingOracle 1 [1] (requesting 3)).events ++ [request 3])
ConcreteSpec/Proofs/ObserveReport.lean:527: (Exec.runFrom program blockingOracle 0 [] (requesting 3))
ConcreteSpec/Proofs/ObserveReport.lean:529: (Exec.runFrom program blockingOracle 0 [] (requesting 3))
ConcreteSpec/Proofs/ObserveReport.lean:540: (Exec.runFrom program blockingOracle 0 [] (requesting 3))
ConcreteSpec/Proofs/ObserveReport.lean:542: (Exec.runFrom program blockingOracle 0 [] (requesting 3))
ConcreteSpec/Proofs/ObserveReport.lean:553: (adapter.project (Exec.runFrom program answeringOracle 0 [1] (requesting 3)).events) ∧
ConcreteSpec/Proofs/ObserveReport.lean:555: (Exec.runFrom program answeringOracle 0 [1] (requesting 3))
ConcreteSpec/Proofs/ObserveReport.lean:566: (Exec.runFrom program answeringOracle 0 [1] (requesting 3)).ending =
ConcreteSpec/Proofs/ObserveReport.lean:570: (adapter.project (Exec.runFrom program answeringOracle 0 [1] (requesting 3)).events) ∧
ConcreteSpec/Proofs/ObserveReport.lean:572: (Exec.runFrom program answeringOracle 0 [1] (requesting 3))
ConcreteSpec/Proofs/ObserveReport.lean:586: (Exec.runFrom program answeringOracle 0 [1] (requesting 3)).ending =
ConcreteSpec/Proofs/ObserveReport.lean:590: (adapter.project (Exec.runFrom program answeringOracle 0 [1] (requesting 3)).events) ∧
ConcreteSpec/Proofs/ObserveReport.lean:592: (Exec.runFrom program answeringOracle 0 [1] (requesting 3))
ConcreteSpec/Proofs/ObserveReport.lean:652: (Exec.runFrom program blockingOracle 1 [1] abortingConfig)
ConcreteSpec/Proofs/ObserveReport.lean:656: (Exec.runFrom program blockingOracle 1 [1] abortingConfig).events,
ConcreteSpec/Proofs/ObserveReport.lean:672: (Exec.runFrom program blockingOracle 1 [1] abortingConfig).ending =
ConcreteSpec/Proofs/ObserveReport.lean:676: (Exec.runFrom program blockingOracle 1 [1] abortingConfig)
ConcreteSpec/Proofs/ObserveReport.lean:680: (Exec.runFrom program blockingOracle 1 [1] abortingConfig).events,
ConcreteSpec/Proofs/ObserveReport.lean:702: (Exec.runFrom program blockingOracle 1 [1] abortingConfig).ending =
ConcreteSpec/Proofs/ObserveReport.lean:706: (Exec.runFrom program blockingOracle 1 [1] abortingConfig)
ConcreteSpec/Proofs/ObserveReport.lean:710: (Exec.runFrom program blockingOracle 1 [1] abortingConfig).events,
ConcreteSpec/Proofs/ObserveReport.lean:738: (Exec.runFrom program blockingOracle 1 [1] abortingConfig) := by
ConcreteSpec/Proofs/ObserveReport.lean:745: (Exec.runFrom program blockingOracle 1 [1] abortingConfig) sourcePermitted =
ConcreteSpec/Proofs/ObserveReport.lean:746: some (.terminal ⟨sourceAdapter.project (Exec.runFrom program blockingOracle 1 [1] abortingConfig).events,
ConcreteSpec/Proofs/ObserveReport.lean:760: (Exec.runFrom program blockingOracle 1 [1] abortingConfig).ending =
ConcreteSpec/Proofs/ObserveReport.lean:763: (Exec.runFrom program blockingOracle 1 [1] abortingConfig) ∧
ConcreteSpec/Proofs/ObserveReport.lean:766: (Exec.runFrom program blockingOracle 1 [1] abortingConfig) sourcePermitted =
ConcreteSpec/Proofs/ObserveReport.lean:767: some (.terminal ⟨sourceAdapter.project (Exec.runFrom program blockingOracle 1 [1] abortingConfig).events,
ConcreteSpec/Proofs/ObserveReport.lean:790: (Exec.runFrom program blockingOracle 1 [1] abortingConfig).ending =
ConcreteSpec/Proofs/ObserveReport.lean:793: (Exec.runFrom program blockingOracle 1 [1] abortingConfig) ∧
ConcreteSpec/Proofs/ObserveReport.lean:796: (Exec.runFrom program blockingOracle 1 [1] abortingConfig) sourcePermitted =
ConcreteSpec/Proofs/ObserveReport.lean:797: some (.terminal ⟨sourceAdapter.project (Exec.runFrom program blockingOracle 1 [1] abortingConfig).events,
ConcreteSpec/Proofs/Own.lean:2968: Ending.NotStuck (Exec.runFrom P (liftOracle O types permit ρ) fuel hist c).ending →
ConcreteSpec/Proofs/Own.lean:2969: Exec.runFrom (eraseProgram P) ρ fuel hist (eraseConfig c) =
ConcreteSpec/Proofs/Own.lean:2970: eraseReport (Exec.runFrom P (liftOracle O types permit ρ) fuel hist c)
ConcreteSpec/Proofs/Own.lean:2972: unfold Exec.runFrom
ConcreteSpec/Proofs/Own.lean:2976: unfold Exec.runFrom at hns ⊢
ConcreteSpec/Proofs/Own.lean:3407: Exec.runFrom (eraseProgram P) ρ fuel hist (eraseConfig c) =
ConcreteSpec/Proofs/Own.lean:3408: eraseReport (Exec.runFrom P (liftOracle O types permit ρ) fuel hist c)
ConcreteSpec/Proofs/Own.lean:3410: unfold Exec.runFrom
ConcreteSpec/Proofs/Own.lean:3415: unfold Exec.runFrom
ConcreteSpec/Proofs/Own.lean:3911: Exec.runFrom (eraseProgram emptyProg) erasedOracle 100 [1] (eraseConfig primTraceConfig) =
ConcreteSpec/Proofs/Own.lean:3912: eraseReport (Exec.runFrom emptyProg ρI 100 [1] primTraceConfig) ∧
ConcreteSpec/Proofs/Own.lean:3913: Exec.runFrom (eraseProgram emptyProg) erasedOracle 100 [] (eraseConfig cTrap) =
ConcreteSpec/Proofs/Own.lean:3914: eraseReport (Exec.runFrom emptyProg ρI 100 [] cTrap) ∧
ConcreteSpec/Proofs/Own.lean:3915: Exec.runFrom (eraseProgram emptyProg) erasedOracle 100 [] (eraseConfig cNoAns) =
ConcreteSpec/Proofs/Own.lean:3916: eraseReport (Exec.runFrom emptyProg ρI 100 [] cNoAns) ∧
ConcreteSpec/Proofs/Own.lean:3917: Exec.runFrom (eraseProgram emptyProg) badOracle 100 [] (eraseConfig primTraceConfig) =
ConcreteSpec/Proofs/Own.lean:3918: eraseReport (Exec.runFrom emptyProg (liftOracle erasedOps types permitE badOracle) 100 [] primTraceConfig) ∧
ConcreteSpec/Proofs/Own.lean:3919: Exec.runFrom (eraseProgram emptyProg) erasedOracle 2 [] (eraseConfig primTraceConfig) =
ConcreteSpec/Proofs/Own.lean:3920: eraseReport (Exec.runFrom emptyProg ρI 2 [] primTraceConfig) :=
ConcreteSpec/Proofs/Own.lean:3928: example : ¬ (Exec.runFrom (eraseProgram emptyProg)
ConcreteSpec/Proofs/Own.lean:3930: eraseReport (Exec.runFrom emptyProg (liftOracle erasedOps types permitE erasedOracle) 100 [] moveConfig)) := by
ConcreteSpec/Proofs/Own.lean:3981: /-- Observation. For both non-stuck (`acceptedMoveConfig`) and stuck (`acceptedStuckConfig`) checker-accepted programs, `runFrom` on the erased machine equals `eraseReport` of the instrumented `runFrom`. -/
ConcreteSpec/Proofs/Own.lean:3984: Exec.runFrom (eraseProgram emptyProg) erasedOracle 100 [] (eraseConfig acceptedMoveConfig) =
ConcreteSpec/Proofs/Own.lean:3985: eraseReport (Exec.runFrom emptyProg ρI 100 [] acceptedMoveConfig) ∧
ConcreteSpec/Proofs/Own.lean:3986: Exec.runFrom (eraseProgram emptyProg) erasedOracle 100 [] (eraseConfig acceptedStuckConfig) =
ConcreteSpec/Proofs/Own.lean:3987: eraseReport (Exec.runFrom emptyProg ρI 100 [] acceptedStuckConfig) :=
ConcreteSpec/Proofs/Own.lean:3993: /-- Rejection observation. Without `CheckerAccepted`, `moveConfig` violates `runFrom` erasure because instrumentation refuses `.read x` after `.move x`. -/
ConcreteSpec/Proofs/Own.lean:3994: example : ¬ (Exec.runFrom (eraseProgram emptyProg)
ConcreteSpec/Proofs/Own.lean:3996: eraseReport (Exec.runFrom emptyProg (liftOracle erasedOps types permitE erasedOracle) 100 [] moveConfig)) := by
ConcreteSpec/Proofs/Own.lean:4326: example : Exec.runFrom (eraseProgram emptyProg) erasedOracle 100 [] (eraseConfig primTraceConfig) =
ConcreteSpec/Proofs/Own.lean:4327: eraseReport (Exec.runFrom emptyProg (liftOracle erasedOps types permitE erasedOracle) 100 [] primTraceConfig) :=
ConcreteSpec/Proofs/Own.lean:4331: example : Ending.NotStuck (Exec.runFrom emptyProg
ConcreteSpec/Proofs/Own.lean:4333: ¬ (Exec.runFrom (eraseProgram emptyProg) erasedOracle 100 [] (eraseConfig primTraceConfig) =
ConcreteSpec/Proofs/Own.lean:4334: eraseReport (Exec.runFrom emptyProg (liftOracleMutateOwn erasedOps types permitE erasedOracle) 100 [] primTraceConfig)) := by
ConcreteSpec/Proofs/Own.lean:4381: example : Exec.runFrom (eraseProgram emptyProg) erasedOracle 100 [] (eraseConfig primFrameStuckConfig) =
ConcreteSpec/Proofs/Own.lean:4382: eraseReport (Exec.runFrom emptyProg (liftOracle erasedOps types permitE erasedOracle) 100 [] primFrameStuckConfig) :=
ConcreteSpec/Proofs/Own.lean:4388: ¬ (Exec.runFrom (eraseProgram emptyProg) erasedOracle 100 [] (eraseConfig primFrameStuckConfig) =
ConcreteSpec/Proofs/Own.lean:4389: eraseReport (Exec.runFrom emptyProg (liftOracleMutateOwn erasedOps types permitE erasedOracle) 100 [] primFrameStuckConfig)) :=
ConcreteSpec/Proofs/Prim.lean:296: /-- Law (subject `ConcreteSpec.Core.Ghost.drive`).
ConcreteSpec/Proofs/Prim.lean:301: ∃ tr, (drive P ρ fuel hist c).events = hist ++ tr ∧
ConcreteSpec/Proofs/Prim.lean:302: Exec.Steps P c tr (drive P ρ fuel hist c).config := by
ConcreteSpec/Proofs/Prim.lean:303: have sound : ∀ (fuel : Nat) (hist : List O.Event) (c : Exec.Config O) (r : SegmentReport O),
ConcreteSpec/Proofs/Prim.lean:304: drive P ρ fuel hist c = r → ∃ tr, r.events = hist ++ tr ∧ Exec.Steps P c tr r.config := by
ConcreteSpec/Proofs/Prim.lean:311: simp [drive, hf] at h
ConcreteSpec/Proofs/Prim.lean:315: simp [drive, hf] at h
ConcreteSpec/Proofs/Prim.lean:322: simp [drive, hf] at h
ConcreteSpec/Proofs/Prim.lean:328: simp [drive, hf, hs] at h
ConcreteSpec/Proofs/Prim.lean:334: simp [drive, hf, hs] at h
ConcreteSpec/Proofs/Prim.lean:340: simp [drive, hf, hs, ha] at h
ConcreteSpec/Proofs/Prim.lean:347: simp [drive, hf, hs, ha, hallow, hk] at h
ConcreteSpec/Proofs/Prim.lean:351: simp [drive, hf, hs, ha, hallow, hk] at h
ConcreteSpec/Proofs/Prim.lean:359: · simp [drive, hf, hs, ha, hallow] at h
ConcreteSpec/Proofs/Prim.lean:364: /-- Helper (computation). Exposes the non-normal guard branch for the erasure induction. The terminal report and drive-step laws carry execution evidence.
ConcreteSpec/Proofs/Prim.lean:368: (hf : c.final? ≠ some .normal) : runStmts P ρ fuel hist ss c = drive P ρ fuel hist c := by
ConcreteSpec/Proofs/Prim.lean:373: | none => simp [runStmts, h]
ConcreteSpec/Proofs/Prim.lean:374: | some f => cases f <;> simp [runStmts, h] at hf ⊢
ConcreteSpec/Proofs/Prim.lean:376: /-- Law (subject `ConcreteSpec.Core.Ghost.runStmts`).
ConcreteSpec/Proofs/Prim.lean:383: runStmts P ρ fuel hist ss c = ⟨hist, .final f c.store, c, fuel⟩ := by
ConcreteSpec/Proofs/Prim.lean:385: | nil => simp [runStmts, drive, hf]
ConcreteSpec/Proofs/Prim.lean:386: | cons s ss => cases f <;> simp [runStmts, drive, hf] at hn ⊢
ConcreteSpec/Proofs/Prim.lean:388: /-- Law (subject `ConcreteSpec.Core.Ghost.runPieces`, `ConcreteSpec.Core.Ghost.erase`).
ConcreteSpec/Proofs/Prim.lean:394: (c : Exec.Config O), runPieces G P ρ fuel hist ps Γ c =
ConcreteSpec/Proofs/Prim.lean:395: runStmts P ρ fuel hist (erase ps) c := by
ConcreteSpec/Proofs/Prim.lean:405: simpa [runPieces, erase, erasePiece, List.filterMap, hn] using h
ConcreteSpec/Proofs/Prim.lean:406: · simp [runPieces, erase, erasePiece, List.filterMap, hn, runStmts_of_not_normal]
ConcreteSpec/Proofs/Prim.lean:409: · cases hr : drive P ρ fuel hist ⟨.exec s, c.locals, c.store, c.frames⟩ with
ConcreteSpec/Proofs/Prim.lean:416: simpa [runPieces, runStmts, erase, erasePiece, List.filterMap, hn, hr] using h
ConcreteSpec/Proofs/Prim.lean:417: | returned v => simp [runPieces, runStmts, erase, erasePiece, List.filterMap, hn, hr]
ConcreteSpec/Proofs/Prim.lean:418: | trapped k site => simp [runPieces, runStmts, erase, erasePiece, List.filterMap, hn, hr]
ConcreteSpec/Proofs/Prim.lean:419: | stuck c' => simp [runPieces, runStmts, erase, erasePiece, List.filterMap, hn, hr]
ConcreteSpec/Proofs/Prim.lean:420: | noAnswer c' => simp [runPieces, runStmts, erase, erasePiece, List.filterMap, hn, hr]
ConcreteSpec/Proofs/Prim.lean:421: | badAnswer c' => simp [runPieces, runStmts, erase, erasePiece, List.filterMap, hn, hr]
ConcreteSpec/Proofs/Prim.lean:422: | outOfFuel c' => simp [runPieces, runStmts, erase, erasePiece, List.filterMap, hn, hr]
ConcreteSpec/Proofs/Prim.lean:423: · simp [runPieces, erase, erasePiece, List.filterMap, hn, runStmts_of_not_normal]
ConcreteSpec/Proofs/Prim.lean:431: (runPieces G P ρ fuel hist ps Γ c).events = (runStmts P ρ fuel hist (erase ps) c).events ∧
ConcreteSpec/Proofs/Prim.lean:432: (runPieces G P ρ fuel hist ps Γ c).ending = (runStmts P ρ fuel hist (erase ps) c).ending := by
ConcreteSpec/Proofs/Prim.lean:813: (fuel : Nat) (hist : List O.Event) (c : Exec.Config O) : Ghost.SegmentReport O :=
ConcreteSpec/Proofs/Prim.lean:814: { Ghost.drive P ρ fuel hist c with events := [] }
ConcreteSpec/Proofs/Prim.lean:818: (Ghost.drive requestProgram answerOracle 2 initialHist registeredStart).events =
ConcreteSpec/Proofs/Prim.lean:821: (Ghost.drive requestProgram answerOracle 2 initialHist registeredStart).config :=
ConcreteSpec/Proofs/Prim.lean:824: /-- Planted (law `ConcreteSpec.Core.Ghost.drive_steps_result`, case `ConcreteSpec.Core.Vacuity.registeredStart`, subject `ConcreteSpec.Core.Ghost.drive`, mutant `ConcreteSpec.Core.Vacuity.dropHistoryDrive`). -/
ConcreteSpec/Proofs/Prim.lean:838: (fuel : Nat) (hist : List O.Event) : List (Exec.Stmt O) → Exec.Config O → Ghost.SegmentReport O
ConcreteSpec/Proofs/Prim.lean:839: | [], c => Ghost.drive P ρ fuel hist c
ConcreteSpec/Proofs/Prim.lean:841: let r := Ghost.drive P ρ fuel hist ⟨.exec s, c.locals, c.store, c.frames⟩
ConcreteSpec/Proofs/Prim.lean:847: example : Ghost.runStmts GhostDemo.program GhostDemo.oracle 20 [] [.abort ⟨4⟩] returnedStart =
ConcreteSpec/Proofs/Prim.lean:848: Ghost.drive GhostDemo.program GhostDemo.oracle 20 [] returnedStart :=
ConcreteSpec/Proofs/Prim.lean:855: Ghost.drive GhostDemo.program GhostDemo.oracle 20 [] returnedStart) := by
ConcreteSpec/Proofs/Prim.lean:861: example : Ghost.runStmts GhostDemo.program GhostDemo.oracle 20 [] [.abort ⟨4⟩] returnedStart =
ConcreteSpec/Proofs/Prim.lean:867: example : Ghost.runStmts GhostDemo.program GhostDemo.oracle 20 [] [.abort ⟨4⟩] trappedStart =
ConcreteSpec/Proofs/Prim.lean:872: /-- Planted (law `ConcreteSpec.Core.Ghost.runStmts_terminal`, case `ConcreteSpec.Core.Vacuity.returnedStart`, subject `ConcreteSpec.Core.Ghost.runStmts`, mutant `ConcreteSpec.Core.Vacuity.restartingRunStmts`). -/
ConcreteSpec/Proofs/Prim.lean:888: example : Ghost.runPieces GhostDemo.language GhostDemo.program GhostDemo.oracle 20 []
ConcreteSpec/Proofs/Prim.lean:890: Ghost.runStmts GhostDemo.program GhostDemo.oracle 20 [] (Ghost.erase GhostDemo.pieces)
ConcreteSpec/Proofs/Prim.lean:896: example : ¬ (Ghost.runPieces GhostDemo.language GhostDemo.program GhostDemo.oracle 20 []
ConcreteSpec/Proofs/Prim.lean:898: Ghost.runStmts GhostDemo.program GhostDemo.oracle 20 [] (eraseAllPieces GhostDemo.pieces)
ConcreteSpec/Proofs/Prim.lean:901: have hrem := congrArg Ghost.SegmentReport.remaining h
ConcreteSpec/Proofs/Prim.lean:906: example : (Ghost.runPieces GhostDemo.language GhostDemo.program GhostDemo.oracle 20 []
ConcreteSpec/Proofs/Prim.lean:908: (Ghost.runStmts GhostDemo.program GhostDemo.oracle 20 [] (Ghost.erase abortPieces)
ConcreteSpec/Proofs/Prim.lean:910: (Ghost.runPieces GhostDemo.language GhostDemo.program GhostDemo.oracle 20 []
ConcreteSpec/Proofs/Prim.lean:912: (Ghost.runStmts GhostDemo.program GhostDemo.oracle 20 [] (Ghost.erase abortPieces)
ConcreteSpec/Proofs/Prim.lean:918: example : ¬ ((Ghost.runPieces GhostDemo.language GhostDemo.program GhostDemo.oracle 20 []
ConcreteSpec/Proofs/Prim.lean:920: (Ghost.runStmts GhostDemo.program GhostDemo.oracle 20 [] (eraseAllPieces abortPieces)
ConcreteSpec/Proofs/Prim.lean:922: (Ghost.runPieces GhostDemo.language GhostDemo.program GhostDemo.oracle 20 []
ConcreteSpec/Proofs/Prim.lean:924: (Ghost.runStmts GhostDemo.program GhostDemo.oracle 20 [] (eraseAllPieces abortPieces)
ConcreteSpec/README.md:142: (`runFn`, `runFrom`) after successful predecessor checks, with relational primitive-bind refusal
ConcreteSpec/Spec/Adequacy.lean:8: instrumented copy of `runFrom`, and `Proofs/Adequacy.lean` proves that erasing the queries gives
ConcreteSpec/Spec/Adequacy.lean:69: def QueryReport.toReport (r : QueryReport O) : Report O := ⟨r.events, r.ending⟩
ConcreteSpec/Spec/Adequacy.lean:71: /-- `runFrom` instrumented with its issued queries. The query issued at a request is recorded before
ConcreteSpec/Spec/Exec.lean:119: an operation with no allowed answer has none. The driver `runFrom` reports an oracle that gives
ConcreteSpec/Spec/Exec.lean:306: `Step.prim` requires success, and `runFrom` reports the requesting configuration as stuck
ConcreteSpec/Spec/Exec.lean:842: structure Report (O : Operands) where
ConcreteSpec/Spec/Exec.lean:848: def runFrom (P : Program O) (ρ : Oracle O) : Nat → List O.Event → Config O → Report O
ConcreteSpec/Spec/Exec.lean:856: | some (.next c') => runFrom P ρ n hist c'
ConcreteSpec/Spec/Exec.lean:863: | some c' => runFrom P ρ n (hist ++ a.event.toList) c'
ConcreteSpec/Spec/Exec.lean:868: def run (P : Program O) (ρ : Oracle O) (fuel : Nat) (c : Config O) : Report O :=
ConcreteSpec/Spec/Exec.lean:869: runFrom P ρ fuel [] c
ConcreteSpec/Spec/Exec.lean:874: (st : O.Store) : Option (Report O) :=
ConcreteSpec/Spec/Exec.lean:1020: def go (body : Stmt ops) : Report ops := run prog env 200 (start ⟨[], .int, ⟨0⟩, body⟩ [] .init)
ConcreteSpec/Spec/Exec.lean:1042: def brief (r : Report ops) : List Int × Option (Final Int) :=
ConcreteSpec/Spec/Exec.lean:1048: def isStuck (r : Report ops) : Bool := match r.ending with
ConcreteSpec/Spec/Exec.lean:1053: def isOutOfFuel (r : Report ops) : Bool := match r.ending with
ConcreteSpec/Spec/Exec.lean:1170: example : (fun r : Report ops => (r.events, isStuck r)) (go (.seq (.bind a (.lit 7) s)
ConcreteSpec/Spec/Exec.lean:1173: example : (fun r : Report ops => (r.events, isStuck r)) (go (.seq (.bind a (.lit 7) s)
ConcreteSpec/Spec/Ident.lean:704: /-- A Windows drive introducer is an ASCII alphabetic character followed by a colon. -/
ConcreteSpec/Spec/Ident.lean:822: traversal, empty components and drive prefixes in its representation. -/
ConcreteSpec/Spec/Observe.lean:510: abbrev PhysicalOperands.runFrom {Output : Type} (M : PhysicalOperands Output)
ConcreteSpec/Spec/Observe.lean:512: (c : Exec.Config M.operands) : Exec.Report M.operands :=
ConcreteSpec/Spec/Observe.lean:513: Exec.runFrom P M.oracle fuel hist c
ConcreteSpec/Spec/Observe.lean:607: (C : OperationContract S) (input : C.Input) (r : Exec.Report O) : Prop :=
ConcreteSpec/Spec/Observe.lean:619: (input : C.Input) (r : Exec.Report O) (_permitted : ExecAdapter.BlockingPermitted P A C input r) :
ConcreteSpec/Spec/Observe.lean:676: /-- Turn a typed operation handler into the actual oracle consumed by `Exec.runFrom`. Dispatch
ConcreteSpec/Spec/Observe.lean:826: def refusedReport : Exec.Report refusingBindOps :=
ConcreteSpec/Spec/Observe.lean:864: Exec.runFrom (O := refusingBindOps) ⟨[]⟩ Exec.Demo.env 1 [11] refusedRequest = ⟨[11], .stuck refusedRequest⟩ := by
ConcreteSpec/Spec/Observe.lean:874: (Exec.runFrom (O := refusingBindOps) ⟨[]⟩ Exec.Demo.env 1 [11]
ConcreteSpec/Spec/Own.lean:558: def eraseReport (r : Exec.Report (instrumentOperands O types permit)) : Exec.Report O :=
ConcreteSpec/Spec/Own.lean:853: def runBody (body : Exec.Stmt ops) : Exec.Report ops :=
ConcreteSpec/Spec/Own.lean:856: def stuck (r : Exec.Report ops) : Bool := match r.ending with
ConcreteSpec/Spec/Own.lean:926: def instrumentedMoveRun : Exec.Report instrumentedOps :=
ConcreteSpec/Spec/Own.lean:929: def erasedMoveRun : Exec.Report erasedOps :=
ConcreteSpec/Spec/Own.lean:1005: def callRun (body : Exec.Stmt callOps) : Exec.Report callOps :=
ConcreteSpec/Spec/Prim.lean:371: the final configuration and remaining fuel that C1's `Report` does not expose. `drive` otherwise
ConcreteSpec/Spec/Prim.lean:372: has exactly C1's `runFrom` cases, including request refusal and all `Ending` classifications.
ConcreteSpec/Spec/Prim.lean:407: structure SegmentReport (O : Exec.Operands) where
ConcreteSpec/Spec/Prim.lean:413: /-- C1's `runFrom` with the final configuration and remaining fuel retained. -/
ConcreteSpec/Spec/Prim.lean:414: def drive (P : Exec.Program O) (ρ : Exec.Oracle O) :
ConcreteSpec/Spec/Prim.lean:415: Nat → List O.Event → Exec.Config O → SegmentReport O
ConcreteSpec/Spec/Prim.lean:423: | some (.next c') => drive P ρ n hist c'
ConcreteSpec/Spec/Prim.lean:430: | some c' => drive P ρ n (hist ++ a.event.toList) c'
ConcreteSpec/Spec/Prim.lean:435: def runStmts (P : Exec.Program O) (ρ : Exec.Oracle O) (fuel : Nat) (hist : List O.Event) :
ConcreteSpec/Spec/Prim.lean:436: List (Exec.Stmt O) → Exec.Config O → SegmentReport O
ConcreteSpec/Spec/Prim.lean:437: | [], c => drive P ρ fuel hist c
ConcreteSpec/Spec/Prim.lean:441: let r := drive P ρ fuel hist ⟨.exec s, c.locals, c.store, c.frames⟩
ConcreteSpec/Spec/Prim.lean:443: | .final .normal _ => runStmts P ρ r.remaining r.events ss r.config
ConcreteSpec/Spec/Prim.lean:445: | _ => drive P ρ fuel hist c
ConcreteSpec/Spec/Prim.lean:449: def runPieces (G : Language O) (P : Exec.Program O) (ρ : Exec.Oracle O)
ConcreteSpec/Spec/Prim.lean:451: List (Piece G) → AList Var G.Val → Exec.Config O → SegmentReport O
ConcreteSpec/Spec/Prim.lean:452: | [], _, c => drive P ρ fuel hist c
ConcreteSpec/Spec/Prim.lean:455: | some .normal => runPieces G P ρ fuel hist ps (evalStmt G c.locals c.store g Γ) c
ConcreteSpec/Spec/Prim.lean:456: | _ => drive P ρ fuel hist c
ConcreteSpec/Spec/Prim.lean:460: let r := drive P ρ fuel hist ⟨.exec s, c.locals, c.store, c.frames⟩
ConcreteSpec/Spec/Prim.lean:462: | .final .normal _ => runPieces G P ρ r.remaining r.events ps Γ r.config
ConcreteSpec/Spec/Prim.lean:464: | _ => drive P ρ fuel hist c
ConcreteSpec/Spec/Prim.lean:595: example : Ghost.runPieces language program oracle 20 [] pieces [] start =
ConcreteSpec/Spec/Prim.lean:596: Ghost.runStmts program oracle 20 [] [.skip] start := rfl
ConcreteSpec/Spec/Prim.lean:599: example : (Ghost.runPieces language program oracle 20 [] pieces [] start).config.store = 5 := rfl
docs/archive/bugs-2026-11/bug_disposition.tsv:267: 293	every compile (`-o`, `--emit-llvm`) runs Report's safety gate (provenViolationDiagnostics: the bounds, division and overflow obligation walks), whose shared scoped walker asked for the scalars each statement assigns by walking the statement's whole subtree, filtered the whole hypothesis list after every statement, and copied the hypothesis scope at every nesting level, so a function nested n deep took time n² to compile even with no obligation in it (10000 nested ifs: --emit-llvm 9.5 s with bug 286 fixed, of which the stages through lower 0.25 s). Found re-measuring the R-0550 nesting shapes with whole compiles. Class 17.	fixed	legacy-impl	outside (performance)	-	-	no	old pass data structure superlinear; program useful as scaling input	retired with implementation (B3); not fixed	17	-	none yet (no new-compiler slice landed)	-
docs/archive/crucial-2026-11-06/DIAGNOSTICS.md:33: - The hand-kept legacy E-code map (`legacy/Concrete/Report/Diagnostic.lean`, `codeRuleTag`;
docs/archive/no-hidden-traps-2026-11-06/Q13_CODEX.md:83: | **Terminates** | Discharge every obligation or explicitly permit it at its source. Report evidence, assumptions and unresolved dependencies. | Require established termination; specify acceptable trust. |
docs/archive/no-hidden-traps-2026-11-06/Q13_CODEX.md:85: | **Stack bound** | Report only for the identified final artifact, target and entry context; preserve foreign and runtime assumptions. | Require an established bound and adequate reservation under Q14 claim 1. |
docs/archive/no-hidden-traps-2026-11-06/Q13_CODEX.md:88: | **May block** | Report possible external waits and progress assumptions. Feed unresolved progress into termination. | Require established absence of blocking, or separately established wait bounds. |
docs/archive/roadmap-2026-11/ROADMAP_RECONCILIATION.md:114: | R-0233 | done | — | done — the unprofiled-float exclusion shipped (`ProofCore.lean`, `Report.lean`); profiled floats remain V7 (R-0496/R-0495). |
docs/archive/roadmap-2026-11/ROADMAP_RECONCILIATION.md:384: | A | Report-layer partial definitions, 2465, 3101, 3261–3272, 3369–3371 | superseded by R-0506/S12a | 2, 4 | Merge; discard stale 17/6/5 counts |
docs/archive/roadmap-2026-11/ROADMAP_RECONCILIATION.md:913: | D | “Report the scope as each slice becomes usable” · 15653–15655 | Live | 3 | Keep under R-0285 |
docs/archive/roadmap-2026-11/ROADMAP_TASKS.md:1269: Report splitting, the old Proof/ProofCore overlap audit and old string-identity repairs are
docs/archive/roadmap-2026-11/ROADMAP_TASKS.md:1310: implementation, including native and interpreter. Report every forbidding rule without a reject
docs/archive/roadmap-2026-11/ROADMAP_TASKS.md:1655: file — C1-C4 are compile-time theorems in `Concrete/Report/Evidence.lean`, the module they
docs/archive/roadmap-2026-11/ROADMAP_TASKS.md:1890: contracts, dependencies, correspondence, roots, receipts and policy inputs. Report renders validated
docs/archive/roadmap-2026-11/ROADMAP_TASKS.md:1892: from strings or independently derive evidence. Evidence must not import Report. The evidence ownership targets are Identity, Subject, Attestation, Contract, Dependency,
docs/archive/roadmap-2026-11/ROADMAP_TASKS.md:2750: consuming typed artifacts, Report rendering rather than deriving facts, and
docs/archive/roadmap-2026-11/roadmap_restructure_2026-11-02.tsv:51: R-0554	register	1, 2, 3	split	A	Keep unique specification and document authority, manifest-driven tests, drift and dead-code controls, generated-artifact discipline and branch hygiene, applying structural boundaries to the new compiler as components appear.	-	The cleanup list mixes surviving repository controls with old Report splitting, ProofCore overlap and string-identity repairs.
docs/archive/roadmap-2026-11/roadmap_restructure_2026-11-02.tsv:59: R-0479	register	1	split	B	Represent unavailable or failed computations without fabricating facts, and preserve refusals through typed results, text, JSON and exit codes with production controls.	-	The remaining survey targets old Report defaults, but the body states a general fact and refusal preservation invariant.
docs/archive/roadmap-2026-11/task_disposition.tsv:234: R-0233	Close the unprofiled-float proof hole before any float proof claim	done					done — the unprofiled-float exclusion shipped (ProofCore.lean, Report.lean); profiled floats remain V7 (R-0496/R-0495).
docs/archive/roadmap-2026-11/task_disposition.tsv:250: R-0249	Report runtime-error obligations in human and JSON forms	transferred-with-obligations	F	R-0178	Report runtime-error obligations in human and JSON forms.		duplicate of R-0178/R-0179: merge into R-0178/R-0179.
scripts/proofs/axioms.golden:1194: ConcreteSpec.Core.Exec.Report.mk.inj
scripts/proofs/axioms.golden:1195: ConcreteSpec.Core.Exec.Report.mk.injEq	propext
scripts/proofs/axioms.golden:1196: ConcreteSpec.Core.Exec.Report.mk.sizeOf_spec
scripts/proofs/axioms.golden:1401: ConcreteSpec.Core.Exec.runFrom.eq_1	propext
scripts/proofs/axioms.golden:1402: ConcreteSpec.Core.Exec.runFrom.eq_2	propext
scripts/proofs/axioms.golden:1403: ConcreteSpec.Core.Exec.runFrom.eq_3	propext
scripts/proofs/axioms.golden:1404: ConcreteSpec.Core.Exec.runFrom.eq_def	propext
scripts/proofs/axioms.golden:3285: _private.ConcreteSpec.Proofs.Observe2.0.ConcreteSpec.Core.Exec.runFrom.match_1.eq_1
scripts/proofs/axioms.golden:3286: _private.ConcreteSpec.Proofs.Observe2.0.ConcreteSpec.Core.Exec.runFrom.match_1.eq_2
scripts/proofs/axioms.golden:3287: _private.ConcreteSpec.Proofs.Observe2.0.ConcreteSpec.Core.Exec.runFrom.match_3.eq_1
scripts/proofs/axioms.golden:3288: _private.ConcreteSpec.Proofs.Observe2.0.ConcreteSpec.Core.Exec.runFrom.match_3.eq_2
scripts/proofs/axioms.golden:3289: _private.ConcreteSpec.Proofs.Observe2.0.ConcreteSpec.Core.Exec.runFrom.match_5.eq_1
scripts/proofs/axioms.golden:3290: _private.ConcreteSpec.Proofs.Observe2.0.ConcreteSpec.Core.Exec.runFrom.match_5.eq_2
scripts/proofs/axioms.golden:3291: _private.ConcreteSpec.Proofs.Observe2.0.ConcreteSpec.Core.Exec.runFrom.match_5.eq_3
scripts/proofs/axioms.golden:3292: _private.ConcreteSpec.Proofs.Observe2.0.ConcreteSpec.Core.Exec.runFrom.match_7.eq_1
scripts/proofs/axioms.golden:3293: _private.ConcreteSpec.Proofs.Observe2.0.ConcreteSpec.Core.Exec.runFrom.match_7.eq_2
scripts/proofs/axioms.golden:3294: _private.ConcreteSpec.Proofs.Observe2.0.ConcreteSpec.Core.Exec.runFrom.match_7.eq_3
scripts/proofs/axioms.golden:3295: _private.ConcreteSpec.Proofs.Observe2.0.ConcreteSpec.Core.Exec.runFrom.match_9.eq_1
scripts/push-both.sh:241: # after — a mirror carrying work the primary never accepted. Report it as such
scripts/review/review.sh:252: Report correctness findings only: wrong behaviour, logic errors, regressions, unhandled cases, tests that do not check what they claim, and documentation that contradicts the code. Do not report style preferences.
```

Enclosing declarations, including every directly affected existing theorem and example (names are original source headers; continuation lines remain in the source):

```text
.claude/codex-review-prompt.md:1: (module documentation or namespace context)
.claude/commands/creview.md:1: (module documentation or namespace context)
.claude/commands/cstatus.md:1: (module documentation or namespace context)
.claude/commands/rebase.md:1: (module documentation or namespace context)
.claude/commands/roadmap-item.md:1: (module documentation or namespace context)
ConcreteSpec/Proofs/Adequacy.lean:244: theorem runFromQ_toReport {ρ₀ : Oracle O} :
ConcreteSpec/Proofs/Adequacy.lean:582: theorem runFromQ_request_correspondence {fuel : Nat} {events : List O.Event}
ConcreteSpec/Proofs/Adequacy.lean:619: theorem runFrom_sound {ρ₀ : Oracle O} (fuel : Nat) (events : List O.Event) (c : Config O) :
ConcreteSpec/Proofs/Adequacy.lean:638: theorem runFrom_events_from {ρ₀ : Oracle O} (fuel : Nat) (events : List O.Event) (c : Config O) :
ConcreteSpec/Proofs/Adequacy.lean:648: theorem runFromQ_complete {n : Nat} {pos pos' : Position O} (h : Moves P ρ n pos pos')
ConcreteSpec/Proofs/Adequacy.lean:667: theorem runFrom_complete {ρ₀ : Oracle O} {n : Nat} {events : List O.Event} {c : Config O}
ConcreteSpec/Proofs/Adequacy.lean:678: theorem runFrom_mono {ρ₀ : Oracle O} : ∀ (n m : Nat), n ≤ m →
ConcreteSpec/Proofs/Adequacy.lean:725: theorem runFrom_complete_le {ρ₀ : Oracle O} {n m : Nat} (hle : n ≤ m)
ConcreteSpec/Proofs/Adequacy.lean:739: theorem runFrom_trace_prefix_le {ρ₀ : Oracle O} : ∀ (n m : Nat), n ≤ m →
ConcreteSpec/Proofs/Adequacy.lean:780: theorem runFrom_trace_prefix {ρ₀ : Oracle O} (n : Nat) (events : List O.Event) (c : Config O) :
ConcreteSpec/Proofs/Adequacy.lean:787: theorem runFrom_final_sound {ρ₀ : Oracle O} {fuel : Nat} {events : List O.Event} {c : Config O}
ConcreteSpec/Proofs/Adequacy.lean:1584: theorem not_infinite_of_final {ρ₀ : Oracle O} {fuel : Nat} {c : Config O} {f : Final O.Val} {finalStore : O.Store}
ConcreteSpec/Proofs/Adequacy.lean:1600: theorem not_infinite_of_noAnswer {ρ₀ : Oracle O} {fuel : Nat} {c blocked : Config O}
ConcreteSpec/Proofs/Adequacy.lean:1616: theorem endlesslyBlocked_of_noAnswer {ρ₀ : Oracle O} {fuel : Nat} {c blocked : Config O}
ConcreteSpec/Proofs/Adequacy.lean:1631: theorem TerminationEvidence.all_allowed_runs {domain : CallDomain O} (E : TerminationEvidence O P domain)
ConcreteSpec/Proofs/Adequacy.lean:2799: example : (runFromQ prog observationEnv.toQueryOracle 1 [] [] terminalConfig).toReport =
ConcreteSpec/Proofs/Adequacy.lean:2805: example : (droppedAnswerRun prog observationEnv.toQueryOracle 1 [] [] terminalConfig).toReport ≠
ConcreteSpec/Proofs/Adequacy.lean:2905: def earlyStopRun {O : Operands} (P : Program O) (ρ : Oracle O) (fuel : Nat)
ConcreteSpec/Proofs/Adequacy.lean:2926: example : thirdQuery.occurrence = interleavedQuery0.occurrence + 1 :=
ConcreteSpec/Proofs/Adequacy.lean:2939: def extraFuelDropRunFrom {O : Operands} (P : Program O) (ρ : Oracle O) (fuel : Nat)
ConcreteSpec/Proofs/Adequacy.lean:2945: example : runFrom prog observationEnv 2 [] terminalConfig = runFrom prog observationEnv 1 [] terminalConfig :=
ConcreteSpec/Proofs/Adequacy.lean:2951: example : 1 ≤ 2 ∧
ConcreteSpec/Proofs/Adequacy.lean:2963: example : (runFrom prog observationEnv 1 [] terminalConfig).events <+:
ConcreteSpec/Proofs/Adequacy.lean:3077: def resumeBlockedRun {O : Operands} (P : Program O) (ρ : Oracle O) (fuel : Nat)
ConcreteSpec/Proofs/Exec.lean:2045: private theorem runFrom_zero_ne_stuck_inv (P : Program O) (ρ : Oracle O)
ConcreteSpec/Proofs/Exec.lean:2054: theorem continuation_refused (P : Program O) (ρ : Oracle O)
ConcreteSpec/Proofs/Exec.lean:2074: theorem prim_bind_refused (P : Program O) (ρ : Oracle O) (hist : List O.Event)
ConcreteSpec/Proofs/Exec.lean:2969: def bypassRunFn (P : Program O) (ρ : Oracle O) (fuel : Nat) (fn : Fn O) (vs : List O.Val)
ConcreteSpec/Proofs/Exec.lean:2975: def stuckRunFn (_ : Program O) (_ : Oracle O) (_ : Nat) (_ : Fn O) (_ : List O.Val)
ConcreteSpec/Proofs/Exec.lean:3290: def logCont (O : Operands) (a : Answer O.Val O.Event O.Store) : Option (Config O) :=
ConcreteSpec/Proofs/Exec.lean:3297: def bypassRunFrom (P : Program O) (ρ : Oracle O) (fuel : Nat) (hist : List O.Event)
ConcreteSpec/Proofs/Exec.lean:3318: def stuckRunFrom (_ : Program O) (_ : Oracle O) (_ : Nat) (hist : List O.Event)
ConcreteSpec/Proofs/Exec.lean:3324: example : runFrom prog Demo.env 1 [9] ⟨.exec (.prim (some x) .log [.lit 1] s), [], .init, []⟩ =
ConcreteSpec/Proofs/Exec.lean:3333: example : (⟨.exec (.prim (some x) .log [.lit 1] s), [], .init, []⟩ : Config ops).final? = none ∧
ConcreteSpec/Proofs/Exec.lean:3349: example : runFrom Demo.prog Demo.env 1 [9] ⟨.exec (.prim (some x) .log [.lit 1] s), [], .init, []⟩ =
ConcreteSpec/Proofs/Exec.lean:3372: example : runFrom prog Demo.env 1 [9] ⟨.exec (.prim (some x) .log [.lit 1] s), [], .init, []⟩ =
ConcreteSpec/Proofs/Exec.lean:3380: example : evalArgs ops [] DState.init [.lit 1] = .ok ([1], DState.init) ∧
ConcreteSpec/Proofs/Exec.lean:3395: example : runFrom Demo.prog Demo.env 1 [9] ⟨.exec (.prim (some x) .log [.lit 1] s), [], .init, []⟩ =
ConcreteSpec/Proofs/Ident.lean:20: theorem SourcePath.render_injective (a b : SourcePath) (h : a.render = b.render) : a = b := by
ConcreteSpec/Proofs/Leak.lean:1315: def mixedStatusReport : LineageReport :=
ConcreteSpec/Proofs/Observe2.lean:276: theorem LoadedProject.load_refuses (input : ProjectSources)
ConcreteSpec/Proofs/Observe2.lean:918: example : (("/src", tabSource) ∈ absoluteInput.files ∧
ConcreteSpec/Proofs/Observe2.lean:929: def driveDiagnostic : SourcePath.Refusal := ⟨"C:main", "C:main".toList, .drivePrefixed⟩
ConcreteSpec/Proofs/Observe2.lean:1642: theorem ValidTrace.answer_projection {C : OperationContract S} {input : C.Input}
ConcreteSpec/Proofs/Observe2.lean:1659: theorem ExecAdapter.project_append (A : ExecAdapter O S) (xs ys : List O.Event) :
ConcreteSpec/Proofs/Observe2.lean:1667: theorem runFrom_validTrace (P : Exec.Program O) (A : ExecAdapter O S) (C : OperationContract S)
ConcreteSpec/Proofs/Observe2.lean:1732: theorem runFrom_noAnswer_pending_valid (P : Exec.Program O) (A : ExecAdapter O S)
ConcreteSpec/Proofs/Observe2.lean:1756: theorem runFrom_blockingPermitted (P : Exec.Program O) (A : ExecAdapter O S)
ConcreteSpec/Proofs/Observe2.lean:1786: def completedAsPendingRun {O : Exec.Operands} (_P : Exec.Program O) (_ρ : Exec.Oracle O)
ConcreteSpec/Proofs/Observe2.lean:1854: def answeredReport : Exec.Report ops := Exec.run program answeringOracle 1 (requesting 3)
ConcreteSpec/Proofs/Observe2.lean:1855: def blockedReport : Exec.Report ops := Exec.run program blockingOracle 1 (requesting 3)
ConcreteSpec/Proofs/ObserveEnvironment.lean:80: theorem environment_run_reply {S : EventSignature} (C : OperationContract S)
ConcreteSpec/Proofs/ObserveEnvironment.lean:253: example : (∀ reply, Demo.envHandler.reply () (Environment.history [] ++ [.request ⟨.double, 3⟩]) .double 3 = some reply →
ConcreteSpec/Proofs/ObserveEnvironment.lean:280: example : Environment.Valid Demo.contract Demo.envHandler ∧
ConcreteSpec/Proofs/ObserveEnvironment.lean:401: example : (∀ reply, handler.reply () (Environment.history prior ++ [.request ⟨.double, 3⟩]) .double 3 = some reply →
ConcreteSpec/Proofs/ObserveEnvironment.lean:427: example : Environment.Valid contract handler ∧
ConcreteSpec/Proofs/ObserveEnvironment.lean:515: example : (∀ reply, envHandler.reply () (Environment.history [] ++ [.request ⟨.fourTimes, 3⟩]) .fourTimes 3 = some reply →
ConcreteSpec/Proofs/ObserveEnvironment.lean:542: example : Environment.Valid contract envHandler ∧
ConcreteSpec/Proofs/ObserveEnvironment.lean:664: example : (∀ reply, blockingHandler.reply () (Environment.history [] ++ [.request ⟨.double, 3⟩]) .double 3 = some reply →
ConcreteSpec/Proofs/ObserveEnvironment.lean:691: example : Environment.Valid contract blockingHandler ∧
ConcreteSpec/Proofs/ObserveOutput.lean:1: (module documentation or namespace context)
ConcreteSpec/Proofs/ObserveOutput.lean:37: theorem physical_run_execution {O : Exec.Operands} (Output : Type) [F : PhysicalEffects O Output]
ConcreteSpec/Proofs/ObserveOutput.lean:122: theorem project_physical_outputs {S : EventSignature}
ConcreteSpec/Proofs/ObserveOutput.lean:158: theorem output_before_trap_kept {S : EventSignature}
ConcreteSpec/Proofs/ObserveOutput.lean:184: def dropTrapHistoryReport {S : EventSignature} {O : Exec.Operands}
ConcreteSpec/Proofs/ObserveOutput.lean:279: theorem run_writes (values : List Int) (history : List Int) :
ConcreteSpec/Proofs/ObserveOutput.lean:303: theorem permitted (values : List Int) (history : List Int) :
ConcreteSpec/Proofs/ObserveOutput.lean:312: theorem cannot_erase {outputs : List String} {ending : Exec.Ending Exec.Demo.ops}
ConcreteSpec/Proofs/ObserveOutput.lean:323: example : ∃ outputs, PhysicalEffects.Execution String program Exec.Demo.env [-2, 11] (initial [3, 4])
ConcreteSpec/Proofs/ObserveOutput.lean:333: example : adapter.OutputCorrespondence ∧
ConcreteSpec/Proofs/ObserveOutput.lean:346: example : adapter.OutputCorrespondence ∧
ConcreteSpec/Proofs/ObserveOutput.lean:361: example : reportObservation program adapter Demo.contract ()
ConcreteSpec/Proofs/ObserveOutput.lean:377: example : adapter.OutputCorrespondence ∧
ConcreteSpec/Proofs/ObserveOutput.lean:403: example : adapter.OutputCorrespondence ∧
ConcreteSpec/Proofs/ObserveReport.lean:18: def ReportTraceValid {O : Exec.Operands} {S : EventSignature} (P : Exec.Program O) (A : ExecAdapter O S)
ConcreteSpec/Proofs/ObserveReport.lean:44: theorem runFrom_report_trace {O : Exec.Operands} {S : EventSignature}
ConcreteSpec/Proofs/ObserveReport.lean:76: theorem runFrom_blocked_report {O : Exec.Operands} {S : EventSignature}
ConcreteSpec/Proofs/ObserveReport.lean:99: def onlyPendingHistory {S : EventSignature} {O : Exec.Operands}
ConcreteSpec/Proofs/ObserveReport.lean:109: def dropBlockedHistory {S : EventSignature} {O : Exec.Operands}
ConcreteSpec/Proofs/ObserveReport.lean:119: def alwaysNoneReport {S : EventSignature} {O : Exec.Operands}
ConcreteSpec/Proofs/ObserveReport.lean:128: theorem runFrom_outOfFuel_not_final {O : Exec.Operands}
ConcreteSpec/Proofs/ObserveReport.lean:174: theorem runFrom_outOfFuel_report_none {O : Exec.Operands} {S : EventSignature}
ConcreteSpec/Proofs/ObserveReport.lean:190: def divergeOnOutOfFuelReport {S : EventSignature} {O : Exec.Operands}
ConcreteSpec/Proofs/ObserveReport.lean:199: def exitZeroOnOutOfFuelReport {S : EventSignature} {O : Exec.Operands}
ConcreteSpec/Proofs/ObserveReport.lean:251: theorem runFrom_trapped_status_134 {O : Exec.Operands} {S : EventSignature}
ConcreteSpec/Proofs/ObserveReport.lean:274: def exit134OnTrapReport {S : EventSignature} {O : Exec.Operands}
ConcreteSpec/Proofs/ObserveReport.lean:293: theorem runFrom_source_trap {O : Exec.Operands} {S : EventSignature}
ConcreteSpec/Proofs/ObserveReport.lean:321: def wrongFileReport {S : EventSignature} {O : Exec.Operands}
ConcreteSpec/Proofs/ObserveReport.lean:332: def scalarColumnReport {S : EventSignature} {O : Exec.Operands}
ConcreteSpec/Proofs/ObserveReport.lean:343: def fabricatedBlockingReport {S : EventSignature} {O : Exec.Operands}
ConcreteSpec/Proofs/ObserveReport.lean:364: example : CompleteTrace permittingContract ()
ConcreteSpec/Proofs/ObserveReport.lean:397: example : ReportTraceValid program adapter permittingContract ()
ConcreteSpec/Proofs/ObserveReport.lean:408: example : ExecAdapter.WellFormed adapter permittingContract () blockingOracle ∧
ConcreteSpec/Proofs/ObserveReport.lean:426: example : ExecAdapter.WellFormed adapter permittingContract () blockingOracle ∧
ConcreteSpec/Proofs/ObserveReport.lean:457: example : reportObservation program adapter permittingContract ()
ConcreteSpec/Proofs/ObserveReport.lean:473: example : ExecAdapter.WellFormed adapter permittingContract () blockingOracle ∧
ConcreteSpec/Proofs/ObserveReport.lean:526: example : ReportTraceValid program adapter permittingContract ()
ConcreteSpec/Proofs/ObserveReport.lean:537: example : ExecAdapter.WellFormed adapter permittingContract () blockingOracle ∧
ConcreteSpec/Proofs/ObserveReport.lean:551: example : (requesting 3).final? = none ∧
ConcreteSpec/Proofs/ObserveReport.lean:564: example : ExecAdapter.WellFormed adapter permittingContract () answeringOracle ∧
ConcreteSpec/Proofs/ObserveReport.lean:584: example : ExecAdapter.WellFormed adapter permittingContract () answeringOracle ∧
ConcreteSpec/Proofs/ObserveReport.lean:651: example : reportObservation program relativePathAdapter permittingContract ()
ConcreteSpec/Proofs/ObserveReport.lean:670: example : ExecAdapter.WellFormed relativePathAdapter permittingContract () blockingOracle ∧
ConcreteSpec/Proofs/ObserveReport.lean:700: example : ExecAdapter.WellFormed relativePathAdapter permittingContract () blockingOracle ∧
ConcreteSpec/Proofs/ObserveReport.lean:736: theorem sourcePermitted :
ConcreteSpec/Proofs/ObserveReport.lean:743: example : ∃ sp bytes,
ConcreteSpec/Proofs/ObserveReport.lean:758: example : ([Demo.site] = List.map Prod.fst sourceAdapter.sourceInput.sites) ∧
ConcreteSpec/Proofs/ObserveReport.lean:788: example : ([Demo.site] = List.map Prod.fst sourceAdapter.sourceInput.sites) ∧
ConcreteSpec/Proofs/Own.lean:2964: theorem runFrom_erasure (P : Exec.Program (instrumentOperands O types permit))
ConcreteSpec/Proofs/Own.lean:3401: theorem runFrom_erasure_of_accepted
ConcreteSpec/Proofs/Own.lean:3905: example :
ConcreteSpec/Proofs/Own.lean:3928: example : ¬ (Exec.runFrom (eraseProgram emptyProg)
ConcreteSpec/Proofs/Own.lean:3969: example :
ConcreteSpec/Proofs/Own.lean:3982: example :
ConcreteSpec/Proofs/Own.lean:3994: example : ¬ (Exec.runFrom (eraseProgram emptyProg)
ConcreteSpec/Proofs/Own.lean:4326: example : Exec.runFrom (eraseProgram emptyProg) erasedOracle 100 [] (eraseConfig primTraceConfig) =
ConcreteSpec/Proofs/Own.lean:4331: example : Ending.NotStuck (Exec.runFrom emptyProg
ConcreteSpec/Proofs/Own.lean:4381: example : Exec.runFrom (eraseProgram emptyProg) erasedOracle 100 [] (eraseConfig primFrameStuckConfig) =
ConcreteSpec/Proofs/Own.lean:4387: example : CheckerAccepted erasedOps types permitE emptyProg primFrameStuckConfig ∧
ConcreteSpec/Proofs/Prim.lean:283: theorem PrimRegistry.allows_no_implicit_trap {O : Exec.Operands}
ConcreteSpec/Proofs/Prim.lean:299: theorem drive_steps_result (P : Exec.Program O) (ρ : Exec.Oracle O)
ConcreteSpec/Proofs/Prim.lean:366: theorem runStmts_of_not_normal (P : Exec.Program O) (ρ : Exec.Oracle O)
ConcreteSpec/Proofs/Prim.lean:380: theorem runStmts_terminal (P : Exec.Program O) (ρ : Exec.Oracle O)
ConcreteSpec/Proofs/Prim.lean:392: theorem ghost_erasure_run (G : Language O) (P : Exec.Program O) (ρ : Exec.Oracle O) :
ConcreteSpec/Proofs/Prim.lean:428: theorem ghost_erasure_ending (G : Language O) (P : Exec.Program O) (ρ : Exec.Oracle O)
ConcreteSpec/Proofs/Prim.lean:812: def dropHistoryDrive {O : Exec.Operands} (P : Exec.Program O) (ρ : Exec.Oracle O)
ConcreteSpec/Proofs/Prim.lean:817: example : ∃ tr,
ConcreteSpec/Proofs/Prim.lean:837: def restartingRunStmts {O : Exec.Operands} (P : Exec.Program O) (ρ : Exec.Oracle O)
ConcreteSpec/Proofs/Prim.lean:847: example : Ghost.runStmts GhostDemo.program GhostDemo.oracle 20 [] [.abort ⟨4⟩] returnedStart =
ConcreteSpec/Proofs/Prim.lean:853: example : returnedStart.final? ≠ some .normal ∧
ConcreteSpec/Proofs/Prim.lean:861: example : Ghost.runStmts GhostDemo.program GhostDemo.oracle 20 [] [.abort ⟨4⟩] returnedStart =
ConcreteSpec/Proofs/Prim.lean:867: example : Ghost.runStmts GhostDemo.program GhostDemo.oracle 20 [] [.abort ⟨4⟩] trappedStart =
ConcreteSpec/Proofs/Prim.lean:888: example : Ghost.runPieces GhostDemo.language GhostDemo.program GhostDemo.oracle 20 []
ConcreteSpec/Proofs/Prim.lean:896: example : ¬ (Ghost.runPieces GhostDemo.language GhostDemo.program GhostDemo.oracle 20 []
ConcreteSpec/Proofs/Prim.lean:906: example : (Ghost.runPieces GhostDemo.language GhostDemo.program GhostDemo.oracle 20 []
ConcreteSpec/Proofs/Prim.lean:918: example : ¬ ((Ghost.runPieces GhostDemo.language GhostDemo.program GhostDemo.oracle 20 []
ConcreteSpec/README.md:1: (module documentation or namespace context)
ConcreteSpec/Spec/Adequacy.lean:1: (module documentation or namespace context)
ConcreteSpec/Spec/Adequacy.lean:69: def QueryReport.toReport (r : QueryReport O) : Report O := ⟨r.events, r.ending⟩
ConcreteSpec/Spec/Exec.lean:93: inductive Life where
ConcreteSpec/Spec/Exec.lean:298: def register (d : Deferred O) : List (Frame O) → Option (List (Frame O))
ConcreteSpec/Spec/Exec.lean:842: structure Report (O : Operands) where
ConcreteSpec/Spec/Exec.lean:848: def runFrom (P : Program O) (ρ : Oracle O) : Nat → List O.Event → Config O → Report O
ConcreteSpec/Spec/Exec.lean:868: def run (P : Program O) (ρ : Oracle O) (fuel : Nat) (c : Config O) : Report O :=
ConcreteSpec/Spec/Exec.lean:873: def runFn (P : Program O) (ρ : Oracle O) (fuel : Nat) (fn : Fn O) (vs : List O.Val)
ConcreteSpec/Spec/Exec.lean:1020: def go (body : Stmt ops) : Report ops := run prog env 200 (start ⟨[], .int, ⟨0⟩, body⟩ [] .init)
ConcreteSpec/Spec/Exec.lean:1042: def brief (r : Report ops) : List Int × Option (Final Int) :=
ConcreteSpec/Spec/Exec.lean:1048: def isStuck (r : Report ops) : Bool := match r.ending with
ConcreteSpec/Spec/Exec.lean:1053: def isOutOfFuel (r : Report ops) : Bool := match r.ending with
ConcreteSpec/Spec/Exec.lean:1170: example : (fun r : Report ops => (r.events, isStuck r)) (go (.seq (.bind a (.lit 7) s)
ConcreteSpec/Spec/Exec.lean:1173: example : (fun r : Report ops => (r.events, isStuck r)) (go (.seq (.bind a (.lit 7) s)
ConcreteSpec/Spec/Ident.lean:701: def SourceText.columnAt (source : SourceText) (offset : Nat) : Nat :=
ConcreteSpec/Spec/Ident.lean:816: instance (sp : Span) (source : SourceText) : Decidable (sp.LocationOk source) := by
ConcreteSpec/Spec/Observe.lean:510: abbrev PhysicalOperands.runFrom {Output : Type} (M : PhysicalOperands Output)
ConcreteSpec/Spec/Observe.lean:606: def ExecAdapter.BlockingPermitted (P : Exec.Program O) (A : ExecAdapter O S)
ConcreteSpec/Spec/Observe.lean:618: def reportObservation (P : Exec.Program O) (A : ExecAdapter O S) (C : OperationContract S)
ConcreteSpec/Spec/Observe.lean:672: def Environment.history {S : EventSignature}
ConcreteSpec/Spec/Observe.lean:826: def refusedReport : Exec.Report refusingBindOps :=
ConcreteSpec/Spec/Observe.lean:857: example : ∃ k : Exec.Answer Int Int Exec.Demo.DState → Option (Exec.Config refusingBindOps),
ConcreteSpec/Spec/Observe.lean:869: example : ∃ k : Exec.Answer Int Int Exec.Demo.DState → Option (Exec.Config refusingBindOps),
ConcreteSpec/Spec/Own.lean:558: def eraseReport (r : Exec.Report (instrumentOperands O types permit)) : Exec.Report O :=
ConcreteSpec/Spec/Own.lean:853: def runBody (body : Exec.Stmt ops) : Exec.Report ops :=
ConcreteSpec/Spec/Own.lean:856: def stuck (r : Exec.Report ops) : Bool := match r.ending with
ConcreteSpec/Spec/Own.lean:926: def instrumentedMoveRun : Exec.Report instrumentedOps :=
ConcreteSpec/Spec/Own.lean:929: def erasedMoveRun : Exec.Report erasedOps :=
ConcreteSpec/Spec/Own.lean:1005: def callRun (body : Exec.Stmt callOps) : Exec.Report callOps :=
ConcreteSpec/Spec/Prim.lean:358: example : ¬ evidence.Usable { current with checker := ⟨0, 2, 0⟩ } ∧
ConcreteSpec/Spec/Prim.lean:407: structure SegmentReport (O : Exec.Operands) where
ConcreteSpec/Spec/Prim.lean:414: def drive (P : Exec.Program O) (ρ : Exec.Oracle O) :
ConcreteSpec/Spec/Prim.lean:435: def runStmts (P : Exec.Program O) (ρ : Exec.Oracle O) (fuel : Nat) (hist : List O.Event) :
ConcreteSpec/Spec/Prim.lean:449: def runPieces (G : Language O) (P : Exec.Program O) (ρ : Exec.Oracle O)
ConcreteSpec/Spec/Prim.lean:595: example : Ghost.runPieces language program oracle 20 [] pieces [] start =
ConcreteSpec/Spec/Prim.lean:599: example : (Ghost.runPieces language program oracle 20 [] pieces [] start).config.store = 5 := rfl
docs/archive/bugs-2026-11/bug_disposition.tsv:1: (module documentation or namespace context)
docs/archive/crucial-2026-11-06/DIAGNOSTICS.md:1: (module documentation or namespace context)
docs/archive/no-hidden-traps-2026-11-06/Q13_CODEX.md:1: (module documentation or namespace context)
docs/archive/roadmap-2026-11/ROADMAP_RECONCILIATION.md:1: (module documentation or namespace context)
docs/archive/roadmap-2026-11/ROADMAP_TASKS.md:1: (module documentation or namespace context)
docs/archive/roadmap-2026-11/ROADMAP_TASKS.md:2735: example or workload transcript before the larger package/release machinery
docs/archive/roadmap-2026-11/roadmap_restructure_2026-11-02.tsv:1: (module documentation or namespace context)
docs/archive/roadmap-2026-11/task_disposition.tsv:1: (module documentation or namespace context)
scripts/proofs/axioms.golden:1: (module documentation or namespace context)
scripts/push-both.sh:1: (module documentation or namespace context)
scripts/review/review.sh:1: (module documentation or namespace context)
```
