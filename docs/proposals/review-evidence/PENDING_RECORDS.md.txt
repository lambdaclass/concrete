# Owner answers to record (the decisions worker records them when Claude capacity returns)

- 2026-11-07, verbatim: "concurrency i want to leave at the last task".
  - Context: open question 5 (Codex, Kimi and agy all said keep it trigger-gated and audit the needs now).
  - Record: concurrency is the LAST item of the plan, after slice 27 (networking/ethlambda). It is not scheduled earlier.
  - Effects:
    - The legacy deletion trigger is effectively RocksDB (no concurrency needed), not ethrex.
    - The web server is sequential at 27.
    - Model when it comes (all three reviewers): structured/scoped spawn-join with linear ownership transfer, no shared mutable state. The model itself is not yet owner-decided.

- 2026-11-07, verbatim: "showcase i would like everything right?" (answering open question 4; I confirmed yes).
  - Record: all seven showcases are adopted, with the Codex/Kimi placements:
    - grep 16 → 18 (→ 22 dynamic patterns);
    - QOI 19 → 21, PNG 21–22;
    - EVM 15 → 21 → 23;
    - KV store 23 (durability "not established"; RocksDB comparison at 25);
    - Wasm 21 → 23 → 26;
    - crypto 7 → 15 → 18 file encryptor → tunnel 27 (after the randomness/nonce audit);
    - static web server 27, sequential (concurrency is last).
  - Each showcase states its functional, resource, authority and backend claims separately.
  - Reviewers: Codex and Kimi all seven; agy four (grep, QOI, EVM, tunnel).
  - Lands in docs/proposals/EXAMPLES.md → decisions.md + ROADMAP.

- 2026-11-07, verbatim: "ethlambda can't be done before conccurency".
  - Record: ethlambda moves from slice 27 to after the concurrency slice, which becomes the final slice (28).
  - Slice 27 (network) keeps httpget, tcpserve and the sequential web server.
  - Plan order: 27 network → 28 concurrency → ethlambda acceptance.
  - Follow-up question for the owner: whether the encrypted tunnel (needs both directions at once) also moves after concurrency.

- 2026-11-07, verbatim: "ok go ahead all yes then as you said right". It answers the remaining open questions with my recommendations:
  1. Step/time bounds: the step-counting cost model is built with loops at slice 4 and extended through calls at 5; proofs of step bounds are available at slice 8. Bounds count abstract operations in input/pattern size; wall-clock claims need separate backend evidence. (Codex and agy: 4+8; Kimi: 8.)
  2. Spec reading order: Codex, agy and Kimi read first; unclear points are resolved before the owner's formal read; real design questions still go to the owner; Kimi's absence is recorded, not waited on. Resolves the "Waiting for the owner" outside-readers proposal.
  3. `#[invariant]`: defined in the spec before the freeze; usable in source at slice 17 (modules). This revises the approved row's slice 9.
  6. Developer tools: test runner at slice 8 (with #[test]), extended to modules at 17; formatter at 17; LSP later.
  7. Performance: flagship runtime tracked from slice 2 and compiler speed (wall time, per-stage, memory) from slice 0, report-only until noise is understood, recorded by exact SHA/backend/target.
  8. Cleanup: do the Codex/Kimi list.
     - Simplify AGENTS.md (drop the legacy-only /file-bug, /fix-bug, ledgers and campaigns).
     - Make CLAUDE.md a one-line import of AGENTS.md.
     - Simplify SELF_HOSTED_RUNNERS.md, LANDING.md, ConcreteSpec/README.md, README.md, ROADMAP.md (dead check_roadmap.sh, "when wip/philosophy lands") and the PHILOSOPHY.md opening link.
     - Remove legacy handling from train.sh, bot.sh, resolve_conflicts.py and gates_remote.sh (including the false reuse claim).
     - Fix the stale parts of .claude/commands land.md and creview.md.
     - Refresh proposals IDEAS/MERGED/SPARK.
     - Fix changelog.d/2026-11-06-facts-report-readers.md ("models first" was a proposal; now decided per item 2).
     - Remove the dead prepush_record.sh and treestate.sh.
     - DELETE scripts/land/legacy_move.sh plus move_prereqs.py (git history keeps them), and the empty required-checks-transition.tsv (update inventory, retention and legacy_frozen diagnostics).
  - Tunnel: (a) the encrypted tunnel moves after the concurrency slice; the file encryptor at 18 is the encryption showcase until then.

- 2026-11-07, verbatim: "can we have reproducible builds since the beginning right?" (I confirmed yes).
  - Record: reproducible builds from slice 0. The same source, compiler revision, toolchain pin and target give byte-identical output: the BIR, the LLVM IR text, the LCB job and output, and the final binary.
  - Means:
    - pinned toolchain (nix flake, LCB rev, LLVM/clang version);
    - no timestamps, absolute paths or host names in artifacts (-ffile-prefix-map, SOURCE_DATE_EPOCH, deterministic link order);
    - deterministic ordering in the compiler (no hash-map iteration order in output).
  - Gate: build every slice-0 example twice in different directories, and on two machines (GitHub runner + concrete-ci-2), and compare sha256. Any difference fails.
  - The build identity (compiler SHA, toolchain pins, target) goes into `concrete facts`.
  - Owner of the work: the slice-0 worker (on wip/slice0) when Claude capacity returns.

- 2026-11-07, verbatim: "1 and 2 should be done early too". 1 = stack-overflow bounds and 2 = randomness, from my "what are we forgetting" list.
  - Stack bounds: from slice 0, `concrete facts` reports a proven or measured maximum stack use per function and per program, taken from the FINAL artifact (the existing decision: "stack comes only from the final artifact") for both backends. A program whose bound is unknown or exceeds the Concrete.toml stack budget is refused under the policy, so stack overflow is never a hidden trap. Slice 5 (recursion) extends it with measure-based bounds; unbounded recursion needs the termination/step evidence or is refused.
  - Randomness: an explicit entropy capability, granted to main like the other grants, arrives with capabilities (slice 6). With slice 7 its output is `Secret<T>`. Fill-bytes comes with arrays (15). Its source (e.g. getrandom) is a named trusted premise in facts.
  - These are my proposed slice placements; the owner asked for "early" without naming slices. Confirm with the owner if recording needs exact slices.

- 2026-11-07, verbatim: "ok add the reproducible asap" (work started on wip/slice0-repro).
- 2026-11-07, verbatim: "ok let's do 1, 2, 3, 4, 5". It answers my list of cheap early items:
  1. A trusted-base statement in `concrete facts`: what is not proven (Lean kernel, LLVM printer + LLVM, LCB's MIR→assembly step, linker, OS, named runtime premises), from slice 0.
  2. A random-program fuzzer differential against the Lean evaluator, starting with the slice-0 front end and growing one feature per slice; it runs on concrete-ci-2.
  3. Sanitizers: every example's LLVM output is also built and run under AddressSanitizer and UndefinedBehaviorSanitizer in CI, from slice 0.
  4. Binary size tracked per example per commit, next to speed tracking, report-only.
  5. A frozen, versioned JSON schema for diagnostics and facts output, with golden tests, from the first front-end output.

- 2026-11-07, verbatim: "ok let's add all this right? where is the best place? should we track also in docs and roadmap or changelog?" (after my home/gate table).
  - Tracking rule (one home per fact): decisions.md = the decision and the owner's quote; ROADMAP.md = the work and its slice, plus for each item its home (spec or tooling) and its gate; changelog.d = what landed, written at landing time; the Lean spec = language meaning (stack-bound claim and budget refusal, entropy capability and premise, trusted-premise list, diagnostics/facts types as the schema source); Concrete/README.md + required-checks.txt = the gate list; check scripts + CI = enforcement. No new docs.
  - Gates:
    - repro (2 dirs, 2 machines, hash compare);
    - stack (final-artifact bound vs claim);
    - trusted-base golden;
    - fuzzer (short in CI, long nightly on ci-2);
    - ASan/UBSan;
    - speed/size report-only;
    - JSON schema generated from the Lean types, plus goldens.
  - All gates join required-checks.txt when the compiler lands on main.
  - Third Kimi pass: put the home and gate on each ROADMAP item.

- 2026-11-07, verbatim: "ok to option b then for freeze the foundation>?" (answers "spec before the freeze": (b) the foundation then per slice).
  - Before slice 1 builds on them, the shared foundation is fully written in the Lean spec and frozen: types, values and layout, ownership and places, capabilities and grants, outcomes/traps, the step model's shape, the diagnostics/facts types.
  - Each later slice adds its own rules on top of that foundation, without rewriting it.
  - Changing a frozen foundation definition is an owner decision; earlier proofs are re-run, and the change is recorded in decisions.md.

- 2026-11-08, verbatim: "ok to 1". This answers legacy sweep question 1: legacy_frozen.sh may accept an addition under legacy/ only if it is byte-identical to the same root path at a named commit and the file is gone from the root; every other addition or edit stays refused, and the rule gets negative controls. Question 2 (move the P21-retained decision archives) is NOT answered; those files stay at the root.
- 2026-11-08, verbatim: "ok to 2". This answers legacy sweep question 2: move all of docs/archive/ (including the decision sources and the reconciliation/restructure records P21 retained) under legacy/, and revise P21 accordingly. decisions.md stays the one home of decisions and verbatim quotes, git history keeps the originals, and the final ideas sweep before LD checks them.

- 2026-11-08, verbatim: "ok all recommended". This answers the 16 foundation questions of FOUNDATION_PROPOSAL.md (final: scratchpad/found8/codex/_review/FOUNDATION_PROPOSAL.md; reviewed by Codex, Kimi, agy in four rounds plus Claude; edit check OK by Codex and Kimi):
  1 approve the C1–C10 interface/law boundary, freeze only after Lean implementation, proofs, tests, four-model review and owner acceptance;
  2 the hybrid state (structured places + byte/provenance blocks, materialized only on raw address-taking/expose/foreign);
  3 read-only target profiles, existing 64-bit profile unchanged;
  4 keep i32 enum tags for now, compact tags decided at the layout slice;
  5 niches (b): none now, future niches only by owner decision;
  6 release-context erasure only with a preservation proof;
  7 (c) u64 meaning everywhere per Q3.5, with proved narrower representations;
  8 keep R-0505's later native-image closure and Q8.1's conditional endpoint;
  9 trap kind names decided before the first slice that prints them;
  10 byte columns, a tab counts one byte;
  11 UTF-8 failure classification deferred to the text slice;
  14 Entropy shape deferred to slice 6;
  15 the SPARK exposure definition in the spec before the freeze, the source feature at slice 17;
  16 lawful extensible registries, unknown operations refused;
  17 keep the decided backend-order equality, plus a per-native-trace Core refinement check, with allocation-dependent observations decided at slice 24;
  18 Secret<T> has T's layout.
  Removed as already decided: 12 (OD106 try_convert -> Result<T, OutOfRange>), 13 (Q2.3/Q3.8 destruction orders).
  Also pending, from 2026-11-07: "ok to option b then for freeze the foundation>?".

- 2026-11-08, verbatim: "ok go ahead with all the decisions". This answers my list of 11 (all as recommended):
  1 drop UBSan from the slice-0 gates (it instruments C at clang's front end, so it checks nothing on LLVM IR input); record why; ASan stays;
  2 make the adapter theorem complete: an adapter error produces no artifact, and success implies correctness (retires the P_adapter premise);
  3 shrink the landing scripts: keep train, review leg, gates and legacy/ protection; drop bot, notify, install-bot, systemd units and duplicate helpers; old copies move to legacy/;
  4 Claude quota goes to the critical path (spec, slice 0, front end); mechanical work goes to Codex/Kimi writers;
  5 start the lexer and parser now, in parallel with the foundation;
  6 one writer for docs/decisions.md and ROADMAP.md at a time;
  7 review rounds capped by risk: docs/roadmap 2, scripts 3, compiler/spec 5;
  8 one clones folder (~/projects/clones/<branch>), removed after the branch lands;
  9 a no-libc runtime on the proven path as a later slice (~24/25); libc stays a named premise until then;
  10 a compile-speed budget, tracked per example, report-only like binary size;
  11 ISS-0014 F7 wording: "every function has termination evidence with its premises, or a report naming the explicitly permitted unresolved obligations".

- 2026-11-08, verbatim: "ok let's do 2, for now let's not do 3". Item 2 is approved: a std-per-slice table in ROADMAP (which std module arrives in which slice, with its contracts/proofs and the showcases that need it). Item 3, a user-guide chapter per slice, is NOT now. Items 1 (a written grammar for owner review before the parser) and 4 (a speed target set at the first showcase) are unanswered.
- 2026-11-08, verbatim: "ok do the written grammar". Item 1 approved: before the parser is built, the full surface grammar is written down for owner review (every construct, its decided row, open spellings flagged); after approval it is encoded in Lean as the independent grammar judgment (Q8.3), and only then is the parser built against it. Item 4 (speed target) is still unanswered.
- 2026-11-08, verbatim: "and yes speed target is fine". Item 4 approved: speed and size stay report-only until the first real showcase (grep, slice 16); then a performance target against C is set from measured data, as an owner decision.
- 2026-11-08, verbatim: "i want a comparison of capabilitiy yes" (asks for the side-by-side comparison of today's with(...) permissions + grant values vs capabilities-as-values only; not a decision to merge).
- 2026-11-08, verbatim: "i don't see an isue with noisier if it's clearer". This answers the capability comparison: keep TODAY's design, with(...) permission sets plus grant values. The noisier generic callbacks (cap E, explicit <cap {...}> at call sites) are accepted because the signature ceiling is clearer. All four models recommended KEEP. Follow-up for the allocation slice: agy's note that destroying a collection needs with(Alloc) although release metadata travels with it.
- 2026-11-08, verbatim: "i prefer fully gone, self and as are irrelevant from what i see". This answers the reserve-or-remove question for `self` and `as` (after items 7 and 11-full remove receivers and every use of `as`): neither is reserved; both are ordinary identifiers with no special meaning. Rust-style `&self` or `x as u32` gets an ordinary parse/resolution error, not a dedicated diagnostic. Item 40's reserved list becomes spec, by, kept, Self, while, Entropy (obsolete reservations dropped). The 59-item list itself (scratchpad/verdict2/LIST.md + verdict3/DELTA.md + verdict4/DELTA.md, reviewed unanimous by Codex/Kimi/agy except 11(b) imports, in round 5) is NOT yet approved.
- 2026-11-08, verbatim: "record, yes to all recommended". This approves the full language/process list as recommended: scratchpad/LANGUAGE_DECISIONS_2026-11-08.md (items 1-59, with item 11 = remove `as` entirely and item 40 per the "fully gone" answer). Includes item 5 (land slice 0 at 0e2950e7). Item 11(b), how two same-named imported items are used, is still under review (round 6, options 1-3, Claude recommends 2) and is NOT covered by this approval; it goes back to the owner.
- 2026-11-08, verbatim: "i like 3". This answers item 11(b): option 3, always qualified. Imports stay item lists (`import std::json::{parse};`, MOD-2 manifest and OD139 kept); every imported item is used as `<module last segment>::name` everywhere (values, types, patterns, theorem paths), never bare; items declared in the same module stay bare; builtin types (u8, bool, ...) stay bare. Two imports of the same item name from different modules are allowed (revises MOD-3). Two imported modules with the same last segment are refused. A local named like an imported module segment is refused (no shadowing). Reviewers round 6: Codex 3, Kimi 3, agy 1; Claude revised to 3. Exact spelling in the spelling round.
- NEXT RECORD BATCH (not owner answers; for the decisions writer after record-language lands):
  - ROADMAP: Concrete/README.md gate 2 for Concrete/ is unbuilt (sorry/native_decide ban over all constants, totality ban, external replay); only the trusted-base golden covers headline theorems' axioms today. (slice-0 worker, 2026-11-08)
  - Sweep record text (migration worker): P5 "Revised 2026-11-08 (owner, "ok to 1"): legacy/ also grows by byte-for-byte sweeps recorded in scripts/checks/legacy.lock; no file, once frozen or swept, is ever rewritten"; restart row "(revised 2026-11-08: or a recorded byte-for-byte sweep, see P5)"; sweep row: the check judges landed history on main (no lock line rewritten/removed; no legacy/ file changes from a version it had there).
  - Review-tool issue proposal: scratchpad/review-tool-issue.md (delta vs merge-tree of reviewed SHA and base; outside: findings not counted, filed for main's owner).
- 2026-11-08, verbatim: "sign off ea5181b5, 1547734f8, a27fbb842, yes redesign". Owner sign-offs under process item 1 (exact SHAs reviewed by Codex, Kimi and agy):
  - wip/slice0 at ea5181b535fd2983abfcafe31b3ff7bf59ec6439 (Codex verification ACCEPT, Kimi ACCEPT, agy ACCEPT after withdrawing; branch CI 42451061873 18/18);
  - wip/record-language at 1547734f86e83d381f6cac2f25d2810b083bc9ed (Codex, Kimi verification ACCEPT, agy ACCEPT);
  - wip/one-repository (B) at a27fbb842 (all three ACCEPT at cap 3).
  - "yes redesign": guard A and the acceptance shim are redesigned: shrink the landing scripts first (process decision 3: drop bot, notify, install-bot, systemd units), then B (one repository, lambdaclass/concrete only) lands BEFORE the foundation freeze, then server-side protection of main (GitHub rulesets/branch protection: only a train merge with required checks green updates main) as the real boundary, then a much smaller local guard as a convenience. wip/remote-guard (b60670804) and wip/acceptance-shim (eca268078) are superseded.
- 2026-11-08, verbatim: "also remember we have agy not only codex kimi and claude" (said after my "is there anything we are forgetting" list, before the grammar/written-grammar answers). Related owner quotes the same day: "ok but review the foundation with all the models right? agy, kimi and codex apart from claude that is us"; "use agy laso"; "have everything reviewed by the other llms"; "ok launch codex kimi and agyh review the stdlib deisng and where to do what and when". Together: agy is the fourth model and reviews everything alongside Codex and Kimi.
- 2026-11-08, verbatim: "commit and push everything all the time" (relayed by the coordinator). Commit each piece as soon as it builds and its checks pass, push to the remotes at once on wip/ branches from a full clone with the hook on; main changes only through the train.
- 2026-11-08, verbatim: "can you have kimi, codex and agy implement things and we just review it". Codex, Kimi and agy implement; Claude briefs, reviews, gates and lands.

## MORNING QUEUE additions (06:25 2026-11-08, migration worker)
- Owner sign-off asked: legacy sweep wip/legacy-sweep-2 @35b489e4 (Codex writer; Kimi ACCEPT, agy ACCEPT). Also: its landing slot (old order said "after the shim"; shim superseded).
- Owner review: docs/proposals/MAIN_PROTECTION.md @d6159bf6 (wip/main-protection); Codex+agy ACCEPT, Kimi writer. Owner applies rulesets.
- B will need a fresh sign-off after rebase onto the shrink (install-bot.sh deleted).
- Deployed landing-bot service, if any, must be stopped on the server by hand.
- Record text for decisions writer: P8 revision (landing bot removed, "yes redesign"); 2026-11-07 cleanup row bot.sh done by deletion once shrink lands.

## Next decisions batch (queued 2026-11-08 by the coordinator, from the migration worker)
- P8 still lists the landing bot as a kept server service. Proposed: "Revised 2026-11-08 (owner, "yes redesign"): the landing bot is removed; the landing services are the train, its review leg, the hook and the gates."
- The 2026-11-07 cleanup row names bot.sh: once landing-shrink (R-0617) lands, mark that item done by deletion.
- Coordinate rebase with wip/legacy-sweep-2, which edits decisions.md (sweep row, P21 revision, MERGED link).

## MORNING QUEUE additions (spec worker, ~06:30)
- Foundation Q1 (fallible op in const init): Codex+Kimi = refuse (closed always-succeeds subset); agy = admit arithmetic, refuse failing evaluation. All three add: admit only with closed evidence of success. Note: strict subset excludes even 1 + 2 today.
- Q2 (forward const refs): all three = order-independent, cycles refused (report dependency path).
- Q3 (pointer from partial bytes): all three = no pointer meaning; missing option = separate named bytes-to-address conversion, no provenance.
- Confirm: C1 amendment life : Life → Store → Option Store (refused lifecycle event = stuck) treated as spec structure; not landed on main before owner sees it.
- C1/C2 accepted round 4 (d62d6cac1), spec-core head 5bc0748b8 all 7 gates; C4 80b23d59 gates pass, round 2 in progress; ISS-0017 fixed.
- Spec worker pushed earlier wip commits without pre-push hook (hooksPath unset); fixed.

## MORNING QUEUE additions (std coverage audit, ~06:35)
- docs/proposals/STD_COVERAGE.md @abc648e4 on wip/std-placement (PROPOSED; round-2 cap reached, draft-3 fixes unreviewed).
- Gaps G1-G6: G1 safe replace/take through &mut (OD13 decided, unscheduled; needed by OrderedMap at 23); G2 line framing; G3 fs op signatures; G4 bit ops; G5 float ops + NaN; G6 encryptor large files.
- Owner questions S1-S5: S1 safe replace at 13 + view swap at 15 (recommended); S2 two shared refs from one view; S3 stream interface shape; S4 traversal callback type; S5 float/NaN (canonical NaN recommended).
- Probes D1a-D4; flags F0-F8.

## MORNING QUEUE additions (slice-0 worker, ~06:45) — BLOCKS slice 0
- wip/owner-signoff @fb097fa4 hit scripts cap 3: Codex review.sh clean, agy ACCEPT, Kimi REJECT (2 real findings: transcript bound by any SHA mention; verdict read from whole transcript incl. request). CI 42454431868 green, root 11/11.
- Choice: (a) sign off fb097fa4 now, land, then the split-format fix (wip/owner-signoff-split, being built) lands before any further sign-off record — worker recommends; or (b) waive a 4th round on this branch to implement the split format first.
- Slice 0 (ea5181b5) sign-off record waits on this.
- UPDATE ~07:00: follow-up wip/owner-signoff-split @b7f30d36 (Kimi writer) closes both Kimi findings; Codex review.sh round 1 clean, agy ACCEPT; branch CI 42456223791 queued. Simplest owner answer: sign off fb097fa4 and land it together with b7f30d36 in one train.

## MORNING QUEUE additions (slice-1 brief @19561a9d wip/slice1-brief, ~07:15)
- New owner questions (no decided row, not in R-0625): row 10 how a struct is marked Copy; row 14 `move t` vs plain `t` for reading a linear value; row 16 where abort() may appear in a slice-1 body; row 18 Concrete.toml [policy]/[assumptions] key names + version field.
- Spelling round (R-0625) pending rows: 8,9,11,12,13,15,17. Spec gap: slice-1 spec lacks abort() (first, spelling-free work).
- UPDATE slice-1 brief @906b391b0 (round 2 of 2 running): Copy-struct question moved to slice 9 (dropped from slice-1 list). New owner choice: slice-1 spec wider than slice 1 — (a) compiler admits a defined subset with a pass-preservation proof, or (b) narrow the spec. Roadmap inconsistency R-0600 (B1 vs slice 5) → decisions writer. AGENTS.md still says Claude writes spec/compiler; writers rule (2026-11-08) is newer → decisions writer to align.
- (slice-1 brief review) AGENTS.md process text still assigns spec/compiler writing to Claude in places; align it with the 2026-11-08 writers rule (Codex/Kimi/agy write; Claude briefs, reviews, gates, lands). Process text only, no owner authority added.
- (slice-1 brief review) ROADMAP inconsistency: queue row B1 (ROADMAP.md:187) lists R-0600 (derived "always aborts" fact), but R-0600's own row (ROADMAP.md:476) says "due with the functions-and-calls slice". No decision settles which (checked: OD116, P18, decisions rows citing OD116 do not place it). Do NOT pick: record a revision-note proposal / owner question naming both placements; until answered, flag both rows as inconsistent pending the owner.
- Owner question: R-0600 (always-aborts fact) slice 1 (ROADMAP B1 :187) or slice 5 (its row :476)? No decision settles it.
- UPDATE slice-1 brief @28b42ade9: docs cap reached (Codex+Kimi REJECT round 2, findings applied unreviewed); kept as DRAFT proposal, nothing launches from it. Scope question: worker recommends (a) compiler admits stated subset of slice-1 spec (like slice 0). abort() result type is also an owner question. fn main() -> u8 / typed let spellings pending R-0625.

## MORNING QUEUE additions (lexer inventory, ~08:00) — add to spelling round R-0625 explicitly
- Lexer blocked on spelling: (1) keyword/reserved table; (2) builtin-name table contents (item 32 list = OPEN-4); (3) QUESTION: did item 34 approve the grammar sheet's §2 operator/punctuation token set?; (4) QUESTION: second escape list — sheet had the char list, item 16 removes char; which literal does it belong to, contents, and 'label vs quote; (5) QUESTION: raw control chars other than LF/CR (e.g. tab) inside quotes; (6) integer suffix grammar + base prefixes (items 41/59).
- Launched meanwhile: identifiers, string escapes \n \t \r \\ \" \0, parametric longest match, decimal literals (stacked on e474d8f0).

## OWNER ANSWER 2026-11-08 ~14:00 UTC (verbatim): "yes to 6 things"
Context: Claude's list "is there anything we found tonight that we have to do better or change the architecture or make things simpler": 1 vacuity rule (counter-example per law, reviewer check, audit accepted contracts); 2 axioms golden → property check (only standard axioms) + pin named theorems only; 3 integrate accepted contracts into spec-core continuously (A2i start); 4 freeze process tooling after owner-signoff(+split) and landing-shrink land, and land independent branches independently; 5 one shared launcher that always pairs a job with a bounded watcher; 6 every owner question gets the Codex+Kimi+agy round before it reaches the owner.
- 2026-11-08 ~14:00 UTC, verbatim: "yes to 6 things" (relayed by the coordinator). Approves Claude's six items: (1) vacuity rule: every spec law needs a non-trivial instance and a planted counter-example that must fail; reviewers check each law for vacuity; (2) the axioms golden becomes a property check (only standard axioms) with only named theorems pinned; (3) accepted foundation contracts merge into wip/spec-core continuously (start of A2i); (4) process tooling is frozen once owner-signoff (+ its split follow-up) and landing-shrink land, and independent branches land independently instead of being chained; (5) one shared launcher pairs every background job with a bounded watcher; (6) every owner question gets a Codex+Kimi+agy round before it reaches the owner. Record quote + meaning in decisions.md; process detail (how) in AGENTS.md.

## OWNER ANSWER 2026-11-08 (verbatim): "sign off fb097fa4"
Context: wip/owner-signoff @fb097fa4 reached scripts cap 3 (Codex review.sh clean, agy ACCEPT, Kimi REJECT 2 findings fixed in follow-up b7f30d36). Asked after Claude listed the landing order.

## OWNER ANSWER 2026-11-08 (verbatim): "do this with kimi, codex and agy and review it" [pasted items 2-5: spec-vs-compiler comparison + growing random generator; one manifest test format; scaling + determinism checks; trusted-base list] "we should have ci or gates for scaling, determinism, comparison in the spec and the compiler"
NOT included: item 1 (showcase sketches before freeze).

## OWNER ANSWER 2026-11-08 (verbatim, relayed by the coordinator): "ok wuith this but we have to add it to the roadmap or a slice"
About: "the comparison ships now and compares at the back-end level (spec semantics vs LLVM binary vs proven-path binary and its evaluator); it leaves a fixed hook for the full path, and the branch that adds lowering must also switch on the full spec comparison". Recorded on wip/roadmap-gates with the four gates A-D (R-0632..R-0635).

## 2026-11-08 owner (verbatim)
"you can use codex without sandbox it's okay"

## 2026-11-08 owner (verbatim), on the gate proposals (improvements C1-C6, D1-D3, A1-A5, T1, G1-G3 and new gates 1-12)
"ok add all the gates but we should run some of them in every pr and others every day and others every week?"

## 2026-11-08 owner (verbatim), on the gate tiers
"ok let's add the tiers when you get back"

## 2026-11-08 13:42 UTC owner (verbatim), the approved gate items 2-5
The owner pasted Claude's earlier proposal and wrote: do this with kimi, codex and agy and review it "<pasted items>" we should have ci or gates for scaling, determinism, comparison in the spec and the compiler
Pasted item 4, verbatim:
4. Scaling and determinism checks.
   - Scaling: compile programs of 1×, 10× and 100× size and fail if compile time grows faster than linearly. The old compiler shipped an O(n²) pass (bug 027) that nobody noticed for months.
   - Determinism: compile everything twice and require identical bytes.
(Items 2, 3 and 5, verbatim, are in the session transcript at 2026-11-08T13:42:09Z; see the gates worker brief.)

## 2026-11-08 15:08 UTC owner (verbatim), answering the 29 questions
"ok but i said already all yes based on the best long term reviews of each agent and what we said before right?"
Taken as: all 29 decided as Claude's final picks after the three-model rounds, including the revised 3, 14 and 18:
- 3: `'a'` is a u8 ASCII literal, with tests;
- 14: `help` now, no `run`, which revises the slice-0 surface;
- 18: fallible primitive operations in a closed initializer grammar, with no calls, loops or effects; success is kernel-checked, every initializer is evaluated, running out of fuel refuses, and TYPE-22 is revised.
The final list as shown to the owner is in the session transcript at 2026-11-08T15:08Z.
Item 28: the owner applies d6159bf6 personally.
Item 27: the sweep 35b489e4 is signed off.
B e997548d was asked separately (item 30) and is not covered by this reply.

## 2026-11-08T15:11:33Z owner (verbatim)
"sign off e997548d i amn gonna grab a coffee, gonna come back in an hour"
Full SHA: e997548d93482f37181dc0a523a2a06bca40a77c (B, wip/one-repository-2). Reviewers: Codex ACCEPT (fix check, full access), agy ACCEPT; Kimi = writer, did not review.

## 2026-11-08T15:13:46Z owner (verbatim), on test types T-a..T-j
"ok let's add all this" followed by the pasted list of ten test types (1 rewrite/metamorphic, 2 exhaustive 8-bit arithmetic, 3 validity check after every pass, 4 valgrind on compiled programs, 5 C layout/calling convention vs clang, 6 exact error messages, 7 pinned theorem statements, 8 limit tests, 9 termination refusals, 10 a test for every bug). The ten are T-a..T-j in gates/GATE_PROPOSALS_2026-11-08.md. All APPROVED; the three-model round assigns tiers and build details only.

## 2026-11-08T15:14:36Z owner (verbatim)
"ALWAYS tell the reviewrs to do the best long term thing rather than cuitting corners"

## 2026-11-08T15:20:52Z owner (verbatim)
"i want to have elm like errors for item 1 so we need to make sure they are good"
Context: asked right after the 12 architecture items K1-K12 (item 1 = source provenance through every IR) and the three C5 questions; interpreted as K1 / diagnostics quality.

## 2026-11-08T15:21:26Z owner (verbatim)
"yes to everythiong"
This answers the three pending asks:
1. C5 Q1-Q3, all as recommended.
   - Q1: loans target only the current activation or the entered callee; upward loans are refused.
   - Q2: an owner passed directly as an argument gets a call-duration root loan. Kimi's conditions: a deferred call holds the loan until it runs; no implicit ending of an existing loan.
   - Q3: the C1 erasure contract is unconditional simulation, plus stuck-iff-stuck only for programs the checker accepts.
2. The runner label: add a dedicated concrete-ci-2 label to the concrete-ci-2 runners (origin gha1-3, lambdaclass gha4-8), recorded in the dotfiles servers/ provisioning.
3. Add all 12 architecture items K1-K12 on the plan shown. K5, the optimization architecture, still comes back as its own owner question. K13, Elm-quality diagnostics, is the owner's own direction.

## 2026-11-08T15:21:44Z owner (verbatim), confirming
"yes to adding all the 12" / "add all 12" / "the three things are all good"

## 2026-11-08T15:31:59Z owner (verbatim)
"okay with your decisions"
This answers three pending asks, with Claude's picks as shown:

### Lexer spellings
Block (a)-(d) unanimous:
- (a) Byte-literal escapes are exactly \n \t \r \0 \\ \'. \" and \xHH are refused. Raw content is U+0020-U+007E excluding ' and \.
- (b) Strings refuse raw: all of Cc (U+0000-U+001F, U+007F-U+009F), U+2028-U+2029, and the bidi controls (U+061C, U+200E-U+200F, U+202A-U+202E, U+2066-U+2069). This is explicitly approved as a new restriction. Byte literals allow raw U+0020-U+007E only.
- (c) Prefixes are lowercase only (0x/0b/0o), and hex digits are lowercase only.
- (d) The suffix set is exactly i8 i16 i32 i64 u8 u16 u32 u64, on every base, with no float suffixes. Malformed adjacent text is one refused token.

The two split questions:
1. A raw TAB inside a // comment is PERMITTED. Other Cc, U+2028/9 and bidi controls are refused in comments, and LF/CR end a comment.
2. Suffixes are COMPLEMENTARY. A suffix is required where no expected-type position (OD138) exists, and refused where one does. This revises OD31/OD138, and the Q6.2 example becomes S { a: 200 }.

### C9 identity
1. FunctionRelativeKey is a structured path of (construct kind, name) segments. Unnamed items are keyed by an ordinal among same-kind siblings under the nearest named ancestor, with an optional user label to pin one. There are no raw child indices.
2. PackageId has structured name and version fields, with no content hash; a hash belongs to artifact identity. The same name and version from different origins is refused at load, consistent with OD44.
3. A new premise category is added only when a decided row names it. The schema is versioned, unknown categories are refused, and there is no open-string escape hatch.

### Codex login
Codex device-auth re-login for agent on concrete-ci-2 (option A: main's review.sh on concrete-ci-2 for review-access). Approved.
