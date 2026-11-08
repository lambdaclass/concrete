Codex (codex exec -s danger-full-access, in a git-archive copy of origin/main b6a647be), final answer:

- Rows 16, 45 and 46 confuse deferred features with deliberate removal. R-0448 retains optional cross-kernel replay; R-0214/R-0451 retain solver evidence under triggers. Classify as deferred/partial, not dropped (`docs/archive/roadmap-2026-11/task_disposition.tsv:215,449,452`).

- Row 17’s OD124 anchor does not establish removal: it explicitly approves refusal profiles without behavioural changes (`docs/decisions.md:1414`). Legacy profile reporting already prints policy bundles without code generation (`legacy/Main.lean:3885`); distinguish replacement by policies from dropping the question.

- U1 is an implementation gap, not an absent design prerequisite: C9 explicitly requires distinct obligation identities (`docs/proposals/FOUNDATION.md:121`). Keep the reconciliation task against Ident, but remove “current design does not provide” for this requirement.

- U2 likewise overlooks C9’s explicit “stable source sites” requirement (`docs/proposals/FOUNDATION.md:125`) and R-0492’s persistent evidence identity (`ROADMAP.md:440`). Flag unresolved implementation and freeze sequencing, rather than an unplanned requirement.

- U10 overstates missing design coverage: C9 already requires evidence bound to typed identities, contracts, target and versions (`docs/proposals/FOUNDATION.md:123`). Separate implementing that binding before slice 8 from R-0482’s broader four-identity architecture.

- U7 bundles an owner decision into “scheduling”: source-column units remain explicitly undecided (`review-extra/speccore_Ident.lean:151`). Reconciling span representations is scheduling; choosing bytes versus Unicode scalar columns requires the owner.

- Row 18’s layout key is insufficient: `DeclId` excludes scalar, pointer and instantiated generic types. Require concrete type identity, including specialization arguments and target; Memory CFG is the layout authority (`review-extra/slice0_Concrete_README.md:76`).

- U4 should separate specialization identity from request provenance. Multiple requesting sites can share one specialization; including requesting site in its identity defeats that sharing. Record specialization-to-request edges separately (`review-extra/CLI_QUESTION_AUDIT.draft.md`, row 20 and U4).

- Row 51 incorrectly calls `partial` a CLI flag. It is a returned completeness indicator from the error-tolerant diagnostics path (`legacy/Main.lean:4361`). Preserve the question about multiple errors, but correct the legacy surface.

- Row 55’s “covered” classification exceeds its anchor: preserving refusals through exit codes does not specify distinguishable failure categories (`ROADMAP.md:470`). Mark partial until the exit-code taxonomy and command-specific contracts are defined.

- Add the diff-specific CI contract: weakening exits 1, incompatible fact comparison exits 2 (`legacy/Main.lean:4086`). Row 61 covers detecting drift but omits machine-distinguishable incompatibility versus weakening.

- Row 60 omits external reduction predicates and message-qualified predicates (`legacy/Main.lean:4222`). Stage plus rule identity cannot represent arbitrary external predicates; classify those separately instead of treating U5 as sufficient.

- Rows 21/U11 should distinguish promised provenance from implemented BIR provenance. The README promises every instruction’s origin (`review-extra/slice0_Concrete_README.md:78`), while BIR’s site table describes trap sites (`review-extra/slice0_BIR.lean:76`); reconcile non-trapping instruction provenance before declaring implementation coverage.

ANSWER: AGREE-WITH-CHANGES — The inventory needs corrected deferred classifications, clearer distinctions between planned and implemented prerequisites, and several additional machine-facing contracts.
