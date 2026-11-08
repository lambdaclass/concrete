Kimi (kimi -p, read-only), final answer:

• - Rows 16, 45 and 46 should be (d), not (b): R-0448/R-0214/R-0451 are deferred-with-trigger, not removed; “no SMT” applies to admission, while separate solver evidence remains possible (`task_disposition.tsv:215,449,452`; `docs/decisions.md:596-602`).
  - Row 3 should be (a), not (d): R-0201 explicitly schedules `why` with transitive call chains, matching the legacy query (`ROADMAP.md:559`; `task_disposition.tsv:181-182`).
  - Row 51 should be (d), and U6’s prerequisite already exists: `CheckResult.refused` carries a nonempty `List Refusal`. Only exhaustive error recovery/collection remains an owner question (`docs/proposals/SPEC_UNIFICATION.md:405-417`).
  - U9 is too broad: generated obligation schemas already precede Admission, with semantics in `EvidenceSem`. Narrow it to exporting Lean propositions/stubs, not missing obligation statements (`docs/proposals/SPEC_UNIFICATION.md:650-676,785`).
  - U10 is not a prerequisite gap: `#[proof]` already requires exact current obligations or the function’s semantic identity, with a discharge edge per obligation. R-0482 adds richer identity separation (`docs/decisions.md:686-696`).
  - U4’s proposed identity is wrong: requesting `SiteId` is provenance, not specialization identity. Key instances by `(generic, arguments)` and record separate request edges, allowing many callers per instance (`review-extra/CLI_QUESTION_AUDIT.draft.md:55,153-155`).
  - Row 23/U11 must start at slice 0, not 1: slice 0 already elaborates generated temporaries and records origins, so their generating rule cannot be deferred (`review-extra/slice0_Concrete_README.md:70`).
  - Row 5’s earliest slice should split allocation (19) from cleanup (`defer`/`Destroy` at 14); recording all cleanup sites only at 19 is too late (`ROADMAP.md:224,229`).
  - Row 55 should be partial: legacy also distinguishes proof-check, solver and internal failures; R-0479 covers refusal propagation, while `incomplete` has no CLI exit mapping (`legacy/Main.lean:13-30`; `ROADMAP.md:470`).
  - Add general `help`, `--help` and `-h`: they answer command/flag discovery, distinct from `prove --help=agent`; no new-design help surface is anchored (`legacy/Main.lean:56-86,3921-3927`).
  - Row 67 misses `fmt --check`’s distinct CI question, “is this file already formatted?” Slice 17 names only a formatter; schedule check mode and document `--stdin`/`--write` (`legacy/Main.lean:4265-4304`).
  - The gaps table should mark agent feature discovery and debug-bundle commands as owner CLI-surface questions, like trace/emit/run; only their records and formats are scheduling (`review-extra/CLI_QUESTION_AUDIT.draft.md:176-186`).

  ANSWER: AGREE-WITH-CHANGES — Deferred work is misclassified as dropped, and several prerequisites and CLI contracts need correction before the audit is reliable.

