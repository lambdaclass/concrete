Kimi (kimi -p, read-only), owner questions round:

• Q1: pick c; runner-up b; independent-unit reporting avoids unproved recovery and a second front end, matching one-record diagnostics (`docs/decisions.md:821-833`) and workload-gated complexity (`docs/PHILOSOPHY.md:152-154`). Missing: `CheckResult` cannot currently carry refusals and `incomplete` together (`docs/proposals/SPEC_UNIFICATION.md:408-412`), which b’s reporting requires.

  Q2: pick a+i; runner-up a+iii; a preserves refusal/incomplete/usage/internal distinctions (`ROADMAP.md:470`) and policy outcomes (`docs/decisions.md:765-769`), while i reuses `facts` (`docs/decisions.md:896`). Missing: artifact-reuse rules; without a prior artifact, final-artifact stack facts make clean-tree checks incomplete (`docs/decisions.md:758-760`; `ROADMAP.md:458`).

  Q3: pick a without run; runner-up c; distinct trace/emit/interp/help commands expose distinct questions explicitly (`docs/PHILOSOPHY.md:59-66,131-133`); run duplicates build plus execution. The explicit a-without-run option is missing. B0 interp can cover pure slice-0 programs, but general I/O interpretation awaits the slice-1 driver (`ROADMAP.md:210,467`).

  Q4: pick a; runner-up b; legacy defines stdout/check/write/stdin (`legacy/Main.lean:4265-4304`), and stdin serves agents without file writes (`docs/PHILOSOPHY.md:40-43`); slice 17 is fixed (`docs/decisions.md:957-960`). Missing: reject combined modes as usage; legacy silently gives `--check` precedence over `--write` (`legacy/Main.lean:4291-4303`).

  Q5: pick a; runner-up d; generated capabilities avoid inferring support from schemas containing future `notApplicable` kinds (`docs/proposals/SPEC_UNIFICATION.md:787`); registry-derived views prevent drift (`docs/PHILOSOPHY.md:68-79`), and R-0593 supplies schemas (`ROADMAP.md:461`). Missing: d’s guide could be generated from command/next-action registries rather than maintained as prose.

  Q6: pick d; runner-up a; `Concrete.toml` is the typed, tightening, transitive policy surface (`docs/decisions.md:801-816`), policies consume facts fail-closed (`docs/decisions.md:744-768`), and R-0585 distinguishes point instances from full contracts (`ROADMAP.md:463`). Unlike safety obligations, unproved user contracts may remain reported facts (`docs/PHILOSOPHY.md:391-394`), so b is too strong.

  ANSWER: c, a+i, a without run, a, a, d — These choices keep diagnostics and policies fail-closed, reuse existing facts, and prefer explicit registry-backed interfaces.

