# C9 round 12: source identity and the trap-site table (Codex writes; agy reviews)

State: the branch is at 0551add93, and all seven gates pass. Its latest review, by agy, is a contract-valid ACCEPT.

The owner has decided the C6 source-path question. Read these first:
- _review/OWNER_DECISIONS_2026-11-09.md item 1, which carries the owner's verbatim quote;
- _review/C6-REVIEW4-codex.md finding 2 and its QUESTION, which this decision answers;
- _review/VACUITY_CONVENTION_FINAL.md and _review/BRIEF.md.

Under the decision, C9 owns source identity and the trap-site table. C6 and C10 will build on what you write in later rounds:
- C6 drops the path-validity premise `pathOk` and the lookup premise `found` from its theorem `runFrom_source_trap`. Its branch has a string `PathOk` and a filtering `sourceSites`, which your design replaces.
- C10 writes the refused-input examples.

So design the interface for those consumers.

## Best long-term design (owner rule, binding)
Judge everything by the best long-term design. Never cut corners for speed, a smaller diff or fewer rounds. Shortcuts are findings. A check that re-validates the invariant inside the operation, a filter that drops entries, or a law that holds by construction is a defect here, not a solution.

## Tasks
1. **A structured source path, in Spec/Ident.lean.** Define `SourcePath`: one project-relative path per source file, made of a non-empty list of components. A component is never empty, ".", ".." or drive-prefixed (for example "C:"), and contains no "/". The path is never absolute.
   - The invariants are carried by the type, not by a separately checked predicate.
   - It has exactly one rendering, the components joined by "/". Prove that rendering is injective.
   - Prove that the rendering never yields an empty string, ".", "..", a leading "/", or a drive prefix.
   - `Span`'s path field becomes this type. Update every use in your tree (Ident, Observe, Observe2, Prim, the proofs). Keep the trap-line format byte-identical to the decided rendering.
   - Character rules the decision does not state are open questions: backslash (the current C6 `PathOk` refuses it), NUL, and non-ASCII. Keep exactly the existing refusal set (the current `PathOk` rules plus the decided ones) and list every remaining question under QUESTIONS. Do not decide them.
2. **Parsing raw paths.** Write a parser from raw path strings to `SourcePath` that returns a refusal naming the offending input, with a typed reason: empty, dot, dot-dot/out-of-project, absolute, drive-prefixed, or empty component. It must never normalize silently. If you believe "a/./b" or a trailing "/" should be normalized rather than refused, that is a question; refuse until it is decided. Prove both directions:
   - parsing a rendering returns the same path;
   - every accepted string is that path's rendering.
3. **The loader.** Write the loader for a project's source files and sites. It either admits the complete input or refuses it with an error naming the offending input. Nothing is dropped and there is no fallback. Follow the pattern of the existing `LoadedModules.load`. The refusals are:
   - a malformed or out-of-project path;
   - two files with the same path;
   - a duplicate site identity;
   - a site whose file is not a loaded source file.

   On success it yields the source table and the trap-site table. Laws, each with closed Instances and premise-preserving Planted mutants under the final convention:
   - completeness: every input site appears with its own span, and no other site appears;
   - uniqueness: site lookup is functional;
   - every resolved span's path is a loaded file, and its location agrees with that file's bytes (reuse `SiteTable.Valid`/`LocationOk`);
   - refusal: each malformed input class is refused with a diagnostic naming it;
   - order independence of admission, if it holds. If it does not hold, say why.

   Mutants should include a loader that drops bad entries, one that keeps the first duplicate, and one that accepts a ".." path.
4. **The consumer theorem for C6.** State and prove a theorem that a successfully loaded project gives every site of its table a span with a valid structured path. Also prove that a trap at a site of the program's site set resolves in the table, given the program's site set is the loader's input. C6's `runFrom_source_trap` can then take a loaded project instead of `found`/`pathOk`. Document that interface in the doc comment. Do not edit C6's files; they are not on this branch.
5. **Re-tag and run N2.** Re-tag every changed or new theorem under the final convention, with new exemptions at `(approved: PENDING)`, and keep existing approvals of unchanged declarations. Run the N2 gate on your modules:
  export PWD_SRC="$PWD"; bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh bash -c 'cd /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/n2/repro/gate && timeout 1500 python3 scripts/proofs/vacuity_driver.py --source "$PWD_SRC" --scope ConcreteSpec.Proofs.Ident,ConcreteSpec.Proofs.Prim,ConcreteSpec.Proofs.Observe2'

   Report only new findings. instance-false-discharge findings are a known gate defect being fixed by the gates worker. Do not reshape evidence around them.

## Rules
- Do not change any other owner-decided rule, the ratified vacuity convention, or the meaning of any other normative definition. If a fix seems to need one, stop that item and put it in QUESTIONS with the options you see.
- Commit each piece after the build passes. Do not push.
- Do not edit scripts/proofs/axioms.golden, docs/decisions.md, ROADMAP.md or ISSUES.tsv.
- Never commit _review/ or scratch files.
- Lean 4.34.1 core only: no new axioms, no sorry, no native_decide.
- Keep tool output short.
- Final message: for each item, done or left (with why); each new definition and theorem with its statement in one line; commit SHAs; QUESTIONS.

## Build and commit gating (binding)
Run every Lean build as `bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh nix develop -c timeout 1200 lake build ConcreteSpec`. Gate every commit on the exit status of the check itself, with `set -o pipefail`. Never write `check | tail && git commit`. Do not run reviewers, and do not write git notes.
