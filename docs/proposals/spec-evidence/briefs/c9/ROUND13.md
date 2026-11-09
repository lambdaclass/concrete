# C9 round 13: the path character rules (Codex writes; agy then reviews rounds 12 and 13 together)

State: the branch is at 869c75f1a, round 12. In round 12 you asked three questions: backslash, NUL and non-ASCII. The owner is away. The coordinator has resolved them as assumed-yes, pending owner confirmation, as the best long-term options:
- **Backslash (AY-2):** refuse permanently; keep the existing refusal.
- **Control characters (AY-3):** refuse NUL and every other control character, U+0000 through U+001F and U+007F. The refusal names the input and has its own typed reason.
- **Other Unicode (AY-4):** accept every other Unicode scalar value. A path's identity is its exact scalar sequence, with no normalization. Do not add NFC; it is a possible later refusal, outside this round.

## Best long-term design (owner rule, binding)
Judge everything by the best long-term design. Never cut corners. Shortcuts are findings.

## Tasks
1. Encode the three rules in the `SourcePath` component invariant, in the parser's typed refusal reasons, and in the rendering proofs. The rendering must stay injective, and parsing a rendering must still return the same path. Doc comments must say these rules are assumed-yes pending owner confirmation, and must name the decision IDs AY-2, AY-3 and AY-4.
2. Add laws or extend existing ones, with closed Instances and premise-preserving Planted mutants under the final convention:
   - a NUL or other control character is refused with its reason;
   - a backslash is refused;
   - a non-ASCII component (for example "é") is accepted and renders exactly;
   - two paths that differ only in Unicode normalization (precomposed "é" versus "e" plus a combining accent) are different identities.

   Mutants should include a parser that accepts control characters and one that normalizes.
3. Re-tag under the final convention. New exemptions get `(approved: PENDING)`. Run N2 on Ident, Prim and Observe2 as in round 12, and report only new FAILs.

## Rules
- Do not change any other rule or normative definition. If something seems to need such a change, stop that item and put it in QUESTIONS.
- Commit each piece after the build passes. Do not push.
- Do not edit scripts/proofs/axioms.golden, docs/decisions.md, ROADMAP.md or ISSUES.tsv.
- Never commit _review/.
- Lean core only, no new axioms, no sorry.
- Keep tool output short.
- Write any inventory or log inside _review/, never under /tmp.
- Final message: for each item, done or left; new theorems with their statements in one line; commit SHAs; QUESTIONS.

## Build and commit gating (binding)
Run every Lean build as `bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh nix develop -c timeout 1200 lake build ConcreteSpec`. Gate every commit on the exit status of the check itself, with `set -o pipefail`. Do not run reviewers, and do not write git notes.
