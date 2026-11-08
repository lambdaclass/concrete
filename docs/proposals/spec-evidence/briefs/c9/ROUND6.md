# C9 round 6 (Codex writes; agy wrote round 5)

State: wip/spec-c9 at f35fb5d86 (round 5 7bca532d4 by agy plus the regenerated golden). All seven gates pass at f35fb5d86. Codex's full-access review of 4d11dabc2..7bca532d4 says CHANGES; it is in _review/REVIEW5-codex.md. Read it in full, together with _review/ROUND5.md, the earlier briefs and reviews in _review/, _review/COMMON.md if present and _review/VACUITY_CONVENTION_FINAL.md.

## Best long-term design (owner rule, binding)
Judge and fix everything by the best long-term design. Never cut corners for speed, a smaller diff or fewer rounds. Implement the structural fix, not a containment patch. Shortcuts are findings; a law that restates a definition or projects a premise is a shortcut.

## Tasks
1. Fix findings 1-5 of REVIEW5-codex.md (nested constructs in the function-table key assignment; definitional equations classified as Laws; planted witnesses that change the Instance's fixed arguments; `Ghost.drive_steps` as a Law; the location Law that repackages the validity premise), following each long-term fix or a better one (say why if you deviate).
2. Finding 6 (byte-column theorems at Proofs/Observe2.lean:483, :492): the C6 audit branch owns migrating Observe2.lean's C6 theorems; do not edit those two theorems here, and say so in your final message.
3. Act on every exemption verdict; keep `(approved: PENDING)` only on helpers that stay helpers, each with its own reason.
4. Check EVERY law of C9 against the final convention, not only the changed ones.
5. Owner decisions bind exactly: FunctionRelativeKey = structured path of (construct kind, name) segments, unnamed items get an ordinal among same-kind siblings under the nearest named ancestor with an optional pinning label, no raw child indices, insertion-stability law; PackageId = structured name and version, no hash, same name and version from different origins refused at load naming both; premise categories a closed enumeration, versioned schema, unknown categories refused, no open-string escape.

## Rules
Do not change any owner-decided rule; if a task seems to need one, stop it and ask in QUESTIONS. Commit each piece after the build passes (do not push; do not edit scripts/proofs/axioms.golden, docs/decisions.md or ROADMAP.md; never commit _review/ or scratch files). Lean core only, no new axioms, no sorry. Keep tool output short (pipe builds through tail). Final message: per finding, done or left (with why), commit SHAs, QUESTIONS.

## Build and commit gating (binding)
Run every Lean build through the machine-wide build semaphore: `bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh nix develop -c timeout 1200 lake build ConcreteSpec` (at most two heavy builds on this machine). Gate every commit on the exit status of the check itself with `set -o pipefail`; never write `check | tail && git commit`. Do not run reviewers; do not write git notes.
