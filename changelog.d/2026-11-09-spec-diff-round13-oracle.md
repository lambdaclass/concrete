Gate A round 13, P2: spec-oracle plants use the candidate lifecycle and replay the wrong oracle.

Structural cause: the plant checked a wrong expectation through direct judge calls, while
replay copied the correct expectation. Structural owner: the shared spec_diff candidate
lifecycle and saved-command verifier (Concrete/README.md, differential gates).

Reproduction at 477b0048:
`SPEC_DIFF_REPLAY=spec:control-spec-oracle:0 SPEC_DIFF_REPLAY_CONTROL=spec-oracle nix develop --offline --command bash scripts/checks/compiler/spec_diff.sh`
returned 0; evaluator, LLVM and LCB each logged status 42. No mismatch was recorded.

The control now proves the correct oracle, installs the wrong expectation and runs native_one
again, requiring every exact per-backend mismatch diagnostic, durable expected/observed
artifacts and a standalone saved-command replay. Replay installs expectation 43, while all
three actual observations remain 42. Cheap orchestration controls cover the production control
and replay entry points; the native control executes the unchanged saved replay command.

Audit: other candidate plants already use native_one, driver failure records, or the shared
synthetic report recorder, with standalone saved-command verification. Determinism and the
synthetic partial-lowering summary are contract checks, not candidate mismatch plants.
Their candidate-generation observations still participate in exit collection. No language
rule, issue ID or roadmap row was allocated.
