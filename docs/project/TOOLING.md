# Repository tooling

Paths are relative to `scripts/`.

## Proof soundness

- `proofs/axioms.sh` Checks theorem dependencies against the allowed axioms and recorded inventories.
- `proofs/external.sh` Rechecks Lean proof terms with independent implementations.
- `proofs/export_inventory.py` Checks that the export covers the declarations required for independent replay.
- `proofs/imports.lean` Reads actual import dependencies from Lean’s built modules.
- `proofs/imports.py` Checks compiled dependencies against the proof import policy.
- `proofs/kernel_replay.sh` Replays built declarations in a fresh Lean kernel to catch invalid proof terms.
- `proofs/lake_tool.sh` Builds pinned checker tools in isolation so their identities are reproducible.
- `proofs/lean_lex.py` Reads Lean tokens so comments and strings cannot fool source checks.
- `proofs/lib.sh` Shares setup and controls so proof gates use consistent inputs.
- `proofs/local_identity.lean` Scans Lean declarations for dependence on local variable names and for forbidden diagnostic dependencies.
- `proofs/local_identity.py` Runs the local-variable identity and diagnostic separation scan and tests that it detects planted violations.
- `proofs/test_lean_lex.py` Exercises token handling to protect the source checks.
- `proofs/theorems.lean` Reads theorem and axiom dependencies from the built environment.
- `proofs/trust_ban.sh` Rejects proof shortcuts such as `sorry` and `native_decide`.

## Specification

- `spec/f31_census.lean` Exports Lean declaration data for the census of the F31 legacy scalar representation migration.
- `spec/f31_census.py` Inventories the source files and Lean declarations affected by the F31 legacy scalar representation migration.
- `spec/f31_census_controls.py` Plants census errors to show that the inventory check catches them.
- `spec/landing_inventory.lean` Lists required spec declarations so missing obligations can be detected.
- `spec/landing_inventory.sh` Checks that inventory against the built specification.
- `spec/lean_site.lean` Reads built Lean declarations and documentation for the browsable spec.
- `spec/lean_site.py` Renders the spec pages and checks their links.
- `spec/lean_site.sh` Builds the browsable spec from checked Lean sources.
- `spec/own_control_ranges.lean` Exports ownership control locations so the fixtures can be traced.
- `spec/own_controls.py` Plants ownership mutations in disposable copies of Own.lean and checks that conformance observations reject them.
- `spec/rules.lean` Exports the rule registry from Lean so other tools see the same rules.
- `spec/rules.py` Validates the exported rule records against their schema.
- `spec/rules.sh` Runs the rule export, validation and controls together.
- `spec/rules_controls.py` Plants invalid rule records to test the validator.
- `spec/rules_validate.sh` Bounds registry validation so a stuck check fails visibly.
- `checks/spec_imports.sh` Enforces import boundaries so the spec stays independent of compiler code.

## Compiler

- `checks/compiler/build_identity.sh` Records source, toolchain and dependency identity and verifies Lake dependency checkouts against the manifest.
- `checks/compiler/f31_census.sh` Runs the F31 migration census controls on disposable inputs to verify that census defects are detected.
- `checks/compiler/llvm_mutation_controls.py` Kernel-checks Lean counterexamples against deliberately mutated LLVM backend definitions.
- `checks/compiler/llvm_rendering.lean` Exercises LLVM text rendering to catch changes in emitted code.
- `checks/compiler/slice0_artifacts.sh` Reports native artifact sizes and checks sanitizers and stack bounds.
- `checks/compiler/slice0_bir.lean` Validates manifest BIR and LCB expectations and emits LLVM IR, LCB assembly and expected observations for the compiler tests.
- `checks/compiler/slice0_premise_table.lean` Renders the premise table so documentation can be checked against Lean.
- `checks/compiler/slice0_repro.sh` Compares independent builds to catch nondeterministic outputs.
- `checks/compiler/slice0_trusted_base.lean` Prints the compiler premises and axioms for baseline comparison.
- `checks/compiler/slice0_trusted_base.sh` Checks the premise and axiom baseline so trust changes are visible.
- `checks/compiler/refusal_mutation_controls.py` Compares the working audit and supplied historical audits on each compiled pipeline mutation, and checks the unreachable-statement fixture against a changed specification span.
- `checks/compiler/tests.sh` Checks native LLVM and LCB execution against the test manifest.
- `checks/lib/repro_compare.py` Compares outputs by build stage to locate reproducibility failures.
- `checks/lib/slice0_common.sh` Shares Slice 0 setup and checks so compiler gates use consistent inputs.
- `checks/lib/slice0_native.sh` Builds and runs native artifacts with the intended tools and target.
- `checks/lib/slice0_sources.sh` Identifies the source inputs for repeatable Slice 0 checks.
- `checks/lib/stack_access.py` Evaluates stack addresses so measurements include accesses below the stack pointer.
- `checks/lib/stack_bound.py` Computes a stack bound from the machine instructions.
- `checks/lib/stack_measure.py` Measures stack use with GDB to check the Linux bound.
- `checks/lib/stack_measure_lldb.py` Measures stack use with LLDB to check the macOS bound.
- `checks/lib/test_manifest.py` Validates test records and coverage so fixtures cannot go unnoticed.
- `checks/test_manifest.sh` Checks that every fixture is covered by the test manifest.
- `oracle/build.sh` Rebuilds the pinned oracle and compares digests to check reproducibility.
- `oracle/ci_check.sh` Fetches and smoke-tests the published oracle so CI knows it runs.
- `oracle/fetch.sh` Verifies the oracle artifact before installing it for comparisons.
- `oracle/lib.sh` Shares lock and artifact handling so oracle commands verify the same inputs.
- `oracle/publish.sh` Publishes the verified oracle for consumers to fetch.
- `oracle/run.sh` Runs a program with the pinned oracle to provide a reference result.

## CI and repository checks

- `checks/archived_references.sh` Checks for retired repository references so tooling uses the live repository.
- `checks/doc_links.sh` Checks that Markdown files link to existing files and headings.
- `checks/external_toolchain.sh` Exercises the external checker’s toolchain isolation, cache handling and call-site scan after relative invocation.
- `checks/issues.sh` Checks the issue ledger’s structure and IDs so defects stay traceable.
- `checks/lean_threads.sh` Checks the Lean worker limit so builds respect their resource budget.
- `checks/legacy_frozen.sh` Checks the frozen tree and retention approvals to protect historical material.
- `checks/lib/archived_references.py` Identifies retired references and recognizes historical exceptions.
- `checks/lib/bounded.sh` Applies command time limits and reports repeated timeouts as incomplete.
- `checks/lib/bounded_exec.py` Controls process groups so timed-out commands stop with their children.
- `checks/lib/changelog.py` Folds changelog fragments so link checks can resolve their published headings.
- `checks/lib/doc_links.py` Resolves Markdown targets and headings to detect broken references.
- `checks/lib/gate_env.sh` Checks that gates have the expected environment and loads shared settings.
- `checks/lib/git_sandbox.sh` Clears inherited Git state so fixtures cannot change the caller’s repository.
- `checks/lib/has_match.sh` Checks for output matches while preserving command errors.
- `checks/lib/limited_exec.sh` Runs a command with resource limits so it stays within budget.
- `checks/lib/limits.sh` Shares worker limits so Lean jobs avoid oversubscribing the host.
- `checks/lib/retention.py` Checks retention rows and resolves which row governs each file.
- `checks/lib/shell_assignments.py` Parses Bash to catch declarations whose initializers can read stale values.
- `checks/lib/workflow_checks.py` Matches workflow job names to the gate inventory and requires an explicit success from each landing job.
- `checks/oracle_fetch.sh` Tests fetch failures so missing or corrupt oracle artifacts block CI.
- `checks/retention_manifest.sh` Checks that all files have a governing retention row.
- `checks/root_inventory.sh` Checks the permitted root layout so additions are deliberate.
- `checks/run.sh` Runs every required root check and reports all failures together.
- `checks/sandbox_isolation.sh` Tests that disposable Git repositories leave the caller’s repository intact.
- `checks/shell_assignments.sh` Runs the Bash declaration check with passing and failing controls.
- `checks/workflow_yaml.sh` Checks workflow syntax, the landing job inventory and the controls for the required verdict.
- `worktree-new.sh` Gives each writer a separate checkout to prevent overlapping edits.
- `../.githooks/pre-push` Scans WIP commits for conflict markers before backup pushes.
