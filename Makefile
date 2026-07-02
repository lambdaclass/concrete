.PHONY: build test test-full test-ci-gates test-trust-gate test-proof-gate test-axiom-inventory test-cap-poly-design test-callable-values test-returned-ref-provenance test-proven-violation test-mono-collision test-nested-field-write test-raw-ptr-to-local test-struct-layout test-codegen-execution test-codegen-differential test-ssa test-golden test-examples test-fuzz test-oracle test-wrong-code test-reducer-smoke test-bundle-smoke test-verify-gates test-assumptions test-policy test-catches test-snapshots test-prove-cli test-evidence-corpus test-pv-oracle test-cv-oracle test-fc-oracle test-ct-oracle test-hmac-oracle test-release-bundle test-showcase test-registry-retirement test-proof-namespace test-proof-patterns test-contract-negatives test-contract-stability test-phase1-contracts test-vc-schema test-proofkit-arith test-smt-path test-smt-policy test-smt-replay test-smt-negatives test-vc-discharge-examples test-smt-examples test-smt-redteam test-vc-examples test-fpf-oracle test-phase2-vc test-obligation-core test-scoped-collector test-call-site-migration test-bounds-migration test-div-migration test-overflow-migration test-assume-migration test-loop-migration test-contract-clause-migration test-proof-link-migration test-obligation-lowering test-discharge-adapters test-obligation-report-views test-contracts-ledger-parity test-docs-drift test-workflow-yaml test-module-visibility test-project-model test-concrete-test test-diagnostics-quality test-byte-view test-collections test-concrete-fmt test-loop-control test-type-alias test-pattern-ergonomics test-struct-update test-no-tuples test-nested-patterns test-numeric-literals test-lex-escapes test-mixed-width-binops test-trailing-value-blocks test-submodule-check test-defer test-ignored-result test-array-bounds test-linear-discard test-linear-nested-scope test-linear-conservation test-fuzz-differential test-no-macros test-phase6-redteam test-stdlib-handoff test-build-profiles test-iteration-protocol test-wrapping-arith test-saturating-arith test-checked-arith test-arith-redteam test-report-arithmetic test-memory-model test-float-cast test-operational-vc-autodischarge test-no-duplicate-walkers test-single-truth-source test-obligation-policy-views test-obligation-prove-views test-phase3-obligation-core test-obligation-redteam test-compiler-ledger test-rich-diagnostics test-partial-facts test-source-maps test-cli-plumbing test-cli-contract test-api-boundary test-backend-contracts clean check-grammar paper paper-ec papers docs-site docs-serve help

NIX_DEVELOP = XDG_CACHE_HOME=$(CURDIR)/.cache nix --extra-experimental-features "nix-command flakes" develop --command

##@ Build

build: ## Build the compiler (lake build inside nix develop)
	$(NIX_DEVELOP) lake build

clean: ## Remove lake build artifacts
	$(NIX_DEVELOP) lake clean

##@ Core tests

test: build ## Main test suite (run_tests.sh)
	$(NIX_DEVELOP) sh ./scripts/tests/run_tests.sh

test-full: build ## Main test suite, full mode (run_tests.sh --full)
	$(NIX_DEVELOP) sh ./scripts/tests/run_tests.sh --full

test-ci-gates: build ## Run every gate the CI workflow runs, locally (extracted from the workflow file; pre-push ritual, ROADMAP #34a)
	$(NIX_DEVELOP) bash ./scripts/tests/run_ci_gates_local.sh

test-trust-gate: build ## Trust gate: correctness-contract sections only
	$(NIX_DEVELOP) sh ./scripts/tests/run_tests.sh --trust-gate

test-ssa: build ## SSA backend test suite
	$(NIX_DEVELOP) bash ./scripts/tests/test_ssa.sh

test-linear-discard: build ## Linear-discard gate (KNOWN_HOLES H6): silent discard of a non-Copy value is rejected; `let _ =`/free/consumed compile
	$(NIX_DEVELOP) bash ./scripts/tests/check_linear_discard.sh

test-linear-nested-scope: build ## Nested-scope linearity gate (KNOWN_HOLES H9): a linear value in an if/else branch or match arm must be consumed; move-through-let; while-true/abort exempt
	$(NIX_DEVELOP) bash ./scripts/tests/check_linear_nested_scope.sh

test-linear-conservation: build ## Linear-conservation gate: every value-flow site (let/array-lit/struct-lit/destructure/arg/return/match) MOVES a linear value exactly once — no duplication
	$(NIX_DEVELOP) bash ./scripts/tests/check_linear_conservation.sh

test-array-bounds: build ## Array-bounds gate (KNOWN_HOLES H8): raw a[i] is runtime-checked — OOB read/write/negative/nested/&mut trap
	$(NIX_DEVELOP) bash ./scripts/tests/check_array_bounds.sh

test-fuzz-differential: build ## Random interp-vs-compiled differential fuzzer smoke (Phase 14 #13). Loops re-enabled after H7 fix.
	$(NIX_DEVELOP) python3 ./scripts/tests/fuzz_differential.py --n 200 --seed 1 --depth 3
	$(NIX_DEVELOP) python3 ./scripts/tests/fuzz_differential.py --n 200 --seed 2 --depth 3

test-golden: build ## Golden baselines for --emit-core, --emit-ssa, --fmt
	$(NIX_DEVELOP) bash ./scripts/tests/test_golden.sh

test-examples: build ## Example coverage manifest gate (every examples/ .con is checked)
	$(NIX_DEVELOP) bash ./scripts/tests/check_examples.sh

test-snapshots: build ## Diagnostic snapshot tests
	$(NIX_DEVELOP) bash ./scripts/tests/check_snapshots.sh

test-catches: build ## Negative "the compiler catches this" examples
	$(NIX_DEVELOP) bash ./scripts/tests/check_catches.sh

test-verify-gates: build ## Verification gate plumbing tests
	$(NIX_DEVELOP) bash ./scripts/tests/test_verify_gates.sh

test-assumptions: build ## Assumption-tracking checks
	$(NIX_DEVELOP) bash ./scripts/tests/check_assumptions.sh

test-policy: build ## Policy enforcement checks
	$(NIX_DEVELOP) bash ./scripts/tests/check_policy.sh

test-prove-cli: build ## `concrete prove` CLI gate
	$(NIX_DEVELOP) bash ./scripts/tests/test_prove_cli.sh

test-rich-diagnostics: build ## Rich-diagnostics gate (human/JSON from one record)
	$(NIX_DEVELOP) bash ./scripts/tests/check_rich_diagnostics.sh

test-partial-facts: build ## Partial-facts gate (error-tolerant diagnostics)
	$(NIX_DEVELOP) bash ./scripts/tests/check_partial_facts.sh

test-source-maps: build ## Source-maps gate (spans survive AST -> Core)
	$(NIX_DEVELOP) bash ./scripts/tests/check_source_maps.sh

test-cli-plumbing: build ## CLI-plumbing gate (shared prologue + exit codes)
	$(NIX_DEVELOP) bash ./scripts/tests/check_cli_plumbing.sh

test-cli-contract: build ## CLI-contract gate (golden command behavior matrix)
	$(NIX_DEVELOP) bash ./scripts/tests/check_cli_contract.sh

test-api-boundary: build ## API-boundary gate (consumers use the boundary)
	$(NIX_DEVELOP) bash ./scripts/tests/check_compiler_api_boundary.sh

test-backend-contracts: build ## Backend-contract gate (codegen guarantees, no drift)
	$(NIX_DEVELOP) bash ./scripts/tests/check_backend_contracts.sh

##@ Proof / obligation gates

test-proof-gate: build ## Proof evidence gate (extraction, registry, Lean kernel check)
	$(NIX_DEVELOP) bash ./scripts/ci/proof_gate.sh

test-axiom-inventory: build ## Axiom-inventory gate (#print axioms over every proof_by theorem; docs/AXIOMS.md)
	$(NIX_DEVELOP) bash ./scripts/tests/check_axiom_inventory.sh

test-cap-poly-design: build ## Callable-values/capability design gate (fn-ptr smuggling stays closed; stdlib HOF freeze)
	$(NIX_DEVELOP) bash ./scripts/tests/check_capability_polymorphism_design.sh

test-callable-values: build ## Callable-values gate (three context modes; &mut *ctx reborrow aliases, no copy)
	$(NIX_DEVELOP) bash ./scripts/tests/check_callable_values.sh

test-proven-violation: build ## Proven-violation known-hole gate (constant OOB / div-zero compile today)
	$(NIX_DEVELOP) bash ./scripts/tests/check_proven_violation_enforcement.sh

test-float-cast: build ## Checked float->int cast gate: NaN/inf/out-of-range abort, in-range truncates (Phase 6 #10, closed H2)
	$(NIX_DEVELOP) bash ./scripts/tests/check_float_cast.sh

test-mono-collision: build ## Mono name-collision known-hole gate (nested-generic specializations merge)
	$(NIX_DEVELOP) bash ./scripts/tests/check_mono_name_collision.sh

test-nested-field-write: build ## Nested place-write regression gate (o.inner.v / a[i].x / m[i][j])
	$(NIX_DEVELOP) bash ./scripts/tests/check_nested_field_write.sh

test-raw-ptr-to-local: build ## Raw-ptr-to-local known-hole gate (&mut x does not alias local)
	$(NIX_DEVELOP) bash ./scripts/tests/check_raw_ptr_to_local.sh

test-struct-layout: build ## Struct mixed-width field-layout regression gate
	$(NIX_DEVELOP) bash ./scripts/tests/check_struct_field_layout.sh

test-codegen-execution: build ## Codegen execution-oracle gate (compile-run-assert value, 30 cases)
	$(NIX_DEVELOP) bash ./scripts/tests/check_codegen_execution.sh

test-codegen-differential: build ## Interp-vs-compiled differential gate (oracle agreement)
	$(NIX_DEVELOP) bash ./scripts/tests/check_codegen_differential.sh

test-returned-ref-provenance: build ## Known hole: returned-ref provenance + aggregate-ref public API freeze
	$(NIX_DEVELOP) bash ./scripts/tests/check_returned_ref_provenance.sh

test-evidence-corpus: build ## Evidence-class corpus integrity gate
	$(NIX_DEVELOP) bash ./scripts/tests/check_evidence_corpus.sh

test-showcase: build ## Showcase gate (graduated flagships stay graduated)
	$(NIX_DEVELOP) bash ./scripts/tests/check_showcase.sh

test-registry-retirement: ## Registry-retirement gate (no JSON example registries)
	bash ./scripts/tests/check_no_example_registries.sh

test-proof-namespace: ## Proof-namespace guard (Concrete.Examples.* only)
	bash ./scripts/tests/check_proof_namespace.sh

test-proof-patterns: build ## Proof-patterns corpus gate
	$(NIX_DEVELOP) bash ./scripts/tests/check_proof_patterns.sh

test-contract-negatives: build ## Contract-negatives gate (source-contract hardening)
	$(NIX_DEVELOP) bash ./scripts/tests/check_contract_negatives.sh

test-contract-stability: build ## Contract-stability gate (contract API drift)
	$(NIX_DEVELOP) bash ./scripts/tests/check_contract_stability.sh

test-phase1-contracts: build ## Phase 1 contracts validation artifact (umbrella)
	$(NIX_DEVELOP) bash ./scripts/tests/check_phase1_contracts.sh

test-vc-schema: build ## VC schema v1 gate
	$(NIX_DEVELOP) bash ./scripts/tests/check_vc_schema.sh

test-proofkit-arith: build ## ProofKit arithmetic-bridge gate
	$(NIX_DEVELOP) bash ./scripts/tests/check_proofkit_arith.sh

test-smt-path: build ## External-SMT path gate (trust boundary)
	$(NIX_DEVELOP) bash ./scripts/tests/check_smt_path.sh

test-smt-policy: build ## SMT release-policy gate (evidence governance)
	$(NIX_DEVELOP) bash ./scripts/tests/check_smt_policy.sh

test-smt-replay: build ## Lean-replay gate (SMT -> kernel upgrade artifact)
	$(NIX_DEVELOP) bash ./scripts/tests/check_smt_replay.sh

test-smt-negatives: build ## SMT negatives gate (honesty boundaries)
	$(NIX_DEVELOP) bash ./scripts/tests/check_smt_negatives.sh

test-vc-discharge-examples: build ## VC/discharge example matrix gate
	$(NIX_DEVELOP) bash ./scripts/tests/check_vc_discharge_examples.sh

test-smt-examples: build ## External-SMT teaching group gate
	$(NIX_DEVELOP) bash ./scripts/tests/check_smt_examples.sh

test-smt-redteam: build ## Red-team VC/SMT gate (adversarial)
	$(NIX_DEVELOP) bash ./scripts/tests/check_smt_redteam.sh

test-vc-examples: build ## End-of-Phase-2 VC/SMT examples gate
	$(NIX_DEVELOP) bash ./scripts/tests/check_vc_examples.sh

test-phase2-vc: build ## Phase 2 VC/discharge validation artifact (umbrella)
	$(NIX_DEVELOP) bash ./scripts/tests/check_phase2_vc.sh

test-obligation-core: build ## ObligationCore ledger gate (schema + vocabulary)
	$(NIX_DEVELOP) bash ./scripts/tests/check_obligation_core.sh

test-scoped-collector: build ## Unified scoped-collector gate (adversarial)
	$(NIX_DEVELOP) bash ./scripts/tests/check_scoped_collector.sh

test-call-site-migration: build ## Call-site precondition migration gate
	$(NIX_DEVELOP) bash ./scripts/tests/check_call_site_migration.sh

test-bounds-migration: build ## Array-bounds migration gate
	$(NIX_DEVELOP) bash ./scripts/tests/check_bounds_migration.sh

test-div-migration: build ## Div/mod + sound-lowering migration gate
	$(NIX_DEVELOP) bash ./scripts/tests/check_div_migration.sh

test-overflow-migration: build ## Overflow migration gate (three-route parity)
	$(NIX_DEVELOP) bash ./scripts/tests/check_overflow_migration.sh

test-assume-migration: build ## Assert/assume migration gate (no-laundering)
	$(NIX_DEVELOP) bash ./scripts/tests/check_assume_migration.sh

test-loop-migration: build ## Loop-obligation migration gate (O1-O5 scoped)
	$(NIX_DEVELOP) bash ./scripts/tests/check_loop_migration.sh

test-contract-clause-migration: build ## Contract-clause migration gate
	$(NIX_DEVELOP) bash ./scripts/tests/check_contract_clause_migration.sh

test-proof-link-migration: build ## Proof-link freshness migration gate
	$(NIX_DEVELOP) bash ./scripts/tests/check_proof_link_migration.sh

test-obligation-lowering: build ## Obligation-expression lowering gate
	$(NIX_DEVELOP) bash ./scripts/tests/check_obligation_lowering.sh

test-discharge-adapters: build ## Backend discharge-adapter gate (evidence firewall)
	$(NIX_DEVELOP) bash ./scripts/tests/check_discharge_adapters.sh

test-obligation-report-views: build ## Reports-as-views gate (ledger consistency)
	$(NIX_DEVELOP) bash ./scripts/tests/check_obligation_report_views.sh

test-contracts-ledger-parity: build ## --report contracts ↔ ledger parity (#15/#18e)
	$(NIX_DEVELOP) bash ./scripts/tests/check_contracts_ledger_parity.sh

test-docs-drift: build ## Docs-drift gate: present-tense docs reference only real artifacts (#44)
	$(NIX_DEVELOP) bash ./scripts/tests/check_docs_drift.sh

test-workflow-yaml: ## Workflow-YAML gate: every .github/workflows/*.yml parses (no build needed)
	bash ./scripts/tests/check_workflow_yaml.sh

test-module-visibility: build ## Module/import/visibility gate (Phase 5 #1)
	$(NIX_DEVELOP) bash ./scripts/tests/check_module_visibility.sh

test-project-model: build ## Project-model gate: Concrete.toml structure + anti-ambient-config (Phase 5 #2)
	$(NIX_DEVELOP) bash ./scripts/tests/check_project_model.sh

test-concrete-test: build ## `concrete test` gate: project-mode test runner (Phase 5 #3)
	$(NIX_DEVELOP) bash ./scripts/tests/check_concrete_test.sh

test-diagnostics-quality: build ## Diagnostics-quality gate: span floor + reason/next-action exemplar (Phase 5 #4)
	$(NIX_DEVELOP) bash ./scripts/tests/check_diagnostics_quality.sh

test-byte-view: build ## ByteView gate: owned, reference-free, stored zero-copy view + guards (Phase 5 #5a)
	$(NIX_DEVELOP) bash ./scripts/tests/check_byte_view.sh

test-collections: ## Collections gate: H1-clean surface (no returned refs) + value/op/scoped APIs (Phase 5 #6)
	$(NIX_DEVELOP) bash ./scripts/tests/check_collections.sh

test-concrete-fmt: build ## `concrete fmt` gate: subcommand contract + semantics-preserving formatting (Phase 6 #1)
	$(NIX_DEVELOP) bash ./scripts/tests/check_concrete_fmt.sh

test-loop-control: build ## Loop-control gate: break/continue/labeled/while-expr value + linear cleanup (Phase 6 #4)
	$(NIX_DEVELOP) bash ./scripts/tests/check_loop_control.sh

test-type-alias: build ## Type-alias gate: transparency + no-new-identity + bad-form rejection (Phase 6 #3)
	$(NIX_DEVELOP) bash ./scripts/tests/check_type_alias.sh

test-pattern-ergonomics: build ## Pattern-ergonomics gate: range patterns (+ future guards/OR/...) (Phase 6 #5)
	$(NIX_DEVELOP) bash ./scripts/tests/check_pattern_ergonomics.sh

test-struct-update: build ## Struct functional-update gate: S { f: x, ..base } (Phase 6 #5)
	$(NIX_DEVELOP) bash ./scripts/tests/check_struct_update.sh

test-no-tuples: build ## No-tuples decision gate: tuple syntax stays rejected, use named structs (Phase 6 #5)
	$(NIX_DEVELOP) bash ./scripts/tests/check_no_tuples.sh

test-nested-patterns: build ## Nested-patterns decision gate: one level/arm; deeper stays rejected + workarounds (Phase 6 #5)
	$(NIX_DEVELOP) bash ./scripts/tests/check_nested_patterns.sh

test-numeric-literals: build ## Numeric-literals gate: out-of-range literals rejected (E0227), in-range compile (Phase 6 #6)
	$(NIX_DEVELOP) bash ./scripts/tests/check_numeric_literals.sh

test-lex-escapes: build ## Escape gate: unknown string/char escapes and unterminated literals are E0001, never silently mangled
	$(NIX_DEVELOP) bash ./scripts/tests/check_lex_escapes.sh

test-mixed-width-binops: build ## Mixed-width gate: numeric binop operands must agree exactly (E0228); literals stay flexible; interp normalizes as-casts
	$(NIX_DEVELOP) bash ./scripts/tests/check_mixed_width_binops.sh

test-trailing-value-blocks: build ## Trailing-value gate: if/match as value-block trailing values (interp==compiled); all-statement forms stay statements
	$(NIX_DEVELOP) bash ./scripts/tests/check_trailing_value_blocks.sh

test-submodule-check: build ## Submodule-checking gate (KNOWN_HOLES H12): user sub-file bodies get the full front-end; std exemption stays disclosed
	$(NIX_DEVELOP) bash ./scripts/tests/check_submodule_check_coverage.sh

test-ignored-result: build ## Ignored-result gate: discarded Result/Option rejected (E0286) unless `let _ =` acknowledged (Phase 6 #13)
	$(NIX_DEVELOP) bash ./scripts/tests/check_ignored_result.sh

test-defer: build ## defer gate: LIFO + runs-on-every-exit-path; block-form rejected (Phase 6 #7)
	$(NIX_DEVELOP) bash ./scripts/tests/check_defer.sh

test-no-macros: build ## Macro-stance gate: no v1 macros; macro/derive syntax stays rejected (Phase 6 #11)
	$(NIX_DEVELOP) bash ./scripts/tests/check_no_macros.sh

test-phase6-redteam: build ## Red-team gate: parser recovery, match consumption/lowering, fingerprint stability, stale fixtures (Phase 6 #35a)
	$(NIX_DEVELOP) bash ./scripts/tests/check_phase6_redteam.sh

test-stdlib-handoff: ## Stdlib handoff gate: required Phase 5/6 surfaces stable/provisional, none blocked (Phase 6 #19)
	bash ./scripts/tests/check_stdlib_handoff.sh

test-build-profiles: build ## Build-profiles gate: --profile/[profile]/default/--report profile mechanism, no codegen (Phase 6 #10 Stage 1)
	$(NIX_DEVELOP) bash ./scripts/tests/check_build_profiles.sh

test-iteration-protocol: build ## Iteration-protocol gate: for/cursor/for_each/fold/map forms + no Iterator-trait/dyn/closure (Phase 6 #17)
	$(NIX_DEVELOP) bash ./scripts/tests/check_iteration_protocol.sh

test-wrapping-arith: build ## Wrapping-arith gate: wrapping_add/sub/mul wrap correctly (interp==compiled), integer-only (Phase 6 #10 Stage 2.1)
	$(NIX_DEVELOP) bash ./scripts/tests/check_wrapping_arith.sh

test-saturating-arith: build ## Saturating-arith gate: saturating_add/sub clamp at bounds (interp==compiled), integer-only (Phase 6 #10 Stage 2.2)
	$(NIX_DEVELOP) bash ./scripts/tests/check_saturating_arith.sh

test-checked-arith: build ## Checked-arith gate: ordinary + traps on overflow; wrapping_* escapes (Phase 6 #10 Stage 2.3)
	$(NIX_DEVELOP) bash ./scripts/tests/check_checked_arith.sh

test-arith-redteam: build ## Arithmetic red-team gate: overflow/neg/div-mod/shift traps + interp==compiled value agreement (Phase 6 #10)
	$(NIX_DEVELOP) bash ./scripts/tests/check_arith_redteam.sh

test-report-arithmetic: build ## --report arithmetic gate: per-site classification (runtime-checked/proved/wrapping/saturating) (Phase 6 #10 §3.2)
	$(NIX_DEVELOP) bash ./scripts/tests/check_report_arithmetic.sh

test-memory-model: build ## Memory-model gate: no uninitialized reads by construction (Phase 6 #33)
	$(NIX_DEVELOP) bash ./scripts/tests/check_memory_model.sh

test-operational-vc-autodischarge: build ## Operational-VC-auto-discharge forcing-probe gate (Phase 9 #16a)
	$(NIX_DEVELOP) bash ./scripts/tests/check_operational_vc_auto_discharge.sh

test-no-duplicate-walkers: ## No-duplicate-obligation-walkers guard
	bash ./scripts/tests/check_no_duplicate_obligation_walkers.sh

test-single-truth-source: ## Single-truth-source guard (ObligationCore)
	bash ./scripts/tests/check_obligation_single_truth_source.sh

test-obligation-policy-views: build ## Policy-as-ledger-view gate
	$(NIX_DEVELOP) bash ./scripts/tests/check_obligation_policy_views.sh

test-obligation-prove-views: build ## prove-as-ledger-view gate
	$(NIX_DEVELOP) bash ./scripts/tests/check_obligation_prove_views.sh

test-phase3-obligation-core: build ## Phase 3 capstone umbrella (single truth source)
	$(NIX_DEVELOP) bash ./scripts/tests/check_phase3_obligation_core.sh

test-obligation-redteam: build ## ObligationCore red-team gate (hardening)
	$(NIX_DEVELOP) bash ./scripts/tests/check_obligation_redteam.sh

test-compiler-ledger: build ## CompilerLedger gate (non-proof fact store)
	$(NIX_DEVELOP) bash ./scripts/tests/check_compiler_ledger.sh

##@ Fuzz / stress / oracles

test-fuzz: build ## Parser fuzzing
	$(NIX_DEVELOP) bash ./scripts/tests/test_parser_fuzz.sh

test-oracle: build ## Differential oracle tests
	$(NIX_DEVELOP) bash ./scripts/tests/test_oracle.sh

test-wrong-code: build ## Wrong-code detector tests
	$(NIX_DEVELOP) bash ./scripts/tests/test_wrong_code.sh

test-reducer-smoke: build ## Test-case reducer smoke test
	$(NIX_DEVELOP) bash ./scripts/tests/test_reducer_smoke.sh

test-pv-oracle: build ## parse_validate randomized oracle (seeds 0, 42, 999)
	$(NIX_DEVELOP) bash ./examples/parse_validate/oracle/run_oracle.sh 0
	$(NIX_DEVELOP) bash ./examples/parse_validate/oracle/run_oracle.sh 42
	$(NIX_DEVELOP) bash ./examples/parse_validate/oracle/run_oracle.sh 999

test-cv-oracle: build ## crypto_verify randomized oracle (seeds 0, 42, 999)
	$(NIX_DEVELOP) bash ./examples/crypto_verify/oracle/run_oracle.sh 0
	$(NIX_DEVELOP) bash ./examples/crypto_verify/oracle/run_oracle.sh 42
	$(NIX_DEVELOP) bash ./examples/crypto_verify/oracle/run_oracle.sh 999

test-fc-oracle: build ## fixed_capacity randomized oracle (seeds 0, 42, 999)
	$(NIX_DEVELOP) bash ./examples/fixed_capacity/oracle/run_oracle.sh 0
	$(NIX_DEVELOP) bash ./examples/fixed_capacity/oracle/run_oracle.sh 42
	$(NIX_DEVELOP) bash ./examples/fixed_capacity/oracle/run_oracle.sh 999

test-ct-oracle: build ## constant_time_tag randomized oracle (seeds 0, 42, 999)
	$(NIX_DEVELOP) bash ./examples/constant_time_tag/oracle/run_oracle.sh 0
	$(NIX_DEVELOP) bash ./examples/constant_time_tag/oracle/run_oracle.sh 42
	$(NIX_DEVELOP) bash ./examples/constant_time_tag/oracle/run_oracle.sh 999

test-hmac-oracle: build ## hmac_sha256 randomized oracle (seeds 0, 42, 999)
	$(NIX_DEVELOP) bash ./examples/hmac_sha256/oracle/run_oracle.sh 0
	$(NIX_DEVELOP) bash ./examples/hmac_sha256/oracle/run_oracle.sh 42
	$(NIX_DEVELOP) bash ./examples/hmac_sha256/oracle/run_oracle.sh 999

test-fpf-oracle: build ## fixed_point_filter randomized oracle (seeds 0, 7)
	$(NIX_DEVELOP) bash ./examples/vc_suite/fixed_point_filter_oracle/run_oracle.sh 0
	$(NIX_DEVELOP) bash ./examples/vc_suite/fixed_point_filter_oracle/run_oracle.sh 7

##@ Reports / artifacts

test-bundle-smoke: build ## Evidence-bundle smoke test
	$(NIX_DEVELOP) bash ./scripts/tests/test_bundle_smoke.sh

test-release-bundle: build ## Release bundle capture test
	$(NIX_DEVELOP) bash ./scripts/tests/test_release_bundle.sh

paper: ## Build the main paper PDF (typst)
	$(NIX_DEVELOP) typst compile paper/main.typ paper/main.pdf

# Evidence-carrying workflow paper
paper-ec: ## Build the evidence-carrying workflow paper PDF (typst)
	$(NIX_DEVELOP) typst compile paper/evidence-carrying.typ paper/evidence-carrying.pdf

papers: paper paper-ec ## Build all paper PDFs

docs-site: ## Build the documentation site (zola)
	$(NIX_DEVELOP) zola --root site build

docs-serve: ## Serve the documentation site locally on port 18080
	$(NIX_DEVELOP) zola --root site serve --interface 127.0.0.1 --port 18080

##@ Misc

check-grammar: ## Verify the grammar is LL(1)
	$(NIX_DEVELOP) python3 scripts/check_ll1.py grammar/concrete.ebnf

help: ## Show this help (targets grouped by category)
	@awk 'BEGIN {FS = ":.*##"} \
		/^##@/ { printf "\n\033[1m%s\033[0m\n", substr($$0, 5) } \
		/^[a-zA-Z0-9_-]+:.*##/ { printf "  \033[36m%-32s\033[0m %s\n", $$1, $$2 }' $(MAKEFILE_LIST)
