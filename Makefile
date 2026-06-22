.PHONY: build test test-full test-trust-gate test-proof-gate test-axiom-inventory test-cap-poly-design test-callable-values test-returned-ref-provenance test-proven-violation test-mono-collision test-nested-field-write test-raw-ptr-to-local test-struct-layout test-codegen-execution test-codegen-differential test-ssa test-golden test-examples test-fuzz test-oracle test-wrong-code test-reducer-smoke test-bundle-smoke test-verify-gates test-assumptions test-policy test-catches test-snapshots test-prove-cli test-evidence-corpus test-pv-oracle test-cv-oracle test-fc-oracle test-ct-oracle test-hmac-oracle test-release-bundle test-showcase test-registry-retirement test-proof-namespace test-proof-patterns test-contract-negatives test-contract-stability test-phase1-contracts test-vc-schema test-proofkit-arith test-smt-path test-smt-policy test-smt-replay test-smt-negatives test-vc-discharge-examples test-smt-examples test-smt-redteam test-vc-examples test-fpf-oracle test-phase2-vc test-obligation-core test-scoped-collector test-call-site-migration test-bounds-migration test-div-migration test-overflow-migration test-assume-migration test-loop-migration test-contract-clause-migration test-proof-link-migration test-obligation-lowering test-discharge-adapters test-obligation-report-views test-contracts-ledger-parity test-docs-drift test-module-visibility test-project-model test-concrete-test test-diagnostics-quality test-byte-view test-collections test-concrete-fmt test-loop-control test-type-alias test-pattern-ergonomics test-no-duplicate-walkers test-single-truth-source test-obligation-policy-views test-obligation-prove-views test-phase3-obligation-core test-obligation-redteam test-compiler-ledger test-rich-diagnostics test-partial-facts test-source-maps test-cli-plumbing test-cli-contract test-api-boundary test-backend-contracts clean check-grammar paper paper-ec papers docs-site docs-serve help

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

test-trust-gate: build ## Trust gate: correctness-contract sections only
	$(NIX_DEVELOP) sh ./scripts/tests/run_tests.sh --trust-gate

test-ssa: build ## SSA backend test suite
	$(NIX_DEVELOP) bash ./scripts/tests/test_ssa.sh

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
