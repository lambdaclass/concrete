.PHONY: build test test-full test-trust-gate test-proof-gate test-ssa test-fuzz test-oracle test-wrong-code test-reducer-smoke test-bundle-smoke test-verify-gates test-assumptions test-policy test-catches test-snapshots test-prove-cli test-evidence-corpus test-pv-oracle test-cv-oracle test-fc-oracle test-ct-oracle test-hmac-oracle test-release-bundle test-showcase test-registry-retirement test-proof-namespace test-proof-patterns test-contract-negatives test-contract-stability test-phase1-contracts test-vc-schema test-proofkit-arith clean check-grammar paper paper-ec papers docs-site docs-serve

NIX_DEVELOP = XDG_CACHE_HOME=$(CURDIR)/.cache nix --extra-experimental-features "nix-command flakes" develop --command

build:
	$(NIX_DEVELOP) lake build

test: build
	$(NIX_DEVELOP) sh ./scripts/tests/run_tests.sh

test-full: build
	$(NIX_DEVELOP) sh ./scripts/tests/run_tests.sh --full

test-trust-gate: build
	$(NIX_DEVELOP) sh ./scripts/tests/run_tests.sh --trust-gate

test-proof-gate: build
	$(NIX_DEVELOP) bash ./scripts/ci/proof_gate.sh

test-ssa: build
	$(NIX_DEVELOP) bash ./scripts/tests/test_ssa.sh

test-fuzz: build
	$(NIX_DEVELOP) bash ./scripts/tests/test_parser_fuzz.sh

test-oracle: build
	$(NIX_DEVELOP) bash ./scripts/tests/test_oracle.sh

test-wrong-code: build
	$(NIX_DEVELOP) bash ./scripts/tests/test_wrong_code.sh

test-reducer-smoke: build
	$(NIX_DEVELOP) bash ./scripts/tests/test_reducer_smoke.sh

test-bundle-smoke: build
	$(NIX_DEVELOP) bash ./scripts/tests/test_bundle_smoke.sh

test-verify-gates: build
	$(NIX_DEVELOP) bash ./scripts/tests/test_verify_gates.sh

test-assumptions: build
	$(NIX_DEVELOP) bash ./scripts/tests/check_assumptions.sh

test-policy: build
	$(NIX_DEVELOP) bash ./scripts/tests/check_policy.sh

test-catches: build
	$(NIX_DEVELOP) bash ./scripts/tests/check_catches.sh

test-snapshots: build
	$(NIX_DEVELOP) bash ./scripts/tests/check_snapshots.sh

test-prove-cli: build
	$(NIX_DEVELOP) bash ./scripts/tests/test_prove_cli.sh

test-evidence-corpus: build
	$(NIX_DEVELOP) bash ./scripts/tests/check_evidence_corpus.sh

test-pv-oracle: build
	$(NIX_DEVELOP) bash ./examples/parse_validate/oracle/run_oracle.sh 0
	$(NIX_DEVELOP) bash ./examples/parse_validate/oracle/run_oracle.sh 42
	$(NIX_DEVELOP) bash ./examples/parse_validate/oracle/run_oracle.sh 999

test-cv-oracle: build
	$(NIX_DEVELOP) bash ./examples/crypto_verify/oracle/run_oracle.sh 0
	$(NIX_DEVELOP) bash ./examples/crypto_verify/oracle/run_oracle.sh 42
	$(NIX_DEVELOP) bash ./examples/crypto_verify/oracle/run_oracle.sh 999

test-fc-oracle: build
	$(NIX_DEVELOP) bash ./examples/fixed_capacity/oracle/run_oracle.sh 0
	$(NIX_DEVELOP) bash ./examples/fixed_capacity/oracle/run_oracle.sh 42
	$(NIX_DEVELOP) bash ./examples/fixed_capacity/oracle/run_oracle.sh 999

test-ct-oracle: build
	$(NIX_DEVELOP) bash ./examples/constant_time_tag/oracle/run_oracle.sh 0
	$(NIX_DEVELOP) bash ./examples/constant_time_tag/oracle/run_oracle.sh 42
	$(NIX_DEVELOP) bash ./examples/constant_time_tag/oracle/run_oracle.sh 999

test-hmac-oracle: build
	$(NIX_DEVELOP) bash ./examples/hmac_sha256/oracle/run_oracle.sh 0
	$(NIX_DEVELOP) bash ./examples/hmac_sha256/oracle/run_oracle.sh 42
	$(NIX_DEVELOP) bash ./examples/hmac_sha256/oracle/run_oracle.sh 999

test-release-bundle: build
	$(NIX_DEVELOP) bash ./scripts/tests/test_release_bundle.sh

test-showcase: build
	$(NIX_DEVELOP) bash ./scripts/tests/check_showcase.sh

test-registry-retirement:
	bash ./scripts/tests/check_no_example_registries.sh

test-proof-namespace:
	bash ./scripts/tests/check_proof_namespace.sh

test-proof-patterns: build
	$(NIX_DEVELOP) bash ./scripts/tests/check_proof_patterns.sh

test-contract-negatives: build
	$(NIX_DEVELOP) bash ./scripts/tests/check_contract_negatives.sh

test-contract-stability: build
	$(NIX_DEVELOP) bash ./scripts/tests/check_contract_stability.sh

test-phase1-contracts: build
	$(NIX_DEVELOP) bash ./scripts/tests/check_phase1_contracts.sh

test-vc-schema: build
	$(NIX_DEVELOP) bash ./scripts/tests/check_vc_schema.sh

test-proofkit-arith: build
	$(NIX_DEVELOP) bash ./scripts/tests/check_proofkit_arith.sh

check-grammar:
	$(NIX_DEVELOP) python3 scripts/check_ll1.py grammar/concrete.ebnf

paper:
	$(NIX_DEVELOP) typst compile paper/main.typ paper/main.pdf

# Evidence-carrying workflow paper
paper-ec:
	$(NIX_DEVELOP) typst compile paper/evidence-carrying.typ paper/evidence-carrying.pdf

papers: paper paper-ec

docs-site:
	$(NIX_DEVELOP) zola --root site build

docs-serve:
	$(NIX_DEVELOP) zola --root site serve --interface 127.0.0.1 --port 18080

clean:
	$(NIX_DEVELOP) lake clean
