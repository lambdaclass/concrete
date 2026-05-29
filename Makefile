.PHONY: build test test-full test-trust-gate test-proof-gate test-ssa test-fuzz test-oracle test-wrong-code test-reducer-smoke test-bundle-smoke test-verify-gates test-assumptions test-policy test-catches test-snapshots test-pv-oracle test-cv-oracle test-fc-oracle test-ct-oracle test-release-bundle test-showcase clean check-grammar paper docs-site docs-serve

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

test-release-bundle: build
	$(NIX_DEVELOP) bash ./scripts/tests/test_release_bundle.sh

test-showcase: build
	$(NIX_DEVELOP) bash ./scripts/tests/check_showcase.sh

check-grammar:
	$(NIX_DEVELOP) python3 scripts/check_ll1.py grammar/concrete.ebnf

paper:
	$(NIX_DEVELOP) typst compile paper/main.typ paper/main.pdf

docs-site:
	$(NIX_DEVELOP) zola --root site build

docs-serve:
	$(NIX_DEVELOP) zola --root site serve --interface 127.0.0.1 --port 18080

clean:
	$(NIX_DEVELOP) lake clean
