.PHONY: usage
usage: check-deps
	@echo "Usage:"
	@echo "    build:    builds the project"
	@echo "    check:    runs 'cargo fmt' and clippy"
	@echo "    clean:    cleans up all build artifacts"
	@echo "    test:     runs all tests"
	@echo "    coverage: runs llvm-cov to generate a test coverage report"
	@echo "    bench:    runs the benchmarks"

.PHONY: check-deps
check-deps:
ifeq (, $(shell which cargo))
	$(error "The cargo command could not be found in your PATH, please install Rust: https://www.rust-lang.org/tools/install")
endif
ifndef LLVM_SYS_190_PREFIX
	$(error Could not find a suitable LLVM 18 toolchain, please set LLVM_SYS_190_PREFIX env pointing to the LLVM 18 dir)
endif
ifndef MLIR_SYS_190_PREFIX
	$(error Could not find a suitable LLVM 18 toolchain (mlir), please set MLIR_SYS_190_PREFIX env pointing to the LLVM 18 dir)
endif
ifndef TABLEGEN_190_PREFIX
	$(error Could not find a suitable LLVM 18 toolchain (tablegen), please set TABLEGEN_190_PREFIX env pointing to the LLVM 18 dir)
endif
	@echo "[make] LLVM is correctly set at $(MLIR_SYS_190_PREFIX)."

.PHONY: build
build: check-deps
	cargo build --workspace --release --all-features

.PHONY: check
check: check-deps
	cargo fmt --all -- --check
	cargo clippy --workspace --all-targets --all-features -- -D warnings

.PHONY: clean
clean:
	cargo clean

.PHONY: test
test: check-deps
	cargo test --workspace --all-targets --all-features

.PHONY: coverage
coverage: check-deps
	cargo llvm-cov --verbose --all-features --all-targets --workspace --lcov --output-path lcov.info

.PHONY: bench
bench: check-deps
	./bench/bench.sh
