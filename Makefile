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
ifndef LLVM_SYS_191_PREFIX
	$(error Could not find a suitable LLVM 19 toolchain, please set LLVM_SYS_190_PREFIX env pointing to the LLVM 19 dir)
endif
ifndef MLIR_SYS_190_PREFIX
	$(error Could not find a suitable LLVM 19 toolchain (mlir), please set MLIR_SYS_190_PREFIX env pointing to the LLVM 19 dir)
endif
ifndef TABLEGEN_190_PREFIX
	$(error Could not find a suitable LLVM 19 toolchain (tablegen), please set TABLEGEN_190_PREFIX env pointing to the LLVM 19 dir)
endif
	@echo "[make] LLVM is correctly set at $(MLIR_SYS_190_PREFIX)."

.PHONY: build
build: check-deps
	cargo build --release --all-features

.PHONY: check
check: check-deps
	cargo fmt --all -- --check
	cargo clippy --all-targets --all-features -- -D warnings

.PHONY: clean
clean:
	cargo clean

.PHONY: test
test: check-deps
	cargo test --all-targets --all-features

.PHONY: coverage
coverage: check-deps
	cargo llvm-cov --verbose --all-features --all-targets --lcov --output-path lcov.info

.PHONY: bench
bench: check-deps
	./bench/bench.sh

.PHONY: book
book:
	cd ./docs/book && mdbook serve --open
