.PHONY: build check clean test coverage

check-deps:
ifeq (, $(shell which cargo))
	$(error "The cargo command could not be found in your PATH, please install Rust: https://www.rust-lang.org/tools/install")
endif
ifndef LLVM_SYS_170_PREFIX
	$(error Could not find a suitable LLVM 17 toolchain, please set LLVM_SYS_170_PREFIX env pointing to the LLVM 17 dir)
endif
ifndef MLIR_SYS_170_PREFIX
	$(error Could not find a suitable LLVM 17 toolchain (mlir), please set MLIR_SYS_170_PREFIX env pointing to the LLVM 17 dir)
endif
ifndef TABLEGEN_170_PREFIX
	$(error Could not find a suitable LLVM 17 toolchain (tablegen), please set TABLEGEN_170_PREFIX env pointing to the LLVM 17 dir)
endif
	@echo "[make] LLVM is correctly set at $(MLIR_SYS_170_PREFIX)."

build: check-deps
	cargo build --workspace --release --all-features

check: check-deps
	cargo fmt --all -- --check
	cargo clippy --workspace --all-targets --all-features -- -D warnings

clean:
	cargo clean

test: check-deps
	cargo test --workspace --all-targets --all-features

coverage: check-deps
	cargo llvm-cov --verbose --all-features --workspace --lcov --output-path lcov.info
