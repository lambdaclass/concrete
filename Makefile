# Build directories
BUILD_DIR := build
C_BUILD_DIR := $(BUILD_DIR)/c_backend
LLVM_BUILD_DIR := $(BUILD_DIR)/llvm_backend
CONCRETE := ./target/release/concrete

.PHONY: usage
usage: check-deps
	@echo "Usage:"
	@echo "    build:          builds the project"
	@echo "    check:          runs 'cargo fmt' and clippy"
	@echo "    clean:          cleans up all build artifacts"
	@echo "    test:           runs all tests"
	@echo "    coverage:       runs llvm-cov to generate a test coverage report"
	@echo "    bench:          runs the benchmarks"
	@echo "    test-c-backend: compares C backend vs LLVM backend for all examples"
	@echo "    clean-c-backend: removes C backend build artifacts"

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
	rm -rf $(BUILD_DIR)

.PHONY: test
test: check-deps
	cargo test --all-targets --all-features && \
	echo "Testing example concrete project" && cd examples/project && cargo run -- build

.PHONY: coverage
coverage: check-deps
	cargo llvm-cov --verbose --all-features --all-targets --lcov --output-path lcov.info

.PHONY: bench
bench: check-deps
	./bench/bench.sh

.PHONY: book
book:
	cd ./docs/book && mdbook serve --open

# C Backend comparison testing
.PHONY: test-c-backend
test-c-backend: check-deps build
	@mkdir -p $(C_BUILD_DIR) $(LLVM_BUILD_DIR)
	@echo "=== C Backend vs LLVM Backend Comparison ==="
	@echo ""
	@MATCH=0; MISMATCH=0; SKIP=0; \
	for file in examples/*.con examples/c_backend_tests/*.con; do \
		name=$$(basename "$$file" .con); \
		\
		$(CONCRETE) build --backend c --output-c "$$file" 2>/dev/null; \
		if [ $$? -ne 0 ]; then \
			SKIP=$$((SKIP + 1)); \
			continue; \
		fi; \
		mv "$$name" $(C_BUILD_DIR)/ 2>/dev/null; \
		mv "$$name.c" $(C_BUILD_DIR)/ 2>/dev/null; \
		C_RESULT=$$($(C_BUILD_DIR)/$$name 2>/dev/null; echo $$?); \
		\
		$(CONCRETE) build "$$file" 2>/dev/null; \
		if [ $$? -ne 0 ]; then \
			SKIP=$$((SKIP + 1)); \
			continue; \
		fi; \
		mv "$$name" $(LLVM_BUILD_DIR)/ 2>/dev/null; \
		LLVM_RESULT=$$($(LLVM_BUILD_DIR)/$$name 2>/dev/null; echo $$?); \
		\
		if [ "$$C_RESULT" = "$$LLVM_RESULT" ]; then \
			echo "✓ $$name: $$C_RESULT"; \
			MATCH=$$((MATCH + 1)); \
		else \
			echo "✗ $$name: C=$$C_RESULT LLVM=$$LLVM_RESULT"; \
			MISMATCH=$$((MISMATCH + 1)); \
		fi; \
	done; \
	echo ""; \
	echo "=== Summary ==="; \
	echo "Match:    $$MATCH"; \
	echo "Mismatch: $$MISMATCH"; \
	echo "Skipped:  $$SKIP"; \
	echo ""; \
	echo "Generated C files: $(C_BUILD_DIR)/*.c"; \
	if [ $$MISMATCH -gt 0 ]; then exit 1; fi

.PHONY: clean-c-backend
clean-c-backend:
	rm -rf $(BUILD_DIR)
