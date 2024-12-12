#!/usr/bin/env bash

# This script is only useful on macOS using brew.
# It sets the LLVM environment variables.

export MLIR_SYS_190_PREFIX="$(brew --prefix llvm@19)"
export LLVM_SYS_191_PREFIX="$(brew --prefix llvm@19)"
export TABLEGEN_190_PREFIX="$(brew --prefix llvm@19)"
