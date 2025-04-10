#!/usr/bin/env bash

# This script is only useful on macOS using brew.
# It sets the LLVM environment variables.

export LIBRARY_PATH=/opt/homebrew/lib
export MLIR_SYS_200_PREFIX="$(brew --prefix llvm@20)"
export LLVM_SYS_201_PREFIX="$(brew --prefix llvm@20)"
export TABLEGEN_200_PREFIX="$(brew --prefix llvm@20)"
