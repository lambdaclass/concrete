# Homebrew

This directory holds a [Homebrew](https://brew.sh/) formula for building Concrete from source.

## Install from a tap

```bash
brew tap lambdaclass/concrete https://github.com/lambdaclass/concrete
brew install --HEAD concrete
```

To try a fork before changes land upstream, point the tap URL at your fork instead.

## Requirements

The formula installs the pinned Lean toolchain from `lean-toolchain` via elan, then runs `lake build concrete`. It depends on LLVM (for `clang`), `zstd` (macOS link path), and network access while elan downloads the toolchain.

## Verify

```bash
concrete --version
concrete examples/snippets/hello_world.con -o /tmp/hello && /tmp/hello
```
