# Installation

Concrete is currently distributed via source. The normal path today is:

1. clone the repository
2. install the compiler/build dependencies
3. build the Lean compiler
4. run the produced `concrete` binary from `.lake/build/bin/concrete`

## Required Dependencies

For ordinary source builds, you currently need:

- `git`
- [Lean 4](https://leanprover.github.io/lean4/doc/setup.html) and `lake`
- `clang`
- `make` if you want the convenience targets from the repository root

The CI uses Lean plus `clang` directly, so those are the important requirements to match.

## Optional Dependencies

- `nix`, if you want to use the repository's `make build` wrapper exactly as maintained
- `mdbook`, if you want to build this documentation book locally

## Build From Source

Repository build:

```bash
git clone https://github.com/unbalancedparentheses/concrete2.git
cd concrete2
make build
```

Direct Lean build:

```bash
lake build
```

The compiler binary is then available at:

```bash
.lake/build/bin/concrete
```

## Verify The Build

Compile and run a small example:

```bash
.lake/build/bin/concrete examples/snippets/hello_world.con -o /tmp/hello
/tmp/hello
```

Run the main fast test path:

```bash
./run_tests.sh
```

Run the full suite before merge-level changes:

```bash
./run_tests.sh --full
```

## Notes

- Concrete is still changing quickly; building from source is the expected path.
- If you are debugging the compiler itself, prefer running `.lake/build/bin/concrete` directly instead of copying it into a global location.
- If you want a public-facing project overview before installation details, see [Why Concrete](./landing.md).
