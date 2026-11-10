#!/usr/bin/env bash
# The tracked inputs of a slice-0 build: what slice0_repro.sh copies and build_identity.sh digests.
# shellcheck disable=SC2034
slice0_sources=(Concrete Concrete.lean ConcreteSpec scripts/checks tests lakefile.toml lean-toolchain
  lake-manifest.json)
