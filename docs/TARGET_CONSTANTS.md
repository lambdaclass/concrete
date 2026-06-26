# Compiler-Known Target Constants — Decision

Status: decided, implementation deferred — ROADMAP Phase 6 #27.

Programs sometimes need to know the target (OS, arch, endianness, profile).
Inspired by Odin's builtin target constants, but Concrete's constraint is that
these must be **audit facts, not preprocessor magic**. This records the decision;
the source-level surface is **not** built yet.

## Decision

Target/build identity will eventually be exposed as **typed, resolved facts** —
names like `CONCRETE_OS`, `CONCRETE_ARCH`, `CONCRETE_ENDIAN`,
`CONCRETE_TARGET_PROFILE`, `CONCRETE_BUILD_PROFILE`, `CONCRETE_TOOLCHAIN_VERSION`
— surfaced **only through the resolved/typed fact layer**, never as hidden textual
substitution. The load-bearing requirement: every constant that affects compiled
source, proofs, obligations, or stdlib module selection must be **recorded in
reports and release bundles** (`concrete inspect --resolved --json`,
`--report audit`). A reviewer can then see exactly which target facts shaped the
artifact.

This is the same principle as [TARGET_CONDITIONAL.md](TARGET_CONDITIONAL.md):
target influence is a visible build/audit fact, not preprocessor state.

## Deferred — and why

No source-level target constants are added now. They should land **only when the
target-profile machinery actually needs them** — i.e., alongside #26's
profile/target-selected source roots and the cross-platform/freestanding stdlib
work. Introducing `CONCRETE_OS`-style names before there is a resolved-fact layer
to expose them through (and a build that varies by target) would be exactly the
hidden-state design this decision rejects.

When it lands, the acceptance bar is the roadmap's: `examples/target_constants/`
+ `scripts/tests/check_target_constants.sh` proving the constants appear in
`concrete inspect --resolved --json`, `--report audit`, and the release bundle —
not as ad-hoc compile-time globals.
