# Target-Conditional Code Selection

Status: decision — ROADMAP Phase 6 #26.

How does a Concrete project select different code for different targets/profiles
(OS, arch, freestanding vs hosted, build profile)? This document records the
decision so freestanding and cross-platform stdlib work can proceed without
prematurely committing to a preprocessor.

## Decision

**Prefer profile/target-selected source roots and modules in `Concrete.toml`
over in-source conditional attributes.** The unit of conditional compilation is a
*module or source root chosen by the build*, not a `#if`-style branch scattered
through a function body.

Rationale:

- **No hidden preprocessor state.** Which file compiled for which target is a
  build/audit fact (visible in `Concrete.toml` and in reports), not invisible
  textual substitution. This matches Concrete's "no semantically dark
  constructs" stance: a reviewer can see exactly which source the target selected.
- **LL(1)-clean.** Source-root selection needs no new in-language syntax, so the
  grammar (`grammar/concrete.ebnf`, three independent LL(1) checkers) is
  untouched.
- **Auditable.** The selected target/profile and the source set it pulled in are
  recorded as resolved facts (see ROADMAP #27 target constants), so reports and
  release bundles can state what affected the compiled artifact.

`Concrete.toml` already has a `[profile]` section (parsed by `Concrete/Resolve/Project.lean`);
profile/target-keyed source roots build on that mechanism rather than introducing
a parallel one.

## Deferred: narrow `cfg`-style attributes

In-source conditional attributes (a `cfg`-like form) are **deferred, not
forbidden**. If a real workload shows that source-root selection is too coarse
(e.g. a three-line endianness swap inside one otherwise-shared function), a
narrow attribute MAY be added later, but only under these constraints:

1. **LL(1)-safe** — fits the existing attribute grammar; no new ambiguity.
2. **Small and target/profile-only** — conditions over target/profile facts
   (`CONCRETE_OS`, `CONCRETE_ARCH`, `CONCRETE_TARGET_PROFILE`, …), never arbitrary
   expressions; no nesting into a general macro system (see MACRO_STANCE.md).
3. **Reported in audit** — every conditional that affected compiled source,
   proofs, obligations, or stdlib module selection appears in `--report audit`,
   exactly like the target constants in #27.

Until a workload pulls it, there is **no `cfg` in the language**: target-conditional
code is expressed by build-selected source roots.

## Relationship to other items

- ROADMAP #27 (target constants) provides the typed, audit-visible target facts
  that both source-root selection and any future `cfg` would condition on.
- ROADMAP #10 / [PROFILES.md](PROFILES.md): *build profiles* are policy bundles
  (gates/reports/evidence), not arithmetic modes and not target selectors;
  target selection is a separate axis layered on the same `[profile]` mechanism.
