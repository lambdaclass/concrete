# Optimization Policy

**Status:** Open research direction
**Affects:** Backend design, performance policy, debug/observability quality, incrementality, proof/audit story
**Date:** 2026-03-15

## Purpose

This note defines how Concrete should think about optimization as a project policy, not only as opportunistic compiler work.

The main question is not "can the compiler become faster?"
It is:

how should Concrete pursue performance without quietly eroding auditability, observability, or proof-friendliness?

## Why This Matters

Concrete is not trying to be a generic optimizer playground.
Its backend work needs explicit goals and explicit non-goals.

Without that, performance work can drift into:

- hidden semantic coupling
- harder-to-explain code generation
- degraded debug/inspection quality
- optimization folklore instead of stated design

## Recommended Optimization Stance

Concrete should prefer:

- explicit optimization levels with documented intent
- optimizations that preserve backend transparency
- optimizations whose audit/debug cost is understood
- performance regression discipline tied to representative workloads

Concrete should avoid:

- undocumented optimizer cleverness
- transformations that make reports or inspection surfaces misleading
- performance wins that silently depend on unstable compiler coupling

## Project Questions To Answer

1. What is the default optimization goal?
2. Which optimization families are clearly in-scope?
3. Which are intentionally out-of-scope or late?
4. How much debug/observability quality may optimization trade away?
5. How should performance regressions be measured and enforced?

## Likely Good In-Scope Work

- straightforward SSA cleanup and simplification
- predictable scalar optimizations
- explicit, auditable function inlining policy where it clearly improves code quality without hiding control/report structure excessively
- explicit lowering/ABI improvements
- backend decisions that remove obviously unnecessary work
- profiling-guided performance investigation on real workloads

## Likely Late Or Careful Work

- large backend-family expansions
- aggressive target-specific peephole policy
- optimization work that obscures report/debug surfaces
- incrementality built on unstable artifact contracts

## Debug And Observability Constraint

Optimization policy should explicitly account for:

- debug info quality
- stack trace usefulness
- symbol fidelity
- report truthfulness under optimized builds

These are not secondary concerns. They are part of product quality.

## Relationship To Performance Validation

Optimization policy and performance validation should stay linked:

- choose representative workloads
- define what counts as a regression
- decide what is measured in CI vs local profiling
- keep the metrics narrow and meaningful

## Relationship To Other Research

- [complete-language-system.md](complete-language-system.md)
- [testing-strategy.md](testing-strategy.md)
- [showcase-workloads.md](showcase-workloads.md)

## Working Conclusion

Concrete should treat optimization as an explicit policy surface:

- stated goals
- stated non-goals
- stated observability constraints
- stated regression expectations

That is much healthier than treating backend performance as ambient compiler folklore.
