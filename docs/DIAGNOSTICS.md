# Diagnostics

Status: stable reference

This document describes Concrete's diagnostics model and the remaining diagnostics work.

For pass ownership, see [PASSES.md](PASSES.md). For active priorities, see [../ROADMAP.md](../ROADMAP.md). For the safety model that diagnostics enforce, see [SAFETY.md](SAFETY.md). For the eight audit report modes, see [PASSES.md](PASSES.md).

## Current Model

Concrete has structured error kinds across the semantic pipeline:

- `ResolveError`
- `CheckError`
- `ElabError`
- `CoreCheckError`
- `SSAVerifyError`

The parser and AST now carry source spans, including range-capable spans, and semantic diagnostics render with source locations.

## Current Strengths

Today the compiler already has:

- structured per-pass error kinds
- span-bearing diagnostics
- native `Except Diagnostics` plumbing through `Check`, `Elab`, `CoreCheck`, and `SSAVerify`
- range-aware rendering support in `Diagnostic`
- hint text on a growing set of semantic diagnostics
- stable rendered messages for the semantic passes
- a shared `Diagnostic` type
- module/function-level and statement-level multi-error accumulation in `Check` and `Elab`
- actionable `hint:` text on capability-related errors (suggests `with(Cap)` or trusted wrappers)

This means diagnostics are no longer mostly raw strings, and pass ownership is visible in emitted errors.

## Remaining Work

The remaining diagnostics work is now mostly about fidelity and presentation, not basic plumbing.

### 1. Better span/range fidelity

Improve the precision of source reporting:

- range-aware spans
- better postfix/operator-site highlighting
- cleaner attachment of diagnostics to transformed constructs

### 2. Rendering quality

Add richer presentation support:

- secondary labels
- notes
- suggestions
- more consistent multi-line formatting

### 3. Accumulation refinement (done)

Concrete accumulates across functions/modules and within function bodies (statement-level) in both `Check` and `Elab`. The current granularity is intentional — statement-level recovery catches independent errors without guessing at expression-level placeholders.

Further accumulation refinement (e.g., expression-level recovery, cross-block recovery) should only happen if it improves real diagnostic quality without producing misleading cascading errors.

## Current Architectural Rule

Diagnostics work should proceed in this order:

1. improve span/range fidelity
2. improve rendering quality
3. only then consider broader accumulation/refinement

The basic diagnostics plumbing is already in place. Remaining work should avoid reopening that boundary unless there is a clear payoff.

## Current Accumulation Policy

Concrete is no longer purely fail-fast in the semantic pipeline.

Current behavior:

- `Resolve` accumulates diagnostics across shallow/interface and body-level work
- `Check` and `Elab` accumulate across functions/modules
- `Check` and `Elab` also accumulate within function bodies at statement granularity: `checkStmts` and `elabStmts` catch per-statement errors, restore the type environment on failure, and add placeholder types for failed let-declarations to prevent cascading errors

This means users see all independent errors in a function body, not just the first one. The recovery is bounded — it operates at statement level, not expression level — which avoids guessing at placeholder values while still catching most independent errors.

Design constraints:
- Failed let-declarations add the declared type (or `.placeholder`) to the environment so subsequent statements can still be checked/elaborated
- Environment is restored to pre-statement state on failure, keeping the type environment honest
- All accumulated diagnostics are thrown together at the end of the statement list

If this changes further later, this document should be the place where the policy and rollout are recorded.
