# REPL And Playground Direction

Status: research

## Problem

Concrete is a low-level language with:

- explicit ownership
- explicit capabilities
- explicit cleanup
- proof/report-oriented artifacts

That makes a traditional interactive REPL an awkward fit.

At the same time, there are reasonable lightweight use cases for interactive tooling:

- trying small examples
- inspecting compiler facts quickly
- teaching or demos
- querying ownership/capability/report state without a full edit-build-run cycle

## Why This Is Not An Obvious Fit

A fully stateful language REPL pushes against several Concrete instincts:

- low-level resource lifetimes become awkward in a persistent session
- capability context becomes ambiguous unless made explicit
- cleanup behavior can become less visible
- it risks encouraging an exploratory style that is not the main value proposition of the language

Concrete should be careful not to build an environment that requires softening the language's explicitness.

## Narrow Versions That Might Make Sense

The most plausible versions are not "full dynamic language shell."

Better candidates are:

1. **Artifact/query shell**
   - inspect reports
   - inspect proof eligibility
   - inspect authority, allocation, or layout facts
   - ask semantic search/query questions interactively

2. **Example playground**
   - small code snippets
   - constrained capabilities/profiles
   - easy save/export into ordinary source files
   - useful for docs, onboarding, and demos

3. **Debug/inspection shell**
   - explore emitted Core/SSA/LLVM for one function
   - inspect per-function artifacts without a full manual workflow

These all fit Concrete better than a REPL that tries to be the primary development environment.

## What Not To Do

Do not:

- make the REPL a core language design driver
- hide ownership or cleanup to make interactivity feel nicer
- create session semantics that diverge from ordinary compilation
- prioritize this above `check`, `watch`, per-function inspection, or report/query tooling

## Roadmap Placement

This belongs in:

- **Phase O** only, as a low-priority research item

If it ever becomes real, it should likely build on:

- machine-readable reports
- semantic query/search tooling
- per-function artifact inspection
- later package/example tooling

## Current Recommendation

Keep this visible as:

- **research only**

and phrase it narrowly:

- Concrete may eventually want a constrained playground or interactive artifact/query shell
- it should not become a reason to bend the core language model toward interactivity
