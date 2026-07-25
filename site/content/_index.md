+++
title = "Concrete"
+++

Concrete is an experimental systems language for inspectable safety claims.

The site is organized around evidence: what the source claims, which checks
the compiler enforces, what the reports prove, what tests only exercise, and
which assumptions or trusted boundaries remain.

One consequence of the language's refusals is easy to miss: with no closures,
no trait objects, no macros, and whole-program monomorphization, callable values
come from a closed set of named functions. A function-pointer target may still
be selected at runtime, but its possible targets are statically enumerable.
That gives per-function facts about authority, allocation, and failure a
tractable path toward whole-program claims.

Use the guide for a narrative path through the language. Use the reference for
the stable invariants, value model, safety model, and compiler/reporting
contracts.
