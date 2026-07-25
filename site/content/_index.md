+++
title = "Concrete"
+++

Concrete is an experimental systems language for inspectable safety claims.

The site is organized around evidence: what the source claims, which checks
the compiler enforces, what the reports prove, what tests only exercise, and
which assumptions or trusted boundaries remain.

One consequence of the language's refusals is easy to miss: with no closures,
no trait objects, no macros, and whole-program monomorphization, every call
target is known at compile time. The whole program is statically knowable, not
just inspectable — per-function facts about authority, allocation, and failure
compose into whole-program facts, and that is the path from "reported" to
"proved."

Use the guide for a narrative path through the language. Use the reference for
the stable invariants, value model, safety model, and compiler/reporting
contracts.
