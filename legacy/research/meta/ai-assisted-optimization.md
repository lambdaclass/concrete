# AI-Assisted Optimization Via Structured Reports

Status: exploratory note

Concrete's report system (`--report authority`, `--report alloc`, `--report proof`, and related outputs) produces structured facts about programs, not just "it compiled." That creates a plausible feedback loop for automated optimization and refactoring agents.

The interesting property is not generic "AI for code." It is that Concrete already computes semantic facts that can be checked mechanically after each rewrite:

- what authority each function requires
- where allocation happens
- which functions are pure enough to prove
- where trusted or foreign boundaries exist

This gives an optimization loop a better oracle than raw benchmarks alone.

## Concrete Missing Artifact

AI-assisted optimization needs an agent-readable performance research packet,
not an open-ended instruction to "look around the repo."

See [../compiler/performance-research-packets.md](../compiler/performance-research-packets.md).

That packet should collect benchmark history, compiler reports, IR/binary size,
allocation/effects facts, known hypotheses, and safety/evidence guardrails in
one place.

## Promising Directions

### Allocation minimization

`--report alloc` can identify which functions allocate and where cleanup happens.

An agent could try to:
- move a function from `Unbounded` toward `Bounded` or `NoAlloc`
- reduce allocation sites while preserving behavior
- confirm that a change actually reduced allocation rather than merely moving it elsewhere

### Purity expansion

`--report proof` identifies which functions are already pure enough to be proof-eligible.

An agent could try to:
- extract pure cores from effectful wrappers
- push capabilities to narrower boundary functions
- expand the proof-eligible subset without changing semantics

### Authority narrowing

`--report authority` shows the transitive chain behind each capability.

An agent could try to:
- remove unnecessary authority from helper functions
- isolate logging, file access, or other effects at the boundary
- verify that a refactor did not silently expand the authority surface

### Performance with semantic guardrails

Benchmark-driven optimization is noisy and can accidentally widen trust boundaries or increase allocation.

Concrete's reports make it possible to ask a stronger question:
- did the program get faster
- without adding authority
- without expanding trusted or foreign boundaries
- without worsening allocation behavior

## Why Concrete Is Unusually Suitable

Concrete is relatively tractable for this style of automated work because it has:

- no hidden runtime finalization
- no closures
- no dynamic dispatch as the default design center
- explicit capabilities
- explicit trust/unsafe boundaries
- structured compiler passes with report surfaces already designed for inspection

This is not a phase by itself. It becomes more practical as:

- machine-readable reports mature
- authority budgets and policy gates mature
- allocation classification matures
- source locations become available in reports
- performance packets make the current state easy for humans and LLM agents to inspect

Near-term experiments could still happen with current text reports, but the stronger version depends on later report/output maturity.
