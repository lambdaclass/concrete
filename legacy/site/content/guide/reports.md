+++
title = "Reports"
+++

# How Reports Fit Together

Concrete reports are not random extra outputs. They are part of the language's audit story.

The point is not only "the program compiled." The point is also:

- what authority does it require?
- where does `Unsafe` appear?
- what do trusted wrappers hide?
- where does allocation happen?
- what gets cleaned up?
- what ABI/layout facts are actually true?
- what monomorphized code was actually produced?

## The Report Surface

Concrete's report surface is built around a small set of modes:

- capability reports
- unsafe/trusted boundary reports
- allocation/cleanup reports
- layout reports
- interface reports
- monomorphization reports

These should be read together, not in isolation.

## One Program, Multiple Views

```text
                 program
                    |
    -----------------------------------------
    |         |         |        |     |    |
   caps    unsafe    alloc    layout  api  mono
```

Each report answers a different review question:

- `caps`: what authority exists, and why?
- `unsafe`: where are the danger boundaries?
- `alloc`: where are resources created, freed, or deferred?
- `layout`: what ABI-facing facts are true?
- `api`: what public surface is exposed?
- `mono`: what concrete code was instantiated?

## Why These Matter Together

A single review question often crosses report modes.

Example:

- "Can this dependency talk to the network?"
  - look at capability reports
- "Does it hide that behind a trusted wrapper?"
  - look at unsafe/trusted reports
- "Does it allocate in the hot path?"
  - look at allocation reports

That is why Concrete keeps pushing reports as a product surface instead of a debugging afterthought.

## Why-Traces

The most useful report shift so far is that reports increasingly explain *why*.

Not just:

- `function X needs File`

But:

- `function X needs File because it calls Y, which calls Z`

That is what makes reports usable in real review workflows.

## Reports And The Roadmap

Reports matter in two different phases:

- **Phase C** made them readable and routine
- **Phase D** should make them even more valuable by connecting them to stronger artifacts, proofs, and reproducibility

Longer term, reports should support:

- package/subsystem authority budgets
- high-integrity evidence review
- proof-carrying or evidence-carrying artifact workflows

## The Right Mental Model

Concrete reports are not a second compiler pipeline and not a second source of semantics.

They are:

- structured views over facts the compiler already knows
- meant for humans, CI, and later tooling
- one of the main reasons Concrete can become more auditable than ordinary systems languages
