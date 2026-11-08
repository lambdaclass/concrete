# Documentation

Start with the [project overview](../README.md) for motivation and current implementation status.

| Reader | Start here | Purpose |
| --- | --- | --- |
| Exploring Concrete | [Philosophy](PHILOSOPHY.md) and [roadmap](../ROADMAP.md) | Design principles, active work and acceptance milestones. |
| Reading the language | [Specification guide](../ConcreteSpec/README.md) | Proposed Lean rules, proof boundaries, examples and open obligations. Core freeze determines normative status. |
| Reading the compiler | [Compiler architecture](../Concrete/README.md) | Pipeline design, implemented slice boundaries, premises and gates. |
| Checking a design choice | [Owner decisions](decisions.md) | Decided rules and questions still awaiting the owner. |
| Reviewing a proposal | [Proposal index](proposals/README.md) | Pending questions, adopted plans and review evidence. |
| Contributing | [Contributor rules](../AGENTS.md), [review](REVIEW.md), [landing](LANDING.md) | Branch isolation, validation, review and publication. |
| Operating CI | [Runner guide](SELF_HOSTED_RUNNERS.md) | Runner pools and operating constraints. |
| Investigating history | [Roadmap archive](archive/roadmap-2026-11/README.md) and [retention manifest](../RETENTION.tsv) | Earlier decisions, task dispositions and retained assets. |

The proposed Lean library and the owner's decisions determine language status; the roadmap
schedules work. Proposal approval is distinct from implementation and proof acceptance. The
frozen `legacy/` tree is historical material and is never edited or run here.

A user tutorial is planned in phase C (R-0334). Until then, the specification guide explains how
to read the current examples and generate the browsable owner view.
