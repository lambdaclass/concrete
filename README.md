# The Concrete Programming Language

[![CI](https://github.com/unbalancedparentheses/concrete2/actions/workflows/lean_action_ci.yml/badge.svg)](https://github.com/unbalancedparentheses/concrete2/actions/workflows/lean_action_ci.yml)
[![license](https://img.shields.io/github/license/lambdaclass/concrete)](LICENSE)

**Concrete is a fully proven systems programming language with no garbage collector, where you can
know everything a program does: what it does, what it may touch, whether it can fail or stop, and
what is proved about it.** Every unproven step is named as an explicit assumption, never silently
trusted ([decision](docs/decisions.md#decided-project-core-statement)).

It checks resource ownership, makes external authority explicit at every use, and is specified in
Lean so that its rules can be proved about. From the first compiler slice, a per-function facts
report states what each function may do: its capabilities and reachable resources, allocation,
trusted and foreign boundaries, how each possible failure is handled, termination and proof status
([decision](docs/decisions.md#decided-project-facts-report)).

An accepted program has no hidden traps: an index, a division, a shift or an arithmetic operation
is either proven safe when the program is compiled or has its failure handled in the code. Before a
program runs, every function's termination is known: proved, or traced to a written permission at the
loop or boundary responsible, and the compiler reports recursion depth, stack use, allocation,
blocking and foreign calls per function, SPARK-style. Each guarantee holds as its evidence lands
([decision](docs/decisions.md#decided-project-no-hidden-traps)).

**Status: restarting.** A new compiler is being built from a Lean specification. Nothing at this
root compiles Concrete programs yet.

## Where things are

| Path | What it is |
|---|---|
| [ROADMAP.md](ROADMAP.md) | The plan, in order. It decides nothing; the decisions below do. |
| [docs/PHILOSOPHY.md](docs/PHILOSOPHY.md) | What Concrete is for and the rules it follows. |
| [docs/decisions.md](docs/decisions.md) | The owner's recorded language decisions. |
| [ConcreteSpec/](ConcreteSpec/) | The specification: readable Lean with doc comments and examples. |
| [AGENTS.md](AGENTS.md) | How work is done and landed here (humans and agents alike). |
| [ISSUES.tsv](ISSUES.tsv) | Defects in the spec, proofs, CI and tooling, until the new bug ledger returns. |
| [RETENTION.tsv](RETENTION.tsv) | Where every asset of the old tree went, and when it may be deleted. |
| [legacy/](legacy/) | The old compiler and everything around it, frozen byte for byte. |

## What Concrete learns from

| Part of Concrete | Main precedent |
|---|---|
| Linear ownership, scoped borrows, capabilities in signatures | Austral |
| Mutable value semantics: mutation through scoped access, no returned or stored references | Hylo (formerly Val) |
| A small explicit language: no hidden control flow or allocation, allocators passed as parameters | Zig |
| Compiler-enforced safety culture and an explicit trusted boundary | Rust |
| Verified compilation: source behaviour carried down to the binary | CakeML and CompCert |
| Absence of run-time errors and the assurance workflow | SPARK, with Dafny and Why3 |
| Rules and proofs in one checked system; the implementation language | Lean 4 |

Smaller influences include Aeneas and Effekt. The full comparison, including what Concrete takes
from each and what it rejects, is archived in
[legacy/docs/project/INFLUENCES.md](legacy/docs/project/INFLUENCES.md).

## The old compiler

`legacy/` is a frozen snapshot of the old tree: it is never edited and never run here. The old
compiler is tag `legacy-compiler-final`; to use it, check that tag out in its own worktree. The
differential oracle is built from the tag, not from `legacy/` (`scripts/oracle/`). Assets the new
compiler still needs (programs, examples, std, the bug ledger, the site) stay archived in `legacy/`
until they migrate; [RETENTION.tsv](RETENTION.tsv) records each one.

## Building

```sh
nix develop            # Lean and the tools the checks use
lake build             # the specification
bash scripts/checks/run.sh   # the root checks (also run by the pre-push hook and CI)
```

## License

[Apache 2.0](LICENSE).
