# The Concrete Programming Language

[![CI](https://github.com/unbalancedparentheses/concrete2/actions/workflows/lean_action_ci.yml/badge.svg)](https://github.com/unbalancedparentheses/concrete2/actions/workflows/lean_action_ci.yml)
[![license](https://img.shields.io/github/license/lambdaclass/concrete)](LICENSE)

Concrete is a systems programming language without garbage collection. It checks resource
ownership, makes external authority explicit at every use, and is specified in Lean so that its
rules can be proved about.

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
