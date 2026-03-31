+++
title = "Paper"
+++

# Paper

Concrete includes an in-repository research paper written in Typst.

The paper is the best compact statement of the project thesis:

- Concrete is not trying to hide authority, ownership, or cleanup costs
- its central goal is auditability, not maximal abstraction
- the compiler is intended to produce mechanically reviewable semantic reports
- the proof story is based on a checked-core boundary and a narrower proof-oriented subset

## Source

- [paper/main.typ](https://github.com/unbalancedparentheses/concrete2/blob/main/paper/main.typ)
- [paper/README.md](https://github.com/unbalancedparentheses/concrete2/blob/main/paper/README.md)

## Build

From the repository root:

```bash
make paper
```

This uses the nix-backed Typst toolchain defined in the repository.

## Related

- [Why Concrete](@/guide/landing.md)
- [Safety](@/reference/SAFETY.md)
- [Architecture](@/reference/ARCHITECTURE.md)
- [Roadmap](https://github.com/unbalancedparentheses/concrete2/blob/main/ROADMAP.md)
