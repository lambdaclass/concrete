+++
title = "Papers"
+++

# Papers

Concrete includes two in-repository research papers written in Typst.

**_When Proofs Go Stale: Evidence-Carrying Source in a Small Systems Compiler_**
([`paper/evidence-carrying.typ`](https://github.com/unbalancedparentheses/concrete2/blob/main/paper/evidence-carrying.typ))
is the paper about the proof story. Its thesis:

- a proof about source code can go stale silently — the body changes, the
  theorem does not, and the build stays green
- Concrete treats this as a *compiler* problem: it extracts a proof-eligible
  fragment of checked Core into Lean, records attached theorems in a registry
  keyed by a body fingerprint, and **revokes a `proved` claim when the source no
  longer matches what was proved** (the spec-drift gate)
- the result is not a verified compiler; it is a compiler that keeps evidence
  current enough for review
- the strongest case study is a bounded **HMAC-SHA256** whose *exact extracted
  body* is proved to refine an independent Lean SHA-256/HMAC spec (11 registered
  theorems), with the reusable machinery factored into `ProofKit`

**_Concrete: Explicit Systems Programming With Visible Authority and Ownership_**
([`paper/main.typ`](https://github.com/unbalancedparentheses/concrete2/blob/main/paper/main.typ))
is the language paper:

- Concrete is not trying to hide authority, ownership, or cleanup costs
- its central goal is auditability, not maximal abstraction
- the compiler is intended to produce mechanically reviewable semantic reports
- the proof story is based on a checked-core boundary and a narrower proof-oriented subset

## Source

- [paper/evidence-carrying.typ](https://github.com/unbalancedparentheses/concrete2/blob/main/paper/evidence-carrying.typ) — the proof / evidence-carrying paper
- [paper/main.typ](https://github.com/unbalancedparentheses/concrete2/blob/main/paper/main.typ) — the language paper
- [paper/README.md](https://github.com/unbalancedparentheses/concrete2/blob/main/paper/README.md)

## Build

From the repository root (nix-backed Typst toolchain):

```bash
make papers     # both papers
make paper      # the language paper (paper/main.pdf)
make paper-ec   # the evidence-carrying paper (paper/evidence-carrying.pdf)
```

## Related

- [Why Concrete](@/guide/landing.md)
- [Safety](@/reference/SAFETY.md)
- [Architecture](@/reference/ARCHITECTURE.md)
- [Roadmap](https://github.com/unbalancedparentheses/concrete2/blob/main/ROADMAP.md)
