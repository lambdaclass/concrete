# two_kernel_demo — multi-kernel evidence (spike)

Demonstrates the prover-neutral obligation layer on branch
`spike/multi-prover-evidence`: the *same* linear no-overflow obligation is
discharged by multiple independent kernels, and the evidence class reflects how
many agreed.

## Run

The second-kernel tools live in the `provers` dev shell (Isabelle is a multi-GB
closure, so it is kept out of the default shell):

```sh
nix develop .#provers -c concrete examples/two_kernel_demo/src/main.con --report multi-kernel --all-provers
```

Or a single external kernel:

```sh
concrete examples/two_kernel_demo/src/main.con --report multi-kernel --rocq
concrete examples/two_kernel_demo/src/main.con --report multi-kernel --isabelle
```

Without any flag it reports Lean only; absent tools report `unavailable`, not a
false verdict.

## What it shows

- `add_bounded` — `a + b` with both operands bounded → all kernels close it →
  `proved_by_multi_kernel` (or `proved_by_two_kernels` with one external).
- `mul_unbounded` — `a * b` with only sign bounds → **no** kernel can bound it →
  stays `unproven`. This is the teeth: the badge is earned, not stamped.

## What it does NOT show

This attests *checker* diversity (independent kernels each accept a lowering of
the obligation), not *bridge* diversity: the Core→obligation lowering is
single-sourced and trusted, identical for every kernel. See
`research/proof-evidence/response-to-why-rocq-is-better.md`.
