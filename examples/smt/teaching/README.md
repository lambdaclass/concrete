# When SMT is useful — and when Concrete refuses

A teaching group, not a test matrix. It explains the one question that matters
for the external solver: **when is SMT worth trusting, and when does Concrete
deliberately keep a fact on the kernel-checked path?**

## The one rule

Concrete exhausts its kernel-checked tiers first — constant fold → `omega` →
Lean `bv_decide`, all in-toolchain with no growth of the trusted base. It reaches
for an external solver **only** for what those genuinely cannot do: a *nonlinear*
integer product (`var * var`) whose result can't be bounded by interval analysis.
Everything else stays kernel-checked.

| you might expect SMT for… | reality | where |
| --- | --- | --- |
| a nonlinear product over signed ranges | **SMT** (`solver_trusted`) | `../nonlinear_overflow/` (`scale`) |
| a linear fact (`a + b`) | omega — **no SMT** | `kernel_preferred.con` (`linear_sum`) |
| a bounded non-negative product | bv_decide — **no SMT** | `kernel_preferred.con` (`bounded_product`) |
| a false claim | **counterexample** (non-proof) | `../nonlinear_overflow/` (`scale_unbounded`) |
| a fact outside the SMT fragment | shown `unproven`, **no query** | `unsupported_theory.con` |

`solver_trusted` is never kernel evidence: it needs policy allowance
(`[policy] solver-evidence`) and can graduate to `proved_by_lean_replay` only if a
kernel tactic independently closes it. See `../README.md`.

## Two cases that need backend work, not a teaching example

The roadmap also imagined `range_block_count` and `path_feasibility` as SMT
examples. In this backend they are **not** SMT cases, and we do not ship a fake
one:

- **`range_block_count`** — an HMAC-shaped `(len + 9 + 63) / 64 <= max` summary.
  The divisor is the constant `64`, which `omega` handles directly — so this is a
  *kernel* fact, not an SMT one. It is not yet shown as a VC only because division
  is not lowered into the assert/contract goal language (adding it naively would
  mismatch `Int./` vs Concrete `/` semantics on negatives — a soundness risk).
  Sound division lowering is the prerequisite, and then it lands in
  `kernel_preferred`, not here.
- **`path_feasibility`** — branch facts implying a postcondition. `omega` could
  close it *if* the enclosing branch conditions were threaded into the assert VC
  (call-site/bounds/div VCs already thread their scope; asserts do not yet). With
  that threading it is a kernel fact; without it the VC is honestly `unproven`.
  Either way it is not an external-solver case.

Both are queued as backend work (sound division lowering; path-condition threading
into assert VCs), not as teaching fixtures — shipping them as working SMT examples
would misrepresent what the solver is doing.
