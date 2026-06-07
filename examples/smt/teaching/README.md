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
| an HMAC block-count summary (`(len+72)/64 <= 6`) | omega — **no SMT** | `range_block_count.con` (`nblocks`) |
| a false claim | **counterexample** (non-proof) | `../nonlinear_overflow/` (`scale_unbounded`) |
| a fact outside the SMT fragment | shown `unproven`, **no query** | `unsupported_theory.con` |

`solver_trusted` is never kernel evidence: it needs policy allowance
(`[policy] solver-evidence`) and can graduate to `proved_by_lean_replay` only if a
kernel tactic independently closes it. See `../README.md`.

The roadmap also imagined `range_block_count` and `path_feasibility` as *SMT*
examples. Neither is — both are kernel facts, and `range_block_count` now works
as one (ROADMAP #21: sound division lowering — division is lowered to a VC only
when the dividend is provably non-negative, since Concrete's toward-zero `/`
agrees with Lean's floor `/` exactly there; `range_block_count.con::signed_div`
is the negative control proving a possibly-negative dividend is never mis-proved).

## One case still queued as backend work

- **`path_feasibility`** — branch facts implying a postcondition. `omega` could
  close it *if* the enclosing branch conditions were threaded into the assert VC
  (call-site/bounds/div VCs already thread their scope; asserts do not yet —
  ROADMAP #22). Without that threading the VC is honestly `unproven`; either way
  it is not an external-solver case. Not shipped as a teaching fixture until the
  threading lands.
