# External-SMT examples (Phase 2 #8/#9)

Concrete exhausts its **kernel-checked** discharge tiers first — constant fold →
`omega` → Lean `bv_decide` — all in-toolchain with no growth of the trusted base.
What genuinely remains outside them (nonlinear integer arithmetic) can be handed
to an **external SMT solver**, but the result is never confused with kernel
evidence:

| route | evidence class | trust |
| --- | --- | --- |
| constant fold / omega / bv_decide | `proved_by_kernel_decision` | kernel-checked, no TCB growth |
| registered Lean proof | `proved_by_lean` | kernel-checked |
| **external solver (Z3) — `unsat`** | **`solver_trusted`** | **solver enters the TCB (no replay yet)** |
| external solver — `sat` | `counterexample` | — |
| external solver — `unknown` / `timeout` | `unknown` / `timeout` | not a proof |
| solver missing / crashed | `solver_error` | not a proof |

## Opt-in only

The external solver is **never** reached by default. The VC report shows the
nonlinear obligation as `unproven` until you pass a flag:

```sh
# emit the stable SMT-LIB query (no solver needed — the auditable artifact):
concrete examples/smt/nonlinear_overflow/src/main.con --report vcs --emit-smt

# run Z3 (timeout pinned) and fold the result in as solver_trusted / counterexample:
concrete examples/smt/nonlinear_overflow/src/main.con --report vcs --smt
```

If Z3 is not on `PATH`, `--smt` reports `solver_error` for every query — an absent
solver never yields a proof.

## The boundary is enforced

- SMT only ever touches a VC the kernel tiers left `unproven`
  (`Report.foldSmtResults`) — it can never override or be confused with a
  `proved_by_kernel_decision` / `proved_by_lean` result.
- Without `--smt`/`--emit-smt`, no VC advertises `smt` (`expected_discharge`,
  `engine`) or carries a solver status. `check_vc_schema.sh` pins the default;
  `check_smt_path.sh` pins the flagged behaviour.

## `nonlinear_overflow/`

`scale(sample, gain) = sample * gain` with both operands in signed ranges. The
product provably fits `i32` (`|s·g| ≤ 30000·60000 < 2³¹`), but:

- `omega` can't own it — the goal is nonlinear (a product of two variables);
- the interval `bv_decide` path models unsigned, non-negative operands, so signed
  ranges make it bail.

So it is the genuine SMT niche: useful for an external solver, **not**
kernel-equivalent. Z3 returns `unsat` on the emitted query → `solver_trusted`.
