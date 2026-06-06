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

### Provenance & determinism

Every SMT-routed VC records, in `--report vcs --smt` and JSON (`smt` object), the
information needed to reproduce its verdict: the **logic** (`QF_NIA`), the
**timeout** (5 s), a stable **`smtlib_sha`** digest of the exact query, the
**solver** identity + version (e.g. `z3 4.16.0`), the **query** itself (the replay
artifact), and a **replay** command. The `smtlib_sha` and result class are
deterministic across runs; `check_smt_path.sh` pins this. `timeout`, `unknown`,
and `solver_error` are always treated as non-proofs.

## Lean replay (`--emit-lean-replay` / `--smt --replay`)

`solver_trusted` is not the end of the story: for each SMT VC we also emit a
**standalone Lean theorem** restating the same obligation, with an in-toolchain
proof attempt (`by omega`):

```sh
concrete examples/smt/nonlinear_overflow/src/main.con --report vcs --emit-lean-replay
# theorem vc_replay (sample gain : Int) (h0 : ((-30000) ≤ sample ∧ ...)) ... := by omega
```

If a kernel-checked tactic **independently** closes that theorem, the VC graduates
`solver_trusted` → **`proved_by_lean_replay`** (engine `lean:omega`) — the solver
is no longer part of the claim, so it is **not** subject to `solver-evidence`
policy. `--smt --replay` runs the attempt and folds the result in.

**Today this fragment stays `solver_trusted`.** The obligations we route to SMT
are bounded *nonlinear* integer facts, which `omega` cannot close and `bv_decide`
cannot bound — and `nlinarith` lives in Mathlib, which is deliberately **not** a
dependency. So the in-toolchain replay does not close, and the class boundary
holds: nothing is silently upgraded. The artifact is still emitted so a reviewer,
or a Mathlib-enabled build that swaps `omega` → `nlinarith`, can check it and
graduate the evidence. Pinned by `check_smt_replay.sh`.

## Release-policy gate (`[policy] solver-evidence`)

**SMT is useful, but `solver_trusted` is not Lean/kernel evidence unless replayed.**
A project declares its release stance and `concrete build` enforces it — if an
external solver discharged a VC as `solver_trusted`, the build is rejected (E0615)
unless the policy allows it:

```toml
[policy]
solver-evidence  = "forbid"        # solver_trusted is a release blocker (default stance)
# solver-evidence = "allow"        # accepted
# solver-evidence = "assumptions"  # accepted ONLY with a named justification:
# solver-assumption = "z3-4.16-QF_NIA-trusted"
```

The gate inspects the solver results, not just the flag: a `forbid` project that
relies on a solver-only VC fails to build; an `assumptions` project must name what
it trusts (the solver + version + logic), recording the provenance for audit.
`counterexample` / `unknown` / `timeout` / `solver_error` are non-proofs and never
count as evidence, so they never trip — or satisfy — this gate. Fixtures:
`examples/smt/policy_forbid/`, `policy_allow/`, `policy_assumptions_missing/`;
pinned by `check_smt_policy.sh`.

## The boundary is enforced

- SMT only ever touches a VC the kernel tiers left `unproven`
  (`Report.foldSmtResults`) — it can never override or be confused with a
  `proved_by_kernel_decision` / `proved_by_lean` result.
- Without `--smt`/`--emit-smt`, no VC advertises `smt` (`expected_discharge`,
  `engine`) or carries a solver status. `check_vc_schema.sh` pins the default;
  `check_smt_path.sh` pins the flagged behaviour.

## Negatives — the honesty boundaries (`negatives/`)

A solver result is a **non-proof** unless it is a genuine kernel/Lean discharge.
`check_smt_negatives.sh` pins every negative path so a solver result is never
mistaken for proof:

| case | result | is it a proof? |
| --- | --- | --- |
| solver absent | `solver_error` | no |
| configured tiny timeout (`--smt-timeout-ms 1`) | `unknown` / `timeout` | no |
| out-of-fragment obligation (e.g. linear `a + b`) | **no SMT query** | no — kernel owns it |
| satisfiable negated goal (`scale_unbounded`) | `counterexample` | no |
| `[policy] solver-evidence = forbid` | release build **fails** (E0615) | n/a |
| Lean replay cannot close | stays `solver_trusted` | no |

`examples/smt/negatives/` holds `linear_add` (the out-of-fragment case — SMT must
not reach for what the kernel tiers own); the other cases reuse
`nonlinear_overflow/` and `policy_forbid/`.

## `nonlinear_overflow/`

Two `sample * gain` products that the kernel tiers cannot own (nonlinear; omega
can't, and interval `bv_decide` bails) — the genuine SMT niche.

- **`scale`** — operands in signed ranges, product provably fits `i32`
  (`|s·g| ≤ 30000·60000 < 2³¹`). Z3 returns `unsat` → `solver_trusted`.
- **`scale_unbounded`** — bounds too loose (`100000·100000 = 10¹⁰ > 2³¹`), so the
  product *can* overflow. Z3 returns `sat` and the emitted `(get-model)` is parsed
  back to **source variables**, e.g.:

  ```
  [smt.scale_unbounded#ovf0]  no_overflow
      status:  counterexample (smt:z3)
      counterexample:  sample = 99161, gain = 98166
  ```

  Status is `counterexample`, never a proof — the contract bug surfaces with
  concrete inputs. This is the user-facing payoff of the SMT path: a
  counterexample in source terms is more valuable for debugging a contract than a
  trusted "proved". The model variable names are the function's own parameters; no
  remapping is needed because the SMT query declares them by source name.
