# External-SMT soundness

Concrete may hand a verification condition to an external SMT solver, but only
under tight, auditable conditions. This document states exactly what is trusted,
what is encoded, what is out of scope, and — most importantly — **how a solver
bug affects each evidence class.** The short version: an external solver can only
ever produce `solver_trusted` (or a non-proof), and that class is firewalled from
every kernel-checked class.

See also: [CONTRACTS_AND_VCS.md](CONTRACTS_AND_VCS.md) (the VC pipeline),
[OBLIGATION_CORE.md](OBLIGATION_CORE.md) (the evidence ledger),
`examples/smt/README.md` (worked examples), and the gates `check_smt_path.sh`,
`check_smt_negatives.sh`, `check_smt_policy.sh`, `check_smt_replay.sh`.

## Trusted solver binary

- **Solver:** Z3, invoked as `z3` on `PATH`. Its identity and version are
  recorded per VC (`smt.solver`, e.g. `z3 4.16.0`) for provenance.
- **Configuration (pinned):** logic `QF_NIA`; per-run wall timeout `-T:5`
  (overridable to a millisecond soft timeout via `--smt-timeout-ms` for the
  timeout negative case). The exact query is recorded as `smt.query` and digested
  as `smt.smtlib_sha`, so any run is reproducible: `z3 -T:5 vc.smt2`.
- **Absent / failed solver:** if `z3` is not on `PATH`, or crashes, or prints
  anything other than `sat`/`unsat`, the verdict is `solver_error` — never a
  proof. An absent solver yields no evidence.
- **Opt-in:** the solver is never invoked unless `--smt` (report) is passed, and
  for a release build only when `[policy] solver-evidence` takes a stance. The
  default path touches no solver and emits no SMT data.

When a VC reaches `solver_trusted`, **the solver binary is part of the trusted
computing base for that one claim** — exactly as recorded in the audit bundle and
gated by policy.

## Encoding assumptions

The query is generated from the structured obligation expression
(`Report.exprToSmt`), not by re-parsing a rendered string, so it is well-formed
by construction. The encoding assumes:

- **Theory:** quantifier-free nonlinear integer arithmetic (`QF_NIA`). Variables
  are declared `Int`; the fragment is integer literals, `+ - *`, comparisons,
  `and`/`or`/`not`, and unary negation.
- **Refutation shape:** the query asserts the in-scope hypotheses (the function's
  `#[requires]`) and the **negation** of the goal, then `(check-sat)`. `unsat`
  means the goal holds (`solver_trusted`); `sat` means a counterexample exists
  (`counterexample`, with the model mapped back to source variable names because
  the query declares each variable by its source name).
- **Soundness gate on the query itself:** if any hypothesis falls outside the SMT
  fragment, the whole query is **dropped, not emitted** — a query missing a
  constraint could read as a spurious counterexample. Better no query than an
  unsound one.

## Unsupported theories

A construct outside the encodable fragment is **never silently dropped and never
sent to the solver**. The obligation stays visible:

- An obligation kind not routed to SMT (e.g. array bounds, loop preservation) is
  shown with its normal status (`unproven` if no kernel tier closed it).
- An operand outside `exprToSmt`'s fragment (shifts, xor, calls, non-integer)
  produces no SMT query; the obligation remains `unproven`.

This is the `unsupported_theory` / out-of-fragment case in
`examples/smt/teaching/` and `check_smt_negatives.sh`.

## Replayed fragments

A `solver_trusted` result can graduate to a kernel-checked class **only** if a
kernel tactic independently re-derives it: `concrete ... --report vcs --smt
--replay` emits a standalone Lean theorem restating the obligation and runs
`lake env lean` on it. If Lean closes it, the VC becomes `proved_by_lean_replay`
(engine `lean:omega`) and the solver is dropped from the claim — so it is no
longer subject to `solver-evidence` policy.

Today this path is **real but dormant**: the only fragment routed to SMT is the
bounded *nonlinear* product, which `omega` cannot close and `bv_decide` cannot
bound, and `nlinarith` lives in Mathlib (deliberately not a dependency). So the
in-toolchain replay does not close, and the VC honestly stays `solver_trusted`.
The artifact is still emitted so a reviewer — or a Mathlib-enabled build that
swaps `omega` → `nlinarith` — can graduate it.

## How a solver bug affects each claim class

This is the blast-radius analysis a reviewer needs. A Z3 bug (wrong `unsat`,
wrong model, miscompiled theory) can affect **only** claims that flowed through
Z3:

| evidence class | touches Z3? | affected by a solver bug? |
| --- | --- | --- |
| `proved_by_kernel_decision` (omega / bv_decide / constant) | no | **no** — kernel-checked in-toolchain |
| `proved_by_lean` | no | **no** — Lean kernel-checked |
| `proved_by_lean_replay` | no (the replay is kernel-checked) | **no** — the solver was dropped from the claim |
| `arithmetic_proved` | no | **no** |
| `runtime_checked` / `enforced` / `tested_by_oracle` / `assumed` / `trusted` | no | **no** |
| `solver_trusted` | **yes** (`unsat`) | **yes** — a wrong `unsat` would mark an unproved fact proved |
| `counterexample` | **yes** (`sat` + model) | a spurious `sat`/model would be a false alarm (a non-proof either way) |
| `unknown` / `timeout` / `solver_error` | maybe | non-proofs regardless |

Consequences:

- A solver bug **cannot** turn a kernel-checked or Lean claim false. Those never
  call Z3.
- The entire trust exposure of the external solver is the set of `solver_trusted`
  VCs. They are enumerated by name (with solver version and SMT-LIB hash) in the
  audit report and the `smt` JSON object, and a release can **forbid them
  outright** (`[policy] solver-evidence = "forbid"`, E0615) or require a named
  assumption (`"assumptions"` + `solver-assumption`).
- A wrong counterexample is only a false *alarm* — it never upgrades a fact to
  proved. The worst case is a spurious "this might overflow" report, which a
  reviewer can dismiss by inspecting the (recorded) model.

The design rule, enforced by `Report.foldSmtResults` and the gates: **the
external solver may only ever assign `solver_trusted` / `counterexample` /
`unknown` / `timeout` / `solver_error`, and only to a VC the kernel tiers left
`unproven`.** It can never relabel a kernel/Lean result.
