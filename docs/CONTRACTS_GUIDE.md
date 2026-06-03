# Concrete Contracts: A Practical Guide

Status: usage guide for what works **today** (2026-06-03).

This is the hands-on companion to [CONTRACTS_AND_VCS.md](CONTRACTS_AND_VCS.md)
(the design/architecture). That document says where contracts are going; this
one shows what you can write right now, what the compiler does with it, and —
honestly — what still falls through to a hand-written Lean proof.

Everything below is exercised by real fixtures:
`examples/loop_invariant`, `examples/ghost_state`, and
`tests/programs/contract_callsite`. The output snippets are from
`concrete <file> --report contracts` (and `--report audit`).

## The frame: claim → obligation → evidence

A contract is a **claim**, not a guarantee. The compiler turns each claim into
one or more **obligations** with stable ids, then attaches **evidence**. Audit
shows the evidence class — never a single green badge. The classes you will see:

| status | meaning |
|---|---|
| `proved_by_lean` | a registered Lean theorem discharges it (kernel-checked) |
| `proved_by_kernel_decision` | the compiler closed it with a kernel decision procedure (`omega` / `bv_decide`) — no external SMT in the trusted base |
| `proved_at_callsite` | a precondition was constant-folded true at a call site |
| `assumed_at_entry` | a precondition the function assumes; callers are checked separately |
| `failed_at_callsite` | a precondition is provably violated at a call site |
| `planned` / `missing` | the obligation shape is generated but nothing discharges it yet |
| `n/a` | not applicable (e.g. an exit obligation on a function with no postcondition) |

Trust is tiered: kernel-checked first. `omega` and `bv_decide` are decision
procedures with checked certificates — using them adds **no** external solver to
the trusted base. See [PROOF_LADDER.md](PROOF_LADDER.md).

## `#[requires]` — preconditions

```rust
#[requires(0 <= n && n < 32)]
fn rotr(x: u32, n: u32) -> u32 { return (x >> n) | (x << (32 - n)); }
```

Inside the function the precondition is `assumed_at_entry`. The obligation is
pushed to the **callers**: every call site of `rotr` must establish it.

## `#[ensures]` — postconditions

```rust
#[ensures(result == rotr_spec(x, n))]
fn rotr(x: u32, n: u32) -> u32 { ... }
```

`result` names the return value; `rotr_spec` is a `spec fn` (an erased
mathematical specification, no body, no codegen). The postcondition becomes an
obligation discharged by a registered Lean theorem (`proved_by_lean`) or shown
`missing` until one exists.

## Call-site obligations

For each call into a `#[requires]`-bearing function, `--report contracts` shows
how that specific call discharges the precondition:

```
call rotr(x, 13)      requires 0 <= n && n < 32   →  proved_at_callsite
call rotr(x, n) [n=7] requires 0 <= n && n < 32   →  proved_by_kernel_decision (bv_decide)
call rotr(x, 40)      requires 0 <= n && n < 32   →  failed_at_callsite
call rotr(x, k)       requires 0 <= n && n < 32   →  unproven_at_callsite
```

- **constant** arguments fold and check immediately (`proved`/`failed`).
- **closed-after-let-substitution** arguments are handed to `bv_decide`
  (`proved_by_kernel_decision`).
- **non-constant** arguments with no backend stay `unproven` — never silently
  green. (Full `concrete prove`-driven discharge is a later slice.)

See `tests/programs/contract_callsite/main.con` for all five states.

## Kernel decision discharge: `omega` and `bv_decide`

Two procedures discharge obligations without a hand-written proof, both
kernel-checked:

- **`bv_decide`** — closed bitvector/bool goals (e.g. constant-time bound
  checks at call sites). Bit-blasted, LRAT-checked.
- **`omega`** — linear-integer goals over `Int` (the loop init/variant/exit
  arithmetic below). Presburger, certificate-checked.

Both report `proved_by_kernel_decision` with the engine named. Neither adds an
external SMT solver to the trusted base.

## Loop invariants: `#[invariant]` / `#[variant]`

```rust
let mut i: i32 = 0;
#[invariant(0 <= i && i <= 8)]
#[variant(8 - i)]
while i < 8 { i = i + 1; }
```

A loop with an invariant (and optional variant) generates **five** obligations.
From `examples/loop_invariant` (`--report contracts`):

```
O1 invariant_init          proved_by_kernel_decision (omega)
   ∀ ..., (0 ≤ 0 ∧ 0 ≤ 8)
O2 invariant_preservation
   arithmetic step:   proved_by_kernel_decision (omega)
   operational step:  proved_by_lean  (theorem: count_up_loop_preserves)
   ∀ (i acc : Int), (0 ≤ i ∧ i ≤ 8) → i < 8 → (0 ≤ (i+1) ∧ (i+1) ≤ 8)
O3 loop_exit_post_link     n/a (no #[ensures])   -- or proved, see below
O4 variant_nonnegative     proved_by_kernel_decision (omega)
   ∀ (i : Int), (0 ≤ i ∧ i ≤ 8) → i < 8 → 0 ≤ (8 - i)
O5 variant_decreases       proved_by_kernel_decision (omega)
   ∀ (i : Int), (0 ≤ i ∧ i ≤ 8) → i < 8 → (8 - (i+1)) < (8 - i)
```

What the compiler does for you:

- **O1, O4, O5** are pure linear arithmetic — `omega` closes them outright.
- **O2 (invariant_preservation)** is split honestly into two halves:
  - the **arithmetic step** (the invariant is inductive) — `omega` closes it;
  - the **operational step** (the extracted loop body really performs the
    substitution) — still needs Lean. When no theorem is registered, the report
    prints the exact theorem *shape* to write.

The discharge is **real**: each goal is shipped to `lake env lean` and
kernel-checked. A false invariant produces a goal `omega` rejects, and the
obligation stays `planned` — the compiler will not claim what it cannot prove.

## Exit-to-postcondition (O3)

`O3` bridges the loop's exit facts to the function's postcondition. The exit
hypothesis is `invariant ∧ ¬guard`; the goal is the `#[ensures]` with `result`
replaced by the loop-exit return expression. From `examples/loop_invariant`'s
`count_to_eight` (`#[ensures(result == 8)]` over `while i < 8 { i = i+1 }`):

```
O3 loop_exit_post_link     proved_by_kernel_decision (omega)
   ∀ (i : Int), (0 ≤ i ∧ i ≤ 8) ∧ ¬(i < 8) → i = 8
```

The postcondition follows **only** because of the exit condition: the invariant
alone gives `0 ≤ i ≤ 8`; only `¬(i < 8)` pins `i = 8`. O3 is generated only when
the loop is immediately followed by a single `return` (so the loop-exit state is
the returned state); otherwise it stays conservative.

## Ghost values: `ghost let`

A `ghost let` is a proof-only binding. It is **erased before codegen** — it never
reaches Core, the body fingerprint, or LLVM — and may be referenced only in
contracts, VCs, and audit. It lets an invariant name a value the runtime code
does not keep around.

```rust
fn count_up() -> i32 {
    ghost let bound: i32 = 8;            // erased; proof-only
    let mut acc: i32 = 0;
    #[invariant(0 <= i && i <= bound)]   // ghost `bound` names the limit
    #[variant(bound - i)]
    for (let mut i: i32 = 0; i < 8; i = i + 1) { acc = acc + i; }
    return acc;
}
```

Reading a ghost value from runtime code is rejected:

```
error[elab]: (E0420) ghost value 'bound' cannot be used in runtime code
  hint: ghost bindings are proof-only and erased before codegen; reference them
        only in contracts (#[ensures]/#[invariant]) or other ghost code
```

See `examples/ghost_state` (accepted) and its `catches/ghost_runtime_use.con`
(rejected). Ghost destructuring bindings are not supported yet.

## What still requires Lean (today)

Be clear-eyed about the boundary. The kernel decision procedures cover linear
integer and closed bitvector facts; everything else is a hand-written,
kernel-checked Lean proof, surfaced (not hidden) in the report:

- **The operational step of invariant_preservation** — that the extracted loop
  body realizes the state transition (`evalAssigns body = state'`). The report
  prints the theorem shape; you write it with the ProofKit (see
  [PROOFKIT_GUIDE.md](PROOFKIT_GUIDE.md)).
- **Refinement theorems** — "this extracted `PExpr` computes exactly this spec"
  (e.g. the HMAC/SHA-256 chain). These are the flagship-grade proofs.
- **Ghost *value* substitution into VCs** — today a ghost like `bound = 8` is a
  free variable in the generated VC, so obligations that need its concrete value
  (e.g. `0 ≤ bound`) show `planned`, not proved. Substituting ghost values is a
  later slice.
- **Non-linear arithmetic, aliasing/multi-cell array updates, and frame
  conditions** — see [PROOF_LADDER.md](PROOF_LADDER.md) and the frame-inference
  note in the roadmap.

The proof registry (`proof-registry.json`) is how a function is linked to its
hand-written Lean proof/spec today; it is transitional and will be replaced by
in-source proof attributes once `concrete prove` lands (ROADMAP Phase 3).

## Generating a proof scaffold: `concrete prove`

When a function needs a hand-written Lean proof, `concrete prove` generates a
read-only scaffold so you do not start from a blank file:

```sh
concrete prove path/to/file.con module.function        # print to stdout
concrete prove path/to/file.con module.function --out proofs/module_function.lean
```

A bare function name is accepted when it is unique; the qualified
`module.function` form is recommended. `--out` writes a stub file and **refuses
to overwrite** unless you pass `--force`. v1 is deliberately conservative: it
**writes nothing** unless `--out` is given, never edits the proof registry or
`Concrete/` sources, and never tries to prove anything for you.

It prints, for the chosen function:

1. suggested imports (`Concrete.ProofKit`, `Concrete.Proof`);
2. the body fingerprint;
3. the extracted ProofCore body;
4. the contract/VC list with current discharge — which obligations omega or
   `bv_decide` already close, which are linked to a Lean theorem, and which are
   still planned/missing;
5. a theorem skeleton to fill in;
6. ProofKit hints from detected features (loops → `eval_while_count`, arrays →
   `lookupIndex_set_self/ne`/`length_set`, bitvectors → `bv_decide`, calls →
   FnTable/call wrappers);
7. the **next** obligation to discharge, with the reason it still needs Lean.

For an excluded function it prints why it is not in the provable subset instead
of a stub.

## Reading the evidence

```sh
concrete <file>.con --report contracts   # source contracts + call sites + loops
concrete <file>.con --report audit       # everything, contracts included
concrete <file>.con --report eligibility # which functions are in the provable subset
```

Audit is the truth surface: it shows the evidence class for every obligation and
never collapses `assumed`, `proved_by_kernel_decision`, and `proved_by_lean` into
one badge.
