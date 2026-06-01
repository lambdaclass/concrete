# The Proof Ladder — discharge infrastructure and evidence tiers

Status: **partly shipped, partly design.** This document is explicit about
which is which (see the table at the end). It is the discharge-side companion
to [CONTRACTS_AND_VCS.md](CONTRACTS_AND_VCS.md), which covers the claim →
obligation side.

The pipeline is:

```
claim  ->  obligation  ->  DISCHARGE  ->  audit class
                            ^^^^^^^^^
                         this document
```

`CONTRACTS_AND_VCS.md` owns the first two arrows (where claims come from and
how they become obligations). This document owns the third: *how an obligation
is discharged, and what trust class that discharge earns.*

## The reusable proof layer (SHIPPED)

These are kernel-verified lemmas in `Concrete/Proof.lean`. Their point is to
turn loop/array proofs from heroic one-off `simp` scripts into a reusable
layer, so the next proof is "large but systematic," not speculative.

- **Array update lemmas** — `lookupIndex_set_self`, `lookupIndex_set_ne`,
  `length_set`. Read-after-write, framing, and length for the `arraySet`/
  `lookupIndex` model. The backbone for byte/word packing, schedule expansion,
  and state updates.
- **`while_` unfolding** — `eval_while_false`, `eval_while_true`. Stable
  rewrite rules for the two loop transitions, so loop proofs induct on the
  iteration count instead of unfolding the body N times.
- **Fuel monotonicity (the keystone)** — `eval_fuel_succ` / `eval_fuel_le`.
  `eval` consumes fuel and its termination is lexicographic on `(fuel, expr)`,
  so relating two fuel levels needs a theorem, not a fuel induction. Proved
  over `eval` and all six mutually recursive helpers via `eval.induct`. Once it
  exists, loop proofs evaluate everything at a single large fuel and drop all
  per-iteration fuel bookkeeping. (We deliberately did **not** refactor `eval`
  to sized types to avoid this; see `SIZED_EVALUATOR_INVESTIGATION.md`. The
  additive lemma was the lower-risk choice and it neutralized the recurring
  cost.)
- **`evalAssigns_fuel_le`** — loop-body assignments are fuel-monotone (a short
  list induction over `eval_fuel_le`).
- **Bounded counter-loop induction** — `eval_while_count`. For a `while_` whose
  env at the start of iteration `k` is `st k`: given the guard holds and the
  body steps `st k` to `st (k+1)` for every `k < N`, and the guard fails at
  `st N`, the loop evaluates to `cont` in the final env `st N`. This is the
  lemma SHA-256's packing / schedule / compression loops are proved with.

Critically, a source-level `#[invariant(...)]` (when contracts land — see
below) compiles to **exactly the hypotheses of `eval_while_count`**. The
discharge engine for loop contracts already exists; the syntax that feeds it
does not yet.

## The discharge ladder (cheapest trust first)

An obligation is discharged by one of these, and the audit records *which*.
The ordering is by trust cost: prefer the tiers that grow nothing in the
trusted computing base.

| Tier | For | Audit class | TCB impact |
|---|---|---|---|
| **Lean (kernel)** | induction, refinement (`eval_while_count`, `f ⊑ spec`) | `proved_by_lean` | none (kernel checks the proof) |
| **`bv_decide` (kernel-checked)** | BitVec / bounded-arithmetic leaves — `ch`/`maj`/`rotr`/σ, u32 wrap, shift-pack, index bounds | `proved_by_kernel_decision` | none (solver proposes, kernel checks the LRAT certificate) |
| **external SMT** (Z3/cvc5) | nonlinear arithmetic, goals `bv_decide` cannot — **only when needed** | `solver_trusted` | **grows** (solver in TCB unless a certificate is replayed) |
| **runtime check** (gradual mode) | unproved claims asserted at runtime | `runtime_checked` | none for soundness; shifts the check to runtime |
| **`assume`** | explicitly accepted gaps | `assumed` | the assumption itself |

Two tiers are **non-negotiably distinct**: Lean and `bv_decide` are
*kernel-checked* (no TCB growth); external SMT is *solver-trusted* (the solver
enters the TCB). The whole project is about not hiding trust, so these are
different audit classes and must never collapse into one "green."

### `bv_decide` first (validated)

Lean 4.28's `bv_decide` (`import Std.Tactic.BVDecide`, already in the
toolchain — no external dependency) bit-blasts a BitVec/boolean goal to SAT and
replays a certificate the kernel checks. It targets exactly the obligations
this domain produces. Confirmed against the HMAC helper facts: the universal
`Ch(0xFFFFFFFF, y, z) = y`, `Maj` commutativity, `u32` wrapping-add identities,
and shift/pack facts all close with zero added trust. So the arithmetic *leaves*
of crypto proofs discharge kernel-checked, and an external solver is a later,
louder, optional tier reached for only when `bv_decide` cannot.

### Division of labour

- **`bv_decide`** does the *leaves*: BitVec identities, bounds, per-iteration
  arithmetic side conditions, the `decreases` variant.
- **Lean** does the *structure*: loop induction (`eval_while_count`),
  cross-function composition, and refinement against the spec layer.
- **External SMT** does what neither can cheaply, and pays for it in trust
  class.

## `assume` is a controlled trapdoor

An inline `assume P` (planned) can make any downstream `proved` claim vacuous
if `P` is false. Rules, not one rule:

1. It **taints** its obligation's class to `assumed` — never `proved`. The
   taint propagates; you cannot launder an assumption into a proof.
2. It flows into the **same audit ledger** as `assumptions.toml`
   (`[claims.*]`), not a parallel one.
3. It is **gate-forbiddable**: release / showcase mode can require zero
   undischarged assumptions.

This is the one feature whose default is suspicion.

## Build order — the proof teaches the syntax

Do **not** design contract syntax and VC shapes against imagined obligations.
Prove a real HMAC obligation first; let it dictate the obligation format the
contract system must generate.

1. **Lean spec layer** — pure/total/erased SHA-256/HMAC spec functions
   (the refinement targets *and* the vocabulary future contracts point at).
2. **`bv_decide` automation tier** — wire it into the proof workflow; use it on
   the HMAC helper lemmas. Low-risk, useful immediately, zero TCB cost.
   *(Shipped: `Concrete/Sha256Refine.lean` uses `bv_decide` to close the
   word-level core of the first refinements — the `proved_by_kernel_decision`
   tier's first committed use.)*
3. **Refinement theorem pattern** — establish the shape "extracted `PExpr`
   refines the pure Lean spec," not "evaluates to these bytes."
   *(Shipped: `ch_refines` / `maj_refines` prove the extracted Boolean round
   functions equal `Sha256Spec.ch` / `maj` for ALL inputs, via two reusable
   `BitVec.ofInt ∘ toNat` round-trip bridging lemmas + `bv_decide`. Also shipped:
   `rotr_refines` (with the `32−n` shift amount) and all four sigma refinements
   (`big_sigma0/1`, `small_sigma0/1`) — plus the `rotr_call` call/bind
   scaffolding (`shaFns` function table + `eval.evalArgs`/`bindArgs` reduction)
   that lets extracted `rotr(...)` calls inside the sigmas reduce to
   `rotr_refines`. Every SHA-256 round *function* now refines its spec.)*
4. **`block_to_words_refines_spec`** — the first real proof using the pattern +
   `eval_while_count`. This validates what kind of obligation contracts must
   generate.
   *(Shipped: `block_to_words_refines_spec` proves the extracted 16-iteration
   big-endian word-packing loop refines `Sha256Spec.blockToWords` for ALL 64
   input bytes — `eval_while_count` for the loop, a `List.set`/`getElem`
   invariant (`wList_set`) for the per-iteration array write, `bv_decide` for
   the per-word packing fact, and the spec layer as target. The project has
   moved from straight-line helper refinement to **looping crypto-state
   refinement**. The remaining steps in the SHA-256 pipeline — `rotr`/sigmas
   (the `32−n` shift amounts and `rotr` calls), the 48-round schedule, the
   64-round compression, and the `hash`/`hmac` composition — reuse this same
   machinery.)*
5. **Attribute contracts + `ghost`** — `#[requires]`, `#[ensures]`,
   `#[invariant]`, `#[decreases]` (attribute form, LL(1)-safe), now designed
   against an obligation shape we have actually discharged.
6. **VC generator** — contracts lower to the obligations from steps 3–4.
7. **Discharge routing + classification** — route each VC to Lean / `bv_decide`
   / SMT / runtime / assumed; surface the class in `concrete audit`.
8. **Gradual mode, then external SMT** — runtime checks for unproved
   obligations; external solver as the `solver_trusted` tier.

Reason for the ordering: if contract syntax lands before
`block_to_words_refines_spec`, we risk freezing syntax and VC shapes around a
proof we have not done. HMAC should teach the contract system.

## What this is NOT

No dependent types or tactics in `.con` source, no row effects, no sized-types
evaluator refactor (settled), no proof-directed optimization yet. Contracts and
ghost values erase; the compiled binary is unchanged.

## Shipped vs. planned

| Piece | Status |
|---|---|
| Array / `while_` / fuel-monotonicity / counter-loop lemmas | **shipped** (`Concrete/Proof.lean`) |
| `bv_decide` kernel-checked tier, used in a committed proof | **shipped** (`Concrete/Sha256Refine.lean`; `proved_by_kernel_decision`) |
| Spec layer (`Concrete/Sha256Spec.lean`) | **shipped** |
| Refinement pattern: extracted `PExpr` refines spec, ∀ inputs | **shipped** for the Boolean round functions (`ch_refines` / `maj_refines`) |
| First **loop** refinement: `block_to_words_refines_spec` (∀ 64 bytes, via `eval_while_count`) | **shipped** (`Concrete/Sha256Refine.lean`) |
| `rotr` + four sigma refinements (incl. `32−n` shift) + `rotr_call` call/bind scaffolding | **shipped** (`Concrete/Sha256Refine.lean`) |
| `sha256_round` refinement (Σ/Ch/Maj + wrapping-`addw` chain, state reads, helper calls) | **shipped** (`round_refines`) |
| `sha256_schedule` refinement — two loops incl. the **self-referential** 48-round expansion, via `expandSchedule_recurrence` + `eval_while_count` | **shipped** (`sha256_schedule_refines_spec`) |
| `sha256_compress` **body** refinement — `compressFold` recurrence + 64-round `eval_while_count` (state invariant `s = compressFold m`, `round_call` lifts `round_refines` to the opaque 8-word state) + Davies-Meyer feed-forward | **shipped** (`compress_body_refines_spec`) |
| Generic counter-loop frame lemma (`set_in_counter_map`) dedup'ing per-construct frame proofs | **shipped** |
| Evidence classes `proved` / `enforced` / `reported` / `assumed` + `concrete audit` | **shipped** |
| Full `sha256_compress(state, block)` setup-call wiring (`block_to_words`/`schedule`/`k`), then `hash` / `hmac` | **next** (ROADMAP Phase 8 / bar #2; composes with #21) |
| `#[requires/ensures/invariant/decreases]`, `ghost`, `assume`, `contract` | **design** (ROADMAP Phase 4) |
| VC generator; `proved_by_kernel_decision` / `proved_by_smt` / `solver_trusted` / `runtime_checked` classes; external SMT; gradual mode | **design** (ROADMAP Phase 5) |

## See also

- [CONTRACTS_AND_VCS.md](CONTRACTS_AND_VCS.md) — the claim → obligation side.
- [PROOF_OBLIGATIONS_REGISTER.md](PROOF_OBLIGATIONS_REGISTER.md) — per-construct
  ProvableV1 extensions (R-01…R-28) and Phase-12 obligations.
- [PROOF_STORY_MATRIX.md](PROOF_STORY_MATRIX.md) — per-construct proved/trusted
  status.
- [SIZED_EVALUATOR_INVESTIGATION.md](SIZED_EVALUATOR_INVESTIGATION.md) — why the
  evaluator is not refactored to sized types.
- [TRUSTED_COMPUTING_BASE.md](TRUSTED_COMPUTING_BASE.md) — the trust layers.
