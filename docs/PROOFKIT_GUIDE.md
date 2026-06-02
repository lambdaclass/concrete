# Prove Your First Concrete Function — the Proof Kit Guide

This guide teaches the **reusable proof path** for Concrete: how to take a
function's *exact extracted body* and prove it refines an independent spec, with
the proof tied to the source through the spec-drift gate. It is grounded in the
HMAC-SHA256 flagship (`examples/hmac_sha256`), whose entire composition chain is
proved this way.

> The one-line thesis the whole workflow serves:
> **source extracts to this ProofCore body, this body refines the spec, and
> drift breaks the claim.**

---

## 1. What ProofKit is

`Concrete/ProofKit/*` is proof infrastructure **harvested from the HMAC proof**
and de-specialized so it is reusable by *any* refinement proof:

- **Not SHA-specific.** Every kit lemma is generic over an arbitrary function
  table `fns : FnTable` and over buffer sizes / byte functions. Nothing in
  `ProofKit/` assumes the SHA `shaFns` table. (SHA/HMAC-specific wrappers live
  in `Concrete.Sha256Refine` and *instantiate* the kit templates at `shaFns`.)
- **Built for exact-extraction, spec-drift-tied proofs.** The kit lemmas step
  through the `eval` interpreter over the *extracted* `PExpr` (not a hand
  model), so the theorem you build is about the real source body, and the
  registry's spec-drift gate enforces that link.

The lemmas live in the `Concrete.Proof` namespace (so you reference them
unqualified) but in the `Concrete.ProofKit.*` modules. To use the kit:

```lean
import Concrete.ProofKit      -- pulls in all six modules
open Concrete Concrete.Proof
```

The loop-induction keystone `eval_while_count` and fuel monotonicity
`eval_fuel_le` live in `Concrete.Proof`'s ladder section (already `fns`-generic)
and are part of the same surface.

---

## 2. Proof shapes

Pick the shape that matches your function's control flow. Each is exemplified by
a real, kernel-checked theorem in the flagship.

### Straight-line (no loops) — `ch`
`theorem ch_selects_high` / the round-function refinements (`rotr`, the σ
functions). The body is a tree of `binOp`/`call`/`cast`; the proof is
`simp only [<expr>, eval, evalBinOp, Env.bind, …]` to reduce the evaluator, then
**`bv_decide`** to discharge the fixed-width bit-vector identity. The kit's
`ProofKit.BitVec` round-trip lemmas (`ofInt_natCast_toNat`, `ofInt32_byte`, …)
bridge the casts the interpreter inserts so `bv_decide` sees a clean goal.

### Array loop — `block_to_words`
`theorem block_to_words_refines_spec` — a `for i in 0..N { w[i] = … }` loop.
The skeleton:
1. `ProofKit.Eval.eval_letIn` to step past the `let w = [0;N]` / `let i = 0`.
2. `ProofKit.Eval.arrayLit_replicate_eval` for the zero-initialized buffer.
3. `eval_while_count` to run the loop by **induction on the iteration count**
   (not by unfolding the body N times). You supply a state function
   `st : Nat → Env` (the env after `k` iterations) and prove one generic step.
4. `ProofKit.Array.set_in_counter_map` is the **frame lemma** that turns
   "wrote slot `k`" into "first `k+1` slots filled" — proved once, reused every
   iteration, so there is no per-index frame cost.
5. `ProofKit.Array.eval_arraySet_lemma` + `arrN_set` for the write itself; a
   `bv_decide` leaf for the per-element value.

### State loop / multi-store — `state_to_bytes`
`theorem state_to_bytes_refines_spec` — a loop whose body does **several stores
per iteration** (`out[i*4] = …; out[i*4+1] = …; …`). Same `eval_while_count`
skeleton; the per-iteration step threads several `eval_arraySet_lemma`/`arrN_set`
rewrites and a `set_in_counter_map`-style frame for each store. The index
arithmetic (`i*4 + k`) is closed by `omega`; commutative-`add` operand order is
auto-normalized by the spec-drift gate, so it need not match the source byte-for-byte.

### Composition (calls into proved sub-functions) — `sha256_compress`
`theorem sha256_compress_refines_spec` — a body that `call`s already-proved
functions (`block_to_words`, `sha256_schedule`, `sha256_round`). You don't
re-prove the callees; you reduce each `.call` to its refinement fact via a
call-reduction lemma (`ProofKit.Calls.unary_call` for `u32→u32` callees, or a
bespoke `*_call` wrapper that names the callee's `shaFns` entry) and compose. The
`FnTable` you evaluate through must map each name to its **extracted** body so
the composed evaluation is the real program.

### Full flagship — `hmac_sha256`
`theorem hmac_sha256_refines_spec` composes all of the above: a top-level
`if (k_len > 64)` branch (the extracted source duplicates the continuation into
both branches), several copy loops (`ProofKit.Loops.copy_loop`), the ipad/opad
two-buffer xor loop, and three nested `sha256_hash` calls — proved to equal
`Sha256Spec.hmac` for all inputs in the documented bounds.

---

## 3. Modules

| Module | What it gives you |
|---|---|
| `ProofKit.Eval` | `eval` stepping: `eval_letIn`, `eval_ite_true/false`, `evalElems_replicate_lit`, `arrayLit_replicate_eval` (generic zero/constant array). |
| `ProofKit.BitVec` | `Int`/`Nat`/`BitVec` round-trips + indexing so `bv_decide` closes after the interpreter's casts: `ofInt_natCast_toNat`, `ofInt_ofNat_toNat`, `ofInt32_byte(+_cast)`, `ofInt8_natCast_toNat`, `lookupIndex_eq`, `lookupIndex_range_map`. |
| `ProofKit.Array` | size-generic buffer model `arrN` + point-update `bufUpd`; `arrN_set`/`arrN_read`/`arrN_length`/`arrN_zfn`/`zfn`; the counter-loop frame lemma `set_in_counter_map`; single-step `eval_arraySet_lemma`. |
| `ProofKit.Loops` | the generic copy-into-buffer loop template `copy_loop` + `cpy_step` + `copyEnv`/`copyEnv_self`, and the `copyFn` spec family (`copyFn_zero/step/step0/map_append/zfn_map`). All over an arbitrary `fns`. |
| `ProofKit.Calls` | `unary_call` — a `u32→u32` `.call` reduces to `specf` of its argument, given the function is registered and its body refines `specf`. Over `fns`. |
| `ProofKit.Refinement` | `List` ↔ spec-function bridges: `getD_eq_getElem_mem`, `list_eq_rangeGetD`, `map_toNat_eq_arrN` (the `arrN` / `getD` views the specs are stated over). |

Plus, from `Concrete.Proof`: **`eval_while_count`** (bounded counter-loop
induction — the keystone every loop proof routes through) and **`eval_fuel_le`**
(evaluate everything at one large fuel).

`eval_while_count`'s shape (you supply `st`, one generic step, and the exit):

```lean
theorem eval_while_count (fns : FnTable)
    (cond : PExpr) (assigns : List (String × PExpr)) (cont : PExpr)
    (st : Nat → Env) (N base : Nat)
    (hstep : ∀ k, k < N →
        eval fns (st k) base cond = some (.bool true) ∧
        eval.evalAssigns fns (st k) base assigns = some (st (k + 1)))
    (hexit : eval fns (st N) base cond = some (.bool false)) :
    eval fns (st 0) (base + N + 1) (.while_ cond assigns cont)
      = eval fns (st N) base cont
```

---

## 4. How to start a proof

1. **Get the exact extracted PExpr.** Generate the kernel-checkable model of
   your function's body:
   ```sh
   concrete examples/<flagship>/src/main.con --report lean-stubs
   ```
   This emits a `def {name}Expr : PExpr := …` for every eligible function — the
   *exact* extraction, normalized. Copy the one(s) you're proving into your Lean
   file. Do not retype the body from the source by hand; use this output.

2. **Define the expr where the registry can see it, and an independent spec.**
   The expr must live in `Concrete.Proof` (the `specs` table references it). The
   spec is a separate, independently-written model (e.g. a `BitVec` function in
   `Concrete.Sha256Spec`) — *not* derived from the same source.

3. **Choose the proof shape** (§2) from the body's control flow and state the
   refinement theorem: `eval fns <env> <fuel> {name}Expr = some (<spec> …)`.

4. **Build the proof with the kit lemmas:**
   - step `let`s with `eval_letIn`, branches with `eval_ite_*`;
   - run loops with `eval_while_count` (+ `set_in_counter_map`, `arrN_set`,
     `eval_arraySet_lemma`) or, for plain copies, `copy_loop`;
   - reduce calls with `unary_call` / a `*_call` wrapper;
   - bridge casts with `ProofKit.BitVec` and close leaves with `bv_decide`/`omega`;
   - bridge the result list to the spec view with `ProofKit.Refinement`.

5. **Register and gate it.** Add a row to `Concrete.Proof.specs`
   (`(qualName, {name}Expr)`) and an entry to
   `examples/<flagship>/src/proof-registry.json` with `proof` (your theorem),
   `spec` (the expr), `coverage`, and `body_fingerprint` (from
   `--report fingerprints`). Then verify:
   ```sh
   concrete examples/<flagship>/src/main.con --report check-proofs   # kernel: theorem exists & type-checks
   concrete audit examples/<flagship>/src/main.con                    # spec-drift gate: spec == extracted, fingerprint fresh
   ```
   `check-proofs` should report your theorem verified; the audit should show
   `0` spec-drift and `0` stale. To convince yourself the tie is real, perturb a
   constant in the source body and re-run the audit — the proof should go
   `stale`.

> Note: registered proofs are `#check`'d from `import Concrete` (the umbrella),
> so a theorem in any module the umbrella imports is reachable. Spec exprs,
> however, must be in `Concrete.Proof` itself (the `specs` table's module).

---

## 5. What not to do

- **Don't prove a hand model unless it is tied to extraction.** A theorem about
  a hand-transcribed `PExpr` that doesn't equal the extracted body proves
  nothing about the source — it is "proved, with a footnote." (This is exactly
  the honesty gap HMAC bar #2 was held open to fix.) Always start from
  `--report lean-stubs` and let the spec-drift gate confirm the spec equals the
  extracted PExpr.
- **Don't treat oracle tests as proof.** A differential oracle (e.g. 600 cases
  vs a reference implementation) is strong *evidence* and covers the unbounded
  tail beyond a bounded theorem — but it is `tested_by_oracle`, not
  `proved_by_lean`. Report them as distinct classes; never let oracle coverage
  stand in for a missing theorem.
- **Don't hide assumptions.** State the bounds your theorem holds under
  (`k_len ≤ 128`, `len ≤ 375`, …) and what it does *not* cover — machine-level
  constant time, side channels, the LLVM lowering, cryptographic security.
  Record the trusted edges in `assumptions.toml`. A refinement proves
  *functional correctness against the spec*, nothing more.

---

## See also

- [`PROOF_LADDER.md`](PROOF_LADDER.md) — the rung-by-rung capability ladder the
  kit climbed (array lemmas → loop induction → composition).
- `examples/hmac_sha256/AUDIT.md` — the ten graduation bars and how each was met.
- `Concrete/ProofKit.lean` — the module surface, in code.
