import Concrete.ProofKit.Eval
import Concrete.ProofKit.BitVec
import Concrete.ProofKit.Arith
import Concrete.ProofKit.Array
import Concrete.ProofKit.Loops
import Concrete.ProofKit.Calls
import Concrete.ProofKit.Refinement

/-!
# Concrete Proof Kit (v1.1)

Reusable, domain-agnostic proof infrastructure harvested from the HMAC-SHA256
flagship (`Concrete.Sha256Refine`) so refinement proofs of future Concrete
functions don't re-derive the same machinery. All lemmas live in the
`Concrete.Proof` namespace.

Modules:
- `ProofKit.Eval`       — `eval` semantics: letIn / ifThenElse / arrayLit stepping.
- `ProofKit.BitVec`     — Int/Nat/BitVec round-trips + indexing for `bv_decide`.
- `ProofKit.Arith`      — arithmetic bridges: width conversions, byte masking,
                          sign-bit facts, and the signed-division bridge
                          (`sdiv` over unsigned-range operands = `Nat` division).
- `ProofKit.Array`      — size-generic buffer model `arrN`, frame lemma, arraySet eval.
- `ProofKit.Loops`      — generic copy-into-buffer loop template `copy_loop`/`cpy_step`
                          + `copyFn` spec, over an arbitrary `fns : FnTable`.
- `ProofKit.Calls`      — `unary_call` (a `u32→u32` call-site reduction), over `fns`.
- `ProofKit.Refinement` — List ↔ spec-function bridges (`arrN` / `getD` views).

Every kit lemma is now `fns`-generic — none assumes the SHA `shaFns` table — so
the kit teaches the general refinement path. The loop-induction keystone
`eval_while_count` and fuel monotonicity `eval_fuel_le` live in `Concrete.Proof`'s
ladder section (already `fns`-generic) and are part of the same surface.

SHA/HMAC-specific wrappers (the ipad/opad two-buffer xor loop, the per-function
`*_call` reductions, the schedule/compress/hash loop lemmas) remain in
`Concrete.Sha256Refine` — they instantiate these templates at `shaFns`.
-/
