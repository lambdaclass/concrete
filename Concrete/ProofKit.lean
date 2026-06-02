import Concrete.ProofKit.Eval
import Concrete.ProofKit.BitVec
import Concrete.ProofKit.Array
import Concrete.ProofKit.Refinement

/-!
# Concrete Proof Kit (v1)

Reusable, domain-agnostic proof infrastructure harvested from the HMAC-SHA256
flagship (`Concrete.Sha256Refine`) so refinement proofs of future Concrete
functions don't re-derive the same machinery. All lemmas live in the
`Concrete.Proof` namespace.

Modules:
- `ProofKit.Eval`       — `eval` semantics: letIn / ifThenElse / arrayLit stepping.
- `ProofKit.BitVec`     — Int/Nat/BitVec round-trips + indexing for `bv_decide`.
- `ProofKit.Array`      — size-generic buffer model `arrN`, frame lemma, arraySet eval.
- `ProofKit.Refinement` — List ↔ spec-function bridges (`arrN` / `getD` views).

The loop-induction keystone `eval_while_count` and fuel monotonicity
`eval_fuel_le` currently live in `Concrete.Proof`'s ladder section and are part
of the same kit surface.

PENDING (v1.1): a `ProofKit.Loops` (the generic copy-into-buffer loop:
`copy_loop`/`cpy_step`/`copyEnv`) and `ProofKit.Calls` (`unary_call`) module.
These currently hardcode the SHA `shaFns` table; extracting them requires a
`shaFns → (fns : FnTable)` generalization plus call-site updates, done as a
focused follow-up so each step stays green.
-/
