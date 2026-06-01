# Sized / Indexed Evaluator Investigation

Status: research note, not a committed design.

Roadmap reference: Phase 16 research-gated extensions.

## Why this exists

Concrete's ProofCore evaluator currently uses an explicit fuel parameter:

```lean
eval : FnTable -> Env -> Nat -> PExpr -> Option PVal
```

Fuel makes the evaluator structurally acceptable to Lean and prevents
nontermination. The cost is proof bookkeeping. Loop and composition proofs must
carry facts such as:

- more fuel preserves a successful result;
- loop bodies can run with the fuel left after previous iterations;
- generated proof stubs need stable `fuel + N` margins.

The current proof layer has already paid down the immediate cost with reusable
lemmas:

- `eval_fuel_succ`
- `eval_fuel_le`
- `evalAssigns_fuel_le`
- `eval_while_count`
- loop unfolding lemmas

This means fuel is no longer the active blocker for HMAC or the next flagship
proofs. The question for this note is longer-term: would a sized or indexed
evaluator remove recurring proof noise once larger proof automation and
generated VCs arrive?

## The idea

Replace or supplement the fuel-indexed evaluator with an evaluator whose
termination budget is represented by a type/index with a built-in decreasing
discipline:

```lean
eval : FnTable -> Env -> Size -> PExpr -> Option PVal
```

or an equivalent structurally indexed formulation.

The goal is not to change Concrete source syntax. This would be an internal
ProofCore v2 design, affecting Lean-side semantics and generated proof
obligations.

## Potential advantages

The main advantage is removing proof obligations that are about the evaluator
rather than the user program. A proof should spend its effort on statements
like:

```text
after k SHA-256 rounds, the state refines the mathematical spec state
```

not on:

```text
there is enough fuel left;
increasing fuel preserves success;
this sub-expression can run at fuel + N.
```

If the evaluator index makes monotonicity or termination behavior definitional,
future generated proofs may become smaller and less brittle.

Potential wins:

- less fuel bookkeeping in generated proof stubs;
- simpler loop and composition proofs;
- cleaner compiler-soundness bridge evaluator/source-semantics preservation
  statements;
- fewer ad hoc `fuel + N` constants in theorem statements;
- better proof ergonomics for users who should not think about evaluator fuel.

## Risks

This is not a free cleanup.

- It touches the core proof semantics.
- Existing theorems may need migration.
- Existing generated stubs and proof-shape docs assume `fuel : Nat`.
- A bad redesign could make proofs harder, not easier.
- It does not directly improve Concrete source, codegen, or audit output.

Because `eval_fuel_le` and `eval_while_count` are already in place, migrating
now would be churn unless future proofs show that fuel remains a dominant
source of complexity.

## Investigation trigger

Do not start this migration just because it is theoretically cleaner.

Investigate a sized/indexed evaluator only if at least two substantial proof
efforts after HMAC show that fuel bookkeeping remains one of the main proof
costs even with the current reusable lemmas.

Good triggers:

- generated loop proofs repeatedly need hand-written fuel plumbing;
- contract/VC-generated proof stubs become dominated by fuel constants;
- compiler-soundness bridge source/evaluator preservation proofs repeatedly
  duplicate fuel-monotonicity arguments;
- proof authors report that fuel, not program invariants, is the main obstacle.

Bad triggers:

- "sized types are cleaner" in the abstract;
- desire to rewrite fresh proof infrastructure immediately after it landed;
- replacing working HMAC proof work with evaluator architecture churn.

## Non-goals

- No Concrete source syntax for sized types.
- No dependent-type surface language.
- No rewrite of the evaluator before HMAC bar #2 work has used the current
  loop-proof layer.
- No promise that sized/indexed evaluation will replace fuel.

## Decision rule

Track this as ProofCore v2 research. Keep using the current fuel-indexed
evaluator until real proof work shows that the fuel layer is still a repeated
tax after the reusable lemmas are applied.
