# Proof Theorem Shapes

Status: active reference (Phase 2, items 3 and 5)

This document defines the canonical theorem shapes, naming rules, property forms, readability standards, and explicit non-goals for Concrete-to-Lean proofs. Generated stubs (`--report lean-stubs`) and hand-written proofs in `Concrete/Proof.lean` must conform to these shapes.

## Theorem categories

Three categories, in order of complexity:

### 1. Concrete test cases

Fixed-input evaluation verified by kernel reduction.

```lean
theorem check_nonce_valid_5 : eval_check_nonce 5 100 = some (.int 1) := by native_decide
```

**Shape**: `theorem <fn>_<case> : eval_<fn> <args> = some (.int <expected>) := by native_decide`

**Purpose**: Regression anchors. Cheap to write, catch extraction regressions instantly.

**Naming**: `<fn>_<descriptive_case>` — e.g. `check_nonce_valid_5`, `clamp_value_below`, `compute_checksum_zero`.

### 2. Universal boundary theorems

Universally quantified over inputs, with preconditions.

```lean
theorem check_nonce_rejects_negative (nonce maxNonce : Int) (h : nonce < 0) (fuel : Nat) :
    eval generatedFns ((Env.empty.bind "nonce" (.int nonce)).bind "max_nonce" (.int maxNonce))
      (fuel + 3) check_nonceExpr
    = some (.int 2) := by ...
```

**Shape**: `theorem <fn>_<property> (<params>) (<preconditions>) (fuel : Nat) : eval ... = some (.int <result>) := by ...`

**Purpose**: Prove specific code paths — rejection, acceptance, boundary behavior.

**Naming**: `<fn>_rejects_<condition>`, `<fn>_accepts_<condition>`, `<fn>_returns_<what>_when_<condition>`.

### 3. Full contract theorems

Complete input-output specification in one theorem.

```lean
theorem check_nonce_correct (nonce maxNonce : Int) (fuel : Nat) :
    eval generatedFns ((Env.empty.bind "nonce" (.int nonce)).bind "max_nonce" (.int maxNonce))
      (fuel + 3) check_nonceExpr
    = some (.int (if nonce < 0 then 2
                  else if nonce > maxNonce then 1
                  else 0)) := by ...
```

**Shape**: `theorem <fn>_correct (<params>) (fuel : Nat) : eval ... = some (.int <spec_expr>) := by ...`

**Purpose**: The theorem attached in the proof registry. States the complete behavior.

**Naming**: `<fn>_correct`. This is the name referenced by `proof-registry.json`.

## Naming rules

| Element | Pattern | Example |
|---------|---------|---------|
| PExpr definition | `<fn>Expr` | `check_nonceExpr` |
| PFnDef | `<fn>Fn` | `check_nonceFn` |
| Function table | `generatedFns` (or domain-specific: `cryptoFns`) | |
| Eval helper | `eval_<fn>` or `eval<CamelFn>` | `eval_check_nonce` |
| Concrete test | `<fn>_<case>` | `check_nonce_valid_5` |
| Boundary theorem | `<fn>_rejects_<X>` / `<fn>_accepts_<X>` | `check_nonce_rejects_negative` |
| Full contract | `<fn>_correct` | `check_nonce_correct` |

## Property forms

### Allowed (current scope)

- **Total input-output**: `eval fns env fuel expr = some (.int <spec>)` — the function returns a specific value for all inputs satisfying preconditions.
- **Branch coverage**: separate theorems for each branch outcome (reject/accept), combined into a full contract.
- **Compositional**: theorem about a caller that chains through proved callees (e.g. `validate_header` calls `check_nonce`).

### Explicit non-goals

- **Loop invariants**: The proof fragment does not support loops. No `while` or iteration proofs.
- **Effect proofs**: Functions with capabilities are ineligible. No proofs about I/O, allocation, or mutable state.
- **Refinement types**: No dependent typing or subtype constraints. Preconditions are explicit hypotheses.
- **Temporal/liveness**: No reasoning about termination beyond fuel, no progress guarantees.
- **Floating-point**: `PVal` has `.int` and `.bool` only. No float proofs.
- **String/char**: Not in the proof fragment.
- **Struct/enum field access**: Blocked in extraction. No proofs until extraction supports it.

## Fuel convention

All universally quantified theorems take `(fuel : Nat)` as the last parameter before the colon. The statement uses `fuel + N` where N is the maximum nesting depth of the expression. This makes theorems fuel-independent: they hold for any fuel value.

## Proof tactics

| Complexity | Tactic |
|-----------|--------|
| Concrete test cases | `native_decide` |
| Simple arithmetic | `simp [fooExpr, eval, Env.bind, evalBinOp]` |
| Conditional branches | `by_cases` on branch condition, then `simp` with `decide_eq_true`/`decide_eq_false` |
| Compositional | `simp` with callee function table entries and `bindArgs` |

## Readability standards for generated stubs

The `--report lean-stubs` output must satisfy:

1. **PExpr constructors match source structure**: `.ifThenElse` for if-else, `.letIn` for let bindings, `.call` for function calls. The generated PExpr should be recognizable as the source function without referring back to the extraction report.

2. **Parameter names preserved**: Lean parameters use the same names as the Concrete source (`nonce`, `max_nonce`, not `arg0`, `arg1`).

3. **Theorem signature readable**: The `eval generatedFns (env) (fuel + N) fooExpr = sorry` shape is immediately fillable — replace `sorry` on the RHS with the spec expression, replace `sorry` in the proof with tactics.

4. **No normalization surprises**: The generated PExpr is the normalized form (commutative canonicalization, dead-let elimination applied). If normalization changes the apparent structure (e.g. `a + b` becomes `b + a`), the docstring shows the readable form via `renderPExpr`.

5. **Excluded functions absent**: Functions that are ineligible (capabilities) or blocked (unsupported constructs) do not appear in stubs. No `sorry`-forever theorems.

## Pressure set theorem targets

| Function | Category | Contract shape |
|----------|----------|---------------|
| `check_nonce` | conditional validator | Returns 1 (negative), 2 (over max), or 0 (valid) based on range |
| `validate_header` | compositional | Calls `check_nonce`, then checks version range 1-3 |
| `compute_checksum` | arithmetic | Returns `key * message + salt + 1` |
| `clamp_value` | conditional | Returns `lo` if below, `hi` if above, `x` if in range |

These four functions are the immediate proof targets. `classify_range` is blocked (struct field access) and `format_result` is ineligible (Console capability).
