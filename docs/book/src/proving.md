# How to Prove a Concrete Function

This guide walks you through proving a Concrete function with Lean 4, from start to finish. You do not need to know the compiler internals — only the commands and the workflow.

By the end you will have: chosen a function, checked it is provable, generated a Lean theorem stub, written and attached a proof, verified it with the Lean kernel, and understood what exactly the proof covers and what it does not.

## Prerequisites

- A working Concrete installation (see [Installation](./installation.md))
- Lean 4 and Lake installed (the proof checker)
- A Concrete source file with at least one pure function

## The Example

We will prove `clamp_value` from the proof pressure set (`examples/proof_pressure/src/main.con`):

```concrete
fn clamp_value(x: Int, lo: Int, hi: Int) -> Int {
    if x < lo {
        return lo;
    } else {
        if x > hi {
            return hi;
        } else {
            return x;
        }
    }
}
```

This function clamps `x` to the range `[lo, hi]`. It is pure (no I/O, no allocation, no mutation), uses only supported constructs (arithmetic and if-else), and has no proof registered yet — the compiler reports it as **missing**.

## Step 1: Check Eligibility

First, confirm the function can be proved:

```bash
concrete examples/proof_pressure/src/main.con --report extraction
```

Look for `clamp_value` in the output:

```
  main.clamp_value(x, lo, hi)
    status: extracted
    ProofCore: if (x < lo) then lo else if (x > hi) then hi else x
    fingerprint: [(if (binop Concrete.BinOp.lt (var x) (var lo)) ...)]
    loc: examples/proof_pressure/src/main.con:90
```

**Key things to check:**

- **status: extracted** — the function is eligible and its body was successfully extracted to the proof model. If you see `eligible (extraction failed)`, the function uses an unsupported construct. If you see `excluded`, it fails an eligibility gate (has capabilities, is trusted, etc.).
- **ProofCore** — the human-readable version of what you are proving about. This is the pure functional model of the function body.
- **fingerprint** — the structural hash you will need for the registry entry.

## Step 2: See the Full Picture

Check the overall proof status to understand where `clamp_value` fits:

```bash
concrete examples/proof_pressure/src/main.con --report proof-status
```

```
=== Proof Status Report ===

-- proved -------- main.con:30
  ✓ `main.check_nonce` — proof matches current body.

-- proved -------- main.con:47
  ✓ `main.validate_header` — proof matches current body.

-- proof stale --- main.con:70
  Function `main.compute_checksum` has a registered proof, but the body changed.

-- not eligible -- main.con:78
  `main.format_result` cannot be proved: fails predictable profile (has capabilities: Console).

-- no proof ------ main.con:90
  `main.clamp_value` passes the predictable profile but has no registered proof.

-- blocked ------- main.con:109
  `main.classify_range` is eligible but uses unsupported constructs — extraction failed.

-- not eligible -- main.con:123
  `main.main` cannot be proved: fails predictable profile (has capabilities: Console, is entry point).

Totals: 7 functions — 2 proved, 1 stale, 1 unproved, 1 blocked, 2 ineligible, 0 trusted
```

The five possible states are:

| State | What it means |
|-------|---------------|
| **proved** | Has a proof, fingerprint matches, Lean theorem exists |
| **stale** | Has a proof, but the function body changed since the proof was written |
| **no proof** (missing) | Eligible and extractable, but nobody wrote a proof yet |
| **blocked** | Eligible, but uses a construct the extraction pipeline does not support |
| **not eligible** (ineligible) | Fails an eligibility gate — has capabilities, is trusted, or is an entry point |

`clamp_value` shows **no proof** — it is ready to prove.

## Step 3: Generate Lean Stubs

Generate the theorem scaffolding:

```bash
concrete examples/proof_pressure/src/main.con --report lean-stubs
```

The output is a complete Lean 4 file. Find the section for `clamp_value`:

```lean
/-- Extracted from `main.clamp_value`. -/
def clamp_valueExpr : PExpr :=
    .ifThenElse
      (.binOp .lt (.var "x") (.var "lo"))
      (.var "lo")
      (.ifThenElse
        (.binOp .gt (.var "x") (.var "hi"))
        (.var "hi")
        (.var "x"))

def clamp_valueFn : PFnDef :=
  { name := "clamp_value", params := ["x", "lo", "hi"], body := clamp_valueExpr }
```

And at the bottom, the theorem stub:

```lean
/-- TODO: State the correctness property for `main.clamp_value`.
    Current ProofCore: if (x < lo) then lo else if (x > hi) then hi else x -/
theorem clamp_value_correct (x : Int) (lo : Int) (hi : Int) (fuel : Nat) :
    eval generatedFns (((Env.empty.bind "x" (.int x)).bind "lo" (.int lo)).bind "hi" (.int hi))
      (fuel + 1) clamp_valueExpr
    = sorry := by
  sorry
```

This tells you:
- **Parameters**: `x`, `lo`, `hi` — same names as the Concrete source
- **Fuel**: `fuel + 1` — the expression nesting depth. You do not need to change this.
- **Two `sorry`s to fill**: the right-hand side (what the function should return) and the proof (why it returns that)

## Step 4: Write the Proof

Replace the two `sorry`s. The first `sorry` is the specification — what the function computes. The second is the proof.

For `clamp_value`, the spec is: return `lo` if `x < lo`, `hi` if `x > hi`, otherwise `x`.

```lean
theorem clamp_value_correct (x : Int) (lo : Int) (hi : Int) (fuel : Nat) :
    eval generatedFns (((Env.empty.bind "x" (.int x)).bind "lo" (.int lo)).bind "hi" (.int hi))
      (fuel + 1) clamp_valueExpr
    = some (.int (if x < lo then lo
                  else if x > hi then hi
                  else x)) := by
  simp [clamp_valueExpr, generatedFns, eval, Env.bind, evalBinOp]
  by_cases h1 : x < lo
  · simp [h1, decide_eq_true h1]
  · by_cases h2 : x > hi
    · simp [h1, h2, decide_eq_false h1, decide_eq_true h2]
    · simp [h1, h2, decide_eq_false h1, decide_eq_false h2]
```

### Proof strategy

The proof follows the function's branch structure:

1. **`simp`** unfolds the definitions and simplifies
2. **`by_cases`** splits on each branch condition
3. Each branch simplifies to the expected return value

If you are not sure the proof is correct yet, start with a concrete test case instead:

```lean
-- Quick sanity check: clamp_value(5, 0, 10) should return 5
theorem clamp_value_in_range : eval_clamp_value 5 0 10 = some (.int 5) := by native_decide
```

`native_decide` is a Lean tactic that evaluates the expression and checks the result. It works for fixed inputs and is the fastest way to validate that extraction is correct.

## Step 5: Attach the Proof

Add a registry entry to `proof-registry.json` in the same directory as the source file:

```json
{
  "function": "main.clamp_value",
  "body_fingerprint": "<paste the fingerprint from the extraction report>",
  "proof": "Concrete.Proof.clamp_value_correct",
  "spec": "clamp_value_behavior"
}
```

Get the fingerprint by running:

```bash
concrete examples/proof_pressure/src/main.con --report extraction 2>&1 \
  | grep -A6 clamp_value | grep fingerprint
```

The `proof` field is the fully qualified Lean theorem name. The `spec` field is a human-readable description of what property is proved.

### Registry format

The full registry file looks like this:

```json
{
  "version": 1,
  "proofs": [
    {
      "function": "main.clamp_value",
      "body_fingerprint": "[(if (binop ...)]",
      "proof": "Concrete.Proof.clamp_value_correct",
      "spec": "clamp_value_behavior"
    }
  ]
}
```

All four fields are required. The compiler validates the registry and rejects entries with unknown function names, empty fields, or proofs attached to ineligible functions.

## Step 6: Verify

### Check proof status

```bash
concrete examples/proof_pressure/src/main.con --report proof-status
```

You should now see:

```
-- proved -------- main.con:90
  ✓ `main.clamp_value` — proof matches current body.
```

### Verify with the Lean kernel

```bash
concrete examples/proof_pressure/src/main.con --report check-proofs
```

This invokes Lean to verify that the theorem exists and type-checks:

```
Kernel-verified (3):
  ✓ main.clamp_value — Concrete.Proof.clamp_value_correct (registry)
```

If the theorem is missing or has errors, you will see it under `Failed`.

## Step 7: Understand the Claim

A proof in Concrete has a precise and limited meaning. Here is what your proof of `clamp_value` covers and does not cover:

### What is proved

The Lean theorem states that evaluating the function's PExpr representation with Lean's pure semantics and unbounded integers produces the specified result. The Lean kernel has mechanically verified this — it is not a human assertion.

### What is NOT proved

| Not covered | Why |
|-------------|-----|
| The compiled binary behaves the same way | The Core IR → SSA → LLVM → machine code chain is unverified |
| Integer overflow is impossible | PExpr uses Lean's unbounded `Int`; the binary uses 64-bit integers |
| The function is safe | Safety (no use-after-move, no leak) is the type checker's job, not the proof's |
| Other properties | The proof covers only the stated theorem. If you proved `clamp_value` returns `lo` when `x < lo`, you did not prove it terminates or is free of overflow |
| Composition | A proof of `f` and a proof of `g` do not automatically prove anything about `f(g(x))` |

For a complete treatment, see [PROOF_CONTRACT.md](../../docs/PROOF_CONTRACT.md).

## When Things Go Wrong

### The function body changed (stale proof)

If you edit `clamp_value`'s body, the fingerprint changes and the proof becomes stale:

```
-- proof stale --- main.con:90
  Function `main.clamp_value` has a registered proof, but the body changed.
  expected fingerprint: [(if ...)]   ← what the proof was written against
  current fingerprint:  [(if ...)]   ← the new body
```

**To fix**: update the Lean theorem to match the new body, then update the `body_fingerprint` in the registry to the new fingerprint.

### The function is not eligible

```
-- not eligible -- main.con:78
  `main.format_result` cannot be proved: fails predictable profile (has capabilities: Console).
```

The function has capabilities (I/O). Remove them to make it eligible — or accept that it is outside the proof boundary by design.

### Extraction failed (blocked)

```
-- blocked --- main.con:109
  `main.classify_range` is eligible but uses unsupported constructs — extraction failed.
  unsupported: field access
```

The function uses a construct the extraction pipeline does not yet support. Refactor to avoid it, or wait for extraction to be extended.

### Detailed failure diagnosis

For structured failure information:

```bash
concrete examples/proof_pressure/src/main.con --report proof-diagnostics
```

Each diagnostic tells you:
- **failure class**: what went wrong (e.g., `stale_proof`, `missing_proof`, `unsupported_construct`)
- **repair class**: what to do about it (e.g., `theorem_update`, `add_proof`, `code_rewrite`)
- **hint**: a human-readable suggestion

## Maintaining Proofs Over Time

### Dependency tracking

If your proved function calls other proved functions, the compiler tracks the dependency:

```bash
concrete examples/proof_pressure/src/main.con --report proof-deps
```

```
main.validate_header [proved]
  → main.check_nonce (proved)
```

If `check_nonce`'s proof goes stale, `validate_header`'s dependency graph shows it — fix the callee first.

### Refactors

| Change | Proof survives? |
|--------|----------------|
| Add comments | Yes |
| Change whitespace | Yes |
| Add a new function | Yes |
| Edit a different function | Yes |
| Rename a variable in the body | **No** — stale |
| Change a literal value | **No** — stale |
| Rename the function | **Registry orphan** — update `function` field |

When you rename a function, the compiler detects the orphan by matching fingerprints and suggests the update:

```
warning: unknown function 'main.old_name'
  (appears renamed to 'main.new_name' — update the registry entry)
```

### Daily workflow

```bash
# After any code change — quick check
concrete check

# Before merging — full verification
concrete check && concrete src/main.con --report check-proofs
```

`concrete check` runs the proof pipeline without code generation, shows all statuses, and exits 1 if anything is stale, missing, or blocked.

## Quick Reference

| I want to... | Command |
|--------------|---------|
| Check if a function is provable | `--report extraction` |
| See all proof statuses | `--report proof-status` |
| Generate theorem stubs | `--report lean-stubs` |
| Verify proofs compile | `--report check-proofs` |
| See why a proof failed | `--report proof-diagnostics` |
| See dependency graph | `--report proof-deps` |
| Quick project-level check | `concrete check` |

## Further Reading

- [PROOF_CONTRACT.md](../../docs/PROOF_CONTRACT.md) — what "proved" means precisely
- [PROOF_THEOREM_SHAPES.md](../../docs/PROOF_THEOREM_SHAPES.md) — naming rules and theorem categories
- [PROOF_WORKFLOW.md](../../docs/PROOF_WORKFLOW.md) — complete reference workflow
- [PROOF_PRESSURE_SET.md](../../docs/PROOF_PRESSURE_SET.md) — the 6-function test set used during development
