# Proof-Authoring and Maintenance Workflow

Status: canonical reference (Phase 2, item 16)

This document defines the complete workflow for proving Concrete functions with Lean 4. It covers choosing a function, writing the proof, attaching it, verifying it, diagnosing failures, repairing stale proofs, reviewing dependency fallout, and landing proof-preserving refactors.

For proof contract semantics, see [PROOF_CONTRACT.md](PROOF_CONTRACT.md).
For theorem shapes and naming rules, see [PROOF_THEOREM_SHAPES.md](PROOF_THEOREM_SHAPES.md).
For the proof pressure set used during development, see [PROOF_PRESSURE_SET.md](PROOF_PRESSURE_SET.md).

---

## 1. Choose a Function

Not every function is provable. Before writing a proof, check whether the function passes the eligibility gates.

### Eligibility gates

A function must pass **all** of these to be proof-eligible:

| Gate | Excludes |
|------|----------|
| Not an entry point | `main` functions |
| No capabilities | Functions with `with(Console)`, `with(Net)`, etc. |
| Not `trusted` | Functions marked `trusted fn` |
| No trusted implementation origin | Functions backed by trusted code |
| No recursion | Recursive functions |
| No loops | Functions containing `while` |
| No allocation | Functions using `new` |
| No FFI | Functions calling `extern` |
| No blocking I/O | Functions with blocking operations |
| No mutable assignment | Functions with `var` mutation in body |

### Extraction gates

Even if eligible, a function must use only constructs the extraction pipeline supports. Currently blocked constructs include:

- Struct field access (`b.lo`, `b.hi`)
- Match expressions
- String literals
- If-without-else
- Mutable assignment

### Inspect eligibility

```bash
concrete src/main.con --report extraction
```

This shows each function's extraction status:

| Status | Meaning | Next step |
|--------|---------|-----------|
| `extracted` | Eligible and extractable — ready to prove | Generate stubs |
| `eligible (extraction failed)` | Eligible but uses unsupported constructs | Refactor or wait for extraction support |
| `excluded` | Fails eligibility gates | Remove capabilities/trusted/etc., or accept as ineligible |

The extraction report also shows the PExpr form, parameters, fingerprint, and the specific unsupported construct (if blocked).

### Choose a claim shape

Before writing the proof, decide what property to prove. Three shapes are supported (see [PROOF_THEOREM_SHAPES.md](PROOF_THEOREM_SHAPES.md)):

| Shape | Purpose | Effort |
|-------|---------|--------|
| Concrete test case | Fixed-input regression anchor | Low — `native_decide` |
| Universal boundary theorem | Prove one branch/path for all inputs | Medium — `by_cases` + `simp` |
| Full contract | Complete input-output specification | High — full tactic proof |

Start with a concrete test case to validate extraction, then upgrade to a boundary or full contract theorem.

---

## 2. Generate Lean Stubs

```bash
concrete src/main.con --report lean-stubs > Concrete/Proof/Generated.lean
```

This generates a complete Lean 4 file with five sections:

1. **PExpr definitions** — one `<fn>Expr` per extracted function
2. **PFnDef entries** — one `<fn>Fn` per function
3. **Function table** — `generatedFns : FnTable` lookup
4. **Eval helpers** — `eval_<fn>` convenience wrappers
5. **Theorem stubs** — `<fn>_correct` with `sorry` placeholders

Example generated stub:

```lean
/-- TODO: State the correctness property for `main.check_nonce`.
    Current ProofCore: (if nonce < 0 then 2 else (if nonce > maxNonce then 1 else 0)) -/
theorem check_nonce_correct (nonce : Int) (maxNonce : Int) (fuel : Nat) :
    eval generatedFns ((Env.empty.bind "nonce" (.int nonce)).bind "maxNonce" (.int maxNonce))
      (fuel + 3) check_nonceExpr
    = sorry := by
  sorry
```

The stub tells you: the parameter names, the fuel depth, the expression to prove against, and the naming convention. Replace the RHS `sorry` with the spec expression and the proof `sorry` with tactics.

---

## 3. Write the Lean Proof

Edit the generated file. Replace the `sorry` placeholders:

```lean
theorem check_nonce_correct (nonce : Int) (maxNonce : Int) (fuel : Nat) :
    eval generatedFns ((Env.empty.bind "nonce" (.int nonce)).bind "maxNonce" (.int maxNonce))
      (fuel + 3) check_nonceExpr
    = some (.int (if nonce < 0 then 2
                  else if nonce > maxNonce then 1
                  else 0)) := by
  simp [check_nonceExpr, generatedFns, eval, Env.bind, evalBinOp]
  by_cases h1 : nonce < 0
  · simp [h1, decide_eq_true h1]
  · by_cases h2 : nonce > maxNonce
    · simp [h1, h2, decide_eq_false h1, decide_eq_true h2]
    · simp [h1, h2, decide_eq_false h1, decide_eq_false h2]
```

### Verify locally

```bash
cd Concrete && lake env lean Concrete/Proof/Generated.lean
```

The file must compile with `warningAsError = true` in `lakefile.toml`. Fix any `sorry` or type errors before proceeding.

---

## 4. Attach the Proof

Add an entry to `proof-registry.json` in the same directory as the source file:

```json
{
  "version": 1,
  "proofs": [
    {
      "function": "main.check_nonce",
      "body_fingerprint": "[(ret (if (< (var nonce) (lit 0)) ...))]",
      "proof": "Concrete.Proof.check_nonce_correct",
      "spec": "check_nonce_boundary_check"
    }
  ]
}
```

### Get the fingerprint

The fingerprint must match the function's current Core IR body exactly. Get it from:

```bash
concrete src/main.con --report extraction 2>&1 | grep -A5 check_nonce
```

The `fingerprint:` line is the value for `body_fingerprint`.

### Registry fields

| Field | Value | Rule |
|-------|-------|------|
| `function` | Qualified name | `module.functionName` — must match exactly |
| `body_fingerprint` | Core IR structural hash | Must match current body — copy from extraction report |
| `proof` | Lean theorem name | `Concrete.Proof.<fn>_correct` by convention |
| `spec` | Property name | Human-readable spec name |

### Registry validation

The compiler validates registry entries and rejects:

- Unknown function names (function not in source)
- Duplicate entries (last wins, warning issued)
- Empty proof/spec/fingerprint fields
- Proofs attached to ineligible functions (capabilities, entry points, trusted)
- Proofs attached to blocked functions (extraction failed)

---

## 5. Verify the Attachment

### Quick check

```bash
concrete check
```

This runs the full proof pipeline without code generation. Output shows each function's status and actionable next steps:

```
=== Proof Status Report ===
  main.check_nonce [proved]
  main.compute_checksum [stale]
  main.clamp_value [missing]

Next steps:
  - fix stale proof for main.compute_checksum
  - add proof for main.clamp_value
```

Exit code: 0 if all user obligations proved, 1 otherwise.

### Kernel verification

```bash
concrete src/main.con --report check-proofs
```

This invokes `lake env lean` to verify that each referenced Lean theorem actually exists and type-checks. Output:

```
Kernel-verified (1):
  ✓ main.check_nonce — Concrete.Proof.check_nonce_correct (registry)

Failed (0):
```

### Full status

```bash
concrete src/main.con --report proof-status
```

Shows all functions with their complete proof state: status, fingerprint, spec, proof name, source (registry/hardcoded), dependencies, and source location.

---

## 6. Diagnose Failures

When `concrete check` exits 1, use `--report proof-diagnostics` to see why:

```bash
concrete src/main.con --report proof-diagnostics
```

### Diagnostic kinds

| Kind | Code | Meaning | Repair |
|------|------|---------|--------|
| `stale_proof` | E0800 | Body changed, fingerprint mismatch | Update theorem or fingerprint |
| `missing_proof` | E0801 | Eligible, no registry entry | Add registry entry |
| `ineligible` | E0802 | Fails eligibility gate | Remove capability/trusted/etc. |
| `unsupported_construct` | E0803 | Extraction blocked | Refactor to supported constructs |
| `trusted` | E0804 | Function is `trusted` | Remove `trusted` if proof desired |
| `attachment_integrity` | E0805 | Registry entry invalid | Fix registry (unknown fn, duplicate, etc.) |
| `theorem_lookup` | E0806 | Lean proof name not found | Fix theorem name or write the theorem |
| `lean_check_failure` | E0807 | Lean kernel rejected proof | Fix the Lean proof |

### Failure and repair classes

Each diagnostic carries a `failure_class` and `repair_class`:

| Failure class | Repair class |
|---------------|-------------|
| `stale_proof` | `theorem_update` |
| `missing_proof` | `add_proof` |
| `effect_boundary` | `policy_change` |
| `structural_gate` | `code_rewrite` |
| `entry_point` | `none` |
| `unsupported_construct` | `code_rewrite` |
| `trusted_boundary` | `policy_change` |
| `attachment_integrity` | `registry_update` |
| `theorem_lookup` | `registry_update` |
| `lean_check_failure` | `theorem_update` |

### Machine-readable diagnostics

```bash
concrete src/main.con --report diagnostics-json 2>&1 | python3 -c "
import sys, json
data = json.load(sys.stdin)
for f in data.get('facts', []):
    if f['kind'] == 'proof_diagnostic':
        print(f['value']['function'], f['value']['diagnostic_kind'], f['value']['failure_class'])
"
```

---

## 7. Repair a Stale Proof

A proof goes stale when the function body changes (any change to variables, expressions, control flow, operators, or literals in the Core IR).

### Diagnosis

```bash
concrete src/main.con --report proof-status
```

The stale function shows both fingerprints:

```
main.compute_checksum [stale]
  fingerprint: [(ret (+ (* (var key) (var message)) (var salt) (lit 1)))]
  expected:    [(ret (+ (* (var key) (var message)) (var salt)))]
```

### Repair options

**Option A: Update the proof to match the new body.**

1. Regenerate stubs: `concrete src/main.con --report lean-stubs`
2. Update the theorem statement and proof to match the new PExpr
3. Update `body_fingerprint` in `proof-registry.json` to the new fingerprint
4. Verify: `concrete src/main.con --report check-proofs`

**Option B: Revert the code change.**

If the body change was unintentional, revert it. The original fingerprint will match again and the proof returns to `proved`.

### What does NOT cause staleness

- Comments (not in Core IR)
- Whitespace/formatting changes
- Changes to other functions
- Changes to the function's type signature alone (unless the body also changes)

---

## 8. Review Dependency Fallout

When a proved function calls other proved functions, the dependency graph tracks whether callees are still proved.

### View dependencies

```bash
concrete src/main.con --report proof-deps
```

```
main.validate_header [proved]
  → main.check_nonce (proved)

main.check_nonce [proved, no dependencies]
```

### When a dependency goes stale

If `check_nonce`'s proof becomes stale, `validate_header`'s `staleDeps` field updates:

```
main.validate_header [proved]
  → main.check_nonce (stale)
```

The caller's own proof status remains `proved` (its body didn't change), but the `stale_deps` field signals that a dependency's proof is no longer current.

### Repair order

1. Fix the callee's proof first (repair `check_nonce`)
2. Re-verify the caller (`validate_header`'s `stale_deps` clears automatically)

---

## 9. Land Proof-Preserving Refactors

### Refactors that preserve proofs

| Change | Proof preserved? | Why |
|--------|-----------------|-----|
| Add/change comments | Yes | Comments not in Core IR |
| Change formatting | Yes | Whitespace not in Core IR |
| Add a new function | Yes | Other functions' fingerprints unaffected |
| Change code in a different function | Yes | Per-function fingerprints |

### Refactors that invalidate proofs

| Change | Effect | Recovery |
|--------|--------|----------|
| Rename a variable in the body | Stale | Update theorem + fingerprint |
| Change a literal value | Stale | Update theorem + fingerprint |
| Reorder statements | Stale | Update theorem + fingerprint |
| Add/remove a branch | Stale | Rewrite theorem |
| Change operators | Stale | Rewrite theorem |
| Extract a helper (move code out) | Stale | Rewrite theorem for new body |
| Rename the function | Registry orphan | Update `function` field in registry |

### Rename detection

When a function is renamed, the compiler detects the orphaned registry entry by matching its fingerprint against current functions:

```
warning: proof-registry.json: unknown function 'main.old_name'
  (appears renamed to 'main.new_name' — update the registry entry)
```

Update the `function` field in `proof-registry.json` to the new name.

### Workflow for proof-preserving refactors

1. Run `concrete check` before the refactor — record baseline
2. Make the refactor
3. Run `concrete check` after — compare against baseline
4. For each newly stale function:
   - Regenerate stubs: `--report lean-stubs`
   - Update the theorem and fingerprint
   - Verify with `--report check-proofs`
5. For renamed functions: update registry `function` field
6. Confirm `concrete check` exits 0

---

## 10. Build Integration

### Build summary

Every `concrete build` shows a proof summary line:

```
Proofs: 2 proved, 1 stale, 7 missing, 53 blocked
```

### Project-level check

`concrete check` runs the full proof pipeline without code generation. It:

- Filters to user-package functions only (hides stdlib/dependency obligations)
- Shows actionable next steps (prioritized: stale > missing > blocked)
- Exits 1 if any user obligations are stale, missing, or blocked

### CI integration

Gate on `concrete check` exit code:

```yaml
- name: Proof status
  run: concrete check
```

Or use machine-readable output:

```bash
concrete src/main.con --report diagnostics-json | jq '.facts[] | select(.kind == "proof_diagnostic")'
```

---

## 11. Command Reference

| Command | Purpose | When to use |
|---------|---------|-------------|
| `concrete check` | Project-level proof status | Daily driver — "are all proofs current?" |
| `--report extraction` | Eligibility and PExpr forms | Before writing a proof — "is this function provable?" |
| `--report lean-stubs` | Generate Lean theorem stubs | Starting a new proof or regenerating after body change |
| `--report proof-status` | Full obligation details | Debugging — "what exactly is the status of each function?" |
| `--report obligations` | Obligation internals | Debugging — spec source, fingerprint, dependencies |
| `--report check-proofs` | Lean kernel verification | After writing a proof — "does the theorem actually compile?" |
| `--report proof-diagnostics` | Failure taxonomy | When check fails — "why did it fail and how do I fix it?" |
| `--report proof-deps` | Dependency graph | After a callee change — "which callers are affected?" |
| `--report proof-bundle` | JSON evidence bundle | CI, review, release evidence — "what is proved, under what assumptions?" |
| `--report diagnostic-codes` | Error code registry | Reference — "what does E0805 mean?" |

---

## 12. End-to-End Example

Prove `clamp_value` from the proof pressure set (currently `missing`):

```bash
# 1. Check eligibility
concrete examples/proof_pressure/src/main.con --report extraction 2>&1 | grep -A5 clamp_value
# → status: extracted — good, it's provable

# 2. Generate stubs
concrete examples/proof_pressure/src/main.con --report lean-stubs > /tmp/stubs.lean
# Look at clamp_valueExpr and the theorem stub

# 3. Write the theorem (in Concrete/Proof.lean or a separate file)
# theorem clamp_value_correct (x lo hi : Int) (fuel : Nat) :
#     eval generatedFns (...) (fuel + 3) clamp_valueExpr
#     = some (.int (if x < lo then lo else if x > hi then hi else x)) := by ...

# 4. Get the fingerprint
concrete examples/proof_pressure/src/main.con --report extraction 2>&1 | grep -A5 clamp_value | grep fingerprint

# 5. Add registry entry to proof-registry.json
# { "function": "main.clamp_value", "body_fingerprint": "<from above>",
#   "proof": "Concrete.Proof.clamp_value_correct", "spec": "clamp_value_behavior" }

# 6. Verify
concrete check
# → main.clamp_value [proved]

# 7. Kernel check
concrete examples/proof_pressure/src/main.con --report check-proofs
# → ✓ main.clamp_value — Concrete.Proof.clamp_value_correct
```

---

## Assumptions and Limitations

This workflow operates under explicit assumptions documented in [PROOF_CONTRACT.md](PROOF_CONTRACT.md):

- **PExpr model**: Proofs are about the pure functional model with unbounded integers, not the compiled binary
- **Per-function scope**: Proofs are per-function; cross-function composition relies on dependency tracking, not compositional verification
- **Fingerprint stability**: Fingerprints are stable within a compiler version but may change across versions
- **Extraction subset**: Only functions using supported constructs can be extracted and proved
- **Unverified compilation**: The Core IR → SSA → LLVM IR → binary chain is not formally verified
