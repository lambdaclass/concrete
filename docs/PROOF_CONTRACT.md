# User-Facing Proof Contract

Status: canonical reference — defines what a proof artifact means, what a user may rely on, and what "proved" does and does not promise. Written for an outsider deciding whether to rely on a proof artifact in real work.

For the proof-claim taxonomy, see [CLAIM_TAXONOMY.md](CLAIM_TAXONOMY.md).
For the language-semantics vs proof-semantics boundary, see [PROOF_SEMANTICS_BOUNDARY.md](PROOF_SEMANTICS_BOUNDARY.md).
For the provable subset definition, see [PROVABLE_SUBSET.md](PROVABLE_SUBSET.md).

---

## 1. What a Proof Artifact Is

A proof artifact in Concrete consists of:

1. **A Lean 4 theorem** in `Concrete/Proof.lean` that proves a property about a function's `PExpr` (proof expression) representation.
2. **A registry entry** that binds the theorem to a specific function by qualified name, spec name, and body fingerprint.
3. **A body fingerprint** — a structural hash of the function's Core IR body that identifies the exact code the proof was written against.

These three components together constitute a proof artifact. All three must be present and consistent for the compiler to report a function as `"proved"`.

### Registry entry format

Each proof is registered with four fields:

| Field | Example | Purpose |
|-------|---------|---------|
| `function` | `"main.parse_byte"` | Qualified function name |
| `bodyFingerprint` | `"[(ret (binop ...))]"` | Structural hash of the Core IR body |
| `proof` | `"Concrete.Proof.parse_byte_correct"` | Lean theorem name |
| `spec` | `"parse_byte_adds_offset"` | Spec/property name |

The registry can be either hardcoded in `Proof.lean` (via `provedFunctions`) or loaded from a `proof-registry.json` file.

---

## 2. What "Proved" Means

When the compiler reports a function as `"proved"`, this is the precise meaning:

**The function's PExpr representation, evaluated using `Proof.eval` with Lean's unbounded integer arithmetic and pure functional semantics, satisfies the stated theorem. The function's current body fingerprint matches the fingerprint the proof was written against.**

Concretely, a function is `"proved"` when all of the following hold:

1. The function is proof-eligible (pure, not trusted, not entry point, no trusted impl origin).
2. The function's body is extractable to PExpr (the supported construct subset).
3. A registry entry exists for the function with a matching spec and proof name.
4. The registry entry's `bodyFingerprint` equals the function's current `bodyFingerprint`.
5. The Lean theorem compiles and is accepted by the Lean kernel.

If any of these conditions fails, the function is not `"proved"`.

---

## 3. What a User May Rely On

A user may rely on the following properties of a proved function:

### The theorem is mechanically checked

The Lean 4 kernel has verified the theorem. This is not a human assertion — it is a machine-checked proof. If the theorem statement says `eval(fns, env, fuel, body) = some (PVal.int expected)`, then that equality holds in the proof model.

### Stale detection is automatic

If the function's source code changes in any way that affects its Core IR body, the fingerprint changes, and the proof is automatically marked stale. The compiler will not report a function as `"proved"` with a mismatched fingerprint. This detection is deterministic and cannot be bypassed without modifying the compiler.

### The proof scope is explicit

The `--report proof-status` and `--report traceability` outputs show exactly which functions are proved, which are stale, which are unproved, and which are ineligible. The `--snapshot` JSON output includes `proof_status` and `obligation` facts with all relevant fields. There is no ambiguity about which functions carry proofs.

### The evidence level is machine-readable

JSON facts include `"state": "proved"` (in `proof_status` facts), `"status": "proved"` (in `obligation` facts), and `"evidence": "proved"` (in `effects` facts). These can be consumed by CI gates, audit tools, or external analysis.

---

## 4. What "Proved" Does NOT Mean

### It does not mean the compiled binary is correct

The proof operates over PExpr, a pure functional model with unbounded integers. The compiled binary uses fixed-width integers (64-bit `Int`, 32-bit `i32`, etc.) that wrap on overflow. The compilation chain from Core IR through SSA, LLVM IR, and machine code is unverified. A proof of `f(x) = y` in PExpr does not guarantee the binary produces `y` for input `x` — it guarantees this only within the representable integer range and assuming the compilation chain is correct.

### It does not mean the function is safe

Safety (no use-after-move, no leak, no borrow conflict) is a checker-enforced property, not a proof-backed one. A proved function also has checker-enforced safety, but the proof itself does not establish safety — the checker does.

### It does not mean all properties are proved

A proof covers the specific theorem stated. A function proved to satisfy `f(x) ≥ 0` is not proved to terminate, not proved free of overflow, and not proved correct for any property other than `f(x) ≥ 0`.

### It does not mean the checker is sound

There is no formal proof that the Concrete checker (`Check.lean`) is correct. The Lean theorems prove properties of specific functions, not that the compiler pipeline is sound.

### It does not cover composition

A proof of `f` and a proof of `g` do not automatically prove anything about `f(g(x))`. Proofs are per-function. Cross-function composition is not yet supported.

---

## 5. When a Proof Becomes Stale

A proof becomes stale when the function's body fingerprint changes. The fingerprint is a structural hash of the entire Core IR body, including:

- Variable names, types, and order
- Expression structure and nesting
- Control flow (if/while/match structure)
- Function calls and their arguments
- Operators and literal values
- Array, struct, and enum construction

**Any change** to any of these elements invalidates the fingerprint. This includes:

| Change | Fingerprint invalidated? |
|--------|-------------------------|
| Rename a local variable | Yes |
| Change a literal value | Yes |
| Reorder statements | Yes |
| Add or remove a branch | Yes |
| Change a called function name | Yes |
| Change the function's type signature | Yes (if body changes) |
| Add a comment | No (comments are not in Core IR) |
| Change whitespace/formatting | No (not in Core IR) |
| Change a different function | No (fingerprint is per-function) |

When a proof is stale:
- The obligation status becomes `ObligationStatus.stale`
- The evidence level drops to `"enforced (proof stale: body changed)"`
- The `--report proof-status` output shows the function as `stale` with a hint to update the proof
- The `proof_status` JSON fact has `"state": "stale"` with both `current_fingerprint` and `expected_fingerprint` fields
- A `proof_diagnostic` fact is emitted with `kind: "stale_proof"` and severity `"error"`

**To restore a proved function:** Either update the Lean theorem to match the new body, or revert the code change to restore the original fingerprint.

---

## 6. What Invalidates a Proof

Beyond staleness (body change), these conditions also remove the `"proved"` status:

| Condition | Result | Recovery |
|-----------|--------|----------|
| Function body changes | Stale | Update the Lean proof or revert the change |
| Function becomes trusted | Ineligible | Remove `trusted` marker |
| Function gains capabilities | Ineligible | Remove capabilities (make function pure) |
| Function becomes an entry point | Ineligible | Not recoverable (entry points cannot be proved) |
| Function body uses unsupported constructs | Blocked | Refactor to use only supported constructs |
| Registry entry removed | Missing | Re-add the registry entry |
| Lean theorem fails to compile | Invalid | Fix the theorem |

Note: changes to *other* functions do not affect a proved function's status, even if the proved function calls them. The fingerprint is computed from the function's own body, not from its callees' bodies. If a callee changes behavior without changing the caller's body, the caller's proof remains valid over its PExpr — but the semantic assumption about the callee may no longer hold. This is a known limitation of per-function proofs.

---

## 7. Compatibility Promise

### What is stable

| Artifact | Stability |
|----------|-----------|
| Fingerprint algorithm | Stable within a compiler version. The same source produces the same fingerprint. |
| Obligation status derivation | Deterministic. Same inputs → same status. |
| JSON fact schema for `proof_status` and `obligation` | Fields documented in this contract are stable. New fields may be added. |
| Five evidence levels | Stable vocabulary. See [CLAIM_TAXONOMY.md](CLAIM_TAXONOMY.md). |
| Stale detection logic | Stable. Fingerprint mismatch → stale. No exceptions. |

### What is NOT stable

| Artifact | Why not stable |
|----------|---------------|
| Fingerprint values across compiler versions | The fingerprint algorithm may change between compiler versions. A proof written against one compiler version may be stale under a different version even if the source code has not changed. |
| PExpr representation | The proof IR may be extended (new constructs, changed normalization). Existing theorems remain valid, but new theorems may need to account for the extended model. |
| Eligibility gates | The set of constructs that are extractable to PExpr may change (expand or contract). A function eligible today may become ineligible if extraction rules tighten, or eligible if they expand. |
| Report formatting | Human-readable report output may change at any time. Do not parse it programmatically — use JSON facts instead. |

### Cross-version proof migration

When upgrading the compiler, the expected workflow is:

1. Run the compiler on existing code.
2. Check `--report proof-status` for stale proofs.
3. For each stale proof, either:
   - Re-verify the Lean theorem against the new PExpr (if the proof model changed)
   - Update the registry fingerprint if only the fingerprint format changed
   - Accept the stale status temporarily and track it as a known debt

There is no automatic proof migration. Fingerprint changes are intentional signals that the proof needs re-examination.

---

## 8. Interaction with Trusted Code

A proved function cannot itself be trusted (`trusted fn` excludes from proof eligibility). But a proved function can *call* a function that is implemented by trusted code, provided:

- The trusted function has a declared capability set.
- The proved function does not itself have capabilities (it must remain pure).
- Therefore: a pure proved function cannot call any trusted function that has capabilities.

In practice, this means proved functions operate entirely within the pure subset. They cannot reach trusted code through any path that introduces capabilities. The proof boundary and the trust boundary do not overlap.

If a trusted function is made non-trusted (e.g., by removing pointer-level operations and the `trusted` marker), it may become proof-eligible, but its proofs must be written from scratch.

---

## 9. Interaction with FFI

Proved functions cannot call extern functions. The proof eligibility gate requires an empty capability set, and calling an untrusted extern function requires `with(Unsafe)`. Even `trusted extern fn` calls are excluded by the `--report proof` heuristic (which flags functions with extern calls in their bodies).

If a proved function calls another proved function, and that callee is later changed to call an extern function, the callee becomes ineligible. The caller's fingerprint does not change (the caller still calls the same function by name), so the caller's proof remains nominally valid — but the semantic assumption about the callee is no longer backed by a proof. This is a per-function limitation.

---

## 10. Interaction with Backend/Target Assumptions

The proof model (PExpr + `eval`) is entirely above the backend:

| Layer | Proof coverage |
|-------|---------------|
| PExpr evaluation | Covered by the Lean theorem |
| PExpr ↔ CExpr correspondence | Assumed correct (not formally verified) |
| Core IR → SSA lowering | Not covered |
| SSA → LLVM IR emission | Not covered |
| LLVM optimization and codegen | Not covered |
| Linker, OS, hardware | Not covered |

The integer-representation gap is the most concrete backend interaction: PExpr uses Lean's unbounded `Int`, while the compiled code uses fixed-width integers. A proof that `f(x) = 42` holds for all mathematical integers, but the binary may produce a different result if `x` causes overflow during computation.

---

## 11. Summary: Decision Framework

Use this to decide whether to rely on a Concrete proof artifact:

| Question | Answer |
|----------|--------|
| Can I trust the theorem itself? | Yes — it is Lean-kernel-checked. |
| Can I trust that it applies to the current code? | Yes, if `state == "proved"` — fingerprint match is automatic. |
| Can I trust that the binary behaves the same way? | Only within the integer-representable range, and assuming correct compilation. |
| Can I trust it across compiler upgrades? | No — fingerprints may change. Re-check after upgrades. |
| Can I trust composition of proved functions? | No — proofs are per-function. |
| Can I trust that safety properties hold? | Safety is checker-enforced, not proof-backed. The proof adds correctness, not safety. |
| Should I use proof artifacts in CI gates? | Yes — JSON facts are machine-readable and deterministic. Gate on `"state": "proved"` in `proof_status` facts. |
| What should I do when a proof goes stale? | Update the Lean theorem, update the registry fingerprint, or revert the code change. |
