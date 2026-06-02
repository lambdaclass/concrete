# Proof-Claim Taxonomy

Status: canonical reference — the standard vocabulary for every claim Concrete makes about a program. All docs, reports, CLI output, JSON facts, and release criteria must use these five classes and no others.

For the public guarantee statement, see [GUARANTEE_STATEMENT.md](GUARANTEE_STATEMENT.md).
For the effect/trust proof boundary, see [EFFECT_PROOF_BOUNDARIES.md](EFFECT_PROOF_BOUNDARIES.md).
For the language-semantics vs proof-semantics boundary, see [PROOF_SEMANTICS_BOUNDARY.md](PROOF_SEMANTICS_BOUNDARY.md).
For the user-facing proof contract, see [PROOF_CONTRACT.md](PROOF_CONTRACT.md).

---

## 1. The Five Claim Classes

Every statement the compiler makes about a program falls into exactly one of these classes:

### Class 1: Enforced

**Definition:** A property mechanically checked by the compiler (Check, CoreCheck, SSAVerify) that the program cannot violate and still compile.

**What qualifies:**
- Ownership and linearity (use-after-move, no-leak, no-linear-reassignment)
- Borrow safety (no-conflict, no-escape, frozen-variable access)
- Capability containment (caller ⊇ callee capabilities)
- Control-flow agreement (branch consumption, loop-depth restriction, break/continue checks)
- `&mut T` consumption tracking (borrow-block refs consumed on call)
- Type checking, match coverage, scope-exit checks
- SSA verification (use-before-def, domination)

**Compiler vocabulary:**
- `--report effects` → `"evidence": "enforced"` or `"enforced (proof stale: body changed)"`
- `--report traceability` → `"evidenceLevel": "enforced"`
- Checker errors are hard failures — the program does not compile

**What it means for users:** If the program compiles without `trusted` or `with(Unsafe)`, these properties hold. No trust required.

---

### Class 2: Proved

**Definition:** A property with a Lean 4 theorem attached via the proof registry, over the function's extracted PExpr representation, with a matching body fingerprint.

**What qualifies:**
- Functional correctness theorems (e.g., `abs_positive`, `max_right`, `clamp_in_range`)
- Structural lemmas (e.g., `eval_lit`, `eval_if_true`, `eval_add_lits`)
- Any user-attached theorem that satisfies the proof pipeline (spec → extraction → proof → registry)

**Compiler vocabulary:**
- `--report effects` → `"evidence": "proved"`
- `--report traceability` → `"evidenceLevel": "proved"`
- `--report proof-status` → `"state": "proved"`
- JSON fact kind `"proof_status"` → `"state": "proved"`
- JSON fact kind `"obligation"` → `"status": "proved"`
- `--report traceability` → `"boundary": "ProofCore (source-level proof, not preserved past Core)"`

**What it means for users:** The stated theorem holds over the PExpr model with Lean's unbounded integer arithmetic. The proof does not cover integer overflow, compiled binary behavior, or backend correctness. See [PROOF_SEMANTICS_BOUNDARY.md](PROOF_SEMANTICS_BOUNDARY.md) for the exact scope.

**Stale detection:** If the function body changes, the fingerprint changes, and the proof is revoked. The evidence level drops to `"enforced (proof stale: body changed)"` until the proof is updated.

---

### Class 3: Reported

**Definition:** A fact or analysis result the compiler computes and presents in reports, but does not enforce as a hard error. The program can compile regardless of what the report says.

**What qualifies:**
- Allocation classification (`--report alloc`: allocating, stack-only, cleanup patterns)
- Effect summaries (`--report effects`: recursion class, loop class, FFI crossings)
- Predictable profile results (`--check predictable`: five-gate pass/fail)
- Capability chain analysis (`--report authority`: transitive capability propagation)
- Layout information (`--report layout`: sizes, alignment, offsets)
- Eligibility assessment (`--report eligibility`: source/profile gate breakdown)
- Proof diagnostics (`--report proof-diagnostics`: unsupported constructs, blocked extraction)

**Compiler vocabulary:**
- `--report effects` → `"evidence": "reported"`
- `--report traceability` → `"evidenceLevel": "reported"`
- JSON fact kind `"eligibility"` → `"status": "eligible" | "excluded" | "trusted"`
- JSON fact kind `"proof_diagnostic"` → diagnostic entries
- `--report traceability` → `"boundary": "source (fails predictable profile)"`

**What it means for users:** The compiler observed this property but does not block compilation if the property fails. Reports are audit tools, not gates. Use them to understand code, not to rely on them as enforcement.

---

### Class 4: Trusted Assumption

**Definition:** A property that the programmer explicitly asserts by using `trusted` or `extern fn`, and that the compiler accepts without verification.

**What qualifies:**
- `trusted fn` / `trusted impl` — programmer asserts the pointer-level implementation is correct
- `trusted extern fn` — programmer asserts the foreign binding is pure and safe
- `extern fn` (untrusted) — programmer asserts the foreign function's behavior matches its declared type
- Any property of code inside a `trusted` boundary that depends on raw pointer correctness

**Compiler vocabulary:**
- `--report effects` → `"evidence": "trusted-assumption"`
- `--report traceability` → `"evidenceLevel": "trusted-assumption"`
- `--report unsafe` → trust boundary analysis (what each trusted function wraps)
- `--report traceability` → `"boundary": "source (trusted, no verification)"`
- JSON fact kind `"obligation"` → `"status": "trusted"`
- JSON fact kind `"eligibility"` → `"status": "trusted"`

**What it means for users:** The compiler tracks and reports where trust boundaries exist but does not verify the code inside them. If trusted code is wrong, the guarantees it wraps are violated silently. `--report unsafe` is the audit surface.

---

### Class 5: Backend/Target Assumption

**Definition:** A property that depends on the backend toolchain, operating system, hardware, or runtime — below the level where the Concrete compiler operates.

**What qualifies:**
- LLVM IR semantics and optimization correctness
- `malloc`/`realloc`/`free` behaving correctly
- OS guard page for stack overflow
- Two's-complement integer wrapping on target architecture
- Linker producing a correct executable
- ABI compatibility between Concrete and C calling conventions
- `trusted extern fn` foreign library correctness

**Compiler vocabulary:** No direct Report.lean field. This class exists only in documentation and release criteria. The compiler cannot observe or report on these properties.

**What it means for users:** Everything below LLVM IR emission is assumed correct. There is no compiler-side verification of these assumptions. They are the foundation on which all other claims rest.

---

## 2. Mapping to Report.lean Vocabulary

| Taxonomy class | `evidence` field (effects) | `evidenceLevel` field (traceability) | `state` field (proof_status) | `status` field (obligation) | `status` field (eligibility) |
|---------------|---------------------------|--------------------------------------|------------------------------|-----------------------------|-----------------------------|
| Enforced | `"enforced"` | `"enforced"` | — | — | — |
| Proved | `"proved"` | `"proved"` | `"proved"` | `"proved"` | — |
| Reported | `"reported"` | `"reported"` | `"missing"` / `"blocked"` / `"ineligible"` | `"missing"` / `"ineligible"` | `"eligible"` / `"excluded"` |
| Trusted assumption | `"trusted-assumption"` | `"trusted-assumption"` | `"trusted"` | `"trusted"` | `"trusted"` |
| Backend/target | — | — | — | — | — |

**Special case:** `"enforced (proof stale: body changed)"` — a function that was previously proved but whose body has changed. The proof is stale, so the evidence drops from Proved to Enforced. This is correct: the function still passes the checker, but the Lean theorem no longer matches.

---

## 3. Mapping to CLI Output

| CLI flag | Primary taxonomy class shown | What it reveals |
|----------|------------------------------|-----------------|
| `--report effects` | All four compiler classes | Per-function evidence level with effect breakdown |
| `--report proof-status` | Proved, Enforced (stale) | Per-function proof state with fingerprints |
| `--report eligibility` | Reported | Per-function eligibility with gate breakdown |
| `--report obligations` | Proved, Reported, Trusted | Per-function obligation status |
| `--report proof-diagnostics` | Reported | Unsupported constructs, blocked extraction |
| `--report caps` | Enforced | Per-function capability requirements |
| `--report unsafe` | Trusted assumption | Trust boundaries, what trusted functions wrap |
| `--report authority` | Enforced, Reported | Transitive capability chains |
| `--check predictable` | Reported | Five-gate predictable profile check |
| `--report alloc` | Reported | Allocation/cleanup analysis |
| `--report traceability` | All four compiler classes | Full per-function traceability with evidence + boundary |

---

## 4. Mapping to JSON Facts

| JSON `kind` | Fields carrying taxonomy info | Values |
|-------------|-------------------------------|--------|
| `"effects"` | `"evidence"` | `"proved"`, `"enforced"`, `"enforced (proof stale: body changed)"`, `"reported"`, `"trusted-assumption"` |
| `"proof_status"` | `"state"` | `"proved"`, `"stale"`, `"missing"`, `"blocked"`, `"ineligible"`, `"trusted"` |
| `"eligibility"` | `"status"` | `"eligible"`, `"excluded"`, `"trusted"` |
| `"obligation"` | `"status"` | `"proved"`, `"stale"`, `"missing"`, `"blocked"`, `"ineligible"`, `"trusted"` |
| `"traceability"` | `"evidence"`, `"boundary"` | Evidence: same as effects; Boundary: claim scope description |
| `"proof_diagnostic"` | `"severity"` | `"warning"`, `"info"` |

---

## 5. Mapping to Docs and Release Criteria

Every claim in a Concrete document must carry its taxonomy class explicitly or be obviously inferable from context:

| Document | What it should label |
|----------|---------------------|
| GUARANTEE_STATEMENT.md | Each guarantee tier maps to one taxonomy class |
| MEMORY_GUARANTEES.md | Properties 1–11 are **Enforced**; the "Stronger Claim" items note their class |
| EFFECT_PROOF_BOUNDARIES.md | Each effect category table has an "Evidence level" column using taxonomy terms |
| PROOF_SEMANTICS_BOUNDARY.md | Sections 2–4 use "proof-backed" = Proved, "compiler-enforced" = Enforced |
| PROVABLE_SUBSET.md | Eligibility is Reported; proved theorems are Proved; checked invariants are Enforced |
| SAFETY.md | Three-way split maps to: Capabilities = Enforced, Trusted = Trusted Assumption, Unsafe = Trusted Assumption |
| MUT_REF_SEMANTICS.md | Checker behavior = Enforced |
| Release criteria (future) | Must specify which class each requirement belongs to |

---

## 6. Forbidden Conflations

These conflations are errors. If they appear in docs, reports, CLI output, or discussion, they must be corrected:

### Reported ≠ Proved

A report saying a function is "eligible for ProofCore" does not mean it is proved. Eligibility is a Reported observation. A proof is a Lean theorem with a matching fingerprint. The gap between them is real: many eligible functions have no proof attached.

**Wrong:** "This function is eligible, so it's proved."
**Right:** "This function is eligible for proof extraction. No proof has been attached yet."

### Checker-enforced ≠ Lean-proved

The checker enforcing linearity does not mean linearity is formally proved. Enforcement is mechanical checking in `Check.lean`. A Lean proof would be a theorem in `Proof.lean` or a separate soundness argument. Concrete has the former but not the latter.

**Wrong:** "The checker proves no use-after-move."
**Right:** "The checker enforces no use-after-move. There is no formal proof of checker soundness."

### Proved ≠ Binary-correct

A Lean theorem about a function's PExpr representation does not guarantee the compiled binary behaves identically. The proof model uses unbounded integers; the binary uses fixed-width integers. The compilation chain (Core → SSA → LLVM IR → machine code) is unverified.

**Wrong:** "This function is proved correct."
**Right:** "This function's PExpr model satisfies the stated theorem. The proof does not cover integer overflow or backend correctness."

### Trusted ≠ Verified

`trusted fn` means the programmer asserts correctness, not that the compiler has verified it. The compiler tracks trust boundaries but does not look inside them.

**Wrong:** "This trusted function is safe."
**Right:** "This function is marked trusted. Its safety depends on audit, not compiler verification."

### Enforced ≠ Complete

The checker enforcing a property does not mean the property covers all possible scenarios. Conservative restrictions (whole-value borrows, no field-granular tracking) mean some valid programs are rejected. The enforced property is sound (no false negatives for the checked property) but the coverage is intentionally limited.

**Wrong:** "The borrow checker handles all aliasing scenarios."
**Right:** "The borrow checker enforces whole-value borrowing. Field-granular and per-element borrows are not supported."

---

## 7. Vocabulary Rules

For consistency across the project:

1. **Use the five class names exactly.** Do not invent synonyms. "Verified" is not a class. "Guaranteed" is ambiguous. "Checked" is ambiguous (use "enforced"). "Validated" is ambiguous (use "enforced" for checker, "proved" for Lean).

2. **Always qualify "safe."** "Safe code" means code without `trusted` or `with(Unsafe)`. Never use "safe" to mean "proved" or "correct."

3. **Always qualify "correct."** A function can be "proved correct over PExpr" or "enforced to satisfy linearity." Do not say "correct" without specifying the scope and evidence level.

4. **Always qualify "proof."** "Proof" in Concrete means a Lean 4 theorem over PExpr. It does not mean "evidence," "report," or "enforcement." If you mean something other than a Lean theorem, use a different word.

5. **Use "trusted" only for the compiler mechanism.** `trusted fn` and `trusted extern fn` are specific Concrete keywords. Do not use "trusted" colloquially to mean "reliable" or "believed to be correct."

6. **Use canonical status strings in all machine-readable output.** The six canonical proof/obligation status terms are defined in `ObligationStatus.canonical` and must be used consistently across all JSON facts, CLI status fields, and programmatic output:

   | Canonical | Meaning | Former variants (now banned) |
   |-----------|---------|------------------------------|
   | `"proved"` | Spec attached, fingerprint matches, extraction succeeded | — |
   | `"stale"` | Spec attached, fingerprint changed | — |
   | `"missing"` | Eligible, extractable, no spec attached | (see below) |
   | `"blocked"` | Eligible but extraction failed (unsupported constructs) | — |
   | `"ineligible"` | Fails profile gates | (see below) |
   | `"trusted"` | Marked trusted (proof bypassed) | — |

   Banned former variants: `missing` replaces the former no\_proof, missing\_proof, and not\_proved. `ineligible` replaces the former not\_eligible.

   **Note:** Diagnostic kind labels (stale\_proof, missing\_proof, unsupported\_construct) are a different domain — they describe the *type of diagnostic*, not the obligation status. These are intentionally more descriptive than status terms.

   The terminology gate (`scripts/tests/test_terminology_gate.sh`) enforces this in CI.
