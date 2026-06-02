# Thesis Threat / Accident Model

Status: stable reference

This document defines the threats and accidents that Concrete's thesis properties are designed to catch, and maps each to the compiler mechanism that catches it.

## Scope

Concrete does not protect against all software bugs. It protects against a specific set of **trust-weakening changes** — modifications that silently degrade the safety, correctness, or auditability of code that was previously verified.

The threat model covers both **deliberate attacks** (malicious contributor, supply chain compromise) and **accidental drift** (refactoring mistakes, copy-paste errors, merge conflicts).

## Threat Categories

### 1. Proof Semantic Drift

**What:** A proved function's body changes in a way that invalidates the Lean theorem backing it, but the change compiles and runs without error.

**Example:** `compute_tag(key, msg, nonce) = key * msg + nonce` → `key * msg - nonce`. The tag computation silently breaks. In a real HMAC wrapper, this would produce wrong authentication tags.

**Detection mechanisms:**
- Body fingerprinting: the compiler hashes the Core IR body. Any change makes the proof stale.
- `--report effects`: evidence drops from `proved` to `enforced (proof stale: body changed)`
- `--report proof-status`: shows `proof stale` with expected vs. actual fingerprint
- `concrete diff`: reports `TRUST WEAKENED` with proof_status `proved → stale`
- `[policy] require-proofs = true`: turns stale proofs into compile errors

**Demo:** `examples/crypto_verify/src/main_drifted.con` — `compute_tag` changed `+` to `-`.

### 2. Authority Escalation

**What:** A pure function gains capabilities it did not previously have, widening its authority without changing its name or apparent purpose.

**Example:** `validate(data, len) -> Int` (pure, no capabilities) → `validate(data, len) with(File) -> Int`. The function can now read files, but call sites don't change.

**Detection mechanisms:**
- Capability system: callers without `File` get a compile error
- `--report effects`: capabilities change `[] → [File]`, `is_pure: true → false`
- `--report authority`: function appears in the File authority group
- `concrete diff`: reports `TRUST WEAKENED` with `capability / validate: capabilities [] → [File]`
- `[policy] deny = ["File"]`: capability use becomes a compile error
- `--check predictable`: function fails the predictable profile (may block)

**Demo:** `examples/thesis_demo/src/main_drifted.con` — `validate` gains `with(File)`.

### 3. Validation Weakening

**What:** A security-critical validation function accepts inputs it previously rejected, weakening the domain boundary.

**Example:** `check_nonce(nonce, max) → if nonce > 0` → `if nonce >= 0`. Now accepts nonce=0, which may be a sentinel or replay value.

**Detection mechanisms:**
- Body fingerprinting: Core IR changes, proof becomes stale
- `--report effects`: evidence drops from `proved` to `enforced`
- `concrete diff`: shows the exact Core IR change (`BinOp.gt → BinOp.geq`)
- Lean proof: the old theorem no longer applies (incorrect for nonce=0)

**Demo:** `examples/crypto_verify/src/main_drifted.con` — `check_nonce` changed `>` to `>=`.

### 4. Resource Drift

**What:** A function's resource usage changes: bounded loops become unbounded, allocation appears, or blocking I/O is introduced.

**Example:** `for (let mut i = 0; i < len; i = i + 1)` → `while i < len`. The loop is no longer structurally bounded.

**Detection mechanisms:**
- `--check predictable`: unbounded loop violation
- `--report effects`: `loops: bounded → unbounded`, `evidence: enforced → reported`
- `concrete diff`: reports `TRUST WEAKENED` with predictable violation
- `[policy] predictable = true`: compile error

**Demo:** `examples/thesis_demo/src/main_drifted.con` — `validate` uses `while` loop.

### 5. Trust Boundary Erosion

**What:** Trusted code expands or formerly pure code becomes trusted, moving logic outside the proof boundary.

**Example:** A pure validation function is marked `trusted`, removing it from proof eligibility.

**Detection mechanisms:**
- `--report effects`: evidence changes to `trusted-assumption`
- `--report proof`: function becomes ineligible for proofs
- `concrete diff`: `TRUST WEAKENED` with evidence drop
- `[policy] deny = ["Unsafe"]`: compile error if `Unsafe` capability used

**Demos:** `tests/programs/adversarial_evidence_trusted_not_proved.con` — trusted function cannot claim proved.

### 6. Specification Mismatch

**What:** A function with the same name and arity has different semantics than what its proof covers.

**Example:** Module A has `parse_byte(a, b) = a + b` (proved). Module B has `parse_byte(a, b) = a * b` (same name). Without qualified identity, B's function could inherit A's proof.

**Detection mechanisms:**
- Qualified proof identity: proofs bind to `module.function`, not just `function`
- Arity checking: wrong parameter count invalidates proof attachment
- `concrete diff`: fingerprint mismatch across versions

**Demo:** `tests/programs/adversarial_proof_cross_module.con` — same name in different module, only one proved.

## What Concrete Does NOT Protect Against

- **Logic errors that don't change the IR fingerprint**: renaming a parameter, changing a comment, or modifying code outside the proved core.
- **Trusted-code correctness**: trusted FFI functions are outside the proof boundary. If `fread` is wrong, Concrete won't catch it.
- **Backend miscompilation**: Concrete proves properties at the Core IR level. LLVM/clang bugs are below the trust boundary.
- **Side-channel attacks**: timing, power analysis, and cache behavior are not modeled.
- **Completeness of the type system**: the linearity checker is heavily tested but not formally verified.

## End-to-End Demonstration

Three example pairs demonstrate the threat model:

| Example | Original | Drifted | Attacks |
|---------|----------|---------|---------|
| `crypto_verify` | Pure MAC model, 3 proved functions | `+` → `-`, `>` → `>=` | Proof drift, validation weakening |
| `elf_header` | ELF validator, 5 proved functions | Magic byte `127` → `0`, version accepts `0` | Proof drift, validation weakening |
| `thesis_demo` | 2 proved, 2 enforced, 1 reported | `-` in parse_byte, `with(File)` + `while` in validate | Proof drift, authority escalation, resource drift |

Each pair can be tested with:
```bash
concrete snapshot examples/X/src/main.con -o original.json
concrete snapshot examples/X/src/main_drifted.con -o drifted.json
concrete diff original.json drifted.json
# Exit code 1 = trust weakened
```

These are tested in CI via the `evidence` section of `run_tests.sh --trust-gate`.
