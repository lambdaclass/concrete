# Diagnostic UX: First-Class Product Surface

Status: canonical reference

Diagnostics are a product surface, not debug output. Every predictable/proof/policy/ownership failure must explain four things:

1. **What rule was violated** — the specific invariant or contract
2. **Where** — source location with snippet
3. **Why it matters** — the semantic consequence of the violation
4. **One plausible next step** — a concrete action the developer can take

## Current state

The compiler already has solid infrastructure:

- **Stable error codes**: E0001 (parse) through E0807 (proof), 100+ distinct codes
- **Structured diagnostics**: `Diagnostic.lean` carries severity, message, pass, span, hint, code, file, context
- **Hint field**: many errors include actionable suggestions
- **Source snippets**: rendered with caret underlines at the violation point
- **Proof diagnostics**: `ProofDiagnostic` has `failureClass` and `repairClass` beyond basic message/hint

## Quality tiers

### Tier 1: Already good (rule + location + hint)

These diagnostics have a clear message, source location, and actionable hint:

| Code | Category | Message pattern | Hint |
|------|----------|-----------------|------|
| E0240 | Capability | "function 'X' requires capability 'Y' but 'Z' does not declare it" | "add 'with(Y)' to 'Z', or wrap the call" |
| E0208 | Linearity | "linear variable 'x' was never consumed" | "pass it to a function, return it, or use destroy()" |
| E0217 | Mutability | "cannot assign to immutable variable 'x'" | "declare with 'let mut' to make it mutable" |
| E0800 | Proof | "stale proof (fingerprint changed)" | "update the proof to match the current function body" |
| E0803 | Proof | "eligible but uses unsupported constructs" | "Remove struct literal to enable extraction" |

### Tier 2: Missing "why it matters"

These diagnostics state the violation and offer a hint, but do not explain the semantic consequence:

| Code | Category | Current message | Missing explanation |
|------|----------|-----------------|---------------------|
| E0610 | Policy | "policy violation: contains allocation" | Why predictability requires no allocation (bounded memory, no OOM, proof eligibility) |
| E0611 | Policy | "denied capability 'X' used" | Why the project denies this capability (policy decision, not language rule) |
| E0205 | Linearity | "linear variable 'x' used after move" | Why linearity exists (prevents UAF, double-free, enables static reasoning) |
| E0240 | Capability (Alloc) | "requires capability 'Alloc'" | Why allocation is a tracked capability (breaks predictable, unbounded growth) |

### Tier 3: Bare or generic

These diagnostics need the most work — they state the violation but the hint is generic or absent:

| Code | Category | Current hint | Better hint |
|------|----------|-------------|-------------|
| E0205 | Linearity | "consider cloning or restructuring" | Instance-specific: "variable was moved at line N; restructure so the second use receives a separate copy or borrows instead" |
| E0803 (struct literal) | Extraction | "Remove struct literal" | "PExpr only supports arithmetic and function calls, not constructor expressions. Initialize fields separately or extract initialization to a helper function" |
| E0803 (if-without-else) | Extraction | "Remove if expression" | "PExpr requires both branches for complete reasoning. Add an else branch, or restructure as two separate return statements" |

## Target diagnostic format

Every diagnostic in tier 2 and 3 should move toward this structure:

```
error[E0610]: predictable profile violation
 --> src/main.con:42:5
  |
42 |     let v = Vec.new();
  |             ^^^^^^^^^
  = rule: predictable code must not allocate (stack-only, bounded memory)
  = why:  allocation introduces OOM failure paths and unbounded growth,
          which breaks the predictable guarantee of bounded execution
  = hint: use a fixed-size array like `let buf: [i32; 64] = [0; 64]`
          or move allocation to a non-predictable caller
```

The four fields map to:
- **Header**: error code + category name
- **Location**: file, line, column, snippet with caret
- **Rule**: the invariant being enforced (from `= rule:`)
- **Why**: semantic consequence (from `= why:`)
- **Hint**: concrete next step (from `= hint:`)

## Priority categories

Ranked by user impact (most confusing diagnostics first):

### 1. Predictable/policy violations (E0610, E0611, E0612)

These are the most confusing because they enforce project-level policy, not language rules. The developer may not understand why their valid code is rejected.

**E0610 (predictable violation)** should explain:
- Which of the 5 gates failed (recursion, unbounded loops, allocation, blocking I/O, FFI)
- Why that gate exists in the predictable profile
- What alternative patterns work (e.g., bounded for-loop instead of while, fixed array instead of Vec)

**E0611 (denied capability)** should explain:
- That this is a project policy decision (`[policy] deny = [...]`)
- Which Concrete.toml file sets the policy
- That the code is valid Concrete — only the policy rejects it

**E0612 (require-proofs)** should explain:
- That the function is proof-eligible but has no attached proof
- What "proof-eligible" means (pure, no trusted, extractable)
- How to add a proof (proof-registry.json entry + Lean theorem)

### 2. Extraction blockers (E0803)

These are confusing because the code compiles and runs fine — extraction fails for reasons invisible in the source. Each unsupported construct needs its own explanation:

| Construct | Why unsupported | Alternative |
|-----------|----------------|-------------|
| struct literal | PExpr has no constructor syntax | Initialize fields separately or use helper function |
| if-without-else | PExpr requires both branches for completeness | Add explicit else branch |
| match expression | PExpr pattern matching not yet implemented | Restructure as if-else chain |
| while expression | PExpr has no unbounded loop support | Use bounded for-loop |
| enum literal | PExpr has no variant constructor | Use helper function returning the variant |
| float literal | PExpr is integer-only | Use integer scaling |
| unary operator | PExpr has no unary ops | Rewrite: `-x` as `0 - x`, `!x` as `1 - x` |
| mutable assignment | PExpr is SSA-like, no mutation | Restructure as new bindings |
| string literal | PExpr has no string type | Use integer encoding or exclude from proof scope |

### 3. Ownership/linearity failures (E0205, E0207, E0208, E0230, E0234)

These enforce Concrete's ownership model. Each should explain why the rule exists (memory safety without GC) and what the developer can do:

- **E0205 (used after move)**: "variable was consumed at [location]; after a move, the value is gone. Borrow with `&x` if you need read access, or clone if Copy"
- **E0207 (consume linear in loop)**: "consuming a linear value inside a loop would destroy it on the first iteration. Move it out of the loop, or use a Copy type"
- **E0208 (never consumed)**: "linear types must be explicitly consumed to prevent resource leaks. Pass to a function, return, or destroy()"
- **E0230 (borrow after move)**: "cannot borrow a value that has already been moved. Restructure so the borrow happens before the move"
- **E0234 (reference escapes borrow)**: "borrowed references cannot outlive their borrow scope. This prevents dangling pointers"

### 4. Stale proof repair (E0800)

After code changes, proofs become stale. The diagnostic should explain:
- What changed (fingerprint mismatch)
- What to do (re-verify the Lean theorem against updated extraction, update registry fingerprint)
- That the old proof may still be valid if the change was cosmetic (hint: re-extract and diff)

## Implementation approach

This is a documentation and message-quality item, not a structural change. The `Diagnostic` type already supports all needed fields. The work is:

1. Expand `hint` strings in Check.lean, Policy.lean, ProofCore.lean to include "why it matters"
2. Add per-construct explanations for E0803 extraction blockers in ProofCore.lean
3. Add per-gate explanations for E0610 predictable violations in Policy.lean
4. Update `--report diagnostic-codes` in Report.lean to include rule + consequence per code

No new Lean types or infrastructure needed. The existing `message`, `hint`, `details`, and `context` fields are sufficient.

## Verification

- `--report diagnostic-codes` should list all codes with rule explanation
- Each diagnostic category should have at least one adversarial test that checks the hint text
- `docs/DIAGNOSTICS.md` should cross-reference this document for the quality standard
