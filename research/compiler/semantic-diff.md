# Semantic Diff and Code Archaeology

**Status:** Open research direction  
**Affects:** Developer tooling, code review, audit workflows, change management  
**Priority:** P1 (High value, low implementation cost)

## Summary

A `concrete diff` command that understands the semantics of Concrete code, not just its text. This enables meaningful change review by revealing how modifications affect capabilities, trust boundaries, memory safety, and API contracts.

## Motivation

Traditional diffs show text changes. Semantic diffs show **meaning changes**:

```bash
# Text diff (what we have now)
$ diff old.con new.con
- fn process() { ... }
+ fn process() with(File, Network) { ... }

# Semantic diff (what we want)
$ concrete diff old.con new.con --trust-impact
⚠️  Trust Impact Detected
   Function: process
   Change: Added Network capability
   Impact: 3 call sites affected
   Risk: Function now makes network requests
```

## Features

### 1. Capability Diff

```bash
$ concrete diff v1.0 v2.0 --capabilities

Capability Changes:
  + crypto::verify now requires Network (was: pure)
    → Called by: tls::handshake → net::connect
    ⚠️  Crypto function should not need Network
    
  ~ parser::parse requires File (unchanged)
  
  - logger::init no longer requires Console (was: with(Console))
    ✓ Reduced authority
```

### 2. Trust Boundary Diff

```bash
$ concrete diff --trust-boundaries

Trust Boundary Changes:
  + 3 new trusted extern fn (was: 0)
    - src/crypto.con:45: extern fn openssl_sha256
    - src/net.con:89: extern fn curl_fetch
    
  ~ sha256::hash moved from trusted to safe
    ✓ Now proven in Lean (no trusted code needed)
    
  ! 1 function gained Unsafe capability
    - src/ffi.con:23: raw pointer dereference added
    ⚠️  Requires security review
```

### 3. Layout/ABI Diff

```bash
$ concrete diff --layout

Layout Changes:
  ! PacketHeader repr(C) field order changed
    - Old: { flags, length, data }
    + New: { length, flags, data }
    ⚠️  ABI BREAK: Binary incompatibility with v1.0
    
  ~ struct User size: 48 bytes → 56 bytes (+8)
    Reason: added email field
    Impact: 3 dependencies affected
```

### 4. API Compatibility Diff

```bash
$ concrete diff --api v1.0

API Changes (semver analysis):
  ✗ BREAKING: parse_config() removed parameter
    - parse_config(path: &str, strict: bool)
    + parse_config(path: &str)
    Suggestion: Add default parameter instead
    
  ✗ BREAKING: Return type changed
    - fn find_user(id: u64) -> Option<User>
    + fn find_user(id: u64) -> Result<User, Error>
    Migration: Add ? operator at call sites
    
  ✓ Compatible: Added new function
    + fn find_users(query: &str) -> Vec<User>
    
Suggestion: Bump major version (2 breaking changes)
```

### 5. Safety Diff

```bash
$ concrete diff --safety

Safety Property Changes:
  ✓ Function became safer
    - parse() was: may panic on bad input
    + parse() now: returns Result, no panic paths
    Proof: panic_freedom.lean (automatically generated)
    
  ⚠️  Function became less safe
    - process() was: pure function
    + process() now: with(Alloc) - may fail on OOM
    Review: 5 call sites need error handling
```

## Usage Modes

### Code Review Integration

```bash
# In CI/CD pipeline
$ concrete diff --ci origin/main

Trust Impact Report for PR #123:
================================
Files changed: 12
Functions changed: 47

Capability Changes:
  + 2 functions gained Network capability
  ~ 5 functions changed capability set
  
Trust Boundary Changes:
  + 1 new trusted extern fn
  
Safety Changes:
  ✓ 3 functions proven panic-free (was: not proven)
  
Policy Violations:
  ⚠️  crypto::sign uses Network (violates: crypto modules must be pure)
  
Recommendation: Reject or request security review
```

### Release Analysis

```bash
$ concrete diff v1.0.0 v1.1.0 --release-notes

Generated Release Notes:
------------------------
Security:
  - Removed Unsafe from 12 functions (now proven safe)
  
Performance:
  - 5 functions no longer allocate (stack-only)
  
Breaking Changes:
  - None (API compatible)
  
Trust Changes:
  - Reduced trusted surface by 200 lines
  - All crypto functions now formally verified
```

### Audit Trail

```bash
$ concrete diff --audit-log --since="2024-01-01"

Audit Trail for Compliance:
===========================
Total changes: 1,247 commits
Trusted code changes: 3 (all reviewed)
Capability increases: 12 (all documented)
Proof regressions: 0

Significant changes:
  2024-03-15: Added File capability to config module
    Reviewer: security@company.com
    Justification: Required for configuration loading
    
  2024-05-22: Removed Unsafe from hash function
    Proof: hash_proof.lean added
    Verifier: automated
```

## Implementation

### Architecture

```
old version ──┐
              ├──► parse ──► Core IR ──► summarize ──┐
new version ──┘                                      ├──► compare ──► report
                                                     │
                                            (capabilities, layout,
                                             trust boundaries, etc.)
```

### Data Structures

```lean
structure SemanticDiff where
  capabilityChanges : List CapabilityChange
  trustChanges : List TrustChange
  layoutChanges : List LayoutChange
  apiChanges : List APIChange
  safetyChanges : List SafetyChange
  
structure CapabilityChange where
  function : FunctionId
  oldCaps : CapSet
  newCaps : CapSet
  callSitesAffected : List Location
  severity : Severity
```

### CLI Design

```bash
# Basic usage
concrete diff <old> <new> [options]

old/new can be:
  - File paths: concrete diff old.con new.con
  - Git refs: concrete diff v1.0 v2.0
  - Directories: concrete diff src/ src.new/
  - Commit ranges: concrete diff HEAD~10..HEAD

Options:
  --capabilities    Show capability changes
  --trust           Show trust boundary changes
  --layout          Show layout/ABI changes
  --api             Show API compatibility
  --safety          Show safety property changes
  --all             Show all categories
  --ci              CI mode (exit non-zero on policy violations)
  --format=json     Machine-readable output
  --policy=<file>   Apply custom policies
```

## Policy Configuration

```yaml
# concrete-diff-policy.yaml
capability_rules:
  - module: crypto::*
    forbidden: [Network, File]
    rationale: Crypto modules must be pure
    
  - module: net::*
    required: [Network]
    rationale: Network module must declare capability

trust_rules:
  max_trusted_lines_per_release: 100
  require_review_for: [Unsafe, trusted extern]
  
abi_rules:
  breaking_changes_require: major_version_bump
  
safety_rules:
  require_panic_freedom_for: [parse_*, decode_*]
  ```

## Integration Points

### Git

```bash
# Pre-commit hook
$ concrete diff --cached --ci
Error: Breaking ABI change detected
Commit blocked. Use --force to override (requires justification).

# Git attribute for semantic diffs
$ git diff
concrete diff v1..v2 -- src/crypto.con:
  - trusted fn hash(...)
  + fn hash(...)  // Now proven!
```

### CI/CD

```yaml
# .github/workflows/security.yml
- name: Check for trust violations
  run: concrete diff origin/main HEAD --ci --policy=security.yaml
  
- name: Generate API changelog
  run: concrete diff v${{ env.LAST_VERSION }} HEAD --api --format=json > api-changes.json
```

### Package Manager

```bash
$ concrete install some-package
Analyzing trust impact...
This package requires:
  - File capability (will access filesystem)
  - Network capability (will make HTTP requests)
  
Trusted code:
  - 3 extern fn (verified by: registry.example.com)
  
Proceed? [Y/n]
```

## Open Questions

1. **How to handle large diffs?**
   - Paginate? Filter? Summarize?

2. **What's the performance?**
   - Need to parse both versions fully
   - Cache Core IR artifacts?

3. **How to integrate with git?**
   - Custom diff driver?
   - Separate tool?

4. **What's the policy language?**
   - YAML sufficient?
   - Need embedded Concrete expressions?

5. **How to handle dependencies?**
   - Diff transitive changes?
   - Just direct changes?

## Relation to Existing Research

- `evidence-review-workflows.md`: Semantic diff is the primary review tool
- `authority-budgets.md`: Diff shows authority budget violations
- `trust-multipliers.md`: Trust drift detection via diff
- `comparative-program-suite.md`: Diff enables cross-version comparison
- `testing-strategy.md`: Diff as a test (did capabilities change unexpectedly?)

## Recommendation

**Implement semantic diff early.** It has:
- High value (enables proper code review)
- Low cost (reuses existing compiler infrastructure)
- Strong philosophy fit (explicit, auditable)
- Clear ecosystem value (differentiator from other languages)

This should be a **Phase F or G** deliverable, alongside the report infrastructure.
