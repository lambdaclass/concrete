# Proof-Carrying Software Supply Chain

**Status:** Open research direction  
**Affects:** Ecosystem, compliance, security, distribution, adoption  
**Priority:** P0 (Highest strategic value)

## The 10000x Vision

Concrete becomes the infrastructure for **software that proves its own properties** — not as a separate activity, but as the default way software is built, distributed, and consumed.

Every piece of Concrete software carries a **cryptographic evidence bundle** that proves its safety properties. Operating systems and package managers verify these proofs before installation.

## The Evidence Bundle

```
┌─────────────────────────────────────────┐
│  myapp-v1.2.3.concrete-bundle           │
├─────────────────────────────────────────┤
│  Binary (ELF/Mach-O/PE)                 │
│  Source commitment (Merkle root)        │
│  Capability manifest                    │
│  Safety proof (memory safety, no UB)    │
│  Authority proof (respects declared caps)│
│  Supply chain proof (deps verified)     │
│  Signed by: compiler, builder, auditor  │
└─────────────────────────────────────────┘
```

## Usage Scenario

### Developer Experience

```bash
$ concrete build --release
Compiling myapp v1.2.3...
Generating evidence bundle...
  ✓ Memory safety: Proved (47 functions)
  ✓ Capability discipline: Verified
  ✓ No undefined behavior: Proved
  ✓ Authority budget: Within declared limits
  ✓ Supply chain: 12 dependencies verified
  
Bundle: target/myapp-v1.2.3.concrete-bundle
Size: 2.4 MB (includes proofs: 145 KB)
```

### Distribution

```bash
$ concrete publish
Uploading to registry...
Verifying bundle integrity...
✓ Signature valid (signer: developer@company.com)
✓ Reproducible build verified
✓ All proofs check out

Package available: registry.example.com/myapp@1.2.3
```

### Installation

```bash
$ apt install nginx
Reading package lists... Done
Verifying nginx-1.24.0.concrete-bundle...
  ✓ Memory safety proof: Verified
  ✓ Capability manifest: {File, Network, Process}
  ✓ No Unsafe capability: Confirmed
  ✓ Supply chain: 12 dependencies, all proofs valid
  ✓ Reproducible build: Binary matches source

Security profile: High (all properties mechanically proven)
Installing...
```

### Runtime Enforcement

```rust
// Operating system kernel
fn execve(path: &Path, ...) -> Result<Process, Error> {
    let bundle = ConcreteBundle::load(path)?;
    
    // Verify proof (takes milliseconds)
    bundle.verify_proof()?;
    
    // Check capabilities against policy
    let caps = bundle.capability_manifest();
    if caps.contains(Unsafe) && !system_policy.allow_unsafe {
        return Err(UnsafeCodeNotAllowed);
    }
    
    // Execute with capabilities enforced
    spawn_with_capabilities(bundle.binary(), caps)
}
```

## Why This Is 10000x

| Current World | Concrete World |
|--------------|----------------|
| Hope software is safe | Verify proof of safety |
| Audit by reading code | Audit by checking evidence |
| Trust the developer | Trust the mathematics |
| Supply chain attacks common | Attacks impossible (proofs don't lie) |
| Compliance is manual | Compliance is automatic |
| Security is expensive | Security is the default |

## Technical Components

### 1. Proof Generation (Compiler)

```bash
$ concrete build --evidence-bundle
Creating: target/myapp.concrete-bundle

Contents:
  1. Binary                      2.4 MB
  2. Source attestation          32 B (Merkle root)
  3. Core IR (for audit)         145 KB
  4. Safety proof obligations    12 items
  5. Capability flow proof       ✓ Verified
  6. Authority budget compliance ✓ Verified
  7. Reproducibility evidence    ✓ Deterministic build

Signed: compiler v0.8.0, builder ci@company.com
```

### 2. Proof Verification (Runtime)

Verification happens in milliseconds:

1. Check signatures (cryptographic)
2. Verify Merkle root matches source
3. Check proof certificates (Lean proofs)
4. Verify capability manifest
5. Confirm reproducible build

### 3. Policy Enforcement

```yaml
# /etc/concrete/policy.yaml
allowed_capabilities:
  - File
  - Network
  - Alloc
  
forbidden_capabilities:
  - Unsafe
  
require_proofs:
  - memory_safety
  - capability_discipline
  
audit_mode: enforce  # or: warn, log
```

## Compliance Automation

### SOC2 Type II

| SOC2 Requirement | Concrete Evidence |
|-----------------|-------------------|
| CC6.1: Logical access controls | Capability manifest shows exact resource access |
| CC6.2: Access removal | Linear types prove resources are freed |
| CC6.6: Encryption | Crypto boundary marked and proven |
| CC7.2: Change management | Signed source attestations + reproducible builds |
| CC8.1: Test coverage | Proof verification = 100% coverage of safety properties |

Auto-generated evidence package:
```yaml
soc2_evidence:
  cc6.1_logical_access:
    - proof: memory_safety.lean
      status: verified
      scope: all_modules
      
  cc6.2_access_removal:
    - proof: capability_manifest.json
      network_access: false
      file_access: restricted_to /var/app/*
      
  cc7.2_change_management:
    - proof: git_commit_signed
      merkle_root: 0x7a3f...
      reproducible: true
```

### FedRAMP

Government software authorization that normally takes 6-12 months and costs $500K-$1M:

| FedRAMP Control | Concrete Solution |
|-----------------|-------------------|
| AC-3: Access Enforcement | Capability system = automated proof |
| SC-39: Process Isolation | Memory safety proof shows isolation |
| SI-10: Input Validation | Parser proofs + bounds checking |
| CM-7: Least Functionality | Authority budget proves minimal capabilities |
| SA-11: Developer Testing | All proofs auto-generated at build |

Evidence package:
```json
{
  "fedramp_controls": {
    "AC-3": {
      "evidence": "capability_flow.proof",
      "verification": "mechanically_proven",
      "assessor_notes": "Compiler proves no unauthorized access"
    },
    "SC-39": {
      "evidence": "memory_safety.lean",
      "isolation_proof": "address_space_separation",
      "trusted_computing_base": "Concrete_compiler_v0.8.0"
    }
  }
}
```

## Platform Effects

### The Flywheel

1. **Developers write Concrete** → Get proofs for free
2. **Distros require Concrete proofs** → For security-critical packages
3. **Enterprise mandates Concrete** → For compliance (SOC2, FedRAMP)
4. **Governments specify Concrete** → For critical infrastructure
5. **Insurance discounts for Concrete** → Lower premiums for provable code
6. **More developers write Concrete** → Ecosystem growth

### Why Other Languages Can't Do This

- **Rust**: Has safety, but no proof infrastructure, no capability system, no evidence bundles
- **Zig**: Explicit but no formal proofs, no structured evidence
- **C**: No safety guarantees
- **Ada/SPARK**: Has proofs but is niche, complex, not general-purpose systems

Concrete is uniquely positioned: **low-level + explicit + proof-friendly + usable**.

## Implementation Path

### Phase I: Basic Bundles

- Signed binaries with capability manifests
- Source attestation (Merkle trees)
- Basic proof obligations

### Phase J: Verification Infrastructure

- Package manager integration (verify on install)
- Policy enforcement
- Machine-readable reports

### Phase L: Compliance Integration

- Auto-generated SOC2 evidence
- FedRAMP package generation
- Insurance reporting

### Phase O: Ecosystem Lock-in

- Major distros adopt
- Enterprise mandates
- Government specifications

## Business Model (Open Source Sustainability)

- **Core compiler**: Free, open source
- **Evidence verification service**: Paid CI/CD integration ("Concrete Cloud")
- **Compliance reporting**: Auto-generated SOC2/FedRAMP packages
- **Insurance partnerships**: Verified software = lower premiums

## Open Questions

1. How to handle proof size for large programs?
2. What's the revocation story for compromised signing keys?
3. How to integrate with existing package managers (apt, npm, etc.)?
4. What's the legal status of machine-generated proofs in court/compliance?
5. How to handle "proof oracles" (assumptions about hardware, OS)?

## Relation to Existing Research

- `evidence-review-workflows.md`: Bundles enable report-first review
- `trust-multipliers.md`: Bundles are the ultimate trust multiplier
- `authority-budgets.md`: Bundles enforce authority budgets at distribution
- `proof-evidence-artifacts.md`: Bundles package artifacts into evidence
- `high-integrity-profile.md`: Bundles prove profile compliance

## Recommendation

**Pursue proof-carrying supply chain as the primary ecosystem strategy.** This is Concrete's strongest differentiator and creates a moat that other languages cannot easily cross. It turns the language from "a better systems language" into "the infrastructure for trustworthy software."

This is the 10000x idea.
