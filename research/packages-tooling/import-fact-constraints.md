# Import Fact Constraints

Status: research note, design input for ROADMAP Phase 17.

## Thesis

Imports should not grant power. Imports should declare, summarize, and constrain
facts about the imported interface.

In Concrete, a function still needs the capabilities it uses in its own
signature:

```con
fn main() with(File) {
    // calls file APIs
}
```

An import does not make `File` available. Instead, the import boundary should be
an audit and policy surface:

```con
import std.fs requires(File)
import std.hash requires(no File, no Network, no Unsafe)
import hmac.compute requires(proved_by_lean)
import crypto.compare requires(constant_time, no secret_sink)
import embedded.ringbuf requires(freestanding, no Alloc)
```

The first form says "this import is allowed to expose/use File authority." The
second says "this import must remain below this authority ceiling." The third is
the evidence-typed import idea: the imported surface must carry a minimum proof
or evidence class. The later forms show that the same boundary can constrain
security, platform, allocation, and runtime facts.

## Why This Matters

Concrete already makes local authority visible through `with(...)`. Packages and
interfaces will also expose allocation, trust, evidence, runtime-failure,
platform, determinism, arithmetic-profile, and supply-chain facts. Imports are
where drift in those facts becomes social and supply-chain relevant:

- a parser dependency starts logging to disk;
- a hash helper adds `Network` for telemetry;
- a pure package begins depending on `Unsafe`;
- a dependency's evidence downgrades from `proved_by_lean` to `assumed`.
- a dependency starts allocating on a formerly no-alloc path;
- a freestanding dependency becomes hosted-only;
- a constant-time helper loses its constant-time evidence.

Without import constraints, reports can describe that drift after the fact. With
constraints, the importing package can fail closed.

## Layers

### 1. Module capability summary

Every interface artifact should summarize the direct and transitive capability
sets of its public surface:

```json
{
  "module": "std.fs",
  "public_capabilities": ["File", "Alloc"],
  "trusted_boundaries": ["std.fs.File.read_raw"],
  "unsafe_surface": []
}
```

This is descriptive and should exist before policy enforcement.

### 2. Import fact constraints

Source or manifest syntax can then constrain an import:

```con
import std.parse requires(no File, no Network, no Unsafe)
import std.fs requires(File)
import parser.core requires(no Alloc, deterministic)
import std.net requires(hosted, posix)
import dsp.filter requires(arithmetic = wrap)
import crypto.compare requires(constant_time, no secret_sink)
```

The exact syntax is deferred. The invariant is not: the compiler checks the
imported interface artifact, not private source side channels, and rejects a
fact widening or fact downgrade unless the importing package updates the
constraint.

The first implementation should start with capabilities because they already
exist in compiler facts. The design should not hard-code "capabilities only";
the interface artifact and grammar should leave room for the other fact classes.

### 3. Package authority budgets

Package manifests can state package-wide or target-wide authority ceilings:

```toml
[authority]
allowed = ["Alloc"]
forbidden = ["File", "Network", "Process", "Unsafe"]
```

Import constraints are local assertions; package budgets are global policy.
Both should use the same capability vocabulary and report machinery.

### 4. Evidence-typed imports

Capability constraints compose naturally with evidence constraints:

```con
import hmac.compute requires(proved_by_lean, no Unsafe)
```

The package resolver checks both:

- authority does not exceed the accepted set;
- evidence is present, fresh, non-vacuous, and at least as strong as required by
  policy.

### 5. Other fact classes

The same import-boundary mechanism should eventually cover:

- **Allocation:** `requires(no Alloc)`, `requires(bounded_alloc)`.
- **Trust:** `requires(no trusted, no extern, no Unsafe)`.
- **Runtime safety:** `requires(no unchecked_oob, no div_zero, no panic)`.
- **Platform/target:** `requires(hosted)`, `requires(freestanding)`,
  `requires(posix)`.
- **Arithmetic profile:** `requires(arithmetic = wrap)` or
  `requires(arithmetic = checked)`.
- **Determinism:** `requires(deterministic)`, `requires(no nondeterminism)`.
- **Security claims:** `requires(constant_time)`, `requires(no secret_sink)`.
- **Supply-chain facts:** `requires(source_verified)`,
  `requires(license = MIT)`.

Not all of these need v1 syntax. The important architectural decision is that
the package/interface schema models imports as fact-checked boundaries, not only
name lookup.

## Non-Goals

- Imports do not grant authority.
- Imports do not create ambient capabilities.
- Import constraints are not a second effect system or a general policy
  language.
- Import constraints should not require reading dependency source.
- Import constraints should not become a broad policy language before package
  artifacts exist.

## Gate Shape

Future gate: `scripts/tests/check_import_fact_constraints.sh`.

It should prove:

- an import with `requires(no Network)` rejects a dependency whose public API
  transitively needs `Network`;
- an import with `requires(File)` accepts a file API but does not grant `File`
  to the importer;
- a dependency capability widening breaks the importing package until the
  constraint is updated;
- release bundles show imported authority requirements and the artifact hashes
  they were checked against;
- evidence and authority constraints can be checked together at the same import
  boundary.
- representative non-authority facts can be checked from the same artifact shape:
  no-allocation, hosted/freestanding, deterministic/nondeterministic, and
  constant-time where those facts exist.

## Relation To Existing Notes

- `research/packages-tooling/authority-budgets.md` covers package/subsystem
  authority ceilings.
- ROADMAP Phase 17 #15 covers evidence-typed imports.
- This note connects them: imports are both evidence boundaries and authority
  boundaries, and eventually general fact boundaries.
