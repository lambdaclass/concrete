# Package Manager Design

Status: open

This note sketches a Concrete package manager that is:

- Cargo-simple on the surface
- Nix-inspired underneath in reproducibility and graph discipline
- Concrete-specific in authority, trust, and evidence

It is intentionally narrower than Cargo and much simpler than Nix.
The goal is not to build a universal package/build language.
The goal is to make Concrete usable for real projects without losing auditability.

## Core Claim

Concrete should not copy Cargo wholesale, and it should definitely not copy the full Nix language.

The right shape is:

- a boring manifest
- a small CLI
- an explicit package/dependency graph artifact
- reproducible inputs and outputs
- authority/evidence integrated from day one

The user-facing shape should feel like one coherent toolchain:

- one binary: `concrete`
- package management as subcommands, not a second executable
- Cargo-like everyday UX
- Concrete-specific authority/report/evidence outputs

## Example-First UX

This design is easiest to explain through normal user flows.

### Create a project

```bash
concrete new verifier
cd verifier
concrete run
```

Generated layout:

```text
verifier/
  Concrete.toml
  src/
    main.con
```

### Build and test

```bash
concrete build
concrete test
concrete run
concrete check
```

### Inspect reports

```bash
concrete report authority
concrete report alloc
concrete graph
```

### Add a dependency

```bash
concrete add sha2 --path ../sha2
```

### Build a release binary

```bash
concrete build --release
```

### Build an evidence bundle later

```bash
concrete build --release --evidence out/
```

This should feel like one system:

- compiler
- package manager
- report engine
- artifact/evidence workflow

## Design Goals

1. Keep the user surface small and obvious.
2. Make builds reproducible and graph-shaped, not shell folklore.
3. Treat packages as authority and trust boundaries, not only dependency buckets.
4. Reuse compiler artifacts instead of rediscovering the world on every build.
5. Leave room for evidence bundles, trust drift, and proof-facing outputs later.

## What To Learn From Existing Package Managers

Concrete should not copy any one package manager whole.
It should take the strongest parts from a few systems that are good for different reasons.

| System | What it contributes | Why it is good |
|---|---|---|
| **Cargo** | familiar commands, obvious project layout, low-friction everyday workflow | it makes ordinary development easy without requiring users to think about the build graph all day |
| **Go modules** | simplicity, small manifest surface, low-ceremony defaults | it proves a package manager can stay boring and still work well for real projects |
| **Nix** | reproducibility, explicit inputs/outputs, graph honesty | it treats builds as something that should be inspectable and repeatable rather than shell folklore |
| **Elm** | ecosystem restraint, compatibility discipline, willingness to say no | it keeps the package ecosystem coherent by refusing complexity that weakens user trust |
| **uv** | workspace coherence, lock/sync discipline, simple project commands | it shows how project state can stay consistent without making the user think about environment internals |
| **Bazel / Buck2** | internal build-graph rigor, cacheability, incremental correctness | they are excellent models for what the package/build engine should look like under the hood, even if their UX is too heavy for Concrete |

### Cargo

Cargo is good because it gets the day-to-day workflow right:

- `build`
- `run`
- `test`
- simple manifests
- obvious project layout

Concrete should copy that surface familiarity.
That is the right level of friction for ordinary users.

What Concrete should not copy:

- feature-flag complexity
- build-script sprawl
- implicit dependency magic

### Go Modules

Go modules are good because they stay small.

They show that a package system can succeed with:

- a minimal manifest
- boring defaults
- straightforward UX

Concrete should copy that bias toward simplicity.
If a package feature needs a long explanation, it is probably too heavy for the MVP.

### Nix

Nix is good because it treats builds honestly.

The most valuable Nix ideas for Concrete are:

- exact inputs matter
- the dependency/build graph should be explicit
- reproducibility is a first-class property
- build outputs should be explainable

Concrete should copy this mindset, not the full Nix language.

That means:

- graph artifacts
- lockfiles
- stable artifact identities
- cache keys tied to real inputs
- evidence bundles that can be reproduced

What Concrete should not copy:

- the Nix language itself
- lazy evaluation complexity
- a second complicated world users must learn just to build code

### Package Management As Trust Infrastructure

Concrete should treat package management as more than dependency download.

It should also eventually be part of:

- package integrity
- publishing/authentication policy
- provenance and attestations
- trust/evidence bundles

In other words, package management should become part of the audit story, not sit outside it.

### Trusted publishing, provenance, and trust ecosystems

Recent package-manager ecosystems increasingly treat publishing as part of the supply-chain trust story, not just as file upload.

Concrete should learn from that direction too.

#### Trusted publishing

Trusted publishing means packages are published from a trusted build identity, usually CI, instead of from a long-lived API token.

So the story becomes:

- not “someone had the secret and uploaded a package”
- but “this exact CI workflow from this exact repo/tag published this artifact”

That is stronger because it ties publishing to a real build process and a verifiable source context.

#### Provenance by default

Provenance means the published artifact carries machine-verifiable information about where it came from.

In Concrete terms, a future published package could be tied to:

- source commit/hash
- compiler version/commit
- dependency graph hash
- target/profile
- report hashes
- evidence bundle hash

That would let users answer:

- what source produced this artifact?
- what toolchain produced it?
- what exact package/dependency graph was used?

#### Why this matters for Concrete

Concrete already cares about:

- auditability
- explicit trust
- explicit authority
- reproducibility
- evidence

So package publication should fit the same philosophy.
Publishing should eventually be part of the evidence story, not outside it.

#### SLSA / Sigstore-style alignment

Concrete does not need to implement those ecosystems wholesale, but it should avoid designing itself into a dead end that cannot interoperate with them later.

The important idea is:

- Concrete should produce evidence/trust bundles that can later map cleanly to broader provenance/attestation systems

That means a published package should eventually be able to carry:

- the package artifact
- reports
- build manifest
- trust/evidence metadata
- later, signatures or attestations over those outputs

So the Concrete story becomes:

- trusted publishing = trusted way to publish
- provenance = verifiable origin of the artifact
- evidence/trust bundle = Concrete-specific review package
- SLSA/Sigstore alignment = ability to integrate with broader supply-chain verification ecosystems later

### Elm

Elm is good because it values ecosystem quality over ecosystem freedom.

Concrete should learn from Elm's willingness to constrain:

- what packages can do
- how compatibility is expressed
- how much complexity is tolerated in the ecosystem

That is directly aligned with Concrete's identity.
It is better to have a smaller, more coherent package world than a larger one full of hidden behavior and accidental complexity.

### uv

`uv` is useful mainly as a UX reference.

What it gets right:

- workspace-first coherence
- shared lockfile expectations
- commands that keep project state synchronized without surprising the user

Concrete should copy the coherence, not the Python-environment complexity.

The important lesson is:

- normal commands should keep project state and lockfile state coherent
- workspaces should feel like one project, not a pile of loosely related packages

### Bazel / Buck2

Bazel and Buck2 are great not because their UX should be copied, but because their internal model is strong.

Concrete should borrow from them:

- explicit build graphs
- incremental correctness
- strong artifact/caching discipline

But keep that mostly under the hood.
Users should feel Cargo/Go simplicity, not Bazel complexity.

## Lockfile Modes

Concrete should not treat the lockfile as just “present” or “absent.”

It should eventually support explicit modes such as:

- `update`
- `refresh`
- `error`
- `off`

The point is to make dependency resolution behavior visible and policy-friendly.

That matters especially for:

- offline or hermetic builds
- CI
- high-integrity review
- evidence/trust bundles

Audit-heavy projects should be able to say:

- do not change dependency resolution during this build
- fail if the lockfile is stale

That is much stronger than silently mutating project state.

## Workspace And Sync Coherence

Concrete should copy the idea that project commands should keep the workspace in a coherent state.

That does not mean copying Python environment management.
It means:

- one workspace lockfile
- consistent graph resolution across members
- commands that do not surprise the user by drifting project state silently

The package manager should feel like:

- if you build, run, or test the workspace, you are doing so against a coherent resolved graph
- if the graph needs to change, that change should be explicit

## Manifest Shape

One package should be described by one `Concrete.toml`.

The stdlib should be treated as a builtin dependency, not as a repo-relative path dependency.
So the intended model is:

- `std = "builtin"`
- or later, implicit std availability in normal package mode if that proves cleaner

What should not survive into the real package model:

```toml
[dependencies]
std = { path = "../../std", version = "0.1.0" }
```

That is an implementation workaround tied to checkout layout, not a real package-system contract.

Example:

```toml
[package]
name = "artifact_verifier"
version = "0.1.0"
edition = "2026"

[targets]
bin = ["src/main.con"]
lib = ["src/lib.con"]

[dependencies]
std = "builtin"
sha2 = { path = "../sha2" }
hex = { registry = "default", version = "0.2.1" }

[authority_budget]
package = ["Alloc", "File"]
forbid = ["Network", "Process"]

[profile.dev]
opt = "O0"

[profile.release]
opt = "O2"
```

The manifest should stay declarative.
No embedded scripting language.
No arbitrary build hooks in the MVP.

### Manifest By Example

```toml
[package]
name = "verifier"
version = "0.1.0"
edition = "2026"

[targets]
bin = ["src/main.con"]
lib = ["src/lib.con"]

[dependencies]
std = "builtin"
sha2 = { path = "../sha2" }
hex = { registry = "default", version = "0.2.1" }

[authority_budget]
package = ["Alloc", "File"]
forbid = ["Network", "Process"]

[profile.dev]
opt = "O0"

[profile.release]
opt = "O2"
```

This should be readable without learning a second programming language.

`std = "builtin"` means:

- the stdlib is always resolved by the toolchain
- projects do not depend on repository-relative std paths
- lockfiles and package graphs should still record the std identity/version even though the manifest does not point at a filesystem path

## CLI Shape

Use one binary: `concrete`.

The first commands should be:

- `concrete build`
- `concrete run`
- `concrete test`
- `concrete check`
- `concrete report`
- `concrete graph`

Later:

- `concrete add`
- `concrete update`
- `concrete audit`

The package manager should not become a second compiler.
It should resolve graphs, manage lockfiles, and invoke the compiler with explicit inputs.

Single-file mode can still exist as a convenience:

```bash
concrete file.con
```

But package/project mode should be the default path for serious code.

## Dependency Sources

The MVP should support only:

1. `builtin`
   - for std and any future toolchain-shipped packages
2. `path`
   - for local packages and workspaces
3. `registry`
   - later, curated remote index

Avoid early support for:

- arbitrary git dependencies everywhere
- build scripts
- package-time code generation
- complex feature unification

## Workspace Model

Keep the first workspace surface simple:

```toml
[workspace]
members = ["packages/*"]
```

The workspace should provide:

- shared lockfile
- shared package graph
- shared authority/trust policy checks
- shared incremental rebuild context

### Workspace UX

```toml
[workspace]
members = ["packages/*"]
```

```bash
concrete build --workspace
concrete test --workspace
concrete graph
```

Example output:

```text
workspace: satellite-stack
  verifier -> sha2
  verifier -> hex
  control_cli -> verifier

authority budgets:
  verifier: [Alloc, File]
  control_cli: [Alloc, File, Console]
```

## Lockfile

Concrete should have a lockfile from the start.

It should pin:

- exact resolved versions
- source identity or hashes
- registry identity
- package graph shape

Later, it may also pin:

- authority/trust metadata
- evidence/report schema versions

This is important because reproducibility is part of Concrete's identity, not an optional afterthought.

## Package Graph Artifact

This is the most important Nix-like idea to steal.

The package manager should produce an explicit graph artifact describing:

- package IDs
- versions
- source roots
- target roots
- resolved dependency edges
- profiles
- authority budgets
- trust/evidence metadata

The compiler should consume this graph instead of reconstructing project structure ad hoc from the filesystem.

This graph artifact should eventually support:

- incremental compilation
- report reuse
- evidence bundles
- trust-drift comparison

## Authority Budgets

This is where Concrete should go beyond Cargo.

Packages should be able to declare not only what they depend on, but what authority they may require at all.

Examples:

- parser package may require no ambient authority
- CLI package may require `File`
- binary may forbid `Process`

Then the build should fail if transitive authority exceeds the declared budget.

This turns capabilities from local function facts into subsystem policy.

### Authority Budget UX

This is the kind of failure Concrete should make easy to read:

```text
error: authority budget violated for package `verifier`
forbidden capability introduced: Network
path: verifier.main -> fetch_manifest -> tcp_read
```

That is a much more Concrete-like package experience than plain dependency resolution.

## Reproducibility: Nix Ideas Worth Copying

Concrete should borrow several ideas from Nix:

### 1. Input-addressed thinking

Track exact build inputs:

- source
- compiler version
- dependency graph
- target/profile

Not necessarily full Nix-style derivations at first, but the same discipline.

### 2. Build as an explicit graph

Builds should be graph nodes with declared dependencies and outputs.

This fits:

- artifact-driven compiler work
- incremental compilation
- evidence/review outputs

### 3. Description separate from execution

Manifest and graph evaluation should be distinct from compilation.

This keeps the package manager simple and auditable.

### 4. Reproducibility as a first-class feature

Concrete should care early about:

- lockfiles
- deterministic graph resolution
- explicit toolchain identity
- comparable outputs

This aligns with later trust bundles and report-first review workflows.

### Reproducibility UX

Ordinary builds:

```bash
concrete build --release
```

Should produce:

```text
build/
  verifier
Concrete.lock
.concrete/
  graph.json
  artifacts/
```

Later evidence-oriented builds:

```bash
concrete build --release --evidence out/
```

Could produce:

```text
out/
  verifier
  reports/authority.json
  reports/alloc.json
  manifest.json
```

## Nix Ideas Not Worth Copying

Concrete should not copy:

- the Nix language itself
- lazy evaluation complexity
- highly abstract package expressions
- “everything is a derivation” maximalism

Concrete needs build honesty, not another complicated programming language.

## Cargo Ideas Worth Copying

Concrete should copy the parts of Cargo that reduce friction:

- simple manifest layout
- obvious package layout conventions
- small command set
- workspace model
- lockfile expectation

But it should avoid Cargo’s more complex edges early:

- feature unification explosion
- build.rs-style arbitrary hooks
- overly implicit dependency magic

## UI/UX Principles

The package manager should be explained mostly through examples, not abstract theory.

The intended experience is:

- **Cargo-like on the surface**
- **Go-like in simplicity**
- **Nix-like in reproducibility discipline**
- **Elm-like in ecosystem restraint**
- **Bazel/Buck2-like underneath in build-graph rigor**

Another way to say it:

- commands should feel ordinary
- manifests should feel boring
- outputs should feel explicit
- graph/artifact behavior should be strict and reproducible
- ecosystem growth should be intentionally constrained

## Other Package Managers Worth Studying

Cargo and Nix are the two most obvious references, but they are not the only useful ones.

### Go modules

Best for:

- simplicity
- low-friction defaults
- small manifest surface
- straightforward everyday UX

Concrete should copy the bias toward boring, obvious workflows.

### Dune / opam

Best for:

- clean separation between package management and build orchestration
- multi-package/project structure
- explicit project/build discipline

Concrete should study these for workspace/build graph clarity.

### pnpm

Best for:

- deterministic installs
- shared package storage
- workspace ergonomics

Concrete does not need pnpm's full model, but its storage/workspace discipline is worth studying.

### Bazel / Buck2

Best for:

- explicit build graphs
- artifact/caching rigor
- large-scale incremental correctness

Concrete should copy the graph discipline, not the user-facing complexity.

### Elm package system

Best for:

- ecosystem restraint
- compatibility discipline
- keeping the package surface coherent

Concrete should study Elm mainly as a warning against package-manager sprawl and weak ecosystem constraints.

### Poetry / uv

Best for:

- onboarding ergonomics
- lockfile expectations
- simple project workflow commands

Useful mostly as a UX reference, not as a deep architectural model.

## Recommended Synthesis

The strongest Concrete-specific blend is probably:

- **Cargo** for surface familiarity
- **Nix** for reproducibility and graph honesty
- **Go modules** for simplicity
- **Bazel/Buck2** for build graph rigor
- **Elm** for ecosystem discipline

That gives Concrete:

- a small user surface
- explicit graph and artifact discipline
- reproducible builds
- room for authority/evidence integration
- resistance to package-system complexity creep

Another way to say it:

- **Cargo** and **Go** should dominate the day-to-day UX
- **Nix** should dominate reproducibility and input/output honesty
- **Elm** should dominate ecosystem restraint and compatibility discipline
- **Bazel/Buck2** should dominate the internal build-graph model, not the user-facing interface

That is a better fit for Concrete than copying any one system whole.

## What To Avoid

If Concrete follows the synthesis above, it should explicitly avoid:

- arbitrary build scripts as a default extension mechanism
- feature-flag explosion and feature unification complexity
- overly permissive dependency sources
- hidden or surprising resolution behavior
- package-manager sprawl that weakens auditability
- a second complex DSL for describing builds

## Bazel / Buck2: What To Steal And What Not To Steal

Bazel and Buck2 are primarily build systems, not package managers in the Cargo sense.

What they are excellent at:

- explicit build dependency graphs
- deterministic rebuild logic
- strong incremental correctness
- cacheability keyed by real inputs
- treating artifacts and targets as first-class build nodes

That makes them valuable to Concrete as architectural inspiration for:

- package graph artifacts
- incremental compilation
- interface/body artifact separation
- reproducible build orchestration

What Concrete should not copy from them:

- heavy user-facing complexity
- large rule languages or build DSLs
- monorepo-first assumptions
- the expectation that ordinary users should think in build-graph internals every day

So the right approach is:

- **Bazel/Buck2 underneath**
  for graph rigor and caching discipline
- **Cargo/Go on the surface**
  for normal developer workflow

## Concrete Package Manager Philosophy

The package manager should feel like:

- simple surface
- strict graph
- disciplined ecosystem

That is the package-management equivalent of Concrete's language philosophy:

- explicit where it matters
- small enough to audit
- strong enough to scale
- hostile to hidden behavior and accidental complexity

## One-Binary Rule

Concrete should use one binary for the compiler, package manager, reports, and later evidence workflow:

- `concrete build`
- `concrete run`
- `concrete test`
- `concrete check`
- `concrete report`
- `concrete graph`
- `concrete new`

Not:

- one compiler binary
- one package-manager binary
- one report binary

Internally these should be separate modules.
Externally they should feel like one coherent toolchain.

## Compiler Boundary

The package manager should own:

- manifest parsing
- dependency resolution
- workspace graph construction
- lockfile management
- invoking the compiler with resolved graph inputs

The compiler should own:

- parsing
- elaboration
- checking
- reports
- proof/evidence artifacts

That separation matters.
Otherwise the package manager becomes a shadow semantic engine.

## MVP Sequence

1. `Concrete.toml`
2. path dependencies
3. workspace support
4. lockfile
5. `build` / `run` / `test` / `check`
6. explicit package graph artifact
7. stdlib/project resolution cleanup
8. authority budgets
9. curated registry
10. trust-drift and evidence integration

## Long-Term Differentiators

The package manager becomes uniquely Concrete when it supports:

- authority budgets
- trust/evidence metadata in the package graph
- machine-readable report outputs per package
- trust-drift diffing across versions
- reproducible trust bundles

That is the real opportunity:
not to out-Cargo Cargo,
but to make package/dependency management part of Concrete's auditability story.

## Open Questions

1. What should “just work” in single-file mode versus requiring a package?
2. How much semver complexity is worth carrying in the first registry design?
3. Should authority budgets start package-wide only, or allow target/subsystem granularity immediately?
4. How much of the package graph should become a stable, serialized public artifact?
5. Should std be modeled as a normal builtin package or as a special case in the resolver?
