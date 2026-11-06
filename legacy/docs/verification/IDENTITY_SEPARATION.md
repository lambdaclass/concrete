# Identity separation — ratified definitions (R-0482 step 0)

Status: RATIFIED 2026-10-13 (definitions D1–D7, with the user's amendments) and IMPLEMENTED by the
R-0482 migration (`IDENTITY_MIGRATION_PLAN.md`; decisions P1–P4 accepted 2026-10-13). Section 1
records the pre-migration scheme as measured against baseline `61b2a261`; the old-to-new map is
`identity-migration-map-r0482.tsv`.

## 1. What was wrong before the migration, measured

One `PackageIdentity` (`Concrete/Proof/DefinitionIdentity.lean`) does three jobs. It is
`(declaredName, origin, contentRoot)` where, for a project with a manifest:

- `origin` = hash(name, version, **every dependency module name, std included**);
- `contentRoot` = hash(module names, **every source in the build, std included**), names and contents
  sorted independently.

`DefinitionIdentity` = (package digest, module, declaration, implementation digest) inherits all of it:

| Effect | Measured | Gate |
|---|---|---|
| an unused std edit renames generated symbols | 42 symbols in 7 of 21 packages (all 7 manifestful proof fixtures) | `check_identity_overbinding.sh`, `check_identity_rows.sh` control B |
| editing one function renames a sibling's attestation | `proof_pressure` sibling edit moves `check_nonce`'s package | `check_identity_overbinding.sh` control 2 |
| swapping two modules' bodies does not move the package | names and contents sorted separately | comment in `packageIdentityOf` |
| claim dependency roots move with unrelated content | every root node embeds `DefinitionIdentity.canonical`, hence the package digest | `Concrete/Proof/DependencyRoot.lean` |
| four manifestless producers hash different inputs | `Main.lean` (full source map / invoked file only), `DebugBundle.lean` and `Reduce.lean` (names only) | none yet |

The failure mode is demotion and renaming, never a proof credited to the wrong body.

## 2. The frozen baseline

Two files, produced by `scripts/tools/identity_rows.sh` and held by `check_identity_rows.sh` (19 legs).

**Normalized rows** (`docs/verification/identity-rows-pre-r0482.tsv`, 323 rows):

- 24 scopes, 86 subject rows, 41 dependency attestations, 63 generated references;
- 44 consumer pairings: model ↔ reference, with the holding declaration;
- 65 stored proof links.

Each package digest is replaced by the fixture that produces it. Every tree is held to these rows,
which are exact about associations and immune to the over-binding.

**Raw baseline** (`docs/verification/identity-raw-baseline-pre-r0482.tsv`, tied to `61b2a261`):

- the same rows with every digest byte kept: package, definition and implementation digests, and full
  generated symbol names;
- the migration's exact old-to-new map is written against these bytes;
- the gate checks that the raw rows project exactly onto the normalized rows. It does not hold later
  trees to the raw bytes.

**The fixture ↔ package-digest bijection.** Labelling needs it, and it holds at the baseline. It is not
imposed on migrated identities: under these definitions legitimately shared scopes may break it, and
the migration is judged by its typed map instead.

**Hostile controls:**

- **A.** A model swap passes every count in `IDENTITY-FREEZE` yet moves 4 row lines.
- **B.** An unused std edit, plus the consumer renames it forces today, moves no row.
- **C.** The same edit without those renames is refused.
- **Projection.** A raw reference re-pointed at another package no longer projects onto the frozen
  rows.

**Facts the definitions respect:**

- 17 of 24 proof fixtures have no manifest.
- Of the 7 with a manifest, 1 declares a version and none declares an origin.
- Repository-wide, 159 manifests declare only `name`/`version`. `app` is declared by 4 test packages
  and `declared_ok` by 2.
- **Legitimate sharing that must stay representable:**
  - the same `(table, module, decl, impl)` under several scopes: `combineFns` `calls.inc` and
    `calls.combine` in 3 composition fixtures, `calls.dbl` in 2, and `cryptoFns` `main.check_nonce` in
    `crypto_verify` and `proof_pressure`;
  - the same `(scope, module, decl, impl)` under two tables: `hmac_sha256.ch` and `sha256_init`
    (`FnTable.empty` and `shaFns`);
  - the same `(module, decl)` with different implementations in different scopes: `hmac_sha256.ch` and
    `constant_time_tag.ct_compare`.

## 3. Definitions

Each identity has one producer, a canonical length-prefixed rendering with its own domain tag, and a
private constructor reached only through a refusing `of?`.

### ScopeOrigin — a tagged variant, never a string convention

```text
ScopeOrigin :=
  | declared   (namespace : String)       -- a manifest's `origin`; a namespace CLAIM, not an
                                          --   authenticated publisher identity
  | undeclared                            -- a manifest without `origin`
  | standalone (artifact : PackageArtifactIdentity)  -- no manifest at all (D3)
  | toolchain  (component : String)       -- reserved: only the resolver's bundled std (D4)
```

- **Distinct tags.** Each variant renders under its own tag (`D`, `U`, `S`, `T`), and each payload is
  length-prefixed.
- **The manifest only ever produces `declared`.** A manifest's `origin` value always parses into
  `declared`, whatever text it holds: `origin = "«undeclared»"` or `origin = "concrete-toolchain"` is an
  ordinary declared namespace. It cannot impersonate another variant.
- **`undeclared` and `standalone`** are produced only by the resolver, from the ABSENCE of an origin or
  of a manifest.
- **`toolchain` is produced only when resolving the compiler's bundled std.** A user package named
  `std` gets an ordinary origin variant.

### PackageScopeIdentity — who the package claims to be

- **Binds:** `ScopeOrigin` and the declared package name. Manifestless scope is the `standalone`
  variant.
- **Never binds:** version (D2), dependency names, dependency or std content, or module names.
- **Domain tag:** `pkgScopeV1`.
- **General invariance:** source, dependency and std edits leave scope unchanged.
- **The standalone exception, stated here rather than hidden:** a `standalone` scope contains the
  package's OWN artifact digest. Edits to its own modules may therefore change it; dependency-only
  edits must not.
- **Sensitivity:** changing the declared name or origin moves every definition scoped by it.

### PackageArtifactIdentity — what the package's own code is

- **Binds:** the sorted canonical `(module identity, content digest)` pairs of the package's OWN
  modules. Pairing is mandatory; this fixes the independent-sort limit.
- **Content digest:** over the module's source bytes.
- **Duplicates:** never deduplicated. A duplicate module identity within one package is refused at
  its producer (the bug 074 discipline).
- **Incomplete inventories:** an inventory that is incomplete or unpaired refuses identity production
  (D6).
- **Never binds:** dependencies or std.
- **Domain tag:** `pkgArtifactV1`.

### ResolutionContextIdentity — how the package was interpreted

- **Binds:** the resolved dependency graph, as each dependency's binding name, scope and artifact;
  std enters here as a dependency (D4). It references, but does not duplicate, the toolchain identity
  receipts already bind (`toolchainIdOf`, compiler plus checker).
- **Bound into:** receipts and package evidence bundles.
- **Never bound into:** `DefinitionIdentity`.
- **Domain tag:** `resolutionCtxV1`.

### DefinitionIdentity — which definition, exactly

- **Binds:** scope digest, module, declaration and implementation digest. The implementation digest
  is unchanged: typed signature, generics, capabilities, contracts and canonical body.
- **Change from today:** the package component becomes the scope digest instead of the over-bound
  package digest.
- **Collision rule:** collisions and ambiguity are decided at the complete row key
  `(table, DefinitionIdentity)`, never by package injectivity. Every legitimate-sharing case in
  section 2 stays representable.
- **Domain tag:** `defIdV2`. A V1 value is `needs_recheck`; there is no mixed-version fallback.

### Claim dependency root — the claim's relevant dependency closure

- **Owner:** `Concrete/Proof/DependencyRoot.lean`, produced per claim in `Issue.lean`. One producer,
  unchanged.
- **Binds:** the claim's subject digest plus every proof-reachable node (`DefinitionIdentity` and
  digest) and every typed contract, body, trusted or missing edge. This is already per-claim and
  reachable-only, and fails closed on unscoped, unresolved or missing edges.
- **Change from today:** nodes embed `defIdV2`, so roots stop moving with unrelated content.
- **Rule:** it is the identity that records what the claim rests on. Scope, artifact and context are
  never substituted for it.

**What a reachable edit moves, and what each change requires.** A reachable dependency edit is not
confined to the root:

- the edited dependency's `PackageArtifactIdentity` changes;
- so does every dependent's `ResolutionContextIdentity`;
- and so does the claim's root.

The two kinds of change mean different things:

| Changed | Unchanged | Meaning | Required |
|---|---|---|---|
| resolution context (a dependency artifact, std, toolchain) | claim root | the claim's reachable closure is identical; it was established under another context | re-validation under policy: a fresh re-check and policy decision, as R-0208 does for the checker. No new proof. |
| claim root | — | a definition the claim reaches changed | re-establishing the claim: re-prove or re-replay. The old proof may no longer hold. |

Reports keep these distinct. A context-only change never reads as a broken proof, and a root change
never reads as mere drift.

**Std in roots.** Reachable std definitions are ordinary nodes under the std scope (D4). A reachable
std edit must move exactly the roots of the claims that reach it. How many current claims reach std
nodes is an open measurement for the migration.

## 4. Ratified decisions

- **D1 — origin.**
  - The `origin` field in `Concrete.toml` is optional. A missing origin is the explicit `undeclared`
    variant, and local evidence remains allowed.
  - Ambiguous same-named undeclared packages are refused at every EXISTING resolution and
    evidence-linking boundary (the resolver, attestation and receipt joins), now, not deferred to
    R-0440.
  - A declared origin is a namespace claim, not an authenticated publisher identity, and reports say
    so.
- **D2 — version.**
  - Version is excluded from scope and kept as metadata.
  - An unchanged definition retains its identity across versions. Evidence acceptance still depends on
    its dependencies (claim root, resolution context) and on policy.
- **D3 — manifestless packages.**
  - A distinct `standalone` scope variant contains the package's own artifact digest.
  - Own-content edits may change it; dependency-only edits must not. The exception is stated beside the
    general invariance rule (section 3).
- **D4 — std.**
  - std has its own scope (the reserved `toolchain` origin, produced only for the bundled std) and
    ordinary definition identities.
  - Its artifact contributes to resolution context, and reachable std definitions contribute to claim
    roots.
- **D5 — symbols.**
  - Keep `<table>_<scope8>_<decl>`.
  - Generation refuses a truncated-prefix collision, and any generated-symbol collision between
    distinct full keys.
  - Stability is promised for manifestful packages; standalone scopes remain content-addressed.
- **D6 — manifestless producer.**
  - One producer from PAIRED module names and contents replaces the four paths.
  - An incomplete inventory refuses identity production. The name-only debug paths stop deriving
    identity themselves.
- **D7 — migration (separate deliverable).**
  - A total typed old-to-new mapping against the raw baseline: every row maps or names its refusal.
  - Legitimate sharing is preserved.
  - Producers (generator, references, tables) and consumers (proof selections, receipts, reports)
    migrate in one commit.
  - Unused-edit stability and reachable-edit sensitivity must both pass on that same commit:
    - `check_identity_rows.sh` control B becomes "an unused std edit renames no manifestful symbol";
    - a reachable definition edit moves exactly the affected claim roots, while a context-only edit
      moves resolution context and no root;
    - `check_identity_overbinding.sh` controls 1 and 2 flip to their positive forms;
    - `check_identity_freeze.sh` is re-pinned in that commit.

## 5. Not in this step

The model attestation, exported-contract and partial-linking parts of R-0482 come later (sequencing
steps 4–13). They depend on these identities existing first. The tagged origin and the D1 refusals are
part of the migration, not this step.
