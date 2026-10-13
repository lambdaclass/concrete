# Identity separation — ratification draft (R-0482 step 0)

Status: DRAFT FOR RATIFICATION. Nothing here is implemented or normative until the decisions in
section 4 are ratified; the migration (queue row 1) is held until then. Measurements are against
`61b2a261` and the frozen rows in `docs/verification/identity-rows-pre-r0482.tsv`.

## 1. What is wrong today, measured

One `PackageIdentity` (`Concrete/Proof/DefinitionIdentity.lean`) does three jobs. It is
`(declaredName, origin, contentRoot)` where, for a project with a manifest:

- `origin` = hash(name, version, **every dependency module name, std included**);
- `contentRoot` = hash(module names, **every source in the build, std included**), names and
  contents sorted independently.

`DefinitionIdentity` = (package digest, module, declaration, implementation digest) inherits all of
it. Consequences, each pinned by a gate:

| Effect | Measured | Gate |
|---|---|---|
| an unused std edit renames generated symbols | 42 symbols in 7 of 21 packages (all 7 manifestful proof fixtures) | `check_identity_overbinding.sh`, `check_identity_rows.sh` control B |
| editing one function renames a sibling's attestation | `proof_pressure` sibling edit moves `check_nonce`'s package | `check_identity_overbinding.sh` control 2 |
| swapping two modules' bodies does not move the package | names and contents sorted separately | comment in `packageIdentityOf` |
| claim dependency roots move with unrelated content | each root node embeds `DefinitionIdentity.canonical`, hence the package digest | `Concrete/Proof/DependencyRoot.lean` |
| four manifestless producers hash different inputs | `Main.lean` (full source map / invoked file only), `DebugBundle.lean` and `Reduce.lean` (names only) | none yet |

The failure mode is demotion and renaming, never a proof credited to the wrong body.

## 2. The corpus the migration must preserve

`scripts/tools/identity_rows.sh` emits 323 canonical rows; `check_identity_rows.sh` (11 legs)
freezes them. They are associations, not counts: 24 scopes, 86 subject rows, 41 dependency
attestations, 63 generated references, 44 consumer pairings (model ↔ reference, with the holding
declaration), and 65 stored proof links. Package digests appear only as the fixture that produces
them, and the tool refuses unless that correspondence is a bijection. Hostile controls:

- **A.** A model swap passes every count in `IDENTITY-FREEZE` and still moves 4 row lines.
- **B.** An unused std edit (plus the consumer renames it forces today) moves no row.
- **C.** The same edit without those renames is refused.

Facts the definitions must respect:

- **17 of 24 proof fixtures have no manifest.** The manifestless rule decides most of the corpus.
- **Of the 7 with a manifest, 1 declares a version; none declares an origin.** Repository-wide, 159
  manifests declare only `name`/`version`; `app` is declared by 4 test packages and `declared_ok`
  by 2.
- **Legitimate many-to-one rows** (must stay representable):
  - the same `(table, module, decl, impl)` under several scopes: `combineFns` `calls.inc` and
    `calls.combine` in 3 composition fixtures, `calls.dbl` in 2, `cryptoFns` `main.check_nonce` in
    `crypto_verify` and `proof_pressure`;
  - the same `(scope, module, decl, impl)` under two tables: `hmac_sha256.ch` and `sha256_init`
    (`FnTable.empty` and `shaFns`);
  - the same `(module, decl)` with different implementations in different scopes:
    `hmac_sha256.ch`, `constant_time_tag.ct_compare`.

## 3. Proposed definitions

Each identity has one producer, a canonical length-prefixed rendering with its own domain tag, and
a private constructor reached only through a refusing `of?`.

### PackageScopeIdentity — who the package claims to be

- **Binds:** declared origin (publisher/namespace) and declared package name. For a manifestless
  compilation, the rule in D3.
- **Never binds:** version (D2), dependency names, module names, or any content.
- **Domain tag:** `pkgScopeV1`.
- **Sensitivity:** changing name or origin moves every definition scoped by it (overbinding matrix
  row "publisher/origin", deferred until now).
- **Invariance:** source, dependency and std edits leave it unchanged.

### PackageArtifactIdentity — what the package's own code is

- **Binds:** the sorted canonical `(module identity, content digest)` pairs of the package's OWN
  modules. Pairing is mandatory; this fixes the independent-sort limit.
- **Content digest:** over the module's source bytes.
- **Duplicates:** never deduplicated. A duplicate module identity within one package is refused at
  its producer (the bug 074 discipline).
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
  `(table, DefinitionIdentity)`, never by package injectivity. Every many-to-one row in section 2
  stays distinct or legitimately shared.
- **Domain tag:** `defIdV2`. A V1 value is `needs_recheck`; there is no mixed-version fallback.

### Claim dependency root — what this claim actually rests on

- **Owner:** `Concrete/Proof/DependencyRoot.lean`, produced per claim in `Issue.lean`. One producer,
  unchanged.
- **Binds:** the claim's subject digest plus every proof-reachable node (`DefinitionIdentity` and
  digest) and every typed contract, body, trusted or missing edge. This is already per-claim and
  reachable-only, and fails closed on unscoped, unresolved or missing edges.
- **Change from today:** nodes embed `defIdV2`, so roots stop moving with unrelated content.
- **Rule:** it is the ONLY identity that moves when a reachable dependency's content changes. Scope,
  artifact and context are never substituted for it.
- **Open measurement:** how many current claims reach std nodes. A reachable std edit must move
  exactly those roots, and this needs std definitions to carry scoped identity (D4).

## 4. Decisions to ratify

Each has a recommendation; the alternatives are stated so the choice is explicit.

- **D1 — origin.**
  - **Recommended:** add an optional `[package] origin = "<publisher/namespace>"` to
    `Concrete.toml`. If it is absent, the scope uses the explicit sentinel origin `«undeclared»`. The
    report discloses that the scope collides with any same-named undeclared package, and the
    package-evidence linker (R-0440 territory) refuses to compose two undeclared-origin scopes with
    the same name.
  - **Alternative:** refuse scoped evidence without an origin. That is stricter, and the 7 manifestful
    proof fixtures stop minting until they declare one.
- **D2 — version.**
  - **Recommended:** version is NOT part of scope. An upgrade that leaves an implementation unchanged
    keeps its `DefinitionIdentity`, so the proof carries; a changed implementation moves it anyway.
    Version stays visible as artifact/resolution metadata.
  - **Alternative:** scope binds version, and every release re-attests everything.
- **D3 — manifestless scope.**
  - **Recommended:** `«standalone»` plus the package's `PackageArtifactIdentity` digest. A
    manifestless compilation has nothing declared, so its scope is content-addressed, honestly
    labelled, and moves with any of its own edits: today's behaviour for 17 fixtures, now stated as
    the rule. Declaring a manifest is how a package gets a stable scope. The three composition
    fixtures stay three scopes.
  - **Alternative:** one constant standalone scope. That collapses the composition trio's rows into
    one scope and makes every standalone program "the same package".
- **D4 — std.**
  - **Recommended:** std has scope `(origin «concrete-toolchain», name std)`. Its artifact enters each
    dependent's `ResolutionContextIdentity` and nothing else, and std definitions carry ordinary
    `DefinitionIdentity` under the std scope so reachable std edges are scoped closure nodes.
- **D5 — symbol names.**
  - **Recommended:** keep `<table>_<scope8>_<decl>`. The prefix becomes the stable scope digest, so
    the renames stop without a new naming scheme. A prefix collision within one surface refuses
    generation.
- **D6 — one manifestless producer.**
  - **Recommended:** a single function from the resolved module inventory (names and contents)
    replaces the four paths. The name-only debug paths stop deriving identity, or call that function.
- **D7 — migration acceptance (queue row 1, after ratification).**
  - **Typed map:** every frozen row maps old to new, or names its refusal, in one commit together with
    the generator, references, consumers, receipts and tables. Two controls graduate together:
    - `check_identity_rows.sh` control B becomes "an unused std edit renames NO symbol";
    - a reachable definition edit moves exactly the affected claim roots.
  - **Converted gates:** `check_identity_overbinding.sh` control 1 (std moves 0 scopes) and control 2
    (a sibling edit moves 0 scopes for a manifestful package) flip to their positive form.
    `check_identity_freeze.sh` is re-pinned in that same commit.

## 5. Not in this step

The model attestation, exported-contract and partial-linking parts of R-0482 come later (sequencing
steps 4–13). They depend on these identities existing first.
