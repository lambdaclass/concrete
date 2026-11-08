### Summary & Merge Resolution

The remerge in `ConcreteSpec.lean` and `ConcreteSpec/Spec.lean` cleanly combines `Prim`, `Ident`, and `Observe` / `Observe2`. Part (0) fixes the round-2 vacuity findings in `Prim.lean` (`SafetyEvidence.Discharges` + `discharge_fact`, `runStmts_terminal` + `restartingRunStmts` mutant, single-field mismatch witnesses, `drive` report fabrications, and `twoTypeContract`), and `Observe2.lean` properly updates `trapLine_injective` to project only the rendered fields (`path`, `line`, `col`).

However, several laws and schema definitions in Part (1) (`Ident.lean` / `Proofs/Ident.lean`) violate the vacuity and long-term schema rules.

---

### Vacuity Audit of Laws in the Diff

1. **`StableSiteKey.stable_renumber` (`ConcreteSpec/Proofs/Ident.lean:13`)**
   - **Verdict**: **VACUOUS (REAL DEFECT)**
   - **Witness / Counter-example**: Instance `site` + `successor` (`Ident.lean:69`); mutant `renumberAndMutate` (`Ident.lean:73`).
   - **Why vacuous**: `StableSiteKey` is just a product `(site : SiteId) × (key : StableKey)`, and `StableSiteKey.renumber` is `{ k with site := ⟨r.apply k.site.id⟩ }`, which never touches `key` or uses `r.injective`. Proving `(k.renumber r).key = k.key` by `rfl` is `(a', b).2 = (a, b).2`. Moreover, `r.injective` is unused in the theorem—a constant map `fun _ => 0` satisfies the exact same theorem. A non-vacuous law for U2 must operate on a site table / lookup map (e.g., looking up the `StableKey` of a renumbered `SiteId` in the renumbered table returns the original `StableKey`, and injectivity ensures no two distinct sites collide in the renumbered table).

2. **`LoadedModules.collision_refuses` (`ConcreteSpec/Proofs/Ident.lean:18`)**
   - **Verdict**: **WEAKENED / VACUOUS (REAL DEFECT)**
   - **Witness / Counter-example**: Instance `[firstModule, collidingModule]` (`Ident.lean:100`); mutant `Coexistence` (`Ident.lean:107`).
   - **Why defective**: If a module has `path = []` (as used in `ConcreteSpec/Spec/Exec.lean:729`: `ModuleId.mk ⟨"exec"⟩ []` and `ConcreteSpec/Spec/Ident.lean:391`: `ModuleId.mk ⟨"p"⟩ []`), then `path.head? = none`. Under `LoadedModules.WellFormed`, two different packages each having a root module `path = []` both have `head? = none`, so `first.path.head? = second.path.head?` holds (`none = none`) and `WellFormed` falsely classifies them as a top-level module name collision! Or, if empty module paths are illegal, `ModuleId` fails to enforce `path ≠ []` (or `TopLevelName`), making `Exec.lean` and `Ident.lean` examples ill-formed. Furthermore, `collision_refuses` is just unpacking the negated definition of `WellFormed`.

3. **`SpecializationRequest.site_independent` (`ConcreteSpec/Proofs/Ident.lean:26`)**
   - **Verdict**: **VACUOUS (REAL DEFECT)**
   - **Witness / Counter-example**: Instance `⟨0⟩` vs `⟨1⟩` (`Ident.lean:122`); mutant `withRequestSite` (`Ident.lean:136`).
   - **Why vacuous**: `SpecializationRequest.for site g tys caps` is defined as `⟨site, ⟨g, tys, caps⟩⟩`, and `.specialization` projects the second field. This is literally `(a₁, b).2 = (a₂, b).2 := rfl`, restating the constructor projection.

4. **`SpecializationId.ne_of_typeArguments_ne` & `SpecializationId.ne_of_capabilityArguments_ne` (`ConcreteSpec/Proofs/Ident.lean:32, 39`)**
   - **Verdict**: **VACUOUS (REAL DEFECT)**
   - **Witness / Counter-example**: Instances `numeric ≠ otherType`, `numeric ≠ otherCapability` (`Ident.lean:128, 131`); mutant `onlyGeneric` (`Ident.lean:143`).
   - **Why vacuous**: Both theorems are generic record-field contrapositive boilerplate (`x.field ≠ y.field → x ≠ y`), true of every Lean `structure` by `congrArg`.

5. **`Span.location_determines_line_col` (`ConcreteSpec/Proofs/Ident.lean:47`)**
   - **Verdict**: **NON-VACUOUS (with minor strengthens possible)**
   - **Witness / Counter-example**: `tabSpan` vs `tabPointSpan` (`Ident.lean:175`); mutant `wrongTabSpan` (`Ident.lean:182`).
   - **Note**: Proves that `LocationOk` functionally determines `line` and `col` from `source` and `startOffset` regardless of `endOffset` or `path`. However, `SourceText.lineAt` and `SourceText.columnAt` lack direct behavioral laws (e.g., that advancing past a non-newline byte `\t` increments `columnAt` by 1 and leaves `lineAt` unchanged, while `\n` increments `lineAt` and resets `columnAt` to 1).

6. **`trapLine_injective` (`ConcreteSpec/Proofs/Observe2.lean:364`)**
   - **Verdict**: **NON-VACUOUS**
   - **Witness / Counter-example**: `Demo.sp` vs `spDifferentRange` (`Observe2.lean:378`); counter-example to full-span equality (`Observe2.lean:385`).

7. **`SafetyEvidence.discharge_fact` (`ConcreteSpec/Proofs/Prim.lean:129`)**
   - **Verdict**: **NON-VACUOUS**
   - **Witness / Counter-example**: `evidence` discharging `obligation` (`Prim.lean:491`); counter-example `mismatchedObligation` (`Prim.lean:497`).

8. **`runStmts_terminal` (`ConcreteSpec/Proofs/Prim.lean:315`)**
   - **Verdict**: **NON-VACUOUS**
   - **Witness / Counter-example**: `returnedStart` and `trappedStart` with `[.abort ⟨4⟩]` (`Prim.lean:733, 738`); counter-example `restartingRunStmts` on `returnedWithLocal` (`Prim.lean:758, 763`).

---

### Detailed Findings

1. **`ConcreteSpec/Proofs/Ident.lean:13` — `StableSiteKey.stable_renumber` is a vacuous projection identity and never uses `r.injective`**
   - **Severity**: Blocker — **REAL DEFECT**
   - **Problem**: `StableSiteKey.renumber` modifies `.site` and leaves `.key` untouched; `(k.renumber r).key = k.key` is `rfl` and holds even if `r.apply` maps every site to `0`. `SiteRenumbering.injective` is dead weight.
   - **Concrete fix**: Define a site table / bijective lookup relation `SiteKeyTable := List StableSiteKey` with a lookup function `lookupKey (t : SiteKeyTable) (s : SiteId) : Option StableKey` and table well-formedness `NoDupSites (t : SiteKeyTable) : Prop` (distinct entries have distinct `SiteId`s). Prove that renumbering the whole table via `r : SiteRenumbering` preserves `NoDupSites` (which genuinely requires `r.injective`) and satisfies `lookupKey (renumberTable r t) ⟨r.apply s.id⟩ = lookupKey t s`.

2. **`ConcreteSpec/Proofs/Ident.lean:26-44` — Specialization laws are trivial projection/contrapositive tautologies**
   - **Severity**: Blocker — **REAL DEFECT**
   - **Problem**: `SpecializationRequest.site_independent`, `ne_of_typeArguments_ne`, and `ne_of_capabilityArguments_ne` just restate structure projections (`rfl` and `congrArg`).
   - **Concrete fix**: Define specialization deduplication / canonicalization over a list of `SpecializationRequest Ty Cap` (e.g., `dedupRequests : List (SpecializationRequest Ty Cap) → List (SpecializationId Ty Cap)`) or a specialization table lookup keyed by `SpecializationId`, and prove that:
     1. Every requested specialization is present in the deduplicated table regardless of request site;
     2. Two requests from distinct sites with identical `(generic, typeArguments, capabilityArguments)` resolve to the exact same entry (table length / membership invariance under site changes);
     3. Two requests differing in `typeArguments` or `capabilityArguments` cannot be coalesced into a single `SpecializationId`.

3. **`ConcreteSpec/Spec/Ident.lean:111-125` — `ModuleCollision` and `LoadedModules.WellFormed` mishandle empty module paths (`head? = none`)**
   - **Severity**: Major — **REAL DEFECT**
   - **Problem**: `first.path.head? = second.path.head?` evaluates to `none = none` (`True`) whenever both modules have `path = []` (which `Spec/Exec.lean:729` and `Spec/Ident.lean:391` actually construct). Either a module path is never empty (every module has a top-level name), or `head? = none` is not a named collision.
   - **Concrete fix**: Either enforce non-empty module paths in `ModuleId` (or require ` ∃ top, first.path.head? = some top ∧ second.path.head? = some top` in `ModuleCollision` and `LoadedModules.WellFormed`), and update the empty-path test fixtures in `Spec/Exec.lean` and `Spec/Ident.lean` to use a non-empty module path. Also add a non-trivial law on `LoadedModules` (e.g., a decidable collision checker `findCollision : List ModuleId → Option ModuleCollision` with soundness and completeness: `findCollision ms = some c → HasCollision ms c` and `findCollision ms = none ↔ WellFormed ms`).

4. **`ConcreteSpec/Spec/Ident.lean:171` — `GenericId` omits trait methods / associated items (`MethodId` or `TraitId`)**
   - **Severity**: Major — **REAL DEFECT**
   - **Problem**: Because the Lean types are the frozen JSON schema source, `GenericId` having only `| function (f : FnId)` and `| declaration (d : DeclId)` means specializing a generic trait or trait method will require either encoding a method as a bare `FnId` (losing its trait/impl owner) or breaking the schema later. Similarly, `StableKey` (`Ident.lean:206`) is hardcoded to `function : FnId`, which cannot represent obligations or `assume` premises inside `const` initializers, `struct` invariants, or top-level/trait items without fabricating fake `FnId`s.
   - **Concrete fix**:
     - For `StableKey`: check whether obligations/sites can occur outside a `FnId` (e.g. `const` blocks or `structInvariant` on `DeclId`, which is already in `ObligationKind.structInvariant`!). Since `ObligationKind.structInvariant` exists on a struct declaration (`DeclId`), an enclosing scope for `StableKey` needs an owner sum type (e.g., `EnclosingId` with `function : FnId`, `declaration : DeclId`, `constant : ConstId`) rather than only `function : FnId`.
     - For `GenericId`: include trait/instance or confirm if trait methods are always lowered to `FnId` or need their own constructor.

5. **`ConcreteSpec/Spec/Ident.lean:226` — `PremiseScope.package` uses a bare `String` (`relative : String`) instead of an opaque token structure**
   - **Severity**: Minor — **REAL DEFECT**
   - **Problem**: `FunctionRelativeKey` and `PackageId` properly wrap their undecided representation in a single-field opaque structure (`token : String`), whereas `PremiseScope.package` uses a bare `(package : PackageId) (relative : String)`. For schema consistency and to honour "never by a bare string" and keeping undecided token contents abstract, package-relative assumption keys should be wrapped in `PackageRelativeKey` (with `token : String`).
   - **Concrete fix**: Define `structure PackageRelativeKey where token : String deriving DecidableEq, Repr` and use `| package (package : PackageId) (relative : PackageRelativeKey)` in `PremiseScope`.

6. **`ConcreteSpec/Spec/Ident.lean:311-320` — Missing direct byte-column laws on `SourceText.lineAt` and `SourceText.columnAt`**
   - **Severity**: Minor — **REAL DEFECT**
   - **Problem**: `Span.location_determines_line_col` only proves that `LocationOk` is functional (`first.line = second.line ∧ first.col = second.col` when `first.startOffset = second.startOffset`), which would still hold even if `lineAt` and `columnAt` were constant `1`! There is no general theorem proving the decided Q10 byte-column rule (that appending a non-`\n` byte—specifically including `\t` (`9`)—advances `columnAt` by `1` and preserves `lineAt`, while appending `\n` (`10`) advances `lineAt` by `1` and resets `columnAt` to `1`).
   - **Concrete fix**: Add and prove the step theorems for `SourceText.lineAt` and `SourceText.columnAt` at `offset = source.length` (or `source ++ [b]`) for `b = 10` (`\n`) and `b ≠ 10` (instantiated at `b = 9`, `\t`), with non-trivial instances and a planted mutant (e.g. a tab-stop column calculator that advances by 4 or 8).

---

### Assessment of the Writer's Three Questions

1. **`FunctionRelativeKey.token` content (writer rec: syntactic path, per-kind ordinal for generated/unnamed)**
   - **Assessment**: Keeping `FunctionRelativeKey` as an opaque `structure FunctionRelativeKey where token : String` in Core right now is **right**—Core must not encode an undecided surface AST path format before the surface/elaboration AST is frozen.
   - For the owner decision on the concrete format: **prefer `(constructKind, per-kind ordinal, optional user label)` or a canonical AST-spine path with per-kind ordinals**, *not* raw child indices. Raw AST child indices break when an unrelated statement is inserted earlier in the same block; keying by `(ObligationKind / construct kind, user label if present, ordinal among same-kind siblings in scope)` survives unrelated statement insertions above/below inside the function body unless the same construct kind is reordered. Also note Finding 4 above: `StableKey` needs an owner type (`EnclosingId`: `FnId | DeclId | ConstId`), not just `FnId`, because `structInvariant` obligations and `const` initializers do not live inside a `FnId`.

2. **`PackageId.token` content (writer rec: name and version)**
   - **Assessment**: Keeping `PackageId` opaque (`token : String`) in `Ident.lean` is **right** for now because OD44 refuses same-top-level-module collisions across packages and R-0440 (multi-version / package coexistence) is still deferred.
   - For the owner decision: **name alone is sufficient until R-0440, or structured `(name, version)` once R-0440 is decided, but do NOT pack `name@version` into an unstructured string once decided.** Because the Lean types generate the JSON schema, once package identity is decided it should either be the package name (if a build graph allows at most one version per package pre-R-0440) or explicit fields `name : String` and `version : ...`. Keeping `token : String` as the abstract placeholder until that row is decided is clean, provided `token` is documented as an opaque placeholder.

3. **How future premise categories are added (writer rec: new constructors only per decided row)**
   - **Assessment**: **Agree completely.** `PremiseKind` must remain a closed inductive enumeration with no `"other : String"` / open-string escape hatch, and new constructors should be added strictly one-for-one as owner decision rows land (bumping `specVersion`). Allowing an open string constructor would reintroduce bare-string premise identities and defeat U12 schema typing.

VERDICT: CHANGES
rc=0
