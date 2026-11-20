import ConcreteSpec.Diagnostics.Types
import ConcreteSpec.Spec.Basics
import ConcreteSpec.Spec.Ident
import ConcreteSpec.Spec.Outcome
import ConcreteSpec.Spec.Rules

/-!
# Laws of typed and persistent identities

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c).
See `ConcreteSpec/README.md`.

## Authority and evidence boundary

Authorities below are current rows in `docs/decisions.md`; historical source approval tags
are not authorities and have been removed.

* `decided-2026-11-08-c9-identities`: structured function-relative keys, same-kind unnamed
  ordinals under the nearest named ancestor, pinned labels, package versions and origin
  collisions. The collection, lookup, edit-invariance and renumbering laws establish the
  executable assignment interface's consequences, rather than choosing a new key policy.
* Q9.1–Q9.2: fresh and nominal identities; OD109: specialization depends on capability
  arguments as well as type arguments. Canonicalization laws prove sharing, separation,
  edge completeness and absence of duplicate entries. OD44 and the C9 row authorize the
  two load-collision cases and refusal rather than coexistence.
* `decided-project-facts-report`: conditional facts retain named premise debt;
  `decided-2026-11-08-c9-identities`: closed, versioned premise categories and refusal of
  unknown categories. Codec laws establish this schema's round trip and exact guards;
  they do not choose a cross-version compatibility policy for future schemas.
* `foundation-q10`: byte columns, with a tab counting one byte;
  `decided-project-runtime-2026-11-06`: one-based source lines and columns. The six
  `SourceText` laws prove arithmetic over bytes, including LF steps, tab and multibyte
  fixtures. They impose no source-admission, BOM or comment-termination policy.

Classification follows docs/decisions.md#decided-2026-11-13-proof-labels.
A `Law` states behavior of an executable subject. A `Helper` is computation, projection,
inversion or proof support and is not independent law evidence. `Instance` examples apply
laws to concrete inputs; `Planted` examples refute named wrong implementations with the
same premises. Representation examples are explicitly not law evidence.

## Extraction disposition

From `origin/wip/spec-integration-head` at `aa44140a3`, retain complete commands from
Proofs/Ident lines 281–1299, 1306–2340 and 2357–2450. These cover every non-path group,
including the byte-only `otherTabSource` support for later F04p. Adapt documentation and
imports to F03 without inheriting approval claims. The numeric local-identity proofs
already live in `Proofs/LocalIdentity`; no spelling-bearing local identity is restored.

The local SourcePath laws replace the held path group under ISS-0068 and R-0709.
They cover the admitted ASCII identity, local checks and exact refusal locations.
Project-wide path admission and source binding are separate obligations. U+FEFF scope
and bare-CR comment termination remain outside the byte-coordinate interface.
-/

namespace ConcreteSpec.Core
/-- Law (subject `ConcreteSpec.Core.SiteKeyTable.renumber`, `ConcreteSpec.Core.SiteKeyTable.NoDupSites`).
**Injective renumbering preserves site-table well-formedness.** No two distinct old sites can
become one new site, so the renamed table still has no duplicate sites. -/
theorem SiteKeyTable.nodup_renumber (r : SiteRenumbering) (table : SiteKeyTable)
    (distinct : table.NoDupSites) : (table.renumber r).NoDupSites := by
  induction table with
  | nil => exact distinct
  | cons entry rest ih =>
    rcases entry with ⟨site, key⟩
    simp only [SiteKeyTable.NoDupSites, SiteKeyTable.renumber, SiteKeyTable.renumberSites,
      List.map_cons, List.nodup_cons] at distinct ⊢
    constructor
    · intro member
      simp only [List.map_map, Function.comp_def] at member
      obtain ⟨oldSite, oldMember, equal⟩ := List.mem_map.1 member
      apply distinct.1
      apply List.mem_map.2
      refine ⟨oldSite, oldMember, ?_⟩
      have ids : oldSite.1.id = site.id := r.injective (congrArg SiteId.id equal)
      rcases oldSite with ⟨oldSite, oldValue⟩
      cases site with
      | mk siteId =>
        cases oldSite with
        | mk oldId => simp_all
    · exact ih distinct.2

/-- Law (subject `ConcreteSpec.Core.SiteKeyTable.renumber`, `ConcreteSpec.Core.AList.lookup`).
**Lookup is equivariant under injective renumbering.** Looking up the renamed site in the
renamed table returns exactly the original persistent key; no entry is merged or lost. -/
theorem SiteKeyTable.lookup_renumber (r : SiteRenumbering) :
    ∀ (table : SiteKeyTable) (site : SiteId),
      AList.lookup (table.renumber r) ⟨r.apply site.id⟩ = AList.lookup table site
  | [], _ => rfl
  | (entrySite, key) :: rest, site => by
    by_cases same : entrySite = site
    · subst site
      simp [SiteKeyTable.renumber, SiteKeyTable.renumberSites, AList.lookup]
    · have renamed : (⟨r.apply entrySite.id⟩ : SiteId) ≠ ⟨r.apply site.id⟩ := by
        intro equal
        apply same
        have ids : entrySite.id = site.id := r.injective (congrArg SiteId.id equal)
        cases entrySite
        cases site
        simp_all
      simp only [SiteKeyTable.renumber, SiteKeyTable.renumberSites, List.map_cons,
        AList.lookup, renamed, same, ↓reduceIte]
      exact SiteKeyTable.lookup_renumber r rest site

/-- Helper (computation). Projects the persistent-key column of `KeyConstruct.numberFunctionKeys`
independently of the starting lexical site number. -/
theorem KeyConstruct.numberFunctionKeys_snd (f : FnId) :
    ∀ (startSite : Nat) (keys : List FunctionRelativeKey),
      List.map Prod.snd (KeyConstruct.numberFunctionKeys f startSite keys) =
        keys.map (fun rel => ⟨.function f, rel⟩)
  | _, [] => rfl
  | startSite, rel :: rest => by
    simp [KeyConstruct.numberFunctionKeys, KeyConstruct.numberFunctionKeys_snd f (startSite + 1) rest]

/-- Helper (computation). Resolves the lexical site numbered for an indexed relative key;
this arithmetic correspondence is used to connect occurrence coverage to actual lookup. -/
theorem KeyConstruct.numberFunctionKeys_lookup (f : FnId) (keys : List FunctionRelativeKey)
    (startSite index : Nat) :
    AList.lookup (numberFunctionKeys f startSite keys) ⟨startSite + index⟩ =
      (keys[index]?).map (fun key => ⟨.function f, key⟩) := by
  induction keys generalizing startSite index with
  | nil => rfl
  | cons key rest ih =>
    cases index with
    | zero => simp [numberFunctionKeys, AList.lookup]
    | succ index =>
      have different : (⟨startSite⟩ : SiteId) ≠ ⟨startSite + (index + 1)⟩ := by
        intro equal
        have ids := congrArg SiteId.id equal
        simp only at ids
        omega
      simp only [numberFunctionKeys, AList.lookup, different, ↓reduceIte,
        List.getElem?_cons_succ]
      have shift : startSite + (index + 1) = (startSite + 1) + index := by omega
      rw [shift]
      exact ih (startSite + 1) index

/-- Helper (computation). Moving past a root extends precisely the preceding scope. -/
theorem KeyConstruct.pathAtIn_succ (scope : List KeyConstruct) (root : KeyConstruct)
    (rest : List KeyConstruct) (index : Nat) (route : Numbers) :
    pathAtIn scope (root :: rest) ((index + 1) :: route) =
      pathAtIn (scope ++ [root]) rest (index :: route) := by
  simp [pathAtIn, List.take_succ_cons, List.append_assoc]

/-- Law (subject `ConcreteSpec.Core.KeyConstruct.collectKeysIn`, `ConcreteSpec.Core.KeyConstruct.pathAtIn`).
Every collected key belongs to a real nonempty occurrence route, including descendants. -/
theorem KeyConstruct.collectKeysIn_sound (scope forest : List KeyConstruct) :
    ∀ (key : FunctionRelativeKey), key ∈ collectKeysIn scope forest →
      ∃ route, route ≠ [] ∧ pathAtIn scope forest route = some key := by
  intro key member
  cases forest with
  | nil => simp [collectKeysIn] at member
  | cons root rest =>
    simp only [collectKeysIn, List.mem_cons, List.mem_append, List.mem_map] at member
    rcases member with equal | children | later
    · subst key
      exact ⟨[0], by decide, by simp [pathAtIn, KeyConstruct.key]⟩
    · obtain ⟨suffix, member, rfl⟩ := children
      obtain ⟨route, _, found⟩ := collectKeysIn_sound
        (root.childScope scope) root.children suffix member
      exact ⟨0 :: route, by simp, by
        rw [pathAtIn]
        simp only [List.getElem?_cons_zero, List.take_zero, List.append_nil, Option.bind_eq_bind, Option.bind_some]
        rw [found]
        rfl⟩
    · obtain ⟨route, nonempty, found⟩ := collectKeysIn_sound (scope ++ [root]) rest key later
      cases route with
      | nil => exact False.elim (nonempty rfl)
      | cons index route =>
        exact ⟨(index + 1) :: route, by simp, by rw [pathAtIn_succ]; exact found⟩
termination_by sizeOf forest
decreasing_by
  all_goals simp_wf
  all_goals cases root <;> simp_all <;> omega

/-- Law (subject `ConcreteSpec.Core.KeyConstruct.collectKeysIn`).
The complete output of collection satisfies occurrence soundness, including every descendant.
The contract is stated over the returned collection so faulty collectors can be tested at the
same closed scope and forest without changing a pointwise membership premise. -/
theorem KeyConstruct.collectKeysIn_collectionSound (scope forest : List KeyConstruct) :
    CollectionSound scope forest (collectKeysIn scope forest) := by
  intro key member
  exact collectKeysIn_sound scope forest key member

/-- Law (subject `ConcreteSpec.Core.KeyConstruct.collectKeysIn`).
Every successfully located occurrence is collected; no nested site can disappear. -/
theorem KeyConstruct.collectKeysIn_complete (scope forest : List KeyConstruct)
    (route : Numbers) (key : FunctionRelativeKey) (nonempty : route ≠ [])
    (found : pathAtIn scope forest route = some key) : key ∈ collectKeysIn scope forest := by
  cases forest with
  | nil => cases route <;> simp_all [pathAtIn]
  | cons root rest =>
    cases route with
    | nil => exact False.elim (nonempty rfl)
    | cons index route =>
      cases index with
      | zero =>
        cases route with
        | nil =>
          simp [pathAtIn] at found
          subst key
          simp [collectKeysIn, KeyConstruct.key]
        | cons index route =>
          rw [pathAtIn] at found
          simp only [List.getElem?_cons_zero, List.take_zero, List.append_nil, Option.bind_eq_bind, Option.bind_some] at found
          cases child : pathAtIn (root.childScope scope) root.children (index :: route) with
          | none => simp [child] at found
          | some suffix =>
            have equal : (⟨root.segment scope :: suffix.path⟩ : FunctionRelativeKey) = key := by
              simpa [child] using found
            subst key
            have member := collectKeysIn_complete (root.childScope scope) root.children
              (index :: route) suffix (by simp) child
            simp only [collectKeysIn, List.mem_cons, List.mem_append, List.mem_map]
            exact Or.inr (Or.inl ⟨suffix, member, rfl⟩)
      | succ index =>
        rw [pathAtIn_succ] at found
        have member := collectKeysIn_complete (scope ++ [root]) rest (index :: route) key
          (by simp) found
        simp only [collectKeysIn, List.mem_cons, List.mem_append]
        exact Or.inr (Or.inr member)
termination_by sizeOf forest
decreasing_by
  all_goals simp_wf
  all_goals cases root <;> simp_all <;> omega

/-- Law (subject `ConcreteSpec.Core.KeyConstruct.assignFunctionTable`).
The assigned table contains precisely the complete paths of real nonempty occurrences. Lexical
site numbering changes neither completeness nor the semantic path associated with an occurrence. -/
theorem KeyConstruct.assignFunctionTable_occurrences (f : FnId) (startSite : Nat)
    (body : List KeyConstruct) (key : FunctionRelativeKey) :
    (⟨.function f, key⟩ : StableKey) ∈ (List.map Prod.snd (assignFunctionTable f startSite body)) ↔
      ∃ route, route ≠ [] ∧ pathAt body route = some key := by
  rw [assignFunctionTable, numberFunctionKeys_snd]
  constructor
  · intro member
    obtain ⟨relative, member, equal⟩ := List.mem_map.1 member
    have same : relative = key := congrArg StableKey.relative equal
    subst relative
    exact collectKeysIn_sound [] body key member
  · rintro ⟨route, nonempty, found⟩
    exact List.mem_map.2 ⟨key, collectKeysIn_complete [] body route key nonempty found, rfl⟩

/-- Law (subject `ConcreteSpec.Core.KeyConstruct.assignFunctionTable`, `ConcreteSpec.Core.AList.lookup`).
Looking up lexical site `startSite + index` in a function's assigned table resolves to the
`index`-th collected occurrence key under `StableOwner.function f`, and every such entry belongs
to a real nonempty occurrence route in `body`. -/
theorem KeyConstruct.assignFunctionTable_lookup (f : FnId) (startSite index : Nat)
    (body : List KeyConstruct) (key : FunctionRelativeKey) :
    AList.lookup (assignFunctionTable f startSite body) ⟨startSite + index⟩ =
        some ⟨.function f, key⟩ ↔
      (collectKeys body)[index]? = some key ∧
        ∃ route, route ≠ [] ∧ pathAt body route = some key := by
  rw [assignFunctionTable, numberFunctionKeys_lookup]
  constructor
  · intro found
    obtain ⟨relative, indexed, equal⟩ := Option.map_eq_some_iff.1 found
    have same : relative = key := congrArg StableKey.relative equal
    subst relative
    have member : key ∈ collectKeys body := List.mem_of_getElem? indexed
    exact ⟨indexed, collectKeysIn_sound [] body key member⟩
  · rintro ⟨indexed, _⟩
    simp [indexed]

/-- Law (subject `ConcreteSpec.Core.KeyConstruct.assignProgramTables`, `ConcreteSpec.Core.OutsideFunctionEdit`).
**Per-function syntactic key assignment satisfies `OutsideFunctionEdit`.** Editing `edited`—even
when that shifts the starting lexical site offsets of every other function—leaves the persistent
key sequence of every `f ≠ edited` unchanged. -/
theorem KeyConstruct.assignProgramTables_outsideEdit
    (bodiesBefore bodiesAfter : FnId → List KeyConstruct)
    (startBefore startAfter : FnId → Nat) (edited : FnId)
    (unchanged : ∀ f, f ≠ edited → bodiesAfter f = bodiesBefore f) :
    OutsideFunctionEdit
      (KeyConstruct.assignProgramTables bodiesBefore startBefore)
      (KeyConstruct.assignProgramTables bodiesAfter startAfter)
      edited := by
  intro f outside
  simp only [KeyConstruct.assignProgramTables, KeyConstruct.assignFunctionTable,
    KeyConstruct.numberFunctionKeys_snd, unchanged f outside]

/-- Law (subject `ConcreteSpec.Core.KeyConstruct.key`, `ConcreteSpec.Core.KeyConstruct.segment`).
**A different-kind insertion does not rekey an existing sibling.** The per-kind ordinal filter
is the actual key-assignment operation, not an invariant rechecked by the theorem. -/
theorem KeyConstruct.key_insert_differentKind (before after : List KeyConstruct)
    (inserted target : KeyConstruct) (different : inserted.kind ≠ target.kind) :
    target.key (before ++ inserted :: after) = target.key (before ++ after) := by
  simp [KeyConstruct.key, KeyConstruct.segment, List.filter_append, different]

/-- Law (subject `ConcreteSpec.Core.KeyConstruct.key`, `ConcreteSpec.Core.KeyConstruct.segment`, `ConcreteSpec.Core.ConstructName.isUnnamed`).
**A named insertion does not consume an unnamed ordinal.** Every existing sibling key is
therefore unchanged, independently of where the named sibling is inserted. -/
theorem KeyConstruct.key_insert_named (before after : List KeyConstruct)
    (label : Name) (kind : ConstructKind) (children : List KeyConstruct) (target : KeyConstruct) :
    target.key (before ++ KeyConstruct.mk kind (.named label) children :: after) =
      target.key (before ++ after) := by
  simp [KeyConstruct.key, KeyConstruct.segment, List.filter_append, ConstructName.isUnnamed]

/-- Helper (projection). Renaming preserves participation in unnamed-sibling ordinals. -/
theorem ConstructName.isUnnamed_rename (constructName : ConstructName) (name : Name) :
    (constructName.rename name).isUnnamed = constructName.isUnnamed := by
  cases constructName <;> rfl

/-- Helper (projection). Pinning or unpinning an unnamed construct's label preserves participation
in unnamed-sibling ordinals. -/
theorem ConstructName.isUnnamed_pin (constructName : ConstructName) (label : Option Name) :
    (constructName.pin label).isUnnamed = constructName.isUnnamed := by
  cases constructName <;> rfl

/-- Helper (congruence). Replacing one preceding construct with another of the same kind and
unnamed status preserves `KeyConstruct.segment`. -/
theorem KeyConstruct.segment_replace_same_unnamed (before after : List KeyConstruct)
    (first second other : KeyConstruct) (sameKind : first.kind = second.kind)
    (sameUnnamed : first.name.isUnnamed = second.name.isUnnamed) :
    other.segment (before ++ first :: after) = other.segment (before ++ second :: after) := by
  by_cases h : second.kind = other.kind ∧ second.name.isUnnamed = true <;>
    simp [KeyConstruct.segment, List.filter_append, sameKind, sameUnnamed, h]

/-- Law (subject `ConcreteSpec.Core.KeyConstruct.key`, `ConcreteSpec.Core.KeyConstruct.rename`).
**Renaming one item leaves every sibling key unchanged.** Renaming preserves the item's kind
and named/unnamed status, so it cannot change any sibling's ordinal. -/
theorem KeyConstruct.key_rename_other (before after : List KeyConstruct)
    (target other : KeyConstruct) (name : Name) :
    other.key (before ++ target.rename name :: after) = other.key (before ++ target :: after) := by
  simp only [KeyConstruct.key]
  rw [KeyConstruct.segment_replace_same_unnamed before after (target.rename name) target other
    rfl (ConstructName.isUnnamed_rename target.name name)]

/-- Law (subject `ConcreteSpec.Core.KeyConstruct.key`, `ConcreteSpec.Core.KeyConstruct.pin`).
**Pinning or unpinning an unnamed item's label leaves every sibling key unchanged.** Pinning
preserves the item's kind and unnamed status, so it still occupies its place in the per-kind
ordinal sequence. -/
theorem KeyConstruct.key_pin_other (before after : List KeyConstruct)
    (target other : KeyConstruct) (label : Option Name) :
    other.key (before ++ target.pin label :: after) = other.key (before ++ target :: after) := by
  simp only [KeyConstruct.key]
  rw [KeyConstruct.segment_replace_same_unnamed before after (target.pin label) target other
    rfl (ConstructName.isUnnamed_pin target.name label)]

/-- Prefix a descendant's relative path by its enclosing item's segment. -/
def KeyConstruct.under (preceding : List KeyConstruct) (construct : KeyConstruct)
    (descendant : FunctionRelativeKey) : FunctionRelativeKey :=
  ⟨construct.segment preceding :: descendant.path⟩

/-- Helper (computation). Computes the renamed root construct's own key for
`KeyConstruct.assignFunctionTable_rename_named`. -/
theorem KeyConstruct.key_rename_named (preceding : List KeyConstruct) (kind : ConstructKind)
    (old : Name) (children : List KeyConstruct) (name : Name) :
    ((KeyConstruct.mk kind (.named old) children).rename name).key preceding =
      ⟨[⟨kind, .named name⟩]⟩ := by
  simp [KeyConstruct.key, KeyConstruct.segment, KeyConstruct.rename, ConstructName.rename]

/-- Helper (computation). Computes the pinned root construct's own key for
`KeyConstruct.assignFunctionTable_pin`. -/
theorem KeyConstruct.key_pin_unnamed (preceding : List KeyConstruct) (kind : ConstructKind)
    (old : Option Name) (children : List KeyConstruct) (label : Name) :
    ((KeyConstruct.mk kind (.unnamed old) children).pin (some label)).key preceding =
      ⟨[⟨kind, .pinned label⟩]⟩ := by
  simp [KeyConstruct.key, KeyConstruct.segment, KeyConstruct.pin, ConstructName.pin]

/-- Helper (congruence). Replacing an in-scope construct with one of the same kind and unnamed
status leaves `KeyConstruct.pathAtIn` unchanged. -/
theorem KeyConstruct.pathAtIn_replace_same_unnamed (forest : List KeyConstruct)
    (route : Numbers) (before after : List KeyConstruct) (first second : KeyConstruct)
    (sameKind : first.kind = second.kind)
    (sameUnnamed : first.name.isUnnamed = second.name.isUnnamed) :
    KeyConstruct.pathAtIn (before ++ first :: after) forest route =
      KeyConstruct.pathAtIn (before ++ second :: after) forest route := by
  induction route generalizing forest before after with
  | nil => rfl
  | cons index rest ih =>
    unfold KeyConstruct.pathAtIn
    cases hget : forest[index]? with
    | none => rfl
    | some construct =>
      simp only [List.append_assoc, List.cons_append]
      have hseg :=
        KeyConstruct.segment_replace_same_unnamed before (after ++ forest.take index)
          first second construct sameKind sameUnnamed
      cases hname : construct.name with
      | named _ =>
        simp [KeyConstruct.childScope, hname, hseg]
      | unnamed _ =>
        have hrec := ih construct.children before (after ++ forest.take index)
        simp [KeyConstruct.childScope, hname, hseg, hrec]

/-- Helper (computation). Inserting a named construct in the inherited scope leaves
`KeyConstruct.pathAtIn` unchanged. -/
theorem KeyConstruct.pathAtIn_insert_named (forest : List KeyConstruct) (route : Numbers)
    (before after : List KeyConstruct) (label : Name) (kind : ConstructKind)
    (children : List KeyConstruct) :
    KeyConstruct.pathAtIn
        (before ++ KeyConstruct.mk kind (.named label) children :: after) forest route =
      KeyConstruct.pathAtIn (before ++ after) forest route := by
  induction route generalizing forest before after with
  | nil => rfl
  | cons index rest ih =>
    unfold KeyConstruct.pathAtIn
    cases hget : forest[index]? with
    | none => rfl
    | some construct =>
      simp only [List.append_assoc, List.cons_append]
      have hseg :
          construct.segment
            (before ++ KeyConstruct.mk kind (.named label) children :: (after ++ forest.take index)) =
          construct.segment (before ++ (after ++ forest.take index)) := by
        simp [KeyConstruct.segment, List.filter_append, ConstructName.isUnnamed]
      cases hname : construct.name with
      | named _ =>
        simp [KeyConstruct.childScope, hname, hseg]
      | unnamed _ =>
        have hrec := ih construct.children before (after ++ forest.take index)
        simp [KeyConstruct.childScope, hname, hseg, hrec]

/-- Helper (computation). Inserting a construct whose kind is avoided along `route` in the
enclosing named scope leaves `KeyConstruct.pathAtIn` unchanged. -/
theorem KeyConstruct.pathAtIn_insert_differentKind (forest : List KeyConstruct) (route : Numbers)
    (before after : List KeyConstruct) (inserted : KeyConstruct)
    (avoids : KeyConstruct.RouteAvoidsKind inserted.kind forest route) :
    KeyConstruct.pathAtIn (before ++ inserted :: after) forest route =
      KeyConstruct.pathAtIn (before ++ after) forest route := by
  induction route generalizing forest before after with
  | nil => rfl
  | cons index rest ih =>
    unfold KeyConstruct.pathAtIn
    cases hget : forest[index]? with
    | none => rfl
    | some construct =>
      simp only [KeyConstruct.RouteAvoidsKind, KeyConstruct.routeAvoidsKind, hget,
        Bool.and_eq_true, bne_iff_ne] at avoids
      rcases avoids with ⟨hne, hchild⟩
      simp only [List.append_assoc, List.cons_append]
      have hseg :
          construct.segment (before ++ inserted :: (after ++ forest.take index)) =
          construct.segment (before ++ (after ++ forest.take index)) := by
        simp [KeyConstruct.segment, List.filter_append, hne]
      cases hname : construct.name with
      | named _ =>
        simp [KeyConstruct.childScope, hname, hseg]
      | unnamed _ =>
        rw [hname] at hchild
        have hrec := ih construct.children before (after ++ forest.take index) hchild
        simp [KeyConstruct.childScope, hname, hseg, hrec]

/-- Helper (computation). Unfolds `KeyConstruct.pathAt` at the exact index `before.length` of a
split forest `before ++ target :: tail`. -/
theorem KeyConstruct.pathAt_at_length (before tail : List KeyConstruct)
    (target : KeyConstruct) (route : Numbers) :
    KeyConstruct.pathAt (before ++ target :: tail) (before.length :: route) =
      target.descendantKey before route := by
  have htake : (before ++ target :: tail).take before.length = before := by
    rw [List.take_append_of_le_length (Nat.le_refl _), List.take_length]
  simp [KeyConstruct.pathAt, KeyConstruct.pathAtIn, htake,
    KeyConstruct.descendantKey, Option.map_eq_bind, Function.comp_def]

/-- Law (subject `ConcreteSpec.Core.KeyConstruct.descendantKey`, `ConcreteSpec.Core.KeyConstruct.SubtreeAvoidsKind`).
Inserting a sibling whose kind is avoided along `route` in the enclosing named scope preserves the
complete assigned descendant path, including through unnamed intermediate constructs. -/
theorem KeyConstruct.descendant_insert_differentKind (before after : List KeyConstruct)
    (inserted target : KeyConstruct) (route : Numbers)
    (avoids : KeyConstruct.SubtreeAvoidsKind inserted.kind target route) :
    target.descendantKey (before ++ inserted :: after) route =
      target.descendantKey (before ++ after) route := by
  simp only [KeyConstruct.SubtreeAvoidsKind, KeyConstruct.subtreeAvoidsKind,
    Bool.and_eq_true, bne_iff_ne] at avoids
  rcases avoids with ⟨hne, hchild⟩
  have hseg : target.segment (before ++ inserted :: after) = target.segment (before ++ after) := by
    simp [KeyConstruct.segment, List.filter_append, hne]
  cases hname : target.name with
  | named _ =>
    simp [KeyConstruct.descendantKey, KeyConstruct.childScope, hname, hseg]
  | unnamed _ =>
    rw [hname] at hchild
    have hrec := KeyConstruct.pathAtIn_insert_differentKind target.children route before after inserted hchild
    simp [KeyConstruct.descendantKey, KeyConstruct.childScope, hname, hseg, hrec]

/-- Law (subject `ConcreteSpec.Core.KeyConstruct.descendantKey`, `ConcreteSpec.Core.ConstructName.isUnnamed`).
Inserting a named sibling preserves complete descendant paths, not just root segments. -/
theorem KeyConstruct.descendant_insert_named (before after : List KeyConstruct)
    (label : Name) (kind : ConstructKind) (children : List KeyConstruct)
    (target : KeyConstruct) (route : Numbers) :
    target.descendantKey (before ++ KeyConstruct.mk kind (.named label) children :: after) route =
      target.descendantKey (before ++ after) route := by
  have hseg :
      target.segment (before ++ KeyConstruct.mk kind (.named label) children :: after) =
      target.segment (before ++ after) := by
    simp [KeyConstruct.segment, List.filter_append, ConstructName.isUnnamed]
  cases hname : target.name with
  | named _ =>
    simp [KeyConstruct.descendantKey, KeyConstruct.childScope, hname, hseg]
  | unnamed _ =>
    have hrec := KeyConstruct.pathAtIn_insert_named target.children route before after label kind children
    simp [KeyConstruct.descendantKey, KeyConstruct.childScope, hname, hseg, hrec]

/-- Law (subject `ConcreteSpec.Core.KeyConstruct.pathAt`).
**Inserting a sibling after a construct in a forest leaves that construct's and its descendants'
assigned paths unchanged.** Only siblings before `before.length` enter `target`'s scope. -/
theorem KeyConstruct.pathAt_insert_after (before mid after : List KeyConstruct)
    (target inserted : KeyConstruct) (route : Numbers) :
    KeyConstruct.pathAt (before ++ target :: (mid ++ inserted :: after)) (before.length :: route) =
      KeyConstruct.pathAt (before ++ target :: (mid ++ after)) (before.length :: route) := by
  rw [KeyConstruct.pathAt_at_length, KeyConstruct.pathAt_at_length]

/-- Law (subject `ConcreteSpec.Core.KeyConstruct.pathAt`, `ConcreteSpec.Core.ConstructName.isUnnamed`).
**Inserting a named sibling before `target` in a forest shifts `target`'s raw child index without
changing its assigned path.** -/
theorem KeyConstruct.pathAt_insert_named (before mid after : List KeyConstruct)
    (label : Name) (kind : ConstructKind) (children : List KeyConstruct)
    (target : KeyConstruct) (route : Numbers) :
    KeyConstruct.pathAt
        ((before ++ KeyConstruct.mk kind (.named label) children :: mid) ++ target :: after)
        ((before ++ KeyConstruct.mk kind (.named label) children :: mid).length :: route) =
      KeyConstruct.pathAt
        ((before ++ mid) ++ target :: after)
        ((before ++ mid).length :: route) := by
  rw [KeyConstruct.pathAt_at_length, KeyConstruct.pathAt_at_length]
  exact KeyConstruct.descendant_insert_named before mid label kind children target route

/-- Law (subject `ConcreteSpec.Core.KeyConstruct.pathAt`, `ConcreteSpec.Core.KeyConstruct.SubtreeAvoidsKind`).
**Inserting a different-kind sibling before `target` in a forest shifts `target`'s raw child index
without changing its assigned path.** -/
theorem KeyConstruct.pathAt_insert_differentKind (before mid after : List KeyConstruct)
    (inserted target : KeyConstruct) (route : Numbers)
    (avoids : KeyConstruct.SubtreeAvoidsKind inserted.kind target route) :
    KeyConstruct.pathAt
        ((before ++ inserted :: mid) ++ target :: after)
        ((before ++ inserted :: mid).length :: route) =
      KeyConstruct.pathAt
        ((before ++ mid) ++ target :: after)
        ((before ++ mid).length :: route) := by
  rw [KeyConstruct.pathAt_at_length, KeyConstruct.pathAt_at_length]
  exact KeyConstruct.descendant_insert_differentKind before mid inserted target route avoids

/-- Helper (computation). Splits collection at a forest boundary, carrying all preceding roots
into the suffix scope. Used to isolate actual subtree entries in the assigned table. -/
theorem KeyConstruct.collectKeysIn_append (scope before after : List KeyConstruct) :
    collectKeysIn scope (before ++ after) =
      collectKeysIn scope before ++ collectKeysIn (scope ++ before) after := by
  induction before generalizing scope with
  | nil => simp [collectKeysIn]
  | cons root rest ih =>
    simp only [List.cons_append, collectKeysIn, ih]
    simp [List.append_assoc]

/-- Law (subject `ConcreteSpec.Core.KeyConstruct.assignFunctionTable`).
Replacing the outer forest of a named construct preserves its entire ordered assigned subtree:
root, all descendants, and multiplicities. The slice starts after the collected preceding forest
and ends before the following forest; arbitrary lexical site offsets are erased only after the
actual table has been assigned. No occurrence-validity or admission premise is required. -/
theorem KeyConstruct.assignFunctionTable_named_scope (f : FnId) (start₁ start₂ : Nat)
    (before₁ after₁ before₂ after₂ : List KeyConstruct)
    (kind : ConstructKind) (name : Name) (children : List KeyConstruct) :
    ((List.map Prod.snd (assignFunctionTable f start₁
      (before₁ ++ KeyConstruct.mk kind (.named name) children :: after₁))).drop
        (collectKeys before₁).length).take
          (collectKeys [KeyConstruct.mk kind (.named name) children]).length =
    ((List.map Prod.snd (assignFunctionTable f start₂
      (before₂ ++ KeyConstruct.mk kind (.named name) children :: after₂))).drop
        (collectKeys before₂).length).take
          (collectKeys [KeyConstruct.mk kind (.named name) children]).length := by
  simp only [assignFunctionTable, numberFunctionKeys_snd, collectKeys,
    collectKeysIn_append, List.map_append]
  simp only [← List.length_map (f := fun rel : FunctionRelativeKey =>
    (⟨.function f, rel⟩ : StableKey)), List.drop_left]
  simp [collectKeysIn, KeyConstruct.key, KeyConstruct.segment, KeyConstruct.childScope,
    List.map_append, List.take_succ_cons]

/-- Helper (computation). Computes the scope reset of a named root for
`KeyConstruct.pathAt_named_ancestor_resets_scope`. -/
theorem KeyConstruct.descendant_named_ancestor_resets_scope
    (preceding₁ preceding₂ : List KeyConstruct) (kind : ConstructKind) (name : Name)
    (children : List KeyConstruct) (route : Numbers) :
    (KeyConstruct.mk kind (.named name) children).descendantKey preceding₁ route =
      (KeyConstruct.mk kind (.named name) children).descendantKey preceding₂ route := by
  simp [KeyConstruct.descendantKey, KeyConstruct.childScope, KeyConstruct.segment]

/-- Helper (computation). Transports the named-root scope equation through forest locators.
This is not assignment soundness; `KeyConstruct.assignFunctionTable_named_scope` establishes
preservation of the complete assigned subtree. -/
theorem KeyConstruct.pathAt_named_ancestor_resets_scope
    (before₁ after₁ before₂ after₂ : List KeyConstruct)
    (kind : ConstructKind) (name : Name) (children : List KeyConstruct) (route : Numbers) :
    KeyConstruct.pathAt
        (before₁ ++ KeyConstruct.mk kind (.named name) children :: after₁)
        (before₁.length :: route) =
      KeyConstruct.pathAt
        (before₂ ++ KeyConstruct.mk kind (.named name) children :: after₂)
        (before₂.length :: route) := by
  rw [KeyConstruct.pathAt_at_length, KeyConstruct.pathAt_at_length]
  exact KeyConstruct.descendant_named_ancestor_resets_scope before₁ before₂ kind name children route

/-- Law (subject `ConcreteSpec.Core.KeyConstruct.descendantKey`, `ConcreteSpec.Core.KeyConstruct.rename`).
Renaming a sibling cannot change the complete assigned path of any descendant of another item. -/
theorem KeyConstruct.descendant_rename_other (before after : List KeyConstruct)
    (target other : KeyConstruct) (name : Name) (route : Numbers) :
    other.descendantKey (before ++ target.rename name :: after) route =
      other.descendantKey (before ++ target :: after) route := by
  have hseg := KeyConstruct.segment_replace_same_unnamed before after (target.rename name) target other
    rfl (ConstructName.isUnnamed_rename target.name name)
  cases hname : other.name with
  | named _ =>
    simp [KeyConstruct.descendantKey, KeyConstruct.childScope, hname, hseg]
  | unnamed _ =>
    have hrec := KeyConstruct.pathAtIn_replace_same_unnamed other.children route before after
      (target.rename name) target rfl (ConstructName.isUnnamed_rename target.name name)
    simp [KeyConstruct.descendantKey, KeyConstruct.childScope, hname, hseg, hrec]

/-- Law (subject `ConcreteSpec.Core.KeyConstruct.descendantKey`, `ConcreteSpec.Core.KeyConstruct.pin`).
Pinning or unpinning a sibling's label cannot change the complete assigned path of any descendant
of another item. -/
theorem KeyConstruct.descendant_pin_other (before after : List KeyConstruct)
    (target other : KeyConstruct) (label : Option Name) (route : Numbers) :
    other.descendantKey (before ++ target.pin label :: after) route =
      other.descendantKey (before ++ target :: after) route := by
  have hseg := KeyConstruct.segment_replace_same_unnamed before after (target.pin label) target other
    rfl (ConstructName.isUnnamed_pin target.name label)
  cases hname : other.name with
  | named _ =>
    simp [KeyConstruct.descendantKey, KeyConstruct.childScope, hname, hseg]
  | unnamed _ =>
    have hrec := KeyConstruct.pathAtIn_replace_same_unnamed other.children route before after
      (target.pin label) target rfl (ConstructName.isUnnamed_pin target.name label)
    simp [KeyConstruct.descendantKey, KeyConstruct.childScope, hname, hseg, hrec]

/-- Helper (computation). Computes the renamed prefix over a descendant route for
`KeyConstruct.assignFunctionTable_rename_named`. -/
theorem KeyConstruct.descendant_rename_named (preceding : List KeyConstruct)
    (kind : ConstructKind) (old name : Name) (children : List KeyConstruct) (route : Numbers) :
    ((KeyConstruct.mk kind (.named old) children).rename name).descendantKey preceding route =
      (KeyConstruct.pathAt children route).map (fun suffix =>
        ⟨⟨kind, .named name⟩ :: suffix.path⟩) := by
  simp [KeyConstruct.descendantKey, KeyConstruct.rename, ConstructName.rename,
    KeyConstruct.segment, KeyConstruct.childScope, KeyConstruct.pathAt]

/-- Helper (computation). Transports a located descendant through the occurrence/table
correspondence; supplies no independent soundness evidence. -/
theorem KeyConstruct.assignFunctionTable_descendant_mem (f : FnId) (startSite : Nat)
    (before after : List KeyConstruct) (root : KeyConstruct) (route : Numbers)
    (key : FunctionRelativeKey) (found : root.descendantKey before route = some key) :
    (⟨.function f, key⟩ : StableKey) ∈
      (List.map Prod.snd (assignFunctionTable f startSite (before ++ root :: after))) := by
  apply (assignFunctionTable_occurrences f startSite (before ++ root :: after) key).mpr
  refine ⟨before.length :: route, by simp, ?_⟩
  rw [pathAt_at_length]
  exact found

/-- Law (subject `ConcreteSpec.Core.KeyConstruct.assignFunctionTable`, `ConcreteSpec.Core.KeyConstruct.rename`).
Renaming a named root assigns the new name to the construct's own table entry and the new prefix to
every actual descendant. Its suffix is resolved in a fresh named scope, regardless of preceding
siblings of any kind. Sibling preservation is proved by `KeyConstruct.descendant_rename_other` and
the occurrence/table correspondence. -/
theorem KeyConstruct.assignFunctionTable_rename_named (f : FnId) (startSite : Nat)
    (before after : List KeyConstruct) (kind : ConstructKind) (old name : Name)
    (children : List KeyConstruct) (route : Numbers) (suffix : FunctionRelativeKey)
    (found : pathAt children route = some suffix) :
    (⟨.function f, ⟨[⟨kind, .named name⟩]⟩⟩ : StableKey) ∈
      (List.map Prod.snd (assignFunctionTable f startSite
        (before ++ (KeyConstruct.mk kind (.named old) children).rename name :: after))) ∧
    (⟨.function f, ⟨⟨kind, .named name⟩ :: suffix.path⟩⟩ : StableKey) ∈
      (List.map Prod.snd (assignFunctionTable f startSite
        (before ++ (KeyConstruct.mk kind (.named old) children).rename name :: after))) := by
  constructor
  · rw [← key_rename_named before kind old children name]
    apply assignFunctionTable_descendant_mem f startSite before after _ []
    rfl
  · apply assignFunctionTable_descendant_mem f startSite before after _ route
    rw [descendant_rename_named, found]
    rfl

/-- Law (subject `ConcreteSpec.Core.KeyConstruct.assignFunctionTable`, `ConcreteSpec.Core.KeyConstruct.pin`).
Pinning an unnamed root assigns its label to the construct's own table entry and throughout its
actual subtree while preserving the inherited named scope used to resolve the suffix. -/
theorem KeyConstruct.assignFunctionTable_pin (f : FnId) (startSite : Nat)
    (before after : List KeyConstruct) (kind : ConstructKind) (old : Option Name)
    (label : Name) (children : List KeyConstruct) (route : Numbers) (suffix : FunctionRelativeKey)
    (found : pathAtIn before children route = some suffix) :
    (⟨.function f, ⟨[⟨kind, .pinned label⟩]⟩⟩ : StableKey) ∈
      (List.map Prod.snd (assignFunctionTable f startSite
        (before ++ (KeyConstruct.mk kind (.unnamed old) children).pin (some label) :: after))) ∧
    (⟨.function f, ⟨⟨kind, .pinned label⟩ :: suffix.path⟩⟩ : StableKey) ∈
      (List.map Prod.snd (assignFunctionTable f startSite
        (before ++ (KeyConstruct.mk kind (.unnamed old) children).pin (some label) :: after))) := by
  constructor
  · rw [← key_pin_unnamed before kind old children label]
    apply assignFunctionTable_descendant_mem f startSite before after _ []
    rfl
  · apply assignFunctionTable_descendant_mem f startSite before after _ route
    simp only [descendantKey, pin, ConstructName.pin, childScope, segment]
    rw [found]
    rfl

/-- Helper (projection). A successful table lookup is in bounds. -/
theorem SpecializationTable.entryAt_lt_length {Ty Cap : Type}
    {entries : List (SpecializationId Ty Cap)} {index : Nat} {requested : SpecializationId Ty Cap}
    (present : entryAt entries index = some requested) : index < entries.length :=
  (List.getElem?_eq_some_iff.1 present).1

/-- Helper (projection). A successful entry search resolves to the requested specialization. -/
theorem SpecializationTable.entryIndex_sound {Ty Cap : Type} [DecidableEq Ty] [DecidableEq Cap] :
    ∀ (entries : List (SpecializationId Ty Cap)) (requested : SpecializationId Ty Cap) (index : Nat),
      entryIndex entries requested = some index → entryAt entries index = some requested
  | [], requested, index, found => by simp [entryIndex] at found
  | entry :: rest, requested, index, found => by
    by_cases same : entry = requested
    · subst requested
      simp [entryIndex] at found
      subst index
      rfl
    · simp only [entryIndex, same, ↓reduceIte, Option.map_eq_some_iff] at found
      obtain ⟨old, oldIndex, equal⟩ := found
      subst index
      exact entryIndex_sound rest requested old oldIndex

/-- Helper (projection). Interning resolves its requested specialization at the returned index. -/
theorem SpecializationTable.intern_sound {Ty Cap : Type} [DecidableEq Ty] [DecidableEq Cap]
    (entries : List (SpecializationId Ty Cap)) (requested : SpecializationId Ty Cap) :
    entryAt (intern entries requested).1 (intern entries requested).2 = some requested := by
  unfold intern
  cases found : entryIndex entries requested with
  | none => simp [entryAt]
  | some index => exact entryIndex_sound entries requested index found

/-- Helper (projection). Interning preserves every old numeric entry. -/
theorem SpecializationTable.intern_preserves {Ty Cap : Type} [DecidableEq Ty] [DecidableEq Cap]
    (entries : List (SpecializationId Ty Cap)) (requested : SpecializationId Ty Cap)
    (index : Nat) (old : SpecializationId Ty Cap)
    (present : entryAt entries index = some old) :
    entryAt (intern entries requested).1 index = some old := by
  unfold intern
  cases found : entryIndex entries requested with
  | some existing => exact present
  | none =>
    have bounded : index < entries.length := entryAt_lt_length present
    unfold entryAt at present ⊢
    rw [List.getElem?_append_left bounded]
    exact present

/-- Helper (projection). A failed entry search really means the specialization is absent. -/
theorem SpecializationTable.entryIndex_none_not_mem {Ty Cap : Type} [DecidableEq Ty]
    [DecidableEq Cap] : ∀ (entries : List (SpecializationId Ty Cap))
    (requested : SpecializationId Ty Cap), entryIndex entries requested = none → requested ∉ entries
  | [], requested, found => by simp
  | entry :: rest, requested, found => by
    by_cases same : entry = requested
    · subst requested
      simp [entryIndex] at found
    · simp only [entryIndex, same, ↓reduceIte, Option.map_eq_none_iff] at found
      simp only [List.mem_cons, not_or]
      exact ⟨Ne.symm same, entryIndex_none_not_mem rest requested found⟩

/-- Law (subject `ConcreteSpec.Core.SpecializationTable.canonicalize`).
Canonicalization never creates duplicate specialization entries. -/
theorem SpecializationTable.canonicalize_nodup {Ty Cap : Type} [DecidableEq Ty] [DecidableEq Cap] :
    ∀ requests : List (SpecializationRequest Ty Cap), (canonicalize requests).entries.Nodup
  | [] => by simp [canonicalize]
  | request :: rest => by
    have tail := canonicalize_nodup rest
    simp only [canonicalize]
    unfold intern
    cases found : entryIndex (canonicalize rest).entries request.specialization with
    | some index => simpa [found] using tail
    | none =>
      simp only [List.nodup_append]
      refine ⟨tail, by simp, ?_⟩
      intro old oldMember new newMember
      simp only [List.mem_singleton] at newMember
      subst new
      intro equal
      subst old
      exact entryIndex_none_not_mem (canonicalize rest).entries request.specialization found oldMember

/-- Law (subject `ConcreteSpec.Core.SpecializationTable.canonicalize`, `ConcreteSpec.Core.SpecializationTable.entryAt`).
**Every canonical request edge resolves to its own specialization.** Edges inherited from the
tail remain valid whether the new head reuses an entry or appends one. -/
theorem SpecializationTable.canonicalize_edge_sound {Ty Cap : Type} [DecidableEq Ty]
    [DecidableEq Cap] : ∀ (requests : List (SpecializationRequest Ty Cap))
    (edge : SpecializationEdge Ty Cap), edge ∈ (canonicalize requests).edges →
      entryAt (canonicalize requests).entries edge.entry = some edge.request.specialization
  | [], edge, member => by simp [canonicalize] at member
  | request :: rest, edge, member => by
    simp only [canonicalize, List.mem_cons] at member
    cases member with
    | inl head =>
      subst edge
      exact intern_sound (canonicalize rest).entries request.specialization
    | inr tail =>
      exact intern_preserves (canonicalize rest).entries request.specialization edge.entry
        edge.request.specialization (canonicalize_edge_sound rest edge tail)

/-- Law (subject `ConcreteSpec.Core.SpecializationTable.canonicalize`, `ConcreteSpec.Core.SpecializationTable.entryAt`).
**Every request keeps an edge.** No request is dropped during canonicalization, and its edge
resolves to the requested entry. -/
theorem SpecializationTable.canonicalize_complete {Ty Cap : Type} [DecidableEq Ty]
    [DecidableEq Cap] : ∀ (requests : List (SpecializationRequest Ty Cap))
    (request : SpecializationRequest Ty Cap), request ∈ requests →
      ∃ edge ∈ (canonicalize requests).edges, edge.request = request ∧
        entryAt (canonicalize requests).entries edge.entry = some request.specialization
  | [], request, member => by simp at member
  | head :: rest, request, member => by
    simp only [List.mem_cons] at member
    cases member with
    | inl equal =>
      subst request
      let assigned := intern (canonicalize rest).entries head.specialization
      exact ⟨⟨head, assigned.2⟩, by simp [canonicalize, assigned], rfl,
        intern_sound (canonicalize rest).entries head.specialization⟩
    | inr tail =>
      obtain ⟨edge, kept, identity, resolved⟩ := canonicalize_complete rest request tail
      refine ⟨edge, ?_, identity, intern_preserves (canonicalize rest).entries head.specialization
        edge.entry request.specialization resolved⟩
      simp only [canonicalize, List.mem_cons]
      exact Or.inr kept

/-- Helper (projection). Two numeric entries holding one specialization in a duplicate-free table
are one entry. -/
theorem SpecializationTable.entry_unique {Ty Cap : Type}
    {entries : List (SpecializationId Ty Cap)} (distinct : entries.Nodup)
    {first second : Nat} {requested : SpecializationId Ty Cap}
    (atFirst : entryAt entries first = some requested) (atSecond : entryAt entries second = some requested) :
    first = second := by
  apply distinct.eq_of_getElem?_eq (entryAt_lt_length atFirst)
  exact atFirst.trans atSecond.symm

/-- Law (subject `ConcreteSpec.Core.SpecializationTable.canonicalize`).
**Equal generic and argument lists share one canonical entry even at distinct request sites.**
The proof compares the actual edges produced by canonicalization. -/
theorem SpecializationTable.canonicalize_share {Ty Cap : Type} [DecidableEq Ty]
    [DecidableEq Cap] (requests : List (SpecializationRequest Ty Cap))
    (first second : SpecializationRequest Ty Cap) (firstMember : first ∈ requests)
    (secondMember : second ∈ requests)
    (same : first.specialization = second.specialization) :
    ∃ firstEdge secondEdge, firstEdge ∈ (canonicalize requests).edges ∧
      secondEdge ∈ (canonicalize requests).edges ∧ firstEdge.request = first ∧
      secondEdge.request = second ∧ firstEdge.entry = secondEdge.entry := by
  obtain ⟨firstEdge, firstKept, firstIdentity, firstResolved⟩ :=
    canonicalize_complete requests first firstMember
  obtain ⟨secondEdge, secondKept, secondIdentity, secondResolved⟩ :=
    canonicalize_complete requests second secondMember
  refine ⟨firstEdge, secondEdge, firstKept, secondKept, firstIdentity, secondIdentity, ?_⟩
  apply entry_unique (canonicalize_nodup requests) firstResolved
  rw [secondResolved, same]

/-- Law (subject `ConcreteSpec.Core.SpecializationTable.canonicalize`).
**Different arguments cannot be coalesced.** The canonical edges of requests with different
specialization identities point to different numeric entries. -/
theorem SpecializationTable.canonicalize_distinct {Ty Cap : Type} [DecidableEq Ty]
    [DecidableEq Cap] (requests : List (SpecializationRequest Ty Cap))
    (first second : SpecializationRequest Ty Cap) (firstMember : first ∈ requests)
    (secondMember : second ∈ requests) (different : first.specialization ≠ second.specialization) :
    ∃ firstEdge secondEdge, firstEdge ∈ (canonicalize requests).edges ∧
      secondEdge ∈ (canonicalize requests).edges ∧ firstEdge.request = first ∧
      secondEdge.request = second ∧ firstEdge.entry ≠ secondEdge.entry := by
  obtain ⟨firstEdge, firstKept, firstIdentity, firstResolved⟩ :=
    canonicalize_complete requests first firstMember
  obtain ⟨secondEdge, secondKept, secondIdentity, secondResolved⟩ :=
    canonicalize_complete requests second secondMember
  refine ⟨firstEdge, secondEdge, firstKept, secondKept, firstIdentity, secondIdentity, ?_⟩
  intro sameEntry
  apply different
  apply Option.some.inj
  rw [← firstResolved, ← secondResolved, sameEntry]

/-- Helper (projection). A collision returned by the pair check has exactly the checked modules as
its two sides. -/
theorem LoadCollision.between?_sound (first second : LoadedModule) (collision : LoadCollision)
    (found : between? first second = some collision) :
    collision.first = first ∧ collision.second = second := by
  by_cases package : first.module.package = second.module.package
  · by_cases origin : first.origin = second.origin
    · simp [between?, package, origin] at found
    · simp [between?, package, origin] at found
      subst collision
      exact ⟨rfl, rfl⟩
  · by_cases top : first.module.topLevel = second.module.topLevel
    · simp [between?, package, top] at found
      subst collision
      exact ⟨rfl, rfl⟩
    · simp [between?, package, top] at found

/-- Helper (projection). A collision returned by the one-module scan names that module and a member
of the scanned list. -/
theorem LoadCollision.withModule_sound (first : LoadedModule) :
    ∀ (modules : List LoadedModule) (collision : LoadCollision),
      withModule first modules = some collision →
        collision.first = first ∧ collision.second ∈ modules
  | [], collision, found => by simp [withModule] at found
  | second :: rest, collision, found => by
    unfold withModule at found
    cases pair : between? first second with
    | some direct =>
      rw [pair] at found
      simp only [Option.some.injEq] at found
      subst collision
      exact ⟨(between?_sound first second direct pair).1,
        List.mem_cons.2 (Or.inl (between?_sound first second direct pair).2)⟩
    | none =>
      rw [pair] at found
      obtain ⟨same, member⟩ := withModule_sound first rest collision found
      exact ⟨same, List.mem_cons.2 (Or.inr member)⟩

/-- Helper (projection). Soundness of the complete ordered-pair search. -/
theorem LoadedModules.findCollisionFrom_sound (candidates modules : List LoadedModule)
    (collision : LoadCollision) (found : findCollisionFrom candidates modules = some collision) :
    collision.first ∈ candidates ∧ collision.second ∈ modules := by
  induction candidates with
  | nil => simp [findCollisionFrom] at found
  | cons first rest ih =>
    unfold findCollisionFrom at found
    cases pair : LoadCollision.withModule first modules with
    | some direct =>
      rw [pair] at found
      simp only [Option.some.injEq] at found
      subst collision
      obtain ⟨same, member⟩ := LoadCollision.withModule_sound first modules direct pair
      exact ⟨List.mem_cons.2 (Or.inl same), member⟩
    | none =>
      rw [pair] at found
      obtain ⟨firstMember, secondMember⟩ := ih found
      exact ⟨List.mem_cons.2 (Or.inr firstMember), secondMember⟩

/-- Helper (projection). A real pair collision is visible to the ordered-pair checker. -/
theorem LoadCollision.between?_complete (first second : LoadedModule) (collision : LoadCollision)
    (sides : collision.first = first ∧ collision.second = second) :
    ∃ detected, between? first second = some detected := by
  cases collision with
  | topLevel a b sameTop differentPackage =>
    obtain ⟨rfl, rfl⟩ := sides
    exact ⟨.topLevel a b sameTop differentPackage, by
      simp [between?, LoadCollision.first, LoadCollision.second, sameTop, differentPackage]⟩
  | packageOrigin a b samePackage differentOrigins =>
    obtain ⟨rfl, rfl⟩ := sides
    exact ⟨.packageOrigin a b samePackage differentOrigins, by
      simp [between?, LoadCollision.first, LoadCollision.second, samePackage, differentOrigins]⟩

/-- Helper (projection). The one-module scan finds every collision with a member of its input list. -/
theorem LoadCollision.withModule_complete (first : LoadedModule) :
    ∀ (modules : List LoadedModule) (collision : LoadCollision),
      collision.first = first → collision.second ∈ modules →
        ∃ detected, withModule first modules = some detected
  | [], collision, same, member => by simp at member
  | second :: rest, collision, same, member => by
    simp only [List.mem_cons] at member
    cases member with
    | inl equal =>
      obtain ⟨detected, found⟩ := between?_complete first second collision ⟨same, equal⟩
      exact ⟨detected, by simp [withModule, found]⟩
    | inr later =>
      cases pair : between? first second with
      | some detected => exact ⟨detected, by simp [withModule, pair]⟩
      | none =>
        obtain ⟨detected, found⟩ := withModule_complete first rest collision same later
        exact ⟨detected, by simp [withModule, pair, found]⟩

/-- Law (subject `ConcreteSpec.Core.LoadedModules.load`, `ConcreteSpec.Core.LoadedModules.HasCollision`).
**Load refusal is complete.** Every top-level or same-package/different-origin collision in the
input causes refusal with a two-sided diagnostic. -/
theorem LoadedModules.load_refusal_complete (modules : List LoadedModule) (collision : LoadCollision)
    (present : HasCollision modules collision) : ∃ detected, load modules = .refused detected := by
  obtain ⟨firstMember, secondMember⟩ := present
  have search : ∀ candidates : List LoadedModule, collision.first ∈ candidates →
      ∃ detected, findCollisionFrom candidates modules = some detected := by
    intro candidates candidateMember
    induction candidates with
    | nil => simp at candidateMember
    | cons first rest ih =>
      simp only [List.mem_cons] at candidateMember
      cases candidateMember with
      | inl equal =>
        subst first
        obtain ⟨detected, found⟩ := LoadCollision.withModule_complete collision.first modules collision
          rfl secondMember
        exact ⟨detected, by simp [findCollisionFrom, found]⟩
      | inr later =>
        cases pair : LoadCollision.withModule first modules with
        | some detected => exact ⟨detected, by simp [findCollisionFrom, pair]⟩
        | none =>
          obtain ⟨detected, found⟩ := ih later
          exact ⟨detected, by simp [findCollisionFrom, pair, found]⟩
  obtain ⟨detected, found⟩ := search modules firstMember
  exact ⟨detected, by simp [load, findCollision, found]⟩

/-- Law (subject `ConcreteSpec.Core.LoadedModules.load`, `ConcreteSpec.Core.LoadedModules.HasCollision`, `ConcreteSpec.Core.LoadedModules.WellFormed`).
Every loader output satisfies its exact contract: a refusal names a collision present in the actual
input, and an admission returns the input unchanged while excluding both kinds of load collision. -/
theorem LoadedModules.load_refusal_sound (modules : List LoadedModule) :
    match load modules with
    | .refused collision => HasCollision modules collision
    | .admitted admitted => admitted = modules ∧ WellFormed modules := by
  unfold load
  cases found : findCollision modules with
  | none =>
    refine ⟨rfl, ?_⟩
    intro collision present
    obtain ⟨detected, refused⟩ := load_refusal_complete modules collision present
    simp [load, found] at refused
  | some collision => exact findCollisionFrom_sound modules modules collision found

/-- Law (subject `ConcreteSpec.Core.LoadedModules.load`, `ConcreteSpec.Core.LoadedModules.WellFormed`).
A module list is admitted if and only if the admitted list equals the input and the input is free
of both top-level and same-package/different-origin collisions. -/
theorem LoadedModules.load_admitted (modules admitted : List LoadedModule) :
    load modules = .admitted admitted ↔ admitted = modules ∧ WellFormed modules := by
  unfold load
  cases found : findCollision modules with
  | some collision =>
    simp only [reduceCtorEq, false_iff, not_and]
    intro _ wf
    exact wf collision (findCollisionFrom_sound modules modules collision found)
  | none =>
    simp only [LoadResult.admitted.injEq]
    constructor
    · rintro rfl
      refine ⟨rfl, ?_⟩
      intro collision present
      obtain ⟨detected, refused⟩ := load_refusal_complete modules collision present
      simp [load, found] at refused
    · rintro ⟨rfl, _⟩
      rfl

/-- Helper (erasure). The host byte view recovers every original byte in order. -/
@[simp] theorem SourceText.toBytes_ofBytes (bytes : List UInt8) :
    (SourceText.ofBytes bytes).toBytes = bytes := by
  simp [SourceText.toBytes, SourceText.ofBytes, List.map_map, Function.comp_def]

/-- Helper (erasure). The byte bound makes the numeric projection exact in the other direction. -/
@[simp] theorem SourceText.ofBytes_toBytes (source : SourceText) :
    SourceText.ofBytes source.toBytes = source := by
  cases source with
  | mk bytes byte_lt =>
    simp only [SourceText.ofBytes, SourceText.toBytes, List.map_map, SourceText.mk.injEq]
    calc
      bytes.map (UInt8.toNat ∘ UInt8.ofNat) = bytes.map id :=
        List.map_congr_left (fun byte member => UInt8.toNat_ofNat_of_lt' (byte_lt byte member))
      _ = bytes := List.map_id bytes

/-- Helper (erasure). Taking a prefix commutes with the exact byte projection. -/
theorem SourceText.take_toBytes (source : SourceText) (offset : Nat) :
    source.toBytes.take offset = (source.bytes.take offset).map UInt8.ofNat :=
  (List.map_take ..).symm

/-- Helper (erasure). The byte projection preserves length. -/
@[simp] theorem SourceText.length_toBytes (source : SourceText) :
    source.toBytes.length = source.length := by
  simp [SourceText.toBytes, SourceText.length]

/-- Helper (erasure). Appending a byte commutes with projection. -/
@[simp] theorem SourceText.toBytes_push (source : SourceText) (byte : UInt8) :
    (source.push byte).toBytes = source.toBytes ++ [byte] := by
  simp [SourceText.push, SourceText.toBytes]

/-- Law (subject `ConcreteSpec.Core.SourceText.lineAt`).
The line at byte offset zero is one. -/
theorem SourceText.lineAt_zero (source : SourceText) : source.lineAt 0 = 1 := rfl

/-- Law (subject `ConcreteSpec.Core.SourceText.columnAt`).
The column at byte offset zero is one. -/
theorem SourceText.columnAt_zero (source : SourceText) : source.columnAt 0 = 1 := rfl

/-- Law (subject `ConcreteSpec.Core.SourceText.lineAt`).
Appending a byte updates the source line count by its actual newline step. -/
theorem SourceText.lineAt_append (source : SourceText) (byte : UInt8) :
    (source.push byte).lineAt (source.length + 1) = source.lineAt source.length + if byte = 10 then 1 else 0 := by
  have whole : (source.bytes ++ [byte.toNat]).take (source.bytes.length + 1) =
      source.bytes ++ [byte.toNat] :=
    List.take_of_length_le (by simp)
  have newline_iff : byte.toNat = 10 ↔ byte = 10 := by
    simpa using (UInt8.toNat_inj (a := byte) (b := 10))
  simp only [SourceText.lineAt, SourceText.push, whole, SourceText.length, List.take_length]
  by_cases newline : byte = 10 <;> simp [newline_iff, newline]

/-- Law (subject `ConcreteSpec.Core.SourceText.lineAt`).
A newline starts a new line. -/
theorem SourceText.lineAt_append_newline (source : SourceText) :
    (source.push 10).lineAt (source.length + 1) = source.lineAt source.length + 1 := by
  simpa using SourceText.lineAt_append source 10

/-- Law (subject `ConcreteSpec.Core.SourceText.lineAt`).
A non-newline byte leaves the line unchanged. -/
theorem SourceText.lineAt_append_other (source : SourceText) (byte : UInt8) (other : byte ≠ 10) :
    (source.push byte).lineAt (source.length + 1) = source.lineAt source.length := by
  simpa [other] using SourceText.lineAt_append source byte

/-- Law (subject `ConcreteSpec.Core.SourceText.columnAt`).
Appending a byte updates the byte column: newline resets to one, every other byte adds one. -/
theorem SourceText.columnAt_append (source : SourceText) (byte : UInt8) :
    (source.push byte).columnAt (source.length + 1) =
      if byte = 10 then 1 else source.columnAt source.length + 1 := by
  have whole : (source.bytes ++ [byte.toNat]).take (source.bytes.length + 1) =
      source.bytes ++ [byte.toNat] :=
    List.take_of_length_le (by simp)
  have newline_iff : byte.toNat = 10 ↔ byte = 10 := by
    simpa using (UInt8.toNat_inj (a := byte) (b := 10))
  simp only [SourceText.columnAt, SourceText.push, whole, SourceText.length, List.take_length]
  by_cases newline : byte = 10 <;> simp [newline_iff, newline]

/-- Law (subject `ConcreteSpec.Core.SourceText.columnAt`).
A tab is one byte and therefore advances exactly one column. -/
theorem SourceText.columnAt_append_tab (source : SourceText) :
    (source.push 9).columnAt (source.length + 1) = source.columnAt source.length + 1 := by
  simpa using SourceText.columnAt_append source 9

/-- Law (subject `ConcreteSpec.Core.SourceText.columnAt`).
A newline resets the byte column to one. -/
theorem SourceText.columnAt_append_newline (source : SourceText) :
    (source.push 10).columnAt (source.length + 1) = 1 := by
  simpa using SourceText.columnAt_append source 10

/-- Law (subject `ConcreteSpec.Core.EncodedPremise.read`, `ConcreteSpec.Core.PremiseRecord.encode`).
A known category survives encoding and reading at the same specification version. -/
theorem EncodedPremise.read_encode (record : PremiseRecord) (current : record.spec = specVersion) :
    record.encode.read specVersion = some record := by
  cases record with
  | mk kind scope spec =>
    simp only at current
    subst spec
    cases kind <;> rfl

/-- Helper (inversion). A category number that is not the encoding of any `PremiseKind` decodes to
`none` at every reader version. -/
theorem PremiseKind.decode_eq_none_of_forall_encode_ne (reader : SpecVersion) (category : Nat)
    (unknown : ∀ kind : PremiseKind, kind.encode ≠ category) :
    PremiseKind.decode reader category = none := by
  unfold PremiseKind.decode
  split
  · match category with
    | 0 => exact False.elim (unknown .target rfl)
    | 1 => exact False.elim (unknown .heap rfl)
    | 2 => exact False.elim (unknown .stack rfl)
    | 3 => exact False.elim (unknown .foreignFunction rfl)
    | 4 => exact False.elim (unknown .trustedSurface rfl)
    | 5 => exact False.elim (unknown .entropy rfl)
    | 6 => exact False.elim (unknown .assume rfl)
    | n + 7 => rfl
  · rfl

/-- Law (subject `ConcreteSpec.Core.EncodedPremise.read`, `ConcreteSpec.Core.PremiseKind.encode`).
An unknown category—one that is not the encoding of any `PremiseKind` in the closed schema—is
refused at every reader version rather than assigned a catch-all meaning. -/
theorem EncodedPremise.read_unknown (reader : SpecVersion) (record : EncodedPremise)
    (unknown : ∀ kind : PremiseKind, kind.encode ≠ record.category) :
    record.read reader = none := by
  unfold EncodedPremise.read
  split
  · rw [PremiseKind.decode_eq_none_of_forall_encode_ne reader record.category unknown]
  · rfl

/-- Helper (computation). Computes the writer/reader version guards for `EncodedPremise.read_iff`. -/
theorem EncodedPremise.read_version_mismatch (reader : SpecVersion) (record : EncodedPremise)
    (mismatch : record.spec ≠ reader ∨ reader ≠ specVersion) :
    record.read reader = none := by
  rcases mismatch with hspec | hreader
  · simp [EncodedPremise.read, hspec]
  · simp [EncodedPremise.read, PremiseKind.decode, hreader]

/-- Law (subject `ConcreteSpec.Core.EncodedPremise.read`, `ConcreteSpec.Core.PremiseKind.encode`, `ConcreteSpec.Core.specVersion`).
Reading a wire premise succeeds if and only if the writer version matches the reader version, the
reader version is the current specification version, and the numeric category encodes a known
`PremiseKind` from the closed enumeration. -/
theorem EncodedPremise.read_iff (reader : SpecVersion) (record : EncodedPremise)
    (decoded : PremiseRecord) :
    record.read reader = some decoded ↔
      record.spec = reader ∧ reader = specVersion ∧
        ∃ kind : PremiseKind, kind.encode = record.category ∧
          decoded = ⟨kind, record.scope, record.spec⟩ := by
  constructor
  · intro found
    by_cases hspec : record.spec = reader
    · by_cases hreader : reader = specVersion
      · refine ⟨hspec, hreader, ?_⟩
        rcases record with ⟨category, scope, spec⟩
        simp only at hspec
        subst hreader
        subst hspec
        unfold EncodedPremise.read PremiseKind.decode at found
        simp only [↓reduceIte] at found
        match category with
        | 0 =>
          simp only [Option.some.injEq] at found
          exact ⟨.target, rfl, found.symm⟩
        | 1 =>
          simp only [Option.some.injEq] at found
          exact ⟨.heap, rfl, found.symm⟩
        | 2 =>
          simp only [Option.some.injEq] at found
          exact ⟨.stack, rfl, found.symm⟩
        | 3 =>
          simp only [Option.some.injEq] at found
          exact ⟨.foreignFunction, rfl, found.symm⟩
        | 4 =>
          simp only [Option.some.injEq] at found
          exact ⟨.trustedSurface, rfl, found.symm⟩
        | 5 =>
          simp only [Option.some.injEq] at found
          exact ⟨.entropy, rfl, found.symm⟩
        | 6 =>
          simp only [Option.some.injEq] at found
          exact ⟨.assume, rfl, found.symm⟩
        | n + 7 => cases found
      · rw [EncodedPremise.read_version_mismatch reader record (Or.inr hreader)] at found
        cases found
    · rw [EncodedPremise.read_version_mismatch reader record (Or.inl hspec)] at found
      cases found
  · rintro ⟨hspec, hreader, kind, hkind, rfl⟩
    unfold EncodedPremise.read PremiseKind.decode
    rw [← hkind]
    cases kind <;> simp [PremiseKind.encode, hspec, hreader]

namespace Vacuity

/-- A package and module used by the identity examples. -/
def package (name : Name := ⟨"demo"⟩) : PackageId := ⟨name, ⟨⟨"1"⟩⟩⟩

/-- An enclosing function used by the persistent-key examples. -/
def function : FnId := FnId.mk (ModuleId.mk package [⟨"demo"⟩] (by decide)) ⟨"f"⟩

/-- A second enclosing function. -/
def otherFunction : FnId := FnId.mk (ModuleId.mk package [⟨"demo"⟩] (by decide)) ⟨"g"⟩

/-- A declaration and constant that own obligations outside function bodies. -/
def declaration : DeclId := ⟨function.module, ⟨"S"⟩⟩
def constant : ConstId := ⟨function.module, ⟨"N"⟩⟩

/-- Structured paths for a binding, a call and a struct invariant. -/
def bindingKey : StableKey := ⟨.function function, ⟨[⟨.binding, .ordinal 0⟩]⟩⟩
def callKey : StableKey := ⟨.function function, ⟨[⟨.call, .ordinal 0⟩]⟩⟩
def invariantKey : StableKey := ⟨.declaration declaration, ⟨[]⟩⟩
def initializerKey : StableKey := ⟨.constant constant, ⟨[⟨.return, .ordinal 0⟩]⟩⟩

/-- Two entries whose lexical and persistent identities differ. -/
def siteTable : SiteKeyTable := [(⟨0⟩, bindingKey), (⟨1⟩, callKey)]

/-- Successor is an injective lexical renumbering. -/
def successor : SiteRenumbering := ⟨Nat.succ, fun _ _ h => Nat.succ.inj h⟩

/-- A non-injective site function that merges both sites. -/
def collapse : Nat → Nat := fun _ => 0

/-- Mutant table renumbering that collapses all site numbers to zero regardless of `r`. -/
def collapseRenumber (_r : SiteRenumbering) (table : SiteKeyTable) : SiteKeyTable :=
  table.renumberSites collapse

/-- Instance (law `ConcreteSpec.Core.SiteKeyTable.lookup_renumber`, case `ConcreteSpec.Core.Vacuity.siteTable`). -/
example : AList.lookup (siteTable.renumber successor) ⟨successor.apply (⟨1⟩ : SiteId).id⟩ =
    AList.lookup siteTable ⟨1⟩ :=
  SiteKeyTable.lookup_renumber successor siteTable ⟨1⟩

/-- Planted (law `ConcreteSpec.Core.SiteKeyTable.lookup_renumber`, case `ConcreteSpec.Core.Vacuity.siteTable`, subject `ConcreteSpec.Core.SiteKeyTable.renumber`, mutant `ConcreteSpec.Core.Vacuity.collapseRenumber`). -/
example : ¬ (AList.lookup (collapseRenumber successor siteTable) ⟨successor.apply (⟨1⟩ : SiteId).id⟩ =
    AList.lookup siteTable ⟨1⟩) := by decide

/-- Instance (law `ConcreteSpec.Core.SiteKeyTable.nodup_renumber`, case `ConcreteSpec.Core.Vacuity.siteTable`). -/
example : (siteTable.renumber successor).NoDupSites :=
  SiteKeyTable.nodup_renumber successor siteTable (by decide)

/-- Planted (law `ConcreteSpec.Core.SiteKeyTable.nodup_renumber`, case `ConcreteSpec.Core.Vacuity.siteTable`, subject `ConcreteSpec.Core.SiteKeyTable.renumber`, mutant `ConcreteSpec.Core.Vacuity.collapseRenumber`). -/
example : siteTable.NoDupSites ∧ ¬ (collapseRenumber successor siteTable).NoDupSites :=
  ⟨by decide, by decide⟩

/-- An unnamed binding, a named binding, a named call, and an unnamed loop used by key-stability examples. -/
def unnamedBinding : KeyConstruct := KeyConstruct.mk .binding (.unnamed none) []
def namedBinding : KeyConstruct := KeyConstruct.mk .binding (.named ⟨"x"⟩) []
def namedCall : KeyConstruct := KeyConstruct.mk .call (.named ⟨"work"⟩) []
def unnamedLoop : KeyConstruct := KeyConstruct.mk .loop (.unnamed none) []
def unnamedBlock : KeyConstruct := KeyConstruct.mk .block (.unnamed none) []

/-- Function bodies before and after editing `otherFunction`: `function`'s body is unchanged. -/
def bodiesBefore (f : FnId) : List KeyConstruct :=
  if f = function then [unnamedBinding, namedCall] else []

def bodiesAfter (f : FnId) : List KeyConstruct :=
  if f = otherFunction then [unnamedLoop, unnamedBinding, namedCall]
  else bodiesBefore f

/-- Starting lexical site offsets before and after editing `otherFunction`: `function`'s starting
site shifts from `0` to `10` because `otherFunction` gained sites. -/
def startSitesBefore (f : FnId) : Nat := if f = function then 0 else 5
def startSitesAfter (f : FnId) : Nat := if f = function then 10 else 0

/-- Mutant program key-table assignment that bakes the function's global starting lexical site
offset into the assigned persistent keys. -/
def assignProgramTablesFromSiteOffset (bodies : FnId → List KeyConstruct)
    (startSite : FnId → Nat) (f : FnId) : SiteKeyTable :=
  (bodies f).map fun c =>
    (⟨startSite f⟩, ⟨.function f, ⟨[⟨c.kind, .ordinal (startSite f)⟩]⟩⟩)

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.assignProgramTables_outsideEdit`, case `ConcreteSpec.Core.Vacuity.bodiesBefore`). -/
example : OutsideFunctionEdit
    (KeyConstruct.assignProgramTables bodiesBefore startSitesBefore)
    (KeyConstruct.assignProgramTables bodiesAfter startSitesAfter)
    otherFunction :=
  KeyConstruct.assignProgramTables_outsideEdit bodiesBefore bodiesAfter
    startSitesBefore startSitesAfter otherFunction
    (fun f hf => by simp [bodiesAfter, hf])

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.assignProgramTables_outsideEdit`, case `ConcreteSpec.Core.Vacuity.bodiesBefore`, subject `ConcreteSpec.Core.KeyConstruct.assignProgramTables`, mutant `ConcreteSpec.Core.Vacuity.assignProgramTablesFromSiteOffset`). -/
example : (∀ f, f ≠ otherFunction → bodiesAfter f = bodiesBefore f) ∧
    ¬ OutsideFunctionEdit
      (assignProgramTablesFromSiteOffset bodiesBefore startSitesBefore)
      (assignProgramTablesFromSiteOffset bodiesAfter startSitesAfter)
      otherFunction := by
  refine ⟨fun f hf => by simp [bodiesAfter, hf], ?_⟩
  intro h
  have hf := h function (by decide)
  revert hf
  decide

/-- A nested toy function body with two unnamed levels and a pinned leaf. -/
def nestedBody : KeyConstruct := ⟨.block, .unnamed none,
  [⟨.loop, .unnamed none, [⟨.primitive, .unnamed (some ⟨"effect"⟩), []⟩]⟩]⟩
def namedNested : KeyConstruct := ⟨.block, .named ⟨"body"⟩, nestedBody.children⟩
def nestedPath : FunctionRelativeKey :=
  ⟨[⟨.block, .ordinal 0⟩, ⟨.loop, .ordinal 0⟩, ⟨.primitive, .pinned ⟨"effect"⟩⟩]⟩
def insertedCall : KeyConstruct := ⟨.call, .unnamed none, []⟩

/-- Mutant collector that reproduces the old omission of every descendant. -/
def shallowCollect (scope : List KeyConstruct) : List KeyConstruct → List FunctionRelativeKey
  | [] => []
  | root :: rest => root.key scope :: shallowCollect (scope ++ [root]) rest

/-- Mutant collector that forgets to prefix descendant keys with their enclosing construct's segment. -/
def unprefixedCollect (scope : List KeyConstruct) : List KeyConstruct → List FunctionRelativeKey
  | [] => []
  | construct :: rest =>
    construct.key scope ::
      (unprefixedCollect (construct.childScope scope) construct.children ++
       unprefixedCollect (scope ++ [construct]) rest)
termination_by forest => sizeOf forest
decreasing_by
  all_goals simp_wf
  all_goals cases construct <;> simp_all <;> omega

/-- Mutant occurrence resolver that loses every route, with exactly the resolver's type. -/
def missingPathAtIn (_scope _forest : List KeyConstruct) (_route : Numbers) :
    Option FunctionRelativeKey := none

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.collectKeysIn_sound`, case `ConcreteSpec.Core.Vacuity.nestedBody`). -/
example : ∃ route, route ≠ [] ∧
    KeyConstruct.pathAtIn [] [nestedBody] route = some nestedPath :=
  KeyConstruct.collectKeysIn_sound [] [nestedBody] nestedPath
    (KeyConstruct.collectKeysIn_complete [] [nestedBody] [0, 0, 0] nestedPath
      (by decide) (by decide))

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.collectKeysIn_sound`, case `ConcreteSpec.Core.Vacuity.nestedBody`, subject `ConcreteSpec.Core.KeyConstruct.pathAtIn`, mutant `ConcreteSpec.Core.Vacuity.missingPathAtIn`). -/
example : nestedPath ∈ KeyConstruct.collectKeysIn [] [nestedBody] ∧
    ¬ (∃ route, route ≠ [] ∧ missingPathAtIn [] [nestedBody] route = some nestedPath) := by
  refine ⟨KeyConstruct.collectKeysIn_complete [] [nestedBody] [0, 0, 0] nestedPath
    (by decide) (by decide), ?_⟩
  rintro ⟨route, _, found⟩
  cases found

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.collectKeysIn_collectionSound`, case `ConcreteSpec.Core.Vacuity.nestedBody`). -/
example : KeyConstruct.CollectionSound [] [nestedBody]
    (KeyConstruct.collectKeysIn [] [nestedBody]) :=
  KeyConstruct.collectKeysIn_collectionSound [] [nestedBody]

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.collectKeysIn_collectionSound`, case `ConcreteSpec.Core.Vacuity.nestedBody`, subject `ConcreteSpec.Core.KeyConstruct.collectKeysIn`, mutant `ConcreteSpec.Core.Vacuity.unprefixedCollect`). -/
example : ¬ KeyConstruct.CollectionSound [] [nestedBody]
    (unprefixedCollect [] [nestedBody]) := by
  intro sound
  obtain ⟨route, nonempty, found⟩ := sound ⟨[⟨.loop, .ordinal 0⟩]⟩ (by
    simp [unprefixedCollect, nestedBody, KeyConstruct.key, KeyConstruct.segment,
      KeyConstruct.childScope])
  cases route with
  | nil => exact nonempty rfl
  | cons index tail =>
    cases index with
    | zero =>
      rw [KeyConstruct.pathAtIn] at found
      simp only [List.getElem?_cons_zero, List.take_zero, List.append_nil, Option.bind_eq_bind,
        Option.bind_some] at found
      cases hchild : KeyConstruct.pathAtIn (nestedBody.childScope []) nestedBody.children tail with
      | none =>
        rw [hchild] at found
        cases found
      | some suffix =>
        rw [hchild] at found
        simp [nestedBody, KeyConstruct.segment] at found
    | succ n =>
      simp [KeyConstruct.pathAtIn] at found

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.collectKeysIn_complete`, case `ConcreteSpec.Core.Vacuity.nestedBody`). -/
example : nestedPath ∈ KeyConstruct.collectKeysIn [] [nestedBody] :=
  KeyConstruct.collectKeysIn_complete [] [nestedBody] [0, 0, 0] nestedPath (by decide) (by decide)

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.collectKeysIn_complete`, case `ConcreteSpec.Core.Vacuity.nestedBody`, subject `ConcreteSpec.Core.KeyConstruct.collectKeysIn`, mutant `ConcreteSpec.Core.Vacuity.shallowCollect`). -/
example : (([0, 0, 0] : Numbers) ≠ [] ∧
    KeyConstruct.pathAtIn [] [nestedBody] [0, 0, 0] = some nestedPath) ∧
    ¬ nestedPath ∈ shallowCollect [] [nestedBody] := by decide

/-- Mutant assignment that drops descendants before assigning site numbers. -/
def shallowAssign (f : FnId) (startSite : Nat) (body : List KeyConstruct) : SiteKeyTable :=
  KeyConstruct.numberFunctionKeys f startSite (shallowCollect [] body)

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.assignFunctionTable_occurrences`, case `ConcreteSpec.Core.Vacuity.nestedBody`). -/
example : (⟨.function function, nestedPath⟩ : StableKey) ∈
      (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7 [nestedBody])) ↔
    ∃ route, route ≠ [] ∧ KeyConstruct.pathAt [nestedBody] route = some nestedPath :=
  KeyConstruct.assignFunctionTable_occurrences function 7 [nestedBody] nestedPath

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.assignFunctionTable_occurrences`, case `ConcreteSpec.Core.Vacuity.nestedBody`, subject `ConcreteSpec.Core.KeyConstruct.assignFunctionTable`, mutant `ConcreteSpec.Core.Vacuity.shallowAssign`). -/
example : ¬ ((⟨.function function, nestedPath⟩ : StableKey) ∈
      (List.map Prod.snd (shallowAssign function 7 [nestedBody])) ↔
    ∃ route, route ≠ [] ∧ KeyConstruct.pathAt [nestedBody] route = some nestedPath) := by
  intro equal
  have member := equal.mpr ⟨[0, 0, 0], by decide, by decide⟩
  revert member
  decide

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.assignFunctionTable_lookup`, case `ConcreteSpec.Core.Vacuity.nestedBody`). -/
example : AList.lookup (KeyConstruct.assignFunctionTable function 7 [nestedBody]) ⟨7 + 2⟩ =
      some ⟨.function function, nestedPath⟩ ↔
    (KeyConstruct.collectKeys [nestedBody])[2]? = some nestedPath ∧
      ∃ route, route ≠ [] ∧ KeyConstruct.pathAt [nestedBody] route = some nestedPath :=
  KeyConstruct.assignFunctionTable_lookup function 7 2 [nestedBody] nestedPath

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.assignFunctionTable_lookup`, case `ConcreteSpec.Core.Vacuity.nestedBody`, subject `ConcreteSpec.Core.KeyConstruct.assignFunctionTable`, mutant `ConcreteSpec.Core.Vacuity.shallowAssign`). -/
example : ¬ (AList.lookup (shallowAssign function 7 [nestedBody]) ⟨7 + 2⟩ =
      some ⟨.function function, nestedPath⟩ ↔
    (KeyConstruct.collectKeys [nestedBody])[2]? = some nestedPath ∧
      ∃ route, route ≠ [] ∧ KeyConstruct.pathAt [nestedBody] route = some nestedPath) := by
  intro equal
  have found := equal.mpr ⟨by
    simp [KeyConstruct.collectKeys, KeyConstruct.collectKeysIn, nestedBody, nestedPath,
      KeyConstruct.key, KeyConstruct.segment, KeyConstruct.childScope],
    [0, 0, 0], by decide, by decide⟩
  revert found
  decide

/-- Suffix below the toy block, independently resolved in its named scope. -/
def nestedSuffix : FunctionRelativeKey := ⟨[⟨.loop, .ordinal 0⟩, ⟨.primitive, .pinned ⟨"effect"⟩⟩]⟩
def renamedRootKey : FunctionRelativeKey := ⟨[⟨.block, .named ⟨"new"⟩⟩]⟩
def renamedPath : FunctionRelativeKey := ⟨⟨.block, .named ⟨"new"⟩⟩ :: nestedSuffix.path⟩
def pinnedRootKey : FunctionRelativeKey := ⟨[⟨.block, .pinned ⟨"pin"⟩⟩]⟩
def pinnedPath : FunctionRelativeKey := ⟨⟨.block, .pinned ⟨"pin"⟩⟩ :: nestedSuffix.path⟩

/-- Mutant rename that leaves the construct's old name unchanged. -/
def noopRename (construct : KeyConstruct) (_name : Name) : KeyConstruct := construct

/-- Mutant pin that ignores the supplied user label. -/
def noopPin (construct : KeyConstruct) (_label : Option Name) : KeyConstruct := construct

/-- Mutant collector that fails to reset the sibling scope when entering a `.named` construct. -/
def unresetScopeCollect (scope : List KeyConstruct) : List KeyConstruct → List FunctionRelativeKey
  | [] => []
  | construct :: rest =>
    construct.key scope ::
      ((unresetScopeCollect scope construct.children).map
        (fun key => ⟨construct.segment scope :: key.path⟩) ++
       unresetScopeCollect (scope ++ [construct]) rest)
termination_by forest => sizeOf forest
decreasing_by
  all_goals simp_wf
  all_goals cases construct <;> simp_all <;> omega

/-- Mutant function-table assignment that fails to reset scope when entering a `.named` construct. -/
def unresetScopeAssign (f : FnId) (startSite : Nat) (body : List KeyConstruct) : SiteKeyTable :=
  KeyConstruct.numberFunctionKeys f startSite (unresetScopeCollect [] body)

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.assignFunctionTable_named_scope`, case `ConcreteSpec.Core.Vacuity.namedNested`). -/
example : ((List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
      ([unnamedLoop] ++ KeyConstruct.mk .block (.named ⟨"body"⟩) nestedBody.children :: [namedCall]))).drop
        (KeyConstruct.collectKeys [unnamedLoop]).length).take
          (KeyConstruct.collectKeys [KeyConstruct.mk .block (.named ⟨"body"⟩) nestedBody.children]).length =
    ((List.map Prod.snd (KeyConstruct.assignFunctionTable function 19
      ([] ++ KeyConstruct.mk .block (.named ⟨"body"⟩) nestedBody.children :: [unnamedBinding]))).drop
        (KeyConstruct.collectKeys []).length).take
          (KeyConstruct.collectKeys [KeyConstruct.mk .block (.named ⟨"body"⟩) nestedBody.children]).length :=
  KeyConstruct.assignFunctionTable_named_scope function 7 19 [unnamedLoop] [namedCall]
    [] [unnamedBinding] .block ⟨"body"⟩ nestedBody.children

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.assignFunctionTable_named_scope`, case `ConcreteSpec.Core.Vacuity.namedNested`, subject `ConcreteSpec.Core.KeyConstruct.assignFunctionTable`, mutant `ConcreteSpec.Core.Vacuity.unresetScopeAssign`). -/
example : ¬ (((List.map Prod.snd (unresetScopeAssign function 7
      ([unnamedLoop] ++ KeyConstruct.mk .block (.named ⟨"body"⟩) nestedBody.children :: [namedCall]))).drop
        (KeyConstruct.collectKeys [unnamedLoop]).length).take
          (KeyConstruct.collectKeys [KeyConstruct.mk .block (.named ⟨"body"⟩) nestedBody.children]).length =
    ((List.map Prod.snd (unresetScopeAssign function 19
      ([] ++ KeyConstruct.mk .block (.named ⟨"body"⟩) nestedBody.children :: [unnamedBinding]))).drop
        (KeyConstruct.collectKeys []).length).take
          (KeyConstruct.collectKeys [KeyConstruct.mk .block (.named ⟨"body"⟩) nestedBody.children]).length) := by
  simp [unresetScopeAssign, KeyConstruct.numberFunctionKeys_snd, unresetScopeCollect,
    KeyConstruct.collectKeys, KeyConstruct.collectKeysIn, KeyConstruct.key,
    KeyConstruct.segment, KeyConstruct.childScope, ConstructName.isUnnamed,
    unnamedLoop, nestedBody, namedCall, unnamedBinding]

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.assignFunctionTable_rename_named`, case `ConcreteSpec.Core.Vacuity.namedNested`). -/
example : (⟨.function function, renamedRootKey⟩ : StableKey) ∈
      (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
        ([unnamedLoop] ++ (KeyConstruct.mk .block (.named ⟨"old"⟩) nestedBody.children).rename ⟨"new"⟩ :: []))) ∧
    (⟨.function function, renamedPath⟩ : StableKey) ∈
      (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
        ([unnamedLoop] ++ (KeyConstruct.mk .block (.named ⟨"old"⟩) nestedBody.children).rename ⟨"new"⟩ :: []))) :=
  KeyConstruct.assignFunctionTable_rename_named function 7 [unnamedLoop] [] .block ⟨"old"⟩ ⟨"new"⟩
    nestedBody.children [0, 0] nestedSuffix (by decide)

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.assignFunctionTable_rename_named`, case `ConcreteSpec.Core.Vacuity.namedNested`, subject `ConcreteSpec.Core.KeyConstruct.rename`, mutant `ConcreteSpec.Core.Vacuity.noopRename`). -/
example : KeyConstruct.pathAt nestedBody.children [0, 0] = some nestedSuffix ∧
    ¬ ((⟨.function function, renamedRootKey⟩ : StableKey) ∈
        (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
          ([unnamedLoop] ++ noopRename (KeyConstruct.mk .block (.named ⟨"old"⟩) nestedBody.children) ⟨"new"⟩ :: []))) ∧
      (⟨.function function, renamedPath⟩ : StableKey) ∈
        (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
          ([unnamedLoop] ++ noopRename (KeyConstruct.mk .block (.named ⟨"old"⟩) nestedBody.children) ⟨"new"⟩ :: [])))) := by
  refine ⟨by decide, ?_⟩
  rintro ⟨member, _⟩
  simp [KeyConstruct.assignFunctionTable, KeyConstruct.numberFunctionKeys_snd,
    KeyConstruct.collectKeys, KeyConstruct.collectKeysIn, noopRename, unnamedLoop, nestedBody,
    renamedRootKey, KeyConstruct.key, KeyConstruct.segment, KeyConstruct.childScope] at member

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.assignFunctionTable_rename_named`, case `ConcreteSpec.Core.Vacuity.namedNested`, subject `ConcreteSpec.Core.KeyConstruct.assignFunctionTable`, mutant `ConcreteSpec.Core.Vacuity.unresetScopeAssign`). -/
example : KeyConstruct.pathAt nestedBody.children [0, 0] = some nestedSuffix ∧
    ¬ ((⟨.function function, renamedRootKey⟩ : StableKey) ∈
        (List.map Prod.snd (unresetScopeAssign function 7
          ([unnamedLoop] ++ (KeyConstruct.mk .block (.named ⟨"old"⟩) nestedBody.children).rename ⟨"new"⟩ :: []))) ∧
      (⟨.function function, renamedPath⟩ : StableKey) ∈
        (List.map Prod.snd (unresetScopeAssign function 7
          ([unnamedLoop] ++ (KeyConstruct.mk .block (.named ⟨"old"⟩) nestedBody.children).rename ⟨"new"⟩ :: [])))) := by
  refine ⟨by decide, ?_⟩
  rintro ⟨_, member⟩
  simp [unresetScopeAssign, KeyConstruct.numberFunctionKeys_snd, unresetScopeCollect,
    KeyConstruct.rename, ConstructName.rename, unnamedLoop, nestedBody, renamedPath, nestedSuffix,
    KeyConstruct.key, KeyConstruct.segment] at member
  revert member
  decide

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.assignFunctionTable_rename_named`, case `ConcreteSpec.Core.Vacuity.namedCall`). -/
example : (⟨.function function, ⟨[⟨.call, .named ⟨"renamed"⟩⟩]⟩⟩ : StableKey) ∈
      (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
        ([] ++ (KeyConstruct.mk .call (.named ⟨"work"⟩) []).rename ⟨"renamed"⟩ :: []))) ∧
    (⟨.function function, ⟨⟨.call, .named ⟨"renamed"⟩⟩ :: ([] : List KeySegment)⟩⟩ : StableKey) ∈
      (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
        ([] ++ (KeyConstruct.mk .call (.named ⟨"work"⟩) []).rename ⟨"renamed"⟩ :: []))) :=
  KeyConstruct.assignFunctionTable_rename_named function 7 [] [] .call ⟨"work"⟩ ⟨"renamed"⟩
    [] [] ⟨[]⟩ rfl

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.assignFunctionTable_rename_named`, case `ConcreteSpec.Core.Vacuity.namedCall`, subject `ConcreteSpec.Core.KeyConstruct.rename`, mutant `ConcreteSpec.Core.Vacuity.noopRename`). -/
example : KeyConstruct.pathAt ([] : List KeyConstruct) [] = some ⟨[]⟩ ∧
    ¬ ((⟨.function function, ⟨[⟨.call, .named ⟨"renamed"⟩⟩]⟩⟩ : StableKey) ∈
        (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
          ([] ++ noopRename (KeyConstruct.mk .call (.named ⟨"work"⟩) []) ⟨"renamed"⟩ :: []))) ∧
      (⟨.function function, ⟨⟨.call, .named ⟨"renamed"⟩⟩ :: ([] : List KeySegment)⟩⟩ : StableKey) ∈
        (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
          ([] ++ noopRename (KeyConstruct.mk .call (.named ⟨"work"⟩) []) ⟨"renamed"⟩ :: [])))) := by
  refine ⟨rfl, ?_⟩
  rintro ⟨member, _⟩
  simp [KeyConstruct.assignFunctionTable, KeyConstruct.numberFunctionKeys_snd,
    KeyConstruct.collectKeys, KeyConstruct.collectKeysIn, noopRename, KeyConstruct.key,
    KeyConstruct.segment] at member

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.assignFunctionTable_pin`, case `ConcreteSpec.Core.Vacuity.nestedBody`). -/
example : (⟨.function function, pinnedRootKey⟩ : StableKey) ∈
      (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
        ([] ++ (KeyConstruct.mk .block (.unnamed none) nestedBody.children).pin (some ⟨"pin"⟩) :: []))) ∧
    (⟨.function function, pinnedPath⟩ : StableKey) ∈
      (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
        ([] ++ (KeyConstruct.mk .block (.unnamed none) nestedBody.children).pin (some ⟨"pin"⟩) :: []))) :=
  KeyConstruct.assignFunctionTable_pin function 7 [] [] .block none ⟨"pin"⟩ nestedBody.children
    [0, 0] nestedSuffix (by decide)

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.assignFunctionTable_pin`, case `ConcreteSpec.Core.Vacuity.nestedBody`, subject `ConcreteSpec.Core.KeyConstruct.pin`, mutant `ConcreteSpec.Core.Vacuity.noopPin`). -/
example : KeyConstruct.pathAtIn [] nestedBody.children [0, 0] = some nestedSuffix ∧
    ¬ ((⟨.function function, pinnedRootKey⟩ : StableKey) ∈
        (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
          ([] ++ noopPin (KeyConstruct.mk .block (.unnamed none) nestedBody.children) (some ⟨"pin"⟩) :: []))) ∧
      (⟨.function function, pinnedPath⟩ : StableKey) ∈
        (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
          ([] ++ noopPin (KeyConstruct.mk .block (.unnamed none) nestedBody.children) (some ⟨"pin"⟩) :: [])))) := by
  refine ⟨by decide, ?_⟩
  rintro ⟨member, _⟩
  simp [KeyConstruct.assignFunctionTable, KeyConstruct.numberFunctionKeys_snd,
    KeyConstruct.collectKeys, KeyConstruct.collectKeysIn, noopPin, nestedBody, pinnedRootKey,
    KeyConstruct.key, KeyConstruct.segment, KeyConstruct.childScope] at member

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.assignFunctionTable_pin`, case `ConcreteSpec.Core.Vacuity.unnamedBinding`). -/
example : (⟨.function function, ⟨[⟨.binding, .pinned ⟨"pinned"⟩⟩]⟩⟩ : StableKey) ∈
      (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
        ([] ++ (KeyConstruct.mk .binding (.unnamed none) []).pin (some ⟨"pinned"⟩) :: []))) ∧
    (⟨.function function, ⟨⟨.binding, .pinned ⟨"pinned"⟩⟩ :: ([] : List KeySegment)⟩⟩ : StableKey) ∈
      (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
        ([] ++ (KeyConstruct.mk .binding (.unnamed none) []).pin (some ⟨"pinned"⟩) :: []))) :=
  KeyConstruct.assignFunctionTable_pin function 7 [] [] .binding none ⟨"pinned"⟩ [] [] ⟨[]⟩ rfl

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.assignFunctionTable_pin`, case `ConcreteSpec.Core.Vacuity.unnamedBinding`, subject `ConcreteSpec.Core.KeyConstruct.pin`, mutant `ConcreteSpec.Core.Vacuity.noopPin`). -/
example : KeyConstruct.pathAtIn [] ([] : List KeyConstruct) [] = some ⟨[]⟩ ∧
    ¬ ((⟨.function function, ⟨[⟨.binding, .pinned ⟨"pinned"⟩⟩]⟩⟩ : StableKey) ∈
        (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
          ([] ++ noopPin (KeyConstruct.mk .binding (.unnamed none) []) (some ⟨"pinned"⟩) :: []))) ∧
      (⟨.function function, ⟨⟨.binding, .pinned ⟨"pinned"⟩⟩ :: ([] : List KeySegment)⟩⟩ : StableKey) ∈
        (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
          ([] ++ noopPin (KeyConstruct.mk .binding (.unnamed none) []) (some ⟨"pinned"⟩) :: [])))) := by
  refine ⟨rfl, ?_⟩
  rintro ⟨member, _⟩
  simp [KeyConstruct.assignFunctionTable, KeyConstruct.numberFunctionKeys_snd,
    KeyConstruct.collectKeys, KeyConstruct.collectKeysIn, noopPin, KeyConstruct.key,
    KeyConstruct.segment] at member

/-- A bad assignment that uses one ordinal across all unnamed construct kinds. -/
def allKindsOrdinal (preceding : List KeyConstruct) (target : KeyConstruct) : FunctionRelativeKey :=
  ⟨[⟨target.kind, .ordinal (preceding.filter fun sibling => sibling.name.isUnnamed).length⟩]⟩

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.key_insert_differentKind`, case `ConcreteSpec.Core.Vacuity.unnamedBinding`). -/
example : unnamedBinding.key ([] ++ insertedCall :: []) = unnamedBinding.key ([] ++ []) :=
  KeyConstruct.key_insert_differentKind [] [] insertedCall unnamedBinding (by decide)

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.key_insert_differentKind`, case `ConcreteSpec.Core.Vacuity.unnamedBinding`, subject `ConcreteSpec.Core.KeyConstruct.key`, mutant `ConcreteSpec.Core.Vacuity.allKindsOrdinal`). -/
example : insertedCall.kind ≠ unnamedBinding.kind ∧
    ¬ (allKindsOrdinal ([] ++ insertedCall :: []) unnamedBinding =
      allKindsOrdinal ([] ++ []) unnamedBinding) :=
  ⟨by decide, by decide⟩

/-- A bad assignment that counts named siblings in the ordinal. -/
def countNamedKey (preceding : List KeyConstruct) (target : KeyConstruct) : FunctionRelativeKey :=
  ⟨[⟨target.kind, .ordinal (preceding.filter fun sibling => sibling.kind == target.kind).length⟩]⟩

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.key_insert_named`, case `ConcreteSpec.Core.Vacuity.unnamedBinding`). -/
example : unnamedBinding.key ([] ++ KeyConstruct.mk .binding (.named ⟨"x"⟩) [] :: []) =
    unnamedBinding.key ([] ++ []) :=
  KeyConstruct.key_insert_named [] [] ⟨"x"⟩ .binding [] unnamedBinding

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.key_insert_named`, case `ConcreteSpec.Core.Vacuity.unnamedBinding`, subject `ConcreteSpec.Core.KeyConstruct.key`, mutant `ConcreteSpec.Core.Vacuity.countNamedKey`). -/
example : ¬ (countNamedKey ([] ++ KeyConstruct.mk .binding (.named ⟨"x"⟩) [] :: []) unnamedBinding =
    countNamedKey ([] ++ []) unnamedBinding) := by decide

/-- Mutant rename that demotes a named construct to an unnamed pinned construct, causing it to
consume an unnamed ordinal. -/
def demoteRename (construct : KeyConstruct) (name : Name) : KeyConstruct :=
  { construct with name := .unnamed (some name) }

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.key_rename_other`, case `ConcreteSpec.Core.Vacuity.namedBinding`). -/
example : unnamedBinding.key ([] ++ namedBinding.rename ⟨"y"⟩ :: [unnamedBinding]) =
    unnamedBinding.key ([] ++ namedBinding :: [unnamedBinding]) :=
  KeyConstruct.key_rename_other [] [unnamedBinding] namedBinding unnamedBinding ⟨"y"⟩

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.key_rename_other`, case `ConcreteSpec.Core.Vacuity.namedBinding`, subject `ConcreteSpec.Core.KeyConstruct.rename`, mutant `ConcreteSpec.Core.Vacuity.demoteRename`). -/
example : ¬ (unnamedBinding.key ([] ++ demoteRename namedBinding ⟨"y"⟩ :: [unnamedBinding]) =
    unnamedBinding.key ([] ++ namedBinding :: [unnamedBinding])) := by decide

/-- Mutant key assignment that excludes pinned unnamed siblings from the per-kind ordinal count. -/
def labelOnlyOrdinal (preceding : List KeyConstruct) (target : KeyConstruct) : FunctionRelativeKey :=
  let count := (preceding.filter fun sibling => sibling.kind == target.kind &&
    match sibling.name with | .named _ => false | .unnamed label => label.isNone).length
  ⟨[⟨target.kind, .ordinal count⟩]⟩

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.key_pin_other`, case `ConcreteSpec.Core.Vacuity.unnamedBinding`). -/
example : unnamedBinding.key ([] ++ unnamedBinding.pin (some ⟨"x"⟩) :: [unnamedBinding]) =
    unnamedBinding.key ([] ++ unnamedBinding :: [unnamedBinding]) :=
  KeyConstruct.key_pin_other [] [unnamedBinding] unnamedBinding unnamedBinding (some ⟨"x"⟩)

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.key_pin_other`, case `ConcreteSpec.Core.Vacuity.unnamedBinding`, subject `ConcreteSpec.Core.KeyConstruct.key`, mutant `ConcreteSpec.Core.Vacuity.labelOnlyOrdinal`). -/
example : ¬ (labelOnlyOrdinal ([] ++ unnamedBinding.pin (some ⟨"x"⟩) :: [unnamedBinding]) unnamedBinding =
    labelOnlyOrdinal ([] ++ unnamedBinding :: [unnamedBinding]) unnamedBinding) := by decide

/-- Helper computation example (not law evidence). -/
example : ((KeyConstruct.mk .call (.named ⟨"work"⟩) []).rename ⟨"renamed"⟩).key [] =
    ⟨[⟨.call, .named ⟨"renamed"⟩⟩]⟩ :=
  KeyConstruct.key_rename_named [] .call ⟨"work"⟩ [] ⟨"renamed"⟩

/-- Helper computation example (not law evidence). -/
example : ((KeyConstruct.mk .binding (.unnamed none) []).pin (some ⟨"pinned"⟩)).key [] =
    ⟨[⟨.binding, .pinned ⟨"pinned"⟩⟩]⟩ :=
  KeyConstruct.key_pin_unnamed [] .binding none [] ⟨"pinned"⟩

/-- Mutant `descendantKey` that uses one ordinal across all construct kinds for the root segment. -/
def allKindsDescendantKey (preceding : List KeyConstruct) (construct : KeyConstruct)
    (route : Numbers) : Option FunctionRelativeKey :=
  (KeyConstruct.pathAtIn (construct.childScope preceding) construct.children route).map
    fun descendant => ⟨(allKindsOrdinal preceding construct).path ++ descendant.path⟩

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.descendant_insert_differentKind`, case `ConcreteSpec.Core.Vacuity.nestedBody`). -/
example : nestedBody.descendantKey ([] ++ insertedCall :: []) [0, 0] =
    nestedBody.descendantKey ([] ++ []) [0, 0] :=
  KeyConstruct.descendant_insert_differentKind [] [] insertedCall nestedBody [0, 0] (by decide)

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.descendant_insert_differentKind`, case `ConcreteSpec.Core.Vacuity.nestedBody`, subject `ConcreteSpec.Core.KeyConstruct.descendantKey`, mutant `ConcreteSpec.Core.Vacuity.allKindsDescendantKey`). -/
example : KeyConstruct.SubtreeAvoidsKind insertedCall.kind nestedBody [0, 0] ∧
    ¬ (allKindsDescendantKey ([] ++ insertedCall :: []) nestedBody [0, 0] =
      allKindsDescendantKey ([] ++ []) nestedBody [0, 0]) :=
  ⟨by decide, by decide⟩

/-- Mutant `descendantKey` that counts named siblings in the root segment's ordinal. -/
def countNamedDescendantKey (preceding : List KeyConstruct) (construct : KeyConstruct)
    (route : Numbers) : Option FunctionRelativeKey :=
  (KeyConstruct.pathAtIn (construct.childScope preceding) construct.children route).map
    fun descendant => ⟨(countNamedKey preceding construct).path ++ descendant.path⟩

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.descendant_insert_named`, case `ConcreteSpec.Core.Vacuity.nestedBody`). -/
example : nestedBody.descendantKey ([] ++ KeyConstruct.mk .block (.named ⟨"body"⟩) nestedBody.children :: []) [0, 0] =
    nestedBody.descendantKey ([] ++ []) [0, 0] :=
  KeyConstruct.descendant_insert_named [] [] ⟨"body"⟩ .block nestedBody.children nestedBody [0, 0]

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.descendant_insert_named`, case `ConcreteSpec.Core.Vacuity.nestedBody`, subject `ConcreteSpec.Core.KeyConstruct.descendantKey`, mutant `ConcreteSpec.Core.Vacuity.countNamedDescendantKey`). -/
example : ¬ (countNamedDescendantKey ([] ++ KeyConstruct.mk .block (.named ⟨"body"⟩) nestedBody.children :: []) nestedBody [0, 0] =
    countNamedDescendantKey ([] ++ []) nestedBody [0, 0]) := by decide

/-- Mutant `pathAt` that counts ordinals across the whole forest rather than `forest.take index`,
so inserting a sibling after `target` shifts `target`'s ordinal. -/
def wholeForestPathAt (forest : List KeyConstruct) : Numbers → Option FunctionRelativeKey
  | [] => some ⟨[]⟩
  | index :: route => do
    let construct ← forest[index]?
    let descendant ← KeyConstruct.pathAtIn (construct.childScope forest) construct.children route
    pure ⟨construct.segment forest :: descendant.path⟩

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.pathAt_insert_after`, case `ConcreteSpec.Core.Vacuity.nestedBody`). -/
example : KeyConstruct.pathAt ([] ++ nestedBody :: ([] ++ unnamedBlock :: []))
      (([] : List KeyConstruct).length :: [0, 0]) =
    KeyConstruct.pathAt ([] ++ nestedBody :: ([] ++ []))
      (([] : List KeyConstruct).length :: [0, 0]) :=
  KeyConstruct.pathAt_insert_after [] [] [] nestedBody unnamedBlock [0, 0]

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.pathAt_insert_after`, case `ConcreteSpec.Core.Vacuity.nestedBody`, subject `ConcreteSpec.Core.KeyConstruct.pathAt`, mutant `ConcreteSpec.Core.Vacuity.wholeForestPathAt`). -/
example : ¬ (wholeForestPathAt ([] ++ nestedBody :: ([] ++ unnamedBlock :: []))
      (([] : List KeyConstruct).length :: [0, 0]) =
    wholeForestPathAt ([] ++ nestedBody :: ([] ++ []))
      (([] : List KeyConstruct).length :: [0, 0])) := by decide

/-- Mutant `pathAt` that counts named siblings in the root segment's ordinal. -/
def countNamedPathAt (forest : List KeyConstruct) : Numbers → Option FunctionRelativeKey
  | [] => some ⟨[]⟩
  | index :: route =>
    match forest[index]? with
    | none => none
    | some construct =>
      countNamedDescendantKey (forest.take index) construct route

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.pathAt_insert_named`, case `ConcreteSpec.Core.Vacuity.nestedBody`). -/
example : KeyConstruct.pathAt
      (([] ++ KeyConstruct.mk .block (.named ⟨"body"⟩) nestedBody.children :: []) ++ nestedBody :: [])
      (([] ++ KeyConstruct.mk .block (.named ⟨"body"⟩) nestedBody.children :: []).length :: [0, 0]) =
    KeyConstruct.pathAt
      (([] ++ []) ++ nestedBody :: [])
      (([] ++ ([] : List KeyConstruct)).length :: [0, 0]) :=
  KeyConstruct.pathAt_insert_named [] [] [] ⟨"body"⟩ .block nestedBody.children nestedBody [0, 0]

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.pathAt_insert_named`, case `ConcreteSpec.Core.Vacuity.nestedBody`, subject `ConcreteSpec.Core.KeyConstruct.pathAt`, mutant `ConcreteSpec.Core.Vacuity.countNamedPathAt`). -/
example : ¬ (countNamedPathAt
      (([] ++ KeyConstruct.mk .block (.named ⟨"body"⟩) nestedBody.children :: []) ++ nestedBody :: [])
      (([] ++ KeyConstruct.mk .block (.named ⟨"body"⟩) nestedBody.children :: []).length :: [0, 0]) =
    countNamedPathAt
      (([] ++ []) ++ nestedBody :: [])
      (([] ++ ([] : List KeyConstruct)).length :: [0, 0])) := by decide

/-- Mutant `pathAt` that counts all unnamed siblings regardless of kind. -/
def allKindsPathAt (forest : List KeyConstruct) : Numbers → Option FunctionRelativeKey
  | [] => some ⟨[]⟩
  | index :: route =>
    match forest[index]? with
    | none => none
    | some construct =>
      allKindsDescendantKey (forest.take index) construct route

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.pathAt_insert_differentKind`, case `ConcreteSpec.Core.Vacuity.nestedBody`). -/
example : KeyConstruct.pathAt
      (([] ++ insertedCall :: []) ++ nestedBody :: [])
      (([] ++ insertedCall :: []).length :: [0, 0]) =
    KeyConstruct.pathAt
      (([] ++ []) ++ nestedBody :: [])
      (([] ++ ([] : List KeyConstruct)).length :: [0, 0]) :=
  KeyConstruct.pathAt_insert_differentKind [] [] [] insertedCall nestedBody [0, 0] (by decide)

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.pathAt_insert_differentKind`, case `ConcreteSpec.Core.Vacuity.nestedBody`, subject `ConcreteSpec.Core.KeyConstruct.pathAt`, mutant `ConcreteSpec.Core.Vacuity.allKindsPathAt`). -/
example : KeyConstruct.SubtreeAvoidsKind insertedCall.kind nestedBody [0, 0] ∧
    ¬ (allKindsPathAt
        (([] ++ insertedCall :: []) ++ nestedBody :: [])
        (([] ++ insertedCall :: []).length :: [0, 0]) =
      allKindsPathAt
        (([] ++ []) ++ nestedBody :: [])
        (([] ++ ([] : List KeyConstruct)).length :: [0, 0])) :=
  ⟨by decide, by decide⟩

/-- Mutant `descendantKey` that fails to reset the sibling scope when entering a `.named`
construct, leaking outer siblings into the named construct's scope. -/
def unresetScopeDescendantKey (preceding : List KeyConstruct) (construct : KeyConstruct)
    (route : Numbers) : Option FunctionRelativeKey :=
  (KeyConstruct.pathAtIn preceding construct.children route).map
    fun descendant => ⟨construct.segment preceding :: descendant.path⟩

/-- Mutant `pathAt` that fails to reset the sibling scope when entering a `.named` construct. -/
def unresetScopePathAt (forest : List KeyConstruct) : Numbers → Option FunctionRelativeKey
  | [] => some ⟨[]⟩
  | index :: route =>
    match forest[index]? with
    | none => none
    | some construct =>
      unresetScopeDescendantKey (forest.take index) construct route

/-- Regression example (not independent soundness evidence). -/
example : KeyConstruct.pathAt
      ([unnamedLoop] ++ KeyConstruct.mk .block (.named ⟨"body"⟩) nestedBody.children :: [])
      (([unnamedLoop] : List KeyConstruct).length :: [0, 0]) =
    KeyConstruct.pathAt
      ([] ++ KeyConstruct.mk .block (.named ⟨"body"⟩) nestedBody.children :: [])
      (([] : List KeyConstruct).length :: [0, 0]) :=
  KeyConstruct.pathAt_named_ancestor_resets_scope [unnamedLoop] [] [] [] .block ⟨"body"⟩
    nestedBody.children [0, 0]

/-- Regression example (not independent soundness evidence). -/
example : ¬ (unresetScopePathAt
      ([unnamedLoop] ++ KeyConstruct.mk .block (.named ⟨"body"⟩) nestedBody.children :: [])
      (([unnamedLoop] : List KeyConstruct).length :: [0, 0]) =
    unresetScopePathAt
      ([] ++ KeyConstruct.mk .block (.named ⟨"body"⟩) nestedBody.children :: [])
      (([] : List KeyConstruct).length :: [0, 0])) := by decide

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.descendant_rename_other`, case `ConcreteSpec.Core.Vacuity.nestedBody`). -/
example : nestedBody.descendantKey ([] ++ namedNested.rename ⟨"otherName"⟩ :: []) [0, 0] =
    nestedBody.descendantKey ([] ++ namedNested :: []) [0, 0] :=
  KeyConstruct.descendant_rename_other [] [] namedNested nestedBody ⟨"otherName"⟩ [0, 0]

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.descendant_rename_other`, case `ConcreteSpec.Core.Vacuity.nestedBody`, subject `ConcreteSpec.Core.KeyConstruct.rename`, mutant `ConcreteSpec.Core.Vacuity.demoteRename`). -/
example : ¬ (nestedBody.descendantKey ([] ++ demoteRename namedNested ⟨"otherName"⟩ :: []) [0, 0] =
    nestedBody.descendantKey ([] ++ namedNested :: []) [0, 0]) := by decide

/-- Mutant `descendantKey` that excludes pinned unnamed siblings from the root's ordinal count. -/
def labelOnlyDescendantKey (preceding : List KeyConstruct) (construct : KeyConstruct)
    (route : Numbers) : Option FunctionRelativeKey :=
  (KeyConstruct.pathAtIn (construct.childScope preceding) construct.children route).map
    fun descendant => ⟨(labelOnlyOrdinal preceding construct).path ++ descendant.path⟩

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.descendant_pin_other`, case `ConcreteSpec.Core.Vacuity.nestedBody`). -/
example : nestedBody.descendantKey ([] ++ nestedBody.pin (some ⟨"pinned"⟩) :: []) [0, 0] =
    nestedBody.descendantKey ([] ++ nestedBody :: []) [0, 0] :=
  KeyConstruct.descendant_pin_other [] [] nestedBody nestedBody (some ⟨"pinned"⟩) [0, 0]

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.descendant_pin_other`, case `ConcreteSpec.Core.Vacuity.nestedBody`, subject `ConcreteSpec.Core.KeyConstruct.descendantKey`, mutant `ConcreteSpec.Core.Vacuity.labelOnlyDescendantKey`). -/
example : ¬ (labelOnlyDescendantKey ([] ++ nestedBody.pin (some ⟨"pinned"⟩) :: []) nestedBody [0, 0] =
    labelOnlyDescendantKey ([] ++ nestedBody :: []) nestedBody [0, 0]) := by decide

/-- Representation example (not law evidence). -/
example : ((KeyConstruct.mk .block (.named ⟨"body"⟩) nestedBody.children).rename ⟨"renamed"⟩).descendantKey [] [0, 0] =
    (KeyConstruct.pathAt nestedBody.children [0, 0]).map (fun suffix =>
      ⟨⟨.block, .named ⟨"renamed"⟩⟩ :: suffix.path⟩) :=
  KeyConstruct.descendant_rename_named [] .block ⟨"body"⟩ ⟨"renamed"⟩ nestedBody.children [0, 0]

/-- Representation example (not law evidence). -/
example : ¬ ((noopRename (KeyConstruct.mk .block (.named ⟨"body"⟩) nestedBody.children) ⟨"renamed"⟩).descendantKey [] [0, 0] =
    (KeyConstruct.pathAt nestedBody.children [0, 0]).map (fun suffix =>
      ⟨⟨.block, .named ⟨"renamed"⟩⟩ :: suffix.path⟩)) := by decide

/-- Three specializations of one generic, differing one argument list at a time. -/
def numeric : SpecializationId Nat Name := ⟨.function function, [0], [⟨"Fs"⟩]⟩
def otherType : SpecializationId Nat Name := ⟨.function function, [1], [⟨"Fs"⟩]⟩
def otherCapability : SpecializationId Nat Name := ⟨.function function, [0], [⟨"Console"⟩]⟩

/-- Distinct request sites for the same specialization. -/
def requestZero : SpecializationRequest Nat Name := ⟨⟨0⟩, numeric⟩
def requestOne : SpecializationRequest Nat Name := ⟨⟨1⟩, numeric⟩
def requestOtherType : SpecializationRequest Nat Name := ⟨⟨2⟩, otherType⟩
def requestOtherCapability : SpecializationRequest Nat Name := ⟨⟨3⟩, otherCapability⟩
def edgeZero : SpecializationEdge Nat Name := ⟨requestZero, 0⟩

/-- Mutated implementation that drops the first request's edge. -/
def dropFirstEdges {Ty Cap : Type} [DecidableEq Ty] [DecidableEq Cap]
    (requests : List (SpecializationRequest Ty Cap)) : SpecializationTable Ty Cap :=
  { SpecializationTable.canonicalize requests with edges := (SpecializationTable.canonicalize requests).edges.tail }

/-- Instance (law `ConcreteSpec.Core.SpecializationTable.canonicalize_complete`, case `ConcreteSpec.Core.Vacuity.requestZero`). -/
example : ∃ edge ∈ (SpecializationTable.canonicalize [requestZero, requestOne]).edges,
    edge.request = requestZero ∧
      SpecializationTable.entryAt (SpecializationTable.canonicalize [requestZero, requestOne]).entries edge.entry =
        some requestZero.specialization :=
  SpecializationTable.canonicalize_complete [requestZero, requestOne] requestZero (by decide)

/-- Planted (law `ConcreteSpec.Core.SpecializationTable.canonicalize_complete`, case `ConcreteSpec.Core.Vacuity.requestZero`, subject `ConcreteSpec.Core.SpecializationTable.canonicalize`, mutant `ConcreteSpec.Core.Vacuity.dropFirstEdges`). -/
example : requestZero ∈ [requestZero, requestOne] ∧
    ¬ (∃ edge ∈ (dropFirstEdges [requestZero, requestOne]).edges,
      edge.request = requestZero ∧
        SpecializationTable.entryAt (dropFirstEdges [requestZero, requestOne]).entries edge.entry =
          some requestZero.specialization) :=
  ⟨by decide, by decide⟩

/-- Mutant `entryAt` that offsets the lookup index out of bounds. -/
def oobEntryAt {Ty Cap : Type} (entries : List (SpecializationId Ty Cap))
    (index : Nat) : Option (SpecializationId Ty Cap) :=
  entries[index + 9]?

/-- Instance (law `ConcreteSpec.Core.SpecializationTable.canonicalize_edge_sound`, case `ConcreteSpec.Core.Vacuity.edgeZero`). -/
example : SpecializationTable.entryAt
    (SpecializationTable.canonicalize [requestZero, requestOne]).entries edgeZero.entry =
    some edgeZero.request.specialization :=
  SpecializationTable.canonicalize_edge_sound [requestZero, requestOne] edgeZero (by decide)

/-- Planted (law `ConcreteSpec.Core.SpecializationTable.canonicalize_edge_sound`, case `ConcreteSpec.Core.Vacuity.edgeZero`, subject `ConcreteSpec.Core.SpecializationTable.entryAt`, mutant `ConcreteSpec.Core.Vacuity.oobEntryAt`). -/
example : edgeZero ∈ (SpecializationTable.canonicalize [requestZero, requestOne]).edges ∧
    ¬ (oobEntryAt (SpecializationTable.canonicalize [requestZero, requestOne]).entries edgeZero.entry =
      some edgeZero.request.specialization) :=
  ⟨by decide, by decide⟩

/-- Instance (law `ConcreteSpec.Core.SpecializationTable.canonicalize_share`, case `ConcreteSpec.Core.Vacuity.requestZero`). -/
example : ∃ zeroEdge oneEdge, zeroEdge ∈ (SpecializationTable.canonicalize
    [requestZero, requestOne]).edges ∧ oneEdge ∈ (SpecializationTable.canonicalize
    [requestZero, requestOne]).edges ∧ zeroEdge.request = requestZero ∧
    oneEdge.request = requestOne ∧ zeroEdge.entry = oneEdge.entry :=
  SpecializationTable.canonicalize_share [requestZero, requestOne] requestZero requestOne
    (by simp) (by simp) rfl

/-- A planted interner parameterized by a lossy request key. -/
def canonicalizeByKey {Key : Type} [DecidableEq Key]
    (key : SpecializationRequest Nat Name → Key) :
    List (SpecializationRequest Nat Name) → List Key × SpecializationTable Nat Name
  | [] => ([], ⟨[], []⟩)
  | request :: rest =>
    let (keys, table) := canonicalizeByKey key rest
    let index := keys.idxOf (key request)
    if index < keys.length then
      (keys, { table with edges := ⟨request, index⟩ :: table.edges })
    else
      (keys ++ [key request], ⟨table.entries ++ [request.specialization],
        ⟨request, table.entries.length⟩ :: table.edges⟩)

/-- Canonicalizing by lexical request site incorrectly separates equal specializations requested
from different sites. -/
def bySite {Ty Cap : Type} [DecidableEq Ty] [DecidableEq Cap]
    (requests : List (SpecializationRequest Ty Cap)) : SpecializationTable Ty Cap :=
  ⟨requests.map (·.specialization),
    requests.zipIdx.map fun (req, idx) => ⟨req, idx⟩⟩

/-- Planted (law `ConcreteSpec.Core.SpecializationTable.canonicalize_share`, case `ConcreteSpec.Core.Vacuity.requestZero`, subject `ConcreteSpec.Core.SpecializationTable.canonicalize`, mutant `ConcreteSpec.Core.Vacuity.bySite`). -/
example : (requestZero ∈ [requestZero, requestOne] ∧
    requestOne ∈ [requestZero, requestOne] ∧
    requestZero.specialization = requestOne.specialization) ∧
    ¬ (∃ first second, first ∈ (bySite [requestZero, requestOne]).edges ∧
      second ∈ (bySite [requestZero, requestOne]).edges ∧ first.request = requestZero ∧
      second.request = requestOne ∧ first.entry = second.entry) := by
  refine ⟨⟨by decide, by decide, rfl⟩, ?_⟩
  simp [bySite, requestZero, requestOne]

/-- Instance (law `ConcreteSpec.Core.SpecializationTable.canonicalize_distinct`, case `ConcreteSpec.Core.Vacuity.requestOtherType`). -/
example : ∃ typeEdge otherEdge, typeEdge ∈ (SpecializationTable.canonicalize
    [requestZero, requestOtherType]).edges ∧ otherEdge ∈ (SpecializationTable.canonicalize
    [requestZero, requestOtherType]).edges ∧ typeEdge.request = requestZero ∧
    otherEdge.request = requestOtherType ∧ typeEdge.entry ≠ otherEdge.entry :=
  SpecializationTable.canonicalize_distinct [requestZero, requestOtherType] requestZero requestOtherType
    (by simp) (by simp) (by decide)

/-- Instance (law `ConcreteSpec.Core.SpecializationTable.canonicalize_distinct`, case `ConcreteSpec.Core.Vacuity.requestOtherCapability`). -/
example : ∃ capabilityEdge otherEdge, capabilityEdge ∈ (SpecializationTable.canonicalize
    [requestZero, requestOtherCapability]).edges ∧ otherEdge ∈ (SpecializationTable.canonicalize
    [requestZero, requestOtherCapability]).edges ∧ capabilityEdge.request = requestZero ∧
    otherEdge.request = requestOtherCapability ∧ capabilityEdge.entry ≠ otherEdge.entry :=
  SpecializationTable.canonicalize_distinct [requestZero, requestOtherCapability] requestZero
    requestOtherCapability (by simp) (by simp) (by decide)

/-- A generic-only interner that coalesces all requests into entry `0`. -/
def onlyGeneric {Ty Cap : Type} [DecidableEq Ty] [DecidableEq Cap]
    (requests : List (SpecializationRequest Ty Cap)) : SpecializationTable Ty Cap :=
  ⟨match requests.head? with | some r => [r.specialization] | none => [],
    requests.map fun req => ⟨req, 0⟩⟩

/-- Planted (law `ConcreteSpec.Core.SpecializationTable.canonicalize_distinct`, case `ConcreteSpec.Core.Vacuity.requestOtherType`, subject `ConcreteSpec.Core.SpecializationTable.canonicalize`, mutant `ConcreteSpec.Core.Vacuity.onlyGeneric`). -/
example : (requestZero ∈ [requestZero, requestOtherType] ∧
    requestOtherType ∈ [requestZero, requestOtherType] ∧
    requestZero.specialization ≠ requestOtherType.specialization) ∧
    ¬ (∃ first second, first ∈ (onlyGeneric [requestZero, requestOtherType]).edges ∧
      second ∈ (onlyGeneric [requestZero, requestOtherType]).edges ∧ first.request = requestZero ∧
      second.request = requestOtherType ∧ first.entry ≠ second.entry) := by
  refine ⟨⟨by decide, by decide, by decide⟩, ?_⟩
  simp [onlyGeneric]

/-- Planted (law `ConcreteSpec.Core.SpecializationTable.canonicalize_distinct`, case `ConcreteSpec.Core.Vacuity.requestOtherCapability`, subject `ConcreteSpec.Core.SpecializationTable.canonicalize`, mutant `ConcreteSpec.Core.Vacuity.onlyGeneric`). -/
example : (requestZero ∈ [requestZero, requestOtherCapability] ∧
    requestOtherCapability ∈ [requestZero, requestOtherCapability] ∧
    requestZero.specialization ≠ requestOtherCapability.specialization) ∧
    ¬ (∃ first second, first ∈ (onlyGeneric [requestZero, requestOtherCapability]).edges ∧
      second ∈ (onlyGeneric [requestZero, requestOtherCapability]).edges ∧ first.request = requestZero ∧
      second.request = requestOtherCapability ∧ first.entry ≠ second.entry) := by
  refine ⟨⟨by decide, by decide, by decide⟩, ?_⟩
  simp [onlyGeneric]

/-- Instance (law `ConcreteSpec.Core.SpecializationTable.canonicalize_nodup`, case `ConcreteSpec.Core.Vacuity.requestZero`). -/
example : (SpecializationTable.canonicalize [requestZero, requestOne]).entries.Nodup :=
  SpecializationTable.canonicalize_nodup [requestZero, requestOne]

/-- Planted (law `ConcreteSpec.Core.SpecializationTable.canonicalize_nodup`, case `ConcreteSpec.Core.Vacuity.requestZero`, subject `ConcreteSpec.Core.SpecializationTable.canonicalize`, mutant `ConcreteSpec.Core.Vacuity.bySite`). -/
example : ¬ (bySite [requestZero, requestOne]).entries.Nodup := by decide

/-- Loaded modules with explicit package versions and origins. -/
def firstModule : LoadedModule := ⟨⟨package ⟨"p1"⟩, [⟨"first"⟩], by decide⟩, ⟨⟨"registry-a"⟩⟩⟩
def secondModule : LoadedModule := ⟨⟨package ⟨"p1"⟩, [⟨"second"⟩], by decide⟩, ⟨⟨"registry-a"⟩⟩⟩
def collidingModule : LoadedModule := ⟨⟨package ⟨"p2"⟩, [⟨"first"⟩], by decide⟩, ⟨⟨"registry-b"⟩⟩⟩
def samePackageOtherOrigin : LoadedModule := ⟨⟨package ⟨"p1"⟩, [⟨"other"⟩], by decide⟩, ⟨⟨"mirror"⟩⟩⟩
def collidingTop : LoadCollision := .topLevel firstModule collidingModule rfl (by decide)
def collidingOrigin : LoadCollision := .packageOrigin firstModule samePackageOtherOrigin (by decide) (by decide)

/-- Mutant loader that fabricates a collision naming a module not in the loaded list. -/
def fabricatedRefusalLoad (_modules : List LoadedModule) : LoadResult :=
  .refused collidingOrigin

/-- Instance (law `ConcreteSpec.Core.LoadedModules.load_refusal_sound`, case `ConcreteSpec.Core.Vacuity.collidingTop`). -/
example : match LoadedModules.load [firstModule, collidingModule] with
    | .refused collision => LoadedModules.HasCollision [firstModule, collidingModule] collision
    | .admitted admitted => admitted = [firstModule, collidingModule] ∧
        LoadedModules.WellFormed [firstModule, collidingModule] :=
  LoadedModules.load_refusal_sound [firstModule, collidingModule]

/-- Planted (law `ConcreteSpec.Core.LoadedModules.load_refusal_sound`, case `ConcreteSpec.Core.Vacuity.collidingTop`, subject `ConcreteSpec.Core.LoadedModules.load`, mutant `ConcreteSpec.Core.Vacuity.fabricatedRefusalLoad`). -/
example : ¬ (match fabricatedRefusalLoad [firstModule, collidingModule] with
    | .refused collision => LoadedModules.HasCollision [firstModule, collidingModule] collision
    | .admitted admitted => admitted = [firstModule, collidingModule] ∧
        LoadedModules.WellFormed [firstModule, collidingModule]) := by
  change ¬ LoadedModules.HasCollision [firstModule, collidingModule] collidingOrigin
  unfold LoadedModules.HasCollision
  decide

/-- Mutant loader that admits all module lists without checking for collisions. -/
def admitAll (modules : List LoadedModule) : LoadResult := .admitted modules

/-- Planted (law `ConcreteSpec.Core.LoadedModules.load_refusal_sound`, case `ConcreteSpec.Core.Vacuity.collidingTop`, subject `ConcreteSpec.Core.LoadedModules.load`, mutant `ConcreteSpec.Core.Vacuity.admitAll`). -/
example : ¬ (match admitAll [firstModule, collidingModule] with
    | .refused collision => LoadedModules.HasCollision [firstModule, collidingModule] collision
    | .admitted admitted => admitted = [firstModule, collidingModule] ∧
        LoadedModules.WellFormed [firstModule, collidingModule]) := by
  rintro ⟨_, wf⟩
  exact wf collidingTop ⟨by decide, by decide⟩

/-- Instance (law `ConcreteSpec.Core.LoadedModules.load_refusal_complete`, case `ConcreteSpec.Core.Vacuity.collidingOrigin`). -/
example : ∃ detected, LoadedModules.load [firstModule, samePackageOtherOrigin] = .refused detected :=
  LoadedModules.load_refusal_complete [firstModule, samePackageOtherOrigin] collidingOrigin
    ⟨by decide, by decide⟩

/-- Planted (law `ConcreteSpec.Core.LoadedModules.load_refusal_complete`, case `ConcreteSpec.Core.Vacuity.collidingOrigin`, subject `ConcreteSpec.Core.LoadedModules.load`, mutant `ConcreteSpec.Core.Vacuity.admitAll`). -/
example : LoadedModules.HasCollision [firstModule, samePackageOtherOrigin] collidingOrigin ∧
    ¬ (∃ detected, admitAll [firstModule, samePackageOtherOrigin] = .refused detected) := by
  refine ⟨⟨by decide, by decide⟩, ?_⟩
  rintro ⟨detected, h⟩
  cases h

/-- Instance (law `ConcreteSpec.Core.LoadedModules.load_admitted`, case `ConcreteSpec.Core.Vacuity.firstModule`). -/
example : LoadedModules.load [firstModule, secondModule] =
      .admitted [firstModule, secondModule] ↔
    [firstModule, secondModule] = [firstModule, secondModule] ∧
      LoadedModules.WellFormed [firstModule, secondModule] :=
  LoadedModules.load_admitted [firstModule, secondModule] [firstModule, secondModule]

/-- Mutant loader that discards the input instead of admitting it unchanged. -/
def discardLoad (_modules : List LoadedModule) : LoadResult := .admitted []

/-- Planted (law `ConcreteSpec.Core.LoadedModules.load_admitted`, case `ConcreteSpec.Core.Vacuity.firstModule`, subject `ConcreteSpec.Core.LoadedModules.load`, mutant `ConcreteSpec.Core.Vacuity.discardLoad`). -/
example : ¬ (discardLoad [firstModule, secondModule] =
      .admitted [firstModule, secondModule] ↔
    [firstModule, secondModule] = [firstModule, secondModule] ∧
      LoadedModules.WellFormed [firstModule, secondModule]) := by
  intro equal
  have admitted := equal.mpr ⟨rfl, (LoadedModules.load_admitted [firstModule, secondModule]
    [firstModule, secondModule]).mp rfl |>.2⟩
  cases admitted

/-- Instance (law `ConcreteSpec.Core.LoadedModules.load_admitted`, case `ConcreteSpec.Core.Vacuity.collidingModule`). -/
example : LoadedModules.load [firstModule, collidingModule] =
      .admitted [firstModule, collidingModule] ↔
    [firstModule, collidingModule] = [firstModule, collidingModule] ∧
      LoadedModules.WellFormed [firstModule, collidingModule] :=
  LoadedModules.load_admitted [firstModule, collidingModule] [firstModule, collidingModule]

/-- Planted (law `ConcreteSpec.Core.LoadedModules.load_admitted`, case `ConcreteSpec.Core.Vacuity.collidingModule`, subject `ConcreteSpec.Core.LoadedModules.load`, mutant `ConcreteSpec.Core.Vacuity.admitAll`). -/
example : ¬ (admitAll [firstModule, collidingModule] =
      .admitted [firstModule, collidingModule] ↔
    [firstModule, collidingModule] = [firstModule, collidingModule] ∧
      LoadedModules.WellFormed [firstModule, collidingModule]) := by
  intro equal
  have wf := (equal.mp rfl).2
  exact wf collidingTop ⟨by decide, by decide⟩

/-- Typed byte strings for newline, tab, EOF and multibyte examples. -/
def aSource : SourceText := SourceText.ofBytes [97]
def aNewline : SourceText := aSource.push 10
def aTab : SourceText := aSource.push 9
def aTabNewline : SourceText := aTab.push 10
def utf8Prefix : SourceText := SourceText.ofBytes [195]
def utf8Source : SourceText := SourceText.ofBytes [195, 169]
def otherTabSource : SourceText := SourceText.ofBytes [10, 97, 98]

example : aSource ≠ aTab := by decide

/-- Mutant line function with a zero-based origin. -/
def zeroBasedLine (source : SourceText) (offset : Nat) : Nat :=
  (source.bytes.take offset).count 10

/-- Instance (law `ConcreteSpec.Core.SourceText.lineAt_zero`, case `ConcreteSpec.Core.Vacuity.aSource`). -/
example : aSource.lineAt 0 = 1 := SourceText.lineAt_zero aSource

/-- Planted (law `ConcreteSpec.Core.SourceText.lineAt_zero`, case `ConcreteSpec.Core.Vacuity.aSource`, subject `ConcreteSpec.Core.SourceText.lineAt`, mutant `ConcreteSpec.Core.Vacuity.zeroBasedLine`). -/
example : ¬ (zeroBasedLine aSource 0 = 1) := by decide

/-- Mutant column function with a zero-based origin. -/
def zeroBasedColumn (source : SourceText) (offset : Nat) : Nat :=
  ((source.bytes.take offset).reverse.takeWhile (fun byte => byte != 10)).length

/-- Instance (law `ConcreteSpec.Core.SourceText.columnAt_zero`, case `ConcreteSpec.Core.Vacuity.aSource`). -/
example : aSource.columnAt 0 = 1 := SourceText.columnAt_zero aSource

/-- Planted (law `ConcreteSpec.Core.SourceText.columnAt_zero`, case `ConcreteSpec.Core.Vacuity.aSource`, subject `ConcreteSpec.Core.SourceText.columnAt`, mutant `ConcreteSpec.Core.Vacuity.zeroBasedColumn`). -/
example : ¬ (zeroBasedColumn aSource 0 = 1) := by decide

/-- Mutant line function that always returns line 1. -/
def constantLine (_source : SourceText) (_offset : Nat) : Nat := 1

/-- Mutant line function that increments on every byte regardless of newline. -/
def offsetLine (_source : SourceText) (offset : Nat) : Nat := offset + 1

/-- Instance (law `ConcreteSpec.Core.SourceText.lineAt_append_newline`, case `ConcreteSpec.Core.Vacuity.aSource`). -/
example : (aSource.push 10).lineAt (aSource.length + 1) = aSource.lineAt aSource.length + 1 :=
  SourceText.lineAt_append_newline aSource

/-- Planted (law `ConcreteSpec.Core.SourceText.lineAt_append_newline`, case `ConcreteSpec.Core.Vacuity.aSource`, subject `ConcreteSpec.Core.SourceText.lineAt`, mutant `ConcreteSpec.Core.Vacuity.constantLine`). -/
example : ¬ (constantLine (aSource.push 10) (aSource.length + 1) =
    constantLine aSource aSource.length + 1) := by decide

/-- Instance (law `ConcreteSpec.Core.SourceText.lineAt_append_other`, case `ConcreteSpec.Core.Vacuity.aSource`). -/
example : (aSource.push 9).lineAt (aSource.length + 1) = aSource.lineAt aSource.length :=
  SourceText.lineAt_append_other aSource 9 (by decide)

/-- Planted (law `ConcreteSpec.Core.SourceText.lineAt_append_other`, case `ConcreteSpec.Core.Vacuity.aSource`, subject `ConcreteSpec.Core.SourceText.lineAt`, mutant `ConcreteSpec.Core.Vacuity.offsetLine`). -/
example : (9 : UInt8) ≠ 10 ∧
    ¬ (offsetLine (aSource.push 9) (aSource.length + 1) = offsetLine aSource aSource.length) :=
  ⟨by decide, by decide⟩

/-- Mutant column function that counts a tab byte (`9`) as four columns instead of one. -/
def tabStopFour (source : SourceText) (offset : Nat) : Nat :=
  ((source.toBytes.take offset).reverse.takeWhile (fun byte => byte != 10)).foldl
    (fun acc byte => acc + if byte == 9 then 4 else 1) 1

/-- Instance (law `ConcreteSpec.Core.SourceText.columnAt_append_tab`, case `ConcreteSpec.Core.Vacuity.aSource`). -/
example : (aSource.push 9).columnAt (aSource.length + 1) = aSource.columnAt aSource.length + 1 :=
  SourceText.columnAt_append_tab aSource

/-- Planted (law `ConcreteSpec.Core.SourceText.columnAt_append_tab`, case `ConcreteSpec.Core.Vacuity.aSource`, subject `ConcreteSpec.Core.SourceText.columnAt`, mutant `ConcreteSpec.Core.Vacuity.tabStopFour`). -/
example : ¬ (tabStopFour (aSource.push 9) (aSource.length + 1) =
    tabStopFour aSource aSource.length + 1) := by decide

/-- Mutant column function that never resets at a newline byte. -/
def noResetColumn (source : SourceText) (offset : Nat) : Nat :=
  (source.toBytes.take offset).length + 1

/-- Instance (law `ConcreteSpec.Core.SourceText.columnAt_append_newline`, case `ConcreteSpec.Core.Vacuity.aTab`). -/
example : (aTab.push 10).columnAt (aTab.length + 1) = 1 :=
  SourceText.columnAt_append_newline aTab

/-- Planted (law `ConcreteSpec.Core.SourceText.columnAt_append_newline`, case `ConcreteSpec.Core.Vacuity.aTab`, subject `ConcreteSpec.Core.SourceText.columnAt`, mutant `ConcreteSpec.Core.Vacuity.noResetColumn`). -/
example : ¬ (noResetColumn (aTab.push 10) (aTab.length + 1) = 1) := by decide

/-- Instance (law `ConcreteSpec.Core.SourceText.lineAt_append`, case `ConcreteSpec.Core.Vacuity.aSource`). -/
example : (aSource.push 10).lineAt (aSource.length + 1) =
    aSource.lineAt aSource.length + if (10 : UInt8) = 10 then 1 else 0 :=
  SourceText.lineAt_append aSource 10

/-- Planted (law `ConcreteSpec.Core.SourceText.lineAt_append`, case `ConcreteSpec.Core.Vacuity.aSource`, subject `ConcreteSpec.Core.SourceText.lineAt`, mutant `ConcreteSpec.Core.Vacuity.constantLine`). -/
example : ¬ (constantLine (aSource.push 10) (aSource.length + 1) =
    constantLine aSource aSource.length + if (10 : UInt8) = 10 then 1 else 0) := by decide

/-- Mutant column function that skips UTF-8 continuation bytes (`128 ≤ b < 192`). -/
def scalarColumn (source : SourceText) (offset : Nat) : Nat :=
  (((source.toBytes.take offset).reverse.takeWhile (fun byte => byte != 10)).filter
    (fun byte => decide (byte < 128 ∨ 192 ≤ byte))).length + 1

/-- Instance (law `ConcreteSpec.Core.SourceText.columnAt_append`, case `ConcreteSpec.Core.Vacuity.utf8Prefix`). -/
example : (utf8Prefix.push 169).columnAt (utf8Prefix.length + 1) =
    if (169 : UInt8) = 10 then 1 else utf8Prefix.columnAt utf8Prefix.length + 1 :=
  SourceText.columnAt_append utf8Prefix 169

/-- Planted (law `ConcreteSpec.Core.SourceText.columnAt_append`, case `ConcreteSpec.Core.Vacuity.utf8Prefix`, subject `ConcreteSpec.Core.SourceText.columnAt`, mutant `ConcreteSpec.Core.Vacuity.scalarColumn`). -/
example : ¬ (scalarColumn (utf8Prefix.push 169) (utf8Prefix.length + 1) =
    if (169 : UInt8) = 10 then 1 else scalarColumn utf8Prefix utf8Prefix.length + 1) := by decide

/-- A source `assume`, a package stack assumption, and their wire records. -/
def sourceAssume : PremiseId :=
  ⟨.assume, .function (StableKey.mk (.function function) ⟨[⟨.assumption, .ordinal 0⟩]⟩)⟩
def stackAssumption : PremiseId := ⟨.stack, .package package ⟨⟨"defaultStack"⟩⟩⟩
def stackRecord : PremiseRecord := ⟨stackAssumption.kind, stackAssumption.scope, specVersion⟩
def unknownRecord : EncodedPremise := ⟨99, stackAssumption.scope, specVersion⟩
def futureVersion : SpecVersion := ⟨0, 2⟩
def mismatchedRecord : EncodedPremise := ⟨PremiseKind.stack.encode, stackAssumption.scope, futureVersion⟩

/-- Instance (law `ConcreteSpec.Core.EncodedPremise.read_encode`, case `ConcreteSpec.Core.Vacuity.stackRecord`). -/
example : stackRecord.encode.read specVersion = some stackRecord :=
  EncodedPremise.read_encode stackRecord rfl

/-- A faulty encoder that discards the known premise category, retaining the writer version and
typed scope. -/
def encodeDroppingKind (record : PremiseRecord) : EncodedPremise :=
  { record.encode with category := 0 }

/-- Planted (law `ConcreteSpec.Core.EncodedPremise.read_encode`, case `ConcreteSpec.Core.Vacuity.stackRecord`, subject `ConcreteSpec.Core.PremiseRecord.encode`, mutant `ConcreteSpec.Core.Vacuity.encodeDroppingKind`). -/
example : stackRecord.spec = specVersion ∧
    ¬ ((encodeDroppingKind stackRecord).read specVersion = some stackRecord) :=
  ⟨rfl, by decide⟩

/-- Instance (law `ConcreteSpec.Core.EncodedPremise.read_unknown`, case `ConcreteSpec.Core.Vacuity.unknownRecord`). -/
example : unknownRecord.read specVersion = none :=
  EncodedPremise.read_unknown specVersion unknownRecord (by intro kind; cases kind <;> decide)

/-- Mutated reader that assigns a catch-all `.assume` category to unknown numbers. -/
def catchAllRead (reader : SpecVersion) (record : EncodedPremise) : Option PremiseRecord :=
  if record.spec = reader then some ⟨.assume, record.scope, record.spec⟩ else none

/-- Planted (law `ConcreteSpec.Core.EncodedPremise.read_unknown`, case `ConcreteSpec.Core.Vacuity.unknownRecord`, subject `ConcreteSpec.Core.EncodedPremise.read`, mutant `ConcreteSpec.Core.Vacuity.catchAllRead`). -/
example : (∀ kind : PremiseKind, kind.encode ≠ unknownRecord.category) ∧
    ¬ (catchAllRead specVersion unknownRecord = none) :=
  ⟨by intro kind; cases kind <;> decide, by decide⟩

/-- Mutated reader that ignores version mismatches and decodes every record under `specVersion`. -/
def versionIgnoringRead (_reader : SpecVersion) (record : EncodedPremise) : Option PremiseRecord :=
  (PremiseKind.decode specVersion record.category).map fun kind => ⟨kind, record.scope, record.spec⟩

/-- Decoded premise record corresponding to `mismatchedRecord` when version checks are skipped. -/
def mismatchedDecoded : PremiseRecord := ⟨.stack, stackAssumption.scope, futureVersion⟩

/-- Instance (law `ConcreteSpec.Core.EncodedPremise.read_iff`, case `ConcreteSpec.Core.Vacuity.mismatchedRecord`). -/
example : mismatchedRecord.read specVersion = some mismatchedDecoded ↔
    mismatchedRecord.spec = specVersion ∧ specVersion = specVersion ∧
      ∃ kind : PremiseKind, kind.encode = mismatchedRecord.category ∧
        mismatchedDecoded = ⟨kind, mismatchedRecord.scope, mismatchedRecord.spec⟩ :=
  EncodedPremise.read_iff specVersion mismatchedRecord mismatchedDecoded

/-- Planted (law `ConcreteSpec.Core.EncodedPremise.read_iff`, case `ConcreteSpec.Core.Vacuity.mismatchedRecord`, subject `ConcreteSpec.Core.EncodedPremise.read`, mutant `ConcreteSpec.Core.Vacuity.versionIgnoringRead`). -/
example : ¬ (versionIgnoringRead specVersion mismatchedRecord = some mismatchedDecoded ↔
    mismatchedRecord.spec = specVersion ∧ specVersion = specVersion ∧
      ∃ kind : PremiseKind, kind.encode = mismatchedRecord.category ∧
        mismatchedDecoded = ⟨kind, mismatchedRecord.scope, mismatchedRecord.spec⟩) := by
  intro equal
  have hspec := (equal.mp rfl).1
  revert hspec
  decide

/-- Instance (law `ConcreteSpec.Core.EncodedPremise.read_iff`, case `ConcreteSpec.Core.Vacuity.futureVersion`).
Writer and reader agree on the future version, isolating the current-schema guard. -/
example : mismatchedRecord.read futureVersion = some mismatchedDecoded ↔
    mismatchedRecord.spec = futureVersion ∧ futureVersion = specVersion ∧
      ∃ kind : PremiseKind, kind.encode = mismatchedRecord.category ∧
        mismatchedDecoded = ⟨kind, mismatchedRecord.scope, mismatchedRecord.spec⟩ :=
  EncodedPremise.read_iff futureVersion mismatchedRecord mismatchedDecoded

/-- Planted (law `ConcreteSpec.Core.EncodedPremise.read_iff`, case `ConcreteSpec.Core.Vacuity.futureVersion`, subject `ConcreteSpec.Core.EncodedPremise.read`, mutant `ConcreteSpec.Core.Vacuity.versionIgnoringRead`).
The category is known and writer equals reader; only the current-schema guard fails. -/
example : mismatchedRecord.spec = futureVersion ∧
    PremiseKind.decode specVersion mismatchedRecord.category = some .stack ∧
    ¬ (versionIgnoringRead futureVersion mismatchedRecord = some mismatchedDecoded ↔
      mismatchedRecord.spec = futureVersion ∧ futureVersion = specVersion ∧
        ∃ kind : PremiseKind, kind.encode = mismatchedRecord.category ∧
          mismatchedDecoded = ⟨kind, mismatchedRecord.scope, mismatchedRecord.spec⟩) := by
  refine ⟨rfl, rfl, ?_⟩
  intro equal
  have hreader := (equal.mp rfl).2.1
  revert hreader
  decide

/-- Accepted: declaration and constant owners cover non-function obligations without fake functions. -/
example : invariantKey.owner = .declaration declaration ∧ initializerKey.owner = .constant constant := by
  exact ⟨rfl, rfl⟩

/-- Two facts with the same conclusion but different premise dependencies. -/
def conditional : ConditionalFact Name := ⟨⟨"bounded"⟩, [sourceAssume]⟩
def unconditional : ConditionalFact Name := ⟨⟨"bounded"⟩, []⟩

/-- Planted counter-example: comparing only conclusions loses the conditional fact's premise debt. -/
def conclusionOnly (first second : ConditionalFact Name) : Prop :=
  first.conclusion = second.conclusion

example : conclusionOnly conditional unconditional ∧ conditional ≠ unconditional := by
  exact ⟨rfl, by decide⟩

end Vacuity
namespace SourcePath

private theorem reason_inOrder (reason : Reason) : reason ∈ Reason.inOrder := by
  cases reason <;> decide

private theorem candidate_iff (raw : SourceText) (violation : Violation raw.length) :
    violation ∈ candidates raw ↔
      violation.location ∈ check raw (components raw) violation.reason := by
  cases violation with
  | mk reason location =>
    simp [candidates, candidatesWith, check, List.mem_flatMap, List.mem_map, reason_inOrder]

private theorem noViolation_iff (raw : SourceText) : firstViolation raw = none ↔ RawOk raw := by
  refine ⟨rawOk_of_noViolation raw, ?_⟩
  intro valid
  cases selected : firstViolation raw with
  | none => rfl
  | some violation =>
    have found := (candidate_iff raw violation).1 (List.minOn?_mem selected)
    have clear := (check_witness_iff violation.reason raw).2
      (valid violation.reason (reason_inOrder _))
    simp [clear] at found

private theorem parse_of_clear (raw : SourceText) (clear : firstViolation raw = none) :
    parse raw = .ok ⟨raw, rawOk_of_noViolation raw clear⟩ := by
  unfold parse
  split <;> simp_all

private theorem parse_of_hit (raw : SourceText) (hit : Violation raw.length)
    (found : firstViolation raw = some hit) : parse raw = .error ⟨raw, hit⟩ := by
  unfold parse
  split <;> simp_all

private theorem parse_probe_iff (raw : SourceText) (path : SourcePath) :
    parse raw = .ok path ↔ path.renderBytes = raw ∧ RawOk raw := by
  cases found : firstViolation raw with
  | none =>
    have valid := (noViolation_iff raw).1 found
    cases path
    simp only [parse_of_clear raw found, Except.ok.injEq, SourcePath.mk.injEq, renderBytes]
    exact ⟨fun equal => ⟨equal.symm, valid⟩, fun h => h.1.symm⟩
  | some violation =>
    have invalid : ¬RawOk raw := fun valid => by
      have absent := (noViolation_iff raw).2 valid
      simp [found] at absent
    simp [parse_of_hit raw violation found, invalid]

private theorem scan_located {length : Nat} (start : Nat) (rev rest : ConcreteSpec.Numbers)
    (total : start + rev.length + rest.length = length) :
    (scan start rev rest total).map (fun part => (part.start, part.bytes)) =
      Reference.locate start (rest.splitOnPPrepend (· == 47) rev) := by
  induction rest generalizing start rev with
  | nil => simp [scan, Reference.locate]
  | cons byte rest ih =>
    by_cases slash : byte = 47
    · simp [scan, List.splitOnPPrepend, Reference.locate, slash, ih]
    · simp [scan, List.splitOnPPrepend, slash, ih]

/-- The production scan and an ordinary slash split agree on every component and its offset. -/
theorem components_reference (raw : SourceText) :
    (components raw).map (fun part => (part.start, part.bytes)) = Reference.pieces raw.bytes :=
  scan_located 0 [] raw.bytes _

/-- A successful probe identifies a reference offense, and every reference offense
has a hit at or before it. Offsets in the reference list are relative to the base. -/
def ProbeEarliest {length : Nat} (probe : Option (Location length)) (base : Nat)
    (offsets : List Nat) : Prop :=
  (∀ hit, probe = some hit → ∃ i ∈ offsets, hit.offset = base + i) ∧
    (∀ i ∈ offsets, ∃ hit, probe = some hit ∧ hit.offset ≤ base + i)

private theorem earliest_some {length : Nat} (hit : Location length) (base i : Nat)
    (offset : hit.offset = base + i) : ProbeEarliest (some hit) base [i] := by
  subst_vars
  simp [ProbeEarliest, offset]

private theorem earliest_none {length : Nat} (base : Nat) :
    ProbeEarliest (none : Option (Location length)) base [] := by simp [ProbeEarliest]

private theorem startIf_agrees {length : Nat} (part : Component length) (test : Bool) :
    ProbeEarliest (startIf part test) part.start (if test then [0] else []) := by
  cases test with
  | false => exact earliest_none _
  | true => exact earliest_some _ _ _ rfl

private theorem mem_byteOffsets (bytes : ConcreteSpec.Numbers) (test : Nat → Bool) (i : Nat) :
    i ∈ Reference.byteOffsets bytes test ↔ ∃ inside : i < bytes.length, test bytes[i] = true := by
  simp only [Reference.byteOffsets, List.mem_filterMap, Prod.exists,
    List.mk_mem_zipIdx_iff_getElem?, List.getElem?_eq_some_iff]
  constructor
  · rintro ⟨byte, j, ⟨inside, rfl⟩, found⟩
    split at found
    · rename_i passes
      have same := Option.some.inj found
      subst j
      exact ⟨inside, passes⟩
    · contradiction
  · rintro ⟨inside, passes⟩
    exact ⟨bytes[i], i, ⟨inside, rfl⟩, by simp [passes]⟩

/-- Law (subject `ConcreteSpec.Core.SourcePath.firstByte`).
A production byte probe emits the first of all offsets matching its predicate. -/
theorem byte_probe_reference {length : Nat} (reason : Reason) (part : Component length) (test : Nat → Bool) :
    ProbeEarliest (firstByte localPolicy reason part test) part.start (Reference.byteOffsets part.bytes test) := by
  constructor
  · intro hit found
    obtain ⟨index, selected, rfl⟩ := Option.map_eq_some_iff.mp found
    have passes := (List.findFinIdx?_eq_some_iff.mp selected).1
    exact ⟨index.val, (mem_byteOffsets _ _ _).2 ⟨index.isLt, passes⟩, rfl⟩
  · intro i member
    obtain ⟨inside, testAt⟩ := (mem_byteOffsets _ _ _).1 member
    let index : Fin part.bytes.length := ⟨i, inside⟩
    cases found : part.bytes.findFinIdx? test with
    | none =>
      have clear := List.findFinIdx?_eq_none_iff.mp found
      have contradiction := clear _ (List.getElem_mem inside)
      exact False.elim (contradiction testAt)
    | some first =>
      have before : first.val ≤ i := by
        by_cases earlier : i < first.val
        · exact False.elim ((List.findFinIdx?_eq_some_iff.mp found).2 index earlier testAt)
        · omega
      exact ⟨componentLocation part first (Nat.le_of_lt first.isLt),
        by simp [firstByte, localPolicy, found], Nat.add_le_add_left before _⟩

private theorem endpointDot_agrees {length : Nat} (part : Component length) (leading : Bool) :
    ProbeEarliest (endpointDot part leading (· == 46)) part.start
      (Reference.componentOffsets (if leading then .leadingDot else .trailingDot)
        part.start part.bytes) := by
  cases leading <;> simp only [Bool.false_eq_true, ↓reduceIte,
    Reference.componentOffsets, endpointDot, List.head?_eq_getElem?] <;>
    split
  all_goals first
    | exact earliest_none _
    | split
      · exact earliest_some _ _ _ rfl
      · exact earliest_none _

/-- Law (subject `ConcreteSpec.Core.SourcePath.checkComponent`).
Each component probe emits its first applicable reference offense, if any. -/
theorem component_probe_reference {length : Nat} (part : Component length) (reason : Reason) :
    ProbeEarliest (checkComponent reason part) part.start
      (Reference.componentOffsets reason part.start part.bytes) := by
  cases reason with
  | empty | absolute | pathCap => exact earliest_none _
  | traversal | singleDot | reserved => exact startIf_agrees part _
  | emptyComponent =>
    cases raw : part.bytes <;> simp [checkComponent, checkComponentWith, localPolicy,
      Reference.componentOffsets, startIf, raw, ProbeEarliest, componentLocation]
  | nonAscii | backslash | control | space | disallowed => exact byte_probe_reference _ part _
  | leadingDot => exact endpointDot_agrees part true
  | trailingDot => exact endpointDot_agrees part false
  | componentCap =>
    change ProbeEarliest
      (if over : 100 < part.bytes.length then
        some (componentLocation part 100 (Nat.le_of_lt over)) else none)
      part.start (if 100 < part.bytes.length then [100] else [])
    by_cases over : 100 < part.bytes.length
    · rw [dite_eq_left over, ite_eq_left over]
      exact earliest_some _ _ _ rfl
    · rw [dite_eq_right over, ite_eq_right over]
      exact earliest_none _
  | drive =>
    change ProbeEarliest
      (if size : 1 < part.bytes.length then
        if part.start == 0 && (asciiLetter (part.bytes[0]'(by omega)) && part.bytes[1] == 58)
        then some (componentLocation part 1 (Nat.le_of_lt size)) else none
      else none) part.start (Reference.componentOffsets .drive part.start part.bytes)
    by_cases size : 1 < part.bytes.length
    · rw [dite_eq_left size]
      have initial : 0 < part.bytes.length := by omega
      simp only [Reference.componentOffsets, getElem?_pos, initial, size, Option.any_some,
        Option.some_beq_some, Bool.and_assoc]
      let drive := part.start == 0 && (asciiLetter part.bytes[0] && part.bytes[1] == 58)
      change ProbeEarliest
        (if drive then some (componentLocation part 1 (Nat.le_of_lt size)) else none)
        part.start (if drive then [1] else [])
      by_cases isDrive : drive = true
      · rw [ite_eq_left isDrive, ite_eq_left isDrive]
        exact earliest_some _ _ _ rfl
      · rw [ite_eq_right isDrive, ite_eq_right isDrive]
        exact earliest_none _
    · simp [size, Reference.componentOffsets, earliest_none]

private theorem whole_agrees (raw : SourceText) (reason : Reason)
    (whole : reason = .empty ∨ reason = .absolute ∨ reason = .pathCap) :
    ∃ probe, probes raw (components raw) reason = [probe] ∧
      ProbeEarliest probe 0 (Reference.offsets reason raw) := by
  rcases whole with rfl | rfl | rfl
  · by_cases empty : raw.bytes = []
    · exact ⟨_, rfl, by simp [localPolicy, Reference.offsets, empty,
        earliest_some, wholeLocation]⟩
    · exact ⟨_, rfl, by simp [localPolicy, Reference.offsets,
        List.isEmpty_eq_false_iff.mpr empty, earliest_none]⟩
  · by_cases slash : raw.bytes[0]? = some 47
    · exact ⟨_, rfl, by simp [localPolicy, Reference.offsets, slash,
        List.head?_eq_getElem?, earliest_some, wholeLocation]⟩
    · exact ⟨_, rfl, by simp [localPolicy, Reference.offsets, slash,
        List.head?_eq_getElem?, earliest_none]⟩
  · by_cases over : 200 < raw.bytes.length
    · exact ⟨_, rfl, by simp [localPolicy, Reference.offsets, over,
        pathLimit, SourceText.length, earliest_some, wholeLocation]⟩
    · exact ⟨_, rfl, by simp [localPolicy, Reference.offsets, over,
        pathLimit, SourceText.length, earliest_none]⟩

private theorem component_probes (raw : SourceText) (reason : Reason)
    (component : ¬(reason = .empty ∨ reason = .absolute ∨ reason = .pathCap)) :
    probes raw (components raw) reason = (components raw).map (checkComponent reason) ∧
      Reference.offsets reason raw = (components raw).flatMap (fun part =>
        (Reference.componentOffsets reason part.start part.bytes).map (part.start + ·)) := by
  let partOffsets := fun (part : Nat × ConcreteSpec.Numbers) =>
    (Reference.componentOffsets reason part.1 part.2).map (part.1 + ·)
  have ref : Reference.offsets reason raw = (Reference.pieces raw.bytes).flatMap partOffsets := by
    cases reason <;> first | rfl | simp at component
  rw [ref, ← components_reference, List.flatMap_map]
  exact ⟨by cases reason <;> first | rfl | simp at component, rfl⟩

/-- Every emitted candidate denotes a reference offense. Every reference offense has
an emitted candidate for the same rule at its offset or earlier. -/
theorem check_reference (raw : SourceText) (reason : Reason) :
    (∀ hit ∈ check raw (components raw) reason, hit.offset ∈ Reference.offsets reason raw) ∧
    (∀ offset ∈ Reference.offsets reason raw, ∃ hit ∈ check raw (components raw) reason,
      hit.offset ≤ offset) := by
  change (∀ hit ∈ (probes raw (components raw) reason).filterMap id, _) ∧
    (∀ offset ∈ Reference.offsets reason raw, ∃ hit ∈ (probes raw (components raw) reason).filterMap id, _)
  by_cases whole : reason = .empty ∨ reason = .absolute ∨ reason = .pathCap
  · obtain ⟨probe, equal, sound, covers⟩ := whole_agrees raw reason whole
    rw [equal]
    simp only [List.mem_filterMap, List.mem_singleton, exists_eq_left, id_eq]
    constructor
    · intro hit selected
      obtain ⟨i, member, offset⟩ := sound hit selected
      simpa [offset] using member
    · intro i member
      simpa using covers i member
  · obtain ⟨probesEq, offsetsEq⟩ := component_probes raw reason whole
    rw [probesEq, offsetsEq]
    simp only [List.filterMap_map, Function.comp_def, id_eq, List.mem_filterMap, List.mem_flatMap,
      List.mem_map]
    constructor
    · rintro hit ⟨part, member, selected⟩
      obtain ⟨i, offense, offset⟩ := (component_probe_reference part reason).1 hit selected
      exact ⟨part, member, i, offense, offset.symm⟩
    · rintro offset ⟨part, member, i, offense, rfl⟩
      obtain ⟨hit, selected, earlier⟩ := (component_probe_reference part reason).2 i offense
      exact ⟨hit, ⟨part, member, selected⟩, earlier⟩

/-- Law (subject `ConcreteSpec.Core.SourcePath.probes`).
The probes for a rule all pass exactly when its independent byte rule permits the input. -/
theorem permitted_reference_iff (reason : Reason) (raw : SourceText) :
    ProbePermitted reason raw ↔ Permitted reason raw := by
  rw [← check_witness_iff]
  unfold Permitted
  obtain ⟨sound, covers⟩ := check_reference raw reason
  simp only [List.eq_nil_iff_forall_not_mem]
  exact ⟨fun clear offset offense => by
    obtain ⟨hit, present, _⟩ := covers offset offense
    exact clear hit present,
    fun clear hit present => clear _ (sound hit present)⟩

private theorem reference_reason (reason : Reason) : reason ∈ Reference.priority := by
  cases reason <;> decide

/-- The search certificate is equivalent to the conjunction of the independent byte rules. -/
theorem rawOk_reference_iff (raw : SourceText) : RawOk raw ↔ Reference.Valid raw :=
  ⟨fun valid reason _ => (permitted_reference_iff reason raw).1 (valid reason (reason_inOrder reason)),
   fun valid reason _ => (permitted_reference_iff reason raw).2 (valid reason (reference_reason reason))⟩

/-- Law (subject `ConcreteSpec.Core.SourcePath.parse`).
Admission is equivalent to exact original bytes satisfying all sixteen reference rules. -/
theorem parse_iff (raw : SourceText) (path : SourcePath) :
    parse raw = .ok path ↔ path.renderBytes = raw ∧ Reference.Valid raw := by
  rw [parse_probe_iff, rawOk_reference_iff]

/-- Law (subject `ConcreteSpec.Core.SourcePath.parse`).
The parser admits exactly the byte sequences that every independent local rule permits. -/
theorem admitted_iff (raw : SourceText) :
    (∃ path, parse raw = .ok path) ↔ Reference.Valid raw := by
  constructor
  · rintro ⟨path, parsed⟩
    exact ((parse_iff raw path).1 parsed).2
  · intro valid
    exact ⟨⟨raw, (rawOk_reference_iff raw).2 valid⟩, (parse_iff raw _).2 ⟨rfl, valid⟩⟩

/-- The checks other than the named rule, needed for the progress direction. -/
def OtherRules (reason : Reason) (raw : SourceText) : Prop :=
  ∀ rule ∈ Reference.priority, rule ≠ reason → Permitted rule raw

instance otherRulesDecidable (reason : Reason) (raw : SourceText) : Decidable (OtherRules reason raw) := by
  unfold OtherRules
  infer_instance

/-- Law (subject `ConcreteSpec.Core.SourcePath.parse`).
Admission implies this individual rule. Conversely this rule and all remaining rules
suffice for admission. The safety direction places no restriction on overlapping failures. -/
theorem admission_rule_iff (reason : Reason) (raw : SourceText) :
    (∃ path, parse raw = .ok path) ↔ Permitted reason raw ∧ OtherRules reason raw := by
  rw [admitted_iff]
  constructor
  · exact fun valid => ⟨valid reason (reference_reason reason), fun rule member _ => valid rule member⟩
  · rintro ⟨permitted, other⟩ rule member
    by_cases equal : rule = reason
    · exact equal ▸ permitted
    · exact other rule member equal

/-- Law (subject `ConcreteSpec.Core.SourcePath.parse`).
The full path has at least one byte. NonemptyComponents already excludes the empty
input, so deleting this probe is observable through refusal selection.
-/
theorem admission_empty_iff (raw : SourceText) :
    (∃ path, parse raw = .ok path) ↔
      Nonempty raw ∧ OtherRules .empty raw :=
  admission_rule_iff .empty raw

/-- Law (subject `ConcreteSpec.Core.SourcePath.parse`).
The first byte is not a slash. NonemptyComponents already excludes an initial
slash, so deleting this probe is observable through refusal selection.
-/
theorem admission_absolute_iff (raw : SourceText) :
    (∃ path, parse raw = .ok path) ↔
      Relative raw ∧ OtherRules .absolute raw :=
  admission_rule_iff .absolute raw

/-- Law (subject `ConcreteSpec.Core.SourcePath.parse`).
An initial ASCII letter followed by a colon is forbidden. FilenameCharacters
already excludes the colon, so deleting this probe is observable through refusal selection.
-/
theorem admission_drive_iff (raw : SourceText) :
    (∃ path, parse raw = .ok path) ↔
      NoDrivePrefix raw ∧ OtherRules .drive raw :=
  admission_rule_iff .drive raw

/-- Law (subject `ConcreteSpec.Core.SourcePath.parse`).
A parent component is forbidden.
-/
theorem admission_traversal_iff (raw : SourceText) :
    (∃ path, parse raw = .ok path) ↔
      NoTraversal raw ∧ OtherRules .traversal raw :=
  admission_rule_iff .traversal raw

/-- Law (subject `ConcreteSpec.Core.SourcePath.parse`).
A single-dot component is forbidden.
-/
theorem admission_singleDot_iff (raw : SourceText) :
    (∃ path, parse raw = .ok path) ↔
      NoSingleDot raw ∧ OtherRules .singleDot raw :=
  admission_rule_iff .singleDot raw

/-- Law (subject `ConcreteSpec.Core.SourcePath.parse`).
Each slash-delimited component has at least one byte.
-/
theorem admission_emptyComponent_iff (raw : SourceText) :
    (∃ path, parse raw = .ok path) ↔
      NonemptyComponents raw ∧ OtherRules .emptyComponent raw :=
  admission_rule_iff .emptyComponent raw

/-- Law (subject `ConcreteSpec.Core.SourcePath.parse`).
Every byte is ASCII, including when the input is ill-formed UTF-8.
-/
theorem admission_nonAscii_iff (raw : SourceText) :
    (∃ path, parse raw = .ok path) ↔
      Ascii raw ∧ OtherRules .nonAscii raw :=
  admission_rule_iff .nonAscii raw

/-- Law (subject `ConcreteSpec.Core.SourcePath.parse`).
Backslash is forbidden. Slash is the sole separator.
-/
theorem admission_backslash_iff (raw : SourceText) :
    (∃ path, parse raw = .ok path) ↔
      SlashSeparator raw ∧ OtherRules .backslash raw :=
  admission_rule_iff .backslash raw

/-- Law (subject `ConcreteSpec.Core.SourcePath.parse`).
Bytes below 32 and DEL are forbidden.
-/
theorem admission_control_iff (raw : SourceText) :
    (∃ path, parse raw = .ok path) ↔
      NoControls raw ∧ OtherRules .control raw :=
  admission_rule_iff .control raw

/-- Law (subject `ConcreteSpec.Core.SourcePath.parse`).
ASCII space is forbidden.
-/
theorem admission_space_iff (raw : SourceText) :
    (∃ path, parse raw = .ok path) ↔
      NoSpaces raw ∧ OtherRules .space raw :=
  admission_rule_iff .space raw

/-- Law (subject `ConcreteSpec.Core.SourcePath.parse`).
A component cannot start with a dot. Single dot and parent traversal have separate checks.
-/
theorem admission_leadingDot_iff (raw : SourceText) :
    (∃ path, parse raw = .ok path) ↔
      NoLeadingDot raw ∧ OtherRules .leadingDot raw :=
  admission_rule_iff .leadingDot raw

/-- Law (subject `ConcreteSpec.Core.SourcePath.parse`).
A component cannot end with a dot. Single dot and parent traversal have separate checks.
-/
theorem admission_trailingDot_iff (raw : SourceText) :
    (∃ path, parse raw = .ok path) ↔
      NoTrailingDot raw ∧ OtherRules .trailingDot raw :=
  admission_rule_iff .trailingDot raw

/-- Law (subject `ConcreteSpec.Core.SourcePath.parse`).
Windows reserved stems are forbidden, ignoring ASCII case and inspecting only the bytes before the first dot.
-/
theorem admission_reserved_iff (raw : SourceText) :
    (∃ path, parse raw = .ok path) ↔
      NoReservedStem raw ∧ OtherRules .reserved raw :=
  admission_rule_iff .reserved raw

/-- Law (subject `ConcreteSpec.Core.SourcePath.parse`).
Each component is at most 100 bytes.
-/
theorem admission_componentCap_iff (raw : SourceText) :
    (∃ path, parse raw = .ok path) ↔
      ComponentByteCap raw ∧ OtherRules .componentCap raw :=
  admission_rule_iff .componentCap raw

/-- Law (subject `ConcreteSpec.Core.SourcePath.parse`).
The whole path is at most 200 bytes, counting slashes.
-/
theorem admission_pathCap_iff (raw : SourceText) :
    (∃ path, parse raw = .ok path) ↔
      PathByteCap raw ∧ OtherRules .pathCap raw :=
  admission_rule_iff .pathCap raw

/-- Law (subject `ConcreteSpec.Core.SourcePath.parse`).
Component bytes admit exactly ASCII letters, digits, underscore, hyphen and dot, subject to the dedicated character checks.
-/
theorem admission_disallowed_iff (raw : SourceText) :
    (∃ path, parse raw = .ok path) ↔
      FilenameCharacters raw ∧ OtherRules .disallowed raw :=
  admission_rule_iff .disallowed raw

/-- Law (subject `ConcreteSpec.Core.SourcePath.parse`).
Refusal occurs exactly for invalid input. This also covers overlapping checks. -/
theorem parse_refusal_iff (raw : SourceText) :
    (∃ refusal, parse raw = .error refusal) ↔ ¬Reference.Valid raw := by
  rw [← rawOk_reference_iff]
  cases found : firstViolation raw with
  | none => simp [parse_of_clear raw found, (noViolation_iff raw).1 found]
  | some violation =>
    have invalid : ¬RawOk raw := fun valid => by
      have clear := (noViolation_iff raw).2 valid
      simp [found] at clear
    exact ⟨fun _ => invalid, fun _ => ⟨⟨raw, violation⟩, parse_of_hit raw violation found⟩⟩

/-- Law (subject `ConcreteSpec.Core.SourcePath.parse`).
The refusal retains the input and contains a witness of the selected check. -/
theorem parse_refusal_sound (raw : SourceText) (refusal : Refusal)
    (failed : parse raw = .error refusal) :
    refusal.raw = raw ∧
      refusal.violation.location ∈ check refusal.raw (components refusal.raw) refusal.violation.reason := by
  cases found : firstViolation raw with
  | none => simp [parse_of_clear raw found] at failed
  | some violation =>
    have equal : (⟨raw, violation⟩ : Refusal) = refusal := by simpa [parse_of_hit raw violation found] using failed
    subst refusal
    exact ⟨rfl, (candidate_iff raw violation).1 (List.minOn?_mem found)⟩

/-- Law (subject `ConcreteSpec.Core.SourcePath.parse`, `ConcreteSpec.Core.SourcePath.renderBytes`).
Parsing the exact bytes of a checked path returns that same path. -/
theorem parse_render (path : SourcePath) : parse path.renderBytes = .ok path :=
  (parse_probe_iff _ _).2 ⟨rfl, path.valid⟩

/-- Law (subject `ConcreteSpec.Core.SourcePath.renderBytes`).
The original bytes determine the checked identity. -/
theorem renderBytes_injective {left right : SourcePath} (same : left.renderBytes = right.renderBytes) :
    left = right := by
  cases left
  cases right
  simpa [renderBytes] using same

/-- Law (subject `ConcreteSpec.Core.SourcePath.Reason.inOrder`).
The production tie-break list is exactly the design priority list. -/
theorem priority_reference : Reason.inOrder = Reference.priority := rfl

private theorem priority_lt_count (reason : Reason) :
    Reason.inOrder.idxOf reason < Reason.inOrder.length := by
  cases reason <;> decide

private theorem candidate_precedes (raw : SourceText) (chosen other : Violation raw.length)
    (selected : firstViolation raw = some chosen) (present : other ∈ candidates raw) :
    Reference.Precedes chosen.reason chosen.location.offset other.reason other.location.offset := by
  have minimum := List.apply_get_minOn?_le_of_mem (f := Violation.key) present
  change Violation.key ((firstViolation raw).get _) ≤ Violation.key other at minimum
  simp only [selected, Option.get_some] at minimum
  have chosenBound := priority_lt_count chosen.reason
  have otherBound := priority_lt_count other.reason
  have count : Reference.priority.length = 16 := rfl
  simp only [Violation.key, priority_reference, count] at minimum chosenBound otherBound
  unfold Reference.Precedes
  omega

/-- Law (subject `ConcreteSpec.Core.SourcePath.firstViolation`).
The selected offense precedes every applicable reference offense, including bytes
that were not emitted as candidates. -/
theorem firstViolation_order (raw : SourceText) (chosen : Violation raw.length)
    (selected : firstViolation raw = some chosen) :
    Reference.IsFirst raw chosen.reason chosen.location.offset := by
  refine ⟨(check_reference raw chosen.reason).1 _ ((candidate_iff raw chosen).1
    (List.minOn?_mem selected)), ?_⟩
  intro reason offset offense
  obtain ⟨hit, member, before⟩ := (check_reference raw reason).2 offset offense
  have order := candidate_precedes raw chosen ⟨reason, hit⟩ selected
    ((candidate_iff raw ⟨reason, hit⟩).2 member)
  simp only [Reference.Precedes] at order ⊢
  rcases order with earlier | ⟨same, priority⟩
  · exact Or.inl (Nat.lt_of_lt_of_le earlier before)
  · by_cases tied : chosen.location.offset = offset
    · exact Or.inr ⟨tied, priority⟩
    · exact Or.inl (by omega)

/-- Law (subject `ConcreteSpec.Core.SourcePath.parse`).
A refusal reports an applicable offense at the earliest byte, with ties resolved by
the independent design priority. -/
theorem parse_refusal_order (raw : SourceText) (refusal : Refusal)
    (failed : parse raw = .error refusal) :
    Reference.IsFirst raw refusal.violation.reason refusal.violation.location.offset := by
  cases found : firstViolation raw with
  | none => simp [parse_of_clear raw found] at failed
  | some violation =>
    have same : (⟨raw, violation⟩ : Refusal) = refusal := by
      simpa [parse_of_hit raw violation found] using failed
    subst refusal
    exact firstViolation_order raw violation found

/-- Law (subject `ConcreteSpec.Core.SourcePath.probes`).
The whole-path cap passes exactly when there are at most 200 bytes, including slash bytes. -/
theorem pathCap_iff (raw : SourceText) : PathByteCap raw ↔ raw.length ≤ pathLimit := by
  simp [PathByteCap, Permitted, Reference.offsets, pathLimit, SourceText.length]

/-- Law (subject `ConcreteSpec.Core.SourcePath.probes`).
The component cap passes exactly when every component has at most 100 bytes. -/
theorem componentCap_iff (raw : SourceText) :
    ComponentByteCap raw ↔ ∀ part ∈ components raw, part.bytes.length ≤ componentLimit := by
  change Permitted .componentCap raw ↔ _
  rw [← permitted_reference_iff]
  simp only [ProbePermitted, probes, probesWith, List.forall_mem_map, checkComponentWith, localPolicy]
  constructor
  · intro clear part member
    by_cases over : componentLimit < part.bytes.length
    · have noHit := clear part member
      simp [over] at noHit
    · exact Nat.le_of_not_gt over
  · intro bounded part member
    simp [Nat.not_lt.mpr (bounded part member)]

/-- Law (subject `ConcreteSpec.Core.SourcePath.parse`).
Every admitted path has at most 200 bytes, including its slash separators. -/
theorem path_bytes_le (path : SourcePath) : path.renderBytes.length ≤ pathLimit :=
  (pathCap_iff path.raw).1 ((permitted_reference_iff .pathCap path.raw).1
    (path.valid .pathCap (reason_inOrder _)))

/-- Law (subject `ConcreteSpec.Core.SourcePath.parse`).
Every component of an admitted path has at most 100 bytes. -/
theorem component_bytes_le (path : SourcePath) (part : Component path.raw.length)
    (member : part ∈ components path.raw) : part.bytes.length ≤ componentLimit :=
  (componentCap_iff path.raw).1 ((permitted_reference_iff .componentCap path.raw).1
    (path.valid .componentCap (reason_inOrder _))) part member

/-- Law (subject `ConcreteSpec.Core.SourcePath.checkComponent`).
A cap witness denotes the first byte in excess of the component limit, strictly inside
the offending component, in whole-path coordinates. -/
theorem component_cap_offset {length : Nat} (part : Component length) (hit : Location length)
    (found : checkComponent .componentCap part = some hit) :
    hit.start = part.start ∧ hit.stop = part.start + part.bytes.length ∧
      hit.offset = part.start + componentLimit ∧ hit.offset < hit.stop := by
  change (if h : componentLimit < part.bytes.length then
    some (componentLocation part componentLimit (Nat.le_of_lt h)) else none) = some hit at found
  by_cases over : componentLimit < part.bytes.length
  · rw [dite_eq_left over, Option.some.injEq] at found
    subst hit
    exact ⟨rfl, rfl, rfl, Nat.add_lt_add_left over _⟩
  · simp [over] at found

private theorem scan_split {length : Nat} (start : Nat) (rev rest : ConcreteSpec.Numbers)
    (total : start + rev.length + rest.length = length) :
    (scan start rev rest total).map Component.bytes = rest.splitOnPPrepend (· == 47) rev := by
  induction rest generalizing start rev with
  | nil => simp [scan]
  | cons byte rest ih =>
    by_cases slash : byte = 47
    · simp [scan, List.splitOnPPrepend, slash, ih]
    · simp [scan, List.splitOnPPrepend, slash, ih]

/-- Law (subject `ConcreteSpec.Core.SourcePath.components`).
Slash-delimited components reconstruct exactly the original byte sequence. -/
theorem components_exact (raw : SourceText) :
    [47].intercalate ((components raw).map Component.bytes) = raw.bytes := by
  change [47].intercalate ((scan (length := raw.bytes.length) 0 [] raw.bytes _).map
    Component.bytes) = raw.bytes
  rw [scan_split]
  exact List.intercalate_splitOn 47

-- A scanned component starts and ends at the input boundaries or at slash delimiters.
private theorem scan_partition {length : Nat} (start : Nat) (rev rest : ConcreteSpec.Numbers)
    (total : start + rev.length + rest.length = length) (part : Component length)
    (member : part ∈ scan start rev rest total) :
    ∃ before after : ConcreteSpec.Numbers,
      rev.reverse ++ rest = before ++ part.bytes ++ after ∧
      start + before.length = part.start ∧
      (before = [] ∨ before.getLast? = some 47) ∧ (after = [] ∨ after.head? = some 47) := by
  induction rest generalizing start rev with
  | nil =>
    simp [scan] at member
    subst part
    exact ⟨[], [], by simp, by simp, Or.inl rfl, Or.inl rfl⟩
  | cons byte rest ih =>
    by_cases slash : byte = 47
    · subst byte
      simp only [scan, ↓reduceIte, List.mem_cons] at member
      rcases member with rfl | later
      · exact ⟨[], 47 :: rest, by simp, by simp, Or.inl rfl, Or.inr rfl⟩
      · obtain ⟨before, after, partition, position, left, right⟩ := ih _ [] _ later
        simp only [List.reverse_nil, List.nil_append] at partition
        refine ⟨rev.reverse ++ [47] ++ before, after, ?_, ?_, Or.inr ?_, right⟩
        · rw [partition]
          simp [List.append_assoc]
        · simp only [List.length_append, List.length_reverse, List.length_cons, List.length_nil]
          omega
        · rcases left with rfl | delimiter
          · simp
          · rw [List.getLast?_append, delimiter]
            rfl
    · simp only [scan, slash, ↓reduceIte] at member
      simpa [List.reverse_cons, List.append_assoc] using ih _ _ _ member

/-- Helper: the exact component slice is delimited by slashes or by the ends of the input. -/
theorem component_partition (raw : SourceText) (part : Component raw.length)
    (member : part ∈ components raw) :
    ∃ before after : ConcreteSpec.Numbers,
      raw.bytes = before ++ part.bytes ++ after ∧ before.length = part.start ∧
      (before = [] ∨ before.getLast? = some 47) ∧ (after = [] ∨ after.head? = some 47) := by
  simpa using scan_partition 0 [] raw.bytes _ part member

/-- Law (subject `ConcreteSpec.Core.SourcePath.components`).
Each component is the exact original slice at its reported byte range. -/
theorem component_slice (raw : SourceText) (part : Component raw.length)
    (member : part ∈ components raw) :
    (raw.bytes.drop part.start).take part.bytes.length = part.bytes := by
  obtain ⟨before, after, partition, position, _, _⟩ := component_partition raw part member
  rw [← position, partition, List.append_assoc, List.drop_append_length]
  simp

/-- Law (subject `ConcreteSpec.Core.SourcePath.checkComponent`).
The selected cap byte in the original path is byte 100 of the offending component. -/
theorem component_cap_byte (raw : SourceText) (part : Component raw.length)
    (member : part ∈ components raw) (hit : Location raw.length)
    (found : checkComponent .componentCap part = some hit) :
    raw.bytes[hit.offset]? = part.bytes[componentLimit]? ∧
      componentLimit < part.bytes.length := by
  obtain ⟨_, stop, offset, inside⟩ := component_cap_offset part hit found
  have over : componentLimit < part.bytes.length := by omega
  have slice := component_slice raw part member
  refine ⟨?_, over⟩
  rw [offset, ← List.getElem?_drop]
  have atIndex := congrArg (fun bytes : List Nat => bytes[componentLimit]?) slice
  simpa [List.getElem?_take, over] using atIndex

/-- The reason-specific condition at the component-relative byte or boundary. -/
def ComponentOffense (reason : Reason) (start : Nat) (bytes : ConcreteSpec.Numbers)
    (index : Nat) : Prop :=
  match reason with
  | .drive => start = 0 ∧ index = 1 ∧ bytes[1]? = some 58 ∧
      ∃ letter, bytes[0]? = some letter ∧ asciiLetter letter = true
  | .traversal => index = 0 ∧ bytes = [46, 46]
  | .singleDot => index = 0 ∧ bytes = [46]
  | .emptyComponent => index = 0 ∧ bytes = []
  | .nonAscii => ∃ byte, bytes[index]? = some byte ∧ 128 ≤ byte
  | .backslash => bytes[index]? = some 92
  | .control => ∃ byte, bytes[index]? = some byte ∧ (byte < 32 ∨ byte = 127)
  | .space => bytes[index]? = some 32
  | .leadingDot => index = 0 ∧ bytes[index]? = some 46
  | .trailingDot => index = bytes.length - 1 ∧ bytes[index]? = some 46
  | .reserved => index = 0 ∧ reservedStem bytes = true
  | .componentCap => index = componentLimit ∧ componentLimit < bytes.length
  | .disallowed => ∃ byte, bytes[index]? = some byte ∧
      32 < byte ∧ byte < 127 ∧ byte ≠ 92 ∧ filenameByte byte = false
  | .empty | .absolute | .pathCap => False

private theorem firstByte_witness {length : Nat} (reason : Reason) (part : Component length)
    (test : Nat → Bool) (hit : Location length) (found : firstByte localPolicy reason part test = some hit) :
    ∃ index : Fin part.bytes.length, hit = componentLocation part index (Nat.le_of_lt index.isLt) ∧
      test part.bytes[index] = true := by
  obtain ⟨index, selected, equal⟩ := Option.map_eq_some_iff.mp found
  exact ⟨index, equal.symm, (List.findFinIdx?_eq_some_iff.mp selected).1⟩

private theorem startIf_witness {length : Nat} (part : Component length) (test : Bool)
    (hit : Location length) (found : startIf part test = some hit) :
    hit = componentLocation part 0 (Nat.zero_le _) ∧ test = true := by
  cases test <;> simp_all [startIf]

private theorem endpointDot_witness {length : Nat} (part : Component length) (leading : Bool)
    (hit : Location length) (found : endpointDot part leading (· == 46) = some hit) :
    ∃ index, ∃ bound : index ≤ part.bytes.length,
      hit = componentLocation part index bound ∧
      index = (if leading then 0 else part.bytes.length - 1) ∧ part.bytes[index]? = some 46 := by
  unfold endpointDot at found
  split at found
  · contradiction
  · let endpoint := if leading then part.bytes.head? else part.bytes.getLast?
    by_cases selected : endpoint.any (· == 46) = true
    · obtain ⟨byte, atEnd, isDot⟩ := (Option.any_eq_true _ _).1 selected
      have dot : byte = 46 := beq_iff_eq.mp isDot
      subst byte
      rw [ite_eq_left selected, Option.some.injEq] at found
      refine ⟨_, _, found.symm, rfl, ?_⟩
      cases leading <;> simpa [endpoint, List.head?_eq_getElem?, List.getLast?_eq_getElem?] using atEnd
    · rw [ite_eq_right selected] at found
      contradiction

private theorem component_offense {length : Nat} (part : Component length) (reason : Reason)
    (hit : Location length) (found : checkComponent reason part = some hit) :
    ∃ index, ∃ bound : index ≤ part.bytes.length,
      hit = componentLocation part index bound ∧ ComponentOffense reason part.start part.bytes index := by
  cases reason with
  | empty | absolute | pathCap => contradiction
  | drive =>
    simp only [checkComponent, checkComponentWith, localPolicy] at found
    by_cases size : 1 < part.bytes.length
    · simp only [dite_eq_left size] at found
      change (if part.start == 0 && (asciiLetter (part.bytes[0]'(by omega)) && part.bytes[1] == 58)
        then some (componentLocation part 1 (Nat.le_of_lt size)) else none) = some hit at found
      by_cases drive : (part.start == 0 && (asciiLetter (part.bytes[0]'(by omega)) &&
          part.bytes[1] == 58)) = true
      · rw [ite_eq_left drive] at found
        simp only [Bool.and_eq_true, beq_iff_eq] at drive
        rcases drive with ⟨start, letter, colon⟩
        injection found with equal
        exact ⟨1, Nat.le_of_lt size, equal.symm, start, rfl,
          (List.getElem?_eq_some_iff.mpr ⟨size, colon⟩),
          part.bytes[0]'(by omega), by simp, letter⟩
      · rw [ite_eq_right drive] at found
        contradiction
    · simp [size] at found
  | traversal | singleDot | emptyComponent | reserved =>
    simp only [checkComponent, checkComponentWith, localPolicy] at found
    obtain ⟨equal, test⟩ := startIf_witness part _ hit found
    exact ⟨0, Nat.zero_le _, equal, rfl, by simpa using test⟩
  | nonAscii | backslash | control | space | disallowed =>
    simp only [checkComponent, checkComponentWith, localPolicy] at found
    obtain ⟨index, equal, test⟩ := firstByte_witness _ part _ hit found
    refine ⟨index, Nat.le_of_lt index.isLt, equal, ?_⟩
    simp [ComponentOffense, and_assoc] at test ⊢
    exact test
  | leadingDot | trailingDot =>
    simp only [checkComponent, checkComponentWith, localPolicy] at found
    obtain ⟨index, bound, equal, endpoint, dot⟩ := endpointDot_witness part _ hit found
    exact ⟨index, bound, equal, endpoint, dot⟩
  | componentCap =>
    obtain ⟨start, stop, offset, inside⟩ := component_cap_offset part hit found
    have over : componentLimit < part.bytes.length := by omega
    refine ⟨componentLimit, Nat.le_of_lt over, ?_, rfl, over⟩
    cases hit
    simp_all [componentLocation]

/-- Helper: the empty input and an initial slash each emit an empty first component. -/
theorem nonemptyComponents_implies_nonempty_relative (raw : SourceText)
    (valid : NonemptyComponents raw) : Nonempty raw ∧ Relative raw := by
  change Permitted .emptyComponent raw at valid
  rw [← permitted_reference_iff] at valid
  change Permitted .empty raw ∧ Permitted .absolute raw
  simp only [← permitted_reference_iff]
  have neither : raw.bytes ≠ [] ∧ raw.bytes.head? ≠ some 47 := by
    obtain ⟨bytes, bounded⟩ := raw
    cases bytes with
    | nil =>
      simp [ProbePermitted, probes, probesWith, components, scan,
        checkComponentWith, localPolicy, startIf] at valid
    | cons byte rest =>
      refine ⟨by simp, ?_⟩
      intro initialSlash
      have slash : byte = 47 := by simpa using initialSlash
      subst byte
      simp [ProbePermitted, probes, probesWith, components, scan,
        checkComponentWith, localPolicy, startIf] at valid
  rcases neither with ⟨nonempty, relative⟩
  constructor
  · simp [ProbePermitted, probes, probesWith, localPolicy,
      List.isEmpty_eq_false_iff.mpr nonempty]
  · simp [ProbePermitted, probes, probesWith, localPolicy, relative]

/-- Helper: a drive prefix necessarily has a colon that fails the filename whitelist. -/
theorem filenameCharacters_implies_noDrivePrefix (raw : SourceText)
    (valid : FilenameCharacters raw) : NoDrivePrefix raw := by
  change Permitted .disallowed raw at valid
  rw [← permitted_reference_iff] at valid
  change Permitted .drive raw
  rw [← permitted_reference_iff]
  change ∀ probe ∈ (components raw).map (checkComponent .drive), probe = none
  intro probe member
  obtain ⟨part, partMember, rfl⟩ := List.mem_map.mp member
  cases found : checkComponent .drive part with
  | none => rfl
  | some hit =>
    obtain ⟨_, _, _, offense⟩ := component_offense part .drive hit found
    obtain ⟨_, _, colon, _⟩ := offense
    have clear := valid (checkComponent .disallowed part) (List.mem_map_of_mem partMember)
    change (part.bytes.findFinIdx? localPolicy.disallowed).map _ = none at clear
    have allowed := List.findFinIdx?_eq_none_iff.mp (Option.map_eq_none_iff.mp clear)
    have passes := allowed 58 (List.mem_of_getElem? colon)
    simp [localPolicy, filenameByte, asciiLetter] at passes

private theorem checkComponent_member (raw : SourceText) (reason : Reason)
    (hit : Location raw.length)
    (found : hit ∈ ((components raw).map (checkComponent reason)).filterMap id) :
    ∃ part ∈ components raw, checkComponent reason part = some hit := by
  obtain ⟨probe, member, selected⟩ := List.mem_filterMap.mp found
  obtain ⟨part, partMember, equal⟩ := List.mem_map.mp member
  exact ⟨part, partMember, equal ▸ selected⟩

/-- Meaning of the exact byte range and offset reported by the parser. Whole-path checks
name the entire input. Component checks name an original slash-delimited component and
identify the offending byte or boundary within it. -/
def Refusal.LocationOk (refusal : Refusal) : Prop :=
  let raw := refusal.raw
  let hit := refusal.violation.location
  match refusal.violation.reason with
  | .empty => hit.start = 0 ∧ hit.stop = 0 ∧ hit.offset = 0 ∧ raw.bytes = [] ∧ refusal.contextBytes = []
  | .absolute => hit.start = 0 ∧ hit.stop = raw.length ∧ hit.offset = 0 ∧
      raw.bytes[0]? = some 47 ∧ refusal.contextBytes = raw.bytes
  | .pathCap => hit.start = 0 ∧ hit.stop = raw.length ∧ hit.offset = pathLimit ∧
      pathLimit < raw.length ∧ refusal.contextBytes = raw.bytes
  | reason =>
    ∃ part ∈ components raw, ∃ index,
      hit.start = part.start ∧ hit.stop = part.start + part.bytes.length ∧
      hit.offset = part.start + index ∧ refusal.contextBytes = part.bytes ∧
      ComponentOffense reason part.start part.bytes index ∧
      (∀ i, i < part.bytes.length → raw.bytes[part.start + i]? = part.bytes[i]?) ∧
      (∃ before after : ConcreteSpec.Numbers,
        raw.bytes = before ++ part.bytes ++ after ∧ before.length = part.start ∧
        (before = [] ∨ before.getLast? = some 47) ∧ (after = [] ∨ after.head? = some 47))

/-- Law (subject `ConcreteSpec.Core.SourcePath.parse`).
Every refusal points to the actual offending byte or boundary and preserves the exact original context. -/
theorem parse_refusal_location (raw : SourceText) (refusal : Refusal)
    (failed : parse raw = .error refusal) : refusal.LocationOk := by
  have found := (parse_refusal_sound raw refusal failed).2
  obtain ⟨raw, ⟨reason, hit⟩⟩ := refusal
  cases reason with
  | empty =>
    simp only [check, checkWith, probesWith, localPolicy] at found
    by_cases empty : raw.bytes = []
    · have same : hit = wholeLocation raw 0 (Nat.zero_le _) := by simpa [empty] using found
      subst hit
      simp [Refusal.LocationOk, Refusal.contextBytes, wholeLocation, SourceText.length, empty]
    · simp [List.isEmpty_eq_false_iff.mpr empty] at found
  | absolute =>
    simp only [check, checkWith, probesWith, localPolicy] at found
    by_cases slash : raw.bytes.head? = some 47
    · have same : hit = wholeLocation raw 0 (Nat.zero_le _) := by simpa [slash] using found
      subst hit
      simp [Refusal.LocationOk, Refusal.contextBytes, wholeLocation, SourceText.length,
        ← List.head?_eq_getElem?, slash]
    · simp [slash] at found
  | pathCap =>
    simp only [check, checkWith, probesWith, localPolicy] at found
    by_cases over : pathLimit < raw.length
    · have same : hit = wholeLocation raw pathLimit (Nat.le_of_lt over) := by simpa [over] using found
      subst hit
      simpa [Refusal.LocationOk, Refusal.contextBytes, wholeLocation, SourceText.length] using over
    · simp [over] at found
  | drive | traversal | singleDot | emptyComponent | nonAscii | backslash | control | space |
    leadingDot | trailingDot | reserved | componentCap | disallowed =>
    obtain ⟨part, member, selected⟩ := checkComponent_member raw _ hit found
    obtain ⟨index, bound, equal, offense⟩ := component_offense part _ hit selected
    clear failed found selected
    rw [equal]
    have slice := component_slice raw part member
    refine ⟨part, member, index, rfl, rfl, rfl, ?_, offense, ?_, component_partition raw part member⟩
    · simpa [Refusal.contextBytes, componentLocation] using slice
    · intro i inside
      have atIndex := congrArg (fun bytes : List Nat => bytes[i]?) slice
      simpa [List.getElem?_take, List.getElem?_drop, inside] using atIndex

private theorem intercalate_ascii (parts : List ConcreteSpec.Numbers)
    (ascii : ∀ part ∈ parts, ∀ byte ∈ part, byte < 128) :
    ∀ byte ∈ [47].intercalate parts, byte < 128 := by
  induction parts with
  | nil => simp
  | cons part rest ih =>
    cases rest with
    | nil => simpa using ascii part (by simp)
    | cons head tail =>
      rw [List.intercalate_cons_of_ne_nil (by simp)]
      intro byte member
      simp only [List.mem_append, List.mem_singleton] at member
      rcases member with (inPart | separator) | inRest
      · exact ascii part (by simp) byte inPart
      · omega
      · exact ih (fun p hp => ascii p (List.mem_cons_of_mem _ hp)) byte inRest

/-- Law (subject `ConcreteSpec.Core.SourcePath.parse`).
All bytes in a checked path are ASCII. -/
theorem ascii_bytes (path : SourcePath) : ∀ byte ∈ path.raw.bytes, byte < 128 := by
  have clear := path.valid .nonAscii (reason_inOrder _)
  rw [← components_exact path.raw]
  apply intercalate_ascii
  intro bytes member byte inBytes
  obtain ⟨part, inPath, rfl⟩ := List.mem_map.mp member
  have noHit := clear (checkComponent .nonAscii part) (List.mem_map_of_mem inPath)
  change (part.bytes.findFinIdx? (fun b => decide (128 ≤ b))).map _ = none at noHit
  have allAscii := List.findFinIdx?_eq_none_iff.mp (Option.map_eq_none_iff.mp noHit)
  simpa using allAscii byte inBytes

private theorem charOfNat_value (byte : Nat) (ascii : byte < 128) :
    (Char.ofNat byte).val = UInt32.ofNat byte := by
  unfold Char.ofNat
  split
  · apply UInt32.toNat_inj.mp
    change (UInt32.ofNatLT byte _).toNat = (UInt32.ofNat byte).toNat
    rw [UInt32.toNat_ofNatLT, UInt32.toNat_ofNat', Nat.mod_eq_of_lt (show byte < 2 ^ 32 by omega)]
  · rename_i invalid
    exact False.elim (invalid (Or.inl (by omega)))

private theorem ascii_utf8 (bytes : ConcreteSpec.Numbers) (ascii : ∀ byte ∈ bytes, byte < 128) :
    (bytes.map Char.ofNat).utf8Encode.data.toList = bytes.map UInt8.ofNat := by
  induction bytes with
  | nil => rfl
  | cons byte rest ih =>
    have bound := ascii byte (by simp)
    have value := charOfNat_value byte bound
    have size : (Char.ofNat byte).utf8Size = 1 := by
      apply Char.utf8Size_eq_one_iff.mpr
      rw [value, UInt32.le_iff_toNat_le, UInt32.toNat_ofNat',
        Nat.mod_eq_of_lt (show byte < 2 ^ 32 by omega)]
      change byte ≤ 127
      omega
    rw [List.map_cons, List.utf8Encode_cons, List.utf8Encode_singleton,
      String.utf8EncodeChar_eq_singleton size]
    simp [value, ih (fun b hb => ascii b (List.mem_cons_of_mem _ hb))]

/-- Law (subject `ConcreteSpec.Core.SourcePath.render`).
The UTF-8 bytes of the rendered name are the original admitted bytes. -/
theorem render_exact (path : SourcePath) :
    SourceText.ofBytes path.render.text.toByteArray.data.toList = path.renderBytes := by
  rw [← SourceText.ofBytes_toBytes path.renderBytes]
  congr 1
  simp only [render, renderBytes, String.toByteArray_ofList, SourceText.toBytes]
  exact ascii_utf8 _ (ascii_bytes path)

/-- Law (subject `ConcreteSpec.Core.SourcePath.render`).
Equal rendered names identify equal paths. -/
theorem render_injective {left right : SourcePath} (same : left.render = right.render) :
    left = right := by
  apply renderBytes_injective
  rw [← render_exact left, ← render_exact right, same]

/-- Law (subject `ConcreteSpec.Core.SourcePath.parseString`, `ConcreteSpec.Core.SourcePath.render`).
Parsing the rendered name returns the original checked path. -/
theorem parseString_render (path : SourcePath) : parseString path.render.text = .ok path := by
  rw [parseString, render_exact, parse_render]

end SourcePath

/-- Helper: a source span carries the checked path invariant. -/
theorem Span.pathOk (span : Span) : span.PathOk := span.path.valid

/-- Representation lemma (subject `ConcreteSpec.Core.Span.LocationOk`).
A valid span starts within the source and stores the exact line and byte column at that start. -/
theorem Span.locationOk_iff (span : Span) (source : SourceText) :
    span.LocationOk source ↔
      span.startOffset ≤ span.endOffset ∧ span.endOffset ≤ source.length ∧
      span.line = source.lineAt span.startOffset ∧ span.col = source.columnAt span.startOffset := by
  simp [Span.LocationOk, span.range_ok]

/-- Instance (law `ConcreteSpec.Core.Span.locationOk_iff`, case `ConcreteSpec.Core.tabSpan`).
the span after a tab matches the original source's byte coordinates. -/
theorem tabSpan_location : tabSpan.LocationOk tabSource :=
  (Span.locationOk_iff _ _).2 (by decide)

/-- Instance (law `ConcreteSpec.Core.Span.locationOk_iff`, case `ConcreteSpec.Core.Vacuity.otherTabSource`).
the unchanged span fails for the source with a different line structure. -/
theorem tabSpan_otherSource : ¬tabSpan.LocationOk Vacuity.otherTabSource := by
  rw [Span.locationOk_iff]
  decide

/-- Instance (law `ConcreteSpec.Core.Span.locationOk_iff`, case `ConcreteSpec.Core.tabSpan`).
an incorrect column or an out-of-bounds end invalidates the span. -/
theorem tabSpan_badCoordinates :
    ¬({ tabSpan with col := 2, col_pos := by decide }).LocationOk tabSource ∧
    ¬({ tabSpan with endOffset := 4, range_ok := by decide }).LocationOk tabSource := by
  simp only [Span.locationOk_iff]
  decide

namespace SourcePath

namespace LocalControls

open Registry

/-- The selected reason, half-open range, byte offset and exact context. -/
def reportedLocation (result : Except Refusal SourcePath) :
    Option (Reason × Nat × Nat × Nat × ConcreteSpec.Numbers) :=
  match result with
  | .ok _ => none
  | .error refusal =>
    let location := refusal.violation.location
    some (refusal.violation.reason, location.start, location.stop, location.offset, refusal.contextBytes)

/-- Read the selected reason and byte offset from a production parse. -/
def reported (result : Except Refusal SourcePath) : Option (Reason × Nat) :=
  match result with
  | .ok _ => none
  | .error refusal => some (refusal.violation.reason, refusal.violation.location.offset)

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_empty_iff`, case `ConcreteSpec.Core.SourcePath.Registry.mixedCaseInput`). -/
theorem emptyAdmitted : ∃ path, parse mixedCaseInput = .ok path :=
  (admission_empty_iff _).2 ⟨emptyAccepted, by decide⟩

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_empty_iff`, case `ConcreteSpec.Core.SourcePath.Registry.emptyRefusedInput`). -/
theorem emptyRejected : ¬∃ path, parse emptyRefusedInput = .ok path :=
  fun admitted => emptyRefused ((admission_empty_iff _).1 admitted).1

/-- Instance (law `ConcreteSpec.Core.SourcePath.parse_refusal_location`, case `ConcreteSpec.Core.SourcePath.Registry.emptyRefusedInput`). -/
theorem emptyReported :
    reportedLocation (parse emptyRefusedInput) = some (.empty, 0, 0, 0, []) := by decide

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_absolute_iff`, case `ConcreteSpec.Core.SourcePath.Registry.mixedCaseInput`). -/
theorem absoluteAdmitted : ∃ path, parse mixedCaseInput = .ok path :=
  (admission_absolute_iff _).2 ⟨absoluteAccepted, by decide⟩

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_absolute_iff`, case `ConcreteSpec.Core.SourcePath.Registry.absoluteRefusedInput`). -/
theorem absoluteRejected : ¬∃ path, parse absoluteRefusedInput = .ok path :=
  fun admitted => absoluteRefused ((admission_absolute_iff _).1 admitted).1

/-- Instance (law `ConcreteSpec.Core.SourcePath.parse_refusal_location`, case `ConcreteSpec.Core.SourcePath.Registry.absoluteRefusedInput`). -/
theorem absoluteReported :
    reportedLocation (parse absoluteRefusedInput) = some (.absolute, 0, 2, 0, [47, 65]) := by decide

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_drive_iff`, case `ConcreteSpec.Core.SourcePath.Registry.mixedCaseInput`). -/
theorem driveAdmitted : ∃ path, parse mixedCaseInput = .ok path :=
  (admission_drive_iff _).2 ⟨driveAccepted, by decide⟩

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_drive_iff`, case `ConcreteSpec.Core.SourcePath.Registry.driveRefusedInput`). -/
theorem driveRejected : ¬∃ path, parse driveRefusedInput = .ok path :=
  fun admitted => driveRefused ((admission_drive_iff _).1 admitted).1

/-- Instance (law `ConcreteSpec.Core.SourcePath.parse_refusal_location`, case `ConcreteSpec.Core.SourcePath.Registry.driveRefusedInput`). -/
theorem driveReported :
    reportedLocation (parse driveRefusedInput) = some (.drive, 0, 2, 1, [67, 58]) := by decide

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_traversal_iff`, case `ConcreteSpec.Core.SourcePath.Registry.mixedCaseInput`). -/
theorem traversalAdmitted : ∃ path, parse mixedCaseInput = .ok path :=
  (admission_traversal_iff _).2 ⟨traversalAccepted, by decide⟩

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_traversal_iff`, case `ConcreteSpec.Core.SourcePath.Registry.traversalRefusedInput`). -/
theorem traversalRejected : ¬∃ path, parse traversalRefusedInput = .ok path :=
  fun admitted => traversalRefused ((admission_traversal_iff _).1 admitted).1

/-- Instance (law `ConcreteSpec.Core.SourcePath.parse_refusal_location`, case `ConcreteSpec.Core.SourcePath.Registry.traversalRefusedInput`). -/
theorem traversalReported :
    reportedLocation (parse traversalRefusedInput) = some (.traversal, 2, 4, 2, [46, 46]) := by decide

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_singleDot_iff`, case `ConcreteSpec.Core.SourcePath.Registry.mixedCaseInput`). -/
theorem singleDotAdmitted : ∃ path, parse mixedCaseInput = .ok path :=
  (admission_singleDot_iff _).2 ⟨singleDotAccepted, by decide⟩

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_singleDot_iff`, case `ConcreteSpec.Core.SourcePath.Registry.singleDotRefusedInput`). -/
theorem singleDotRejected : ¬∃ path, parse singleDotRefusedInput = .ok path :=
  fun admitted => singleDotRefused ((admission_singleDot_iff _).1 admitted).1

/-- Instance (law `ConcreteSpec.Core.SourcePath.parse_refusal_location`, case `ConcreteSpec.Core.SourcePath.Registry.singleDotRefusedInput`). -/
theorem singleDotReported :
    reportedLocation (parse singleDotRefusedInput) = some (.singleDot, 2, 3, 2, [46]) := by decide

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_emptyComponent_iff`, case `ConcreteSpec.Core.SourcePath.Registry.mixedCaseInput`). -/
theorem emptyComponentAdmitted : ∃ path, parse mixedCaseInput = .ok path :=
  (admission_emptyComponent_iff _).2 ⟨emptyComponentAccepted, by decide⟩

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_emptyComponent_iff`, case `ConcreteSpec.Core.SourcePath.Registry.emptyComponentRefusedInput`). -/
theorem emptyComponentRejected : ¬∃ path, parse emptyComponentRefusedInput = .ok path :=
  fun admitted => emptyComponentRefused ((admission_emptyComponent_iff _).1 admitted).1

/-- Instance (law `ConcreteSpec.Core.SourcePath.parse_refusal_location`, case `ConcreteSpec.Core.SourcePath.Registry.emptyComponentRefusedInput`). -/
theorem emptyComponentReported :
    reportedLocation (parse emptyComponentRefusedInput) = some (.emptyComponent, 2, 2, 2, []) := by decide

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_nonAscii_iff`, case `ConcreteSpec.Core.SourcePath.Registry.mixedCaseInput`). -/
theorem nonAsciiAdmitted : ∃ path, parse mixedCaseInput = .ok path :=
  (admission_nonAscii_iff _).2 ⟨nonAsciiAccepted, by decide⟩

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_nonAscii_iff`, case `ConcreteSpec.Core.SourcePath.Registry.nonAsciiRefusedInput`). -/
theorem nonAsciiRejected : ¬∃ path, parse nonAsciiRefusedInput = .ok path :=
  fun admitted => nonAsciiRefused ((admission_nonAscii_iff _).1 admitted).1

/-- Instance (law `ConcreteSpec.Core.SourcePath.parse_refusal_location`, case `ConcreteSpec.Core.SourcePath.Registry.nonAsciiRefusedInput`). -/
theorem nonAsciiReported :
    reportedLocation (parse nonAsciiRefusedInput) = some (.nonAscii, 0, 2, 1, [65, 128]) := by decide

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_backslash_iff`, case `ConcreteSpec.Core.SourcePath.Registry.mixedCaseInput`). -/
theorem backslashAdmitted : ∃ path, parse mixedCaseInput = .ok path :=
  (admission_backslash_iff _).2 ⟨backslashAccepted, by decide⟩

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_backslash_iff`, case `ConcreteSpec.Core.SourcePath.Registry.backslashRefusedInput`). -/
theorem backslashRejected : ¬∃ path, parse backslashRefusedInput = .ok path :=
  fun admitted => backslashRefused ((admission_backslash_iff _).1 admitted).1

/-- Instance (law `ConcreteSpec.Core.SourcePath.parse_refusal_location`, case `ConcreteSpec.Core.SourcePath.Registry.backslashRefusedInput`). -/
theorem backslashReported :
    reportedLocation (parse backslashRefusedInput) = some (.backslash, 0, 3, 1, [65, 92, 66]) := by decide

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_control_iff`, case `ConcreteSpec.Core.SourcePath.Registry.mixedCaseInput`). -/
theorem controlAdmitted : ∃ path, parse mixedCaseInput = .ok path :=
  (admission_control_iff _).2 ⟨controlAccepted, by decide⟩

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_control_iff`, case `ConcreteSpec.Core.SourcePath.Registry.controlRefusedInput`). -/
theorem controlRejected : ¬∃ path, parse controlRefusedInput = .ok path :=
  fun admitted => controlRefused ((admission_control_iff _).1 admitted).1

/-- Instance (law `ConcreteSpec.Core.SourcePath.parse_refusal_location`, case `ConcreteSpec.Core.SourcePath.Registry.controlRefusedInput`). -/
theorem controlReported :
    reportedLocation (parse controlRefusedInput) = some (.control, 0, 2, 1, [65, 127]) := by decide

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_space_iff`, case `ConcreteSpec.Core.SourcePath.Registry.mixedCaseInput`). -/
theorem spaceAdmitted : ∃ path, parse mixedCaseInput = .ok path :=
  (admission_space_iff _).2 ⟨spaceAccepted, by decide⟩

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_space_iff`, case `ConcreteSpec.Core.SourcePath.Registry.spaceRefusedInput`). -/
theorem spaceRejected : ¬∃ path, parse spaceRefusedInput = .ok path :=
  fun admitted => spaceRefused ((admission_space_iff _).1 admitted).1

/-- Instance (law `ConcreteSpec.Core.SourcePath.parse_refusal_location`, case `ConcreteSpec.Core.SourcePath.Registry.spaceRefusedInput`). -/
theorem spaceReported :
    reportedLocation (parse spaceRefusedInput) = some (.space, 0, 2, 1, [65, 32]) := by decide

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_leadingDot_iff`, case `ConcreteSpec.Core.SourcePath.Registry.mixedCaseInput`). -/
theorem leadingDotAdmitted : ∃ path, parse mixedCaseInput = .ok path :=
  (admission_leadingDot_iff _).2 ⟨leadingDotAccepted, by decide⟩

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_leadingDot_iff`, case `ConcreteSpec.Core.SourcePath.Registry.leadingDotRefusedInput`). -/
theorem leadingDotRejected : ¬∃ path, parse leadingDotRefusedInput = .ok path :=
  fun admitted => leadingDotRefused ((admission_leadingDot_iff _).1 admitted).1

/-- Instance (law `ConcreteSpec.Core.SourcePath.parse_refusal_location`, case `ConcreteSpec.Core.SourcePath.Registry.leadingDotRefusedInput`). -/
theorem leadingDotReported :
    reportedLocation (parse leadingDotRefusedInput) = some (.leadingDot, 0, 2, 0, [46, 65]) := by decide

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_trailingDot_iff`, case `ConcreteSpec.Core.SourcePath.Registry.mixedCaseInput`). -/
theorem trailingDotAdmitted : ∃ path, parse mixedCaseInput = .ok path :=
  (admission_trailingDot_iff _).2 ⟨trailingDotAccepted, by decide⟩

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_trailingDot_iff`, case `ConcreteSpec.Core.SourcePath.Registry.trailingDotRefusedInput`). -/
theorem trailingDotRejected : ¬∃ path, parse trailingDotRefusedInput = .ok path :=
  fun admitted => trailingDotRefused ((admission_trailingDot_iff _).1 admitted).1

/-- Instance (law `ConcreteSpec.Core.SourcePath.parse_refusal_location`, case `ConcreteSpec.Core.SourcePath.Registry.trailingDotRefusedInput`). -/
theorem trailingDotReported :
    reportedLocation (parse trailingDotRefusedInput) = some (.trailingDot, 0, 2, 1, [65, 46]) := by decide

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_reserved_iff`, case `ConcreteSpec.Core.SourcePath.Registry.reservedAcceptedInput`). -/
theorem reservedAdmitted : ∃ path, parse reservedAcceptedInput = .ok path :=
  (admission_reserved_iff _).2 ⟨reservedAccepted, by decide⟩

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_reserved_iff`, case `ConcreteSpec.Core.SourcePath.Registry.reservedRefusedInput`). -/
theorem reservedRejected : ¬∃ path, parse reservedRefusedInput = .ok path :=
  fun admitted => reservedRefused ((admission_reserved_iff _).1 admitted).1

/-- Instance (law `ConcreteSpec.Core.SourcePath.parse_refusal_location`, case `ConcreteSpec.Core.SourcePath.Registry.reservedRefusedInput`). -/
theorem reservedReported :
    reportedLocation (parse reservedRefusedInput) = some (.reserved, 0, 5, 0, [99, 79, 78, 46, 67]) := by decide

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_componentCap_iff`, case `ConcreteSpec.Core.SourcePath.Registry.componentCapAcceptedInput`). -/
theorem componentCapAdmitted : ∃ path, parse componentCapAcceptedInput = .ok path :=
  (admission_componentCap_iff _).2 ⟨componentCapAccepted, of_decide_eq_true (by cbv)⟩

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_componentCap_iff`, case `ConcreteSpec.Core.SourcePath.Registry.componentCapRefusedInput`). -/
theorem componentCapRejected : ¬∃ path, parse componentCapRefusedInput = .ok path :=
  fun admitted => componentCapRefused ((admission_componentCap_iff _).1 admitted).1

/-- Instance (law `ConcreteSpec.Core.SourcePath.parse_refusal_location`, case `ConcreteSpec.Core.SourcePath.Registry.componentCapRefusedInput`). -/
theorem componentCapReported :
    reportedLocation (parse componentCapRefusedInput) = some (.componentCap, 0, 101, 100, List.replicate 101 65) := of_decide_eq_true (by cbv)

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_pathCap_iff`, case `ConcreteSpec.Core.SourcePath.Registry.pathCapRefusedInput`). -/
theorem pathCapRejected : ¬∃ path, parse pathCapRefusedInput = .ok path :=
  fun admitted => pathCapRefused ((admission_pathCap_iff _).1 admitted).1

-- Reduce long fixture scans one component at a time using the production scanner equations.
private theorem scan_rev_congr {length start : Nat}
    (left right rest : ConcreteSpec.Numbers) (same : left = right)
    (a : start + left.length + rest.length = length)
    (b : start + right.length + rest.length = length) :
    scan start left rest a = scan start right rest b := by
  subst right
  rfl

private theorem scan_replicate {length : Nat} (start : Nat) (rev rest : ConcreteSpec.Numbers)
    (byte count : Nat) (ordinary : byte ≠ 47)
    (total : start + rev.length + (List.replicate count byte ++ rest).length = length) :
    scan start rev (List.replicate count byte ++ rest) total =
      scan start (List.replicate count byte ++ rev) rest (by simp at total ⊢; omega) := by
  induction count generalizing rev with
  | zero => rfl
  | succ count ih =>
    change scan start rev (byte :: (List.replicate count byte ++ rest)) _ = _
    simp only [scan, ordinary, ↓reduceIte]
    rw [ih]
    apply scan_rev_congr
    simp [List.replicate_succ', List.append_assoc]

private theorem scan_repeat_slash {length : Nat} (start : Nat) (rest : ConcreteSpec.Numbers)
    (byte count : Nat) (ordinary : byte ≠ 47)
    (total : start + [].length + (List.replicate count byte ++ 47 :: rest).length = length) :
    scan start [] (List.replicate count byte ++ 47 :: rest) total =
      ⟨start, List.replicate count byte, by simp at total ⊢; omega⟩ ::
        scan (start + count + 1) [] rest (by simp at total ⊢; omega) := by
  rw [scan_replicate start [] _ byte count ordinary]
  simp [scan, List.reverse_replicate]

private theorem scan_repeat_end {length : Nat} (start byte count : Nat) (ordinary : byte ≠ 47)
    (total : start + [].length + (List.replicate count byte ++ []).length = length) :
    scan start [] (List.replicate count byte ++ []) total =
      [⟨start, List.replicate count byte, by simp at total ⊢; omega⟩] := by
  rw [scan_replicate start [] [] byte count ordinary]
  simp [scan, List.reverse_replicate]

private theorem pathCapRefusedInput_length : pathCapRefusedInput.length = 201 := by
  simp only [pathCapRefusedInput, SourceText.length, List.length_append, List.length_replicate,
    List.length_cons, List.length_nil]

private theorem scan_rest_congr {length start : Nat} (rev left right : ConcreteSpec.Numbers)
    (same : left = right) (a : start + rev.length + left.length = length)
    (b : start + rev.length + right.length = length) :
    scan start rev left a = scan start rev right b := by
  subst right
  rfl

private theorem pathCapRefusedInput_components :
    components pathCapRefusedInput =
      [⟨0, List.replicate 98 65, by simp [pathCapRefusedInput_length]⟩,
       ⟨99, List.replicate 100 66, by simp [pathCapRefusedInput_length]⟩,
       ⟨200, [67], by simp [pathCapRefusedInput_length]⟩] := by
  unfold components
  calc
    _ = scan 0 [] (List.replicate 98 65 ++ 47 :: (List.replicate 100 66 ++ [47, 67]))
          (by simp only [pathCapRefusedInput_length, List.length_append, List.length_cons,
                List.length_replicate, List.length_nil]) := by
      apply scan_rest_congr
      simp only [pathCapRefusedInput, List.append_assoc, List.cons_append, List.nil_append]
    _ = _ := by
      simp only [scan_repeat_slash 0 _ 65 98 (by decide),
        scan_repeat_slash 99 [67] 66 100 (by decide), scan]
      rfl

private theorem pathCapAcceptedInput_length : pathCapAcceptedInput.length = 200 := by
  simp only [pathCapAcceptedInput, SourceText.length, List.length_append, List.length_replicate,
    List.length_cons, List.length_nil]

private theorem pathCapAcceptedInput_components :
    components pathCapAcceptedInput =
      [⟨0, List.replicate 99 65, by simp [pathCapAcceptedInput_length]⟩,
       ⟨100, List.replicate 100 66, by simp [pathCapAcceptedInput_length]⟩] := by
  unfold components
  calc
    _ = scan 0 [] (List.replicate 99 65 ++ 47 :: List.replicate 100 66)
          (by simp only [pathCapAcceptedInput_length, List.length_append, List.length_cons,
                List.length_replicate, List.length_nil]) := by
      apply scan_rest_congr
      simp only [pathCapAcceptedInput, List.append_assoc, List.cons_append, List.nil_append]
    _ = _ := by
      simp only [scan_repeat_slash 0 _ 65 99 (by decide)]
      have last := scan_repeat_end (length := pathCapAcceptedInput.length) 100 66 100 (by decide)
        (by simp [pathCapAcceptedInput_length])
      simpa only [List.append_nil] using congrArg (List.cons ⟨0, List.replicate 99 65,
        by simp [pathCapAcceptedInput_length]⟩) last

-- Reduce the reference offenses component by component on long inputs.
private theorem pathCapAcceptedInput_otherRules : OtherRules .pathCap pathCapAcceptedInput := by
  have pieces : Reference.pieces pathCapAcceptedInput.bytes =
      [(0, List.replicate 99 65), (100, List.replicate 100 66)] := by
    rw [← components_reference, pathCapAcceptedInput_components]
    rfl
  intro reason _ other
  cases reason <;> try exact False.elim (other rfl)
  all_goals
    unfold Permitted
    simp only [Reference.offsets]
    try rw [pieces]
    cbv

private theorem pathCapRefusedInput_otherRules : OtherRules .pathCap pathCapRefusedInput := by
  have pieces : Reference.pieces pathCapRefusedInput.bytes =
      [(0, List.replicate 98 65), (99, List.replicate 100 66), (200, [67])] := by
    rw [← components_reference, pathCapRefusedInput_components]
    rfl
  intro reason _ other
  cases reason <;> try exact False.elim (other rfl)
  all_goals
    unfold Permitted
    simp only [Reference.offsets]
    try rw [pieces]
    cbv

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_pathCap_iff`, case `ConcreteSpec.Core.SourcePath.Registry.pathCapAcceptedInput`). -/
theorem pathCapAdmitted : ∃ path, parse pathCapAcceptedInput = .ok path :=
  (admission_pathCap_iff _).2 ⟨pathCapAccepted, pathCapAcceptedInput_otherRules⟩

/-- Instance (law `ConcreteSpec.Core.SourcePath.parse_refusal_location`, case `ConcreteSpec.Core.SourcePath.Registry.pathCapRefusedInput`). -/
theorem pathCapReported :
    reportedLocation (parse pathCapRefusedInput) =
      some (.pathCap, 0, 201, 200, List.replicate 98 65 ++ [47] ++ List.replicate 100 66 ++ [47, 67]) := by
  let witness : Violation pathCapRefusedInput.length :=
    ⟨.pathCap, ⟨0, 201, 200, by decide, by decide, by simp [pathCapRefusedInput_length]⟩⟩
  have hit : firstViolation pathCapRefusedInput = some witness := by
    unfold firstViolation selectViolation candidatesWith
    rw [pathCapRefusedInput_components]
    cbv
  rw [parse_of_hit _ _ hit]
  change some (Reason.pathCap, 0, 201, 200, pathCapRefusedInput.bytes.take 201) =
    some (Reason.pathCap, 0, 201, 200, pathCapRefusedInput.bytes)
  rw [← pathCapRefusedInput_length]
  simp [SourceText.length]

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_disallowed_iff`, case `ConcreteSpec.Core.SourcePath.Registry.mixedCaseInput`). -/
theorem disallowedAdmitted : ∃ path, parse mixedCaseInput = .ok path :=
  (admission_disallowed_iff _).2 ⟨disallowedAccepted, by decide⟩

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_disallowed_iff`, case `ConcreteSpec.Core.SourcePath.Registry.disallowedRefusedInput`). -/
theorem disallowedRejected : ¬∃ path, parse disallowedRefusedInput = .ok path :=
  fun admitted => disallowedRefused ((admission_disallowed_iff _).1 admitted).1

/-- Instance (law `ConcreteSpec.Core.SourcePath.parse_refusal_location`, case `ConcreteSpec.Core.SourcePath.Registry.disallowedRefusedInput`). -/
theorem disallowedReported :
    reportedLocation (parse disallowedRefusedInput) = some (.disallowed, 2, 5, 3, [66, 58, 67]) := by decide

/-- Case: a noninitial 101-byte component starts at byte 4. -/
def noninitialCap : SourceText := SourceText.ofBytes ([100, 105, 114, 47] ++ List.replicate 101 65)

/-- Case: the whole-path cap is reached at byte 200 and the third component cap at 222. -/
def competingCaps : SourceText :=
  ⟨List.replicate 60 65 ++ [47] ++ List.replicate 60 66 ++ [47] ++ List.replicate 101 67, by
    intro byte member
    simp only [List.mem_append, List.mem_replicate, List.mem_cons, List.not_mem_nil, or_false] at member
    omega⟩

/-- Instance (law `ConcreteSpec.Core.SourcePath.parse_refusal_location`,
case `ConcreteSpec.Core.SourcePath.LocalControls.noninitialCap`).
Component-cap coordinates are absolute byte offsets. -/
theorem noninitialCapReported :
    reportedLocation (parse noninitialCap) = some (.componentCap, 4, 105, 104, List.replicate 101 65) :=
  of_decide_eq_true (by cbv)

private theorem competingCaps_length : competingCaps.length = 223 := by
  simp only [competingCaps, SourceText.length, List.length_append, List.length_replicate,
    List.length_cons, List.length_nil]

private theorem competingCaps_components :
    components competingCaps =
      [⟨0, List.replicate 60 65, by simp [competingCaps_length]⟩,
       ⟨61, List.replicate 60 66, by simp [competingCaps_length]⟩,
       ⟨122, List.replicate 101 67, by simp [competingCaps_length]⟩] := by
  unfold components
  calc
    _ = scan 0 [] (List.replicate 60 65 ++ 47 ::
          (List.replicate 60 66 ++ 47 :: (List.replicate 101 67 ++ [])))
          (by simp only [competingCaps_length, List.length_append, List.length_cons,
                List.length_replicate, List.length_nil]) := by
      apply scan_rest_congr
      simp only [competingCaps, List.append_assoc, List.cons_append, List.nil_append, List.append_nil]
    _ = _ := by
      simp only [scan_repeat_slash 0 _ 65 60 (by decide),
        scan_repeat_slash 61 _ 66 60 (by decide), scan_repeat_end 122 67 101 (by decide)]

/-- Instance: an earlier byte beats a higher-priority check at a later byte. -/
theorem competingCapsReported : reported (parse competingCaps) = some (.pathCap, 200) := by
  let witness : Violation competingCaps.length :=
    ⟨.pathCap, ⟨0, 223, 200, by decide, by decide, by simp [competingCaps_length]⟩⟩
  have hit : firstViolation competingCaps = some witness := by
    unfold firstViolation selectViolation candidatesWith
    rw [competingCaps_components]
    cbv
  rw [parse_of_hit _ _ hit]
  rfl

/-- Instance: each of the lower control bytes is reported at the first offending position. -/
theorem controlBytesReported :
    reported (parse (SourceText.ofBytes [65, 0, 9])) = some (.control, 1) ∧
    reported (parse (SourceText.ofBytes [65, 9])) = some (.control, 1) := by decide

/-- Instance: a 99-byte component and a 199-byte path both remain admissible. -/
theorem belowCapsAdmitted :
    RawOk (SourceText.ofBytes (List.replicate 99 65)) ∧
    RawOk (SourceText.ofBytes (List.replicate 98 65 ++ [47] ++ List.replicate 100 66)) := of_decide_eq_true (by cbv)

/-- Mutant: fold ASCII names while rendering them. -/
def renderFolded (path : SourcePath) : ConcreteSpec.Name :=
  ⟨String.ofList (path.raw.bytes.map fun b => Char.ofNat (foldAscii b))⟩

/-- Case: a valid mixed-case path. -/
def mixedCasePath : SourcePath := ⟨mixedCaseInput, by decide⟩

/-- Instance (law `ConcreteSpec.Core.SourcePath.render_exact`, case `ConcreteSpec.Core.SourcePath.LocalControls.mixedCasePath`). -/
theorem mixedCaseRendered :
    SourceText.ofBytes mixedCasePath.render.text.toByteArray.data.toList = mixedCasePath.renderBytes :=
  render_exact mixedCasePath

/-- Planted (law `ConcreteSpec.Core.SourcePath.render_exact`,
case `ConcreteSpec.Core.SourcePath.LocalControls.mixedCasePath`,
subject `ConcreteSpec.Core.SourcePath.render`, mutant `ConcreteSpec.Core.SourcePath.LocalControls.renderFolded`). -/
theorem foldedRenderingChangesIdentity :
    SourceText.ofBytes (renderFolded mixedCasePath).text.toByteArray.data.toList ≠ mixedCasePath.renderBytes := by
  decide

/-- Mutant: select a reason by priority before looking at byte offset. -/
def priorityFirst (raw : SourceText) : Option (Violation raw.length) :=
  selectViolation localPolicy raw fun v => Reason.inOrder.idxOf v.reason

/-- Case: the early backslash should precede the later non-ASCII byte. -/
def earlyBackslash : SourceText := SourceText.ofBytes [65, 92, 66, 128]

/-- Instance: the first offending byte wins even when a later reason has higher priority. -/
theorem earlyBackslashReported : reported (parse earlyBackslash) = some (.backslash, 1) := by decide

/-- Observe the exact admitted bytes without projecting the representation's proof. -/
def admittedBytes (result : Except Refusal SourcePath) : Option SourceText :=
  match result with
  | .ok path => some path.renderBytes
  | .error _ => none

/-- Law (subject `ConcreteSpec.Core.SourcePath.parse`).
The parser's admission observation returns the original bytes exactly for valid input. -/
theorem admittedBytes_iff (raw : SourceText) : admittedBytes (parse raw) = some raw ↔ Reference.Valid raw := by
  cases parsed : parse raw with
  | ok path =>
    have exactBytes := (parse_iff raw path).1 parsed
    simp [admittedBytes, exactBytes.1, exactBytes.2]
  | error refusal =>
    simp [admittedBytes, (parse_refusal_iff raw).1 ⟨refusal, parsed⟩]

/-- A mutation uses the parser's decision before the successful branch packages its proof.
The production specialization is equivalent to observing an actual successful parse. -/
def admitsWith (policy : ProbePolicy) (raw : SourceText) : Prop :=
  selectViolation policy raw Violation.key = none

/-- Representation lemma for substituting a probe policy in an admission counterexample. -/
theorem admitsWith_local_iff (raw : SourceText) :
    admitsWith localPolicy raw ↔ ∃ path, parse raw = .ok path :=
  (noViolation_iff raw).trans ((rawOk_reference_iff raw).trans (admitted_iff raw).symm)

private theorem rejectsValid {policy : ProbePolicy} {raw : SourceText} {reason : Reason}
    (valid : Reference.Valid raw) (rejected : ¬ admitsWith policy raw) :
    ¬ (admitsWith policy raw ↔ Permitted reason raw ∧ OtherRules reason raw) :=
  fun law => rejected (law.mpr ⟨valid reason (reference_reason reason), fun rule member _ => valid rule member⟩)

private theorem admitsInvalid {policy : ProbePolicy} {raw : SourceText} {reason : Reason}
    (_other : OtherRules reason raw) (invalid : ¬ Permitted reason raw)
    (admitted : admitsWith policy raw) :
    ¬ (admitsWith policy raw ↔ Permitted reason raw ∧ OtherRules reason raw) :=
  fun law => invalid (law.mp admitted).1

/-- Case: valid input at a tightened probe boundary. -/
def oneByteInput : SourceText := SourceText.ofBytes [65]

/-- Mutant: refuse a one-byte path as empty. -/
def tightEmpty : ProbePolicy :=
  { localPolicy with empty := fun bytes => bytes.length ≤ 1 }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_empty_iff`,
case `ConcreteSpec.Core.SourcePath.LocalControls.oneByteInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.tightEmpty`). -/
theorem emptyRejectsAllowed :
    ¬ (admitsWith tightEmpty oneByteInput ↔
      Nonempty oneByteInput ∧ OtherRules .empty oneByteInput) := by
  exact rejectsValid (of_decide_eq_true (by cbv)) (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Mutant: delete the empty-path probe. -/
def withoutEmpty : ProbePolicy :=
  { localPolicy with empty := fun _ => false }

/-- Mutant: treat an initial A as a root marker. -/
def tightAbsolute : ProbePolicy :=
  { localPolicy with absolute := fun bytes => localPolicy.absolute bytes || bytes.head? == some 65 }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_absolute_iff`,
case `ConcreteSpec.Core.SourcePath.LocalControls.oneByteInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.tightAbsolute`). -/
theorem absoluteRejectsAllowed :
    ¬ (admitsWith tightAbsolute oneByteInput ↔
      Relative oneByteInput ∧ OtherRules .absolute oneByteInput) := by
  exact rejectsValid (of_decide_eq_true (by cbv)) (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Mutant: delete the absolute-path probe. -/
def withoutAbsolute : ProbePolicy :=
  { localPolicy with absolute := fun _ => false }

/-- Case: valid input at a tightened probe boundary. -/
def digitAfterInitialLetter : SourceText := SourceText.ofBytes [67, 49]

/-- Mutant: treat a 1 after an initial ASCII letter as a drive separator. -/
def tightDrive : ProbePolicy :=
  { localPolicy with drive := fun letter next => asciiLetter letter && (next == 58 || next == 49) }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_drive_iff`,
case `ConcreteSpec.Core.SourcePath.LocalControls.digitAfterInitialLetter`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.tightDrive`). -/
theorem driveRejectsAllowed :
    ¬ (admitsWith tightDrive digitAfterInitialLetter ↔
      NoDrivePrefix digitAfterInitialLetter ∧ OtherRules .drive digitAfterInitialLetter) := by
  exact rejectsValid (of_decide_eq_true (by cbv)) (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Mutant: delete the drive probe. -/
def withoutDrive : ProbePolicy :=
  { localPolicy with drive := fun _ _ => false }

/-- Case: valid input at a tightened probe boundary. -/
def ordinaryTwoByteComponent : SourceText := SourceText.ofBytes [65, 47, 65, 65]

/-- Mutant: treat all two-byte components as traversal. -/
def tightTraversal : ProbePolicy :=
  { localPolicy with traversal := fun bytes => bytes.length == 2 }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_traversal_iff`,
case `ConcreteSpec.Core.SourcePath.LocalControls.ordinaryTwoByteComponent`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.tightTraversal`). -/
theorem traversalRejectsAllowed :
    ¬ (admitsWith tightTraversal ordinaryTwoByteComponent ↔
      NoTraversal ordinaryTwoByteComponent ∧ OtherRules .traversal ordinaryTwoByteComponent) := by
  exact rejectsValid (of_decide_eq_true (by cbv)) (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Mutant: look for three dots instead of two. -/
def looseTraversal : ProbePolicy :=
  { localPolicy with traversal := (· == [46, 46, 46]) }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_traversal_iff`,
case `ConcreteSpec.Core.SourcePath.Registry.traversalRefusedInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.looseTraversal`). -/
theorem traversalAllowsRefused :
    ¬ (admitsWith looseTraversal traversalRefusedInput ↔
      NoTraversal traversalRefusedInput ∧ OtherRules .traversal traversalRefusedInput) := by
  exact admitsInvalid (of_decide_eq_true (by cbv)) traversalRefused (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Case: valid input at a tightened probe boundary. -/
def ordinaryOneByteComponent : SourceText := SourceText.ofBytes [65, 47, 66]

/-- Mutant: treat every one-byte component as a single-dot component. -/
def tightSingleDot : ProbePolicy :=
  { localPolicy with singleDot := fun bytes => bytes.length == 1 }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_singleDot_iff`,
case `ConcreteSpec.Core.SourcePath.LocalControls.ordinaryOneByteComponent`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.tightSingleDot`). -/
theorem singleDotRejectsAllowed :
    ¬ (admitsWith tightSingleDot ordinaryOneByteComponent ↔
      NoSingleDot ordinaryOneByteComponent ∧ OtherRules .singleDot ordinaryOneByteComponent) := by
  exact rejectsValid (of_decide_eq_true (by cbv)) (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Mutant: look for two dots instead of one. -/
def looseSingleDot : ProbePolicy :=
  { localPolicy with singleDot := (· == [46, 46]) }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_singleDot_iff`,
case `ConcreteSpec.Core.SourcePath.Registry.singleDotRefusedInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.looseSingleDot`). -/
theorem singleDotAllowsRefused :
    ¬ (admitsWith looseSingleDot singleDotRefusedInput ↔
      NoSingleDot singleDotRefusedInput ∧ OtherRules .singleDot singleDotRefusedInput) := by
  exact admitsInvalid (of_decide_eq_true (by cbv)) singleDotRefused (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Mutant: treat one-byte components as empty. -/
def tightEmptyComponent : ProbePolicy :=
  { localPolicy with emptyComponent := fun bytes => bytes.length ≤ 1 }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_emptyComponent_iff`,
case `ConcreteSpec.Core.SourcePath.LocalControls.ordinaryOneByteComponent`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.tightEmptyComponent`). -/
theorem emptyComponentRejectsAllowed :
    ¬ (admitsWith tightEmptyComponent ordinaryOneByteComponent ↔
      NonemptyComponents ordinaryOneByteComponent ∧ OtherRules .emptyComponent ordinaryOneByteComponent) := by
  exact rejectsValid (of_decide_eq_true (by cbv)) (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Mutant: allow empty components. -/
def looseEmptyComponent : ProbePolicy :=
  { localPolicy with emptyComponent := fun _ => false }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_emptyComponent_iff`,
case `ConcreteSpec.Core.SourcePath.Registry.emptyComponentRefusedInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.looseEmptyComponent`). -/
theorem emptyComponentAllowsRefused :
    ¬ (admitsWith looseEmptyComponent emptyComponentRefusedInput ↔
      NonemptyComponents emptyComponentRefusedInput ∧ OtherRules .emptyComponent emptyComponentRefusedInput) := by
  exact admitsInvalid (of_decide_eq_true (by cbv)) emptyComponentRefused (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Case: valid input at a tightened probe boundary. -/
def highestFilenameLetter : SourceText := SourceText.ofBytes [65, 122]

/-- Mutant: move the non-ASCII cutoff to 122. -/
def tightNonAscii : ProbePolicy :=
  { localPolicy with nonAscii := (122 ≤ ·) }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_nonAscii_iff`,
case `ConcreteSpec.Core.SourcePath.LocalControls.highestFilenameLetter`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.tightNonAscii`). -/
theorem nonAsciiRejectsAllowed :
    ¬ (admitsWith tightNonAscii highestFilenameLetter ↔
      Ascii highestFilenameLetter ∧ OtherRules .nonAscii highestFilenameLetter) := by
  exact rejectsValid (of_decide_eq_true (by cbv)) (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Mutant: allow the first non-ASCII byte. -/
def looseNonAscii : ProbePolicy :=
  { localPolicy with nonAscii := (129 ≤ ·) }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_nonAscii_iff`,
case `ConcreteSpec.Core.SourcePath.Registry.nonAsciiRefusedInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.looseNonAscii`). -/
theorem nonAsciiAllowsRefused :
    ¬ (admitsWith looseNonAscii nonAsciiRefusedInput ↔
      Ascii nonAsciiRefusedInput ∧ OtherRules .nonAscii nonAsciiRefusedInput) := by
  exact admitsInvalid (of_decide_eq_true (by cbv)) nonAsciiRefused (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Case: valid input at a tightened probe boundary. -/
def underscoreInput : SourceText := SourceText.ofBytes [65, 95, 66]

/-- Mutant: treat underscore as another backslash. -/
def tightBackslash : ProbePolicy :=
  { localPolicy with backslash := fun byte => byte == 92 || byte == 95 }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_backslash_iff`,
case `ConcreteSpec.Core.SourcePath.LocalControls.underscoreInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.tightBackslash`). -/
theorem backslashRejectsAllowed :
    ¬ (admitsWith tightBackslash underscoreInput ↔
      SlashSeparator underscoreInput ∧ OtherRules .backslash underscoreInput) := by
  exact rejectsValid (of_decide_eq_true (by cbv)) (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Mutant: check for a closing bracket in place of backslash. -/
def looseBackslash : ProbePolicy :=
  { localPolicy with backslash := (· == 93) }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_backslash_iff`,
case `ConcreteSpec.Core.SourcePath.Registry.backslashRefusedInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.looseBackslash`). -/
theorem backslashAllowsRefused :
    ¬ (admitsWith looseBackslash backslashRefusedInput ↔
      SlashSeparator backslashRefusedInput ∧ OtherRules .backslash backslashRefusedInput) := by
  exact admitsInvalid (of_decide_eq_true (by cbv)) backslashRefused (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Case: valid input at a tightened probe boundary. -/
def hyphenInput : SourceText := SourceText.ofBytes [65, 45]

/-- Mutant: classify hyphen as a control byte. -/
def tightControl : ProbePolicy :=
  { localPolicy with control := fun byte => localPolicy.control byte || byte == 45 }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_control_iff`,
case `ConcreteSpec.Core.SourcePath.LocalControls.hyphenInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.tightControl`). -/
theorem controlRejectsAllowed :
    ¬ (admitsWith tightControl hyphenInput ↔
      NoControls hyphenInput ∧ OtherRules .control hyphenInput) := by
  exact rejectsValid (of_decide_eq_true (by cbv)) (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Mutant: omit DEL from the control bytes. -/
def looseControl : ProbePolicy :=
  { localPolicy with control := (· < 32) }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_control_iff`,
case `ConcreteSpec.Core.SourcePath.Registry.controlRefusedInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.looseControl`). -/
theorem controlAllowsRefused :
    ¬ (admitsWith looseControl controlRefusedInput ↔
      NoControls controlRefusedInput ∧ OtherRules .control controlRefusedInput) := by
  exact admitsInvalid (of_decide_eq_true (by cbv)) controlRefused (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Mutant: classify hyphen as a space. -/
def tightSpace : ProbePolicy :=
  { localPolicy with space := fun byte => byte == 32 || byte == 45 }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_space_iff`,
case `ConcreteSpec.Core.SourcePath.LocalControls.hyphenInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.tightSpace`). -/
theorem spaceRejectsAllowed :
    ¬ (admitsWith tightSpace hyphenInput ↔
      NoSpaces hyphenInput ∧ OtherRules .space hyphenInput) := by
  exact rejectsValid (of_decide_eq_true (by cbv)) (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Mutant: look for byte 160 instead of ASCII space. -/
def looseSpace : ProbePolicy :=
  { localPolicy with space := (· == 160) }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_space_iff`,
case `ConcreteSpec.Core.SourcePath.Registry.spaceRefusedInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.looseSpace`). -/
theorem spaceAllowsRefused :
    ¬ (admitsWith looseSpace spaceRefusedInput ↔
      NoSpaces spaceRefusedInput ∧ OtherRules .space spaceRefusedInput) := by
  exact admitsInvalid (of_decide_eq_true (by cbv)) spaceRefused (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Case: valid input at a tightened probe boundary. -/
def leadingUnderscore : SourceText := SourceText.ofBytes [95, 65]

/-- Mutant: reject an initial underscore as a dot. -/
def tightLeadingDot : ProbePolicy :=
  { localPolicy with leadingDot := fun byte => byte == 46 || byte == 95 }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_leadingDot_iff`,
case `ConcreteSpec.Core.SourcePath.LocalControls.leadingUnderscore`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.tightLeadingDot`). -/
theorem leadingDotRejectsAllowed :
    ¬ (admitsWith tightLeadingDot leadingUnderscore ↔
      NoLeadingDot leadingUnderscore ∧ OtherRules .leadingDot leadingUnderscore) := by
  exact rejectsValid (of_decide_eq_true (by cbv)) (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Mutant: look for an initial underscore in place of a dot. -/
def looseLeadingDot : ProbePolicy :=
  { localPolicy with leadingDot := (· == 95) }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_leadingDot_iff`,
case `ConcreteSpec.Core.SourcePath.Registry.leadingDotRefusedInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.looseLeadingDot`). -/
theorem leadingDotAllowsRefused :
    ¬ (admitsWith looseLeadingDot leadingDotRefusedInput ↔
      NoLeadingDot leadingDotRefusedInput ∧ OtherRules .leadingDot leadingDotRefusedInput) := by
  exact admitsInvalid (of_decide_eq_true (by cbv)) leadingDotRefused (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Case: valid input at a tightened probe boundary. -/
def trailingUnderscore : SourceText := SourceText.ofBytes [65, 95]

/-- Mutant: reject a final underscore as a dot. -/
def tightTrailingDot : ProbePolicy :=
  { localPolicy with trailingDot := fun byte => byte == 46 || byte == 95 }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_trailingDot_iff`,
case `ConcreteSpec.Core.SourcePath.LocalControls.trailingUnderscore`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.tightTrailingDot`). -/
theorem trailingDotRejectsAllowed :
    ¬ (admitsWith tightTrailingDot trailingUnderscore ↔
      NoTrailingDot trailingUnderscore ∧ OtherRules .trailingDot trailingUnderscore) := by
  exact rejectsValid (of_decide_eq_true (by cbv)) (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Mutant: look for a final underscore in place of a dot. -/
def looseTrailingDot : ProbePolicy :=
  { localPolicy with trailingDot := (· == 95) }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_trailingDot_iff`,
case `ConcreteSpec.Core.SourcePath.Registry.trailingDotRefusedInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.looseTrailingDot`). -/
theorem trailingDotAllowsRefused :
    ¬ (admitsWith looseTrailingDot trailingDotRefusedInput ↔
      NoTrailingDot trailingDotRefusedInput ∧ OtherRules .trailingDot trailingDotRefusedInput) := by
  exact admitsInvalid (of_decide_eq_true (by cbv)) trailingDotRefused (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Mutant: add COM0 and LPT0 to the reserved stems. -/
def tightReserved : ProbePolicy :=
  { localPolicy with reserved := reservedStemWith foldAscii 48 }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_reserved_iff`,
case `ConcreteSpec.Core.SourcePath.Registry.reservedAcceptedInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.tightReserved`). -/
theorem reservedRejectsAllowed :
    ¬ (admitsWith tightReserved reservedAcceptedInput ↔
      NoReservedStem reservedAcceptedInput ∧ OtherRules .reserved reservedAcceptedInput) := by
  exact rejectsValid (of_decide_eq_true (by cbv)) (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Mutant: match reserved stems without ASCII case folding. -/
def looseReserved : ProbePolicy :=
  { localPolicy with reserved := reservedStemWith id 49 }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_reserved_iff`,
case `ConcreteSpec.Core.SourcePath.Registry.reservedRefusedInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.looseReserved`). -/
theorem reservedAllowsRefused :
    ¬ (admitsWith looseReserved reservedRefusedInput ↔
      NoReservedStem reservedRefusedInput ∧ OtherRules .reserved reservedRefusedInput) := by
  exact admitsInvalid (of_decide_eq_true (by cbv)) reservedRefused (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Mutant: cap components at 99 bytes. -/
def tightComponentCap : ProbePolicy :=
  { localPolicy with componentCap := 99 }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_componentCap_iff`,
case `ConcreteSpec.Core.SourcePath.Registry.componentCapAcceptedInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.tightComponentCap`). -/
theorem componentCapRejectsAllowed :
    ¬ (admitsWith tightComponentCap componentCapAcceptedInput ↔
      ComponentByteCap componentCapAcceptedInput ∧ OtherRules .componentCap componentCapAcceptedInput) := by
  exact rejectsValid (of_decide_eq_true (by cbv)) (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Mutant: cap components at 101 bytes. -/
def looseComponentCap : ProbePolicy :=
  { localPolicy with componentCap := 101 }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_componentCap_iff`,
case `ConcreteSpec.Core.SourcePath.Registry.componentCapRefusedInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.looseComponentCap`). -/
theorem componentCapAllowsRefused :
    ¬ (admitsWith looseComponentCap componentCapRefusedInput ↔
      ComponentByteCap componentCapRefusedInput ∧ OtherRules .componentCap componentCapRefusedInput) := by
  exact admitsInvalid (of_decide_eq_true (by cbv)) componentCapRefused (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Mutant: cap paths at 199 bytes. -/
def tightPathCap : ProbePolicy :=
  { localPolicy with pathCap := 199 }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_pathCap_iff`,
case `ConcreteSpec.Core.SourcePath.Registry.pathCapAcceptedInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.tightPathCap`). -/
theorem pathCapRejectsAllowed :
    ¬ (admitsWith tightPathCap pathCapAcceptedInput ↔
      PathByteCap pathCapAcceptedInput ∧ OtherRules .pathCap pathCapAcceptedInput) := by
  apply rejectsValid (reason := .pathCap)
  · intro reason member
    by_cases same : reason = .pathCap
    · exact same ▸ pathCapAccepted
    · exact pathCapAcceptedInput_otherRules reason member same
  · intro admitted
    let witness : Violation pathCapAcceptedInput.length :=
      ⟨.pathCap, ⟨0, 200, 199, by decide, by decide, by simp [pathCapAcceptedInput_length]⟩⟩
    have hit : selectViolation tightPathCap pathCapAcceptedInput Violation.key = some witness := by
      unfold selectViolation candidatesWith
      rw [pathCapAcceptedInput_components]
      cbv
    change selectViolation tightPathCap pathCapAcceptedInput Violation.key = none at admitted
    rw [admitted] at hit
    contradiction

/-- Mutant: cap paths at 201 bytes. -/
def loosePathCap : ProbePolicy :=
  { localPolicy with pathCap := 201 }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_pathCap_iff`,
case `ConcreteSpec.Core.SourcePath.Registry.pathCapRefusedInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.loosePathCap`). -/
theorem pathCapAllowsRefused :
    ¬ (admitsWith loosePathCap pathCapRefusedInput ↔
      PathByteCap pathCapRefusedInput ∧ OtherRules .pathCap pathCapRefusedInput) := by
  apply admitsInvalid pathCapRefusedInput_otherRules pathCapRefused
  unfold admitsWith selectViolation candidatesWith
  rw [pathCapRefusedInput_components]
  cbv

/-- Mutant: remove hyphen from the filename whitelist. -/
def tightDisallowed : ProbePolicy :=
  { localPolicy with disallowed := fun byte => localPolicy.disallowed byte || byte == 45 }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_disallowed_iff`,
case `ConcreteSpec.Core.SourcePath.LocalControls.hyphenInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.tightDisallowed`). -/
theorem disallowedRejectsAllowed :
    ¬ (admitsWith tightDisallowed hyphenInput ↔
      FilenameCharacters hyphenInput ∧ OtherRules .disallowed hyphenInput) := by
  exact rejectsValid (of_decide_eq_true (by cbv)) (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Mutant: allow colons through the filename whitelist. -/
def looseDisallowed : ProbePolicy :=
  { localPolicy with disallowed := fun byte => localPolicy.disallowed byte && byte != 58 }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_disallowed_iff`,
case `ConcreteSpec.Core.SourcePath.Registry.disallowedRefusedInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.looseDisallowed`). -/
theorem disallowedAllowsRefused :
    ¬ (admitsWith looseDisallowed disallowedRefusedInput ↔
      FilenameCharacters disallowedRefusedInput ∧ OtherRules .disallowed disallowedRefusedInput) := by
  exact admitsInvalid (of_decide_eq_true (by cbv)) disallowedRefused (by unfold admitsWith; exact of_decide_eq_true (by cbv))


/-- The refusal-order obligation with a replacement diagnostic search. -/
def KeepsRefusalOrder (raw : SourceText) (selection : Option (Violation raw.length)) : Prop :=
  ∀ chosen, selection = some chosen →
    Reference.IsFirst raw chosen.reason chosen.location.offset

/-- Representation lemma for substituting the search used by the parser's refusal branch. -/
theorem keepsProductionRefusalOrder (raw : SourceText) :
    KeepsRefusalOrder raw (firstViolation raw) :=
  fun chosen found => parse_refusal_order raw ⟨raw, chosen⟩ (parse_of_hit raw chosen found)

/-- Mutant: preserve byte order but prefer later rules when offsets tie. -/
def reverseTieKey {length : Nat} (violation : Violation length) : Nat :=
  violation.location.offset * Reason.inOrder.length +
    (Reason.inOrder.length - 1 - Reason.inOrder.idxOf violation.reason)

private theorem losesRefusalOrder {raw : SourceText} {selection : Option (Violation raw.length)}
    (chosen : Violation raw.length) (found : selection = some chosen)
    (reason : Reason) (offset : Nat) (offense : offset ∈ Reference.offsets reason raw)
    (late : ¬ Reference.Precedes chosen.reason chosen.location.offset reason offset) :
    ¬ KeepsRefusalOrder raw selection :=
  fun order => late ((order chosen found).2 reason offset offense)

/-- Planted (law `ConcreteSpec.Core.SourcePath.firstViolation_order`,
case `ConcreteSpec.Core.SourcePath.LocalControls.earlyBackslash`,
subject `ConcreteSpec.Core.SourcePath.firstViolation`, mutant `ConcreteSpec.Core.SourcePath.LocalControls.priorityFirst`). -/
theorem priorityFirstMissesEarlyByte :
    ¬ KeepsRefusalOrder earlyBackslash (priorityFirst earlyBackslash) := by
  exact losesRefusalOrder ⟨.nonAscii, ⟨0, 4, 3, by decide, by decide, by decide⟩⟩
    rfl .backslash 1 (by decide) (by unfold Reference.Precedes; decide)

/-- Mutant: give empty components precedence over empty paths on a tie. -/
def swappedEmptyPriority : List Reason :=
  Reason.inOrder.map fun
    | .empty => .emptyComponent
    | .emptyComponent => .empty
    | reason => reason

def swappedEmptyKey {length : Nat} (violation : Violation length) : Nat :=
  violation.location.offset * swappedEmptyPriority.length +
    swappedEmptyPriority.idxOf violation.reason

/-- Planted (law `ConcreteSpec.Core.SourcePath.parse_refusal_order`,
case `ConcreteSpec.Core.SourcePath.Registry.emptyRefusedInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.swappedEmptyKey`). -/
theorem swappedEmptyLosesPriority :
    ¬ KeepsRefusalOrder emptyRefusedInput
      (selectViolation localPolicy emptyRefusedInput swappedEmptyKey) := by
  exact losesRefusalOrder ⟨.emptyComponent, ⟨0, 0, 0, by decide, by decide, by decide⟩⟩
    rfl .empty 0 (by decide) (by unfold Reference.Precedes; decide)

/-- Case: two offending bytes in one component, at whole-path offsets 3 and 5. -/
def repeatedBackslash : SourceText := SourceText.ofBytes [65, 47, 66, 92, 67, 92]

/-- Skip the first matching byte and look for another match in the same component. -/
def skipFirstMatch (bytes : ConcreteSpec.Numbers) (test : Nat → Bool) :
    Option (Fin bytes.length) := do
  let first ← bytes.findFinIdx? test
  let start := first.val + 1
  let next ← (bytes.drop start).findFinIdx? test
  pure ⟨start + next.val, by
    have shorter : next.val < bytes.length - start :=
      Nat.lt_of_lt_of_eq next.isLt (List.length_drop (i := start))
    omega⟩

/-- Mutant: select the second match of the backslash probe. -/
def skipEarlierBackslash : ProbePolicy :=
  { localPolicy with findByte := fun reason bytes test =>
      match reason with
      | .backslash => skipFirstMatch bytes test
      | _ => bytes.findFinIdx? test }

/-- Planted (law `ConcreteSpec.Core.SourcePath.parse_refusal_order`,
case `ConcreteSpec.Core.SourcePath.LocalControls.repeatedBackslash`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.skipEarlierBackslash`). -/
theorem skippedBackslashLosesEarliest :
    ¬ KeepsRefusalOrder repeatedBackslash
      (selectViolation skipEarlierBackslash repeatedBackslash Violation.key) := by
  exact losesRefusalOrder ⟨.backslash, ⟨2, 6, 5, by decide, by decide, by decide⟩⟩
    rfl .backslash 3 (by decide) (by unfold Reference.Precedes; decide)

/-- Case: a drive prefix with a following component. -/
def driveWithSuffix : SourceText := SourceText.ofBytes [67, 58, 47, 65]

/-- Instance (law `ConcreteSpec.Core.SourcePath.parse_refusal_location`,
case `ConcreteSpec.Core.SourcePath.LocalControls.driveWithSuffix`). -/
theorem driveWithSuffixReported :
    reportedLocation (parse driveWithSuffix) = some (.drive, 0, 2, 1, [67, 58]) := by decide

/-- Planted (law `ConcreteSpec.Core.SourcePath.parse_refusal_order`,
case `ConcreteSpec.Core.SourcePath.Registry.emptyRefusedInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.withoutEmpty`). -/
theorem deleteEmptyLosesPriority :
    ¬ KeepsRefusalOrder emptyRefusedInput (selectViolation withoutEmpty emptyRefusedInput Violation.key) := by
  exact losesRefusalOrder
    ⟨.emptyComponent, ⟨0, 0, 0, by decide, by decide, by decide⟩⟩ rfl .empty 0 (by decide) (by unfold Reference.Precedes; decide)

/-- Planted (law `ConcreteSpec.Core.SourcePath.parse_refusal_order`,
case `ConcreteSpec.Core.SourcePath.Registry.emptyRefusedInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.reverseTieKey`). -/
theorem reverseEmptyLosesPriority :
    ¬ KeepsRefusalOrder emptyRefusedInput (selectViolation localPolicy emptyRefusedInput reverseTieKey) := by
  exact losesRefusalOrder
    ⟨.emptyComponent, ⟨0, 0, 0, by decide, by decide, by decide⟩⟩ rfl .empty 0 (by decide) (by unfold Reference.Precedes; decide)

/-- Planted (law `ConcreteSpec.Core.SourcePath.parse_refusal_order`,
case `ConcreteSpec.Core.SourcePath.Registry.absoluteRefusedInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.withoutAbsolute`). -/
theorem deleteAbsoluteLosesPriority :
    ¬ KeepsRefusalOrder absoluteRefusedInput (selectViolation withoutAbsolute absoluteRefusedInput Violation.key) := by
  exact losesRefusalOrder
    ⟨.emptyComponent, ⟨0, 0, 0, by decide, by decide, by decide⟩⟩ rfl .absolute 0 (by decide) (by unfold Reference.Precedes; decide)

/-- Planted (law `ConcreteSpec.Core.SourcePath.parse_refusal_order`,
case `ConcreteSpec.Core.SourcePath.Registry.absoluteRefusedInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.reverseTieKey`). -/
theorem reverseAbsoluteLosesPriority :
    ¬ KeepsRefusalOrder absoluteRefusedInput (selectViolation localPolicy absoluteRefusedInput reverseTieKey) := by
  exact losesRefusalOrder
    ⟨.emptyComponent, ⟨0, 0, 0, by decide, by decide, by decide⟩⟩ rfl .absolute 0 (by decide) (by unfold Reference.Precedes; decide)

/-- Planted (law `ConcreteSpec.Core.SourcePath.parse_refusal_order`,
case `ConcreteSpec.Core.SourcePath.LocalControls.driveWithSuffix`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.withoutDrive`). -/
theorem deleteDriveLosesPriority :
    ¬ KeepsRefusalOrder driveWithSuffix (selectViolation withoutDrive driveWithSuffix Violation.key) := by
  exact losesRefusalOrder
    ⟨.disallowed, ⟨0, 2, 1, by decide, by decide, by decide⟩⟩ rfl .drive 1 (by decide) (by unfold Reference.Precedes; decide)

/-- Planted (law `ConcreteSpec.Core.SourcePath.parse_refusal_order`,
case `ConcreteSpec.Core.SourcePath.LocalControls.driveWithSuffix`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.reverseTieKey`). -/
theorem reverseDriveLosesPriority :
    ¬ KeepsRefusalOrder driveWithSuffix (selectViolation localPolicy driveWithSuffix reverseTieKey) := by
  exact losesRefusalOrder
    ⟨.disallowed, ⟨0, 2, 1, by decide, by decide, by decide⟩⟩ rfl .drive 1 (by decide) (by unfold Reference.Precedes; decide)


/-- Mutant: admit ASCII filenames only if they have no uppercase letters. -/
def lowercaseOnly : ProbePolicy :=
  { localPolicy with disallowed := fun byte => localPolicy.disallowed byte || (65 ≤ byte && byte ≤ 90) }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_disallowed_iff`,
case `ConcreteSpec.Core.SourcePath.Registry.mixedCaseInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.lowercaseOnly`). -/
theorem lowercaseOnlyRejectsMixedCase :
    ¬ (admitsWith lowercaseOnly mixedCaseInput ↔
      FilenameCharacters mixedCaseInput ∧ OtherRules .disallowed mixedCaseInput) := by
  exact rejectsValid (of_decide_eq_true (by cbv)) (by unfold admitsWith; decide)

/-- Case: the exact UTF-8 bytes for Aé, including both bytes of é. -/
def utf8Input : SourceText := SourceText.ofBytes [65, 195, 169]

/-- Mutant: replace non-ASCII bytes with underscores before admission. -/
def replaceNonAscii (raw : SourceText) : Prop :=
  let replaced := SourceText.ofBytes (raw.toBytes.map fun byte => if byte.toNat < 128 then byte else 95)
  admitsWith localPolicy replaced

/-- Instance (law `ConcreteSpec.Core.SourcePath.parse_refusal_location`,
case `ConcreteSpec.Core.SourcePath.LocalControls.utf8Input`). -/
theorem utf8RefusalPreservesBytes :
    reportedLocation (parse utf8Input) = some (.nonAscii, 0, 3, 1, [65, 195, 169]) := by decide

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_nonAscii_iff`,
case `ConcreteSpec.Core.SourcePath.LocalControls.utf8Input`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.replaceNonAscii`). -/
theorem replacementAdmitsNonAscii :
    ¬ (replaceNonAscii utf8Input ↔ Ascii utf8Input ∧ OtherRules .nonAscii utf8Input) := by
  have admitted : replaceNonAscii utf8Input := by rfl
  have invalid : ¬ Permitted .nonAscii utf8Input := by decide
  exact fun law => invalid (law.mp admitted).1

end LocalControls
end SourcePath

end ConcreteSpec.Core

namespace ConcreteSpec.Core.SourcePath.Project

private theorem caseCollision_nil_left (right : List ConcreteSpec.Numbers) :
    ¬ Reference.CaseCollision [] right := by
  rintro ⟨n, bound, _, unequal, _⟩
  have : n = 0 := by simpa using bound
  subst n
  exact unequal rfl

private theorem caseCollision_symm {left right : List ConcreteSpec.Numbers}
    (h : Reference.CaseCollision left right) : Reference.CaseCollision right left := by
  obtain ⟨n, hl, hr, distinct, folded⟩ := h
  exact ⟨n, hr, hl, Ne.symm distinct, folded.symm⟩

private theorem caseCollision_cons (left right : ConcreteSpec.Numbers)
    (ls rs : List ConcreteSpec.Numbers) :
    Reference.CaseCollision (left :: ls) (right :: rs) ↔
      (left ≠ right ∧ left.map SourcePath.Reference.lower = right.map SourcePath.Reference.lower) ∨
        (left = right ∧ Reference.CaseCollision ls rs) := by
  constructor
  · rintro ⟨n, hl, hr, different, folded⟩
    cases n with
    | zero => exact (different rfl).elim
    | succ n =>
      simp only [List.take_succ_cons, List.map_cons, List.cons.injEq] at different folded
      by_cases h : left = right
      · exact Or.inr ⟨h, n, by simpa using hl, by simpa using hr,
          fun eq => different (by simp [h, eq]), folded.2⟩
      · exact Or.inl ⟨h, folded.1⟩
  · rintro (⟨different, folded⟩ | ⟨rfl, n, hl, hr, different, folded⟩)
    · exact ⟨1, by simp, by simp, by simpa, by simpa⟩
    · exact ⟨n + 1, by simpa using hl, by simpa using hr, by simpa, by simpa only [List.take_succ_cons, List.map_cons, List.cons.injEq,
        true_and] using folded⟩

private theorem classifyComponents_correct (left right : List ConcreteSpec.Numbers)
    (reason : Reason) :
    classifyComponents left right = some reason ↔
      match reason with
      | .caseCollision => Reference.CaseCollision left right
      | .fileDirectoryConflict => left ≠ right ∧ (left <+: right ∨ right <+: left)
      | .duplicate => left = right := by
  induction left generalizing right with
  | nil =>
    cases right <;> cases reason <;>
      simp [classifyComponents, caseCollision_nil_left]
  | cons left ls ih =>
    cases right with
    | nil =>
      have noCase : ¬ Reference.CaseCollision (left :: ls) [] :=
        fun h => caseCollision_nil_left _ (caseCollision_symm h)
      cases reason <;> simp [classifyComponents, noCase]
    | cons right rs =>
      by_cases same : left = right
      · subst right
        cases reason <;> simpa [classifyComponents, caseCollision_cons] using ih rs
      · have foldEqual : foldAscii = SourcePath.Reference.lower := rfl
        have opposite := Ne.symm same
        by_cases folded : left.map foldAscii = right.map foldAscii <;>
          cases reason <;> simp_all [classifyComponents, caseCollision_cons, ← foldEqual]

private theorem component_bytes (path : SourcePath) :
    (components path.raw).map Component.bytes = Reference.parts path := by
  change (scan (length := path.raw.bytes.length) 0 [] path.raw.bytes _).map Component.bytes = _
  rw [ConcreteSpec.Core.SourcePath.scan_split]
  rfl

private theorem parts_eq {left right : SourcePath} :
    Reference.parts left = Reference.parts right ↔ left.raw = right.raw := by
  constructor
  · intro h
    have bytes := congrArg ([47].intercalate) h
    simp only [Reference.parts, List.intercalate_splitOn] at bytes
    cases hl : left.raw with
    | mk ls lhs =>
      cases hr : right.raw with
      | mk rs rhs =>
        rw [hl, hr] at bytes
        exact SourceText.mk.injEq .. |>.mpr bytes
  · intro h
    simp [Reference.parts, h]

private theorem classify_correct (left right : SourcePath) (reason : Reason) :
    classify left right = some reason ↔ Reference.Conflict reason left right := by
  unfold classify
  rw [component_bytes, component_bytes, classifyComponents_correct]
  cases reason <;> simp [Reference.Conflict, parts_eq]

private theorem conflict_symm {reason : Reason} {left right : SourcePath}
    (h : Reference.Conflict reason left right) : Reference.Conflict reason right left := by
  cases reason with
  | caseCollision => exact caseCollision_symm h
  | fileDirectoryConflict => exact ⟨Ne.symm h.1, h.2.symm⟩
  | duplicate => exact h.symm

/-- A checker accepts only pairwise-admitted projects. -/
def Safety (checker : List SourcePath → Except Refusal Unit) : Prop :=
  ∀ paths, checker paths = .ok () → Reference.Admitted paths

/-- A checker accepts every pairwise-admitted project. -/
def Progress (checker : List SourcePath → Except Refusal Unit) : Prop :=
  ∀ paths, Reference.Admitted paths → checker paths = .ok ()

/-- Both reported positions are in bounds and distinct, with the reported conflict. -/
def RefusalEvidence (checker : List SourcePath → Except Refusal Unit) : Prop :=
  ∀ paths refusal, checker paths = .error refusal →
    ∃ (first : refusal.first < paths.length) (second : refusal.second < paths.length),
      refusal.first ≠ refusal.second ∧
        Reference.Conflict refusal.reason paths[refusal.first] paths[refusal.second]

/-- A permutation preserves whether the checker accepts. -/
def OrderIndependent (checker : List SourcePath → Except Refusal Unit) : Prop :=
  ∀ left right, left.Perm right → (checker left = .ok () ↔ checker right = .ok ())

private theorem check_accepts_iff (paths : List SourcePath) :
    check paths = .ok () ↔ Reference.Admitted paths := by
  unfold check checkWith Reference.Admitted
  simp only [List.getElem_toArray]
  rw [List.pairwise_iff_getElem]
  simp only [← classify_correct]
  constructor
  · intro accepted i j hi hj before reason conflict
    split at accepted
    next noneFound =>
      have outer := (List.findSome?_eq_none_iff.mp noneFound) (⟨i, hi⟩ : Fin paths.length) (by simp)
      have inner := (List.findSome?_eq_none_iff.mp outer) (⟨j, hj⟩ : Fin paths.length) (by simp)
      simp [before, conflict] at inner
    next => contradiction
  · intro clear
    have noneFound : (List.finRange paths.length).findSome? (fun first =>
        (List.finRange paths.length).findSome? (fun second =>
          if first.val < second.val then
            (classify paths[first.val] paths[second.val]).map fun reason =>
              { reason, first := first.val, second := second.val : Refusal }
          else none)) = none := by
      apply List.findSome?_eq_none_iff.mpr
      intro first _
      apply List.findSome?_eq_none_iff.mpr
      intro second _
      split
      · next before =>
        cases found : classify paths[first.val] paths[second.val] with
        | none => rfl
        | some reason => exact (clear first.val second.val first.isLt second.isLt
            before reason found).elim
      · rfl
    rw [noneFound]

/-- Law (subject `ConcreteSpec.Core.SourcePath.Project.check`). -/
theorem check_safety : Safety check := fun paths => (check_accepts_iff paths).mp

/-- Law (subject `ConcreteSpec.Core.SourcePath.Project.check`). -/
theorem check_progress : Progress check := fun paths => (check_accepts_iff paths).mpr

/-- Law (subject `ConcreteSpec.Core.SourcePath.Project.check`). -/
theorem check_refusal_evidence : RefusalEvidence check := by
  intro paths refusal reported
  unfold check checkWith at reported
  simp only [List.getElem_toArray] at reported
  split at reported
  · contradiction
  · next conflict found =>
    cases reported
    obtain ⟨first, _, found⟩ := List.exists_of_findSome?_eq_some found
    obtain ⟨second, _, found⟩ := List.exists_of_findSome?_eq_some found
    split at found
    · next before =>
      change (classify paths[first.val] paths[second.val]).map _ = some refusal at found
      cases why : classify paths[first.val] paths[second.val] with
      | none => rw [why] at found; contradiction
      | some reason =>
        simp only [why, Option.map_some, Option.some.injEq] at found
        subst refusal
        exact ⟨first.isLt, second.isLt, Nat.ne_of_lt before,
          (classify_correct _ _ _).mp why⟩
    · contradiction

/-- Law (subject `ConcreteSpec.Core.SourcePath.Project.check`). -/
theorem check_order_independent : OrderIndependent check := by
  intro left right perm
  rw [check_accepts_iff, check_accepts_iff]
  have symmetric : Std.Symm (fun left right => ∀ reason, ¬ Reference.Conflict reason left right) :=
    ⟨fun _ _ clear reason conflict => clear reason (conflict_symm conflict)⟩
  exact ⟨fun h => h.perm perm (symmetric.symm _ _), fun h => h.perm perm.symm (symmetric.symm _ _)⟩

/-- A local refusal wins over project conflicts, even conflicts in earlier inputs. -/
def LocalFirst (parser : List SourceText →
    Except (SourcePath.Refusal ⊕ Refusal) (List SourcePath)) : Prop :=
  ∀ (before after : List SourceText) (raw : SourceText) (refusal : SourcePath.Refusal),
    (∀ input ∈ before, ∃ path, SourcePath.parse input = .ok path) →
    SourcePath.parse raw = .error refusal →
    parser (before ++ raw :: after) = .error (.inl refusal)

/-- Law (subject `ConcreteSpec.Core.SourcePath.Project.parse`). -/
theorem parse_local_first : LocalFirst parse := by
  intro before suffix raw refusal earlier refused
  have localRefusal : parseLocal (before ++ raw :: suffix) = .error refusal := by
    induction before with
    | nil => simp [parseLocal, refused, bind, Except.bind]
    | cons input rest ih =>
      obtain ⟨path, accepted⟩ := earlier input (by simp)
      have tail := ih (fun item member => earlier item (by simp [member]))
      simp [parseLocal, accepted, tail, bind, Except.bind]
  simp [parse, localRefusal, Except.mapError, bind, Except.bind]


namespace Controls

open ConcreteSpec.Core.SourcePath.Project.Registry (fileA fileB lowerDirectory upperDirectory)

def sameDirectory : SourcePath := ⟨SourceText.ofBytes [97, 47, 99, 46, 99, 111, 110], by decide⟩
def lowerFile : SourcePath := ⟨SourceText.ofBytes [97, 47, 98, 46, 99, 111, 110], by decide⟩

def directoryCase : List SourcePath := [lowerDirectory, upperDirectory]
def filenameCase : List SourcePath := [lowerDirectory, lowerFile]
def exactPrefix : List SourcePath := [fileA, lowerDirectory]
def sharedDirectory : List SourcePath := [lowerDirectory, sameDirectory]
def evidenceInput : List SourcePath := [fileA, lowerDirectory, fileB]

def ignoreDirectoryCase : List SourcePath → Except Refusal Unit :=
  checkWith fun left right =>
    match classify left right with
    | some .caseCollision =>
      if left.raw.bytes.map foldAscii = right.raw.bytes.map foldAscii then some .caseCollision
      else none
    | other => other

def caseSensitive : List SourcePath → Except Refusal Unit :=
  checkWith fun left right =>
    match classify left right with
    | some .caseCollision => none
    | other => other

def ignoreFileDirectory : List SourcePath → Except Refusal Unit :=
  checkWith fun left right =>
    match classify left right with
    | some .fileDirectoryConflict => none
    | other => other

def rejectSharedDirectory : List SourcePath → Except Refusal Unit :=
  checkWith fun left right =>
    match classify left right, (components left.raw).map Component.bytes,
        (components right.raw).map Component.bytes with
    | none, first :: _ :: _, second :: _ :: _ =>
      if first = second then some .caseCollision else none
    | other, _, _ => other

def wrongSecondOccurrence (paths : List SourcePath) : Except Refusal Unit :=
  (check paths).mapError fun refusal => { refusal with second := refusal.second + 1 }

def onlyFileBeforeDirectory : List SourcePath → Except Refusal Unit :=
  checkWith fun left right =>
    match classify left right with
    | some .fileDirectoryConflict =>
      if left.raw.length < right.raw.length then some .fileDirectoryConflict else none
    | other => other

/-- Planted (law `ConcreteSpec.Core.SourcePath.Project.check_safety`,
case `ConcreteSpec.Core.SourcePath.Project.Controls.directoryCase`, subject `ConcreteSpec.Core.SourcePath.Project.check`,
mutant `ConcreteSpec.Core.SourcePath.Project.Controls.ignoreDirectoryCase`). -/
theorem ignoreDirectoryCase_violates_safety : ¬ Safety ignoreDirectoryCase := by
  intro law
  have admitted := law directoryCase (by decide)
  rw [Reference.Admitted, directoryCase, List.pairwise_cons] at admitted
  exact admitted.1 upperDirectory (by simp)
    .caseCollision ⟨1, by decide, by decide, by decide, by decide⟩

/-- Planted (law `ConcreteSpec.Core.SourcePath.Project.check_safety`,
case `ConcreteSpec.Core.SourcePath.Project.Controls.filenameCase`, subject `ConcreteSpec.Core.SourcePath.Project.check`,
mutant `ConcreteSpec.Core.SourcePath.Project.Controls.caseSensitive`). -/
theorem caseSensitive_violates_safety : ¬ Safety caseSensitive := by
  intro law
  have admitted := law filenameCase (by decide)
  rw [Reference.Admitted, filenameCase, List.pairwise_cons] at admitted
  exact admitted.1 lowerFile (by simp)
    .caseCollision ⟨2, by decide, by decide, by decide, by decide⟩

/-- Planted (law `ConcreteSpec.Core.SourcePath.Project.check_safety`,
case `ConcreteSpec.Core.SourcePath.Project.Controls.exactPrefix`, subject `ConcreteSpec.Core.SourcePath.Project.check`,
mutant `ConcreteSpec.Core.SourcePath.Project.Controls.ignoreFileDirectory`). -/
theorem ignoreFileDirectory_violates_safety : ¬ Safety ignoreFileDirectory := by
  intro law
  have admitted := law exactPrefix (by decide)
  rw [Reference.Admitted, exactPrefix, List.pairwise_cons] at admitted
  exact admitted.1 lowerDirectory (by simp) .fileDirectoryConflict
    ⟨by decide, Or.inl ⟨[[66, 46, 99, 111, 110]], rfl⟩⟩

/-- Planted (law `ConcreteSpec.Core.SourcePath.Project.check_progress`,
case `ConcreteSpec.Core.SourcePath.Project.Controls.sharedDirectory`, subject `ConcreteSpec.Core.SourcePath.Project.check`,
mutant `ConcreteSpec.Core.SourcePath.Project.Controls.rejectSharedDirectory`). -/
theorem rejectSharedDirectory_violates_progress : ¬ Progress rejectSharedDirectory := by
  intro law
  have admitted : Reference.Admitted sharedDirectory := by
    unfold Reference.Admitted sharedDirectory
    simp only [List.pairwise_cons, List.mem_singleton, List.not_mem_nil, false_implies,
      implies_true, List.Pairwise.nil, and_true]
    intro path eq reason conflict
    subst path
    have found := (classify_correct _ _ _).mpr conflict
    change none = some reason at found
    contradiction
  exact (by decide : rejectSharedDirectory sharedDirectory ≠ .ok ()) (law _ admitted)

/-- Planted (law `ConcreteSpec.Core.SourcePath.Project.check_refusal_evidence`,
case `ConcreteSpec.Core.SourcePath.Project.Controls.evidenceInput`, subject `ConcreteSpec.Core.SourcePath.Project.check`,
mutant `ConcreteSpec.Core.SourcePath.Project.Controls.wrongSecondOccurrence`). -/
theorem wrongSecondOccurrence_violates_refusal_evidence : ¬ RefusalEvidence wrongSecondOccurrence := by
  intro law
  obtain ⟨_, _, _, conflict⟩ := law evidenceInput ⟨.fileDirectoryConflict, 0, 2⟩ (by decide)
  have found := (classify_correct _ _ _).mpr conflict
  change none = some Reason.fileDirectoryConflict at found
  contradiction

/-- Planted (law `ConcreteSpec.Core.SourcePath.Project.check_order_independent`,
case `ConcreteSpec.Core.SourcePath.Project.Controls.exactPrefix`, subject `ConcreteSpec.Core.SourcePath.Project.check`,
mutant `ConcreteSpec.Core.SourcePath.Project.Controls.onlyFileBeforeDirectory`). -/
theorem onlyFileBeforeDirectory_violates_order_independence :
    ¬ OrderIndependent onlyFileBeforeDirectory := by
  intro law
  have perm : exactPrefix.Perm [lowerDirectory, fileA] := List.Perm.swap ..
  exact (by decide : onlyFileBeforeDirectory exactPrefix ≠ .ok ()) ((law _ _ perm).mpr (by decide))


def beforeLocalRefusal : List SourceText := [fileA.raw, fileA.raw]

def prematureProjectCheck (inputs : List SourceText) :
    Except (SourcePath.Refusal ⊕ Refusal) (List SourcePath) :=
  let admitted := inputs.filterMap fun raw => (SourcePath.parse raw).toOption
  match check admitted with
  | .error refusal => .error (.inr refusal)
  | .ok () => parse inputs

/-- Planted (law `ConcreteSpec.Core.SourcePath.Project.parse_local_first`,
case `ConcreteSpec.Core.SourcePath.Project.Controls.beforeLocalRefusal`, subject `ConcreteSpec.Core.SourcePath.Project.parse`,
mutant `ConcreteSpec.Core.SourcePath.Project.Controls.prematureProjectCheck`). -/
theorem prematureProjectCheck_violates_local_first : ¬ LocalFirst prematureProjectCheck := by
  intro law
  let raw := SourceText.ofBytes []
  let refusal : SourcePath.Refusal := ⟨raw, ⟨.empty, wholeLocation raw 0 (by decide)⟩⟩
  have refused : SourcePath.parse raw = .error refusal := rfl
  have earlier : ∀ input ∈ beforeLocalRefusal, ∃ path, SourcePath.parse input = .ok path := by
    intro input member
    have eq : input = fileA.raw := by simpa [beforeLocalRefusal] using member
    subst input
    exact ⟨fileA, rfl⟩
  have project : prematureProjectCheck (beforeLocalRefusal ++ [raw]) =
      .error (.inr ⟨.duplicate, 0, 1⟩) := rfl
  have localResult := law beforeLocalRefusal [] raw refusal earlier refused
  rw [project] at localResult
  injection localResult with incompatible
  cases incompatible

end Controls

end ConcreteSpec.Core.SourcePath.Project
