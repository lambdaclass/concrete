import ConcreteSpec.Spec.Basics
import ConcreteSpec.Spec.Ident

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

Hold lines 11–279 (SourcePath), 1300–1305 (Span projection), and 2341–2355 (Span controls)
for F04p, tracked with the F03p path interface by ISS-0068. Path backslashes, controls
and Unicode normalization/identity lack settled independent owner authority. U+FEFF
scope and bare-CR comment termination (ISS-0064 and ISS-0065), and the five queued
partial items in `decided-2026-11-08-recovery-queue` (ISS-0065), are likewise excluded; neither this
arithmetic nor any fixture resolves them. No assumed-yes branch is an implementation
or authority source.
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
    (rest : List KeyConstruct) (index : Nat) (route : List Nat) :
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
    (route : List Nat) (key : FunctionRelativeKey) (nonempty : route ≠ [])
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
    (label : String) (kind : ConstructKind) (children : List KeyConstruct) (target : KeyConstruct) :
    target.key (before ++ KeyConstruct.mk kind (.named label) children :: after) =
      target.key (before ++ after) := by
  simp [KeyConstruct.key, KeyConstruct.segment, List.filter_append, ConstructName.isUnnamed]

/-- Helper (projection). Renaming preserves participation in unnamed-sibling ordinals. -/
theorem ConstructName.isUnnamed_rename (constructName : ConstructName) (name : String) :
    (constructName.rename name).isUnnamed = constructName.isUnnamed := by
  cases constructName <;> rfl

/-- Helper (projection). Pinning or unpinning an unnamed construct's label preserves participation
in unnamed-sibling ordinals. -/
theorem ConstructName.isUnnamed_pin (constructName : ConstructName) (label : Option String) :
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
    (target other : KeyConstruct) (name : String) :
    other.key (before ++ target.rename name :: after) = other.key (before ++ target :: after) := by
  simp only [KeyConstruct.key]
  rw [KeyConstruct.segment_replace_same_unnamed before after (target.rename name) target other
    rfl (ConstructName.isUnnamed_rename target.name name)]

/-- Law (subject `ConcreteSpec.Core.KeyConstruct.key`, `ConcreteSpec.Core.KeyConstruct.pin`).
**Pinning or unpinning an unnamed item's label leaves every sibling key unchanged.** Pinning
preserves the item's kind and unnamed status, so it still occupies its place in the per-kind
ordinal sequence. -/
theorem KeyConstruct.key_pin_other (before after : List KeyConstruct)
    (target other : KeyConstruct) (label : Option String) :
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
    (old : String) (children : List KeyConstruct) (name : String) :
    ((KeyConstruct.mk kind (.named old) children).rename name).key preceding =
      ⟨[⟨kind, .named name⟩]⟩ := by
  simp [KeyConstruct.key, KeyConstruct.segment, KeyConstruct.rename, ConstructName.rename]

/-- Helper (computation). Computes the pinned root construct's own key for
`KeyConstruct.assignFunctionTable_pin`. -/
theorem KeyConstruct.key_pin_unnamed (preceding : List KeyConstruct) (kind : ConstructKind)
    (old : Option String) (children : List KeyConstruct) (label : String) :
    ((KeyConstruct.mk kind (.unnamed old) children).pin (some label)).key preceding =
      ⟨[⟨kind, .pinned label⟩]⟩ := by
  simp [KeyConstruct.key, KeyConstruct.segment, KeyConstruct.pin, ConstructName.pin]

/-- Helper (congruence). Replacing an in-scope construct with one of the same kind and unnamed
status leaves `KeyConstruct.pathAtIn` unchanged. -/
theorem KeyConstruct.pathAtIn_replace_same_unnamed (forest : List KeyConstruct)
    (route : List Nat) (before after : List KeyConstruct) (first second : KeyConstruct)
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
theorem KeyConstruct.pathAtIn_insert_named (forest : List KeyConstruct) (route : List Nat)
    (before after : List KeyConstruct) (label : String) (kind : ConstructKind)
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
theorem KeyConstruct.pathAtIn_insert_differentKind (forest : List KeyConstruct) (route : List Nat)
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
    (target : KeyConstruct) (route : List Nat) :
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
    (inserted target : KeyConstruct) (route : List Nat)
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
    (label : String) (kind : ConstructKind) (children : List KeyConstruct)
    (target : KeyConstruct) (route : List Nat) :
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
    (target inserted : KeyConstruct) (route : List Nat) :
    KeyConstruct.pathAt (before ++ target :: (mid ++ inserted :: after)) (before.length :: route) =
      KeyConstruct.pathAt (before ++ target :: (mid ++ after)) (before.length :: route) := by
  rw [KeyConstruct.pathAt_at_length, KeyConstruct.pathAt_at_length]

/-- Law (subject `ConcreteSpec.Core.KeyConstruct.pathAt`, `ConcreteSpec.Core.ConstructName.isUnnamed`).
**Inserting a named sibling before `target` in a forest shifts `target`'s raw child index without
changing its assigned path.** -/
theorem KeyConstruct.pathAt_insert_named (before mid after : List KeyConstruct)
    (label : String) (kind : ConstructKind) (children : List KeyConstruct)
    (target : KeyConstruct) (route : List Nat) :
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
    (inserted target : KeyConstruct) (route : List Nat)
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
    (kind : ConstructKind) (name : String) (children : List KeyConstruct) :
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
    (preceding₁ preceding₂ : List KeyConstruct) (kind : ConstructKind) (name : String)
    (children : List KeyConstruct) (route : List Nat) :
    (KeyConstruct.mk kind (.named name) children).descendantKey preceding₁ route =
      (KeyConstruct.mk kind (.named name) children).descendantKey preceding₂ route := by
  simp [KeyConstruct.descendantKey, KeyConstruct.childScope, KeyConstruct.segment]

/-- Helper (computation). Transports the named-root scope equation through forest locators.
This is not assignment soundness; `KeyConstruct.assignFunctionTable_named_scope` establishes
preservation of the complete assigned subtree. -/
theorem KeyConstruct.pathAt_named_ancestor_resets_scope
    (before₁ after₁ before₂ after₂ : List KeyConstruct)
    (kind : ConstructKind) (name : String) (children : List KeyConstruct) (route : List Nat) :
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
    (target other : KeyConstruct) (name : String) (route : List Nat) :
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
    (target other : KeyConstruct) (label : Option String) (route : List Nat) :
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
    (kind : ConstructKind) (old name : String) (children : List KeyConstruct) (route : List Nat) :
    ((KeyConstruct.mk kind (.named old) children).rename name).descendantKey preceding route =
      (KeyConstruct.pathAt children route).map (fun suffix =>
        ⟨⟨kind, .named name⟩ :: suffix.path⟩) := by
  simp [KeyConstruct.descendantKey, KeyConstruct.rename, ConstructName.rename,
    KeyConstruct.segment, KeyConstruct.childScope, KeyConstruct.pathAt]

/-- Helper (computation). Transports a located descendant through the occurrence/table
correspondence; supplies no independent soundness evidence. -/
theorem KeyConstruct.assignFunctionTable_descendant_mem (f : FnId) (startSite : Nat)
    (before after : List KeyConstruct) (root : KeyConstruct) (route : List Nat)
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
    (before after : List KeyConstruct) (kind : ConstructKind) (old name : String)
    (children : List KeyConstruct) (route : List Nat) (suffix : FunctionRelativeKey)
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
    (before after : List KeyConstruct) (kind : ConstructKind) (old : Option String)
    (label : String) (children : List KeyConstruct) (route : List Nat) (suffix : FunctionRelativeKey)
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

/-- Law (subject `ConcreteSpec.Core.SourceText.lineAt`).
Appending a byte updates the source line count by its actual newline step. -/
theorem SourceText.lineAt_append (source : SourceText) (byte : UInt8) :
    (source ++ [byte]).lineAt (source.length + 1) = source.lineAt source.length + if byte = 10 then 1 else 0 := by
  have whole : (source ++ [byte]).take (source.length + 1) = source ++ [byte] :=
    List.take_of_length_le (by simp)
  simp only [SourceText.lineAt]
  rw [whole]
  by_cases newline : byte = 10 <;> simp [newline]

/-- Law (subject `ConcreteSpec.Core.SourceText.lineAt`).
A newline starts a new line. -/
theorem SourceText.lineAt_append_newline (source : SourceText) :
    (source ++ [10]).lineAt (source.length + 1) = source.lineAt source.length + 1 := by
  simpa using SourceText.lineAt_append source 10

/-- Law (subject `ConcreteSpec.Core.SourceText.lineAt`).
A non-newline byte leaves the line unchanged. -/
theorem SourceText.lineAt_append_other (source : SourceText) (byte : UInt8) (other : byte ≠ 10) :
    (source ++ [byte]).lineAt (source.length + 1) = source.lineAt source.length := by
  simpa [other] using SourceText.lineAt_append source byte

/-- Law (subject `ConcreteSpec.Core.SourceText.columnAt`).
Appending a byte updates the byte column: newline resets to one, every other byte adds one. -/
theorem SourceText.columnAt_append (source : SourceText) (byte : UInt8) :
    (source ++ [byte]).columnAt (source.length + 1) =
      if byte = 10 then 1 else source.columnAt source.length + 1 := by
  have whole : (source ++ [byte]).take (source.length + 1) = source ++ [byte] :=
    List.take_of_length_le (by simp)
  simp only [SourceText.columnAt]
  rw [whole]
  by_cases newline : byte = 10 <;> simp [newline]

/-- Law (subject `ConcreteSpec.Core.SourceText.columnAt`).
A tab is one byte and therefore advances exactly one column. -/
theorem SourceText.columnAt_append_tab (source : SourceText) :
    (source ++ [9]).columnAt (source.length + 1) = source.columnAt source.length + 1 := by
  simpa using SourceText.columnAt_append source 9

/-- Law (subject `ConcreteSpec.Core.SourceText.columnAt`).
A newline resets the byte column to one. -/
theorem SourceText.columnAt_append_newline (source : SourceText) :
    (source ++ [10]).columnAt (source.length + 1) = 1 := by
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
def package (name : String := "demo") : PackageId := ⟨name, ⟨"1"⟩⟩

/-- An enclosing function used by the persistent-key examples. -/
def function : FnId := FnId.mk (ModuleId.mk package ["demo"] (by decide)) "f"

/-- A second enclosing function. -/
def otherFunction : FnId := FnId.mk (ModuleId.mk package ["demo"] (by decide)) "g"

/-- A declaration and constant that own obligations outside function bodies. -/
def declaration : DeclId := ⟨function.module, "S"⟩
def constant : ConstId := ⟨function.module, "N"⟩

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
def namedBinding : KeyConstruct := KeyConstruct.mk .binding (.named "x") []
def namedCall : KeyConstruct := KeyConstruct.mk .call (.named "work") []
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
  [⟨.loop, .unnamed none, [⟨.primitive, .unnamed (some "effect"), []⟩]⟩]⟩
def namedNested : KeyConstruct := ⟨.block, .named "body", nestedBody.children⟩
def nestedPath : FunctionRelativeKey :=
  ⟨[⟨.block, .ordinal 0⟩, ⟨.loop, .ordinal 0⟩, ⟨.primitive, .pinned "effect"⟩]⟩
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
def missingPathAtIn (_scope _forest : List KeyConstruct) (_route : List Nat) :
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
example : (([0, 0, 0] : List Nat) ≠ [] ∧
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
def nestedSuffix : FunctionRelativeKey := ⟨[⟨.loop, .ordinal 0⟩, ⟨.primitive, .pinned "effect"⟩]⟩
def renamedRootKey : FunctionRelativeKey := ⟨[⟨.block, .named "new"⟩]⟩
def renamedPath : FunctionRelativeKey := ⟨⟨.block, .named "new"⟩ :: nestedSuffix.path⟩
def pinnedRootKey : FunctionRelativeKey := ⟨[⟨.block, .pinned "pin"⟩]⟩
def pinnedPath : FunctionRelativeKey := ⟨⟨.block, .pinned "pin"⟩ :: nestedSuffix.path⟩

/-- Mutant rename that leaves the construct's old name unchanged. -/
def noopRename (construct : KeyConstruct) (_name : String) : KeyConstruct := construct

/-- Mutant pin that ignores the supplied user label. -/
def noopPin (construct : KeyConstruct) (_label : Option String) : KeyConstruct := construct

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
      ([unnamedLoop] ++ KeyConstruct.mk .block (.named "body") nestedBody.children :: [namedCall]))).drop
        (KeyConstruct.collectKeys [unnamedLoop]).length).take
          (KeyConstruct.collectKeys [KeyConstruct.mk .block (.named "body") nestedBody.children]).length =
    ((List.map Prod.snd (KeyConstruct.assignFunctionTable function 19
      ([] ++ KeyConstruct.mk .block (.named "body") nestedBody.children :: [unnamedBinding]))).drop
        (KeyConstruct.collectKeys []).length).take
          (KeyConstruct.collectKeys [KeyConstruct.mk .block (.named "body") nestedBody.children]).length :=
  KeyConstruct.assignFunctionTable_named_scope function 7 19 [unnamedLoop] [namedCall]
    [] [unnamedBinding] .block "body" nestedBody.children

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.assignFunctionTable_named_scope`, case `ConcreteSpec.Core.Vacuity.namedNested`, subject `ConcreteSpec.Core.KeyConstruct.assignFunctionTable`, mutant `ConcreteSpec.Core.Vacuity.unresetScopeAssign`). -/
example : ¬ (((List.map Prod.snd (unresetScopeAssign function 7
      ([unnamedLoop] ++ KeyConstruct.mk .block (.named "body") nestedBody.children :: [namedCall]))).drop
        (KeyConstruct.collectKeys [unnamedLoop]).length).take
          (KeyConstruct.collectKeys [KeyConstruct.mk .block (.named "body") nestedBody.children]).length =
    ((List.map Prod.snd (unresetScopeAssign function 19
      ([] ++ KeyConstruct.mk .block (.named "body") nestedBody.children :: [unnamedBinding]))).drop
        (KeyConstruct.collectKeys []).length).take
          (KeyConstruct.collectKeys [KeyConstruct.mk .block (.named "body") nestedBody.children]).length) := by
  simp [unresetScopeAssign, KeyConstruct.numberFunctionKeys_snd, unresetScopeCollect,
    KeyConstruct.collectKeys, KeyConstruct.collectKeysIn, KeyConstruct.key,
    KeyConstruct.segment, KeyConstruct.childScope, ConstructName.isUnnamed,
    unnamedLoop, nestedBody, namedCall, unnamedBinding]

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.assignFunctionTable_rename_named`, case `ConcreteSpec.Core.Vacuity.namedNested`). -/
example : (⟨.function function, renamedRootKey⟩ : StableKey) ∈
      (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
        ([unnamedLoop] ++ (KeyConstruct.mk .block (.named "old") nestedBody.children).rename "new" :: []))) ∧
    (⟨.function function, renamedPath⟩ : StableKey) ∈
      (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
        ([unnamedLoop] ++ (KeyConstruct.mk .block (.named "old") nestedBody.children).rename "new" :: []))) :=
  KeyConstruct.assignFunctionTable_rename_named function 7 [unnamedLoop] [] .block "old" "new"
    nestedBody.children [0, 0] nestedSuffix (by decide)

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.assignFunctionTable_rename_named`, case `ConcreteSpec.Core.Vacuity.namedNested`, subject `ConcreteSpec.Core.KeyConstruct.rename`, mutant `ConcreteSpec.Core.Vacuity.noopRename`). -/
example : KeyConstruct.pathAt nestedBody.children [0, 0] = some nestedSuffix ∧
    ¬ ((⟨.function function, renamedRootKey⟩ : StableKey) ∈
        (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
          ([unnamedLoop] ++ noopRename (KeyConstruct.mk .block (.named "old") nestedBody.children) "new" :: []))) ∧
      (⟨.function function, renamedPath⟩ : StableKey) ∈
        (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
          ([unnamedLoop] ++ noopRename (KeyConstruct.mk .block (.named "old") nestedBody.children) "new" :: [])))) := by
  refine ⟨by decide, ?_⟩
  rintro ⟨member, _⟩
  simp [KeyConstruct.assignFunctionTable, KeyConstruct.numberFunctionKeys_snd,
    KeyConstruct.collectKeys, KeyConstruct.collectKeysIn, noopRename, unnamedLoop, nestedBody,
    renamedRootKey, KeyConstruct.key, KeyConstruct.segment, KeyConstruct.childScope] at member

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.assignFunctionTable_rename_named`, case `ConcreteSpec.Core.Vacuity.namedNested`, subject `ConcreteSpec.Core.KeyConstruct.assignFunctionTable`, mutant `ConcreteSpec.Core.Vacuity.unresetScopeAssign`). -/
example : KeyConstruct.pathAt nestedBody.children [0, 0] = some nestedSuffix ∧
    ¬ ((⟨.function function, renamedRootKey⟩ : StableKey) ∈
        (List.map Prod.snd (unresetScopeAssign function 7
          ([unnamedLoop] ++ (KeyConstruct.mk .block (.named "old") nestedBody.children).rename "new" :: []))) ∧
      (⟨.function function, renamedPath⟩ : StableKey) ∈
        (List.map Prod.snd (unresetScopeAssign function 7
          ([unnamedLoop] ++ (KeyConstruct.mk .block (.named "old") nestedBody.children).rename "new" :: [])))) := by
  refine ⟨by decide, ?_⟩
  rintro ⟨_, member⟩
  simp [unresetScopeAssign, KeyConstruct.numberFunctionKeys_snd, unresetScopeCollect,
    KeyConstruct.rename, ConstructName.rename, unnamedLoop, nestedBody, renamedPath, nestedSuffix,
    KeyConstruct.key, KeyConstruct.segment] at member
  revert member
  decide

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.assignFunctionTable_rename_named`, case `ConcreteSpec.Core.Vacuity.namedCall`). -/
example : (⟨.function function, ⟨[⟨.call, .named "renamed"⟩]⟩⟩ : StableKey) ∈
      (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
        ([] ++ (KeyConstruct.mk .call (.named "work") []).rename "renamed" :: []))) ∧
    (⟨.function function, ⟨⟨.call, .named "renamed"⟩ :: ([] : List KeySegment)⟩⟩ : StableKey) ∈
      (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
        ([] ++ (KeyConstruct.mk .call (.named "work") []).rename "renamed" :: []))) :=
  KeyConstruct.assignFunctionTable_rename_named function 7 [] [] .call "work" "renamed"
    [] [] ⟨[]⟩ rfl

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.assignFunctionTable_rename_named`, case `ConcreteSpec.Core.Vacuity.namedCall`, subject `ConcreteSpec.Core.KeyConstruct.rename`, mutant `ConcreteSpec.Core.Vacuity.noopRename`). -/
example : KeyConstruct.pathAt ([] : List KeyConstruct) [] = some ⟨[]⟩ ∧
    ¬ ((⟨.function function, ⟨[⟨.call, .named "renamed"⟩]⟩⟩ : StableKey) ∈
        (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
          ([] ++ noopRename (KeyConstruct.mk .call (.named "work") []) "renamed" :: []))) ∧
      (⟨.function function, ⟨⟨.call, .named "renamed"⟩ :: ([] : List KeySegment)⟩⟩ : StableKey) ∈
        (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
          ([] ++ noopRename (KeyConstruct.mk .call (.named "work") []) "renamed" :: [])))) := by
  refine ⟨rfl, ?_⟩
  rintro ⟨member, _⟩
  simp [KeyConstruct.assignFunctionTable, KeyConstruct.numberFunctionKeys_snd,
    KeyConstruct.collectKeys, KeyConstruct.collectKeysIn, noopRename, KeyConstruct.key,
    KeyConstruct.segment] at member

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.assignFunctionTable_pin`, case `ConcreteSpec.Core.Vacuity.nestedBody`). -/
example : (⟨.function function, pinnedRootKey⟩ : StableKey) ∈
      (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
        ([] ++ (KeyConstruct.mk .block (.unnamed none) nestedBody.children).pin (some "pin") :: []))) ∧
    (⟨.function function, pinnedPath⟩ : StableKey) ∈
      (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
        ([] ++ (KeyConstruct.mk .block (.unnamed none) nestedBody.children).pin (some "pin") :: []))) :=
  KeyConstruct.assignFunctionTable_pin function 7 [] [] .block none "pin" nestedBody.children
    [0, 0] nestedSuffix (by decide)

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.assignFunctionTable_pin`, case `ConcreteSpec.Core.Vacuity.nestedBody`, subject `ConcreteSpec.Core.KeyConstruct.pin`, mutant `ConcreteSpec.Core.Vacuity.noopPin`). -/
example : KeyConstruct.pathAtIn [] nestedBody.children [0, 0] = some nestedSuffix ∧
    ¬ ((⟨.function function, pinnedRootKey⟩ : StableKey) ∈
        (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
          ([] ++ noopPin (KeyConstruct.mk .block (.unnamed none) nestedBody.children) (some "pin") :: []))) ∧
      (⟨.function function, pinnedPath⟩ : StableKey) ∈
        (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
          ([] ++ noopPin (KeyConstruct.mk .block (.unnamed none) nestedBody.children) (some "pin") :: [])))) := by
  refine ⟨by decide, ?_⟩
  rintro ⟨member, _⟩
  simp [KeyConstruct.assignFunctionTable, KeyConstruct.numberFunctionKeys_snd,
    KeyConstruct.collectKeys, KeyConstruct.collectKeysIn, noopPin, nestedBody, pinnedRootKey,
    KeyConstruct.key, KeyConstruct.segment, KeyConstruct.childScope] at member

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.assignFunctionTable_pin`, case `ConcreteSpec.Core.Vacuity.unnamedBinding`). -/
example : (⟨.function function, ⟨[⟨.binding, .pinned "pinned"⟩]⟩⟩ : StableKey) ∈
      (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
        ([] ++ (KeyConstruct.mk .binding (.unnamed none) []).pin (some "pinned") :: []))) ∧
    (⟨.function function, ⟨⟨.binding, .pinned "pinned"⟩ :: ([] : List KeySegment)⟩⟩ : StableKey) ∈
      (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
        ([] ++ (KeyConstruct.mk .binding (.unnamed none) []).pin (some "pinned") :: []))) :=
  KeyConstruct.assignFunctionTable_pin function 7 [] [] .binding none "pinned" [] [] ⟨[]⟩ rfl

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.assignFunctionTable_pin`, case `ConcreteSpec.Core.Vacuity.unnamedBinding`, subject `ConcreteSpec.Core.KeyConstruct.pin`, mutant `ConcreteSpec.Core.Vacuity.noopPin`). -/
example : KeyConstruct.pathAtIn [] ([] : List KeyConstruct) [] = some ⟨[]⟩ ∧
    ¬ ((⟨.function function, ⟨[⟨.binding, .pinned "pinned"⟩]⟩⟩ : StableKey) ∈
        (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
          ([] ++ noopPin (KeyConstruct.mk .binding (.unnamed none) []) (some "pinned") :: []))) ∧
      (⟨.function function, ⟨⟨.binding, .pinned "pinned"⟩ :: ([] : List KeySegment)⟩⟩ : StableKey) ∈
        (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
          ([] ++ noopPin (KeyConstruct.mk .binding (.unnamed none) []) (some "pinned") :: [])))) := by
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
example : unnamedBinding.key ([] ++ KeyConstruct.mk .binding (.named "x") [] :: []) =
    unnamedBinding.key ([] ++ []) :=
  KeyConstruct.key_insert_named [] [] "x" .binding [] unnamedBinding

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.key_insert_named`, case `ConcreteSpec.Core.Vacuity.unnamedBinding`, subject `ConcreteSpec.Core.KeyConstruct.key`, mutant `ConcreteSpec.Core.Vacuity.countNamedKey`). -/
example : ¬ (countNamedKey ([] ++ KeyConstruct.mk .binding (.named "x") [] :: []) unnamedBinding =
    countNamedKey ([] ++ []) unnamedBinding) := by decide

/-- Mutant rename that demotes a named construct to an unnamed pinned construct, causing it to
consume an unnamed ordinal. -/
def demoteRename (construct : KeyConstruct) (name : String) : KeyConstruct :=
  { construct with name := .unnamed (some name) }

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.key_rename_other`, case `ConcreteSpec.Core.Vacuity.namedBinding`). -/
example : unnamedBinding.key ([] ++ namedBinding.rename "y" :: [unnamedBinding]) =
    unnamedBinding.key ([] ++ namedBinding :: [unnamedBinding]) :=
  KeyConstruct.key_rename_other [] [unnamedBinding] namedBinding unnamedBinding "y"

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.key_rename_other`, case `ConcreteSpec.Core.Vacuity.namedBinding`, subject `ConcreteSpec.Core.KeyConstruct.rename`, mutant `ConcreteSpec.Core.Vacuity.demoteRename`). -/
example : ¬ (unnamedBinding.key ([] ++ demoteRename namedBinding "y" :: [unnamedBinding]) =
    unnamedBinding.key ([] ++ namedBinding :: [unnamedBinding])) := by decide

/-- Mutant key assignment that excludes pinned unnamed siblings from the per-kind ordinal count. -/
def labelOnlyOrdinal (preceding : List KeyConstruct) (target : KeyConstruct) : FunctionRelativeKey :=
  let count := (preceding.filter fun sibling => sibling.kind == target.kind &&
    match sibling.name with | .named _ => false | .unnamed label => label.isNone).length
  ⟨[⟨target.kind, .ordinal count⟩]⟩

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.key_pin_other`, case `ConcreteSpec.Core.Vacuity.unnamedBinding`). -/
example : unnamedBinding.key ([] ++ unnamedBinding.pin (some "x") :: [unnamedBinding]) =
    unnamedBinding.key ([] ++ unnamedBinding :: [unnamedBinding]) :=
  KeyConstruct.key_pin_other [] [unnamedBinding] unnamedBinding unnamedBinding (some "x")

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.key_pin_other`, case `ConcreteSpec.Core.Vacuity.unnamedBinding`, subject `ConcreteSpec.Core.KeyConstruct.key`, mutant `ConcreteSpec.Core.Vacuity.labelOnlyOrdinal`). -/
example : ¬ (labelOnlyOrdinal ([] ++ unnamedBinding.pin (some "x") :: [unnamedBinding]) unnamedBinding =
    labelOnlyOrdinal ([] ++ unnamedBinding :: [unnamedBinding]) unnamedBinding) := by decide

/-- Helper computation example (not law evidence). -/
example : ((KeyConstruct.mk .call (.named "work") []).rename "renamed").key [] =
    ⟨[⟨.call, .named "renamed"⟩]⟩ :=
  KeyConstruct.key_rename_named [] .call "work" [] "renamed"

/-- Helper computation example (not law evidence). -/
example : ((KeyConstruct.mk .binding (.unnamed none) []).pin (some "pinned")).key [] =
    ⟨[⟨.binding, .pinned "pinned"⟩]⟩ :=
  KeyConstruct.key_pin_unnamed [] .binding none [] "pinned"

/-- Mutant `descendantKey` that uses one ordinal across all construct kinds for the root segment. -/
def allKindsDescendantKey (preceding : List KeyConstruct) (construct : KeyConstruct)
    (route : List Nat) : Option FunctionRelativeKey :=
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
    (route : List Nat) : Option FunctionRelativeKey :=
  (KeyConstruct.pathAtIn (construct.childScope preceding) construct.children route).map
    fun descendant => ⟨(countNamedKey preceding construct).path ++ descendant.path⟩

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.descendant_insert_named`, case `ConcreteSpec.Core.Vacuity.nestedBody`). -/
example : nestedBody.descendantKey ([] ++ KeyConstruct.mk .block (.named "body") nestedBody.children :: []) [0, 0] =
    nestedBody.descendantKey ([] ++ []) [0, 0] :=
  KeyConstruct.descendant_insert_named [] [] "body" .block nestedBody.children nestedBody [0, 0]

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.descendant_insert_named`, case `ConcreteSpec.Core.Vacuity.nestedBody`, subject `ConcreteSpec.Core.KeyConstruct.descendantKey`, mutant `ConcreteSpec.Core.Vacuity.countNamedDescendantKey`). -/
example : ¬ (countNamedDescendantKey ([] ++ KeyConstruct.mk .block (.named "body") nestedBody.children :: []) nestedBody [0, 0] =
    countNamedDescendantKey ([] ++ []) nestedBody [0, 0]) := by decide

/-- Mutant `pathAt` that counts ordinals across the whole forest rather than `forest.take index`,
so inserting a sibling after `target` shifts `target`'s ordinal. -/
def wholeForestPathAt (forest : List KeyConstruct) : List Nat → Option FunctionRelativeKey
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
def countNamedPathAt (forest : List KeyConstruct) : List Nat → Option FunctionRelativeKey
  | [] => some ⟨[]⟩
  | index :: route =>
    match forest[index]? with
    | none => none
    | some construct =>
      countNamedDescendantKey (forest.take index) construct route

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.pathAt_insert_named`, case `ConcreteSpec.Core.Vacuity.nestedBody`). -/
example : KeyConstruct.pathAt
      (([] ++ KeyConstruct.mk .block (.named "body") nestedBody.children :: []) ++ nestedBody :: [])
      (([] ++ KeyConstruct.mk .block (.named "body") nestedBody.children :: []).length :: [0, 0]) =
    KeyConstruct.pathAt
      (([] ++ []) ++ nestedBody :: [])
      (([] ++ ([] : List KeyConstruct)).length :: [0, 0]) :=
  KeyConstruct.pathAt_insert_named [] [] [] "body" .block nestedBody.children nestedBody [0, 0]

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.pathAt_insert_named`, case `ConcreteSpec.Core.Vacuity.nestedBody`, subject `ConcreteSpec.Core.KeyConstruct.pathAt`, mutant `ConcreteSpec.Core.Vacuity.countNamedPathAt`). -/
example : ¬ (countNamedPathAt
      (([] ++ KeyConstruct.mk .block (.named "body") nestedBody.children :: []) ++ nestedBody :: [])
      (([] ++ KeyConstruct.mk .block (.named "body") nestedBody.children :: []).length :: [0, 0]) =
    countNamedPathAt
      (([] ++ []) ++ nestedBody :: [])
      (([] ++ ([] : List KeyConstruct)).length :: [0, 0])) := by decide

/-- Mutant `pathAt` that counts all unnamed siblings regardless of kind. -/
def allKindsPathAt (forest : List KeyConstruct) : List Nat → Option FunctionRelativeKey
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
    (route : List Nat) : Option FunctionRelativeKey :=
  (KeyConstruct.pathAtIn preceding construct.children route).map
    fun descendant => ⟨construct.segment preceding :: descendant.path⟩

/-- Mutant `pathAt` that fails to reset the sibling scope when entering a `.named` construct. -/
def unresetScopePathAt (forest : List KeyConstruct) : List Nat → Option FunctionRelativeKey
  | [] => some ⟨[]⟩
  | index :: route =>
    match forest[index]? with
    | none => none
    | some construct =>
      unresetScopeDescendantKey (forest.take index) construct route

/-- Regression example (not independent soundness evidence). -/
example : KeyConstruct.pathAt
      ([unnamedLoop] ++ KeyConstruct.mk .block (.named "body") nestedBody.children :: [])
      (([unnamedLoop] : List KeyConstruct).length :: [0, 0]) =
    KeyConstruct.pathAt
      ([] ++ KeyConstruct.mk .block (.named "body") nestedBody.children :: [])
      (([] : List KeyConstruct).length :: [0, 0]) :=
  KeyConstruct.pathAt_named_ancestor_resets_scope [unnamedLoop] [] [] [] .block "body"
    nestedBody.children [0, 0]

/-- Regression example (not independent soundness evidence). -/
example : ¬ (unresetScopePathAt
      ([unnamedLoop] ++ KeyConstruct.mk .block (.named "body") nestedBody.children :: [])
      (([unnamedLoop] : List KeyConstruct).length :: [0, 0]) =
    unresetScopePathAt
      ([] ++ KeyConstruct.mk .block (.named "body") nestedBody.children :: [])
      (([] : List KeyConstruct).length :: [0, 0])) := by decide

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.descendant_rename_other`, case `ConcreteSpec.Core.Vacuity.nestedBody`). -/
example : nestedBody.descendantKey ([] ++ namedNested.rename "otherName" :: []) [0, 0] =
    nestedBody.descendantKey ([] ++ namedNested :: []) [0, 0] :=
  KeyConstruct.descendant_rename_other [] [] namedNested nestedBody "otherName" [0, 0]

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.descendant_rename_other`, case `ConcreteSpec.Core.Vacuity.nestedBody`, subject `ConcreteSpec.Core.KeyConstruct.rename`, mutant `ConcreteSpec.Core.Vacuity.demoteRename`). -/
example : ¬ (nestedBody.descendantKey ([] ++ demoteRename namedNested "otherName" :: []) [0, 0] =
    nestedBody.descendantKey ([] ++ namedNested :: []) [0, 0]) := by decide

/-- Mutant `descendantKey` that excludes pinned unnamed siblings from the root's ordinal count. -/
def labelOnlyDescendantKey (preceding : List KeyConstruct) (construct : KeyConstruct)
    (route : List Nat) : Option FunctionRelativeKey :=
  (KeyConstruct.pathAtIn (construct.childScope preceding) construct.children route).map
    fun descendant => ⟨(labelOnlyOrdinal preceding construct).path ++ descendant.path⟩

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.descendant_pin_other`, case `ConcreteSpec.Core.Vacuity.nestedBody`). -/
example : nestedBody.descendantKey ([] ++ nestedBody.pin (some "pinned") :: []) [0, 0] =
    nestedBody.descendantKey ([] ++ nestedBody :: []) [0, 0] :=
  KeyConstruct.descendant_pin_other [] [] nestedBody nestedBody (some "pinned") [0, 0]

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.descendant_pin_other`, case `ConcreteSpec.Core.Vacuity.nestedBody`, subject `ConcreteSpec.Core.KeyConstruct.descendantKey`, mutant `ConcreteSpec.Core.Vacuity.labelOnlyDescendantKey`). -/
example : ¬ (labelOnlyDescendantKey ([] ++ nestedBody.pin (some "pinned") :: []) nestedBody [0, 0] =
    labelOnlyDescendantKey ([] ++ nestedBody :: []) nestedBody [0, 0]) := by decide

/-- Representation example (not law evidence). -/
example : ((KeyConstruct.mk .block (.named "body") nestedBody.children).rename "renamed").descendantKey [] [0, 0] =
    (KeyConstruct.pathAt nestedBody.children [0, 0]).map (fun suffix =>
      ⟨⟨.block, .named "renamed"⟩ :: suffix.path⟩) :=
  KeyConstruct.descendant_rename_named [] .block "body" "renamed" nestedBody.children [0, 0]

/-- Representation example (not law evidence). -/
example : ¬ ((noopRename (KeyConstruct.mk .block (.named "body") nestedBody.children) "renamed").descendantKey [] [0, 0] =
    (KeyConstruct.pathAt nestedBody.children [0, 0]).map (fun suffix =>
      ⟨⟨.block, .named "renamed"⟩ :: suffix.path⟩)) := by decide

/-- Three specializations of one generic, differing one argument list at a time. -/
def numeric : SpecializationId Nat String := ⟨.function function, [0], ["Fs"]⟩
def otherType : SpecializationId Nat String := ⟨.function function, [1], ["Fs"]⟩
def otherCapability : SpecializationId Nat String := ⟨.function function, [0], ["Console"]⟩

/-- Distinct request sites for the same specialization. -/
def requestZero : SpecializationRequest Nat String := ⟨⟨0⟩, numeric⟩
def requestOne : SpecializationRequest Nat String := ⟨⟨1⟩, numeric⟩
def requestOtherType : SpecializationRequest Nat String := ⟨⟨2⟩, otherType⟩
def requestOtherCapability : SpecializationRequest Nat String := ⟨⟨3⟩, otherCapability⟩
def edgeZero : SpecializationEdge Nat String := ⟨requestZero, 0⟩

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
    (key : SpecializationRequest Nat String → Key) :
    List (SpecializationRequest Nat String) → List Key × SpecializationTable Nat String
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
def firstModule : LoadedModule := ⟨⟨package "p1", ["first"], by decide⟩, ⟨"registry-a"⟩⟩
def secondModule : LoadedModule := ⟨⟨package "p1", ["second"], by decide⟩, ⟨"registry-a"⟩⟩
def collidingModule : LoadedModule := ⟨⟨package "p2", ["first"], by decide⟩, ⟨"registry-b"⟩⟩
def samePackageOtherOrigin : LoadedModule := ⟨⟨package "p1", ["other"], by decide⟩, ⟨"mirror"⟩⟩
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
def aSource : SourceText := [97]
def aNewline : SourceText := aSource ++ [10]
def aTab : SourceText := aSource ++ [9]
def aTabNewline : SourceText := aTab ++ [10]
def utf8Prefix : SourceText := [195]
def utf8Source : SourceText := [195, 169]
def otherTabSource : SourceText := [10, 97, 98]

/-- Mutant line function that always returns line 1. -/
def constantLine (_source : SourceText) (_offset : Nat) : Nat := 1

/-- Mutant line function that increments on every byte regardless of newline. -/
def offsetLine (_source : SourceText) (offset : Nat) : Nat := offset + 1

/-- Instance (law `ConcreteSpec.Core.SourceText.lineAt_append_newline`, case `ConcreteSpec.Core.Vacuity.aSource`). -/
example : (aSource ++ [10]).lineAt (aSource.length + 1) = aSource.lineAt aSource.length + 1 :=
  SourceText.lineAt_append_newline aSource

/-- Planted (law `ConcreteSpec.Core.SourceText.lineAt_append_newline`, case `ConcreteSpec.Core.Vacuity.aSource`, subject `ConcreteSpec.Core.SourceText.lineAt`, mutant `ConcreteSpec.Core.Vacuity.constantLine`). -/
example : ¬ (constantLine (aSource ++ [10]) (aSource.length + 1) =
    constantLine aSource aSource.length + 1) := by decide

/-- Instance (law `ConcreteSpec.Core.SourceText.lineAt_append_other`, case `ConcreteSpec.Core.Vacuity.aSource`). -/
example : (aSource ++ [9]).lineAt (aSource.length + 1) = aSource.lineAt aSource.length :=
  SourceText.lineAt_append_other aSource 9 (by decide)

/-- Planted (law `ConcreteSpec.Core.SourceText.lineAt_append_other`, case `ConcreteSpec.Core.Vacuity.aSource`, subject `ConcreteSpec.Core.SourceText.lineAt`, mutant `ConcreteSpec.Core.Vacuity.offsetLine`). -/
example : (9 : UInt8) ≠ 10 ∧
    ¬ (offsetLine (aSource ++ [9]) (aSource.length + 1) = offsetLine aSource aSource.length) :=
  ⟨by decide, by decide⟩

/-- Mutant column function that counts a tab byte (`9`) as four columns instead of one. -/
def tabStopFour (source : SourceText) (offset : Nat) : Nat :=
  ((source.take offset).reverse.takeWhile (fun byte => byte != 10)).foldl
    (fun acc byte => acc + if byte == 9 then 4 else 1) 1

/-- Instance (law `ConcreteSpec.Core.SourceText.columnAt_append_tab`, case `ConcreteSpec.Core.Vacuity.aSource`). -/
example : (aSource ++ [9]).columnAt (aSource.length + 1) = aSource.columnAt aSource.length + 1 :=
  SourceText.columnAt_append_tab aSource

/-- Planted (law `ConcreteSpec.Core.SourceText.columnAt_append_tab`, case `ConcreteSpec.Core.Vacuity.aSource`, subject `ConcreteSpec.Core.SourceText.columnAt`, mutant `ConcreteSpec.Core.Vacuity.tabStopFour`). -/
example : ¬ (tabStopFour (aSource ++ [9]) (aSource.length + 1) =
    tabStopFour aSource aSource.length + 1) := by decide

/-- Mutant column function that never resets at a newline byte. -/
def noResetColumn (source : SourceText) (offset : Nat) : Nat :=
  (source.take offset).length + 1

/-- Instance (law `ConcreteSpec.Core.SourceText.columnAt_append_newline`, case `ConcreteSpec.Core.Vacuity.aTab`). -/
example : (aTab ++ [10]).columnAt (aTab.length + 1) = 1 :=
  SourceText.columnAt_append_newline aTab

/-- Planted (law `ConcreteSpec.Core.SourceText.columnAt_append_newline`, case `ConcreteSpec.Core.Vacuity.aTab`, subject `ConcreteSpec.Core.SourceText.columnAt`, mutant `ConcreteSpec.Core.Vacuity.noResetColumn`). -/
example : ¬ (noResetColumn (aTab ++ [10]) (aTab.length + 1) = 1) := by decide

/-- Instance (law `ConcreteSpec.Core.SourceText.lineAt_append`, case `ConcreteSpec.Core.Vacuity.aSource`). -/
example : (aSource ++ [10]).lineAt (aSource.length + 1) =
    aSource.lineAt aSource.length + if (10 : UInt8) = 10 then 1 else 0 :=
  SourceText.lineAt_append aSource 10

/-- Planted (law `ConcreteSpec.Core.SourceText.lineAt_append`, case `ConcreteSpec.Core.Vacuity.aSource`, subject `ConcreteSpec.Core.SourceText.lineAt`, mutant `ConcreteSpec.Core.Vacuity.constantLine`). -/
example : ¬ (constantLine (aSource ++ [10]) (aSource.length + 1) =
    constantLine aSource aSource.length + if (10 : UInt8) = 10 then 1 else 0) := by decide

/-- Mutant column function that skips UTF-8 continuation bytes (`128 ≤ b < 192`). -/
def scalarColumn (source : SourceText) (offset : Nat) : Nat :=
  (((source.take offset).reverse.takeWhile (fun byte => byte != 10)).filter
    (fun byte => decide (byte < 128 ∨ 192 ≤ byte))).length + 1

/-- Instance (law `ConcreteSpec.Core.SourceText.columnAt_append`, case `ConcreteSpec.Core.Vacuity.utf8Prefix`). -/
example : (utf8Prefix ++ [169]).columnAt (utf8Prefix.length + 1) =
    if (169 : UInt8) = 10 then 1 else utf8Prefix.columnAt utf8Prefix.length + 1 :=
  SourceText.columnAt_append utf8Prefix 169

/-- Planted (law `ConcreteSpec.Core.SourceText.columnAt_append`, case `ConcreteSpec.Core.Vacuity.utf8Prefix`, subject `ConcreteSpec.Core.SourceText.columnAt`, mutant `ConcreteSpec.Core.Vacuity.scalarColumn`). -/
example : ¬ (scalarColumn (utf8Prefix ++ [169]) (utf8Prefix.length + 1) =
    if (169 : UInt8) = 10 then 1 else scalarColumn utf8Prefix utf8Prefix.length + 1) := by decide

/-- A source `assume`, a package stack assumption, and their wire records. -/
def sourceAssume : PremiseId :=
  ⟨.assume, .function (StableKey.mk (.function function) ⟨[⟨.assumption, .ordinal 0⟩]⟩)⟩
def stackAssumption : PremiseId := ⟨.stack, .package package ⟨"defaultStack"⟩⟩
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
def conditional : ConditionalFact String := ⟨"bounded", [sourceAssume]⟩
def unconditional : ConditionalFact String := ⟨"bounded", []⟩

/-- Planted counter-example: comparing only conclusions loses the conditional fact's premise debt. -/
def conclusionOnly (first second : ConditionalFact String) : Prop :=
  first.conclusion = second.conclusion

example : conclusionOnly conditional unconditional ∧ conditional ≠ unconditional := by
  exact ⟨rfl, by decide⟩

end Vacuity
end ConcreteSpec.Core
