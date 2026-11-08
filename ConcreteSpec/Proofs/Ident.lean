import ConcreteSpec.Spec.Ident

/-!
# Laws of typed and persistent identities

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). See `ConcreteSpec/README.md`.
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
independently of the starting lexical site number. (approved: PENDING). -/
theorem KeyConstruct.numberFunctionKeys_snd (f : FnId) :
    ∀ (startSite : Nat) (keys : List FunctionRelativeKey),
      List.map Prod.snd (KeyConstruct.numberFunctionKeys f startSite keys) =
        keys.map (fun rel => ⟨.function f, rel⟩)
  | _, [] => rfl
  | startSite, rel :: rest => by
    simp [KeyConstruct.numberFunctionKeys, KeyConstruct.numberFunctionKeys_snd f (startSite + 1) rest]

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

/-- Helper (projection). Renaming preserves participation in unnamed-sibling ordinals.
(approved: rev-c9r4-agy). -/
theorem ConstructName.isUnnamed_rename (constructName : ConstructName) (name : String) :
    (constructName.rename name).isUnnamed = constructName.isUnnamed := by
  cases constructName <;> rfl

/-- Helper (projection). Pinning or unpinning an unnamed construct's label preserves participation
in unnamed-sibling ordinals. (approved: PENDING). -/
theorem ConstructName.isUnnamed_pin (constructName : ConstructName) (label : Option String) :
    (constructName.pin label).isUnnamed = constructName.isUnnamed := by
  cases constructName <;> rfl

/-- Helper (congruence). Replacing one preceding construct with another of the same kind and
unnamed status preserves `KeyConstruct.segment`. (approved: PENDING). -/
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

/-- Law (subject `ConcreteSpec.Core.KeyConstruct.key`, `ConcreteSpec.Core.KeyConstruct.rename`).
**Renaming a named item recomputes its assigned key to the new item name.** Together with
`key_rename_other`, this confines the effect of renaming to the item and its descendants. -/
theorem KeyConstruct.key_rename_named (preceding : List KeyConstruct) (kind : ConstructKind)
    (old : String) (children : List KeyConstruct) (name : String) :
    ((KeyConstruct.mk kind (.named old) children).rename name).key preceding =
      ⟨[⟨kind, .named name⟩]⟩ := by
  simp [KeyConstruct.key, KeyConstruct.segment, KeyConstruct.rename, ConstructName.rename]

/-- Law (subject `ConcreteSpec.Core.KeyConstruct.key`, `ConcreteSpec.Core.KeyConstruct.pin`).
**Pinning an unnamed item replaces its ordinal with the pinned label in its own key.** -/
theorem KeyConstruct.key_pin_unnamed (preceding : List KeyConstruct) (kind : ConstructKind)
    (old : Option String) (children : List KeyConstruct) (label : String) :
    ((KeyConstruct.mk kind (.unnamed old) children).pin (some label)).key preceding =
      ⟨[⟨kind, .pinned label⟩]⟩ := by
  simp [KeyConstruct.key, KeyConstruct.segment, KeyConstruct.pin, ConstructName.pin]

/-- Helper (congruence). Replacing an in-scope construct with one of the same kind and unnamed
status leaves `KeyConstruct.pathAtIn` unchanged. (approved: PENDING). -/
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
`KeyConstruct.pathAtIn` unchanged. (approved: PENDING). -/
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
enclosing named scope leaves `KeyConstruct.pathAtIn` unchanged. (approved: PENDING). -/
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
split forest `before ++ target :: tail`. (approved: PENDING). -/
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

/-- Law (subject `ConcreteSpec.Core.KeyConstruct.descendantKey`, `ConcreteSpec.Core.KeyConstruct.childScope`).
**A named construct establishes its own enclosing named scope.** Preceding siblings outside a
named construct never shift the ordinals of its descendants, even when those siblings share a kind
with an unnamed descendant inside the named construct. -/
theorem KeyConstruct.descendant_named_ancestor_resets_scope
    (preceding₁ preceding₂ : List KeyConstruct) (kind : ConstructKind) (name : String)
    (children : List KeyConstruct) (route : List Nat) :
    (KeyConstruct.mk kind (.named name) children).descendantKey preceding₁ route =
      (KeyConstruct.mk kind (.named name) children).descendantKey preceding₂ route := by
  simp [KeyConstruct.descendantKey, KeyConstruct.childScope, KeyConstruct.segment]

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

/-- Law (subject `ConcreteSpec.Core.KeyConstruct.descendantKey`, `ConcreteSpec.Core.KeyConstruct.rename`).
Renaming a named ancestor replaces its segment throughout its descendants' assigned paths.
The suffix is computed from the actual subtree in its fresh named scope and remains unchanged. -/
theorem KeyConstruct.descendant_rename_named (preceding : List KeyConstruct)
    (kind : ConstructKind) (old name : String) (children : List KeyConstruct) (route : List Nat) :
    ((KeyConstruct.mk kind (.named old) children).rename name).descendantKey preceding route =
      (KeyConstruct.pathAt children route).map (fun suffix =>
        ⟨⟨kind, .named name⟩ :: suffix.path⟩) := by
  simp [KeyConstruct.descendantKey, KeyConstruct.rename, ConstructName.rename,
    KeyConstruct.segment, KeyConstruct.childScope, KeyConstruct.pathAt]

/-- Helper (projection). A successful table lookup is in bounds.
(approved: rev-c9r4-agy). -/
theorem SpecializationTable.entryAt_lt_length {Ty Cap : Type}
    {entries : List (SpecializationId Ty Cap)} {index : Nat} {requested : SpecializationId Ty Cap}
    (present : entryAt entries index = some requested) : index < entries.length :=
  (List.getElem?_eq_some_iff.1 present).1

/-- Helper (projection). A successful entry search resolves to the requested specialization.
(approved: rev-c9r4-agy). -/
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

/-- Helper (projection). Interning resolves its requested specialization at the returned index.
(approved: rev-c9r4-agy). -/
theorem SpecializationTable.intern_sound {Ty Cap : Type} [DecidableEq Ty] [DecidableEq Cap]
    (entries : List (SpecializationId Ty Cap)) (requested : SpecializationId Ty Cap) :
    entryAt (intern entries requested).1 (intern entries requested).2 = some requested := by
  unfold intern
  cases found : entryIndex entries requested with
  | none => simp [entryAt]
  | some index => exact entryIndex_sound entries requested index found

/-- Helper (projection). Interning preserves every old numeric entry.
(approved: rev-c9r4-agy). -/
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

/-- Helper (projection). A failed entry search really means the specialization is absent.
(approved: rev-c9r4-agy). -/
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
are one entry. (approved: rev-c9r4-agy). -/
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
its two sides. (approved: rev-c9r4-agy). -/
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
of the scanned list. (approved: rev-c9r4-agy). -/
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

/-- Helper (projection). Soundness of the complete ordered-pair search.
(approved: rev-c9r4-agy). -/
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

/-- Law (subject `ConcreteSpec.Core.LoadedModules.load`, `ConcreteSpec.Core.LoadedModules.HasCollision`).
**Load refusal is sound.** A refused collision names two modules that really occur in the
loaded input; neither package identity nor origin is discarded from the diagnostic. -/
theorem LoadedModules.load_refusal_sound (modules : List LoadedModule) (collision : LoadCollision)
    (refused : load modules = .refused collision) : HasCollision modules collision := by
  unfold load at refused
  cases found : findCollision modules with
  | none => simp [found] at refused
  | some foundCollision =>
    simp only [found, LoadResult.refused.injEq] at refused
    subst collision
    exact findCollisionFrom_sound modules modules foundCollision found

/-- Helper (projection). A real pair collision is visible to the ordered-pair checker.
(approved: rev-c9r4-agy). -/
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

/-- Helper (projection). The one-module scan finds every collision with a member of its input list.
(approved: rev-c9r4-agy). -/
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

/-- Law (subject `ConcreteSpec.Core.LoadedModules.load`, `ConcreteSpec.Core.LoadedModules.WellFormed`).
Admission is exactly the absence of a collision, not a second coexisting interpretation. -/
theorem LoadedModules.load_admitted (modules : List LoadedModule) (admitted : List LoadedModule)
    (result : load modules = .admitted admitted) : admitted = modules ∧ WellFormed modules := by
  unfold load at result
  cases found : findCollision modules with
  | some collision => simp [found] at result
  | none =>
    simp only [found, LoadResult.admitted.injEq] at result
    subst admitted
    refine ⟨rfl, ?_⟩
    intro collision present
    obtain ⟨detected, refused⟩ := load_refusal_complete modules collision present
    simp [load, found] at refused

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

/-- Helper (projection). `LocationOk` projects to bounds and the two computed coordinates.
(approved: rev-c9r4-agy). -/
theorem Span.locationOk_iff (source : SourceText) (sp : Span) :
    sp.LocationOk source ↔ sp.endOffset ≤ source.length ∧
      sp.line = source.lineAt sp.startOffset ∧ sp.col = source.columnAt sp.startOffset := Iff.rfl

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
`none` at every reader version. (approved: PENDING). -/
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

/-- Law (subject `ConcreteSpec.Core.EncodedPremise.read`, `ConcreteSpec.Core.PremiseKind.decode`, `ConcreteSpec.Core.specVersion`).
A premise record whose writer version disagrees with the reader version, or that is read at a
version other than `specVersion`, is refused fail-closed. -/
theorem EncodedPremise.read_version_mismatch (reader : SpecVersion) (record : EncodedPremise)
    (mismatch : record.spec ≠ reader ∨ reader ≠ specVersion) :
    record.read reader = none := by
  rcases mismatch with hspec | hreader
  · simp [EncodedPremise.read, hspec]
  · simp [EncodedPremise.read, PremiseKind.decode, hreader]

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

/-- Mutant rename that leaves the construct's old name unchanged. -/
def noopRename (construct : KeyConstruct) (_name : String) : KeyConstruct := construct

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.key_rename_named`, case `ConcreteSpec.Core.Vacuity.namedCall`). -/
example : ((KeyConstruct.mk .call (.named "work") []).rename "renamed").key [] =
    ⟨[⟨.call, .named "renamed"⟩]⟩ :=
  KeyConstruct.key_rename_named [] .call "work" [] "renamed"

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.key_rename_named`, case `ConcreteSpec.Core.Vacuity.namedCall`, subject `ConcreteSpec.Core.KeyConstruct.rename`, mutant `ConcreteSpec.Core.Vacuity.noopRename`). -/
example : ¬ ((noopRename (KeyConstruct.mk .call (.named "work") []) "renamed").key [] =
    ⟨[⟨.call, .named "renamed"⟩]⟩) := by decide

/-- Mutant pin that ignores the supplied user label. -/
def noopPin (construct : KeyConstruct) (_label : Option String) : KeyConstruct := construct

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.key_pin_unnamed`, case `ConcreteSpec.Core.Vacuity.unnamedBinding`). -/
example : ((KeyConstruct.mk .binding (.unnamed none) []).pin (some "pinned")).key [] =
    ⟨[⟨.binding, .pinned "pinned"⟩]⟩ :=
  KeyConstruct.key_pin_unnamed [] .binding none [] "pinned"

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.key_pin_unnamed`, case `ConcreteSpec.Core.Vacuity.unnamedBinding`, subject `ConcreteSpec.Core.KeyConstruct.pin`, mutant `ConcreteSpec.Core.Vacuity.noopPin`). -/
example : ¬ ((noopPin (KeyConstruct.mk .binding (.unnamed none) []) (some "pinned")).key [] =
    ⟨[⟨.binding, .pinned "pinned"⟩]⟩) := by decide

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

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.descendant_named_ancestor_resets_scope`, case `ConcreteSpec.Core.Vacuity.namedNested`). -/
example : (KeyConstruct.mk .block (.named "body") nestedBody.children).descendantKey [unnamedLoop] [0, 0] =
    (KeyConstruct.mk .block (.named "body") nestedBody.children).descendantKey [] [0, 0] :=
  KeyConstruct.descendant_named_ancestor_resets_scope [unnamedLoop] [] .block "body" nestedBody.children [0, 0]

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.descendant_named_ancestor_resets_scope`, case `ConcreteSpec.Core.Vacuity.namedNested`, subject `ConcreteSpec.Core.KeyConstruct.descendantKey`, mutant `ConcreteSpec.Core.Vacuity.unresetScopeDescendantKey`). -/
example : ¬ (unresetScopeDescendantKey [unnamedLoop] (KeyConstruct.mk .block (.named "body") nestedBody.children) [0, 0] =
    unresetScopeDescendantKey [] (KeyConstruct.mk .block (.named "body") nestedBody.children) [0, 0]) := by decide

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

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.descendant_rename_named`, case `ConcreteSpec.Core.Vacuity.namedNested`). -/
example : ((KeyConstruct.mk .block (.named "body") nestedBody.children).rename "renamed").descendantKey [] [0, 0] =
    (KeyConstruct.pathAt nestedBody.children [0, 0]).map (fun suffix =>
      ⟨⟨.block, .named "renamed"⟩ :: suffix.path⟩) :=
  KeyConstruct.descendant_rename_named [] .block "body" "renamed" nestedBody.children [0, 0]

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.descendant_rename_named`, case `ConcreteSpec.Core.Vacuity.namedNested`, subject `ConcreteSpec.Core.KeyConstruct.rename`, mutant `ConcreteSpec.Core.Vacuity.noopRename`). -/
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
example : LoadedModules.HasCollision [firstModule, collidingModule] collidingTop :=
  LoadedModules.load_refusal_sound [firstModule, collidingModule] collidingTop rfl

/-- Planted (law `ConcreteSpec.Core.LoadedModules.load_refusal_sound`, case `ConcreteSpec.Core.Vacuity.collidingTop`, subject `ConcreteSpec.Core.LoadedModules.load`, mutant `ConcreteSpec.Core.Vacuity.fabricatedRefusalLoad`). -/
example : fabricatedRefusalLoad [firstModule, collidingModule] = .refused collidingOrigin ∧
    ¬ LoadedModules.HasCollision [firstModule, collidingModule] collidingOrigin := by
  refine ⟨rfl, ?_⟩
  unfold LoadedModules.HasCollision
  decide

/-- Mutant loader that admits all module lists without checking for collisions. -/
def admitAll (modules : List LoadedModule) : LoadResult := .admitted modules

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
example : [firstModule, secondModule] = [firstModule, secondModule] ∧
    LoadedModules.WellFormed [firstModule, secondModule] :=
  LoadedModules.load_admitted [firstModule, secondModule] [firstModule, secondModule] rfl

/-- Planted (law `ConcreteSpec.Core.LoadedModules.load_admitted`, case `ConcreteSpec.Core.Vacuity.firstModule`, subject `ConcreteSpec.Core.LoadedModules.load`, mutant `ConcreteSpec.Core.Vacuity.admitAll`). -/
example : admitAll [firstModule, collidingModule] = .admitted [firstModule, collidingModule] ∧
    ¬ ([firstModule, collidingModule] = [firstModule, collidingModule] ∧
      LoadedModules.WellFormed [firstModule, collidingModule]) := by
  refine ⟨rfl, ?_⟩
  rintro ⟨_, wf⟩
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

/-- The same start with a different end, and a bad state with a wrong column. -/
def tabPointSpan : Span := { tabSpan with endOffset := 2, range_ok := by decide }
def wrongTabSpan : Span := { tabSpan with col := 2, col_pos := by decide }

/-- Accepted: the tab span satisfies the `LocationOk` projection at a real in-bounds range. -/
example : tabSpan.LocationOk tabSource ↔ tabSpan.endOffset ≤ tabSource.length ∧
    tabSpan.line = tabSource.lineAt tabSpan.startOffset ∧
      tabSpan.col = tabSource.columnAt tabSpan.startOffset := Span.locationOk_iff tabSource tabSpan

/-- Refused: the same stored coordinates do not satisfy `LocationOk` for a source with a different
line structure. -/
example : ¬ tabSpan.LocationOk otherTabSource ∧
    ¬ (tabSpan.endOffset ≤ otherTabSource.length ∧
      tabSpan.line = otherTabSource.lineAt tabSpan.startOffset ∧
      tabSpan.col = otherTabSource.columnAt tabSpan.startOffset) := by decide

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

/-- Instance (law `ConcreteSpec.Core.EncodedPremise.read_version_mismatch`, case `ConcreteSpec.Core.Vacuity.mismatchedRecord`). -/
example : mismatchedRecord.read specVersion = none :=
  EncodedPremise.read_version_mismatch specVersion mismatchedRecord (Or.inl (by decide))

/-- Instance (law `ConcreteSpec.Core.EncodedPremise.read_version_mismatch`, case `ConcreteSpec.Core.Vacuity.futureVersion`). -/
example : stackRecord.encode.read futureVersion = none :=
  EncodedPremise.read_version_mismatch futureVersion stackRecord.encode (Or.inr (by decide))

/-- Planted (law `ConcreteSpec.Core.EncodedPremise.read_version_mismatch`, case `ConcreteSpec.Core.Vacuity.mismatchedRecord`, subject `ConcreteSpec.Core.EncodedPremise.read`, mutant `ConcreteSpec.Core.Vacuity.versionIgnoringRead`). -/
example : (mismatchedRecord.spec ≠ specVersion ∨ specVersion ≠ specVersion) ∧
    ¬ (versionIgnoringRead specVersion mismatchedRecord = none) :=
  ⟨Or.inl (by decide), by decide⟩

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
